(*===============================================================================*)
(*                                                                               *)
(*                rmem executable model                                          *)
(*                =====================                                          *)
(*                                                                               *)
(*  This file is:                                                                *)
(*                                                                               *)
(*  Copyright Jon French, University of Cambridge        2016-2017               *)
(*  Copyright Shaked Flur, University of Cambridge       2016-2018               *)
(*  Copyright Christopher Pulte, University of Cambridge 2016-2018               *)
(*  Copyright Peter Sewell, University of Cambridge           2016               *)
(*                                                                               *)
(*  All rights reserved.                                                         *)
(*                                                                               *)
(*  It is part of the rmem tool, distributed under the 2-clause BSD licence in   *)
(*  LICENCE.txt.                                                                 *)
(*                                                                               *)
(*===============================================================================*)

open RunOptions
open Screen_base

open Params

module Make (ConcModel: Concurrency_model.S) = struct


module StateHashSet = Set.Make(String)

module TidMap = Map.Make(struct
  type t = Events.thread_id
  let compare = compare
end)

module StateMap = Map.Make(struct
  type t = Test.C.state
  let compare (r1, m1) (r2, m2) =
    begin match Pervasives.compare r1 r2 with
    | 0 ->
        let cmp (footprint1, value1) (footprint2, value2) = fun () ->
          begin match Pervasives.compare footprint1 footprint2 with
          | 0 -> Nat_big_num.compare value1 value2
          | c -> c
          end
        in
        begin try Model_aux.cmps (List.map2 cmp m1 m2) with
        | Invalid_argument _ -> if List.length m1 > List.length m2 then 1 else -1
        end
    | c -> c
    end
end)

module ExceptionMap = Map.Make(struct
  type t = Events.thread_id * Events.ioid * BasicTypes.exception_type
  let compare (tid1, ioid1, e1) (tid2, ioid2, e2) =
    begin match (Pervasives.compare tid1 tid2, Pervasives.compare ioid1 ioid2) with
    | (0, 0) -> BasicTypes.exception_type_compare e1 e2
    | (0, c)
    | (c, _) -> c
    end
end)

type state_predicate = ConcModel.state -> bool
type trans_predicate = ConcModel.trans -> bool
type state_and_trans_predicate = ConcModel.state -> ConcModel.trans -> bool

type breakpoint_predicate =
  | StateBreakpoint of (ConcModel.state -> bool)
  | TransitionBreakpoint of (ConcModel.state -> ConcModel.trans -> bool)
  | SharedBreakpoint of (Sail_impl_base.footprint Pset.set ->
                         ConcModel.state ->
                         ConcModel.trans ->
                         bool)

type breakpoint_id =
  | Numbered of int
  | Named of int * string

type breakpoint = (breakpoint_id * breakpoint_predicate * string)

type search_node =
  { system_state: ConcModel.state;

    (* [] - not explored yet
       tn :: ... :: [t1] - t1 is transition that used to be in unexplored_transitions
                           (actually the index of the transition in system_state.sst_system_transitions).
                           The other transitions are eager transitions that followed.
                           For the initial node, t1 might also be eager. *)
    open_transition: int list;

    (* filtered transitions that we still need to explore;
    in '(i, t) :: _' t is transition from system_state.sst_system_transitions
    and i is its index in that list *)
    unexplored_transitions: (int * ConcModel.trans) list;
  }

(* the last int in each list is a non-eager transition index and the rest
are eager transition indices *)
type trace = (int list) list

type search_state =
  { (* DFS stack, head is the active node *)
    search_nodes: search_node list;

    (* hashes of observed states, used for pruning *)
    state_hashes: StateHashSet.t;

    (*** accumulation of observed states ***)

    (* final states paired with trace and the number of times they were observed *)
    observed_finals:           (trace * int) StateMap.t;
    (* the above states that pass the (litmus file) filter property *)
    observed_filterred_finals: (trace * int) StateMap.t;
    (* model deadlocks *)
    observed_deadlocks:        (trace * int) option;
    (* unhandled ISA exception states *)
    observed_exceptions:       (trace * int) ExceptionMap.t;

    observed_branch_targets: Params.branch_targets_map;
    observed_shared_memory:  Sail_impl_base.footprint Pset.set;

    (*** statistics ***)

    started_timestamp:      int; (* the UNIX timestamp we started the search *)
    transitions_count:      int; (* the total number of transitions *)
    trace_count:            int; (* the total number of complete traces *)
    prune_count:            int; (* the total number hash prunes *)
    restart_prune_count:    int;
    discard_prune_count:    int;
    late_write_prune_count: int;
    pr_count:               int; (* priority reduction *)
    loop_limit_count:       int;

    print_partial_results: search_state -> unit;

    (*** control the search ***)

    (* these states will cause the search to terminate immediately *)
    breakpoints:   breakpoint list;

    (* back track when these states are reached *)
    bounds:        state_predicate list;

    (* back track when these states are reached but also record them as if they were final *)
    targets:       state_predicate list;

    (* filter out transitions that do not match all of these predicates,
    i.e., a transition must satisfy all the filters to not be filtered out *)
    filters:       trans_predicate list;

    test_info:     Test.info;
    options:       RunOptions.t;
    ppmode:        Globals.ppmode;
}


(* retrun a trace leading to the head of search_state.search_nodes;
head is the last transition *)
let choices_so_far search_state : trace =
  List.map (fun n -> n.open_transition) search_state.search_nodes

(** log/status/debug messages ***************************************)

let sprint_time () : string =
  let time = Unix.localtime (Unix.time ()) in
  Printf.sprintf "[%02d:%02d:%02d]"
    time.Unix.tm_hour time.Unix.tm_min time.Unix.tm_sec

let print_search_counters header search_state : unit =
  if header then
    OTConcat [
      (*       "[##:##:##] #######|########|#######|#######|#######|#######|#####|#######|#######" *)
      otStrLine "                  |transit-|hashed |hash   |restart|discard|write|priori-|loop   ";
      otStrLine "           traces |ions    |states |prunes |prunes |prunes |prune|ty red.|limit  ";
    ]
    |> Screen.show_message search_state.ppmode;


  otStrLine "%s %7d|%8d|%7d|%7d|%7d|%7d|%5d|%7d|%7d"
    (sprint_time ())
    search_state.trace_count
    search_state.transitions_count
    (StateHashSet.cardinal search_state.state_hashes)
    search_state.prune_count
    search_state.restart_prune_count
    search_state.discard_prune_count
    search_state.late_write_prune_count
    search_state.pr_count
    search_state.loop_limit_count
  |> Screen.show_message search_state.ppmode

(* for throttling status messages *)
let last_status_print          = ref 0.0
let last_partial_results_print = ref 0.0

let print_status_message search_state : unit =
  if Globals.is_verbosity_at_least Globals.ThrottledInformation
      && Sys.time () > !last_status_print +. 10.0
  then begin
    let print_header = (!last_status_print = !last_partial_results_print) in
    last_status_print := Sys.time ();

    if search_state.options.allow_partial
        && Sys.time () > !last_partial_results_print +. (5.0 *. 60.0)
    then begin
      last_partial_results_print := !last_status_print;
      search_state.print_partial_results search_state
    end else
      print_search_counters print_header search_state;
  end;
  ()


let print_last_state title search_state : unit =
  let ppmode = search_state.ppmode in

  let state =
    begin match search_state.search_nodes with
    | [] -> OTString "no state"

    | [node] ->
        let (ppmode', ui_state) =
          ConcModel.make_ui_state ppmode None node.system_state []
        in
        OTEncoded (ConcModel.pp_ui_state ppmode' ui_state)

    | node :: node' :: _ ->
        let pp_choices choices =
          List.map
            (fun et -> List.rev et |> List.map string_of_int |> String.concat ",")
            choices
          |> List.rev |> String.concat "; " |> Printf.sprintf "\"%s\""
        in

        begin match List.rev node'.open_transition with
        | [] -> assert false
        | t :: ts ->
            let tran = List.nth (ConcModel.transitions node'.system_state) t in
            let (ppmode', ui_state) =
              ConcModel.make_ui_state
                ppmode
                (Some node'.system_state)
                node.system_state
                []
            in

            OTConcat [
              OTEncoded (ConcModel.pp_ui_state ppmode' ui_state);
              otStrLine "via %s" (pp_choices (choices_so_far search_state));
              otStrLine "last (non eager) transition: %s" (ConcModel.pp_trans ppmode tran);
              otIfTrue (ts <> []) @@ otStrLine "(followed by eager transitions: %s)"
                  (List.map string_of_int ts |> String.concat ",");
              otStrLine "all transitions: [";
              otConcat @@ List.map
                  (fun t -> otStrLine "%s" (ConcModel.pp_trans ppmode t))
                  (ConcModel.transitions node.system_state);
              otStrLine "]";
              otStrLine "unexplored (filtered) transitions: [";
              otConcat @@ List.map
                (fun (i, t) -> otStrLine "[%d] %s" i (ConcModel.pp_trans ppmode t))
                node.unexplored_transitions;
              otStrLine "]";
            ]
        end
    end
  in

  OTConcat [
    otStrLine "***** %s *****" title;
    state;
    otStrLine "**********************************";
  ]
  |> Screen.show_message ppmode

let update_observed_branch_targets_and_shared_memory search_state search_node : search_state =
  let observed_branch_targets =
    let (union, diff) =
      Params.union_and_diff_branch_targets
        (ConcModel.branch_targets_of_state search_node.system_state)
        search_state.observed_branch_targets
    in
    if diff <> [] && Globals.is_verbosity_at_least Globals.ThrottledInformation then
      otStrLine "%s found new branch-register target(s): %s"
        (sprint_time ())
        (Pp.pp_branch_targets search_state.ppmode diff)
      |> Screen.show_message search_state.ppmode;
    union
  in

  let observed_shared_memory =
    if search_state.options.eager_mode.eager_local_mem then
      let (union, diff) =
        Params.union_and_diff_shared_memory
          (ConcModel.shared_memory_of_state search_node.system_state)
          search_state.observed_shared_memory
      in
      if not (Pset.is_empty diff) && Globals.is_verbosity_at_least Globals.ThrottledInformation then
        otStrLine "%s found new shared memory footprint(s): %s"
          (sprint_time ())
          (Pp.pp_shared_memory search_state.ppmode diff)
        |> Screen.show_message search_state.ppmode;
      union
    else search_state.observed_shared_memory
  in

  { search_state with
    observed_branch_targets = observed_branch_targets;
    observed_shared_memory  = observed_shared_memory;
  }


let record_exception search_state (tid, ioid, e) : search_state =
  if Globals.is_verbosity_at_least Globals.Debug then
    print_last_state "state before exception" search_state;

  let search_state =
    match search_state.search_nodes with
    | [] -> assert false
    | node :: _ ->
        update_observed_branch_targets_and_shared_memory search_state node
  in

  let (choices, count) =
    try ExceptionMap.find (tid, ioid, e) search_state.observed_exceptions with
    | Not_found -> (choices_so_far search_state, 0)
  in
  let observed_exceptions = ExceptionMap.add (tid, ioid, e) (choices, count + 1)
      search_state.observed_exceptions
  in

  {search_state with observed_exceptions = observed_exceptions}


(* TODO: this should probably be in a different file *)
let reduced_final_state regs mem (system_state: ConcModel.state) =
  let final_reg_states = ConcModel.final_reg_states system_state in
  let reg_state : (Events.thread_id * (Sail_impl_base.reg_base_name * Test.C.value) list) list =
    let reg_values_of_thread (tid,regstate) =
      let final_value = function
        | Some v ->
           begin match Sail_impl_base.integer_of_register_value v with
           | Some i -> Test.C.big_num_to_value i
           | None -> failwith "register final value has unknown/undef bits"
           end
        | None -> failwith "register final value read is blocked"
      in

      let filtered = List.filter (fun (reg, _) -> List.mem (tid, reg) regs) regstate in
      let regvals = List.map (fun (reg, value) -> (reg, final_value value)) filtered in
      (tid,regvals)
    in
    List.map reg_values_of_thread final_reg_states
  in

  let mem_state =
    List.map
      (fun ((addr, size), value) ->
        let int64_addr = Test.C.interp_address_to_address addr in
        let big_value =
          match Sail_impl_base.integer_of_memory_value (Globals.get_endianness ()) value with
          | Some bi -> bi
          | None -> failwith "bad final memory value"
        in
        ((int64_addr, size), big_value))
      (ConcModel.memory_value_of_footprints system_state mem)
  in

  (reg_state, mem_state)


let record_final_state search_state search_node : search_state =
  assert (compare (List.hd search_state.search_nodes) search_node = 0);

  if Globals.is_verbosity_at_least Globals.Debug then
    print_last_state "final state" search_state;

  let search_state = {search_state with trace_count = search_state.trace_count + 1} in

  let search_state = update_observed_branch_targets_and_shared_memory search_state search_node in

  if ConcModel.is_final_state search_node.system_state then
    let final_state = reduced_final_state search_state.test_info.Test.show_regs search_state.test_info.Test.show_mem search_node.system_state in

    let (choices, count) =
      try StateMap.find final_state search_state.observed_finals with
      | Not_found -> (choices_so_far search_state, 0)
    in

    let observed_finals = StateMap.add final_state (choices, count + 1) search_state.observed_finals in

    let observed_filterred_finals =
      if  match search_state.test_info.Test.filter with
          | None -> true
          | Some filter ->
              reduced_final_state search_state.test_info.Test.filter_regs search_state.test_info.Test.filter_mem search_node.system_state
              |> Test.C.check_filter filter
      then
        StateMap.add final_state (choices, count + 1) search_state.observed_filterred_finals
      else
        search_state.observed_filterred_finals
    in

    { search_state with
      observed_finals = observed_finals;
      observed_filterred_finals = observed_filterred_finals;
    }
  else
    let choices' = choices_so_far search_state in
    match search_state.observed_deadlocks with
    | Some (choices, count) ->
        { search_state with
            observed_deadlocks =
              if List.length choices' < List.length choices then
                Some (choices', count + 1)
              else
                Some (choices, count + 1)
        }
    | None ->
        {search_state with observed_deadlocks = Some (choices', 1)}

let is_eager search_state transition =
  match search_state.search_nodes with
  | [] -> assert false
  | node :: _ ->
      ConcModel.is_eager_trans
        node.system_state
        search_state.options.eager_mode
        transition

let add_search_node system_state search_state : search_state =
  let filtered_transitions =
    List.mapi (fun i t -> (i, t)) (ConcModel.transitions system_state)
    |> List.filter (fun (_, t) -> (List.for_all (fun p -> p t) search_state.filters))
  in

  let new_search_node =
    { system_state           = system_state;
      open_transition        = [];
      unexplored_transitions = filtered_transitions;
    }
  in

  let search_state =
    { search_state with
      search_nodes = new_search_node :: search_state.search_nodes;
      transitions_count = search_state.transitions_count + 1;
    }
  in

  if Globals.is_verbosity_at_least Globals.Debug then
    print_last_state "new state" search_state;

  begin match search_state.options.max_trace_length with
  | Some max_length when List.length search_state.search_nodes > max_length ->
      raise (Failure (Printf.sprintf "trace exceeded %d states (see -max_trace_length)" max_length))
  | Some _
  | None -> ()
  end;

  search_state

let pop search_state : search_state =
  (* pop the head and continue *)
  begin match search_state.search_nodes with
  | [] -> search_state
  | _ :: search_nodes' ->
     {search_state with search_nodes = search_nodes'}
  end

let take_transition search_state state (i, (transition : ConcModel.trans)) eager : search_state =
  let m = search_state.ppmode in
  match search_state.search_nodes with
  | [] -> assert false
  | search_node :: search_nodes ->
      Screen.show_debug m (fun () -> OTConcat [
        otStrLine "Taking %s transition: [%d] %s"
          (if eager then "eager" else "### NON-EAGER ###")
          i
          (ConcModel.pp_trans m transition);
        otStrLine "of: (%d) ["
          (List.length search_node.unexplored_transitions);
        OTConcat
          (List.map
            (fun (i, t) -> otStrLine "[%d] %s" i (ConcModel.pp_trans m t))
            search_node.unexplored_transitions);
        otStrLine "] (open_transition = [%s])"
          (Pp.pp_list m string_of_int search_node.open_transition);
      ]);

      (* Invariant: either
          - we're at the initial state, or
          - the previous state has at least one open transition *)
      if search_nodes <> [] && (List.hd search_nodes).open_transition = [] then
        failwith "not at initial state and open_transition of previous state is empty";

      (* Invariant: if we're taking a non-eager transition, this is a fresh search node *)
      if not eager && not (search_node.open_transition = []) then
        failwith "trying to take a non-eager transition of a non-fresh node (open_transition <> [])";

      (* Invariant: if we're taking a non-eager transition, we've taken all eager transitions already *)
      if not eager &&
          List.exists
            (fun (_, t) -> is_eager search_state t)
            search_node.unexplored_transitions
      then
        failwith "trying to take a non-eager transition, but eager transitions still exist";

      begin match ConcModel.state_after_trans state
                    (transition : ConcModel.trans) with
      | BasicTypes.TO_unhandled_exception (tid, ioid, e) ->
          (* SF: I don't think this will ever happen *)
          record_exception search_state (tid, ioid, e)
      | BasicTypes.TO_system_state state' when eager ->
          (* If the transition was eager, pop and re-push the head node,
          prepending it to the previous node's open_transition *)
          begin match (pop search_state) with
          | { search_nodes = [] } ->
              (* Special case: the first transition from the initial state is eager *)
              let search_node = List.hd search_state.search_nodes in
              assert (search_node.open_transition = []);
              { search_state with
                search_nodes = [{search_node with open_transition = [i]; unexplored_transitions = [];}];
              }
              |> add_search_node state'
          | { search_nodes = search_node' :: search_nodes' } as search_state' ->
              { search_state' with
                search_nodes =
                  { search_node' with
                    open_transition = i :: search_node'.open_transition;
                  } :: search_nodes';
              }
              |> add_search_node state'
          end
      | BasicTypes.TO_system_state state' (* when not eager *) ->
          (* Otherwise we push a new node. We have previously asserted that open_transition = [] *)
          assert (search_node.open_transition = []);
          { search_state with
            search_nodes =
              { search_node with
                open_transition = [i];
                unexplored_transitions = List.remove_assoc i search_node.unexplored_transitions;
              } :: search_nodes;
          }
          |> add_search_node state'
      end

let rec some_if_any (f : 'a -> 'b option) : 'a list -> 'b option = function
  | []     -> None
  | h :: t -> match f h with None -> some_if_any f t | s -> s

(* when set to true the search should stop *)
let interrupt = ref false;;

type search_outcome =
  | Complete    of search_state
  | Breakpoints of search_state * (breakpoint list) (* which breakpoints were hit *)
  | Interrupted of search_state * string (* explanation of why interrupted *)
  | OcamlExn    of search_state * string

type search_try_outcome =
  | Terminate_search of search_outcome
  | Continue_search  of search_state
let continue_search s = Continue_search s

(* search for final states (exhaustive/random) with breakpoints
   IMPORTANT: all calls to 'search' must be tail calls, otherwise the
   stack will explode *)
let rec search search_state : search_outcome =
  if !interrupt then Interrupted (search_state, "received interrupt signal") else

  let check_limits cont = fun search_state ->
    let hit_transition_limit search_state =
      match search_state.options.transition_limit with
      | None -> None
      | Some limit ->
          if search_state.transitions_count >= limit then
            Some (Printf.sprintf "exceeded transition limit of %d" limit)
          else None
    in
    let hit_trace_limit search_state =
      match search_state.options.trace_limit with
      | None -> None
      | Some limit ->
          if search_state.trace_count >= limit then
            Some (Printf.sprintf "exceeded trace limit of %d" limit)
          else None
    in
    let hit_time_limit search_state =
      match search_state.options.time_limit with
      | None -> None
      | Some limit ->
          let time = Sys.time () |> int_of_float in
          if time >= (search_state.started_timestamp + limit) then
            Some (Printf.sprintf "exceeded time limit of %d seconds" limit)
          else None
    in
    match some_if_any (fun f -> f search_state)
        [ hit_transition_limit;
          hit_trace_limit;
          hit_time_limit;
        ]
    with
    | Some reason -> Terminate_search (Interrupted (search_state, reason))
    | None -> cont search_state
  in

  let check_breakpoints cont = fun search_state ->
    begin match search_state.search_nodes with
    | [] -> assert false
    | search_node :: _ ->
        if search_node.open_transition = [] then
          let breakpoints =
            List.filter (function
              | (_, StateBreakpoint pred, _) ->
                  pred search_node.system_state
              | (_, TransitionBreakpoint pred, _) ->
                  List.exists
                    (fun (_, t) -> pred search_node.system_state t)
                    search_node.unexplored_transitions
              | (_, SharedBreakpoint pred, _) ->
                  List.exists
                    (fun (_, t) -> pred search_state.observed_shared_memory search_node.system_state t)
                    search_node.unexplored_transitions
              )
              search_state.breakpoints
          in
          if breakpoints <> [] then
            Terminate_search (Breakpoints (search_state, breakpoints))
          else
            cont search_state
        else
          cont search_state
    end
  in

  let check_targets cont = fun search_state ->
    begin match search_state.search_nodes with
    | [] -> assert false
    | search_node :: _ ->
        if search_node.open_transition = []
          && List.exists (fun f -> f search_node.system_state) search_state.targets
        then
          Continue_search (record_final_state search_state search_node |> pop)
        else
          cont search_state
    end
  in

  (* there is a special case where unexplored_transitions = [] but
  sst_system_transitions <> [], this case is handled by the final
  pop continuation and not in check_bound *)
  let check_bound cont = fun search_state ->
    begin match search_state.search_nodes with
    | [] -> assert false
    | search_node :: _ ->
        if search_node.open_transition = []
            && List.exists (fun f -> f search_node.system_state) search_state.bounds
        then
          Continue_search (pop search_state)
        else
          cont search_state
    end
  in

  let prune_restarts cont = fun search_state ->
    begin match search_state.search_nodes with
    | [] -> assert false
    | search_node :: _ ->
        if search_node.open_transition = []
            && search_state.options.prune_restarts
            && ConcModel.inst_restarted search_node.system_state
        then
          { search_state with
            restart_prune_count = search_state.restart_prune_count + 1
          } |> pop |> continue_search
        else
          cont search_state
    end
  in

  let prune_discards cont = fun search_state ->
    begin match search_state.search_nodes with
    | [] -> assert false
    | search_node :: _ ->
        if search_node.open_transition = []
            && search_state.options.prune_discards
            && ConcModel.inst_discarded search_node.system_state
        then
          { search_state with
            discard_prune_count = search_state.discard_prune_count + 1
          } |> pop |> continue_search
        else
          cont search_state
    end
  in

  let prune_late_writes cont = fun search_state ->
    begin match search_state.search_nodes with
    | [] -> assert false
    | search_node :: _ ->
        if search_node.open_transition = []
            && search_state.options.prune_late_writes
            && ConcModel.write_after_stop_promising search_node.system_state
        then
          { search_state with
            late_write_prune_count = search_state.late_write_prune_count + 1
          } |> pop |> continue_search
        else
          cont search_state
    end
  in

  let take_eager_transition cont = fun search_state ->
    match search_state.search_nodes with
    | [] -> assert false
    | search_node :: _ ->
        if search_node.open_transition = [] then
          match List.find
              (fun (_, t) -> is_eager search_state t)
              search_node.unexplored_transitions
          with
          | transition ->
              take_transition search_state search_node.system_state transition true
              |> continue_search
          | exception Not_found -> cont search_state
        else
          cont search_state
  in

  (*
  let assert_no_eager cont = fun search_state ->
    begin match search_state.search_nodes with
    | [] -> assert false
    | search_node :: _ ->
        let m = search_state.ppmode in
        if search_node.open_transition <> [] then begin
            let leftovers = List.filter
                              (fun (_, t) -> is_eager search_state t)
                              search_node.unexplored_transitions
            in
            if leftovers <> [] then begin
                Screen.show_message m "there was a leftover eager transition after all should have been taken!\n[\n%s\n]"
                                    (Pp.pp_list m (fun (i, t) -> Printf.sprintf "[%d] %s (eager: %b)\n" i (Pp.pp_trans m t) (is_eager search_state t)) search_node.unexplored_transitions);
                assert false
              end
          end;
        cont search_state
    end
  in
  *)

  let hash_prune cont = fun search_state ->
    begin match search_state.search_nodes with
    | [] -> assert false
    | search_node :: _ ->
        if search_node.open_transition = [] && 
             search_state.options.hash_prune 
             && not (ConcModel.stopped_promising search_node.system_state)
        then
          let hash =
            ConcModel.make_ui_state Globals.ppmode_for_hashing
              None search_node.system_state []
            |> snd
            |> ConcModel.pp_ui_state Globals.ppmode_for_hashing
            |> Digest.string
          in

          if StateHashSet.mem hash search_state.state_hashes then
            (* we have already seen this state *)
            { search_state with
              prune_count = search_state.prune_count + 1;
            } |> pop |> continue_search
          else
            { search_state with
              state_hashes = StateHashSet.add hash search_state.state_hashes;
            } |> cont
        else
          cont search_state
    end
  in

  let loop_limit cont = fun search_state ->
    begin match search_state.search_nodes with
    | [] -> assert false
    | search_node :: search_nodes' ->
        if search_node.open_transition = []
            && (ConcModel.model_params search_node.system_state).t.thread_loop_unroll_limit <> None
            && search_state.options.prune_discards
        then
          let is_loop_limit (_, t) = ConcModel.is_loop_limit_trans t in
          begin match List.filter is_loop_limit search_node.unexplored_transitions with
          | [] -> cont search_state
          | ts ->
              let search_node' =
                { search_node with
                  unexplored_transitions = ts;
                }
              in
              { search_state with
                search_nodes     = search_node' :: search_nodes';
                loop_limit_count = search_state.loop_limit_count + 1;
              }
              |> cont
          end
        else cont search_state
    end
  in

  let priority_reduction cont = fun search_state ->
    begin match search_state.search_nodes with
    | [] -> assert false
    | search_node :: search_nodes' ->
        let priority_transitions =
          ConcModel.priority_trans
            search_node.system_state
            search_state.options.eager_mode
        in
        if search_node.open_transition = [] && search_state.options.priority_reduction then
          let rec reduce = function
            | [] -> search_state
            | (f,per_ioid) :: fs ->
                begin match List.filter (fun (_, t) -> f t) search_node.unexplored_transitions with
                | [] -> reduce fs
                | reduced ->
                    if per_ioid then
                      (* filter only the transitions from a single thread and
                      single instruction (smallest ioid, i.e., po-oldest) *)
                      let cmp_ioid (_, t1) (_, t2) : int =
                        if ConcModel.is_thread_trans t1 && 
                           ConcModel.is_thread_trans t2
                        then compare (ConcModel.ioid_of_thread_trans t1)
                                     (ConcModel.ioid_of_thread_trans t2)
                        else assert false
                      in
                      begin match List.stable_sort cmp_ioid reduced with
                      | t :: ts ->
                          let search_node' =
                            { search_node with
                              unexplored_transitions = t :: List.filter (fun t' -> cmp_ioid t t' = 0) ts;
                            }
                          in
                          { search_state with
                            search_nodes = search_node' :: search_nodes';
                            pr_count     = search_state.pr_count + 1;
                          }
                      | _ -> assert false
                      end
                    else 
                      let search_node' =
                        { search_node with unexplored_transitions = reduced }
                      in
                      { search_state with
                        search_nodes = search_node' :: search_nodes';
                        pr_count     = search_state.pr_count + 1;
                      }
                end
          in
          reduce priority_transitions |> cont

        else
          cont search_state
    end
  in

  let take_next_transition cont = fun search_state ->
    begin match search_state.search_nodes with
    | [] -> assert false
    | search_node :: search_nodes' ->
        begin match search_node.unexplored_transitions with
        | [] -> cont search_state (* no transitions to take *)

        | transition :: unexplored_transitions' ->
            (* explore the next transition *)
            let (transition, unexplored_transitions') =
              if search_state.options.pseudorandom then
                let choice = Random.int (List.length search_node.unexplored_transitions) in
                (List.nth search_node.unexplored_transitions choice, [])
              else
                (transition, unexplored_transitions')
            in

            let search_node' =
              { search_node with
                open_transition = [];
                unexplored_transitions = unexplored_transitions';
              }
            in
            let search_state' =
              { search_state with
                search_nodes = search_node' :: search_nodes';
              }
            in

            take_transition search_state' search_node'.system_state transition false
            |> continue_search
        end
    end
  in

  let check_final cont = fun search_state ->
    begin match search_state.search_nodes with
    | [] -> Terminate_search (Complete search_state) (* no more nodes to explore *)
    | search_node :: _ ->
        if search_node.open_transition = [] && search_node.unexplored_transitions = [] then
          Continue_search (record_final_state search_state search_node |> pop)
        else
          cont search_state
    end
  in

  begin match search_state.search_nodes with
  | [] -> Complete search_state (* no more nodes to explore *)

  | _ :: _ ->
      (* explore the transitions of the head node *)
      print_status_message search_state;

      begin match
        ( check_limits
          @@ check_breakpoints
          @@ check_targets
          @@ check_final
          @@ check_bound
          @@ prune_restarts
          @@ prune_discards
          @@ prune_late_writes
          @@ take_eager_transition
          (* @@ assert_no_eager *)
          @@ hash_prune
          @@ loop_limit
          @@ priority_reduction
          @@ take_next_transition
          (* if all else fails, pop and continue *)
          @@ (fun search_state -> Continue_search (pop search_state))
        ) search_state
      with
      | Terminate_search r -> r
      | Continue_search s  -> search s
      | exception e ->
          let msg = Printexc.to_string e in
          let stack = Printexc.get_backtrace () in
          OcamlExn (search_state, Printf.sprintf "there was an error: %s\n%s\n" msg stack)
      end
  end

let init_search_state = ref None

let print_transitions trace : unit =
  let rec do_trace trace eager search_state =
    match trace with
    | [] -> ()
    | [] :: trace -> do_trace trace false search_state
    | (t :: ts) :: trace ->
        let node = List.hd search_state.search_nodes in
        let transition = List.nth (ConcModel.transitions node.system_state) t in
        take_transition search_state node.system_state (t, transition) eager
        |> do_trace (ts :: trace) true
  in

  let trace =
    List.rev trace
    |> List.map List.rev
  in

  match !init_search_state with
  | None -> assert false
  | Some s -> do_trace trace true s

(* This exception is just to break out of the for loop below *)
exception RandomResult of search_outcome

exception BadSearchOptions of string

let search_from_state
    (ppmode:         Globals.ppmode)
    (options:        RunOptions.t)
    (test_info:      Test.info) (* TODO: probably should be abstracted,
                                used only in reduced_final_state *)
    (system_state:   ConcModel.state)
    (breakpoints:    breakpoint list)
    (bounds:         state_predicate list)
    (targets:        state_predicate list)
    (filters:        trans_predicate list)
    (print_partial_results: search_state -> unit)
    : search_outcome
  =
  let started_timestamp = Sys.time () |> int_of_float in

  (* FIXME: (SF) The interpreter PP is not complete and might cause different
  state hash to compare as equal *)
  if options.hash_prune &&
    not (options.eager_mode.eager_pseudocode_internal ||
          (ConcModel.model_params system_state).ss.ss_model = Promising_storage_model)
  then
    raise (BadSearchOptions "hash_prune is not safe without eager_pseudocode_internal");

  if options.prune_discards &&
      (ConcModel.model_params system_state).t.thread_allow_tree_speculation
  then
    raise (BadSearchOptions "prune_discards requires forbid_tree_speculation");

  let options =
    { options with
        (* Every iteration of the random search starts from the initial state.
        Hence, if hash_prune is enabled, every iteration following the first
        one, will be hash-pruned immediately. *)
        hash_prune =
          if options.pseudorandom then false else options.hash_prune;
    }
  in

  let initial_search_state =
    { search_nodes = [];

      state_hashes        = StateHashSet.empty;

      observed_finals           = StateMap.empty;
      observed_filterred_finals = StateMap.empty;
      observed_deadlocks        = None;
      observed_exceptions       = ExceptionMap.empty;

      observed_branch_targets = (ConcModel.model_params system_state).t.branch_targets;
      observed_shared_memory  = options.eager_mode.em_shared_memory;

      (* statistics *)
      started_timestamp      = started_timestamp;
      transitions_count      = 0;
      trace_count            = 0;
      prune_count            = 0;
      restart_prune_count    = 0;
      discard_prune_count    = 0;
      late_write_prune_count = 0;
      pr_count               = 0;
      loop_limit_count       = 0;

      print_partial_results = print_partial_results;

      breakpoints = breakpoints;

      bounds      = bounds;
      targets     = targets;
      filters     = filters;

      test_info = test_info;
      options   = options;
      ppmode    = ppmode;
    } |> add_search_node system_state
  in

  interrupt := false;

  let old_signal_behaviors =
    if options.allow_partial then
      let set_signal_with_restore (sig_name, sig_num) : unit -> unit =
        let interrupt_handler n =
          otStrLine "received interrupt signal %s (%d)" sig_name n
          |> Screen.show_message ppmode;
          interrupt := true
        in
        let old_signal_behavior = Sys.signal sig_num (Sys.Signal_handle interrupt_handler) in
        fun () -> Sys.set_signal sig_num old_signal_behavior
      in

      List.map
        set_signal_with_restore
        [ ("INT" , Sys.sigint );
          ("ABRT", Sys.sigabrt);
          ("ALRM", Sys.sigalrm);
          ("QUIT", Sys.sigquit);
          ("TERM", Sys.sigterm);
        ]
    else []
  in

  let print_diffs search_state bt_union bt_diff sm_union sm_diff : unit =
    if Globals.is_verbosity_at_least Globals.ThrottledInformation then begin
      if not (Pmap.is_empty bt_union) then
        OTConcat [
          otStrLine "Branch-register targets that were observed:";
          Params.branch_targets_to_list bt_union
            |> Pp.pp_branch_targets search_state.ppmode
            |> otStrLine "%s";
          otIfTrue (bt_diff <> []) @@
            otStrLine "(from which the following are new: %s)"
              (Pp.pp_branch_targets search_state.ppmode bt_diff);
        ]
        |> Screen.show_message search_state.ppmode;

      if options.eager_mode.eager_local_mem then begin
        if Pset.is_empty sm_union then
          otStrLine "No shared memory footprints were observed!"
          |> Screen.show_message search_state.ppmode
        else
          OTConcat [
            otStrLine "Shared memory footprints that were observed:";
            otStrLine "%s" (Pp.pp_shared_memory search_state.ppmode sm_union);
            otIfTrue (not (Pset.is_empty sm_diff)) @@
              otStrLine "(from which the following are new: %s)"
                (Pp.pp_shared_memory search_state.ppmode sm_diff);
          ]
          |> Screen.show_message search_state.ppmode;
      end;
    end;
  in

  let rec search_fixed_point n initial_search_state : search_outcome =
    match search initial_search_state with
    | (Complete search_state') as result ->
        let (bt_union, bt_diff) =
          Params.union_and_diff_branch_targets
            search_state'.observed_branch_targets
            initial_search_state.observed_branch_targets
            (*sst.sst_state.model.t.branch_targets*)
        in

        let (sm_union, sm_diff) =
          Params.union_and_diff_shared_memory
            search_state'.observed_shared_memory
            initial_search_state.observed_shared_memory
            (*options.eager_mode.em_shared_memory*)
        in

        if bt_diff = [] && (not options.eager_mode.eager_local_mem || Pset.is_empty sm_diff) then
          (* search terminated after exploring all reachable states *)
          let () = init_search_state := Some initial_search_state in
          result
        else begin
          if Globals.is_verbosity_at_least Globals.ThrottledInformation then begin
            otStrLine "%s Finished exhaustive search %d (did not reach a fixed point)."
              (sprint_time ())
              n
            |> Screen.show_message ppmode;
            print_diffs search_state' bt_union bt_diff sm_union sm_diff;
            if options.allow_partial then print_partial_results search_state';
          end;

          let options' =
            { initial_search_state.options with
              eager_mode =
                { initial_search_state.options.eager_mode with
                  em_shared_memory = sm_union
                };
            }
          in

          let system_state' =
            let model_params = ConcModel.model_params system_state in
            let model_params' =
              { model_params with
                t = {model_params.t with branch_targets = bt_union};
              }
            in
            ConcModel.set_model_params model_params' system_state
          in

          { initial_search_state with
            search_nodes            = [];
            observed_branch_targets = bt_union;
            observed_shared_memory  = sm_union;
            options                 = options';
          }
          |> add_search_node system_state'
          |> search_fixed_point (n + 1)
        end
    | result -> result
  in

  let search_random initial_search_state : search_outcome =
    let search_state = ref initial_search_state in
    try
      (* we use for-loop because js_of_ocaml does not handle tail call so well *)
      for _ = 0 to options.pseudorandom_traces - 1 do
        match search !search_state with
        | Complete search_state' ->
            let (bt_union, bt_diff) =
              Params.union_and_diff_branch_targets
                search_state'.observed_branch_targets
                (!search_state).observed_branch_targets
            in

            let (sm_union, sm_diff) =
              Params.union_and_diff_shared_memory
                search_state'.observed_shared_memory
                (!search_state).observed_shared_memory
            in

            if bt_diff = [] && (not options.eager_mode.eager_local_mem || Pset.is_empty sm_diff) then
              search_state := {search_state' with search_nodes = initial_search_state.search_nodes}
            else begin
              if Globals.is_verbosity_at_least Globals.ThrottledInformation then
                print_diffs search_state' bt_union bt_diff sm_union sm_diff;

              let options' =
                { (!search_state).options with
                  eager_mode =
                    { (!search_state).options.eager_mode with
                      em_shared_memory = sm_union
                    };
                }
              in

              let system_state' =
                let model_params = ConcModel.model_params system_state in
                let model_params' =
                  { model_params with
                    t = {model_params.t with branch_targets = bt_union};
                  }
                in
                ConcModel.set_model_params model_params' system_state
              in

              search_state :=
                { search_state' with
                  search_nodes            = [];
                  observed_branch_targets = bt_union;
                  observed_shared_memory  = sm_union;
                  options                 = options';
                }
                |> add_search_node system_state'
            end

        (* This is just to break out of the loop; caught a few lines below *)
        | result -> raise (RandomResult result)
      done;
      Complete !search_state
    with
    | RandomResult result -> result
  in

  let search_results =
    if options.pseudorandom then search_random initial_search_state
    else search_fixed_point 1 initial_search_state
  in

  (* restore interrupts *)
  List.iter (fun o -> o ()) old_signal_behaviors;

  search_results

end (* Make *)
