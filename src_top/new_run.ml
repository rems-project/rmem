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
open MachineDefTypes

module Make (ConcModel: Concurrency_model.S) = struct

module StateHashSet = Set.Make(String)

module TidMap = Map.Make(struct
  type t = MachineDefTypes.thread_id
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
  type t = MachineDefTypes.thread_id * MachineDefTypes.ioid * MachineDefTypes.exception_type
  let compare (tid1, ioid1, e1) (tid2, ioid2, e2) =
    begin match (Pervasives.compare tid1 tid2, Pervasives.compare ioid1 ioid2) with
    | (0, 0) -> MachineDefTypes.exception_type_compare e1 e2
    | (0, c)
    | (c, _) -> c
    end
end)

type sst_predicate = ConcModel.system_state_and_transitions -> bool
type trans_predicate = ConcModel.trans -> bool
type state_and_trans_predicate = ConcModel.system_state -> ConcModel.trans -> bool

type breakpoint_reason =
  | NoReason
  | StateReason of ConcModel.system_state_and_transitions
  | TransReason of ConcModel.trans * ConcModel.system_state_and_transitions
  | SharedReason of ConcModel.trans * ConcModel.system_state_and_transitions * MachineDefTypes.footprint

type breakpoint_predicate =
  | StateBreakpoint of (ConcModel.system_state_and_transitions -> breakpoint_reason)
  | TransitionBreakpoint of (ConcModel.system_state_and_transitions -> ConcModel.trans -> breakpoint_reason)
  | SharedBreakpoint of (MachineDefTypes.footprint list ->
                         ConcModel.system_state_and_transitions ->
                         ConcModel.trans ->
                         breakpoint_reason)

type breakpoint_id =
  | Numbered of int
  | Named of int * string

type breakpoint = (breakpoint_id * (breakpoint_predicate * string))

type search_node =
  { system_state: ConcModel.system_state_and_transitions;

    (* [] - not explored yet
       tn :: ... :: [t1] - t1 is transition that used to be in unexplored_transitions
                           (actually the index of the transition in system_state.sst_system_transitions);
                           the other transitions are eager transitions that followed *)
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
    (* model deadlock states *)
    observed_deadlocks:        (trace * int) StateMap.t;
    (* unhandled ISA exception states *)
    observed_exceptions:       (trace * int) ExceptionMap.t;

    observed_branch_targets: MachineDefTypes.branch_targets_map;
    observed_shared_memory:  MachineDefTypes.footprint Pset.set;

    (* REMOVE: *)
    (*** shared memory approximation ***)
    read_locations:      (int (* tid *) , (MachineDefTypes.footprint list)) Pmap.map;
    written_locations:   (int (* tid *) , (MachineDefTypes.footprint list)) Pmap.map;
    shared_locations:    MachineDefTypes.footprint list;
    shared_program_locations: Sail_impl_base.address Pset.set;

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
    bounds:        sst_predicate list;

    (* back track when these states are reached but also record them as if they were final *)
    targets:       sst_predicate list;

    (* filter out transitions that do not match all of these predicates,
    i.e., a transition must satisfy all the filters to not be filtered out *)
    filters:       trans_predicate list;

    test_info:     Test.info;
    options:       RunOptions.t;
    ppmode:        Globals.ppmode;
}

let coalesce_sorted_footprints (list: MachineDefTypes.footprint list) =
  (* list must be sorted by increasing start address and then increasing size *)
  let rec coalesce list acc =
    let int_of a = Nat_big_num.to_int (Sail_impl_base.integer_of_address a) in
    match list with
    | []  -> acc
    | [x] -> x :: acc
    | (x :: y :: l) -> let ((a1, s1), (a2, s2)) = (x, y) in
                      if a1 = a2 then
                        (* x and y start at the same address,
                           take the larger size *)
                        coalesce ((a1, max s1 s2) :: l) acc
                      else if (int_of a2) <= (int_of a1) + s1 then
                        if (int_of a2) + s2 <= (int_of a1) + s1 then
                          (* x fully contains y, drop y *)
                          coalesce (x :: l) acc
                        else
                          (* x and y overlap, merge them *)
                          coalesce ((a1, (int_of a2) + s2 - (int_of a1)) :: l) acc
                      else
                        (* x and y are separate, move on *)
                        coalesce (y :: l) (x :: acc)
  in
  coalesce list []

(* TODO: make better, probably using a sweep line approach or something *)
let find_overlaps
      (read_locations :    (int, MachineDefTypes.footprint list) Pmap.map)
      (written_locations : (int, MachineDefTypes.footprint list) Pmap.map) =
  (* let decorate (tid, locations) = List.map (fun loc -> (tid, loc)) locations in *)
  (* let undecorate locations = List.map (fun (tid, loc) -> loc) locations in *)
  (* let decorated_locations = List.concat (List.map decorate (Pmap.bindings_list written_locations)) in *)
  (* let compare_decorated (_, x) (_, y) = MachineDefTypes.footprintCompare x y in *)
  (* let sorted_decorated = List.sort compare_decorated decorated_locations in *)
  (* simple test-all-pairs approach for now *)
  let int_of a = Nat_big_num.to_int (Sail_impl_base.integer_of_address a) in
  let addr_of i = Sail_impl_base.address_of_integer (Nat_big_num.of_int i) in
  let overlap (a1', s1) (a2', s2) : MachineDefTypes.footprint list =
    let a1 = int_of a1' in
    let a2 = int_of a2' in
    if a1 >= a2 && a1 + s1 <= a2 + s2 then
      (* 2 fully contains 1 *)
      [(addr_of a1, s1)]
    else if a2 >= a1 && a2 + s2 <= a1 + s1 then
      (* 1 fully contains 2 *)
      [(addr_of a2, s2)]
    else if a2 < a1 + s1 && a2 + s2 > a1 + s1 then
      (* 2 starts inside 1 but ends outside *)
      [(addr_of a2, (a1 + s1) - a2)]
    else if a1 < a2 + s2 && a1 + s1 > a2 + s2 then
      (* 1 starts inside 2 but ends outside *)
      [(addr_of a1, (a2 + s2) - a1)]
    else
      (* -> a2 >= a1 + s1 || a1 >= a2 + s2, no overlap *)
      []
  in
  let one_thread (tid, locations) =
    let others = List.concat (List.map snd (Pmap.bindings_list (Pmap.remove tid written_locations))) in
    let test_one loc = List.concat (List.map (overlap loc) others) in
    List.concat (List.map test_one locations)
  in
  coalesce_sorted_footprints
    (List.sort
       MachineDefTypes.footprintCompare
       (List.concat (List.map one_thread (Pmap.bindings_list written_locations)))
     @ (List.concat (List.map one_thread (Pmap.bindings_list read_locations))))

let all_instructions_of_state state =
  let thread_set = Pmap.range compare state.thread_states in
  let all_instructions =
    Pset.map compare (MachineDefThreadSubsystemUtils.ts_instructions state.t_model) thread_set in
  (Pset.fold Pset.union all_instructions (Pset.empty compare))

let get_shared_program_locations system_state new_shared_locations =
  (* TODO: unnecessarily quadratic? cache some of this / reason about what must have already been recorded? *)
  let instructions = all_instructions_of_state system_state in
  let new_shared_instructions = Pset.filter
                                  (fun ii ->
                                    MachineDefFragments.non_empty_intersection_set
                                      (Pset.from_list compare new_shared_locations)
                                      (MachineDefThreadSubsystem.footprints_of_instruction_instance ii))
                                  instructions
  in
  Pset.map compare (fun ii -> ii.program_loc) new_shared_instructions

let record_read_location tid footprint state =
  if state.options.record_shared_locations then
    let current_list =
      (try Pmap.find tid state.read_locations
       with Not_found -> []) in
    let new_list = coalesce_sorted_footprints
                     (List.sort_uniq
                        MachineDefTypes.footprintCompare
                        (footprint @ current_list)) in
    let new_read_locations = Pmap.add tid new_list state.read_locations in
    let new_shared_locations = find_overlaps new_read_locations state.written_locations in
    let new_program_locations =
      match List.hd state.search_nodes with
      | root_node ->
          get_shared_program_locations
              root_node.system_state.sst_state
              new_shared_locations
          |> Pset.union state.shared_program_locations
      | exception Failure _ -> state.shared_program_locations
    in
    { state with
      read_locations = new_read_locations;
      shared_locations = new_shared_locations;
      shared_program_locations = new_program_locations;
    }
  else
    state


let record_write_location tid footprint state =
  if state.options.record_shared_locations then
    let current_list =
      (try Pmap.find tid state.written_locations
       with Not_found -> []) in
    let new_list = coalesce_sorted_footprints
                     (List.sort_uniq
                        MachineDefTypes.footprintCompare
                        (footprint @ current_list)) in
    let new_written_locations = Pmap.add tid new_list state.written_locations in
    let new_shared_locations = find_overlaps state.read_locations new_written_locations in
    let new_program_locations =
      match List.hd state.search_nodes with
      | root_node ->
        get_shared_program_locations
             root_node.system_state.sst_state
             new_shared_locations
        |> Pset.union state.shared_program_locations
      | exception Failure _ -> state.shared_program_locations
    in
    { state with
      written_locations = new_written_locations;
      shared_locations = new_shared_locations;
      shared_program_locations = new_program_locations;
    }
  else
    state


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
    Screen.show_message search_state.ppmode "%s\n%s"
      "                  |transit-|hashed |hash   |restart|discard|write|priori-|loop   "
      "           traces |ions    |states |prunes |prunes |prunes |prune|ty red.|limit  ";
    (*"[##:##:##] #######|########|#######|#######|#######|#######|#####|#######|#######"*)

  Screen.show_message search_state.ppmode "%s %7d|%8d|%7d|%7d|%7d|%7d|%5d|%7d|%7d"
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

let sprint_branch_targets ppmode branch_targets : string =
  List.map (fun (tid, tbts) ->
    List.map (fun (addr, addrs) ->
      List.map (fun a -> Printf.sprintf "%s" (Pp.pp_address ppmode None a)) addrs
      |> String.concat ", "
      |> Printf.sprintf "%d:%s -> {%s};" tid (Pp.pp_address ppmode None addr)
    ) tbts
    |> String.concat " "
  ) branch_targets
  |> String.concat " "

let sprint_shared_memory ppmode shared_memory : string =
  Pset.elements shared_memory
  |> List.map (fun fp -> Printf.sprintf "%s;" (Pp.pp_footprint ppmode None fp))
  |> String.concat " "

let print_last_state title search_state : unit =
  let ppmode = search_state.ppmode in

  Screen.show_message ppmode "***** %s *****" title;
  begin match search_state.search_nodes with
  | [] -> Screen.show_message ppmode "no state"

  | [node] ->
      let (ppmode', ui_state) =
        ConcModel.make_ui_system_state ppmode None node.system_state.sst_state []
      in
      Screen.show_message ppmode "%s"              (Pp.pp_ui_system_state ppmode' ui_state)

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
          let tran = List.nth node'.system_state.sst_system_transitions t in
          let (ppmode', ui_state) =
            ConcModel.make_ui_system_state ppmode (Some node'.system_state.sst_state) node.system_state.sst_state []
          in
          Screen.show_message ppmode "%s"              (Pp.pp_ui_system_state ppmode' ui_state);
          Screen.show_message ppmode "via %s"          (pp_choices (choices_so_far search_state));
          Screen.show_message ppmode "last (non eager) transition: %s"
            (Pp.pp_trans ppmode tran);
          if ts <> [] then
            Screen.show_message ppmode "(followed by eager transitions: %s)"
              (List.map string_of_int ts |> String.concat ",");

          Screen.show_message ppmode  "all transitions: [\n%s]"
              (Pp.pp_list ppmode (fun t -> Pp.pp_trans ppmode t ^ "\n") node.system_state.sst_system_transitions);
          Screen.show_message ppmode  "unexplored (filtered) transitions: [\n%s]"
              (Pp.pp_list ppmode (fun (i, t) -> "[" ^ string_of_int i ^ "] " ^ Pp.pp_trans ppmode t ^ "\n") node.unexplored_transitions)
      end
  end;
  Screen.show_message ppmode "**********************************"

let update_observed_branch_targets_and_shared_memory search_state search_node : search_state =
  let observed_branch_targets =
    let (union, diff) =
      MachineDefSystem.union_and_diff_branch_targets
        (ConcModel.branch_targets_of_state search_node.system_state.sst_state)
        search_state.observed_branch_targets
    in
    if diff <> [] && Globals.is_verbosity_at_least Globals.ThrottledInformation then
      Screen.show_message search_state.ppmode "%s found new branch-register target(s): %s"
        (sprint_time ())
        (sprint_branch_targets search_state.ppmode diff);
    union
  in

  let observed_shared_memory =
    if search_state.options.eager_mode.eager_local_mem then
      let (union, diff) =
        MachineDefSystem.union_and_diff_shared_memory
          (ConcModel.shared_memory_of_state search_node.system_state.sst_state)
          search_state.observed_shared_memory
      in
      if not (Pset.is_empty diff) && Globals.is_verbosity_at_least Globals.ThrottledInformation then
        Screen.show_message search_state.ppmode "%s found new shared memory footprint(s): %s"
          (sprint_time ())
          (sprint_shared_memory search_state.ppmode diff);
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
let reduced_final_state regs mem (system_state: ConcModel.system_state) =
  let reg_state =
    let reg_values_of_thread state =
      let final_value = function
        | Some v ->
            begin match Sail_impl_base.integer_of_register_value v with
            | Some i -> Test.C.big_num_to_value i
            | None -> failwith "register final value has unknown/undef bits"
            end
        | None -> failwith "register final value read is blocked"
      in

      List.filter
        (fun (reg, _) -> List.mem ((system_state.t_model.ts_tid state), reg) regs)
        (system_state.t_model.ts_final_reg_state state)
      |> List.map (fun (reg, value) -> (reg, final_value value))
    in

    Pmap.bindings_list system_state.thread_states
    |> List.map (fun (tid, state) -> (tid, reg_values_of_thread state))
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

  let final_state = reduced_final_state search_state.test_info.Test.show_regs search_state.test_info.Test.show_mem search_node.system_state.sst_state in

  if ConcModel.is_final_state search_node.system_state.sst_state then
    let (choices, count) =
      try StateMap.find final_state search_state.observed_finals with
      | Not_found -> (choices_so_far search_state, 0)
    in

    let observed_finals = StateMap.add final_state (choices, count + 1) search_state.observed_finals in

    let observed_filterred_finals =
      if  match search_state.test_info.Test.filter with
          | None -> true
          | Some filter ->
              reduced_final_state search_state.test_info.Test.filter_regs search_state.test_info.Test.filter_mem search_node.system_state.sst_state
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
    let (choices, count) =
      try StateMap.find final_state search_state.observed_deadlocks with
      | Not_found -> (choices_so_far search_state, 0)
    in
    {search_state with observed_deadlocks =
        StateMap.add final_state (choices, count + 1) search_state.observed_deadlocks}


let is_eager options shared_locations shared_program_locations sst = fun transition ->
  let inherently_eager = MachineDefTransitionUtils.is_eager_transition
    sst.sst_state.model
    options.eager_mode
    transition
  in
  let touches_shared_location = (List.exists (fun fp -> MachineDefTransitionUtils.trans_reads_footprint fp sst transition
                                                        || MachineDefTransitionUtils.trans_writes_footprint fp sst transition)
                                             shared_locations)
  in
  let (tid, iid) = MachineDefTypes.principal_ioid_of_trans transition in
  let is_shared_program_location =
    match Pmap.lookup tid sst.sst_state.thread_states with
    | None -> false
    | Some ts ->
       Lem.is_some 
         (MachineDefThreadSubsystemUtils.ts_find_instruction sst.sst_state.t_model
            (fun _ i -> i.instance_ioid = (tid,iid) && 
                          not (Pset.mem i.program_loc shared_program_locations))
            ts)
  in
  inherently_eager || (options.eager_up_to_shared && MachineDefTransitionUtils.is_rw_transition transition
                       && not touches_shared_location && not is_shared_program_location)

let is_eager_in_search search_state = fun transition ->
  match search_state.search_nodes with
  | [] -> assert false
  | node :: _ ->
     is_eager search_state.options
              search_state.shared_locations
              search_state.shared_program_locations
              node.system_state
              transition

let add_search_node system_state search_state : search_state =
  let filtered_transitions =
    List.mapi (fun i t -> (i, t)) system_state.sst_system_transitions
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

type take_transition_result =
  | NextState of search_state
  | TransBreakpoint of search_state * int * ConcModel.trans * breakpoint * breakpoint_reason

let take_transition search_state sst (i, transition) eager : take_transition_result =
  let m = search_state.ppmode in
  begin match search_state.search_nodes with
  | [] -> assert false
  | search_node :: search_nodes ->
     if Globals.is_verbosity_at_least Globals.Debug then begin
         Screen.show_message m "Taking %s transition: [%d] %s\nof: (%d) [\n%s\n] (open_transition = [%s])"
                             (if eager then "eager" else "### NON-EAGER ###")
                             i
                             (Pp.pp_trans m transition)
                             (List.length search_node.unexplored_transitions)
                             (Pp.pp_list m
                                         (fun (i, t) -> "[" ^ string_of_int i ^ "] " ^ Pp.pp_trans m t ^ "\n")
                                         search_node.unexplored_transitions)
                             (Pp.pp_list m string_of_int search_node.open_transition)
       end;

     (* Invariant: either
        - we're at the initial state, or
        - the previous state has at least one open transition *)
     if not (search_nodes = [] || (List.hd search_nodes).open_transition <> []) then
       failwith "not at initial state and open_transition of previous state is empty";

     (* Invariant: if we're taking a non-eager transition, this is a fresh search node *)
     if not eager && not (search_node.open_transition = []) then
       failwith "trying to take a non-eager transition of a non-fresh node (open_transition <> [])";

     (* Invariant: if we're taking a non-eager transition, we've taken all eager transitions already *)
     if not eager && (List.exists
                        (fun (_, t) -> is_eager_in_search search_state t)
                        search_node.unexplored_transitions) then
       failwith "trying to take a non-eager transition, but eager transitions still exist";


     (* Ask all breakpoints whether they want to fire *)
     let reasons =
      List.map (fun bp ->
          let (_, (pred, _)) = bp in
          match pred with
          | StateBreakpoint _ -> (bp, NoReason)
          | TransitionBreakpoint f ->
              (bp, f search_node.system_state transition)
          | SharedBreakpoint f ->
              (bp, (f search_state.shared_locations
                      search_node.system_state
                      transition))
        )
        search_state.breakpoints
      |> List.filter (fun (_, r) -> r <> NoReason)
     in

     begin match reasons with
     | (bp, reason) :: _ -> TransBreakpoint (search_state, i, transition, bp, reason)
     | [] ->
       (* No breakpoints want to fire. Take the transition. *)
       begin match ConcModel.sst_after_transition search_state.options sst transition with
       | TO_unhandled_exception (tid, ioid, e) ->
          (* SF: I don't think this will ever happen *)
          NextState (record_exception search_state (tid, ioid, e))
       | TO_system_state sst' ->
          let search_state' =
            if eager then
              (* If the transition was eager, pop and re-push the head node,
                 prepending it to the previous node's open_transition *)
              match (pop search_state) with
              (* Special case: if search_nodes = [] then we're at the initial state
                 and shouldn't record the eager transition, since we pretend
                 it's been taken by the eager tail of the nonexistent previous node.
                 TODO FIXME: we probably want to record them somehow for completeness. *)
              | { search_nodes = [] } as search_state' ->
                 search_state'
              | { search_nodes = search_node' :: search_nodes' } as search_state' -> begin
                  { search_state' with search_nodes =
                                         { search_node' with
                                           open_transition = i :: search_node'.open_transition }
                                         :: search_nodes';
                  }
              end
            else
              (* Otherwise we push a new node. We have previously asserted that open_transition = [] *)
              { search_state with
                search_nodes = { search_node with
                                 open_transition = [i];
                                 unexplored_transitions = List.remove_assoc i search_node.unexplored_transitions;
                               } :: search_nodes;
              }
          in
          let search_state' = add_search_node sst' search_state' in

          (* Update shared memory approximation *)
          let maybe_thread_id = MachineDefTypes.thread_id_of_thread_transition transition in
          let maybe_read_footprint = MachineDefTransitionUtils.read_footprint_of_trans transition in
          let write_footprints = MachineDefTransitionUtils.write_footprints_of_trans transition in
          let search_state' =
            (match (maybe_thread_id, maybe_read_footprint) with
             | (Some tid, Some footprint) -> record_read_location tid [footprint] search_state'
             | _ -> search_state') in
          let search_state' =
            (match (maybe_thread_id, write_footprints) with
             | (Some tid, _ :: _) -> record_write_location tid write_footprints search_state'
             | _ -> search_state') in

          (* Return new state *)
          NextState search_state'
       end
     end
  end

let rec some_if_any (f : 'a -> 'b option) : 'a list -> 'b option = function
  | []     -> None
  | h :: t -> match f h with None -> some_if_any f t | s -> s

(* when set to true the search should stop *)
let interrupt = ref false;;

type search_outcome =
  | Complete    of search_state
  | Breakpoint  of search_state * breakpoint * breakpoint_reason (* which breakpoint was hit and why *)
  | Interrupted of search_state * string (* explanation of why interrupted *)

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
    | Some reason -> Interrupted (search_state, reason)
    | None -> cont search_state
  in

  let check_state_breakpoints cont = fun search_state ->
    begin match search_state.search_nodes with
    | [] -> assert false
    | search_node :: _ ->
        if search_node.open_transition = [] then
          let reasons =
            List.map (fun bp ->
                let (_, (pred, _)) = bp in
                match pred with
                | StateBreakpoint f -> (bp, f search_node.system_state)
                | TransitionBreakpoint _ -> (bp, NoReason)
                | SharedBreakpoint _ -> (bp, NoReason))
              search_state.breakpoints
          in

          match List.find (function (_, NoReason) -> false | _ -> true) reasons with
          | (bp, reason) -> Breakpoint (search_state, bp, reason)
          | exception Not_found -> cont search_state
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
          record_final_state search_state search_node |> pop |> search
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
          pop search_state |> search
        else
          cont search_state
    end
  in

  let prune_restarts cont = fun search_state ->
    begin match search_state.search_nodes with
    | [] -> assert false
    | search_node :: _ ->
        if search_node.open_transition = [] && search_state.options.prune_restarts
            && search_node.system_state.sst_inst_restarted
        then
          { search_state with
            restart_prune_count = search_state.restart_prune_count + 1
          } |> pop |> search
        else
          cont search_state
    end
  in

  let prune_discards cont = fun search_state ->
    begin match search_state.search_nodes with
    | [] -> assert false
    | search_node :: _ ->
        if search_node.open_transition = [] && search_state.options.prune_discards
            && search_node.system_state.sst_inst_discarded
        then
          { search_state with
            discard_prune_count = search_state.discard_prune_count + 1
          } |> pop |> search
        else
          cont search_state
    end
  in

  let prune_late_writes cont = fun search_state ->
    begin match search_state.search_nodes with
    | [] -> assert false
    | search_node :: _ ->
        if search_node.open_transition = [] && search_state.options.prune_late_writes
            && search_node.system_state.sst_write_after_stop_promising
        then
          { search_state with
            late_write_prune_count = search_state.late_write_prune_count + 1
          } |> pop |> search
        else
          cont search_state
    end
  in

  let take_eager_transition cont = fun search_state ->
    begin match search_state.search_nodes with
    | [] -> assert false
    | search_node :: _ ->
        if search_node.open_transition = [] then
          begin match List.find
              (fun (_, t) -> is_eager_in_search search_state t)
              search_node.unexplored_transitions
          with
          | transition ->
              begin match take_transition search_state search_node.system_state transition true with
              | NextState next_state -> search next_state
              | TransBreakpoint (break_state, i, transition, bp, reason) -> Breakpoint (search_state, bp, reason)
              | exception Failure s ->
                  Interrupted (search_state, Printf.sprintf "encountered an exception while taking an eager transition\nFailure '%s'" s)
              end
          | exception Not_found -> cont search_state
          end
        else
          cont search_state
    end
  in

  (*
  let assert_no_eager cont = fun search_state ->
    begin match search_state.search_nodes with
    | [] -> assert false
    | search_node :: _ ->
        let m = search_state.ppmode in
        if search_node.open_transition <> [] then begin
            let leftovers = List.filter
                              (fun (_, t) -> is_eager_in_search search_state t)
                              search_node.unexplored_transitions
            in
            if leftovers <> [] then begin
                Screen.show_message m "there was a leftover eager transition after all should have been taken!\n[\n%s\n]"
                                    (Pp.pp_list m (fun (i, t) -> Printf.sprintf "[%d] %s (eager: %b)\n" i (Pp.pp_trans m t) (is_eager_in_search search_state t)) search_node.unexplored_transitions);
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
             && not (search_node.system_state.sst_state.MachineDefTypes.s_model.ss_stopped_promising 
                     search_node.system_state.sst_state.storage_subsystem)
        then
          let hash =
            ConcModel.make_ui_system_state Globals.ppmode_for_hashing None search_node.system_state.sst_state []
            |> snd
            |> Pp.pp_ui_system_state Globals.ppmode_for_hashing
            |> Digest.string
          in

          if StateHashSet.mem hash search_state.state_hashes then
            (* we have already seen this state *)
            { search_state with
              prune_count = search_state.prune_count + 1;
            } |> pop |> search
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
            && search_node.system_state.sst_state.model.t.thread_loop_unroll_limit <> None
            && search_state.options.prune_discards
        then
          let is_loop_limit (_, t) =
            MachineDefTransitionUtils.is_loop_limit_exception_transition t
          in
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
          MachineDefTransitionUtils.priority_transitions 
            search_node.system_state.sst_state.model
            (ConcModel.final_ss_state search_node.system_state.sst_state)
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
                      let cmp_ioid (_, t1) (_, t2) =
                        match (t1, t2) with
                        | (T_trans t1, T_trans t2) ->
                            compare
                              (MachineDefTypes.ioid_of_thread_trans t1)
                              (MachineDefTypes.ioid_of_thread_trans t2)
                        | _ -> assert false
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

            match take_transition search_state' search_node'.system_state transition false with
            | NextState next_state -> search next_state
            | TransBreakpoint (break_state, i, transition, bp, reason) -> Breakpoint (search_state, bp, reason)
        end
    end
  in

  let check_final cont = fun search_state ->
    begin match search_state.search_nodes with
    | [] -> Complete search_state (* no more nodes to explore *)
    | search_node :: _ ->
        if search_node.open_transition = [] && search_node.system_state.sst_system_transitions = [] then
          record_final_state search_state search_node |> pop |> search
        else
          cont search_state
    end
  in

  begin match search_state.search_nodes with
  | [] -> Complete search_state (* no more nodes to explore *)

  | _ :: _ ->
      (* explore the transitions of the head node *)
      print_status_message search_state;

      ( check_limits
        @@ check_state_breakpoints
        @@ check_targets
        @@ check_bound
        @@ prune_restarts
        @@ prune_discards
        @@ prune_late_writes
        @@ take_eager_transition
        (* @@ assert_no_eager *)
        @@ hash_prune
        @@ loop_limit
        @@ priority_reduction
        @@ check_final
        @@ take_next_transition
        (* if all else fails, pop and continue *)
        @@ (fun search_state -> pop search_state |> search)
      ) search_state
  end

exception RandomResult of search_outcome

let search_from_state
    ?(ppmode=Globals.get_ppmode ())
    (options:        RunOptions.t)
    (test_info:      Test.info) (* TODO: probably should be abstracted,
                                used only in reduced_final_state *)
    (system_state:   ConcModel.system_state_and_transitions)
    (breakpoints:    breakpoint list)
    (bounds:         sst_predicate list)
    (targets:        sst_predicate list)
    (filters:        trans_predicate list)
    (print_partial_results: search_state -> unit)
    : search_outcome
  =
  let started_timestamp = Sys.time () |> int_of_float in

  let initial_search_state =
    { search_nodes = [];

      state_hashes        = StateHashSet.empty;

      observed_finals           = StateMap.empty;
      observed_filterred_finals = StateMap.empty;
      observed_deadlocks        = StateMap.empty;
      observed_exceptions       = ExceptionMap.empty;

      observed_branch_targets = system_state.sst_state.model.t.branch_targets;
      observed_shared_memory  = system_state.sst_state.model.shared_memory;

      (* REMOVE: *)
      read_locations      = Pmap.empty compare;
      written_locations   = Pmap.empty compare;
      shared_locations    = [];
      shared_program_locations = Pset.empty compare;

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

      (* eager_up_to_shared must imply record_shared_locations to work *)
      options   = { options with
                    record_shared_locations =
                      options.record_shared_locations || options.eager_up_to_shared;
                  };
      ppmode    = ppmode;
    } |> add_search_node system_state
  in

  interrupt := false;

  let old_signal_behaviors =
    if options.allow_partial then
      let set_signal_with_restore (sig_name, sig_num) : unit -> unit =
        let interrupt_handler n =
          Screen.show_message ppmode "received interrupt signal %s (%d)" sig_name n;
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

(* REMOVE:
  let search_iter search_state =
    if options.pseudorandom then
      (* we use for-loop because js_of_ocaml does not handle tail call so well *)
      let r = ref (Complete search_state) in
      for _ = 0 to options.pseudorandom_traces - 1 do
        match !r with
        | Breakpoint _ -> ()
        | Interrupted _ -> ()
        | Complete r' ->
           r := search {r' with search_nodes = search_state.search_nodes}
      done;
      !r
    else
      search search_state
  in

  let search_iter_count = ref 0 in

  let rec search_until_fixed_shared search_state =
    let m = search_state.ppmode in
    if Globals.is_verbosity_at_least Globals.UnthrottledInformation then begin
        search_iter_count := !search_iter_count + 1;
        Screen.show_message m "Fixed shared iteration %d: [%s]" !search_iter_count (Pp.pp_list m (Pp.pp_raw_footprint m) search_state.shared_locations);
      end;
    let previous_shared_locations = search_state.shared_locations in
    let results = search_iter search_state in
    match results with
    | Breakpoint _
      | Interrupted _ -> results
    | Complete state' ->
       let new_shared_locations = state'.shared_locations in
       if Globals.is_verbosity_at_least Globals.UnthrottledInformation then
         Screen.show_message m "                      completed with %d transitions" state'.transitions_count;
       if previous_shared_locations = new_shared_locations then begin
           if Globals.is_verbosity_at_least Globals.UnthrottledInformation then
             Screen.show_message m "                      and reached fixed point";
           results
         end
       else
         search_until_fixed_shared { search_state with read_locations = state'.read_locations;
                                                       written_locations = state'.written_locations;
                                                       shared_locations = state'.shared_locations;
                                   }
  in

  let search_results =
    if options.eager_up_to_shared then
      search_until_fixed_shared init_search_state
    else
      search_iter init_search_state
  in
*)

  let search_random initial_search_state : search_outcome =
    let search_state = ref initial_search_state in
    try
      (* we use for-loop because js_of_ocaml does not handle tail call so well *)
      for _ = 0 to options.pseudorandom_traces - 1 do
        match search !search_state with
        | (Complete search_state') as result ->
            let (_, bt_diff) =
              MachineDefSystem.union_and_diff_branch_targets
                search_state'.observed_branch_targets
                initial_search_state.observed_branch_targets
            in
            if bt_diff <> [] then
              raise (RandomResult result); (* caught a few lines below *)


            if options.eager_mode.eager_local_mem &&
              let (_, sm_diff) =
                MachineDefSystem.union_and_diff_shared_memory
                  search_state'.observed_shared_memory
                  initial_search_state.observed_shared_memory
              in
              not (Pset.is_empty sm_diff)
            then raise (RandomResult result); (* caught a few lines below *)

            search_state := {search_state' with search_nodes = initial_search_state.search_nodes}

        | result -> raise (RandomResult result) (* caught a few lines below *)
      done;
      Complete !search_state
    with
    | RandomResult result -> result
  in

  let search_results =
    if options.pseudorandom then search_random initial_search_state
    else search initial_search_state
  in

  (* restore interrupts *)
  List.iter (fun o -> o ()) old_signal_behaviors;

  search_results

end (* Make *)
