(*========================================================================================================*)
(*                                                                                                        *)
(*                rmem executable model                                                                   *)
(*                =====================                                                                   *)
(*                                                                                                        *)
(*  This file is:                                                                                         *)
(*                                                                                                        *)
(*  Copyright Shaked Flur, University of Cambridge                                            2014-2018   *)
(*  Copyright Peter Sewell, University of Cambridge                                2011-2012, 2014-2016   *)
(*  Copyright Susmit Sarkar, University of St Andrews                              2011-2012, 2014-2015   *)
(*  Copyright Christopher Pulte, University of Cambridge                                      2015-2018   *)
(*  Copyright Jon French, University of Cambridge                                             2017-2018   *)
(*  Copyright Luc Maranget, INRIA Paris                                           2011-2012, 2015, 2017   *)
(*  Copyright Kathy Gray, University of Cambridge (when this work was done)                   2014-2015   *)
(*  Copyright Pankaj Pawan, IIT Kanpur and INRIA (when this work was done)                    2011-2012   *)
(*  Copyright Ohad Kammar, University of Cambridge (when this work was done)                       2013   *)
(*  Copyright Dominic Mulligan, University of Cambridge (when this work was done)                  2013   *)
(*                                                                                                        *)
(*  All rights reserved.                                                                                  *)
(*                                                                                                        *)
(*  It is part of the rmem tool, distributed under the 2-clause BSD licence in                            *)
(*  LICENCE.txt.                                                                                          *)
(*                                                                                                        *)
(*========================================================================================================*)

open RunOptions
open Globals
open MachineDefTypes
open Interact
open ConstrGen
open Screen_base

module StateHashSet = Set.Make(String)
module TidMap = Map.Make(struct type t = MachineDefTypes.thread_id
                                let compare = compare
                         end)

(* given the thread index, tid, and the instruction offset in the
thread, n, return the memory address of the instruction *)
let aval_of_inst_index tid n = Nat_big_num.of_int (0x50000 + 0x1000 * tid + n)

let big_num_to_int64 i : Int64.t =
  if Nat_big_num.greater i (Nat_big_num.of_int64 Int64.max_int) then
    Nat_big_num.to_int64 (Nat_big_num.sub i (Nat_big_num.pow_int_positive 2 64))
  else
    Nat_big_num.to_int64 i

(********************************************************************)
module type S = sig
  val calc_finals:
    RunOptions.t ->
    Globals.ppmode ->
    Test.info ->
    MachineDefSystem.initial_state_record list ->
    unit
end
(********************************************************************)

(* when set to true the search should stop *)
let interrupt = ref false;;

module Make (ConcModel: Concurrency_model.S) : S = struct

let get_graph_backend () =
  begin match !Globals.graph_backend with
  | Globals.Dot -> (module Graphviz.S : Pp.GraphBackend)
  | Globals.Tikz -> (module Tikz.S : Pp.GraphBackend)
  end

let write_snapshot ppmode snapshot : unit =
  let (filename, snapshots) = !Globals.snapshot_data in
  let snapshots' = snapshot :: snapshots in
  Globals.snapshot_data := (filename, snapshots');
  (* and write logfile if we've hit the second *)
  if filename <> "" then
    begin match snapshots' with
    | [(ioid', regs', mem'); (ioid, regs, mem)] ->
       let fd = open_out filename in
       output_string fd (Pp.pp_logfile ppmode (ioid, regs, mem) (ioid', regs', mem'));
       close_out fd
    | _ -> ()
    end


let snapshot_of_transition ppmode (trans: ConcModel.trans) : unit =
  (* remember any register/memory snapshot data from the transition *)
  begin match MachineDefTransitionUtils.maybe_snapshot_of_transition trans with
  | Some snapshot -> write_snapshot ppmode snapshot
  | None -> ()
  end


let pp_choices choices =
  if !Globals.deterministic_output then
    "(suppressed for deterministic comparison)"
  else
    let choices = List.rev choices in
    "\"" ^ (String.concat ";" (List.map string_of_int choices)) ^ "\""


let debug_print_state verb ppmode choices_so_far trans description state : unit =
  if Globals.is_verbosity_at_least verb then
  begin
    let (ppmode', ui_state) = ConcModel.make_ui_system_state ppmode None state [] in
    Screen.show_message ppmode "*** %s ***" description;
    Screen.show_message ppmode "";
    Screen.show_message ppmode "%s"         (Pp.pp_ui_system_state ppmode' ui_state);
    Screen.show_message ppmode "via %s"     (pp_choices choices_so_far);
    begin match trans with
    | Some trans ->
    Screen.show_message ppmode "last transition: %s" (Pp.pp_trans ppmode trans) (* HACK: should not use Pp directly *)
    | None -> ()
    end;
    Screen.show_message ppmode "***********************";
    Screen.show_message ppmode ""
  end

(********************************************************************)
(** record information about the reachable states *)

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


(* all final states paired with "choices" path and the number of times
they were observed *)
let observed_finals    : (int list * int) StateMap.t ref = ref StateMap.empty
let observed_deadlocks : (int list * int) StateMap.t ref = ref StateMap.empty
let observed_exceptions: (int list * int) ExceptionMap.t ref = ref ExceptionMap.empty
let axiomatic_failure = ref false (* failed axiomatic model *)
(* hash table of observed states *)
let state_hashes      = ref (StateHashSet.empty)

let next_branch_targets = ref (MachineDefSystem.branch_targets_from_list [])
let next_shared_memory = ref (Pset.empty MachineDefTypes.footprintCompare)

let reset_observed_states () =
  observed_finals     := StateMap.empty;
  observed_deadlocks  := StateMap.empty;
  observed_exceptions := ExceptionMap.empty;
  axiomatic_failure   := false;
  state_hashes        := StateHashSet.empty

(********************************************************************)
(** record information for statistics *)

let fixedpoint_iterations = ref 0 (* the number of completed iterations *)

let transitions_count     = ref 0 (* the total number of transitions *)
let trace_count           = ref 0 (* the total number of complete traces *)
let prune_count           = ref 0 (* the total number hash prunes *)
let por_count             = ref 0 (* the total number of transitions
                                  partial-order-reduction removed *)
let restart_prune_count   = ref 0
let discard_prune_count   = ref 0
let late_write_prune_count= ref 0
let pr_count              = ref 0
let loop_limit_count      = ref 0

let last_status_print = ref 0.0
let last_partial_results_print = ref 0.0


let reset_counters = fun () ->
  transitions_count     := 0;
  trace_count           := 0;
  prune_count           := 0;
  por_count             := 0;
  restart_prune_count   := 0;
  discard_prune_count   := 0;
  late_write_prune_count:= 0;
  pr_count              := 0;
  loop_limit_count      := 0;
  last_status_print          := 0.0;
  last_partial_results_print := 0.0

let sprint_time () =
  let time = Unix.localtime (Unix.time ()) in
  Printf.sprintf "[%02d:%02d:%02d]"
    time.Unix.tm_hour time.Unix.tm_min time.Unix.tm_sec

let print_counters ppmode header =
  if header then begin
    Screen.show_message ppmode "           fix.p|       |transit-|hashed |hash   |restart|discard|write|priori-|loop   |partial";
    Screen.show_message ppmode "           iters|traces |ions    |states |prunes |prunes |prunes |prune|ty red.|limit  |ord.red"
                             (*"[00:00:00] #####|#######|########|#######|#######|#######|#######|#####|#######|#######|#######"*)
  end;
  Screen.show_message ppmode "%s %5d|%7d|%8d|%7d|%7d|%7d|%7d|%5d|%7d|%7d|%7d"
    (sprint_time ())
    !fixedpoint_iterations
    !trace_count
    !transitions_count
    (StateHashSet.cardinal !state_hashes)
    !prune_count
    !restart_prune_count
    !discard_prune_count
    !late_write_prune_count
    !pr_count
    !loop_limit_count
    !por_count

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

(********************************************************************)

type result_kind =
  | No_transitions
  | Raised_exception of MachineDefTypes.thread_id * MachineDefTypes.ioid * MachineDefTypes.exception_type

type result_type =
  { kind:        result_kind;
    transitions: ConcModel.trans list; (* the transitions taken to reach 'state',
                             head is the last transition taken *)
    choices:     int list;   (* the choice number associated with each
                             transition in 'transitions' *)
    state:       ConcModel.system_state;
  }

type state_transitions_args =
  { test_info:     Test.info;
    options:       RunOptions.t;
    ppmode:        Globals.ppmode; (* printing information *)
    info:          (ConcModel.thread_subsystem_state,ConcModel.storage_subsystem_state) Interact.info;  (* current state of search *)
    state:         ConcModel.system_state; (* system state we're looking at *)
    on_trace_end:  RunOptions.t ->    (* continuation to feed out result to, when no more transitions *)
                   Globals.ppmode ->
                   result_type ->
                   unit;
    on_done:       RunOptions.t ->    (* continuation after done processing the sub-tree rooted at this state *)
                   Globals.ppmode ->
                   (RunOptions.t -> unit) ->
                   unit;
    on_undo:       RunOptions.t -> unit;   (* continuation to undo one step *)
    print_partial: RunOptions.t ->         (* print intermediate results *)
                   Globals.ppmode ->
                   unit;

    cached_ss_trans:     (ConcModel.ss_trans list) option;
    cached_thread_trans: (MachineDefTypes.thread_id, ConcModel.thread_subsystem_state MachineDefTypes.thread_trans list) Pmap.map;
    cached_sys_thread_trans: (MachineDefTypes.thread_id, ((ConcModel.thread_subsystem_state,ConcModel.storage_subsystem_state) MachineDefTypes.trans * bool) list) Pmap.map;

    prev_storage_trans: ConcModel.trans option;
    prev_thread_trans:  ConcModel.trans TidMap.t;

    trace_hashes:  StateHashSet.t;
  }

(* This is the recursive DFS state space search (in interactive
mode the user chooses the transitions). For performance it's important
to be tail recursive. args.on_done is the DFS stack in the form of
continuations. As the control flow of this function is quite
complicated we use continuations to break it down and make sure we
are always tail recursive:
  do_transitions - called first for each new state;
  eager_transitions_cont - do eager transitions if possible;
  hash_prune_cont - do hash prune if possible;
  partial_order_cont - use partial order reduction to reduce the candidate transitions;
  priority_cont - do high priority transitions first
  sequential_cont - remove all transitions except the first one (priority to storage transitions)
  regular_transition_cont - let the user choose transition or do all of them. *)
let rec do_transitions (args: state_transitions_args) : unit =
  if !interrupt then begin
    args.print_partial args.options args.ppmode;
    exit 1
  end;

  if args.options.sequential then
    Debug.timer_start "step_total";

  transitions_count := 1 + !transitions_count;

  (* SF: this can be a bit confusing when doing eager transitions
  as the 'via [path]' will only get you to the state before the
  last eager sequence *)
  begin match args.info.Interact.trans_so_far with
    | trans :: _ ->
        debug_print_state Globals.Debug args.ppmode args.info.choices_so_far (Some trans) "current state" args.state
    | [] ->
        debug_print_state Globals.Debug args.ppmode args.info.choices_so_far None "current state" args.state
  end;

  if Globals.is_verbosity_at_least Globals.ThrottledInformation
      && not args.options.interactive
      && Sys.time () > !last_status_print +. 10.0
  then begin
    let print_header = (!last_status_print = !last_partial_results_print) in
    last_status_print := Sys.time ();

    if args.options.allow_partial && Sys.time () > !last_partial_results_print +. (5.0 *. 60.0) then begin
      last_partial_results_print := !last_status_print;
      args.print_partial args.options args.ppmode
    end else
      print_counters args.ppmode print_header;
  end;

  begin match args.options.max_trace_length with
  | Some max_length when List.length args.info.trans_so_far >= max_length ->
      raise (Failure (Printf.sprintf "trace exceeded %d states (see -max_trace_length)" max_length))
  | Some _
  | None -> ()
  end;

  Debug.set_start_time2 ();
  let (cands, cached_ss_trans, cached_thread_trans, cached_sys_thread_trans) =
    ConcModel.enumerate_transitions args.options args.state 
      args.cached_ss_trans args.cached_thread_trans args.cached_sys_thread_trans
  in
  Debug.print_time2 "all enumerate_transitions_of_system\n";


  (* compose the continuations in the order we want them and execute *)
  ( eager_transitions_cont
    @@ hash_prune_cont   (* for space performance it's important to hash only if eager fails *)
    @@ loop_limit_cont
    @@ partial_order_cont
    @@ priority_cont
    @@ sequential_cont
    @@ regular_transition_cont
  ) args cands cached_ss_trans cached_thread_trans cached_sys_thread_trans 

and eager_transitions_cont
    (fail_cont:  state_transitions_args ->
                  ConcModel.trans list ->
                  ConcModel.ss_trans list ->
                  (MachineDefTypes.thread_id, ConcModel.thread_subsystem_state MachineDefTypes.thread_trans list) Pmap.map ->
                  (MachineDefTypes.thread_id, ((ConcModel.thread_subsystem_state,ConcModel.storage_subsystem_state) MachineDefTypes.trans * bool) list) Pmap.map ->
                  unit)
    (args:       state_transitions_args)
    (cands:      ConcModel.trans list)
    (cached_ss_trans:     ConcModel.ss_trans list)
    (cached_thread_trans: (MachineDefTypes.thread_id, ConcModel.thread_subsystem_state MachineDefTypes.thread_trans list) Pmap.map)
    (cached_sys_thread_trans: (MachineDefTypes.thread_id, ((ConcModel.thread_subsystem_state,ConcModel.storage_subsystem_state) MachineDefTypes.trans * bool) list) Pmap.map)
    : unit
  =
  begin match
    List.find
      (MachineDefTransitionUtils.is_eager_transition args.state.model args.options.eager_mode)
      cands
  with
  | transition ->
      (* If any of the above where found, do it immediately and recurse *)
      snapshot_of_transition args.ppmode transition;
      begin match ConcModel.state_after_transition args.ppmode args.state transition with
      | TO_system_state (state', storage_changed, thread_changed, true, _, _)
        when args.options.prune_restarts ->
          restart_prune_count := !restart_prune_count + 1;
          args.on_done args.options args.ppmode args.on_undo

      | TO_system_state (state', storage_changed, thread_changed, _, true, _)
        when args.options.prune_discards ->
          discard_prune_count := !discard_prune_count + 1;
          args.on_done args.options args.ppmode args.on_undo

      | TO_system_state (state', storage_changed, thread_changed, _, _, true)
        when args.options.prune_late_writes ->
          late_write_prune_count := !late_write_prune_count + 1;
          args.on_done args.options args.ppmode args.on_undo

      | TO_system_state (state', storage_changed, thread_changed, _, _, _) ->
          do_transitions
            { args with
                state = state';
                cached_ss_trans = MachineDefSystem.clean_cached_ss_trans cached_ss_trans storage_changed;
                cached_thread_trans = MachineDefSystem.clean_cached_thread_trans cached_thread_trans thread_changed;
                cached_sys_thread_trans = MachineDefSystem.clean_cached_sys_thread_trans cached_sys_thread_trans thread_changed storage_changed;
            }

      (* if an eager transition will result in exception, don't do it eagerly *)
      | TO_unhandled_exception _ -> fail_cont args cands cached_ss_trans cached_thread_trans cached_sys_thread_trans
      end
  | exception Not_found -> fail_cont args cands cached_ss_trans cached_thread_trans cached_sys_thread_trans
  end

and hash_prune_cont
    (fail_cont:  state_transitions_args ->
                  ConcModel.trans list ->
                  ConcModel.ss_trans list ->
                  (MachineDefTypes.thread_id, ConcModel.thread_subsystem_state MachineDefTypes.thread_trans list) Pmap.map ->
                  (MachineDefTypes.thread_id, ((ConcModel.thread_subsystem_state,ConcModel.storage_subsystem_state) MachineDefTypes.trans * bool) list) Pmap.map ->
                  unit)
    (args:       state_transitions_args)
    (cands:      ConcModel.trans list)
    (cached_ss_trans:     ConcModel.ss_trans list)
    (cached_thread_trans: (MachineDefTypes.thread_id, ConcModel.thread_subsystem_state MachineDefTypes.thread_trans list) Pmap.map)
    (cached_sys_thread_trans: (MachineDefTypes.thread_id, ((ConcModel.thread_subsystem_state,ConcModel.storage_subsystem_state) MachineDefTypes.trans * bool) list) Pmap.map)
    : unit
  =
  (* we don't do hash-prune when in interactive mode as undo (or recalc)
  would result in pruning and we don't want that *)
  let hash_prune = args.options.hash_prune 
                   && not args.options.interactive 
                   && not (args.state.MachineDefTypes.s_model.ss_stopped_promising 
                           args.state.storage_subsystem)
  in

  if args.options.check_inf_loop || hash_prune then
    let hash_s =
      let (_, ui_state) = ConcModel.make_ui_system_state args.ppmode None args.state [] in
      Digest.string (Pp.pp_ui_system_state Globals.ppmode_for_hashing ui_state)
    in

    if args.options.check_inf_loop && StateHashSet.mem hash_s args.trace_hashes then begin
      Screen.show_warning args.ppmode "a trace with an infinite loop was detected via %s" (pp_choices args.info.choices_so_far);
      raise (Failure "detected infinite loop")
    end else if hash_prune && StateHashSet.mem hash_s !state_hashes then
      begin
          prune_count := 1 + !prune_count;
          args.on_done args.options args.ppmode args.on_undo
      end
    else
      begin
        state_hashes := StateHashSet.add hash_s !state_hashes;

        let args' =
          if args.options.check_inf_loop then
            {args with trace_hashes = StateHashSet.add hash_s args.trace_hashes}
          else args
        in
        fail_cont args' cands cached_ss_trans cached_thread_trans cached_sys_thread_trans
      end

  else
    fail_cont args cands cached_ss_trans cached_thread_trans cached_sys_thread_trans


(* if a loop-limit has been reach, take it before other transitions *)
and loop_limit_cont
    (next_cont:  state_transitions_args ->
                  ConcModel.trans list ->
                  ConcModel.ss_trans list ->
                  (MachineDefTypes.thread_id, ConcModel.thread_subsystem_state MachineDefTypes.thread_trans list) Pmap.map ->
                  (MachineDefTypes.thread_id, ((ConcModel.thread_subsystem_state,ConcModel.storage_subsystem_state) MachineDefTypes.trans * bool) list) Pmap.map ->
                  unit)
    (args:       state_transitions_args)
    (cands:      ConcModel.trans list)
    (cached_ss_trans:     ConcModel.ss_trans list)
    (cached_thread_trans: (MachineDefTypes.thread_id, ConcModel.thread_subsystem_state MachineDefTypes.thread_trans list) Pmap.map)
    (cached_sys_thread_trans: (MachineDefTypes.thread_id, ((ConcModel.thread_subsystem_state,ConcModel.storage_subsystem_state) MachineDefTypes.trans * bool) list) Pmap.map)
    : unit
  =
  if args.state.model.t.thread_loop_unroll_limit <> None
      && args.options.prune_discards
  then
    match List.filter MachineDefTransitionUtils.is_loop_limit_exception_transition cands with
    | []     -> next_cont args cands cached_ss_trans cached_thread_trans cached_sys_thread_trans
    | cands' ->
        loop_limit_count := !loop_limit_count + 1;
        next_cont args cands' cached_ss_trans cached_thread_trans cached_sys_thread_trans
  else next_cont args cands cached_ss_trans cached_thread_trans cached_sys_thread_trans


(* partial order reduction *)
and partial_order_cont
    (next_cont:  state_transitions_args ->
                  ConcModel.trans list ->
                  ConcModel.ss_trans list ->
                  (MachineDefTypes.thread_id, ConcModel.thread_subsystem_state MachineDefTypes.thread_trans list) Pmap.map ->
                  (MachineDefTypes.thread_id, ((ConcModel.thread_subsystem_state,ConcModel.storage_subsystem_state) MachineDefTypes.trans * bool) list) Pmap.map ->
                  unit)
    (args:       state_transitions_args)
    (cands:      ConcModel.trans list)
    (cached_ss_trans:     ConcModel.ss_trans list)
    (cached_thread_trans: (MachineDefTypes.thread_id, ConcModel.thread_subsystem_state MachineDefTypes.thread_trans list) Pmap.map)
    (cached_sys_thread_trans: (MachineDefTypes.thread_id, ((ConcModel.thread_subsystem_state,ConcModel.storage_subsystem_state) MachineDefTypes.trans * bool) list) Pmap.map)
    : unit
  =
  (* decommissioned:
  let global_por_filter prev_trans next_trans =
    begin match (prev_trans, next_trans) with
    (* always allow fetching the initial instruction *)
    | ( _, TSS_fetch (next_tid, _, _, next_address, _, _))
        (* TODO: compare with thread.initial_fetch_address *)
        when (Sail_impl_base.integer_of_address next_address) = (aval_of_inst_index next_tid 0)
        -> true

    (* thread-only transitions are allowed only if the previous
    transition has changed the thread *)
    | (SS_only_trans _, T_only_trans _)
    | (SS_only_trans _, TSS_fetch _)
    (*| (SS_lazy_trans _, T_only_trans _)*)
    (*| (SS_lazy_trans _, TSS_fetch _)*)
        -> false
    | (SST_POP_read_response ({r_thread = prev_tid}, _, _, _),          T_only_trans (next_tid, _, _))
    | (SST_POP_read_response ({r_thread = prev_tid}, _, _, _),          TSS_fetch (next_tid, _, _, _, _, _))
    | (SST_Flowing_read_response ({r_thread = prev_tid}, _, _, _),      T_only_trans (next_tid, _, _))
    | (SST_Flowing_read_response ({r_thread = prev_tid}, _, _, _),      TSS_fetch (next_tid, _, _, _, _, _))
    | (TSS_PLDI11_mem_satisfy_read (_, _, {r_thread = prev_tid}, _, _, _), T_only_trans (next_tid, _, _))
    | (TSS_PLDI11_mem_satisfy_read (_, _, {r_thread = prev_tid}, _, _, _), TSS_fetch (next_tid, _, _, _, _, _))
    | (T_lazy_trans (prev_tid, _, _),       T_only_trans (next_tid, _, _))
    | (T_lazy_trans (prev_tid, _, _),       TSS_fetch (next_tid, _, _, _, _, _))
    | (T_only_trans (prev_tid, _, _),       T_only_trans (next_tid, _, _))
    | (T_only_trans (prev_tid, _, _),       TSS_fetch (next_tid, _, _, _, _, _))
    | (TSS_fetch (prev_tid, _, _, _, _, _), T_only_trans (next_tid, _, _))
    | (TSS_fetch (prev_tid, _, _, _, _, _), TSS_fetch (next_tid, _, _, _, _, _))
    | (TSS_Flowing_POP_commit_mem_write_exclusive_successful ({w_thread = prev_tid}, _, _), T_only_trans (next_tid, _, _))
    | (TSS_Flowing_POP_commit_mem_write_exclusive_successful ({w_thread = prev_tid}, _, _), TSS_fetch (next_tid, _, _, _, _, _))
        when prev_tid <> next_tid -> false

    (* storage-only transitions are allowed only if the previous
    transition has changed the storage *)
    | (T_only_trans _, SS_only_trans _)
    | (TSS_fetch _, SS_only_trans _)
        -> false

    (* thread transitions are not allowed to follow other thread
    transition with higher tid *)
    | (T_only_trans (prev_tid, _, _),       T_lazy_trans (next_tid, _, _))
    | (T_lazy_trans (prev_tid, _, _),       T_lazy_trans (next_tid, _, _))
    | (TSS_fetch (prev_tid, _, _, _, _, _), T_lazy_trans (next_tid, _, _))
    (*| (T_only_trans (prev_tid, _, _),       T_only_trans (next_tid, _, _))*)
    (*| (T_lazy_trans (prev_tid, _, _),       T_only_trans (next_tid, _, _))*)
    (*| (TSS_fetch (prev_tid, _, _, _, _, _), T_only_trans (next_tid, _, _))*)
    (*| (T_only_trans (prev_tid, _, _),       TSS_fetch (next_tid, _, _, _, _, _))*)
    (*| (T_lazy_trans (prev_tid, _, _),       TSS_fetch (next_tid, _, _, _, _, _))*)
    (*| (TSS_fetch (prev_tid, _, _, _, _, _), TSS_fetch (next_tid, _, _, _, _, _))*)
        when prev_tid > next_tid -> false

    (* in flowing and pop the storage does not need to make any
    transition until it is ready to satisfy a read, at which point
    all the internal transitions can be made to satisfy that goal
    FIXME: (SF) I'm not sure this is a safe assumption in PLDI11 (consider
    reaching co-point).
    In the SC mode, when an SS transition is enabled we disabled all other
    transitions (except for the first SS transition) hence we can't use
    this restriction. *)
    (*| (SS_only_trans _, T_only_trans _)*)
    | (SS_only_trans _, T_lazy_trans _)
    (*| (SS_only_trans _, TSS_fetch _)*)
        when not args.state.model.ss.ss_sc -> false

    | _ -> true
    end
  in

  let local_por_filter prev_storage_trans prev_thread_trans next_trans =
    begin match (prev_storage_trans,next_trans) with
    (*| (SS_only_trans (SS_POP_propagate_event_to_thread _), T_lazy_trans (_, _, T_mem_read_request _))
    | (SS_only_trans (SS_POP_propagate_event_to_thread _), T_lazy_trans (_, _, T_commit_mem_write _))
    | (SS_only_trans (SS_POP_propagate_event_to_thread _), T_lazy_trans (_, _, T_commit_barrier _))
      -> false*)
    | _ -> true
    end
  in

  if args.options.partial_order_reduction then
    begin match args.info.Interact.trans_so_far with
    | trans :: _ ->
        let filtered_cands = List.filter (global_por_filter trans) cands in
        let filtered_cands =
          begin match args.prev_storage_trans with
          | Some prev_storage_trans ->
              List.filter (local_por_filter prev_storage_trans args.prev_thread_trans) filtered_cands
          | _ -> filtered_cands
          end
        in
        por_count := !por_count + (List.length cands) - (List.length filtered_cands);
        begin match (cands, filtered_cands) with
        | (_ :: _, []) -> args.on_done args.options args.ppmode args.on_undo
        | _            -> next_cont args filtered_cands cached_ss_trans cached_thread_trans
        end
    | _ -> next_cont args cands cached_ss_trans cached_thread_trans
    end
  else next_cont args cands cached_ss_trans cached_thread_trans
  *)
  if args.options.partial_order_reduction then
    failwith "partial order reduction was decommissioned do to lack of use (the code is commented out and can be resurrected)"
  else next_cont args cands cached_ss_trans cached_thread_trans cached_sys_thread_trans

(* priority reduction
NOTE: the exclusive success/fail part of the reduction is safe only when it
is performed before any of the load-exclusives are satisfied. If a load
exclusive is already satisfied the order in which we guarantee success
is observable *)
and priority_cont
    (next_cont:  state_transitions_args ->
                  ConcModel.trans list ->
                  ConcModel.ss_trans list ->
                  (MachineDefTypes.thread_id, ConcModel.thread_subsystem_state MachineDefTypes.thread_trans list) Pmap.map ->
                  (MachineDefTypes.thread_id, ((ConcModel.thread_subsystem_state,ConcModel.storage_subsystem_state) MachineDefTypes.trans * bool) list) Pmap.map ->
                  unit)
    (args:       state_transitions_args)
    (cands:      ConcModel.trans list)
    (cached_ss_trans:     ConcModel.ss_trans list)
    (cached_thread_trans: (MachineDefTypes.thread_id, ConcModel.thread_subsystem_state MachineDefTypes.thread_trans list) Pmap.map)
    (cached_sys_thread_trans: (MachineDefTypes.thread_id, ((ConcModel.thread_subsystem_state,ConcModel.storage_subsystem_state) MachineDefTypes.trans * bool) list) Pmap.map)
    : unit
  =
  let rec reduce = function
    | [] -> cands
    | (f,per_ioid) :: fs ->
        begin match List.filter f cands with
        | [] -> reduce fs
        | cands ->
            pr_count := !pr_count + 1;
            if per_ioid then 
              (* filter only the transitions from a single thread and
              single instruction (smallest ioid, i.e., po-oldest) *)
              let cmp_ioid t1 t2 =
                match (t1, t2) with
                | (T_trans t1, T_trans t2) ->
                    compare
                      (MachineDefTypes.ioid_of_thread_trans t1)
                      (MachineDefTypes.ioid_of_thread_trans t2)
                | _ -> assert false
              in
              begin match List.stable_sort cmp_ioid cands with
              | t :: ts -> t :: List.filter (fun t' -> cmp_ioid t t' = 0) ts
              | _       -> assert false
              end
            else 
              cands
        end
  in
  let priority_transitions =
    MachineDefTransitionUtils.priority_transitions 
      args.state.model (ConcModel.final_ss_state args.state)
      args.options.eager_mode
  in
  if args.options.priority_reduction then
    next_cont args (reduce priority_transitions) cached_ss_trans cached_thread_trans cached_sys_thread_trans
  else next_cont args cands cached_ss_trans cached_thread_trans cached_sys_thread_trans

and sequential_cont
    (next_cont:  state_transitions_args ->
                  ConcModel.trans list ->
                  ConcModel.ss_trans list ->
                  (MachineDefTypes.thread_id, ConcModel.thread_subsystem_state MachineDefTypes.thread_trans list) Pmap.map ->
                  (MachineDefTypes.thread_id, ((ConcModel.thread_subsystem_state,ConcModel.storage_subsystem_state) MachineDefTypes.trans * bool) list) Pmap.map ->
                  unit)
    (args:       state_transitions_args)
    (cands:      ConcModel.trans list)
    (cached_ss_trans:     ConcModel.ss_trans list)
    (cached_thread_trans: (MachineDefTypes.thread_id, ConcModel.thread_subsystem_state MachineDefTypes.thread_trans list) Pmap.map)
    (cached_sys_thread_trans: (MachineDefTypes.thread_id, ((ConcModel.thread_subsystem_state, ConcModel.storage_subsystem_state) MachineDefTypes.trans * bool) list) Pmap.map)
    : unit
  =
  (* remove all the transitions except for one (priority to storage). *)
  if args.options.sequential then
    let (storage_cands, non_storage_cands) = List.partition MachineDefTypes.is_storage_transition cands in
    begin match storage_cands @ non_storage_cands with
    | cand :: _ -> next_cont args [cand] cached_ss_trans cached_thread_trans cached_sys_thread_trans
    | []        -> next_cont args []     cached_ss_trans cached_thread_trans cached_sys_thread_trans
    end
  else next_cont args cands cached_ss_trans cached_thread_trans cached_sys_thread_trans

and regular_transition_cont
    (args:       state_transitions_args)
    (cands:      ConcModel.trans list)
    (cached_ss_trans:     ConcModel.ss_trans list)
    (cached_thread_trans: (MachineDefTypes.thread_id, ConcModel.thread_subsystem_state MachineDefTypes.thread_trans list) Pmap.map)
    (cached_sys_thread_trans: (MachineDefTypes.thread_id, ((ConcModel.thread_subsystem_state,ConcModel.storage_subsystem_state) MachineDefTypes.trans * bool) list) Pmap.map)
    : unit
  =
  (* when doing eager, the first time we get here
  most of the memory access events should be visible in args.state.
  we call pp_ui_gen_eiid_table to assign a nice ascii ID to these
  events for a nicer pp. *)
  let ppmode' =
    begin match args.info.Interact.last_system_state with
    | None -> Pp.pp_ui_gen_eiid_table args.ppmode args.state (* HACK: should not use Pp directly *)
    | Some _ -> args.ppmode
    end
  in

  (* add numbers to the transitions (for the UI), for transitions that
  were present in the previous state try to use the same number. *)
  let (numbered_cands : (int * ConcModel.trans) list) =
    (* "find_old_number [] t nts" return "Some (n, nts')" if the
    transition t is in nts (actually a transition very similar to t).
    n will be the number that was associated with t in nts and nts' is
    nts without t. Otherwise return None.
    We have to remove the match to make sure it doesn't match with
    another transition as fuzzy_compare_transitions is not precise. *)
    let rec find_old_number accum trans = function
      | [] -> None
      | (n, cand) :: old_cands ->
          if Model_aux.fuzzy_compare_transitions cand trans = 0 then
            Some (n, (List.rev accum) @ old_cands)
          else find_old_number ((n, cand) :: accum) trans old_cands
    in

    (* assign numbers to transitions that were present in the previous state *)
    let (_, numbered, unnumbered) =
      (* we fold right to preserve the order of unnumbered in the result *)
      List.fold_right
        (fun trans (old_cands, numbered, unnumbered) ->
          match find_old_number [] trans old_cands with
          | Some (n, old_cands) -> (old_cands, (n, trans) :: numbered, unnumbered)
          | None                -> (old_cands, numbered, trans :: unnumbered)
        )
        cands
        (args.info.last_numbered_cands, [], [])
    in

    (* "assign_numbers n ns ts []" assign numbers to the transitions ts
    starting from n and only numbers that are not in ns *)
    let rec assign_numbers current_n used_ns transitions accum =
      begin match transitions with
      | [] -> List.rev accum (* rev makes it easier on the sort below *)
      | t :: transitions' ->
          if List.mem current_n used_ns then
            assign_numbers (current_n + 1) used_ns transitions accum
          else
            assign_numbers (current_n + 1) used_ns transitions' ((current_n, t) :: accum)
      end
    in

    let (used_ns, _) = List.split numbered in
    let new_numbered = assign_numbers 0 used_ns unnumbered [] in

    List.sort
      (fun (n1, _) (n2, _) -> compare n1 n2)
      (numbered @ new_numbered)
  in

  (* the imperative state_hashes is used for a poor-man's
  memoised search - it's not used to calculate the final
  results, so it's ok to remove this state from it on an
  undo even if we've seen the state many times before *)
  let remove_current_and_undo = fun (options: RunOptions.t) ->
    begin
(*SS: turn off memoizing
      state_hashes := System_State_Set.remove s (!state_hashes);*)
      args.on_undo options
    end
  in

  (* interact.ml hands this do_transitions_continuation either 0
  transitions, or one user-chosen one, or (in auto mode) all of them.
  Always a subsequence of the numbered_cands we've got in scope *)
  let do_transitions_continuation = fun (interaction_state: ('ts,'ss) interaction_state) ->
    let new_undo_fn = fun (options: RunOptions.t) ->
      (* start again from here (cancelling any follow list) *)
      begin
        (*SS: turn off memoizing
        state_hashes := System_State_Set.remove s (!state_hashes); *)

        (* FIXME: (SF) these printfs can't be seen without -debug.
        If this is on purpose the code should be properly wrapped
        in a 'if debug then ...'. *)
        Screen.show_message ppmode' "----------------------------------------------------------------------------";
        Screen.show_message ppmode' "Undo to:";

        (* cancel follow list after undoing *)
        let info' = {args.info with Interact.trans_todo = []} in
        do_transitions {args with options = options; info = info'}
      end
    in

    (* calculate an info with last_system_state updated
    (a) with any eager transitions, and (b) with new id
    state following find_candidates *)
    let info_s = {interaction_state.info with Interact.last_system_state = Some args.state} in

    begin match interaction_state.numbered_cands with
    | [] ->
        (* Execution done *)
        if args.options.sequential then
          Debug.timer_stop "step_total";

        args.on_trace_end
          args.options
          ppmode'
          { kind = No_transitions;
            transitions = args.info.Interact.trans_so_far;
            choices = args.info.Interact.choices_so_far;
            state = args.state;
          };
        args.on_done args.options ppmode' remove_current_and_undo
    | _ ->
        (* Execution not done - fold over the transitions in interaction_state.numbered_cands *)
        let continuations_chain =
          List.fold_left
            (fun on_done (n, transition) ->
              snapshot_of_transition args.ppmode transition;

              begin match ConcModel.state_after_transition ppmode' args.state transition with
              | TO_unhandled_exception (tid, ioid, e) ->
                  fun options ppmode _ ->
                    args.on_trace_end
                      args.options
                      ppmode'
                      { kind = Raised_exception (tid, ioid, e);
                        transitions = transition :: args.info.Interact.trans_so_far;
                        choices = n :: args.info.Interact.choices_so_far;
                        state = args.state;
                      };
                    on_done options ppmode new_undo_fn

              | TO_system_state (state', storage_changed, thread_changed, true, _, _)
                when args.options.prune_restarts ->
                  fun options ppmode _ ->
                    restart_prune_count := !restart_prune_count + 1;
                    on_done options ppmode new_undo_fn

              | TO_system_state (state', storage_changed, thread_changed, _, true, _)
                when args.options.prune_discards ->
                  fun options ppmode _ ->
                    discard_prune_count := !discard_prune_count + 1;
                    on_done options ppmode new_undo_fn

              | TO_system_state (state', storage_changed, thread_changed, _, _, true)
                when args.options.prune_late_writes ->
                  fun options ppmode _ ->
                    late_write_prune_count := !late_write_prune_count + 1;
                    on_done options ppmode new_undo_fn

              | TO_system_state (state', storage_changed, thread_changed, _, _, _) ->
                  let info' = { info_s with
                                Interact.trans_so_far = transition :: info_s.Interact.trans_so_far;
                                Interact.choices_so_far = n :: info_s.Interact.choices_so_far;
                                Interact.last_numbered_cands = numbered_cands;
                              }
                  in

                  if args.options.sequential then
                  begin
                    let step = List.length args.info.choices_so_far in
                    if not (MachineDefTransitionUtils.is_internal transition &&
                            args.options.suppress_internal)
                    then Screen.show_message ppmode' "Step %d  %s" step (Pp.pp_cand ppmode' (n, transition));
                    if step mod 1 = 0 then Screen.flush_buffer ppmode';

                    Debug.timer_stop "step_total";
                    Debug.set_label (Pp.pp_trans_label_only ppmode' transition);
                    Debug.increment_step_counter ()
                  end;

                  fun options ppmode _ ->
                    do_transitions
                      {args with
                        options = options;
                        ppmode = ppmode;
                        info = info';
                        state = state';
                        on_done = on_done;
                        on_undo = new_undo_fn;
                        cached_ss_trans = MachineDefSystem.clean_cached_ss_trans cached_ss_trans storage_changed;
                        cached_thread_trans = MachineDefSystem.clean_cached_thread_trans cached_thread_trans thread_changed;
                        cached_sys_thread_trans = MachineDefSystem.clean_cached_sys_thread_trans cached_sys_thread_trans thread_changed storage_changed;
                        prev_storage_trans = if storage_changed then Some transition
                                              else args.prev_storage_trans;
                        prev_thread_trans = List.fold_left
                                              (fun m tid -> TidMap.add tid transition m)
                                              args.prev_thread_trans
                                              thread_changed}
              end
            )
            args.on_done (* initial value of accumulator *)
            interaction_state.numbered_cands
        in
        continuations_chain args.options ppmode' new_undo_fn
    end
  in

  let random_cand =
    if args.options.pseudorandom && args.info.trans_todo = [] && numbered_cands <> [] then
      let i = Random.int (List.length numbered_cands) in
      Some (List.nth numbered_cands i)
    else
      None
  in

  let interaction_state =
    { Interact.info =
        begin match random_cand with
        | Some (n, _) -> {args.info with trans_todo = [n]}
        | None        -> args.info
        end;

      Interact.numbered_cands = numbered_cands;

      Interact.make_graph = fun () ->
        let module G = (val get_graph_backend ()) in
        G.make_graph ppmode'
                      args.test_info
                      args.state
                      (ConcModel.make_cex_candidate args.state)
                      numbered_cands;
    }
  in

  (* LM: It is important to be tail recursive here... *)
  if args.options.interactive then
    let (ppmode', ui_state) = ConcModel.make_ui_system_state ppmode' args.info.last_system_state args.state numbered_cands in
    Interact.interact_with_user
      args.options
      ppmode'
      args.state
      ui_state
      interaction_state
      do_transitions_continuation
      remove_current_and_undo
      (fun options ppmode -> do_transitions {args with options = options; ppmode = ppmode})
  else
    let interaction_state =
      begin match interaction_state.info.trans_todo with
      | n :: ns ->
          let cand =
            try List.assoc n interaction_state.numbered_cands with
            | Not_found -> raise (Failure "can't find transition from the follow list")
          in
          { Interact.info           = { interaction_state.info with trans_todo = ns };
            Interact.numbered_cands = [(n, cand)];
            Interact.make_graph = fun () -> ();
          }
      | [] -> interaction_state
      end
    in
    do_transitions_continuation interaction_state
(* end of do_regular_transition *)


(* trace_end is what to do with a state that has no transitions *)
let trace_end
    (test_info: Test.info)
    (options:   RunOptions.t)
    (ppmode:    Globals.ppmode)
    (result:    result_type)
    : unit
  =
  trace_count := !trace_count + 1;

  next_branch_targets := begin
    let (union, diff) =
      MachineDefSystem.union_and_diff_branch_targets
        (ConcModel.branch_targets_of_state result.state)
        !next_branch_targets
    in
    if diff <> [] && Globals.is_verbosity_at_least Globals.ThrottledInformation then
      Screen.show_message ppmode "%s found new branch-register target(s): %s"
        (sprint_time ())
        (sprint_branch_targets ppmode diff);
    union
  end;

  if options.eager_mode.eager_local_mem then
    next_shared_memory := begin
      let (union, diff) =
        MachineDefSystem.union_and_diff_shared_memory
          (ConcModel.shared_memory_of_state result.state)
          !next_shared_memory
      in
      if not (Pset.is_empty diff) && Globals.is_verbosity_at_least Globals.ThrottledInformation then
        Screen.show_message ppmode "%s found new shared memory footprint(s): %s"
          (sprint_time ())
          (sprint_shared_memory ppmode diff);
      union
    end;

  begin match result.transitions with
  | last :: _ -> debug_print_state Globals.Debug ppmode result.choices (Some last) "final state" result.state
  | [] ->        debug_print_state Globals.Debug ppmode result.choices None        "final state" result.state
  end;

  begin match result.kind with
  | Raised_exception (tid, ioid, e) ->
      (* Print nice dot pictures *)
      begin match !Globals.run_dot with
      | Some RD_final
      | Some RD_final_ok
      | Some RD_final_not_ok
        ->
        (* because this is not a valid final state we do not check test_info.constr *)
        (* HACK: *)
        if !Globals.graph_backend = Globals.Tikz then begin
          Tikz.make_final_state test_info (Pp.pp_exception ppmode ioid e)
        end;

        Screen.show_message ppmode "Unhandled exception: %s" (Pp.pp_exception ppmode ioid e);
        let module G = (val get_graph_backend ()) in
        G.make_graph ppmode test_info result.state (ConcModel.make_cex_candidate result.state) []
      | Some RD_step
      | None
        -> ()
      end;

      let (choices, count) =
        try ExceptionMap.find (tid, ioid, e) !observed_exceptions with
        | Not_found -> (result.choices, 0)
      in
      observed_exceptions := ExceptionMap.add (tid, ioid, e) (choices, count + 1) !observed_exceptions;

      if options.interactive then
      begin
        let (ppmode', ui_state) = ConcModel.make_ui_system_state ppmode None result.state [] in
        Screen.draw_system_state ppmode' result.choices [] result.state ui_state None [] [];
        Screen.show_warning ppmode "Unhandled exception: %s" (Pp.pp_exception ppmode ioid e)
      end
      else ()

  | No_transitions ->

      let mk_final_state kregs kmem =
        let reg_state =
          let reg_values_of_thread t_model state =
            let reg_values = t_model.ts_final_reg_state state in
            Lem_list.mapMaybe
              (fun (name, value) ->
                match value with
                | Some v ->
                    begin match Sail_impl_base.integer_of_register_value v with
                    | Some i -> Some (name, big_num_to_int64 i)
                    | None -> None
                    end
                | None -> None)
              reg_values
          in

          List.map
            (fun (tid, ts) ->
              let regs =
                List.filter
                  (fun (r, _) -> List.mem (tid, r) kregs)
                  (reg_values_of_thread result.state.t_model ts)
              in
              (tid, regs))
            (Pmap.bindings_list result.state.thread_states)
        in

        let mem_state =
          List.map
            (fun ((addr, size), value) ->
                let int64_addr = Nat_big_num.to_int64 (Sail_impl_base.integer_of_address addr) in
                let big_value =
                  match Sail_impl_base.integer_of_memory_value (Globals.get_endianness ()) value with
                  | Some bi -> bi
                  | None -> failwith "bad memory value"
                in
                ((int64_addr, size), big_value))
            (ConcModel.memory_value_of_footprints result.state kmem)
        in
        (reg_state, mem_state)
      in

      let final_state =
        mk_final_state test_info.Test.show_regs test_info.Test.show_mem in

      if options.interactive then
      begin
        let symtab =
          List.map
            (fun ((a,sz),s) ->
              (Nat_big_num.to_int64 (Sail_impl_base.integer_of_address a), s))
            ppmode.pp_symbol_table
        in
        let pp_state =
          if ConcModel.is_final_state result.state then
            otLine @@ otCenter @@ otEmph @@ otStrColor Screen.final_state_color "%s" (Test.C.pp_state symtab final_state)
          else
            otLine @@ otCenter @@ otEmph @@ otStrColor "red" "-DEADLOCK- %s" (Test.C.pp_state symtab final_state)
        in

        let pp_state =
          if ppmode.Globals.pp_kind = Latex then
            Screen_base.latex_of_output_tree pp_state
          else Screen.of_output_tree pp_state
        in

        let (ppmode', ui_state) = ConcModel.make_ui_system_state ppmode None result.state [] in
        Screen.draw_system_state ppmode' result.choices [] result.state ui_state None [] [];

        Screen.final_message ppmode true pp_state
      end;

      let make_graph () = begin
        (* HACK: *)
        if !Globals.graph_backend = Globals.Tikz then begin
          let symtab =
            List.map
              (fun ((a,sz),s) ->
                (Nat_big_num.to_int64 (Sail_impl_base.integer_of_address a), s))
              ppmode.pp_symbol_table
          in
          Tikz.make_final_state test_info (Test.C.pp_state symtab final_state)
        end;

        let module G = (val get_graph_backend ()) in
        G.make_graph ppmode test_info result.state (ConcModel.make_cex_candidate result.state) []
      end in

      (* Print nice dot pictures *)
      begin match !Globals.run_dot with
      | Some Globals.RD_final -> make_graph ()
      | Some Globals.RD_final_ok
        when Test.C.check_constr test_info.Test.constr [final_state]
        -> make_graph ()
      | Some Globals.RD_final_not_ok
        when not (Test.C.check_constr test_info.Test.constr [final_state])
        -> make_graph ()
      | _ -> ()
      end;

      if ConcModel.is_final_state result.state then begin
        let filterred =
          match test_info.Test.filter with
          | None -> true
          | Some filter ->
              mk_final_state test_info.Test.filter_regs test_info.Test.filter_mem
              |> Test.C.check_filter filter
        in

        (* final state is ignored when filter is invalid *)
        if filterred then
          let (choices, count) =
            try StateMap.find final_state !observed_finals with
            | Not_found -> (result.choices, 0)
          in
          observed_finals := StateMap.add final_state (choices, count + 1) !observed_finals;

      end else
        let (choices, count) =
          try StateMap.find final_state !observed_deadlocks with
          | Not_found -> (result.choices, 0)
        in
        observed_deadlocks := StateMap.add final_state (choices, count + 1) !observed_deadlocks

      (* FIXME: axiomatic_failure := ... *)
  end

(* ******************************************************************** *)
(* output of exhaustive results                                         *)
(* ******************************************************************** *)

let print_observations
    (test_info: Test.info)
    (model:     MachineDefTypes.model_params)
    (ppmode:    Globals.ppmode)
  =
  let symtab =
    List.map
      (fun ((addr, _), s) ->
        let addr = big_num_to_int64 (Sail_impl_base.integer_of_address addr) in
        (addr, s))
      ppmode.pp_symbol_table
  in

  let branch_targets_output =
    otIfTrue (not (Pmap.is_empty model.t.branch_targets)) @@
      otVerbose Globals.Normal @@
        otStrLine "Branch-targets=%s"
          (MachineDefSystem.branch_targets_to_list model.t.branch_targets |> sprint_branch_targets ppmode)
  in

  let shared_memory_output =
    otIfTrue (not (Pset.is_empty model.shared_memory)) @@
      otVerbose Globals.Normal @@
        otStrLine "Shared-memory=%s" (sprint_shared_memory ppmode model.shared_memory)
  in

  let states_output =
    let check_prop =
      match test_info.Test.constr with
      | ForallStates p
      | ExistsState p
      | NotExistsState p ->
          Test.C.check_prop p
    in

    OTConcat [
      otStrLine "States %i" (StateMap.cardinal !observed_finals);
      OTConcat
        (List.map
          (fun (state, (choices, count)) ->
            otLine @@ OTConcat [otStrVerbose Globals.Normal "%-6d%s>" count (if check_prop state then "*" else ":");
                                otString "%s" (Test.C.pp_state symtab state);
                                otStrVerbose Globals.Normal "via %s" (pp_choices choices)])
          (StateMap.bindings !observed_finals));
    ]
  in

  let deadlock_states_output =
    otIfTrue (StateMap.cardinal !observed_deadlocks > 0) @@
      otVerbose Globals.Normal @@ OTConcat [
        otStrLine "Deadlock states %i" (StateMap.cardinal !observed_deadlocks);
        OTConcat
          (List.map
            (fun (state, (choices, count)) ->
              otLine @@ OTConcat [otString "%-6d:>" count;
                                  otString "%s" (Test.C.pp_state symtab state);
                                  otString "via %s" (pp_choices choices)])
            (StateMap.bindings !observed_deadlocks));
      ]
  in

  let exceptions_output =
    otIfTrue (ExceptionMap.cardinal !observed_exceptions > 0) @@
      otVerbose Globals.Normal @@ OTConcat [
        otStrLine "Unhandled exceptions %i" (ExceptionMap.cardinal !observed_exceptions);
        OTConcat
          (List.map
            (fun ((tid, ioid, exception_type), (choices, count)) ->
              otLine @@ OTConcat [otString "%-6d:>" count;
                                  otString "thread %d instruction %s: %s" tid (Pp.pp_pretty_ioid ioid) (Pp.pp_exception ppmode ioid exception_type);
                                  otString " via %s" (pp_choices choices)])
            (ExceptionMap.bindings !observed_exceptions));
      ]
  in

  OTConcat [
    branch_targets_output;
    shared_memory_output;
    states_output;
    deadlock_states_output;
    exceptions_output;
  ]


let print_results
    (test_info: Test.info)
    (model:     MachineDefTypes.model_params)
    (options:   RunOptions.t)
    (ppmode:    Globals.ppmode)
  =
  let symtab =
    List.map
      (fun ((addr, _), s) ->
        let addr = big_num_to_int64 (Sail_impl_base.integer_of_address addr) in
        (addr, s))
      ppmode.pp_symbol_table
  in

  if Globals.is_verbosity_at_least Globals.ThrottledInformation then print_counters ppmode true;

  let test_name_output =
    let name' =
      "Test " ^ test_info.Test.name ^ " " ^ (ConstrGen.pp_kind (ConstrGen.kind_of test_info.Test.constr))
    in
    otLine @@ otStrEmph "%s" name'
  in

  let (constraint_holds, constraint_output) =
    let holds =  StateMap.bindings !observed_finals |> List.split |> fst |> Test.C.check_constr test_info.Test.constr in
    if holds then (true, otStrLine "Ok")
    else if is_existential test_info.Test.constr then
      (false, OTConcat [otVerbose Globals.ThrottledInformation @@ otWarning @@ otString "%s: Existential constraint not satisfied!" test_info.Test.name;
                otStrLine "No (allowed not found)"])
    else (* universal failed *)
      (false, OTConcat [otVerbose Globals.ThrottledInformation @@ otWarning @@ otString "%s: Universal constraint invalidated!" test_info.Test.name;
                otStrLine "No (forbidden found)"])
  in

  let condition_output =
    otStrLine "Condition %s" (ConstrGen.constraints_to_string (Test.C.pp_atom symtab) test_info.Test.constr)
  in

  let test_info_output =
    if !Globals.dont_tool then
    begin
      OTConcat
        (List.map
          (fun (k, v) ->
            if Misc.string_eq k "Relax" then
              otStrLine "Relax %s %s %s" test_info.Test.name (if constraint_holds then "Ok" else "No") v
            else
              otStrLine "%s=%s" k v)
          test_info.Test.info)
    end
    else
    begin
      OTConcat
        (Lem_list.mapMaybe
          (fun (k, v) ->
            if Misc.string_eq k "Hash" then
              Some (otStrLine "%s=%s" k v)
            else None)
          test_info.Test.info)
    end
  in

  let observation_output =
    let (matches, non_matches) =
      let prop =
        match test_info.Test.constr with
        | ForallStates p
        | ExistsState p
        | NotExistsState p -> p
      in
      StateMap.bindings !observed_finals |> List.split |> fst |> List.partition (Test.C.check_prop prop)
    in

    otStrLine "Observation %s %s %d %d%s%s"
      test_info.Test.name
      (if List.length matches = 0 then "Never"
      else if List.length non_matches = 0 then "Always"
      else "Sometimes")
      (List.length matches)
      (List.length non_matches)
      (if not (StateMap.is_empty !observed_deadlocks) then " with deadlocks" else "")
      (if not (ExceptionMap.is_empty !observed_exceptions) then " with unhandled exceptions" else "")
  in

  (* OLD CODE:
  let axiomatic_output =
    if !Globals.optoax then
      otStrLine (sprintf "Axiomatic model passes all complete traces found: %b" (not !axiomatic_failure))
    else
      OTEmpty
  in
  *)

  let runtime_output =
    if !Globals.deterministic_output then
      otStrLine "Runtime: 0.0 sec (suppressed for deterministic comparison)"
    else
      otStrLine "Runtime: %f sec" (Sys.time ())
  in

  let output_tree =
    OTConcat
      [ test_name_output;
        print_observations test_info model ppmode;
        constraint_output;
        condition_output;
        test_info_output;
        observation_output;
        (*axiomatic_output;*)
        runtime_output;
      ]
  in

  Screen.final_message ppmode false (Screen.of_output_tree output_tree)



(* ******************************************************************** *)
(* output of sequential results                                         *)
(* ******************************************************************** *)

let print_results_sequential
    (name:      string)
    (options:   RunOptions.t)
    (ppmode:    Globals.ppmode)
  =
  let symtab =
    List.map
      (fun ((addr, _), s) ->
        let addr = big_num_to_int64 (Sail_impl_base.integer_of_address addr) in
        (addr, s))
      ppmode.pp_symbol_table
  in

  if Globals.is_verbosity_at_least Globals.ThrottledInformation then print_counters ppmode true;

  let test_name_output = otLine @@ otStrEmph "%s" ("Test " ^ name) in

  let deadlock_states_count_output =
    if StateMap.is_empty !observed_deadlocks
    then otVerbose Globals.Normal @@ otStrLine "No deadlock."
    else otVerbose Globals.Normal @@ otStrLine "Ended in deadlock state:" in

  let deadlock_states_output =
    otVerbose Globals.Normal @@
    OTConcat
      (List.map
        (fun (state, (choices, count)) ->
          otLine @@ OTConcat [otString "%-6d:>" count;
                              otString "%s" (Test.C.pp_state symtab state);
                              otString "via %s" (pp_choices choices)])
        (StateMap.bindings !observed_deadlocks))
  in

  let exceptions_count_output =
    if ExceptionMap.is_empty !observed_exceptions
    then otVerbose Globals.Normal @@ otStrLine "Exited normally."
    else otVerbose Globals.Normal @@ otStrLine "Exited with exception:" in

  let exceptions_output =
    otVerbose Globals.Normal @@
    OTConcat
      (List.map
        (fun ((tid, ioid, exception_type), (choices, count)) ->
          otLine @@ OTConcat [otString "%-6d:>" count;
                              otString "thread %d instruction %s: %s" tid (Pp.pp_pretty_ioid ioid) (Pp.pp_exception ppmode ioid exception_type);
                              otString " via %s" (pp_choices choices)])
        (ExceptionMap.bindings !observed_exceptions))
  in

  let runtime_output =
    if !Globals.deterministic_output then
      otStrLine "Runtime: 0.0 sec (suppressed for deterministic comparison)"
    else
      otStrLine "Runtime: %f sec" (Sys.time ())
  in

  let output_tree =
    OTConcat [test_name_output;
              deadlock_states_count_output;
              deadlock_states_output;
              exceptions_count_output;
              exceptions_output;
              (*axiomatic_output;*)
              runtime_output]
  in

  Screen.final_message ppmode false (Screen.of_output_tree output_tree)




(* ******************************************************************** *)
(* the top-level of the main search                                     *)
(* ******************************************************************** *)

let calc_finals
    (options:   RunOptions.t)
    (ppmode:    Globals.ppmode)
    (test_info: Test.info)
    (initial_state_records:
                MachineDefSystem.initial_state_record list)
  =
  let print_results_fn is_partial model options ppmode : unit =
    if is_partial then
    begin
      Screen.show_message ppmode "";
      Screen.show_message ppmode "***********************";
      Screen.show_message ppmode "*** PARTIAL RESULTS ***";
      Screen.show_message ppmode "***********************";
      Screen.show_message ppmode "";
    end;
    print_results test_info model options ppmode
  in

  interrupt := false;

  if options.allow_partial then
  begin
    let interrupt_handler sig_name signum =
      Screen.show_message ppmode "received interrupt signal %s (%d)" sig_name signum;
      interrupt := true
    in

    Sys.set_signal Sys.sigint  (Sys.Signal_handle (interrupt_handler "INT"));
    Sys.set_signal Sys.sigabrt (Sys.Signal_handle (interrupt_handler "ABRT"));
    Sys.set_signal Sys.sigalrm (Sys.Signal_handle (interrupt_handler "ALRM"));
    Sys.set_signal Sys.sigquit (Sys.Signal_handle (interrupt_handler "QUIT"));
    Sys.set_signal Sys.sigterm (Sys.Signal_handle (interrupt_handler "TERM"));
  end;

  let calc_from_initial_record isr : unit =
    let info =
      { Interact.testname = Some test_info.Test.name;
        Interact.trans_todo = List.map
                                (function Interact_parser_base.Transitions [i] -> i
                                        | _ -> assert false)
                                !Globals.follow;
        Interact.choices_so_far = [];
        Interact.trans_so_far = [];
        Interact.last_system_state = None;
        Interact.last_numbered_cands = []}
    in

    let initial_state = ConcModel.initial_system_state test_info.Test.ism options isr in

    fixedpoint_iterations := 0;
    next_branch_targets   := initial_state.model.t.branch_targets;
    next_shared_memory    := initial_state.model.shared_memory;

    let rec start state (options: RunOptions.t) : unit =
      initial_args state options
      |> do_transitions
    and initial_undo state : RunOptions.t -> unit = fun options ->
      Screen.show_message ppmode "Cannot undo from initial state";
      start state options
    and initial_args state (options: RunOptions.t) : state_transitions_args =
      { test_info = test_info;
        options = options;
        ppmode = ppmode;
        info = info;
        state = state;
        on_trace_end = trace_end test_info;
        on_done = on_done state.model;
        on_undo = initial_undo state;
        print_partial = print_results_fn true state.model;
        cached_ss_trans = None;
        cached_thread_trans = (Pmap.empty compare);
        cached_sys_thread_trans = (Pmap.empty compare);
        prev_storage_trans = None;
        prev_thread_trans = TidMap.empty;
        trace_hashes = StateHashSet.empty;
      }
    and on_done model = fun options ppmode on_undo ->
      if options.interactive then
        (* if we do undo at the end of a trace we probably want pseudorandom
        and interactive_auto turned off *)
        let on_undo' = fun () ->
          Globals.interactive_auto := false;
          Globals.auto_follow := false;
          on_undo {options with pseudorandom = false}
        in
        Interact.ask_quit_or_undo on_undo'

      else if options.pseudorandom then begin
        let (bt_union, bt_diff) =
          MachineDefSystem.union_and_diff_branch_targets
            !next_branch_targets
            model.t.branch_targets
        in

        let (sm_union, sm_diff) =
          MachineDefSystem.union_and_diff_shared_memory
            !next_shared_memory
            model.shared_memory
        in

        if bt_diff <> []
            || (options.eager_mode.eager_local_mem && not (Pset.is_empty sm_diff))
            || options.pseudorandom_traces <= 1
        then begin
          OTConcat [
            otStrLine "---------------------------------------------";
            print_observations test_info model ppmode;
            otStrLine "---------------------------------------------";
          ]
          |> Screen.of_output_tree
          |> Screen.show_message ppmode "%s";

          reset_counters ();
          reset_observed_states ()
        end;

        if options.pseudorandom_traces > 1 then begin
          let model' =
            { model with
              t = {model.t with branch_targets = bt_union};
              shared_memory = sm_union;
            }
          in
          {options with pseudorandom_traces = options.pseudorandom_traces - 1}
          |> start {initial_state with model = model'}
        end;

      end else if options.sequential then
        begin
          print_results_sequential test_info.Test.name options ppmode;
          if not (ExceptionMap.is_empty !observed_exceptions) then failwith "There were exceptions.";
          if not (StateMap.is_empty !observed_deadlocks) then failwith "There were deadlocks.";
        end
      else
        begin
          let (bt_union, bt_diff) =
            MachineDefSystem.union_and_diff_branch_targets
              !next_branch_targets
              model.t.branch_targets
          in

          let (sm_union, sm_diff) =
            MachineDefSystem.union_and_diff_shared_memory
              !next_shared_memory
              model.shared_memory
          in

          if bt_diff = [] && (not options.eager_mode.eager_local_mem || Pset.is_empty sm_diff) then
            print_results_fn false model options ppmode
          else begin
            if Globals.is_verbosity_at_least Globals.ThrottledInformation then begin
              Screen.show_message ppmode "%s An exhaustive search was finished but a fixed point was not reached." (sprint_time ());

              if not (Pmap.is_empty bt_union) then begin
                Screen.show_message ppmode "Branch-register targets that were observed:\n%s"
                  (MachineDefSystem.branch_targets_to_list bt_union |> sprint_branch_targets ppmode);

                if bt_diff <> [] then
                  Screen.show_message ppmode "(from which the following are new: %s)"
                    (sprint_branch_targets ppmode bt_diff);
              end;

              if options.eager_mode.eager_local_mem then begin
                if Pset.is_empty sm_union then
                  Screen.show_message ppmode "No shared memory footprints were observed!"
                else begin
                  Screen.show_message ppmode "Shared memory footprints that were observed:\n%s"
                    (sprint_shared_memory ppmode sm_union);

                  if not (Pset.is_empty sm_diff) then
                    Screen.show_message ppmode "(from which the following are new: %s)"
                      (sprint_shared_memory ppmode sm_diff);
                end;
              end;

              if options.allow_partial then
                print_results_fn true model options ppmode;

              Screen.show_message ppmode "%s Starting another exhaustive search." (sprint_time ());
            end;

            fixedpoint_iterations := !fixedpoint_iterations + 1;

            let model' =
              { model with
                t = {model.t with branch_targets = bt_union};
                shared_memory = sm_union;
              }
            in

            reset_counters ();
            reset_observed_states ();
            start {initial_state with model = model'} options
          end
        end
    in
    reset_counters ();
    reset_observed_states ();
    start initial_state options;
  in

  List.iter calc_from_initial_record initial_state_records
end
