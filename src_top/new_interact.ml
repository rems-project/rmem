(*===============================================================================*)
(*                                                                               *)
(*                rmem executable model                                          *)
(*                =====================                                          *)
(*                                                                               *)
(*  This file is:                                                                *)
(*                                                                               *)
(*  Copyright Jon French, University of Cambridge        2016-2018               *)
(*  Copyright Shaked Flur, University of Cambridge       2016-2018               *)
(*  Copyright Christopher Pulte, University of Cambridge 2016-2018               *)
(*  Copyright Peter Sewell, University of Cambridge      2016-2017               *)
(*                                                                               *)
(*  All rights reserved.                                                         *)
(*                                                                               *)
(*  It is part of the rmem tool, distributed under the 2-clause BSD licence in   *)
(*  LICENCE.txt.                                                                 *)
(*                                                                               *)
(*===============================================================================*)

open Lexing
open MachineDefTypes
open RunOptions

open Screen_base

module Make (ConcModel: Concurrency_model.S) = struct
  module Runner = New_run.Make (ConcModel)

(** Parser **********************************************************)

type parser_outcome =
  | ParserASTs of Interact_parser_base.ast list
  | ParserError of string

let parse (str: string) : parser_outcome =
  let print_position lexbuf : string =
    let pos = lexbuf.lex_curr_p in
    Printf.sprintf "%s:%d:%d" pos.pos_fname
        pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)
  in

  let lexbuf = Lexing.from_string str in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = "$" };

  try ParserASTs (Interact_parser.commands Interact_lexer.read lexbuf) with
  | Interact_lexer.SyntaxError msg ->
      ParserError (Printf.sprintf "%s: %s\n" (print_position lexbuf) msg)
  | Parsing.Parse_error ->
      ParserError (Printf.sprintf "%s: syntax error\n" (print_position lexbuf))

(********************************************************************)

type interact_node_type =
  | InitialStateNode
  | TransitionNode
  | EagerTransitionsNode
  | EagerModeChangeNode
  | ModelChangeNode

type interact_node =
  {
    node_type:            interact_node_type;
    system_state:         ConcModel.system_state_and_transitions;
    open_transition:      int list; (* actual followed by eagers *)
    (* filtered_transitions: the option is None if the transition is filtered out,
    otherwise the int is a number the user can enter to take the transition.
    The order of the transitions in this list must match the order of the transitions
    in system_state.sst_system_transitions (so it is possible to translate a search
    trace to an interact trace). *)
    filtered_transitions: (int option * ConcModel.trans) list;
    eager_mode:           MachineDefTypes.eager_mode;
    follow_ast:           Interact_parser_base.ast option;
  }

type interact_state =
  { test_info:     Test.info;

    options:       RunOptions.t;
    ppmode:        Globals.ppmode; (* printing information *)

    (* head is the active node *)
    interact_nodes: interact_node list;
    cmd_history:    Interact_parser_base.ast list;

    follow_suffix: Interact_parser_base.ast list;
    default_cmd: Interact_parser_base.ast option;

    breakpoints: Runner.breakpoint list;

    (*** shared memory approximation ***)
    read_locations:      (int (* tid *) , (MachineDefTypes.footprint list)) Pmap.map;
    written_locations:   (int (* tid *) , (MachineDefTypes.footprint list)) Pmap.map;
    shared_locations:    MachineDefTypes.footprint list;
    shared_program_locations: Sail_impl_base.address Pset.set;
    (* shared_program_locations: Sail_impl_base.address list; *)
  }

let run_options_lens = { Lens.get = (fun s -> s.options); Lens.set = (fun o s -> { s with options = o }) }
let ppmode_lens = { Lens.get = (fun s -> s.ppmode); Lens.set = (fun m s -> { s with ppmode = m }) }

let sorted_filtered_transitions node =
  List.filter (function (Some _, _) -> true | _ -> false) node.filtered_transitions
  |> List.map (function (Some n, t) -> (n ,t) | _ -> assert false)
  |> List.sort (fun (l, _) (r, _) -> compare l r)

let filtered_out_transitions node =
  List.filter (function (None, _) -> true | _ -> false) node.filtered_transitions
  |> List.split
  |> snd

let is_eager_in_interact interact_state = fun transition ->
  match interact_state.interact_nodes with
  | [] -> assert false
  | { system_state = sst } :: _ ->
     Runner.is_eager
              interact_state.options
              interact_state.shared_locations
              interact_state.shared_program_locations
              sst
              transition

let get_graph_backend () =
  begin match !Globals.graph_backend with
  | Globals.Dot -> (module Graphviz.S : Pp.GraphBackend)
  | Globals.Tikz -> (module Tikz.S : Pp.GraphBackend)
  end

let append_to_history silenced ast interact_state =
  if !silenced then
    interact_state
  else
    {interact_state with cmd_history = ast :: interact_state.cmd_history}

let number_transitions
    (prev_ts: ((int * ConcModel.trans) list) option)
    (ts:      (bool * ConcModel.trans) list)
    : (int option * ConcModel.trans) list
  =
  begin match prev_ts with
  | None ->
      List.fold_left
        (fun (accum, n) (filter, trans) ->
          if filter then
            ((Some n, trans) :: accum, n+1)
          else
            ((None, trans) :: accum, n)
        )
        ([], 0)
        ts
      |> fst
      |> List.rev
  | Some prev_ts ->
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
      let (_, nts) =
        (* we fold right to preserve the order of unnumbered in the result *)
        List.fold_right
          (function
          | (true, trans) -> fun (old_cands, nts) ->
            begin match find_old_number [] trans old_cands with
            | Some (n, old_cands) -> (old_cands, (Some (Some n), trans) :: nts)
            | None                -> (old_cands, (Some None,     trans) :: nts)
            end
          | (false, trans) -> fun (old_cands, nts) ->
            (old_cands, (None, trans) :: nts)
          )
          ts
          (prev_ts, [])
      in

      (* "assign_numbers n ns nts []" assign numbers to the transitions nts
      starting from n and only numbers that are not in ns *)
      let rec assign_numbers current_n used_ns nts accum =
        begin match nts with
        | [] -> List.rev accum
        | (None, t) :: nts' -> assign_numbers current_n used_ns nts' ((None, t) :: accum)
        | (Some None, t) :: nts' ->
            if List.mem current_n used_ns then
              assign_numbers (current_n + 1) used_ns nts accum
            else
              assign_numbers (current_n + 1) used_ns nts' ((Some current_n, t) :: accum)
        | (Some (Some n), t) :: nts' ->
            assign_numbers current_n used_ns nts' ((Some n, t) :: accum)
        end
      in

      let used_ns =
        List.split nts
        |> fst
        |> List.filter (function Some (Some _) -> true | _ -> false)
        |> List.map (function Some (Some n) -> n | _ -> assert false)
      in
      assign_numbers 0 used_ns nts []
  end

let default_cmd interact_state : Interact_parser_base.ast option =
  begin match interact_state.follow_suffix with
  | ast :: _ -> Some ast
  | []     ->
      begin match List.hd interact_state.interact_nodes with
      | node ->
        let transitions = sorted_filtered_transitions node in
        let transitions =
          if interact_state.options.storage_first then
            match List.partition (fun (_, t) -> MachineDefTypes.is_storage_transition t) transitions with
            | ([], _)  -> transitions
            | (sts, _) -> sts
          else
            transitions
        in
        let transitions =
           if interact_state.options.pseudorandom
              && transitions <> [] then
             let i = Random.int (List.length transitions) in
             [List.nth transitions i]
           else
             transitions
         in
         begin match List.hd transitions with
         | (i, _) -> Some (Interact_parser_base.Transitions [i])
         | exception Failure _ -> None
         end
      | exception Failure _ -> None
      end
  end

let update_default_cmd interact_state =
  { interact_state with
    default_cmd = default_cmd interact_state }

(* return the indices of the transitions leading to the head of
interact_state.interact_nodes; head is the last transition *)
let choices_so_far interact_state : int list =
  let rec trace accum : interact_node list -> int list = function
    | []      -> assert (accum = []); []
    | _ :: [] -> accum
    | n :: ns ->
        begin match n.open_transition with
        | []   -> trace accum ns
        | i :: _ -> trace (i :: accum) ns
        end
  in
  List.rev interact_state.interact_nodes |> trace []

let print_written_locations state =
  Screen.show_message state.ppmode "Read locations:";
  Pmap.iter (fun tid locations ->
      Screen.show_message state.ppmode "Thread %d: [%s]" tid (Pp.pp_list state.ppmode (Pp.pp_footprint state.ppmode None) locations))
            state.read_locations;
  Screen.show_message state.ppmode "Written locations:";
  Pmap.iter (fun tid locations ->
      Screen.show_message state.ppmode "Thread %d: %s" tid (Pp.pp_list state.ppmode (Pp.pp_footprint state.ppmode None) locations))
            state.written_locations;
  Screen.show_message state.ppmode "Shared locations:\n%s\n" (Pp.pp_list state.ppmode (Pp.pp_footprint state.ppmode None) state.shared_locations);
  Screen.show_message state.ppmode "Shared program locations:\n%s\n" (Pp.pp_list state.ppmode (Pp.pp_address state.ppmode None) (Pset.elements state.shared_program_locations))

let announce_options interact_state : unit =
  let ppmode = interact_state.ppmode in
  let pp_maybe_int suffix maybe_int =
    match maybe_int with
    | None   -> "none"
    | Some n -> Printf.sprintf "%d%s" n suffix
  in
  let pp_maybe_ioid suffix maybe_ioid =
    match maybe_ioid with
    | None            -> "none"
    | Some (tid, iid) -> Printf.sprintf "(%d:%d)%s" tid iid suffix
  in
  Screen.show_message ppmode "Global options: suppress_internal=%b record_writes=%b embedding=%s loop_limit=%s"
                      interact_state.options.suppress_internal
                      interact_state.options.record_shared_locations
                      (if interact_state.options.interpreter then "interpreter" else "shallow")
                      (pp_maybe_int "" (!Globals.model_params).t.thread_loop_unroll_limit);
  Screen.show_message ppmode "Eager mode: fetch (single successor)=%b fetch_new_branch=%b pseudocode_internal=%b constant_reg_read=%b reg_rw=%b memory_aux=%b finish=%b fp_recalc=%b"
                      interact_state.options.eager_mode.eager_fetch_single
                      interact_state.options.eager_mode.eager_fetch_multi
                      interact_state.options.eager_mode.eager_pseudocode_internal
                      interact_state.options.eager_mode.eager_constant_reg_read
                      interact_state.options.eager_mode.eager_reg_rw
                      interact_state.options.eager_mode.eager_memory_aux
                      interact_state.options.eager_mode.eager_finish
                      interact_state.options.eager_mode.eager_fp_recalc;
  Screen.show_message ppmode "Stepping options: random=%b storage_first=%b focused-thread=%s focused-instruction=%s"
                      interact_state.options.pseudorandom
                      interact_state.options.storage_first
                      (pp_maybe_int "" interact_state.options.focused_thread)
                      (pp_maybe_ioid "" interact_state.options.focused_ioid);
  Screen.show_message ppmode "Interface options: verbosity=%s pp_style=%s choice_history_limit=%s always_print=%b suppress_newpage=%b buffer_messages=%b"
                      (Globals.pp_verbosity_level !Globals.verbosity)
                      (Globals.pp_ppstyle ppmode.Globals.pp_style)
                      (pp_maybe_int "" ppmode.Globals.pp_choice_history_limit)
                      interact_state.options.always_print
                      ppmode.Globals.pp_suppress_newpage
                      ppmode.Globals.pp_buffer_messages;
  Screen.show_message ppmode "                   announce_options=%b prefer_symbolic_values=%b pp_hide_pseudoregister_reads=%b pp_colours=%b condense_finished_instructions=%b max_finished=%s"
                      ppmode.Globals.pp_announce_options
                      ppmode.Globals.pp_prefer_symbolic_values
                      ppmode.Globals.pp_hide_pseudoregister_reads
                      ppmode.Globals.pp_colours
                      ppmode.Globals.pp_condense_finished_instructions
                      (pp_maybe_int "" ppmode.Globals.pp_max_finished);
  Screen.show_message ppmode "                   pp_hex=%b dwarf_show_all_variable_locations=%b pp_sail=%b dumb_terminal=%b"
                      !Globals.print_hex
                      !Globals.dwarf_show_all_variable_locations
                      ppmode.Globals.pp_sail
                      !Globals.dumb_terminal;
  Screen.show_message ppmode "Graph options: always_graph=%b dot_final_ok=%b dot_final_not_ok=%b ppg_shared=%b ppg_rf=%b ppg_fr=%b ppg_co=%b ppg_addr=%b ppg_data=%b ppg_ctrl=%b ppg_ppg_regs=%b ppg_reg_rf=%b ppg_trans=%b graph_backend=%s"
                      (!Globals.run_dot = Some Globals.RD_step)
                      (!Globals.run_dot = Some Globals.RD_final_ok)
                      (!Globals.run_dot = Some Globals.RD_final_not_ok)
                      ppmode.Globals.ppg_shared
                      ppmode.Globals.ppg_rf
                      ppmode.Globals.ppg_fr
                      ppmode.Globals.ppg_co
                      ppmode.Globals.ppg_addr
                      ppmode.Globals.ppg_data
                      ppmode.Globals.ppg_ctrl
                      ppmode.Globals.ppg_regs
                      ppmode.Globals.ppg_reg_rf
                      ppmode.Globals.ppg_trans
                      (Globals.pp_graph_backend !Globals.graph_backend);
  Screen.show_message ppmode "Search options: check_inf_loop=%b hash_prune=%b partial_order_reduction=%b priority_reduction=%b allow_partial=%b "
                      interact_state.options.check_inf_loop
                      interact_state.options.hash_prune
                      interact_state.options.partial_order_reduction
                      interact_state.options.priority_reduction
                      interact_state.options.allow_partial;
  Screen.show_message ppmode "                transition-limit=%s trace-limit=%s time-limit=%s"
                      (pp_maybe_int "" interact_state.options.transition_limit)
                      (pp_maybe_int "" interact_state.options.trace_limit)
                      (pp_maybe_int "s" interact_state.options.time_limit);
  Screen.show_message ppmode "Model options: %s"
                      (Model_aux.pp_model (match interact_state.interact_nodes with
                                           | []        -> !Globals.model_params
                                           | node :: _ -> node.system_state.sst_state.model));
  Screen.show_message ppmode "Version: %s" Versions.Rmem.describe

let print_last_state interact_state : unit =
  let ppmode = interact_state.ppmode in
  begin match interact_state.interact_nodes with
  | [] -> Screen.show_message interact_state.ppmode "no state"

  | node :: ns ->
      let ts = sorted_filtered_transitions node in
      let (ppmode', ui_state) =
        match ns with
        | [] ->
            ConcModel.make_ui_system_state ppmode None node.system_state.sst_state ts
        | node' :: _ ->
            ConcModel.make_ui_system_state ppmode (Some node'.system_state.sst_state) node.system_state.sst_state ts
      in
      let ppmode'' = { ppmode' with Globals.pp_default_cmd = interact_state.default_cmd } in
      let cand_ex = ConcModel.make_cex_candidate node.system_state.sst_state in
      Screen.draw_system_state
        ppmode''
        (choices_so_far interact_state)
        interact_state.follow_suffix
        node.system_state.sst_state
        ui_state
        (Some cand_ex)
        ts
        (filtered_out_transitions node)
  end;
  if ppmode.Globals.pp_announce_options then begin
      announce_options interact_state
    end else ();
  if interact_state.options.record_shared_locations then
    print_written_locations interact_state
    (* SF: I don't know why this is caught here, if you find this bug
    please make the exception more descriptive:
    with Failure "hd" -> ()
    *)
  else ()

(* map each transition to a pair of (bool, trans) where the bool is
true iff the transition is "interesting" *)
let filter_transitions interact_state system_state transitions =
  let transitions = List.map (fun t -> (true, t)) transitions in
  let transitions =
    match interact_state.options.focused_thread with
    | Some n ->
        List.map
          (fun (b, t) -> (b && MachineDefTypes.thread_id_of_thread_transition t = Some n, t))
          transitions
    | None -> transitions
  in
  let transitions =
    match interact_state.options.focused_ioid with
    | Some target_ioid ->
        List.map
          (fun (b, t) -> (b && MachineDefTypes.is_transition_of_ioid target_ioid t, t))
          transitions
    | None -> transitions
  in
  (* more filters expected later *)
  transitions

let generate_transitions interact_state interact_nodes system_state =
  let transitions = filter_transitions
                      interact_state
                      system_state
                      system_state.sst_system_transitions in
  (* add numbers to the transitions; try to use the same number
  for transitions that were present in the previous state *)
  match interact_nodes with
  | []      -> number_transitions None transitions
  | n' :: _ -> number_transitions (Some (sorted_filtered_transitions n')) transitions


let add_interact_node ?(open_transition=[]) ?follow_ast node_type interact_state system_state : interact_state =
  let transitions = generate_transitions
                      interact_state
                      interact_state.interact_nodes
                      system_state
  in

  let new_interact_node =
    {
      node_type            = node_type;
      system_state         = system_state;
      open_transition      = open_transition;
      filtered_transitions = transitions;
      eager_mode           = interact_state.options.eager_mode;
      follow_ast           = follow_ast;
    }
  in

  { interact_state with
    interact_nodes = new_interact_node :: interact_state.interact_nodes;
  } |> update_default_cmd

let rec refilter_nodes interact_state interact_nodes =
  match interact_nodes with
  | []                     -> []
  | node :: nodes -> { node with
                       filtered_transitions = generate_transitions
                                                interact_state
                                                nodes
                                                node.system_state }
                     :: refilter_nodes interact_state nodes


let record_read_location tid footprint state =
  if state.options.record_shared_locations then
    let current_list =
      (try Pmap.find tid state.read_locations
       with Not_found -> []) in
    let new_list = Runner.coalesce_sorted_footprints
                     (List.sort_uniq
                        MachineDefTypes.footprintCompare
                        (footprint @ current_list)) in
    let new_read_locations = Pmap.add tid new_list state.read_locations in
    let new_shared_locations = Runner.find_overlaps new_read_locations state.written_locations in
    let new_program_locations =
      match List.hd state.interact_nodes with
      | root_node ->
          Runner.get_shared_program_locations
            root_node.system_state.sst_state
            new_shared_locations
          |> Pset.union state.shared_program_locations
      | exception Failure _ -> state.shared_program_locations
    in
    { state with
      read_locations = new_read_locations;
      shared_locations = new_shared_locations;
      shared_program_locations = new_program_locations
    }
  else
    state

let record_write_location tid footprint state =
  if state.options.record_shared_locations then
    let current_list =
      (try Pmap.find tid state.written_locations
       with Not_found -> []) in
    let new_list = Runner.coalesce_sorted_footprints
                     (List.sort_uniq
                        MachineDefTypes.footprintCompare
                        (footprint @ current_list)) in
    let new_written_locations = Pmap.add tid new_list state.written_locations in
    let new_shared_locations = Runner.find_overlaps state.read_locations new_written_locations in
    let new_program_locations =
      match List.hd state.interact_nodes with
      | root_node ->
          Runner.get_shared_program_locations
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

let update_shared_memory interact_state transition =
  (* shared memory approximation housekeeping *)
  let maybe_thread_id = MachineDefTypes.thread_id_of_thread_transition transition in
  let maybe_read_footprint = MachineDefTransitionUtils.read_footprint_of_trans transition in
  let write_footprints = MachineDefTransitionUtils.write_footprints_of_trans transition in
  let interact_state =
    (match (maybe_thread_id, maybe_read_footprint) with
     | (Some tid, Some footprint) -> record_read_location tid [footprint] interact_state
     | _ -> interact_state) in
  let interact_state =
    (match (maybe_thread_id, write_footprints) with
     | (Some tid, _ :: _) -> record_write_location tid write_footprints interact_state
     | _ -> interact_state) in
  interact_state

let rec do_transition ?(eager=false) (n: int) interact_state : interact_state =
  begin match interact_state.interact_nodes with
  | [] -> Screen.show_warning interact_state.ppmode "no initial state"; interact_state
  | interact_node :: interact_nodes' ->
      begin match List.find (function (Some n', _) -> n' = n | _ -> false) interact_node.filtered_transitions with
      | (_, transition) ->
         match ConcModel.sst_after_transition interact_state.options
                                              interact_node.system_state
                                              transition with
         | TO_unhandled_exception (tid, ioid, e) ->
              Screen.show_message interact_state.ppmode "the transition leads to exception";
              (* TODO: pp the exception *)
              interact_state
         | TO_system_state system_state' -> begin
             let interact_node =
               if eager then
                 { interact_node with open_transition = interact_node.open_transition @ [n] }
               else
                 { interact_node with open_transition = [n] }
             in

             let follow_suffix =
               if eager then interact_state.follow_suffix
               else match interact_state.follow_suffix with
                    | Interact_parser_base.Transitions [n'] :: ns when n' = n -> ns
                    | _ -> []
             in

             let open_transition =
               if eager then
                 interact_node.open_transition
               else
                 []
             in

             let interact_state =
               { interact_state with
                 interact_nodes =
                   if eager then
                     interact_nodes'
                   else
                     interact_node :: interact_nodes';
                 follow_suffix = follow_suffix;
               }
             in

             let interact_state = update_shared_memory interact_state transition in

             add_interact_node
               ~open_transition
               ~follow_ast:(Interact_parser_base.Transitions [n])
               (if eager then EagerTransitionsNode else TransitionNode)
               interact_state
               system_state'
             |> check_eager
           end

      | exception Not_found ->
          Screen.show_warning interact_state.ppmode "current state does not have transition %d" n;
          interact_state
      end
  end
and check_eager interact_state : interact_state =
  let search_iter interact_state =
    match List.hd interact_state.interact_nodes with
    | node ->
        begin match
          List.find (fun (f, t) -> f <> None && is_eager_in_interact interact_state t)
              node.filtered_transitions
        with
        | (Some i, t) -> begin
              if Globals.is_verbosity_at_least Globals.Debug then
                Screen.show_message interact_state.ppmode "+ check_eager taking [%d] %s" i (Pp.pp_trans interact_state.ppmode t);
              do_transition ~eager:true i interact_state
            end
        | (None, _) -> assert false
        | exception Not_found -> interact_state
        end
    | exception Failure _ -> interact_state
  in
  let rec search_until_fixed_shared interact_state =
    let previous_shared_locations = interact_state.shared_locations in
    let state' = search_iter interact_state in
    let new_shared_locations = state'.shared_locations in
    if previous_shared_locations = new_shared_locations then
      state'
    else
      search_until_fixed_shared { interact_state with read_locations = state'.read_locations;
                                                      written_locations = state'.written_locations;
                                                      shared_locations = state'.shared_locations;
                                }
  in
  if interact_state.options.eager_up_to_shared then
    let open Lens.Infix in
    (* eager_up_to_shared must imply record_shared_locations to work *)
    search_until_fixed_shared (((run_options_lens |-- record_shared_locations_lens) ^= true) interact_state)
  else
    search_iter interact_state

let change_model new_model interact_state : interact_state =
  let new_state =
    { interact_state with
      interact_nodes =
        (* TODO: lenses because look at this *)
        List.map (fun node ->
            { node with
              system_state =
                { node.system_state with
                  sst_state =
                    { node.system_state.sst_state with
                      model = new_model
                    }
                }
            }
          ) interact_state.interact_nodes
    }
  in
  Globals.model_params := new_model;
  match new_state.interact_nodes with
  | []            -> new_state (* nothing to change *)
  | node :: nodes ->
     let (system_trans, storage_trans, thread_trans, sys_thread_trans) =
       ConcModel.enumerate_transitions
         new_state.options
         node.system_state.sst_state
         None (Pmap.empty compare) (Pmap.empty compare) (* empty transition caches to fully recalculate *)
     in
      add_interact_node
        ModelChangeNode
        new_state
        { node.system_state with
          sst_system_transitions = system_trans;
          sst_storage_transitions = storage_trans;
          sst_thread_transitions = thread_trans;
          sst_sys_thread_transitions = sys_thread_trans;
        }

let rec do_fetch_all interact_state : interact_state =
  match List.hd interact_state.interact_nodes with
  | root_node ->
      begin match List.find
                    (fun (f, transition) ->
                        f <> None &&
                        (MachineDefTransitionUtils.is_fetch_single_transition transition ||
                        ((!Globals.model_params).t.thread_allow_tree_speculation && MachineDefTransitionUtils.is_fetch_multi_transition transition)))
                    root_node.filtered_transitions
      with
      | (Some i, _)         -> do_fetch_all (do_transition i interact_state)
      | (None,   _)         -> assert false
      | exception Not_found -> interact_state
      end

  | exception Failure _ -> interact_state

let rec do_back count interact_state : interact_state =
  if count <= 0 then interact_state else
  begin match interact_state.interact_nodes with
  | [] ->
      Screen.show_warning interact_state.ppmode "no state";
      interact_state
  | n :: [] ->
      Screen.show_warning interact_state.ppmode "can't go back from the initial state";
      interact_state
  | n :: n' :: ns ->
     { interact_state with
       interact_nodes = {n' with open_transition = []} :: ns;
       options = { interact_state.options with eager_mode = n'.eager_mode };
       follow_suffix = match n.follow_ast with
                       | Some ast -> ast :: interact_state.follow_suffix
                       | None     -> interact_state.follow_suffix;
     }
     |> update_default_cmd
  end

exception TraceRecon of string

let rec reconstruct_search_trace
    (search_trace: Runner.trace) (* head is the first/old transition; each element is also pre-reversed to be [real; eager; eager; ...] *)
    interact_state
    : interact_state
  =
  begin match search_trace with
  | []           -> interact_state
  | [] :: trace' -> reconstruct_search_trace trace' interact_state
  | (i :: is) :: trace' ->
    if Globals.is_verbosity_at_least Globals.Debug then
        Screen.show_message interact_state.ppmode "reconstructing from [%s]" (Pp.pp_list interact_state.ppmode (fun l -> "[" ^ (Pp.pp_list interact_state.ppmode string_of_int l) ^ "]") ((i::is)::trace'));

    let (i', t) =
      (* List.hd should not fail *)
      let node = List.hd interact_state.interact_nodes in
      match List.nth node.filtered_transitions i with
      | (Some i', t) -> (i', t)
      | (None,    _) ->
          raise (TraceRecon (Printf.sprintf "bad transition number %d (transition is filtered out) for trans list [\n%s] in trace %s"
              i
              (Pp.pp_list interact_state.ppmode
                  (fun t -> (Pp.pp_trans interact_state.ppmode t) ^ "\n")
                  node.system_state.sst_system_transitions)
              (Pp.pp_trace interact_state.ppmode search_trace)
          ))
      | exception Failure _ -> (* nth *)
          raise (TraceRecon (Printf.sprintf "bad transition number %d (too big) for trans list [\n%s] in trace %s"
              i
              (Pp.pp_list interact_state.ppmode
                  (fun t -> (Pp.pp_trans interact_state.ppmode t) ^ "\n")
                  node.system_state.sst_system_transitions)
              (Pp.pp_trace interact_state.ppmode search_trace)
          ))
    in

    let interact_state =
      if Globals.is_verbosity_at_least Globals.Debug then
        Screen.show_message interact_state.ppmode "reconstructing via [%d] %s" i' (Pp.pp_trans interact_state.ppmode t);
      do_transition i' interact_state
      |> append_to_history (ref false) (Interact_parser_base.Transitions [i'])
    in

    reconstruct_search_trace (trace') interact_state
  end

let ui_choices_of_search_trace
    interact_state
    interact_node
    (search_trace: Runner.trace) (* head is the last transition *)
    : (Interact_parser_base.ast list * string) (* head is the last cmd *)
  =
  let interact_state =
    { interact_state with
      interact_nodes = [interact_node];
      cmd_history = [];
    }
    |> check_eager
    |> reconstruct_search_trace (List.rev search_trace |> List.map List.rev)
  in
  let node = List.hd interact_state.interact_nodes in
  let (_, ui_state) = ConcModel.make_ui_system_state interact_state.ppmode
                                                     None
                                                     node.system_state.sst_state
                                                     (sorted_filtered_transitions node)
  in
  (
    interact_state.cmd_history,
    (Pp.pp_transition_history interact_state.ppmode
                              ~filter:(fun t -> not (is_eager_in_interact interact_state t))
                              ui_state)
  )

let print_history interact_state : unit =
  Screen.show_message interact_state.ppmode "\"%s\"" (List.rev interact_state.cmd_history |> List.map Interact_parser_base.pp |> String.concat ";" |> Screen.escape)


let print_observed_finals interact_state interact_node observed_finals : Screen_base.output_tree =
  let symtab =
    List.map
      (fun ((addr, _), s) -> (Test.C.interp_address_to_address addr, s))
      interact_state.ppmode.Globals.pp_symbol_table
  in

  let states_count_output = otStrLine "States %i" (Runner.StateMap.cardinal observed_finals) in

  let states_output =
    let check_prop =
      match interact_state.test_info.Test.constr with
      | ConstrGen.ForallStates p
      | ConstrGen.ExistsState p
      | ConstrGen.NotExistsState p -> Test.C.check_prop p
    in

    OTConcat
      (List.map
        (fun (state, (choices, count)) ->
          let (ui_choices, ui_transitions) =
            match ui_choices_of_search_trace interact_state interact_node choices with
            | (choices, transitions) -> (Interact_parser_base.history_to_string choices, transitions)
            | exception (TraceRecon s) ->
                (Printf.sprintf "could not reconstruct trace (%s)" s, "")
          in
          OTConcat (otLine (OTConcat [otStrVerbose Globals.Normal "%-6d%s>" count (if check_prop state then "*" else ":");
                                      otString "%s" (Test.C.pp_state symtab state);
                                      OTVerbose (Globals.Normal, OTConcat [
                                                                      OTString "via \"";
                                                                      OTFollowList ui_choices;
                                                                      OTString "\""
                                                ])])
                    :: if Globals.is_verbosity_at_least Globals.Debug then
                         [otLine (OTString ui_transitions)]
                       else
                         [])
        )
        (Runner.StateMap.bindings observed_finals))
  in

  OTConcat [states_count_output; states_output]

let print_observed_deadlocks interact_state interact_node observed_deadlocks : Screen_base.output_tree =
  match observed_deadlocks with
  | Some (deadlock_choices, deadlock_count) ->
      let via =
        match ui_choices_of_search_trace interact_state interact_node deadlock_choices with
        | (choices, _) ->
            OTConcat [
              OTString "via \"";
              OTFollowList (Interact_parser_base.history_to_string choices);
              OTString "\"";
            ]
        | exception (TraceRecon s) ->
            otString "(could not reconstruct trace (%s))" s
      in

      otVerbose Globals.Normal @@ otLine @@
        OTConcat [
          otString "Deadlock states %i" deadlock_count;
          OTString " ";
          via;
        ]
  | None -> OTEmpty

let print_observed_exceptions interact_state interact_node observed_exceptions : Screen_base.output_tree =
  let exceptions_count_output = otVerbose Globals.Normal @@ otStrLine "Unhandled exceptions %i" (Runner.ExceptionMap.cardinal observed_exceptions) in

  let exceptions_output =
    otVerbose Globals.Normal @@
    OTConcat
      (List.map
        (fun ((tid, ioid, exception_type), (choices, count)) ->
          let ui_choices =
            match ui_choices_of_search_trace interact_state interact_node choices with
            | (choices, _) -> Interact_parser_base.history_to_string choices
            | exception (TraceRecon s) ->
                Printf.sprintf "could not reconstruct trace (%s)" s
          in
          otLine @@ OTConcat [otString "%-6d:>" count;
                              otString "thread %d instruction %s: %s" tid (Pp.pp_pretty_ioid ioid) (Pp.pp_exception interact_state.ppmode ioid exception_type);
                              OTConcat [
                                  OTString "via \"";
                                  OTFollowList ui_choices;
                                  OTString "\""
                                ]
                             ]
        )
        (Runner.ExceptionMap.bindings observed_exceptions))
  in

  OTConcat [exceptions_count_output; exceptions_output]

(* TODO FIXME: this should by all rights be in Pp but that's a circular dependency... *)
let pp_breakpoint ppmode transitions (id, (pred, desc)) reason =
  let id_string = match id with
    | Runner.Numbered n -> Printf.sprintf "#%d" n
    | Runner.Named (n, s) -> Printf.sprintf "#%d (\"%s\")" n s
  in
  let type_string = match pred with
    | Runner.StateBreakpoint _ -> Printf.sprintf "when state %s" desc
    | Runner.TransitionBreakpoint _ | Runner.SharedBreakpoint _ ->
       Printf.sprintf "when transition about to be taken %s" desc
  in
  let reason_string =
    let find_id t ts =
      (* TODO FIXME: fuzzy_compare_transitions is not exact *)
      fst (List.find (fun (_, t') -> Model_aux.fuzzy_compare_transitions t' t = 0) ts)
    in
    try
      match reason with
      | Runner.NoReason -> ""
      | Runner.StateReason _ -> ""
      | Runner.TransReason (trans, state) ->
         Printf.sprintf "(transition %d)" (find_id trans transitions)
      | Runner.SharedReason (trans, state, footprint) ->
         Printf.sprintf "(transition %d, location %s)" (find_id trans transitions) (Pp.pp_footprint ppmode None footprint)
    with Not_found -> "(could not find UI transition)"
  in
  String.concat " " [id_string; type_string; reason_string]

let wrap_state_pred pred = fun state ->
  if pred state then Runner.StateReason state else Runner.NoReason

let wrap_trans_pred pred = fun state trans ->
  if pred state trans then Runner.TransReason (trans, state) else Runner.NoReason

let update_shared interact_state search_state =
  if interact_state.options.record_shared_locations then
    let update_thread other_locations tid locations =
      try Runner.coalesce_sorted_footprints
            (List.sort MachineDefTypes.footprintCompare
                       (locations @ (Pmap.find tid other_locations)))
      with Not_found -> locations in
    (* union to handle new tids not already in the interact_state *)
    let new_read_locations = Pmap.union search_state.Runner.read_locations
                                        (Pmap.mapi (update_thread search_state.Runner.read_locations)
                                                   interact_state.read_locations) in
    let new_written_locations = Pmap.union search_state.Runner.written_locations
                                           (Pmap.mapi (update_thread search_state.Runner.written_locations)
                                                      interact_state.written_locations) in
    { interact_state with
      read_locations = new_read_locations;
      written_locations = new_written_locations;
      shared_locations = Runner.find_overlaps new_read_locations new_written_locations;
    }
  else
    interact_state


let do_search mode interact_state breakpoints bounds targets filters: interact_state =
  begin match interact_state.interact_nodes with
  | [] ->
      Screen.show_warning interact_state.ppmode "no state";
      interact_state
  | node :: nodes' ->
      (* let general_run_options = { *)
      (*   interact_state.options with *)
      (*     (\* TODO FIXME HACK: for now, match the current interactive eager mode *\) *)
      (*     (\*                  in future, we want to always search eagerly because *\) *)
      (*     (\*                  otherwise the search space is blown up unnecessarily *\) *)
      (*     (\* eager            = true; *\) *)
      (*   hash_prune             = true; *)
      (*   suppress_internal      = true; *)
      (* } in *)
      (* OK, OK, I give up. For now. *)
      let general_run_options = interact_state.options in
      let run_options =
        match mode with
        | Interact_parser_base.Exhaustive -> {
            general_run_options with
            pseudorandom        = false;
            pseudorandom_traces = 1;
          }
        | Interact_parser_base.Random i -> {
            general_run_options with
            pseudorandom        = true;
            pseudorandom_traces = i;
          }
      in

      begin match Runner.search_from_state ~ppmode:interact_state.ppmode run_options interact_state.test_info
          node.system_state
          (breakpoints @ interact_state.breakpoints)
          bounds targets filters
          (* TODO: bounds, targets, filters from interact_state*)
          (fun _ -> ()) (* TODO: print_partial_results? *)
      with
      | Runner.Complete search_state ->
          Screen_base.OTConcat
            [ print_observed_finals interact_state node search_state.Runner.observed_filterred_finals;
              print_observed_deadlocks interact_state node search_state.Runner.observed_deadlocks;
              print_observed_exceptions interact_state node search_state.Runner.observed_exceptions;
            ]
          |> Screen.of_output_tree |> Screen.final_message interact_state.ppmode false;
          update_shared interact_state search_state

      | Runner.Breakpoint (search_state, bp, reason) ->

          let search_trace =
            Runner.choices_so_far search_state
            |> List.rev |> List.map List.rev
          in begin try
                 let new_state = update_shared (reconstruct_search_trace search_trace interact_state) search_state in
                 Screen.show_message interact_state.ppmode "Breakpoint hit: %s"
                                     (pp_breakpoint
                                        interact_state.ppmode
                                        (sorted_filtered_transitions (List.hd new_state.interact_nodes))
                                        bp
                                        reason);
                 new_state
               with TraceRecon s ->
                 Screen.show_message interact_state.ppmode "Breakpoint hit: %s (problem reconstructing trace)" (pp_breakpoint interact_state.ppmode [] bp Runner.NoReason);
                 Screen.show_warning interact_state.ppmode "%s" (Screen.escape s);
                 interact_state
             end
      | Runner.Interrupted (search_state, reason) ->
         Screen.show_message interact_state.ppmode "Interrupted because %s" reason;
          let search_trace =
            Runner.choices_so_far search_state
            |> List.rev |> List.map List.rev
          in
          begin try update_shared (reconstruct_search_trace search_trace interact_state) search_state with
          | TraceRecon s ->
              Screen.show_warning interact_state.ppmode "%s" (Screen.escape s);
              interact_state
          end
      | Runner.OcamlExn (search_state, msg) ->
          Screen.show_warning interact_state.ppmode "%s" msg;
          Screen.show_warning interact_state.ppmode "%s" ("***********************\n" ^
                                                          "*** PARTIAL RESULTS ***\n" ^
                                                          "***********************");
          Screen_base.OTConcat
            [ print_observed_finals interact_state node search_state.Runner.observed_filterred_finals;
              print_observed_deadlocks interact_state node search_state.Runner.observed_deadlocks;
              print_observed_exceptions interact_state node search_state.Runner.observed_exceptions;
            ]
          |> Screen.of_output_tree |> Screen.final_message interact_state.ppmode false;
          Screen.show_warning interact_state.ppmode "*** Error ***\n";
          interact_state
      end
  end

let fresh_bp_id bps' =
  let numbers = List.map
                  (fun (id, _) ->
                    match id with Runner.Numbered n | Runner.Named (n, _) -> n)
                  bps'
  in
  match numbers with
  | [] -> Runner.Numbered 1
  | numbers -> Runner.Numbered (1 + List.hd (List.sort (fun x y -> -(compare x y)) numbers))

let add_trans_breakpoint pred desc interact_state : interact_state =
  { interact_state with
    breakpoints = ((fresh_bp_id interact_state.breakpoints),
                   (Runner.TransitionBreakpoint pred, desc)) :: interact_state.breakpoints
  }

let add_shared_breakpoint pred desc interact_state : interact_state =
  { interact_state with
    breakpoints = ((fresh_bp_id interact_state.breakpoints),
                   (Runner.SharedBreakpoint pred, desc)) :: interact_state.breakpoints
  }


let pp_dwarf_line_of_addr m addr =
  match m.Globals.pp_dwarf_static with
  | Some ds ->
     (
       match Pp.pp_dwarf_source_file_lines m ds true addr with
       | Some s -> Printf.sprintf " [%s]" s
       | None -> ""
     )
  | None -> ""

let big_range (x : Nat_big_num.num) (y : Nat_big_num.num) =
  let rec big_range' (x : Nat_big_num.num) (y : Nat_big_num.num) acc =
    if y < x then acc
    else big_range' x (Nat_big_num.sub y (Nat_big_num.of_int 1)) (y :: acc)
  in
  big_range' x (Nat_big_num.sub y (Nat_big_num.of_int 1)) []


let do_add_breakpoint_fetch target state : interact_state =
  let open Interact_parser_base in
  try
    (let (addr', size) = (
         match target with
         | Address a ->        (a, Nat_big_num.of_int 1)
         | Footprint (a, n) -> (a, Nat_big_num.of_int n)
         | Symbol (name, offset) ->
            (* TODO FIXME: this probably isn't right (?)
               Sometimes symbols can have multiple values because of e.g. inlining... *)
            let (_, size, addr, _, _) = List.assoc name state.test_info.Test.symbol_map in
            ((Nat_big_num.add addr (Nat_big_num.of_int offset)), size)

       ) in
     let addr = Sail_impl_base.address_of_integer addr' in
     let addrs = List.map Sail_impl_base.address_of_integer (big_range addr' size) in
     add_trans_breakpoint
       (wrap_trans_pred (fun state trans ->
            List.exists (fun addr -> MachineDefTransitionUtils.trans_fetches_address addr state trans) addrs))
       (Printf.sprintf "fetches address 0x%s/%d%s"
                       (Misc.big_num_to_hex_string (Sail_impl_base.integer_of_address addr))
                       (Nat_big_num.to_int size)
                       (pp_dwarf_line_of_addr state.ppmode addr)
       )
       state)
  with Not_found ->
       Screen.show_warning state.ppmode "no such symbol"; state

let do_add_watchpoint typ target state : interact_state =
  let open Interact_parser_base in
  try
    let (addr, size) = (
         match target with
         | Address a -> (Sail_impl_base.address_of_integer a, 1)
         | Footprint (a, n) -> (Sail_impl_base.address_of_integer a, n)
         | Symbol (name, offset) ->
            (* TODO FIXME: this probably isn't right (?)
               Sometimes symbols can have multiple values because of e.g. inlining... *)
            (let (_, size, addr, _, _) = List.assoc name state.test_info.Test.symbol_map in
             (Sail_impl_base.address_of_integer (Nat_big_num.add addr (Nat_big_num.of_int offset)),
              Nat_big_num.to_int size))
       ) in
     let f = match typ with
       | Read -> MachineDefTransitionUtils.trans_reads_footprint (addr, size)
       | Write -> MachineDefTransitionUtils.trans_writes_footprint (addr, size)
       | Either -> (fun state trans -> MachineDefTransitionUtils.trans_reads_footprint (addr, size) state trans
                                      || MachineDefTransitionUtils.trans_writes_footprint (addr, size) state trans)
     in
     let verb = (match typ with
       | Read -> "reads"
       | Write -> "writes"
       | Either -> "reads or writes")
     in
     add_trans_breakpoint (wrap_trans_pred f) (Printf.sprintf "%s address 0x%x"
                                            verb
                                            (Nat_big_num.to_int (Sail_impl_base.integer_of_address addr)))
                          state
  with Not_found ->
       Screen.show_warning state.ppmode "no such symbol"; state


let do_add_shared_watchpoint typ state : interact_state =
  let open Interact_parser_base in
  try
    let pred = match typ with
      | Read -> MachineDefTransitionUtils.trans_reads_footprint
      | Write -> MachineDefTransitionUtils.trans_writes_footprint
      | Either -> (fun fp state trans -> MachineDefTransitionUtils.trans_reads_footprint fp state trans
                                         || MachineDefTransitionUtils.trans_writes_footprint fp state trans)
    in
    let f fps state trans =
      try
        Runner.SharedReason (trans, state, (List.find (fun fp -> pred fp state trans) fps))
      with Not_found -> Runner.NoReason
    in
    let verb = (match typ with
                | Read -> "reads"
                | Write -> "writes"
                | Either -> "reads or writes")
    in
    add_shared_breakpoint f (Printf.sprintf "%s shared memory" verb)
                          state
  with Not_found ->
       Screen.show_warning state.ppmode "no such symbol"; state


  (*    let a = Sail_impl_base.address_of_integer addr in *)
  (* add_trans_breakpoint *)
  (*   (MachineDefSystem.trans_reads_footprint (a, size)) *)
  (*   (Printf.sprintf "reads address 0x%x%s" *)
  (*                   (Nat_big_num.to_int addr) *)
  (*                   ( *)
  (*                     try *)
  (*                       let name = List.assoc (a, size) state.test_info.Test.symbol_table in *)
  (*                       Printf.sprintf " [%s]" name *)
  (*                     with Not_found -> "" *)
  (*                   ) *)
  (*   ) *)
  (*   state *)

let make_graph interact_state =
  match List.hd interact_state.interact_nodes with
  | node ->
      match interact_state.ppmode.Globals.pp_kind with
      | Globals.Html ->
        Screen.display_dot { interact_state.ppmode with Globals.pp_default_cmd = interact_state.default_cmd }
                            (Some interact_state.test_info.Test.name)
                            node.system_state.sst_state
                            (ConcModel.make_cex_candidate node.system_state.sst_state)
                            (sorted_filtered_transitions node)
      | _ ->
        let module G = (val get_graph_backend ()) in
        G.make_graph
          interact_state.ppmode
          interact_state.test_info
          node.system_state.sst_state
          (ConcModel.make_cex_candidate node.system_state.sst_state)
          (sorted_filtered_transitions node);
        (* TODO: make filename configurable and more DRY *)
        Screen.show_message interact_state.ppmode "wrote %s" "out.dot"
  | exception Failure _ ->
      Screen.show_warning interact_state.ppmode "could not generate graph from no state"

let typeset interact_state filename =
  match interact_state.interact_nodes with
  | node :: _ ->
      let state = node.system_state.sst_state in
      let ppmode' = {interact_state.ppmode with Globals.pp_kind = Globals.Latex} in
      let prev_state =
        match interact_state.interact_nodes with
        | _ :: node' :: _ -> Some node'.system_state.sst_state
        | _ -> None
      in

      let (ppmode', ui_state) =
        ConcModel.make_ui_system_state ppmode' prev_state state (sorted_filtered_transitions node)
      in
      let fd = open_out filename in
      Printf.fprintf fd "%s\n" (Pp.pp_ui_system_state ppmode' ui_state);
      close_out fd;
      Screen.show_message interact_state.ppmode "wrote %s" (Screen.escape filename)

  | [] ->
      Screen.show_warning interact_state.ppmode "could not typeset from no state"


let do_add_breakpoint_line filename line interact_state : interact_state =
  let pred state transition =
    match transition with
    | T_trans (T_sync ((T_fetch {tl_label = (addr, _)}), _)) ->
       (match interact_state.ppmode.Globals.pp_dwarf_static with
        | Some ds ->
               let lines = Dwarf.source_lines_of_address ds (Sail_impl_base.integer_of_address addr) in
               List.exists
                 (fun (filename', line', _) ->
                   filename' = filename && (Nat_big_num.to_int line') = line)
                 lines
        | None ->
           failwith "No DWARF static information (this should have been caught when attempting to add this breakpoint)"
       )
    | _ -> false
  in
  match interact_state.ppmode.Globals.pp_dwarf_static with
  | Some _ ->
     add_trans_breakpoint (wrap_trans_pred pred) (Printf.sprintf "reaches line %s:%d" filename line) interact_state
  | None ->
     Screen.show_warning interact_state.ppmode
       "No DWARF static information available, cannot break on line numbers.";
     interact_state

let compare_breakpoints (id1, _) (id2, _) =
  let n1 = match id1 with Runner.Numbered n | Runner.Named (n, _) -> n in
  let n2 = match id2 with Runner.Numbered n | Runner.Named (n, _) -> n in
  compare n1 n2

let do_info_breakpoints interact_state : interact_state =
  (match interact_state.breakpoints with
  | [] -> Screen.show_warning interact_state.ppmode "No breakpoints defined."
  | bps -> List.iter (fun bp -> Screen.show_message interact_state.ppmode "%s" (pp_breakpoint interact_state.ppmode [] bp Runner.NoReason))
                     (List.sort compare_breakpoints bps));
  interact_state

let do_delete_breakpoint n interact_state : interact_state =
  try
    let key = fst (List.find (fun (id, _) -> match id with
                                             | Runner.Numbered n' | Runner.Named (n', _) -> n = n')
                             interact_state.breakpoints)
    in { interact_state with
         breakpoints = List.remove_assoc key interact_state.breakpoints }
  with Not_found ->
       Screen.show_warning interact_state.ppmode "no such breakpoint %d" n;
       interact_state

exception Step_thread_instruction of int
exception Step_any_instruction
exception Step_specific_instruction of int * int

let rec filter_map f l =
  match l with
  | [] -> []
  | x :: xs -> match (f x) with
               | None -> filter_map f xs
               | Some y -> y :: filter_map f xs

let rec do_step_instruction (maybe_thread_n : int option) (maybe_inst_n : int option) interact_state : interact_state =
  let m = interact_state.ppmode in
  begin match interact_state.interact_nodes with
        | [] -> Screen.show_warning interact_state.ppmode "no initial state"; interact_state
        | node :: _ ->
           try
             (* TODO FIXME: should we know about the internal structure of an ioid? *)
             let ioid =
               match (maybe_thread_n, maybe_inst_n) with
               | (None, Some _) ->
                  (* makes no sense *)
                  assert false
               | (None, None) ->
                  (* bare stepi, find lowest-numbered unfinished instruction from lowest-numbered thread *)
                  let transitions =
                    sorted_filtered_transitions node
                    |> List.map snd
                  in
                  let sorted_transitions = (List.sort compare
                                                      (filter_map MachineDefTypes.ioid_of_thread_transition transitions))
                  in
                  begin
                    Screen.show_message m "transitions:\n%s" (String.concat "\n" (List.map (Pp.pp_trans m) transitions));
                    Screen.show_message m "sorted_transitions:\n%s" (String.concat "\n" (List.map (fun (x, y) -> Printf.sprintf "(%d:%d)" x y) sorted_transitions));
                    try List.hd sorted_transitions with
                    | Failure _ -> raise Step_any_instruction
                  end
               | (Some tid, None) ->
                  (* step thread, find lowest-numbered unfinished instruction in that thread *)
                  let transitions =
                    sorted_filtered_transitions node
                    |> List.map snd
                  in
                  let thread_transitions = (List.filter
                                              (MachineDefTypes.is_transition_of_thread tid)
                                              transitions) in
                  let sorted_thread_transitions = (List.sort compare
                                                             (filter_map MachineDefTypes.ioid_of_thread_transition thread_transitions))
                  in
                  begin try List.hd sorted_thread_transitions with
                  | Failure _ -> raise (Step_thread_instruction tid)
                  end
               | (Some tid, Some iid) ->
                  (* step specific instruction *)
                  let transitions =
                    sorted_filtered_transitions node
                    |> List.map snd
                  in
                  if (List.exists
                        (MachineDefTypes.is_transition_of_ioid (tid, iid))
                        transitions) then
                    (tid, iid)
                  else
                    raise (Step_specific_instruction (tid, iid))
             in
                  (* slight HACK *)
                  let new_breakpoints = [(Runner.Numbered (-1), (Runner.StateBreakpoint (fun state -> if (MachineDefTransitionUtils.is_ioid_finished ioid state) then Runner.StateReason state else Runner.NoReason), "instruction finished"))] in
                  let new_filters = [MachineDefTypes.is_transition_of_ioid ioid] in
                  let (tid, iid) = ioid in
                  Screen.show_message interact_state.ppmode "stepping instruction (%d:%d)" tid iid;
                  do_search Interact_parser_base.Exhaustive interact_state new_breakpoints [] [] new_filters
           with
           | Step_thread_instruction tid ->
              Screen.show_warning interact_state.ppmode "thread %d has no available instructions to step" tid;
              interact_state
           | Step_any_instruction ->
              Screen.show_warning interact_state.ppmode "no available instructions to step";
              interact_state
           | Step_specific_instruction (tid, iid) ->
              Screen.show_warning interact_state.ppmode "no such instruction (%d:%d) to step" tid iid;
              interact_state
  end

let rec do_peek_instruction (thread_n : int) (inst_n : int)  interact_state : interact_state =
  begin match interact_state.interact_nodes with
        | [] -> Screen.show_warning interact_state.ppmode "no initial state"; interact_state
        | interact_node :: _ ->
           (* TODO FIXME: should we know about the internal structure of an ioid? *)
           let ioid = (thread_n, inst_n) in
           let new_targets = [MachineDefTransitionUtils.is_ioid_finished ioid] in
           let new_filters = [MachineDefTypes.is_transition_of_ioid ioid] in
           do_search Interact_parser_base.Exhaustive interact_state [] [] new_targets new_filters
  end

exception Invalid_option
exception Invalid_option_value of (* valid values = *) string
exception Invalid_option_state of (* explanation = *) string

let rec do_follow interact_state : interact_state =
  let m = interact_state.ppmode in
  let rec _do_follow interact_state =
    match interact_state.interact_nodes with
    | [] -> begin
        Screen.show_warning m "no initial state";
        interact_state
      end
    | node :: _ ->
       match interact_state.follow_suffix with
       | []     -> interact_state
       | Interact_parser_base.Transitions [n] :: _ ->
          if List.mem_assoc (Some n) node.filtered_transitions then
            do_transition n interact_state |> _do_follow
          else begin
              Screen.show_warning m "warning: current state does not have transition %d from follow list" n;
              interact_state
            end
       | ast :: _ ->
          do_cmds (ref false) [ast] interact_state |> _do_follow
  in
  match interact_state.follow_suffix with
  | [] -> begin
      Screen.show_warning m "warning: there was no follow-list to follow";
      interact_state
    end
  | _ :: _ -> _do_follow interact_state

and do_auto interact_state : interact_state =
  let m = interact_state.ppmode in
  let rec _do_auto interact_state =
    let interact_state = update_default_cmd interact_state in
    match interact_state.default_cmd with
    | None   -> interact_state
    | Some ast ->  do_cmds (ref false) [ast] interact_state |> _do_auto
  in
  let interact_state = update_default_cmd interact_state in
  match interact_state.default_cmd with
  | None -> begin
      Screen.show_warning m "warning: there were no enabled transitions to auto-take";
      interact_state
    end
  | Some _ -> _do_auto interact_state
and do_cmds
    (silenced : bool ref)
    (asts: Interact_parser_base.ast list)
    interact_state
    : interact_state
  =
  let ppmode = interact_state.ppmode in
  let append_to_history = append_to_history silenced in
  let do_cmds = do_cmds silenced in
  begin match asts with
  | [] -> interact_state

  | Interact_parser_base.Nothing :: asts ->
     do_cmds asts interact_state

  | Interact_parser_base.Silence :: asts ->
     silenced := true;
     do_cmds asts interact_state

  | Interact_parser_base.Quit :: _ ->
      exit 0

  | Interact_parser_base.Help None as ast :: asts ->
     Screen.show_message ppmode "%s" (Screen.escape Console_help.help_message);
     append_to_history ast interact_state |> do_cmds asts

  | Interact_parser_base.Help (Some cmd) as ast :: asts ->
     (* TODO: show help message for cmd *)
     Screen.show_message ppmode "TODO: show help for %s\nIn the meantime try just 'help'" cmd;
     append_to_history ast interact_state |> do_cmds asts

  | Interact_parser_base.ShowOptions as ast :: asts ->
     announce_options interact_state;
     append_to_history ast interact_state |> do_cmds asts

  | Interact_parser_base.Debug s as ast :: asts ->
     begin match s with
     | "transition_history" ->
        begin match interact_state.interact_nodes with
        | [] -> Screen.show_message ppmode "(no interact_nodes)"
        | nodes ->
           let n_places = List.length nodes |> float |> log10 |> truncate in
           List.iteri
             (fun i n -> Screen.show_message ppmode "%*d: [%s]" n_places i
                           (String.concat ", " (List.map string_of_int n.open_transition)))
             nodes
        end
     | "follow_list" ->
        begin match interact_state.follow_suffix with
        | [] -> Screen.show_message ppmode "(no follow list)"
        | fl ->
           let n_places = List.length fl |> float |> log10 |> truncate in
           List.iteri
             (fun i ast -> Screen.show_message ppmode "%*d: %s" n_places i
                             (Interact_parser_base.pp ast))
             fl
        end
     | _ -> Screen.show_warning ppmode "unknown debug command '%s'" s
     end;
     append_to_history ast interact_state |> do_cmds asts

  | Interact_parser_base.Transitions ns as ast :: asts ->
     List.fold_left (fun s n -> do_transition n s) (append_to_history ast interact_state) ns
     |> do_cmds asts

  | Interact_parser_base.Default :: asts ->
     (* currently the same as Step *)
     do_cmds ((Interact_parser_base.Step None) :: asts) interact_state

  | Interact_parser_base.Step None as ast :: asts ->
      begin match interact_state.default_cmd with
      | None   -> interact_state
                  |> append_to_history ast
      | Some ast -> do_cmds [ast] interact_state
      end
      |> do_cmds asts

  | Interact_parser_base.Step (Some i) :: asts when i > 0 ->
      let rec fold i interact_state =
        if i <= 0 then interact_state else
        begin match interact_state.default_cmd with
        | None   ->
            Screen.show_warning ppmode "can't step any more";
            interact_state
        | Some ast -> do_cmds [ast] interact_state
                      |> fold (i - 1)
        end
      in
      fold i interact_state
      |> do_cmds asts

  | Interact_parser_base.Step (Some i) as ast :: asts (* when i <= 0 *) ->
      Screen.show_warning ppmode "can't step a negative number (use back)";
      append_to_history ast interact_state
      |> do_cmds asts

  | Interact_parser_base.Undo as ast :: asts ->
      do_back 1 interact_state
      |> append_to_history ast
      |> do_cmds asts

  | Interact_parser_base.Back None as ast :: asts ->
      do_back 1 interact_state
      |> append_to_history ast
      |> do_cmds asts

  | Interact_parser_base.Back (Some i) as ast :: asts when i > 0 ->
      do_back i interact_state
      |> append_to_history ast
      |> do_cmds asts

  | Interact_parser_base.Back (Some i) as ast :: asts (* when i <= 0 *) ->
      Screen.show_warning ppmode "can't step back a negative number (use step)";
      append_to_history ast interact_state
      |> do_cmds asts

  | Interact_parser_base.Follow as ast :: asts ->
     do_follow interact_state
     |> append_to_history ast
     |> do_cmds asts

  | Interact_parser_base.Auto as ast :: asts ->
     do_auto interact_state
     |> append_to_history ast
     |> do_cmds asts

  | Interact_parser_base.Search (Interact_parser_base.Random i) as ast :: asts when i < 1 ->
      Screen.show_warning ppmode "the number of traces must be greater than 0";
      append_to_history ast interact_state
      |> do_cmds asts

  | Interact_parser_base.Search mode as ast :: asts ->
      do_search mode interact_state [] [] [] []
      |> append_to_history ast
      |> do_cmds asts

  | Interact_parser_base.Typeset as ast :: asts ->
     typeset interact_state "ui_snapshot.tex";
     append_to_history ast interact_state
     |> do_cmds asts

  | Interact_parser_base.Graph as ast :: asts ->
     make_graph interact_state;
     append_to_history ast interact_state
     |> do_cmds asts

  | Interact_parser_base.Print as ast :: asts ->
      print_last_state interact_state;
      append_to_history ast interact_state
      |> do_cmds asts

  | Interact_parser_base.History as ast :: asts ->
      print_history interact_state;
      append_to_history ast interact_state
      |> do_cmds asts

  | Interact_parser_base.FetchAll as ast :: asts ->
     do_fetch_all interact_state
     |> append_to_history ast
     |> do_cmds asts

  | Interact_parser_base.BreakpointFetch target as ast :: asts ->
      do_add_breakpoint_fetch target interact_state
      |> append_to_history ast
      |> do_cmds asts

  | Interact_parser_base.Watchpoint (typ, target) as ast :: asts ->
     do_add_watchpoint typ target interact_state
     |> append_to_history ast
     |> do_cmds asts

  | Interact_parser_base.SharedWatchpoint typ as ast :: asts ->
     do_add_shared_watchpoint typ interact_state
     |> append_to_history ast
     |> do_cmds asts

  | Interact_parser_base.BreakpointLine (filename, line) as ast :: asts ->
     do_add_breakpoint_line filename line interact_state
     |> append_to_history ast
     |> do_cmds asts

  | Interact_parser_base.SetOption (key, value) as ast :: asts ->
     let open Lens.Infix in
     let open RunOptions in
     let open Globals in
     let squote s = "'" ^ s ^ "'" in
     let squote_list l = String.concat ", " (List.map squote l) in
     let parse_bool s =
       let trues = ["on"; "true"; "1"; "t"; "yes"; "y"] in
       let falses = ["off"; "false"; "0"; "f"; "no"; "n"] in
       if List.mem s trues then true
       else if List.mem s falses then false
       else
         raise (Invalid_option_value (Printf.sprintf "%s = true; %s = false" (squote_list trues) (squote_list falses)))
     in
     let enum_parser values = fun s ->
       try List.assoc (String.lowercase s) values
       with Not_found -> raise (Invalid_option_value (squote_list (List.map fst values)))
     in
     let parse_verbosity = enum_parser [
                               ("quiet", Quiet);
                               ("normal", Normal);
                               ("verbose", ThrottledInformation);
                               ("very", UnthrottledInformation);
                               ("debug", Debug);
                             ] in
     let parse_ppstyle = enum_parser [
                             ("full", Ppstyle_full);
                             ("compact", Ppstyle_compact);
                             ("screenshot", Ppstyle_screenshot);
                           ] in
     let validated valids = fun s ->
       if (List.mem (String.lowercase s) valids) then s else raise (Invalid_option_value (squote_list valids))
     in
     let parse_int s =
       try int_of_string s with
       | Failure _ -> raise (Invalid_option_value "<N>")
     in
     let parse_option f s =
       if (String.lowercase s) = "none"
       then None
       else begin try
                Some (f s)
              with Invalid_option_value valids -> raise (Invalid_option_value ("(" ^ valids ^ ")|none"))
            end
     in
     let parse_int_option = parse_option parse_int in
     begin try
         begin match key with
         | "eager_up_to_shared" ->
            check_eager (((run_options_lens |-- eager_up_to_shared_lens) ^= (parse_bool value)) interact_state)

         | s when Utils.string_startswith "eager_" s -> begin
             let lens =
               match (Utils.string_drop (String.length "eager_") s) with
               | "fetch_single"        -> eager_fetch_single_lens
               | "fetch_multi"         -> eager_fetch_multi_lens
               | "pseudocode_internal" -> eager_pseudocode_internal_lens
               | "constant_reg_read"   -> eager_constant_reg_read_lens
               | "reg_rw"              -> eager_reg_rw_lens
               | "memory_aux"          -> eager_memory_aux_lens
               | "finish"              -> eager_finish_lens
               | "fp_recalc"           -> eager_fp_recalc_lens
               | "thread_start"        -> eager_thread_start_lens
               | _                     -> raise Invalid_option
             in
             add_interact_node
               ~follow_ast:ast
               EagerModeChangeNode
               (((run_options_lens |-- eager_mode_lens |-- lens) ^= (parse_bool value))
                  { interact_state with
                    follow_suffix = match interact_state.follow_suffix with
                                    | ast :: asts -> asts
                                    | _ -> [];
                  }
               )
               (List.hd interact_state.interact_nodes).system_state
             |> check_eager
           end

         | "eager" -> begin
             let b = parse_bool value in
             add_interact_node
               ~follow_ast:ast
               EagerModeChangeNode
               (((run_options_lens |-- eager_mode_lens) ^= (if b
                                                            then RunOptions.eager_mode_all_on
                                                            else RunOptions.eager_mode_all_off))
                  { interact_state with
                    follow_suffix = match interact_state.follow_suffix with
                                    | ast :: asts -> asts
                                    | _ -> [];
                  }
               )
               (List.hd interact_state.interact_nodes).system_state
             |> check_eager
           end

         | "suppress_internal"              -> ((run_options_lens |-- suppress_internal_lens)                 ^= (parse_bool value))       interact_state
         | "record_writes"                  -> ((run_options_lens |-- record_shared_locations_lens)           ^= (parse_bool value))       interact_state
         | "random"                         -> ((run_options_lens |-- pseudorandom_lens)                      ^= (parse_bool value))       interact_state
         | "storage_first"                  -> ((run_options_lens |-- storage_first_lens)                     ^= (parse_bool value))       interact_state
         | "always_print"                   -> ((run_options_lens |-- always_print_lens)                      ^= (parse_bool value))       interact_state
         | "check_inf_loop"                 -> ((run_options_lens |-- check_inf_loop_lens)                    ^= (parse_bool value))       interact_state
         | "compare_analyses"               -> ((run_options_lens |-- compare_analyses_lens)                  ^= (parse_bool value))       interact_state
         | "hash_prune"                     -> ((run_options_lens |-- hash_prune_lens)                        ^= (parse_bool value))       interact_state
         | "partial_order_reduction"        -> ((run_options_lens |-- partial_order_reduction_lens)           ^= (parse_bool value))       interact_state
         | "allow_partial"                  -> ((run_options_lens |-- allow_partial_lens)                     ^= (parse_bool value))       interact_state
         | "priority_reduction"             -> ((run_options_lens |-- priority_reduction_lens)                ^= (parse_bool value))       interact_state
         | "prune_restarts"                 -> ((run_options_lens |-- prune_restarts_lens)                    ^= (parse_bool value))       interact_state

         | "prune_discards"                 -> let b = parse_bool value in
                                               if b && (!Globals.model_params).t.thread_allow_tree_speculation then
                                                 raise (Invalid_option_state "requires forbid_tree_speculation")
                                               else
                                                 ((run_options_lens |-- prune_discards_lens)                  ^= b)                        interact_state

         | "transition_limit"               -> ((run_options_lens |-- transition_limit_lens)                  ^= (parse_int_option value)) interact_state
         | "trace_limit"                    -> ((run_options_lens |-- trace_limit_lens)                       ^= (parse_int_option value)) interact_state
         | "time_limit"                     -> ((run_options_lens |-- time_limit_lens)                        ^= (parse_int_option value)) interact_state

         | "suppress_newpage"               -> ((ppmode_lens      |-- pp_suppress_newpage_lens)               ^= (parse_bool value))       interact_state
         | "buffer_messages"                -> ((ppmode_lens      |-- pp_buffer_messages_lens)                ^= (parse_bool value))       interact_state
         | "announce_options"               -> ((ppmode_lens      |-- pp_announce_options_lens)               ^= (parse_bool value))       interact_state
         | "ppg_shared"                     -> ((ppmode_lens      |-- ppg_shared_lens)                        ^= (parse_bool value))       interact_state
         | "ppg_rf"                         -> ((ppmode_lens      |-- ppg_rf_lens)                            ^= (parse_bool value))       interact_state
         | "ppg_fr"                         -> ((ppmode_lens      |-- ppg_fr_lens)                            ^= (parse_bool value))       interact_state
         | "ppg_co"                         -> ((ppmode_lens      |-- ppg_co_lens)                            ^= (parse_bool value))       interact_state
         | "ppg_addr"                       -> ((ppmode_lens      |-- ppg_addr_lens)                          ^= (parse_bool value))       interact_state
         | "ppg_data"                       -> ((ppmode_lens      |-- ppg_data_lens)                          ^= (parse_bool value))       interact_state
         | "ppg_ctrl"                       -> ((ppmode_lens      |-- ppg_ctrl_lens)                          ^= (parse_bool value))       interact_state
         | "ppg_regs"                       -> ((ppmode_lens      |-- ppg_regs_lens)                          ^= (parse_bool value))       interact_state
         | "ppg_reg_rf"                     -> ((ppmode_lens      |-- ppg_reg_rf_lens)                        ^= (parse_bool value))       interact_state
         | "ppg_trans"                      -> ((ppmode_lens      |-- ppg_trans_lens)                         ^= (parse_bool value))       interact_state
         | "prefer_symbolic_values"         -> ((ppmode_lens      |-- pp_prefer_symbolic_values_lens)         ^= (parse_bool value))       interact_state
         | "hide_pseudoregister_reads"      -> ((ppmode_lens      |-- pp_hide_pseudoregister_reads_lens)      ^= (parse_bool value))       interact_state
         | "max_finished"                   -> ((ppmode_lens      |-- pp_max_finished_lens)                   ^= (parse_int_option value)) interact_state
         | "pp_style"                       -> ((ppmode_lens      |-- pp_style_lens)                          ^= (parse_ppstyle value))    interact_state
         | "choice_history_limit"           -> ((ppmode_lens      |-- pp_choice_history_limit_lens)           ^= (parse_int_option value)) interact_state
         | "condense_finished_instructions" -> ((ppmode_lens      |-- pp_condense_finished_instructions_lens) ^= (parse_bool value))       interact_state
         | "pp_sail"                        -> ((ppmode_lens      |-- pp_sail_lens)                           ^= (parse_bool value))       interact_state

         | "verbosity"                         -> begin Globals.verbosity                         := (parse_verbosity value); interact_state end
         | "pp_hex"                            -> begin Globals.print_hex                         := (parse_bool value);      interact_state end
         | "dwarf_show_all_variable_locations" -> begin Globals.dwarf_show_all_variable_locations := (parse_bool value);      interact_state end
         | "graph_backend"                     -> begin Globals.set_graph_backend (validated ["dot"; "tikz"] value);          interact_state end
         | "random_seed"                       -> begin Random.init (parse_int value);                                        interact_state end
         | "dumb_terminal"                     -> begin Globals.dumb_terminal                     := (parse_bool value);      interact_state end

         | "loop_limit"                        -> let new_model =
                                                    { !Globals.model_params with
                                                      MachineDefTypes.t =
                                                        { (!Globals.model_params).MachineDefTypes.t with
                                                          MachineDefTypes.thread_loop_unroll_limit =
                                                            (parse_int_option value)
                                                        }
                                                    }
                                                  in begin
                                                      change_model new_model interact_state
                                                    end

         (* always_graph and dot_final_ok are mutually exclusive
            because always_graph would overwrite the final graph  *)

         | "always_graph" ->
            begin
              if parse_bool value then
                Globals.run_dot := Some RD_step
              else
                Globals.run_dot := None;
              interact_state
            end

         | "dot_final_ok" ->
            begin
              if parse_bool value then
                Globals.run_dot := Some RD_final_ok
              else
                Globals.run_dot := None;
              interact_state
            end

         | "dot_final_not_ok" ->
            begin
              if parse_bool value then
                Globals.run_dot := Some RD_final_not_ok
              else
                Globals.run_dot := None;
              interact_state
            end

         | "follow_list" ->
            let follow_suffix =
              List.map (fun s ->
                  try Interact_parser_base.Transitions [int_of_string s] with
                  | Failure _ -> raise (Invalid_option_value "comma separated ints")
                ) (Pp.split "," value)
            in
            update_default_cmd
              { interact_state with follow_suffix = follow_suffix }

         | "pp_colours" -> begin
             let b = parse_bool value in
             Printing_functions.set_color_enabled b; (* propagate to sail interpreter pp *)
             ((ppmode_lens |-- pp_colours_lens) ^= b) interact_state
           end

         | _ -> raise Invalid_option
         end
       with
       | Invalid_option ->
          Screen.show_warning interact_state.ppmode "unknown option '%s' (try 'help' command)" (Screen.escape key);
          interact_state
       | Invalid_option_value valids ->
          Screen.show_warning interact_state.ppmode "invalid value '%s' for option '%s' (valid values: %s)"
                              (Screen.escape value) (Screen.escape key) (Screen.escape valids);
          interact_state
       | Invalid_option_state reason ->
          Screen.show_warning interact_state.ppmode "error setting option '%s' to '%s': %s"
                              (Screen.escape key) (Screen.escape value) (Screen.escape reason);
          interact_state
     end
     |> append_to_history ast
     |> do_cmds asts

  | Interact_parser_base.FocusThread maybe_thread as ast :: asts ->
     let new_state = { interact_state with
                       options = { interact_state.options with
                                   focused_thread = maybe_thread
                                 }
                     } in
     let new_state = { new_state with
                       interact_nodes = refilter_nodes new_state new_state.interact_nodes
                     } in
     update_default_cmd new_state
     |> append_to_history ast
     |> do_cmds asts

  | Interact_parser_base.FocusInstruction maybe_ioid as ast :: asts ->
     let new_state = { interact_state with
                       options = { interact_state.options with
                                   focused_ioid = maybe_ioid
                                 }
                     } in
     let new_state = { new_state with
                       interact_nodes = refilter_nodes new_state new_state.interact_nodes
                     } in
     update_default_cmd new_state
     |> append_to_history ast
     |> do_cmds asts


  | Interact_parser_base.StepInstruction (thread_n, inst_n) as ast :: asts ->
     do_step_instruction thread_n inst_n interact_state
     |> append_to_history ast
     |> do_cmds asts

  | Interact_parser_base.PeekInstruction (thread_n, inst_n) as ast :: asts ->
     do_peek_instruction thread_n inst_n interact_state
     |> append_to_history ast
     |> do_cmds asts

  | Interact_parser_base.InfoBreakpoints as ast :: asts ->
     do_info_breakpoints interact_state
     |> append_to_history ast
     |> do_cmds asts

  | Interact_parser_base.DeleteBreakpoint n as ast :: asts ->
     do_delete_breakpoint n interact_state
     |> append_to_history ast
     |> do_cmds asts
  end

let make_prompt interact_state : string =
  begin match interact_state.interact_nodes with
  | [] -> ""
  | node :: _ ->
      let str =
        Printf.sprintf "Step %d (%d/%d finished, %d trns)"
          (List.length interact_state.interact_nodes)
          (MachineDefSystem.count_instruction_instances_finished node.system_state.sst_state)
          (MachineDefSystem.count_instruction_instances_constructed node.system_state.sst_state)
          (List.fold_left (+) 0 (List.map (fun n -> List.length n.open_transition) interact_state.interact_nodes))
      in
      let option_strs = List.fold_right (@)
        [
          (if interact_state.options.pseudorandom then ["random"] else []);
          (if interact_state.options.storage_first then ["storage-first"] else []);
          (match interact_state.options.focused_thread with
           | Some i -> [Printf.sprintf "focused on thread %d" i]
           | None -> []);
          (match interact_state.options.focused_ioid with
           | Some (tid, iid) -> [Printf.sprintf "focused on ioid (%d:%d)" tid iid]
           | None -> []);
        ] []
      in
      let option_str =
        (match option_strs with
         | [] -> ""
         | _  -> Printf.sprintf " (%s)" (String.concat ", " option_strs))
      in
      begin match interact_state.default_cmd with
      | None   -> Printf.sprintf "%s%s" str option_str
      | Some ast -> Printf.sprintf "%s Choose [%s]%s" str (Interact_parser_base.pp ast) option_str
      end
  end

let extract_options interact_state : Screen_base.options_state =
  let open Globals in {
      run_options = interact_state.options;
      model_params = !Globals.model_params;
      ppmode = interact_state.ppmode;
      always_graph = (!Globals.run_dot = Some RD_step);
      dot_final_ok = (!Globals.run_dot = Some RD_final_ok);
      dot_final_not_ok = (!Globals.run_dot = Some RD_final_not_ok);
      pp_hex = !Globals.print_hex;
      dwarf_show_all_variable_locations = !Globals.dwarf_show_all_variable_locations;
      verbosity = !Globals.verbosity;
  }

let rec main_loop interact_state : unit =
  (if interact_state.options.always_print then
    print_last_state interact_state
  else
    ());
  (if !Globals.run_dot = Some Globals.RD_step then
     make_graph interact_state
   else
     ());
  Screen.prompt interact_state.ppmode
                (Some (extract_options interact_state))
                (make_prompt interact_state)
                interact_state.cmd_history
                (parse_and_loop interact_state)
and parse_and_loop interact_state = fun cmd ->
    match parse cmd with
    | ParserError s ->
        Screen.show_warning interact_state.ppmode "%s" s;
        main_loop interact_state
    | ParserASTs input_asts ->
        do_cmds (ref false) input_asts interact_state |> main_loop


let initial_interact_state
    (options:       RunOptions.t)
    (ppmode:        Globals.ppmode)
    (test_info:     Test.info)
    (initial_state: ConcModel.system_state)
    : interact_state
  =
  let interact_state =
    { test_info      = test_info;
      options        = options;
      ppmode         = ppmode;
      interact_nodes = [];
      cmd_history    = [];
      follow_suffix  = !Globals.follow;
      default_cmd = None;
      breakpoints    = [];
      read_locations    = Pmap.empty compare;
      written_locations = Pmap.empty compare;
      shared_locations  = [];
      shared_program_locations = Pset.empty compare;
    }
  in
  ConcModel.sst_of_state options initial_state
  |> add_interact_node InitialStateNode interact_state
  |> check_eager


let run_interactive
    (options:        RunOptions.t)
    (ppmode:         Globals.ppmode)
    (test_info:      Test.info)
    (state_records:  MachineDefSystem.initial_state_record list)
    : unit
  =
  let interact_state =
    begin match state_records with
    | []      -> failwith "no initial state"
    | s :: [] -> s
    | s :: _  ->
        Screen.show_warning ppmode "given multiple initial states, using only the first one";
        s
    end
    |> ConcModel.initial_system_state test_info.Test.ism options
    |> initial_interact_state options ppmode test_info
  in

  begin match !Globals.ui_commands with
  | None   -> main_loop interact_state
  | Some s ->
     match parse s with
     | ParserError err -> failwith (Printf.sprintf "parse error in -cmds: %s" err)
     | _ -> parse_and_loop interact_state s
  end


(** handle the '-interactive false' exhaustive search ***************)

let print_observations interact_state interact_node search_state =
  let model = interact_node.system_state.sst_state.model in

  let branch_targets_output =
    otIfTrue (not (Pmap.is_empty model.t.branch_targets)) @@
      otVerbose Globals.Normal @@
        otStrLine "Branch-targets=%s"
          (MachineDefSystem.branch_targets_to_list model.t.branch_targets
          |> Runner.sprint_branch_targets interact_state.ppmode)
  in

  let shared_memory_output =
    otIfTrue (not (Pset.is_empty model.shared_memory)) @@
      otVerbose Globals.Normal @@
        otStrLine "Shared-memory=%s"
          (Runner.sprint_shared_memory interact_state.ppmode model.shared_memory)
  in

  let states_output = print_observed_finals interact_state interact_node search_state.Runner.observed_filterred_finals in
  let deadlock_states_output = print_observed_deadlocks interact_state interact_node search_state.Runner.observed_deadlocks in
  let exceptions_output = print_observed_exceptions interact_state interact_node search_state.Runner.observed_exceptions in

  OTConcat
    [ branch_targets_output;
      shared_memory_output;
      states_output;
      deadlock_states_output;
      exceptions_output;
    ]

let print_search_results interact_state interact_node search_state runtime : unit =
  if Globals.is_verbosity_at_least Globals.ThrottledInformation then
    Runner.print_search_counters true search_state;

  let test_name_output =
    otLine @@ otStrEmph "Test %s %s"
        interact_state.test_info.Test.name
        (ConstrGen.pp_kind (ConstrGen.kind_of interact_state.test_info.Test.constr))
  in

  let (constraint_holds, constraint_output) =
    let holds =
      Runner.StateMap.bindings search_state.Runner.observed_filterred_finals
      |> List.split
      |> fst
      |> Test.C.check_constr interact_state.test_info.Test.constr
    in
    if holds then (true, otStrLine "Ok")
    else if ConstrGen.is_existential interact_state.test_info.Test.constr then
      (false, OTConcat [otVerbose Globals.ThrottledInformation @@ otWarning @@ otString "%s: Existential constraint not satisfied!" interact_state.test_info.Test.name;
                otStrLine "No (allowed not found)"])
    else (* universal failed *)
      (false, OTConcat [otVerbose Globals.ThrottledInformation @@ otWarning @@ otString "%s: Universal constraint invalidated!" interact_state.test_info.Test.name;
                otStrLine "No (forbidden found)"])
  in

  let symtab =
    List.map
      (fun ((addr, _), s) -> (Test.C.interp_address_to_address addr, s))
      interact_state.ppmode.Globals.pp_symbol_table
  in

  let condition_output =
    otStrLine "Condition %s"
      (ConstrGen.constraints_to_string (Test.C.pp_atom symtab) interact_state.test_info.Test.constr)
  in

  let test_info_output =
    if !Globals.dont_tool then
    begin
      OTConcat
        (List.map
          (fun (k, v) ->
            if Misc.string_eq k "Relax" then
              otStrLine "Relax %s %s %s" interact_state.test_info.Test.name (if constraint_holds then "Ok" else "No") v
            else
              otStrLine "%s=%s" k v)
          interact_state.test_info.Test.info)
    end
    else
    begin
      OTConcat
        (Lem_list.mapMaybe
          (fun (k, v) ->
            if Misc.string_eq k "Hash" then
              Some (otStrLine "%s=%s" k v)
            else None)
          interact_state.test_info.Test.info)
    end
  in

  let observation_output =
    let (matches, non_matches) =
      let prop =
        match interact_state.test_info.Test.constr with
        | ConstrGen.ForallStates p
        | ConstrGen.ExistsState p
        | ConstrGen.NotExistsState p -> p
      in
      Runner.StateMap.bindings search_state.Runner.observed_filterred_finals |> List.split |> fst |> List.partition (Test.C.check_prop prop)
    in

    otStrLine "Observation %s %s %d %d%s%s"
      interact_state.test_info.Test.name
      (if matches = [] then "Never"
      else if non_matches = [] then "Always"
      else "Sometimes")
      (List.length matches)
      (List.length non_matches)
      (if search_state.Runner.observed_deadlocks <> None then " with deadlocks" else "")
      (if not (Runner.ExceptionMap.is_empty search_state.Runner.observed_exceptions) then " with unhandled exceptions" else "")
  in

  let runtime_output = otStrLine "Runtime: %f sec" runtime in

  OTConcat
    [ test_name_output;
      print_observations interact_state interact_node search_state;
      constraint_output;
      condition_output;
      test_info_output;
      observation_output;
      runtime_output;
    ]
  |> Screen.of_output_tree |> Screen.final_message interact_state.ppmode false


let run_exhaustive_search
    (options:        RunOptions.t)
    (ppmode:         Globals.ppmode)
    (test_info:      Test.info)
    (state_records:  MachineDefSystem.initial_state_record list)
    : unit
  =
  (* breakpoint predicates and handlers *)
  let breakpoints =
    let run_dot_final_ok_pred = fun sst ->
      if sst.sst_system_transitions = [] &&
        begin match test_info.Test.filter with
        | None -> true
        | Some filter ->
            Runner.reduced_final_state test_info.Test.filter_regs test_info.Test.filter_mem sst.sst_state
            |> Test.C.check_filter filter
        end &&
        let final_state = Runner.reduced_final_state test_info.Test.show_regs test_info.Test.show_mem sst.sst_state in
        Test.C.check_constr test_info.Test.constr [final_state]
      then Runner.StateReason sst
      else Runner.NoReason
    in

    let run_dot_final_not_ok_pred = fun sst ->
      if sst.sst_system_transitions = [] &&
        begin match test_info.Test.filter with
        | None -> true
        | Some filter ->
            Runner.reduced_final_state test_info.Test.filter_regs test_info.Test.filter_mem sst.sst_state
            |> Test.C.check_filter filter
        end &&
        let final_state = Runner.reduced_final_state test_info.Test.show_regs test_info.Test.show_mem sst.sst_state in
        not (Test.C.check_constr test_info.Test.constr [final_state])
      then Runner.StateReason sst
      else Runner.NoReason
    in

    let run_dot_final = fun sst ->
      match ppmode.Globals.pp_kind with
      | Globals.Html ->
         Screen.display_dot ppmode
                            (Some test_info.Test.name)
                            sst.sst_state
                            (ConcModel.make_cex_candidate sst.sst_state)
                            []
      | _ ->
         if !Globals.graph_backend = Globals.Tikz then begin
          let symtab =
            List.map
              (fun ((a,sz),s) ->
                (Nat_big_num.to_int64 (Sail_impl_base.integer_of_address a), s))
              ppmode.Globals.pp_symbol_table
          in
          let final_state = Runner.reduced_final_state test_info.Test.show_regs test_info.Test.show_mem sst.sst_state in
          Tikz.make_final_state test_info (Test.C.pp_state symtab final_state)
         end;

         let module G = (val get_graph_backend ()) in
         G.make_graph ppmode test_info sst.sst_state
                       (ConcModel.make_cex_candidate sst.sst_state) []
    in

    let run_dot_final_exception = fun sst ->
      let is_exception = function
        | T_trans (T_only {tl_label = (T_exception _)}) -> true
        | _ -> false
      in
      match List.find is_exception sst.sst_system_transitions with
      | T_trans (T_only {tl_label = (T_exception e); tl_cont = tc}) ->
          Screen.show_warning ppmode "Unhandled exception: %s" (Screen.escape (Pp.pp_exception ppmode tc.tc_ioid e));
          begin match ppmode.Globals.pp_kind with
          | Globals.Html ->
             Screen.display_dot ppmode
                                (Some test_info.Test.name)
                                sst.sst_state
                                (ConcModel.make_cex_candidate sst.sst_state)
                                []
          | _ ->
             if !Globals.graph_backend = Globals.Tikz then begin
              Tikz.make_final_state test_info (Pp.pp_exception ppmode tc.tc_ioid e)
             end;

             let module G = (val get_graph_backend ()) in
             G.make_graph ppmode test_info sst.sst_state
                           (ConcModel.make_cex_candidate sst.sst_state) []
          end
      | _ -> assert false
      | exception Not_found -> assert false
    in

    match !Globals.run_dot with
    (* FIXME: handle RD_final *)
    | Some Globals.RD_final_ok ->
        [ (run_dot_final_ok_pred, run_dot_final);
          (wrap_state_pred MachineDefTransitionUtils.enabled_exception_transition, run_dot_final_exception);
        ]
    | Some Globals.RD_final_not_ok ->
      [ (run_dot_final_not_ok_pred, run_dot_final);
        (wrap_state_pred MachineDefTransitionUtils.enabled_exception_transition, run_dot_final_exception);
      ]
    | _ -> []
  in

  let print_diffs search_state bt_union bt_diff sm_union sm_diff : unit =
    if Globals.is_verbosity_at_least Globals.ThrottledInformation then begin
      if not (Pmap.is_empty bt_union) then begin
        Screen.show_message ppmode
          "Branch-register targets that were observed:\n%s"
          (MachineDefSystem.branch_targets_to_list bt_union |> Runner.sprint_branch_targets ppmode);

        if bt_diff <> [] then
          Screen.show_message ppmode
            "(from which the following are new: %s)"
            (Runner.sprint_branch_targets ppmode bt_diff);
      end;

      if options.eager_mode.eager_local_mem then begin
        if Pset.is_empty sm_union then
          Screen.show_message ppmode "No shared memory footprints were observed!"
        else begin
          Screen.show_message ppmode
            "Shared memory footprints that were observed:\n%s"
            (Runner.sprint_shared_memory ppmode sm_union);

          if not (Pset.is_empty sm_diff) then
            Screen.show_message ppmode
              "(from which the following are new: %s)"
              (Runner.sprint_shared_memory ppmode sm_diff);
        end
      end;
    end;
  in

  let run_search print_partial_results options sst : Runner.search_outcome =
    Runner.search_from_state
        ~ppmode:ppmode
        options
        test_info
        sst
        (* HACK *)
        (List.map
            (fun (pred, _) ->
              (Runner.Numbered (-1), (Runner.StateBreakpoint pred, "internal search breakpoint...?")))
            breakpoints)
        [] (* bounds *)
        [] (* targets *)
        [] (* filters *)
        print_partial_results
  in

  let rec search_fixed_point n system_state : unit =
    if Globals.is_verbosity_at_least Globals.ThrottledInformation then
      Screen.show_message ppmode
        "%s Starting exhaustive search (%d)." (Runner.sprint_time ()) n;

    let interact_state = initial_interact_state options ppmode test_info system_state in

    let print_results partial = fun search_state ->
      if partial then
        Screen.show_warning ppmode "%s" ( "***********************\n" ^
                                          "*** PARTIAL RESULTS ***\n" ^
                                          "***********************");
      print_search_results
        interact_state
        (List.hd interact_state.interact_nodes)
        search_state
        (Sys.time ())
    in

    let sst = (List.hd interact_state.interact_nodes).system_state in
    match run_search (print_results true) options sst with
    | Runner.Complete search_state' ->
        let (bt_union, bt_diff) =
          MachineDefSystem.union_and_diff_branch_targets
            search_state'.Runner.observed_branch_targets
            sst.sst_state.model.t.branch_targets
        in

        let (sm_union, sm_diff) =
          MachineDefSystem.union_and_diff_shared_memory
            search_state'.Runner.observed_shared_memory
            sst.sst_state.model.shared_memory
        in

        if bt_diff = [] && (not options.eager_mode.eager_local_mem || Pset.is_empty sm_diff) then
          (* search terminated after exploring all reachable states *)
          print_results false search_state'

        else begin
          if Globals.is_verbosity_at_least Globals.ThrottledInformation then begin
            Screen.show_message ppmode
              "%s An exhaustive search (%d) was finished but a fixed point was not reached."
              (Runner.sprint_time ())
              n;
              print_diffs search_state' bt_union bt_diff sm_union sm_diff;
              if options.allow_partial then print_results true search_state';
          end;

          let model' =
            { sst.sst_state.model with
              t = {sst.sst_state.model.t with branch_targets = bt_union};
              shared_memory = sm_union;
            }
          in

          let initial_node = List.rev interact_state.interact_nodes |> List.hd in
          {initial_node.system_state.sst_state with model = model'}
          |> search_fixed_point (n + 1)
        end

    | Runner.Breakpoint ({Runner.search_nodes = {Runner.system_state = sst} :: _}, bp, reason) ->
        (* a breakpoint was triggered *)

        (* find the specific breakpoint that was triggered and run its handler *)
        List.iter
          (fun (predicate, handler) -> if (predicate sst) <> Runner.NoReason then handler sst)
          breakpoints

    | Runner.Breakpoint _ (* search_nodes = [] *) -> assert false

    | Runner.Interrupted (search_state, reason) ->
        Screen.show_warning ppmode "Interrupted because %s" reason;
        print_results true search_state

    | Runner.OcamlExn (search_state, msg) ->
        Screen.show_warning ppmode "%s" msg;
        print_results true search_state;
        Screen.show_warning ppmode "*** Error ***\n";
        exit 1
  in

  let rec search_random options system_state : unit =
    if Globals.is_verbosity_at_least Globals.ThrottledInformation then
      Screen.show_message ppmode "%s Starting random search (%d traces)."
        (Runner.sprint_time ())
        options.pseudorandom_traces;

    let interact_state = initial_interact_state options ppmode test_info system_state in

    let print_results = fun search_state ->
      OTConcat [
        otStrLine "---------------------------------------------";
        print_observations interact_state (List.hd interact_state.interact_nodes) search_state;
        otStrLine "---------------------------------------------";
      ]
      |> Screen.of_output_tree
      |> Screen.show_message ppmode "%s"
    in

    let sst = (List.hd interact_state.interact_nodes).system_state in
    match run_search (fun _ -> ()) options sst with
    | Runner.Complete search_state' ->
        let (bt_union, bt_diff) =
          MachineDefSystem.union_and_diff_branch_targets
            search_state'.Runner.observed_branch_targets
            sst.sst_state.model.t.branch_targets
        in

        let (sm_union, sm_diff) =
          MachineDefSystem.union_and_diff_shared_memory
            search_state'.Runner.observed_shared_memory
            sst.sst_state.model.shared_memory
        in

        if bt_diff <> [] || (options.eager_mode.eager_local_mem && not (Pset.is_empty sm_diff)) then
          print_diffs search_state' bt_union bt_diff sm_union sm_diff;

        print_results search_state'

    | Runner.Breakpoint ({Runner.search_nodes = {Runner.system_state = sst} :: _}, bp, reason) ->
        (* a breakpoint was triggered *)

        (* find the specific breakpoint that was triggered and run its handler *)
        List.iter
          (fun (predicate, handler) -> if (predicate sst) <> Runner.NoReason then handler sst)
          breakpoints

    | Runner.Breakpoint _ (* search_nodes = [] *) -> assert false

    | Runner.Interrupted (search_state, reason) ->
        Screen.show_warning ppmode "Interrupted because %s" reason;
        print_results search_state

    | Runner.OcamlExn (search_state, msg) ->
        Screen.show_warning ppmode "%s" msg;
        Screen.show_warning ppmode "%s" ( "***********************\n" ^
                                          "*** PARTIAL RESULTS ***\n" ^
                                          "***********************");
        print_results search_state;
        Screen.show_warning ppmode "*** Error ***\n";
        exit 1
  in

  let initial_states =
    List.map
      (ConcModel.initial_system_state test_info.Test.ism options)
      state_records
  in

  (* run search for each initial state; we use for-loop because js_of_ocaml
  does not handle tail call so well *)
  for n = 0 to List.length initial_states - 1 do
    let initial_state = List.nth initial_states n in
    if options.pseudorandom then
      search_random options initial_state
    else
      search_fixed_point 0 initial_state
  done

end (* Make *)
