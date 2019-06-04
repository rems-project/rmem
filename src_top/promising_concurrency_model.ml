(*===================================================================================*)
(*                                                                                   *)
(*                rmem executable model                                              *)
(*                =====================                                              *)
(*                                                                                   *)
(*  This file is:                                                                    *)
(*                                                                                   *)
(*  Copyright Christopher Pulte, University of Cambridge                      2019   *)
(*                                                                                   *)
(*  All rights reserved.                                                             *)
(*                                                                                   *)
(*  It is part of the rmem tool, distributed under the 2-clause BSD licence in       *)
(*  LICENCE.txt.                                                                     *)
(*                                                                                   *)
(*===================================================================================*)


module Make (ISAModel: Isa_model.S) : Concurrency_model.S = struct

  type thread_subsystem_state = PromisingViews.t0 PromisingThread.pts
  type storage_subsystem_state = PromisingStorage.pss
  type system_state = (thread_subsystem_state,storage_subsystem_state,PromisingViews.t0) Promising.p_state
  type ui_state = (PromisingUI.pts_ui_state, PromisingUI.pss_ui_state, PromisingViews.t0) PromisingUI.p_ui_state 

  (* For efficiency, 'state' also includes all the enabled transitions *)
  type state = (thread_subsystem_state,storage_subsystem_state,PromisingViews.t0) Promising.pst
  type trans = (thread_subsystem_state,storage_subsystem_state,PromisingViews.t0) PromisingTransitions.p_trans

  type ui_trans = int * trans

  let final_ss_state _s = true

  let sst_of_state = Promising.type_specialised_p_pst_of_state

  let initial_state ism run_options initial_state_record =
    Promising.p_initial_state
      (ISAModel.instruction_semantics ism run_options)
      ISAModel.ISADefs.reg_data
      initial_state_record
    |> Promising.type_specialised_p_pst_of_state

  let update_dwarf (state: system_state) (ppmode: Globals.ppmode) : Globals.ppmode =
    let pp_dwarf_dynamic =
      { Types.get_evaluation_context = (fun ioid ->
            PromisingDwarf.p_get_dwarf_evaluation_context
              (Globals.get_endianness ()) (*TODO: Shaked, is that right?*)
              state
              (fst ioid)  (* relies on internal construction of fresh ioids*)
              ioid);

        Types.pp_all_location_data_at_instruction = (fun tid ioid ->
            (* turning this off for now - should be switchable *)
            match ppmode.Globals.pp_dwarf_static with
            | Some ds ->
                begin match PromisingDwarf.p_get_dwarf_evaluation_context
                    (Globals.get_endianness ()) (*TODO: Shaked, is that right?*)
                    state
                    tid
                    ioid
                with
                | Some (address, ev0) ->
                    let ev = Pp.wrap_ev ev0 in
                    let alspc = Dwarf.analysed_locations_at_pc ev ds address in
                    (*"analysed_location_data_at_pc:\n"
                    ^*) Dwarf.pp_analysed_location_data_at_pc ds.Dwarf.ds_dwarf alspc
                | None -> "<failure: get_dwarf_evaluation_context>"
                end
            | None -> "<no dwarf_static info available>");
      }
    in
    {ppmode with Globals.pp_dwarf_dynamic = Some pp_dwarf_dynamic}

  let make_ui_state ppmode prev_state state ncands =
    let ppmode' = update_dwarf state.Promising.pst_state ppmode in
    let ui_state =
      match (prev_state,ppmode.Globals.pp_kind) with
      | (None,_)
      | (_,Globals.Hash) ->
          PromisingUI.make_p_ui_state None state.Promising.pst_state ncands
      | (Some prev_state,_) ->
          PromisingUI.make_p_ui_state
            (Some prev_state.Promising.pst_state)
            state.Promising.pst_state ncands
    in
    (ppmode', ui_state)

  let state_after_trans = Promising.type_specialised_pst_after_transition

  let is_final_state s = 
    s.Promising.pst_trans = [] && 
    Promising.type_specialised_p_is_final_state s.Promising.pst_state

  let branch_targets_of_state = Promising.p_branch_targets_of_state
  let shared_memory_of_state = Promising.p_shared_memory_of_state
  let memory_value_of_footprints s fps = 
    PromisingStorage.pss_memory_value_of_footprints 
      s.Promising.pst_state.Promising.p_storage_state fps
  let make_cex_candidate s = Promising.p_make_cex_candidate s.Promising.pst_state

  let is_eager_trans _s _eager_mode _trans = false

  let is_loop_limit_trans _ = false

  let priority_trans s eager_mode = 
    let open Promising in
    p_priority_transitions s.pst_state eager_mode

  let is_thread_trans = PromisingTransitions.p_is_thread_transition

  let fuzzy_compare_transitions = Model_ptransition_aux.fuzzy_compare_p_trans

  let is_fetch_single_trans = (fun _ -> false)
  let is_fetch_multi_trans = (fun _ -> false)

  let trans_fetch_addr = PromisingTransitions.p_trans_fetch_address
  let trans_reads_fp fp _s t = PromisingTransitions.ptrans_reads_footprint fp t
  let trans_writes_fp fp _s t = PromisingTransitions.ptrans_writes_footprint fp t

  let pretty_eiids s = Pp.p_pretty_eiids s.Promising.pst_state

  let number_finished_instructions s =
    Promising.p_number_of_instructions s.Promising.pst_state
  let number_constructed_instructions s =
    Promising.p_number_of_instructions s.Promising.pst_state

  let principal_ioid_of_trans = PromisingTransitions.ioid_of_p_trans
  let is_ioid_finished i s =
    Promising.p_is_ioid_finished i s.Promising.pst_state
  let threadid_of_thread_trans = PromisingTransitions.tid_of_p_trans
  let ioid_of_thread_trans = PromisingTransitions.ioid_of_p_trans
  let is_storage_trans = PromisingTransitions.p_is_storage_transition
  let pp_transition_history = fun _ -> failwith "pp_transition_history"
  let pp_cand = Pp.pp_pcand
  let pp_trans = Pp.pp_p_trans
  let pp_ui_state = Pp.pp_ui_pstate
  let pp_instruction = Pp.pp_p_ui_instruction
  let set_model_params model s =
    let open Promising in
    type_specialised_p_pst_of_state {s.pst_state with p_model = model}
  let model_params s = s.Promising.pst_state.Promising.p_model
  let final_reg_states s = 
    let open Promising in
    Pmap.bindings_list s.pst_state.p_thread_states
    |> List.map (fun (tid, state) -> (tid, PromisingThread.p_registers_final_state state))
  let transitions s = s.Promising.pst_trans
  let stopped_promising s =
    let open Promising in
    s.pst_state.p_stopped_promising
  let write_after_stop_promising s = false
  let inst_discarded s = false
  let inst_restarted s = false

  let interesting_state p s =
    match MoreConstraints.Make.check_prop_incomplete_state p s with
    | Some true -> true
    | Some false -> false
    | None -> true

  let transition_filter test_info p s t =
    match t with
    | PromisingTransitions.PSys_stop_promising ->
       let fps = test_info.Test.show_mem in
       let memory_values = 
         PromisingStorage.pss_memory_value_of_footprints 
           s.Promising.pst_state.Promising.p_storage_state fps in
       let mem = Test.reduced_final_mem_state fps memory_values in
       let state = (None, Some mem) in
       interesting_state p state
    | _ -> true


  type predicate = state -> trans -> bool
  let make_postcondition_filters (test_info: Test.info) : predicate list =
    match test_info.Test.constr with
    | ForallStates p ->   [transition_filter test_info p]
    | ExistsState p ->    [transition_filter test_info p]
    | NotExistsState p -> [transition_filter test_info p]

  let filters (options: RunOptions.t) (test_info: Test.info) : predicate list =
    if options.RunOptions.postcondition_filter
    then make_postcondition_filters test_info
    else []

end

(********************************************************************)

