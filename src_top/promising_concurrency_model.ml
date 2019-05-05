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

  type thread_subsystem_state = PromisingThread.pts
  type storage_subsystem_state = PromisingStorage.pss
  type system_state = (thread_subsystem_state,storage_subsystem_state) Promising.pstate
  type ui_state = (PromisingUI.pts_ui_state, PromisingUI.pss_ui_state) PromisingUI.p_ui_state 

  (* For efficiency, 'state' also includes all the enabled transitions *)
  type state = (thread_subsystem_state,storage_subsystem_state,PromisingViews.t0) Promising.pst
  type trans = (thread_subsystem_state,storage_subsystem_state,PromisingViews.t0) PromisingTransitions.ptrans

  type ui_trans = int * trans

  let final_ss_state s = PromisingStorage.pss_is_final_state
                           s.Promising.p_model.Params.ss
                           s.Promising.p_storage_state

  let sst_of_state = Promising.pst_of_state

  let initial_state ism run_options initial_state_record =
    Promising.p_initial_state
      (ISAModel.instruction_semantics ism run_options)
      ISAModel.ISADefs.reg_data
      initial_state_record
    |> Promising.pst_of_state

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

  let state_after_trans = Promising.pst_after_transition

  let is_final_state s = 
    s.Promising.pst_trans = [] && 
    Promising.p_is_final_state s.Promising.pst_state

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
    PromisingTransitions.p_priority_transitions
      s.pst_state.p_model
      (PromisingStorage.pss_is_final_state
         s.pst_state.p_model.Params.ss
         s.pst_state.p_storage_state)
      eager_mode

  let is_thread_trans = PromisingTransitions.p_is_thread_transition
  let ioid_of_trans = PromisingTransitions.ioid_of_pthread_trans

  let fuzzy_compare_transitions = Model_ptransition_aux.p_fuzzy_compare_transitions

  let is_fetch_single_trans = (fun _ -> false)
  let is_fetch_multi_trans = (fun _ -> false)

  let trans_fetch_addr = PromisingTransitions.ptrans_fetch_address
  let trans_reads_fp fp _s t = PromisingTransitions.ptrans_reads_footprint fp t
  let trans_writes_fp fp _s t = PromisingTransitions.ptrans_writes_footprint fp t

  let pretty_eiids s = Pp.p_pretty_eiids s.Promising.pst_state

  let number_finished_instructions s =
    Promising.p_number_of_instructions s.Promising.pst_state
  let number_constructed_instructions s =
    Promising.p_number_of_instructions s.Promising.pst_state

  let principal_ioid_of_trans = PromisingTransitions.principal_ioid_of_ptrans
  let is_ioid_finished i s =
    Promising.p_is_ioid_finished i s.Promising.pst_state
  let threadid_of_thread_trans = PromisingTransitions.thread_id_of_pthread_trans
  let ioid_of_thread_trans = PromisingTransitions.ioid_of_pthread_trans
  let is_storage_trans = PromisingTransitions.p_is_storage_transition
  let pp_transition_history = fun _ -> failwith "pp_transition_history"
  let pp_cand = Pp.pp_pcand
  let pp_trans = Pp.pp_ptrans
  let pp_ui_state = Pp.pp_ui_pstate
  let pp_instruction = Pp.pp_p_ui_instruction
  let set_model_params model s =
    let open Promising in
    pst_of_state {s.pst_state with p_model = model}
  let model_params s = s.Promising.pst_state.Promising.p_model
  let final_reg_states s = 
    let open Promising in
    Pmap.bindings_list s.pst_state.p_thread_states
    |> List.map (fun (tid, state) -> (tid, PromisingThread.p_registers_final_state state))
  let transitions s = s.Promising.pst_trans
  let stopped_promising s =
    let open Promising in
    s.pst_state.p_storage_state.PromisingStorage.pss_stopped_promising
  let write_after_stop_promising s = false
  let inst_discarded s = false
  let inst_restarted s = false
end

(********************************************************************)

