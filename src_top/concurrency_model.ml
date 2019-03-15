(*===============================================================================*)
(*                                                                               *)
(*                rmem executable model                                          *)
(*                =====================                                          *)
(*                                                                               *)
(*  This file is:                                                                *)
(*                                                                               *)
(*  Copyright Christopher Pulte, University of Cambridge 2017-2018               *)
(*  Copyright Shaked Flur, University of Cambridge            2017               *)
(*  Copyright Jon French, University of Cambridge             2017               *)
(*                                                                               *)
(*  All rights reserved.                                                         *)
(*                                                                               *)
(*  It is part of the rmem tool, distributed under the 2-clause BSD licence in   *)
(*  LICENCE.txt.                                                                 *)
(*                                                                               *)
(*===============================================================================*)

module type S = sig

  type sst (* state and transitions *)
  type state
  type trans
  type ui_state
  type ui_trans = int * trans


  val sst_of_state :
    RunOptions.t -> state -> sst
  val sst_state :
    sst -> state
  val sst_inst_restarted :
    sst -> bool
  val sst_inst_discarded :
    sst -> bool
  val sst_write_after_stop_promising :
    sst -> bool
  val sst_stopped_promising :
    sst -> bool
  val sst_trans :
    sst -> trans list
  val sst_after_trans :
    RunOptions.t ->
    sst ->
    trans ->
    (sst MachineDefExceptions.transition_outcome)

  val initial_state :
    MachineDefInstructionSemantics.instruction_semantics_mode ->
    RunOptions.t ->
    MachineDefParams.initial_state_record ->
    state
  val is_final_state :
    state -> bool
  val branch_targets_of_state :
    state -> MachineDefParams.branch_targets_map
  val shared_memory_of_state :
    state -> Sail_impl_base.footprint Pset.set
  val memory_value_of_footprints :
    state -> Sail_impl_base.footprint list ->
    (Sail_impl_base.footprint * Sail_impl_base.memory_value) list
  val make_cex_candidate :
    state ->
    MachineDefCandidateExecution.cex_candidate
  val final_reg_states :
    state -> (MachineDefEvents.thread_id * (Sail_impl_base.reg_base_name * Sail_impl_base.register_value option) list) list
  val model_params :
    state -> MachineDefParams.model_params
  val set_model_params :
    state -> MachineDefParams.model_params -> state

  val number_finished_instructions : state -> int
  val number_constructed_instructions : state -> int

  val make_ui_state :
    Globals.ppmode ->
    state option -> (* prev state *)
    state ->        (* current state *)
    ui_trans list ->
    (Globals.ppmode * ui_state)
  val pp_ui_state :
    Globals.ppmode ->
    ui_state ->
    string
  val pp_trans :
    ?graph:bool ->
    Globals.ppmode ->
    trans ->
    string
  val pp_cand :
    Globals.ppmode ->
    (int * trans) ->
    string
  val pp_transition_history :           
    Globals.ppmode ->
    ?filter:(trans -> bool) ->
    ui_state ->
    string
  val pretty_eiids :
    state ->
    (MachineDefEvents.eiid * string) list

  val is_thread_trans :
    trans -> bool
  val is_storage_trans :
    trans -> bool
  val priority_trans :
    sst -> MachineDefParams.eager_mode -> ((trans -> bool) * bool) list 
  val is_loop_limit_trans :
    trans ->  bool
  val is_eager_trans :
    state -> MachineDefParams.eager_mode -> trans -> bool
  val is_fetch_single_trans :
    trans -> bool
  val is_fetch_multi_trans :
    trans -> bool
  val trans_fetch_addr :
    trans -> Sail_impl_base.address option
  val trans_reads_fp :
    Sail_impl_base.footprint -> sst -> trans -> bool
  val trans_writes_fp :
    Sail_impl_base.footprint -> sst -> trans -> bool
  val ioid_of_thread_trans :
    trans -> MachineDefEvents.ioid option
  val threadid_of_thread_trans :
    trans -> MachineDefEvents.thread_id option
  val is_ioid_finished :
    MachineDefEvents.ioid -> sst -> bool
  val principal_ioid_of_trans :
    trans -> MachineDefEvents.ioid
  val fuzzy_compare_transitions :
    trans -> trans -> int

end

