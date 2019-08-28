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

(* Abstraction of operational concurrency model *)
module type S = sig

  type instruction_ast
  type state
  type trans
  type ui_state
  type ui_trans = int * trans

  module ISA : Isa_model.S with type instruction_ast = instruction_ast

  val initial_state :
    InstructionSemantics.instruction_semantics_mode ->
    RunOptions.t ->
    instruction_ast Params.initial_state_record ->
    state

  val transitions : state -> trans list
  val state_after_trans :
    state -> trans -> ((instruction_ast,state) ExceptionTypes.transition_outcome)

  val model_params     : state -> Params.model_params
  val set_model_params : Params.model_params -> state -> state

  (* state predicates *)
  val is_final_state    : state -> bool
  val inst_restarted    : state -> bool
  val inst_discarded    : state -> bool
  val write_after_stop_promising
                        : state -> bool
  val stopped_promising : state -> bool


  val branch_targets_of_state :
    state -> Params.branch_targets_map
  val modified_code_locations_of_state :
    state -> Sail_impl_base.footprint Pset.set
  val shared_memory_of_state :
    state -> Sail_impl_base.footprint Pset.set
  val memory_value_of_footprints :
    state -> Sail_impl_base.footprint list ->
    (Sail_impl_base.footprint * Sail_impl_base.memory_value) list
  val final_reg_states :
    state -> (Events.thread_id * (Sail_impl_base.reg_base_name * Sail_impl_base.register_value option) list) list

  val make_cex_candidate :
    state ->
    instruction_ast CandidateExecution.cex_candidate

  val number_finished_instructions    : state -> int
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
  val pp_instruction_ast :
    Globals.ppmode ->
    ((Sail_impl_base.address * Sail_impl_base.size) * string) list ->
    instruction_ast ->
    (Sail_impl_base.address) ->
    string
  val pp_instruction :
    Globals.ppmode ->
    ui_state ->
    Events.thread_id ->
    Events.ioid ->
    string
  val pretty_eiids :
    state ->
    (Events.eiid * string) list

  val is_thread_trans :
    trans -> bool
  val is_storage_trans :
    trans -> bool
  val priority_trans :
    state -> Params.eager_mode -> ((trans -> bool) * bool) list
  val is_loop_limit_trans :
    trans ->  bool
  val is_eager_trans :
    state -> Params.eager_mode -> trans -> bool
  val is_fetch_single_trans :
    trans -> bool
  val is_fetch_multi_trans :
    trans -> bool
  val trans_fetch_addr :
    trans -> Sail_impl_base.address option
  val trans_reads_fp :
    Sail_impl_base.footprint -> state -> trans -> bool
  val trans_writes_fp :
    Sail_impl_base.footprint -> state -> trans -> bool
  val ioid_of_thread_trans :
    trans -> Events.ioid option
  val threadid_of_thread_trans :
    trans -> Events.thread_id option
  val is_ioid_finished :
    Events.ioid -> state -> bool
  val principal_ioid_of_trans :
    trans -> Events.ioid option
  val fuzzy_compare_transitions :
    trans -> trans -> int

  val filters :
    RunOptions.t ->
    Test.info ->
    (state -> trans -> bool) list
end


