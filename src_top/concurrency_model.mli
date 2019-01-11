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
  type thread_subsystem_state
  type storage_subsystem_state
  type system_state = (thread_subsystem_state,storage_subsystem_state) MachineDefTypes.system_state
  type ui_state = (thread_subsystem_state,storage_subsystem_state) MachineDefTypes.ui_system_state
  type system_state_and_transitions =
    (thread_subsystem_state,storage_subsystem_state) MachineDefTypes.system_state_and_transitions

  type ss_trans = storage_subsystem_state MachineDefTypes.ss_trans
  type trans = (thread_subsystem_state,storage_subsystem_state) MachineDefTypes.trans

  val final_ss_state : system_state -> bool

  val initial_system_state :
    MachineDefTypes.instruction_semantics_mode ->
    RunOptions.t ->
    MachineDefSystem.initial_state_record ->
    system_state

  val sst_of_state :
    RunOptions.t ->
    system_state ->
    system_state_and_transitions

  val enumerate_transitions :
    RunOptions.t ->
    system_state ->
    (storage_subsystem_state MachineDefTypes.ss_trans list) option ->
    (MachineDefTypes.thread_id, thread_subsystem_state MachineDefTypes.thread_trans list) Pmap.map ->
    (MachineDefTypes.thread_id, ((thread_subsystem_state,storage_subsystem_state) MachineDefTypes.trans * bool) list) Pmap.map ->
    ((thread_subsystem_state,storage_subsystem_state) MachineDefTypes.trans list) *
        (storage_subsystem_state MachineDefTypes.ss_trans list) *
        ((MachineDefTypes.thread_id, thread_subsystem_state MachineDefTypes.thread_trans list) Pmap.map) * 
          (MachineDefTypes.thread_id, ((thread_subsystem_state,storage_subsystem_state) MachineDefTypes.trans * bool) list) Pmap.map

  val state_after_transition :
    Globals.ppmode ->
    system_state ->
    (thread_subsystem_state,storage_subsystem_state) MachineDefTypes.trans ->
    (system_state * bool * MachineDefTypes.thread_id list * bool * bool * bool) MachineDefTypes.transition_outcome

  val sst_after_transition :
    RunOptions.t ->
    system_state_and_transitions ->
    (thread_subsystem_state,storage_subsystem_state) MachineDefTypes.trans ->
    system_state_and_transitions MachineDefTypes.transition_outcome

  val is_final_state :
    system_state -> bool

  val branch_targets_of_state :
    system_state -> MachineDefTypes.branch_targets_map
    (*(MachineDefTypes.thread_id, (Sail_impl_base.address, Sail_impl_base.address Pset.set) Pmap.map) Pmap.map*)

  val shared_memory_of_state : system_state -> MachineDefTypes.footprint Pset.set

  val memory_value_of_footprints :
    system_state ->
    MachineDefTypes.footprint list ->
    (MachineDefTypes.footprint * Sail_impl_base.memory_value) list

  val make_cex_candidate :
    system_state ->
    MachineDefCandidateExecution.cex_candidate

  val make_ui_system_state :
    Globals.ppmode ->
    system_state option -> (* prev state *)
    system_state ->        (* current state *)
    (thread_subsystem_state,storage_subsystem_state) MachineDefTypes.ui_trans list ->
    (Globals.ppmode * ui_state)
end

val make : (module Isa_model.S) -> MachineDefTypes.thread_model -> MachineDefTypes.storage_model -> (module S)
