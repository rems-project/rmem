(*========================================================================================*)
(*                                                                                        *)
(*                rmem executable model                                                   *)
(*                =====================                                                   *)
(*                                                                                        *)
(*  This file is:                                                                         *)
(*                                                                                        *)
(*  Copyright Christopher Pulte, University of Cambridge                      2017-2018   *)
(*  Copyright Shaked Flur, University of Cambridge                                 2017   *)
(*  Copyright Linden Ralph, University of Cambridge (when this work was done)      2017   *)
(*  Copyright Jon French, University of Cambridge                                  2017   *)
(*                                                                                        *)
(*  All rights reserved.                                                                  *)
(*                                                                                        *)
(*  It is part of the rmem tool, distributed under the 2-clause BSD licence in            *)
(*  LICENCE.txt.                                                                          *)
(*                                                                                        *)
(*========================================================================================*)

module type SS = sig
  type storage_subsystem_state
  val dict_StorageSubsystem : 
    storage_subsystem_state MachineDefTypes.storageSubsystem
  val dict_MachineStorageSubsystem : 
    ('ts,storage_subsystem_state) MachineDefTypes.machineStorageSubsystem
end

module FlowingSS : SS = struct
    type storage_subsystem_state = MachineDefTypes.flowing_storage_subsystem_state
    let dict_StorageSubsystem = MachineDefFlowingStorageSubsystem.flowing_storage
    let dict_MachineStorageSubsystem = MachineDefFlowingStorageSubsystem.flowing_machine_storage
end

module FlatSS : SS = struct
    type storage_subsystem_state = MachineDefTypes.flat_storage_subsystem_state
    let dict_StorageSubsystem = MachineDefFlatStorageSubsystem.flat_storage
    let dict_MachineStorageSubsystem = MachineDefFlatStorageSubsystem.flat_machine_storage
end

module POPSS : SS = struct
    type storage_subsystem_state = MachineDefTypes.pop_storage_subsystem_state
    let dict_StorageSubsystem = MachineDefPOPStorageSubsystem.pop_storage
    let dict_MachineStorageSubsystem = MachineDefPOPStorageSubsystem.pop_machine_storage
end

module NOPSS : SS = struct
    type storage_subsystem_state = MachineDefTypes.nop_storage_subsystem_state
    let dict_StorageSubsystem = MachineDefNOPStorageSubsystem.nop_storage
    let dict_MachineStorageSubsystem = MachineDefNOPStorageSubsystem.nop_machine_storage
end

module PLDI11SS : SS = struct
    type storage_subsystem_state = MachineDefTypes.pldi11_storage_subsystem_state
    let dict_StorageSubsystem = MachineDefPLDI11StorageSubsystem.pldi11_storage
    let dict_MachineStorageSubsystem = MachineDefPLDI11StorageSubsystem.pldi11_machine_storage
end

module TSOSS : SS = struct
    type storage_subsystem_state = MachineDefTypes.tso_storage_subsystem_state
    let dict_StorageSubsystem = MachineDefTSOStorageSubsystem.tso_storage
    let dict_MachineStorageSubsystem = MachineDefTSOStorageSubsystem.tso_machine_storage
end

(* module PromisingSS : SS = struct
 *     type storage_subsystem_state = MachineDefTypes.promising_storage_state
 *     let dict_StorageSubsystem = MachineDefPromisingARM.promising_storage
 * end *)

module type SYS = sig
  type ts
  type ss
  val dict_sys : (ts,ss) MachineDefTypes.system
  val dict_ts : ts MachineDefTypes.threadSubsystem
  val dict_ss : ss MachineDefTypes.storageSubsystem
end

module MachineSYS (Ss : SS) = struct
  type ts = MachineDefTypes.thread_state
  type ss = Ss.storage_subsystem_state
  let dict_sys = MachineDefSystem.machine_system (Ss.dict_MachineStorageSubsystem)
  let dict_ts = MachineDefThreadSubsystem.machine_thread
  let dict_ss = Ss.dict_StorageSubsystem
end 

module PromisingSYS = struct
  type ts = MachineDefTypes.promising_thread_state
  type ss = MachineDefTypes.promising_storage_state
  let dict_sys = MachineDefPromisingARM.promising_system
  let dict_ts = MachineDefPromisingARM.promising_thread
  let dict_ss = MachineDefPromisingARM.promising_storage
end 


(********************************************************************)

module type S = sig
  type thread_subsystem_state
  type storage_subsystem_state
  type system_state = (thread_subsystem_state,storage_subsystem_state) MachineDefTypes.system_state
  type ui_state = (thread_subsystem_state,storage_subsystem_state) MachineDefTypes.ui_system_state
  type system_state_and_transitions =
    (thread_subsystem_state,storage_subsystem_state)
      MachineDefTypes.system_state_and_transitions

  type ss_trans = storage_subsystem_state MachineDefTypes.ss_trans
  type trans = (thread_subsystem_state,storage_subsystem_state) MachineDefTypes.trans

  val final_ss_state :
    system_state -> bool

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
          ((MachineDefTypes.thread_id, 
            ((thread_subsystem_state,storage_subsystem_state) MachineDefTypes.trans * bool) list) Pmap.map)

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

module Make (ISAModel: Isa_model.S) (Sys: SYS) : S = struct
  type thread_subsystem_state = Sys.ts
  type storage_subsystem_state = Sys.ss
  type system_state = (thread_subsystem_state,storage_subsystem_state) MachineDefTypes.system_state
  type ui_state = (thread_subsystem_state,storage_subsystem_state) MachineDefTypes.ui_system_state
  type system_state_and_transitions =
    (thread_subsystem_state,storage_subsystem_state) MachineDefTypes.system_state_and_transitions

  type ss_trans = storage_subsystem_state MachineDefTypes.ss_trans
  type trans = (thread_subsystem_state,storage_subsystem_state) MachineDefTypes.trans

  let final_ss_state s = Sys.dict_ss.MachineDefTypes.ss_is_final_state
                           s.MachineDefTypes.model.MachineDefTypes.ss
                           s.MachineDefTypes.storage_subsystem

  let initial_system_state ism run_options initial_state_record =
    MachineDefSystem.initial_system_state
      (ISAModel.instruction_semantics ism run_options)
      (Sys.dict_ts)
      (Sys.dict_ss)
      (Sys.dict_sys)
      (ISAModel.is_option run_options)
      ISAModel.ISADefs.reg_data
      initial_state_record

  let update_dwarf (state: system_state) (ppmode: Globals.ppmode) : Globals.ppmode =
    let pp_dwarf_dynamic =
      { Types.get_evaluation_context = (fun ioid ->
            MachineDefSystem.get_dwarf_evaluation_context
              (Globals.get_endianness ()) (*TODO: Shaked, is that right?*)
              state
              (fst ioid)  (* relies on internal construction of fresh ioids*)
              ioid);

        Types.pp_all_location_data_at_instruction = (fun tid ioid ->
            (* turning this off for now - should be switchable *)
            match ppmode.Globals.pp_dwarf_static with
            | Some ds ->
                begin match MachineDefSystem.get_dwarf_evaluation_context
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

  let make_ui_system_state ppmode prev_state state ncands =
    let ppmode' = update_dwarf state ppmode in
    let ui_state =
      match (prev_state,ppmode.Globals.pp_kind) with
      | (None,_)
      | (_,Globals.Hash) ->
          Sys.dict_sys.MachineDefTypes.s_make_ui_system_state
            None state ncands
      | (Some prev_state,_) ->
          Sys.dict_sys.MachineDefTypes.s_make_ui_system_state
            (Some prev_state) state ncands
    in
    (ppmode', ui_state)

  let sst_of_state run_options state =
    begin try
      MachineDefSystem.sst_of_state
        (ISAModel.is_option run_options)
        state
    with
    | Debug.Thread_failure (tid, ioid, s, bt) ->
        let (ppmode', ui_state) = make_ui_system_state (Globals.get_ppmode ()) None state [] in
        Printf.eprintf "Failure in the thread subsystem while enumerating the transitions of:\n";
        Printf.eprintf "%s\n" (Pp.pp_ui_instruction ppmode' ui_state tid ioid);
        Printf.eprintf "%s\n\n" s;
        Printf.eprintf "%s\n" bt;
        exit 1

    | (Failure s) as e ->
        Printf.eprintf "Failure while enumerating transitions:\n";
        Printf.eprintf "%s\n\n" s;
        raise e (* changing this line to anything else will destroy the backtrace *)
    end


  let enumerate_transitions run_options state cached_ss_trans cached_thread_trans =
    begin try
      Sys.dict_sys.MachineDefTypes.s_enumerate_transitions_of_system
        (ISAModel.is_option run_options)
        state
        cached_ss_trans
        cached_thread_trans
    with
    | Debug.Thread_failure (tid, ioid, s, bt) ->
        let (ppmode', ui_state) = make_ui_system_state (Globals.get_ppmode ()) None state [] in
        Printf.eprintf "Failure in the thread subsystem while enumerating the transitions of:\n";
        Printf.eprintf "%s\n" (Pp.pp_ui_instruction ppmode' ui_state tid ioid);
        Printf.eprintf "%s\n\n" s;
        Printf.eprintf "%s\n" bt;
        exit 1

    | (Failure s) as e ->
        Printf.eprintf "Failure while enumerating transitions:\n";
        Printf.eprintf "%s\n\n" s;
        raise e (* changing this line to anything else will destroy the backtrace *)
    end

  let state_after_transition ppmode state trans =
    begin try
      MachineDefSystem.system_state_after_transition state trans
    with
    | (Failure s) as e ->
        let (ppmode', ui_state) = make_ui_system_state ppmode None state [] in
        Printf.eprintf "\n*** system state before taking the transition (see failure below) ***\n\n";
        Printf.eprintf "%s\n" (Pp.pp_ui_system_state ppmode' ui_state);
        Printf.eprintf "Failure while taking transition (from the state above):\n";
        Printf.eprintf "  %s\n\n" (Pp.pp_trans ppmode trans);
        Printf.eprintf "%s\n\n" s;
        raise e (* changing this line to anything else will destroy the backtrace *)
    end

  let sst_after_transition run_options sst trans =
    begin try
      MachineDefSystem.sst_after_transition
        (ISAModel.is_option run_options)
        sst
        trans
    with
    | (Failure s) as e ->
        let (ppmode', ui_state) = make_ui_system_state (Globals.get_ppmode ()) None sst.MachineDefTypes.sst_state [] in
        Printf.eprintf "\n*** system state before taking the transition (see failure below) ***\n\n";
        Printf.eprintf "%s\n" (Pp.pp_ui_system_state ppmode' ui_state);
        Printf.eprintf "Failure while taking transition (from the state above):\n";
        Printf.eprintf "  %s\n\n" (Pp.pp_trans ppmode' trans);
        Printf.eprintf "%s\n\n" s;
        raise e (* changing this line to anything else will destroy the backtrace *)
    end

  let is_final_state = MachineDefSystem.is_final_state
  let branch_targets_of_state = MachineDefSystem.branch_targets_of_state
  let shared_memory_of_state = MachineDefSystem.shared_memory_of_state
  let memory_value_of_footprints = MachineDefSystem.memory_value_of_footprints
  let make_cex_candidate = MachineDefCandidateExecution.make_cex_candidate
end

(********************************************************************)


let make (isaModel: (module Isa_model.S)) ts ss = 


  let (module Sys : SYS) = match (ts,ss) with
    | (MachineDefTypes.Promising_thread_model,_)  -> (module PromisingSYS)
    | (_,MachineDefTypes.Promising_storage_model) -> (module PromisingSYS)
    | (_,MachineDefTypes.PLDI11_storage_model)    -> (module MachineSYS(PLDI11SS))
    | (_,MachineDefTypes.Flowing_storage_model)   -> (module MachineSYS(FlowingSS))
    | (_,MachineDefTypes.Flat_storage_model)      -> (module MachineSYS(FlatSS))
    | (_,MachineDefTypes.POP_storage_model)       -> (module MachineSYS(POPSS))
    | (_,MachineDefTypes.NOP_storage_model)       -> (module MachineSYS(NOPSS))
    | (_,MachineDefTypes.TSO_storage_model)       -> (module MachineSYS(TSOSS))
  in

  (module (Make ((val isaModel)) (Sys)) : S)

(*
let make (isaModel: (module Isa_model.S)) = function
  | MachineDefTypes.PLDI11_storage_model  -> (module (Make ((val isaModel)) (PLDI11SS))  : S)
  | MachineDefTypes.Flowing_storage_model -> (module (Make ((val isaModel)) (FlowingSS)) : S)
  | MachineDefTypes.Flat_storage_model    -> (module (Make ((val isaModel)) (FlatSS))    : S)
  | MachineDefTypes.POP_storage_model     -> (module (Make ((val isaModel)) (POPSS))     : S)
  | MachineDefTypes.NOP_storage_model     -> (module (Make ((val isaModel)) (NOPSS))     : S)
  | MachineDefTypes.TSO_storage_model     -> (module (Make ((val isaModel)) (TSOSS))     : S)
  | MachineDefTypes.Promising_storage_model -> (module (Make ((val isaModel)) (PromisingSS)) : S)
 *)
