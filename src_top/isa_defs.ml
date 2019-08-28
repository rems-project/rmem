(*===============================================================================*)
(*                                                                               *)
(*                rmem executable model                                          *)
(*                =====================                                          *)
(*                                                                               *)
(*  This file is:                                                                *)
(*                                                                               *)
(*  Copyright Shaked Flur, University of Cambridge          2017-2018            *)
(*  Copyright Robert Norton-Wright, University of Cambridge      2017            *)
(*  Copyright Jon French, University of Cambridge           2017-2018            *)
(*  Copyright Christopher Pulte, University of Cambridge    2017-2018            *)
(*                                                                               *)
(*  All rights reserved.                                                         *)
(*                                                                               *)
(*  It is part of the rmem tool, distributed under the 2-clause BSD licence in   *)
(*  LICENCE.txt.                                                                 *)
(*                                                                               *)
(*===============================================================================*)

open InstructionSemantics

include Trans


(********************************************************************)

(* CP: this module should go away. The reg_data things are already in
   the ISA type, the rest should be handled purely by Sail *)

(* abstract the Sail ISA specific data *)

let memo_unit (f : unit -> 'a) =
  let v = (ref None : 'a option ref) in
  let g ?(no_memo = false) () =
    if no_memo then begin
        v := None;
        f ()
      end
    else
      match !v with
      | Some x -> x
      | None -> begin
          let x = f () in
          v := Some x;
          x
        end
  in
  g

module type ISADefs = sig
  val name : string
  val ism : InstructionSemantics.instruction_semantics_mode

  val isa_defs_thunk : ?no_memo:bool -> unit -> Interp_interface.specification
  val interp2_isa_defs_thunk : ?no_memo:bool -> unit -> (Type_check.tannot Ast.defs * Type_check.Env.t)
  val isa_memory_access : (Interp_interface.memory_reads *
                        Interp_interface.memory_read_taggeds *
                        Interp_interface.memory_writes *
                        Interp_interface.memory_write_eas *
                        Interp_interface.memory_write_vals *
                        Interp_interface.memory_write_vals_tagged *
                        Interp_interface.barriers *
                        Interp_interface.excl_res)
  val isa_externs : Interp_interface.external_functions
end

let empty_isa_defs = memo_unit (fun () -> Interp_ast.Defs [])
let empty_interp2_isa_defs = memo_unit (fun () -> (Ast.Defs [], Type_check.initial_env))

module PPCGenISADefs : ISADefs = struct
  let name = "PPC"
  let ism = PPCGEN_ism
  let reg_data = PowerIsa.ppcgen_ism.Isa.register_data_info
  let isa_defs_thunk = memo_unit (fun () -> Screen.unmarshal_defs "PPCGen")
  let interp2_isa_defs_thunk = empty_interp2_isa_defs
  let isa_memory_access = (Power_extras.power_read_memory_functions,
                            [],
                            [],
                            Power_extras.power_memory_eas,
                            Power_extras.power_memory_vals,
                            [],
                            Power_extras.power_barrier_functions,
                            None)
  let isa_externs = Power_extras.power_externs
end

module AArch64ISADefs : ISADefs = struct
    let name = "AArch64"
    let ism = AARCH64_ism AArch64HandSail
    let reg_data = Aarch64Isa.aarch64hand_ism.Isa.register_data_info
    let isa_defs_thunk = memo_unit (fun () -> Screen.unmarshal_defs "AArch64")
    let interp2_isa_defs_thunk = empty_interp2_isa_defs
    let isa_memory_access = (ArmV8_extras.aArch64_read_memory_functions,
                              [],
                              ArmV8_extras.aArch64_memory_writes,
                              ArmV8_extras.aArch64_memory_eas,
                              ArmV8_extras.aArch64_memory_vals,
                              [],
                              ArmV8_extras.aArch64_barrier_functions,
                              ArmV8_extras.aArch64_excl_res)
    let isa_externs = []
end

module AArch64GenISADefs : ISADefs = struct
    let name = "AArch64Gen"
    let ism = AARCH64_ism AArch64GenSail
    let reg_data = Aarch64Isa.aarch64gen_ism.Isa.register_data_info
    let isa_defs_thunk = memo_unit (fun () -> Screen.unmarshal_defs "AArch64Gen")
    let interp2_isa_defs_thunk = empty_interp2_isa_defs
    let isa_memory_access = ([], (*ArmV8Gen_extras.aArch64_read_memory_functions*)
                              [],
                              [], (*ArmV8Gen_extras.aArch64_memory_writes*)
                              [], (*ArmV8Gen_extras.aArch64_memory_eas*)
                              [], (*ArmV8Gen_extras.aArch64_memory_vals*)
                              [],
                              [], (*ArmV8Gen_extras.aArch64_barrier_functions*)
                              None)
    let isa_externs = []
end

module MIPS64ISADefs : ISADefs = struct
    let name = "MIPS"
    let ism  = MIPS_ism
    let reg_data = MipsIsa.mips_ism.Isa.register_data_info
    let isa_defs_thunk = memo_unit (fun () -> Screen.unmarshal_defs "MIPS64")
    let interp2_isa_defs_thunk = empty_interp2_isa_defs
    let isa_memory_access = (Mips_extras.mips_read_memory_functions,
                              [],
                              Mips_extras.mips_memory_writes,
                              Mips_extras.mips_memory_eas,
                              Mips_extras.mips_memory_vals,
                              [],
                              Mips_extras.mips_barrier_functions,
                              None)
    let isa_externs = []
end

module RISCVISADefs : ISADefs = struct
    let name = "RISCV"
    let ism = RISCV_ism
    let reg_data = RiscvIsa.riscv_ism.Isa.register_data_info
    let isa_defs_thunk = empty_isa_defs
    let interp2_isa_defs_thunk = memo_unit (fun () -> Screen.unmarshal_interp2_defs "riscv")
    (* let isa_memory_access = (Riscv_extras.riscv_read_memory_functions,
     *                           [],
     *                           Riscv_extras.riscv_memory_writes,
     *                           Riscv_extras.riscv_memory_eas,
     *                           Riscv_extras.riscv_memory_vals,
     *                           [],
     *                           Riscv_extras.riscv_barrier_functions,
     *                           Riscv_extras.riscv_speculate_conditional_success) *)
    let isa_memory_access = ([], [], [], [], [], [], [], None)
    let isa_externs = []
end

module X86ISADefs : ISADefs = struct
    let name = "X86"
    let ism = X86_ism
    let reg_data = X86Isa.x86_ism.Isa.register_data_info
    let isa_defs_thunk = memo_unit (fun () -> Screen.unmarshal_defs "X86")
    let interp2_isa_defs_thunk = empty_interp2_isa_defs
    let isa_memory_access = (X86_extras.x86_read_memory_functions,
                              [],
                              X86_extras.x86_memory_writes,
                              X86_extras.x86_memory_eas,
                              X86_extras.x86_memory_vals,
                              [],
                              X86_extras.x86_barrier_functions,
                              None)
    let isa_externs = []
end

let all_isa_defs : (module ISADefs) list = [
    (module PPCGenISADefs);
    (module AArch64ISADefs);
(*     (module AArch64GenISADefs); *)
    (module MIPS64ISADefs);
    (module RISCVISADefs);
    (module X86ISADefs);
  ]

