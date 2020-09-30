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
open Isa


open Trans


module type S = sig

  val name : string
  val isa_model : Isa.isa_model
  val available : bool

  type instruction_ast
  val isa : instruction_ast Isa.isa
  val pp_instruction_ast :
    Globals.ppmode ->
    ((Sail_impl_base.address * Sail_impl_base.size) * string) list ->
    instruction_ast ->
    (Sail_impl_base.address) ->
    string
end


module PPCGEN_ISA : S with type instruction_ast = Power_embed_types.ast = struct
  
  let name = "PPC"
  let isa_model = PPC
  let reg_data = PowerIsa.ppc_isa.Isa.register_data_info
  let available = PowerIsa.available

  type instruction_ast = Power_embed_types.ast
  let isa = PowerIsa.ppc_isa
  let pp_instruction_ast = Pp.pp_ppcgen_instruction
  module TransSail = PPCGenTransSail

end

module AARCH64_HGEN_ISA : S with type instruction_ast = ArmV8_embed_types.ast = struct

  let name = "AArch64"
  let isa_model = AARCH64 Hand
  let reg_data = Aarch64Isa.aarch64hand_isa.Isa.register_data_info
  let available = Aarch64Isa.available

  type instruction_ast = ArmV8_embed_types.ast
  let isa = Aarch64Isa.aarch64hand_isa
  let pp_instruction_ast = Pp.pp_aarch64_instruction
  module TransSail = (AArch64HGenTransSail)
end

module AARCH64_GEN_ISA : S with type instruction_ast = ArmV8_embed_types.ast = struct
  
  let name = "AArch64Gen"
  let isa_model = AARCH64 Gen
  let reg_data = Aarch64Isa.aarch64gen_isa.Isa.register_data_info
  let available = Aarch64Isa.available
  
  type instruction_ast = ArmV8_embed_types.ast
  let isa = Aarch64Isa.aarch64gen_isa
  let pp_instruction_ast = Pp.pp_aarch64_instruction
  module TransSail = (AArch64GenTransSail)
end

module MIPS_ISA : S with type instruction_ast = Mips_embed_types.ast = struct

  let name = "MIPS"
  let isa_model  = MIPS
  let reg_data = MipsIsa.mips_isa.Isa.register_data_info
  let available = MipsIsa.available

  type instruction_ast = Mips_embed_types.ast
  let isa = MipsIsa.mips_isa
  let pp_instruction_ast = Pp.pp_mips_instruction
  module TransSail = (MIPSHGenTransSail)
end

module RISCV_ISA : S with type instruction_ast = Riscv_types.ast = struct

  let name = "RISCV"
  let isa_model = RISCV
  let reg_data = RiscvIsa.riscv_isa.Isa.register_data_info
  let available = RiscvIsa.available

  type instruction_ast = Riscv_types.ast
  let isa = RiscvIsa.riscv_isa
  let pp_instruction_ast = Pp.pp_riscv_instruction
  module TransSail = (RiscvTransSail)
end

module X86_ISA : S with type instruction_ast = X86_embed_types.ast = struct
  let name = "X86"
  let isa_model = X86
  let reg_data = X86Isa.x86_isa.Isa.register_data_info
  let available = X86Isa.available
  type instruction_ast = X86_embed_types.ast
  let isa = X86Isa.x86_isa
  let pp_instruction_ast = Pp.pp_x86_instruction
  module TransSail = (X86HGenTransSail)
end

let all_isa_defs : (module S) list = [
    (module PPCGEN_ISA);
    (module AARCH64_HGEN_ISA);
    (module AARCH64_GEN_ISA);
    (module MIPS_ISA);
    (module RISCV_ISA);
    (module X86_ISA);
  ]


let make isa_model =
  match isa_model with
  | PPC -> (module PPCGEN_ISA : S)
  | AARCH64 Hand -> (module AARCH64_HGEN_ISA : S)
  | AARCH64 Gen  -> (module AARCH64_GEN_ISA : S)
  | MIPS -> (module MIPS_ISA : S)
  | RISCV -> (module RISCV_ISA : S)
  | X86 -> (module X86_ISA : S)
