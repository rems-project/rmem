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

type instruction_ast = InstructionSemantics.instruction_ast

module type TransSail = sig
  type instruction
  type labelmap = (string * int) list

  val shallow_ast_to_herdtools_ast : instruction_ast -> instruction
  val herdtools_ast_to_shallow_ast : instruction -> instruction_ast

  val herdtools_ast_to_interp_instruction : instruction -> Interp_interface.interp_instruction
  val interp_instruction_to_herdtools_ast : Interp_interface.interp_instruction -> instruction

  val unlabelize_ins :
      (string -> int) (** global variable lookup function *)
      -> labelmap     (** label locations *)
      -> int          (** current instruction index *)
      -> instruction  (** current instruction, possibly with labels *)
      -> instruction  (** unlabelized instruction *)
  val labelize_ins :
    (Sail_impl_base.address -> string option)
    -> Sail_impl_base.address
    -> instruction
    -> instruction

  val end_ins : instruction (** special to stop fetching *)
end

module RiscvTransSail : TransSail = struct
  type instruction = Riscv_types.ast2
  type labelmap = (string * int) list

  let shallow_ast_to_herdtools_ast _ = failwith "stub riscv shallow_ast_to_herdtools_ast"
  let herdtools_ast_to_shallow_ast _ = failwith "stub riscv herdtools_ast_to_shallow_ast"

  let herdtools_ast_to_interp_instruction _ = failwith "stub riscv herdtools_ast_to_interp_instruction"
  let interp_instruction_to_herdtools_ast _ = failwith "stub riscv interp_instruction_to_herdtools_ast"
  let unlabelize_ins _ _ _ _ = failwith "stub riscv unlabelize_ins"
  let labelize_ins _ _ _ = failwith "stub riscv labelize_ins"

  let end_ins = Riscv_types.STOP_FETCHING ()
end


let instruction_to_interp_instruction = function
 | PPCGEN_instr instr -> Power_toFromInterp.astToInterpValue instr
 | AArch64_instr instr -> ArmV8_toFromInterp.astToInterpValue0 instr
 | MIPS_instr instr -> Mips_toFromInterp.astToInterpValue1 instr
 | RISCV_instr instr -> failwith "not implemented yet"
 | X86_instr instr -> X86_toFromInterp.astToInterpValue3 instr


let interp_instruction_to_instruction ism instr = match ism with
  | PPCGEN_ism -> PPCGEN_instr (Power_toFromInterp.astFromInterpValue instr)
  | AARCH64_ism AArch64HandSail -> AArch64_instr (ArmV8_toFromInterp.astFromInterpValue0 instr)
  | AARCH64_ism AArch64GenSail -> failwith "not implemented yet"
  | MIPS_ism -> MIPS_instr (Mips_toFromInterp.astFromInterpValue1 instr)
  | RISCV_ism -> failwith "not implemented yet"
  | X86_ism -> X86_instr (X86_toFromInterp.astFromInterpValue3 instr)
