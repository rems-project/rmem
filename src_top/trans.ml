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

module type TransSail = sig
  type instruction
  type instruction_ast
  type labelmap = (string * int) list

  val shallow_ast_to_herdtools_ast : instruction_ast -> instruction
  val herdtools_ast_to_shallow_ast : instruction -> instruction_ast


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

module RiscvTransSail : (TransSail with type instruction_ast = Riscv_types.ast) = struct
  type instruction_ast = Riscv_types.ast
  type instruction = Riscv_types.ast
  type labelmap = (string * int) list

  let shallow_ast_to_herdtools_ast _ = failwith "stub riscv shallow_ast_to_herdtools_ast"
  let herdtools_ast_to_shallow_ast _ = failwith "stub riscv herdtools_ast_to_shallow_ast"

  let unlabelize_ins _ _ _ _ = failwith "stub riscv unlabelize_ins"
  let labelize_ins _ _ _ = failwith "stub riscv labelize_ins"

  let end_ins = Riscv_types.STOP_FETCHING ()
end



