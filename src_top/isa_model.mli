(*===============================================================================*)
(*                                                                               *)
(*                rmem executable model                                          *)
(*                =====================                                          *)
(*                                                                               *)
(*  This file is:                                                                *)
(*                                                                               *)
(*  Copyright Shaked Flur, University of Cambridge          2016-2017            *)
(*  Copyright Susmit Sarkar, University of St Andrews            2014            *)
(*  Copyright Christopher Pulte, University of Cambridge         2016            *)
(*  Copyright Jon French, University of Cambridge                2017            *)
(*  Copyright Robert Norton-Wright, University of Cambridge      2017            *)
(*                                                                               *)
(*  All rights reserved.                                                         *)
(*                                                                               *)
(*  It is part of the rmem tool, distributed under the 2-clause BSD licence in   *)
(*  LICENCE.txt.                                                                 *)
(*                                                                               *)
(*===============================================================================*)




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


module PPCGEN_ISA : S with type instruction_ast = Power_embed_types.ast
module AARCH64_HGEN_ISA : S with type instruction_ast = ArmV8_embed_types.ast
module AARCH64_GEN_ISA : S with type instruction_ast = ArmV8_embed_types.ast
module MIPS_ISA : S with type instruction_ast = Mips_embed_types.ast
module RISCV_ISA : S with type instruction_ast = Riscv_types.ast
module X86_ISA : S with type instruction_ast = X86_embed_types.ast

val all_isa_defs : (module S) list


val make : Isa.isa_model -> (module S)

