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



module type ISADefs = sig
  val name : string
  val isa_model : Isa.isa_model
  (* val reg_data : Isa.registerdata *)

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


module PPCGenISADefs : ISADefs
module AArch64ISADefs : ISADefs
module AArch64GenISADefs : ISADefs
module MIPS64ISADefs : ISADefs
module RISCVISADefs : ISADefs
module X86ISADefs : ISADefs

val all_isa_defs : (module ISADefs) list


module type Bool = sig
  val b : bool
end

module type ISA = sig
  type instruction_ast
  val isa: instruction_ast Isa.isa
  val pp_instruction_ast :
    Globals.ppmode ->
    ((Sail_impl_base.address * Sail_impl_base.size) * string) list ->
    instruction_ast ->
    (Sail_impl_base.address) ->
    string
  module TransSail: Trans.TransSail with type instruction_ast = instruction_ast
end  

module type S = sig
  type instruction_ast
  module ISADefs : ISADefs
  val isa : instruction_ast Isa.isa
  val pp_instruction_ast :
    Globals.ppmode ->
    ((Sail_impl_base.address * Sail_impl_base.size) * string) list ->
    instruction_ast ->
    (Sail_impl_base.address) ->
    string
end

module PPCGEN_ISA : ISA with type instruction_ast = Power_embed_types.ast0
module AARCH64_HGEN_ISA : ISA with type instruction_ast = ArmV8_embed_types.ast1
module AARCH64_GEN_ISA : ISA with type instruction_ast = ArmV8_embed_types.ast1
module MIPS_ISA : ISA with type instruction_ast = Mips_embed_types.ast2
module RISCV_ISA : ISA with type instruction_ast = Riscv_types.ast
module X86_ISA : ISA with type instruction_ast = X86_embed_types.ast3



module Make 
    (IsShallowEmbedding : Bool)
    (Isa : ISA)
    (ISADefs : ISADefs) :
   (S with type instruction_ast = Isa.instruction_ast)

val make : Isa.isa_model -> RunOptions.t -> (module S)

