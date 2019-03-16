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


type instruction_ast = InstructionSemantics.instruction_ast

module type TransSail = sig
  type instruction
  type labelmap = (string * int) list

  val shallow_ast_to_herdtools_ast : instruction_ast -> instruction
  val herdtools_ast_to_shallow_ast : instruction -> instruction_ast

  val herdtools_ast_to_interp_instruction : instruction -> Interp_interface.instruction
  val interp_instruction_to_herdtools_ast : Interp_interface.instruction -> instruction

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

module type ISADefs = sig
  val name : string
  val reg_data : BasicTypes.registerdata

  val isa_defs_thunk : ?no_memo:bool -> unit -> Interp_interface.specification
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

val all_isa_defs : (module ISADefs) list

module type S = sig
  module ISADefs : ISADefs

  val instruction_semantics :
    InstructionSemantics.instruction_semantics_mode ->
    RunOptions.t ->
    InstructionSemantics.instruction_semantics_p

  val is_option :
    RunOptions.t ->
    InstructionSemantics.instruction_semantics_option
end

val make :
  InstructionSemantics.instruction_semantics_mode ->
  (module S)
