(*===============================================================================*)
(*                                                                               *)
(*                rmem executable model                                          *)
(*                =====================                                          *)
(*                                                                               *)
(*  This file is:                                                                *)
(*                                                                               *)
(*  Copyright Shaked Flur, University of Cambridge       2016-2017               *)
(*  Copyright Christopher Pulte, University of Cambridge      2016               *)
(*  Copyright Jon French, University of Cambridge             2017               *)
(*                                                                               *)
(*  All rights reserved.                                                         *)
(*                                                                               *)
(*  It is part of the rmem tool, distributed under the 2-clause BSD licence in   *)
(*  LICENCE.txt.                                                                 *)
(*                                                                               *)
(*===============================================================================*)

type 'i initial_state_record_maker =
  Params.model_params ->
  'i Params.initial_state_record
   

module type ConcModel_Info_Init = sig
  type instruction_ast
  val initial_state_record_maker : instruction_ast initial_state_record_maker
  val info : Test.info
  module ConcModel : Concurrency_model.S with type instruction_ast = instruction_ast
end


(* abstraction of ppcmem input file/format (e.g. litmus file, ELF binary) *)
module type S = sig

  (* type test *)

  (* the raw content of test file *)
  type data

  (* parse data and return test and the test name *)
  val read_data :
    RunOptions.t ->
    string ->
    data ->
    (Isa.isa_model -> unit) option ->
    (module ConcModel_Info_Init)

  (* parse file and return test and the test name *)
  val read_file :
    RunOptions.t ->
    string ->
    (Isa.isa_model -> unit) option ->
    (module ConcModel_Info_Init)
end
