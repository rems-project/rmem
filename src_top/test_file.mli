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
(*  The rmem tool is distributed under the 2-clause BSD license in LICENCE.txt.  *)
(*  For author information see README.md.                                        *)
(*                                                                               *)
(*===============================================================================*)

(* abstraction of ppcmem input file/format (e.g. litmus file, ELF binary) *)
module type S = sig

  type test

  val initial_state_record :
      test ->
      (module Isa_model.ISADefs) ->
      MachineDefTypes.model_params ->
      MachineDefSystem.initial_state_record

  (* the raw content of test file *)
  type data

  (* parse data and return test and the test name *)
  val read_data : string -> data -> (MachineDefTypes.instruction_semantics_mode -> unit) option -> (Test.info * test)
  (* parse file and return test and the test name *)
  val read_file : string -> (MachineDefTypes.instruction_semantics_mode -> unit) option -> (Test.info * test)
end
