(*===============================================================================*)
(*                                                                               *)
(*                rmem executable model                                          *)
(*                =====================                                          *)
(*                                                                               *)
(*  This file is:                                                                *)
(*                                                                               *)
(*  Copyright Shaked Flur, University of Cambridge 2016-2017                     *)
(*  Copyright Jon French, University of Cambridge       2017                     *)
(*                                                                               *)
(*  All rights reserved.                                                         *)
(*                                                                               *)
(*  It is part of the rmem tool, distributed under the 2-clause BSD licence in   *)
(*  LICENCE.txt.                                                                 *)
(*                                                                               *)
(*===============================================================================*)

val from_litmus_data: RunOptions.t -> string -> Litmus_test_file.data -> (InstructionSemantics.instruction_semantics_mode -> unit) option -> unit
val from_ELF_data:    RunOptions.t -> string -> Elf_test_file.data    -> (InstructionSemantics.instruction_semantics_mode -> unit) option -> unit

val from_files: RunOptions.t -> (Types.filetype * string) list -> unit
