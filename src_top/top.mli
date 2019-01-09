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
(*  The rmem tool is distributed under the 2-clause BSD license in LICENCE.txt.  *)
(*  For author information see README.md.                                        *)
(*                                                                               *)
(*===============================================================================*)

val from_litmus_data: RunOptions.t -> string -> Litmus_test_file.data -> (MachineDefTypes.instruction_semantics_mode -> unit) option -> unit
val from_ELF_data:    RunOptions.t -> string -> Elf_test_file.data    -> (MachineDefTypes.instruction_semantics_mode -> unit) option -> unit

val from_files: RunOptions.t -> (Types.filetype * string) list -> unit
