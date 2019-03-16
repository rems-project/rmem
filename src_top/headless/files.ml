(*===============================================================================*)
(*                                                                               *)
(*                rmem executable model                                          *)
(*                =====================                                          *)
(*                                                                               *)
(*  This file is:                                                                *)
(*                                                                               *)
(*  Copyright Shaked Flur, University of Cambridge 2017                          *)
(*                                                                               *)
(*  All rights reserved.                                                         *)
(*                                                                               *)
(*  It is part of the rmem tool, distributed under the 2-clause BSD licence in   *)
(*  LICENCE.txt.                                                                 *)
(*                                                                               *)
(*===============================================================================*)

let read_source_file (name: string) : string array option=
  let read_lines chan =
    let lines = ref [] in
    let () =
      try
        while true do lines := (input_line chan) :: !lines done
      with
      | End_of_file -> ()
    in
    !lines |> List.rev |> Array.of_list
  in
  let file = Filename.concat !Globals.dwarf_source_dir name in
  match MlUtils.safe_open_in file read_lines with
  | lines -> Some lines
  | exception Sys_error _ -> None
