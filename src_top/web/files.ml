(*===============================================================================*)
(*                                                                               *)
(*                rmem executable model                                          *)
(*                =====================                                          *)
(*                                                                               *)
(*  This file is:                                                                *)
(*                                                                               *)
(*  Copyright Shaked Flur, University of Cambridge 2017                          *)
(*  Copyright Jon French, University of Cambridge  2017                          *)
(*                                                                               *)
(*  All rights reserved.                                                         *)
(*                                                                               *)
(*  It is part of the rmem tool, distributed under the 2-clause BSD licence in   *)
(*  LICENCE.txt.                                                                 *)
(*                                                                               *)
(*===============================================================================*)

let sources = ref ([] : (string * string array) list)

let update_sources new_sources =
  (* Ew, javascript (again) *)
  let objects = Array.to_list (Js.to_array new_sources) in
  sources := List.map
               (fun source ->
                 (Js.to_string source##.name,
                  (Array.map Js.to_string (Js.to_array (Js.str_array (source##.content##split (Js.string "\n")))))))
               objects

let read_source_file (name: string) : string array option =
  try
    Some (List.assoc name !sources)
  with Not_found -> None
