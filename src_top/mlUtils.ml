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

(* Things that really should be in the OCaml stdlib, but aren't *)

let rec option_map f xs =
  match xs with
  | [] -> []
  | x::xs ->
      ( match f x with
      | None -> option_map f xs
      | Some x -> x :: (option_map f xs) )

let string_startswith prefix str =
  match String.sub str 0 (String.length prefix) with
  | prefix' -> prefix' = prefix
  | exception Invalid_argument _ -> false

let string_drop n s =
  String.sub s n ((String.length s) - n)

(* 'safe_open_in filename f' will open filename, pass it to f and cloth
 * the channel at the end or when an exception is raised
 *)
let safe_open_in (filename: string) (f: in_channel -> 'a) : 'a =
    let chan = open_in filename in
    let res = try f chan with e -> close_in chan; raise e in
    close_in chan;
    res
