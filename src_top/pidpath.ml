(*===============================================================================*)
(*                                                                               *)
(*                rmem executable model                                          *)
(*                =====================                                          *)
(*                                                                               *)
(*  This file is:                                                                *)
(*                                                                               *)
(*  Copyright Jon French, University of Cambridge  2017                          *)
(*  Copyright Shaked Flur, University of Cambridge 2017                          *)
(*                                                                               *)
(*  All rights reserved.                                                         *)
(*                                                                               *)
(*  It is part of the rmem tool, distributed under the 2-clause BSD licence in   *)
(*  LICENCE.txt.                                                                 *)
(*                                                                               *)
(*===============================================================================*)

module type S = sig
  val pidpath : int -> string
  (** @raise [Unix.Unix_error] in case of error *)
end

(* module Mac : S = struct *)
(*   (\* Shamelessly stolen from this stackoverflow answer https://stackoverflow.com/a/22660134 *\) *)
(*   (\* but it's by Daniel Bünzli so I don't think he'll mind... ;-) *\) *)
(*   open Ctypes *)
(*   open Foreign *)

(*   let pidpathinfo_maxsize = 1024 * 4 *)
(*   (\* value of PROC_PIDPATHINFO_MAXSIZE. *)
(*      N.B. this should not be hardcoded, see 1) in this answer *)
(*      https://stackoverflow.com/questions/20851390 *\) *)

(*   let char_array_as_string a len = *)
(*   (\* This function should soon no longer be required see: *)
(*      https://github.com/ocamllabs/ocaml-ctypes/pull/139 *\) *)
(*     let b = Buffer.create len in *)
(*     for i = 0 to len -1 do Buffer.add_char b (CArray.get a i) done; *)
(*     Buffer.contents b *)

(*   let pidpath pid = *)
(*     let lib = Dl.dlopen ~filename:"libproc.dylib" ~flags:[] in *)
(*     let f = *)
(*       foreign ~from:lib ~check_errno:true "proc_pidpath" *)
(*               (int @-> ptr char @-> int @-> returning int) *)
(*     in *)
(*     let path = CArray.make char ~initial:'\x00' pidpathinfo_maxsize in *)
(*     let len = f pid (CArray.start path) pidpathinfo_maxsize in *)
(*     char_array_as_string path len *)
(* end *)

module Linux : S = struct
  let pidpath pid =
    let exe = Printf.sprintf "/proc/%d/exe" pid in
    Unix.readlink exe
end

let find_self () =
  let pid = Unix.getpid () in
  try
    let path = Linux.pidpath pid in
    if Structured_output.is_verbosity_at_least Structured_output.Debug then
      Printf.printf "found pidpath %s using Linux /proc method (for ISA defs)\n" path;
    path
  with
  | Unix.Unix_error (errno, func, msg) -> begin
      if Structured_output.is_verbosity_at_least Structured_output.Debug then
        Printf.printf "couldn't get pidpath using Linux /proc method (for ISA defs), error was: %s %s %s\n" (Unix.error_message errno) func msg;
      failwith "find_self"
      (* try *)
      (*   let path = Mac.pidpath pid in *)
      (*   if is_verbosity_at_least Debug then *)
      (*     Printf.printf "found pidpath %s using Mac libproc method (for ISA defs)\n" path; *)
      (*   path *)
      (* with *)
      (* | Dl.DL_error msg -> begin *)
      (*     if is_verbosity_at_least Debug then *)
      (*       Printf.printf "couldn't get pidpath using Mac libproc method (for ISA defs), error was: %s\n" msg; *)
      (*     failwith "find_self" *)
      (*   end *)
      (* | Unix.Unix_error (errno, func, msg) -> begin *)
      (*     if is_verbosity_at_least Debug then *)
      (*       Printf.printf "couldn't get pidpath using Mac libproc method (for ISA defs), error was: %s %s %s\n" (Unix.error_message errno) func msg; *)
      (*     failwith "find_self" *)
      (*   end *)
    end
