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

(* signatures for *_version.ml files *)

module type Svn = sig
  (* the output of 'svn info' as associative list (each line is split to
  key-value with ':' as the delimiter) *)
  val info   : (string * string) list

  (* the output of: svn status -q *)
  val status : string
end

module Make_svn (Version: Svn) = struct
  include Version

  let str : string =
    (List.map (fun (key, data) -> key ^ ": " ^ data) Version.info
    |> String.concat "\n") ^
    "\n\n" ^
    begin match String.trim Version.status with
    | ""     -> "Status: (clean)\n"
    | status -> "Status:\n" ^ status ^ "\n"
    end

  let revision : string =
    match List.assoc "Revision" Version.info with
    | s ->
        let clean_str =
          if String.trim Version.status = "" then "(clean)"
          else "(not clean)"
        in
        s ^ " " ^ clean_str
    | exception Not_found -> "unknown"

  let last_changed : string =
    try List.assoc "Last Changed Date" Version.info with
    | Not_found -> "unknown date"
end (* Make_svn *)

(********************************************************************)

module type Git = sig
  (* the output of: git describe --dirty --always --abbrev= *)
  val describe : string

  (* the output of: git log -1 --format=%ci *)
  val last_changed : string

  (* the output of: git status -suno *)
  val status   : string
end

module Make_git (Version: Git) = struct
  include Version

  let str : string =
    "Git hash: " ^ Version.describe ^ "\n\n" ^
    begin match String.trim Version.status with
    | ""     -> "Status: (clean)\n"
    | status -> "Status:\n" ^ status ^ "\n"
    end
end (* Make_git *)

(********************************************************************)

module Rmem    = Make_git(Version)
module Lem     = Make_git(Lem_version)
module Sail    = Make_git(Sail_version)
module Sail2   = Make_git(Sail2_version)
module Linksem = Make_git(Linksem_version)

let print : unit -> unit = fun () ->
  Printf.printf "Version: %s\n" Rmem.describe;

  if Structured_output.is_verbosity_at_least Structured_output.ThrottledInformation then begin
    Printf.printf "\nOCaml: %s\n\n" Version.ocaml;
    Printf.printf "** RMEM: ***********************************\n";
    Printf.printf "%s\n" Rmem.str;
    Printf.printf "** Lem: ************************************\n";
    Printf.printf "%s\n" Lem.str;
    Printf.printf "** Sail: ***********************************\n";
    Printf.printf "%s\n" Sail.str;
    Printf.printf "** Sail2: ***********************************\n";
    Printf.printf "%s\n" Sail2.str;
    Printf.printf "** linksem: ********************************\n";
    Printf.printf "%s\n" Linksem.str;
    Printf.printf "** libraries: ******************************\n";
    List.iter
      (fun (lib, ver) -> Printf.printf "%s: %s\n" lib ver)
      Version.libraries
  end

