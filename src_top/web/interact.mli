(*===============================================================================*)
(*                                                                               *)
(*                rmem executable model                                          *)
(*                =====================                                          *)
(*                                                                               *)
(*  This file is:                                                                *)
(*                                                                               *)
(*  Copyright Shaked Flur, University of Cambridge 2015-2017                     *)
(*                                                                               *)
(*  All rights reserved.                                                         *)
(*                                                                               *)
(*  It is part of the rmem tool, distributed under the 2-clause BSD licence in   *)
(*  LICENCE.txt.                                                                 *)
(*                                                                               *)
(*===============================================================================*)

open Js_of_ocaml

(* val exit_on_bad_fetch : Globals.ppmode -> MachineDefTypes.trans -> unit *)

(* The info structure of state of search procedure *)
type info = {
  (* stashed name of the test *)
  testname:            string option;
  (* follow list as numbered choices *)
  trans_todo:          int list;
  (* numbered choices made so far *)
  choices_so_far:      int list;
  (* transitions made so far *)
  trans_so_far:        MachineDefTypes.trans list;
  (* the preceding system state (one step up in search tree) *)
  last_system_state:   MachineDefTypes.system_state option;
  (* the user-interface numbered transitions from before, for text ui *)
  last_numbered_cands: (int * MachineDefTypes.trans) list;
}

(* The state interaction manipulates *)
type interaction_state = {
  (* The list of numbered transitions *)
  numbered_cands: (int * MachineDefTypes.trans) list;
  (* The info structure *)
  info:           info;
}

val display : string -> string -> bool -> unit

(* main interaction function *)
val interact_with_user :
  RunOptions.t ->                  (* the current RunOptions *)
  Globals.ppmode ->                (* pretty printing mode *)
  MachineDefTypes.system_state ->  (* system state that is being processed *)
  interaction_state ->             (* interaction state on input *)
  (interaction_state -> unit) ->   (* continuation in the normal case, a function
                                    taking the new interaction state as input *)
  (RunOptions.t -> unit) ->        (* function to call if user wants to undo *)
  (unit -> unit) ->                (* function to call if user wants to quit *)
  (RunOptions.t -> Globals.ppmode -> unit) ->
                                    (* recalculate transitions for the current state
                                    using the modified RunOptions *)
  unit

(* function called at end of interactive search, asking whether to quit or undo *)
val ask_quit_or_undo :
  (unit -> unit) -> (* function to call if user wants to undo *)
  (unit -> unit) -> (* function to call if user wants to quit *)
  unit


(** web interface only (i.e. not in src/text/interact.mli) *)

val createButton : Dom_html.document Js.t -> Dom_html.inputElement Js.t

val init : Dom_html.divElement Js.t -> Dom_html.divElement Js.t -> unit
val start : RunOptions.t -> (unit -> unit) -> unit
