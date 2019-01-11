(*===============================================================================*)
(*                                                                               *)
(*                rmem executable model                                          *)
(*                =====================                                          *)
(*                                                                               *)
(*  This file is:                                                                *)
(*                                                                               *)
(*  Copyright Shaked Flur, University of Cambridge       2015-2017               *)
(*  Copyright Christopher Pulte, University of Cambridge 2017-2018               *)
(*  Copyright Susmit Sarkar, University of St Andrews         2015               *)
(*  Copyright Jon French, University of Cambridge             2017               *)
(*                                                                               *)
(*  All rights reserved.                                                         *)
(*                                                                               *)
(*  It is part of the rmem tool, distributed under the 2-clause BSD licence in   *)
(*  LICENCE.txt.                                                                 *)
(*                                                                               *)
(*===============================================================================*)

(* val exit_on_bad_fetch : Globals.ppmode -> 'ss MachineDefTypes.trans -> unit *)

(* The info structure of state of search procedure *)
type ('ts,'ss) info = {
  (* stashed name of the test *)
  testname:            string option;
  (* follow list as numbered choices *)
  trans_todo:          int list;
  (* numbered choices made so far *)
  choices_so_far:      int list;
  (* transitions made so far *)
  trans_so_far:        ('ts,'ss) MachineDefTypes.trans list;
  (* the preceding system state (one step up in search tree) *)
  last_system_state:   (('ts,'ss) MachineDefTypes.system_state) option;
  (* the user-interface numbered transitions from before, for text ui *)
  last_numbered_cands: (int * ('ts,'ss) MachineDefTypes.trans) list;
}

(* The state interaction manipulates *)
type ('ts,'ss) interaction_state = {
  (* The list of numbered transitions *)
  numbered_cands: (int * ('ts,'ss) MachineDefTypes.trans) list;
  make_graph:     unit -> unit;
  (* The info structure *)
  info:           ('ts,'ss) info;
}

val display : string -> string -> bool -> unit

(* main interaction function *)
val interact_with_user :
  RunOptions.t ->                  (* the current RunOptions *)
  Globals.ppmode ->                (* pretty printing mode *)
  ('ts,'ss) MachineDefTypes.system_state ->  (* system state that is being processed *)
  ('ts,'ss) MachineDefTypes.ui_system_state ->  (* the UI representation for the state above *)
  ('ts,'ss) interaction_state ->             (* interaction state on input *)
  (('ts,'ss) interaction_state -> unit) ->   (* continuation in the normal case, a function
                                    taking the new interaction state as input *)
  (RunOptions.t -> unit) ->        (* function to call if user wants to undo *)
  (RunOptions.t -> Globals.ppmode -> unit) ->
                                    (* recalculate transitions for the current state
                                    using the modified RunOptions *)
  unit

(* function called at end of interactive search, asking whether to quit or undo *)
val ask_quit_or_undo :
  (unit -> unit) -> (* function to call if user wants to undo *)
  unit
