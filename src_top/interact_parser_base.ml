(*===============================================================================*)
(*                                                                               *)
(*                rmem executable model                                          *)
(*                =====================                                          *)
(*                                                                               *)
(*  This file is:                                                                *)
(*                                                                               *)
(*  Copyright Jon French, University of Cambridge  2016-2018                     *)
(*  Copyright Shaked Flur, University of Cambridge 2016-2017                     *)
(*                                                                               *)
(*  All rights reserved.                                                         *)
(*                                                                               *)
(*  It is part of the rmem tool, distributed under the 2-clause BSD licence in   *)
(*  LICENCE.txt.                                                                 *)
(*                                                                               *)
(*===============================================================================*)

exception Parse_error of string

type ast_search =
  | Random of int
  | Exhaustive

type ast_search_final =
  | Any_final
  | Final_ok
  | Final_not_ok

type ast_breakpoint_target =
  | Address of Nat_big_num.num
  | Footprint of Nat_big_num.num * int
  | Symbol of string * int

type ast_watchpoint_type =
  | Read
  | Write
  | Either

type transition_choice =
  | WithEager of int
  (* 'WithBoundedEager (t,e)' t is the transition, e is the number of eager steps after *)
  | WithBoundedEager of int * int

type ast =
  | Quit
  | Help of string option
  | ShowOptions
  | Debug of string
  | Default
  | Transition of transition_choice
  | Step of int option
  | StepInstruction of int option * int option
  | PeekInstruction of int * int
  | Back of int option
  | Undo
  | Follow
  | Auto
  | Typeset
  | Graph
  | Print
  | History
  | FetchAll
  | BreakpointFetch of ast_breakpoint_target
  | Watchpoint of (ast_watchpoint_type * ast_breakpoint_target)
  | SharedWatchpoint of ast_watchpoint_type
  | BreakpointLine of string * int
  | SetOption of string * string
  | FocusThread of int option
  | FocusInstruction of Events.ioid option
  | Search of ast_search * (ast_search_final option)
  | InfoBreakpoints
  | DeleteBreakpoint of int

let pp_ast_search : ast_search -> string = function
  | Random i   -> Printf.sprintf "random %d" i
  | Exhaustive -> "exhaustive"

let pp_ast_search_final : ast_search_final -> string = function
  | Any_final    -> "final"
  | Final_ok     -> "final_ok"
  | Final_not_ok -> "final_not_ok"

let pp_on_off : bool -> string = function
  | true  -> "on"
  | false -> "off"

let pp_int_option : int option -> string = function
  | None   -> ""
  | Some i -> Printf.sprintf "%d" i

let pp_breakpoint_target : ast_breakpoint_target -> string = function
  | Address a -> Printf.sprintf "0x%s" (Misc.big_num_to_hex_string a)
  | Footprint (a, s) -> Printf.sprintf "0x%s/%d" (Misc.big_num_to_hex_string a) s
  | Symbol (name, offset) ->
     if offset = 0 then
       name
     else if offset > 0 then
       Printf.sprintf "%s+%d" name offset
     else (* offset < 0 *)
       Printf.sprintf "%s%d" name offset

let pp_transition_choice : transition_choice -> string = function
  | WithEager t             -> string_of_int t
  | WithBoundedEager (t, e) -> Printf.sprintf "%de%d" t e

(* PP the second argument of 'set' *)
let pp_string str =
  if String.contains str ';'
  || String.contains str ' '
  || String.escaped str <> str (* we only really care about double quote and backslash *)
  then
    Printf.sprintf "%S" str (* '%S' adds double quotes and escapes *)
  else str

let pp : ast -> string = function
  | Quit              -> "quit"
  | Help None         -> "help"
  | Help (Some s)     -> Printf.sprintf "help %s" s
  | ShowOptions       -> "options"
  | Debug s           -> "debug " ^ s
  | Default           -> ""
  | Transition t      -> (pp_transition_choice t)
  | Step i            -> Printf.sprintf "step %s" (pp_int_option i)
  | StepInstruction (maybe_tid, maybe_iid) -> begin
      match (maybe_tid, maybe_iid) with
      | (None, Some _) ->
         assert false
      | (Some tid, None) ->
         Printf.sprintf "stepi %d" tid
      | (Some tid, Some iid) ->
         Printf.sprintf "stepi %d:%d" tid iid
      | (None, None) ->
         "stepi"
    end
  | PeekInstruction (i, j) -> Printf.sprintf "peeki %d %d" i j
  | Back i            -> Printf.sprintf "back%s" (pp_int_option i)
  | Undo              -> "undo"
  | Follow            -> "follow"
  | Auto             -> "auto"
  | Typeset           -> "typeset"
  | Graph             -> "graph"
  | Print             -> "print"
  | History           -> "history"
  | FetchAll          -> "fetch"
  | BreakpointFetch target -> Printf.sprintf "break %s" (pp_breakpoint_target target)
  | Watchpoint (typ, target) ->
     let prefix = (match typ with
                   | Read -> "r"
                   | Write -> ""
                   | Either -> "a"
                  ) in
     Printf.sprintf "%swatch %s" prefix (pp_breakpoint_target target)
  | SharedWatchpoint typ ->
     let prefix = (match typ with
                   | Read -> "r"
                   | Write -> ""
                   | Either -> "a"
                  ) in
     Printf.sprintf "%swatch shared" prefix
  | BreakpointLine (filename, line) -> Printf.sprintf "break %s:%d" filename line
  | SetOption (key, value) -> Printf.sprintf "set %s %s" key (pp_string value)
  | FocusThread maybe_thread -> (match maybe_thread with
                                 | None -> "focus thread off"
                                 | Some thread -> (Printf.sprintf "focus thread %d" thread))
  | FocusInstruction maybe_ioid -> (match maybe_ioid with
                                    | None -> "focus instruction off"
                                    | Some (tid, iid) -> (Printf.sprintf "focus instruction (%d:%d)" tid iid))
  | Search (s, None)   -> Printf.sprintf "search %s" (pp_ast_search s)
  | Search (s, Some f) -> Printf.sprintf "search %s %s" (pp_ast_search s) (pp_ast_search_final f)
  | InfoBreakpoints    -> "info break"
  | DeleteBreakpoint n -> Printf.sprintf "delete break %d" n

let history_to_string history : string =
  List.rev history
  |> List.map pp
  |> String.concat ";"
