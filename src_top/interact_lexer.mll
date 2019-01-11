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

{
open Lexing
open Interact_parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}

let digit = ['0'-'9']
let hexadigit = ['0'-'9' 'a'-'f' 'A'-'F']
let decimal = '-' ? digit+
let hexadecimal = ("0x"|"0X") hexadigit+
let num = decimal | hexadecimal

let alpha = ['a'-'z' 'A'-'Z']
let name  = (alpha | '_') (alpha | digit | '_' | '/' | '.' | '-')*

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule read =
  parse
  | white    { read lexbuf }
  | newline  { next_line lexbuf; read lexbuf }

  | '{'      { LEFT_BRACE }
  | '}'      { RIGHT_BRACE }
  | '['      { LEFT_BRACK }
  | ']'      { RIGHT_BRACK }
  | ':'      { COLON }
  | ';'      { SEMICOLON }
  | ','      { COMMA }
  | '/'      { FORWARD_SLASH }
  | '+'      { PLUS  }
  | '-'      { MINUS }

  | num as n  { try NUM (int_of_string n) with
                | Failure _ -> BIG_NUM (Misc.big_num_of_string n)
              }

  | "silence"              { SILENCE }

  | "x"  | "exit"
  | "q"  | "quit"          { QUIT }

  | "?"
  | "h"  | "help"          { HELP }

  | "options"              { OPTIONS }

  | "s"  | "step"          { STEP }
  | "b"  | "back"          { BACK }
  | "u"  | "undo"          { UNDO }

  | "f"  | "follow"        { FOLLOW }
  | "a"  | "auto"          { AUTO }

  | "search"               { SEARCH }
  | "ran"  | "random"      { RANDOM (Lexing.lexeme lexbuf) }
  | "exh"  | "exhaustive"  { EXHAUSTIVE }

  | "typeset"              { TYPESET }
  | "graph"                { GRAPH }

  | "p"   | "print"        { PRINT }
  | "his" | "history"      { HISTORY }

  | "debug"                { DEBUG }

  | "break" | "breakpoint" { BREAK }
  | "fetch"                { FETCH }
  | "symbol" | "sym"       { SYMBOL }
  | "line"                 { LINE }
  | "rwatch"               { WATCH_READ }
  | "watch"                { WATCH_WRITE }
  | "awatch"               { WATCH_EITHER }
  | "shared"               { SHARED }

  | "name"                 { NAME }

  | "si" | "stepi"         { STEPI }
  | "peeki"                { PEEKI }

  | "set"                  { SET }

  | "focus"                { FOCUS }
  | "thread"               { THREAD }
  | "instruction"
  | "inst" | "ioid"        { INSTRUCTION }

                                (* Slight hack to pass strings through to 'set ...' *)
  | "on" | "true" | "1"
  | "t" | "yes" | "y"      { ON (Lexing.lexeme lexbuf) }
  | "off" | "false" | "0"
  | "f" | "no" | "n"       { OFF (Lexing.lexeme lexbuf) }

  | "none"                 { NONE  (Lexing.lexeme lexbuf) }

  | "info"                 { INFO }
  | "delete"               { DELETE }

  | name as s              { IDENT s }

  | _        { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof      { EOF }
