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

{
open Lexing
open Branch_targets_parser

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
let decimal = digit+
let hexadecimal = ("0x"|"0X") hexadigit+
let num = decimal | hexadecimal

let alpha = ['a'-'z' 'A'-'Z']
let name  = (alpha | '_' | '.') (alpha | digit | '_' | '/' | '.' | '$')*

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule read =
  parse
  | white   { read lexbuf }
  | newline { next_line lexbuf; read lexbuf }

  | '#' { skip_comment lexbuf; read lexbuf }

  | '{'  { LEFT_BRACE }
  | '}'  { RIGHT_BRACE }
  | ':'  { COLON }
  | ';'  { SEMICOLON }
  | ','  { COMMA }
  | "->" { ARROW }
  | '+'  { PLUS  }
  | '-'  { MINUS }

  | num as n  { NUM (Misc.big_num_of_string n) }
  | name as s { IDENT s }

  | _   { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof { EOF }

and skip_comment = parse
  | newline { next_line lexbuf }
  | eof     {}
  | _       { skip_comment lexbuf}
