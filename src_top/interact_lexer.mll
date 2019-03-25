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

type state = LS_Token | LS_Set_args
let state = ref LS_Token

let create_hashtable keywords =
  let size = List.fold_left (fun c (keys, _) -> c + List.length keys) 0 keywords in
  let tbl = Hashtbl.create size in
  List.iter (fun (keys, tok) -> (List.iter (fun key -> Hashtbl.add tbl key tok) keys)) keywords;
  tbl

let keywords =
  create_hashtable [
    (["x"; "exit"; "q"; "quit"], QUIT);
    (["h"; "help"],              HELP);
    (["o"; "options"],           OPTIONS);

    (["s"; "step"], STEP);
    (["b"; "back"], BACK);
    (["u"; "undo"], UNDO);

    (["fo"; "follow"], FOLLOW);
    (["a"; "auto"],    AUTO);

    (["search"],            SEARCH);
    (["ran"; "random"],     RANDOM);
    (["exh"; "exhaustive"], EXHAUSTIVE);
    (["final"],             FINAL Interact_parser_base.Any_final);
    (["final_ok"],          FINAL Interact_parser_base.Final_ok);
    (["final_not_ok"],      FINAL Interact_parser_base.Final_not_ok);

    (["typeset"], TYPESET);
    (["graph"],   GRAPH);

    (["p"; "print"],     PRINT);
    (["his"; "history"], HISTORY);

    (["debug"], DEBUG);

    (["break"; "breakpoint"], BREAK);
    (["fetch"],               FETCH);
    (["symbol"; "sym"],       SYMBOL);
    (["line"],                LINE);
    (["rwatch"],              WATCH_READ);
    (["watch"],               WATCH_WRITE);
    (["awatch"],              WATCH_EITHER);
    (["shared"],              SHARED);

    (["on";  "true";  "t"; "yes"; "y"], ON);
    (["off"; "false"; "f"; "no";  "n"], OFF);

    (["si"; "stepi"], STEPI);
    (["peeki"],       PEEKI);

    (["focus"], FOCUS);
    (["thread"], THREAD);
    (["instruction"; "inst"; "ioid"], INSTRUCTION);

    (["info"], INFO);
    (["delete"], DELETE);
  ]

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

rule token =
  parse
  | white    { token lexbuf }
  | newline  { next_line lexbuf; token lexbuf }

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
  | '?'      { QUESTION }

  | num as n  { try NUM (int_of_string n) with
                | Failure _ -> BIG_NUM (Misc.big_num_of_string n)
              }

  | (num as n1) 'e' (num as n2)
              { NUMENUM (int_of_string n1, int_of_string n2) }

  | "set" { state := LS_Set_args; SET }

  | name as s {
      try Hashtbl.find keywords s with
      | Not_found -> IDENT s
    }

  | _        { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof      { EOF }

and read_set_args =
  parse
  | white     { read_set_args lexbuf }
  | newline   { next_line lexbuf; read_set_args lexbuf }

  | ';'       { state := LS_Token; SEMICOLON }

  | '"'       { SETARG (read_string (Buffer.create 50) lexbuf) }

  | (alpha | digit | '_' | '/' | '.' | '-')+ as s
              { SETARG s }

  | _         { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof       { state := LS_Token; EOF }

and read_string buf =
  parse
  | '"'           { Scanf.unescaped (Buffer.contents buf) }
  | '\\' _        { Buffer.add_string buf (Lexing.lexeme lexbuf); read_string buf lexbuf }
  | [^ '"' '\\']+ { Buffer.add_string buf (Lexing.lexeme lexbuf); read_string buf lexbuf }
  | _             { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof           { raise (SyntaxError ("String is not terminated")) }

{
  let internal_lexer = fun lb ->
    match !state with
    | LS_Token    -> token lb
    | LS_Set_args -> read_set_args lb

  let get_lexer = fun () ->
    state := LS_Token;
    internal_lexer
}

