/*===============================================================================*/
/*                                                                               */
/*                rmem executable model                                          */
/*                =====================                                          */
/*                                                                               */
/*  This file is:                                                                */
/*                                                                               */
/*  Copyright Shaked Flur, University of Cambridge 2017                          */
/*                                                                               */
/*  All rights reserved.                                                         */
/*                                                                               */
/*  It is part of the rmem tool, distributed under the 2-clause BSD licence in   */
/*  LICENCE.txt.                                                                 */
/*                                                                               */
/*===============================================================================*/

%{
module Base = Branch_targets_parser_base
%}

%token EOF

%token LEFT_BRACE
%token RIGHT_BRACE
%token LEFT_PAREN
%token RIGHT_PAREN
%token COLON
%token SEMICOLON
%token COMMA
%token ARROW

%token PLUS
%token MINUS

%token <Nat_big_num.num> NUM
%token <string> IDENT

%type <Branch_targets_parser_base.ast list> lines
%start lines
%%

lines:
  | EOF       { [] }
  | rev_lines { List.rev $1 }
  ;

rev_lines:
  | line           { [$1] }
  | rev_lines line { $2 :: $1 }
  ;

line:
  | NUM COLON location ARROW LEFT_BRACE locations RIGHT_BRACE SEMICOLON
      { { Base.thread         = Nat_big_num.to_int $1;
          Base.branch_loc     = $3;
          Base.branch_targets = $6;
        }
      }
  ;

locations:
  | rev_locations { List.rev $1 }
  ;

rev_locations:
  | location                     { [$1] }
  | rev_locations COMMA location { $3 :: $1 }
  ;

location:
  | NUM             { Base.Absolute $1 }
  | IDENT           { Base.Label_and_offset ($1, Nat_big_num.zero) }
  | IDENT PLUS NUM  { Base.Label_and_offset ($1, $3) }
  | IDENT MINUS NUM { Base.Label_and_offset ($1, Nat_big_num.negate $3) }
  | location LEFT_PAREN IDENT RIGHT_PAREN { $1 }
  ;
