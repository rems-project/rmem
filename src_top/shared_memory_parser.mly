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
module Base = Shared_memory_parser_base
%}

%token EOF

%token LEFT_BRACK
%token RIGHT_BRACK
%token SEMICOLON
%token FORWARD_SLASH
%token MINUS
%token PLUS

%token <Nat_big_num.num> NUM
%token <string> IDENT

%type <Shared_memory_parser_base.footprint list> footprints
%start footprints
%%

footprints:
  | EOF       { [] }
  | rev_footprints { List.rev $1 }
  ;

rev_footprints:
  | footprint                { [$1] }
  | rev_footprints footprint { $2 :: $1 }
  ;

footprint:
  | NUM SEMICOLON
    { Base.Absolute ($1, 1) }
  | NUM MINUS NUM SEMICOLON
    { let size = Nat_big_num.sub $3 $1 |> Nat_big_num.to_int |> (+) 1 in
      if size <= 0 then failwith "bad range"
      else Base.Absolute ($1, size)
    }
  | NUM offset_range SEMICOLON
    { Base.Absolute (Nat_big_num.add $1 (fst $2), snd $2) }
  | IDENT SEMICOLON
    { Base.Symbol ($1, None) }
  | IDENT offset_range SEMICOLON
    { Base.Symbol ($1, Some $2) }
  ;

offset_range: /* (offset, size) */
  | LEFT_BRACK NUM RIGHT_BRACK
    { ($2, 1) }
  | LEFT_BRACK NUM MINUS NUM RIGHT_BRACK
    { let size = Nat_big_num.sub $4 $2 |> Nat_big_num.to_int |> (+) 1 in
      if size <= 0 then failwith "bad range"
      else ($2, size)
    }
  | FORWARD_SLASH NUM
    { let size = Nat_big_num.to_int $2 in
      if size <= 0 then failwith "size must be greater than zero"
      else (Nat_big_num.zero, size)
    }
  | PLUS NUM FORWARD_SLASH NUM
    { let size = Nat_big_num.to_int $4 in
      if size <= 0 then failwith "size must be greater than zero"
      else ($2, size)
    }
  ;
