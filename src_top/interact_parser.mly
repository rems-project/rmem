/*===============================================================================*/
/*                                                                               */
/*                rmem executable model                                          */
/*                =====================                                          */
/*                                                                               */
/*  This file is:                                                                */
/*                                                                               */
/*  Copyright Jon French, University of Cambridge  2016-2018                     */
/*  Copyright Shaked Flur, University of Cambridge 2016-2017                     */
/*                                                                               */
/*  All rights reserved.                                                         */
/*                                                                               */
/*  It is part of the rmem tool, distributed under the 2-clause BSD licence in   */
/*  LICENCE.txt.                                                                 */
/*                                                                               */
/*===============================================================================*/

%{
module Base = Interact_parser_base
%}

%token EOF

%token LEFT_BRACE
%token RIGHT_BRACE
%token LEFT_BRACK
%token RIGHT_BRACK
%token COLON
%token SEMICOLON
%token COMMA
%token FORWARD_SLASH
%token PLUS
%token MINUS
%token QUESTION

%token QUIT
%token HELP
%token OPTIONS

%token STEP
%token STEPI
%token PEEKI
%token BACK
%token UNDO
%token FOLLOW
%token AUTO
%token TYPESET
%token GRAPH
%token PRINT
%token HISTORY
%token DEBUG

%token SEARCH
%token RANDOM
%token EXHAUSTIVE
%token <Interact_parser_base.ast_search_final> FINAL

%token BREAK
%token FETCH
%token SYMBOL
%token LINE
%token <Interact_parser_base.ast_watchpoint_type> WATCH
%token SHARED

%token SET
%token <string> ARG

%token FOCUS
%token THREAD
%token INSTRUCTION

%token ON
%token OFF

%token INFO
%token DELETE

%token <int> NUM
%token <Nat_big_num.num> BIG_NUM
%token <int * int> NUMENUM
%token <string> IDENT

%type <Interact_parser_base.ast list> commands
%start commands
%%

commands:
  | EOF                    { [Base.Default] }
  | rev_commands           { List.rev $1 }
  ;

rev_commands:
  | command                        { [$1] }
  | rev_commands SEMICOLON command { $3 :: $1 }
  ;

command:
  |             { Base.Default }
  | QUIT        { Base.Quit          }
  | help        { $1                 }
  | OPTIONS     { Base.ShowOptions   }
  | transition  { Base.Transition $1 }
  | step        { $1                 }
  | stepi       { $1                 }
  | peeki       { $1                 }
  | back        { $1                 }
  | UNDO        { Base.Undo          }
  | FOLLOW      { Base.Follow        }
  | AUTO        { Base.Auto          }
  | TYPESET     { Base.Typeset       }
  | GRAPH       { Base.Graph         }
  | PRINT       { Base.Print         }
  | HISTORY     { Base.History       }
  | DEBUG IDENT { Base.Debug $2      }
  | FETCH       { Base.FetchAll      }
  | breakpoint  { $1                 }
  | watchpoint  { $1                 }
  | set         { $1                 }
  | search      { $1                 }
  | info        { $1                 }
  | delete      { $1                 }
  | focus       { $1                 }
  ;

transition:
  | NUM      { Base.WithEager $1 }
  | NUMENUM  { Base.WithBoundedEager (fst $1, snd $1) }
  ;

help:
  | HELP          { Base.Help [] }
  | QUESTION      { Base.Help [] }
  | HELP rev_args { Base.Help (List.rev $2) }
  ;

info:
  | INFO { raise (Base.Parse_error "'info' takes arguments (see 'help info')") }
  | INFO BREAK { Base.InfoBreakpoints }
  ;

delete:
  | DELETE { raise (Base.Parse_error "'delete' takes arguments (see 'help delete')") }
  | DELETE BREAK NUM     { Base.DeleteBreakpoint $3 }
  | DELETE NUM           { Base.DeleteBreakpoint $2 }
  ;

step:
  | STEP     { Base.Step None      }
  | STEP NUM { Base.Step (Some $2) }
  ;

back:
  | BACK     { Base.Back None      }
  | BACK NUM { Base.Back (Some $2) }
  ;

symbol_offset_expr:
  | IDENT           { Base.Symbol ($1,  0) }
  | IDENT PLUS  NUM { Base.Symbol ($1, $3) }
  | IDENT MINUS NUM { Base.Symbol ($1, $3) }
  ;

breakpoint_target:
  | big_num                   { Base.Address    $1      }
  | big_num FORWARD_SLASH NUM { Base.Footprint ($1, $3) }
  | symbol_offset_expr        { $1                      }
  ;

breakpoint:
  | BREAK { raise (Base.Parse_error "'break' takes arguments (see 'help break')") }
  | BREAK breakpoint_target { Base.BreakpointFetch $2      }
  | BREAK IDENT COLON NUM   { Base.BreakpointLine ($2, $4) }
  ;

watchpoint:
  | WATCH { raise (Base.Parse_error "'watch' takes arguments (see 'help watch')") }
  | WATCH breakpoint_target { Base.Watchpoint ($1, $2) }
  | WATCH SHARED            { Base.SharedWatchpoint $1 }
  ;

search:
  | SEARCH { raise (Base.Parse_error "'search' takes arguments (see 'help search')") }
  | SEARCH EXHAUSTIVE       { Base.Search (Base.Exhaustive, None)    }
  | SEARCH EXHAUSTIVE FINAL { Base.Search (Base.Exhaustive, Some $3) }
  | SEARCH RANDOM NUM       { Base.Search (Base.Random $3, None)     }
  | SEARCH RANDOM NUM FINAL { Base.Search (Base.Random $3, Some $4)  }
  ;

set:
  | SET { raise (Base.Parse_error "'set' takes arguments (see 'help set')") }
  | SET ARG          { Base.SetOption ($2, []) }
  | SET ARG rev_args { Base.SetOption ($2, List.rev $3) }
  ;

rev_args:
  | ARG          { $1 :: [] }
  | rev_args ARG { $2 :: $1 }
  ;

focus:
  | FOCUS { raise (Base.Parse_error "'focus' takes arguments (see 'help focus')") }
  | FOCUS THREAD NUM       { Base.FocusThread (Some $3)      }
  | FOCUS THREAD OFF       { Base.FocusThread None           }
  | FOCUS INSTRUCTION ioid { Base.FocusInstruction (Some $3) }
  | FOCUS INSTRUCTION OFF  { Base.FocusInstruction None      }
  ;

big_num:
  | NUM     { Nat_big_num.of_int $1 }
  | BIG_NUM { $1 }
  ;

stepi:
  | STEPI               { Base.StepInstruction (   None, None) }
  | STEPI NUM           { Base.StepInstruction (Some $2, None) }
  | STEPI ioid          { Base.StepInstruction (Some (fst $2), Some (snd $2)) }
  ;

peeki:
  | PEEKI { raise (Base.Parse_error "'peeki' takes arguments (see 'help peeki')") }
  | PEEKI ioid { Base.PeekInstruction (fst $2, snd $2) }
  ;

ioid:
  | NUM NUM       { ($1, $2) }
  | NUM COLON NUM { ($1, $3) }
  ;
