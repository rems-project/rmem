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
/*  The rmem tool is distributed under the 2-clause BSD license in LICENCE.txt.  */
/*  For author information see README.md.                                        */
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

%token SILENCE

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
%token <string> RANDOM
%token EXHAUSTIVE

%token BREAK
%token FETCH
%token SYMBOL
%token LINE
%token WATCH_READ
%token WATCH_WRITE
%token WATCH_EITHER
%token SHARED

%token NAME

%token SET

%token FOCUS
%token THREAD
%token INSTRUCTION

%token <string> ON
%token <string> OFF
%token <string> NONE

%token INFO
%token DELETE


%token <int> NUM
%token <Nat_big_num.num> BIG_NUM
%type <int option> int_or_none

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
  |             { Base.Nothing       }
  | SILENCE     { Base.Silence       }
  | QUIT        { Base.Quit          }
  | help        { $1                 }
  | OPTIONS     { Base.ShowOptions   }
  | transitions { Base.Transitions $1 }
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

transitions:
  | NUM COMMA transitions { $1 :: $3 }
  | NUM                   { [$1] }
  ;

help:
  | HELP { Base.Help None }
  ;

info:
  | INFO BREAK { Base.InfoBreakpoints }
  ;

delete:
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
  | BREAK breakpoint_target { Base.BreakpointFetch $2      }
  | BREAK IDENT COLON NUM   { Base.BreakpointLine ($2, $4) }
  ;

watchpoint:
  | WATCH_READ   breakpoint_target { Base.Watchpoint (Base.Read,   $2) }
  | WATCH_WRITE  breakpoint_target { Base.Watchpoint (Base.Write,  $2) }
  | WATCH_EITHER breakpoint_target { Base.Watchpoint (Base.Either, $2) }
  | WATCH_READ   SHARED            { Base.SharedWatchpoint Base.Read     }
  | WATCH_WRITE  SHARED            { Base.SharedWatchpoint Base.Write    }
  | WATCH_EITHER SHARED            { Base.SharedWatchpoint Base.Either   }
  | BREAK        SHARED            { Base.SharedWatchpoint Base.Write    }
  ;

search:
  | SEARCH EXHAUSTIVE { Base.Search Base.Exhaustive  }
  | SEARCH RANDOM NUM { Base.Search (Base.Random $3) }
  ;

set:
  | SET set_key set_value { Base.SetOption ($2, $3) }
  ;

set_key:
  | IDENT { $1 }
  | RANDOM { $1 }
  ;

/* Slight hack to pass strings through to 'set ...' */
set_value:
  | IDENT { $1 }
  | ON    { $1 }
  | OFF   { $1 }
  | NONE  { $1 }
  | NUM   { string_of_int $1 }
  | set_value COMMA set_value { $1 ^ "," ^ $3 }
  ;

focus:
  | FOCUS THREAD NUM { Base.FocusThread (Some $3) }
  | FOCUS THREAD OFF { Base.FocusThread     None  }
  | FOCUS INSTRUCTION NUM NUM       { Base.FocusInstruction (Some ($3, $4)) }
  | FOCUS INSTRUCTION NUM COLON NUM { Base.FocusInstruction (Some ($3, $5)) }
  | FOCUS INSTRUCTION OFF           { Base.FocusInstruction           None  }
  ;

int_or_none:
  | NONE { None }
  | NUM { Some $1 }

on_off:
  | ON  { true  }
  | OFF { false }
  ;

big_num:
  | NUM     { Nat_big_num.of_int $1 }
  | BIG_NUM { $1 }
  ;

stepi:
  | STEPI               { Base.StepInstruction (   None,    None) }
  | STEPI NUM           { Base.StepInstruction (Some $2,    None) }
  | STEPI NUM NUM       { Base.StepInstruction (Some $2, Some $3) }
  | STEPI NUM COLON NUM { Base.StepInstruction (Some $2, Some $4) }
  ;

peeki:
  | PEEKI NUM NUM { Base.PeekInstruction ($2, $3) }
  ;
