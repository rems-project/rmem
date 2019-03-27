(*===============================================================================*)
(*                                                                               *)
(*                rmem executable model                                          *)
(*                =====================                                          *)
(*                                                                               *)
(*  This file is:                                                                *)
(*                                                                               *)
(*  Copyright Jon French, University of Cambridge  2017-2018                     *)
(*  Copyright Shaked Flur, University of Cambridge      2017                     *)
(*                                                                               *)
(*  All rights reserved.                                                         *)
(*                                                                               *)
(*  It is part of the rmem tool, distributed under the 2-clause BSD licence in   *)
(*  LICENCE.txt.                                                                 *)
(*                                                                               *)
(*===============================================================================*)

let help_message = {help_string|
================================================================================
RMEM console help
================================================================================

Multiple commands may be semicolon-separated on one line.
The command line indicates in [square brackets] the default
command to be taken if enter is pressed on an empty command line.

Notation
--------------------------------------------------------------------------------
(... | ...)    represents alternatives
[...]          represents an optional section
<N[n]>         represents a positive integer (including zero) in either decimal
                 or (0x prefixed) hex
<ioid>         represents instruction instance ID. This can be entered as two
                 integers, separated by colon or space, where the first one is
                 the thread number and the second one is the instruction number
                 within the thread (e.g. '0:1' or '0 1' both represent the first
                 instruction of thread 0).
<string[n]>    represents an arbitrary string containing no spaces
<bool[n]>      represents a boolean. Accepted values:
                 true = 'true', 'on', '1', 't', 'yes', 'y'
                 false = 'false', 'off', '0', 'f', 'no', 'n'

Stepping commands
--------------------------------------------------------------------------------
<N>                              Take the transition with the given number.
(step|s) [<N>]                   Take the default transition <N> times,
                                   default once.
(back|b) [<N>]                   Step back <N> previously taken transitions,
                                   default one.
auto | a                         Repeatedly take the default transition until
                                   no more enabled transitions exist.
                                   Analogous to 'holding down enter'.

set follow_list "<N>[;<N>...]"   Set the list of upcoming transitions.
follow | fo                      Repeatedly take transitions from the follow-list.

fetch                            Take all fetch transitions. (note, this may
                                   not terminate if the test has loops)

set random <bool>                Enable or disable the default transition being
                                   pseudorandomly chosen.
set storage_first <bool>         Enable or disable whether storage transitions
                                   are preferentially taken first when stepping.

focus thread (<N>|off)           Enable only transitions for the given thread
                                   id, or 'off' for all threads
focus instruction (<ioid>|off)   Enable only transitions for the given
                                   instruction ioid, or 'off' for all
                                   instructions
(stepi|si) [<N>|<ioid>]          Take transitions of the specified instruction
                                   until it is finished, if possible. The
                                   specified instruction is either <ioid>, the
                                   first instruction of <N> that has enabled
                                   transitions, or the first instruction with
                                   enabled transitions of the first thread with
                                   such instruction.
peeki <ioid>                     Show all the states instruction <ioid> can
                                   reach by taking only <ioid> transitions. Each
                                   state of <ioid> will be printed with the
                                   sequence of transitions that leads to it
                                   below.

Searching commands
--------------------------------------------------------------------------------
search (exhaustive|random <N>) [final|final_ok|final_not_ok]
                                 Run an exhaustive search, or <N> pseudorandom
                                   linear searches, through the graph of
                                   possible states, looking for final states
                                   in which no more transitions are possible,
                                   and print a histogram of the reachable
                                   final states. If a breakpoint is hit
                                   during the search, it will stop and jump
                                   to the breakpoint-matching state.
                                   For exhaustive search, one usually wants
                                   hash_prune and forbid_tree_speculation.

set hash_prune <bool>            Enable or disable using hashing of states to
                                   avoid visiting a state's subtree more than
                                   once in a search
set partial_order_reduction <bool>
                                 Enable or disable restricting searches
                                   according to the partial order reduction
set allow_partial <bool>         Enable or disable setting a signal handler to
                                   allow interrupting a search and returning
                                  partial results
set compare_analyses <bool>      Enable or disable comparing the handwritten and
                                   exhaustive analyses
set priority_reduction <bool>    Enable or disable taking certain kinds of
                                   transition preferentially by disabling all
                                   others when they exist. Currently:
                                   exclusives, multiple-successor fetches
set prune_restarts <bool>        Enable or disable discounting traces with
                                   instruction restarts when searching (this is
                                   safe for searching because of the proven
                                   axiomatic/operational equivalence)
set prune_discards <bool>        Enable or disable discounting traces with
                                   instruction discards when searching (same.
                                   Requires forbid_tree_speculation)

set transition_limit (<N>|none)  Set the transition limit on searches
set trace_limit (<N>|none)       Set the trace limit on searches
set time_limit (<N>|none)        Set the time limit on searches in seconds

  In the 'set foo_limit ...' commands, 'none' means no limit (the default.)

set branch-targets "<bt-map>"    EXPERIMENTAL. Sets the initial approximation of
                                   branch targets for branch-register
                                   instructions (will be extended by the search
                                   until a fixed-point is reached)

Breakpoint commands
--------------------------------------------------------------------------------
break <N>             Insert a breakpoint that will fire when the given address
                        can be fetched.
break <string>
break <string>+<N>
break <string>-<N>    Insert a breakpoint that will fire when the address of the
                        given symbol, optionally plus or minus an offset, can be
                        fetched.
break <string>:<N>    Insert a breakpoint that will fire when the given source
                        line can be fetched.

[a|r]watch <N>        Insert a watchpoint of size one byte at the given address
[a|r]watch <N1>/<N2>  Insert a watchpoint of size <N2> at address <N1>
[a|r]watch <string>
[a|r]watch <string>+<N>
[a|r]watch <string>-<N>
                      Insert a watchpoint of size one byte at the given symbol,
                        optionally plus or minus an offset
[a|r]watch shared     Insert a watchpoint that triggers on all locations which
                        have been recorded as 'shared'

  For all watchpoint commands, the type of watchpoint inserted
  is determined by the prefix of the command:
      if no prefix is given, a write watchpoint;
      or if 'r' is prefixed, a read watchpoint;
      or if 'a' is prefixed, an access watchpoint (either read or write.)

info break            Print all the currently active breakpoints
delete [break] <N>    Delete the breakpoint with the given number

Eager options
--------------------------------------------------------------------------------
When exploring interactively, these options control which transitions are
considered 'uninteresting' and eagerly taken, which should not affect the set
of observable behaviours because these are all (believed to be...)
confluent with each other.

set eager_fetch_single <bool>    Fetch transitions with a single successor
set eager_fetch_multi <bool>     Fetch transitions with multiple successors
                                   (i.e., branches. Can cause infinite loops)
set eager_pseudocode_internal <bool>
                                 Pseudocode-internal transitions
set eager_constant_reg_read <bool>
                                 Constant register reads (e.g. pseudo-regs.)
set eager_reg_rw <bool>          Register reads and writes
set eager_memory_aux <bool>      Memory auxiliary transitions (e.g. flowing
                                   events, barriers, commits)
set eager_finish <bool>          Instruction finish transitions
set eager_fp_recalc <bool>       PLDI11 footprint recalculation transitions
set eager_thread_start <bool>    Thread start transitions

set eager <bool>                 Shortcut to enable/disable all of the above at
                                   once, except that 'set eager on' does not
                                   enable eager_fetch_multi (as that can cause
                                   infinite loops).

set eager_local_mem <bool>       EXPERIMENTAL. Affects searching only. Eagerly
                                   take thread-local memory access transitions
                                   using an approximation of the shared memory
                                   footprint. The approximation is refined at
                                   the end of the search, and another search is
                                   started, until a fixed-point is reached.
set shared-memory "<footprints>" EXPERIMENTAL. Sets the initial approximation of
                                   the shared memory footprint for
                                   eager_local_mem (will be extended by the
                                   search until a fixed-point is reached).
Interface options
--------------------------------------------------------------------------------
set pp_style (full|compact|screenshot)
                                 Set the printing style
set verbosity (quiet|normal|verbose|very|debug)
                                 Set the verbosity of command line output
set choice_history_limit (<N>|none)
                                 Set the maximum number of 'choices so far'
                                   printed with the current state, 'none' for no
                                   limit.
set always_print <bool>          Enable or disable printing the current state
                                   after every command.
set prefer_symbolic_values <bool>
                                 Enable or disable preferring symbolic values to
                                 hex in output.
set hide_pseudoregister_reads <bool>
                                 Hide or show pseudoregister reads in output
set max_finished (<N>|none)      Set the maximum number of finished instructions
                                   printed at the start of each thread.
set pp_colours <bool>            Enable or disable colourised output
set dumb_terminal <bool>         Enable or disable dumb terminal mode which
                                   disables readline, cursor movement, etc.
set condense_finished_instructions <bool>
                                 Enable or disable condensing finished
                                   instructions in printed states.
set pp_hex <bool>                Enable/disable printing histogram values in hex
set dwarf_show_all_variable_locations <bool>
                                 Enable or disable showing all DWARF variable
                                   location data at each instruction
set pp_sail <bool>               Enable or disable showing the Sail interpreter
                                   state and code for instructions (if
                                   interpreter enabled).

set state_output <file>          Print the current state, every time it changes,
                                   to <file>.
set trace_output <file>          Print the current trace, every time it changes,
                                   to <file>.

Graph options
--------------------------------------------------------------------------------
set always_graph  <bool>  Enable/disable generate a .dot or onscreen transition
                            graph after every command
set ppg_shared <bool>     Enable/disable showing *only* shared-memory
                            instructions in graph output
set ppg_rf <bool>         Enable/disable showing read-from (rf) edges
set ppg_fr <bool>         Enable/disable showing from-read (fr) edges
set ppg_co <bool>         Enable/disable showing coherence (co) edges
set ppg_addr <bool>       Enable/disable showing address dependency (addr) edges
set ppg_data <bool>       Enable/disable showing data dependency (data) edges
set ppg_ctrl <bool>       Enable/disable showing control dependency (ctrl) edges
set ppg_regs <bool>       Enable/disable showing registers in graph output
set ppg_reg_rf <bool>     Enable/disable showing rf edges in graph output
set ppg_trans <bool>      Enable/disable showing transitions in graph output
set graph_backend (dot|tikz)
                          Set the backend used for generating graphs

Miscellaneous commands
--------------------------------------------------------------------------------
quit | q | exit | x  Quit rmem
help | h | ?         Show this help message
typeset              Write LaTeX of the current state to ui_snapshot.tex
graph                Write a graphviz/TikZ graph of state and transitions to
                       out.dot/out.tikz
print | p            Print the current state
history | his        Print the history of previously entered commands
options | o          Print the current configuration of options
|help_string}
