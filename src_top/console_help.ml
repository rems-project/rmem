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

let help_message = "\
===============================================================================
RMEM console help
===============================================================================

Multiple commands may be semicolon-separated on one line.
The command line indicates in [square brackets] the default
command to be taken if enter is pressed on an empty command line.

Notation
-------------------------------------------------------------------------------
(... | ...)    represents alternatives
[...]          represents an optional section
<N[n]>         represents a positive integer (including zero)
                 in either decimal or (0x prefixed) hex
<string[n]>    represents an arbitrary string containing no spaces
<bool[n]>      represents a boolean. Accepted values:
                 true = 'true', 'on', '1', 't', 'yes', 'y'
                 false = 'false', 'off', '0', 'f', 'yes', 'y'

Stepping commands
-------------------------------------------------------------------------------
<N>                              Take the transition with the given number.
step | s | step <N>              Take the default transition <N> times,
                                   default once.
back | b | undo | u | back <N>   Undo <N> previously taken transitions,
                                   default one.
stepi <N1> <N2> | si <N1> <N2>   'Step instruction', specifically, search for
                                   and take the shortest path of transitions
                                   to a state in which instruction <N2> of
                                   thread <N1> is finished
follow | f                       Repeatedly take transitions from the
                                   follow-list until either the follow-list is
                                   empty, or it specifies a non-existent
                                   transition number
auto | a                         Repeatedly take the default transition until
                                   no more enabled transitions exist.
                                   Analogous to 'holding down enter'
fetch                            Take all fetch transitions. (note, this may
                                   not terminate if the test has loops)

set random <bool>         Enable or disable the default transition being
                            pseudorandomly chosen
set storage_first <bool>  Enable or disable whether storage transitions are
                            preferentially taken first when stepping

focus thread (<N>|off)           Enable only transitions for the given
                                   thread id, or 'off' for all threads
focus instruction (<N>|off)      Enable only transitions for the given
                                   instruction ioid, or 'off' for all
                                   instructions

Searching commands
--------------------------------------------------------------------------------
search exhaustive | search random <N>
                                 Run an exhaustive search, or <N> pseudorandom
                                   linear searches, through the graph of
                                   possible states, looking for final states
                                   in which no more transitions are possible,
                                   and print a histogram of the reachable
                                   final states. If a breakpoint is hit
                                   during the search, it will stop and jump
                                   to the breakpoint-matching state.
                                   For exhaustive search, one usually wants
                                   hash_prune, eager, eager_fetch,
                                   and forbid_tree_speculation.

set check_inf_loop <bool>  Enable or disable terminating a search if an
                             infinite loop is detected (Not guaranteed)
set hash_prune <bool>     Enable or disable using hashing of states to
                            avoid visiting a state's subtree more than
                            once in a search
set partial_order_reduction <bool>  Enable or disable restricting searches
                                      according to the partial order reduction
set allow_partial <bool>   Enable or disable setting a signal handler to
                             allow interrupting a search and returning
                             partial results
set compare_analyses <bool>  Enable or disable comparing the handwritten
                               and exhaustive analyses
set priority_reduction <bool>  Enable or disable taking certain kinds of
                                 transition preferentially by disabling
                                 all others when they exist. Currently:
                                 exclusives, multiple-successor fetches
set prune_restarts <bool>  Enable or disable discounting traces with
                             instruction restarts when searching (this
                             is safe for searching because of the proven
                             axiomatic/operational equivalence)
set prune_discards <bool>  Enable or disable discounting traces with
                             instruction discards when searching (same.
                             Requires forbid_tree_speculation)

set transition_limit (<N>|none)  Set the transition limit on searches
set trace_limit (<N>|none)       Set the trace limit on searches
set time_limit (<N>|none)        Set the time limit on searches in seconds

  In the 'set foo_limit ...' commands, 'none' means no limit (the default.)

Breakpoint commands
-------------------------------------------------------------------------------
break <N>             Insert a breakpoint when the given address is fetched
break <string> | break <string>+<N> | break <string>-<N>
                      Insert a breakpoint when the address of the given symbol,
                        optionally plus or minus an offset, is fetched
break <string>:<N>    Insert a breakpoint when the given source line is fetched

[a|r]watch <N>        Insert a watchpoint of size one byte at the given address
[a|r]watch <N1>/<N2>  Insert a watchpoint of size <N2> at address <N1>
[a|r]watch <string> | [a|r]watch <string>+<N> | [a|r]watch <string>-<N>
                      Insert a watchpoint of size one byte at the given symbol,
                        optionally plus or minus an offset
[a|r]watch shared     Insert a watchpoint that triggers on all locations which
                        have been recorded as 'shared'
                        (see also 'set record_writes')

  For all watchpoint commands, the type of watchpoint inserted
  is determined by the prefix of the command:
      if no prefix is given, a write watchpoint;
      or if 'r' is prefixed, a read watchpoint;
      or if 'a' is prefixed, an access watchpoint (either read or write.)

info break            Print all the currently active breakpoints
delete [break] <N>    Delete the breakpoint with the given number

Global options
-------------------------------------------------------------------------------
set suppress_internal <bool>  Enable or disable whether Sail micro-steps
                                (exposed as 'Internal' transitions) are suppressed
set record_writes <bool>  Enable or disable written-to memory locations being
                            recorded for the shared-memory approximation
set follow_list <N>[,<N>,...] Set the list of upcoming transitions.
                                (Note, comma separated.)

Eager options
-------------------------------------------------------------------------------
When both searching and exploring interactively, these options control which
transitions are considered 'uninteresting' and eagerly taken, which should not
affect the set of observable behaviours because these are all (believed to be...)
confluent with each other.

set eager_fetch_single <bool>         Fetch transitions with a single successor
set eager_fetch_multi <bool>          Fetch transitions with multiple successors
                                        (i.e., branches. Can cause infinite loops)
set eager_pseudocode_internal <bool>  Pseudocode-internal transitions
set eager_constant_reg_read <bool>    Constant register reads (e.g. pseudo-regs.)
set eager_reg_rw <bool>               Register reads and writes
set eager_memory_aux <bool>           Memory auxiliary transitions
                                        (e.g. flowing events, barriers, commits)
set eager_finish <bool>               Instruction finish transitions
set eager_fp_recalc <bool>            PLDI11 footprint recalculation transitions
set eager_thread_start <bool>         Thread start transitions

set eager <bool>          Shortcut to enable/disable all of the above at once,
                            except that 'set eager on' does not enable
                            eager_fetch_multi (as that can cause infinite loops)

set eager_up_to_shared <bool>         EXPERIMENTAL. Eagerly take even effectful memory
                                        transitions of instructions not in the
                                        shared-memory approximation. May not do what
                                        you expect if enabled at the start of an execution!
                                        Requires record_writes on.


Interface options
--------------------------------------------------------------------------------
set pp_style (full|compact|screenshot)  Set the printing style
set verbosity (quiet|normal|verbose|very|debug)
                          Set the verbosity of command line output
set choice_history_limit (<N>|none)
                          Set the maximum number of 'choices so far' printed
                            with the current state, 'none' for no limit.
set always_print <bool>   Enable or disable printing the current
                            state after every command
set suppress_newpage <bool>  Enable or disable not clearing the screen before
                               every prompt
set buffer_messages <bool>   Enable or disable holding messages until the
                               next prompt is displayed, for easy reading
set announce_options <bool>  Enable or disable printing the current
                               configuration of options before each prompt
set prefer_symbolic_values <bool>  Enable or disable preferring symbolic
                                     values to hex in output
set hide_pseudoregister_reads <bool>  Hide or show pseudoregister reads in output
set max_finished (<N>|none)  Set the maximum number of finished instructions
                               printed at the start of each thread
set pp_colours <bool>      Enable or disable colourised output
set dumb_terminal <bool>   Enable or disable dumb terminal mode which disables
                             readline, cursor movement, etc
set condense_finished_instructions <bool> Enable or disable condensing
                                            finished instructions in printed states
set pp_hex <bool>          Enable or disable printing histogram values in hex
set dwarf_show_all_variable_locations <bool>  Enable or disable showing all DWARF
                                                variable location data at each
                                                instruction
set pp_sail <bool>         Enable or disable showing the Sail interpreter state
                             and code for instructions (if interpreter enabled)

Graph options
--------------------------------------------------------------------------------
set always_graph  <bool>  Enable or disable generate a .dot or onscreen
                            transition graph after every command
set dot_final_ok <bool>    Enable or disable generating a .dot or onscreen
                             transition graph for the first
                             final-constraint-satisfying execution of searches
set ppg_shared <bool>     Enable or disable showing *only* shared-memory
                            instructions in graph output
set ppg_rf <bool>         Enable or disable showing read-from (rf) edges
set ppg_fr <bool>         Enable or disable showing from-read (fr) edges
set ppg_co <bool>         Enable or disable showing coherence (co) edges
set ppg_addr <bool>       Enable or disable showing address dependency (addr) edges
set ppg_data <bool>       Enable or disable showing data dependency (data) edges
set ppg_ctrl <bool>       Enable or disable showing control dependency (ctrl) edges
set ppg_regs <bool>       Enable or disable showing registers in graph output
set ppg_reg_rf <bool>     Enable or disable showing rf edges in graph output
set ppg_trans <bool>      Enable or disable showing transitions in graph output
set graph_backend (dot|tikz)  Set the backend used for generating graphs

Miscellaneous commands
-------------------------------------------------------------------------------
quit | q | exit | x  Quit ppcmem
help | h | ?         Show this help message
typeset              Write LaTeX of the current state to ui_snapshot.tex
graph                Write a graphviz graph of state and transitions to out.dot
print | p            Print the current state
history | his        Print the history of previously entered commands

===============================================================================
"
