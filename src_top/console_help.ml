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

open Interact_parser_base

module SO = Structured_output

let cmd_info : ast -> (string * string) list = function

  (** help_stepping *************************************************)

  | Default ->
      [ ("<empty>",
          "Take the default command as indicated by the [square brackets] in the command line prompt");
      ]

  | Transition (WithEager _)
  | Transition (WithBoundedEager (_, _)) ->
      [ ("<N>",
          "Take the transition with the given number, and all the enabled eager transitions that follow (see 'help eager').");
        ("<N1>e<N2>",
          "Take transition <N1>, and then another <N2> enabled eager transitions (see 'help eager').");
      ]

  | Step None
  | Step (Some _) ->
      [ ("s|step [<N>]",
          "Take the default transition <N> times, default once.");
      ]

  | Back None
  | Back (Some _) ->
      [ ("b|back [<N>]",
          "Step back <N> previously taken transitions, default one.");
      ]

  | Auto ->
      [ ("a|auto",
          "Repeatedly take the default transition until no more enabled transitions exist. Analogous to 'holding down enter'.");
      ]

  | SetOption ("follow_list", _)
  | Follow ->
      [ ("set follow_list \"[<N>[;<N>[...]]]\"",
          "Set the list of upcoming transitions.");
        ("", "  Note 1: 'set follow_list \"4;2\"' is a single command that sets the follow-list to 4 followed by 2, and 'set follow_list 4;2' (without the double quotes) is two commands: set the follow-list to 4 and take transition 2 (which has the side effect of clearing the follow-list because it does not match the next transition in the follow-list, 4).");
        ("", "  Note 2: 'set follow_list \"\"' clears the follow-list.");
        ("fo|follow",
          "Repeatedly take all the transitions from the follow-list.");
      ]

  | FetchAll ->
      [ ("fetch",
          "Take all fetch transitions. Note, this may not terminate if the test has loops.");
      ]

  | SetOption ("random", _) ->
      [ ("set random <bool>",
          "Enable or disable the default transition being pseudorandomly chosen.");
      ]

  | SetOption ("storage_first", _) ->
      [ ("set storage_first <bool>",
          "Enable or disable whether storage transitions are preferentially taken first when stepping.");
      ]

  | FocusThread _
  | FocusInstruction _ ->
      [ ("focus thread (<N>|off)",
          "Enable only transitions for the given thread id, or 'off' for all threads.");
        ("focus instruction (<ioid>|off)",
          "Enable only transitions for the given instruction ioid, or 'off' for all instructions.");
      ]

  | StepInstruction (_, _)
  | PeekInstruction (_, _) ->
      [ ("si|stepi [<N>|<ioid>]",
          "Take transitions of the specified instruction until it is finished, if possible. The specified instruction is either <ioid>, the first instruction of <N> that has enabled transitions, or the first instruction with enabled transitions of the first thread with such instruction.");
        ("peeki <ioid>",
          "Show all the states instruction <ioid> can reach by taking only <ioid> transitions. Each state of <ioid> will be printed with the sequence of transitions that leads to it below.");
      ]

  (** help_searching ************************************************)

  | Search (_, None)
  | Search (_, Some _) ->
      [ ("search (exhaustive|random <N>) [final|final_ok|final_not_ok]",
          "Run an exhaustive search, or <N> pseudorandom linear searches, through the graph of possible states, looking for final states in which no more transitions are possible, and print a histogram of the reachable final states. If a breakpoint is hit during the search, it will stop and jump to the breakpoint-matching state.");
      ]

  | SetOption ("hash_prune", _) ->
      [ ("set hash_prune <bool>",
          "Enable or disable using hashing of states to avoid visiting a state's subtree more than once in a search.");
      ]

  | SetOption ("prune_restarts", _)
  | SetOption ("prune_discards", _) ->
      [ ("set prune_restarts <bool>",
          "Enable or disable discounting traces with instruction restarts when searching (believed to be safe).");
        ("set prune_discards <bool>",
          "Enable or disable discounting traces with instruction discards when searching (believed to be safe; requires '-model forbid_tree_speculation').");
      ]

  | SetOption ("partial_order_reduction", _)
  | SetOption ("priority_reduction", _) ->
      [ ("set partial_order_reduction <bool>",
          "Enable or disable restricting searches according to the partial order reduction");
        ("set priority_reduction <bool>",
          "Enable or disable taking certain kinds of transition preferentially by disabling all others when they exist. Currently: exclusives, multiple-successor fetches.");
      ]

  | SetOption ("allow_partial", _) ->
      [ ("set allow_partial <bool>",
          "Enable or disable setting a signal handler to allow interrupting a search and returning partial results.");
      ]

  | SetOption ("compare_analyses", _) ->
      [ ("set compare_analyses <bool>",
          "Enable or disable comparing the handwritten and exhaustive analyses.");
      ]

  | SetOption ("transition_limit", _)
  | SetOption ("trace_limit", _)
  | SetOption ("time_limit", _) ->
      [ ("set transition_limit (<N>|none)",
          "Set the transition limit on searches.");
        ("set trace_limit (<N>|none)",
          "Set the trace limit on searches.");
        ("set time_limit (<N>|none)",
          "Set the time limit on searches in seconds.");
      ]

  | SetOption ("branch-targets", _) ->
      [ ("set branch-targets \"<bt-map>\"",
          "EXPERIMENTAL. Sets the initial approximation of branch targets for branch-register instructions (will be extended by the search until a fixed-point is reached).");
      ]

  (** help_breakpoints **********************************************)

  | BreakpointFetch _
  | BreakpointLine (_, _) ->
      [ ("break <N>",
          "Insert a breakpoint that will fire when the given address can be fetched.");
        ("break <string>","");
        ("break <string>+<N>","");
        ("break <string>-<N>",
          "Insert a breakpoint that will fire when the address of the given symbol, optionally plus or minus an offset, can be fetched.");
        ("break <string>:<N>",
          "Insert a break-point that will fire when the given source line can be fetched.");
      ]

  | Watchpoint (_, _)
  | SharedWatchpoint _ ->
      [ ("[a|r]watch <N>",
          "Insert a watch-point of size one byte at the given address.");
        ("[a|r]watch <N1>/<N2>",
          "Insert a watch-point of size <N2> at address <N1>.");
        ("[a|r]watch <string>","");
        ("[a|r]watch <string>+<N>","");
        ("[a|r]watch <string>-<N>",
          "Insert a watchpoint of size one byte at the given symbol, optionally plus or minus an offset.");
        ("[a|r]watch shared",
          "Insert a watchpoint that triggers on all locations which have been recorded as 'shared'.");
      ]

  | InfoBreakpoints ->
      [ ("info break",
          "Print all the currently active breakpoints.");
      ]

  | DeleteBreakpoint _ ->
      [ ("delete [break] <N>",
          "Delete the breakpoint with the given number.");
      ]


  (** help_eager ****************************************************)

  | SetOption ("eager_fetch_single", _)
  | SetOption ("eager_fetch_multi", _)
  | SetOption ("eager_pseudocode_internal", _)
  | SetOption ("eager_constant_reg_read", _)
  | SetOption ("eager_reg_rw", _)
  | SetOption ("eager_memory_aux", _)
  | SetOption ("eager_finish", _)
  | SetOption ("eager_fp_recalc", _)
  | SetOption ("eager_thread_start", _)
  | SetOption ("eager", _)
  | SetOption ("eager_local_mem", _)
  | SetOption ("eager_fetch_unmodified", _)
  | SetOption ("shared-memory", _) ->
      [ ("set eager <bool>",
          "Shortcut to enable/disable all the eager options at once, except that 'set eager on' does not enable eager_fetch_multi (as that can cause infinite loops) and eager_local_mem.");
        ("set eager_fetch_single <bool>",
          "Fetch transitions with a single successor");
        ("set eager_fetch_multi <bool>",
          "Fetch transitions with multiple successors (i.e., branches. Can cause infinite loops)");
        ("set eager_pseudocode_internal <bool>",
          "Pseudocode-internal transitions");
        ("set eager_constant_reg_read <bool>",
          "Constant register reads (e.g. pseudo-regs.)");
        ("set eager_reg_rw <bool>",
          "Register reads and writes");
        ("set eager_memory_aux <bool>",
          "Memory auxiliary transitions (e.g. flowing events, barriers, commits)");
        ("set eager_finish <bool>",
          "Instruction finish transitions");
        ("set eager_fp_recalc <bool>",
          "PLDI11 footprint recalculation transitions");
        ("set eager_thread_start <bool>",
          "Thread start transitions");
        ("set eager_local_mem <bool>",
          "EXPERIMENTAL. Affects searching only. Eagerly take thread-local memory access transitions using an approximation of the shared memory footprint. The approximation is refined at the end of the search, and another search is started, until a fixed-point is reached.");
        ("set shared-memory \"<footprints>\"",
          "EXPERIMENTAL. Sets the initial approximation of the shared memory footprint for eager_local_mem (will be extended by the search until a fixed-point is reached).");
        ("set eager_fetch_unmodified <bool>",
          "EXPERIMENTAL. Affects searching only. Eagerly take fetch transitions using an approximation of the set of modified locations. The approximation is refined at the end of the search, and another search is started, until a fixed-point is reached.");
        ("set memory-writes \"<footprints>\"",
          "EXPERIMENTAL. Overwrites the approximation of the modified code locations for eager_fetch_unmodified.");
      ]

  (** help_interface ************************************************)

  | SetOption ("pp_style", _)
  | SetOption ("condense_finished_instructions", _)
  | SetOption ("max_finished", _)
  | SetOption ("pp_sail", _) ->
      [ ("set pp_style (full|compact|screenshot)",
          "Set the printing style");
        ("set condense_finished_instructions <bool>",
          "Enable or disable condensing finished instructions in printed states.");
        ("set max_finished (<N>|none)",
          "Set the maximum number of finished instructions printed at the start of each thread.");
        ("set pp_sail <bool>",
          "Enable or disable showing the Sail interpreter state and code for instructions (if interpreter enabled).");
      ]

  | SetOption ("verbosity", _) ->
      [ ("set verbosity (quiet|normal|verbose|very|debug)",
          "Set the verbosity of command line output.");
      ]

  | SetOption ("choice_history_limit", _) ->
      [ ("set choice_history_limit (<N>|none)",
          "Set the maximum number of 'choices so far' printed with the current state, 'none' for no limit.");
      ]

  | SetOption ("always_print", _) ->
      [ ("set always_print <bool>",
          "Enable or disable printing the current state after every command.");
      ]

  | SetOption ("prefer_symbolic_values", _)
  | SetOption ("pp_hex", _) ->
      [ ("set prefer_symbolic_values <bool>",
          "Enable or disable preferring symbolic values to hex in output.");
        ("set pp_hex <bool>",
          "Enable/disable printing histogram values in hex.");
      ]


  | SetOption ("hide_pseudoregister_reads", _) ->
      [ ("set hide_pseudoregister_reads <bool>",
          "Hide or show pseudoregister reads in output");
      ]

  | SetOption ("pp_colours", _) ->
      [ ("set pp_colours <bool>",
          "Enable or disable colourised output");
      ]

  | SetOption ("dwarf_show_all_variable_locations", _) ->
      [ ("set dwarf_show_all_variable_locations <bool>",
          "Enable or disable showing all DWARF variable location data at each instruction");
      ]

  | SetOption ("state_output", _)
  | SetOption ("trace_output", _) ->
      [ ("set state_output <file>",
          "Print the current state, every time it changes, to <file>.");
        ("set trace_output <file>",
          "Print the current trace, every time it changes, to <file>.");
      ]

  (** help_graphs ***************************************************)

  | SetOption ("always_graph", _)
  | SetOption ("graph_backend", _) ->
      [ ("set always_graph  <bool>",
          "Enable/disable generate a .dot or onscreen transition graph after every command");
        ("set graph_backend (dot|tikz)",
          "Set the backend used for generating graphs");
      ]

  | SetOption ("ppg_shared", _)
  | SetOption ("ppg_rf", _)
  | SetOption ("ppg_fr", _)
  | SetOption ("ppg_co", _)
  | SetOption ("ppg_addr", _)
  | SetOption ("ppg_data", _)
  | SetOption ("ppg_ctrl", _)
  | SetOption ("ppg_regs", _)
  | SetOption ("ppg_reg_rf", _)
  | SetOption ("ppg_trans", _) ->
      [ ("set ppg_shared <bool>",
          "Enable/disable showing *only* shared-memory instructions in graph output");
        ("set ppg_rf <bool>",
          "Enable/disable showing read-from (rf) edges");
        ("set ppg_fr <bool>",
          "Enable/disable showing from-read (fr) edges");
        ("set ppg_co <bool>",
          "Enable/disable showing coherence (co) edges");
        ("set ppg_addr <bool>",
          "Enable/disable showing address dependency (addr) edges");
        ("set ppg_data <bool>",
          "Enable/disable showing data dependency (data) edges");
        ("set ppg_ctrl <bool>",
          "Enable/disable showing control dependency (ctrl) edges");
        ("set ppg_regs <bool>",
          "Enable/disable showing registers in graph output");
        ("set ppg_reg_rf <bool>",
          "Enable/disable showing rf edges in graph output");
        ("set ppg_trans <bool>",
          "Enable/disable showing transitions in graph output");
      ]

  (** help_misc *****************************************************)

  | Quit ->
      [ ("e|exit", "");
        ("q|quit",
          "Quit rmem");
      ]

  | Help _ ->
      [ ("?|h|help",
          "Show a help message for all the commands.");
        ("h|help <command> [<args>]",
          "Show a help message for <command>.");
        ("h|help <category>",
          "Show all the commands related to <category>. The following categories are available: stepping, searching, breakpoints, eager, interface, graphs, misc");
      ]

  | Typeset ->
      [ ("typeset",
          "Write LaTeX of the current state to ui_snapshot.tex");
      ]


  | Graph ->
      [ ("graph",
          "Write a graphviz/TikZ graph of state and transitions to out.dot/out.tikz");
      ]

  | Print ->
      [ ("p|print",
          "Print the current state");
      ]

  | History ->
      [ ("his|history",
          "Print the history of previously entered commands");
      ]

  | ShowOptions ->
      [ ("o|options",
          "Print the current configuration of options");
      ]

  | SetOption ("", []) ->
      [ ("set <key> [<arg> [<arg>[...]]]",
          "Set <key> according to <arg>s");
      ]

  (******************************************************************)

  | Debug _
  | Undo
  | SetOption (_, _)
      -> []


(* 'break_lines n str' returns a list of strings which are the result
of break 'str' to sub-strings, the first of which is of length at most
'n' and the following are of length at most 'n'-2. The break is always
done over a ' ' (space character) which is removed from the result. If
there is no ' ', the line will be longer then 'n'/'n'-2. *)
let break_lines n str : string list =
  let length = String.length str in
  let rec helper pos lines : string list =
    let n = if lines = [] then n else n - 2 in
    if pos >= length then List.rev lines
    else if pos + n >= length then
      (String.sub str pos (length - pos)) :: lines
      |> List.rev
    else
      let pos' =
        try
          match String.rindex_from str (pos + n) ' ' with
          | pos' when pos' > pos -> pos'
          | _ -> raise Not_found
        with
        | Not_found ->
            begin try String.index_from str (pos + n) ' ' with
            | Not_found -> length
            end
      in
      (String.sub str pos (pos' - pos)) :: lines
      |> helper (pos' + 1)
  in
  helper 0 []


let print_help pad cmds : SO.t =
  let pad =
    match pad with
    | None ->
        List.map (fun (cmd, _) -> String.length cmd) cmds
        |> List.fold_left (fun m l -> if l > 33 then m else max m l) 0
        |> (+) 2
    | Some p -> p
  in
  List.map (fun (cmd, text) ->
    match break_lines (80 - pad) text with
    | []
    | "" :: [] ->
        SO.aclass CommandUsage @@ SO.strLine "%s" cmd
    | hd :: tl ->
        if String.length cmd > (pad - 2) then
          SO.concat @@
            (SO.aclass CommandUsage @@ SO.strLine "%s" cmd) ::
            (SO.strLine "%*s%s" pad "" hd) ::
            (List.map (fun s -> SO.strLine "%*s%s" (pad + 2) "" s) tl)
        else
          SO.concat @@
            (SO.line @@ SO.Concat [
              SO.aclass CommandUsage @@ SO.str "%-*s" pad cmd;
              SO.String hd;
            ]) ::
            (List.map (fun s -> SO.strLine "%*s%s" (pad + 2) "" s) tl)
  ) cmds
  |> SO.concat


let help_cmd cmd : SO.t =
  match cmd_info cmd with
  | [] ->
      SO.strLine "Help is not avilable for this command (try 'help' or 'help help')"
  | lines -> print_help None lines


let help_stepping pad : SO.t =
  List.map cmd_info [
    Transition (WithEager 0);
    Step None;
    Back None;
    Auto;
    SetOption ("follow_list", []);
    Follow;
    FetchAll;
    SetOption ("random", []);
    SetOption ("storage_first", []);
    FocusThread None;
    (*FocusInstruction _*)
    StepInstruction (None, None);
    (*PeekInstruction (_, _)*)
  ]
  |> List.concat
  |> print_help pad


let help_searching pad : SO.t =
  SO.Concat [
    List.map cmd_info [
      Search (Exhaustive, None);
      SetOption ("hash_prune", []);
      SetOption ("prune_restarts", []);
      (*SetOption ("prune_discards", _)*)
      SetOption ("partial_order_reduction", []);
      (*SetOption ("priority_reduction", _)*)
      SetOption ("allow_partial", []);
      SetOption ("compare_analyses", []);
      SetOption ("transition_limit", []);
      (*SetOption ("trace_limit", _)*)
      (*SetOption ("time_limit", _)*)
      SetOption ("branch-targets", []);
    ]
    |> List.concat
    |> print_help pad;
    SO.Line SO.Empty;
    SO.strLine "In the 'set foo_limit ...' commands, 'none' means no limit (the default).";
  ]

let help_breakpoints pad : SO.t =
  SO.Concat [
    List.map cmd_info [
      BreakpointLine ("", 0);
      (*BreakpointFetch _*)
      Watchpoint (Write, Symbol ("", 0));
      (*SharedWatchpoint _*)
      InfoBreakpoints;
      DeleteBreakpoint 0;
    ]
    |> List.concat
    |> print_help pad;
    SO.Line SO.Empty;
    SO.strLine "For all watchpoint commands, the type of watchpoint inserted";
    SO.strLine "is determined by the prefix of the command:";
    SO.strLine "  if no prefix is given, a write watchpoint;";
    SO.strLine "  or if 'r' is prefixed, a read watchpoint;";
    SO.strLine "  or if 'a' is prefixed, an access watchpoint (either read or write.)";
  ]

let help_eager pad : SO.t =
  SO.Concat [
    List.map cmd_info [
      SetOption ("eager", []);
      (*SetOption ("eager_fetch_single", _)*)
      (*SetOption ("eager_fetch_multi", _)*)
      (*SetOption ("eager_pseudocode_internal", _)*)
      (*SetOption ("eager_constant_reg_read", _)*)
      (*SetOption ("eager_reg_rw", _)*)
      (*SetOption ("eager_memory_aux", _)*)
      (*SetOption ("eager_finish", _)*)
      (*SetOption ("eager_fp_recalc", _)*)
      (*SetOption ("eager_thread_start", _)*)
      (*SetOption ("eager_local_mem", _)*)
      (*SetOption ("shared-memory", _)*)
    ]
    |> List.concat
    |> print_help pad;
    SO.Line SO.Empty;
    SO.strLine "When exploring interactively, these options control which transitions are";
    SO.strLine "considered 'uninteresting' and eagerly taken, which should not affect the set";
    SO.strLine "of observable behaviours because these are all (believed to be...)";
    SO.strLine "confluent with each other.";
  ]

let help_interface pad : SO.t =
  List.map cmd_info [
    SetOption ("pp_style", []);
    (*SetOption ("condense_finished_instructions", _)*)
    (*SetOption ("max_finished", _)*)
    (*SetOption ("pp_sail", _)*)
    SetOption ("verbosity", []);
    SetOption ("choice_history_limit", []);
    SetOption ("always_print", []);
    SetOption ("prefer_symbolic_values", []);
    (*SetOption ("pp_hex", _)*)
    SetOption ("hide_pseudoregister_reads", []);
    SetOption ("pp_colours", []);
    SetOption ("dwarf_show_all_variable_locations", []);
    SetOption ("state_output", []);
    (*SetOption ("trace_output", _)*)
  ]
  |> List.concat
  |> print_help pad

let help_graphs pad : SO.t =
  List.map cmd_info [
    SetOption ("always_graph", []);
    (*SetOption ("graph_backend", _)*)
    SetOption ("ppg_shared", []);
    (*SetOption ("ppg_rf", _)*)
    (*SetOption ("ppg_fr", _)*)
    (*SetOption ("ppg_co", _)*)
    (*SetOption ("ppg_addr", _)*)
    (*SetOption ("ppg_data", _)*)
    (*SetOption ("ppg_ctrl", _)*)
    (*SetOption ("ppg_regs", _)*)
    (*SetOption ("ppg_reg_rf", _)*)
    (*SetOption ("ppg_trans", _)*)
  ]
  |> List.concat
  |> print_help pad

let help_misc pad : SO.t =
  List.map cmd_info [
    Quit;
    Help [];
    Typeset;
    Graph;
    Print;
    History;
    ShowOptions;
    SetOption ("", []);
  ]
  |> List.concat
  |> print_help pad


let help_message pad : SO.t =
  SO.Concat [
    SO.HorLine SO.Thick;
    SO.strLine "RMEM console help";
    SO.HorLine SO.Thick;
    SO.Line SO.Empty;
    SO.strLine "The command line indicates in [square brackets] the default command to be taken";
    SO.strLine "if enter is pressed on an empty command line.  Multiple commands may be";
    SO.strLine "semicolon-separated on one line.  Note that a trailing semicolon has an empty";
    SO.strLine "command on the right hand side, which is interpreted as the default command.";
    SO.Line SO.Empty;
    SO.strLine "Notation";
    SO.HorLine SO.Normal;
    print_help pad [
      ("(...|...)", "Represents alternatives");
      ("[...]", "Represents an optional section");
      ("<N[n]>", "Represents a positive integer (including zero) in either decimal or (0x prefixed) hex");
      ("<ioid>", "Represents instruction instance ID. This can be entered as two integers, separated by colon or space, where the first one is the thread number and the second one is the instruction number within the thread (e.g. '0:1' or '0 1' both represent the first instruction of thread 0).");
      ("<string[n]>", "Represents an arbitrary string containing no spaces");
      ("<bool[n]>", "Represents a boolean. Accepted values:");
      ("",          "  true = 'true', 'on', '1', 't', 'yes', 'y'");
      ("",          "  false = 'false', 'off', '0', 'f', 'no', 'n'");
    ];
    SO.Line SO.Empty;
    SO.strLine "Stepping commands";
    SO.HorLine SO.Normal;
    help_stepping pad;
    SO.Line SO.Empty;
    SO.strLine "Searching commands";
    SO.HorLine SO.Normal;
    help_searching pad;
    SO.Line SO.Empty;
    SO.strLine "Breakpoint commands";
    SO.HorLine SO.Normal;
    help_breakpoints pad;
    SO.Line SO.Empty;
    SO.strLine "Eager options";
    SO.HorLine SO.Normal;
    help_eager pad;
    SO.Line SO.Empty;
    SO.strLine "Interface options";
    SO.HorLine SO.Normal;
    help_interface pad;
    SO.Line SO.Empty;
    SO.strLine "Graph options";
    SO.HorLine SO.Normal;
    help_graphs pad;
    SO.Line SO.Empty;
    SO.strLine "Miscellaneous commands";
    SO.HorLine SO.Normal;
    help_misc pad;
  ]

