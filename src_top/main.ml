(*==================================================================================================*)
(*                                                                                                  *)
(*                rmem executable model                                                             *)
(*                =====================                                                             *)
(*                                                                                                  *)
(*  This file is:                                                                                   *)
(*                                                                                                  *)
(*  Copyright Shaked Flur, University of Cambridge                                      2014-2018   *)
(*  Copyright Peter Sewell, University of Cambridge                          2011-2012, 2014-2017   *)
(*  Copyright Jon French, University of Cambridge                                       2017-2018   *)
(*  Copyright Christopher Pulte, University of Cambridge                                2015-2018   *)
(*  Copyright Susmit Sarkar, University of St Andrews                        2011-2012, 2014-2015   *)
(*  Copyright Luc Maranget, INRIA, Paris, France                                  2011-2012, 2015   *)
(*  Copyright Ohad Kammar, University of Cambridge (when this work was done)                 2013   *)
(*  Copyright Kathy Gray, University of Cambridge (when this work was done)                  2015   *)
(*  Copyright Pankaj Pawan, IIT Kanpur and INRIA (when this work was done)                   2011   *)
(*                                                                                                  *)
(*  All rights reserved.                                                                            *)
(*                                                                                                  *)
(*  It is part of the rmem tool, distributed under the 2-clause BSD licence in                      *)
(*  LICENCE.txt.                                                                                    *)
(*                                                                                                  *)
(*==================================================================================================*)

open Params;;
open RunOptions;;

module SO = Structured_output;;

(* FIXME: (SF) does this have any effect on performance? when needed
OCAMLRUNPARAM=b can be used instead *)
Printexc.record_backtrace true

let fatal_error msg =
  Printf.eprintf "Fatal error: %s\n" msg;
  exit 1
;;

(* SF: I think this is used to make sure -q overrides -v and anything
else that might change verbosity *)
let quiet = ref false;;

let run_options : RunOptions.t ref =
  ref {RunOptions.default_options with RunOptions.interactive = Screen.interactive}

let should_list_isas = ref false

let do_list_isas () =
  let s =
    String.concat " " 
      (MlUtils.option_map
         (fun isa ->
           let module Isa = (val isa : Isa_model.S) in
           if Isa.available 
           then Some Isa.name
           else None
         )
         Isa_model.all_isa_defs)
  in
  Printf.printf "%s\n" s;
  exit 0

(** command line argument specifications *)
let opts = [
(** model options ***************************************************)
("-model",
    Arg.String (fun s ->
      Globals.model_params :=
        try (Model_aux.parse_and_update_model s !Globals.model_params) with
        | Misc.UserError msg -> raise (Arg.Bad msg)),
    match Model_aux.model_strings with
    | m1 :: m2 :: m3 :: _ ->
        Printf.sprintf "<%s|%s|%s|...> model options (repeatable), see details below" m1 m2 m3
    | _ -> "<option> model options (repeatable), see details below");
("-thread_fetch_limit",
    Arg.Int (fun i -> if i < 0 then raise (Failure "-thread_fetch_limit must be at least 0");
        Globals.model_params :=
            Model_aux.set_thread_fetch_limit i !Globals.model_params),
    ("<n> number of un-decoded instructions in each path (default: None)"));
    (* TODO: if both this and topologies are specified, check consistency *)
("-loop_limit",
 Arg.Int (fun i -> Globals.model_params := { !Globals.model_params with t = { (!Globals.model_params).t with thread_loop_unroll_limit = Some i }}),
 ("<integer> automatically unroll loops to this depth (default: off)"));
("-promising_fuel",
 Arg.Int (fun i -> Globals.model_params := { !Globals.model_params with t = { (!Globals.model_params).t with p_fuel = Some i }}),
 ("<integer> automatically unroll loops to this depth (default: off)"));
("-topauto",
    Arg.Bool (fun b -> Globals.topauto := b; Globals.flowing_topologies := []),
    (Printf.sprintf "<bool> for Flowing model: iterate over all possible topologies (%b)" !Globals.topauto));
("-top",
    Arg.String (fun s -> match Model_aux.parse_topologies s with
                         | Some t -> Globals.flowing_topologies := t; Globals.topauto := false
                         | None -> raise (Failure "topology parse error")),
    "<topology-list> for Flowing model: specify topologies, e.g. \"[0,[1,2]];[0,1,2]\"");

("-elf_threads",
    Arg.Int (fun i -> if i < 1 then raise (Failure "-elf_threads must be at least 1");
                      Globals.elf_threads := i ),
    (Printf.sprintf "<n> number of threads to create for an ELF test (%d)" !Globals.elf_threads));
    (* TODO: if both this and topologies are specified, check consistency *)

("-aarch64gen",
    Arg.Bool (fun b -> Globals.aarch64gen := b),
    (Printf.sprintf "<bool> when running an AArch64 test, use the auto-generated ISA model (%b)" !Globals.aarch64gen));

("-big_endian",
    Arg.Bool (fun b -> Globals.big_endian := Some b),
    (Printf.sprintf "<bool> use big endian"));

("-final_cond",
    Arg.String (fun s -> Globals.final_cond := Some s),
    (Printf.sprintf "<cond> change the final condition to \"cond\""));

("-branch_targets",
    Arg.String (fun filename ->
        try Globals.branch_targets := Some (Model_aux.branch_targets_parse_from_file filename) with
        | Model_aux.BranchTargetsParsingError msg ->
            Printf.eprintf "Error, cannot parse the -branch_targets file: %s" msg;
            exit 1
    ),
    (Printf.sprintf "<file> set the initial branch targets for branch-register instructions (overrides the litmus file key \"Branch-targets\")"));

("-branch_targets_str",
    Arg.String (fun s ->
        try Globals.branch_targets := Some (Model_aux.branch_targets_parse_from_string s) with
        | Model_aux.BranchTargetsParsingError msg ->
            Printf.eprintf "Error, cannot parse the -branch_targets_str argument: %s" msg;
            exit 1
    ),
    (Printf.sprintf "<string> same as -branch_targets, but read the targets from the string"));




("-litmus_test_base_address",
    Arg.Int (fun i -> if i < 0 then raise (Failure "-litmus_test_base_address must be at least 0");
                      Globals.litmus_test_base_address := i ),
    (Printf.sprintf "<n> the first memory address to use for the litmus test variables (%d)" !Globals.litmus_test_base_address));

("-litmus_test_minimum_width",
    Arg.Int (fun i -> if i < 0 then raise (Failure "-litmus_test_minimum_width must be at least 0");
                      Globals.litmus_test_minimum_width := i ),
    (Printf.sprintf "<n> the minimum width to use for the litmus test variables (%d)" !Globals.litmus_test_minimum_width));


(** run modes *******************************************************)
("-interactive",
    Arg.Bool (fun b -> run_options := {!run_options with RunOptions.interactive = b}),
    Printf.sprintf "<bool> interactive or batch mode (%b)" (!run_options).RunOptions.interactive);

(** interactive mode options ****************************************)

("-follow",
    Arg.String (fun s ->
      if (String.length (String.trim s)) > 0 then
        Globals.ui_commands := Some (match !Globals.ui_commands with None -> "set follow_list \"" ^ s ^ "\""| Some s0 -> s0 ^ ";" ^ "set follow_list \"" ^ s ^ "\"")
    ),
    "<int-list> short hand for \"-cmds 'set follow_list \"<int-list>\"'\"");

("-cmds",
    Arg.String (fun s ->
      if (String.length (String.trim s)) > 0 then
        Globals.ui_commands := Some (match !Globals.ui_commands with None -> s | Some s0 -> s0 ^ ";" ^ s)
    ),
    "<cmd-list> list of UI commands to execute, semicolon-separated (repeatable)");

("-random",
    Arg.Bool (fun b -> run_options := {!run_options with RunOptions.pseudorandom = b};
                       Globals.interactive_auto := b),
    (Printf.sprintf "<bool> in interactive mode, make pseudorandom choices of transition candidates (%b)" !run_options.RunOptions.pseudorandom));
("-random_traces",
    Arg.Int (fun i -> if i < 1 then raise (Failure "-random_traces must be a natural number");
                      run_options := {!run_options with RunOptions.pseudorandom_traces = i;
                                                        RunOptions.pseudorandom = true};
                      Globals.interactive_auto := true),
    "<n> in non-interactive (batch) mode, do <n> pseudorandom traces (implies -random true)");

("-random_seed",
    Arg.String (fun s ->
      if s="fresh" then Globals.random_seed := (None:int option)
      else Globals.random_seed := Some
          (try (int_of_string s)
          with Failure _ -> raise (Failure "-random_seed must be an integer or the string fresh"))),
    (Printf.sprintf "random seed, either <n> or \"fresh\" (%s)" (match !Globals.random_seed with None -> "fresh" | Some n -> string_of_int n)));

("-auto_internal",
    Arg.Bool (fun b -> Globals.auto_internal := b),
    (Printf.sprintf "<bool> in interactive mode, automatically take internal transitions (%b)" !Globals.auto_internal));

(** optimizations ***************************************************)
("-eager",
    Arg.Bool (fun b -> run_options :=
        {!run_options with RunOptions.eager_mode =
          if b then RunOptions.eager_mode_all_on (!run_options).eager_mode
          else RunOptions.eager_mode_all_off (!run_options).eager_mode}),
    (Printf.sprintf "<bool> eagerly take transitions that don't affect observable behaviour (false)"));
("-hash_prune",
    Arg.Bool (fun b -> run_options := {!run_options with RunOptions.hash_prune = b}),
    (Printf.sprintf "<bool> for batch mode, prune search tree using a hashmap of already seen states (%b)" !run_options.RunOptions.hash_prune));
("-postcondition_filter",
    Arg.Bool (fun b -> run_options := {!run_options with RunOptions.postcondition_filter = b}),
    (Printf.sprintf "<bool> filter states based on the postcondition (%b)" !run_options.RunOptions.postcondition_filter));
("-partial_order_reduction",
    Arg.Bool (fun b -> run_options := {!run_options with RunOptions.partial_order_reduction = b}),
    (Printf.sprintf "<bool> for batch mode, restrict transitions according to partial order reduction (%b)" !run_options.RunOptions.partial_order_reduction));

("-priority_reduction",
    Arg.Bool (fun b -> run_options := {!run_options with RunOptions.priority_reduction = b}),
    (Printf.sprintf "<bool> when certain kinds of transitions are avilable, remove all other transitions (%b)" !run_options.RunOptions.priority_reduction));

("-prune_restarts",
    Arg.Bool (fun b -> run_options := {!run_options with RunOptions.prune_restarts = b}),
    (Printf.sprintf "<bool> for batch mode, don't explore traces with instruction restarts (%b)" !run_options.RunOptions.prune_restarts));

("-prune_discards",
    Arg.Bool (fun b -> run_options := {!run_options with RunOptions.prune_discards = b}),
    (Printf.sprintf "<bool> (requires '-model forbid_tree_speculation') for batch mode, don't explore traces with discarded instructions (%b)" !run_options.RunOptions.prune_discards));

("-suppress_non_symbol_memory",
    Arg.Bool (fun b -> Globals.suppress_non_symbol_memory := b),
    (Printf.sprintf "<bool> suppress non-symbol memory for ELF input files (%b)" !Globals.suppress_non_symbol_memory));

("-max_trace_length",
    Arg.String (fun s ->
      if s="none"  then
        run_options := {!run_options with RunOptions.max_trace_length = None}
      else
        let n =
          try int_of_string s with
          | Failure _ -> raise (Failure "-max_trace_length must be an integer or the string none")
        in
        run_options := {!run_options with RunOptions.max_trace_length = Some n}),
    (Printf.sprintf "<n> fail if a trace longer than n is found, \"none\" for no limit (%s)"
        (match !run_options.RunOptions.max_trace_length with
         | None -> "none"
         | Some n -> string_of_int n)));

("-eager_local_mem",
    Arg.Bool (fun b ->
      let eager_mode =
        {!run_options.RunOptions.eager_mode with
            eager_local_mem = b}
      in
      run_options := {!run_options with RunOptions.eager_mode = eager_mode}),
    (Printf.sprintf "<bool> eagerly take thread-local memory access transitions (%b); exhaustive mode will run multiple times; at the end of each run we calculate the shared memory footprint and use it in the next run, to a fixed-point; the initial shared memory footprint can be set using the litmus file key \"Shared-memory=...\" and the -shared_memory option (empty otherwise)." !run_options.RunOptions.eager_mode.eager_local_mem));

("-eager_fetch_unmodified",
    Arg.Bool (fun b ->
      let eager_mode =
        {!run_options.RunOptions.eager_mode with
            eager_fetch_unmodified = b}
      in
      run_options := {!run_options with RunOptions.eager_mode = eager_mode}),
    (Printf.sprintf "<bool> eagerly take fetches of unmodified locations (%b); exhaustive mode will run multiple times; at the end of each run we calculate the set of memory locations that were fetched and written to and use it in the next run, to a fixed-point." !run_options.RunOptions.eager_mode.eager_fetch_unmodified));

("-shared_memory",
    Arg.String (fun filename ->
      try Globals.shared_memory := Some (Model_aux.shared_memory_parse_from_file filename) with
      | Model_aux.SharedMemoryParsingError msg ->
          Printf.eprintf "Error, cannot parse the -shared_memory file: %s" msg;
          exit 1
    ),
    (Printf.sprintf "<file> sets the initial shared memory footprint for the -eager_local_mem option (overrides the litmus file key \"Shared-memory\")"));

("-shared_memory_str",
    Arg.String (fun s ->
      try Globals.shared_memory := Some (Model_aux.shared_memory_parse_from_string s) with
      | Model_aux.SharedMemoryParsingError msg ->
          Printf.eprintf "Error, cannot parse the -shared_memory_str argument: %s" msg;
          exit 1
    ),
    (Printf.sprintf "<string> same as -shared_memory, but read the shared memory footprint from the string"));

(** verbosity and output ********************************************)
("-v",
    Arg.Unit SO.increment_verbosity,
    Printf.sprintf " increase the level of verbosity (repeatable)");
("-q",
    Arg.Set quiet,
    Printf.sprintf " set the level of verbosity to quiet");
("-debug",
    Arg.Unit (fun () -> Debug.enable_debug ();
                        SO.verbosity := SO.Debug),
    " highest level of verbosity (for debugging)");
("-perfdebug",
    Arg.Unit (fun () -> Debug.enable_perfdebug ();
                        (* FIXME: (SF) high verbosity will affect performance *)
                        SO.verbosity := SO.Debug),
    " turn on performance debug");
("-dont",
    Arg.Unit (fun () -> quiet := true;
                        Globals.dont_tool := true),
    (Printf.sprintf " produce output suitable for the dont tool (%b)" !Globals.dont_tool));

("-pp_hex",
    Arg.Bool (fun b -> Globals.print_hex := b),
    (Printf.sprintf "<bool> print the histogram values in hexadecimal (%b)" !Globals.print_hex));

("-logdir",
    Arg.String (fun str -> match str with
                           | "" -> Globals.logdir := None
                           | _  -> Globals.logdir := Some str),
    (Printf.sprintf "<dir> logfile directory (empty to suppress log output) (%s)" (match !Globals.logdir with None -> "\"\"" | Some s -> s)));

("-allow_partial",
    Arg.Bool (fun b -> run_options := {!run_options with RunOptions.allow_partial = b}),
    (Printf.sprintf "<bool> print partial results if interrupted (%b)" !run_options.RunOptions.allow_partial));

("-backtrace",
    Arg.Bool (fun b -> Printexc.record_backtrace b),
    " record back trace (true); \"true\" is equivalent to \"OCAMLRUNPARAM=b\" and \"ocamlrun -b\"");

(** UI options ******************************************************)
("-state_output",
    Arg.String Screen.set_state_output,
    "<file> in interactive mode, print the current state to <file>");

("-trace_output",
    Arg.String Screen.set_trace_output,
    "<file> in interactive mode, print the current trace to <file>");

("-colours",
    Arg.Bool (fun b -> Globals.pp_colours := b),
    (Printf.sprintf "<bool> colours in interactive terminal output (turned on by -interactive true) (%b)" !Globals.pp_colours));

("-pp_style",
    Arg.String (fun s -> match s with
    | "full" -> Globals.pp_style := Globals.Ppstyle_full
    | "compact" -> Globals.pp_style := Globals.Ppstyle_compact
    | "screenshot" -> Globals.pp_style := Globals.Ppstyle_screenshot
    | _ -> fatal_error "bad -pp_style"),
    (Printf.sprintf "<%s> select pp style (%s)" (String.concat "|" (List.map Globals.pp_ppstyle Globals.ppstyles)) (Globals.pp_ppstyle !Globals.pp_style)));
("-pp_kind",
    Arg.String Globals.set_pp_kind,
    (Printf.sprintf "<Ascii|Latex|Html|Hash> select pp kind (%s)" (Globals.pp_pp_kind !Globals.pp_kind)));
("-pp_prefer_symbolic_values",
    Arg.Bool (fun b -> Globals.pp_prefer_symbolic_values := b),
    (Printf.sprintf "<bool> prefer symbolic values over hex (%b)" !Globals.pp_prefer_symbolic_values));

("-graph_backend",
    Arg.String Globals.set_graph_backend,
    (Printf.sprintf "<dot|tikz> select graph backend (%s)"
        (Globals.pp_graph_backend !Globals.graph_backend)));
("-dot",
    Arg.Bool (function
      | true  -> Globals.run_dot := Some Globals.RD_step
      | false -> Globals.run_dot := None),
    (Printf.sprintf "<bool> in interactive mode, generate execution graph out.pdf at every step (using graphviz via out.dot or tikz) (%b)" (!Globals.run_dot = Some Globals.RD_step)));
("-dot_final",
    Arg.Bool (function
      | true  -> Globals.run_dot := Some Globals.RD_final
      | false -> Globals.run_dot := None),
    (Printf.sprintf "<bool> in non-interactive mode, generate execution graph out.pdf for first complete execution (using graphviz via out.dot or tikz) (%b)" (!Globals.run_dot = Some Globals.RD_final)));
("-dot_final_ok",
    Arg.Bool (function
      | true  -> Globals.run_dot := Some Globals.RD_final_ok
      | false -> Globals.run_dot := None),
    (Printf.sprintf "<bool> in non-interactive mode, generate execution graph out.pdf for first final-constraint-satisfying execution (using graphviz via out.dot or tikz) (%b)" (!Globals.run_dot = Some Globals.RD_final_ok)));
("-dot_final_not_ok",
    Arg.Bool (function
      | true  -> Globals.run_dot := Some Globals.RD_final_not_ok
      | false -> Globals.run_dot := None),
    (Printf.sprintf "<bool> in non-interactive mode, generate execution graph out.pdf for first final-constraint-satisfying execution (using graphviz via out.dot or tikz) (%b)" (!Globals.run_dot = Some Globals.RD_final_not_ok)));
("-print_cexs",
    Arg.Bool (fun b -> Globals.print_cexs := b),
    (Printf.sprintf "<bool> in non-interactive mode, produce a file containing the candidate execution data of the allowed executions, in json format) (%b)" !Globals.print_cexs));
("-dot_dir",
    Arg.String (fun dir -> Globals.generateddir := Some dir),
    (Printf.sprintf "<dir> directory for generated graphs (%s)" (match !Globals.generateddir with None -> "\"\"" | Some s -> s)));
("-dot_show_regs",
    Arg.Bool (fun b -> Globals.ppg_regs := b),
    (Printf.sprintf "<bool> show registers in graphs (%b)" !Globals.ppg_regs));
("-dot_show_reg_rf",
    Arg.Bool (fun b -> Globals.ppg_reg_rf := b),
    (Printf.sprintf "<bool> show register rf edges in graphs (%b)" !Globals.ppg_reg_rf));
("-dot_show_transitions",
    Arg.Bool (fun b -> Globals.ppg_trans := b),
    (Printf.sprintf "<bool> show transitions in graphs (%b)" !Globals.ppg_trans));
("-dot_only_shared",
    Arg.Bool (fun b -> Globals.ppg_shared := b),
    (Printf.sprintf "<bool> show only shared-memory instruction (%b)" !Globals.ppg_shared));

("-dwarf",
    Arg.Bool (fun b -> Globals.use_dwarf := b),
    Printf.sprintf "<bool> use DWARF debug info from ELF file if available (%b)" !Globals.use_dwarf);
("-dwarf_show_all_variable_locations",
    Arg.Bool (fun b -> Globals.dwarf_show_all_variable_locations := b),
    Printf.sprintf "<bool> show all DWARF variable location data at each instruction (%b)" !Globals.dwarf_show_all_variable_locations);
("-dwarf_source_path",
    Arg.String (fun s -> Globals.dwarf_source_dir := s),
    Printf.sprintf "<string> directory containing source files, for DWARF debug output (%s)" !Globals.dwarf_source_dir);


(** version *)
("-version",
    Arg.Unit (fun () -> Versions.print (); exit 0),
    " show version information and exit (-v -version for more details)");

("-list_isas",
    Arg.Set should_list_isas,
    " list the supported ISA models");

("-print_console_help",
    Arg.Unit (fun () ->
        Console_help.help_message (Some 35)
        |> Screen.show_message (Globals.get_ppmode ());
        exit 0),
    " print the help message for the console and then exit");

(** deprecated ******************************************************)
("-auto",
    Arg.Unit (fun () -> fatal_error "-auto was renamed to -dont"),
    "");
("-follow_to",
    Arg.Unit (fun () -> fatal_error "-follow_to was deprecated"),
    "");
("-breakpoint_actual",
    Arg.Unit (fun () -> fatal_error "-breakpoint_actual was deprecated"),
    "");
("-sequential",
    Arg.Unit (fun () -> fatal_error "-sequential was deprecated"),
    "");
("-suppress_internal",
    Arg.Unit (fun () -> fatal_error "-suppress_internal was deprecated. The interactive mode command 'set eaget_pseudocode_internal true' has a simmilar effect to '-suppress_internal true'."),
    "");
("-dumb_terminal",
    Arg.Unit (fun () -> fatal_error "-dumb_terminal was deprecated (try building with 'UI=headless')"),
    "");
(*
("-test_syscall",
    Arg.Unit (fun () -> ignore (Syscalls.load_footprints_from_file "src_syscall_libs/syscall-introspect-tools/submodules/libfootprints-ocaml/spec.idl")),
    " test syscall machinery");
*)

];;


(* to make things nice and clean lets wrap everything as a function 'main' *)
let main = fun () ->
  let usage = "Usage: rmem [options]* filename (.litmus or binary)\n" ^
              "       rmem -help   to show options"
  in

  let sources = ref [] in

  let collect_file s = sources := s :: !sources in
  let usage' = usage ^ "\n" ^ "Options:" in
  let help outchan msg =
    Printf.fprintf outchan "%s\n\n" msg;
    Printf.fprintf outchan "Model options: %s\n\n" (String.concat "; " Model_aux.model_strings);
    Printf.fprintf outchan "Default model: %s\n" (Model_aux.pp_model Params.default_model_params)
  in

  begin try Arg.parse_argv Sys.argv (Arg.align opts) collect_file usage' with
  | Arg.Bad msg  -> help stderr msg; exit 1
  | Arg.Help msg -> help stdout msg; exit 0
  end;

  (* this ppmode is just for printing some messages, don't use it for anything else *)
  let ppmode = Globals.get_ppmode () in

  if !run_options.RunOptions.interactive && not (Unix.isatty Unix.stdout) then
    fatal_error "Output is not a terminal, '-interactive true' is not allowed.\n  Use '-interactive false'";

  if !should_list_isas then
    do_list_isas ();


  (* initialise pseudorandom number generator *)
  begin
    match !Globals.random_seed with
    | None ->
        Random.self_init ();
        let n = Random.bits () in
        (Random.init n);
        let c = open_out ("random_seed.dat") in
        (Printf.fprintf c "%i" n);
        let _ = close_out c in
        ()
    | Some n -> Random.init n
  end;

  if !sources = [] then fatal_error ("\n" ^ usage);

  (* FIXME: this will override the -colours *)
  if !run_options.RunOptions.interactive then Globals.pp_colours := true;

  if not Screen.interactive && !run_options.RunOptions.interactive then
    fatal_error "the tool was built without interactive support, '-interactive true' is not allowed";

  if !quiet then SO.verbosity := SO.Quiet;

  if !Globals.model_params.ss.ss_model = Flowing_storage_model
    && !Globals.flowing_topologies = []
    && not !Globals.topauto
  then
      fatal_error "'-model flowing' requires a topology ('-topauto true' or '-top <topology-list>')";

  if not !run_options.RunOptions.interactive
      && !Globals.run_dot = Some Globals.RD_step
  then
    SO.strLine "'-dot true' is ignored in non-interactive mode"
    |> Screen.show_warning ppmode;

  if !run_options.RunOptions.interactive then
    begin match !Globals.run_dot with
    | Some Globals.RD_final ->
        SO.strLine "'-dot_final true' is ignored in interactive mode"
        |> Screen.show_warning ppmode;
    | Some Globals.RD_final_ok ->
        SO.strLine "'-dot_final_ok true' is ignored in interactive mode"
        |> Screen.show_warning ppmode;
    | Some Globals.RD_final_not_ok ->
        SO.strLine "'-dot_final_not_ok true' is ignored in interactive mode"
        |> Screen.show_warning ppmode;
    | Some Globals.RD_step
    | None -> ()
    end;

  let files =
    List.map
      (fun s ->
          if not (Sys.file_exists s) then
            (* It is important to check that the file exists at this point
            as it is harder to catch this error later (e.g. linksem will just crash) *)
            Printf.sprintf "%s: No such file" s
            |> fatal_error;
          if Filename.check_suffix s ".litmus" then (Types.Litmus_file, s)
          else (Types.Binary_file, s)
      )
      (Misc.expand_argv !sources)
  in

  Pp.linebreak_init();

  if !SO.verbosity <> SO.Normal (* specially important for quiet mode as Luc's tools rely on this *)
      && not !Globals.dont_tool
  then
    SO.Concat [
      SO.strLine "#Version: %s" Versions.Rmem.describe; (* TODO: do we want more info here? *)
      SO.strLine "#Command line: %s" (String.concat " " @@ Array.to_list Sys.argv);
      SO.strLine "#Model: %s" (Model_aux.pp_model !Globals.model_params);
    ]
    |> Screen.show_message ppmode;

  List.iter
    (fun (filetype, name) ->
        begin try Top.from_file !run_options filetype name with
        | Misc.UserError s -> Printf.eprintf "Error in test %s: %s\n%!" name s
        | Misc.Fatal     s -> fatal_error s (* exit 1 *)
        | Misc.Exit -> ()
        end)
    files
;;

(* now let's call main *)
let _ = main ()
