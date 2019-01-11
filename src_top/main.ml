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

let check_inf_loop : bool option ref = ref None

let run_options : RunOptions.t ref =
  ref {RunOptions.default_options with RunOptions.interactive = Screen.interactive}

let pp_buffer_messages : bool option ref = ref None

let should_list_isas = ref false

let do_list_isas () =
  let s =
    String.concat " " (Utils.option_map
                       (fun isa ->
                         let module Isa = (val isa : Isa_model.ISADefs) in
                         if Isa.isa_defs_thunk () <> Interp_ast.Defs [] then
                           Some Isa.name
                         else
                           None)
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
("-loop_limit",
 Arg.Int (fun i -> Globals.model_params := { !Globals.model_params with MachineDefTypes.t = { (!Globals.model_params).MachineDefTypes.t with MachineDefTypes.thread_loop_unroll_limit = Some i }}),
 ("<integer> automatically unroll loops to this depth (default: off)"));
("-topauto",
    Arg.Bool (fun b -> Globals.topauto := b),
    (Printf.sprintf "<bool> for Flowing model: iterate over all possible topologies (%b)" !Globals.topauto));
("-top",
    Arg.String (fun s -> match Model_aux.parse_topologies s with
                         | Some t -> Globals.flowing_topologies := t
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
    Arg.String (fun filename -> Globals.branch_targets_parse_from_file filename),
    (Printf.sprintf "<file> set the initial branch targets for branch-register instructions (overrides the litmus file key \"Branch-targets\")"));

("-branch_targets_str",
    Arg.String (fun s -> Globals.branch_targets_parse_from_string s),
    (Printf.sprintf "<string> same as -branch_targets, but read the targets from the string"));

(** run modes *******************************************************)
("-interactive",
    Arg.Bool (fun b -> run_options := {!run_options with RunOptions.interactive = b}),
    Printf.sprintf "<bool> interactive or batch mode (%b)" (!run_options).RunOptions.interactive);

("-new_run",
    Arg.Unit (fun () -> Globals.use_new_run := true),
    (Printf.sprintf " use new_run.ml instead of run.ml"));

(** interactive mode options ****************************************)

("-follow",
    Arg.String Globals.set_follow,
    "<int-list> list of transition candidates to follow, semicolon-separated");

("-cmds",
    Arg.String (fun s -> if (String.length (String.trim s)) > 0 then Globals.ui_commands := Some (match !Globals.ui_commands with None -> s | Some s0 -> s0^";"^s)),
    "<cmd-list> list of UI commands to execute, semicolon-separated");

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
("-sequential",
    Arg.Unit (fun () -> run_options := {!run_options with RunOptions.sequential = true};
                        Globals.pp_colours := true),
    (Printf.sprintf " run in sequential mode, restricting to transition 0 at each step (%b)" !run_options.RunOptions.sequential));
("-new_sequential",
    Arg.Unit (fun () -> run_options := {!run_options with RunOptions.new_sequential = true};
                        Globals.pp_colours := true),
    (Printf.sprintf " run with state monad (%b)" !run_options.RunOptions.new_sequential));
("-breakpoint_actual",
    Arg.Unit (fun () -> Globals.interactive_auto := true;
                        Globals.breakpoint_actual := true;
                        Globals.pp_colours := true),
    " for RIT, run automatically until the fetch of the actual test instruction and then enter interactive mode");
("-auto_internal",
    Arg.Bool (fun b -> Globals.auto_internal := b),
    (Printf.sprintf "<bool> for interactive mode, automatically take internal transitions (%b)" !Globals.auto_internal));

(** optimizations ***************************************************)
("-suppress_internal",
    Arg.Bool (fun b -> run_options := {!run_options with RunOptions.suppress_internal = b}),
    (Printf.sprintf "<bool> suppress visibility of internal transitions, doing them within the Sail interpreter (%b)" !run_options.RunOptions.suppress_internal));
("-shallow_embedding",
    Arg.Bool (fun b -> run_options := {!run_options with RunOptions.interpreter = not b}),
    (Printf.sprintf "<bool> Run shallow embedding instruction semantics (%b)" !run_options.RunOptions.suppress_internal));
("-compare_analyses",
    Arg.Bool (fun b -> run_options := {!run_options with RunOptions.compare_analyses = b}),
    (Printf.sprintf "<bool> Compare the exhaustive and the handwritten analysis (%b)" !run_options.RunOptions.suppress_internal));
("-eager",
    Arg.Bool (fun b -> run_options :=
        {!run_options with RunOptions.eager_mode =
          if b then RunOptions.eager_mode_all_on
          else RunOptions.eager_mode_all_off}),
    (Printf.sprintf "<bool> eagerly take transitions that don't affect observable behaviour (%b)" (!run_options.RunOptions.eager_mode = RunOptions.eager_mode_all_on)));
("-eager_fetch_internal",
 Arg.Bool (fun b -> run_options :=
        {!run_options with RunOptions.eager_mode =
          if b then RunOptions.eager_mode_all_on
          else RunOptions.eager_mode_all_off}),
 "<bool> DEPRECATED synonym for -eager");
("-hash_prune",
    Arg.Bool (fun b -> run_options := {!run_options with RunOptions.hash_prune = b}),
    (Printf.sprintf "<bool> for batch mode, prune search tree using a hashmap of already seen states (%b)" !run_options.RunOptions.hash_prune));
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

("-prune_late_writes",
    Arg.Bool (fun b -> run_options := {!run_options with RunOptions.prune_late_writes = b}),
    (Printf.sprintf "<bool> for batch mode, don't explore traces where writes are committed after the Promising 'stop promising transition' (%b)" !run_options.RunOptions.prune_late_writes));

("-suppress_non_symbol_memory",
    Arg.Bool (fun b -> Globals.suppress_non_symbol_memory := b),
    (Printf.sprintf "<bool> suppress non-symbol memory for ELF input files (%b)" !Globals.suppress_non_symbol_memory));

("-check_inf_loop",
    Arg.Bool (fun b -> check_inf_loop := Some b),
    (* default: true when '-interactive true', except when '-random true' *)
    "<bool> in non-interactive (batch) mode terminate if an infinite loop is detected in the test");

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
    (Printf.sprintf "<n> fail if a trace is longer than n, \"none\" for no limit (%s)"
        (match !run_options.RunOptions.max_trace_length with
         | None -> "none"
         | Some n -> string_of_int n)));

("-eager_local_mem",
    Arg.Bool (fun b ->
      let eager_mode =
        {!run_options.RunOptions.eager_mode with
            MachineDefTypes.eager_local_mem = b}
      in
      run_options := {!run_options with RunOptions.eager_mode = eager_mode}),
    (Printf.sprintf "<bool> eagerly take thread-local memory access transitions (%b); exhaustive mode will run multiple times; at the end of each run we calculate the shared memory footprint and use it in the next run, to a fixed-point; the initial shared memory footprint can be set using the litmus file key \"Shared-memory=...\" and the -shared_memory option (empty otherwise)." !run_options.RunOptions.eager_mode.MachineDefTypes.eager_local_mem));


("-shared_memory",
    Arg.String (fun filename -> Globals.shared_memory_parse_from_file filename),
    (Printf.sprintf "<file> sets the initial shared memory footprint for the -eager_local_mem option (overrides the litmus file key \"Branch-targets\")"));

("-shared_memory_str",
    Arg.String (fun s -> Globals.shared_memory_parse_from_string s),
    (Printf.sprintf "<string> same as -shared_memory, but read the shared memory footprint from the string"));

(** verbosity and output ********************************************)
("-v",
    Arg.Unit Globals.increment_verbosity,
    (Printf.sprintf " increase the level of verbosity (repeatable) (levels: %s) (default: %s)" (String.concat "," (List.map Globals.pp_verbosity_level Globals.verbosity_levels)) (Globals.pp_verbosity_level !Globals.verbosity)));
("-q",
    Arg.Set quiet,
    (Printf.sprintf " set the level of verbosity to %s" (Globals.pp_verbosity_level (List.hd Globals.verbosity_levels))));
("-debug",
    Arg.Unit (fun () -> Debug.enable_debug ();
                        Globals.verbosity := Globals.Debug),
    " highest level of verbosity (for debugging)");
("-debug_sail_interp",
    Arg.Bool (fun b -> Globals.debug_sail_interp := b), (Printf.sprintf " enable Sail interpreter debug AST printing (%b)" !Globals.debug_sail_interp));
("-perfdebug",
    Arg.Unit (fun () -> Debug.enable_perfdebug ();
                        (* FIXME: (SF) high verbosity will affect performance *)
                        Globals.verbosity := Globals.Debug),
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
("-colours",
    Arg.Bool (fun b -> Globals.pp_colours := b),
    (Printf.sprintf "<bool> colours in interactive terminal output (turned on by -interactive true) (%b)" !Globals.pp_colours));
("-dumb_terminal",
 Arg.Bool (fun b -> Globals.dumb_terminal := b),
 (Printf.sprintf "<bool> disable readline, cursor movement, etc for dumb terminal or testing (%b)" !Globals.dumb_terminal));
("-suppress_newpage",
    Arg.Bool (fun b -> Globals.pp_suppress_newpage := b),
    (Printf.sprintf "<bool> suppress newpage in interactive mode (%b)" !Globals.pp_suppress_newpage));
("-buffer_messages",
    Arg.Bool (fun b -> pp_buffer_messages := Some b),
    Printf.sprintf "<bool> buffer messages until next prompt in interactive mode (%b)"
        begin match !pp_buffer_messages with
        | None -> !Globals.pp_buffer_messages
        | Some b -> b
        end);
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
    (Printf.sprintf "<bool> generate execution graph out.pdf at every step (using graphviz via out.dot or tikz) (%b)" (!Globals.run_dot = Some Globals.RD_step)));
("-dot_final",
    Arg.Bool (function
      | true  -> Globals.run_dot := Some Globals.RD_final
      | false -> Globals.run_dot := None),
    (Printf.sprintf "<bool> generate execution graph out.pdf for first complete execution (using graphviz via out.dot or tikz) (%b)" (!Globals.run_dot = Some Globals.RD_final)));
("-dot_final_ok",
    Arg.Bool (function
      | true  -> Globals.run_dot := Some Globals.RD_final_ok
      | false -> Globals.run_dot := None),
    (Printf.sprintf "<bool> generate execution graph out.pdf for first final-constraint-satisfying execution (using graphviz via out.dot or tikz) (%b)" (!Globals.run_dot = Some Globals.RD_final_ok)));
("-dot_final_not_ok",
    Arg.Bool (function
      | true  -> Globals.run_dot := Some Globals.RD_final_not_ok
      | false -> Globals.run_dot := None),
    (Printf.sprintf "<bool> generate execution graph out.pdf for first final-constraint-satisfying execution (using graphviz via out.dot or tikz) (%b)" (!Globals.run_dot = Some Globals.RD_final_not_ok)));
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

("-isa_defs_path",
 Arg.String (fun s -> Globals.isa_defs_path := Some s),
 Printf.sprintf "<string> directory containing ISA .defs files previously marshalled out (default: autodetect)");


(** version *)
("-version",
    Arg.Unit (fun () -> Versions.print (); exit 0),
    " show version information and exit (-v -version for more details)");

("-list_isas",
    Arg.Set should_list_isas,
    " list the supported ISA models");

("-print_console_help",
    Arg.Unit (fun () -> Printf.printf "%s" Console_help.help_message; exit 0),
    " print the help message for the console and then exit");

("-deterministic_output",
    Arg.Bool (fun b -> Globals.deterministic_output := b),
    Printf.sprintf "<bool> suppress non-deterministic output, e.g. runtime, command line, for test suite diffs (%b)" !Globals.deterministic_output);

(** deprecated ******************************************************)
("-auto",
    Arg.Unit (fun () -> fatal_error "-auto was renamed to -dont"),
    "");
("-follow_to",
    Arg.Int (fun n -> fatal_error "-follow_to was deprecated"),
    "");
    (* OLD:
    Arg.Int (fun n -> Globals.follow := (let rec f n = if n=0 then [] else Globals.Int 0::f (n-1) in f n)),
    "<int> number of transition candidates to follow");
    *)

(*
("-test_syscall",
    Arg.Unit (fun () -> ignore (Syscalls.load_footprints_from_file "src_syscall_libs/syscall-introspect-tools/submodules/libfootprints-ocaml/spec.idl")),
    " test syscall machinery");
*)


(** OLD OPTIONS: *****************************************************
("-truncate_thread_pp",
    Arg.Bool (fun b -> Globals.truncate_thread_pp := b),
    "<bool> truncate pp of long threads");
("-safe",
    Arg.Unit (fun () -> Globals.set_safemode true),
    " explore all paths, exhaustively (default)");
("-quick",
    Arg.Unit (fun () -> Globals.set_safemode false),
    " quick exploration of possibilities (experimental: enumerates all conceivable rfmaps, doing a search for each, but pruning branches that violate that rfmap) (opposite of -safe)");
("-O",
    Arg.Unit (fun () -> Globals.set_fast true),
    " enable (experimental) optmisations aiming at speed");
("-onlystate",
    Arg.Unit (fun () -> Globals.set_statematchmode true),
    " check only whether the final state mentioned explicitly in the test is observable (experimental)");
("-allstates",
    Arg.Unit (fun () -> Globals.set_statematchmode false),
    " explore all reachable states (default)");
("-candidates",
    Arg.String (fun s -> Globals.candidates := Some s),
    "<name> save candidates in file <name> and exit") ;
("-optoax",
    Arg.Bool (fun b -> Globals.optoax := b),
    "<bool> check all operational traces in axiomatic model");
("-axtoop",
    Arg.Bool (fun b -> Globals.axtoop := b),
    "<bool> check all axiomatic candidates for operational trace behaviour");
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
    Printf.fprintf outchan "Default model: %s\n" (Model_aux.pp_model MachineDefSystem.default_model_params)
  in

  begin try Arg.parse_argv Sys.argv (Arg.align opts) collect_file usage' with
  | Arg.Bad msg  -> help stderr msg; exit 1
  | Arg.Help msg -> help stdout msg; exit 0
  end;

  (* decide early whether to buffer printing so startup messages don't get accidentally swallowed *)
  begin match !pp_buffer_messages with
  | None -> Globals.pp_buffer_messages := !run_options.RunOptions.interactive
  | Some false -> Globals.pp_buffer_messages := false
  | Some true  ->
      if !run_options.RunOptions.interactive then
        Globals.pp_buffer_messages := true
      else
        (* this does not have to be an error *)
        fatal_error "'-buffer_messages true' is not allowed with '-interactive false'"
  end;

  (* this ppmode is just for printing some messages, don't use it for anything else *)
  let ppmode = Globals.get_ppmode () in

  Screen.show_debug ppmode "*** BEGIN ISA DEFS PATH AUTODETECTION";

  let check_isa_path path =
    List.for_all
      (fun isa ->
        let module Isa = (val isa : Isa_model.ISADefs) in
        let old_val = !Globals.isa_defs_path in
        Globals.isa_defs_path := Some path;
        try
          Isa.isa_defs_thunk ~no_memo:true () |> ignore;
          Globals.isa_defs_path := old_val;
          true
        with
        | Screen_base.Isa_defs_unmarshal_error (basename, msg) ->
            Screen.show_debug ppmode "(while checking currently ISA defs path candidate %s, got error '%s' for %s" path msg basename;
            Globals.isa_defs_path := old_val;
            false)
      Isa_model.all_isa_defs
  in

  let try_isa_path_candidate path_thunk =
    if !Globals.isa_defs_path = None then begin
        match path_thunk () with
        | Some path ->
            Screen.show_debug ppmode "trying candidate %s for ISA defs path" path;
            if check_isa_path path then
                Globals.isa_defs_path := Some path
        | None -> ()
    end;
    ()
  in

  if !Globals.isa_defs_path <> None then
    Screen.show_debug ppmode "have ISA defs path from command line";

  let isa_path_candidates = [
      (fun () ->
        try
          let path = Sys.getenv "ISA_DEFS_PATH" in
          Screen.show_debug ppmode "have ISA defs path from ISA_DEFS_PATH env var";
          Some path
        with
          Not_found -> None);

      (fun () -> Some (Filename.current_dir_name));

      (fun () ->
        match Pidpath.find_self () with
        | p -> Some (Filename.dirname p)
        | exception _ -> None
      );

      (fun () ->
        match Pidpath.find_self () with
        | p ->
            let open Filename in
            (* Also try pidpath/../.. because of build dir layout *)
            Some (concat (dirname p) (concat parent_dir_name parent_dir_name))
        | exception _ -> None
      );
    ] in

  List.iter try_isa_path_candidate isa_path_candidates;

  begin match !Globals.isa_defs_path with
  | Some path -> Screen.show_debug ppmode "found ISA defs in %s" path
  | None ->
      Screen.show_message ppmode "warning, no valid ISA defs path found, trying passing one with -isa_defs_path <path> or the ISA_DEFS_PATH env var. Pass -debug for more details"
  end;

  Screen.show_debug ppmode "*** ISA DEFS PATH AUTODETECTION COMPLETE";

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

  (* FIXME: (SF) The interpreter PP is not complete and might cause different
  state hash to compare as equal *)
  if !run_options.RunOptions.hash_prune &&
    not (!run_options.RunOptions.eager_mode.MachineDefTypes.eager_pseudocode_internal ||
           !Globals.model_params.MachineDefTypes.ss.MachineDefTypes.ss_model =
             MachineDefTypes.Promising_storage_model)
  then
    fatal_error "'-hash_prune true' is not safe without '-eager true'";

  (* FIXME: (SF) should this be forbidden? *)
  if !run_options.RunOptions.hash_prune && !Globals.breakpoint_actual then
    fatal_error "'-breakpoint_actual' mode does not allow '-hash_prune true'";

  if !run_options.RunOptions.interactive && !run_options.RunOptions.prune_restarts then
    fatal_error "'-prune_restarts true' requires '-interactive false'";

  if !run_options.RunOptions.interactive && !run_options.RunOptions.prune_discards then
    fatal_error "'-prune_discards true' requires '-interactive false'";

  if !run_options.RunOptions.prune_discards &&
      !Globals.model_params.MachineDefTypes.t.MachineDefTypes.thread_allow_tree_speculation
  then
    fatal_error "'-prune_discards true' requires '-model forbid_tree_speculation'";

  (* OLD OPTIONS:
  if our_runopts.Globals.statematchmode && not our_runopts.Globals.safemode then
    fatal_error "cannot combine '-quick' and -onlystate";
  *)

  begin match !check_inf_loop with
  | Some b -> run_options := {!run_options with RunOptions.check_inf_loop = b}
  | None   ->
      run_options :=
        { !run_options with
          RunOptions.check_inf_loop = !run_options.RunOptions.hash_prune
                                      && not (!run_options.RunOptions.interactive)
        }
  end;

  if !quiet then Globals.verbosity := Globals.Quiet;

  let is_flowing = (!Globals.model_params.MachineDefTypes.ss.MachineDefTypes.ss_model = MachineDefTypes.Flowing_storage_model) in

  if is_flowing then begin
    if !Globals.flowing_topologies = [] && not (!Globals.topauto) then
      fatal_error "'-model flowing' requires a topology";
    if !Globals.flowing_topologies <> [] && !Globals.topauto then
      fatal_error "cannot have both '-top ...' and '-topauto true'"
  end;

  let files =
    List.map
      (fun s -> if Filename.check_suffix s ".litmus" then (Types.Litmus_file, s)
                else (Types.Binary_file, s))
      (Misc.expand_argv !sources)
  in

  Pp.linebreak_init();

  if !Globals.verbosity <> Globals.Normal (* specially important for quiet mode as Luc's tools rely on this *)
      && not !Globals.dont_tool then
  begin
    Screen.show_message ppmode "#Version: %s" Versions.Rmem.describe; (* TODO: do we want more info here? *)
    if !Globals.deterministic_output then
      Screen.show_message ppmode "#Command line: (suppressed for deterministic comparison)"
    else
      Screen.show_message ppmode "#Command line: %s" (String.concat " " @@ Array.to_list Sys.argv);
    Screen.show_message ppmode "#Model: %s" (Model_aux.pp_model !Globals.model_params)
  end;

  begin try Top.from_files !run_options files with
  | Misc.Fatal msg           -> fatal_error msg
  | Globals.Interactive_quit -> exit 0
  end
;;

(* now let's call main *)
let _ = main ()
