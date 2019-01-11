(*=========================================================================================================*)
(*                                                                                                         *)
(*                rmem executable model                                                                    *)
(*                =====================                                                                    *)
(*                                                                                                         *)
(*  This file is:                                                                                          *)
(*                                                                                                         *)
(*  Copyright Shaked Flur, University of Cambridge                                             2014-2018   *)
(*  Copyright Peter Sewell, University of Cambridge                                 2011-2012, 2014-2016   *)
(*  Copyright Susmit Sarkar, University of St Andrews                                          2011-2015   *)
(*  Copyright Christopher Pulte, University of Cambridge                                       2015-2017   *)
(*  Copyright Kathy Gray, University of Cambridge (when this work was done)              2014-2015, 2017   *)
(*  Copyright Jon French, University of Cambridge                                              2017-2018   *)
(*  Copyright Pankaj Pawan, IIT Kanpur and INRIA (when this work was done)                     2011-2012   *)
(*  Copyright Luc Maranget, INRIA Paris                                                       2012, 2014   *)
(*  Copyright Robert Norton-Wright, University of Cambridge                                         2016   *)
(*  Copyright Ohad Kammar, University of Cambridge (when this work was done)                        2013   *)
(*  Copyright Dominic Mulligan, University of Cambridge (when this work was done)              2013-2014   *)
(*  Copyright Sela Mador-Haim, University of Pennsylvania (when this work was done)                 2012   *)
(*                                                                                                         *)
(*  All rights reserved.                                                                                   *)
(*                                                                                                         *)
(*  It is part of the rmem tool, distributed under the 2-clause BSD licence in                             *)
(*  LICENCE.txt.                                                                                           *)
(*                                                                                                         *)
(*=========================================================================================================*)

open MachineDefTypes

let logfile_name name =
  begin match !Globals.logdir with
  | Some logdir -> Filename.concat logdir ((Filename.basename name) ^ ".log")
  | None -> ""
  end

let rec range from until =
  if from >= until then
    []
  else
    from :: (range (from + 1) until)

(* run a litmus/ELF test from file/data *)
module Run_test (Test_file: Test_file.S) = struct
  (* name is either a file name to read the test from or the name of
  the test if data is provided *)
  let run (run_options: RunOptions.t) (name: string) (data: Test_file.data option) (isa_callback: (MachineDefTypes.instruction_semantics_mode -> unit) option) : unit =
    (* read the file/data *)
    let (test_info, test) =
      begin match data with
      | Some data -> Test_file.read_data name data isa_callback
      | None      -> Test_file.read_file name isa_callback
      end
    in

    (** FIXME: currently, RISC-V works only with shallow-embedding, hence
    we force 'interpreter = false' in run_options, even if the user explicitly
    did '-shallow_embedding false' *)
    let run_options =
      if test_info.Test.ism = RISCV_ism then
        {run_options with interpreter = false}
      else
        run_options
    in

    let module ISAModel = (val (Isa_model.make test_info.Test.ism)) in
    if ISAModel.ISADefs.isa_defs_thunk () = Interp_ast.Defs [] && run_options.RunOptions.interpreter then
      print_endline ("Warning: the interpreter ISA defs are missing");
    let module ConcModel  =
      (val (Concurrency_model.make
              (module ISAModel)
              !Globals.model_params.t.thread_model !Globals.model_params.ss.ss_model)) in

    (* calculate list of initial states (paired with their topology options) *)
    let initial_state_records =
      begin match !Globals.model_params.ss.ss_model with
      | Flowing_storage_model ->
          let topologies = Globals.get_topologies test_info.Test.thread_count in
          let thread_ids = range 0 test_info.Test.thread_count in
          if List.exists
               (fun top ->
                 let top_thread_ids = Model_aux.all_top_thread_ids top in
                 not (List.for_all (fun tid -> List.mem tid top_thread_ids) thread_ids))
               topologies then
            failwith "at least one flowing topology didn't have all threads of this test";
          let params_of_topo t = {!Globals.model_params with ss = {!Globals.model_params.ss with flowing_topology = t}} in
          let ps = List.map params_of_topo topologies in
          List.map (Test_file.initial_state_record test (module ISAModel.ISADefs)) ps

      | _ -> [Test_file.initial_state_record test (module ISAModel.ISADefs) !Globals.model_params]
      end
    in

    (* specially important for quiet mode as Luc's tools rely on this *)
    if !Globals.verbosity <> Globals.Normal
      && not !Globals.dont_tool then
    begin
      Screen.show_message (Globals.get_ppmode ()) "#Endianness: %s" (Globals.pp_endianness ());
    end;

    (* map-to-list to pp addresses properly *)
    let ppmode =
      match !Globals.pp_kind with
      | Globals.Hash ->
         Globals.ppmode_for_hashing
           (* with Globals.pp_instruction = Pp.pp_instruction_of_ism !Globals.model_params.t.thread_isa_info.ism;*)
      | Globals.Ascii | Globals.Html | Globals.Latex ->
          { (Globals.get_ppmode ()) with
              Globals.pp_symbol_table = test_info.Test.symbol_table;
              Globals.pp_dwarf_static = test_info.Test.dwarf_static;
              (* Globals.pp_instruction = Pp.pp_instruction_of_ism !Globals.model_params.t.thread_isa_info.ism; *)
          }
    in

    (* for ELF run: *)
    Globals.snapshot_data := (logfile_name test_info.Test.name, []);

    (* do stuff for each of those initial states *)
    if !Globals.use_new_run then
      let module Interact = New_interact.Make (ConcModel) in

      if run_options.RunOptions.interactive then
        (* interactive mode *)
        Interact.run_interactive run_options ppmode test_info initial_state_records
      else
        (* non-interactive mode (exhaustive/random) *)
        Interact.run_exhaustive_search run_options ppmode test_info initial_state_records

    else
      let module Runner = Run.Make (ConcModel) in
      Runner.calc_finals run_options ppmode test_info initial_state_records
end

let run_sequential (run_options: RunOptions.t) (name: string) : unit =
  (* read the file/data *)
  let (test_info, test) = Elf_test_file.read_file name None in

  let module ISAModel = (val (Isa_model.make test_info.Test.ism)) in

  (* check if the ISA model was compiled (if you get this failure run
  make again with the proper ISA=...) *)
  if ISAModel.ISADefs.isa_defs_thunk () = Interp_ast.Defs [] then
    print_endline ("Warning: the interpreter ISA defs are missing");

  let interp_context =
    let defs = ISAModel.ISADefs.isa_defs_thunk () in
    let (read_functions,read_taggeds,mem_writes,mem_eas,mem_vals,write_vals_tagged,barrier_functions,excl_res) =
      ISAModel.ISADefs.isa_memory_access in
    let externs = ISAModel.ISADefs.isa_externs in
    Interp_inter_imp.build_context
      !Globals.debug_sail_interp defs read_functions read_taggeds mem_writes mem_eas mem_vals write_vals_tagged barrier_functions excl_res externs in
  let ism = test_info.Test.ism in

  let initial_state =
    Elf_test_file.initial_state_sequential
      (!Globals.model_params.t.thread_isa_info)
      test
      (module ISAModel.ISADefs)
      ism
      interp_context in

  (* map-to-list to pp addresses properly *)
  let ppmode =
    { (Globals.get_ppmode ()) with
        Globals.pp_symbol_table = test_info.Test.symbol_table;
        Globals.pp_dwarf_static = test_info.Test.dwarf_static;
        (* Globals.pp_instruction = Pp.pp_instruction_of_ism !Globals.model_params.t.thread_isa_info.ism *)
    }
  in

  (* for ELF run: *)
  Globals.snapshot_data := (logfile_name test_info.Test.name, []);

  (* do stuff for each of those initial states *)
  Run_sequential.calc_final ppmode (!Globals.model_params.t.thread_isa_info)
                            test_info.Test.name initial_state




module Run_litmus = Run_test(Litmus_test_file)
module Run_elf    = Run_test(Elf_test_file)

let from_litmus_data
    (run_options: RunOptions.t)
    (name:        string)
    (data:        Litmus_test_file.data)
    (isa_callback: (MachineDefTypes.instruction_semantics_mode -> unit) option)
    : unit
  =
  Run_litmus.run run_options name (Some data) isa_callback

let from_ELF_data
    (run_options: RunOptions.t)
    (name:        string)
    (data:        Elf_test_file.data)
    (isa_callback: (MachineDefTypes.instruction_semantics_mode -> unit) option)
    : unit
  =
  Run_elf.run run_options name (Some data) isa_callback

let from_file
    (run_options: RunOptions.t)
    (filetype:    Types.filetype)
    (name:        string)
    : unit
  =
  begin match filetype with
  | Types.Litmus_file ->
     Run_litmus.run run_options name None None
  | Types.Binary_file ->
     if run_options.RunOptions.new_sequential
     then run_sequential run_options name
     else Run_elf.run run_options name None None
  end

let from_files
    (run_options:         RunOptions.t)
    (filetypes_and_names: (Types.filetype * string) list)
    : unit
  =
  List.iter
    (fun (filetype, name) ->
        begin try from_file run_options filetype name with
        | Misc.UserError s -> Printf.eprintf "Error in test %s: %s\n%!" name s
        | Misc.Exit -> ()
        end)
    filetypes_and_names
