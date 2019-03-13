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
(*  Copyright Luc Maranget, INRIA, Paris, France                                              2012, 2014   *)
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

open MachineDefParams

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
  let run (run_options: RunOptions.t) (name: string) (data: Test_file.data option) (isa_callback: (MachineDefInstructionSemantics.instruction_semantics_mode -> unit) option) : unit =
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
      if test_info.Test.ism = MachineDefInstructionSemantics.RISCV_ism then
        {run_options with RunOptions.interpreter = false}
      else
        run_options
    in

    let run_options =
      let open RunOptions in
      match !Globals.model_params.shared_memory with
      | Some sm ->
        { run_options with
          eager_mode =
            { run_options.eager_mode with
              em_shared_memory = sm;
            };
        }
      | None -> run_options
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
      Screen_base.otStrLine "#Endianness: %s" (Globals.pp_endianness ())
      |> Screen.show_message (Globals.get_ppmode ());
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

    let module Interact = New_interact.Make (ConcModel) in
    if run_options.RunOptions.interactive then
      (* interactive mode *)
      Interact.run_interactive run_options ppmode test_info initial_state_records
    else
      (* non-interactive mode (exhaustive/random) *)
      Interact.run_search run_options ppmode test_info initial_state_records

end

module Run_litmus = Run_test(Litmus_test_file)
module Run_elf    = Run_test(Elf_test_file)

let from_litmus_data
    (run_options: RunOptions.t)
    (name:        string)
    (data:        Litmus_test_file.data)
    (isa_callback: (MachineDefInstructionSemantics.instruction_semantics_mode -> unit) option)
    : unit
  =
  Run_litmus.run run_options name (Some data) isa_callback

let from_ELF_data
    (run_options: RunOptions.t)
    (name:        string)
    (data:        Elf_test_file.data)
    (isa_callback: (MachineDefInstructionSemantics.instruction_semantics_mode -> unit) option)
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
     Run_elf.run run_options name None None
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
