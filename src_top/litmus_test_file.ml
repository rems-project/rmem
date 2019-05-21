(*========================================================================================*)
(*                                                                                        *)
(*                rmem executable model                                                   *)
(*                =====================                                                   *)
(*                                                                                        *)
(*  This file is:                                                                         *)
(*                                                                                        *)
(*  Copyright Shaked Flur, University of Cambridge                            2016-2018   *)
(*  Copyright Christopher Pulte, University of Cambridge                      2016-2018   *)
(*  Copyright Jon French, University of Cambridge                             2016-2018   *)
(*  Copyright Robert Norton-Wright, University of Cambridge                        2017   *)
(*  Copyright Peter Sewell, University of Cambridge                                2016   *)
(*  Copyright Luc Maranget, INRIA, Paris, France                                   2017   *)
(*  Copyright Linden Ralph, University of Cambridge (when this work was done)      2017   *)
(*                                                                                        *)
(*  All rights reserved.                                                                  *)
(*                                                                                        *)
(*  It is part of the rmem tool, distributed under the 2-clause BSD licence in            *)
(*  LICENCE.txt.                                                                          *)
(*                                                                                        *)
(*========================================================================================*)

open Test

include Litmus_test_file_base



let check_configuration () = 
  let open Params in
  let open Globals in
  let params = !Globals.model_params in
  begin match (params.t.thread_isa_info.ism, params.ss.ss_model, params.t.thread_model) with
    | (AARCH64_ism AArch64HandSail, POP_storage_model, POP_thread_model _)
    | (AARCH64_ism AArch64HandSail, Flowing_storage_model, POP_thread_model _)
    | (AARCH64_ism AArch64HandSail, Flat_storage_model, POP_thread_model _)
    | (AARCH64_ism AArch64HandSail, NOP_storage_model, POP_thread_model _)
    | (AARCH64_ism AArch64HandSail, Promising_storage_model, Promising_thread_model)
    | (AARCH64_ism AArch64HandSail, Flat_storage_model, Relaxed_thread_model) ->
       ()

    | (AARCH64_ism AArch64GenSail, POP_storage_model, POP_thread_model _)
    | (AARCH64_ism AArch64GenSail, Flowing_storage_model, POP_thread_model _)
    | (AARCH64_ism AArch64GenSail, Flat_storage_model, POP_thread_model _)
    | (AARCH64_ism AArch64GenSail, NOP_storage_model, POP_thread_model _)
    | (AARCH64_ism AArch64GenSail, Promising_storage_model, Promising_thread_model)
    | (AARCH64_ism AArch64GenSail, Flat_storage_model, Relaxed_thread_model) ->
       ()

    | (AARCH64_ism _, PLDI11_storage_model, PLDI11_thread_model) ->
        Printf.eprintf "The pldi11 model does not support the AArch64 architecture\n";
        exit 1

    | (PPCGEN_ism, POP_storage_model, POP_thread_model _)
    | (PPCGEN_ism, Flat_storage_model, POP_thread_model _)
    | (PPCGEN_ism, PLDI11_storage_model, PLDI11_thread_model)
    | (PPCGEN_ism, Flat_storage_model, Relaxed_thread_model) ->
       ()

    | (PPCGEN_ism, Flowing_storage_model, POP_thread_model _) ->
        Printf.eprintf "The flowing model does not support the PPC architecture\n";
        exit 1
    | (PPCGEN_ism, NOP_storage_model, POP_thread_model _) ->
        Printf.eprintf "The nop model does not support the PPC architecture\n";
        exit 1

    | (MIPS_ism, POP_storage_model, POP_thread_model _)
    | (MIPS_ism, Flowing_storage_model, POP_thread_model _)
    | (MIPS_ism, Flat_storage_model, POP_thread_model _)
    | (MIPS_ism, NOP_storage_model, POP_thread_model _)
    | (MIPS_ism, Flat_storage_model, Relaxed_thread_model) ->
       ()

    | (MIPS_ism, PLDI11_storage_model, PLDI11_thread_model) ->
        Printf.eprintf "The pldi11 model does not support the MIPS architecture\n";
        exit 1

    | (RISCV_ism, POP_storage_model, POP_thread_model _)
    | (RISCV_ism, Flowing_storage_model, POP_thread_model _)
    | (RISCV_ism, Flat_storage_model, POP_thread_model _)
    | (RISCV_ism, NOP_storage_model, POP_thread_model _)
    | (RISCV_ism, Promising_storage_model, Promising_thread_model)
    | (RISCV_ism, TSO_storage_model, TSO_thread_model)
    | (RISCV_ism, Flat_storage_model, Relaxed_thread_model) ->
       ()

    | (X86_ism, TSO_storage_model, TSO_thread_model)
    | (X86_ism, Flat_storage_model, Relaxed_thread_model) ->
       ()

    | _ ->
        Printf.eprintf "Unsupported model and architecture configuration\n";
        exit 1
    end


let read_channel_and_update_globals
      (name: string)
      (in_chan: lex_input)
      (isa_callback: (InstructionSemantics.instruction_semantics_mode -> unit) option) 
      (aarch64gen: bool)
    : (Test.info * test)
  =

  let ((info,test),isa_info,maybe_x86_syntax) = 
    read_channel name in_chan aarch64gen (fun _ -> !Globals.final_cond) in

  Globals.set_model_ism isa_info;
  check_configuration ();  

  begin match isa_callback with
  | Some f -> f isa_info.ism
  | _ -> ()
  end;

  begin match maybe_x86_syntax with
  | Some X86_syntax_gas ->   Globals.x86syntax := Some X86_gas
  | Some X86_syntax_intel -> Globals.x86syntax := Some X86_intel
  | _ -> ()
  end;

  if !Globals.branch_targets = None then begin
    match List.assoc "Branch-targets" info.Test.info with
    | branch_targets ->
        begin try Globals.branch_targets := Some (Model_aux.branch_targets_parse_from_string branch_targets) with
        | Model_aux.BranchTargetsParsingError msg ->
            Printf.eprintf "%s\n" msg;
            exit 1
        end
    | exception Not_found -> ()
  end;

  if !Globals.shared_memory = None then begin
    match List.assoc "Shared-memory" info.Test.info with
    | shared_memory ->
        begin try Globals.shared_memory := Some (Model_aux.shared_memory_parse_from_string shared_memory) with
        | Model_aux.SharedMemoryParsingError msg ->
            Printf.eprintf "%s" msg;
            exit 1
        end
    | exception Not_found -> ()
  end;

  Globals.add_bt_and_sm_to_model_params info.symbol_table;

  (* HACK: *)
  (* CP: I'm commenting out because I don't know what to do with
     it. It makes no sense to call the tikz module from here *)

  if !Globals.graph_backend = Globals.Tikz then
    begin match !Globals.run_dot with
    | Some Globals.RD_final
    | Some Globals.RD_final_ok
    | Some Globals.RD_final_not_ok
        -> Tikz.make_init_state info test
    | None
    | Some Globals.RD_step
        -> ()
    end;

  (info,test)




let read_data (name: string) (data: data) (isa_callback: (InstructionSemantics.instruction_semantics_mode -> unit) option) : Test.info * test =
  read_channel_and_update_globals
    name
    (LexInString data)
    isa_callback
    !Globals.aarch64gen


let read_file (name: string) (isa_callback: (InstructionSemantics.instruction_semantics_mode -> unit) option) : Test.info * test =
   Misc.input_protect begin
       fun (in_chan: in_channel) ->
       read_channel_and_update_globals
         name
         (LexInChannel in_chan)
         isa_callback
         !Globals.aarch64gen
   end name

  




(********************************************************************)

module Make_litmus_parser_to_xml
    (Arch: Arch.S)
    (LexParse: GenParser.LexParse with type instruction = Arch.parsedPseudo)
    =
struct
  module Parser = GenParser.Make(GenParser.DefaultConfig)(Arch)(LexParse)
  module Translator = Translate_to_xml.Make(Arch)

  let parse (in_chan: lex_input) (test_splitted: Splitter.result) =
    (* parse splitted test *)
    begin match in_chan with
    | LexInChannel c -> Parser.parse c test_splitted
    | LexInString s  -> Parser.parse_string s test_splitted
    end
    |> Translator.translate_test test_splitted
end



let channel_to_xml (name: string) (in_chan: lex_input) =
  (* First split the input file in sections *)
  let module SPL = Splitter.Make(Splitter.Default) in
  let test_splitted =
    begin match in_chan with
    | LexInChannel c -> SPL.split name c
    | LexInString s  -> SPL.split_string name s
    end
  in

  begin match test_splitted.Splitter.arch with
  | `PPC           ->
      let module Parser = Make_litmus_parser_to_xml(PPC)(PPCLexParse) in
      Parser.parse in_chan test_splitted
  | `AArch64       ->
      let module Parser = Make_litmus_parser_to_xml(AArch64HGen)(AArch64HGenLexParse) in
      Parser.parse in_chan test_splitted
  | `MIPS          ->
      let module Parser = Make_litmus_parser_to_xml(MIPSHGen)(MIPSHGenLexParse) in
      Parser.parse in_chan test_splitted
  | `RISCV         ->
      let module Parser = Make_litmus_parser_to_xml(RISCVHGen)(RISCVHGenLexParse) in
      Parser.parse in_chan test_splitted
  | `X86           ->
        let syntax = begin try List.assoc "Syntax" test_splitted.Splitter.info with
                     | Not_found -> "intel" end in
        if (syntax = "gas") then
          let module Parser = Make_litmus_parser_to_xml(X86HGen)(X86HGenLexParseGas) in
          Parser.parse in_chan test_splitted
        else 
          let module Parser = Make_litmus_parser_to_xml(X86HGen)(X86HGenLexParseIntel) in
          Parser.parse in_chan test_splitted
  | _ -> Warn.fatal "unknown architecture"
  end

let to_xml (file: string) =
  Misc.input_protect
    (fun (c: in_channel) -> channel_to_xml file (LexInChannel c))
    file


let initial_state_record 
      (test: test)
      (isa_defs: (module Isa_model.ISADefs))
      (model: Params.model_params) = 
  initial_state_record_base
    (Globals.get_endianness ())
    (!Globals.aarch64gen)
    test
    isa_defs
    model
