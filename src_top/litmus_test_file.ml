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

(* Arch.Config *)
module Arch_config =
struct
  let memory = Memory.Direct
  let cautious = true
  let hexa = true
  let asmcomment = None
end

module SymbConstant = SymbConstant.Make(ParsedConstant.StringScalar)
module PPC = PPCGenArch.Make(Arch_config)(SymbConstant)

module PPCLexParse = struct
  type instruction = PPC.pseudo
  type token = PPCGenParser.token

  module LexConfig = struct let debug = false end

  module PL = PPCGenLexer.Make(LexConfig)
  let lexer = PL.token
  let parser = PPCGenParser.main
end

module AArch64HGen = AArch64HGenArch.Make(Arch_config)(SymbConstant)

module AArch64HGenLexParse =
struct
  type instruction = AArch64HGen.pseudo
  type token = AArch64HGenParser.token

  module LexConfig = struct let debug = false end

  module PL = AArch64HGenLexer.Make(LexConfig)
  let lexer = PL.token
  let parser = AArch64HGenParser.main
end

module MIPSHGen = MIPSHGenArch.Make(Arch_config)(SymbConstant)

module MIPSHGenLexParse =
struct
  type instruction = MIPSHGen.pseudo
  type token = MIPSHGenParser.token

  module LexConfig = struct let debug = false end

  module PL = MIPSHGenLexer.Make(LexConfig)
  let lexer = PL.token
  let parser = MIPSHGenParser.main
end

module RISCVHGen = RISCVHGenArch.Make(Arch_config)(SymbConstant)

module RISCVHGenLexParse =
struct
  type instruction = RISCVHGen.pseudo
  type token = RISCVHGenParser.token

  module LexConfig = struct let debug = false end

  module PL = RISCVHGenLexer.Make(LexConfig)
  let lexer = PL.token
  let parser = RISCVHGenParser.main
end

module X86HGen = X86HGenArch.Make(Arch_config)(SymbConstant)

module X86HGenLexParseGas =
struct
  type instruction = X86HGen.pseudo
  type token = X86HGenParser.token

  module LexConfig = struct let debug = false end

  module PL = X86HGenLexer.Make(LexConfig)
  let lexer = PL.token
  let parser = X86HGenParser.main
end

module X86HGenLexParseIntel =
struct
  type instruction = X86HGen.pseudo
  type token = X86HGenParserIntel.token

  module LexConfig = struct let debug = false end

  module PL = X86HGenLexerIntel.Make(LexConfig)
  let lexer = PL.token
  let parser = X86HGenParserIntel.main
end

type lex_input = | LexInChannel of in_channel
                 | LexInString of string

module GenParserConfig : GenParser.Config = struct
  let debuglexer = false
  let check_kind _ = None
  let check_cond _ = !Globals.final_cond
  let verbose = 0
end

module Make_litmus_parser
    (Arch: Arch_litmus.S with type V.Scalar.t = string)
    (TransSail: Trans.TransSail with type instruction = Arch.instruction)
    (LexParse: GenParser.LexParse with type instruction = Arch.parsedPseudo)
    =
struct
  module Parser = GenParser.Make(GenParserConfig)(Arch)(LexParse)
  module Translator = Translate.Make(Arch)(TransSail)

  let parse (in_chan: lex_input) (test_splitted: Splitter.result) : TransSail.instruction_ast Test.test =
    (* parse splitted test *)
    let parsedt =
      begin match in_chan with
      | LexInChannel c -> Parser.parse c test_splitted
      | LexInString s  -> Parser.parse_string s test_splitted
      end
    in
    (* translate to Sail AST *)
    Translator.translate_test parsedt

end

type 'i test = 'i Test.test
type data = string

let isa_model_of_arch arch = 
  let open Isa in
  match arch with
  | `PPC           -> PPC
  | `AArch64 when !Globals.aarch64gen -> AARCH64 Gen
  | `AArch64 when not !Globals.aarch64gen -> AARCH64 Hand
  | `MIPS          -> MIPS
  | `RISCV         -> RISCV
  | `X86           -> X86
  | _ ->
     Printf.eprintf "Unsupported architecture\n";
     exit 1


let test_info (test: 'i Test.test) (name: string) : Test.info =
  let isa_model = isa_model_of_arch test.arch in
  let bindings = Test.LocationMap.bindings test.mem_addr_map in

  let symbol_map =
    List.map
      (fun (symbol, (addr, size)) -> (symbol, (Nat_big_num.of_int 0,
                                               Nat_big_num.of_int size,
                                               Sail_impl_base.integer_of_address addr,
                                               None,
                                               Nat_big_num.of_int 0)))
      bindings
  in

  let symbol_table_pp =
    List.map
      (fun (symbol, (addr,size)) -> ((addr, size), symbol))
      bindings
  in

  (* get the address to code label map for pp *)
  let prog_labels_pp =
    List.concat
      (List.map
        (fun (tid, _, labelmap) ->
          List.map
            (fun (label, n) ->
              let addr = Sail_impl_base.address_of_integer (Globals.aval_of_inst_index_num tid n) in
              ((addr, 4), label))
            labelmap)
        test.prog)
  in

  let tr_locs locs  =  (* translate a list of locations into a pair regs X memory footprint  *)
  (* fold-right to preserve the order (probably not important) *)
    let (reg_locations, mem_locations) =
      List.fold_right
        (function
          | (Test.C.Loc_reg (tid, name)) -> fun (regs, mems) -> ((tid, name) :: regs, mems)
          | (Test.C.Loc_mem addr)        -> fun (regs, mems) -> (regs, addr :: mems))
        locs
        ([], [])
    in

    let mem_footprints =
      let (_, addr_map) = List.split (Test.LocationMap.bindings test.Test.mem_addr_map) in
    (* TODO: this is a quick fix; I suspect the fact address uses Big_int
    means we should never use it in List.assoc *)
      let int64_addr_map =
        List.map
          (fun (a, s) -> (Nat_big_num.to_int64 (Sail_impl_base.integer_of_address a), s))
          addr_map
      in
      List.map
        (fun int64_addr ->
          let addr = Sail_impl_base.address_of_integer (Nat_big_num.of_int64 int64_addr) in
          let size =
            try List.assoc int64_addr int64_addr_map with
            | Not_found -> failwith "unmapped memory location"
          in
          (addr, size))
        mem_locations
    in

    (reg_locations, mem_footprints)
  in
    
  let show_locations = (Test.C.locations test.constr) @ test.flocs in

  (* locations we will show in the histogram *)
  let (show_reg_locations, show_mem_footprints) =
    show_locations
    |> List.sort_uniq compare
    |> tr_locs
  in

  (* locations needed for checking the "filter" *)
  let (filter_reg_locations, filter_mem_footprints) =
    match test.filter with
    | None -> ([], [])
    | Some filter ->
        Test.C.locations_prop filter
        |> List.sort_uniq compare
        |> tr_locs
  in




  { Test.name           = name;
    Test.isa_model      = isa_model;
    Test.thread_count   = List.length test.Test.prog;
    Test.symbol_map     = symbol_map;
    Test.symbol_table   = symbol_table_pp @ prog_labels_pp;
    Test.dwarf_static   = None;

    Test.show_regs   = show_reg_locations;
    Test.show_mem    = show_mem_footprints;
    Test.filter_regs = filter_reg_locations;
    Test.filter_mem  = filter_mem_footprints;


    Test.info           = test.Test.info;
    Test.filter         = test.Test.filter;
    Test.constr         = test.Test.constr;
  }





let initial_state_record
    (test:      'i test)
    (isa:       'i Isa.isa)
    (model:     Params.model_params)
  : 'i Params.initial_state_record
  =

  let open Params in
  let open Isa in
  let open InstructionSemantics in

  (* list of tids 0,1,... *)
  let tids = Lem_list.genlist (fun n -> n) (List.length test.prog) in

  (* write events for the initial memory values *)
  let (init_write_events, ist) =
    (* initial values of memory vars from the test *)
    let init_mem_values =
      (* construct a '0' memory_value of size 'size' *)
      let zero_of_size size =
        Sail_impl_base.memory_value_of_integer
          (Globals.get_endianness ())
          size
          (Nat_big_num.of_int 0)
      in

      let (_, mem_addrs) = List.split (Test.LocationMap.bindings test.mem_addr_map) in
      List.map
        (fun (address, size) ->
            let value =
              begin match Lem_list.lookupBy (=) address test.init_mem_state with
              | Some value -> value
              | None       -> zero_of_size size (* memory var has no initial
                                                value in test, set it to 0 *)
              end
            in ((address, size), value))
        mem_addrs
    in

    let (write_events, ist) =
      List.fold_left
        (fun (writes, ist) (footprint, value) ->
          let (new_ioid, ist) = FreshIds.gen_fresh_id ist in
          let ioid_ist = FreshIds.initial_id_state new_ioid in
          let (new_writes, ioid_ist) =
            Events.make_write_events_big_split
              ioid_ist init_thread new_ioid footprint value Sail_impl_base.Write_plain
          in
          (new_writes @ writes, ist))
        ([], FreshIds.initial_id_state init_thread)
        init_mem_values
    in
    let write_events = List.sort (fun w1 w2 -> Pervasives.compare w1.Events.w_addr w2.Events.w_addr) write_events in

    (write_events, ist)
  in

  (* map from memory address to instruction.
  each thread is placed into memory at 0x50000 + 0x1000 * tid,
  so 0x50000, 0x51000, ... *)
  let prog_map =
    let prog_in_mem =
      List.concat
        (List.map
            (fun (tid, instructions, _) ->
                List.mapi (fun i inst -> (Globals.aval_of_inst_index_num tid (i * 4), inst)) instructions)
            test.prog)
    in

    (* map-from-list: *)
    List.fold_left
      (fun k (a, i) -> Pmap.add a i k)
      (Pmap.empty Nat_big_num.compare)
      prog_in_mem
  in

  let (initial_writes, ist) =
    if Params.is_fetch_atomic model.ss then
      (init_write_events, ist)
    else
      let (initial_prog_writes, ist) =
        Pmap.fold (fun addr instr (ws,ist) ->
            let addr = (Sail_impl_base.address_of_integer addr) in
            let footprint = (addr, 4) in  (* TODO: per-arch ... *)
            let (new_ioid, ist) = FreshIds.gen_fresh_id ist in
            let ioid_ist = FreshIds.initial_id_state new_ioid in
            let endianness = Globals.get_endianness () in
            let value = isa.instruction_semantics.encode_instruction instr endianness in
            let (new_writes, ioid_ist) =
              Events.make_write_events_big_split
                ioid_ist init_thread new_ioid footprint value Sail_impl_base.Write_plain in
            (ws @ new_writes, ist)
        ) prog_map ([], ist)
      in
      (init_write_events @ initial_prog_writes, ist)
  in

  let prog _ (address: Sail_impl_base.address) : 'i InstructionSemantics.fetch_and_decode_outcome =
    let address' = Sail_impl_base.integer_of_address address in
    begin match Pmap.lookup address' prog_map with
    | None -> InstructionSemantics.FDO_illegal_fetch_address
    | Some inst ->
       InstructionSemantics.FDO_success (address, None, inst)
    end
  in

  (* provide the initial fetch address for each thread *)
  let initial_fetch_address tid : Sail_impl_base.address option =
    Some (Sail_impl_base.address_of_integer (Globals.aval_of_inst_index_num tid 0))
  in

  let return_addresses =
    List.map
      (fun (tid, instructions, _) ->
          let aval = Globals.aval_of_inst_index_num tid (((List.length instructions) - 1) * 4) in
          (tid, Sail_impl_base.address_of_integer aval))
      test.prog
  in

  (* Set up TPIDR registers -- used for thread local storage, specifically
     seems to be a pointer to the TCB. For now, just put the thread id there *)
  let reg_values : ((Nat_num.nat * Sail_impl_base.reg_base_name) * Sail_impl_base.register_value) list =
    let open Isa in
    match test.arch with
    | `AArch64 ->
       let tpidr_el0 =
         let registerdata =
           if !Globals.aarch64gen then
             Aarch64Isa.aarch64gen_isa.register_data_info
           else
             Aarch64Isa.aarch64hand_isa.register_data_info
         in
         match RegUtils.reg_from_data registerdata "TPIDR_EL0" with
         | Some r -> r
         | None -> failwith ("\"TPIDR_EL0\" is not in 'register_data_info'")
       in
       (List.map
          (fun tid ->
            ((tid, "TPIDR_EL0"), Sail_impl_base.register_value_for_reg_of_integer tpidr_el0 (Nat_big_num.of_int tid)))
          tids)
       @ test.init_reg_state
    | `RISCV ->
       let registerdata = RiscvIsa.riscv_isa.register_data_info in
       let get_reg r = match RegUtils.reg_from_data registerdata r with
         | Some r -> r
         | None -> failwith (r ^ " not in register_data_info")
       in
       let each_thread tid =
         [
           ((tid, "cur_privilege"), Sail_impl_base.register_value_for_reg_of_integer (get_reg "cur_privilege") (Nat_big_num.of_int 0));
           ((tid, "misa"), Sail_impl_base.register_value_for_reg_of_integer (get_reg "misa") (Nat_big_num.of_string "0x8000000000000881")); (* RV64IMA *)
           ((tid, "mstatus"), Sail_impl_base.register_value_for_reg_of_integer (get_reg "mstatus") (Nat_big_num.of_string "0x0000000a00000000")); (* RV64 in S and U also *)
         ]
       in
       (List.flatten (List.map each_thread tids)) @ test.init_reg_state

    | _ ->
       test.init_reg_state
  in

  (* initial values for registers based on test (default is 0) *)
  let init_reg_value reg_data t r =
    begin try List.assoc (t, r) reg_values with
    | Not_found ->
        (* currently all registers that are not explicitly initialised
        in test are set to zero *)
        RegUtils.register_state_zero reg_data t r
    end
  in

  let reg_values' = List.map (fun ((tid,rbn),v) -> (tid,(rbn,v))) reg_values in
  let init_reg_data t =
    let init_reg_data' = List.filter (fun (tid,_) -> t = tid) reg_values' in
    List.map snd init_reg_data' in


  let isa' =
    let fixed_pseudo_registers' =
      let open Sail_impl_base in
      match isa.isa_model with
      | PPC ->
          let endianness =
            match Globals.get_endianness () with
            | E_little_endian -> register_value_zeros D_increasing 1 0
            | E_big_endian    -> register_value_ones  D_increasing 1 0
          in
          (Reg_slice ("bigendianmode", 0, D_increasing, (0,0)), endianness) ::
          isa.fixed_pseudo_registers

      | AARCH64 _ ->
          let endianness =
            match Globals.get_endianness () with
            | E_little_endian -> register_value_zeros D_decreasing 1 0
            | E_big_endian    -> register_value_ones  D_decreasing 1 0
          in
          (Reg_field ("SCTLR_EL1", 31, D_decreasing, "E0E", (24,24)), endianness) ::
          (Reg_field ("SCTLR_EL1", 31, D_decreasing, "EE",  (25,25)), endianness) ::
          isa.fixed_pseudo_registers

      | MIPS ->
          (* TODO: set endianness? *)
          isa.fixed_pseudo_registers
      | RISCV ->
          isa.fixed_pseudo_registers
      | X86 ->
          isa.fixed_pseudo_registers
    in

    let isa' = { isa with fixed_pseudo_registers = fixed_pseudo_registers' } in
    isa'
  in

  let open Params in
  { isr_params            = model;
    isr_isa               = isa';
    isr_program           = prog;
    isr_return_addr       = return_addresses;
    isr_thread_ids        = tids;
    isr_register_data     = init_reg_data;
    isr_register_values   = init_reg_value;
    isr_first_instruction = initial_fetch_address;
    isr_memory            = initial_writes;
  }








let do_graph_init test info () = 
  if !Globals.graph_backend = Globals.Tikz then
    begin match !Globals.run_dot with
    | Some Globals.RD_final
    | Some Globals.RD_final_ok
    | Some Globals.RD_final_not_ok
        -> Tikz.make_init_state info test
    | None
    | Some Globals.RD_step
        -> ()
    end

let read_channel 
      (runOptions: RunOptions.t)
      (name: string)
      (in_chan: lex_input)
      (isa_callback: (Isa.isa_model -> unit) option) 
    : (module Test_file.ConcModel_Info_Init) =
  (* First split the input file in sections *)
  let module SPL = Splitter.Make(Splitter.Default) in
  let test_splitted =
    begin match in_chan with
    | LexInChannel c -> SPL.split name c
    | LexInString s  -> SPL.split_string name s
    end
  in

  (* extract the architecture from the litmus file *)
  let isa_model = isa_model_of_arch test_splitted.Splitter.arch in
  Globals.set_isa_model isa_model;

  let open Params in
  let open InstructionSemantics in
  let open Isa in

  begin match isa_callback with
  | Some f -> f isa_model
  | _ -> ()
  end;

  let make_info test = test_info test test_splitted.Splitter.name.Name.name in

  (* parse and translate the litmus test *)
  let (do_graph_init_function, (module CII : Test_file.ConcModel_Info_Init)) =
    let open Params in
    let open Isa_model in
    let module IsShallowEmbedding = 
      (struct let b = not runOptions.RunOptions.interpreter end)
    in
    (* let open Globals in *)
    let params = !Globals.model_params in

    begin match (isa_model, params.ss.ss_model, params.t.thread_model) with
    | (AARCH64 Hand, POP_storage_model, POP_thread_model _)
    | (AARCH64 Hand, Flowing_storage_model, POP_thread_model _)
    | (AARCH64 Hand, Flat_storage_model, POP_thread_model _)
    | (AARCH64 Hand, Flat_storage_model, Relaxed_thread_model) -> 
        let module Parser = Make_litmus_parser(AArch64HGen)(AArch64HGenTransSail)(AArch64HGenLexParse) in
        let module ISA = Make (IsShallowEmbedding) (AARCH64_HGEN_ISA) (AArch64ISADefs) in
        let test = Parser.parse in_chan test_splitted in
        let info = make_info test in
        let module SS = (val (Machine_concurrency_model.get_SS_model params.ss.ss_model)) in
        (do_graph_init test info, (module struct
           type instruction_ast = ISA.instruction_ast
           let initial_state_record_maker = initial_state_record test ISA.isa
           let info = info
           module ConcModel = Machine_concurrency_model.Make(ISA)(SS)
         end))

    | (AARCH64 Hand, Promising_storage_model, Promising_thread_model) ->
       let module Parser = Make_litmus_parser(AArch64HGen)(AArch64HGenTransSail)(AArch64HGenLexParse) in
       let module ISA = Make (IsShallowEmbedding) (AARCH64_HGEN_ISA) (AArch64ISADefs) in
       let test = Parser.parse in_chan test_splitted in
       let info = make_info test in
       (do_graph_init test info, (module struct
          type instruction_ast = ISA.instruction_ast
          let initial_state_record_maker = initial_state_record test ISA.isa
          let info = make_info test
          module ConcModel = Promising_concurrency_model.Make(ISA)
        end))

    | (AARCH64 Gen, POP_storage_model, POP_thread_model _)
    | (AARCH64 Gen, Flowing_storage_model, POP_thread_model _)
    | (AARCH64 Gen, Flat_storage_model, POP_thread_model _)
    | (AARCH64 Gen, Flat_storage_model, Relaxed_thread_model) ->
       let module Parser = Make_litmus_parser(AArch64HGen)(AArch64GenTransSail)(AArch64HGenLexParse) in
       let module ISA = Make (IsShallowEmbedding) (AARCH64_GEN_ISA) (AArch64GenISADefs) in
       let test = Parser.parse in_chan test_splitted in
       let info = make_info test in
       let module SS = (val (Machine_concurrency_model.get_SS_model params.ss.ss_model)) in
       (do_graph_init test info, (module struct
          type instruction_ast = ISA.instruction_ast
          let initial_state_record_maker = initial_state_record test ISA.isa
          let info = make_info test
          module ConcModel = Machine_concurrency_model.Make(ISA)(SS)
        end))

    | (AARCH64 Gen, Promising_storage_model, Promising_thread_model) ->

       let module Parser = Make_litmus_parser(AArch64HGen)(AArch64GenTransSail)(AArch64HGenLexParse) in
       let module ISA = Make (IsShallowEmbedding) (AARCH64_GEN_ISA) (AArch64GenISADefs) in
       let test = Parser.parse in_chan test_splitted in
        let info = make_info test in
       (do_graph_init test info, (module struct
          type instruction_ast = ISA.instruction_ast
          let initial_state_record_maker = initial_state_record test ISA.isa
          let info = make_info test
          module ConcModel = Promising_concurrency_model.Make(ISA)
        end))

    | (PPC, POP_storage_model, POP_thread_model _)
    | (PPC, Flat_storage_model, POP_thread_model _)
    | (PPC, PLDI11_storage_model, PLDI11_thread_model)
    | (PPC, Flat_storage_model, Relaxed_thread_model) ->
        let module Parser = Make_litmus_parser(PPC)(PPCGenTransSail)(PPCLexParse) in
        let module ISA = Make (IsShallowEmbedding) (PPCGEN_ISA) (PPCGenISADefs) in
        let test = Parser.parse in_chan test_splitted in
        let info = make_info test in
        let module SS = (val (Machine_concurrency_model.get_SS_model params.ss.ss_model)) in
        (do_graph_init test info, (module struct
          type instruction_ast = ISA.instruction_ast
          let initial_state_record_maker = initial_state_record test ISA.isa
          let info = make_info test
          module ConcModel = Machine_concurrency_model.Make(ISA)(SS)
        end))

    | (MIPS, POP_storage_model, POP_thread_model _)
    | (MIPS, Flowing_storage_model, POP_thread_model _)
    | (MIPS, Flat_storage_model, POP_thread_model _)
    | (MIPS, Flat_storage_model, Relaxed_thread_model) ->
        let module Parser = Make_litmus_parser(MIPSHGen)(MIPSHGenTransSail)(MIPSHGenLexParse) in
        let module ISA = Make(IsShallowEmbedding)(MIPS_ISA)(MIPS64ISADefs) in
        let test = Parser.parse in_chan test_splitted in
        let info = make_info test in
        let module SS = (val (Machine_concurrency_model.get_SS_model params.ss.ss_model)) in
        (do_graph_init test info, (module struct
           type instruction_ast = ISA.instruction_ast
           let initial_state_record_maker = initial_state_record test ISA.isa
           let info = make_info test
           module ConcModel = Machine_concurrency_model.Make(ISA)(SS)
         end))
    | (MIPS, PLDI11_storage_model, PLDI11_thread_model) ->
        Printf.eprintf "The pldi11 model does not support the MIPS architecture\n";
        exit 1

    | (RISCV, POP_storage_model, POP_thread_model _)
    | (RISCV, Flowing_storage_model, POP_thread_model _)
    | (RISCV, Flat_storage_model, POP_thread_model _)
    | (RISCV, TSO_storage_model, TSO_thread_model)
    | (RISCV, Flat_storage_model, Relaxed_thread_model) ->
       let module Parser = Make_litmus_parser(RISCVHGen)(RISCVHGenTransSail)(RISCVHGenLexParse) in
       let module ISA = Make(IsShallowEmbedding)(RISCV_ISA)(RISCVISADefs) in
       let test = Parser.parse in_chan test_splitted in
        let info = make_info test in
        let module SS = (val (Machine_concurrency_model.get_SS_model params.ss.ss_model)) in
        (do_graph_init test info, (module struct
          type instruction_ast = ISA.instruction_ast
          let initial_state_record_maker = initial_state_record test ISA.isa
          let info = make_info test
          module ConcModel = Machine_concurrency_model.Make(ISA)(SS)
        end))

    | (RISCV, Promising_storage_model, Promising_thread_model) ->
       let module Parser = Make_litmus_parser(RISCVHGen)(RISCVHGenTransSail)(RISCVHGenLexParse) in
       let module ISA = Make(IsShallowEmbedding)(RISCV_ISA)(RISCVISADefs) in
       let test = Parser.parse in_chan test_splitted in
        let info = make_info test in
       (do_graph_init test info, (module struct
          type instruction_ast = ISA.instruction_ast
          let initial_state_record_maker = initial_state_record test ISA.isa
          let info = make_info test
          module ConcModel = Promising_concurrency_model.Make(ISA)
        end))

    | (X86, TSO_storage_model, TSO_thread_model)
    | (X86, Flat_storage_model, Relaxed_thread_model) ->
        begin match List.assoc "Syntax" test_splitted.Splitter.info with
        | "gas" ->
            Globals.x86syntax := Some X86_gas;
            let module Parser = Make_litmus_parser(X86HGen)(X86HGenTransSail)(X86HGenLexParseGas) in
            let module ISA = Make(IsShallowEmbedding)(X86_ISA)(X86ISADefs) in
            let test = Parser.parse in_chan test_splitted in
            let info = make_info test in
            let module SS = (val (Machine_concurrency_model.get_SS_model params.ss.ss_model)) in
            (do_graph_init test info, (module struct
               type instruction_ast = ISA.instruction_ast
               let initial_state_record_maker = initial_state_record test ISA.isa
               let info = make_info test
               module ConcModel = Machine_concurrency_model.Make(ISA)(SS)
             end))
        | "intel" ->
            Globals.x86syntax := Some X86_intel;
            let module Parser = Make_litmus_parser(X86HGen)(X86HGenTransSail)(X86HGenLexParseIntel) in
            let module ISA = Make(IsShallowEmbedding)(X86_ISA)(X86ISADefs) in
            let test = Parser.parse in_chan test_splitted in
            let info = make_info test in
            let module SS = (val (Machine_concurrency_model.get_SS_model params.ss.ss_model)) in
            (do_graph_init test info, (module struct
               type instruction_ast = ISA.instruction_ast
               let initial_state_record_maker = initial_state_record test ISA.isa
               let info = make_info test
               module ConcModel = Machine_concurrency_model.Make(ISA)(SS)
             end))
        | _ ->
            Printf.eprintf "Unknown x86 Syntax (expected 'gas' or 'intel')\n";
            exit 1
        | exception Not_found -> (* intel by default *)
            Globals.x86syntax := Some X86_intel;
            let module Parser = Make_litmus_parser(X86HGen)(X86HGenTransSail)(X86HGenLexParseIntel) in
            let module ISA = Make(IsShallowEmbedding)(X86_ISA)(X86ISADefs) in
            let test = Parser.parse in_chan test_splitted in
            let info = make_info test in
            let module SS = (val (Machine_concurrency_model.get_SS_model params.ss.ss_model)) in
            (do_graph_init test info, (module struct
               type instruction_ast = ISA.instruction_ast
               let initial_state_record_maker = initial_state_record test ISA.isa
               let info = make_info test
               module ConcModel = Machine_concurrency_model.Make(ISA)(SS)
             end))
        end
    | _ ->
        Printf.eprintf "Unsupported model and architecture configuration\n";
        exit 1
    end
  in

  (* let info = test_info TestAndConcModel.test test_splitted.Splitter.name.Name.name in *)

  if !Globals.branch_targets = None then begin
    match List.assoc "Branch-targets" CII.info.Test.info with
    | branch_targets ->
        begin try Globals.branch_targets := Some (Model_aux.branch_targets_parse_from_string branch_targets) with
        | Model_aux.BranchTargetsParsingError msg ->
            Printf.eprintf "%s\n" msg;
            exit 1
        end
    | exception Not_found -> ()
  end;

  if !Globals.shared_memory = None then begin
    match List.assoc "Shared-memory" CII.info.Test.info with
    | shared_memory ->
        begin try Globals.shared_memory := Some (Model_aux.shared_memory_parse_from_string shared_memory) with
        | Model_aux.SharedMemoryParsingError msg ->
            Printf.eprintf "%s" msg;
            exit 1
        end
    | exception Not_found -> ()
  end;

  Globals.add_bt_and_sm_to_model_params CII.info.symbol_table;

  (* HACK: *)

  do_graph_init_function ();

  (module CII)


let read_data
      (runOptions : RunOptions.t)
      (name: string)
      (data: data)
      (isa_callback: (Isa.isa_model -> unit) option)
    : (module Test_file.ConcModel_Info_Init) =
  read_channel runOptions name (LexInString data) isa_callback

let read_file
      (runOptions : RunOptions.t)
      (name: string)
      (isa_callback: (Isa.isa_model -> unit) option)
    : (module Test_file.ConcModel_Info_Init) =
  Misc.input_protect begin
      fun (in_chan: in_channel) ->
        read_channel runOptions name (LexInChannel in_chan) isa_callback
  end name



(********************************************************************)

(* module Make_litmus_parser_to_xml
 *     (Arch: Arch.S)
 *     (LexParse: GenParser.LexParse with type instruction = Arch.parsedPseudo)
 *     =
 * struct
 *   module Parser = GenParser.Make(GenParser.DefaultConfig)(Arch)(LexParse)
 *   module Translator = Translate_to_xml.Make(Arch)
 * 
 *   let parse (in_chan: lex_input) (test_splitted: Splitter.result) =
 *     (\* parse splitted test *\)
 *     begin match in_chan with
 *     | LexInChannel c -> Parser.parse c test_splitted
 *     | LexInString s  -> Parser.parse_string s test_splitted
 *     end
 *     |> Translator.translate_test test_splitted
 * end
 * 
 * 
 * 
 * let channel_to_xml (name: string) (in_chan: lex_input) =
 *   (\* First split the input file in sections *\)
 *   let module SPL = Splitter.Make(Splitter.Default) in
 *   let test_splitted =
 *     begin match in_chan with
 *     | LexInChannel c -> SPL.split name c
 *     | LexInString s  -> SPL.split_string name s
 *     end
 *   in
 * 
 *   begin match test_splitted.Splitter.arch with
 *   | `PPC           ->
 *       let module Parser = Make_litmus_parser_to_xml(PPC)(PPCLexParse) in
 *       Parser.parse in_chan test_splitted
 *   | `AArch64       ->
 *       let module Parser = Make_litmus_parser_to_xml(AArch64HGen)(AArch64HGenLexParse) in
 *       Parser.parse in_chan test_splitted
 *   | `MIPS          ->
 *       let module Parser = Make_litmus_parser_to_xml(MIPSHGen)(MIPSHGenLexParse) in
 *       Parser.parse in_chan test_splitted
 *   | `RISCV         ->
 *       let module Parser = Make_litmus_parser_to_xml(RISCVHGen)(RISCVHGenLexParse) in
 *       Parser.parse in_chan test_splitted
 *   | `X86           ->
 *         let syntax = begin try List.assoc "Syntax" test_splitted.Splitter.info with
 *                      | Not_found -> "intel" end in
 *         if (syntax = "gas") then
 *           let module Parser = Make_litmus_parser_to_xml(X86HGen)(X86HGenLexParseGas) in
 *           Parser.parse in_chan test_splitted
 *         else 
 *           let module Parser = Make_litmus_parser_to_xml(X86HGen)(X86HGenLexParseIntel) in
 *           Parser.parse in_chan test_splitted
 *   | _ -> Warn.fatal "unknown architecture"
 *   end
 * 
 * let to_xml (file: string) =
 *   Misc.input_protect
 *     (fun (c: in_channel) -> channel_to_xml file (LexInChannel c))
 *     file *)


