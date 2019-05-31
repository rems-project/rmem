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


(* This module is used not only by rmem. Therefore it should make no
   reference to globals.ml for runtime options.  litmus_test_file is a
   wrapper around litmus_test_file_base used only by rmem, which
   refers to globals.ml for some of the arguments. *)

open Test

(* given the thread index, tid, and the instruction offset in the
thread, n, return the memory address of the instruction *)
let aval_of_inst_index tid n = Nat_big_num.of_int (0x50000 + 0x1000 * tid + n)

(* duplication of the type in Globals, so this module can remain
   independent of globals.ml*)
type x86_syntax =
  | X86_syntax_gas
  | X86_syntax_intel


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



module Make_litmus_parser
    (Arch: Arch_litmus.S with type V.Scalar.t = string)
    (TransSail: Trans.TransSail with type instruction = Arch.instruction)
    (LexParse: GenParser.LexParse with type instruction = Arch.parsedPseudo)
    (GenParserConfig : GenParser.Config)
    =
struct
  module Parser = GenParser.Make(GenParserConfig)(Arch)(LexParse)
  module Translator = Translate.Make(Arch)(TransSail)

  let parse (in_chan: lex_input) (test_splitted: Splitter.result) : Test.test =
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

type test = Test.test
type data = string

let test_info (aarch64gen: bool) (test: Test.test) (name: string) : Test.info =
  let ism =
    let open InstructionSemantics in
    begin match test.arch with
    | `PPC                         -> PPCGEN_ism
    | `AArch64 when aarch64gen     -> AARCH64_ism AArch64GenSail
    | `AArch64 when not aarch64gen -> AARCH64_ism AArch64HandSail
    | `MIPS                        -> MIPS_ism
    | `RISCV                       -> RISCV_ism
    | `X86                         -> X86_ism
    | _ ->
        Printf.eprintf "Unsupported architecture\n";
        exit 1
    end
  in

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
              let addr = Sail_impl_base.address_of_integer (aval_of_inst_index tid n) in
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
    Test.ism            = ism;
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

let read_channel 
      (name: string)
      (in_chan: lex_input)
      (aarch64gen: bool)
      (overwrite_check_cond : string -> string option)
    : (Test.info * test) * BasicTypes.isa_info * x86_syntax option =
  (* First split the input file in sections *)
  let module SPL = Splitter.Make(Splitter.Default) in
  let test_splitted =
    begin match in_chan with
    | LexInChannel c -> SPL.split name c
    | LexInString s  -> SPL.split_string name s
    end
  in

  (* extract the architecture from the litmus file *)
  let isa_info =  
    begin match test_splitted.Splitter.arch with
    | `PPC                         -> IsaInfoPPCGen.ppcgen_ism
    | `AArch64 when aarch64gen     -> IsaInfoAArch64.aarch64gen_ism
    | `AArch64 when not aarch64gen -> IsaInfoAArch64.aarch64hand_ism
    | `MIPS                        -> IsaInfoMIPS.mips_ism
    | `RISCV                       -> IsaInfoRISCV.riscv_ism
    | `X86                         -> IsaInfoX86.x86_ism
    | _ -> Warn.fatal "Can only do %s, %s, %s, %s and %s" 
             (Archs.pp `PPC) (Archs.pp `AArch64) (Archs.pp `MIPS)
             (Archs.pp `RISCV) (Archs.pp `X86)
    end
  in

  let open Params in
  let open InstructionSemantics in
  let open BasicTypes in

  let module GenParserConfig : GenParser.Config = struct
    let debuglexer = false
    let check_kind _ = None
    let check_cond = overwrite_check_cond
    let verbose = 0
  end in


  (* parse and translate the litmus test *)
  let (test, maybe_x86_syntax) =
    begin match isa_info.ism with
    | AARCH64_ism AArch64HandSail ->
       let module Parser =
         Make_litmus_parser
           (AArch64HGen)(AArch64HGenTransSail)
           (AArch64HGenLexParse)(GenParserConfig) in
        (Parser.parse in_chan test_splitted, None)
    | AARCH64_ism AArch64GenSail ->
       let module Parser =
         Make_litmus_parser
           (AArch64HGen)(AArch64GenTransSail)
           (AArch64HGenLexParse)(GenParserConfig) in
        (Parser.parse in_chan test_splitted, None)
    | PPCGEN_ism ->
       let module Parser =
         Make_litmus_parser
           (PPC)(PPCGenTransSail)
           (PPCLexParse)(GenParserConfig) in
        (Parser.parse in_chan test_splitted, None)
    | MIPS_ism ->
       let module Parser =
         Make_litmus_parser
           (MIPSHGen)(MIPSHGenTransSail)
           (MIPSHGenLexParse)(GenParserConfig) in
        (Parser.parse in_chan test_splitted, None)
    | RISCV_ism ->
       let module Parser =
         Make_litmus_parser
           (RISCVHGen)(RISCVHGenTransSail)
           (RISCVHGenLexParse)(GenParserConfig) in
        (Parser.parse in_chan test_splitted, None)
    | X86_ism ->
        begin match List.assoc "Syntax" test_splitted.Splitter.info with
        | "gas" ->
           let module Parser =
             Make_litmus_parser
               (X86HGen)(X86HGenTransSail)
               (X86HGenLexParseGas)(GenParserConfig) in
            (Parser.parse in_chan test_splitted, Some X86_syntax_gas)
        | "intel" ->
           let module Parser =
             Make_litmus_parser(X86HGen)(X86HGenTransSail)
               (X86HGenLexParseIntel)(GenParserConfig) in
            (Parser.parse in_chan test_splitted, Some X86_syntax_intel)
        | _ ->
            Printf.eprintf "Unknown x86 Syntax (expected 'gas' or 'intel')\n";
            exit 1
        | exception Not_found -> (* intel by default *)
           let module Parser =
             Make_litmus_parser
               (X86HGen)(X86HGenTransSail)
               (X86HGenLexParseIntel)(GenParserConfig) in
            (Parser.parse in_chan test_splitted, Some X86_syntax_intel)
        end
    end
  in

  let info = test_info aarch64gen test test_splitted.Splitter.name.Name.name in

  ((info, test), isa_info, maybe_x86_syntax)


let initial_state_record_base
    (endianness: Sail_impl_base.end_flag)
    (aarch64gen: bool)
    (test:      test)
    (thread_isa_info:     BasicTypes.isa_info)
  =

  let open Params in
  let open BasicTypes in
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
          endianness
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
                List.mapi (fun i inst -> (aval_of_inst_index tid (i * 4), inst)) instructions)
            test.prog)
    in

    (* map-from-list: *)
    List.fold_left
      (fun k (a, i) -> Pmap.add a i k)
      (Pmap.empty Nat_big_num.compare)
      prog_in_mem
  in

  let prog _ (address: Sail_impl_base.address) : InstructionSemantics.fetch_and_decode_outcome =
    let address' = Sail_impl_base.integer_of_address address in
    begin match Pmap.lookup address' prog_map with
    | None -> InstructionSemantics.FDO_illegal_fetch_address
    | Some inst ->
       InstructionSemantics.FDO_success (address, None, inst)
    end
  in

  (* provide the initial fetch address for each thread *)
  let initial_fetch_address tid : Sail_impl_base.address option =
    Some (Sail_impl_base.address_of_integer (aval_of_inst_index tid 0))
  in

  let return_addresses =
    List.map
      (fun (tid, instructions, _) ->
          let aval = aval_of_inst_index tid (((List.length instructions) - 1) * 4) in
          (tid, Sail_impl_base.address_of_integer aval))
      test.prog
  in

  (* Set up TPIDR registers -- used for thread local storage, specifically
     seems to be a pointer to the TCB. For now, just put the thread id there *)
  let reg_values : ((Nat_num.nat * Sail_impl_base.reg_base_name) * Sail_impl_base.register_value) list =
    let open BasicTypes in
    match test.arch with
    | `AArch64 ->
       let tpidr_el0 =
         let registerdata =
           if aarch64gen then
             IsaInfoAArch64.aarch64gen_ism.register_data_info
           else
             IsaInfoAArch64.aarch64hand_ism.register_data_info
         in
         match reg_from_data registerdata "TPIDR_EL0" with
         | Some r -> r
         | None -> failwith ("\"TPIDR_EL0\" is not in 'register_data_info'")
       in
       (List.map
          (fun tid ->
            ((tid, "TPIDR_EL0"), Sail_impl_base.register_value_for_reg_of_integer tpidr_el0 (Nat_big_num.of_int tid)))
          tids)
       @ test.init_reg_state
    | `RISCV ->
       let registerdata = IsaInfoRISCV.riscv_ism.register_data_info in
       let get_reg r = match reg_from_data registerdata r with
         | Some r -> r
         | None -> failwith (r ^ " not in register_data_info")
       in
       let each_thread tid =
         [
           ((tid, "cur_privilege"), Sail_impl_base.register_value_for_reg_of_integer (get_reg "cur_privilege") (Nat_big_num.of_int 2));
           ((tid, "misa"), Sail_impl_base.register_value_for_reg_of_integer (get_reg "misa") (Nat_big_num.of_string "0x8000000000000881")); (* RV64IMA *)
           ((tid, "mstatus"), Sail_impl_base.register_value_for_reg_of_integer (get_reg "mstatus") (Nat_big_num.of_string "0x0000000a00000000")); (* RV64 in S and U also *)
         ]
       in
       (List.flatten (List.map each_thread tids)) @ test.init_reg_state

    | _ ->
       test.init_reg_state
  in

  (* initial values for registers based on test (default is 0) *)
  let init_reg_value t r =
    begin try List.assoc (t, r) reg_values with
    | Not_found ->
        (* currently all registers that are not explicitly initialised
        in test are set to zero *)
        RegUtils.register_state_zero thread_isa_info.register_data_info t r
    end
  in

  let reg_values' = List.map (fun ((tid,rbn),v) -> (tid,(rbn,v))) reg_values in
  let init_reg_data t =
    let init_reg_data' = List.filter (fun (tid,_) -> t = tid) reg_values' in
    List.map snd init_reg_data' in


  let thread_isa_info' =
    let fixed_pseudo_registers' =
      let open Sail_impl_base in
      match thread_isa_info.ism with
      | PPCGEN_ism ->
          let endianness =
            match endianness with
            | E_little_endian -> register_value_zeros D_increasing 1 0
            | E_big_endian    -> register_value_ones  D_increasing 1 0
          in
          (Reg_slice ("bigendianmode", 0, D_increasing, (0,0)), endianness) ::
          thread_isa_info.fixed_pseudo_registers

      | AARCH64_ism _ ->
          let endianness =
            match endianness with
            | E_little_endian -> register_value_zeros D_decreasing 1 0
            | E_big_endian    -> register_value_ones  D_decreasing 1 0
          in
          (Reg_field ("SCTLR_EL1", 31, D_decreasing, "E0E", (24,24)), endianness) ::
          (Reg_field ("SCTLR_EL1", 31, D_decreasing, "EE",  (25,25)), endianness) ::
          thread_isa_info.fixed_pseudo_registers

      | MIPS_ism ->
          (* TODO: set endianness? *)
          thread_isa_info.fixed_pseudo_registers
      | RISCV_ism ->
          thread_isa_info.fixed_pseudo_registers
      | X86_ism ->
          thread_isa_info.fixed_pseudo_registers
    in

    {thread_isa_info with fixed_pseudo_registers = fixed_pseudo_registers'}
  in

  (thread_isa_info',
   prog_map,
   prog,
   return_addresses,
   tids,
   init_reg_data,
   init_reg_value,
   initial_fetch_address,
   init_write_events)
  

