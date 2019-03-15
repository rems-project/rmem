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

(* given the thread index, tid, and the instruction offset in the
thread, n, return the memory address of the instruction *)
let aval_of_inst_index tid n = Nat_big_num.of_int (0x50000 + 0x1000 * tid + n)


(* Arch.Config *)
module Arch_config =
struct
  let memory = Memory.Direct
  let cautious = true
  let asmcomment = None
end

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
end

module Make_litmus_parser
    (Arch: Arch.S)
    (TransSail: Isa_model.TransSail with type instruction = Arch.instruction)
    (LexParse: GenParser.LexParse with type instruction = Arch.parsedPseudo)
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

let test_info (test: Test.test) (name: string) : Test.info =
  let ism =
    let open MachineDefInstructionSemantics in
    begin match test.arch with
    | `PPC           -> PPCGEN_ism
    | `AArch64 when !Globals.aarch64gen ->
                        AARCH64_ism AArch64GenSail
    | `AArch64 when not !Globals.aarch64gen ->
                        AARCH64_ism AArch64HandSail
    | `MIPS          -> MIPS_ism
    | `RISCV         -> RISCV_ism
    | `X86           -> X86_ism
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
      (isa_callback: (MachineDefInstructionSemantics.instruction_semantics_mode -> unit) option) 
    : Test.info * test =
  (* First split the input file in sections *)
  let module SPL = Splitter.Make(Splitter.Default) in
  let test_splitted =
    begin match in_chan with
    | LexInChannel c -> SPL.split name c
    | LexInString s  -> SPL.split_string name s
    end
  in

  (* extract the architecture from the litmus file *)
  begin match test_splitted.Splitter.arch with
  | `PPC           -> MachineDefISAInfoPPCGen.ppcgen_ism
  | `AArch64 when !Globals.aarch64gen ->
                      MachineDefISAInfoAArch64.aarch64gen_ism
  | `AArch64 when not !Globals.aarch64gen ->
                      MachineDefISAInfoAArch64.aarch64hand_ism
  | `MIPS          -> MachineDefISAInfoMIPS.mips_ism
  | `RISCV         -> MachineDefISAInfoRISCV.riscv_ism
  | `X86           -> MachineDefISAInfoX86.x86_ism
  | _ -> Warn.fatal "Can only do %s, %s, %s, %s and %s" (Archs.pp `PPC) (Archs.pp `AArch64) (Archs.pp `MIPS) (Archs.pp `RISCV) (Archs.pp `X86)
  end
  |> Globals.set_model_ism;

  let open MachineDefParams in
  let open MachineDefInstructionSemantics in
  let open MachineDefISAInfo in

  begin match isa_callback with
  | Some f -> f (!Globals.model_params).t.thread_isa_info.ism
  | _ -> ()
  end;

  (* parse and translate the litmus test *)
  let test =
    let open MachineDefParams in
    let open Globals in
    let params = !Globals.model_params in
    begin match (params.t.thread_isa_info.ism, params.ss.ss_model, params.t.thread_model) with
    | (AARCH64_ism AArch64HandSail, POP_storage_model, POP_thread_model _)
    | (AARCH64_ism AArch64HandSail, Flowing_storage_model, POP_thread_model _)
    | (AARCH64_ism AArch64HandSail, Flat_storage_model, POP_thread_model _)
    | (AARCH64_ism AArch64HandSail, NOP_storage_model, POP_thread_model _)
    | (AARCH64_ism AArch64HandSail, Promising_storage_model, Promising_thread_model)
    | (AARCH64_ism AArch64HandSail, Flat_storage_model, Relaxed_thread_model) ->

        (* the following code uses diy/litmus parser to generate the hash for the test:
        let module Make_hasher
            (Arch: Arch.S)
            (LexParse: GenParser.LexParse with type instruction = Arch.parsedPseudo)
          =
          struct
            module Parser = GenParser.Make(GenParser.DefaultConfig)(Arch)(LexParse)

            let get_hash (in_chan: lex_input) (test_splitted: Splitter.result) : string option =
              (* parse splitted test *)
              let parsedt =
                begin match in_chan with
                | LexInChannel c -> Parser.parse c test_splitted
                | LexInString s  -> Parser.parse_string s test_splitted
                end
              in
              MiscParser.get_hash parsedt

          end
        in

        let module AArch64 = AArch64Arch.Make(Arch_config)(SymbConstant) in
        let module AArch64LexParse =
          struct
            type instruction = AArch64.parsedPseudo
            type token = AArch64Parser.token

            module LexConfig = struct let debug = false end
            module Lexer = AArch64Lexer.Make(LexConfig)
            let lexer = Lexer.token
            let parser = MiscParser.mach2generic AArch64Parser.main
          end
        in

        let module Litmus_hasher = Make_hasher(AArch64)(AArch64LexParse) in

        begin match Litmus_hasher.get_hash in_chan test_splitted with
        | Some s -> Printf.printf "HASH IS: %s\n" s
        end;
        *)

        let module Parser = Make_litmus_parser(AArch64HGen)(AArch64HGenTransSail)(AArch64HGenLexParse) in
        Parser.parse in_chan test_splitted

    | (AARCH64_ism AArch64GenSail, POP_storage_model, POP_thread_model _)
    | (AARCH64_ism AArch64GenSail, Flowing_storage_model, POP_thread_model _)
    | (AARCH64_ism AArch64GenSail, Flat_storage_model, POP_thread_model _)
    | (AARCH64_ism AArch64GenSail, NOP_storage_model, POP_thread_model _)
    | (AARCH64_ism AArch64GenSail, Promising_storage_model, Promising_thread_model)
    | (AARCH64_ism AArch64GenSail, Flat_storage_model, Relaxed_thread_model) ->
        let module Parser = Make_litmus_parser(AArch64HGen)(AArch64GenTransSail)(AArch64HGenLexParse) in
        Parser.parse in_chan test_splitted

    | (AARCH64_ism _, PLDI11_storage_model, PLDI11_thread_model) ->
        Printf.eprintf "The pldi11 model does not support the AArch64 architecture\n";
        exit 1

    | (PPCGEN_ism, POP_storage_model, POP_thread_model _)
    | (PPCGEN_ism, Flat_storage_model, POP_thread_model _)
    | (PPCGEN_ism, PLDI11_storage_model, PLDI11_thread_model)
    | (PPCGEN_ism, Flat_storage_model, Relaxed_thread_model) ->
        let module Parser = Make_litmus_parser(PPC)(PPCGenTransSail)(PPCLexParse) in
        Parser.parse in_chan test_splitted

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
        let module Parser = Make_litmus_parser(MIPSHGen)(MIPSHGenTransSail)(MIPSHGenLexParse) in
        Parser.parse in_chan test_splitted
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
       (* failwith "no litmus parser for riscv yet" *)
        let module Parser = Make_litmus_parser(RISCVHGen)(RISCVHGenTransSail)(RISCVHGenLexParse) in
        Parser.parse in_chan test_splitted

    | (X86_ism, TSO_storage_model, TSO_thread_model)
    | (X86_ism, Flat_storage_model, Relaxed_thread_model) ->
        begin match List.assoc "Syntax" test_splitted.Splitter.info with
        | "gas" ->
            Globals.x86syntax := Some X86_gas;
            let module Parser = Make_litmus_parser(X86HGen)(X86HGenTransSail)(X86HGenLexParseGas) in
            Parser.parse in_chan test_splitted
        | "intel" ->
            Globals.x86syntax := Some X86_intel;
            let module Parser = Make_litmus_parser(X86HGen)(X86HGenTransSail)(X86HGenLexParseIntel) in
            Parser.parse in_chan test_splitted
        | _ ->
            Printf.eprintf "Unknown x86 Syntax (expected 'gas' or 'intel')\n";
            exit 1
        | exception Not_found -> (* intel by default *)
            Globals.x86syntax := Some X86_intel;
            let module Parser = Make_litmus_parser(X86HGen)(X86HGenTransSail)(X86HGenLexParseIntel) in
            Parser.parse in_chan test_splitted
        end
    | _ ->
        Printf.eprintf "Unsupported model and architecture configuration\n";
        exit 1
    end
  in

  let info = test_info test test_splitted.Splitter.name.Name.name in

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

  (* if !Globals.graph_backend = Globals.Tikz then
   *   begin match !Globals.run_dot with
   *   | Some Globals.RD_final
   *   | Some Globals.RD_final_ok
   *   | Some Globals.RD_final_not_ok
   *     -> Tikz.make_init_state info test
   *   | None
   *   | Some Globals.RD_step
   *     -> ()
   *   end; *)

  (info, test)


let read_data (name: string) (data: data) (isa_callback: (MachineDefInstructionSemantics.instruction_semantics_mode -> unit) option) : Test.info * test =
  read_channel name (LexInString data) isa_callback

let read_file (name: string) (isa_callback: (MachineDefInstructionSemantics.instruction_semantics_mode -> unit) option) : Test.info * test =
  Misc.input_protect begin
      fun (in_chan: in_channel) ->
        read_channel name (LexInChannel in_chan) isa_callback
  end name


let initial_state_record
    (test:      test)
    (isa_defs:  (module Isa_model.ISADefs))
    (model:     MachineDefParams.model_params)
    (*: 'ss MachineDefTypes.system_state*)
  =

  let open MachineDefParams in
  let open MachineDefISAInfo in
  let open MachineDefInstructionSemantics in

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
          let (new_ioid, ist) = MachineDefFreshIds.gen_fresh_id ist in
          let ioid_ist = MachineDefFreshIds.initial_id_state new_ioid in
          let (new_writes, ioid_ist) =
            MachineDefEvents.make_write_events_big_split
              ioid_ist init_thread new_ioid footprint value Sail_impl_base.Write_plain
          in
          (new_writes @ writes, ist))
        ([], MachineDefFreshIds.initial_id_state init_thread)
        init_mem_values
    in
    let write_events = List.sort (fun w1 w2 -> Pervasives.compare w1.MachineDefEvents.w_addr w2.MachineDefEvents.w_addr) write_events in

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

  let prog _ (address: Sail_impl_base.address) : MachineDefInstructionSemantics.fetch_and_decode_outcome =
    let address' = Sail_impl_base.integer_of_address address in
    begin match Pmap.lookup address' prog_map with
    | None -> MachineDefInstructionSemantics.FDO_illegal_fetch_address
    | Some inst ->
       MachineDefInstructionSemantics.FDO_success (address, None, inst)
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

  let module ISADefs = (val isa_defs) in

  (* Set up TPIDR registers -- used for thread local storage, specifically
     seems to be a pointer to the TCB. For now, just put the thread id there *)
  let reg_values : ((Nat_num.nat * Sail_impl_base.reg_base_name) * Sail_impl_base.register_value) list =
    let open MachineDefISAInfo in
    match test.arch with
    | `AArch64 ->
       let tpidr_el0 =
         let registerdata =
           if !Globals.aarch64gen then
             MachineDefISAInfoAArch64.aarch64gen_ism.register_data_info
           else
             MachineDefISAInfoAArch64.aarch64hand_ism.register_data_info
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
       let registerdata = MachineDefISAInfoRISCV.riscv_ism.register_data_info in
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
        MachineDefThreadSubsystemUtils.register_state_zero ISADefs.reg_data t r
    end
  in

  let reg_values' = List.map (fun ((tid,rbn),v) -> (tid,(rbn,v))) reg_values in
  let init_reg_data t =
    let init_reg_data' = List.filter (fun (tid,_) -> t = tid) reg_values' in
    List.map snd init_reg_data' in


  let model' =
    let fixed_pseudo_registers' =
      let open Sail_impl_base in
      match model.t.thread_isa_info.ism with
      | PPCGEN_ism ->
          let endianness =
            match Globals.get_endianness () with
            | E_little_endian -> register_value_zeros D_increasing 1 0
            | E_big_endian    -> register_value_ones  D_increasing 1 0
          in
          (Reg_slice ("bigendianmode", 0, D_increasing, (0,0)), endianness) ::
          model.t.thread_isa_info.fixed_pseudo_registers

      | AARCH64_ism _ ->
          let endianness =
            match Globals.get_endianness () with
            | E_little_endian -> register_value_zeros D_decreasing 1 0
            | E_big_endian    -> register_value_ones  D_decreasing 1 0
          in
          (Reg_field ("SCTLR_EL1", 31, D_decreasing, "E0E", (24,24)), endianness) ::
          (Reg_field ("SCTLR_EL1", 31, D_decreasing, "EE",  (25,25)), endianness) ::
          model.t.thread_isa_info.fixed_pseudo_registers

      | MIPS_ism ->
          (* TODO: set endianness? *)
          model.t.thread_isa_info.fixed_pseudo_registers
      | RISCV_ism ->
          model.t.thread_isa_info.fixed_pseudo_registers
      | X86_ism ->
          model.t.thread_isa_info.fixed_pseudo_registers
    in

    let thread_isa_info' =
      { model.t.thread_isa_info with fixed_pseudo_registers = fixed_pseudo_registers' }
    in
    let t' = {model.t with thread_isa_info = thread_isa_info'} in
    {model with t = t'}
  in

  let open MachineDefSystem in
  { isr_params            = model';
    isr_program           = prog;
    isr_return_addr       = return_addresses;
    isr_thread_ids        = tids;
    isr_register_data     = init_reg_data;
    isr_register_values   = init_reg_value;
    isr_first_instruction = initial_fetch_address;
    isr_memory            = init_write_events;
  }

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


