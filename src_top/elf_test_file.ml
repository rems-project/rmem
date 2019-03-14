(*======================================================================================*)
(*                                                                                      *)
(*                rmem executable model                                                 *)
(*                =====================                                                 *)
(*                                                                                      *)
(*  This file is:                                                                       *)
(*                                                                                      *)
(*  Copyright Shaked Flur, University of Cambridge                          2016-2018   *)
(*  Copyright Peter Sewell, University of Cambridge                              2016   *)
(*  Copyright Christopher Pulte, University of Cambridge                    2016-2018   *)
(*  Copyright Jon French, University of Cambridge                           2016-2018   *)
(*  Copyright Robert Norton-Wright, University of Cambridge                 2016-2017   *)
(*  Copyright Luc Maranget, INRIA, Paris, France                                 2017   *)
(*  Copyright Kathy Gray, University of Cambridge (when this work was done)      2017   *)
(*                                                                                      *)
(*  All rights reserved.                                                                *)
(*                                                                                      *)
(*  It is part of the rmem tool, distributed under the 2-clause BSD licence in          *)
(*  LICENCE.txt.                                                                        *)
(*                                                                                      *)
(*======================================================================================*)

module StringMap = Map.Make(String)

(* TODO: use elf functions directly rather than via sail_interface *)
(* TODO: have a byte-oriented interface to the ELF stuff, avoiding endianness concerns here *)

type word8 = int (* HACK *)

(* TODO: make addr be a big_int throughout, after elf_executable_file3.lem is fixed up? *)

(* can't use Elf_types.read_unsigned_char here as that doesn't generate an OCaml definition *)
(*let elf_types_read_unsigned_char = Ml_bindings.read_unsigned_char*)

let debug_pp_byte_list bs =
  "0x" ^
  (String.concat ""
    (List.map (fun i -> let s = (Printf.sprintf "%x" i) in if (String.length s = 1) then "0"^s else s) bs))

(*let pp_address (Interp_interface.Bytevector bs) = debug_pp_byte_list bs*)

let rec load_memory_segment' s   ((bytes : char list),addr) (mem: (Nat_big_num.num, word8) Pmap.map) =
  match bytes with
  | [] -> mem
  | byte::bytes' ->
    let data_byte = Char.code byte in
    (*Printf.printf "load memory %s  %s %s \n" s (Nat_big_num.to_string addr) (debug_pp_byte_list [data_byte]);*)
    let addr' = Nat_big_num.succ addr in
    let mem' = Pmap.add addr data_byte mem in
    load_memory_segment' s (bytes',addr') mem'

let rec load_memory_segment s (segment: Elf_interpreted_segment.elf64_interpreted_segment) (mem: (Nat_big_num.num,word8) Pmap.map) =
  Debug.print_string (Elf_interpreted_segment.string_of_elf64_interpreted_segment segment);
  let bytes = Byte_sequence.char_list_of_byte_sequence segment.Elf_interpreted_segment.elf64_segment_body in
  let addr = segment.Elf_interpreted_segment.elf64_segment_base in
  load_memory_segment' s (bytes,addr) mem

let rec load_memory_segments segments (mem_prog, mem_data) =
  begin match segments with
  | [] -> (mem_prog, mem_data)
  | segment::segments' ->
      let (x,w,r) = segment.Elf_interpreted_segment.elf64_segment_flags in
      let (mem_prog',mem_data') =
        (if x then
          (load_memory_segment "prog" segment mem_prog, mem_data)
        else
          (mem_prog, load_memory_segment "data" segment mem_data)
        )
      in
      load_memory_segments segments' (mem_prog', mem_data')
  end

let rec read_mem_new mem addr length : word8 list =
  if length = 0
  then []
  else
    begin match (Pmap.find addr mem) with
    | byte ->
        byte ::(read_mem_new mem (Nat_big_num.succ addr) (length - 1))
    | exception Not_found -> failwith "start address not found"
    end


(** ************************************************ *)


let initial_LR_sentinel = Nat_big_num.of_int 1027552 (* 0xFADE0 (* bclr forces alignment so this has to be *) *)


(* initial stack - could be shared between PPC and AArch64? *)
let initial_stack tid : ((Nat_big_num.num * int) (* entire footprint *) *
                           Nat_big_num.num (* initial SP *) *
                           int list (* initial data bytes *) *
                           string (* pp symbol*)) =
  let stack_base_address =
    Nat_big_num.sub
      (Nat_big_num.of_string "17592169267200"  (*"0xfffff000000"*) )
      (Nat_big_num.mul
         (Nat_big_num.of_int tid)
         (Nat_big_num.of_string "16777216"     (*"0x00001000000"*) )) in
  let stack_size_downwards = 1024 in
  let stack_size_upwards = 1024 in

  let stack_pointer = Nat_big_num.add stack_base_address (Nat_big_num.of_int stack_size_downwards) in
  let stack_size = stack_size_downwards + stack_size_upwards in
  let stack_footprint = (stack_base_address, stack_size) in
  let stack_data = Lem_list.replicate stack_size 0 in  (* initial data all 0 *)
  let stack_pp_symbol = Printf.sprintf "stack%n" tid in
  (stack_footprint, stack_pointer, stack_data, stack_pp_symbol)


let initial_stack_and_reg_data_of_PPC_elf_file e_entry memory elf_threads_list =
  let reg name =
    match MachineDefISAInfo.reg_from_data
            MachineDefISAInfoPPCGen.ppcgen_ism.MachineDefISAInfo.register_data_info name with
    | Some r -> r
    | None -> failwith ("\"" ^ name ^ "\" is not in 'MachineDefISAInfoPPCGen.ppcgen_ism.register_data_info'")
  in

  (* set up initial registers, per 3.4.1 of 64-bit PowerPC ELF Application Binary Interface Supplement 1.9 *)
  (* stack frames are described in ~/bitbucket/linksem/doc/PPC-elf64abi-1.7.pdf *)

  (* UNUSED: let auxiliary_vector_space = Nat_big_num.of_string "17592186042368" (*"0xffffffff800"*) in *)
  (* notionally there should be at least an AT_NULL auxiliary vector entry there, but our examples will never read it *)

  let stacks = List.map initial_stack elf_threads_list in

  let initial_stack_data_for_thread tid =
    let (((stack_base_address,stack_size) as stack_footprint), stack_pointer, stack_data, stack_pp_symbol) = List.nth stacks tid in
    [("initial_stack_data", stack_base_address, stack_data)] in

  let initial_stack_data = List.flatten (List.map initial_stack_data_for_thread elf_threads_list) in

  let initial_GPR1_stack_pointer_value tid =
    let (stack_footprint, stack_pointer, stack_data, stack_pp_symbol) = List.nth stacks tid in
    Sail_impl_base.register_value_for_reg_of_integer (reg "GPR1") stack_pointer in


  (* read TOC from the second field of the function descriptor pointed to by e_entry*)
  let initial_GPR2_TOC =
    Sail_impl_base.register_value_of_address (Sail_impl_base.address_of_byte_list (List.map Sail_impl_base.byte_of_int (read_mem_new memory (Nat_big_num.add (Nat_big_num.of_int 8) e_entry) 8))) Sail_impl_base.D_increasing
  in
  (* these initial register values are all mandated to be zero, but that's handled by the generic zeroing below
      let initial_GPR3_argc = (Nat_big_num.of_int 0) in
      let initial_GPR4_argv = (Nat_big_num.of_int 0) in
      let initial_GPR5_envp = (Nat_big_num.of_int 0) in
      let initial_FPSCR = (Nat_big_num.of_int 0) in
      *)

  let initial_LR_sentinel_value = Sail_impl_base.register_value_for_reg_of_integer (reg "LR") initial_LR_sentinel in
  let initial_register_abi_data tid : (string * Sail_impl_base.register_value) list =
    [ ("GPR1", initial_GPR1_stack_pointer_value tid);
      ("GPR2", initial_GPR2_TOC);
      ("LR", initial_LR_sentinel_value);
  (*
    ("GPR3", initial_GPR3_argc);
    ("GPR4", initial_GPR4_argv);
    ("GPR5", initial_GPR5_envp);
    ("FPSCR", initial_FPSCR);
    *)
    ] in

  (initial_stack_data, initial_register_abi_data)


let initial_stack_and_reg_data_of_AAarch64_elf_file e_entry memory elf_threads_list =
  let reg name =
    let registerdata =
      if !Globals.aarch64gen then
        MachineDefISAInfoAArch64.aarch64gen_ism.MachineDefISAInfo.register_data_info
      else
        MachineDefISAInfoAArch64.aarch64hand_ism.MachineDefISAInfo.register_data_info
    in
    match MachineDefISAInfo.reg_from_data registerdata name with
    | Some r -> r
    | None -> failwith ("\"" ^ name ^ "\" is not in 'register_data_info'")
  in

  (* we compiled a small program that prints out SP and run it a few
      times on the Nexus9, these are the results:
      0x0000007fe7f903e0
      0x0000007fdcdbf3f0
      0x0000007fcbe1ba90
      0x0000007fcf378280
      0x0000007fdd54b8d0
      0x0000007fd961bc10
      0x0000007ff3be6350
      0x0000007fd6bf6ef0
      0x0000007fff7676f0
      0x0000007ff2c34560 *)

  let stacks = List.map initial_stack elf_threads_list in

  let initial_stack_data_for_thread tid =
    let ((stack_base_address, _), _, stack_data, _) = List.nth stacks tid in
    [("initial_stack_data", stack_base_address, stack_data)]
  in

  let initial_stack_data = List.flatten (List.map initial_stack_data_for_thread elf_threads_list) in

(*  let initial_SP_EL0 = Nat_big_num.of_string "549739036672" (*"0x0000007fff000000"*) in*)
  let initial_SP_EL0_value tid =
    let (_, stack_pointer, _, _) = List.nth stacks tid in
    Sail_impl_base.register_value_for_reg_of_integer (reg "SP_EL0") stack_pointer
  in

  (* ELF says we need an initial zero doubleword there *)
  (* the code actually uses the stack, both above and below, so we map a bit more memory*)
(*
  let initial_stack_data =
    (* this is a fairly arbitrary chunk: *)
    (* let initial_stack_data_address = Nat_big_num.sub initial_GPR1_stack_pointer (Nat_big_num.of_int 128) in
        [("initial_stack_data", initial_stack_data_address, Lem_list.replicate (128+32) 0 ))] in *)

    (* FIXME: this is the stack memory that test 1938 actually uses *)
    (*[ ("initial_stack_data1", Nat_big_num.sub initial_SP_EL0 (Nat_big_num.of_int 128), Lem_list.replicate 8 0);
      ("initial_stack_data2", Nat_big_num.sub initial_SP_EL0 (Nat_big_num.of_int 8),   Lem_list.replicate 8 0);
      ("initial_stack_data3", Nat_big_num.add initial_SP_EL0 (Nat_big_num.of_int 16),  Lem_list.replicate 8 0);
    ]*)
    [ ("initial_stack_data1", Nat_big_num.sub initial_SP_EL0 (Nat_big_num.of_int 16),  Lem_list.replicate 8 0);
      ("initial_stack_data2", Nat_big_num.sub initial_SP_EL0 (Nat_big_num.of_int 8),   Lem_list.replicate 8 0)
    ]
*)

  let initial_LR_sentinel_value = Sail_impl_base.register_value_for_reg_of_integer (reg "R30") initial_LR_sentinel in

  let initial_TPIDR_EL0_value tid = Sail_impl_base.register_value_for_reg_of_integer (reg "TPIDR_EL0") (Nat_big_num.of_int tid) in

  let initial_register_abi_data tid : (string * Sail_impl_base.register_value) list =
    [ ("SP_EL0",        initial_SP_EL0_value tid);
      ("R30",           initial_LR_sentinel_value);
      ("TPIDR_EL0",     initial_TPIDR_EL0_value tid);
    ]
  in

  (initial_stack_data, initial_register_abi_data)


let initial_stack_and_reg_data_of_mips_elf_file e_entry memory elf_threads_list =
      let reg name =
        match MachineDefISAInfo.reg_from_data MachineDefISAInfoMIPS.mips_ism.MachineDefISAInfo.register_data_info name with
        | Some r -> r
        | None -> failwith ("\"" ^ name ^ "\" is not in 'MachineDefISAInfoMIPS.mips_ism.register_data_info'")
      in

  let stacks = List.map initial_stack elf_threads_list in

  let initial_stack_data_for_thread tid =
    let (((stack_base_address,stack_size) as stack_footprint), stack_pointer, stack_data, stack_pp_symbol) = List.nth stacks tid in
    [("initial_stack_data", stack_base_address, stack_data)] in

  let initial_stack_data = List.flatten (List.map initial_stack_data_for_thread elf_threads_list) in

  let initial_SP_value tid =
    let (((stack_base_address,stack_size) as stack_footprint), stack_pointer, stack_data, stack_pp_symbol) = List.nth stacks tid in
    Sail_impl_base.register_value_for_reg_of_integer (reg "GPR29") stack_pointer
  in

  let initial_LR_sentinel_value = Sail_impl_base.register_value_for_reg_of_integer (reg "GPR31") initial_LR_sentinel in

  let initial_T9_value = Sail_impl_base.register_value_of_address (Sail_impl_base.address_of_integer e_entry) Sail_impl_base.D_decreasing in

  let initial_register_abi_data tid : (string * Sail_impl_base.register_value) list =
    [("GPR25", initial_T9_value);
     ("GPR29", initial_SP_value tid);
     ("GPR31", initial_LR_sentinel_value) ]
  in
  (initial_stack_data, initial_register_abi_data)


let initial_stack_and_reg_data_of_riscv_elf_file symbol_map memory elf_threads_list =
      let reg name =
        match MachineDefISAInfo.reg_from_data MachineDefISAInfoRISCV.riscv_ism.MachineDefISAInfo.register_data_info name with
        | Some r -> r
        | None -> failwith ("\"" ^ name ^ "\" is not in 'MachineDefISAInfoRISCV.riscv_ism.register_data_info'")
      in

  let stacks = List.map initial_stack elf_threads_list in

  let initial_stack_data_for_thread tid =
    let (((stack_base_address,stack_size) as stack_footprint), stack_pointer, stack_data, stack_pp_symbol) = List.nth stacks tid in
    [("initial_stack_data", stack_base_address, stack_data)] in

  let initial_stack_data = List.flatten (List.map initial_stack_data_for_thread elf_threads_list) in

  let initial_SP_value tid =
    let (((stack_base_address,stack_size) as stack_footprint), stack_pointer, stack_data, stack_pp_symbol) = List.nth stacks tid in
    Sail_impl_base.register_value_for_reg_of_integer (reg "x2") stack_pointer
  in

  let initial_register_abi_data : (string * Sail_impl_base.register_value) list =
    [ ("x1", Sail_impl_base.register_value_for_reg_of_integer (reg "x1") initial_LR_sentinel);
      ("cur_privilege", Sail_impl_base.register_value_for_reg_of_integer (reg "cur_privilege") (Nat_big_num.of_int 2)); (* initially at Machine privilege *)
      ("misa", Sail_impl_base.register_value_for_reg_of_integer (reg "misa") (Nat_big_num.of_string "0x8000000000000881")); (* RV64IMA *)
    ]
  in

  (* The gp register (alias of x3) is initialised by the linker to "__global_pointer$" *)
  let initial_register_abi_data : (string * Sail_impl_base.register_value) list =
    match List.assoc "__global_pointer$" symbol_map with
    | (_, _, address, _, _) ->
        let gp_value = Sail_impl_base.register_value_for_reg_of_integer (reg "x3") address in
        ("x3", gp_value) :: initial_register_abi_data
    | exception Not_found -> initial_register_abi_data
  in

  let initial_register_abi_data_of_tid tid : (string * Sail_impl_base.register_value) list =
    ("x2", initial_SP_value tid) :: initial_register_abi_data
  in
  (initial_stack_data, initial_register_abi_data_of_tid)


type test =
 { symbol_map: Elf_file.global_symbol_init_info;
   segments: Elf_interpreted_segment.elf64_interpreted_segment list;
   e_entry: Nat_big_num.num;
   e_machine: Nat_big_num.num;
   elf_threads: int;
   dwarf_static: Dwarf.dwarf_static option;
 }

let mk_elf_threads_list elf_threads =
  let rec to_n ns n =
    if n=0 then [] else (to_n ns (n-1)) @ [n-1] in
  to_n [] elf_threads


type data = char list

let arch_of_test (test: test) : MachineDefISAInfo.isa_info =
  begin match Nat_big_num.to_int test.e_machine with
  | 21  (* EM_PPC64 *)   -> MachineDefISAInfoPPCGen.ppcgen_ism
  | 183 (* EM_ARACH64 *) when !Globals.aarch64gen ->
                            MachineDefISAInfoAArch64.aarch64gen_ism
  | 183 (* EM_ARACH64 *) when not !Globals.aarch64gen ->
                            MachineDefISAInfoAArch64.aarch64hand_ism
  | 8   (* EM_MIPS *)    -> MachineDefISAInfoMIPS.mips_ism
  | 243 (* EM_RISCV *)   -> MachineDefISAInfoRISCV.riscv_ism
  | _ ->
      Printf.eprintf "Unsupported architecture\n";
      exit 1

  end

let symbols_for_stacks threads =
  List.map
    (fun ((stack_base_address,stack_size), stack_pointer, stack_data, stack_pp_symbol) ->
      (stack_pp_symbol, (Elf_symbol_table.stt_notype,
                         Nat_big_num.of_int stack_size,
                         stack_base_address,
                         None,
                         Elf_symbol_table.stb_global)))
    (List.map initial_stack (mk_elf_threads_list threads))


(** invert the symbol table to use for pp *)
let symbol_table test : ((Sail_impl_base.address * int) * string) list =
  Debug.print_string "*************** symbol_table for pp **************\n";

  (* map symbol to (bindings, footprint),
  if a symbol appears more then once keep the one with higher
  precedence (stb_global > stb_weak > stb_local) *)
  let map_all =
    List.fold_left
      (fun map (name, (typ, size, address, mb, binding)) ->
          if String.length name <> 0                                                         (* name nonempty *)
            && (if String.length name = 1 then Char.code (String.get name 0) <> 0 else true) (* name not singleton null char *)
            && (not (Nat_big_num.equal address (Nat_big_num.of_int 0))                       (* address not 0 *)
            && ( (Nat_big_num.equal typ Elf_symbol_table.stt_object)                         (* either object, function, or local notype *)
             || (Nat_big_num.equal typ Elf_symbol_table.stt_func)
             || (Nat_big_num.equal typ Elf_symbol_table.stt_notype && Nat_big_num.equal binding Elf_symbol_table.stb_local))
            ) then
            (* force size to 0 for non-object symbols to avoid bad pp behaviour, as the size of stt_func OPD entities seems to be the size of the associated bytes, which are somewhere else *)
            let my_size = if Nat_big_num.equal typ Elf_symbol_table.stt_object then Nat_big_num.to_int size else 0 in
            try
              let ((_,_,_,_,binding'), _) = StringMap.find name map in
              if  Nat_big_num.equal binding' Elf_symbol_table.stb_local ||
                  Nat_big_num.equal binding Elf_symbol_table.stb_global then
                StringMap.add name ((typ, size, address, mb, binding), (Sail_impl_base.address_of_integer address, my_size)) map
              else map
            with Not_found ->
              StringMap.add name ((typ, size, address, mb, binding), (Sail_impl_base.address_of_integer address, my_size)) map

          else map
      )
      StringMap.empty
      test.symbol_map
  in

  (* turn into a name-indexed association list *)
  let ndfps_all = StringMap.bindings map_all in

  (* if two symbols appear with the same address, if one is an object or func and the other is not then suppress the other *)
  let rec f =
    function
    | [] -> []
    | ((name, ((typ,size,address,mb,binding), fp)) as ndfp) :: ndfps' ->
          begin match List.exists
              (function (name', ((typ',size',address',mb',binding'), fp')) ->
                (Nat_big_num.equal address address')
                  && ( (Nat_big_num.equal typ' Elf_symbol_table.stt_object)
                    || (Nat_big_num.equal typ' Elf_symbol_table.stt_func))
                  && not  ( (Nat_big_num.equal typ Elf_symbol_table.stt_object)
                    || (Nat_big_num.equal typ Elf_symbol_table.stt_func))
              ) ndfps' with
          | true -> f ndfps'
          | false ->
              Debug.print_string (Printf.sprintf "(%s, %s) %s\n" (Nat_big_num.to_string address) (Nat_big_num.to_string size) name);
              ndfp :: f ndfps'
          end
  in
  List.map (function (name, ((typ,size,address,mb,binding),fp)) -> (fp, name)) (f ndfps_all)


let show_mem_locations (test: test) : (Sail_impl_base.address * int) list =
  let map_globals_for_show =
    List.fold_left
      (fun map (name, (typ, size, address, _, _)) ->
          if String.length name <> 0                                                         (* name nonempty *)
            && (if String.length name = 1 then Char.code (String.get name 0) <> 0 else true) (* name not singleton null char *)
            && (not (Nat_big_num.equal address (Nat_big_num.of_int 0))                       (* address not 0 *)
            && (not (Nat_big_num.equal size (Nat_big_num.of_int 0)))                         (* size not 0 *)
            && (Nat_big_num.equal typ Elf_symbol_table.stt_object)                           (* object *)
            )
          then
            let my_size = Nat_big_num.to_int size in
            StringMap.add name (Sail_impl_base.address_of_integer address, my_size) map
          else
            map
      )
      StringMap.empty
      test.symbol_map
  in

  StringMap.bindings map_globals_for_show |> List.split |> snd


let test_info (test: test) (name: string) : Test.info =
  let mems = show_mem_locations test in
  { Test.name           = name;
    Test.ism            = (arch_of_test test).MachineDefISAInfo.ism;
    Test.thread_count   = test.elf_threads;
    Test.symbol_map     = test.symbol_map;
    Test.symbol_table   = symbol_table test;
    Test.dwarf_static   = test.dwarf_static;

    Test.show_regs   = [];
    Test.show_mem    = mems;
    Test.filter_regs = [];
    Test.filter_mem  = [];

    Test.info           = [];
    Test.filter         = None;
    Test.constr         = ConstrGen.ExistsState (ConstrGen.And []); (* i.e. "true" *)
  }


let read (name: string) info (isa_callback: (MachineDefInstructionSemantics.instruction_semantics_mode -> unit) option) : Test.info * test =
  Debug.timer_start_total ();
  Debug.print_string "elf read\n";

  (* call ELF analyser on file *)
  let (elf_file, elf_epi, symbol_map) =
    begin match info with
    | Error.Fail s -> Warn.fatal "populate_and_obtain_global_symbol_init_info: %s" s
    | Error.Success
        ((elf_file: Elf_file.elf_file),
         (elf_epi: Sail_interface.executable_process_image),
         (symbol_map: Elf_file.global_symbol_init_info))
        (*       (symbol_map: Elf_executable_file.global_symbol_init_info)) *)
        ->
          Debug.print_string (Sail_interface.string_of_executable_process_image elf_epi);
          (elf_file, elf_epi, symbol_map)
    end
  in

  Debug.print_string "elf segments etc\n";
  let (segments, e_entry, e_machine, (dso: Dwarf.dwarf_static option)) =
    begin match elf_epi, elf_file with
    | (Sail_interface.ELF_Class_32 _, _)  -> Warn.fatal "cannot handle ELF_Class_32"
    | (_, Elf_file.ELF_File_32 _)  -> Warn.fatal "cannot handle ELF_File_32"
    | (Sail_interface.ELF_Class_64 (segments,e_entry,e_machine), Elf_file.ELF_File_64 f1) ->
        (* remove all the auto generated segments (they contain only 0s) *)
        let segments =
          Lem_list.mapMaybe
            (fun (seg, prov) -> if prov = Elf_file.FromELF then Some seg else None)
            segments
        in
        let dso =
          (*Printf.printf "!Globals.use_dwarf = %b" !Globals.use_dwarf;flush stdout;*)
          begin match !Globals.use_dwarf (* TODO: update wrt new option management scheme *) with
          | false -> (Debug.print_string2 "use_dwarf false\n"; None)
          | true ->
              match Dwarf.extract_dwarf_static (Elf_file.ELF_File_64 f1) with
              | None -> Warn.fatal "extract_dwarf_static failed"
              | Some ds ->
                  Debug.print_string2 (Dwarf.pp_analysed_location_data ds.Dwarf.ds_dwarf ds.Dwarf.ds_analysed_location_data);
                  Debug.print_string2 (Dwarf.pp_evaluated_frame_info ds.Dwarf.ds_evaluated_frame_info);
                  Some ds
          end in
        (segments,e_entry,e_machine, dso)
    end
  in

  Debug.print_string "elf test\n";
  let test =
    { symbol_map  = symbol_map @ (symbols_for_stacks !Globals.elf_threads);
      segments    = segments;
      e_entry     = e_entry;
      e_machine   = e_machine;
      elf_threads = !Globals.elf_threads;
      dwarf_static= dso;
    }
  in
  Globals.set_model_ism (arch_of_test test);

  begin match isa_callback with
  | Some f -> f (!Globals.model_params).MachineDefParams.t.MachineDefParams.thread_isa_info.MachineDefISAInfo.ism
  | _ -> ()
  end;

  let info = test_info test name in

  Globals.add_bt_and_sm_to_model_params info.Test.symbol_table;

  (info, test)

let read_data (name: string) (data: data) (isa_callback: (MachineDefInstructionSemantics.instruction_semantics_mode -> unit) option) : Test.info * test =
  read name (Sail_interface.populate_and_obtain_global_symbol_init_info' (Byte_sequence.byte_sequence_of_byte_list data)) isa_callback

let read_file (name: string) (isa_callback: (MachineDefInstructionSemantics.instruction_semantics_mode -> unit) option) : Test.info * test =
  read name (Sail_interface.populate_and_obtain_global_symbol_init_info name) isa_callback

let initial_state_record elf_test (isa_defs: (module Isa_model.ISADefs)) model : MachineDefSystem.initial_state_record =
  let elf_threads_list = mk_elf_threads_list elf_test.elf_threads in

  let (program_memory  : (Nat_big_num.num, word8) Pmap.map),
      (data_memory     : (Nat_big_num.num, word8) Pmap.map) =
    let empty_map = Pmap.empty Nat_big_num.compare in
    load_memory_segments elf_test.segments (empty_map, empty_map)
  in
  let memory = Pmap.union program_memory data_memory in

  let (startaddr, initial_stack_data, initial_register_abi_data) =
    match Nat_big_num.to_int elf_test.e_machine with
    | 21  (* EM_PPC64 *) ->
        let startaddr =
          let e_entry = Uint64_wrapper.of_bigint elf_test.e_entry in
          match Abi_power64.abi_power64_compute_program_entry_point elf_test.segments e_entry with
            | Error.Fail s -> Warn.fatal "Failed computing entry point"
            | Error.Success s -> s
        in
        let (initial_stack_data, initial_register_abi_data) =
          initial_stack_and_reg_data_of_PPC_elf_file elf_test.e_entry memory elf_threads_list in

        (startaddr,
          initial_stack_data,
          initial_register_abi_data)

    | 183 (* EM_AARCH64 *) ->
        let startaddr =
          let e_entry = Uint64_wrapper.of_bigint elf_test.e_entry in
          match Abi_aarch64_le.abi_aarch64_le_compute_program_entry_point elf_test.segments e_entry with
            | Error.Fail s -> Warn.fatal "Failed computing entry point"
            | Error.Success s -> s
        in

        let (initial_stack_data, initial_register_abi_data) =
          initial_stack_and_reg_data_of_AAarch64_elf_file elf_test.e_entry memory elf_threads_list in

        (startaddr,
          initial_stack_data,
          initial_register_abi_data)
    | 8 (* EM_MIPS *) ->
        let startaddr =
          let e_entry = Uint64_wrapper.of_bigint elf_test.e_entry in
          match Abi_mips64.abi_mips64_compute_program_entry_point elf_test.segments e_entry with
            | Error.Fail s -> Warn.fatal "Failed computing entry point"
            | Error.Success s -> s
        in
        let (initial_stack_data, initial_register_abi_data) =
          initial_stack_and_reg_data_of_mips_elf_file elf_test.e_entry memory elf_threads_list in

        (startaddr,
          initial_stack_data,
          initial_register_abi_data)
    | 243 (* EM_RISCV *) ->
       let startaddr =
         let e_entry = Uint64_wrapper.of_bigint elf_test.e_entry in
         match Abi_riscv.abi_riscv_compute_program_entry_point elf_test.segments e_entry with
         | Error.Fail s -> Warn.fatal "Failed computing entry point"
         | Error.Success s -> s
       in
       let (initial_stack_data, initial_register_abi_data) =
         initial_stack_and_reg_data_of_riscv_elf_file elf_test.symbol_map memory elf_threads_list in
       (startaddr, initial_stack_data, initial_register_abi_data)

    | e_machine ->
        Printf.eprintf "Rmem can't handle the e_machine value %d, only EM_PPC64, EM_AARCH64 and EM_RISCV are supported.\n" e_machine;
        exit 1
  in

  (* pull the object symbols from the symbol table *)
  let symbol_table : (string * Nat_big_num.num * int * word8 list (*their bytes*)) list =
    let rec convert_symbol_table symbol_map =
      begin match symbol_map with
      | [] -> []
      | ((name, (typ, size, address, mbs, binding)) :: symbol_map' : Elf_file.global_symbol_init_info)->
          (*(Debug.print_string "**** name = %s  typ = %d  " name  (Nat_big_num.to_int typ);*)
          if Nat_big_num.equal typ Elf_symbol_table.stt_object && not (Nat_big_num.equal size (Nat_big_num.of_int 0)) then
            (
              (* an object symbol - map *)
              (*Debug.print_string "*** good symbol: size %d ***\n" (Nat_big_num.to_int size);*)
              let bytes =
                match mbs with
                | None -> raise (Failure "this cannot happen")
                | Some bytes ->
                    Byte_sequence.char_list_of_byte_sequence bytes
                    |> List.map Char.code
              in
              (name, address, List.length bytes, bytes) :: convert_symbol_table symbol_map'
            )
          else
            (*Debug.print_string "*** bad symbol: size %d ***\n" (Nat_big_num.to_int size);*)
            (* not an object symbol or of zero size - ignore *)
            convert_symbol_table symbol_map'
      end
    in
    (List.map (fun (n,a,bs) -> (n,a,List.length bs,bs)) initial_stack_data) @ convert_symbol_table elf_test.symbol_map
  in

  (* Now we examine the rest of the data memory, removing the footprint of the object symbols and chunking it into aligned chunks *)
  (*
  Debug.print_string "memory: \n";
  List.iter (fun (bs,b) -> Debug.print_string "memory %s %s \n" (debug_pp_byte_list bs) (debug_pp_byte_list [b])) (Pmap.bindings_list memory);
  *)
  let rec remove_symbols_from_data_memory data_mem symbols =
    match symbols with
    | [] -> data_mem
    | (name,address,size,bs)::symbols' ->
        let data_mem' =
          Pmap.filter
            (fun a v ->
(*                  let a' : Nat_big_num.num = Sail_impl_base.integer_address_of_int_list a in*)
              not (Nat_big_num.greater_equal a address && Nat_big_num.less a (Nat_big_num.add (Nat_big_num.of_int (List.length bs)) address)))
            data_mem in
        remove_symbols_from_data_memory data_mem' symbols' in

  let trimmed_data_memory : (Nat_big_num.num * word8) list =
    Pmap.bindings_list (remove_symbols_from_data_memory memory symbol_table) in

  (* make sure that's ordered increasingly.... *)
  let trimmed_data_memory =
    List.sort (fun (a,b) (a',b') -> Nat_big_num.compare a a') trimmed_data_memory in

  let aligned a n =  (* a mod n = 0 *)
    let n_big = Nat_big_num.of_int n in
    Nat_big_num.equal (Nat_big_num.modulus a n_big) ((Nat_big_num.of_int 0)) in

  let isplus a' a n =   (* a' = a+n *)
    Nat_big_num.equal a' (Nat_big_num.add (Nat_big_num.of_int n) a) in

  let rec chunk_data_memory dm =
    match dm with
    | (a0,b0)::(a1,b1)::(a2,b2)::(a3,b3)::(a4,b4)::(a5,b5)::(a6,b6)::(a7,b7)::dm'  when
        (aligned a0 8 && isplus a1 a0 1 && isplus a2 a0 2 && isplus a3 a0 3 && isplus a4 a0 4 && isplus a5 a0 5 && isplus a6 a0 6 && isplus a7 a0 7) ->
          (a0,8,[b0;b1;b2;b3;b4;b5;b6;b7]) :: chunk_data_memory dm'
    | (a0,b0)::(a1,b1)::(a2,b2)::(a3,b3)::dm' when
        (aligned a0 4 && isplus a1 a0 1 && isplus a2 a0 2 && isplus a3 a0 3) ->
          (a0,4,[b0;b1;b2;b3]) :: chunk_data_memory dm'
    | (a0,b0)::(a1,b1)::dm' when
        (aligned a0 2 && isplus a1 a0 1) ->
          (a0,2,[b0;b1]) :: chunk_data_memory dm'
    | (a0,b0)::dm' ->
        (a0,1,[b0]):: chunk_data_memory dm'
    | [] -> [] in

  let fake_symbols_for_chunked_data_memory =
    if !Globals.suppress_non_symbol_memory then
      []
    else
      List.mapi (fun i -> fun (a,size,bs) -> ("fake"^string_of_int i, a, size, bs)) (chunk_data_memory trimmed_data_memory) in

  (* List.iter (fun (name,address,bytes) -> Debug.print_string "%s %s %s\n" name (pp_address (address_of_big_int address)) (debug_pp_byte_list bytes)) fake_symbols_for_chunked_data_memory; *)

  let compare_symbol_table_entry
        (name, (address:Nat_big_num.num), (size:int), (bytes: word8 list))
        (name', (address':Nat_big_num.num), (size':int), (bytes': word8 list)) =
    Nat_big_num.compare address address' in


  let initial_writes =
    (* thread and ioid for initial writes *)
    let tid = Test.init_thread in (* using 0 will clash with the id_state of the thread-subsystem *)
    (if elf_test.elf_threads > tid then failwith (Printf.sprintf "elf_threads greater than %d (used for initial writes)" tid) else ());
    let thread_ist = MachineDefFreshIds.initial_id_state tid in
    let (ioid, _) = MachineDefFreshIds.gen_fresh_id thread_ist in
    let ioid_ist = MachineDefFreshIds.initial_id_state ioid in

    (* construct initial writes *)
    let rec mk_initial_writes symbol_table ist : (MachineDefEvents.write list * (MachineDefEvents.ioid MachineDefFreshIds.id_state)) =
      match symbol_table with
      | [] -> ([], ist)
      | (name, (address:Nat_big_num.num), (size:int), (bytes: word8 list))::symbol_table' ->

          (*Debug.print_string " construct initial write: (%s, %i) %s\n" (Nat_big_num.to_string address) size name;*)
          let a = Sail_impl_base.address_of_integer address in
          let v = List.map Sail_impl_base.memory_byte_of_int bytes in
          let fp = (a, List.length bytes) in
          let (ws, ist) = MachineDefEvents.make_write_events_big_split ist tid ioid fp v Sail_impl_base.Write_plain in
          let (ws', ist) = mk_initial_writes symbol_table' ist in
          (ws@ws', ist)
    in
    let (initial_writes, _) = mk_initial_writes (List.sort compare_symbol_table_entry (symbol_table @ fake_symbols_for_chunked_data_memory)) ioid_ist in
    initial_writes
  in

  let pmap_map_map_map
      (fkey: 'key1 -> 'key2)
      (fval: 'a1 -> 'a2)
      (m1:   ('key1, 'a1) Pmap.map)
      (m2:   ('key2, 'a2) Pmap.map)
      : ('key2,'a2) Pmap.map
    =
    Pmap.fold
      (fun k1 v1 m2 -> Pmap.add (fkey k1) (fval v1) m2)
      m1
      m2
  in

  let program_memory : (Sail_impl_base.address, Sail_impl_base.byte) Pmap.map =
    pmap_map_map_map
      Sail_impl_base.address_of_integer
      Sail_impl_base.byte_of_int
      (program_memory: (Nat_big_num.num, word8) Pmap.map)
      MachineDefSystem.empty_elf_memory  in


  let module ISADefs = (val isa_defs) in

  let initial_register_state =
    fun tid ->
      let initial_register_abi_data_tid = initial_register_abi_data tid in
      fun rbn ->
      (* CP: TODO? Check that there is not conflict between initial_register_abit_data_tid and
       * the fixed_pseudo_registers list? *)
        begin try List.assoc rbn initial_register_abi_data_tid with
        | Not_found ->
            (MachineDefThreadSubsystemUtils.register_state_zero ISADefs.reg_data) tid rbn
        end
  in

  let program_memory =
    MachineDefSystem.elf_program_memory
      program_memory
      (Globals.get_endianness ())
  in

  let initial_LR_sentinel = Sail_impl_base.address_of_integer initial_LR_sentinel in

  (* list of tids 0,1,... *)
  let tids = Lem_list.genlist (fun n -> n) elf_test.elf_threads in

  let first_instruction = function
    | 0 -> Some (Sail_impl_base.address_of_integer startaddr)
    | _ -> None
  in

  let model' =
    let fixed_pseudo_registers' =
      let open Sail_impl_base in
      let open MachineDefParams in
      let open MachineDefInstructionSemantics in
      let open MachineDefInstructionSemantics in
      let open MachineDefISAInfo in
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
          (* TODO: set endianness? *)
          model.t.thread_isa_info.fixed_pseudo_registers
      | X86_ism ->
          (* TODO: set endianness? *)
          model.t.thread_isa_info.fixed_pseudo_registers
    in
    let open MachineDefParams in
    let open MachineDefISAInfo in
    {model with t =
      {model.t with thread_isa_info =
        {model.t.thread_isa_info with fixed_pseudo_registers =
          fixed_pseudo_registers'}}}
  in

  let open MachineDefSystem in
  { isr_params            = model';
    isr_program           = program_memory;
    isr_return_addr       = List.map (fun tid -> (tid, initial_LR_sentinel)) tids;
    isr_thread_ids        = tids;
    isr_register_data     = initial_register_abi_data;
    isr_register_values   = initial_register_state;
    isr_first_instruction = first_instruction;
    isr_memory            = initial_writes;
  }
