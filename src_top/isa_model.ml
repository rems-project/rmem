(*===============================================================================*)
(*                                                                               *)
(*                rmem executable model                                          *)
(*                =====================                                          *)
(*                                                                               *)
(*  This file is:                                                                *)
(*                                                                               *)
(*  Copyright Shaked Flur, University of Cambridge          2017-2018            *)
(*  Copyright Robert Norton-Wright, University of Cambridge      2017            *)
(*  Copyright Jon French, University of Cambridge           2017-2018            *)
(*  Copyright Christopher Pulte, University of Cambridge    2017-2018            *)
(*                                                                               *)
(*  All rights reserved.                                                         *)
(*                                                                               *)
(*  It is part of the rmem tool, distributed under the 2-clause BSD licence in   *)
(*  LICENCE.txt.                                                                 *)
(*                                                                               *)
(*===============================================================================*)

open InstructionSemantics

type instruction_ast = InstructionSemantics.instruction_ast

module type TransSail = sig
  type instruction
  type labelmap = (string * int) list

  val shallow_ast_to_herdtools_ast : instruction_ast -> instruction
  val herdtools_ast_to_shallow_ast : instruction -> instruction_ast

  val herdtools_ast_to_interp_instruction : instruction -> Interp_interface.instruction
  val interp_instruction_to_herdtools_ast : Interp_interface.instruction -> instruction

  val unlabelize_ins :
      (string -> int) (** global variable lookup function *)
      -> labelmap     (** label locations *)
      -> int          (** current instruction index *)
      -> instruction  (** current instruction, possibly with labels *)
      -> instruction  (** unlabelized instruction *)
  val labelize_ins :
    (Sail_impl_base.address -> string option)
    -> Sail_impl_base.address
    -> instruction
    -> instruction

  val end_ins : instruction (** special to stop fetching *)
end

module RiscvTransSail : TransSail = struct
  type instruction = Riscv_types.ast2
  type labelmap = (string * int) list

  let shallow_ast_to_herdtools_ast _ = failwith "stub riscv shallow_ast_to_herdtools_ast"
  let herdtools_ast_to_shallow_ast _ = failwith "stub riscv herdtools_ast_to_shallow_ast"

  let herdtools_ast_to_interp_instruction _ = failwith "stub riscv herdtools_ast_to_interp_instruction"
  let interp_instruction_to_herdtools_ast _ = failwith "stub riscv interp_instruction_to_herdtools_ast"
  let unlabelize_ins _ _ _ _ = failwith "stub riscv unlabelize_ins"
  let labelize_ins _ _ _ = failwith "stub riscv labelize_ins"

  let end_ins = Riscv_types.STOP_FETCHING ()
end

(********************************************************************)

(* abstract the Sail ISA specific data *)

let memo_unit (f : unit -> 'a) =
  let v = (ref None : 'a option ref) in
  let g ?(no_memo = false) () =
    if no_memo then begin
        v := None;
        f ()
      end
    else
      match !v with
      | Some x -> x
      | None -> begin
          let x = f () in
          v := Some x;
          x
        end
  in
  g

module type ISADefs = sig
  val name : string
  val reg_data : BasicTypes.registerdata

  val isa_defs_thunk : ?no_memo:bool -> unit -> Interp_interface.specification
  val interp2_isa_defs_thunk : ?no_memo:bool -> unit -> (Type_check.tannot Ast.defs * Type_check.Env.t)
  val isa_memory_access : (Interp_interface.memory_reads *
                        Interp_interface.memory_read_taggeds *
                        Interp_interface.memory_writes *
                        Interp_interface.memory_write_eas *
                        Interp_interface.memory_write_vals *
                        Interp_interface.memory_write_vals_tagged *
                        Interp_interface.barriers *
                        Interp_interface.excl_res)
  val isa_externs : Interp_interface.external_functions
end

let empty_isa_defs = memo_unit (fun () -> Interp_ast.Defs [])
let empty_interp2_isa_defs = memo_unit (fun () -> (Ast.Defs [], Type_check.initial_env))

module PPCGenISADefs : ISADefs = struct
  let name = "PPC"
  let reg_data = IsaInfoPPCGen.ppcgen_ism.BasicTypes.register_data_info
  let isa_defs_thunk = memo_unit (fun () -> Screen.unmarshal_defs "PPCGen")
  let interp2_isa_defs_thunk = empty_interp2_isa_defs
  let isa_memory_access = (Power_extras.power_read_memory_functions,
                            [],
                            [],
                            Power_extras.power_memory_eas,
                            Power_extras.power_memory_vals,
                            [],
                            Power_extras.power_barrier_functions,
                            None)
  let isa_externs = Power_extras.power_externs
end

module AArch64ISADefs : ISADefs = struct
    let name = "AArch64"
    let reg_data = IsaInfoAArch64.aarch64hand_ism.BasicTypes.register_data_info
    let isa_defs_thunk = memo_unit (fun () -> Screen.unmarshal_defs "AArch64")
    let interp2_isa_defs_thunk = empty_interp2_isa_defs
    let isa_memory_access = (ArmV8_extras.aArch64_read_memory_functions,
                              [],
                              ArmV8_extras.aArch64_memory_writes,
                              ArmV8_extras.aArch64_memory_eas,
                              ArmV8_extras.aArch64_memory_vals,
                              [],
                              ArmV8_extras.aArch64_barrier_functions,
                              ArmV8_extras.aArch64_excl_res)
    let isa_externs = []
end

module AArch64GenISADefs : ISADefs = struct
    let name = "AArch64Gen"
    let reg_data = IsaInfoAArch64.aarch64gen_ism.BasicTypes.register_data_info
    let isa_defs_thunk = memo_unit (fun () -> Screen.unmarshal_defs "AArch64Gen")
    let interp2_isa_defs_thunk = empty_interp2_isa_defs
    let isa_memory_access = ([], (*ArmV8Gen_extras.aArch64_read_memory_functions*)
                              [],
                              [], (*ArmV8Gen_extras.aArch64_memory_writes*)
                              [], (*ArmV8Gen_extras.aArch64_memory_eas*)
                              [], (*ArmV8Gen_extras.aArch64_memory_vals*)
                              [],
                              [], (*ArmV8Gen_extras.aArch64_barrier_functions*)
                              None)
    let isa_externs = []
end

module MIPS64ISADefs : ISADefs = struct
    let name = "MIPS"
    let reg_data = IsaInfoMIPS.mips_ism.BasicTypes.register_data_info
    let isa_defs_thunk = memo_unit (fun () -> Screen.unmarshal_defs "MIPS64")
    let interp2_isa_defs_thunk = empty_interp2_isa_defs
    let isa_memory_access = (Mips_extras.mips_read_memory_functions,
                              [],
                              Mips_extras.mips_memory_writes,
                              Mips_extras.mips_memory_eas,
                              Mips_extras.mips_memory_vals,
                              [],
                              Mips_extras.mips_barrier_functions,
                              None)
    let isa_externs = []
end

module RISCVISADefs : ISADefs = struct
    let name = "RISCV"
    let reg_data = IsaInfoRISCV.riscv_ism.BasicTypes.register_data_info
    let isa_defs_thunk = empty_isa_defs
    let interp2_isa_defs_thunk = memo_unit (fun () -> Screen.unmarshal_interp2_defs "riscv")
    (* let isa_memory_access = (Riscv_extras.riscv_read_memory_functions,
     *                           [],
     *                           Riscv_extras.riscv_memory_writes,
     *                           Riscv_extras.riscv_memory_eas,
     *                           Riscv_extras.riscv_memory_vals,
     *                           [],
     *                           Riscv_extras.riscv_barrier_functions,
     *                           Riscv_extras.riscv_speculate_conditional_success) *)
    let isa_memory_access = ([], [], [], [], [], [], [], None)
    let isa_externs = []
end

module X86ISADefs : ISADefs = struct
    let name = "X86"
    let reg_data = IsaInfoX86.x86_ism.BasicTypes.register_data_info
    let isa_defs_thunk = memo_unit (fun () -> Screen.unmarshal_defs "X86")
    let interp2_isa_defs_thunk = empty_interp2_isa_defs
    let isa_memory_access = (X86_extras.x86_read_memory_functions,
                              [],
                              X86_extras.x86_memory_writes,
                              X86_extras.x86_memory_eas,
                              X86_extras.x86_memory_vals,
                              [],
                              X86_extras.x86_barrier_functions,
                              None)
    let isa_externs = []
end

let all_isa_defs : (module ISADefs) list = [
    (module PPCGenISADefs);
    (module AArch64ISADefs);
(*     (module AArch64GenISADefs); *)
    (module MIPS64ISADefs);
    (module RISCVISADefs);
    (module X86ISADefs);
  ]

(********************************************************************)

module type S = sig
  module ISADefs : ISADefs

  val instruction_semantics : instruction_semantics_mode ->
      RunOptions.t -> instruction_semantics
end


module Make (ISADefs: ISADefs) (TransSail: TransSail) : S = struct
  module ISADefs = ISADefs

  let initialise_interp_semantics
        compare_analyses
        ism
        context endianness
    =

    let instruction_to_interp_instruction = function
      | PPCGEN_instr instr -> Power_toFromInterp.astToInterpValue instr
      | AArch64_instr instr -> ArmV8_toFromInterp.astToInterpValue0 instr
      | MIPS_instr instr -> Mips_toFromInterp.astToInterpValue1 instr
      | RISCV_instr instr -> failwith "not implemented yet"
      | X86_instr instr -> X86_toFromInterp.astToInterpValue3 instr
      | Fetch_error -> failwith "fetch error"
    in

    let interp_instruction_to_instruction instr = match ism with
       | PPCGEN_ism -> PPCGEN_instr (Power_toFromInterp.astFromInterpValue instr)
       | AARCH64_ism AArch64HandSail -> AArch64_instr (ArmV8_toFromInterp.astFromInterpValue0 instr)
       | AARCH64_ism AArch64GenSail -> failwith "not implemented yet"
       | MIPS_ism -> MIPS_instr (Mips_toFromInterp.astFromInterpValue1 instr)
       | RISCV_ism -> failwith "not implemented yet"
       | X86_ism -> X86_instr (X86_toFromInterp.astFromInterpValue3 instr)
    in


    let decode_error_to_decode_error opcode = function
      | Interp_interface.Unsupported_instruction_error instr ->
         Unsupported_instruction_error0
          (opcode,interp_instruction_to_instruction instr)
      | Interp_interface.Not_an_instruction_error opcode ->
         Not_an_instruction_error0 opcode
      | Interp_interface.Internal_error string ->
         Internal_decode_error string
    in

    let interp__initial_outcome_s_of_instruction eager instruction : Sail_impl_base.outcome_S =
      let instruction = instruction_to_interp_instruction instruction in
      let interp_mode eager = Interp_inter_imp.make_mode eager false !Globals.debug_sail_interp in
      Interp_inter_imp.initial_outcome_s_of_instruction
        Printing_functions.pp_instruction_state context (interp_mode eager) instruction in


    let interp__instruction_analysis outcome_s instruction analysis_function
          reg_info nia_reg environment =

      let nias_function = InstructionSemantics.interp_nias_of_instruction instruction in
      let instruction = instruction_to_interp_instruction instruction in
      let interp_exhaustive = match outcome_s with
        | (_,Some (_,interp_exhaustive)) -> interp_exhaustive
        | _ -> failwith "interp__instruction_analysis outcome_s does not contain Some in snd" in
      let ism_s = match ism with
        | PPCGEN_ism -> "PPCGEN_ism"
        | AARCH64_ism AArch64HandSail -> "AArch64HandSail"
        | AARCH64_ism AArch64GenSail -> "AArch64GenSail"
        | MIPS_ism -> "MIPS_ism"
        | RISCV_ism -> "RISCV_ism"
        | X86_ism -> "X86_ism"
      in

      if compare_analyses then
        let open Params in
        Interp_inter_imp.interp_compare_analyses
          print_endline
          (RegUtils.non_pseudo_registers (!Globals.model_params.t))
          context endianness interp_exhaustive instruction nia_reg nias_function ism_s environment
          analysis_function reg_info

      else
        Interp_inter_imp.interp_instruction_analysis context interp_exhaustive instruction
          nia_reg nias_function ism_s environment in


    let interp__decode_to_instruction (address : Sail_impl_base.address) (opcode : Sail_impl_base.opcode) =
      match Interp_inter_imp.decode_to_instruction context None opcode with
      | Interp_interface.IDE_instr instruction ->
         let instruction = interp_instruction_to_instruction instruction in
         FDO_success (address,Some opcode,instruction)
      | Interp_interface.IDE_decode_error de ->
         FDO_decode_error (decode_error_to_decode_error opcode de)
    in

    (fun eager ->
      { initial_outcome_s_of_instruction0 = interp__initial_outcome_s_of_instruction eager;
        instruction_analysis0 = interp__instruction_analysis;
        decode_to_instruction0 = interp__decode_to_instruction;
      }
    )

  let initialise_interp2_semantics compare_analyses ism state endianness =


    let interp2__rk_to_rk v =
      let open Value in
      let open Sail_impl_base in
      match v with
      | V_ctor ("Read_plain", [])                         -> Read_plain
      | V_ctor ("Read_reserve", [])                       -> Read_reserve
      | V_ctor ("Read_acquire", [])                       -> Read_acquire
      | V_ctor ("Read_exclusive", [])                     -> Read_exclusive
      | V_ctor ("Read_exclusive_acquire", [])             -> Read_exclusive_acquire
      | V_ctor ("Read_stream", [])                        -> Read_stream
      | V_ctor ("Read_RISCV_acquire", [])                 -> Read_RISCV_acquire_RCsc
      | V_ctor ("Read_RISCV_strong_acquire", [])          -> Read_RISCV_acquire_release
      | V_ctor ("Read_RISCV_reserved", [])                -> Read_RISCV_reserved
      | V_ctor ("Read_RISCV_reserved_acquire", [])        -> Read_RISCV_reserved_acquire_RCsc
      | V_ctor ("Read_RISCV_reserved_strong_acquire", []) -> Read_RISCV_reserved_acquire_release
      | V_ctor ("Read_X86_locked", [])                    -> Read_X86_locked
      | _ -> failwith "unknown read kind in interp2__rk_to_rk"
    in

    let interp2__wk_to_wk v =
      let open Value in
      let open Sail_impl_base in
      match v with
      | V_ctor ("Write_plain", [])                            -> Write_plain
      | V_ctor ("Write_conditional", [])                      -> Write_conditional
      | V_ctor ("Write_release", [])                          -> Write_release
      | V_ctor ("Write_exclusive", [])                        -> Write_exclusive
      | V_ctor ("Write_exclusive_release", [])                -> Write_exclusive_release
      | V_ctor ("Write_RISCV_release", [])                    -> Write_RISCV_release_RCsc
      | V_ctor ("Write_RISCV_strong_release", [])             -> Write_RISCV_acquire_release
      | V_ctor ("Write_RISCV_conditional", [])                -> Write_RISCV_conditional
      | V_ctor ("Write_RISCV_conditional_release", [])        -> Write_RISCV_conditional_release_RCsc
      | V_ctor ("Write_RISCV_conditional_strong_release", []) -> Write_RISCV_conditional_acquire_release
      | V_ctor ("Write_X86_locked", [])                       -> Write_X86_locked
      | _                                                     -> failwith "unknown write kind in interp2__wk_to_wk"
    in

    let interp2__bk_to_bk v =
      let open Value in
      let open Sail_impl_base in
      match v with
      | V_ctor ("Barrier_Sync", [])        -> Barrier_Sync
      | V_ctor ("Barrier_LwSync", [])      -> Barrier_LwSync
      | V_ctor ("Barrier_Eieio", [])       -> Barrier_Eieio
      | V_ctor ("Barrier_Isync", [])       -> Barrier_Isync
      | V_ctor ("Barrier_DMB", [])         -> Barrier_DMB
      | V_ctor ("Barrier_DMB_ST", [])      -> Barrier_DMB_ST
      | V_ctor ("Barrier_DMB_LD", [])      -> Barrier_DMB_LD
      | V_ctor ("Barrier_DSB", [])         -> Barrier_DSB
      | V_ctor ("Barrier_DSB_ST", [])      -> Barrier_DSB_ST
      | V_ctor ("Barrier_DSB_LD", [])      -> Barrier_DSB_LD
      | V_ctor ("Barrier_ISB", [])         -> Barrier_ISB
      | V_ctor ("Barrier_MIPS_SYNC", [])   -> Barrier_MIPS_SYNC
      | V_ctor ("Barrier_RISCV_rw_rw", []) -> Barrier_RISCV_rw_rw
      | V_ctor ("Barrier_RISCV_r_rw", [])  -> Barrier_RISCV_r_rw
      | V_ctor ("Barrier_RISCV_r_r", [])   -> Barrier_RISCV_r_r
      | V_ctor ("Barrier_RISCV_rw_w", [])  -> Barrier_RISCV_rw_w
      | V_ctor ("Barrier_RISCV_w_w", [])   -> Barrier_RISCV_w_w
      | V_ctor ("Barrier_RISCV_w_rw", [])  -> Barrier_RISCV_w_rw
      | V_ctor ("Barrier_RISCV_rw_r", [])  -> Barrier_RISCV_rw_r
      | V_ctor ("Barrier_RISCV_r_w", [])   -> Barrier_RISCV_r_w
      | V_ctor ("Barrier_RISCV_w_r", [])   -> Barrier_RISCV_w_r
      | V_ctor ("Barrier_RISCV_i", [])     -> Barrier_RISCV_i
      | V_ctor ("Barrier_x86_MFENCE", [])  -> Barrier_x86_MFENCE
      | _                                  -> failwith "unknown barrier kind in interp2__bk_to_bk"
    in

    let interp2__tk_to_tk v =
      let open Value in
      let open Sail_impl_base in
      match v with
      | V_ctor ("Transaction_start", [])  -> Transaction_start
      | V_ctor ("Transaction_commit", []) -> Transaction_commit
      | V_ctor ("Transaction_abort", [])  -> Transaction_abort
      | _                                 -> failwith "unknown transaction kind in interp2__tk_to_tk"
    in

    let interp2__ik_to_ik v =
      let open Value in
      let open Sail_impl_base in
      match v with
      | V_ctor ("IK_barrier", [v_bk])                 -> IK_barrier (interp2__bk_to_bk v_bk)
      | V_ctor ("IK_mem_read", [v_rk])                -> IK_mem_read (interp2__rk_to_rk v_rk)
      | V_ctor ("IK_mem_write", [v_wk])               -> IK_mem_write (interp2__wk_to_wk v_wk)
      | V_ctor ("IK_mem_rmw", [V_tuple [v_rk; v_wk]]) -> IK_mem_rmw (interp2__rk_to_rk v_rk, interp2__wk_to_wk v_wk)
      | V_ctor ("IK_branch", _)                       -> IK_branch
      | V_ctor ("IK_trans", [v_tk])                   -> IK_trans (interp2__tk_to_tk v_tk)
      | V_ctor ("IK_simple", _)                       -> IK_simple
      | _                                             -> failwith "unknown instruction kind in interp2__ik_to_ik"
    in

    let interp2__regfp_to_regfp reg_info v =
      let open Value in
      let open Sail_impl_base in
      match v with
      | V_ctor ("RFull", [V_string name]) ->
         let (start, len, dir, _) = reg_info name None in
         Reg (name, start, len, dir)
      | V_ctor ("RSlice", _) -> failwith "RSlice not yet implemented in interp2__regfp_to_regfp"
      | V_ctor ("RSliceBit", _) -> failwith "RSliceBit not yet implemented in interp2__regfp_to_regfp"
      | V_ctor ("RField", _) -> failwith "RField not yet implemented in interp2__regfp_to_regfp"
      | _ -> failwith "unknown regfp in interp2__regfp_to_regfp"
    in

    let interp2__list_to_list f v =
      let open Value in
      match v with
      | V_list vs -> List.map f vs
      | _ -> failwith "invalid interpreter value in interp2__list_to_list"
    in

    let bit_to_bitc = function
      | Value.V_bit B0 -> Sail_impl_base.Bitc_zero
      | Value.V_bit B1 -> Sail_impl_base.Bitc_one
      | _ -> failwith "invalid interpreter value in bit_to_bitc"
    in

    let bitc_to_bit = function
      | Sail_impl_base.Bitc_zero -> Value.V_bit B0
      | Sail_impl_base.Bitc_one -> Value.V_bit B1
    in

    let bit_to_bitl = function
      | Value.V_bit B0 -> Sail_impl_base.Bitl_zero
      | Value.V_bit B1 -> Sail_impl_base.Bitl_one
      | _ -> failwith "invalid interpreter value in bit_to_bit"
    in

    let rec bits_to_bytes = function
      | [] -> []
      | (a::b::c::d::e::f::g::h::rest) -> (Sail_impl_base.Byte [a;b;c;d;e;f;g;h])::(bits_to_bytes rest)
      | _ -> failwith "bits_to_bytes given list of bits not divisible by 8"
    in

    let rec bytes_to_bits = function
      | [] -> []
      | ((Sail_impl_base.Byte bs)::rest) -> List.map bitc_to_bit bs @ bytes_to_bits rest
    in

    let interp2__bitlist_to_address v =
      let open Value in
      let open Sail_lib in
      let open Sail_impl_base in
      match v with
      | V_vector bs ->
         address_of_byte_list (bits_to_bytes (List.map bit_to_bitc bs))
      | _ -> failwith "invalid interpreter value in interp2__bitlist_to_address"
    in

    let interp2__bitlist_to_memval v =
      let open Value in
      let open Sail_lib in
      let open Sail_impl_base in
      match v with
      | V_vector bs -> List.map byte_lifted_of_byte (bits_to_bytes (List.map bit_to_bitc bs))
      | _ -> failwith "invalid interpreter value in interp2__bitlist_to_memval"
    in

    let interp2__memval_to_bitlist mv =
      let open Value in
      let open Sail_lib in
      let open Sail_impl_base in
      match (maybe_all (List.map byte_of_memory_byte mv)) with
      | Some bytes -> V_vector (bytes_to_bits bytes)
      | None -> assert false
    in

    let interp2__bool_to_bit = function
      | false -> Value.V_bit (Sail_lib.B0)
      | true -> Value.V_bit (Sail_lib.B1)
    in

    let interp2__regval_to_value rv =
      let open Value in
      let open Sail_impl_base in
      let open InstructionSemantics in
      if rv.rv_start <> 0 || rv.rv_dir <> D_decreasing then failwith "invalid vector interp2__regval_to_interp" else
        match List.length (rv.rv_bits) with
        | 1 -> V_bool (bool_from_bitl (List.hd rv.rv_bits))
        | 2 -> Riscv_toFromInterp2.privilegeToInterpValue (Riscv.privLevel_of_bits (Lem.wordFromBitlist (List.map bool_from_bitl rv.rv_bits)))
        | 64 -> V_vector (List.map (fun b -> bool_from_bitl b |> interp2__bool_to_bit) rv.rv_bits)
        | 65 -> V_record (StringMap.add "Medeleg_chunk_0" (V_vector (List.map (fun b -> bool_from_bitl b |> interp2__bool_to_bit) (Util.drop 1 rv.rv_bits))) StringMap.empty)
        | 66 -> V_record (StringMap.add "Sedeleg_chunk_0" (V_vector (List.map (fun b -> bool_from_bitl b |> interp2__bool_to_bit) (Util.drop 2 rv.rv_bits))) StringMap.empty)
        | 67 -> V_record (StringMap.add "Misa_chunk_0" (V_vector (List.map (fun b -> bool_from_bitl b |> interp2__bool_to_bit) (Util.drop 3 rv.rv_bits))) StringMap.empty)
        | 68 -> V_record (StringMap.add "Mtvec_chunk_0" (V_vector (List.map (fun b -> bool_from_bitl b |> interp2__bool_to_bit) (Util.drop 4 rv.rv_bits))) StringMap.empty)
        | 69 -> V_record (StringMap.add "Minterrupts_chunk_0" (V_vector (List.map (fun b -> bool_from_bitl b |> interp2__bool_to_bit) (Util.drop 5 rv.rv_bits))) StringMap.empty)
        | 70 -> V_record (StringMap.add "Mstatus_chunk_0" (V_vector (List.map (fun b -> bool_from_bitl b |> interp2__bool_to_bit) (Util.drop 6 rv.rv_bits))) StringMap.empty)
        | 71 -> V_record (StringMap.add "Sstatus_chunk_0" (V_vector (List.map (fun b -> bool_from_bitl b |> interp2__bool_to_bit) (Util.drop 6 rv.rv_bits))) StringMap.empty)
        | 72 -> V_record (StringMap.add "Mcause_chunk_0" (V_vector (List.map (fun b -> bool_from_bitl b |> interp2__bool_to_bit) (Util.drop 8 rv.rv_bits))) StringMap.empty)
        | n -> failwith ("invalid vector length " ^ string_of_int n ^ " given to interp2__regval_to_value")
    in

    let interp2__bool_to_bitl = function
      | false -> Sail_impl_base.Bitl_zero
      | true -> Sail_impl_base.Bitl_one
    in

    let interp2__value_to_regval v =
      let open Value in
      let open Sail_impl_base in
      let open InstructionSemantics in
      match v with
      | V_bool b -> build_register_value [interp2__bool_to_bitl b] D_decreasing 1 0
      | V_ctor ("Machine", [])
        | V_ctor ("Supervisor", [])
        | V_ctor ("User", []) -> build_register_value (List.map bool_to_bitl (Lem.bitlistFromWord (Riscv.privLevel_to_bits (Riscv_toFromInterp2.privilegeFromInterpValue v)))) D_decreasing 2 1
      | V_record fs when List.length (StringMap.bindings fs) = 1 -> begin
          match StringMap.bindings fs with
          | [("Medeleg_chunk_0", V_vector bs)] -> build_register_value (bl0::List.map bit_to_bitl bs) D_decreasing 65 0
          | [("Sedeleg_chunk_0", V_vector bs)] -> build_register_value (bl0::bl0::List.map bit_to_bitl bs) D_decreasing 66 0
          | [("Misa_chunk_0", V_vector bs)] -> build_register_value (bl0::bl0::bl0::List.map bit_to_bitl bs) D_decreasing 67 0
          | [("Mtvec_chunk_0", V_vector bs)] -> build_register_value (bl0::bl0::bl0::bl0::List.map bit_to_bitl bs) D_decreasing 68 0
          | [("Minterrupts_chunk_0", V_vector bs)] -> build_register_value (bl0::bl0::bl0::bl0::bl0::List.map bit_to_bitl bs) D_decreasing 69 0
          | [("Mstatus_chunk_0", V_vector bs)] -> build_register_value (bl0::bl0::bl0::bl0::bl0::bl0::List.map bit_to_bitl bs) D_decreasing 70 0
          | [("Sstatus_chunk_0", V_vector bs)] -> build_register_value (bl0::bl0::bl0::bl0::bl0::bl0::bl0::List.map bit_to_bitl bs) D_decreasing 71 0
          | [("Mcause_chunk_0", V_vector bs)] -> build_register_value (bl0::bl0::bl0::bl0::bl0::bl0::bl0::bl0::List.map bit_to_bitl bs) D_decreasing 72 0
          | _ -> failwith ("invalid record type given to interp2__value_to_regval: " ^ Value.string_of_value v)
        end
      | V_vector bs
           when List.for_all (function V_bit _ -> true | _ -> false) bs
        -> build_register_value (List.map bit_to_bitl bs) D_decreasing (List.length bs) 0
      | _ -> failwith ("invalid value given to interp2__value_to_regval: " ^ Value.string_of_value v)
    in

    let instruction_to_interp2_instruction = function
      | RISCV_instr instr -> Riscv_toFromInterp2.astToInterpValue instr
      | Fetch_error -> failwith "fetch error"
      | _ -> failwith "not implemented yet"
    in

    let interp2_instruction_to_instruction instr = match ism with
      | RISCV_ism -> RISCV_instr (Riscv_toFromInterp2.astFromInterpValue instr)
      | _ -> failwith "not implemented yet"
    in

    let pp_state out stack ({ Interpreter.locals = locals }, _) =
      let sep = "\n-----------------------------------------------------\n" in
      let frames = (List.map (fun f -> Interpreter.stack_string f |> Lazy.force) stack |> List.rev) @ [Lazy.force out] in
      let frames_str = String.concat sep frames in
      let locals_str = String.concat ", " (List.map (fun (k, v) -> (Ast_util.string_of_id k) ^ "=" ^ (Value.string_of_value v)) (Ast_util.Bindings.bindings locals)) in
      Some ((fun () -> (frames_str, locals_str)), (fun _ -> []))
    in

    let rec frame_to_outcome frame =
      match frame with
      | Interpreter.Done (state, v) -> (Sail_impl_base.Done (), None)
      | Interpreter.Fail (out, state, _, stack, msg) -> (Sail_impl_base.Fail (Some msg), pp_state out stack state)
      | Interpreter.Step (out, state, _, stack) ->
         (Sail_impl_base.Internal ((None, None), frame_to_outcome (Interpreter.eval_frame frame)), pp_state out stack state)
      | Interpreter.Break frame ->
         frame_to_outcome frame
      | Interpreter.Effect_request (out, state, stack, eff) -> begin
          match eff with
          | Interpreter.Read_mem (rk, addr, len, cont) -> begin
              let rk = interp2__rk_to_rk rk in
              let addr = interp2__bitlist_to_address addr in
              let len = Value.coerce_int len in
              (Sail_impl_base.Read_mem ((rk, InstructionSemantics.address_lifted_of_address addr, Nat_big_num.to_int len), (fun mv -> frame_to_outcome (cont (interp2__memval_to_bitlist mv) state))), pp_state out stack state)
            end
          | Interpreter.Write_ea (wk, addr, len, cont) -> begin
              let wk = interp2__wk_to_wk wk in
              let addr = interp2__bitlist_to_address addr in
              let len = Value.coerce_int len in
              (Sail_impl_base.Write_ea ((wk, InstructionSemantics.address_lifted_of_address addr, Nat_big_num.to_int len), frame_to_outcome (cont () state)) , pp_state out stack state)
            end
          | Interpreter.Excl_res cont -> begin
              (Sail_impl_base.Excl_res (fun b -> frame_to_outcome (cont b state)), pp_state out stack state)
            end
          | Interpreter.Write_mem (wk, addr, len, v, cont) -> begin
              let v = interp2__bitlist_to_memval v in
              (Sail_impl_base.Write_memv (v, (fun b -> frame_to_outcome (cont b state))), pp_state out stack state)
            end
          | Interpreter.Barrier (bk, cont) -> begin
              let bk = interp2__bk_to_bk bk in
              (Sail_impl_base.Barrier (bk, frame_to_outcome (cont () state)), pp_state out stack state)
            end
          | Interpreter.Read_reg (name, cont) -> begin
              let open Sail_impl_base in
              let reg_name = InstructionSemantics.string_to_sail1_reg name in
              ((Sail_impl_base.Read_reg (reg_name, fun rv -> frame_to_outcome (cont (interp2__regval_to_value rv) state))), pp_state out stack state)
            end
          | Interpreter.Write_reg (name, v, cont) -> begin
              let open Sail_impl_base in
              let reg_name = InstructionSemantics.string_to_sail1_reg name in
              (Sail_impl_base.Write_reg ((reg_name, interp2__value_to_regval v), frame_to_outcome (cont () state)), pp_state out stack state)
            end
        end
    in

    let interp2__initial_outcome_s_of_instruction eager instruction : Sail_impl_base.outcome_S =
      let instruction = instruction_to_interp2_instruction instruction in
      let frame = Interpreter.execute_instruction state instruction in
      frame_to_outcome frame
    in

    let interp2__nia_to_nia v =
      let open Value in
      let open Sail_impl_base in
      match v with
      | V_ctor ("NIAFP_successor", _) -> NIA_successor
      | V_ctor ("NIAFP_concrete_address", [bv]) -> NIA_concrete_address (interp2__bitlist_to_address bv)
      | V_ctor ("NIAFP_indirect_address", _) -> NIA_indirect_address
      | _ -> failwith "unknown niafp in interp2__nia_to_nia"
    in

    let interp2__dia_to_dia reg_info v =
      let open Value in
      let open Sail_impl_base in
      match v with
      | V_ctor ("DIAFP_none", _) -> DIA_none
      | V_ctor ("DIAFP_concrete", [bv]) -> DIA_concrete_address (interp2__bitlist_to_address bv)
      | V_ctor ("DIAFP_reg", [reg]) -> DIA_register (interp2__regfp_to_regfp reg_info reg)
      | _ -> failwith "unknown diafp in interp2__dia_to_dia"
    in

    let interp2__analysis_to_analysis reg_info v =
      let open Value in
      match v with
      | V_tuple [v_ir; v_or; v_ar; v_nias; v_dia; v_ik] ->
         (interp2__list_to_list (interp2__regfp_to_regfp reg_info) v_ir,
          interp2__list_to_list (interp2__regfp_to_regfp reg_info) v_or,
          interp2__list_to_list (interp2__regfp_to_regfp reg_info) v_ar,
          interp2__list_to_list interp2__nia_to_nia v_nias,
          interp2__dia_to_dia reg_info v_dia,
          interp2__ik_to_ik v_ik)
      | _ -> failwith "invalid value for interp2__analysis_to_analysis"
    in

    let rec frame_handle_reg_reads rvs frame =
      match frame with
      | Interpreter.Done (state, v) -> v
      | Interpreter.Fail (_, _, _, _, msg) -> failwith ("frame_handle_reg_reads got Fail: " ^ msg)
      | Interpreter.Step (out, state, _, stack) ->
         frame_handle_reg_reads rvs (Interpreter.eval_frame frame)
      | Interpreter.Break frame ->
         frame_handle_reg_reads rvs (Interpreter.eval_frame frame)
      | Interpreter.Effect_request (_, state, _, eff) -> begin
          match eff with
          | Interpreter.Read_reg (name, cont) -> begin
              let open Sail_impl_base in
              let reg_name = InstructionSemantics.string_to_sail1_reg name in
              let rv = List.assoc reg_name rvs in
              frame_handle_reg_reads rvs (cont (interp2__regval_to_value rv) state)
            end
          | _ -> failwith "frame_handle_reg_reads: unhandled effect"
        end
    in


    let interp2__instruction_analysis outcome_s instruction analysis_function reg_info nia_reg environment =
      assert (analysis_function = "initial_analysis");
      interp2__analysis_to_analysis reg_info (frame_handle_reg_reads environment (Interpreter.analyse_instruction state (instruction_to_interp2_instruction instruction)))
    in

    let opcode_to_lits (Sail_impl_base.Opcode bytes) =
      let bit_to_lit = function
        | Sail_impl_base.Bitc_zero -> Ast.L_zero
        | Sail_impl_base.Bitc_one -> Ast.L_one
      in
      List.flatten (List.map (fun (Sail_impl_base.Byte byte) -> List.map bit_to_lit byte) bytes)
    in

    let interp2__decode_to_instruction (address : Sail_impl_base.address) (opcode : Sail_impl_base.opcode) =
      match Interpreter.decode_instruction state (opcode_to_lits opcode) with
      | Interpreter.Value_success v -> FDO_success (address, Some opcode, interp2_instruction_to_instruction v)
      | Interpreter.Value_error exn -> FDO_decode_error (Internal_decode_error (Printexc.to_string exn))
    in

    (fun eager ->
      { initial_outcome_s_of_instruction0 = interp2__initial_outcome_s_of_instruction eager;
        instruction_analysis0 = interp2__instruction_analysis;
        decode_to_instruction0 = interp2__decode_to_instruction;
      }
    )

  let instruction_semantics ism run_options : instruction_semantics =
    let endianness = Globals.get_endianness () in

    if run_options.RunOptions.interpreter then
      match ism with
      | RISCV_ism ->
         begin match ISADefs.interp2_isa_defs_thunk () with
         | Ast.Defs [], _ -> raise (Misc.Fatal "Empty ISA defs (use '-shallow_embedding true')")
         | defs, env ->
            let open Value in
            let primops = StringMap.add "Platform.dram_base" (fun _ -> Value.mk_vector (Sail_lib.bits_of_string "0000000000000000")) primops in
            let primops = StringMap.add "Platform.dram_size" (fun _ -> Value.mk_vector (Sail_lib.bits_of_string "00000000ffffffff")) primops in
            let primops = StringMap.add "Platform.rom_base" (fun _ -> Value.mk_vector (Sail_lib.bits_of_string "0000000000000000")) primops in
            let primops = StringMap.add "Platform.rom_size" (fun _ -> Value.mk_vector (Sail_lib.bits_of_string "0000000000000000")) primops in
            let primops = StringMap.add "Platform.clint_base" (fun _ -> Value.mk_vector (Sail_lib.bits_of_string "0000000000000000")) primops in
            let primops = StringMap.add "Platform.clint_size" (fun _ -> Value.mk_vector (Sail_lib.bits_of_string "0000000000000000")) primops in
            let primops = StringMap.add "Platform.enable_dirty_update" (fun _ -> V_bool false) primops in
            let primops = StringMap.add "Platform.enable_misaligned_access" (fun _ -> V_bool false) primops in
            let primops = StringMap.add "Platform.mtval_has_illegal_inst_bits" (fun _ -> V_bool false) primops in
            let primops = StringMap.add "Platform.insns_per_tick" (fun _ -> V_int (Nat_big_num.of_string "100")) primops in
            let primops = StringMap.add "Platform.load_reservation" (fun _ -> V_unit) primops in
            let primops = StringMap.add "Platform.match_reservation" (fun _ -> V_bool true) primops in
            let primops = StringMap.add "Platform.cancel_reservation" (fun _ -> V_unit) primops in
            let interp_state =
              (* try *)
              Interpreter.initial_state defs env primops
              (* with (Reporting.Fatal_error e) as exn ->
               *   begin
               *     prerr_endline "Error constructing new-interpreter initial state:";
               *     Reporting.print_error e;
               *     Pretty_print_sail.pp_defs stdout defs;
               *     raise exn
               *   end *)
            in
            initialise_interp2_semantics
              run_options.RunOptions.compare_analyses
              ism interp_state endianness
              false (* suppress internal *)
         end
      | _ ->
         begin match ISADefs.isa_defs_thunk () with
         | Interp_ast.Defs [] ->
            raise (Misc.Fatal "Empty ISA defs (use '-shallow_embedding true')")
         | _ ->
            let interp_context =
              let defs = ISADefs.isa_defs_thunk () in
              let (read_functions,read_taggeds,mem_writes,mem_eas,mem_vals,write_vals_tagged,barrier_functions,excl_res) =
                ISADefs.isa_memory_access in
              let externs = ISADefs.isa_externs in
              
              Interp_inter_imp.build_context
                !Globals.debug_sail_interp defs read_functions read_taggeds mem_writes mem_eas mem_vals write_vals_tagged barrier_functions excl_res externs
            in
            initialise_interp_semantics
              run_options.RunOptions.compare_analyses
              ism
              interp_context
              endianness
              false (* suppress internal *)
         end
    else initialise_shallow_embedding_semantics endianness ism
end

let make = function
  | PPCGEN_ism                  -> (module (Make (PPCGenISADefs)     (PPCGenTransSail))     : S)
  | AARCH64_ism AArch64HandSail -> (module (Make (AArch64ISADefs)    (AArch64HGenTransSail)): S)
  | AARCH64_ism AArch64GenSail  -> (module (Make (AArch64GenISADefs) (AArch64GenTransSail)) : S)
  | MIPS_ism                    -> (module (Make (MIPS64ISADefs)     (MIPSHGenTransSail))   : S)
  | RISCV_ism                   -> (module (Make (RISCVISADefs)      (RiscvTransSail))      : S)
  | X86_ism                     -> (module (Make (X86ISADefs)        (X86HGenTransSail))    : S)
