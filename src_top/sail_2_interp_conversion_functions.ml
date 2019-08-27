open Sail_1_2_convert

let bl0 = Sail_impl_base.Bitl_zero

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

let interp2__bk_to_bk v =
  let open Value in
  let open Sail_impl_base in
  match v with
  | V_ctor ("Barrier_Sync", _)        -> Barrier_Sync
  | V_ctor ("Barrier_LwSync", _)      -> Barrier_LwSync
  | V_ctor ("Barrier_Eieio", _)       -> Barrier_Eieio
  | V_ctor ("Barrier_Isync", _)       -> Barrier_Isync
  (* FIXME: | V_ctor ("Barrier_DMB", [])         -> Barrier_DMB*)
  (* FIXME: | V_ctor ("Barrier_DSB", [])         -> Barrier_DSB*)
  | V_ctor ("Barrier_ISB", _)         -> Barrier_ISB
  | V_ctor ("Barrier_MIPS_SYNC", _)   -> Barrier_MIPS_SYNC
  | V_ctor ("Barrier_RISCV_rw_rw", _) -> Barrier_RISCV_rw_rw
  | V_ctor ("Barrier_RISCV_r_rw", _)  -> Barrier_RISCV_r_rw
  | V_ctor ("Barrier_RISCV_r_r", _)   -> Barrier_RISCV_r_r
  | V_ctor ("Barrier_RISCV_rw_w", _)  -> Barrier_RISCV_rw_w
  | V_ctor ("Barrier_RISCV_w_w", _)   -> Barrier_RISCV_w_w
  | V_ctor ("Barrier_RISCV_w_rw", _)  -> Barrier_RISCV_w_rw
  | V_ctor ("Barrier_RISCV_rw_r", _)  -> Barrier_RISCV_rw_r
  | V_ctor ("Barrier_RISCV_r_w", _)   -> Barrier_RISCV_r_w
  | V_ctor ("Barrier_RISCV_w_r", _)   -> Barrier_RISCV_w_r
  | V_ctor ("Barrier_RISCV_i", _)     -> Barrier_RISCV_i
  | V_ctor ("Barrier_RISCV_tso", _)   -> Barrier_RISCV_tso
  | V_ctor ("Barrier_x86_MFENCE", _)  -> Barrier_x86_MFENCE
  | _                                  -> failwith "unknown barrier kind in interp2__bk_to_bk"

let interp2__tk_to_tk v =
  let open Value in
  let open Sail_impl_base in
  match v with
  | V_ctor ("Transaction_start", [])  -> Transaction_start
  | V_ctor ("Transaction_commit", []) -> Transaction_commit
  | V_ctor ("Transaction_abort", [])  -> Transaction_abort
  | _                                 -> failwith "unknown transaction kind in interp2__tk_to_tk"

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


let interp2__list_to_list f v =
  let open Value in
  match v with
  | V_list vs -> List.map f vs
  | _ -> failwith "invalid interpreter value in interp2__list_to_list"


let bit_to_bitc = function
  | Value.V_bit B0 -> Sail_impl_base.Bitc_zero
  | Value.V_bit B1 -> Sail_impl_base.Bitc_one
  | _ -> failwith "invalid interpreter value in bit_to_bitc"


let bitc_to_bit = function
  | Sail_impl_base.Bitc_zero -> Value.V_bit B0
  | Sail_impl_base.Bitc_one -> Value.V_bit B1


let bit_to_bitl = function
  | Value.V_bit B0 -> Sail_impl_base.Bitl_zero
  | Value.V_bit B1 -> Sail_impl_base.Bitl_one
  | _ -> failwith "invalid interpreter value in bit_to_bit"


let rec bits_to_bytes reverse = function
  | [] -> []
  | (a::b::c::d::e::f::g::h::rest) ->
     if reverse 
     then (bits_to_bytes reverse rest)@[Sail_impl_base.Byte [a;b;c;d;e;f;g;h]]
     else (Sail_impl_base.Byte [a;b;c;d;e;f;g;h])::(bits_to_bytes reverse rest)
  | _ -> failwith "bits_to_bytes given list of bits not divisible by 8"


let rec bytes_to_bits reverse = function
  | [] -> []
  | ((Sail_impl_base.Byte bs)::rest) ->
     if reverse
     then bytes_to_bits reverse rest @ List.map bitc_to_bit bs
     else List.map bitc_to_bit bs @ bytes_to_bits reverse rest


let interp2__bitlist_to_address v =
  let open Value in
  let open Sail_lib in
  let open Sail_impl_base in
  match v with
  | V_vector bs ->
     address_of_byte_list (bits_to_bytes false (List.map bit_to_bitc bs))
  | _ -> failwith "invalid interpreter value in interp2__bitlist_to_address"


let interp2__bitlist_to_memval v =
  let open Value in
  let open Sail_lib in
  let open Sail_impl_base in
  match v with
  | V_vector bs -> List.map byte_lifted_of_byte (bits_to_bytes true (List.map bit_to_bitc bs))
  | _ -> failwith "invalid interpreter value in interp2__bitlist_to_memval"


let interp2__memval_to_bitlist mv =
  let open Value in
  let open Sail_lib in
  let open Sail_impl_base in
  match (maybe_all (List.map byte_of_memory_byte mv)) with
  | Some bytes -> V_vector (bytes_to_bits true bytes)
  | None -> assert false


let interp2__bool_to_bit = function
  | false -> Value.V_bit (Sail_lib.B0)
  | true -> Value.V_bit (Sail_lib.B1)


let interp2__regval_to_value rv =
  let open Value in
  let open Sail_impl_base in
  let open InstructionSemantics in
  if rv.rv_start <> 0 || rv.rv_dir <> D_decreasing then failwith "invalid vector interp2__regval_to_interp" else
    match List.length (rv.rv_bits) with
    | 1 -> V_bool (bool_from_bitl (List.hd rv.rv_bits))
    | 2 -> Riscv_toFromInterp2.privilegeToInterpValue (continue_to_sail2_done "privilege" (Riscv.privLevel_of_bits (Lem.wordFromBitlist (List.map bool_from_bitl rv.rv_bits))))
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


let interp2__bool_to_bitl = function
  | false -> Sail_impl_base.Bitl_zero
  | true -> Sail_impl_base.Bitl_one


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





let pp_state out stack ({ Interpreter.locals = locals }, _) =
  let sep = "\n-----------------------------------------------------\n" in
  let frames = (List.map (fun f -> Interpreter.stack_string f |> Lazy.force) stack |> List.rev) @ [Lazy.force out] in
  let frames_str = String.concat sep frames in
  let locals_str = String.concat ", " (List.map (fun (k, v) -> (Ast_util.string_of_id k) ^ "=" ^ (Value.string_of_value v)) (Ast_util.Bindings.bindings locals)) in
  Some ((fun () -> (frames_str, locals_str)), (fun _ -> []))


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
          (Sail_impl_base.Read_mem ((rk, address_lifted_of_address addr, Nat_big_num.to_int len), (fun mv -> frame_to_outcome (cont (interp2__memval_to_bitlist mv) state))), pp_state out stack state)
        end
      | Interpreter.Write_ea (wk, addr, len, cont) -> begin
          let wk = interp2__wk_to_wk wk in
          let addr = interp2__bitlist_to_address addr in
          let len = Value.coerce_int len in
          (Sail_impl_base.Write_ea ((wk, address_lifted_of_address addr, Nat_big_num.to_int len), frame_to_outcome (cont () state)) , pp_state out stack state)
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
          let reg_name = string_to_sail1_reg name in
          ((Sail_impl_base.Read_reg (reg_name, fun rv -> frame_to_outcome (cont (interp2__regval_to_value rv) state))), pp_state out stack state)
        end
      | Interpreter.Write_reg (name, v, cont) -> begin
          let open Sail_impl_base in
          let reg_name = string_to_sail1_reg name in
          (Sail_impl_base.Write_reg ((reg_name, interp2__value_to_regval v), frame_to_outcome (cont () state)), pp_state out stack state)
        end
    end





let interp2__nia_to_nia v =
  let open Value in
  let open Sail_impl_base in
  match v with
  | V_ctor ("NIAFP_successor", _) -> NIA_successor
  | V_ctor ("NIAFP_concrete_address", [bv]) -> NIA_concrete_address (interp2__bitlist_to_address bv)
  | V_ctor ("NIAFP_indirect_address", _) -> NIA_indirect_address
  | _ -> failwith "unknown niafp in interp2__nia_to_nia"


let interp2__dia_to_dia reg_info v =
  let open Value in
  let open Sail_impl_base in
  match v with
  | V_ctor ("DIAFP_none", _) -> DIA_none
  | V_ctor ("DIAFP_concrete", [bv]) -> DIA_concrete_address (interp2__bitlist_to_address bv)
  | V_ctor ("DIAFP_reg", [reg]) -> DIA_register (interp2__regfp_to_regfp reg_info reg)
  | _ -> failwith "unknown diafp in interp2__dia_to_dia"


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




