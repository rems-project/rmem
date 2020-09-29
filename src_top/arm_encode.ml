open Sail_impl_base

let actually_SAIL_encode
        (instr : ArmV8_embed_types.ast)
        (endianness: Sail_impl_base.end_flag)
        : memory_value
    =
    let unsigned_shifted_from_offset sz imm =
        let sign = if imm > 0 then 1 else -1 in
        let imm = sign * (abs imm lsr 2) in
        if imm < 0 then
            imm + (1 lsl sz)
        else
            imm
    in
    (* let rec lsl1 a shift =
     *     match shift with
     *     | 0 -> a
     *     | n -> lsl1 ((a lsl 1) lor 1) (shift - 1)
     * in *)
    (* let bitslice up low n =
     *     (n lsr low) land (lsl1 0 (up - low))
     * in *)
    let v =
      (* begin match instr with
       * | AArch64_instr inst -> *)
           begin match instr with
            (* MOV Xn,#IMM *)
           | MoveWide (d, datasize, imm, pos, MoveWideOp_Z) ->
                let reg = Nat_big_num.to_int d in
                let imm16 = Nat_big_num.to_int (Sail_values.unsigned_big imm) in
                (3531603968 lor reg) lor (imm16 lsl 5)
            (* NOP *)
           | Hint (SystemHintOp_NOP) ->
                 3573751839
           (* (LDR|STR) (X|W)t, [Xn,Xm] *)
           | LoadRegister
              (n,t2,m2,_,memOp,_,_,_,_,_,regsize,datasize)
              (* (n, t, m, acctype, memop, signed, wback, postindex, extend_type, shift, regsize, datasize) -> *)
           ->
                let regt = Nat_big_num.to_int t2 in
                let regn = Nat_big_num.to_int n in
                let regm = Nat_big_num.to_int m2 in
                let sf = 3 in
                let load = (match memOp with
                    | MemOp_STORE -> 0
                    | MemOp_LOAD -> 1
                    | _ -> failwith "instruction encoder: unsupported memOp") in
                let sz = (if (Nat_big_num.to_int regsize) = 64 then 1 else 0) in
                (3089106944
                    lor regt
                    lor (regn lsl 5)
                    lor (sf lsl 13)
                    lor (load lsl 22)
                    lor (regm lsl 16)
                    lor (sz lsl 30))
           | LoadImmediate
              (n,t2,_,memOp,_,_,_,_,regsize,datasize)
           ->
                let regt = Nat_big_num.to_int t2 in
                let regn = Nat_big_num.to_int n in
                let load = (match memOp with
                    | MemOp_STORE -> 0
                    | MemOp_LOAD -> 1
                    | _ -> failwith "instruction encoder: unsupported memOp") in
                let sz = (if (Nat_big_num.to_int regsize) = 64 then 3 else 2) in
                (3087010816
                    lor regt
                    lor (regn lsl 5)
                    lor (load lsl 22)
                    lor (sz lsl 30))
           | CompareAndBranch
              (t2, datasize, iszero, offset)
           ->
                let regt = Nat_big_num.to_int t2 in
                let sf = (if (Nat_big_num.to_int datasize) = 64 then 1 else 0) in
                let imm = Nat_big_num.to_int (Sail_values.signed_big offset) in
                let imm = unsigned_shifted_from_offset 19 imm in
                let zero = (match iszero with
                    | B0 -> 1
                    | B1 -> 0
                    | _ -> failwith "unknown zero bit") in
                (872415232
                    lor regt
                    lor (imm lsl 5)
                    lor (zero lsl 24)
                    lor (sf lsl 31))
           | BranchImmediate
              (branch_type, offset)
           ->
                let op =
                    match branch_type with
                    | BranchType_JMP  -> 0
                    | BranchType_CALL -> 1
                    | _ -> failwith "Unknown branch type when assembling BranchImmediate"
                in
                let imm = Nat_big_num.to_int (Sail_values.signed_big offset) in
                (335544320
                    lor (unsigned_shifted_from_offset 26 imm)
                    lor (op lsl 31))
           | BranchConditional
              (offset, cond)
           ->
                let cond = Nat_big_num.to_int (Sail_values.unsigned_big cond) in
                let label = Nat_big_num.to_int (Sail_values.signed_big offset) in
                (1409286144
                    lor cond
                    lor ((unsigned_shifted_from_offset 19 label) lsl 5))
           | BranchRegister
              (n, branch_type)
           ->
                let op =
                    match branch_type with
                    | BranchType_JMP  -> 0
                    | BranchType_CALL -> 1
                    | BranchType_RET  -> 2
                    | _ -> failwith "Unknown branch type when assembling BranchRegister"
                in
                let regn = Nat_big_num.to_int n in
                (3592355840
                    lor (regn   lsl 5)
                    lor (op     lsl 21))
           | AddSubImmediate
             (d, n, datasize, sub_op, setflags, imm)
           ->
                let op = if (Sail_values.bitU_to_bool sub_op) then 1 else 0 in
                let sflags = if (Sail_values.bitU_to_bool setflags) then 1 else 0 in
                let regd = Nat_big_num.to_int d in
                let regn = Nat_big_num.to_int n in
                let sf = (if (Nat_big_num.to_int datasize) = 64 then 1 else 0) in
                let imm = Nat_big_num.to_int (Sail_values.unsigned_big imm) in
                (285212672
                    lor (sf     lsl 31)
                    lor (op     lsl 30)
                    lor (sflags lsl 29)
                    lor (imm    lsl 10)
                    lor (regn   lsl 5)
                    lor (regd   lsl 0))
           | AddSubExtendRegister
             (d, n, m, datasize, sub_op, setflags, extend_type, shift)
           ->
                let regd = Nat_big_num.to_int d in
                let regn = Nat_big_num.to_int n in
                let regm = Nat_big_num.to_int m in
                let imm3 = Nat_big_num.to_int shift in
                let op = if (Sail_values.bitU_to_bool sub_op) then 1 else 0 in
                let sflags = if (Sail_values.bitU_to_bool setflags) then 1 else 0 in
                let opt =
                    match extend_type with
                        | ExtendType_UXTB -> 0
                        | ExtendType_UXTH -> 1
                        | ExtendType_UXTW -> 2
                        | ExtendType_UXTX -> 3
                        | ExtendType_SXTB -> 4
                        | ExtendType_SXTH -> 5
                        | ExtendType_SXTW -> 6
                        | ExtendType_SXTX -> 7
                in
                let sf = (if (Nat_big_num.to_int datasize) = 64 then 1 else 0) in
                (186646528
                    lor (sf     lsl 31)
                    lor (op     lsl 30)
                    lor (sflags lsl 29)
                    lor (regm   lsl 16)
                    lor (opt    lsl 13)
                    lor (imm3   lsl 10)
                    lor (regn   lsl 5)
                    lor (regd   lsl 0))
           | LogicalShiftedRegister
              (d, n, m, datasize, setflags, op, shift_type, shift_amount, invert)
           ->
                let regd = Nat_big_num.to_int d in
                let regn = Nat_big_num.to_int n in
                let regm = Nat_big_num.to_int m in
                let imm6 = Nat_big_num.to_int shift_amount in
                let shift =
                  match shift_type with
                  | ShiftType_LSL -> 0
                  | ShiftType_LSR -> 1
                  | ShiftType_ASR -> 2
                  | ShiftType_ROR -> 3
                  in
                let bop =
                  match op with
                  | LogicalOp_AND -> 0
                  | LogicalOp_ORR -> 1
                  | LogicalOp_EOR -> 2
                  in
                let sf = (if (Nat_big_num.to_int datasize) = 64 then 1 else 0) in
                (167772160
                    lor regd
                    lor (regn   lsl 5)
                    lor (imm6   lsl 10)
                    lor (regm   lsl 16)
                    lor (shift  lsl 22)
                    lor (bop    lsl 29)
                    lor (sf     lsl 31))
           | Barrier
              (barrierOp,domain,types)
           ->
                let op = (match barrierOp with
                    | MemBarrierOp_DSB -> 0
                    | MemBarrierOp_DMB -> 1
                    | MemBarrierOp_ISB -> 2) in
                let dom = (match domain with
                    | MBReqDomain_OuterShareable -> 0
                    | MBReqDomain_Nonshareable -> 1
                    | MBReqDomain_InnerShareable -> 2
                    | MBReqDomain_FullSystem -> 3) in
                let ty = (match types with
                    | MBReqTypes_Reads -> 1
                    | MBReqTypes_Writes -> 2
                    | MBReqTypes_All -> 3) in
                (3573756063
                    lor (op lsl 5)
                    lor (ty lsl 8)
                    lor (dom lsl 10))
           | DataCache
              (t,dcOp)
           ->
                let regt = Nat_big_num.to_int t in
                let (op1,crm,op2) = (match dcOp with
                    | CVAU -> (3,11,1)
                    | _ -> failwith "unsupported DC operation") in
                (3574099968
                    lor regt
                    lor (op1 lsl 16)
                    lor (op2 lsl 5)
                    lor (crm lsl 8))
           | InstructionCache
              (t,icOp)
           ->
                let regt = Nat_big_num.to_int t in
                let (op1,crm,op2) = (match icOp with
                    | IVAU -> (3,5,1)
                    | _ -> failwith "unsupported IC operation") in
                (3574099968
                    lor regt
                    lor (op1 lsl 16)
                    lor (op2 lsl 5)
                    lor (crm lsl 8))
           | Address0
              (d,op,imm)
           ->
                let regd = Nat_big_num.to_int d in
                let vecimmlo =
                    Sail_values.slice_raw
                        imm
                        (Nat_big_num.of_int 0)
                        (Nat_big_num.of_int 1) in
                let vecimmhi =
                    Sail_values.slice_raw
                        imm
                        (Nat_big_num.of_int 2)
                        (Nat_big_num.of_int 21) in
                let immlo = Nat_big_num.to_int (Sail_values.unsigned_big vecimmlo) in
                let immhi = Nat_big_num.to_int (Sail_values.unsigned_big vecimmhi) in
                (268435456
                    lor regd
                    lor (immhi lsl 5)
                    lor (immlo lsl 29))

           | LogicalImmediate (d,n,datasize,setflags,op,imm) ->
              let datasize = Nat_big_num.to_int datasize in
              let setflags = Sail_values.bitU_to_bool setflags in
              let imm = Sail_values.unsigned_big imm in

              let sf = if datasize = 64 then 1 else 0 in
              let opc =
                match (op, setflags) with
                | (LogicalOp_AND, false) -> 0b00
                | (LogicalOp_ORR, false) -> 0b01
                | (LogicalOp_EOR, false) -> 0b10
                | (LogicalOp_AND, true)  -> 0b11
                | _ -> assert false
              in
              let (_N,imms,immr) =
                match AArch64HGenBase.encodeBitMasks datasize imm with
                | Some (_N,imms,immr) -> (_N,imms,immr)
                | None -> assert false
              in
              let _Rn = Nat_big_num.to_int n in
              let _Rd = Nat_big_num.to_int d in

              (sf       lsl 31) lor
              (opc      lsl 29) lor
              (0b100100 lsl 23) lor
              (_N       lsl 22) lor
              (immr     lsl 16) lor
              (imms     lsl 10) lor
              (_Rn      lsl 5)  lor
              _Rd

           | ConditionalSelect (d,n,m,datasize,condition,else_inv,else_inc) ->
              let datasize = Nat_big_num.to_int datasize in
              let else_inv = Sail_values.bitU_to_bool else_inv in
              let else_inc = Sail_values.bitU_to_bool else_inc in
              let condition = Sail_values.unsigned_big condition in

              let sf = if datasize = 64 then 1 else 0 in
              let op = if else_inv then 1 else 0 in
              let o2 = if else_inc then 1 else 0 in
              let _cond = Nat_big_num.to_int condition in
              let _Rm = Nat_big_num.to_int m in
              let _Rn = Nat_big_num.to_int n in
              let _Rd = Nat_big_num.to_int d in

              (sf         lsl 31) lor
              (op         lsl 30) lor
              (0          lsl 29) lor
              (0b11010100 lsl 21) lor
              (_Rm        lsl 16) lor
              (_cond      lsl 12) lor
              (0          lsl 11) lor
              (o2         lsl 10) lor
              (_Rn        lsl 5)  lor
              _Rd

           | LoadStoreAcqExc (n,t,t2,s,acctype,excl,pair,memop,elsize,regsize,datasize) ->
              let excl = Sail_values.bitU_to_bool excl in
              let pair = Sail_values.bitU_to_bool pair in
              let elsize = Nat_big_num.to_int elsize in

              let size =
                match elsize with
                | 64 -> 3
                | 32 -> 2
                | 16 -> 1
                | 8  -> 0
                | _  -> assert false
              in
              let o2 = if excl then 0 else 1 in
              let _L =
                match memop with
                | MemOp_LOAD -> 1
                | MemOp_STORE -> 0
                | _ -> assert false
              in
              let o1 = if pair then 1 else 0 in
              let o0 =
                match acctype with
                | AccType_ORDERED -> 1
                | AccType_ATOMIC -> 0
                | _ -> assert false
              in

              let _Rn = Nat_big_num.to_int n in
              let _Rt = Nat_big_num.to_int t in
              let _Rt2 = Nat_big_num.to_int t2 in
              let _Rs = Nat_big_num.to_int s in

              (size     lsl 30) lor
              (0b001000 lsl 24) lor
              (o2       lsl 23) lor
              (_L       lsl 22) lor
              (o1       lsl 21) lor
              (_Rs      lsl 16) lor
              (o0       lsl 15) lor
              (_Rt2     lsl 10) lor
              (_Rn      lsl 5)  lor
              _Rt

           | ImplementationDefinedStopFetching -> 0
           | ImplementationDefinedThreadStart  -> 0
           | inst ->
                let i' = (AArch64HGenTransSail.shallow_ast_to_herdtools_ast (inst)) in
                let p = AArch64HGenBase.pp_instruction (PPMode.Ascii) i' in
                failwith ("instruction encoder: instruction not supported: " ^ p)
           end
    (*   | _ -> failwith "instruction encoder: only ARM supported"
     * end *)
  in
      Sail_impl_base.memory_value_of_integer
        endianness
        4
        (Nat_big_num.of_int v)
