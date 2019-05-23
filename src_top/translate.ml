(*============================================================================================*)
(*                                                                                            *)
(*                rmem executable model                                                       *)
(*                =====================                                                       *)
(*                                                                                            *)
(*  This file is:                                                                             *)
(*                                                                                            *)
(*  Copyright Shaked Flur, University of Cambridge                                2015-2017   *)
(*  Copyright Susmit Sarkar, University of St Andrews                             2014-2015   *)
(*  Copyright Jon French, University of Cambridge                                      2018   *)
(*  Copyright Christopher Pulte, University of Cambridge                    2015-2016, 2018   *)
(*  Copyright Robert Norton-Wright, University of Cambridge                       2016-2017   *)
(*  Copyright Peter Sewell, University of Cambridge                                    2014   *)
(*  Copyright Luc Maranget, INRIA, Paris, France                                       2017   *)
(*  Copyright Kathy Gray, University of Cambridge (when this work was done)            2015   *)
(*                                                                                            *)
(*  All rights reserved.                                                                      *)
(*                                                                                            *)
(*  It is part of the rmem tool, distributed under the 2-clause BSD licence in                *)
(*  LICENCE.txt.                                                                              *)
(*                                                                                            *)
(*============================================================================================*)

(** Find a good place for these misc functions, which are technically
    arch-dependent *)

(* let address_base = 0x00001000 *)
(* moved this to globals, to make switchable *)
let address_placement base sz align =
  let nbase = if base mod align = 0 then base else align * ((base + align) / align )
  in
  (nbase, nbase + max sz !Globals.litmus_test_minimum_width) 
(* we make all variables aligned with
   !Globals.litmus_test_minimum_width, the default is 0x100 (256)
   bytes *)


let ptr_size = 8 (** in bytes *)
let int_size = 4 (** in bytes, default sized type, must match the default
                     in Litmus inorder to compare results *)
let instr_size = 4 (* assume all instructions are 4 bytes long *)
let reg_size = 8

let sizeof_typename =
  function
    | "uint8_t" | "int8_t" -> 1
    | "uint16_t" | "int16_t" -> 2
    | "uint32_t" | "int32_t" -> 4
    | "uint64_t" | "int64_t" -> 8
    | "uint128_t" | "int128_t" -> 16
    | "int" -> int_size
    | "tydef_ptr" -> ptr_size
          (* not any of above *)
    | s ->
        let len = String.length s in
        if len > 4 && String.sub s (len - 5) (len - 1) = "_ptr" then
          ptr_size
        else (* ?? *) int_size


let calc_size_alignment =
  function
    | MiscParser.TyDef -> int_size, int_size
    | MiscParser.TyDefPointer -> ptr_size, ptr_size
    | MiscParser.Pointer _ -> ptr_size, ptr_size
    | MiscParser.Ty s ->
        let sz = sizeof_typename s in (sz, sz)
    | MiscParser.TyArray (s, n) ->
        begin let sz = sizeof_typename s in (n * sz, n * sz) end
    | MiscParser.Atomic s -> assert false (* TODO: can this happen? *)



module type S =
  sig
    module A : Arch_litmus.S

    val translate_test : A.pseudo MiscParser.t -> Test.test
  end

module Make (A: Arch_litmus.S with type V.Scalar.t = string) (Trans : Isa_model.TransSail with type instruction = A.instruction)
    : S with module A = A =
  struct
    module A = A
    module C = MoreConstraints.Make
    module T = Test
    module TH = TestHash.Make (A)

    open T

    module AllocArch = struct
      include A
      type global = string
      type v = string Constant.t
      let maybevToV (c : MiscParser.maybev) : v = c
      let maybevToGlobal = A.vToName
    end

    module AllocRegs = SymbReg.Make (AllocArch)

    let translate_test t =
      let endianness = Globals.get_endianness () in
      let t_alloc = AllocRegs.allocate_regs t in
      let init_state = t_alloc.MiscParser.init in
      let filter = t_alloc.MiscParser.filter in
      let condition = t_alloc.MiscParser.condition in
      let map_constant f = (function
       | Constant.Concrete s -> Constant.Concrete (f s)
       | Constant.Symbolic (a,b) -> Constant.Symbolic (a, b)
       | Constant.Label (a,b) -> Constant.Label (a,b)) in
      let i_map_f =
          (fun (loc_reg, (run_type, v)) -> (loc_reg, (run_type, map_constant Nat_big_num.of_string v))) in
      let init_state = List.map i_map_f init_state in
      let filter =
         (match filter with
         | Some p -> Some (Misc_extra.map_constrgen_prop (map_constant Nat_big_num.of_string) p)
         | None   -> None) in
      let condition = Misc_extra.map_constrgen_constr (Misc_extra.map_constrgen_prop (map_constant Nat_big_num.of_string)) condition in

      (* Adapted from herdtools/litmus/compile.ml *)
      let typeof = function
        | Constant.Concrete _ -> MiscParser.Ty "int"
        | Constant.Symbolic _ -> MiscParser.Pointer "int"
        | Constant.Label    _ -> MiscParser.Pointer "int" (* TODO: BS: Come back to this ... *)
      in
      let empty_typemap = A.LocMap.empty in
      let conds_typemap =
        let type_atom a tmap = match a with
        | ConstrGen.LV (loc, v) ->
            A.LocMap.add loc (typeof v) tmap
        | ConstrGen.LL _ -> tmap
        in
        ConstrGen.fold_constr
          type_atom t_alloc.MiscParser.condition empty_typemap
      in
      let locs_typemap =
        List.fold_left
          (fun tmap (loc, t) -> A.LocMap.add loc t tmap)
          conds_typemap t_alloc.MiscParser.locations
      in
      let typemap_all =
        List.fold_left
          (fun tmap (loc,(t,v)) ->
            let tmapn =
              match (t, loc) with
              | (MiscParser.TyDef, A.Location_global _) -> A.LocMap.add loc (typeof v) tmap
              (* by default registers get their natural type, for Power and AArch64 that is uint64_t *)
              | (MiscParser.TyDef, A.Location_reg _)    -> A.LocMap.add loc (MiscParser.Ty "uint64_t") tmap
              | (MiscParser.TyDef, A.Location_deref _) -> failwith "TODO: Location_deref in typemap_all"
              | _ -> A.LocMap.add loc t tmap
            in
(* attempt to get locations mentioned only as values in init *)
(* NOTE: no locations mentioned only in code will get picked up *)
            match (t,v) with
            | MiscParser.TyDef, Constant.Symbolic (a,0) ->
                begin try
                  let _ = A.LocMap.find (A.Location_global a) tmapn in
                  tmapn
                with Not_found ->
                  A.LocMap.add (A.Location_global a) (MiscParser.Ty "int") tmapn
                end
            | MiscParser.TyDef, Constant.Symbolic (_,o) -> failwith "Offset with Symbolic not defined"
            | _ -> tmapn
          )
          locs_typemap init_state
      in
      let loc_types =
        A.LocMap.fold
          (fun l t k ->
            match l with
            | A.Location_global l -> (l, t) :: k
            | A.Location_deref _ -> failwith "TODO: Location_deref in loc_types"
            | A.Location_reg _ -> k)
          typemap_all [] in
      let _,addr_map =
        List.fold_left
          (fun (next_addr, k) (l,ty) ->
            let sz, align = calc_size_alignment ty in
            let addr, next = address_placement next_addr sz align in
            next, LocationMap.add l (addr, sz) k)
          (!Globals.litmus_test_base_address, LocationMap.empty)
          loc_types
          (* symbolic_values *) in
      let lookup_constant c =
        try
          fst (LocationMap.find c addr_map)
        with Not_found -> failwith ("No such address mapped " ^ c) in

      let translate_register_value t v =
        let i =
          match v with
          | Constant.Symbolic (c,0) -> Nat_big_num.of_int (lookup_constant c)
          | Constant.Symbolic (_,_) -> failwith "Offset not defined."
          | Constant.Concrete i -> i
          | Constant.Label _ -> failwith "TODO: handle Constant.Label in translate_register_value"
        in
        let sz, _ = calc_size_alignment t in
        (* currently we only support 64bit general-purpose-registers *)
        if sz > 8 then
          failwith "register type is too big";
        if Nat_big_num.greater_equal i (Nat_big_num.pow_int_positive 2 (sz * 8)) then
          failwith "register value is too big";
        (* TODO: check that the value is not too small; not critical as
        we should not use negatives anyway, though -1 works *)
        let reg_v = Sail_impl_base.register_value_of_integer 64 0 Sail_impl_base.D_increasing i in
        (* zero out bits in indices greater than the type MSB *)
        let zs =
          Sail_impl_base.bit_list_of_integer (64 - (8 * sz)) Nat_big_num.zero
          |> List.map Sail_impl_base.bit_lifted_of_bit
        in
        {reg_v with Sail_impl_base.rv_bits = zs @ (Lem_list.drop (64 - (8 * sz)) reg_v.Sail_impl_base.rv_bits)}
      in

      let translate_memory_value sz v =
        let i =
          (match v with
          | Constant.Symbolic (c,0) ->
              Nat_big_num.of_int (lookup_constant c)
          | Constant.Symbolic _ ->
              failwith "TODO: handle offset Symbolic in translate_memory_value"
          | Constant.Label _ ->
              failwith "TODO: handle Label in translate_memory_value"
          | Constant.Concrete i ->
              i) in
        Sail_impl_base.memory_value_of_integer endianness sz i
      in
      (** BIG NOTE: don't use the location types in i_mem, instead use
      the location to find the real type in loc_types *)
      let i_regs,i_mem =
        List.partition
          (function (A.Location_reg _,_) -> true | _ -> false) init_state in
      (* let (i_regs, i_mem) = (i_map_f i_regs, i_map_f i_mem) in *)
      let trans_reg r =
        let r_out = (A.reg_to_string r) in
        (* hack Luc/assembly style regnames to sail-like regnames *)
        let r_out' =
          begin match A.arch with
          | `PPC ->
              if r_out.[0] = 'r' then
                (* PPCGEN register name *)
                "GPR" ^ String.sub r_out 1 (String.length r_out - 1)
              else
                r_out
          | `AArch64 ->
              begin match AArch64HGenBase.parse_reg r_out with
              | Some (AArch64HGenBase.Ireg ireg) ->
                  let i = AArch64HGenBase.ireg_to_int ireg in
                  Printf.sprintf "R%u" i
              | Some _ -> failwith "unknown reg-type"
              | None -> r_out
              end
          | `MIPS ->
              begin match MIPSHGenBase.parse_reg r_out with
              | Some (MIPSHGenBase.IReg ireg) ->
                  let i = MIPSHGenBase.ireg_to_int ireg in
                  Printf.sprintf "GPR%02u" i
              | Some _ -> failwith "unknown reg-type"
              | None -> r_out
              end
          | `RISCV ->
              begin match RISCVHGenBase.parse_reg r_out with
              | Some (RISCVHGenBase.IReg ireg) ->
                  let i = RISCVHGenBase.ireg_to_int ireg in
                  Printf.sprintf "x%u" i
              | Some _ -> failwith "unknown reg-type"
              | None -> r_out
              end
          | `X86 ->
              begin match X86HGenBase.parse_reg r_out with
              | Some (X86HGenBase.IReg ireg) -> (X86HGenBase.pp_ireg ireg)
              | Some _ -> failwith "unknown reg-type"
              | None -> r_out
              end
          | _ -> failwith "unsupported architecture"
          end
        in
        r_out'
      in
      let init_regs =
        List.map
          (function
            | (A.Location_reg (i,r), (_, v)) ->
                let r_out = trans_reg r in
                let t =
                  try A.LocMap.find (A.Location_reg (i,r)) typemap_all with
                  | Not_found -> MiscParser.Ty "int"
                in
                let v_out = translate_register_value t v in
                ((i, r_out), v_out)
            | _ -> assert false) i_regs in
      let init_mem =
        List.map
          (function
            | A.Location_global l, (_, v) ->
                let ty =
                  try List.assoc l loc_types with
                  | Not_found -> assert false
                in
                let sz, _ = calc_size_alignment ty in
                (Sail_impl_base.address_of_integer (Nat_big_num.of_int (lookup_constant l)),
                 translate_memory_value sz v)
            | _ -> assert false)
          i_mem
      in
      let parsed_prog = t_alloc.MiscParser.prog in
      let rec compact l =
      (* remove A.Nop' sand place 'A.Instruction Trans.end_ins' as the last instruction *)
        match l with
        | [] -> [A.Instruction Trans.end_ins]
        | A.Nop :: t -> compact t
        | A.Label (s, p) :: t ->
            let trans = compact (p :: t) in
            begin
              match trans with
              | [] -> assert false (* [A.Label (s,A.Instruction Trans.end_ins)] *)
              | h :: t' -> (A.Label (s, h)) :: t'
            end
        | A.Instruction i :: t -> A.Instruction i :: (compact t)
        | A.Macro _ :: _ -> failwith "Cannot translate macros!"
        | A.Symbolic _ :: _ -> failwith "Cannot translate Symbolic!"
      in
      let prog_trans =
        List.map
          (fun (tid, prog) ->
            let prog = compact prog in
            let actualp =
              List.map
                (function
                | A.Instruction ins
                | A.Label (_, A.Instruction ins)
                    -> ins
                | _ -> assert false)
                prog
            in
            let labelmap =
              List.mapi
                (fun i -> function
                | A.Instruction _  -> None
                | A.Label (lbl, _) -> Some (lbl, i * instr_size)
                | _ -> assert false)
                prog
            in
            let labelmap = Misc.option_map Misc.identity labelmap in
            let unlabelizep =
              List.mapi
                (fun i ins ->
                  Trans.unlabelize_ins lookup_constant labelmap (i * instr_size) ins
                ) actualp in
            let sailp = List.map Trans.herdtools_ast_to_shallow_ast unlabelizep in
            (tid, sailp, labelmap)
          ) parsed_prog in
      let mem_addr_map = LocationMap.map (fun (x, sz) -> Sail_impl_base.address_of_integer (Nat_big_num.of_int x), sz) addr_map in
      let constr_trans,filter_trans =
        let module CG = ConstrGen in
        let trans_loc l =
          match l with
          | A.Location_global l ->
              let sz = snd (LocationMap.find l addr_map) in
              sz, C.Loc_mem (Int64.of_int (lookup_constant l))
          | A.Location_reg (tid, r) ->
              reg_size, C.Loc_reg (tid, trans_reg r)
          | A.Location_deref _ -> failwith "TODO: Location_deref in trans_loc"
        in
        let trans_atom a am =
          match a with
          | CG.LV (l,v) ->
              let sz, l' = trans_loc l in
              let v' = Nat_big_num.to_int64 (match Sail_impl_base.integer_of_memory_value endianness (translate_memory_value sz v) with None -> Warn.fatal "trans_atom given non-concrete memory value" | Some v' -> v') in
              CG.LV (l', v')
          | CG.LL (l1,l2) ->
              let _, l1' = trans_loc l1 in
              let _, l2' = trans_loc l2 in
              CG.LL (l1', l2')
        in
        let rec trans_prop p am =
          match p with
          | CG.Atom a -> CG.Atom (trans_atom a am)
          | CG.Not p -> CG.Not (trans_prop p am)
          | CG.And pl -> CG.And (List.map (fun p -> trans_prop p am) pl)
          | CG.Or pl -> CG.Or (List.map (fun p -> trans_prop p am) pl)
          | CG.Implies (p1,p2) -> CG.Implies (trans_prop p1 am, trans_prop p2 am)
        in
        let trans_constr constr am =
          match constr with
          | CG.ForallStates p -> CG.ForallStates (trans_prop p am)
          | CG.ExistsState p -> CG.ExistsState (trans_prop p am)
          | CG.NotExistsState p -> CG.NotExistsState (trans_prop p am)
        in
        trans_constr condition mem_addr_map,
        match filter with
        | None -> None
        | Some p -> Some (trans_prop p mem_addr_map) in

      let flocs_trans =
        let trans_loc (l,_) =
          match l with
          | A.Location_global l ->
              C.Loc_mem (Int64.of_int (lookup_constant l))
          | A.Location_reg (tid, r) ->
              C.Loc_reg (tid, trans_reg r)
          | A.Location_deref _ ->
              failwith "TODO: Location_deref in flocs_trans"
        in List.map trans_loc t_alloc.MiscParser.locations in
      { T.arch = A.arch;
        T.info = t.MiscParser.info;
        T.init_reg_state = init_regs;
        T.init_mem_state = init_mem;
        T.mem_addr_map = mem_addr_map;
        T.prog = prog_trans;
        T.constr = constr_trans;
        T.flocs = flocs_trans;
        T.filter = filter_trans;
      }
  end


