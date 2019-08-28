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
open Isa


open Trans
include Isa_defs

(********************************************************************)

module type Bool = sig
  val b : bool
end

module type ISA = sig
  type instruction_ast
  val isa: instruction_ast Isa.isa
  val pp_instruction_ast :
    Globals.ppmode ->
    ((Sail_impl_base.address * Sail_impl_base.size) * string) list ->
    instruction_ast ->
    (Sail_impl_base.address) ->
    string
  module TransSail: Trans.TransSail with type instruction_ast = instruction_ast
end  

module type S = sig
  type instruction_ast
  module ISADefs : ISADefs
  val isa : instruction_ast Isa.isa
  val pp_instruction_ast :
    Globals.ppmode ->
    ((Sail_impl_base.address * Sail_impl_base.size) * string) list ->
    instruction_ast ->
    (Sail_impl_base.address) ->
    string
end


module Make 
         (IsShallowEmbedding: Bool)
         (Isa: ISA)
         (ISADefs: ISADefs)
       : (S with type instruction_ast = Isa.instruction_ast) = struct

  type instruction_ast = Isa.instruction_ast

  module ISADefs = ISADefs

  let pp_instruction_ast = Isa.pp_instruction_ast

  let encode_instruction _ _ = failwith "fixme"

  let initialise_interp_semantics context endianness =
    
    let decode_error_to_decode_error opcode = function
      | Interp_interface.Unsupported_instruction_error instr ->
         Unsupported_instruction_error0
          (opcode,Isa.TransSail.interp_instruction_to_instruction instr)
      | Interp_interface.Not_an_instruction_error opcode ->
         Not_an_instruction_error0 opcode
      | Interp_interface.Internal_error string ->
         Internal_decode_error string
    in

    let interp__initial_outcome_s_of_instruction instruction : Sail_impl_base.outcome_S =
      let instruction = Isa.TransSail.instruction_to_interp_instruction instruction in
      let interp_mode = Interp_inter_imp.make_mode false false !Globals.debug_sail_interp in
      Interp_inter_imp.initial_outcome_s_of_instruction
        Printing_functions.pp_instruction_state context interp_mode instruction in


    let interp__instruction_analysis instruction reanalyse  reg_info environment =
      let analysis_function =
        if reanalyse then Isa.TransSail.instruction_to_reanalylsis_function_name instruction
        else "initial_analysis"
      in

      let instruction = Isa.TransSail.instruction_to_interp_instruction instruction in
      Interp_inter_imp.interp_handwritten_instruction_analysis 
        context endianness instruction analysis_function reg_info environment
    in


    let interp__decode_to_instruction (address : Sail_impl_base.address) (opcode : Sail_impl_base.opcode) =
      match Interp_inter_imp.decode_to_instruction context None opcode with
      | Interp_interface.IDE_instr instruction ->
         let instruction = Isa.TransSail.interp_instruction_to_instruction instruction in
         FDO_success (address,Some opcode,instruction)
      | Interp_interface.IDE_decode_error de ->
         FDO_decode_error (decode_error_to_decode_error opcode de)
    in

    { initial_outcome_of_instruction = interp__initial_outcome_s_of_instruction;
        instruction_analysis0 = interp__instruction_analysis;
        decode_to_instruction0 = interp__decode_to_instruction;
        encode_instruction = encode_instruction;
    }

  let initialise_interp2_semantics state endianness =
    let open Sail_2_interp_conversion_functions in



    let interp2__initial_outcome_s_of_instruction eager instruction : Sail_impl_base.outcome_S =
      let instruction = Isa.TransSail.instruction_to_interp2_instruction instruction in
      let frame = Interpreter.execute_instruction state instruction in
      frame_to_outcome frame
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
              let reg_name = Sail_1_2_convert.string_to_sail1_reg name in
              let rv = List.assoc reg_name rvs in
              frame_handle_reg_reads rvs (cont (interp2__regval_to_value rv) state)
            end
          | _ -> failwith "frame_handle_reg_reads: unhandled effect"
        end
    in


    
    let interp2__instruction_analysis instruction reanalyse reg_info environment =
      (* let analysis_function =
       *   if reanalyse then "initial_analysis"
       *   else Isa.TransSail.instruction_to_reanalylsis_function_name instruction
       * in *)

      interp2__analysis_to_analysis reg_info (frame_handle_reg_reads environment (Interpreter.analyse_instruction state (Isa.TransSail.instruction_to_interp2_instruction instruction)))
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
      | Interpreter.Value_success v -> FDO_success (address, Some opcode, Isa.TransSail.interp2_instruction_to_instruction v)
      | Interpreter.Value_error exn -> FDO_decode_error (Internal_decode_error (Printexc.to_string exn))
    in

    (fun eager ->
      { initial_outcome_of_instruction = interp2__initial_outcome_s_of_instruction eager;
        instruction_analysis0 = interp2__instruction_analysis;
        decode_to_instruction0 = interp2__decode_to_instruction;
        encode_instruction = encode_instruction;
      }
    )

  let interp_instruction_semantics () =


    let endianness = Globals.get_endianness () in

      match ISADefs.isa_model with
      | RISCV ->
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
            let primops = StringMap.add "Platform.enable_pmp" (fun _ -> V_bool false) primops in
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
              interp_state endianness
              false (* suppress internal *)
         end
      | _ ->
         begin match ISADefs.isa_defs_thunk () with
         | Interp_ast.Defs [] ->
            raise (Misc.Fatal "Empty ISA defs (use '-shallow_embedding true')")
         | defs ->
            let interp_context =
              (* let defs = ISADefs.isa_defs_thunk () in *)
              let (read_functions,read_taggeds,mem_writes,mem_eas,mem_vals,write_vals_tagged,barrier_functions,excl_res) =
                ISADefs.isa_memory_access in
              let externs = ISADefs.isa_externs in
              
              Interp_inter_imp.build_context
                !Globals.debug_sail_interp defs read_functions read_taggeds mem_writes mem_eas mem_vals write_vals_tagged barrier_functions excl_res externs
            in
            initialise_interp_semantics
              interp_context
              endianness
         end

  let isa =
    if IsShallowEmbedding.b
    then Isa.isa
    else { Isa.isa with instruction_semantics = interp_instruction_semantics () }

end

module PPCGEN_ISA : ISA with type instruction_ast = Power_embed_types.ast0 = struct
  type instruction_ast = Power_embed_types.ast0
  let isa = PowerIsa.ppc_isa
  let pp_instruction_ast = Pp.pp_ppcgen_instruction
  module TransSail = PPCGenTransSail
end

module AARCH64_HGEN_ISA : ISA with type instruction_ast = ArmV8_embed_types.ast1 = struct
  type instruction_ast = ArmV8_embed_types.ast1
  let isa = Aarch64Isa.aarch64hand_isa
  let pp_instruction_ast = Pp.pp_aarch64_instruction
  module TransSail = (AArch64HGenTransSail)
end

module AARCH64_GEN_ISA : ISA with type instruction_ast = ArmV8_embed_types.ast1 = struct
  type instruction_ast = ArmV8_embed_types.ast1
  let isa = Aarch64Isa.aarch64gen_isa
  let pp_instruction_ast = Pp.pp_aarch64_instruction
  module TransSail = (AArch64GenTransSail)
end

module MIPS_ISA : ISA with type instruction_ast = Mips_embed_types.ast2 = struct
  type instruction_ast = Mips_embed_types.ast2
  let isa = MipsIsa.mips_isa
  let pp_instruction_ast = Pp.pp_mips_instruction
  module TransSail = (MIPSHGenTransSail)
end

module RISCV_ISA : ISA with type instruction_ast = Riscv_types.ast = struct
  type instruction_ast = Riscv_types.ast
  let isa = RiscvIsa.riscv_isa
  let pp_instruction_ast = Pp.pp_riscv_instruction
  module TransSail = (RiscvTransSail)
end

module X86_ISA : ISA with type instruction_ast = X86_embed_types.ast3 = struct
  type instruction_ast = X86_embed_types.ast3
  let isa = X86Isa.x86_isa
  let pp_instruction_ast = Pp.pp_x86_instruction
  module TransSail = (X86HGenTransSail)
end

let make isa_model (runOptions : RunOptions.t) =
  let module IsShallow_embedding =
    (struct let b = not runOptions.RunOptions.interpreter end)
  in
  match isa_model with
  | PPC ->
     (module (Make (IsShallow_embedding) (PPCGEN_ISA) (PPCGenISADefs)) : S)
  | AARCH64 Hand ->
     (module (Make (IsShallow_embedding) (AARCH64_HGEN_ISA) (AArch64ISADefs) ): S)
  | AARCH64 Gen  ->
     (module (Make (IsShallow_embedding) (AARCH64_GEN_ISA ) (AArch64GenISADefs) ) : S)
  | MIPS ->
     (module (Make (IsShallow_embedding) (MIPS_ISA : ISA) (MIPS64ISADefs) ) : S)
  | RISCV ->
     (module (Make (IsShallow_embedding) (RISCV_ISA : ISA) (RISCVISADefs) ) : S)
  | X86 ->
     (module (Make (IsShallow_embedding) (X86_ISA : ISA) (X86ISADefs) ): S)
