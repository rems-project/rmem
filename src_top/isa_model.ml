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

module PPCGenISADefs : ISADefs = struct
  let name = "PPC"
  let reg_data = IsaInfoPPCGen.ppcgen_ism.BasicTypes.register_data_info
  let isa_defs_thunk = memo_unit (fun () -> Screen.unmarshal_defs "PPCGen")
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
    let isa_defs_thunk = memo_unit (fun () -> Screen.unmarshal_defs "RISCV")
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

    let interp__decode_to_instruction address opcode =
      begin match Interp_inter_imp.decode_to_instruction context None opcode with
      | Interp_interface.IDE_instr instruction ->
        let instruction = interp_instruction_to_instruction instruction in
        FDO_success (address,Some opcode,instruction)
      | Interp_interface.IDE_decode_error de ->
        FDO_decode_error (decode_error_to_decode_error opcode de)
      end in

    (fun eager ->
      { initial_outcome_s_of_instruction0 = interp__initial_outcome_s_of_instruction eager;
        instruction_analysis0 = interp__instruction_analysis;
        decode_to_instruction0 = interp__decode_to_instruction;
      }
    )

  let instruction_semantics ism run_options : instruction_semantics =
    let endianness = Globals.get_endianness () in

    if run_options.RunOptions.interpreter then
      match ISADefs.isa_defs_thunk () with
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
    else initialise_shallow_embedding_semantics endianness ism
end

let make = function
  | PPCGEN_ism                  -> (module (Make (PPCGenISADefs)     (PPCGenTransSail))     : S)
  | AARCH64_ism AArch64HandSail -> (module (Make (AArch64ISADefs)    (AArch64HGenTransSail)): S)
  | AARCH64_ism AArch64GenSail  -> (module (Make (AArch64GenISADefs) (AArch64GenTransSail)) : S)
  | MIPS_ism                    -> (module (Make (MIPS64ISADefs)     (MIPSHGenTransSail))   : S)
  | RISCV_ism                   -> (module (Make (RISCVISADefs)      (RiscvTransSail))      : S)
  | X86_ism                     -> (module (Make (X86ISADefs)        (X86HGenTransSail))    : S)
