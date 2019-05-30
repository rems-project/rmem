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


module Lexer = ModelLexer.Make(struct let debug = false end)


type ast = InstructionSemantics.instruction_ast
type address = Sail_impl_base.address
type instruction_semantics = InstructionSemantics.instruction_semantics
type fdo = InstructionSemantics.fetch_and_decode_outcome
type tid = Events.thread_id
type reg_base_name = Sail_impl_base.reg_base_name
type reg_value = Sail_impl_base.register_value
type write = Events.write


let read_litmus_file (litmus_file_name: string) :
      (Test.info * Test.test) * BasicTypes.isa_info =
  let ic = open_in litmus_file_name in
  let (t,isa_info,_) = 
    Litmus_test_file_base.read_channel
      litmus_file_name
      (LexInChannel ic)
      false
      (fun _ -> None)
  in
  let () = close_in ic in
  (t,isa_info)


let read_cat_file (cat_file_name: string) = 
  let ic = open_in cat_file_name in
  let lexbuf = Lexing.from_channel ic in
  try
    let model = ModelParser.main Lexer.token lexbuf in
    let () = close_in ic in
    model
  with Parsing.Parse_error ->
    begin
      close_in ic;
      let curr = lexbuf.Lexing.lex_curr_p in
      let line = curr.Lexing.pos_lnum in
      let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
      let tok = Lexing.lexeme lexbuf in
      failwith ("error " ^ string_of_int line ^ ":" ^ string_of_int cnum ^ " `" ^ tok ^ "'")
  end    

let run catfile litmusfile = 
  let open Isa_defs in

  let ((test_info, test), isa_info) = read_litmus_file litmusfile in
  let cat_model = read_cat_file catfile in

  let endianness = 
    match isa_info.ism with
    | PPCGEN_ism    -> Sail_impl_base.E_big_endian
    | AARCH64_ism _ -> Sail_impl_base.E_little_endian
    | MIPS_ism      -> Sail_impl_base.E_big_endian
    | RISCV_ism     -> Sail_impl_base.E_little_endian
    | X86_ism       -> Sail_impl_base.E_little_endian
  in


  let (module IsaDefs : ISADefs) =
    match isa_info.ism with
    | PPCGEN_ism    -> (module PPCGenISADefs : ISADefs)
    | AARCH64_ism _ -> (module AArch64ISADefs : ISADefs)
    | MIPS_ism      -> (module MIPS64ISADefs : ISADefs)
    | RISCV_ism     -> (module RISCVISADefs : ISADefs)
    | X86_ism       -> (module X86ISADefs : ISADefs)
  in

  let aarch64gen = false in

  let (
      (thread_isa_info'      : BasicTypes.isa_info),
      (prog_map              : (Nat_big_num.num,ast) Pmap.map),
      (_prog                 : instruction_semantics -> address -> fdo),
      (return_addresses      : (tid * address) list),
      (tids                  : tid list),
      (init_reg_data         : tid -> (reg_base_name * reg_value) list),
      (init_reg_value        : tid -> reg_base_name -> reg_value),
      (initial_fetch_address : tid -> address option),
      (init_write_events     : write list)
    )
    =
    Litmus_test_file_base.initial_state_record_base
      endianness aarch64gen test (module IsaDefs) isa_info
  in

  let deep_embedding_prog_map = 
    Pmap.map instruction_to_interp_instruction prog_map in

  ()




let check_file_exists s = 
  if Sys.file_exists s then ()
  else failwith (Printf.sprintf "%s: No such file" s)


let process_files catfile litmusfile = 
  check_file_exists catfile;
  check_file_exists litmusfile;
  run catfile litmusfile


let main () = 
  if (Array.length Sys.argv <> 3) then
    let prog = Sys.executable_name in
    failwith (Printf.sprintf "Usage: %s <catfile> <litmusfile>" prog)
  else
    let catfile = Sys.argv.(1) in
    let litmusfile = Sys.argv.(2) in
    process_files catfile litmusfile
      
