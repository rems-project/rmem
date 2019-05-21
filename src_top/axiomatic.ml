
type ast = InstructionSemantics.instruction_ast
type address = Sail_impl_base.address
type instruction_semantics = InstructionSemantics.instruction_semantics
type fdo = InstructionSemantics.fetch_and_decode_outcome
type tid = Events.thread_id
type reg_base_name = Sail_impl_base.reg_base_name
type reg_value = Sail_impl_base.register_value
type write = Events.write


let read_file (name: string) : (Test.info * Test.test) * BasicTypes.isa_info =
  let (t,isa_info,_) = 
    Litmus_test_file_base.read_channel
      name
      (LexInChannel (open_in name))
      false
      (fun _ -> None)
  in
  (t,isa_info)





let run catfile litmusfile = 
  let open Isa_defs in

  let ((test_info, test), isa_info) = read_file litmusfile in
  
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
      
