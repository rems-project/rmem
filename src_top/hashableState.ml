(*===============================================================================*)
(*                                                                               *)
(*                rmem executable model                                          *)
(*                =====================                                          *)
(*                                                                               *)
(*  This file is:                                                                *)
(*                                                                               *)
(*  Copyright Shaked Flur, University of Cambridge       2015, 2017              *)
(*  Copyright Susmit Sarkar, University of St Andrews          2014              *)
(*  Copyright Peter Sewell, University of Cambridge            2015              *)
(*  Copyright Christopher Pulte, University of Cambridge       2015              *)
(*                                                                               *)
(*  All rights reserved.                                                         *)
(*                                                                               *)
(*  It is part of the rmem tool, distributed under the 2-clause BSD licence in   *)
(*  LICENCE.txt.                                                                 *)
(*                                                                               *)
(*===============================================================================*)

type thread_id = MachineDefTypes.thread_id

type address = int64 (* Specialised *)
type size = MachineDefTypes.size
type footprint = address * size

type memory_value = int64 (* Specialised *)

type w_eiid = MachineDefTypes.w_eiid
type r_eiid = MachineDefTypes.r_eiid
type b_eiid = MachineDefTypes.b_eiid

type ioid = MachineDefFreshIds.ioid

type write_kind = Interp_interface.write_kind
type barrier_kind = Interp_interface.barrier_kind

type write = { 
    (* w_thread : ignore *) 
    (* w_ioid : ignore *)
    weiid: w_eiid ; 
    w_addr:footprint; 
    w_value:memory_value ; 
    w_write_kind: write_kind ;
  } 

type barrier = { 
    (* b_thread : ignore *)
    (* b_ioid : ignore *)
    beiid: b_eiid; 
    b_barrier_kind:barrier_kind 
  } 

type tracked_event = 
  | SWrite of (write * MachineDefTypes.slices)
  | SBarrier of barrier

type storage_subsystem_state = {
    (* threads : assumed constant, ignore *)
    writes_seen : write list;
    coherence : (write * write) list;
    new_coherence_basis : footprint list;
    new_coherence : (footprint * (write*write) list) list;
    writes_past_coherence_point : write list;
    events_propagated_to : (thread_id * tracked_event list) list;
    unacknowledged_sync_requests : barrier list;
  }

type micro_op_state =
(* | MOS_plain : does not arise, all internal transitions done eagerly *)
  | MOS_pending_mem_read
(*  | MOS_potential_mem_write : now done eagerly *)
  | MOS_pending_commit_barrier
  | MOS_pending_commit_write

type instruction_instance = {
    instance_ioid : ioid; 
    program_loc : address;  (* another source of uniqueness, whoever programs with loops? *)
    (* program_opcode : ignore *)
    (* instruction : ignore *)
    (* all static info ignored 
       instruction_kind: instruction_kind;
       initial_micro_op_state: micro_op_state;
       regs_in: reg_name Pset.set; 
       regs_out: reg_name Pset.set; 
       ioids_feeding_address: ioid Pset.set; 
       nias: nia Pset.set; *)

    (* most dynamic info ignored, too much? 
       reg_writes: (reg_name * value0) list; 
       writes_read_from: write Pset.set; 
       committed_barriers: barrier list; 
       committed_mem_writes: write list; 
     *)
  committed : bool; 
  finished : bool; 
  micro_op_state : micro_op_state;  
  }

type instruction_tree =
  | T of (instruction_instance * instruction_tree) list

type thread_state = {
    (* thread : redundant info *)
    (* register_data : assumed constant, ignore *)
    (* initial_register_state : assumed constant, ignore *)
    (* initial_fetch_address : assumed constant, ignore *)
    old_instructions: instruction_instance list; 
    instruction_tree: instruction_tree;
    (* unacknowledged_syncs : redundant, ignore *)
    (* outstanding_read_requests : flowing only, ignore *)
    (* branch_prediction_targets: optimistically hoping ignorable *)
    (* inst_to_read_order : flowing only, ignore *)
    (* next_read_order : flowing only, ignore *)
  }

type system_state = {
    (* program_memory: assumed constant, ignore *)
    (* initial_writes: assumed constant, ignore *)
    (* interp_context: should not change? ignore for now *)
    thread_states: (thread_id * thread_state) list;
    storage_subsystem: storage_subsystem_state;
    (* idstate: states differing only in idstate "should" be same *)
    (* model: assumed constant, ignore *)
  }

let conv_addr a = Nat_big_num.to_int64 (Interp_interface.integer_of_address a)

let conv_footprint (addr, size) = (conv_addr addr, size)

let conv_memory_value mv =
  let endianness =
    if (Globals.get_our_runopts ()).big_endian then
      Interp_interface.E_big_endian
    else
      Interp_interface.E_little_endian
  in
  Nat_big_num.to_int64 (match Interp_interface.integer_of_memory_value endianness mv with Some bi -> bi)

let conv_write w = 
  let module MT = MachineDefTypes in
  { weiid = w.MT.weiid;
    w_addr = conv_footprint w.MT.w_addr;
    w_value = conv_memory_value w.MT.w_value;
    w_write_kind = w.MT.w_write_kind;
  }

let conv_barrier b = 
  let module MT = MachineDefTypes in
  { beiid = b.MT.beiid;
    b_barrier_kind = b.MT.b_barrier_kind;
  }

let conv_tracked_event e =
  let module MT = MachineDefTypes in
  match e with
  | MT.SWrite (w,sls) -> SWrite ((conv_write w),sls)
  | MT.SBarrier b -> SBarrier (conv_barrier b)

let conv_storage_subsystem_state ss =
  let module MT = MachineDefTypes in
  { writes_seen = List.map conv_write (Pset.elements ss.MT.writes_seen);
    coherence = 
    List.map (fun (w1, w2) -> conv_write w1, conv_write w2) 
      (Pset.elements ss.MT.coherence);
    new_coherence_basis = List.map conv_footprint 
      (Pset.elements ss.MT.new_coherence_basis);
    new_coherence = 
      List.map 
      (fun (f, r) -> 
        conv_footprint f,
        List.map (fun (w1, w2) -> conv_write w1, conv_write w2) 
          (Pset.elements r))
      (Pmap.bindings_list ss.MT.new_coherence);
    writes_past_coherence_point =
      List.map conv_write (Pset.elements ss.MT.writes_past_coherence_point);
    events_propagated_to =
      List.map
      (fun tid ->
        tid, 
        List.map (fun e -> conv_tracked_event e) 
          (ss.MT.events_propagated_to tid))
      (Pset.elements ss.MT.threads);
    unacknowledged_sync_requests =
      List.map conv_barrier (Pset.elements ss.MT.unacknowledged_sync_requests);
  }

let conv_mos mos = 
  let module MT = MachineDefTypes in
  match mos with
  | MT.MOS_pending_mem_read _ -> MOS_pending_mem_read
  | MT.MOS_plain is ->
      failwith "unimplemented"
  | MT.MOS_potential_mem_write _ -> assert false

let conv_instruction_instance is = 
  let module MT = MachineDefTypes in
  { instance_ioid = is.MT.instance_ioid;
    program_loc = conv_addr is.MT.program_loc;
    committed = is.MT.committed;
    finished = is.MT.finished;
    micro_op_state = conv_mos is.MT.micro_op_state;
  }
