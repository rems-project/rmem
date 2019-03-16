(*===================================================================================================*)
(*                                                                                                   *)
(*                rmem executable model                                                              *)
(*                =====================                                                              *)
(*                                                                                                   *)
(*  This file is:                                                                                    *)
(*                                                                                                   *)
(*  Copyright Peter Sewell, University of Cambridge                          2011-2012, 2014, 2016   *)
(*  Copyright Shaked Flur, University of Cambridge                                 2014-2015, 2017   *)
(*  Copyright Susmit Sarkar, University of St Andrews                                    2011-2014   *)
(*  Copyright Francesco Zappa Nardelli, INRIA, Paris, France                                  2011   *)
(*  Copyright Ohad Kammar, University of Cambridge (when this work was done)                  2013   *)
(*  Copyright Pankaj Pawan, IIT Kanpur and INRIA (when this work was done)                    2011   *)
(*                                                                                                   *)
(*  All rights reserved.                                                                             *)
(*                                                                                                   *)
(*  It is part of the rmem tool, distributed under the 2-clause BSD licence in                       *)
(*  LICENCE.txt.                                                                                     *)
(*                                                                                                   *)
(*===================================================================================================*)

open Utils
open FreshIds

(* type of information passed down in ppmode through pp functions for pp of addresses and values using dwarf debug info *)
(* later this should probably involve caching; for now we recompute everything from the system_state *)
type dwarf_dynamic =
  { get_evaluation_context: Events.ioid ->
        (Nat_big_num.num * DwarfTypes.dwarf_evaluation_context) option;
    pp_all_location_data_at_instruction: Events.thread_id ->
        Events.ioid -> string;
  }

(* (\* transitions *\) *)
(* type trans = *)
(*    | Commit_instruction of thread_id * instruction_instance  *)
(*    | Write_announce_to_thread of write * thread_id *)
(*    | Barrier_propagate_to_thread of barrier * thread_id  *)
(*    | Read_from_storage_subsystem of thread_id * instruction_instance * write *)
(*    | Write_forward_to_read of thread_id * instruction_instance * write * instruction_instance *)
(*    | Acknowledge_sync of barrier *)
(*    | Partial_coherence_commit of write * write *)
(*    | Write_reaching_coherence_point of write *)
(*    | Partial_evaluate of thread_id * instruction_instance *)
(*    | Register_read_prev of thread_id * instruction_instance * reg * instruction_instance *)
(*    | Register_read_initial of thread_id * instruction_instance * reg *)
(*    | Choice (\* Not a real transition, for marking choice points in traces *\) *)




(* ************ UI TYPES ************ *)

(*type ui_trans = int * trans *)

type ppcolour = 
  | UI_removed
  | UI_old
  | UI_new


(*
type ui_trans = int * trans



type ui_storage_subsystem_state =  {
    ui_threads : thread_id list;
    ui_writes_seen_old : write list;
    ui_writes_seen_new : write list;
    ui_coherence_old : (write*write) list;
    ui_coherence_new : (write*write) list;
    ui_writes_past_coherence_point_old : write list ;
    ui_writes_past_coherence_point_new : write list ;
    ui_events_propagated_to : (thread_id * (tracked_event list * tracked_event list)) list;
    ui_unacknowledged_sync_requests_removed : barrier list;
    ui_unacknowledged_sync_requests_old : barrier list; 
    ui_unacknowledged_sync_requests_new : barrier list; 
(*    ui_reservations : (thread_id * write option * ppcolour) list;*)
    ui_ss_transitions : ui_trans list;
    (* invariant: those are always one of:
       | Write_announce_to_thread 
       | Barrier_propagate_to_thread 
       | Acknowledge_sync 
       | Write_reaching_coherence_point 
       | Partial_coherence_commit
    *)
  }

*)

(*
type ui_sem_state = {
    (* ui_remaining_removed : action list; *)
    ui_remaining_now : action list;
    ui_all_changed : bool;
    ui_val_soln_old : solution;
    ui_val_soln_new : solution
  }
*)



(*
type ui_instruction_instance_kind =
  | UI_committed_old
  | UI_committed_new
  | UI_in_flight of ui_trans list  
  (* invariant: those are always one of:
   Commit_instruction, Read_from_storage_subsystem, Write_forward_to_read *)

type writes_read_from =
  | WRF_all_new of write list
  | WRF_some_new of write list * write list 
  | WRF_none

type read_responses =
  | RR_all_new of read_response list
  | RR_some_new of read_response list * read_response list 
  | RR_none

type writes_bound = 
  | WUB_all_new of write list
  | WUB_some_new of write list * write list 
  | WUB_none
*)

(* 
type ui_instruction_instance = {
    ui_ioid : ioid; 
    ui_kind : ui_instruction_instance_kind;
    ui_behaviour : ui_sem_state;  
    ui_regs_in : reg list;      
    ui_regs_out : reg list;     
    ui_read_responses : read_responses;
    ui_writes_read_from : writes_read_from; 
    ui_program_loc : address;     
    ui_instruction : instruction;
    ui_prev_old : (ioid * reaches_by) option ;
    ui_prev_new : (ioid * reaches_by) option ;
    ui_writes_upper_bound :  writes_bound ;
    ui_writes_lower_bound :  writes_bound ;
 }
*)
(*
type ui_thread_state = {
    ui_thread : thread_id ;                               
    (* FIXME: ui_initial_register_state : (reg * value) list ;            *)
    (* FIXME: ui_instructions : ui_instruction_instance list; *)
    (*ui_writes_received : write list;                      *)
    ui_unacknowledged_syncs_removed : barrier list ;
    ui_unacknowledged_syncs_old : barrier list ;
    ui_unacknowledged_syncs_new : barrier list;
    ui_outstanding_read_requests : read_request list;
  }

type ui_system_state = {
    ui_model : model_params;
    ui_storage_subsystem : ui_storage_subsystem_state;
    ui_thread_states : ui_thread_state list;
	  (* id_state : id_state;*)
  }
*)

(* *********** UI TYPES FOR FLOWING-THINGS VARIANT ******* *)

(* (for the moment, without old/new colouring data) *)

(*TODO NO FLOWING YET open MachineDefFlowingStorageSubsystem *)

(* type ui_flowing_storage_subsystem_state = *)
(*      { *)
(*      uif_threads : thread_id list; *)
(*      uif_memory : write list; *)
(*      uif_topology : tree_topology; *)
(*      uif_buffers : (segment -> (ui_trans list*flowing_event) list); *)
(*      uif_reordered : (flowing_event * flowing_event) list; *)
(* } *)

(* type ui_flowing_system_state = { *)
(*     uif_model : model_params; *)
(*     uif_storage_subsystem : ui_flowing_storage_subsystem_state; *)
(*     uif_thread_states : ui_thread_state list; *)
(*     (\* id_state : id_state;*\) *)
(*   } *)

(* (\* *********** UI TYPES FOR POP VARIANT ******* *\) *)

(* open MachineDefPOPStorageSubsystem *)

(* type ui_pop_storage_subsystem_state = { *)
(*     ui_pop_threads : thread_id list; *)
(*     ui_pop_events_seen_old : flowing_event Pset.set; *)
(*     ui_pop_events_seen_new : flowing_event Pset.set; *)
(*     ui_pop_coherence_old : (flowing_event * flowing_event) Pset.set; *)
(*     ui_pop_coherence_new : (flowing_event * flowing_event) Pset.set; *)
(*     ui_pop_coherence_point_old : flowing_event Pset.set; *)
(*     ui_pop_coherence_point_new : flowing_event Pset.set; *)
(*     ui_pop_events_propagated_to : (thread_id * (flowing_event Pset.set * flowing_event Pset.set)) list; *)
(*     ui_pop_ss_transitions : ui_trans list; *)
(* } *)

(* type ui_pop_system_state = { *)
(*     ui_pop_model : model_params; *)
(*     ui_pop_storage_subsystem : ui_pop_storage_subsystem_state; *)
(*     ui_pop_thread_states : ui_thread_state list; *)
(*   } *)

(* *********** END OF UI TYPES ********** *)

type filetype = 
  | Binary_file
  | Litmus_file

