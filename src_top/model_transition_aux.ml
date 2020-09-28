(*=======================================================================================================*)
(*                                                                                                       *)
(*                rmem executable model                                                                  *)
(*                =====================                                                                  *)
(*                                                                                                       *)
(*  This file is:                                                                                        *)
(*                                                                                                       *)
(*  Copyright Shaked Flur, University of Cambridge                                           2014-2018   *)
(*  Copyright Christopher Pulte, University of Cambridge                                     2015-2018   *)
(*  Copyright Peter Sewell, University of Cambridge                               2011-2014, 2016-2017   *)
(*  Copyright Susmit Sarkar, University of St Andrews                                        2011-2015   *)
(*  Copyright Francesco Zappa Nardelli, INRIA, Paris, France                                      2011   *)
(*  Copyright Ohad Kammar, University of Cambridge (when this work was done)                      2013   *)
(*  Copyright Linden Ralph, University of Cambridge (when this work was done)                     2017   *)
(*  Copyright Pankaj Pawan, IIT Kanpur and INRIA (when this work was done)                        2011   *)
(*  Copyright Jon French, University of Cambridge                                                 2017   *)
(*  Copyright Dominic Mulligan, University of Cambridge (when this work was done)                 2013   *)
(*                                                                                                       *)
(*  All rights reserved.                                                                                 *)
(*                                                                                                       *)
(*  It is part of the rmem tool, distributed under the 2-clause BSD licence in                           *)
(*  LICENCE.txt.                                                                                         *)
(*                                                                                                       *)
(*=======================================================================================================*)

open Events
open MachineDefTypes
open Model_aux

let cmp_tc tc1 tc2 =
    cmps  [ (fun () -> Stdlib.compare tc1.tc_tid tc2.tc_tid);
            (fun () -> Stdlib.compare tc1.tc_ioid tc2.tc_ioid);
          ]

let cmp_tl cmp tl1 tl2 =
    cmps  [ (fun () -> cmp_tc tl1.tl_cont tl2.tl_cont);
            (fun () -> cmp tl1.tl_label tl2.tl_label);
          ]

(* fuzzy_compare_transitions is used to determine if a transition was
enabled in the previous state so we can use the same UI number for it. *)
let fuzzy_compare_transitions trans1 trans2 =
  let cmp_ss_only_trans l1 l2 =
    match (l1, l2) with
    | (SS_PLDI11_partial_coherence_commit (write1, write1'),
       SS_PLDI11_partial_coherence_commit (write2, write2'))
        ->
        cmps [ (fun () -> Stdlib.compare write1  write2);
               (fun () -> Stdlib.compare write1' write2');
             ]
    | (SS_PLDI11_partial_coherence_commit _, _) -> 1
    | (_, SS_PLDI11_partial_coherence_commit _) -> -1

    | (SS_PLDI11_propagate_write_to_thread ((write1, _), tid1),
       SS_PLDI11_propagate_write_to_thread ((write2, _), tid2))
        ->
        cmps [ (fun () -> Stdlib.compare write1 write2);
               (fun () -> Stdlib.compare tid1 tid2);
             ]
    | (SS_PLDI11_propagate_write_to_thread _, _) -> 1
    | (_, SS_PLDI11_propagate_write_to_thread _) -> -1

    | (SS_PLDI11_write_reaches_coherence_point write1,
       SS_PLDI11_write_reaches_coherence_point write2)
        -> Stdlib.compare write1 write2
    | (SS_PLDI11_write_reaches_coherence_point _, _) -> 1
    | (_, SS_PLDI11_write_reaches_coherence_point _) -> -1

    | (SS_PLDI11_propagate_barrier_to_thread (barrier1, tid1),
       SS_PLDI11_propagate_barrier_to_thread (barrier2, tid2))
        ->
        cmps [ (fun () -> Stdlib.compare barrier1 barrier2);
               (fun () -> Stdlib.compare tid1 tid2);
             ]
    | (SS_PLDI11_propagate_barrier_to_thread _, _) -> 1
    | (_, SS_PLDI11_propagate_barrier_to_thread _) -> -1

    | (SS_POP_propagate_event_to_thread (event1, tid1),
       SS_POP_propagate_event_to_thread (event2, tid2))
        ->
        cmps [ (fun () -> flowing_eventCompare event1 event2);
               (fun () -> Stdlib.compare tid1 tid2);
             ]
    | (SS_POP_propagate_event_to_thread _, _) -> 1
    | (_, SS_POP_propagate_event_to_thread _) -> -1

    | (SS_POP_partially_satisfy_read (read1, sliced_write1),
       SS_POP_partially_satisfy_read (read2, sliced_write2))
        ->
        cmps [ (fun () -> read_requestCompare read1 read2);
               (fun () -> Stdlib.compare sliced_write1 sliced_write2) 
          ]
    | (SS_POP_partially_satisfy_read _, _) -> 1
    | (_, SS_POP_partially_satisfy_read _) -> -1

    | (SS_Flowing_flow_write_to_memory writes1,
       SS_Flowing_flow_write_to_memory writes2)
        -> Stdlib.compare writes1 writes2
    | (SS_Flowing_flow_write_to_memory _, _) -> 1
    | (_, SS_Flowing_flow_write_to_memory _) -> -1

    | (SS_Flowing_flow_barrier_to_memory barrier1,
       SS_Flowing_flow_barrier_to_memory barrier2)
        -> Stdlib.compare barrier1 barrier2
    | (SS_Flowing_flow_barrier_to_memory _, _) -> 1
    | (_, SS_Flowing_flow_barrier_to_memory _) -> -1

    | (SS_Flowing_flow_satisfied_read_to_memory read1,
       SS_Flowing_flow_satisfied_read_to_memory read2)
        -> read_requestCompare read1 read2
    | (SS_Flowing_flow_satisfied_read_to_memory _, _) -> 1
    | (_, SS_Flowing_flow_satisfied_read_to_memory _) -> -1

    | (SS_Flowing_reorder_events (event1, event1'),
       SS_Flowing_reorder_events (event2, event2'))
        ->
        cmps [ (fun () -> flowing_eventCompare event1 event2);
               (fun () -> flowing_eventCompare event1' event2');
             ]
    | (SS_Flowing_reorder_events _, _) -> 1
    | (_, SS_Flowing_reorder_events _) -> -1

    | (SS_Flowing_flow_to_segment event1,
       SS_Flowing_flow_to_segment event2)
        -> flowing_eventCompare event1 event2
    | (SS_Flowing_flow_to_segment _, _) -> 1
    | (_, SS_Flowing_flow_to_segment _) -> -1

    | (SS_Flowing_partially_satisfy_read (read1, sliced_write1),
       SS_Flowing_partially_satisfy_read (read2, sliced_write2))
        ->
        cmps [ (fun () -> read_requestCompare read1 read2);
               (* Fix this below *)
               (fun () -> Stdlib.compare sliced_write1 sliced_write2);
             ]
    | (SS_Flowing_partially_satisfy_read _, _) -> 1
    | (_, SS_Flowing_partially_satisfy_read _) -> -1

	| (SS_TSO_propagate_write_to_memory write1, SS_TSO_propagate_write_to_memory write2) ->
		Stdlib.compare write1 write2
	(* unused case (last):
	| (SS_TSO_propagate_write_to_memory _, _) -> 1
	| (_, SS_TSO_propagate_write_to_memory _) -> -1
	*)
    (* | (SS_Promising_stop_promising, SS_Promising_stop_promising) -> 0
     * | (SS_Promising_stop_promising, _) -> 1
     * | (_, SS_Promising_stop_promising) -> -1 *)
    | (SS_Flat_icache_update (tid, addr, mrs), SS_Flat_icache_update (tid2, addr2, mrs2)) -> 
            cmps [ (fun () -> Stdlib.compare tid tid2)
                 ; (fun () -> Sail_impl_base.addressCompare addr addr2)
                 ; (fun () -> Stdlib.compare mrs mrs2)
                 ]
    | (SS_Flat_icache_update (_, _, _), _) -> 1
    | (_, SS_Flat_icache_update (_, _, _)) -> -1
  in

  let cmkCompare cmk1 cmk2 =
      (match (cmk1, cmk2) with
       | (CM_DC, CM_DC) -> 0
       | (CM_IC, CM_DC) -> -1
       | (CM_DC, CM_IC) -> 1
       | (CM_IC, CM_IC) -> 0) in

  let cmrCompare cmr1 cmr2 = 
      cmps [ (fun () -> Sail_impl_base.addressCompare cmr1.cmr_addr cmr2.cmr_addr);
             (fun () -> Stdlib.compare cmr1.cmr_ioid cmr2.cmr_ioid);
             (fun () -> cmkCompare cmr1.cmr_cmk cmr2.cmr_cmk) ] in

  let cmp_ss_sync_trans l1 l2 =
    match (l1, l2) with
    | (SS_PLDI11_acknowledge_sync_barrier barrier1,
       SS_PLDI11_acknowledge_sync_barrier barrier2)
        -> Stdlib.compare barrier1 barrier2
    | (SS_PLDI11_acknowledge_sync_barrier _, _) -> 1
    | (_, SS_PLDI11_acknowledge_sync_barrier _) -> -1

    | (SS_POP_read_response (read1, _),
       SS_POP_read_response (read2, _))
        -> read_requestCompare read1 read2
    | (SS_POP_read_response _, _) -> 1
    | (_, SS_POP_read_response _) -> -1

    | (SS_Flowing_seg_read_response (read1, _),
       SS_Flowing_seg_read_response (read2, _))
        -> read_requestCompare read1 read2
    | (SS_Flowing_seg_read_response _, _) -> 1
    | (_, SS_Flowing_seg_read_response _) -> -1

    | (SS_Flowing_mem_read_response (read1, _),
       SS_Flowing_mem_read_response (read2, _))
        -> read_requestCompare read1 read2
    | (SS_Flat_thread_ic (cmr1, tid1),
       SS_Flat_thread_ic (cmr2, tid2))
        -> cmps [ (fun () -> Stdlib.compare tid1 tid2);
                  (fun () -> cmrCompare cmr1 cmr2) ]
    | (SS_Flat_thread_ic _, _) -> 1
    | (_, SS_Flat_thread_ic _) -> -1

    | (SS_Flat_ic_finish cmr1,
       SS_Flat_ic_finish cmr2)
        -> cmrCompare cmr1 cmr2
    | (SS_Flat_ic_finish _, _) -> 1
    | (_, SS_Flat_ic_finish _) -> -1
    (* unused case (last):
    | (SS_Flowing_mem_read_response _, _) -> 1
    | (_, SS_Flowing_mem_read_response _) -> -1
    *)
  in

  let cmp_t_only_trans l1 l2 =
    match (l1, l2) with
    | (T_init_fetch (addr1,_), T_init_fetch (addr2,_)) ->
        Sail_impl_base.addressCompare addr1 addr2
    | (T_init_fetch _, _) -> 1
    | (_, T_init_fetch _) -> -1

    | (T_decode (addr1, fetched1), T_decode (addr2, fetched2))  ->
        cmps [ (fun () -> Sail_impl_base.addressCompare addr1 addr2)
             ; (fun () -> Stdlib.compare fetched1 fetched2)
             ]
    | (T_decode _, _)  -> 1
    | (_, T_decode _)  -> -1

    | (T_internal_outcome, T_internal_outcome) -> 0
    | (T_internal_outcome, _) -> 1
    | (_, T_internal_outcome) -> -1

    | (T_pending_memory_read_request, T_pending_memory_read_request) -> 0
    | (T_pending_memory_read_request, _) -> 1
    | (_, T_pending_memory_read_request) -> -1

    | (T_pseudoreg_read (reg_name1, _), T_pseudoreg_read (reg_name2, _))
        -> Stdlib.compare reg_name1 reg_name2
    | (T_pseudoreg_read _, _) -> 1
    | (_, T_pseudoreg_read _) -> -1

    | (T_pseudoreg_write (reg_name1, _), T_pseudoreg_write (reg_name2, _))
        -> Stdlib.compare reg_name1 reg_name2
    | (T_pseudoreg_write _, _) -> 1
    | (_, T_pseudoreg_write _) -> -1

    | (T_footprint_outcome, T_footprint_outcome) -> 0
    | (T_footprint_outcome, _) -> 1
    | (_, T_footprint_outcome) -> -1

    | (T_actually_satisfy _, T_actually_satisfy _) -> 0
    | (T_actually_satisfy _, _) -> 1
    | (_, T_actually_satisfy _) -> -1

    | (T_register_read (reg_name1, _, _), T_register_read (reg_name2, _, _))
        -> Stdlib.compare reg_name1 reg_name2
    | (T_register_read _, _) -> 1
    | (_, T_register_read _) -> -1

    | (T_register_write (reg_name1, _), T_register_write (reg_name2, _))
        -> Stdlib.compare reg_name1 reg_name2
    | (T_register_write _, _) -> 1
    | (_, T_register_write _) -> -1

    | (T_mem_forward_write (read_request1, _),
       T_mem_forward_write (read_request2, _))
        -> read_requestCompare read_request1 read_request2
    | (T_mem_forward_write _, _) -> 1
    | (_, T_mem_forward_write _) -> -1

    | (T_mem_write_footprint write1, T_mem_write_footprint write2)
        -> Stdlib.compare write1 write2
    | (T_mem_write_footprint _, _) -> 1
    | (_, T_mem_write_footprint _) -> -1

    | (T_mem_potential_write write1, T_mem_potential_write write2)
        -> Stdlib.compare write1 write2
    | (T_mem_potential_write _, _) -> 1
    | (_, T_mem_potential_write _) -> -1

    | (T_finish (addr,_), T_finish (addr',_))
        -> Sail_impl_base.addressCompare addr addr'
    | (T_finish _, _) -> 1
    | (_, T_finish _) -> -1

    | (T_finish_load_of_rmw, T_finish_load_of_rmw) -> 0
    | (T_finish_load_of_rmw, _) -> 1
    | (_, T_finish_load_of_rmw) -> -1

    | (T_exception _, T_exception _) -> 0
    | (T_exception _, _) -> 1
    | (_, T_exception _) -> -1

    | (T_commit_store, T_commit_store) -> 0
    | (T_commit_store, _) -> 1
    | (_, T_commit_store) -> -1

    | (T_complete_store, T_complete_store) -> 0
    | (T_complete_store, _) -> 1
    | (_, T_complete_store) -> -1

    | (T_commit_barrier barrier1, T_commit_barrier barrier2)
        -> Stdlib.compare barrier1 barrier1
    | (T_commit_barrier _, _) -> 1
    | (_, T_commit_barrier _) -> -1

    | (T_successful_store_excl, T_successful_store_excl) -> 0
    | (T_successful_store_excl, _) -> 1
    | (_, T_successful_store_excl) -> -1

    | (T_potential_store_cond, T_potential_store_cond) -> 0
    | (T_potential_store_cond, _) -> 1
    | (_, T_potential_store_cond) -> -1

    | (T_failed_store_excl, T_failed_store_excl) -> 0
    | (T_failed_store_excl, _) -> 1
    | (_, T_failed_store_excl) -> -1

    | (T_prev_excl_result _, T_prev_excl_result _) -> 0
    | (T_prev_excl_result _, _) -> 1
    | (_, T_prev_excl_result _) -> -1

    | (T_POP_subsumed_write write1, T_POP_subsumed_write write2)
        -> Stdlib.compare write1 write2
    | (T_POP_subsumed_write _, _) -> 1
    | (_, T_POP_subsumed_write _) -> -1

    | (T_RISCV_atomic_begin, T_RISCV_atomic_begin) -> 0
    | (T_RISCV_atomic_begin, _) -> 1
    | (_, T_RISCV_atomic_begin) -> -1

    | (T_RISCV_atomic_end, T_RISCV_atomic_end) -> 0
    (* unused case (last):
    | (T_RISCV_atomic_end, _) -> 1
    | (_, T_RISCV_atomic_end) -> -1
    *)
  in

  let cmp_t_sync_trans l1 l2 =
    match (l1, l2) with
    | (T_mem_read_request tl1, T_mem_read_request tl2) ->
        let cmp (read1, _, _, _) (read2, _, _, _) = read_requestCompare read1 read2 in
        cmp_tl cmp tl1 tl2
    | (T_mem_read_request _, _) -> 1
    | (_, T_mem_read_request _) -> -1

    | (T_propagate_write tl1, T_propagate_write tl2) ->
        let cmp (write1, _, _) (write2, _, _) = Stdlib.compare write1 write2 in
        cmp_tl cmp tl1 tl2
    | (T_propagate_write _, _) -> 1
    | (_, T_propagate_write _) -> -1

    | (T_propagate_barrier tl1, T_propagate_barrier tl2) ->
        let cmp (b1: barrier) (b2: barrier) = Stdlib.compare b1 b2 in
        cmp_tl cmp tl1 tl2
    | (T_propagate_barrier _, _) -> 1
    | (_, T_propagate_barrier _) -> -1

    | (T_PLDI11_mem_satisfy_read tl1, T_PLDI11_mem_satisfy_read tl2) ->
        let cmp (read1, _) (read2, _) = read_requestCompare read1 read2 in
        cmp_tl cmp tl1 tl2
    | (T_PLDI11_mem_satisfy_read _, _) -> 1
    | (_, T_PLDI11_mem_satisfy_read _) -> -1

    | (T_Flat_mem_satisfy_read tl1, T_Flat_mem_satisfy_read tl2) ->
        let cmp (read1, _, _, _) (read2, _, _, _) = read_requestCompare read1 read2 in
        cmp_tl cmp tl1 tl2
    | (T_Flat_mem_satisfy_read _, _) -> 1
    | (_, T_Flat_mem_satisfy_read _) -> -1

    | (T_Flat_try_commit_store_cond tl1, T_Flat_try_commit_store_cond tl2) ->
        let cmp (write1, _) (write2, _) = Stdlib.compare write1 write2 in
        cmp_tl cmp tl1 tl2
    | (T_Flat_try_commit_store_cond _, _) -> 1
    | (_, T_Flat_try_commit_store_cond _) -> -1

    | (T_TSO_mem_satisfy_read tl1, T_TSO_mem_satisfy_read tl2) ->
        let cmp read1 read2 = read_requestCompare read1 read2 in
        cmp_tl cmp tl1 tl2
    | (T_TSO_mem_satisfy_read _, _) -> 1
    | (_, T_TSO_mem_satisfy_read _) -> -1

    | (T_try_store_excl tl1, T_try_store_excl tl2) ->
        let cmp (read1, _, _) (read2, _, _) = read_requestCompare read1 read2 in
        cmp_tl cmp tl1 tl2
    | (T_try_store_excl _, _) -> 1
    | (_, T_try_store_excl _) -> -1


    | (T_fetch tl1, T_fetch tl2) ->
        let cmp f1 f2 = Sail_impl_base.addressCompare f1.fr_addr f2.fr_addr in
        cmp_tl cmp tl1 tl2
    | (T_fetch _, _) -> 1
    | (_, T_fetch _) -> -1

    | (T_propagate_cache_maintenance tl1, T_propagate_cache_maintenance tl2) ->
        let cmp {cmr_cmk=cmk1; cmr_addr=addr1} {cmr_cmk=cmk2; cmr_addr=addr2} =
            cmps [ (fun () -> Sail_impl_base.addressCompare addr1 addr2)
                 ; (fun () ->
                     (match (cmk1, cmk2) with
                      | (CM_DC, CM_DC) -> 0
                      | (CM_IC, CM_IC) -> 0
                      | (CM_DC, _) -> -1
                      | (_, _) -> 1
                     ))
                 ]
        in
        cmp_tl cmp tl1 tl2
    (* unused case (last): *)
    (* | (T_propagate_cache_maintenance _, _) -> 1
     * | (_, T_propagate_cache_maintenance _) -> -1 *)

    (* | (T_Promising_mem_satisfy_read tl1, T_Promising_mem_satisfy_read tl2) -> -1
     *     (\* let cmp (read1,_) (read2,_) = read_requestCompare read1 read2 in
     *      * cmp_tl cmp tl1 tl2 *\)
     * | (T_Promising_mem_satisfy_read _, _) -> 1
     * | (_, T_Promising_mem_satisfy_read _) -> -1
     * 
     * | (T_Promising_mem_satisfy_read_nonshared tl1, T_Promising_mem_satisfy_read_nonshared tl2) -> -1
     *     (\* let cmp (read1,_) (read2,_) = read_requestCompare read1 read2 in
     *      * cmp_tl cmp tl1 tl2 *\)
     * | (T_Promising_mem_satisfy_read_nonshared _, _) -> 1
     * | (_, T_Promising_mem_satisfy_read_nonshared _) -> -1
     * 
     * | (T_Promising_propagate_write tl1, T_Promising_propagate_write tl2) -> -1
     *     (\* let cmp (w1, _) (w2, _) = Stdlib.compare w1 w2 in
     *      * cmp_tl cmp tl1 tl2 *\)
     * | (T_Promising_propagate_write _, _) -> 1
     * | (_, T_Promising_propagate_write _) -> -1
     * 
     * | (T_Promising_fulfil_promise tl1, T_Promising_fulfil_promise tl2) -> -1
     *     (\* let cmp w1 w2 = Stdlib.compare w1 w2 in
     *      * cmp_tl cmp tl1 tl2 *\)
     * | (T_Promising_fulfil_promise _, _) -> 1
     * | (_, T_Promising_fulfil_promise _) -> -1
     * 
     * | (T_Promising_propagate_write_nonshared tl1, T_Promising_propagate_write_nonshared tl2) -> -1
     *     (\* let cmp w1 w2 = Stdlib.compare w1 w2 in
     *      * cmp_tl cmp tl1 tl2 *\)
     * | (T_Promising_propagate_write_nonshared _, _) -> 1
     * | (_, T_Promising_propagate_write_nonshared _) -> -1 *)

    (* unused case (last):
    | (T_fetch _, _) -> 1
    | (_, T_fetch _) -> -1
    *)
  in

  begin match (trans1, trans2) with
  | (SS_trans sys_ss_trans1, SS_trans sys_ss_trans2) ->
    begin match (sys_ss_trans1, sys_ss_trans2) with
    | (SS_only (l1, _), SS_only (l2, _)) -> cmp_ss_only_trans l1 l2
    | (SS_only _, _) -> 1
    | (_, SS_only _) -> -1

    | (SS_sync (l1, _, _), SS_sync (l2, _, _)) -> cmp_ss_sync_trans l1 l2
    (* unused case (last):
    | (SS_sync _, _) -> 1
    | (_, SS_sync _) -> -1
    *)
    end
  | (SS_trans _, _) -> 1
  | (_, SS_trans _) -> -1

  | (T_trans sys_thread_trans1, T_trans sys_thread_trans2) ->
    begin match (sys_thread_trans1, sys_thread_trans2) with
    | (T_only tl1, T_only tl2) -> cmp_tl cmp_t_only_trans tl1 tl2
    | (T_only _, _) -> 1
    | (_, T_only _) -> -1

    | (T_sync (l1, _), T_sync (l2, _)) -> cmp_t_sync_trans l1 l2
    | (T_sync _, _) -> 1
    | (_, T_sync _) -> -1

    | (T_thread_start tl1, T_thread_start tl2) ->
        let cmp (r_address1, _) (r_address2, _) = Stdlib.compare r_address1 r_address2 in
        cmp_tl cmp tl1 tl2
    (* unused case (last):
    | (T_thread_start _, _) -> 1
    | (_, T_thread_start _) -> -1
    *)
    end
  (* unused case (last):
  | (T_trans _, _) -> 1
  | (_, T_trans _) -> -1
  *)
    
  end
