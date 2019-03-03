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

open MachineDefTypes

(* return the return value of the first continuation in the list that
is not 0. *)
let rec cmps : (unit -> int) list -> int = function
  | c :: cs ->
      begin match c () with
      | 0 -> cmps cs
      | n -> n
      end
  | [] -> 0

let cmp_tc tc1 tc2 =
    cmps  [ (fun () -> Pervasives.compare tc1.tc_tid tc2.tc_tid);
            (fun () -> Pervasives.compare tc1.tc_ioid tc2.tc_ioid);
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
        cmps [ (fun () -> Pervasives.compare write1  write2);
               (fun () -> Pervasives.compare write1' write2');
             ]
    | (SS_PLDI11_partial_coherence_commit _, _) -> 1
    | (_, SS_PLDI11_partial_coherence_commit _) -> -1

    | (SS_PLDI11_propagate_write_to_thread ((write1, _), tid1),
       SS_PLDI11_propagate_write_to_thread ((write2, _), tid2))
        ->
        cmps [ (fun () -> Pervasives.compare write1 write2);
               (fun () -> Pervasives.compare tid1 tid2);
             ]
    | (SS_PLDI11_propagate_write_to_thread _, _) -> 1
    | (_, SS_PLDI11_propagate_write_to_thread _) -> -1

    | (SS_PLDI11_write_reaches_coherence_point write1,
       SS_PLDI11_write_reaches_coherence_point write2)
        -> Pervasives.compare write1 write2
    | (SS_PLDI11_write_reaches_coherence_point _, _) -> 1
    | (_, SS_PLDI11_write_reaches_coherence_point _) -> -1

    | (SS_PLDI11_propagate_barrier_to_thread (barrier1, tid1),
       SS_PLDI11_propagate_barrier_to_thread (barrier2, tid2))
        ->
        cmps [ (fun () -> Pervasives.compare barrier1 barrier2);
               (fun () -> Pervasives.compare tid1 tid2);
             ]
    | (SS_PLDI11_propagate_barrier_to_thread _, _) -> 1
    | (_, SS_PLDI11_propagate_barrier_to_thread _) -> -1

    | (SS_POP_propagate_event_to_thread (event1, tid1),
       SS_POP_propagate_event_to_thread (event2, tid2))
        ->
        cmps [ (fun () -> flowing_eventCompare event1 event2);
               (fun () -> Pervasives.compare tid1 tid2);
             ]
    | (SS_POP_propagate_event_to_thread _, _) -> 1
    | (_, SS_POP_propagate_event_to_thread _) -> -1

    | (SS_POP_partially_satisfy_read (read1, sliced_write1),
       SS_POP_partially_satisfy_read (read2, sliced_write2))
        ->
        cmps [ (fun () -> read_requestCompare read1 read2);
               (fun () -> Pervasives.compare sliced_write1 sliced_write2) 
          ]
    | (SS_POP_partially_satisfy_read _, _) -> 1
    | (_, SS_POP_partially_satisfy_read _) -> -1

    | (SS_NOP_constrain_order (write1, write1'),
       SS_NOP_constrain_order (write2, write2'))
        ->
        cmps [ (fun () -> Pervasives.compare write1 write2);
               (fun () -> Pervasives.compare write1' write2');
             ]
    | (SS_NOP_constrain_order _, _) -> 1
    | (_, SS_NOP_constrain_order _) -> -1

    | (SS_NOP_propagate_everything, SS_NOP_propagate_everything) -> 0
    | (SS_NOP_propagate_everything, _) -> 1
    | (_, SS_NOP_propagate_everything) -> -1

    | (SS_Flowing_flow_write_to_memory writes1,
       SS_Flowing_flow_write_to_memory writes2)
        -> Pervasives.compare writes1 writes2
    | (SS_Flowing_flow_write_to_memory _, _) -> 1
    | (_, SS_Flowing_flow_write_to_memory _) -> -1

    | (SS_Flowing_flow_barrier_to_memory barrier1,
       SS_Flowing_flow_barrier_to_memory barrier2)
        -> Pervasives.compare barrier1 barrier2
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
               (fun () -> Pervasives.compare sliced_write1 sliced_write2);
             ]
    | (SS_Flowing_partially_satisfy_read _, _) -> 1
    | (_, SS_Flowing_partially_satisfy_read _) -> -1

	| (SS_TSO_propagate_write_to_memory write1, SS_TSO_propagate_write_to_memory write2) ->
		Pervasives.compare write1 write2
	(* unused case (last):
	| (SS_TSO_propagate_write_to_memory _, _) -> 1
	| (_, SS_TSO_propagate_write_to_memory _) -> -1
	*)



    | (SS_Promising_stop_promising, SS_Promising_stop_promising) -> 0
    | (SS_Promising_stop_promising, _) -> 1
    | (_, SS_Promising_stop_promising) -> -1
  in

  let cmp_ss_sync_trans l1 l2 =
    match (l1, l2) with
    | (SS_PLDI11_acknowledge_sync_barrier barrier1,
       SS_PLDI11_acknowledge_sync_barrier barrier2)
        -> Pervasives.compare barrier1 barrier2
    | (SS_PLDI11_acknowledge_sync_barrier _, _) -> 1
    | (_, SS_PLDI11_acknowledge_sync_barrier _) -> -1

    | (SS_POP_read_response (read1, _),
       SS_POP_read_response (read2, _))
        -> read_requestCompare read1 read2
    | (SS_POP_read_response _, _) -> 1
    | (_, SS_POP_read_response _) -> -1

    | (SS_NOP_read_response_segment (read1, _),
       SS_NOP_read_response_segment (read2, _))
        -> read_requestCompare read1 read2
    | (SS_NOP_read_response_segment _, _) -> 1
    | (_, SS_NOP_read_response_segment _) -> -1

    | (SS_NOP_read_response_memory (read1, _, events1, events1'),
       SS_NOP_read_response_memory (read2, _, events2, events2'))
        ->
        cmps [ (fun () -> read_requestCompare read1 read2);
               (* Fix these below *)
               (fun () -> Pervasives.compare events1 events2);
               (fun () -> Pervasives.compare events1' events2');
             ]
    | (SS_NOP_read_response_memory _, _) -> 1
    | (_, SS_NOP_read_response_memory _) -> -1

    | (SS_Flowing_seg_read_response (read1, _),
       SS_Flowing_seg_read_response (read2, _))
        -> read_requestCompare read1 read2
    | (SS_Flowing_seg_read_response _, _) -> 1
    | (_, SS_Flowing_seg_read_response _) -> -1

    | (SS_Flowing_mem_read_response (read1, _),
       SS_Flowing_mem_read_response (read2, _))
        -> read_requestCompare read1 read2
    (* unused case (last):
    | (SS_Flowing_mem_read_response _, _) -> 1
    | (_, SS_Flowing_mem_read_response _) -> -1
    *)
  in

  let cmp_t_only_trans l1 l2 =
    match (l1, l2) with
    | (T_internal_outcome, T_internal_outcome) -> 0
    | (T_internal_outcome, _) -> 1
    | (_, T_internal_outcome) -> -1

    | (T_pending_memory_read_request, T_pending_memory_read_request) -> 0
    | (T_pending_memory_read_request, _) -> 1
    | (_, T_pending_memory_read_request) -> -1

    | (T_pseudoreg_read (reg_name1, _), T_pseudoreg_read (reg_name2, _))
        -> Pervasives.compare reg_name1 reg_name2
    | (T_pseudoreg_read _, _) -> 1
    | (_, T_pseudoreg_read _) -> -1

    | (T_pseudoreg_write (reg_name1, _), T_pseudoreg_write (reg_name2, _))
        -> Pervasives.compare reg_name1 reg_name2
    | (T_pseudoreg_write _, _) -> 1
    | (_, T_pseudoreg_write _) -> -1

    | (T_footprint_outcome, T_footprint_outcome) -> 0
    | (T_footprint_outcome, _) -> 1
    | (_, T_footprint_outcome) -> -1

    | (T_actually_satisfy _, T_actually_satisfy _) -> 0
    | (T_actually_satisfy _, _) -> 1
    | (_, T_actually_satisfy _) -> -1

    | (T_register_read (reg_name1, _, _), T_register_read (reg_name2, _, _))
        -> Pervasives.compare reg_name1 reg_name2
    | (T_register_read _, _) -> 1
    | (_, T_register_read _) -> -1

    | (T_register_write (reg_name1, _), T_register_write (reg_name2, _))
        -> Pervasives.compare reg_name1 reg_name2
    | (T_register_write _, _) -> 1
    | (_, T_register_write _) -> -1

    | (T_mem_forward_write (read_request1, _),
       T_mem_forward_write (read_request2, _))
        -> read_requestCompare read_request1 read_request2
    | (T_mem_forward_write _, _) -> 1
    | (_, T_mem_forward_write _) -> -1

    | (T_mem_write_footprint write1, T_mem_write_footprint write2)
        -> Pervasives.compare write1 write2
    | (T_mem_write_footprint _, _) -> 1
    | (_, T_mem_write_footprint _) -> -1

    | (T_mem_potential_write write1, T_mem_potential_write write2)
        -> Pervasives.compare write1 write2
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
        -> Pervasives.compare barrier1 barrier1
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
        -> Pervasives.compare write1 write2
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
        let cmp (write1, _, _) (write2, _, _) = Pervasives.compare write1 write2 in
        cmp_tl cmp tl1 tl2
    | (T_propagate_write _, _) -> 1
    | (_, T_propagate_write _) -> -1

    | (T_propagate_barrier tl1, T_propagate_barrier tl2) ->
        let cmp (b1: barrier) (b2: barrier) = Pervasives.compare b1 b2 in
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
        let cmp (write1, _) (write2, _) = Pervasives.compare write1 write2 in
        cmp_tl cmp tl1 tl2
    | (T_Flat_try_commit_store_cond _, _) -> 1
    | (_, T_Flat_try_commit_store_cond _) -> -1

    | (T_TSO_mem_satisfy_read tl1, T_TSO_mem_satisfy_read tl2) ->
        let cmp read1 read2 = read_requestCompare read1 read2 in
        cmp_tl cmp tl1 tl2
    | (T_TSO_mem_satisfy_read _, _) -> 1
    | (_, T_TSO_mem_satisfy_read _) -> -1

    | (T_POP_tm_start tl1, T_POP_tm_start tl2) ->
        let cmp (t1: transaction_start) (t2: transaction_start) = Pervasives.compare t1 t2 in
        cmp_tl cmp tl1 tl2
    | (T_POP_tm_start _, _) -> 1
    | (_, T_POP_tm_start _) -> -1

    | (T_POP_tm_commit tl1, T_POP_tm_commit tl2) ->
        let cmp (t1: transaction_start) (t2: transaction_start) = Pervasives.compare t1 t2 in
        cmp_tl cmp tl1 tl2
    | (T_POP_tm_commit _, _) -> 1
    | (_, T_POP_tm_commit _) -> -1

    | (T_POP_tm_abort tl1, T_POP_tm_abort tl2) ->
        let cmp (t1, r1) (t2, r2) = Pervasives.compare (t1, r1) (t2, r2) in
        cmp_tl cmp tl1 tl2
    | (T_POP_tm_abort _, _) -> 1
    | (_, T_POP_tm_abort _) -> -1

    | (T_try_store_excl tl1, T_try_store_excl tl2) ->
        let cmp (read1, _, _) (read2, _, _) = read_requestCompare read1 read2 in
        cmp_tl cmp tl1 tl2
    | (T_try_store_excl _, _) -> 1
    | (_, T_try_store_excl _) -> -1

    | (T_fetch tl1, T_fetch tl2) ->
        let cmp (addr1, _) (addr2, _) = Sail_impl_base.addressCompare addr1 addr2 in
        cmp_tl cmp tl1 tl2

    | (T_Promising_mem_satisfy_read tl1, T_Promising_mem_satisfy_read tl2) -> -1
        (* let cmp (read1,_) (read2,_) = read_requestCompare read1 read2 in
         * cmp_tl cmp tl1 tl2 *)
    | (T_Promising_mem_satisfy_read _, _) -> 1
    | (_, T_Promising_mem_satisfy_read _) -> -1

    | (T_Promising_mem_satisfy_read_nonshared tl1, T_Promising_mem_satisfy_read_nonshared tl2) -> -1
        (* let cmp (read1,_) (read2,_) = read_requestCompare read1 read2 in
         * cmp_tl cmp tl1 tl2 *)
    | (T_Promising_mem_satisfy_read_nonshared _, _) -> 1
    | (_, T_Promising_mem_satisfy_read_nonshared _) -> -1

    | (T_Promising_propagate_write tl1, T_Promising_propagate_write tl2) -> -1
        (* let cmp (w1, _) (w2, _) = Pervasives.compare w1 w2 in
         * cmp_tl cmp tl1 tl2 *)
    | (T_Promising_propagate_write _, _) -> 1
    | (_, T_Promising_propagate_write _) -> -1

    | (T_Promising_fulfil_promise tl1, T_Promising_fulfil_promise tl2) -> -1
        (* let cmp w1 w2 = Pervasives.compare w1 w2 in
         * cmp_tl cmp tl1 tl2 *)
    | (T_Promising_fulfil_promise _, _) -> 1
    | (_, T_Promising_fulfil_promise _) -> -1

    | (T_Promising_propagate_write_nonshared tl1, T_Promising_propagate_write_nonshared tl2) -> -1
        (* let cmp w1 w2 = Pervasives.compare w1 w2 in
         * cmp_tl cmp tl1 tl2 *)
    | (T_Promising_propagate_write_nonshared _, _) -> 1
    | (_, T_Promising_propagate_write_nonshared _) -> -1

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
        let cmp (r_address1, _) (r_address2, _) = Pervasives.compare r_address1 r_address2 in
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
    
  | (Sys_trans _, _) -> 1
  | (_, Sys_trans _) -> -1
  end


(* ***************************************************************** *)
(* miscellaneous auxiliary functions                                 *)
(* ***************************************************************** *)

exception Transitive
let rec option_map f xs = 
  match xs with
  | [] -> [] 
  | x::xs -> 
      ( match f x with 
      | None -> option_map f xs 
      | Some x -> x :: (option_map f xs) ) 

let transitive_reduction eq r =
  let transitive_pairs = 
    List.flatten 
      (List.map 
         (fun (a1,a2) -> 
           option_map (fun (a1',a2') -> if eq a2 a1' then Some (a1,a2') else None) r)
         r) in
  (* a partial check for cycles *)
  if List.exists (fun (a1,a2)->eq a1 a2) (r @ transitive_pairs) then 
    raise Transitive;
  List.filter (fun (a1,a2) -> not (List.exists (function (b1,b2) -> (eq a1 b1 && eq a2 b2)) transitive_pairs)) r

let set_transitive_reduction eq empty relation =
  let relation2 = Pset.fold (fun (a,b) prev -> Pset.fold (fun (b',c') prev' -> if eq b b' then (Pset.add (a,c') prev') else prev') relation prev) relation empty in
  if Pset.exists (fun (a,b) -> eq a b) relation2 then
    raise Transitive;
  Pset.filter (fun (a,b) -> not (Pset.exists (fun (a',b') -> eq a a' && eq b b') relation2)) relation

(* Utility functions on sets *)
let map_set (f:'a -> 'b) (xs: 'a Pset.set) : 'b Pset.set =
  Pset.fold
    (fun x k ->
      Pset.add (f x) k)
    xs (Pset.empty compare)

let cross_prod_set : 'a Pset.set -> 'b Pset.set -> ('a * 'b) Pset.set =
  fun xs ys ->
    Pset.fold
      (fun x k ->
	Pset.fold
	  (fun y k' ->
	    Pset.add (x,y) k')
	  ys k)
      xs (Pset.empty compare)



(* 
let to_8bytevalue n = 
  Interp_inter_imp.num_to_bits 64
    Interp_interface.Bytev (Big_int_Z.big_int_of_int n)

let to_64bitvalue n = 
  Interp_inter_imp.num_to_bits 64
    Interp_interface.Bitv (Big_int_Z.big_int_of_int n)
    
let to_address n = 
  MachineDefTypes.address_of_bytevector_value 
    (Interp_inter_imp.num_to_bits 64
       Interp_interface.Bytev (Big_int_Z.big_int_of_int n))
    
let to_memory_value n = 
  MachineDefTypes.memory_value_of_value
    (Interp_inter_imp.num_to_bits 32
       Interp_interface.Bytev (Big_int_Z.big_int_of_int n))
    *)



(********************************************************************)

(* the Flowing topologies for exhaustive enumeration; we believe binary
trees cover all possible behaviours *)
let exhaustive_topologies = function
| 1 -> ["0"]
| 2 -> ["[0,1]"]
| 3 -> ["[[0,1],2]"; "[[0,2],1]"; "[0,[1,2]]"]
| 4 ->
    ["[[0,1],[2,3]]";
     "[[0,2],[1,3]]";
     "[[0,3],[1,2]]";
     "[[[0,1],2],3]";
     "[[[0,1],3],2]";
     "[[[0,2],1],3]";
     "[[[0,2],3],1]";
     "[[[0,3],1],2]";
     "[[[0,3],2],1]";
     "[[[1,2],0],3]";
     "[[[1,2],3],0]";
     "[[[1,3],0],2]";
     "[[[1,3],2],0]";
     "[[[2,3],0],1]";
     "[[[2,3],1],0]"]
| _ -> raise (Failure "'-topauto true' does not support less than 1 or more than 4 threads")

(* the Flowing topologies the UI offers; head is the default topology *)
let ui_topologies = function
| 1 -> exhaustive_topologies 1
| 2 -> exhaustive_topologies 2
| 3 -> "[0,1,2]" :: exhaustive_topologies 3
| 4 -> "[0,1,2,3]" :: exhaustive_topologies 4
| n -> exhaustive_topologies n

(* parse a tree topology from a string

tops ::=
  | t ; tops
  | t

t ::=
  | [ ts ]
  | n

ts ::=
    | t , ts
    | t

examples:
  "0" will be parsed to [FTopo_leaf 0]

  "[0,1]" will be parsed to [FTopo_join [FTopo_leaf 0; FTopo_leaf 1]]

  "[[0,1],2]" will be parsed to [FTopo_join [FTopo_join [FTopo_leaf 0; FTopo_leaf 1]; FTopo_leaf 2]]

  "[[2,1],[0,3]]; [0,[1,2,3]]; [0,1,2,3]" will be parsed to
    [ FTopo_join [FTopo_join [FTopo_leaf 2; FTopo_leaf 1]; FTopo_join [FTopo_leaf 0; FTopo_leaf 3]];
      FTopo_join [FTopo_leaf 0; FTopo_join [FTopo_leaf 1; FTopo_leaf 2; FTopo_leaf 3]];
      FTopo_join [FTopo_leaf 0; FTopo_leaf 1; FTopo_leaf 2; FTopo_leaf 3] ]

*)

type parser_token =
  | NON_tok    (* this token will be thrown out from the stream *)
  | NUM of int (* unsigned integer *)
  | COMMA      (* , *)
  | SEMI       (* ; *)
  | LBRK       (* [ *)
  | RBRK       (* ] *)

let rec digs res str pos =
  if pos >= String.length str then res
  else if Char.code str.[pos] < Char.code '0' then res
  else if Char.code str.[pos] > Char.code '9' then res
  else begin match res with
  | None -> digs (Some (String.make 1 str.[pos])) str (pos+1)
  | Some res_str -> digs (Some (res_str ^ (String.make 1 str.[pos]))) str (pos+1)
  end

let rec spaces res str pos =
  if pos >= String.length str then res
  else if str.[pos] <> ' ' then res
  else begin match res with
  | None -> spaces (Some " ") str (pos+1)
  | Some res_str -> spaces (Some (res_str ^ " ")) str (pos+1)
  end

let token_specs =
  [ ((fun str pos -> if str.[pos] = ',' then Some ","  else None),      fun str -> COMMA);
    ((fun str pos -> if str.[pos] = ';' then Some ";"  else None),      fun str -> SEMI);
    ((fun str pos -> if str.[pos] = '[' then Some "["  else None),      fun str -> LBRK);
    ((fun str pos -> if str.[pos] = ']' then Some "]"  else None),      fun str -> RBRK);
    ((digs None),    fun str -> NUM (int_of_string str));
    ((spaces None),  fun str -> NON_tok); (* throw out spaces *)
  ]

(*let token_specs =
  [ ("[0-9]+", fun str -> NUM (int_of_string str));
    (",",      fun str -> COMMA);
    (";",      fun str -> SEMI);
    ("\[",     fun str -> LBRK);
    ("\]",     fun str -> RBRK);
    (" +",     fun str -> NON_tok); (* throw out spaces *)
  ]*)

type parse_record =
  | Tok_rec of parser_token
  | TOPS_rec of flowing_topology list
  | T_rec of flowing_topology
  | TS_rec of flowing_topology list

type stream = parser_token list
type grammer_rule = stream -> (parse_record * stream) option
type grammer_effect = parse_record list -> parse_record
type grammer_list = grammer_rule list * grammer_effect

let rec parse_rule (rules : grammer_rule list) (stream : stream) : (parse_record list * stream) option =
  match rules with
  | [] -> Some ([], stream)
  | rule::rules ->
      match rule stream with
      | None -> None
      | Some (record, stream) ->
          match parse_rule rules stream with
          | None -> None
          | Some (records, stream) -> Some (record::records, stream)

let rec make_rule (rules : grammer_list list) : (grammer_rule) =
  function stream ->
    match rules with
    | [] -> None
    | (rule, update)::rules ->
        match parse_rule rule stream with
        | Some (records, stream) -> Some (update records, stream)
        | None -> make_rule rules stream


let token (tok : parser_token) : grammer_rule =
  function tok'::stream when tok' = tok -> Some (Tok_rec (tok'), stream)
    | _ -> None

let number_rule : grammer_rule =
  function (NUM _ as token)::stream -> Some (Tok_rec (token), stream)
    | _ -> None


(* tops ::= *)
let rec tops_rule stream =
  make_rule
  [(        [t_rule    ; token SEMI; tops_rule    ], (* | t ; tops *)
   function [T_rec t   ; _         ; TOPS_rec tops] -> TOPS_rec (t::tops) | _ -> failwith "topology parser error");

   (        [t_rule ], (* | t *)
   function [T_rec t] -> TOPS_rec [t] | _ -> failwith "topology parser error");
  ] stream

(* t ::= *)
and t_rule stream =
  make_rule
  [(        [token LBRK; ts_rule  ; token RBRK], (* | [ ts ] *)
   function [_         ; TS_rec ts; _         ] -> T_rec (FTopo_join ts) | _ -> failwith "topology parser error");

   (        [number_rule      ], (* | n *)
   function [Tok_rec (NUM tid)] -> T_rec (FTopo_leaf tid) | _ -> failwith "topology parser error");
  ] stream

(* ts ::= *)
and ts_rule stream =
  make_rule
  [(        [t_rule ; token COMMA; ts_rule  ], (* | t, ts *)
   function [T_rec t; _          ; TS_rec ts] -> TS_rec (t::ts) | _ -> failwith "topology parser error");

   (        [t_rule ], (* | t *)
   function [T_rec t] -> TS_rec [t] | _ -> failwith "topology parser error");
  ] stream


let tokenizer token_specs str : stream option =
(*   let token_specs = List.map (fun (regexp, tok) -> (Str.regexp regexp, tok)) token_specs in *)

  let rec tokenizer_helper str_pos tokens =
    if str_pos = String.length str then Some (List.rev tokens)
    else if str_pos > String.length str then failwith "tokenizer passed string end"
    else

    (* find token hits for current position *)
    let possible_tokens =
      List.map
        (fun (regexp, tok) ->
            begin match regexp str str_pos with
            | Some matched_string -> Some (matched_string, tok)
            | None -> None
            end)
        (*(fun (regexp, tok) ->
            if Str.string_match regexp str str_pos then Some (Str.matched_string str, tok)
            else None)*)
        token_specs in

    (* find the token with longest hit, add it to tokens result and continue *)
    let max_token max_tok tok =
      match (max_tok, tok) with
      | (Some (max_str, max_token), Some (str, token))
           when String.length str > String.length max_str -> Some (str, token)
      | (None,                      Some (str, token)) -> Some (str, token)
      | (max, _) -> max
      in
    match List.fold_left max_token None possible_tokens with
    | Some (str, token) -> tokenizer_helper (str_pos + String.length str) ((token str)::tokens)
    | None -> None (* could not find any token at this position *)
    in

  match tokenizer_helper 0 [] with
  | None -> None
  | Some tokens ->
      (* remove the NON_tok-s *)
      Some (List.filter ((<>) NON_tok) tokens)

let rec all_top_thread_ids top =
    match top with
    | FTopo_leaf tid -> [tid]
    | FTopo_join tops -> List.flatten (List.map all_top_thread_ids tops)

let check_topology_duplicates top =
  let all_thread_ids = all_top_thread_ids top in
  if List.length (List.sort_uniq compare all_thread_ids) != List.length all_thread_ids then
    failwith "duplicate thread IDs in topology"
  else
    top

let parse_topologies (str : string) : flowing_topology list option =
  match tokenizer token_specs str with
  | None -> None
  | Some tokens ->
      match parse_rule [tops_rule] tokens with
      | Some ([TOPS_rec tops], stream)
        when stream = [] -> Some (List.map check_topology_duplicates tops)
      | _ -> None



(** **************************************** *)
(** command-line processing of model options *)
(** **************************************** *)

let assoc_image assoc = snd (List.split assoc)
let assoc_rev value assoc =
  let assoc' = List.map (fun (a,b) -> (b,a)) assoc in
  List.assoc value assoc'

let gen_parser assoc update =
  (fun mbef s -> update mbef (assoc_rev s assoc))

(******************************************************
  How to add a new model option:

  Add:
  XXX_assoc of type list (typ * string)
  XXX_update of type model_params -> typ -> model_params
  XXX_value of type model_params -> typ

  Add XXX_assoc and XXX_update to the parsers,
  model_strings and current_model lists

  Given "-model some_string" in the command line the model
  will be updated using XXX_update with the value associated
  with some_string in XXX_assoc.
  Also when -help prints the current model options,
  XXX_value will be used to determine the value which
  will be printed out using the string from XXX_assoc.

******************************************************)

(* Example:
let XX_assoc =
  [(AA, "aA");
   (BB, "bB")]
let XX_update params value = {params with t = {params.t with XX = value}}
let XX_value params = params.t.XX
*)

open MachineDefTypes

let model_assoc =
  [((PLDI11_storage_model,  PLDI11_thread_model),           "pldi11");
   ((Flowing_storage_model, POP_thread_model Standard_POP), "flowing");
   ((Flat_storage_model,    POP_thread_model Flat_POP),     "flat");
   ((POP_storage_model,     POP_thread_model Standard_POP), "pop");
   ((NOP_storage_model,     POP_thread_model Standard_POP), "nop");
   ((TSO_storage_model,     TSO_thread_model),              "tso");
   ((Promising_storage_model, Promising_thread_model),      "promising");
   ((Flat_storage_model,    Relaxed_thread_model),          "relaxed");]

let model_update params (ss_val, t_val) =
  let ss = {params.ss with ss_model=ss_val} in
  let t = {params.t with thread_model=t_val} in
  {{params with ss = ss} with t = t}
let model_value params = (params.ss.ss_model, params.t.thread_model)

let restrict_assoc =
  [((false, RestrictionNone),     "not-restricted");
   ((true,  RestrictionSC),       "sc");
   ((true,  RestrictionSCANASC),  "sca-nasc")]
let restrict_update params (ss_sc, t_res) =
  let ss = {params.ss with ss_sc = ss_sc} in
  let t = {params.t with thread_restriction = t_res} in
  {{params with ss = ss} with t = t}
let restrict_value params = (params.ss.ss_sc, params.t.thread_restriction)

(* thread: *)

let thread_fsa_assoc =
  [(Flowing_same_address_strict,     "flowing_same_address_strict");
   (Flowing_same_address_aggressive, "flowing_same_address_aggressive")]
let thread_fsa_update params value = {params with t = {params.t with thread_fsa = value}}
let thread_fsa_value params = params.t.thread_fsa

let thread_rwc_assoc =
  [(Forbid_prev_uncommitted_read_type0,                "forbid_uncommitted_read");
   (Allow_prev_uncommitted_determined_addr_read_type1, "allow_uncommitted_determined_read")]
let thread_rwc_update params value = {params with t = {params.t with thread_rwc = value}}
let thread_rwc_value params = params.t.thread_rwc

let thread_rr_assoc =
  [(Restart_on_commit,       "restart_on_read_commit");
   (Restart_on_read_satisfy, "restart_on_read_satisfy")]
let thread_rr_update params value = {params with t = {params.t with thread_rr = value}}
let thread_rr_value params = params.t.thread_rr

let thread_allow_tree_speculation_assoc =
  [(true,       "allow_tree_speculation");
   (false,      "forbid_tree_speculation")]
let thread_allow_tree_speculation_update params value = {params with t = {params.t with thread_allow_tree_speculation = value}}
let thread_allow_tree_speculation_value params = params.t.thread_allow_tree_speculation

let thread_allow_write_subsumption_assoc =
  [(true,       "allow_write_subsumption");
   (false,      "forbid_write_subsumption")]
let thread_allow_write_subsumption_update params value = {params with t = {params.t with thread_allow_write_subsumption = value}}
let thread_allow_write_subsumption_value params = params.t.thread_allow_write_subsumption

let thread_fail_on_loop_assoc =
  [(true,       "forbid_loops");
   (false,      "allow_loops")]
let thread_fail_on_loop_update params value = {params with t = {params.t with thread_fail_on_loop = value}}
let thread_fail_on_loop_value params = params.t.thread_fail_on_loop

let thread_certify_after_stop_promising_assoc =
  [(true,       "promising_certify_after_stop_promising");
   (false,      "promising_stop_certifying_after_stop_promising")]
let thread_certify_after_stop_promising_update params value = 
  {params with t = {params.t with thread_certify_after_stop_promising = value}}
let thread_certify_after_stop_promising_value params = params.t.thread_certify_after_stop_promising

let thread_run_after_stop_promising_assoc =
  [(true,       "promising_run_after_stop_promising");
   (false,      "promising_stop_running_after_stop_promising")]
let thread_run_after_stop_promising_update params value = 
  {params with t = {params.t with thread_run_after_stop_promising = value}}
let thread_run_after_stop_promising_value params = params.t.thread_run_after_stop_promising

let promising_partial_order_assoc =
  [(true,       "promising_partial_order");
   (false,      "promising_total_order")]
let promising_partial_order_update params value = 
  {params with t = {params.t with promising_partial_order = value}}
let promising_partial_order_value params = params.t.promising_partial_order



(* storage: *)

(*
let coherence_commit_assoc =
  [(Partial_CC, "partial_cc");
   (Late_CC,    "late_cc")]
let coherence_commit_update params value = {params with ss = {params.ss with coherence_commit = value}}
let coherence_commit_value params = params.ss.coherence_commit
*)

let new_coh_assoc =
  [(Use_old_coherence, "use_old_coherence");
   (Use_new_coherence, "use_new_coherence");
   (Check_new_vs_old,  "check_new_vs_old")]
let new_coh_update params value = {params with ss = {params.ss with new_coh = value}}
let new_coh_value params = params.ss.new_coh

let pw_assoc =
  [(Only_at_end,        "propagate_at_end");
   (Possibly_in_middle, "propagate_in_middle")]
let pw_update params value = {params with ss = {params.ss with pw = value}}
let pw_value params = params.ss.pw

(*
let bc_assoc =
  [(Weak_BC,       "barrier_coherence_weak");
   (SemiStrong_BC, "barrier_coherence_semistrong");
   (Strong_BC,     "barrier_coherence_strong");]
let bc_update params value = {params with ss = {params.ss with bc = value}}
let bc_value params = params.ss.bc
*)

let promise_first_assoc =
  [(true,       "promise_first");
   (false,      "promise_anytime")]
let promise_first_update params value = {params with ss = {params.ss with promise_first = value}}
let promise_first_value params = params.ss.promise_first

let parsers =
  [(*  gen_parser XX_assoc XX_update; *)
    gen_parser model_assoc model_update;

    gen_parser restrict_assoc restrict_update;

    gen_parser thread_fsa_assoc thread_fsa_update;
    gen_parser thread_rwc_assoc thread_rwc_update;
    gen_parser thread_rr_assoc thread_rr_update;
    gen_parser thread_allow_tree_speculation_assoc thread_allow_tree_speculation_update;
    gen_parser thread_allow_write_subsumption_assoc thread_allow_write_subsumption_update;
    gen_parser thread_fail_on_loop_assoc thread_fail_on_loop_update;
    gen_parser thread_certify_after_stop_promising_assoc thread_certify_after_stop_promising_update;
    gen_parser thread_run_after_stop_promising_assoc thread_run_after_stop_promising_update;
    gen_parser promising_partial_order_assoc promising_partial_order_update;

(*    gen_parser coherence_commit_assoc coherence_commit_update;*)
    gen_parser new_coh_assoc new_coh_update;
    gen_parser pw_assoc pw_update;
    gen_parser promise_first_assoc promise_first_update;
(*    gen_parser bc_assoc bc_update;*)
  ]

let model_strings =
(*   (assoc_image XX_assoc) @ *)
  (assoc_image model_assoc) @
  (assoc_image restrict_assoc) @
  (* thread: *)
  (assoc_image thread_fsa_assoc) @
  (assoc_image thread_rwc_assoc) @
  (assoc_image thread_rr_assoc) @
  (assoc_image thread_allow_tree_speculation_assoc) @
  (assoc_image thread_allow_write_subsumption_assoc) @
  (assoc_image thread_fail_on_loop_assoc) @
  (assoc_image thread_certify_after_stop_promising_assoc) @
  (assoc_image thread_run_after_stop_promising_assoc) @
  (assoc_image promising_partial_order_assoc) @
  (* storage: *)
(*  (assoc_image coherence_commit_assoc) @*)
  (assoc_image new_coh_assoc) @
  (assoc_image pw_assoc) @
  (assoc_image promise_first_assoc)
(*  (assoc_image bc_assoc) @*)


let current_model params =
  [(* (List.assoc (XX_value params) XX_assoc); *)
    (List.assoc (model_value params) model_assoc);
    (List.assoc (restrict_value params) restrict_assoc);
    (* thread: *)
    (List.assoc (thread_fsa_value params) thread_fsa_assoc);
    (List.assoc (thread_rwc_value params) thread_rwc_assoc);
    (List.assoc (thread_rr_value params) thread_rr_assoc);
    (List.assoc (thread_allow_tree_speculation_value params) thread_allow_tree_speculation_assoc);
    (List.assoc (thread_allow_write_subsumption_value params) thread_allow_write_subsumption_assoc);
    (List.assoc (thread_fail_on_loop_value params) thread_fail_on_loop_assoc);
    (List.assoc (thread_certify_after_stop_promising_value params) thread_certify_after_stop_promising_assoc);
    (List.assoc (thread_run_after_stop_promising_value params) thread_run_after_stop_promising_assoc);
    (List.assoc (promising_partial_order_value params) promising_partial_order_assoc);
    (* storage: *)
(*    (List.assoc (coherence_commit_value params) coherence_commit_assoc);*)
    (List.assoc (new_coh_value params) new_coh_assoc);
    (List.assoc (pw_value params) pw_assoc);
    (List.assoc (promise_first_value params) promise_first_assoc);
(*    (List.assoc (bc_value params) bc_assoc);*)
  ]


let pp_model params =
  let model =
    try String.concat "; " (current_model params)
    with Not_found -> failwith "failed to stringify the current model options"
  in
  "[" ^ model ^ "]"


let parse_and_update_model an_option params =
  let run_parser (params, succ) a_parser =
    try (a_parser params an_option, true)
    with Not_found -> (params, succ)
  in

  let (params', succ) = List.fold_left run_parser (params,false) parsers in
  if succ then params'
  else Warn.user_error "Unknown model %s" an_option


(********************************************************************)

exception BranchTargetsParsingError of string

let branch_targets_parse lexbuf : Branch_targets_parser_base.ast list =
  let print_position lexbuf : string =
    let pos = lexbuf.Lexing.lex_curr_p in
    Printf.sprintf "%s:%d:%d" pos.Lexing.pos_fname
        pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1)
  in
  try Branch_targets_parser.lines Branch_targets_lexer.read lexbuf with
  | Branch_targets_lexer.SyntaxError msg ->
      let msg = Printf.sprintf "%s: %s" (print_position lexbuf) msg in
      raise (BranchTargetsParsingError msg)
  | Parsing.Parse_error ->
      let msg = Printf.sprintf "%s: syntax error" (print_position lexbuf) in
      raise (BranchTargetsParsingError msg)

let branch_targets_parse_from_file (filename: string) : Branch_targets_parser_base.ast list =
  Utils.safe_open_in filename @@ fun chan ->
  let lexbuf = Lexing.from_channel chan in
  lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = filename };
  branch_targets_parse lexbuf

let branch_targets_parse_from_string (str: string) : Branch_targets_parser_base.ast list =
  let lexbuf = Lexing.from_string str in
  (*lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };*)
  branch_targets_parse lexbuf

let set_branch_targets
    (symbol_table:   ((Sail_impl_base.address * int) * string) list)
    (branch_targets: Branch_targets_parser_base.ast list)
    (model:          model_params)
    : model_params
  =
  let labels_map =
    let (fps, names) = List.split symbol_table in
    List.combine names fps
  in

  let address_of_location = function
    | Branch_targets_parser_base.Absolute n ->
        Sail_impl_base.address_of_integer n
    | Branch_targets_parser_base.Label_and_offset (label, offset) ->
        begin match List.assoc label labels_map with
        | (addr, _) ->
            Sail_impl_base.integer_of_address addr
            |> Nat_big_num.add offset
            |> Sail_impl_base.address_of_integer
        | exception Not_found -> failwith @@ "the location label \"" ^ label ^ "\" does not exist"
        end
  in

  let branch_targets =
    List.map
      (fun (bt: Branch_targets_parser_base.ast) ->
        (bt.Branch_targets_parser_base.thread,
          address_of_location bt.Branch_targets_parser_base.branch_loc,
          List.map address_of_location bt.Branch_targets_parser_base.branch_targets
        )
      )
      branch_targets
    |> List.sort (fun (t1, addr1, _) (t2, addr2, _) ->
          match compare t1 t2 with
          | 0 -> Sail_impl_base.addressCompare addr1 addr2
          | c -> c)
    |> List.fold_left (fun acc (tid, addr, addrs) ->
      match acc with
      | (tid', (addr', addrs') :: tbts) :: acc
          when tid' = tid && Sail_impl_base.addressEqual addr' addr
          -> (tid, (addr, addrs @ addrs') :: tbts) :: acc
      | (tid', tbts) :: acc when tid' = tid ->
          (tid, (addr, addrs) :: tbts) :: acc
      | _ -> (tid, (addr, addrs) :: []) :: acc
    ) []
    |> MachineDefSystem.branch_targets_from_list
  in

  {model with t = {model.t with branch_targets  = branch_targets}}



exception SharedMemoryParsingError of string

let shared_memory_parse lexbuf : Shared_memory_parser_base.footprint list =
  let print_position lexbuf : string =
    let pos = lexbuf.Lexing.lex_curr_p in
    Printf.sprintf "%s:%d:%d" pos.Lexing.pos_fname
        pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1)
  in
  try Shared_memory_parser.footprints Shared_memory_lexer.read lexbuf with
  | Shared_memory_lexer.SyntaxError msg ->
      let msg = Printf.sprintf "%s: %s\n" (print_position lexbuf) msg in
      raise (SharedMemoryParsingError msg)
  | Parsing.Parse_error ->
      let msg = Printf.sprintf "%s: syntax error\n" (print_position lexbuf) in
      raise (SharedMemoryParsingError msg)

let shared_memory_parse_from_file (filename: string) : Shared_memory_parser_base.footprint list =
  Utils.safe_open_in filename @@ fun chan ->
  let lexbuf = Lexing.from_channel chan in
  lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = filename };
  shared_memory_parse lexbuf

let shared_memory_parse_from_string (str: string) : Shared_memory_parser_base.footprint list =
  let lexbuf = Lexing.from_string str in
  (*lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };*)
  shared_memory_parse lexbuf


let set_shared_memory
    (symbol_table:  ((Sail_impl_base.address * int) * string) list)
    (shared_memory: Shared_memory_parser_base.footprint list)
    (model:         model_params)
    : model_params
  =
  let labels_map =
    let (fps, names) = List.split symbol_table in
    List.combine names fps
  in

  let shared_memory =
    List.map
      (function
        | Shared_memory_parser_base.Absolute (addr, size) ->
            (Sail_impl_base.address_of_integer addr, size)
        | Shared_memory_parser_base.Symbol (symb, None) ->
            begin try List.assoc symb labels_map with
            | Not_found -> failwith @@ "the symbol \"" ^ symb ^ "\" does not exist"
            end
        | Shared_memory_parser_base.Symbol (symb, Some (offset, size)) ->
            begin match List.assoc symb labels_map with
            | (addr, _) ->
                let addr =
                  Sail_impl_base.integer_of_address addr
                  |> Nat_big_num.add offset
                  |> Sail_impl_base.address_of_integer
                in
                (addr, size)
            | exception Not_found -> failwith @@ "the symbol \"" ^ symb ^ "\" does not exist"
            end
      )
      shared_memory
    |> Pset.from_list MachineDefTypes.footprintCompare
  in

  {model with shared_memory = Some shared_memory}
