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
open PromisingTransitions
open Model_aux

let cmp_tl cmp tl1 tl2 =
  cmps  [ (fun () -> Pervasives.compare tl1.ptl_tid tl2.ptl_tid);
          (fun () -> Pervasives.compare tl1.ptl_ioid tl2.ptl_ioid);
          (fun () -> cmp tl1.ptl_label tl2.ptl_label);
    ]
(* fuzzy_compare_transitions is used to determine if a transition was
enabled in the previous state so we can use the same UI number for it. *)
let p_fuzzy_compare_transitions trans1 trans2 =
  let cmp_pss_only_trans l1 l2 =
    match (l1, l2) with
    | (PSS_stop_promising _, PSS_stop_promising _) -> 0
    (* | (PSS_stop_promising _, _) -> 1
     * | (_, PSS_stop_promising _) -> -1 *)
  in

  let cmp_t_only_trans l1 l2 =
    match (l1, l2) with
    | (PT_finish (addr,_), PT_finish (addr',_))
        -> Sail_impl_base.addressCompare addr addr'
    | (PT_finish _, _) -> 1
    | (_, PT_finish _) -> -1

    | (PT_exception _, PT_exception _) -> 0
    | (PT_exception _, _) -> 1
    | (_, PT_exception _) -> -1

    | (PT_failed_store_excl, PT_failed_store_excl) -> 0
    (* | (PT_failed_store_excl, _) -> 1
     * | (_, PT_failed_store_excl) -> -1 *)
  in

  let cmp_t_sync_trans l1 l2 =
    match (l1, l2) with
    | (PT_Read tl1, PT_Read tl2) -> -1
        (* let cmp (read1,_) (read2,_) = read_requestCompare read1 read2 in
         * cmp_tl cmp tl1 tl2 *)
    | (PT_Read _, _) -> 1
    | (_, PT_Read _) -> -1
    
    | (PT_Read_nonshared tl1, PT_Read_nonshared tl2) -> -1
        (* let cmp (read1,_) (read2,_) = read_requestCompare read1 read2 in
         * cmp_tl cmp tl1 tl2 *)
    | (PT_Read_nonshared _, _) -> 1
    | (_, PT_Read_nonshared _) -> -1
    
    | (PT_Write tl1, PT_Write tl2) -> -1
        (* let cmp (w1, _) (w2, _) = Pervasives.compare w1 w2 in
         * cmp_tl cmp tl1 tl2 *)
    | (PT_Write _, _) -> 1
    | (_, PT_Write _) -> -1
    
    | (PT_Fulfil tl1, PT_Fulfil tl2) -> -1
        (* let cmp w1 w2 = Pervasives.compare w1 w2 in
         * cmp_tl cmp tl1 tl2 *)
    | (PT_Fulfil _, _) -> 1
    | (_, PT_Fulfil _) -> -1
    
    | (PT_Write_nonshared tl1, PT_Write_nonshared tl2) -> -1
        (* let cmp w1 w2 = Pervasives.compare w1 w2 in
         * cmp_tl cmp tl1 tl2 *)
    (* | (PT_Write_nonshared _, _) -> 1
     * | (_, PT_Write_nonshared _) -> -1 *)

  in

  begin match (trans1, trans2) with
  | (PSS_trans l1, PSS_trans l2) -> cmp_pss_only_trans l1 l2
  | (PSS_trans _, _) -> 1
  | (_, PSS_trans _) -> -1

  | (PT_trans sys_thread_trans1, PT_trans sys_thread_trans2) ->
    begin match (sys_thread_trans1, sys_thread_trans2) with
    | (PT_only tl1, PT_only tl2) -> cmp_tl cmp_t_only_trans tl1 tl2
    | (PT_only _, _) -> 1
    | (_, PT_only _) -> -1

    | (PT_sync (l1, _), PT_sync (l2, _)) -> cmp_t_sync_trans l1 l2
    | (PT_sync _, _) -> 1
    | (_, PT_sync _) -> -1

    | (PT_thread_start tl1, PT_thread_start tl2) ->
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
    
  | (PSys_trans _, _) -> 1
  | (_, PSys_trans _) -> -1
  end
