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


let p_fuzzy_compare_pt_trans trans1 trans2 = 
  match trans1, trans2 with
  | PT_Write (ids, (wd, pr, sh), t, _), PT_Write (ids', (wd', pr', sh'), t', _) ->
     cmps [ (fun () -> Stdlib.compare ids ids');
            (fun () -> Stdlib.compare pr pr');
            (fun () -> Stdlib.compare sh sh');
            (fun () -> Stdlib.compare t t');
       ]
  | PT_Write _, _ -> 1
  | _, PT_Write _ -> -1

  | PT_Fulfil (ids, (wd), (t), _), PT_Fulfil (ids', (wd'), (t'), _) ->
     cmps [ (fun () -> Stdlib.compare ids ids');
            (fun () -> Stdlib.compare wd wd');
            (fun () -> Stdlib.compare t t'); 
       ]
  | PT_Fulfil _, _ -> 1
  | _, PT_Fulfil _ -> -1

  | PT_Read (ids, (rr, t), (w, t2), _), PT_Read (ids', (rr', t'), (w', t2'), _) ->
     cmps [ (fun () -> Stdlib.compare ids ids');
            (fun () -> Stdlib.compare t t'); 
            (fun () -> Stdlib.compare t2 t2'); 
            (fun () -> Stdlib.compare rr rr'); 
            (fun () -> Stdlib.compare w w'); 
       ]
  | PT_Read _, _ -> 1
  | _, PT_Read _ -> -1

  | PT_finish (ids, _, _), PT_finish (ids', _, _) ->
     Stdlib.compare ids ids'
  | PT_finish _, _ -> 1
  | _, PT_finish _ -> 1

  | PT_failed_store_excl (ids, _), PT_failed_store_excl (ids', _) ->
     Stdlib.compare ids ids'
  | PT_failed_store_excl _, _ -> 1
  | _, PT_failed_store_excl _ -> 1

  | PT_exception (ids, _, _), PT_exception (ids', _, _) ->
     Stdlib.compare ids ids'
  (* | PT_exception _, _ -> 1
   * | _, PT_exception _ -> 1 *)

  (* | PT_Start_thread (ids, req, repl, _), PT_Start_thread (ids', req', repl', _) ->
   *    cmps [ (fun () -> Stdlib.compare ids ids');
   *           (fun () -> Stdlib.compare req req'); 
   *           (fun () -> Stdlib.compare repl repl'); 
   *      ] *)

let fuzzy_compare_p_trans trans1 trans2 =
  match (trans1, trans2) with

  | (PT t1, PT t2) -> p_fuzzy_compare_pt_trans t1 t2
  | (PT _, _) -> 1
  | (_, PT _) -> -1
    

  (* | (PSys_thread_start ((tid,ioid), tl, mtid, _),
   *    PSys_thread_start ((tid',ioid'), tl', mtid', _)) ->
   *    cmps [ (fun () -> Stdlib.compare (tid,ioid) (tid',ioid'));
   *           (fun () -> Stdlib.compare mtid mtid');
   *           (fun () -> Stdlib.compare tl tl');
   *          ]
   * | (PSys_thread_start _, _) -> 1
   * | (_, PSys_thread_start _) -> -1 *)

  | (PSys_stop_promising, PSys_stop_promising) -> 0
  | (PSys_stop_promising, _) -> 1
  | (_, PSys_stop_promising) -> -1

  | (PSys_finish _, _) -> 1
