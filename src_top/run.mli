(*===================================================================================================*)
(*                                                                                                   *)
(*                rmem executable model                                                              *)
(*                =====================                                                              *)
(*                                                                                                   *)
(*  This file is:                                                                                    *)
(*                                                                                                   *)
(*  Copyright Shaked Flur, University of Cambridge                                       2014-2018   *)
(*  Copyright Susmit Sarkar, University of St Andrews                         2011-2012, 2014-2015   *)
(*  Copyright Peter Sewell, University of Cambridge                          2011-2012, 2014, 2016   *)
(*  Copyright Pankaj Pawan, IIT Kanpur and INRIA (when this work was done)               2011-2012   *)
(*  Copyright Christopher Pulte, University of Cambridge                                      2016   *)
(*  Copyright Ohad Kammar, University of Cambridge (when this work was done)                  2013   *)
(*  Copyright Luc Maranget, INRIA Paris                                                       2012   *)
(*                                                                                                   *)
(*  All rights reserved.                                                                             *)
(*                                                                                                   *)
(*  It is part of the rmem tool, distributed under the 2-clause BSD licence in                       *)
(*  LICENCE.txt.                                                                                     *)
(*                                                                                                   *)
(*===================================================================================================*)

module type S = sig
  val calc_finals:
    RunOptions.t ->
    Globals.ppmode ->
    Test.info ->
    MachineDefSystem.initial_state_record list ->
    unit
end

module Make: functor (ConcModel: Concurrency_model.S) -> S
