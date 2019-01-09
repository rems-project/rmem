(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

type v = Constant.v
open Constant
        
let intToV i = Concrete (Nat_big_num.of_int i)
and nameToV s = Symbolic s

let compare c1 c2 = match c1,c2 with
| Concrete i1, Concrete i2 -> Nat_big_num.compare i1 i2
| Symbolic s1,Symbolic s2 -> String.compare s1 s2
| Concrete _,Symbolic _ -> -1
| Symbolic _,Concrete _ -> 1

open Printf
let pp hexa = function
  | Concrete i -> if hexa then "0x" ^ Misc.big_num_to_hex_string i else Nat_big_num.to_string i
  | Symbolic s -> s

let pp_v = pp false

let eq c1 c2 =  match c1,c2 with
| Concrete i1, Concrete i2 -> Nat_big_num.equal i1 i2
| Symbolic s1,Symbolic s2 -> Misc.string_eq  s1 s2
| (Concrete _,Symbolic _)
| (Symbolic _,Concrete _) -> false

