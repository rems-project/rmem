(*===============================================================================*)
(*                                                                               *)
(*                rmem executable model                                          *)
(*                =====================                                          *)
(*                                                                               *)
(*  This file is:                                                                *)
(*                                                                               *)
(*  Copyright Shaked Flur, University of Cambridge       2015-2017               *)
(*  Copyright Christopher Pulte, University of Cambridge 2017-2018               *)
(*                                                                               *)
(*  All rights reserved.                                                         *)
(*                                                                               *)
(*  It is part of the rmem tool, distributed under the 2-clause BSD licence in   *)
(*  LICENCE.txt.                                                                 *)
(*                                                                               *)
(*===============================================================================*)

open Printf
open Lem_basic_classes

(* open MachineDefUtils *)
open Events
open MachineDefTypes
open Types
(* open Model_aux *)

(* open BitwiseCompatibility *)

open Globals


type external_pps = {
  pp_colour_tran_id : Globals.ppmode -> string -> string;
  enlink : Globals.ppmode -> int -> string -> string;
  pp_flowing_event : Globals.ppmode -> flowing_event -> string;
}

(* ****************************************** *)
(* pp of a flowing storage subsystem ui state *)
(* ****************************************** *)

(*
0         1        2
|         |        |
|         |        |
|         |        |
|         ----------
|             |
|             |(3)
|             |
---------------
      |
      |(4)
      |


      |0 a:DMB.ST
      |0 a:WX x=1
       1234567890
*)

(************** *)

let list_max xs = List.fold_right max xs 0
let list_sum xs = List.fold_right (+) xs 0
let list_anti_hd xs = List.hd (List.rev xs)
let list_anti_tl xs = (List.rev (List.tl (List.rev xs)))
let rec list_make x n = if n=0 then [] else x::list_make x (n-1)


(* library of "lengthed strings" - strings with explicit notional
lengths (not including any colouring control characters...) *)

let rec lengthed_string_concat : (string*int) -> (string*int) list -> (string*int) =
  fun (s0,i0) sis ->
    match sis with
    | [] -> ("",0)
    | (s,i)::[] -> (s,i)
    | (s,i)::((_::_) as sis') -> let (s',i') = lengthed_string_concat (s0,i0) sis' in (s^s0^s',i+i0+i')

let lengthed_string_pad len (s,i) = let padding_len = max (len-i) 0 in (s^String.make padding_len ' ',i+padding_len)


let make_lengthed_string s = (s,String.length s)

let rec horizontal : ((string*int) list) list -> (string*int) list =
  function xss ->
    (* Printf.printf "Horizontal (%d): " (List.length xss);List.iter (function xs -> Printf.printf "\"\"%d, "  (List.length xs)) xss; Printf.printf "\n"; flush stdout;*)
    match xss with
    | [] -> []
    | []::_ -> []
    | _ ->
        let heads = List.map List.hd xss in
        let tails = List.map List.tl xss in
        lengthed_string_concat ("",0) heads :: horizontal tails


let lengthed_string_hv_pad len siss =
  let height = list_max (List.map List.length siss) in
  let siss' = List.map (function sis -> sis @ list_make ("",0) (height-List.length sis)) siss in
  let siss'' = List.map (function sis -> let len' = max len (list_max (List.map (fun (s,i)->i) sis)) in  List.map (lengthed_string_pad len') sis) siss' in
  horizontal siss''

let rec equi_pad : (string*int) list -> (string*int) list =
  function xs ->
    let len = list_max (List.map snd xs) in
    List.map (function (s,i) -> lengthed_string_pad len (s,i)) xs

(************* *)

type rendered_tree =
    { strings : (string*int) list;   (* all of length width *)
      width : int;
      stem : int; (* number of spaces before the stem *)
    }

let mk_rendered_tree ss stem =
{ strings = ss;
  width = snd (List.hd ss);
  stem = stem; }


let segment_height_min = 3
let event_width_min = 12

let rec height : flowing_tree -> (flowing_segment -> (('ts,'ss) MachineDefTypes.ui_trans list * flowing_event) list) -> int =
  fun tree lookup ->
    match tree with
    | FT_join(seg,trees) ->
        let segment_height = max segment_height_min (List.length (lookup seg)) in
        match trees with
        | [] -> 1 (* for thread id *) + segment_height
        | trees ->
            let heights = List.map (function tree' -> height tree' lookup) trees in
            list_max heights
            + 1 (* for horizontal line *)
            + segment_height (* for this segment *)



let pp_ui_transs m external_pps uits =
  let (s,i) = lengthed_string_concat
      (",",1)
      (List.map
         (function uit ->
           let (n,tr) = uit in
           let s = Printf.sprintf "%d" n in          (* ignore tr here *)
           (external_pps.enlink m n (external_pps.pp_colour_tran_id m s),String.length s))
         uits) in
  if i<=1 then lengthed_string_pad 2 (s,i)
  else (s^" ",i+1)


let render_lower_part : Globals.ppmode -> external_pps -> int (* height *) -> int (* stem indent *) -> (('ts,'ss) MachineDefTypes.ui_trans list * flowing_event) list -> (string*int) list =
  fun m external_pps h stem fes ->
    let lhs = make_lengthed_string (String.make stem ' ' ^ "|") in
    List.map
      (function (uits,fe) ->
        lengthed_string_concat
          ("",0)
          [lhs; pp_ui_transs m external_pps uits; make_lengthed_string (external_pps.pp_flowing_event m fe); make_lengthed_string "  "])
      fes
    @ list_make lhs (max 0 (h - List.length fes))

let rec render_tree : Globals.ppmode -> external_pps -> int (* total height to render at *) -> flowing_tree -> (flowing_segment -> (('ts,'ss) MachineDefTypes.ui_trans list*flowing_event) list) -> (flowing_segment -> thread_id) -> rendered_tree =
  fun m external_pps h (FT_join (seg,trees)) lookup seg_to_tid ->
      match trees with
      | [] ->
          let stem = 0 in
          let upper_part = [make_lengthed_string (Printf.sprintf "%d" (seg_to_tid seg) ^ String.make event_width_min ' ')] in
          let ss = equi_pad (upper_part @ render_lower_part m external_pps (h-1) stem (lookup seg)) in
          mk_rendered_tree ss stem
      | trees ->
          let sub_heights = List.map (function tree' -> height tree' lookup) trees in
          let sub_height = list_max sub_heights in
          let rendered_trees = List.map (function t -> render_tree m external_pps sub_height t lookup seg_to_tid) trees in
          let line_start = (List.hd rendered_trees).stem in
          let line_end = list_sum (list_anti_tl (List.map (function r -> r.width) rendered_trees)) + (list_anti_hd rendered_trees).stem + 1 in
          let line = make_lengthed_string (String.make  line_start ' ' ^ String.make  (line_end-line_start) '-') in
          let upper_part =
            horizontal (List.map (function r -> r.strings) rendered_trees)
            @ [line] in
          let stem = (line_start + line_end)/2 in
          let ss = equi_pad (upper_part @ render_lower_part m external_pps (h-sub_height-1) stem (lookup seg)) in
          mk_rendered_tree ss stem

let pp_rendered_tree m r =
  let s = String.concat "" (List.map (function (s,i) -> Printf.sprintf "%s\n" s) r.strings) in
  match m.Globals.pp_kind with
  | Ascii | Hash -> s
  | Html -> "<pre class='flowingstorage'>"^ s ^"</pre>"
  | Latex -> s
