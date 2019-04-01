(*===============================================================================*)
(*                                                                               *)
(*                rmem executable model                                          *)
(*                =====================                                          *)
(*                                                                               *)
(*  This file is:                                                                *)
(*                                                                               *)
(*  Copyright Shaked Flur, University of Cambridge       2017-2018               *)
(*  Copyright Jon French, University of Cambridge        2017-2018               *)
(*  Copyright Christopher Pulte, University of Cambridge      2017               *)
(*                                                                               *)
(*  All rights reserved.                                                         *)
(*                                                                               *)
(*  It is part of the rmem tool, distributed under the 2-clause BSD licence in   *)
(*  LICENCE.txt.                                                                 *)
(*                                                                               *)
(*===============================================================================*)


(* open Sail_impl_base
 * open Utils
 * open Fragments
 * open Events
 * open BasicTypes
 * open CandidateExecution *)


open Sail_impl_base
open CandidateExecution
open Events
open Printf
   
let spaces (n : int) : string = String.init n (fun _ -> ' ')
let enclose (c : string) (s : string) = c ^ s ^ c
let enquote = enclose "\""


let print_list (indent : int) (sameline : bool)
      (f : int -> 'x -> string) (xs : 'x list) : string = 
  if xs = [] then
    "[]"
  else if sameline then
    spaces indent ^ "[" ^ (String.concat ", " (List.map (f 0) xs)) ^ "]"
  else
    spaces indent ^ "[\n" ^ 
      String.concat ",\n" (List.map (f (indent+1)) xs) ^ "\n" ^ 
        spaces indent ^ "]"
  


let print_eiid (indent : int) ((tid,ioid),eid) =
  spaces indent ^ sprintf "[%d,%d,%d]" tid ioid eid

let print_id_rel (indent : int) rel =
  let rel = Pset.elements rel in
  print_list indent false
    (fun indent (e,e') -> print_list indent true print_eiid [e; e']) rel

let print_event_type indent e =
  let etype = match e with
  | ACEX_read rr -> stringFromReadKind rr.r_read_kind
  | ACEX_write w -> stringFromWriteKind w.w_write_kind
  | ACEX_barrier b -> stringFromBarrierKind b.b_barrier_kind
  in
  spaces indent ^ enquote etype
    

let print_fp (indent : int) (a,sz) = 
  spaces indent ^ 
    (sprintf "{\"addr\": %s, \"size\": %d}"
       (Nat_big_num.to_string (integer_of_address a)) sz)

let print_fp_of_event indent e =
  spaces indent ^ 
  match fp_of_event e with
  | Some fp -> sprintf "[ %s ]" (print_fp 0 fp)
  | None -> "[]"

let print_event indent e = 
  spaces indent ^
    (sprintf "{ \"id\": %s, \"type\": %s, \"fp\": %s }"
      (print_eiid 0 (eiid_of_acex_event e))
      (print_event_type 0 e)
      (print_fp_of_event 0 e)
    )

let print_events indent events = 
  print_list indent false print_event (Pset.elements events)

let print_rf_edge indent (fp,eiid,eiid') = 
  spaces indent ^ 
    sprintf "{ \"fp\": %s, \"write\": %s, \"read\": %s}"
      (print_fp 0 fp)
      (print_eiid 0 eiid)
      (print_eiid 0 eiid')

let print_rf indent rf = 
  let rf = Pset.elements rf in
  print_list indent false print_rf_edge rf


let print_acex (acex : acex) : string = 
  "{\n" ^ 
  "  \"events\":\n" ^ print_events 4 acex.acex_events ^ ",\n" ^
  "  \"po\":\n" ^ print_id_rel 4 acex.acex_po ^ ",\n" ^
  "  \"same-thread\":\n" ^ print_id_rel 4 acex.acex_same_thread ^ ",\n" ^
  "  \"same-instr\":\n" ^ print_id_rel 4 acex.acex_same_instr ^ ",\n" ^
  "  \"addr\":\n" ^ print_id_rel 4 acex.acex_addr ^ ",\n" ^
  "  \"data\":\n" ^ print_id_rel 4 acex.acex_data ^ ",\n" ^
  "  \"ctrl\":\n" ^ print_id_rel 4 acex.acex_ctrl ^ ",\n" ^
  "  \"rf\":\n" ^ print_rf 4 acex.acex_rf ^ ",\n" ^
  "  \"co\":\n" ^ print_id_rel 4 acex.acex_co ^ "\n" ^ 
  "}"
  

(* Copied from tikz.ml, should unify *)
let basename_in_dir (name: string) : string =
  begin match !Globals.generateddir with
  | None -> "out"
  | Some dir ->
      Filename.concat dir (Filename.basename name)
  end

(* let print_acexs (test_info : Test.info) (acexs : acex list) : unit = 
 *   let acexs_printed = String.concat "\n\n\n" (List.map print_acex acexs) in
 *   let tikz_out = open_out ((basename_in_dir test_info.Test.name) ^ ".json") in
 *   fprintf tikz_out "%s\n\n" acexs_printed;
 *   close_out tikz_out *)

let print_distinct_acexs (test_info : Test.info) (acexs : acex list) : unit = 
  let tikz_out = open_out ((basename_in_dir test_info.Test.name) ^ ".json") in

  let _ =
    List.fold_left
      (fun hashes acex ->
        let acex_printed = print_acex acex in
        let hash = Digest.string acex_printed in
        if Pset.mem hash hashes
        then hashes
        else (fprintf tikz_out "%s\n\n" acex_printed; 
              Pset.add hash hashes)
      )
      (Pset.empty String.compare)
      acexs
  in

  close_out tikz_out


