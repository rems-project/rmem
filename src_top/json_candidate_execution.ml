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
   
let spaces (n : int) : string = String.make n ' '
let enclose (c : string) (s : string) = c ^ s ^ c
let enquote = enclose "\""


let print_header (indent : int) test_info =
  spaces indent ^ sprintf "{\n" ^
  spaces indent ^ sprintf "  \"auto generated\": \"by rmem\",\n" ^
  spaces indent ^ sprintf "  \"Revision\": \"%s\",\n" Versions.Rmem.describe ^
  spaces indent ^ sprintf "  \"Command line\": \"%s\",\n" (String.concat " " @@ Array.to_list Sys.argv) ^
  spaces indent ^ sprintf "  \"Model\": \"%s\",\n" (Model_aux.pp_model !Globals.model_params) ^
  spaces indent ^ sprintf "  \"Litmus hash\": \"%s\",\n" (try List.assoc "Hash" test_info.Test.info with Not_found -> "???") ^
  spaces indent ^ sprintf "}\n"


let print_list_body (indent : int)
      (f : int -> 'x -> string) (xs : 'x list) : string list = 
  List.map (f indent) xs

let print_set_body (indent : int)
      (f : int -> 'x -> string) (xs : 'x list) : string list = 
  let (_,pps) =
    List.fold_left
      (fun (hashes,pps) x ->
        let pp = f indent x in
        let hash = Digest.string pp in
        if Pset.mem hash hashes
        then (hashes,pps)
        else (Pset.add hash hashes, pp :: pps)
      )
      (Pset.empty String.compare,[])
      xs in
  List.rev pps

let print_list_or_set (set : bool) (indent : int) (sameline : bool)
      (f : int -> 'x -> string) (xs : 'x list) : string = 
  let body indent =
    if set
    then print_set_body indent f xs
    else print_list_body indent f xs in
  if sameline || xs = [] then
    spaces indent ^ "[" ^ (String.concat ", " (body 0)) ^ "]"
  else
    spaces indent ^ "[\n" ^ 
    String.concat ",\n" (body (indent+1)) ^ "\n" ^ 
    spaces indent ^ "]"
  
let print_list (indent : int) (sameline : bool)
      (f : int -> 'x -> string) (xs : 'x list) : string = 
  print_list_or_set false indent sameline f xs

let print_set (indent : int) (sameline : bool)
      (f : int -> 'x -> string) (xs : 'x list) : string = 
  print_list_or_set true indent sameline f xs

let print_eiid (indent : int) (eiid : Events.eiid) : string =
  let ((tid,ioid),eid) = eiid in
  spaces indent ^ sprintf "[%d,%d,%d]" tid ioid eid

let print_eiid_tuple (indent : int) ((e,e') : (eiid * eiid)) : string =
  print_list indent true print_eiid [e; e']

let print_id_rel (indent : int) (rel : (eiid*eiid) Pset.set) : string = 
  let rel : (eiid*eiid) list = Pset.elements rel in
  print_list indent false print_eiid_tuple rel

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


let print_acex (indent : int) (acex : acex) : string = 
  spaces indent ^ "{\n" ^ 
  spaces indent ^ "  \"events\":\n" ^ print_events (indent+4) acex.acex_events ^ ",\n" ^
  spaces indent ^ "  \"po\":\n" ^ print_id_rel (indent+4) acex.acex_po ^ ",\n" ^
  spaces indent ^ "  \"same-thread\":\n" ^ print_id_rel (indent+4) acex.acex_same_thread ^ ",\n" ^
  spaces indent ^ "  \"same-instr\":\n" ^ print_id_rel (indent+4) acex.acex_same_instr ^ ",\n" ^
  spaces indent ^ "  \"addr\":\n" ^ print_id_rel (indent+4) acex.acex_addr ^ ",\n" ^
  spaces indent ^ "  \"data\":\n" ^ print_id_rel (indent+4) acex.acex_data ^ ",\n" ^
  spaces indent ^ "  \"ctrl\":\n" ^ print_id_rel (indent+4) acex.acex_ctrl ^ ",\n" ^
  spaces indent ^ "  \"rf\":\n" ^ print_rf (indent+4) acex.acex_rf ^ ",\n" ^
  spaces indent ^ "  \"co\":\n" ^ print_id_rel (indent+4) acex.acex_co ^ "\n" ^ 
  spaces indent ^ "}"


let print_top test_info (acexs : acex list) : string = 
  "{\n" ^ 
  "  \"meta\":\n" ^ print_header 4 test_info ^ ",\n" ^
  "  \"executions\":\n" ^ print_set 4 false print_acex acexs ^ "\n" ^
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
  let json_out = open_out ((basename_in_dir test_info.Test.name) ^ ".json") in
  fprintf json_out "%s\n\n" (print_top test_info acexs); 
  close_out json_out


