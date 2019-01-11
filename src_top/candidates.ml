(*====================================================================================================*)
(*                                                                                                    *)
(*                rmem executable model                                                               *)
(*                =====================                                                               *)
(*                                                                                                    *)
(*  This file is:                                                                                     *)
(*                                                                                                    *)
(*  Copyright Peter Sewell, University of Cambridge                                 2011-2012, 2014   *)
(*  Copyright Susmit Sarkar, University of St Andrews                               2011-2012, 2014   *)
(*  Copyright Luc Maranget, INRIA, Paris, France                                               2012   *)
(*  Copyright Sela Mador-Haim, University of Pennsylvania (when this work was done)       2011-2012   *)
(*  Copyright Ohad Kammar, University of Cambridge (when this work was done)              2013-2014   *)
(*  Copyright Shaked Flur, University of Cambridge                                        2016-2017   *)
(*  Copyright Dominic Mulligan, University of Cambridge (when this work was done)              2013   *)
(*                                                                                                    *)
(*  All rights reserved.                                                                              *)
(*                                                                                                    *)
(*  It is part of the rmem tool, distributed under the 2-clause BSD licence in                        *)
(*  LICENCE.txt.                                                                                      *)
(*                                                                                                    *)
(*====================================================================================================*)

open Valconstraints
open Valsolver
open MachineDefFreshIds
open MachineDefValue
open MachineDefTypes
open MachineDefInstructionSemantics
open MachineDefThreadSubsystem
open MachineDefAxiomaticCore

open Printf

let pset_to_list r = Pset.fold (fun x k -> x::k) r []

let rec option_map f xs = 
  match xs with 
  | [] -> [] 
  | x::xs -> 
      ( match f x with 
      | None -> option_map f xs 
      | Some x -> x :: (option_map f xs) ) 



(* ************************************************ *)
(* ***  pretty printing for abstract axiomatic model*)
(* ************************************************ *)

open MachineDefAxiomatic

type ppmode_ax = { ppm : Globals.ppmode;
                   prettyi : (MachineDefFreshIds.ioid * string) list;(* pretty instruction name*)
                   prettywi: (MachineDefFreshIds.ioid * string) list;(* pretty initial write name*)               }
  
let init_thread = 1000  (* PS: yuck *)

let sorted_instructions c =
  List.sort
    (fun i1 i2 -> 
      if thread_of_axiomatic_instruction i1 < thread_of_axiomatic_instruction i2 then -1 
      else if thread_of_axiomatic_instruction i1 > thread_of_axiomatic_instruction i2 then 1 
      else compare (ioid_of_axiomatic_instruction i1)  (ioid_of_axiomatic_instruction i2)) 
    (pset_to_list c.ace_instructions)


let pp_prettyi_of (c:axiomatic_candidate_execution) :(MachineDefFreshIds.ioid*string) list  = 
  let pretty_names = ["a";"b";"c";"d";"e";"f";"g";"h";   "m";"n";"o";"p";"q";"r";"s";"t"] in
  let pretty_id_next = ref 0 in
  let pp_pretty_id n = if n < List.length pretty_names then List.nth pretty_names n else "t"^string_of_int n in
  let next_pretty_id () = 
    let s = pp_pretty_id !pretty_id_next in
    pretty_id_next := 1 + !pretty_id_next;
    s in
  option_map
    (fun i -> 
      if thread_of_axiomatic_instruction i <> init_thread then
        Some (ioid_of_axiomatic_instruction i, next_pretty_id ())
      else
        None)
     (sorted_instructions c)


(*  let pretty_initial_write_names = ["i";"j";"k";"l";] in *)
  

let pp_pretty_ioid m ioid = 
  try List.assoc ioid m.prettyi with Not_found -> raise (Failure (Printf.sprintf "pp_pretty_ioid %d" ioid))

let pp_instruction_id m thread ioid =
  (if thread = init_thread 
  then 
    Printf.sprintf "Init"
  else 
(*    Printf.sprintf "%s:%d,%d" (pp_pretty_ioid m ioid) thread ioid)*)
    Printf.sprintf "%s" (pp_pretty_ioid m ioid))


let pp_instruction_id_mr m thread ioid =
  if thread = init_thread 
  then 
    Printf.sprintf "init"
  else 
    Printf.sprintf "%s,%d" (pp_pretty_ioid m ioid) thread





(*
match m.Globals.pp_kind with 
| Ascii | Html ->
| Latex -> 
*)

let pp_write m w = 
  Printf.sprintf "%s:W %s=%s" (pp_instruction_id m w.w_thread w.w_ioid) (Pp.pp_value w.w_addr) (Pp.pp_value w.w_value)

let pp_wad m wad = 
  Printf.sprintf "%s:W %s " (pp_instruction_id m wad.wad_thread wad.wad_ioid) (Pp.pp_value wad.wad_addr) (*(Pp.pp_value w.w_value)*)

let pp_read m rd = 
  Printf.sprintf "%s:R %s=%s" (pp_instruction_id m rd.rd_thread rd.rd_ioid) (Pp.pp_value rd.rd_addr) (Pp.pp_value rd.rd_value)

let pp_barrier m b = 
  Printf.sprintf "%s:%s" (pp_instruction_id m b.b_thread b.b_ioid) (Pp.pp_barrier_type m.ppm b.b_barrier_type) 
    
let pp_other m s o = 
  Printf.sprintf "%s:%s" (pp_instruction_id m o.o_thread o.o_ioid) s



let pp_write_mr m w = 
  Printf.sprintf "%s:W %s=%s" (pp_instruction_id_mr m w.w_thread w.w_ioid) (Pp.pp_value w.w_addr) (Pp.pp_value w.w_value)

let pp_wad_mr m wad = 
  Printf.sprintf "%s:W %s " (pp_instruction_id_mr m wad.wad_thread wad.wad_ioid) (Pp.pp_value wad.wad_addr) (*(Pp.pp_value w.w_value)*)

let pp_read_mr m rd = 
  Printf.sprintf "%s:R %s=%s" (pp_instruction_id_mr m rd.rd_thread rd.rd_ioid) (Pp.pp_value rd.rd_addr) (Pp.pp_value rd.rd_value)

let pp_barrier_mr m b = 
  Printf.sprintf "%s:%s" (pp_instruction_id_mr m b.b_thread b.b_ioid) (Pp.pp_barrier_type m.ppm b.b_barrier_type) 
    
let pp_other_mr m s o = 
  Printf.sprintf "%s:%s" (pp_instruction_id_mr m o.o_thread o.o_ioid) s


let pp_axiomatic_instruction m i =
  match i with
  | AI_Write_memory w -> pp_write m w
  | AI_Read_memory rd -> pp_read m rd 
  | AI_Barrier b -> pp_barrier m b 
  | AI_Isync o -> pp_other m "Isync"  o 
  | AI_Branch o -> pp_other m "Branch"  o
  | AI_Other o -> pp_other m "Other"  o 

let pp_axiomatic_instruction_mr m i =
  match i with
  | AI_Write_memory w -> pp_write_mr m w
  | AI_Read_memory rd -> pp_read_mr m rd 
  | AI_Barrier b -> pp_barrier_mr m b 
  | AI_Isync o -> pp_other_mr m "Isync"  o 
  | AI_Branch o -> pp_other_mr m "Branch"  o
  | AI_Other o -> pp_other_mr m "Other"  o 

let pp_axiomatic_instruction_id m i =
  match i with
  | AI_Write_memory w ->pp_instruction_id m w.w_thread w.w_ioid
  | AI_Read_memory rd -> pp_instruction_id m rd.rd_thread rd.rd_ioid
  | AI_Barrier b ->     pp_instruction_id m b.b_thread b.b_ioid
  | AI_Isync o ->       pp_instruction_id m o.o_thread o.o_ioid
  | AI_Branch o ->      pp_instruction_id m o.o_thread o.o_ioid
  | AI_Other o ->       pp_instruction_id m o.o_thread o.o_ioid

let pp_axiomatic_instruction_id_mr m i =
   pp_axiomatic_instruction_id m i


let pp_instruction_reln chan m name r = 
  fprintf chan "\t%s := " name;
  Pset.iter 
    (fun (i1,i2) -> fprintf chan "%s -> %s; " (pp_axiomatic_instruction m i1) (pp_axiomatic_instruction m i2)) r;
  fprintf chan "\n"

let pp_instruction_reln_mr chan m name r = 
  fprintf chan "%s = {" name;
  Pset.iter 
    (fun (i1,i2) -> fprintf chan "%s -> %s; " (pp_axiomatic_instruction_id_mr m i1) (pp_axiomatic_instruction_id_mr m i2)) r;
  fprintf chan "}\n"


let pp_instruction_noadj_reln chan m adjacencies name r = 
  let r' = List.filter (fun (i1,i2) -> not (List.mem (i1,i2) adjacencies)) (pset_to_list r) in
  match r' with
  | [] -> 
      ()
  | r' -> 
      fprintf chan "%s: " name;
      List.iter 
        (fun (i1,i2) -> fprintf chan "%s->%s; " (pp_axiomatic_instruction_id m i1) (pp_axiomatic_instruction_id m i2)) r';
      fprintf chan "\n"


let instruction_source c a =
  List.assoc (ioid_of_axiomatic_instruction a)
    c.ace_ins_source 

let pp_axiomatic_candidate_execution chan m c = 
  fprintf chan "\tNumber of instructions = %d\n" (Pset.cardinal c.ace_instructions);
  List.iter
    (fun a -> fprintf chan "\t\t%15s  %s\n"
        (Pp.pp_instruction (instruction_source c a))
        (pp_axiomatic_instruction m a))
    (sorted_instructions c);
  pp_instruction_reln chan m "po" c.ace_po;
  pp_instruction_reln chan m "rf" c.ace_rf;
  pp_instruction_reln chan m "rf_init" c.ace_rf_init;
  pp_instruction_reln chan m "co" c.ace_co;
  pp_instruction_reln chan m "datadep" c.ace_datadep;
  pp_instruction_reln chan m "addrdep" c.ace_addrdep;
  pp_instruction_reln chan m "ctrldep" c.ace_ctrldep;
  fprintf chan "\n\n"


(* machine-readable axiomatic candidate execution printing *)

let pp_axiomatic_candidate_execution_mr chan m c = 
  fprintf chan "threads = { ";
  Pset.iter
    (fun tid -> fprintf chan "%d; " tid) c.ace_threads;
  fprintf chan "}\n";
  fprintf chan "program-size = %d\n" (Pset.cardinal c.ace_instructions);
  fprintf chan "instructions = {\n";
  List.iter
    (fun a -> fprintf chan "%s;\n"  (pp_axiomatic_instruction_mr  m a))
    (sorted_instructions c);
  fprintf chan "}\n";

  fprintf chan "ins_source = [\n";
  List.iter
    (fun a -> fprintf chan "%s,  \"%s\" ;\n"  (pp_axiomatic_instruction_id_mr  m a)  (Pp.pp_instruction (instruction_source c a)))
    (sorted_instructions c);
  fprintf chan "]\n";


  fprintf chan "initial_memory_state = {";
  Pset.iter
    (fun w -> fprintf chan "%s; "  (pp_write_mr  m w)) c.ace_initial_memory_state;
  fprintf chan "}\n";

  fprintf chan "initial_register_state = TODO\n";
  fprintf chan "final_register_state = TODO\n";

  pp_instruction_reln_mr chan m "po" c.ace_po;
  pp_instruction_reln_mr chan m "rf" c.ace_rf;
  pp_instruction_reln_mr chan m "rf_init" c.ace_rf_init;
  pp_instruction_reln_mr chan m "co" c.ace_co;
  pp_instruction_reln_mr chan m "datadep" c.ace_datadep;
  pp_instruction_reln_mr chan m "addrdep" c.ace_addrdep;
  pp_instruction_reln_mr chan m "ctrldep" c.ace_ctrldep;
  fprintf chan "\n\n"


let pp_axiomatic_candidate_execution' chan m0 c = 
  (* for non-axiomatic-model usage, we suppress Isync/ISB events, instead calculating ctrlisync,
     and suppress R->R ctrldep edges  *)
  let ctrlisync = MachineDefAxiomaticCore.ctrlisync_of c in
  let c = MachineDefAxiomaticCore.de_isync c in
  let m  = { ppm = m0; 
             prettyi = pp_prettyi_of c;
             prettywi = [] } in
  let pp_instructions_of_thread tid =
    let sorted_instructions = List.sort 
        (fun i1 i2 -> if i1=i2 then 0 else if Pset.mem (i1,i2) c.ace_po then -1 else 1)
        (List.filter 
           (function i -> thread_of_axiomatic_instruction i = tid) 
           (pset_to_list c.ace_instructions)) in
    let adjacencies = 
      let rec g is = 
        match is with
        | i1::i2::is' -> (i1,i2):: g (i2::is')
        | [i] -> []
        | [] -> [] in
      g sorted_instructions in
    let edges i1 i2 = 
      (*(if Pset.mem (i1,i2) c.ace_po then ["po"] else []) 
         @*) (if Pset.mem (i1,i2) c.ace_rf then ["rf"] else []) 
      @ (if Pset.mem (i1,i2) c.ace_co then ["co"] else []) 
      @ (if Pset.mem (i1,i2) c.ace_datadep then ["data"] else []) 
      @ (if Pset.mem (i1,i2) c.ace_addrdep then ["addr"] else []) 
      @ (if Pset.mem (i1,i2) c.ace_ctrldep then ["ctrl"] else []) 
      @ (if Pset.mem (i1,i2) ctrlisync then ["ctrlisync"] else []) in
    let rec f is ypos = (* calculate ASCII y position *)
      match is with
      | i1::i2::is' -> let es=edges i1 i2 in (ypos,i1,es):: f (i2::is') (ypos+1)
      | [i] -> [(ypos,i,[])]
      | [] -> [] in
    let ppd_thread_horiz = 
      Printf.sprintf "Thread %d: %s\n" tid
        (String.concat "" (List.map (fun (ypos,i,es) -> (pp_axiomatic_instruction m i) ^ match es with | [] -> "  " | es -> "  <"^String.concat "," es^">  ") (f sorted_instructions 0))) in
    let ppd_thread_vert = 
      List.flatten 
        ([Pp_flowing.make_lengthed_string (Printf.sprintf "Thread %d" tid)] ::
         List.map 
           (fun (ypos,i,es) -> 
             [ Pp_flowing.make_lengthed_string (pp_axiomatic_instruction m i) ] @
             match es with | [] -> [] | es -> [Pp_flowing.make_lengthed_string ("<"^String.concat "," es^">")])
           (f sorted_instructions 0)) in
    (ppd_thread_horiz,ppd_thread_vert,adjacencies) in

  let threads = List.sort (Pervasives.compare) (pset_to_list c.ace_threads) in
  let ppd_threads0 = List.map pp_instructions_of_thread threads in

  let adjacencies = List.flatten (List.map (function (ppd1,ppd2,adj)->adj) ppd_threads0) in

  let ppd_threads = String.concat "" (List.map (function (ppd1,ppd2,adj) -> ppd1)  ppd_threads0) in
  fprintf chan "%s\n" ppd_threads;

  let ppd_threads_v = Pp_flowing.lengthed_string_hv_pad 12 
     (List.map (function (ppd1,ppd2,adj) -> ppd2)  ppd_threads0) in

  fprintf chan "%s\n\n" (String.concat "\n" (List.map fst ppd_threads_v));


(*        
a:Wx1    -> 
<po>    /
b:Wy1 --

a:Wx1 <- -> 
<po>    X
b:Wy1 -- --   
*)

(*  fprintf chan "\tNumber of instructions = %d\n" (Pset.cardinal c.ace_instructions);
  List.iter
    (fun a -> fprintf chan "\t\t%15s  %s\n" (Pp.pp_instruction (instruction_source c a)) (pp_axiomatic_instruction m a))
    (sorted_instructions c);
*)
 (* pp_instruction_noadj_reln m adjacencies "po" c.ace_po;*)
  pp_instruction_noadj_reln chan m adjacencies "rf" c.ace_rf;
  pp_instruction_noadj_reln chan m adjacencies "rf_init" c.ace_rf_init;
  pp_instruction_noadj_reln chan m adjacencies "co" c.ace_co;
  pp_instruction_noadj_reln chan m adjacencies "datadep" c.ace_datadep;
  pp_instruction_noadj_reln chan m adjacencies "addrdep" c.ace_addrdep;
  pp_instruction_noadj_reln chan m adjacencies "ctrldep" c.ace_ctrldep;
  pp_instruction_noadj_reln chan m adjacencies "ctrlisync" ctrlisync;
  fprintf chan "\n\n"



(* ************************************************ *)
(* ** pretty-printing for Sela axiomatic model      *)
(* ************************************************ *)


let pp_event m e = 
  match e with
  | Ax_Satisfy_read_from_storage_subsystem rd -> Printf.sprintf "Satisfy read %s" (pp_read m rd)
  | Ax_Write_address_determined wad -> Printf.sprintf "Write address determined %s" (pp_wad m wad)
  | Ax_Commit_write w -> Printf.sprintf "Commit write %s" (pp_write m w)
  | Ax_Commit_read rd -> Printf.sprintf "Commit read %s" (pp_read m rd)
  | Ax_Commit_barrier b -> Printf.sprintf "Commit barrier %s" (pp_barrier m b)
  | Ax_Commit_isync o -> Printf.sprintf "Commit isync %s" (pp_other m "Isync" o)
  | Ax_Commit_branch o -> Printf.sprintf "Commit branch %s" (pp_other m "Branch" o)
  | Ax_Commit_other o -> Printf.sprintf "Commit other %s" (pp_other m "Other" o)
  | Ax_Propagate_write (w,tid) -> Printf.sprintf "Propagate write %s to %d" (pp_write m w)  tid
  | Ax_Propagate_barrier (b,tid) -> Printf.sprintf "Propagate barrier %s to %d" (pp_barrier m b) tid


let pp_event_reln chan m name r = 
  fprintf chan "\t%s := \n" name;
  Pset.iter 
    (fun (e1,e2) -> fprintf chan "\t\t\t%s -> %s;\n" (pp_event m e1) (pp_event m e2)) r
(*  fprintf chan "\n"*)



let pp_axiomatic_calc chan m ac = 
  fprintf chan "\tAxiomatic model:\n";
  pp_instruction_reln chan m "fr"   ac.ac_fr;
  pp_instruction_reln chan m "comm" ac.ac_comm;
  Printf.sprintf "\tac_acyclic_comm: %b\n" ac.ac_acyclic_comm              ;
  Printf.sprintf "\tevents:\n";
  Pset.iter
    (fun e -> fprintf chan "\t\t%s\n" (pp_event m e))
    ac.ac_events;
  pp_event_reln chan m "flocal                  " ac.ac_flocal                    ;
  pp_event_reln chan m "read_from_initiated     " ac.ac_read_from_initiated        ;
  pp_event_reln chan m "events_before_commit  " ac.ac_events_before_commit   ;
  pp_event_reln chan m "propagate_after_commit" ac.ac_propagate_after_commit  ;
  pp_event_reln chan m "communication           " ac.ac_communication             ;
  pp_event_reln chan m "evord_base              " ac.ac_evord_base                ;
  pp_event_reln chan m "evord                   " ac.ac_evord                     ;

  pp_event_reln chan m "fbefore_edges           " ac.ac_fbefore_edges             ;
  pp_event_reln chan m "fafter_edges            " ac.ac_fafter_edges              ;

  pp_instruction_reln chan m "fbefore_cord"      ac.ac_fbefore_cord;
  pp_instruction_reln chan m "cord        "      ac.ac_cord;

  Printf.sprintf "\tacyclic_evord: %b\n" ac.ac_acyclic_evord              ;
  Printf.sprintf "\tacyclic_cord: %b\n" ac.ac_acyclic_cord             ;
  Printf.sprintf "\tuniproc: %b\n" ac.ac_uniproc             ;
  Printf.sprintf "\tconsistent: %b\n" ac.ac_consistent              ;
  fprintf chan "\n\n"

(* ********************************************************* *)
(* relation auxiliaries                                      *)
(* ********************************************************* *)



let rec remove_duplicates l =
  match l with
  | x::xs -> 
      if (List.mem x xs) then remove_duplicates xs else x::(remove_duplicates xs)
  | [] -> []

exception TransitiveCycle

let transitive_reduction r = 
  let transitive_pairs = 
    List.flatten 
      (List.map 
         (fun (a1,a2) -> 
           option_map (fun (a1',a2') -> if a2=a1' then Some (a1,a2') else None) r)
         r) in
    (* a partial check for cycles *)
  if List.exists (fun (a1,a2)->a1=a2) (r @ transitive_pairs) then 
    raise TransitiveCycle;
  List.filter (fun (a1,a2) -> not (List.mem (a1,a2) transitive_pairs)) r 


let transitive_closure r = 
  let rec f r = 
    let two_step_edges =
      List.flatten 
        (List.map (fun (n,n') -> 
          option_map (fun (n'',n''') -> 
            if n'=n'' then Some (n,n''') else None) r) r) in
    let new_two_step_edges = remove_duplicates (List.filter (function e -> not (List.mem e r)) two_step_edges) in
    match new_two_step_edges with
    | [] -> r
    | _  -> f (r @ new_two_step_edges) in
  let r' = f r in
  if List.exists (fun (n1,n2)->n1=n2) r' then 
    raise TransitiveCycle (*(Failure "internal error: transitive_closure invoked on a cyclic relation")*);
  r'

let is_acyclic r =
  try 
    let _ = transitive_closure r in
    true
  with
    TransitiveCycle -> false

let is_transitive r = 
  let two_step_edges =
    List.flatten 
      (List.map (fun (n,n') -> 
        option_map (fun (n'',n''') -> 
          if n'=n'' then Some (n,n''') else None) r) r) in
  List.for_all (function (n,n''') -> List.mem (n,n''') r) two_step_edges
    
let rec equivalence_classes xs per = 
  match xs with
  | [] -> []
  | x::xs -> 
       let xs_equiv,xs_inequiv = List.partition (function x' -> per x x') (x::xs) in
       match xs_equiv with 
       | [_] -> xs_equiv:: equivalence_classes xs_inequiv per (* singleton case *)
       | _::_::_ -> xs_equiv ::equivalence_classes xs_inequiv per
       | _ -> raise (Failure "equivalence_classes")



(* ************************************************ *)
(* graph output                                     *)
(* ************************************************ *)

(* instructions *)

let ppg_instruction_id i = Printf.sprintf "%d_%d" (thread_of_axiomatic_instruction i) (ioid_of_axiomatic_instruction i)

let ppg_instruction_node_name i = 
  "\"" ^
  (  match i with
  | AI_Write_memory w -> Printf.sprintf "Write_%s" (ppg_instruction_id i)
  | AI_Read_memory rd -> Printf.sprintf "Read_%s" (ppg_instruction_id i)
  | AI_Barrier b ->      Printf.sprintf "Barrier_%s" (ppg_instruction_id i)
  | AI_Isync o ->        Printf.sprintf "Isync_%s" (ppg_instruction_id i)
  | AI_Branch o ->       Printf.sprintf "Branch_%s" (ppg_instruction_id i)
  | AI_Other o ->        Printf.sprintf "Other_%s" (ppg_instruction_id i)  )^
  "\""

(* let xscale = 2.0 *)
(* let yscale = 1.0 *)
(* let xorigin = 1.0 *)
(* let yorigin = 1.0 *)


let same_thread_instruction i1 i2 = (thread_of_axiomatic_instruction i1 = thread_of_axiomatic_instruction i2) 

let same_thread_event e1 e2 = (thread_of e1 = thread_of e2) 

let ppg_instruction_node m ac i col fill = 
  Printf.sprintf "%s %s[color=%s][fontsize=\"10\"][shape=\"box\"][label=\"%s\"];\n"  (* [pos=\"%f,%f!\ "] *)
    (ppg_instruction_node_name i) 
    (if fill then "[style=\"setlinewidth(3)\"]" else "")
    col (* (match k with LogState.Allow -> "red" | LogState.Forbid ->"green" | _ -> "magenta") *)
(*      (xscale *. (float_of_int (thread_of_axiomatic_instruction i) +. xorigin)) *)
(*      (yscale *. (float_of_int (ioid_of_axiomatic_instruction i ) +. yorigin)) *)
    ((pp_axiomatic_instruction m i)^"\\n"^(Pp.pp_instruction (instruction_source ac.ac_ace i)))

let ppg_instruction_edge ppg_instruction_node_name i1 i2 col label = 
  Printf.sprintf "%s -> %s [color=\"%s\"][labelfontcolor=\"%s\"][label=\"%s\"];\n"
    (ppg_instruction_node_name i1)
    (ppg_instruction_node_name i2)
    col
    col
    label

let ppg_instruction_edges ppg_instruction_node_name ppg_instruction_edge is col label = 
  Printf.sprintf "/* %s */\n" label 
  ^ String.concat "" (List.map 
    (fun (i1,i2) -> Printf.sprintf "%s" (ppg_instruction_edge ppg_instruction_node_name i1 i2 col label)) is)

let ppg_instruction_reln ppg_instruction_node_name ppg_instruction_edge col label r = 
  ppg_instruction_edges ppg_instruction_node_name ppg_instruction_edge (pset_to_list r) col label

let ppg_instruction_reln_tran ppg_instruction_node_name ppg_instruction_edge col label r = 
  let r = pset_to_list r in
  let r' =
    if not(is_transitive r) then raise (Failure "WARNING: EDGES CYCLIC OR NOT TRANSITIVE")
    else
      transitive_reduction r in
  ppg_instruction_edges ppg_instruction_node_name ppg_instruction_edge r' col label 



(* events *)

let ppg_event_id e = Printf.sprintf "%d_%d" (thread_of e) (ioid_of e)

let ppg_event_node_name e = 
  "\"" ^
  (match e with
  | Ax_Satisfy_read_from_storage_subsystem rd -> Printf.sprintf "Satisfy_read_%s" (ppg_event_id e)
  | Ax_Write_address_determined wad -> Printf.sprintf "Write_address_determined_%s" (ppg_event_id e)
  | Ax_Commit_write w -> Printf.sprintf "Commit_write_%s"  (ppg_event_id e)
  | Ax_Commit_read rd -> Printf.sprintf "Commit_read_%s" (ppg_event_id e)
  | Ax_Commit_barrier b -> Printf.sprintf "Commit_barrier_%s" (ppg_event_id e)
  | Ax_Commit_isync o -> Printf.sprintf "Commit_isync_%s" (ppg_event_id e)
  | Ax_Commit_branch o -> Printf.sprintf "Commit_branch_%s" (ppg_event_id e)
  | Ax_Commit_other o -> Printf.sprintf "Commit_other_%s" (ppg_event_id e)
  | Ax_Propagate_write (w,tid) -> Printf.sprintf "Propagate_write_%s_to_%d" (ppg_event_id e)  tid
  | Ax_Propagate_barrier (b,tid) -> Printf.sprintf "Propagate_barrier_%s_to_%d" (ppg_event_id e) tid)^
  "\""

let ppg_event_node m e col fill = 
  Printf.sprintf "%s %s[color=%s][fontsize=\"10\"][label=\"%s\"];\n" 
    (ppg_event_node_name e) 
    (if fill then "[style=\"setlinewidth(3)\"]" else "")
    col (* (match k with LogState.Allow -> "red" | LogState.Forbid ->"green" | _ -> "magenta") *)
    (pp_event m e)

let ppg_event_edge e1 e2 col label = 
  Printf.sprintf "%s -> %s [label=\"%s\"][color=\"%s\"][labelfontcolor=\"%s\"];\n"
    (ppg_event_node_name e1)
    (ppg_event_node_name e2)
    label
    col
    col

let ppg_event_edges ppg_event_edge es col label = 
  Printf.sprintf "/* %s */\n" label 
  ^   String.concat "" (List.map 
    (fun (e1,e2) -> Printf.sprintf "%s" (ppg_event_edge e1 e2 col label)) es)

let ppg_event_reln ppg_event_edge col label r = 
  ppg_event_edges ppg_event_edge (pset_to_list r) col label

let ppg_event_reln_tran ppg_event_edge col label r = 
  let r = pset_to_list r in
  let r' =
    if not(is_transitive r) then raise (Failure "WARNING: EDGES CYCLIC OR NOT TRANSITIVE")
    else
      transitive_reduction r in
  ppg_event_edges ppg_event_edge r' col label 


(* the whole graph *)

let cluster_events = true

let ppg_axiomatic_calc m ac legend =
  (*  pp_instruction_reln m "comm" ac.ac_comm;*)
  let r = pset_to_list ac.ac_evord in
(*   let evord_acyclic = is_acyclic r in *)
(*   let evord_tran = evord_acyclic && is_transitive r in *)
  "digraph G {\n" 
  ^ " splines=true;\n"
  ^ " overlap=false;\n"
(*  ^ " ranksep = 1.0;\n"  *)
  ^ " label = \""
  ^legend
  ^(if ac.ac_acyclic_evord then "" else " NB: evord cyclic")
  ^(if ac.ac_acyclic_cord then "" else " NB: cord cyclic")
  ^(if ac.ac_uniproc then "" else " NB: violate uniproc")
(*  ^(if evord_acyclic && not(evord_tran) then  " WARNING: evord not transitive" else "")*)
  ^"\";\n"  
  (* instruction part *)
  ^ Printf.sprintf "/* instructions */\n"
  ^ (let is_equivs = equivalence_classes (pset_to_list ac.ac_ace.ace_instructions) same_thread_instruction in
     (String.concat "" (List.map 
      (fun is -> 
        "subgraph cluster_i_"^string_of_int (thread_of_axiomatic_instruction (List.hd is))^ " {\n"
        ^"style=rounded;\n"
        ^ "color=blue;\n"
        ^ Printf.sprintf "label=\"Thread %d\";\n" (thread_of_axiomatic_instruction (List.hd is))
        ^ String.concat "" (List.map (fun i -> Printf.sprintf "%s" (ppg_instruction_node m ac i "black" false)) is)
        ^ "}\n") is_equivs)))
  ^ ppg_instruction_reln_tran ppg_instruction_node_name ppg_instruction_edge "black"  "po"      ac.ac_ace.ace_po
  ^ ppg_instruction_reln_tran ppg_instruction_node_name ppg_instruction_edge "brown"  "co"      ac.ac_ace.ace_co
  ^ ppg_instruction_reln      ppg_instruction_node_name ppg_instruction_edge "red"  "rf"      ac.ac_ace.ace_rf
  ^ ppg_instruction_reln      ppg_instruction_node_name ppg_instruction_edge "red"  "rf_init" ac.ac_ace.ace_rf_init
  ^ ppg_instruction_reln      ppg_instruction_node_name ppg_instruction_edge "red"  "fr"     ac.ac_fr
  ^ ppg_instruction_reln      ppg_instruction_node_name ppg_instruction_edge "indigo" "data" ac.ac_ace.ace_datadep
  ^ ppg_instruction_reln      ppg_instruction_node_name ppg_instruction_edge "indigo" "addr" ac.ac_ace.ace_addrdep
  ^ ppg_instruction_reln      ppg_instruction_node_name ppg_instruction_edge "indigo" "ctrl" ac.ac_ace.ace_ctrldep
  (* cord part over instructions*)                  
  ^ ppg_instruction_reln      ppg_instruction_node_name ppg_instruction_edge "green" "fbefore_cord" ac.ac_fbefore_cord
  (* event part *)
  ^ Printf.sprintf "/* events */\n"
  ^ (if cluster_events then 
    (let es_equivs = equivalence_classes (pset_to_list ac.ac_events) same_thread_event in
    (String.concat "" 
       (List.map 
          (fun es -> 
            "subgraph cluster_e_"^string_of_int (thread_of (List.hd es))^ " {\n"
            ^"style=rounded;\n"
            ^ "color=blue;\n"
            ^ Printf.sprintf "label=\"Thread %d\";\n" (thread_of (List.hd es))
            ^ String.concat "" (List.map (fun e -> Printf.sprintf "%s" (ppg_event_node m e "black" false)) es)
            ^ "}\n") es_equivs)))
  else
    (Pset.fold
       (fun e k -> Printf.sprintf "%s" (ppg_event_node m e "black" false) ^ k)
       ac.ac_events ""))
  ^ ppg_event_reln  ppg_event_edge     "black" "flocal"     ac.ac_flocal                    
  ^ ppg_event_reln  ppg_event_edge     "black" "rfi"        ac.ac_read_from_initiated        
  ^ ppg_event_reln  ppg_event_edge     "black" "ebc"        ac.ac_events_before_commit
  ^ ppg_event_reln  ppg_event_edge     "black" "pac"        ac.ac_propagate_after_commit  
  ^ ppg_event_reln  ppg_event_edge     "black" "comm"       ac.ac_communication             
(*  ^ ppg_event_reln      "black" "evord_base" ac.ac_evord_base                *)
(*   ^  *)
(*     (if evord_acyclic && evord_tran then  *)
(*       ppg_event_reln_tran "black"     "evord"      ac.ac_evord   (\* should be tran? *\) *)
(*     else *)
(*       ppg_event_reln      "black"     "evord"      ac.ac_evord   (\* should be tran? *\) *)
(*     ) *)
(*  Printf.sprintf "\tacyclic_evord: %b\n" ac.ac_acyclic_evord              ^*)
(*  Printf.sprintf "\tconsistent: %b\n" ac.ac_consistent              ;*)
  ^ ppg_event_reln  ppg_event_edge     "green" "fbefore_edges" ac.ac_fbefore_edges
  ^ ppg_event_reln  ppg_event_edge     "green" "fafter_edges" ac.ac_fafter_edges
  ^ "}\n"


type event_kind = Exe | Com | Prop of int


let ppg_axiomatic_calc_2 m ac legend =
  (*  pp_instruction_reln m "comm" ac.ac_comm;*)
  let r = pset_to_list ac.ac_evord in
(*   let evord_acyclic = is_acyclic r in *)
(*   let evord_tran = evord_acyclic && is_transitive r in *)

  let fontsize = "fontsize=\"20\"" in

  let ppg_event_node_0 m e shape label  = 
    Printf.sprintf "%s [shape=%s][%s][fontname=\"Helvetica\"][label=\"%s\"];\n" 
      (ppg_event_node_name e) 
      shape
      fontsize
      label in
      (*  (pp_event m e) *)

  let ppg_event_node_2 e = 
    match e with
    | Ax_Satisfy_read_from_storage_subsystem rd -> ppg_event_node_0 m e "diamond" "\\\\Lx"
    | Ax_Write_address_determined wad -> ppg_event_node_0 m e "diamond" "\\\\Lx" 
    | Ax_Commit_write w ->   ppg_event_node_0 m e "box" "\\\\Lm"
    | Ax_Commit_read rd ->   ppg_event_node_0 m e "box" "\\\\Lm"
    | Ax_Commit_barrier b -> ppg_event_node_0 m e "box" "\\\\Lm"
    | Ax_Commit_isync o ->   ppg_event_node_0 m e "box" "\\\\Lm"
    | Ax_Commit_branch o ->  ppg_event_node_0 m e "box" "\\\\Lm"
    | Ax_Commit_other o ->   ppg_event_node_0 m e "box" "\\\\Lm"
    | Ax_Propagate_write (w,tid) -> ppg_event_node_0 m e "ellipse" (Printf.sprintf "\\\\Lv%d" tid)
    | Ax_Propagate_barrier (b,tid) -> ppg_event_node_0 m e "ellipse" (Printf.sprintf "\\\\Lv%d" tid) in

  let ppg_event e = ppg_event_node_2 e in

  let ppg_instruction_node_name_2 i = Printf.sprintf "cluster_i_%s" (ppg_instruction_id i) in

  let kind_of_event e = match e with 
    | Ax_Satisfy_read_from_storage_subsystem rd -> Exe
    | Ax_Write_address_determined wad -> Exe
    | Ax_Commit_write w ->   Com
    | Ax_Commit_read rd ->   Com
    | Ax_Commit_barrier b -> Com
    | Ax_Commit_isync o ->   Com
    | Ax_Commit_branch o ->  Com
    | Ax_Commit_other o ->   Com
    | Ax_Propagate_write (w,tid) -> Prop tid
    | Ax_Propagate_barrier (b,tid) -> Prop tid in


  let is_exe_event e = kind_of_event e = Exe in
  let is_commit_event e = kind_of_event e = Com in
  let is_prop_event e = match kind_of_event e with Prop _ -> true | _ -> false in

  let commit_event_of_instruction i = 
    List.hd 
      (List.filter (function e -> is_commit_event e &&  ioid_of e = ioid_of_axiomatic_instruction i) (pset_to_list ac.ac_events)) in

  let first_and_last_event_of_instruction i = 
    let exe = (List.filter (function e -> is_exe_event e &&  ioid_of e = ioid_of_axiomatic_instruction i) (pset_to_list ac.ac_events)) in
    let com = (List.filter (function e -> is_commit_event e &&  ioid_of e = ioid_of_axiomatic_instruction i) (pset_to_list ac.ac_events)) in
   let prop = (List.filter (function e -> is_prop_event e &&  ioid_of e = ioid_of_axiomatic_instruction i) (pset_to_list ac.ac_events)) in
   (match exe,com with  (* first *)
     e::_,_ -> e
   | [],e::[] -> e),
   (match exe,com,prop with  (* second *)
     [_],e::_,_ -> e
   | [],[_],e::_ -> e),
   (match com,prop with  (* last *)
   | _    ,e::_ -> e
   | e::[],[]   -> e) in

  let first_event_of_instruction i = let (e1,_,_) = first_and_last_event_of_instruction i in e1 in
  let second_event_of_instruction i = let (_,e1,_) = first_and_last_event_of_instruction i in e1 in

  let instructions_of_tid tid = List.sort (fun i1 i2 -> Pervasives.compare (ioid_of_axiomatic_instruction i1) (ioid_of_axiomatic_instruction i2)) (List.filter (function i -> thread_of_axiomatic_instruction i = tid) (pset_to_list ac.ac_ace.ace_instructions)) in

  let first_instruction_of_thread tid = List.hd (instructions_of_tid tid) in

  let threads = (*List.rev*) (List.sort (Pervasives.compare) (pset_to_list ac.ac_ace.ace_threads)) in

  let thread_pairs = let rec f tids = match tids with | tid::tid'::tids' -> (tid,tid'):: f (tid'::tids') | [_] -> [] | [] -> [] in f threads in

  let ppg_thread_pair_edge (tid1,tid2) = 
    let i1 = first_instruction_of_thread tid1 in
    let i2 = first_instruction_of_thread tid2 in
    Printf.sprintf "%s -> %s %s[ltail=\"%s\",lhead=\"%s\"][%s][color=\"%s\"][labelfontcolor=\"%s\"][label=\"%s\"][len=2.0]%s;\n"
      (ppg_event_node_name (first_event_of_instruction i1))
      (ppg_event_node_name (second_event_of_instruction i2))
      "[style=invis]"
      (ppg_instruction_node_name i1)
      (ppg_instruction_node_name i2)
      fontsize
      "green" 
      "green" 
      ""
      (Printf.sprintf "[constraint=%b]" true) in

  let ppg_thread_pair_edges = String.concat "" (List.map ppg_thread_pair_edge thread_pairs) in


  let ppg_instruction_edge_2 const ppg_instruction_node_name i1 i2 col label = 
    Printf.sprintf "%s -> %s [ltail=\"%s\",lhead=\"%s\"][%s][color=\"%s\"][fontname=\"Helvetica\"][labelfontcolor=\"%s\"][label=\"%s\"]%s;\n"
      (ppg_event_node_name (commit_event_of_instruction i1))
      (ppg_event_node_name (commit_event_of_instruction i2))
      (ppg_instruction_node_name i1)
      (ppg_instruction_node_name i2)
      fontsize
      col
      col
      label 
      (Printf.sprintf "[constraint=%b]" const) in

  let ppg_instruction_edge_3 const ppg_instruction_node_name i1 i2 col label = 
    let (_,_,e1) = first_and_last_event_of_instruction i1 in
    let (e2,_,_) = first_and_last_event_of_instruction i2 in
    Printf.sprintf "%s -> %s [style=invis][ltail=\"%s\",lhead=\"%s\"][%s][color=\"%s\"][fontname=\"Helvetica\"][labelfontcolor=\"%s\"][label=\"%s\"]%s;\n"
      (ppg_event_node_name e1)
      (ppg_event_node_name e2)
      (ppg_instruction_node_name i1)
      (ppg_instruction_node_name i2)
      fontsize
      col
      col
      label 
      (Printf.sprintf "[constraint=%b]" const) in


  let ppg_event_edge_2 const  e1 e2 col label = 
    Printf.sprintf "%s -> %s [label=\"%s\"][%s][color=\"%s\"][fontname=\"Helvetica\"][labelfontcolor=\"%s\"]%s;\n"
      (ppg_event_node_name e1)
      (ppg_event_node_name e2)
      label
      fontsize
      col
      col
      (Printf.sprintf "[constraint=%b]" const) in

  let ppg_instruction_cluster i = 
    let events_for_i = List.filter (function e -> ioid_of e = ioid_of_axiomatic_instruction i) (pset_to_list ac.ac_events) in
    let sorted_events_for_i = 
      List.sort (fun e1 e2 -> match kind_of_event e1, kind_of_event e2 with
      | Exe,Com -> -1
      | Com,Prop _ -> -1
      | Exe,Prop _ -> -1
      | Com,Exe -> 1
      | Prop _,Com -> 1
      | Prop _,Exe -> 1
      | Prop tid1,Prop tid2-> compare tid1 tid2
      | _,_ -> compare e1 e2) events_for_i in

    Printf.sprintf "subgraph %s {\n"  (ppg_instruction_node_name_2 i) 
    ^ Printf.sprintf "style=solid;\n"
(*    ^ Printf.sprintf "rankdir=\"LR\";\n"*)
    ^ Printf.sprintf "label=\"\\\\myL{%s}\";\n" ((pp_axiomatic_instruction m i) (*^"\\n"^(Pp.pp_instruction (instruction_source ac.ac_ace i))*))
    ^ Printf.sprintf "%s;\n" fontsize
    ^ String.concat "\n" (List.map ppg_event sorted_events_for_i)
    ^ Printf.sprintf "}\n" in

  let ppg_thread_cluster tid = 
    Printf.sprintf "subgraph cluster_t_%d {\n" tid
    ^ Printf.sprintf "style=dotted;\n"
    ^ Printf.sprintf "rankdir=\"TB\";\n"
    ^ Printf.sprintf "%s;\n" fontsize
    ^ Printf.sprintf "label=\"\\\\myL{Thread %d}\";\n" tid
    ^ String.concat "\n" (List.map ppg_instruction_cluster (instructions_of_tid tid))
    ^ Printf.sprintf "}\n" in

  let ppg_graph = 
      "digraph G {\n" 
    ^ " compound = true;\n"
    ^ " forcelabels = true;\n"
(*    ^ " remincross = true;\n"*)
(*    ^ " concentrate = true;\n"*)
    ^ fontsize ^ ";\n"
    ^ " fontname=\"Helvetica\";\n"
    ^ " labelfontname=\"Helvetica\";\n"
    ^ " splines=true;\n"
    ^ " overlap=false;\n"
    ^ " rankdir=\"TB\";\n"
    ^ " ranksep = 0.2;\n"  
    ^ " label = \"\\\\myL{"
    ^legend
        (* ^(if ac.ac_acyclic_evord then "" else " NB: evord cyclic") *)
        (* ^(if ac.ac_acyclic_cord then "" else " NB: cord cyclic") *)
        (* ^(if ac.ac_uniproc then "" else " NB: violate uniproc") *)
(*  ^(if evord_acyclic && not(evord_tran) then  " WARNING: evord not transitive" else "")*)
    ^"}\";\n"  
    ^ String.concat "\n" (List.map ppg_thread_cluster threads)

    ^ ppg_thread_pair_edges

(*   (\* instruction part *\) *)
(*   ^ Printf.sprintf "/* instructions */\n" *)
(*   ^ (let is_equivs = equivalence_classes (pset_to_list ac.ac_ace.ace_instructions) same_thread_instruction in *)
(*      (String.concat "" (List.map  *)
(*       (fun is ->  *)
(*         "subgraph cluster_t_"^string_of_int (thread_of_axiomatic_instruction (List.hd is))^ " {\n" *)
(*         ^"style=dotted;\n" *)
(* (\*        ^ "color=blue;\n"*\) *)
(*         ^ Printf.sprintf "label=\"Thread %d\";\n" (thread_of_axiomatic_instruction (List.hd is)) *)
(*         ^ String.concat "" (List.map (fun i -> Printf.sprintf "%s" (ppg_instruction_cluster m ac i)) is) *)
(*         ^ "}\n") is_equivs))) *)
  ^ ppg_instruction_reln_tran ppg_instruction_node_name_2 (ppg_instruction_edge_3  true) "black"  "po"      ac.ac_ace.ace_po

  ^ ppg_instruction_reln_tran ppg_instruction_node_name_2 (ppg_instruction_edge_2 false) "brown"  "co"      ac.ac_ace.ace_co
(*  ^ ppg_instruction_reln ppg_instruction_node_name_2      (ppg_instruction_edge_2 false) "red"  "rf"      ac.ac_ace.ace_rf*)
 (* ^ ppg_instruction_reln ppg_instruction_node_name_2      (ppg_instruction_edge_2 false) "red"  "rf_init" ac.ac_ace.ace_rf_init*)
(*  ^ ppg_instruction_reln ppg_instruction_node_name_2      (ppg_instruction_edge_2 false) "red"  "fr"     ac.ac_fr*)

(*
  ^ ppg_instruction_reln ppg_instruction_node_name_2      (ppg_instruction_edge_2 false) "indigo" "data" ac.ac_ace.ace_datadep
  ^ ppg_instruction_reln ppg_instruction_node_name_2      (ppg_instruction_edge_2 false) "indigo" "addr" ac.ac_ace.ace_addrdep
  ^ ppg_instruction_reln ppg_instruction_node_name_2      (ppg_instruction_edge_2 false) "indigo" "ctrl" ac.ac_ace.ace_ctrldep
*)


  ^ ppg_event_reln  (ppg_event_edge_2 true)     "black" "" (*"ebc"*)        ac.ac_events_before_commit
  ^ ppg_event_reln  (ppg_event_edge_2 true)     "black" "" (*"pac"*)        ac.ac_propagate_after_commit

  ^ ppg_event_reln  (ppg_event_edge_2 false)     "black" "\\\\Ll"     ac.ac_flocal
(*  ^ ppg_event_reln  (ppg_event_edge_2 false)     "black" "rfi"        ac.ac_read_from_initiated*)
  ^ ppg_event_reln  (ppg_event_edge_2 false)     "orange" "\\\\Lc"       ac.ac_communication


(* (\*  ^ ppg_event_reln      "black" "evord_base" ac.ac_evord_base                *\) *)
(* (\*   ^  *\) *)
(* (\*     (if evord_acyclic && evord_tran then  *\) *)
(* (\*       ppg_event_reln_tran "black"     "evord"      ac.ac_evord   (\\* should be tran? *\\) *\) *)
(* (\*     else *\) *)
(* (\*       ppg_event_reln      "black"     "evord"      ac.ac_evord   (\\* should be tran? *\\) *\) *)
(* (\*     ) *\) *)
(* (\*  Printf.sprintf "\tacyclic_evord: %b\n" ac.ac_acyclic_evord              ^*\) *)
(* (\*  Printf.sprintf "\tconsistent: %b\n" ac.ac_consistent              ;*\) *)
   ^ ppg_event_reln (ppg_event_edge_2 false)      "green" "\\\\Lb" (* fbefore-edges *) ac.ac_fbefore_edges 
   ^ ppg_event_reln (ppg_event_edge_2 false)      "green" "\\\\La" (* fafter-edges *) ac.ac_fafter_edges 
(*   ^ "}\n" *)

    ^"}\n" in 

  ppg_graph




let dump_axiomatic_calc m ac legend = 
  let c = open_out_gen [Open_creat; Open_append] 0o644 "out.dot" in
  Printf.fprintf c "%s" (ppg_axiomatic_calc_2 m ac legend);
  let _ = close_out c in
  
  let _ = if (Globals.get_our_runopts ()).Globals.run_dot then 
    Sys.command "dot -Tps out.dot > out.ps" else 0 in
  ()




(*
----------------------------------
let pp_graph  = 

  let family = List.filter (function (t,xs,k) -> List.hd xs = tfam) normalised_kinds  in

  let edges = option_map (fun x->x) (List.flatten (List.map  (function (t,xs,k) -> List.map (function (t',xs',k') -> if test_order xs xs' then Some (t,t') else None) family) family)) in

  let edges' =
    if not(is_transitive edges) then raise (Failure "WARNING: EDGES NOT TRANSITIVE")
    else
      transitive_reduction edges in

  let clusters = non_singleton_equivalence_classes family test_same_dep in

  let cluster_id = ref 0 in

  let pp_cluster nodes = 
    cluster_id := 1 + !cluster_id;
    "subgraph cluster"^string_of_int !cluster_id^ " {\n"
   ^"style=rounded;\n"
   ^"color=blue;\n"
   ^String.concat "" (List.map (function (t,xs,k) -> sprintf "%s;\n" (pp_node_name t)) nodes)
   ^"}\n" in

  let kinds = List.map (function (t,xs,k)->(t,k)) family in
  let kind_of t = List.assoc t kinds in
  let is_forbid_lower_bound a =
    (* t is Forbid *)
    kind_of a = LogState.Forbid &&
    (* no immediate lower-neighbour of t is Forbid *)
    not (List.exists (function (a1,a2) -> kind_of a1=LogState.Forbid && a2=a) edges') &&
    (* everything in the up-closed region above t is Forbid *)
    (List.for_all (function (a1,a2) -> if a1=a then kind_of a2=LogState.Forbid else true) edges) in


  let pp_graph =
    ^ String.concat "" (List.map pp_cluster clusters) 
    ^ String.concat "" (List.map (function (t,xs,k) -> pp_node t k (is_forbid_lower_bound t)) family)
    ^ String.concat "" (List.map (function (a1,a2) -> pp_edge a1 a2) edges')
  ^ "}\n\n" in

  pp_graph 

*)





(* ************************************************ *)
(* enumeration of abstract axiomatic execution cands*)
(* ************************************************ *)

type execution_result =
    {instructions:axiomatic_instruction list;
     ins_source:(ioid * instruction) list;
     vconstraint: vconstraint;
     rf : (axiomatic_instruction * axiomatic_instruction) list;
     rf_init : (axiomatic_instruction * axiomatic_instruction) list;
     co : (axiomatic_instruction * axiomatic_instruction) list;
     po : (axiomatic_instruction * axiomatic_instruction) list;
     final_regs : (thread_id * reg * value) list;
   }

let unit_execution_result =
  { instructions = []; ins_source = []; vconstraint = ctrue; rf = []; rf_init = []; co = []; po = []; final_regs = []; }

let combine_execution_results res1 res2 =
  { instructions = res1.instructions @ res2.instructions;
    ins_source = res1.ins_source @ res2.ins_source;
    vconstraint = 
    begin 
      match (res1.vconstraint,res2.vconstraint) with
      | Conj d1,Conj d2 -> Conj (d1 @ d2)
    end;
    rf = res1.rf @ res2.rf;
    rf_init = res1.rf_init @ res2.rf_init;
    co = res1.co @ res2.co;
    po = res1.po @ res2.po;
    final_regs = res1.final_regs @ res2.final_regs;
  }

let sequence_execution_results res1 res2 =
  { instructions = res1.instructions @ res2.instructions;
    ins_source = res1.ins_source @ res2.ins_source;
    vconstraint = 
    begin 
      match (res1.vconstraint,res2.vconstraint) with
      | Conj d1,Conj d2 -> Conj (d1 @ d2)
    end;
    rf = res1.rf @ res2.rf;
    rf_init = res1.rf_init @ res2.rf_init;
    co = res1.co @ res2.co;
    po = res1.po @ res2.po @ 
    (List.flatten (List.map (fun i1 -> List.map (fun i2 -> (i1,i2)) res2.instructions) res1.instructions));
    final_regs = res1.final_regs @ res2.final_regs;
  }

let subst_instruction s i = 
  match i with
  | AI_Write_memory w ->
      AI_Write_memory {w with
                    w_addr = subst_var s w.w_addr;
                    w_value = subst_var s w.w_value;
                     weiid = {w.weiid with 
                               weiid_addr = subst_var s w.weiid.weiid_addr;
                               weiid_value = subst_var s w.weiid.weiid_value;
                             }
                     }
  | AI_Read_memory rd ->
      AI_Read_memory {rd with
                    rd_addr = subst_var s rd.rd_addr;
                    rd_value = subst_var s rd.rd_value;
                    rdeiid = {rd.rdeiid with
                               rdeiid_addr = subst_var s rd.rdeiid.rdeiid_addr;
                               rdeiid_value = subst_var s rd.rdeiid.rdeiid_value;
                             }
                    }
  | _ -> i

let subst_execution_result s res =
  {instructions = List.map (subst_instruction s) res.instructions;
   ins_source = res.ins_source;
   vconstraint = subst_vconstraint s res.vconstraint;
   rf = List.map (fun (i1,i2) -> subst_instruction s i1,subst_instruction s i2) res.rf;
   rf_init = List.map (fun (i1,i2) -> subst_instruction s i1,subst_instruction s i2) res.rf_init;
   co = List.map (fun (i1,i2) -> subst_instruction s i1,subst_instruction s i2) res.co;
   po = List.map (fun (i1,i2) -> subst_instruction s i1,subst_instruction s i2) res.po;
   final_regs = List.map (fun (tid,r,v) -> (tid,r,subst_var s v)) res.final_regs;
 }

let fold_cross xss sc s =
  let rec fold_rec s ys xss = match xss with
  | [] -> sc ys s
  | xs :: xss ->
      List.fold_left
	(fun s x -> fold_rec s (x :: ys) xss)
	s xs in
  fold_rec s [] (List.rev xss)

let rec fold_all_perms xs sc s = 
  match xs with
  | [] -> sc [] s
  | [x] -> sc [x] s
  | x :: xs ->
      let proc_pxs pxs s_pxs =
	let rec insert zs ys =
	  match ys with
	  | [] -> sc (zs @ [x]) s_pxs
	  | y :: ys -> 
	      sc (zs @ [x] @ (y :: ys))
		(insert (zs @ [y]) ys)
	in
	insert [] pxs
      in
      fold_all_perms xs proc_pxs s

let fold_perms_cross xss sc s =
  let rec fold_rec s ys xss = match xss with
  | [] -> sc ys s
  | xs :: xss ->
      let proc_pxs pxs s_pxs =
	fold_rec s_pxs (pxs :: ys) xss
      in
      fold_all_perms xs proc_pxs s in
  fold_rec s [] (List.rev xss)

let writes_per_loc res = 
  let store_acts = 
    List.filter
      (fun a -> 
	match a with
	| AI_Write_memory _ -> true
	| _ -> false) res.instructions in
  let stores_by_loc =
    List.fold_left 
      (fun sl s ->
	let l = 
	  match s with 
	  | AI_Write_memory {w_addr = l} -> l
	  | _ -> failwith "No location for picked out store action"
	in
	begin 
	  try 
	    let s_at_l = List.assoc l sl 
	    in (l,s::s_at_l) :: (List.remove_assoc l sl)
	  with Not_found -> (l,[s]) :: sl
	end
      ) [] store_acts 
  in
  stores_by_loc

let get_rfm_poss res st = 
  let load_acts = 
    List.filter
      (fun a -> 
	match a with
	| AI_Read_memory _ -> true
	| _ -> false) res.instructions in
  let init_w_instructions = 
    Pset.fold 
      (fun w k ->
        AI_Write_memory w :: k) (st.storage_subsystem.writes_seen) [] in
  let poss = 
    List.map
      (fun ra -> 
	match ra with
	| AI_Read_memory {rd_addr = raddr; rd_value = rval} ->
	    let poss_writes = 
	      Model_aux.option_map
		(fun wa ->
		  match wa with
		  | AI_Write_memory {w_addr = waddr; w_value = wval} ->
                             Some (wa,[(raddr,waddr);(rval,wval)]) 
		  | _ -> None) (res.instructions @ init_w_instructions) in
	    List.map 
	      (fun (wa,cs) -> ((Some wa,ra),cs)) poss_writes
	| _ -> failwith "Non-load in load position")
      load_acts in
  poss

let rec find_last_reg_write ts instrs r i =
  match i.prev with
  | None -> (* No register write in po-prefix *)
      ts.MachineDefTypes.initial_register_state r
  | Some (i'',_) ->
      begin
	try
	  let iprev = 
	    Pset.choose 
	      (Pset.filter (fun ip -> ip.instance_ioid = i'') instrs)
	  in
	  if Pset.exists (fun r' -> r = r') iprev.regs_out
	  then (* iprev is the instruction that will feed this read *)
	    val_written_to_register iprev.behaviour r
	  else find_last_reg_write ts instrs r iprev
	with Not_found ->
	  (* No register write in po-prefix, predecessor aborted *)
          ts.initial_register_state r
      end
          
let eval_instr ihere instrs ts sc s =
  let rec evaluate_behaviour ibeh reads constrs =
    match ibeh with
    | Read_reg (r,v) :: ibeh' -> 
        let val_written = find_last_reg_write ts instrs r ihere in
        let new_constrs = match constrs with
        | Conj ds -> Conj (C (Valconstraints.Eq (v,val_written)) :: ds) in
        evaluate_behaviour ibeh' reads new_constrs
    | Read_mem (a,v) :: ibeh' -> 
        let new_reads = 
          Pset.add 
            {rd_thread = ts.thread; 
                          rd_ioid = ihere.instance_ioid;
                          rdeiid = {rdeiid_thread = ts.thread;
                                    rdeiid_ioid = ihere.instance_ioid;
                                    rdeiid_addr = a;
                                    rdeiid_value = v;};
                          rd_addr = a;
                          rd_value = v;
                          (*rd_isacquire = false;*)}
            reads in
        evaluate_behaviour ibeh' new_reads constrs
    | Binop (v1, op, v2, v3) :: ibeh' ->
        let new_constrs = 
          match constrs with 
          | Conj ds -> Conj (C (EqBinOp (v1,v2,op,v3)) :: ds) in
        evaluate_behaviour ibeh' reads new_constrs
    | Unop (v1, op, v2) :: ibeh' -> 
        let new_constrs = 
          match constrs with 
          | Conj ds -> Conj (C (EqUnOp (v1,op,v2)) :: ds) in
        evaluate_behaviour ibeh' reads new_constrs
    | _ -> (ibeh,reads,constrs)
  in 
  let (fully_evaluated,reads,constrs) = 
    evaluate_behaviour ihere.behaviour.remaining (Pset.empty Pervasives.compare) ctrue in
  let ev_behaviour = {ihere.behaviour with remaining = fully_evaluated} in
  let action = 
    if Pset.cardinal reads > 0 then 
      AI_Read_memory (Pset.choose reads)
    else if will_mem_write ev_behaviour then
      AI_Write_memory (Pset.choose (mem_writes_of ts.thread ihere.instance_ioid ev_behaviour))
    else if will_barrier ev_behaviour then
      AI_Barrier (Pset.choose (barriers_of ts.thread ihere.instance_ioid ev_behaviour))
    else if will_isync ev_behaviour then
      AI_Isync { o_thread = ts.thread; o_ioid= ihere.instance_ioid}
    else if will_branch ev_behaviour then
      AI_Branch { o_thread = ts.thread; o_ioid= ihere.instance_ioid}
    else 
      AI_Other { o_thread = ts.thread; o_ioid= ihere.instance_ioid}
  in
  sc {instructions=[action];ins_source=[(ihere.instance_ioid,ihere.instance_instruction)];vconstraint = constrs;rf=[];rf_init=[];co=[];po=[]; final_regs=[];} s

let rec eval_instrs ihere instrs ts sc s =
  let nexts = Pset.fold
      (fun i k -> 
        match i.prev with 
        | Some (ioid,rb) -> if ioid = ihere.instance_ioid then Pset.add (i,rb) k else k 
        | _ -> k) instrs (Pset.empty (fun (i1,rb1) (i2,rb2) -> Pervasives.compare i1.instance_ioid i2.instance_ioid)) in
  if will_cond_branch ihere.behaviour
  then
    let vbranch = Pset.choose (cond_branch_on_values ihere.behaviour) in
    let ifzero = 
      Pset.fold 
        (fun (i,rb) k -> match rb with
        | IfZero v -> Pset.add (v,i) k
        | _ -> k) nexts 
        (Pset.empty (fun (v1,i1) (v2,i2) -> 
          Pervasives.compare (v1,i1.instance_ioid) (v2,i2.instance_ioid))) in
    let ifnonzero = 
      Pset.fold 
        (fun (i,rb) k -> match rb with
        | IfNonZero v -> Pset.add (v,i) k
        | _ -> k) nexts 
        (Pset.empty (fun (v1,i1) (v2,i2) ->
          Pervasives.compare (v1,i1.instance_ioid) (v2,i2.instance_ioid))) in
    let s_ifzerosuccs =
      if Pset.cardinal ifzero > 0 then 
        Pset.fold 
        (fun (vnext,inext) s ->
          eval_instr ihere instrs ts 
            (fun res1 s1 ->
              eval_instrs inext instrs ts
                (fun res2 s2 -> 
                  let combined = sequence_execution_results res1 res2 in
                  let combined' = 
                    {combined with
                     vconstraint = 
                     match combined.vconstraint with
                     | Conj ds -> Conj ((C (Valconstraints.Eq (vnext,Rigid (Concrete Nat_big_num.zero)))) :: ds)} in
                  sc combined' s2) s1) s
        ) ifzero s 
      else eval_instr ihere instrs ts 
          (fun res s -> 
            let vconstraint' = match res.vconstraint with
            | Conj ds -> Conj ((C (Valconstraints.Eq (vbranch,Rigid (Concrete Nat_big_num.zero)))) :: ds) in
            let res' = {res with vconstraint = vconstraint'} in
            sc res' s) 
          s
    in
    if Pset.cardinal ifnonzero > 0 then 
      Pset.fold 
        (fun (vnext,inext) s ->
          eval_instr ihere instrs ts 
            (fun res1 s1 ->
              eval_instrs inext instrs ts
                (fun res2 s2 -> 
                  let combined = sequence_execution_results res1 res2 in
                  let combined' = 
                    {combined with
                     vconstraint = 
                     match combined.vconstraint with
                     | Conj ds -> Conj ((C (Valconstraints.Neq (vnext,Rigid (Concrete Nat_big_num.zero)))) :: ds)} in
                  sc combined' s2) s1) s
        ) ifnonzero s_ifzerosuccs     
    else eval_instr ihere instrs ts 
        (fun res s -> 
          let vconstraint' = match res.vconstraint with
          | Conj ds -> Conj ((C (Valconstraints.Neq (vbranch,Rigid (Concrete Nat_big_num.zero)))) :: ds) in
          let res' = {res with vconstraint = vconstraint'} in
          sc res' s) 
        s_ifzerosuccs
  else
    if Pset.cardinal nexts > 0 then 
      Pset.fold 
        (fun (inext,rb) s ->
          eval_instr ihere instrs ts 
            (fun res1 s1 ->
              eval_instrs inext instrs ts
                (fun res2 s2 -> 
                  let combined = sequence_execution_results res1 res2 in
                  sc combined s2) s1) s
        ) nexts s 
    else eval_instr ihere instrs ts sc s

let eval_thread ts sc s =
  let instrs = Pset.union ts.committed_instructions ts.in_flight_instructions in
  let init_i = Pset.choose 
      (Pset.filter 
         (fun i -> match i.prev with None -> true | _ -> false) 
         instrs) in
  eval_instrs init_i instrs ts sc s

let add_final_condition res st regs = 
  let reg_values = 
    Model_aux.map_set 
      (fun (tid,r) ->
        let po_maximal_in_res = 
          List.find 
            (fun ilast ->
              thread_of_axiomatic_instruction ilast = tid &&
              not (List.exists
                     (fun inext ->
                       List.mem (ilast,inext) res.po) res.instructions))
            res.instructions in
        let ts = (st.thread_states tid) in
        let instrs = Pset.union ts.committed_instructions ts.in_flight_instructions in
        let maximals = 
          Pset.filter
            (fun i -> i.instance_ioid = ioid_of_axiomatic_instruction po_maximal_in_res)
            instrs in
        let final = Pset.choose maximals in
        let rval = 
          if Pset.mem r final.regs_out then
            val_written_to_register final.behaviour r 
          else
            find_last_reg_write ts instrs r final
        in
        (tid,r,rval)
      ) regs in
  {res with final_regs = Pset.elements reg_values}

(* TODO - this should mostly (all?) go away, into code that makes a Sela candidate execution from an abstract axiomatic execution candidate *)
(* let instruction_to_axiomatic_event i rf =  *)
(*   match i with *)
(*   | Write_memory w -> *)
(*       MDA.Ax_Commit_write w *)
(*   | Read_memory rd -> *)
(*       let write_feeding =  *)
(*         try  *)
(*           match fst (List.find (fun (w,r) -> r = i) rf) with  *)
(*           | Write_memory w -> w  *)
(*           | _ -> failwith "Inconsistent rf"  *)
(*         with Not_found -> failwith "Bad rf calculated" in *)
(*       MDA.Ax_Satisfy_read_from_storage_subsystem *)
(*         {rr_thread = rd.rd_thread; *)
(*          rr_ioid = rd.rd_ioid; *)
(*          rr_eiid = {reiid_thread = rd.rd_thread;reiid_ioid = rd.rd_ioid;reiid_addr = rd.rd_addr}; *)
(*          rr_write = write_feeding;} *)
(*   | Barrier b -> *)
(*       MDA.Ax_Commit_barrier b *)
(*   | Isync (tid,ioid) -> *)
(*       MDA.Ax_Commit_isync {MDA.o_thread = tid;MDA.o_ioid = ioid} *)
(*   | Branch (tid,ioid) -> *)
(*       MDA.Ax_Commit_branch {MDA.o_thread = tid;MDA.o_ioid = ioid} *)
(*   | OtherInstruction (tid,ioid) -> *)
(*       MDA.Ax_Commit_other {MDA.o_thread = tid;MDA.o_ioid = ioid} *)

let find_ax_inst_of_ii i axinstrs =
  List.find (fun i1 -> ioid_of_axiomatic_instruction i1 = i.instance_ioid) axinstrs

let ii_reln_to_ax_inst_reln r axinstrs =
  Pset.fold
    (fun (ii1,ii2) k ->
      try 
        Pset.add (find_ax_inst_of_ii ii1 axinstrs,find_ax_inst_of_ii ii2 axinstrs) k
      with Not_found -> k)
    r (Pset.empty Pervasives.compare)

let execution_result_to_axiomatic_candidate_execution st res =
  let ace0 = 
    { ace_threads = st.storage_subsystem.threads;
      ace_instructions = Pset.from_list Pervasives.compare res.instructions;
      ace_ins_source = res.ins_source;
      ace_initial_memory_state = st.storage_subsystem.writes_seen;
      ace_initial_register_state = 
      (fun tid -> (st.thread_states tid).initial_register_state);
      ace_final_register_state =
      List.fold_right
        (fun (tid,r,v) k ->
          fun tid' r' ->
            if tid = tid' && r = r' then Some v else k tid' r')
        res.final_regs (fun tid r -> None);
      ace_po = Pset.from_list Pervasives.compare res.po;
      ace_rf = Pset.from_list Pervasives.compare res.rf;
      ace_rf_init = Pset.from_list Pervasives.compare res.rf_init;
      ace_co = Pset.from_list Pervasives.compare res.co;
      ace_datadep = Pset.fold
        (fun tid k ->
          Pset.union (ii_reln_to_ax_inst_reln (datadep_reln_of (Globals.get_our_runopts ()).Globals.model_params.t (st.thread_states tid)) res.instructions) k)
        (st.storage_subsystem.threads) (Pset.empty Pervasives.compare);
      ace_addrdep = Pset.fold
        (fun tid k ->
          Pset.union (ii_reln_to_ax_inst_reln (addrdep_reln_of (Globals.get_our_runopts ()).Globals.model_params.t (st.thread_states tid)) res.instructions) k)
        (st.storage_subsystem.threads) (Pset.empty Pervasives.compare);
      ace_ctrldep = Pset.fold
        (fun tid k ->
          Pset.union (ii_reln_to_ax_inst_reln (ctrldep_reln_of (Globals.get_our_runopts ()).Globals.model_params.t (st.thread_states tid)) res.instructions) k)
        (st.storage_subsystem.threads) (Pset.empty Pervasives.compare);
(*    MDA.events = Pset.from_list Pervasives.compare
      (List.map (fun i -> instruction_to_axiomatic_event i res.rf) res.instructions);*)
    } in
  MachineDefAxiomaticCore.de_other ace0




let all_axiomatic_candidates test st thread_regs =
  let proc_raw_cand res s_raw_cand =
    let res = add_final_condition res st thread_regs in
    let proc_sub_first vcw_fst m_fst sub s_sub =
      let res_s = subst_execution_result sub res in

      let rfm_poss = get_rfm_poss res_s st in

      let count_rf_poss = 
        List.fold_left 
          (fun k rf_poss -> k * (List.length rf_poss))
          1 rfm_poss in

      let () = add_mark () in

      let proc_rfm rfcand s_rfm = 
        let () = undo_until_mark () in

	let opt_rf,new_eqss = List.split rfcand in
        let rf = Model_aux.option_map (fun (wopt,r) -> match wopt with None -> None | Some w -> Some (w,r)) opt_rf in
	let new_eqs = List.flatten new_eqss in

	let proc_sub_with_rfm vcw_rfm m_rfm sub_rfm s_srfm =
	  let res_s_rfm = subst_execution_result sub_rfm res_s in

	  let rf_f = List.map (fun (i1,i2) -> subst_instruction sub_rfm i1,subst_instruction sub_rfm i2) rf in
          (* P hack to remove rf edges from the initial writes, to fit axiomatic model *)
          let (rf_f,rf_f_init) = List.partition (fun (AI_Write_memory w,AI_Read_memory r) -> w.w_thread <> init_thread) rf_f in

	  let locs,stores_per_loc = List.split (writes_per_loc res_s_rfm) in

	  let proc_mo mos_list s_mo =
	    let rec list_to_total_order xs =
	      match xs with
	      | [] -> []
	      | x :: xs -> 
		  (List.map (fun y -> (x,y)) xs) @ list_to_total_order xs
	    in
	    let mo_rels = List.map list_to_total_order mos_list in
	    let mo_rel = List.flatten mo_rels in

            {res_s_rfm with rf = rf_f; rf_init = rf_f_init; co = mo_rel; } :: s_mo 
          in
	  fold_perms_cross stores_per_loc proc_mo s_srfm
	in
	solve_more_eqs vcw_fst m_fst new_eqs proc_sub_with_rfm (fun es s -> s) s_rfm
      in
      fold_cross rfm_poss proc_rfm s_sub
    in
    solve res.vconstraint proc_sub_first 
      (fun es s -> s) 
      s_raw_cand 
  in
  let cands = 
    (Pset.fold
      (fun tid k ->
        fun res2 ->
          let ts = (st.thread_states tid) in
          eval_thread ts 
            (fun res1 ->  k (combine_execution_results res1 res2)) 
      )
      (st.storage_subsystem.threads) proc_raw_cand) unit_execution_result [] in
  let cands =
    List.map (fun res -> execution_result_to_axiomatic_candidate_execution st res) cands in
  cands

  (* TODO: do something with ax_cand_executions, eg print it *)


let process_all_axiomatic_candidates test st thread_regs cand_matches_constraint =
  let cands = all_axiomatic_candidates test st thread_regs in
  let name = Test.simple_name test in 
  let _ =
    if !Globals.debug >= 0 then
      ignore (Sys.command "rm -rf out.dot") in
  begin
    let chan = stdout in
    fprintf chan "Axiomatic candidate executions\n";
(*     if !Globals.debug >= 0 then*)
    fprintf chan "Candidates = %d\n" (List.length cands);
    ignore (List.fold_right
      (fun c k ->
        begin 
          let m  = { ppm = Globals.ppmode_default;
                     prettyi = pp_prettyi_of c;
                     prettywi = [] } in
          if !Globals.debug >= 0 then
            begin 
              fprintf chan "Test: \"%s\"  Candidate %d\n" name k;
              fprintf chan "constraint-relevant = %s\n"
                (if cand_matches_constraint c then "yes" else "no");
              pp_axiomatic_candidate_execution_mr chan m c;
              (*pp_axiomatic_candidate_execution m c;*)
              (* pp_axiomatic_candidate_execution' m c; *)
              let ac = MachineDefAxiomatic.mk_axiomatic_calc c in
              (* pp_axiomatic_calc m ac; *)
              if k=4 then dump_axiomatic_calc m ac (Printf.sprintf "Test %s  Candidate %d" name k)
            end 
          else ();
        end; k + 1)
      cands 0);
    fprintf chan "End of axiomatic candidate executions\n";
    cands
  end


open Printf

(* Fork MMexplorer2 and collect result *)
let mmexplorer () = !Globals.solver 

let read_mmexplorer chan =
  let allowed = Str.regexp_string "allowed"
  and forbidden = Str.regexp_string "forbidden"
  and number = Str.regexp " [0-9]+ " in

  let substring reg s =
    try
      ignore (Str.search_forward reg s 0) ;
      true
    with Not_found -> false in

  let get_number line =
    if substring number line then
      let n = Str.matched_string line in
      let n = String.sub n 1 (String.length n-2) in
      let n =
        try int_of_string n
        with _ -> Warn.fatal "Bad MMexplorer output: '%s'" line in
      n
    else
      Warn.fatal "Bad MMexplorer output: '%s'" line in

  let read_line () =
    try Some (input_line chan) with
    | End_of_file -> None in

  let rec do_rec () = match read_line () with
  | None -> []
  | Some s ->
      if substring allowed s then
        (get_number s,true)::do_rec ()
      else if substring forbidden s then
         (get_number s,false)::do_rec ()
      else do_rec () in
  let xs = do_rec () in
  let xs =
    List.sort (fun (x1,_) (x2,_) -> Pervasives.compare x1 x2) xs in
  List.map snd xs

let fork_smt tmp =
  let inp =
    Unix.open_process_in
      (sprintf "%s %s%s"
         (mmexplorer ()) tmp
         (if !Globals.debug > 0 then "" else " 2> /dev/null")) in
  let bs = read_mmexplorer inp in
  begin match Unix.close_process_in inp with
  | Unix.WEXITED 0 -> bs
  | _ ->
      Warn.fatal "Fork of mmexplorer failed"
  end

let axiomatic_candidates_smt test st thread_regs cand_matches_constraint =
  let cands = all_axiomatic_candidates test st thread_regs in
  let name = Test.simple_name test in 
  let tmp =
    match !Globals.candidates with
    | None -> Filename.temp_file "ppcmem" ".cand"
    | Some fname -> fname in  
  Misc.output_protect
    (fun chan ->
      fprintf chan "Axiomatic candidate executions\n";
      fprintf chan "Candidates = %d\n" (List.length cands);
      let k = ref 0 in
      List.iter
        (fun c ->
          let m  = { ppm = Globals.ppmode_default;
                     prettyi = pp_prettyi_of c;
                     prettywi = [] } in
          fprintf chan "Test: \"%s\"  Candidate %d\n" name !k;
          incr k ;
          fprintf chan "constraint-relevant = %s\n"
            (if cand_matches_constraint c then "yes" else "no");
          pp_axiomatic_candidate_execution_mr chan m c;
          ())
        cands ;
      fprintf chan "End of axiomatic candidate executions\n")
    tmp ;
  if !Globals.debug > 0 then begin
    Misc.input_protect
      (fun chan ->
        try
          while true do
            fprintf stderr "%s\n" (input_line chan)
          done
        with End_of_file -> ())
      tmp ;
    flush stderr
  end ;
  begin match !Globals.candidates with
  | None ->
      let bs = fork_smt tmp in
      Sys.remove tmp ;
      let rec filter_cands cs bs = match cs,bs with
      | [],[] -> []
      | ([],_::_)|(_::_,[]) -> assert false
      | c::cs,b::bs ->
          if b then c::filter_cands cs bs
          else filter_cands cs bs in
      filter_cands cands bs
  | Some fname ->
      eprintf "Stop after generating ACE in %s\n" fname ;
      raise (Misc.Exit "")
  end
