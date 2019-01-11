(*===============================================================================*)
(*                                                                               *)
(*                rmem executable model                                          *)
(*                =====================                                          *)
(*                                                                               *)
(*  This file is:                                                                *)
(*                                                                               *)
(*  Copyright Shaked Flur, University of Cambridge  2017                         *)
(*  Copyright Peter Sewell, University of Cambridge 2015                         *)
(*                                                                               *)
(*  All rights reserved.                                                         *)
(*                                                                               *)
(*  It is part of the rmem tool, distributed under the 2-clause BSD licence in   *)
(*  LICENCE.txt.                                                                 *)
(*                                                                               *)
(*===============================================================================*)

open MachineDefUtils
(*open MachineDefValue *)
open MachineDefTypes
open Types
open Model_aux
open Printf
    (*
    let pair_compare cmpa cmpb (a1,b1) (a2,b2) = 
      match cmpa a1 a2 with
      | 0 -> cmpb b1 b2
      | (_ as c) -> c
    *)

let exit_on_bad_fetch m c = 
  (match c with 
  | TSS_fetch (tid,ioid,_,a,(FDO_illegal_fetch_address as fdo),_) 
  | TSS_fetch (tid,ioid,_,a,((FDO_decode_error _) as fdo),_) 
    -> 
      Printf.printf "%s\n" 
        (Pp.pp_thread_trans_prefix tid ioid  ^
         sprintf "Fetch from address %s %s" (Pp.pp_address m a) (Pp.pp_fdo m fdo));
      raise Globals.Test_proper_termination
  | _ -> ())

let stop_interactive_auto_on_test_start_fetch m c =
  match c with
  | TSS_fetch (tid,ioid,_,a,fdo,_) ->
      if ((Globals.get_our_runopts ()).breakpoint_actual) then
        if 
          try List.assoc (a,8) (* guess *) m.Globals.pp_symbol_table = "actual_test"
          with Not_found -> false
        then
          (Globals.set_interactive_auto false;
           Globals.set_breakpoint_actual false)
        else 
          ()
      else 
        ()
  | _ -> ()

    
let init x = ()

(*    type allowed_reads = (address * value) list *)

(* The info structure of state of search procedure *)
    type info = {
        trans_todo : Globals.trans_or_int list;     (* follow list as numbered choices *)
        choices_so_far : int list; (* numbered choices made so far *)
        trans_so_far : trans list; (* transitions made so far *)
        last_system_state : MachineDefTypes.system_state option; (* the preceding system state (one step up in search tree) *)
        last_numbered_cands : (int * trans) list;  (* the user-interface numbered transitions from before, for text ui *)
(*        allowed_acts : allowed_reads; *)(* constraints on read values *)
      }

    type interactive_input = 
      | I_Choice of info * int
      | I_Interactive_auto
      | I_Toggle_debug
      | I_Auto
      | I_Undo
      | I_Quit

(* The state interaction manipulates *)
    type interaction_state = {
        numbered_cands : MachineDefUI.ui_trans list;  (* The list of numbered transitions *)
        info  : info;(* The info structure *)
      }


    let display x y z =
      ()

(* Take inp:interaction_state as input:
     numbered_cands : candidates enabled
     info  : previous info
   EITHER Produces an interaction_state as output:
     numbered_cands : candidates enabled by user interaction
     info  : updated info
   and feeds that to k_info continuation  (also call k_info if there are no enabled transitions)
   OR invokes k_undo continuation
   OR invokes k_userquit continuation*)

    let interact_with_user =
      fun (m: Globals.ppmode)
          (make_ui_system_state_opt: MachineDefTypes.system_state option -> MachineDefTypes.system_state -> MachineDefUI.ui_trans list -> MachineDefUI.ui_system_state)
          (pp_ui_system_state: Globals.ppmode -> MachineDefUI.ui_system_state -> string)
          (s: MachineDefTypes.system_state)
          (inp:interaction_state) 
          (k_info: interaction_state -> unit) 
          (k_undo: unit->unit)
          (k_userquit:unit->unit) ->
        begin
          (match m.Globals.pp_kind with 
          | Globals.Ascii -> 
              (* flip this for interactive debug printing from model *)
              (match m.Globals.pp_suppress_newpage || !Debug.debug with
              | true -> ()
              | false ->Printf.printf "\027[;H\027[J";  (* cursor to top left and clear *))
          | _ -> ());
          Printf.printf "----------------------------------------------------------------------------\n";


(* moved into run.ml
          let numbered_cands = 
            let rec assoc' y0 xys = match xys with | [] -> None | (x,y)::xys' -> if (trans_compare y0 y = 0) then Some x else assoc' y0 xys' in
            let current_trans = List.sort trans_compare (Pset.elements inp.cands) in
            let (carried_over_trans,new_trans) =
              let rec f ts = match ts with
              | [] -> ([],[])
              | t::ts' -> 
                  let (co,nt) = f ts' in
                  match assoc' t inp.info.last_numbered_cands with
                  | Some n -> ((n,t)::co,nt)
                  | None -> (co,t::nt) in
              f current_trans in
            let rec number n ns xs = 
              match xs with 
              | [] -> [] 
              | x::xs' -> 
                  if List.mem n ns then number (n+1) ns xs 
                  else (n,x)::number (n+1) ns xs' in
            (*Printf.printf "***** %s  ---  %s *****\n"
               (String.concat "," (List.map (function (n,_) -> string_of_int n) carried_over_trans))
               (String.concat "," (List.map (function (n,_) -> string_of_int n) inp.info.last_numbered_cands));*)
            List.sort (function (n1,_) -> function (n2,_) -> compare n1 n2)
              (carried_over_trans 
               @ (number 0 (List.map fst carried_over_trans (*@ inp.info.last_numbered_cands*)) new_trans)) in
*)

(* dead code?
   let itd = 
   { per_instruction = opt_map 
   (function 
   | (n,Commit_instruction (tid,ii)) -> Some ((tid,ii.ioid),n)
   | (n,Read_from_storage_subsystem (tid,ii,w)) -> Some ((tid,ii.ioid),n)
   | (n,Write_forward_to_read (tid,ii1,w,ii2)) -> Some ((tid,ii1.ioid),n)
   | _ -> None) 
   numbered_cands ;
   per_write_ann = opt_map
   (function 
   | (n,Write_announce_to_thread (w,tid)) -> Some (w,n)
   | _ -> None)
   numbered_cands;
   per_write_coh = opt_map
   (function 
   | (n,Write_reaching_coherence_point w) -> Some (w,n)
   | _ -> None)
   numbered_cands;
   per_barrier_prop = opt_map 
   (function 
   | (n,Barrier_propagate_to_thread (b,tid)) -> Some (b,n)
   | _ -> None)
   numbered_cands;
   per_barrier_ack = opt_map
   (function 
   | (n,Acknowledge_sync  b) -> Some (b,n)
   | _ -> None)
   numbered_cands;
   per_partial_coh_commit = opt_map
   (function
   | (n,Partial_coherence_commit (w1,w2)) -> Some ((w1,w2),n)
   | _ -> None)
   numbered_cands;
   } in
 *)

          let ui_state = make_ui_system_state_opt (inp.info.last_system_state) s (inp.numbered_cands) in 
          Printf.printf "%s\n" (pp_ui_system_state m ui_state);
            (*Printf.printf "%s\n" (MS.pp_system_state (inp.info.last_system_state) s itd);*)
          (* hack to pp plain instruction tree of thread 0 *)
          (* (match s with PLDI s' -> Printf.printf "%s\n" (Pp.pp_plain_instruction_tree m 0 "" ((Pmap.find 0 s'.thread_states).instruction_tree)));*)
          Printf.printf "Choices so far (%n): [%s]%s\n" 
            (List.length inp.info.choices_so_far)
            (String.concat ";" (List.map string_of_int inp.info.choices_so_far))
	    (match inp.info.trans_todo with
	    | [] -> ""
	    | _  -> "  remaining follow-spec: ["^String.concat ";" (List.map (function Globals.Int n -> string_of_int n) inp.info.trans_todo) ^"]"
            );
	  if List.length inp.numbered_cands = 0 then
	    begin
	      Printf.printf "No enabled transitions\n";
	      k_info {info=inp.info;numbered_cands=inp.numbered_cands}
	    end
	  else
	    begin
	      Printf.printf "%s\n" (Pp.colour_bold m "Enabled transitions:");
	      List.iter (fun (n,c) ->
		
		Printf.printf "  %2s: %s \n" (Pp.colour_tran_id m (*Pp.pp_tran_id m*) (*({Globals.pp_kind=Globals.Ascii;Globals.pp_colours = (Globals.get_our_runopts()).Globals.colours })*) (sprintf "%2d" n)) (Pp.pp_trans m c)) inp.numbered_cands;
	      Printf.printf "----------------------------------------------------------------------------\n";

              let default_choice = match inp.numbered_cands with (n,c)::_ -> Some n | _ -> None in
	      
              (* repeatedly ask the user for input until we get something sensible and return an I_Choice/I_Auto/I_Undo/I_Quit*)
	      let rec n_input info = 

		Printf.printf "Step %d   Choose%s:  " 
                  (List.length inp.info.choices_so_far)
		  (match info.trans_todo with
		  | Globals.Int n::ns -> " ["^string_of_int n^"]"
		  | [] -> 
                      (match default_choice with
                      | Some n -> " ["^string_of_int n^"]"
                      | None -> "")
                  );
                flush stdout;
                (* for auto-internal true, see if there's an internal transition enabled *)
                let auto_internal_candidate = 
                  if (Globals.get_our_runopts ()).auto_internal then
                    try 
                      let (n,_) = List.find (fun (n,c) -> match c with T_only_trans (_,_,_,T_internal _) -> true | _ -> false) inp.numbered_cands in
                      Some n
                    with
                    | Not_found -> None
                  else
                    None in

		(match (auto_internal_candidate, (Globals.get_our_runopts ()).interactive_auto, !Globals.auto_follow, info.trans_todo) with
                (* there's an auto-internal candidate; do that first, whatever *)
		| (Some n, _, _,_) ->
		    I_Choice (info,n) 
                (* there's a follow-mode transition specified - use if legal, otherwise cancel follow-mode*)
		| (_,_, true, Globals.Int n::ns) -> 
                    if (try (List.assoc n inp.numbered_cands;false) with Not_found -> true)   then 
		      (Printf.printf "follow specification error: %d not a legal transition number\n<shifting to interactive mode>\n" n; 
		       n_input {info with trans_todo = []})
                    else (
                      (* Printf.printf "%d: %s (from follow list)\n" n (Pp.pp_trans m (List.assoc n inp.numbered_cands) ; *)
		      I_Choice ({info with trans_todo = ns},n) )
                        
                (* there's no auto-follow-mode transition - ask the user for input and try to parse *)
                | (_,interactive_auto, _, _) -> 
                    let s = if interactive_auto then (Printf.printf "\n";"") else try read_line () with End_of_file -> Printf.printf "\n";"quit" in
                    if s = "auto" then 
		      I_Auto
                    else if List.mem s ["i";"iauto"] then 
                      I_Interactive_auto
                    else if List.mem s ["d"] then 
                      I_Toggle_debug
                    else if List.mem s ["u";"undo"] then 
		      match inp.info.choices_so_far with
		      | [] -> Printf.printf "cannot undo from the initial state\n"; n_input info
		      | _ -> I_Undo
                    else if List.mem s ["t"] then 
                      (let fd = open_out "ui_snapshot.tex" in Printf.fprintf fd "%s\n" (pp_ui_system_state {m with pp_kind=Globals.Latex} ui_state); close_out fd;
                      n_input info)
                    else if List.mem s ["q";"quit"; "exit"; "x"] then 
		      I_Quit
                    (* default choice in interactive follow-mode *)
                    else if s = "" && info.trans_todo <> [] then
                      let (n,ns) = 
                        match  info.trans_todo with
		        | Globals.Int n::ns -> (n,ns) in
		      I_Choice ({info with trans_todo = ns},n)
                    (* default choice not in interactive follow-mode *)
                    else if s = "" && info.trans_todo = [] && default_choice <> None then
                      let n = match default_choice with Some n -> n in
		      I_Choice (info,n)
                    else 
                      let trim s =
                        let len = String.length s in
                        let i = ref 0 in
                        while !i < len &&  s.[!i]=' ' do
                          incr i
                        done;
                        let j = ref (len - 1) in
                        while !j >= !i && s.[!j]=' ' do
                          decr j
                        done;
                        if !i = 0 && !j = len - 1 then
                          s
                        else if !j >= !i then
                          String.sub s !i (!j - !i + 1)
                        else
                          "" in
                      let ns = try List.map (fun s -> int_of_string (trim s)) (Str.split (Str.regexp ";") s) with Failure _ -> [] in
                      if
                        (match ns with
                        | [] -> true
                        | n::ns' -> try (List.assoc n inp.numbered_cands;false) with Not_found -> true)
                      then 
			(Printf.printf "syntax error: must be a transition number (or ; separated list thereof), return (for the next follow-list transition if there is one), \"undo\", \"u\", \"quit\", \"q\", \"exit\", \"x\", \"i\", \"iauto\", \"t\" (to dump typeset tex), \"d\" (to toggle performance debugging), or \"auto\"\n";
			 n_input info)
		      else 
                        I_Choice (  {inp.info with trans_todo = List.map (function i -> Globals.Int i) (List.tl ns) } ,List.hd ns)) in
	      match n_input inp.info with
	      | I_Choice(info,n_choice) -> 
                  let c = List.assoc n_choice inp.numbered_cands in 
                  stop_interactive_auto_on_test_start_fetch m c;
                  exit_on_bad_fetch m c;
                  k_info 
		    {info = {(*inp.*)info with choices_so_far = info.choices_so_far (*@ [n_choice]*); (*EXP: remove: last_numbered_cands = inp.numbered_cands*)};
                     numbered_cands = [(n_choice, c)] }
	      | I_Auto -> 
                  (Globals.set_interactive false;
                   k_info inp)
	      | I_Toggle_debug -> 
                  (Debug.debug := not (!Debug.debug);
                   k_info inp)
	      | I_Interactive_auto -> 
                  (Globals.set_interactive_auto true;
                   k_info inp)
	      | I_Undo ->
                  k_undo ()
	      | I_Quit ->
                  k_userquit ()
            end 
	end

    let ask_quit_or_undo k_undo k_userquit =
      Printf.printf "----------------------------------------------------------------------------\n";
      let rec inp () = 
        let () = Printf.printf "Undo last transition, to continue exploration, or quit [u/q]: " in
        let s = try read_line () with End_of_file -> Printf.printf "\n"; "quit" in
        if s = "u" || s = "undo" then k_undo ()
        else if s = "q" || s = "quit" then k_userquit ()
        else (Printf.printf "syntax error, please input u/q/undo/quit\n"; inp ()) in
      inp ()

