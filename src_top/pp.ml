(*==================================================================================================*)
(*                                                                                                  *)
(*                rmem executable model                                                             *)
(*                =====================                                                             *)
(*                                                                                                  *)
(*  This file is:                                                                                   *)
(*                                                                                                  *)
(*  Copyright Peter Sewell, University of Cambridge                                     2011-2017   *)
(*  Copyright Shaked Flur, University of Cambridge                                      2014-2018   *)
(*  Copyright Christopher Pulte, University of Cambridge                                2015-2018   *)
(*  Copyright Jon French, University of Cambridge                                       2016-2018   *)
(*  Copyright Susmit Sarkar, University of St Andrews                             2011, 2013-2014   *)
(*  Copyright Pankaj Pawan, IIT Kanpur and INRIA (when this work was done)                   2011   *)
(*  Copyright Robert Norton-Wright, University of Cambridge                             2016-2017   *)
(*  Copyright Kathy Gray, University of Cambridge (when this work was done)             2015-2016   *)
(*  Copyright Francesco Zappa Nardelli, INRIA, Paris, France                                 2011   *)
(*  Copyright Ohad Kammar, University of Cambridge (when this work was done)            2013-2014   *)
(*  Copyright Linden Ralph, University of Cambridge (when this work was done)                2017   *)
(*  Copyright Luc Maranget, INRIA, Paris, France                                             2011   *)
(*  Copyright Dominic Mulligan, University of Cambridge (when this work was done)            2013   *)
(*                                                                                                  *)
(*  All rights reserved.                                                                            *)
(*                                                                                                  *)
(*  It is part of the rmem tool, distributed under the 2-clause BSD licence in                      *)
(*  LICENCE.txt.                                                                                    *)
(*                                                                                                  *)
(*==================================================================================================*)


open Printf

open Interp_interface
open Sail_impl_base
open MachineDefUtils
(* open MachineDefValue *)
open MachineDefTypes
(* open MachineDefInstructionSemantics *)
(* open BitwiseCompatibility *)
open MachineDefUI
open MachineDefCandidateExecution

open Types
open Model_aux

open Globals

(*open Printing_functions *)  (* the interpreter printing functions *)


(** ******************* *)
(** dwarf location info *)
(** ******************* *)

(* TODO:

- DONE in the first instance, calculate and print all the dynamic dwarf
  location info with each instruction_instance

- figure out a sensible caching scheme for the per-pc info (the
  Dwarf.analysed_location_data_at_pc) and for the results of
  evaluating that wrt particular register and memory values

Notes:

- where should we do memory reads from in evaluating dwarf location expressions?

- if the memory reads done by dwarf location expression evaluation are all of saved frame-pointer registers and suchlike in the current stack frame, then they *should* be thread-local, and we could just look back in the po-predecessors for writes, rather than asking the storage subsystem (which would have to be wrt a particular storage subsystem state, and it's unclear which, and in any case we don't currently keep them all around)    But those locations can be reused over time...

- what about in pp of dwarf info in storage subsystem events?   For now, punt, roughly taking the location of the corresponding event

- what about the initial-state writes, which aren't associated to any instruction?   in principle should use only the "global" dwarf variables?   And those valid at the main start address?

- what about the branch predication addresses?

- should pp of a read be wrt the ioid of the read or that of the write it reads from?  the former

- is the dwarf location info for an instruction pc address supposed to be valide before or after that instruction executes?   I'm guessing before.   Should we therefore use that info for reads, and use the info associated with the *next* pc address for writes (esp. for register writes)?

- to sensibly pp addresses and values involved in indirect accesses, should we follow all the accessible memory from the in-scope pointers down to some depth, and match those addresses against any values seen in the machine execution?



*)

(* ugly wrapping function to avoid inter-dependency between linksem and model *)
let wrap_ev (ev: MachineDefSystem.dwarf_evaluation_context) : Dwarf.evaluation_context =
  { Dwarf.read_register =
    (function n ->
      match ev.MachineDefSystem.dec_read_register n with
      | MachineDefSystem.DRRR_result n' -> Dwarf.RRR_result n'
      | MachineDefSystem.DRRR_not_currently_available -> Dwarf.RRR_not_currently_available
      | MachineDefSystem.DRRR_bad_register_number -> Dwarf.RRR_bad_register_number);
    Dwarf.read_memory =
    (function n1 -> function n2 ->
      match ev.MachineDefSystem.dec_read_memory n1 n2 with
      | MachineDefSystem.DMRR_result n' -> Dwarf.MRR_result n'
      | MachineDefSystem.DMRR_not_currently_available -> Dwarf.MRR_not_currently_available
      | MachineDefSystem.DMRR_bad_address -> Dwarf.MRR_bad_address);
  }


(* for the moment ignoring footprint sizes and lvalue/rvalue printing *)
let lookup_dwarf_symbol m ioid (a:Sail_impl_base.address) : string option =
  match m.pp_dwarf_static, m.pp_dwarf_dynamic with
  | Some ds, Some pp_dwarf_dynamic ->
      begin match pp_dwarf_dynamic.get_evaluation_context ioid with
      | Some ((instruction_address: Nat_big_num.num (*natural*)), ev0) ->
          let a' = Sail_impl_base.integer_of_address a in
          let ev = wrap_ev ev0 in
          begin match Dwarf.analysed_locations_at_pc ev ds instruction_address with
          | alspc ->
              (*"analysed_location_data_at_pc:\n"
              ^ Dwarf.pp_analysed_location_data_at_pc ds.Dwarf.ds_dwarf alspc *)
              let names = Dwarf.names_of_address ds.Dwarf.ds_dwarf alspc a' in
              begin match names with
              | [] -> None
              | _ -> Some (String.concat "/" names)
              end
          (* FIXME: Dwarf.analysed_locations_at_pc fails sometimes, e.g., when running
          the AArch64 ELF thread_start_test it fails for the first instruction of
          start_thread with the message "evaluate_cfa: no fde encloding pc" s*)
          | exception _ -> None
          end
      | None -> None (*"<failure: get_dwarf_evaluation_context>"*)
      end
  | _,_ -> None


let source_file_cache = ref ([] : (string * (string array option)) list)

let source_line file n1 =
  let access_lines lines n =
    if n < 0 || n >= Array.length lines then Some (sprintf "line out of range: %i vs %i" n (Array.length lines)) else
    Some lines.(n) in

  let n = n1 -1 in
  match (try Some (List.assoc file !source_file_cache) with Not_found -> None) with
  | Some (Some lines) -> access_lines lines n
  | Some (None) -> None
  | None ->
      match Files.read_source_file file with
      | Some lines ->
          source_file_cache := (file, Some lines) :: !source_file_cache;
          access_lines lines n
      | None ->
          source_file_cache := (file, None) :: !source_file_cache;
          None


let pp_source_line so =
  match so with
  | Some s -> " (" ^ s ^ ")"
  | None -> ""


let pp_dwarf_source_file_lines m ds (pp_actual_line: bool) (a: Sail_impl_base.address) : string option =
  let a' = Sail_impl_base.integer_of_address a in
  let sls = Dwarf.source_lines_of_address ds a' in
  match sls with
  | [] -> None
  | _ ->
      Some
        (String.concat
           ", "
           (List.map
              (fun (s,n,lnr) ->
                s ^ ":" ^ Nat_big_num.to_string n ^ if pp_actual_line then pp_source_line (source_line s (Nat_big_num.to_int n))  else ""
              )
              sls
           )
        )



(** ******************* *)
(** padding             *)
(** ******************* *)


let pad n s = if String.length s < n then s ^ String.make (n-String.length s) ' ' else s
let pad_left n s = if String.length s < n then String.make (n-String.length s) ' ' ^ s else s


(** ******************* *)
(** pretty ids          *)
(** ******************* *)



let eiids_of_instruction i =
  let reads = List.map (fun (rr, _) -> rr.reiid) (List.rev i.subreads.sr_unsat_slices) in
  let writes =
    List.map
      (fun w -> w.weiid)
      (i.subwrites.sw_potential_write_addresses
        @ i.subwrites.sw_potential_writes
        @ i.subwrites.sw_propagated_writes)
  in
  let barriers =
    match i.instruction_kind with
    | IK_barrier _ ->
        if i.committed_barriers = [] then
          (* HACK: the barrier has not been committed so there is no barrier
          event yet. We assume there will be exactly one barrier event from
          the instruction and that it will get the next fresh id *)
          [fst (MachineDefFreshIds.gen_fresh_id i.instance_id_state)]
        else
          List.map (fun b -> b.beiid) i.committed_barriers
    | _ -> []
  in
  (* TODO: add eiid of transaction *)
  reads @ writes @ barriers


let rec eiidss_of_instruction_list is =
  match is with
  | [] -> []
  | i::is' ->
      let eiids = eiids_of_instruction i in
      let eiidss = eiidss_of_instruction_list is' in
          (match eiids with
          | [] -> eiidss
          | _ -> eiids::eiidss)

let rec eiidss_of_instruction_tree t =
match t with
| T iits ->
    let rec eiidss_of_iits iits =
      match iits with
      | [] -> []
      | (i,t')::iits' ->
          let eiids = eiids_of_instruction i in
          let eiidss = eiidss_of_instruction_tree t' in
          (match eiids with
          | [] -> eiidss
          | _ -> eiids::eiidss)
            @ eiidss_of_iits iits' in
    eiidss_of_iits iits

let eiids_of_thread t_model (_tid,thread) =
  let (old_instructions,instruction_tree) = t_model.ts_instruction_tree thread in
  eiidss_of_instruction_list (List.rev old_instructions) @
    eiidss_of_instruction_tree instruction_tree


let eiidss_of_threads s =
  let threads = Pmap.bindings_list s.thread_states in
  List.flatten (List.map (eiids_of_thread s.t_model) threads)


let eiidss_of_system_state s =
  let eiidss_of_initial_writes = [ List.map (fun w -> w.weiid) (List.rev s.initial_writes) ] in
  eiidss_of_initial_writes @ eiidss_of_threads s

let pretty_prefixes = ["init";"a";"b";"c";"d";"e";"f";"g";"h";(*ijkl*)"m"; "n"; (*o*) "p"; "q"; (*"r";*) "s"; "t";"u";(*uwxyz*)]
let pretty_prefixes_length = List.length pretty_prefixes

let pretty_eiid m no =
  if m < pretty_prefixes_length then
    List.nth pretty_prefixes m ^ (match no with | None -> "" | Some n -> sprintf "%i" n)
  else  sprintf "z%i" (m-pretty_prefixes_length) ^ (match no with | None -> "" | Some n -> sprintf ".%i" n)

let pretty_eiids s =
  let eiidss = eiidss_of_system_state s in
  List.flatten (
  List.mapi
    (fun m eiids ->
      (match eiids with
      | [eiid] -> [(eiid, pretty_eiid m None)]
      | _ ->
          List.mapi
            (fun n eiid ->
              (eiid, pretty_eiid m (Some n)))
            eiids)
    )
    eiidss
 )


let pp_eiid eiid =
  let ((tid, ioid), eiid) = eiid in
  sprintf "(%d:%d:%d)" tid ioid eiid

let pp_pretty_eiid m eiid =
  try
    List.assoc eiid m.pp_pretty_eiid_table ^ ":"
  with
  | Not_found -> pp_eiid eiid ^ ":"

(** ******************* *)
(** split               *)
(** ******************* *)


(* js_of_ocaml cann't use Str.split (or Str in general), use the
   following function instead.
   split del str splits str into substrings, taking as delimiters the
   substrings del, and returns the list of substrings. An occurrence
   of the delimiter at the beginning or at the end of the string is
   ignored. *)
let split (del : string) (str : string) =
  let del_length = String.length del in

  let part n str =
    let prefix = String.sub str 0 n in
    let suffix = String.sub str n ((String.length str) - n) in
    (prefix, suffix)
  in

  let rec split_helper str pos strs =
    if pos + del_length > String.length str then
      if str = "" then List.rev strs
      else List.rev (str :: strs)
    else

    let (prefix,suffix) = part pos str in
    let (mid,suffix) = part del_length suffix in

    if mid = del then split_helper suffix 0 (prefix :: strs)
    else split_helper str (pos + 1) strs
  in

  match split_helper str 0 [] with
  | head :: tail when head = "" -> tail
  | strs -> strs


(** ******************* *)
(** colour highlighting *)
(** ******************* *)

(* vt220 colour definitions *)

let black   = 0
let red     = 1
let green   = 2
let yellow  = 3
let blue    = 4
let magenta = 5
let cyan    = 6
let white   = 7
let dark_gray = 60

let _reset = "\x1b[0m"
let _color fg br = sprintf "\x1b[%u;%um" br (fg+30)
let _bold = sprintf "\x1b[%u;%um" 0 (1)
let _w = "\x1b[0;1;4m"
let _r = _color red 0
let _b = _color blue 0  (* was blue 1 *)
let _g = _color green 0

let col_wrap col s = col ^ s ^ _reset

let col_bold  s =  col_wrap  _bold s
let col_red  s =  col_wrap  _r s
let col_black  s = col_wrap  _b s
let col_green  s = col_wrap  _g s
let col_yellow  s = col_wrap  (_color yellow 0) s
let col_blue  s = col_wrap  (_color blue 0)  s
let col_magenta  s = col_wrap  (_color  magenta 0) s
let col_cyan  s = col_wrap  (_color  cyan 0) s
let col_white  s = col_wrap   (_color  white 0)  s
let col_dark_gray  s = col_wrap   (_color  dark_gray 0)  s

(* change highlighting *)

let decolour2 (cx:'a changed2) =
  match cx with
  | C2_new x -> x
  | C2_unchanged x -> x


let colour_changed3 m (cs:string changed3) =
  match cs with
  | C3_gone s ->
      if m.Globals.pp_colours then
        match m.Globals.pp_kind with
        | Ascii | Hash -> col_dark_gray s
        | Html -> "<span class='changed_gone'>"^ s ^"</span>"
        | Latex -> "\\mydarkgray{" ^ s ^"}"
      else s
  | C3_unchanged s -> s
  | C3_new s ->
      if m.Globals.pp_colours then
        match m.Globals.pp_kind with
        | Ascii | Hash -> col_red s
        | Html -> "<span class='changed_new'>"^ s ^"</span>"
        | Latex -> "\\myred{" ^ s ^"}"
      else s

let colour_changed2 m (cs:string changed2) =
  match cs with
  | C2_new s -> colour_changed3 m (C3_new s)
  | C2_unchanged s -> colour_changed3 m (C3_unchanged s)

let colour_changed2b m (cs:string changed2b) =
  match cs with
  | C2b_changed s -> colour_changed3 m (C3_new s)
  | C2b_unchanged s -> colour_changed3 m (C3_unchanged s)


let colour_changed2b_f m (pp_f:Globals.ppmode->'a->string) (cx:'a changed2b) =
  match cx with
  | C2b_changed x -> colour_changed3 m (C3_new (pp_f m x))
  | C2b_unchanged x -> colour_changed3 m (C3_unchanged (pp_f m x))

let nocolour_changed2b_f m (pp_f:Globals.ppmode->'a->string) (cx:'a changed2b) =
  match cx with
  | C2b_changed x -> pp_f m x
  | C2b_unchanged x -> pp_f m x

let colour_changed2_f m (pp_f:Globals.ppmode->'a->string) (cx:'a changed2) =
  match cx with
  | C2_new x -> colour_changed3 m (C3_new (pp_f m x))
  | C2_unchanged x -> colour_changed3 m (C3_unchanged (pp_f m x))

let colour_changed2_fp m (pp_f:Globals.ppmode->'a->(string*'b)) (cx:'a changed2) =
  match cx with
  | C2_new x ->       let (s,b) = pp_f m x in (colour_changed3 m (C3_new s), b)
  | C2_unchanged x -> let (s,b) = pp_f m x in (colour_changed3 m (C3_unchanged s), b)

let colour_changed3_f m (pp_f:Globals.ppmode->'a->string) (cx:'a changed3) =
  match cx with
  | C3_new x -> colour_changed3 m (C3_new (pp_f m x))
  | C3_unchanged x -> colour_changed3 m (C3_unchanged (pp_f m x))
  | C3_gone x -> colour_changed3 m (C3_gone (pp_f m x))

let colour_changed3_fp m (pp_f:Globals.ppmode->'a->(string*'b)) (cx:'a changed3) =
  match cx with
  | C3_new x ->       let (s,b) = pp_f m x in (colour_changed3 m (C3_new s), b)
  | C3_unchanged x -> let (s,b) = pp_f m x in (colour_changed3 m (C3_unchanged s), b)
  | C3_gone x ->      let (s,b) = pp_f m x in (colour_changed3 m (C3_gone s), b)


(* lifting the colour_changed functions to lists, lists representing sets, etc. *)

let pp_list_sep m = ", "

let pp_list m pp_f xs = String.concat (pp_list_sep m) (List.map (pp_f) xs)

let pp_leftbrace m =
  match m.Globals.pp_kind with
  | Ascii | Html | Hash -> "{"
  | Latex -> "\\mylb{}"

let pp_rightbrace m =
  match m.Globals.pp_kind with
  | Ascii | Html | Hash -> "}"
  | Latex -> "\\myrb{}"


let pp_arrow m =
  match m.Globals.pp_kind with
  | Ascii | Hash -> "->"
  | Html -> "-&gt;"
  | Latex -> "$\\rightarrow$"

let pp_Arrow m =
  match m.Globals.pp_kind with
  | Ascii | Hash -> "=>"
  | Html -> "=&gt;"
  | Latex -> "$\\Rightarrow$"

let pp_mapsto m =
  match m.Globals.pp_kind with
  | Ascii | Hash -> "|->"
  | Html -> "|-&gt;"
  | Latex -> "$\\mapsto$"

let delim l r s = l ^ s ^ r

let pp_setlist_body m (pp_f:Globals.ppmode->'a->string) (xs:'a list) =
  pp_list m (pp_f m) xs

let pp_setlist m (pp_f:Globals.ppmode->'a->string) (xs:'a list) =
  pp_leftbrace m
  ^ pp_setlist_body m pp_f xs
  ^ pp_rightbrace m


let pp_changed2_setlist_body m (pp_f:Globals.ppmode->'a->string) (xs:'a changed2 list) =
  pp_list m (function (cx:'a changed2) -> colour_changed2_f m pp_f cx) xs

let pp_changed2_setlist m (pp_f:Globals.ppmode->'a->string) (xs:'a changed2 list) =
  pp_leftbrace m
  ^ pp_changed2_setlist_body m pp_f xs
  ^ pp_rightbrace m

let pp_changed2_list_body m (pp_f:Globals.ppmode->'a->string) (xs:'a changed2 list) =
  pp_list m (function (cx:'a changed2) -> colour_changed2_f m pp_f cx) xs

let pp_changed2_list m (pp_f:Globals.ppmode->'a->string) (xs:'a changed2 list) =
  "["
  ^ pp_changed2_list_body m pp_f xs
  ^ "]"

let pp_changed3_setlist_body m (pp_f:Globals.ppmode->'a->string) (xs:'a changed3 list) =
  pp_list m (colour_changed3_f m pp_f) xs

let pp_changed3_setlist m (pp_f:Globals.ppmode->'a->string) (xs:'a changed3 list) =
  pp_leftbrace m
  ^ pp_changed3_setlist_body m pp_f xs
  ^ pp_rightbrace m

let pp_changed3_list_body m (pp_f:Globals.ppmode->'a->string) (xs:'a changed3 list) =
  pp_list m (function (cx:'a changed3) -> colour_changed3_f m pp_f cx) xs

let pp_changed3_list m (pp_f:Globals.ppmode->'a->string) (xs:'a changed3 list) =
  "["
  ^ pp_changed3_list_body m pp_f xs
  ^ "]"
(* version that adds delimiters only for non-singleton list *)
let pp_changed3_list_bis m (pp_f:Globals.ppmode->'a->string) (xs:'a changed3 list) =
  match xs with
  | [_] -> pp_changed3_list_body m pp_f xs
  | _ -> pp_changed3_list m pp_f xs

(* colouring transitions and memory actions *)

let colour_tran_id m s =
  if m.Globals.pp_colours then
    match m.Globals.pp_kind with
    | Ascii | Hash -> col_green s
    | Html -> "<span class='tran_id'>"^ s ^"</span>"
    | Latex -> "\\mygreen{" ^ s ^"}"
  else s

let colour_memory_action m s =
  if m.Globals.pp_colours then
    match m.Globals.pp_kind with
    | Ascii | Hash -> col_cyan s
    | Html -> "<span class='memory_action'>"^ s ^"</span>"
    | Latex -> "\\mycyan{" ^ s ^"}"
  else s

let colour_bold m s =
  if m.Globals.pp_colours then
    match m.Globals.pp_kind with
    | Ascii | Hash -> col_bold s
    | Html -> "<b>"^ s ^"</b>"
    | Latex -> "\\mybold{" ^ s ^"}"
  else s

let colour_warning m s =
  if m.Globals.pp_colours then
    match m.Globals.pp_kind with
    | Ascii | Hash -> col_yellow s
    | Html -> "<span class='warning'>" ^ s ^ "</span>"
    | Latex -> "\\myyellow{" ^ s ^ "}"
  else s

let colour_finished_instruction m s = colour_bold m s

let colour_unfinished_instruction m s =
  if m.Globals.pp_colours then
    match m.Globals.pp_kind with
    | Ascii | Hash -> col_bold (col_yellow s)
    | Html -> "<span class='warning'>" ^ s ^ "</span>"
    | Latex -> "\\myyellow{" ^ s ^ "}"
  else s


let colour_info m s =
  if m.Globals.pp_colours then
    match m.Globals.pp_kind with
    | Ascii | Hash -> col_cyan s
    | Html -> "<span class='info'>" ^ s ^ "</span>"
    | Latex -> "\\mycyan{" ^ s ^ "}"
  else s

let colour_final m s =
  if m.Globals.pp_colours then
    match m.Globals.pp_kind with
    | Ascii | Hash | Latex -> s
    | Html -> "<span class='final'>" ^ s ^ "</span>"
  else s

let colour_sail m s =
(*
  let char_list_of_string s =
    let n = String.length s in
    let rec f i = if i=n then [] else String.get s i :: f (i+1) in
    f 0 in
  let pp_string s =
    String.concat "" (List.map (fun c -> let i=Char.code c in if i>=32 then sprintf "%c" c else sprintf "<%i>" i) (char_list_of_string s)) in
  pp_string s
 ^ "\n" ^
*)
  if m.Globals.pp_colours then
    match m.Globals.pp_kind with
    | Ascii | Hash ->
        (*let r = Str.regexp_string "\x1b[m" in
        let s = (try
          let pos = Str.search_backward r s (String.length s -1) in
          let s' = Str.replace_matched (_color blue 0) s in
          s'
        with
        | Not_found -> s) in
        pp_string s ^ "\n" ^ *)
        col_cyan s
    | Html -> "<span class='sail'>"^ s ^"</span>"
    | Latex -> "\\myblue{" ^ s ^"}"
  else s



(** ******************* *)
(** hack for linebreaks *)
(** ******************* *)

let linebreak = ref "\n"
(* FIXME: make sure to call linebreak_init everytime pp_kind changes *)
let linebreak_init () =
  match !Globals.pp_kind with
  | Ascii | Latex | Hash -> linebreak := "\n"
  | Html -> linebreak :=  "<br/>"


(** ********************************************************** *)
(** pp of basic values, addresses, etc. using interpreter code *)
(** ********************************************************** *)

(*let pp_word8 m (b:int) =
  Printing_functions.val_to_string (Bytevector [b])
*)

let rec lookup_symbol_and_offset
    (st:  ((Sail_impl_base.address * int) * string) list)
    (a2: Sail_impl_base.address) : (string * int) option =
  match st with
  | ((a,0),s) :: st' -> if a = a2 then Some (s, 0) else lookup_symbol_and_offset st' a2
  | (fp,s) :: st' ->
      begin match MachineDefFragments.offset_in_footprint fp a2 with
      | Some 0 -> Some (s, 0)
      | Some i -> Some (s, i)
      | None -> lookup_symbol_and_offset st' a2
      end
  | [] -> None


let lookup_symbol
    (st:  ((Sail_impl_base.address * int) * string) list)
    (a2: Sail_impl_base.address) : string option =
  match lookup_symbol_and_offset st a2 with
  | Some (s, 0) -> Some s
  | Some (s, i) -> Some (sprintf "%s+%i" s i)
  | None        -> None

let pp_corresponding_symbol m mioid (ma:Sail_impl_base.address option) =
(*
"[[[symbol table = " ^ (String.concat ", " (List.map (fun ((a,sz),n) ->
 sprintf "(%s,%i) |-> %s\n"
  (pp_byte_list m (byte_list_of_address a))
  sz
  n
  )
  m.pp_symbol_table)) ^ "]]]\n" ^
*)
  match ma with
  | None   -> None
  | Some a ->
      let symbol = lookup_symbol m.pp_symbol_table a in
      let dwarf_symbol =
        match mioid with
        | None      -> None
        | Some ioid -> lookup_dwarf_symbol m ioid a
      in
      match symbol, dwarf_symbol with
      | None,    None    -> None
      | Some s,  None    -> Some s
      | None,    Some s  -> Some ("dwarf:" ^ s)
      | Some s1, Some s2 -> Some (s1 ^ "/dwarf:" ^ s2)
(*

      match Lem_list.lookupBy (=) a m.pp_symbol_table with
      | Some s -> sprintf "(%s)" s
      | None -> ""
*)

let maybe_pp_corresponding_symbol m mioid (ma:Sail_impl_base.address option) = 
  if m.pp_prefer_symbolic_values
  then match pp_corresponding_symbol m mioid ma with
       | Some str ->" (" ^ str ^ ")"
       | None -> ""
  else ""

(* This is bad performance-wise: we always compute symo, even if we don't need it *)
let deprecated__pp_value_and_or_corresponding_symbol m (v:string) (symo:string option) =
  if m.pp_prefer_symbolic_values then
    match symo with
    | None -> v
    | Some sym -> sym
  else
    match symo with
    | None -> v
    | Some sym -> v ^ "(" ^ sym ^ ")"

(* TODO: check Printing_functions.bit_lifteds_to_string. We should probably rewrite *)
let pp_register_value m ioid (rv:Sail_impl_base.register_value) =
  if m.pp_kind = Hash
  then
    "0b" ^ String.concat "" 
             (List.map Printing_functions.bit_lifted_to_string rv.rv_bits)
  else
    Printing_functions.register_value_to_string rv ^
      maybe_pp_corresponding_symbol m (Some ioid) (address_of_register_value rv)

         

let pp_memory_value m ioid (mv:Sail_impl_base.memory_value) =
(*  if List.length mv > 16 then "...TODO:pp long number..." else *)
  if m.pp_kind = Hash then
    let bls =
      Sail_impl_base.match_endianness (Globals.get_endianness ()) mv
      |> List.map (fun (Byte_lifted bs) -> bs)
      |> List.concat
    in
    "0b" ^ String.concat "" 
             (List.map Printing_functions.bit_lifted_to_string bls)
  else
    Printing_functions.memory_value_to_string (Globals.get_endianness ()) mv ^
      maybe_pp_corresponding_symbol m (Some ioid) 
        (Sail_impl_base.address_of_memory_value (Globals.get_endianness ()) mv)


let pp_address_lifted m ioid (al:Sail_impl_base.address_lifted) =
  pp_memory_value m ioid (Sail_impl_base.memory_value_of_address_lifted (Globals.get_endianness ()) al)

let rec drop_leading_zeros (bs: Sail_impl_base.byte list) =
  match bs with
  | [] -> [] (* probably shouldn't happen *)
  | [b] -> [b]
  | b::((b'::bs') as bs'') -> if b = Sail_impl_base.byte_of_int 0 then drop_leading_zeros bs'' else bs

let pp_byte_list m (bs: Sail_impl_base.byte list) =
  match m.pp_style with
  | Globals.Ppstyle_full | Globals.Ppstyle_screenshot ->
      Printing_functions.byte_list_to_string bs
  | Globals.Ppstyle_compact ->
      Printing_functions.byte_list_to_string (drop_leading_zeros bs)

let pp_address m mioid (a:Sail_impl_base.address) =
  if m.pp_kind = Hash then
    Nat_big_num.to_string (integer_of_address a)
  else 
    pp_byte_list m (byte_list_of_address a) ^
      maybe_pp_corresponding_symbol m mioid (Some a)


let pp_footprint m mioid (fp:MachineDefTypes.footprint) =
  let (a,sz) = fp in
  pp_address m mioid a
  ^"/"^string_of_int sz

let pp_raw_footprint m ((a, sz) : MachineDefTypes.footprint) =
  (pp_byte_list m (byte_list_of_address a)) ^ "/" ^ string_of_int sz

let pp_opcode m (op:Sail_impl_base.opcode) =
  pp_byte_list m (byte_list_of_opcode op)

let pp_reg m r =
  Printing_functions.reg_name_to_string r

let pp_instruction
      (symbol_table: ((Sail_impl_base.address * MachineDefTypes.size) * string) list)
      (inst: MachineDefTypes.instruction_ast)
      (program_loc: Sail_impl_base.address) =
  begin match inst with
  | Fetch_error -> "fetch error"
  | PPCGEN_instr _ ->
     let i = PPCGenTransSail.shallow_ast_to_herdtools_ast inst in
     PPCGenBase.pp_instruction (PPMode.Ascii) i
  | AArch64_instr _ ->
      let (i': AArch64HGenBase.instruction) =
        if !Globals.aarch64gen then
          AArch64GenTransSail.labelize_ins (lookup_symbol symbol_table) program_loc
            (AArch64GenTransSail.shallow_ast_to_herdtools_ast inst)
        else
          AArch64HGenTransSail.labelize_ins (lookup_symbol symbol_table) program_loc
            (AArch64HGenTransSail.shallow_ast_to_herdtools_ast inst)
      in
      (* Shaked's pp from the AArch64 AST *)
      AArch64HGenBase.pp_instruction (PPMode.Ascii) i'
  | MIPS_instr _->
      let (i': MIPSHGenBase.instruction) =
        (MIPSHGenTransSail.labelize_ins (lookup_symbol symbol_table) program_loc)
          (MIPSHGenTransSail.shallow_ast_to_herdtools_ast inst) in
      MIPSHGenBase.pp_instruction (PPMode.Ascii) i'
  | RISCV_instr ast ->
      (* let (i': RISCVHGenBase.instruction) =
       *   (RISCVHGenTransSail.labelize_ins (lookup_symbol symbol_table) program_loc)
       *     (RISCVHGenTransSail.shallow_ast_to_herdtools_ast inst) in
       * RISCVHGenBase.pp_instruction (PPMode.Ascii) i' *)
     let s = Riscv.print_insn ast in begin
         (* Massive hack to show labels *)
         match ast with
         | Riscv_types.RISCV_JAL _
           | Riscv_types.RISCV_JALR _
           | Riscv_types.BTYPE _ ->
            begin try
              let last_comma = String.rindex s ',' in
              let offset_s = (String.trim (String.sub s (last_comma + 1) (String.length s - last_comma - 1))) in
              let offset = int_of_string offset_s in
              let addr = add_address_nat program_loc offset in
              let s_without_imm = String.sub s 0 last_comma in
              begin match lookup_symbol symbol_table addr with
              | Some sym -> s_without_imm ^ ", " ^ sym
              | None -> s
              end
              with Failure _ -> s
            end
         | _ -> s
       end
  | X86_instr _->
      let (i': X86HGenBase.instruction) =
        (X86HGenTransSail.labelize_ins (lookup_symbol symbol_table) program_loc)
          (X86HGenTransSail.shallow_ast_to_herdtools_ast inst) in
      X86HGenBase.pp_instruction (PPMode.Ascii) i'
  end

let pp_instruction_state
      indent (m:Globals.ppmode)
      (instruction_state_pp : unit -> (string * string)) =
  Printing_functions.set_interp_ppmode
    (match m.Globals.pp_kind with
    | Ascii | Hash -> Printing_functions.Interp_ascii
    | Latex -> Printing_functions.Interp_latex
    | Html -> Printing_functions.Interp_html);

  let ((instruction_stack_to_string_output : string),
       (local_variables_to_string_output : string)) =
    instruction_state_pp () in

(*  let s = Printing_functions.top_instruction_state_to_string is in*)
  let s = instruction_stack_to_string_output in
  let ss = split "\n" s in

  let ss =
    if false (*not(m.pp_screenshot)*) then ss else
    begin match ss with
    | "{" :: ss' ->
        begin match List.rev ss' with
        | "}" :: ss' -> List.rev ss'
        | _ -> ss
        end
    | _ -> ss
    end
  in

  String.concat "" (List.map (fun s -> indent ^ "| " ^ colour_sail m s ^ "\n") ss)
  ^ indent ^ "Env: " ^ local_variables_to_string_output ^ " \n"

let pp_bool m b =
  match b with  (* TODO should use k pp to match her output? not sure*)
  | true -> "true"
  | false -> "false"


let pp_decode_error m de addr = match de with
  | MachineDefTypes.Unsupported_instruction_error0 (_opcode, (i:MachineDefTypes.instruction_ast)) ->
     sprintf "Unsupported instruction (%s)" (pp_instruction m.pp_symbol_table i addr)
  | MachineDefTypes.Not_an_instruction_error0 (op:opcode) -> sprintf "Not an instruction (value: %s)" (pp_opcode m op)
  | MachineDefTypes.Internal_decode_error (s:string) -> "Internal error "^s

(* TODO: need to feed in a pp mode (part of m) into the
interpreter code so that special characters can be printed in
Ascii/Latex/HTML form *)

(* let pp_hash = "#" *)
(* (\* m = match m with *\) *)
(* (\* | Ascii | Dot -> "#" *\) *)
(* (\* | Latex -> "\\#" *\) *)
(* (\* | DotFig -> "\\\\#" *\) *)


(** ******************************************** *)
(** pp of events - reads, writes, barriers, etc. *)
(** ******************************************** *)


let pp_pretty_ioid ioid =
  let (tid, t_ioid) = ioid in
  (*try
    List.assoc ioid !pretty_ioid_map
  with Not_found ->*)
  (sprintf "%d:%d" tid t_ioid)

let pp_pretty_ioid_padded ioid =
  pad 6 (pp_pretty_ioid ioid)


let pp_barrier_kind m = function
  (* PPC barriers *)
  | Barrier_Sync   -> "Sync"
  | Barrier_LwSync -> "Lwsync"
  | Barrier_Eieio  -> "Eieio"
  | Barrier_Isync  -> "ISync"
  (* Arch64 barriers *)
  | Barrier_DMB    -> "DMB"
  | Barrier_DMB_LD -> "DMB LD"
  | Barrier_DMB_ST -> "DMB ST"
  | Barrier_DSB    -> "DSB"
  | Barrier_DSB_LD -> "DSB LD"
  | Barrier_DSB_ST -> "DSB ST"
  | Barrier_ISB    -> "ISB"
  | Barrier_TM_COMMIT -> failwith "Barrier_TM_COMMIT is not really a barrier"
  (* MIPS *)
  | Barrier_MIPS_SYNC -> "mips_sync"
  (* RISC-V *)
  | Barrier_RISCV_rw_rw -> "fence rw,rw"
  | Barrier_RISCV_r_rw  -> "fence r,rw"
  | Barrier_RISCV_w_rw  -> "fence w,rw"
  | Barrier_RISCV_rw_r  -> "fence rw,r"
  | Barrier_RISCV_r_r   -> "fence r,r"
  | Barrier_RISCV_w_r   -> "fence w,r"
  | Barrier_RISCV_rw_w  -> "fence rw,w"
  | Barrier_RISCV_r_w   -> "fence r,w"
  | Barrier_RISCV_w_w   -> "fence w,w"
  | Barrier_RISCV_tso   -> "fence.tso"
  | Barrier_RISCV_i     -> "fence.i"
  (* x86 *)
  | Barrier_x86_MFENCE  -> "MFENCE"

let pp_write_kind m wk =
  match wk with
  | Write_plain                             -> "write"
  | Write_conditional                       -> "write conditional"
  | Write_release                           -> "write release"
  | Write_exclusive                         -> "write exclusive"
  | Write_exclusive_release                 -> "write exclusive-release"

  | Write_RISCV_release_RCpc                -> "write release-RCps"
  | Write_RISCV_release_RCsc                -> "write release-RCsc"
  | Write_RISCV_acquire_release             -> "write acquire-release"
  | Write_RISCV_conditional                 -> "write conditional"
  | Write_RISCV_conditional_release_RCpc    -> "write conditional-release-RCpc"
  | Write_RISCV_conditional_release_RCsc    -> "write conditional-release-RCsc"
  | Write_RISCV_conditional_acquire_release -> "write conditional-acquire-release"

  | Write_X86_locked                        -> "write lock'd"

let pp_brief_write_kind m wk =
  match wk with
  | Write_plain                            -> "W"
  | Write_conditional                      -> "WX"
  | Write_release                          -> "Wrel"
  | Write_exclusive                        -> "Wexc"
  | Write_exclusive_release                -> "We-r"

  | Write_RISCV_release_RCpc                -> "W.RCpc"
  | Write_RISCV_release_RCsc                -> "W.RCsc"
  | Write_RISCV_acquire_release             -> "W.aqrl"
  | Write_RISCV_conditional                 -> "Wcon"
  | Write_RISCV_conditional_release_RCpc    -> "Wcon.RCpc"
  | Write_RISCV_conditional_release_RCsc    -> "Wcon.RCsc"
  | Write_RISCV_conditional_acquire_release -> "Wcon.aqrl"

  | Write_X86_locked                       -> "Wlck"

let pp_read_kind m rk =
  match rk with
  | Read_plain                         -> "read"
  | Read_reserve                       -> "read reserve"
  | Read_acquire                       -> "read acquire"
  | Read_weak_acquire                  -> "read weak acquire"
  | Read_exclusive                     -> "read exclusive"
  | Read_exclusive_acquire             -> "read exclusive-acquire"
  | Read_stream                        -> "read non-temporal"

  | Read_RISCV_acquire_RCpc             -> "read acquire-RCpc"
  | Read_RISCV_acquire_RCsc             -> "read acquire-RCsc"
  | Read_RISCV_acquire_release          -> "read acquire-release"
  | Read_RISCV_reserved                 -> "read reserved"
  | Read_RISCV_reserved_acquire_RCpc    -> "read reserved-acquire-RCpc"
  | Read_RISCV_reserved_acquire_RCsc    -> "read reserved-acquire-RCsc"
  | Read_RISCV_reserved_acquire_release -> "read reserved-acquire-release"

  | Read_X86_locked                    -> "read lock'd"

let pp_brief_read_kind m rk =
  match rk with
  | Read_plain                         -> "R"
  | Read_reserve                       -> "RX"
  | Read_acquire                       -> "Racq"
  | Read_weak_acquire                  -> "Rwacq"
  | Read_exclusive                     -> "Rexc"
  | Read_exclusive_acquire             -> "Re-a"
  | Read_stream                        -> "Rnt"

  | Read_RISCV_acquire_RCpc             -> "R.RCpc"
  | Read_RISCV_acquire_RCsc             -> "R.RCsc"
  | Read_RISCV_acquire_release          -> "R.aqrl"
  | Read_RISCV_reserved                 -> "Rres"
  | Read_RISCV_reserved_acquire_RCpc    -> "Rres.RCpc"
  | Read_RISCV_reserved_acquire_RCsc    -> "Rres.RCsc"
  | Read_RISCV_reserved_acquire_release -> "Rres.aqrl"

  | Read_X86_locked                    -> "Rlck"

let pp_trans_kind _m = function
  | Transaction_start  -> "start transaction"
  | Transaction_commit -> "commit transaction"
  | Transaction_abort  -> "abort transaction"

let pp_instruction_kind m ik = match ik with
  | IK_barrier bk   -> pp_barrier_kind m bk
  | IK_mem_read rk  -> pp_read_kind m rk
  | IK_mem_write wk -> pp_write_kind m wk
  | IK_mem_rmw (rk, wk) -> (pp_read_kind m rk) ^ " and " ^ (pp_write_kind m wk)
  | IK_branch       -> "branch"
  | IK_trans tk     -> pp_trans_kind m tk
  | IK_simple       -> "simple"

let pp_nia ioid m nia =
  match nia with
  | NIA_successor -> "succ"
  | NIA_concrete_address a -> pp_address m (Some ioid) a
  | NIA_indirect_address -> "indirect"

let pp_dia m ioid dia =
  match dia with
  | DIA_none -> "none"
  | DIA_concrete_address a -> pp_address m (Some ioid) a
  | DIA_register reg_name ->
      begin match reg_name with
      | Reg (name,_,_,_) -> name
      | _ -> assert false
      end


(* with pretty ids *)

let pp_write_value m w =
  begin match w.w_value with
  | Some value -> pp_memory_value m w.w_ioid value
  | None       -> "(awaiting)"
  end

let pp_write_uncoloured m w =
  sprintf "%s%s %s=%s" (*(pp_pretty_write_id w.weiid)*) (pp_pretty_eiid m w.weiid) (pp_brief_write_kind m w.w_write_kind) (*w.w_Thread*) (pp_footprint m (Some w.w_ioid) w.w_addr) (pp_write_value m w)

let pp_writes_uncoloured m ws = String.concat " " (List.map (pp_write_uncoloured m) ws)

let pp_ui_view m = function
  | UI_Timestamp t -> string_of_int t
  | UI_Write_Ids ids -> pp_list m pp_eiid ids

let pp_t = string_of_int

let pp_view m = function
  | Timestamp t -> string_of_int t
  | Write_Ids (WI ids) -> 
     pp_list m pp_eiid (Pset.elements ids)


let pp_ui_c2_t m v = colour_changed2b_f m pp_ui_view v


let pp_writedata_uncoloured m wd =
  sprintf "%s%s %s=%s. Required view: %s"
    (pp_pretty_eiid m wd.wd_w.weiid)
    (pp_brief_write_kind m wd.wd_w.w_write_kind)
    (pp_footprint m (Some wd.wd_w.w_ioid) wd.wd_w.w_addr)
    (pp_write_value m wd.wd_w)
    (pp_view m wd.wd_req_view)

let pp_write_and_time_uncoloured m (w,t) =
  sprintf "%s%s %s=%s @ %s"
    (pp_pretty_eiid m w.weiid)
    (pp_brief_write_kind m w.w_write_kind)
    (pp_footprint m (Some w.w_ioid) w.w_addr)
    (pp_write_value m w)
    (pp_t t)

let pp_write_time_and_maybetid_uncoloured m (w,t,maybetid) =
  sprintf "%s%s %s=%s @ %s%s"
    (pp_pretty_eiid m w.weiid)
    (pp_brief_write_kind m w.w_write_kind)
    (pp_footprint m (Some w.w_ioid) w.w_addr)
    (pp_write_value m w)
    (pp_t t)
    (match maybetid with
     | Some tid -> " (owned by " ^ string_of_int tid ^ ")"
     | None -> ""
    )

let pp_write_and_view_uncoloured m (w,v) =
  sprintf "%s%s %s=%s. View = %s"
    (pp_pretty_eiid m w.weiid)
    (pp_brief_write_kind m w.w_write_kind)
    (pp_footprint m (Some w.w_ioid) w.w_addr)
    (pp_write_value m w)
    (pp_view m v)

let pp_write_and_view_uncoloured' m (w,v) =
  sprintf "%s%s %s=%s. View = %s"
    (pp_pretty_eiid m w.weiid)
    (pp_brief_write_kind m w.w_write_kind)
    (pp_footprint m (Some w.w_ioid) w.w_addr)
    (pp_write_value m w)
    (pp_ui_view m v)

let pp_transaction_start_uncoloured m ts =
  sprintf "%sTRANSACTION " (pp_pretty_eiid m ts.ts_eiid)

let pp_barrier_uncoloured  m b =
  sprintf "%s%s " (*(pp_pretty_barrier_id  b.beiid)*) (pp_pretty_eiid m b.beiid) (pp_barrier_kind m b.b_barrier_kind)

let pp_read_uncoloured_prefix m r =
  sprintf "%s%s"
    (*(pp_pretty_read_id r.reiid)*)
    (pp_pretty_eiid m r.reiid)
    (pp_brief_read_kind m r.r_read_kind)

let pp_read_uncoloured m r =
  sprintf "%s %s"
    (pp_read_uncoloured_prefix m r)
    (*w.w_Thread*)
    (pp_footprint m (Some r.r_ioid) r.r_addr)


let pp_reads_uncoloured m rs = String.concat "; " (List.map (pp_read_uncoloured m) rs)

let pp_ordered_reads m (rr1, rr2) =
  sprintf "%s -> %s" (pp_read_uncoloured m rr1) (pp_read_uncoloured m rr2)

let pp_slice m (i1,i2) = sprintf "%d-%d" i1 i2

let pp_slice' m (i1,i2) = if i1 = i2 then sprintf "[%d]" i1 else sprintf "[%d-%d]" i1 i2

let pp_read_with_slices_uncoloured m r unsat_slices =
  pp_read_uncoloured m r ^
  if unsat_slices = [MachineDefFragments.complete_slice r.r_addr] then ""
  else " " ^ (pp_list m (pp_slice' m) unsat_slices)

let pp_read_with_slices_and_view_uncoloured m r unsat_slices view =
  pp_read_uncoloured m r ^ "(view>=" ^ pp_view m view ^ ")" ^ 
  if unsat_slices = [MachineDefFragments.complete_slice r.r_addr] then ""
  else " " ^ (pp_list m (pp_slice' m) unsat_slices)

let pp_write_slice m write slice =
  let slice_value = MachineDefFragments.value_of_write_slices [(write, [slice])] in
  let slice_footprint = MachineDefFragments.footprint_of_write_slice write slice in
  sprintf "[%s=%s]" (pp_footprint m (Some write.w_ioid) slice_footprint) (pp_memory_value m write.w_ioid slice_value)
(* ^ sprintf "[%s,%s]" (pp_slice m slice) (pp_memory_value m slice_value) *)

let pp_write_slices_uncoloured m (w, slices) =
  pp_write_uncoloured m w ^
  if slices = [MachineDefFragments.complete_slice w.w_addr] then ""
  else " " ^ String.concat "," (List.map (pp_write_slice m w) slices)

let pp_mrs_uncoloured m r_ioid mrs =
(*  pp_footprint m mrs.mrs_footprint
  ^ " = "
^ *)  pp_memory_value m r_ioid mrs.mrs_value
  ^" from "
  ^ String.concat "; "
      (List.map
         (pp_write_slices_uncoloured m)
         mrs.mrs_writes_read_from
      )

let pp_mrss_uncoloured m r_ioid mrss =
  String.concat "," (List.map (pp_mrs_uncoloured m r_ioid) mrss)

let pp_memory_mrs_uncoloured m r_ioid mrs =
  if ! Debug.debug then pp_mrs_uncoloured m r_ioid mrs
  else sprintf "%s=%s" (pp_footprint m (Some r_ioid) mrs.mrs_footprint) (pp_memory_value m r_ioid mrs.mrs_value)


let pp_flowing_event_uncoloured m event =
  match event with
  | FWrite write -> pp_write_uncoloured m write
  | FFWrite _ -> assert false
  | FRead (read, _, writes) ->
      sprintf "%s%s"
        (pp_read_uncoloured m read)
        (if writes = [] then "" else
            " [" ^ String.concat "," (List.map (pp_write_slices_uncoloured m) writes) ^ "]")
  | FBarrier barrier -> pp_barrier_uncoloured m barrier
  | FTStart ts -> pp_transaction_start_uncoloured m ts

let pp_vf m ioid vf =
  let (sl,mv) = vf in
  pp_slice m sl ^ ":" ^
  match mv with
  | None -> "None"
  | Some (v:Sail_impl_base.bit_lifted list) ->
      (* TODO: this is a ghastly hack to make a register_value from the lifted-bit list we have to hand *)
      let rv = { rv_bits=v;rv_dir=D_increasing; rv_start=0; rv_start_internal=0 }  in
      pp_register_value m ioid rv

let pp_vfs m ioid vfs = String.concat ", " (List.map (pp_vf m ioid) vfs)

let pp_rrs m ioid rrs =
  String.concat ","
    (List.map
       (fun rrse ->
         match rrse with
         | RRS_instruction (ioid', _, vfs) ->
             (match m.pp_style with
             | Globals.Ppstyle_full | Globals.Ppstyle_screenshot ->
                 sprintf "i%s of %s" (pp_pretty_ioid ioid') (pp_vfs m ioid vfs)
             | Globals.Ppstyle_compact ->
                 sprintf "%s" (pp_pretty_ioid ioid))
         | RRS_initial_state vfs ->
             (match m.pp_style with
             | Globals.Ppstyle_full | Globals.Ppstyle_screenshot ->
                 sprintf "initialstate of %s" (pp_vfs m ioid vfs)
             | Globals.Ppstyle_compact ->
                 sprintf "initialstate" )
         | RRS_pseudoregister -> sprintf "pseudoregister")
       rrs)

let rrs_all_from_pseudoregisters rrs =
  List.for_all (function rrse -> match rrse with RRS_pseudoregister -> true | _ -> false) rrs

let optionally_hide_pseudoregister_reads m reg_reads =
  if m.pp_hide_pseudoregister_reads then
    List.filter (function (reg,rrs,v) -> not (rrs_all_from_pseudoregisters rrs)) reg_reads
  else
    reg_reads

let optionally_hide_ui_pseudoregister_reads m ui_reg_reads =
  if m.pp_hide_pseudoregister_reads then
    List.filter
      (function ui_reg_read ->
        match ui_reg_read with
        | C3_new (reg,rrs,v) -> not (rrs_all_from_pseudoregisters rrs)
        | C3_gone (reg,rrs,v) -> not (rrs_all_from_pseudoregisters rrs)
        | C3_unchanged(reg,rrs,v) -> not (rrs_all_from_pseudoregisters rrs)
      )
      ui_reg_reads
  else
    ui_reg_reads

let pp_reg_read ioid m reg_read =
  match reg_read with
     | (reg,rrs,v) ->
         sprintf "%s=%s from %s" (pp_reg m reg) (pp_register_value m ioid v) (pp_rrs m ioid rrs)

let pp_reg_reads ioid m reg_reads =
  String.concat ", " (List.map (pp_reg_read ioid m) (optionally_hide_pseudoregister_reads m reg_reads))



(** ************************************************ *)
(** pp of before/after register and memory snapshots *)
(** ************************************************ *)

let pp_logfile_address a =
  Printing_functions.logfile_address_to_string a

let pp_logfile_footprint (a,sz) =
  pp_logfile_address a

let pp_logfile_register_value rv =
  Printing_functions.logfile_register_value_to_string rv

let pp_logfile_memory_value mv =
  Printing_functions.logfile_memory_value_to_string (Globals.get_endianness ()) mv

let pp_logfile_register_maybe_value mv =
  match mv with
  | None -> pad (String.length "0x0000000000000000") "blocked"
  | Some v -> Printing_functions.logfile_register_value_to_string v

let pp_logfile_register_line rbn mv mv' =
  sprintf "%s %s %s\n"
    rbn
    (pp_logfile_register_maybe_value mv)
    (pp_logfile_register_maybe_value mv')

let pp_logfile_memory_line a i v v' =
  (sprintf "%s %s %s\n")
    a
    (pp_logfile_memory_value v)
    (pp_logfile_memory_value v')
  (*
  sprintf "MEM%d %s %s\n"
(*  sprintf "%s %s %s\n"
    (pp_logfile_footprint a)*)
    i
    (pp_logfile_memory_value v)
    (pp_logfile_memory_value v')
   *)


let pp_logfile m (ioid, rs, ms) (ioid', rs', ms') =
  String.concat ""
    (List.map2
       (fun (rbn,mv) (rbn',mv') ->
         if rbn <> rbn' then Warn.fatal "pp_logfile register names don't match" else
         pp_logfile_register_line rbn mv mv'
       )
       rs rs') ^

  String.concat ""
    (List.mapi
      (fun i (((a,sz), v), ((a',sz'), v')) ->
          if a <> a' then Warn.fatal "pp_logfile addresses don't match" else
          if sz <> 8 || sz' <> 8 then Warn.fatal "pp_logfile footprints of non-64bit size" else
          let (addr_pp,addr_pp') = (pp_address m (Some ioid) a,pp_address m (Some ioid') a') in
          if addr_pp <> addr_pp' then Warn.fatal "pp'ed addresses don't match" else
          pp_logfile_memory_line addr_pp i v v')
      (List.combine ms ms'))


(** ***************** *)
(** pp of transitions *)
(** ***************** *)

let pp_exception m ioid (e: exception_type) =
  begin match e with
  | ET_read_from_unmapped_memory (read_request, slices) ->
      sprintf "unmapped memory read exception: %s"
        (pp_read_with_slices_uncoloured m read_request slices)
  | ET_write_to_unmapped_memory writes ->
      sprintf "unmapped memory write exception: %s"
        (String.concat "; "
          (List.map
            (pp_write_uncoloured m)
            writes))
  | ET_fetch_and_decode FDE_non_concrete_fetch_address_error ->
      "non concrete fetch address error"
  | ET_fetch_and_decode (FDE_illegal_fetch_address_error address) ->
      sprintf "illegal fetch address exception: %s" (pp_address m (Some ioid) address)
  | ET_fetch_and_decode (FDE_decode_error (decode_error, address)) ->
      sprintf "decode error: %s" (pp_decode_error m decode_error address)
  | ET_loop_limit_reached ->
      "reached loop limit"
  | ET_ISA_termination msg ->
      "ISA termination: " ^ msg
  end

let pp_register_snapshot m ioid rs =
"register snapshot:\n" ^ String.concat "" (List.map (fun (rbn,mv) -> sprintf "%s = %s\n" rbn (match mv with None -> "blocked" | Some v -> (pp_register_value m ioid v))) rs)

let pp_memory_snapshot m ioid ms =
"memory snapshot:\n" ^ String.concat "" (List.map (fun (a,vs) -> sprintf "%s = %s\n" (pp_footprint m (Some ioid) a) (String.concat ", " (List.map (pp_memory_value m ioid) vs))) ms)

let pp_maybe_opcode m mop =
  match mop with
  | None -> ""
  | Some op -> sprintf "(opcode: %s)" (pp_opcode m op)

let pp_fdo ?(suppress_opcode=false) m fdo addr =
  match fdo with
  | FDO_success (a,mop,inst) ->
      sprintf "%s%s"
        (pp_instruction m.pp_symbol_table inst addr)
        (if not suppress_opcode then " " ^ (pp_maybe_opcode m mop) else "")
  | FDO_illegal_fetch_address ->
        let _ = Debug.print_log () in
     "illegal fetch address"
  | FDO_decode_error de -> "decode error: " ^ pp_decode_error m de addr
  | FDO_address_not_concrete -> "decode error: address is not concrete"


let pp_ss_only_label ?(graph=false) (m: Globals.ppmode) t =
  match t with
  | SS_PLDI11_partial_coherence_commit (w1,w2) ->
      let info =
        sprintf "%s %s %s"
          (colour_memory_action m (pp_write_uncoloured m w1))
          (pp_arrow m)
          (colour_memory_action m (pp_write_uncoloured m w2))
      in
      ("partial coherence commit", Some info)

  | SS_PLDI11_propagate_write_to_thread ((w,sls),tid) ->
      let info =
        sprintf "%s to Thread %d"
          (colour_memory_action m (pp_write_slices_uncoloured m (w,sls)))
          tid
      in
      ("propagate write to thread", Some info)

  | SS_PLDI11_write_reaches_coherence_point w ->
      let info = colour_memory_action m (pp_write_uncoloured m w) in
      ("write reaches coherences point", Some info)

  | SS_PLDI11_propagate_barrier_to_thread (b,tid) ->
      let info =
        sprintf "%s to Thread %d"
          (colour_memory_action m (pp_barrier_uncoloured m b))
          tid
      in
      ("propagate barrier to thread", Some info)

  | SS_POP_propagate_event_to_thread (event, tid) ->
      let info =
        sprintf "%s to Thread %d "
          (colour_memory_action m (pp_flowing_event_uncoloured m event))
          tid
      in
      ("propagate event to thread", Some info)

  | SS_Flowing_partially_satisfy_read (read, writes_read_from)
  | SS_POP_partially_satisfy_read     (read, writes_read_from) ->
      let info =
        sprintf "%s from %s"
          (pp_read_uncoloured m read)
          (String.concat "; "
            (List.map
              (pp_write_slices_uncoloured m)
              writes_read_from))
      in
      ("partially satisfy read", Some info)

  | SS_NOP_constrain_order (w1, w2) ->
      let info =
        sprintf "%s %s %s"
          (pp_write_uncoloured m w1)
          (pp_arrow m)
          (pp_write_uncoloured m w2)
      in
      ("constrain coherence order", Some info)

  | SS_NOP_propagate_everything ->
     ("propagate everything", None)

  | SS_Flowing_flow_write_to_memory write ->
      let info = colour_memory_action m (pp_write_uncoloured m write) in
      ("flow write to memory", Some info)

  | SS_Flowing_flow_barrier_to_memory barrier ->
      let info = colour_memory_action m (pp_barrier_uncoloured m barrier) in
      ("flow barrier to memory", Some info)

  | SS_Flowing_flow_satisfied_read_to_memory read ->
      let info = colour_memory_action m (pp_read_uncoloured m read) in
      ("flow satisfied read to memory", Some info)

  | SS_Flowing_reorder_events (event, event_after) ->
      let info =
        sprintf "%s and %s"
          (colour_memory_action m (pp_flowing_event_uncoloured m event))
          (colour_memory_action m (pp_flowing_event_uncoloured m event_after))
      in
      ("reorder events", Some info)

  | SS_Flowing_flow_to_segment event ->
      let info = colour_memory_action m (pp_flowing_event_uncoloured m event) in
      ("flow event", Some info)

  | SS_TSO_propagate_write_to_memory write ->
      let info = colour_memory_action m (pp_write_uncoloured m write) in
      ("propagate write to memory", Some info)

  | SS_Promising_stop_promising -> ("stop promising", None)


let pp_ss_sync_label ?(graph=false) m t =
  match t with
  | SS_PLDI11_acknowledge_sync_barrier b ->
      let info =
        sprintf "%s %s"
          (pp_barrier_kind m b.b_barrier_kind)
          (colour_memory_action m (pp_barrier_uncoloured m b))
      in
      ("acknowledge sync barrier", Some info)

  | SS_POP_read_response     (read_request, source)
  | SS_Flowing_seg_read_response (read_request, source)
  | SS_Flowing_mem_read_response (read_request, source) ->
      let info =
        sprintf "%s from %s"
          (colour_memory_action m (pp_read_uncoloured m read_request))
          (colour_memory_action m (pp_mrs_uncoloured m read_request.r_ioid source))
      in
      ("memory read request response from storage", Some info)

  | SS_NOP_read_response_segment (read_request, source) ->
      let info =
        sprintf "%s from %s"
          (colour_memory_action m (pp_read_uncoloured m read_request))
          (colour_memory_action m (pp_mrs_uncoloured m read_request.r_ioid source))
      in
      ("memory read request response from storage-segment", Some info)

  | SS_NOP_read_response_memory (read_request, source, before, after) ->
      let info =
        sprintf "%s from %s, ordering %s before %s"
          (colour_memory_action m (pp_read_uncoloured m read_request))
          (colour_memory_action m (pp_mrs_uncoloured m read_request.r_ioid source))
          (pp_setlist m pp_flowing_event_uncoloured (Pset.elements before))
          (colour_memory_action m (pp_mrs_uncoloured m read_request.r_ioid source))
      in
      ("memory read request response from storage-memory", Some info)


let pp_thread_trans_prefix ?(graph=false) m tid ioid =
  if m.pp_trans_prefix then sprintf "%s " (pp_pretty_ioid_padded ioid)
  else ""


let pp_t_only_label ?(graph=false) m tl =
  let ioid = tl.tl_cont.tc_ioid in
  match tl.tl_label with
  | T_register_read (r,rrs,v) ->
      let info =
        sprintf "%s = %s from %s" (pp_reg m r) (pp_register_value m ioid v) (pp_rrs m ioid rrs)
      in
      ("register read", Some info)

  | T_register_write (r,v) ->
      let info = sprintf "%s = %s" (pp_reg m r) (pp_register_value m ioid v) in
      ("register write", Some info)

  | T_internal_outcome ->
      ("interpreter", None)

  | T_pending_memory_read_request ->
      ("initiate memory reads of load instruction", None)

  | T_pseudoreg_read (r,v) ->
      let info = sprintf "%s = %s" (pp_reg m r) (pp_register_value m ioid v) in
      ("pseudoregister read", Some info)

  | T_pseudoreg_write (r,v) ->
      let info = sprintf "%s = %s" (pp_reg m r) (pp_register_value m ioid v) in
      ("pseudoregister write", Some info)

  | T_footprint_outcome ->
      ("footprint", None)

  | T_actually_satisfy v ->
      let info = pp_memory_value m ioid v in
      ("complete load instruction", Some info)

  | T_mem_forward_write (r, write_slices) ->
      let info =
        sprintf "%s from %s"
          (colour_memory_action m (pp_read_uncoloured m r))
          (pp_list m (pp_write_slices_uncoloured m) write_slices)
      in
      ("satisfy memory read by forwarding from writes", Some info)

  | T_mem_potential_write ws ->
      let info = colour_memory_action m (pp_writes_uncoloured m ws) in
      ("instantiate memory write values of store instruction", Some info)

  | T_finish ((addr,instr), mrs, mms) ->
      let info =
        match (mrs, mms) with
        | (None,    None)    -> None
        | (Some rs, None)    -> Some "[register_snapshot]" (* "\n" ^ pp_register_snapshot m rs *)
        | (Some rs, Some ms) -> Some "[register_snapshot, memory_snapshot]"
        | (None,    Some ms) -> Some "[memory_snapshot]" (* "\n" ^ pp_memory_snapshot m ms *)
      in
      
      let instr_pped = pp_instruction m.pp_symbol_table instr addr in
      ("finish instruction: " ^ instr_pped, info)

  | T_finish_load_of_rmw ->
      ("finish the load part of RMW instruction", None)
  | T_exception exception_type ->
      ("raise exception", Some (pp_exception m ioid exception_type))

  | T_mem_write_footprint writes ->
      let info =
        let pp_write_addresses_uncoloured m ws = String.concat " " (List.map (pp_write_uncoloured m) ws) in
        (colour_memory_action m (pp_write_addresses_uncoloured m writes))
      in
      ("initiate memory writes of store instruction, with their footprints", Some info)

  | T_commit_store -> ("commit store instruction", None)

  | T_complete_store -> ("complete store instruction", None)

  | T_successful_store_excl -> ("guarantee success of store-exclusive instruction", None)

  | T_potential_store_cond -> ("potential store-conditional", None)

  | T_failed_store_excl ->
      begin match !(Globals.model_params).t.thread_isa_info.ism with
      | AARCH64_ism _ -> ("failed store-exclusive instruction", None)
      | RISCV_ism     -> ("failed store-conditional instruction", None)
      | PPCGEN_ism    -> failwith "not implemented for PPC"
      | MIPS_ism      -> failwith "not implemented for PPC"
      | X86_ism       -> failwith "not implemented for PPC"
      end

  | T_prev_excl_result s ->
      let info = if s then "(successful)" else "(failed)" in
      ("previously determined store-exclusive result", Some info)

  | T_commit_barrier b ->
      ("commit barrier", Some (colour_memory_action m (pp_barrier_uncoloured m b)))

  | T_POP_subsumed_write write ->
      ("subsumed write", Some (colour_memory_action m (pp_write_uncoloured m write)))

  | T_RISCV_atomic_begin ->
      ("begin atomic memory access of AMO", None)

  | T_RISCV_atomic_end ->
      ("end atomic memory access of AMO", None)

let pp_t_sync_label ?(graph=false) m t =
  match t with
  | T_try_store_excl {tl_suppl = None} -> assert false
  | T_try_store_excl _ ->
      ("guarantee success of store-exclusive instruction", None)

  | T_mem_read_request {tl_suppl = None} -> assert false
  | T_mem_read_request {tl_label = (r, unsat_slices, _, _); tl_suppl = Some s; tl_cont = tc} ->
      let info =
        (if not s then "(unmapped memory) " else "") ^
        (colour_memory_action m (pp_read_with_slices_uncoloured m r unsat_slices))
      in
      ("memory read request from storage", Some info)

  | T_PLDI11_mem_satisfy_read {tl_suppl = None} -> assert false
  | T_PLDI11_mem_satisfy_read {tl_label = (r,unsat_slices); tl_suppl = Some mrss; tl_cont = tc} ->
      let info =
        sprintf "%s = [%s]"
          (colour_memory_action m (pp_read_with_slices_uncoloured m r unsat_slices))
          (colour_memory_action m (pp_mrss_uncoloured m tc.tc_ioid mrss))
      in
      ("satisfy memory read from storage", Some info)

  | T_Flat_mem_satisfy_read {tl_suppl = None} -> assert false
  | T_Flat_mem_satisfy_read {tl_label = (r, unsat_slices, _, _); tl_suppl = Some (Some mrss); tl_cont = tc} ->
      let info =
        sprintf "%s = [%s]"
          (colour_memory_action m (pp_read_with_slices_uncoloured m r unsat_slices))
          (colour_memory_action m (pp_mrss_uncoloured m tc.tc_ioid mrss))
      in
      ("satisfy memory read from memory", Some info)
  | T_Flat_mem_satisfy_read {tl_label = (r, unsat_slices, _, _); tl_suppl = Some None; tl_cont = tc} ->
      let info =
        "(unmapped memory) " ^
        (colour_memory_action m (pp_read_with_slices_uncoloured m r unsat_slices))
      in
      ("satisfy memory read from memory", Some info)

  | T_Flat_try_commit_store_cond {tl_suppl = None} -> assert false
  | T_Flat_try_commit_store_cond {tl_label = (write, _); tl_suppl = Some MWO_successful _} ->
     let info = colour_memory_action m (pp_write_uncoloured m write) in
     ("successfully commit and propagate store-conditional to memory", Some info)
  | T_Flat_try_commit_store_cond {tl_label = (write, _); tl_suppl = Some (MWO_unmapped_address _)} ->
      (* TODO: print the MWO_unmapped_address _ *)
      let info = colour_memory_action m (pp_write_uncoloured m write) in
      ("commit and propagate store-conditional to memory (unmapped address)", Some info)
  | T_Flat_try_commit_store_cond {tl_label = (write, _); tl_suppl = Some MWO_exclusive_failed} ->
      let info = colour_memory_action m (pp_write_uncoloured m write) in
      ("failed store-conditional instruction", Some info)

  | T_Promising_mem_satisfy_read {tl_suppl = None}
  | T_Promising_mem_satisfy_read_nonshared {tl_suppl = None} -> assert false

  | T_Promising_mem_satisfy_read {tl_label = ((r,t)); tl_suppl = Some (Some (w,wt)); tl_cont = tc}
  | T_Promising_mem_satisfy_read_nonshared {tl_label = ((r,t)); tl_suppl = Some (Some (w,wt)); tl_cont = tc} ->
      let info =
        sprintf "%s = [%s]"
        (* sprintf (if lock then "%s = [%s] and lock" else "%s = [%s]") *)
          (colour_memory_action m (pp_read_with_slices_and_view_uncoloured m r [] t))
          (colour_memory_action m (pp_write_and_view_uncoloured m (w,wt)))
      in
      ("satisfy memory read", Some info)

  | T_Promising_mem_satisfy_read {tl_label = ((r,t)); tl_suppl = Some None; tl_cont = tc}
  | T_Promising_mem_satisfy_read_nonshared {tl_label = ((r,t)); tl_suppl = Some None; tl_cont = tc} ->
      let info =
        "(unmapped memory) " ^
          (colour_memory_action m (pp_read_with_slices_and_view_uncoloured m r [] t))
      in
      ("satisfy memory read from memory", Some info)

  | T_TSO_mem_satisfy_read {tl_suppl = None} -> assert false
  | T_TSO_mem_satisfy_read {tl_label = r; tl_suppl = Some (Some mrss); tl_cont = tc} ->
      let info =
        sprintf "%s = [%s]"
          (colour_memory_action m (pp_read_uncoloured m r))
          (colour_memory_action m (pp_mrss_uncoloured m tc.tc_ioid mrss))
      in
      ("satisfy memory read from memory", Some info)
  | T_TSO_mem_satisfy_read {tl_label = r; tl_suppl = Some None; tl_cont = tc} ->
      let info =
        "(unmapped memory) " ^
        (colour_memory_action m (pp_read_uncoloured m r))
      in
      ("satisfy memory read from memory", Some info)

  | T_propagate_write {tl_suppl = None} -> assert false
  | T_propagate_write {tl_label = (write, None, _); tl_suppl = Some MWO_successful _; tl_cont = tc} ->
     let info = colour_memory_action m (pp_write_uncoloured m write) in
     ("propagate memory write to storage", Some info)
  | T_propagate_write {tl_label = (write, Some r, _); tl_suppl = Some MWO_successful _; tl_cont = tc} ->
      let info =
        sprintf "%s - %s"
          (colour_memory_action m (pp_write_uncoloured m write))
          (pp_read_uncoloured m r)
      in
      ("propagate memory write to storage", Some info)
  | T_propagate_write {tl_label = (write, _, _); tl_suppl = Some (MWO_unmapped_address _); tl_cont = tc} ->
      (* TODO: print the MWO_unmapped_address _ *)
      let info = colour_memory_action m (pp_write_uncoloured m write) in
      ("propagate memory write to storage (unmapped address)", Some info)
  | T_propagate_write {tl_suppl = Some MWO_exclusive_failed} -> assert false

  | T_Promising_fulfil_promise {tl_label = wd; tl_suppl = _; tl_cont = tc} ->
      let info = colour_memory_action m (pp_writedata_uncoloured m wd) in
     ("fulfill promise", Some info)

  | T_Promising_propagate_write {tl_suppl = None}
  | T_Promising_propagate_write_nonshared {tl_suppl = None} -> assert false

  | T_Promising_propagate_write {tl_label = (wd,true); tl_suppl = Some MWO_successful _; tl_cont = tc} ->
      let info = colour_memory_action m (pp_writedata_uncoloured m wd) in
      ("promise write", Some info)

  | T_Promising_propagate_write {tl_label = (wd,false); tl_suppl = Some MWO_successful _; tl_cont = tc}
  | T_Promising_propagate_write_nonshared {tl_label = wd; tl_suppl = Some MWO_successful _; tl_cont = tc} ->
      let info = colour_memory_action m (pp_writedata_uncoloured m wd) in
      ("propagate memory write to storage", Some info)

  | T_Promising_propagate_write {tl_label = (wd,true); tl_suppl = Some (MWO_unmapped_address _); tl_cont = tc} ->
      let info = colour_memory_action m (pp_writedata_uncoloured m wd) in
      ("promise write (unmapped address)", Some info)

  | T_Promising_propagate_write {tl_label = (wd,false); tl_suppl = Some (MWO_unmapped_address _); tl_cont = tc}
  | T_Promising_propagate_write_nonshared {tl_label = wd; tl_suppl = Some (MWO_unmapped_address _); tl_cont = tc} ->
      let info = colour_memory_action m (pp_writedata_uncoloured m wd) in
      ("propagate memory write to storage (unmapped address)", Some info)

  | T_Promising_propagate_write {tl_suppl = Some MWO_exclusive_failed} 
  | T_Promising_propagate_write_nonshared {tl_suppl = Some MWO_exclusive_failed} -> assert false

  | T_propagate_barrier {tl_suppl = None} -> assert false
  | T_propagate_barrier {tl_label = b; tl_cont = tc} ->
      let info = colour_memory_action m (pp_barrier_uncoloured m b) in
      ("propagate barrier to storage", Some info)

  | T_fetch {tl_suppl = None} -> assert false
  | T_fetch {tl_label = (a, _); tl_suppl = (Some fdo); tl_cont = tc} ->
      let info =
        sprintf "%s %s%s"
          (pp_address m (Some tc.tc_ioid) a)
          (pp_fdo ~suppress_opcode:graph m fdo a)
          begin match m.pp_dwarf_static with
          | Some ds ->
              begin match pp_dwarf_source_file_lines m ds (* pp_actual_line: *) false a with 
              | Some s -> sprintf " [%s]" s
              | None -> ""
              end
          | None -> ""
          end
      in
      ("fetch instruction", Some info)

  | T_POP_tm_start {tl_suppl = None} -> assert false
  | T_POP_tm_start {tl_cont = tc} ->
      ("start memory transaction", None)

  | T_POP_tm_commit {tl_suppl = None} -> assert false
  | T_POP_tm_commit {tl_cont = tc} ->
      ("commit memory transaction", None)

  | T_POP_tm_abort {tl_suppl = None} -> assert false
  | T_POP_tm_abort {tl_label = (_, v); tl_cont = tc} ->
      let info = Printing_functions.register_value_to_string v in
      ("abort memory transaction", Some info)


let pp_t_thread_start_label ?(graph=false) m tl =
  let (r_address, r_toc) = tl.tl_label in
  begin match tl.tl_suppl with
  | Some (Some new_tid) ->
      let info =
        sprintf "addr = %s%s, new_tid = %d"
          (pp_register_value m tl.tl_cont.tc_ioid r_address)
          (match r_toc with
            | Some r_toc ->
                ", toc = " ^ pp_register_value m tl.tl_cont.tc_ioid r_toc
            | None -> "")
          new_tid
      in
      ("thread start", Some info)

  | Some None ->
      let info =
        sprintf "addr=%s%s"
          (pp_register_value m tl.tl_cont.tc_ioid r_address)
          (match r_toc with
            | Some r_toc ->
                ", toc=" ^ pp_register_value m tl.tl_cont.tc_ioid r_toc
            | None -> "")
      in
      ("thread start (unsuccessful)", Some info)

  | None -> assert false
  end


let pp_trans_label_only ?(graph=false) m (t: ('ts,'ss) trans) =
  begin match t with
  | Sys_trans _ -> "Finalise Promising execution"
  | SS_trans (SS_only (t, _))    -> fst (pp_ss_only_label ~graph m t)
  | SS_trans (SS_sync (t, _, _)) -> fst (pp_ss_sync_label ~graph m t)
  | T_trans (T_only tl)          -> fst (pp_t_only_label ~graph m tl)
  | T_trans (T_sync (t, _))      -> fst (pp_t_sync_label ~graph m t)
  | T_trans (T_thread_start tl)  -> fst (pp_t_thread_start_label ~graph m tl)
  end

let pp_ss_only_trans ?(graph=false) (m: Globals.ppmode) t =
  match pp_ss_only_label ~graph m t with
  | (label, None)      -> label
  | (label, Some info) -> label ^ ": " ^ info

let pp_ss_sync_trans ?(graph=false) m t =
  match pp_ss_sync_label ~graph m t with
  | (label, None)      -> label
  | (label, Some info) -> label ^ ": " ^ info

let pp_t_only_trans ?(graph=false) m tl =
  pp_thread_trans_prefix ~graph m tl.tl_cont.tc_tid tl.tl_cont.tc_ioid  ^
  match pp_t_only_label ~graph m tl with
  | (label, None)      -> label
  | (label, Some info) -> label ^ ": " ^ info

let pp_t_sync_trans ?(graph=false) m t =
  let tid = MachineDefTypes.tid_of_thread_sync_trans t in
  let ioid = ioid_of_thread_sync_trans t in
  pp_thread_trans_prefix ~graph m tid ioid  ^
  match pp_t_sync_label ~graph m t with
  | (label, None)      -> label
  | (label, Some info) -> label ^ ": " ^ info

let pp_t_thread_start_trans ?(graph=false) m tl =
  pp_thread_trans_prefix ~graph m tl.tl_cont.tc_tid tl.tl_cont.tc_ioid  ^
  match pp_t_thread_start_label ~graph m tl with
  | (label, None)      -> label
  | (label, Some info) -> label ^ ": " ^ info

let pp_trans ?(graph=false) m (t: ('ts,'ss) trans) =
  match t with
  | Sys_trans _ -> "Finalise Promising execution"
  | SS_trans (SS_only (t', _))    -> pp_ss_only_trans ~graph m t'
  | SS_trans (SS_sync (t', _, _)) -> pp_ss_sync_trans ~graph m t'
  | T_trans (T_only tl)           -> pp_t_only_trans ~graph m tl
  | T_trans (T_sync (t', _))      -> pp_t_sync_trans ~graph m t'
  | T_trans (T_thread_start tl)   -> pp_t_thread_start_trans ~graph m tl


let principal_ioid_of_event (fe:flowing_event) =
  match fe with
  | FWrite w        -> w.w_ioid
  | FFWrite _       -> assert false
  | FRead (r, _, _) -> r.r_ioid
  | FBarrier b      -> b.b_ioid
  | FTStart t       -> t.ts_ioid

let pp_tracked_event m e =
  match e with
  | SWrite (w,sls) -> pp_write_slices_uncoloured m (w,sls)
  | SBarrier b -> pp_barrier_uncoloured m b

let pp_coherence_edge m (w1,w2) =
   sprintf "%s %s %s" (pp_write_uncoloured m w1) (pp_arrow m) (pp_write_uncoloured m w2)

let pp_pop_order_constraints m (event1, event2) =
  sprintf "%s %s %s" (pp_flowing_event_uncoloured m event1) (pp_arrow m) (pp_flowing_event_uncoloured m event2)

let pp_store_exclusive_map_uncoloured m (write, prev_write) =
  sprintf "%s %s %s" (pp_write_uncoloured m write) (pp_Arrow m) (pp_write_uncoloured m prev_write)

let enlink m n s =
  let active_str =
    match m.Globals.pp_default_cmd with
    | Some (Interact_parser_base.Transitions [i]) when i = n -> " default"
    | _ -> ""
  in
  match m.Globals.pp_kind with
  | Html -> sprintf "<span class=\"trans%s\" id=\"%d\">%s</span>" active_str n s
  | _ -> s

let pp_cand m (n,t) =
  match m.Globals.pp_kind with
  | Html -> sprintf "  %s"  (enlink m n (colour_tran_id m ((sprintf "%-2d" n) ^ pp_trans m t)))
  | _ -> let s = (match m.Globals.pp_default_cmd with
                  | Some (Interact_parser_base.Transitions [i]) when i = n -> "***"
                  | _ -> "   ")
         in
         sprintf "%s %s    %s %s" s (colour_tran_id m (sprintf "%-2d" n)) (colour_tran_id m (pp_trans m t)) s

(** pp a UI storage subsystem state *)

let pldi11_pp_ui_storage_subsystem_state m model ss =

  (* don't explicitly print:  ui_threads : thread_id list; *)

  let ppd_writes_seen =
    pp_changed2_setlist_body m pp_write_uncoloured ss.ui_writes_seen in

  let ppd_coherence =
    pp_changed2_setlist_body m pp_coherence_edge ss.ui_coherence in

  let ppd_new_coherence =
    let dummy_ioid = (0,0) in (* HACK - ok because ppd_new_coherence is currently dead code *)
    String.concat ""
      (List.map
         (fun nce ->
           colour_changed3_f m
             (fun m -> fun (fp,coh) ->
               sprintf "%s %s %s\n"
                 (pp_footprint m (Some dummy_ioid) fp)
                 (pp_mapsto m)
                 (pp_changed2_setlist m pp_coherence_edge coh))
             nce)
         ss.ui_new_coherence) in

  let pp_ss_transitions ts =
    String.concat "\n" (List.map (pp_cand m) ts) ^ "\n" in

  let ppd_writes_past_coherence_point =
    (match m.pp_style with
    | Globals.Ppstyle_compact ->
        let non_init_writes =
          (List.filter
             (function cw -> (decolour2 cw).w_thread <> Test.init_thread)
             ss.ui_writes_past_coherence_point
          ) in
        (*pp_leftbrace m ^*) "...init writes..." ^ (if non_init_writes <> [] then ", " else "")
        ^ pp_changed2_setlist_body m pp_write_uncoloured non_init_writes
        (*^ pp_rightbrace m*)
    | _ ->
        pp_changed2_setlist_body m pp_write_uncoloured ss.ui_writes_past_coherence_point) in

  let ppd_events_propagated_to =
    (String.concat ""
       (List.map (function (tid,events) ->
         sprintf "    Thread %d: %s\n"
           tid
           (match m.pp_style with
           | Globals.Ppstyle_compact ->
               let non_init_events =
                 (List.filter
                    (function cte -> match decolour2 cte with | SWrite (w,_) -> w.w_thread <> Test.init_thread | SBarrier b -> b.b_thread <> Test.init_thread)
                    events
                 ) in
               "[" ^ "...init writes..." ^ (if non_init_events <> [] then ", " else "") ^
               pp_changed2_list_body m pp_tracked_event
                 non_init_events
               ^ "]"
           | _ ->
               (pp_changed2_list m pp_tracked_event events)
           ))
          ss.ui_events_propagated_to )) in

  let ppd_unacknowledged_sync_requests =
    pp_changed3_setlist m pp_barrier_uncoloured ss.ui_unacknowledged_sync_requests in

(*  let ppd_reservations =
    (String.concat ""
       (List.map (fun (tid,wopt,is_new) ->
         sprintf "    Thread %d: %s  " tid
           (pp_write_option m is_new wopt))
          ss.ui_reservations)) in
*)
  (* ignore for now:
    ui_ss_transitions_pcc: ui_trans list;
    ui_ss_transitions_cp: ui_trans list;
    ui_ss_transitions_prop: ui_trans list;
    ui_ss_transitions_ack_sync: ui_trans list;
*)

  if not (m.pp_style = Globals.Ppstyle_screenshot) then

      let res =
        match m.Globals.pp_kind with
        | Html -> sprintf "<b>Storage subsystem state:</b><br/> <b> writes seen:</b> %s <b> coherence:  </b> %s  %s<b>events propagated to</b>:<br/>%s <b> unacknowledged Sync requests:</b> %s "  (*<b> reservations </b> = %s*)
        | _ -> sprintf "%s:\n  writes seen: %s  coherence: %s  %s  events propagated to:\n%s  unacknowledged Sync requests: %s" (colour_bold m "Storage subsystem state")(*  reservations = %s*)
      in

    res
      (ppd_writes_seen
       ^ !linebreak)

      ((if model.ss.new_coh = Use_new_coherence then ""
      else  (ppd_coherence ^ !linebreak))
       ^ (if model.ss.new_coh = Use_old_coherence then "" else ppd_new_coherence)
       ^ pp_ss_transitions ss.ui_ss_transitions_pcc)

      ("writes past coherence point: "
       ^ ppd_writes_past_coherence_point
       ^ !linebreak
       ^ pp_ss_transitions ss.ui_ss_transitions_cp)

      (ppd_events_propagated_to
       ^ pp_ss_transitions ss.ui_ss_transitions_prop)

      (ppd_unacknowledged_sync_requests
       ^ !linebreak
       ^ pp_ss_transitions ss.ui_ss_transitions_ack_sync)

(*    (ppd_reservations
      ^ !linebreak
      )
 *)
  else

    let res =
      match m.Globals.pp_kind with
      | _ -> sprintf "%s:\n  writes seen: %s  coherence: %s  events propagated to:\n%s  unacknowledged Sync requests: %s"
            (colour_bold m "Storage subsystem state")(*  reservations = %s*)
    in

    res
      (ppd_writes_seen
       ^ !linebreak)

      ((if model.ss.new_coh = Use_new_coherence then ""
      else  (ppd_coherence ^ !linebreak))
       ^ (if model.ss.new_coh = Use_old_coherence then "" else ppd_new_coherence)
       ^ pp_ss_transitions ss.ui_ss_transitions_pcc)

      (*("writes_past_coherence_point = "
       ^ ppd_writes_past_coherence_point
       ^ !linebreak
       ^ pp_ss_transitions ss.ui_ss_transitions_cp)*)

      (ppd_events_propagated_to
       ^ pp_ss_transitions ss.ui_ss_transitions_prop)

      (ppd_unacknowledged_sync_requests
       ^ !linebreak
       ^ pp_ss_transitions ss.ui_ss_transitions_ack_sync)

(*    (ppd_reservations
      ^ !linebreak
      )
 *)


let flowing_pp_ui_storage_subsystem_state m model ss =
  let tree =
    let ext_pps = {
      Pp_flowing_tree.pp_colour_tran_id = colour_tran_id;
      enlink = enlink;
      pp_flowing_event = pp_flowing_event_uncoloured; } in

    Pp_flowing_tree.pp_rendered_tree m (Pp_flowing_tree.render_tree m ext_pps (Pp_flowing_tree.height ss.ui_flowing_ss_topology ss.ui_flowing_ss_buffers) ss.ui_flowing_ss_topology ss.ui_flowing_ss_buffers ss.ui_flowing_ss_segment_to_thread) in

  (*
  let reordered =
    pp_changed3_setlist m
      (fun m (fe, fe') ->
        sprintf "(%s, %s)"
          (pp_flowing_event_uncoloured m fe)
          (pp_flowing_event_uncoloured m fe'))
      ss.ui_flowing_ss_reordered
  in
  *)

  let memory =
    pp_changed3_list m pp_write_slices_uncoloured ss.ui_flowing_ss_memory_writes in

  let pp_ss_transitions ts =
    String.concat "\n" (List.map (pp_cand m) ts) ^ "\n" in

  (*begin match m.Globals.pp_kind with
  | Ascii ->*)
    String.concat ""
      [sprintf "%s:"                      (colour_bold m "Storage subsystem state (flowing)"); !linebreak;
      sprintf "%s"                       tree; !linebreak;
      (*sprintf "  Reordered: %s"          reordered; !linebreak;*)
      sprintf "  Memory = %s"            memory; !linebreak;
      sprintf "%s"                       (pp_ss_transitions ss.ui_flowing_transitions_reorder);
      sprintf "%s"                       (pp_ss_transitions ss.ui_flowing_transitions_flow_to_seg);
      sprintf "%s"                       (pp_ss_transitions ss.ui_flowing_transitions_write_to_mem);
      sprintf "%s"                       (pp_ss_transitions ss.ui_flowing_transitions_barrier_to_mem);
      sprintf "%s"                       (pp_ss_transitions ss.ui_flowing_transitions_satisfied_read_to_mem);
      sprintf "%s"                       (pp_ss_transitions ss.ui_flowing_transitions_partially_satisfy_read);
      sprintf "%s"                       (pp_ss_transitions ss.ui_flowing_transitions_read_response);
      ]
  (*| Html ->
  end*)


let flat_pp_ui_storage_subsystem_state m model ss =
  let memory =
    pp_changed3_list m pp_write_slices_uncoloured ss.ui_flat_ss_memory_writes in

  let old_writes = 
    pp_changed3_list m pp_write_uncoloured ss.ui_flat_ss_old_writes in

  (*begin match m.Globals.pp_kind with
  | Ascii ->*)
    String.concat ""
      [sprintf "%s:"                      (colour_bold m "Storage subsystem state (flat)"); !linebreak;
       sprintf "  Memory     = %s"            memory; !linebreak;
       if m.pp_kind <> Hash then "" else
       sprintf "  Old writes = %s"            old_writes; !linebreak
       ;
      ]
  (*| Html ->
  end*)

let pop_pp_ui_storage_subsystem_state m model ss =
  let events_seen =
    pp_changed3_setlist m pp_flowing_event_uncoloured ss.ui_pop_ss_events_seen in

  let order_constraints =
    pp_changed3_setlist m pp_pop_order_constraints ss.ui_pop_ss_order_constraints_closure in

  let ppd_events_propagated_to =
    (String.concat !linebreak
      (List.map
        (function (tid, events) ->
            (sprintf "    Thread %d: " tid) ^
            (pp_changed3_setlist m pp_flowing_event_uncoloured events))
        ss.ui_pop_ss_events_propagated_to)) in

  (*let store_exclusive_map =
    pp_changed2_setlist m pp_store_exclusive_map_uncoloured ss.ui_pop_ss_store_exclusive_map in*)

  let pp_ss_transitions ts =
    String.concat "\n" (List.map (pp_cand m) ts) ^ "\n" in

    (*begin match m.Globals.pp_kind with
    | Ascii ->*)
      String.concat ""
       [sprintf "%s:"                           (colour_bold m "Storage subsystem state (POP)"); !linebreak;
        sprintf "  events seen = %s"            events_seen; !linebreak;
        (*sprintf "  store exclusive pairs = %s"  store_exclusive_map; !linebreak;*)
        sprintf "  order constraints = %s"      order_constraints; !linebreak;
        sprintf "  events propagated to:";      !linebreak;
        sprintf      "%s"                       ppd_events_propagated_to; !linebreak;
        sprintf "%s"                            (pp_ss_transitions ss.ui_pop_ss_transitions_prop_event);
        sprintf "%s"                            (pp_ss_transitions ss.ui_pop_ss_transitions_partially_satisfy_read);
        sprintf "%s"                            (pp_ss_transitions ss.ui_pop_ss_transitions_read_response);
       ]
    (*| Html ->
    end*)


let nop_pp_ui_storage_subsystem_state m model ss =
  let events_seen =
    pp_changed3_setlist m pp_flowing_event_uncoloured ss.ui_nop_ss_events_seen in

  let order_constraints =
    pp_changed3_setlist m pp_pop_order_constraints ss.ui_nop_ss_order_constraints_closure in

  let ppd_events_propagated_to =
    (String.concat !linebreak
      (List.map
        (function (tid, events) ->
            (sprintf "    Thread %d: " tid) ^
            (pp_changed3_setlist m pp_flowing_event_uncoloured events))
        ss.ui_nop_ss_events_propagated_to)) in

  (*let store_exclusive_map =
    pp_changed2_setlist m pp_store_exclusive_map_uncoloured ss.ui_pop_ss_store_exclusive_map in*)

  let pp_ss_transitions ts =
    String.concat "\n" (List.map (pp_cand m) ts) ^ "\n" in

    (*begin match m.Globals.pp_kind with
    | Ascii ->*)
      String.concat ""
       [sprintf "%s:"                          (colour_bold m "Storage subsystem state (NOP)"); !linebreak;
        sprintf "  events seen = %s"           events_seen; !linebreak;
        (*sprintf "  store exclusive pairs = %s"  store_exclusive_map; !linebreak;*)
        sprintf "  order constraints = %s"     order_constraints; !linebreak;
        sprintf "  events propagated to:";     !linebreak;
        sprintf      "%s"                      ppd_events_propagated_to; !linebreak;
        sprintf "%s"                           (pp_ss_transitions ss.ui_nop_ss_transitions_constrain_order);
        sprintf "%s"                           (pp_ss_transitions  ss.ui_nop_ss_transitions_propagate_everything);
       ]
    (*| Html ->
    end*)

let tso_pp_ui_storage_subsystem_state m model ss =
  let tree =
    let ext_pps = {
      Pp_flowing_tree.pp_colour_tran_id = colour_tran_id;
      enlink = enlink;
      pp_flowing_event = pp_flowing_event_uncoloured; } in

    let (topology, buffers, segment_to_thread) = MachineDefUI.tso_ss_tree ss in
    Pp_flowing_tree.pp_rendered_tree m (Pp_flowing_tree.render_tree m ext_pps (Pp_flowing_tree.height topology buffers) topology buffers segment_to_thread) in

  let memory =
    pp_changed3_list m pp_write_slices_uncoloured ss.ui_tso_ss_memory_writes in

  let lock =
    let pp_lock _ = function
      | Some tid -> sprintf "locked by thread %d" tid
      | None -> "unlocked"
    in
    colour_changed2b_f m pp_lock ss.ui_tso_ss_lock
  in

  let pp_ss_transitions ts =
    String.concat "\n" (List.map (pp_cand m) ts) ^ "\n" in

  (*begin match m.Globals.pp_kind with
  | Ascii ->*)
    String.concat ""
      [sprintf "%s:"                     (colour_bold m "Storage subsystem state (TSO)"); !linebreak;
      sprintf "%s"                       tree; !linebreak;
      sprintf "  Memory = %s"            memory; !linebreak;
      sprintf "  Lock = %s"              lock; !linebreak;
      sprintf "%s"                       (pp_ss_transitions ss.ui_tso_transitions_propagate_write);
      ]
  (*| Html ->
  end*)



let pp_pssto_state m model ss =
  let memory =
    pp_changed3_list_body m
      pp_write_time_and_maybetid_uncoloured ss.ui_pssto_memory
  in

  let stopped_promising = 
    let pp _m = function
      | true -> "(stopped_promising)"
      | _ -> "(still promising)"
    in
    colour_changed2b_f m pp ss.ui_pssto_stopped_promising
  in

  let pp_ss_transitions ts =
    String.concat "\n" (List.map (pp_cand m) ts) ^ "\n" in

    (*begin match m.Globals.pp_kind with
    | Ascii ->*)
      String.concat ""
       [sprintf "%s:"                           (colour_bold m "Storage subsystem state (Promising)"); !linebreak;
        "memory:  " ^ memory; !linebreak;
        stopped_promising; !linebreak;
        sprintf "%s"                            (pp_ss_transitions ss.ui_pssto_transitions);
       ]
    (*| Html ->
    end*)

let pp_psspo_state m model ss =
  let writes =
    pp_changed3_list_body m pp_write_uncoloured ss.ui_psspo_writes
  in

  let write_order =
    pp_changed3_list_body m 
      (fun m (w,w') -> sprintf "%s -> %s" (pp_eiid w) (pp_eiid w'))
      ss.ui_psspo_write_order
  in

  let addr_view =
    let last_ioid = (-1,-1) in
    pp_changed3_list_body m (fun m (w,addr) -> 
        sprintf "%s -> %s"
        (pp_eiid w) (pp_footprint m (Some last_ioid) addr)
      )
      ss.ui_psspo_addr_view
  in

  let stopped_promising = 
    let pp _m = function
      | true -> "(stopped_promising)"
      | _ -> "(still promising)"
    in
    colour_changed2b_f m pp ss.ui_psspo_stopped_promising
  in

  let pp_ss_transitions ts =
    String.concat "\n" (List.map (pp_cand m) ts) ^ "\n" in

    (*begin match m.Globals.pp_kind with
    | Ascii ->*)
      String.concat ""
       [sprintf "%s:"                           (colour_bold m "Storage subsystem state (Promising)"); !linebreak;
        "writes:  " ^ writes; !linebreak;
        "write_order:  " ^ write_order; !linebreak;
        "addr_view:  " ^ addr_view; !linebreak;
        stopped_promising; !linebreak;
        sprintf "%s"                            (pp_ss_transitions ss.ui_psspo_transitions);
       ]
    (*| Html ->
    end*)


let promising_pp_ui_storage_subsystem_state m model ss =
  match ss with
  | PSSTO_UI ss -> pp_pssto_state m model ss
  | PSSPO_UI ss -> pp_psspo_state m model ss



let pp_ui_storage_subsystem_state m model ss =
  match ss with
  | PLDI11_UI_storage  ui_storage_subsystem -> pldi11_pp_ui_storage_subsystem_state  m model ui_storage_subsystem
  | Flowing_UI_storage ui_storage_subsystem -> flowing_pp_ui_storage_subsystem_state m model ui_storage_subsystem
  | Flat_UI_storage    ui_storage_subsystem -> flat_pp_ui_storage_subsystem_state    m model ui_storage_subsystem
  | POP_UI_storage     ui_storage_subsystem -> pop_pp_ui_storage_subsystem_state     m model ui_storage_subsystem
  | NOP_UI_storage     ui_storage_subsystem -> nop_pp_ui_storage_subsystem_state     m model ui_storage_subsystem
  | TSO_UI_storage     ui_storage_subsystem -> tso_pp_ui_storage_subsystem_state     m model ui_storage_subsystem
  | Promising_UI_storage ui_storage_subsystem -> promising_pp_ui_storage_subsystem_state m model ui_storage_subsystem

(** pp an instruction instance state *)

let pp_unsat_reads m subreads =
  let pp_rr_unsat_reads (rr, unsat_slices) =
    (pp_read_uncoloured m rr) ^ ": " ^ (pp_changed3_list m pp_slice unsat_slices)
  in
  "[" ^ (pp_list m pp_rr_unsat_reads subreads.ui_sr_unsat_slices) ^ "]"

let pp_writes_read_from_body m subreads =
  let pp_rr_writes_read_from (rr, write_slices) =
    (* abbreviate if the read response reads from exactly one write and they have the same footprint *)
    let abbreviate =
      match write_slices with
      | [ C3_new (w, slices) ]
      | [ C3_unchanged (w, slices) ]
      | [ C3_gone (w, slices) ]
        ->  rr.r_addr = w.w_addr &&  slices = [MachineDefFragments.complete_slice w.w_addr]
      | _ -> false
    in
    (if abbreviate then (pp_read_uncoloured_prefix m rr) else (pp_read_uncoloured m rr))
    ^ " from " ^ (pp_changed3_list_bis m pp_write_slices_uncoloured write_slices)
  in
  (pp_list m pp_rr_writes_read_from subreads.ui_sr_writes_read_from)

let pp_writes_read_from m subreads =
  "[" ^ pp_writes_read_from_body m subreads ^ "]"


let pp_requested_reads m subreads =
  let pp_rr_requested (rr, unsat_slices) =
    (pp_read_uncoloured m rr) ^ ": " ^ (pp_changed3_list m pp_slice unsat_slices)
  in
  "[" ^ (pp_list m pp_rr_requested subreads.ui_sr_requested) ^ "]"

let pp_micro_op_state_top indent ioid m mos =
  if not(m.pp_style = Globals.Ppstyle_screenshot) then
    match mos with
    | MOS_plain is              -> "MOS_plain"
    | MOS_pending_mem_read ic   -> "MOS_pending_mem_read"
    | MOS_potential_mem_write c -> "MOS_pending_mem_write"
    | MOS_AMO_lock ic           -> "MOS_AMO_lock"
    | MOS_AMO_unlock c          -> "MOS_AMO_unlock"
    | MOS_pending_exception exception_type ->
        "MOS_pending_exception " ^ (pp_exception m ioid exception_type)

  else
    match mos with
    | MOS_plain is              -> ""
    | MOS_pending_mem_read ic   -> "MOS_pending_mem_read"
    | MOS_potential_mem_write c -> "MOS_pending_mem_write"
    | MOS_AMO_lock ic           -> "MOS_AMO_lock"
    | MOS_AMO_unlock c          -> "MOS_AMO_unlock"
    | MOS_pending_exception exception_type ->
        "MOS_pending_exception " ^ (pp_exception m ioid exception_type)

let pp_outcome_S indent m is =
  begin match is with
  | (_, Some (instruction_state_pp, _)) -> "\n" ^ pp_instruction_state indent m instruction_state_pp
  | (o, None) ->
      " " ^
      begin match o with
      (* these are special outcomes that we abuse instead of adding new effect kinds *)
      | Read_reg ((Reg ("TMStartEffect", 63, 64, D_decreasing)), _) ->
          "TMStartEffect"
      | Write_reg ((Reg ("TMAbortEffect", 63, 64, D_decreasing), _), _) ->
          "TMAbortEffect"
      | Barrier (Barrier_TM_COMMIT, _) ->
          "TMCommitEffect"

      (* normal outcomes *)
      | Read_mem _        -> "Read_mem"
      | Write_ea _        -> "Write_ea"
      | Write_memv _      -> "Write_memv"
      | Excl_res _        -> "Excl_res"
      | Barrier _         -> "Barrier"
      | Read_reg _        -> "Read_reg"
      | Write_reg _       -> "Write_reg"
      | Internal _        -> "Internal"
      | Footprint _       -> "Footprint"
      | Done ()           -> "Done ()"

      | Escape (Some msg) -> "Escape: " ^ msg
      | Escape None       -> "Escape"
      | Error msg         -> "Error: " ^ msg
      | Fail (Some msg)   -> "Fail: " ^ msg
      | Fail None         -> "Fail"
      end
      ^ "\n"
  end


let pp_micro_op_state_body indent subreads potential_writes m mos =
(*  let indent = if not(m.pp_screenshot) then indent else indent^"  " in*)
  match mos with
  | MOS_plain is -> pp_outcome_S indent m is

  | MOS_pending_mem_read ic ->
      "\n"
      (*^ indent ^ "footprint: " ^ begin match subreads.ui_sr_addr with Some addr -> (pp_footprint m addr) | _ -> "" end ^ "\n"*)
      ^ indent ^ "unsatisfied slices: " ^ (pp_unsat_reads m subreads) ^ "\n"
      ^ indent ^ "writes read from: " ^ (pp_writes_read_from m subreads) ^ "\n"
      ^ indent ^ "requested slices: " ^ (pp_requested_reads m subreads) ^ "\n"

  | MOS_potential_mem_write c ->
      "\n" ^ indent ^ (pp_writes_uncoloured m potential_writes) ^ "\n"

  | MOS_AMO_lock _ ->
      "\n" ^ indent ^ "waiting for the AMO lock\n"

  | MOS_AMO_unlock _ ->
      "\n" ^ indent ^ "ready to release the AMO lock\n"

  | MOS_pending_exception _ ->
      "\n" ^ indent ^ "waiting to raise an exception\n" (* TODO: print more details *)

let pad_instruction =
    (* sample long instruction *)
    String.length "stdu r11,65392(r21)"

(* this is a verbose pp, and only for text-mode - we probably want to tune it and to let the user toggle how much is displayed *)


let compact_loc_max_width = ref 0   (* nasty imperative hack *)

let pp_ui_instruction_instance (m:Globals.ppmode) tid indent i =

  let ppd_dwarf_source_file_lines =
    match !Globals.use_dwarf, m.pp_dwarf_static with
    | true, Some ds ->
       (match pp_dwarf_source_file_lines m ds true i.ui_program_loc with
        | Some s ->
           (match m.Globals.pp_kind with
            | Ascii | Hash -> indent ^ col_yellow s ^ "\n"
            | Html -> "<span class='dwarf_source_lines'>" ^ indent ^ s ^ "</span><br/>"
            | Latex -> "\\myyellow{" ^ indent ^ s ^ "}\n"
           )
           | None ->
              "")
    | _, _ -> ""
  in

  let ppd_dwarf =
    match m.pp_dwarf_dynamic with
    | Some pp_dwarf_dynamic when
            !Globals.use_dwarf &&
            !Globals.dwarf_show_all_variable_locations ->
        pp_dwarf_dynamic.pp_all_location_data_at_instruction tid i.ui_instance_ioid
    | _ -> ""
  in


  let ppd_the_instruction =
    sprintf "%sioid: %s  loc: %s  %s  %s"
      indent
      (pad 2 (pp_pretty_ioid_padded i.ui_instance_ioid))
      (pp_address m (Some i.ui_instance_ioid) i.ui_program_loc)
      (
       let finished = match i.ui_finished with C2b_changed x -> x | C2b_unchanged x -> x in
       let s = pad pad_instruction (pp_instruction m.pp_symbol_table i.ui_instruction i.ui_program_loc) in
       if finished then colour_finished_instruction m s else colour_unfinished_instruction m s)
      (if not (m.pp_style = Globals.Ppstyle_screenshot) then pp_maybe_opcode m i.ui_program_opcode ^ " " else "\n") in

  let indent' = indent ^ "  " in

  let ppd_static_analysis =
    if not(m.pp_style = Globals.Ppstyle_screenshot) then
      sprintf "instruction kind: %s\n%sregs_in: %s  regs_out: %s  ioids_feeding_address: %s  nias: %s dia: %s\n"
        (pp_instruction_kind m i.ui_instruction_kind)
        indent'
        (pp_changed3_setlist m pp_reg i.ui_regs_in)
        (pp_changed3_setlist m pp_reg i.ui_regs_out)
        (pp_changed3_setlist m (fun m ioid -> pp_pretty_ioid ioid) i.ui_ioids_feeding_address)
        (pp_setlist m (pp_nia i.ui_instance_ioid) i.ui_nias)
        (pp_dia m i.ui_instance_ioid i.ui_dia)
    else     sprintf "%sregs_in: %s  regs_out: %s  nias: %s dia: %s "
        indent'
        (pp_changed3_setlist m pp_reg i.ui_regs_in)
        (pp_changed3_setlist m pp_reg i.ui_regs_out)
        (pp_setlist m (pp_nia i.ui_instance_ioid) i.ui_nias)
        (pp_dia m i.ui_instance_ioid i.ui_dia) in

  let ppd_reg_reads =
    pp_changed3_list m (pp_reg_read i.ui_instance_ioid) (List.rev (optionally_hide_ui_pseudoregister_reads m i.ui_reg_reads)) in
  let ppd_reg_writes =
    pp_changed3_list m (fun m -> fun (reg,v) -> sprintf "%s=%s" (pp_reg m reg) (pp_register_value m i.ui_instance_ioid v)) i.ui_reg_writes in
  let ppd_writes_read_from = pp_writes_read_from m i.ui_subreads in
  let ppd_committed_barriers =
    pp_changed2_list m pp_barrier_uncoloured i.ui_committed_barriers in

  let ppd_propagated_writes =
    pp_changed2_list m pp_write_uncoloured i.ui_subwrites.ui_sw_propagated_writes in
  let ppd_potential_write_addresses =
    pp_changed2_list m pp_write_uncoloured i.ui_subwrites.ui_sw_potential_write_addresses in

  let ppd_successful_atomic_store =
    let pp_maybe_bool m = function
      | Some b -> pp_bool m b
      | None   -> "not set"
    in
    colour_changed2b_f m pp_maybe_bool i.ui_successful_atomic_store
  in

  let ppd_sw_committed = colour_changed2b_f m pp_bool i.ui_subwrites.ui_sw_committed in

  let ppd_finished = colour_changed2b_f m pp_bool i.ui_finished in
  let ppd_micro_op_state =
    if m.pp_sail then
      "  micro_op_state: " ^
        (colour_changed2b_f m
                            (pp_micro_op_state_top indent' i.ui_instance_ioid)
                            i.ui_micro_op_state
         ^ (match i.ui_finished with
            | C2b_unchanged true -> "\n"
            | _ -> nocolour_changed2b_f m
                                        (pp_micro_op_state_body indent' i.ui_subreads i.ui_subwrites.ui_sw_potential_writes)
                                        i.ui_micro_op_state))
    else
      ""
  in

  let ppd_dynamic_fields =
    if not(m.pp_style = Globals.Ppstyle_screenshot) then

      indent' ^ "mem writes_read_from: " ^ ppd_writes_read_from
      ^ "  committed store: " ^ ppd_sw_committed
      ^ "  propagated_writes: " ^ ppd_propagated_writes
      ^ "  potential_write_addresses: " ^ ppd_potential_write_addresses
      ^ "  atomic store success: " ^ ppd_successful_atomic_store
      ^ "  committed_barriers: " ^ ppd_committed_barriers ^ "\n"
      ^ indent' ^ "reg_reads: " ^ ppd_reg_reads ^ "\n"
      ^ indent' ^ "reg_writes: " ^ ppd_reg_writes
      ^ "  finished: " ^ ppd_finished
    else
      ""
  in

  let ppd_transitions =
    String.concat "\n" (List.map (pp_cand m) i.ui_instruction_transitions)
    ^ (if i.ui_instruction_transitions <> [] then "\n" else "")
  in

  match m.pp_style with
  | Globals.Ppstyle_compact ->

      let filtered_ui_reg_reads = List.rev (optionally_hide_ui_pseudoregister_reads m i.ui_reg_reads) in
      let ppd_reg_reads =
        pp_changed3_list_body m (pp_reg_read i.ui_instance_ioid) filtered_ui_reg_reads in
      let ppd_reg_writes =
        pp_changed3_list_body m (fun m -> fun (reg,v) -> sprintf "%s=%s" (pp_reg m reg) (pp_register_value m i.ui_instance_ioid v)) i.ui_reg_writes in
      let ppd_writes_read_from = pp_writes_read_from_body m i.ui_subreads in
      let ppd_committed_barriers =
        pp_changed2_list_body m pp_barrier_uncoloured i.ui_committed_barriers in
      let ppd_propagated_writes =
        pp_changed2_list_body m pp_write_uncoloured i.ui_subwrites.ui_sw_propagated_writes in


      ppd_dwarf_source_file_lines
      ^ indent
      ^ (pp_pretty_ioid_padded i.ui_instance_ioid)
      ^ " " ^  (let ppd_loc = let s = (pp_address m (Some i.ui_instance_ioid) i.ui_program_loc) in (if String.length s > !compact_loc_max_width then compact_loc_max_width := String.length s else ()); pad (!compact_loc_max_width) s in ppd_loc)
      ^ " " ^
       (
        let finished = match i.ui_finished with C2b_changed x -> x | C2b_unchanged x -> x in
        let s = pad pad_instruction (pp_instruction m.pp_symbol_table i.ui_instruction i.ui_program_loc) in
        if finished then colour_finished_instruction m s else colour_unfinished_instruction m s)
      ^ "  " ^
      pad 0  (* was pad 2, to indent reg r/w iff there are no mem r/w  *)
        (
         (if i.ui_subreads.ui_sr_writes_read_from = [] then "" else
         (sprintf "mem reads: %s  "
            ppd_writes_read_from) )

         ^ (if i.ui_subwrites.ui_sw_propagated_writes = [] then "" else
         sprintf "mem writes: %s  "
           ppd_propagated_writes)
        )

      ^ (if i.ui_committed_barriers = [] then "" else
      sprintf "barriers: %s  "
        ppd_committed_barriers)

      ^ (if filtered_ui_reg_reads = [] then "" else
      sprintf "reg reads: %s  "
        ppd_reg_reads)

      ^ (if i.ui_reg_writes = [] then "" else
      (* was uncommented, to indent reg writes if there's nothing much else going on *)
      (*(if i.ui_subreads.ui_sr_writes_read_from = [] && i.ui_subwrites.ui_sw_propagated_writes = [] && i.ui_committed_barriers = [] then "  " else "")
      ^ *)sprintf "reg writes: %s  "
          ppd_reg_writes)


 (*      ^ " " ^ "reads:" ^ ppd_reg_reads ^ ppd_writes_read_from
       ^ " " ^ "writes:" ^ ppd_reg_writes ^ ppd_propagated_writes ^ ppd_committed_barriers*)
      ^ "\n"
      ^ ppd_micro_op_state
      ^ ppd_transitions
      ^ ppd_dwarf

   | Globals.Ppstyle_full
   | Globals.Ppstyle_screenshot ->
       match (m.pp_condense_finished_instructions, i.ui_finished) with
       | (true, C2b_unchanged true) ->
           ppd_dwarf_source_file_lines
           ^ sprintf "%sioid: %s  loc: %s  %s  "
             indent
             (pad 2 (pp_pretty_ioid_padded i.ui_instance_ioid))
             (pp_address m (Some i.ui_instance_ioid) i.ui_program_loc)
             (colour_bold m (pad pad_instruction (pp_instruction m.pp_symbol_table i.ui_instruction i.ui_program_loc)))
           ^ sprintf "%s%s%s\n"

               (pad
                  (if m.pp_kind=Latex || m.pp_style = Globals.Ppstyle_screenshot then 0 else String.length "read from: {W 0x00000fffffffefe0=0x00000000}      ")
                  (
                   (if i.ui_subreads.ui_sr_writes_read_from = [] then "" else
                   sprintf "read from: %s  "
                     ppd_writes_read_from)
                   ^
                     (if i.ui_subwrites.ui_sw_propagated_writes = [] then "" else
                     sprintf "mem writes: %s  "
                       ppd_propagated_writes)
                  ))

               (if i.ui_committed_barriers = [] then "" else
               sprintf "barriers: %s  "
                 ppd_committed_barriers)

               (if i.ui_reg_writes = [] then "" else
               sprintf "reg writes: %s"
                 ppd_reg_writes)

           ^ ppd_micro_op_state
           ^ ppd_transitions
           ^ ppd_dwarf
       | _ ->
           ppd_dwarf_source_file_lines
           ^ ppd_the_instruction
           ^ ppd_static_analysis
           ^ ppd_dynamic_fields
           ^ ppd_micro_op_state
           ^ ppd_transitions
           ^ ppd_dwarf


 (** pp the instruction instances of a thread state, rendering the tree structure with indentation *)

let is_finished_unchanged (i: ('ts,'ss) MachineDefTypes.ui_instruction_instance) : bool =
  match i.ui_finished with
  | C2b_unchanged true -> true
  | C2b_unchanged false -> false
  | C2b_changed b -> false

 let rec pp_list_ui_instruction_tree m tid (indent:string) t : (string * bool) list =
   match t with
   | UI_T [] -> []
   | UI_T [iit] -> f m tid indent iit
   | UI_T iits ->
       let indent' = "  " ^ indent in
       List.flatten
         (List.map
            (function iit ->
              let ss = (f m tid indent' iit) in
              match ss with
              | (s,f)::ss' ->
              (* hack to add "-" at start of indentation of first line *)
                  (try
                    let s = Bytes.of_string s in
                    let first_space = Bytes.index s ' ' in
                    Bytes.set s first_space '-';
                    (Bytes.to_string s,f)::ss'
                  with
                  | Not_found -> ss)
              | [] -> [])
            iits)
 and pp_ui_old_instruction_list m tid indent (is: ('ts,'ss) ui_instruction_instance changed2 list) : (string*bool) list =
   List.map (function ic -> g m tid indent ic) is

 and f m tid indent (ic,t) : (string*bool) list =
   colour_changed3_fp m (fun m i -> (pp_ui_instruction_instance m tid indent i, is_finished_unchanged i)) ic
 (*  ^ "\n"*)
   :: pp_list_ui_instruction_tree m tid indent t

 and g m tid indent ic : (string * bool) =
   colour_changed2_fp m (fun m i -> (pp_ui_instruction_instance m tid indent i, is_finished_unchanged i)) ic
 (*  ^ "\n"*)

 let pp_ui_instruction_tree m tid (indent:string) t : string =
   let sbs = pp_list_ui_instruction_tree m tid indent t in
   match m.pp_style with
   | Globals.Ppstyle_compact ->
       let initial_finished =
         let rec f xbs = match xbs with [] -> 0 | (x,b)::xbs' -> if b then 1+f xbs' else 0 in
         f sbs in
       let initial_finished_to_keep = match m.pp_max_finished with None -> initial_finished | Some mx -> min mx initial_finished in
       let initial_finished_to_drop = initial_finished - initial_finished_to_keep in
       let rec drop n xs = if n=0 then xs else match xs with [] -> [] | x::xs' -> drop (n-1) xs' in
       let sbs' = drop initial_finished_to_drop sbs in
       String.concat "" (List.map fst sbs')

   | _ ->
       String.concat "" (List.map fst sbs)

  

let pp_list_ui_instruction_list m tid is : (string * bool) list =
  List.map
    (fun ic ->
      colour_changed3_fp m (fun m i -> (pp_ui_instruction_instance m tid "  " i, is_finished_unchanged i)) ic
    ) is

let pp_ui_instruction_list m tid (indent:string) t : string =
   let sbs = pp_list_ui_instruction_list m tid t in
   match m.pp_style with
   | Globals.Ppstyle_compact ->
       let initial_finished =
         let rec f xbs = match xbs with [] -> 0 | (x,b)::xbs' -> if b then 1+f xbs' else 0 in
         f sbs in
       let initial_finished_to_keep = match m.pp_max_finished with None -> initial_finished | Some mx -> min mx initial_finished in
       let initial_finished_to_drop = initial_finished - initial_finished_to_keep in
       let rec drop n xs = if n=0 then xs else match xs with [] -> [] | x::xs' -> drop (n-1) xs' in
       let sbs' = drop initial_finished_to_drop sbs in
       String.concat "" (List.map fst sbs')

   | _ ->
       String.concat "" (List.map fst sbs)



 let pp_plain_instruction_instance m tid (indent:string) (i:MachineDefTypes.instruction_instance)  =
     sprintf "%sioid:%s %s"
       indent
       (pad 1 (pp_pretty_ioid i.instance_ioid))
 (*      (pp_address m i.ui_program_loc)*)
       (pad 6 (pp_instruction m.pp_symbol_table i.instruction i.program_loc))
 (*      (pp_maybe_opcode m i.ui_program_opcode) *)

 let rec pp_plain_instruction_tree m tid (indent:string) (t:MachineDefTypes.instruction_tree) =
   match t with
   | T iiits ->
       "T [" ^ String.concat ", " (List.map (function (i,it) -> sprintf "(%s, %s)" (pp_plain_instruction_instance m tid indent i) (pp_plain_instruction_tree m tid indent it)) iiits) ^ "]"


 (** pp a UI thread state *)



 let pp_initial_fetch_address m indent initial_ioid addr =
   sprintf "%sInitial fetch address: %s\n"
       indent
       (colour_changed2b_f m
          (fun m -> fun maybe_addr -> match maybe_addr with None -> "None" | Some addr -> pp_address m (Some initial_ioid) addr)
          addr)

(* let pp_promise last_ioid m p = 
 *   match p.prxinfo with
 *   | None -> pp_pwrite_uncoloured m p.pwrite
 *   | Some (t,addr) -> sprintf "%s paired: (%s,%s)"
 *                        (pp_pwrite_uncoloured m p.pwrite)
 *                        (pp_ui_view m t)
 *                        (pp_address m (Some last_ioid) addr) *)

let pp_promise _m = pp_eiid

let pp_ui_machine_thread_state m (tid,ts) =
  let initial_ioid = (tid,0) in (* hack for dwarf pp... *)
  let base_indent = "" in
  let ppd_tid = sprintf "%d" ts.ui_thread in

  let ppd_initial_fetch_address = pp_initial_fetch_address m base_indent initial_ioid ts.ui_initial_fetch_address in
  let ppd_initial_fetch_transitions =
    String.concat "\n" (List.map (pp_cand m) ts.ui_initial_fetch_transitions) ^ "\n" in
  let ppd_empty_thread_instructions = ppd_initial_fetch_address ^ ppd_initial_fetch_transitions in
  let ppd_nonempty_thread_instructions =
    (* base_indent ^ "old instructions\n"
     ^ pp_ui_old_instruction_list m tid base_indent (List.rev_append ts.ui_old_instructions [])
     ^ base_indent ^ "new instructions\n"
     ^ (if not(m.pp_style = Globals.Ppstyle_screenshot) then base_indent ^ "-------------------------\n" else "")
     ^*) pp_ui_instruction_tree m tid base_indent ts.ui_instruction_tree in

  let ppd_instructions =
    (*match (ts.ui_old_instructions,ts.ui_instruction_tree) with
     | ([],UI_T []) -> ppd_empty_thread_instructions
     | _ -> ppd_nonempty_thread_instructions in*)
    match ts.ui_instruction_tree with
    | UI_T [] -> ppd_empty_thread_instructions
    | _ -> ppd_nonempty_thread_instructions in

 (*   let ppd_instructions =  *)
 (*     String.concat "" (List.map (pp_ui_instruction_instance m ts.ui_thread) ts.ui_instructions) in *)

   let ppd_unacknowledged_syncs =
     match ts.ui_unacknowledged_syncs with
     | Some unacknowledged_syncs ->
           Some (pp_changed3_setlist m pp_barrier_uncoloured unacknowledged_syncs)
     | None -> None
   in

   let ppd_read_issuing_order =
     match ts.ui_read_issuing_order with
     | Some read_issuing_order ->
           Some (pp_changed3_list m pp_ordered_reads read_issuing_order)
     | None -> None
   in

 (*   (\* ignore for now:  ui_initial_register_state : (reg_base_name * value) list ;            *\) *)

   let t_state =
     colour_bold m (sprintf "Thread %s state:" ppd_tid) in

   let t_instructions =
     match m.Globals.pp_kind with
     | Html -> sprintf "<div class='state_instructions'>%s</div>" ppd_instructions
     | _ -> sprintf "%s" (*"  committed [| |] and in-flight <| |> instructions:\n%s"*) ppd_instructions in

   let t_unacknowledged_syncs =
     match ppd_unacknowledged_syncs with
     | Some str -> base_indent ^ "unacknowledged Syncs: " ^ str
     | None -> ""
   in

   let t_transaction =
      (* make transactions stealthy, show only when the transaction is active *)
      match ts.ui_transaction with
      | C2b_changed   (Some _)
      | C2b_unchanged (Some _) ->
          let pp_ts m = function
            | Some ts -> pp_transaction_start_uncoloured m ts
            | None -> "--"
          in
          base_indent ^ "transaction: " ^ (colour_changed2b_f m pp_ts ts.ui_transaction) ^ !linebreak
      | _ -> ""
   in

   let t_read_issuing_order =
     match ppd_read_issuing_order with
     | Some str -> base_indent ^ "read issue order: " ^ str ^ !linebreak
     | None -> ""
   in

   t_state ^ (if m.pp_style = Globals.Ppstyle_compact then "    " else !linebreak)
   ^ (if m.pp_style = Globals.Ppstyle_screenshot then "" else t_unacknowledged_syncs ^ !linebreak)
   ^ t_transaction
   ^ t_read_issuing_order
   ^ t_instructions

 (* (\*  sprintf  *)
 (*     "Thread %s state:\n  committed [| |] and in-flight <| |> instructions:\n%s  unacknowledged_syncs = %s\n"  *)
 (*     ppd_tid *)
 (*     ppd_instructions *)
 (*     ppd_unacknowledged_syncs*\) *)


let pp_ui_promising_thread_state m (tid,ts) =
  let base_indent = "" in
  let ppd_tid = sprintf "%d" ts.ui_promising_thread in

  let last_ioid = match ts.ui_promising_last_ioid with
    | None -> (tid,-1) (* fake up ioid *)
    | Some ioid -> ioid
  in

  let ppd_instructions =
    if m.pp_kind = Hash
    then "last ioid "  ^ pp_pretty_ioid last_ioid
    else pp_ui_instruction_list m tid base_indent ts.ui_promising_instrs  
  in

  let ppd_reg = 
    pp_changed3_list_body m 
      (fun m (rbn,rv) -> rbn ^ ": " ^ pp_register_value m last_ioid rv)
      ts.ui_promising_reg
  in

  let ppd_vReg = 
    pp_changed3_list_body m 
      (fun m (rbn,t) -> rbn ^ ": " ^ pp_ui_view m t)
      ts.ui_promising_vReg
  in

  let ppd_promises = 
    pp_changed3_list m (pp_promise) ts.ui_promising_promises
  in

  let ppd_xcl_bank = 
    let pp _m = function
      | None -> "no read exclusive"
      | Some ((weiid,addr),view) -> sprintf "((%s,%s),%s)"
                                (pp_eiid weiid)
                                (pp_address m (Some last_ioid) addr)
                                (pp_ui_view m view)
    in
    colour_changed2b_f m pp ts.ui_promising_xcl_bank
  in

  let ppd_vCoh =
    pp_changed3_setlist_body m (fun m (a,v) -> pp_address m (Some last_ioid) a ^ ": " ^ pp_ui_view m v)
                     ts.ui_promising_vCoh in

  let ppd_fwd_bank =
    pp_changed3_setlist_body m (fun m (a,(t,wx,tFwd)) -> 
        sprintf "%s: (@%s,fwd %s %s)" 
          (pp_address m (Some last_ioid) a)
          (pp_eiid t)
          (pp_ui_view m tFwd)
          (if wx then ", from exclusive write" else "")
      )
      ts.ui_promising_fwd_bank in

  let ppd_vRm = pp_ui_c2_t m ts.ui_promising_vRm in
  let ppd_vRp = pp_ui_c2_t m ts.ui_promising_vRp in
  let ppd_vWm = pp_ui_c2_t m ts.ui_promising_vWm in
  let ppd_vWp = pp_ui_c2_t m ts.ui_promising_vWp in
  let ppd_vCAP = pp_ui_c2_t m ts.ui_promising_vCAP in
  let ppd_vRel = pp_ui_c2_t m ts.ui_promising_vRel in

  let t_state =
    colour_bold m (sprintf "Thread %s state:" ppd_tid) in

  

  let t_instructions =
    match m.Globals.pp_kind with
    | Html -> sprintf "<div class='state_instructions'>%s</div>" ppd_instructions
    | _ -> sprintf "%s" (*"  committed [| |] and in-flight <| |> instructions:\n%s"*) ppd_instructions in

  t_state ^ (if m.pp_style = Globals.Ppstyle_compact then "    " else
               !linebreak ^
               sprintf "Promises: %s" ppd_promises ^ !linebreak ^
               sprintf "reg:      %s" ppd_reg ^ !linebreak ^
               sprintf "vReg:     %s" ppd_vReg ^ !linebreak ^
               sprintf "vCoh:     %s" ppd_vCoh ^ !linebreak ^
               sprintf "vRm:      %s" ppd_vRm ^ !linebreak ^
               sprintf "vRp:      %s" ppd_vRp ^ !linebreak ^
               sprintf "vWm:      %s" ppd_vWm ^ !linebreak ^
               sprintf "vWp:      %s" ppd_vWp ^ !linebreak ^
               sprintf "vCAP:     %s" ppd_vCAP ^ !linebreak ^
               sprintf "vRel:     %s" ppd_vRel ^ !linebreak ^
               sprintf "xcl_bank: %s" ppd_xcl_bank ^ !linebreak ^
               sprintf "fwd_bank: %s" ppd_fwd_bank ^ !linebreak ^
               !linebreak)
  ^ t_instructions


let pp_ui_viewhread_state m (tid,ts) =
  match ts with
  | UI_machine_thread_state ts -> pp_ui_machine_thread_state m (tid,ts)
  | UI_promising_thread_state ts -> pp_ui_promising_thread_state m (tid,ts)


(* raw pp of values for testing *)

let pp_bit_raw = function
  | Bitc_zero -> "0"
  | Bitc_one ->  "1"
                   
                   
let pp_bitl_raw = function
  | Bitl_zero -> "0"
  | Bitl_one ->  "1"
  | Bitl_undef ->  "u"
  | Bitl_unknown ->  "?"

 let pp_byte_raw = function Byte bits -> "[" ^ String.concat "." (List.map pp_bit_raw bits) ^"]"

 let pp_register_value_raw = function rv -> "[" ^ String.concat "." (List.map pp_bitl_raw rv.rv_bits) ^"]"

 let pp_address_raw = function (Address (bs, _)) ->  String.concat ", " (List.map pp_byte_raw bs)

 (** pp a UI transition trace *)

(*val rev_mapi : (int -> 'a -> 'b) -> 'a list -> 'b list*)

let rev_mapi f l =
  let rec rmapi_f i accu = function
    | [] -> accu
    | a::l -> rmapi_f (i-1) (f (i-1) a :: accu) l
  in
  rmapi_f (List.length l) [] l

let transition_history_loc_max_width = ref 0
let transition_history_loc_max_width' = ref 0

let pp_transition_history_trans m s j trans =
  let ioid = MachineDefTypes.principal_ioid_of_trans trans in
  let io = MachineDefUI.lookup_ui_instruction_in_system ioid s in
(*  (pp_pretty_ioid_padded ioid)
  ^ " " ^ *)
  (string_of_int j ^ " ")
  ^
  let s1 =
    match io with
    | Some i ->
        let ppd_loc =
          let s = (pp_address m (Some i.ui_instance_ioid) i.ui_program_loc) in
          (if String.length s > !transition_history_loc_max_width then transition_history_loc_max_width := String.length s else ());
          pad (!transition_history_loc_max_width) s in
        ppd_loc
        ^ " "
        ^ let s = pad pad_instruction (pp_instruction m.pp_symbol_table i.ui_instruction i.ui_program_loc) in s
        ^  " "
        ^ let ppd_dwarf_source_file_lines =
          (match !Globals.use_dwarf, m.pp_dwarf_static with
          | true, Some ds ->
              (match pp_dwarf_source_file_lines m ds false i.ui_program_loc with
                | Some s -> s ^ " "
                | None -> ""
              )
          | _,_ -> "") in
        ppd_dwarf_source_file_lines
   | None -> "" in
(if String.length s1 > !transition_history_loc_max_width' then transition_history_loc_max_width' := String.length s1 else ());
(pad (!transition_history_loc_max_width') s1)
^ pp_trans m trans
^ !linebreak

let pp_transition_history m ?(filter = fun _ -> true) s =
  transition_history_loc_max_width := 0;
  String.concat "" (rev_mapi (pp_transition_history_trans m s) (List.filter filter s.ui_transition_history))


 (** pp a UI system state *)

let pp_ui_system_state m s =
  begin match !(Globals.model_params).t.thread_isa_info.ism with
  | AARCH64_ism _ -> ""
  | RISCV_ism     ->
      let ppd_riscv_AMO_lock =
        let pp_maybe_ioid m = function
          | Some ioid -> pp_pretty_ioid ioid
          | None   -> "unlocked"
        in
        colour_changed2b_f m pp_maybe_ioid s.ui_riscv_AMO_lock
      in
      colour_bold m "System state:" ^ !linebreak
      ^ "AMO lock: " ^ ppd_riscv_AMO_lock ^ !linebreak
      ^ !linebreak
  | PPCGEN_ism    -> ""
  | MIPS_ism      -> ""
  | X86_ism       -> ""
  end
  ^ pp_ui_storage_subsystem_state m s.ui_model s.ui_storage_subsystem
(*  ^ (Pset.elements s.ui_model.shared_memory |> List.map (pp_footprint m None) |> String.concat "; ")
  ^ !linebreak*)
  ^ !linebreak
  ^ String.concat !linebreak (List.map (pp_ui_viewhread_state m) s.ui_thread_states)
  ^ !linebreak

let pp_ui_instruction m s tid ioid =
  let indent = "  " in

  let ic =
    let check_ic = function C3_new i | C3_gone i | C3_unchanged i -> i.ui_instance_ioid = ioid in

    match List.assoc tid s.ui_thread_states with
    | UI_machine_thread_state ts ->
        let rec find_inst = begin function
          | UI_T [] -> None
          | UI_T ((ic, ts') :: ts) ->
              if check_ic ic then Some ic
              else
                begin match find_inst ts' with
                | None -> find_inst (UI_T ts)
                | Some ic -> Some ic
                end
          end
        in
        find_inst ts.ui_instruction_tree

    | UI_promising_thread_state ts ->
        begin match List.find check_ic ts.ui_promising_instrs with
        | ic -> Some ic
        | exception Not_found -> None
        end
  in

  match ic with
  | Some ic -> colour_changed3_f m (fun m i -> pp_ui_instruction_instance m tid indent i) ic
  | None -> pp_pretty_ioid ioid ^ " (pp could not find the instruction-instance)"


let pp_sequential_regsnap m fake_ioid regsnap =
  (String.concat "\n")
    ((List.map
        (fun (reg_base_name,rv) ->
          reg_base_name ^ ":\t" ^
            match rv with
            | Some rv -> pp_register_value m fake_ioid rv
            | None -> "missing")
        regsnap))

let pp_sequential_memsnap m fake_ioid memsnap =
  (String.concat "\n")
    ((List.map
        (fun ((a,_),memory_byte) ->
          pp_address m (Some fake_ioid) a ^ ":\t" ^
            pp_memory_value m fake_ioid memory_byte))
    memsnap)

let pp_sequential_write_ea m fake_ioid = function
  | Some (write_kind,integer_address,size) ->
     let address = address_of_integer integer_address in
     pp_write_kind m write_kind ^
       " " ^ pp_address m (Some fake_ioid) address ^ "   " ^ Nat_big_num.to_string size
  | None -> "none"

let pp_sequential_state m fake_ioid isa_info state =
  "## Register state ## \n" ^
  let regsnap = MachineDefSystemSequential.register_snapshot_of_state isa_info state in
  pp_sequential_regsnap m fake_ioid regsnap ^ "\n" ^
  "## Memory state ## \n" ^
  let memsnap = MachineDefSystemSequential.memory_snapshot_of_state isa_info state in
  pp_sequential_memsnap m fake_ioid memsnap ^ "\n" ^
  "Write address, size:  \t" ^ pp_sequential_write_ea m fake_ioid state.State.write_ea ^ "\n\n\n"


let pp_ui_gen_eiid_table m (s: ('ts,'ss) MachineDefTypes.system_state) = { m with pp_pretty_eiid_table = pretty_eiids s }

let pp_trace m trace : string =
  "[" ^ pp_list m (fun ints -> "[" ^ pp_list m (fun i -> sprintf "%d" i) ints ^ "]") trace ^ "]"


module type GraphBackend = sig
  val make_graph :
    Globals.ppmode -> Test.info ->
    ('ts,'ss) MachineDefTypes.system_state -> MachineDefCandidateExecution.cex_candidate ->
    (int * ('ts,'ss) MachineDefTypes.trans) list -> unit
end