#!/usr/bin/env ocaml
#load "str.cma";;
#load "unix.cma";;


(*===============================================================================*)
(*                                                                               *)
(*                rmem executable model                                          *)
(*                =====================                                          *)
(*                                                                               *)
(*  This file is:                                                                *)
(*                                                                               *)
(*  Copyright Christopher Pulte, University of Cambridge 2018                    *)
(*  Copyright Peter Sewell, University of Cambridge      2018                    *)
(*                                                                               *)
(*  All rights reserved.                                                         *)
(*                                                                               *)
(*  The rmem tool is distributed under the 2-clause BSD license in LICENCE.txt.  *)
(*  For author information see README.md.                                        *)
(*                                                                               *)
(*===============================================================================*)


open Printf
open Unix
open Filename



(* Script to collect author information from svn log. The script takes
   no or a single argument and has three modes:

   1.  No argument: collect author information for the ppcmem/ and
   ppcmem2/ directories and output author list on stdout.

   2.  Directory as argument: collect author information for this
   directory and output autho list on stdout.

   3.  File as argument: collect author information for this file and
   call the headache tool to apply a copyright header to the file. 

*)


let ignore_svn_revisions =
  [ "r5125" ; "r4235" ; "r5139" ; "r5140" ; "r5142" ; "r5143"]



let script_path = Sys.argv.(0)
let script_directory = Filename.dirname script_path ^ "/"
let ppcmem_directory = Filename.dirname script_path ^ "/../../ppcmem"
let ppcmem2_directory = Filename.dirname script_path ^ "/../../ppcmem2"


let list_user = function
  | "pl405" -> false
  | "rg436" -> false
  | _ -> true

let transform_user = function
  | "dpm35" -> "dpm36" (* Dominic seems to have two userids *)
  | user -> user

let userid_to_name = function
  | "aa2019"   -> ("Alasdair", "Armstrong", "University of Cambridge")
  | "acjf3"    -> ("Anthony", "Fox", "University of Cambridge (when this work was done)")
  | "as2418"   -> ("Ali", "Sezgin", "University of Cambridge (when this work was done)")
  | "catzilla" -> ("Kayvan","Memarian", "University of Cambridge")
  | "cp526"    -> ("Christopher", "Pulte", "University of Cambridge")
  | "dpm35"    -> ("Dominic", "Mulligan", "University of Cambridge (when this work was done)")
  | "dpm36"    -> ("Dominic", "Mulligan", "University of Cambridge (when this work was done)")
  | "gk338"    -> ("Gabriel", "Kerneis", "University of Cambridge (when this work was done)")
  | "jf451"    -> ("Jon", "French", "University of Cambridge")
  | "jpichon"  -> ("Jean", "Pichon-Pharabod", "University of Cambridge")
  | "keg29"    -> ("Kathy", "Gray", "University of Cambridge (when this work was done)")
  | "kn307"    -> ("Kyndylan", "Nienhuis", "University of Cambridge")
  | "lr408"    -> ("Linden", "Ralph", "University of Cambridge (when this work was done)")
  | "maranget" -> ("Luc", "Maranget", "INRIA Paris")
  | "mjb220"   -> ("Mark", "Batty", "University of Cambridge (when this work was done)")
  | "ok259"    -> ("Ohad", "Kammar", "University of Cambridge (when this work was done)")
  | "pankaj"   -> ("Pankaj", "Pawan", "IIT Kanpur and INRIA (when this work was done)")
  | "pes20"    -> ("Peter", "Sewell", "University of Cambridge")
  | "rb501"    -> ("Richard", "Bornat", "Middlesex University")
  | "rmn30"    -> ("Robert", "Norton-Wright", "University of Cambridge")
  | "selama"   -> ("Sela", "Mador-Haim", "University of Pennsylvania (when this work was done)")
  | "sf502"    -> ("Shaked", "Flur", "University of Cambridge")
  | "srk31"    -> ("Stephen", "Kell", "University of Cambridge (when this work was done)")
  | "ss726"    -> ("Susmit", "Sarkar", "University of St Andrews")
  | "vb358"    -> ("Victor", "Gomes", "University of Cambridge")
  | "zappa"    -> ("Francesco", "Zappa Nardelli", "INRIA Paris")
  | name -> failwith ("Error: user id " ^ name ^ " not found")


(* from https://www.rosettacode.org/wiki/Execute_a_system_command#OCaml *)
let syscall cmd =
  let ic, oc = Unix.open_process cmd in
  let buf = Buffer.create 16 in
  (try
     while true do
       Buffer.add_channel buf ic 1
     done
   with End_of_file -> ());
  let _ = Unix.close_process (ic, oc) in
  (Buffer.contents buf)

(* from https://gist.github.com/kra3/3775813 *)
(* excludes revisions 4235, r5123 and 5125 which both are just commits updating the header information*)
let svn_list_all_authors file =
  let grep_ignore c = "grep -vE '^" ^ c ^ "' | " in
  let grep_ignore_revisions =
    String.concat "" (List.map grep_ignore ignore_svn_revisions) in
  let cmd = 
    "svn log -r 1:HEAD "^file^" --quiet | grep \"^r\" | " ^ grep_ignore_revisions ^
      "awk '{print $3 \"-\" $5}' | awk -F\"-\" '{print $1 \" \" $2}' | sort" 
  in
  syscall cmd


let newline = "\n"
let space = " "

let print_name (s1,s2,s3) = s1^" "^s2^", "^s3

let print_range (y1,y2) = if y1=y2 then sprintf "%s" (string_of_int y1) else sprintf "%s-%s" (string_of_int y1) (string_of_int y2)
let print_ranges ranges = String.concat ", " (List.map print_range ranges)

let pad_string (left : bool) n str =
  let len = String.length str in
  let () = if len > n then failwith "pad_string" else () in
  let pad = String.make (n - len) ' ' in
  if left then pad ^ str else str ^ pad
  

let make_author_list authors = 
  let author_year_strings =
    List.map (fun (a,y) -> (print_name a,print_ranges y)) authors in
  let (max_author_len,max_year_len) = 
    List.fold_left (fun (m,n) (a,y) -> (max (String.length a) m, max (String.length y) n)) 
      (0,0) author_year_strings in
  let author_strings =
    List.map (fun (a,y) ->
        sprintf "Copyright %s %s \n"
          (pad_string false max_author_len a)
          (pad_string true max_year_len y)) 
      author_year_strings in
  String.concat "" author_strings

let make_header authors = 
  let author_list = make_author_list authors in
  let rmem_string1 = "              rmem executable model\n" in
  let rmem_string2 = "              =====================\n\nThis file is:\n" in
  let rights_string = "All rights reserved.\n" in
  let license_string = "The rmem tool is distributed under the 2-clause BSD license in LICENCE.txt.\n" in
  let contributors_string = "For author information see README.md.            \n" in
  Printf.sprintf "\n%s%s\n%s\n%s\n%s%s"
    rmem_string1
    rmem_string2
    author_list
    rights_string
    license_string
    contributors_string



(* from https://ocaml.org/learn/tutorials/file_manipulation.html *)
let write_file file string =
  let oc = open_out file in
  fprintf oc "%s\n" string;
  close_out oc


let headache_cmd file = 
  let cfg = script_directory ^ "lem_headache.cfg" in
  let header = "/tmp/headache.hdr" in
  "headache -c "^cfg^" -h "^header^" " ^ file

let space_regexp = Str.regexp_string " "
let newline_regexp = Str.regexp_string "\n"

let compare_last_name
      ((firstname1,lastname1,_) : string * string * string)
      ((firstname2,lastname2,_) : string * string * string)
    : int =
  String.compare lastname1 lastname2

(* this must match lem_headache.cfg *)
let supported_file_regexp = 
  let patterns = 
    [ ".*\\.v"
    ; ".*\\.lem"
    ; ".*\\.ml[il]?\\(\\.[a-z]+\\)*"
    ; ".*\\.fml[i]?\\(\\.[a-z]+\\)*"
    ; ".*\\.mly\\(\\.[a-z]+\\)*"
    ; ".*\\.[ch]"
    ; ".*\\.js"
    ; ".*\\.css"
    ; ".*\\.html"
    ; ".*\\.ott"
    ; ".*Makefile.*"
    ; ".*\\.mk"
    ; ".*\\.sh"
    ; ".*\\.py"
    ] 
  in
  let parens_patterns = List.map (fun s -> "\\(" ^ s ^ "\\)") patterns in
  let patterns = String.concat "\\|" parens_patterns in
  Str.regexp patterns

let supported_file str =
  Str.string_match supported_file_regexp str 0
  
module Users = Map.Make(String)

let space_separated_tuple str = 
  match Str.split space_regexp str with
  | [left;right] -> (left,right)
  | _ -> failwith "Str.split did not return a list with two elements"

let add_to_userid_map (map : (int * int list) Users.t) str = 
  let (user,year) = space_separated_tuple str in
  let user = transform_user user in
  if list_user user then
    let entry = Users.singleton user (1,[int_of_string year]) in
    Users.union (fun _user (n1,y1) (n2,y2) -> Some (n1+n2, y1@y2)) map entry
  else map

let list_user_years files : ((string * string * string) * (int * int list)) list =
  let out = String.concat "" (List.map svn_list_all_authors files) in
  let user_years_map = 
    List.fold_left add_to_userid_map Users.empty 
      (Str.split newline_regexp out) in
  let user_years = Users.bindings user_years_map in
  List.map (fun (u,y) -> (userid_to_name u,y)) user_years


let rec sorted_nums_to_ranges_aux (x,y) zs =
  match zs with
  | [] -> [(x,y)]
  | z :: zs ->
     if z = y then
       sorted_nums_to_ranges_aux (x,y) zs
     else if z = y+1 then
       sorted_nums_to_ranges_aux (x,z) zs
     else
       (x,y) :: sorted_nums_to_ranges_aux (z,z) zs

let sorted_nums_to_ranges = function
  | [] -> []
  | x :: xs -> sorted_nums_to_ranges_aux (x,x) xs

let rec nums_to_ranges nums = 
  let sorted_nums = List.sort compare nums in
  sorted_nums_to_ranges sorted_nums

let make_author_data_entry (name,(_count,years)) = (name,nums_to_ranges years)

let make_author_data files =
 let user_years = list_user_years files in
 let sorted_authors = 
   List.sort (fun (_,(n1,_)) (_,(n2,_)) -> compare n1 n2) user_years in
 (* reverse for decreasing order of commits *)
 List.rev (List.map make_author_data_entry sorted_authors)

let process_file file = 
  if not (supported_file file) then
    print_endline ("           Unsupported file: " ^ file ^ ". (skipping)")
  else
    let author_data = make_author_data [file] in
    let header = make_header author_data in
    let () = write_file "/tmp/headache.hdr" header in
    let _  = syscall (headache_cmd file) in
    ()

let process_directory dir = 
  let author_data = make_author_data [dir] in
  let author_list = make_author_list author_data in
  print_endline author_list

let process_top_level_directory () = 
  let author_data = make_author_data [ppcmem_directory;ppcmem2_directory] in
  let author_list = make_author_list author_data in
  print_endline author_list


let main = 
  if (Array.length Sys.argv < 2) then
    process_top_level_directory ()
  else if Sys.is_directory Sys.argv.(1) then
    process_directory Sys.argv.(1)
  else
    process_file Sys.argv.(1)




