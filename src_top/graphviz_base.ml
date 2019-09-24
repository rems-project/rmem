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

open Printf

open Sail_impl_base
open Fragments
open Events
open RegUtils
open CandidateExecution

open Globals

module Make (ConcModel: Concurrency_model.S)
  = struct

type i = ConcModel.instruction_ast


(** ******************** pp of graphs **************** *)

 (*

 DONE:
 unsatisfied read requests
 committed/finished indication
 register in/out
 thread ids
 omit hex addresses if all addresses are symbolic (sort-of, not controllable)
 register reads-from  (sort-of, not pretty enough to use)
 ...with or without values (sort-of, not controllable)
 set font size (just fixed reasonable values)
 calculated layout (cheated by using dot to calculate)
 output to (controllable) generated/ directory
 pretty read/write/barrier names
 legend from test name
 output of first condition-satisfying completed execution
 omit leading zeros in large hex number printing (combined with the following)
 omit 0x for single-digit values 0-9


 TODO:
 some way to control co order graph in tests without reads.  explicit co conditions in tests??
 pp instructions in assembly, not in sail-internal
 registers-to-address indication
 suppress non-memory instructions
 ...and calculate addr, data, ctrl, ctrlisync/isb
 pretty instruction names
 optional check all condition-satisfying completed executions are isomorphic
 transtive reduction of --co-->
 *)


 let preamble legend_opt render_edges = "\
 /* -*- C -*- */\n\
 digraph G {\n"
 (* Note: graphviz does some internal name translation so 'Helvetica'
    (which is a Postscript standard font name) should come out sensible
    even if Helvetica itself isn't installed on your system. *)
 ^ "graph [ fontname = \"Helvetica\" ];\n"
 ^ "node [ fontname = \"Helvetica\" ];\n"
 ^ "edge [ fontname = \"Helvetica\" ];\n"
 ^ (if render_edges then
 "splines=true;\n"
 ^"overlap=true;\n"
 ^"concentrate=true;\n"
 else
 "splines=false;\n"
 ) ^
 "margin=\"0.0\";\n" ^
 (* pad the graph a bit to avoid clipping it in the web interface
    when lines fall exactly on the edge of the drawn region.
    bizarrely this is specified in inches *)
 "pad=\"0.1\";\n\
 tooltip=\"\";\n\
 pencolor=\"transparent\"; /* the cluster bounding rect */\n"

 let postamble legend_opt =
   "/* legend */\n" ^
   "fontsize=10 fontname=\"helvetica\";\n" ^
   begin match legend_opt with
   | Some s -> "label=\"Test " ^ s ^ "\";\n"
   | None -> ""
   end ^
   "}\n"

 type baseid = string

 type nodeid = string
 (*type clusterid = string*)

 (* NOTE: if you double qoute ID that does not really need the double qoute
 (e.g. a string of alphabetic characters, underscores or digits, not beginning
 with a digit) dot -Tplain will remove the double qoute in the output and
 break the name matching *)
 let dquote_str (str: string) : string = "\"" ^ str ^ "\""

 let strip_quotes s =
   let open String in
   let s = (if s.[0] = '"'
            then sub s 1 (length s - 1)
            else s)
   in
   let s = (if s.[length s - 1] = '"'
            then sub s 0 (length s - 1)
            else s)
   in
   s


 let str_to_instruction_nodeid  (str: string) : string = dquote_str ("i"  ^ str)
 let str_to_instruction_portid  (str: string) : string = dquote_str ("ip" ^ str)
 let str_to_write_portid        (str: string) : string = dquote_str ("w"  ^ str)
 let str_to_read_request_portid (str: string) : string = dquote_str ("r"  ^ str)
 let str_to_out_reg_portid      (str: string) : string = dquote_str ("ro" ^ str)

 let baseid_of_instruction (i:i cex_instruction_instance) : baseid = Pp.pp_pretty_ioid i.cex_instance_ioid

 let nodeid_of_instruction (i:i cex_instruction_instance) : string = str_to_instruction_nodeid (baseid_of_instruction i)

 let nodeid_of_ioid ioid : string = str_to_instruction_nodeid (Pp.pp_pretty_ioid ioid)

 let portid_of_instruction (i:i cex_instruction_instance) : string = str_to_instruction_portid (baseid_of_instruction i)

 let label_of_instruction m (i:i cex_instruction_instance) =
   (* for barriers, we pun the instruction and event in the graph output *)
   (match i.cex_instruction_kind with
   | IK_barrier _ -> String.concat "" (List.map (fun b -> Pp.pp_pretty_eiid m b.beiid) i.cex_committed_barriers)
   | _ -> "")
   (*^ "i"*) ^ baseid_of_instruction i ^ " " ^ ConcModel.pp_instruction_ast m m.pp_symbol_table i.cex_instruction i.cex_program_loc


 let portid_of_write (w:write) = str_to_write_portid (Pp.pp_eiid w.weiid)

 let instruction_nodeid_of_write m (w:write) = nodeid_of_ioid (if List.mem w.w_ioid m.pp_initial_write_ioids then List.hd m.pp_initial_write_ioids else w.w_ioid)

 let nodeid_of_write m (w:write) = instruction_nodeid_of_write m w ^ ":" ^ portid_of_write w

 let nodeid_of_thread_id m tid = dquote_str (sprintf "thread%i" tid)
 let portid_of_thread_id m tid = dquote_str (sprintf "threadp%i" tid)

 let portid_of_read_request (r:read_request) = str_to_read_request_portid (Pp.pp_eiid r.reiid)
 let nodeid_of_read_request (r:read_request) = nodeid_of_ioid r.r_ioid ^ ":" ^ portid_of_read_request r

 let portid_of_out_reg (r : Sail_impl_base.reg_name) =
   str_to_out_reg_portid (match r with
                          | Reg (reg_name, _, _, _)
                            | Reg_slice (reg_name, _, _, _) -> reg_name
                          | Reg_field (reg_name, _, _, field_name, _)
                            | Reg_f_slice (reg_name, _, _, field_name, _, _) -> reg_name ^ "_" ^ field_name
                         )

 let pp_satisfied_read m (r,mrs) =
   (Pp.pp_read_uncoloured m r)
   ^ " = "
   ^ Pp.pp_memory_value m r.r_ioid mrs.mrs_value
 (*  ^ (pp_mrs_uncoloured m mrs)
 *)

 let pp_unsatisfied_read m r =
   (Pp.pp_read_uncoloured m r)
   ^ " = "
   ^ "?"


 let rf_edges_of_satisfied_read pw (r,mrs) =
   Model_aux.option_map
     (function (w,wss) ->
       if pw w then
         Some (w,r,wss )
       else
         None)
     mrs.mrs_writes_read_from

 let rec my_list_assoc x xys =
   match xys with
   | [] -> None
   | (x',y)::xys' -> if x=x' then Some y else my_list_assoc x xys'

 let arrowsz = "[arrowsize=\"0.5\"]"

 let pp_rf_edge m positions (w,r,slices) =
   let n1 = nodeid_of_write m w in
   let n2 = nodeid_of_read_request r in
   let pp_slices = List.map (Pp.pp_write_slice m w) slices in
   let label = "rf" ^ String.concat "," pp_slices in
   n1 ^ " -> " ^ n2 ^ " [weight=0, constraint=false color=\"red\", fontcolor=\"red\", label=\"" ^ label ^ "\",fontsize=10 fontname=\"helvetica\" ]" ^ arrowsz ^ ";\n"

 let rfreg_edges_of_reg_read m thread_node (* thread nodeid *) ioid lookup (*from reg to node/port *) (r,rrs,v) =
   let edge_of reg source (label_func : 'a -> string) =
     match source with
     | RRS_instruction (ioid', _, vfs) -> [(nodeid_of_ioid ioid' ^ ":" ^ portid_of_out_reg r,
                                          List.assoc r lookup ^":n" (*nodeid_of_ioid ioid*),
                                          label_func vfs)]
     | RRS_initial_state vfs -> [(thread_node ^ ":s",
                                List.assoc r lookup ^":n" (*nodeid_of_ioid ioid*),
                                label_func vfs)]
     | RRS_pseudoregister -> [] (*[("pseudoregister", nodeid_of_ioid ioid,r_base_name ^ "" ^"(pseudo)")]*)
   in
   match (r, rrs) with
   (* can this happen? should we assert that it doesn't? *)
   | (_, []) -> []
   (* Special case whole registers/fields from a single source
      and omit the slice info for cleaner graphs *)
   | (Reg (reg_name, _, _, _), [source]) ->
      edge_of r source (fun _ -> reg_name)
   | (Reg_field (reg_name, _, _, field_name, _), [source]) ->
      edge_of r source (fun _ -> reg_name ^ "." ^ field_name)
   (* General case *)
   | _ -> begin
       let r_base_name = Sail_impl_base.register_base_name r in
       let slices_of_vfs vfs =
         r_base_name ^ "[" ^String.concat "," (List.map (function (sl,mv) -> Pp.pp_slice m sl) vfs) ^ "]"
       in
       List.flatten (List.map (fun rrse -> edge_of r rrse slices_of_vfs) rrs)
     end

 let pp_rfreg_edge m positions (n1,n2,label) =
   n1 ^ " -> " ^ n2 ^ " [weight=0, constraint=false color=\"orange\", fontcolor=\"orange\", label=\"" ^ label ^ "\",fontsize=10 fontname=\"helvetica\" ]" ^ arrowsz ^ ";\n"



 let pp_html_co_edge m positions (w1,w2) =
   let n1 = nodeid_of_write m w1 in
   let n2 = nodeid_of_write m w2 in
   let label = "co" in
   n1 ^ " -> " ^ n2 ^ " [weight=0, constraint=false color=\"brown\", fontcolor=\"brown\", label=\"" ^ label ^ "\",fontsize=10 fontname=\"helvetica\" ]" ^ arrowsz ^ ";\n"

 let pp_html_fr_edge m positions (r,(w,slices)) =
   let n1 = nodeid_of_read_request r in
   let n2 = nodeid_of_write m w in
   let pp_slices = List.map (Pp.pp_write_slice m w) slices in
   let label = "fr" ^ String.concat "," pp_slices in
   n1 ^ " -> " ^ n2 ^ " [weight=0, constraint=false color=\"orange\", fontcolor=\"orange\", label=\"" ^ label ^ "\",fontsize=10 fontname=\"helvetica\" ]" ^ arrowsz ^ ";\n"



let pp_positioning_edge n1 n2 =
  n1 ^ " -> " ^ n2
  ^ " [style=invis]"
  ^ ";\n"


let x=ref 0.0
let y=ref 0.0

type table_cell = {
    colspan: int option;
    padding: int option;
    background: string option;
    colour: string option;
    align: string option;
    portid: string option;
    label: string;
  }

let make_table_cell ?colspan ?padding ?background ?colour ?align ?portid label : table_cell =
  { colspan; padding; background; colour; align; portid; label }

let table_body ?colour ?border (rows : table_cell list list) =
  let rows_str =
    String.concat
      ""
      (List.map
         (function row ->
                   "<TR>"
                   ^  String.concat
                        ""
                        (List.map (function ({ colspan; padding; background; colour; align; portid; label } : table_cell) ->
                                            let attrs =
                                              String.concat " "
                                                            (List.filter (fun s -> s <> "") [
                                                                           begin match colspan with
                                                                           | Some n -> sprintf "COLSPAN=\"%d\"" n
                                                                           | None -> ""
                                                                           end;
                                                                           begin match background with
                                                                           | Some s -> sprintf "BGCOLOR=\"%s\"" s
                                                                           | None -> ""
                                                                           end;
                                                                           begin match portid with
                                                                           | Some s -> sprintf "PORT=%s" s
                                                                           | None -> ""
                                                                           end;
                                                                           begin match padding with
                                                                           | Some n -> sprintf "CELLPADDING=\"%d\"" n
                                                                           | None -> ""
                                                                           end;
                                                                           begin match align with
                                                                           | Some s -> sprintf "ALIGN=\"%s\"" s
                                                                           | None -> ""
                                                                           end;
                                                            ])
                                            in
                                            let (font, unfont) =
                                              match colour with
                                              | Some s -> (sprintf "<FONT COLOR=\"%s\">" s, "</FONT>")
                                              | None -> ("", "")
                                            in
                                            sprintf "<TD%s%s>%s%s%s</TD>"
                                                           (if String.length attrs > 0 then " " else "") attrs font label unfont)
                                  row)
                   ^ "</TR>\n"
         )
         rows)
  in
  sprintf "\n<TABLE CELLSPACING=\"0\" BORDER=\"0\"%s%s>\n%s</TABLE>\n"
                 (match colour with
                  | Some s -> sprintf " COLOR=\"%s\"" s
                  | None -> "")
                 (match border with
                  | Some n -> sprintf " CELLBORDER=\"%d\"" n
                  | None -> "")
                 rows_str

let table_node ?colour ?border positions nodeid rows =
  let pos_str =
    match my_list_assoc (strip_quotes nodeid) positions with
    | Some (x,y) -> sprintf "pos=\"%f,%f!\" " x y
    | None -> ""
  in
  sprintf "%s [%sshape=none margin=0 height=0.02 fontsize=10 label=< %s >];\n"
                 nodeid
                 pos_str
                 (table_body ?colour ?border rows)

let trans_rows m ?colspan (trans : ConcModel.ui_trans list) =
  if m.ppg_trans && trans <> [] then
    let highlight n =
      match m.pp_default_cmd with
      (* it would be nice to make the highlighted transitions bold
         but graphviz has a bug which makes them overflow the table *)
      | Some (Interact_parser_base.Transition (Interact_parser_base.WithEager i)) when i = n ->
         ("<B>", "</B>", "blue", Some "yellow", None)
      | Some (Interact_parser_base.Transition (Interact_parser_base.WithBoundedEager (i, _))) when i = n ->
         ("<B>", "</B>", "blue", Some "yellow", None)
      | _ ->
         (   "",     "", "blue", None,          None)
    in
    let body =
      table_body ~border:0
        (List.mapi
           (fun i (n, t) ->
                let (bold, unbold, colour, background, padding) = highlight n in
                let label =
                  sprintf "%s%i:%s%s"
                    bold n (ConcModel.pp_trans ~graph:true {m with pp_kind=Html} t) unbold
                in
                [make_table_cell ?colspan
                   ?padding
                   ?background
                   ~colour
                   ~align:"left"
                   ~portid:(sprintf "\"tr%i\"" i)
                   label])
           
           trans)
    in
    [[make_table_cell ?colspan ~align:"left" body]]
  else
    []

let ppd_trans_ids m trans =
  let trans_ids = List.map fst trans in
  if not(m.ppg_trans) then
    if trans_ids<>[] then "["^(String.concat "," (List.map string_of_int trans_ids)) ^"]" else ""
  else
    ""

let pp_html_node_of_thread_id m positions t
      (ioid_trans_lookup : Events.ioid -> (ConcModel.ui_trans) list) =
  let tid = t.cex_thread in
  (* HACK: we assume the dummy ioid used by the initil fetch will be 0 *)
  let init_ioid = (tid, 0) in
  let trans = ioid_trans_lookup init_ioid in
  let rows =
    [[make_table_cell ~colour:"black" ~portid:(portid_of_thread_id m tid) (ppd_trans_ids m trans ^ "Thread "^sprintf "%i" tid)]]
    @ trans_rows m trans
  in
  "/* thread header */\n"
  ^ table_node ~colour:"black" ~border:1 positions (nodeid_of_thread_id m tid) rows


let pp_html_dep_edge m label i1 ioid2 =
  let n1 = nodeid_of_instruction i1 in
  let n2 = nodeid_of_ioid ioid2 in
  n2 ^ " -> " ^ n1 ^ " [weight=0, constraint=false color=\"indigo\", fontcolor=\"indigo\", label=\"" ^ label ^ "\",fontsize=10 fontname=\"helvetica\" ]" ^ arrowsz ^ ";\n"


let pp_html_node_of_instruction  m pi pw render_edges positions thread_node (i:i cex_instruction_instance) prefix ioid_trans_lookup =
  let n_regs_in = Pset.cardinal i.cex_regs_in in
  let n_regs_out = Pset.cardinal i.cex_regs_out in
  let colspan = (max 1 n_regs_in) * (max 1 n_regs_out) in
  (* lookup from regs_in regname to nodeid*)
  let lookup =
    List.mapi
      (fun n r ->
        (r, nodeid_of_instruction i ^ ":ri" ^ sprintf "%i" n))
      (Pset.elements i.cex_regs_in)
  in
  (* not sure we can get away with identity of register names here *)
  let reg_row n_regs n_other_regs reg_set fulfilled_set portid_fun =
    if m.ppg_regs && n_regs > 0 then
      [List.mapi
         (fun n r ->
           make_table_cell ~colspan:(max 1 n_other_regs)
                           ~colour:(if List.mem r fulfilled_set
                                    then "black"
                                    else "gray")
                           ~portid:(portid_fun n r)
                           (Pp.pp_reg m r))
         (Pset.elements reg_set)]
    else
      []
  in
  let regs_in_row = reg_row n_regs_in
                            n_regs_out
                            i.cex_regs_in
                            (List.map (fun (r', _, _) -> r') i.cex_reg_reads)
                            (fun n r -> sprintf "\"ri%d\"" n)
  in
  let regs_out_row = reg_row n_regs_out
                             n_regs_in
                             i.cex_regs_out
                             (List.map (fun (r', _) -> r') i.cex_reg_writes)
                             (fun n r -> portid_of_out_reg r)
  in
  let trans = ioid_trans_lookup i.cex_instance_ioid in
  let ppd_symbolic_address =
    match Pp.lookup_symbol m.pp_symbol_table i.cex_program_loc with
    | None -> ""
    | Some s -> s^":" in
  let rows =
    regs_in_row
    @ [[make_table_cell ~colspan
                        ~colour:(if trans <> [] then "blue" else "black")
                        ~portid:(portid_of_instruction i)
                        (ppd_trans_ids m trans ^ ppd_symbolic_address ^ label_of_instruction m i)]]
    @ trans_rows m ~colspan trans
    @ (List.map (function w ->       [make_table_cell ~colspan ~colour:"gray"  ~portid:(portid_of_write w)        (Pp.pp_write_uncoloured m w)])     i.cex_potential_write_addresses)
    @ (List.map (function w ->       [make_table_cell ~colspan ~colour:"gray"  ~portid:(portid_of_write w)        (Pp.pp_write_uncoloured m w)])     i.cex_potential_writes)
    @ (List.map (function w ->       [make_table_cell ~colspan ~colour:"black" ~portid:(portid_of_write w)        (Pp.pp_write_uncoloured m w)])     i.cex_propagated_writes)
    @ (List.map (function r ->       [make_table_cell ~colspan ~colour:"black" ~portid:(portid_of_read_request r) (pp_unsatisfied_read m r)])     i.cex_requested_unsatisfied_reads)
    @ (List.map (function (r,mrs) -> [make_table_cell ~colspan ~colour:"black" ~portid:(portid_of_read_request r) (pp_satisfied_read m (r,mrs))]) i.cex_satisfied_reads)
    @ regs_out_row in

  "/* instruction " ^ nodeid_of_instruction i ^" */\n"
  ^ table_node ~colour:(if i.cex_finished then "black" else if trans <> [] then "blue" else "gray")
               ~border:1
               positions
               (nodeid_of_instruction i)
               rows
  ^ (if render_edges then
       (match i.cex_satisfied_reads with
        | [] -> ""
        | _ ->
           "/* reads-from edges to satisfied reads */\n"
           ^ (if m.ppg_rf then String.concat "" (List.map (pp_rf_edge m positions) (List.flatten (List.map (rf_edges_of_satisfied_read pw) i.cex_satisfied_reads))) else ""))
       ^
         (if m.ppg_reg_rf then
            match i.cex_reg_reads with
            | [] -> ""
            | _ -> begin
                if Structured_output.is_verbosity_at_least Structured_output.Debug then
                  printf "* Graph constructing reg-rfs for instruction with ioid %s, cex_regs_in = [%s], cex_regs_out = [%s], cex_reg_reads = [%s], cex_reg_writes = [%s]\n"
                         (Pp.pp_pretty_ioid i.cex_instance_ioid)
                         (String.concat "," (List.map (Pp.pp_reg m) (Pset.elements i.cex_regs_in)))
                         (String.concat "," (List.map (Pp.pp_reg m) (Pset.elements i.cex_regs_out)))
                         (String.concat "," (List.map (fun (r, _, _) -> Pp.pp_reg m r) i.cex_reg_reads)) (String.concat "," (List.map (fun (r, _) -> Pp.pp_reg m r) i.cex_reg_writes));
                "/* register reads */\n"
                ^ "/* " ^ Pp.pp_reg_reads i.cex_instance_ioid m i.cex_reg_reads ^ " */\n"
                ^ String.concat "" (List.map (pp_rfreg_edge m positions) (List.flatten (List.map (rfreg_edges_of_reg_read m thread_node i.cex_instance_ioid lookup) i.cex_reg_reads)))
              end
          else
            "")
       ^ (if m.ppg_addr then List.map (pp_html_dep_edge m "addr" i) (Pset.elements i.cex_address_dependencies) |> String.concat "\n" else "")
       ^ (if m.ppg_data then List.map (pp_html_dep_edge m "data" i) (Pset.elements i.cex_data_dependencies) |> String.concat "\n" else "")
       ^ (if m.ppg_ctrl then
            let prefix_ctrls = List.map (fun i -> Pset.elements i.cex_control_dependencies) prefix in
            Pset.elements i.cex_control_dependencies
            |> List.filter (fun ioid -> not (List.exists (fun deps -> List.mem ioid deps) prefix_ctrls))
            |> List.map (pp_html_dep_edge m "ctrl" i)
            |> String.concat "\n"
          else "")
     else
       "")


let pp_html_initial_writes  m pw positions (cex: i cex_candidate) =
  let cex_initial_writes = List.filter pw cex.cex_initial_writes in
  match cex_initial_writes with
  | [] -> ""
  | _ ->
     let rows = List.map (function w -> [make_table_cell
                                           ~colour:"black"
                                           ~portid:(portid_of_write w)
                                           (Pp.pp_write_uncoloured m w)])
                         (List.rev cex_initial_writes) in
      "/* initial writes */\n"
      ^ "subgraph cluster_initial_writes {\n"
      ^ table_node ~colour:"black"
                   ~border:1
                   positions
                   (nodeid_of_ioid (List.hd cex.cex_initial_writes).w_ioid)
                   rows
      ^ "}\n"

let rec tree_edges (i_parent: i cex_instruction_instance option) it : (i cex_instruction_instance * i cex_instruction_instance * bool (* true if single*) ) list =
  match it with
  | CEX_T iits ->
      let single = (List.length iits = 1) in
      List.flatten
        (List.map
           (function (i,it) ->
             let subtree_edges = tree_edges (Some i) it in
             match i_parent with
             | None -> subtree_edges
             | Some ip -> (ip,i,single)::subtree_edges
           )
           iits)

let rec tree_instructions (pi : i cex_instruction_instance -> bool) it : (i cex_instruction_instance list) =
  match it with
  | CEX_T iits ->
      List.flatten
        (List.map
           (function (i,it') ->
             if pi i then
               i :: tree_instructions pi it'
             else
               tree_instructions pi it'
           )
           iits)

let map_with_tail f l =
  let rec _map f l acc =
    match l with
    | [] -> acc
    | x :: xs -> _map f xs ((f x xs) :: acc)
  in _map f l []
     |> List.rev

let prefix_list l =
  List.rev l
  |> map_with_tail (fun x xs -> xs)
  |> List.map List.rev
  |> List.rev

let pp_html_nodes_of_thread m pi pw render_edges positions
      (cex_t:i cex_thread_state) cex_it
      (ioid_trans_lookup : Events.ioid -> (ConcModel.ui_trans) list) =
  let thread_node = nodeid_of_thread_id m cex_t.cex_thread in
  pp_html_node_of_thread_id m positions cex_t ioid_trans_lookup
  ^ "/* thread nodes */\n"
  ^
    let is = tree_instructions pi cex_it (*cex_t.cex_instruction_tree*) in
    let prefixes = prefix_list is in
    String.concat "" (List.map2 (fun i prefix -> pp_html_node_of_instruction m pi pw render_edges positions thread_node i prefix ioid_trans_lookup) is prefixes)

let pp_html_tree_edge m (i1,i2,single) =
  let n1 = nodeid_of_instruction i1 in
  let n2 = nodeid_of_instruction i2 in
  n1 ^ " -> " ^ n2
(*  ^ " [label=\"po\" fontsize=10 fontname=\"helvetica\" ]"*)
  ^ " " ^ arrowsz ^ "" ^";\n"

let pp_html_tree_edges_of_thread m pi (cex_t:i cex_thread_state) cex_it =
  let top_edges =
    (match cex_it with
    | CEX_T iiits ->
        List.map (function (i,_) ->  (nodeid_of_thread_id m cex_t.cex_thread, nodeid_of_instruction i)) iiits) in
  String.concat "" (List.map (function (e1,e2) -> pp_positioning_edge e1 e2) top_edges)
  ^
    let edges = tree_edges None cex_it in
    String.concat "" (List.map (pp_html_tree_edge m) edges)

let pp_html_thread m pi pw render_edges positions (cex_t:i cex_thread_state) ioid_trans_lookup =
  let cex_it = CandidateExecution.filter_instruction_tree pi cex_t.cex_instruction_tree in
  "/* thread " ^( sprintf "%i" cex_t.cex_thread) ^ " */\n"
  ^ "subgraph cluster" ^ sprintf "%i" cex_t.cex_thread ^ " {\n"
  ^ pp_html_nodes_of_thread m pi pw render_edges positions cex_t cex_it ioid_trans_lookup
  ^
(*    (if render_edges then*)
      "/* thread po edges */\n"
      ^ pp_html_tree_edges_of_thread m pi cex_t cex_it
(*    else
      "")
 *)
  ^ "}\n"

let pp_html_candidate_execution m legend_opt render_edges positions (cex: i cex_candidate)
      (ioid_trans_lookup : Events.ioid -> (ConcModel.ui_trans) list) =
  let iwi (*initial_write_ioids*) = List.map (function w -> w.w_ioid) cex.cex_initial_writes in
  let m = { m with pp_initial_write_ioids = iwi } in
  let threads = Pmap.bindings_list cex.cex_threads in
  let (pi,pw,pr) =
    match m.ppg_shared with
    | true ->
        let fp_shared = CandidateExecution.shared_memory_footprints cex in
        let pi = function (i:i cex_instruction_instance) -> CandidateExecution.is_shared_memory_instruction fp_shared i in
        let pw = function (w:write) -> CandidateExecution.is_shared_memory_write fp_shared w in
        let pr = function (r:read_request) -> CandidateExecution.is_shared_memory_read fp_shared r in
        (pi,pw,pr)
    | false ->
        let pi = function (i:i cex_instruction_instance) -> true in
        let pw = function (w:write) -> true in
        let pr = function (r:read_request) -> true in
        (pi,pw,pr)
  in
  preamble legend_opt render_edges
  ^ pp_html_initial_writes m pw positions cex
  ^ String.concat "\n" (List.map (function (tid,t) -> pp_html_thread m pi pw render_edges positions t ioid_trans_lookup) threads)
  ^ "\n"
  ^
    (if render_edges then
      "/* coherence */\n"
      ^ (if m.ppg_co then String.concat "" (List.map (pp_html_co_edge m positions) (List.filter (function (w1,w2) -> pw w1 && pw w2) (Pset.elements cex.cex_co))) else "")
      ^ "/* from-reads */\n"
      ^ (if m.ppg_fr then String.concat "" (List.map (pp_html_fr_edge m positions) (List.filter (function (r1,(w2, _)) -> pr r1 && pw w2) (Pset.elements cex.cex_fr))) else "")
    else
      "")
  ^ (postamble legend_opt)


let parse_dot_positions lines =
  let parse_line ss =
    begin match Pp.split " " ss with
    | keyword::name::x::y::_ when keyword="node" ->
       Some ((strip_quotes name), (float_of_string x, float_of_string y))
    | _ -> None
    end
  in
  Misc.option_map parse_line lines

let ioid_trans_lookup (nc: (ConcModel.ui_trans) list)
    : Events.ioid -> (ConcModel.ui_trans) list
  =
  let ioid_trans_lookup_data  : (Events.ioid option * (ConcModel.ui_trans)) list =
    List.map (fun (n,t) -> (ConcModel.principal_ioid_of_trans t, (n,t))) nc
  in
  fun (ioid: Events.ioid) ->
    Misc.option_map
      (fun (ioid',n) -> if ioid' = Some ioid then Some n else None)
      ioid_trans_lookup_data

end (* Make *)
