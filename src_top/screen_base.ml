(*===============================================================================*)
(*                                                                               *)
(*                rmem executable model                                          *)
(*                =====================                                          *)
(*                                                                               *)
(*  This file is:                                                                *)
(*                                                                               *)
(*  Copyright Jon French, University of Cambridge        2017-2018               *)
(*  Copyright Shaked Flur, University of Cambridge       2016-2018               *)
(*  Copyright Peter Sewell, University of Cambridge           2017               *)
(*  Copyright Christopher Pulte, University of Cambridge 2017-2018               *)
(*                                                                               *)
(*  All rights reserved.                                                         *)
(*                                                                               *)
(*  It is part of the rmem tool, distributed under the 2-clause BSD licence in   *)
(*  LICENCE.txt.                                                                 *)
(*                                                                               *)
(*===============================================================================*)

(* type for structured output that can be realized as string or HTML or
what ever *)
type ot_calss =
  | OTCInfo
  | OTCWarning
  | OTCFinal
  | OTCFollowList


(* TODO: move to pp.ml *)
type output_tree =
  | OTEmpty
  | OTString of string
  | OTEncoded of string
  | OTLine of output_tree
  | OTEmph of output_tree
  | OTColor of string * output_tree
  | OTCenter of output_tree
  | OTVerbose of Globals.verbosity_level * output_tree
  | OTConcat of output_tree list
  | OTClass of ot_calss * output_tree
  | OTHorLine

let otString format          = Printf.ksprintf (fun s -> OTString s) format
let otStrLine format         = Printf.ksprintf (fun s -> OTLine (OTString s)) format
let otStrEmph format         = Printf.ksprintf (fun s -> OTEmph (OTString s)) format
let otStrColor color format  = Printf.ksprintf (fun s -> OTColor (color, OTString s)) format
let otStrVerbose verb format = Printf.ksprintf (fun s -> OTVerbose (verb, OTString s)) format
let otStrClass cls format    = Printf.ksprintf (fun s -> OTClass (cls, OTString s)) format

let otEncoded str = OTEncoded str
let otLine tree = OTLine tree
let otEmph tree = OTEmph tree
let otColor color tree = OTColor (color, tree)
let otCenter tree = OTCenter tree
let otVerbose verb tree = OTVerbose (verb, tree)
let otConcat trees = OTConcat trees
let otClass cls tree = OTClass (cls, tree)
let otIfTrue b tree = if b then tree else OTEmpty

let otConcatWith sep = function
  | [] -> OTEmpty
  | hd :: tl ->
      List.fold_left (fun acc t -> t :: sep :: acc) [hd] tl
      |> List.rev
      |> otConcat

let reset_color = "\x1b[0m"
let string_of_color color =
  let br = 0 in
  let fg =
    match color with
    | "black"     -> 0
    | "red"       -> 1
    | "green"     -> 2
    | "yellow"    -> 3
    | "blue"      -> 4
    | "magenta"   -> 5
    | "cyan"      -> 6
    | "white"     -> 7
    | "dark_gray" -> 60
    | _ -> failwith ("unknown color: " ^ color)
  in
  Printf.sprintf "\x1b[%u;%um" br (fg + 30)

let rec string_of_output_tree ppmode tree : string =
  match tree with
  | OTEmpty -> ""
  | OTString str -> str
  | OTEncoded str -> str
  | OTLine tree -> (string_of_output_tree ppmode tree) ^ "\n"
  | OTEmph tree ->
      if ppmode.Globals.pp_colours then
        "\x1b[1m" ^ (string_of_output_tree ppmode tree) ^ reset_color
      else
        string_of_output_tree ppmode tree
  | OTColor (color, tree) ->
      if ppmode.Globals.pp_colours then
        (string_of_color color) ^ (string_of_output_tree ppmode tree) ^ reset_color
      else
        string_of_output_tree ppmode tree
  | OTCenter tree -> string_of_output_tree ppmode tree
  | OTClass (cls, tree) -> string_of_calss_output_tree ppmode cls tree
  | OTVerbose (verb, tree) ->
      if Globals.is_verbosity_at_least verb then string_of_output_tree ppmode tree
      else ""
  | OTConcat trees -> String.concat "" (List.map (string_of_output_tree ppmode) trees)
  | OTHorLine ->
      OTString "----------------------------------------------------------------------------"
      |> otLine
      |> string_of_output_tree ppmode
and string_of_calss_output_tree ppmode cls tree : string =
  match cls with
  | OTCInfo    -> OTColor ("cyan", tree) |> string_of_output_tree ppmode
  | OTCWarning -> OTColor ("yellow", tree) |> string_of_output_tree ppmode
  | OTCFinal
  | OTCFollowList
      -> string_of_output_tree ppmode tree


let ascii_to_html = [
    ('&',  "&amp;");
    ('<',  "&lt;");
    ('>',  "&gt;");
    ('"',  "&quot;");
    ('\'', "&#x27;");
    ('/',  "&#x2f;");
  ]

let replace_chars tbl s =
  let buf = Buffer.create (String.length s) in
  String.iter (fun c ->
    match List.assoc c tbl with
    | rep ->
        Buffer.add_string buf rep
    | exception Not_found ->
        Buffer.add_char buf c
  ) s;
  Buffer.contents buf

let html_escape s = replace_chars ascii_to_html s

let rec html_of_output_tree ppmode tree : string =
  match tree with
  | OTEmpty -> ""
  | OTString str -> html_escape str
  | OTEncoded str -> str
  (* We use "<p>...</p>" and not "...<br>" because it works better with
  the "scroll on output" UI option *)
  | OTLine tree -> "<p>" ^ (html_of_output_tree ppmode tree) ^ "</p>"
  | OTEmph tree -> "<b>" ^ (html_of_output_tree ppmode tree) ^ "</b>"
  | OTColor (color, tree) ->
      if ppmode.Globals.pp_colours then
        "<span style='color:" ^ color ^ "'>" ^ html_of_output_tree ppmode tree ^ "</span>"
      else
        html_of_output_tree ppmode tree
  | OTCenter tree -> "<center>" ^ html_of_output_tree ppmode tree ^ "</center>"
  | OTClass (cls, tree) ->
      if ppmode.Globals.pp_colours then
        html_of_calss_output_tree ppmode cls tree
      else
        html_of_output_tree ppmode tree
  | OTVerbose (verb, tree) ->
      if Globals.is_verbosity_at_least verb then html_of_output_tree ppmode tree
      else ""
  | OTConcat trees -> String.concat "" (List.map (html_of_output_tree ppmode) trees)
  | OTHorLine -> "<hr style=\"border: 1px solid;\">"
and html_of_calss_output_tree ppmode cls tree : string =
  match cls with
  | OTCInfo ->
      "<span class='info'>" ^ (html_of_output_tree ppmode tree) ^ "</span>"
  | OTCWarning ->
      "<span class='warning'>" ^ (html_of_output_tree ppmode tree) ^ "</span>"
  | OTCFinal ->
      "<span class='final'>" ^ (html_of_output_tree ppmode tree) ^ "</span>"
  | OTCFollowList ->
      "<span class=\"follow_list\">" ^ (html_of_output_tree ppmode tree) ^ "</span>"

let rec latex_of_output_tree ppmode tree : string =
  match tree with
  | OTEmpty -> ""
  | OTString str -> str
  | OTEncoded str -> str
  | OTLine tree -> (latex_of_output_tree ppmode tree) ^ "\n"
  | OTEmph tree ->
      if ppmode.Globals.pp_colours then
        "\\mybold{" ^ (latex_of_output_tree ppmode tree) ^ "}"
      else
        latex_of_output_tree ppmode tree
  | OTColor (color, tree) -> "\\my" ^ color ^ "{" ^ (latex_of_output_tree ppmode tree) ^ "}"
  | OTCenter tree -> "\\begin{center}" ^ (latex_of_output_tree ppmode tree) ^ "\\end{center}"
  | OTClass (cls, tree) -> latex_of_class_output_tree ppmode cls tree
  | OTVerbose (verb, tree) ->
      if Globals.is_verbosity_at_least verb then latex_of_output_tree ppmode tree
      else ""
  | OTConcat trees -> String.concat "" (List.map (latex_of_output_tree ppmode) trees)
  | OTHorLine -> "\n\\line\n"
and latex_of_class_output_tree ppmode cls tree : string =
  match cls with
  | OTCInfo ->
      OTColor ("cyan", tree) |> latex_of_output_tree ppmode
  | OTCWarning ->
      OTColor ("yellow", tree) |> latex_of_output_tree ppmode
  | OTCFinal ->
      OTColor ("yellow", tree) |> latex_of_output_tree ppmode
  | OTCFollowList ->
      latex_of_output_tree ppmode tree


module type Printers = sig
  val print : string -> unit (* prints the string to the screen (does not add a new line at the end) *)

  val update_transition_history : (unit -> string) -> (unit -> string) -> unit
  val update_system_state : (unit -> string) -> unit

  val read_filename : string -> string

  val of_output_tree : Globals.ppmode -> output_tree -> string
end

module type S = sig
  val printf : ('a, unit, string, unit) format4 -> 'a

  val show_message : Globals.ppmode -> output_tree -> unit
  val show_warning : Globals.ppmode -> output_tree -> unit
  val show_debug :   Globals.ppmode -> output_tree -> unit

  (* print the system state and trace (normally those will be in a different window) *)
  val show_system_state : Globals.ppmode ->
      (unit -> output_tree) -> (* trace *)
      (unit -> output_tree) -> (* choice summary *)
      (unit -> output_tree) -> (* state *)
      unit

  val unmarshal_defs : string -> Interp_interface.specification
  val unmarshal_interp2_defs : string -> (Type_check.tannot Ast.defs * Type_check.Env.t)
end

exception Isa_defs_unmarshal_error of string * string

module Make (Pr : Printers) : S = struct
  let printf fmt = Printf.ksprintf Pr.print fmt

  let print ppmode output_tree =
    let s = Pr.of_output_tree ppmode output_tree in
    Pr.print s

  let show_message ppmode output_tree =
    OTClass (OTCInfo, output_tree)
    |> print ppmode

  let show_warning ppmode output_tree =
    OTClass (OTCWarning, OTConcat [OTString "Warning: "; output_tree])
    |> print ppmode

  let show_debug ppmode output_tree =
    OTVerbose (Globals.Debug, output_tree)
    |> print ppmode

  let show_system_state ppmode trace choice_summary state =
    Pr.update_transition_history
      (fun () -> Pr.of_output_tree ppmode (trace ()))
      (fun () -> Pr.of_output_tree ppmode (choice_summary ()));

    Pr.update_system_state
      (fun () -> Pr.of_output_tree ppmode (state ()))

  let unmarshal_defs isa_name =
    let bail s =
      raise (Isa_defs_unmarshal_error (isa_name, s))
    in
    let str = Pr.read_filename (isa_name ^ ".defs") in
    try
      ((Marshal.from_string (B64.decode str) 0) : Interp_interface.specification)
    with Failure s -> bail s
       | Sys_error s -> bail s
       | Not_found -> bail "invalid base64"

  let unmarshal_interp2_defs isa_name =
    let bail s =
      raise (Isa_defs_unmarshal_error (isa_name, s))
    in
    let str = Pr.read_filename (isa_name ^ ".defs") in
    try
      let (defs, env) = ((Marshal.from_string (B64.decode str) 0) : (Type_check.tannot Ast.defs * Type_check.Env.t)) in
      let replace_prover (l, tannot) =
        if Type_check.is_empty_tannot tannot then
          (l, tannot)
        else
          (l, Type_check.replace_env (Type_check.Env.set_prover (Some (Type_check.prove __POS__)) (Type_check.env_of_tannot tannot)) tannot)
      in
      (Ast_util.map_defs_annot replace_prover defs, Type_check.Env.set_prover (Some (Type_check.prove __POS__)) env)
    with Failure s -> bail s
       | Sys_error s -> bail s
       | Not_found -> bail "invalid base64"

end

type options_state = {
    run_options: RunOptions.t;
    model_params: Params.model_params;
    ppmode: Globals.ppmode;
    always_graph: bool;
    dot_final_ok: bool;
    dot_final_not_ok: bool;
    pp_hex: bool;
    dwarf_show_all_variable_locations: bool;
    verbosity: Globals.verbosity_level;
  }
