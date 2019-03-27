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
  (* OTVerbose takes a continuation because we do not want to waste time
  if the result is not printed out (specially for debug prints) *)
  | OTVerbose of Globals.verbosity_level * (unit -> output_tree)
  | OTConcat of output_tree list
  | OTClass of ot_calss * output_tree
  | OTHorLine

let otString format          = Printf.ksprintf (fun s -> OTString s) format
let otStrLine format         = Printf.ksprintf (fun s -> OTLine (OTString s)) format
let otStrEmph format         = Printf.ksprintf (fun s -> OTEmph (OTString s)) format
let otStrColor color format  = Printf.ksprintf (fun s -> OTColor (color, OTString s)) format
let otStrVerbose verb format = Printf.ksprintf (fun s -> OTVerbose (verb, fun () -> OTString s)) format
let otStrClass cls format    = Printf.ksprintf (fun s -> OTClass (cls, OTString s)) format

let otEncoded str = OTEncoded str
let otLine tree = OTLine tree
let otEmph tree = OTEmph tree
let otColor color tree = OTColor (color, tree)
let otCenter tree = OTCenter tree
let otVerbose verb treec = OTVerbose (verb, treec)
let otConcat trees = OTConcat trees
let otClass cls tree = OTClass (cls, tree)
let otIfTrue b tree = if b then tree else OTEmpty

let otConcatWith sep = function
  | [] -> OTEmpty
  | hd :: tl ->
      List.fold_left (fun acc t -> t :: sep :: acc) [hd] tl
      |> List.rev
      |> otConcat

let reset_graphics  = 0
let bold_attr       = 1
let underscore_attr = 4
let blink_attr      = 5
let reverse_attr    = 7
let concealed_attr  = 8
let fg_color_graphics_mode = function
  | "black"         -> 30
  | "red"           -> 31
  | "green"         -> 32
  | "yellow"        -> 33
  | "blue"          -> 34
  | "magenta"       -> 35
  | "cyan"          -> 36
  | "white"         -> 37
  (** I think the 90s/100s are not in the standard **)
  | "dark_gray"     -> 90
  | "light_red"     -> 91
  | "light_green"   -> 92
  | "light_yellow"  -> 93
  | "light_blue"    -> 94
  | "light_magenta" -> 95
  | "light_cyan"    -> 96
  | "light_white"   -> 97
  | color -> failwith ("unknown color: " ^ color)
let bg_color_graphics_mode c = (fg_color_graphics_mode c) + 10
let graphics_mode_seq reset mode : string =
  (* the head of mode is applied last *)
  List.rev mode
  |> (fun l -> if reset then reset_graphics :: l else l)
  |> List.map string_of_int
  |> String.concat ";"
  |> Printf.sprintf "\x1b[%sm"

let string_of_output_tree ppmode tree : string =
  let rec helper graphics = function
  | OTEmpty -> ""
  | OTString str -> str
  | OTEncoded str ->
      (graphics_mode_seq true []) ^
      str ^
      (graphics_mode_seq true graphics)
  | OTLine tree -> (helper graphics tree) ^ "\n"
  | OTEmph tree ->
      if ppmode.Globals.pp_colours then
        (graphics_mode_seq false [bold_attr]) ^ (helper (bold_attr :: graphics) tree) ^
        (graphics_mode_seq true graphics)
      else
        helper graphics tree
  | OTColor (color, tree) ->
      if ppmode.Globals.pp_colours then
        let color = fg_color_graphics_mode color in
        (graphics_mode_seq false [color]) ^
        (helper (color :: graphics) tree) ^
        (graphics_mode_seq true graphics)
      else
        helper graphics tree
  | OTCenter tree -> helper graphics tree
  | OTClass (cls, tree) -> class_helper cls graphics tree
  | OTVerbose (verb, treec) ->
      if Globals.is_verbosity_at_least verb then helper graphics (treec ())
      else ""
  | OTConcat trees -> String.concat "" (List.map (helper graphics) trees)
  | OTHorLine ->
      OTString "----------------------------------------------------------------------------"
      |> otLine
      |> helper graphics
  and class_helper cls graphics tree : string =
    match cls with
    | OTCInfo    -> OTColor ("cyan", tree)   |> helper graphics
    | OTCWarning -> OTColor ("yellow", tree) |> helper graphics
    | OTCFinal
    | OTCFollowList
        -> helper graphics tree
  in
  helper [] tree


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
  | OTVerbose (verb, treec) ->
      if Globals.is_verbosity_at_least verb then html_of_output_tree ppmode (treec ())
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
  | OTVerbose (verb, treec) ->
      if Globals.is_verbosity_at_least verb then latex_of_output_tree ppmode (treec ())
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
  val show_debug   : Globals.ppmode -> (unit -> output_tree) -> unit

  (* print the system state and trace (normally those will be in a different window) *)
  val show_system_state : Globals.ppmode ->
      (unit -> output_tree) -> (* trace *)
      (unit -> output_tree) -> (* choice summary *)
      (unit -> output_tree) -> (* state *)
      unit

  val unmarshal_defs : string -> Interp_interface.specification
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

  let show_debug ppmode output_treec =
    OTVerbose (Globals.Debug, output_treec)
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

end

type options_state = {
    run_options: RunOptions.t;
    model_params: Params.model_params;
    ppmode: Globals.ppmode;
    pp_hex: bool;
    dwarf_show_all_variable_locations: bool;
    verbosity: Globals.verbosity_level;
  }
