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

(********************************************************************)
(** Verbosity level *)

type verbosity_level =
  | Quiet                  (* -q, minimal output and things important for herd-tools *)
  | Normal                 (* default, things normal users would like to see *)
  | ThrottledInformation   (* -v, normal mode for informed users, no more than one line
                           every 5 seconds or so *)
  | UnthrottledInformation (* -v -v, even more information, might render the output unusable *)
  | Debug                  (* -debug, cryptic information *)

let verbosity = ref Normal

let increment_verbosity = fun () ->
  verbosity :=
    begin match !verbosity with
    | Quiet                  -> Normal
    | Normal                 -> ThrottledInformation
    | ThrottledInformation   -> UnthrottledInformation
    | UnthrottledInformation -> Debug
    | Debug                  -> Debug
    end

let is_verbosity_at_least (verb: verbosity_level) : bool =
  let int_of_verbosity_level = function
    | Quiet                  -> 0
    | Normal                 -> 1
    | ThrottledInformation   -> 2
    | UnthrottledInformation -> 3
    | Debug                  -> 4
  in
  int_of_verbosity_level !verbosity >= int_of_verbosity_level verb

(********************************************************************)
(** Structured output that can be realized as string or HTML or LaTeX
or whatever *)

type class_t =
  | Info
  | Warning
  | Final
  | FollowList
  | CommandUsage

type line_width =
  | Normal
  | Thick

(* TODO: move to pp.ml *)
type t =
  | Empty
  | String of string
  | Encoded of string
  | Line of t
  | Emph of t
  | Color of string * t (* Do not use this directly, instead use a class *)
  | Center of t
  (* Verbose takes a continuation because we do not want to waste time
  if the result is not printed out (specially for debug prints) *)
  | Verbose of verbosity_level * (unit -> t)
  | Concat of t list
  | Class of class_t * t
  | HorLine of line_width

let str format             = Printf.ksprintf (fun s -> String s) format
let strLine format         = Printf.ksprintf (fun s -> Line (String s)) format
let strEmph format         = Printf.ksprintf (fun s -> Emph (String s)) format
let strVerbose verb format = Printf.ksprintf (fun s -> Verbose (verb, fun () -> String s)) format
let strClass cls format    = Printf.ksprintf (fun s -> Class (cls, String s)) format

let encoded str = Encoded str
let line t = Line t
let emph t = Emph t
let center t = Center t
let verbose verb tc = Verbose (verb, tc)
let concat ts = Concat ts
let aclass cls t = Class (cls, t)
(* TODO: change ifTrue to take continuation? *)
let ifTrue b t = if b then t else Empty

let concatWith sep = function
  | [] -> Empty
  | hd :: tl ->
      List.fold_left (fun acc t -> t :: sep :: acc) [hd] tl
      |> List.rev
      |> concat

(** String **********************************************************)

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

let to_string pp_colours t : string =
  let rec helper graphics = function
  | Empty -> ""
  | String str -> str
  | Encoded str ->
      (graphics_mode_seq true []) ^
      str ^
      (graphics_mode_seq true graphics)
  | Line t -> (helper graphics t) ^ "\n"
  | Emph t ->
      if pp_colours then
        (graphics_mode_seq false [bold_attr]) ^ (helper (bold_attr :: graphics) t) ^
        (graphics_mode_seq true graphics)
      else
        helper graphics t
  | Color (color, t) ->
      if pp_colours then
        let color = fg_color_graphics_mode color in
        (graphics_mode_seq false [color]) ^
        (helper (color :: graphics) t) ^
        (graphics_mode_seq true graphics)
      else
        helper graphics t
  | Center t -> helper graphics t
  | Class (cls, t) -> class_helper cls graphics t
  | Verbose (verb, tc) ->
      if is_verbosity_at_least verb then helper graphics (tc ())
      else ""
  | Concat ts -> String.concat "" (List.map (helper graphics) ts)
  | HorLine typ ->
      begin match typ with
      | Normal ->
          String (String.make 80 '-')
      | Thick ->
          String (String.make 80 '=')
      end
      |> line
      |> helper graphics
  and class_helper cls graphics t : string =
    match cls with
    | Info -> Color ("cyan", t)   |> helper graphics
    | Warning
    | CommandUsage
        -> Color ("yellow", t) |> helper graphics
    | Final
    | FollowList
        -> helper graphics t
  in
  helper [] t

(** HTML ************************************************************)

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

let rec to_html pp_colours t : string =
  match t with
  | Empty -> ""
  | String str -> html_escape str
  | Encoded str -> str
  (* We use "<p>...</p>" and not "...<br>" because it works better with
  the "scroll on output" UI option *)
  | Line t -> "<p class='rmem'>" ^ (to_html pp_colours t) ^ "</p>"
  | Emph t -> "<b class='rmem'>" ^ (to_html pp_colours t) ^ "</b>"
  | Color (color, t) ->
      if pp_colours then
        "<span class='rmem' style='color:" ^ color ^ "'>" ^ to_html pp_colours t ^ "</span>"
      else
        to_html pp_colours t
  | Center t -> "<center class='rmem'>" ^ to_html pp_colours t ^ "</center>"
  | Class (cls, t) ->
      if pp_colours then
        to_html_class pp_colours cls t
      else
        to_html pp_colours t
  | Verbose (verb, tc) ->
      if is_verbosity_at_least verb then to_html pp_colours (tc ())
      else ""
  | Concat ts -> String.concat "" (List.map (to_html pp_colours) ts)
  | HorLine Normal -> "<hr class='rmem' style=\"border: 1px solid;\">"
  | HorLine Thick  -> "<hr class='rmem' style=\"border: 3px solid;\">"
and to_html_class pp_colours cls t : string =
  match cls with
  | Info ->
      "<span class='rmem info'>" ^ (to_html pp_colours t) ^ "</span>"
  | Warning ->
      "<span class='rmem warning'>" ^ (to_html pp_colours t) ^ "</span>"
  | Final ->
      "<span class='rmem final'>" ^ (to_html pp_colours t) ^ "</span>"
  | FollowList ->
      "<span class='rmem follow_list'>" ^ (to_html pp_colours t) ^ "</span>"
  | CommandUsage ->
      "<span class='rmem command_usage'>" ^ (to_html pp_colours t) ^ "</span>"

(** LaTeX ***********************************************************)

let rec to_latex pp_colours t : string =
  match t with
  | Empty -> ""
  | String str -> str
  | Encoded str -> str
  | Line t -> (to_latex pp_colours t) ^ "\n"
  | Emph t ->
      if pp_colours then
        "\\mybold{" ^ (to_latex pp_colours t) ^ "}"
      else
        to_latex pp_colours t
  | Color (color, t) -> "\\my" ^ color ^ "{" ^ (to_latex pp_colours t) ^ "}"
  | Center t -> "\\begin{center}" ^ (to_latex pp_colours t) ^ "\\end{center}"
  | Class (cls, t) -> to_latex_class pp_colours cls t
  | Verbose (verb, tc) ->
      if is_verbosity_at_least verb then to_latex pp_colours (tc ())
      else ""
  | Concat ts -> String.concat "" (List.map (to_latex pp_colours) ts)
  | HorLine Normal -> "\n\\line\n"
  | HorLine Thick  -> "\n\\noindent\\rule{\\textwidth}{3pt}\n"
and to_latex_class pp_colours cls t : string =
  match cls with
  | Info ->
      Color ("cyan", t) |> to_latex pp_colours
  | Warning ->
      Color ("yellow", t) |> to_latex pp_colours
  | Final ->
      Color ("yellow", t) |> to_latex pp_colours
  | FollowList ->
      to_latex pp_colours t
  | CommandUsage ->
      Color ("yellow", t) |> to_latex pp_colours
