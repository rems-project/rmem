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
(* TODO: move to pp.ml *)
type output_tree =
  | OTEmpty
  | OTString of string
  | OTLine of output_tree
  | OTEmph of output_tree
  | OTColor of string * output_tree
  | OTCenter of output_tree
  | OTWarning of output_tree
  | OTVerbose of Globals.verbosity_level * output_tree
  | OTConcat of output_tree list
  | OTFollowList of string

let otString format          = Printf.ksprintf (fun s -> OTString s) format
let otStrLine format         = Printf.ksprintf (fun s -> OTLine (OTString s)) format
let otStrEmph format         = Printf.ksprintf (fun s -> OTEmph (OTString s)) format
let otStrColor color format  = Printf.ksprintf (fun s -> OTColor (color, OTString s)) format
let otStrVerbose verb format = Printf.ksprintf (fun s -> OTVerbose (verb, OTString s)) format

let otLine tree = OTLine tree
let otEmph tree = OTEmph tree
let otColor color tree = OTColor (color, tree)
let otCenter tree = OTCenter tree
let otWarning tree = OTWarning tree
let otVerbose verb tree = OTVerbose (verb, tree)
let otIfTrue b tree = if b then tree else OTEmpty

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

let rec string_of_output_tree tree : string =
  begin match tree with
  | OTEmpty -> ""
  | OTString str -> str
  | OTLine tree -> (string_of_output_tree tree) ^ "\n"
  | OTEmph tree -> string_of_output_tree tree
  | OTColor (color, tree) -> (string_of_color color) ^ (string_of_output_tree tree) ^ reset_color
  | OTCenter tree -> string_of_output_tree tree
  | OTWarning tree -> "Warning: " ^ (string_of_output_tree tree) ^ "\n"
  | OTVerbose (verb, tree) ->
      if Globals.is_verbosity_at_least verb then string_of_output_tree tree
      else ""
  | OTConcat trees -> String.concat "" (List.map string_of_output_tree trees)
  | OTFollowList s -> s
  end

let rec html_of_output_tree tree : string =
  begin match tree with
  | OTEmpty -> ""
  | OTString str -> str
  | OTLine tree -> (html_of_output_tree tree) ^ "<br>"
  | OTEmph tree -> "<b>" ^ (html_of_output_tree tree) ^ "</b>"
  | OTColor (color, tree) -> "<span style='color:" ^ color ^ "'>" ^ html_of_output_tree tree ^ "</span>"
  | OTCenter tree -> "<center>" ^ html_of_output_tree tree ^ "</center>"
  | OTWarning tree ->"<span style='color:red'>" ^ "Warning: " ^ (html_of_output_tree tree) ^ "<br></span>"
  | OTVerbose (verb, tree) ->
      if Globals.is_verbosity_at_least verb then html_of_output_tree tree
      else ""
  | OTConcat trees -> String.concat "" (List.map html_of_output_tree trees)
  | OTFollowList s -> "<span class=\"follow_list\">" ^ s ^ "</span>"
  end

let rec latex_of_output_tree tree : string =
  begin match tree with
  | OTEmpty -> ""
  | OTString str -> str
  | OTLine tree -> (latex_of_output_tree tree) ^ "\n"
  | OTEmph tree -> "\\mybold{" ^ (latex_of_output_tree tree) ^ "}"
  | OTColor (color, tree) -> "\\my" ^ color ^ "{" ^ (latex_of_output_tree tree) ^ "}"
  | OTCenter tree -> "\\begin{center}" ^ (latex_of_output_tree tree) ^ "\\end{center}"
  | OTWarning tree -> "Warning: " ^ (latex_of_output_tree tree) ^ "\n"
  | OTVerbose (verb, tree) ->
      if Globals.is_verbosity_at_least verb then latex_of_output_tree tree
      else ""
  | OTConcat trees -> String.concat "" (List.map latex_of_output_tree trees)
  | OTFollowList s -> s
  end

module type Printers = sig
  (* val print : string -> unit *)
  val println : string -> unit
  val clear_screen : unit -> unit
  val escape : string -> string
  val update_transition_history : string -> string -> unit
  val read_filename : string -> string
end

module type S = sig
  include Printers

  val printfln : ('a, unit, string, unit) format4 -> 'a
  val show_warning : Globals.ppmode -> ('a, unit, string, unit) format4 -> 'a
  val show_message : Globals.ppmode -> ('a, unit, string, unit) format4 -> 'a
  val show_verbosity : Globals.ppmode -> Globals.verbosity_level -> ('a, unit, string, unit) format4 -> 'a
  val show_debug : Globals.ppmode -> ('a, unit, string, unit) format4 -> 'a
  val final_message : Globals.ppmode -> bool -> string -> unit
  val draw_system_state : Globals.ppmode ->
                          Interact_parser_base.ast list ->
                          Interact_parser_base.ast list ->
                          ('ts,'ss) MachineDefTypes.system_state ->
                          ('ts,'ss) MachineDefTypes.ui_system_state ->
                          MachineDefCandidateExecution.cex_candidate option ->
                          ('ts,'ss) MachineDefTypes.ui_trans list ->
                          ('ts,'ss) MachineDefTypes.trans list ->
                          unit
  val flush_buffer : Globals.ppmode -> unit
  val unmarshal_defs : string -> Interp_interface.specification
  val unmarshal_interp2_defs : string -> (Type_check.tannot Ast.defs * Type_check.Env.t)
end

exception Isa_defs_unmarshal_error of string * string

module Make (Pr : Printers) : S = struct
  include Pr

  (* let printf fmt = Printf.ksprintf print fmt *)
  let printfln fmt = Printf.ksprintf println fmt
  let line = "----------------------------------------------------------------------------"

  let message_buffer = ref ([] : string list)

  let queue_message ppmode s =
    if ppmode.Globals.pp_buffer_messages then
      message_buffer := s :: !message_buffer
    else
      println s

  let show_message ppmode format =
    Printf.ksprintf (fun s -> Pp.colour_info ppmode s |> queue_message ppmode) format

  let show_verbosity ppmode verbosity format =
    if Globals.is_verbosity_at_least verbosity then
      show_message ppmode format
    else
      (* ignore the message in an ocaml-printf-type-safe way. *)
      (* this still invokes the printf machinery so isn't ideal for performance, but what can you do *)
      (* note: Printf.i[k]fprintf seem to artificially have too restrictive types *)
      let CamlinternalFormatBasics.Format (fmt, _) = format in
      CamlinternalFormat.make_printf (fun _ _ -> ()) () CamlinternalFormat.End_of_acc fmt

  let show_debug ppmode format =
    show_verbosity ppmode Globals.Debug format

  let show_warning ppmode format =
    Printf.ksprintf (fun s -> Pp.colour_warning ppmode s |> queue_message ppmode) format

  let final_message ppmode append s =
    Pp.colour_final ppmode s |> queue_message ppmode

  let flush_buffer ppmode =
    (if ppmode.Globals.pp_buffer_messages then begin
         List.iter (fun s -> println s) (List.rev !message_buffer);
         message_buffer := []
       end
     else
       ())

  let make_bufferer r =
    fun s -> r := s :: !r

  let choice_summary_str ppmode choices_so_far follow_suffix numbered_cands disabled_trans force_verbose =
    let choice_summary_lines = ref ([] : string list) in
    let buffer_line = make_bufferer choice_summary_lines in

    let n_choices = List.length choices_so_far in
      let limited_choices = (match ppmode.Globals.pp_choice_history_limit with
                             | Some n -> Lem_list.take n choices_so_far
                             | None -> choices_so_far
                            ) in
      let choice_str = Printf.sprintf "%s%s"
                                      (if (List.length limited_choices) < n_choices then "..." else "")
                                      (Interact_parser_base.history_to_string limited_choices)
      in

      let choices_str = Printf.sprintf "Choices so far (%n): \"%s\"" n_choices choice_str in

    let follow_str = match follow_suffix with
      | _ :: _ ->
         Printf.sprintf " remaining follow-spec: [%s]"
            (List.rev follow_suffix |> Interact_parser_base.history_to_string)
      | [] -> ""
    in

    buffer_line (choices_str ^ follow_str);

    if numbered_cands = [] then
      buffer_line "No enabled transitions"
    else
      begin
        match (ppmode.Globals.pp_style, force_verbose) with
        | (Globals.Ppstyle_compact, false) ->
           buffer_line (Printf.sprintf "%d enabled transitions" (List.length numbered_cands))
        | _ ->
           buffer_line (Pp.colour_bold ppmode "Enabled transitions:");
           List.iter
             (fun cand ->
               buffer_line (Pp.pp_cand ppmode cand))
             numbered_cands;
      end;

    if disabled_trans = [] then
      buffer_line "No disabled transitions"
    else
      begin
        match (ppmode.Globals.pp_style, force_verbose) with
        | (Globals.Ppstyle_compact, false) ->
           buffer_line (Printf.sprintf "%d disabled transitions" (List.length disabled_trans))
        | _ ->
           buffer_line (Pp.colour_bold ppmode "Disabled transitions:");
           (List.iter (fun t -> buffer_line (Pp.pp_trans ppmode t)) disabled_trans)
      end;

    String.concat "\n" (List.rev !choice_summary_lines)


  let draw_system_state
        (ppmode:         Globals.ppmode)
        (choices_so_far: Interact_parser_base.ast list)
        (follow_suffix:  Interact_parser_base.ast list)
        (_state:         ('ts,'ss) MachineDefTypes.system_state)
        (ui_state:       ('ts,'ss) MachineDefTypes.ui_system_state)
        (_candidate_ex:  MachineDefCandidateExecution.cex_candidate option)
        (numbered_cands: ('ts,'ss) MachineDefTypes.ui_trans list)
        (disabled_trans: ('ts,'ss) MachineDefTypes.trans list)
      : unit
    =
    let state_lines = ref ([] : string list) in
    let buffer_line = make_bufferer state_lines in

    (if ppmode.Globals.pp_suppress_newpage then
       buffer_line line);

    buffer_line (Pp.pp_ui_system_state ppmode ui_state);

    (* hack to pp plain instruction tree of thread 0 *)
    (* (match state with PLDI s' -> Printf.printf "%s\n" (Pp.pp_plain_instruction_tree ppmode 0 "" ((Pmap.find 0 s'.thread_states).instruction_tree)));*)

    buffer_line (choice_summary_str ppmode
                                    choices_so_far
                                    follow_suffix
                                    numbered_cands
                                    disabled_trans
                                    false);

    if ppmode.Globals.pp_style <> Globals.Ppstyle_compact then
      buffer_line line
    else
      ();

    begin match ppmode.Globals.pp_kind with
    | Globals.Ascii | Globals.Html ->
       (* flip this for interactive debug printing from model *)
       if not (ppmode.Globals.pp_suppress_newpage || !Debug.debug) then
         clear_screen ()
    | _ -> ()
    end;

    update_transition_history (Pp.pp_transition_history ppmode ui_state)
                              (choice_summary_str ppmode
                                                  choices_so_far
                                                  follow_suffix
                                                  numbered_cands
                                                  disabled_trans
                                                  true);

    List.iter (fun s -> println s) (List.rev !state_lines)

  let unmarshal_defs isa_name =
    let bail s =
      raise (Isa_defs_unmarshal_error (isa_name, s))
    in
    let str = read_filename (isa_name ^ ".defs") in
    try
      ((Marshal.from_string (B64.decode str) 0) : Interp_interface.specification)
    with Failure s -> bail s
       | Sys_error s -> bail s
       | Not_found -> bail "invalid base64"

  let unmarshal_interp2_defs isa_name =
    let bail s =
      raise (Isa_defs_unmarshal_error (isa_name, s))
    in
    let str = read_filename (isa_name ^ ".defs") in
    try
      let (defs, env) = ((Marshal.from_string (B64.decode str) 0) : (Type_check.tannot Ast.defs * Type_check.Env.t)) in
      (defs, Type_check.Env.set_prover (Some (Type_check.prove __POS__)) env)
    with Failure s -> bail s
       | Sys_error s -> bail s
       | Not_found -> bail "invalid base64"

end

type options_state = {
    run_options: RunOptions.t;
    model_params: MachineDefTypes.model_params;
    ppmode: Globals.ppmode;
    always_graph: bool;
    dot_final_ok: bool;
    dot_final_not_ok: bool;
    pp_hex: bool;
    dwarf_show_all_variable_locations: bool;
    verbosity: Globals.verbosity_level;
  }
