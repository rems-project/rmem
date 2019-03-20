(*===============================================================================*)
(*                                                                               *)
(*                rmem executable model                                          *)
(*                =====================                                          *)
(*                                                                               *)
(*  This file is:                                                                *)
(*                                                                               *)
(*  Copyright Shaked Flur, University of Cambridge 2017                          *)
(*                                                                               *)
(*  All rights reserved.                                                         *)
(*                                                                               *)
(*  It is part of the rmem tool, distributed under the 2-clause BSD licence in   *)
(*  LICENCE.txt.                                                                 *)
(*                                                                               *)
(*===============================================================================*)

type output_chan =
  | NoOutput
  | FileName of string
  | OpenChan of out_channel

let state_output : output_chan ref = ref NoOutput
let trace_output : output_chan ref = ref NoOutput

let set_state_output str =
  state_output :=
    if str = "" then NoOutput
    else FileName str

let set_trace_output str =
  trace_output :=
    if str = "" then NoOutput
    else FileName str

let clear_screen_to_chan chan =
  output_string chan "\027[;H\027[J";
  output_string chan "============================================================================\n"


module TextPrinters : Screen_base.Printers = struct
  let print s = Printf.printf "%s%!" s

  let update_transition_history history available = ()

  let read_filename basename =
    let bail s =
      raise (Screen_base.Isa_defs_unmarshal_error (basename, s))
    in
    let filename =
      match !Globals.isa_defs_path with
      | Some path -> Filename.concat path basename
      | None -> raise (Screen_base.Isa_defs_unmarshal_error (basename, "have no valid ISA defs path!"))
    in
    let f =
      try
        open_in_bin filename
      with Sys_error s -> bail s
    in
    let str =
      try
        really_input_string f (in_channel_length f)
      with Sys_error s -> bail s
         | End_of_file -> bail "End_of_file"
         | Invalid_argument s -> bail ("Invalid_argument " ^ s)
    in
    (try close_in f with Sys_error s -> bail s);
    str

  let of_output_tree = Screen_base.string_of_output_tree

  let update_transition_history trace choice_summary =
    let to_chan chan =
      try
        clear_screen_to_chan chan;
        trace () |> output_string chan;
        choice_summary () |> output_string chan;
        flush chan
      with
      | Sys_error msg -> print (Printf.sprintf "Cannot write to 'trace_output': %s\n" msg)
    in

    match !trace_output with
    | NoOutput -> ()
    | FileName name ->
        (* FIXME: use open_out_gen? *)
        begin match open_out name with
        | chan ->
            trace_output := OpenChan chan;
            to_chan chan
        | exception Sys_error msg ->
            print (Printf.sprintf "%s\n" msg)
        end
    | OpenChan chan -> to_chan chan


  let update_system_state state =
    let to_chan chan =
      try
        clear_screen_to_chan chan;
        state () |> output_string chan;
        flush chan
      with
      | Sys_error msg -> print (Printf.sprintf "Cannot write to 'state_output': %s\n" msg)
    in

    match !state_output with
    | NoOutput -> ()
    | FileName name ->
        (* FIXME: use open_out_gen? *)
        begin match open_out name with
        | chan ->
            state_output := OpenChan chan;
            to_chan chan
        | exception Sys_error msg ->
            print (Printf.sprintf "%s\n" msg)
        end
    | OpenChan chan -> to_chan chan
end

include (Screen_base.Make (TextPrinters))

let quit = fun () -> (exit 0 |> ignore)


module Dot (ConcModel: Concurrency_model.S) = struct
  let display_dot ppmode legend_opt cex (nc: ConcModel.ui_trans list) =
    Screen_base.OTString "dot rendering not implemented on terminal yet"
    |> show_warning ppmode
end

let rec prompt ppmode maybe_options prompt_ot _hist (cont: string -> unit) =
  Screen_base.string_of_output_tree ppmode prompt_ot
  |> Printf.printf "%s: %!";
  let str =
    try read_line () with
    | End_of_file -> (Printf.printf "quit\n%!"; "quit")
  in
  cont str

let interactive = true
