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

module TextPrinters : Screen_base.Printers = struct
  let print s = Printf.printf "%s%!" s

  (* cursor to top left and clear *)
  let clear_screen () = Printf.printf "\027[;H\027[J"

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
end

include (Screen_base.Make (TextPrinters))

let quit = fun () -> (exit 0 |> ignore)


let display_dot ppmode legend_opt s cex (nc: (int * ('ts,'ss) MachineDefTypes.trans) list) =
  Screen_base.OTString "dot rendering not implemented on terminal yet"
  |> show_warning ppmode

let rec prompt ppmode maybe_options prompt_str _hist (cont: string -> unit) =
  flush_buffer ppmode;
  Printf.printf "%s: %!" prompt_str;
  let str =
    try read_line () with
    | End_of_file -> (Printf.printf "quit\n%!"; "quit")
  in
  cont str

let interactive = true
