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
(*  The rmem tool is distributed under the 2-clause BSD license in LICENCE.txt.  *)
(*  For author information see README.md.                                        *)
(*                                                                               *)
(*===============================================================================*)

let of_output_tree = Screen_base.string_of_output_tree;;

let final_state_color = "green"

module TextPrinters : Screen_base.Printers = struct
  let println s = Printf.printf "%s\n" s
  let clear_screen = fun () -> ()
  let escape s = s
  let update_transition_history _ _ = ()
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
    (try
       close_in f
     with Sys_error s -> bail s);
    str
end

include (Screen_base.Make (TextPrinters))

let clear_warnings : unit -> unit = fun () -> ()

let quit = fun () -> ()


let display_dot ppmode legend_opt s cex (nc: (int * ('ts,'ss) MachineDefTypes.trans) list) =
  show_warning ppmode "dot rendering not implemented on terminal yet"

let prompt ppmode maybe_options prompt_str _hist cont = ()

let interactive = false
