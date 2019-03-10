(*===============================================================================*)
(*                                                                               *)
(*                rmem executable model                                          *)
(*                =====================                                          *)
(*                                                                               *)
(*  This file is:                                                                *)
(*                                                                               *)
(*  Copyright Jon French, University of Cambridge        2017-2018               *)
(*  Copyright Shaked Flur, University of Cambridge       2016-2017               *)
(*  Copyright Peter Sewell, University of Cambridge           2016               *)
(*  Copyright Christopher Pulte, University of Cambridge      2017               *)
(*                                                                               *)
(*  All rights reserved.                                                         *)
(*                                                                               *)
(*  It is part of the rmem tool, distributed under the 2-clause BSD licence in   *)
(*  LICENCE.txt.                                                                 *)
(*                                                                               *)
(*===============================================================================*)

let run = Lwt_main.run

let term = run (let open Lwt in
                LTerm_inputrc.load ()
                >>= fun () ->
                Lwt.catch (fun () ->
                    Lazy.force LTerm.stdout)
                          (function exn -> Lwt.fail exn)
               )

let run_and_flush f = run (Lwt.bind f (fun () -> LTerm.flush term))

module TextPrinters : Screen_base.Printers = struct
  let print s =
    if !Globals.dumb_terminal then
      Printf.printf "%s%!" s
    else
      run_and_flush (LTerm.fprint term s)

  let clear_screen () =
    if not !Globals.dumb_terminal then
      let open Lwt in
      run (LTerm.clear_screen term >>= fun () ->
           LTerm.goto term { LTerm_geom.row = 0; LTerm_geom.col = 0; })

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

(* Adapted from Lambda-Term examples repl.ml *)

class read_line ~term ~history ~prompt_str = object(self)
  inherit LTerm_read_line.read_line ~history ()
  inherit [Zed_utf8.t] LTerm_read_line.term term

  method show_box = false

  initializer
    self#set_prompt (React.S.const (LTerm_text.eval [LTerm_text.S (prompt_str ^ ": ")]))
end

let history = LTerm_history.create []

let rec prompt ppmode maybe_options prompt_str _hist (cont: string -> unit) =
  flush_buffer ppmode;
  if !Globals.dumb_terminal then begin
      Printf.printf "%s: %!" prompt_str;
      let str =
        try read_line () with
        | End_of_file -> (Printf.printf "quit\n%!"; "quit")
      in
      cont str
    end
  else
    let open Lwt in
    run (Lwt.catch (fun () ->
             let rl = new read_line term (LTerm_history.contents history) prompt_str in
             rl#run >|= fun command -> Some (command))
           (function
            | Sys.Break -> return None
            | LTerm_read_line.Interrupt -> LTerm.fprintl term "quit" >>= (fun () -> return (Some "quit"))
            | exn -> Lwt.fail exn))
    |> function
      | Some command ->
         LTerm_history.add history command;
         cont command
      | None -> prompt ppmode maybe_options prompt_str _hist cont

let interactive = true
