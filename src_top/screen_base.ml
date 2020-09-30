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

module SO = Structured_output

exception File_read_error of string * string



module type Printers = sig
  val print : string -> unit (* prints the string to the screen (does not add a new line at the end) *)

  val update_transition_history : (unit -> string) -> (unit -> string) -> unit
  val update_system_state : (unit -> string) -> unit

  val read_filename : string -> string

  val of_structured_output : bool -> SO.t -> string
end

module type S = sig
  val printf : ('a, unit, string, unit) format4 -> 'a

  val show_message : Globals.ppmode -> SO.t -> unit
  val show_warning : Globals.ppmode -> SO.t -> unit
  val show_debug   : Globals.ppmode -> (unit -> SO.t) -> unit

  (* print the system state and trace (normally those will be in a different window) *)
  val show_system_state : Globals.ppmode ->
      (unit -> SO.t) -> (* trace *)
      (unit -> SO.t) -> (* choice summary *)
      (unit -> SO.t) -> (* state *)
      unit

  type options_state = {
    run_options: RunOptions.t;
    model_params: Params.model_params;
    ppmode: Globals.ppmode;
    pp_hex: bool;
    dwarf_show_all_variable_locations: bool;
    verbosity: SO.verbosity_level;
  }
end

module Make (Pr : Printers) : S = struct
  let printf fmt = Printf.ksprintf Pr.print fmt

  let print ppmode output =
    let s = Pr.of_structured_output ppmode.Globals.pp_colours output in
    Pr.print s

  let show_message ppmode output =
    SO.Class (SO.Info, output)
    |> print ppmode

  let show_warning ppmode output =
    SO.Class (SO.Warning, SO.Concat [SO.String "Warning: "; output])
    |> print ppmode

  let show_debug ppmode outputc =
    SO.Verbose (SO.Debug, outputc)
    |> print ppmode

  let show_system_state ppmode trace choice_summary state =
    Pr.update_transition_history
      (fun () -> Pr.of_structured_output ppmode.Globals.pp_colours (trace ()))
      (fun () -> Pr.of_structured_output ppmode.Globals.pp_colours (choice_summary ()));

    Pr.update_system_state
      (fun () -> Pr.of_structured_output ppmode.Globals.pp_colours (state ()))

    type options_state = {
      run_options: RunOptions.t;
      model_params: Params.model_params;
      ppmode: Globals.ppmode;
      pp_hex: bool;
      dwarf_show_all_variable_locations: bool;
      verbosity: SO.verbosity_level;
    }
end
