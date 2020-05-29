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

  val unmarshal_defs : string -> Interp_interface.specification
  val unmarshal_interp2_defs : string -> (Type_check.tannot Ast.defs * Type_check.Env.t)

  type options_state = {
    run_options: RunOptions.t;
    model_params: Params.model_params;
    ppmode: Globals.ppmode;
    pp_hex: bool;
    dwarf_show_all_variable_locations: bool;
    verbosity: SO.verbosity_level;
  }
end

exception Isa_defs_unmarshal_error of string * string

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

  let unmarshal_defs isa_name =
    let bail s =
      raise (Isa_defs_unmarshal_error (isa_name, s))
    in
    let str = Pr.read_filename (isa_name ^ ".defs") in
    try
      ((Marshal.from_string (Base64.decode_exn str) 0) : Interp_interface.specification)
    with Failure s -> bail s
       | Sys_error s -> bail s
       | Not_found -> bail "invalid base64"

  let unmarshal_interp2_defs isa_name =
    let bail s =
      raise (Isa_defs_unmarshal_error (isa_name, s))
    in
    let str = Pr.read_filename (isa_name ^ ".defs") in
    try
      let (defs, env) = ((Marshal.from_string (Base64.decode_exn str) 0) : (Type_check.tannot Ast.defs * Type_check.Env.t)) in
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

  type options_state = {
      run_options: RunOptions.t;
      model_params: Params.model_params;
      ppmode: Globals.ppmode;
      pp_hex: bool;
      dwarf_show_all_variable_locations: bool;
      verbosity: SO.verbosity_level;
    }
end
