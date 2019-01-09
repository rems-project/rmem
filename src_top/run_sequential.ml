(*===============================================================================*)
(*                                                                               *)
(*                rmem executable model                                          *)
(*                =====================                                          *)
(*                                                                               *)
(*  This file is:                                                                *)
(*                                                                               *)
(*  Copyright Shaked Flur, University of Cambridge       2017                    *)
(*  Copyright Jon French, University of Cambridge        2018                    *)
(*  Copyright Christopher Pulte, University of Cambridge 2018                    *)
(*                                                                               *)
(*  All rights reserved.                                                         *)
(*                                                                               *)
(*  The rmem tool is distributed under the 2-clause BSD license in LICENCE.txt.  *)
(*  For author information see README.md.                                        *)
(*                                                                               *)
(*===============================================================================*)

open RunOptions
open Globals
open MachineDefTypes
open Interact
open ConstrGen
open Screen_base

open Printf

let write_snapshot ppmode snapshot : unit =
  let (filename, snapshots) = !Globals.snapshot_data in
  let snapshots' = snapshot :: snapshots in
  Globals.snapshot_data := (filename, snapshots');
  (* and write logfile if we've hit the second *)
  if filename <> "" then
    begin match snapshots' with
    | [(ioid', regs', mem'); (ioid, regs, mem)] ->
       let fd = open_out filename in
       output_string fd (Pp.pp_logfile ppmode (ioid, regs, mem) (ioid', regs', mem'));
       close_out fd
    | _ -> ()
    end


let calc_final ppmode isa_info test_name initial_setup =
  let open MachineDefSystemSequential in
  let open Pp in
  let run_address = initial_setup.run_address0 in

  let rec run counter address state =
    let fake_ioid = (0,counter) in

    (* TODO: Do something better here? This is always only running the first trace. *)
    match List.hd (run_address address state) with
    | Fetch_error0 address ->
       let addr_pp = pp_address ppmode (Some fake_ioid) address in
       print_endline ("Fetch error at " ^ addr_pp);
       print_endline (Pp.pp_sequential_state ppmode fake_ioid isa_info state);
    | Decode_error0 (address,decode_error) ->
       let addr_pp = pp_address ppmode (Some fake_ioid) address in
       let err_pp = pp_decode_error ppmode decode_error address in
       print_endline ("Decode error at " ^ addr_pp ^ ": " ^ err_pp)
    | Exception0 (error,state) ->
       let err_pp = error in
       print_endline ("Error " ^ err_pp)
    | Final_state state ->
       let counter_pp = string_of_int counter in
       print_endline ("Finished without error. (" ^ counter_pp ^ " instructions)");
       print_endline (Pp.pp_sequential_state ppmode fake_ioid isa_info state);
    | Next_state (instr,state',address',take_snapshot) ->
       let instr_pp = pp_instruction ppmode.pp_symbol_table instr address in
       let addr_pp = pp_address ppmode (Some fake_ioid) address in
       print_endline ("Running " ^ addr_pp ^ ":  " ^ instr_pp);
       if take_snapshot
       then write_snapshot ppmode (snapshot_of_state fake_ioid isa_info state');
       run (counter+1) address' state';
  in

  run 0 initial_setup.initial_address initial_setup.initial_state
