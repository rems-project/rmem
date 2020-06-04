(*=============================================================================================*)
(*                                                                                             *)
(*                rmem executable model                                                        *)
(*                =====================                                                        *)
(*                                                                                             *)
(*  This file is:                                                                              *)
(*                                                                                             *)
(*  Copyright Shaked Flur, University of Cambridge                                 2015-2017   *)
(*  Copyright Pankaj Pawan, IIT Kanpur and INRIA (when this work was done)         2011-2012   *)
(*  Copyright Jon French, University of Cambridge                                  2017-2018   *)
(*  Copyright Peter Sewell, University of Cambridge                          2011-2014, 2017   *)
(*  Copyright Susmit Sarkar, University of St Andrews                             2012, 2014   *)
(*  Copyright Ohad Kammar, University of Cambridge (when this work was done)            2013   *)
(*  Copyright Jean Pichon-Pharabod, University of Cambridge                             2014   *)
(*  Copyright Francesco Zappa Nardelli, INRIA, Paris, France                            2011   *)
(*                                                                                             *)
(*  All rights reserved.                                                                       *)
(*                                                                                             *)
(*  It is part of the rmem tool, distributed under the 2-clause BSD licence in                 *)
(*  LICENCE.txt.                                                                               *)
(*                                                                                             *)
(*=============================================================================================*)

open InstructionSemantics
open Params
open Js_of_ocaml

let js = Js.string;;
let document = Dom_html.window##.document;;

let elf_file_name : string option ref = ref None;;
let elf_data : (char list) ref = ref [];;

let set_elf_data file_name array_buffer length =
  elf_file_name := Some file_name;

  let rec int8Array_to_char_list arr pos last acc =
    if pos <= last then
      int8Array_to_char_list arr (pos + 1) last (char_of_int (Typed_array.unsafe_get arr pos) :: acc)
    else List.rev acc
  in

  elf_data := int8Array_to_char_list array_buffer 0 (length - 1) []
;;

let getElementById x =
  Js.Opt.get (document##(getElementById (js x)))
  (fun () ->
      Printf.printf "could not find element ID: \"%s\"\n" x;
      assert false)
;;

let topo_radio name value =
  let b =
    Dom_html.createInput
      ~_type:(js "radio")
      ~name:(js name)
      Dom_html.document
  in
  b##.id := js (name ^ "_" ^ value);
  b##.value := js value;

  let label = Dom_html.createLabel Dom_html.document in
  Dom.appendChild label b;
  Dom.appendChild label (Dom_html.document##(createTextNode (js value)));

  let div = Dom_html.createDiv document in
  Dom.appendChild div label;
  div
;;


let check_checkbox (id: string) (checked: bool) : unit =
  (Js.Unsafe.coerce (getElementById id))##.checked := (Js.bool checked)

let is_checked_checkbox (id: string) : bool =
  Js.to_bool (Js.Unsafe.coerce (getElementById id))##.checked

let check_radio (id: string) (checked: bool) : unit =
  (Js.Unsafe.coerce (getElementById id))##.checked := (Js.bool checked)

let is_checked_radio (id: string) : bool =
  Js.to_bool (Js.Unsafe.coerce (getElementById id))##.checked

let assign_number (id: string) (num: int) : unit =
  (Js.Unsafe.coerce (getElementById id))##.value := (Js.string (string_of_int num))

let read_number (id: string) : int =
  int_of_string (Js.to_string (Js.Unsafe.coerce (getElementById id))##.value)

let default_options : unit -> RunOptions.t = fun () ->
(*   Globals.verbosity := Globals.Quiet; *)
  Globals.set_pp_kind "Html";
  Globals.pp_colours := true;
  Globals.auto_internal := false;
  Globals.pp_style := Globals.Ppstyle_compact;
  Globals.pp_prefer_symbolic_values := true;
  Globals.print_hex := true;
  Globals.elf_threads := 1;
  Globals.use_dwarf := true;
  Globals.dwarf_show_all_variable_locations := true;
  Globals.run_dot := Some Globals.RD_step;

  Globals.model_params := Params.default_model_params;

  Globals.model_params := Model_aux.parse_and_update_model "allow_tree_speculation" !Globals.model_params;
  Globals.model_params := Model_aux.parse_and_update_model "promise_anytime" !Globals.model_params;

  Globals.auto_follow := false;
  Globals.interactive_auto := false;

  Pp.linebreak_init();

  { RunOptions.default_options with
      RunOptions.interactive         = true;
      RunOptions.pseudorandom        = false;
      RunOptions.pseudorandom_traces = 1;
      RunOptions.always_print        = false;
      RunOptions.interpreter         = true;
  }
;;

let model_to_html : unit -> unit = fun () ->
  check_radio "model_pldi11"
              (List.assoc (Model_aux.model_value !Globals.model_params) Model_aux.model_assoc = "pldi11");
  check_radio "model_flowing"
              (List.assoc (Model_aux.model_value !Globals.model_params) Model_aux.model_assoc = "flowing");
  check_radio "model_pop"
              (List.assoc (Model_aux.model_value !Globals.model_params) Model_aux.model_assoc = "pop");
  check_radio "model_flat"
              (List.assoc (Model_aux.model_value !Globals.model_params) Model_aux.model_assoc = "flat");
  check_radio "model_tso"
              (List.assoc (Model_aux.model_value !Globals.model_params) Model_aux.model_assoc = "tso");
  check_radio "model_promising"
              (List.assoc (Model_aux.model_value !Globals.model_params) Model_aux.model_assoc = "promising");

  Js.Unsafe.fun_call (Js.Unsafe.js_expr "show_hide_select_topology") [||] |> ignore;
  Js.Unsafe.fun_call (Js.Unsafe.js_expr "show_hide_restrict_promises") [||] |> ignore;

  check_radio "allow_tree_speculation"
              (!Globals.model_params).t.thread_allow_tree_speculation;
  check_radio "forbid_tree_speculation"
              (not (!Globals.model_params).t.thread_allow_tree_speculation);

  check_radio "promiseFirst"
              (!Globals.model_params).t.p_promise_first;
  check_radio "promiseAnytime"
              (not (!Globals.model_params).t.p_promise_first;);

  check_radio "force_sc_true"
              ((!Globals.model_params).t.thread_restriction = RestrictionSC && (!Globals.model_params).ss.ss_sc);
  check_radio "force_sc_false"
              (not ((!Globals.model_params).t.thread_restriction = RestrictionSC && (!Globals.model_params).ss.ss_sc));

  check_checkbox "relaxed_fetch" (!Globals.model_params.ss.ss_fetch_type <> Fetch_Atomic);

  check_radio "sequential_fetch_true"
              (is_fetch_sequential (!Globals.model_params).t);
  check_radio "sequential_fetch_false"
              (not (is_fetch_sequential (!Globals.model_params).t));

  check_checkbox "fetch_flat_idc"
              (is_flat_idc_1 (!Globals.model_params));
  check_checkbox "fetch_flat_dic"
              (is_flat_dic_1 (!Globals.model_params));

  Js.Unsafe.fun_call (Js.Unsafe.js_expr "set_model") [|Js.Unsafe.inject (Js.string (List.assoc (Model_aux.model_value !Globals.model_params) Model_aux.model_assoc))|] |> ignore

let options_to_html run_options : unit =
  model_to_html ();

  assign_number "elf_threads" !Globals.elf_threads;

  check_checkbox "use_dwarf" !Globals.use_dwarf;

  check_radio "semantics_shallow_embedding" (not run_options.RunOptions.interpreter);
  check_radio "semantics_interpreter"       (    run_options.RunOptions.interpreter);

  List.iter
    (fun top_string ->
      check_radio ("topology_2_" ^ top_string) (!Globals.topology_2 = top_string))
    (Model_aux.ui_topologies 2);
  List.iter
    (fun top_string ->
      check_radio ("topology_3_" ^ top_string) (!Globals.topology_3 = top_string))
    (Model_aux.ui_topologies 3);
  List.iter
    (fun top_string ->
      check_radio ("topology_4_" ^ top_string) (!Globals.topology_4 = top_string))
    (Model_aux.ui_topologies 4);
;;

let options_of_html : unit -> RunOptions.t = fun () ->
  let run_options = default_options () in

  if is_checked_radio "model_pldi11" then
    Globals.model_params := Model_aux.parse_and_update_model "pldi11" !Globals.model_params;
  if is_checked_radio "model_flowing" then
    Globals.model_params := Model_aux.parse_and_update_model "flowing" !Globals.model_params;
  if is_checked_radio "model_pop" then
    Globals.model_params := Model_aux.parse_and_update_model "pop" !Globals.model_params;
  if is_checked_radio "model_flat" then
    Globals.model_params := Model_aux.parse_and_update_model "flat" !Globals.model_params;
  if is_checked_radio "model_tso" then
    Globals.model_params := Model_aux.parse_and_update_model "tso" !Globals.model_params;
  if is_checked_radio "model_promising" then
    Globals.model_params := Model_aux.parse_and_update_model "promising" !Globals.model_params;

  if is_checked_radio "allow_tree_speculation" then
    Globals.model_params := Model_aux.parse_and_update_model "allow_tree_speculation" !Globals.model_params
  else (*if is_checked_radio "forbid_tree_speculation" then*)
    Globals.model_params := Model_aux.parse_and_update_model "forbid_tree_speculation" !Globals.model_params;

  if is_checked_radio "promiseFirst" then
    Globals.model_params := Model_aux.parse_and_update_model "promise_first" !Globals.model_params
  else (*if is_checked_radio "forbid_tree_speculation" then*)
    Globals.model_params := Model_aux.parse_and_update_model "promise_anytime" !Globals.model_params;

  if is_checked_radio "force_sc_true" then
    Globals.model_params := Model_aux.parse_and_update_model "sc" !Globals.model_params
  else (*if is_checked_radio "force_sc_false" then*)
    Globals.model_params := Model_aux.parse_and_update_model "not-restricted" !Globals.model_params;

  if is_checked_checkbox "relaxed_fetch" then
    Globals.model_params := Model_aux.parse_and_update_model "fetch-relaxed" !Globals.model_params
  else
    Globals.model_params := Model_aux.parse_and_update_model "fetch-atomic" !Globals.model_params;

  let mp = !Globals.model_params  in
  if is_checked_radio "sequential_fetch_true" then
    Globals.model_params := { mp with t = { mp.t with thread_fetch_order =
        Fetch_Sequential; }}
  else
    Globals.model_params := { mp with t = { mp.t with thread_fetch_order =
        Fetch_Unrestricted; }};

  if is_checked_checkbox "fetch_flat_idc" then
    Globals.model_params := update_model_params_flat_idc !Globals.model_params true
  else
    Globals.model_params := update_model_params_flat_idc !Globals.model_params false;

  if is_checked_checkbox "fetch_flat_dic" then
    Globals.model_params := update_model_params_flat_dic !Globals.model_params true
  else
    Globals.model_params := update_model_params_flat_dic !Globals.model_params false;

  Globals.elf_threads := read_number "elf_threads";

  Globals.use_dwarf := is_checked_checkbox "use_dwarf";

  List.iter
    (fun top_string ->
      if is_checked_radio ("topology_2_" ^ top_string) then
        Globals.topology_2 := top_string;)
    (Model_aux.ui_topologies 2);
  List.iter
    (fun top_string ->
      if is_checked_radio ("topology_3_" ^ top_string) then
        Globals.topology_3 := top_string;)
    (Model_aux.ui_topologies 3);
  List.iter
    (fun top_string ->
      if is_checked_radio ("topology_4_" ^ top_string) then
        Globals.topology_4 := top_string;)
    (Model_aux.ui_topologies 4);

  { run_options with
    RunOptions.interpreter = (not (is_checked_radio "semantics_shallow_embedding"))
  }
;;

let isa_callback (isa: Isa.isa_model) =
  begin match isa with
  | PPC ->
     Globals.model_params := Model_aux.parse_and_update_model "pldi11" !Globals.model_params;
     Js.Unsafe.fun_call (Js.Unsafe.js_expr "set_isa") [|Js.Unsafe.inject (js "PPC")|] |> ignore
  | MIPS ->
      begin match (!Globals.model_params).ss.ss_model with
      | PLDI11_storage_model
      | TSO_storage_model
      | Promising_storage_model ->
          Globals.model_params := Model_aux.parse_and_update_model "flat" !Globals.model_params
      | Flowing_storage_model
      | Flat_storage_model
      | POP_storage_model -> ()
      end;
      Js.Unsafe.fun_call (Js.Unsafe.js_expr "set_isa") [|Js.Unsafe.inject (js "MIPS")|] |> ignore
  | RISCV ->
      begin match (!Globals.model_params).ss.ss_model with
      | PLDI11_storage_model
      | Flowing_storage_model
      | POP_storage_model
      | Promising_storage_model ->
          Globals.model_params := Model_aux.parse_and_update_model "flat" !Globals.model_params
      | Flat_storage_model
      | TSO_storage_model -> ()
      end;
      Js.Unsafe.fun_call (Js.Unsafe.js_expr "set_isa") [|Js.Unsafe.inject (js "RISCV")|] |> ignore
  | AARCH64 _ ->
      begin match (!Globals.model_params).ss.ss_model with
      | PLDI11_storage_model
      | TSO_storage_model ->
          Globals.model_params := Model_aux.parse_and_update_model "flat" !Globals.model_params
      | Flowing_storage_model
      | POP_storage_model
      | Flat_storage_model
      | Promising_storage_model -> ()
      end;
      Js.Unsafe.fun_call (Js.Unsafe.js_expr "set_isa") [|Js.Unsafe.inject (js "AArch64")|] |> ignore
  | X86 ->
      begin match (!Globals.model_params).ss.ss_model with
      | PLDI11_storage_model
      | Flowing_storage_model
      | POP_storage_model
      | Flat_storage_model
      | Promising_storage_model ->
          Globals.model_params := Model_aux.parse_and_update_model "tso" !Globals.model_params
      | TSO_storage_model -> ()
      end;
      Js.Unsafe.fun_call (Js.Unsafe.js_expr "set_isa") [|Js.Unsafe.inject (js "X86")|] |> ignore
  end;
  model_to_html ()

let error_dialog (msg: string) : unit =
  Js.Unsafe.fun_call (Js.Unsafe.js_expr "error_dialog") [|
    Js.Unsafe.inject (Js.string msg)
  |]
  |> ignore

let start_interactive_litmus (name: string) (litmus_str: string) : unit =
  try
    Top.from_litmus_data (options_of_html ()) name litmus_str (Some isa_callback)
  with
  | Misc.UserError s -> error_dialog s
  | Misc.Fatal     s -> error_dialog s
;;

let start_interactive_elf () : unit =
  match !elf_file_name with
  | None ->
      (* This should never happen *)
      error_dialog "elf_file_name is not set"
  | Some file ->
    begin try
      Top.from_ELF_data (options_of_html ()) file !elf_data (Some isa_callback)
    with
    | Misc.Fatal msg -> error_dialog msg
    end
;;

let load : unit -> unit  = fun () ->
  let default_run_options = default_options () in

  let topos_div = getElementById "topos_two_threads" in
  List.iter
    (fun top_string ->
      Dom.appendChild topos_div (topo_radio "topology_2" top_string))
    (Model_aux.ui_topologies 2);

  let topos_div = getElementById "topos_three_threads" in
  List.iter
    (fun top_string ->
      Dom.appendChild topos_div (topo_radio "topology_3" top_string))
    (Model_aux.ui_topologies 3);

  let topos_div = getElementById "topos_four_threads" in
  List.iter
    (fun top_string ->
      Dom.appendChild topos_div (topo_radio "topology_4" top_string))
    (Model_aux.ui_topologies 4);

  options_to_html default_run_options
;;

(* export functions to JS *)
let () =
  Js.export "webppc_lib" (object%js
    (* These fields all have trailing underscores because of
    js_of_ocaml's overloading system *)
    method load_ = load ()
    method set_elf_data_ file_name array_buffer length = set_elf_data (Js.to_string file_name) array_buffer length
    method unset_elf_data_ = elf_file_name := None; elf_data := []
    method elf_file_name_ =
      match !elf_file_name with
      | Some s -> Js.some (Js.string s)
      | None   -> Js.null

    method start_interactive_litmus_ name litmus_str = start_interactive_litmus (Js.to_string name) (Js.to_string litmus_str)
    method start_interactive_elf_ = start_interactive_elf ()

    method reset_options_    = options_to_html (default_options ())
    method get_version_      = Js.string Versions.Rmem.describe
    method get_last_changed_ = Js.string Versions.Rmem.last_changed
  end)
