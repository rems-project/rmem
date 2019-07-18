(*===============================================================================*)
(*                                                                               *)
(*                rmem executable model                                          *)
(*                =====================                                          *)
(*                                                                               *)
(*  This file is:                                                                *)
(*                                                                               *)
(*  Copyright Shaked Flur, University of Cambridge       2017-2018               *)
(*  Copyright Jon French, University of Cambridge        2017-2018               *)
(*  Copyright Christopher Pulte, University of Cambridge      2017               *)
(*                                                                               *)
(*  All rights reserved.                                                         *)
(*                                                                               *)
(*  It is part of the rmem tool, distributed under the 2-clause BSD licence in   *)
(*  LICENCE.txt.                                                                 *)
(*                                                                               *)
(*===============================================================================*)

open Js_of_ocaml

module Make (ConcModel: Concurrency_model.S)
    : (GraphBackend.S with type ui_trans = ConcModel.ui_trans)
  = struct
  type ui_trans = ConcModel.ui_trans
  module Base = Graphviz_base.Make(ConcModel)

  let render_dot ppmode legend_opt cex ioid_trans_lookup = fun layout_dot ->
    (* Pp.split doesn't like this for some reason --
      I believe because of its 'skip beginning and end' behaviour *)
    let lines = (Js.string layout_dot)##split (Js.string "\n") |>
                  Js.str_array |> Js.to_array |> Array.map Js.to_string |> Array.to_list in
    let positions = Base.parse_dot_positions lines in
    let dot = Base.pp_html_candidate_execution ppmode legend_opt true positions cex ioid_trans_lookup in
    Js.Unsafe.fun_call (Js.Unsafe.js_expr "display_dot")
                      [|Js.Unsafe.inject (Js.string dot)|]
    |> ignore


  let make_graph ppmode test_info cex (nc: ui_trans list) =
    let ppmode =
      { ppmode with
        Globals.pp_kind = Globals.Ascii;
        Globals.pp_colours = false;
        Globals.pp_trans_prefix = false;
      }
    in
    let legend_opt = Some test_info.Test.name in
    let ioid_trans_lookup = Base.ioid_trans_lookup nc in
    let layout_dot =
      Base.pp_html_candidate_execution ppmode legend_opt false [] cex ioid_trans_lookup
    in
    Js.Unsafe.fun_call (Js.Unsafe.js_expr "layout_dot")
                      [|
                        Js.Unsafe.inject (Js.string layout_dot);
                        Js.Unsafe.inject (Js.wrap_callback
                                            (render_dot ppmode legend_opt cex ioid_trans_lookup))
                      |]
    |> ignore
end (* Make *)
