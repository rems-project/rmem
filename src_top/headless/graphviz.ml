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


let mycommand ppmode command input_file output_file =
  let error_file = output_file ^ ".err" in
  let command_line = (command ^ " " ^ input_file ^ " -o " ^ output_file ^ " 2> " ^ error_file) in
  let exitcode = Sys.command command_line in
  if exitcode = 0 then
    ignore @@ Sys.command ("rm " ^ error_file)
  else
    begin
      let err = open_in error_file in
      let err_lines : string list ref = ref [] in
      try while true do
        err_lines := input_line err :: !err_lines
      done with
      | End_of_file ->
          close_in err;
          Screen_base.otConcat @@
            Screen_base.otStrLine "Error executing:" ::
            Screen_base.otStrLine "%s" command_line ::
            Screen_base.OTLine Screen_base.OTEmpty ::
            (List.map (Screen_base.otStrLine "%s") !err_lines |> List.rev)
          |> Screen.show_warning ppmode;
          exit exitcode;
    end


module Make (ConcModel: Concurrency_model.S)
    : (GraphBackend.S with type ui_trans = ConcModel.ui_trans)
  = struct
  type ui_trans = ConcModel.ui_trans
  module Base = Graphviz_base.Make(ConcModel)

  let make_dot_graph ppmode legend_opt cex basename_in_dir ioid_trans_lookup =
    let dot_ppmode =
      { ppmode with
        Globals.pp_kind = Globals.Ascii;
        Globals.pp_colours = false;
        Globals.pp_trans_prefix = false
      }
    in
    let layout = basename_in_dir ^ ".layout.tmp" in
    let graph = basename_in_dir in

    (* first render graph without any edges, to get the positions  *)
    (*     let c = open_out_gen [Open_creat; ] 0o644 "out.dot" in*)
    let c = open_out (layout ^ ".dot") in
    (* Open_append *)
    Base.pp_html_candidate_execution dot_ppmode legend_opt false [] cex ioid_trans_lookup
    |> Printf.fprintf c "%s";
    close_out c;

    mycommand ppmode ("dot -Tplain") (layout ^ ".dot") (layout ^ ".txt");
    mycommand ppmode ("dot -Tpdf")   (layout ^ ".dot") (layout ^ ".pdf");

    (* now pull out the positions from the graphviz output *)
    let c = open_in (layout ^ ".txt") in
    let lines  : string  list =
      let rec f acc =
        try
          let s = input_line c in
          f (s :: acc)
        with
          End_of_file -> close_in c; List.rev acc in
      f [] in
    let positions = Base.parse_dot_positions lines in

    (* now re-render the graph with those node positions *)
    let c = open_out (graph ^ ".dot") in
    Base.pp_html_candidate_execution dot_ppmode legend_opt true positions cex ioid_trans_lookup
    |> Printf.fprintf c "%s";
    let _ = close_out c in

    mycommand ppmode ("neato -Tpdf") (graph ^ ".dot") (graph ^ ".pdf");
    mycommand ppmode ("neato -Tfig") (graph ^ ".dot") (graph ^ ".fig");

    (* TODO: make filename configurable and more DRY *)
    Screen_base.otStrLine "wrote %s.pdf" graph
    |> Screen.show_message ppmode


  let make_graph ppmode test_info cex (nc: ui_trans list) =
    (* if generated_dir is set, create files there with filename based on the test name, otherwise create files here based on "out" *)
    let basename_in_dir = match !Globals.generateddir with None -> "out" | Some dir -> Filename.concat dir (Filename.basename test_info.Test.name) in

    make_dot_graph ppmode (Some test_info.Test.name) cex basename_in_dir (Base.ioid_trans_lookup nc);

    (* hack to terminate after finding the first "final" graph *)
    begin match !Globals.run_dot with
    | Some RD_final
    | Some RD_final_ok
    | Some RD_final_not_ok
      -> exit 0
    | None
    | Some RD_step
      -> ()
    end
end (* Make *)
