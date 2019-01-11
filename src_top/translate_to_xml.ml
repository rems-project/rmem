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

open Printf

module Make (A: Arch.S) = struct
  type xml_att = string * string

  let att_to_string (name, value) = sprintf "%s=\"%s\"" name value

  type xml_tag =
    { name: string;
      atts: xml_att list;
    }

  let tag_to_string tag =
    tag.name ^
    begin match tag.atts with
    | [] -> ""
    | _ ->
        " " ^ (String.concat " " (List.map att_to_string tag.atts))
    end

  let empty_tag_to_string tag = sprintf "<%s/>" (tag_to_string tag)
  let start_tag_to_string tag = sprintf "<%s>"  (tag_to_string tag)
  let end_tag_to_string   tag = sprintf "</%s>" tag.name

  type xml_node =
    | XML_String of string
    | XML_Element of xml_tag * (xml_node list)
    | XML_None

  let line n s = sprintf "%s%s\n" (String.make (2 * n) ' ') s

  let rec xml_to_string (n: int) (nodes: xml_node list) : string =
    List.map (xml_node_to_string n) nodes |> String.concat ""
  and xml_node_to_string (n: int) : xml_node -> string = begin function
    | XML_String s             -> line n s
    | XML_Element (tag, nodes) ->
        begin match xml_to_string (n + 1) nodes with
        | ""   -> empty_tag_to_string tag |> line n
        | body ->
            (start_tag_to_string tag |> line n) ^
            body ^
            (end_tag_to_string tag |> line n)
        end
    | XML_None -> ""
  end

  let pp_run_type = begin function
    | MiscParser.TyDef           -> "int"
    | MiscParser.TyDefPointer    -> "ptr"
    | MiscParser.Ty s            -> s
    | MiscParser.Atomic s        -> assert false
    | MiscParser.Pointer _       -> "ptr"
    | MiscParser.TyArray (s, sz) -> sprintf "%s[%i]" s sz
    end

  let loc_to_xml loc atts =
    begin match loc with
    | MiscParser.Location_reg (t, reg) ->
        let atts =
          [ ("thread", sprintf "%d" t);
            ("id",     reg);
          ] @ atts
        in
        XML_Element ({name = "reg"; atts = atts}, [])

    | MiscParser.Location_sreg str ->
        failwith ("symbolic registers (" ^ str ^ ") are not supported")

    | MiscParser.Location_global name ->
        let atts = [ ("id",  SymbConstant.pp_v name)] @ atts in
        XML_Element ({name ="loc"; atts = atts}, [])
    end

  let val_to_xml maybev =
      let atts = [("value", SymbConstant.pp false maybev)] in
      XML_Element ({name = "const"; atts = atts}, [])

  let rec prop_to_xml = begin function
    | ConstrGen.Atom (ConstrGen.LV  (location, maybev)) ->
        XML_Element ({name = "eq"; atts = []}, [loc_to_xml location []; val_to_xml maybev])
    | ConstrGen.Atom (ConstrGen.LL  (l_loc, r_loc)) ->
        XML_Element ({name = "eq"; atts = []}, [loc_to_xml l_loc []; loc_to_xml r_loc []])
    | ConstrGen.Not prop ->
        XML_Element ({name = "not"; atts = []}, [prop_to_xml prop])
    | ConstrGen.And props ->
        XML_Element ({name = "and"; atts = []}, List.map prop_to_xml props)
    | ConstrGen.Or props ->
        XML_Element ({name = "or";  atts = []}, List.map prop_to_xml props)
    | ConstrGen.Implies (l_prop, r_prop) ->
        XML_Element ({name = "implies"; atts = []}, [prop_to_xml l_prop; prop_to_xml r_prop])
    end



  let translate_test test_splitted test : string =
    (* <header arch="RISCV" name="MP">..<header/> *)
    let header =
      let info =
        List.map
          begin fun (l_str, r_str) ->
            XML_Element ({name = "info"; atts = [("type", l_str)]}, [XML_String r_str])
          end
          test.MiscParser.info
      in
      let atts =
        [ ("arch", Archs.pp test_splitted.Splitter.arch);
          ("name", test_splitted.Splitter.name.Name.name);
        ]
      in
      XML_Element ({name = "header"; atts = atts}, info)
    in

    (* <initial>...</initial> *)
    let initial =
      let (locs, regs) =
        List.fold_left
          begin fun (locs, regs) (loc, (run_type, maybev)) ->
            let atts =
              [ ("type",  pp_run_type run_type);
                ("value", SymbConstant.pp false maybev);
              ]
            in
            match loc with
            | MiscParser.Location_reg _
            | MiscParser.Location_sreg _ ->
                (* <reg thread="0" type="int" id="x6" value="x"/> *)
                let xml = loc_to_xml loc atts in
                (locs, xml :: regs)
            | MiscParser.Location_global _ ->
                (* <loc id="x" type="int" value="0"/> *)
                let xml = loc_to_xml loc atts in
                (xml :: locs, regs)
          end
          ([], [])
          test.MiscParser.init
      in

      (* <memory>...</memory> *)
      let memory =
        if locs = [] then XML_None
        else XML_Element ({name ="memory"; atts = []}, List.rev locs)
      in

      (* <registers>...</registers> *)
      let regs =
        if regs = [] then XML_None
        else  XML_Element ({name = "registers"; atts = []}, List.rev regs)
      in

      if memory = XML_None && regs = XML_None then XML_None
      else XML_Element ({name ="initial"; atts = []}, [memory; regs])
    in

    (* <code>...</code> *)
    let code =
      let threads =
        List.fold_left
          begin fun threads (t, ps) ->
            let rec compact = function
              | [] -> []
              | A.Nop :: ps -> compact ps
              | A.Label (l, p) :: ps ->
                  begin match compact (p :: ps) with
                  | [] -> [A.Label (l, A.Nop)]
                  | p :: ps -> A.Label (l, p) :: ps
                  end
              | p :: ps -> p :: compact ps
            in

            (* <thread id="0">...</thread> *)
            let rec pseudo_inst_to_xml = function
              | A.Nop           -> [XML_Element ({name = "op_nop"; atts = []}, [])]
              | A.Label (l, p)  ->
                  XML_Element ({name = "label"; atts = [("id", l)]}, []) :: pseudo_inst_to_xml p
              | A.Instruction i -> [XML_String (A.pp_instruction PPMode.Xml i)]
              | A.Macro _       -> failwith "Cannot translate Macro!"
              | A.Symbolic _    -> failwith "Cannot translate Symbolic!"
            in
            let ops = compact ps |> List.map pseudo_inst_to_xml |> List.concat in
            XML_Element ({name = "thread"; atts = [("id", sprintf "%d" t)]}, ops) :: threads
          end
          []
          test.MiscParser.prog
        |> List.rev
      in
      XML_Element ({name = "code"; atts = []}, threads)
    in

    let filter =
      match test.MiscParser.filter with
      | Some prop ->
          XML_Element ({name = "filter"; atts = []}, [prop_to_xml prop])
      | None -> XML_None
    in

    (* <condition type="...">...<condition/> *)
    let condition =
      match test.MiscParser.condition with
      | ConstrGen.ForallStates prop ->
          XML_Element ({name = "condition"; atts = [("type", "forall")]}, [prop_to_xml prop])
      | ConstrGen.ExistsState prop ->
          XML_Element ({name = "condition"; atts = [("type", "exists")]}, [prop_to_xml prop])
      | ConstrGen.NotExistsState prop ->
          XML_Element ({name = "condition"; atts = [("type", "notexists")]}, [prop_to_xml prop])
    in

    let locations =
      let locs =
        List.map
          (fun (loc, run_type) -> loc_to_xml loc [("type", pp_run_type run_type)])
          test.MiscParser.locations
      in
      if locs = [] then XML_None
      else XML_Element ({name = "locations"; atts = []}, locs)
    in

    (* TODO: test.MiscParser.extra_data : extra_data *)

    [ header;
      initial;
      code;
      filter;
      condition;
      locations;
    ] |> xml_to_string 0

end
