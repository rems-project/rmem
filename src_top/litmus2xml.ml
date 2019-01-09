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

let output_suffix : string ref = ref ".xml";;

let output_file : (string option) ref = ref None;;

let had_errors = ref false;;

let rec mkdirp (dir: string) : unit =
  try Unix.mkdir dir 0o777 with
  | Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  | Unix.Unix_error (Unix.ENOENT, _, _) ->
      mkdirp (Filename.dirname dir);
      Unix.mkdir dir 0o777
;;

let litmus_to_xml (file: string) (output_f: string -> string) : unit =
  let to_xml out_channel =
    Litmus_test_file.to_xml file |> Printf.fprintf out_channel "%s"
  in

  try
    begin match !output_file with
    | Some f ->
        let output_file = output_f f in
        mkdirp (Filename.dirname output_file);
        Misc.output_protect to_xml output_file
    | None   -> to_xml stdout
    end
  with
  | Misc.UserError s -> Printf.eprintf "Error in test %s: %s\n%!" file s; had_errors := true
  | Misc.Fatal msg   -> Printf.eprintf "Fatal error: %s\n" msg; exit 1
  | Misc.Exit        -> had_errors := true
;;

let at_file_to_xml (file: string) : unit =
  let dir = Filename.dirname file in
  let base = Filename.basename file in

  let work_dir = Sys.getcwd () in
  Sys.chdir dir;
  let files = Misc.fold_argv (fun x xs -> x :: xs) [base] [] in
  Sys.chdir work_dir;

  List.iter
    begin fun file ->
        let input_file = Filename.concat dir file in
        litmus_to_xml input_file
          (fun dir ->
              Filename.chop_suffix file ".litmus" ^ !output_suffix
              |> Filename.concat dir)
    end
    files
;;

let main : unit -> unit = fun () ->
  let usage =
    "Usage: litmus2xml [options] infile...\n" ^
    "       litmus2xml --help   to show options\n"
  in
  let help out_channel msg = Printf.fprintf out_channel "%s\n" msg in

  let opts =
    [ ("-o",
          Arg.String (fun s -> output_file := Some s),
          "<file/dir>");
      ("--output",
          Arg.String (fun s -> output_file := Some s),
          "<file/dir> If infile is a single .litmus file, place the output into <file>,\n" ^
          "                      otherwise place the output files in the directory <dir>,\n" ^
          "                      preserving directory structure of @ files (default: print on\n" ^
          "                      the standard output).");

      ("-s",
          Arg.Set_string output_suffix,
          "<suffix>");
      ("--suffix",
          Arg.Set_string output_suffix,
          "<suffix> When the '-o' option is used, and infile is an @ file or more than\n" ^
          "                      one infile is given, the extension of each output file is\n" ^
          "                      replaced with <suffix> (default: .xml).");

      ("--stdout",
          Arg.Unit (fun () -> output_file := None),
          " Print output on the standard output (overrides '-o').");
    ]
  in

  let files = ref [] in
  let collect_file s = files := s :: !files in

  begin try Arg.parse_argv Sys.argv (Arg.align opts) collect_file (usage ^ "Options:") with
  | Arg.Bad msg  -> help stderr msg; exit 1
  | Arg.Help msg -> help stdout msg; exit 0
  end;

  begin match !files with
  | [] -> help stderr usage; exit 1

  | file :: [] ->
      if Misc.is_list file then at_file_to_xml file
      else litmus_to_xml file (fun f -> f)

  | files ->
      List.iter
        begin fun file ->
          if Misc.is_list file then at_file_to_xml file
          else
            litmus_to_xml file
              (fun dir ->
                  Filename.chop_suffix (Filename.basename file) ".litmus" ^ !output_suffix
                  |> Filename.concat dir)
        end
        files
  end;

  if !had_errors then exit 1
  else exit 0
;;

main ()
