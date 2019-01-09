(*===============================================================================*)
(*                                                                               *)
(*                rmem executable model                                          *)
(*                =====================                                          *)
(*                                                                               *)
(*  This file is:                                                                *)
(*                                                                               *)
(*  Copyright Shaked Flur, University of Cambridge    2015, 2017                 *)
(*  Copyright Susmit Sarkar, University of St Andrews       2014                 *)
(*  Copyright Peter Sewell, University of Cambridge         2014                 *)
(*                                                                               *)
(*  All rights reserved.                                                         *)
(*                                                                               *)
(*  The rmem tool is distributed under the 2-clause BSD license in LICENCE.txt.  *)
(*  For author information see README.md.                                        *)
(*                                                                               *)
(*===============================================================================*)

open Ocamlbuild_plugin

(* pdep ["ocaml"; "compile"] "ocamlcdep" (fun param -> [param]); *)

let () = dispatch begin function
  | Before_rules ->
      rule "pp2ml: .{ml|mly|mll|notstub}.include -> .{ml|mly|mll|notstub}"
        ~prod:"%.%(sfx:<ml> or <mly> or <mll> or <notstub>)"
        ~dep:"%.%(sfx).include"
        ~doc:"call pp2ml to handle the magic comment '#include \"<file>\"'; \\
              where the path of <file> is relative to the project root (do \\
              not prefix it with ./)"
        begin fun env build ->
          let includes =
            let r = Str.regexp ".*#include[ \t]*\"\\([^\"]*\\)\".*" in

            with_input_file (env "%.%(sfx).include") (fun inchan ->
              let files = ref [] in
              begin try
                while true do
                  let line = input_line inchan in
                  if Str.string_match r line 0 then begin
                    let file = Str.replace_first r "\\1" line in
                    files := file :: !files
                  end;
                  (* if you don't want to use Str, you can use this:
                  try
                    Scanf.sscanf line "%_[^#]#include%_[ ]\"%[^\"]\"%_s"
                      (fun file -> files := file :: !files)
                  with
                  | Scanf.Scan_failure _ -> ()
                  *)
                done
              with
              | End_of_file -> ()
              end;
              !files
            )
            |> List.map (fun file -> [file])
          in
          build includes |> List.iter (function
            | Outcome.Good _ -> ()
            | Outcome.Bad e  -> () (* ignore the missing file (pp2ml will report it) *)
            );
          Cmd (S [ A "../pp2ml.native"; P (env "%.%(sfx).include"); Sh ">"; Px (env "%.%(sfx)")])
        end;

      rule "stub: .ml.{notstub|stub} -> .ml"
        ~prod: "%.ml"
        ~doc:"if <file>.ml is in the environment variable RMEMSTUBS copy <file>.ml.stub \\
              to <file>.ml, otherwise copy <file>.ml.notstub to <file>.ml"
        begin fun env build ->
          let ml_file = env "%.ml" in
          let stub_choice_file =
            if getenv ~default:"" "RMEMSTUBS" |> String.trim |> Str.split (Str.regexp "[ \t]+") |> List.mem ml_file
            (* if you don't want to use Str, you can use this:
            let split s =
              let s = ref s in
              let strs = ref [] in
              while !s <> "" do
                Scanf.sscanf !s "%s %[^;]" (fun a b -> strs := a :: !strs; s := b)
              done;
              !strs
            in
            if getenv ~default:"" "RMEMSTUBS" |> String.trim |> split |> List.mem ml_file
            *)
            then env "%.ml.stub"
            else env "%.ml.notstub"
          in
          [[stub_choice_file]] |> build |> List.iter Outcome.ignore_good;
          cp stub_choice_file (env "%.ml")
        end;

  | After_rules ->
      ocaml_lib ~extern:true ~dir:"../src_elf_libraries/batteries/_build/src" "batteries"

  | _ -> ()
end
