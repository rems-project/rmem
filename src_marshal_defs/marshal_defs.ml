(*===============================================================================*)
(*                                                                               *)
(*                rmem executable model                                          *)
(*                =====================                                          *)
(*                                                                               *)
(*  This file is:                                                                *)
(*                                                                               *)
(*  Copyright Shaked Flur, University of Cambridge               2017            *)
(*  Copyright Jon French, University of Cambridge           2017-2018            *)
(*  Copyright Robert Norton-Wright, University of Cambridge      2017            *)
(*  Copyright Christopher Pulte, University of Cambridge         2018            *)
(*                                                                               *)
(*  All rights reserved.                                                         *)
(*                                                                               *)
(*  The rmem tool is distributed under the 2-clause BSD license in LICENCE.txt.  *)
(*  For author information see README.md.                                        *)
(*                                                                               *)
(*===============================================================================*)

let marshal_isa (defs : Interp_interface.specification) filename =
  let f = open_out_bin filename in
  Marshal.to_string defs [Marshal.No_sharing; Marshal.Compat_32]
  |> B64.encode
  |> output_string f;
  close_out f

let opts = [
  ("-PPCGen",     Arg.String (marshal_isa Power.defs),    "<filename>");
  ("-AArch64",    Arg.String (marshal_isa ArmV8.defs),    "<filename>");
(*   ("-AArch64Gen", Arg.String (marshal_isa ArmV8Gen.defs), "<filename>"); *)
  ("-MIPS64",     Arg.String (marshal_isa Mips.defs),     "<filename>");
  ("-RISCV",      Arg.String (marshal_isa (Interp_ast.Defs []) (* Riscv.defs *)),    "<filename>");
  ("-X86",        Arg.String (marshal_isa X86.defs),      "<filename>");
]

let usage = "marshal_defs: give an option to marshal the defs for that ISA to a file"

let anon_fun arg = raise (Arg.Bad (Printf.sprintf "unknown argument '%s'" arg))

let main () =
  if Array.length Sys.argv <= 1 then begin
    prerr_string (Arg.usage_string opts usage);
    exit 1
  end;

  try Arg.parse_argv Sys.argv (Arg.align opts) anon_fun usage with
  | Arg.Bad msg  -> prerr_string msg; exit 1
  | Arg.Help msg -> print_string msg; exit 0

let _ = main ()
