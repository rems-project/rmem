(*==================================================================================================*)
(*                                                                                                  *)
(*                rmem executable model                                                             *)
(*                =====================                                                             *)
(*                                                                                                  *)
(*  This file is:                                                                                   *)
(*                                                                                                  *)
(*  Copyright Peter Sewell, University of Cambridge                          2011-2012, 2014-2017   *)
(*  Copyright Shaked Flur, University of Cambridge                                      2014-2018   *)
(*  Copyright Susmit Sarkar, University of St Andrews                                   2011-2015   *)
(*  Copyright Jon French, University of Cambridge                                       2017-2018   *)
(*  Copyright Christopher Pulte, University of Cambridge                                2015-2016   *)
(*  Copyright Luc Maranget, INRIA, Paris, France                                        2011-2012   *)
(*  Copyright Francesco Zappa Nardelli, INRIA, Paris, France                                 2011   *)
(*  Copyright Robert Norton-Wright, University of Cambridge                             2016-2017   *)
(*  Copyright Ohad Kammar, University of Cambridge (when this work was done)                 2013   *)
(*  Copyright Pankaj Pawan, IIT Kanpur and INRIA (when this work was done)                   2011   *)
(*                                                                                                  *)
(*  All rights reserved.                                                                            *)
(*                                                                                                  *)
(*  It is part of the rmem tool, distributed under the 2-clause BSD licence in                      *)
(*  LICENCE.txt.                                                                                    *)
(*                                                                                                  *)
(*==================================================================================================*)

(* let get_cands = ref false *)
(* let optoax = ref false *)
(* let axtoop = ref false *)
(* let minimal = ref false *)
(* let smt = ref false *)
(* let solver = ref "MMExplorer2" *)
(* let candidates = ref None *)


let use_new_run = ref false;

(** output options **************************************************)

type verbosity_level =
  | Quiet                  (* -q, minimal output and things important for herd-tools *)
  | Normal                 (* default, things normal users would like to see *)
  | ThrottledInformation   (* -v, normal mode for informed users, no more than one line
                           every 5 seconds or so *)
  | UnthrottledInformation (* -v -v, even more information, might render the output unusable *)
  | Debug                  (* -debug, cryptic information *)

let verbosity_levels = [
 Quiet                  ;
 Normal                 ;
 ThrottledInformation   ;
 UnthrottledInformation ;
 Debug                  ]

let pp_verbosity_level v =
  match v with
  | Quiet                  -> "Quiet"
  | Normal                 -> "Normal"
  | ThrottledInformation   -> "ThrottledInformation"
  | UnthrottledInformation -> "UnthrottledInformation"
  | Debug                  -> "Debug"



let verbosity = ref Normal

let increment_verbosity = fun () ->
  verbosity :=
    begin match !verbosity with
    | Quiet                  -> Normal
    | Normal                 -> ThrottledInformation
    | ThrottledInformation   -> UnthrottledInformation
    | UnthrottledInformation -> Debug
    | Debug                  -> Debug
    end

let is_verbosity_at_least (verb: verbosity_level) : bool =
  let int_of_verbosity_level v =
    match v with
    | Quiet                  -> 0
    | Normal                 -> 1
    | ThrottledInformation   -> 2
    | UnthrottledInformation -> 3
    | Debug                  -> 4
  in
  int_of_verbosity_level !verbosity >= int_of_verbosity_level verb

let logdir = ref None

let dont_tool = ref false (* "Dont" output *)

let debug_sail_interp = ref false

let deterministic_output = ref false

(** model options ***************************************************)

(* model_params should probably not be changed after the initial state
was created *)
let model_params = ref MachineDefSystem.default_model_params

let big_endian = ref None
(*let big_endian = fun () -> false
  let open MachineDefTypes in
  begin match !model_params.t.thread_isa_info.ism with
  | PPCGEN_ism    -> true
  | AARCH64_ism _ -> false
  | MIPS_ism      -> true
  end*)

(* BE CAREFUL: call big_endian only after thread_ism (of model_params)
has been set properly (i.e. set_model_ism was called) *)
let get_endianness = fun () ->
  begin match !big_endian with
  | None ->
      let open MachineDefTypes in
      begin match !model_params.t.thread_isa_info.ism with
      | PPCGEN_ism    -> Sail_impl_base.E_big_endian
      | AARCH64_ism _ -> Sail_impl_base.E_little_endian
      | MIPS_ism      -> Sail_impl_base.E_big_endian
      | RISCV_ism      -> Sail_impl_base.E_little_endian
      | X86_ism       -> Sail_impl_base.E_little_endian
      end
  | Some true  -> Sail_impl_base.E_big_endian
  | Some false -> Sail_impl_base.E_little_endian
  end

let pp_endianness = fun () ->
  begin match get_endianness () with
  | Sail_impl_base.E_little_endian -> "little endian"
  | Sail_impl_base.E_big_endian    -> "big endian"
  end

let set_model_ism ism =
  model_params :=
    {!model_params with
        MachineDefTypes.t = {!model_params.MachineDefTypes.t with
                                MachineDefTypes.thread_isa_info = ism;
                            }
    }

let suppress_non_symbol_memory = ref false (* ELF *)

let aarch64gen = ref false

let final_cond = ref None

let branch_targets = ref None

let branch_targets_parse lexbuf : unit =
  let print_position lexbuf : string =
    let pos = lexbuf.Lexing.lex_curr_p in
    Printf.sprintf "%s:%d:%d" pos.Lexing.pos_fname
        pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1)
  in

  branch_targets :=
    try Some (Branch_targets_parser.lines Branch_targets_lexer.read lexbuf) with
    | Branch_targets_lexer.SyntaxError msg ->
        Printf.eprintf "%s: %s\n" (print_position lexbuf) msg;
        exit 1
    | Parsing.Parse_error ->
        Printf.eprintf "%s: syntax error\n" (print_position lexbuf);
        exit 1

let branch_targets_parse_from_file (filename: string) : unit =
  Utils.safe_open_in filename @@ fun chan ->
  let lexbuf = Lexing.from_channel chan in
  lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = filename };
  branch_targets_parse lexbuf

let branch_targets_parse_from_string (str: string) : unit =
  let lexbuf = Lexing.from_string str in
  (*lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };*)
  branch_targets_parse lexbuf


let shared_memory = ref None

let shared_memory_parse lexbuf : unit =
  let print_position lexbuf : string =
    let pos = lexbuf.Lexing.lex_curr_p in
    Printf.sprintf "%s:%d:%d" pos.Lexing.pos_fname
        pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1)
  in

  shared_memory :=
    try Some (Shared_memory_parser.footprints Shared_memory_lexer.read lexbuf) with
    | Shared_memory_lexer.SyntaxError msg ->
        Printf.eprintf "%s: %s\n" (print_position lexbuf) msg;
        exit 1
    | Parsing.Parse_error ->
        Printf.eprintf "%s: syntax error\n" (print_position lexbuf);
        exit 1

let shared_memory_parse_from_file (filename: string) : unit =
  Utils.safe_open_in filename @@ fun chan ->
  let lexbuf = Lexing.from_channel chan in
  lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = filename };
  shared_memory_parse lexbuf

let shared_memory_parse_from_string (str: string) : unit =
  let lexbuf = Lexing.from_string str in
  (*lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };*)
  shared_memory_parse lexbuf

let add_bt_and_sm_to_model_params symbol_table =
  let labels_map =
    let (fps, names) = List.split symbol_table in
    List.combine names fps
  in

  let bt =
    let address_of_location = function
      | Branch_targets_parser_base.Absolute n ->
          Sail_impl_base.address_of_integer n
      | Branch_targets_parser_base.Label_and_offset (label, offset) ->
          begin match List.assoc label labels_map with
          | (addr, _) ->
              Sail_impl_base.integer_of_address addr
              |> Nat_big_num.add offset
              |> Sail_impl_base.address_of_integer
          | exception Not_found -> failwith @@ "the location label \"" ^ label ^ "\" does not exist"
          end
    in

    begin match !branch_targets with
    | Some bts ->
        List.map
          (fun (bt: Branch_targets_parser_base.ast) ->
            (bt.Branch_targets_parser_base.thread,
              address_of_location bt.Branch_targets_parser_base.branch_loc,
              List.map address_of_location bt.Branch_targets_parser_base.branch_targets
            )
          )
          bts
        |> List.sort (fun (t1, addr1, _) (t2, addr2, _) ->
              match compare t1 t2 with
              | 0 -> Sail_impl_base.addressCompare addr1 addr2
              | c -> c)
        |> List.fold_left (fun acc (tid, addr, addrs) ->
          match acc with
          | (tid', (addr', addrs') :: tbts) :: acc
              when tid' = tid && Sail_impl_base.addressEqual addr' addr
              -> (tid, (addr, addrs @ addrs') :: tbts) :: acc
          | (tid', tbts) :: acc when tid' = tid ->
              (tid, (addr, addrs) :: tbts) :: acc
          | _ -> (tid, (addr, addrs) :: []) :: acc
        ) []

    | None -> []
    end |> MachineDefSystem.branch_targets_from_list
  in

  let sm =
    begin match !shared_memory with
    | Some shared_memory ->
        List.map
          (function
            | Shared_memory_parser_base.Absolute (addr, size) ->
                (Sail_impl_base.address_of_integer addr, size)
            | Shared_memory_parser_base.Symbol (symb, None) ->
                begin try List.assoc symb labels_map with
                | Not_found -> failwith @@ "the symbol \"" ^ symb ^ "\" does not exist"
                end
            | Shared_memory_parser_base.Symbol (symb, Some (offset, size)) ->
                begin match List.assoc symb labels_map with
                | (addr, _) ->
                    let addr =
                      Sail_impl_base.integer_of_address addr
                      |> Nat_big_num.add offset
                      |> Sail_impl_base.address_of_integer
                    in
                    (addr, size)
                | exception Not_found -> failwith @@ "the symbol \"" ^ symb ^ "\" does not exist"
                end
          )
          shared_memory
        |> Pset.from_list MachineDefTypes.footprintCompare

    | None -> Pset.empty MachineDefTypes.footprintCompare
    end
  in

  model_params :=
    MachineDefTypes.{ !model_params with
      t = {!model_params.t with branch_targets  = bt};
      shared_memory = sm;
    }

(** UI options ******************************************************)

exception Interactive_quit

let auto_follow       = ref false
let random_seed       = ref (None : int option)
let interactive_auto  = ref false
let breakpoint_actual = ref false
let auto_internal     = ref false
let dumb_terminal     = ref false

let follow = ref ([] : Interact_parser_base.ast list)
let set_follow str : unit =
  follow := List.map (fun s -> Interact_parser_base.Transitions [int_of_string s]) (Str.split (Str.regexp ";") str)

let ui_commands = ref None

let use_dwarf = ref false
let dwarf_source_dir = ref ""
let dwarf_show_all_variable_locations = ref false (* later will probably refactor into ppmode *)

let isa_defs_path = ref None

(** PP options ******************************************************)

type x86syntax =
  | X86_gas
  | X86_intel

let x86syntax = ref None

type ppstyle =
  | Ppstyle_full
  | Ppstyle_compact
  | Ppstyle_screenshot

let ppstyles = [Ppstyle_full; Ppstyle_compact; Ppstyle_screenshot]

let pp_ppstyle s =
  match s with
  | Ppstyle_full -> "full"
  | Ppstyle_compact -> "compact"
  | Ppstyle_screenshot -> "screenshot"

type ppkind =
  | Ascii
  | Html
  | Latex
  | Hash

type graph_backend =
  | Dot
  | Tikz

let graph_backend              = ref Dot

let set_graph_backend (b: string) =
  graph_backend :=
    begin match b with
    | "dot"  -> Dot
    | "tikz" -> Tikz
    | _ -> raise (Failure ("graph backend must be one of dot or tikz"))
    end

let pp_graph_backend (b: graph_backend) =
  begin match b with
  | Dot -> "dot"
  | Tikz -> "tikz"
  end

type run_dot = (* generate execution graph... *)
  | RD_step         (* at every step *)
  | RD_final        (* when reaching a final state (and stop) *)
  | RD_final_ok     (* when reaching a final state that sat. the
                    condition (and stop) *)
  | RD_final_not_ok (* when reaching a final state that does not sat.
                    the condition (and stop) *)
let run_dot                    = ref None
let generateddir               = ref None
let print_hex                  = ref false

let pp_colours                 = ref false
let pp_kind                    = ref Ascii
let pp_condense_finished_instructions = ref true
let pp_suppress_newpage        = ref false
let pp_buffer_messages         = ref true
let pp_style                   = ref Ppstyle_full
let pp_prefer_symbolic_values  = ref true
let pp_hide_pseudoregister_reads  = ref true
let pp_max_finished            = ref (Some 4)
let ppg_shared                 = ref false
let ppg_regs                   = ref false
let ppg_reg_rf                 = ref false
let ppg_trans                  = ref true
let pp_announce_options        = ref true
let pp_sail                    = ref true

let set_pp_kind (k: string) =
  pp_kind :=
    begin match k with
    | "Ascii" -> Ascii
    | "Latex" -> Latex
    | "Html"  -> Html
    | "Hash"  -> Hash
    | _ -> raise (Failure ("ppkind must be one of Ascii, Latex, Html or Hash"))
    end

let pp_pp_kind (k:ppkind) =
  match k with
    | Ascii -> "Ascii"
    | Latex -> "Latex"
    | Html  -> "Html"
    | Hash  -> "Hash"


type ppmode =
  { pp_kind:                           ppkind;
    pp_colours:                        bool;
    pp_condense_finished_instructions: bool;
    pp_style:                          ppstyle;
    pp_suppress_newpage:               bool;
    pp_buffer_messages:                bool;
    pp_choice_history_limit:           int option;
    pp_symbol_table:                   ((Sail_impl_base.address * int) * string) list;
    pp_dwarf_static:                   Dwarf.dwarf_static option;
    pp_dwarf_dynamic:                  Types.dwarf_dynamic option;
    pp_initial_write_ioids:            MachineDefTypes.ioid list;
    pp_prefer_symbolic_values:         bool;
    pp_hide_pseudoregister_reads: bool;
    pp_max_finished:                   int option;
    ppg_shared:                        bool;
    ppg_rf:                            bool;
    ppg_fr:                            bool;
    ppg_co:                            bool;
    ppg_addr:                          bool;
    ppg_data:                          bool;
    ppg_ctrl:                          bool;
    ppg_regs:                          bool;
    ppg_reg_rf:                        bool;
    ppg_trans:                         bool;
    pp_pretty_eiid_table:              (MachineDefTypes.eiid * string) list;
    pp_trans_prefix:                   bool;
    pp_announce_options:               bool;
    pp_sail:                           bool;
    pp_default_cmd:             Interact_parser_base.ast option;

(*    pp_instruction : (((Sail_impl_base.address * MachineDefTypes.size) * string) list) ->
                     MachineDefTypes.instruction_ast ->
                     Sail_impl_base.address ->
                     string; *)
  }

(* ppmode lenses *)
let pp_kind_lens = { Lens.get = (fun m -> m.pp_kind); Lens.set = (fun v m -> { m with pp_kind = v }) }
let pp_colours_lens = { Lens.get = (fun m -> m.pp_colours); Lens.set = (fun v m -> { m with pp_colours = v }) }
let pp_condense_finished_instructions_lens = { Lens.get = (fun m -> m.pp_condense_finished_instructions); Lens.set = (fun v m -> { m with pp_condense_finished_instructions = v }) }
let pp_style_lens = { Lens.get = (fun m -> m.pp_style); Lens.set = (fun v m -> { m with pp_style = v }) }
let pp_suppress_newpage_lens = { Lens.get = (fun m -> m.pp_suppress_newpage); Lens.set = (fun v m -> { m with pp_suppress_newpage = v }) }
let pp_buffer_messages_lens = { Lens.get = (fun m -> m.pp_buffer_messages); Lens.set = (fun v m -> { m with pp_buffer_messages = v }) }
let pp_choice_history_limit_lens = { Lens.get = (fun m -> m.pp_choice_history_limit); Lens.set = (fun v m -> { m with pp_choice_history_limit = v }) }
let pp_symbol_table_lens = { Lens.get = (fun m -> m.pp_symbol_table); Lens.set = (fun v m -> { m with pp_symbol_table = v }) }
let pp_dwarf_static_lens = { Lens.get = (fun m -> m.pp_dwarf_static); Lens.set = (fun v m -> { m with pp_dwarf_static = v }) }
let pp_dwarf_dynamic_lens = { Lens.get = (fun m -> m.pp_dwarf_dynamic); Lens.set = (fun v m -> { m with pp_dwarf_dynamic = v }) }
let pp_initial_write_ioids_lens = { Lens.get = (fun m -> m.pp_initial_write_ioids); Lens.set = (fun v m -> { m with pp_initial_write_ioids = v }) }
let pp_prefer_symbolic_values_lens = { Lens.get = (fun m -> m.pp_prefer_symbolic_values); Lens.set = (fun v m -> { m with pp_prefer_symbolic_values = v }) }
let pp_hide_pseudoregister_reads_lens = { Lens.get = (fun m -> m.pp_hide_pseudoregister_reads); Lens.set = (fun v m -> { m with pp_hide_pseudoregister_reads = v }) }
let pp_max_finished_lens = { Lens.get = (fun m -> m.pp_max_finished); Lens.set = (fun v m -> { m with pp_max_finished = v }) }
let ppg_shared_lens = { Lens.get = (fun m -> m.ppg_shared); Lens.set = (fun v m -> { m with ppg_shared = v }) }
let ppg_rf_lens = { Lens.get = (fun m -> m.ppg_rf); Lens.set = (fun v m -> { m with ppg_rf = v }) }
let ppg_fr_lens = { Lens.get = (fun m -> m.ppg_fr); Lens.set = (fun v m -> { m with ppg_fr = v }) }
let ppg_co_lens = { Lens.get = (fun m -> m.ppg_co); Lens.set = (fun v m -> { m with ppg_co = v }) }
let ppg_addr_lens = { Lens.get = (fun m -> m.ppg_addr); Lens.set = (fun v m -> { m with ppg_addr = v }) }
let ppg_data_lens = { Lens.get = (fun m -> m.ppg_data); Lens.set = (fun v m -> { m with ppg_data = v }) }
let ppg_ctrl_lens = { Lens.get = (fun m -> m.ppg_ctrl); Lens.set = (fun v m -> { m with ppg_ctrl = v }) }
let ppg_regs_lens = { Lens.get = (fun m -> m.ppg_regs); Lens.set = (fun v m -> { m with ppg_regs = v }) }
let ppg_reg_rf_lens = { Lens.get = (fun m -> m.ppg_reg_rf); Lens.set = (fun v m -> { m with ppg_reg_rf = v }) }
let ppg_trans_lens = { Lens.get = (fun m -> m.ppg_trans); Lens.set = (fun v m -> { m with ppg_trans = v }) }
let pp_pretty_eiid_table_lens = { Lens.get = (fun m -> m.pp_pretty_eiid_table); Lens.set = (fun v m -> { m with pp_pretty_eiid_table = v }) }
let pp_trans_prefix_lens = { Lens.get = (fun m -> m.pp_trans_prefix); Lens.set = (fun v m -> { m with pp_trans_prefix = v }) }
let pp_announce_options_lens = { Lens.get = (fun m -> m.pp_announce_options); Lens.set = (fun v m -> { m with pp_announce_options = v }) }
let pp_sail_lens = { Lens.get = (fun m -> m.pp_sail); Lens.set = (fun v m -> { m with pp_sail = v }) }
let pp_default_cmd_lens = { Lens.get = (fun m -> m.pp_default_cmd); Lens.set = (fun v m -> { m with pp_default_cmd = v }) }


(* NOTE: this function should be called only once, from top.ml *)
let get_ppmode : unit -> ppmode = fun () ->
  { pp_kind                               = !pp_kind;
    pp_colours                            = !pp_colours;
    pp_condense_finished_instructions     = !pp_condense_finished_instructions;
    pp_style                              = !pp_style;
    pp_suppress_newpage                   = !pp_suppress_newpage;
    pp_buffer_messages                    = !pp_buffer_messages;
    pp_choice_history_limit               = None;
    pp_symbol_table                       = [];
    pp_dwarf_static                       = None;
    pp_dwarf_dynamic                      = None;
    pp_initial_write_ioids                = [];
    pp_prefer_symbolic_values             = !pp_prefer_symbolic_values;
    pp_hide_pseudoregister_reads = !pp_hide_pseudoregister_reads;
    pp_max_finished                       = !pp_max_finished;
    ppg_shared                            = !ppg_shared;
    ppg_rf                                = true;
    ppg_fr                                = true;
    ppg_co                                = true;
    ppg_addr                              = true;
    ppg_data                              = true;
    ppg_ctrl                              = true;
    ppg_regs                              = !ppg_regs;
    ppg_reg_rf                            = !ppg_reg_rf;
    ppg_trans                             = !ppg_trans;
    pp_pretty_eiid_table                  = [];
    pp_trans_prefix                       = true;
    pp_announce_options                   = !pp_announce_options;
    pp_sail                               = !pp_sail;
    pp_default_cmd                        = None;
    (*    pp_instruction = Pp.pp_instruction *)

      (*fun
        (_: ((Sail_impl_base.address * MachineDefTypes.size) * string) list)
        (inst: Sail_impl_base.instruction)
        (_: Sail_impl_base.address)
      ->
      Printing_functions.instruction_to_string inst; *)
  }

let ppmode_for_hashing : ppmode =
  { pp_kind                               = Hash;
    pp_colours                            = false;
    pp_condense_finished_instructions     = false;
    pp_style                              = Ppstyle_full;
    pp_suppress_newpage                   = true;
    pp_buffer_messages                    = false;
    pp_choice_history_limit               = None;
    pp_symbol_table                       = [];
    pp_dwarf_static                       = None;
    pp_dwarf_dynamic                      = None;
    pp_initial_write_ioids                = [];
    pp_prefer_symbolic_values             = false;
    pp_hide_pseudoregister_reads          = false;
    pp_max_finished                       = None;
    ppg_shared                            = false;
    ppg_rf                                = false;
    ppg_fr                                = false;
    ppg_co                                = false;
    ppg_addr                              = false;
    ppg_data                              = false;
    ppg_ctrl                              = false;
    ppg_regs                              = false;
    ppg_reg_rf                            = false;
    ppg_trans                             = false;
    pp_pretty_eiid_table                  = [];
    pp_trans_prefix                       = true;
    pp_announce_options                   = true;
    pp_sail                               = true;
    pp_default_cmd                        = None;
    (*    pp_instruction                        = Pp.pp_instruction *)
  }

(** topologies ******************************************************)

let elf_threads = ref 1

let flowing_topologies = ref ([]: MachineDefTypes.flowing_topology list)

let topauto = ref false

(* topologies to use for web interface (not for text)*)
let topology_2 = ref (List.hd (Model_aux.ui_topologies 2))
let topology_3 = ref (List.hd (Model_aux.ui_topologies 3))
let topology_4 = ref (List.hd (Model_aux.ui_topologies 4))

let get_topologies thread_count =
  begin match !pp_kind with
  | Latex (* TODO: (SF) untested *)
  | Ascii | Hash ->
      if !topauto then
        begin match Model_aux.parse_topologies (String.concat ";" (Model_aux.exhaustive_topologies thread_count)) with
        | Some tops -> tops
        | None -> raise (Failure "exhaustive_topologies")
        end
      else !flowing_topologies

  | Html ->
      let s =
        begin match thread_count with
        | 1 -> List.hd (Model_aux.ui_topologies 1)
        | 2 -> !topology_2
        | 3 -> !topology_3
        | 4 -> !topology_4
        | _ -> raise (Failure "web interface only supports topology for tests with 1..4 threads")
        end
      in
      begin match Model_aux.parse_topologies s with
      | Some t -> t
      | None -> raise (Failure "top")
      end
  end


(** snapshot data ***************************************************)

let snapshot_data : (string * (MachineDefTypes.ioid * MachineDefTypes.register_snapshot * MachineDefTypes.memory_snapshot) list) ref = ref ("",[])

exception Test_proper_termination
