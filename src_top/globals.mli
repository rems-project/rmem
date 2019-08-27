(*==================================================================================================*)
(*                                                                                                  *)
(*                rmem executable model                                                             *)
(*                =====================                                                             *)
(*                                                                                                  *)
(*  This file is:                                                                                   *)
(*                                                                                                  *)
(*  Copyright Peter Sewell, University of Cambridge                          2011-2012, 2014-2017   *)
(*  Copyright Shaked Flur, University of Cambridge                                      2014-2018   *)
(*  Copyright Jon French, University of Cambridge                                       2017-2018   *)
(*  Copyright Susmit Sarkar, University of St Andrews                        2011-2012, 2014-2015   *)
(*  Copyright Christopher Pulte, University of Cambridge                                2015-2016   *)
(*  Copyright Luc Maranget, INRIA, Paris, France                                        2011-2012   *)
(*  Copyright Francesco Zappa Nardelli, INRIA, Paris, France                                 2011   *)
(*  Copyright Pankaj Pawan, IIT Kanpur and INRIA (when this work was done)                   2011   *)
(*  Copyright Ohad Kammar, University of Cambridge (when this work was done)                 2013   *)
(*                                                                                                  *)
(*  All rights reserved.                                                                            *)
(*                                                                                                  *)
(*  It is part of the rmem tool, distributed under the 2-clause BSD licence in                      *)
(*  LICENCE.txt.                                                                                    *)
(*                                                                                                  *)
(*==================================================================================================*)

(* val get_cands : bool ref *)
(* val smt : bool ref *)
(* val solver : string ref *)
(* val candidates : string option ref *)
(* val minimal : bool ref *)
(* val optoax : bool ref *)
(* val axtoop : bool ref *)

(** output options **************************************************)

val logdir: (string option) ref

val dont_tool: bool ref (* "Dont" output *)

val debug_sail_interp : bool ref

(** model options ***************************************************)

val ism: InstructionSemantics.instruction_semantics_mode ref
val model_params: Params.model_params ref

val big_endian: (bool option) ref

(* BE CAREFUL: call get_endianness only after thread_ism (of model_params)
has been set properly (i.e. set_model_ism was called) *)
val get_endianness: unit -> Sail_impl_base.end_flag
val pp_endianness: unit -> string

val set_model_ism: InstructionSemantics.instruction_semantics_mode -> unit

val suppress_non_symbol_memory: bool ref (* ELF *)

val aarch64gen: bool ref

val final_cond: string option ref (* 'Some s': change the final condition to 's' *)

val branch_targets: Branch_targets_parser_base.ast list option ref
val litmus_test_base_address: int ref
val litmus_test_minimum_width: int ref
val aval_of_inst_index : int -> int -> int
val aval_of_inst_index_num : int -> int -> Nat_big_num.num

val shared_memory: Shared_memory_parser_base.footprint list option ref
val add_bt_and_sm_to_model_params: ((Sail_impl_base.address * int) * string) list -> unit

(** UI options ******************************************************)

val auto_follow:       bool ref
val interactive_auto:  bool ref
val auto_internal:     bool ref

val random_seed: int option ref   (* per ppcmem invocation seed: None for fresh, or Some n for seed n *)

val follow:     Interact_parser_base.ast list ref

val ui_commands: (string option) ref

val use_dwarf: bool ref
val dwarf_source_dir: string ref
val dwarf_show_all_variable_locations: bool ref

(** PP stuff ********************************************************)

type x86syntax =
  | X86_gas
  | X86_intel

val x86syntax : (x86syntax option) ref

type ppstyle =
  | Ppstyle_full
  | Ppstyle_compact
  | Ppstyle_screenshot

val ppstyles : ppstyle list

val pp_ppstyle : ppstyle -> string

type ppkind =
  | Ascii
  | Html
  | Latex
  | Hash

type graph_backend =
  | Dot
  | Tikz

val set_graph_backend: string -> unit
val pp_graph_backend: graph_backend -> string

val graph_backend:             graph_backend ref
type run_dot = (* generate execution graph... *)
  | RD_step         (* at every step *)
  | RD_final        (* when reaching a final state (and stop) *)
  | RD_final_ok     (* when reaching a final state that sat. the
                    condition (and stop) *)
  | RD_final_not_ok (* when reaching a final state that does not sat.
                    the condition (and stop) *)
val run_dot:                   (run_dot option) ref
val print_cexs:                bool ref
val generateddir:              (string option) ref
val print_hex:                 bool ref


val pp_colours:                bool ref
val pp_kind:                   ppkind ref
val pp_condense_finished_instructions: bool ref
val pp_style:                  ppstyle ref
val pp_prefer_symbolic_values: bool ref
val pp_hide_pseudoregister_reads: bool ref
val pp_max_finished:           int option ref
val ppg_shared:                bool ref
val ppg_regs:                  bool ref
val ppg_reg_rf:                bool ref
val ppg_trans:                 bool ref
val pp_sail:                   bool ref

val set_pp_kind : string -> unit

val pp_pp_kind : ppkind -> string

type ppmode =
  { pp_kind:                           ppkind;
    pp_colours:                        bool;
    pp_condense_finished_instructions: bool;
    pp_style:                          ppstyle;
    pp_choice_history_limit:           int option;
    pp_symbol_table: ((Sail_impl_base.address * Sail_impl_base.size) * string) list;
    pp_dwarf_static:                   Dwarf.dwarf_static option;
    pp_dwarf_dynamic:                  Types.dwarf_dynamic option;
    pp_initial_write_ioids:            Events.ioid list;
    pp_prefer_symbolic_values:         bool;
    pp_hide_pseudoregister_reads:      bool;
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
    pp_pretty_eiid_table:              (Events.eiid * string) list;
    pp_trans_prefix:                   bool;
    pp_sail:                           bool;
    pp_default_cmd:                    Interact_parser_base.ast option;

  }

val pp_kind_lens                            : (ppmode, ppkind)                                                          Lens.t
val pp_colours_lens                         : (ppmode, bool)                                                            Lens.t
val pp_condense_finished_instructions_lens  : (ppmode, bool)                                                            Lens.t
val pp_style_lens                           : (ppmode, ppstyle)                                                         Lens.t
val pp_choice_history_limit_lens            : (ppmode, int option)                                                      Lens.t
val pp_symbol_table_lens                    : (ppmode, ((Sail_impl_base.address * Sail_impl_base.size) * string) list) Lens.t
val pp_dwarf_static_lens                    : (ppmode, Dwarf.dwarf_static option)                                       Lens.t
val pp_dwarf_dynamic_lens                   : (ppmode, Types.dwarf_dynamic option)                                      Lens.t
val pp_initial_write_ioids_lens             : (ppmode, Events.ioid list)                                       Lens.t
val pp_prefer_symbolic_values_lens          : (ppmode, bool)                                                            Lens.t
val pp_hide_pseudoregister_reads_lens       : (ppmode, bool)                                                            Lens.t
val pp_max_finished_lens                    : (ppmode, int option)                                                      Lens.t
val ppg_shared_lens                         : (ppmode, bool)                                                            Lens.t
val ppg_rf_lens                             : (ppmode, bool)                                                            Lens.t
val ppg_fr_lens                             : (ppmode, bool)                                                            Lens.t
val ppg_co_lens                             : (ppmode, bool)                                                            Lens.t
val ppg_addr_lens                           : (ppmode, bool)                                                            Lens.t
val ppg_data_lens                           : (ppmode, bool)                                                            Lens.t
val ppg_ctrl_lens                           : (ppmode, bool)                                                            Lens.t
val ppg_regs_lens                           : (ppmode, bool)                                                            Lens.t
val ppg_reg_rf_lens                         : (ppmode, bool)                                                            Lens.t
val ppg_trans_lens                          : (ppmode, bool)                                                            Lens.t
val pp_pretty_eiid_table_lens               : (ppmode, (Events.eiid * string) list)                            Lens.t
val pp_trans_prefix_lens                    : (ppmode, bool)                                                            Lens.t
val pp_sail_lens                            : (ppmode, bool)                                                            Lens.t
val pp_default_cmd_lens                     : (ppmode, Interact_parser_base.ast option)                                 Lens.t

val get_ppmode : unit -> ppmode

val ppmode_for_hashing : ppmode

(** topologies ******************************************************)

val elf_threads: int ref

val flowing_topologies : Params.flowing_topology list ref
val topauto: bool ref

(* topologies to use for web interface (not for text)*)
val topology_2: string ref
val topology_3: string ref
val topology_4: string ref

val get_topologies : int -> Params.flowing_topology list
