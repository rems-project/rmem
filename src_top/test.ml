(*===============================================================================*)
(*                                                                               *)
(*                rmem executable model                                          *)
(*                =====================                                          *)
(*                                                                               *)
(*  This file is:                                                                *)
(*                                                                               *)
(*  Copyright Susmit Sarkar, University of St Andrews     2014-2015              *)
(*  Copyright Shaked Flur, University of Cambridge        2016-2018              *)
(*  Copyright Peter Sewell, University of Cambridge      2014, 2016              *)
(*  Copyright Luc Maranget, INRIA, Paris, France               2017              *)
(*  Copyright Christopher Pulte, University of Cambridge       2016              *)
(*  Copyright Jon French, University of Cambridge              2016              *)
(*                                                                               *)
(*  All rights reserved.                                                         *)
(*                                                                               *)
(*  It is part of the rmem tool, distributed under the 2-clause BSD licence in   *)
(*  LICENCE.txt.                                                                 *)
(*                                                                               *)
(*===============================================================================*)

(* open Events *)

type reg_base_name = Sail_impl_base.reg_base_name
type address = Sail_impl_base.address
type register_value = Sail_impl_base.register_value
type memory_value = Sail_impl_base.memory_value
type instruction_ast = InstructionSemantics.instruction_ast
type thread_id = Events.thread_id

module C = MoreConstraints.Make

module LocationMap = MyMap.Make(String)

(* 'fake' tid for the memory write events of initial values *)
let init_thread = 1000

type labelmap = (string * int) list

type test = 
  { arch:            Archs.t;
    info:            MiscParser.info;
    init_reg_state:  ((thread_id * reg_base_name) * register_value) list;
    init_mem_state:  (address * memory_value) list;
    mem_addr_map:    (address * int) LocationMap.t;
    prog:            (thread_id * instruction_ast list * labelmap) list;
    filter:          C.prop option;
    constr:          C.constr;
    flocs:           C.location list;
  }

type info =
  { name:           string;

    ism:            InstructionSemantics.instruction_semantics_mode;
    thread_count:   int;
    symbol_map:     Elf_file.global_symbol_init_info;
    symbol_table:   ((address * int) * string) list;
    dwarf_static:   Dwarf.dwarf_static option;

    (* Locations that will show in the histogram *)
    show_regs: (thread_id * reg_base_name) list;
    show_mem:  (address * int) list;

    (* Locations we need for checking the filter property *)
    filter_regs: (thread_id * reg_base_name) list;
    filter_mem:  (address * int) list;

    info:           MiscParser.info;
    filter:         C.prop option;
    constr:         C.constr;
  }

let trim_state ti state = C.trim_state ti.show_regs ti.show_mem state
