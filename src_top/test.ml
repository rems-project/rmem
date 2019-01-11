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
(*  Copyright Luc Maranget, INRIA Paris                        2017              *)
(*  Copyright Christopher Pulte, University of Cambridge       2016              *)
(*  Copyright Jon French, University of Cambridge              2016              *)
(*                                                                               *)
(*  All rights reserved.                                                         *)
(*                                                                               *)
(*  It is part of the rmem tool, distributed under the 2-clause BSD licence in   *)
(*  LICENCE.txt.                                                                 *)
(*                                                                               *)
(*===============================================================================*)

open MachineDefTypes

module C = MoreConstraints.Make

module LocationMap = MyMap.Make(String)

(* 'fake' tid for the memory write events of initial values *)
let init_thread = 1000

type labelmap = (string * int) list

type test = 
  { arch:            Archs.t;
    info:            MiscParser.info;
    init_reg_state:  ((thread_id * reg_base_name) * Sail_impl_base.register_value) list;
    init_mem_state:  (Sail_impl_base.address * Sail_impl_base.memory_value) list;
    mem_addr_map:    (Sail_impl_base.address * int) LocationMap.t;
    prog:            (thread_id * MachineDefTypes.instruction_ast list * labelmap) list;
    filter:          C.prop option;
    constr:          C.constr;
    flocs:           C.location list;
  }

type info =
  { name:           string;

    ism:            MachineDefTypes.instruction_semantics_mode;
    thread_count:   int;
    symbol_map:     Elf_file.global_symbol_init_info;
    symbol_table:   ((Sail_impl_base.address * int) * string) list;
    dwarf_static:   Dwarf.dwarf_static option;

    (* Locations that will show in the histogram *)
    show_regs: (MachineDefTypes.thread_id * MachineDefTypes.reg_base_name) list;
    show_mem:  (Sail_impl_base.address * int) list;

    (* Locations we need for checking the filter property *)
    filter_regs: (MachineDefTypes.thread_id * MachineDefTypes.reg_base_name) list;
    filter_mem:  (Sail_impl_base.address * int) list;

    info:           MiscParser.info;
    filter:         C.prop option;
    constr:         C.constr;
  }

let trim_state ti state = C.trim_state ti.show_regs ti.show_mem state
