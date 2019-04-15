(*===============================================================================*)
(*                                                                               *)
(*                rmem executable model                                          *)
(*                =====================                                          *)
(*                                                                               *)
(*  This file is:                                                                *)
(*                                                                               *)
(*  Copyright Jon French, University of Cambridge        2016-2017               *)
(*  Copyright Shaked Flur, University of Cambridge       2016-2017               *)
(*  Copyright Christopher Pulte, University of Cambridge 2016-2018               *)
(*                                                                               *)
(*  All rights reserved.                                                         *)
(*                                                                               *)
(*  It is part of the rmem tool, distributed under the 2-clause BSD licence in   *)
(*  LICENCE.txt.                                                                 *)
(*                                                                               *)
(*===============================================================================*)

open Params

type t =
  { interactive:             bool;
    eager_mode:              eager_mode;
    hash_prune:              bool;
    allow_partial:           bool;
    partial_order_reduction: bool;
    priority_reduction:      bool;
    prune_restarts:          bool;
    prune_discards:          bool;
    prune_late_writes:       bool; (* promising only *)
    pseudorandom:            bool;
    pseudorandom_traces:     int;

    interpreter:             bool; (* true for interpreter, false for shallow embedding *)
    compare_analyses:        bool; (* true for comparing handwritten and exhaustive analysis *)

    (* max_trace_length: in exhaustive and random search, fail if a trace
    longer than max_trace_length (non-eager) transitions is found. This
    is useful when running regression tests where a bug in rmem might
    lead to infinite loops. We expect litmus tests will not have traces
    with more than 1000 (non-eager) transitions. *)
    max_trace_length:        int option;

    (* Limits *)
    transition_limit:        int option;
    time_limit:              int option;
    trace_limit:             int option;

    always_print:            bool;

    focused_thread:          int option;
    focused_ioid:            Events.ioid option;
    storage_first:           bool;
  }

(* Lenses *)
let interactive_lens = { Lens.get = (fun o -> o.interactive); Lens.set = (fun v o -> { o with interactive = v }) }
let eager_mode_lens = { Lens.get = (fun o -> o.eager_mode); Lens.set = (fun v o -> { o with eager_mode = v }) }
let hash_prune_lens = { Lens.get = (fun o -> o.hash_prune); Lens.set = (fun v o -> { o with hash_prune = v }) }
let allow_partial_lens = { Lens.get = (fun o -> o.allow_partial); Lens.set = (fun v o -> { o with allow_partial = v }) }
let partial_order_reduction_lens = { Lens.get = (fun o -> o.partial_order_reduction); Lens.set = (fun v o -> { o with partial_order_reduction = v }) }
let priority_reduction_lens = { Lens.get = (fun o -> o.priority_reduction); Lens.set = (fun v o -> { o with priority_reduction = v }) }
let prune_restarts_lens = { Lens.get = (fun o -> o.prune_restarts); Lens.set = (fun v o -> { o with prune_restarts = v }) }
let prune_discards_lens = { Lens.get = (fun o -> o.prune_discards); Lens.set = (fun v o -> { o with prune_discards = v }) }
let prune_late_writes_lens = { Lens.get = (fun o -> o.prune_late_writes); Lens.set = (fun v o -> { o with prune_late_writes = v }) }
let pseudorandom_lens = { Lens.get = (fun o -> o.pseudorandom); Lens.set = (fun v o -> { o with pseudorandom = v }) }
let pseudorandom_traces_lens = { Lens.get = (fun o -> o.pseudorandom_traces); Lens.set = (fun v o -> { o with pseudorandom_traces = v }) }
let interpreter_lens = { Lens.get = (fun o -> o.interpreter); Lens.set = (fun v o -> { o with interpreter = v }) }
let compare_analyses_lens = { Lens.get = (fun o -> o.compare_analyses); Lens.set = (fun v o -> { o with compare_analyses = v }) }
let max_trace_length_lens = { Lens.get = (fun o -> o.max_trace_length); Lens.set = (fun v o -> { o with max_trace_length = v }) }
let transition_limit_lens = { Lens.get = (fun o -> o.transition_limit); Lens.set = (fun v o -> { o with transition_limit = v }) }
let time_limit_lens = { Lens.get = (fun o -> o.time_limit); Lens.set = (fun v o -> { o with time_limit = v }) }
let trace_limit_lens = { Lens.get = (fun o -> o.trace_limit); Lens.set = (fun v o -> { o with trace_limit = v }) }
let always_print_lens = { Lens.get = (fun o -> o.always_print); Lens.set = (fun v o -> { o with always_print = v }) }
let focused_thread_lens = { Lens.get = (fun o -> o.focused_thread); Lens.set = (fun v o -> { o with focused_thread = v }) }
let focused_ioid_lens = { Lens.get = (fun o -> o.focused_ioid); Lens.set = (fun v o -> { o with focused_ioid = v }) }
let storage_first_lens = { Lens.get = (fun o -> o.storage_first); Lens.set = (fun v o -> { o with storage_first = v }) }


let eager_fetch_single_lens = ({ Lens.get = (fun o -> o.eager_fetch_single); Lens.set = (fun v o -> { o with eager_fetch_single = v }) })
let eager_fetch_multi_lens = ({ Lens.get = (fun o -> o.eager_fetch_multi); Lens.set = (fun v o -> { o with eager_fetch_multi = v }) })
let eager_pseudocode_internal_lens = ({ Lens.get = (fun o -> o.eager_pseudocode_internal); Lens.set = (fun v o -> { o with eager_pseudocode_internal = v }) })
let eager_constant_reg_read_lens = ({ Lens.get = (fun o -> o.eager_constant_reg_read); Lens.set = (fun v o -> { o with eager_constant_reg_read = v }) })
let eager_reg_rw_lens = ({ Lens.get = (fun o -> o.eager_reg_rw); Lens.set = (fun v o -> { o with eager_reg_rw = v }) })
let eager_memory_aux_lens = ({ Lens.get = (fun o -> o.eager_memory_aux); Lens.set = (fun v o -> { o with eager_memory_aux = v }) })
let eager_finish_lens = ({ Lens.get = (fun o -> o.eager_finish); Lens.set = (fun v o -> { o with eager_finish = v }) })
let eager_fp_recalc_lens = ({ Lens.get = (fun o -> o.eager_fp_recalc); Lens.set = (fun v o -> { o with eager_fp_recalc = v }) })
let eager_thread_start_lens = ({ Lens.get = (fun o -> o.eager_thread_start); Lens.set = (fun v o -> { o with eager_thread_start = v }) })
let eager_local_mem_lens = ({ Lens.get = (fun o -> o.eager_local_mem); Lens.set = (fun v o -> { o with eager_local_mem = v }) })

let eager_mode_all_off eager_mode : eager_mode =
  ({ eager_mode with
     eager_fetch_single        = false;
     eager_fetch_multi         = false;
     eager_pseudocode_internal = false;
     eager_constant_reg_read   = false;
     eager_reg_rw              = false;
     eager_memory_aux          = false;
     eager_finish              = false;
     eager_fp_recalc           = false;
     eager_thread_start        = false;

     eager_local_mem           = false;
  })

let eager_mode_all_on eager_mode : eager_mode =
  ({ eager_mode with
     eager_fetch_single        = true;
     (*eager_fetch_multi         = true;*)
     eager_pseudocode_internal = true;
     eager_constant_reg_read   = true;
     eager_reg_rw              = true;
     eager_memory_aux          = true;
     eager_finish              = true;
     eager_fp_recalc           = true;
     eager_thread_start        = true;

     (*eager_local_mem           = true;*)
  })


let default_options =
  { interactive             = true;
    eager_mode              =
      ({
        eager_fetch_single        = false;
        eager_fetch_multi         = false;
        eager_pseudocode_internal = false;
        eager_constant_reg_read   = false;
        eager_reg_rw              = false;
        eager_memory_aux          = false;
        eager_finish              = false;
        eager_fp_recalc           = false;
        eager_thread_start        = false;

        eager_local_mem           = false;
        em_shared_memory          = Pset.empty Sail_impl_base.footprintCompare;
      });
    hash_prune              = true;
    allow_partial           = false;
    partial_order_reduction = false;
    priority_reduction      = false;
    prune_restarts          = false;
    prune_discards          = false;
    prune_late_writes       = false;
    pseudorandom            = false;
    pseudorandom_traces     = 1;

    interpreter             = true;
    compare_analyses        = false;

    max_trace_length        = None;

    transition_limit        = None;
    time_limit              = None;
    trace_limit             = None;

    always_print            = false;

    focused_thread          = None;
    focused_ioid            = None;
    storage_first           = false;
  }
