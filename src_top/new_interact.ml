(*===============================================================================*)
(*                                                                               *)
(*                rmem executable model                                          *)
(*                =====================                                          *)
(*                                                                               *)
(*  This file is:                                                                *)
(*                                                                               *)
(*  Copyright Jon French, University of Cambridge        2016-2018               *)
(*  Copyright Shaked Flur, University of Cambridge       2016-2018               *)
(*  Copyright Christopher Pulte, University of Cambridge 2016-2018               *)
(*  Copyright Peter Sewell, University of Cambridge      2016-2017               *)
(*                                                                               *)
(*  All rights reserved.                                                         *)
(*                                                                               *)
(*  It is part of the rmem tool, distributed under the 2-clause BSD licence in   *)
(*  LICENCE.txt.                                                                 *)
(*                                                                               *)
(*===============================================================================*)

open RunOptions
open Params

module SO = Structured_output

(** Parser **********************************************************)

type parser_outcome =
  | ParserASTs of Interact_parser_base.ast list
  | ParserError of string

let parse (str: string) : parser_outcome =
  let open Lexing in
  let print_position lexbuf : string =
    let pos = lexbuf.lex_curr_p in
    Printf.sprintf "%s:%d:%d" pos.pos_fname
        pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)
  in

  let lexbuf = Lexing.from_string str in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = "$" };

  try ParserASTs (Interact_parser.commands (Interact_lexer.get_lexer ()) lexbuf) with
  | Interact_lexer.SyntaxError msg ->
      ParserError (Printf.sprintf "%s: %s" (print_position lexbuf) msg)
  | Parsing.Parse_error ->
      ParserError (Printf.sprintf "%s: syntax error" (print_position lexbuf))
  | Interact_parser_base.Parse_error msg ->
      ParserError (Printf.sprintf "error: %s: %s" (print_position lexbuf) msg)

(********************************************************************)

module Make (ConcModel: Concurrency_model.S) = struct
  module Runner = New_run.Make (ConcModel)
  module GraphvizBackend = Graphviz.Make (ConcModel)
  module TikzBackend = Tikz.Make (ConcModel)

type interact_node =
  {
    system_state:         ConcModel.state;
    (* open_transition: Tail is always a number from filtered_transitions that
    was taken as a non-eager transition. The following numbers are eager
    transitions that were taken in a sequence leading to the next interact_node
    (head is the last transition).  This will always be empty for the head node *)
    open_transition:      int list;
    (* filtered_transitions: the option is None if the transition is filtered out,
    otherwise the int is a number the user can enter to take the transition.
    The order of the transitions in this list must match the order of the transitions
    in system_state.sst_system_transitions (so it is possible to translate a search
    trace to an interact trace). *)
    filtered_transitions: (int option * ConcModel.trans) list;
  }

type interact_state =
  { test_info: Test.info;

    options: RunOptions.t;
    ppmode:  Globals.ppmode; (* printing information *)

    (* head is the active node *)
    interact_nodes: interact_node list;
    cmd_history:    Interact_parser_base.ast list;

    (* follow_suffix: intended for holding a list of transitions that
    should be followed. As it is relatively simple to generalise this
    to a list of commands we allow that, but support might be shaky for
    non-transition commands.
    Changing eager_mode, model, focuse (and maybe other things) might
    invalidate the transitions, we do not try to fix them or even to
    clear them, this is the users responsibility. *)
    follow_suffix: Interact_parser_base.ast list;

    (* default_cmd: holds the command that will be taken by default.
    This needs to be recorded only because in random mode we want to show
    the user the transition that was chosen randomly before they actually
    take the transition. *)
    default_cmd: Interact_parser_base.ast option;

    breakpoints: Runner.breakpoint list;
  }

let run_options_lens = { Lens.get = (fun s -> s.options); Lens.set = (fun o s -> { s with options = o }) }
let ppmode_lens = { Lens.get = (fun s -> s.ppmode); Lens.set = (fun m s -> { s with ppmode = m }) }

let sorted_filtered_transitions node =
  List.filter (function (Some _, _) -> true | _ -> false) node.filtered_transitions
  |> List.map (function (Some n, t) -> (n ,t) | _ -> assert false)
  |> List.sort (fun (l, _) (r, _) -> compare l r)

let filtered_out_transitions node =
  List.filter (function (None, _) -> true | _ -> false) node.filtered_transitions
  |> List.split
  |> snd

let get_graph_backend () : (module GraphBackend.S with type ui_trans = ConcModel.ui_trans) =
  match !Globals.graph_backend with
  | Globals.Dot  -> (module GraphvizBackend)
  | Globals.Tikz -> (module TikzBackend)


let number_transitions
    (prev_ts: (int option * ConcModel.trans) list)
    (ts:      (bool * ConcModel.trans) list)
    : (int option * ConcModel.trans) list
  =
  let prev_ts =
    prev_ts
    |> List.filter (function (Some _, _) -> true | _ -> false)
    |> List.map (function (Some n, t) -> (n, t) | _ -> assert false)
  in

  (* "find_old_number [] t nts" return "Some (n, nts')" if the
  transition t is in nts (actually a transition very similar to t).
  n will be the number that was associated with t in nts and nts' is
  nts without t. Otherwise return None.
  We have to remove the match to make sure it doesn't match with
  another transition as fuzzy_compare_transitions is not precise. *)
  let rec find_old_number trans accum = function
    | [] -> None
    | (n, cand) :: old_cands ->
        if ConcModel.fuzzy_compare_transitions cand trans = 0 then
          Some (n, (List.rev accum) @ old_cands)
        else find_old_number trans ((n, cand) :: accum) old_cands
  in

  (* assign numbers to transitions that were present in the previous state *)
  let nts =
    List.fold_left
      (fun (prev_ts, accum) (filter, trans) ->
        if filter then
          match find_old_number trans [] prev_ts with
          | Some (n, prev_ts) -> (prev_ts, (Some (Some n), trans) :: accum)
          | None              -> (prev_ts, (Some None,     trans) :: accum)
        else
          (prev_ts, (None, trans) :: accum)
      )
      (prev_ts, [])
      ts
    |> snd
    |> List.rev
  in

  (* "assign_numbers n ns nts []" assign numbers to the transitions nts
  starting from n and only numbers that are not in ns *)
  let rec assign_numbers n used_ns nts accum =
    match nts with
    | [] -> List.rev accum
    | (None, t) :: nts' -> assign_numbers n used_ns nts' ((None, t) :: accum)
    | (Some None, t) :: nts' ->
        if List.mem n used_ns then
          assign_numbers (n + 1) used_ns nts accum
        else
          assign_numbers (n + 1) used_ns nts' ((Some n, t) :: accum)
    | (Some (Some n'), t) :: nts' ->
        assign_numbers n used_ns nts' ((Some n', t) :: accum)
  in

  let used_ns =
    List.split nts
    |> fst
    |> List.filter (function Some (Some _) -> true | _ -> false)
    |> List.map (function Some (Some n) -> n | _ -> assert false)
  in
  assign_numbers 0 used_ns nts []


let default_cmd interact_state : Interact_parser_base.ast option =
  let cmd =
    match List.hd interact_state.interact_nodes with
    | node ->
      let transitions = sorted_filtered_transitions node in
      let transitions =
        if interact_state.options.storage_first then
          match List.filter (fun (_, t) -> ConcModel.is_storage_trans t) transitions with
          | []  -> transitions
          | sts -> sts
        else
          transitions
      in
      let transitions =
          if interact_state.options.pseudorandom
            && transitions <> [] then
            let i = Random.int (List.length transitions) in
            [List.nth transitions i]
          else
            transitions
        in
        begin match List.hd transitions with
        | (i, _) -> Some (Interact_parser_base.Transition (Interact_parser_base.WithEager i))
        | exception Failure _ -> None
        end
    | exception Failure _ -> None
  in

  (* When following commands that use the default_cmd we have to take
  special care not to end up in an infinite loop *)
  match interact_state.follow_suffix with
  | []
  | Interact_parser_base.Default :: _
  | Interact_parser_base.Step None :: _
  | Interact_parser_base.Auto :: _
      -> cmd
  | Interact_parser_base.Step (Some i) :: _ when i >= 1 -> cmd
  | Interact_parser_base.Follow :: _ ->
      Interact_parser_base.pp Interact_parser_base.Follow
      |> SO.strLine "Cannot follow the '%s' command"
      |> Screen.show_warning interact_state.ppmode;
      None
  | ast :: _ -> Some ast

let rec update_default_cmd interact_state =
  let interact_state = {interact_state with default_cmd = default_cmd interact_state} in

  (* When following commands that use the default_cmd we have to take
  special care not to end up in an infinite loop *)
  match (interact_state.follow_suffix, interact_state.default_cmd) with
  | (Interact_parser_base.Default :: follow_suffix,   Some ast)
  | (Interact_parser_base.Step None :: follow_suffix, Some ast) ->
      { interact_state with
        follow_suffix = ast :: follow_suffix;
      }

  | (Interact_parser_base.Step (Some i) :: follow_suffix, Some ast) when i > 1 ->
      { interact_state with
        follow_suffix = ast :: Interact_parser_base.Step (Some (i-1)) :: follow_suffix;
      }
  | (Interact_parser_base.Step (Some i) :: follow_suffix, Some ast) when i = 1 ->
      { interact_state with
        follow_suffix = ast :: follow_suffix;
      }

  | (Interact_parser_base.Auto :: follow_suffix, Some ast) ->
      { interact_state with
        follow_suffix = ast :: Interact_parser_base.Auto :: follow_suffix;
      }
  | (Interact_parser_base.Auto :: follow_suffix, None) ->
      update_default_cmd {interact_state with follow_suffix = follow_suffix}

  | _ -> interact_state


let is_eager interact_state transition : bool =
  match List.hd interact_state.interact_nodes with
  | exception Failure _ -> assert false
  | node ->
      ConcModel.is_eager_trans
        node.system_state
        (* eager_local_mem is sound only in a fixed-point *)
        {interact_state.options.eager_mode with eager_local_mem = false}
        transition

let find_eager_transition interact_state =
  match List.hd interact_state.interact_nodes with
  | exception Failure _ -> assert false
  | node ->
      begin match
        List.mapi (fun n a -> (n, a)) node.filtered_transitions
        |> List.find (fun (_, (f, t)) -> f <> None && is_eager interact_state t)
      with
      | (n, (Some i, t))    -> Some (n, i, t)
      | (_, (None, _))      -> assert false
      | exception Not_found -> None
      end


(* return the indices of the transitions leading to the head of
interact_state.interact_nodes; head is the last transition *)
let choices_so_far interact_state : Interact_parser_base.ast list =
  List.fold_left
    (fun (cs, bound_eager) node ->
      let c = List.rev node.open_transition |> List.hd in
      let c =
        if bound_eager then
          Interact_parser_base.WithBoundedEager (c, (List.length node.open_transition) - 1)
        else
          Interact_parser_base.WithEager c
      in
      let c = Interact_parser_base.Transition c in
      let eager =
        find_eager_transition
          {interact_state with interact_nodes = [{node with open_transition = []}]}
        <> None
      in
      (c :: cs, eager)
    )
    ([], find_eager_transition interact_state <> None)
    (List.tl interact_state.interact_nodes)
  |> fst
  |> List.rev

let show_options interact_state : unit =
  let ppmode = interact_state.ppmode in
  let pp_maybe_int suffix maybe_int =
    match maybe_int with
    | None   -> "none"
    | Some n -> Printf.sprintf "%d%s" n suffix
  in
  let pp_maybe_ioid suffix maybe_ioid =
    match maybe_ioid with
    | None            -> "none"
    | Some (tid, iid) -> Printf.sprintf "(%d:%d)%s" tid iid suffix
  in

  SO.Concat [
    SO.strLine "Global options:";
    SO.strLine "  embedding = %s"
      (if interact_state.options.interpreter then "interpreter" else "shallow");
    SO.strLine "  loop_limit = %s"
      (pp_maybe_int "" (!Globals.model_params).t.thread_loop_unroll_limit);
  ]
  |> Screen.show_message ppmode;

  SO.Concat [
    SO.strLine "Eager mode:";
    SO.strLine "  fetch (single successor) = %b"
      interact_state.options.eager_mode.eager_fetch_single;
    SO.strLine "  fetch_new_branch = %b"
      interact_state.options.eager_mode.eager_fetch_multi;
    SO.strLine "  pseudocode_internal = %b"
      interact_state.options.eager_mode.eager_pseudocode_internal;
    SO.strLine "  constant_reg_read = %b"
      interact_state.options.eager_mode.eager_constant_reg_read;
    SO.strLine "  reg_rw = %b"
      interact_state.options.eager_mode.eager_reg_rw;
    SO.strLine "  memory_aux = %b"
      interact_state.options.eager_mode.eager_memory_aux;
    SO.strLine "  local_mem = %b"
      interact_state.options.eager_mode.eager_local_mem;
    SO.strLine "  finish = %b"
      interact_state.options.eager_mode.eager_finish;
    SO.strLine "  fp_recalc = %b"
      interact_state.options.eager_mode.eager_fp_recalc;
  ]
  |> Screen.show_message ppmode;

  SO.Concat [
    SO.strLine "Shared-memory (approximation for eager_local_mem):";
    SO.line @@ SO.encoded @@
      Pp.pp_shared_memory interact_state.ppmode interact_state.options.eager_mode.em_shared_memory;
  ]
  |> Screen.show_message ppmode;

  SO.Concat [
    SO.strLine "Stepping options:";
    SO.strLine "  random = %b" interact_state.options.pseudorandom;
    SO.strLine "  storage_first = %b" interact_state.options.storage_first;
    SO.strLine "  focused-thread = %s"
      (pp_maybe_int "" interact_state.options.focused_thread);
    SO.strLine "  focused-instruction = %s"
      (pp_maybe_ioid "" interact_state.options.focused_ioid);
  ]
  |> Screen.show_message ppmode;


  SO.Concat [
    SO.strLine "Interface options:";
    SO.strLine "  verbosity = %s"
      begin match !SO.verbosity with
      | SO.Quiet                  -> "quiet"
      | SO.Normal                 -> "normal"
      | SO.ThrottledInformation   -> "verbose"
      | SO.UnthrottledInformation -> "very"
      | SO.Debug                  -> "debug"
      end;
    SO.strLine "  pp_style = %s"
      (Globals.pp_ppstyle ppmode.Globals.pp_style);
    SO.strLine "  choice_history_limit = %s"
      (pp_maybe_int "" ppmode.Globals.pp_choice_history_limit);
    SO.strLine "  always_print = %b"
      interact_state.options.always_print;
    SO.strLine "  prefer_symbolic_values = %b"
      ppmode.Globals.pp_prefer_symbolic_values;
    SO.strLine "  pp_hide_pseudoregister_reads = %b"
      ppmode.Globals.pp_hide_pseudoregister_reads;
    SO.strLine "  pp_colours = %b" ppmode.Globals.pp_colours;
    SO.strLine "  condense_finished_instructions = %b"
      ppmode.Globals.pp_condense_finished_instructions;
    SO.strLine "  max_finished = %s"
      (pp_maybe_int "" ppmode.Globals.pp_max_finished);
    SO.strLine "  pp_hex = %b" !Globals.print_hex;
    SO.strLine "  dwarf_show_all_variable_locations = %b"
      !Globals.dwarf_show_all_variable_locations;
    SO.strLine "  pp_sail = %b" ppmode.Globals.pp_sail;
  ]
  |> Screen.show_message ppmode;

  SO.Concat [
    SO.strLine "Graph options:";
    SO.strLine "  always_graph = %b" (!Globals.run_dot = Some Globals.RD_step);
    SO.strLine "  ppg_shared = %b" ppmode.Globals.ppg_shared;
    SO.strLine "  ppg_rf = %b" ppmode.Globals.ppg_rf;
    SO.strLine "  ppg_fr = %b" ppmode.Globals.ppg_fr;
    SO.strLine "  ppg_co = %b" ppmode.Globals.ppg_co;
    SO.strLine "  ppg_addr = %b" ppmode.Globals.ppg_addr;
    SO.strLine "  ppg_data = %b" ppmode.Globals.ppg_data;
    SO.strLine "  ppg_ctrl = %b" ppmode.Globals.ppg_ctrl;
    SO.strLine "  ppg_ppg_regs = %b" ppmode.Globals.ppg_regs;
    SO.strLine "  ppg_reg_rf = %b" ppmode.Globals.ppg_reg_rf;
    SO.strLine "  ppg_trans = %b" ppmode.Globals.ppg_trans;
    SO.strLine "  graph_backend = %s"
      (Globals.pp_graph_backend !Globals.graph_backend);
  ]
  |> Screen.show_message ppmode;


  SO.Concat [
    SO.strLine "Search options:";
    SO.strLine "  hash_prune = %b" interact_state.options.hash_prune;
    SO.strLine "  partial_order_reduction = %b"
      interact_state.options.partial_order_reduction;
    SO.strLine "  priority_reduction = %b"
      interact_state.options.priority_reduction;
    SO.strLine "  allow_partial = %b" interact_state.options.allow_partial;
    SO.strLine "  transition-limit = %s"
      (pp_maybe_int "" interact_state.options.transition_limit);
    SO.strLine "  trace-limit = %s"
      (pp_maybe_int "" interact_state.options.trace_limit);
    SO.strLine "  time-limit = %s"
      (pp_maybe_int "s" interact_state.options.time_limit);
  ]
  |> Screen.show_message ppmode;

  SO.Concat [
    SO.strLine "Branch-targets:";
    SO.line @@ SO.encoded @@ begin
      (ConcModel.model_params (List.hd interact_state.interact_nodes).system_state).t.branch_targets
      |> Params.branch_targets_to_list
      |> Pp.pp_branch_targets interact_state.ppmode
      end
  ]
  |> Screen.show_message ppmode;

  SO.Concat [
    SO.strLine "Model options:";
    SO.strLine "  %s"
      (Model_aux.pp_model (ConcModel.model_params (List.hd interact_state.interact_nodes).system_state));
  ]
  |> Screen.show_message ppmode;

  SO.strLine "Version: %s" Versions.Rmem.describe
  |> Screen.show_message ppmode


let choice_summary
    ppmode
    choices_so_far
    follow_suffix
    numbered_cands
    disabled_trans
    verbose
    : SO.t
  =
  let n_choices = List.length choices_so_far in
  let limited_choices =
    match ppmode.Globals.pp_choice_history_limit with
    | Some n -> Lem_list.take n choices_so_far
    | None -> choices_so_far
  in

  SO.Concat [
    SO.line @@ SO.Concat [
      SO.str "Choices so far (%n): " n_choices;
      SO.ifTrue ((List.length limited_choices) < n_choices) @@
        SO.String "...";
      SO.String (Interact_parser_base.history_to_string limited_choices);
      SO.ifTrue (follow_suffix <> []) @@ begin
        List.rev follow_suffix
        |> Interact_parser_base.history_to_string
        |> SO.str " remaining follow-spec: [%s]"
        end;
    ];

    begin if numbered_cands = [] then
      SO.strLine "No enabled transitions"
    else if verbose then
      SO.Concat [
        SO.line @@ SO.strEmph "Enabled transitions:";
        SO.concat @@ List.map
          (fun cand -> SO.line @@ SO.encoded @@ ConcModel.pp_cand ppmode cand)
          numbered_cands;
      ]
    else
      SO.strLine "%d enabled transitions" (List.length numbered_cands)
    end;

    begin if disabled_trans = [] then
      SO.strLine "No disabled transitions"
    else if verbose then
      SO.Concat [
        SO.line @@ SO.strEmph "Disabled transitions:";
        SO.concat @@ List.map
          (fun t -> SO.line @@ SO.encoded @@ ConcModel.pp_trans ppmode t)
          disabled_trans;
      ]
    else
      SO.strLine "%d disabled transitions" (List.length disabled_trans)
    end;
  ]


let show_last_state inline interact_state : unit =
  let ppmode = interact_state.ppmode in
  begin match interact_state.interact_nodes with
  | [] ->
     SO.strLine "no state"
     |> Screen.show_message interact_state.ppmode

  | node :: ns ->
     let ts = sorted_filtered_transitions node in
     let (ppmode, ui_state) =
       match ns with
       | [] ->
          ConcModel.make_ui_state ppmode None node.system_state ts
       | node' :: _ ->
          ConcModel.make_ui_state ppmode (Some node'.system_state)
            node.system_state ts
     in
     let ppmode = {ppmode with Globals.pp_default_cmd = interact_state.default_cmd} in

     let trace = fun () ->
       SO.Encoded (ConcModel.pp_transition_history ppmode ui_state)
     in

     let choice_summary verbose : SO.t =
       choice_summary
         ppmode
         (choices_so_far interact_state)
         interact_state.follow_suffix
         ts
         (filtered_out_transitions node)
         verbose
     in

     let state = fun () ->
       SO.Concat [
           SO.ifTrue inline @@
             SO.Concat [SO.Line SO.Empty; SO.HorLine SO.Normal];
           SO.Encoded (ConcModel.pp_ui_state ppmode ui_state);
           SO.ifTrue (ppmode.Globals.pp_style <> Globals.Ppstyle_compact) @@
             SO.Concat [SO.Line SO.Empty; SO.HorLine SO.Normal];
           choice_summary (ppmode.Globals.pp_style <> Globals.Ppstyle_compact);
         ]
     in

     if inline then
       Screen.show_message ppmode (state ())
     else
       Screen.show_system_state ppmode trace (fun () -> choice_summary true) state
  end

(* map each transition to a pair of (bool, trans) where the bool is
true iff the transition is "interesting" *)
let filter_transitions options transitions =
  let transitions = List.map (fun t -> (true, t)) transitions in
  let transitions =
    match options.focused_thread with
    | Some n ->
        List.map
          (fun (b, t) -> (b && ConcModel.threadid_of_thread_trans t = Some n, t))
          transitions
    | None -> transitions
  in
  let transitions =
    match options.focused_ioid with
    | Some target_ioid ->
        List.map
          (fun (b, t) -> (b && ConcModel.ioid_of_thread_trans t = Some target_ioid, t))
          transitions
    | None -> transitions
  in
  (* more filters expected later *)
  transitions

let generate_transitions interact_state system_state =
  let transitions = filter_transitions
                      interact_state.options
                      (ConcModel.transitions system_state) in
  (* add numbers to the transitions; try to use the same number
  for transitions that were present in the previous state *)
  match interact_state.interact_nodes with
  | []      -> number_transitions [] transitions
  | n' :: _ -> number_transitions n'.filtered_transitions transitions

let regenerate_transitions interact_state : interact_state =
  begin match interact_state.interact_nodes with
  | [] -> assert false
  | n :: ns ->
      let n =
        { n with
          filtered_transitions =
            generate_transitions
              {interact_state with interact_nodes = ns}
              n.system_state;
        }
      in
      {interact_state with interact_nodes = n :: ns}
  end
  |> update_default_cmd

let add_interact_node interact_state system_state : interact_state =
  let new_node =
    { system_state         = system_state;
      open_transition      = [];
      filtered_transitions = generate_transitions interact_state system_state;
    }
  in
  { interact_state with
    interact_nodes = new_node :: interact_state.interact_nodes;
  } |> update_default_cmd


let change_model update_model interact_state : interact_state =
  Globals.model_params := update_model !Globals.model_params;
  match interact_state.interact_nodes with
  | []            -> interact_state (* nothing to change *)
  | node :: nodes ->
      let node' =
        {node with system_state = ConcModel.set_model_params !Globals.model_params node.system_state}
      in
      {interact_state with interact_nodes = node' :: nodes}
      |> regenerate_transitions


let update_branch_targets interact_state =
  change_model
    (fun model ->
      let branch_targets =
        (List.hd interact_state.interact_nodes).system_state
        |> ConcModel.branch_targets_of_state
        |> Params.union_and_diff_branch_targets model.t.branch_targets
        |> fst
      in
      {model with t = {model.t with branch_targets = branch_targets}}
    )
    interact_state


let update_shared_memory interact_state =
  let system_state = (List.hd interact_state.interact_nodes).system_state in
  let shared_memory =
    Params.union_and_diff_shared_memory
      (ConcModel.shared_memory_of_state system_state)
      interact_state.options.eager_mode.em_shared_memory
    |> fst
  in
  { interact_state with
    options =
      { interact_state.options with
        eager_mode =
          { interact_state.options.eager_mode with
            em_shared_memory = shared_memory;
          };
      };
  }

(* make sure to call update_shared_memory before calling check_breakpoints
if there is a SharedBreakpoint *)
let check_breakpoints interact_state =
  let node = List.hd interact_state.interact_nodes in
  let transitions = sorted_filtered_transitions node |> List.split |> snd in
  let shared_mem = interact_state.options.eager_mode.em_shared_memory in
  List.filter (function
    | (_, Runner.StateBreakpoint pred, _) ->
        pred node.system_state
    | (_, Runner.TransitionBreakpoint pred, _) ->
        List.exists (pred node.system_state) transitions
    | (_, Runner.SharedBreakpoint pred, _) ->
        List.exists (pred shared_mem node.system_state) transitions
    )
    interact_state.breakpoints


exception NoSuchTransition

let try_single_transition
    (eager:          bool)
    (n:              int)
    (interact_state: interact_state)
    : interact_state option
  =
  match interact_state.interact_nodes with
  | [] -> assert false
  | interact_node :: interact_nodes ->
      begin match List.find (function (Some n', _) -> n' = n | _ -> false) interact_node.filtered_transitions with
      | (_, transition) ->
          begin match ConcModel.state_after_trans
                  interact_node.system_state
                  transition
          with
          | BasicTypes.TO_unhandled_exception (tid, ioid, e) ->
              SO.strLine "the transition leads to exception"
              |> Screen.show_message interact_state.ppmode;
              (* TODO: pp the exception *)
              None
          | BasicTypes.TO_system_state system_state' ->
              let interact_nodes =
                if eager then
                  match interact_nodes with
                  | [] -> [] (* This is a special case where the eager
                  transition is taken from the initial state *)
                  (* TODO: We loose the real initial state and replace it
                  with the state after eager transitions. Find a way to
                  record the initial state. *)
                  | interact_node' :: interact_nodes' ->
                      { interact_node' with open_transition = n :: interact_node'.open_transition } :: interact_nodes'
                else begin
                  assert (interact_node.open_transition = []);
                  { interact_node with open_transition = [n] } :: interact_nodes
                end
              in
              let interact_state =
                {interact_state with interact_nodes = interact_nodes}
              in
              Some (add_interact_node interact_state system_state')
          end

      | exception Not_found -> raise NoSuchTransition
      end

type eager_constraints =
  | NoConstraints (* take all the followig eager transitions *)
  | Sequence of int list (* take following eager transitions as long as they match the list *)
  | Bound of int (* take 'n' following eager transitions *)

exception BreakpointHit of interact_state * (Runner.breakpoint list)

(* do_transition raises a BreakpointHit exception when a breakpoint
is reached. This allows us to stop sequences of transition (e.g. the
'auto' command) *)
let rec do_transition
    (eager:             bool) (* true iff the transition is eager *)
    (n:                 int)  (* the UI number of the transition *)
    (eager_constraints: eager_constraints) (* used by follow_search_trace *)
    (interact_state:    interact_state)
    : eager_constraints * interact_state
  =
  match try_single_transition eager n interact_state with
  | Some interact_state ->
      let interact_state =
        if List.exists (function (_,Runner.SharedBreakpoint _, _) -> true | _ -> false) interact_state.breakpoints then
          update_shared_memory interact_state
        else
          interact_state
      in
      begin match check_breakpoints interact_state with
      | [] -> check_eager eager_constraints interact_state
      | bps -> raise (BreakpointHit (interact_state, bps))
      end
  | None ->
      assert (eager_constraints = NoConstraints);
      (eager_constraints, interact_state)
and check_eager
    (eager_constraints: eager_constraints)
    (interact_state:    interact_state )
    : eager_constraints * interact_state
  =
  match eager_constraints with
  | Bound b when b <= 0 -> (eager_constraints, interact_state)
  | Sequence []         -> (eager_constraints, interact_state)
  | _ ->
      begin match find_eager_transition interact_state with
      | Some (n, i, t) ->
          let take_transition eager_constraints =
            Screen.show_debug interact_state.ppmode (fun () ->
              ConcModel.pp_trans interact_state.ppmode t
              |> SO.strLine "+ check_eager taking [%d] %s" i
            );
            do_transition true i eager_constraints interact_state
          in
          begin match eager_constraints with
          | Sequence (n' :: ns) when n = n' ->
              take_transition (Sequence ns)
          | Sequence _ (* n <> n' *) ->
              (* do not take the transition *)
              (eager_constraints, interact_state)
          | Bound b ->
              take_transition (Bound (b-1))
          | NoConstraints ->
              take_transition NoConstraints
          end
      | None -> (eager_constraints,  interact_state)
      end


let rec do_fetch_all interact_state : interact_state =
  let node = List.hd interact_state.interact_nodes in
  match
    List.find
      (fun (f, transition) ->
          f <> None &&
          (ConcModel.is_fetch_single_trans transition ||
              ((ConcModel.model_params node.system_state).t.thread_allow_tree_speculation &&
                ConcModel.is_fetch_multi_trans transition)))
      node.filtered_transitions
  with
  | (Some i, _) ->
      do_transition false i NoConstraints interact_state
      |> snd
      |> do_fetch_all
  | (None,   _) -> assert false
  | exception Not_found -> interact_state


let rec do_back count interact_state : interact_state =
  if count <= 0 then interact_state else
  match interact_state.interact_nodes with
  | [] ->
      SO.strLine "no state"
      |> Screen.show_warning interact_state.ppmode;
      interact_state
  | n :: [] ->
      SO.strLine "can't go back from the initial state"
      |> Screen.show_warning interact_state.ppmode;
      interact_state
  | n :: n' :: ns ->
      let follow_suffix =
        let c = List.hd (List.rev n'.open_transition) in
        let c =
          if find_eager_transition interact_state = None then
            Interact_parser_base.WithEager c
          else
            Interact_parser_base.WithBoundedEager (c, (List.length n'.open_transition) - 1)
        in
        Interact_parser_base.Transition c :: interact_state.follow_suffix
      in

      { interact_state with
        interact_nodes = {n' with open_transition = []} :: ns;
        follow_suffix  = follow_suffix;
      }
      (* change_model also refilters the transitions, which we need
      because options might have changed. *)
      |> change_model (fun _ -> ConcModel.model_params n.system_state)
      |> update_default_cmd
      |> do_back (count - 1)


exception TraceRecon of string

(* follow_search_trace: we assume an interactive eager transition
must also be a search eager transition *)
let follow_search_trace
    (search_trace:   Runner.trace) (* head is the last transition *)
    (interact_state: interact_state)
    : interact_state
  =
  let rec follow interact_state = function
  | [] -> interact_state
  | i :: flat_trace ->
      let node = List.hd interact_state.interact_nodes in
      begin match List.nth node.filtered_transitions i with
      | (Some i', t) ->
          begin match do_transition false i' (Sequence flat_trace) interact_state with
          | (NoConstraints, _) -> assert false
          | (Bound _, _) -> assert false
          | (Sequence flat_trace, interact_state) ->
              follow interact_state flat_trace
          end

      | (None,    _) ->
          raise (TraceRecon (Printf.sprintf "bad transition number %d (transition is filtered out)" i))

      | exception Failure _ -> (* nth *)
          raise (TraceRecon (Printf.sprintf "bad transition number %d (too big)" i))
      end
  in
  List.rev search_trace
  |> List.map List.rev
  |> List.concat
  |> follow interact_state


let ui_choices_of_search_trace
    (interact_state: interact_state)
    (search_trace:   Runner.trace) (* head is the last transition *)
    : Interact_parser_base.ast list (* head is the last cmd *)
  =
  let choices =
    choices_so_far interact_state
    |> List.length
  in
  let choices' =
    follow_search_trace search_trace interact_state
    |> choices_so_far
  in
  Lem_list.take ((List.length choices') - choices) choices'


let print_observed_finals interact_state observed_finals : SO.t =
  let symtab =
    List.map
      (fun ((addr, _), s) -> (Test.C.interp_address_to_address addr, s))
      interact_state.ppmode.Globals.pp_symbol_table
  in

  let states_count_output = SO.strLine "States %i" (Runner.StateMap.cardinal observed_finals) in

  let states_output =
    let check_prop =
      match interact_state.test_info.Test.constr with
      | ConstrGen.ForallStates p
      | ConstrGen.ExistsState p
      | ConstrGen.NotExistsState p -> Test.C.check_prop p
    in

    SO.Concat
      (List.map
        (fun (state, (choices, count)) ->
          let ui_choices =
            match ui_choices_of_search_trace interact_state choices with
            | choices -> Interact_parser_base.history_to_string choices
            | exception (TraceRecon s) ->
                Printf.sprintf "could not reconstruct trace (%s)" s
          in
          SO.Line (SO.Concat [
            SO.strVerbose SO.Normal "%-6d%s>" count (if check_prop state then "*" else ":");
            SO.String (Test.C.pp_state symtab state);
            SO.verbose SO.Normal (fun () -> SO.Concat [
              SO.String " via ";
              SO.strClass SO.FollowList "%S" ui_choices;
            ]);
          ])
        )
        (Runner.StateMap.bindings observed_finals))
  in

  SO.Concat [states_count_output; states_output]

let print_observed_deadlocks interact_state observed_deadlocks : SO.t =
  match observed_deadlocks with
  | Some (deadlock_choices, deadlock_count) ->
      let via =
        match ui_choices_of_search_trace interact_state deadlock_choices with
        | choices ->
            SO.Concat [
              SO.String " via ";
              SO.strClass SO.FollowList "%S" (Interact_parser_base.history_to_string choices);
            ]
        | exception (TraceRecon s) ->
            SO.str "(could not reconstruct trace (%s))" s
      in

      SO.verbose SO.Normal @@ fun () -> SO.line @@
        SO.Concat [
          SO.str "Deadlock states %i" deadlock_count;
          SO.String " ";
          via;
        ]
  | None -> SO.Empty

let print_observed_exceptions interact_state observed_exceptions : SO.t =
  let exceptions_count_output = SO.verbose SO.Normal @@ fun () -> SO.strLine "Unhandled exceptions %i" (Runner.ExceptionMap.cardinal observed_exceptions) in

  let exceptions_output =
    SO.verbose SO.Normal @@ fun () -> SO.Concat
      (List.map
        (fun ((tid, ioid, exception_type), (choices, count)) ->
          let ui_choices =
            match ui_choices_of_search_trace interact_state choices with
            | choices -> Interact_parser_base.history_to_string choices
            | exception (TraceRecon s) ->
                Printf.sprintf "could not reconstruct trace (%s)" s
          in
          SO.line @@ SO.Concat [SO.str "%-6d:>" count;
                              SO.str "thread %d instruction %s: %s" tid (Pp.pp_pretty_ioid ioid) (Pp.pp_exception interact_state.ppmode ioid exception_type);
                              SO.Concat [
                                  SO.String " via ";
                                  SO.strClass SO.FollowList "%S" ui_choices;
                                ]
                             ]
        )
        (Runner.ExceptionMap.bindings observed_exceptions))
  in

  SO.Concat [exceptions_count_output; exceptions_output]

(* TODO FIXME: this should by all rights be in Pp but that's a circular dependency... *)
let pp_breakpoint ppmode (id, pred, desc) =
  let id_string = match id with
    | Runner.Numbered n   -> Printf.sprintf "#%d" n
    | Runner.Named (n, s) -> Printf.sprintf "#%d (\"%s\")" n s
  in
  let type_string = match pred with
    | Runner.StateBreakpoint _ -> Printf.sprintf "(reached state) %s" desc
    | Runner.TransitionBreakpoint _
    | Runner.SharedBreakpoint _ ->
        Printf.sprintf "(transition is enabled) %s" desc
  in
  String.concat " " [id_string; type_string]

let set_follow_list_from_search_trace
    (search_trace:   Runner.trace) (* head is the last transition *)
    (interact_state: interact_state)
    : interact_state
  =
  match ui_choices_of_search_trace interact_state search_trace with
  | choices ->
      SO.strLine "Follow-list was set"
      |> Screen.show_message interact_state.ppmode;
      {interact_state with follow_suffix = List.rev choices}
      |> update_default_cmd
  | exception (TraceRecon s) ->
      SO.strLine "could not reconstruct trace (%s)" s
      |> Screen.show_warning interact_state.ppmode;
      interact_state

let set_follow_list_from_observations observed_finals interact_state : interact_state =
  let check_prop state =
    match interact_state.test_info.Test.constr with
    | ConstrGen.ForallStates p
    | ConstrGen.ExistsState p
    | ConstrGen.NotExistsState p -> Test.C.check_prop p state
  in

  let observed_finals = Runner.StateMap.bindings observed_finals in
  match List.find (fun (state, _) -> check_prop state) observed_finals with
  | (_, (choices, _))   -> set_follow_list_from_search_trace choices interact_state
  | exception Not_found ->
      (* If none of the observations match the condition, use the first one *)
      begin match observed_finals with
      | (_, (choices, _)) :: _ -> set_follow_list_from_search_trace choices interact_state
      | [] -> interact_state
      end


let update_bt_and_sm search_state interact_state : interact_state =
  let (sm_union, _) =
    Params.union_and_diff_shared_memory
      search_state.Runner.observed_shared_memory
      interact_state.options.eager_mode.em_shared_memory
  in

  { interact_state with
    options =
      { interact_state.options with
        eager_mode =
          { interact_state.options.eager_mode with
            em_shared_memory = sm_union;
          };
      };
  }
  |> change_model (fun model ->
      let (bt_union, _) =
        Params.union_and_diff_branch_targets
          search_state.Runner.observed_branch_targets
          model.t.branch_targets
      in
      {model with t = {model.t with branch_targets = bt_union}}
  )

let run_interactive_search mode interact_state breakpoints bounds targets filters handle_search_outcome : interact_state =
  let interact_state = update_branch_targets interact_state in
  let interact_state =
    if interact_state.options.eager_mode.eager_local_mem then
      update_shared_memory interact_state
    else interact_state
  in

  let search_options =
    (* FIXME: the UI should allow eager_mode for search to be set
    separately from the interactive eger_mode. For now, the eager_mode
    for search is (almost) everything eager.
    For reconstruction of the search trace, interactive eager transition
    must also be a search eager transition (but not necessarily vice versa) *)
    match mode with
    | Interact_parser_base.Exhaustive ->
        { interact_state.options with
          pseudorandom = false;
          eager_mode   = eager_mode_all_on interact_state.options.eager_mode;
        }
    | Interact_parser_base.Random i ->
        { interact_state.options with
          pseudorandom        = true;
          pseudorandom_traces = i;
          eager_mode          = eager_mode_all_on interact_state.options.eager_mode;
        }
  in

  let interact_filters =
    let filter t =
      filter_transitions interact_state.options [t]
      |> List.hd
      |> fst
    in
    [filter]
  in

  match
    Runner.search_from_state
      interact_state.ppmode
      search_options
      interact_state.test_info
      (List.hd interact_state.interact_nodes).system_state
      (breakpoints @ interact_state.breakpoints)
      (* TODO: bounds, targets from interact_state *)
      bounds
      targets
      (filters @ interact_filters)
      (fun _ -> ()) (* TODO: print_partial_results? *)
  with
  | outcome -> handle_search_outcome interact_state outcome
  | exception (Runner.BadSearchOptions msg) ->
      Screen.show_warning interact_state.ppmode (SO.strLine "%s" msg);
      interact_state

let handle_search_outcome interact_state search_outcome : interact_state =
  match search_outcome with
  | Runner.Complete search_state' ->
      (* search terminated after exploring all reachable states *)
      let interact_state = update_bt_and_sm search_state' interact_state in

      SO.aclass SO.Final @@ SO.Concat [
        print_observed_finals interact_state search_state'.Runner.observed_filterred_finals;
        print_observed_deadlocks interact_state search_state'.Runner.observed_deadlocks;
        print_observed_exceptions interact_state search_state'.Runner.observed_exceptions;
      ]
      |> Screen.show_message interact_state.ppmode;

      set_follow_list_from_observations search_state'.Runner.observed_filterred_finals interact_state

  | Runner.Breakpoints (search_state', _) ->
      let interact_state = update_bt_and_sm search_state' interact_state in
      let search_trace = Runner.choices_so_far search_state' in
      (* We expect follow_search_trace to raise BreakpointHit, if it does not
      then it is because the breakpoint the search hit is one that was added
      just for this search (i.e. it is not from interact_state.breakpoints) *)
      begin try follow_search_trace search_trace {interact_state with follow_suffix = []} with
      | TraceRecon s ->
          SO.strLine "Problem reconstructing trace: %s" s
          |> Screen.show_warning interact_state.ppmode;
          interact_state
      end

  | Runner.Interrupted (search_state', reason) ->
      let interact_state = update_bt_and_sm search_state' interact_state in

      SO.aclass SO.Final @@ SO.Concat [
        print_observed_finals interact_state search_state'.Runner.observed_filterred_finals;
        print_observed_deadlocks interact_state search_state'.Runner.observed_deadlocks;
        print_observed_exceptions interact_state search_state'.Runner.observed_exceptions;
      ]
      |> Screen.show_message interact_state.ppmode;

      SO.strLine "Interrupted: %s" reason;
      |> Screen.show_warning interact_state.ppmode;

      set_follow_list_from_observations search_state'.Runner.observed_filterred_finals interact_state

  | Runner.OcamlExn (search_state', msg) ->
      SO.aclass SO.Final @@ SO.Concat [
        SO.aclass SO.Warning @@ SO.Concat [
          SO.strLine "%s" msg;
          SO.Line SO.Empty;
          SO.strLine "***********************";
          SO.strLine "*** PARTIAL RESULTS ***";
          SO.strLine "***********************";
        ];
        print_observed_finals interact_state search_state'.Runner.observed_filterred_finals;
        print_observed_deadlocks interact_state search_state'.Runner.observed_deadlocks;
        print_observed_exceptions interact_state search_state'.Runner.observed_exceptions;
        SO.line @@ SO.aclass SO.Warning @@ SO.strLine "*** Error ***";
      ]
      |> Screen.show_message interact_state.ppmode;

      (* I'm not sure if it is safe to continue from this point.
      Maybe we should 'exit 1' here? *)
      interact_state
      |> set_follow_list_from_search_trace (Runner.choices_so_far search_state')

let do_search_final final mode interact_state : interact_state =
  let final_pred = fun state ->
    let test_info = interact_state.test_info in
    (ConcModel.transitions state) = [] &&
    begin match test_info.Test.filter with
    | None -> true
    | Some filter ->
        Runner.reduced_final_state test_info.Test.filter_regs
          test_info.Test.filter_mem state
        |> Test.C.check_filter filter
    end &&
    let final_state =
      Runner.reduced_final_state
        test_info.Test.show_regs
        test_info.Test.show_mem state in
    match final with
    | Interact_parser_base.Any_final -> true
    | Interact_parser_base.Final_ok ->
        Test.C.check_constr test_info.Test.constr [final_state]
    | Interact_parser_base.Final_not_ok ->
        not (Test.C.check_constr test_info.Test.constr [final_state])
  in

  let final_desc =
    match final with
    | Interact_parser_base.Any_final    -> "final state"
    | Interact_parser_base.Final_ok     -> "final state ok"
    | Interact_parser_base.Final_not_ok -> "final state not ok"
  in

  run_interactive_search mode interact_state
    [(Runner.Numbered (-1), Runner.StateBreakpoint final_pred, final_desc)]
    [] [] []
    (fun interact_state outcome ->
        match outcome with
        | Runner.Complete search_state' ->
            (* search terminated after exploring all reachable states *)
            let interact_state = update_bt_and_sm search_state' interact_state in

            begin match final with
            | Interact_parser_base.Any_final ->
                SO.strLine "Did not find a final state"
            | Interact_parser_base.Final_ok ->
                SO.strLine "Did not find a final state where the assertion holds"
            | Interact_parser_base.Final_not_ok ->
                SO.strLine "Did not find a final state where the assertion does not hold"
            end
            |> Screen.show_message interact_state.ppmode;

            interact_state

        | outcome -> handle_search_outcome interact_state outcome
    )

let do_search mode interact_state : interact_state =
  run_interactive_search mode interact_state [] [] [] [] handle_search_outcome

let fresh_bp_id bps' =
  let numbers =
    List.map
      (fun (id, _, _) ->
        match id with Runner.Numbered n | Runner.Named (n, _) -> n)
      bps'
  in
  match numbers with
  | [] -> Runner.Numbered 1
  | numbers -> Runner.Numbered (1 + List.hd (List.sort (fun x y -> -(compare x y)) numbers))

let add_breakpoint pred desc interact_state : interact_state =
  { interact_state with
    breakpoints = (fresh_bp_id interact_state.breakpoints, pred, desc) :: interact_state.breakpoints;
  }

let pp_dwarf_line_of_addr m addr =
  match m.Globals.pp_dwarf_static with
  | Some ds ->
      begin match Pp.pp_dwarf_source_file_lines m ds true addr with
      | Some s -> Printf.sprintf " [%s]" s
      | None -> ""
      end
  | None -> ""

let big_range (x : Nat_big_num.num) (y : Nat_big_num.num) =
  let rec big_range' (x : Nat_big_num.num) (y : Nat_big_num.num) acc =
    if y < x then acc
    else big_range' x (Nat_big_num.sub y (Nat_big_num.of_int 1)) (y :: acc)
  in
  big_range' x (Nat_big_num.sub y (Nat_big_num.of_int 1)) []


let do_add_breakpoint_fetch target state : interact_state =
  let add_breakpoint_fetch addr' size =
    let addr = Sail_impl_base.address_of_integer addr' in
    let addrs = List.map Sail_impl_base.address_of_integer (big_range addr' size) in
    let pred = fun state trans ->
      List.exists
        (fun addr -> ConcModel.trans_fetch_addr trans = Some addr)
        addrs
    in
    let desc =
      Printf.sprintf "fetches address 0x%s/%d%s"
        (Misc.big_num_to_hex_string (Sail_impl_base.integer_of_address addr))
        (Nat_big_num.to_int size)
        (pp_dwarf_line_of_addr state.ppmode addr)
    in
    add_breakpoint (Runner.TransitionBreakpoint pred) desc state
  in
  match target with
  | Interact_parser_base.Address a ->
      add_breakpoint_fetch a (Nat_big_num.of_int 1)
  | Interact_parser_base.Footprint (a, n) ->
      add_breakpoint_fetch a (Nat_big_num.of_int n)
  | Interact_parser_base.Symbol (name, offset) ->
      (* TODO FIXME: this probably isn't right (?)
      Sometimes symbols can have multiple values because of e.g. inlining... *)
      begin match List.assoc name state.test_info.Test.symbol_map with
      | (_, size, addr, _, _) ->
          add_breakpoint_fetch
              (Nat_big_num.add addr (Nat_big_num.of_int offset))
              size
      | exception Not_found ->
          SO.strLine "no such symbol"
          |> Screen.show_warning state.ppmode;
          state
      end


let do_add_watchpoint typ target state : interact_state =
  let add_watchpoint addr size =
    let addr = Sail_impl_base.address_of_integer addr in
    let pred = fun state trans ->
      match typ with
      | Interact_parser_base.Read   ->
          ConcModel.trans_reads_fp (addr, size) state trans
      | Interact_parser_base.Write  ->
          ConcModel.trans_writes_fp (addr, size) state trans
      | Interact_parser_base.Either ->
          ConcModel.trans_reads_fp (addr, size) state trans
          || ConcModel.trans_writes_fp (addr, size) state trans
    in
    let desc =
      match typ with
      | Interact_parser_base.Read ->
          Printf.sprintf "read from address 0x%x"
            (Nat_big_num.to_int (Sail_impl_base.integer_of_address addr))
      | Interact_parser_base.Write ->
          Printf.sprintf "write to address 0x%x"
            (Nat_big_num.to_int (Sail_impl_base.integer_of_address addr))
      | Interact_parser_base.Either ->
          Printf.sprintf "access address 0x%x"
            (Nat_big_num.to_int (Sail_impl_base.integer_of_address addr))
    in
    add_breakpoint (Runner.TransitionBreakpoint pred) desc state
  in
  match target with
  | Interact_parser_base.Address a ->
      add_watchpoint a 1
  | Interact_parser_base.Footprint (a, n) ->
      add_watchpoint a n
  | Interact_parser_base.Symbol (name, offset) ->
      (* TODO FIXME: this probably isn't right (?)
      Sometimes symbols can have multiple values because of e.g. inlining... *)
      begin match List.assoc name state.test_info.Test.symbol_map with
      | (_, size, addr, _, _) ->
          add_watchpoint
            (Nat_big_num.add addr (Nat_big_num.of_int offset))
            (Nat_big_num.to_int size)
      | exception Not_found ->
          SO.strLine "no such symbol"
          |> Screen.show_warning state.ppmode;
          state
      end


let do_add_shared_watchpoint typ state : interact_state =
  let fp_pred = match typ with
    | Interact_parser_base.Read ->
        ConcModel.trans_reads_fp
    | Interact_parser_base.Write ->
        ConcModel.trans_writes_fp
    | Interact_parser_base.Either ->
        (fun fp state trans ->
          ConcModel.trans_reads_fp fp state trans
          || ConcModel.trans_writes_fp fp state trans)
  in
  let pred fps state trans =
    Pset.exists (fun fp -> fp_pred fp state trans) fps
  in
  let desc =
    match typ with
    | Interact_parser_base.Read   -> "read from shared memory"
    | Interact_parser_base.Write  -> "write to shared memory"
    | Interact_parser_base.Either -> "access shared memory"
  in
  add_breakpoint (Runner.SharedBreakpoint pred) desc state


let make_graph interact_state =
  let node = List.hd interact_state.interact_nodes in
  let module G = (val get_graph_backend ()) in
  G.make_graph
    { interact_state.ppmode with
      Globals.pp_default_cmd = interact_state.default_cmd;
      Globals.pp_pretty_eiid_table = ConcModel.pretty_eiids node.system_state;
    }
    interact_state.test_info
    (ConcModel.make_cex_candidate node.system_state)
    (sorted_filtered_transitions node)

let typeset interact_state filename =
  match interact_state.interact_nodes with
  | node :: _ ->
      let ppmode' = {interact_state.ppmode with Globals.pp_kind = Globals.Latex} in
      let prev_state =
        match interact_state.interact_nodes with
        | _ :: node' :: _ -> Some node'.system_state
        | _ -> None
      in

      let (ppmode', ui_state) =
        ConcModel.make_ui_state ppmode' prev_state node.system_state (sorted_filtered_transitions node)
      in
      let fd = open_out filename in
      Printf.fprintf fd "%s\n" (ConcModel.pp_ui_state ppmode' ui_state);
      close_out fd;
      SO.strLine "wrote %s" filename
      |> Screen.show_message interact_state.ppmode

  | [] ->
      SO.strLine "could not typeset from no state"
      |> Screen.show_warning interact_state.ppmode


let do_add_breakpoint_line filename line interact_state : interact_state =
  let pred = fun state transition ->
    match ConcModel.trans_fetch_addr transition with
    | Some addr ->
        begin match interact_state.ppmode.Globals.pp_dwarf_static with
        | Some ds ->
               let lines = Dwarf.source_lines_of_address ds (Sail_impl_base.integer_of_address addr) in
               List.exists
                 (fun (filename', line', _) ->
                   filename' = filename && (Nat_big_num.to_int line') = line)
                 lines
        | None ->
           failwith "No DWARF static information (this should have been caught when attempting to add this breakpoint)"
        end
    | _ -> false
  in
  let desc = Printf.sprintf "reaches line %s:%d" filename line in
  match interact_state.ppmode.Globals.pp_dwarf_static with
  | Some _ ->
     add_breakpoint (Runner.TransitionBreakpoint pred) desc interact_state

  | None ->
      SO.strLine "No DWARF static information available, cannot break on line numbers."
      |> Screen.show_warning interact_state.ppmode;
      interact_state

let compare_breakpoints (id1, _, _) (id2, _, _) =
  let n1 = match id1 with Runner.Numbered n | Runner.Named (n, _) -> n in
  let n2 = match id2 with Runner.Numbered n | Runner.Named (n, _) -> n in
  compare n1 n2

let do_info_breakpoints interact_state : interact_state =
  begin match interact_state.breakpoints with
  | [] ->
      SO.strLine "No breakpoints defined."
      |> Screen.show_warning interact_state.ppmode
  | bps ->
      List.iter
        (fun bp ->
            SO.strLine "%s" (pp_breakpoint interact_state.ppmode bp)
            |> Screen.show_message interact_state.ppmode
        )
        (List.sort compare_breakpoints bps)
  end;
  interact_state

let do_delete_breakpoint n interact_state : interact_state =
  match List.find
    (fun (id, _, _) -> match id with | Runner.Numbered n' | Runner.Named (n', _) -> n = n')
    interact_state.breakpoints
  with
  | (id, _, _) ->
      { interact_state with
        breakpoints = List.filter (fun (id', _, _) -> id' <> id) interact_state.breakpoints
      }
  | exception Not_found ->
      SO.strLine "no such breakpoint %d" n
      |> Screen.show_warning interact_state.ppmode;
      interact_state


let do_step_instruction (maybe_thread_n : int option) (maybe_inst_n : int option) interact_state : interact_state =
  (* TODO FIXME: should we know about the internal structure of an ioid? *)
  let rec step ioid interact_state =
    let node = List.hd interact_state.interact_nodes in
    if ConcModel.is_ioid_finished ioid node.system_state then interact_state else
      begin match
        sorted_filtered_transitions node
        |> List.filter (fun (_, t) -> ConcModel.ioid_of_thread_trans t = Some ioid)
        |> List.hd
      with
      | (i, t) ->
          do_transition false i NoConstraints interact_state
          |> snd
          |> step ioid
      | exception (Failure _) -> (* List.hd *)
          SO.strLine "No more enabled transitions"
          |> Screen.show_warning interact_state.ppmode;
          interact_state
      end
  in

  let node = List.hd interact_state.interact_nodes in
  let transitions =
    sorted_filtered_transitions node
    |> List.map snd
  in
  match (maybe_thread_n, maybe_inst_n) with
  | (None, Some _) ->
      (* makes no sense *)
      assert false
  | (None, None) ->
      (* bare stepi, find lowest-numbered unfinished instruction from lowest-numbered thread *)
      let sorted_ioids =
        List.map ConcModel.ioid_of_thread_trans transitions
        |> List.filter (function Some _ -> true | _ -> false)
        |> List.map (function Some x -> x | _ -> assert false)
        |> List.sort compare
      in
      begin match List.hd sorted_ioids with
      | ioid ->
          SO.strLine "Stepping transitions of instruction %s" (Pp.pp_pretty_ioid ioid)
          |> Screen.show_message interact_state.ppmode;
          step ioid interact_state
      | exception (Failure _) ->
          SO.strLine "no available instructions to step"
          |> Screen.show_warning interact_state.ppmode;
          interact_state
      end
  | (Some tid, None) ->
      (* step thread, find lowest-numbered unfinished instruction in that thread *)
      let sorted_ioids =
        List.filter
           (fun t -> ConcModel.threadid_of_thread_trans t = Some tid)
           transitions
        |> List.map ConcModel.ioid_of_thread_trans
        |> List.filter (function Some _ -> true | _ -> false)
        |> List.map (function Some x -> x | _ -> assert false)
        |> List.sort compare
      in
      begin match List.hd sorted_ioids with
      | ioid ->
          SO.strLine "Stepping transitions of instruction %s" (Pp.pp_pretty_ioid ioid)
          |> Screen.show_message interact_state.ppmode;
          step ioid interact_state
      | exception (Failure _) ->
          SO.strLine "thread %d has no available instructions to step" tid
          |> Screen.show_warning interact_state.ppmode;
          interact_state
      end
  | (Some tid, Some iid) ->
      (* step specific instruction *)
      if (List.exists
            (fun t -> ConcModel.ioid_of_thread_trans t = Some (tid, iid))
            transitions)
      then
        step (tid, iid) interact_state
      else begin
        SO.strLine "instruction (%d:%d) has no transitions to step" tid iid
        |> Screen.show_warning interact_state.ppmode;
        interact_state
      end


let do_peek_instruction (tid : int) (inst_n : int)  interact_state : interact_state =
  (* TODO FIXME: should we know about the internal structure of an ioid? *)
  let ioid = (tid, inst_n) in
  let filter = fun t -> ConcModel.ioid_of_thread_trans t = Some ioid in
  let target = fun state ->
    ConcModel.transitions state
    |> List.filter filter
    |> (function [] -> true | _ -> false)
  in
  run_interactive_search
    Interact_parser_base.Exhaustive
    interact_state
    [] [] [target] [filter]
    (fun interact_state outcome ->
        match outcome with
        | Runner.Complete search_state' ->
            let node = List.hd interact_state.interact_nodes in
            let traces = Hashtbl.create (List.length search_state'.Runner.observed_targets) in
            List.iter
              (fun (trace, state) ->
                let trace =
                  match ui_choices_of_search_trace interact_state trace with
                  | trace -> Some trace
                  | exception (TraceRecon s) ->
                      SO.strLine "could not reconstruct trace (%s)" s
                      |> Screen.show_warning interact_state.ppmode;
                      None
                in
                let (ppmode, ui_state) =
                  ConcModel.make_ui_state
                    interact_state.ppmode
                    (Some node.system_state)
                    state
                    []
                in
                let inst = ConcModel.pp_instruction ppmode ui_state tid ioid in
                match (trace, Hashtbl.find traces inst) with
                | (Some trace, Some trace') when
                  List.length trace < List.length trace' ->
                    Hashtbl.replace traces inst (Some trace)
                | (Some _, None) ->
                    Hashtbl.replace traces inst trace
                | _ -> ()
                | exception Not_found ->
                    Hashtbl.add traces inst trace
              )
              search_state'.Runner.observed_targets;
            Hashtbl.fold
              (fun inst trace acc ->
                (SO.Concat [
                  SO.Line (SO.Encoded inst);
                  begin match trace with
                  | Some trace ->
                      SO.line @@ SO.Concat [
                        SO.String "via ";
                        SO.strClass SO.FollowList "%S" @@
                          Interact_parser_base.history_to_string trace;
                      ]
                  | None ->
                      SO.strLine "(could not reconstruct trace)"
                  end
                ]) :: acc
              )
              traces
              []
            |> SO.concatWith (SO.HorLine SO.Normal)
            |> Screen.show_message interact_state.ppmode;

          update_bt_and_sm search_state' interact_state

        | outcome -> handle_search_outcome interact_state outcome
    )

let do_help ppmode (args: string list) : unit =
  if args = [] then
    Console_help.help_message (Some 35)
    |> Screen.show_message ppmode
  else

  match parse (String.concat " " args) with
  | ParserASTs asts ->
      List.iter (fun ast ->
        Console_help.help_cmd ast
        |> Screen.show_message ppmode
      ) asts
  | ParserError _ ->
      (* The above does not work for 'help break' and any other command
      with mandatory arguments. Below we handle those cases *)
      begin match args with
      | "peeki" :: _ ->
          Console_help.help_cmd (Interact_parser_base.PeekInstruction (0, 0))
      | "debug" :: _ ->
          Console_help.help_cmd (Interact_parser_base.Debug "")
      | "break" :: _
      | "breakpoint" :: _ ->
          Console_help.help_cmd (Interact_parser_base.BreakpointLine ("", 0))
      | "watch" :: _
      | "rwatch" :: _
      | "awatch" :: _ ->
          Console_help.help_cmd (Interact_parser_base.Watchpoint (Interact_parser_base.Write, Interact_parser_base.Symbol ("", 0)))
      | "set" :: _ ->
          Console_help.help_cmd (Interact_parser_base.SetOption ("", []))
      | "search" :: _ ->
          Console_help.help_cmd (Interact_parser_base.Search (Interact_parser_base.Exhaustive, None))
      | "info" :: _ ->
          Console_help.help_cmd (Interact_parser_base.InfoBreakpoints)
      | "delete" :: _ ->
          Console_help.help_cmd (Interact_parser_base.DeleteBreakpoint 0)
      | "focus" :: _ ->
          Console_help.help_cmd (Interact_parser_base.FocusThread None)

      (* Categories *)
      | "stepping" :: _ ->
          Console_help.help_stepping None
      | "searching" :: _ ->
          Console_help.help_searching None
      | "breakpoints" :: _ ->
          Console_help.help_breakpoints None
      | "eager" :: _ ->
          Console_help.help_eager None
      | "interface" :: _ ->
          Console_help.help_interface None
      | "graphs" :: _ ->
          Console_help.help_graphs None
      | "misc" :: _ ->
          Console_help.help_misc None

      | _ ->
          SO.strLine "this command is not recognised"
      end
      |> Screen.show_message ppmode


exception InvalidKey
exception InvalidNumberOfArgs of int (* the int is the expected number *)
exception InvalidValue of string


let do_set key args interact_state =
  let ensure_one_arg () =
    match args with
    | a :: [] -> a
    | _ -> raise (InvalidNumberOfArgs 1)
  in

  let value_parser values = fun s ->
    try List.assoc s values with
    | Not_found -> raise (InvalidValue s)
  in

  let parse_bool s =
    value_parser [
      ("on", true);  ("true", true);  ("t",true);  ("yes",true);  ("y",true);  ("1",true);
      ("off",false); ("false",false); ("f",false); ("no", false); ("n",false); ("0",false);
    ] s
  in

  let parse_int s =
    try int_of_string s with
    | Failure _ -> raise (InvalidValue s)
  in

  let parse_int_option s =
    if s = "none" then None
    else try Some (int_of_string s) with Failure _ -> raise (InvalidValue s)
  in

  let open Lens.Infix in
  let open Globals in
  let open RunOptions in

  match key with
  | s when MlUtils.string_startswith "eager_" s ->
      let lens =
        match MlUtils.string_drop (String.length "eager_") s with
        | "fetch_single"        -> eager_fetch_single_lens
        | "fetch_multi"         -> eager_fetch_multi_lens
        | "pseudocode_internal" -> eager_pseudocode_internal_lens
        | "constant_reg_read"   -> eager_constant_reg_read_lens
        | "reg_rw"              -> eager_reg_rw_lens
        | "memory_aux"          -> eager_memory_aux_lens
        | "finish"              -> eager_finish_lens
        | "fp_recalc"           -> eager_fp_recalc_lens
        | "thread_start"        -> eager_thread_start_lens
        | "local_mem"           -> eager_local_mem_lens
        | _                     -> raise InvalidKey
      in
      let value = ensure_one_arg () in
      interact_state
      |> (run_options_lens |-- eager_mode_lens |-- lens) ^= (parse_bool value)
      |> check_eager NoConstraints
      |> snd

  | "eager" ->
      let value = ensure_one_arg () in
      let eager_mode =
        if parse_bool value then
          RunOptions.eager_mode_all_on interact_state.options.eager_mode
        else
          RunOptions.eager_mode_all_off interact_state.options.eager_mode
      in
      interact_state
      |> (run_options_lens |-- eager_mode_lens) ^= eager_mode
      |> check_eager NoConstraints
      |> snd

  | "random" ->
      let value = ensure_one_arg () in
      interact_state
      |> (run_options_lens |-- pseudorandom_lens) ^= (parse_bool value)
      |> update_default_cmd

  | "storage_first" ->
      let value = ensure_one_arg () in
      interact_state
      |> (run_options_lens |-- storage_first_lens) ^= (parse_bool value)
      |> update_default_cmd

  | "always_print"            -> ((run_options_lens |-- always_print_lens)            ^= (parse_bool (ensure_one_arg ()))) interact_state
  | "compare_analyses"        -> ((run_options_lens |-- compare_analyses_lens)        ^= (parse_bool (ensure_one_arg ()))) interact_state
  | "hash_prune"              -> ((run_options_lens |-- hash_prune_lens)              ^= (parse_bool (ensure_one_arg ()))) interact_state
  | "partial_order_reduction" -> ((run_options_lens |-- partial_order_reduction_lens) ^= (parse_bool (ensure_one_arg ()))) interact_state
  | "allow_partial"           -> ((run_options_lens |-- allow_partial_lens)           ^= (parse_bool (ensure_one_arg ()))) interact_state
  | "priority_reduction"      -> ((run_options_lens |-- priority_reduction_lens)      ^= (parse_bool (ensure_one_arg ()))) interact_state
  | "prune_restarts"          -> ((run_options_lens |-- prune_restarts_lens)          ^= (parse_bool (ensure_one_arg ()))) interact_state
  | "prune_discards"          -> ((run_options_lens |-- prune_discards_lens)          ^= (parse_bool (ensure_one_arg ()))) interact_state
  | "transition_limit"        -> ((run_options_lens |-- transition_limit_lens)        ^= (parse_int_option (ensure_one_arg ()))) interact_state
  | "trace_limit"             -> ((run_options_lens |-- trace_limit_lens)             ^= (parse_int_option (ensure_one_arg ()))) interact_state
  | "time_limit"              -> ((run_options_lens |-- time_limit_lens)              ^= (parse_int_option (ensure_one_arg ()))) interact_state

  | "ppg_shared"                     -> ((ppmode_lens |-- ppg_shared_lens) ^= (parse_bool (ensure_one_arg ()))) interact_state
  | "ppg_rf"                         -> ((ppmode_lens |-- ppg_rf_lens)     ^= (parse_bool (ensure_one_arg ()))) interact_state
  | "ppg_fr"                         -> ((ppmode_lens |-- ppg_fr_lens)     ^= (parse_bool (ensure_one_arg ()))) interact_state
  | "ppg_co"                         -> ((ppmode_lens |-- ppg_co_lens)     ^= (parse_bool (ensure_one_arg ()))) interact_state
  | "ppg_addr"                       -> ((ppmode_lens |-- ppg_addr_lens)   ^= (parse_bool (ensure_one_arg ()))) interact_state
  | "ppg_data"                       -> ((ppmode_lens |-- ppg_data_lens)   ^= (parse_bool (ensure_one_arg ()))) interact_state
  | "ppg_ctrl"                       -> ((ppmode_lens |-- ppg_ctrl_lens)   ^= (parse_bool (ensure_one_arg ()))) interact_state
  | "ppg_regs"                       -> ((ppmode_lens |-- ppg_regs_lens)   ^= (parse_bool (ensure_one_arg ()))) interact_state
  | "ppg_reg_rf"                     -> ((ppmode_lens |-- ppg_reg_rf_lens) ^= (parse_bool (ensure_one_arg ()))) interact_state
  | "ppg_trans"                      -> ((ppmode_lens |-- ppg_trans_lens)  ^= (parse_bool (ensure_one_arg ()))) interact_state

  | "prefer_symbolic_values"         -> ((ppmode_lens |-- pp_prefer_symbolic_values_lens)         ^= (parse_bool (ensure_one_arg ())))       interact_state
  | "hide_pseudoregister_reads"      -> ((ppmode_lens |-- pp_hide_pseudoregister_reads_lens)      ^= (parse_bool (ensure_one_arg ())))       interact_state
  | "max_finished"                   -> ((ppmode_lens |-- pp_max_finished_lens)                   ^= (parse_int_option (ensure_one_arg ()))) interact_state
  | "choice_history_limit"           -> ((ppmode_lens |-- pp_choice_history_limit_lens)           ^= (parse_int_option (ensure_one_arg ()))) interact_state
  | "condense_finished_instructions" -> ((ppmode_lens |-- pp_condense_finished_instructions_lens) ^= (parse_bool (ensure_one_arg ())))       interact_state
  | "pp_sail"                        -> ((ppmode_lens |-- pp_sail_lens)                           ^= (parse_bool (ensure_one_arg ())))       interact_state
  | "pp_style" ->
      let value = ensure_one_arg () in
      let value =
        value_parser [
          ("full",       Ppstyle_full);
          ("compact",    Ppstyle_compact);
          ("screenshot", Ppstyle_screenshot);
        ] value
      in
      ((ppmode_lens |-- pp_style_lens) ^= value) interact_state

  | "verbosity" ->
      let value = ensure_one_arg () in
      SO.verbosity := value_parser [
        ("quiet",   SO.Quiet);
        ("normal",  SO.Normal);
        ("verbose", SO.ThrottledInformation);
        ("very",    SO.UnthrottledInformation);
        ("debug",   SO.Debug);
      ] value;
      interact_state

  | "pp_hex" ->
      let value = ensure_one_arg () in
      Globals.print_hex := parse_bool value;
      interact_state

  | "dwarf_show_all_variable_locations" ->
      let value = ensure_one_arg () in
      Globals.dwarf_show_all_variable_locations := parse_bool value;
      interact_state

  | "graph_backend" ->
      let value = ensure_one_arg () in
      value_parser [("dot", ()); ("tikz", ())] value; (* this is just to check that 'value' is a valid value *)
      Globals.set_graph_backend value;
      interact_state

  | "random_seed" ->
      let value = ensure_one_arg () in
      Random.init (parse_int value);
      interact_state

  | "loop_limit" ->
      let value = ensure_one_arg () in
      change_model
        (fun model ->
          { model with
            t =
              { model.t with
                thread_loop_unroll_limit =
                  parse_int_option value;
              };
          }
        )
        interact_state

  | "always_graph" ->
      let value = ensure_one_arg () in
      if parse_bool value then
        Globals.run_dot := Some RD_step
      else
        Globals.run_dot := None;
      interact_state

  | "pp_colours" ->
      let value = ensure_one_arg () in
      let b = parse_bool value in
      Printing_functions.set_color_enabled b; (* propagate to sail interpreter pp *)
      ((ppmode_lens |-- pp_colours_lens) ^= b) interact_state

  | "follow_list" ->
      let value = ensure_one_arg () in
      if value = "" then
        update_default_cmd {interact_state with follow_suffix = []}
      else
        begin match parse value with
        | ParserError msg ->
            SO.strLine "bad follow list: %s" msg
            |> Screen.show_warning interact_state.ppmode;
            raise (InvalidValue value)
        | ParserASTs asts ->
            update_default_cmd {interact_state with follow_suffix = asts}
        end

  | "branch-targets" | "Branch-targets" ->
      let value = ensure_one_arg () in
      begin match Model_aux.branch_targets_parse_from_string value with
      | branch_targets ->
          change_model
            (Model_aux.set_branch_targets interact_state.test_info.Test.symbol_table branch_targets)
            interact_state
      | exception (Model_aux.BranchTargetsParsingError msg) ->
          SO.strLine "bad branch targets: %s" msg
          |> Screen.show_warning interact_state.ppmode;
          raise (InvalidValue value)
      end

  | "shared-memory" | "Shared-memory" ->
      let value = ensure_one_arg () in
      begin match Model_aux.shared_memory_parse_from_string value with
      | shared_memory ->
          let interact_state =
            change_model
              (Model_aux.set_shared_memory interact_state.test_info.Test.symbol_table shared_memory)
              interact_state
          in
          let shared_memory =
            match (ConcModel.model_params (List.hd interact_state.interact_nodes).system_state).shared_memory with
            | Some shared_memory -> shared_memory
            | None -> assert false
          in
          { interact_state with
            options =
              { interact_state.options with
                eager_mode =
                  { interact_state.options.eager_mode with
                    em_shared_memory = shared_memory;
                  };
              };
          }
      | exception (Model_aux.SharedMemoryParsingError msg) ->
          SO.strLine "bad shared memory: %s" msg
          |> Screen.show_warning interact_state.ppmode;
          raise (InvalidValue value)
      end

  | "state_output" ->
      ensure_one_arg ()
      |> Screen.set_state_output;
      interact_state

  | "trace_output" ->
      ensure_one_arg ()
      |> Screen.set_trace_output;
      interact_state

  | _ -> raise InvalidKey


exception DoCmdError of interact_state * string

let rec do_cmd
    (history:        bool) (* append ast to cmd_history *)
    (ast:            Interact_parser_base.ast)
    (interact_state: interact_state)
    : interact_state
  =
  (* Add the command to cmd_history *)
  let interact_state =
    if history then
      {interact_state with cmd_history = ast :: interact_state.cmd_history}
    else interact_state
  in

  (* Drop the head of follow_suffix *)
  let interact_state =
    match interact_state.follow_suffix with
    | Interact_parser_base.Transition _ :: _ ->
        (* This case is handled when we actually take the transition.
        If the transition cannot be taken (e.g. does not exist) we do
        not want it to be removed from the follow-list, so the user
        can see what is going on *)
        interact_state
    | ast' :: follow_suffix when ast' = ast ->
        {interact_state with follow_suffix = follow_suffix}
        |> update_default_cmd
    | _ -> interact_state
  in

  let ppmode = interact_state.ppmode in

  match ast with
  | Interact_parser_base.Quit ->
      Screen.quit ();
      assert false

  | Interact_parser_base.Help args ->
      do_help ppmode args;
      interact_state

  | Interact_parser_base.ShowOptions ->
      show_options interact_state;
      interact_state

  | Interact_parser_base.Debug s ->
     begin match s with
     | "transition_history" ->
        begin match interact_state.interact_nodes with
        | [] ->
            SO.strLine "(no interact_nodes)"
            |> Screen.show_message ppmode
        | nodes ->
           let n_places = List.length nodes |> float |> log10 |> truncate in
           List.iteri
            (fun i n ->
                SO.strLine "%*d: [%s]"
                    n_places i
                    (String.concat ", " (List.map string_of_int n.open_transition))
                |> Screen.show_message ppmode
            )
            nodes
        end
     | "follow_list" ->
        begin match interact_state.follow_suffix with
        | [] -> Screen.show_message ppmode (SO.strLine "(no follow list)")
        | fl ->
           let n_places = List.length fl |> float |> log10 |> truncate in
           List.iteri
              (fun i ast ->
                  SO.strLine "%*d: %s"
                      n_places i
                      (Interact_parser_base.pp ast)
                  |> Screen.show_message ppmode
              )
              fl
        end
     | _ ->
        SO.strLine "unknown debug command '%s'" s
        |> Screen.show_warning ppmode;
     end;
     interact_state

  | Interact_parser_base.Transition t ->
      let update_follow_suffix interact_state =
        match interact_state.follow_suffix with
        | [] -> interact_state
        | ast' :: follow_suffix when ast' = ast ->
            {interact_state with follow_suffix = follow_suffix}
            |> update_default_cmd
        | _ ->
            SO.strLine "Transition does not match the follow-list, removing the rest of the transitions from the follow-list"
            |> Screen.show_warning ppmode;
            {interact_state with follow_suffix = []}
            |> update_default_cmd
      in

      let (n, eager_constraints) =
        match t with
        | Interact_parser_base.WithEager n             -> (n, NoConstraints)
        | Interact_parser_base.WithBoundedEager (n, b) -> (n, Bound b)
      in

      begin try do_transition false n eager_constraints interact_state with
      | BreakpointHit (interact_state, bps) ->
          raise (BreakpointHit (update_follow_suffix interact_state, bps))
      | NoSuchTransition ->
          let msg =
            Printf.sprintf "current state does not have transition %d" n
          in
          raise (DoCmdError (interact_state, msg))
      end
      |> snd
      |> update_follow_suffix

  | Interact_parser_base.Default ->
      (* Currently the same as Step. *)
      do_cmd false (Interact_parser_base.Step None) interact_state

  | Interact_parser_base.Step None ->
      begin match interact_state.default_cmd with
      | None     -> interact_state
      | Some ast -> do_cmd false ast interact_state
      end

  | Interact_parser_base.Step (Some i) when i > 0 ->
      let rec do_step i interact_state =
        if i <= 0 then interact_state else
        begin match interact_state.default_cmd with
        | None   ->
            SO.strLine "can't step any more"
            |> Screen.show_warning ppmode;
            interact_state
        | Some ast ->
            do_cmd false ast interact_state
            |> do_step (i - 1)
        end
      in
      do_step i interact_state

  | Interact_parser_base.Step (Some i) (* when i <= 0 *) ->
      raise (DoCmdError (interact_state, "can't step a non-positive number (use back)"))

  | Interact_parser_base.Undo ->
      do_back 1 interact_state

  | Interact_parser_base.Back None ->
      do_back 1 interact_state

  | Interact_parser_base.Back (Some i) when i > 0 ->
      do_back i interact_state

  | Interact_parser_base.Back (Some i) (* when i <= 0 *) ->
      raise (DoCmdError (interact_state, "can't step back a non-positive number (use step)"))

  | Interact_parser_base.Follow ->
      if interact_state.follow_suffix = [] then
        SO.strLine "the follow-list is empty"
        |> Screen.show_warning ppmode;
      List.fold_left
        (fun interact_state ast -> do_cmd false ast interact_state)
        interact_state
        interact_state.follow_suffix

  | Interact_parser_base.Auto ->
      if interact_state.default_cmd = None then begin
          SO.strLine "there are no enabled transitions to auto-take"
          |> Screen.show_warning ppmode;
          interact_state
      end else
        let rec do_auto interact_state =
          match interact_state.default_cmd with
          | None -> interact_state
          | Some ast ->
              do_cmd false ast interact_state
              |> do_auto
        in
        do_auto interact_state

  | Interact_parser_base.Search (Interact_parser_base.Random i, _) when i < 1 ->
      raise (DoCmdError (interact_state, "the number of traces must be greater than 0"))

  | Interact_parser_base.Search (mode, None) ->
      do_search mode interact_state

  | Interact_parser_base.Search (mode, Some final) ->
      do_search_final final mode interact_state

  | Interact_parser_base.Typeset ->
      typeset interact_state "ui_snapshot.tex";
      interact_state

  | Interact_parser_base.Graph ->
      make_graph interact_state;
      interact_state

  | Interact_parser_base.Print ->
      show_last_state true interact_state;
      interact_state

  | Interact_parser_base.History ->
      Interact_parser_base.history_to_string interact_state.cmd_history
      |> SO.strLine "%s"
      |> Screen.show_message ppmode;
      interact_state

  | Interact_parser_base.FetchAll ->
      do_fetch_all interact_state

  | Interact_parser_base.BreakpointLine (filename, line) ->
      do_add_breakpoint_line filename line interact_state

  | Interact_parser_base.BreakpointFetch target ->
      do_add_breakpoint_fetch target interact_state

  | Interact_parser_base.Watchpoint (typ, target) ->
      do_add_watchpoint typ target interact_state

  | Interact_parser_base.SharedWatchpoint typ ->
      do_add_shared_watchpoint typ interact_state

  | Interact_parser_base.SetOption (key, values) ->
      begin try do_set key values interact_state with
      | InvalidKey ->
          let msg = Printf.sprintf "unknown set option '%s' (see 'help')" key in
          raise (DoCmdError (interact_state, msg))
      | InvalidNumberOfArgs n ->
          let msg =
            Printf.sprintf "'%s' takes %d arguments (see 'help')"
              (Interact_parser_base.pp (Interact_parser_base.SetOption (key, [])))
              n
          in
          raise (DoCmdError (interact_state, msg))
      | InvalidValue value ->
          let msg =
            Printf.sprintf "'%s' is not a valid value for '%s'"
              (Interact_parser_base.pp_args [value])
              (Interact_parser_base.pp_args [key])
          in
          raise (DoCmdError (interact_state, msg))
      end

  | Interact_parser_base.FocusThread maybe_thread ->
      { interact_state with
        options = {interact_state.options with focused_thread = maybe_thread};
      }
      |> regenerate_transitions

  | Interact_parser_base.FocusInstruction maybe_ioid ->
      { interact_state with
        options = {interact_state.options with focused_ioid = maybe_ioid};
      }
      |> regenerate_transitions

  | Interact_parser_base.StepInstruction (thread_n, inst_n) ->
      do_step_instruction thread_n inst_n interact_state

  | Interact_parser_base.PeekInstruction (thread_n, inst_n) ->
      do_peek_instruction thread_n inst_n interact_state

  | Interact_parser_base.InfoBreakpoints ->
      do_info_breakpoints interact_state

  | Interact_parser_base.DeleteBreakpoint n ->
      do_delete_breakpoint n interact_state


let make_prompt interact_state : SO.t =
  begin match interact_state.interact_nodes with
  | [] -> SO.Empty
  | node :: _ ->
      let options =
        [ SO.ifTrue interact_state.options.pseudorandom @@
            SO.String "random";
          SO.ifTrue interact_state.options.storage_first @@
            SO.String "storage-first";
          begin match interact_state.options.focused_thread with
          | Some i -> SO.str "focused on thread %d" i
          | None   -> SO.Empty
          end;
          begin match interact_state.options.focused_ioid with
          | Some (tid, iid) -> SO.str "focused on ioid (%d:%d)" tid iid
          | None            -> SO.Empty
          end;
        ]
        |> List.filter (function SO.Empty -> false | _ -> true)
        |> SO.concatWith (SO.String ", ")
      in

      SO.Concat [
        SO.str "Step %d (%d/%d finished, %d trns)"
          (List.length interact_state.interact_nodes)
          (ConcModel.number_finished_instructions node.system_state)
          (ConcModel.number_constructed_instructions node.system_state)
          (List.fold_left (+) 0 (List.map (fun n -> List.length n.open_transition) interact_state.interact_nodes));

        begin match interact_state.default_cmd with
        | None     -> SO.Empty
        | Some ast -> SO.str " Choose [%s]" (Interact_parser_base.pp ast)
        end;

        SO.ifTrue (options <> SO.Empty) @@ SO.Concat [
          SO.String " (";
          options;
          SO.String ")";
        ];
      ]
  end

let extract_options interact_state : Screen.options_state =
  let open Screen in
  { run_options  = interact_state.options;
    model_params = !Globals.model_params;
    ppmode       = interact_state.ppmode;
    pp_hex       = !Globals.print_hex;
    dwarf_show_all_variable_locations
                 = !Globals.dwarf_show_all_variable_locations;
    verbosity    = !SO.verbosity;
  }

let rec main_loop interact_state : unit =
  show_last_state false interact_state;
  if interact_state.options.always_print then
    show_last_state true interact_state;

  if !Globals.run_dot = Some Globals.RD_step then
     make_graph interact_state;

  Screen.prompt interact_state.ppmode
                (Some (extract_options interact_state))
                (make_prompt interact_state)
                interact_state.cmd_history
                (parse_and_loop interact_state)
and parse_and_loop interact_state = fun cmd ->
    match parse cmd with
    | ParserError s ->
        SO.strLine "%s" s
        |> Screen.show_warning interact_state.ppmode;
        main_loop interact_state
    | ParserASTs input_asts ->
        begin try
          List.fold_left
            (fun interact_state ast -> do_cmd true ast interact_state)
            interact_state
            input_asts
        with
        | DoCmdError (interact_state, msg) ->
            SO.strLine "%s" msg
            |> Screen.show_warning interact_state.ppmode;
            interact_state
        | BreakpointHit (interact_state, bps) ->
            List.map (pp_breakpoint interact_state.ppmode) bps
            |> List.map (SO.strLine "Breakpoint hit: %s")
            |> SO.concat
            |> Screen.show_message interact_state.ppmode;
            interact_state
        end
        |> main_loop


let initial_interact_state
    (options:       RunOptions.t)
    (ppmode:        Globals.ppmode)
    (test_info:     Test.info)
    (initial_state: ConcModel.state)
    : interact_state
  =
  let interact_state =
    { test_info      = test_info;
      options        = options;
      ppmode         = ppmode;
      interact_nodes = [];
      cmd_history    = [];
      follow_suffix  = !Globals.follow;
      default_cmd    = None;
      breakpoints    = [];
    }
  in
  add_interact_node interact_state initial_state
  |> check_eager NoConstraints
  |> snd


let run_interactive
    (options:        RunOptions.t)
    (ppmode:         Globals.ppmode)
    (test_info:      Test.info)
    (state_records:  Params.initial_state_record list)
    : unit
  =
  let interact_state =
    begin match state_records with
    | []      -> failwith "no initial state"
    | s :: [] -> s
    | s :: _  ->
        SO.strLine "given multiple initial states, using only the first one"
        |> Screen.show_warning ppmode;
        s
    end
    |> ConcModel.initial_state test_info.Test.ism options
    |> initial_interact_state options ppmode test_info
  in

  match !Globals.ui_commands with
  | None   -> main_loop interact_state
  | Some s ->
      begin match parse s with
      | ParserError err ->
          Printf.eprintf "Parse error in '-cmds ...': %s" err;
          exit 1
      | _ -> parse_and_loop interact_state s
      end


(** handle the '-interactive false' exhaustive/random search ********)

let print_observations interact_state search_state =
  (* These are the branch targets as they were assumed before the search *)
  let branch_targets_output =
    let branch_targets = (ConcModel.model_params (List.hd interact_state.interact_nodes).system_state).t.branch_targets in
    SO.ifTrue (not (Pmap.is_empty branch_targets)) @@
      SO.verbose SO.Normal @@ fun () ->
        SO.strLine "Branch-targets=%s"
          (Params.branch_targets_to_list branch_targets
          |> Pp.pp_branch_targets interact_state.ppmode)
  in

  (* This is the shared memory as it was approximated before the search *)
  let shared_memory_output =
    let shared_memory = interact_state.options.eager_mode.em_shared_memory in
    SO.ifTrue (not (Pset.is_empty shared_memory)) @@
      SO.verbose SO.Normal @@ fun () ->
        SO.strLine "Shared-memory=%s"
          (Pp.pp_shared_memory interact_state.ppmode shared_memory)
  in

  let states_output = print_observed_finals interact_state search_state.Runner.observed_filterred_finals in
  let deadlock_states_output = print_observed_deadlocks interact_state search_state.Runner.observed_deadlocks in
  let exceptions_output = print_observed_exceptions interact_state search_state.Runner.observed_exceptions in

  SO.Concat
    [ branch_targets_output;
      shared_memory_output;
      states_output;
      deadlock_states_output;
      exceptions_output;
    ]

let print_search_results interact_state search_state runtime : unit =
  if SO.is_verbosity_at_least SO.ThrottledInformation then
    Runner.print_search_counters true search_state;

  let test_name_output =
    SO.line @@ SO.strEmph "Test %s %s"
        interact_state.test_info.Test.name
        (ConstrGen.pp_kind (ConstrGen.kind_of interact_state.test_info.Test.constr))
  in

  let (constraint_holds, constraint_output) =
    let holds =
      Runner.StateMap.bindings search_state.Runner.observed_filterred_finals
      |> List.split
      |> fst
      |> Test.C.check_constr interact_state.test_info.Test.constr
    in
    if holds then (true, SO.strLine "Ok")
    else if ConstrGen.is_existential interact_state.test_info.Test.constr then
      (false, SO.Concat [SO.line @@ SO.verbose SO.ThrottledInformation @@ (fun () -> SO.strClass SO.Warning "%s: Existential constraint not satisfied!" interact_state.test_info.Test.name);
                SO.strLine "No (allowed not found)"])
    else (* universal failed *)
      (false, SO.Concat [SO.line @@ SO.verbose SO.ThrottledInformation @@ (fun () -> SO.strClass SO.Warning "%s: Universal constraint invalidated!" interact_state.test_info.Test.name);
                SO.strLine "No (forbidden found)"])
  in

  let symtab =
    List.map
      (fun ((addr, _), s) -> (Test.C.interp_address_to_address addr, s))
      interact_state.ppmode.Globals.pp_symbol_table
  in

  let condition_output =
    SO.strLine "Condition %s"
      (ConstrGen.constraints_to_string (Test.C.pp_atom symtab) interact_state.test_info.Test.constr)
  in

  let test_info_output =
    if !Globals.dont_tool then
    begin
      SO.Concat
        (List.map
          (fun (k, v) ->
            if Misc.string_eq k "Relax" then
              SO.strLine "Relax %s %s %s" interact_state.test_info.Test.name (if constraint_holds then "Ok" else "No") v
            else
              SO.strLine "%s=%s" k v)
          interact_state.test_info.Test.info)
    end
    else
    begin
      SO.Concat
        (Lem_list.mapMaybe
          (fun (k, v) ->
            if Misc.string_eq k "Hash" then
              Some (SO.strLine "%s=%s" k v)
            else None)
          interact_state.test_info.Test.info)
    end
  in

  let observation_output =
    let (matches, non_matches) =
      let prop =
        match interact_state.test_info.Test.constr with
        | ConstrGen.ForallStates p
        | ConstrGen.ExistsState p
        | ConstrGen.NotExistsState p -> p
      in
      Runner.StateMap.bindings search_state.Runner.observed_filterred_finals |> List.split |> fst |> List.partition (Test.C.check_prop prop)
    in

    SO.strLine "Observation %s %s %d %d%s%s"
      interact_state.test_info.Test.name
      (if matches = [] then "Never"
      else if non_matches = [] then "Always"
      else "Sometimes")
      (List.length matches)
      (List.length non_matches)
      (if search_state.Runner.observed_deadlocks <> None then " with deadlocks" else "")
      (if not (Runner.ExceptionMap.is_empty search_state.Runner.observed_exceptions) then " with unhandled exceptions" else "")
  in

  let runtime_output = SO.strLine "Runtime: %f sec" runtime in

  SO.aclass SO.Final @@ SO.Concat [
    test_name_output;
    print_observations interact_state search_state;
    constraint_output;
    condition_output;
    test_info_output;
    observation_output;
    runtime_output;
  ]
  |> Screen.show_message interact_state.ppmode


let run_search
    (options:        RunOptions.t)
    (ppmode:         Globals.ppmode)
    (test_info:      Test.info)
    (state_records:  Params.initial_state_record list)
    : unit
  =
  (* breakpoint predicates and handlers *)
  let breakpoints =
    let run_dot_final_pred = fun state ->
      (ConcModel.transitions state) = [] &&
      begin match test_info.Test.filter with
      | None -> true
      | Some filter ->
         Runner.reduced_final_state test_info.Test.filter_regs
           test_info.Test.filter_mem state
          |> Test.C.check_filter filter
      end &&
      let final_state =
        Runner.reduced_final_state
          test_info.Test.show_regs
          test_info.Test.show_mem state in
      match !Globals.run_dot with
      | Some Globals.RD_final -> true
      | Some Globals.RD_final_ok ->
          Test.C.check_constr test_info.Test.constr [final_state]
      | Some Globals.RD_final_not_ok ->
          not (Test.C.check_constr test_info.Test.constr [final_state])

      | Some Globals.RD_step
      | None -> assert false
    in

    let run_dot_final = fun state ->
      match ppmode.Globals.pp_kind with
      | Globals.Html -> assert false
      | _ ->
          if !Globals.graph_backend = Globals.Tikz then begin
            let symtab =
              List.map
                (fun ((a, sz), s) ->
                  (Nat_big_num.to_int64 (Sail_impl_base.integer_of_address a), s))
                ppmode.Globals.pp_symbol_table
            in
            let final_state = Runner.reduced_final_state test_info.Test.show_regs
              test_info.Test.show_mem state
            in
            Test.C.pp_state symtab final_state
            |> Tikz.make_final_state test_info
          end;

          let module G = (val get_graph_backend ()) in
          G.make_graph
            { ppmode with
              Globals.pp_default_cmd = None;
              Globals.pp_pretty_eiid_table = ConcModel.pretty_eiids state;
            }
            test_info
            (ConcModel.make_cex_candidate state)
            []
    in

    match !Globals.run_dot with
    | Some Globals.RD_final ->
        [ ((Runner.Numbered 0, Runner.StateBreakpoint run_dot_final_pred, "final (run dot)"), run_dot_final);
        ]
    | Some Globals.RD_final_ok ->
        [ ((Runner.Numbered 0, Runner.StateBreakpoint run_dot_final_pred, "final ok (run dot)"), run_dot_final);
        ]
    | Some Globals.RD_final_not_ok ->
        [ ((Runner.Numbered 0, Runner.StateBreakpoint run_dot_final_pred, "final not ok (run dot)"), run_dot_final);
        ]

    | Some Globals.RD_step
    | _ -> []
  in

  let targets = 
    if !Globals.print_cexs then
      let print_cex_pred state = (ConcModel.transitions state) = [] in
      [print_cex_pred]
    else []
  in

  let run_search_from interact_state : unit =
    let print_results partial = fun search_state ->
      if partial then
        SO.Concat [
          SO.Line SO.Empty;
          SO.strLine "***********************";
          SO.strLine "*** PARTIAL RESULTS ***";
          SO.strLine "***********************";
        ]
        |> Screen.show_warning ppmode;
      print_search_results
        (update_bt_and_sm search_state interact_state)
        search_state
        (Sys.time ())
    in

    (* let print_cexs search_state : unit = 
     *   let (_,acexs) =
     *     List.fold_left
     *       (fun (hashes, acexs) (_,s) -> 
     *         let hash = Runner.hash_of_system_state s in
     *         if Pset.mem hash hashes then (hashes,acexs)
     *         else
     *           let cex = ConcModel.make_cex_candidate s in
     *           let acex = CandidateExecution.acex_of_cex cex in
     *           (Pset.add hash hashes, acex :: acexs)
     *       )
     *       (Pset.empty String.compare, [])
     *       search_state.Runner.observed_targets 
     *   in
     *   Json_candidate_execution.print_acexs search_state.test_info acexs
     * in *)

    let print_cexs search_state : unit = 
      let acexs =
        List.map
          (fun (_,s) -> 
            let cex = ConcModel.make_cex_candidate s in
            CandidateExecution.acex_of_cex cex
          )
          search_state.Runner.observed_targets 
      in
      Json_candidate_execution.print_distinct_acexs
        search_state.Runner.test_info acexs
    in

    match
      Runner.search_from_state
        ppmode
        options
        test_info
        (List.hd interact_state.interact_nodes).system_state
        (List.map fst breakpoints)
        [] (* bounds *)
        targets (* targets *)
        [] (* filters *)
        (print_results true)
    with
    | Runner.Complete search_state' -> 
       if !Globals.print_cexs then print_cexs search_state'
       else print_results false search_state'

    | Runner.Breakpoints ({Runner.search_nodes = {Runner.system_state = sst} :: _}, bps) ->
        (* a breakpoint was triggered *)
        (* run the appropriate handlers *)
        List.iter
          (fun (id, _, _) ->
            match List.find (fun ((id', _, _), _) -> id' = id) breakpoints with
            | (_, handler) -> handler sst
            | exception Not_found -> assert false)
          bps

    | Runner.Breakpoints _ (* search_nodes = [] *) -> assert false

    | Runner.Interrupted (search_state, reason) ->
        SO.strLine "Interrupted: %s" reason
        |> Screen.show_warning ppmode;
        print_results true search_state

    | Runner.OcamlExn (search_state, msg) ->
        SO.strLine "%s" msg
        |> Screen.show_warning ppmode;
        print_results true search_state;
        SO.strLine "*** Error ***"
        |> Screen.show_warning ppmode;
        exit 1

    | exception (Runner.BadSearchOptions msg) ->
        SO.strLine "%s" msg
        |> Screen.show_warning ppmode;
        exit 1
  in

  state_records
  |> List.map (ConcModel.initial_state test_info.Test.ism options)
  |> List.map (initial_interact_state options ppmode test_info)
  |> List.iter run_search_from

end (* Make *)
