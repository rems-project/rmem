(*===============================================================================*)
(*                                                                               *)
(*                rmem executable model                                          *)
(*                =====================                                          *)
(*                                                                               *)
(*  This file is:                                                                *)
(*                                                                               *)
(*  Copyright Shaked Flur, University of Cambridge             2015-2018         *)
(*  Copyright Peter Sewell, University of Cambridge            2015-2017         *)
(*  Copyright Jon French, University of Cambridge              2017-2018         *)
(*  Copyright Christopher Pulte, University of Cambridge 2015, 2017-2018         *)
(*  Copyright Susmit Sarkar, University of St Andrews               2015         *)
(*                                                                               *)
(*  All rights reserved.                                                         *)
(*                                                                               *)
(*  It is part of the rmem tool, distributed under the 2-clause BSD licence in   *)
(*  LICENCE.txt.                                                                 *)
(*                                                                               *)
(*===============================================================================*)

open MachineDefUtils
open MachineDefTypes
open Types
open Model_aux

(*let exit_on_bad_fetch m c =
  begin match c with
  | TSS_fetch (tid, ioid, _, a, (FDO_illegal_fetch_address as fdo), _)
  | TSS_fetch (tid, ioid, _, a, ((FDO_decode_error _) as fdo), _) ->
      Screen.show_warning m "%sFetch from address %s %s"
            (Pp.pp_thread_trans_prefix m tid ioid)
            (Pp.pp_address m ioid a)
            (Pp.pp_fdo m fdo a);
      raise Globals.Test_proper_termination
  | _ -> ()
  end*)

let munge_to_ast trans_todo =
  List.map (fun i -> Interact_parser_base.Transitions [i]) trans_todo

let stop_interactive_auto_on_test_start_fetch m c =
  begin match c with
  | T_trans (T_sync (T_fetch tl, _)) ->
      if !Globals.breakpoint_actual then
      begin
        let (addr, _) = tl.tl_label in
        let test_start =
          try List.assoc (addr, 8) (* guess *) m.Globals.pp_symbol_table = "actual_test" with
          | Not_found -> false
        in

        if test_start then
        begin
          Globals.interactive_auto := false;
          Globals.breakpoint_actual := false
        end
      end

  | _ -> ()
  end

(* The info structure of state of search procedure *)
type ('ts,'ss) info =
  { testname            : string option;             (* stashed name of the test *)
    trans_todo          : int list;                  (* follow list as numbered choices *)
    choices_so_far      : int list;                  (* numbered choices made so far (head is last) *)
    trans_so_far        : ('ts,'ss) MachineDefTypes.trans list;            (* transitions made so far *)
    last_system_state   : ('ts,'ss) MachineDefTypes.system_state option; (* the preceding system state (one step up in search tree) *)
    last_numbered_cands : (int * ('ts,'ss) MachineDefTypes.trans) list;    (* the user-interface numbered transitions from before, for text ui *)
  }

type ('ts,'ss) interactive_input =
  | I_Choice of ('ts,'ss) info * int
  | I_Undo
  | I_Quit
  | I_RunOptionChanged of RunOptions.t * Globals.ppmode
  | I_InteractOptionChanged of ('ts,'ss) info
  | I_Async of ('ts,'ss) info

(* The state interaction manipulates *)
type ('ts,'ss) interaction_state =
  { numbered_cands : ('ts,'ss) MachineDefTypes.ui_trans list;  (* The list of numbered transitions *)
    make_graph     : unit -> unit;
    info           : ('ts,'ss) info;(* The info structure *)
  }

let display (_: string) (_: string) (_: bool) : unit = () (* for the web-interface *)

let typeset
    (ppmode:         Globals.ppmode)
    (ui_state:       ('ts,'ss) MachineDefTypes.ui_system_state)
    (file:           string)
    : unit
  =
  let fd = open_out file in
  Printf.fprintf fd "%s\n" (Pp.pp_ui_system_state {ppmode with Globals.pp_kind = Globals.Latex} ui_state);
  close_out fd

(* Take inp:interaction_state as input:
     numbered_cands : candidates enabled
     info  : previous info
   EITHER Produces an interaction_state as output:
     numbered_cands : candidates enabled by user interaction
     info  : updated info
   and feeds that to k_info continuation  (also call k_info if there are no enabled transitions)
   OR invokes k_undo continuation*)

let interact_with_user
    (run_options: RunOptions.t)
    (ppmode:      Globals.ppmode)
    (s:           ('ts,'ss) MachineDefTypes.system_state)
    (ui_state:    ('ts,'ss) MachineDefTypes.ui_system_state)
    (inp:         ('ts,'ss) interaction_state)
    (k_info:      ('ts,'ss) interaction_state -> unit)
    (k_undo:      RunOptions.t -> unit)
    (k_recalc:    RunOptions.t -> Globals.ppmode -> unit)
    : unit
  =
  Screen.clear_warnings ();

  if !Globals.run_dot = Some Globals.RD_step then inp.make_graph ();

  begin match inp.numbered_cands with
  | [] -> k_info inp

  | (first_n, _) :: _ ->
      let default_choice info =
        match (info.trans_todo) with
        | n :: _ -> n
        | []     -> first_n
      in

      let match_user_input info : string -> ('ts,'ss) interactive_input =
        begin function
        | "h" | "?" | "help" ->
            let message =
              (Printf.sprintf "Available commands:\n")
              ^ (Printf.sprintf "  next             (or return with empty command) take the next transition\n")
              ^ (Printf.sprintf "                   from the follow-list or the 0 transition.\n")
              ^ (Printf.sprintf "  <int-list>       (; separated list of transition numbers) take the first\n")
              ^ (Printf.sprintf "                   transition from the list and set the follow-list to the\n")
              ^ (Printf.sprintf "                   rest of the list.\n")
              ^ (Printf.sprintf "  u|undo           undo the last transitions\n")
            ^ (if !Globals.pp_kind != Globals.Html then
                (Printf.sprintf "  e|exhaustive     exhaustively try all the transitions from the current state\n")
              else "")
              ^ (Printf.sprintf "  E|eager          toggle eager-safe\n")
              ^ (Printf.sprintf "  EU|eager-unsafe  toggle eager-unsafe\n")
              ^ (Printf.sprintf "  f|follow         toggle auto-follow (automatically take the next transition\n")
              ^ (Printf.sprintf "                   from the follow-list).\n")
              ^ (Printf.sprintf "  r|random         toggle pseudorandom (automatically take a random transition)\n")
            (* FIXME: what does i|iauto supposed to do? maybe Globals.auto_follow? why are there two
            Globals variables for it? also notice Globals.breakpoint_actual *)
              ^ (Printf.sprintf "  i|iauto          take the 'next' transition autmatically until there are\n")
              ^ (Printf.sprintf "                   no more enabled transitions.\n")
            ^ (if !Globals.pp_kind != Globals.Html then
                (Printf.sprintf "  t|typeset        dump typeset tex\n")
              ^ (Printf.sprintf "  d|debug          toggle performance debugging\n")
              else "")
              ^ (Printf.sprintf "  q|quit           quit\n")
              ^ (Printf.sprintf "  x|exit           quit\n")
              ^ (Printf.sprintf "  h|help|?         show this message\n")
              ^ (Printf.sprintf "  redraw           redraw the screen\n")
            ^ (if !Globals.pp_kind != Globals.Html then
                (Printf.sprintf "  graph            generate graph\n")
              ^ (Printf.sprintf "  ppg_regs         toggle show registers in graphs\n")
              ^ (Printf.sprintf "  ppg_reg_rf       toggle show register rf edges in graphs\n")
              ^ (Printf.sprintf "  ppg_trans        toggle show transitions in graphs\n")
              else "")

              ^ (Printf.sprintf "\n")
              ^ (Printf.sprintf "Current run options:\n")
              ^ (Printf.sprintf "  eager mode: fetch (single successor)=%b fetch_new_branch=%b pseudocode_internal=%b constant_reg_read=%b reg_rw=%b memory_aux=%b finish=%b fp_recalc=%b"
                                run_options.RunOptions.eager_mode.eager_fetch_single
                                run_options.RunOptions.eager_mode.eager_fetch_multi
                                run_options.RunOptions.eager_mode.eager_pseudocode_internal
                                run_options.RunOptions.eager_mode.eager_constant_reg_read
                                run_options.RunOptions.eager_mode.eager_reg_rw
                                run_options.RunOptions.eager_mode.eager_memory_aux
                                run_options.RunOptions.eager_mode.eager_finish
                                run_options.RunOptions.eager_mode.eager_fp_recalc)
              ^ (Printf.sprintf "  auto-follow:  %B\n" !Globals.auto_follow)
              ^ (Printf.sprintf "  pseudorandom: %B\n" run_options.RunOptions.pseudorandom)
            ^ (if !Globals.pp_kind != Globals.Html then
                (Printf.sprintf "  debug:        %B\n" !Debug.debug)
              else "")

              ^ (Printf.sprintf "\n")
              ^ (Printf.sprintf "Model options: %s\n" (Model_aux.pp_model s.model))
              ^ (Printf.sprintf "\n")
              ^ (Printf.sprintf "Version: %s" Versions.Rmem.describe)
            in
            Screen.show_message ppmode "%s" message;
            I_Async info

        | "E" | "eager" ->
           let run_options' =
             { run_options with RunOptions.eager_mode =
                                  if run_options.RunOptions.eager_mode = RunOptions.eager_mode_all_on
                                  then RunOptions.eager_mode_all_off
                                  else RunOptions.eager_mode_all_on }
            in
            I_RunOptionChanged (run_options', ppmode)

        | "f" | "follow" ->
            Globals.auto_follow := not !Globals.auto_follow;
            Screen.show_message ppmode "auto-follow was set to '%B'" !Globals.auto_follow;
            I_InteractOptionChanged info

        | "r" | "random" ->
            let run_options' =
              {run_options with RunOptions.pseudorandom = not run_options.RunOptions.pseudorandom}
            in
            Screen.show_message ppmode "random was set to '%B'" run_options'.RunOptions.pseudorandom;
            I_RunOptionChanged (run_options', ppmode)

        | "graph" when !Globals.pp_kind != Globals.Html ->
            inp.make_graph ();
            Screen.show_message ppmode "saved graph";
            I_Async info

        | "redraw" ->
            Screen.draw_system_state ppmode info.choices_so_far (munge_to_ast info.trans_todo)
                s ui_state None inp.numbered_cands [];
            I_Async info

        | "e" | "exhaustive" when !Globals.pp_kind != Globals.Html ->
            I_RunOptionChanged ({run_options with RunOptions.interactive = false}, ppmode)

        | "i" | "iauto" ->
            Globals.interactive_auto := not !Globals.interactive_auto;
            Screen.show_message ppmode "auto-next was set to '%B'" !Globals.interactive_auto;
            I_InteractOptionChanged info

        | "d" | "debug" when !Globals.pp_kind != Globals.Html ->
            (* FIXME: the descriptio seems to refer to Deug.perfdebug *)
            Debug.debug := not (!Debug.debug);
            Screen.show_message ppmode "debug was set to '%B'" !Debug.debug;
            I_Async info

        | "u" | "undo"  ->
          begin match info.choices_so_far with
          | [] ->
              Screen.show_warning ppmode "cannot undo from the initial state";
              I_Async info
          | _ :: _ -> I_Undo
          end

        | "t" | "typeset" when !Globals.pp_kind != Globals.Html ->
          begin
            typeset ppmode ui_state "ui_snapshot.tex";
            Screen.show_message ppmode "created ui_snapshot.tex";
            I_Async info
          end

        | "q" | "quit" -> I_Quit
        | "x" | "exit" -> I_Quit

        (* default choice *)
        | "" | "next" ->
            let d = default_choice info in
            if List.mem_assoc d inp.numbered_cands then
              begin match info.trans_todo with
              | n :: ns ->
                  assert (n = d);
                  I_Choice ({info with trans_todo = ns}, d)
              | [] -> I_Choice (info, d)
              end
            else
              begin
                (* IMPORTANT: we don't reset trans_todo. This makes
                the UI fail again as long as the user does not
                explicitly enter a different transition, allowing
                us to reach the end of the follow path by holding
                the return without overshooting it. *)
                Screen.show_warning ppmode "there is no transition %d" d;
                I_Async info
              end

        | user_input ->
          let ns =
            let rec split str pos accum =
              if pos >= String.length str then List.rev accum else
              let next =
                try String.index_from str pos ';' with
                | Not_found -> String.length str
              in
              split str (next + 1) ((String.sub str pos (next - pos)) :: accum)
            in

            try List.map (fun s -> int_of_string @@ String.trim s) (split user_input 0 [])
            with Failure _ -> []
          in

          begin match ns with
          | n :: ns ->
              if List.mem_assoc n inp.numbered_cands then
                I_Choice ({info with trans_todo = ns}, n)
              else
              begin
                Screen.show_warning ppmode "there is no transition %d" n;
                I_Async info
              end
          | _ ->
              Screen.show_warning ppmode "syntax error: enter 'help' to see the available commands";
              I_Async info
          end
        end
      in

      let auto_internal (next: ('ts,'ss) info -> ('ts,'ss) interactive_input) :
            ('ts,'ss) info -> ('ts,'ss) interactive_input = fun info ->
        if !Globals.auto_internal then
          let internal_opt =
            Lem_list.list_find_opt
                (fun (_, t) -> MachineDefTransitionUtils.is_internal t)
                inp.numbered_cands
          in
          match internal_opt with
          | Some (n, _) -> I_Choice (info, n) (* FIXME: reset info.trans_todo to [] ? *)
          | None        -> next info
        else next info
      in

      let auto_follow (next: ('ts,'ss) info -> ('ts,'ss) interactive_input) : ('ts,'ss) info -> ('ts,'ss) interactive_input = fun info ->
        (* there's a follow-mode transition specified - use if legal, otherwise cancel follow-mode*)
        if !Globals.auto_follow then (* FIXME: auto_follow is never set to true *)
          match info.trans_todo with
          | n :: ns ->
              if List.mem_assoc n inp.numbered_cands then
                I_Choice ({info with trans_todo = ns}, n)
              else begin
                Screen.draw_system_state ppmode info.choices_so_far (munge_to_ast info.trans_todo)
                        s ui_state None inp.numbered_cands [];
                let m =
                  (Printf.sprintf "follow specification error: %d is not a legal transition number\n" n) ^
                  (Printf.sprintf "switching to interactive mode")
                in
                Screen.show_warning ppmode "%s" m;
                I_Async {info with trans_todo = []}
              end
          | [] -> next info
        else next info
      in

      let interactive_auto (next: ('ts,'ss) info -> ('ts,'ss) interactive_input) : ('ts,'ss) info -> ('ts,'ss) interactive_input = fun info ->
        if !Globals.interactive_auto then match_user_input info ""
        else next info
      in

      let user_choice : ('ts,'ss) info -> ('ts,'ss) interactive_input = fun info ->
        Screen.draw_system_state ppmode info.choices_so_far (munge_to_ast info.trans_todo)
                s ui_state None inp.numbered_cands [];
        I_Async info
      in

      let rec do_interaction : ('ts,'ss) interactive_input -> unit =
        begin function
        | I_InteractOptionChanged info ->
            do_interaction ((auto_internal @@ auto_follow @@ interactive_auto @@ user_choice) info)

        | I_Async info ->
           let prompt_str =
             Printf.sprintf "Step %d  (%d/%d finished)  Choose [%d]"
                  (1 + List.length info.choices_so_far)
                  (MachineDefSystem.count_instruction_instances_finished s)
                  (MachineDefSystem.count_instruction_instances_constructed s)
                            (default_choice info)
           in begin
           Screen.prompt ppmode None prompt_str [] (fun str -> do_interaction (match_user_input info str))
             end

        | I_Choice (info, n_choice) ->
            let c = try List.assoc n_choice inp.numbered_cands with
                    | Not_found -> assert false
            in
            stop_interactive_auto_on_test_start_fetch ppmode c;
            (*exit_on_bad_fetch ppmode c;*)
            k_info {info = info;
                    numbered_cands = [(n_choice, c)];
                    make_graph = fun () -> ();}

        | I_Undo -> k_undo run_options
        | I_Quit -> Screen.quit ()

        | I_RunOptionChanged (run_options, ppmode) ->
            k_recalc run_options ppmode
        end
      in

      do_interaction (I_InteractOptionChanged inp.info)
  end


let ask_quit_or_undo
    (undo:   unit -> unit)
    : unit
  =
  let ppmode = Globals.get_ppmode () in
  let rec input_handler () : unit =
    let prompt_str = "Undo last transition, to continue exploration, or quit [u/q]" in

    let match_user_input : string -> unit =
      begin function
      | "u" | "undo" -> undo ()
      | "q" | "quit"
      | "x" | "exit" -> Screen.quit ()
      | _ ->
          Screen.show_warning ppmode "syntax error, please input u/undo/q/quit/x/exit";
          input_handler ()
      end
    in

    Screen.prompt ppmode None prompt_str [] match_user_input
  in
  input_handler ()
