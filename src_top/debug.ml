(*===============================================================================*)
(*                                                                               *)
(*                rmem executable model                                          *)
(*                =====================                                          *)
(*                                                                               *)
(*  This file is:                                                                *)
(*                                                                               *)
(*  Copyright Shaked Flur, University of Cambridge        2015-2017              *)
(*  Copyright Christopher Pulte, University of Cambridge       2015              *)
(*  Copyright Peter Sewell, University of Cambridge      2014, 2016              *)
(*  Copyright Susmit Sarkar, University of St Andrews          2014              *)
(*                                                                               *)
(*  All rights reserved.                                                         *)
(*                                                                               *)
(*  It is part of the rmem tool, distributed under the 2-clause BSD licence in   *)
(*  LICENCE.txt.                                                                 *)
(*                                                                               *)
(*===============================================================================*)

module Tracker = Map.Make(String)

let function_no =
  fst (List.fold_left
	 (fun (map,number) name -> (Tracker.add name number map,number + 1)) (Tracker.empty,0)
	 ["step_total";
	  "enumerate_transitions_of_system";
	  "enumerate_transitions_of_thread";
	  "enumerate_transitions_of_storage_subsystem";
	  "enumerate_transitions_of_instruction";
	  "update_state";
	  "interp";
	  "interp_exhaustive";
	  "enumerate_transitions_of_storage_subsystem__pcc";
	  "enumerate_transitions_of_storage_subsystem__pw";
	  "enumerate_transitions_of_storage_subsystem__pb";
	  "enumerate_transitions_of_storage_subsystem__sack";
	  "coherence_commitment_cand";
	  "coherence_commitment_action";
          "decode"
         ]
     ) 

let steps_tracked = (30000 : int) (* AArch64 needs a lot of steps *)

let debug = ref false
let perfdebug = ref false
let debug3 = ref false

let enable_debug () = (debug:=true)
let enable_perfdebug () = (perfdebug:=true)

let functions_tracked = Tracker.fold (fun _ _ i -> i + 1) function_no 0

let current_step = ref 0
let increment_step_counter () = current_step := 1 + !current_step
						     
let total_time_start = ref (Unix.gettimeofday ())
let timer_start_total () = total_time_start := Unix.gettimeofday ()
		     
let lock = Array.init functions_tracked (fun _ -> ref false)
let labels = Array.init steps_tracked (fun _ -> ref "")
let writes_seen = Array.init steps_tracked (fun _ -> ref (Nat_big_num.of_int 0))

let set_label (label : string) = labels.(!current_step) := label

let set_writes_seen n = 
  writes_seen.(!current_step) := n

type tinfo = {
    mutable start : float;
    mutable stop  : float;
    mutable delta : float;
    mutable count : int;
  }
			  
(* times.(step_no).(function_no).( {START;STOP;DELTA;LOCK}) *)
let times =
  let init_zero = fun _ -> {start = 0.0; stop = 0.0; delta = 0.0; count = 0} in
  let init_function_times = fun _ -> Array.init functions_tracked init_zero in
  Array.init steps_tracked init_function_times
	      
let timer_start (function_name : string)=
  if !perfdebug then 
    ( let function_no = Tracker.find function_name function_no in
      let now = Unix.gettimeofday () in
      let current_count = times.(!current_step).(function_no).count in
      if not !(lock.(function_no)) then
	(times.(!current_step).(function_no).count <- current_count + 1;
	 times.(!current_step).(function_no).start <- now;
	 lock.(function_no) := true)
      else let error =
	     "Failed to acquire lock for function " ^
	       string_of_int function_no in
	   print_string error;
	   failwith error )
  else ()
		  
let timer_stop (function_name : string) =
  if !perfdebug then 
    ( let function_no = Tracker.find function_name function_no in
      let now = Unix.gettimeofday () in
      let tstart = times.(!current_step).(function_no).start in
      let current_delta = times.(!current_step).(function_no).delta in
      if !(lock.(function_no)) then
	(times.(!current_step).(function_no).stop <- now;
	 times.(!current_step).(function_no).delta <- current_delta +. now -. tstart;
	 lock.(function_no) := false)
      else let error =
	     "Failed to release lock for function " ^
	       string_of_int function_no in
	   print_string error;
	   failwith error )
  else ()

let rec range i j = if i > j then [] else i :: (range (i+1) j)

let print_log () =
  if !perfdebug then
    ( let t = Unix.gettimeofday () in
      
      (* performance summary *)
      let cout = open_out "perf_summary.txt" in
      
      let () = Printf.fprintf cout "%.17fs total time\n" (t -. !total_time_start) in
      
      let acc_time function_no =
	Array.fold_left
	  (fun x array -> x +. array.(function_no).delta)
	  0.0 times in
      
      let () =
	Tracker.iter
	  (fun fname fno -> Printf.fprintf cout "%.17fs %s\n" (acc_time fno) fname)
	  function_no in
      
      let () = close_out cout in
      
      (*  runtime/step-table *)
      let cout = open_out "performance.txt" in

      let () = Printf.fprintf cout "\"step\"  \"label\"  \"writes seen\"  " in
      let () = Tracker.iter (fun fname _ -> Printf.fprintf cout "\"%s\" " fname) function_no in
      let () = Printf.fprintf cout "\n%!" in
      
      let f step =
	Printf.fprintf cout "%d  %!" step;
	Printf.fprintf cout "\"%s\"  %!" !(labels.(step));
	Printf.fprintf cout "%s  %!" (Nat_big_num.to_string !(writes_seen.(step)));
	Tracker.iter (fun _ fno -> Printf.fprintf cout "%.17f  %!" (times.(step)).(fno).delta) function_no;
	Printf.fprintf cout "\n%!" in
      let _ = List.map f (range 0 (!current_step)) in
      
      let () = close_out cout in
      
      (* function-call/step-table *)
      let cout = open_out "function_calls.txt" in
      let g x = Printf.fprintf cout "%d %!" x.count in
      let f i =
	let _ = Printf.fprintf cout "%d %!" i in
	let _ = Array.map g (times.(i)) in
	Printf.fprintf cout "\n%!" in
      let _ = List.map f (range 0 !(current_step)) in
      let () = close_out cout in
      
      () )
  else ()
	 
(* let print_time s = if !debug then let t = Unix.gettimeofday () in Printf.printf "#%.17fs  %s\n" (t -. !start_time) s; flush stdout; start_time :=t *)
let print_string s = if !debug then Printf.printf "%s" s; flush stdout
let print_string2 s = if !debug then Printf.printf "%s" s; flush stdout
let print_integer i = if !debug then Printf.printf "%s" (Nat_big_num.to_string i); flush stdout
											       
let start_time2 = ref (Unix.gettimeofday ()) 
let set_start_time2 () = start_time2 := Unix.gettimeofday ()
let print_time2 s = if !debug then let t = Unix.gettimeofday () in Printf.printf "%.17fs  %s" (t -. !start_time2) s; flush stdout; start_time2 :=t

let start_time3 = ref (Unix.gettimeofday ()) 
let set_start_time3 () = start_time3 := Unix.gettimeofday ()
let print_time3 s = if !debug3 then let t = Unix.gettimeofday () in Printf.printf "\n%.17fs  %s\n" (t -. !start_time3) s; flush stdout; start_time3 :=t



exception Thread_failure of MachineDefTypes.thread_id * MachineDefTypes.ioid * string * string

let catch_thread_errors tid ioid c =
  try c () with
  | Failure s ->
      let bt =
        Printexc.get_backtrace ()
        ^ (Printexc.get_callstack 20 |> Printexc.raw_backtrace_to_string)
      in
      raise (Thread_failure (tid, ioid, s, bt))
