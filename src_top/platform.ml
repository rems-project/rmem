(*===============================================================================*)
(*                                                                               *)
(*                rmem executable model                                          *)
(*                =====================                                          *)
(*                                                                               *)
(*  This file is:                                                                *)
(*                                                                               *)
(*  Copyright Jon French, University of Cambridge 2018                           *)
(*                                                                               *)
(*  All rights reserved.                                                         *)
(*                                                                               *)
(*  The rmem tool is distributed under the 2-clause BSD license in LICENCE.txt.  *)
(*  For author information see README.md.                                        *)
(*                                                                               *)
(*===============================================================================*)

(* Platform info for RISCV model *)

open Sail_lib

let dram_base  = 0x00000000L;;  (* Spike::DRAM_BASE *)
let dram_size  = Int64.(shift_left 2048L 20)
let clint_base = 0x82000000L;;  (* Spike::CLINT_BASE *)
let clint_size = 0x800c0000L;;  (* Spike::CLINT_SIZE *)
let rom_base   = 0x80001000L;;  (* Spike::DEFAULT_RSTVEC *)
let cpu_hz = 1000000000;;
let insns_per_tick = 100;;

let bits_of_int i =
  get_slice_int (Big_int.of_int 64, Big_int.of_int i, Big_int.zero)

let bits_of_int64 i =
  get_slice_int (Big_int.of_int 64, Big_int.of_int64 i, Big_int.zero)


let rom_base () = Lem_machine_word.wordFromInteger Lem_machine_word.instance_Machine_word_Size_Machine_word_ty64_dict (Big_int.of_int64 rom_base)
let rom_size () = Lem_machine_word.wordFromInteger Lem_machine_word.instance_Machine_word_Size_Machine_word_ty64_dict Big_int.zero   (* !rom_size_ref *)

let dram_base () = Lem_machine_word.wordFromInteger Lem_machine_word.instance_Machine_word_Size_Machine_word_ty64_dict (Big_int.of_int64 dram_base)
let dram_size () = Lem_machine_word.wordFromInteger Lem_machine_word.instance_Machine_word_Size_Machine_word_ty64_dict (Big_int.of_int64 dram_size)

let htif_tohost () =
  Lem_machine_word.wordFromInteger Lem_machine_word.instance_Machine_word_Size_Machine_word_ty64_dict (Big_int.of_int 0x100) (*  bits_of_int 0*) (* (Big_int.to_int64 (Elf.elf_tohost ())) *)

let clint_base () = Lem_machine_word.wordFromInteger Lem_machine_word.instance_Machine_word_Size_Machine_word_ty64_dict  (Big_int.of_int64 clint_base)
let clint_size () = Lem_machine_word.wordFromInteger Lem_machine_word.instance_Machine_word_Size_Machine_word_ty64_dict (Big_int.of_int64 clint_size)

let insns_per_tick () = Big_int.of_int insns_per_tick

let term_write _ = failwith "term_write stub"

let enable_dirty_update () = false

let enable_misaligned_access () = true

let mtval_has_illegal_inst_bits () = false
