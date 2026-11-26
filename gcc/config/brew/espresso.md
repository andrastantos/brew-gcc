;; Pipeline description of the Espresso core, for the instruction scheduler.
;; Copyright (C) 2026 Free Software Foundation, Inc.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;; Espresso issues one instruction per cycle, in order, and keeps the
;; result register reserved until write-back, so a dependent instruction
;; waits (latency 3: two instructions in between hide it).  The timing is
;; from anachron/docs/cpu_espresso/instruction_timing.rst, measured on the
;; RTL; the simulator's cycle model (--timing=espresso) reproduces it.
;; Fetch over the 16-bit bus and taken branches are not modelled here.
;; Brew has no other core, so every -mtune uses this description.

(define_automaton "espresso")
(define_cpu_unit "espresso_issue" "espresso")

;; ALU operations, moves, immediates and shifts: one cycle, result after 3.
(define_insn_reservation "espresso_alu" 3
  (eq_attr "type" "alu,shift")
  "espresso_issue")

;; The multiplier blocks the next instruction for a cycle.
(define_insn_reservation "espresso_mult" 3
  (eq_attr "type" "mult")
  "espresso_issue*2")

;; A load stalls the pipeline until its data is back: the instruction after
;; it still issues, those after wait (32-bit: 3 cycles, data after 6;
;; 8/16-bit: 2 cycles, data after 5).
(define_insn_reservation "espresso_load" 6
  (eq_attr "type" "load")
  "espresso_issue,nothing,espresso_issue*3")

(define_insn_reservation "espresso_loadn" 5
  (eq_attr "type" "loadn")
  "espresso_issue,nothing,espresso_issue*2")

;; Stores are posted: the memory unit waits until the bus accepts the beats.
(define_insn_reservation "espresso_store" 1
  (eq_attr "type" "store")
  "espresso_issue*2")

(define_insn_reservation "espresso_storen" 1
  (eq_attr "type" "storen")
  "espresso_issue")

(define_insn_reservation "espresso_branch" 1
  (eq_attr "type" "branch,jump,call,return,multi")
  "espresso_issue")
