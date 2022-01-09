;; Predicate definitions for brew
;; Copyright (C) 2009-2021 Free Software Foundation, Inc.
;; Contributed by Andras Tantos <andras@tantosonline.com>

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.

;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;; -------------------------------------------------------------------------
;; Predicates
;; -------------------------------------------------------------------------

;; Nonzero if OP can be source of a simple move operation.

(define_predicate "brew_general_mov_src_operand"
  (match_code "mem,const_int,reg,pc,subreg,symbol_ref,label_ref,const")
{
  return brew_mov_operand(mode, op, false);
})

(define_predicate "brew_general_mov_dst_operand"
  (match_code "mem,reg,subreg,pc")
{
  return brew_mov_operand(mode, op, true);
})

(define_predicate "brew_mov_memory_operand"
  (match_code "mem")
{
  return brew_mov_memory_operand(mode, op);
})


;; A predicate that accepts either a register or a constant integer operand.
;;   this is used for the many 3-operand ALU operations.
;;(define_predicate "brew_allreg_or_const_operand"
;;  (match_code "reg,pc,const_int,subreg,symbol_ref,label_ref,const")
;;)
(define_predicate "brew_allreg_or_const_operand"
  (match_code "reg,pc,subreg,const_int")
)

(define_predicate "brew_allreg_operand"
  (match_code "reg,pc,subreg")
)

(define_predicate "brew_pc_operand"
  (match_code "pc")
)



(define_predicate "brew_comparison_operand"
  (ior
    (match_code "reg,subreg")
    (and
      (match_code "const_int")
      (match_test "INTVAL(op) == 0")
    )
  )
)
