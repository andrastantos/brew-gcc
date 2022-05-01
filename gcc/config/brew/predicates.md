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

(define_predicate "brew_comparison_operand"
  (ior
    (match_code "reg,subreg")
    (and
      (match_code "const_int")
      (match_test "INTVAL(op) == 0")
    )
  )
)

;; A predicate that matches memory operands with a base register of $fp, $sp $?fp and $?ap
(define_predicate "brew_tiny_memory_operand"
  (and
    (match_operand 0 "memory_operand")
    (match_test "MEM_P(op) && brew_legitimate_tiny_address_p(XEXP(op, 0))")
  )
)