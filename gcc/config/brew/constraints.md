;; Constraint definitions for brew
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
;; Constraints
;; -------------------------------------------------------------------------

(define_constraint "O"
  "The constant zero"
  (and
    (match_code "const_int")
    (match_test "ival == 0")
  )
)

(define_constraint "I"
  "The constant 1"
  (and
    (match_code "const_int")
    (match_test "ival == 1")
  )
)

(define_constraint "N"
  "Tiny immediate (4-bits, one's complement)"
  (and
    (match_code "const_int")
    (match_test "IN_RANGE(ival, -7, 7)")
  )
)

(define_constraint "M"
  "The constant -1"
  (and
    (match_code "const_int")
    (match_test "ival == -1")
  )
)

(define_constraint "L"
  "A signed 16-bit immediate."
  (and
    (match_code "const_int")
    (match_test "IN_RANGE(ival, -32768, 32767)")
  )
)

(define_constraint "K"
  "Address offsets for link setup"
  (and
    (match_code "const_int")
    (match_test "IN_RANGE(ival / 2, 0, 14) && ((ival & 1) == 0)")
  )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Various addressing modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_constraint "A"
  "An absolute address."
  (and
    (match_code "mem")
    (ior
      (match_test "GET_CODE(XEXP(op, 0)) == SYMBOL_REF")
      (match_test "GET_CODE(XEXP(op, 0)) == LABEL_REF")
      (match_test "GET_CODE(XEXP(op, 0)) == CONST")
    )
  )
)

(define_constraint "B"
  "A register-offset address."
  (and
    (match_code "mem")
    (match_test "brew_legitimate_offset_address_p(XEXP(op, 0), true)")
  )
)

(define_constraint "T"
  "A tiny register-offset address."
  (and
    (match_code "mem")
    (match_test "brew_legitimate_tiny_address_p(XEXP(op, 0))")
  )
)

(define_constraint "W"
  "A register indirect memory operand."
  (and
    (match_code "mem")
    (match_test "REG_P(XEXP(op, 0)) && REGNO_OK_FOR_BASE_P(REGNO(XEXP(op, 0)))")
  )
)
