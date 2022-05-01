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

(define_register_constraint
  "a"
  "TINY_OFS_REGS"
  "Registers with tiny offsets"
)

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

(define_constraint "J"
  "Tiny constant offset (8-bits)"
  (and
    (match_code "const_int")
    (match_test "IN_RANGE (ival / 4, -64, 63)")
  )
)

(define_constraint "N"
  "Tiny immediate (4-bits, one's complement)"
  (and
    (match_code "const_int")
    (match_test "IN_RANGE (ival, -7, 7)")
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
    (match_test "IN_RANGE (ival, -32768, 32767)")
  )
)

(define_constraint "K"
  "Address offsets for link setup"
  (and
    (match_code "const_int")
    (match_test "IN_RANGE (ival / 2, 0, 14)")
  )
)
