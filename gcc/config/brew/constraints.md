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

(define_constraint "L"
  "The constant 1"
  (and
    (match_code "const_int")
    (match_test "ival == 1")
  )
)

(define_constraint "M"
  "The constant -1"
  (and
    (match_code "const_int")
    (match_test "ival == -1")
  )
)

(define_constraint "T"
  "A 16-bit signed constant"
  (and (match_code "const_int")
       (match_test "ival >= -0x8000 && ival <= 0x7fff")))

(define_constraint "P"
  "A 16-bit signed address offset"
  (and (match_code "const_int")
       (match_test "ival >= -0x8000*2 && ival <= 0x7fff*2")))

(define_constraint "K"
  "Address offsets for link setup"
  (and (match_code "const_int")
       (match_test "ival >= 0 && ival <= 28")))
