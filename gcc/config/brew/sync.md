;; Machine description for Brew: atomic operations
;; Copyright (C) 2026 Free Software Foundation, Inc.

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

;; Brew has load-lock / store-conditional (memll / memsc, 32 bits only; see
;; brew/docs_src/memory_model.rst): memsc stores only while the lock memll
;; placed is still valid and sets its register to 0 on success, non-zero on
;; failure.  Each read-modify-write below is a single insn holding the whole
;; retry loop, so that nothing (in particular no spill) can end up between
;; the memll and the memsc.
;;
;; Brew doesn't order loads and stores among themselves, so the memory
;; model is implemented with fences around the loop: a full fence before it
;; for release semantics and one after it for acquire semantics.  Aligned
;; 32-bit (and narrower) loads and stores are single accesses, so GCC's
;; default expansion of atomic loads and stores (a plain move plus fences
;; from mem_thread_fence) is correct.
;;
;; 8- and 16-bit atomics operate on the aligned word that contains them,
;; with a mask for their field (Brew is little-endian); see
;; brew_expand_subword_* in brew.cc.  There are no 64-bit ones: the ISA has
;; no 64-bit memll/memsc pair, so GCC calls the __atomic_*_8 library
;; functions for those.

(define_c_enum "unspecv" [
  UNSPECV_FENCE
  UNSPECV_CAS
  UNSPECV_XCHG
  UNSPECV_ATOMIC_OP
  UNSPECV_MASKED_CAS
  UNSPECV_MASKED_OP
])

(define_mode_iterator BREW_SHORT [QI HI])

(define_code_iterator brew_atomic_op [plus minus and ior xor])
(define_code_attr brew_atomic_name [(plus "add") (minus "sub") (and "and")
                                    (ior "or") (xor "xor")])
(define_code_attr brew_atomic_asm [(plus "+") (minus "-") (and "&")
                                   (ior "|") (xor "^")])

;; -------------------------------------------------------------------------
;; Fences
;; -------------------------------------------------------------------------

(define_expand "mem_thread_fence"
  [(match_operand:SI 0 "const_int_operand" "")] ;; model
  ""
{
  if (!is_mm_relaxed (memmodel_from_int (INTVAL (operands[0]))))
    emit_insn (gen_brew_fence ());
  DONE;
})

(define_expand "brew_fence"
  [(set (match_dup 0)
        (unspec:BLK [(match_dup 0)] UNSPECV_FENCE))]
  ""
{
  operands[0] = gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (Pmode));
  MEM_VOLATILE_P (operands[0]) = 1;
})

(define_insn "*brew_fence"
  [(set (match_operand:BLK 0 "" "")
        (unspec:BLK [(match_dup 0)] UNSPECV_FENCE))]
  ""
  "fence_rw_rw"
)

;; -------------------------------------------------------------------------
;; Compare and swap
;; -------------------------------------------------------------------------

(define_expand "atomic_compare_and_swapsi"
  [(match_operand:SI 0 "register_operand" "")    ;; bool success output
   (match_operand:SI 1 "register_operand" "")    ;; old value output
   (match_operand:SI 2 "memory_operand" "")      ;; memory
   (match_operand:SI 3 "register_operand" "")    ;; expected value
   (match_operand:SI 4 "register_operand" "")    ;; desired value
   (match_operand:SI 5 "const_int_operand" "")   ;; is_weak
   (match_operand:SI 6 "const_int_operand" "")   ;; success model
   (match_operand:SI 7 "const_int_operand" "")]  ;; failure model
  ""
{
  brew_atomic_pre_fence (INTVAL (operands[6]));
  emit_insn (gen_brew_compare_and_swapsi (operands[1], operands[2],
                                          operands[3], operands[4]));
  /* The fence after the loop also orders the failure path (which never
     stores), so it must satisfy the failure model too; the failure model
     is never stronger than the success model.  */
  brew_atomic_post_fence (INTVAL (operands[6]));
  emit_store_flag_force (operands[0], EQ, operands[1], operands[3], SImode,
                         1, 1);
  DONE;
})

(define_insn "brew_compare_and_swapsi"
  [(set (match_operand:SI 0 "register_operand" "=&r")
        (match_operand:SI 1 "memory_operand" "+Q"))
   (set (match_dup 1)
        (unspec_volatile:SI
          [(match_operand:SI 2 "register_operand" "r")
           (match_operand:SI 3 "register_operand" "r")]
          UNSPECV_CAS))
   (clobber (match_scratch:SI 4 "=&r"))]
  ""
  "1:\;%0 <- memll[%1]\;if %0 != %2 $pc <- 2f\;%4 <- %3\;memsc[%1] <- %4\;if %4 != 0 $pc <- 1b\;2:"
  [(set_attr "length" "14")]
)

;; -------------------------------------------------------------------------
;; Exchange
;; -------------------------------------------------------------------------

(define_expand "atomic_exchangesi"
  [(match_operand:SI 0 "register_operand" "")    ;; old value output
   (match_operand:SI 1 "memory_operand" "")      ;; memory
   (match_operand:SI 2 "register_operand" "")    ;; new value
   (match_operand:SI 3 "const_int_operand" "")]  ;; model
  ""
{
  brew_atomic_pre_fence (INTVAL (operands[3]));
  emit_insn (gen_brew_exchangesi (operands[0], operands[1], operands[2]));
  brew_atomic_post_fence (INTVAL (operands[3]));
  DONE;
})

(define_insn "brew_exchangesi"
  [(set (match_operand:SI 0 "register_operand" "=&r")
        (match_operand:SI 1 "memory_operand" "+Q"))
   (set (match_dup 1)
        (unspec_volatile:SI
          [(match_operand:SI 2 "register_operand" "r")]
          UNSPECV_XCHG))
   (clobber (match_scratch:SI 3 "=&r"))]
  ""
  "1:\;%0 <- memll[%1]\;%3 <- %2\;memsc[%1] <- %3\;if %3 != 0 $pc <- 1b"
  [(set_attr "length" "10")]
)

;; -------------------------------------------------------------------------
;; Fetch and operate.  GCC derives <op>_fetch and the result-less forms
;; from these.
;; -------------------------------------------------------------------------

(define_expand "atomic_fetch_<brew_atomic_name>si"
  [(match_operand:SI 0 "register_operand" "")    ;; old value output
   (match_operand:SI 1 "memory_operand" "")      ;; memory
   (brew_atomic_op:SI (match_dup 1)
     (match_operand:SI 2 "register_operand" "")) ;; operand
   (match_operand:SI 3 "const_int_operand" "")]  ;; model
  ""
{
  brew_atomic_pre_fence (INTVAL (operands[3]));
  emit_insn (gen_brew_fetch_<brew_atomic_name>si (operands[0], operands[1],
                                                  operands[2]));
  brew_atomic_post_fence (INTVAL (operands[3]));
  DONE;
})

(define_insn "brew_fetch_<brew_atomic_name>si"
  [(set (match_operand:SI 0 "register_operand" "=&r")
        (match_operand:SI 1 "memory_operand" "+Q"))
   (set (match_dup 1)
        (unspec_volatile:SI
          [(brew_atomic_op:SI (match_dup 1)
             (match_operand:SI 2 "register_operand" "r"))]
          UNSPECV_ATOMIC_OP))
   (clobber (match_scratch:SI 3 "=&r"))]
  ""
  "1:\;%0 <- memll[%1]\;%3 <- %0 <brew_atomic_asm> %2\;memsc[%1] <- %3\;if %3 != 0 $pc <- 1b"
  [(set_attr "length" "10")]
)

(define_expand "atomic_fetch_nandsi"
  [(match_operand:SI 0 "register_operand" "")    ;; old value output
   (match_operand:SI 1 "memory_operand" "")      ;; memory
   (match_operand:SI 2 "register_operand" "")    ;; operand
   (match_operand:SI 3 "const_int_operand" "")]  ;; model
  ""
{
  brew_atomic_pre_fence (INTVAL (operands[3]));
  emit_insn (gen_brew_fetch_nandsi (operands[0], operands[1], operands[2]));
  brew_atomic_post_fence (INTVAL (operands[3]));
  DONE;
})

(define_insn "brew_fetch_nandsi"
  [(set (match_operand:SI 0 "register_operand" "=&r")
        (match_operand:SI 1 "memory_operand" "+Q"))
   (set (match_dup 1)
        (unspec_volatile:SI
          [(not:SI (and:SI (match_dup 1)
                     (match_operand:SI 2 "register_operand" "r")))]
          UNSPECV_ATOMIC_OP))
   (clobber (match_scratch:SI 3 "=&r"))]
  ""
  "1:\;%0 <- memll[%1]\;%3 <- %0 & %2\;%3 <- ~%3\;memsc[%1] <- %3\;if %3 != 0 $pc <- 1b"
  [(set_attr "length" "12")]
)

;; -------------------------------------------------------------------------
;; 8- and 16-bit atomics: a masked loop on the containing word
;; -------------------------------------------------------------------------

(define_expand "atomic_compare_and_swap<mode>"
  [(match_operand:SI 0 "register_operand" "")            ;; bool success output
   (match_operand:BREW_SHORT 1 "register_operand" "")    ;; old value output
   (match_operand:BREW_SHORT 2 "memory_operand" "")      ;; memory
   (match_operand:BREW_SHORT 3 "nonmemory_operand" "")   ;; expected value
   (match_operand:BREW_SHORT 4 "nonmemory_operand" "")   ;; desired value
   (match_operand:SI 5 "const_int_operand" "")           ;; is_weak
   (match_operand:SI 6 "const_int_operand" "")           ;; success model
   (match_operand:SI 7 "const_int_operand" "")]          ;; failure model
  ""
{
  brew_expand_subword_cas (operands[0], operands[1], operands[2], operands[3],
                           operands[4], INTVAL (operands[6]));
  DONE;
})

(define_expand "atomic_exchange<mode>"
  [(match_operand:BREW_SHORT 0 "register_operand" "")    ;; old value output
   (match_operand:BREW_SHORT 1 "memory_operand" "")      ;; memory
   (match_operand:BREW_SHORT 2 "nonmemory_operand" "")   ;; new value
   (match_operand:SI 3 "const_int_operand" "")]          ;; model
  ""
{
  brew_expand_subword_rmw (operands[0], operands[1], operands[2], SET,
                           INTVAL (operands[3]));
  DONE;
})

(define_expand "atomic_fetch_<brew_atomic_name><mode>"
  [(match_operand:BREW_SHORT 0 "register_operand" "")    ;; old value output
   (match_operand:BREW_SHORT 1 "memory_operand" "")      ;; memory
   (brew_atomic_op:BREW_SHORT (match_dup 1)
     (match_operand:BREW_SHORT 2 "nonmemory_operand" "")) ;; operand
   (match_operand:SI 3 "const_int_operand" "")]          ;; model
  ""
{
  brew_expand_subword_rmw (operands[0], operands[1], operands[2], <CODE>,
                           INTVAL (operands[3]));
  DONE;
})

(define_expand "atomic_fetch_nand<mode>"
  [(match_operand:BREW_SHORT 0 "register_operand" "")    ;; old value output
   (match_operand:BREW_SHORT 1 "memory_operand" "")      ;; memory
   (match_operand:BREW_SHORT 2 "nonmemory_operand" "")   ;; operand
   (match_operand:SI 3 "const_int_operand" "")]          ;; model
  ""
{
  brew_expand_subword_rmw (operands[0], operands[1], operands[2], NOT,
                           INTVAL (operands[3]));
  DONE;
})

;; Operand 1 is the containing word, 2 the field's mask, 3 its complement,
;; 4 and 5 the expected and desired values shifted into the field.
(define_insn "brew_masked_cassi"
  [(set (match_operand:SI 0 "register_operand" "=&r")
        (match_operand:SI 1 "memory_operand" "+Q"))
   (set (match_dup 1)
        (unspec_volatile:SI
          [(match_operand:SI 2 "register_operand" "r")
           (match_operand:SI 3 "register_operand" "r")
           (match_operand:SI 4 "register_operand" "r")
           (match_operand:SI 5 "register_operand" "r")]
          UNSPECV_MASKED_CAS))
   (clobber (match_scratch:SI 6 "=&r"))]
  ""
  "1:\;%0 <- memll[%1]\;%6 <- %0 & %2\;if %6 != %4 $pc <- 2f\;%6 <- %0 & %3\;%6 <- %6 | %5\;memsc[%1] <- %6\;if %6 != 0 $pc <- 1b\;2:"
  [(set_attr "length" "18")]
)

;; Operands as above, with 4 the operand shifted into the field and 5 the
;; operation (an rtx code: SET for exchange, NOT for nand); see
;; brew_output_masked_rmw.
(define_insn "brew_masked_rmwsi"
  [(set (match_operand:SI 0 "register_operand" "=&r")
        (match_operand:SI 1 "memory_operand" "+Q"))
   (set (match_dup 1)
        (unspec_volatile:SI
          [(match_operand:SI 2 "register_operand" "r")
           (match_operand:SI 3 "register_operand" "r")
           (match_operand:SI 4 "register_operand" "r")
           (match_operand:SI 5 "const_int_operand" "")]
          UNSPECV_MASKED_OP))
   (clobber (match_scratch:SI 6 "=&r"))
   (clobber (match_scratch:SI 7 "=&r"))]
  ""
{
  return brew_output_masked_rmw (operands);
}
  [(set_attr "length" "18")]
)
