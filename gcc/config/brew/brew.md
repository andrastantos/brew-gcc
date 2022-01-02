;; Machine description for Brew
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
;; Special agument formating codes:
;; 's' (as in %s0) prints the register in it's 'signed format'
;; 'f' (as in %f0) prints the register in it's 'flat format'
;; For non-register references, these formatting codes are simply ignored.
;; -------------------------------------------------------------------------


;; -------------------------------------------------------------------------
;; Brew specific constraints, predicates and attributes
;; -------------------------------------------------------------------------

(include "constraints.md")
(include "predicates.md")

; Most instructions are two bytes long.
(define_attr "length" "" (const_int 2))

;; -------------------------------------------------------------------------
;; nop instruction
;; -------------------------------------------------------------------------

(define_insn "nop"
  [(const_int 0)]
  ""
  "nop")

;; -------------------------------------------------------------------------
;; Arithmetic instructions
;; -------------------------------------------------------------------------

(define_insn "addsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r,r,r")
        (plus:SI
          (match_operand:SI 1 "register_operand" "r,r,r,r")
          (match_operand:SI 2 "brew_reg_or_const" "L,M,r,i")))]
  ""
  "@
  %0 <- %1 + 1
  %0 <- %1 - 1
  %0 <- %1 + %2
  %0 <- %1 + (%2)")


(define_insn "subsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r,r,r")
          (minus:SI
          (match_operand:SI 1 "register_operand" "r,r,r,r")
          (match_operand:SI 2 "brew_reg_or_const" "L,M,r,i")))]
  ""
  "@
  %0 <- %1 - 1
  %0 <- %1 + 1
  %0 <- %1 - %2
  %0 <- %1 - (%2)")

;; FIXME: THIS IS THE SIGNED VERSION. HOW TO DEFINE THE UNSIGNED VERSION?
(define_insn "mulsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
          (mult:SI
           (match_operand:SI 1 "register_operand" "r,r")
           (match_operand:SI 2 "brew_reg_or_const" "r,i")))]
  ""
  "@
  %0 <- %1 * %2
  %0 <- %1 * (%2)")

(define_insn "mulsi3_highpart"
  [(set (match_operand:SI 0 "register_operand"                       "=r,r")
        (truncate:SI
         (lshiftrt:DI
          (mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand"  "r,r"))
                   (sign_extend:DI (match_operand:SI 2 "brew_reg_or_const"  "r,i")))
          (const_int 32))))]
  ""
  "@
  %s0 <- upper %s1 * %s2
  %s0 <- upper %s1 * (%s2)")

(define_insn "umulsi3_highpart"
  [(set (match_operand:SI 0 "register_operand"                       "=r,r")
        (truncate:SI
         (lshiftrt:DI
          (mult:DI (zero_extend:DI (match_operand:SI 1 "register_operand"  "r,r"))
                   (zero_extend:DI (match_operand:SI 2 "brew_reg_or_const"  "r,i")))
          (const_int 32))))]
  ""
  "@
  %0 <- upper %1 * %2
  %0 <- upper %1 * (%2)")

;; FIXME: These generate incorrect code at the moment, unfortunately.
;;        The most likely reason is that gen_lowpart and gen_highpart (which are
;;        function pointers in a struct, and can mean different things in different
;;        passes) insist on generating the two extra multiplications and the additions
;;        that are usually needed to create a 64-bit result using 32-bit arithmetic.
;;(define_expand "mulsidi3"
;;  [(set (match_operand:DI 0 "register_operand" "=r")
;;        (mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand" "r"))
;;                 (sign_extend:DI (match_operand:SI 2 "brew_reg_or_const" "ir"))))]
;;  ""
;;{
;;  rtx hi = gen_reg_rtx (SImode);
;;  rtx lo = gen_reg_rtx (SImode);
;;
;;  emit_insn (gen_mulsi3_highpart (hi, operands[1], operands[2]));
;;  emit_insn (gen_mulsi3 (lo, operands[1], operands[2]));
;;  emit_move_insn (gen_lowpart (SImode, operands[0]), lo);
;;  emit_move_insn (gen_highpart (SImode, operands[0]), hi);
;;  DONE;
;;})

;;(define_expand "umulsidi3"
;;  [(set (match_operand:DI 0 "register_operand" "=r")
;;        (mult:DI (zero_extend:DI (match_operand:SI 1 "register_operand" "r"))
;;                 (zero_extend:DI (match_operand:SI 2 "brew_reg_or_const" "ir"))))]
;;  ""
;;{
;;  rtx hi = gen_reg_rtx (SImode);
;;  rtx lo = gen_reg_rtx (SImode);
;;
;;  emit_insn (gen_umulsi3_highpart (hi, operands[1], operands[2]));
;;  emit_insn (gen_mulsi3 (lo, operands[1], operands[2]));
;;  emit_move_insn (gen_lowpart (SImode, operands[0]), lo);
;;  emit_move_insn (gen_highpart (SImode, operands[0]), hi);
;;  DONE;
;;})

;; -------------------------------------------------------------------------
;; Unary arithmetic instructions
;; -------------------------------------------------------------------------

(define_insn "negsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
          (neg:SI (match_operand:SI 1 "register_operand" "r")))]
  ""
  "%s0 <- -%s1")

(define_insn "one_cmplsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (not:SI (match_operand:SI 1 "register_operand" "r")))]
  ""
  "%0 <- ~%1")

;; -------------------------------------------------------------------------
;; Logical operators
;; -------------------------------------------------------------------------

(define_insn "andsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (and:SI
          (match_operand:SI 1 "register_operand" "r")
          (match_operand:SI 2 "brew_reg_or_const" "ir")))]
  ""
  "%0 <- %1 & %2"
)

(define_insn "xorsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (xor:SI
          (match_operand:SI 1 "register_operand" "r")
          (match_operand:SI 2 "brew_reg_or_const" "ir")))]
  ""
  "%0 <- %1 ^ %2"
)

(define_insn "iorsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (ior:SI
          (match_operand:SI 1 "register_operand" "r")
          (match_operand:SI 2 "brew_reg_or_const" "ir")))]
  ""
  "%0 <- %1 | %2"
)

;; -------------------------------------------------------------------------
;; Shifters
;; -------------------------------------------------------------------------

(define_insn "ashlsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
        (ashift:SI
          (match_operand:SI 1 "brew_reg_or_const" "r,ir")
          (match_operand:SI 2 "brew_reg_or_const" "ir,r")))]
  ""
  "@
  %0 <- %1 << %2
  %0 <- %1 << %2"
)

(define_insn "ashrsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
        (ashiftrt:SI
          (match_operand:SI 1 "brew_reg_or_const" "r,ir")
          (match_operand:SI 2 "brew_reg_or_const" "ir,r")))]
  ""
  "@
  %s0 <- %s1 >> %2
  %s0 <- %s1 >> %2"
)

(define_insn "lshrsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
        (lshiftrt:SI
          (match_operand:SI 1 "brew_reg_or_const" "r,ir")
          (match_operand:SI 2 "brew_reg_or_const" "ir,r")))]
  ""
  "@
  %0 <- %1 >> %2
  s0 <- %1 >> %2"
)

;; -------------------------------------------------------------------------
;; Move instructions
;; -------------------------------------------------------------------------

;; SImode

;; Push a register onto the stack
(define_insn "movsi_push"
  [(set
    (mem:SI (pre_dec:SI (reg:SI 1)))
    (match_operand:SI 0 "register_operand" "r")
  )
  ]
  ""
  "$sp <- $sp - 4
  mem[$sp] <- %0")

;; Pop a register from the stack
(define_insn "movsi_pop"
  [(set (match_operand:SI 1 "register_operand" "=r")
          (mem:SI (post_inc:SI (match_operand:SI 0 "register_operand" "r"))))]
  ""
  "%1 <- mem[$sp]
  $sp <- $sp + 4")

(define_expand "movsi"
   [(set (match_operand:SI 0 "general_operand" "")
         (match_operand:SI 1 "general_operand" ""))]
   ""
  "
{
  /* If this is a store, force the value into a register.  */
  if (! (reload_in_progress || reload_completed))
  {
    if (MEM_P (operands[0]))
    {
      operands[1] = force_reg (SImode, operands[1]);
      if (MEM_P (XEXP (operands[0], 0)))
        operands[0] = gen_rtx_MEM (SImode, force_reg (SImode, XEXP (operands[0], 0)));
    }
    else 
      if (MEM_P (operands[1])
          && MEM_P (XEXP (operands[1], 0)))
        operands[1] = gen_rtx_MEM (SImode, force_reg (SImode, XEXP (operands[1], 0)));
  }
}")

(define_insn "*movsi"
  [(set (match_operand:SI 0 "nonimmediate_operand"        "=r,r,r,W,A,B,r,r,r")
        (match_operand:SI 1 "brew_general_mov_src_operand" "O,r,i,r,r,r,W,A,B"))]
  "register_operand (operands[0], SImode)
   || register_operand (operands[1], SImode)"
  "@
   %0 <- %0 - %0
   %0 <- %1
   %0 <- %1
   mem[%0] <- %1
   mem[%0] <- %1
   mem[%0] <- %1
   %0 <- mem[%1]
   %0 <- mem[%1]
   %0 <- mem[%1]"
  [(set_attr "length"        "2,2,6,6,6,6,6,6,6")])

(define_insn "zero_extendhisi2"
  [(set (match_operand:SI 0 "register_operand"                    "=r,r,r,r")
        (zero_extend:SI (match_operand:HI 1 "nonimmediate_operand" "r,W,A,B")))]
  ""
  "@
   %0 <- %1 & 0xffff
   %0 <- mem16[%1]
   %0 <- mem16[%1]
   %0 <- mem16[%1]"
  [(set_attr "length" "6,6,6,6")])

(define_insn "zero_extendqisi2"
  [(set (match_operand:SI 0 "register_operand"                    "=r,r,r,r")
        (zero_extend:SI (match_operand:QI 1 "nonimmediate_operand" "r,W,A,B")))]
  ""
  "@
   %0 <- %1 & 0xff
   %0 <- mem8[%1]
   %0 <- mem8[%1]
   %0 <- mem8[%1]"
  [(set_attr "length" "6,6,6,6")])

(define_insn "extendhisi2"
  [(set (match_operand:SI 0 "register_operand"                    "=r,r,r,r")
        (sign_extend:SI (match_operand:HI 1 "nonimmediate_operand" "r,W,A,B")))]
  ""
  "@
   %s0 <- wsi %s1
   %s0 <- mem16[%1]
   %s0 <- mem16[%1]
   %s0 <- mem16[%1]"
  [(set_attr "length" "2,6,6,6")])

(define_insn "extendqisi2"
  [(set (match_operand:SI 0 "register_operand"                    "=r,r,r,r")
        (sign_extend:SI (match_operand:QI 1 "nonimmediate_operand" "r,W,A,B")))]
  ""
  "@
   %s0 <- bsi %s1
   %s0 <- mem8[%1]
   %s0 <- mem8[%1]
   %s0 <- mem8[%1]"
  [(set_attr "length" "2,6,6,6")])

(define_expand "movqi"
  [(set (match_operand:QI 0 "general_operand" "")
        (match_operand:QI 1 "general_operand" ""))]
  ""
  "
{
  /* If this is a store, force the value into a register.  */
  if (MEM_P (operands[0]))
    operands[1] = force_reg (QImode, operands[1]);
}")

(define_insn "*movhi"
  [(set (match_operand:HI 0 "nonimmediate_operand"        "=r,r,r,W,A,B,r,r,r")
        (match_operand:HI 1 "brew_general_mov_src_operand" "O,r,i,r,r,r,W,A,B"))]
  "(register_operand (operands[0], HImode)
    || register_operand (operands[1], HImode))"
  "@
   %0 <- %0 - %0
   %0 <- %1
   %0 <- %1
   mem16[%0] <- %1
   mem16[%0] <- %1
   mem16[%0] <- %1
   %0 <- mem16[%1]
   %0 <- mem16[%1]
   %0 <- mem16[%1]"
  [(set_attr "length"        "2,2,6,6,6,6,6,6,6")])

(define_insn "*movqi"
  [(set (match_operand:QI 0 "nonimmediate_operand"        "=r,r,r,W,A,B,r,r,r")
        (match_operand:QI 1 "brew_general_mov_src_operand" "O,r,i,r,r,r,W,A,B"))]
  "register_operand (operands[0], QImode)
   || register_operand (operands[1], QImode)"
  "@
   %0 <- %0 - %0
   %0 <- %1
   %0 <- %1
   mem8[%0] <- %1
   mem8[%0] <- %1
   mem8[%0] <- %1
   %0 <- mem8[%1]
   %0 <- mem8[%1]
   %0 <- mem8[%1]"
  [(set_attr "length"        "2,2,6,6,6,6,6,6,6")])

(define_expand "movhi"
  [(set (match_operand:HI 0 "general_operand" "")
        (match_operand:HI 1 "general_operand" ""))]
  ""
  "
{
  /* If this is a store, force the value into a register.  */
  if (MEM_P (operands[0]))
    operands[1] = force_reg (HImode, operands[1]);
}")

;; -------------------------------------------------------------------------
;; Conditional branch
;; -------------------------------------------------------------------------

;; FIXME: removed 'i' as it caused all sorts of trouble in register
;;        allocation. But now, 0-compares don't match, which is a petty
;;        and results in suboptimal codegen.
(define_insn "cbranchsi4"
  [(set
    (pc)
    (if_then_else
      (match_operator 0 "comparison_operator"
        [
         (match_operand:SI 1 "register_operand" "r")
         (match_operand:SI 2 "brew_comparison_operand" "r")
        ]
      )
      (label_ref(match_operand 3 "" ""))
      (pc)
    )
  )]
  ""
{
  return brew_emit_cbranch(SImode, operands);
}
  [(set_attr "length" "6")]
)

;; -------------------------------------------------------------------------
;; Branch instructions (these apparently must be defined according to chapter
;; 'Interdependence of patterns')
;; -------------------------------------------------------------------------
;;
;;(define_code_iterator cond [ne eq lt ltu gt gtu ge le geu leu])
;;
;;(define_insn "*b<cond:code>"
;;  [(set
;;    (pc)
;;    (if_then_else
;;      (cond:CC (match_operand:SI 0 ) (const_int 0))
;;      (pc)
;;      (label_ref (match_operand 1 "" ""))
;;    )
;;  )]
;;  ""
;;{
;;  return brew_emit_bcond(SImode, <CODE>, false, operands);
;;}
;;  [(set_attr "length" "6")]
;;)
;;
;;(define_insn "*b<cond:code>"
;;  [(set
;;    (pc)
;;    (if_then_else
;;      (cond:CC (match_operand:SI 0 ) (const_int 0))
;;      (label_ref (match_operand 1 "" ""))
;;      (pc)
;;    )
;;  )]
;;  ""
;;{
;;  return brew_emit_bcond(SImode, <CODE>, true, operands);
;;}
;;  [(set_attr "length" "6")]
;;)
;;
;;
;; -------------------------------------------------------------------------
;; Call and Jump instructions
;; -------------------------------------------------------------------------

(define_expand "call"
  [(call
    (match_operand:QI 0 "memory_operand" "")
    (match_operand 1 "general_operand" "")
  )]
  ""
{
  gcc_assert (MEM_P (operands[0]));
})

;;  (clobber (reg:SI 3))

(define_insn "*call"
  [(call
    (mem:QI (match_operand:SI 0 "nonmemory_operand" "ir"))
    (match_operand 1 "")
  )
  ]
  ""
  "$r3 <- $pc + 12
  $sp <- $sp - 4
  mem[$sp] <- $r3
  $pc <- %0"
)

(define_expand "call_value"
  [(set (match_operand 0 "" "")
                (call (match_operand:QI 1 "memory_operand" "")
                 (match_operand 2 "" "")))]
  ""
{
  gcc_assert (MEM_P (operands[1]));
})

;;  (clobber (reg:SI 3))
(define_insn "*call_value"
  [(set (match_operand 0 "register_operand" "=r")
        (call (mem:QI (match_operand:SI
                       1 "immediate_operand" "i"))
              (match_operand 2 "" "")))
  ]
  ""
  "$r3 <- $pc + 12
  $sp <- $sp - 4
  mem[$sp] <- $r3
  $pc <- %0"
)

;;  (clobber (reg:SI 3))

(define_insn "*call_value_indirect"
  [(set (match_operand 0 "register_operand" "=r")
        (call (mem:QI (match_operand:SI
                       1 "register_operand" "r"))
              (match_operand 2 "" "")))
  ]
  ""
  "$r3 <- $pc + 12
  $sp <- $sp - 4
  mem[$sp] <- $r3
  $pc <- %0"
)

(define_insn "indirect_jump"
  [(set (pc) (match_operand:SI 0 "nonimmediate_operand" "r"))]
  ""
  "$pc <-%0")

(define_insn "jump"
  [(set (pc)
        (label_ref (match_operand 0 "" "")))]
  ""
  "$pc <- %l0"
  [(set_attr "length"        "6")]
)


;; -------------------------------------------------------------------------
;; Prologue & Epilogue
;; -------------------------------------------------------------------------

(define_expand "prologue"
  [(clobber (const_int 0))]
  ""
  "
{
  brew_expand_prologue ();
  DONE;
}
")

(define_expand "epilogue"
  [(return)]
  ""
  "
{
  brew_expand_epilogue ();
  DONE;
}
")

;; FIXME: this is not ideal: we have to increment $sp
;;        before we can load $pc (well, d'uh), but that
;;        means we either clobber $r3 or we leave the
;;        stack in an unstable state. An interrupt in that
;;        point might corrupt the return address.
;;        So, maybe we do need an atomic RET instruction?
(define_insn "returner"
  [(return)]
  "reload_completed"
  "$r3 <- mem[$sp]
   $sp <- $sp + 4
   $pc <- $r3"
)
