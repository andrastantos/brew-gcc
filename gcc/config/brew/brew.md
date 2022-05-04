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
;; Brew specific constraints, predicates and attributes
;; -------------------------------------------------------------------------

(include "constraints.md")
(include "predicates.md")

; Most instructions are two bytes long.
(define_attr "length" "" (const_int 2))

(define_constants
  [
    (BREW_REG_LINK 14)
  ]
)

;; -------------------------------------------------------------------------
;; nop instruction
;; -------------------------------------------------------------------------

(define_insn "nop"
  [(const_int 0)]
  ""
  "nop"
)

;; -------------------------------------------------------------------------
;; Plus and minus instructions
;; -------------------------------------------------------------------------

(define_insn "addsi3"
  [(set
    (match_operand:SI 0 "register_operand"   "=r,r,r,r")
    (plus:SI
      (match_operand:SI 1 "register_operand"  "r,r,r,r")
      (match_operand:SI 2 "nonmemory_operand" "N,r,L,i")
    )
  )]
  ""
  "@
  %0 <- tiny %1 + %2
  %0 <- %1 + %2
  %0 <- short %1 + %2
  %0 <- %1 + %2"
  [(set_attr "length" "2,2,4,6")]
)

(define_insn "*add_reg_pc"
  [(set
    (match_operand:SI 0 "register_operand" "=r")
    (plus:SI
      (pc)
      (match_operand:SI 1 "immediate_operand" "K")
    )
  )]
  ""
  "%0 <- $pc + %1"
)

(define_insn "subsi3"
  [(set
    (match_operand:SI 0 "register_operand"   "=r,r,r,r")
    (minus:SI
      (match_operand:SI 1 "register_operand"  "r,r,r,r")
      (match_operand:SI 2 "nonmemory_operand" "N,r,L,i")
    )
  )]
  ""
  "@
  %0 <- tiny %1 + -%2
  %0 <- %1 - %2
  %0 <- short %1 - %2
  %0 <- %1 - %2"
  [(set_attr "length" "2,2,4,6")]
)

;; -------------------------------------------------------------------------
;; Multiplications
;; -------------------------------------------------------------------------

(define_insn "mulsi3"
  [(set
    (match_operand:SI 0 "register_operand"   "=r,r,r,r,r,r")
    (mult:SI
      (match_operand:SI 1 "register_operand"  "r,r,r,r,r,r")
      (match_operand:SI 2 "nonmemory_operand" "O,I,M,r,L,i")
    )
  )]
  ""
  "@
  %0 <- %0 ^ %0
  %0 <- %1
  %0 <- -%1
  %0 <- %1 * %2
  %0 <- short %1 * %2
  %0 <- %1 * %2"
  [(set_attr "length" "2,2,2,2,4,6")]
)

;; FIXME:
;;    These things, if enabled, cause an internal compiler error in this, if compiled with -O1 or -O2:
;;
;;       extern void bar();
;;
;;       unsigned int t175_2umul (unsigned int y) {
;;         unsigned int x = (((unsigned int) -1) / 25);
;;         unsigned int r;
;;         if (__builtin_umul_overflow (x, y, &r)) bar ();
;;         return r;
;;       }
;;
;;(define_insn "mulsi3_highpart"
;;  [(set (match_operand:SI 0 "register_operand"                       "=r,r")
;;        (truncate:SI
;;         (lshiftrt:DI
;;          (mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand"  "r,r"))
;;                   (sign_extend:DI (match_operand:SI 2 "nonmemory_operand"  "r,i")))
;;          (const_int 32))))]
;;  ""
;;  "@
;;  %0 <- full %1 * %s2 >>> 0
;;  %0 <- full %1 * %2 >>> 0")
;;
;;(define_insn "umulsi3_highpart"
;;  [(set (match_operand:SI 0 "register_operand"                       "=r,r")
;;        (truncate:SI
;;         (lshiftrt:DI
;;          (mult:DI (zero_extend:DI (match_operand:SI 1 "register_operand"  "r,r"))
;;                   (zero_extend:DI (match_operand:SI 2 "nonmemory_operand"  "r,i")))
;;          (const_int 32))))]
;;  ""
;;  "@
;;  %0 <- full %1 * %2 >> 0
;;  %0 <- full %1 * %2 >> 0")

;; FIXME: These generate incorrect code at the moment, unfortunately.
;;        The most likely reason is that gen_lowpart and gen_highpart (which are
;;        function pointers in a struct, and can mean different things in different
;;        passes) insist on generating the two extra multiplications and the additions
;;        that are usually needed to create a 64-bit result using 32-bit arithmetic.
;;(define_expand "mulsidi3"
;;  [(set (match_operand:DI 0 "register_operand" "=r")
;;        (mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand" "r"))
;;                 (sign_extend:DI (match_operand:SI 2 "brew_allreg_or_const_operand" "ir"))))]
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
;;                 (zero_extend:DI (match_operand:SI 2 "brew_allreg_or_const_operand" "ir"))))]
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
  [(set
    (match_operand:SI 0 "register_operand" "=r")
    (neg:SI
      (match_operand:SI 1 "register_operand" "r")
    )
  )]
  ""
  "%0 <- -%1"
)

(define_insn "one_cmplsi2"
  [(set
    (match_operand:SI 0 "register_operand" "=r")
    (not:SI
      (match_operand:SI 1 "register_operand" "r")
    )
  )]
  ""
  "%0 <- ~%1"
)

(define_insn "bswapsi2"
  [(set
    (match_operand:SI 0 "register_operand" "=r")
    (bswap:SI
      (match_operand:SI 1 "register_operand" "r")
    )
  )]
  ""
  "%0 <- lane_swizzle %1, 0123"
)

;; -------------------------------------------------------------------------
;; Logical operators
;; -------------------------------------------------------------------------

(define_insn "andsi3"
  [(set
    (match_operand:SI 0 "register_operand"   "=r,r,r,r")
    (and:SI
      (match_operand:SI 1 "register_operand"  "r,r,r,r")
      (match_operand:SI 2 "nonmemory_operand" "O,r,L,i")
    )
  )]
  ""
  "@
  %0 <- %1 - %1
  %0 <- %1 & %2
  %0 <- short %1 & %2
  %0 <- %1 & %2"
  [(set_attr "length" "2,2,4,6")]
)

(define_insn "xorsi3"
  [(set
    (match_operand:SI 0 "register_operand"   "=r,r,r,r")
    (xor:SI
      (match_operand:SI 1 "register_operand"  "r,r,r,r")
      (match_operand:SI 2 "nonmemory_operand" "O,r,L,i")
    )
  )]
  ""
  "@
  %0 <- %1
  %0 <- %1 ^ %2
  %0 <- short %1 ^ %2
  %0 <- %1 ^ %2"
  [(set_attr "length" "2,2,4,6")]
)

(define_insn "iorsi3"
  [(set
    (match_operand:SI 0 "register_operand"   "=r,r,r,r")
    (ior:SI
      (match_operand:SI 1 "register_operand"  "r,r,r,r")
      (match_operand:SI 2 "nonmemory_operand" "O,r,L,i")
    )
  )]
  ""
  "@
  %0 <- %1
  %0 <- %1 | %2
  %0 <- short %1 | %2
  %0 <- %1 | %2"
  [(set_attr "length" "2,2,4,6")]
)

;; -------------------------------------------------------------------------
;; Shifts
;; -------------------------------------------------------------------------

(define_insn "ashlsi3"
  [(set
    (match_operand:SI 0 "register_operand"   "=r,r,r")
    (ashift:SI
      (match_operand:SI 1 "nonmemory_operand" "r,L,i")
      (match_operand:SI 2 "register_operand"  "r,r,r")
    )
  )]
  ""
  "@
  %0 <- %1 << %2
  %0 <- short %1 << %2
  %0 <- %1 << %2"
  [(set_attr "length" "2,4,6")]
)

(define_insn "ashrsi3"
  [(set
    (match_operand:SI 0 "register_operand"   "=r,r,r")
    (ashiftrt:SI
      (match_operand:SI 1 "nonmemory_operand" "r,L,i")
      (match_operand:SI 2 "register_operand"  "r,r,r")
    )
  )]
  ""
  "@
  %0 <- %1 >>> %2
  %0 <- short %1 >>> %2
  %0 <- %1 >>> %2"
  [(set_attr "length" "2,4,6")]
)

(define_insn "lshrsi3"
  [(set
    (match_operand:SI 0 "register_operand"   "=r,r,r")
    (lshiftrt:SI
      (match_operand:SI 1 "nonmemory_operand" "r,L,i")
      (match_operand:SI 2 "register_operand"  "r,r,r")
    )
  )]
  ""
  "@
  %0 <- %1 >> %2
  %0 <- short %1 >> %2
  %0 <- %1 >> %2"
  [(set_attr "length" "2,4,6")]
)

;; -------------------------------------------------------------------------
;; Move instructions
;; -------------------------------------------------------------------------

;; The define_expand pattern transforms the move into something
;; that a single ASM instruction can handle. That is, a register
;; to register move, an immediate load or a memory load/store
;; with a <reg>+<offset> address pattern.
;;
;; The corresponding define_insn pattern then recognizes these
;; generated patterns and emits the appropriate instruction.

(define_expand "movsi"
  [(set
    (match_operand:SI 0 "general_operand" "")
    (match_operand:SI 1 "general_operand" "")
  )]
  ""
  "
{
  if (!(reload_in_progress || reload_completed))
    {
      if(MEM_P(operands[0]))
        {
          // For stores, force the second arg. into a register
          operands[1] = force_reg(SImode, operands[1]);
          // We should make sure that the address
          // generated for the store is based on a <reg>+<offset> pattern
          if(MEM_P(XEXP(operands[0], 0)))
            operands[0] = gen_rtx_MEM(SImode, force_reg(SImode, XEXP(operands[0], 0)));
        }
      else if(MEM_P(operands[1]))
        {
          // We should make sure that the address
          // generated for the load is based on a <reg>+<offset> pattern
          if(MEM_P(XEXP (operands[1], 0)))
            operands[1] = gen_rtx_MEM(SImode, force_reg(SImode, XEXP(operands[1], 0)));
        }
    }
}")

(define_insn "*movsi_general"
  [(set
    (match_operand:SI 0 "nonimmediate_operand"  "=r,r,r,r,A,T,B,W,r,r,r,r,m,r")
    (match_operand:SI 1 "general_operand"        "N,L,i,r,r,r,r,r,A,T,B,W,r,m")
  )]
  ""
  "@
   %0 <- tiny %1
   %0 <- short %1
   %0 <- %1
   %0 <- %1
   mem[%0] <- %1 #A
   mem[tiny %0] <- %1 #T
   mem[%0] <- %1 #B
   mem[%0] <- %1 #W
   %0 <- mem[%1] #A
   %0 <- mem[tiny %1] #T
   %0 <- mem[%1] #B
   %0 <- mem[%1] #W
   THIS IS AN ERROR !!!! mem[%0] <- %1 #m
   THIS IS AN ERROR !!!! %0 <- mem[%1] #m"
  [(set_attr "length" "2,4,6,2,6,2,4,2,6,2,4,2,6,6")]
)






(define_expand "movhi"
  [(set
    (match_operand:HI 0 "general_operand" "")
    (match_operand:HI 1 "general_operand" "")
  )]
  ""
  "
{
  if (!(reload_in_progress || reload_completed))
    {
      if(MEM_P(operands[0]))
        {
          // For stores, force the second arg. into a register
          operands[1] = force_reg(HImode, operands[1]);
          // We should make sure that the address
          // generated for the store is based on a <reg>+<offset> pattern
          if(MEM_P(XEXP(operands[0], 0)))
            operands[0] = gen_rtx_MEM(HImode, force_reg(HImode, XEXP(operands[0], 0)));
        }
      else if(MEM_P(operands[1]))
        {
          // We should make sure that the address
          // generated for the load is based on a <reg>+<offset> pattern
          if(MEM_P(XEXP (operands[1], 0)))
            operands[1] = gen_rtx_MEM (HImode, force_reg(HImode, XEXP(operands[1], 0)));
        }
    }
}")

(define_insn "*movhi_general"
  [(set
    (match_operand:HI 0 "nonimmediate_operand"  "=r,r,r,r,A,B,W,r,r,r,m,r")
    (match_operand:HI 1 "general_operand"        "N,L,i,r,r,r,r,A,B,W,r,m")
  )]
  ""
  "@
   %0 <- tiny %1
   %0 <- short %1
   %0 <- %1
   %0 <- %1
   mem16[%0] <- %1
   mem16[%0] <- %1
   mem16[%0] <- %1
   %0 <- mem16[%1]
   %0 <- mem16[%1]
   %0 <- mem16[%1]
   THIS IS AN ERROR !!!! mem16[%0] <- %1
   THIS IS AN ERROR !!!! %0 <- mem16[%1]"
  [(set_attr "length" "2,4,6,2,6,4,2,6,4,2,6,6")]
)







(define_expand "movqi"
  [(set
    (match_operand:QI 0 "general_operand" "")
    (match_operand:QI 1 "general_operand" "")
  )]
  ""
  "
{
  if (!(reload_in_progress || reload_completed))
    {
      if(MEM_P(operands[0]))
        {
          // For stores, force the second arg. into a register
          operands[1] = force_reg(QImode, operands[1]);
          // We should make sure that the address
          // generated for the store is based on a <reg>+<offset> pattern
          if(MEM_P(XEXP(operands[0], 0)))
            operands[0] = gen_rtx_MEM(QImode, force_reg(QImode, XEXP(operands[0], 0)));
        }
      else if(MEM_P(operands[1]))
        {
          // We should make sure that the address
          // generated for the load is based on a <reg>+<offset> pattern
          if(MEM_P(XEXP (operands[1], 0)))
            operands[1] = gen_rtx_MEM(QImode, force_reg(QImode, XEXP(operands[1], 0)));
        }
    }
}")

(define_insn "*movqi_general"
  [(set
    (match_operand:QI 0 "nonimmediate_operand"  "=r,r,r,r,A,B,W,r,r,r,m,r")
    (match_operand:QI 1 "general_operand"        "N,L,i,r,r,r,r,A,B,W,r,m")
  )]
  ""
  "@
   %0 <- tiny %1
   %0 <- short %1
   %0 <- %1
   %0 <- %1
   mem8[%0] <- %1
   mem8[%0] <- %1
   mem8[%0] <- %1
   %0 <- mem8[%1]
   %0 <- mem8[%1]
   %0 <- mem8[%1]
   THIS IS AN ERROR !!!! mem8[%0] <- %1
   THIS IS AN ERROR !!!! %0 <- mem8[%1]"
  [(set_attr "length" "2,4,6,2,6,4,2,6,4,2,6,6")]
)


;; -------------------------------------------------------------------------
;; Sign- and zero-extend
;; -------------------------------------------------------------------------

(define_insn "zero_extendhisi2"
  [(set
    (match_operand:SI 0 "register_operand"      "=r,r,r,r,r")
    (zero_extend:SI
      (match_operand:HI 1 "nonimmediate_operand" "r,A,B,W,m")
    )
  )]
  ""
  "@
   %0 <- %1 & 0xffff
   %0 <- mem16[%1]
   %0 <- mem16[%1]
   %0 <- mem16[%1]
   THIS IS AN ERROR !!!! %0 <- mem16[%1]"
  [(set_attr "length" "6,6,4,2,6")])

(define_insn "zero_extendqisi2"
  [(set
    (match_operand:SI 0 "register_operand"      "=r,r,r,r,rr")
    (zero_extend:SI
      (match_operand:QI 1 "nonimmediate_operand" "r,A,B,W,m")
    )
  )]
  ""
  "@
   %0 <- short %1 & 0xff
   %0 <- mem8[%1]
   %0 <- mem8[%1]
   %0 <- mem8[%1]
   THIS IS AN ERROR !!!! %0 <- mem8[%1]"
  [(set_attr "length" "4,6,4,2,6")])

(define_insn "extendhisi2"
  [(set
    (match_operand:SI 0 "register_operand"      "=r,r,r,r,r")
    (sign_extend:SI
      (match_operand:HI 1 "nonimmediate_operand" "r,A,B,W,m")
    )
  )]
  ""
  "@
   %0 <- wsi %1
   %0 <- smem16[%1]
   %0 <- smem16[%1]
   %0 <- smem16[%1]
   THIS IS AN ERROR !!!! %0 <- smem16[%1]"
  [(set_attr "length" "2,6,4,2,6")])

(define_insn "extendqisi2"
  [(set
    (match_operand:SI 0 "register_operand"      "=r,r,r,r,r")
    (sign_extend:SI
      (match_operand:QI 1 "nonimmediate_operand" "r,A,B,W,m")
    )
  )]
  ""
  "@
   %0 <- bsi %1
   %0 <- smem8[%1]
   %0 <- smem8[%1]
   %0 <- smem8[%1]
   THIS IS AN ERROR !!!! %0 <- smem8[%1]"
  [(set_attr "length" "2,6,4,2,6")])

;; -------------------------------------------------------------------------
;; Conditional branch
;; -------------------------------------------------------------------------

(define_insn "cbranchsi4"
  [(set
    (pc)
    (if_then_else
      (match_operator 0 "comparison_operator"
        [
         (match_operand:SI 1 "register_operand" "r")
         (match_operand:SI 2 "brew_comparison_operand" "rO")
        ]
      )
      (label_ref(match_operand 3 "" ""))
      (pc)
    )
  )]
  ""
{
  return brew_emit_cbranch(SImode, operands, get_attr_length(insn));
}
  [(set
    (attr "length")
    (if_then_else
      (and
        (ge (minus (match_dup 3) (pc)) (const_int -32768))
        (le (minus (match_dup 3) (pc)) (const_int 32767))
      )
      (const_int 4)
      (const_int 10)
    )
  )]
)
;;      (lt (abs (minus (pc) (match_dup 3))) (const_int 32767))

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


;; -------------------------------------------------------------------------
;; Call and Jump instructions
;; -------------------------------------------------------------------------

;; The way these patterns work is that the 'define_expand' version creates
;; all but the last instruction needed for a given call sequence.
;; Then, the 'define_insn' version of the pattern will attach the last
;; instruction (namely the actual jump to the call target).

(define_expand "call"
  [(parallel
    [(call
      (match_operand:SI 0 "" "")
      (match_operand 1 "" "")
    )
    (clobber (reg:SI BREW_REG_LINK))
    ]
  )]
  ""
{
  brew_expand_call(Pmode, operands);
})

(define_insn "*call"
  [
    (call
      (mem:QI (match_operand:SI 0 "nonmemory_operand" "L,i,r"))
      (match_operand 1 "")
    )
    (clobber (reg:SI BREW_REG_LINK))
  ]

  ""
  "@
  $lr <- $pc + 6\;if $r0 == $r0 $pc <- (%0)
  $lr <- $pc + 8\;$pc <- (%0)
  $lr <- $pc + 4\;$pc <- %0"
  [(set_attr "length"        "6,8,4")]
)

;; FIXME: DO WE NEED THIS????
;;(define_insn "*call"
;;  [
;;    (call
;;      (label_ref(match_operand 0 "" ""))
;;      (match_operand 1 "")
;;    )
;;    (clobber (reg:SI BREW_REG_LINK))
;;  ]
;;  ""
;;  "$lr <- $pc + 8\;$pc <- (%l0)"
;;  [(set_attr "length"        "8")]
;;)

(define_expand "call_value"
  [(parallel
    [(set
        (match_operand 0 "" "")
        (call
          (match_operand:SI 1 "" "")
          (match_operand 2 "" "")
        )
      )
      (clobber (reg:SI BREW_REG_LINK))
    ]
  )]
  ""
{
  brew_expand_call(Pmode, operands + 1);
})

(define_insn "*call_value"
  [
    (set
      (match_operand 0 "register_operand" "=r,r,r")
      (call
        (mem:QI (match_operand:SI 1 "nonmemory_operand" "L,i,r"))
        (match_operand 2 "")
      )
    )
    (clobber (reg:SI BREW_REG_LINK))
  ]
  ""
  "@
  $lr <- $pc + 6\;if $r0 == $r0 $pc <- (%1)
  $lr <- $pc + 8\;$pc <- (%1)
  $lr <- $pc + 4\;$pc <- %1"
  [(set_attr "length"        "6,8,4")]
)






(define_expand "sibcall"
  [(parallel
    [(call
      (match_operand:SI 0 "" "")
      (match_operand 1 "" "")
    )
    (return)
    ]
  )]
  ""
{
  brew_expand_call(Pmode, operands);
})

(define_expand "sibcall_value"
  [(parallel
    [(set
        (match_operand 0 "" "")
        (call
          (match_operand:SI 1 "" "")
          (match_operand 2 "" "")
        )
      )
      (return)
    ]
  )]
  ""
{
  brew_expand_call(Pmode, operands + 1);
})

(define_insn "*sibcall"
  [
    (call
      (mem:QI (match_operand:SI 0 "nonmemory_operand" "L,i,r"))
      (match_operand 1 "")
    )
    (return)
  ]
  ""
  "@
  if $r0 == $r0 $pc <- %0
  $pc <- %0
  $pc <- %0"
  [(set_attr "length"        "4,6,2")]
)

(define_insn "*sibcall_value"
  [
    (set
      (match_operand 0 "register_operand" "=r,r,r")
      (call
        (mem:QI (match_operand:SI 1 "nonmemory_operand" "L,i,r"))
        (match_operand 2 "")
      )
    )
    (return)
  ]
  ""
  "@
  if $r0 == $r0 $pc <- %1
  $pc <- %1
  $pc <- %1"
  [(set_attr "length"        "4,6,2")]
)







(define_insn "indirect_jump"
  [(set (pc) (match_operand:SI 0 "nonimmediate_operand" "r"))]
  ""
  "$pc <- %0"
  [(set_attr "length"        "2")]
)

(define_insn "jump"
  [(set (pc) (label_ref (match_operand 0 "" "")))]
  ""
{
  if (get_attr_length(insn) == 4)
    return "if $r0 == $r0 $pc <- %l0";
  else
    return "$pc <- %l0";
}
  [(set
    (attr "length")
    (if_then_else
      (and
        (ge (minus (match_dup 0) (pc)) (const_int -32768))
        (le (minus (match_dup 0) (pc)) (const_int 32767))
      )
      (const_int 4)
      (const_int 6)
    )
  )]
)

;;       (lt (abs (minus (pc) (match_dup 0))) (const_int 32767))

;; -------------------------------------------------------------------------
;; Prologue & Epilogue
;; -------------------------------------------------------------------------

(define_expand "prologue"
  [(clobber (const_int 0))]
  ""
  "
{
  brew_expand_prologue();
  DONE;
}
")

(define_expand "epilogue"
  [(return)]
  ""
  "
{
  brew_expand_epilogue(false);
  DONE;
}
")

(define_expand "sibcall_epilogue"
  [(return)]
  ""
{
  brew_expand_epilogue(true);
  DONE;
})

;; This pattern is used by brew_expand_epilogue to generate the actual
;; return statement. At this point, the stack pointer is already adjusted
;; so all we need is the indirect jump.
(define_insn "returner"
  [(return)]
  "reload_completed"
  "$pc <- $lr"
)
