/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_UNARY_0(
  __riscv_vsetvlmax_e32m2,
  vuint32m2_t,
  vfloat64m4_t,
  __riscv_vle32_v_u32m2,
  __riscv_vfwcvt_f_xu_v_f64m4,
  __riscv_vse64_v_f64m4,
  vfwcvt_f_xu,
  LOOP_UNARY_BODY_X8)

/* { dg-final { scan-assembler-times {vfwcvt\.f\.xu\.v\s+v0,v2([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vfwcvt\.f\.xu\.v\s+v4,v6} 1 } } */
