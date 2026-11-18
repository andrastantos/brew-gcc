/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_UNARY_0(
  __riscv_vsetvlmax_e32m4,
  vint32m4_t,
  vfloat64m8_t,
  __riscv_vle32_v_i32m4,
  __riscv_vfwcvt_f_x_v_f64m8,
  __riscv_vse64_v_f64m8,
  vfwcvt_f_x,
  LOOP_UNARY_BODY_X4)

/* { dg-final { scan-assembler-times {vfwcvt\.f\.x\.v\s+v0,v4} 1 } } */
/* { dg-final { scan-assembler-times {vfwcvt\.f\.x\.v\s+v8,v12} 1 } } */
