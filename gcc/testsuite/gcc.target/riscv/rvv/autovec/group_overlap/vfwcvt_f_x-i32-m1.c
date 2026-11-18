/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_UNARY_0(
  __riscv_vsetvlmax_e32m1,
  vint32m1_t,
  vfloat64m2_t,
  __riscv_vle32_v_i32m1,
  __riscv_vfwcvt_f_x_v_f64m2,
  __riscv_vse64_v_f64m2,
  vfwcvt_f_x,
  LOOP_UNARY_BODY_X16)

/* { dg-final { scan-assembler-times {vfwcvt\.f\.x\.v\s+v0,v1([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vfwcvt\.f\.x\.v\s+v2,v3([^0-9]|$)} 1 } } */
