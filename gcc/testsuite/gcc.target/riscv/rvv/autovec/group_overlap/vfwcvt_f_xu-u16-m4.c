/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_UNARY_0(
  __riscv_vsetvlmax_e16m4,
  vuint16m4_t,
  vfloat32m8_t,
  __riscv_vle16_v_u16m4,
  __riscv_vfwcvt_f_xu_v_f32m8,
  __riscv_vse32_v_f32m8,
  vfwcvt_f_xu,
  LOOP_UNARY_BODY_X4)

/* { dg-final { scan-assembler-times {vfwcvt\.f\.xu\.v\s+v0,v4} 1 } } */
/* { dg-final { scan-assembler-times {vfwcvt\.f\.xu\.v\s+v8,v12} 1 } } */
