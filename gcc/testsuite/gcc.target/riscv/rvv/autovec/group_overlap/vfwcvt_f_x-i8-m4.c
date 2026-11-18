/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_UNARY_0(
  __riscv_vsetvlmax_e8m4,
  vint8m4_t,
  vfloat16m8_t,
  __riscv_vle8_v_i8m4,
  __riscv_vfwcvt_f_x_v_f16m8,
  __riscv_vse16_v_f16m8,
  vfwcvt_f_x,
  LOOP_UNARY_BODY_X4)

/* { dg-final { scan-assembler-times {vfwcvt\.f\.x\.v\s+v0,v4} 1 } } */
/* { dg-final { scan-assembler-times {vfwcvt\.f\.x\.v\s+v8,v12} 1 } } */
