/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh -mabi=lp64d" } */

#include "group_overlap.h"

DEF_GROUP_OVERLAP_UNARY_0(
  __riscv_vsetvlmax_e8m1,
  vuint8m1_t,
  vfloat16m2_t,
  __riscv_vle8_v_u8m1,
  __riscv_vfwcvt_f_xu_v_f16m2,
  __riscv_vse16_v_f16m2,
  vfwcvt_f_xu,
  LOOP_UNARY_BODY_X16)

/* { dg-final { scan-assembler-times {vfwcvt\.f\.xu\.v\s+v0,v1([^0-9]|$)} 1 } } */
/* { dg-final { scan-assembler-times {vfwcvt\.f\.xu\.v\s+v2,v3([^0-9]|$)} 1 } } */
