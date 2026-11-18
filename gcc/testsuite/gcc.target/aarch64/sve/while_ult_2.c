/* { dg-do compile } */
/* { dg-options "-O3 -ftree-vectorize -march=armv8-a+sve -msve-vector-bits=scalable -mautovec-preference=sve-only -fdump-tree-vect-details --save-temps" } */
/* { dg-final { scan-tree-dump-times "LOOP VECTORIZED" 1 "vect" } } */

#include <arm_sve.h>
#include <stdint.h>

void
large_exact_gap (int *__restrict a, const int *__restrict b)
{
  for (unsigned long long i = 0; i < 0x80000000ULL; ++i)
    a[i] = b[i] + 1;
}

/* { dg-final { scan-assembler-times {\tptrue\tp[0-7]\.b, all\n} 1 } } */

/* All loops still need a latch WHILELO.  */
/* { dg-final { scan-assembler-times {\twhilelo\tp[0-7]\.s, [wx][0-9]+, [wx][0-9]+} 1 } } */
