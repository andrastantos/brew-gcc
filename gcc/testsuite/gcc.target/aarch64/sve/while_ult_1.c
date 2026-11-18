/* { dg-do compile } */
/* { dg-options "-O3 -ftree-vectorize -march=armv8-a+sve -msve-vector-bits=scalable -mautovec-preference=sve-only -fdump-tree-vect-details --save-temps" } */
/* { dg-final { scan-tree-dump-times "LOOP VECTORIZED" 14 "vect" } } */

#include <arm_sve.h>
#include <stdint.h>

#define NOIPA __attribute__ ((noipa))

#define ADD_LOOP(TYPE, NAME, N)					\
  void NOIPA							\
  NAME (TYPE *__restrict a, const TYPE *__restrict b)			\
  {								\
    for (unsigned int i = 0; i < N; ++i)			\
      a[i] = b[i] + (TYPE) 1;					\
  }

ADD_LOOP (uint8_t, full_b, 300)
ADD_LOOP (uint16_t, full_h, 1000)
ADD_LOOP (uint32_t, full_s, 100)
ADD_LOOP (uint64_t, full_d, 40)

ADD_LOOP (uint8_t, exact_b, 256)
ADD_LOOP (uint16_t, exact_h, 128)
ADD_LOOP (uint32_t, exact_s, 64)
ADD_LOOP (uint64_t, exact_d, 32)

svbool_t NOIPA
finite_b (void)
{
  return svptrue_pat_b8 (SV_VL8);
}

svbool_t NOIPA
finite_h (void)
{
  return svptrue_pat_b16 (SV_VL4);
}

svbool_t NOIPA
finite_s (void)
{
  return svptrue_pat_b32 (SV_VL2);
}

svbool_t NOIPA
finite_d (void)
{
  return svptrue_pat_b64 (SV_VL1);
}


ADD_LOOP (uint8_t, partial_b, 255)
ADD_LOOP (uint16_t, partial_h, 127)
ADD_LOOP (uint32_t, partial_s, 63)
ADD_LOOP (uint64_t, partial_d, 31)

void NOIPA
full_range_h (uint16_t *__restrict a, const uint16_t *__restrict b,
	      unsigned int n)
{
  if (n < 1000)
    __builtin_unreachable ();
  for (unsigned int i = 0; i < n; ++i)
    a[i] = b[i] + (uint16_t) 1;
}

void NOIPA
partial_range_h (uint16_t *__restrict a, const uint16_t *__restrict b,
		 unsigned int n)
{
  if (n < 100 || n > 127)
    __builtin_unreachable ();
  for (unsigned int i = 0; i < n; ++i)
    a[i] = b[i] + (uint16_t) 1;
}

/* The first WHILELO in the "full" loops should fold to an all-true
   predicate.  GCC prints all-true predicates as .b, regardless of the
   element size of the consuming instruction.  */
/* { dg-final { scan-assembler-times {\tptrue\tp[0-7]\.b, all\n} 9 } } */

/* The finite loops should fold to finite PTRUE patterns.  */
/* { dg-final { scan-assembler-times {\tptrue\tp[0-7]\.b, vl8\n} 1 } } */
/* { dg-final { scan-assembler-times {\tptrue\tp[0-7]\.h, vl4\n} 1 } } */
/* { dg-final { scan-assembler-times {\tptrue\tp[0-7]\.s, vl2\n} 1 } } */
/* { dg-final { scan-assembler-times {\tptrue\tp[0-7]\.b, vl1\n} 1 } } */

/* The "partial" loops should keep their initial WHILELO.  */
/* { dg-final { scan-assembler-times {\twhilelo\tp[0-7]\.b, wzr, w[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {\twhilelo\tp[0-7]\.h, wzr, w[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {\twhilelo\tp[0-7]\.s, wzr, w[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {\twhilelo\tp[0-7]\.d, wzr, w[0-9]+} 1 } } */

/* All loops still need a latch WHILELO.  */
/* { dg-final { scan-assembler-times {\twhilelo\tp[0-7]\.b, [wx][0-9]+, [wx][0-9]+} 3 } } */
/* { dg-final { scan-assembler-times {\twhilelo\tp[0-7]\.h, [wx][0-9]+, [wx][0-9]+} 5 } } */
/* { dg-final { scan-assembler-times {\twhilelo\tp[0-7]\.s, [wx][0-9]+, [wx][0-9]+} 3 } } */
/* { dg-final { scan-assembler-times {\twhilelo\tp[0-7]\.d, [wx][0-9]+, [wx][0-9]+} 3 } } */
