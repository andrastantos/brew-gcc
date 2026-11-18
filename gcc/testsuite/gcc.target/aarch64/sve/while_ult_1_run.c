/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O3 -ftree-vectorize -march=armv8-a+sve -msve-vector-bits=scalable -mautovec-preference=sve-only" } */

#include "while_ult_1.c"

#define CHECK_LOOP(TYPE, NAME, N)				\
  do								\
    {								\
      TYPE a[N], b[N];						\
      for (unsigned int i = 0; i < N; ++i)			\
	{							\
	  a[i] = 0;						\
	  b[i] = (TYPE) (i * 3 + i % 7);			\
	}							\
      NAME (a, b);						\
      for (unsigned int i = 0; i < N; ++i)			\
	if (a[i] != (TYPE) (b[i] + (TYPE) 1))			\
	  __builtin_abort ();					\
    }								\
  while (0)

#define CHECK_LOOP_ARG(TYPE, NAME, N)				\
  do								\
    {								\
      TYPE a[N], b[N];						\
      for (unsigned int i = 0; i < N; ++i)			\
	{							\
	  a[i] = 0;						\
	  b[i] = (TYPE) (i * 3 + i % 7);			\
	}							\
      NAME (a, b, N);						\
      for (unsigned int i = 0; i < N; ++i)			\
	if (a[i] != (TYPE) (b[i] + (TYPE) 1))			\
	  __builtin_abort ();					\
    }								\
  while (0)

int __attribute__ ((optimize (1)))
main (void)
{
  CHECK_LOOP (uint8_t, full_b, 300);
  CHECK_LOOP (uint16_t, full_h, 1000);
  CHECK_LOOP (uint32_t, full_s, 100);
  CHECK_LOOP (uint64_t, full_d, 40);

  CHECK_LOOP (uint8_t, exact_b, 256);
  CHECK_LOOP (uint16_t, exact_h, 128);
  CHECK_LOOP (uint32_t, exact_s, 64);
  CHECK_LOOP (uint64_t, exact_d, 32);

  svbool_t pg = svptrue_b8 ();
  if (svcntp_b8 (pg, finite_b ()) != 8
      || svcntp_b16 (pg, finite_h ()) != 4
      || svcntp_b32 (pg, finite_s ()) != 2
      || svcntp_b64 (pg, finite_d ()) != 1)
    __builtin_abort ();

  CHECK_LOOP (uint8_t, partial_b, 255);
  CHECK_LOOP (uint16_t, partial_h, 127);
  CHECK_LOOP (uint32_t, partial_s, 63);
  CHECK_LOOP (uint64_t, partial_d, 31);

  CHECK_LOOP_ARG (uint16_t, full_range_h, 1000);
  CHECK_LOOP_ARG (uint16_t, partial_range_h, 127);
  return 0;
}
