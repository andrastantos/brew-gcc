/* { dg-do run { target { ! avr_tiny } } } */
/* { dg-additional-options { -std=gnu99 -Os -mcall-prologues } } */

// !!! Requires the fx64 <-> float conversions from AVR-LibC.

#include "fx64.h"

#define MK_TEST(fx)				    \
  NI bool in_range_##fx (float x)		    \
  {						    \
    return x < fmax_##fx && x >= fmin_##fx;	    \
  }						    \
						    \
  NI void test_mul_##fx (float a, float b)	    \
  {						    \
    if (!in_range_##fx (a))			    \
      return;					    \
    if (!in_range_##fx (b))			    \
      return;					    \
    float f = a * b;				    \
    __asm ("" : "+r" (f));			    \
    fx##_t ab = ((fx##_t) a) * (fx##_t) b;	    \
    if (f < fmin_##fx)				    \
      {						    \
	if (ab != min_##fx)			    \
	  exit (id_##fx + 1);			    \
	return;					    \
      }						    \
    if (f > fmax_##fx)				    \
      {						    \
	if (ab != max_##fx)			    \
	  exit (id_##fx + 2);			    \
	return;					    \
      }						    \
    if (f != (float) ab)			    \
      exit (id_##fx + 3);			    \
  }

MK_TEST (lk)
MK_TEST (ulk)
MK_TEST (llk)
MK_TEST (ullk)
MK_TEST (llr)
MK_TEST (ullr)

NI void test_mul (float a, float b)
{
  test_mul_lk (a, b);
  test_mul_ulk (a, b);

  test_mul_llk (a, b);
  test_mul_ullk (a, b);

  test_mul_llr (a, b);
  test_mul_ullr (a, b);
}

// Results / arguments must be representable as float, so no rounding occurs.
// No-overflow results must be representable as fixed, so no rounding occurs.
const PROGMEM float fvals[] =
  {
    0.0,
    +1.0, +2.0, +0.5, +0xff.0p0, +0xf.fp0, +0x0.ffp0, +0xcd.0p12, +0x0.0a1p0,
    -1.0, -2.0, -0.5, -0xff.0p0, -0xf.fp0, -0x0.ffp0, -0xcd.0p12, -0x0.0a1p0,
  };

NI void test (void)
{
  for (uint8_t a = 0; a < ARRAY_SIZE (fvals); ++a)
    for (uint8_t b = 0; b < ARRAY_SIZE (fvals); ++b)
      {
	float fa = pgm_read_float (&fvals[a]);
	float fb = pgm_read_float (&fvals[b]);
	test_mul (fa, fb);
      }	
}

int main (void)
{
  test ();
  return 0;
}
