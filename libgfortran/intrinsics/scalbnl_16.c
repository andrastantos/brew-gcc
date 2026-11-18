/* s_scalbnl.c -- long double version of s_scalbn.c.
 * Conversion to IEEE quad long double by Jakub Jelinek, jj@ultra.linux.cz.
 */

/* @(#)s_scalbn.c 5.1 93/09/24 */
/*
 * ====================================================
 * Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
 *
 * Developed at SunPro, a Sun Microsystems, Inc. business.
 * Permission to use, copy, modify, and distribute this
 * software is freely granted, provided that this notice
 * is preserved.
 * ====================================================
 */

long double
scalbnl (long double x, int n)
{
  int64_t k, hx, lx;

  GET_LDOUBLE_WORDS64 (hx, lx, x);

  /* extract exponent */
  k = (hx >> 48) & 0x7fff;
  if (k == 0)
    {
      /* 0 or subnormal x */
      if ((lx | (hx & 0x7fffffffffffffffULL)) == 0)
	return x; /* +-0 */
      x *= two114;
      GET_LDOUBLE_MSW64 (hx, x);
      k = ((hx >> 48) & 0x7fff) - 114;
    }
  if (k == 0x7fff)
    /* NaN or Inf */
    return x + x;
  if (n < -50000)
    /*underflow*/
    return tiny * copysignl (tiny, x);
  if (n > 50000 || k + n > 0x7ffe)
    /* overflow  */
    return huge * copysignl (huge, x);

  /* Now k and n are bounded we know that k = k + n does not overflow.  */
  k = k + n;
  if (k > 0)
    {
      /* normal result */
      SET_LDOUBLE_MSW64 (x, (hx&0x8000ffffffffffffULL) | (k << 48));
      return x;
    }
  if (k <= -114)
    /* underflow */
    return tiny * copysignl (tiny, x);

  /* subnormal result */
  k += 114;
  SET_LDOUBLE_MSW64 (x, (hx & 0x8000ffffffffffffULL) | (k << 48));
  return x * twom114;
}
