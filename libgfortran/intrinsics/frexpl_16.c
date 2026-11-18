/* s_frexpl.c -- long double version of s_frexp.c.
 * Conversion to IEEE quad long double by Jakub Jelinek, jj@ultra.linux.cz.
 */

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
frexpl (long double x, int *eptr)
{
  uint64_t hx, lx, ix;

  GET_LDOUBLE_WORDS64 (hx, lx, x);
  ix = 0x7fffffffffffffffULL&hx;
  *eptr = 0;
  if (ix >= 0x7fff000000000000ULL || ((ix | lx) == 0))
    /* 0,inf,nan */
    return x + x;
  if (ix < 0x0001000000000000ULL)
    {
      /* subnormal */
      x *= two114;
      GET_LDOUBLE_MSW64 (hx, x);
      ix = hx & 0x7fffffffffffffffULL;
      *eptr = -114;
    }
  *eptr += (ix >> 48) - 16382;
  hx = (hx & 0x8000ffffffffffffULL) | 0x3ffe000000000000ULL;
  SET_LDOUBLE_MSW64 (x, hx);
  return x;
}
