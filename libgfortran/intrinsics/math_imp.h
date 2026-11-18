/* Header file for bit manipulation of long double type
   Copyright (C) 2004-2026 Free Software Foundation, Inc.

This file is part of the GNU Fortran 95 runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public
License as published by the Free Software Foundation; either
version 3 of the License, or (at your option) any later version.

Libgfortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#ifndef LIBGFORTRAN_MATH_IMP_H
#define LIBGFORTRAN_MATH_IMP_H

#if (__SIZEOF_LONG_DOUBLE__ == 16) && (__LDBL_IS_IEC_60559__ > 0)

/* Main union type we use to manipulate the floating-point type.  */
typedef union
{
  long double value;

  struct
#ifdef __MINGW32__
  /* On mingw targets the ms-bitfields option is active by default.
     Therefore enforce gnu-bitfield style.  */
  __attribute__ ((gcc_struct))
#endif
  {
#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
    unsigned negative:1;
    unsigned exponent:15;
    unsigned mantissa0:16;
    unsigned mantissa1:32;
    unsigned mantissa2:32;
    unsigned mantissa3:32;
#else
    unsigned mantissa3:32;
    unsigned mantissa2:32;
    unsigned mantissa1:32;
    unsigned mantissa0:16;
    unsigned exponent:15;
    unsigned negative:1;
#endif
  } ieee;

  struct
  {
#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
    uint64_t high;
    uint64_t low;
#else
    uint64_t low;
    uint64_t high;
#endif
  } words64;

  struct
  {
#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
    uint32_t w0;
    uint32_t w1;
    uint32_t w2;
    uint32_t w3;
#else
    uint32_t w3;
    uint32_t w2;
    uint32_t w1;
    uint32_t w0;
#endif
  } words32;

  struct
#ifdef __MINGW32__
  /* Make sure we are using gnu-style bitfield handling.  */
  __attribute__ ((gcc_struct))
#endif
  {
#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
    unsigned negative:1;
    unsigned exponent:15;
    unsigned quiet_nan:1;
    unsigned mantissa0:15;
    unsigned mantissa1:32;
    unsigned mantissa2:32;
    unsigned mantissa3:32;
#else
    unsigned mantissa3:32;
    unsigned mantissa2:32;
    unsigned mantissa1:32;
    unsigned mantissa0:15;
    unsigned quiet_nan:1;
    unsigned exponent:15;
    unsigned negative:1;
#endif
  } ieee_nan;

} ieee754_long_double;


/* Get two 64 bit ints from a long double.  */
#define GET_LDOUBLE_WORDS64(ix0,ix1,d)  \
do {                                   \
  ieee754_long_double u;               \
  u.value = (d);                       \
  (ix0) = u.words64.high;              \
  (ix1) = u.words64.low;               \
} while (0)

/* Set a long double from two 64 bit ints.  */
#define SET_LDOUBLE_WORDS64(d,ix0,ix1)  \
do {                                   \
  ieee754_long_double u;               \
  u.words64.high = (ix0);              \
  u.words64.low = (ix1);               \
  (d) = u.value;                       \
} while (0)

/* Get the more significant 64 bits of a long double mantissa.  */
#define GET_LDOUBLE_MSW64(v,d)          \
do {                                   \
  ieee754_long_double u;               \
  u.value = (d);                       \
  (v) = u.words64.high;                \
} while (0)

/* Set the more significant 64 bits of a long double mantissa from an int.  */
#define SET_LDOUBLE_MSW64(d,v)          \
do {                                   \
  ieee754_long_double u;               \
  u.value = (d);                       \
  u.words64.high = (v);                \
  (d) = u.value;                       \
} while (0)

/* Get the least significant 64 bits of a long double mantissa.  */
#define GET_LDOUBLE_LSW64(v,d)          \
do {                                   \
  ieee754_long_double u;               \
  u.value = (d);                       \
  (v) = u.words64.low;                 \
} while (0)

static const long double __attribute__ ((unused))
two114 = 2.0769187434139310514121985316880384E+34L, /* 0x4071000000000000, 0 */
twom114 = 4.8148248609680896326399448564623183E-35L, /* 0x3F8D000000000000, 0 */
huge   = 1.0E+4900L,
tiny   = 1.0E-4900L;
#endif

#ifndef math_opt_barrier
# define math_opt_barrier(x) \
({ __typeof (x) __x = (x); __asm ("" : "+m" (__x)); __x; })
# define math_force_eval(x) \
({ __typeof (x) __x = (x); __asm __volatile__ ("" : : "m" (__x)); })
#endif

#endif  /* LIBGFORTRAN_MATH_IMP_H */
