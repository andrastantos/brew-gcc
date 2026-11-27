/* Multiplication for Brew cores without a multiplier (-mno-mul).
   Copyright (C) 2026 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

/* On such a core GCC calls __mulsi3 instead of using the instruction.  It
   must not use multiplication or shifts itself (it may be built for a core
   without a barrel shifter too): it uses additions only, a + a doubles a,
   and a mask that doubles every step walks the bits.  The empty asm
   statements keep the optimizers from recognizing the doubling loops as
   shifts.  (The shifts are in shift.S.)  */

typedef unsigned int USItype __attribute__ ((mode (SI)));

USItype __mulsi3 (USItype, USItype);

/* One addition per set bit of B, one loop iteration per bit up to B's
   highest set bit.  */
USItype
__mulsi3 (USItype a, USItype b)
{
  USItype r = 0;
  USItype bit = 1;

  while (b != 0)
    {
      if (b & bit)
	{
	  r += a;
	  b -= bit;
	}
      a += a;
      bit += bit;
      __asm__ ("" : "+r" (a), "+r" (bit));
    }
  return r;
}
