/* Shifts for Brew cores without a barrel shifter (-mno-shift).
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

/* GCC calls these from an instruction pattern (the shifts in brew.md),
   not as library functions, with a private convention: the value in $r0,
   the shift count in $r1 (taken modulo 32, as the shift instructions do),
   the result back in $r0; $r1 is preserved, only $r2, $r3 and $lr are
   clobbered.  None of them is an argument register, so the call is safe
   even while a function's incoming arguments are still in their
   registers; and no input is clobbered, which the pattern couldn't
   describe.  They use no shift or multiply instructions.  */

	.text

/* $r0 <- $r0 << $r1: double $r0, $r1 times.  */
	.global __brew_ashl
	.type __brew_ashl, @function
__brew_ashl:
	$r2 <- short $r1 & 31
	if $r2 == 0 $pc <- 2f
1:
	$r0 <- $r0 + $r0
	$r2 <- tiny $r2 - 1
	if $r2 != 0 $pc <- 1b
2:
	$pc <- $lr
	.size __brew_ashl, . - __brew_ashl

/* $r0 <- $r0 >>> $r1 and $r0 <- $r0 >> $r1.  The result ($r3) collects
   the top 32 - $r1 bits of $r0, one per iteration: double the result, add
   $r0's top bit (its sign), double $r0.  For the arithmetic shift the
   result starts as all ones if $r0 is negative, which leaves the sign in
   the top $r1 bits.  */
	.global __brew_ashr
	.type __brew_ashr, @function
__brew_ashr:
	$r3 <- tiny 0
	if $r0 >= 0 $pc <- 3f
	$r3 <- tiny -1
	$pc <- 3f
	.size __brew_ashr, . - __brew_ashr

	.global __brew_lshr
	.type __brew_lshr, @function
__brew_lshr:
	$r3 <- tiny 0
3:
	$r2 <- short $r1 & 31
	$r2 <- short 32 - $r2
1:
	$r3 <- $r3 + $r3
	if $r0 >= 0 $pc <- 2f
	$r3 <- tiny $r3 + 1
2:
	$r0 <- $r0 + $r0
	$r2 <- tiny $r2 - 1
	if $r2 != 0 $pc <- 1b
	$r0 <- $r3
	$pc <- $lr
	.size __brew_lshr, . - __brew_lshr

	.section .note.GNU-stack,"",@progbits
