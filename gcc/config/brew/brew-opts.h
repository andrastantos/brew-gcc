/* Definitions for option handling for Brew.
   Copyright (C) 2026 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_BREW_OPTS_H
#define GCC_BREW_OPTS_H

/* The processors -mtune= knows.  brew_tune_table in brew.cc has one
   entry for each, in this order.  */
enum brew_processor
{
  PROCESSOR_GENERIC,
  PROCESSOR_ESPRESSO,
  PROCESSOR_max
};

/* -march=: which of the optional instructions (multiply, shifts; see
   "Optional instructions" in the ISA) the core implements.  */
enum brew_arch
{
  BREW_ARCH_FULL,	/* brew: multiplier and barrel shifter */
  BREW_ARCH_NOMUL,	/* brew-nomul: no multiplier */
  BREW_ARCH_NOSHIFT,	/* brew-noshift: no barrel shifter */
  BREW_ARCH_MIN		/* brew-min: neither */
};

#endif /* GCC_BREW_OPTS_H */
