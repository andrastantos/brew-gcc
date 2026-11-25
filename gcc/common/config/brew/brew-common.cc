/* Common hooks for Brew.

   Copyright (C) 2026 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "common/common-target.h"
#include "common/common-target-def.h"
#include "opts.h"
#include "flags.h"

// Default options per optimization level (explicit options override them).
//
// Basic-block reordering: on Brew a taken branch is the costly part of
// a branch (on Espresso it costs 9 cycles, and nearly half the cycles of
// optimized code go to refills after taken branches), so the layout that
// takes fewer branches wins.  Measured on the code-generation tests and
// dhrystone (brew/benchmarks) with the simulator's Espresso cycle model,
// geometric means against GCC's defaults: the "simple" algorithm at -O2
// takes 3% fewer branches and runs 1.2% fewer cycles than "stc" (+0.65%
// code, +0.3% instructions); "stc" at -Os takes 7-9% fewer branches and
// runs 2-2.7% fewer cycles than "simple" (no code size change, +0.5%
// instructions).  The same for every -mtune, so that code compiled for
// one core and libraries compiled for another stay alike.
static const struct default_options brew_option_optimization_table[] =
  {
    { OPT_LEVELS_2_PLUS_SPEED_ONLY, OPT_freorder_blocks_algorithm_, NULL,
      REORDER_BLOCKS_ALGORITHM_SIMPLE },
    { OPT_LEVELS_SIZE, OPT_freorder_blocks_algorithm_, NULL,
      REORDER_BLOCKS_ALGORITHM_STC },
    { OPT_LEVELS_NONE, 0, NULL, 0 }
  };

#undef TARGET_OPTION_OPTIMIZATION_TABLE
#define TARGET_OPTION_OPTIMIZATION_TABLE brew_option_optimization_table

struct gcc_targetm_common targetm_common = TARGETM_COMMON_INITIALIZER;
