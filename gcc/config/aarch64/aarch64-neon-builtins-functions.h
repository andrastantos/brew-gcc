/* ACLE support for AArch64 NEON (function_base classes)
   Copyright (C) 2026-2026 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#ifndef GCC_AARCH64_NEON_BUILTINS_FUNCTIONS_H
#define GCC_AARCH64_NEON_BUILTINS_FUNCTIONS_H

/* Declare the global function base NAME, creating it from an instance
   of class CLASS with constructor arguments ARGS.  */
#define NEON_FUNCTION_IMPL(NAME, CLASS, ARGS)                                  \
  namespace { static constexpr const CLASS NAME##_obj ARGS; }                  \
  const function_base *const aarch64_acle::functions::NAME = &NAME##_obj;

/* Count the number of arguments passed as input (upto 12).  Push a sequence
   rightwards using __VA_ARGS__ and have N fall onto the correct element in
   it.  */
#define NEON_FUNCTION_COUNT_IMPL(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, \
				 _12, N, ...) N
#define NEON_FUNCTION_COUNT(...)                                               \
  NEON_FUNCTION_COUNT_IMPL (__VA_ARGS__, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2,   \
			    1, 0)

/* Base case, where only one intrinsic is being mapped.  The sequence must start
   with 3 as there are three minimum arguments: NAME, CLASS, ARGS (even if
   ARGS is empty).  */
#define NEON_FUNCTION_N_3(N1, C, A) NEON_FUNCTION_IMPL (N1, C, A)
/* For all other cases: define 1..(N-1) intrinsics by falling through, then
   define the Nth one.  */
#define NEON_FUNCTION_N_4(N1, N2, C, A)                                        \
  NEON_FUNCTION_N_3 (N1, C, A)                                                 \
  NEON_FUNCTION_IMPL (N2, C, A)
#define NEON_FUNCTION_N_5(N1, N2, N3, C, A)                                    \
  NEON_FUNCTION_N_4 (N1, N2, C, A)                                             \
  NEON_FUNCTION_IMPL (N3, C, A)
#define NEON_FUNCTION_N_6(N1, N2, N3, N4, C, A)                                \
  NEON_FUNCTION_N_5 (N1, N2, N3, C, A)                                         \
  NEON_FUNCTION_IMPL (N4, C, A)
#define NEON_FUNCTION_N_7(N1, N2, N3, N4, N5, C, A)                            \
  NEON_FUNCTION_N_6 (N1, N2, N3, N4, C, A)                                     \
  NEON_FUNCTION_IMPL (N5, C, A)
#define NEON_FUNCTION_N_8(N1, N2, N3, N4, N5, N6, C, A)                        \
  NEON_FUNCTION_N_7 (N1, N2, N3, N4, N5, C, A)                                 \
  NEON_FUNCTION_IMPL (N6, C, A)
#define NEON_FUNCTION_N_9(N1, N2, N3, N4, N5, N6, N7, C, A)                    \
  NEON_FUNCTION_N_8 (N1, N2, N3, N4, N5, N6, C, A)                             \
  NEON_FUNCTION_IMPL (N7, C, A)
#define NEON_FUNCTION_N_10(N1, N2, N3, N4, N5, N6, N7, N8, C, A)               \
  NEON_FUNCTION_N_9 (N1, N2, N3, N4, N5, N6, N7, C, A)                         \
  NEON_FUNCTION_IMPL (N8, C, A)
#define NEON_FUNCTION_N_11(N1, N2, N3, N4, N5, N6, N7, N8, N9, C, A)           \
  NEON_FUNCTION_N_10 (N1, N2, N3, N4, N5, N6, N7, N8, C, A)                    \
  NEON_FUNCTION_IMPL (N9, C, A)
#define NEON_FUNCTION_N_12(N1, N2, N3, N4, N5, N6, N7, N8, N9, N10, C, A)      \
  NEON_FUNCTION_N_11 (N1, N2, N3, N4, N5, N6, N7, N8, N9, C, A)                \
  NEON_FUNCTION_IMPL (N10, C, A)

/* Count the number of arguments (including CLASS and ARGS) and dispatch to the
   relevant handler macro.  */
#define NEON_FUNCTION_2(N, ...) NEON_FUNCTION_N_##N (__VA_ARGS__)
#define NEON_FUNCTION_1(N, ...) NEON_FUNCTION_2 (N, __VA_ARGS__)
#define NEON_FUNCTION(...)                                                     \
  NEON_FUNCTION_1 (NEON_FUNCTION_COUNT (__VA_ARGS__), __VA_ARGS__)

#endif
