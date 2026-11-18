/* Verify that __has_builtin reports whether the built-ins are available.  */
/* { dg-do compile } */
/* The default -ansi predates static_assert.  */
/* { dg-options "-std=c23" } */

#ifdef __SH_FPU_ANY__
static_assert (__has_builtin (__builtin_sh_get_fpscr));
static_assert (__has_builtin (__builtin_sh_set_fpscr));
#else
static_assert (!__has_builtin (__builtin_sh_get_fpscr));
static_assert (!__has_builtin (__builtin_sh_set_fpscr));
#endif
