/* Verify that built-ins compile correctly if floating-point is supported.  */
/* { dg-do compile { target { any_fpu } } }  */

void
test (void)
{
  __builtin_sh_get_fpscr ();
  __builtin_sh_set_fpscr (0);
}
