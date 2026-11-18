/* Verify that no ICE occurs if target doesn't have an FPU.  */
/* { dg-do compile { target { no_fpu } } }  */
/* The default -ansi makes an implicit declaration legal and silent.  */
/* { dg-options "-Werror=implicit-function-declaration" } */

void
error (void)
{
  __builtin_sh_get_fpscr (); /* { dg-error "implicit declaration of function" } */
  __builtin_sh_set_fpscr (0); /* { dg-error "implicit declaration of function" } */
}
