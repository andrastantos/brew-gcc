/* { dg-do run } */
/* { dg-options "-fstack-protector-explicit" } */
/* { dg-require-effective-target fstack_protector } */
/* Brew starts the stack at the top of memory and crt0 puts no argv/environment
   strings above main's frame, so the overrun faults before the canary is checked.  */
/* { dg-xfail-run-if "overrun runs off the top of memory" { brew-*-* } } */

#include <stdlib.h>

void
__stack_chk_fail (void)
{
  exit (0); /* pass */
}

int __attribute__((stack_protect)) main ()
{
  int i;
  char foo[255];

  // smash stack
  for (i = 0; i <= 400; i++)
    foo[i] = 42;

  return 1; /* fail */
}
