/* { dg-do compile } */
/* { dg-options "-O2 -mno-gfni" } */

unsigned int foo(unsigned int x)
{
  return __builtin_bitreverse32 (x);
}

/* { dg-final { scan-assembler-times "leal" 2 } } */
/* { dg-final { scan-assembler-not "orl" } } */
