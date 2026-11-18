/* { dg-do compile } */
/* { dg-options "-O2 -mavx -masm=att" } */

typedef double v2df __attribute__((vector_size (16)));

v2df
f1 (double *a, double *b)
{
  return (v2df) { *b, *a };
}

/* { dg-final { scan-assembler-times "vmovhpd\[ \t]" 1 } } */
/* { dg-final { scan-assembler-not "vunpcklpd" } } */

v2df
f2 (double *a)
{
  return (v2df) { *a, 0.0 };
}

/* { dg-final { scan-assembler-times "vmovq\[ \t]" 1 } } */
/* { dg-final { scan-assembler-times "vmovsd\[ \t]" 1 } } */
