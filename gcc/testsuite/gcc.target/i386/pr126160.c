/* PR target/126160 */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-Ofast -mavx512dq -mavx512vl" } */
/* { dg-final { scan-assembler-times "vcvtps2qq" 2 } } */
/* { dg-final { scan-assembler-not "vcvtss2si" } } */

/* The float -> long conversions should vectorize into vcvtps2qq rather
   than scalar vcvtss2siq: V4SF -> V4DI for foo, V2SF -> V2DI for bar.  */

void
foo (long *a, float *b)
{
  a[0] = __builtin_lrint (b[0]);
  a[1] = __builtin_lrint (b[1]);
  a[2] = __builtin_lrint (b[2]);
  a[3] = __builtin_lrint (b[3]);
}

void
bar (long *a, float *b)
{
  a[0] = __builtin_lrint (b[0]);
  a[1] = __builtin_lrint (b[1]);
}
