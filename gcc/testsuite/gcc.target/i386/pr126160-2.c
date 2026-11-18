/* PR target/126160 */
/* { dg-do compile } */
/* { dg-options "-Ofast -mavx512f -mavx512vl -mavx512dq -mprefer-vector-width=512" } */
/* { dg-final { scan-assembler-times "vcvtpd2dq" 3 } } */
/* { dg-final { scan-assembler-not "vcvtsd2si" } } */

/* __builtin_irint (double -> int) should vectorize into vcvtpd2dq rather
   than scalar vcvtsd2sil: V2DF -> V2SI, V4DF -> V4SI and V8DF -> V8SI.  */

void
f2 (int *a, double *b)
{
  a[0] = __builtin_irint (b[0]);
  a[1] = __builtin_irint (b[1]);
}

void
f4 (int *a, double *b)
{
  a[0] = __builtin_irint (b[0]);
  a[1] = __builtin_irint (b[1]);
  a[2] = __builtin_irint (b[2]);
  a[3] = __builtin_irint (b[3]);
}

void
f8 (int *a, double *b)
{
  a[0] = __builtin_irint (b[0]);
  a[1] = __builtin_irint (b[1]);
  a[2] = __builtin_irint (b[2]);
  a[3] = __builtin_irint (b[3]);
  a[4] = __builtin_irint (b[4]);
  a[5] = __builtin_irint (b[5]);
  a[6] = __builtin_irint (b[6]);
  a[7] = __builtin_irint (b[7]);
}
