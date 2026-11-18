/* { dg-options "-O3 -mtune=neoverse-v2" } */

void
f (float *restrict in, float *restrict out, int num)
{
  for (int i = 0; i < num; ++i)
    {
      float r = in[4 * i];
      float g = in[4 * i + 1];
      float b = in[4 * i + 2];
      float a = in[4 * i + 3];
      out[4 * i] = r * 0.9f + g * 1.0f + b * 0.7f + a * 1.0f;
      out[4 * i + 1] = r * 1.1f + g * 0.8f + b * 1.2f + a * 1.0f;
      out[4 * i + 2] = r * 0.7f + g * 1.2f + b * 0.8f + a * 1.0f;
      out[4 * i + 3] = r * 1.2f + g * 0.7f + b * 1.1f + a * 1.0f;
    }
}

/* We should use AdvSIMD ld4/st4 rather than SVE for the vectorized main loop
   on Neoverse-V2.  */
/* { dg-final { scan-assembler {\tld4\t} } } */
/* { dg-final { scan-assembler {\tst4\t} } } */
