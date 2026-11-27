/* The scheduler doesn't break dependencies on Brew: moving the pointer
   increment above the store would turn mem8[$rA] into the longer
   mem8[$rA + -1].  */
/* { dg-do compile } */
/* { dg-options "-O2 -mmul -mshift" } */

unsigned char buf[256];

void
fill (unsigned x)
{
  for (int i = 0; i < 256; i++)
    {
      x ^= x << 13;
      x ^= x >> 17;
      x ^= x << 5;
      buf[i] = x;
    }
}

/* { dg-final { scan-assembler {mem8\[\$r[0-9]+\] <-} } } */
/* { dg-final { scan-assembler-not {mem8\[\$r[0-9]+ \+ -} } } */
