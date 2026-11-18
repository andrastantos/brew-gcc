/* Include arm-only outputs in the multiple-set conversion limit.  */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-ter -fno-tree-coalesce-vars -fdump-rtl-ce1" } */
/* { dg-additional-options "--param=max-rtl-if-conversion-insns=3 --param=max-rtl-if-conversion-unpredictable-cost=100" } */

void sink3 (long, long, long);

void
f (long c, long p, long q, long t, long e)
{
  long a;
  if (c > 7)
    {
      a = p + 1;
      t = a + q;
      a = t + 3;
    }
  else
    {
      a = p - 3;
      e = q - 4;
    }
  sink3 (a, t, e);
}

/* { dg-final { scan-rtl-dump-not "if-conversion succeeded through noce_convert_multiple_sets" "ce1" } } */
