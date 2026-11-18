/* { dg-do run } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O2 -mtune=generic -fdump-rtl-ce1" } */
/* { dg-additional-options "--param=max-rtl-if-conversion-insns=3 --param=max-rtl-if-conversion-predictable-cost=100 --param=max-rtl-if-conversion-unpredictable-cost=100" } */

/* The arms have disjoint live-out destinations.  The speculative xor
   clobbers FLAGS, so the conversion must not reuse the incoming
   comparison.  */

volatile long ga, gb, gd;

__attribute__ ((noipa)) void
f (long c, long x, long y, long a, long b, long d)
{
  if (__builtin_expect (c > 7, 0))
    {
      a = x + 1;
      b = y + 2;
    }
  else
    d = x ^ y;
  ga = a;
  gb = b;
  gd = d;
}

/* Keep the runtime driver out of noce so that the dump count is specific to
   F.  */
__attribute__ ((optimize ("O0"))) int
main (void)
{
  for (long c = 5; c != 11; ++c)
    for (long x = -8; x != 9; ++x)
      for (long y = -8; y != 9; ++y)
	{
	  f (c, x, y, 2, 4, 6);
	  if (ga != (c > 7 ? x + 1 : 2)
	      || gb != (c > 7 ? y + 2 : 4)
	      || gd != (c > 7 ? 6 : (x ^ y)))
	    __builtin_abort ();
	}
  return 0;
}

/* { dg-final { scan-rtl-dump-times "if-conversion succeeded through noce_convert_multiple_sets" 1 "ce1" } } */
