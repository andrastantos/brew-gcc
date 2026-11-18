/* { dg-do run } */
/* { dg-options "-O2 -fno-ssa-phiopt -fno-tree-ter -fno-tree-coalesce-vars -fdump-rtl-ce1" } */
/* { dg-additional-options "--param=max-rtl-if-conversion-unpredictable-cost=100 --param=max-rtl-if-conversion-predictable-cost=100" } */

volatile long ga, gt, ge, gw;

__attribute__ ((noipa)) void
disjoint_arm_values (long a, long b, long c, long d, long s,
		     long x, long y, long z, long w)
{
  if (s > 0)
    {
      x = a;
      y = b - 2;
    }
  else
    {
      z = c;
      w = d - 4;
    }
  ga = x;
  gt = y;
  ge = z;
  gw = w;
}

/* Each arm has one set and defines a different live-out destination.  */
__attribute__ ((noipa)) void
single_set_arms (long c, long a, long b, long x, long y)
{
  if (c > 0)
    x = a + 1;
  else
    y = b - 1;
  ga = x;
  gt = y;
}

/* The arms share X through register copies and compute distinct live-out
   destinations.  */
__attribute__ ((noipa)) void
partially_overlapping_values (long c, long a, long b,
			      long x, long t, long e)
{
  if (c > 0)
    {
      x = a;
      t = b + 1;
    }
  else
    {
      x = b;
      e = a - 1;
    }
  ga = x;
  gt = t;
  ge = e;
}

__attribute__ ((optimize ("O0"))) int
main (void)
{
  disjoint_arm_values (10, 20, 30, 40, 1, 50, 60, 70, 80);
  if (ga != 10 || gt != 18 || ge != 70 || gw != 80)
    __builtin_abort ();
  disjoint_arm_values (10, 20, 30, 40, 0, 50, 60, 70, 80);
  if (ga != 50 || gt != 60 || ge != 30 || gw != 36)
    __builtin_abort ();

  single_set_arms (1, 10, 20, 30, 40);
  if (ga != 11 || gt != 40)
    __builtin_abort ();
  single_set_arms (0, 10, 20, 30, 40);
  if (ga != 30 || gt != 19)
    __builtin_abort ();

  partially_overlapping_values (1, 10, 20, 30, 40, 50);
  if (ga != 10 || gt != 21 || ge != 50)
    __builtin_abort ();
  partially_overlapping_values (0, 10, 20, 30, 40, 50);
  if (ga != 20 || gt != 40 || ge != 9)
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-rtl-dump-times "if-conversion succeeded through noce_convert_multiple_sets" 3 "ce1" } } */
