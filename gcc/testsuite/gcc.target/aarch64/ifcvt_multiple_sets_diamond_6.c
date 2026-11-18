/* Test dependencies in diamonds with arm-only live-out destinations.  */
/* { dg-do run } */
/* { dg-options "-O2 -fno-tree-ter -fno-tree-coalesce-vars -fdump-rtl-ce1" } */
/* { dg-additional-options "--param=max-rtl-if-conversion-unpredictable-cost=100 --param=max-rtl-if-conversion-predictable-cost=100" } */

volatile long ga, gt, ge, gu;

/* Each arm reads the value that the other arm can replace.  The else
   arm also defines E twice with a dependent live-out value in between.  */
__attribute__ ((noipa)) void
cross_arm_repeated (long c, long p, long q, long t, long e)
{
  long k;
  if (c > 7)
    {
      t = e + p;
      k = q + 1;
    }
  else
    {
      e = t + q;
      k = e ^ p;
      e = k + 3;
    }
  ga = k;
  gt = t;
  ge = e;
}

/* C is both a condition input and an arm-only live-out destination.  */
__attribute__ ((noipa)) void
condition_input (long c, long p, long q, long a)
{
  long t;
  if (c > 7)
    {
      a = p + 1;
      t = q + 2;
    }
  else
    {
      t = p - 3;
      c = q - 4;
    }
  ga = a;
  gt = t;
  ge = c;
}

/* One arm has more live-out destinations.  The conversion keeps the CFG arm
   order.  */
__attribute__ ((noipa)) void
unequal_output_counts (long c, long p, long q, long t, long e, long u)
{
  long a;
  if (__builtin_expect (c > 7, 1))
    {
      a = p + 1;
      t = q + 2;
    }
  else
    {
      a = p - 3;
      e = q - 4;
      u = p + q;
    }
  ga = a;
  gt = t;
  ge = e;
  gu = u;
}

__attribute__ ((optimize ("O0"))) int
main (void)
{
  cross_arm_repeated (8, 10, 20, 31, 47);
  if (ga != 21 || gt != 57 || ge != 47)
    __builtin_abort ();
  cross_arm_repeated (7, 10, 20, 31, 47);
  if (ga != 57 || gt != 31 || ge != 60)
    __builtin_abort ();

  condition_input (8, 10, 20, 47);
  if (ga != 11 || gt != 22 || ge != 8)
    __builtin_abort ();
  condition_input (7, 10, 20, 47);
  if (ga != 47 || gt != 7 || ge != 16)
    __builtin_abort ();

  unequal_output_counts (8, 10, 20, 31, 47, 59);
  if (ga != 11 || gt != 22 || ge != 47 || gu != 59)
    __builtin_abort ();
  unequal_output_counts (7, 10, 20, 31, 47, 59);
  if (ga != 7 || gt != 31 || ge != 16 || gu != 30)
    __builtin_abort ();

  return 0;
}

/* { dg-final { scan-rtl-dump-times "if-conversion succeeded through noce_convert_multiple_sets" 3 "ce1" } } */
