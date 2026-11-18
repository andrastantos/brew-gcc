/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" "-Os" "-Oz" } } */
/* { dg-options "-O2 -march=rv64gc_zicond -mabi=lp64d -mtune=generic -mbranch-cost=3" } */

extern void consume (void *, void *, void *);

__attribute__ ((noinline, noclone))
void
select_three_pointers_generic (long x, long y,
			       void *a0, void *a1,
			       void *b0, void *b1,
			       void *c0, void *c1)
{
  if (x >= y)
    {
      a0 = a1;
      b0 = b1;
      c0 = c1;
    }

  consume (a0, b0, c0);
}

/* The generic tuning model has an issue rate of one, so the three pointer
   selections are not profitable at a branch cost of three.  */

/* { dg-final { scan-assembler-times "\\sbge\\s" 1 } } */
/* { dg-final { scan-assembler-not "\\sczero\\.eqz\\s" } } */
/* { dg-final { scan-assembler-not "\\sczero\\.nez\\s" } } */
