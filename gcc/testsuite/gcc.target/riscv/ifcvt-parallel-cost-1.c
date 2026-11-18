/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" "-Os" "-Oz" } } */
/* { dg-options "-O2 -march=rv64gc_zicond -mabi=lp64d -mtune=xt-c9501fdvt -mbranch-cost=3" } */

extern void consume (void *, void *, void *);

__attribute__ ((noinline, noclone))
void
select_three_pointers (long x, long y,
		       void *a0, void *a1,
		       void *b0, void *b1,
		       void *c0, void *c1)
{
  if (x > y)
    {
      a0 = a1;
      b0 = b1;
      c0 = c1;
    }

  consume (a0, b0, c0);
}

/* { dg-final { scan-assembler-times "\\sczero\\.eqz\\s" 3 } } */
/* { dg-final { scan-assembler-times "\\sczero\\.nez\\s" 3 } } */
/* { dg-final { scan-assembler-not "\\s(?:bge|bgt|ble|blt)\\s" } } */
