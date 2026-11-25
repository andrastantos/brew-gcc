/* -O2 lays out basic blocks with the "simple" algorithm on Brew: it takes
   fewer branches than "stc" (brew-common.cc).  */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-rtl-bbro-details" } */

int
f (int *p, int n)
{
  int s = 0;
  for (int i = 0; i < n; i++)
    if (p[i] & 1)
      s += p[i];
  return s;
}

/* { dg-final { scan-rtl-dump {Reordering with the "simple" algorithm} "bbro" } } */
