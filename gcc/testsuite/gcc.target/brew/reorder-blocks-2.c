/* -Os lays out basic blocks with the "stc" algorithm on Brew: it takes
   fewer branches than "simple" at no cost in size (brew-common.cc).  */
/* { dg-do compile } */
/* { dg-options "-Os -fdump-rtl-bbro-details" } */

int
f (int *p, int n)
{
  int s = 0;
  for (int i = 0; i < n; i++)
    if (p[i] & 1)
      s += p[i];
  return s;
}

/* { dg-final { scan-rtl-dump {Reordering with the STC algorithm} "bbro" } } */
