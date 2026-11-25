/* An explicit -freorder-blocks-algorithm overrides Brew's default.  */
/* { dg-do compile } */
/* { dg-options "-O2 -freorder-blocks-algorithm=stc -fdump-rtl-bbro-details" } */

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
