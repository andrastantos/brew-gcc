/* $rA + N addresses cost no more than $rA when optimizing for speed, so
   ivopts addresses a[j] and a[j + 1] from one induction variable instead
   of keeping a second pointer for the load.  */
/* { dg-do compile } */
/* { dg-options "-O2" } */

void
isort (int *a, int n)
{
  for (int i = 1; i < n; i++)
    {
      int x = a[i];
      int j = i - 1;
      while (j >= 0 && a[j] > x)
	{
	  a[j + 1] = a[j];
	  j--;
	}
      a[j + 1] = x;
    }
}

/* { dg-final { scan-assembler {<- mem\[\$r[0-9]+ \+ -4\]} } } */
