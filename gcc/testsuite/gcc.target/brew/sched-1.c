/* With the Espresso pipeline description, the second scheduler moves the
   independent load up behind the first one, into the cycles the first
   load's result takes, instead of waiting for that result first.  */
/* { dg-do compile } */
/* { dg-options "-O2" } */

int
f (int *p, int *q)
{
  int x = *p;
  int y = x + 1;
  int z = *q;
  return y * z;
}

/* { dg-final { scan-assembler {<- mem\[\$a0\][^\n]*\n\t\$[a-z0-9]+ <- mem\[\$a1\]} } } */
