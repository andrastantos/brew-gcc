/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-widening_mul" } */

unsigned
f (unsigned a, unsigned b)
{
  unsigned sum = a + b;
  return a <= ~b ? sum : ~0u;
}

/* { dg-final { scan-tree-dump {ADD_OVERFLOW \(a_[^,]*, b_} "widening_mul" } } */
