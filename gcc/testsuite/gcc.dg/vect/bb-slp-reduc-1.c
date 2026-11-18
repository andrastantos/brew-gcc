/* { dg-do compile } */
/* { dg-require-effective-target vect_double } */

double foo (double *x, double *y, double *z)
{
  return (x[0] * y[0] - z[0]) + (x[1] * y[1] - z[1]);
}

/* Even though without -ffast-math a reduction with double requires in-order
   vectorization which we do not fully implement for BB vectorization we
   should be able to handle the two-lane vector case just fine.  */
/* { dg-final { scan-tree-dump "optimized: basic block part vectorized" "slp2" { target vect_hw_misalign } } } */
