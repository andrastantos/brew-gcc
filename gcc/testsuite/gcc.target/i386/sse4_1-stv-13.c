/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2 -msse4.1" } */

typedef long long v2di __attribute__ ((__vector_size__ (16)));

long long ext();
v2di mem;

void bar()
{
  long long x = ext();
  mem = (v2di){x,0};
}

/* { dg-final { scan-assembler "pinsrd" } } */
/* { dg-final { scan-assembler-not "movq" } } */
