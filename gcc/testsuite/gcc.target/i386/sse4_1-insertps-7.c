/* { dg-do compile } */
/* { dg-options "-O2 -msse4.1" } */

typedef float v4sf __attribute__ ((__vector_size__ (16)));

v4sf sz_0(v4sf x) { x[0]=0.0f; return x; }
v4sf sz_1(v4sf x) { x[1]=0.0f; return x; }
v4sf sz_2(v4sf x) { x[2]=0.0f; return x; }
v4sf sz_3(v4sf x) { x[3]=0.0f; return x; }

v4sf sz_01(v4sf x) { x[0]=0.0f; x[1]=0.0f; return x; }
v4sf sz_02(v4sf x) { x[0]=0.0f; x[2]=0.0f; return x; }
v4sf sz_03(v4sf x) { x[0]=0.0f; x[3]=0.0f; return x; }
v4sf sz_12(v4sf x) { x[1]=0.0f; x[2]=0.0f; return x; }
v4sf sz_13(v4sf x) { x[1]=0.0f; x[3]=0.0f; return x; }
v4sf sz_23(v4sf x) { x[2]=0.0f; x[3]=0.0f; return x; }

v4sf sz_012(v4sf x) { x[0]=0.0f; x[1]=0.0f; x[2]=0.0f; return x; }
v4sf sz_013(v4sf x) { x[0]=0.0f; x[1]=0.0f; x[3]=0.0f; return x; }
v4sf sz_023(v4sf x) { x[0]=0.0f; x[2]=0.0f; x[3]=0.0f; return x; }

#if 0
v4sf sz_123(v4sf x) { x[1]=0.0f; x[2]=0.0f; x[3]=0.0f; return x; }
#endif

/* { dg-final { scan-assembler-times "\tv?insertps\t" 13 } } */
