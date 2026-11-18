/* { dg-do compile } */
/* { dg-options "-O2 -msse4.1" } */

typedef int v4si __attribute__ ((__vector_size__ (16)));

v4si sz_0(v4si x) { x[0]=0; return x; }
v4si sz_1(v4si x) { x[1]=0; return x; }
v4si sz_2(v4si x) { x[2]=0; return x; }
v4si sz_3(v4si x) { x[3]=0; return x; }

v4si sz_01(v4si x) { x[0]=0; x[1]=0; return x; }
v4si sz_02(v4si x) { x[0]=0; x[2]=0; return x; }
v4si sz_03(v4si x) { x[0]=0; x[3]=0; return x; }
v4si sz_12(v4si x) { x[1]=0; x[2]=0; return x; }
v4si sz_13(v4si x) { x[1]=0; x[3]=0; return x; }
v4si sz_23(v4si x) { x[2]=0; x[3]=0; return x; }

v4si sz_012(v4si x) { x[0]=0; x[1]=0; x[2]=0; return x; }
v4si sz_013(v4si x) { x[0]=0; x[1]=0; x[3]=0; return x; }
v4si sz_023(v4si x) { x[0]=0; x[2]=0; x[3]=0; return x; }
// v4si sz_123(v4si x) { x[1]=0; x[2]=0; x[3]=0; return x; }

/* { dg-final { scan-assembler-times "\tv?insertps\t" 13 } } */
