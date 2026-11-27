/* -march=brew-noshift: no shift instruction; shifts call libgcc's
   __brew_ashl, __brew_lshr and __brew_ashr, except a left shift by a
   constant, which becomes a multiplication by the power of two.  */
/* { dg-do compile } */
/* { dg-options "-O2 -march=brew-noshift" } */

#ifndef __BREW_MUL__
#error __BREW_MUL__ not defined
#endif
#ifdef __BREW_SHIFT__
#error __BREW_SHIFT__ defined
#endif

unsigned shl (unsigned a, int n) { return a << n; }
unsigned lshr (unsigned a, int n) { return a >> n; }
int ashr (int a, int n) { return a >> n; }
unsigned shl20 (unsigned a) { return a << 20; }

/* { dg-final { scan-assembler {call \(__brew_ashl\)} } } */
/* { dg-final { scan-assembler {call \(__brew_lshr\)} } } */
/* { dg-final { scan-assembler {call \(__brew_ashr\)} } } */
/* { dg-final { scan-assembler {<- [^\n]*\* 1048576} } } */
/* { dg-final { scan-assembler-not {<<|>>} } } */
