/* -march=brew-min: neither multiply nor shift instructions; a left shift
   by a small constant becomes additions.  An explicit -mmul overrides
   -march (march-min-mul.c).  */
/* { dg-do compile } */
/* { dg-options "-O2 -march=brew-min" } */

#if defined (__BREW_MUL__) || defined (__BREW_SHIFT__)
#error __BREW_MUL__ or __BREW_SHIFT__ defined
#endif

unsigned mul (unsigned a, unsigned b) { return a * b; }
unsigned shl3 (unsigned a) { return a << 3; }
int ashr (int a, int n) { return a >> n; }

/* { dg-final { scan-assembler {call \(?__mulsi3\)?} } } */
/* { dg-final { scan-assembler {call \(__brew_ashr\)} } } */
/* { dg-final { scan-assembler-times {(\$[0-9a-z]+) <- \1 \+ \1} 3 } } */
/* { dg-final { scan-assembler-not {<- [^\n]*\*} } } */
/* { dg-final { scan-assembler-not {<<|>>} } } */
