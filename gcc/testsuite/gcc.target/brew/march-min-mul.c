/* -mmul overrides -march=brew-min: the multiply instruction is used,
   shifts still aren't.  */
/* { dg-do compile } */
/* { dg-options "-O2 -march=brew-min -mmul" } */

#if !defined (__BREW_MUL__) || defined (__BREW_SHIFT__)
#error wrong __BREW_MUL__ / __BREW_SHIFT__
#endif

unsigned mul (unsigned a, unsigned b) { return a * b; }
unsigned lshr (unsigned a, int n) { return a >> n; }

/* { dg-final { scan-assembler {<- \$a0 \* \$a1} } } */
/* { dg-final { scan-assembler {call \(__brew_lshr\)} } } */
