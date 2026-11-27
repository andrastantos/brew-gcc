/* -march=brew-nomul: no multiply instruction; a multiplication calls
   __mulsi3 (constant multipliers become additions and shifts), and
   __BREW_MUL__ is not defined.  */
/* { dg-do compile } */
/* { dg-options "-O2 -march=brew-nomul" } */

#ifdef __BREW_MUL__
#error __BREW_MUL__ defined
#endif
#ifndef __BREW_SHIFT__
#error __BREW_SHIFT__ not defined
#endif

unsigned mul (unsigned a, unsigned b) { return a * b; }
unsigned mul10 (unsigned a) { return a * 10; }

/* { dg-final { scan-assembler {call \(?__mulsi3\)?} } } */
/* { dg-final { scan-assembler-not {<- [^\n]*\*} } } */
