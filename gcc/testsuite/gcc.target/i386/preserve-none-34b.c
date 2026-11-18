/* { dg-do compile { target { llp64 } } } */
/* { dg-options "-O2 -mabi=ms -mtune-ctrl=^prologue_using_move,^epilogue_using_move -mno-push-args -fomit-frame-pointer -mtune=generic" } */
/* Keep labels and directives ('.seh_endprologue').  */
/* { dg-final { check-function-bodies "**" "" "" { target "*-*-*" } {^\t?\.} } } */

/*
**entry:
**	.seh_endprologue
**	jmp	continuation
*/

#include "preserve-none-34a.c"
