/* { dg-do compile { target { *-*-linux* && lp64 } } } */
/* { dg-options "-O2 -mabi=ms -mtune-ctrl=^prologue_using_move,^epilogue_using_move -mno-push-args -fomit-frame-pointer -march=x86-64" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target "*-*-*" } {^\t?\.}  } } */

/*
**entry:
**.LFB0:
**	.cfi_startproc
**	jmp	continuation
**	.cfi_endproc
**...
*/

#define CALLEE_ATTRIBUTE __attribute__ ((preserve_none))
#define CALLER_ATTRIBUTE __attribute__ ((preserve_none))

#include "preserve-none-37a.c"
