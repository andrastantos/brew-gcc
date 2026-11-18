/* { dg-do compile { target { *-*-linux* && lp64 } } } */
/* { dg-options "-O2 -mtune-ctrl=^prologue_using_move,^epilogue_using_move -mno-push-args -fomit-frame-pointer -march=x86-64" } */
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

#define CALLEE_ATTRIBUTE __attribute__ ((preserve_none, ms_abi))
#define CALLER_ATTRIBUTE __attribute__ ((no_callee_saved_registers, ms_abi))

#include "preserve-none-37a.c"
