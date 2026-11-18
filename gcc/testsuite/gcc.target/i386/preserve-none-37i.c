/* { dg-do compile { target { *-*-linux* && lp64 } } } */
/* { dg-options "-O2 -mabi=ms -mtune-ctrl=^prologue_using_move,^epilogue_using_move -mno-push-args -fomit-frame-pointer -march=x86-64" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target "*-*-*" } {^\t?\.}  } } */

/*
**entry:
**.LFB0:
**	.cfi_startproc
**	subq	\$8, %rsp
**	.cfi_def_cfa_offset 16
**	movsd	72\(%rsp\), %xmm7
**	movsd	64\(%rsp\), %xmm6
**	movsd	56\(%rsp\), %xmm5
**	movsd	48\(%rsp\), %xmm4
**	call	continuation
**	addq	\$8, %rsp
**	.cfi_def_cfa_offset 8
**	ret
**	.cfi_endproc
**...
*/

#define CALLEE_ATTRIBUTE __attribute__ ((preserve_none, sysv_abi))
#define CALLER_ATTRIBUTE __attribute__ ((preserve_none))

#include "preserve-none-37a.c"
