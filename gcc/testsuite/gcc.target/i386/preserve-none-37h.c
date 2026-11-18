/* { dg-do compile { target { *-*-linux* && lp64 } } } */
/* { dg-options "-O2 -mabi=ms -mtune-ctrl=^prologue_using_move,^epilogue_using_move -mno-push-args -fomit-frame-pointer -march=x86-64" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target "*-*-*" } {^\t?\.}  } } */

/*
**entry:
**.LFB0:
**	.cfi_startproc
**	subq	\$72, %rsp
**	.cfi_def_cfa_offset 80
**	movsd	%xmm7, 56\(%rsp\)
**	movsd	%xmm6, 48\(%rsp\)
**	movsd	%xmm5, 40\(%rsp\)
**	movsd	%xmm4, 32\(%rsp\)
**	call	continuation
**	addq	\$72, %rsp
**	.cfi_def_cfa_offset 8
**	ret
**	.cfi_endproc
**...
*/

#define CALLEE_ATTRIBUTE __attribute__ ((preserve_none))
#define CALLER_ATTRIBUTE __attribute__ ((preserve_none, sysv_abi))

#include "preserve-none-37a.c"
