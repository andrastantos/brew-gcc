/* { dg-do compile { target { *-*-linux* && lp64 } } } */
/* { dg-options "-O2 -mtune-ctrl=^prologue_using_move,^epilogue_using_move -mno-push-args -fomit-frame-pointer -march=x86-64" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target "*-*-*" } {^\t?\.}  } } */

/*
**entry:
**.LFB0:
**	.cfi_startproc
**	subq	\$72, %rsp
**	.cfi_def_cfa_offset 80
**	movq	120\(%rsp\), %rax
**	movaps	\(%rax\), %xmm0
**	leaq	48\(%rsp\), %rax
**	movq	%rax, 40\(%rsp\)
**	movq	112\(%rsp\), %rax
**	movaps	%xmm0, 48\(%rsp\)
**	movq	%rax, 32\(%rsp\)
**	call	continuation
**	addq	\$72, %rsp
**	.cfi_def_cfa_offset 8
**	ret
**	.cfi_endproc
**...
*/

#define CALLEE_ATTRIBUTE __attribute__ ((preserve_none, ms_abi))
#define CALLER_ATTRIBUTE __attribute__ ((preserve_none, ms_abi))

#include "preserve-none-36a.c"
