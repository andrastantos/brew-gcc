/* { dg-do compile { target { llp64 } } } */
/* { dg-options "-O2 -mabi=ms -mtune-ctrl=^prologue_using_move,^epilogue_using_move -mno-push-args -fomit-frame-pointer -mtune=generic" } */
/* Keep labels and directives ('.seh_endprologue').  */
/* { dg-final { check-function-bodies "**" "" "" { target "*-*-*" } {^\t?\.} } } */

/*
**entry:
**	subq	\$72, %rsp
**	.seh_stackalloc	72
**	.seh_endprologue
**	movl	120\(%rsp\), %eax
**	movl	%eax, 40\(%rsp\)
**	movl	112\(%rsp\), %eax
**	movl	\$-559038737, 48\(%rsp\)
**	movl	%eax, 32\(%rsp\)
**	call	continuation
**	nop
**	addq	\$72, %rsp
**	ret
*/

#include "preserve-none-31a.c"
