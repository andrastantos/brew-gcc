/* { dg-do compile { target { *-*-linux* && lp64 } } } */
/* { dg-options "-O2 -mabi=ms -mtune-ctrl=^prologue_using_move,^epilogue_using_move -mno-push-args -fomit-frame-pointer -mtune=generic" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target "*-*-*" } {^\t?\.}  } } */

/*
**entry:
**.LFB0:
**	.cfi_startproc
**	subq	\$72, %rsp
**	.cfi_def_cfa_offset 80
**	movl	120\(%rsp\), %eax
**	movl	\$-559038737, 48\(%rsp\)
**	movl	%eax, 40\(%rsp\)
**	movl	112\(%rsp\), %eax
**	movl	%eax, 32\(%rsp\)
**	call	continuation
**	addq	\$72, %rsp
**	.cfi_def_cfa_offset 8
**	ret
**	.cfi_endproc
**...
*/

extern void continuation (int, int, int, int, int, int, ...)
  __attribute__ ((preserve_none));

__attribute__ ((no_callee_saved_registers))
void
entry (int arg1, int arg2, int arg3, int arg4, int arg5, int arg6)
{
  continuation (arg1, arg2, arg3, arg4, arg5, arg6, 0xdeadbeef);
}
