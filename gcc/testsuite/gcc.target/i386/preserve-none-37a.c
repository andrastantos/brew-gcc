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

#ifndef CALLEE_ATTRIBUTE
#define CALLEE_ATTRIBUTE __attribute__ ((preserve_none, sysv_abi))
#endif

#ifndef CALLER_ATTRIBUTE
#define CALLER_ATTRIBUTE __attribute__ ((preserve_none, sysv_abi))
#endif

extern void continuation (double, double, double, double,
			  double, double, double, double)
    CALLEE_ATTRIBUTE;

CALLER_ATTRIBUTE
void
entry (double arg1, double arg2, double arg3, double arg4,
       double arg5, double arg6, double arg7, double arg8)
{
  continuation (arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
}
