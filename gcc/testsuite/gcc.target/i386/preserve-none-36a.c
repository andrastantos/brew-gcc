/* { dg-do compile { target { *-*-linux* && lp64 } } } */
/* { dg-options "-O2 -mtune-ctrl=^prologue_using_move,^epilogue_using_move -mno-push-args -fomit-frame-pointer -mtune=generic" } */
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

typedef float v4sf __attribute__((vector_size(16)));

extern void continuation (long, long, long, long, long, v4sf)
    CALLEE_ATTRIBUTE;

CALLER_ATTRIBUTE
void
entry (long arg1, long arg2, long arg3, long arg4, long arg5, v4sf arg6)
{
  continuation (arg1, arg2, arg3, arg4, arg5, arg6);
}
