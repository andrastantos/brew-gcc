/* { dg-do compile { target { llp64 } } } */
/* { dg-options "-O2 -mabi=ms -mtune-ctrl=^prologue_using_move,^epilogue_using_move -mno-push-args -fomit-frame-pointer -mtune=generic" } */
/* Keep labels and directives ('.seh_endprologue').  */
/* { dg-final { check-function-bodies "**" "" "" { target "*-*-*" } {^\t?\.} } } */

/*
**entry:
**	.seh_endprologue
**	jmp	continuation
*/

extern void continuation (double, int, int, int, int, int, int)
  __attribute__ ((preserve_none));

__attribute__ ((no_callee_saved_registers))
void
entry (double fp, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6)
{
  continuation (fp, arg1, arg2, arg3, arg4, arg5, arg6);
}
