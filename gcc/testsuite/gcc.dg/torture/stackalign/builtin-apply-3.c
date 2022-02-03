/* PR middle-end/12210 */
/* Origin: Ossadchy Yury A. <waspcoder@mail.ru> */

/* This used to fail on i686 because the argument was not copied
   to the right location by __builtin_apply after the direct call.  */

/* { dg-do run } */
/* { dg-require-effective-target untyped_assembly } */


#define INTEGER_ARG  5

extern void abort(void);

void foo(int arg)
{
  if (arg != INTEGER_ARG)
    abort();
}

void bar(int arg)
{
  foo(arg);
  __builtin_apply(foo, __builtin_apply_args(), 16);
}

int main(void)
{
  /* Allocate 64 bytes on the stack to make sure that __builtin_apply
     can read at least 64 bytes above the return address.  */
  char dummy[64];

  bar(INTEGER_ARG);

  return 0;
}
