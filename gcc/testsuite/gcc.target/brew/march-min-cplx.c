/* Without a barrel shifter, splitting a _Complex char argument at
   function entry takes shifts.  They used to be library calls, and setting
   up the call's arguments overwrote the second incoming argument in $a1
   before it was saved; the shifts call __brew_lshr through a pattern that
   uses no argument register now.  */
/* { dg-do run } */
/* { dg-options "-O0 -march=brew-min" } */

extern void abort (void);

_Complex char g;

void __attribute__ ((noipa))
init (_Complex char *p, _Complex char v)
{
  *p = v;
}

int __attribute__ ((noipa))
ne (_Complex char x, _Complex char v)
{
  return x != v;
}

int
main (void)
{
  _Complex char v;
  __real__ v = 3;
  __imag__ v = 5;
  init (&g, v);
  if (__real__ g != 3 || __imag__ g != 5 || ne (g, v))
    abort ();
  return 0;
}
