// PR c++/67064
// { dg-options "-w" }

struct s {
  int i;
};

#ifdef __hppa__
/* Register %r1 can't be fixed when generating PIC code.  */
register struct s *reg __asm__( "4" );
#elif defined __BREW__
/* Register $r1 is the frame pointer, which can't be used as a variable. */
register struct s *reg __asm__( "4" );
#else
register struct s *reg __asm__( "1" );
#endif

int f(void)
{
  int i;

  i = reg->i;
  i = (reg)->i;

  return i;
}
