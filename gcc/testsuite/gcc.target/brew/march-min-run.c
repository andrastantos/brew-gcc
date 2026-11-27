/* The libgcc routines that replace the multiply and shift instructions on
   reduced cores (__mulsi3, __ashlsi3, __lshrsi3, __ashrsi3), checked
   against values computed on the host: a hash of a * b and of every shift
   by 0..31 over a set of interesting values.  Built for brew-min, so every
   operation on variables calls them.  */
/* { dg-do run } */
/* { dg-options "-O2 -march=brew-min" } */

extern void abort (void);

static const unsigned vals[] = {
  0x0u, 0x1u, 0x2u, 0x3u, 0x7u, 0x7fu, 0x80u, 0xffu, 0x1234u, 0xffffu, 0x10000u, 0x12345678u, 0x7fffffffu, 0x80000000u, 0x80000001u, 0xdeadbeefu, 0xffffffffu
};
#define N (sizeof (vals) / sizeof (vals[0]))

/* Keep GCC from folding the operations at compile time.  */
static unsigned __attribute__ ((noipa)) id (unsigned x) { return x; }

int
main (void)
{
  unsigned mul = 0, shl = 0, lshr = 0, ashr = 0;
  for (unsigned i = 0; i < N; i++)
    {
      unsigned a = id (vals[i]);
      for (unsigned j = 0; j < N; j++)
	mul = mul * 31 + a * id (vals[j]);
      for (int n = 0; n < 32; n++)
	{
	  int k = (int) id (n);
	  shl = shl * 31 + (a << k);
	  lshr = lshr * 31 + (a >> k);
	  ashr = ashr * 31 + (unsigned) ((int) a >> k);
	}
    }
  if (mul != 0x400f2b90u || shl != 0x5e3756f4u || lshr != 0xcbc8e8b2u || ashr != 0x4dbab15au)
    abort ();
  return 0;
}
