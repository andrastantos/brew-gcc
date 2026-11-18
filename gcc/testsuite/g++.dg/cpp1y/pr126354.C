// PR c++/126354
// { dg-do compile { target c++14 } }

[[noreturn]] constexpr void
foo (bool x)
{
  if (x)
    for (;;)
      ;
}

constexpr bool
bar ()
{
  foo (false);		// { dg-error "'\\\[\\\[noreturn\\\]\\\]' call returns" }
}

constexpr bool a = bar ();
