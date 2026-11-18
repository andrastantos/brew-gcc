// PR c++/126354
// { dg-do compile { target c++26 } }

[[noreturn]] constexpr void
foo (bool x)
{
  if (x)
    throw 42;
}

consteval {
  try
    {
      foo (true);
      throw nullptr;
    }
  catch (int x)
    {
      if (x != 42)
	throw nullptr;
      return;
    }
  throw nullptr;
}

consteval {
  foo (false);		// { dg-error "'\\\[\\\[noreturn\\\]\\\]' call returns" }
}
