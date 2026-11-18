// { dg-do run { target c++26 } }
// { dg-options "-freflection" }

#include <meta>

struct A { struct { int a; long b; }; };
using U = typename [: parent_of (^^A::a) :];

long
foo (U &x)
{
  return x.a + x.b;
}

int
main ()
{
  A a;
  a.a = 2;
  a.b = 42;
  constexpr auto ctx = std::meta::access_context::unchecked ();
  if (foo (a.[: members_of (^^A, ctx)[1] :]) != 44)
    __builtin_abort ();
}
