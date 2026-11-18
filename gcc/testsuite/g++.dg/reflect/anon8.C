// CWG3130 - Naming function members of anonymous unions
// { dg-do run { target c++26 } }
// { dg-additional-options "-freflection" }

#include <meta>

struct A { union { int a; long b; }; };
using U = typename [: parent_of (^^A::a) :];

long
foo (U &x)
{
  return x.b;
}

int
main ()
{
  A a;
  a.b = 42;
  constexpr auto ctx = std::meta::access_context::unchecked ();
  if (foo (a.[: members_of (^^A, ctx)[1] :]) != 42)
    __builtin_abort ();
}
