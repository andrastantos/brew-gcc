// { dg-do compile { target c++26 } }
// { dg-options "-freflection" }

#include <meta>

struct A { struct { int a; long b; }; }; // { dg-message "anonymous struct declared here" }
using U = typename [: parent_of (^^A::a) :];
U b;					// { dg-error "declaration of variable 'b' with anonymous struct type 'U' {aka 'A::<unnamed struct>'}" }
struct B { U b; };			// { dg-error "declaration of member 'B::b' with anonymous struct type 'U' {aka 'A::<unnamed struct>'}" }
U &plugh ();
auto c = plugh ();			// { dg-error "declaration of variable 'c' with anonymous struct type 'A::<unnamed struct>'" }
					// { dg-error "temporary object with anonymous struct type 'A::<unnamed struct>'" "" { target *-*-* } .-1 }
U g;					// { dg-error "declaration of variable 'g' with anonymous struct type 'U' {aka 'A::<unnamed struct>'}" }
struct E { U e[2]; };			// { dg-error "declaration of member 'E::e' with array of anonymous struct type 'U \\\[2\\\]' {aka 'A::<unnamed struct> \\\[2\\\]'}" }
struct F { static U f; };		// { dg-error "declaration of member 'F::f' with anonymous struct type 'U' {aka 'A::<unnamed struct>'}" }
struct G { static U g[2][42]; };	// { dg-error "declaration of member 'G::g' with array of anonymous struct type 'U \\\[2\\\]\\\[42\\\]' {aka 'A::<unnamed struct> \\\[2\\\]\\\[42\\\]'}" }
using UA = U[2];

void
foo (U x)				// { dg-error "declaration of parameter 'x' with anonymous struct type 'U' {aka 'A::<unnamed struct>'}" }
{
  U ({ .a = 1 });			// { dg-error "temporary object with anonymous struct type 'U' {aka 'A::<unnamed struct>'}" }
}

void
foo (U &x)
{
  throw x;				// { dg-error "'throw' operand has anonymous struct type 'U' {aka 'A::<unnamed struct>'}" }
}

void
bar ()
{
  U y;					// { dg-error "declaration of variable 'y' with anonymous struct type 'U' {aka 'A::<unnamed struct>'}" }
  U r[4];				// { dg-error "object with array of anonymous struct type 'U' {aka 'A::<unnamed struct>'}" }
  union W { U u; } v;			// { dg-error "declaration of member 'bar\\\(\\\)::W::u' with anonymous struct type 'U' {aka 'A::<unnamed struct>'}" }
  try
    {
    }
  catch (U z)				// { dg-error "'catch' parameter with anonymous struct type 'U' {aka 'A::<unnamed struct>'}" }
    {
    }
  try
    {
    }
  catch (U)				// { dg-error "'catch' parameter with anonymous struct type 'U' {aka 'A::<unnamed struct>'}" }
    {
    }
}

void
baz (U)					// { dg-error "declaration of a parameter with anonymous struct type 'U' {aka 'A::<unnamed struct>'}" }
{
}

struct D : public U {};			// { dg-error "base type 'U' {aka 'A::<unnamed struct>'} is anonymous struct type" }

long
garply (const U &x)
{
  return x.b;
}

void
corge ()
{
  garply (U { .b = 42 });		// { dg-error "temporary object with anonymous struct type 'U' {aka 'A::<unnamed struct>'}" }
}

U *
xyzzy ()
{
  return new U { .b = 42 };		// { dg-error "temporary object with anonymous struct type 'U' {aka 'A::<unnamed struct>'}" }
}

void qux (U);
void fred (U x);
void waldo (int, ...);

void
boo (U &x)
{
  qux (x);				// { dg-error "temporary object with anonymous struct type 'U' {aka 'A::<unnamed struct>'}" }
  fred (x);				// { dg-error "temporary object with anonymous struct type 'U' {aka 'A::<unnamed struct>'}" }
  waldo (1, x);				// { dg-error "temporary object with anonymous struct type 'U' {aka 'A::<unnamed struct>'}" }
}
