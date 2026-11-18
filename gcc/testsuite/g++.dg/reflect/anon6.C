// CWG3130 - Naming function members of anonymous unions
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

#include <meta>

struct A { union { int a; long b; }; };	// { dg-message "anonymous union declared here" }
using U = typename [: parent_of (^^A::a) :];
U b;					// { dg-error "declaration of variable 'b' with anonymous union type 'U' {aka 'A::<unnamed union>'}" }
struct B { U b; };			// { dg-error "declaration of member 'B::b' with anonymous union type 'U' {aka 'A::<unnamed union>'}" }
U &plugh ();
auto c = plugh ();			// { dg-error "declaration of variable 'c' with anonymous union type 'A::<unnamed union>'" }
					// { dg-error "temporary object with anonymous union type 'A::<unnamed union>'" "" { target *-*-* } .-1 }
static union { int d; long e; };	// { dg-message "anonymous union declared here" }
using V = typename [: parent_of (^^e) :];
V &thud () { throw 1; }
auto f = thud ();			// { dg-error "declaration of variable 'f' with anonymous union type '<unnamed union>'" }
					// { dg-error "temporary object with anonymous union type '<unnamed union>'" "" { target *-*-* } .-1 }
V g;					// { dg-error "declaration of variable 'g' with anonymous union type 'V' {aka '<unnamed union>'}" }
struct D { V d; };			// { dg-error "declaration of member 'D::d' with anonymous union type 'V' {aka '<unnamed union>'}" }
struct E { U e[2]; };			// { dg-error "declaration of member 'E::e' with array of anonymous union type 'U \\\[2\\\]' {aka 'A::<unnamed union> \\\[2\\\]'}" }
struct F { V f[2]; };			// { dg-error "declaration of member 'F::f' with array of anonymous union type 'V \\\[2\\\]' {aka '<unnamed union> \\\[2\\\]'}" }
struct G { static U g; };		// { dg-error "declaration of member 'G::g' with anonymous union type 'U' {aka 'A::<unnamed union>'}" }
struct H { static V h; };		// { dg-error "declaration of member 'H::h' with anonymous union type 'V' {aka '<unnamed union>'}" }
struct I { static U i[1][2]; };		// { dg-error "declaration of member 'I::i' with array of anonymous union type 'U \\\[1\\\]\\\[2\\\]' {aka 'A::<unnamed union> \\\[1\\\]\\\[2\\\]'}" }
struct J { static V j[2][1]; };		// { dg-error "declaration of member 'J::j' with array of anonymous union type 'V \\\[2\\\]\\\[1\\\]' {aka '<unnamed union> \\\[2\\\]\\\[1\\\]'}" }
using UA = U[2];
using VA = V[3];

void
foo (U x)				// { dg-error "declaration of parameter 'x' with anonymous union type 'U' {aka 'A::<unnamed union>'}" }
{
  U ({ .a = 1 });			// { dg-error "temporary object with anonymous union type 'U' {aka 'A::<unnamed union>'}" }
}

void
foo (U &x)
{
  throw x;				// { dg-error "'throw' operand has anonymous union type 'U' {aka 'A::<unnamed union>'}" }
}

void
bar ()
{
  U y;					// { dg-error "declaration of variable 'y' with anonymous union type 'U' {aka 'A::<unnamed union>'}" }
  U r[4];				// { dg-error "object with array of anonymous union type 'U' {aka 'A::<unnamed union>'}" }
  V s[2];				// { dg-error "object with array of anonymous union type 'V'" }
  union W { U u; } v;			// { dg-error "declaration of member 'bar\\\(\\\)::W::u' with anonymous union type 'U' {aka 'A::<unnamed union>'}" }
  try
    {
    }
  catch (U z)				// { dg-error "'catch' parameter with anonymous union type 'U' {aka 'A::<unnamed union>'}" }
    {
    }
  try
    {
    }
  catch (U)				// { dg-error "'catch' parameter with anonymous union type 'U' {aka 'A::<unnamed union>'}" }
    {
    }
}

void
baz (U)					// { dg-error "declaration of a parameter with anonymous union type 'U' {aka 'A::<unnamed union>'}" }
{
}

long
garply (const U &x)
{
  return x.b;
}

void
corge ()
{
  garply (U { .b = 42 });		// { dg-error "temporary object with anonymous union type 'U' {aka 'A::<unnamed union>'}" }
}

V *
xyzzy ()
{
  return new V { .e = 42 };		// { dg-error "temporary object with anonymous union type 'V' {aka '<unnamed union>'}" }
}

void
foo (V &x)
{
  V ({ .d = 1 });			// { dg-error "temporary object with anonymous union type 'V' {aka '<unnamed union>'}" }
  V v;					// { dg-error "declaration of variable 'v' with anonymous union type 'V' {aka '<unnamed union>'}" }
  throw x;				// { dg-error "'throw' operand has anonymous union type 'V' {aka '<unnamed union>'}" }
}

void qux (U);
void fred (V x);
void waldo (int, ...);

void
boo (U &x, V &y)
{
  qux (x);				// { dg-error "temporary object with anonymous union type 'U' {aka 'A::<unnamed union>'}" }
  fred (y);				// { dg-error "temporary object with anonymous union type 'V' {aka '<unnamed union>'}" }
  waldo (1, x);				// { dg-error "temporary object with anonymous union type 'U' {aka 'A::<unnamed union>'}" }
  waldo (2, y);				// { dg-error "temporary object with anonymous union type 'V' {aka '<unnamed union>'}" }
}
