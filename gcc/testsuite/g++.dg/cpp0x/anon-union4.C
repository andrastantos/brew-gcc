// { dg-do compile { target c++11 } }

struct A { A () = default; A (int); int a; };
struct B { union { int a; A b; }; };

B *
foo ()
{
  return new B ();
}
