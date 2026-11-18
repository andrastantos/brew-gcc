// PR c++/127132: typeid inside a generic lambda's capture-default failed
// to capture, because capture incorrectly depended on whether the
// typeid operand was evaluated ([expr.typeid]/4-5), rather than on
// [expr.prim.lambda.capture]/7 (P0588R1 / CWG1468), which says capture
// must ignore the effect of any enclosing typeid.  Regression against
// the PR c++/125886 fix (r17-2200-g621661f7d2e9).
// { dg-do compile { target c++14 } }
// c++14 target: several cases below use a generic lambda's auto
// parameter, valid from C++14 on.

#include <typeinfo>

void foo(const std::type_info &type);

// [expr.typeid]/4: *c is a polymorphic glvalue, so typeid's operand is
// evaluated, and the generic lambda's by-copy capture-default must
// capture c for that use.  On trunk this capture is currently missing,
// leaving a decl reference with nothing behind it once the lambda's
// call operator is expanded - crashing at RTL expand time instead of
// compiling cleanly.

class C {
  virtual ~C();
};

void
bar (C *c)
{
  [=](auto a) { foo(typeid(*c)); }(1);
}

// [expr.prim.lambda.capture]/7 (P0588R1 / CWG1468): capture must ignore
// the effect of enclosing typeid expressions.  n is not a polymorphic
// glvalue, so the operand is unevaluated ([expr.typeid]/5), but n is
// still captured.
//
// If this starts to fail that's OK, the compiler is allowed to optimize
// away the capture, but we need to control it with -fabi-version.

void
capture_regardless_of_evaluated (int n)
{
  auto lam = [=] { (void) typeid (n); };
  static_assert (sizeof (lam) == sizeof (int), "");
}

// Generic-lambda variant.

void
capture_regardless_of_evaluated_generic (int n)
{
  auto lam = [=](auto a) { (void) typeid (n); };
  lam (0);
  static_assert (sizeof (lam) == sizeof (int), "");
}

// No capture-default and a genuinely unevaluated operand: must not
// diagnose "not captured".

void
no_capture_default_non_poly (int n)
{
  [] { (void) typeid (n); };
}

// [expr.prim.lambda.capture]/7: capture must not depend on whether the
// typeid operand is evaluated.  F is final, so [expr.typeid]/4 leaves
// typeid (f) unevaluated, but f is still captured.  Generic-lambda
// variant of pr125886.C's final_glvalue.

struct F final { virtual ~F (); };

void
final_glvalue_generic (F &f)
{
  auto lam = [=](auto a) { foo (typeid (f)); };
  lam (1);
  static_assert (sizeof (lam) == sizeof (F), "");
}

// D::i names no local entity, so [expr.prim.lambda.capture]/7 does not
// apply and a DR613 non-static member id-expression stays unevaluated
// (no capture).  Generic-lambda variant of pr125886.C's nsm.

struct D { int i; };

void
nsm_generic ()
{
  auto lam = [=](auto a) { foo (typeid (D::i)); };
  lam (1);
  static_assert (sizeof (lam) == 1, "");
}

// sizeof (typeid (b)) is a genuinely unevaluated context wrapping
// typeid, so the cutoff stays unraised and b is not captured.

struct B { virtual ~B (); };

void
nested_unevaluated_lambda_size (B &b)
{
  auto lam = [=] { (void) sizeof (typeid (b)); };
  static_assert (sizeof (lam) == 1, "");
}

// Nested typeid looks through every enclosing typeid
// ([expr.prim.lambda.capture]/7, "any enclosing typeid expressions").

void
nested_typeid_capture (int x)
{
  auto lam = [=] { (void) typeid (typeid (x)); };
  static_assert (sizeof (lam) == sizeof (int), "");
}

// Extremely nested typeid: look-through on each probe
// (operand == cutoff -> cutoff += 1).

void
nested_typeid_capture_extreme (int x)
{
  auto lam = [=] {
    (void) typeid (typeid (typeid (typeid (typeid (typeid (typeid (typeid (typeid (typeid (typeid (x)))))))))));
  };
  static_assert (sizeof (lam) == sizeof (int), "");
}

// sizeof does not raise the cutoff, so all 10 typeid probes stay above
// it and x is not captured.

void
nested_typeid_capture_extreme_v2 (int x)
{
  auto lam = [=] { (void) sizeof (typeid (typeid (typeid (typeid (typeid (typeid (typeid (typeid (typeid (typeid (x))))))))))); };
  static_assert (sizeof (lam) == 1, "");
}

// this-capture variant of capture_regardless_of_evaluated:
// [expr.prim.lambda.capture]/7 applies to *this like any other local
// entity.  [&] avoids the (unrelated) C++20 deprecation warning for
// implicit this-capture via [=].

struct E {
  int i;
  void this_capture_regardless_of_evaluated ();
};

void
E::this_capture_regardless_of_evaluated ()
{
  auto lam = [&] { (void) typeid (this->i); };
  static_assert (sizeof (lam) == sizeof (E *), "");
}

// Generic-lambda variant.

struct E2 {
  int i;
  void this_capture_regardless_of_evaluated_generic ();
};

void
E2::this_capture_regardless_of_evaluated_generic ()
{
  auto lam = [&](auto a) { (void) typeid (this->i); };
  lam (1);
  static_assert (sizeof (lam) == sizeof (E2 *), "");
}
