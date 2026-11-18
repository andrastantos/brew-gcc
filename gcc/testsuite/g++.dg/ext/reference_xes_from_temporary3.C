// PR c++/112908
// { dg-do compile { target c++17 } }

struct U {
  U();
  U(U&&) = delete;
};

struct T {
  T(U);
};

struct T2 {
  explicit T2(U);
};
static_assert(__reference_converts_from_temporary(const T&, U));
static_assert(__reference_constructs_from_temporary(const T&, U));
static_assert(!__reference_converts_from_temporary(const T2&, U));
static_assert(!__reference_constructs_from_temporary(const T2&, U));

struct NonMovable {
  NonMovable() = default;
  NonMovable(NonMovable&&) = delete;
};

static_assert(__reference_converts_from_temporary(int&&, int));
static_assert(__reference_converts_from_temporary(NonMovable&&, NonMovable));
static_assert(__reference_constructs_from_temporary(int&&, int));
static_assert(__reference_constructs_from_temporary(NonMovable&&, NonMovable));

struct Base {
  Base();
  Base(Base&&) = delete;
};
struct Derived : Base {
  Derived();
  Derived(Derived&&) = delete;
};

static_assert(__reference_converts_from_temporary(const Base&, Derived));
static_assert(__reference_constructs_from_temporary(const Base&, Derived));

static_assert(!__reference_converts_from_temporary (int&, void));
static_assert(!__reference_converts_from_temporary (int(&)[], int[]));
static_assert(!__reference_constructs_from_temporary (int&, void));
static_assert(!__reference_constructs_from_temporary (int(&)[], int[]));

struct A {
  A();
  A(A&&) = delete;
  ~A();
};
struct B { B(A); };
static_assert (__reference_converts_from_temporary (const B&, A));
static_assert (__reference_constructs_from_temporary (const B&, A));
static_assert (__reference_converts_from_temporary (const B&, const A));
static_assert (__reference_constructs_from_temporary (const B&, const A));

struct C {
  operator int() const;
};
static_assert(!__reference_converts_from_temporary(int&, C));
static_assert(__reference_converts_from_temporary(const int&, C));
static_assert(!__reference_constructs_from_temporary(int&, C));
static_assert(__reference_constructs_from_temporary(const int&, C));

struct D {
  D(int);
  D(D&&) = delete;
};
struct E { E(D); };
static_assert(__reference_converts_from_temporary(const E&, D));
static_assert(__reference_constructs_from_temporary(const E&, D));


struct F {
  F();
  F(F&&) = delete;
  virtual ~F();
};

struct G {
  G(F);
};
static_assert(__reference_converts_from_temporary(const G&, F));
static_assert(__reference_constructs_from_temporary(const G&, F));
