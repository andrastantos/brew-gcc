// P0847R7
// { dg-do compile { target c++23 } }

namespace std {
  template<typename T> struct tuple_size;
  template<int, typename> struct tuple_element;
}

// PR c++/122258 - friend with explicit object param must be rejected

struct type {
  template<unsigned I, class Self>
  friend auto get(this Self &&self) { return 0; } // { dg-error "a non-member function cannot have an explicit object parameter" }
};

struct S {
  friend void f(this S &); // { dg-error "a non-member function cannot have an explicit object parameter" }
  friend void g(this S &&); // { dg-error "a non-member function cannot have an explicit object parameter" }
};

struct U {
  template<class Self>
  friend void h(this Self &&); // { dg-error "a non-member function cannot have an explicit object parameter" }
  template<class Self>
  friend void k(this Self &&) {} // { dg-error "a non-member function cannot have an explicit object parameter" }
};

struct V {
  friend void m(this V &) {} // { dg-error "a non-member function cannot have an explicit object parameter" }
};

struct Mem {
  void mem(this Mem&);
};

struct A {
  friend void Mem::mem(this Mem&);
};

struct B {
  friend void Mem::mem(this Mem&);
};

struct C {
  friend void Mem::mem(this Mem&);
};

namespace std {
template<> struct tuple_size<type> { static const int value = 1; };
template<int I> struct tuple_element<I, type> { using type = int; };
}

void test_sb()
{
  auto [a] = type();
}

void test_friend_member(void)
{
  struct Mem2 {
    void mem(this Mem2&);
  };
  struct A {
    friend void Mem2::mem(this Mem2&);
  };
}
