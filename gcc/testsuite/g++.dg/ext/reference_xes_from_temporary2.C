// PR c++/112908
// { dg-do compile { target c++11 } }

struct T;

#define SA(X) static_assert ((X), #X);

SA (!__reference_constructs_from_temporary (T&&, T));	    // { dg-error "invalid use of incomplete type" }
SA (!__reference_constructs_from_temporary (const T&, T));  // { dg-error "invalid use of incomplete type" }
SA (!__reference_constructs_from_temporary (T&, T));	    // { dg-error "invalid use of incomplete type" }
SA (!__reference_converts_from_temporary (T&&, T));	    // { dg-error "invalid use of incomplete type" }
SA (!__reference_converts_from_temporary (const T&, T));    // { dg-error "invalid use of incomplete type" }
SA (!__reference_converts_from_temporary (T&, T));	    // { dg-error "invalid use of incomplete type" }

struct Self {
  static constexpr bool b = __reference_constructs_from_temporary (Self&, Self); // { dg-error "invalid use of incomplete type" }
};
