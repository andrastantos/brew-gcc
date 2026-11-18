// { dg-do run { target c++11 } }
// { dg-require-cstdint "" }

#include <random>
#include <testsuite_hooks.h>

class Constant {
public:
  using result_type = unsigned;
  static constexpr result_type min() { return 0u; }
  static constexpr result_type max() { return ~0u; }

  // always return 0
  result_type operator()() {
    ++calls;
    VERIFY( calls < 1000 );
    return 0;
  }

  unsigned calls = 0;
};

int main()
{
  Constant gen;
  std::uniform_int_distribution<int> dist(0, 42);
  dist(gen);
}
