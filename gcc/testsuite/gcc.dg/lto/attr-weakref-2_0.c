/* { dg-lto-do link } */
/* { dg-lto-options {{-O2 -flto}} } */

#if __APPLE__
# undef __weak
#endif
#define __weak __attribute__((__weak__))
void __weak other() {}
void __weak fn() {}

int main() {
  fn();
  other();
}
