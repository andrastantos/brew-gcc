/* { dg-do compile { target gnu_retain } } */
/* { dg-skip-if "non-ELF target" { *-*-darwin* powerpc*-*-aix* } } */
/* { dg-options "-Wall -O2" } */

int __attribute__((used,retain,section(".data.foo"))) foo2 = 2;
int __attribute__((section(".data.foo"))) foo1 = 1;
/* { dg-warning "'.*' without 'retain' attribute and '.*' with 'retain' attribute are placed in a section with the same name" "" { target gnu_retain } .-1 } */

/* { dg-final { scan-assembler ".data.foo,\"aw\"" { target gnu_retain } } } */
/* { dg-final { scan-assembler ".data.foo,\"awR\"" { target gnu_retain } } } */
