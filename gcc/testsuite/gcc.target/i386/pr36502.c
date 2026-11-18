/* PR target/36502 */
/* { dg-do compile { target { *-*-darwin* && ilp32 } } } */
/* { dg-options "-O -fomit-frame-pointer -mdynamic-no-pic" } */
int a;
void f() {a++;}
/* { dg-final { scan-assembler-not "esp" } } */

