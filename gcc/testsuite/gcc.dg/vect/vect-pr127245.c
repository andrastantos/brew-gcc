/* { dg-do compile } */
/* { dg-additional-options "-march=armv8.3-a+sve" { target aarch64-*-* } } */

struct vbi_char {
  unsigned bold : 1;
  unsigned : 16;
} ;
struct vbi_char *v;
void f(int l, bool bold)
{
  for (int i = 0; i < l; i++)
    v[i].bold = bold;
}
