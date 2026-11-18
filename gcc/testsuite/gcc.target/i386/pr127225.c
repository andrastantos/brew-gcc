/* PR target/127225 */
/* { dg-do compile } */
/* { dg-options "-O2 -frounding-math -mno-sse2 -mno-mmx -mno-3dnow" } */

typedef __attribute__((__vector_size__(4 * sizeof(float)))) float V;

V g;

void
foo ()
{
  (V){68380689, 263227704};
}

void
bar (float a, float b)
{
  g = (V){a, b};
}
