/* { dg-do link } */
/* { dg-options "-O2" } */

volatile double gd;

static const double values[4] = {0.5, 1.5, 2.5, 3.5};

void link_error (void);

void foo (int index)
{
  if (index < 0 || index > 3)
    return;
  const double v = values[index];
  if (v < 0.5 || v > 3.5)
    link_error ();
  if (v != v)
    link_error ();
  gd = v;
}

__attribute__((noipa)) int get_index (void) { return 2; }

int main (void)
{
  foo (get_index ());
  return 0;
}
