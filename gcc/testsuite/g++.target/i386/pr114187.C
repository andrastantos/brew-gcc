/* { dg-do compile } */
/* { dg-options "-O2" } */

struct P2d {
    double x, y;
};

double sumxy_p(P2d p) {
    return p.x + p.y;
}

/* No move between GPR and XMM.  */
/* { dg-final { scan-assembler-not "movq\[ \t%\]*r" } } */
/* { dg-final { scan-assembler-not "xchg" } } */
