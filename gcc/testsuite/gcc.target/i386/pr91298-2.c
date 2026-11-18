/* PR target/91298 */
/* { dg-do assemble { target fpic } } */
/* { dg-options "-O2 -g -fdollars-in-identifiers -fpic" } */
/* { dg-xfail-if "No support for $ in identifiers" { solaris_as } } */

#include "pr91298-1.c"
