/* { dg-do compile } */
/* { dg-options "-mtune=no-such-processor" } */
/* { dg-error "unrecognized argument in option '-mtune=no-such-processor'" "" { target *-*-* } 0 } */
/* { dg-message "valid arguments to '-mtune=' are: espresso generic" "" { target *-*-* } 0 } */

int x;
