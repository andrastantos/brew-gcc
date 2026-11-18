/* Check that NEON vector shifts support immediate values == size.  /*

/* { dg-do compile } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-options "-save-temps" } */
/* { dg-add-options arm_neon } */

#include <arm_neon.h>

uint16x8_t test_vshll_n_u8 (uint8x8_t a)
{
    return vshll_n_u8(a, 8);
}
/* { dg-final { scan-assembler {vshll\.i8\tq[0-9]+, d[0-9]+, #8} } } */

uint32x4_t test_vshll_n_u16 (uint16x4_t a)
{
    return vshll_n_u16(a, 16);
}
/* { dg-final { scan-assembler {vshll\.i16\tq[0-9]+, d[0-9]+, #16} } } */

uint64x2_t test_vshll_n_u32 (uint32x2_t a)
{
    return vshll_n_u32(a, 32);
}
/* { dg-final { scan-assembler {vshll\.i32\tq[0-9]+, d[0-9]+, #32} } } */
