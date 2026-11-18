! { dg-do compile }
! { dg-options "-O0 -fdump-tree-original" }
!
! PR fortran/56907
!
subroutine sub(xxx, yyy)
  use iso_c_binding
  implicit none
  integer, target, contiguous :: xxx(:)
  integer, target             :: yyy(:)
  type(c_ptr) :: ptr1, ptr2, ptr3, ptr4
  ptr1 = c_loc (xxx)
  ptr2 = c_loc (xxx(5:))
  ptr3 = c_loc (yyy)
  ptr4 = c_loc (yyy(5:))
end
! { dg-final { scan-tree-dump-not " _gfortran_internal_pack" "original" } }
! { dg-final { scan-tree-dump-times "parm.\[0-9\]+.data = \\(void .\\) &\\(.xxx.\[0-9\]+\\)\\\[0\\\];" 1 "original" } }
! { dg-final { scan-tree-dump-times "parm.\[0-9\]+.data = \\(void .\\) &\\(.xxx.\[0-9\]+\\)\\\[D.\[0-9\]+ \\* 4\\\];" 1 "original" } }
! The elements of a TARGET assumed-shape dummy can be spaced by more than the
! element length.  The spacing is folded into the strides on entry where the
! element length is the element alignment, so that the elements are addressed
! by the constant element length rather than by a span loaded from the
! descriptor.  Elsewhere they are addressed by the span.
! { dg-final { scan-tree-dump-not "span.\[0-9\]+ = yyy->span;" "original" { target natural_alignment_32 } } }
! { dg-final { scan-tree-dump-times "parm.\[0-9\]+.data = \\(void .\\) yyy.\[0-9\]+;" 1 "original" { target natural_alignment_32 } } }
! { dg-final { scan-tree-dump-times "parm.\[0-9\]+.data = \\(void .\\) yyy.\[0-9\]+ \\+ \\(sizetype\\) \\(D.\[0-9\]+ \\* 16\\);" 1 "original" { target natural_alignment_32 } } }

! { dg-final { scan-tree-dump-times "D.\[0-9\]+ = parm.\[0-9\]+.data;\[^;]+ptr\[1-4\] = D.\[0-9\]+;" 4 "original" } }
