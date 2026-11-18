! { dg-do compile }
! { dg-additional-options "-fdump-tree-original" }
!
! PR fortran/126950
! PR126964
!
! A TARGET assumed-shape dummy is addressed through the span of its
! descriptor.  The descriptor is not mapped to the device, so the span has
! to be read into a local variable on entry, rather than the target region
! dereferencing the descriptor.

module m
  use iso_c_binding
contains
  subroutine tgt (t)
    real(c_double), target :: t(:)
    !$omp target has_device_addr(t)
    call inner (t(1))
    !$omp end target
  end subroutine tgt

  subroutine inner (a)
    real(c_double) :: a
  end subroutine inner
end module m

! The span is read from the descriptor once, on entry.  Where the element
! length is the element alignment it scales the strides, and the element
! reference uses the local strides and the element length.
! { dg-final { scan-tree-dump-times "= t->span;" 1 "original" } }
! { dg-final { scan-tree-dump "stride\.\[0-9\]+ = \[^;\]* != 8 \\? stride\.\[0-9\]+ \\* \[^;\]* : stride\.\[0-9\]+;" "original" { target natural_alignment_64 } } }
! { dg-final { scan-tree-dump "t\.\[0-9\]+ \\+ \\(sizetype\\) \\(\\(offset\.\[0-9\]+ \\+ \[^)\]*stride\.\[0-9\]+\[^)\]*\\) \\* 8\\)" "original" { target natural_alignment_64 } } }
! { dg-final { scan-tree-dump-not "\\* t->span" "original" } }
