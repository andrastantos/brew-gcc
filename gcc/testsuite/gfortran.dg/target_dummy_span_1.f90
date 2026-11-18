! { dg-do run }
! { dg-additional-options "-fdump-tree-original" }
!
! PR126964
!
! The elements of a TARGET assumed-shape dummy can be spaced by more than
! the element length.  Where the element length is the element alignment,
! the spacing is folded into the strides on entry, so that the elements are
! addressed by the constant element length rather than by a span loaded
! from the descriptor.  Addressing by a runtime span leaves the data
! reference step symbolic, which stops the loops from being vectorized.

module m
  implicit none
  type :: t
    real(8) :: a, b
  end type
  real(8), pointer :: saved(:,:) => null()
contains
  subroutine axpy (self, n)
    real(8), dimension(:,:), target :: self
    integer, intent(in) :: n
    integer :: k
    do k = 1, n - 1
      self(:,n) = self(:,n) + self(:,k)
    end do
    saved => self
  end subroutine
end module

program p
  use m
  implicit none
  type(t), target :: x(4,3)
  integer :: i, j
  x%a = -1.0_8
  x%b = reshape ([(real (i, 8), i = 1, 12)], [4, 3])

  call axpy (x%b, 3)

  do j = 1, 2
    do i = 1, 4
      if (x(i,j)%b /= real (i + 4*(j-1), 8)) stop 1
    end do
  end do
  if (any (x(:,3)%b /= [15.0_8, 18.0_8, 21.0_8, 24.0_8])) stop 2
  if (any (x%a /= -1.0_8)) stop 3

  saved = 0.0_8
  if (any (x%b /= 0.0_8)) stop 4
  if (any (x%a /= -1.0_8)) stop 5
end program

! Where the element length is the element alignment, the spacing is read from
! the descriptor once, on entry, and scales both strides and the offset; no
! span variable is created and the elements are addressed by the element
! length.
! { dg-final { scan-tree-dump-times "= self->span;" 1 "original" } }
! { dg-final { scan-tree-dump-times "\\? stride\.\[0-9\]+ \\* \[^;\]+ : stride\.\[0-9\]+;" 2 "original" { target natural_alignment_64 } } }
! { dg-final { scan-tree-dump-times "\\? offset\.\[0-9\]+ \\* \[^;\]+ : offset\.\[0-9\]+;" 1 "original" { target natural_alignment_64 } } }
! { dg-final { scan-tree-dump-not "span\.\[0-9\]+" "original" { target natural_alignment_64 } } }
! { dg-final { scan-tree-dump-not "\\* self->span" "original" } }
