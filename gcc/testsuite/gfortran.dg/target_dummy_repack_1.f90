! { dg-do run }
! { dg-options "-fdump-tree-original" }
! PR126964
! The spacing of the elements of a numeric TARGET assumed-shape dummy whose
! element length is its alignment is folded into the strides of its
! descriptor on entry, so the dummy is not
! addressed through its span and its elements are not subobjects of larger
! ones.  Passing it on to a dummy that has a descriptor of its own therefore
! needs neither a copy nor a test.  Testing full contiguity instead made a
! non-contiguous actual argument be copied on every call.
!
module m
  implicit none
contains

  ! Assumed shape, no TARGET: takes any stride, needs no repacking.
  real function elem (a, i, j) result (s)
    real, intent(in) :: a(:,:)
    integer, intent(in) :: i, j
    s = a(i,j)
  end function

  real function sum_target (self) result (s)
    real, target, intent(in) :: self(:,:)
    integer :: i, j
    s = 0.0
    do j = 1, size (self,2)
       do i = 1, size (self,1)
          s = s + elem (self, i, j)
       end do
    end do
  end function

  real function sum_plain (self) result (s)
    real, intent(in) :: self(:,:)
    integer :: i, j
    s = 0.0
    do j = 1, size (self,2)
       do i = 1, size (self,1)
          s = s + elem (self, i, j)
       end do
    end do
  end function

end module

program p
  use m
  implicit none
  integer, parameter :: n = 6
  real, allocatable, target :: a(:,:)
  real :: expect
  integer :: i, j

  allocate (a(2*n,n))
  do j = 1, n
     do i = 1, 2*n
        a(i,j) = real (i + 100*j)
     end do
  end do

  ! Contiguous actual argument.
  expect = sum_plain (a)
  if (abs (sum_target (a) - expect) > 1.0e-4) stop 1

  ! Non-contiguous actual argument: every other row.
  expect = sum_plain (a(1:2*n:2,:))
  if (abs (sum_target (a(1:2*n:2,:)) - expect) > 1.0e-4) stop 2

  ! A section of the second dimension too.
  expect = sum_plain (a(1:2*n:2,2:n:2))
  if (abs (sum_target (a(1:2*n:2,2:n:2)) - expect) > 1.0e-4) stop 3

end program

! The dummy is passed on directly: no copy and no test of its span.
! { dg-final { scan-tree-dump-not "contiguous\\.\[0-9\]+" "original" { target natural_alignment_32 } } }
! { dg-final { scan-tree-dump-not "atmp\\.\[0-9\]+" "original" { target natural_alignment_32 } } }
