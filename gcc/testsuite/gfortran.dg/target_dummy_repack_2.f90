! { dg-do run }
! { dg-options "-fdump-tree-original" }
! PR126964
! A TARGET assumed-shape dummy of a type whose element length need not divide
! the spacing is addressed through the span of its descriptor.  When it is
! passed on to a dummy that has a descriptor of its own, that dummy addresses
! the elements by the strides it holds, so the only condition for passing it
! directly is that the span be the element length.  Testing full contiguity
! instead made a non-contiguous actual argument be copied on every call.
!
module m
  implicit none
  type :: t
     complex :: z
     real :: r
  end type
contains

  ! Assumed shape, no TARGET: takes any stride, needs no repacking.
  complex function elem (a, i, j) result (s)
    complex, intent(in) :: a(:,:)
    integer, intent(in) :: i, j
    s = a(i,j)
  end function

  complex function sum_target (self) result (s)
    complex, target, intent(in) :: self(:,:)
    integer :: i, j
    s = 0.0
    do j = 1, size (self,2)
       do i = 1, size (self,1)
          s = s + elem (self, i, j)
       end do
    end do
  end function

  complex function sum_plain (self) result (s)
    complex, intent(in) :: self(:,:)
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
  type(t), allocatable, target :: v(:,:)
  complex :: expect
  integer :: i, j

  allocate (v(2*n,n))
  do j = 1, n
     do i = 1, 2*n
        v(i,j)%z = cmplx (i + 100*j, i - j)
        v(i,j)%r = 0.0
     end do
  end do

  ! Elements that are subobjects of larger ones.
  expect = sum_plain (v%z)
  if (abs (sum_target (v%z) - expect) > 1.0e-4) stop 1

  ! Contiguous actual argument.
  block
    complex, allocatable, target :: c(:,:)
    c = v%z
    expect = sum_plain (c)
    if (abs (sum_target (c) - expect) > 1.0e-4) stop 2
    expect = sum_plain (c(1:2*n:2,:))
    if (abs (sum_target (c(1:2*n:2,:)) - expect) > 1.0e-4) stop 3
  end block

end program

! The condition for passing the dummy on must be the span alone; testing the
! strides as well would repack a non-contiguous actual argument needlessly.
! { dg-final { scan-tree-dump "contiguous\\.\[0-9\]+ = \[^;\]*span == \[^;&\]*elem_len;" "original" } }
