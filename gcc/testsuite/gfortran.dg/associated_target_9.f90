! { dg-do run }
! PR126964
! Elements that are subobjects of larger ones are described either by the
! spacing of the larger objects as the span of the descriptor, or by that
! spacing folded into its strides with the element length as the span.
! ASSOCIATED compared the two encodings field by field, so a pointer to a
! TARGET assumed-shape dummy was not associated with the actual argument.
!
module m
  implicit none
  type :: tr
     real :: a, b, c
  end type
  type :: tc
     complex :: z
     real :: r
  end type
  real, pointer :: pr(:) => null()
  real, pointer :: ps(:) => null()
  complex, pointer :: pc(:) => null()
contains
  subroutine taker (x)
    real, target, intent(inout) :: x(:)
    pr => x
    ps => x(1:size (x):2)
  end subroutine
  subroutine takec (x)
    complex, target, intent(inout) :: x(:)
    pc => x
  end subroutine
end module

program p
  use m
  implicit none
  type(tr), target :: v(4)
  type(tc), target :: w(4)
  real, target :: flat(4)
  integer :: i

  v%a = 0.0
  v%b = [(real (i), i = 1, 4)]
  v%c = 0.0
  w%z = [(cmplx (i, 0), i = 1, 4)]
  w%r = 0.0
  flat = [(real (i), i = 1, 4)]

  ! Elements that are subobjects of larger ones.
  call taker (v%b)
  if (.not. associated (pr, v%b)) stop 1
  if (.not. associated (ps, v(1:3:2)%b)) stop 2

  ! Contiguous actual argument.
  call taker (flat)
  if (.not. associated (pr, flat)) stop 3
  if (.not. associated (ps, flat(1:3:2))) stop 4

  ! A type whose element length need not divide the spacing.
  call takec (w%z)
  if (.not. associated (pc, w%z)) stop 5

  ! Storage sequences that differ are still not associated.
  call taker (v%b)
  if (associated (pr, v(1:2)%b)) stop 6
  if (associated (pr, v(2:4)%b)) stop 7
  if (associated (pr, v%a)) stop 8
  if (associated (ps, v%b)) stop 9
  if (associated (ps, v(2:4:2)%b)) stop 10
  if (associated (pr, flat)) stop 11
end program
