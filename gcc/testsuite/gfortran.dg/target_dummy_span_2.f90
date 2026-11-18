! { dg-do run }
!
! PR126964
!
! The spacing of the elements of a TARGET assumed-shape dummy is folded into
! its strides only where the element length is the least alignment the element
! can be given as a component, so that the spacing is a multiple of it.  Using
! the alignment of the type instead scaled the strides by a truncated ratio
! wherever a target aligns a component less strictly than its type: a real(8)
! component is aligned to 4 by the i386 ABI, so the spacing here is 12.

module m
  implicit none
  type :: t
     integer :: i
     real(8) :: d
  end type
contains
  subroutine chk (a, n)
    real(8), target :: a(:)
    integer, intent(in) :: n
    integer :: k
    do k = 1, n
       if (a(k) /= real (k, 8)) stop 1
    end do
    a(2) = -a(2)
  end subroutine
end module

program p
  use m
  implicit none
  integer, parameter :: n = 5
  type(t), target :: v(n)
  integer :: k

  v%i = 0
  v%d = [(real (k, 8), k = 1, n)]

  call chk (v%d, n)

  if (v(2)%d /= -2.0_8) stop 2
  if (any (v%i /= 0)) stop 3
  if (any (v((/1,3,4,5/))%d /= [1.0_8, 3.0_8, 4.0_8, 5.0_8])) stop 4
end program
