! { dg-do compile }
! { dg-additional-options "-Wundefined-vars" }
! In the code below, a can be allocated by calling bar and
! c from the outside.
module x
  implicit none
  integer, allocatable, private, dimension(:) :: a, b
  integer, allocatable, public, dimension(:) :: c
contains
  subroutine foo
    print *,a
    print *,b ! { dg-warning "Unallocated variable" }
    print *,c
  end subroutine foo
  subroutine bar
    a = [1,2,3]
  end subroutine bar
end module x
