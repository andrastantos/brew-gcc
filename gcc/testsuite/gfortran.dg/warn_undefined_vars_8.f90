! { dg-do compile }
! { dg-additional-options "-Wundefined-vars" }
! A variable referenced in a charlen was not tracked for warnings.

program memain
  implicit none
  character(len=:), allocatable :: c, d
  integer :: n, m
  n = 42
  allocate (character(len=n) :: c)
  read (*,*) c
  print *,c
  ! m = 21
  allocate (character(len=m) :: d) ! { dg-warning "Undefined variable" }
  read (*,*) d
  print *,d
end program memain
