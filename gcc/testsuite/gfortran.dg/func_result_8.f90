! { dg-do run }
!
! PR fortran/78718
program p
   integer :: n, m
   n = 1
   if (f() /= 1) stop 1
contains
   function f() result(m)
      call s(n, m)
   end function
end program

subroutine s(a, b)
   integer :: a, b
   b = a
end subroutine
