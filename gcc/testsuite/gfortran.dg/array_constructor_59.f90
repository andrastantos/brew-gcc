! { dg-do run }
! PR fortran/53296
! Test case based on the test case from Steve Kargl
program pr53296
  implicit none
  character(len=128) :: str(2)
  integer :: ncalls = 0

  str = [ucase("abcde"), ucase("ghij")]
  if (ncalls /= 2) stop 1
  if (trim (str(1)) /= "abcde") stop 2
  if (trim (str(2)) /= "ghij") stop 3

contains

  function ucase(s)
    character(*), intent(in) :: s
    character(len(s)) :: ucase
    ncalls = ncalls + 1
    ucase = s
  end function ucase

end program pr53296
