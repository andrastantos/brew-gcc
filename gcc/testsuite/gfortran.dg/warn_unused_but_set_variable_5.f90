! { dg-do compile }
! { dg-additional-options "-Wunused-but-set-variable" }

module x
  implicit none
contains
  function asdf() result(res)
    character(len=1) :: res
    integer :: i
    read (*,*) i
    select case(i)
    case(1)
       res = 'a'
    case(2)
       res = 's'
    case(3)
       res = 'd'
    case(4)
       res = 'f'
    case default
       res = ' '
    end select
  end function asdf
end module x
  
