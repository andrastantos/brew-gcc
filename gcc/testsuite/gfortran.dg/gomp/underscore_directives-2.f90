! { dg-do compile }

! OpenMP 6 adds underscore variants to the directive names
! Check declare_mapper here

module m2
  type t
    integer :: ptr
  end type t
  !$omp declare mapper(mymapper : t :: v) map(v%ptr)
contains
  subroutine mapit(y)
    type(t) :: y
    ! Must be last until implemented
    !$omp target_enter_data map(mapper(mymapper), to: y) ! { dg-error "Sorry, declared mapper 'mymapper', used for 'y' at .1., is not yet supported" }
  end
end



