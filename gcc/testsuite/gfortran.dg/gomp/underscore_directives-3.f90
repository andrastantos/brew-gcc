! { dg-do compile }

! OpenMP 6 adds underscore variants to the directive names
! Check 'if' clause and constructs in contains/absent clauses of assume(s)

subroutine sub(x)
  integer :: x
 
  !$omp target_enter_data map(x) if(target_enter_data : x > 0)
  !$omp target_data map(x)       if(target_data : x < 0)
    call sub2(x)
  !$omp end target_data
  !$omp target_exit_data map(x)  if(target_exit_data : x > 0)

  !$omp target_update from(x)    if (target_update : x == 0)
end

module m2
!$omp assumes absent(cancellation_point, target_data, target_enter_data, target_exit_data, target_update)

!$omp assumes contains(declare_mapper)     ! { dg-error "Invalid 'DECLARE MAPPER' directive at .1. in CONTAINS clause: declarative, informational, and meta directives not permitted" }
!$omp assumes contains(declare_reduction)  ! { dg-error "Invalid 'DECLARE REDUCTION' directive at .1. in CONTAINS clause: declarative, informational, and meta directives not permitted" }
!$omp assumes contains(declare_simd)       ! { dg-error "Invalid 'DECLARE SIMD' directive at .1. in CONTAINS clause: declarative, informational, and meta directives not permitted" }
!$omp assumes contains(declare_target)     ! { dg-error "Invalid 'DECLARE TARGET' directive at .1. in CONTAINS clause: declarative, informational, and meta directives not permitted" }
!$omp assumes contains(declare_variant)    ! { dg-error "Invalid 'DECLARE VARIANT' directive at .1. in CONTAINS clause: declarative, informational, and meta directives not permitted" }
end
