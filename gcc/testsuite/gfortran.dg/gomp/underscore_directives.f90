! { dg-do compile }
! { dg-additional-options "-fdump-tree-original" }

! OpenMP 6 adds underscore variants to the directive names
! Check all but declare_mapper here.

subroutine test
  !$omp declare_target enter(test)
end

subroutine data
  integer :: x
  !$omp target_enter_data map(x)

  !$omp target_data map(x)
    call bar(x)
  !$omp end target_data

  !$omp target_update from(x)

  !$omp target_exit_data map(x)
end subroutine data

module m
contains
subroutine varfn()
end
subroutine basefn()
  !$omp declare_variant(varfn) match(construct={parallel})
end
end

module m3
!$omp declare_reduction (foo:integer:omp_out = omp_out + omp_in)
contains
    integer function foo (x, y)
      integer, value :: x, y
      !$omp declare_simd (foo) linear (y : 2)
    end function foo
subroutine bar
  integer :: b, i, a(64)
  a = 1
  b = 0
  !$omp parallel
    !$omp cancellation_point parallel
    !$omp critical (bar)
      b = b + 1
    !$omp end critical (bar)
    !$omp cancel parallel
  !$omp end parallel

  !$omp simd reduction (foo: b)
  do i = 1, 64
    b = b + a(i)
  end do
end
end

! { dg-final { scan-tree-dump "#pragma omp target enter data map\\(to:x\\)" "original" } }
! { dg-final { scan-tree-dump "#pragma omp target data map\\(tofrom:x\\)" "original" } }
! { dg-final { scan-tree-dump "#pragma omp target update from\\(x\\)" "original" } }
! { dg-final { scan-tree-dump "#pragma omp target exit data map\\(from:x\\)" "original" } }

! { dg-final { scan-tree-dump "#pragma omp simd linear\\(i:1\\) reduction\\(b\\)" "original" } }

! { dg-final { scan-tree-dump "__attribute__\\(\\(omp declare target \\(device_type\\(any\\)\\)\\)\\)" "original" } }
! { dg-final { scan-tree-dump "__attribute__\\(\\(omp declare simd \\(linear\\(1:2\\)\\)\\)\\)" "original" } }

! { dg-final { scan-tree-dump "__attribute__\\(\\(omp declare variant base \\(varfn match construct = {parallel}\\)\\)\\)" "original" } }
! { dg-final { scan-tree-dump "__attribute__\\(\\(omp declare variant variant" "original" } }

! { dg-final { scan-tree-dump "__builtin_GOMP_cancellation_point \\(1\\);" "original" } }

