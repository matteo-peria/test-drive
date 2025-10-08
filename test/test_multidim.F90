module test_multidim
  use, intrinsic :: ieee_arithmetic, only : ieee_value, ieee_quiet_nan
  use testdrive, only : new_unittest, unittest_type, error_type, check_equal
  implicit none
  private
  public :: collect_check_equal

  ! Your kind definitions
  integer, parameter :: sp = selected_real_kind(6)
  integer, parameter :: dp = selected_real_kind(15)
  integer, parameter :: i1 = selected_int_kind(2)
  integer, parameter :: i2 = selected_int_kind(4)
  integer, parameter :: i4 = selected_int_kind(9)
  integer, parameter :: i8 = selected_int_kind(18)

contains

subroutine collect_check_equal(testsuite)
  type(unittest_type), allocatable, intent(out) :: testsuite(:)
  testsuite = [ &
    new_unittest("int_1d_equal_pass",        int_1d_equal_pass), &
    new_unittest("int_2d_equal_pass",        int_2d_equal_pass), &
    new_unittest("int_2d_mismatch_fail",     int_2d_mismatch_fail, should_fail=.true.), &
    new_unittest("real_sp_1d_tol_pass",      real_sp_1d_tol_pass), &
    new_unittest("real_dp_1d_rel_tol_pass",  real_dp_1d_rel_tol_pass), &
    new_unittest("real_dp_1d_nan_equal_pass", real_dp_1d_nan_equal_pass), &
    new_unittest("real_sp_subset_rows_cols", real_sp_subset_rows_cols), &
    new_unittest("real_dp_3d_equal_pass",    real_dp_3d_equal_pass), &
    new_unittest("shape_mismatch_fail",      shape_mismatch_fail, should_fail=.true.) &
  ]
end subroutine collect_check_equal

! ---- tests ----

subroutine int_1d_equal_pass(error)
  type(error_type), allocatable, intent(out) :: error
  integer(i4) :: a(3) = [1_i4, 2_i4, 3_i4]
  integer(i4) :: b(3) = [1_i4, 2_i4, 3_i4]
  call check_equal(error, a, b, message="int 1d equal pass")
end subroutine int_1d_equal_pass

subroutine int_2d_equal_pass(error)
  type(error_type), allocatable, intent(out) :: error
  integer(i4) :: a(2,2) = reshape([1_i4,2_i4,3_i4,4_i4],[2,2])
  integer(i4) :: b(2,2) = reshape([1_i4,2_i4,3_i4,4_i4],[2,2])
  call check_equal(error, a, b, message="int 2d equal pass")
end subroutine int_2d_equal_pass

subroutine int_2d_mismatch_fail(error)
  type(error_type), allocatable, intent(out) :: error
  integer(i4) :: a(2,2) = reshape([1_i4,2_i4,3_i4,4_i4],[2,2])
  integer(i4) :: b(2,2) = reshape([1_i4,9_i4,3_i4,4_i4],[2,2])  ! one mismatch
  call check_equal(error, a, b, message="int 2d mismatch should fail")
end subroutine int_2d_mismatch_fail

subroutine real_sp_1d_tol_pass(error)
  type(error_type), allocatable, intent(out) :: error
  real(sp) :: a(2) = [1.0_sp + 5.0e-8_sp, 2.0_sp - 5.0e-8_sp]
  real(sp) :: b(2) = [1.0_sp,              2.0_sp             ]
  call check_equal(error, a, b, message="sp abs tol", thr=1.0e-7_sp, rel=.false.)
end subroutine real_sp_1d_tol_pass

subroutine real_dp_1d_rel_tol_pass(error)
  type(error_type), allocatable, intent(out) :: error
  real(dp), parameter :: r = 5.0e-13_dp
  real(dp) :: b(3) = [1.0_dp, -2.0_dp, 3.0_dp]
  real(dp) :: a(3)
  a = b*(1.0_dp + r)   ! relative error r
  call check_equal(error, a, b, message="dp rel tol", thr=1.0e-12_dp, rel=.true.)
end subroutine real_dp_1d_rel_tol_pass

subroutine real_dp_1d_nan_equal_pass(error)
  type(error_type), allocatable, intent(out) :: error
  real(dp) :: nan, a(3), b(3)
  nan = ieee_value(1.0_dp, ieee_quiet_nan)
  a = [1.0_dp, nan, 3.0_dp]
  b = [1.0_dp, nan, 3.0_dp]
  call check_equal(error, a, b, message="dp NaNs allowed", nan_equal=.true.)
end subroutine real_dp_1d_nan_equal_pass

subroutine real_sp_subset_rows_cols(error)
  type(error_type), allocatable, intent(out) :: error
  real(sp) :: a(3,4), b(3,4)
  integer  :: rows(2) = [1,3]
  integer  :: cols(2) = [2,4]
  integer :: i,j
  do j=1,4; do i=1,3
    a(i,j) = real(10*j + i, sp)
    b(i,j) = a(i,j)
  end do; end do
  ! differences outside the compared submatrix (rows=1,3 ; cols=2,4)
  b(2,2) = b(2,2) + 1.0_sp
  b(2,4) = b(2,4) - 1.0_sp
  call check_equal(error, a, b, message="sp subset rows/cols", indx1=rows, indx2=cols)
end subroutine real_sp_subset_rows_cols

subroutine real_dp_3d_equal_pass(error)
  type(error_type), allocatable, intent(out) :: error
  real(dp) :: a(2,2,2), b(2,2,2)
  integer :: k
  a = reshape([ (real(k,dp), k=1,8 ) ], shape(a))
  b = a
  call check_equal(error, a, b, message="dp 3d equal")
end subroutine real_dp_3d_equal_pass

subroutine shape_mismatch_fail(error)
  type(error_type), allocatable, intent(out) :: error
  real(sp) :: a(2,2) = 0.0_sp
  real(sp) :: b(2,3) = 0.0_sp
  call check_equal(error, a, b, message="shape mismatch should fail")
end subroutine shape_mismatch_fail

end module test_multidim

