

! Uncomment to have a submodule
! Now these subroutines bodies will be included directly in testdrive.F90
!submodule (testdrive) testdrive_multidim
!  implicit none
!
!  double precision :: threshold_default = 1.e-6
!
!contains

!======================
! Integers: exact match
!======================
!module subroutine check_equal_i2_1d( &
subroutine check_equal_i2_1d( &
      error, actual, expect, message, show &
    , indx1            &
)
  type(error_type), allocatable, intent(out) :: error
  integer(i2), intent(in) :: actual(:)
  integer(i2), intent(in) :: expect(:)
  character(len=*), intent(in), optional :: message
  logical, intent(in), optional :: show
  integer, intent(in), optional :: indx1(:)

  if (.not.all(shape(actual) == shape(expect))) then
    if (present(message)) then
      call test_failed(error, message)
    else
      call test_failed(error, "Array shape mismatch")
    end if
    return
  end if

  if (.not.all(actual == expect)) then
    if (present(message)) then
      call test_failed(error, message)
    else
      call test_failed(error, "Integer array mismatch")
    end if
  end if
end subroutine


!module subroutine check_equal_i2_2d( &
subroutine check_equal_i2_2d( &
      error, actual, expect, message, show &
    , indx1, indx2        &
)
  type(error_type), allocatable, intent(out) :: error
  integer(i2), intent(in) :: actual(:,:)
  integer(i2), intent(in) :: expect(:,:)
  character(len=*), intent(in), optional :: message
  logical, intent(in), optional :: show
  integer, intent(in), optional :: indx1(:)
  integer, intent(in), optional :: indx2(:)

  if (.not.all(shape(actual) == shape(expect))) then
    if (present(message)) then
      call test_failed(error, message)
    else
      call test_failed(error, "Array shape mismatch")
    end if
    return
  end if

  if (.not.all(actual == expect)) then
    if (present(message)) then
      call test_failed(error, message)
    else
      call test_failed(error, "Integer array mismatch")
    end if
  end if
end subroutine


!module subroutine check_equal_i2_3d( &
subroutine check_equal_i2_3d( &
      error, actual, expect, message, show &
    , indx1, indx2, indx3    &
)
  type(error_type), allocatable, intent(out) :: error
  integer(i2), intent(in) :: actual(:,:,:)
  integer(i2), intent(in) :: expect(:,:,:)
  character(len=*), intent(in), optional :: message
  logical, intent(in), optional :: show
  integer, intent(in), optional :: indx1(:)
  integer, intent(in), optional :: indx2(:)
  integer, intent(in), optional :: indx3(:)

  if (.not.all(shape(actual) == shape(expect))) then
    if (present(message)) then
      call test_failed(error, message)
    else
      call test_failed(error, "Array shape mismatch")
    end if
    return
  end if

  if (.not.all(actual == expect)) then
    if (present(message)) then
      call test_failed(error, message)
    else
      call test_failed(error, "Integer array mismatch")
    end if
  end if
end subroutine


!module subroutine check_equal_i4_1d( &
subroutine check_equal_i4_1d( &
      error, actual, expect, message, show &
    , indx1            &
)
  type(error_type), allocatable, intent(out) :: error
  integer(i4), intent(in) :: actual(:)
  integer(i4), intent(in) :: expect(:)
  character(len=*), intent(in), optional :: message
  logical, intent(in), optional :: show
  integer, intent(in), optional :: indx1(:)

  if (.not.all(shape(actual) == shape(expect))) then
    if (present(message)) then
      call test_failed(error, message)
    else
      call test_failed(error, "Array shape mismatch")
    end if
    return
  end if

  if (.not.all(actual == expect)) then
    if (present(message)) then
      call test_failed(error, message)
    else
      call test_failed(error, "Integer array mismatch")
    end if
  end if
end subroutine


!module subroutine check_equal_i4_2d( &
subroutine check_equal_i4_2d( &
      error, actual, expect, message, show &
    , indx1, indx2        &
)
  type(error_type), allocatable, intent(out) :: error
  integer(i4), intent(in) :: actual(:,:)
  integer(i4), intent(in) :: expect(:,:)
  character(len=*), intent(in), optional :: message
  logical, intent(in), optional :: show
  integer, intent(in), optional :: indx1(:)
  integer, intent(in), optional :: indx2(:)

  if (.not.all(shape(actual) == shape(expect))) then
    if (present(message)) then
      call test_failed(error, message)
    else
      call test_failed(error, "Array shape mismatch")
    end if
    return
  end if

  if (.not.all(actual == expect)) then
    if (present(message)) then
      call test_failed(error, message)
    else
      call test_failed(error, "Integer array mismatch")
    end if
  end if
end subroutine


!module subroutine check_equal_i4_3d( &
subroutine check_equal_i4_3d( &
      error, actual, expect, message, show &
    , indx1, indx2, indx3    &
)
  type(error_type), allocatable, intent(out) :: error
  integer(i4), intent(in) :: actual(:,:,:)
  integer(i4), intent(in) :: expect(:,:,:)
  character(len=*), intent(in), optional :: message
  logical, intent(in), optional :: show
  integer, intent(in), optional :: indx1(:)
  integer, intent(in), optional :: indx2(:)
  integer, intent(in), optional :: indx3(:)

  if (.not.all(shape(actual) == shape(expect))) then
    if (present(message)) then
      call test_failed(error, message)
    else
      call test_failed(error, "Array shape mismatch")
    end if
    return
  end if

  if (.not.all(actual == expect)) then
    if (present(message)) then
      call test_failed(error, message)
    else
      call test_failed(error, "Integer array mismatch")
    end if
  end if
end subroutine


!module subroutine check_equal_i8_1d( &
subroutine check_equal_i8_1d( &
      error, actual, expect, message, show &
    , indx1            &
)
  type(error_type), allocatable, intent(out) :: error
  integer(i8), intent(in) :: actual(:)
  integer(i8), intent(in) :: expect(:)
  character(len=*), intent(in), optional :: message
  logical, intent(in), optional :: show
  integer, intent(in), optional :: indx1(:)

  if (.not.all(shape(actual) == shape(expect))) then
    if (present(message)) then
      call test_failed(error, message)
    else
      call test_failed(error, "Array shape mismatch")
    end if
    return
  end if

  if (.not.all(actual == expect)) then
    if (present(message)) then
      call test_failed(error, message)
    else
      call test_failed(error, "Integer array mismatch")
    end if
  end if
end subroutine


!module subroutine check_equal_i8_2d( &
subroutine check_equal_i8_2d( &
      error, actual, expect, message, show &
    , indx1, indx2        &
)
  type(error_type), allocatable, intent(out) :: error
  integer(i8), intent(in) :: actual(:,:)
  integer(i8), intent(in) :: expect(:,:)
  character(len=*), intent(in), optional :: message
  logical, intent(in), optional :: show
  integer, intent(in), optional :: indx1(:)
  integer, intent(in), optional :: indx2(:)

  if (.not.all(shape(actual) == shape(expect))) then
    if (present(message)) then
      call test_failed(error, message)
    else
      call test_failed(error, "Array shape mismatch")
    end if
    return
  end if

  if (.not.all(actual == expect)) then
    if (present(message)) then
      call test_failed(error, message)
    else
      call test_failed(error, "Integer array mismatch")
    end if
  end if
end subroutine


!module subroutine check_equal_i8_3d( &
subroutine check_equal_i8_3d( &
      error, actual, expect, message, show &
    , indx1, indx2, indx3    &
)
  type(error_type), allocatable, intent(out) :: error
  integer(i8), intent(in) :: actual(:,:,:)
  integer(i8), intent(in) :: expect(:,:,:)
  character(len=*), intent(in), optional :: message
  logical, intent(in), optional :: show
  integer, intent(in), optional :: indx1(:)
  integer, intent(in), optional :: indx2(:)
  integer, intent(in), optional :: indx3(:)

  if (.not.all(shape(actual) == shape(expect))) then
    if (present(message)) then
      call test_failed(error, message)
    else
      call test_failed(error, "Array shape mismatch")
    end if
    return
  end if

  if (.not.all(actual == expect)) then
    if (present(message)) then
      call test_failed(error, message)
    else
      call test_failed(error, "Integer array mismatch")
    end if
  end if
end subroutine



!========================================
! Reals: abs/rel threshold + optional NaN
!========================================
!module subroutine check_equal_sp_1d( &
subroutine check_equal_sp_1d( &
      error, actual, expect &
    , message, thr, rel, show, nan_equal &
  , indx1              &
)
  type(error_type), allocatable, intent(out) :: error
  real(sp), intent(in) :: actual(:)
  real(sp), intent(in) :: expect(:)
  character(len=*), intent(in), optional :: message
  real(sp)  , intent(in), optional :: thr
  logical         , intent(in), optional :: rel
  logical         , intent(in), optional :: show
  logical         , intent(in), optional :: nan_equal 
  !
  integer, intent(in), optional :: indx1(:)
  ! Manage optional arguments
  character(len=:), allocatable :: msg
  real(sp) :: threshold
  logical :: relative
  logical :: show_array

  logical :: ok, allow_nan, nan_ok

  ! Other 
  double precision, allocatable :: arr(:,:)
  logical, allocatable :: mask(:,:)
  !integer :: row, col 
  integer :: n1, n2, n3 ! bounds (per-rank)


  ! Check arguments
  if (any(shape(actual) /= shape(expect))) then
    print *, "X Arrays have different shapes."
    return
  end if

  ! Manage optional arguments
  threshold = epsilon(1.0_sp)
  if (present(thr)) threshold = thr

  relative = .false.
  if (present(rel)) relative = rel
  
  nan_ok   = .false.
  if (present(nan_equal)) nan_ok = nan_equal

  if (present(show)) then 
    show_array = show
  else
    show_array = .True.
  end if

  ! bound checks
  n1 = size(actual,1)
  if (present(indx1)) then
    if (size(indx1)==0 .or. any(indx1<1 .or. indx1>n1)) then
      call test_failed(error, "Index indx1 out of bounds"); return
    end if
  end if


  if (present(message)) then
    allocate(character(len=len_trim(message)) :: msg)
    msg = trim(message)
  else
    msg = "Difference is: "
  end if

  if (present(rel)) then
    relative = rel
  else
    relative = .false.
  end if

  ! Check shape compatibility
  if (.not.all(shape(actual) == shape(expect))) then
    if (present(message)) then
      call test_failed(error, message)
    else
      call test_failed(error, "Array shape mismatch")
    end if
    return
  end if

  ! Check values
  if (relative) then
    ! Relative criterion: abs(a-b)/abs(b) <= thr
    ok = all( (abs(actual - expect) / abs(expect)) <= threshold )
  else
    ! Absolute criterion: abs(a-b) <= thr
    ok = all( abs(actual - expect) <= threshold )
  end if

  if (.not.ok .and. allow_nan) then
    ok = all( (relative .and. (abs(actual - expect) / abs(expect) <= threshold)) .or. &
              ((.not.relative) .and. (abs(actual - expect) <= threshold)) .or. &
              (is_nan(actual) .and. is_nan(expect)) )
  end if

  if (.not.ok) then
    if (present(message)) then
      call test_failed(error, message)
    else
      call test_failed(error, "Real array mismatch")
    end if
  end if
end subroutine


!module subroutine check_equal_sp_2d( &
subroutine check_equal_sp_2d( &
      error, actual, expect &
    , message, thr, rel, show, nan_equal &
  , indx1, indx2          &
)
  type(error_type), allocatable, intent(out) :: error
  real(sp), intent(in) :: actual(:,:)
  real(sp), intent(in) :: expect(:,:)
  !
  character(len=*), intent(in), optional :: message
  real(sp)  , intent(in), optional :: thr
  logical         , intent(in), optional :: rel
  logical         , intent(in), optional :: show
  logical         , intent(in), optional :: nan_equal 
  !
  integer, intent(in), optional :: indx1(:)
  integer, intent(in), optional :: indx2(:)
  ! Manage optional arguments
  character(len=:), allocatable :: msg
  real(sp) :: threshold
  logical :: relative
  logical :: show_array

  logical :: ok, allow_nan, nan_ok

  ! Other 
  double precision, allocatable :: arr(:,:)
  logical, allocatable :: mask(:,:)
  !integer :: row, col 
  integer :: n1, n2, n3 ! bounds (per-rank)


  ! Check arguments
  if (any(shape(actual) /= shape(expect))) then
    print *, "X Arrays have different shapes."
    return
  end if

  ! Manage optional arguments
  threshold = epsilon(1.0_sp)
  if (present(thr)) threshold = thr

  relative = .false.
  if (present(rel)) relative = rel
  
  nan_ok   = .false.
  if (present(nan_equal)) nan_ok = nan_equal

  if (present(show)) then 
    show_array = show
  else
    show_array = .True.
  end if

  ! bound checks
  n1 = size(actual,1)
  n2 = size(actual,2)
  if (present(indx1)) then
    if (size(indx1)==0 .or. any(indx1<1 .or. indx1>n1)) then
      call test_failed(error, "Index indx1 out of bounds"); return
    end if
  end if
  if (present(indx2)) then
    if (size(indx2)==0 .or. any(indx2<1 .or. indx2>n2)) then
      call test_failed(error, "Index indx2 out of bounds"); return
    end if
  end if


  if (present(message)) then
    allocate(character(len=len_trim(message)) :: msg)
    msg = trim(message)
  else
    msg = "Difference is: "
  end if

  if (present(rel)) then
    relative = rel
  else
    relative = .false.
  end if

  ! Check shape compatibility
  if (.not.all(shape(actual) == shape(expect))) then
    if (present(message)) then
      call test_failed(error, message)
    else
      call test_failed(error, "Array shape mismatch")
    end if
    return
  end if

  ! Check values
  if (relative) then
    ! Relative criterion: abs(a-b)/abs(b) <= thr
    ok = all( (abs(actual - expect) / abs(expect)) <= threshold )
  else
    ! Absolute criterion: abs(a-b) <= thr
    ok = all( abs(actual - expect) <= threshold )
  end if

  if (.not.ok .and. allow_nan) then
    ok = all( (relative .and. (abs(actual - expect) / abs(expect) <= threshold)) .or. &
              ((.not.relative) .and. (abs(actual - expect) <= threshold)) .or. &
              (is_nan(actual) .and. is_nan(expect)) )
  end if

  if (.not.ok) then
    if (present(message)) then
      call test_failed(error, message)
    else
      call test_failed(error, "Real array mismatch")
    end if
  end if
end subroutine


!module subroutine check_equal_sp_3d( &
subroutine check_equal_sp_3d( &
      error, actual, expect &
    , message, thr, rel, show, nan_equal &
  , indx1, indx2, indx3      &
)
  type(error_type), allocatable, intent(out) :: error
  real(sp), intent(in) :: actual(:,:,:)
  real(sp), intent(in) :: expect(:,:,:)
  !
  character(len=*), intent(in), optional :: message
  real(sp)  , intent(in), optional :: thr
  logical         , intent(in), optional :: rel
  logical         , intent(in), optional :: show
  logical         , intent(in), optional :: nan_equal 
  !
  integer, intent(in), optional :: indx1(:)
  integer, intent(in), optional :: indx2(:)
  integer, intent(in), optional :: indx3(:)
  ! Manage optional arguments
  character(len=:), allocatable :: msg
  real(sp) :: threshold
  logical :: relative
  logical :: show_array

  logical :: ok, allow_nan, nan_ok

  ! Other 
  double precision, allocatable :: arr(:,:)
  logical, allocatable :: mask(:,:)
  !integer :: row, col 
  integer :: n1, n2, n3 ! bounds (per-rank)


  ! Check arguments
  if (any(shape(actual) /= shape(expect))) then
    print *, "X Arrays have different shapes."
    return
  end if

  ! Manage optional arguments
  threshold = epsilon(1.0_sp)
  if (present(thr)) threshold = thr

  relative = .false.
  if (present(rel)) relative = rel
  
  nan_ok   = .false.
  if (present(nan_equal)) nan_ok = nan_equal

  if (present(show)) then 
    show_array = show
  else
    show_array = .True.
  end if

  ! bound checks
  n1 = size(actual,1)
  n2 = size(actual,2)
  n3 = size(actual,3)
  if (present(indx1)) then
    if (size(indx1)==0 .or. any(indx1<1 .or. indx1>n1)) then
      call test_failed(error, "Index indx1 out of bounds"); return
    end if
  end if
  if (present(indx2)) then
    if (size(indx2)==0 .or. any(indx2<1 .or. indx2>n2)) then
      call test_failed(error, "Index indx2 out of bounds"); return
    end if
  end if
  if (present(indx3)) then
    if (size(indx3)==0 .or. any(indx3<1 .or. indx3>n3)) then
      call test_failed(error, "Index indx3 out of bounds"); return
    end if
  end if


  if (present(message)) then
    allocate(character(len=len_trim(message)) :: msg)
    msg = trim(message)
  else
    msg = "Difference is: "
  end if

  if (present(rel)) then
    relative = rel
  else
    relative = .false.
  end if

  ! Check shape compatibility
  if (.not.all(shape(actual) == shape(expect))) then
    if (present(message)) then
      call test_failed(error, message)
    else
      call test_failed(error, "Array shape mismatch")
    end if
    return
  end if

  ! Check values
  if (relative) then
    ! Relative criterion: abs(a-b)/abs(b) <= thr
    ok = all( (abs(actual - expect) / abs(expect)) <= threshold )
  else
    ! Absolute criterion: abs(a-b) <= thr
    ok = all( abs(actual - expect) <= threshold )
  end if

  if (.not.ok .and. allow_nan) then
    ok = all( (relative .and. (abs(actual - expect) / abs(expect) <= threshold)) .or. &
              ((.not.relative) .and. (abs(actual - expect) <= threshold)) .or. &
              (is_nan(actual) .and. is_nan(expect)) )
  end if

  if (.not.ok) then
    if (present(message)) then
      call test_failed(error, message)
    else
      call test_failed(error, "Real array mismatch")
    end if
  end if
end subroutine


!module subroutine check_equal_dp_1d( &
subroutine check_equal_dp_1d( &
      error, actual, expect &
    , message, thr, rel, show, nan_equal &
  , indx1              &
)
  type(error_type), allocatable, intent(out) :: error
  real(dp), intent(in) :: actual(:)
  real(dp), intent(in) :: expect(:)
  !
  character(len=*), intent(in), optional :: message
  real(dp)  , intent(in), optional :: thr
  logical         , intent(in), optional :: rel
  logical         , intent(in), optional :: show
  logical         , intent(in), optional :: nan_equal 
  !
  integer, intent(in), optional :: indx1(:)
  ! Manage optional arguments
  character(len=:), allocatable :: msg
  real(dp) :: threshold
  logical :: relative
  logical :: show_array

  logical :: ok, allow_nan, nan_ok

  ! Other 
  double precision, allocatable :: arr(:,:)
  logical, allocatable :: mask(:,:)
  !integer :: row, col 
  integer :: n1, n2, n3 ! bounds (per-rank)


  ! Check arguments
  if (any(shape(actual) /= shape(expect))) then
    print *, "X Arrays have different shapes."
    return
  end if

  ! Manage optional arguments
  threshold = epsilon(1.0_dp)
  if (present(thr)) threshold = thr

  relative = .false.
  if (present(rel)) relative = rel
  
  nan_ok   = .false.
  if (present(nan_equal)) nan_ok = nan_equal

  if (present(show)) then 
    show_array = show
  else
    show_array = .True.
  end if

  ! bound checks
  n1 = size(actual,1)
  if (present(indx1)) then
    if (size(indx1)==0 .or. any(indx1<1 .or. indx1>n1)) then
      call test_failed(error, "Index indx1 out of bounds"); return
    end if
  end if


  if (present(message)) then
    allocate(character(len=len_trim(message)) :: msg)
    msg = trim(message)
  else
    msg = "Difference is: "
  end if

  if (present(rel)) then
    relative = rel
  else
    relative = .false.
  end if

  ! Check shape compatibility
  if (.not.all(shape(actual) == shape(expect))) then
    if (present(message)) then
      call test_failed(error, message)
    else
      call test_failed(error, "Array shape mismatch")
    end if
    return
  end if

  ! Check values
  if (relative) then
    ! Relative criterion: abs(a-b)/abs(b) <= thr
    ok = all( (abs(actual - expect) / abs(expect)) <= threshold )
  else
    ! Absolute criterion: abs(a-b) <= thr
    ok = all( abs(actual - expect) <= threshold )
  end if

  if (.not.ok .and. allow_nan) then
    ok = all( (relative .and. (abs(actual - expect) / abs(expect) <= threshold)) .or. &
              ((.not.relative) .and. (abs(actual - expect) <= threshold)) .or. &
              (is_nan(actual) .and. is_nan(expect)) )
  end if

  if (.not.ok) then
    if (present(message)) then
      call test_failed(error, message)
    else
      call test_failed(error, "Real array mismatch")
    end if
  end if
end subroutine


!module subroutine check_equal_dp_2d( &
subroutine check_equal_dp_2d( &
      error, actual, expect &
    , message, thr, rel, show, nan_equal &
  , indx1, indx2          &
)
  type(error_type), allocatable, intent(out) :: error
  real(dp), intent(in) :: actual(:,:)
  real(dp), intent(in) :: expect(:,:)
  !
  character(len=*), intent(in), optional :: message
  real(dp)  , intent(in), optional :: thr
  logical         , intent(in), optional :: rel
  logical         , intent(in), optional :: show
  logical         , intent(in), optional :: nan_equal 
  !
  integer, intent(in), optional :: indx1(:)
  integer, intent(in), optional :: indx2(:)
  ! Manage optional arguments
  character(len=:), allocatable :: msg
  real(dp) :: threshold
  logical :: relative
  logical :: show_array

  logical :: ok, allow_nan, nan_ok

  ! Other 
  double precision, allocatable :: arr(:,:)
  logical, allocatable :: mask(:,:)
  !integer :: row, col 
  integer :: n1, n2, n3 ! bounds (per-rank)


  ! Check arguments
  if (any(shape(actual) /= shape(expect))) then
    print *, "X Arrays have different shapes."
    return
  end if

  ! Manage optional arguments
  threshold = epsilon(1.0_dp)
  if (present(thr)) threshold = thr

  relative = .false.
  if (present(rel)) relative = rel
  
  nan_ok   = .false.
  if (present(nan_equal)) nan_ok = nan_equal

  if (present(show)) then 
    show_array = show
  else
    show_array = .True.
  end if

  ! bound checks
  n1 = size(actual,1)
  n2 = size(actual,2)
  if (present(indx1)) then
    if (size(indx1)==0 .or. any(indx1<1 .or. indx1>n1)) then
      call test_failed(error, "Index indx1 out of bounds"); return
    end if
  end if
  if (present(indx2)) then
    if (size(indx2)==0 .or. any(indx2<1 .or. indx2>n2)) then
      call test_failed(error, "Index indx2 out of bounds"); return
    end if
  end if


  if (present(message)) then
    allocate(character(len=len_trim(message)) :: msg)
    msg = trim(message)
  else
    msg = "Difference is: "
  end if

  if (present(rel)) then
    relative = rel
  else
    relative = .false.
  end if

  ! Check shape compatibility
  if (.not.all(shape(actual) == shape(expect))) then
    if (present(message)) then
      call test_failed(error, message)
    else
      call test_failed(error, "Array shape mismatch")
    end if
    return
  end if

  ! Check values
  if (relative) then
    ! Relative criterion: abs(a-b)/abs(b) <= thr
    ok = all( (abs(actual - expect) / abs(expect)) <= threshold )
  else
    ! Absolute criterion: abs(a-b) <= thr
    ok = all( abs(actual - expect) <= threshold )
  end if

  if (.not.ok .and. allow_nan) then
    ok = all( (relative .and. (abs(actual - expect) / abs(expect) <= threshold)) .or. &
              ((.not.relative) .and. (abs(actual - expect) <= threshold)) .or. &
              (is_nan(actual) .and. is_nan(expect)) )
  end if

  if (.not.ok) then
    if (present(message)) then
      call test_failed(error, message)
    else
      call test_failed(error, "Real array mismatch")
    end if
  end if
end subroutine


!module subroutine check_equal_dp_3d( &
subroutine check_equal_dp_3d( &
      error, actual, expect &
    , message, thr, rel, show, nan_equal &
  , indx1, indx2, indx3      &
)
  type(error_type), allocatable, intent(out) :: error
  real(dp), intent(in) :: actual(:,:,:)
  real(dp), intent(in) :: expect(:,:,:)
  !
  character(len=*), intent(in), optional :: message
  real(dp)  , intent(in), optional :: thr
  logical         , intent(in), optional :: rel
  logical         , intent(in), optional :: show
  logical         , intent(in), optional :: nan_equal 
  !
  integer, intent(in), optional :: indx1(:)
  integer, intent(in), optional :: indx2(:)
  integer, intent(in), optional :: indx3(:)
  ! Manage optional arguments
  character(len=:), allocatable :: msg
  real(dp) :: threshold
  logical :: relative
  logical :: show_array

  logical :: ok, allow_nan, nan_ok

  ! Other 
  double precision, allocatable :: arr(:,:)
  logical, allocatable :: mask(:,:)
  !integer :: row, col 
  integer :: n1, n2, n3 ! bounds (per-rank)


  ! Check arguments
  if (any(shape(actual) /= shape(expect))) then
    print *, "X Arrays have different shapes."
    return
  end if

  ! Manage optional arguments
  threshold = epsilon(1.0_dp)
  if (present(thr)) threshold = thr

  relative = .false.
  if (present(rel)) relative = rel
  
  nan_ok   = .false.
  if (present(nan_equal)) nan_ok = nan_equal

  if (present(show)) then 
    show_array = show
  else
    show_array = .True.
  end if

  ! bound checks
  n1 = size(actual,1)
  n2 = size(actual,2)
  n3 = size(actual,3)
  if (present(indx1)) then
    if (size(indx1)==0 .or. any(indx1<1 .or. indx1>n1)) then
      call test_failed(error, "Index indx1 out of bounds"); return
    end if
  end if
  if (present(indx2)) then
    if (size(indx2)==0 .or. any(indx2<1 .or. indx2>n2)) then
      call test_failed(error, "Index indx2 out of bounds"); return
    end if
  end if
  if (present(indx3)) then
    if (size(indx3)==0 .or. any(indx3<1 .or. indx3>n3)) then
      call test_failed(error, "Index indx3 out of bounds"); return
    end if
  end if


  if (present(message)) then
    allocate(character(len=len_trim(message)) :: msg)
    msg = trim(message)
  else
    msg = "Difference is: "
  end if

  if (present(rel)) then
    relative = rel
  else
    relative = .false.
  end if

  ! Check shape compatibility
  if (.not.all(shape(actual) == shape(expect))) then
    if (present(message)) then
      call test_failed(error, message)
    else
      call test_failed(error, "Array shape mismatch")
    end if
    return
  end if

  ! Check values
  if (relative) then
    ! Relative criterion: abs(a-b)/abs(b) <= thr
    ok = all( (abs(actual - expect) / abs(expect)) <= threshold )
  else
    ! Absolute criterion: abs(a-b) <= thr
    ok = all( abs(actual - expect) <= threshold )
  end if

  if (.not.ok .and. allow_nan) then
    ok = all( (relative .and. (abs(actual - expect) / abs(expect) <= threshold)) .or. &
              ((.not.relative) .and. (abs(actual - expect) <= threshold)) .or. &
              (is_nan(actual) .and. is_nan(expect)) )
  end if

  if (.not.ok) then
    if (present(message)) then
      call test_failed(error, message)
    else
      call test_failed(error, "Real array mismatch")
    end if
  end if
end subroutine



!end submodule testdrive_multidim

