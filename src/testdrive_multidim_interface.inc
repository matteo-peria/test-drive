

  !---- interfaces for the main module ----
  interface check_equal
  
    module procedure check_equal_i2_1d
    module procedure check_equal_i2_2d
    module procedure check_equal_i2_3d
    module procedure check_equal_i4_1d
    module procedure check_equal_i4_2d
    module procedure check_equal_i4_3d
    module procedure check_equal_i8_1d
    module procedure check_equal_i8_2d
    module procedure check_equal_i8_3d
  
    module procedure check_equal_sp_1d
    module procedure check_equal_sp_2d
    module procedure check_equal_sp_3d
    module procedure check_equal_dp_1d
    module procedure check_equal_dp_2d
    module procedure check_equal_dp_3d
  
  end interface check_equal

  public :: check_equal
  
