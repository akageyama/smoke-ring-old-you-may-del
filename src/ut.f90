!-----------------------------------------------------------------------------
! smoke-ring: A simple 3-D Fluid Solver by FDM on Cartesian Grid.
!
!    by Akira Kageyama,
!       Department of Computational Science,
!       Kobe University, Japan.
!       email: kage@port.kobe-u.ac.jp or sgks@mac.com
!-----------------------------------------------------------------------------
! ut.f90
!     2008.06.02: Developed by Akira Kageyama. Copied from kindanb.
!     2018.04.12: Copied from boxfluid.
!     2018.05.16: Added ut__int_to_str7.
!-----------------------------------------------------------------------------

module ut_m
!*****************************************************************************
! MODULE UT                                                  Utility Functions
!*****************************************************************************
  use constants_m
  implicit none
  private
  public :: ut__assert,                 &
            ut__fatal,                  &
            ut__int_to_str3,            &
            ut__int_to_str7,            &
            ut__message

  interface ut__message
     module procedure message_decorated_str,          &
                      message_decorated_str_int,      &
                      message_str,                    &
                      message_str_double,             &
                      message_str_double_double,      &
                      message_str_int,                &
                      message_str_int_int,            &
                      message_str_int_int_int,        &
                      message_str_int_int_double,     &
                      message_str_int_double,         &
                      message_str_int_double_double,  &
                      message_str_int_float_float,    &
                      message_str_int_str_int
  end interface



contains


!===============
!    Private
!===============


!_______________________________________________________________private__
!
  subroutine message_decorated_str(mark,string)
    character, intent(in)        :: mark
    character(len=*), intent(in) :: string
!________________________________________________________________________
! ! Usage:
  !    call message_decorated_str('#',"This is a test.")
  ! Output:
  !                              ###################
  !                              # This is a test. #
  !                              ###################
  !----------------------------------------------------------
    integer :: len

    len = len_trim(string) + 4

    write(FILE_STANDARD_OUT,*) repeat(mark,len)
    write(FILE_STANDARD_OUT,*) mark//' '//trim(string)//' '//mark
    write(FILE_STANDARD_OUT,*) repeat(mark,len)
  end subroutine message_decorated_str


!_______________________________________________________________private__
!
  subroutine message_decorated_str_int(mark,string,int)
    character, intent(in)        :: mark
    character(len=*), intent(in) :: string
    integer, intent(in)          :: int
!________________________________________________________________________
! Usage:
!    call mess...('#','This is message at nloop = ', nloop)
!----------------------------------------------------------
    character(len=200) :: string_int

    write(string_int,'(a,i8)') string, int

    call message_decorated_str(mark, string_int)
  end subroutine message_decorated_str_int


!_______________________________________________________________private__
!
  subroutine message_str(string)
    character(len=*), intent(in) :: string
!________________________________________________________________________
!
    write(FILE_STANDARD_OUT,*) string
  end subroutine message_str


!_______________________________________________________________private__
!
  subroutine message_str_double(string, double)
    character(len=*), intent(in) :: string
    real(DP), intent(in)         :: double
!________________________________________________________________________
!
    write(FILE_STANDARD_OUT,*) string, double
  end subroutine message_str_double

!_______________________________________________________________private__
!
  subroutine message_str_double_double(string, double1, double2)
    character(len=*), intent(in) :: string
    real(DP), intent(in)         :: double1
    real(DP), intent(in)         :: double2
!________________________________________________________________________
!
    write(FILE_STANDARD_OUT,*) string, double1, double2
  end subroutine message_str_double_double


!_______________________________________________________________private__
!
  subroutine message_str_int(string, int)
    character(len=*), intent(in) :: string
    integer, intent(in)          :: int
!________________________________________________________________________
!
   write(FILE_STANDARD_OUT,*) string, int
  end subroutine message_str_int


!_______________________________________________________________private__
!
  subroutine message_str_int_int(string, i1, i2)
    character(len=*), intent(in) :: string
    integer, intent(in)          :: i1, i2
!________________________________________________________________________
!
    write(FILE_STANDARD_OUT,*) string, i1, i2
  end subroutine message_str_int_int


!_______________________________________________________________private__
!
  subroutine message_str_int_int_int(string, i1, i2, i3)
    character(len=*), intent(in) :: string
    integer, intent(in)          :: i1, i2, i3
!________________________________________________________________________
!
    write(FILE_STANDARD_OUT,*) string, i1, i2, i3
  end subroutine message_str_int_int_int


!_______________________________________________________________private__
!
  subroutine message_str_int_int_double(string, i1, i2, d1)
    character(len=*), intent(in) :: string
    integer, intent(in)          :: i1, i2
    real(DP), intent(in)         :: d1
!________________________________________________________________________
!
    write(FILE_STANDARD_OUT,*) string, i1, i2, d1
  end subroutine message_str_int_int_double


!_______________________________________________________________private__
!
  subroutine message_str_int_double(string, i1, d1)
    character(len=*), intent(in) :: string
    integer, intent(in)          :: i1
    real(DP), intent(in)         :: d1
!________________________________________________________________________
!
    write(FILE_STANDARD_OUT,*) string, i1, d1
  end subroutine message_str_int_double


!_______________________________________________________________private__
!
  subroutine message_str_int_double_double(string, i1, d1, d2)
    character(len=*), intent(in) :: string
    integer, intent(in)          :: i1
    real(DP), intent(in)         :: d1, d2
!________________________________________________________________________
!
    write(FILE_STANDARD_OUT,*) string, i1, d1, d2
  end subroutine message_str_int_double_double


!_______________________________________________________________private__
!
  subroutine message_str_int_float_float(string, i1, f1, f2)
    character(len=*), intent(in) :: string
    integer, intent(in)          :: i1
    real(SP), intent(in)         :: f1, f2
!________________________________________________________________________
!
    write(FILE_STANDARD_OUT,*) string, i1, f1, f2
  end subroutine message_str_int_float_float


!_______________________________________________________________private__
!
  subroutine message_str_int_str_int(str1, i1, str2, i2)
    character(len=*), intent(in) :: str1, str2
    integer, intent(in)          :: i1, i2
!________________________________________________________________________
!
    write(FILE_STANDARD_OUT,*) str1, i1, str2, i2
  end subroutine message_str_int_str_int


!==============
!    Public
!==============


!________________________________________________________________public__
!
  subroutine ut__assert(condition, last_will)
    logical, intent(in)          :: condition
    character(len=*), intent(in) :: last_will
!________________________________________________________________________
!
    if (.not.condition) then
       call ut__fatal(last_will)
    end if

  end subroutine ut__assert


!________________________________________________________________public__
!
  subroutine ut__fatal(last_will)
    character(len=*), intent(in) :: last_will
!________________________________________________________________________
!
!  Print fatal message and exit.
!________________________________________________________________________/
!
    call ut__message('!',last_will)
    stop 'Program stopped by ut__fatal.'

  end subroutine ut__fatal


!________________________________________________________________public__
!
  function ut__int_to_str3(i) result(str3)
    integer, intent(in) :: i
    character(len=3)    :: str3
!________________________________________________________________________
!  Convert an integer into 3 characters.
!             e.g., i=10 --> str3="010"
!________________________________________________________________________/
!

    if ( i>999 ) then
       str3 = 'XXX'
    else
       write(str3,'(i3.3)') i
    end if

  end function ut__int_to_str3



!________________________________________________________________public__
!
  function ut__int_to_str7(i) result(str7)
    integer, intent(in) :: i
    character(len=7)    :: str7
!________________________________________________________________________
!  Convert an integer into 7 characters.
!             e.g., i=12345 --> str7="0012345"
!________________________________________________________________________/
!

    if ( i>9999999 ) then
       str7 = 'XXXXXXX'
    else
       write(str7,'(i7.7)') i
    end if

  end function ut__int_to_str7

end module ut_m
