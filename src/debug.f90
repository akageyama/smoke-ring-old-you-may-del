!-----------------------------------------------------------------------------
! smoke-ring: A simple 3-D Fluid Solver by FDM on Cartesian Grid.
!
!    by Akira Kageyama,
!       Department of Computational Science,
!       Kobe University, Japan.
!       email: kage@port.kobe-u.ac.jp or sgks@mac.com
!-----------------------------------------------------------------------------
! debug.f90
!     2008.06.02: Developed by Akira Kageyama.
!     2018.04.12: Copied from boxfluid.
!-----------------------------------------------------------------------------

module debug_m
!*****************************************************************************
! MODULE DEBUG                                                   For Debugging
!*****************************************************************************
  use constants_m
  use ut_m
  use namelist_m
  implicit none

  private
  public :: debug__message

  interface debug__message
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
!
    if (namelist__logical('Debug')) then
       call ut__message(mark, 'debug: '//string)
    end if

  end subroutine message_decorated_str


!_______________________________________________________________private__
!
  subroutine message_decorated_str_int(mark,string,int)
    character, intent(in)        :: mark
    character(len=*), intent(in) :: string
    integer, intent(in)          :: int
!________________________________________________________________________
!
    if (namelist__logical('Debug')) then
       call ut__message(mark, 'debug: '//string, int)
    end if

  end subroutine message_decorated_str_int


!_______________________________________________________________private__
!
  subroutine message_str(string)
    character(len=*), intent(in) :: string
!________________________________________________________________________
!
    if (namelist__logical('Debug')) then
       call ut__message('debug: '//string)
    end if

  end subroutine message_str


!_______________________________________________________________private__
!
  subroutine message_str_double(string, double)
    character(len=*), intent(in) :: string
    real(DP), intent(in)         :: double
!________________________________________________________________________
!
    if (namelist__logical('Debug')) then
       call ut__message('debug: '//string, double)
    end if

  end subroutine message_str_double

!_______________________________________________________________private__
!
  subroutine message_str_double_double(string, double1, double2)
    character(len=*), intent(in) :: string
    real(DP), intent(in)         :: double1
    real(DP), intent(in)         :: double2
!________________________________________________________________________
!
    if (namelist__logical('Debug')) then
       call ut__message('debug: '//string, double1, double2)
    end if

  end subroutine message_str_double_double


!_______________________________________________________________private__
!
  subroutine message_str_int(string, int)
    character(len=*), intent(in) :: string
    integer, intent(in)          :: int
!________________________________________________________________________
!
    if (namelist__logical('Debug')) then
       call ut__message('debug: '//string, int)
    end if

  end subroutine message_str_int


!_______________________________________________________________private__
!
  subroutine message_str_int_int(string, i1, i2)
    character(len=*), intent(in) :: string
    integer, intent(in)          :: i1, i2
!________________________________________________________________________
!
    if (namelist__logical('Debug')) then
       call ut__message('debug: '//string, i1, i2)
    end if

  end subroutine message_str_int_int


!_______________________________________________________________private__
!
  subroutine message_str_int_int_int(string, i1, i2, i3)
    character(len=*), intent(in) :: string
    integer, intent(in)          :: i1, i2, i3
!________________________________________________________________________
!
    if (namelist__logical('Debug')) then
       call ut__message('debug: '//string, i1, i2, i3)
    end if

  end subroutine message_str_int_int_int


!_______________________________________________________________private__
!
  subroutine message_str_int_int_double(string, i1, i2, d1)
    character(len=*), intent(in) :: string
    integer, intent(in)          :: i1, i2
    real(DP), intent(in)         :: d1
!________________________________________________________________________
!
    if (namelist__logical('Debug')) then
       call ut__message('debug: '//string, i1, i2, d1)
    end if

  end subroutine message_str_int_int_double


!_______________________________________________________________private__
!
  subroutine message_str_int_double(string, i1, d1)
    character(len=*), intent(in) :: string
    integer, intent(in)          :: i1
    real(DP), intent(in)         :: d1
!________________________________________________________________________
!
    if (namelist__logical('Debug')) then
       call ut__message('debug: '//string, i1, d1)
    end if

  end subroutine message_str_int_double


!_______________________________________________________________private__
!
  subroutine message_str_int_double_double(string, i1, d1, d2)
    character(len=*), intent(in) :: string
    integer, intent(in)          :: i1
    real(DP), intent(in)         :: d1, d2
!________________________________________________________________________
!
    if (namelist__logical('Debug')) then
       call ut__message('debug: '//string, i1, d1, d2)
    end if

  end subroutine message_str_int_double_double


!_______________________________________________________________private__
!
  subroutine message_str_int_float_float(string, i1, f1, f2)
    character(len=*), intent(in) :: string
    integer, intent(in)          :: i1
    real(SP), intent(in)         :: f1, f2
!________________________________________________________________________
!
    if (namelist__logical('Debug')) then
       call ut__message('debug: '//string, i1, f1, f2)
    end if

  end subroutine message_str_int_float_float


!_______________________________________________________________private__
!
  subroutine message_str_int_str_int(str1, i1, str2, i2)
    character(len=*), intent(in) :: str1, str2
    integer, intent(in)          :: i1, i2
!________________________________________________________________________
!
    if (namelist__logical('Debug')) then
       call ut__message('debug: '//str1, i1, str2 , i2)
    end if

  end subroutine message_str_int_str_int


!==============
!    Public
!==============


end module debug_m
