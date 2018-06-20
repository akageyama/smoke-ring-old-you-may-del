!-----------------------------------------------------------------------------
! smoke-ring: A simple 3-D Fluid Solver by FDM on Cartesian Grid.
!
!    by Akira Kageyama,
!       Department of Computational Science,
!       Kobe University, Japan.
!       email: kage@port.kobe-u.ac.jp or sgks@mac.com
!-----------------------------------------------------------------------------
! namelist.f90
!     2008.06.02: Developed by Akira Kageyama. Copied from kindanb.
!     2018.04.12: Copied from boxfluid.
!-----------------------------------------------------------------------------

module namelist_m
!*****************************************************************************
! MODULE NAMELIST                                              Namelist Loader
!*****************************************************************************
  use constants_m
  use ut_m
  implicit none
  private
  public :: & !< routines >!
            namelist__double,   &
            namelist__integer,  &
            namelist__logical,  &
            namelist__read,     &
            namelist__string

  logical, save :: Read_done = .false.

  integer                              :: Total_nloop
  integer                              :: Kvs_nskip, Slicedata_nskip
  character(len=TAG_STRING_LENGTH_MAX) :: Kvs_tag,   Slicedata_tag
  real(DP)                             :: Viscosity, Kappa
  logical                              :: Debug

  namelist /data00/ Total_nloop
  namelist /data01/ Kvs_nskip,        Kvs_tag
  namelist /data02/ Slicedata_nskip,  Slicedata_tag
  namelist /data03/ Viscosity
  namelist /data04/ Kappa
  namelist /data05/ Debug


contains


!==============
!    Public
!==============

!________________________________________________________________public__
!
  function namelist__double(variable)
    character(len=*), intent(in) :: variable
    real(DP) :: namelist__double
!________________________________________________________________________
!
    call ut__assert(Read_done, &
                    '<namelist__double> Read namelist file first.')

    select case (variable)
    case                ('Kappa')
       namelist__double = Kappa
    case                ('Viscosity')
       namelist__double = Viscosity
    case default
       call ut__message('? arg = ', variable)
       call ut__fatal('<namelist__double> not in the namelist?')
    end select

  end function namelist__double


!________________________________________________________________public__
!
  function namelist__integer(variable)
    character(len=*), intent(in) :: variable
    integer :: namelist__integer
!________________________________________________________________________
!
    call ut__assert(Read_done, &
                    '<namelist__integer> Read namelist file first.')

    select case (variable)
    case                 ('Kvs_nskip')
       namelist__integer = Kvs_nskip
    case                 ('Slicedata_nskip')
       namelist__integer = Slicedata_nskip
    case                 ('Total_nloop')
       namelist__integer = Total_nloop
    case default
       call ut__message('? arg = ', variable)
       call ut__fatal('<namelist__integer> not in the namelist?')
    end select

  end function namelist__integer


!________________________________________________________________public__
!
  function namelist__logical(variable)
    character(len=*), intent(in) :: variable
    logical :: namelist__logical
!________________________________________________________________________
!
    call ut__assert(Read_done, &
                    '<namelist__logical> Read namelist file first.')

    select case (variable)
    case                 ('Debug')
       namelist__logical = Debug
    case default
       call ut__message('? arg = ', variable)
       call ut__fatal('<namelist__logical> not in the namelist?')
    end select

  end function namelist__logical


!________________________________________________________________public__
!
  subroutine namelist__read
!________________________________________________________________________
!

    read(FILE_NAMELIST,nml=data00)
    read(FILE_NAMELIST,nml=data01)
    read(FILE_NAMELIST,nml=data02)
    read(FILE_NAMELIST,nml=data03)
    read(FILE_NAMELIST,nml=data04)
    read(FILE_NAMELIST,nml=data05)

    write(FILE_STANDARD_OUT,nml=data00)
    write(FILE_STANDARD_OUT,nml=data01)
    write(FILE_STANDARD_OUT,nml=data02)
    write(FILE_STANDARD_OUT,nml=data03)
    write(FILE_STANDARD_OUT,nml=data04)
    write(FILE_STANDARD_OUT,nml=data05)

    Read_done = .true.

  end subroutine namelist__read


!________________________________________________________________public__
!
  function namelist__string(variable)
    character(len=*), intent(in) :: variable
    character(len=TAG_STRING_LENGTH_MAX) :: namelist__string
!________________________________________________________________________
!
    call ut__assert(Read_done, &
                    '<namelist__string> Read namelist file first.')

    select case         (variable)
    case                ('Kvs_tag')
       namelist__string = Kvs_tag
    case                ('Slicedata_tag')
       namelist__string = Slicedata_tag
    case default
       call ut__message('? arg = ', variable)
       call ut__fatal('<namelist__string> not in the namelist?')
    end select

  end function namelist__string

end module namelist_m
