!-----------------------------------------------------------------------------
! smoke-ring: A simple 3-D Fluid Solver by FDM on Cartesian Grid.
!
!    by Akira Kageyama,
!       Department of Computational Science,
!       Kobe University, Japan.
!       email: kage@port.kobe-u.ac.jp or sgks@mac.com
!-----------------------------------------------------------------------------
! slicedata.f90
!     2008.06.02: Developed by Akira Kageyama. Copied from kindanb.
!     2018.04.12: Copied from boxfluid.
!-----------------------------------------------------------------------------

module slicedata_m
!*****************************************************************************
! MODULE SLICEDATA                                    Generate 2-D Sliced Data
!*****************************************************************************
  use ut_m
  use field_m
  use namelist_m
  use debug_m
  use solver_m
  implicit none

  private
  public :: slicedata__initialize,  &
            slicedata__write

  ! - 2-D single precision real arrays.
  real(SP), dimension(:,:), allocatable :: Slice_vx  ! x-comp. of velocity
  real(SP), dimension(:,:), allocatable :: Slice_vy  ! y-comp.
  real(SP), dimension(:,:), allocatable :: Slice_vz  ! z-comp.
  real(SP), dimension(:,:), allocatable :: Slice_ps  ! Pressure
  real(SP), dimension(:,:), allocatable :: Slice_en  ! Enstrophy

  logical, save :: Initialize_done = .false.

contains


!===============
!    Private
!===============


!_______________________________________________________________private__
!
  subroutine make_single_precision_field(vel,ps)
    type(field__vector3d_),        intent(in) :: vel
    real(DP), dimension(NX,NY,NZ), intent(in) :: ps
!________________________________________________________________________
!
    integer :: slice_j = NY / 2

    type(field__vector3d_)        :: vor   ! vorticity
    real(DP), dimension(NX,NY,NZ) :: enstrophy

!>        vor = .curl.vel
!>  enstrophy = vor.dot.vor
          vor = operator_curl(vel)
    enstrophy = operator_dot_product(vor,vor)

    Slice_vx = real(    vel%x(:,slice_j,:),SP)
    Slice_vy = real(    vel%y(:,slice_j,:),SP)
    Slice_vz = real(    vel%z(:,slice_j,:),SP)
    Slice_ps = real(       ps(:,slice_j,:),SP)
    Slice_en = real(enstrophy(:,slice_j,:),SP)

    call debug__message('called slicedata/make_single_precision_field.')

  end subroutine make_single_precision_field


!==============
!    Public
!==============


!________________________________________________________________public__
!
  subroutine slicedata__initialize
!________________________________________________________________________
!
    allocate(Slice_vx(NX,NZ),   &
             Slice_vy(NX,NZ),   &
             Slice_vz(NX,NZ),   &
             Slice_ps(NX,NZ),   &
             Slice_en(NX,NZ))

    call debug__message('Slice data allocated.')

    open(FILE_SLICEDATA,                                &
         file=trim(namelist__string('Slicedata_tag')),  &
         form='unformatted')

    Initialize_done = .true.

    call debug__message('called slicedata__initlilize')

  end subroutine slicedata__initialize


!________________________________________________________________public__
!
  subroutine slicedata__write(nloop,time,fluid)
    integer,             intent(in) :: nloop
    real(DP),            intent(in) :: time
    type(field__fluid_), intent(in) :: fluid
!________________________________________________________________________
!
    type(field__vector3d_) :: vel

    if ( namelist__integer('Slicedata_nskip') <= 0 ) return
                                      ! Set zero or negative integer
                                      ! when you don't want to
                                      ! save any slice data.

    if ( mod(nloop,namelist__integer('Slicedata_nskip')) /= 0 ) return


    call ut__assert(Initialize_done,"<slicedata__write> Forgot init?")

    call solver__get_subfield(fluid,vel)

    call make_single_precision_field(vel,fluid%pressure)

    write(FILE_SLICEDATA) nloop, real(time,SP),                 &
                          Slice_vx, Slice_vy, Slice_vz,         &
                          Slice_ps, Slice_en

    call ut__message('#slice data saved at ', nloop, time)

    call debug__message('called slicedata__write.')

  end subroutine slicedata__write

end module slicedata_m
