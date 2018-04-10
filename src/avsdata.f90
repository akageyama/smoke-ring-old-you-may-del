!-----------------------------------------------------------------------------
! FLUIDBOX: 3D Fluid Solver by FDM in Cartesian Geometry.
!
!     This code is developed for Simulation School at Kobe on 2008.06.09.
!
!                                           Akira Kageyama, kage@jamstec.go.jp
!                                              Earth Simulator Center, JAMSTEC
!-----------------------------------------------------------------------------
! avsdata.f90
!     2008.06.04: gavs ==> avsdatalib
!     2008.06.03: Developed by Akira Kageyama. Copied from kindanb.
!-----------------------------------------------------------------------------

module avsdata
!*****************************************************************************
! MODULE AVSDATA                           Make and Save 3D Data in AVS Format
!*****************************************************************************
  use grid
  use ut
  use debug
  use field
  use solver
  use avsdatalib
  use namelist
  implicit none

  private
  public :: avsdata__initialize,  &
            avsdata__write

  integer :: Avs_nx  ! mesh size of the avs data
  integer :: Avs_ny
  integer :: Avs_nz

  ! - 3D single precision real arrays.
  real(SP), dimension(:,:,:), allocatable :: Avs_vx  ! x-comp. of velocity
  real(SP), dimension(:,:,:), allocatable :: Avs_vy  ! y-comp.
  real(SP), dimension(:,:,:), allocatable :: Avs_vz  ! z-comp.
  real(SP), dimension(:,:,:), allocatable :: Avs_ps  ! Pressure
  real(SP), dimension(:,:,:), allocatable :: Avs_en  ! Enstrophy

  !--- Coordinates information for AVS field data ---
  real(SP), dimension(:,:,:), allocatable :: Avs_coord_x
  real(SP), dimension(:,:,:), allocatable :: Avs_coord_y
  real(SP), dimension(:,:,:), allocatable :: Avs_coord_z

  logical, save :: Initialize_done = .false.


contains


!===============
!    Private
!===============


!_______________________________________________________________private__
!                                                                        !
  subroutine make_single_precision_field(vel,ps)                         !
    type(field__vector3d_),        intent(in) :: vel                     !
    real(DP), dimension(NX,NY,NZ), intent(in) :: ps                      !
!________________________________________________________________________!
!
    type(field__vector3d_)        :: vor   ! vorticity
    real(DP), dimension(NX,NY,NZ) :: enstrophy

          vor = .curl.vel
    enstrophy = vor.dot.vor

    Avs_vx = real(    vel%x,SP)
    Avs_vy = real(    vel%y,SP)
    Avs_vz = real(    vel%z,SP)
    Avs_ps = real(       ps,SP)
    Avs_en = real(enstrophy,SP)

    call debug__message('called avsdata/make_single_precision_field.')

  end subroutine make_single_precision_field


!_______________________________________________________________private__
!                                                                        !
  subroutine output(nth_call)                                            !
    character(len=3), intent(in) :: nth_call                             !
!________________________________________________________________________!
!
    logical :: firsttime = .true.              ! Automatic save attribute.

    if (firsttime) then                        ! Save coordinate data
       call avsdatalib__coord(FILE_AVS_DATA,                               &
                              trim(namelist__string('Avs_tag')),           &
                              Avs_nx, Avs_ny, Avs_nz,                      &
                              Avs_coord_x,                                 &
                              Avs_coord_y,                                 &
                              Avs_coord_z)
       firsttime = .false.
    end if

    call avsdatalib__fld(FILE_AVS_DATA,                                    &
                         trim(namelist__string('Avs_tag')),                &
                         trim(namelist__string('Avs_tag'))//'.'//nth_call, &
                         Avs_nx, Avs_ny, Avs_nz,                           &
                         Avs_vx, 'vx',                                     &
                         Avs_vy, 'vy',                                     &
                         Avs_vz, 'vz',                                     &
                         Avs_ps, 'pressure',                               &
                         Avs_en, 'enstrophy')

    call debug__message('called avsdata/output.')

  end subroutine output


!==============
!    Public
!==============


!________________________________________________________________public__
!                                                                        !
  subroutine avsdata__initialize                                         !
!________________________________________________________________________!
!
!  We define the AVS coordinates as the so-called 'irregular',
!  since my module 'gavs' accepts only this grid system.
!________________________________________________________________________/
!
    integer  :: i, j, k

    Avs_nx = NX    ! You may want to reduce the output data size here.
    Avs_ny = NY
    Avs_nz = NZ

    call iMalloc(Avs_nx,Avs_ny,Avs_nz)

    call debug__message('AVS data allocated.')

    do k = 1 , Avs_nz
       do j = 1 , Avs_ny
          do i = 1 , Avs_nx
             Avs_coord_x(i,j,k) = real(grid__pos%x(i),SP)
             Avs_coord_y(i,j,k) = real(grid__pos%y(j),SP)
             Avs_coord_z(i,j,k) = real(grid__pos%z(k),SP)
          end do
       end do
    end do

    Initialize_done = .true.

    call debug__message('called avsdata__initialize.')

  contains

  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -!
    subroutine iMalloc(nx,ny,nz)
      integer, intent(in) :: nx, ny, nz
  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -!

      allocate(Avs_vx(nx,ny,nz),         &
               Avs_vy(nx,ny,nz),         &
               Avs_vz(nx,ny,nz),         &
               Avs_ps(nx,ny,nz),         &
               Avs_en(nx,ny,nz))

      allocate(Avs_coord_x(nx,ny,nz),    &
               Avs_coord_y(nx,ny,nz),    &
               Avs_coord_z(nx,ny,nz))

    end subroutine iMalloc

  end subroutine avsdata__initialize


!________________________________________________________________public__
!                                                                        !
  subroutine avsdata__write(nloop,time,fluid)                            !
    integer,             intent(in) :: nloop                             !
    real(DP),            intent(in) :: time                              !
    type(field__fluid_), intent(in) :: fluid                             !
!________________________________________________________________________!
!
    type(field__vector3d_) :: vel

    integer :: counter = 0            ! it has automatic save attribute.

    if ( namelist__integer('Avs_nskip') <= 0 ) return    
                                      ! Set zero or negative integer
                                      ! when you don't want to
                                      ! save any AVS data.

    if ( mod(nloop,namelist__integer('Avs_nskip')) /= 0 ) return

    call ut__assert(Initialize_done,"<avsdata__write> Forgot init?")

    call solver__get_subfield(fluid,vel)
    call make_single_precision_field(vel,fluid%pressure)

    call output(ut__i2c3(counter))

    call ut__message('#avs data saved: '//ut__i2c3(counter), nloop, time)

    counter = counter + 1

    call debug__message("called avsdata__write.")

  end subroutine avsdata__write

end module avsdata
