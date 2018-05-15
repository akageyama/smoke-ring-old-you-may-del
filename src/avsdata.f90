!-----------------------------------------------------------------------------
! smoke-ring: A simple 3-D Fluid Solver by FDM on Cartesian Grid.
!
!    by Akira Kageyama,
!       Department of Computational Science,
!       Kobe University, Japan.
!       email: kage@port.kobe-u.ac.jp or sgks@mac.com
!-----------------------------------------------------------------------------
! avsdata.f90
!     2008.06.04: gavs ==> avsdatalib
!     2008.06.03: Developed by Akira Kageyama. Copied from kindanb.
!     2018.04.12: Copied from boxfluid.
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
  use iso_c_binding
  implicit none

  private
  public :: avsdata__initialize,  &
            avsdata__write
  
  integer :: Avs_nx  ! mesh size of the avs data
  integer :: Avs_ny
  integer :: Avs_nz
  integer :: grid_size
  real(SP) :: isolevel

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

  interface
     subroutine vis_Isosurface( values, size, dimx, dimy, dimz, visstep, isolevel  )&
          bind( c, name = "Isosurface" )
       import
       integer(c_int), value :: size
       real(c_float)         :: values(size)
       integer(c_int), value :: dimx
       integer(c_int), value :: dimy
       integer(c_int), value :: dimz
       integer(c_int), value :: visstep
       real(c_float),  value :: isolevel
     end subroutine vis_Isosurface
     
     subroutine vis_SlicePlane( values, size, dimx, dimy, dimz, visstep )&
          bind( c, name = "SlicePlane" )
       import
       integer(c_int), value :: size
       real(c_float)         :: values(size)
       integer(c_int), value :: dimx
       integer(c_int), value :: dimy
       integer(c_int), value :: dimz
       integer(c_int), value :: visstep            
     end subroutine vis_SlicePlane

     subroutine vis_RayCasting( values, size, dimx, dimy, dimz, visstep )&
          bind( c, name = "RayCasting" )
       import
       integer(c_int), value :: size
       real(c_float)         :: values(size)
       integer(c_int), value :: dimx
       integer(c_int), value :: dimy
       integer(c_int), value :: dimz
       integer(c_int), value :: visstep       
     end subroutine vis_RayCasting

  end interface


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

!>        vor = .curl.vel
!>  enstrophy = vor.dot.vor
          vor = operator_curl(vel)
    enstrophy = operator_dot_product(vor,vor)

    Avs_vx = real(    vel%x,SP)
    Avs_vy = real(    vel%y,SP)
    Avs_vz = real(    vel%z,SP)
    Avs_ps = real(       ps,SP)
    Avs_en = real(enstrophy,SP)

    call debug__message('called avsdata/make_single_precision_field.')

  end subroutine make_single_precision_field


!_______________________________________________________________private__
!                                                                        !
  subroutine output(nth_call, nloop)                                            !
    character(len=3), intent(in) :: nth_call                             !
    integer,          intent(in) :: nloop                                !                  
!________________________________________________________________________!
!
    logical :: firsttime = .true.              ! Automatic save attribute.

    if (firsttime) then                        ! Save coordinate data
!       call avsdatalib__coord(FILE_AVS_DATA,                               &
!                              trim(namelist__string('Avs_tag')),           &
!                              Avs_nx, Avs_ny, Avs_nz,                      &
!                              Avs_coord_x,                                 &
!                              Avs_coord_y,                                 &
!                              Avs_coord_z)
       firsttime = .false.
    end if

!    call avsdatalib__fld(FILE_AVS_DATA,                                    &
!                         trim(namelist__string('Avs_tag')),                &
!                         trim(namelist__string('Avs_tag'))//'.'//nth_call, &
!                         Avs_nx, Avs_ny, Avs_nz,                           &
!                         Avs_vx, 'vx',                                     &
!                         Avs_vy, 'vy',                                     &
!                         Avs_vz, 'vz',                                     &
!                         Avs_ps, 'pressure',                               &
!                         Avs_en, 'enstrophy')

    call debug__message('called avsdata/output.')
    
    grid_size = Avs_nx * Avs_ny * Avs_nz
    isolevel = 0.5
    call vis_Isosurface( Avs_en, grid_size, Avs_nx, Avs_ny, Avs_nz , nloop, isolevel )
    call vis_SlicePlane( Avs_en, grid_size, Avs_nx, Avs_ny, Avs_nz , nloop )
    call vis_RayCasting( Avs_en, grid_size, Avs_nx, Avs_ny, Avs_nz , nloop )
    
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

    call output(ut__i2c3(counter), nloop)

    call ut__message('#avs data saved: '//ut__i2c3(counter), nloop, time)

    counter = counter + 1

    call debug__message("called avsdata__write.")

  end subroutine avsdata__write

end module avsdata
