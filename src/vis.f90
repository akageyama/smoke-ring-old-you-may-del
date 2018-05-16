!-----------------------------------------------------------------------------
! smoke-ring: A simple 3-D Fluid Solver by FDM on Cartesian Grid.
!
!    by Akira Kageyama,
!       Department of Computational Science,
!       Kobe University, Japan.
!       email: kage@port.kobe-u.ac.jp or sgks@mac.com
!-----------------------------------------------------------------------------
! vis.f90
!     2018.05.15: Copied from avsdata.f90.
!-----------------------------------------------------------------------------


module vis
!*****************************************************************************
! MODULE VIS                                             VISUALIZE DATA BY KVS
!*****************************************************************************
  use grid
  use ut
  use debug
  use field
  use solver
  use namelist
  use iso_c_binding
  implicit none

  private
  public :: vis__initialize,  &
            vis__write

  integer :: Kvs_nx  ! mesh size of the kvs data
  integer :: Kvs_ny
  integer :: Kvs_nz
  integer :: grid_size
  real(SP) :: isolevel

  ! - 3D single precision real arrays.
  real(SP), dimension(:,:,:), allocatable :: Kvs_vx  ! x-comp. of velocity
  real(SP), dimension(:,:,:), allocatable :: Kvs_vy  ! y-comp.
  real(SP), dimension(:,:,:), allocatable :: Kvs_vz  ! z-comp.
  real(SP), dimension(:,:,:), allocatable :: Kvs_ps  ! Pressure
  real(SP), dimension(:,:,:), allocatable :: Kvs_en  ! Enstrophy

  !--- Coordinates information for KVS field data ---
! real(SP), dimension(:,:,:), allocatable :: Kvs_coord_x
! real(SP), dimension(:,:,:), allocatable :: Kvs_coord_y
! real(SP), dimension(:,:,:), allocatable :: Kvs_coord_z

  logical, save :: Initialize_done = .false.

  interface
     subroutine vis_Isosurface(values,size,dimx,dimy,dimz,visstep,isolevel) &
                          bind(c,name="Isosurface")
       import
       integer(c_int), value :: size
       real(c_float)         :: values(size)
       integer(c_int), value :: dimx
       integer(c_int), value :: dimy
       integer(c_int), value :: dimz
       integer(c_int), value :: visstep
       real(c_float),  value :: isolevel
     end subroutine vis_Isosurface

     subroutine vis_SlicePlane(values,size,dimx,dimy,dimz,visstep) &
                          bind(c,name="SlicePlane")
       import
       integer(c_int), value :: size
       real(c_float)         :: values(size)
       integer(c_int), value :: dimx
       integer(c_int), value :: dimy
       integer(c_int), value :: dimz
       integer(c_int), value :: visstep
     end subroutine vis_SlicePlane

     subroutine vis_RayCasting(values,size,dimx,dimy,dimz,visstep) &
                          bind(c,name="RayCasting")
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
!
  subroutine make_single_precision_field(vel,ps)
    type(field__vector3d_),        intent(in) :: vel
    real(DP), dimension(NX,NY,NZ), intent(in) :: ps
!________________________________________________________________________
!
    type(field__vector3d_)        :: vor   ! vorticity
    real(DP), dimension(NX,NY,NZ) :: enstrophy

!>        vor = .curl.vel
!>  enstrophy = vor.dot.vor
          vor = operator_curl(vel)
    enstrophy = operator_dot_product(vor,vor)

    Kvs_vx = real(    vel%x,SP)
    Kvs_vy = real(    vel%y,SP)
    Kvs_vz = real(    vel%z,SP)
    Kvs_ps = real(       ps,SP)
    Kvs_en = real(enstrophy,SP)

    call debug__message('called vis/make_single_precision_field.')

  end subroutine make_single_precision_field


!_______________________________________________________________private__
!
  subroutine output(nth_call, nloop)
    character(len=3), intent(in) :: nth_call
    integer,          intent(in) :: nloop
!________________________________________________________________________
!
    logical :: firsttime = .true.              ! Automatic save attribute.

    if (firsttime) then                        ! Save coordinate data
!       call vislib__coord(FILE_AVS_DATA,                               &
!                          trim(namelist__string('Kvs_tag')),           &
!                          Kvs_nx, Kvs_ny, Kvs_nz,                      &
!                          Kvs_coord_x,                                 &
!                          Kvs_coord_y,                                 &
!                          Kvs_coord_z)
       firsttime = .false.
    end if

    call debug__message('called vis/output.')

    grid_size = Kvs_nx * Kvs_ny * Kvs_nz
    isolevel = 0.5
    call vis_Isosurface(Kvs_en,grid_size,Kvs_nx,Kvs_ny,Kvs_nz,nloop,isolevel)
    call vis_SlicePlane(Kvs_en,grid_size,Kvs_nx,Kvs_ny,Kvs_nz,nloop )
    call vis_RayCasting(Kvs_en,grid_size,Kvs_nx,Kvs_ny,Kvs_nz,nloop )

  end subroutine output


!==============
!    Public
!==============


!________________________________________________________________public__
!
  subroutine vis__initialize
!________________________________________________________________________
!
!  We define the AVS coordinates as the so-called 'irregular',
!  since my module 'gavs' accepts only this grid system.
!________________________________________________________________________/
!
    integer  :: i, j, k

    Kvs_nx = NX    ! You may want to reduce the output data size here.
    Kvs_ny = NY
    Kvs_nz = NZ

    call iMalloc(Kvs_nx,Kvs_ny,Kvs_nz)

    call debug__message('AVS data allocated.')

!   do k = 1 , Kvs_nz
!      do j = 1 , Kvs_ny
!         do i = 1 , Kvs_nx
!            Kvs_coord_x(i,j,k) = real(grid__pos%x(i),SP)
!            Kvs_coord_y(i,j,k) = real(grid__pos%y(j),SP)
!            Kvs_coord_z(i,j,k) = real(grid__pos%z(k),SP)
!         end do
!      end do
!   end do

    Initialize_done = .true.

    call debug__message('called vis__initialize.')

  contains

  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -!
    subroutine iMalloc(nx,ny,nz)
      integer, intent(in) :: nx, ny, nz
  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -!

      allocate(Kvs_vx(nx,ny,nz),         &
               Kvs_vy(nx,ny,nz),         &
               Kvs_vz(nx,ny,nz),         &
               Kvs_ps(nx,ny,nz),         &
               Kvs_en(nx,ny,nz))

!     allocate(Kvs_coord_x(nx,ny,nz),    &
!              Kvs_coord_y(nx,ny,nz),    &
!              Kvs_coord_z(nx,ny,nz))

    end subroutine iMalloc

  end subroutine vis__initialize


!________________________________________________________________public__
!
  subroutine vis__write(nloop,time,fluid)
    integer,             intent(in) :: nloop
    real(DP),            intent(in) :: time
    type(field__fluid_), intent(in) :: fluid
!________________________________________________________________________
!
    type(field__vector3d_) :: vel

    integer :: counter = 0            ! it has automatic save attribute.

    if ( namelist__integer('Kvs_nskip') <= 0 ) return
                                      ! Set zero or negative integer
                                      ! when you don't want to
                                      ! save any AVS data.

    if ( mod(nloop,namelist__integer('Kvs_nskip')) /= 0 ) return

    call ut__assert(Initialize_done,"<vis__write> Forgot init?")

    call solver__get_subfield(fluid,vel)
    call make_single_precision_field(vel,fluid%pressure)

    call output(ut__i2c3(counter), nloop)

    call ut__message('#avs data saved: '//ut__i2c3(counter), nloop, time)

    counter = counter + 1

    call debug__message("called vis__write.")

  end subroutine vis__write

end module vis
