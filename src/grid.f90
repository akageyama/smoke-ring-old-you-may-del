!-----------------------------------------------------------------------------
! smoke-ring: A simple 3-D Fluid Solver by FDM on Cartesian Grid.
!
!    by Akira Kageyama,
!       Department of Computational Science,
!       Kobe University, Japan.
!       email: kage@port.kobe-u.ac.jp or sgks@mac.com
!-----------------------------------------------------------------------------
! grid.f90
!     2008.06.02: Developed by Akira Kageyama. Copied from kindanb.
!     2018.04.12: Copied from boxfluid.
!-----------------------------------------------------------------------------

module grid_m
!*****************************************************************************
! MODULE GRID                                                        Grid Mesh
!*****************************************************************************
  use constants_m
  use ut_m
  use debug_m
  implicit none
  private
  public :: & !<< type >>!
            grid__pos_,                         &
            grid__delta_,                       &
            grid__derivative_operator_1st_,     &
            grid__derivative_operator_2nd_
  public :: & !<< variable >>!
            grid__pos,                          &
            grid__delta,                        &
            grid__d1,                           &
            grid__d2,                           &
            grid__delta_min
  public :: & !<< routine >>!
            grid__initialize

  type grid__pos_
     real(DP), dimension(NX) :: x
     real(DP), dimension(NY) :: y
     real(DP), dimension(NZ) :: z
  end type grid__pos_

  type(grid__pos_) :: grid__pos

  type grid__delta_
     real(DP) :: x, y, z
  end type grid__delta_

  real(DP) :: grid__delta_min

  type(grid__delta_) :: grid__delta

  type grid__derivative_operator_1st_
     real(DP) :: x, y, z
  end type grid__derivative_operator_1st_

  type(grid__derivative_operator_1st_) :: grid__d1

  type grid__derivative_operator_2nd_
     real(DP) :: x, y, z
  end type grid__derivative_operator_2nd_

  type(grid__derivative_operator_2nd_) :: grid__d2


contains


!===============
!    Private
!===============


!==============
!    Public
!==============


!________________________________________________________________public__
!
  subroutine grid__initialize
!________________________________________________________________________
!
!   Here we suppose the periodic boundary condition in all directions.
!________________________________________________________________________/
!
    integer  :: i, j, k
    real(DP) :: dx, dy, dz

    real(DP), parameter :: NEARLY_ZERO = 1.e-10_DP

    !___________________________________________________________________
    !
    ! Periodic boundary condition.
    !
    !    --+-----+-----|                             |-----+-----+---
    !      6     7     8                             1     2     3
    !            |-----+-----+-----+-----+-----+-----+-----|
    !            1     2     3     4     5     6     7     8
    !               |===================================|
    !              XMIN                                XMAX
    !__________________________________________________________________/
    !

    dx = (XMAX-XMIN)/(NX-2)  ! from (1.5) to (NX-0.5), see above figure.
    dy = (YMAX-YMIN)/(NY-2)
    dz = (ZMAX-ZMIN)/(NZ-2)

    grid__delta%x = dx
    grid__delta%y = dy
    grid__delta%z = dz
    call debug__message('grid__delta%x = ', grid__delta%x)
    call debug__message('grid__delta%y = ', grid__delta%y)
    call debug__message('grid__delta%z = ', grid__delta%z)

    grid__delta_min = min(grid__delta%x,grid__delta%y,grid__delta%z)
    call debug__message('grid__delta_min = ', grid__delta_min)

    grid__d1%x = 1.0_DP/(2*dx)   ! factor for 1st derivative
    grid__d1%y = 1.0_DP/(2*dy)
    grid__d1%z = 1.0_DP/(2*dz)

    grid__d2%x = 1.0_DP/(dx**2)  ! factor for 2nd derivative
    grid__d2%y = 1.0_DP/(dy**2)
    grid__d2%z = 1.0_DP/(dz**2)

    do i = 1 , NX
       grid__pos%x(i) = XMIN + dx*(real(i,DP)-1.5_DP)
       call debug__message('grid_x: i, x = ', i, grid__pos%x(i))
    end do

    do j = 1 , NY
       grid__pos%y(j) = YMIN + dy*(real(j,DP)-1.5_DP)
       call debug__message('grid_y: j, y = ', j, grid__pos%y(j))
    end do

    do k = 1 , NZ
       grid__pos%z(k) = ZMIN + dz*(real(k,DP)-1.5_DP)
       call debug__message('grid_z: k, z = ', k, grid__pos%z(k))
    end do

    call ut__assert(abs(grid__pos%x( 1)-XMIN+dx/2) +                 &
                    abs(grid__pos%x(NX)-XMAX-dx/2) +                 &
                    abs(grid__pos%y( 1)-YMIN+dy/2) +                 &
                    abs(grid__pos%y(NY)-YMAX-dy/2) +                 &
                    abs(grid__pos%z( 1)-ZMIN+dz/2) +                 &
                    abs(grid__pos%z(NZ)-ZMAX-dz/2)  < NEARLY_ZERO,   &
                   "<grid__initialize> grid min/max inconsistent.")

    call debug__message('called grid__initialize.')

  end subroutine grid__initialize

end module grid_m

