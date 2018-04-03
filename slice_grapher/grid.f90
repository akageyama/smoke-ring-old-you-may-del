!-----------------------------------------------------------------------------
! grid.f90
!     2008.06.02: Created by Akira Kageyama. Copied from ../code.
!-----------------------------------------------------------------------------

module grid
!*****************************************************************************
! MODULE GRID                                                        Grid Mesh
!*****************************************************************************
  use constants
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
            grid__d2
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


!_______________________________________________________________prublic__
!                                                                        !
  subroutine grid__initialize                                            !
!________________________________________________________________________!
!
    integer  :: i, j, k
    real(DP) :: dx, dy, dz

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

    dx = (XMAX-XMIN)/(NX-2)  ! from 1.5 to (NX-0.5)
    dy = (YMAX-YMIN)/(NY-2)
    dz = (ZMAX-ZMIN)/(NZ-2)

    grid__delta%x = dx
    grid__delta%y = dy
    grid__delta%z = dz

    grid__d1%x = 1.0_DP/(2*dx)   ! factor for 1st derivative
    grid__d1%y = 1.0_DP/(2*dy)
    grid__d1%z = 1.0_DP/(2*dz)

    grid__d2%x = 1.0_DP/(dx**2)  ! factor for 2nd derivative
    grid__d2%y = 1.0_DP/(dy**2)
    grid__d2%z = 1.0_DP/(dz**2)

    do i = 1 , NX
       grid__pos%x(i) = XMIN + dx*(real(i,DP)-1.5_DP)
    end do

    do j = 1 , NY
       grid__pos%y(j) = YMIN + dy*(real(j,DP)-1.5_DP)
    end do

    do k = 1 , NZ
       grid__pos%z(k) = ZMIN + dz*(real(k,DP)-1.5_DP)
    end do

  end subroutine grid__initialize


end module grid

