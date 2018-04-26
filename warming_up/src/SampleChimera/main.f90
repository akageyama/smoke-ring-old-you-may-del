!-----------------------------------------------------------------------------
! 200708_LesHouches/src/SampleChimera
!                                       Akira Kageyama
!                                       kage@jamstec.go.jp
!                                       Earth Simulator Center, JAMSTEC, Japan
!-----------------------------------------------------------------------------
! main.f90
!              2007.07.22: Created by Akira Kageyama
!-----------------------------------------------------------------------------
program main                !
  use constants             !                              numerical constants
  use ut                    !                                utility functions
  use namelist              !                                  namelist loader
  use rk4                   !               4th runge-kutta integration method
  implicit none             !-------------------------------------------------

  real(DP), dimension(:), allocatable :: xpos_left   ! grid position
  real(DP), dimension(:), allocatable :: xpos_rite   ! grid position
  real(DP), dimension(:), allocatable :: psi_left    
  real(DP), dimension(:), allocatable :: psi_rite
  real(DP), dimension(:), allocatable :: dpsi01_left, dpsi02_left,  &
                                         dpsi03_left, dpsi04_left
  real(DP), dimension(:), allocatable :: dpsi01_rite, dpsi02_rite,  &
                                         dpsi03_rite, dpsi04_rite
  integer :: i, nx_left, nx_rite, nloop
  integer :: nloop_max = 1000
  real(DP) :: dx_left, dx_rite, dt, time, x
  real(DP), parameter :: ONE_SIXTH = 1.0_DP / 6.0_DP


  !     
  !        1   2   3   4                  NX_l           1   2   3   4...
  !        |   |   |   |                   |             |   |   |   |
  !        L---L---L---L--- ---L---L---L---L             L---L---L---L---
  !            |               |       |                     |       |   
  !  --R-------R-------R       R-------R-------R-----  ------R-------R
  !    |       |       |       |       |       |             |       |   
  !  NX_r-2  NX_r-1   NX_r     1       2       3           NX_r-1  NX_r
  !            |                       |                     |
  !           -PI                      0                    +PI


  call namelist__read

  nx_rite = namelist__integer('Nx_right')
  nx_left = 2*nx_rite-3
  dx_left = PI / (nx_left-3)
  dx_rite = PI / (nx_rite-3)
  dt = min(dx_left,dx_rite)**2   &  ! though these are the same now,
             / namelist__double('Diffusion_coeff')     &
             * namelist__double('Cfl_factor')

  allocate(xpos_left(nx_left),xpos_rite(nx_rite))
  allocate(psi_left(nx_left),psi_rite(nx_rite))
  allocate(dpsi01_left(nx_left),dpsi01_rite(nx_rite))
  allocate(dpsi02_left(nx_left),dpsi02_rite(nx_rite))
  allocate(dpsi03_left(nx_left),dpsi03_rite(nx_rite))
  allocate(dpsi04_left(nx_left),dpsi04_rite(nx_rite))

  xpos_left(:) = 0.0_DP
  xpos_rite(:) = 0.0_DP

  psi_left(:)  = 0.0_DP
  psi_rite(:)  = 0.0_DP
  
  do i = 1 , nx_left
     xpos_left(i) = -PI + dx_left*(i-2)    ! Grid location
  end do
  
  do i = 1 , nx_rite
     xpos_rite(i) = dx_rite*(i-2)       ! Grid location
  end do

  do i = 1 , nx_left
     x = xpos_left(i)
     psi_left(i) = 0.8+0.2*cos(x)     ! Initial condition
  end do

  do i = 1 , nx_rite
     x = xpos_rite(i)
     psi_rite(i) = 0.8+0.2*cos(x)     ! Initial condition
  end do

  time = 0.0_DP

  call iSave     ! Save the initial condition profile on the disk.
  
  do nloop = 1 , nloop_max
     !--< Runge-Kutta step 1 >--!
     dpsi01_left(:) = rk4__step('1st',dt,dx_left,psi_left)
     dpsi01_rite(:) = rk4__step('1st',dt,dx_rite,psi_rite)
     call iBoundary_condition(dpsi01_left,dpsi01_rite)

     !--< Runge-Kutta step 2 >--!
     dpsi02_left(:) = rk4__step('2nd',dt,dx_left,psi_left,dpsi01_left)
     dpsi02_rite(:) = rk4__step('2nd',dt,dx_rite,psi_rite,dpsi01_rite)
     call iBoundary_condition(dpsi02_left,dpsi02_rite)

     !--< Runge-Kutta step 3 >--!
     dpsi03_left(:) = rk4__step('3rd',dt,dx_left,psi_left,dpsi02_left)
     dpsi03_rite(:) = rk4__step('3rd',dt,dx_rite,psi_rite,dpsi02_rite)
     call iBoundary_condition(dpsi03_left,dpsi03_rite)

     !--< Runge-Kutta step 4 >--!
     dpsi04_left(:) = rk4__step('4th',dt,dx_left,psi_left,dpsi03_left)
     dpsi04_rite(:) = rk4__step('4th',dt,dx_rite,psi_rite,dpsi03_rite)
     call iBoundary_condition(dpsi04_left,dpsi04_rite)
     
     time = time + dt

     psi_left(:) = psi_left(:) + ONE_SIXTH*(dpsi01_left(:)      &
                                         +2*dpsi02_left(:)      &
                                         +2*dpsi03_left(:)      &
                                           +dpsi04_left(:))
     psi_rite(:) = psi_rite(:) + ONE_SIXTH*(dpsi01_rite(:)      &
                                         +2*dpsi02_rite(:)      &
                                         +2*dpsi03_rite(:)      &
                                           +dpsi04_rite(:))

     if ( mod(nloop,4)==0 ) then
        call iSave                      ! Save the profile on the disk.
     end if
  end do

contains

  subroutine iBoundary_condition(psi_left,psi_rite)
    real(DP), dimension(nx_left), intent(inout) :: psi_left
    real(DP), dimension(nx_rite), intent(inout) :: psi_rite

    psi_left(1)         = (psi_rite(nx_rite-2)+psi_rite(nx_rite-1))*0.5_DP
    psi_left(nx_left)   = (psi_rite(2)+psi_rite(3))*0.5_DP
    psi_rite(1)        = psi_left(nx_left-3)
    psi_rite(nx_rite) = psi_left(4)
  end subroutine iBoundary_condition
  
  subroutine iSave
    integer, save :: counter = 0 
    integer :: i

    open(10,file=trim(namelist__string('Filename'))//'.'//ut__i2c3(counter))
    do i = 1 , nx_left
       write(10,*) xpos_left(i), psi_left(i)
    end do
    do i = 1 , nx_rite
       write(10,*) xpos_rite(i), psi_rite(i)
    end do
    close(10)

    counter = counter + 1

    call ut__message(" Data saved at nloop, time = ", nloop, time)

  end subroutine iSave

end program main
