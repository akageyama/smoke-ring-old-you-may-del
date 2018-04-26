!-----------------------------------------------------------------------------
! 200708_LesHouches/src/BurgersEquation
!                                       Akira Kageyama
!                                       kage@jamstec.go.jp
!                                       Earth Simulator Center, JAMSTEC, Japan
!-----------------------------------------------------------------------------
! main.f90
!              2007.07.20: Created by Akira Kageyama
!-----------------------------------------------------------------------------
program main                !
  use constants             !                              numerical constants
  use ut                    !                                utility functions
  use namelist              !                                  namelist loader
  use rk4                   !               4th runge-kutta integration method
  implicit none             !-------------------------------------------------

  real(DP), dimension(:), allocatable :: xpos   ! grid position, size=nx
  real(DP), dimension(:), allocatable :: psi    ! size=nx
  real(DP), dimension(:), allocatable :: dpsi01, dpsi02,  &   ! used for RK4.
                                         dpsi03, dpsi04
  integer :: i, nx, nloop
  integer :: nloop_max = 1000
  real(DP) :: dx, dt, time, x
  real(DP), parameter :: ONE_SIXTH = 1.0_DP / 6.0_DP


  !    1   2   3   4                                              NX-1 NX
  !    !---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!
  !        !                                                           !
  !        !                                                           !
  !       -PI<---------------------- TWOPI -------------------------->+PI
  !        !                                                           !
  !        !                                                           !
  !  --!---!                                                       !---!--
  !  NX-1  NX                                                      1   2   


  call namelist__read

  nx = namelist__integer('Nx')
  dx = TWOPI / (nx-2)
  dt = dx**2 / namelist__double('Diffusion_coeff')     &
             * namelist__double('Cfl_factor')

  allocate(xpos(nx),psi(nx))
  allocate(dpsi01(nx))
  allocate(dpsi02(nx))
  allocate(dpsi03(nx))
  allocate(dpsi04(nx))

  xpos(:) = 0.0_DP
  psi(:)  = 0.0_DP
  dpsi01(:) = 0.0_DP
  dpsi02(:) = 0.0_DP
  dpsi03(:) = 0.0_DP
  dpsi04(:) = 0.0_DP
  
  do i = 1 , nx
     xpos(i) = -PI + dx*(i-2)    ! Grid location
  end do

  do i = 1 , nx
     x = xpos(i)
     psi(i) = 0.8+0.2*cos(x)     ! Initial condition
  end do

  call ut__message('initial check:    nx = ', nx)
  call ut__message('initial check:    dx = ', dx)

  time = 0.0_DP

  call iSave     ! Save the initial condition profile on the disk.
  
  do nloop = 1 , nloop_max
     !--< Runge-Kutta step 1 >--!
     dpsi01(:) = rk4__step('1st',dt,dx,psi)
     call iBoundary_condition(dpsi01)

     !--< Runge-Kutta step 2 >--!
     dpsi02(:) = rk4__step('2nd',dt,dx,psi,dpsi01)
     call iBoundary_condition(dpsi02)

     !--< Runge-Kutta step 3 >--!
     dpsi03(:) = rk4__step('3rd',dt,dx,psi,dpsi02)
     call iBoundary_condition(dpsi03)

     !--< Runge-Kutta step 4 >--!
     dpsi04(:) = rk4__step('4th',dt,dx,psi,dpsi03)
     call iBoundary_condition(dpsi04)
     
     time = time + dt
     psi(:) = psi(:) + ONE_SIXTH*(dpsi01(:)      &
                               +2*dpsi02(:)      &
                               +2*dpsi03(:)      &
                                 +dpsi04(:))

     if ( mod(nloop,4)==0 ) then
        call iSave                      ! Save the profile on the disk.
     end if
  end do

contains

  subroutine iBoundary_condition(psi)
    real(DP), dimension(nx), intent(inout) :: psi

    psi(1)    = psi(nx-1)
    psi(nx)   = psi(2)
  end subroutine iBoundary_condition
  
  subroutine iSave
    integer, save :: counter = 0 
    integer :: i

    open(10,file=trim(namelist__string('Filename'))//'.'//ut__i2c3(counter))
    do i = 1 , nx
       write(10,*) xpos(i), psi(i)
    end do
    close(10)

    counter = counter + 1

    call ut__message(" Data saved at nloop, time = ", nloop, time)

  end subroutine iSave

end program main
