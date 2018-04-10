!-----------------------------------------------------------------------------
! FLUIDBOX: 3D Fluid Solver by FDM in Cartesian Geometry.
!
!     This code is developed for Simulation School at Kobe on 2008.06.09.
!
!                                           Akira Kageyama, kage@jamstec.go.jp
!                                              Earth Simulator Center, JAMSTEC
!-----------------------------------------------------------------------------
! main.f90
!     2008.06.02: Developed by Akira Kageyama. kage@jamstec.go.jp.
!-----------------------------------------------------------------------------

program main
!=============================================================================
! PROGAM MAIN                 FLUIDBOX: A simple sample 3D fluid solver by FDM
!=============================================================================
  !                                        Akira Kageyama (kage@jamstec.go.jp)
  !---------------------------------------------------------------------------
  use constants             !                              numerical constants
  use ut                    !                                utility functions
  use namelist              !                                   namelis loader
  use debug                 !                                    for debugging
  use grid                  !                                        grid mesh
  use field                 !                   field operators and operations
  use avsdatalib            !            a generator of avs-field data (*.fld)
  use avsdata               !              make and save 3D data in avs format
  use slicedata             !                         generate 2-d sliced data
  use solver                !         4th order runge-kutta integration method
  implicit none             !-------------------------------------------------

  integer  :: nloop, karte=KARTE_FINE
  real(DP) :: dt, time

  type(field__fluid_) :: fluid

  call namelist__read
  call grid__initialize
  call solver__initialize(fluid)
  call avsdata__initialize
  call slicedata__initialize

  time = 0.0_DP
  nloop = 0

  call solver__diagnosis(nloop,time,fluid,karte)

  dt = solver__set_time_step(nloop,fluid)

  do while(karte==KARTE_FINE)
                               ; call debug__message("running. nloop = ", nloop)
     call solver__advance(time,dt,fluid)
     dt = solver__set_time_step(nloop,fluid)
     nloop = nloop + 1
     if (nloop >= namelist__integer('Total_nloop')) karte = KARTE_LOOP_MAX
     call solver__diagnosis(nloop,time,fluid,karte)
     call avsdata__write(nloop,time,fluid)
     call slicedata__write(nloop,time,fluid)
  end do

  select case (karte)
  case (KARTE_FINE)
     call ut__message('#',"Successfully finished.")
  case (KARTE_LOOP_MAX)
     call ut__message('=',"Reached max nloop = ", nloop)
  case (KARTE_TIME_OUT)
     call ut__message('-',"Time out at nloop = ", nloop)
  case (KARTE_OVERFLOW)
     call ut__message('%',"Overflow at nloop = ", nloop)
  case (KARTE_UNDERFLOW)
     call ut__message('%',"Underflow at nloop = ",nloop)
  case default
     call ut__message('?',"Stopped at nloop = ",  nloop)
  end select

end program main
