!-----------------------------------------------------------------------------
! smoke-ring: A simple 3-D Fluid Solver by FDM on Cartesian Grid.
!
!    by Akira Kageyama,
!       Department of Computational Science,
!       Kobe University, Japan.
!       email: kage@port.kobe-u.ac.jp or sgks@mac.com
!-----------------------------------------------------------------------------
!
!  - A simple CFD code for educational purposes.
!
!  - It solves compressible Navier-Stokes equations for an ideal gas under
!    the periodic boundary conditions in all (three) directions.
!
!  - In the original setting, an external force is applied in a local
!    region near an end of the box to drive the fluid to flow towrard the
!    other end of the box. The well-known smoke ring will be formed.
!
!-----------------------------------------------------------------------------
!
! Old comments follows:
!
!!-----------------------------------------------------------------------------
!! FLUIDBOX: 3D Fluid Solver by FDM in Cartesian Geometry.
!!
!!     This code is developed for Simulation School at Kobe on 2008.06.09.
!!
!!                                           Akira Kageyama, kage@jamstec.go.jp
!!                                              Earth Simulator Center, JAMSTEC
!!-----------------------------------------------------------------------------
!! main.f90
!!     2008.06.02: Developed by Akira Kageyama. kage@jamstec.go.jp.
!!-----------------------------------------------------------------------------

program main_m
!=============================================================================
! PROGAM MAIN                BOX-FLUID: A simple sample 3D fluid solver by FDM
!=============================================================================
  !                                    Akira Kageyama (kage@port.kobe-u.ac.jp)
  !---------------------------------------------------------------------------
  use constants_m             !                              numerical constants
  use ut_m                    !                                utility functions
  use namelist_m              !                                   namelis loader
  use debug_m                 !                                    for debugging
  use grid_m                  !                                        grid mesh
  use field_m                 !                   field operators and operations
  use vis_m                   !                            visualize data by kvs
  use slicedata_m             !                         generate 2-d sliced data
  use solver_m                !         4th order runge-kutta integration method
  implicit none               !-------------------------------------------------

  integer  :: nloop, karte=KARTE_FINE
  real(DP) :: dt, time

  type(field__fluid_) :: fluid

  call namelist__read
  call grid__initialize
  call solver__initialize(fluid)
  call vis__initialize
  call slicedata__initialize

  time = 0.0_DP
  nloop = 0

  call solver__diagnosis(nloop,time,fluid,karte)

  dt = solver__set_time_step(nloop,fluid)

  do while(karte==KARTE_FINE)  ; call debug__message("running. nloop=",nloop)
     call solver__advance(time,dt,fluid)
     dt = solver__set_time_step(nloop,fluid)
     nloop = nloop + 1
     if (nloop >= namelist__integer('Total_nloop')) karte = KARTE_LOOP_MAX
     call solver__diagnosis(nloop,time,fluid,karte)
     call vis__apply(nloop,time,fluid)
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

end program main_m
