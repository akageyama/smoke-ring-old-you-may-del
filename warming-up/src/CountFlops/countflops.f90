!-----------------------------------------------------------------------------
! 200708_LesHouches/src/CountFlops
!                                       Akira Kageyama
!                                       kage@jamstec.go.jp
!                                       Earth Simulator Center, JAMSTEC, Japan
!-----------------------------------------------------------------------------
!
! countflops.f90
!
!      To estimate flops on this computer:
!                       by: Akira Kageyama
!                       on: 2007.08.13
!                      for: Les Houches Summer School "Dynamos".
!-----------------------------------------------------------------------------
!

program countflops
  implicit none
  integer, parameter :: SP = kind(1.0)
  integer, parameter :: DP = selected_real_kind(2*precision(1.0_SP))
  real(DP), parameter :: PI = 3.14159265358979_DP
  real(DP) :: a
  integer :: i

  do i = 1 , 3*(10**6)
     a = PI*PI
  end do

  print *, ' a = ', a

end program countflops
