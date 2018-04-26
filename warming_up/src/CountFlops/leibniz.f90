!-----------------------------------------------------------------------------
! 200708_LesHouches/src/CountFlops
!                                       Akira Kageyama
!                                       kage@jamstec.go.jp
!                                       Earth Simulator Center, JAMSTEC, Japan
!-----------------------------------------------------------------------------
!
! leibniz.f90
!
!      Calculate pi by the Leibniz formula.
!                       by: Akira Kageyama
!                       on: 2007.07.23
!                      for: Les Houches Summer School "Dynamos".
!-----------------------------------------------------------------------------
!

program leibniz
  implicit none
  integer, parameter :: SP = kind(1.0)
  integer, parameter :: DP = selected_real_kind(2*precision(1.0_SP))
  real(DP) :: sign = -1.0_DP
  real(DP) :: sum  = 0.0_DP
  integer  :: odd_int

  do odd_int =  2*(10**6)+1 , 1 , -2
     sign = sign * (-1.0_DP)
     sum = sum + sign/odd_int
  end do

  print *, ' 4*sum = ', 4*sum

end program leibniz
