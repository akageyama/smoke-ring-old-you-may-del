!-----------------------------------------------------------------------------
! 200708_LesHouches/src/DiffusionEquation
!                                       Akira Kageyama
!                                       kage@jamstec.go.jp
!                                       Earth Simulator Center, JAMSTEC, Japan
!-----------------------------------------------------------------------------
! constants.f90
!              2007.07.20: Created by Akira Kageyama
!-----------------------------------------------------------------------------

module constants      
  implicit none

  ! << f90 constants >>
  integer, parameter ::  SP = kind(1.0)
  integer, parameter ::  DP = selected_real_kind(2*precision(1.0_SP))
  integer, parameter :: DPC = kind((1.0_DP,1.0_DP))

  ! << Mathematical constants >>
  real(DP),     parameter ::    PI = 3.141592653589793238462643383279502884_DP
  real(DP),     parameter :: TWOPI = PI*2
  complex(DPC), parameter :: Z0 = (0.0_DP,0.0_DP)
  complex(DPC), parameter :: ZI = (0.0_DP,1.0_DP)

  ! << I/O files >>
  integer, parameter      :: FILE_NAMELIST     = 05
  integer, parameter      :: FILE_STANDARD_OUT = 06

end module constants
