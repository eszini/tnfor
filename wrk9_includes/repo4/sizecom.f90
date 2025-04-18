!     ******************************************************************
!     Sizecom.obj
!     
!     Created: 05/22/2024 
!     Original Author : Mark S GErber as .MON file
!     Migration Author: Pablo A. Bilbao
!     ******************************************************************
      module sizecom
	   implicit none
       CHARACTER (len=136) :: SCREEN_MESSAGES
       INTEGER (KIND=4), PARAMETER :: ALL_VERSIONS=0
       INTEGER (KIND=4), PARAMETER :: LF77_VERSION=1
       INTEGER (KIND=4), PARAMETER :: LF95_VERSION=2
       INTEGER (KIND=4), PARAMETER :: WIN95_VERSION=3
      end module sizecom
