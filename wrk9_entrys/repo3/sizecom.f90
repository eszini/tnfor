!     ******************************************************************
!     Sizecom.mon
!     Copyright(c) Microsoft 2000
!
!     Created: 3/6/2011 2:07:09 PM
!     Author : Mark S GErber
!     Last change: MSG 3/6/2011 2:07:09 PM
!     ******************************************************************
module sizecom
implicit none
      character (len=136) :: screen_messages
      integer (kind=4), parameter :: all_versions=0, &
         lf77_version=1, lf95_version=2, win95_version=3
end module sizecom
