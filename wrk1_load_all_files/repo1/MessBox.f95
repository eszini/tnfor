!     ******************************************************************
!     MessBox.f95
!     Copyright(c)  2000
!
!     Created: 3/17/2010 12:04:56 PM
!     Author : MARK S GERBER
!     Last change: MSG 3/17/2010 12:04:56 PM
!     ******************************************************************

module mb

!  Example of calling a Winapi function to display a messagebox.
!  Of course, you may find it easier to use WiSK to do this

  implicit none

! This declaration is needed so that the proper "decorations"
! will be applied to the imported symbol

  dll_import MessageBoxA

  integer :: MessageBoxA

!  Some (not all) Message box flags. The C header file winuser.h
!  contains a complete list of flags. Winuser.h is not provided
!  in the LF95 distribution.

  integer, parameter :: MB_OK                   = 0
  integer, parameter :: MB_OKCANCEL             = 1
  integer, parameter :: MB_ABORTRETRYIGNORE     = 2
  integer, parameter :: MB_YESNOCANCEL          = 3
  integer, parameter :: MB_YESNO                = 4
  integer, parameter :: MB_RETRYCANCEL          = 5
  integer, parameter :: MB_ICONHAND             = 16
  integer, parameter :: MB_ICONQUESTION         = 32
  integer, parameter :: MB_ICONEXCLAMATION      = 48
  integer, parameter :: MB_ICONASTERISK         = 64
  integer, parameter :: MB_ICONWARNING          = MB_ICONEXCLAMATION
  integer, parameter :: MB_ICONERROR            = MB_ICONHAND
  integer, parameter :: MB_ICONINFORMATION      = MB_ICONASTERISK
  integer, parameter :: MB_ICONSTOP             = MB_ICONHAND
  integer, parameter :: MB_DEFBUTTON1           = 0
  integer, parameter :: MB_DEFBUTTON2           = 256
  integer, parameter :: MB_DEFBUTTON3           = 512
  integer, parameter :: MB_DEFBUTTON4           = 768

! Dialog Box Command IDs

  integer, parameter :: IDOK     = 1
  integer, parameter :: IDCANCEL = 2
  integer, parameter :: IDABORT  = 3
  integer, parameter :: IDRETRY  = 4
  integer, parameter :: IDIGNORE = 5
  integer, parameter :: IDYES    = 6
  integer, parameter :: IDNO     = 7
  integer, parameter :: IDCLOSE  = 8
  integer, parameter :: IDHELP   = 9

end module mb

!   Driver program

Program mess_box

  use mb

  implicit none

!  iret is used to receive the function return

  integer            :: iret
  character(len=256) :: mess, title

!  Display a messagebox with an OK button
!  Note that all strings must be null terminated for C's sake

  mess = 'This is the message text!' // char(0)
  title = 'This is the messagebox title.' // char(0)

  iret = MessageBoxA(val(0), &
                     val(pointer(mess)), &
                     val(pointer(title)), &
                     val(MB_OK))

  if(iret == IDCANCEL) stop

!  Display a messagebox with two buttons

  mess = 'Do you want to continue looking at messageboxes?' // char(0)
  title = 'Messagebox title.' // char(0)

  iret = MessageBoxA(val(0), &
                     val(pointer(mess)), &
                     val(pointer(title)), &
                     val(MB_YESNO))

  if(iret == IDYES) then
!   Display a messagebox with several buttons and an icon
!   By default, the first button is selected
    mess = 'Do you still want to look at messageboxes?' // char(0)
    iret = MessageBoxA(val(0), &
                       val(pointer(mess)), &
                       val(pointer(title)), &
                       val(MB_YESNOCANCEL + MB_ICONQUESTION))
  else if(iret == IDNO .or. iret == IDCANCEL) then
    call end_program("Stop requested from messbox MB1")
  end if

  if(iret == IDYES) then
!   Display a messagebox with several buttons, an icon, and with the
!   CANCEL button selected
    iret = MessageBoxA(val(0), &
                       val(pointer(mess)), &
                       val(pointer(title)), &
                       val(MB_YESNOCANCEL + MB_ICONEXCLAMATION + MB_DEFBUTTON3))
  else if(iret == IDNO .or. iret == IDCANCEL) then
    call end_program("Stop requested from messbox MB2")
  end if

  if(iret == IDYES) then
    mess = 'There are no more messageboxes.' // char(0)
    title = 'Done' // char(0)
    iret = MessageBoxA(val(0), &
                     val(pointer(mess)), &
                     val(pointer(title)), &
                       val(MB_OK + MB_ICONHAND))
  end if

end program


