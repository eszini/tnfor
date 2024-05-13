module ModFileList

  implicit none

  integer, parameter, private :: maxFileRecordLength = 2047

  type DynamicString
    character (len=:), allocatable :: record
  end type DynamicString

contains

!*********************************************************************
!*********************************************************************

  function getFileList(searchText,order,excludeText)

    implicit none
    character(len=*)   , intent(in)           :: searchText
    character(len=*)   , intent(in), optional :: order
    character(len=*)   , intent(in), optional :: excludeText
    type(DynamicString), allocatable          :: getFileList(:)
    character(len=:)   , allocatable          :: command,filename,orderLowerCase
    character(len=maxFileRecordLength)        :: record
    integer                                   :: iunit,counter,iostat,nRecord,nskip
    character(8)                              :: date
    character(10)                             :: time
    logical                                   :: exist

    if (present(order)) then
      orderLowerCase = getLowerCase(order)
    else
      orderLowerCase = 'name'
    end if

    if (getSlash()=='\') then  ! it's Windows cmd
      if (orderLowerCase=='name') then  ! ascending in name
        command = 'dir /b /a-d ' // searchText
      elseif (orderLowerCase=='date') then   ! oldest will be first
        command = 'dir /b /a-d /o:d ' // searchText
      else
        write(*,"(A)") '    FATAL: In Misc@getFileList()'
        write(*,"(A)") '           The requested file order is not supported.'
        write(*,*) '           order = ', orderLowerCase
        write(*,"(A)") 'Program aborted.'
      end if
      if (present(excludeText)) then
        command = command // " | findstr /v /i " // trim(adjustl(excludeText))
      end if
    else
      if (orderLowerCase=='name') then  ! ascending in name
        command = 'ls -1 ' // searchText
      elseif (orderLowerCase=='date') then   ! oldest will be first
        command = 'ls -tr ' // searchText
      else
        write(*,*) '    FATAL: In Misc@getFileList()'
        write(*,*) '           The requested file order is not supported.'
        write(*,*) '           order = ', orderLowerCase
        write(*,*) 'Program aborted.'
      end if
      if (present(excludeText)) then
        command = command // " --ignore=" // trim(adjustl(excludeText))
      end if
    end if

    ! generate a brand new, non-existing filename
    counter = 0
    do
      counter = counter + 1
      call date_and_time(date,time)
      filename = date // '_' // time // '_' // 'getFileList_' // int2str(counter) // '.temp'
      inquire(file=filename,exist=exist)    ! check if the file already exists
      if (exist) cycle
      exit
    end do
    call execute_command_line(command//' > '//filename)

    nRecord = getNumRecordInFile(filename)

    ! check filename is not among records
    nskip = 0
    open(newunit=iunit,file=filename,status='old')
    do counter = 1,nRecord
      read(iunit,'(A)',iostat=iostat) record
      if(iostat==0) then
        if(filename==trim(adjustl(record))) nskip = nskip + 1
      else
        write(*,*) '    FATAL (1): In Misc@getFileList()'
        write(*,*) '               Error occurred while reading file.'
        write(*,*) 'Program aborted.'
        stop
      end if
    end do
    close(iunit)

    allocate(getFileList(nRecord-nskip))
    open(newunit=iunit,file=filename,status='old')
    do counter = 1,nRecord
      read(iunit,'(A)',iostat=iostat) record
      if(iostat==0) then
        if (filename/=trim(adjustl(record))) getFileList(counter)%record = trim(adjustl(record))
      else
        write(*,*) '    FATAL (2): In Misc@getFileList()'
        write(*,*) '               Error occurred while reading file.'
        write(*,*) 'Program aborted.'
        stop
      end if
    end do
    close(iunit)

    if (getSlash()=='\') then  ! it's Windows cmd
      command = 'del '//filename
    else
      command = 'rm '//filename
    end if
    call execute_command_line(command)

  end function getFileList

!*********************************************************************
!*********************************************************************

  pure function getLowerCase(string)
    implicit None
    character(*), intent(in) :: string
    character(len(string))   :: getLowerCase
    character(26), parameter :: lowerCase = 'abcdefghijklmnopqrstuvwxyz', upperCase = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    integer                  :: ic, i
    ! capitalize each letter if it is lowercase
    getLowerCase = string
    do i = 1, len(string)
        ic = INDEX(upperCase, string(i:i))
        if (ic > 0) getLowerCase(i:i) = lowerCase(ic:ic)
    end do
  end function getLowerCase

!*********************************************************************
!*********************************************************************

  function getNumRecordInFile(filename)
    implicit none
    character(len=*), intent(in) :: filename
    character(len=8)             :: record
    integer                      :: getNumRecordInFile,iunit,iostat
    open(newunit=iunit,file=filename,status='old')
    getNumRecordInFile = 0
    do
      read(iunit,'(A)',iostat=iostat) record
      if(iostat==0) then
        getNumRecordInFile = getNumRecordInFile + 1
        cycle
      elseif(iostat<0) then
        exit
      else
        write(*,*) 'FATAL error occurred reading file in Misc in getNumRecordInFile().'
        write(*,*) 'Program aborted.'
        stop
      end if
    end do
    close(iunit)
  end function getNumRecordInFile

!*********************************************************************
!*********************************************************************
  character(len=1) function getSlash()
    implicit none
    character(len=7) :: os
    call get_environment_variable('OS',os)
    if (os=='Windows') then
      getSlash = '\'
    else
      getSlash = '/'
    end if
  end function getSlash

!*********************************************************************
!*********************************************************************

  pure function int2str(integerIn,formatIn)
    implicit none
    integer     , intent(in)           :: integerIn
    character(*), intent(in), optional :: formatIn
    character(:), allocatable          :: int2str
    integer                            :: i,length
    character(len=63)                  :: thisFormat
    if (present(formatIn)) then
      write(thisFormat,formatIn) integerIn
      int2str = trim(adjustl(thisFormat))
    else
      do i=1,63
        if(abs(integerIn)<10**i) then
          length = i
          if (integerIn<0) length = length + 1
          exit
        end if
      end do
      allocate(character(length) :: int2str)
      write(thisFormat,'(1I63)') length
      thisFormat = '(1I' // trim(adjustl(thisFormat)) // ')'
      write(int2str,thisFormat) integerIn
    end if
  end function int2str

!*********************************************************************
!*********************************************************************  

end module ModFileList
