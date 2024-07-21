module filemod

implicit none

contains
subroutine alt_delete_file(filename)
    character (len=512) :: filename
    call system("del " // trim(filename))
    
end subroutine alt_delete_file

subroutine delete_file(filename)
    character (len=512) :: filename
    character (len=512) :: foldername
    integer :: ierr
    integer :: file_unit
	logical :: exists 
    call GETCWD(foldername)
    ! filename=trim(foldername) // "\" // trim(filename)
    INQUIRE(FILE=FILENAME,EXIST=exists)
	
    if(exists) then
        file_unit=1965
        open (unit=file_unit,FILE=filename,iostat=ierr)
        if (ierr == 0) then
            close (file_unit,STATUS="DELETE")
        else
            stop "Cannot delete file."
        endif
		INQUIRE(FILE=FILENAME,EXIST=exists)
        if (exists) then
            call alt_delete_file(filename)
        endif
    endif
    
end subroutine delete_file
integer function get_new_unit()
      integer :: unit
      integer, parameter :: LUN_MIN=2500, LUN_MAX=10000
      integer :: lun
      get_new_unit=-1
      do lun=LUN_MIN,LUN_MAX
      if(.not. is_unit_in_use(lun)) then
          get_new_unit=lun
          exit
        end if
      end do

end function get_new_unit
    
logical function is_unit_in_use(unit_to_check)
        logical :: opened
        integer :: unit_to_check
        inquire(unit=unit_to_check,opened=opened)
        if (.not. opened) then
            is_unit_in_use=.false.
        else
            is_unit_in_use=.true.
        endif
        
end function is_unit_in_use

end module filemod
