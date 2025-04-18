module file_existence
use end_routine

implicit none

contains
	function get_file_size(filename)
        integer (kind=4) :: get_file_size
		character (len=255) :: filename
        inquire(file=filename,recl=get_file_size)    
    end function get_file_size
    function does_file_exist(filename)
        logical :: does_file_exist, fe
        character(512), intent(in) :: filename
		character(256) :: line, cmd
		character(1024) :: file_text
        integer(kind=4) :: ios, fu

		file_text=""
		
        if(index(filename, "C1BPRIM")>0) then
            ios=ios
        endif

        inquire(FILE=filename, EXIST=fe)
      
        does_file_exist=fe
      
    end function does_file_exist
subroutine lalt_delete_file(filename)
    character*512 :: filename
    call system("del " // trim(filename))
    
end subroutine lalt_delete_file

subroutine ldelete_file(filename)
    character*512 :: filename
    character*512 :: foldername
    integer :: ierr
    integer :: file_unit
    call GETCWD(foldername)
    ! filename=trim(foldername) // "\" // trim(filename)
    
    if(does_file_exist(filename)) then
        file_unit=1965
        open (unit=file_unit,FILE=filename,iostat=ierr)
        if (ierr == 0) then
            close (file_unit,STATUS="DELETE")
        else
            stop "Cannot delete file."
        endif
        if (does_file_exist(filename)) then
            call lalt_delete_file(filename)
        endif
    endif
    
end subroutine ldelete_file
end module file_existence