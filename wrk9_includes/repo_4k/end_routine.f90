module end_routine
use params
implicit none
character(len=1024) :: er_message=""
character(len=512) :: params_wf=""
contains
    function get_end_message_filename()
    character(len=512) :: get_end_message_filename
        params_wf=trim(working_folder)
        get_end_message_filename=trim(params_wf) // "\exit_message.txt"
    end function get_end_message_filename
	
    subroutine end_program(message)
        character(len=*), intent(in) :: message
        character(len=256) :: filename=""
        integer :: file_unit, open_status
        character(len=10) :: stat
        filename=get_end_message_filename()
        
        stat="REPLACE"
        
        file_unit=6669

        
        open(unit=file_unit, file=trim(filename), action="write", IOSTAT=open_status)
        if(open_status/=0)then
            stop "Unable to open exit_message file."
        else
            write(file_unit,*) trim(message)
			call  flush(file_unit)
            close(file_unit)
        endif
        

        stop "Error condition encountered. See exit_message.txt for details." 
    end subroutine end_program
    
   
end module end_routine
