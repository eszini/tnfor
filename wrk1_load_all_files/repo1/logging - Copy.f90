module logging
use end_routine, only: end_program, er_message
use miscmod
use filemod
use params
implicit none
integer :: logging_log_file_unit=-1
character(512) :: log_message=""
contains

function get_log_filename()
    character*512 :: get_log_filename, wf
    
    wf=working_folder !can't see working_folder in debugger
    
    get_log_filename=trim(working_folder) // "\mdsgold.log"

end function

subroutine init_log()
character*512 :: log_filename
logical :: exists

    log_filename=get_log_filename()

    INQUIRE(FILE=log_filename,EXIST=exists)
    
    if (exists) then
        call delete_file(log_filename)
    endif
    call write_log_entry("logging", "Log initialized.")
end subroutine init_log

subroutine log_file_open(section, unitno, filename)
character*50, intent(in) :: section
integer, intent(in) :: unitno
character*512, intent(in) :: filename
character*512 :: msg
character*12 :: unit_str

     unit_str=itos(unitno)
     
     msg="Opening " // trim(filename) // " as unit " // trim(unit_str)
     
     
     call write_log_entry(section,msg)
     
end subroutine log_file_open

subroutine write_log_entry(section, message)
character(len=*), intent(in) :: section
character(len=*) :: message
character*256 :: output=""
character*512 :: filename=""
integer :: istat
character*256 :: error_message=""
character*11 :: date_text=""
character*25 :: time_text=""
character*5 :: sistat
character*8 :: pos
logical :: fe
integer :: charindex
integer, allocatable :: datastream(:)
integer :: len_datastream, ch

    if(.true.) then
        sistat=""
        filename=get_log_filename()
		if(logging_log_file_unit==-1) then
			logging_log_file_unit=get_new_unit()

            inquire(file=filename, exist=fe)
            
            if(fe) then
                open(unit=logging_log_file_unit, iostat=istat, file=trim(filename),status="OLD", &
                    access="SEQUENTIAL", action="write", position="APPEND")
            else
                open(unit=logging_log_file_unit, iostat=istat, file=trim(filename), &
                     action="write", status="NEW")
            endif			
		endif
        len_datastream=len_trim(message)
        allocate(datastream(len_datastream))
        do charindex=1, len_trim(message)
            ch=iachar(message(charindex:charindex))
            if(ch<0 .or. ch>127 .or. ch==10 .or. ch==13) then
                len_datastream=charindex
                exit
            else
                datastream(charindex:charindex)=ch
            endif
            
        enddo
        
        if(len_trim(message)>len_datastream) then
            message=trim(message(1:len_datastream))
        endif
        
        
        if(index(message, "DIRECTORY_FOR_BASE_FILES")>0) then
            output=output ! Debug
        endif
        
        
        if(logging_log_file_unit >-1) then


            

            if(istat==0) then
                
                CALL get_tds(date_text, time_text)
                output=""
                output=trim(date_text)
                output = trim(output) // " - " // trim(time_text) 
                output=trim(output) // ":      " // trim(section)
                output=trim(output) // " - " // trim(message)

                write(logging_log_file_unit, "(A)", iostat=istat) trim(output)
                if(istat/=0)then
                    call end_program("Unable to write to " // trim(filename) // &
                      " IOSTAT=" // int_to_string(istat) // "logging:1")
                endif
                
                
            else
                sistat=int_to_string(istat)
                
                er_message="Cannot open log file " // trim(filename) // &
                  " in write_log_entry. Returned error " // &
                  "status=" // trim(sistat) // ": logging.f90#1"
                call end_program(er_message)
            endif
            
            
        else
            er_message="Cannot open log file " // trim(filename) // " in logging.f90 (write_log_entry) : logging.f90#2"

            call end_program(er_message)
        endif

        if(allocated(datastream)) deallocate(datastream)
        
    endif
    

    
end subroutine write_log_entry
subroutine get_tds(date_text, time_text)
implicit none
character(15), intent(out) :: date_text, time_text
character(50) :: dt, tt
integer :: lt

    call date_and_time(dt, tt)
    ! 20230222 084456.284
    date_text=""
    date_text=dt(1:4) // "-"
    lt=len(trim(dt))
    date_text= trim(date_text) // dt(5:lt)
   
    time_text=tt(1:2) // ":" // tt(3:4) // ":" // tt(5:6) // tt(7:10)
        
    


end subroutine get_tds


end module logging