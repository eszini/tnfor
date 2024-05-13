module logging
use end_routine, only: end_program, er_message
use miscmod
use filemod
use params
use logging_decs
implicit none
character*1024 :: log_message=""
contains

function get_log_filename()
    character*512 :: get_log_filename
    character*256 :: OUTPUT_DIRECTORY

    call CurDir(" ",working_folder)
   
    get_log_filename=trim(working_folder) // "\\mdsgold.log"

end function

subroutine init_log()
logical :: exists
logical :: beenhere=.false.


    if(.not. beenhere) then

        beenhere=.true.

        log_filename=get_log_filename()

        INQUIRE(FILE=log_filename,EXIST=exists)
        
        if (exists) then
            call delete_file(log_filename)
        endif

        call open_log_file("logging")
        
        call write_log_entry("logging", "Log initialized.")
    endif ! beenhere
    
end subroutine init_log

subroutine close_log_file
    if(log_file_unit>0) then
		close(log_file_unit)
	endif
end subroutine close_log_file

subroutine open_log_file(section)
logical :: fe
character(len=*), intent(in) :: section
integer :: istat
character*512 :: msg
character*12 :: unit_str
integer :: lfu

    if(.not.log_file_open) then
    
        lfu=log_file_unit
        log_filename=get_log_filename()
        
        if(lfu==-1) then
            log_file_unit=get_new_unit()
            lfu=log_file_unit
            
            unit_str=itos(lfu)

            
            inquire(file=log_filename, exist=fe)

            if(fe) then
                open(unit=lfu, iostat=istat, file=trim(log_filename), &
                    status="REPLACE", form="FORMATTED", &
                    access="SEQUENTIAL", action="write", position="APPEND")
            else
                open(unit=lfu, iostat=istat, file=trim(log_filename), &
                     action="write")
            endif
             
             if(istat/=0) then
                stop "logging"
             else
                log_file_open=.true.
             endif
             
             msg="Opened " // trim(log_filename) // " as unit " // trim(unit_str)
             call write_log_entry(section,msg)
             
        endif ! lfu==-11
    endif ! not log_file_open
     
end subroutine open_log_file

subroutine write_log_entry(section, message)
character(len=*), intent(in) :: section
character(len=*) :: message
character*256 :: output=""
character*256 :: error_message=""
character*25 :: date_text=""
character*25 :: time_text=""
character*5 :: sistat
character*8 :: pos
integer :: charindex, istat

character*512 :: msg, sec
integer :: asciival, lastindex, destindex
logical, save :: beenhere=.false.


	if(.not. beenhere) then
		call init_log
		beenhere=.true.
	endif
    
    msg=""
    sec=""
    destindex=1

    lastindex=len_trim(section)
    do charindex=1, lastindex
        asciival=iachar(section(charindex:charindex))
        if (asciival<24 .or. asciival>128) then
            sec=trim(sec)
            exit
        else
            sec(destindex:destindex)=achar(asciival)
            destindex=destindex+1
        endif
    enddo

    destindex=1
    lastindex=len_trim(message)
    do charindex=1, lastindex
        asciival=iachar(message(charindex:charindex))
        if (asciival<24 .or. asciival>128) then
            msg=trim(msg)
            exit
        else
            msg(destindex:destindex)=achar(asciival)
            destindex=destindex+1
        endif
    enddo

    sistat=""

    call open_log_file(sec)
    
    if(log_file_unit >-1) then

        CALL get_tds(date_text, time_text)
        output=""
        output=trim(date_text)
        output = trim(output) // " - " // trim(time_text) 
        output=trim(output) // ":      " // trim(sec)
        output=trim(output) // " - " // trim(msg)

        write(log_file_unit, "(A)", iostat=istat) trim(output)
        if(istat/=0)then
            call end_program("Unable to write to " // trim(log_filename) // &
              " IOSTAT=" // int_to_string(istat) // "logging:1")
        endif
    else
        er_message="Cannot open log file " // trim(log_filename) // &
        " in logging.f90 (write_log_entry) : logging.f90#2"

        call end_program(er_message)
    endif


    

    
end subroutine write_log_entry
subroutine get_tds(date_text, time_text)
implicit none
character(25), intent(out) :: date_text, time_text
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