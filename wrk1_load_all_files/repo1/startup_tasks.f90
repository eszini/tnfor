module startup_tasks
use logging
use miscmod

implicit none
contains
subroutine startup_msgold(command_line)
integer :: cl_len, charval
integer, parameter :: null_character=0, command_line_size=256
CHARACTER (LEN=command_line_size), intent(inout):: command_line
character (len=command_line_size) :: tempcl
integer :: INDEX, stat
character :: ch


	tempcl=""
	call write_log_entry("st:0001", "Removing null characters...")
    tempcl=remove_null_characters(tempcl)

	call write_log_entry("msgold:1", &
       "Passed tempcl: " // trim(tempcl))

	   cl_len=len(trim(tempcl))
	   if(cl_len>0) then

		   charval=iachar(tempcl(cl_len:cl_len))


		   log_message="ASCII - Last character in command-line is " // &
		   trim(itos(charval))


		   call write_log_entry("startup_tasks:0002", log_message)


		   if(charval==null_character) then
			tempcl=tempcl(1:cl_len-1)
		   endif
		endif



	   log_message="tempcl length: " // trim(itos(cl_len))
	   call write_log_entry("startup_tasks:0001", log_message)


       if(cl_len==0) then

	      call write_log_entry("msgold:1.b", &
       "tempcl variable is apparently empty.")
	   !stop "before getenv"
	      CALL GETENV("DEBUGDIR",tempcl)
          call write_log_entry("msgold:1.c", &
		  "command-line: " // trim(tempcl))
       endif


      IF(trim(tempcl) =="") THEN

           call write_log_entry("msgold:0017", &
        "command line is empty. No arguments were passed. " // &
       "Reading command line from system.")
          tempcl = " "
          CALL GETCL(tempcl)

           call write_log_entry("msgold1", "Read command line: " // &
            trim(tempcl))

      ELSE
          call write_log_entry("msgold2", "Command line: " // &
            trim(tempcl))

       ENDIF



	   command_line=tempcl

end subroutine startup_msgold
subroutine startup_dsex(TEMP_BASE_FAMILY_NAME, tempcl, SCENAME)
	character (len=256), intent(inout) :: TEMP_BASE_FAMILY_NAME
	character (len=256), intent(inout) :: tempcl
    character (len=256), intent(inout) :: SCENAME
	character*1024 :: anotherstring
	integer :: length


        call write_log_entry("dsex:00017", "In start-up routine.")
         CALL GET_MIDAS_COMMAND_LINE(TEMP_BASE_FAMILY_NAME)
         log_message="After calling get_midas_command_line, " // &
       "temp_base_family_name=" // trim(temp_base_family_name)

         call write_log_entry("dsex:00018", trim(log_message))


         CALL UPC(TEMP_BASE_FAMILY_NAME,tempcl)
         log_message="After calling UPC, tempcl=" // &
       trim(tempcl)
       call write_log_entry("dsex:0002", trim(log_message))

         tempcl=trim(tempcl)
         log_message="command-line: " // trim(tempcl)
         call write_log_entry("dsex:0002a", trim(log_message))
         anotherstring=trim(tempcl)
         log_message="anotherstring=" // trim(anotherstring)
         call write_log_entry("dsex:0002b", trim(log_message))

         if(trim(anotherstring)=="") then
            call write_log_entry("dsex:0002c", "Anotherstring is " // &
       "empty when it had just been set.  Filling it using WRITE.")
            write(anotherstring, "(A)") tempcl
         endif

         if(trim(anotherstring)=="") then
            er_message="Command line not set. Either" // &
            " environment variable DEBUGDIR must" // &
            " be set or arguments must be passed on" // &
             " the command line. " // &
        "(DEBUGDIR=" // trim(tempcl) // ")" // &
        "(anotherstring=" //trim(anotherstring) // ")."


            call write_log_entry("dsex:0001a", er_message)
            er_message="dsex_obj:0001 " // trim(er_message)

            call end_program(er_message)
         endif
         length = INDEX(tempcl,' ')

         READ(tempcl(1:length),*) SCENAME
		 call write_log_entry( &
		 "startup_tasks:scename", &
		 "SCENAME=" // trim(scename))

         IF(trim(SCENAME) == ' ') CALL CRITICAL_ERROR_MSG(1) !NO STUDY
end subroutine startup_dsex
end module startup_tasks
