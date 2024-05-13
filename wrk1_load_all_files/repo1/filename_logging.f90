module filename_logging
use miscmod
implicit none
integer, parameter :: FNL_FileUnit=969

contains

	subroutine open_filename_log()
	integer ::ios
	character*100 :: filename
	logical :: fe
		filename="filename.log"
	
	    inquire(file=filename, exist=fe)
		if(fe) then
			open(unit=FNL_FileUnit, file=filename, iostat=ios)
		else
			open(unit=FNL_FileUnit, iostat=ios, &
				file=filename)
		endif
		
		if(ios/=0) then
			STOP "Couldn't open filename log."
		endif
	end subroutine open_filename_log
	
	subroutine close_filename_log()
		close(FNL_FileUnit)
		
	end subroutine close_filename_log
	
	subroutine log_file_open_action(token, filename)
	character*256 :: token
	character*1024 :: filename
	integer :: ios=0
		call open_filename_log()
		write(unit=FNL_FileUnit, IOSTAT=ios, fmt=*) trim(token),  &
			trim(filename)

		if(ios/=0) then
			! 113 Invalid numeric input (see "Literal Data" in the Essential Lahey Fortran 90 Reference).
			stop "Couldn't write filename.log."
		endif
		call close_filename_log()
		return
		

	
	end subroutine log_file_open_action
end module filename_logging