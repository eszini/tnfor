module miscmod
use end_routine
implicit none
contains

function remove_null_characters(string_to_fix)
character(*), intent(in) :: string_to_fix
character*1024 :: return_value, remove_null_characters
integer, parameter :: command_line_size=256
integer :: index
character :: ch
	return_value=""
	
    do index=1,command_line_size
		ch=string_to_fix(index:index)
		if(iachar(ch)/=0) then
			return_value=trim(return_value) // ch
		endif
	enddo
	
	remove_null_characters=trim(return_value)
end function remove_null_characters
function are_strings_equal(string1, string2)
    logical :: are_strings_equal
	character(*), intent(in) :: string1, string2
	integer :: stcr
	are_strings_equal=.true. ! Good unless they're different
	stcr=strcmp(string1, string2)
	if(stcr/=0) then
		are_strings_equal=.true.
	endif

end function are_strings_equal
function isgoodchar(ich)
integer :: ich
logical :: isgoodchar
	isgoodchar=.false.
	
	if(ich>31) then
		isgoodchar=.true.
	endif
end function isgoodchar

function realtrim(value_to_trim)
character(len=1024) :: value_to_trim
character*1024 :: result, realtrim
character :: thisch
integer :: index, lastspace, ich


	result=""
	lastspace=0
	do index=1, 1024
	    thisch=value_to_trim(index:index)
		ich=iachar(thisch)
		if(isgoodchar(ich)) then
			result=trim(result) // achar(ich)
		endif
		
		if(ich==32) then
			lastspace=index
		endif
	enddo
	
	if (lastspace/=0) then
		if(lastspace>index) then
			result=result(1:lastspace-1)
		endif
	endif
	
	realtrim=trim(result)
	
end function realtrim
function strcmp(string1, string2)
character(*), intent(in) :: string1, string2
integer :: index, maxlen, ch1, ch2, result, strcmp
	result=0
	maxlen=len(string1)
	if(maxlen<len(string2)) then
		maxlen=len(string2)
	endif
	
	do index=1, maxlen
		if(string1(index:index)/=string2(index:index)) then
			ch1=iachar(string1(index:index))
			ch2=iachar(string2(index:index))
			if(ch2/=ch1) then
				if(ch2<ch1)then
					result=-1
				elseif (ch2>ch1) then
					result=1
				endif
				exit
			endif
			
		endif
		
		strcmp=result
		
	enddo
	
	

	

	
end function strcmp


    subroutine array_error(caller, array_name, upper_bound, requested_index)
    character*256, intent(in) :: array_name, caller
    integer :: upper_bound, requested_index
    
        er_message=trim(caller) // " - An attempt was made to access element " // &
        trim(itos(requested_index)) // " from the " // trim(array_name) // " array , " // &
        "which only has " //trim(itos(upper_bound)) // " elements."
        
        call end_program(er_message)
    
    end subroutine array_error
    
    function bl_2str(the_bool)
    character*5 :: bl_2str
    logical :: the_bool
    
        if(the_bool) then
            bl_2str="True"
        else
            bl_2str="False"
        endif
        
    end function bl_2str
    
    function r_ats(array_to_convert)
    real, dimension(:) :: array_to_convert
    character*16000 :: r_ats
    
        r_ats=real_array_to_string(array_to_convert)
        
    end function r_ats

    function rtos(the_real)
        real*4 :: the_real
        character*256  rtos
        rtos=" "
        
        write(rtos, *) the_real
    end function  rtos
    function real_array_to_string(array_to_convert)
    character*512 :: real_array_to_string
    real, dimension(:) :: array_to_convert
    integer :: i, lb, ub
    
        real_array_to_string=""
        lb=lbound(array_to_convert, 1)
        ub=ubound(array_to_convert, 1)
        do i=lb, ub
            real_array_to_string=trim(real_array_to_string) // " " //  &
            trim(rtos(array_to_convert(i))) // ", " 
        enddo
    
    
    end function real_array_to_string
    
  
    ! ftos converts REALs (float) to string
	function ftos(the_float)
	character*20 :: the_string, ftos
	real :: the_float
	
		the_string=""
		write(the_string, *) the_float
		ftos=the_string
		
	end function ftos
    ! itos calls int_to_string. Simpler for fixed line length code.
    function itos(the_int)
    integer, intent(in) :: the_int
    character*20 :: the_string, itos
    
        itos=int_to_string(the_int)

    end function itos
    function int_to_string(the_int)
    integer, intent(in) :: the_int
    character*20 :: the_string, int_to_string
    
        the_string=""
        write(the_string, *) the_int
        int_to_string=trim(the_string)
        
        
    end function int_to_string

    function is_trimmable(ch)
    logical :: is_trimmable
    character(len=1), intent(in) :: ch
    integer :: ascii_value
    
        ascii_value=iachar(ch)
        if(ascii_value < 33 .or. ascii_value>126) then
            is_trimmable=.true.
        else
            is_trimmable=.false.
        endif
        
        
    end function is_trimmable    
    
    subroutine real_trim(the_string)
    character(len=*) :: the_string
    integer :: index
    
        do index=1, len_trim(the_string)
            if(is_trimmable(the_string(index:index))) then
                the_string(index:index)=" "
            endif
        enddo
        the_string=trim(the_string)
        
    end subroutine real_trim
    
    function get_filename_from_unit(unit_no)
        integer :: unit_no
        logical :: file_opened
        character*512 :: filename, get_filename_from_unit
        
        
        ! This code doesn't work when compiled under LAHEY Fortran.
        ! LAHEY always returns "" for the filename argument.
        
        filename=""
        get_filename_from_unit=" "
        inquire(unit=unit_no, opened=file_opened, file=filename)

        if(file_opened) then
            get_filename_from_unit=filename
        endif
    end function get_filename_from_unit
    
	subroutine check_capacity_array_bounds(array_name, array, &
	    idx1st, idx2nd) 
		
	real, allocatable, intent(in) ::  array(:,:)
	integer, intent(in) :: idx1st, idx2nd
	integer :: ub1st, ub2nd
	character*256 :: error_reason, array_name
	logical :: bounds_exceeded
	
	bounds_exceeded=.false.
	
	ub1st=ubound(array, 1)
	ub2nd=ubound(array, 2)
	
	if(idx1st>ub1st) then
		bounds_exceeded=.true.
		error_reason="first element exceeds bounds"
	else if (idx2nd>ub2nd) then
		bounds_exceeded=.true.
		error_reason="second element exceeds bounds"
	endif
	
	if (bounds_exceeded) then
		er_message="miscmod:0001 -  Bounds exceeded for the " // &
		trim(array_name) // " array: " // trim(error_reason) // "."
		call end_program(er_message)
	endif
	
	
	end subroutine check_capacity_array_bounds
end module miscmod