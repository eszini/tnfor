module miscmod
use end_routine
use string
implicit none
contains
function remove_nulls(str)
character (len=1024) :: result_string, remove_nulls
character (len=*) :: str
character :: ch 
integer :: index
 
    result_string=" "
    do index=1, trimlen(str)
        ch=str(index:index)
        if(iachar(ch)>0) then
            result_string=trim(result_string) // ch
        endif
    enddo
    
    remove_nulls=result_string
    
end function remove_nulls
function trimlen(strtocount)
! Return length of string if it were to be trimmed.  LAHEY
! intrinsics sometimes return the actual variable length or
! (variable_length+1) when a null character is embedded in the
! character array.
character(len=*), intent(in) :: strtocount
character(1024) :: sc
integer :: result, trimlen, index, ch
integer :: lt
    
    result=0
    lt=min(len(strtocount), len_trim(strtocount))
    sc=strtocount

    do index=1, lt
        ch=iachar(sc(index:index))
        if(ch==0 .or. ch<8) then
            sc(index:index)=" "
        endif

    enddo
    
    result=len_trim(sc)

    trimlen=result

end function trimlen
function remove_null_characters(string_to_fix)
character(*), intent(in) :: string_to_fix
character*1024 :: return_value, remove_null_characters
integer, parameter :: command_line_size=256
integer :: index, lastidx
character :: ch
    return_value=""
    lastidx=min(len_trim(string_to_fix), command_line_size)


    do index=1,command_line_size
        ch=string_to_fix(index:index)
        if(iachar(ch)/=0) then
            return_value=trim(return_value) // ch
        else
            exit
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
    
    
    function get_filename_from_unit(unit_no)
        integer :: unit_no
        logical :: file_opened
        character*512 :: filename, get_filename_from_unit

        filename=""
        get_filename_from_unit=" "
        inquire(unit=unit_no, opened=file_opened, NAME=filename)

        if(file_opened) then
            get_filename_from_unit=filename
        else
            call end_program( "miscmod:0004 - unit " // &
            trim(itos(unit_no)) // " has not been opened, yet an " // &
            "attempt has been made to retrieve a filename from it.")

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

function just_the_filename(filename)
character (len=*) :: filename
character (len=512) :: return_filename
character (len=512) :: just_the_filename
integer :: index, lastidx, foundidx

    return_filename=" "
    lastidx=len_trim(filename)
    foundidx=-1
    
    do index=lastidx,1, -1
        if (filename(index:index)=="\") then
            foundidx=index
            exit
        endif
    enddo
    
    if(foundidx<1) then
        er_message="miscmod:0002 - Unable to find path terminator " // &
        " in " // trim(filename)
        call end_program(er_message)
    else
        if(foundidx>1) then
            return_filename=filename(foundidx+1:len_trim(filename))
        else
            call end_program("miscmod:0003 - Could not parse " // &
            "return filename.")
        endif
    endif

    just_the_filename=return_filename
end function just_the_filename
!
      subroutine copy_file_2_file(FROM_FILE,TO_FILE)
	  use logging
	  implicit none

      CHARACTER (LEN=*)    :: FROM_FILE
      CHARACTER (LEN=*)    :: TO_FILE
      CHARACTER (LEN=4096) :: BUFFER
      INTEGER (KIND=4) :: FILE_SIZE
      INTEGER (KIND=4) :: FILSIZ
      INTEGER (KIND=4) :: BYTES_WRITTEN
      INTEGER (KIND=4) :: BUFFER_SIZE
      INTEGER (KIND=4) :: BYTES_LEFT
      INTEGER (KIND=4) :: UNIT_USED

      call write_log_entry("mainovl:0001", &
       "copy_file_2_file is copying " // trim(from_file) // " to " // &
       trim(to_file) // ".")

      BUFFER_SIZE = 4096
      FILE_SIZE = FILSIZ(FROM_FILE)
      if(FILE_SIZE .GT. 0) THEN
         INQUIRE(FILE=FROM_FILE,NUMBER=UNIT_USED)
         if(UNIT_USED > 0) CLOSE(UNIT_USED)
         INQUIRE(FILE=TO_FILE,NUMBER=UNIT_USED)
         if(UNIT_USED > 0) CLOSE(UNIT_USED)
         OPEN(99,FILE=FROM_FILE,ACCESS='TRANSPARENT')
         OPEN(98,FILE=TO_FILE,ACCESS='TRANSPARENT')
         BYTES_WRITTEN = 0
         DO WHILE (BYTES_WRITTEN+BUFFER_SIZE < FILE_SIZE)
            READ(99) BUFFER
            WRITE(98) BUFFER
            BYTES_WRITTEN = BYTES_WRITTEN + BUFFER_SIZE
         enddo
         BYTES_LEFT = FILE_SIZE - BYTES_WRITTEN
         READ(99) BUFFER(1:BYTES_LEFT)
         WRITE(98) BUFFER(1:BYTES_LEFT)
         CLOSE(99)
         CLOSE(98)
      endif
      return
      end subroutine copy_file_2_file

end module miscmod
