module string
implicit none
contains
    function bl_2str(the_bool)
    character(len=5):: bl_2str
    logical :: the_bool
    
        if(the_bool) then
            bl_2str="True"
        else
            bl_2str="False"
        endif
        
    end function bl_2str
    
    function r_ats(array_to_convert)
    real, dimension(:) :: array_to_convert
    character(len=16000) :: r_ats
    
        r_ats=real_array_to_string(array_to_convert)
        
    end function r_ats

    function rtos(the_real)
        real(kind=4) :: the_real
        character(len=256) ::  rtos
        rtos=" "
        
        write(rtos, *) the_real
    end function  rtos
    function real_array_to_string(array_to_convert)
    character(len=512) :: real_array_to_string
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
    character(len=20) :: the_string, ftos
    real :: the_float
    
        the_string=""
        write(the_string, *) the_float
        ftos=the_string
        
    end function ftos
    ! itos calls int_to_string. Simpler for fixed line length code.
    function itos(the_int)
    integer, intent(in) :: the_int
    character(len=20) :: the_string, itos
    
        itos=int_to_string(the_int)

    end function itos
    function int_to_string(the_int)
    integer, intent(in) :: the_int
    character(len=20) :: the_string, int_to_string
    
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

end module string
