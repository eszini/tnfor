       module miscfmt

       implicit none
       contains
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

         function rp(value, precision, convert_to_sng)
			real*4 :: rp, value
			integer :: precision
			character*256 :: prevision
			logical :: convert_to_sng
			rp=reduce_precision(value, precision, convert_to_sng)
		 end function rp
		 
         function reduce_precision(value, precision, convert_to_sng)
         real*4 :: result, reduce_precision
         integer :: precision
         real*4, intent(in) :: value
         character*256 :: tempvalue, fmt
         logical, intent(in) :: convert_to_sng
         result=value
         
            if(convert_to_sng) then
				result=sngl(dble(value))
			else
                tempvalue=" "
                fmt="(F10." // trim(itos(precision)) //")"
                write(tempvalue,fmt) sngl(value)
                read(tempvalue,fmt) result
            endif
                
            reduce_precision=result
         end function reduce_precision
         
       end module miscfmt

