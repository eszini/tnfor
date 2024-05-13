module array_debug_help
use cla_objt_arrays
use miscmod
use capacity_arrays
implicit none
contains


subroutine find_value_in_cl_tg_cap(value_to_find, return_value)
    integer :: value_to_find, retrieved_value
    character*1024:: return_value
    integer :: FT, TG, R_YEAR, SP
    character*20 :: array_name
    
            
        write(return_value, *) "Value ", value_to_find, " found at: "
        
        do FT=1, ubound(cl_tg_cap, 1)
            do  TG=1, ubound(cl_tg_cap, 2)
                do R_YEAR=1,ubound(cl_tg_cap, 3)
                    do sp=1,ubound(cl_tg_cap, 4)
                        retrieved_value=int(cl_tg_cap(ft, tg, r_year, sp))
                        if(real(retrieved_value) == value_to_find) then

                           return_value=trim(return_value) //"cl_tg_cap [FT=" // trim(itos(int(FT))) // "," // &
                           "TG=" // trim(itos(int(TG))) // ", " // &
                           "R_YEAR=" // trim(itos(int(r_year))) // ", " // &
                           "sp=" // trim(itos(int(sp))) // "]" // achar(13) // achar(10)
                           
                        endif
                    enddo !sp
                enddo !R_Year
            enddo ! TG
        enddo ! FT

        return_value=trim(return_value)

        
    end subroutine find_value_in_cl_tg_cap
    
    subroutine find_value_in_dev_tg_cap(value_to_find, return_value)
    integer :: value_to_find, retrieved_value
    character*1024:: return_value
    integer :: FT, TG, R_YEAR
    character*20 :: array_name
        
            
        write(return_value, *) "Value ", value_to_find, " found at: "
        
        do FT=1, ubound(dev_tg_cap, 1)
            do  TG=1, ubound(dev_tg_cap, 2)
                do R_YEAR=1,ubound(dev_tg_cap, 3)
                    retrieved_value=int(dev_tg_cap(ft, tg, r_year))
                    if(real(retrieved_value) == value_to_find) then

                       return_value=trim(return_value) //"dev_tg_cap [FT=" // trim(itos(int(FT))) // "," // &
                       "TG=" // trim(itos(int(TG))) // ", " // &
                       "R_YEAR=" // trim(itos(int(r_year))) // "]" // achar(13) // achar(10)
           
                    endif
                enddo !R_Year
            enddo ! TG
        enddo ! FT

        return_value=trim(return_value)

        
    end subroutine find_value_in_dev_tg_cap

    
end module array_debug_help
