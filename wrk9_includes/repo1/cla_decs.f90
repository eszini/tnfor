module cla_decs
use PROD_ARRAYS_DIMENSIONS
implicit none
integer (kind=2) :: capacity_market_pointer_acl(MAX_CL_UNITS)

contains
    function cla_ord_bad(claord)
    logical :: cla_ord_bad
    integer :: claord
        cla_ord_bad=claord>=18976
    end function cla_ord_bad
    
    function cla_bad(idx)
        logical :: cla_bad
        integer, intent(in) :: idx
        integer (kind=2) :: thiscl
        cla_bad=.false.
        if(idx>0) then
            thiscl=capacity_market_pointer_acl(idx) ! For debugging
            
            cla_bad=cla_ord_bad(int(capacity_market_pointer_acl(idx)))

        else
            cla_bad=.true.
        endif

    end function cla_bad

end module cla_decs