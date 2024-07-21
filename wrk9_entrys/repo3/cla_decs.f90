module cla_decs
use PROD_ARRAYS_DIMENSIONS
implicit none


type t_ns_cla_decs
integer (kind=2) :: capacity_market_pointer_acl(MAX_CL_UNITS)
! TODO: Rename UNITS variable to number_of_units, when safe to do so.

real (kind=4), allocatable :: MON_MDS_CL_CLASS_PURCHASES(:,:,:)
real (kind=4), allocatable :: MON_MDS_CL_CLASS_FUEL_COST(:,:,:)
real (kind=4), allocatable :: MON_MDS_NF_FUEL_OWNED_BC(:,:,:)
real (kind=4), allocatable :: MON_MDS_NF_FUEL_LEASED_BC(:,:,:)
real (kind=4), allocatable :: MON_MDS_CL_CAP_PURCHASES(:,:,:)
real (kind=4), allocatable :: MON_MDS_CL_CLASS_REVENUE(:,:,:)
real (kind=4), allocatable :: MON_MDS_ICAP_REV_BY_CLASS(:,:)
real (kind=4), allocatable :: MON_MDS_CL_CAP_REVENUE(:,:,:)


integer (kind=2) :: MAX_CAP_LIMITED_CLASS_ID_NUM
integer (kind=2) :: asset_cls_loc=0
integer (kind=2), allocatable :: ASSET_CLASS_ptr(:)

integer (kind=2) :: m_units=0
end type t_ns_cla_decs

    type(t_ns_cla_decs), save :: ns_cla_decs

contains

    function get_units()
        integer (kind=2) :: get_units
        get_units=ns_cla_decs%m_units
    end function get_units
    
    subroutine set_units(the_value)
        integer (kind=2) :: the_value
        ns_cla_decs%m_units=the_value
    end subroutine set_units
    
    subroutine increment_units()
        ns_cla_decs%m_units=ns_cla_decs%m_units+1
    end subroutine increment_units
    
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
     thiscl=ns_cla_decs%capacity_market_pointer_acl(idx) ! For debugging
            
            cla_bad=cla_ord_bad( &
				int(ns_cla_decs%capacity_market_pointer_acl(idx)) &
	        )

        else
            cla_bad=.true.
        endif

    end function cla_bad

end module cla_decs