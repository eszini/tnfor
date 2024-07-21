module p_fuel
use hesi_gendecs
implicit none
    real (kind=4), allocatable :: p_fuel_delivery_1(:)
    real (kind=4), allocatable :: p_fuel_delivery_2(:)
    real (kind=4), allocatable :: p_fuel_delivery_3(:)
contains
real function get_p_fuel_delivery(r_nunits)  !r4
    integer (kind=2) :: r_nunits
        get_p_fuel_delivery = p_fuel_delivery_1(r_nunits)
    end function

    real function  get_p_fuel_delivery_2(r_nunits)   !r4
        integer (kind=2) :: r_nunits
        get_p_fuel_delivery_2 = p_fuel_delivery_2(r_nunits)
    end function

    real function  get_p_fuel_delivery_3(r_nunits)  !r4
        integer (kind=2) :: r_nunits
        get_p_fuel_delivery_3 = p_fuel_delivery_3(r_nunits)
    end function
end module p_fuel
