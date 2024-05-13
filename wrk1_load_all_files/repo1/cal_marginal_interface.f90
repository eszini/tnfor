module cal_marginal_interface
interface 
    function CAL_MAR_MONTHLY_ENERGY_routine( ENERGY,BLKNO,UNIT, &
      LEFT_HEAT,RIGHT_HEAT, REMAINING_ENERGY)
      use PRODUCTION_ARRAYS_AND_DIMENSIONS
        logical*1 :: cal_mar_monthly_energy_routine
        real :: energy(2,MAX_CL_UNITS)
        integer*2 :: blkno(max_dispatch_blocks), unit(*)
        real :: left_heat(max_cl_units), right_heat(max_cl_units), remaining_energy
    end function CAL_MAR_MONTHLY_ENERGY_routine
end interface 
end module cal_marginal_interface