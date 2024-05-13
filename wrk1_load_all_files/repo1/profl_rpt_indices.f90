module profl_rpt_indices
logical, parameter :: use_updated_output=.true.

! Constant naming:
! AMTF:  annual_by_tg_by_fuel
	  
integer, parameter :: amtf_coal=1, amtf_gas=2, amtf_oil=3, amtf_nuclear=4, &
  amtf_water=5, amtf_biomass=6, amtf_geothermal=7, amtf_other=8, &
  amtf_solar=9, amtf_dg=10, amtf_wind=11, amtf_osw=12
 
integer :: amtf_landfill=12, amtf_battery=13, amtf_total=14  
  
contains
subroutine init_amtf_params()
	if (use_updated_output) then
		amtf_landfill=13
		amtf_battery=14
		amtf_total=15
	endif
end subroutine init_amtf_params

end module profl_rpt_indices