module size_array_dimensions
implicit none
	integer (kind = 2), save :: max_simulation_years = 30,&
		system_class_num = 7
	integer (kind=2), parameter :: avail_data_years = 30,&
		max_planning_years=100,&
		max_financial_simulation_years=101
	logical (kind=1), save :: monthly_midas_active=.false.
end module size_array_dimensions
