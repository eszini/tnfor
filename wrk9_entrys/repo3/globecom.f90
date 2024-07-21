module globecom
    integer (kind=2) :: base_year,year,end_point,study_period,endyr, &
                    last_study_year,extension_period, &
                    last_extension_year, max_years

    integer (kind=2), parameter ::  last_available_monthly_year=5
       
    integer (kind=2) :: stored_base_year,stored_current_year, &
                         stored_end_year
end module globecom


