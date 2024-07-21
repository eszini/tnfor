module tf_decs
use msgoutpt_data
implicit none
real (kind=8), allocatable :: MONTHLY_TRANS_LOAD_HYDRO_MWH(:)
integer (kind=2) :: TRANS_POSITION=0
contains

      function store_gen_by_hour(r_hour_in_month, r_trans_group)
        implicit none
        real (kind=4) ::  store_gen_by_hour
        integer (kind=2) :: r_trans_group, r_hour_in_month
        

        if( abs(hourly_hydro(r_hour_in_month,r_trans_group,2))>.01) then
            store_gen_by_hour = 0.0
         else
            store_gen_by_hour=hourly_hydro(r_hour_in_month, &
                r_trans_group,1)
         endif


     end function store_gen_by_hour
end module tf_decs
