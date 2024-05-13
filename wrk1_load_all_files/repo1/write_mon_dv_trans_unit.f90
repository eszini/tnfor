module write_mon_dv_trans_unit
implicit none
contains
subroutine WriteMonDvTransUnitEntry(unit_no, LOCAL_YEAR, unit_rec, month_name, trans_name, &
    trans_group, cap_value, product_hours, trans_hours, trans_cap_factor, &
    ENRG, trans_strikes, trans_var_exp, AVG_PROD_COSTS, unit_emissions, &
    nr_emission_types, trans_fix_exp, total_unit_cost, trans_rev, avg_rev_from_sales, UNIT_GROSS_MARGIN, &
    AVERAGE_VAR_OM, UNIT_EMISSIONS_COST, WHOLESALE_PRODUCTION_COST, RETAIL_SALES, &
     RTG,RMK,RFT,TRANS_UNIT_ID, POWERDAT_PLANT_ID, TOTAL_EMISSION_COSTS, TRANS_ASSET_CLASS, &
     NEWGEN_UNIT_STATUS, TRANS_ON_LINE, TRANS_OFF_LINE, RPM, EV_DATA_SOURCE, EV_PLANNING_AREA, &
     CAP_MARKET_REVENUE, CAPITAL_COST_OF_BUILD,  UNIT_EBITDA, RSP, CHARGING_ENERGY)
     
     
    
    integer, intent(in) :: unit_no ! MON_DV_TRANS_UNIT_NO
    integer, intent(inout) :: unit_rec ! MON_DV_TRANS_UNIT_REC
    REAL (KIND=4) :: PRT_ENDPOINT ! Function dec

    character*9, intent(in) :: month_name
    character*20, intent(in)  :: trans_name
    integer*2, intent(in) :: trans_group
    real*4, intent(in) :: cap_value, product_hours, trans_hours, &
        trans_cap_factor, trans_strikes, trans_var_exp, trans_fix_exp, &
        total_unit_cost, trans_rev
    real, intent(in) :: enrg, AVG_PROD_COSTS 
    real*4, intent(in) :: UNIT_EMISSIONS_COST(5), avg_rev_from_sales, unit_emissions(5)
    integer, intent(in) :: nr_emission_types
    real, intent(in) :: UNIT_GROSS_MARGIN, TRANS_UNIT_ID,POWERDAT_PLANT_ID
    real*4, intent(in) ::  AVERAGE_VAR_OM, WHOLESALE_PRODUCTION_COST, &
        RETAIL_SALES, TOTAL_EMISSION_COSTS, NEWGEN_UNIT_STATUS
    real, intent(in) :: TRANS_ASSET_CLASS, TRANS_ON_LINE, TRANS_OFF_LINE
    real*4, intent(in) :: RTG,RMK,RFT, RPM, CHARGING_ENERGY, LOCAL_YEAR
    real, intent(in) :: EV_DATA_SOURCE, EV_PLANNING_AREA, CAP_MARKET_REVENUE
    real , intent(in):: CAPITAL_COST_OF_BUILD, UNIT_EBITDA, RSP
    integer*2:: em


     WRITE(UNIT_NO, REC=unit_rec) PRT_ENDPOINT(), LOCAL_YEAR, month_name, trans_name, trans_group, cap_value, &
        product_hours, TRANS_HOURS, TRANS_CAP_FACTOR, ENRG/1000., 0., TRANS_STRIKES, 0., TRANS_VAR_EXP/1000000., &
        AVG_PROD_COSTS, (UNIT_EMISSIONS(EM), EM=1,nr_emission_types), TRANS_FIX_EXP/1000000., &
        TOTAL_UNIT_COST/1000000., TRANS_REV/1000000., ENRG/1000., avg_rev_from_sales, UNIT_GROSS_MARGIN, &
        0., 0., 0., 0.,  0., 0.,  0.,  0.,  AVERAGE_VAR_OM,  (UNIT_EMISSIONS_COST(EM), EM=1,NR_EMISSION_TYPES), &
        WHOLESALE_PRODUCTION_COST, RETAIL_SALES, RTG,RMK,RFT, TRANS_UNIT_ID, &! HESI UNIT ID NUM 
        POWERDAT_PLANT_ID, & ! POWER PLANT ID NUM 
        0., TOTAL_EMISSION_COSTS, 0., TRANS_ASSET_CLASS, & ! ASSET CLASS 
        0., 0., 0., &! UNECONOMIC_RATE 
        NEWGEN_UNIT_STATUS, TRANS_ON_LINE, & ! ONLINE 
        TRANS_OFF_LINE, & ! offline
        RPM, 0., &! HESI_SECOND_UNIT_ID 
        EV_DATA_SOURCE, EV_PLANNING_AREA, CAP_MARKET_REVENUE, CAPITAL_COST_OF_BUILD, UNIT_EBITDA, RSP,  &
        CHARGING_ENERGY/1000.

        UNIT_REC = UNIT_REC + 1
    end subroutine WriteMonDvTransUnitEntry
end module write_mon_dv_trans_unit