module clreport_summing
use capacity_arrays
use params
use prim_mover_idx
use ABB_CapMarketRptData
use IREC_ENDPOINT_CONTROL ! in modules.f90
use filemod
use end_routine
use logging
use filemod
use annlcommon


implicit none
logical :: text_output_file_open=.false.
integer :: text_output_file_handle
logical :: clreport_seed_with_sample_data=.false.

contains
subroutine handle_prod_for_steam(TG_POSITION, PM, FT, LM, PROD_BY_TG_BY_MW, PROD_BY_TG_BY_MWH, &
    ANNUAL_PROD_BY_TG_BY_MWH, ENRG, TRANS_CAP)
    real*4 :: TRANS_CAP
    integer*2 :: PM, TG_POSITION, FT, LM
    real :: PROD_BY_TG_BY_MWH(:,:), ANNUAL_PROD_BY_TG_BY_MWH(:,:), &
     PROD_BY_TG_BY_MW(:,:), ENRG
     

    IF(PM == pm_st) THEN ! STEAM

        LM = get_calculated_lm(int(pm), int(ft))

        PROD_BY_TG_BY_MWH(TG_POSITION,LM) =PROD_BY_TG_BY_MWH(TG_POSITION,LM) + ENRG
        ANNUAL_PROD_BY_TG_BY_MWH(TG_POSITION,LM) = ANNUAL_PROD_BY_TG_BY_MWH(TG_POSITION,LM) + ENRG
        PROD_BY_TG_BY_MW(TG_POSITION,LM) = PROD_BY_TG_BY_MW(TG_POSITION,LM) + TRANS_CAP

    ENDIF

end subroutine handle_prod_for_steam
subroutine update_start_costs(ANNUAL_UNIT_START_COSTS, MAX_CL_UNITS, MONTH_UNIT_START_COSTS, &
    MONTHLY_START_COSTS, MONTHLY_CAPACITY_REVENUE, &
    CAP_MARKET_REVENUE, ANNUAL_UNIT_CAPACITY_REVENUE, ANNUAL_UNIT_STARTS, MONTHLY_STARTS, &
    MONTH_UNIT_STARTS, ANNUAL_P_FUEL_CONSUMPTION, ANNUAL_S_FUEL_CONSUMPTION, &
    ANNUAL_E_FUEL_CONSUMPTION, E_FUEL_CONSUMPTION, WHOLESALE_PRODUCTION_COST, &
    ANNUAL_WHOLESALE_PROD_COST, I, P_FUEL_CONSUMPTION, S_FUEL_CONSUMPTION, UNITNO, &
    AVERAGE_PRODUCTION_COSTS, MON_ECO_SALES_ENRG_FROM)
    
integer :: MAX_CL_UNITS, I, UNITNO
real*4 :: ANNUAL_UNIT_START_COSTS(MAX_CL_UNITS), MONTH_UNIT_START_COSTS(MAX_CL_UNITS), &
    MONTHLY_START_COSTS, MONTHLY_CAPACITY_REVENUE, &
    ANNUAL_UNIT_CAPACITY_REVENUE(MAX_CL_UNITS), ANNUAL_E_FUEL_CONSUMPTION(MAX_CL_UNITS)
real*4 :: ANNUAL_WHOLESALE_PROD_COST(MAX_CL_UNITS), &
    WHOLESALE_PRODUCTION_COST, ANNUAL_S_FUEL_CONSUMPTION(MAX_CL_UNITS), &
    ANNUAL_P_FUEL_CONSUMPTION(MAX_CL_UNITS), MON_ECO_SALES_ENRG_FROM(MAX_CL_UNITS)
real :: CAP_MARKET_REVENUE, E_FUEL_CONSUMPTION, P_FUEL_CONSUMPTION, S_FUEL_CONSUMPTION, &
    AVERAGE_PRODUCTION_COSTS
integer*2 :: MONTHLY_STARTS, ANNUAL_UNIT_STARTS(MAX_CL_UNITS), &
    MONTH_UNIT_STARTS(MAX_CL_UNITS)



          ANNUAL_UNIT_START_COSTS(I) = ANNUAL_UNIT_START_COSTS(I) + MONTH_UNIT_START_COSTS(I)
               MONTHLY_START_COSTS = MONTHLY_START_COSTS + MONTH_UNIT_START_COSTS(I)

               MONTHLY_CAPACITY_REVENUE = MONTHLY_CAPACITY_REVENUE + CAP_MARKET_REVENUE
               ANNUAL_UNIT_CAPACITY_REVENUE(I) = ANNUAL_UNIT_CAPACITY_REVENUE(I) + CAP_MARKET_REVENUE
!
               ANNUAL_UNIT_STARTS(I) = ANNUAL_UNIT_STARTS(I) + MONTH_UNIT_STARTS(I)
            MONTHLY_STARTS = MONTHLY_STARTS + MONTH_UNIT_STARTS(I)
!
               ANNUAL_P_FUEL_CONSUMPTION(I) = ANNUAL_P_FUEL_CONSUMPTION(I) + P_FUEL_CONSUMPTION
               ANNUAL_S_FUEL_CONSUMPTION(I) = ANNUAL_S_FUEL_CONSUMPTION(I) + S_FUEL_CONSUMPTION
               ANNUAL_E_FUEL_CONSUMPTION(I) = ANNUAL_E_FUEL_CONSUMPTION(I) + E_FUEL_CONSUMPTION
               WHOLESALE_PRODUCTION_COST = AVERAGE_PRODUCTION_COSTS * MON_ECO_SALES_ENRG_FROM(UNITNO)/1000000.
               ANNUAL_WHOLESALE_PROD_COST(I) = ANNUAL_WHOLESALE_PROD_COST(I) + WHOLESALE_PRODUCTION_COST
end subroutine update_start_costs

subroutine update_energy_in_prod_arrays(PROD_BY_MK_BY_MWH, MK, MW, ANNUAL_PROD_BY_MK_BY_MWH, ENRG, I, MAX_CL_UNITS)
real :: PROD_BY_MK_BY_MWH(:,:), MW(:,:), ANNUAL_PROD_BY_MK_BY_MWH(:,:)
real :: enrg
integer :: mk
integer :: I, MAX_CL_UNITS

    PROD_BY_MK_BY_MWH(MK,1) = PROD_BY_MK_BY_MWH(MK,1) + ENRG
    PROD_BY_MK_BY_MWH(MK,2) = PROD_BY_MK_BY_MWH(MK,2) + MW(2,I)
    PROD_BY_MK_BY_MWH(MK,3) = PROD_BY_MK_BY_MWH(MK,3) + 1
    ANNUAL_PROD_BY_MK_BY_MWH(MK,1) = ANNUAL_PROD_BY_MK_BY_MWH(MK,1) + ENRG
    ANNUAL_PROD_BY_MK_BY_MWH(MK,2) = ANNUAL_PROD_BY_MK_BY_MWH(MK,2) + MW(2,I)
    ANNUAL_PROD_BY_MK_BY_MWH(MK,3) = ANNUAL_PROD_BY_MK_BY_MWH(MK,3) + 1
end subroutine update_energy_in_prod_arrays

subroutine collect_monthly_unit_emissions(MONTHLY_UNIT_EMISSIONS, UNIT_EMISSIONS)
real*4, intent(inout) :: MONTHLY_UNIT_EMISSIONS(5), UNIT_EMISSIONS(5)
integer :: index, ub
    ub=ubound(MONTHLY_UNIT_EMISSIONS,1)
    do index=1, ub ! Was explicitly setting elements, 1..5
        MONTHLY_UNIT_EMISSIONS(index)=MONTHLY_UNIT_EMISSIONS(index)+UNIT_EMISSIONS(index)
    enddo
    
end subroutine collect_monthly_unit_emissions

subroutine collect_energy_data(AVERAGE_HEATRATE, AVERAGE_FUEL_COST_PER_MWH, &
    AVERAGE_PRODUCTION_COSTS, enrg, CANADA, HEAT, HEAT_CONVERSION, UNITNO, &
    MAX_CL_UNITS, VCPMWH)
    
integer :: MAX_CL_UNITS

real :: AVERAGE_HEATRATE, AVERAGE_FUEL_COST_PER_MWH, AVERAGE_PRODUCTION_COSTS, enrg, HEAT_CONVERSION, VCPMWH(MAX_CL_UNITS)
logical*1 :: CANADA
real*8 :: FUEL_COST, HEAT
integer*2 UNITNO 
    IF(ENRG > 0.45) THEN
      AVERAGE_HEATRATE = HEAT_CONVERSION*HEAT/ENRG
      AVERAGE_FUEL_COST_PER_MWH = FUEL_COST / ENRG

      AVERAGE_PRODUCTION_COSTS = SNGL((FUEL_COST))/ENRG+VCPMWH(UNITNO)
      IF(.NOT. CANADA) AVERAGE_HEATRATE = AVERAGE_HEATRATE + .5
    ELSE
      FUEL_COST = 0.0 D0
      HEAT = 0.0 D0
      AVERAGE_HEATRATE = 0.0
      AVERAGE_FUEL_COST_PER_MWH = 0.0
      AVERAGE_PRODUCTION_COSTS = 0.0
    ENDIF
end subroutine collect_energy_data
function process_pm(pm)
integer :: PM
logical :: process_pm, return_value
integer, dimension(21) :: pms=(/pm_cc, pm_fc, pm_ge, pm_gt, pm_hydro, pm_ic, pm_na, pm_nu, pm_sl, pm_st, pm_wt, pm_zz, pm_default, pm_bi, pm_lf, pm_ba, pm_dg, pm_ow, pm_hb, pm_h2, pm_cs/)
integer :: index

    return_value=.false.
    do index=1, ubound(pms,1)
        if(pms(index)==pm) THEN
            return_value=.true.
            exit
        ENDIF
    enddo
    
    process_pm=return_value


end function process_pm
subroutine handle_transact_prod_report(SEAS_HOURS, MFT1,TG_BASE, TG_PEAK, multi_area_name, YEAR, BASE_YEAR, &
    ISEAS, TRAN_PROD_FUEL_NO, TRAN_PROD_FUEL_REC, CL_MONTH_NAME, MAX_LOAD_TYPES, upper_trans_group)


integer*2 :: TEMP_TL_MWH, LM, tmp_LM, I, PM, FT, TRAN_PROD_FUEL_NO, &
            MAX_LOAD_TYPES, ISEAS, upper_trans_group
real :: SEAS_HOURS 
real*4 ::    ROR_CAPACITY, GET_MONTHLY_TL_HYDRO_MWH, TEMP_TL_HYDRO_MW, &
             GET_SYSTEM_PROD_BY_TG_BY_MWH, GET_WH_MONTH_ENERGY, GET_WH_MONTH_CAPACITY,  &
              GET_MONTHLY_TL_HYDRO_MW
real ::   BASE_YEAR, YEAR
 
integer*2 ::  TFT, GET_TRANS_GROUP_INDEX
real*4 :: TG_MWH, TG_BASE, TG_PEAK 
integer*2 :: MFT1
integer :: tg
logical :: TEMP_L1
logical*1 ::GET_TG_MONTH_SUM_B4_HYDRO 
real*4 :: SYSTEM_PROD_BY_TG_BY_MWH

character*35, allocatable ::  multi_area_name(:)
character*9 :: CL_MONTH_NAME(14)
integer :: J, TRAN_PROD_FUEL_REC, tgi, TRANS_PROD_TEXT_FILENO=998, jj
logical :: trans_prod_file_open=.false., write_trans_prod_text_file=.false.
integer, dimension(23) :: prime_mover_indices=(/pm_total, pm_cc, pm_fc, pm_ge, pm_gt, pm_hydro, pm_ic, pm_na, pm_nu, &
       pm_sl, pm_st, pm_wt, pm_zz, pm_default, pm_bi, pm_lf, pm_ba, pm_dg, pm_ow, pm_hb, pm_h2, pm_cs, &
       pm_noname/)
integer :: pm_index, this_pm


        ! Text file for testing. Contains all data from both main WRITE (mopro, profl) statements in this module.
        if(.not. trans_prod_file_open .and. write_trans_prod_text_file) then
            open(file="trans_prod_data.txt", unit=TRANS_PROD_TEXT_FILENO)
        endif
        


         IF(TRANSACT_PROD_REPORT_ACTIVE) THEN

            DO I = 1,  UPPER_TRANS_GROUP  ! ubound(ANNUAL_PROD_BY_TG_BY_MW, 1)
                !tgi=GET_TRANS_GROUP_INDEX(int(i)) mine - keeping around in case
                if(.not. clreport_seed_with_sample_data) then

     
                    do pm_index=1, ubound(prime_mover_indices, 1)
                        this_pm=prime_mover_indices(pm_index)
                        if(process_pm(this_pm)) then

							ANNUAL_PROD_BY_TG_BY_MW(i,this_pm) = &
								MAX(ANNUAL_PROD_BY_TG_BY_MW(i,this_pm), &
								PROD_BY_TG_BY_MW(i,this_pm))



! ***************

                              if(this_pm==pm_st) then
                                DO FT = idx_steam_coal, idx_steam_oil ! 1-3
                                    LM=get_calculated_lm(int(this_pm), int(FT))
                                    ANNUAL_PROD_BY_TG_BY_MW(i,LM) = &
                                        MAX(ANNUAL_PROD_BY_TG_BY_MW(i,LM), &
                                        PROD_BY_TG_BY_MW(i,LM))
										

										
                                enddo
								
								! added by John 6/16. commented out this setting above,
								! and setting it here to see if that fixes the odd
								! accumulation problem with one of the steam elements.
								ANNUAL_PROD_BY_TG_BY_MW(i,this_pm) = &
								MAX(ANNUAL_PROD_BY_TG_BY_MW(i,this_pm), &
									PROD_BY_TG_BY_MW(i,this_pm))

                             endif
                             
                        ENDIF

! ****************


                    enddo
   



                endif
                
                do pm_index=1, ubound(prime_mover_indices, 1)
                    this_pm=prime_mover_indices(pm_index)
                    ! sum_annuals can be tgi or i. I is what was used originally.
                    !if(this_pm/=pm_st) then
                        call sum_annuals(int(i), int(this_pm), int(this_pm), .true., year, BASE_YEAR, CL_MONTH_NAME)
                    !endif
                enddo

                
!
! 031005. HYDRO SECTION. ADDED FOR DOUG
!
               WH_MONTH_ENERGY = GET_WH_MONTH_ENERGY(ISEAS,I)
!
               CALL GET_ONE_TRANS_ROR_CAP(I,ROR_CAPACITY)
!
               T_I = 1
               T_J = 6
!
               SYSTEM_PROD_BY_TG_BY_MWH = &
                       GET_SYSTEM_PROD_BY_TG_BY_MWH(T_I,I,T_J)
!
               TEMP_TL_MWH = ROR_CAPACITY*SEAS_HOURS + &
             (GET_MONTHLY_TL_HYDRO_MWH(I) + WH_MONTH_ENERGY) - &
                                     SYSTEM_PROD_BY_TG_BY_MWH
!
               TEMP_TL_HYDRO_MW = GET_MONTHLY_TL_HYDRO_MW(I) + &
                                GET_WH_MONTH_CAPACITY(ISEAS,I) + &
                                                      ROR_CAPACITY
!

               PROD_BY_TG_BY_MWH(I,pm_hydro) = &
                              PROD_BY_TG_BY_MWH(I,pm_hydro) + &
                                            TEMP_TL_MWH
               ANNUAL_PROD_BY_TG_BY_MWH(I,pm_hydro) = &
                        ANNUAL_PROD_BY_TG_BY_MWH(I,pm_hydro) + &
                                            TEMP_TL_MWH

              prod_by_tg_by_mwh(I, pm_ow)=prod_by_tg_by_mwh(i, pm_ow)
              ANNUAL_PROD_BY_TG_BY_MWH(I, pm_ow)= &
                ANNUAL_PROD_BY_TG_BY_MWH(I, pm_ow)
              

               PROD_BY_TG_BY_MWH(I,pm_ba) = &
                              PROD_BY_TG_BY_MWH(I,pm_ba) + &
                                SYSTEM_PROD_BY_TG_BY_MWH
               ANNUAL_PROD_BY_TG_BY_MWH(I,pm_ba) = &
                        ANNUAL_PROD_BY_TG_BY_MWH(I,pm_ba) + &
                                SYSTEM_PROD_BY_TG_BY_MWH
               PROD_BY_TG_BY_MW(I,pm_ba) = &
                              PROD_BY_TG_BY_MW(I,pm_ba) + &
                        SYSTEM_PROD_BY_TG_BY_MWH/SEAS_HOURS
               ANNUAL_PROD_BY_TG_BY_MW(I,pm_ba) = &
                        ANNUAL_PROD_BY_TG_BY_MW(I,pm_ba) + &
                      SYSTEM_PROD_BY_TG_BY_MWH/SEAS_HOURS
!
               PROD_BY_TG_BY_MW(I,pm_hydro) = &
                     PROD_BY_TG_BY_MW(I,pm_hydro) + TEMP_TL_HYDRO_MW
               ANNUAL_PROD_BY_TG_BY_MW(I,pm_hydro) = &
                      MAX(ANNUAL_PROD_BY_TG_BY_MW(I,pm_hydro), &
                                     PROD_BY_TG_BY_MW(I,pm_hydro))

!
               MW_BY_TG_BY_FUEL(I,pm_hydro) = &
                   MW_BY_TG_BY_FUEL(I,pm_hydro) + TEMP_TL_HYDRO_MW
               ANNUAL_MW_BY_TG_BY_FUEL(I,pm_hydro) = &
          MAX(ANNUAL_MW_BY_TG_BY_FUEL(I,pm_hydro),TEMP_TL_HYDRO_MW)
               MWH_BY_TG_BY_FUEL(I,pm_hydro) = &
                        MWH_BY_TG_BY_FUEL(I,pm_hydro) + TEMP_TL_MWH
               ANNUAL_MWH_BY_TG_BY_FUEL(I,pm_hydro) = &
                  ANNUAL_MWH_BY_TG_BY_FUEL(I,pm_hydro) + TEMP_TL_MWH

!
               MW_BY_TG_BY_FUEL(I,MFT1) = &
                        MW_BY_TG_BY_FUEL(I,MFT1) + TEMP_TL_HYDRO_MW
               IF(ISEAS == 12) THEN
                  ANNUAL_MW_BY_TG_BY_FUEL(I,MFT1) = &
                         ANNUAL_MW_BY_TG_BY_FUEL(I,MFT1) + &
                         ANNUAL_MW_BY_TG_BY_FUEL(I,pm_hydro)

               ENDIF
               MWH_BY_TG_BY_FUEL(I,MFT1) = &
                        MWH_BY_TG_BY_FUEL(I,MFT1) + TEMP_TL_MWH
               ANNUAL_MWH_BY_TG_BY_FUEL(I,MFT1) = &
                  ANNUAL_MWH_BY_TG_BY_FUEL(I,MFT1) + TEMP_TL_MWH
!
!

               PROD_BY_TG_BY_MWH(I,pm_bi) = &
                        PROD_BY_TG_BY_MWH(I,pm_bi) + &
                         SYSTEM_PROD_BY_TG_BY_MWH + TEMP_TL_MWH
               ANNUAL_PROD_BY_TG_BY_MWH(I,pm_bi) = &
                        ANNUAL_PROD_BY_TG_BY_MWH(I,pm_bi) + &
                          SYSTEM_PROD_BY_TG_BY_MWH + TEMP_TL_MWH
               PROD_BY_TG_BY_MW(I,pm_bi) = &
                        PROD_BY_TG_BY_MW(I,pm_bi) +TEMP_TL_HYDRO_MW

               ANNUAL_PROD_BY_TG_BY_MW(I,pm_bi) = &
                      MAX(ANNUAL_PROD_BY_TG_BY_MW(I,pm_bi), &
                       PROD_BY_TG_BY_MW(I,pm_bi))


               TG = GET_TRANS_GROUP_INDEX(I)

               !TEMP_L1 is set here but never used.
               TEMP_L1 = GET_TG_MONTH_SUM_B4_HYDRO(TG, &
                                                   TG_MWH, &
                                                   TG_PEAK, &
                                                   TG_BASE)
               LOAD_BY_TG_BY_MWH(I,PM_CC) = MAX(0.,TG_BASE)
               LOAD_BY_TG_BY_MWH(I,PM_GE) = MAX(0.,TG_PEAK)
               LOAD_BY_TG_BY_MWH(I,PM_GT) = MAX(0.,TG_MWH)

               IF(SEAS_HOURS > 0.0) THEN

                  LOAD_BY_TG_BY_MWH(I,pm_fc) = &
                     LOAD_BY_TG_BY_MWH(I,pm_gt)/SEAS_HOURS
               ENDIF
               ANNUAL_LOAD_BY_TG_BY_MWH(I,pm_cc) = &
               MIN(ANNUAL_LOAD_BY_TG_BY_MWH(I,pm_cc), &
                          LOAD_BY_TG_BY_MWH(I,pm_cc))
               ANNUAL_LOAD_BY_TG_BY_MWH(I,pm_ge) = &
                 MAX(ANNUAL_LOAD_BY_TG_BY_MWH(I,pm_ge), &
                            LOAD_BY_TG_BY_MWH(I,pm_ge))
           ANNUAL_LOAD_BY_TG_BY_MWH(I,pm_gt) = &
                ANNUAL_LOAD_BY_TG_BY_MWH(I,pm_gt) + &
                        LOAD_BY_TG_BY_MWH(I,pm_gt)


                tg_CapacitySupplyObligation(I,ISEAS)= &
                            PROD_BY_TG_BY_MW(I,pm_ge) &
                            + PROD_BY_TG_BY_MW(I,pm_hydro) &
                            + PROD_BY_TG_BY_MW(I,pm_ic) &
                            + PROD_BY_TG_BY_MW(I,pm_nu) &
                            + PROD_BY_TG_BY_MW(I,pm_zz) &
                            + PROD_BY_TG_BY_MW(I,pm_lf) &
                            + PROD_BY_TG_BY_MW(I,pm_ba)
                            
               TG_ResourceActualPreference = &
                                  TG_EffectiveCapacity(I,ISEAS) &
                              - TG_CapacitySupplyObligation(I,ISEAS)




            UZVars(ISEAS,I,1:pm_ba) = &
        
            PROD_BY_TG_BY_MW(I,1:pm_ba)
            UZVars(ISEAS,I,pm_ba_1)= &
                 TG_EffectiveCapacity(I,ISEAS)
            UZVars(ISEAS,I,pm_ba_2)= &
                 TG_CapacitySupplyObligation(I,ISEAS)
            UZVars(ISEAS,I,pm_ba_3)= &
                 TG_ResourceActualPreference
            UZVars(ISEAS,I,pm_ba_4)= &
                PROD_BY_TG_BY_MWH(I,pm_bi)


       CAPACITY_AREA_NAME(I) = MULTI_AREA_NAME(I)
       TRANS_PROD_REC = RPTREC(TRANS_PROD_NO)

       
       ! If clreport_seed_with_sample_data is false, seed_clreport won't do anything.
       call seed_clreport(year, BASE_YEAR, iseas, CL_MONTH_NAME, I, MULTI_AREA_NAME, MAX_LOAD_TYPES, MFT1)
        if (write_trans_prod_text_file) THEN
            WRITE(TRANS_PROD_TEXT_FILENO, *) &
            PRT_ENDPOINT(), REAL(YEAR+BASE_YEAR), &
            CL_MONTH_NAME(ISEAS), MULTI_AREA_NAME(I)
            do jj=1, max_prod_types
                write(TRANS_PROD_TEXT_FILENO, *) PROD_BY_TG_BY_MW(i, jj)
            ENDDO
            do jj=1, max_prod_types
                write(TRANS_PROD_TEXT_FILENO, *) PROD_BY_TG_BY_MWH(i, jj)
            ENDDO
            
            write(TRANS_PROD_TEXT_FILENO, *) 2.0
            do jj=1, MAX_LOAD_TYPES
                write(TRANS_PROD_TEXT_FILENO, *) LOAD_BY_TG_BY_MWH(i, jj)
            ENDDO
            write(trans_prod_text_fileno, *) trim(MULTI_AREA_NAME(I))
            do jj=1, MFT1
                write(TRANS_PROD_TEXT_FILENO, *) MW_BY_TG_BY_FUEL(i, JJ)
            enddo

            do jj=1, MFT1
                write(TRANS_PROD_TEXT_FILENO, *) MWH_BY_TG_BY_FUEL(i, JJ)
            enddo

        endif

       ! Transact Production report (msgmopro)
       WRITE(TRANS_PROD_NO,REC=TRANS_PROD_REC) &
        PRT_ENDPOINT(), REAL(YEAR+BASE_YEAR), &
        CL_MONTH_NAME(ISEAS), MULTI_AREA_NAME(I),  &
        (PROD_BY_TG_BY_MW(I,J),J=1,MAX_PROD_TYPES), &
        (PROD_BY_TG_BY_MWH(I,J),J=1,MAX_PROD_TYPES), &
        2.0, &
        (LOAD_BY_TG_BY_MWH(I,J),J=1,MAX_LOAD_TYPES)
        
        ! TG_EffectiveCapacity(I,ISEAS), TG_CapacitySupplyObligation(I,ISEAS), &
        ! TG_ResourceActualPreference

       TRANS_PROD_REC = TRANS_PROD_REC + 1
!

       !Transaction Production and Fuel report (msgprofl)
       TRAN_PROD_FUEL_REC = RPTREC(TRAN_PROD_FUEL_NO)
       WRITE(TRAN_PROD_FUEL_NO,REC=TRAN_PROD_FUEL_REC) &
            PRT_ENDPOINT(), &
            REAL(YEAR+BASE_YEAR), &
            CL_MONTH_NAME(ISEAS), &
            MULTI_AREA_NAME(I), &
            (MW_BY_TG_BY_FUEL(I,J),J=1,MFT1), &!1-20 &
            (MWH_BY_TG_BY_FUEL(I,J),J=1,MFT1) !21-40
       TRAN_PROD_FUEL_REC = TRAN_PROD_FUEL_REC + 1
    ENDDO
 ENDIF

end subroutine handle_transact_prod_report

function get_calculated_lm(PM, FT)
integer, intent(in) :: PM, FT
integer :: get_calculated_lm, iresult, file_handle
character*255 :: filename="get_calculated_lm.log"
logical :: fe
integer :: istat, max_result


!integer :: count=0
!save :: count
!count=count+1
    
    ! log_message="get_calculated_lm(PM=" // trim(itos(PM)) // ", " // &
    !    "FT="//trim(itos(FT))
    !call write_log_entry("clreport_summing:22", log_message)

    if(pm==pm_st) then
        max_result=pm_lm3_steam+FT-1
        iresult=pm_lm1_steam-1+FT
    elseif(pm==pm_ba) THEN
        max_result=pm_ba_4+FT-1
        iresult=pm_ba_1-1+ft
    else
        er_message="clreport_summing:1 - PM of " // trim(itos(pm)) //" (" // trim(pm2str(PM)) // ")" // &
        " is unknown or not supported."
        call end_program(er_message)
    endif
    
    if(iresult>max_result) THEN
        er_message="clreport_summing:4 - get_calculated_lm returning invalid (high) result for " // trim(pm2str(pm))
        call end_program(er_message)
    endif
    
    if(.false.) then
        ! debug - not needed in production
        inquire(file=filename, exist=fe)
        file_handle=690
        if(fe) then
            open(unit=file_handle, iostat=istat, file=trim(filename), &
                status="OLD", &
                access="SEQUENTIAL", action="write", position="APPEND")
        else
            open(unit=file_handle, iostat=istat, file=trim(filename), &
                 action="write", status="NEW")
        endif
        
        if (istat/=0) THEN
            stop
        endif
        
        write(file_handle, *) "result: ", iresult
        close(file_handle)
    endif
    
    ! end debug - not needed in production
    get_calculated_lm=iresult
    ! log_message=trim(log_message)//" ... returning " // trim(itos(iresult))
    ! call write_log_entry("get_calculated_lm:1", log_message)
    


end function get_calculated_lm


subroutine sum_annuals(TG, firstpm_idx, lastpm_idx, handle_steam, year, BASE_YEAR, CL_MONTH_NAME)
integer :: PM, FT, LM
real :: year, base_year
integer, intent(in) :: TG, lastpm_idx, firstpm_idx
logical, intent(in) :: handle_steam
character*9 :: CL_MONTH_NAME(14)
integer :: upper_bound_rank_2
logical :: check_dims=.true.
real :: this_value, that_value


    
    
      DO PM = firstpm_idx, lastpm_idx
      if (.not. clreport_seed_with_sample_data) then
          upper_bound_rank_2=ubound(ANNUAL_PROD_BY_TG_BY_MW,2)
          
          if(check_dims) then
              if(PM>upper_bound_rank_2) THEN
                call array_error("clreport_summing:2", &
                "ANNUAL_PROD_BY_TG_BY_MW", &
                upper_bound_rank_2, int(PM))
              endif
              
              upper_bound_rank_2=ubound(PROD_BY_TG_BY_MW,2)
              if (pm>upper_bound_rank_2) THEN
                call array_error("clreport_summing:3", &
                "PROD_BY_TG_BY_MW", &
                upper_bound_rank_2, int(PM))
              endif
         endif

          ANNUAL_PROD_BY_TG_BY_MW(TG,PM) = &
              MAX(ANNUAL_PROD_BY_TG_BY_MW(TG,PM), &
                               PROD_BY_TG_BY_MW(TG,PM))
							   

      endif
      
      IF(PM == pm_st .and. handle_steam) THEN ! STEAM
          DO FT = idx_steam_coal, idx_steam_oil ! 1-3
              
              LM=get_calculated_lm(int(PM), int(FT))

              
              ANNUAL_PROD_BY_TG_BY_MW(TG,LM) = &
                  MAX(ANNUAL_PROD_BY_TG_BY_MW(TG,LM), &
                  PROD_BY_TG_BY_MW(TG,LM))
            if (clreport_seed_with_sample_data) then
                ANNUAL_PROD_BY_TG_BY_MW(TG, LM)=LM
            endif


          END DO
          
          
      ENDIF
      
        

    ENDDO
end subroutine sum_annuals
subroutine seed_clreport(year, BASE_YEAR, iseas, CL_MONTH_NAME, I, MULTI_AREA_NAME, MAX_LOAD_TYPES, &
    MFT1)
integer :: upb, upb2
integer*2 :: iseas, I, MAX_LOAD_TYPES, MFT1
real :: year, base_year
character*9 :: CL_MONTH_NAME(14)
character*35, allocatable ::  multi_area_name(:)
integer :: i_index, j_index, out_index
character*255 :: text_output_filename="clreport_summing.log"
character*255 :: index_num_str

end subroutine seed_clreport

end module clreport_summing
