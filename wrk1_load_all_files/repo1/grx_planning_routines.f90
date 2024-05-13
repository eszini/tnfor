!--------
      MODULE GRX_PLANNING_ROUTINES
      USE NEWTON_INTERFACE
      USE PRODUCTION_ARRAYS_AND_DIMENSIONS
      INTEGER (KIND=4), SAVE :: GRX_ITERATIONS
      REAL (KIND=4), SAVE :: CURRENT_CO2_DISPATCH_COST/0./, &
                          NEXT_CURRENT_CO2_DISPATCH_COST/0./, &
                          MRX_RPS_CL_CURVE_GROSS_MARGIN(MAX_CL_UNITS), &
                          MRX_RPS_CL_CURVE_REVENUE(MAX_CL_UNITS), &
                          MRX_RPS_CL_CURVE_COST(MAX_CL_UNITS), &
                          MRX_RPS_CL_CURVE_UMWH(MAX_CL_UNITS)
      REAL (KIND=4), SAVE, ALLOCATABLE :: SAVED_CL_ANN_CAP(:,:,:), &
                                          SAVED_CL_TG_CAP(:,:,:,:), &
                                   MRX_RPS_CL_CURVE_MWH(:,:), &
                                   MRX_RPS_CL_CURVE_CUM_MWH(:,:), &
                                   MRX_RPS_CL_CURVE_MARGIN(:,:), &
                                   MRX_RPS_CL_CURVE_CUM_MARGIN(:,:), &
                                   MRX_RPS_DV_CURVE_MWH(:,:), &
                                   MRX_RPS_DV_CURVE_CUM_MWH(:,:), &
                                   MRX_RPS_DV_CURVE_MARGIN(:,:), &
                                   MRX_RPS_DV_CURVE_CUM_MARGIN(:,:), &
! 062020. ALLOCATED IN CLREPORT.
                                   MRX_RPS_DV_CURVE_GROSS_MARGIN(:), &
                                   MRX_RPS_DV_CURVE_REVENUE(:), &
                                   MRX_RPS_DV_CURVE_COST(:), &
                                   MRX_RPS_DV_CURVE_UMWH(:),&
                                   MRX_RPS_MX_CURVE_MARGIN(:,:),&
                                   MRX_RPS_MX_CURVE_REVENUE(:,:),&
                                   MRX_RPS_MX_CURVE_COST(:,:), &
                                   MRX_RPS_MX_CURVE_UMWH(:,:), &
                                   MRX_RPS_MX_CURVE_MWH(:,:), &
                                   RPS_REC_PRICE_BY(:), &
                                   RPS_GEN_PRICE_BY(:)
      INTEGER (KIND=2),SAVE,ALLOCATABLE :: MRX_RPS_CL_CURVE_INDEX(:,:), &
                                           MRX_RPS_CL_UNIT_NO(:), &
                                           MRX_RPS_CL_UNIT_INDEX(:,:), &
                                           MRX_RPS_DV_CURVE_INDEX(:,:), &
                                           MRX_RPS_DV_UNIT_NO(:), &
                                           MRX_RPS_DV_UNIT_INDEX(:,:), &
                                           MRX_RPS_MX_OPTION_NO(:), &
                                           MRX_RPS_MX_CURVE_INDEX(:,:), &
                                           MRX_RPS_MX_OPTION_INDEX(:,:), &
                                           MRX_RPS_MX_NEXT_CN_INDEX(:,:)
      REAL (KIND=4),SAVE, ALLOCATABLE :: GRX_CO2_EMISS_REDUCTION_REQUIRED(:)
      LOGICAL (KIND=4), PARAMETER :: STORE=.TRUE.,RESTORE=.FALSE.
      REAL (KIND=4),SAVE, ALLOCATABLE :: GRX_ITER_CO2_PRICE(:,:), &
                                           GRX_ITER_TG_CO2_EMISS(:,:)
      INTEGER (KIND=2), SAVE :: TG_MAX,GRX_NUNITS
      LOGICAL (KIND=4), SAVE :: GRX_CONVERGED
      INTEGER (KIND=2), SAVE :: GRX_MAXIMUM_ITERATIONS
      LOGICAL (KIND=1), SAVE :: BALANCE_WITH_CO2_PRICE/.FALSE./
!      INTEGER (KIND=8), SAVE :: HESI_UNIT_ID_NUM(MAX_CL_UNITS)
      INTEGER (KIND=8), SAVE :: H2_UNIT_ID_NUM(MAX_CL_UNITS)
      LOGICAL (KIND=1), ALLOCATABLE :: GRX_BOP_RETROFIT_ACTIVE(:)
      INTEGER (KIND=2), SAVE :: RETROFIT_LIST(MAX_CL_UNITS),UNITS_RETROFITTED/0/, &
                                RETROFIT_TYPE_FIXED_GRX(MAX_CL_UNITS)
      REAL (KIND=4), SAVE :: RETROFIT_CO2_DECISION_COST(MAX_CL_UNITS)
      INTEGER (KIND=2), SAVE :: RETIRED_OPTIONS_PNTR/0/
      REAL (KIND=4), PARAMETER :: CO2_ERROR_TRAGET=.02
      INTEGER (KIND=2), SAVE :: GRX_SAVED_ENDPOINT,GRX_PRT_ENDPOINT, &
                                RETIRE_RETRO_COUNTER
      REAL (KIND=4), SAVE :: CO2_CAP_INCREASE_FROM_MARKET/0/, &
                             CO2_CAP_EXPENDITURES/0/
      INTEGER (KIND=2) :: GRX_CO2_MARKET_PTS_USED/0/,GRX_CO2_MARKET_PTS_USED_SAVED/0/
      LOGICAL (KIND=4) :: IS_CO2_PRICE_MRKT_CAPPED/.FALSE./
      REAL (KIND=4) :: LAST_CO2_MARKET_PRICE/0./
      INTEGER (KIND=2) :: GRX_UNIT_COUNT(100,-1:20)
      INTEGER (KIND=2), SAVE :: RETRO_POINTER/0/
CONTAINS
!--------
        FUNCTION CHECK_FOR_GRX_ABATEMENT_CONVERGENCE(TG)
        USE co2_market_abatement_curves
        USE CO2_CAP_N_TRADE
        USE CLA_OBJT_ARRAYS
        REAL (KIND=4), SAVE :: ITER_CO2_PRICE(0:20), ITER_CO2_ELEC_EMISS(0:20)
        REAL (KIND=4), SAVE  :: ABOVE_PRICE,ABOVE_EMISS,BELOW_PRICE,BELOW_EMISS
        LOGICAL (KIND=4) :: CHECK_FOR_GRX_ABATEMENT_CONVERGENCE
        LOGICAL (KIND=4), SAVE :: MaxPriceCapHit, MinPriceHit, BELOW_NOT_FOUND
        INTEGER (KIND=2) :: TG,I,GET_NUNITS,NumUnits
        REAL (KIND=4) :: GRX_NON_ELECTRIC_SECTOR_CO2_EMISSIONS, GET_CO2_EMISS_FOR_SYSTEM, &
                         CO2_EMISSIONS_CAP, CO2_EMISSIONS,CAP_ERROR, &
                         GRX_CO2_ABATEMENTS_AVAILABLE,GRX_ALL_SECTOR_CO2_EMISSIONS_CAP, &
                         EMISSION_ADJ,ADJ_SLOPE
        REAL (KIND=4), SAVE :: LAST_PRICE/0./,CURRENT_PRICE/0./, &
                                LAST_CO2_EMISS/0./,CURRENT_CO2_EMISS/0./
        CHARACTER (LEN=2) CAPACITY_PLANNING_METHOD,GREEN_MRX_METHOD
        REAL (KIND=4) :: GRX_CO2_MARKET_TONS,GRX_CO2_MARKET_PRICE,MARKET_REDUCTION,MAX_ABATEMENTS_AVAIL
        REAL (KIND=4), SAVE :: LOW_PRICE_VALUE/9999./,HIGH_PRICE_VALUE/0./, &
                               CO2_ABATEMENTS_NEEDED_LAST_ITER
        REAL (KIND=4) :: CO2_ELECTRIC_EMISSION,CO2_NON_ELECTRIC_EMISSION, &
                         CO2_ABATEMENTS_NEEDED,GRX_ABATEMENT_PRICE,CO2_NET_EMISSIONS, &
                         GRX_CO2_BANK_DELTA,CO2_BANK_CREDITS_USED,CO2_BANK_CREDITS_PURCHASED, &
                         CO2_BANK_CAP_ADJUSTMENT,CO2_SHORT_FALL,CO2_SHORT_FALL_COVERED_BY_BANK
        INTEGER*2 BASE_YEAR,YEAR,END_POINT,STUDY_PERIOD,ENDYR, &
                LAST_STUDY_YEAR,EXTENSION_PERIOD,LAST_EXTENSION_YEAR, &
               MAX_YEARS
        COMMON /GLOBAL_VARIABLES/ BASE_YEAR,YEAR,END_POINT,STUDY_PERIOD, &
               LAST_STUDY_YEAR,EXTENSION_PERIOD,LAST_EXTENSION_YEAR, &
               ENDYR,MAX_YEARS

          IF(.NOT. (CAPACITY_PLANNING_METHOD() == 'MX'  .AND.   &
                                  GREEN_MRX_METHOD() == 'GX') .OR.  &
                                                         YEAR == 1) THEN
             CHECK_FOR_GRX_ABATEMENT_CONVERGENCE = .TRUE.
             GRX_CONVERGED = .TRUE.
             RETURN
          ENDIF
          CHECK_FOR_GRX_ABATEMENT_CONVERGENCE = .FALSE.
          GRX_CONVERGED = .FALSE.
          CO2_EMISSIONS_CAP = GRX_ALL_SECTOR_CO2_EMISSIONS_CAP(BASE_YEAR+YEAR)
          CO2_ELECTRIC_EMISSION = GET_CO2_EMISS_FOR_SYSTEM()
          CO2_NON_ELECTRIC_EMISSION = GRX_NON_ELECTRIC_SECTOR_CO2_EMISSIONS(BASE_YEAR+YEAR)
          CO2_EMISSIONS = CO2_ELECTRIC_EMISSION &
                          + CO2_NON_ELECTRIC_EMISSION
! bank credits used increase the co2 cap
! bank credits purchased are added at the end at the cost of the credits bought to meet cap
! NOTE: version 1.0 of banking dated 12/11/09 does not check for max and min credit market limits on bank purchases
          CO2_BANK_CREDITS_USED = ABS(MIN(0,GRX_CO2_BANK_DELTA(BASE_YEAR+YEAR))) ! negative term when bank is used for offset.
          CO2_BANK_CREDITS_PURCHASED = MAX(0,GRX_CO2_BANK_DELTA(BASE_YEAR+YEAR)) ! positive term when bank purchases offsets
          CO2_BANK_CAP_ADJUSTMENT = GRX_CO2_BANK_DELTA(BASE_YEAR+YEAR) ! positive term when bank purchases offsets
          MAX_ABATEMENTS_AVAIL = 0.
          IF(CO2_Market_Available(BASE_YEAR+YEAR)) &
          MAX_ABATEMENTS_AVAIL = 10**6*(Non_Elect_Max_Reductions(BASE_YEAR+YEAR) &
                                        + Dom_Max_Off_Sets_To_Buy(BASE_YEAR+YEAR) &
                                        + Int_Max_Off_Sets_To_Buy(BASE_YEAR+YEAR))
          CURRENT_CO2_EMISS = CO2_EMISSIONS - CO2_BANK_CREDITS_USED
          CURRENT_PRICE = CURRENT_CO2_DISPATCH_COST
          write(9,*) "in GRX conver"
          write(9,*) "Emiss CAP", CO2_EMISSIONS_CAP
          write(9,*) "Emiss Prod", CO2_EMISSIONS
          write(9,*) "Current Price", CURRENT_CO2_DISPATCH_COST
          IF(BASE_YEAR+YEAR == 2023) then
             write(9,*) "at 2018"
          endif
          IF(GRX_ITERATIONS == 0) THEN
            ITER_CO2_PRICE = 0.
            ITER_CO2_ELEC_EMISS = 0.
            ABOVE_PRICE = 0.
            ABOVE_EMISS = 0.
            BELOW_PRICE = 0.
            BELOW_EMISS = 0.
            BELOW_NOT_FOUND = .TRUE.
          ENDIF
          ITER_CO2_PRICE(GRX_ITERATIONS) = CURRENT_CO2_DISPATCH_COST
          ITER_CO2_ELEC_EMISS(GRX_ITERATIONS) = CO2_ELECTRIC_EMISSION
          IF(CO2_Price_Evaluation(BASE_YEAR+YEAR)) THEN
            IF(GRX_ITERATIONS < CO2_Min_GRX_Iterations(BASE_YEAR+YEAR)) THEN
               NEXT_CURRENT_CO2_DISPATCH_COST = CO2_Price_Forecast(BASE_YEAR+YEAR)
            ELSE ! if(GRX_ITERATIONS == 3.) THEN
               CHECK_FOR_GRX_ABATEMENT_CONVERGENCE = .TRUE.
               GRX_CONVERGED = .TRUE.
               RETIRED_OPTIONS_PNTR = 0
               CALL GRX_ABATEMENT_MARKET_DATA_RPT()
            ENDIF
            CALL GRX_ABATE_ITER_RESULTS()
            RETURN
          ENDIF
          IF(GRX_ITERATIONS == 0) THEN
             CO2_ABATEMENTS_NEEDED_LAST_ITER = CO2_EMISSIONS - CO2_BANK_CREDITS_USED &
                                              - CO2_EMISSIONS_CAP + CO2_BANK_CREDITS_PURCHASED
             CO2_ABATEMENTS_NEEDED_LAST_ITER = 1.1 * CO2_ABATEMENTS_NEEDED_LAST_ITER
             IF(CO2_Market_Available(BASE_YEAR+YEAR)) THEN
                NEXT_CURRENT_CO2_DISPATCH_COST = MAX(CO2_Min_Price(BASE_YEAR+YEAR), &
                   GRX_ABATEMENT_PRICE(CO2_ABATEMENTS_NEEDED_LAST_ITER,BASE_YEAR+YEAR))
                IF(CURRENT_CO2_EMISS > CO2_EMISSIONS_CAP) THEN
                   ABOVE_PRICE = CURRENT_CO2_DISPATCH_COST
                   ABOVE_EMISS = CURRENT_CO2_EMISS
                ELSE
                   BELOW_PRICE = CURRENT_CO2_DISPATCH_COST
                   BELOW_EMISS = CURRENT_CO2_EMISS
                ENDIF
             ELSE
               IF(CURRENT_CO2_EMISS > CO2_EMISSIONS_CAP) THEN
                 NEXT_CURRENT_CO2_DISPATCH_COST = (CO2_Min_Price(BASE_YEAR+YEAR)  &
                                                + CO2_Max_Price_Cap(BASE_YEAR+YEAR))/2.
                 ABOVE_PRICE = CURRENT_CO2_DISPATCH_COST
                 ABOVE_EMISS = CO2_ELECTRIC_EMISSION
               ELSE
                 NEXT_CURRENT_CO2_DISPATCH_COST = CO2_Min_Price(BASE_YEAR+YEAR)
                 BELOW_PRICE = CURRENT_CO2_DISPATCH_COST
                 BELOW_EMISS = CO2_ELECTRIC_EMISSION
               ENDIF
             ENDIF
             CALL GRX_ABATE_ITER_RESULTS()
             LAST_PRICE = CURRENT_CO2_DISPATCH_COST
             LAST_CO2_EMISS = CO2_EMISSIONS - CO2_BANK_CREDITS_USED
             CURRENT_PRICE = NEXT_CURRENT_CO2_DISPATCH_COST
             MaxPriceCapHit = .FALSE.
             MinPriceHit = .FALSE.
            RETURN
          ENDIF
          IF(.NOT. CO2_Market_Available(BASE_YEAR+YEAR)) THEN
               IF(CURRENT_CO2_EMISS > CO2_EMISSIONS_CAP) THEN
                  ABOVE_PRICE = CURRENT_CO2_DISPATCH_COST
                  ABOVE_EMISS = CO2_ELECTRIC_EMISSION
                  IF(GRX_ITERATIONS == 1) THEN
                     NEXT_CURRENT_CO2_DISPATCH_COST = CO2_Max_Price_Cap(BASE_YEAR+YEAR)
                  ELSEIF(CURRENT_CO2_DISPATCH_COST == CO2_Max_Price_Cap(BASE_YEAR+YEAR)) THEN
                     NEXT_CURRENT_CO2_DISPATCH_COST = CO2_Max_Price_Cap(BASE_YEAR+YEAR)
                     GRX_CONVERGED = GRX_ITERATIONS >= CO2_Min_GRX_Iterations(BASE_YEAR+YEAR)
                  ELSE
                     IF(ABS_ERROR(CO2_EMISSIONS_CAP,CURRENT_CO2_EMISS,DELTA=.01) .AND. &
                            GRX_ITERATIONS >= CO2_Min_GRX_Iterations(BASE_YEAR+YEAR)) THEN
                        NEXT_CURRENT_CO2_DISPATCH_COST = CURRENT_CO2_DISPATCH_COST
                        GRX_CONVERGED = .TRUE.
                     ELSE
                        NEXT_CURRENT_CO2_DISPATCH_COST = (BELOW_PRICE + ABOVE_PRICE)/2.
                     ENDIF
                  ENDIF
               ELSE
                  BELOW_PRICE = CURRENT_CO2_DISPATCH_COST
                  BELOW_EMISS = CO2_ELECTRIC_EMISSION
                  IF(CURRENT_CO2_DISPATCH_COST == CO2_Min_Price(BASE_YEAR+YEAR)) THEN
                     NEXT_CURRENT_CO2_DISPATCH_COST = CO2_Min_Price(BASE_YEAR+YEAR)
                     GRX_CONVERGED = GRX_ITERATIONS >= CO2_Min_GRX_Iterations(BASE_YEAR+YEAR)
                  ELSE
                     IF(ABS_ERROR(CO2_EMISSIONS_CAP,CURRENT_CO2_EMISS,DELTA=.01) .AND. &
                            GRX_ITERATIONS >= CO2_Min_GRX_Iterations(BASE_YEAR+YEAR)) THEN
                        NEXT_CURRENT_CO2_DISPATCH_COST = CURRENT_CO2_DISPATCH_COST
                        GRX_CONVERGED = .TRUE.
                     ELSE
                        NEXT_CURRENT_CO2_DISPATCH_COST = (BELOW_PRICE + ABOVE_PRICE)/2.
                     ENDIF
                  ENDIF
               ENDIF
               LAST_CO2_EMISS = CURRENT_CO2_EMISS
               LAST_PRICE = CURRENT_CO2_DISPATCH_COST
               CALL GRX_ABATE_ITER_RESULTS()
               IF(GRX_CONVERGED) RETIRED_OPTIONS_PNTR = 0
               IF(GRX_CONVERGED) CALL GRX_ABATEMENT_MARKET_DATA_RPT()
               CHECK_FOR_GRX_ABATEMENT_CONVERGENCE = GRX_CONVERGED
               RETURN
          ENDIF
          IF(CO2_Market_Available(BASE_YEAR+YEAR)) THEN
               CURRENT_CO2_EMISS =  CURRENT_CO2_EMISS &
                        - GRX_CO2_ABATEMENTS_AVAILABLE(CURRENT_CO2_DISPATCH_COST,BASE_YEAR+YEAR)
               IF(CURRENT_CO2_EMISS > CO2_EMISSIONS_CAP) THEN
                  ABOVE_PRICE = CURRENT_CO2_DISPATCH_COST
                  ABOVE_EMISS = CO2_ELECTRIC_EMISSION
                  IF(ABS_ERROR(CO2_EMISSIONS_CAP,CURRENT_CO2_EMISS,DELTA=.01) .AND. &
                             GRX_ITERATIONS >= CO2_Min_GRX_Iterations(BASE_YEAR+YEAR)) THEN
                     NEXT_CURRENT_CO2_DISPATCH_COST = CURRENT_CO2_DISPATCH_COST
                     GRX_CONVERGED = .TRUE.
                  ELSE
                     IF(GRX_ITERATIONS == 1 .OR. BELOW_NOT_FOUND) THEN
                        CO2_ABATEMENTS_NEEDED_LAST_ITER = CO2_EMISSIONS - CO2_BANK_CREDITS_USED &
                                              - CO2_EMISSIONS_CAP + CO2_BANK_CREDITS_PURCHASED
                        CO2_ABATEMENTS_NEEDED_LAST_ITER = 1.05 * CO2_ABATEMENTS_NEEDED_LAST_ITER
                        NEXT_CURRENT_CO2_DISPATCH_COST = MAX(CO2_Min_Price(BASE_YEAR+YEAR), &
                            GRX_ABATEMENT_PRICE(CO2_ABATEMENTS_NEEDED_LAST_ITER,BASE_YEAR+YEAR))
                     ELSEIF(CURRENT_CO2_DISPATCH_COST == CO2_Max_Price_Cap(BASE_YEAR+YEAR)) THEN
                        NEXT_CURRENT_CO2_DISPATCH_COST = CO2_Max_Price_Cap(BASE_YEAR+YEAR)
                        GRX_CONVERGED = GRX_ITERATIONS >= CO2_Min_GRX_Iterations(BASE_YEAR+YEAR)
                     ELSE
                        NEXT_CURRENT_CO2_DISPATCH_COST = (BELOW_PRICE + ABOVE_PRICE)/2.
                     ENDIF
                  ENDIF
               ELSE
                  BELOW_PRICE = CURRENT_CO2_DISPATCH_COST
                  BELOW_EMISS = CO2_ELECTRIC_EMISSION
                  BELOW_NOT_FOUND = .FALSE.
                  IF(CURRENT_CO2_DISPATCH_COST == CO2_Min_Price(BASE_YEAR+YEAR)) THEN
                     NEXT_CURRENT_CO2_DISPATCH_COST = CO2_Min_Price(BASE_YEAR+YEAR)
                     GRX_CONVERGED = GRX_ITERATIONS >= CO2_Min_GRX_Iterations(BASE_YEAR+YEAR)
                  ELSE
                     NEXT_CURRENT_CO2_DISPATCH_COST = (BELOW_PRICE + ABOVE_PRICE)/2.
                  ENDIF
               ENDIF
               LAST_CO2_EMISS = CURRENT_CO2_EMISS
               LAST_PRICE = CURRENT_CO2_DISPATCH_COST
               CALL GRX_ABATE_ITER_RESULTS()
               IF(GRX_CONVERGED) RETIRED_OPTIONS_PNTR = 0
               IF(GRX_CONVERGED) CALL GRX_ABATEMENT_MARKET_DATA_RPT()
               CHECK_FOR_GRX_ABATEMENT_CONVERGENCE = GRX_CONVERGED
               RETURN
          ENDIF
        END FUNCTION
!--------
        FUNCTION CHECK_FOR_GRX_CONVERGENCE(TG)
        USE co2_market_abatement_curves
        LOGICAL (KIND=4) :: CHECK_FOR_GRX_CONVERGENCE,OneMoreIter
        INTEGER (KIND=2) :: TG,I
        REAL (KIND=4) :: GET_EMISS_CAP_FOR_CLASS, GET_CO2_EMISS_FOR_SYSTEM, &
                         CO2_EMISSIONS_CAP, CO2_EMISSIONS,CAP_ERROR, &
                         ADD_MARKET_PURCHASES
!        LOGICAL (KIND=4) :: ABS_ERROR
        LOGICAL (KIND=1) :: GET_CO2_RETIREMENTS_LOGIC,CO2_RETROFIT_LOGIC_ACTIVE
        CHARACTER (LEN=2) CAPACITY_PLANNING_METHOD,GREEN_MRX_METHOD
        REAL (KIND=4) :: GRX_CO2_MARKET_TONS,GRX_CO2_MARKET_PRICE,MARKET_REDUCTION
        REAL (KIND=4), SAVE :: LOW_PRICE_VALUE/9999./,HIGH_PRICE_VALUE/0./
        LOGICAL (KIND=1), SAVE :: HIGH_PRICE_FOUND,LOW_PRICE_FOUND,MIN_MAX_SET

          IF(.NOT. (CAPACITY_PLANNING_METHOD() == 'MX'  .AND.   &
                                          GREEN_MRX_METHOD() == 'GX')) THEN
             CHECK_FOR_GRX_CONVERGENCE = .TRUE.
             GRX_CONVERGED = .TRUE.
             RETURN
          ENDIF
          CHECK_FOR_GRX_CONVERGENCE = .FALSE.
          GRX_CONVERGED = .FALSE.
          CO2_EMISSIONS_CAP = GET_EMISS_CAP_FOR_CLASS(3_2,TG)
          CO2_EMISSIONS = GET_CO2_EMISS_FOR_SYSTEM()
          IS_CO2_PRICE_MRKT_CAPPED = .FALSE.
          write(9,*) "in GRX conver"
          write(9,*) "Emiss CAP", CO2_EMISSIONS_CAP
          write(9,*) "Emiss Prod", CO2_EMISSIONS
          write(9,*) "CO2 Cedits (tons)", CO2_CAP_INCREASE_FROM_MARKET
          IF(GRX_ITERATIONS == 0) THEN
             OneMoreIter = .FALSE.
             HIGH_PRICE_FOUND = .FALSE.
             LOW_PRICE_FOUND = .FALSE.
             MIN_MAX_SET = .FALSE.
             IF(CO2_EMISSIONS <= CO2_EMISSIONS_CAP) THEN
               LOW_PRICE_VALUE = CURRENT_CO2_DISPATCH_COST/4.
               HIGH_PRICE_VALUE = NEWTON_MRKT_PRICE_SEARCH(MAX_POINT=CURRENT_CO2_DISPATCH_COST, &
                                                           MIN_POINT=LOW_PRICE_VALUE, &
                                                           MAX_CO2=CO2_EMISSIONS, &
                                                           CO2_CAP=CO2_EMISSIONS_CAP)
               NEXT_CURRENT_CO2_DISPATCH_COST = LOW_PRICE_VALUE
               HIGH_PRICE_FOUND = .TRUE.
             ELSE  ! CO2_EMISSIONS > CO2_EMISSIONS_CAP UP THE CO2 PRICE
               HIGH_PRICE_VALUE = MAX(4.*CURRENT_CO2_DISPATCH_COST,20.)
               NEXT_CURRENT_CO2_DISPATCH_COST = NEWTON_MRKT_PRICE_SEARCH( &
                                                         MIN_POINT=CURRENT_CO2_DISPATCH_COST, &
                                                         MAX_POINT=HIGH_PRICE_VALUE, &
                                                         MIN_CO2=CO2_EMISSIONS, &
                                                         CO2_CAP=CO2_EMISSIONS_CAP)
               NEXT_CURRENT_CO2_DISPATCH_COST = HIGH_PRICE_VALUE
               LOW_PRICE_FOUND = .TRUE.
             ENDIF
             CALL GRX_ITER_RESULTS(0_2)
            RETURN
          ENDIF
          IF(GRX_ITERATIONS == 1 .AND. CURRENT_CO2_DISPATCH_COST == 0. .AND. &
                                               CO2_EMISSIONS < CO2_EMISSIONS_CAP) THEN
             CHECK_FOR_GRX_CONVERGENCE = .TRUE.  ! THEN MARKET WAS THEN LAST POINT USED
             GRX_CONVERGED = .TRUE.
             CALL GRX_ITER_RESULTS(0_2)
             RETIRED_OPTIONS_PNTR = 0
             RETURN
          ENDIF
          IF(GRX_CO2_MARKET_PTS_USED > 0 .AND. CO2_EMISSIONS > CO2_EMISSIONS_CAP) THEN
            CO2_CAP_INCREASE_FROM_MARKET = 10**6 * &
                                SUM(CO2_ABATE_VOLUMS(1:GRX_CO2_MARKET_PTS_USED))
            IF(CO2_EMISSIONS-CO2_CAP_INCREASE_FROM_MARKET <= CO2_EMISSIONS_CAP) THEN
               MARKET_REDUCTION = 0.
               CO2_CAP_EXPENDITURES = 0.
               DO I = 1, GRX_CO2_MARKET_PTS_USED
                  MARKET_REDUCTION = MARKET_REDUCTION + 10**6 * CO2_ABATE_VOLUMS(I)
                  IF(CO2_EMISSIONS-MARKET_REDUCTION <= CO2_EMISSIONS_CAP) THEN
                     IF(I == GRX_CO2_MARKET_PTS_USED) THEN
                        CO2_CAP_INCREASE_FROM_MARKET = CO2_EMISSIONS - CO2_EMISSIONS_CAP
                        IF(CURRENT_CO2_DISPATCH_COST == CO2_ABATE_PRICES(I)) THEN
                           CHECK_FOR_GRX_CONVERGENCE = .TRUE.  ! THEN MARKET WAS THEN LAST POINT USED
                           GRX_CONVERGED = .TRUE.
                           RETIRED_OPTIONS_PNTR = 0
                           MARKET_REDUCTION = MARKET_REDUCTION - 10**6 * CO2_ABATE_VOLUMS(I)
                           CO2_CAP_EXPENDITURES = CO2_CAP_EXPENDITURES &
                                       + (CO2_CAP_INCREASE_FROM_MARKET - MARKET_REDUCTION) &
                                                    * CO2_ABATE_PRICES(I)/10.**6
                        ELSE ! CO2 PRICE ABOVE LAST MARKET PRICE WERE ALL THEN LAST PURCHASES USED
                           IF(ABS_ERROR(CO2_EMISSIONS_CAP, &
                                                    CO2_EMISSIONS-MARKET_REDUCTION,DELTA=.01)) THEN
                              CHECK_FOR_GRX_CONVERGENCE = .TRUE.  ! THEN MARKET WAS THEN LAST POINT USED
                              GRX_CONVERGED = .TRUE.
                              RETIRED_OPTIONS_PNTR = 0
                              CO2_CAP_EXPENDITURES = CO2_CAP_EXPENDITURES &
                                           + CO2_ABATE_VOLUMS(I) * CO2_ABATE_PRICES(I)
                           ELSE !
                              NEXT_CURRENT_CO2_DISPATCH_COST = &
                                   NEWTON_MRKT_PRICE_SEARCH(MAX_POINT=CURRENT_CO2_DISPATCH_COST, &
                                                            MAX_CO2=CO2_EMISSIONS, &
                                                         CO2_CAP=CO2_EMISSIONS_CAP+MARKET_REDUCTION)
                              LOW_PRICE_FOUND = .TRUE.
                              HIGH_PRICE_FOUND = .TRUE.
                              MIN_MAX_SET = .TRUE.
                           ENDIF
                        ENDIF
                     ELSE ! PRICE TOO HIGH TOO USE ALL OF MARKET
                        NEXT_CURRENT_CO2_DISPATCH_COST = &
                                   NEWTON_MRKT_PRICE_SEARCH(MAX_POINT=CURRENT_CO2_DISPATCH_COST, &
                                                            MAX_CO2=CO2_EMISSIONS, &
                                                            MIN_POINT= CO2_ABATE_PRICES(I), &
                                                         CO2_CAP=CO2_EMISSIONS_CAP+MARKET_REDUCTION)
                     ENDIF
                     LAST_CO2_MARKET_PRICE = NEXT_CURRENT_CO2_DISPATCH_COST
                     IF(GRX_CONVERGED) THEN
                        CALL GRX_ITER_RESULTS(0_2,CONVER_CREDITS_USED=CO2_CAP_INCREASE_FROM_MARKET, &
                                              CONVER_CREDITS_COST=CO2_CAP_EXPENDITURES)
                     ELSE
                        CALL GRX_ITER_RESULTS(0_2)
                     ENDIF
                     EXIT
                  ENDIF
                  CO2_CAP_EXPENDITURES = CO2_CAP_EXPENDITURES &
                                         + CO2_ABATE_VOLUMS(I) * CO2_ABATE_PRICES(I)
               ENDDO
            ELSE ! PRICE NOT HIGH ENOUGHT LOOK FOR NEXT MARKET POINT AS NEW PRICE
               IF(ABS_ERROR(CO2_EMISSIONS_CAP, &
                                        CO2_EMISSIONS-CO2_CAP_INCREASE_FROM_MARKET,DELTA=.01)) THEN
                  CHECK_FOR_GRX_CONVERGENCE = .TRUE.  ! THEN MARKET WAS THEN LAST POINT USED
                  GRX_CONVERGED = .TRUE.
                  RETIRED_OPTIONS_PNTR = 0
                  CO2_CAP_EXPENDITURES = DOT_PRODUCT(CO2_ABATE_VOLUMS(1:GRX_CO2_MARKET_PTS_USED), &
                                                        CO2_ABATE_PRICES(1:GRX_CO2_MARKET_PTS_USED))
               ELSE !
                  LOW_PRICE_VALUE = CURRENT_CO2_DISPATCH_COST ! CO2_ABATE_PRICES(GRX_CO2_MARKET_PTS_USED)
                  LOW_PRICE_FOUND = .TRUE.
                  CHECK_FOR_GRX_CONVERGENCE = .FALSE.
                  NEXT_CURRENT_CO2_DISPATCH_COST = &
                        NEWTON_MRKT_PRICE_SEARCH(MIN_POINT=LOW_PRICE_VALUE, &
                                                 MIN_CO2=CO2_EMISSIONS, &
                                                 CO2_CAP=CO2_EMISSIONS_CAP+ &
                                                         CO2_CAP_INCREASE_FROM_MARKET)
                  MIN_MAX_SET = .TRUE.
                  LAST_CO2_MARKET_PRICE = NEXT_CURRENT_CO2_DISPATCH_COST
               ENDIF
               CALL GRX_ITER_RESULTS(0_2)
            ENDIF
            write(9,*) "CO2 Cedits After (tons)", CO2_CAP_INCREASE_FROM_MARKET
            write(9,*) "Next Market Price", NEXT_CURRENT_CO2_DISPATCH_COST
            RETURN
          ELSEIF(GRX_CO2_MARKET_PTS_USED > 0 .AND. CO2_EMISSIONS <= CO2_EMISSIONS_CAP) THEN
! CO2 PRICE TOO HIGH ADJUST TO FIRST MARKET PRICE
             HIGH_PRICE_VALUE = CURRENT_CO2_DISPATCH_COST ! CO2_ABATE_PRICES(GRX_CO2_MARKET_PTS_USED)
             HIGH_PRICE_FOUND = .TRUE.
             IF(LOW_PRICE_FOUND .AND. .NOT. MIN_MAX_SET) THEN
                NEXT_CURRENT_CO2_DISPATCH_COST = &
                     NEWTON_MRKT_PRICE_SEARCH(MAX_POINT=HIGH_PRICE_VALUE, &
                                                 MAX_CO2=CO2_EMISSIONS, &
                                                 CO2_CAP=CO2_EMISSIONS_CAP+ &
                                                         CO2_CAP_INCREASE_FROM_MARKET)
                MIN_MAX_SET = .TRUE.
             ELSEIF(MIN_MAX_SET) THEN
                NEXT_CURRENT_CO2_DISPATCH_COST = &
                     NEWTON_MRKT_PRICE_SEARCH(MAX_POINT=HIGH_PRICE_VALUE, &
                                              MAX_CO2=CO2_EMISSIONS, &
                                              CO2_CAP=CO2_EMISSIONS_CAP+ &
                                                         CO2_CAP_INCREASE_FROM_MARKET)
             ELSE
                NEXT_CURRENT_CO2_DISPATCH_COST =  &
                               CO2_ABATE_PRICES(MAX(1,GRX_CO2_MARKET_PTS_USED-1))
             ENDIF
             CALL GRX_ITER_RESULTS(0_2)
          ELSE
            IF(CO2_EMISSIONS <= CO2_EMISSIONS_CAP) THEN
!                IF(GET_CO2_RETIREMENTS_LOGIC() .OR. CO2_RETROFIT_LOGIC_ACTIVE()) THEN     ! AUG 09
                   CAP_ERROR = (CO2_EMISSIONS_CAP - CO2_EMISSIONS)/CO2_EMISSIONS_CAP
                  CHECK_FOR_GRX_CONVERGENCE = CAP_ERROR <= CO2_ERROR_TRAGET
                  IF(GRX_ITERATIONS > 1 .AND. .NOT. CHECK_FOR_GRX_CONVERGENCE) THEN
                     CHECK_FOR_GRX_CONVERGENCE = &
                           CURRENT_CO2_DISPATCH_COST == 0. .AND. &
                                     GRX_ITER_CO2_PRICE(GRX_ITERATIONS,TG) == 0.
                  ENDIF
 !              ELSE
 !                 CHECK_FOR_GRX_CONVERGENCE = .TRUE.
 !              ENDIF
            ENDIF
            IF(.NOT. CHECK_FOR_GRX_CONVERGENCE) THEN
               IF(GRX_ITERATIONS == 1) THEN
                  IF(CO2_EMISSIONS <= CO2_EMISSIONS_CAP) &
                     NEXT_CURRENT_CO2_DISPATCH_COST =  &
                           NEWTON_MRKT_PRICE_SEARCH(MIN_POINT=.5*CURRENT_CO2_DISPATCH_COST, &
                                                    MAX_POINT=CURRENT_CO2_DISPATCH_COST, &
                                                    MAX_CO2=CO2_EMISSIONS, &
                                                    CO2_CAP=CO2_EMISSIONS_CAP)

                  IF(CO2_EMISSIONS > CO2_EMISSIONS_CAP) &
                     NEXT_CURRENT_CO2_DISPATCH_COST =  &
                           NEWTON_MRKT_PRICE_SEARCH(MIN_POINT=CURRENT_CO2_DISPATCH_COST, &
                                                    MAX_POINT=3.*CURRENT_CO2_DISPATCH_COST, &
                                                    MIN_CO2=CO2_EMISSIONS, &
                                                    CO2_CAP=CO2_EMISSIONS_CAP)
               ELSE
!                  NEXT_CURRENT_CO2_DISPATCH_COST = GRX_CO2_PRICE_ADJ(TG)
                  IF(CO2_EMISSIONS <= CO2_EMISSIONS_CAP) THEN
                     LOW_PRICE_FOUND = .TRUE.
                     NEXT_CURRENT_CO2_DISPATCH_COST =  &
                           NEWTON_MRKT_PRICE_SEARCH(MAX_POINT=CURRENT_CO2_DISPATCH_COST, &
                                                    MAX_CO2=CO2_EMISSIONS, &
                                                    CO2_CAP=CO2_EMISSIONS_CAP)
                  ENDIF

                  IF(CO2_EMISSIONS > CO2_EMISSIONS_CAP) THEN
                     HIGH_PRICE_FOUND = .TRUE.
                     NEXT_CURRENT_CO2_DISPATCH_COST =  &
                           NEWTON_MRKT_PRICE_SEARCH(MIN_POINT=CURRENT_CO2_DISPATCH_COST, &
                                                    MAX_CO2=CO2_EMISSIONS, &
                                                    CO2_CAP=CO2_EMISSIONS_CAP)
                  ENDIF
               ENDIF
            ENDIF
            CALL GRX_ITER_RESULTS(0_2)
          ENDIF
          GRX_CO2_MARKET_PTS_USED_SAVED = GRX_CO2_MARKET_PTS_USED
          CHECK_FOR_GRX_CONVERGENCE = CHECK_FOR_GRX_CONVERGENCE .OR. &
                                        GRX_ITERATIONS >= GRX_ITERATIONS
          GRX_CONVERGED = CHECK_FOR_GRX_CONVERGENCE
          write(9,*) "GRX Convergence", GRX_CONVERGED
          IF(GRX_CONVERGED) THEN
!             NEXT_CURRENT_CO2_DISPATCH_COST = CURRENT_CO2_DISPATCH_COST
             RETIRED_OPTIONS_PNTR = 0
          ENDIF
        END FUNCTION
!--------
        FUNCTION ITERATE_GRX_PLANNING()
        LOGICAL (KIND=4) :: ITERATE_GRX_PLANNING
        CHARACTER (LEN=2) :: GREEN_MRX_METHOD
           ITERATE_GRX_PLANNING = GREEN_MRX_METHOD() == 'GX'
        END FUNCTION
!--------
        FUNCTION MAX_GRX_ITERATIONS(YR)
        USE CO2_CAP_N_TRADE
        INTEGER (KIND=2):: MAX_GRX_ITERATIONS,YR
           MAX_GRX_ITERATIONS = 10
           IF(ALLOCATED(CO2_Max_GRX_Iterations)) THEN
              MAX_GRX_ITERATIONS = CO2_Max_GRX_Iterations(YR)
           ENDIF
           GRX_MAXIMUM_ITERATIONS = MAX_GRX_ITERATIONS
        END FUNCTION
!--------
        SUBROUTINE GRX_CAPACITY_RETIRE_OPTIONS(R_UNIT_NO,GROSS_MARGIN)
         USE CLA_OBJT_ARRAYS
         USE PRODUCTION_ARRAYS_AND_DIMENSIONS
         REAL (KIND=4) :: GROSS_MARGIN
         INTEGER (KIND=2) :: R_UNIT_NO
!
         INTEGER (KIND=2) :: I
         INTEGER (KIND=2), SAVE :: TEMP_OPTIONS_RETIRED_UNIT_NO(MAX_CL_UNITS)
         REAL (KIND=4), SAVE :: TEMP_OPTIONS_RETIREMENTS_CO2(max_retirement_units), &
                                TEMP_OPTIONS_RETIREMENTS_CO2_COST(max_retirement_units), &
                                TEMP_OPTIONS_RETIREMENTS_CAP_COST(max_retirement_units), &
                                TEMP_OPTIONS_RETIREMENTS_MW(max_retirement_units)
         REAL (KIND=4), SAVE :: TEMP_OPTIONS_RETIREMENTS_MWH(max_retirement_units), &
                                TEMP_OPTIONS_RETIRE_GROSS_MARGIN(max_retirement_units)
!         INTEGER (KIND=2) :: OFLINE
!         COMMON/FOSHYDI2/ OFLINE(MAX_CL_UNITS)
!
          IF(GRX_ITERATIONS == 0) THEN
           RETIRED_OPTIONS_PNTR = 1 + RETIRED_OPTIONS_PNTR
           TEMP_OPTIONS_RETIRED_UNIT_NO(R_UNIT_NO) = RETIRED_OPTIONS_PNTR
           TEMP_OPTIONS_RETIREMENTS_CO2(RETIRED_OPTIONS_PNTR) = RETIREMENTS_CO2(R_UNIT_NO)
           TEMP_OPTIONS_RETIREMENTS_CO2_COST(RETIRED_OPTIONS_PNTR) = RETIREMENTS_CO2_COST(R_UNIT_NO)
           TEMP_OPTIONS_RETIREMENTS_CAP_COST(RETIRED_OPTIONS_PNTR) = RETIREMENTS_CAP_COST(R_UNIT_NO)
           TEMP_OPTIONS_RETIREMENTS_MW(RETIRED_OPTIONS_PNTR) = RETIREMENTS_MW(R_UNIT_NO)
           TEMP_OPTIONS_RETIREMENTS_MWH(RETIRED_OPTIONS_PNTR) = RETIREMENTS_MWH(R_UNIT_NO)
           TEMP_OPTIONS_RETIRE_GROSS_MARGIN(RETIRED_OPTIONS_PNTR) = GROSS_MARGIN
!           write(9,*) "7505 Retirements",INSTALLED_POINTER, &
!                                        R_UNIT_NO, &
!                                        RETIREMENTS_CO2(R_UNIT_NO), &
!                                        RETIREMENTS_MW(R_UNIT_NO), &
!                                        RETIREMENTS_MWH(R_UNIT_NO), &
!                                        GROSS_MARGIN
          ELSE
            I = TEMP_OPTIONS_RETIRED_UNIT_NO(R_UNIT_NO)
            IF(I > 0) THEN
               RETIREMENTS_CO2(R_UNIT_NO) = TEMP_OPTIONS_RETIREMENTS_CO2(I)
               RETIREMENTS_CO2_COST(R_UNIT_NO) = TEMP_OPTIONS_RETIREMENTS_CO2_COST(I)
               RETIREMENTS_CAP_COST(R_UNIT_NO) = TEMP_OPTIONS_RETIREMENTS_CAP_COST(I)
               RETIREMENTS_MW(R_UNIT_NO) = TEMP_OPTIONS_RETIREMENTS_MW(I)
               RETIREMENTS_MWH(R_UNIT_NO) = TEMP_OPTIONS_RETIREMENTS_MWH(I)
               GROSS_MARGIN = TEMP_OPTIONS_RETIRE_GROSS_MARGIN(I)
            ENDIF
          ENDIF
         END SUBROUTINE
!--------
        SUBROUTINE GRX_SAVE_PLANNIG_VALUES(R_SAVE_VALUES)
        USE CLA_OBJT_ARRAYS
        INTEGER*2 BASE_YEAR,YEAR,END_POINT,STUDY_PERIOD,ENDYR, &
               LAST_STUDY_YEAR,EXTENSION_PERIOD,LAST_EXTENSION_YEAR, &
               MAX_YEARS
        COMMON /GLOBAL_VARIABLES/ BASE_YEAR,YEAR,END_POINT,STUDY_PERIOD, &
               LAST_STUDY_YEAR,EXTENSION_PERIOD,LAST_EXTENSION_YEAR, &
               ENDYR,MAX_YEARS
        LOGICAL (KIND=4) :: SAVE_VALUES,R_SAVE_VALUES
        INTEGER (KIND=2), SAVE :: UPPER_TRANS_GROUP
        INTEGER (KIND=2) :: GET_NUMBER_OF_ACTIVE_GROUPS
           SAVE_VALUES = R_SAVE_VALUES
           IF(.NOT. ALLOCATED(SAVED_CL_ANN_CAP)) THEN
               ALLOCATE(SAVED_CL_ANN_CAP(3,STUDY_PERIOD,2))
               SAVE_VALUES = .TRUE.
           ENDIF
           IF(.NOT. ALLOCATED(SAVED_CL_TG_CAP)) THEN
              UPPER_TRANS_GROUP = GET_NUMBER_OF_ACTIVE_GROUPS()
              ALLOCATE(SAVED_CL_TG_CAP(0:6,MAX(1,UPPER_TRANS_GROUP),STUDY_PERIOD,2))
              SAVE_VALUES = .TRUE.
           ENDIF
           IF(SAVE_VALUES) THEN
              SAVED_CL_ANN_CAP(1:3,1:STUDY_PERIOD,1:2) = &
                                              CL_ANN_CAP(1:3,1:STUDY_PERIOD,1:2)
              SAVED_CL_TG_CAP(0:6,1:UPPER_TRANS_GROUP,1:STUDY_PERIOD,1:2) = &
                           CL_TG_CAP(0:6,1:UPPER_TRANS_GROUP,1:STUDY_PERIOD,1:2)
           ELSE
              CL_ANN_CAP(1:3,1:STUDY_PERIOD,1:2) = &
                                        SAVED_CL_ANN_CAP(1:3,1:STUDY_PERIOD,1:2)
              CL_TG_CAP(0:6,1:UPPER_TRANS_GROUP,1:STUDY_PERIOD,1:2) = &
                     SAVED_CL_TG_CAP(0:6,1:UPPER_TRANS_GROUP,1:STUDY_PERIOD,1:2)
           ENDIF
        END SUBROUTINE
!-------
        SUBROUTINE GRX_SAVE_CAPACITY_OPTIONS(R_SAVE_VALUES)
        USE CAPACITY_OPTIONS_ALLOC_VARIABLES
        USE CAPACITY_OPTIONS_FIXED_VARIABLES
        LOGICAL (KIND=4) :: R_SAVE_VALUES
           IF(.NOT. ALLOCATED(GRX_SAVED_CUMULATIVE_UNITS)) RETURN
           IF(R_SAVE_VALUES) THEN
              GRX_SAVED_CUMULATIVE_UNITS(1:FOR_ALL_OPTIONS) =  &
                                             CUMULATIVE_UNITS(1:FOR_ALL_OPTIONS)
           ELSE
              CUMULATIVE_UNITS(1:FOR_ALL_OPTIONS) =  &
                                   GRX_SAVED_CUMULATIVE_UNITS(1:FOR_ALL_OPTIONS)
              ANNUAL_UNITS_LEFT(1:FOR_ALL_OPTIONS) = ANNUAL_UNITS(1:FOR_ALL_OPTIONS)
           ENDIF
        END SUBROUTINE
!-------
        SUBROUTINE GRX_RETROFIT_OPTONS(R_SAVE_VALUES)
        USE PRODUCTION_ARRAYS_AND_DIMENSIONS
        USE CLA_OBJT_ARRAYS
        LOGICAL (KIND=4) :: SAVE_VALUES,R_SAVE_VALUES
           SAVE_VALUES = R_SAVE_VALUES
           IF(.NOT. ALLOCATED(GRX_BOP_RETROFIT_ACTIVE)) THEN
              ALLOCATE(GRX_BOP_RETROFIT_ACTIVE(MAX_CL_UNITS))
              SAVE_VALUES = .TRUE.
           ENDIF
           IF(SAVE_VALUES) THEN
              GRX_BOP_RETROFIT_ACTIVE(1:MAX_CL_UNITS) = &
                                                 RETROFIT_ACTIVE(1:MAX_CL_UNITS)
           ELSE
              RETROFIT_ACTIVE(1:MAX_CL_UNITS) = &
                       GRX_BOP_RETROFIT_ACTIVE(1:MAX_CL_UNITS) !   &
!                                            .AND. RETROFIT_ACTIVE(1:MAX_CL_UNITS)
           ENDIF
        END SUBROUTINE
!-------
        SUBROUTINE GRX_CO2_REDUCTION_CURVE(TG)
        INTEGER*2 BASE_YEAR,YEAR,END_POINT,STUDY_PERIOD,ENDYR, &
               LAST_STUDY_YEAR,EXTENSION_PERIOD,LAST_EXTENSION_YEAR, &
               MAX_YEARS
        COMMON /GLOBAL_VARIABLES/ BASE_YEAR,YEAR,END_POINT,STUDY_PERIOD, &
               LAST_STUDY_YEAR,EXTENSION_PERIOD,LAST_EXTENSION_YEAR, &
               ENDYR,MAX_YEARS
        REAL (KIND=4) :: GET_CO2_EMISS_FOR_SYSTEM
        INTEGER (KIND=2) :: GET_NUMBER_OF_ACTIVE_GROUPS,TG,I,GRX_MAX_ITERS
        IF(GRX_ITERATIONS == 0) THEN
           TG_MAX = GET_NUMBER_OF_ACTIVE_GROUPS()
           GRX_MAX_ITERS = GRX_MAXIMUM_ITERATIONS
           IF(ALLOCATED(GRX_ITER_CO2_PRICE)) DEALLOCATE (GRX_ITER_CO2_PRICE)
           ALLOCATE(GRX_ITER_CO2_PRICE(0:GRX_MAX_ITERS,0:TG_MAX))
           GRX_ITER_CO2_PRICE = 0.
           IF(ALLOCATED(GRX_ITER_TG_CO2_EMISS)) DEALLOCATE (GRX_ITER_TG_CO2_EMISS)
           ALLOCATE(GRX_ITER_TG_CO2_EMISS(0:GRX_MAX_ITERS,0:TG_MAX))
           GRX_ITER_TG_CO2_EMISS = 0.
           IF(ALLOCATED(GRX_CO2_EMISS_REDUCTION_REQUIRED)) &
                                    DEALLOCATE(GRX_CO2_EMISS_REDUCTION_REQUIRED)
           ALLOCATE(GRX_CO2_EMISS_REDUCTION_REQUIRED(0:TG_MAX))
           GRX_CO2_EMISS_REDUCTION_REQUIRED = 0.
           IF(YEAR == 1) CURRENT_CO2_DISPATCH_COST = 0.
           NEXT_CURRENT_CO2_DISPATCH_COST = 0.
           RETURN
        ELSE
           TG_MAX = GET_NUMBER_OF_ACTIVE_GROUPS()
           GRX_MAX_ITERS = GRX_MAXIMUM_ITERATIONS
           IF(.NOT. ALLOCATED(GRX_ITER_CO2_PRICE)) THEN
              ALLOCATE(GRX_ITER_CO2_PRICE(0:GRX_MAX_ITERS,0:TG_MAX))
              GRX_ITER_CO2_PRICE = 0.
           ENDIF
           IF(.NOT. ALLOCATED(GRX_ITER_TG_CO2_EMISS)) THEN
              ALLOCATE(GRX_ITER_TG_CO2_EMISS(0:GRX_MAX_ITERS,0:TG_MAX))
              GRX_ITER_TG_CO2_EMISS = 0.
           ENDIF
           IF(.NOT. ALLOCATED(GRX_CO2_EMISS_REDUCTION_REQUIRED)) THEN
              ALLOCATE(GRX_CO2_EMISS_REDUCTION_REQUIRED(0:TG_MAX))
              GRX_CO2_EMISS_REDUCTION_REQUIRED = 0.
           ENDIF
        ENDIF
        GRX_ITER_CO2_PRICE(GRX_ITERATIONS,TG) = CURRENT_CO2_DISPATCH_COST
        GRX_ITER_TG_CO2_EMISS(GRX_ITERATIONS,TG) = GET_CO2_EMISS_FOR_SYSTEM()
!        CURRENT_CO2_DISPATCH_COST = NEXT_CURRENT_CO2_DISPATCH_COST
!        IF(GRX_ITERATIONS == 8) THEN
           WRITE(9,*) "CO2 REDUCTION CURVE"
           DO I = 1, GRX_ITERATIONS
              WRITE(9,*) I,GRX_ITER_CO2_PRICE(I,TG),GRX_ITER_TG_CO2_EMISS(I,TG)
           ENDDO
!        ENDIF
        END SUBROUTINE
!-------
        FUNCTION GRX_CO2_PRICE_ADJ(TG)
        USE co2_market_abatement_curves
        REAL (KIND=4) :: GRX_CO2_PRICE_ADJ
        REAL (KIND=4) :: GET_EMISS_CAP_FOR_CLASS,X,SLOPE,INTERCEPT
        INTEGER (KIND=2) :: TG,PT1,PT2
        LOGICAL (KIND=1) :: CO2_EMISSIONS_GT_CAP
         IF(IS_CO2_PRICE_MRKT_CAPPED) THEN
            GRX_CO2_PRICE_ADJ = LAST_CO2_MARKET_PRICE
            WRITE(9,*) "MRKT PRICE CONTROLS SLOPE BASED PRICE",GRX_CO2_PRICE_ADJ
            IS_CO2_PRICE_MRKT_CAPPED = .FALSE.
            LAST_CO2_MARKET_PRICE = 0.
         ELSE
            PT2 = GRX_ITERATIONS
            PT1 = GRX_ITERATIONS - 1
            SLOPE = (GRX_ITER_CO2_PRICE(PT2,TG)-GRX_ITER_CO2_PRICE(PT1,TG))
            X = GET_EMISS_CAP_FOR_CLASS(3_2,TG)
            IF(GRX_CO2_MARKET_PTS_USED > 0) THEN
               X = X + 10**6 * SUM(CO2_ABATE_VOLUMS(1:GRX_CO2_MARKET_PTS_USED))
            ENDIF
            CO2_EMISSIONS_GT_CAP = GRX_ITER_TG_CO2_EMISS(PT2,TG) > X
            IF(SLOPE == 0. .AND. GRX_ITER_CO2_PRICE(PT2,TG) == 0.) THEN
               GRX_CO2_PRICE_ADJ = 20.
            ELSEIF(SLOPE == 0.) THEN
               IF(CO2_EMISSIONS_GT_CAP) THEN
                  GRX_CO2_PRICE_ADJ = 1.2* GRX_ITER_CO2_PRICE(PT2,TG)
               ELSE
                  GRX_CO2_PRICE_ADJ = 0.8 * GRX_ITER_CO2_PRICE(PT2,TG)
               ENDIF
            ELSE
               SLOPE = SLOPE/ &
                     (GRX_ITER_TG_CO2_EMISS(PT2,TG) - GRX_ITER_TG_CO2_EMISS(PT1,TG))
               INTERCEPT = GRX_ITER_CO2_PRICE(PT2,TG) - SLOPE * GRX_ITER_TG_CO2_EMISS(PT2,TG)
               GRX_CO2_PRICE_ADJ = SLOPE * X + INTERCEPT
            ENDIF
            WRITE(9,*) "SLOPE BASED PRICE",GRX_CO2_PRICE_ADJ
            WRITE(9,*) "SLOPE",SLOPE
            WRITE(9,*) "CAP USED",X
            WRITE(9,*) "CO2 PRICE 1",GRX_ITER_CO2_PRICE(PT2,TG)
            WRITE(9,*) "CO2 PRICE 2",GRX_ITER_CO2_PRICE(PT1,TG)
         ENDIF
         BALANCE_WITH_CO2_PRICE = .TRUE.
         CALL FLUSH(9)
        END FUNCTION
!-------
        FUNCTION GRX_CO2_LEAST_SQUARED_PRICE_ADJ(TG)
        REAL (KIND=4) :: GRX_CO2_LEAST_SQUARED_PRICE_ADJ
        REAL (KIND=4) :: GET_EMISS_CAP_FOR_CLASS,X,SLOPE,&
                         INTERCEPT,SUMX,SUMY,SUM_PROD_XY,SUM_X2
        INTEGER (KIND=2) :: TG,I,ISTART
        SUMX = 0
        SUMY = 0
        SUM_PROD_XY = 0.
        SUM_X2 = 0.
        ISTART = MAX(1,GRX_ITERATIONS-3)
        DO I = 1, GRX_ITERATIONS
            SUMX = SUMX + GRX_ITER_TG_CO2_EMISS(I,TG)
            SUM_X2 = SUM_X2 + GRX_ITER_TG_CO2_EMISS(I,TG)**2
            SUMY = SUMY + GRX_ITER_CO2_PRICE(I,TG)
            SUM_PROD_XY = SUM_PROD_XY + GRX_ITER_TG_CO2_EMISS(I,TG) * GRX_ITER_CO2_PRICE(I,TG)
        ENDDO
        SLOPE = (FLOAT(GRX_ITERATIONS)* SUM_PROD_XY - SUMX * SUMY)/ &
                     (FLOAT(GRX_ITERATIONS)* SUM_X2 - SUMX**2)
        INTERCEPT = (SUMY - SLOPE * SUMX)/FLOAT(GRX_ITERATIONS)
        X = (1.-CO2_ERROR_TRAGET) * GET_EMISS_CAP_FOR_CLASS(3_2,TG)
        GRX_CO2_LEAST_SQUARED_PRICE_ADJ = SLOPE * X + INTERCEPT
        BALANCE_WITH_CO2_PRICE = .TRUE.
         WRITE(9,*) "LEAST SQR BASED PRICE",GRX_CO2_LEAST_SQUARED_PRICE_ADJ
         WRITE(9,*) "SLOPE",SLOPE
         WRITE(9,*) "INTERCEPT",INTERCEPT
         CALL FLUSH(9)
        END FUNCTION
!--------
        SUBROUTINE GRX_CAPACITY_TEMP_RETRO_UNIT(R_UNIT_NO)
         USE PRODUCTION_ARRAYS_AND_DIMENSIONS
         USE CLA_OBJT_ARRAYS
         REAL (KIND=4), SAVE :: TEMP_RETRO_CO2(max_retirement_units), &
                                TEMP_RETRO_MW(max_retirement_units)
         REAL (KIND=4), SAVE :: TEMP_RETRO_MWH(max_retirement_units), &
                                TEMP_RETRO_CO2_PRICE(max_retirement_units)
         INTEGER (KIND=2) :: R_YEAR,R_UNIT_NO,CL_CAPACITY_TEMP_RETRO_UNIT
         INTEGER (KIND=2), SAVE :: TEMP_RETRO_UNIT_PTR(MAX_CL_UNITS)
         CHARACTER (LEN=1) :: TEMP_RETRO_SWITCH(max_retirement_units)
         INTEGER (KIND=2) :: RETRO_PTR
!
          IF(GRX_ITERATIONS == 0) THEN
            RETRO_POINTER = 1 + RETRO_POINTER
            TEMP_RETRO_UNIT_PTR(R_UNIT_NO) = RETRO_POINTER
!            TEMP_RETRO_SWITCH(RETRO_POINTER) = RETIREMENT_CANDIDATE(R_UNIT_NO)
!            TEMP_FIRST_RETIREMENT_YEAR(RETRO_POINTER) = FIRST_RETIREMENT_YEAR(R_UNIT_NO)
            TEMP_RETRO_CO2(RETRO_POINTER) = RETIREMENTS_CO2(R_UNIT_NO)
            TEMP_RETRO_MW(RETRO_POINTER) = RETIREMENTS_MW(R_UNIT_NO)
            TEMP_RETRO_MWH(RETRO_POINTER) = RETIREMENTS_MWH(R_UNIT_NO)
            TEMP_RETRO_CO2_PRICE(RETRO_POINTER) = RETIREMENTS_CO2_PRICE(R_UNIT_NO)
            write(9,*) "GRX Retros Saved"
         ELSE
            RETRO_PTR = TEMP_RETRO_UNIT_PTR(R_UNIT_NO)
            IF(RETRO_PTR > 0) THEN
!            RETIREMENT_CANDIDATE(R_UNIT_NO) = TEMP_RETRO_SWITCH(RETRO_PTR)
!            TEMP_FIRST_RETIREMENT_YEAR(R_UNIT_NO) = FIRST_RETIREMENT_YEAR(RETRO_PTR)
               RETIREMENTS_CO2(R_UNIT_NO) = TEMP_RETRO_CO2(RETRO_PTR)
               RETIREMENTS_MW(R_UNIT_NO) = TEMP_RETRO_MW(RETRO_PTR)
               RETIREMENTS_MWH(R_UNIT_NO) = TEMP_RETRO_MWH(RETRO_PTR)
               RETIREMENTS_CO2_PRICE(R_UNIT_NO) = TEMP_RETRO_CO2_PRICE(RETRO_PTR)
            ENDIF
            write(9,*) "GRX Retros Restored"
         ENDIF
         write(9,*) RETRO_PTR, &
                    R_UNIT_NO, &
                    RETIREMENT_CANDIDATE(R_UNIT_NO), &
                    RETIREMENTS_CO2(R_UNIT_NO), &
                    RETIREMENTS_MW(R_UNIT_NO), &
                    RETIREMENTS_MWH(R_UNIT_NO), &
                    RETIREMENTS_CO2_PRICE(R_UNIT_NO), &
                    FIRST_RETIREMENT_YEAR(R_UNIT_NO)

         CL_CAPACITY_TEMP_RETRO_UNIT = RETRO_PTR
         END SUBROUTINE
!-------
        SUBROUTINE GRX_ITER_RESULTS(TG,CONVER_CREDITS_USED,CONVER_CREDITS_COST)
        USE co2_market_abatement_curves
        REAL (KIND=4), OPTIONAL :: CONVER_CREDITS_USED,CONVER_CREDITS_COST
        INTEGER*2 BASE_YEAR,YEAR,END_POINT,STUDY_PERIOD,ENDYR, &
               LAST_STUDY_YEAR,EXTENSION_PERIOD,LAST_EXTENSION_YEAR, &
               MAX_YEARS
        COMMON /GLOBAL_VARIABLES/ BASE_YEAR,YEAR,END_POINT,STUDY_PERIOD, &
               LAST_STUDY_YEAR,EXTENSION_PERIOD,LAST_EXTENSION_YEAR, &
               ENDYR,MAX_YEARS
         INTEGER (KIND=4) :: NEXT_REC/0/
         CHARACTER (LEN=40) :: TITLE
         CHARACTER (LEN=28) :: COMPANY_NAME
         INTEGER (KIND=1) :: F7/z"f7"/
         LOGICAL (KIND=4) FILE_EXISTS
         INTEGER (KIND=2) :: OVERHEAD_LENGTH,DIMENSIONS, &
                             VARIABLE_NUMBER,RECORD_LENGTH, &
                             GET_NUM_OF_END_POINTS, &
                             TG
         CHARACTER (LEN=256) :: FILE_NAME,GET_RESULTS_DIRECTORY
         CHARACTER (LEN=5) :: GET_SCENAME
         CHARACTER (LEN=35) :: GET_GROUP_NAME
         REAL (KIND=4) :: CAP_ERROR,GET_EMISS_CAP_FOR_CLASS, &
                          GET_CO2_EMISS_FOR_SYSTEM, &
                          CO2_CREDITS_FROM_MARKET, &
                          CO2_CREDIT_EXPENDITURES, &
                          CREDITS_PURCHASED_AT_LAST_PRICE
         REAL (KIND=4) :: CO2_EMISS_CAP,CO2_EMISSIONS,UNCOSTED_CREDITS
         INTEGER (KIND=2) :: I
         IF(NEXT_REC == 0) THEN
            FILE_NAME = TRIM(GET_RESULTS_DIRECTORY())//"MSG"// &
                           TRIM(GET_SCENAME())//".XGD"
            OVERHEAD_LENGTH = 64
            VARIABLE_NUMBER = 20 ! 08/24/05. ADDED FORECAST VARIABLE
            DIMENSIONS = 4
            RECORD_LENGTH = MAX(64,OVERHEAD_LENGTH + 4*VARIABLE_NUMBER)
            OPEN(1337,FILE=FILE_NAME,ACCESS="DIRECT", &
                                   STATUS="REPLACE",RECL=RECORD_LENGTH)
!
            WRITE(1337,REC=1) F7,RECORD_LENGTH, &
                                      DIMENSIONS, &
                                      BASE_YEAR, &
                                      ENDYR+EXTENSION_PERIOD, &
                                      GET_NUM_OF_END_POINTS()
            WRITE(1337,REC=2) COMPANY_NAME()
            WRITE(1337,REC=3) TITLE()
            WRITE(1337,REC=4) 'Endpoint            ','N',INT2(4),'F','D'
            WRITE(1337,REC=5) 'Year                ','N',INT2(4),'F','D'
            WRITE(1337,REC=6) 'Transaction Group   ','C',INT2(35),'V','D'
            WRITE(1337,REC=7) 'Iteration           ','N',INT2(4),'F','D'
            NEXT_REC = 7
         ENDIF
         NEXT_REC = NEXT_REC + 1
         IF(GRX_CO2_MARKET_PTS_USED == 0) THEN
            CAP_ERROR = -100.*(GET_EMISS_CAP_FOR_CLASS(3_2,TG) - GET_CO2_EMISS_FOR_SYSTEM()) &
                                                 /GET_EMISS_CAP_FOR_CLASS(3_2,TG)
            WRITE(1337,REC=NEXT_REC) PRT_ENDPOINT(),FLOAT(BASE_YEAR+YEAR), &
                                  GET_GROUP_NAME(TG), &
                                  FLOAT(GRX_ITERATIONS), &
                                  CURRENT_CO2_DISPATCH_COST, & !GRX_ITER_CO2_PRICE(GRX_ITERATIONS,TG), &
                                  GET_CO2_EMISS_FOR_SYSTEM()/1000000. , &
                                  CAP_ERROR, &
                                  GET_EMISS_CAP_FOR_CLASS(3_2,TG)/1000000., &
                                  0., & !CO2_CAP_INCREASE_FROM_MARKET/1000000., &
                                  0., & !CO2_CAP_EXPENDITURES, &
                                  0., &
                                  0., &
                                  0., &
                                  GET_CO2_EMISS_FOR_SYSTEM()/1000000., &
                                  0., &
                                  0.
         ELSE
            CO2_EMISS_CAP = GET_EMISS_CAP_FOR_CLASS(3_2,TG)/1000000.
            CO2_EMISSIONS = GET_CO2_EMISS_FOR_SYSTEM()/1000000.
            IF(PRESENT(CONVER_CREDITS_USED)) THEN
               CO2_CREDITS_FROM_MARKET = CONVER_CREDITS_USED/1000000.
               IF(GRX_CO2_MARKET_PTS_USED > 1) THEN
                  CREDITS_PURCHASED_AT_LAST_PRICE= CO2_CREDITS_FROM_MARKET &
                            - SUM(CO2_ABATE_VOLUMS(1:GRX_CO2_MARKET_PTS_USED-1))
               ELSE
                  CREDITS_PURCHASED_AT_LAST_PRICE = CO2_CREDITS_FROM_MARKET
               ENDIF
            ELSE
               CO2_CREDITS_FROM_MARKET = SUM(CO2_ABATE_VOLUMS(1:GRX_CO2_MARKET_PTS_USED))
               CREDITS_PURCHASED_AT_LAST_PRICE= CO2_ABATE_VOLUMS(GRX_CO2_MARKET_PTS_USED)
            ENDIF
            IF(PRESENT(CONVER_CREDITS_COST)) THEN
               CO2_CAP_EXPENDITURES = CONVER_CREDITS_COST
            ELSE
               CO2_CAP_EXPENDITURES = 0.
               UNCOSTED_CREDITS = CO2_CREDITS_FROM_MARKET
               DO I = 1, GRX_CO2_MARKET_PTS_USED
                  CO2_CAP_EXPENDITURES = CO2_CAP_EXPENDITURES + &
                          CO2_ABATE_PRICES(I) * MIN(CO2_ABATE_VOLUMS(I),UNCOSTED_CREDITS)
                  UNCOSTED_CREDITS = UNCOSTED_CREDITS - CO2_ABATE_VOLUMS(I)
               ENDDO
            ENDIF
            CAP_ERROR = -100.*(CO2_EMISS_CAP - (CO2_EMISSIONS - CO2_CREDITS_FROM_MARKET))/ &
                                         CO2_EMISS_CAP
            WRITE(1337,REC=NEXT_REC) PRT_ENDPOINT(),FLOAT(BASE_YEAR+YEAR), &
                                  GET_GROUP_NAME(TG), &
                                  FLOAT(GRX_ITERATIONS), &
                                  CURRENT_CO2_DISPATCH_COST, & !GRX_ITER_CO2_PRICE(GRX_ITERATIONS,TG), &
                                  CO2_EMISSIONS, &
                                  CAP_ERROR, &
                                  CO2_EMISS_CAP, &
                                  CO2_CREDITS_FROM_MARKET, &
                                  CO2_CAP_EXPENDITURES, &
                                  FLOAT(GRX_CO2_MARKET_PTS_USED), &
                                  CO2_ABATE_PRICES(GRX_CO2_MARKET_PTS_USED), &
                                  CO2_ABATE_VOLUMS(GRX_CO2_MARKET_PTS_USED), &
                                  (CO2_EMISSIONS - CO2_CREDITS_FROM_MARKET), &
                                  SUM(CO2_ABATE_VOLUMS(1:GRX_CO2_MARKET_PTS_USED)), &
                                  CREDITS_PURCHASED_AT_LAST_PRICE

         ENDIF
         CALL FLUSH(1337)
      END SUBROUTINE
!-------
        SUBROUTINE GRX_ABATE_ITER_RESULTS(CONVER_CREDITS_USED,CONVER_CREDITS_COST)
        USE NEWTON_INTERFACE
        USE CO2_CAP_N_TRADE
        USE co2_market_abatement_curves
        REAL (KIND=4), OPTIONAL :: CONVER_CREDITS_USED,CONVER_CREDITS_COST
        LOGICAL (KIND=4) :: UPDATE
        INTEGER*2 BASE_YEAR,YEAR,END_POINT,STUDY_PERIOD,ENDYR, &
               LAST_STUDY_YEAR,EXTENSION_PERIOD,LAST_EXTENSION_YEAR, &
               MAX_YEARS
        COMMON /GLOBAL_VARIABLES/ BASE_YEAR,YEAR,END_POINT,STUDY_PERIOD, &
               LAST_STUDY_YEAR,EXTENSION_PERIOD,LAST_EXTENSION_YEAR, &
               ENDYR,MAX_YEARS
         INTEGER (KIND=4) :: NEXT_REC/0/
         CHARACTER (LEN=40) :: TITLE
         CHARACTER (LEN=28) :: COMPANY_NAME
         INTEGER (KIND=1) :: F7/z"f7"/
         LOGICAL (KIND=4) FILE_EXISTS
         INTEGER (KIND=2) :: OVERHEAD_LENGTH,DIMENSIONS, &
                             VARIABLE_NUMBER,RECORD_LENGTH, &
                             GET_NUM_OF_END_POINTS, &
                             TG/0/
         CHARACTER (LEN=256) :: FILE_NAME,GET_RESULTS_DIRECTORY
         CHARACTER (LEN=5) :: GET_SCENAME
         CHARACTER (LEN=35) :: GET_GROUP_NAME
         REAL (KIND=4) :: CAP_ERROR,GET_EMISS_CAP_FOR_CLASS, &
                          GET_CO2_EMISS_FOR_SYSTEM, &
                          CO2_CREDITS_FROM_MARKET, &
                          CO2_CREDIT_EXPENDITURES, &
                          NET_CO2_EMISSIONS, &
                          ABATEMENTS_USED_TO_MEET_CAP, &
                          ABATEMENTS_FROM_MARKET
         REAL (KIND=4) :: CO2_EMISS_CAP,CO2_EMISSIONS,UNCOSTED_CREDITS
         REAL (KIND=4) :: CO2_ELECTRIC_EMISSION,CO2_NON_ELECTRIC_EMISSION, &
                          CO2_ABATEMENTS_AVAIL,GRX_ABATEMENT_PRICE, &
                          GRX_ALL_SECTOR_CO2_EMISSIONS_CAP, &
                          GRX_NON_ELECTRIC_SECTOR_CO2_EMISSIONS, &
                          GRX_CO2_ABATEMENTS_AVAILABLE, &
                          CO2_BANKED_BALANCE, &
                          GRX_CO2_BANK_DELTA,CO2_BANK_CREDITS_USED,CO2_BANK_CREDITS_PURCHASED 
         IF(NEXT_REC == 0) THEN
            FILE_NAME = TRIM(GET_RESULTS_DIRECTORY())//"MSG"// &
                           TRIM(GET_SCENAME())//".XHD"
            OVERHEAD_LENGTH = 64
            VARIABLE_NUMBER = 20 ! 08/24/05. ADDED FORECAST VARIABLE
            DIMENSIONS = 4
            RECORD_LENGTH = MAX(64,OVERHEAD_LENGTH + 4*VARIABLE_NUMBER)
            OPEN(1338,FILE=FILE_NAME,ACCESS="DIRECT", &
                                   STATUS="REPLACE",RECL=RECORD_LENGTH)
!
            WRITE(1338,REC=1) F7,RECORD_LENGTH, &
                                      DIMENSIONS, &
                                      BASE_YEAR, &
                                      ENDYR+EXTENSION_PERIOD, &
                                      GET_NUM_OF_END_POINTS()
            WRITE(1338,REC=2) COMPANY_NAME()
            WRITE(1338,REC=3) TITLE()
            WRITE(1338,REC=4) 'Endpoint            ','N',INT2(4),'F','D'
            WRITE(1338,REC=5) 'Year                ','N',INT2(4),'F','D'
            WRITE(1338,REC=6) 'Transaction Group   ','C',INT2(35),'V','D'
            WRITE(1338,REC=7) 'Iteration           ','N',INT2(4),'F','D'
            NEXT_REC = 7
         ENDIF
         NEXT_REC = NEXT_REC + 1
         CO2_EMISS_CAP = GRX_ALL_SECTOR_CO2_EMISSIONS_CAP(BASE_YEAR+YEAR) /1000000.
         CO2_BANK_CREDITS_USED = ABS(MIN(0,GRX_CO2_BANK_DELTA(BASE_YEAR+YEAR)))/10.**6 ! negative term when bank is used for offset.
         CO2_BANK_CREDITS_PURCHASED = MAX(0,GRX_CO2_BANK_DELTA(BASE_YEAR+YEAR))/10.**6 ! positive term when bank purchases offsets
         CO2_ELECTRIC_EMISSION = GET_CO2_EMISS_FOR_SYSTEM()/1000000.
         CO2_NON_ELECTRIC_EMISSION = GRX_NON_ELECTRIC_SECTOR_CO2_EMISSIONS(BASE_YEAR+YEAR)/1000000.
         CO2_EMISSIONS = CO2_ELECTRIC_EMISSION &
                         + CO2_NON_ELECTRIC_EMISSION
         CO2_ABATEMENTS_AVAIL = &
                         GRX_CO2_ABATEMENTS_AVAILABLE(CURRENT_CO2_DISPATCH_COST, &
                                                      BASE_YEAR+YEAR)/1000000.
! changes 7/19/10
            if(year+base_year == 2016) then
               ABATEMENTS_USED_TO_MEET_CAP = 0.
            endif
         ABATEMENTS_USED_TO_MEET_CAP = CO2_EMISSIONS - CO2_EMISS_CAP
         IF(ABATEMENTS_USED_TO_MEET_CAP < 0.) THEN
            IF(CO2_ABATEMENTS_AVAIL > 0.) CO2_BANK_CREDITS_PURCHASED = CO2_ABATEMENTS_AVAIL
            CO2_CREDITS_FROM_MARKET = 0.
            CO2_BANK_CREDITS_USED = 0.
         ELSE
            ABATEMENTS_USED_TO_MEET_CAP = ABATEMENTS_USED_TO_MEET_CAP - CO2_BANK_CREDITS_USED
            IF(ABATEMENTS_USED_TO_MEET_CAP < 0.) THEN
               CO2_BANK_CREDITS_USED = ABATEMENTS_USED_TO_MEET_CAP
               IF(CO2_ABATEMENTS_AVAIL > 0.) CO2_BANK_CREDITS_PURCHASED = CO2_ABATEMENTS_AVAIL
               CO2_CREDITS_FROM_MARKET = 0.
            ELSEIF(ABATEMENTS_USED_TO_MEET_CAP > CO2_ABATEMENTS_AVAIL) THEN
               CO2_CREDITS_FROM_MARKET = CO2_ABATEMENTS_AVAIL
               CO2_BANK_CREDITS_PURCHASED = 0.
            ELSE
               CO2_CREDITS_FROM_MARKET = ABATEMENTS_USED_TO_MEET_CAP
               CO2_BANK_CREDITS_PURCHASED = CO2_ABATEMENTS_AVAIL - ABATEMENTS_USED_TO_MEET_CAP
            ENDIF
         ENDIF
         CO2_CAP_EXPENDITURES = CURRENT_CO2_DISPATCH_COST * CO2_ABATEMENTS_AVAIL

!         IF(PRESENT(CONVER_CREDITS_USED)) THEN
!            CO2_CREDITS_FROM_MARKET = CONVER_CREDITS_USED/1000000.
!         ELSE
!            CO2_CREDITS_FROM_MARKET = CO2_ABATEMENTS_AVAIL
!         ENDIF
!         IF(PRESENT(CONVER_CREDITS_COST)) THEN
!            CO2_CAP_EXPENDITURES = CONVER_CREDITS_COST
!         ELSE
!            CO2_CAP_EXPENDITURES = CURRENT_CO2_DISPATCH_COST * CO2_CREDITS_FROM_MARKET
!         ENDIF
         UPDATE = GRX_ITERATIONS == 0
         NET_CO2_EMISSIONS = (CO2_EMISSIONS - CO2_CREDITS_FROM_MARKET &
                              - CO2_BANK_CREDITS_USED)
         ABATEMENTS_USED_TO_MEET_CAP = CO2_CREDITS_FROM_MARKET
         ABATEMENTS_FROM_MARKET = ABATEMENTS_USED_TO_MEET_CAP  &
                                  + CO2_BANK_CREDITS_PURCHASED &
                                  - CO2_BANK_CREDITS_USED
         CAP_ERROR = -100.*(CO2_EMISS_CAP - NET_CO2_EMISSIONS)/ &
                                     CO2_EMISS_CAP

         WRITE(1338,REC=NEXT_REC) PRT_ENDPOINT(),FLOAT(BASE_YEAR+YEAR), &
                     GET_GROUP_NAME(TG), &
                     FLOAT(GRX_ITERATIONS), &
                     CURRENT_CO2_DISPATCH_COST, & ! 0 GRX_ITER_CO2_PRICE(GRX_ITERATIONS,TG), &
                     CO2_EMISSIONS, &
                     CAP_ERROR, &
                     CO2_EMISS_CAP, &
                     CO2_CREDITS_FROM_MARKET, &
                     CO2_CAP_EXPENDITURES, &  ! 5
                     NET_CO2_EMISSIONS, &
                     CO2_ABATEMENTS_AVAIL, &
                     CO2_ELECTRIC_EMISSION, &
                     CO2_NON_ELECTRIC_EMISSION, &
                     ABATEMENTS_USED_TO_MEET_CAP, &    ! 10
                     GRX_CO2_OTHER_ABATEMENTS_AVAILABLE(BASE_YEAR+YEAR,CO2_PRICE=CURRENT_CO2_DISPATCH_COST), &
                     GRX_CO2_DOMISIC_ABATEMENTS_AVAILABLE(BASE_YEAR+YEAR,CO2_PRICE=CURRENT_CO2_DISPATCH_COST), &
                     GRX_CO2_INTRL_ABATEMENTS_AVAILABLE(BASE_YEAR+YEAR,CO2_PRICE=CURRENT_CO2_DISPATCH_COST), & !13
                     CO2_BANK_CREDITS_USED, &
                     CO2_BANK_CREDITS_PURCHASED, &
                     CO2_BANK_CREDITS_USED * CURRENT_CO2_DISPATCH_COST, &
                     CO2_BANK_CREDITS_PURCHASED * CURRENT_CO2_DISPATCH_COST, &  ! 17
                     CO2_BANKED_BALANCE(GRX_CONVERGED,CO2_BANK_CREDITS_PURCHASED), &
                     ABATEMENTS_FROM_MARKET   ! 19
         CALL FLUSH(1338)
      END SUBROUTINE
!-------
        SUBROUTINE GRX_ABATEMENT_MARKET_DATA_RPT()
        USE NEWTON_INTERFACE
!        USE co2_market_abatement_curves
        INTEGER*2 BASE_YEAR,YEAR,END_POINT,STUDY_PERIOD,ENDYR, &
               LAST_STUDY_YEAR,EXTENSION_PERIOD,LAST_EXTENSION_YEAR, &
               MAX_YEARS
        COMMON /GLOBAL_VARIABLES/ BASE_YEAR,YEAR,END_POINT,STUDY_PERIOD, &
               LAST_STUDY_YEAR,EXTENSION_PERIOD,LAST_EXTENSION_YEAR, &
               ENDYR,MAX_YEARS
         INTEGER (KIND=4) :: NEXT_REC/0/
         CHARACTER (LEN=40) :: TITLE
         CHARACTER (LEN=28) :: COMPANY_NAME
         INTEGER (KIND=1) :: F7/z"f7"/
         LOGICAL (KIND=4) FILE_EXISTS
         INTEGER (KIND=2) :: OVERHEAD_LENGTH,DIMENSIONS, &
                             VARIABLE_NUMBER,RECORD_LENGTH, &
                             GET_NUM_OF_END_POINTS,I
         CHARACTER (LEN=256) :: FILE_NAME,GET_RESULTS_DIRECTORY
         CHARACTER (LEN=5) :: GET_SCENAME
         CHARACTER (LEN=35) :: GET_GROUP_NAME
         REAL (KIND=4) :: CO2_PRICE
         IF(NEXT_REC == 0) THEN
            FILE_NAME = TRIM(GET_RESULTS_DIRECTORY())//"MSG"// &
                           TRIM(GET_SCENAME())//".XID"
            OVERHEAD_LENGTH = 32
            VARIABLE_NUMBER = 20 ! 08/24/05. ADDED FORECAST VARIABLE
            DIMENSIONS = 3
            RECORD_LENGTH = MAX(64,OVERHEAD_LENGTH + 4*VARIABLE_NUMBER)
            OPEN(1339,FILE=FILE_NAME,ACCESS="DIRECT", &
                                   STATUS="REPLACE",RECL=RECORD_LENGTH)
!
            WRITE(1339,REC=1) F7,RECORD_LENGTH, &
                                 DIMENSIONS, &
                                 BASE_YEAR, &
                                 ENDYR+EXTENSION_PERIOD, &
                                 GET_NUM_OF_END_POINTS()
            WRITE(1339,REC=2) COMPANY_NAME()
            WRITE(1339,REC=3) TITLE()
            WRITE(1339,REC=4) 'Endpoint            ','N',INT2(4),'F','D'
            WRITE(1339,REC=5) 'Year                ','N',INT2(4),'F','D'
            WRITE(1339,REC=6) 'Price               ','N',INT2(4),'F','D'
            NEXT_REC = 6
         ENDIF
         CO2_PRICE = 5.
         DO I = 1, 55
            NEXT_REC = NEXT_REC + 1
            WRITE(1339,REC=NEXT_REC) PRT_ENDPOINT(), &
                                     FLOAT(BASE_YEAR+YEAR), &
                                     CO2_PRICE, &
                                     GRX_CO2_DOMISIC_ABATEMENTS_AVAILABLE(BASE_YEAR+YEAR,CO2_PRICE=CO2_PRICE), &
                                     GRX_CO2_INTRL_ABATEMENTS_AVAILABLE(BASE_YEAR+YEAR,CO2_PRICE=CO2_PRICE), &
                                     GRX_CO2_OTHER_ABATEMENTS_AVAILABLE(BASE_YEAR+YEAR,CO2_PRICE=CO2_PRICE), &
                                     GRX_CO2_DOMISIC_ABATEMENTS_AVAILABLE(BASE_YEAR+YEAR,CO2_PRICE=CO2_PRICE)  &
                                      + GRX_CO2_INTRL_ABATEMENTS_AVAILABLE(BASE_YEAR+YEAR,CO2_PRICE=CO2_PRICE) &
                                      + GRX_CO2_OTHER_ABATEMENTS_AVAILABLE(BASE_YEAR+YEAR,CO2_PRICE=CO2_PRICE)

            IF(I > 40) THEN
               CO2_PRICE = CO2_PRICE + 100.
            ELSEIF(I > 20) THEN
               CO2_PRICE = CO2_PRICE + 50.
            ELSE
               CO2_PRICE = CO2_PRICE + 5.
            ENDIF
         ENDDO
         CALL FLUSH(1339)
      END SUBROUTINE
      END MODULE GRX_PLANNING_ROUTINES