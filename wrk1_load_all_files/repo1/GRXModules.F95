!     ******************************************************************
!     GRXModules.F95
!     Copyright(c)  2000
!
!     Created: 9/2/2009 12:15:27 PM
!     Author : MARK S GERBER


!--------
      REAL FUNCTION GRX_CO2_OTHER_ABATEMENTS_AVAILABLE(YEAR,CO2_PRICE,CO2_CREDITS)
      USE CO2_CAP_N_TRADE
      REAL (KIND=4), OPTIONAL :: CO2_PRICE,CO2_CREDITS
      REAL (KIND=4) :: GRX_CO2_PRICE
      INTEGER (KIND=2) :: YEAR
         IF(PRESENT(CO2_PRICE)) THEN
            IF(YEAR <= YR2_DATA .AND. YEAR >= YR1_DATA .AND. CO2_PRICE > 0. .AND. CO2_PARAM_FILE_EXISTS) THEN
               IF(CO2_PRICE <= Non_El_Price_Break(YEAR)) THEN
                  IF(Non_El_1_B_Coefficient(YEAR) == 0.) THEN
                     GRX_CO2_OTHER_ABATEMENTS_AVAILABLE = 0.
                  ELSE
                     IF(INDEX(Non_El_Price_Equation_1_Type(YEAR),"Expon") /= 0) THEN
                        GRX_CO2_OTHER_ABATEMENTS_AVAILABLE=MAX(0.,(LOG(CO2_PRICE)-LOG(Non_El_1_A_Coefficient(YEAR)))/ &
                                                                        Non_El_1_B_Coefficient(YEAR))
                     ELSE
                        GRX_CO2_OTHER_ABATEMENTS_AVAILABLE = (CO2_PRICE-Non_El_1_A_Coefficient(YEAR))/ &
                                                                 Non_El_1_B_Coefficient(YEAR)
                     ENDIF
                  ENDIF
!                 GRX_CO2_DOMISIC_ABATEMENTS_PRICE =  COEFF1(YEAR) *
!                                                      EXP(CO2_ABATEMENTS_PURCHASED* COEFF2(YEAR))
               ELSE ! Equation 2
                  IF(Non_El_2_B_Coefficient(YEAR) == 0.) THEN
                     GRX_CO2_OTHER_ABATEMENTS_AVAILABLE = 0.
                  ELSE
                     IF(INDEX(Non_El_Price_Equation_2_Type(YEAR),"Expon") /= 0) THEN
                        GRX_CO2_OTHER_ABATEMENTS_AVAILABLE=MAX(0.,(LOG(CO2_PRICE)-LOG(Non_El_2_A_Coefficient(YEAR)))/ &
                                                                        Non_El_2_B_Coefficient(YEAR))
                     ELSE
                        GRX_CO2_OTHER_ABATEMENTS_AVAILABLE = (CO2_PRICE-Non_El_2_A_Coefficient(YEAR))/  &
                                                                    Non_El_2_B_Coefficient(YEAR)
                     ENDIF
                  ENDIF

!                 GRX_CO2_DOMISIC_ABATEMENTS_PRICE =  COEFF1(YEAR) + COEFF2(YEAR) * CO2_ABATEMENTS_PURCHASED                                                       +
               ENDIF
            ELSE
               GRX_CO2_OTHER_ABATEMENTS_AVAILABLE = 0.
            ENDIF
            GRX_CO2_OTHER_ABATEMENTS_AVAILABLE = MAX(GRX_CO2_OTHER_ABATEMENTS_AVAILABLE, &
                                                       Non_Elect_Min_Reductions(YEAR))
            GRX_CO2_OTHER_ABATEMENTS_AVAILABLE = MIN(GRX_CO2_OTHER_ABATEMENTS_AVAILABLE, &
                                                       Non_Elect_Max_Reductions(YEAR))
         ENDIF ! PRICE PRESENT
         IF(PRESENT(CO2_CREDITS)) THEN
!               GRX_CO2_DOMISIC_ABATEMENTS_PRICE =  COEFF1(YEAR) *
!                                                      EXP(CO2_ABATEMENTS_PURCHASED* COEFF2(YEAR))
            IF(YEAR <= YR2_DATA .AND. YEAR >= YR1_DATA .AND. CO2_PARAM_FILE_EXISTS) THEN
               IF(INDEX(Non_El_Price_Equation_1_Type(YEAR),"Expon") /= 0) THEN
                  GRX_CO2_PRICE = Non_El_1_A_Coefficient(YEAR)*EXP(CO2_CREDITS*Non_El_1_B_Coefficient(YEAR))
                  IF(GRX_CO2_PRICE > Non_El_Price_Break(YEAR)) THEN
                     IF(INDEX(Non_El_Price_Equation_2_Type(YEAR),"Expon") /= 0) THEN
                        GRX_CO2_PRICE = Non_El_2_A_Coefficient(YEAR)*EXP(CO2_CREDITS*Non_El_2_B_Coefficient(YEAR))
                     ELSE
                        GRX_CO2_PRICE = Non_El_2_A_Coefficient(YEAR) + CO2_CREDITS*Non_El_2_B_Coefficient(YEAR)
                     ENDIF
                  ENDIF
               ELSE
                  GRX_CO2_PRICE = Non_El_1_A_Coefficient(YEAR) + CO2_CREDITS*Non_El_1_B_Coefficient(YEAR)
                  IF(GRX_CO2_PRICE > Non_El_Price_Break(YEAR)) THEN
                     IF(INDEX(Non_El_Price_Equation_2_Type(YEAR),"Expon") /= 0) THEN
                        GRX_CO2_PRICE = Non_El_2_A_Coefficient(YEAR)*EXP(CO2_CREDITS*Non_El_2_B_Coefficient(YEAR))
                     ELSE
                        GRX_CO2_PRICE = Non_El_2_A_Coefficient(YEAR) + CO2_CREDITS*Non_El_2_B_Coefficient(YEAR)
                     ENDIF
                  ENDIF

!                 GRX_CO2_DOMISIC_ABATEMENTS_PRICE =  COEFF1(YEAR) + COEFF2(YEAR) * CO2_ABATEMENTS_PURCHASED                                                       +
               ENDIF
               GRX_CO2_OTHER_ABATEMENTS_AVAILABLE = GRX_CO2_PRICE
            ELSE
               GRX_CO2_OTHER_ABATEMENTS_AVAILABLE = 0.
            ENDIF
         ENDIF
      END FUNCTION
!--------
      REAL FUNCTION GRX_CO2_DOMISIC_ABATEMENTS_AVAILABLE(YEAR,CO2_PRICE,CO2_CREDITS)
      USE CO2_CAP_N_TRADE
      REAL (KIND=4), OPTIONAL :: CO2_PRICE,CO2_CREDITS
      REAL (KIND=4) :: GRX_CO2_PRICE
      INTEGER (KIND=2) :: YEAR
         IF(PRESENT(CO2_PRICE)) THEN
            IF(YEAR <= YR2_DATA .AND. YEAR >= YR1_DATA .AND. CO2_PRICE > 0. .AND. CO2_PARAM_FILE_EXISTS) THEN
               IF(CO2_PRICE <= Dom_Price_Break(YEAR)) THEN
                  IF(Dom_1_B_Coefficient(YEAR) == 0.) THEN
                     GRX_CO2_DOMISIC_ABATEMENTS_AVAILABLE = 0.
                  ELSE
                     IF(INDEX(Dom_Price_Equation_1_Type(YEAR),"Expon") /= 0) THEN
                        GRX_CO2_DOMISIC_ABATEMENTS_AVAILABLE=MAX(0.,(LOG(CO2_PRICE)-LOG(Dom_1_A_Coefficient(YEAR)))/ &
                                                                     Dom_1_B_Coefficient(YEAR))
                     ELSE
                        GRX_CO2_DOMISIC_ABATEMENTS_AVAILABLE = (CO2_PRICE-Dom_1_A_Coefficient(YEAR))/  &
                                                              Dom_1_B_Coefficient(YEAR)
                     ENDIF
                  ENDIF
!                  GRX_CO2_DOMISIC_ABATEMENTS_PRICE =  COEFF1(YEAR) *
!                                                         EXP(CO2_ABATEMENTS_PURCHASED* COEFF2(YEAR))
               ELSE ! Equation Type 2
                  IF(Dom_2_B_Coefficient(YEAR) == 0.) THEN
                     GRX_CO2_DOMISIC_ABATEMENTS_AVAILABLE = 0.
                  ELSE
                     IF(INDEX(Dom_Price_Equation_2_Type(YEAR),"Expon") /= 0) THEN
                        GRX_CO2_DOMISIC_ABATEMENTS_AVAILABLE=MAX(0.,(LOG(CO2_PRICE)-LOG(Dom_2_A_Coefficient(YEAR)))/ &
                                                                     Dom_2_B_Coefficient(YEAR))
                     ELSE
                        GRX_CO2_DOMISIC_ABATEMENTS_AVAILABLE = (CO2_PRICE-Dom_2_A_Coefficient(YEAR))/  &
                                                                    Dom_2_B_Coefficient(YEAR)
                     ENDIF
                  ENDIF
           
!                  GRX_CO2_DOMISIC_ABATEMENTS_PRICE =  COEFF1(YEAR) + COEFF2(YEAR) * CO2_ABATEMENTS_PURCHASED                                                       +
               ENDIF
            ELSE
              GRX_CO2_DOMISIC_ABATEMENTS_AVAILABLE = 0.
            ENDIF
            GRX_CO2_DOMISIC_ABATEMENTS_AVAILABLE = MAX(GRX_CO2_DOMISIC_ABATEMENTS_AVAILABLE, &
                                                       Dom_Min_Off_Sets_To_Buy(YEAR))
            GRX_CO2_DOMISIC_ABATEMENTS_AVAILABLE = MIN(GRX_CO2_DOMISIC_ABATEMENTS_AVAILABLE, &
                                                       Dom_Max_Off_Sets_To_Buy(YEAR))
         ENDIF
         IF(PRESENT(CO2_CREDITS)) THEN
!               GRX_CO2_DOMISIC_ABATEMENTS_PRICE =  COEFF1(YEAR) *
!                                                      EXP(CO2_ABATEMENTS_PURCHASED* COEFF2(YEAR))
            IF(YEAR <= YR2_DATA .AND. YEAR >= YR1_DATA .AND. CO2_PARAM_FILE_EXISTS) THEN
               IF(INDEX(Dom_Price_Equation_1_Type(YEAR),"Expon") /= 0) THEN
                  GRX_CO2_PRICE = Dom_1_A_Coefficient(YEAR)*EXP(CO2_CREDITS*Dom_1_B_Coefficient(YEAR))
                  IF(GRX_CO2_PRICE > Dom_Price_Break(YEAR)) THEN
                     IF(INDEX(Dom_Price_Equation_2_Type(YEAR),"Expon") /= 0) THEN
                        GRX_CO2_PRICE = Dom_2_A_Coefficient(YEAR)*EXP(CO2_CREDITS*Dom_2_B_Coefficient(YEAR))
                     ELSE
                        GRX_CO2_PRICE = Dom_2_A_Coefficient(YEAR) + CO2_CREDITS*Dom_2_B_Coefficient(YEAR)
                     ENDIF
                  ENDIF
               ELSE
                  GRX_CO2_PRICE = Dom_1_A_Coefficient(YEAR) + CO2_CREDITS*Dom_1_B_Coefficient(YEAR)
                  IF(GRX_CO2_PRICE > Dom_Price_Break(YEAR)) THEN
                     IF(INDEX(Dom_Price_Equation_2_Type(YEAR),"Expon") /= 0) THEN
                        GRX_CO2_PRICE = Dom_2_A_Coefficient(YEAR)*EXP(CO2_CREDITS*Dom_2_B_Coefficient(YEAR))
                     ELSE
                        GRX_CO2_PRICE = Dom_2_A_Coefficient(YEAR) + CO2_CREDITS*Dom_2_B_Coefficient(YEAR)
                     ENDIF
                  ENDIF

!                 GRX_CO2_DOMISIC_ABATEMENTS_PRICE =  COEFF1(YEAR) + COEFF2(YEAR) * CO2_ABATEMENTS_PURCHASED                                                       +
               ENDIF
               GRX_CO2_DOMISIC_ABATEMENTS_AVAILABLE = GRX_CO2_PRICE
            ELSE
               GRX_CO2_DOMISIC_ABATEMENTS_AVAILABLE = 0.
            ENDIF
         ENDIF
      END FUNCTION
!--------
      REAL FUNCTION GRX_CO2_INTRL_ABATEMENTS_AVAILABLE(YEAR,CO2_PRICE,CO2_CREDITS)
      USE CO2_CAP_N_TRADE
      REAL (KIND=4), OPTIONAL :: CO2_PRICE,CO2_CREDITS
      REAL (KIND=4) :: GRX_CO2_PRICE
      INTEGER (KIND=2) :: YEAR
         IF(PRESENT(CO2_PRICE)) THEN
            IF(YEAR <= YR2_DATA .AND. YEAR >= YR1_DATA .AND. CO2_PRICE > 0. .AND. CO2_PARAM_FILE_EXISTS) THEN
               IF(CO2_PRICE <= Int_Price_Break(YEAR)) THEN
                  IF(Int_1_B_Coefficient(YEAR) == 0.) THEN
                     GRX_CO2_INTRL_ABATEMENTS_AVAILABLE = 0.
                  ELSE
                     IF(INDEX(Int_Price_Equation_1_Type(YEAR),"Expon") /= 0) THEN
                        GRX_CO2_INTRL_ABATEMENTS_AVAILABLE=MAX(0.,(LOG(CO2_PRICE)-LOG(Int_1_A_Coefficient(YEAR)))/ &
                                                                        Int_1_B_Coefficient(YEAR))
                     ELSE
                        GRX_CO2_INTRL_ABATEMENTS_AVAILABLE = (CO2_PRICE-Int_1_A_Coefficient(YEAR))/  &
                                                                    Int_1_B_Coefficient(YEAR)
                     ENDIF
                  ENDIF
!                  GRX_CO2_INTRL_ABATEMENTS_PRICE =  COEFF1(YEAR) *
!                                                         EXP(CO2_ABATEMENTS_PURCHASED* COEFF2(YEAR))
               ELSE ! Equation Type 2
                  IF(Int_2_B_Coefficient(YEAR) == 0.) THEN
                     GRX_CO2_INTRL_ABATEMENTS_AVAILABLE = 0.
                  ELSE
                     IF(INDEX(Int_Price_Equation_2_Type(YEAR),"Expon") /= 0) THEN
                        GRX_CO2_INTRL_ABATEMENTS_AVAILABLE=MAX(0.,(LOG(CO2_PRICE)-LOG(Int_2_A_Coefficient(YEAR)))/ &
                                                                        Int_2_B_Coefficient(YEAR))
                     ELSE
                        GRX_CO2_INTRL_ABATEMENTS_AVAILABLE = (CO2_PRICE-Int_2_A_Coefficient(YEAR))/  &
                                                                    Int_2_B_Coefficient(YEAR)
                     ENDIF
                  ENDIF
           
!                  GRX_CO2_INTRL_ABATEMENTS_PRICE =  COEFF1(YEAR) + COEFF2(YEAR) * CO2_ABATEMENTS_PURCHASED                                                       +
               ENDIF
            ELSE
              GRX_CO2_INTRL_ABATEMENTS_AVAILABLE = 0.
            ENDIF
            GRX_CO2_INTRL_ABATEMENTS_AVAILABLE = MAX(GRX_CO2_INTRL_ABATEMENTS_AVAILABLE, &
                                                       Int_Min_Off_Sets_To_Buy(YEAR))
            GRX_CO2_INTRL_ABATEMENTS_AVAILABLE = MIN(GRX_CO2_INTRL_ABATEMENTS_AVAILABLE, &
                                                       Int_Max_Off_Sets_To_Buy(YEAR))
         ENDIF
         IF(PRESENT(CO2_CREDITS)) THEN
!               GRX_CO2_DOMISIC_ABATEMENTS_PRICE =  COEFF1(YEAR) *
!                                                      EXP(CO2_ABATEMENTS_PURCHASED* COEFF2(YEAR))
            IF(YEAR <= YR2_DATA .AND. YEAR >= YR1_DATA .AND. CO2_PARAM_FILE_EXISTS) THEN
               IF(INDEX(Int_Price_Equation_1_Type(YEAR),"Expon") /= 0) THEN
                  GRX_CO2_PRICE = Int_1_A_Coefficient(YEAR)*EXP(CO2_CREDITS*Int_1_B_Coefficient(YEAR))
                  IF(GRX_CO2_PRICE > Int_Price_Break(YEAR)) THEN
                     IF(INDEX(Int_Price_Equation_2_Type(YEAR),"Expon") /= 0) THEN
                        GRX_CO2_PRICE = Int_2_A_Coefficient(YEAR)*EXP(CO2_CREDITS*Int_2_B_Coefficient(YEAR))
                     ELSE
                        GRX_CO2_PRICE = Int_2_A_Coefficient(YEAR) + CO2_CREDITS*Int_2_B_Coefficient(YEAR)
                     ENDIF
                  ENDIF
               ELSE
                  GRX_CO2_PRICE = Int_1_A_Coefficient(YEAR) + CO2_CREDITS*Int_1_B_Coefficient(YEAR)
                  IF(GRX_CO2_PRICE > Int_Price_Break(YEAR)) THEN
                     IF(INDEX(Int_Price_Equation_2_Type(YEAR),"Expon") /= 0) THEN
                        GRX_CO2_PRICE = Int_2_A_Coefficient(YEAR)*EXP(CO2_CREDITS*Int_2_B_Coefficient(YEAR))
                     ELSE
                        GRX_CO2_PRICE = Int_2_A_Coefficient(YEAR) + CO2_CREDITS*Int_2_B_Coefficient(YEAR)
                     ENDIF
                  ENDIF

!                 GRX_CO2_DOMISIC_ABATEMENTS_PRICE =  COEFF1(YEAR) + COEFF2(YEAR) * CO2_ABATEMENTS_PURCHASED                                                       +
               ENDIF
               GRX_CO2_INTRL_ABATEMENTS_AVAILABLE = GRX_CO2_PRICE
            ELSE
               GRX_CO2_INTRL_ABATEMENTS_AVAILABLE = 0.
            ENDIF
         ENDIF
      END FUNCTION
!--------
      REAL FUNCTION GRX_NON_ELECTRIC_SECTOR_CO2_EMISSIONS(YEAR)
      USE CO2_CAP_N_TRADE
      INTEGER (KIND=2) :: YEAR
         IF(YEAR <= YR2_DATA .AND. YEAR >= YR1_DATA .AND. CO2_PARAM_FILE_EXISTS) THEN
            GRX_NON_ELECTRIC_SECTOR_CO2_EMISSIONS = 10.**6*(Residential_Emissions(YEAR) &
                                               + Commercial_Emission(YEAR) &
                                               + Industrial_Emissions(YEAR) &
                                               + Transportation_Emissions(YEAR))
         ELSE
            GRX_NON_ELECTRIC_SECTOR_CO2_EMISSIONS = 0.
         ENDIF

      END FUNCTION
!--------
      REAL FUNCTION GRX_CO2_BANK_DELTA(YEAR)
      USE CO2_CAP_N_TRADE
      INTEGER (KIND=2) :: YEAR
         IF(YEAR <= YR2_DATA .AND. YEAR >= YR1_DATA .AND. CO2_PARAM_FILE_EXISTS) THEN
            GRX_CO2_BANK_DELTA = 10**6*CO2_Banking(YEAR)
         ELSE
            GRX_CO2_BANK_DELTA = 0.
         ENDIF
      END FUNCTION
!--------
      REAL FUNCTION CO2_BANKED_BALANCE(UPDATE,Model_Purchases)
      USE CO2_CAP_N_TRADE
      LOGICAL (KIND=4) :: UPDATE
      REAL (KIND=4) :: Model_Purchases
         CO2_BANKED_BALANCE = CO2_BANK_BALANCE + Model_Purchases
         IF(UPDATE) CO2_BANK_BALANCE = CO2_BANK_BALANCE + Model_Purchases
      END FUNCTION
!--------
      REAL FUNCTION GRX_ALL_SECTOR_CO2_EMISSIONS_CAP(YEAR)
      USE CO2_CAP_N_TRADE
      INTEGER (KIND=2) :: YEAR
         IF(YEAR <= YR2_DATA .AND. YEAR >= YR1_DATA .AND. CO2_PARAM_FILE_EXISTS) THEN
            GRX_ALL_SECTOR_CO2_EMISSIONS_CAP = 10**6*CO2_Emission_Cap(YEAR)
         ELSE
            GRX_ALL_SECTOR_CO2_EMISSIONS_CAP = 0.
         ENDIF
      END FUNCTION
!--------
      REAL FUNCTION GRX_CO2_ABATEMENTS_AVAILABLE(CO2_PRICE,YEAR)
      USE NEWTON_INTERFACE
      USE CO2_CAP_N_TRADE
      REAL (KIND=4) :: CO2_PRICE
      INTEGER (KIND=2) :: YEAR
         IF(CO2_Market_Available(YEAR)) THEN
            GRX_CO2_ABATEMENTS_AVAILABLE = GRX_CO2_OTHER_ABATEMENTS_AVAILABLE(YEAR,CO2_PRICE=CO2_PRICE) &
                                         + GRX_CO2_DOMISIC_ABATEMENTS_AVAILABLE(YEAR,CO2_PRICE=CO2_PRICE) &
                                         + GRX_CO2_INTRL_ABATEMENTS_AVAILABLE(YEAR,CO2_PRICE=CO2_PRICE)
            GRX_CO2_ABATEMENTS_AVAILABLE = 10**6*GRX_CO2_ABATEMENTS_AVAILABLE
         ELSE
            GRX_CO2_ABATEMENTS_AVAILABLE = 0.
         ENDIF
      END FUNCTION
!--------
      REAL FUNCTION GRX_ABATEMENT_PRICE(CO2_ABATEMENTS,YEAR)
         USE NEWTON_INTERFACE
         USE CO2_CAP_N_TRADE
         INTEGER (KIND=2) :: YEAR,I
         REAL (KIND=4) :: CO2_PRICE,CO2_ABATEMENTS,CO2_ABATEMENTS_AT_PRICE
         REAL GRX_CO2_ABATEMENTS_AVAILABLE
         REAL (KIND=4) :: SAVE_CO2_ABATEMENTS,MAX_ABATEMENTS_AVAIL
!
         SAVE_CO2_ABATEMENTS = CO2_ABATEMENTS
         MAX_ABATEMENTS_AVAIL = 10**6*(Non_Elect_Max_Reductions(YEAR) &
                                     + Dom_Max_Off_Sets_To_Buy(YEAR) &
                                     + Int_Max_Off_Sets_To_Buy(YEAR))
         IF(CO2_ABATEMENTS <= 0.) THEN
            CO2_PRICE = 0.
         ELSEIF(CO2_ABATEMENTS < MAX_ABATEMENTS_AVAIL) THEN
            CO2_PRICE = 5.
            DO WHILE (GRX_CO2_ABATEMENTS_AVAILABLE(CO2_PRICE,YEAR) < CO2_ABATEMENTS)
               CO2_PRICE = CO2_PRICE + 5
            ENDDO
            CO2_PRICE = CO2_PRICE - 5
            DO WHILE (GRX_CO2_ABATEMENTS_AVAILABLE(CO2_PRICE,YEAR) < CO2_ABATEMENTS)
               CO2_PRICE = CO2_PRICE + 1
            ENDDO
            CO2_PRICE = CO2_PRICE - 1
            DO WHILE (GRX_CO2_ABATEMENTS_AVAILABLE(CO2_PRICE,YEAR) < CO2_ABATEMENTS)
               CO2_PRICE = CO2_PRICE + .1
            ENDDO
            CO2_PRICE = CO2_PRICE - .1
            DO WHILE (GRX_CO2_ABATEMENTS_AVAILABLE(CO2_PRICE,YEAR) < CO2_ABATEMENTS)
               CO2_PRICE = CO2_PRICE + .01
            ENDDO
            CO2_PRICE = CO2_PRICE - .005
!            CO2_PRICE = CO2_PRICE - 5 + 5 * (CO2_ABATEMENTS- GRX_CO2_ABATEMENTS_AVAILABLE(CO2_PRICE-5.,YEAR))/ &
!               (GRX_CO2_ABATEMENTS_AVAILABLE(CO2_PRICE,YEAR) - GRX_CO2_ABATEMENTS_AVAILABLE(CO2_PRICE-5.,YEAR))
!               IF(ABS_ERROR(CO2_ABATEMENTS,CO2_ABATEMENTS_AT_PRICE,.01)) EXIT
!               IF(CO2_ABATEMENTS_AT_PRICE < CO2_ABATEMENTS_AT_PRICE) THEN
!                  CO2_PRICE = NEWTON_ABAT_PRICE_SEARCH(MAX_POINT=CO2_PRICE, &
!                                                     MAX_CO2=CO2_ABATEMENTS_AT_PRICE, &
!                                                     CO2_CAP=CO2_ABATEMENTS)
!!               ELSE
!                  CO2_PRICE = NEWTON_ABAT_PRICE_SEARCH(MIN_POINT=CO2_PRICE, &
!                                                     MIN_CO2=CO2_ABATEMENTS_AT_PRICE, &
!                                                     CO2_CAP=CO2_ABATEMENTS)
!               ENDIF
!            ENDDO
         ELSE
            CO2_PRICE = CO2_Max_Price_Cap(YEAR)
            CO2_ABATEMENTS = MAX_ABATEMENTS_AVAIL
         ENDIF
         GRX_ABATEMENT_PRICE = CO2_PRICE
      END FUNCTION



!-------
      FUNCTION UNITS_BEFORE_ADDITIONS()
      USE GRX_PLANNING_ROUTINES
      INTEGER (KIND=2) :: UNITS_BEFORE_ADDITIONS
         UNITS_BEFORE_ADDITIONS = GRX_NUNITS 
      END FUNCTION
!-------
      SUBROUTINE GRX_RETROFIT_UNIT_LIST(CO2_UNIT,CO2_RETRO_TYPE,CO2_DECISION_COST)
      USE GRX_PLANNING_ROUTINES
      INTEGER (KIND=2) :: CO2_UNIT,CO2_RETRO_TYPE
      REAL (KIND=4) :: CO2_DECISION_COST
         IF(UNITS_RETROFITTED == 0) THEN
            RETROFIT_LIST = 0
            RETROFIT_TYPE_FIXED_GRX = 0
            RETROFIT_CO2_DECISION_COST = 0.
         ENDIF
         UNITS_RETROFITTED =  UNITS_RETROFITTED + 1
         RETROFIT_LIST(UNITS_RETROFITTED) = CO2_UNIT
         RETROFIT_TYPE_FIXED_GRX(CO2_UNIT) = CO2_RETRO_TYPE
         RETROFIT_CO2_DECISION_COST(CO2_UNIT) = CO2_DECISION_COST
      END SUBROUTINE
!-------
      LOGICAL FUNCTION GRX_RETROFITED_UNIT(CO2_UNIT,RETRO_CAP_CO2_MULT)
      USE GRX_PLANNING_ROUTINES
      INTEGER (KIND=2) :: CO2_UNIT,RETRO_ID,RETROFIT_ACTIVE_ID
      REAL (KIND=4) :: RETRO_CAP_CO2_MULT, &
                       CO2_Min_Capacity_Change, &
                       CO2_Max_Capacity_Change, &
                       CO2_HEATRATE_MULITPLIER, &
                       CO2_REMOVAL_PERCENTAGE
!
         GRX_RETROFITED_UNIT = .FALSE.
         IF(RETROFIT_TYPE_FIXED_GRX(CO2_UNIT) == 2) THEN
            RETRO_ID = RETROFIT_ACTIVE_ID(CO2_UNIT)
            CALL RETURN_RETROFIT_PROJECT_IMPACTS(RETRO_ID,  &
                                                 CO2_Min_Capacity_Change, &
                                                 CO2_Max_Capacity_Change, &
                                                 CO2_HEATRATE_MULITPLIER, &
                                                 CO2_REMOVAL_PERCENTAGE)
            RETRO_CAP_CO2_MULT = CO2_Max_Capacity_Change
            GRX_RETROFITED_UNIT = .TRUE.
         ENDIF
      END FUNCTION
!-------
      SUBROUTINE GRX_RETIREMENT_RETROFIT_RPT(BASE_YEAR,YEAR,YEAR_END)
      USE CLA_OBJT_ARRAYS
      USE CAPACITY_OPTIONS_ALLOC_VARIABLES
      USE CONVERSTION_ROUTINES
      USE TRANS_GROUP_VARIABLES
      USE GRX_PLANNING_ROUTINES
      USE ANNLCOMMON
      USE INTERNAL_OPERATION_SWITCHES
      LOGICAL (KIND=1), SAVE :: RPT_FILE_OPEN/.FALSE./,RPT_FILE_OPEN_2/.FALSE./
      CHARACTER (LEN=30) :: H2_STR30,EV_STR30,DATE_STR
      CHARACTER (LEN=5) :: GET_SCENAME
      CHARACTER (LEN=7) :: OFF_LINE_STR
      INTEGER (KIND=2) :: UNIT_NO,TG,TG_POSITION,I,GET_TRANS_GROUP_POSITION, &
                          TRANSACTION_GROUP,YEAR,BASE_YEAR,YEAR_END, &
                          RETRO_ID,RETROFIT_ACTIVE_ID,GET_NUNITS,RETIREMENT_YEAR
      CHARACTER (LEN=20) :: RETURN_UNITNM
      REAL (KIND=4) :: CO2_Min_Capacity_Change,CO2_Max_Capacity_Change, &
                       CO2_HEATRATE_MULITPLIER,CO2_REMOVAL_PERCENTAGE, &
                       RETRO_VOM,RETRO_FOM,Decision_CO2_Price,TEMP_R4, &
                       CL_CAPACITY_PLANNING_ADJUSTMENTS
      LOGICAL (KIND=1) :: GET_CO2_RETIREMENTS_LOGIC,CO2_RETROFIT_LOGIC_ACTIVE
      character (len=1024) :: recln
!
         IF(.NOT. (GET_CO2_RETIREMENTS_LOGIC() .OR. CO2_RETROFIT_LOGIC_ACTIVE())) RETURN
         IF(.NOT. RPT_FILE_OPEN) THEN
            OPEN(8337,FILE="GRX2EnterprizeRetireRetro-"//TRIM(GET_SCENAME())//".CSV",STATUS="REPLACE")
            IF(INSTALLED_POINTER + UNITS_RETROFITTED > 0) THEN
               WRITE(8337,"(1X,5A)") '"MktSymArea Names",MktSymAreaAbbrev,"Unit Name",', &
                '"EV ID","HESI ID",Date,ModType,"VOM Adder","FOM Adder","Max Capacity Derate Multiplier",', &
                '"Min Capacity Derate Multiplier","HR Multiplier","CO2 Removal Percent","MWs Retired",' , &
                '"Endpoint","Unit CO2 Cost","CO2 Price","EBITDA1","EBITDA2","EBITDA3","EBITDA4","Off-line Date"'
               RPT_FILE_OPEN = .TRUE.
               CALL FLUSH(8337)
            ELSE
               CLOSE(8337,STATUS='DELETE')
            ENDIF
         ENDIF
         IF(.NOT. RPT_FILE_OPEN_2) THEN
            OPEN(8339,FILE="GRXMRXRetireRetro-"//TRIM(GET_SCENAME())//".CSV",STATUS="REPLACE")
            IF(INSTALLED_POINTER + UNITS_RETROFITTED > 0) THEN
               WRITE(8339,"(1X,5A)") '"Transaction Group","Unit Name",', &
                '"EV ID",Date,ModType,"VOM Adder","FOM Adder","Max Capacity Derate Multiplier",', &
                '"Min Capacity Derate Multiplier","HR Multiplier","CO2 Removal Percent","MWs Retired",' , &
                '"Endpoint","Unit CO2 Cost","CO2 Price","EBITDA1","EBITDA2","EBITDA3","EBITDA4",', &
                '"Regional CO2 Price","Off-Line Date"'
               RPT_FILE_OPEN_2 = .TRUE.
               CALL FLUSH(8339)
            ELSE
               CLOSE(8339,STATUS='DELETE')
            ENDIF
         ENDIF
         IF(GET_CO2_RETIREMENTS_LOGIC() .AND. INSTALLED_POINTER > 0) THEN
            RETIREMENT_YEAR = BASE_YEAR+YEAR+1
            DO I = 1, INSTALLED_POINTER
               UNIT_NO = TEMP_RETIRED_UNIT_NO(I)
               IF(GRX_RETIRE_THIS_YEAR .AND. RETIREMENT_YEAR <= YEAR_END) THEN
                  TEMP_R4 = CL_CAPACITY_PLANNING_ADJUSTMENTS(RETIREMENT_YEAR,UNIT_NO,2,.TRUE.)
               ENDIF
               RETROFIT_CANDIDATE(UNIT_NO) = 'F'
               RETIREMENT_CANDIDATE(UNIT_NO) = 'F'
               FIRST_RETIREMENT_YEAR(UNIT_NO) = 9999 ! RETIRED
               TG = MAX(1,TRANSACTION_GROUP(UNIT_NO))
               TG_POSITION = MAX(1,GET_TRANS_GROUP_POSITION(TG))
               H2_STR30 = CONVERT_2_STR(H2_UNIT_ID_NUM(UNIT_NO))
               EV_STR30 = CONVERT_2_STR(HESI_UNIT_ID_NUM(UNIT_NO))   ! EV ID
               OFF_LINE_STR = TRIM(CONVERT_2_STR(INT4(OFF_LINE_MONTH(UNIT_NO))))// &
                           '/'//CONVERT_2_STR(INT4(OFF_LINE_YEAR(UNIT_NO)))
               IF(GRX_RETIRE_THIS_YEAR) THEN
                  DATE_STR = "12/31/"//CONVERT_2_STR(INT4(BASE_YEAR+YEAR))
               ELSE
                  DATE_STR = "12/31/"//CONVERT_2_STR(INT4(BASE_YEAR+YEAR-1))
               ENDIF
               recln = '"'//TRIM(TRANS_GROUP_FULL_NAME(TG_POSITION))//'",'
               recln = trim(recln)//'"'//TRIM(TG_BASECASE_MARKET_AREA_ID(TG_POSITION))//'",' ! MktSymAreaAbbrev
               recln = trim(recln)//'"'//TRIM(RETURN_UNITNM(UNIT_NO))//'",'    ! Unit Name
               recln = trim(recln)//TRIM(EV_STR30)//','     ! EV ID
               recln = trim(recln)//TRIM(H2_STR30)//','     ! HESI ID
               recln = trim(recln)//'"'//TRIM(DATE_STR)//'",Retirement,'// & ! Date
                                   '0.,0.,0.,0.,0.,100.,'// &
                                   TRIM(CONVERT_2_STR(RETIREMENTS_MW_RPT(UNIT_NO)))//','
               Decision_CO2_Price = sum(ECON_RETIRE_MARGIN(UNIT_NO,1:4))
               recln = trim(recln)//TRIM(CONVERT_2_STR(PRT_ENDPOINT()))//','// &         ! Endpoint
                                TRIM(CONVERT_2_STR(Decision_CO2_Price))//','// &
                                TRIM(CONVERT_2_STR(CURRENT_CO2_DISPATCH_COST))//','
               recln = trim(recln)//TRIM(CONVERT_2_STR(ECON_RETIRE_MARGIN(UNIT_NO,1)))//','// &
                                TRIM(CONVERT_2_STR(ECON_RETIRE_MARGIN(UNIT_NO,2)))//','// &         ! Endpoint
                                TRIM(CONVERT_2_STR(ECON_RETIRE_MARGIN(UNIT_NO,3)))//','// &
                                TRIM(CONVERT_2_STR(ECON_RETIRE_MARGIN(UNIT_NO,4)))//','// &
                                TRIM(OFF_LINE_STR)//','
               WRITE(8337,'(A)') trim(recln)   ! Enterprize Interface
! Doug requested interface to Alan program
               recln = '"'//TRIM(TRANS_GROUP_FULL_NAME(TG_POSITION))//'",'
               recln = trim(recln)//'"'//TRIM(RETURN_UNITNM(UNIT_NO))//'",'    ! Unit Name
               recln = trim(recln)//TRIM(EV_STR30)//','     ! EV ID
               recln = trim(recln)//'"'//TRIM(DATE_STR)//'",Retirement,'// & ! Date
                                   '0.,0.,0.,0.,0.,100.,'// &
                                   TRIM(CONVERT_2_STR(RETIREMENTS_MW_RPT(UNIT_NO)))//','
               Decision_CO2_Price = sum(ECON_RETIRE_MARGIN(UNIT_NO,1:4))
               recln = trim(recln)//TRIM(CONVERT_2_STR(PRT_ENDPOINT()))//','// &         ! Endpoint
                                TRIM(CONVERT_2_STR(Decision_CO2_Price))//','// &
                                TRIM(CONVERT_2_STR(CURRENT_CO2_DISPATCH_COST))//','
               recln = trim(recln)//TRIM(CONVERT_2_STR(ECON_RETIRE_MARGIN(UNIT_NO,1)))//','// &
                                TRIM(CONVERT_2_STR(ECON_RETIRE_MARGIN(UNIT_NO,2)))//','// &         ! Endpoint
                                TRIM(CONVERT_2_STR(ECON_RETIRE_MARGIN(UNIT_NO,3)))//','// &
                                TRIM(CONVERT_2_STR(ECON_RETIRE_MARGIN(UNIT_NO,4)))//','// &
                                TRIM(CONVERT_2_STR(RETIREMENT_CO2_COST_PER_TON(UNIT_NO)))//',"'// &
                                TRIM(OFF_LINE_STR)//'",'
               WRITE(8339,'(A)') trim(recln)   ! Alan Interface
            ENDDO
            CALL FLUSH(8337)
            CALL FLUSH(8339)
         ENDIF
         IF(.NOT. (CO2_RETROFIT_LOGIC_ACTIVE() .AND.  UNITS_RETROFITTED > 0)) RETURN
         DO I = 1, UNITS_RETROFITTED
            UNIT_NO = RETROFIT_LIST(I)
            RETIREMENT_CANDIDATE(UNIT_NO) = 'F'
            FIRST_RETIREMENT_YEAR(UNIT_NO) = 9999 ! RETIRED
            RETROFIT_CANDIDATE(UNIT_NO) = 'F'
            TG = MAX(1,TRANSACTION_GROUP(UNIT_NO))
            TG_POSITION = MAX(1,GET_TRANS_GROUP_POSITION(TG))
            RETRO_ID = RETROFIT_ACTIVE_ID(UNIT_NO)
            CALL RETROFIT_VOM_FOM_ADDERS(RETRO_ID,RETRO_VOM,RETRO_FOM)
            CALL RETURN_RETROFIT_PROJECT_IMPACTS(RETRO_ID,  &
                                                 CO2_Min_Capacity_Change, &
                                                 CO2_Max_Capacity_Change, &
                                                 CO2_HEATRATE_MULITPLIER, &
                                                 CO2_REMOVAL_PERCENTAGE)
            CO2_Min_Capacity_Change = 1.-CO2_Min_Capacity_Change
            CO2_Max_Capacity_Change = 1.-CO2_Max_Capacity_Change
            H2_STR30 = CONVERT_2_STR(H2_UNIT_ID_NUM(UNIT_NO))
            EV_STR30 = CONVERT_2_STR(HESI_UNIT_ID_NUM(UNIT_NO))   ! EV ID
            DATE_STR = "1/1/"//CONVERT_2_STR(INT4(BASE_YEAR+YEAR))
            recln = '"'//TRIM(TRANS_GROUP_FULL_NAME(TG_POSITION))//'","'//  &
                         TRIM(TG_BASECASE_MARKET_AREA_ID(TG_POSITION))//'","'// & ! MktSymAreaAbbrev
                         TRIM(RETURN_UNITNM(UNIT_NO))//'",'    ! Unit Name
            recln = trim(recln)//TRIM(EV_STR30)//','// & ! EV ID
                                 TRIM(H2_STR30)//','     ! HESI ID
            IF(RETROFIT_TYPE_FIXED_GRX(UNIT_NO) == 4) THEN
               recln = trim(recln)//'"'//TRIM(DATE_STR)//'","Fixed Retrofit",'  ! Date
            ELSE
               recln = trim(recln)//'"'//TRIM(DATE_STR)//'","GRX Retrofit",'  ! Date
            ENDIF
            recln = trim(recln)//TRIM(CONVERT_2_STR(RETRO_VOM))//','// & ! VOM Adder
                          TRIM(CONVERT_2_STR(RETRO_FOM))//','// & ! FOM Adder
                          TRIM(CONVERT_2_STR(CO2_Min_Capacity_Change))//','// &! Capacity
                          TRIM(CONVERT_2_STR(CO2_Max_Capacity_Change))//','  ! Derate Multipliear
            recln = trim(recln)//TRIM(CONVERT_2_STR(CO2_HEATRATE_MULITPLIER))//','// &! HR Multiplier
                          TRIM(CONVERT_2_STR(CO2_REMOVAL_PERCENTAGE))//','// & ! CO2 Removal Percent
                          TRIM(CONVERT_2_STR(RETIREMENTS_MW_RPT(UNIT_NO)))//','
            recln = trim(recln)//TRIM(CONVERT_2_STR(PRT_ENDPOINT()))//','// &      ! Endpoint
                          TRIM(CONVERT_2_STR(RETROFIT_CO2_DECISION_COST(UNIT_NO)))//','// &
                          TRIM(CONVERT_2_STR(CURRENT_CO2_DISPATCH_COST))//','// &
                          TRIM(CONVERT_2_STR(ECON_RETIRE_MARGIN(UNIT_NO,1)))//','// &
                          TRIM(CONVERT_2_STR(ECON_RETIRE_MARGIN(UNIT_NO,2)))//','// &
                          TRIM(CONVERT_2_STR(ECON_RETIRE_MARGIN(UNIT_NO,3)))//','// &
                          TRIM(CONVERT_2_STR(ECON_RETIRE_MARGIN(UNIT_NO,4)))//','// &
                          TRIM(OFF_LINE_STR)//','
            WRITE(8337,'(A)') trim(recln)
! Alan interface
            recln = '"'//TRIM(TRANS_GROUP_FULL_NAME(TG_POSITION))//'","'//  &
                         TRIM(RETURN_UNITNM(UNIT_NO))//'",'    ! Unit Name
            recln = trim(recln)//TRIM(EV_STR30)//','  ! EV ID
            IF(RETROFIT_TYPE_FIXED_GRX(UNIT_NO) == 4) THEN
               recln = trim(recln)//'"'//TRIM(DATE_STR)//'","Fixed Retrofit",'  ! Date
            ELSE
               recln = trim(recln)//'"'//TRIM(DATE_STR)//'","GRX Retrofit",'  ! Date
            ENDIF
            recln = trim(recln)//TRIM(CONVERT_2_STR(RETRO_VOM))//','// & ! VOM Adder
                          TRIM(CONVERT_2_STR(RETRO_FOM))//','// & ! FOM Adder
                          TRIM(CONVERT_2_STR(CO2_Min_Capacity_Change))//','// &! Capacity
                          TRIM(CONVERT_2_STR(CO2_Max_Capacity_Change))//','  ! Derate Multipliear
            recln = trim(recln)//TRIM(CONVERT_2_STR(CO2_HEATRATE_MULITPLIER))//','// &! HR Multiplier
                          TRIM(CONVERT_2_STR(CO2_REMOVAL_PERCENTAGE))//','// & ! CO2 Removal Percent
                          TRIM(CONVERT_2_STR(RETIREMENTS_MW_RPT(UNIT_NO)))//','
            recln = trim(recln)//TRIM(CONVERT_2_STR(PRT_ENDPOINT()))//','// &      ! Endpoint
                          TRIM(CONVERT_2_STR(RETROFIT_CO2_DECISION_COST(UNIT_NO)))//','// &
                          TRIM(CONVERT_2_STR(CURRENT_CO2_DISPATCH_COST))//','// &
                          TRIM(CONVERT_2_STR(ECON_RETIRE_MARGIN(UNIT_NO,1)))//','// &
                          TRIM(CONVERT_2_STR(ECON_RETIRE_MARGIN(UNIT_NO,2)))//','// &
                          TRIM(CONVERT_2_STR(ECON_RETIRE_MARGIN(UNIT_NO,3)))//','// &
                          TRIM(CONVERT_2_STR(ECON_RETIRE_MARGIN(UNIT_NO,4)))//','// &
                          TRIM(CONVERT_2_STR(RETIREMENT_CO2_COST_PER_TON(UNIT_NO)))//',"'// &
                          TRIM(OFF_LINE_STR)//'",'
            WRITE(8339,'(A)') trim(recln)
         ENDDO
         CALL FLUSH(8337)
         CALL FLUSH(8339)
         UNITS_RETROFITTED = 0
 1000 FORMAT(3('"',A,'",'),2(A,','),2('"',A,'",'),20(A,','))
      END SUBROUTINE
!-------
      SUBROUTINE CO2_MARKET_REDUCTION(CO2_CREDITS_AVAILABLE, &
                                      CO2_CREDIT_PRICE)
!-------
        USE CONVERSTION_ROUTINES
        INTEGER*2 BASE_YEAR,YEAR,END_POINT,STUDY_PERIOD,ENDYR, &
               LAST_STUDY_YEAR,EXTENSION_PERIOD,LAST_EXTENSION_YEAR, &
               MAX_YEARS
        COMMON /GLOBAL_VARIABLES/ BASE_YEAR,YEAR,END_POINT,STUDY_PERIOD, &
               LAST_STUDY_YEAR,EXTENSION_PERIOD,LAST_EXTENSION_YEAR, &
               ENDYR,MAX_YEARS
        REAL (KIND=4) :: CO2_CREDITS_AVAILABLE,CO2_CREDIT_PRICE
!
        WRITE(9,1009) END_POINT,  &
                      YEAR+BASE_YEAR , &
                      YEAR+BASE_YEAR , &
                      "CO2 Market", &
                      "CO2 Mkt Purchase-"//CONVERT_2_STR(CO2_CREDIT_PRICE), &   ! 20 CHARACTERS
                      0., &
                      CO2_CREDITS_AVAILABLE, &
                      CO2_CREDIT_PRICE
!
 1009 FORMAT(1X,I4,5X,I4,4X,I4,2X,A,2X,A20,1X,F9.1,F12.0,F7.2)
      END SUBROUTINE
!-------
      LOGICAL FUNCTION ABS_ERROR(A,B,DELTA) RESULT(ARE_EQUAL)
      REAL (KIND=4) :: A,B,CHECK
      REAL (KIND=4), OPTIONAL :: DELTA
      IF(PRESENT(DELTA)) THEN
         CHECK = DELTA
      ELSE
         CHECK = .001
      ENDIF
      ARE_EQUAL = ABS(A-B)/ABS(A) <= CHECK
      END
!-------
      REAL FUNCTION NEWTON_MRKT_PRICE_SEARCH(MIN_POINT,MAX_POINT, &
                                             MIN_CO2,MAX_CO2,CO2_CAP) &
                                                                RESULT(NEW_PRICE)
         REAL (KIND=4), OPTIONAL :: MIN_POINT,MAX_POINT,MIN_CO2,MAX_CO2,CO2_CAP
         REAL (KIND=4), SAVE :: MIN_PRICE/0./,MAX_PRICE/0./, &
                                MIN_CO2_EMISS/0./,MAX_CO2_EMISS/0./,EMISS_CAP/0./
         IF(PRESENT(MAX_POINT)) MAX_PRICE = MAX_POINT
         IF(PRESENT(MIN_POINT)) MIN_PRICE = MIN_POINT
         IF(PRESENT(MIN_CO2)) MIN_CO2_EMISS = MIN_CO2
         IF(PRESENT(MAX_CO2)) MAX_CO2_EMISS = MAX_CO2
         IF(PRESENT(CO2_CAP)) EMISS_CAP = CO2_CAP
         IF(MIN_CO2_EMISS-MAX_CO2_EMISS == 0.) THEN
            NEW_PRICE = (MAX_PRICE + MIN_PRICE)/2.
         ELSE
            NEW_PRICE = (EMISS_CAP-MIN_CO2_EMISS)*(MIN_PRICE-MAX_PRICE)/ &
                         (MIN_CO2_EMISS-MAX_CO2_EMISS)+ MIN_PRICE
         ENDIF
      END FUNCTION
!-------
      REAL FUNCTION NEWTON_ABAT_PRICE_SEARCH(MIN_POINT,MAX_POINT, &
                                             MIN_CO2,MAX_CO2,CO2_CAP) &
                                                                RESULT(NEW_PRICE)
         REAL (KIND=4), OPTIONAL :: MIN_POINT,MAX_POINT,MIN_CO2,MAX_CO2,CO2_CAP
         REAL (KIND=4), SAVE :: MIN_PRICE/0./,MAX_PRICE/0./, &
                                MIN_CO2_EMISS/0./,MAX_CO2_EMISS/0./,EMISS_CAP/0./
         IF(PRESENT(MAX_POINT)) MAX_PRICE = MAX_POINT
         IF(PRESENT(MIN_POINT)) MIN_PRICE = MIN_POINT
         IF(PRESENT(MIN_CO2)) MIN_CO2_EMISS = MIN_CO2
         IF(PRESENT(MAX_CO2)) MAX_CO2_EMISS = MAX_CO2
         IF(PRESENT(CO2_CAP)) EMISS_CAP = CO2_CAP
         IF(MIN_CO2_EMISS-MAX_CO2_EMISS == 0.) THEN
            NEW_PRICE = (MAX_PRICE + MIN_PRICE)/2.
         ELSE
            NEW_PRICE = (EMISS_CAP-MIN_CO2_EMISS)*(MIN_PRICE-MAX_PRICE)/ &
                         (MIN_CO2_EMISS-MAX_CO2_EMISS)+ MIN_PRICE
         ENDIF
      END FUNCTION
!-------
      REAL FUNCTION GRX_NEWTON_CONVERGENCE(TG)
         REAL (KIND=4) :: GET_EMISS_CAP_FOR_CLASS, GET_CO2_EMISS_FOR_SYSTEM, &
                          CO2_EMISSIONS_CAP, CO2_EMISSIONS
         INTEGER (KIND=2) :: GRX_ITERATIONS/1/,TG
         LOGICAL (KIND=1) :: GRX_CONVERGED
         REAL (KIND=4) :: CURRENT_CO2_DISPATCH_COST/0./

         CO2_EMISSIONS_CAP = GET_EMISS_CAP_FOR_CLASS(3_2,TG)
         CO2_EMISSIONS = GET_CO2_EMISS_FOR_SYSTEM()
         IF(GRX_ITERATIONS == 1) THEN
            IF(CO2_EMISSIONS < CO2_EMISSIONS_CAP) THEN
               IF(CURRENT_CO2_DISPATCH_COST == 0.) THEN
                  GRX_CONVERGED = .TRUE.
               ENDIF
            ENDIF
         ENDIF
         GRX_NEWTON_CONVERGENCE = 1.
      END FUNCTION
!-------
      REAL FUNCTION CALCULATE_CO2_EMISS_FOR_SYSTEM()
        USE ANNLCOMMON
        INTEGER (KIND=2) :: I,GET_NUNITS
         CO2_EMISSIONS_FOR_SYSTEM = 0.
         DO I = 1, GET_NUNITS()
            CO2_EMISSIONS_FOR_SYSTEM = CO2_EMISSIONS_FOR_SYSTEM + ANNUAL_CL_UNIT_EMISSIONS(3,I)
         ENDDO
         CALCULATE_CO2_EMISS_FOR_SYSTEM = CO2_EMISSIONS_FOR_SYSTEM
      END
      REAL FUNCTION GET_CO2_EMISS_FOR_SYSTEM()
        USE ANNLCOMMON
        GET_CO2_EMISS_FOR_SYSTEM = CO2_EMISSIONS_FOR_SYSTEM
      END
!-------
      LOGICAL FUNCTION GRX_WITH_MRX_PLANNING()
         GRX_WITH_MRX_PLANNING = .true. ! .FALSE.
      END FUNCTION
!***********************************************************************
      LOGICAL FUNCTION HardWiredRetrofitProject(R_NUNITS)
       USE CLA_OBJT_ARRAYS
       INTEGER (KIND=2) :: R_NUNITS,RETROFIT_PROJECT_YEAR
       INTEGER (KIND=2) :: BASE_YEAR,YEAR,END_POINT,STUDY_PERIOD,ENDYR, &
                 LAST_STUDY_YEAR,EXTENSION_PERIOD,LAST_EXTENSION_YEAR, &
                 MAX_YEARS
       COMMON /GLOBAL_VARIABLES/ BASE_YEAR,YEAR,END_POINT,STUDY_PERIOD, &
               LAST_STUDY_YEAR,EXTENSION_PERIOD,LAST_EXTENSION_YEAR, &
               ENDYR,MAX_YEARS
       HardWiredRetrofitProject = .FALSE.
       IF(CO2_CONTROL_PERCENT(R_NUNITS) >= 0.) THEN
          RETROFIT_PROJECT_YEAR=(CO2_CONTROL_DATE(R_NUNITS)-10000)/100 &
                                   + 2000  ! should be a valid year ie 2028
          HardWiredRetrofitProject = BASE_YEAR < RETROFIT_PROJECT_YEAR &
                         .AND. RETROFIT_PROJECT_YEAR <= LAST_STUDY_YEAR
       ENDIF
      END FUNCTION
      LOGICAL FUNCTION HardWiredRetrofitProjectThisYear(R_NUNITS)
       USE CLA_OBJT_ARRAYS
       INTEGER (KIND=2) :: R_NUNITS,RETROFIT_PROJECT_YEAR
       INTEGER (KIND=2) :: BASE_YEAR,YEAR,END_POINT,STUDY_PERIOD,ENDYR, &
                 LAST_STUDY_YEAR,EXTENSION_PERIOD,LAST_EXTENSION_YEAR, &
                 MAX_YEARS
       COMMON /GLOBAL_VARIABLES/ BASE_YEAR,YEAR,END_POINT,STUDY_PERIOD, &
               LAST_STUDY_YEAR,EXTENSION_PERIOD,LAST_EXTENSION_YEAR, &
               ENDYR,MAX_YEARS
       HardWiredRetrofitProjectThisYear = .FALSE.
       IF(CO2_CONTROL_PERCENT(R_NUNITS) >= 0.) THEN
          RETROFIT_PROJECT_YEAR=(CO2_CONTROL_DATE(R_NUNITS)-10000)/100 &
                                 + 2000  ! should be a valid year ie 2028
          HardWiredRetrofitProjectThisYear = RETROFIT_PROJECT_YEAR == BASE_YEAR + YEAR
       ENDIF
      END FUNCTION

