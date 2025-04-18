!     Last change: msg 9/11/2021 10:07:35 AM
!     msgnorm
!***********************************************************************
      FUNCTION MIDAS_NORMAL_SIMULATION()
!***********************************************************************
!
!
! VARIABLES TO SIZE THE MODEL
!
      use SpinDriftLib
      use prod_arrays_dimensions
      use grx_planning_routines
      use endpoint
      USE IREC_ENDPOINT_CONTROL
      USE FINANCIAL_SWITCHES_COMMON
      USE GRX_PLANNING_ROUTINES
      USE CLA_OBJT_ARRAYS
      USE CO2_CAP_N_TRADE
      USE ABB_CapMarketRptData
      use cl_data
      use rptreccontrol
      use grxModules
      use grx_data
      use dr_booth_modules
      use cls_load
      use tim
      use dreptcom
      USE SIZECOM
      use globecom
      use foshydi2com
      use contracts_data
      use envircom
      use prodcom

!
! LP COAL MODEL
!
      LOGICAL (KIND=4) :: READ_BASIN_SUPPLY_SO2_DATA,
     +                    READ_PLANT_CONT_DEMAND_LINK_DTA,
     +                    READ_GEN_PLANT_DEMAND_LINK_DATA,
     +                    READ_COAL_ESCALATION_FILE,
     +                    CoalExistingPlant,CoalGenericPlant
      REAL (KIND=4) :: TestFactor
!
! GRX VARIABLES
!
      INTEGER (KIND=2) :: RETIRED_UNITS,
     +                    RESET_ANNUAL_UNITS_LEFT,I
      REAL (KIND=4) :: INIT_ANNUAL_GROSS_MARGIN,VOID_REAL
      LOGICAL (KIND=1) :: VOID_LOGICAL,ZERO_TOTAL_EMIS_VARS,
     +                    GET_CO2_RETIREMENTS_LOGIC,
     +                    CO2_RETROFIT_LOGIC_ACTIVE,
     +                    ADJUST_CO2_RETRO_PLAN_CAP,
     +                    GRX_SAVE_DERIV_PLAN_VALUES,
     +                    GRX_SAVE_RPS_PROGRAM_VALUES
      INTEGER (KIND=2) :: MAX_GRX_ITERs
      LOGICAL (KIND=4) :: FILE_IS_OPEN,OneMoreIter
! END GRX VARIBLES
      LOGICAL (kind=1) ::  ASSET_ANALYST_ONLY
      LOGICAL (kind=1) ::  SET_ASSET_ANALYST_ONLY_SWITCH,
     +          TRANSACT_ANALYST_ONLY,INIT_FILE_FOUND,
     +          INIT_FILE_IS_ACTIVE,
     +          gas_lp_active,LP_GAS_MODEL,
     +          TEMP_L,GAS_PRICING_ROUTINE,
     +          WRITE_LNG_TO_DAT,
     +          MULTI_YEAR_ENERGY_PRODUCTS
      INTEGER (kind=2) ::  EL_UNITS_READ,CL_UNITS_READ,CONTRACTS_READ
      INTEGER (kind=2) ::  VOID_INT2
      INTEGER (kind=2) ::  YEAR_LOOP,SET_ESCALATION_VECTOR_STRUCTURE
      INTEGER (kind=2) ::  START_YEAR,MIDAS_NORMAL_SIMULATION
      INTEGER (kind=2) ::  SETUP_DAY_OF_WEEK_4,DAYS_IN_EACH_MONTH,MONTH,
     +          GET_PRB_SEQUENCE_NUMBER,
     +          R_MONTH
      INTEGER ::  IOS
      LOGICAL (KIND=1) :: SP_CAPEX_ACTIVE,SP_CAPEX_DISPLAY_ACTIVE
      LOGICAL (kind=1) ::  CL_RESET_PRICES,
     +          EL_RESET_PRICES,
     +          CONTRACT_RESET_OPTIONS,
     +          DSM_RESET_OPTIONS,
     +          MARKET_GROUPS,
     +          READ_MARKET_GROUPS_DATA,
     +          WVPA,IPALCO,
     +          QUICKSILVER,
     +          SIMSTATUS_REPORT,
     +          SIMSTATUS_ACTIVE,
     +          UPDATE_STATUS_REPORT,
     +          GAS_MODEL_ONLY,YES_GAS_MODEL_ONLY,
     +          RESET_INTO_EXTENSION_PERIOD

      INTEGER (kind=2) ::  RESET_EL_UNITS,
     +          RESET_CL_UNITS
      CHARACTER (LEN=10) :: START_SIMTIME,START_SIMDATE,START_SIMZONE,
     +                      SIMTIME,SIMDATE,SIMZONE
      INTEGER :: START_SIMDT(10),SIMDT(10),SIM_REC
      SAVE      SIMSTATUS_REPORT,
     +          START_SIMTIME,START_SIMDATE,START_SIMZONE,
     +          SIMTIME,SIMDATE,SIMZONE,
     +          START_SIMDT,SIMDT,SIM_REC

!
! PALO ALTO ITEMS
!
      CHARACTER (len=1) ::  PALO_ALTO
      PARAMETER(PALO_ALTO='P')
!
      LOGICAL (kind=1) ::    RUN_FUTURE_ASSETS,CAPACITY_WAS_ADDED,
     +            READ_REFERENCE_LOAD_DATA,
     +            EMISSIONS_CREDITS,
     +            ENVIR_DATA_READ,
     +            ANNUAL_ENERGY_PRODUCTS,
     +            ANNUAL_TRANSACTION_LOADS,
     +            ANNUAL_GAS_DEMAND,
     +            CALC_ANNUAL_SUPPLY_CURVE_PRICE,
     +            MONTHLY_TRANSACTION_LOADS,
     +            WEEKLY_HYDRO_ACTIVE,
     +            MANAGE_WEEKLY_HYDRO_FORECASTS,
     +            MANAGE_RPS_REQUIREMENTS,
     +            MANAGE_RPS_PROGRAM_FORECASTS,
     +            MANAGE_CO2_PARAM_FORECASTS,
     +            RESET_WARNINGS
      CHARACTER (len=128) ::  SIMLINE
      CHARACTER (len=256) ::  OUTPUT_DIRECTORY
      INTEGER (kind=2) ::  YEARS_TO_RUN,
     +          LAST_YEAR,
     +          LAST_LOOP_YEAR,
     +          ENDPOINTS_RUN,
     +          TEMP_I2,
     +          STARTING_END_POINT
      INTEGER (kind=4) ::  REC_LENGHT
      INTEGER (kind=2) ::  I2_REC_LENGHT
      INTEGER (kind=4) ::  SYSTEM_CAPACITY,
     +          PROCOST
      REAL (kind=4) ::  TEMP_R,UPDATE_HH_PRICE
      REAL ::  GET_ENERGY_EXCHANGE_ADJUSTMENT
      REAL (kind=8) ::  GET_TOTAL_SALES_REVENUE
!     VARIABLES FOR COST OF SERVICE
      LOGICAL (kind=1) ::  FIRST_END_POINT,
     +          CAPACITY_PLANNING_ACTIVE,
     +          CAPACITY_PLANNING_IS_ACTIVE,
     +          OBJECTIVE_FUNCTION_ACTIVE,
     +          READ_PRICE_FEEDBACK_DATA,
     +          READ_SCENARIO_MAKER_DATA,
     +          READ_LH_GLOBAL_FILE,
     +          READ_REGIONAL_OUTAGES_DATA,
     +          READ_REGIONAL_PARAMS_DATA,
     +          MANAGE_TRAN_EXP_FORECASTS,
     +          MANAGE_GAS_NODE_FORECASTS,
     +          MANAGE_GAS_SUPPLY_FORECASTS,
     +          MANAGE_GAS_DEMAND_FORECASTS,
     +          MANAGE_GAS_LINK_FORECASTS,
     +          MANAGE_GAS_STORAGE_FORECASTS,
     +          FIRST_PASS,FIRST_PASS_IS_ACTIVE,
     +          PRICE_PLANNING_FIRST_PASS,
     +          UPDATE_PRODUCTION_PARAMETERS,
     +          READ_ENERGY_PRODUCTS_DATA,
     +          READ_TRANS_CONSTRAINT_DATA,
     +          READ_TRANS_PATH_DATA,
     +          READ_TRANS_GROUPS_DATA
      LOGICAL (kind=4) ::  PRODP_FILE_EXISTS
      LOGICAL (kind=4) ::  PRODUCTION_PARAMETERS_OBJECT,
     +          FILE_OPENED,
     +          ERROR_FILE_OPEN,
     +          ERROR_FILE_EXISTS
      SAVE FIRST_PASS

!
! TYPE DECLARATION FOR THE CHECKING THE HISTORICAL LOAD DATA
!
! 2/14/93. GAT. ADDED DAY_* FOR KCPL. 3/3/93. GAT. ALTERED FOR HOURLY_IN
!
!     TYPE DECLARATION FOR /SCREEN/
!
      LOGICAL (kind=4) ::  ESCAPE
      LOGICAL (kind=1) ::  LM_FIN_FILE_EXISTS
!     LOGICAL*4 REPORT_FILE_EXISTS
      LOGICAL (kind=1) ::  RUN_LOAD,RUN_PROCOST,
     +          RUN_FINANCE,
     +          RUN_FINASSET
! 3/1/93. GAT. HOURLY REFERENCE LOAD SHAPES
! 5/5/93. GAT. SCREENING INFORMATION.
      INTEGER (kind=4) ::  END_POINT_STARTING_RECORD,
     +          SAVE_END_POINT_STARTING_RECORD,
     +          SET_OUT_REC_TO_ENDPNT_START_REC
!     LOGICAL*1 CONVERT_ALL
      LOGICAL (kind=1) ::  FORECAST_REPORT
! FILE NAMES
      CHARACTER (LEN=256) :: FILE_NAME,STATUS_FILE_NAME,
     +                       CURRENT_DIRECTORY,BIP_FILE_NAME
! DECLARATION SECTION FOR LAM PROGRAMS
!
! COMMON BLOCKS
!

! MONTHLY FUEL PRICE ITEMS ADDED 9/22/92
!
      LOGICAL (kind=1) ::  FUEL_PRICE_DATA_AVAILABLE
      INCLUDE 'DSMFRCOM.MON'
!
      INTEGER (kind=4) ::  ERROR,DEALLOCATE_FUEL_INVENTORY_ID
!
! PARAMETER SECTION
!  COMMON BLOCK FOR ERROR MESSAGES
      LOGICAL (kind=1) ::  MESSAGE,DEBUG_ON
      INTEGER (kind=2) ::  INVERSE_VIDEO_COM
      LOGICAL (kind=1) ::  USER_TERMINATED
      COMMON /ERRORS/ INVERSE_VIDEO_COM,MESSAGE,DEBUG_ON,USER_TERMINATED
!  BEGINNING OF THE COMMON BLOCK SECTION FOR THE PRODUCTION MODULE
!
! VARIABLES FOR THE DETAILED REPORTS
!
!

!
!  END COMMON BLOCK SECTION FOR THE PRODUCTION MODULES
!
! NEW VARIABLES ADDED BY M.S.G. JULY 29, 1993 -
!
      LOGICAL (kind=1) ::  RUN_SPECS_OBJECT,
     +          DETAILED_REPORTS_OBJECT,
     +          CLOSE_RUN_SPECS_FILE,
     +          CLOSE_DETAILED_REPORTS_FILE,
     +          RETURN_RUN_SPEC_SWITCHES,
     +          RETURN_DETAILED_REPORTS,
     +          SET_END_OF_STUDY_FLAG_FALSE,
     +          WVPA_INIT_TRACKER_DATABASE,
     +          READ_ESCALATION_FILE
      LOGICAL (kind=1) ::  SYSTEM_BASED_FORECAST,SYSTEM_FORECAST,
     +          VOID_LOG1,READ_DAY_TYPE_DATA,
     +          READ_USER_DAY_DATA,NERC_REGION_BASED_FORECAST
      LOGICAL (kind=1) ::  END_OF_STUDY_FLAG,
     +          END_OF_PLAN_FLAG,
     +          RATIOS_FOUND,READ_ICAP_FILE
      INTEGER (kind=2) ::  RUN_YEARS
      INTEGER (kind=2) ::  LAST_REFERENCE_LOADS/9999/,HOURLY_LOAD_IN
      CHARACTER (len=5) :: LD_FIN_FIL
      CHARACTER (len=2) ::  CAPACITY_PLANNING_METHOD,GREEN_MRX_METHOD,
     +            ACTIVE_CAPACITY_PLANNING_METHOD
      LOGICAL (kind=1) ::  DEALLOCATE_TRANS_OUTAGE_DATA,
     +          DEALLOCATE_ANNUAL_TRANS_LOADS,
     +          DEALLOCATE_ANNUAL_GAS_LOADS
      LOGICAL (kind=1) ::  LAHEY_LF95
      CHARACTER (LEN=1) :: UTILITY_TYPE_CODE,UTILITY_TYPE
! COAL MODEL
      LOGICAL (KIND=1) :: CoalLPActive,RUN_COAL_MODEL,COAL_MODEL_ONLY,
     +                    RUN_COAL_MODEL_ONLY
!      LOGICAL (KIND=4) :: READ_BASIN_SUPPLY_SO2_DATA,
!     +                    READ_PLANT_CONT_DEMAND_LINK_DTA
      INTEGER (KIND=2) :: CURRENT_YEAR
      LOGICAL (KIND=1) :: TRUE=.TRUE.,FALSE=.FALSE.
!
!********** END OF THE DATA DECLARATIONS *********
!
!
      FORECAST_CUSTOMERS = 0.
      HISTORICAL_LOADS = 0.
      COINCIDENT_HISTORICAL_PEAK = 0.
      FORECAST_ENERGY = 0.
      FORECAST_COINCIDENT_PEAK = 0.
      CLASS_LOSSES = 0.
      CLASS_COIN_FACTOR = 0.
      CLASS_RESERVE_MARGIN = 0.
      CLASS_PEAK_LOSSES = 0.
      CALL DISPLAY_FORECAST_TYPE
      RUN_LOAD = .TRUE.
      UTILITY_TYPE_CODE = UTILITY_TYPE()
      RUN_PROCOST = .TRUE.
      RUN_FINANCE = .TRUE.
      RUN_FINASSET = .TRUE.
      ENDPOINTS_RUN = 0
      NUNITS = 0
      HYDRO_UNITS = 0
      NUMBER_OF_CONTRACTS = 0
      SYSCAP = 0
      CAPACITY_PLANNING_IS_ACTIVE = .FALSE.
      START_YEAR = 1
      STARTING_END_POINT = 1
!     YEARS_TO_RUN = RUN_YEARS()
      LAST_LOOP_YEAR = STUDY_PERIOD + EXTENSION_PERIOD
      YEARS_TO_RUN = LAST_LOOP_YEAR
!
      TESTING_PLAN = .FALSE.
! 091416.
      UZVars = 0.0
      RXVars = 0.0
      TBVARS = 0.0
      MXVARS = 0.0
      NEWVars = 0.0
      CAPACITY_AREA_NAME = " "
!
! IF CAPACITY PLANNING IS ACTIVE READ THE OBJECTIVE FUNCTION
!
      FIRST_END_POINT = END_POINT == STARTING_END_POINT
      SYSTEM_FORECAST = SYSTEM_BASED_FORECAST()
      INIT_FILE_FOUND = INIT_FILE_IS_ACTIVE()  .AND.
     +                                           .NOT. SP_CAPEX_ACTIVE()
      IF(CAPACITY_PLANNING_ACTIVE())
     +      CALL POSTFIX_NOTATION_OBJECT(OBJECTIVE_FUNCTION_ACTIVE)
      ACTIVE_CAPACITY_PLANNING_METHOD = CAPACITY_PLANNING_METHOD()
      IF(ACTIVE_CAPACITY_PLANNING_METHOD == 'NE' .AND.
     +                                  CAPACITY_PLANNING_ACTIVE()) THEN
!        TEMP_CHR = PLANNING_DECISIONS()
      ENDIF
!
      SIMSTATUS_REPORT = SIMSTATUS_ACTIVE()
!
      IF(SIMSTATUS_REPORT) THEN
         INQUIRE(UNIT=7101,OPENED=ERROR_FILE_OPEN)
         IF(ERROR_FILE_OPEN) CLOSE(7101)
         CALL CURDIR(" ",CURRENT_DIRECTORY)
         STATUS_FILE_NAME = TRIM(CURRENT_DIRECTORY)//"\SIMSTATUS.LOG"
         INQUIRE(FILE=STATUS_FILE_NAME,EXIST=ERROR_FILE_EXISTS)
         IF(ERROR_FILE_EXISTS) CALL ERASE(STATUS_FILE_NAME)
         OPEN(7101,FILE=STATUS_FILE_NAME,
     +                  CARRIAGE CONTROL="FORTRAN",
     +                  ACCESS= 'DIRECT',RECL=128)
         CALL DATE_AND_TIME(START_SIMDATE,
     +                      START_SIMTIME,
     +                      START_SIMZONE,
     +                      START_SIMDT)
         SIM_REC = 1
      ENDIF
!
      DO ! START OF STUDY LOOP
         CALL SET_NEW_ASSETS_INACTIVE
!
! 090106. TEMP TEST FOR LONG RUNS
! 090606. SEEMS TO CAUSE MORE PROBLEMS. ERASE IF EXISTS AS A TEST.
!
      GRX_ITERATIONS = 0

      END_POINT_STARTING_RECORD = SAVE_END_POINT_STARTING_RECORD()
      RUN_FUTURE_ASSETS = .FALSE.
      CALL RESET_PROJECTS_IN_PROCESS_RECS
! 042811. MOVED FOR TVA STOCHASTICS.
      VOID_LOGICAL = READ_LH_GLOBAL_FILE()
      BIP_FILE_NAME = TRIM(OUTPUT_DIRECTORY())//"BIP"//
     +      TRIM(cldata%SCENAME)//".BIN"
      IF(FIRST_END_POINT) THEN
         FILE_NAME = trim(OUTPUT_DIRECTORY())//"OL*.BIN"
         CALL ERASEWC(FILE_NAME)
         CALL BC_RESET
         CALL STARTOVL(YEARS_TO_RUN,START_YEAR,END_POINT,
     +                 ENDPOINTS_RUN,*30,
     +                 RUN_FUTURE_ASSETS)
!
! FOR A ONE BRANCH TREE NEED TO UNSET THE END_OF_STUDY_FLAG
!  WHEN AUTO_OPTIM IS BEING RUN
!
         IF(CAPACITY_PLANNING_ACTIVE() .AND.
     +           ACTIVE_CAPACITY_PLANNING_METHOD == 'OP')
     +                      VOID_LOGICAL = SET_END_OF_STUDY_FLAG_FALSE()
      ELSEIF(.NOT. END_OF_STUDY_FLAG()) THEN
         IF(CAPACITY_PLANNING_ACTIVE() .AND.
     +                     ACTIVE_CAPACITY_PLANNING_METHOD == 'OP') THEN
            END_POINT = END_POINT + 1
            CALL SET_FIRST_FINANCIAL_ADD_TRUE
            IF(LAHEY_LF95()) THEN
               CALL RW_SET_ENDPOINTS_ON_STATUS(END_POINT)
            ELSE

               WRITE(SCREEN_MESSAGES,"(I4)") END_POINT
               CALL MG_CLEAR_LINE_WRITE(8,59,63,trim(SCREEN_MESSAGES),
     +                                                   ALL_VERSIONS,0)
            ENDIF
         ELSE
            CALL CLOSE_FORECAST_DATA_FILES
            FILE_NAME = trim(OUTPUT_DIRECTORY())//"OL*.BIN"
            CALL ERASEWC(FILE_NAME)
            CALL BC_RESET
            CALL GETOVLS(YEARS_TO_RUN,START_YEAR,END_POINT,
     +                   ENDPOINTS_RUN,*30,RUN_FUTURE_ASSETS)
         ENDIF
      ENDIF
      GRX_SAVED_ENDPOINT = END_POINT
      IF(END_POINT == 3) THEN
         END_POINT = END_POINT
      ENDIF
!
! code to run coal model as a stand alone
!
      TestFactor = .5
      PRODP_FILE_EXISTS = PRODUCTION_PARAMETERS_OBJECT(INT2(472))
      VOID_LOGICAL = UPDATE_PRODUCTION_PARAMETERS(START_YEAR)
      IF(RUN_COAL_MODEL_ONLY() .OR. COAL_MODEL_ONLY()) THEN
         CALL MG_LOCATE_WRITE(20,70,"Stand Alone Coal Model Running",
     +                                                   ALL_VERSIONS,0)
         CoalLPActive = READ_COAL_ESCALATION_FILE(BASE_YEAR)
         CoalLPActive = READ_BASIN_SUPPLY_SO2_DATA()
         CoalLPActive = READ_PLANT_CONT_DEMAND_LINK_DTA(BASE_YEAR,
     +                                           BASE_YEAR+STUDY_PERIOD)

         CALL InitCNWStructure(cldata%SCENAME,TRUE,TestFactor)
         CALL DISPLAY_TIME
         do YEAR = 1, STUDY_PERIOD
            CURRENT_YEAR = BASE_YEAR + YEAR
            WRITE(SCREEN_MESSAGES,"(I4,A,I4)") CURRENT_YEAR,
     +                                       ' End Point: ',END_POINT
            CALL MG_LOCATE_WRITE(6,26,TRIM(SCREEN_MESSAGES),
     +                                                   ALL_VERSIONS,1)
            CALL GregsCnwRoutine(BASE_YEAR,YEAR,.true.) ! owner of interface arrays
            CALL COAL_MODEL_REPORTS(CURRENT_YEAR,YEAR,END_POINT,.true.)
            CALL DISPLAY_TIME
         enddo
         FIRST_END_POINT = .FALSE.
         ENDPOINTS_RUN = ENDPOINTS_RUN + 1
         IF(END_OF_STUDY_FLAG()) THEN
            CLOSE(38,IOSTAT=IOS)
            OPEN(38,FILE=BIP_FILE_NAME,
     +                            ACCESS="TRANSPARENT",STATUS="UNKNOWN")
            WRITE(38,REC=4)END_POINT,BASE_YEAR,CURRENT_YEAR
            CLOSE(38)
            EXIT
         ENDIF
         CYCLE ! TO END OF ENDPOINT LOOP
      ENDIF

      IF(WVPA()) VOID_LOGICAL = WVPA_INIT_TRACKER_DATABASE()
      CALL MANAGE_ASSET_ALLOCATIONS(.TRUE.)
      CALL STORE_END_POINT(END_POINT)
      TEMP_L = RESET_INTO_EXTENSION_PERIOD() ! 050709.
      IF(INIT_FILE_FOUND) THEN
         CALL OPEN_ASSET_VECTOR_FILE(82) ! 3/28/95 MAKING ONE OPEN
         CALL INIT_ASSET_CLASS_INFO  ! NEED ELIMINATION CLASS B4 ASSET ANALYSIS
         CALL ZERO_ASSET_CLASS_ARRAYS
         CALL READ_CLASS_INITIALIZATION_FILE
      ENDIF

!
! THIS IS THE RECYCLE POINT FOR AUTO_OPTIM OPERATION
!
!
! SET PROPER ESCALATION VALUES
!
! CALL TO ESCALATION OBJECT ADDED 5/20/93 WILL REMOVE THE OTHER READS
! LATER
!
! 0803006 MOVED FOR BURESH.
!
      VOID_LOGICAL = READ_LH_GLOBAL_FILE()
!
      VOID_LOGICAL = READ_SCENARIO_MAKER_DATA()
      CALL PROCESS_CAPITAL_RATES_FILE
      VOID_LOGICAL = READ_ESCALATION_FILE()
!
! SET-UP MONTHLY FUEL PRICE FILE FOR THIS END POINT
!
      CALL BUILD_FUEL_PRICE_INFO(FUEL_PRICE_DATA_AVAILABLE)
!
      CALL OPEN_VAR_UNITS_FILE
!
      CALL INIT_ENDPT_SWITCHES
! 03/16/05. IN FOR BURESH.
!      RATIOS_FOUND = READ_ICAP_FILE()
!
      CALL CLS(9,10,12)
      SIMULATION_YEARS = RUN_YEARS()
      WRITE(RUN_END_POINT,100) 'End point: ',END_POINT
      CALL CLS(16,9,36)
      CALL DISPLAY_TIME
      VOID_LOGICAL = SET_ASSET_ANALYST_ONLY_SWITCH()
!
!     VOID_LOGICAL = RUN_SPECS_OBJECT()
      VOID_LOGICAL = DETAILED_REPORTS_OBJECT()
      CALL INIT_USER_MARKET_PRICES()
      YEAR = START_YEAR
      IF(.NOT. ASSET_ANALYST_ONLY()) THEN
         VOID_LOGICAL = READ_PRICE_FEEDBACK_DATA()
         CALL OPEN_ASSET_VECTOR_FILE(82)
         LM_FIN_FILE_EXISTS = INDEX(LD_FIN_FIL(),'NONE') == 0

!
         VOID_LOGICAL = READ_TRANS_GROUPS_DATA()
! 070409.
         VOID_LOGICAL = MANAGE_TRAN_EXP_FORECASTS()
!
         VOID_LOGICAL = MANAGE_GAS_NODE_FORECASTS()
         VOID_LOGICAL = MANAGE_GAS_SUPPLY_FORECASTS()
         VOID_LOGICAL = MANAGE_GAS_DEMAND_FORECASTS()
         VOID_LOGICAL = MANAGE_GAS_LINK_FORECASTS()
         VOID_LOGICAL = MANAGE_GAS_STORAGE_FORECASTS()
!
! 042209.
!
         VOID_LOGICAL = MANAGE_RPS_REQUIREMENTS()
         VOID_LOGICAL = MANAGE_RPS_PROGRAM_FORECASTS()
         CALL MANAGE_RPS_STATE_DEMAND()
         CALL MANAGE_THERMAL_RETROFIT()
         VOID_LOGICAL = MANAGE_CO2_PARAM_FORECASTS()
!
         gas_lp_active = LP_GAS_MODEL()
!
         if(gas_lp_active) then
            TEMP_R = UPDATE_HH_PRICE(0_2,0_2)
!            call GasFileNames()
            call FirstGNWCallGreg
            TEMP_L = GAS_PRICING_ROUTINE()
         endif
!
!         VOID_LOGICAL = READ_REGIONAL_OUTAGES_DATA()
         VOID_LOGICAL = READ_REGIONAL_PARAMS_DATA()
!
         VOID_LOGICAL = READ_ENERGY_PRODUCTS_DATA()
! 010618. MOVED FROM PROCOST.
         TEMP_L = READ_TRANS_CONSTRAINT_DATA()
         TEMP_L = READ_TRANS_PATH_DATA()
! 090106.
         CALL RESET_FISCAL_MONTHLY_GROUP()
!
!
! 8/9/94. GAT. MOVED FROM INSIDE THE YEAR=1 LOOP
!
         CALL INIT_LAST_REFERENCE_LOADS
         IF(.NOT. NERC_REGION_BASED_FORECAST())
     +                                       CALL UPDATE_REFERENCE_LOADS
         CAPACITY_PLANNING_IS_ACTIVE = CAPACITY_PLANNING_ACTIVE()
      ELSE
         VOID_LOGICAL = READ_TRANS_GROUPS_DATA()
         CAPACITY_PLANNING_IS_ACTIVE = .FALSE.
         PRODP_FILE_EXISTS = .FALSE.
      ENDIF
!
!
      FIRST_PASS = .TRUE.
!
      CoalLPActive = .FALSE.
      DO          ! if pr planning loop
         TESTING_PLAN = .FALSE.
         IF(.NOT. ASSET_ANALYST_ONLY()) THEN
            IF(ACTIVE_CAPACITY_PLANNING_METHOD == 'PR' .AND.
     +                      CAPACITY_PLANNING_IS_ACTIVE .AND.
     +                                                  FIRST_PASS) THEN
               TESTING_PLAN = .TRUE.
            ENDIF
!        CALL RINIT ! 8/12/94. GAT. MOVED FROM INSIDE FINANBSE. removed msg 3/2/98
            IF(.NOT. SP_CAPEX_ACTIVE()) THEN
               IF(SYSTEM_FORECAST) THEN
                  CALL SF_GET_PEAKS
               ELSEIF(.NOT. NERC_REGION_BASED_FORECAST()) THEN
                  CALL CF_GET_PEAKS
               ENDIF
               CALL MDSLDCAP
            ENDIF
!
!
            IF(FIRST_PASS .OR. (CAPACITY_PLANNING_METHOD() == 'MX'
     +                           .AND. GREEN_MRX_METHOD() == 'GX')) THEN
               TEMP_L = MULTI_YEAR_ENERGY_PRODUCTS()
               MARKET_GROUPS = READ_MARKET_GROUPS_DATA()
               HYDRO_UNITS = EL_UNITS_READ()
               VOID_LOG1 = READ_DAY_TYPE_DATA()
               VOID_LOG1 = READ_USER_DAY_DATA()
               NUNITS = CL_UNITS_READ()
               NUMBER_OF_CONTRACTS = CONTRACTS_READ()
               CAPACITY_PLANNING_IS_ACTIVE = CAPACITY_PLANNING_ACTIVE()
            ENDIF
         ENDIF
         IF(INIT_FILE_FOUND) THEN
            IF(FIRST_PASS) THEN
               CALL FINASSET(.FALSE.)
            ELSE
               CALL ZERO_ASSET_CLASS_ARRAYS
            ENDIF
            CALL READ_CLASS_INITIALIZATION_FILE
         ENDIF
!
         CUM_ANN_EMIS = 0.
         YEAR_LOOP = START_YEAR
         IF(.NOT. INIT_FILE_FOUND) CALL WRITE_BASE_YR_RESULTS() ! WRITE THE BASE YEAR TO THE BIP FILE
!
         TEMP_I2 = ENDPOINTS_RUN + 1 ! 081416.
         CALL RW_SET_ENDPOINTS_ON_STATUS(TEMP_I2) ! END_POINT)
         GRX_SAVED_ENDPOINT = END_POINT
         GRX_PRT_ENDPOINT = 100 * END_POINT
         GRX_UNIT_COUNT = 0
         DOWHILE (YEAR_LOOP <= LAST_LOOP_YEAR)
            YEAR = YEAR_LOOP
            GRX_UNIT_COUNT(YEAR_LOOP,-1) = NUNITS
            MAX_GRX_ITERs = MAX_GRX_ITERATIONS(BASE_YEAR + YEAR)
            GRX_ITERATIONS = 0
            VOID_LOGICAL = UPDATE_PRODUCTION_PARAMETERS(YEAR)
            IF(.NOT. CoalLPActive) THEN !  .AND. RUN_COAL_MODEL()) THEN
!
! the following call order must be kept.
!
               CoalLPActive = READ_COAL_ESCALATION_FILE(BASE_YEAR)
               CoalLPActive = READ_BASIN_SUPPLY_SO2_DATA()
               CoalGenericPlant =
     +                    READ_GEN_PLANT_DEMAND_LINK_DATA(BASE_YEAR)
               CoalExistingPlant =
     +                   READ_PLANT_CONT_DEMAND_LINK_DTA(BASE_YEAR,
     +                                           BASE_YEAR+STUDY_PERIOD)
               IF((CoalExistingPlant .OR. CoalGenericPlant) .AND.
     +                                                CoalLPActive) THEN
                   CALL COAL_DEMAND_LINK()
                   CALL InitCNWStructure(cldata%SCENAME,TRUE,TestFactor)
                   CoalLPActive = .TRUE.
               ENDIF
             ENDIF

               CALL RW_SET_YEAR_ON_STATUS(BASE_YEAR + YEAR)
               IF(SP_CAPEX_ACTIVE()) THEN
                  WRITE(SCREEN_MESSAGES,"(I4,A,I4,A)") BASE_YEAR + YEAR,
     +                ' End Point: ',INT(PRT_ENDPOINT()),' for SP CapEx'
               ELSE
                  WRITE(SCREEN_MESSAGES,"(I4,A,I4)") BASE_YEAR + YEAR,
     +                                ' End Point: ',INT(PRT_ENDPOINT())
               ENDIF
               CALL MG_LOCATE_WRITE(6,26,trim(SCREEN_MESSAGES),
     +                                                   ALL_VERSIONS,1)
               IF(QUICKSILVER()) THEN
                  OPEN(10,FILE='IamStillRunning') ! ,STATUS='REPLACE')
                  WRITE(10,*) trim(SCREEN_MESSAGES)
                  call flush(10)
                  CLOSE(10)
               ENDIF
!
            IF(WVPA()) THEN
               CALL WVPA_STORE_CURRENT_DATE(YEAR,BASE_YEAR)
            ENDIF
!            WRITE(FORECAST_YEAR,100)  'Forecast Year: ',BASE_YEAR+YEAR
!
            VOID_LOGICAL = RETURN_DETAILED_REPORTS(YEAR)
!
            VOID_INT2 = SET_ESCALATION_VECTOR_STRUCTURE(YEAR)
!
            VOID_INT2 = SETUP_DAY_OF_WEEK_4(BASE_YEAR + YEAR)
            YES_GAS_MODEL_ONLY = GAS_MODEL_ONLY()
            GRX_NUNITS = NUNITS
            write(9,*) "Before GRX Loop",NUNITS
            if(base_year+year == 2018) then
               GRX_NUNITS = NUNITS
            endif
            IF(.NOT. ASSET_ANALYST_ONLY() .AND.
     +            CAPACITY_PLANNING_METHOD() == 'MX'  .AND.
     +                                  GREEN_MRX_METHOD() == 'GX') THEN
               TEMP_L = ADJUST_CO2_RETRO_PLAN_CAP(YEAR)
               CALL GRX_SAVE_PLANNIG_VALUES(STORE)
               CALL GRX_SAVE_CAPACITY_OPTIONS(STORE)
               CALL GRX_RETROFIT_OPTONS(STORE)
               VOID_LOGICAL = GRX_SAVE_DERIV_PLAN_VALUES(STORE,YEAR)
               VOID_LOGICAL = GRX_SAVE_RPS_PROGRAM_VALUES(STORE)
            ENDIF
!
! TESTING THE GRX PLANNING LOOP FOR HOLDING THE CURRENT VALUE OF VARIABLES
!
          GRX_CONVERGED =.FALSE.
          OneMoreIter = .false.
          DO    ! GRX PLANNING LOOP
            INSTALLED_POINTER = 0
            IF(.NOT. ASSET_ANALYST_ONLY()) THEN
               IF(CAPACITY_PLANNING_METHOD() == 'MX'  .AND.
     +                                  GREEN_MRX_METHOD() == 'GX') THEN
                  WRITE(SCREEN_MESSAGES,"(A,I2)")
     +                             ' GRX Iteration: ',GRX_ITERATIONS
                  CALL MG_LOCATE_WRITE(6,26,trim(SCREEN_MESSAGES),
     +                                                   ALL_VERSIONS,1)
               ENDIF
!               VOID_LOGICAL = UPDATE_PRODUCTION_PARAMETERS(YEAR)
               IF((RUN_LOAD .OR. FORECAST_REPORT()) .AND.
     +                                 (YEAR_LOOP <= STUDY_PERIOD)) THEN
                  IF(YEAR_LOOP == 1 .AND. GRX_ITERATIONS == 0) THEN
!
                     CALL DISPLAY_TIME
                     IF(.NOT. SP_CAPEX_ACTIVE())
     +                  CALL MG_LOCATE_WRITE(16,9,
     +                         'Analyzing forecast data',ALL_VERSIONS,1)
!
! NEED PEAKS FOR CAPACITY PLANNING
!
                        CALL DISPLAY_TIME
                        CALL CLS(16,9,32)
!
                        WEEKLY_HYDRO_ACTIVE =
     +                                   MANAGE_WEEKLY_HYDRO_FORECASTS()
!
                     ENDIF
                     IF(.NOT. SP_CAPEX_ACTIVE())
     +                          CALL MG_LOCATE_WRITE(9,15,"Markets",3,4)
                     IF(.NOT. TRANSACT_ANALYST_ONLY()) THEN
                        CALL LOAD_ANALYSIS_OBJECT()
                     ENDIF
!
!     USED TO ACCUMULATE LOADS FOR USE IN TRANSACT
!     MOVED 7/17/98. GAT. FOR PRE-SCHEDULED MAINTENANCE IN TRANSACT.
!     MOVED TO MSGNORM/MSGANDEC. 10/20/98. GAT. FOR CAPACITY PLANNING.
!
                     VOID_LOGICAL =  ANNUAL_TRANSACTION_LOADS(YEAR)
                     VOID_LOGICAL =  ANNUAL_GAS_DEMAND(YEAR)
                     VOID_LOGICAL =
     +                            CALC_ANNUAL_SUPPLY_CURVE_PRICE(YEAR,
     +                                                 GRX_ITERATIONS)
!
! 061307. DISABLE COMBINATORIAL GAS MODEL.
!                  if(.not. gas_lp_active) then
!                     VOID_LOGICAL =  ANNUAL_GAS_LINK_FORECAST(YEAR)
!                  endif
                     CALL DISPLAY_TIME
                     CLOSE(29)
                  ELSE
                     IF(GRX_ITERATIONS == 0)
     +                    VOID_LOGICAL =  ANNUAL_TRANSACTION_LOADS(YEAR) ! added 11/03/06 to reset values for extension period.
                  ENDIF
                  CALL ANNUAL_RPS_STATE_DEMAND(YEAR)
!
! READ THE ENVIRONMENTAL FILE
!
                  VOID_LOGICAL = ENVIR_DATA_READ()
                  VOID_LOGICAL = ANNUAL_ENERGY_PRODUCTS(YEAR)
                  IF(((CAPACITY_PLANNING_METHOD() == 'MX'
     +                      .AND.      GREEN_MRX_METHOD() == 'GX') .OR.
     +                CAPACITY_PLANNING_IS_ACTIVE) .AND.
     +                                    .NOT. YES_GAS_MODEL_ONLY) THEN
                     IF(ACTIVE_CAPACITY_PLANNING_METHOD == 'NE' .OR.
     +                  ACTIVE_CAPACITY_PLANNING_METHOD == 'SC' .OR.
     +                  ACTIVE_CAPACITY_PLANNING_METHOD == 'PR' .OR.
     +                  ACTIVE_CAPACITY_PLANNING_METHOD == 'TR' .OR.
     +                  ACTIVE_CAPACITY_PLANNING_METHOD == 'MX' .OR.
     +                                          YEAR == START_YEAR) THEN
                        IF(.NOT. SP_CAPEX_ACTIVE())
     +                       CALL MG_LOCATE_WRITE(10,15,"Resources",3,4)
                        CALL CAPACITY_PLANNING_OBJECT(NUNITS)
                        IF(YEAR ==1 .AND. GRX_ITERATIONS == 0 .AND.
     +                                                     CoalLPActive)
     +                           CALL COAL_HARDWRIED_DEMAND_LINK(NUNITS)
                        CAPACITY_WAS_ADDED = .TRUE.
                        CALL DISPLAY_TIME
                     ENDIF
                  ENDIF
                  IF(RUN_PROCOST) THEN
                     IF(.NOT. SP_CAPEX_ACTIVE())
     +                      CALL MG_LOCATE_WRITE(11,15,"Operations",3,4)
                     PRICE_PLANNING_FIRST_PASS = FIRST_PASS .AND.
     +                     ACTIVE_CAPACITY_PLANNING_METHOD == 'PR' .AND.
     +                      CAPACITY_PLANNING_IS_ACTIVE
                     write(9,*) "Before PROCOST",year,ofline(35)
                     SYSTEM_CAPACITY=PROCOST(PRICE_PLANNING_FIRST_PASS,
     +                                       CoalLPActive)
                     write(9,*) "After PROCOST",year,ofline(35)
                     CALL CLS(11,27,32)
                     CALL DISPLAY_TIME
                  ENDIF
                  VOID_LOGICAL = EMISSIONS_CREDITS()
               ELSE ! TESTING !
                  VOID_LOGICAL =  ANNUAL_GAS_DEMAND(YEAR)
                  VOID_LOGICAL =
     +                            CALC_ANNUAL_SUPPLY_CURVE_PRICE(YEAR,
     +                                                 GRX_ITERATIONS)
               ENDIF ! END ASSETS ONLY
!
! 09/27/03. MOVED FROM BELOW TO ACCOMODATE LEVELIZED PLANNING
!
               IF(.NOT. (ACTIVE_CAPACITY_PLANNING_METHOD == 'PR' .AND.
     +                         CAPACITY_PLANNING_IS_ACTIVE .AND.
     +                                                 FIRST_PASS)) THEN
                  CALL TRANSACT_GROUP_RESERVE(
     +                                 YEAR,.TRUE.,ASSET_ANALYST_ONLY())
               ENDIF
!
               VOID_LOGICAL = ZERO_TOTAL_EMIS_VARS()
            CALL DISPLAY_TIME
!
! GRX PLANNING LOOP
!
            IF(.NOT. ASSET_ANALYST_ONLY() .AND.
     +              CAPACITY_PLANNING_METHOD() == 'MX'  .AND.
     +                                  GREEN_MRX_METHOD() == 'GX') THEN
               write(9,*) "End GRX Loop Before Reset",NUNITS
               CALL FLUSH(9)
               CALL FLUSH(4)
               write(9,*) "670 converged", GRX_CONVERGED
               GRX_UNIT_COUNT(YEAR_LOOP,GRX_ITERATIONS) = NUNITS
               OneMoreIter = .true.

              IF(GRX_CONVERGED .AND. .NOT. OneMoreIter
     +                     .AND. YEAR > 1 .AND. GRX_ITERATIONS > 0) THEN
                  OneMoreIter = .true.
                  GRX_CONVERGED = .FALSE.
              ELSEIF((GRX_CONVERGED .AND. GRX_ITERATIONS >=
     +                   CO2_Min_GRX_Iterations(BASE_YEAR+YEAR)-1) .OR.
     +             GRX_ITERATIONS >= MAX_GRX_ITERs .OR. YEAR == 1) THEN
                  IREC_SAVED = RPTREC(REC_OPT='U')   ! UPDATE ALL START REC POSITIONS
                  IF(YEAR > 1)
     +                  CALL GRX_RETIREMENT_RETROFIT_RPT(BASE_YEAR,YEAR,
     +                                   INT2(BASE_YEAR+LAST_LOOP_YEAR))
                  END_POINT = GRX_SAVED_ENDPOINT
                  NEXT_CURRENT_CO2_DISPATCH_COST = 0.
                  CURRENT_CO2_DISPATCH_COST = 0.
                  EXIT
               ENDIF
               IREC_SAVED = RPTREC(REC_OPT='A')   ! RESET ALL START REC POSITIONS
               HYDRO_UNITS = RESET_EL_UNITS()
!            VOID_LOGICAL = EL_RESET_PRICES()

               RETIRED_UNITS = RESET_ANNUAL_UNITS_LEFT() !RESETS THE AVAILABLE ANNUAL UNITS
               NUNITS = GRX_NUNITS
               VOID_LOGICAL = CONTRACT_RESET_OPTIONS()
               VOID_LOGICAL = DSM_RESET_OPTIONS()

               GRX_ITERATIONS = GRX_ITERATIONS + 1
               GRX_PRT_ENDPOINT = GRX_PRT_ENDPOINT + 1

               CALL GRX_SAVE_PLANNIG_VALUES(RESTORE)
               CALL GRX_SAVE_CAPACITY_OPTIONS(RESTORE)
               CALL GRX_RETROFIT_OPTONS(RESTORE)
               VOID_LOGICAL = GRX_SAVE_DERIV_PLAN_VALUES(RESTORE,YEAR)
               VOID_LOGICAL = GRX_SAVE_RPS_PROGRAM_VALUES(RESTORE)
               write(9,*) "End GRX Loop After Reset",NUNITS
             ELSE
               EXIT
             ENDIF
          ENDDO ! END GRX PLANNING LOOP
!             IF(ASSET_ANALYST_ONLY() .OR.
!     +             GRX_CONVERGED .OR. GRX_ITERATIONS>=MAX_GRX_ITERs .OR.
!     +                 .NOT. (GET_CO2_RETIREMENTS_LOGIC() .OR.
!     +                             CO2_RETROFIT_LOGIC_ACTIVE()) .OR.
!     +                                                   YEAR == 1) THEN
               IF(INIT_FILE_FOUND) THEN
                  IF(RUN_FINANCE .OR. RUN_FINASSET .OR.
     +                                          LM_FIN_FILE_EXISTS) THEN
                     IF(.NOT. SP_CAPEX_ACTIVE()) THEN
                        IF(ASSET_ANALYST_ONLY()) THEN
                           CALL MG_LOCATE_WRITE(8,15,"Financial",3,4)
                        ELSE
                           CALL MG_LOCATE_WRITE(12,15,"Financial",3,4)
                        ENDIF
                     ENDIF
                     IF(CAPACITY_PLANNING_IS_ACTIVE .AND.
     +                   CAPACITY_WAS_ADDED .AND.
     +                     (YEAR <= STUDY_PERIOD) .AND.
     +                     (YEAR == START_YEAR .OR.
     +                      ACTIVE_CAPACITY_PLANNING_METHOD == 'NE' .OR.
     +                      ACTIVE_CAPACITY_PLANNING_METHOD == 'TR' .OR.
     +                      ACTIVE_CAPACITY_PLANNING_METHOD == 'MX' .OR.
     +                    (ACTIVE_CAPACITY_PLANNING_METHOD == 'PR' .AND.
     +                                                FIRST_PASS))) THEN
                        IF(ACTIVE_CAPACITY_PLANNING_METHOD == 'NE' .OR.
     +                      ACTIVE_CAPACITY_PLANNING_METHOD == 'TR' .OR.
     +                      ACTIVE_CAPACITY_PLANNING_METHOD == 'SO' .OR.
     +                      ACTIVE_CAPACITY_PLANNING_METHOD == 'MX' .OR.
     +                    (ACTIVE_CAPACITY_PLANNING_METHOD == 'PR' .AND.
     +                                                 FIRST_PASS)) THEN
                           CALL FUASST(.FALSE.,.TRUE.)
                        ELSE
                           CALL FUASST(.TRUE.,.TRUE.)
                        ENDIF
                        CALL write_scroll_line_RW(' ',2)
                        CALL write_scroll_line_RW(' ',0)
                     ENDIF
                     CALL DISPLAY_TIME
                     IF(YEAR == START_YEAR) CALL ZERO_DSM_RESULTS
                     IF(LM_FIN_FILE_EXISTS) CALL DSM_FINANCIAL_COST
                  IF(ACTIVE_CAPACITY_PLANNING_METHOD == 'PR' .AND.
     +                FIRST_PASS .AND. CAPACITY_PLANNING_IS_ACTIVE) THEN
                     CALL FINANCE(YEAR,.FALSE.,LAST_LOOP_YEAR)
                  ELSE
                     CALL FINANCE(YEAR,.TRUE.,LAST_LOOP_YEAR)
                  ENDIF
                  CALL DISPLAY_TIME
               ENDIF
               IF(.NOT. ASSET_ANALYST_ONLY()) CALL COST_OF_SERVICE
             ENDIF
!            ENDIF
!
! OUTPUT REPORTS
!
            CURRENT_YEAR = BASE_YEAR + YEAR
            CALL ABB_CapacityMarketReport(CURRENT_YEAR)
!           IF(.NOT. ASSET_ANALYST_ONLY) THEN
            IF(ACTIVE_CAPACITY_PLANNING_METHOD == 'PR' .AND.
     +                         CAPACITY_PLANNING_IS_ACTIVE .AND.
     +                                                  FIRST_PASS) THEN
               CALL OUTPUT(YEAR,.FALSE.,ASSET_ANALYST_ONLY())
            ELSE
               CALL OUTPUT(YEAR,.TRUE.,ASSET_ANALYST_ONLY())
            ENDIF
            IF(CAPACITY_PLANNING_IS_ACTIVE .AND.
     +                                   OBJECTIVE_FUNCTION_ACTIVE) THEN
               IF(ACTIVE_CAPACITY_PLANNING_METHOD /= 'AN') THEN
                  CALL COMPUTE_OBJECTIVE_FUNCTION
                  IF(END_OF_PLAN_FLAG()) EXIT
               ENDIF
            ENDIF
            CALL DISPLAY_TIME
! 7/1/02.
            RESET_WARNINGS = .FALSE.
!            RESET_WARNINGS = .TRUE. ! 110612 TEST
            CALL FLUSH(4)
            IF(RESET_WARNINGS) THEN
               INQUIRE(UNIT=4,OPENED=FILE_OPENED)
               IF(FILE_OPENED) THEN
                  CLOSE(4)
                  FILE_NAME = TRIM(OUTPUT_DIRECTORY())//"MSG"//
     +               TRIM(cldata%SCENAME)//".ERR"
                  OPEN(4,FILE=FILE_NAME,
     +                              CARRIAGE CONTROL="FORTRAN",RECL=132,
     +                                                POSITION='APPEND')
               ENDIF
            ENDIF
!
          YEAR_LOOP = YEAR_LOOP + 1
         ENDDO  !WHILE YEAR LOOP
!
         IF(WVPA()) THEN
            CALL WVPA_WRITE_PLANT_BAL_RPT(END_POINT,BASE_YEAR)
            CALL WVPA_CONSTRUCTION_SUMMARY_RPT(END_POINT,BASE_YEAR)
         ENDIF
         IF(MONTHLY_MIDAS_ACTIVE) THEN
            CALL BASE_YEAR_ACTUAL_INCOME_DATA()
            CALL MONTHLY_TRAILING_12_MONTHS_REVENUE_REPORT()
         ENDIF
         IF(IPALCO() .AND. MONTHLY_MIDAS_ACTIVE) THEN
            CALL IPALCO_REPORTS()
         ENDIF

!
! RECOVERY MEMORY AT END OF ENDPOINT 7/24/00 MSG
!
         VOID_LOGICAL = DEALLOCATE_TRANS_OUTAGE_DATA()
         VOID_LOGICAL = DEALLOCATE_ANNUAL_TRANS_LOADS()
         VOID_LOGICAL = DEALLOCATE_ANNUAL_GAS_LOADS()
!
         IF(ACTIVE_CAPACITY_PLANNING_METHOD == 'PR' .AND.
     +                     CAPACITY_PLANNING_IS_ACTIVE .AND.
     +                                                  FIRST_PASS) THEN
            HYDRO_UNITS = RESET_EL_UNITS()
            VOID_LOGICAL = EL_RESET_PRICES()
            NUNITS = RESET_CL_UNITS()
            VOID_LOGICAL = CL_RESET_PRICES()
            VOID_LOGICAL = CONTRACT_RESET_OPTIONS()
            VOID_LOGICAL = DSM_RESET_OPTIONS()
            FIRST_PASS = .FALSE.
            CAPACITY_PLANNING_IS_ACTIVE = .FALSE.
!
! temp code to look at run differences 2/19/95
!
!            ENDPOINTS_RUN = ENDPOINTS_RUN + 1
!            END_POINT = END_POINT + 1
!            CALL STORE_END_POINT(END_POINT)
         ELSE
            EXIT
         ENDIF
      ENDDO  ! END PR PLANNING LOOP
      FIRST_END_POINT = .FALSE.


      IF(YEAR_LOOP > LAST_LOOP_YEAR) THEN
         ENDPOINTS_RUN = ENDPOINTS_RUN + 1

      ELSE
!
! NEED TO RESET THE OUTPUT POINTER BECAUSE THE PLAN DIDN'T
!
         END_POINT_STARTING_RECORD = SET_OUT_REC_TO_ENDPNT_START_REC()
!
      ENDIF
!     IF(ACTIVE_CAPACITY_PLANNING_METHOD=='SC')
!    +                                  CALL CLOSE_SCREENING_INFORMATION
!
! CLEAN UP NEEDED ? IF AUTO_OPTIM NOT ACTIVE (NORMAL CLEAN UP)  THIS
!  NEEDS TO REVIEW BECAUSE IT IS 640k AND FILES = DEPENDENT
!
      CALL CLOSE_FINANCIAL_PARAMETER_FILE
      CALL CLOSE_CAPITAL_RATES_VECTOR_FILE
!
! CLOSE THE MULTI-YEAR FINANCIAL RUN SPECIFICATIONS FILE
!
      CLOSE(472)    !PRODUCTION PARAMETERS FILE
      IF(FUEL_PRICE_DATA_AVAILABLE) CLOSE(666) !FUEL PRICE FILE
      CALL CLOSE_CLASS_RUN_SWITCH_FILE
      VOID_LOGICAL = CLOSE_DETAILED_REPORTS_FILE()
      CLOSE(50)
      CALL CLOSE_ASSET_VECTOR_FILE
      CLOSE(84)
      CLOSE(43,IOSTAT=IOS) ! ENVIRONMENTAL DATA FILE
      CLOSE(111,IOSTAT=IOS) ! FUEL INVENTORY FILE
      CLOSE(112,IOSTAT=IOS) ! FUEL INVENTORY FILE
      CLOSE(3458,IOSTAT=IOS) ! REVENUE FORECAST
      CLOSE(900,IOSTAT=IOS) ! SYSTEM FORECAST
      CALL CLOSE_FINANCIAL_OPTIONS
!
! CLOSE CPO FILE FOR THE END POINT
!
      INQUIRE(UNIT=9979,OPENED=FILE_IS_OPEN)
      IF(FILE_IS_OPEN) THEN
         IREC_SAVED = RPTREC(9979_2,FILE_OPT = 'S')
      ENDIF
!
      if(gas_lp_active) then
         call LastGNWCallGreg
         TEMP_L = WRITE_LNG_TO_DAT()
      endif
! 101116.
      CALL WRITE_GRX_RPS_CAPACITY()
!
!      CALL CLOSE_SERVICE_TRANSACTIONS
!      CALL FREE_INIT_ASSET_CLASS_INFO_ARRAYS()
      IF(END_OF_STUDY_FLAG()) EXIT
      ENDDO ! STUDY LOOP
   30 CONTINUE
      IF(SP_CAPEX_ACTIVE()) THEN
         OPEN(10,FILE="SPCapExDONE")
         write(10,*) "SP has finished"
         CLOSE(10)
      ENDIF
! 11/10/04. FOR XML INTERFACE
      INQUIRE(UNIT=4444,OPENED=FILE_OPENED)
      IF(FILE_OPENED) THEN
         WRITE(4444,4445) '</NewStation>'
         WRITE(4444,4445) '</XMLIntegration>'
         CLOSE(4444,IOSTAT=IOS)
      ENDIF
! SEQUENTIAL FILE WRITTEN IN GRX DOESN'T NEED RE-SIZING
      CLOSE(8337,IOSTAT=IOS) ! EV GRX RETIRE/RETRO CSV REPORT
      CLOSE(8339,IOSTAT=IOS) ! EV GRX RETIRE/RETRO CSV REPORT
!      IF(SIMSTATUS_REPORT) THEN
!         CLOSE(7101)
!         INQUIRE(FILE=STATUS_FILE_NAME,EXIST=ERROR_FILE_EXISTS)
!         IF(ERROR_FILE_EXISTS) CALL ERASE(STATUS_FILE_NAME)
!      ENDIF
!      CLOSE(MON_CL_TRANS_NO,IOSTAT=IOS) ! CL TRANS REPORT
!      CLOSE (7010)
! THE FOLLOWING CLOSES ALL OPEN BINARY FILES.
      IREC_SAVED = RPTREC(FILE_OPT = 'C')
      CALL CLOSE_FILES(ENDPOINTS_RUN,BASE_YEAR+LAST_LOOP_YEAR)
      MESSAGE = .FALSE.
      MIDAS_NORMAL_SIMULATION = ENDPOINTS_RUN
      RETURN
4445  FORMAT(1X,4A)
!
!***********************************************************************
      ENTRY UPDATE_STATUS_REPORT(R_MONTH)
!***********************************************************************
            IF(SIMSTATUS_REPORT) THEN
!               CALL TIMER(SIMTIME)
               CLOSE(7101)
               OPEN(7101,FILE=STATUS_FILE_NAME,
     +                  ACCESS= 'DIRECT',RECL=128)
!     +                              CARRIAGE CONTROL="FORTRAN",RECL=128)
!     +                                       POSITION='APPEND',RECL=128)
               CALL DATE_AND_TIME(
     +                      SIMDATE,
     +                      SIMTIME,
     +                      SIMZONE,
     +                      SIMDT)
               TEMP_R = PRT_ENDPOINT() ! FLOAT(GET_PRB_SEQUENCE_NUMBER() + END_POINT - 1)
               WRITE(SIMLINE,*)  START_SIMDATE,',',
     +                           START_SIMTIME,',',
     +                           SIMDATE,',',
     +                           SIMTIME,',',
     +                        cldata%SCENAME,',',
     +                           RPT_PROJECT_NAME(1:5),',',
     +                           RPT_BASE_FAMILY_NAME(1:5),',',
     +                           TEMP_R,',',
     +                           BASE_YEAR+YEAR,',',
     +                           R_MONTH,',',
     +                           '"Normal Simulation"'
               SIMLINE = trim(SIMLINE)
!               WRITE(7101,'(A)') SIMLINE
               WRITE(7101,REC=1) SIMLINE
!               WRITE(7101) SIMLINE
               SIM_REC = SIM_REC + 1
               UPDATE_STATUS_REPORT = .TRUE.
            ELSE
               UPDATE_STATUS_REPORT = .FALSE.
            ENDIF
      RETURN
!***********************************************************************
      ENTRY FIRST_PASS_IS_ACTIVE
!***********************************************************************
         FIRST_PASS_IS_ACTIVE = FIRST_PASS
      RETURN
  100 FORMAT (A,I4)
      END
!***********************************************************************
      SUBROUTINE INIT_ENDPT_SWITCHES
!***********************************************************************
         CALL INIT_ENDPT_FORE_DAYS_REPORT
      RETURN
      END subroutine INIT_ENDPT_SWITCHES

!
!
