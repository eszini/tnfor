!     ******************************************************************
!     MIDASMOD
!     Copyright(c) M.S. Gerber & Associates 2000
!
!     Created: 12/26/2002 12:14:31 PM
!     Author : MARK S GERBER
!     Last change: msg 11/11/2018 12:28:24 PM
!     ******************************************************************

!**********************************************************************
      MODULE DRILLING_REPT_PARAMETERS
         INTEGER (KIND=2), PARAMETER :: DrillingAccountNameWidth=50
         CHARACTER (LEN=DrillingAccountNameWidth) 
     +                                             DRILLING_ACCOUNT_NAME
      CONTAINS
!***********************************************************************
         FUNCTION WVPA_REPORTING_NAME(ACCOUNT_DESCRIPTION,
     +                                WVPA_ACCTNO,
     +                                WVPA_ACCOUNTING_UNIT,
     +                                WVPA_SUB_ACCOUNT_NUMBER)
!***********************************************************************
!
            CHARACTER (LEN=DrillingAccountNameWidth) 
     +                                        WVPA_REPORTING_NAME,
     +                                        WVPA_OUTPUT_REPORTING_NAME
            INTEGER WVPA_ACCTNO,
     +              WVPA_ACCOUNTING_UNIT,
     +              WVPA_SUB_ACCOUNT_NUMBER
            CHARACTER (LEN=*) :: ACCOUNT_DESCRIPTION
            CHARACTER*12 TEMP_STR
!
            WRITE(TEMP_STR,'(I4)') WVPA_ACCOUNTING_UNIT
            WVPA_OUTPUT_REPORTING_NAME = TEMP_STR
            WRITE(TEMP_STR,'(I6)') WVPA_ACCTNO
            WVPA_OUTPUT_REPORTING_NAME =TRIM(WVPA_OUTPUT_REPORTING_NAME)
     +                                           //' '//TEMP_STR
            WRITE(TEMP_STR,'(I7)') WVPA_SUB_ACCOUNT_NUMBER 
            WVPA_OUTPUT_REPORTING_NAME =TRIM(WVPA_OUTPUT_REPORTING_NAME)
     +                                             //' '//TEMP_STR
            WVPA_OUTPUT_REPORTING_NAME =TRIM(WVPA_OUTPUT_REPORTING_NAME)
     +                                        //' '//ACCOUNT_DESCRIPTION
            WVPA_REPORTING_NAME = WVPA_OUTPUT_REPORTING_NAME 
         END FUNCTION WVPA_REPORTING_NAME
!***********************************************************************
         FUNCTION WVPA_DEPART_EXP_REPORT_NAME(ACCOUNT_DESCRIPTION,
     +                                WVPA_ACCTNO,
     +                                WVPA_ACCOUNTING_UNIT,
     +                                WVPA_WORK_ORDER_NUMBER)
!***********************************************************************
!
            CHARACTER (LEN=DrillingAccountNameWidth) 
     +                                      WVPA_DEPART_EXP_REPORT_NAME,
     +                                      WVPA_OUTPUT_REPORTING_NAME
            INTEGER WVPA_ACCTNO,
     +              WVPA_ACCOUNTING_UNIT
            CHARACTER (LEN=*) :: ACCOUNT_DESCRIPTION
            CHARACTER (LEN=10) :: WVPA_WORK_ORDER_NUMBER
            CHARACTER*12 TEMP_STR
!
            WRITE(TEMP_STR,'(I4)') WVPA_ACCOUNTING_UNIT
            WVPA_OUTPUT_REPORTING_NAME = TEMP_STR
            WRITE(TEMP_STR,'(I6)') WVPA_ACCTNO
            WVPA_OUTPUT_REPORTING_NAME =TRIM(WVPA_OUTPUT_REPORTING_NAME)
     +                                           //' '//TEMP_STR
            TEMP_STR = WVPA_WORK_ORDER_NUMBER(1:7) 
            WVPA_OUTPUT_REPORTING_NAME =TRIM(WVPA_OUTPUT_REPORTING_NAME)
     +                                             //' '//TEMP_STR
            WVPA_OUTPUT_REPORTING_NAME =TRIM(WVPA_OUTPUT_REPORTING_NAME)
     +                                        //' '//ACCOUNT_DESCRIPTION
            WVPA_DEPART_EXP_REPORT_NAME = WVPA_OUTPUT_REPORTING_NAME 
         END FUNCTION WVPA_DEPART_EXP_REPORT_NAME
!***********************************************************************
         FUNCTION WVPA_WORK_ORDER_REPORT_NAME(ACCOUNT_DESCRIPTION,
     +                                WVPA_ACCTNO,
     +                                WVPA_ACCOUNTING_UNIT,
     +                                WVPA_WORK_ORDER_NUMBER)
!***********************************************************************
!
            CHARACTER (LEN=DrillingAccountNameWidth) 
     +                                      WVPA_WORK_ORDER_REPORT_NAME,
     +                                      WVPA_OUTPUT_REPORTING_NAME
            INTEGER WVPA_ACCTNO,
     +              WVPA_ACCOUNTING_UNIT
            CHARACTER (LEN=*) :: ACCOUNT_DESCRIPTION
            CHARACTER (LEN=10) :: WVPA_WORK_ORDER_NUMBER
            CHARACTER*12 TEMP_STR_ACCTNO
!
            WRITE(TEMP_STR_ACCTNO,'(I6)') WVPA_ACCTNO
            WVPA_OUTPUT_REPORTING_NAME =
     +                            TRIM(WVPA_WORK_ORDER_NUMBER(1:7))//','
            WVPA_OUTPUT_REPORTING_NAME =TRIM(WVPA_OUTPUT_REPORTING_NAME)
     +                                      //' '//TRIM(TEMP_STR_ACCTNO)
            WVPA_OUTPUT_REPORTING_NAME =TRIM(WVPA_OUTPUT_REPORTING_NAME)
     +                                        //' '//ACCOUNT_DESCRIPTION
            WVPA_WORK_ORDER_REPORT_NAME = WVPA_OUTPUT_REPORTING_NAME 
         END FUNCTION WVPA_WORK_ORDER_REPORT_NAME
      END MODULE DRILLING_REPT_PARAMETERS
!**********************************************************************
      FUNCTION BUILD_NUM()
!***********************************************************************
!
      CHARACTER*25 BUILD_NUM,BUILD_NUM_STR
      CHARACTER*256 SIM_FILE_NAME
      LOGICAL*4 FILE_EXISTS
      INTEGER*4 R_EXE_DATE,EXE_TIME,SIZE
      INTEGER*4 YEAR,MONTH,DAY
      INTEGER*2 DAYS_YEAR_TO_DATE,DAYS_2_DATE,MO
         CALL MYNAME(SIM_FILE_NAME)
         CALL FILE_INQUIRE(SIM_FILE_NAME,FILE_EXISTS,
     +                                         R_EXE_DATE,EXE_TIME,SIZE)
         CALL SEC2DATE(R_EXE_DATE,YEAR,MONTH,DAY) ! ,cHour,cMinute,cSecond)
         MO = MONTH
         DAYS_2_DATE = DAYS_YEAR_TO_DATE(MO) + DAY
         WRITE(BUILD_NUM_STR,"(I4,'.',I3.3)") YEAR,DAYS_2_DATE
         BUILD_NUM = "1.0.0.19 " // trim(BUILD_NUM_STR)
      RETURN
      END

!***********************************************************************
      SUBROUTINE MIDAS_ROOT_PROGRAM
!***********************************************************************

      use SpinDriftLib
      use prod_arrays_dimensions
      USE GRX_PLANNING_ROUTINES
      USE ProSymModule
      USE FINANCIAL_SWITCHES_COMMON
      use logging
      use miscmod
      use midas_decs
      use cl_data
      use endpoint
      use kepcocom
      use dreptcom
      USE SIZECOM
      use globecom
      use contracts_data
      use prodcom
      
      implicit none
!
! VARIABLES TO SIZE THE MODEL
!
      INCLUDE 'DSMFRCOM.MON'

! BEGINNING OF THE COMMON BLOCK SECTION FOR THE PRODUCTION MODULE
! VARIABLES FOR THE DETAILED REPORTS
!
! SHORT FORM ITEMS
!
      INTEGER*2 LAST_VARIABLE,STORE_LAST_VARIABLE,
     +          SET_CALENDAR_INFORMATION
      CHARACTER*1 SET_CHARACTER_DATA
      INTEGER*2 MIDAS_NORMAL_SIMULATION
!
!     VARIABLES FOR COST OF SERVICE
      LOGICAL*1 POSSIBLE_OPTIONS_EXIST,WVPA,NO_AUTO_INDEXING
!
!     TYPE DECLARATION FOR /SCREEN/
!
      LOGICAL*4 ESCAPE
!     LOGICAL*4 REPORT_FILE_EXISTS
! FILE NAMES
      CHARACTER*5 STUDY_NAME
!
! PARAMETER SECTION
!  COMMON BLOCK FOR ERROR MESSAGES
      LOGICAL*1 MESSAGE,DEBUG_ON
      INTEGER*2 SET_VIDEO_SYSTEM,INVERSE_VIDEO_COM
      LOGICAL*1 USER_TERMINATED
      COMMON /ERRORS/ INVERSE_VIDEO_COM,MESSAGE,DEBUG_ON,USER_TERMINATED
      LOGICAL*1 ASSET_ANALYST_ONLY
!
!  END COMMON BLOCK SECTION FOR THE PRODUCTION MODULES
!
! NEW VARIABLES ADDED BY M.S.G. JULY 29, 1993 - 
!      
      logical*1 LAHEY_LF95
      LOGICAL*1 VOID_LOGICAL,RETURN_DET_RPTS_FOR_ASSETS,
     +          RETURN_RUN_SPECS_FOR_ASSETS,
     +          READ_LH_GLOBAL_FILE,
     +          READ_ESCALATION_FILE
      LOGICAL*1 SET_STARTING_RECORD_4_UNIT38
      INTEGER*2 I2_RETURN_VALUE,STORED_SYSTEM_VARIABLES,
     +          POINTS_IN_LOAD_CURVE,CL_UNITS_READ
      CHARACTER*1 CHR_RETURN_VALUE,DSEXEC_OBJECT
      CHARACTER*5 :: BASE_CASE_FILES
      CHARACTER*256 BASE_FILE_DEFINITION,
     +              BASE_FILE_FAMILY, cwd
      LOGICAL*1 START_UP,TRANSACT_ANALYST_ONLY,
     +          SET_TRANSACT_ANALYST_ONLY_SWTCH,INIT_FILE_IS_ACTIVE,
     +          SET_ASSET_ANALYST_ONLY_SWITCH,RUN_COAL_MODEL_ONLY
      CHARACTER*2 CAPACITY_PLANNING_METHOD
      INTEGER*2 VOID_INT2,FINANCIAL_DRILLING_HEADER
      INTEGER*2 SOD_SIZE
      LOGICAL (KIND=1) :: SP_CAPEX_ACTIVE
      INTEGER*2 RUN_YEARS,EXTENSION_YEARS,FINANCIAL_SIMULATION_YEARS
      INTEGER (KIND=4) :: IOS
      character*256 :: package="midasmod", msg, stmp

! last variable used 628 12/11/97 msg
! last variable used 650 4/23/98 GAT
! last variable used 651 6/5/98 MSG GOVERNMENT REVENUES
! last variable used 652 7/1/98 MSG LTD PS ISSUE EXPENSE
! last variable used 653-657 8/8/98 MSG SRP VARIABLES
! last variable used 658-659 9/28/98 MSG CASH CS DIVIDENDS,PS_DIVIDEND_CASH_PAYMENTS
! last variable used 660-663 10/2/98 MSG ACCOUNTS RECEIVABLE, FUEL INVENTORY,
!                                        DEFERRED FUEL, ACCOUNTS PAYABLE
! last variable used 664 10/10/98 MSG Cash Flow Amortization
! last variable used 665 10/12/98 MSG Capacity Sales Revenues
! last variable used 666 10/21/98 MSG Invectment in acquisitions
      PARAMETER (SOD_SIZE = 670)  
!********** END OF THE DATA DECLARATIONS *********
!
!
      EXTERNAL ERROR_MESSAGE
!
      USER_TERMINATED = .FALSE.

      INVERSE_VIDEO_COM = SET_VIDEO_SYSTEM()
!      
      CALL NDPEXC
!
! SET SOME PARAMETERS
!
      
!
      LAST_VARIABLE = STORE_LAST_VARIABLE(SOD_SIZE)
!
      MESSAGE = .TRUE.
      CALL PRECFILL(' ')

      
      CALL START_DISPLAY_TIME
      
       if (trim(base_file_definition)=="") then
        call end_program("midasmod:00003 - " // 
     + "Base file definition not set before call to start_up().")
       endif
       
       call getcwd(cwd)
       call clear_files_from_last_run()
       
       VOID_LOGICAL = START_UP(DEBUG_ON,BASE_FILE_DEFINITION, 
     + POINTS_IN_LOAD_CURVE,BASE_FILE_FAMILY)
     
      if(trim(base_file_family)=="" .or. 
     + trim(base_file_family) == "NONE") then
        call end_program("midasmod:0003 - base file family not set.")
      endif
     
      CHR_RETURN_VALUE = DSEXEC_OBJECT(POINTS_IN_LOAD_CURVE)

      
      if(trim(cldata%scename) == trim(BASE_FILE_DEFINITION)) then
            call write_log_entry("midasmod:0002", "Scenario name " //
     + " and base_file_definition are the same: " // 
     + trim(cldata%scename) // ". This is a likely programming error.")
      endif
      
      call write_log_entry("midasmod:0001", "calling " // 
     + "base_case_files with scename=" // trim(cldata%scename) 
     + // " and " //
     + "base_file_definition=" // trim(BASE_FILE_DEFINITION) // 
     + " and base_file_family=" // trim(BASE_FILE_FAMILY))
      study_name=" "
      
      STUDY_NAME = BASE_CASE_FILES(BASE_FILE_DEFINITION,
     +   BASE_FILE_FAMILY)

! 11/08/06 ARRAYS USING MAX_SIMULATION_YEARS ARE NOW DYNAMIC Dr.G
      MAX_SIMULATION_YEARS = RUN_YEARS() + EXTENSION_YEARS() + 1
!
      VOID_LOGICAL = SET_ASSET_ANALYST_ONLY_SWITCH()
      WABASH_VALLEY = WABASH_VALLEY .AND. .NOT. ASSET_ANALYST_ONLY()
      VOID_LOGICAL = SET_TRANSACT_ANALYST_ONLY_SWTCH()
      CALL DSSCREEN(cldata%SCENAME)
      I2_RETURN_VALUE = STORED_SYSTEM_VARIABLES(POINTS_IN_LOAD_CURVE)
      I2_RETURN_VALUE = SET_CALENDAR_INFORMATION(INT2(1))
      CALL SET_UP_OVERLAY_FAMILIES(BASE_FILE_DEFINITION)

!      
! OPEN AND READ DSS FILES FILE
!
      CALL CREATE_REPORT_HEADER(cldata%SCENAME)
      VOID_LOGICAL = SET_STARTING_RECORD_4_UNIT38()
      END_POINT = 1
      GRX_SAVED_ENDPOINT = END_POINT
!
! IF THE BASE CASE EXISTS THE OVERHEAD FINANCIAL CALCULATIONS ARE NOT DONE
! **** NEED A CHECK FOR INDENTIFYING CHANGES TO THE BASE CASE
!
      CALL ERASEWC('BC*.BIN')
      CALL ERASEWC('OL*.BIN')
      CALL DISPLAY_TIME


         CALL MAKE_ALL_BINARY

      CALL DISPLAY_TIME
!
! SET PROPER ESCALATION VALUES
!
! THE CALL TO THE ESCALATION RATE OBJECT ADDED 5/20/93 FOR THE FINANCIAL
! CALCULATIONS
!
      IF(.NOT. RUN_COAL_MODEL_ONLY()) THEN
         VOID_LOGICAL = READ_ESCALATION_FILE()
         CALL ADDENDUMS(.TRUE.)

         IF(INIT_FILE_IS_ACTIVE()) THEN
            VOID_LOGICAL = RETURN_DET_RPTS_FOR_ASSETS()
            CALL MG_LOCATE_WRITE(16,9,'Financial asset overhead',
     +                                                   ALL_VERSIONS,1)
!
! ADDED FOR PROPERTY TAX CALCULATIONS 4/14/92
!
!
! START FINANCIAL DRILLING REPORT
!
            VOID_INT2 = FINANCIAL_DRILLING_HEADER()
            CALL OPEN_ASSET_VECTOR_FILE(82) ! 3/28/95 MAKING ONE OPEN
            CALL INIT_ASSET_CLASS_INFO  ! very slow routine NEED ELIMINATION CLASS B4 ASSET ANALYSIS
            CALL ZERO_ASSET_CLASS_ARRAYS
            CALL READ_CLASS_INITIALIZATION_FILE
            CALL PROCESS_CAPITAL_RATES_FILE
            CALL DISPLAY_TIME
            CALL FINASSET(.TRUE.)
            IF(.NOT. WVPA()) CALL ProSymInputData(.TRUE.)  ! Processes base file
            CALL CLOSE_ASSET_VECTOR_FILE
            CALL CLOSE_CAPITAL_RATES_VECTOR_FILE
!
         ENDIF
         CALL MG_CLEAR_LINE(16,9,36,ALL_VERSIONS,0)
         CALL CALULATE_OVERHEAD_TIME
!
! ENTRY POINT FOR THE ENDPOINT LOOP
!
         IF(WVPA()) THEN
            CALL WVPA_DATA_BASES
            CALL WVPA_TRACKER_DATA_BASE()
            CALL WVPA_PRC_ACTUAL_PURCHASED_POWER()
         ENDIF

         IF(POSSIBLE_OPTIONS_EXIST() .AND.
     +                          CAPACITY_PLANNING_METHOD() == 'AN') THEN
            CALL MIDAS_ANN_DECOMP_SIMULATION(cldata%SCENAME)
         ELSE
            I2_RETURN_VALUE = MIDAS_NORMAL_SIMULATION()
         ENDIF
      ELSE
         I2_RETURN_VALUE = MIDAS_NORMAL_SIMULATION()
      ENDIF
      CALL DISPLAY_TIME
      CALL MG_LOCATE_WRITE(15,70," ",3,0)
      IF(.NOT. NO_AUTO_INDEXING() .AND. .NOT. SP_CAPEX_ACTIVE()) THEN
         CALL START_INDEX_TIMING
         CALL INDEX_RESULT_FILES(clData%SCENAME)
         CALL END_INDEX_TIMING
      ENDIF
      CLOSE(4,IOSTAT=IOS)
      MESSAGE = .FALSE.
      RETURN
 1000 FORMAT('&',A,A,A,A)
      END
!***********************************************************************
      FUNCTION END_OF_STUDY_FLAG()
!***********************************************************************
      LOGICAL*1 SET_END_OF_STUDY_FLAG_TRUE,
     +          END_OF_STUDY_FLAG,
     +          SET_END_OF_STUDY_FLAG_FALSE,
     +          VALUE_END_OF_STUDY_FLAG/.FALSE./
!
         END_OF_STUDY_FLAG = VALUE_END_OF_STUDY_FLAG
      RETURN
      ENTRY SET_END_OF_STUDY_FLAG_TRUE
         VALUE_END_OF_STUDY_FLAG = .TRUE.
         SET_END_OF_STUDY_FLAG_TRUE = .TRUE.
      RETURN
      ENTRY SET_END_OF_STUDY_FLAG_FALSE
         VALUE_END_OF_STUDY_FLAG = .FALSE.
         SET_END_OF_STUDY_FLAG_FALSE = .FALSE.
      RETURN
      END
!***********************************************************************
      FUNCTION END_OF_PLAN_FLAG()
!***********************************************************************
      LOGICAL*1 SET_END_OF_PLAN_FLAG_TRUE,
     +          END_OF_PLAN_FLAG,
     +          SET_END_OF_PLAN_FLAG_FALSE,
     +          VALUE_END_OF_PLAN_FLAG/.FALSE./
!
         END_OF_PLAN_FLAG = VALUE_END_OF_PLAN_FLAG
      RETURN
      ENTRY SET_END_OF_PLAN_FLAG_TRUE
         VALUE_END_OF_PLAN_FLAG = .TRUE.
         SET_END_OF_PLAN_FLAG_TRUE = .TRUE.
      RETURN
      ENTRY SET_END_OF_PLAN_FLAG_FALSE
         VALUE_END_OF_PLAN_FLAG = .FALSE.
         SET_END_OF_PLAN_FLAG_FALSE = .FALSE.
      RETURN
      END
!***********************************************************************
      FUNCTION ASSET_ANALYST_ONLY()
!***********************************************************************
!
      use SpinDriftLib
      use prod_arrays_dimensions
      use kepcocom
      USE SIZECOM
      
      LOGICAL (KIND=1) :: ASSET_ANALYST_ONLY,
     +                    SET_ASSET_ANALYST_ONLY_SWITCH
      LOGICAL (KIND=1) :: STATE_OF_ASSET_ANALYST=.FALSE.
      LOGICAL (KIND=1) :: TF_FILE_EXISTS,SYSTEM_BASED_FORECAST,
     +                    SYSTEM_FILE_EXISTS,
     +                    NERC_REGION_BASED_FORECAST,
     +                    CD_LINE_ASSET_ANALYST_ACTIVE,CL_FILE_EXISTS,
     +                    SP_CAPEX_ACTIVE
         ASSET_ANALYST_ONLY = STATE_OF_ASSET_ANALYST
      RETURN
!***********************************************************************
      ENTRY SET_ASSET_ANALYST_ONLY_SWITCH()
!***********************************************************************
!
         IF(SP_CAPEX_ACTIVE()) THEN
            STATE_OF_ASSET_ANALYST = .FALSE.
         ELSE
            CALL DOES_TF_FILE_EXIST(TF_FILE_EXISTS)
            CALL DOES_CL_FILE_EXIST(CL_FILE_EXISTS)
            CALL SYSTEM_FORECAST_FILE_EXISTS(SYSTEM_FILE_EXISTS)
            STATE_OF_ASSET_ANALYST = .NOT. (

     +                   (SYSTEM_BASED_FORECAST() .AND.
     +                                          SYSTEM_FILE_EXISTS) .OR.
     +                   (.NOT. SYSTEM_BASED_FORECAST()
     +                               .AND. CL_FILE_EXISTS
     +                                         .AND. WABASH_VALLEY) .OR.
     +                   .NOT. NERC_REGION_BASED_FORECAST()) .OR.
     +                    CD_LINE_ASSET_ANALYST_ACTIVE()
         ENDIF
         SET_ASSET_ANALYST_ONLY_SWITCH = STATE_OF_ASSET_ANALYST

      RETURN
      END
!***********************************************************************
      FUNCTION TRANSACT_ANALYST_ONLY()
!***********************************************************************
!
      LOGICAL (KIND=1) :: TRANSACT_ANALYST_ONLY,
     +                    SET_TRANSACT_ANALYST_ONLY_SWTCH
      LOGICAL (KIND=1) :: NERC_REGION_BASED_FORECAST,
     +                    SP_CAPEX_ACTIVE
      LOGICAL (KIND=1) :: STATE_OF_TRANSACT_ANALYST=.FALSE.
         IF(SP_CAPEX_ACTIVE()) THEN
            TRANSACT_ANALYST_ONLY = .TRUE.
         ELSE
            TRANSACT_ANALYST_ONLY = STATE_OF_TRANSACT_ANALYST
         ENDIF
      RETURN
!***********************************************************************
      ENTRY SET_TRANSACT_ANALYST_ONLY_SWTCH()
!***********************************************************************
!
         STATE_OF_TRANSACT_ANALYST = NERC_REGION_BASED_FORECAST()         
         SET_TRANSACT_ANALYST_ONLY_SWTCH = STATE_OF_TRANSACT_ANALYST 
      RETURN
      END
!
!***********************************************************************
!
      SUBROUTINE BC_RESET
!
      USE SIZECOM
!
      INTEGER*2 FA_UNIT/44/,NEW_ADDITIONS_FA/444/

         CALL SET_FOSSILOL_TO_OL !debugstop
!
         CALL RESET_FUASTOL
         CALL RESET_ESCAL_OL
         CALL RESET_COST_ALLOC_OL

         CALL RESET_SYSFRC_OL
         CALL RESET_CLASS_FORECAST_OL
         CALL RESET_CLS_OL
         CALL RESET_CLASS_HIST_OL
         CALL RESET_CAP_STACK_OL
         CALL RESET_UPE_OL
         CALL RESET_ELASTICITY_FILE_OL
         CALL RESET_ECON_OL
         CALL RESET_HYDRO_OL
         CALL RESET_CAPACITY_RATIOS_OL
         CALL RESET_ENV_COM_OL
         CALL RESET_ELIMINATION_OL
!         
         CALL RESET_CONTRACT_OL
         CALL RESET_NEWPLT_PROD_OL
         CALL RESET_NEWPLT_FIN_OL
         CALL RESET_INTRSTOL
         CALL RESET_CAP_RATES_OL
         CALL RESET_PARMASOL
         CALL RESET_INITOL
         CALL RESET_TAX_LOSS_OL
         CALL RESET_PARMFNOL
         CALL RESET_NFUEL_OL
         CALL RESET_DEBITOL
         CALL RESET_PRODP_OL
         CALL RESET_FUEL_PRICE_OL
         CALL RESET_LDMGT_OL
         CALL RESET_AREA_OL
         CALL RESET_DEBTOL
         CALL RESET_EXASTOL
         CALL RESET_EXPENOL
         CALL RESET_FUEL_INVENTORY_OL
         CALL RESET_SERVICE_TRANS_OL
         CALL RESET_FUEL_EMISSIONS_FILE_OL
         CALL RESET_LMGLS_OL
         CALL RESET_LOAD_MAN_FIN_OL
         CALL RESET_CLASS_RUN_SWITCH_OL
         CALL RESET_CLASS_SALES_OL
         CALL RESET_DET_RPT_OL
         CALL RESET_ENERGY_PRODUCTS_OL
         CALL RESET_SCENARIO_MAKER_OL
!         CALL RESET_REGIONAL_OUTAGES_OL
!         CALL RESET_REGIONAL_PARAMETER_OL
         CALL RESET_REG_OUT_PARAM_OL
         CALL RESET_FUEL_DERIVATIVES_OL
         CALL RESET_CPL_OL
         CALL RESET_RDI_POWERDAT_OL
         CALL RESET_RDI_BASECASE_OL
         CALL RESET_TSYFRC_OL 
         CALL RESET_OUTAGE_OL
         CALL RESET_WEEKLY_HYDRO_OL
         CALL RESET_RPS_REQUIREMENTS_OL
         CALL RESET_RPS_STATE_DEMAND_OL
         CALL RESET_THERMAL_RETROFIT_OL
         CALL RESET_CO2_PARAM_OL
         CALL RESET_RPS_PROGRAM_OL ! 11/13/18
         CALL RESET_WEATHER_DEMAND_OL
! 4/14/02
         CALL RESET_PAYABLES_OL
         CALL RESET_DAY_TYPE_OL
         CALL RESET_USER_DAY_OL
         CALL RESET_ICAP_OL
!
         CALL RESET_MKROUP_OL
!         
         CALL RESET_TRCON_OL
         CALL RESET_TRPATH_OL
         CALL RESET_TGROUP_OL
!         
         CALL RESET_MARKET_PRICES
         CALL RESET_SCENARIO_HOURLY
         CALL RESET_NEW_MARKET_PRICES
         CALL RESET_LH_GLOBAL
         CALL RESET_ELECT_HOUR_MULT
         CALL RESET_GAS_HOUR_MULT
         CALL RESET_OIL_HOUR_MULT
         CALL RESET_REFERENCE_LOAD
         CALL RESET_ADDENDUM_OL
!         
         CALL RESET_GAS_LINK_OL()
         CALL RESET_GAS_NODE_OL()
         CALL RESET_GAS_STORAGE_OL()
         CALL RESET_GAS_DEMAND_OL()
         CALL RESET_GAS_SUPPLY_OL()
         CALL RESET_GAS_WEATHER_DEMAND_OL()
! COAL MODEL     
         CALL RESET_COAL_PRICE_MODEL_OL_CODES()
! 070409.
         CALL RESET_TRAN_EXP_OL()
!         
      RETURN
      END
!***********************************************************************
!
!     A SUBROUTINE TO INTEGRATE THE PROB CURVE TO FIND THE ENERGY
!
!***********************************************************************
      SUBROUTINE INTEG8(ENERGY,LODDUR,LPROB,LDCPTS,HOURS,BASE_ENRG)
      REAL LPROB(*),LODDUR(*)
      REAL*8 ENERGY
      REAL BASE_ENRG
      INTEGER*2 I,HOURS,LDCPTS
      ENERGY = 0. D0
      DO I = 1, LDCPTS - 1
         ENERGY = ENERGY + 
     +                 DBLE(LPROB(I)+LPROB(I+1))*(LODDUR(I+1)-LODDUR(I))
      ENDDO
      ENERGY = (ABS(ENERGY)/2. + DBLE(BASE_ENRG)) * DBLE(HOURS)
      RETURN
      END
!
!
! LAST_VARIABLE FUNCTION
!
!***********************************************************************
!     module StoredSysVarLocals
!***********************************************************************
!        REAL*4 DELTA_X_FOR_LOAD_CURVE/1./
!        INTEGER*2 POINTS_IN_CURRENT_CURVE,POINTS_IN_LOAD_CURVE
!        SAVE POINTS_IN_CURRENT_CURVE
!        SAVE POINTS_IN_LOAD_CURVE
!     END module
!***********************************************************************
      INTEGER*2 FUNCTION STORED_SYSTEM_VARIABLES(R_POINTS_IN_LOAD_CURVE)
!***********************************************************************
!

      use logging
      use SpinDriftLib
      use prod_arrays_dimensions
      USE SIZECOM

      REAL*4 DELTA_X_FOR_LOAD_CURVE
      INTEGER*2 POINTS_IN_CURRENT_CURVE,POINTS_IN_LOAD_CURVE
      COMMON /StoredSysVars/DELTA_X_FOR_LOAD_CURVE,
     +                      POINTS_IN_CURRENT_CURVE,POINTS_IN_LOAD_CURVE
      INTEGER*2 VALUE_OF_LAST_VARIABLE,S_LAST_VARIABLE,
     +          STORE_LAST_VARIABLE
!
      INTEGER*2 LAST_VARIABLE,MAX_YEARS
      INTEGER*2 max_fuel_types_mm,
     +          R_POINTS_IN_LOAD_CURVE
      LOGICAL*1 SET_POINTS
      INTEGER*2 R_IPOT,IPNT,SET_CURRENT_IPNT
      REAL R_DX
!
!  COMMON BLOCK VARIABLES FOR /WORLD/
      CHARACTER*2 DATA_DRIVE,ACTIVE_DATA_DRIVE
      INTEGER*2 BASE_YEAR_COMMON,MAXIMUM_YEARS,BASE_YEAR
      COMMON /WORLD/ BASE_YEAR_COMMON,MAXIMUM_YEARS
      COMMON /DATA_DRIVE_LOCATION/ DATA_DRIVE
      SAVE VALUE_OF_LAST_VARIABLE
!
         POINTS_IN_LOAD_CURVE = MIN(R_POINTS_IN_LOAD_CURVE,600)
         IF(2*AINT(POINTS_IN_LOAD_CURVE/2.0)==POINTS_IN_LOAD_CURVE) THEN
            POINTS_IN_LOAD_CURVE = POINTS_IN_LOAD_CURVE-1
         ENDIF
         MAXIMUM_YEARS = MAX_SIMULATION_YEARS + 1
         BASE_YEAR_COMMON = BASE_YEAR()
         DATA_DRIVE = ACTIVE_DATA_DRIVE()
         STORED_SYSTEM_VARIABLES = MAXIMUM_YEARS
      RETURN
      ENTRY IPNT
         IF(DELTA_X_FOR_LOAD_CURVE > 0.) THEN
            IPNT = POINTS_IN_CURRENT_CURVE
         ELSE
            IPNT = POINTS_IN_LOAD_CURVE
         ENDIF
      RETURN
      ENTRY SET_CURRENT_IPNT(R_IPOT)
         POINTS_IN_CURRENT_CURVE = R_IPOT
         SET_CURRENT_IPNT = POINTS_IN_CURRENT_CURVE
      RETURN
      ENTRY max_fuel_types_mm
         max_fuel_types_mm = 50
      RETURN
      ENTRY MAX_YEARS
         MAX_YEARS = MAX_SIMULATION_YEARS + 1
      RETURN
      ENTRY LAST_VARIABLE
         LAST_VARIABLE = VALUE_OF_LAST_VARIABLE
      RETURN
      ENTRY STORE_LAST_VARIABLE(S_LAST_VARIABLE)
         VALUE_OF_LAST_VARIABLE = S_LAST_VARIABLE
         STORE_LAST_VARIABLE = VALUE_OF_LAST_VARIABLE
      RETURN
      END
!***********************************************************************
      LOGICAL*1 FUNCTION SET_DELTA_X_VALUE(R_DELTA_X_FOR_LOAD_CURVE)
!***********************************************************************
!
      REAL*4 DELTA_X_FOR_LOAD_CURVE
      INTEGER*2 POINTS_IN_CURRENT_CURVE,POINTS_IN_LOAD_CURVE
      COMMON /StoredSysVars/DELTA_X_FOR_LOAD_CURVE,
     +                      POINTS_IN_CURRENT_CURVE,POINTS_IN_LOAD_CURVE
!
      REAL*4 R_DELTA_X_FOR_LOAD_CURVE,R_DX
      INTEGER*2 R_IPOT
      LOGICAL*1 SET_POINTS
!
         IF(R_DELTA_X_FOR_LOAD_CURVE > 0.) THEN
            DELTA_X_FOR_LOAD_CURVE = MAX(R_DELTA_X_FOR_LOAD_CURVE,1.)
         ELSE
            DELTA_X_FOR_LOAD_CURVE = R_DELTA_X_FOR_LOAD_CURVE
         ENDIF
         SET_DELTA_X_VALUE = .TRUE.
      RETURN
!***********************************************************************
      ENTRY SET_POINTS(R_IPOT,R_DX)
!***********************************************************************
         R_IPOT = POINTS_IN_LOAD_CURVE
         POINTS_IN_CURRENT_CURVE = POINTS_IN_LOAD_CURVE
         SET_POINTS = .TRUE.
         R_DX = DELTA_X_FOR_LOAD_CURVE
         IF(DELTA_X_FOR_LOAD_CURVE > 0.) SET_POINTS = .FALSE.
      RETURN
      END
!***********************************************************************
      CHARACTER*5 FUNCTION VERSION()
!***********************************************************************
!
      CHARACTER*256 SIM_FILE_NAME,UC_SIM_FILE_NAME
      INTEGER*4 POS
      LOGICAL*1 LAHEY_LF95
         CALL MYNAME(SIM_FILE_NAME)
         CALL UPC(SIM_FILE_NAME,UC_SIM_FILE_NAME)
         POS = INDEX(UC_SIM_FILE_NAME,'.EXE')
         SIM_FILE_NAME = UC_SIM_FILE_NAME(POS-5:POS-1)
         IF(LAHEY_LF95()) THEN
            VERSION = 'F95-A'
            IF(INDEX(SIM_FILE_NAME,'WIN') /= 0 .OR.
     +           INDEX(SIM_FILE_NAME,'VSW') /= 0 .OR.
     +                 INDEX(SIM_FILE_NAME,'RW') /= 0) VERSION = 'F95-W'
         ELSE
            IF(INDEX(SIM_FILE_NAME,'6A') /= 0) THEN
               VERSION = '6.0A '
            ELSEIF(INDEX(SIM_FILE_NAME,'6L') /= 0) THEN
               VERSION = '6.0L '
            ELSEIF(INDEX(SIM_FILE_NAME,'6M') /= 0) THEN
               VERSION = '6.0M '
            ELSEIF(INDEX(SIM_FILE_NAME,'5B') /= 0) THEN
               VERSION = '5.0B '
            ELSEIF(INDEX(SIM_FILE_NAME,'5D') /= 0) THEN
               VERSION = '5.0D '
            ELSEIF(INDEX(SIM_FILE_NAME,'5C') /= 0) THEN
               VERSION = '5.0C '
            ELSEIF(INDEX(SIM_FILE_NAME,'8') /= 0) THEN
               VERSION = '8.0A '
            ELSE
               VERSION = '7.0A '
            ENDIF
         ENDIF
      RETURN
      END
!
!***********************************************************************
      FUNCTION SET_VIDEO_SYSTEM()
!***********************************************************************
!
      use SpinDriftLib
      use prod_arrays_dimensions

      INTEGER*2 SET_VIDEO_SYSTEM
      INTEGER*2 DBLUBKG,DBLUCHR,WHITEBKG,WHITECHR,BRITE
      INTEGER*2 VIDEOTYP,VIDEO_TYPE
      INTEGER*2 NORMAL,INVERSE
      INTEGER*2 NORMAL_VIDEO,INVERSE_VIDEO
      INTEGER*2 R_NORMAL_VIDEO,R_INVERSE_VIDEO
      LOGICAL*4 ISCROLL
      PARAMETER(DBLUBKG=16,DBLUCHR=1,WHITEBKG=112,WHITECHR=7,
     +          BRITE=8,NORMAL=7,INVERSE=112)
      SAVE R_NORMAL_VIDEO,R_INVERSE_VIDEO
!
         VIDEO_TYPE = VIDEOTYP()
         IF(VIDEO_TYPE < 7 .AND. .NOT. ISCROLL()) THEN
            R_NORMAL_VIDEO = DBLUBKG+WHITECHR+BRITE
            R_INVERSE_VIDEO = DBLUCHR+WHITEBKG+BRITE
         ELSE
            CALL VIDEOMOD(2)
            R_NORMAL_VIDEO = NORMAL
            R_INVERSE_VIDEO = INVERSE
         ENDIF
         CALL OPENWIND(4,0,0,23,79,16+4,R_NORMAL_VIDEO)
         CALL OPENWIND(0,0,0,23,79,4,R_NORMAL_VIDEO)
         CALL SETATTR(R_NORMAL_VIDEO)
         CALL CLS
         CALL STORE_VIDEO_MODES(R_INVERSE_VIDEO,R_NORMAL_VIDEO)
         SET_VIDEO_SYSTEM = R_INVERSE_VIDEO
      RETURN
      ENTRY NORMAL_VIDEO
         NORMAL_VIDEO = R_NORMAL_VIDEO
      RETURN
      ENTRY INVERSE_VIDEO
         INVERSE_VIDEO = R_INVERSE_VIDEO
      RETURN
      END
!***********************************************************************
      FUNCTION SET_CHARACTER_DATA()
!***********************************************************************
!
      USE PROD_ARRAYS_DIMENSIONS
!         USE SIZECOM
      use globecom

         CHARACTER*1 SET_CHARACTER_DATA
         CHARACTER*20 SAVE_CLASS_NAME(MAX_LOAD_CLASSES+1)/'Commercial',
     +            'Residential','Industrial','Other 1','Other 2',
     +            'Other 3','System '/,CLASS_NAME
         INTEGER*2 CLASS
!        SAVE SAVE_CLASS_NAME
         SET_CHARACTER_DATA = 'X'
      RETURN
      ENTRY CLASS_NAME(CLASS)
         CLASS_NAME = SAVE_CLASS_NAME(CLASS)
      RETURN
      END
!
! ADDED 8/4/93
!
!***********************************************************************
      FUNCTION SET_CALENDAR_INFORMATION(START_MO)
!***********************************************************************
!
      INTEGER*2 SET_CALENDAR_INFORMATION
      INTEGER*2 MONTH,START_MO,I,J
      INTEGER*2 DAYS_IN
      INTEGER*2 DAYS_YEAR_TO_DATE
      INTEGER*2 ICLNDR(12)
     +           /0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334/
      INTEGER*2 DAYS_PER(12),TOTAL_DAYS_AT_THE_END_OF(12),
     +          TOTAL_DAYS_AT_THE_END_OF_MONTH
      INTEGER*2 DAYS_BY_MONTH(12)/31,28,31,30,31,30,31,31,30,31,30,31/
      INTEGER*2 HOURS_IN_EACH_MONTH(12)
     +                 /744,672,744,720,744,720,744,744,720,744,720,744/
      INTEGER*2 CUM_HOURS_END_OF_EACH_MONTH(12)
     +      /744,1416,2160,2880,3624,4344,5088,5832,6552,7296,8016,8760/
      INTEGER*2 DAYS_IN_EACH_MONTH,CUM_HOURS_AT_THE_END_OF
      INTEGER*2 HOURS_IN_PERIOD
      CHARACTER*1 SET_CHARACTER_CALENDAR_INFO,VOID_CHAR
!
      SAVE DAYS_PER,TOTAL_DAYS_AT_THE_END_OF
!
         TOTAL_DAYS_AT_THE_END_OF_MONTH = 0
         J = 1
         DO I = START_MO, 12
            DAYS_PER(J) = DAYS_BY_MONTH(I)
            TOTAL_DAYS_AT_THE_END_OF_MONTH = DAYS_BY_MONTH(I) +
     +                                    TOTAL_DAYS_AT_THE_END_OF_MONTH
            TOTAL_DAYS_AT_THE_END_OF(J) = TOTAL_DAYS_AT_THE_END_OF_MONTH
            J = J + 1
         ENDDO
         DO I = 1, START_MO-1
            DAYS_PER(J) = DAYS_BY_MONTH(I)
            TOTAL_DAYS_AT_THE_END_OF(J) = TOTAL_DAYS_AT_THE_END_OF(J-1)+
     +                                                  DAYS_BY_MONTH(I)
            J = J + 1
         ENDDO
         VOID_CHAR = SET_CHARACTER_CALENDAR_INFO(START_MO)
         SET_CALENDAR_INFORMATION = START_MO
      RETURN
      ENTRY DAYS_IN(MONTH)
         IF(MONTH == 0) THEN
            DAYS_IN = 0
         ELSE
            DAYS_IN = DAYS_PER(MONTH)
         ENDIF
      RETURN
      ENTRY DAYS_IN_EACH_MONTH(MONTH)
         DAYS_IN_EACH_MONTH = DAYS_BY_MONTH(MONTH)
      RETURN
      ENTRY HOURS_IN_PERIOD(MONTH)
         HOURS_IN_PERIOD = HOURS_IN_EACH_MONTH(MONTH)
      RETURN
      ENTRY DAYS_YEAR_TO_DATE(MONTH)
         DAYS_YEAR_TO_DATE = ICLNDR(MONTH)
      RETURN
      ENTRY CUM_HOURS_AT_THE_END_OF(MONTH)
         CUM_HOURS_AT_THE_END_OF = CUM_HOURS_END_OF_EACH_MONTH(MONTH)
      RETURN
      END
!***********************************************************************
      FUNCTION SET_CHARACTER_CALENDAR_INFO(START_MO)
!***********************************************************************
!
      CHARACTER*1 SET_CHARACTER_CALENDAR_INFO
      LOGICAL*1 SALT_RIVER_PROJECT
      CHARACTER*20 MONTH_NAME
      CHARACTER*6 SHORT_MONTH_NAMES,BAL_SHORT_MONTH_NAMES
      CHARACTER*7 DATE_12_MOS_ENDING
      CHARACTER*9 LOADS_MONTH_NAME
      INTEGER*2 MONTH,I,START_MO,J,PRODUCTION_PERIODS
      INTEGER*2 PERIOD,DAY_TYPE
      CHARACTER*9 TEMP_NAME(12)
      CHARACTER*9 DAY_TYPE_NAME(3)/'Weekday','Weekend','Peak day'/
      CHARACTER*9 DAY_NAME
      CHARACTER*9 LOADS_NAME(12)/'January','February','March','April',
     +                         'May','June','July','August','September',
     +                         'October','November','December'/
      CHARACTER*6 MONTH_NAMES_SHORT(0:12)/'Annual','Jan','Feb','March',
     +       'April','May','June','July','Aug','Sept','Oct','Nov','Dec'/
      CHARACTER*3 MONTH_CAP_NAMES_SHORT(12)/'JAN','FEB','MAR',
     +            'APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC'/
      CHARACTER*3 MONTH_3_CHAR_NAMES_SHORT(12)/'Jan','Feb','Mar','Apr',
     +            'May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'/,
     +            THREE_CHAR_MONTH_NAMES
      CHARACTER*6 SRP_MONTH_NAMES_SHORT(0:12)/'Annual','May','June',
     +                                'July','Aug','Sept','Oct','Nov',
     +                                'Dec','Jan','Feb','March','April'/
      CHARACTER*10 SRP_MONTH_NAMES_LONG(0:12)/'Annual','May','June',
     +      'July','August','September','October','November','December',
     +      'January','February','March','April'/
      CHARACTER*10 MONTH_NAMES_LONG(0:12)/'Annual',
     +     'January','February','March','April','May','June',
     +     'July','August','September','October','November','December'/,
     +      BAL_LONG_MONTH_NAMES,LONG_MONTH_NAMES
      CHARACTER*20 PERIOD_NAME(12)/
     +      'January','February','March','April','May','June',
     +      'July','August','September','October','November','December'/
      INTEGER (KIND=2) :: TEMP_YR,BASE_YEAR,R_YR,R_MO
      CHARACTER (LEN=2) :: TEMP_YR_STR
      CHARACTER (LEN=4) :: TEMP_YR_STR_I4
      CHARACTER (LEN=5) :: MONTH_YEAR_FORMATT
!      
         J = 1
         DO I = START_MO, 12
            TEMP_NAME(J) = PERIOD_NAME(I)
            J = J + 1
         ENDDO
         DO I = 1, START_MO-1
            TEMP_NAME(J) = PERIOD_NAME(I)
            J = J + 1
         ENDDO
!         IF(PRODUCTION_PERIODS() == 1) THEN
!            PERIOD_NAME(1) = 'Annual'
!         ELSE ! IF(PRODUCTION_PERIODS() == 12) THEN
!            DO I = 1, 12
               PERIOD_NAME = TEMP_NAME
!            ENDDO
!         ENDIF
         IF(SALT_RIVER_PROJECT()) THEN 
!            MONTH_NAMES_SHORT = SRP_MONTH_NAMES_SHORT
!            MONTH_NAMES_LONG = SRP_MONTH_NAMES_LONG
         ENDIF
         SET_CHARACTER_CALENDAR_INFO = 'X'
      RETURN
!***********************************************************************
      ENTRY MONTH_NAME(MONTH)
!***********************************************************************
         IF(MONTH <= 0 .OR. MONTH > 12) THEN
            MONTH_NAME = "NA"
         ELSE
            MONTH_NAME = PERIOD_NAME(MONTH)
         ENDIF
      RETURN
!***********************************************************************
      ENTRY LOADS_MONTH_NAME(MONTH)
!***********************************************************************
         IF(MONTH <= 0 .OR. MONTH > 12) THEN
            LOADS_MONTH_NAME = "NA"
         ELSE
            LOADS_MONTH_NAME = LOADS_NAME(MONTH)
         ENDIF
      RETURN
!***********************************************************************
      ENTRY SHORT_MONTH_NAMES(MONTH)
!***********************************************************************
         IF(MONTH < 0 .OR. MONTH > 12) THEN
            SHORT_MONTH_NAMES = "NA"
         ELSE
            SHORT_MONTH_NAMES = MONTH_NAMES_SHORT(MONTH)
         ENDIF
      RETURN
!***********************************************************************
      ENTRY BAL_SHORT_MONTH_NAMES(MONTH)
!***********************************************************************
         IF(MONTH == 0) THEN
            BAL_SHORT_MONTH_NAMES = 'BOY'
         ELSE
            BAL_SHORT_MONTH_NAMES = MONTH_NAMES_SHORT(MONTH)
         ENDIF
      RETURN
!***********************************************************************
      ENTRY LONG_MONTH_NAMES(MONTH)
!***********************************************************************
         IF(MONTH < 0 .OR. MONTH > 12) THEN
            LONG_MONTH_NAMES = "NA"
         ELSE
            LONG_MONTH_NAMES = MONTH_NAMES_LONG(MONTH)
         ENDIF
      RETURN
!***********************************************************************
      ENTRY THREE_CHAR_MONTH_NAMES(MONTH)
!***********************************************************************
         IF(MONTH <= 0 .OR. MONTH > 12) THEN
            THREE_CHAR_MONTH_NAMES = "NA"
         ELSE
            THREE_CHAR_MONTH_NAMES = MONTH_3_CHAR_NAMES_SHORT(MONTH)
         ENDIF
      RETURN
!***********************************************************************
      ENTRY BAL_LONG_MONTH_NAMES(MONTH)
!***********************************************************************
         IF(MONTH == 0) THEN
            BAL_LONG_MONTH_NAMES = 'BOY'
         ELSE
            BAL_LONG_MONTH_NAMES = MONTH_NAMES_LONG(MONTH)
         ENDIF
      RETURN
!***********************************************************************
      ENTRY DAY_NAME(DAY_TYPE)
!***********************************************************************
         DAY_NAME = DAY_TYPE_NAME(DAY_TYPE)
      RETURN
!***********************************************************************
      ENTRY MONTH_YEAR_FORMATT(R_MO,R_YR)
!***********************************************************************
         TEMP_YR = BASE_YEAR()+R_YR-2000
         WRITE(TEMP_YR_STR,'(I2.2)') TEMP_YR
         MONTH_YEAR_FORMATT = MONTH_CAP_NAMES_SHORT(R_MO)//TEMP_YR_STR
      RETURN
!***********************************************************************
      ENTRY DATE_12_MOS_ENDING(R_MO,R_YR)
!***********************************************************************
         TEMP_YR = BASE_YEAR()+R_YR
         WRITE(TEMP_YR_STR_I4,'(I4)') TEMP_YR
         DATE_12_MOS_ENDING=MONTH_CAP_NAMES_SHORT(R_MO)//TEMP_YR_STR_I4
      END
!***********************************************************************
      FUNCTION GET_MONTH_NUMBER(MONTHLY_LAST_MONTH_STR)
!***********************************************************************
!
      INTEGER*2 GET_MONTH_NUMBER 
      CHARACTER*(*) MONTHLY_LAST_MONTH_STR
!
         GET_MONTH_NUMBER = -1       
         IF(INDEX(MONTHLY_LAST_MONTH_STR,'May') /= 0) THEN
            GET_MONTH_NUMBER = 5
         ELSEIF(INDEX(MONTHLY_LAST_MONTH_STR,'Jun') /= 0) THEN
            GET_MONTH_NUMBER = 6
         ELSEIF(INDEX(MONTHLY_LAST_MONTH_STR,'Jul') /= 0) THEN
            GET_MONTH_NUMBER = 7
         ELSEIF(INDEX(MONTHLY_LAST_MONTH_STR,'Aug') /= 0) THEN
            GET_MONTH_NUMBER = 8
         ELSEIF(INDEX(MONTHLY_LAST_MONTH_STR,'Non') /= 0) THEN
            GET_MONTH_NUMBER = -1
         ELSE
            GET_MONTH_NUMBER =
     +                 INDEX('JFMAMJJASOND',MONTHLY_LAST_MONTH_STR(1:1))
         ENDIF
      RETURN   
      END
!***********************************************************************
      SUBROUTINE MAKE_ALL_BINARY
!***********************************************************************
!
      USE BASE_FILE_NAMES_FROM_BFIL_OBJ  ! SOURCE IN MODULES95
      use SpinDriftLib
      use prod_arrays_dimensions
      USE SIZECOM

      CHARACTER*1 UTILITY_TYPE
      LOGICAL (KIND=1) :: SYSTEM_BASED_FORECAST,
     +                    CLASS_BASED_FORECAST,
     +                    NERC_REGION_BASED_FORECAST,
     +                    WVPA
      LOGICAL (KIND=1) :: RUN_COAL_MODEL_ONLY
!
!      CALL LOCATE(16,9)
!      WRITE(6,"('&',A)") 'Input file overhead'
      CALL MG_LOCATE_WRITE(16,9,'Input file overhead',ALL_VERSIONS,1)
!***********************************************************************
!
!
! THESE MAKEBINs AND DYNAMIC VECTOR ALLOCATIONS FOR CP&L MUST BE RUN BEFORE
!  THE ASSET VECTOR ROUTINES 4/7/99 MSG
!
! 11/01/18 CONVERT THE RPS PROGRAMS FILE TO BINARY
!
      CALL RPS_PROGRAM_MAKEBIN()

!
! COAL MODEL FILES
!      
      CALL COAL_BASIN_MAKEBIN(COAL_NODE)
      CALL COAL_SUPPLY_MAKEBIN(COAL_SUPPLY_FORECAST)
      CALL COAL_TRANSPORT_LINKS_MAKEBIN(COAL_TRANSPORT,'HT')
      CALL PLANT_DEMAND_MAKEBIN(PLANT_DEMAND_FORECAST)
      CALL COAL_CONTRACTS_MAKEBIN(COAL_CONTRACTS)
      CALL COAL_ESCALATIONS_MAKEBIN(COAL_MODEL_POINTER_FILE)
      CALL COAL_SO2_INFO_MAKEBIN(COAL_MODEL_SO2_INFO_FILE)        ! 373
      CALL COAL_TRANSPORT_LINKS_MAKEBIN(
     +                           COAL_MODEL_GENERIC_TRANSPORT_FILE,'ZD')  ! 374
      CALL GENERIC_DEMAND_MAKEBIN(COAL_MODEL_GENERIC_DEMAND_FILE)  ! 375
      CALL PP_MAKEBIN ! THIS MUST STAY IN FRONT OF CL_MAKEBIN
!
! CPL CONTRACT 
!
      CALL FV_MAKEBIN
      CALL OPEN_ASSET_VECTOR_FILE(82)
      CALL CPL_MAKEBIN
      IF(WVPA()) THEN
         CALL WVPA_RATE_STRUCTURES_MAKEBIN
         CALL WVPA_SALES_MAKEBIN
      ELSE
         CALL CLASS_SALES_MAKEBIN
      ENDIF
      CALL MANAGE_ASSET_ALLOCATIONS(.FALSE.)
      CALL MR_MAKEBIN()
!
      IF(UTILITY_TYPE() == 'P') THEN
         CALL MUNI_PF_MAKEBIN   
!
! INITIALIZATION FILE
!
         CALL MUNI_IN_MAKEBIN
      ELSE
         CALL PF_MAKEBIN   
!
! INITIALIZATION FILE
!
         CALL IN_MAKEBIN
         CALL ADDENDUMS_MAKEBIN
      ENDIF
      CALL DISPLAY_TIME
!
!
! CONVERT MONTHLY PAYABLES FILE
      CALL MONTHLY_PAYABLES_MAKEBIN
!
      CALL AR_MAKEBIN
!
!
! CONVERT FUTURE ASSET COMMITTED FILE TO BINARY
!
      CALL FA_MAKEBIN
!
! CONVERT GENERATING UNIT PARAMETER FILE TO BINARY AND RANDOM ACCESS
! NOTE THIS MUST BE BEFORE THE CL_MAKEBIN ROUTINE
      CALL UP_MAKEBIN
      CALL OPEN_VAR_UNITS_FILE
!
! CONVERT THE FOSSIL FILE
!
      CALL CL_MAKEBIN
!
! CONVERT FUTURE ASSET EXPANSION FILE TO BINARY
!
      CALL FN_MAKEBIN
!
! CONVERT THE EXPANSION PLANT FILE TO BINARY
!
      CALL PN_MAKEBIN
!
! CONVERSION OF THE EXPANSION CANIDATE FILE
!
      CALL CN_MAKEBIN
!
! CONVERSION OF THE ENERGY LIMITED RESOURECE FILE
!
      CALL EL_MAKEBIN
      CALL DISPLAY_TIME
!
!  CONVERT DEBT FILE TO BINARY BASE CASE FORM
!
      CALL DB_MAKEBIN
!
!  EXISTING ASSET FILE CONVERSION
!
      CALL EA_MAKEBIN
!
!  CONVERT DEBIT FILES
!
      CALL DD_MAKEBIN
!
! CONVERT EXPENSE FILE
!
      CALL EX_MAKEBIN
!
! CONVERT NUCLEAR FUEL FILE TO BINARY
!
      CALL NF_MAKEBIN
!
! CONVERT TAX LOSS FILE TO BINARY
!
      CALL TX_MAKEBIN
      CALL DISPLAY_TIME
!
!***********************************************************************
!
! MAKEBIN2 SECTION
!
!
! CONVERT THE LOAD MANAGEMENT APPLICATION FILE
!
      CALL LG_MAKEBIN
!
! CONVERT THE LOAD MANAGEMENT DEVICE FILE
!
      CALL LS_MAKEBIN
!
! CONVERT LOAD MANAGEMENT FINANCIAL FILE TO BINARY
!
      CALL LF_MAKEBIN
!
! OPEN THE ECONOMY INTERCHANGE DATA FILE
!
      CALL EC_MAKEBIN
!
! OPEN THE ESCALATION RATE DATA FILE
!
      CALL ES_MAKEBIN
!
! OPEN THE SYSTEM FORECAST DATA FILE
!
      IF(SYSTEM_BASED_FORECAST()) THEN
         CALL SF_MAKEBIN
      ELSEIF(.NOT. NERC_REGION_BASED_FORECAST()) THEN
         CALL CF_MAKEBIN
         IF(CLASS_BASED_FORECAST()) THEN
            CALL CH_MAKEBIN
            CALL CS_MAKEBIN
         ENDIF
      ENDIF
      CALL DISPLAY_TIME
! ENDIF SYSTEM BASED FORECAST
!
!***********************************************************************
!
! OLD MAKEBIN3 SECTION
!
! CONVERT COST ALLOCATION FILE TO BINARY
!
      CALL CO_MAKEBIN
!
! CONVERT ELASTICITY FILE TO BINARY
!
      CALL FP_MAKEBIN
!
! FINANICAL RUN SPECS 
!
!     CALL RS_MAKEBIN
      CALL CLASS_RUN_SWITCHES_MAKEBIN
      CALL EM_MAKEBIN
!
! DETAILED REPORTS 
!
      CALL DE_MAKEBIN
      CALL DISPLAY_TIME
!
! CATAWBA CONTRACT 
!
      CALL CAT2_MAKEBIN
!
! RDI POWERDAT
!
      CALL RDI_MAKEBIN
!
! RDI BASE CASE
!
      CALL RBC_MAKEBIN
!
! TRANSACTION SYSTEM FORECAST
!
      CALL TF_MAKEBIN
!
! TRANSACTION GROUPS
!
      CALL TG_MAKEBIN
!
! TRANSACTION CONSTRAINT 4/11/98. GAT.
!
      CALL CONSTRAINT_MAKEBIN
!
! TRANSACTION PATH 4/1/98. GAT.
!
      CALL PATH_MAKEBIN
!
! TRANSACT TIE 
!
      CALL TIE_MAKEBIN
      CALL DISPLAY_TIME
!
! DAY TYPE 
!
      CALL DAY_TYPE_MAKEBIN
!
! USER DAY  
!
      CALL USER_DAY_MAKEBIN
!
! ICAP  
!
      CALL ICAP_MAKEBIN
!
! UNIT OUTAGES 
!
      CALL OUTAGE_MAKEBIN
!
! SCENARIO MAKER
!
      CALL SCENARIO_MAKER_MAKEBIN
!
! REGIONAL OUTAGES
!      
!      CALL REGIONAL_OUTAGES_MAKEBIN
!
! REGIONAL PARAMETER
!      
!      CALL REGIONAL_PARAMETER_MAKEBIN
!
! REGIONAL OUTAGE PARAMETER
!      
      CALL REG_OUT_PARAM_MAKEBIN
!
! FUEL DERIVATIVES
!      
      CALL FUEL_DERIVATIVES_MAKEBIN
!
! ENERGY PRODUCTS
!
      CALL ENERGY_PRODUCTS_MAKEBIN
!
! MARKET PRICE
!
      CALL MARKET_PRICE_MAKEBIN
      CALL DISPLAY_TIME
!
! NEW MARKET PRICE
!
      CALL NEW_MARKET_PRICE_MAKEBIN
!
! LH_GLOBAL
!
      CALL LH_GLOBAL_MAKEBIN
!
! ELECT_HOUR_MULT
!
      CALL ELECT_HOUR_MULT_MAKEBIN
!
! GAS_HOUR_MULT
!
      CALL GAS_HOUR_MULT_MAKEBIN
!
! OIL_HOUR_MULT
!
      CALL OIL_HOUR_MULT_MAKEBIN
!
! SCENARIO HOURLY
!
      CALL SCENARIO_HOURLY_MAKEBIN
!
! WEEKLY HYDRO
!
      CALL WEEKLY_HYDRO_MAKEBIN
!
! RPS REQUIREMENTS
!
      CALL RPS_REQUIREMENTS_MAKEBIN
!
! RPS STATE DEMAND
!
      CALL RPS_STATE_DEMAND_MAKEBIN
!
! RETROFIT REQUIREMENTS
!
      CALL THERMAL_RETROFIT_MAKEBIN
!
! CO2 NATIONAL COMPLIANCE
!
      CALL CO2_PARAM_MAKEBIN
!
! WEATHER DEMAND
!
      CALL WEATHER_DEMAND_MAKEBIN
!
! MARKET_GROUPS
!
      CALL MK_MAKEBIN
!
! REFERENCE LOAD
!
      CALL REFERENCE_LOAD_MAKEBIN
!
! CAPACITY TARGET RATIOS
!
      CALL CR_MAKEBIN
!
! CONVERT ENVIRONMENTAL FILE TO BINARY
!
      CALL EN_MAKEBIN
!
! CONVERT PURCHASE CONTRACTS FILE TO BINARY AND RANDOM ACCESS
!
      CALL CT_MAKEBIN
      CALL DISPLAY_TIME
!
      CALL TRAN_EXP_MAKEBIN
!
!
! CONVERT GAS MODEL FILES
!
! 071406
!
      CALL GAS_NODE_MAKEBIN
!
! 090907
!
      CALL GAS_STORAGE_MAKEBIN
!
! 070906
!
      CALL GAS_LINK_MAKEBIN
!      
! 050506
!
      CALL GAS_DEMAND_FORECAST_MAKEBIN
!
! 070806
!      
      CALL GAS_SUPPLY_FORECAST_MAKEBIN
!
! 072006
!
      CALL GAS_WEATHER_DEMAND_MAKEBIN
!
!
!***********************************************************************
!
!
      CALL AA_MAKEBIN
!
! CONVERT FUEL INVENTORY FILE
!
      CALL FI_MAKEBIN
!
! CONVERT FUEL PRICE FILE
!
      CALL FC_MAKEBIN
!
! CONVERT TRANSACTIONS FILE
!
      CALL TR_MAKEBIN
!
! CONVERT FUEL EMISSIONS FILE
!
      CALL FE_MAKEBIN
!
!***********************************************************************
!
      CALL CLOSE_ASSET_VECTOR_FILE
      CALL DISPLAY_TIME
      CALL MG_CLEAR_LINE(17,9,36,0,0)
      CALL MG_CLEAR_LINE(16,9,36,0,0)
      RETURN
      END   
!***********************************************************************
      SUBROUTINE MAKE_CAPEX_BINARY
!***********************************************************************
!
      use SpinDriftLib
      use prod_arrays_dimensions
      USE SIZECOM
      CHARACTER*1 UTILITY_TYPE
      LOGICAL*1 SYSTEM_BASED_FORECAST,
     +          CLASS_BASED_FORECAST,
     +          NERC_REGION_BASED_FORECAST,
     +          WVPA
!
!      CALL LOCATE(16,9)
!      WRITE(6,"('&',A)") 'Input file overhead'
      CALL MG_LOCATE_WRITE(16,9,'Input file overhead',ALL_VERSIONS,1)
!***********************************************************************
!
!
! THESE MAKEBINs AND DYNAMIC VECTOR ALLOCATIONS FOR CP&L MUST BE RUN BEFORE
!  THE ASSET VECTOR ROUTINES 4/7/99 MSG
!
! CPL CONTRACT 
!
      CALL FV_MAKEBIN
      CALL OPEN_ASSET_VECTOR_FILE(82)
      CALL PP_MAKEBIN ! THIS MUST STAY IN FRONT OF CL_MAKEBIN
      IF(UTILITY_TYPE() == 'P') THEN
!         CALL MUNI_PF_MAKEBIN
!
! INITIALIZATION FILE
!
         CALL MUNI_IN_MAKEBIN
      ELSE
!         CALL PF_MAKEBIN
!
! INITIALIZATION FILE
!
         CALL IN_MAKEBIN
      ENDIF
      CALL DISPLAY_TIME
!
! CONVERT GENERATING UNIT PARAMETER FILE TO BINARY AND RANDOM ACCESS
! NOTE THIS MUST BE BEFORE THE CL_MAKEBIN ROUTINE
      CALL UP_MAKEBIN
      CALL OPEN_VAR_UNITS_FILE
!
! CONVERT THE FOSSIL FILE
!
      CALL CL_MAKEBIN
!
! CONVERSION OF THE ENERGY LIMITED RESOURECE FILE
!
!      CALL EL_MAKEBIN
      CALL DISPLAY_TIME
!
! CONVERT NUCLEAR FUEL FILE TO BINARY
!
!      CALL NF_MAKEBIN
!
!***********************************************************************
!
! MAKEBIN2 SECTION
!
!
! CONVERT THE LOAD MANAGEMENT APPLICATION FILE
!
!      CALL LG_MAKEBIN
!
! CONVERT THE LOAD MANAGEMENT DEVICE FILE
!
!      CALL LS_MAKEBIN
!
! CONVERT LOAD MANAGEMENT FINANCIAL FILE TO BINARY
!
!      CALL LF_MAKEBIN
!
! OPEN THE ESCALATION RATE DATA FILE
!
      CALL ES_MAKEBIN
!
! OPEN THE SYSTEM FORECAST DATA FILE
!
      IF(SYSTEM_BASED_FORECAST()) THEN
         CALL SF_MAKEBIN
      ELSEIF(.NOT. NERC_REGION_BASED_FORECAST()) THEN
         CALL CF_MAKEBIN
         IF(CLASS_BASED_FORECAST()) THEN
            CALL CH_MAKEBIN
            CALL CS_MAKEBIN
         ENDIF
      ENDIF !If System based forecast
      CALL DISPLAY_TIME

!
!***********************************************************************
!
! OLD MAKEBIN3 SECTION
!
! CONVERT COST ALLOCATION FILE TO BINARY
!
      CALL CO_MAKEBIN
!
! CONVERT ELASTICITY FILE TO BINARY
!
      CALL FP_MAKEBIN
!
! FINANICAL RUN SPECS 
!
!     CALL RS_MAKEBIN
      CALL CLASS_RUN_SWITCHES_MAKEBIN
      CALL EM_MAKEBIN
!
! DETAILED REPORTS 
!
      CALL DE_MAKEBIN
      CALL DISPLAY_TIME
!
! TRANSACTION SYSTEM FORECAST
!
      CALL TF_MAKEBIN
!
! TRANSACTION GROUPS
!
      CALL TG_MAKEBIN
!
! TRANSACTION CONSTRAINT 4/11/98. GAT.
!
      CALL CONSTRAINT_MAKEBIN
!
! TRANSACTION PATH 4/1/98. GAT.
!
      CALL PATH_MAKEBIN
!
! TRANSACT TIE 
!
      CALL TIE_MAKEBIN
      CALL DISPLAY_TIME
!
! DAY TYPE 
!
      CALL DAY_TYPE_MAKEBIN
!
! USER DAY  
!
      CALL USER_DAY_MAKEBIN
!
! ICAP  
!
      CALL ICAP_MAKEBIN
!
! UNIT OUTAGES 
!
      CALL OUTAGE_MAKEBIN
!
! SCENARIO MAKER
!
      CALL SCENARIO_MAKER_MAKEBIN
!
! REGIONAL OUTAGE PARAMETER
!      
      CALL REG_OUT_PARAM_MAKEBIN
!
! FUEL DERIVATIVES
!      
      CALL FUEL_DERIVATIVES_MAKEBIN
!
! ENERGY PRODUCTS
!
      CALL ENERGY_PRODUCTS_MAKEBIN
!
! MARKET PRICE
!
      CALL MARKET_PRICE_MAKEBIN
      CALL DISPLAY_TIME
!
! NEW MARKET PRICE
!
      CALL NEW_MARKET_PRICE_MAKEBIN
!
! LH_GLOBAL
!
      CALL LH_GLOBAL_MAKEBIN
!
! ELECT_HOUR_MULT
!
      CALL ELECT_HOUR_MULT_MAKEBIN
!
! GAS_HOUR_MULT
!
      CALL GAS_HOUR_MULT_MAKEBIN
!
! OIL_HOUR_MULT
!
      CALL OIL_HOUR_MULT_MAKEBIN
!
! SCENARIO HOURLY
!
      CALL SCENARIO_HOURLY_MAKEBIN
!
! WEEKLY HYDRO
!
      CALL WEEKLY_HYDRO_MAKEBIN
!
! WEATHER DEMAND
!
      CALL WEATHER_DEMAND_MAKEBIN
!
! MARKET_GROUPS
!
      CALL MK_MAKEBIN
!
! REFERENCE LOAD
!
      CALL REFERENCE_LOAD_MAKEBIN
!
! CAPACITY TARGET RATIOS
!
      CALL CR_MAKEBIN
!
! CONVERT ENVIRONMENTAL FILE TO BINARY
!
      CALL EN_MAKEBIN
!
! CONVERT PURCHASE CONTRACTS FILE TO BINARY AND RANDOM ACCESS
!
      CALL CT_MAKEBIN
      CALL DISPLAY_TIME
!
!***********************************************************************
!
!
      CALL AA_MAKEBIN
!
! CONVERT FUEL INVENTORY FILE
!
      CALL FI_MAKEBIN
!
! CONVERT FUEL PRICE FILE
!
      CALL FC_MAKEBIN
!
! CONVERT TRANSACTIONS FILE
!
      CALL TR_MAKEBIN
!
! CONVERT FUEL EMISSIONS FILE
!
      CALL FE_MAKEBIN
!
!***********************************************************************
!
      CALL CLOSE_ASSET_VECTOR_FILE
      CALL DISPLAY_TIME
      CALL MG_CLEAR_LINE(17,9,36,0,0)
      CALL MG_CLEAR_LINE(16,9,36,0,0)
      RETURN
      END   
! *********************************************************************
      SUBROUTINE DELETE_RESULTS_OF_PREVIOUS_RUN() 
! *********************************************************************      
!
      use cl_data
      implicit none
      CHARACTER*256 BASE_FILE_DIRECTORY,FILE_NAME,GET_RESULTS_DIRECTORY
      LOGICAL FILE_EXISTS
      logical*1 LAHEY_LF95,WVPA

         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"BC*.BIN"
         CALL ERASEWC(FILE_NAME)
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"OL*.BIN"
         CALL ERASEWC(FILE_NAME)

! ERASE ALL DETAILED REPORT FILES
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                                     trim(cldata%SCENAME)//".??D"
         CALL ERASEWC(FILE_NAME)

            FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"WVP"//
     +                                      trim(cldata%SCENAME)//".??D"
            CALL ERASEWC(FILE_NAME)
!
!
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//'MSG'//
     +                                      trim(cldata%SCENAME)//'.STR'
         INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
         IF(FILE_EXISTS) CALL ERASE(FILE_NAME)
!
      RETURN
      END SUBROUTINE DELETE_RESULTS_OF_PREVIOUS_RUN
!***********************************************************************
       SUBROUTINE SET_ASSET_CLASSES(R_ASSET_CLASS_NUM,
     +                             NUMBER_OF_ASSET_CLASSES,
     +                              MAX_CLASS_ID_NUM,
     +                             TEMP_ASSET_CLASS_POINTER)
      use end_routine, only: end_program, er_message
!***********************************************************************
! USE TO DETERMINE THE NUMBER OF DEFINED ASSET CLASS IN EACH DATA
! TYPE.  CALLED FROM MAKEBIN AND MAKEOVLS 11/30/94
!
      use SpinDriftLib
      use prod_arrays_dimensions
      USE SIZECOM

      INTEGER*2 ASSET_CLASS_NUM,PARENT_CLASS_ID_NUM,
     +          NUMBER_OF_ASSET_CLASSES,
     +           MAX_CLASS_ID_NUM,
     +          TEMP_ASSET_CLASS_POINTER(*),
     +          R_ASSET_CLASS_NUM,VECTOR_PTR
!
      REAL ASSET_CLASS_LIST(1024)
      REAL*4 VECTOR_ASSET_CLASSES(0:AVAIL_DATA_YEARS)
      REAL*4 VECTOR_CLASSES(AVAIL_DATA_YEARS)
      INTEGER*2 I,ASSET_CLASS
      INTEGER*2 VECTOR_POINTER,CLASS_POINTER,LINKED_CLASS
      CHARACTER*1 DUMMY_TYPE
!
      INTEGER*2 R_ALLOCATION_VECTOR,
     +          R_ASSET_CLASS,
     +          R_CLASS_ID_LIST(*)
      REAL*4 R_ASSET_ALLOCATION_LIST(*)      
!
         VECTOR_ASSET_CLASSES = 0
         IF(R_ASSET_CLASS_NUM >= 0) THEN
            VECTOR_ASSET_CLASSES(1) = R_ASSET_CLASS_NUM
         ELSEIF(R_ASSET_CLASS_NUM < 0) THEN
            VECTOR_CLASSES = 0 
            ASSET_CLASS_NUM = ABS(R_ASSET_CLASS_NUM)
            CALL GET_ASSET_VAR(ASSET_CLASS_NUM,DUMMY_TYPE,
     +                                                   VECTOR_CLASSES)
            VECTOR_ASSET_CLASSES(1:) = VECTOR_CLASSES
         ENDIF
!
         VECTOR_POINTER = 1
         IF(NUMBER_OF_ASSET_CLASSES == 0) THEN
            VECTOR_POINTER = 0
            CALL PARENT_CLASS_ID(PARENT_CLASS_ID_NUM)
            VECTOR_ASSET_CLASSES(0) = PARENT_CLASS_ID_NUM ! - 1
         ENDIF
         CLASS_POINTER = 1
         DO
            ASSET_CLASS = VECTOR_ASSET_CLASSES(VECTOR_POINTER)
            ASSET_CLASS_LIST(CLASS_POINTER) = ASSET_CLASS
            DO
               CLASS_POINTER = CLASS_POINTER + 1
               CALL GET_LINKED_CLASS(ASSET_CLASS,LINKED_CLASS)
               IF(LINKED_CLASS < 0 .OR. CLASS_POINTER > 1024) EXIT
               ASSET_CLASS_LIST(CLASS_POINTER) = LINKED_CLASS
               ASSET_CLASS = LINKED_CLASS
            ENDDO
            VECTOR_POINTER = VECTOR_POINTER + 1
            IF(VECTOR_POINTER > AVAIL_DATA_YEARS) EXIT
            IF(VECTOR_POINTER == 1) CYCLE
            IF(VECTOR_ASSET_CLASSES(VECTOR_POINTER) == 0. .OR. 
     +                                        CLASS_POINTER > 1024) EXIT
         ENDDO
         IF(CLASS_POINTER <= 1024) THEN
            ASSET_CLASS_LIST(CLASS_POINTER) = -99.
         ENDIF
!
         I = 1
         DO
            ASSET_CLASS = ASSET_CLASS_LIST(I) + 1
            IF(ASSET_CLASS > 0) THEN
! 081808. FOR WPS AND DIANE.            
               IF(ASSET_CLASS < 1 .OR. ASSET_CLASS > 1024) THEN
                  WRITE(4,*) 'IN THE ASSET CLASS LOGIC'
                  WRITE(4,*) 'THE MAXIMUM NUMBER OF CLASSES IS'
                  WRITE(4,*) 'GREATER THAN 1024'
                  er_message='Stop requested from MIDASMOD SIID199'
                  call end_program(er_message)
               ELSE
                  IF(TEMP_ASSET_CLASS_POINTER(ASSET_CLASS) == 0) THEN
                     NUMBER_OF_ASSET_CLASSES = 
     +                                       NUMBER_OF_ASSET_CLASSES + 1
                     TEMP_ASSET_CLASS_POINTER(ASSET_CLASS) =
     +                                           NUMBER_OF_ASSET_CLASSES
                     MAX_CLASS_ID_NUM = 
     +                                 MAX(MAX_CLASS_ID_NUM,ASSET_CLASS)
                  ENDIF
               ENDIF
            ENDIF
            I = I + 1
            IF(I > 1024) EXIT
            IF(ASSET_CLASS_LIST(I) == -99.) EXIT
         ENDDO
      RETURN
!***********************************************************************
      ENTRY CREATE_CLASS_ID_ALLOCATION_LIST(R_ASSET_CLASS,
     +                                      R_ALLOCATION_VECTOR,
     +                                      R_CLASS_ID_LIST,
     +                                      R_ASSET_ALLOCATION_LIST)      
!***********************************************************************
!
         IF(R_ASSET_CLASS < 0.) THEN
            VECTOR_PTR = ABS(R_ASSET_CLASS)
            CALL GET_ASSET_VAR(VECTOR_PTR,DUMMY_TYPE,R_CLASS_ID_LIST)
            VECTOR_PTR = ABS(R_ALLOCATION_VECTOR)
            CALL GET_ASSET_VAR(VECTOR_PTR,
     +                               DUMMY_TYPE,R_ASSET_ALLOCATION_LIST)
         ELSE
            R_CLASS_ID_LIST(1) = R_ASSET_CLASS
            R_CLASS_ID_LIST(2) = 0
            IF(R_ALLOCATION_VECTOR < 0) THEN
               R_ASSET_ALLOCATION_LIST(1) =
     +                                   FLOAT(ABS(R_ALLOCATION_VECTOR))
            ELSEIF(R_ALLOCATION_VECTOR > 0) THEN
               R_ASSET_ALLOCATION_LIST(1) = -FLOAT(R_ALLOCATION_VECTOR)
            ELSE
               R_ASSET_ALLOCATION_LIST(1) = 100.
            ENDIF
            R_ASSET_ALLOCATION_LIST(2) = 0.
         ENDIF
!
      RETURN
       END
! *********************************************************************      
       SUBROUTINE SET_INIT_CLASSES(R_ASSET_CLASS_NUM,
     +                             NUMBER_OF_ASSET_CLASSES,
     +                              MAX_CLASS_ID_NUM,
     +                             TEMP_ASSET_CLASS_POINTER)
! *********************************************************************      
! USE TO DETERMINE THE NUMBER OF DEFINED ASSET CLASS IN EACH DATA
! TYPE.  CALLED FROM MAKEBIN AND MAKEOVLS 11/30/94
!
      use SpinDriftLib
      use prod_arrays_dimensions
      USE SIZECOM

      INTEGER*2 ASSET_CLASS_NUM,R_ASSET_CLASS_NUM,
     +          NUMBER_OF_ASSET_CLASSES,
     +           MAX_CLASS_ID_NUM,
     +          TEMP_ASSET_CLASS_POINTER(*)
!
      REAL ASSET_CLASS_LIST(1024)
      REAL*4 VECTOR_ASSET_CLASSES(0:AVAIL_DATA_YEARS),
     +       VECTOR_CLASSES(AVAIL_DATA_YEARS)
      INTEGER*2 I,ASSET_CLASS
      INTEGER*2 VECTOR_POINTER,CLASS_POINTER,LINKED_CLASS
      CHARACTER*1 DUMMY_TYPE
!
         VECTOR_ASSET_CLASSES = 0.
         IF(R_ASSET_CLASS_NUM >= 0) THEN
            VECTOR_ASSET_CLASSES(1) = R_ASSET_CLASS_NUM
         ELSEIF(R_ASSET_CLASS_NUM < 0) THEN
            VECTOR_CLASSES = 0 
            ASSET_CLASS_NUM = ABS(R_ASSET_CLASS_NUM)
            CALL GET_ASSET_VAR(ASSET_CLASS_NUM,DUMMY_TYPE,
     +                                                   VECTOR_CLASSES)
            VECTOR_ASSET_CLASSES(1:) = VECTOR_CLASSES
         ENDIF
!
         VECTOR_POINTER = 1
         CLASS_POINTER = 1
         DO
            ASSET_CLASS = VECTOR_ASSET_CLASSES(VECTOR_POINTER)
            ASSET_CLASS_LIST(CLASS_POINTER) = ASSET_CLASS
            DO
               CLASS_POINTER = CLASS_POINTER + 1
               CALL GET_LINKED_CLASS(ASSET_CLASS,LINKED_CLASS)
               IF(LINKED_CLASS < 0 .OR. CLASS_POINTER > 1024) EXIT
               ASSET_CLASS_LIST(CLASS_POINTER) = LINKED_CLASS
               ASSET_CLASS = LINKED_CLASS
            ENDDO
            VECTOR_POINTER = VECTOR_POINTER + 1
            IF(VECTOR_POINTER > AVAIL_DATA_YEARS) EXIT
            IF(VECTOR_ASSET_CLASSES(VECTOR_POINTER) == 0. .OR. 
     +                                        CLASS_POINTER > 1024) EXIT
         ENDDO
         IF(CLASS_POINTER <= 1024) THEN
            ASSET_CLASS_LIST(CLASS_POINTER) = -99.
         ENDIF
!
         I = 1
         DO
            ASSET_CLASS = ASSET_CLASS_LIST(I) + 1
            IF(ASSET_CLASS > 0) THEN
               IF(TEMP_ASSET_CLASS_POINTER(ASSET_CLASS) == 0) THEN
                  NUMBER_OF_ASSET_CLASSES = NUMBER_OF_ASSET_CLASSES + 1
                  TEMP_ASSET_CLASS_POINTER(ASSET_CLASS) =
     +                                           NUMBER_OF_ASSET_CLASSES
                  MAX_CLASS_ID_NUM = MAX(MAX_CLASS_ID_NUM,ASSET_CLASS)
               ENDIF
            ENDIF
            I = I + 1
            IF(I > 1024) EXIT
            IF(ASSET_CLASS_LIST(I) == -99.) EXIT
         ENDDO

       END SUBROUTINE SET_INIT_CLASSES
! *********************************************************************      
      FUNCTION INIT_CREATE_CLASS_LIST(INIT_TABLE_POINTER)
! *********************************************************************      
!
      use SpinDriftLib
      use prod_arrays_dimensions
      USE SIZECOM

      CHARACTER*1 DUMMY_TYPE
      LOGICAL*1 INIT_CREATE_CLASS_LIST
      INTEGER*2 CREATE_CLASS_LIST,VECTOR_PTR
      INTEGER*2 CLASS_NUM,R_CLASS_NUM,I,MAX_CLASS_IN_TABLE,
     +          PASS_TABLE_POINTER,INIT_TABLE_POINTER,
     +          INCREMENT_TABLE_NUMBER
      REAL ASSET_CLASS_LIST(:)
      INTEGER*2 TABLE_POSITION_FOR(:),TABLE_NUMBER,MAX_CLASS_NUMBER,
     +          R_TABLE_POSITION_FOR(0:*),SAVE_INIT_TABLE_POINTER
      SAVE TABLE_POSITION_FOR,TABLE_NUMBER,MAX_CLASS_NUMBER,
     +     ASSET_CLASS_LIST,SAVE_INIT_TABLE_POINTER
      ALLOCATABLE :: TABLE_POSITION_FOR,ASSET_CLASS_LIST
!
         ALLOCATE(TABLE_POSITION_FOR(1024),
     +            ASSET_CLASS_LIST(AVAIL_DATA_YEARS))
         TABLE_POSITION_FOR = INIT_TABLE_POINTER
         TABLE_NUMBER = 0
         MAX_CLASS_NUMBER = 0
         SAVE_INIT_TABLE_POINTER = INIT_TABLE_POINTER
         INIT_CREATE_CLASS_LIST = .TRUE.
      RETURN
!         
! *********************************************************************      
      ENTRY CREATE_CLASS_LIST(R_CLASS_NUM)
! *********************************************************************      
         TABLE_NUMBER = TABLE_NUMBER + 1
         CREATE_CLASS_LIST = TABLE_NUMBER
         IF(R_CLASS_NUM < 0) THEN
            VECTOR_PTR = ABS(R_CLASS_NUM)
            CALL GET_ASSET_VAR(VECTOR_PTR,DUMMY_TYPE,ASSET_CLASS_LIST)
         ELSE
            ASSET_CLASS_LIST(1) = R_CLASS_NUM
            ASSET_CLASS_LIST(2) = 0.
         ENDIF
         I = 1
         DO
            CLASS_NUM = INT2(ASSET_CLASS_LIST(I)) + 1
            TABLE_POSITION_FOR(CLASS_NUM) = TABLE_NUMBER
            MAX_CLASS_NUMBER = MAX(MAX_CLASS_NUMBER,CLASS_NUM)
            I = I + 1
            IF(I > AVAIL_DATA_YEARS) EXIT
            IF(ASSET_CLASS_LIST(I) == 0.) EXIT
         ENDDO
      RETURN
! *********************************************************************      
      ENTRY INCREMENT_TABLE_NUMBER
! *********************************************************************      
         TABLE_NUMBER = TABLE_NUMBER + 1
         INCREMENT_TABLE_NUMBER = TABLE_NUMBER
      RETURN
! *********************************************************************      
      ENTRY MAX_CLASS_IN_TABLE
! *********************************************************************      
         MAX_CLASS_IN_TABLE = MAX_CLASS_NUMBER
      RETURN
! *********************************************************************      
      ENTRY PASS_TABLE_POINTER(R_TABLE_POSITION_FOR)
! *********************************************************************      
         R_TABLE_POSITION_FOR(0) = SAVE_INIT_TABLE_POINTER
         DO I = 1, MAX_CLASS_NUMBER
            R_TABLE_POSITION_FOR(I) = TABLE_POSITION_FOR(I)   
         ENDDO
         DEALLOCATE(ASSET_CLASS_LIST,TABLE_POSITION_FOR)
         PASS_TABLE_POINTER = TABLE_NUMBER
      RETURN   
      END
!***********************************************************************
!
! ROUTINE WILL THE MESSAGES IN IT 
!
!***********************************************************************
      SUBROUTINE ERROR_AND_WARNING_MSG
      use end_routine, only: end_program, er_message
      use SpinDriftLib
      use prod_arrays_dimensions
      USE SIZECOM

      CHARACTER*(*) FILE_TYPE,FILE_NAME
      
!
!***********************************************************************
      ENTRY STOP_NOFILE(FILE_TYPE,FILE_NAME)
!***********************************************************************

         CALL MG_LOCATE_WRITE(20,0,'The '//trim(FILE_TYPE)//
     +             ' base file '//trim(FILE_NAME)//' does not exist.',
     +                                                   ALL_VERSIONS,1)
      er_message= trim(file_name) // ' missing at MIDASMOD SIID200 '
      call end_program(er_message)
      END
! *********************************************************************      
      FUNCTION LOAD_FILE_CHAR_EXT(INT_EXTENSION)
! *********************************************************************      
!
      CHARACTER*2 LOAD_FILE_CHAR_EXT,TEMP_CHR
      INTEGER*2 INT_EXTENSION
         IF(INT_EXTENSION < 100) THEN
            TEMP_CHR(1:1) = CHAR(48 + INT(FLOAT(INT_EXTENSION)/10.))
         ELSE
            TEMP_CHR(1:1) = CHAR(48 + INT(FLOAT(INT_EXTENSION-100)/10.))
         ENDIF
         TEMP_CHR(2:2) = CHAR(48 + MOD(INT_EXTENSION,10))
         LOAD_FILE_CHAR_EXT = TEMP_CHR
      RETURN
      END
! *********************************************************************      
      FUNCTION AREA_CHAR_EXT(INT_EXTENSION)
! *********************************************************************      
!
      CHARACTER*3 AREA_CHAR_EXT,TEMP_CHR
      INTEGER*2 INT_EXTENSION
         IF(INT_EXTENSION < 100) THEN
            TEMP_CHR(1:1) = CHAR(48 + INT(FLOAT(INT_EXTENSION)/10.))
         ELSE
            TEMP_CHR(1:1) = CHAR(48 + INT(FLOAT(INT_EXTENSION-100)/10.))
         ENDIF
         TEMP_CHR(2:2) = CHAR(48 + MOD(INT_EXTENSION,10))
         AREA_CHAR_EXT = TEMP_CHR
      RETURN
      END
! *********************************************************************      
      FUNCTION SCEN_FILE_CHAR_EXT(R_INT_EXTENSION)
! *********************************************************************      
!
      CHARACTER*3 SCEN_FILE_CHAR_EXT,TEMP_CHR
      INTEGER*2   R_INT_EXTENSION,
     +            FIRST_POSITION,
     +            SECOND_POSITION,
     +            THIRD_POSITION,
     +            TEMP_EXTENSION
!
         TEMP_EXTENSION = R_INT_EXTENSION
!
         FIRST_POSITION = INT(TEMP_EXTENSION/100.)
         SECOND_POSITION =  
     +                     INT((TEMP_EXTENSION-FIRST_POSITION*100)/10.)
         THIRD_POSITION = 
     +             TEMP_EXTENSION-FIRST_POSITION*100-SECOND_POSITION*10
!     
         TEMP_CHR(1:1) = CHAR(48 + FIRST_POSITION)
         TEMP_CHR(2:2) = CHAR(48 + SECOND_POSITION)
         TEMP_CHR(3:3) = CHAR(48 + THIRD_POSITION)
! TEMP         
         SCEN_FILE_CHAR_EXT = TEMP_CHR
      RETURN
      END
!***********************************************************************
      FUNCTION FINANCIAL_DRILLING_HEADER()
!***********************************************************************
      USE DRILLING_REPT_PARAMETERS
      USE PROD_ARRAYS_DIMENSIONS
      USE IREC_ENDPOINT_CONTROL
      use logging
      use dsex_data
      use GRX_PLANNING_ROUTINES
      use dreptcom
      USE SIZECOM
      use globecom

      INCLUDE 'NAMESCOM.MON'
      INCLUDE 'MTHNMCOM.MON'
!
      LOGICAL*1 CPL_ACTIVE
      LOGICAL*1 SET_DRILLING_DATA_INCOME_TRUE,
     +          SET_DRILLING_DATA_TAX_TRUE,
     +          SET_DRILLING_DATA_BALANCE_TRUE,
     +          SET_DRILLING_DATA_CASH_TRUE
      CHARACTER*1 FINANCIAL_DRILLING
      LOGICAL*1 DRILLING_FILE_IS_OPEN/.FALSE./
      INTEGER*2 RUN_YEARS,EXTENSION_YEARS,I,YR
      INTEGER*2 ESTABLISH_DRILLING_REPORT_FILE,
     +          FINANCIAL_DRILLING_HEADER,
     +          RETURN_DRILLING_REPORT_STATUS
      INTEGER*2 OVERHEAD_LENGTH,REPORTING_YEARS,RECORD_LENGTH,
     +          REPORTING_UNIT_NUMBER,
     +          DIMENSIONS,FIRST_YEAR,
     +          FILE_VARIABLES
      CHARACTER*256 FILE_NAME,GET_RESULTS_DIRECTORY, fname_for_output
      CHARACTER*5 GET_SCENAME,FINANCIAL_DRILLING_CODE*4
      SAVE FILE_NAME,OVERHEAD_LENGTH,REPORTING_YEARS,DIMENSIONS,
     +     REPORTING_UNIT_NUMBER,FIRST_YEAR
      INTEGER*2 INCOME_DRILLING_UNIT_NO,
     +          CASH_DRILLING_UNIT_NO,
     +          BALANCE_DRILLING_UNIT_NO,
     +          TAX_DRILLING_UNIT_NO,
     +          MONTHLY_INCOME_UNIT_NO,
     +          MONTHLY_CASH_UNIT_NO,
     +          MONTHLY_BALANCE_UNIT_NO,
     +          MONTHLY_TAX_UNIT_NO,
     +          CPL_TAX_REPORTING_UNIT_NO
      CHARACTER*15 LEFT_JUSTIFY_I2_IN_STR
      CHARACTER*4 YEAR_STR,VAR_NUM_STR
!
      REAL*4 MONTHLY_DATA(0:12,0:*)
      INTEGER*2 DRILLING_REPRT_UNIT,
     +          FINANCIAL_SIMULATION_YEARS
      CHARACTER*6 SHORT_MONTH_NAMES
      INTEGER*2 MO,WRITE_DRILLING_RPT
      REAL*4 OUTPUT_VALUE(0:MAX_FINANCIAL_SIMULATION_YEARS)
      REAL*4 NOT_AVAIL
      PARAMETER(NOT_AVAIL=-999999.)
      CHARACTER (LEN=DrillingAccountNameWidth) :: ACCOUNT_NAME
      CHARACTER*1 DRILLING_REPORT
      CHARACTER*30 DRILLING_NAME
      LOGICAL*1 ANNUAL_REPORT
      INTEGER*2 START_MONTHLY,
     +          END_MONTHLY,
     +          ANN_RPT_MO,
     +          START_YEAR
      INTEGER NEXT_REC,START_REC
      LOGICAL*1 DATA_NOT_IN_CASH_DRILLING/.TRUE./,
     +          DATA_NOT_IN_BALANCE_DRILLING/.TRUE./,
     +          DATA_NOT_IN_INCOME_DRILLING/.TRUE./,
     +          DATA_NOT_IN_TAX_DRILLING/.TRUE./
      LOGICAL*1 R_DATA_NOT_IN_CASH_DRILLING,
     +          R_DATA_NOT_IN_BALANCE_DRILLING,
     +          R_DATA_NOT_IN_INCOME_DRILLING,
     +          R_DATA_NOT_IN_TAX_DRILLING
!
      LOGICAL*1 NEED_CLOSING_BALANCE
      INTEGER MONTHLY_INCOME_REC,MONTHLY_CASH_REC,MONTHLY_BALANCE_REC,
     +        MONTHLY_TAX_REC,MONTHLY_CPL_TAX_REC
      SAVE MONTHLY_INCOME_REC,MONTHLY_CASH_REC,MONTHLY_BALANCE_REC,
     +     MONTHLY_TAX_REC,MONTHLY_CPL_TAX_REC
      INTEGER DRILLING_BALANCE_SHEET_REC,
     +        DRILLING_INCOME_REC,
     +        DRILLING_CASH_REC,
     +        DRILLING_TAX_REC
      SAVE DRILLING_BALANCE_SHEET_REC,
     +     DRILLING_INCOME_REC,
     +     DRILLING_CASH_REC,
     +     DRILLING_TAX_REC
      INTEGER INCOME_DRILLING_REPRT_REC,
     +        CASH_DRILLING_REPRT_REC,
     +        BALANCE_DRILLING_REPRT_REC,
     +        TAX_DRILLING_REPRT_REC
      INTEGER*2 Period Width
      PARAMETER (Period Width=10)
!
         IF(FINANCIAL_DRILLING() == 'B' .OR.
     +                                 FINANCIAL_DRILLING() == 'M') THEN
            REPORTING_YEARS = MAX(5,RUN_YEARS()+EXTENSION_YEARS())
         ELSE
            REPORTING_YEARS = RUN_YEARS() + EXTENSION_YEARS()
         ENDIF
!
! OPEN THE MONTHLY OUTPUT REPORT
!
         IF(MONTHLY_MIDAS_ACTIVE) THEN
            FIRST_YEAR = BASE_YEAR + 1
            DIMENSIONS = 4
            
            FILE_VARIABLES = INCOME_VARS + 1
            OVERHEAD_LENGTH = 4 + 4 + 38 + Period Width
!
! MONTHLY INCOME STATEMENT
!
            REPORTING_UNIT_NUMBER = FINANCIAL_MONTHLY_INCOME_NO
            FINANCIAL_DRILLING_CODE = FINANCIAL_MONTHLY_INCOME_CODE
            FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                    trim(GET_SCENAME())//FINANCIAL_DRILLING_CODE
            RECORD_LENGTH = ESTABLISH_DRILLING_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            FILE_VARIABLES,
     +                                            DIMENSIONS,FIRST_YEAR,
     +                                            REPORTING_UNIT_NUMBER,
     +                                            START_REC)
!
            WRITE(REPORTING_UNIT_NUMBER,REC=START_REC)
     +                        'Endpoint            ','N',INT2(4),'V','D'
            WRITE(REPORTING_UNIT_NUMBER,REC=START_REC+1) 
     +                        'Year                ','N',INT2(4),'F','D'
            WRITE(REPORTING_UNIT_NUMBER,REC=START_REC+2)
     +                       'Asset Class Name    ','C',INT2(38),'F','D'
            WRITE(REPORTING_UNIT_NUMBER,REC=START_REC+3) 
     +                   'Period              ','C',Period Width,'V','D'
            MONTHLY_INCOME_REC = START_REC+4
!
! MONTHLY CASH REPORT
!          
            FILE_VARIABLES = CASH_VARS + 1
            REPORTING_UNIT_NUMBER = FINANCIAL_MONTHLY_CASH_NO
            FINANCIAL_DRILLING_CODE = FINANCIAL_MONTHLY_CASH_CODE
            FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                    trim(GET_SCENAME())//FINANCIAL_DRILLING_CODE
            RECORD_LENGTH = ESTABLISH_DRILLING_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            FILE_VARIABLES,
     +                                            DIMENSIONS,FIRST_YEAR,
     +                                            REPORTING_UNIT_NUMBER,
     +                                            START_REC)
!
            WRITE(REPORTING_UNIT_NUMBER,REC=START_REC)
     +                        'Endpoint            ','N',INT2(4),'V','D'
            WRITE(REPORTING_UNIT_NUMBER,REC=START_REC+1) 
     +                        'Year                ','N',INT2(4),'F','D'
            WRITE(REPORTING_UNIT_NUMBER,REC=START_REC+2)
     +                       'Asset Class Name    ','C',INT2(38),'F','D'
            WRITE(REPORTING_UNIT_NUMBER,REC=START_REC+3) 
     +                   'Period              ','C',Period Width,'V','D'
            MONTHLY_CASH_REC = START_REC+4
!
! MONTHLY BALANCE SHEET
!          
            FILE_VARIABLES = BAL_SHEET_VARS + 1 + 30 
            REPORTING_UNIT_NUMBER = FINANCIAL_MONTHLY_BALANCE_NO
            FINANCIAL_DRILLING_CODE = FINANCIAL_MONTHLY_BALANCE_CODE
            FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                    trim(GET_SCENAME())//FINANCIAL_DRILLING_CODE
            RECORD_LENGTH = ESTABLISH_DRILLING_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            FILE_VARIABLES,
     +                                            DIMENSIONS,FIRST_YEAR,
     +                                            REPORTING_UNIT_NUMBER,
     +                                            START_REC)
!
            WRITE(REPORTING_UNIT_NUMBER,REC=START_REC)
     +                        'Endpoint            ','N',INT2(4),'V','D'
            WRITE(REPORTING_UNIT_NUMBER,REC=START_REC+1) 
     +                        'Year                ','N',INT2(4),'F','D'
            WRITE(REPORTING_UNIT_NUMBER,REC=START_REC+2)
     +                       'Asset Class Name    ','C',INT2(38),'F','D'
            WRITE(REPORTING_UNIT_NUMBER,REC=START_REC+3) 
     +                   'Period              ','C',Period Width,'V','D'
            MONTHLY_BALANCE_REC = START_REC+4
!
! MONTHLY TAX DETAIL REPORT
!
            FILE_VARIABLES = TAX_VARS + 1 + 200 
            REPORTING_UNIT_NUMBER = FINANCIAL_MONTHLY_TAX_NO
            FINANCIAL_DRILLING_CODE = FINANCIAL_MONTHLY_TAX_CODE
            FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                    trim(GET_SCENAME())//FINANCIAL_DRILLING_CODE
            RECORD_LENGTH = ESTABLISH_DRILLING_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            FILE_VARIABLES,
     +                                            DIMENSIONS,FIRST_YEAR,
     +                                            REPORTING_UNIT_NUMBER,
     +                                            START_REC)
!
            WRITE(REPORTING_UNIT_NUMBER,REC=START_REC)
     +                        'Endpoint            ','N',INT2(4),'V','D'
            WRITE(REPORTING_UNIT_NUMBER,REC=START_REC+1) 
     +                        'Year                ','N',INT2(4),'F','D'
            WRITE(REPORTING_UNIT_NUMBER,REC=START_REC+2)
     +                       'Asset Class Name    ','C',INT2(38),'F','D'
            WRITE(REPORTING_UNIT_NUMBER,REC=START_REC+3) 
     +                   'Period              ','C',Period Width,'V','D'
            MONTHLY_TAX_REC = START_REC+4
!
! MONTHLY CP&L TAX REPORT
!     
            FINANCIAL_DRILLING_CODE = CPL_TAX_REPORTING_CODE
            FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"CPL"//
     +                    trim(GET_SCENAME())//FINANCIAL_DRILLING_CODE
!            CALL ERASE(FILE_NAME)
            IF(CPL_ACTIVE()) THEN     
               DIMENSIONS = 5
               FILE_VARIABLES = CPL_TAX_VARS + 1 ! number of variables
               OVERHEAD_LENGTH = 4 + 4 + 38 + 6 + 14
               REPORTING_UNIT_NUMBER = CPL_TAX_REPORTING_NO
               RECORD_LENGTH = ESTABLISH_DRILLING_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            FILE_VARIABLES,
     +                                            DIMENSIONS,FIRST_YEAR,
     +                                            REPORTING_UNIT_NUMBER,
     +                                            START_REC)
!
               WRITE(REPORTING_UNIT_NUMBER,REC=START_REC)
     +                        'Endpoint            ','N',INT2(4),'V','D'
               WRITE(REPORTING_UNIT_NUMBER,REC=START_REC+1) 
     +                        'Year                ','N',INT2(4),'F','D'
               WRITE(REPORTING_UNIT_NUMBER,REC=START_REC+2)
     +                       'Asset Class Name    ','C',INT2(38),'F','D'
               WRITE(REPORTING_UNIT_NUMBER,REC=START_REC+3) 
     +                   'Period              ','C',Period Width,'V','D'
               WRITE(REPORTING_UNIT_NUMBER,REC=START_REC+4) 
     +                       'Op/Non-Op/Total     ','C',INT2(14),'F','D'
               MONTHLY_CPL_TAX_REC = START_REC+5
            ENDIF
         ENDIF 
!
! OPEN THE THREE DRILLING FILES
! 
         FINANCIAL_DRILLING_HEADER = FINANCIAL_INCOME_DRILLING_NO
!        INQUIRE(UNIT=FINANCIAL_DRILLING_NO,OPEN=FILE_IS_OPEN)
         IF(DRILLING_FILE_IS_OPEN .OR.
     +                               FINANCIAL_DRILLING() == 'O') RETURN
!
         OVERHEAD_LENGTH = 4 + 30 + DrillingAccountNameWidth

         DIMENSIONS = 3
         IF(FINANCIAL_DRILLING() == 'B') THEN
            DIMENSIONS = 5
            OVERHEAD_LENGTH = 74 + 30 + 6 + 4
         ELSEIF(FINANCIAL_DRILLING() == 'C' .OR.
     +                                 FINANCIAL_DRILLING() == 'M') THEN
            DIMENSIONS = 4   
            IF(FINANCIAL_DRILLING() == 'C') OVERHEAD_LENGTH = 74 + 30 +4
            IF(FINANCIAL_DRILLING() == 'M') OVERHEAD_LENGTH = 74 + 6 + 4
         ENDIF
         FILE_VARIABLES = 13 * REPORTING_YEARS
!
         DO I = 1, 4
            IF(I == 1) THEN
               REPORTING_UNIT_NUMBER = FINANCIAL_INCOME_DRILLING_NO
               FINANCIAL_DRILLING_CODE = FINANCIAL_INCOME_DRILLING_CODE
               FIRST_YEAR = BASE_YEAR + 1

               fname_for_output='MSG'//trim(cldata%SCENAME)//'.DIN'

               OPEN(10,FILE='MSG'//trim(cldata%SCENAME)//'.DIN',
     +                                                STATUS="REPLACE")
               DO YR = 1, REPORTING_YEARS  ! 1997,V,0,,S,,,,,,,,""
                  YEAR_STR=LEFT_JUSTIFY_I2_IN_STR(INT2(FIRST_YEAR+YR-1)) 
                  VAR_NUM_STR = LEFT_JUSTIFY_I2_IN_STR(INT2(YR-1))
                  WRITE(10,*) YEAR_STR//',V,'//trim(VAR_NUM_STR)//
     +                                                   ',,S,,,,,,,,""'
               ENDDO
               CLOSE(10)
            ELSEIF(I == 2) THEN
               REPORTING_UNIT_NUMBER = FINANCIAL_CASH_DRILLING_NO
               FINANCIAL_DRILLING_CODE = FINANCIAL_CASH_DRILLING_CODE
               FIRST_YEAR = BASE_YEAR + 1
               OPEN(10,FILE='MSG'//trim(cldata%SCENAME)//'.DCH',
     +                                                 STATUS="REPLACE")
               DO YR = 1, REPORTING_YEARS
                  YEAR_STR=LEFT_JUSTIFY_I2_IN_STR(INT2(FIRST_YEAR+YR-1)) 
                  VAR_NUM_STR = LEFT_JUSTIFY_I2_IN_STR(INT2(YR-1)) 
                  WRITE(10,*) YEAR_STR//',V,'//trim(VAR_NUM_STR)//
     +                                                   ',,S,,,,,,,,""'
               ENDDO
               CLOSE(10)
            ELSEIF(I == 3) THEN
               REPORTING_UNIT_NUMBER = FINANCIAL_BALANCE_DRILLING_NO
               FINANCIAL_DRILLING_CODE = FINANCIAL_BALANCE_DRILLING_CODE
               FIRST_YEAR = BASE_YEAR+1
               OPEN(10,FILE='MSG'//trim(cldata%SCENAME)//'.DBL',
     +                                                 STATUS="REPLACE")
               DO YR = 1, REPORTING_YEARS ! +1  ! 1997,V,0,,S,,,,,,,,""
                  YEAR_STR=LEFT_JUSTIFY_I2_IN_STR(INT2(FIRST_YEAR+YR-1))
                  VAR_NUM_STR = LEFT_JUSTIFY_I2_IN_STR(INT2(YR-1))
                  WRITE(10,*) YEAR_STR//',V,'//trim(VAR_NUM_STR)//
     +                                                   ',,S,,,,,,,,""'
               ENDDO
               CLOSE(10)
            ELSEIF(I == 4) THEN
               REPORTING_UNIT_NUMBER = FINANCIAL_TAX_DRILLING_NO
               FINANCIAL_DRILLING_CODE = FINANCIAL_TAX_DRILLING_CODE
               FIRST_YEAR = BASE_YEAR + 1
               OPEN(10,FILE='MSG'//trim(cldata%SCENAME)//'.DTX',
     +                                                 STATUS="REPLACE")
               DO YR = 1, REPORTING_YEARS  ! 1997,V,0,,S,,,,,,,,""
                  YEAR_STR=LEFT_JUSTIFY_I2_IN_STR(INT2(FIRST_YEAR+YR-1))
                  VAR_NUM_STR = LEFT_JUSTIFY_I2_IN_STR(INT2(YR-1))
                  WRITE(10,*) YEAR_STR//',V,'//trim(VAR_NUM_STR)//
     +                                                   ',,S,,,,,,,,""'
               ENDDO
               CLOSE(10)
            ENDIF
            FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                    trim(GET_SCENAME())//FINANCIAL_DRILLING_CODE
            RECORD_LENGTH = ESTABLISH_DRILLING_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            FILE_VARIABLES,
     +                                            DIMENSIONS,FIRST_YEAR,
     +                                            REPORTING_UNIT_NUMBER,
     +                                            NEXT_REC)
!
            WRITE(REPORTING_UNIT_NUMBER,REC=NEXT_REC)
     +                        'Endpoint            ','N',INT2(4),'V','D'
            NEXT_REC = NEXT_REC + 1
            IF(I == 3) THEN
               WRITE(REPORTING_UNIT_NUMBER,REC=NEXT_REC)
     +                       'Reporting Variable  ','C',INT2(30),'V','D'
               NEXT_REC = NEXT_REC + 1
               IF(FINANCIAL_DRILLING() == 'B' .OR.
     +                                 FINANCIAL_DRILLING() == 'C') THEN
!                 WRITE(REPORTING_UNIT_NUMBER,REC=NEXT_REC)
!    +                       'Asset Group         ','C',INT2(30),'V','D'
!                 NEXT_REC = NEXT_REC + 1
               ENDIF
               IF(FINANCIAL_DRILLING() == 'B' .OR.
     +                                 FINANCIAL_DRILLING() == 'M') THEN
                  WRITE(REPORTING_UNIT_NUMBER,REC=NEXT_REC)
     +                        'Period              ','C',INT2(6),'V','D'
                  NEXT_REC = NEXT_REC + 1
               ENDIF
               WRITE(REPORTING_UNIT_NUMBER,REC=NEXT_REC)
     +           'Account Name        ','C',DrillingAccountNameWidth,
     +                       'V','D',INT2(DrillingAccountNameWidth-5)
               NEXT_REC = NEXT_REC + 1
               DRILLING_BALANCE_SHEET_REC = NEXT_REC
            ELSEIF(I == 1 .OR. I == 2 .OR. I == 4) THEN
               WRITE(REPORTING_UNIT_NUMBER,REC=NEXT_REC)
     +                       'Reporting Variable  ','C',INT2(30),'V','D'
               NEXT_REC = NEXT_REC + 1
               IF(FINANCIAL_DRILLING() == 'B' .OR.
     +                                 FINANCIAL_DRILLING() == 'C') THEN
!                 WRITE(REPORTING_UNIT_NUMBER,REC=NEXT_REC)
!    +                       'Asset Group         ','C',INT2(30),'V','D'
!                 NEXT_REC = NEXT_REC + 1
               ENDIF
               IF(FINANCIAL_DRILLING() == 'B' .OR.
     +                                 FINANCIAL_DRILLING() == 'M') THEN
                  WRITE(REPORTING_UNIT_NUMBER,REC=NEXT_REC)
     +                        'Period              ','C',INT2(6),'V','D'
                  NEXT_REC = NEXT_REC + 1
               ENDIF
               WRITE(REPORTING_UNIT_NUMBER,REC=NEXT_REC)
     +           'Account Name        ','C',DrillingAccountNameWidth,
     +                       'V','D',INT2(DrillingAccountNameWidth-5)
               NEXT_REC = NEXT_REC + 1
               IF(I == 1) DRILLING_INCOME_REC = NEXT_REC
               IF(I == 2) DRILLING_CASH_REC = NEXT_REC
               IF(I == 4) DRILLING_TAX_REC = NEXT_REC
            ENDIF
         ENDDO
!
         DRILLING_FILE_IS_OPEN = .TRUE.
      RETURN
!***********************************************************************
      ENTRY MONTHLY_INCOME_UNIT_NO
!***********************************************************************
         MONTHLY_INCOME_UNIT_NO = FINANCIAL_MONTHLY_INCOME_NO
      RETURN
!***********************************************************************
      ENTRY MONTHLY_CASH_UNIT_NO
!***********************************************************************
         MONTHLY_CASH_UNIT_NO = FINANCIAL_MONTHLY_CASH_NO
      RETURN
!***********************************************************************
      ENTRY MONTHLY_BALANCE_UNIT_NO
!***********************************************************************
         MONTHLY_BALANCE_UNIT_NO = FINANCIAL_MONTHLY_BALANCE_NO
      RETURN
!***********************************************************************
      ENTRY MONTHLY_TAX_UNIT_NO
!***********************************************************************
         MONTHLY_TAX_UNIT_NO = FINANCIAL_MONTHLY_TAX_NO
      RETURN
!***********************************************************************
      ENTRY CPL_TAX_REPORTING_UNIT_NO
!***********************************************************************
         CPL_TAX_REPORTING_UNIT_NO = CPL_TAX_REPORTING_NO
      RETURN
!***********************************************************************
      ENTRY INCOME_DRILLING_UNIT_NO
!***********************************************************************
         INCOME_DRILLING_UNIT_NO = FINANCIAL_INCOME_DRILLING_NO
      RETURN
!***********************************************************************
      ENTRY CASH_DRILLING_UNIT_NO
!***********************************************************************
         CASH_DRILLING_UNIT_NO = FINANCIAL_CASH_DRILLING_NO
      RETURN
!***********************************************************************
      ENTRY BALANCE_DRILLING_UNIT_NO
!***********************************************************************
         BALANCE_DRILLING_UNIT_NO = FINANCIAL_BALANCE_DRILLING_NO
      RETURN
!***********************************************************************
      ENTRY TAX_DRILLING_UNIT_NO
!***********************************************************************
         TAX_DRILLING_UNIT_NO = FINANCIAL_TAX_DRILLING_NO
      RETURN
!***********************************************************************
! COPYRIGHT (C) 1998 M.S. GERBER & ASSOCIATES, INC. ALL RIGHTS RESERVED
!***********************************************************************
      ENTRY WRITE_DRILLING_RPT(DRILLING_NAME,
     +                         ACCOUNT_NAME,
     +                         MONTHLY_DATA,
     +                         DRILLING_REPORT,
     +                         ANNUAL_REPORT)
!***********************************************************************
!
         FINANCIAL_SIMULATION_YEARS = REPORTING_YEARS
         START_MONTHLY = 1
         START_YEAR = 1
         ANN_RPT_MO = 0
         NEED_CLOSING_BALANCE = .FALSE.
         END_MONTHLY = LAST_AVAILABLE_MONTHLY_YEAR
         IF(DRILLING_REPORT == BALANCE_SHEET_ITEM) THEN
            DRILLING_REPRT_UNIT = FINANCIAL_BALANCE_DRILLING_NO
            NEXT_REC = DRILLING_BALANCE_SHEET_REC
            START_YEAR = 1 ! 0
            ANN_RPT_MO = 0
            NEED_CLOSING_BALANCE = .TRUE.
            DATA_NOT_IN_BALANCE_DRILLING = .FALSE.
         ELSEIF(DRILLING_REPORT == BALANCE_SHEET_ANNUAL_ITEM) THEN
            DRILLING_REPRT_UNIT = FINANCIAL_BALANCE_DRILLING_NO
            NEXT_REC = DRILLING_BALANCE_SHEET_REC
            START_YEAR = 1 ! 0
            ANN_RPT_MO = 0
            DATA_NOT_IN_BALANCE_DRILLING = .FALSE.
         ELSEIF(DRILLING_REPORT == INCOME_REPORT_ITEM) THEN
            DRILLING_REPRT_UNIT = FINANCIAL_INCOME_DRILLING_NO
            NEXT_REC = DRILLING_INCOME_REC
            DATA_NOT_IN_INCOME_DRILLING = .FALSE.
         ELSEIF(DRILLING_REPORT == CASH_REPORT_ITEM) THEN
            DRILLING_REPRT_UNIT = FINANCIAL_CASH_DRILLING_NO
            NEXT_REC = DRILLING_CASH_REC
            DATA_NOT_IN_CASH_DRILLING = .FALSE.
         ELSEIF(DRILLING_REPORT == TAX_REPORT_ITEM) THEN
            DRILLING_REPRT_UNIT = FINANCIAL_TAX_DRILLING_NO
            NEXT_REC = DRILLING_TAX_REC
            START_YEAR = 0
            ANN_RPT_MO = 0
            DATA_NOT_IN_TAX_DRILLING = .FALSE.
         ELSE
            RETURN
         ENDIF
!
         WRITE_DRILLING_RPT = FINANCIAL_SIMULATION_YEARS
!
         IF(ANNUAL_REPORT) THEN
            WRITE(DRILLING_REPRT_UNIT,REC=NEXT_REC) PRT_ENDPOINT(),
     +                                 DRILLING_NAME,
     +                                 ACCOUNT_NAME,
     +                             (MONTHLY_DATA(ANN_RPT_MO,YR),
     +                                   YR = START_YEAR,
     +                                       FINANCIAL_SIMULATION_YEARS)
            NEXT_REC = NEXT_REC + 1
         ELSE   
            WRITE(DRILLING_REPRT_UNIT,REC=NEXT_REC) PRT_ENDPOINT(),
     +                                 DRILLING_NAME,
     +                                 SHORT_MONTH_NAMES(INT2(0)),
     +                                 ACCOUNT_NAME,
     +                             (MONTHLY_DATA(ANN_RPT_MO,YR),
     +                                   YR = START_YEAR,
     +                                       FINANCIAL_SIMULATION_YEARS)
            NEXT_REC = NEXT_REC + 1
            DO MO = 1, 12
               DO YR = START_YEAR, FINANCIAL_SIMULATION_YEARS
                  IF(YR >= START_MONTHLY  .AND. YR <= END_MONTHLY) THEN
                     IF(START_MONTHLY == 1) THEN
                        OUTPUT_VALUE(YR) = MONTHLY_DATA(MO,YR)  
                     ELSE
                        OUTPUT_VALUE(YR) = MONTHLY_DATA(MO,YR-1)  
                     ENDIF
                  ELSE
                     IF(MO == 12 .AND. YR > 0 .AND.
     +                                        NEED_CLOSING_BALANCE) THEN
                        OUTPUT_VALUE(YR) = MONTHLY_DATA(MO,YR)  
                     ELSE   
                        OUTPUT_VALUE(YR) = NOT_AVAIL  
                     ENDIF
                  ENDIF
               ENDDO
               WRITE(DRILLING_REPRT_UNIT,REC=NEXT_REC) PRT_ENDPOINT(),
     +                                DRILLING_NAME,
     +                                SHORT_MONTH_NAMES(MO),
     +                                ACCOUNT_NAME,
     +                              (OUTPUT_VALUE(YR), YR = START_YEAR,
     +                                       FINANCIAL_SIMULATION_YEARS)
               NEXT_REC = NEXT_REC + 1
            ENDDO
         ENDIF
         IF(DRILLING_REPORT == BALANCE_SHEET_ITEM) THEN
            DRILLING_BALANCE_SHEET_REC = NEXT_REC
         ELSEIF(DRILLING_REPORT == BALANCE_SHEET_ANNUAL_ITEM) THEN
            DRILLING_BALANCE_SHEET_REC = NEXT_REC
         ELSEIF(DRILLING_REPORT == INCOME_REPORT_ITEM) THEN
            DRILLING_INCOME_REC = NEXT_REC
         ELSEIF(DRILLING_REPORT == CASH_REPORT_ITEM) THEN
            DRILLING_CASH_REC = NEXT_REC
         ELSEIF(DRILLING_REPORT == TAX_REPORT_ITEM) THEN
            DRILLING_TAX_REC = NEXT_REC
         ENDIF
      RETURN
!***********************************************************************
      ENTRY INCOME_DRILLING_REPRT_REC 
!***********************************************************************
         INCOME_DRILLING_REPRT_REC = DRILLING_INCOME_REC
         DRILLING_INCOME_REC = DRILLING_INCOME_REC + 1
      RETURN
!***********************************************************************
      ENTRY CASH_DRILLING_REPRT_REC 
!***********************************************************************
         CASH_DRILLING_REPRT_REC = DRILLING_CASH_REC
         DRILLING_CASH_REC = DRILLING_CASH_REC + 1
      RETURN
!***********************************************************************
      ENTRY BALANCE_DRILLING_REPRT_REC 
!***********************************************************************
         BALANCE_DRILLING_REPRT_REC = DRILLING_BALANCE_SHEET_REC
         DRILLING_BALANCE_SHEET_REC = DRILLING_BALANCE_SHEET_REC + 1
      RETURN
!***********************************************************************
      ENTRY TAX_DRILLING_REPRT_REC
!***********************************************************************
         TAX_DRILLING_REPRT_REC = DRILLING_TAX_REC
         DRILLING_TAX_REC = DRILLING_TAX_REC + 1
      RETURN
!***********************************************************************
! COPYRIGHT (C) 1998 M.S. GERBER & ASSOCIATES, INC. ALL RIGHTS RESERVED
!***********************************************************************
      ENTRY RETURN_DRILLING_REPORT_STATUS(R_DATA_NOT_IN_CASH_DRILLING,
     +                                   R_DATA_NOT_IN_BALANCE_DRILLING,
     +                                   R_DATA_NOT_IN_INCOME_DRILLING,
     +                                   R_DATA_NOT_IN_TAX_DRILLING)
!***********************************************************************
!
         RETURN_DRILLING_REPORT_STATUS = 1
         R_DATA_NOT_IN_CASH_DRILLING = DATA_NOT_IN_CASH_DRILLING
         R_DATA_NOT_IN_BALANCE_DRILLING = DATA_NOT_IN_BALANCE_DRILLING
         R_DATA_NOT_IN_INCOME_DRILLING = DATA_NOT_IN_INCOME_DRILLING
         R_DATA_NOT_IN_TAX_DRILLING = DATA_NOT_IN_TAX_DRILLING
      RETURN
!***********************************************************************
      ENTRY SET_DRILLING_DATA_INCOME_TRUE         
!***********************************************************************
!
         DATA_NOT_IN_INCOME_DRILLING = .FALSE.
         SET_DRILLING_DATA_INCOME_TRUE = .TRUE.
      RETURN
!***********************************************************************
      ENTRY SET_DRILLING_DATA_TAX_TRUE
!***********************************************************************
!
         DATA_NOT_IN_TAX_DRILLING = .FALSE.
         SET_DRILLING_DATA_TAX_TRUE = .TRUE.
      RETURN
!***********************************************************************
      ENTRY SET_DRILLING_DATA_BALANCE_TRUE
!***********************************************************************
!
         DATA_NOT_IN_BALANCE_DRILLING = .FALSE.
         SET_DRILLING_DATA_BALANCE_TRUE = .TRUE.
      RETURN
!***********************************************************************
      ENTRY SET_DRILLING_DATA_CASH_TRUE
!***********************************************************************
!
         DATA_NOT_IN_CASH_DRILLING = .FALSE.
         SET_DRILLING_DATA_CASH_TRUE = .TRUE.
      RETURN
      END
!***********************************************************************
      FUNCTION ESTABLISH_DRILLING_REPORT_FILE(FILE_NAME,OVERHEAD_LENGTH,
     +                                        FILE_VARIABLES,
     +                                        DIMENSIONS,FIRST_YEAR,
     +                                        REPORTING_UNIT_NUMBER,
     +                                        NEXT_REC)
!***********************************************************************
!
      use GRX_PLANNING_ROUTINES
      INTEGER*2 ESTABLISH_DRILLING_REPORT_FILE,RECORD_LENGTH,
     +          FILE_VARIABLES,DIMENSIONS,FIRST_YEAR,
     +          REPORTING_UNIT_NUMBER,OVERHEAD_LENGTH,
     +          ENDYR,
     +          GET_NUM_OF_END_POINTS
      INTEGER NEXT_REC
      CHARACTER*(*) FILE_NAME
      CHARACTER*40 TITLE
      CHARACTER*28 COMPANY_NAME
      INTEGER*1 F7/z"f7"/
      LOGICAL*1 LAHEY_LF95
!
         RECORD_LENGTH = MAX(128,OVERHEAD_LENGTH + 4*FILE_VARIABLES)
         OPEN(REPORTING_UNIT_NUMBER,FILE=FILE_NAME,ACCESS="DIRECT",
     +                              STATUS="REPLACE",RECL=RECORD_LENGTH)
         IF(LAHEY_LF95()) THEN
            
            WRITE(REPORTING_UNIT_NUMBER,REC=1) F7,RECORD_LENGTH,
     +                                DIMENSIONS,FIRST_YEAR,
     +                                MIN(ENDYR(),FIRST_YEAR+INT2(4)),
     +                                GET_NUM_OF_END_POINTS()
            NEXT_REC = 2
         ELSE
            CLOSE(REPORTING_UNIT_NUMBER)
!         
            OPEN(REPORTING_UNIT_NUMBER,FILE=FILE_NAME,
     +                                ACCESS="TRANSPARENT",STATUS="OLD")
            WRITE(REPORTING_UNIT_NUMBER,REC=4) DIMENSIONS,FIRST_YEAR,
     +                                MIN(ENDYR(),FIRST_YEAR+INT2(4)),
     +                                GET_NUM_OF_END_POINTS()
            CLOSE(REPORTING_UNIT_NUMBER)
!         
            OPEN(REPORTING_UNIT_NUMBER,FILE=FILE_NAME,
     +                  ACCESS="DIRECT",STATUS="OLD",RECL=RECORD_LENGTH)
            NEXT_REC = 1
         ENDIF
         WRITE(REPORTING_UNIT_NUMBER,REC=NEXT_REC)  COMPANY_NAME()
         NEXT_REC = NEXT_REC+1
         WRITE(REPORTING_UNIT_NUMBER,REC=NEXT_REC)  TITLE()
         NEXT_REC = NEXT_REC+1
         ESTABLISH_DRILLING_REPORT_FILE = RECORD_LENGTH
      RETURN
      END
!***********************************************************************
      SUBROUTINE CREATE_DOLLAR_STREAM(YEARS,DATA_TYPE,ESCAL_VECT,
     +                                VALUES_IN,VALUES_OUT)
!***********************************************************************
!
      use PROD_ARRAYS_DIMENSIONS
      use grx_planning_routines
      USE SIZECOM
      use globecom

      INTEGER*2 YEARS,ESCAL_VECT,I,YR_2_CHANGE_2_VECTOR

      REAL*4 VALUES_OUT(0:*),VALUES_IN(0:*)
      CHARACTER*1 DATA_TYPE
      REAL*4 ESCALATION_RATES(AVAIL_DATA_YEARS),MULTIPLIER,RATE,
     +       ESCALATION_MULTIPLIER
      LOGICAL*1 VECTOR_TYPE,RETURN_A_VECTOR_FOR_ALL_YEARS
      REAL*4 ANNUAL_VALUES(*),MONTHLY_VALUES(12,5)
      INTEGER*2 YR,MO
!
         VALUES_OUT(0) = VALUES_IN(0)
         IF(DATA_TYPE == 'E' .OR. DATA_TYPE == 'C') THEN
            VECTOR_TYPE=RETURN_A_VECTOR_FOR_ALL_YEARS(ESCALATION_RATES,
     +                                                       ESCAL_VECT)
            IF(VECTOR_TYPE) THEN ! THE VECTOR IS VALUES
               DO I = 1, YEARS
                  IF(I < AVAIL_DATA_YEARS) THEN
                     VALUES_OUT(I) = ESCALATION_RATES(I)
                  ELSEIF(I == AVAIL_DATA_YEARS) THEN
                     VALUES_OUT(I) = ESCALATION_RATES(I)
                     IF(VALUES_OUT(I-1) /= 0) THEN
                        ESCALATION_MULTIPLIER = VALUES_OUT(I)/
     +                                                   VALUES_OUT(I-1)
                        IF(ESCALATION_MULTIPLIER > 1.2) THEN
                           ESCALATION_MULTIPLIER = 1.2   ! Added 11/14/06 to protect against "wild" implied escalation rates
                        ELSEIF(ESCALATION_MULTIPLIER < -1.2) THEN
                           ESCALATION_MULTIPLIER = 0.
                        ENDIF
                     ELSEIF(VALUES_OUT(AVAIL_DATA_YEARS) /= 0.) THEN
                        ESCALATION_MULTIPLIER = 1.
                     ELSE
                        ESCALATION_MULTIPLIER = 0.
                     ENDIF
                  ELSE
                     VALUES_OUT(I) = VALUES_OUT(I-1) *
     +                                             ESCALATION_MULTIPLIER
                  ENDIF
               ENDDO
            ELSE   
               DO I = 1, YEARS
                  IF(I <= AVAIL_DATA_YEARS)
     +                      ESCALATION_MULTIPLIER = ESCALATION_RATES(I)
                  VALUES_OUT(I) = VALUES_OUT(I-1)*ESCALATION_MULTIPLIER
               ENDDO
            ENDIF
            IF(DATA_TYPE == 'C') THEN
               DO I = 1, YEARS
                  IF(I <= AVAIL_DATA_YEARS) MULTIPLIER = VALUES_IN(I) 
                  VALUES_OUT(I) = (VALUES_OUT(I) * MULTIPLIER)/1000000.
               ENDDO
            ENDIF
         ELSE
            IF(DATA_TYPE == 'R') THEN
               DO I = 1, MIN(YEARS,AVAIL_DATA_YEARS)
                  IF(BASE_YEAR+I >= ESCAL_VECT) THEN
                     VALUES_OUT(I) = VALUES_OUT(I-1) *
     +                                            (1.+VALUES_IN(I)/100.)
                  ELSE
                     VALUES_OUT(I) = VALUES_IN(I)
                  ENDIF
               ENDDO
               IF(AVAIL_DATA_YEARS < YEARS) THEN
                  RATE = 1.+VALUES_IN(AVAIL_DATA_YEARS)/100.
                  DO I = AVAIL_DATA_YEARS+1, YEARS
                     VALUES_OUT(I) = VALUES_OUT(I-1) * RATE
                  ENDDO
               ENDIF
            ELSEIF(DATA_TYPE == 'V') THEN
               YR_2_CHANGE_2_VECTOR = ESCAL_VECT
               DO I = 1, MIN(YEARS,AVAIL_DATA_YEARS)
                  IF(BASE_YEAR+I < YR_2_CHANGE_2_VECTOR
     +                      .OR. YR_2_CHANGE_2_VECTOR <= BASE_YEAR) THEN
                     VALUES_OUT(I) = VALUES_IN(I)
                  ELSE
                     IF(BASE_YEAR+I == ESCAL_VECT) THEN
                        ESCAL_VECT = ABS(VALUES_IN(I))
                        VECTOR_TYPE =
     +                   RETURN_A_VECTOR_FOR_ALL_YEARS(ESCALATION_RATES,
     +                                                       ESCAL_VECT)
                     ENDIF
                     IF(VECTOR_TYPE) THEN ! THE VECTOR IS VALUES
                        VALUES_OUT(I) = ESCALATION_RATES(I)
                     ELSE
                        VALUES_OUT(I) = VALUES_OUT(I-1) *
     +                                             ESCALATION_RATES(I)
                     ENDIF
                  ENDIF
               ENDDO
               IF(AVAIL_DATA_YEARS < YEARS) THEN
                  IF(VECTOR_TYPE) THEN ! THE VECTOR IS VALUES
                     VALUES_OUT(AVAIL_DATA_YEARS+1:YEARS) =
     +                                ESCALATION_RATES(AVAIL_DATA_YEARS)
                  ELSE
                     RATE = ESCALATION_RATES(AVAIL_DATA_YEARS)
                     DO I = AVAIL_DATA_YEARS+1, YEARS
                        VALUES_OUT(I) = VALUES_OUT(I-1) * RATE
                     ENDDO
                  ENDIF
               ENDIF
            ELSEIF(DATA_TYPE == 'A') THEN   ! addendum dollars
               DO I = 1, MIN(YEARS,AVAIL_DATA_YEARS)
                  VALUES_OUT(I) = VALUES_IN(I)
               ENDDO
               IF(AVAIL_DATA_YEARS < YEARS) THEN
                  VALUES_OUT(AVAIL_DATA_YEARS+1:YEARS) =
     +                                       VALUES_IN(AVAIL_DATA_YEARS)
               ENDIF
            ELSE 
               DO I = 1, MIN(YEARS,AVAIL_DATA_YEARS)
                  VALUES_OUT(I) = VALUES_IN(I)
               ENDDO
               IF(AVAIL_DATA_YEARS < YEARS) THEN
                  IF(VALUES_OUT(AVAIL_DATA_YEARS-1) /= 0.) THEN
                     RATE = VALUES_OUT(AVAIL_DATA_YEARS)/
     +                              VALUES_OUT(AVAIL_DATA_YEARS-1)
                     IF(RATE > 1.2) THEN
                        RATE = 1.2   ! Added 11/14/06 to protect against "wild" implied escalation rates
                     ELSEIF(RATE < -1.2) THEN
                        RATE = 0.
                     ENDIF
                  ELSEIF(VALUES_OUT(AVAIL_DATA_YEARS) /= 0.) THEN
                     RATE = 1.
                  ELSE
                     RATE = 0.
                  ENDIF
                  DO I = AVAIL_DATA_YEARS+1, YEARS
                     VALUES_OUT(I) = VALUES_OUT(I-1) * RATE
                  ENDDO
               ENDIF
            ENDIF
         ENDIF
      RETURN
!***********************************************************************
      ENTRY RIPPLE_MONTHLY_DATA(ANNUAL_VALUES,MONTHLY_VALUES)
!***********************************************************************
!
!
         DO YR = 1, 5
            IF(MONTHLY_VALUES(1,YR) == -999999.) THEN
               MONTHLY_VALUES(1,YR) = ANNUAL_VALUES(YR) ! /12.
            ENDIF
            DO MO = 2, 12
               IF(MONTHLY_VALUES(MO,YR) == -999999.) THEN
                  MONTHLY_VALUES(MO,YR) = MONTHLY_VALUES(MO-1,YR)
               ENDIF
            ENDDO
         ENDDO
      RETURN
      END
!***********************************************************************
      SUBROUTINE MONTHLY_FINANCIAL_DATABASE(R_CLASS_ID,
     +                                      R_ASSET_CLASS_NAME,
     +                                      R_INCOME_VARS)
!***********************************************************************
!
      USE IREC_ENDPOINT_CONTROL
      use GRX_PLANNING_ROUTINES
      use globecom

      INCLUDE 'MTHNMCOM.MON'

      INTEGER (KIND=2), SAVE :: STORED_YEARS=0,SAVED_STORED_YEARS
      INTEGER (KIND=2) :: YR,MO
      REAL (KIND=4) :: R_INCOME_VARS(0:12,INCOME_VARS)
      REAL (KIND=4) :: TRAILING_12_MONTHS(INCOME_VARS)
      REAL (KIND=4), SAVE :: INCOME_ITEMS(0:12,
     +                        0:LAST_AVAILABLE_MONTHLY_YEAR,INCOME_VARS)
      LOGICAL (KIND=1), SAVE :: IPL_TRAILING_12_MONTH_RPT_OPEN=.FALSE.
      INTEGER (KIND=2), SAVE :: TRAILING_12_MOS_UNIT
      INTEGER (KIND=4), SAVE :: TRAILING_12_MOS_RPT_REC
      INTEGER (KIND=2) :: TRAILING_12_MOS_RPT_HEADER,R_YR,
     +                    CURRENT_CLASS_ID
      CHARACTER*7 MONTH_ENDING,DATE_12_MOS_ENDING
      REAL (KIND=4) :: MONTHLY_ATL_INCOME_STATEMENT_TAX_DEDUCTIONS,
     +                 R_CLASS_ID
      REAL (KIND=4), SAVE :: CLASS_ID
      INTEGER (KIND=4) :: REC_LEN
      LOGICAL (KIND=1) :: IPALCO,THIS_IS_REPORTING_CLASS
      CHARACTER (LEN=38), SAVE :: ASSET_CLASS_NAME
      CHARACTER (LEN=38) R_ASSET_CLASS_NAME
!      
         IF(YEAR > LAST_AVAILABLE_MONTHLY_YEAR) RETURN
         CLASS_ID = R_CLASS_ID
         ASSET_CLASS_NAME = R_ASSET_CLASS_NAME
         INCOME_ITEMS(:,YEAR,:) = R_INCOME_VARS(:,:)
         STORED_YEARS = STORED_YEARS + 1
      RETURN
!***********************************************************************
      ENTRY IPALCO_JURISDICTIONAL_RETAIL_REVENUES_REPORT(R_YR,
     +                                          CURRENT_CLASS_ID,
     +                                          R_INCOME_VARS,
     +                                          THIS_IS_REPORTING_CLASS)
!***********************************************************************
         R_INCOME_VARS(:,ipl_operating_revenues) = 
     +                 R_INCOME_VARS(:,monthly_tot_opn_rvnues) 
     +                 - R_INCOME_VARS(:,monthly_govt_revenues)
     +                 - R_INCOME_VARS(:,monthly_bulk_power)
         R_INCOME_VARS(:,mty_depn_n_amortn) = 
     +                        R_INCOME_VARS(:,Monthly_Book_Depreciation) 
     +                        + R_INCOME_VARS(:,Monthly_Amortization)
         R_INCOME_VARS(:,monthy_taxes_nonincome) = 
     +                           R_INCOME_VARS(:,Monthly_Property_Taxes) 
     +                           + R_INCOME_VARS(:,monthly_oth_taxes)
!         R_INCOME_VARS(:,IPL Monthy OandM Expenses) =
!     +                           R_INCOME_VARS(:,Monthly Property Taxes)
!     +                           + R_INCOME_VARS(:,monthly_oth_taxes)
         DO MO = 0, 12 
            R_INCOME_VARS(MO,IPL_Monthy_OandM_Expenses) =
     +                  MONTHLY_ATL_INCOME_STATEMENT_TAX_DEDUCTIONS(MO,
     +                                                    R_INCOME_VARS) 
     +                  + R_INCOME_VARS(MO,monthly_vacation_pay)
     +                  + R_INCOME_VARS(MO,MonthlyPensionExpense)
     +                  + R_INCOME_VARS(MO,MonthlySTORMExpense)
         ENDDO
         IF(THIS_IS_REPORTING_CLASS)
     +                   CALL IPALCO_ELECT_COSTUMER_COSTS(R_YR,
     +                                                 CURRENT_CLASS_ID,
     +                                                 R_INCOME_VARS)
         R_INCOME_VARS(:,IPL_Total_Operating_Expenses) = 
     +            R_INCOME_VARS(:,mty_depn_n_amortn)
     +            + R_INCOME_VARS(:,IPL_Monthy_OandM_Expenses) 
     +            + R_INCOME_VARS(:,monthy_taxes_nonincome)
     +            + R_INCOME_VARS(:,Monthly_Operating_Revenue_Tax)
     +            + R_INCOME_VARS(:,mthy_fed_atl_income_tax_pd)
     +            + R_INCOME_VARS(:,mthy_state_atl_incm_tax_pd)
     +            + R_INCOME_VARS(:,Monthly_Income_Tax_Deferrals_Cr)
     +            + R_INCOME_VARS(:,Monthly_ITC_Amortization)
         R_INCOME_VARS(:,IPL_Operating_Income) = 
     +                   R_INCOME_VARS(:,ipl_operating_revenues)
     +                   - R_INCOME_VARS(:,IPL_Total_Operating_Expenses)
      RETURN
!***********************************************************************
      ENTRY MONTHLY_TRAILING_12_MONTHS_REVENUE_REPORT()
!***********************************************************************
!
!  read the base year 12 months
!
         IF(.NOT. IPL_TRAILING_12_MONTH_RPT_OPEN) THEN
             TRAILING_12_MOS_UNIT = 
     +               TRAILING_12_MOS_RPT_HEADER(TRAILING_12_MOS_RPT_REC)

            IPL_TRAILING_12_MONTH_RPT_OPEN = .TRUE. 
         ENDIF
!
         DO YR = 1, STORED_YEARS
            DO MO = 1, 12
               IF(MO == 12) THEN ! .OR. YR ==1) THEN
                  TRAILING_12_MONTHS(:) = 
     +                                SUM(INCOME_ITEMS(1:MO,YR,:),DIM=1)
               ELSE
                  TRAILING_12_MONTHS(:) = 
     +                           SUM(INCOME_ITEMS(MO+1:12,YR-1,:),DIM=1)
     +                           + SUM(INCOME_ITEMS(1:MO,YR,:),DIM=1)
               ENDIF
               MONTH_ENDING = DATE_12_MOS_ENDING(MO,YR) 
               WRITE(TRAILING_12_MOS_UNIT,REC=TRAILING_12_MOS_RPT_REC)
     +                                        PRT_ENDPOINT(),
     +                                        FLOAT(BASE_YEAR+YR),
     +                                        MONTH_ENDING,
     +                                        CLASS_ID,
     +                                        TRAILING_12_MONTHS 
               TRAILING_12_MOS_RPT_REC = TRAILING_12_MOS_RPT_REC + 1
            ENDDO
         ENDDO
         SAVED_STORED_YEARS = STORED_YEARS
         STORED_YEARS = 0      
      RETURN
!***********************************************************************
      ENTRY BASE_YEAR_ACTUAL_INCOME_DATA()
!***********************************************************************
!
         IF(END_POINT > 1) RETURN  ! THE ACTUAL DATA HAS BEEN READ
         IF(IPALCO()) THEN
            CALL IPALCO_BASE_YEAR_ACTUAL_INCOME_DATA(INCOME_ITEMS)
         ENDIF
      RETURN
!***********************************************************************
      ENTRY IPALCO_REPORTS()
!***********************************************************************
         CALL IPALCO_TRAILING_12_MONTHS_REVENUE_REPORT(CLASS_ID,
     +                                               ASSET_CLASS_NAME,
     +                                               SAVED_STORED_YEARS,
     +                                               INCOME_ITEMS)
      RETURN
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE IPALCO_ELECT_COSTUMER_COSTS(R_YR,R_CLASS,R_INCOME_VARS)
!***********************************************************************
!
      INCLUDE 'MTHNMCOM.MON'

      LOGICAL (KIND=1) :: VOID_LOGICAL,TF_IPL_ELECTIC_PLAN_COST
      REAL (KIND=4) :: R_INCOME_VARS(0:12,INCOME_VARS)
      REAL (KIND=4) :: IPL_ELECTRIC_PLAN_FUEL_COSTS(0:12),
     +                 IPL_ELECTRIC_PLAN_PUR_COSTS(0:12)
      INTEGER (KIND=2) :: R_YR,R_CLASS
!
         VOID_LOGICAL = TF_IPL_ELECTIC_PLAN_COST(
     +                                     IPL_ELECTRIC_PLAN_FUEL_COSTS,
     +                                     IPL_ELECTRIC_PLAN_PUR_COSTS)
         CALL RETURN_MONTHLY_IPL_ELECT_PLAN(R_YR,R_CLASS,
     +                                     IPL_ELECTRIC_PLAN_FUEL_COSTS,
     +                                     IPL_ELECTRIC_PLAN_PUR_COSTS)
         R_INCOME_VARS(:,ipl_elec_plan_fuel_costs) =
     +                     R_INCOME_VARS(:,ipl_elec_plan_fuel_costs)
     +                     + IPL_ELECTRIC_PLAN_FUEL_COSTS(:)
         R_INCOME_VARS(:,ipl_elect_pln_purch_costs) = 
     +                 R_INCOME_VARS(:,ipl_elect_pln_purch_costs)
     +                 + IPL_ELECTRIC_PLAN_PUR_COSTS(:)
         R_INCOME_VARS(:,IPL_Monthy_OandM_Expenses) =
     +               R_INCOME_VARS(:,IPL_Monthy_OandM_Expenses)
     +               - R_INCOME_VARS(:,ipl_elec_plan_fuel_costs)
     +               - R_INCOME_VARS(:,ipl_elect_pln_purch_costs)
      RETURN
      END SUBROUTINE IPALCO_ELECT_COSTUMER_COSTS


!***********************************************************************
      SUBROUTINE IPALCO_BASE_YEAR_ACTUAL_INCOME_DATA(INCOME_ITEMS)
!***********************************************************************
!
      USE IREC_ENDPOINT_CONTROL
      use GRX_PLANNING_ROUTINES
      use globecom

      INCLUDE 'MTHNMCOM.MON'
      REAL (KIND=4), PARAMETER :: NOT_AVAIL=-999999.
      CHARACTER*7 MONTH_ENDING,DATE_12_MOS_ENDING
      CHARACTER*6 SHORT_MONTH_NAMES
      CHARACTER*25 COLUMN_HEADING
      INTEGER (KIND=2) :: YR,MO,R_YR,DELETE,RPT_YR,GET_MONTH_NUMBER,
     +                    STORED_YEARS,IPL_REGULATED_REVENUES_RPT_H
      INTEGER (KIND=4) :: IOS 
      CHARACTER (LEN=1024) :: RECLN
      REAL (KIND=4) :: INCOME_ITEMS(0:12,
     +                        0:LAST_AVAILABLE_MONTHLY_YEAR,INCOME_VARS)
      CHARACTER*256 FILE_NAME,IPALCO_ACTUAL_REGULATED_REVENUES_FILE
      LOGICAL FILE_OPEN
      REAL (KIND=4), SAVE :: IPL_REG_EXP_DATA(12,0:5,11)=0.,
     +                       IPL_REG_FACTORS(12,0:5,9)=100.,
     +                       IPL_TOTAL_ELECTRIC_REVS(12,0:5)=0.,
     +                       IPL_RETAIL_CUSTOMER_REVS(12,0:5)=0.
      REAL (KIND=4) :: TRAILING_12_MONTHS(11),
     +                 TRAILING_12_MONTHS_OP_REV,
     +                 TRAILING_12_MONTHS_RETAIL_REV,
     +                 CLASS_ID
      LOGICAL (KIND=1), SAVE :: IPL_ACTUAL_AVAILABLE(12,0:1)=.FALSE.,
     +                          IPL_12_MONTHS_REVENUE_RPT_OPEN=.FALSE.
      REAL (KIND=4) :: TEMP_IPL_REG_FACTORS(9)
      CHARACTER (LEN=4) :: RESERVED,
     +                     RPT_MO,
     +                     COMMENT
      CHARACTER (LEN=1) :: ACTIVE
      LOGICAL FILE_EXISTS
      INTEGER (KIND=2), SAVE :: IPL_REG_REVS_UNIT
      INTEGER (KIND=4), SAVE :: IPL_REG_REVS_REC
      CHARACTER (LEN=38) :: ASSET_CLASS_NAME
!
         FILE_NAME = IPALCO_ACTUAL_REGULATED_REVENUES_FILE()
!
         INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
         IF(FILE_EXISTS) THEN
            OPEN(10,FILE=FILE_NAME)
            READ(10,*) DELETE
            DO                                   
               DO
                  READ(10,'(A)',IOSTAT=IOS) RECLN
                  IF(IOS /=0) EXIT
                  IF(RECLN(1:1) == '7') EXIT
                  RECLN = TRIM(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
     +                               //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
     +                               //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
                  READ(RECLN,*,IOSTAT=IOS) DELETE,RPT_MO,RPT_YR,ACTIVE
                  IF(IOS /= 0) EXIT
                  IF(ACTIVE == 'N') CYCLE
                  MO = GET_MONTH_NUMBER(RPT_MO)
                  YR = RPT_YR - BASE_YEAR
                  IF(YR < 0 .OR. YR > 1 .OR. MO < 1) CYCLE   ! Data for the base year or the first year
                  READ(RECLN,*,IOSTAT=IOS) DELETE,RPT_MO,RPT_YR,ACTIVE,
     +                                  RESERVED,
     +                                  RESERVED,
     +                                  RESERVED,
     +                                  IPL_REG_EXP_DATA(MO,YR,:),
     +                                  IPL_TOTAL_ELECTRIC_REVS(MO,YR),
     +                                  IPL_RETAIL_CUSTOMER_REVS(MO,YR),
     +                                  RESERVED,
     +                                  COMMENT,
     +                                  TEMP_IPL_REG_FACTORS(:)   ! PROVIDES FOR RIPPLE OF INPUT         
                  IF(IOS /= 0) EXIT
                  IPL_REG_FACTORS(MO,YR,:) = TEMP_IPL_REG_FACTORS(:)
                  IPL_ACTUAL_AVAILABLE(MO,YR) = .TRUE. 
               ENDDO
               IF(IOS /= 0) EXIT
            ENDDO
            CLOSE(10)
            DO YR = 0, 1
               DO MO = 1, 12
                  IF(IPL_ACTUAL_AVAILABLE(MO,YR)) THEN
                     INCOME_ITEMS(MO,YR,
     +                               monthly_tot_opn_rvnues) = 
     +                                    IPL_TOTAL_ELECTRIC_REVS(MO,YR)
                     INCOME_ITEMS(MO,YR,ipl_operating_revenues) = 
     +                                   IPL_RETAIL_CUSTOMER_REVS(MO,YR)
                     INCOME_ITEMS(MO,YR,IPL_Monthy_OandM_Expenses) = 
     +                                         IPL_REG_EXP_DATA(MO,YR,1)
                     INCOME_ITEMS(MO,YR,
     +                           mty_depn_n_amortn) = 
     +                                         IPL_REG_EXP_DATA(MO,YR,2)
                     INCOME_ITEMS(MO,YR,monthy_taxes_nonincome) = 
     +                                         IPL_REG_EXP_DATA(MO,YR,3)
                     INCOME_ITEMS(MO,YR,Monthly_Operating_Revenue_Tax) = 
     +                                         IPL_REG_EXP_DATA(MO,YR,4)
                     INCOME_ITEMS(MO,YR,
     +                            ipl_misc_regltd_revnu) = 
     +                                         IPL_REG_EXP_DATA(MO,YR,5)
                     INCOME_ITEMS(MO,YR,
     +                            mthy_fed_atl_income_tax_pd) = 
     +                                         IPL_REG_EXP_DATA(MO,YR,6)
                     INCOME_ITEMS(MO,YR,
     +                              mthy_state_atl_incm_tax_pd) = 
     +                                         IPL_REG_EXP_DATA(MO,YR,7)
                     INCOME_ITEMS(MO,YR,
     +                                Monthly_Income_Tax_Deferrals_Cr) = 
     +                                         IPL_REG_EXP_DATA(MO,YR,8)
                     INCOME_ITEMS(MO,YR,Monthly_ITC_Amortization) = 
     +                                         IPL_REG_EXP_DATA(MO,YR,9)
                     INCOME_ITEMS(MO,YR,IPL_Total_Operating_Expenses) = 
     +                                        IPL_REG_EXP_DATA(MO,YR,10)
                     INCOME_ITEMS(MO,YR,IPL_Operating_Income) = 
     +                                        IPL_REG_EXP_DATA(MO,YR,11)
                     TEMP_IPL_REG_FACTORS(:)= IPL_REG_FACTORS(MO,YR,:)
                  ELSE
                     IPL_REG_FACTORS(MO,YR,:) = TEMP_IPL_REG_FACTORS(:)
                  ENDIF
               ENDDO
            ENDDO
            DO YR = 2, 5
               DO MO = 1, 12
                  IPL_REG_FACTORS(MO,YR,:) = TEMP_IPL_REG_FACTORS(:)
               ENDDO
            ENDDO
                             ! 1  Operation_and_Maintenance_Expenses,
                             ! 2  Depreciation_and_Amortization,
                             ! 3  Real_Estate_and_Personal_Property_Taxes,
                             ! 4  Indiana_Gross_Income_Taxes,
                             ! 5  Miscellaneous,
                             ! 6  Current_Federal,
                             ! 7  Current_State,
                             ! 8  Deferred_Income_Taxes,
                             ! 9  Investment_Tax_Credit_Adjustments,
                             ! 10 Total_Operating_Expenses,
                             ! 11 Operating_Income
         ENDIF
      RETURN
!***********************************************************************
      ENTRY IPALCO_TRAILING_12_MONTHS_REVENUE_REPORT(CLASS_ID,
     +                                               ASSET_CLASS_NAME,
     +                                               STORED_YEARS,
     +                                               INCOME_ITEMS)
!***********************************************************************
!
         IF(.NOT. IPL_12_MONTHS_REVENUE_RPT_OPEN) THEN
            IPL_REG_REVS_UNIT = 
     +               IPL_REGULATED_REVENUES_RPT_H(IPL_REG_REVS_REC)
            IPL_12_MONTHS_REVENUE_RPT_OPEN = .TRUE.
         ENDIF
         IPL_TOTAL_ELECTRIC_REVS(:,:) =
     +             INCOME_ITEMS(1:12,:,monthly_tot_opn_rvnues) 
         IPL_RETAIL_CUSTOMER_REVS(:,:) =
     +                       INCOME_ITEMS(1:12,:,ipl_operating_revenues)  
         IPL_REG_EXP_DATA(:,:,1) =
     +                    INCOME_ITEMS(1:12,:,IPL_Monthy_OandM_Expenses)
         IPL_REG_EXP_DATA(:,:,2) =
     +         INCOME_ITEMS(1:12,:,mty_depn_n_amortn)  
         IPL_REG_EXP_DATA(:,:,3) =
     +                       INCOME_ITEMS(1:12,:,monthy_taxes_nonincome)  
         IPL_REG_EXP_DATA(:,:,4) =
     +                INCOME_ITEMS(1:12,:,Monthly_Operating_Revenue_Tax)  
         IPL_REG_EXP_DATA(:,:,5) =
     +          INCOME_ITEMS(1:12,:,ipl_misc_regltd_revnu)  
         IPL_REG_EXP_DATA(:,:,6) =
     +          INCOME_ITEMS(1:12,:,mthy_fed_atl_income_tax_pd)  
         IPL_REG_EXP_DATA(:,:,7) =
     +            INCOME_ITEMS(1:12,:,mthy_state_atl_incm_tax_pd)  
         IPL_REG_EXP_DATA(:,:,8) =
     +              INCOME_ITEMS(1:12,:,Monthly_Income_Tax_Deferrals_Cr)  
         IPL_REG_EXP_DATA(:,:,9) =
     +                     INCOME_ITEMS(1:12,:,Monthly_ITC_Amortization)  
         IPL_REG_EXP_DATA(:,:,10) =
     +                 INCOME_ITEMS(1:12,:,IPL_Total_Operating_Expenses)  
         IPL_REG_EXP_DATA(:,:,11) =
     +                         INCOME_ITEMS(1:12,:,IPL_Operating_Income)  
         DO YR = 0, STORED_YEARS
            DO MO = 1, 12
               IF(MO == 12 .OR. YR ==0) THEN ! .OR. YR ==1) THEN
                  TRAILING_12_MONTHS(:) = 
     +                            SUM(IPL_REG_EXP_DATA(1:MO,YR,:),DIM=1)
                  TRAILING_12_MONTHS_OP_REV =
     +                             SUM(IPL_TOTAL_ELECTRIC_REVS(1:MO,YR))
                  TRAILING_12_MONTHS_RETAIL_REV =
     +                            SUM(IPL_RETAIL_CUSTOMER_REVS(1:MO,YR))
               ELSE
                  TRAILING_12_MONTHS(:) = 
     +                       SUM(IPL_REG_EXP_DATA(MO+1:12,YR-1,:),DIM=1)
     +                       + SUM(IPL_REG_EXP_DATA(1:MO,YR,:),DIM=1)
                  TRAILING_12_MONTHS_OP_REV =
     +                        SUM(IPL_TOTAL_ELECTRIC_REVS(MO+1:12,YR-1))
     +                        + SUM(IPL_TOTAL_ELECTRIC_REVS(1:MO,YR))
                  TRAILING_12_MONTHS_RETAIL_REV =
     +                       SUM(IPL_RETAIL_CUSTOMER_REVS(MO+1:12,YR-1))
     +                       + SUM(IPL_RETAIL_CUSTOMER_REVS(1:MO,YR))
               ENDIF
               
               MONTH_ENDING = DATE_12_MOS_ENDING(MO,YR)
               MONTH_ENDING = SHORT_MONTH_NAMES(MO)
               COLUMN_HEADING = 'Total Electric' 
               WRITE(IPL_REG_REVS_UNIT,REC=IPL_REG_REVS_REC)
     +                                        ASSET_CLASS_NAME,
     +                                        PRT_ENDPOINT(),
     +                                        FLOAT(BASE_YEAR+YR),
     +                                        MONTH_ENDING,
     +                                        COLUMN_HEADING,
     +                                        CLASS_ID,
     +                                        TRAILING_12_MONTHS_OP_REV,
     +                                        TRAILING_12_MONTHS 
               IPL_REG_REVS_REC = IPL_REG_REVS_REC + 1
               COLUMN_HEADING = 'Allocation %' 
               WRITE(IPL_REG_REVS_UNIT,REC=IPL_REG_REVS_REC)
     +                                        ASSET_CLASS_NAME,
     +                                          PRT_ENDPOINT(),
     +                                         FLOAT(BASE_YEAR+YR),
     +                                         MONTH_ENDING,
     +                                         COLUMN_HEADING,
     +                                         CLASS_ID,
     +                                         NOT_AVAIL,
     +                                         IPL_REG_FACTORS(MO,YR,:),
     +                                         NOT_AVAIL, 
     +                                         NOT_AVAIL 
               IPL_REG_REVS_REC = IPL_REG_REVS_REC + 1
               TRAILING_12_MONTHS(1:9) = TRAILING_12_MONTHS(1:9) * 
     +                                     IPL_REG_FACTORS(MO,YR,:)/100.
               TRAILING_12_MONTHS(10) = SUM(TRAILING_12_MONTHS(1:9))
               TRAILING_12_MONTHS(11) = TRAILING_12_MONTHS_RETAIL_REV
     +                                  - TRAILING_12_MONTHS(10)
               COLUMN_HEADING = 'Jurisdictional Customers' 
               WRITE(IPL_REG_REVS_UNIT,REC=IPL_REG_REVS_REC)
     +                                    ASSET_CLASS_NAME,
     +                                    PRT_ENDPOINT(),
     +                                    FLOAT(BASE_YEAR+YR),
     +                                    MONTH_ENDING,
     +                                    COLUMN_HEADING,
     +                                    CLASS_ID,
     +                                    TRAILING_12_MONTHS_RETAIL_REV,
     +                                    TRAILING_12_MONTHS 
               IPL_REG_REVS_REC = IPL_REG_REVS_REC + 1
            ENDDO
         ENDDO
         
      END SUBROUTINE IPALCO_BASE_YEAR_ACTUAL_INCOME_DATA
!***********************************************************************
      SUBROUTINE IMPLIED_ESC(VALUE29,VALUE30)
!***********************************************************************
         REAL (KIND=4) :: RATE,VALUE29,VALUE30
         IF(VALUE29 /= 0.) THEN
            RATE = VALUE30/VALUE29
            IF(RATE > 1.2) THEN
               RATE = 1.2   ! Added 11/14/06 to protect against "wild" implied escalation rates
            ELSEIF(RATE < -1.2) THEN
               RATE = 0.
            ENDIF
         ELSEIF(VALUE30 /= 0.) THEN
            RATE = 1.
         ELSE
            RATE = 0.
         ENDIF
         VALUE29 =  RATE
      END SUBROUTINE IMPLIED_ESC
!***********************************************************************
      
