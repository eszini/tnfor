!     ******************************************************************
!     MIDASMOD
!     Copyright(c) M.S. Gerber & Associates 2000
!
!     Created: 12/26/2002 12:14:31 PM
!     Author : MARK S GERBER
!     Last change: msg 11/11/2018 12:28:24 PM
!     ******************************************************************

C**********************************************************************
      MODULE DRILLING_REPT_PARAMETERS
         INTEGER (KIND=2), PARAMETER :: DrillingAccountNameWidth=50
         CHARACTER (LEN=DrillingAccountNameWidth) 
     +                                             DRILLING_ACCOUNT_NAME
      CONTAINS
C***********************************************************************
         FUNCTION WVPA_REPORTING_NAME(ACCOUNT_DESCRIPTION,
     +                                WVPA_ACCTNO,
     +                                WVPA_ACCOUNTING_UNIT,
     +                                WVPA_SUB_ACCOUNT_NUMBER)
C***********************************************************************
C
            CHARACTER (LEN=DrillingAccountNameWidth) 
     +                                        WVPA_REPORTING_NAME,
     +                                        WVPA_OUTPUT_REPORTING_NAME
            INTEGER WVPA_ACCTNO,
     +              WVPA_ACCOUNTING_UNIT,
     +              WVPA_SUB_ACCOUNT_NUMBER
            CHARACTER (LEN=*) :: ACCOUNT_DESCRIPTION
            CHARACTER*12 TEMP_STR
C
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
C***********************************************************************
         FUNCTION WVPA_DEPART_EXP_REPORT_NAME(ACCOUNT_DESCRIPTION,
     +                                WVPA_ACCTNO,
     +                                WVPA_ACCOUNTING_UNIT,
     +                                WVPA_WORK_ORDER_NUMBER)
C***********************************************************************
C
            CHARACTER (LEN=DrillingAccountNameWidth) 
     +                                      WVPA_DEPART_EXP_REPORT_NAME,
     +                                      WVPA_OUTPUT_REPORTING_NAME
            INTEGER WVPA_ACCTNO,
     +              WVPA_ACCOUNTING_UNIT
            CHARACTER (LEN=*) :: ACCOUNT_DESCRIPTION
            CHARACTER (LEN=10) :: WVPA_WORK_ORDER_NUMBER
            CHARACTER*12 TEMP_STR
C
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
C***********************************************************************
         FUNCTION WVPA_WORK_ORDER_REPORT_NAME(ACCOUNT_DESCRIPTION,
     +                                WVPA_ACCTNO,
     +                                WVPA_ACCOUNTING_UNIT,
     +                                WVPA_WORK_ORDER_NUMBER)
C***********************************************************************
C
            CHARACTER (LEN=DrillingAccountNameWidth) 
     +                                      WVPA_WORK_ORDER_REPORT_NAME,
     +                                      WVPA_OUTPUT_REPORTING_NAME
            INTEGER WVPA_ACCTNO,
     +              WVPA_ACCOUNTING_UNIT
            CHARACTER (LEN=*) :: ACCOUNT_DESCRIPTION
            CHARACTER (LEN=10) :: WVPA_WORK_ORDER_NUMBER
            CHARACTER*12 TEMP_STR_ACCTNO
C
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
C**********************************************************************
      FUNCTION BUILD_NUM()
C***********************************************************************
C
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
C
C      INCLUDE 'MSGINDEX.FOR'
C
C***********************************************************************
      SUBROUTINE MIDAS_ROOT_PROGRAM
C***********************************************************************
C
      USE GRX_PLANNING_ROUTINES
      USE ProSymModule
      USE FINANCIAL_SWITCHES_COMMON
      use logging
      use miscmod
      use midas_decs
      use cl_data

      INCLUDE 'SpinLib.MON'
      use kepcocom
      implicit none
C
C VARIABLES TO SIZE THE MODEL
C
      INCLUDE 'SIZECOM.MON'
      INCLUDE 'LAMCOM.MON'
      INCLUDE 'DSMFRCOM.MON'
C  BEGINNING OF THE COMMON BLOCK SECTION FOR THE PRODUCTION MODULE
      INCLUDE 'PRODCOM.MON'
      INCLUDE 'PROD2COM.MON'
      INCLUDE 'CNTRCOM.MON'
C
C VARIABLES FOR THE DETAILED REPORTS
C
      INCLUDE 'DREPTCOM.MON'
C
      INCLUDE 'ANNLCOM.MON'
      
C
C SHORT FORM ITEMS
C
      INTEGER*2 LAST_VARIABLE,STORE_LAST_VARIABLE,
     +          SET_CALENDAR_INFORMATION
      CHARACTER*1 SET_CHARACTER_DATA
      INTEGER*2 MIDAS_NORMAL_SIMULATION
C
C     VARIABLES FOR COST OF SERVICE
      LOGICAL*1 POSSIBLE_OPTIONS_EXIST,WVPA,NO_AUTO_INDEXING
C
C     TYPE DECLARATION FOR /SCREEN/
C
      LOGICAL*4 ESCAPE
C     LOGICAL*4 REPORT_FILE_EXISTS
C FILE NAMES
      CHARACTER*5 STUDY_NAME
C
C PARAMETER SECTION
C  COMMON BLOCK FOR ERROR MESSAGES
      LOGICAL*1 MESSAGE,DEBUG_ON
      INTEGER*2 SET_VIDEO_SYSTEM,INVERSE_VIDEO_COM
      LOGICAL*1 USER_TERMINATED
      COMMON /ERRORS/ INVERSE_VIDEO_COM,MESSAGE,DEBUG_ON,USER_TERMINATED
      LOGICAL*1 ASSET_ANALYST_ONLY
C
C  END COMMON BLOCK SECTION FOR THE PRODUCTION MODULES
C
C NEW VARIABLES ADDED BY M.S.G. JULY 29, 1993 - 
C      
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
C********** END OF THE DATA DECLARATIONS *********
C
C
      EXTERNAL ERROR_MESSAGE
C
      USER_TERMINATED = .FALSE.

      INVERSE_VIDEO_COM = SET_VIDEO_SYSTEM()
C      
      CALL NDPEXC
C
C SET SOME PARAMETERS
C
      
C
      LAST_VARIABLE = STORE_LAST_VARIABLE(SOD_SIZE)
C
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

C 11/08/06 ARRAYS USING MAX_SIMULATION_YEARS ARE NOW DYNAMIC Dr.G
      MAX_SIMULATION_YEARS = RUN_YEARS() + EXTENSION_YEARS() + 1
C
      VOID_LOGICAL = SET_ASSET_ANALYST_ONLY_SWITCH()
      WABASH_VALLEY = WABASH_VALLEY .AND. .NOT. ASSET_ANALYST_ONLY()
      VOID_LOGICAL = SET_TRANSACT_ANALYST_ONLY_SWTCH()
      CALL DSSCREEN(cldata%SCENAME)
      I2_RETURN_VALUE = STORED_SYSTEM_VARIABLES(POINTS_IN_LOAD_CURVE)
      I2_RETURN_VALUE = SET_CALENDAR_INFORMATION(INT2(1))
      CALL SET_UP_OVERLAY_FAMILIES(BASE_FILE_DEFINITION)

C      
C OPEN AND READ DSS FILES FILE
C
      CALL CREATE_REPORT_HEADER(cldata%SCENAME)
      VOID_LOGICAL = SET_STARTING_RECORD_4_UNIT38()
      END_POINT = 1
      GRX_SAVED_ENDPOINT = END_POINT
C
C IF THE BASE CASE EXISTS THE OVERHEAD FINANCIAL CALCULATIONS ARE NOT DONE
C **** NEED A CHECK FOR INDENTIFYING CHANGES TO THE BASE CASE
C
      CALL ERASEWC('BC*.BIN')
      CALL ERASEWC('OL*.BIN')
      CALL DISPLAY_TIME


         CALL MAKE_ALL_BINARY

      CALL DISPLAY_TIME
C
C SET PROPER ESCALATION VALUES
C
C THE CALL TO THE ESCALATION RATE OBJECT ADDED 5/20/93 FOR THE FINANCIAL
C CALCULATIONS
C
      IF(.NOT. RUN_COAL_MODEL_ONLY()) THEN
         VOID_LOGICAL = READ_ESCALATION_FILE()
         CALL ADDENDUMS(.TRUE.)

         IF(INIT_FILE_IS_ACTIVE()) THEN
            VOID_LOGICAL = RETURN_DET_RPTS_FOR_ASSETS()
            CALL MG_LOCATE_WRITE(16,9,'Financial asset overhead',
     +                                                   ALL_VERSIONS,1)
C
C ADDED FOR PROPERTY TAX CALCULATIONS 4/14/92
C
C
C START FINANCIAL DRILLING REPORT
C
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
C
         ENDIF
         CALL MG_CLEAR_LINE(16,9,36,ALL_VERSIONS,0)
         CALL CALULATE_OVERHEAD_TIME
C
C ENTRY POINT FOR THE ENDPOINT LOOP
C
         IF(WVPA()) THEN
            CALL WVPA_DATA_BASES
            CALL WVPA_TRACKER_DATA_BASE()
            CALL WVPA_PROCESS_ACTUAL_PURCHASED_POWER()
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
C***********************************************************************
      FUNCTION END_OF_STUDY_FLAG()
C***********************************************************************
      LOGICAL*1 SET_END_OF_STUDY_FLAG_TRUE,
     +          END_OF_STUDY_FLAG,
     +          SET_END_OF_STUDY_FLAG_FALSE,
     +          VALUE_END_OF_STUDY_FLAG/.FALSE./
C
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
C***********************************************************************
      FUNCTION END_OF_PLAN_FLAG()
C***********************************************************************
      LOGICAL*1 SET_END_OF_PLAN_FLAG_TRUE,
     +          END_OF_PLAN_FLAG,
     +          SET_END_OF_PLAN_FLAG_FALSE,
     +          VALUE_END_OF_PLAN_FLAG/.FALSE./
C
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
C***********************************************************************
      FUNCTION ASSET_ANALYST_ONLY()
C***********************************************************************
C
      INCLUDE 'SpinLib.MON'
      use kepcocom
      INCLUDE 'SIZECOM.MON'
      
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
C***********************************************************************
      ENTRY SET_ASSET_ANALYST_ONLY_SWITCH()
C***********************************************************************
C
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
C***********************************************************************
      FUNCTION TRANSACT_ANALYST_ONLY()
C***********************************************************************
C
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
C***********************************************************************
      ENTRY SET_TRANSACT_ANALYST_ONLY_SWTCH()
C***********************************************************************
C
         STATE_OF_TRANSACT_ANALYST = NERC_REGION_BASED_FORECAST()         
         SET_TRANSACT_ANALYST_ONLY_SWTCH = STATE_OF_TRANSACT_ANALYST 
      RETURN
      END
C
C***********************************************************************
C
      SUBROUTINE BC_RESET
C
      INCLUDE 'SIZECOM.MON'
C
      INTEGER*2 FA_UNIT/44/,NEW_ADDITIONS_FA/444/

         CALL SET_FOSSILOL_TO_OL !debugstop
C
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
C***********************************************************************
C
C     A SUBROUTINE TO INTEGRATE THE PROB CURVE TO FIND THE ENERGY
C
C***********************************************************************
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
C
C
C LAST_VARIABLE FUNCTION
C
C***********************************************************************
c     module StoredSysVarLocals
C***********************************************************************
c        REAL*4 DELTA_X_FOR_LOAD_CURVE/1./
c        INTEGER*2 POINTS_IN_CURRENT_CURVE,POINTS_IN_LOAD_CURVE
c        SAVE POINTS_IN_CURRENT_CURVE
c        SAVE POINTS_IN_LOAD_CURVE
c     END module
C***********************************************************************
      INTEGER*2 FUNCTION STORED_SYSTEM_VARIABLES(R_POINTS_IN_LOAD_CURVE)
C***********************************************************************
C

      use logging
      INCLUDE 'SPINLIB.MON'
      INCLUDE 'SIZECOM.MON'
      REAL*4 DELTA_X_FOR_LOAD_CURVE
      INTEGER*2 POINTS_IN_CURRENT_CURVE,POINTS_IN_LOAD_CURVE
      COMMON /StoredSysVars/DELTA_X_FOR_LOAD_CURVE,
     +                      POINTS_IN_CURRENT_CURVE,POINTS_IN_LOAD_CURVE
      INTEGER*2 VALUE_OF_LAST_VARIABLE,S_LAST_VARIABLE,
     +          STORE_LAST_VARIABLE
C
      INTEGER*2 LAST_VARIABLE,MAX_YEARS
      INTEGER*2 max_fuel_types_mm,
     +          R_POINTS_IN_LOAD_CURVE
      LOGICAL*1 SET_POINTS
      INTEGER*2 R_IPOT,IPNT,SET_CURRENT_IPNT
      REAL R_DX
C
C  COMMON BLOCK VARIABLES FOR /WORLD/
      CHARACTER*2 DATA_DRIVE,ACTIVE_DATA_DRIVE
      INTEGER*2 BASE_YEAR_COMMON,MAXIMUM_YEARS,BASE_YEAR
      COMMON /WORLD/ BASE_YEAR_COMMON,MAXIMUM_YEARS
      COMMON /DATA_DRIVE_LOCATION/ DATA_DRIVE
      SAVE VALUE_OF_LAST_VARIABLE
C
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
C***********************************************************************
      LOGICAL*1 FUNCTION SET_DELTA_X_VALUE(R_DELTA_X_FOR_LOAD_CURVE)
C***********************************************************************
C
c     USE StoredSysVarLocals
      REAL*4 DELTA_X_FOR_LOAD_CURVE
      INTEGER*2 POINTS_IN_CURRENT_CURVE,POINTS_IN_LOAD_CURVE
      COMMON /StoredSysVars/DELTA_X_FOR_LOAD_CURVE,
     +                      POINTS_IN_CURRENT_CURVE,POINTS_IN_LOAD_CURVE
c
      REAL*4 R_DELTA_X_FOR_LOAD_CURVE,R_DX
      INTEGER*2 R_IPOT
      LOGICAL*1 SET_POINTS
C
         IF(R_DELTA_X_FOR_LOAD_CURVE > 0.) THEN
            DELTA_X_FOR_LOAD_CURVE = MAX(R_DELTA_X_FOR_LOAD_CURVE,1.)
         ELSE
            DELTA_X_FOR_LOAD_CURVE = R_DELTA_X_FOR_LOAD_CURVE
         ENDIF
         SET_DELTA_X_VALUE = .TRUE.
      RETURN
C***********************************************************************
      ENTRY SET_POINTS(R_IPOT,R_DX)
C***********************************************************************
         R_IPOT = POINTS_IN_LOAD_CURVE
         POINTS_IN_CURRENT_CURVE = POINTS_IN_LOAD_CURVE
         SET_POINTS = .TRUE.
         R_DX = DELTA_X_FOR_LOAD_CURVE
         IF(DELTA_X_FOR_LOAD_CURVE > 0.) SET_POINTS = .FALSE.
      RETURN
      END
C***********************************************************************
      CHARACTER*5 FUNCTION VERSION()
C***********************************************************************
C
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
C
C***********************************************************************
      FUNCTION SET_VIDEO_SYSTEM()
C***********************************************************************
C
      INCLUDE 'SPINLIB.MON'
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
C
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
C***********************************************************************
      FUNCTION SET_CHARACTER_DATA()
C***********************************************************************
C
      USE PROD_ARRAYS_DIMENSIONS
C         INCLUDE 'SIZECOM.MON'
         INCLUDE 'GLOBECOM.MON'
         CHARACTER*1 SET_CHARACTER_DATA
         CHARACTER*20 SAVE_CLASS_NAME(MAX_LOAD_CLASSES+1)/'Commercial',
     +            'Residential','Industrial','Other 1','Other 2',
     +            'Other 3','System '/,CLASS_NAME
         INTEGER*2 CLASS
C        SAVE SAVE_CLASS_NAME
         SET_CHARACTER_DATA = 'X'
      RETURN
      ENTRY CLASS_NAME(CLASS)
         CLASS_NAME = SAVE_CLASS_NAME(CLASS)
      RETURN
      END
C
C ADDED 8/4/93
C
C***********************************************************************
      FUNCTION SET_CALENDAR_INFORMATION(START_MO)
C***********************************************************************
C
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
C
      SAVE DAYS_PER,TOTAL_DAYS_AT_THE_END_OF
C
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
C***********************************************************************
      FUNCTION SET_CHARACTER_CALENDAR_INFO(START_MO)
C***********************************************************************
C
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
C      
         J = 1
         DO I = START_MO, 12
            TEMP_NAME(J) = PERIOD_NAME(I)
            J = J + 1
         ENDDO
         DO I = 1, START_MO-1
            TEMP_NAME(J) = PERIOD_NAME(I)
            J = J + 1
         ENDDO
c         IF(PRODUCTION_PERIODS() == 1) THEN
c            PERIOD_NAME(1) = 'Annual'
c         ELSE ! IF(PRODUCTION_PERIODS() == 12) THEN
c            DO I = 1, 12
               PERIOD_NAME = TEMP_NAME
c            ENDDO
c         ENDIF
         IF(SALT_RIVER_PROJECT()) THEN 
c            MONTH_NAMES_SHORT = SRP_MONTH_NAMES_SHORT
c            MONTH_NAMES_LONG = SRP_MONTH_NAMES_LONG
         ENDIF
         SET_CHARACTER_CALENDAR_INFO = 'X'
      RETURN
C***********************************************************************
      ENTRY MONTH_NAME(MONTH)
C***********************************************************************
         IF(MONTH <= 0 .OR. MONTH > 12) THEN
            MONTH_NAME = "NA"
         ELSE
            MONTH_NAME = PERIOD_NAME(MONTH)
         ENDIF
      RETURN
C***********************************************************************
      ENTRY LOADS_MONTH_NAME(MONTH)
C***********************************************************************
         IF(MONTH <= 0 .OR. MONTH > 12) THEN
            LOADS_MONTH_NAME = "NA"
         ELSE
            LOADS_MONTH_NAME = LOADS_NAME(MONTH)
         ENDIF
      RETURN
C***********************************************************************
      ENTRY SHORT_MONTH_NAMES(MONTH)
C***********************************************************************
         IF(MONTH < 0 .OR. MONTH > 12) THEN
            SHORT_MONTH_NAMES = "NA"
         ELSE
            SHORT_MONTH_NAMES = MONTH_NAMES_SHORT(MONTH)
         ENDIF
      RETURN
C***********************************************************************
      ENTRY BAL_SHORT_MONTH_NAMES(MONTH)
C***********************************************************************
         IF(MONTH == 0) THEN
            BAL_SHORT_MONTH_NAMES = 'BOY'
         ELSE
            BAL_SHORT_MONTH_NAMES = MONTH_NAMES_SHORT(MONTH)
         ENDIF
      RETURN
C***********************************************************************
      ENTRY LONG_MONTH_NAMES(MONTH)
C***********************************************************************
         IF(MONTH < 0 .OR. MONTH > 12) THEN
            LONG_MONTH_NAMES = "NA"
         ELSE
            LONG_MONTH_NAMES = MONTH_NAMES_LONG(MONTH)
         ENDIF
      RETURN
C***********************************************************************
      ENTRY THREE_CHAR_MONTH_NAMES(MONTH)
C***********************************************************************
         IF(MONTH <= 0 .OR. MONTH > 12) THEN
            THREE_CHAR_MONTH_NAMES = "NA"
         ELSE
            THREE_CHAR_MONTH_NAMES = MONTH_3_CHAR_NAMES_SHORT(MONTH)
         ENDIF
      RETURN
C***********************************************************************
      ENTRY BAL_LONG_MONTH_NAMES(MONTH)
C***********************************************************************
         IF(MONTH == 0) THEN
            BAL_LONG_MONTH_NAMES = 'BOY'
         ELSE
            BAL_LONG_MONTH_NAMES = MONTH_NAMES_LONG(MONTH)
         ENDIF
      RETURN
C***********************************************************************
      ENTRY DAY_NAME(DAY_TYPE)
C***********************************************************************
         DAY_NAME = DAY_TYPE_NAME(DAY_TYPE)
      RETURN
C***********************************************************************
      ENTRY MONTH_YEAR_FORMATT(R_MO,R_YR)
C***********************************************************************
         TEMP_YR = BASE_YEAR()+R_YR-2000
         WRITE(TEMP_YR_STR,'(I2.2)') TEMP_YR
         MONTH_YEAR_FORMATT = MONTH_CAP_NAMES_SHORT(R_MO)//TEMP_YR_STR
      RETURN
C***********************************************************************
      ENTRY DATE_12_MOS_ENDING(R_MO,R_YR)
C***********************************************************************
         TEMP_YR = BASE_YEAR()+R_YR
         WRITE(TEMP_YR_STR_I4,'(I4)') TEMP_YR
         DATE_12_MOS_ENDING=MONTH_CAP_NAMES_SHORT(R_MO)//TEMP_YR_STR_I4
      END
C***********************************************************************
      FUNCTION GET_MONTH_NUMBER(MONTHLY_LAST_MONTH_STR)
C***********************************************************************
C
      INTEGER*2 GET_MONTH_NUMBER 
      CHARACTER*(*) MONTHLY_LAST_MONTH_STR
C
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
C***********************************************************************
      SUBROUTINE MAKE_ALL_BINARY
C***********************************************************************
C
      USE BASE_FILE_NAMES_FROM_BFIL_OBJ  ! SOURCE IN MODULES95
      INCLUDE 'SPINLIB.MON'
      INCLUDE 'SIZECOM.MON'
      CHARACTER*1 UTILITY_TYPE
      LOGICAL (KIND=1) :: SYSTEM_BASED_FORECAST,
     +                    CLASS_BASED_FORECAST,
     +                    NERC_REGION_BASED_FORECAST,
     +                    WVPA
      LOGICAL (KIND=1) :: RUN_COAL_MODEL_ONLY
C
C      CALL LOCATE(16,9)
C      WRITE(6,"('&',A)") 'Input file overhead'
      CALL MG_LOCATE_WRITE(16,9,'Input file overhead',ALL_VERSIONS,1)
C***********************************************************************
C
C
C THESE MAKEBINs AND DYNAMIC VECTOR ALLOCATIONS FOR CP&L MUST BE RUN BEFORE
C  THE ASSET VECTOR ROUTINES 4/7/99 MSG
C
C 11/01/18 CONVERT THE RPS PROGRAMS FILE TO BINARY
C
      CALL RPS_PROGRAM_MAKEBIN()

C
C COAL MODEL FILES
C      
      CALL COAL_BASIN_MAKEBIN(COAL_NODE)
      CALL COAL_SUPPLY_MAKEBIN(COAL_SUPPLY_FORECAST)
      CALL COAL_TRANSPORTATION_LINKS_MAKEBIN(COAL_TRANSPORT,'HT')
      CALL PLANT_DEMAND_MAKEBIN(PLANT_DEMAND_FORECAST)
      CALL COAL_CONTRACTS_MAKEBIN(COAL_CONTRACTS)
      CALL COAL_ESCALATIONS_MAKEBIN(COAL_MODEL_POINTER_FILE)
      CALL COAL_SO2_INFO_MAKEBIN(COAL_MODEL_SO2_INFO_FILE)        ! 373
      CALL COAL_TRANSPORTATION_LINKS_MAKEBIN(
     +                           COAL_MODEL_GENERIC_TRANSPORT_FILE,'ZD')  ! 374
      CALL GENERIC_DEMAND_MAKEBIN(COAL_MODEL_GENERIC_DEMAND_FILE)  ! 375
      CALL PP_MAKEBIN ! THIS MUST STAY IN FRONT OF CL_MAKEBIN
C
C CPL CONTRACT 
C
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
C
      IF(UTILITY_TYPE() == 'P') THEN
         CALL MUNI_PF_MAKEBIN   
C
C INITIALIZATION FILE
C
         CALL MUNI_IN_MAKEBIN
      ELSE
         CALL PF_MAKEBIN   
C
C INITIALIZATION FILE
C
         CALL IN_MAKEBIN
         CALL ADDENDUMS_MAKEBIN
      ENDIF
      CALL DISPLAY_TIME
C
C
C CONVERT MONTHLY PAYABLES FILE
      CALL MONTHLY_PAYABLES_MAKEBIN
C
      CALL AR_MAKEBIN
C
C
C CONVERT FUTURE ASSET COMMITTED FILE TO BINARY
C
      CALL FA_MAKEBIN
C
C CONVERT GENERATING UNIT PARAMETER FILE TO BINARY AND RANDOM ACCESS
C NOTE THIS MUST BE BEFORE THE CL_MAKEBIN ROUTINE
      CALL UP_MAKEBIN
      CALL OPEN_VAR_UNITS_FILE
C
C CONVERT THE FOSSIL FILE
C
      CALL CL_MAKEBIN
C
C CONVERT FUTURE ASSET EXPANSION FILE TO BINARY
C
      CALL FN_MAKEBIN
C
C CONVERT THE EXPANSION PLANT FILE TO BINARY
C
      CALL PN_MAKEBIN
C
C CONVERSION OF THE EXPANSION CANIDATE FILE
C
      CALL CN_MAKEBIN
C
C CONVERSION OF THE ENERGY LIMITED RESOURECE FILE
C
      CALL EL_MAKEBIN
      CALL DISPLAY_TIME
C
C  CONVERT DEBT FILE TO BINARY BASE CASE FORM
C
      CALL DB_MAKEBIN
C
C  EXISTING ASSET FILE CONVERSION
C
      CALL EA_MAKEBIN
C
C  CONVERT DEBIT FILES
C
      CALL DD_MAKEBIN
C
C CONVERT EXPENSE FILE
C
      CALL EX_MAKEBIN
C
C CONVERT NUCLEAR FUEL FILE TO BINARY
C
      CALL NF_MAKEBIN
C
C CONVERT TAX LOSS FILE TO BINARY
C
      CALL TX_MAKEBIN
      CALL DISPLAY_TIME
C
C***********************************************************************
C
C MAKEBIN2 SECTION
C
C
C CONVERT THE LOAD MANAGEMENT APPLICATION FILE
C
      CALL LG_MAKEBIN
C
C CONVERT THE LOAD MANAGEMENT DEVICE FILE
C
      CALL LS_MAKEBIN
C
C CONVERT LOAD MANAGEMENT FINANCIAL FILE TO BINARY
C
      CALL LF_MAKEBIN
C
C OPEN THE ECONOMY INTERCHANGE DATA FILE
C
      CALL EC_MAKEBIN
C
C OPEN THE ESCALATION RATE DATA FILE
C
      CALL ES_MAKEBIN
C
C OPEN THE SYSTEM FORECAST DATA FILE
C
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
C ENDIF SYSTEM BASED FORECAST
C
C***********************************************************************
C
C OLD MAKEBIN3 SECTION
C
C CONVERT COST ALLOCATION FILE TO BINARY
C
      CALL CO_MAKEBIN
C
C CONVERT ELASTICITY FILE TO BINARY
C
      CALL FP_MAKEBIN
C
C FINANICAL RUN SPECS 
C
C     CALL RS_MAKEBIN
      CALL CLASS_RUN_SWITCHES_MAKEBIN
      CALL EM_MAKEBIN
C
C DETAILED REPORTS 
C
      CALL DE_MAKEBIN
      CALL DISPLAY_TIME
C
C CATAWBA CONTRACT 
C
      CALL CAT2_MAKEBIN
C
C RDI POWERDAT
C
      CALL RDI_MAKEBIN
C
C RDI BASE CASE
C
      CALL RBC_MAKEBIN
C
C TRANSACTION SYSTEM FORECAST
C
      CALL TF_MAKEBIN
C
C TRANSACTION GROUPS
C
      CALL TG_MAKEBIN
C
C TRANSACTION CONSTRAINT 4/11/98. GAT.
C
      CALL CONSTRAINT_MAKEBIN
C
C TRANSACTION PATH 4/1/98. GAT.
C
      CALL PATH_MAKEBIN
C
C TRANSACT TIE 
C
      CALL TIE_MAKEBIN
      CALL DISPLAY_TIME
C
C DAY TYPE 
C
      CALL DAY_TYPE_MAKEBIN
C
C USER DAY  
C
      CALL USER_DAY_MAKEBIN
C
C ICAP  
C
      CALL ICAP_MAKEBIN
C
C UNIT OUTAGES 
C
      CALL OUTAGE_MAKEBIN
C
C SCENARIO MAKER
C
      CALL SCENARIO_MAKER_MAKEBIN
C
C REGIONAL OUTAGES
C      
!      CALL REGIONAL_OUTAGES_MAKEBIN
C
C REGIONAL PARAMETER
C      
!      CALL REGIONAL_PARAMETER_MAKEBIN
C
C REGIONAL OUTAGE PARAMETER
C      
      CALL REG_OUT_PARAM_MAKEBIN
C
C FUEL DERIVATIVES
C      
      CALL FUEL_DERIVATIVES_MAKEBIN
C
C ENERGY PRODUCTS
C
      CALL ENERGY_PRODUCTS_MAKEBIN
C
C MARKET PRICE
C
      CALL MARKET_PRICE_MAKEBIN
      CALL DISPLAY_TIME
C
C NEW MARKET PRICE
C
      CALL NEW_MARKET_PRICE_MAKEBIN
C
C LH_GLOBAL
C
      CALL LH_GLOBAL_MAKEBIN
C
C ELECT_HOUR_MULT
C
      CALL ELECT_HOUR_MULT_MAKEBIN
C
C GAS_HOUR_MULT
C
      CALL GAS_HOUR_MULT_MAKEBIN
C
C OIL_HOUR_MULT
C
      CALL OIL_HOUR_MULT_MAKEBIN
C
C SCENARIO HOURLY
C
      CALL SCENARIO_HOURLY_MAKEBIN
C
C WEEKLY HYDRO
C
      CALL WEEKLY_HYDRO_MAKEBIN
C
C RPS REQUIREMENTS
C
      CALL RPS_REQUIREMENTS_MAKEBIN
C
C RPS STATE DEMAND
C
      CALL RPS_STATE_DEMAND_MAKEBIN
C
C RETROFIT REQUIREMENTS
C
      CALL THERMAL_RETROFIT_MAKEBIN
C
C CO2 NATIONAL COMPLIANCE
C
      CALL CO2_PARAM_MAKEBIN
C
C WEATHER DEMAND
C
      CALL WEATHER_DEMAND_MAKEBIN
C
C MARKET_GROUPS
C
      CALL MK_MAKEBIN
C
C REFERENCE LOAD
C
      CALL REFERENCE_LOAD_MAKEBIN
C
C CAPACITY TARGET RATIOS
C
      CALL CR_MAKEBIN
C
C CONVERT ENVIRONMENTAL FILE TO BINARY
C
      CALL EN_MAKEBIN
C
C CONVERT PURCHASE CONTRACTS FILE TO BINARY AND RANDOM ACCESS
C
      CALL CT_MAKEBIN
      CALL DISPLAY_TIME
!
      CALL TRAN_EXP_MAKEBIN
!
C
C CONVERT GAS MODEL FILES
C
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
C
C***********************************************************************
C
C
      CALL AA_MAKEBIN
C
C CONVERT FUEL INVENTORY FILE
C
      CALL FI_MAKEBIN
C
C CONVERT FUEL PRICE FILE
C
      CALL FC_MAKEBIN
C
C CONVERT TRANSACTIONS FILE
C
      CALL TR_MAKEBIN
C
C CONVERT FUEL EMISSIONS FILE
C
      CALL FE_MAKEBIN
C
C***********************************************************************
C
      CALL CLOSE_ASSET_VECTOR_FILE
      CALL DISPLAY_TIME
      CALL MG_CLEAR_LINE(17,9,36,0,0)
      CALL MG_CLEAR_LINE(16,9,36,0,0)
      RETURN
      END   
C***********************************************************************
      SUBROUTINE MAKE_CAPEX_BINARY
C***********************************************************************
C
      INCLUDE 'SPINLIB.MON'
      INCLUDE 'SIZECOM.MON'
      CHARACTER*1 UTILITY_TYPE
      LOGICAL*1 SYSTEM_BASED_FORECAST,
     +          CLASS_BASED_FORECAST,
     +          NERC_REGION_BASED_FORECAST,
     +          WVPA
C
C      CALL LOCATE(16,9)
C      WRITE(6,"('&',A)") 'Input file overhead'
      CALL MG_LOCATE_WRITE(16,9,'Input file overhead',ALL_VERSIONS,1)
C***********************************************************************
C
C
C THESE MAKEBINs AND DYNAMIC VECTOR ALLOCATIONS FOR CP&L MUST BE RUN BEFORE
C  THE ASSET VECTOR ROUTINES 4/7/99 MSG
C
C CPL CONTRACT 
C
      CALL FV_MAKEBIN
      CALL OPEN_ASSET_VECTOR_FILE(82)
      CALL PP_MAKEBIN ! THIS MUST STAY IN FRONT OF CL_MAKEBIN
      IF(UTILITY_TYPE() == 'P') THEN
C         CALL MUNI_PF_MAKEBIN
C
C INITIALIZATION FILE
C
         CALL MUNI_IN_MAKEBIN
      ELSE
C         CALL PF_MAKEBIN
C
C INITIALIZATION FILE
C
         CALL IN_MAKEBIN
      ENDIF
      CALL DISPLAY_TIME
C
C CONVERT GENERATING UNIT PARAMETER FILE TO BINARY AND RANDOM ACCESS
C NOTE THIS MUST BE BEFORE THE CL_MAKEBIN ROUTINE
      CALL UP_MAKEBIN
      CALL OPEN_VAR_UNITS_FILE
C
C CONVERT THE FOSSIL FILE
C
      CALL CL_MAKEBIN
C
C CONVERSION OF THE ENERGY LIMITED RESOURECE FILE
C
C      CALL EL_MAKEBIN
      CALL DISPLAY_TIME
C
C CONVERT NUCLEAR FUEL FILE TO BINARY
C
C      CALL NF_MAKEBIN
C
C***********************************************************************
C
C MAKEBIN2 SECTION
C
C
C CONVERT THE LOAD MANAGEMENT APPLICATION FILE
C
C      CALL LG_MAKEBIN
C
C CONVERT THE LOAD MANAGEMENT DEVICE FILE
C
C      CALL LS_MAKEBIN
C
C CONVERT LOAD MANAGEMENT FINANCIAL FILE TO BINARY
C
C      CALL LF_MAKEBIN
C
C OPEN THE ESCALATION RATE DATA FILE
C
      CALL ES_MAKEBIN
C
C OPEN THE SYSTEM FORECAST DATA FILE
C
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

C
C***********************************************************************
C
C OLD MAKEBIN3 SECTION
C
C CONVERT COST ALLOCATION FILE TO BINARY
C
      CALL CO_MAKEBIN
C
C CONVERT ELASTICITY FILE TO BINARY
C
      CALL FP_MAKEBIN
C
C FINANICAL RUN SPECS 
C
C     CALL RS_MAKEBIN
      CALL CLASS_RUN_SWITCHES_MAKEBIN
      CALL EM_MAKEBIN
C
C DETAILED REPORTS 
C
      CALL DE_MAKEBIN
      CALL DISPLAY_TIME
C
C TRANSACTION SYSTEM FORECAST
C
      CALL TF_MAKEBIN
C
C TRANSACTION GROUPS
C
      CALL TG_MAKEBIN
C
C TRANSACTION CONSTRAINT 4/11/98. GAT.
C
      CALL CONSTRAINT_MAKEBIN
C
C TRANSACTION PATH 4/1/98. GAT.
C
      CALL PATH_MAKEBIN
C
C TRANSACT TIE 
C
      CALL TIE_MAKEBIN
      CALL DISPLAY_TIME
C
C DAY TYPE 
C
      CALL DAY_TYPE_MAKEBIN
C
C USER DAY  
C
      CALL USER_DAY_MAKEBIN
C
C ICAP  
C
      CALL ICAP_MAKEBIN
C
C UNIT OUTAGES 
C
      CALL OUTAGE_MAKEBIN
C
C SCENARIO MAKER
C
      CALL SCENARIO_MAKER_MAKEBIN
C
C REGIONAL OUTAGE PARAMETER
C      
      CALL REG_OUT_PARAM_MAKEBIN
C
C FUEL DERIVATIVES
C      
      CALL FUEL_DERIVATIVES_MAKEBIN
C
C ENERGY PRODUCTS
C
      CALL ENERGY_PRODUCTS_MAKEBIN
C
C MARKET PRICE
C
      CALL MARKET_PRICE_MAKEBIN
      CALL DISPLAY_TIME
C
C NEW MARKET PRICE
C
      CALL NEW_MARKET_PRICE_MAKEBIN
C
C LH_GLOBAL
C
      CALL LH_GLOBAL_MAKEBIN
C
C ELECT_HOUR_MULT
C
      CALL ELECT_HOUR_MULT_MAKEBIN
C
C GAS_HOUR_MULT
C
      CALL GAS_HOUR_MULT_MAKEBIN
C
C OIL_HOUR_MULT
C
      CALL OIL_HOUR_MULT_MAKEBIN
C
C SCENARIO HOURLY
C
      CALL SCENARIO_HOURLY_MAKEBIN
C
C WEEKLY HYDRO
C
      CALL WEEKLY_HYDRO_MAKEBIN
C
C WEATHER DEMAND
C
      CALL WEATHER_DEMAND_MAKEBIN
C
C MARKET_GROUPS
C
      CALL MK_MAKEBIN
C
C REFERENCE LOAD
C
      CALL REFERENCE_LOAD_MAKEBIN
C
C CAPACITY TARGET RATIOS
C
      CALL CR_MAKEBIN
C
C CONVERT ENVIRONMENTAL FILE TO BINARY
C
      CALL EN_MAKEBIN
C
C CONVERT PURCHASE CONTRACTS FILE TO BINARY AND RANDOM ACCESS
C
      CALL CT_MAKEBIN
      CALL DISPLAY_TIME
C
C***********************************************************************
C
C
      CALL AA_MAKEBIN
C
C CONVERT FUEL INVENTORY FILE
C
      CALL FI_MAKEBIN
C
C CONVERT FUEL PRICE FILE
C
      CALL FC_MAKEBIN
C
C CONVERT TRANSACTIONS FILE
C
      CALL TR_MAKEBIN
C
C CONVERT FUEL EMISSIONS FILE
C
      CALL FE_MAKEBIN
C
C***********************************************************************
C
      CALL CLOSE_ASSET_VECTOR_FILE
      CALL DISPLAY_TIME
      CALL MG_CLEAR_LINE(17,9,36,0,0)
      CALL MG_CLEAR_LINE(16,9,36,0,0)
      RETURN
      END   
C *********************************************************************
      SUBROUTINE DELETE_RESULTS_OF_PREVIOUS_RUN() 
C *********************************************************************      
C
      use cl_data
      implicit none
      CHARACTER*256 BASE_FILE_DIRECTORY,FILE_NAME,GET_RESULTS_DIRECTORY
      LOGICAL FILE_EXISTS
      logical*1 LAHEY_LF95,WVPA

         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"BC*.BIN"
         CALL ERASEWC(FILE_NAME)
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"OL*.BIN"
         CALL ERASEWC(FILE_NAME)

C ERASE ALL DETAILED REPORT FILES
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                                     trim(cldata%SCENAME)//".??D"
         CALL ERASEWC(FILE_NAME)

            FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"WVP"//
     +                                      trim(cldata%SCENAME)//".??D"
            CALL ERASEWC(FILE_NAME)
c
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//'MSG'//
     +                                      trim(cldata%SCENAME)//'.STR'
         INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
         IF(FILE_EXISTS) CALL ERASE(FILE_NAME)
C
      RETURN
      END SUBROUTINE DELETE_RESULTS_OF_PREVIOUS_RUN
C***********************************************************************
       SUBROUTINE SET_ASSET_CLASSES(R_ASSET_CLASS_NUM,
     +                             NUMBER_OF_ASSET_CLASSES,
     +                              MAX_CLASS_ID_NUM,
     +                             TEMP_ASSET_CLASS_POINTER)
      use end_routine, only: end_program, er_message
C***********************************************************************
C USE TO DETERMINE THE NUMBER OF DEFINED ASSET CLASS IN EACH DATA
C TYPE.  CALLED FROM MAKEBIN AND MAKEOVLS 11/30/94
C
      INCLUDE 'SpinLib.MON'
      INCLUDE 'SIZECOM.MON'
      INTEGER*2 ASSET_CLASS_NUM,PARENT_CLASS_ID_NUM,
     +          NUMBER_OF_ASSET_CLASSES,
     +           MAX_CLASS_ID_NUM,
     +          TEMP_ASSET_CLASS_POINTER(*),
     +          R_ASSET_CLASS_NUM,VECTOR_PTR
C
      REAL ASSET_CLASS_LIST(1024)
      REAL*4 VECTOR_ASSET_CLASSES(0:AVAIL_DATA_YEARS)
      REAL*4 VECTOR_CLASSES(AVAIL_DATA_YEARS)
      INTEGER*2 I,ASSET_CLASS
      INTEGER*2 VECTOR_POINTER,CLASS_POINTER,LINKED_CLASS
      CHARACTER*1 DUMMY_TYPE
C
      INTEGER*2 R_ALLOCATION_VECTOR,
     +          R_ASSET_CLASS,
     +          R_CLASS_ID_LIST(*)
      REAL*4 R_ASSET_ALLOCATION_LIST(*)      
C
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
C
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
C
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
C***********************************************************************
      ENTRY CREATE_CLASS_ID_ALLOCATION_LIST(R_ASSET_CLASS,
     +                                      R_ALLOCATION_VECTOR,
     +                                      R_CLASS_ID_LIST,
     +                                      R_ASSET_ALLOCATION_LIST)      
C***********************************************************************
C
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
C
      RETURN
       END
C *********************************************************************      
       SUBROUTINE SET_INIT_CLASSES(R_ASSET_CLASS_NUM,
     +                             NUMBER_OF_ASSET_CLASSES,
     +                              MAX_CLASS_ID_NUM,
     +                             TEMP_ASSET_CLASS_POINTER)
C *********************************************************************      
C USE TO DETERMINE THE NUMBER OF DEFINED ASSET CLASS IN EACH DATA
C TYPE.  CALLED FROM MAKEBIN AND MAKEOVLS 11/30/94
C
      INCLUDE 'SpinLib.MON'
      INCLUDE 'SIZECOM.MON'
      INTEGER*2 ASSET_CLASS_NUM,R_ASSET_CLASS_NUM,
     +          NUMBER_OF_ASSET_CLASSES,
     +           MAX_CLASS_ID_NUM,
     +          TEMP_ASSET_CLASS_POINTER(*)
C
      REAL ASSET_CLASS_LIST(1024)
      REAL*4 VECTOR_ASSET_CLASSES(0:AVAIL_DATA_YEARS),
     +       VECTOR_CLASSES(AVAIL_DATA_YEARS)
      INTEGER*2 I,ASSET_CLASS
      INTEGER*2 VECTOR_POINTER,CLASS_POINTER,LINKED_CLASS
      CHARACTER*1 DUMMY_TYPE
C
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
C
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
C
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
C *********************************************************************      
      FUNCTION INIT_CREATE_CLASS_LIST(INIT_TABLE_POINTER)
C *********************************************************************      
C
      INCLUDE 'SpinLib.MON'
      INCLUDE 'SIZECOM.MON'
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
C
         ALLOCATE(TABLE_POSITION_FOR(1024),
     +            ASSET_CLASS_LIST(AVAIL_DATA_YEARS))
         TABLE_POSITION_FOR = INIT_TABLE_POINTER
         TABLE_NUMBER = 0
         MAX_CLASS_NUMBER = 0
         SAVE_INIT_TABLE_POINTER = INIT_TABLE_POINTER
         INIT_CREATE_CLASS_LIST = .TRUE.
      RETURN
C         
C *********************************************************************      
      ENTRY CREATE_CLASS_LIST(R_CLASS_NUM)
C *********************************************************************      
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
C *********************************************************************      
      ENTRY INCREMENT_TABLE_NUMBER
C *********************************************************************      
         TABLE_NUMBER = TABLE_NUMBER + 1
         INCREMENT_TABLE_NUMBER = TABLE_NUMBER
      RETURN
C *********************************************************************      
      ENTRY MAX_CLASS_IN_TABLE
C *********************************************************************      
         MAX_CLASS_IN_TABLE = MAX_CLASS_NUMBER
      RETURN
C *********************************************************************      
      ENTRY PASS_TABLE_POINTER(R_TABLE_POSITION_FOR)
C *********************************************************************      
         R_TABLE_POSITION_FOR(0) = SAVE_INIT_TABLE_POINTER
         DO I = 1, MAX_CLASS_NUMBER
            R_TABLE_POSITION_FOR(I) = TABLE_POSITION_FOR(I)   
         ENDDO
         DEALLOCATE(ASSET_CLASS_LIST,TABLE_POSITION_FOR)
         PASS_TABLE_POINTER = TABLE_NUMBER
      RETURN   
      END
C***********************************************************************
C
C ROUTINE WILL THE MESSAGES IN IT 
C
C***********************************************************************
      SUBROUTINE ERROR_AND_WARNING_MSG
      use end_routine, only: end_program, er_message
      INCLUDE 'SPINLIB.MON'
      INCLUDE 'SIZECOM.MON'
      CHARACTER*(*) FILE_TYPE,FILE_NAME
      
C
C***********************************************************************
      ENTRY STOP_NOFILE(FILE_TYPE,FILE_NAME)
C***********************************************************************

         CALL MG_LOCATE_WRITE(20,0,'The '//trim(FILE_TYPE)//
     +             ' base file '//trim(FILE_NAME)//' does not exist.',
     +                                                   ALL_VERSIONS,1)
      er_message= trim(file_name) // ' missing at MIDASMOD SIID200 '
      call end_program(er_message)
      END
C *********************************************************************      
      FUNCTION LOAD_FILE_CHAR_EXT(INT_EXTENSION)
C *********************************************************************      
C
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
C *********************************************************************      
      FUNCTION AREA_CHAR_EXT(INT_EXTENSION)
C *********************************************************************      
C
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
C *********************************************************************      
      FUNCTION SCEN_FILE_CHAR_EXT(R_INT_EXTENSION)
C *********************************************************************      
C
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
C***********************************************************************
      FUNCTION FINANCIAL_DRILLING_HEADER()
C***********************************************************************
      USE DRILLING_REPT_PARAMETERS
      USE SIZE_ARRAY_DIMENSIONS  ! makes MAX_SIMULATION_YEARS dynamic 11/08/06 Dr. G
      USE IREC_ENDPOINT_CONTROL
      use logging
      use dsex_data
      INCLUDE 'GLOBECOM.MON'
      INCLUDE 'SIZECOM.MON'
      INCLUDE 'DREPTCOM.MON'
      INCLUDE 'NAMESCOM.MON'
      INCLUDE 'MTHNMCOM.MON'
C
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
C
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
C
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
C
         IF(FINANCIAL_DRILLING() == 'B' .OR.
     +                                 FINANCIAL_DRILLING() == 'M') THEN
            REPORTING_YEARS = MAX(5,RUN_YEARS()+EXTENSION_YEARS())
         ELSE
            REPORTING_YEARS = RUN_YEARS() + EXTENSION_YEARS()
         ENDIF
C
C OPEN THE MONTHLY OUTPUT REPORT
C
         IF(MONTHLY_MIDAS_ACTIVE) THEN
            FIRST_YEAR = BASE_YEAR + 1
            DIMENSIONS = 4
            
            FILE_VARIABLES = INCOME_VARS + 1
            OVERHEAD_LENGTH = 4 + 4 + 38 + Period Width
C
C MONTHLY INCOME STATEMENT
C
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
C
            WRITE(REPORTING_UNIT_NUMBER,REC=START_REC)
     +                        'Endpoint            ','N',INT2(4),'V','D'
            WRITE(REPORTING_UNIT_NUMBER,REC=START_REC+1) 
     +                        'Year                ','N',INT2(4),'F','D'
            WRITE(REPORTING_UNIT_NUMBER,REC=START_REC+2)
     +                       'Asset Class Name    ','C',INT2(38),'F','D'
            WRITE(REPORTING_UNIT_NUMBER,REC=START_REC+3) 
     +                   'Period              ','C',Period Width,'V','D'
            MONTHLY_INCOME_REC = START_REC+4
C
C MONTHLY CASH REPORT
C          
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
C
            WRITE(REPORTING_UNIT_NUMBER,REC=START_REC)
     +                        'Endpoint            ','N',INT2(4),'V','D'
            WRITE(REPORTING_UNIT_NUMBER,REC=START_REC+1) 
     +                        'Year                ','N',INT2(4),'F','D'
            WRITE(REPORTING_UNIT_NUMBER,REC=START_REC+2)
     +                       'Asset Class Name    ','C',INT2(38),'F','D'
            WRITE(REPORTING_UNIT_NUMBER,REC=START_REC+3) 
     +                   'Period              ','C',Period Width,'V','D'
            MONTHLY_CASH_REC = START_REC+4
C
C MONTHLY BALANCE SHEET
C          
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
C
            WRITE(REPORTING_UNIT_NUMBER,REC=START_REC)
     +                        'Endpoint            ','N',INT2(4),'V','D'
            WRITE(REPORTING_UNIT_NUMBER,REC=START_REC+1) 
     +                        'Year                ','N',INT2(4),'F','D'
            WRITE(REPORTING_UNIT_NUMBER,REC=START_REC+2)
     +                       'Asset Class Name    ','C',INT2(38),'F','D'
            WRITE(REPORTING_UNIT_NUMBER,REC=START_REC+3) 
     +                   'Period              ','C',Period Width,'V','D'
            MONTHLY_BALANCE_REC = START_REC+4
C
C MONTHLY TAX DETAIL REPORT
C
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
C
            WRITE(REPORTING_UNIT_NUMBER,REC=START_REC)
     +                        'Endpoint            ','N',INT2(4),'V','D'
            WRITE(REPORTING_UNIT_NUMBER,REC=START_REC+1) 
     +                        'Year                ','N',INT2(4),'F','D'
            WRITE(REPORTING_UNIT_NUMBER,REC=START_REC+2)
     +                       'Asset Class Name    ','C',INT2(38),'F','D'
            WRITE(REPORTING_UNIT_NUMBER,REC=START_REC+3) 
     +                   'Period              ','C',Period Width,'V','D'
            MONTHLY_TAX_REC = START_REC+4
C
C MONTHLY CP&L TAX REPORT
C     
            FINANCIAL_DRILLING_CODE = CPL_TAX_REPORTING_CODE
            FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"CPL"//
     +                    trim(GET_SCENAME())//FINANCIAL_DRILLING_CODE
C            CALL ERASE(FILE_NAME)
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
C
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
C
C OPEN THE THREE DRILLING FILES
C 
         FINANCIAL_DRILLING_HEADER = FINANCIAL_INCOME_DRILLING_NO
C        INQUIRE(UNIT=FINANCIAL_DRILLING_NO,OPEN=FILE_IS_OPEN)
         IF(DRILLING_FILE_IS_OPEN .OR.
     +                               FINANCIAL_DRILLING() == 'O') RETURN
C
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
C
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
C
            WRITE(REPORTING_UNIT_NUMBER,REC=NEXT_REC)
     +                        'Endpoint            ','N',INT2(4),'V','D'
            NEXT_REC = NEXT_REC + 1
            IF(I == 3) THEN
               WRITE(REPORTING_UNIT_NUMBER,REC=NEXT_REC)
     +                       'Reporting Variable  ','C',INT2(30),'V','D'
               NEXT_REC = NEXT_REC + 1
               IF(FINANCIAL_DRILLING() == 'B' .OR.
     +                                 FINANCIAL_DRILLING() == 'C') THEN
C                 WRITE(REPORTING_UNIT_NUMBER,REC=NEXT_REC)
C    +                       'Asset Group         ','C',INT2(30),'V','D'
C                 NEXT_REC = NEXT_REC + 1
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
C                 WRITE(REPORTING_UNIT_NUMBER,REC=NEXT_REC)
C    +                       'Asset Group         ','C',INT2(30),'V','D'
C                 NEXT_REC = NEXT_REC + 1
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
C
         DRILLING_FILE_IS_OPEN = .TRUE.
      RETURN
C***********************************************************************
      ENTRY MONTHLY_INCOME_UNIT_NO
C***********************************************************************
         MONTHLY_INCOME_UNIT_NO = FINANCIAL_MONTHLY_INCOME_NO
      RETURN
C***********************************************************************
      ENTRY MONTHLY_CASH_UNIT_NO
C***********************************************************************
         MONTHLY_CASH_UNIT_NO = FINANCIAL_MONTHLY_CASH_NO
      RETURN
C***********************************************************************
      ENTRY MONTHLY_BALANCE_UNIT_NO
C***********************************************************************
         MONTHLY_BALANCE_UNIT_NO = FINANCIAL_MONTHLY_BALANCE_NO
      RETURN
C***********************************************************************
      ENTRY MONTHLY_TAX_UNIT_NO
C***********************************************************************
         MONTHLY_TAX_UNIT_NO = FINANCIAL_MONTHLY_TAX_NO
      RETURN
C***********************************************************************
      ENTRY CPL_TAX_REPORTING_UNIT_NO
C***********************************************************************
         CPL_TAX_REPORTING_UNIT_NO = CPL_TAX_REPORTING_NO
      RETURN
C***********************************************************************
      ENTRY INCOME_DRILLING_UNIT_NO
C***********************************************************************
         INCOME_DRILLING_UNIT_NO = FINANCIAL_INCOME_DRILLING_NO
      RETURN
C***********************************************************************
      ENTRY CASH_DRILLING_UNIT_NO
C***********************************************************************
         CASH_DRILLING_UNIT_NO = FINANCIAL_CASH_DRILLING_NO
      RETURN
C***********************************************************************
      ENTRY BALANCE_DRILLING_UNIT_NO
C***********************************************************************
         BALANCE_DRILLING_UNIT_NO = FINANCIAL_BALANCE_DRILLING_NO
      RETURN
C***********************************************************************
      ENTRY TAX_DRILLING_UNIT_NO
C***********************************************************************
         TAX_DRILLING_UNIT_NO = FINANCIAL_TAX_DRILLING_NO
      RETURN
C***********************************************************************
C COPYRIGHT (C) 1998 M.S. GERBER & ASSOCIATES, INC. ALL RIGHTS RESERVED
C***********************************************************************
      ENTRY WRITE_DRILLING_RPT(DRILLING_NAME,
     +                         ACCOUNT_NAME,
     +                         MONTHLY_DATA,
     +                         DRILLING_REPORT,
     +                         ANNUAL_REPORT)
C***********************************************************************
C
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
C
         WRITE_DRILLING_RPT = FINANCIAL_SIMULATION_YEARS
C
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
C***********************************************************************
      ENTRY INCOME_DRILLING_REPRT_REC 
C***********************************************************************
         INCOME_DRILLING_REPRT_REC = DRILLING_INCOME_REC
         DRILLING_INCOME_REC = DRILLING_INCOME_REC + 1
      RETURN
C***********************************************************************
      ENTRY CASH_DRILLING_REPRT_REC 
C***********************************************************************
         CASH_DRILLING_REPRT_REC = DRILLING_CASH_REC
         DRILLING_CASH_REC = DRILLING_CASH_REC + 1
      RETURN
C***********************************************************************
      ENTRY BALANCE_DRILLING_REPRT_REC 
C***********************************************************************
         BALANCE_DRILLING_REPRT_REC = DRILLING_BALANCE_SHEET_REC
         DRILLING_BALANCE_SHEET_REC = DRILLING_BALANCE_SHEET_REC + 1
      RETURN
C***********************************************************************
      ENTRY TAX_DRILLING_REPRT_REC
C***********************************************************************
         TAX_DRILLING_REPRT_REC = DRILLING_TAX_REC
         DRILLING_TAX_REC = DRILLING_TAX_REC + 1
      RETURN
C***********************************************************************
C COPYRIGHT (C) 1998 M.S. GERBER & ASSOCIATES, INC. ALL RIGHTS RESERVED
C***********************************************************************
      ENTRY RETURN_DRILLING_REPORT_STATUS(R_DATA_NOT_IN_CASH_DRILLING,
     +                                   R_DATA_NOT_IN_BALANCE_DRILLING,
     +                                   R_DATA_NOT_IN_INCOME_DRILLING,
     +                                   R_DATA_NOT_IN_TAX_DRILLING)
C***********************************************************************
C
         RETURN_DRILLING_REPORT_STATUS = 1
         R_DATA_NOT_IN_CASH_DRILLING = DATA_NOT_IN_CASH_DRILLING
         R_DATA_NOT_IN_BALANCE_DRILLING = DATA_NOT_IN_BALANCE_DRILLING
         R_DATA_NOT_IN_INCOME_DRILLING = DATA_NOT_IN_INCOME_DRILLING
         R_DATA_NOT_IN_TAX_DRILLING = DATA_NOT_IN_TAX_DRILLING
      RETURN
C***********************************************************************
      ENTRY SET_DRILLING_DATA_INCOME_TRUE         
C***********************************************************************
C
         DATA_NOT_IN_INCOME_DRILLING = .FALSE.
         SET_DRILLING_DATA_INCOME_TRUE = .TRUE.
      RETURN
C***********************************************************************
      ENTRY SET_DRILLING_DATA_TAX_TRUE
C***********************************************************************
C
         DATA_NOT_IN_TAX_DRILLING = .FALSE.
         SET_DRILLING_DATA_TAX_TRUE = .TRUE.
      RETURN
C***********************************************************************
      ENTRY SET_DRILLING_DATA_BALANCE_TRUE
C***********************************************************************
C
         DATA_NOT_IN_BALANCE_DRILLING = .FALSE.
         SET_DRILLING_DATA_BALANCE_TRUE = .TRUE.
      RETURN
C***********************************************************************
      ENTRY SET_DRILLING_DATA_CASH_TRUE
C***********************************************************************
C
         DATA_NOT_IN_CASH_DRILLING = .FALSE.
         SET_DRILLING_DATA_CASH_TRUE = .TRUE.
      RETURN
      END
C***********************************************************************
      FUNCTION ESTABLISH_DRILLING_REPORT_FILE(FILE_NAME,OVERHEAD_LENGTH,
     +                                        FILE_VARIABLES,
     +                                        DIMENSIONS,FIRST_YEAR,
     +                                        REPORTING_UNIT_NUMBER,
     +                                        NEXT_REC)
C***********************************************************************
C
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
C
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
C         
            OPEN(REPORTING_UNIT_NUMBER,FILE=FILE_NAME,
     +                                ACCESS="TRANSPARENT",STATUS="OLD")
            WRITE(REPORTING_UNIT_NUMBER,REC=4) DIMENSIONS,FIRST_YEAR,
     +                                MIN(ENDYR(),FIRST_YEAR+INT2(4)),
     +                                GET_NUM_OF_END_POINTS()
            CLOSE(REPORTING_UNIT_NUMBER)
C         
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
C***********************************************************************
      SUBROUTINE CREATE_DOLLAR_STREAM(YEARS,DATA_TYPE,ESCAL_VECT,
     +                                VALUES_IN,VALUES_OUT)
C***********************************************************************
C
      USE SIZE_ARRAY_DIMENSIONS  ! makes MAX_SIMULATION_YEARS dynamic 11/08/06 Dr. G
      INCLUDE 'GLOBECOM.MON'
      INCLUDE 'SIZECOM.MON'
      INTEGER*2 YEARS,ESCAL_VECT,I,YR_2_CHANGE_2_VECTOR
!      REAL*4 VALUES_OUT(0:YEARS),VALUES_IN(0:YEARS)
      REAL*4 VALUES_OUT(0:*),VALUES_IN(0:*)
      CHARACTER*1 DATA_TYPE
      REAL*4 ESCALATION_RATES(AVAIL_DATA_YEARS),MULTIPLIER,RATE,
     +       ESCALATION_MULTIPLIER
      LOGICAL*1 VECTOR_TYPE,RETURN_A_VECTOR_FOR_ALL_YEARS
      REAL*4 ANNUAL_VALUES(*),MONTHLY_VALUES(12,5)
      INTEGER*2 YR,MO
C
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
C***********************************************************************
      ENTRY RIPPLE_MONTHLY_DATA(ANNUAL_VALUES,MONTHLY_VALUES)
C***********************************************************************
C
C
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
C***********************************************************************
      SUBROUTINE MONTHLY_FINANCIAL_DATABASE(R_CLASS_ID,
     +                                      R_ASSET_CLASS_NAME,
     +                                      R_INCOME_VARS)
C***********************************************************************
C
      USE IREC_ENDPOINT_CONTROL
      INCLUDE 'GLOBECOM.MON'
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
C      
         IF(YEAR > LAST_AVAILABLE_MONTHLY_YEAR) RETURN
         CLASS_ID = R_CLASS_ID
         ASSET_CLASS_NAME = R_ASSET_CLASS_NAME
         INCOME_ITEMS(:,YEAR,:) = R_INCOME_VARS(:,:)
         STORED_YEARS = STORED_YEARS + 1
      RETURN
C***********************************************************************
      ENTRY IPALCO_JURISDICTIONAL_RETAIL_REVENUES_REPORT(R_YR,
     +                                          CURRENT_CLASS_ID,
     +                                          R_INCOME_VARS,
     +                                          THIS_IS_REPORTING_CLASS)
C***********************************************************************
         R_INCOME_VARS(:,IPL Operating Revenues) = 
     +                 R_INCOME_VARS(:,Monthly Total Operating Revenues) 
     +                 - R_INCOME_VARS(:,Monthly Government Revenues)
     +                 - R_INCOME_VARS(:,Monthly Bulk Power)
         R_INCOME_VARS(:,Monthy Depreciation and Amortization) = 
     +                        R_INCOME_VARS(:,Monthly Book Depreciation) 
     +                        + R_INCOME_VARS(:,Monthly Amortization)
         R_INCOME_VARS(:,Monthy Taxes NonIncome) = 
     +                           R_INCOME_VARS(:,Monthly Property Taxes) 
     +                           + R_INCOME_VARS(:,Monthly Other Taxes)
c         R_INCOME_VARS(:,IPL Monthy OandM Expenses) =
c     +                           R_INCOME_VARS(:,Monthly Property Taxes)
c     +                           + R_INCOME_VARS(:,Monthly Other Taxes)
         DO MO = 0, 12 
            R_INCOME_VARS(MO,IPL Monthy OandM Expenses) =
     +                  MONTHLY_ATL_INCOME_STATEMENT_TAX_DEDUCTIONS(MO,
     +                                                    R_INCOME_VARS) 
     +                  + R_INCOME_VARS(MO,Monthly Vacation Pay)
     +                  + R_INCOME_VARS(MO,Monthly Pension Expense)
     +                  + R_INCOME_VARS(MO,Monthly Storm Expense)
         ENDDO
         IF(THIS_IS_REPORTING_CLASS)
     +                   CALL IPALCO_ELECT_COSTUMER_COSTS(R_YR,
     +                                                 CURRENT_CLASS_ID,
     +                                                 R_INCOME_VARS)
         R_INCOME_VARS(:,IPL Total Operating Expenses) = 
     +            R_INCOME_VARS(:,Monthy Depreciation and Amortization)
     +            + R_INCOME_VARS(:,IPL Monthy OandM Expenses) 
     +            + R_INCOME_VARS(:,Monthy Taxes NonIncome)
     +            + R_INCOME_VARS(:,Monthly Operating Revenue Tax)
     +            + R_INCOME_VARS(:,Monthly Federal ATL Income Tax Paid)
     +            + R_INCOME_VARS(:,Monthly State ATL Income Tax Paid)
     +            + R_INCOME_VARS(:,Monthly Income Tax Deferrals Cr)
     +            + R_INCOME_VARS(:,Monthly ITC Amortization)
         R_INCOME_VARS(:,IPL Operating Income) = 
     +                   R_INCOME_VARS(:,IPL Operating Revenues)
     +                   - R_INCOME_VARS(:,IPL Total Operating Expenses)
      RETURN
C***********************************************************************
      ENTRY MONTHLY_TRAILING_12_MONTHS_REVENUE_REPORT()
C***********************************************************************
c
c  read the base year 12 months
c
         IF(.NOT. IPL_TRAILING_12_MONTH_RPT_OPEN) THEN
             TRAILING_12_MOS_UNIT = 
     +               TRAILING_12_MOS_RPT_HEADER(TRAILING_12_MOS_RPT_REC)

            IPL_TRAILING_12_MONTH_RPT_OPEN = .TRUE. 
         ENDIF
C
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
C***********************************************************************
      ENTRY BASE_YEAR_ACTUAL_INCOME_DATA()
C***********************************************************************
c
         IF(END_POINT > 1) RETURN  ! THE ACTUAL DATA HAS BEEN READ
         IF(IPALCO()) THEN
            CALL IPALCO_BASE_YEAR_ACTUAL_INCOME_DATA(INCOME_ITEMS)
         ENDIF
      RETURN
C***********************************************************************
      ENTRY IPALCO_REPORTS()
C***********************************************************************
         CALL IPALCO_TRAILING_12_MONTHS_REVENUE_REPORT(CLASS_ID,
     +                                               ASSET_CLASS_NAME,
     +                                               SAVED_STORED_YEARS,
     +                                               INCOME_ITEMS)
      RETURN
      END SUBROUTINE
C***********************************************************************
      SUBROUTINE IPALCO_ELECT_COSTUMER_COSTS(R_YR,R_CLASS,R_INCOME_VARS)
C***********************************************************************
C
      INCLUDE 'MTHNMCOM.MON'
      LOGICAL (KIND=1) :: VOID_LOGICAL,TF_IPL_ELECTIC_PLAN_COST
      REAL (KIND=4) :: R_INCOME_VARS(0:12,INCOME_VARS)
      REAL (KIND=4) :: IPL_ELECTRIC_PLAN_FUEL_COSTS(0:12),
     +                 IPL_ELECTRIC_PLAN_PUR_COSTS(0:12)
      INTEGER (KIND=2) :: R_YR,R_CLASS
C
         VOID_LOGICAL = TF_IPL_ELECTIC_PLAN_COST(
     +                                     IPL_ELECTRIC_PLAN_FUEL_COSTS,
     +                                     IPL_ELECTRIC_PLAN_PUR_COSTS)
         CALL RETURN_MONTHLY_IPL_ELECT_PLAN(R_YR,R_CLASS,
     +                                     IPL_ELECTRIC_PLAN_FUEL_COSTS,
     +                                     IPL_ELECTRIC_PLAN_PUR_COSTS)
         R_INCOME_VARS(:,IPL Electric Plan Fuel Costs) =
     +                     R_INCOME_VARS(:,IPL Electric Plan Fuel Costs)
     +                     + IPL_ELECTRIC_PLAN_FUEL_COSTS(:)
         R_INCOME_VARS(:,IPL Electric Plan Purchase Costs) = 
     +                 R_INCOME_VARS(:,IPL Electric Plan Purchase Costs)
     +                 + IPL_ELECTRIC_PLAN_PUR_COSTS(:)
         R_INCOME_VARS(:,IPL Monthy OandM Expenses) =
     +               R_INCOME_VARS(:,IPL Monthy OandM Expenses)
     +               - R_INCOME_VARS(:,IPL Electric Plan Fuel Costs)
     +               - R_INCOME_VARS(:,IPL Electric Plan Purchase Costs)
      RETURN
      END SUBROUTINE IPALCO_ELECT_COSTUMER_COSTS
C***********************************************************************
      SUBROUTINE IPALCO_BASE_YEAR_ACTUAL_INCOME_DATA(INCOME_ITEMS)
C***********************************************************************
c
      USE IREC_ENDPOINT_CONTROL
      INCLUDE 'GLOBECOM.MON'
      INCLUDE 'MTHNMCOM.MON'
      REAL (KIND=4), PARAMETER :: NOT_AVAIL=-999999.
      CHARACTER*7 MONTH_ENDING,DATE_12_MOS_ENDING
      CHARACTER*6 SHORT_MONTH_NAMES
      CHARACTER*25 COLUMN_HEADING
      INTEGER (KIND=2) :: YR,MO,R_YR,DELETE,RPT_YR,GET_MONTH_NUMBER,
     +                    STORED_YEARS,IPL_REGULATED_REVENUES_RPT_HEADER
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
C
         FILE_NAME = IPALCO_ACTUAL_REGULATED_REVENUES_FILE()
C
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
     +                               Monthly Total Operating Revenues) = 
     +                                    IPL_TOTAL_ELECTRIC_REVS(MO,YR)
                     INCOME_ITEMS(MO,YR,IPL Operating Revenues) = 
     +                                   IPL_RETAIL_CUSTOMER_REVS(MO,YR)
                     INCOME_ITEMS(MO,YR,IPL Monthy OandM Expenses) = 
     +                                         IPL_REG_EXP_DATA(MO,YR,1)
                     INCOME_ITEMS(MO,YR,
     +                           Monthy Depreciation and Amortization) = 
     +                                         IPL_REG_EXP_DATA(MO,YR,2)
                     INCOME_ITEMS(MO,YR,Monthy Taxes NonIncome) = 
     +                                         IPL_REG_EXP_DATA(MO,YR,3)
                     INCOME_ITEMS(MO,YR,Monthly Operating Revenue Tax) = 
     +                                         IPL_REG_EXP_DATA(MO,YR,4)
                     INCOME_ITEMS(MO,YR,
     +                            IPL Miscellaneous Regulated Revenue) = 
     +                                         IPL_REG_EXP_DATA(MO,YR,5)
                     INCOME_ITEMS(MO,YR,
     +                            Monthly Federal ATL Income Tax Paid) = 
     +                                         IPL_REG_EXP_DATA(MO,YR,6)
                     INCOME_ITEMS(MO,YR,
     +                              Monthly State ATL Income Tax Paid) = 
     +                                         IPL_REG_EXP_DATA(MO,YR,7)
                     INCOME_ITEMS(MO,YR,
     +                                Monthly Income Tax Deferrals Cr) = 
     +                                         IPL_REG_EXP_DATA(MO,YR,8)
                     INCOME_ITEMS(MO,YR,Monthly ITC Amortization) = 
     +                                         IPL_REG_EXP_DATA(MO,YR,9)
                     INCOME_ITEMS(MO,YR,IPL Total Operating Expenses) = 
     +                                        IPL_REG_EXP_DATA(MO,YR,10)
                     INCOME_ITEMS(MO,YR,IPL Operating Income) = 
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
C***********************************************************************
      ENTRY IPALCO_TRAILING_12_MONTHS_REVENUE_REPORT(CLASS_ID,
     +                                               ASSET_CLASS_NAME,
     +                                               STORED_YEARS,
     +                                               INCOME_ITEMS)
C***********************************************************************
C
         IF(.NOT. IPL_12_MONTHS_REVENUE_RPT_OPEN) THEN
            IPL_REG_REVS_UNIT = 
     +               IPL_REGULATED_REVENUES_RPT_HEADER(IPL_REG_REVS_REC)
            IPL_12_MONTHS_REVENUE_RPT_OPEN = .TRUE.
         ENDIF
         IPL_TOTAL_ELECTRIC_REVS(:,:) =
     +             INCOME_ITEMS(1:12,:,Monthly Total Operating Revenues) 
         IPL_RETAIL_CUSTOMER_REVS(:,:) =
     +                       INCOME_ITEMS(1:12,:,IPL Operating Revenues)  
         IPL_REG_EXP_DATA(:,:,1) =
     +                    INCOME_ITEMS(1:12,:,IPL Monthy OandM Expenses)
         IPL_REG_EXP_DATA(:,:,2) =
     +         INCOME_ITEMS(1:12,:,Monthy Depreciation and Amortization)  
         IPL_REG_EXP_DATA(:,:,3) =
     +                       INCOME_ITEMS(1:12,:,Monthy Taxes NonIncome)  
         IPL_REG_EXP_DATA(:,:,4) =
     +                INCOME_ITEMS(1:12,:,Monthly Operating Revenue Tax)  
         IPL_REG_EXP_DATA(:,:,5) =
     +          INCOME_ITEMS(1:12,:,IPL Miscellaneous Regulated Revenue)  
         IPL_REG_EXP_DATA(:,:,6) =
     +          INCOME_ITEMS(1:12,:,Monthly Federal ATL Income Tax Paid)  
         IPL_REG_EXP_DATA(:,:,7) =
     +            INCOME_ITEMS(1:12,:,Monthly State ATL Income Tax Paid)  
         IPL_REG_EXP_DATA(:,:,8) =
     +              INCOME_ITEMS(1:12,:,Monthly Income Tax Deferrals Cr)  
         IPL_REG_EXP_DATA(:,:,9) =
     +                     INCOME_ITEMS(1:12,:,Monthly ITC Amortization)  
         IPL_REG_EXP_DATA(:,:,10) =
     +                 INCOME_ITEMS(1:12,:,IPL Total Operating Expenses)  
         IPL_REG_EXP_DATA(:,:,11) =
     +                         INCOME_ITEMS(1:12,:,IPL Operating Income)  
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
C***********************************************************************
      SUBROUTINE IMPLIED_ESC(VALUE29,VALUE30)
C***********************************************************************
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
C***********************************************************************
      
