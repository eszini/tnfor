!
      SUBROUTINE CL_OBJECT
      use end_routine, only: end_program, er_message
      use SpinDriftLib
      use prod_arrays_dimensions
      use logging
      use miscmod
      use filemod
      use prim_mover_idx
      use cla_decs
      USE SIZECOM
      use hesi

      implicit none

      INTEGER (kind=2) ::  IREC
      INTEGER (kind=2) ::  INUNIT
      INTEGER (kind=2) ::  DELETE
      INTEGER (kind=2) ::
     +    LRECL=1159  ! 120218 ! 081818 ! 022510 ! 052609 1068 ! 011809 1041! 091107 ! 1033! 100506 for KCPL ! 080906 ! /961/
      INTEGER (kind=2) ::  UNIT_NO,R_ACTIVE_CL_RECORDS,
     +          MONTHLY_CAPACITY_POINTER,I,
     +          MONTHLY_FUEL_INDEX,
     +          RETROFIT_PROJECT_ID
      INTEGER ::  IOS,IOS_BASE
      logical, save :: cl_21_hit=.false., cl_31_hit=.false.
      CHARACTER (len=2) ::  CL_LOAD_TYPE,
     +            UNIT_TYPE,
     +            UNIT_TYPE_CATEGORY
      CHARACTER (len=5) ::  OVERLAY_FAMILY_NAME,FOSSILFL
      CHARACTER (len=36) ::  THERMAL_GUID
      CHARACTER (len=48) ::  UNIT_NAME
      CHARACTER (len=30) ::  DESC,CO2_BASIN_NAME
      CHARACTER (len=256) ::  FILE_NAME
      CHARACTER (len=256) ::  BASE_FILE_DIRECTORY,OUTPUT_DIRECTORY

      real(kind=4) :: p_fuel_deliv_2_ord
      CHARACTER (len=2048) :: RECLN
! DECLARATION FOR /FOSSIL FILE/
      LOGICAL (kind=1) ::  PHASE_I
      CHARACTER (LEN=1) :: PHASE_I_STR
      logical :: file_exists
      INTEGER (kind=2) ::  GENGRP,ONLIMO,ONLIYR,OFLIMO,OFLIYR,
     +          PRESCR,SDESCR,VRESCR,FIXED_COST_ESCALATOR,
     +          AI_CAPACITY_ESCALATOR,AI_ENERGY_ESCALATOR,
     +          AI_REMAINING_LIFE,TIE_GROUP,
     +          ASSET_CLASS_NUM,ASSET_CLASS_VECTOR,
     +          INTRA_COMPANY_CLASS_ID,
     +          NOX_SEASON_DATE,
     +          NOX_CONTROL_DATE,
     +          SOX_CONTROL_DATE,
     +          CO2_CONTROL_DATE,
     +          HG_CONTROL_DATE,
     +          OTHER3_CONTROL_DATE,
     +          MONTHLY_MUST_RUN_VECTOR,
     +          MONTHLY_DECOMMIT_VECTOR,
     +          EMISSION_MARKET_LINK,
     +          FIRST_RETIREMENT_YEAR,
     +          RPS_PROGRAM_NUMBER,
     +          GET_RPS_PROGRAM_POSITION
      CHARACTER (len=1) ::  INTRA_COMPANY_TRANSACTION,
     +            SPECIAL_UNIT_ID*20,
     +            UNIT_ACTIVE,
     +            REPORT_THIS_UNIT,
     +            UTILITY_OWNED,
     +            START_UP_LOGIC,
     +            CONTRIBUTES_TO_SPIN,
     +            APPLY_NOX_SEASON_DATE,
     +            MARKET_RESOURCE,
     +            USE_POLY_HEAT_RATES,
     +            WVPA_RATE_TRACKER,
     +            WVPA_RES_TRACKER,
     +            WVPA_FUEL_TRACKER,
     +            WVPA_MEM_TRACKER,
     +            ALLOW_DECOMMIT,
     +            MONTE_OR_FREQ,
     +            PRIMARY_FUEL_CATEGORY,
     +            SECONDARY_FUEL_CATEGORY,
     +            EMISSIONS_FUEL_CATEGORY,
     +            RETIREMENT_CANDIDATE,
     +            RETROFIT_CANDIDATE,
     +            THERMAL_AGGREGATED_UNIT
      CHARACTER (len=6) ::     BASECASE_PLANT_ID,
     +               BASECASE_UNIT_ID,
     +               BASECASE_MARKET_AREA_ID,
     +               BASECASE_TRANS_AREA_ID,
     +               BASECASE_PLANT_NERC_SUB_ID,
     +               BASECASE_PLANT_OWNER_ID,
     +               PRIMARY_MOVER_STR
      CHARACTER (len=20) ::  COUNTY,NEWGEN_UNIT_STATUS,STATE_PROV_NAME
      CHARACTER (len=6) ::  STATE_PROVINCE
      REAL ::  AI_TAX_LIFE,
     +     AI_ADR_TAX_LIFE,
     +     START_UP_COSTS
! DECLARATION FOR TRANSACT
      INTEGER (kind=2) ::  TRANSACTION_GROUP_ID,DAY_TYPE_ID
!
! CHANGED FROM INTEGER*2 TO REAL 5/14/92 ADDED THE ID AS INTEGER*1
!
      REAL ::  MW(2),ANNUAL_CL_FIXED_COST,CO2_PIPELINE_DISTANCE,
     +     CO2_RETRO_HEAT_MULT,CO2_RETRO_CAP_MULT
      INTEGER (kind=2) ::  RESOURCE_ID,P_FUEL_SUPPLY_ID
! END 5/14/92 CHANGES
      INTEGER (kind=2) ::  ANNUAL_CL_FIXED_COST_ESC
      REAL :: FRAOWN,FUELMX,PRFCST,SEFCST,EFOR,FUELADJ,MAINRT(12),
     + VARCST,
     +     DISPADJ,HRFACT,COEFF(3),FIXED_COST,AI_CAPACITY_RATE,
     +     AI_ENERGY_RATE,POOL_FRAC_OWN,DISP_ADJ2_OUT,DISPADJ2,
     +     MAINT_DAYS,DISPATCH_MULT,EXCESS_ENERGY_SALES,
     +     MINIMUM_CAPACITY_FACTOR,MAXIMUM_CAPACITY_FACTOR,
     +     RAMP_RATE,MIN_DOWN_TIME,RAMP_DOWN_RATE,
     +     MIN_UP_TIME,
     +     P_FUEL_DELIVERY,
     +     P_FUEL_DELIVERY_2,P_FUEL_DELIVERY_3,
     +     S_FUEL_DELIVERY,
     +     S_FUEL_DELIVERY_2,S_FUEL_DELIVERY_3,
     +     FOR_FREQUENCY,
     +     FOR_DURATION,
     +     WINTER_TOTAL_CAPACITY,

     +     NOX_CONTROL_PERCENT,
     +     SOX_CONTROL_PERCENT,
     +     CO2_CONTROL_PERCENT,
     +     HG_CONTROL_PERCENT,
     +     OTHER3_CONTROL_PERCENT,
     +     EMERGENCY_CAPACITY,
     +     EMERGENCY_HEATRATE,
     +     NOX_VOM,
     +     NOX_FOM,
     +     SOX_VOM,
     +     SOX_FOM,
     +     CO2_VOM,
     +     CO2_FOM,
     +     HG_VOM,
     +     HG_FOM,
     +     OTHER3_VOM,
     +     OTHER3_FOM,
     +     LATITUDE,
     +     LONGITUDE,
     +     MW_INSIDE_FENCE,
     +     CONSTANT_POLY,
     +     FIRST_POLY,
     +     SECOND_POLY,
     +     THIRD_POLY,
     +     MIN_SPIN_CAP,
     +     MAX_SPIN_CAP,
     +     INTER_BLOCKS(3),
     +     MARKET_FLOOR,
     +     MARKET_CEILING,
     +     START_UP_COSTS_ESCALATION,  ! 212
     +     RPS_CONTRIBUTION_PERCENT
! DECLARATION FOR EMISSIONS ITEMS
      REAL ::  P_SO2,P_NOX_BK1,P_CO2,P_NOX,P_PARTICULATES,
     + P_NOX_BK2_OUT,
! ADDED 6/17/91
     +     P_OTHER2,P_OTHER3,
! ADDED 12/9/92 FOR SOUTHERN COMPANY
     +     P_NOX_BK2,EMISS_FUEL_COST,EMISS_BLENDING_RATE
       INTEGER (kind=2) ::  SEC_EMISS_PTR,EMISS_FUEL_ESCAL,
     + EMISS_FUEL_EMISS_PTR,
     +          PRIM_FUEL_EMISS_PTR
! DECLARATION FOR /ENERGY LIMITED RESOURCE FILE/
! 5/20/92 ADDED EL GROUP CAPABILITY-GT
      REAL ::  PLANNING_FACTOR
      CHARACTER (len=1) ::  EXPENSE_ASSIGNMENT,EXPENSE_COLLECTION,
! ADDED 3/10/93 FOR CLINTON TAX
     +            FUEL_TYPES(2),PRIM_FUEL_TYPE*6
      character (len=3) ::  temp_expense_collection
      CHARACTER (len=22) ::  FILE_TYPE='Capacity-limited Units' 
      INTEGER (kind=2) ::  NUCLEAR_UNITS_ACTIVE,R_NUM_OF_NUCLEAR_UNITS
      SAVE NUCLEAR_UNITS_ACTIVE
      CHARACTER (len=1) ::  NUCLEAR
      PARAMETER (NUCLEAR='N')
      INTEGER (kind=8) ::  HESI_UNIT_ID_NUM_ord,
     +          H2_UNIT_ID_NUM,
     +          TRANS_BUS_ID,
     +          THERMAL_PARENT_ID
      INTEGER ::  ! H2_UNIT_ID_NUM,
     +        POWERDAT_PLANT_ID

! MULTI-CL FILE VARIABLES
!
      INTEGER ::  FILE_NUMBER,FILE_ID
      INTEGER ::  MAX_CL_FILES,R_MAX_CL_FILES
      PARAMETER (MAX_CL_FILES=36) ! note file 24 is for SPCapEx
      INTEGER (kind=2) ::
     +    ACTIVE_CL_RECORDS(0:MAX_CL_FILES-1)/MAX_CL_FILES*0/
      INTEGER (kind=2) ::
     +    NUC_UNITS_IN_FILE(0:MAX_CL_FILES-1)/MAX_CL_FILES*0/
      INTEGER (kind=2) ::  NUC_UNITS_IN_OL_FILE(0:MAX_CL_FILES-1)
     +  /MAX_CL_FILES*0/
      CHARACTER (len=2) ::
     +    FOSSILOL(0:MAX_CL_FILES-1)/MAX_CL_FILES*'BC'/
      INTEGER (kind=2) ::  NUC_RECORD_POSITION(200,0:MAX_CL_FILES-1)
      INTEGER (kind=2) ::  ONLINE,OFLINE ! 2/19/98. GAT. CHANGED FROM 50
      CHARACTER (len=5) ::  CL_FILE_BASE_NAMES(0:MAX_CL_FILES-1),
     +            VOID_CHR,BASE_FILE_NAME
      CHARACTER (len=2) ::  CL_FILE_CODES(0:MAX_CL_FILES-1)/
     +                              'CL','C1','C2','C3','C4','C5',
     +                              'C6','C7','C8','C9','C0',
     +                              'CA','CB','CC','CD','CE','CG',
     +                              'CI','CJ','CK','CM','CP','CQ',
     +                              'XC','CS','CU','CV','CW','CX',
     +  'CY',
     +                              'EJ','EK','ER','ET','MX','MY'/,
     +            FILE_CODE
      INTEGER (kind=2) ::
     +               capacity_market_pointer_ocl
      REAL (kind=4) ::          CAPACITY_MARKET_COIN_ADJ_FACT
      CHARACTER (len=5) ::
     +               CAPACITY_MARKET_TYPE,
     +               CAPACITY_MARKET_MONTH,
     +               CAPACITY_MARKET_EXP_COLLECT
      CHARACTER (len=20) ::    CAPACITY_MARKET_COST_ASSIGN
      CHARACTER (len=6) ::
     +   CL_FILE_BINARY_NAMES(0:MAX_CL_FILES-1)/'FOSIL ',
     +                                       'FOSIL1',
     +                                       'FOSIL2',
     +                                       'FOSIL3',
     +                                       'FOSIL4',
     +                                       'FOSIL5',
     +                                       'FOSIL6',
     +                                       'FOSIL7',
     +                                       'FOSIL8',
     +                                       'FOSIL9',
     +                                       'FOSIL0',
     +                                       'FOSILA',
     +                                       'FOSILB',
     +                                       'FOSILC',
     +                                       'FOSILD',
     +                                       'FOSILE',
     +                                       'FOSILF',
     +                                       'FOSILG',
     +                                       'FOSILH',
     +                                       'FOSILI',
     +                                       'FOSILJ',
     +                                       'FOSILK',
     +                                       'FOSILL',
     +                                       'FOSILM',
     +                                       'FOSILN',
     +                                       'FOSILO',
     +                                       'FOSILP',
     +                                       'FOSILQ',
     +                                       'CapExF',
     +                                       'FOSILR',
     +                                       'FOSILS',
     +                                       'FOSILU',
     +                                       'FOSILV',
     +                                       'FOSILW',
     +                                       'FOSILX',
     +                                       'FOSILY'/,
     +            BINARY_FILE_NAME
      LOGICAL ::  ACTIVE_BASE_CL_FILES(0:MAX_CL_FILES-1)/
     +                                            MAX_CL_FILES*.FALSE./,
     +        ACTIVE_OVERLAY_CL_FILES(0:MAX_CL_FILES-1)/
     +                                            MAX_CL_FILES*.FALSE./
      LOGICAL (kind=1) ::  CL_FILES_ARE_ACTIVE=.FALSE. 
      LOGICAL (kind=1) ::  R_CL_FILES_ARE_ACTIVE
      CHARACTER (len=256) ::  COMMAND1,COMMAND2
      LOGICAL (kind=1) ::  LAHEY_LF95
      CHARACTER (len=30) ::  SCREEN_OUTPUT
! SPCapEx additions Nov. 2005
      CHARACTER (LEN=20)  :: TECH_TYPE
      LOGICAL (KIND=1) :: SP_CAPEX_ACTIVE,SPCapExOption
      CHARACTER (LEN=15) ,DIMENSION(155) :: CL_VARIABLES
      REAL (KIND=4) :: FIRST_YEAR_DECOMM_AVAIALABLE
      REAL (KIND=4) :: DECOMMISSIONING_BASE_YR_COST,
     +                 DECOMMISSIONING_COST_ESCALATION,
     +                 ANNUAL_ENERGY_PLANNING_FACTOR,
     +                 NAME_PLATE_CAPACITY,
     +                 FuelRatio(5),
     +                 BlendableFuelsPtr(4), ! 167
     +                 FuelTransportationCost(4),
     +                 BlendedEnthalpyUp,
     +                 BlendedEnthalpyLo,
     +                 BlendedSO2Up,                  ! 177
     +                 BettermentProjectID,
     +                 DECOM_CONTINUING_COST,
     +                 DECOM_CONT_COST_ESCALATION,
     +                 EnrgPatternPointer, ! 181
     +                 EmissRedRate(5),
     +                 EmissMaxRate(5),
     +                 MaxEnergyLimit(3)
      CHARACTER (LEN=1) :: AGGREGATE_THIS_UNIT,
     +                     EMISSION_DATA_UNITS,
     +                     LINKED_BETTERMENT_OPTION
      CHARACTER (len=10) ::  FUEL_BLENDING_IS
      character (len=256) :: misc_character_string
      character (len=1024) :: other_filename
      character (len=512) :: basename




       utility_owned=""
!***********************************************************************
!
!          ROUTINE TO CONVERT METAFILE FILES TO DIRECT-ACESS BINARY
!          COPYRIGHT (C) 1983, 84, 85  M.S. GERBER & ASSOCIATES, INC.
!
!***********************************************************************
!
! CONVERT THE (FOSSIL-FUELED) CAPACITY-LIMITED-UNITS FILE
!***************************************************************
      ENTRY CL_MAKEBIN
!***************************************************************
      VOID_CHR = FOSSILFL(CL_FILE_BASE_NAMES)
!
      IF(.NOT. LAHEY_LF95())
     +        CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
      NUCLEAR_UNITS_ACTIVE = 0

!
       if(.not. cl_21_hit) then
        cl_21_hit=.true.
               inquire(unit=10, NAME=file_name)
               call write_log_entry("cla_objt:0021",
     + "Reading " // trim(file_name) // " (unit 10) ")
       endif

      DO FILE_ID = 0, MAX_CL_FILES-1
         IF(.NOT. SP_CAPEX_ACTIVE() .AND. FILE_ID == 23) CYCLE  ! CQ file
         ACTIVE_BASE_CL_FILES(FILE_ID) = .FALSE.
         BASE_FILE_NAME = CL_FILE_BASE_NAMES(FILE_ID)

!        ! Prevent spurious Lahey compiler warning about
!        ! use of uninitialized variable in call to
!        ! int_to_string().
         misc_character_string=""

!        ! Get file handle
         misc_character_string=int_to_string(FILE_ID)

         IF(INDEX(BASE_FILE_NAME,'NONE') /= 0) CYCLE
         FILE_CODE = CL_FILE_CODES(FILE_ID)
         BINARY_FILE_NAME = CL_FILE_BINARY_NAMES(FILE_ID)
         FILE_NAME = trim(BASE_FILE_DIRECTORY())//FILE_CODE//
     +                               "B"//trim(BASE_FILE_NAME)//".DAT"
         INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
         IF(FILE_EXISTS) THEN
            SPCapExOption = FILE_ID == 23
            IF(LAHEY_LF95()) THEN
               SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//BASE_FILE_NAME
               CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
            ELSE
               CALL MG_CLEAR_LINE_WRITE(16,30,34,trim(BASE_FILE_NAME),
     +                                                   ALL_VERSIONS,0)
            ENDIF
            ACTIVE_BASE_CL_FILES(FILE_ID) = .TRUE.
            CL_FILES_ARE_ACTIVE = .TRUE.
            OPEN(10,FILE=FILE_NAME)
            other_filename=trim(OUTPUT_DIRECTORY())//
     +                     "BC"//trim(BINARY_FILE_NAME)//".BIN"

            OPEN(11,FILE=other_filename,
     +                   ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
            IREC = 0
               inquire(unit=10, NAME=file_name)
               call write_log_entry("cla_objt:0020",
     + "Reading " // trim(just_the_filename(file_name)) //
     + " (unit 10) ")

               inquire(unit=11, NAME=file_name)
               call write_log_entry("cla_objt:0025",
     + "Reading " // trim(just_the_filename(file_name)) //
     + " (unit 11) ")

            READ(10,*) DELETE
!
            DO
               AI_CAPACITY_RATE = 0.
               AI_CAPACITY_ESCALATOR = 0.
               AI_ENERGY_RATE = 0.
               AI_ENERGY_ESCALATOR = 0.
               AI_REMAINING_LIFE = 0
               P_SO2 = 0.
               P_NOX_BK1 = 0.
               P_CO2 = 0.
               P_OTHER2 = 0.
               P_OTHER3 = 0.
               POOL_FRAC_OWN = 100.
               PHASE_I = .TRUE.
               P_FUEL_SUPPLY_ID = 0
               DISPADJ2  = -99999.
               P_NOX_BK2 = -99999.
               SEC_EMISS_PTR = -1
               EMISS_FUEL_COST = 0.
               EMISS_FUEL_ESCAL = 0
               EMISS_FUEL_EMISS_PTR = 0
               EMISS_BLENDING_RATE = 0.
               PRIM_FUEL_EMISS_PTR = 0
               PRIM_FUEL_TYPE = 'T'
               FUEL_TYPES(1) = 'T'
               FUEL_TYPES(2) = 'T'
               TIE_GROUP = 0
               MONTHLY_CAPACITY_POINTER = 0
               ANNUAL_CL_FIXED_COST = 0.
               ANNUAL_CL_FIXED_COST_ESC = 0
               MAINT_DAYS = 0.
               DISPATCH_MULT = 1.
               EXCESS_ENERGY_SALES = 0.
               AI_TAX_LIFE = 99.
               AI_ADR_TAX_LIFE = 99.
               ASSET_CLASS_NUM = 0
               ASSET_CLASS_VECTOR = 0
               INTRA_COMPANY_CLASS_ID = -1
               INTRA_COMPANY_TRANSACTION = 'N'
               MINIMUM_CAPACITY_FACTOR = 0.
               MAXIMUM_CAPACITY_FACTOR = 8800.
               TRANSACTION_GROUP_ID = 1
               DAY_TYPE_ID = 0
               UNIT_ACTIVE = 'T'
               REPORT_THIS_UNIT = 'T'
               BASECASE_PLANT_ID = 'BLANK ' ! CHAR*6
               BASECASE_UNIT_ID = 'BLANK ' ! CHAR*6
               BASECASE_MARKET_AREA_ID = 'BLANK ' ! CHAR*6
               BASECASE_TRANS_AREA_ID = 'BLANK ' ! CHAR*6
               BASECASE_PLANT_NERC_SUB_ID = 'BLANK ' ! CHAR*6
               BASECASE_PLANT_OWNER_ID = 'BLANK ' ! CHAR*6
               UTILITY_OWNED = 'U'
               MONTHLY_FUEL_INDEX = 0
               START_UP_COSTS = 0.
               START_UP_LOGIC = 'F'
               CONTRIBUTES_TO_SPIN = 'F'
               RAMP_RATE = 999999.
               RAMP_DOWN_RATE = -999999.
               MIN_DOWN_TIME = 0.0
               MIN_UP_TIME = 0.0
               P_FUEL_DELIVERY = 0.0
               p_fuel_deliv_2_ord = 0.0
               P_FUEL_DELIVERY_3 = 0.0
               S_FUEL_DELIVERY = 0.0
               s_fuel_delivery_2 = 0.0
               s_fuel_delivery_3 = 0.0
               HESI_UNIT_ID_NUM_ord = 0
               H2_UNIT_ID_NUM = 0
               FOR_FREQUENCY = 0.
               FOR_DURATION = 0.
               WINTER_TOTAL_CAPACITY = 0.
               CO2_CONTROL_PERCENT = 0.
               HG_CONTROL_PERCENT = 0.
               OTHER3_CONTROL_PERCENT = 0.
               NOX_CONTROL_PERCENT = 0.
               SOX_CONTROL_PERCENT = 0.
               NOX_CONTROL_DATE = 9001
               SOX_CONTROL_DATE = 9001
               CO2_CONTROL_DATE = 9001
               HG_CONTROL_DATE = 9001
               OTHER3_CONTROL_DATE = 9001
               APPLY_NOX_SEASON_DATE = 'F'
               NOX_SEASON_DATE = 0150
!
               EMERGENCY_CAPACITY = 0.
               EMERGENCY_HEATRATE = 0.
               MARKET_RESOURCE = 'F'
!
               PRIMARY_MOVER_STR = 'PMNONE'
               COUNTY = 'BLANK'
               NEWGEN_UNIT_STATUS = 'BLANK'
               STATE_PROVINCE = '#N/A'
!
               NOX_VOM = 0.
               NOX_FOM = 0.
               SOX_VOM = 0.
               SOX_FOM = 0.
               CO2_VOM = 0.
               CO2_FOM = 0.
               HG_VOM = 0.
               HG_FOM = 0.
               OTHER3_VOM = 0.
               OTHER3_FOM = 0.
               LATITUDE = 0.
               LONGITUDE = 0.
               MW_INSIDE_FENCE = 0.
!
               USE_POLY_HEAT_RATES = 'F'
               WVPA_RATE_TRACKER = 'N'
               WVPA_RES_TRACKER = 'N'
               WVPA_FUEL_TRACKER = 'N'
               WVPA_MEM_TRACKER = 'M'
               MONTE_OR_FREQ = 'G'
               LINKED_BETTERMENT_OPTION = 'F'

               ALLOW_DECOMMIT = 'T'

               MONTHLY_MUST_RUN_VECTOR = 0
               MONTHLY_DECOMMIT_VECTOR = 0
               TRANS_BUS_ID = 0
               EMISSION_MARKET_LINK = -9999
               FIRST_RETIREMENT_YEAR = 1901
               RPS_PROGRAM_NUMBER = 0
!
               MIN_SPIN_CAP = 0.
               MAX_SPIN_CAP = 9999.
!
               MARKET_FLOOR = 0.
               MARKET_CEILING = 0.
               START_UP_COSTS_ESCALATION = 0.
!
               INTER_BLOCKS(1) = -99.
               INTER_BLOCKS(2) = -99.
               INTER_BLOCKS(3) = -99.
               NAME_PLATE_CAPACITY = -99.
               FIRST_YEAR_DECOMM_AVAIALABLE = 2100.
               DECOMMISSIONING_BASE_YR_COST = 0.
               DECOMMISSIONING_COST_ESCALATION = 0.
               ANNUAL_ENERGY_PLANNING_FACTOR = 0.
               AGGREGATE_THIS_UNIT = "F"
!
               FuelRatio = 0.
               BlendableFuelsPtr = 0.
               FuelTransportationCost = 0.
               BlendedEnthalpyUp = 0.
               BlendedEnthalpyLo = 0.
               BlendedSO2Up = 0.
               BettermentProjectID = 0.
               DECOM_CONTINUING_COST = 0.
               DECOM_CONT_COST_ESCALATION = 0.
               EnrgPatternPointer = 0.
               EMISSION_DATA_UNITS = 'l'
               EmissRedRate = 0.
               EmissMaxRate = 0.
               MaxEnergyLimit= 0.
               TECH_TYPE = " "
               FUEL_BLENDING_IS = 'Primary'
               FUELMX = 1. ! 070806.
               CAPACITY_MARKET_TYPE = 'NONE'
               CAPACITY_MARKET_COIN_ADJ_FACT = 1.0
               PRIMARY_FUEL_CATEGORY = ' '
               SECONDARY_FUEL_CATEGORY = ' '
               EMISSIONS_FUEL_CATEGORY = ' '
               RPS_CONTRIBUTION_PERCENT = 0.0
               RETIREMENT_CANDIDATE = 'F'
               RETROFIT_CANDIDATE = 'F'
!
               THERMAL_GUID = 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
               THERMAL_PARENT_ID = 123456789
               THERMAL_AGGREGATED_UNIT = 'F'
               if(.not. cl_31_hit) then
                   cl_31_hit=.true.
                   inquire(unit=10, NAME=file_name)
                   call write_log_entry("cla_objt:0031",
     + "Reading " // trim(just_the_filename(file_name)) //
     + " (unit 10) ")
               endif

               DO
            file_name=trim(get_filename_from_unit(10))
            call write_log_entry("cla_objt:0092", "Reading RECLEN " //
     + "from " // trim(file_name) // " (unit 10).")

                  READ(10,1000,IOSTAT=IOS) RECLN
                  IF(IOS /=0) EXIT
                  IF(RECLN(1:1) == '7') EXIT
                  RECLN=trim(RECLN)//
     +            ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'//
     +            ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
!
! VARIABLES THAT CAN NOT PROPAGATE LEFT TO RIGHT
!
                  SPECIAL_UNIT_ID = " "
                  CO2_RETRO_CAP_MULT = 1
                  CO2_RETRO_HEAT_MULT = 1
                  FIRST_RETIREMENT_YEAR = 1901


! first read

                  READ(RECLN,*)DELETE,UNIT_NAME,CL_LOAD_TYPE,
     +               EXPENSE_ASSIGNMENT,TEMP_EXPENSE_COLLECTION,GENGRP,
     +               FRAOWN,
     +               ONLIMO,ONLIYR,OFLIMO,OFLIYR,    ! 10
     +               FUELMX,PRFCST,PRESCR,
     +               SEFCST,SDESCR,FUELADJ,EFOR,
     +               MAINRT,VARCST,VRESCR,  ! 31
     +               FIXED_COST,FIXED_COST_ESCALATOR,
     +               DISPADJ,HRFACT,MW,PLANNING_FACTOR,COEFF,DESC,  !42
     +               P_SO2,P_NOX_BK1,P_CO2,POOL_FRAC_OWN,
     +               P_OTHER2,P_OTHER3,PHASE_I_STR,DISPADJ2, !50
     +               RESOURCE_ID,P_FUEL_SUPPLY_ID,P_NOX_BK2,
     +               SEC_EMISS_PTR,EMISS_FUEL_COST,EMISS_FUEL_ESCAL,
     +               EMISS_FUEL_EMISS_PTR,EMISS_BLENDING_RATE,
     +               PRIM_FUEL_EMISS_PTR,
     +               PRIM_FUEL_TYPE,                          !  60
     +               FUEL_TYPES,TIE_GROUP,
     +               MONTHLY_CAPACITY_POINTER,
     +               AI_CAPACITY_RATE,AI_CAPACITY_ESCALATOR,
     +               AI_ENERGY_RATE,AI_ENERGY_ESCALATOR,
     +               AI_REMAINING_LIFE,
     +               ANNUAL_CL_FIXED_COST,                     ! 70
     +               ANNUAL_CL_FIXED_COST_ESC,
     +               MAINT_DAYS,DISPATCH_MULT,
     +               EXCESS_ENERGY_SALES,
     +               AI_TAX_LIFE,
     +               AI_ADR_TAX_LIFE,
     +               ASSET_CLASS_NUM,
     +               ASSET_CLASS_VECTOR,
     +               INTRA_COMPANY_CLASS_ID,
     +               INTRA_COMPANY_TRANSACTION,               ! 80
     +               SPECIAL_UNIT_ID,
     +               MINIMUM_CAPACITY_FACTOR,
     +               MAXIMUM_CAPACITY_FACTOR,
     +               TRANSACTION_GROUP_ID,
     +               DAY_TYPE_ID,
     +               UNIT_ACTIVE,
     +               BASECASE_PLANT_ID,
     +               BASECASE_UNIT_ID,
     +               BASECASE_MARKET_AREA_ID,
     +               BASECASE_TRANS_AREA_ID,                  ! 90
     +               BASECASE_PLANT_NERC_SUB_ID,
     +               BASECASE_PLANT_OWNER_ID,
     +               UTILITY_OWNED,
     +               MONTHLY_FUEL_INDEX,
     +               START_UP_COSTS,
     +               START_UP_LOGIC,
     +               RAMP_RATE,
     +               MIN_DOWN_TIME,
     +               REPORT_THIS_UNIT,
     +               P_FUEL_DELIVERY,   ! 100
     +               p_fuel_deliv_2_ord,
     +               P_FUEL_DELIVERY_3,
     +               MIN_UP_TIME,
     +               HESI_UNIT_ID_NUM_ord,  ! 104
     +               FOR_FREQUENCY, ! 105
     +               FOR_DURATION, ! 106
     +               WINTER_TOTAL_CAPACITY,
     +               CONTRIBUTES_TO_SPIN,
     +               H2_UNIT_ID_NUM,      ! 109
     +               POWERDAT_PLANT_ID,   ! 110
     +               APPLY_NOX_SEASON_DATE,  ! 111
     +               NOX_SEASON_DATE,    ! 112
     +               RAMP_DOWN_RATE,      ! 113
     +               NOX_CONTROL_PERCENT,
     +               NOX_CONTROL_DATE,
     +               EMERGENCY_CAPACITY,
     +               EMERGENCY_HEATRATE,
     +               MARKET_RESOURCE,
     +               PRIMARY_MOVER_STR,
     +               COUNTY,              ! 120
     +               STATE_PROVINCE,
     +               NOX_VOM,
     +               NOX_FOM,
     +               S_FUEL_DELIVERY,
     +               s_fuel_delivery_2,   ! 125
     +               s_fuel_delivery_3,
     +               CONSTANT_POLY,
     +               FIRST_POLY,
     +               SECOND_POLY,
     +               THIRD_POLY,          ! 130
     +               USE_POLY_HEAT_RATES,
     +               NEWGEN_UNIT_STATUS,
     +               MONTHLY_MUST_RUN_VECTOR,
     +               EMISSION_MARKET_LINK,
     +               WVPA_RATE_TRACKER,    ! 135
     +               ALLOW_DECOMMIT,
     +               MIN_SPIN_CAP,
     +               MAX_SPIN_CAP,
     +               WVPA_RES_TRACKER,
     +               WVPA_FUEL_TRACKER,  ! 140
     +               MONTE_OR_FREQ,
     +               WVPA_MEM_TRACKER,
     +               MONTHLY_DECOMMIT_VECTOR,
     +               TRANS_BUS_ID,
     +               SOX_CONTROL_PERCENT,
     +               SOX_CONTROL_DATE,
     +               SOX_VOM,
     +               SOX_FOM,
     +               LATITUDE,
     +               LONGITUDE,          ! 150
     +               MW_INSIDE_FENCE,
     +               INTER_BLOCKS,     ! 152-154

     +               FIRST_YEAR_DECOMM_AVAIALABLE,     ! 155
     +               DECOMMISSIONING_BASE_YR_COST,            ! 156
     +               DECOMMISSIONING_COST_ESCALATION,
     +               ANNUAL_ENERGY_PLANNING_FACTOR,
     +               AGGREGATE_THIS_UNIT,              ! 159
     +               NAME_PLATE_CAPACITY,
     +               FUEL_BLENDING_IS,
     +               FuelRatio,         ! 162-166
     +               BlendableFuelsPtr, ! 167-170
     +               FuelTransportationCost, ! 171-174
     +               BlendedEnthalpyUp, ! 175
     +               BlendedEnthalpyLo,
     +               BlendedSO2Up,                  ! 177
     +               BettermentProjectID,
     +               DECOM_CONTINUING_COST,
     +               DECOM_CONT_COST_ESCALATION,
     +               EnrgPatternPointer, ! 181
     +               EMISSION_DATA_UNITS,
     +               EmissRedRate,       ! 183-187
     +               EmissMaxRate,
     +               MaxEnergyLimit,
     +               TECH_TYPE,           ! 196
     +               LINKED_BETTERMENT_OPTION, ! 197
     +               CO2_CONTROL_PERCENT,
     +               HG_CONTROL_PERCENT,
     +               OTHER3_CONTROL_PERCENT,
     +               CO2_CONTROL_DATE,
     +               HG_CONTROL_DATE,
     +               OTHER3_CONTROL_DATE,
     +               CO2_VOM, ! 204
     +               CO2_FOM,
     +               HG_VOM,
     +               HG_FOM,
     +               OTHER3_VOM,
     +               OTHER3_FOM, ! 209
     +               MARKET_FLOOR,
     +               MARKET_CEILING,  ! 211
     +               START_UP_COSTS_ESCALATION,  ! 212
     +               CAPACITY_MARKET_TYPE,
     +               CAPACITY_MARKET_MONTH,
     +               capacity_market_pointer_ocl,
     +               CAPACITY_MARKET_COST_ASSIGN,
     +               CAPACITY_MARKET_EXP_COLLECT,
     +               CAPACITY_MARKET_COIN_ADJ_FACT,
     +               PRIMARY_FUEL_CATEGORY,
     +               SECONDARY_FUEL_CATEGORY, ! 220
     +               EMISSIONS_FUEL_CATEGORY,
     +               RPS_CONTRIBUTION_PERCENT,
     +               RETIREMENT_CANDIDATE,
     +               RETROFIT_CANDIDATE,
     +               RETROFIT_PROJECT_ID,
     +               CO2_BASIN_NAME,
     +               CO2_PIPELINE_DISTANCE,
     +               STATE_PROV_NAME, ! 228
     +               CO2_RETRO_HEAT_MULT,
     +               CO2_RETRO_CAP_MULT,
     +               THERMAL_GUID,
     +               THERMAL_PARENT_ID, ! 232
     +               THERMAL_AGGREGATED_UNIT,
     +               FIRST_RETIREMENT_YEAR,
     +               UNIT_TYPE,
     +               UNIT_TYPE_CATEGORY,
     +               RPS_PROGRAM_NUMBER
!

       if(cla_ord_bad(int(capacity_market_pointer_ocl))) then
        call end_program("cla_objt:0010 - capacity_market_pointer " //
     + "at point of save is invalid. Value: " //
     + trim(itos(int(capacity_market_pointer_ocl))))

          endif



                  IF(IOS /= 0) THEN
                     CL_VARIABLES = "BAD"
                     READ(RECLN,*,IOSTAT=IOS)DELETE,CL_VARIABLES
                     DO I = 1, 100
                        IF(INDEX(CL_VARIABLES(I),"BAD") /= 0) THEN
        call end_program("cla_objt:0001 - Found BAD variable " //
     + "entry at cl_variables(" // trim(itos(int(I))) // ")")


                           EXIT
                        ENDIF
                     ENDDO
                  ENDIF
                  IF(SPCapExOption) then
                      NEWGEN_UNIT_STATUS = 'SPCapEx'
                  endif
                  IF(RAMP_DOWN_RATE == -999999.) THEN
                     RAMP_DOWN_RATE = RAMP_RATE
                  ENDIF
                  IF(DISPADJ2 == -99999.) THEN
                     DISP_ADJ2_OUT = DISPADJ
                  ELSE
                     DISP_ADJ2_OUT = DISPADJ2
                  ENDIF

!
                  IF(P_NOX_BK2 == -99999.) THEN
                     P_NOX_BK2_OUT = P_NOX_BK1
                  ELSE
                     P_NOX_BK2_OUT = P_NOX_BK2
                  ENDIF
                  IF(INDEX(TEMP_EXPENSE_COLLECTION,'BTL') /= 0) THEN
                     EXPENSE_COLLECTION = 'X'
                  ELSE
                     EXPENSE_COLLECTION = TEMP_EXPENSE_COLLECTION(1:1)
                  ENDIF
                  IREC = IREC + 1
                 IF(CL_LOAD_TYPE(1:1) == NUCLEAR .AND. DELETE < 8 .AND.
     +               (UNIT_ACTIVE == 'T' .OR. UNIT_ACTIVE == 't')) THEN
                     NUC_UNITS_IN_FILE(FILE_ID) =
     +                                   NUC_UNITS_IN_FILE(FILE_ID) + 1
                     NUC_RECORD_POSITION(NUC_UNITS_IN_FILE(FILE_ID),
     +                                                  FILE_ID) = IREC
                     NUCLEAR_UNITS_ACTIVE = NUCLEAR_UNITS_ACTIVE + 1
                  ENDIF

                 if(cla_ord_bad(int(capacity_market_pointer_ocl))) THEN
        call end_program("cla_objt:0009 - capacity_market_pointer " //
     + "at point of save is invalid. Value: " //
     + trim(itos(int(capacity_market_pointer_ocl))))


                  endif
            file_name=trim(get_filename_from_unit(11))
            call write_log_entry("cla_objt:0028", "Writing to " //
     + trim(file_name) // " (unit 11).")

                                    WRITE(11,REC=IREC) DELETE,UNIT_NAME,CL_LOAD_TYPE,
     +               EXPENSE_ASSIGNMENT,EXPENSE_COLLECTION,
     +               GENGRP,FRAOWN,
     +               ONLIMO,ONLIYR,OFLIMO,OFLIYR,FUELMX,PRFCST,PRESCR,
     +               SEFCST,SDESCR,FUELADJ,EFOR,MAINRT,VARCST,VRESCR,
     +               FIXED_COST,FIXED_COST_ESCALATOR,
     +               DISPADJ,HRFACT,MW,PLANNING_FACTOR,COEFF,
     +               AI_CAPACITY_RATE,AI_CAPACITY_ESCALATOR,
     +               AI_ENERGY_RATE,AI_ENERGY_ESCALATOR,
     +               AI_REMAINING_LIFE,
     +               P_SO2,P_NOX_BK1,P_CO2,POOL_FRAC_OWN,
     +               P_OTHER2,P_OTHER3,PHASE_I_STR,DISP_ADJ2_OUT,
     +               RESOURCE_ID,P_FUEL_SUPPLY_ID,P_NOX_BK2_OUT,
     +               SEC_EMISS_PTR,EMISS_FUEL_COST,EMISS_FUEL_ESCAL,
     +               EMISS_FUEL_EMISS_PTR,EMISS_BLENDING_RATE,
     +               PRIM_FUEL_EMISS_PTR,
     +               PRIM_FUEL_TYPE,FUEL_TYPES,TIE_GROUP,
     +               MONTHLY_CAPACITY_POINTER,
     +               ANNUAL_CL_FIXED_COST,
     +               ANNUAL_CL_FIXED_COST_ESC,
     +               MAINT_DAYS,DISPATCH_MULT,
     +               EXCESS_ENERGY_SALES,
     +               AI_TAX_LIFE,
     +               AI_ADR_TAX_LIFE,
     +               ASSET_CLASS_NUM,
     +               ASSET_CLASS_VECTOR,
     +               INTRA_COMPANY_CLASS_ID,
     +               INTRA_COMPANY_TRANSACTION,
     +               SPECIAL_UNIT_ID,
     +               MINIMUM_CAPACITY_FACTOR,
     +               MAXIMUM_CAPACITY_FACTOR,
     +               TRANSACTION_GROUP_ID,
     +               DAY_TYPE_ID,
     +               UNIT_ACTIVE,
     +               BASECASE_PLANT_ID,
     +               BASECASE_UNIT_ID,
     +               BASECASE_MARKET_AREA_ID,
     +               BASECASE_TRANS_AREA_ID,
     +               BASECASE_PLANT_NERC_SUB_ID,
     +               BASECASE_PLANT_OWNER_ID,
     +               UTILITY_OWNED,
     +               MONTHLY_FUEL_INDEX,
     +               START_UP_COSTS,
     +               START_UP_LOGIC,
     +               RAMP_RATE,
     +               MIN_DOWN_TIME,
     +               REPORT_THIS_UNIT,
     +               P_FUEL_DELIVERY,
     +               p_fuel_deliv_2_ord,
     +               P_FUEL_DELIVERY_3,
     +               MIN_UP_TIME,
     +               HESI_UNIT_ID_NUM_ord,
     +               FOR_FREQUENCY,
     +               FOR_DURATION,
     +               WINTER_TOTAL_CAPACITY,
     +               CONTRIBUTES_TO_SPIN,
     +               H2_UNIT_ID_NUM,
     +               POWERDAT_PLANT_ID,
     +               APPLY_NOX_SEASON_DATE,
     +               NOX_SEASON_DATE,
     +               RAMP_DOWN_RATE,
     +               NOX_CONTROL_PERCENT,
     +               NOX_CONTROL_DATE,
     +               EMERGENCY_CAPACITY,
     +               EMERGENCY_HEATRATE,
     +               MARKET_RESOURCE,
     +               PRIMARY_MOVER_STR,
     +               COUNTY,
     +               STATE_PROVINCE,
     +               NOX_VOM,
     +               NOX_FOM,
     +               S_FUEL_DELIVERY,
     +               s_fuel_delivery_2,
     +               s_fuel_delivery_3,
     +               CONSTANT_POLY,
     +               FIRST_POLY,
     +               SECOND_POLY,
     +               THIRD_POLY,
     +               USE_POLY_HEAT_RATES,
     +               NEWGEN_UNIT_STATUS,
     +               MONTHLY_MUST_RUN_VECTOR,
     +               EMISSION_MARKET_LINK,
     +               WVPA_RATE_TRACKER,
     +               ALLOW_DECOMMIT,
     +               MIN_SPIN_CAP,
     +               MAX_SPIN_CAP,
     +               WVPA_RES_TRACKER,
     +               WVPA_FUEL_TRACKER,
     +               MONTE_OR_FREQ,
     +               WVPA_MEM_TRACKER,
     +               MONTHLY_DECOMMIT_VECTOR,
     +               TRANS_BUS_ID,
     +               SOX_CONTROL_PERCENT,
     +               SOX_CONTROL_DATE,
     +               SOX_VOM,
     +               SOX_FOM,
     +               LATITUDE,
     +               LONGITUDE,
     +               MW_INSIDE_FENCE,
     +               INTER_BLOCKS,     ! 152-154
! SP CapEx Variables added 11/17/05 MSG
     +               FIRST_YEAR_DECOMM_AVAIALABLE,
     +               DECOMMISSIONING_BASE_YR_COST,            ! 156
     +               DECOMMISSIONING_COST_ESCALATION,
     +               ANNUAL_ENERGY_PLANNING_FACTOR,
     +               AGGREGATE_THIS_UNIT,               ! 159
     +               NAME_PLATE_CAPACITY,
     +               FUEL_BLENDING_IS,
     +               FuelRatio,
     +               BlendableFuelsPtr, ! 167
     +               FuelTransportationCost,
     +               BlendedEnthalpyUp,
     +               BlendedEnthalpyLo,
     +               BlendedSO2Up,                  ! 178
     +               BettermentProjectID,
     +               DECOM_CONTINUING_COST,
     +               DECOM_CONT_COST_ESCALATION,
     +               EnrgPatternPointer, ! 181
     +               EMISSION_DATA_UNITS, ! 182
     +               EmissRedRate, ! 183-187
     +               EmissMaxRate,
     +               MaxEnergyLimit,
     +               TECH_TYPE,
     +               LINKED_BETTERMENT_OPTION,
     +               CO2_CONTROL_PERCENT,
     +               HG_CONTROL_PERCENT,
     +               OTHER3_CONTROL_PERCENT,
     +               CO2_CONTROL_DATE,
     +               HG_CONTROL_DATE,
     +               OTHER3_CONTROL_DATE,
     +               CO2_VOM,
     +               CO2_FOM,
     +               HG_VOM,
     +               HG_FOM,
     +               OTHER3_VOM,
     +               OTHER3_FOM, ! 209
     +               MARKET_FLOOR,
     +               MARKET_CEILING,
     +               START_UP_COSTS_ESCALATION,  ! 212
     +               CAPACITY_MARKET_TYPE,
     +               CAPACITY_MARKET_MONTH,
     +               capacity_market_pointer_ocl,
     +               CAPACITY_MARKET_COST_ASSIGN,
     +               CAPACITY_MARKET_EXP_COLLECT,
     +               CAPACITY_MARKET_COIN_ADJ_FACT,
     +               PRIMARY_FUEL_CATEGORY,
     +               SECONDARY_FUEL_CATEGORY,
     +               EMISSIONS_FUEL_CATEGORY,
     +               RPS_CONTRIBUTION_PERCENT,
     +               RETIREMENT_CANDIDATE,
     +               RETROFIT_CANDIDATE,
     +               RETROFIT_PROJECT_ID,
     +               CO2_BASIN_NAME,
     +               CO2_PIPELINE_DISTANCE,
     +               STATE_PROV_NAME, ! 228
     +               CO2_RETRO_HEAT_MULT,
     +               CO2_RETRO_CAP_MULT,
     +               THERMAL_GUID,
     +               THERMAL_PARENT_ID, ! 232
     +               THERMAL_AGGREGATED_UNIT,
     +               FIRST_RETIREMENT_YEAR,
     +               UNIT_TYPE,
     +               UNIT_TYPE_CATEGORY,
     +               RPS_PROGRAM_NUMBER
               ENDDO
               IF(IOS /= 0) EXIT
            ENDDO
            ACTIVE_CL_RECORDS(FILE_ID) = IREC
            CLOSE(10)
            CLOSE(11)
         ELSE IF(INDEX(BASE_FILE_NAME,'NONE') == 0) THEN
            CALL STOP_NOFILE(FILE_TYPE,FILE_NAME)
         ENDIF
      ENDDO ! FILE_ID FIRST PASS
      IF(NUCLEAR_UNITS_ACTIVE > 0) THEN
         CALL SETUP_GENERATING_INFORMATION(NUCLEAR_UNITS_ACTIVE)
         DO FILE_ID = 0, MAX_CL_FILES-1
            IF(NUC_UNITS_IN_FILE(FILE_ID) > 0) THEN
               BINARY_FILE_NAME = CL_FILE_BINARY_NAMES(FILE_ID)
               other_filename=trim(OUTPUT_DIRECTORY())//
     +                     "BC"//trim(BINARY_FILE_NAME)//".BIN"

               OPEN(11,FILE=other_filename,
     +                     ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
           file_name=get_filename_from_unit(11)
            call write_log_entry("cla_objt:0027", "Reading from " //
     +          trim(file_name) // " (unit 11).")

               DO I = 1, NUC_UNITS_IN_FILE(FILE_ID)
                  IREC = NUC_RECORD_POSITION(I,FILE_ID)
!                 ! second read
                  READ(11,REC=IREC) DELETE,UNIT_NAME,CL_LOAD_TYPE,
     +               EXPENSE_ASSIGNMENT,EXPENSE_COLLECTION,
     +               GENGRP,FRAOWN,ONLIMO,ONLIYR,OFLIMO,OFLIYR,FUELMX,
     +               PRFCST,PRESCR,SEFCST,SDESCR,FUELADJ,EFOR,MAINRT,
     +               VARCST,VRESCR,FIXED_COST,FIXED_COST_ESCALATOR,
     +               DISPADJ,HRFACT,MW,PLANNING_FACTOR,COEFF,
     +               AI_CAPACITY_RATE,AI_CAPACITY_ESCALATOR,
     +               AI_ENERGY_RATE,AI_ENERGY_ESCALATOR,
     +               AI_REMAINING_LIFE,
     +               P_SO2,P_NOX_BK1,P_CO2,POOL_FRAC_OWN,
     +               P_OTHER2,P_OTHER3,PHASE_I_STR,DISP_ADJ2_OUT,
     +               RESOURCE_ID,P_FUEL_SUPPLY_ID,P_NOX_BK2,
     +               SEC_EMISS_PTR,EMISS_FUEL_COST,EMISS_FUEL_ESCAL,
     +               EMISS_FUEL_EMISS_PTR,EMISS_BLENDING_RATE,
     +               PRIM_FUEL_EMISS_PTR,
     +               PRIM_FUEL_TYPE,FUEL_TYPES,TIE_GROUP,
     +               MONTHLY_CAPACITY_POINTER,
     +               ANNUAL_CL_FIXED_COST,
     +               ANNUAL_CL_FIXED_COST_ESC,
     +               MAINT_DAYS,DISPATCH_MULT,
     +               EXCESS_ENERGY_SALES,
     +               AI_TAX_LIFE,
     +               AI_ADR_TAX_LIFE,
     +               ASSET_CLASS_NUM,
     +               ASSET_CLASS_VECTOR,
     +               INTRA_COMPANY_CLASS_ID,
     +               INTRA_COMPANY_TRANSACTION,
     +               SPECIAL_UNIT_ID,
     +               MINIMUM_CAPACITY_FACTOR,
     +               MAXIMUM_CAPACITY_FACTOR,
     +               TRANSACTION_GROUP_ID,
     +               DAY_TYPE_ID,
     +               UNIT_ACTIVE,
     +               BASECASE_PLANT_ID,
     +               BASECASE_UNIT_ID,
     +               BASECASE_MARKET_AREA_ID,
     +               BASECASE_TRANS_AREA_ID,
     +               BASECASE_PLANT_NERC_SUB_ID,
     +               BASECASE_PLANT_OWNER_ID,
     +               UTILITY_OWNED,
     +               MONTHLY_FUEL_INDEX,
     +               START_UP_COSTS,
     +               START_UP_LOGIC,
     +               RAMP_RATE,
     +               MIN_DOWN_TIME,
     +               REPORT_THIS_UNIT,
     +               P_FUEL_DELIVERY,
     +               p_fuel_deliv_2_ord,
     +               P_FUEL_DELIVERY_3,
     +               MIN_UP_TIME,
     +               HESI_UNIT_ID_NUM_ord,
     +               FOR_FREQUENCY,
     +               FOR_DURATION,
     +               WINTER_TOTAL_CAPACITY,
     +               CONTRIBUTES_TO_SPIN,
     +               H2_UNIT_ID_NUM,
     +               POWERDAT_PLANT_ID,
     +               APPLY_NOX_SEASON_DATE,
     +               NOX_SEASON_DATE,
     +               RAMP_DOWN_RATE,
     +               NOX_CONTROL_PERCENT,
     +               NOX_CONTROL_DATE,
     +               EMERGENCY_CAPACITY,
     +               EMERGENCY_HEATRATE,
     +               MARKET_RESOURCE,
     +               PRIMARY_MOVER_STR,
     +               COUNTY,
     +               STATE_PROVINCE,
     +               NOX_VOM,
     +               NOX_FOM,
     +               S_FUEL_DELIVERY,
     +               s_fuel_delivery_2,
     +               s_fuel_delivery_3,
     +               CONSTANT_POLY,
     +               FIRST_POLY,
     +               SECOND_POLY,
     +               THIRD_POLY,
     +               USE_POLY_HEAT_RATES,
     +               NEWGEN_UNIT_STATUS,
     +               MONTHLY_MUST_RUN_VECTOR,
     +               EMISSION_MARKET_LINK,
     +               WVPA_RATE_TRACKER,
     +               ALLOW_DECOMMIT,
     +               MIN_SPIN_CAP,
     +               MAX_SPIN_CAP,
     +               WVPA_RES_TRACKER,
     +               WVPA_FUEL_TRACKER,
     +               MONTE_OR_FREQ,
     +               WVPA_MEM_TRACKER,
     +               MONTHLY_DECOMMIT_VECTOR,
     +               TRANS_BUS_ID,
     +               SOX_CONTROL_PERCENT,
     +               SOX_CONTROL_DATE,
     +               SOX_VOM,
     +               SOX_FOM,
     +               LATITUDE,
     +               LONGITUDE,
     +               MW_INSIDE_FENCE,
     +               INTER_BLOCKS,     ! 152-153
! SP CapEx Variables added 11/17/05 MSG
     +               FIRST_YEAR_DECOMM_AVAIALABLE,
     +               DECOMMISSIONING_BASE_YR_COST,            ! 155
     +               DECOMMISSIONING_COST_ESCALATION,
     +               ANNUAL_ENERGY_PLANNING_FACTOR,
     +               AGGREGATE_THIS_UNIT,               ! 158
     +               NAME_PLATE_CAPACITY,
     +               FUEL_BLENDING_IS,
     +               FuelRatio,
     +               BlendableFuelsPtr, ! 167
     +               FuelTransportationCost,
     +               BlendedEnthalpyUp,
     +               BlendedEnthalpyLo,
     +               BlendedSO2Up,                  ! 177
     +               BettermentProjectID,
     +               DECOM_CONTINUING_COST,
     +               DECOM_CONT_COST_ESCALATION,
     +               EnrgPatternPointer, ! 181
     +               EMISSION_DATA_UNITS,
     +               EmissRedRate,
     +               EmissMaxRate,
     +               MaxEnergyLimit,
     +               TECH_TYPE,
     +               LINKED_BETTERMENT_OPTION,
     +               CO2_CONTROL_PERCENT,
     +               HG_CONTROL_PERCENT,
     +               OTHER3_CONTROL_PERCENT,
     +               CO2_CONTROL_DATE,
     +               HG_CONTROL_DATE,
     +               OTHER3_CONTROL_DATE,
     +               CO2_VOM,
     +               CO2_FOM,
     +               HG_VOM,
     +               HG_FOM,
     +               OTHER3_VOM,
     +               OTHER3_FOM, ! 209
     +               MARKET_FLOOR,
     +               MARKET_CEILING,
     +               START_UP_COSTS_ESCALATION,  ! 212
     +               CAPACITY_MARKET_TYPE,
     +               CAPACITY_MARKET_MONTH,
     +               capacity_market_pointer_ocl,
     +               CAPACITY_MARKET_COST_ASSIGN,
     +               CAPACITY_MARKET_EXP_COLLECT,
     +               CAPACITY_MARKET_COIN_ADJ_FACT,
     +               PRIMARY_FUEL_CATEGORY,
     +               SECONDARY_FUEL_CATEGORY,
     +               EMISSIONS_FUEL_CATEGORY,
     +               RPS_CONTRIBUTION_PERCENT,
     +               RETIREMENT_CANDIDATE,
     +               RETROFIT_CANDIDATE,
     +               RETROFIT_PROJECT_ID,
     +               CO2_BASIN_NAME,
     +               CO2_PIPELINE_DISTANCE,
     +               STATE_PROV_NAME, ! 228
     +               CO2_RETRO_HEAT_MULT,
     +               CO2_RETRO_CAP_MULT,
     +               THERMAL_GUID,
     +               THERMAL_PARENT_ID, ! 232
     +               THERMAL_AGGREGATED_UNIT,
     +               FIRST_RETIREMENT_YEAR,
     +               UNIT_TYPE,
     +               UNIT_TYPE_CATEGORY,
     +               RPS_PROGRAM_NUMBER
!

!       ! After second read

                  ONLINE = 100*(ONLIYR - 1900) + ONLIMO
                  OFLINE = 100*(OFLIYR - 1900) + OFLIMO
                  CALL CAL_GENERATING_INFORMATION(RESOURCE_ID,
     +                                         FRAOWN,
     +                                         MW(2),
     +                                        MONTHLY_CAPACITY_POINTER,
     +                                         EFOR,
     +                                         MAINRT,
     +                                         ONLINE,
     +                                         OFLINE,
     +                                         UNIT_NAME,
     +                                         PRIM_FUEL_TYPE)
               ENDDO
               CLOSE(11)
            ENDIF
         ENDDO ! FILE_ID FOR NUC UNITS
      ENDIF ! NUC UNITS IN FILES
      RETURN

!***********************************************************************
!
!          ROUTINE TO CREATE OVERLAY FILES
!          COPYRIGHT (C) 1984-88  M.S. GERBER & ASSOCIATES, INC.
!          COPYRIGHT (C) 1991-92  M.S. GERBER & ASSOCIATES, INC.
!
!***********************************************************************
!
! OVERLAY THE (FOSSIL-FUELED) CAPACITY-LIMITED-UNITS FILE
!***************************************************************
      ENTRY CL_MAKEOVL(OVERLAY_FAMILY_NAME,FILE_NUMBER)
!***************************************************************
      IF(.NOT. ACTIVE_BASE_CL_FILES(FILE_NUMBER)) RETURN
      FILE_CODE = CL_FILE_CODES(FILE_NUMBER)
      BINARY_FILE_NAME = CL_FILE_BINARY_NAMES(FILE_NUMBER)
      IF(LAHEY_LF95()) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSE
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         CALL LOCATE(10,51)
      ENDIF
      FILE_NAME = trim(OUTPUT_DIRECTORY())//FILE_CODE//"O"//
     +                               trim(OVERLAY_FAMILY_NAME)//".DAT"
      OPEN(10,FILE=FILE_NAME)
      call write_log_entry("cla_objt:0027", "Reading from " //
     +    trim(file_name) // " (unit 10).")

      READ(10,*) DELETE
      INUNIT = 12
      NUCLEAR_UNITS_ACTIVE = 0
      IF(FOSSILOL(FILE_NUMBER) == 'BC') THEN
         other_filename=trim(OUTPUT_DIRECTORY())//"BC"//
     +                      trim(BINARY_FILE_NAME)//".BIN"

         OPEN(11,FILE=other_filename,
     +                      ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
         INUNIT = 11
      ENDIF
      other_filename=trim(OUTPUT_DIRECTORY())//"OL"//
     +                      trim(BINARY_FILE_NAME)//".BIN"

      OPEN(12,FILE=other_filename,
     +                      ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
      IREC = 0
            file_name=get_filename_from_unit(10)
            call write_log_entry("cla_objt:0100", "Reading from " //
     +          trim(file_name) // " (unit 11).")

      DO
         DO
!           !third read
            READ(10,'(A)',IOSTAT=IOS) RECLN
            IF(RECLN(1:1) == '7') EXIT
            IREC = IREC + 1
            READ(INUNIT,REC=IREC,IOSTAT=IOS_BASE) DELETE,UNIT_NAME,
     +         CL_LOAD_TYPE,EXPENSE_ASSIGNMENT,EXPENSE_COLLECTION,
     +         GENGRP,FRAOWN,ONLIMO,ONLIYR,OFLIMO,OFLIYR,FUELMX,PRFCST,
     +         PRESCR,SEFCST,SDESCR,FUELADJ,EFOR,MAINRT,VARCST,VRESCR,
     +         FIXED_COST,FIXED_COST_ESCALATOR,
     +         DISPADJ,HRFACT,MW,PLANNING_FACTOR,COEFF,
     +         AI_CAPACITY_RATE,AI_CAPACITY_ESCALATOR,AI_ENERGY_RATE,
     +         AI_ENERGY_ESCALATOR,AI_REMAINING_LIFE,
     +         P_SO2,P_NOX,P_PARTICULATES,POOL_FRAC_OWN,
     +         P_OTHER2,P_OTHER3,PHASE_I_STR,DISPADJ2,
     +         RESOURCE_ID,P_FUEL_SUPPLY_ID,
     +        P_NOX_BK2,SEC_EMISS_PTR,EMISS_FUEL_COST,EMISS_FUEL_ESCAL,
     +         EMISS_FUEL_EMISS_PTR,EMISS_BLENDING_RATE,
     +         PRIM_FUEL_EMISS_PTR,
     +         PRIM_FUEL_TYPE,FUEL_TYPES,TIE_GROUP,
     +         MONTHLY_CAPACITY_POINTER,
     +         ANNUAL_CL_FIXED_COST,
     +         ANNUAL_CL_FIXED_COST_ESC,
     +         MAINT_DAYS,DISPATCH_MULT,
     +         EXCESS_ENERGY_SALES,
     +         AI_TAX_LIFE,
     +         AI_ADR_TAX_LIFE,
     +         ASSET_CLASS_NUM,
     +         ASSET_CLASS_VECTOR,
     +         INTRA_COMPANY_CLASS_ID,
     +         INTRA_COMPANY_TRANSACTION,
     +         SPECIAL_UNIT_ID,
     +         MINIMUM_CAPACITY_FACTOR,
     +         MAXIMUM_CAPACITY_FACTOR,
     +         TRANSACTION_GROUP_ID,
     +         DAY_TYPE_ID,
     +         UNIT_ACTIVE,
     +         BASECASE_PLANT_ID,
     +         BASECASE_UNIT_ID,
     +         BASECASE_MARKET_AREA_ID,
     +         BASECASE_TRANS_AREA_ID,
     +         BASECASE_PLANT_NERC_SUB_ID,
     +         BASECASE_PLANT_OWNER_ID,
     +         UTILITY_OWNED,
     +         MONTHLY_FUEL_INDEX,
     +         START_UP_COSTS,
     +         START_UP_LOGIC,
     +         RAMP_RATE,
     +         MIN_DOWN_TIME,
     +         REPORT_THIS_UNIT,
     +         P_FUEL_DELIVERY,
     +         p_fuel_deliv_2_ord,
     +         P_FUEL_DELIVERY_3,
     +         MIN_UP_TIME,
     +         HESI_UNIT_ID_NUM_ord,
     +         FOR_FREQUENCY,
     +         FOR_DURATION,
     +         WINTER_TOTAL_CAPACITY,
     +         CONTRIBUTES_TO_SPIN,
     +         H2_UNIT_ID_NUM,
     +         POWERDAT_PLANT_ID,
     +         APPLY_NOX_SEASON_DATE,
     +         NOX_SEASON_DATE,
     +         RAMP_DOWN_RATE,
     +         NOX_CONTROL_PERCENT,
     +         NOX_CONTROL_DATE,
     +         EMERGENCY_CAPACITY,
     +         EMERGENCY_HEATRATE,
     +         MARKET_RESOURCE,
     +         PRIMARY_MOVER_STR,
     +         COUNTY,
     +         STATE_PROVINCE,
     +         NOX_VOM,
     +         NOX_FOM,
     +         S_FUEL_DELIVERY,
     +         s_fuel_delivery_2,
     +         s_fuel_delivery_3,
     +         CONSTANT_POLY,
     +         FIRST_POLY,
     +         SECOND_POLY,
     +         THIRD_POLY,
     +         USE_POLY_HEAT_RATES,
     +         NEWGEN_UNIT_STATUS,
     +         MONTHLY_MUST_RUN_VECTOR,
     +         EMISSION_MARKET_LINK,
     +         WVPA_RATE_TRACKER,
     +         ALLOW_DECOMMIT,
     +         MIN_SPIN_CAP,
     +         MAX_SPIN_CAP,
     +         WVPA_RES_TRACKER,
     +         WVPA_FUEL_TRACKER,
     +         MONTE_OR_FREQ,
     +         WVPA_MEM_TRACKER,
     +         MONTHLY_DECOMMIT_VECTOR,
     +         TRANS_BUS_ID,
     +         SOX_CONTROL_PERCENT,
     +         SOX_CONTROL_DATE,
     +         SOX_VOM,
     +         SOX_FOM,
     +         LATITUDE,
     +         LONGITUDE,
     +         MW_INSIDE_FENCE,
     +         INTER_BLOCKS,     ! 152-153
! SP CapEx Variables added 11/17/05 MSG
     +         FIRST_YEAR_DECOMM_AVAIALABLE,
     +         DECOMMISSIONING_BASE_YR_COST,            ! 155
     +         DECOMMISSIONING_COST_ESCALATION,
     +         ANNUAL_ENERGY_PLANNING_FACTOR,
     +         AGGREGATE_THIS_UNIT,               ! 158
     +         NAME_PLATE_CAPACITY,
     +         FUEL_BLENDING_IS,
     +         FuelRatio,
     +         BlendableFuelsPtr, ! 167
     +         FuelTransportationCost,
     +         BlendedEnthalpyUp,
     +         BlendedEnthalpyLo,
     +         BlendedSO2Up,                  ! 177
     +         BettermentProjectID,
     +         DECOM_CONTINUING_COST,
     +         DECOM_CONT_COST_ESCALATION,
     +         EnrgPatternPointer, ! 181
     +         EMISSION_DATA_UNITS,
     +         EmissRedRate,
     +         EmissMaxRate,
     +         MaxEnergyLimit,
     +         TECH_TYPE,
     +         LINKED_BETTERMENT_OPTION,
     +         CO2_CONTROL_PERCENT,
     +         HG_CONTROL_PERCENT,
     +         OTHER3_CONTROL_PERCENT,
     +         CO2_CONTROL_DATE,
     +         HG_CONTROL_DATE,
     +         OTHER3_CONTROL_DATE,
     +         CO2_VOM,
     +         CO2_FOM,
     +         HG_VOM,
     +         HG_FOM,
     +         OTHER3_VOM,
     +         OTHER3_FOM, ! 209
     +         MARKET_FLOOR,
     +         MARKET_CEILING,
     +         START_UP_COSTS_ESCALATION,  ! 212
     +         CAPACITY_MARKET_TYPE,
     +         CAPACITY_MARKET_MONTH,
     +         capacity_market_pointer_ocl,
     +         CAPACITY_MARKET_COST_ASSIGN,
     +         CAPACITY_MARKET_EXP_COLLECT,
     +         CAPACITY_MARKET_COIN_ADJ_FACT,
     +         PRIMARY_FUEL_CATEGORY,
     +         SECONDARY_FUEL_CATEGORY,
     +         EMISSIONS_FUEL_CATEGORY,
     +         RPS_CONTRIBUTION_PERCENT,
     +         RETIREMENT_CANDIDATE,
     +         RETROFIT_CANDIDATE,
     +         RETROFIT_PROJECT_ID,
     +         CO2_BASIN_NAME,
     +         CO2_PIPELINE_DISTANCE,
     +         STATE_PROV_NAME, ! 228
     +         CO2_RETRO_HEAT_MULT,
     +         CO2_RETRO_CAP_MULT,
     +         THERMAL_GUID,
     +         THERMAL_PARENT_ID, ! 232
     +         THERMAL_AGGREGATED_UNIT,
     +         FIRST_RETIREMENT_YEAR,
     +         UNIT_TYPE,
     +         UNIT_TYPE_CATEGORY,
     +         RPS_PROGRAM_NUMBER

            IF(IOS_BASE /= 0) EXIT
            IF(IOS == 0) THEN
               RECLN = trim(RECLN)//',,,,,,,,,,,,,'//
     +                       ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'//
     +                       ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'//
     +                       ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               TEMP_EXPENSE_COLLECTION = EXPENSE_COLLECTION
!              ! third read
               READ(RECLN,*,ERR=200) DELETE,UNIT_NAME,CL_LOAD_TYPE,
     +               EXPENSE_ASSIGNMENT,TEMP_EXPENSE_COLLECTION,GENGRP,
     +               FRAOWN,ONLIMO,ONLIYR,OFLIMO,OFLIYR,FUELMX,PRFCST,
     +               PRESCR,SEFCST,SDESCR,FUELADJ,EFOR,MAINRT,VARCST,
     +               VRESCR,FIXED_COST,FIXED_COST_ESCALATOR,
     +               DISPADJ,HRFACT,MW,PLANNING_FACTOR,COEFF,DESC,
     +               P_SO2,P_NOX,P_PARTICULATES,POOL_FRAC_OWN,
     +               P_OTHER2,P_OTHER3,PHASE_I_STR,DISPADJ2,
     +               RESOURCE_ID,P_FUEL_SUPPLY_ID,
     +               P_NOX_BK2,SEC_EMISS_PTR,EMISS_FUEL_COST,
     +               EMISS_FUEL_ESCAL,EMISS_FUEL_EMISS_PTR,
     +               EMISS_BLENDING_RATE,PRIM_FUEL_EMISS_PTR,
     +               PRIM_FUEL_TYPE,FUEL_TYPES,TIE_GROUP,
     +               MONTHLY_CAPACITY_POINTER,
     +               AI_CAPACITY_RATE,AI_CAPACITY_ESCALATOR,
     +               AI_ENERGY_RATE,AI_ENERGY_ESCALATOR,
     +               AI_REMAINING_LIFE,
     +               ANNUAL_CL_FIXED_COST,
     +               ANNUAL_CL_FIXED_COST_ESC,
     +               MAINT_DAYS,DISPATCH_MULT,
     +               EXCESS_ENERGY_SALES,
     +               AI_TAX_LIFE,
     +               AI_ADR_TAX_LIFE,
     +               ASSET_CLASS_NUM,
     +               ASSET_CLASS_VECTOR,
     +               INTRA_COMPANY_CLASS_ID,
     +               INTRA_COMPANY_TRANSACTION,
     +               SPECIAL_UNIT_ID,
     +               MINIMUM_CAPACITY_FACTOR,
     +               MAXIMUM_CAPACITY_FACTOR,
     +               TRANSACTION_GROUP_ID,
     +               DAY_TYPE_ID,
     +               UNIT_ACTIVE,
     +               BASECASE_PLANT_ID,
     +               BASECASE_UNIT_ID,
     +               BASECASE_MARKET_AREA_ID,
     +               BASECASE_TRANS_AREA_ID,
     +               BASECASE_PLANT_NERC_SUB_ID,
     +               BASECASE_PLANT_OWNER_ID,
     +               UTILITY_OWNED,
     +               MONTHLY_FUEL_INDEX,
     +               START_UP_COSTS,
     +               START_UP_LOGIC,
     +               RAMP_RATE,
     +               MIN_DOWN_TIME,
     +               REPORT_THIS_UNIT,
     +               P_FUEL_DELIVERY,
     +               p_fuel_deliv_2_ord,
     +               P_FUEL_DELIVERY_3,
     +               MIN_UP_TIME,
     +               HESI_UNIT_ID_NUM_ord,
     +               FOR_FREQUENCY,
     +               FOR_DURATION,
     +               WINTER_TOTAL_CAPACITY,
     +               CONTRIBUTES_TO_SPIN,
     +               H2_UNIT_ID_NUM,
     +               POWERDAT_PLANT_ID,
     +               APPLY_NOX_SEASON_DATE,
     +               NOX_SEASON_DATE,
     +               RAMP_DOWN_RATE,
     +               NOX_CONTROL_PERCENT,
     +               NOX_CONTROL_DATE,
     +               EMERGENCY_CAPACITY,
     +               EMERGENCY_HEATRATE,
     +               MARKET_RESOURCE,
     +               PRIMARY_MOVER_STR,
     +               COUNTY,
     +               STATE_PROVINCE,
     +               NOX_VOM,
     +               NOX_FOM,
     +               S_FUEL_DELIVERY,
     +               s_fuel_delivery_2,
     +               s_fuel_delivery_3,
     +               CONSTANT_POLY,
     +               FIRST_POLY,
     +               SECOND_POLY,
     +               THIRD_POLY,
     +               USE_POLY_HEAT_RATES,
     +               NEWGEN_UNIT_STATUS,
     +               MONTHLY_MUST_RUN_VECTOR,
     +               EMISSION_MARKET_LINK,
     +               WVPA_RATE_TRACKER,
     +               ALLOW_DECOMMIT,
     +               MIN_SPIN_CAP,
     +               MAX_SPIN_CAP,
     +               WVPA_RES_TRACKER,
     +               WVPA_FUEL_TRACKER,
     +               MONTE_OR_FREQ,
     +               WVPA_MEM_TRACKER,
     +               MONTHLY_DECOMMIT_VECTOR,
     +               TRANS_BUS_ID,
     +               SOX_CONTROL_PERCENT,
     +               SOX_CONTROL_DATE,
     +               SOX_VOM,
     +               SOX_FOM,
     +               LATITUDE,
     +               LONGITUDE,
     +               MW_INSIDE_FENCE,
     +               INTER_BLOCKS,     ! 152-153
! SP CapEx Variables added 11/17/05 MSG
     +               FIRST_YEAR_DECOMM_AVAIALABLE,
     +               DECOMMISSIONING_BASE_YR_COST,            ! 155
     +               DECOMMISSIONING_COST_ESCALATION,
     +               ANNUAL_ENERGY_PLANNING_FACTOR,
     +               AGGREGATE_THIS_UNIT,               ! 158
     +               NAME_PLATE_CAPACITY,
     +               FUEL_BLENDING_IS,
     +               FuelRatio,
     +               BlendableFuelsPtr, ! 167
     +               FuelTransportationCost,
     +               BlendedEnthalpyUp,
     +               BlendedEnthalpyLo,
     +               BlendedSO2Up,                  ! 177
     +               BettermentProjectID,
     +               DECOM_CONTINUING_COST,
     +               DECOM_CONT_COST_ESCALATION,
     +               EnrgPatternPointer, ! 181
     +               EMISSION_DATA_UNITS,
     +               EmissRedRate,
     +               EmissMaxRate,
     +               MaxEnergyLimit,
     +               TECH_TYPE,
     +               LINKED_BETTERMENT_OPTION,
     +               CO2_CONTROL_PERCENT,
     +               HG_CONTROL_PERCENT,
     +               OTHER3_CONTROL_PERCENT,
     +               CO2_CONTROL_DATE,
     +               HG_CONTROL_DATE,
     +               OTHER3_CONTROL_DATE,
     +               CO2_VOM,
     +               CO2_FOM,
     +               HG_VOM,
     +               HG_FOM,
     +               OTHER3_VOM,
     +               OTHER3_FOM, ! 209
     +               MARKET_FLOOR,
     +               MARKET_CEILING,
     +               START_UP_COSTS_ESCALATION,  ! 212
     +               CAPACITY_MARKET_TYPE,
     +               CAPACITY_MARKET_MONTH,
     +               capacity_market_pointer_ocl,
     +               CAPACITY_MARKET_COST_ASSIGN,
     +               CAPACITY_MARKET_EXP_COLLECT,
     +               CAPACITY_MARKET_COIN_ADJ_FACT,
     +               PRIMARY_FUEL_CATEGORY,
     +               SECONDARY_FUEL_CATEGORY,
     +               EMISSIONS_FUEL_CATEGORY,
     +               RPS_CONTRIBUTION_PERCENT,
     +               RETIREMENT_CANDIDATE,
     +               RETROFIT_CANDIDATE,
     +               RETROFIT_PROJECT_ID,
     +               CO2_BASIN_NAME,
     +               CO2_PIPELINE_DISTANCE,
     +               STATE_PROV_NAME, ! 228
     +               CO2_RETRO_HEAT_MULT,
     +               CO2_RETRO_CAP_MULT,
     +               THERMAL_GUID,
     +               THERMAL_PARENT_ID, ! 232
     +               THERMAL_AGGREGATED_UNIT,
     +               FIRST_RETIREMENT_YEAR,
     +               UNIT_TYPE,
     +               UNIT_TYPE_CATEGORY,
     +               RPS_PROGRAM_NUMBER

!     ! after third read
      RPS_PROGRAM_NUMBER=RPS_PROGRAM_NUMBER ! debugstop
               IF(INDEX(TEMP_EXPENSE_COLLECTION,'BTL') /= 0) THEN
                  EXPENSE_COLLECTION = 'X'
               ELSE
                  EXPENSE_COLLECTION = TEMP_EXPENSE_COLLECTION(1:1)
               ENDIF
            ENDIF
            IF(CL_LOAD_TYPE(1:1) == NUCLEAR .AND. DELETE < 8 .AND.
     +               (UNIT_ACTIVE == 'T' .OR. UNIT_ACTIVE == 't')) THEN
                NUCLEAR_UNITS_ACTIVE = NUCLEAR_UNITS_ACTIVE + 1
            ENDIF
            file_name=get_filename_from_unit(12)
            call write_log_entry("cla_objt:0127", "Writing to " //
     +          trim(file_name) // " (unit 12).")

!           ! First write
            WRITE(12,REC=IREC) DELETE,UNIT_NAME,
     +         CL_LOAD_TYPE,EXPENSE_ASSIGNMENT,EXPENSE_COLLECTION,
     +         GENGRP,FRAOWN,ONLIMO,ONLIYR,OFLIMO,OFLIYR,FUELMX,PRFCST,
     +         PRESCR,SEFCST,SDESCR,FUELADJ,EFOR,MAINRT,VARCST,VRESCR,
     +         FIXED_COST,FIXED_COST_ESCALATOR,
     +         DISPADJ,HRFACT,MW,PLANNING_FACTOR,COEFF,
     +         AI_CAPACITY_RATE,AI_CAPACITY_ESCALATOR,AI_ENERGY_RATE,
     +         AI_ENERGY_ESCALATOR,AI_REMAINING_LIFE,
     +         P_SO2,P_NOX,P_PARTICULATES,POOL_FRAC_OWN,
     +         P_OTHER2,P_OTHER3,PHASE_I_STR,DISPADJ2,
     +         RESOURCE_ID,P_FUEL_SUPPLY_ID,
     +        P_NOX_BK2,SEC_EMISS_PTR,EMISS_FUEL_COST,EMISS_FUEL_ESCAL,
     +         EMISS_FUEL_EMISS_PTR,EMISS_BLENDING_RATE,
     +         PRIM_FUEL_EMISS_PTR,
     +         PRIM_FUEL_TYPE,FUEL_TYPES,TIE_GROUP,
     +         MONTHLY_CAPACITY_POINTER,
     +         ANNUAL_CL_FIXED_COST,
     +         ANNUAL_CL_FIXED_COST_ESC,
     +         MAINT_DAYS,DISPATCH_MULT,
     +         EXCESS_ENERGY_SALES,
     +         AI_TAX_LIFE,
     +         AI_ADR_TAX_LIFE,
     +         ASSET_CLASS_NUM,
     +         ASSET_CLASS_VECTOR,
     +         INTRA_COMPANY_CLASS_ID,
     +         INTRA_COMPANY_TRANSACTION,
     +         SPECIAL_UNIT_ID,
     +         MINIMUM_CAPACITY_FACTOR,
     +         MAXIMUM_CAPACITY_FACTOR,
     +         TRANSACTION_GROUP_ID,
     +         DAY_TYPE_ID,
     +         UNIT_ACTIVE,
     +         BASECASE_PLANT_ID,
     +         BASECASE_UNIT_ID,
     +         BASECASE_MARKET_AREA_ID,
     +         BASECASE_TRANS_AREA_ID,
     +         BASECASE_PLANT_NERC_SUB_ID,
     +         BASECASE_PLANT_OWNER_ID,
     +         UTILITY_OWNED,
     +         MONTHLY_FUEL_INDEX,
     +         START_UP_COSTS,
     +         START_UP_LOGIC,
     +         RAMP_RATE,
     +         MIN_DOWN_TIME,
     +         REPORT_THIS_UNIT,
     +         P_FUEL_DELIVERY,
     +         p_fuel_deliv_2_ord,
     +         P_FUEL_DELIVERY_3,
     +         MIN_UP_TIME,
     +         HESI_UNIT_ID_NUM_ord,
     +         FOR_FREQUENCY,
     +         FOR_DURATION,
     +         WINTER_TOTAL_CAPACITY,
     +         CONTRIBUTES_TO_SPIN,
     +         H2_UNIT_ID_NUM,
     +         POWERDAT_PLANT_ID,
     +         APPLY_NOX_SEASON_DATE,
     +         NOX_SEASON_DATE,
     +         RAMP_DOWN_RATE,
     +         NOX_CONTROL_PERCENT,
     +         NOX_CONTROL_DATE,
     +         EMERGENCY_CAPACITY,
     +         EMERGENCY_HEATRATE,
     +         MARKET_RESOURCE,
     +         PRIMARY_MOVER_STR,
     +         COUNTY,
     +         STATE_PROVINCE,
     +         NOX_VOM,
     +         NOX_FOM,
     +         S_FUEL_DELIVERY,
     +         s_fuel_delivery_2,
     +         s_fuel_delivery_3,
     +         CONSTANT_POLY,
     +         FIRST_POLY,
     +         SECOND_POLY,
     +         THIRD_POLY,
     +         USE_POLY_HEAT_RATES,
     +         NEWGEN_UNIT_STATUS,
     +         MONTHLY_MUST_RUN_VECTOR,
     +         EMISSION_MARKET_LINK,
     +         WVPA_RATE_TRACKER,
     +         ALLOW_DECOMMIT,
     +         MIN_SPIN_CAP,
     +         MAX_SPIN_CAP,
     +         WVPA_RES_TRACKER,
     +         WVPA_FUEL_TRACKER,
     +         MONTE_OR_FREQ,
     +         WVPA_MEM_TRACKER,
     +         MONTHLY_DECOMMIT_VECTOR,
     +         TRANS_BUS_ID,
     +         SOX_CONTROL_PERCENT,
     +         SOX_CONTROL_DATE,
     +         SOX_VOM,
     +         SOX_FOM,
     +         LATITUDE,
     +         LONGITUDE,
     +         MW_INSIDE_FENCE,
     +         INTER_BLOCKS,     ! 152-153
! SP CapEx Variables added 11/17/05 MSG
     +         FIRST_YEAR_DECOMM_AVAIALABLE,
     +         DECOMMISSIONING_BASE_YR_COST,            ! 155
     +         DECOMMISSIONING_COST_ESCALATION,
     +         ANNUAL_ENERGY_PLANNING_FACTOR,
     +         AGGREGATE_THIS_UNIT,               ! 158
     +         NAME_PLATE_CAPACITY,
     +         FUEL_BLENDING_IS,
     +         FuelRatio,
     +         BlendableFuelsPtr, ! 167
     +         FuelTransportationCost,
     +         BlendedEnthalpyUp,
     +         BlendedEnthalpyLo,
     +         BlendedSO2Up,                  ! 177
     +         BettermentProjectID,
     +         DECOM_CONTINUING_COST,
     +         DECOM_CONT_COST_ESCALATION,
     +         EnrgPatternPointer, ! 181
     +         EMISSION_DATA_UNITS,
     +         EmissRedRate,
     +         EmissMaxRate,
     +         MaxEnergyLimit,
     +         TECH_TYPE,
     +         LINKED_BETTERMENT_OPTION,
     +         CO2_CONTROL_PERCENT,
     +         HG_CONTROL_PERCENT,
     +         OTHER3_CONTROL_PERCENT,
     +         CO2_CONTROL_DATE,
     +         HG_CONTROL_DATE,
     +         OTHER3_CONTROL_DATE,
     +         CO2_VOM,
     +         CO2_FOM,
     +         HG_VOM,
     +         HG_FOM,
     +         OTHER3_VOM,
     +         OTHER3_FOM, ! 209
     +         MARKET_FLOOR,
     +         MARKET_CEILING,
     +         START_UP_COSTS_ESCALATION,  ! 212
     +         CAPACITY_MARKET_TYPE,
     +         CAPACITY_MARKET_MONTH,
     +         capacity_market_pointer_ocl,
     +         CAPACITY_MARKET_COST_ASSIGN,
     +         CAPACITY_MARKET_EXP_COLLECT,
     +         CAPACITY_MARKET_COIN_ADJ_FACT,
     +         PRIMARY_FUEL_CATEGORY,
     +         SECONDARY_FUEL_CATEGORY,
     +         EMISSIONS_FUEL_CATEGORY,
     +         RPS_CONTRIBUTION_PERCENT,
     +         RETIREMENT_CANDIDATE,
     +         RETROFIT_CANDIDATE,
     +         RETROFIT_PROJECT_ID,
     +         CO2_BASIN_NAME,
     +         CO2_PIPELINE_DISTANCE,
     +         STATE_PROV_NAME, ! 228
     +         CO2_RETRO_HEAT_MULT,
     +         CO2_RETRO_CAP_MULT,
     +         THERMAL_GUID,
     +         THERMAL_PARENT_ID, ! 232
     +         THERMAL_AGGREGATED_UNIT,
     +         FIRST_RETIREMENT_YEAR,
     +         UNIT_TYPE,
     +         UNIT_TYPE_CATEGORY,
     +         RPS_PROGRAM_NUMBER
         ENDDO
         IF(IOS_BASE /= 0) EXIT
      ENDDO
      FILE_NAME=get_filename_from_unit(10)
      call write_log_entry("Cla_objt:0023", "Closing " //
     + trim(file_name) // " (unit 10)")

      CLOSE(10)
            call write_log_entry("Cla_objt:0029", "Closing " //
     + trim(file_name) // " (unit 10)")

      CLOSE(12)
      NUC_UNITS_IN_OL_FILE(FILE_NUMBER) = NUCLEAR_UNITS_ACTIVE
      IF(FOSSILOL(FILE_NUMBER) == 'BC') CLOSE(11)
      FOSSILOL(FILE_NUMBER) = 'OL'
      RETURN

  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from Cla_objt SIID20'
      call end_program(er_message)
!
!***************************************************************
      ENTRY SET_FOSSILOL_TO_OL
!***************************************************************
         DO FILE_ID = 0, MAX_CL_FILES-1
            FOSSILOL(FILE_ID) = 'OL'
!
            COMMAND1 = trim(OUTPUT_DIRECTORY())//'BC'//
     +                     trim(CL_FILE_BINARY_NAMES(FILE_ID))//'.BIN'
            COMMAND2 = trim(OUTPUT_DIRECTORY())//'OL'//
     +                     trim(CL_FILE_BINARY_NAMES(FILE_ID))//'.BIN'
            call write_log_entry("cla_objt:2001",
     + "Copying " // trim(Command1) // " to " // trim(command2)) !debugstop

            CALL COPY_FILE_2_FILE(COMMAND1,COMMAND2) ! debugstop
            NUC_UNITS_IN_OL_FILE(FILE_ID) = NUC_UNITS_IN_FILE(FILE_ID)
         ENDDO
      RETURN
!***************************************************************
      ENTRY RESET_FOSSILOL
!***************************************************************
         DO FILE_ID = 0, MAX_CL_FILES-1
            FOSSILOL(FILE_ID) = 'BC'
         ENDDO
      RETURN
!
!***************************************************************
      ENTRY OPEN_CL_FILE(UNIT_NO,R_ACTIVE_CL_RECORDS,FILE_NUMBER)
!***************************************************************
         IF(ACTIVE_CL_RECORDS(FILE_NUMBER) > 0) THEN
            other_filename=trim(OUTPUT_DIRECTORY())//
     +                FOSSILOL(FILE_NUMBER)//
     +                trim(CL_FILE_BINARY_NAMES(FILE_NUMBER))//".BIN"

            OPEN(UNIT_NO,FILE=other_filename,
     +                     ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
         ENDIF
         R_ACTIVE_CL_RECORDS = MAX(ACTIVE_CL_RECORDS(FILE_NUMBER),0)
      RETURN
!
!***************************************************************
      ENTRY RETURN_NUCLEAR_UNITS_ACTIVE(R_NUM_OF_NUCLEAR_UNITS)
!***************************************************************
         NUCLEAR_UNITS_ACTIVE = 0
         DO FILE_ID = 0, MAX_CL_FILES-1
            NUCLEAR_UNITS_ACTIVE = NUCLEAR_UNITS_ACTIVE
     +                             + NUC_UNITS_IN_OL_FILE(FILE_ID)
         ENDDO
         R_NUM_OF_NUCLEAR_UNITS = NUCLEAR_UNITS_ACTIVE
      RETURN
!***************************************************************
      ENTRY DOES_CL_FILE_EXIST(R_CL_FILES_ARE_ACTIVE)
!***************************************************************
         R_CL_FILES_ARE_ACTIVE = CL_FILES_ARE_ACTIVE
      RETURN
 1000 FORMAT(A)
 1010 FORMAT('&',A)
      END
!
!***********************************************************************
!
!          ROUTINE TO READ FOSSIL DIRECT ACESS BINARY FILES
!          COPYRIGHT (C) 1983, 84, 85  M.S. GERBER & ASSOCIATES, INC.
!
!***********************************************************************
!
!     THIS FILE IS BEFORE THE YEAR COUNTER LOOP. CAPACITY
!     LIMITED UNITS' CHARACTERISTICS ARE READ IN AND PLACED INTO THE
!     PROD-COM.MON AND PROD-2COM.MON FILES.
!
      FUNCTION CL_UNITS_READ()
!
      use end_routine, only: end_program, er_message
      use SpinDriftLib
      use prod_arrays_dimensions
      use grx_data
      use expansion
      use logging
      use prim_mover_idx
      use miscmod
      use filename_tracker
      use grx_planning_routines
      use rptreccontrol
      use rps_data
      USE SPCapExVariables
      USE CLA_OBJT_ARRAYS
      USE conversion_routines
      USE LH_GLOBAL_VARIABLES
      USE TRANS_GROUP_VARIABLES
      USE GRX_PLANNING_ROUTINES
      USE IREC_ENDPOINT_CONTROL
      USE INTERNAL_OPERATION_SWITCHES
      USE CL_UNITS_READ_DATA
      use cla_decs
      use fuelused_decs
      use grxModules
      use kepcocom
      use hesi
      use annual_cl_unit
      use monthly_icap
      use cla_fuel
      use annl_nuc
      use trans_slope
      use mwunih
      USE SIZECOM
      use globecom
      use foshydi2com
      use aitvaprod
      use prod2com
      use prodcom
      implicit none
      SAVE

      INCLUDE 'NAMESCOM.MON'
      INCLUDE 'MTHNMCOM.MON'

      logical :: fe ! Can't be declared in entry; must be declared here.
      integer :: min_tmp
      REAL (KIND=4) :: MN_RATE,TEMP_REAL,
     +                 GET_NEW_UNIT_RETIRE_VALUE_BY_CM
      REAL (KIND=4) , ALLOCATABLE :: RETIRE_RETRO_UPLIFT_PER_TON(:),
     +       RETIRE_RETRO_CUM_CO2(:),
     +       NEW_UNIT_RETIRE_VALUE_BY_CM(:),
     +       NUC_FUEL_LEASED_BURN_BY_CLASS(:),
     +       NUC_FUEL_OWNED_BURN_BY_CLASS(:),
     +       NUCLEAR_MWH_BY_CLASS(:),
     +       NUCLEAR_MMBTU_BY_CLASS(:),
     +       INTRA_COMPANY_NF_BURN(:),
     +       ANNUAL_CL_MAINT_MW(:),
     +       MAINT_DAYS(:),
     +       MAINTENANCE_DAYS(:),
     +       MAINT_INDEX_MW(:),
     +       MAINT_INDEX_DAYS(:),
     +       ASSET_CLASS_LIST(:),
     +       ASSET_ALLOCATION_LIST(:),
     +       ANNUAL_CL_MAINTENANCE(:,:),
     +      MON_MDS_CL_UNIT_CAPACITY(:,:),
     +      MON_MDS_CL_UNIT_ENERGY(:,:),
     +      MON_MDS_CL_UNIT_FUEL_COST(:,:),
     +      MON_MDS_CL_UNIT_VAR_COST(:,:),
     +      MON_MDS_CL_UNIT_FIXED_COST(:,:),
     +      MON_MDS_NUC_FUEL_ADDER_COST(:,:),
     +      MON_MDS_ECO_SALES_REV_FROM(:,:),
     +      MON_MDS_ECO_SALES_ENRG_FROM(:,:),
     +      MON_MDS_ECO_PUCH_COST_FROM(:,:),
     +      MON_MDS_ECO_PUCH_ENRG_FROM(:,:),
     +      MON_MDS_ICAP_REVENUES(:,:),
     +      CL_ANN_CLASS_FUEL_COST(:,:),
     +     CL_ANN_MULT_FUEL_COST(:,:),
     +     FE_ANN_MULT_FUEL_COST(:,:),
     +     CL_ANN_MULT_VAR_COST(:,:),
     +     CL_ANN_MULT_FIXED_COST(:,:),
     +     CL_ANN_MULT_REVENUE(:,:),
     +     CL_ANN_MULT_CAPACITY(:,:),
     +     CL_ANN_MULT_PURCHASES(:,:),
     +     CL_ANN_MULT_ENERGY(:,:),
     +     NF_FUEL_LEASED_BY_CLASS(:,:),
     +     NF_FUEL_OWNED_BY_CLASS(:,:),
     +     CL_ANN_CLASS_VAR_COST(:,:),
     +     CL_ANN_CLASS_FIXED_COST(:,:),
     +     CL_ANN_CLASS_REVENUE(:,:),
     +     CL_ANN_CLASS_CAPACITY(:,:),
     +     CL_ANN_CLASS_PURCHASES(:,:),
     +     CL_ANN_CLASS_ENERGY(:,:),
     +     CL_ANN_CLASS_EMISSIONS(:,:),
     +     INTRA_COMPANY_REVENUES(:,:),
     +     MON_MDS_NUC_FUEL_LEASE_BURN_BC(:,:),
     +     MON_MDS_NUC_FUEL_OWNED_BURN_BC(:,:),
     +     MON_MDS_NUCLEAR_MWH_BY_CLASS(:,:),
     +     MON_MDS_NUCLEAR_MMBTU_BY_CLASS(:,:),
     +     MON_MDS_ICAP_REV_BY_CLASS(:,:),
     +     MONTHLY_AC_WHOLESALE_PROD_COST(:,:),
     +     MONTHLY_AC_ECITY_VAR_PROD_COST(:,:),
     +     MONTHLY_AC_ECITY_NEW_FIX_COST(:,:),
     +     MAINT_INDEX_MONTHLY_MW(:,:),
     +     MON_MDS_CL_CLASS_FUEL_COST(:,:,:),
     +     ANNUAL_CL_CAPACITY(:,:,:),
     +     MON_MDS_CL_UNIT_EMISSIONS_COST(:,:,:),
     +     MON_MDS_CL_UNIT_EMISSIONS(:,:,:),
     +     MON_MDS_CL_MULT_FUEL_COST(:,:,:),
     +     MON_MDS_CL_MULT_VAR_COST(:,:,:),
     +     MON_MDS_CL_MULT_FIXED_COST(:,:,:),
     +     MON_MDS_CL_MULT_CAPACITY(:,:,:),
     +     MON_MDS_CL_MULT_ENERGY(:,:,:),
     +     MON_MDS_CL_MULT_PURCHASES(:,:,:),
     +     MON_MDS_CL_CLASS_VAR_COST(:,:,:),
     +     MON_MDS_CL_CLASS_FIXED_COST(:,:,:),
     +     MON_MDS_CL_CLASS_REVENUE(:,:,:),
     +     MON_MDS_CL_CAP_REVENUE(:,:,:),
     +     MON_MDS_CL_CLASS_CAPACITY(:,:,:),
     +     MON_MDS_CL_CLASS_PURCHASES(:,:,:),
     +     MON_MDS_CL_CAP_PURCHASES(:,:,:),
     +     MON_MDS_CL_CLASS_ENERGY(:,:,:),
     +     MON_MDS_CL_CLASS_EMISSIONS(:,:,:),
     +     MON_MDS_CL_CLASS_EMISSIONS_COST(:,:,:),
     +     MON_MDS_INTRA_COMPANY_REVENUES(:,:,:),
     +     MON_MDS_INTRA_COMPANY_NF_BURN(:,:),
     +     MON_MDS_NF_FUEL_LEASED_BC(:,:,:),
     +     MON_MDS_NF_FUEL_OWNED_BC(:,:,:)
!
! FIX TO MAINTENANCE SCHEDULER. 3/8/1.
!
      REAL (KIND=8) ,ALLOCATABLE :: MON_MDS_CL_UNIT_MMBTUS(:,:)

      integer :: gctg_result



      INTEGER (KIND=2) , ALLOCATABLE :: TG_2_PLANNING_AREA(:),
     +          RETIRE_RETRO_INDEX(:),
     +          TRANS_GROUP_FOR_DATA_BASE(:),
     +          RETIRE_RETRO_POSITION(:),
     +          RETIRE_RETRO_ABATE_INDEX(:),
     +          PURCHASE_ASSET_CLASS_ID(:),
     +          MARKET_RESOURCE_INDEX(:),
     +          MARKET_RESOURCE_COUNTER(:),
     +          ASSET_CLASS_POINTER(:),
     +          MAINT_INDEX(:),
     +          DAY_TYPE_TRANS_GROUP_PAIR(:,:),
     +          DATA_BASE_POSITION(:,:),
     +          MRX_TG_CAP_CURVE_INDEX(:,:),
     +          MRX_TG_CAP_UNIT_NO(:),
     +          MRX_TG_CAP_UNIT_INDEX(:)

      LOGICAL (KIND=1) :: RETROFIT_UNIT_IS_ACTIVE(MAX_CL_UNITS),
     +                    TEMP_L1,RETROFIT_PROJECT_ACTIVE,
     +                    CALC_MRX_RPS_CURVES
      INTEGER (KIND=2) :: CAP_LIMITED_CLASS_POINTER(1024),
     +                    TOP,BOTTOM,HALF,
     +                    CM_INDEX,GET_CM_INDEX_FROM_TG,
     +                    UnitTG,UnitCG
      CHARACTER (LEN=1) , ALLOCATABLE :: PURCHASE_POWER_ASSIGN(:)
      CHARACTER (LEN=1) :: INTRA_COMPANY_TRANSACTION(MAX_CL_UNITS)

      LOGICAL (KIND=1) , ALLOCATABLE :: MAINT_DIVISIBLE(:)
      CHARACTER (len=20) ::  SPECIAL_UNIT_ID(MAX_CL_UNITS)
      INTEGER (KIND=4) :: DA_ERR,CPO_REC,RESET_REC
      LOGICAL (KIND=4) :: temp_grx_converged,GRX_CO2_MARKET_LOGIC
      REAL (KIND=4) :: ITER_0_ECON_RETIRE_MARGIN(MAX_CL_UNITS),
     +                 LAST_ITER_GROSS_MARGIN(MAX_CL_UNITS)
      REAL (KIND=4) :: UNIT_ANNUAL_GROSS_MARGIN,RETIREMENT_MEASURE,
     +                 GET_MRX_TG_CAP_CURVE_PRICE,
     +                 GET_MRX_CM_CAP_CURVE_PRICE
      CHARACTER (LEN=48) :: RPT_UNIT_NAME
      REAL (KIND=4) :: RPT_RETIREMENTS_MW,RPT_RETIREMENTS_MWH,
     +                 RPT_RETIREMENTS_CO2,GRX_CO2_MARKET_TONS,
     +                 GRX_CO2_MARKET_PRICE
      INTEGER (KIND=2) :: RETURN_MONTHLY_CL_EXPENSES,R_YR,
     +  RETIRED_UNITS,
     +                    CO2_MARKET_PTS,ABATE_RETROFIT_COUNTER,
     +                    RETROFIT_PROJECT_YEAR,K
      REAL (kind=4) ::  MONTH_VARS(0:12,1:*),TRANS_DISPATCH_EMIS_ADDER
      REAL ::  R_MONTHLY_OWNED_NF(0:12),
     +     R_MONTHLY_LEASED_NF(0:12),
     +     R_MONTHLY_DOE_DISPOSAL(0:12),
     +     R_MONTHLY_DOE_DECOMMISSIONING(0:12)

      REAL (KIND=4) :: RETRO_CAP_CO2_MULT
      LOGICAL (kind=1) ::    RETURN_CL_INTRA_CLASS_REVENUES
      LOGICAL (kind=1) ::    INIT_MON_MDS_CL_UNITS,
     +            MON_MDS_CL_VAR,MON_MDS_CL_FIXED,MON_MDS_NUC_ADDER,
     +            STRATEGIC_RETIRMENTS_LOGIC=.FALSE. ,
     +            GET_STRATEGIC_RETIRMENTS_LOGIC,
     +            SCHEDULE_FO,
     +            YES_FREQUENCY_DURATION,
     +            FREQUENCY_DURATION,
     +            UNIT_FREQUENCY_DURATION=.FALSE. ,
     +            YES_DETAILED_FOR,
     +            YES_DETAILED_MAINTENANCE,
     +            DETAILED_MAINT,
     +            PRICE_ONLY_WHOLESALE_REV=.FALSE. ,
     +            APPLY_TRANS_REV_TO_WHOLESALE,
     +            DETAILED_TRANSFER_PRICING=.FALSE. ,
     +            YES_DETAILED_TRANSFER_PRICING,
     +            NOX_ACTIVE_FOR_UNIT,
     +            GET_NOX_CONTROL_FOR_UNIT,
     +            GET_SOX_CONTROL_FOR_UNIT,
     +            GET_CO2_CONTROL_FOR_UNIT,
     +            SET_SOX_CONTROL_COAL_LP,
     +            RESET_SOX_CONTROL_COAL_LP,
     +            R_CO2_FIRST_CONTROL_MONTH,
     +            R_CO2_FIRST_CONTROL_YEAR,
     +            R_CO2_YEAR_BEFORE_CONTROL,
     +            GET_HG_CONTROL_FOR_UNIT,
     +            GET_OTHER3_CONTROL_FOR_UNIT,
     +            R_SEASON_IS_NOX_SEASON,
     +            FIRST_ENERGY=.FALSE. ,
     +            R_CALCULATE_NOX,
     +            GET_MONTHLY_MAINTENANCE_PENALTY,
     +            YES_REGIONAL_OUTAGES_ACTIVE,
     +            REGIONAL_PARAMS_ACTIVE,
     +            YES_TURN_OFF_POLY,TURN_OFF_POLY,
     +            TEST_MONTHLY_MUST_RUN,
     +            YES_NEW_BUILD_ADDITIONS_ACTIVE,
     +            NEW_BUILD_ADDITIONS_ACTIVE,
     +            GET_NEW_BUILD_PERCENTS,
     +            KEEP_UNIT,
     +            EXPENSE_ASSIGNMENT_IS_PURCHASE,
     +            CapEx_Running,YES_POWERWORLD_REPORT,
     +            WVPA,
     +            AOD_THERMAL_UNIT_VALUE_LIST,
     +            YES_AOD_PRICE_THERM_VALUE_LIST,
     +            YES_AOD_COMP_THERM_VALUE_LIST,
     +            AOD_THERMAL_LIST_COMPARE,
     +            CO2_RETROFIT_LOGIC_ACTIVE
      REAL (kind=4) ::  R_INTRA_BASE_REVENUES,
     +       R_INTRA_ADJ_REVENUES,
     +       R_INTRA_SALES_REVENUES,
     +       R_INTRA_OTHER_REVENUES,
     +       R_INTRA_COMPANY_NF_BURN,
     +       R_WVPA_EMISSIONS_EXPENSE,
     +       R_CAPACITY_SALES_TO_LEVEL_RM,
     +       R_CAPACITY_PURCHASES_TO_LEVEL_RM,
     +       RETIREMENTS_PROFIT_PER_KW,
     +       R1_MONTHLY_TRANSFER_REV,
     +       R1_MONTHLY_TRANSFER_COST,
     +       R2_MONTHLY_TRANSFER_REV,
     +       R2_MONTHLY_TRANSFER_COST,
     +       UNDER_CONSTRUCTION_PERCENT,
     +       ADVANCED_DEVELOPMENT_PERCENT,
     +       EARLY_DEVELOPMENT_PERCENT,
     +       PROPOSED_PERCENT,
     +       INDEFIN_POSTPHONED_PERCENT
      INTEGER (kind=2) ::  MAX_INTRA_CLASS,
     +       CM_RETIREMENT_UNIT(0:1000)
      REAL ::  INTRA_FOSSIL_FUEL_EXPENSES,
     +     INTRA_LEASED_NUC_FUEL_EXPENSES,
     +     INTRA_OWNED_NUC_FUEL_EXPENSES,
     +     INTRA_NUC_OWN_BURN,
     +     INTRA_NUC_LEASE_BURN,
     +     INTRA_PURCHASE_EXPENSES
      LOGICAL (kind=1) ::  RETURN_CL_INTRA_EXPENSES,
     +          REGIONAL_MAINT_SCHEDULING,
     +          REGIONAL_PA_MAINT_SCHEDULING,
     +          REGIONAL_TG_MAINT_SCHEDULING,
     +          REGIONAL_PA_MAINTENANCE_LOGIC,
     +          REGIONAL_TG_MAINTENANCE_LOGIC
      REAL (kind=4) ::  R_INTRA_FOSSIL_FUEL_EXPENSES,
     +       R_INTRA_LEASED_NUCLEAR_FUEL,
     +       R_INTRA_OWNED_NUC_FUEL_EXPENSES,
     +       R_INTRA_NUC_OWN_BURN,
     +       R_INTRA_NUC_LEASE_BURN,
     +       R_INTRA_PURCHASE_EXPENSES,
     +       R_ENRG,R_FIXED_COST,R_CAP,R_EMIS(5),
     +       ANNUAL_MARGIN_PER_KW,MINIMUM_MARGIN_PER_KW=0. ,R_MW,
     +       R_CO2,R_MWH,R_CO2_COST,
     +       R_CAP_COST,
     +       CO2_K,

     +       RETIREMENTS_CUM_CO2(MAX_CL_UNITS),
     +       RETROFIT_CUM_CO2(MAX_CL_UNITS),

     +       CM_RETIREMENT_KW(0:1000), ! ASSUMES NO MORE THAN 1000 CM
     +       GET_CM_RETIREMENT_KW,

     +       RETIREMENTS_UPLIFT_PER_TON(MAX_CL_UNITS),
     +       RETROFIT_UPLIFT_PER_TON(MAX_CL_UNITS),

     +       ANNUAL_CAPITAL_COST,
     +       INIT_ANNUAL_GROSS_MARGIN,
     +       MONTHLY_EAVAIL(12),
     +       MONTHLY_NUCLEAR_AVAIL_MULT(12),
     +       GET_SCENARIO_NUCLEAR_AVAIL,
     +       GET_MONTHLY_REGIONAL_OUTAGE,
     +       REGIONAL_MONTHLY_MULT(12),
     +       MONTHLY_COAL_AVAIL_MULT(12),
     +       MONTHLY_GAS_AVAIL_MULT(12),
     +       MONTHLY_OIL_AVAIL_MULT(12),
     +       MONTHLY_OTHER_AVAIL_MULT(12),
     +       GET_SCENARIO_COAL_AVAIL,
     +       GET_SCENARIO_GAS_AVAIL,
     +       GET_SCENARIO_OIL_AVAIL,
     +       GET_SCENARIO_OTHER_AVAIL,
     +       MONTHLY_HYDRO_WATER_YEAR_MULT(12),
     +       GET_SCENARIO_HYDRO_WATER_YEAR,
     +       TT_PERCENT,
     +       NOX_VOM(MAX_CL_UNITS),
     +       GET_NOX_VOM,
     +       NOX_FOM(MAX_CL_UNITS),
     +       GET_NOX_FOM,
     +       SOX_VOM(MAX_CL_UNITS),
     +       GET_SOX_VOM,
     +       SOX_FOM(MAX_CL_UNITS),
     +       GET_SOX_FOM,
     +       GET_CO2_VOM,
     +       CO2_VOM(MAX_CL_UNITS),
     +       GET_CO2_FOM,
     +       CO2_FOM(MAX_CL_UNITS),
     +       GET_HG_VOM,
     +       HG_VOM(MAX_CL_UNITS),
     +       GET_HG_FOM,
     +       HG_FOM(MAX_CL_UNITS),
     +       GET_OTHER3_VOM,
     +       OTHER3_VOM(MAX_CL_UNITS),
     +       GET_OTHER3_FOM,
     +       OTHER3_FOM(MAX_CL_UNITS),
     +       GET_MARKET_FLOOR,
     +       MARKET_FLOOR(MAX_CL_UNITS),
     +       GET_MARKET_CEILING,
     +       MARKET_CEILING(MAX_CL_UNITS),
     +       LATITUDE(MAX_CL_UNITS),
     +       LONGITUDE(MAX_CL_UNITS),
     +       MW_INSIDE_FENCE,
     +       INTER_BLOCKS(3,MAX_CL_UNITS),
     +       PERCENT_INSIDE_FENCE,
     +       MONTHLY_MAINTENANCE_PEAKS(12),
     +       GET_TRANS_GROUP_PEAK,
     +       GET_PA_PEAK,
     +       MAINTENANCE_PENALTY(12),
     +       LOCAL_MW(2),
     +       CO2_PRICE,
     +       CUM_CO2_EMISSIONS,
     +       RETROFIT_CO2,
     +       TRANS_MRX_DISPATCH_EMIS_ADDER,
     +       GET_CO2_RETIREMENT_PRICE,
     +       GET_CO2_RETIRE_RETRO_PRICE,
     +       R_CO2_REDUCTION,
     +       CUM_MW,
     +       CUM_MWH,
     +       RETIREMENT_COST_PER_KWMO,
     +       LOCAL_RPS_CONTRIB_PERCENT,
     +       GET_TRANS_RPS_PERCENT
      INTEGER (kind=2) ::    R_ISEAS,R_UNITNO,
     +            LOCAL_MONTH,
     +            MAX_RESOURCE_ID_NO,
     +            FT, ! PRIMARY MOVER
     +            TT,  ! TRANSFER TYPE
     +            NG,R_NG, ! NEWGEN INDEX
     +            YEAR_OR_SCEN,
     +            PA,
     +            MAX_PLANNING_AREAS,
     +            MAX_MAINT_PLANNING_AREAS,
     +            GET_NUMBER_OF_PLANNING_GROUPS,
     +            GET_PA_FROM_TG,
     +            MAX_NEWGEN_INDEX
      PARAMETER(  MAX_RESOURCE_ID_NO=9999,
     +            MAX_NEWGEN_INDEX=100)
      REAL (kind=8) ::       R_HEAT
      INTEGER (kind=4) ::    VALUES_2_ZERO,
     +            INT4_EMIS_TYPE
      INTEGER (kind=2) ::    CL_UNITS_READ,CL_RECORDS,EMISS_TYPE,
     +            RDI_RECORDS,RDI_CL_RECORDS,RDI_REC,RDI_CL_REC,
     +            RDI_IN_UNIT,RDI_OUT_UNIT,
     +            NEWGEN_COUNTER(MAX_NEWGEN_INDEX), ! 082309.
     +            GET_NEWGEN_INDEX,
     +            START_UP_INDEX(MAX_CL_UNITS),
     +            MONTHLY_MUST_RUN_VECTOR(MAX_CL_UNITS),
     +            MONTHLY_DECOMMIT_VECTOR(MAX_CL_UNITS),
     +            State_TG_Index(MAX_CL_UNITS),
     +            State_Index(MAX_CL_UNITS),
     +            EMISSION_MARKET_LINK(MAX_CL_UNITS),
     +            GET_THERMAL_TRACKER_INDEX,
     +            GET_THERMAL_RES_TRACKER_INDEX,
     +            GET_THERMAL_FUEL_TRACKER_INDEX,
     +            GET_THERMAL_MEM_TRACKER_INDEX,
     +            WVPA_TRACKING_TYPE,
     +            WVPA_RESOURCE_TRACKING_TYPE,
     +            WVPA_FUEL_TRACKING_TYPE,
     +            WVPA_MEM_TRACKING_TYPE,
     +            GET_EMISSION_MARKET_LINK,
     +            GET_START_UP_INDEX,
     +            TOTAL_START_UP_UNITS=0 ,
     +            GET_TOTAL_START_UP_UNITS,
     +            GET_CL_ENERGY_BY_TYPE,
     +            LOCAL_RESOURCE_ID,
     +            RESOURCE_ID_TO_UNIT(0:MAX_RESOURCE_ID_NO),
     +            NUMBER_OF_RESOURCE_ID(0:MAX_RESOURCE_ID_NO),
     +            GET_RESOURCE_ID_TO_UNIT,
     +            GET_I8_ID_TO_UNIT,
     +            R_ID,
     +            R_NUMBER_OF_RESOURCE_ID,
     +            MAX_FO_PER_MONTH=1 ,
     +            GET_MAX_FO_PER_MONTH,
     +            NOX_SEASON_DATE(MAX_CL_UNITS),
     +            R_MONTH,
     +            POINTER_FOR_NEW_CL_UNIT(MAX_CL_UNITS),
     +            GET_POINTER_FOR_NEW_CL_UNIT,
     +            GET_PRIMARY_MOVER_INDEX,
     +            GET_NUM_ANNUAL_DERIVATIVES,
     +            RETIREMENTS_INDEX(MAX_CL_UNITS),
     +            RETIRE_RETRO_COUNT_AFTER_SORT,
     +            LAST_COUNTER,
     +            RETIRE_RETRO_USED(MAX_CL_UNITS),
     +            CO2_ABATEMENT_INDEX(MAX_CL_UNITS),
     +            CO2_RETROFIT_ABATEMENT_INDEX(MAX_CL_UNITS),
     +            CO2_COUNTER,
     +            ANN_ECON_RETIRE_COUNTER,
     +            NEXT_MRX_RETIREMENT,
     +            NEXT_MRX_RETROFIT,
     +            U,
     +            MONTH
      INTEGER (kind=2) ::  DELETE,ON_LINE_MONTH,ON_LINE_YEAR,
     +          M,IREC,
     +          I,R_NUNITS,
     +          R_LAST_NUNITS,
     +          R_ON_LINE_MONTH,
     +          R_DATE,
     +          BASE_CL_UNITS=0 ,
     +          BASE_PLUS_HARDWIRED_CL_UNITS=0 ,
     +          AVAILABLE_CL_UNITS=0 ,
     +          INCREMENT_AVAILABLE_CL_UNITS,
     +          RETURN_CL_UNITS_B4_ADDITIONS,
     +          RETURN_UNIT_ADDITIONS,
     +          RESET_CL_UNITS,
     +          INCREMENT_HARDWIRED_CL_UNITS,
     +          CL_PLANNING_ADDITIONS,
     +          RETURN_CL_UNITS_AFTER_HARD_ADDS,
     +          MARKET_AREA_LOOKUP,
     +          CO2_ABATEMENT_NO,
     +          CO2_ABATEMENT_RPT_HEADER,
     +          CO2_ABATEMENT_VAR_NUM
      INTEGER ::  IOS,IRAN32,IRAN32_OUT,CO2_ABATEMENT_REC
      REAL ::  GET_CL_MW_FROM_POINTR,R_POINTR,MONTH_MULT,VOID_REAL,
     +     RDI_MONTH_GENERATION(12),RDI_MONTH_CAPACITY(12),
     +     RANDOM_NUMBER
      LOGICAL (kind=4) ::  FILE_EXISTS
      INTEGER (kind=2) ::  NUMBER_OF_CL_UNITS
      CHARACTER (len=2) ::  LOAD_FILE_CHAR_EXT
      CHARACTER (len=3) ::  GET_HOURLY_PRICE_NAME
      CHARACTER (len=5) ::  MARKET_PRICE_NAME
      CHARACTER (len=256) ::  FILE_NAME
      CHARACTER (len=256) ::  PRB_FILE_DIRECTORY,OUTPUT_DIRECTORY
      CHARACTER (len=20) ::  RETURN_UNITNM,MONTH_NAME,LOCAL_NAME
      CHARACTER (len=4) ::  YEAR_CHR
      CHARACTER (len=5) ::  NUNITS_CHR
      INTEGER (kind=2) ::  RETURN_SHADOW_UNIT_NUMBER,AI_REMAINING_LIFE
      INTEGER (kind=2) ::  SET_AI_CL_REMAINING_LIFE,LOCAL_POINTER
      INTEGER (kind=2) ::  ADD_NEW_CL_UNIT,
     +          GET_THERMAL_STATE_INDEX
      LOGICAL (kind=1) ::  CL_RESET_PRICES,TRANS_ANNUAL_UNIT_RETIREMENT,
     +          ANNUAL_CO2_RETIREMENTS_PROCESS,
     +          GET_CO2_RETIREMENTS_LOGIC,
     +          ANNUAL_CO2_RETROFIT_PROCESS,
     +          ANNUAL_RETIRE_RETRO_PROCESS,
     +          CO2_ABATEMENT_REPORT_ACTIVE,
     +          YES_CO2_ABATEMENT_REPORT,
     +          CO2_ABATEMENT_REPORT_NOT_OPEN=.TRUE. ,
     +          RETROFIT_LOGIC_ACTIVE,
     +          ASCENDING,
     +          GET_NEXT_MRX_RETIREMENT,
     +          RETIRE_CO2_THERMAL_UNIT,
     +          RETROFIT_CO2_THERMAL_UNIT,
     +          GET_NEXT_MRX_RETIRE_RETRO,
     +          R_CO2_END_LIST,
     +          MRX_RETIRE_THIS_UNIT,
     +          GET_THERMAL_RPS_DATA

      LOGICAL (kind=1) ::  PUT_THERMAL_RPS_ENRG_CAP,
     +          PUT_RPS_ENRG_CAP
      INTEGER (kind=2) ::  TEMP_RPS_NO,
     +          TEMP_PM
      REAL (kind=4) ::     TEMP_ENRG,
     +          TEMP_CAP
      LOGICAL (kind=4) ::  NEW_CL_UNIT_OPENED,RDI_FILE_EXISTS,
     + temp_l4
!
      INTEGER (kind=2) ::  ! INSTALLED_POINTER/0/,

     +          MARKETSYM_UNIT,
     +          R_CO2_COUNTER,
     +          R_RETIRE_OR_RETRO,
     +          R_CO2_NUNIT,RR_CO2_NUNIT,
     +          R_CO2_TG,R_CO2_RETIRE_OR_RETRO,
     +          COUNTER,
     +          RETROFIT_ACTIVE_ID,
     +          R_PM,R_ST_TG,I2_RETURN_RESULT,R_ST_P

      LOGICAL (kind=1) ::  RETURN_CL_ASSET_CLASS_PROD,
     +  RETURN_CL_CLASS_TOTAL_PROD,
     +          RETURN_FE_PNL_EXPENSES
!

!
      INTEGER (kind=2) ::  PEAK_MONTH ! ,CL_CAPACITY_TEMP_RETIRE_UNIT
!
!     VARIABLES FOR CAPACITY PLANNING
!
      INTEGER (kind=2) ::  UNIT_NO,R_UNIT_NO
      LOGICAL (kind=1) ::  SUBTRACT_IT,REPORT_THIS_CL_UNIT
      INTEGER (kind=2) ::  PT_YR,YEAR_START,YEAR_END,UNIT_OFF_YEAR
      REAL ::  CAP_MW,GET_CLASS_PEAK_NET_DSM_FOR_CAP,
     + FIRST_YEAR_CL_CAPACITY,
     +     R_CAP_MW
      REAL ::  GET_CL_TG_CAP,
     +     GET_NEWGEN_CAP_BY_INDEX,
     +     GET_CL_AFTER_PEAK,
     +     GET_CL_TG_RETIRE,
     +     R_CO2_UNIT_MW,
     +     R_CO2_UNIT_MWH,
     +     R_CO2_UNIT_CO2,
     +     R_CO2_UNIT_CO2_AFTER,
     +     R_CO2_UNIT_PRICE,
     +     R_CO2_STRIKE_PRICE,
     +     R_CO2_UNIT_MW_AFTER,
     +     R_CO2_STRIKE_PRICE_AFTER,
     +     Min_Cap_Change,
     +     Max_Cap_Change,
     +     Ave_Heat_Rate_Mult,
     +     CO2_Cntl_Percent,
     +     GET_TOTAL_INCREMENTAL_COST,
     +     R_RPS_CONTRIB
      CHARACTER (len=2) ::  CL_LOAD_TYPE
      CHARACTER (len=1) ::  UNIT_ACTIVE,
     +            REPORT_THIS_UNIT(MAX_CL_UNITS),
     +            UTILITY_OWNED,IGNORE_NON_UTILITY,
     +            SAVE_IGNORE_NON_UTILITY='F' ,
     +            START_UP_LOGIC(MAX_CL_UNITS),R_START_UP_TYPE,
     +            CONTRIBUTES_TO_SPIN(MAX_CL_UNITS),
     +            APPLY_NOX_SEASON_DATE(MAX_CL_UNITS),
     +            MARKET_RESOURCE(MAX_CL_UNITS),
     +            FOR_SEED_OPTIONS,
     +            R_MARKET_RESOURCE_STR,
     +            USE_POLY_HEAT_RATES,
     +            WVPA_RATE_TRACKER(MAX_CL_UNITS),
     +            WVPA_RES_TRACKER(MAX_CL_UNITS),
     +            WVPA_FUEL_TRACKER(MAX_CL_UNITS),
     +            MONTE_OR_FREQ(MAX_CL_UNITS),
     +            WVPA_MEM_TRACKER(MAX_CL_UNITS),
     +            ALLOW_DECOMMIT(MAX_CL_UNITS),
     +            PRIMARY_FUEL_CATEGORY(MAX_CL_UNITS),
     +            SECONDARY_FUEL_CATEGORY(MAX_CL_UNITS),
     +            EMISSIONS_FUEL_CATEGORY(MAX_CL_UNITS),

     +            THERMAL_AGGREGATED_UNIT

      LOGICAL (kind=1) ::  GET_START_UP_LOGIC,GET_CL_BASECASE_MARKET_ID,
     +          GET_SPIN_STATUS,
     +          IS_A_MARKET_RESOURCE,
     +          IS_STRICT_MARKET_RESOURCE,
     +          PROCESS_MARKET_RESOURCES,
     +          R_AREA_PRICE,
     +          TEMP_L,
     +          ANNUAL_CALL_PUT_CAPACITY,
     +          YES_ALLOW_DECOMMIT_BY_UNIT,
     +          REGION_OR_STATE_POWER_MAP,
     +          YES_REGION_POWER_MAP,
     +          CLA_UNIT_UP_YESTERDAY(MAX_CL_UNITS),
     +          INIT_CLA_UNIT_UP_YESTERDAY,
     +          GET_CLA_UNIT_UP_YESTERDAY,
     +          PUT_CLA_UNIT_UP_YESTERDAY,
     +          R_LOGICAL
      LOGICAL (kind=1) ::  VOID_LOGICAL,GET_PURCHASE_POWER_ASSIGN,
     +           ZONAL_LEVEL_MARKET,
     +           YES_ZONAL_LEVEL_MARKET,
     +           RESURRECT_RETROFIT_UNIT,
     +           ADJUST_CO2_RETRO_PLAN_CAP,
     +           ADJUST_GRX_CO2_RETRO_PLAN_CAP
      LOGICAL (kind=1) ::  RETURN_ASSET_CLASS_LISTS
      LOGICAL (kind=1) ::  PICK_FOR_SEED_OPTIONS,
     +          OLD_ADJ_LOGIC=.TRUE. 
      CHARACTER (len=6) ::     BASECASE_PLANT_ID(MAX_CL_UNITS),
     +               R_EIA_PLANT_CODE,
     +               BASECASE_UNIT_ID,
     +               BASECASE_MARKET_AREA_ID(MAX_CL_UNITS),
     +               R_MARKET_ID,
     +               BASECASE_TRANS_AREA_ID,
     +               BASECASE_PLANT_NERC_SUB_ID,
     +               BASECASE_PLANT_OWNER_ID,
     +               PRIMARY_MOVER_STR(MAX_CL_UNITS)
      LOGICAL (kind=1) ::       CAP_MARKET_ACTIVE,CAP_MARKET_SALE,
     +               INIT_CAP_MARKET_REVENUE,
     +               CALC_CL_CAP_MARKETS,
     +               YES_GSP_IS_ST_TG
      INTEGER (kind=2) ::
     +               CAP_MARKET_MONTH_NO(MAX_CL_UNITS),
     +               CAP_MARKET_MONTH_NO_INDEX,
     +               LOCAL_COLLECTION,
     +               RETROFIT_PROJECT_ID(MAX_CL_UNITS),
     +               RETROFIT_UNIT_INDEX(MAX_CL_UNITS),
     +               RETROFIT_COUNTER,RETIREMENT_COUNTER,
     +               ANN_RETROFIT_COUNTER
      REAL (kind=4) ::          GET_CAP_MARKET_REVENUE,
     +               CAP_MARKET_RATE,
     +               PLAN_FACTOR,
     +               PRIMARY_FUEL_ID,
     +               CAPACITY_MARKET_COIN_ADJ_FACT(MAX_CL_UNITS),
     +               GET_CL_TG_CAP_MARKET_MW,
     +               GET_CL_TG_CAP_MARKET_REV
      CHARACTER (len=5) ::   MARKET_ID,
     +               CAPACITY_MARKET_TYPE(MAX_CL_UNITS),
     +               CAPACITY_MARKET_MONTH(MAX_CL_UNITS),
     +               CAPACITY_MARKET_EXP_COLLECT(MAX_CL_UNITS)
      CHARACTER (len=20) ::    CAPACITY_MARKET_COST_ASSIGN(MAX_CL_UNITS)
      CHARACTER (len=20) ::
     +               STATE_PROV_NAME
      CHARACTER (len=36) ::  THERMAL_GUID
      REAL (kind=4) ::
     +         MON_CAP_SALE_MW_FROM(MAX_CL_UNITS),
     +         MON_CAP_SALE_REV_FROM(MAX_CL_UNITS),
     +         MON_CAP_PUCH_MW_FROM(MAX_CL_UNITS),
     +         MON_CAP_PUCH_COST_FROM(MAX_CL_UNITS),
     +         RPS_CONTRIBUTION_PERCENT(MAX_CL_UNITS),
     +         CO2_PIPELINE_DISTANCE(MAX_CL_UNITS),
     +         CO2_RETRO_HEAT_MULT(MAX_CL_UNITS),
     +         GET_CO2_RETRO_HEAT_MULT,
     +         CO2_RETRO_CAP_MULT(MAX_CL_UNITS),
     +         GET_CO2_RETRO_CAP_MULT
      CHARACTER (len=20) ::  COUNTY,NEWGEN_UNIT_STATUS
      CHARACTER (len=25) ::  CL_TRANS_NAME
      REAL ::  UNIT_CAPACITY,
     +     CL_PLANNING_CAPACITY,FIRST_YEAR_CAPACITY,
     +     R_CL_CAP_PLANNING_REMOVALS,
     +     R_CL_CAPACITY_PLANNING_ADDITIONS,
     +     OLD_CL_PLANNING_CAPACITY,NEW_CL_PLANNING_CAPACITY
      REAL ::  CL_CAPACITY_PLANNING_ADJ
      REAL ::  CL_PLANNING_LOAD_REDUCTION
      INTEGER (kind=2) ::  R_YEAR,ST
      INTEGER ::  R_CAP_TYPE
      CHARACTER (len=6) ::  STATE_PROVINCE,
     +            TEMP_STATE,
     +            STATE_PROVINCE_NAMES(0:MAX_STATE_LOOKUP_IDS),
     +            R_STATE_PROVINCE
      INTEGER (kind=2) ::  POINTER,UNIT_ON_LINE_YEAR,
     +          OPERATION_LIFE,
     +          THIS_YEAR,
     +          STATE_PROVINCE_INDEX(-1:MAX_STATE_LOOKUP_IDS),
     +          STATE_PROVINCE_ADDRESS(MAX_STATE_LOOKUP_IDS),
     +          UNIT_STATE_PROVINCE_INDEX(MAX_CL_UNITS),
     +          UNIT_GAS_REGION_INDEX(MAX_CL_UNITS),
     +          GET_UNIT_GAS_REGION_INDEX,
     +          GET_UNIT_STATE_PROVINCE_INDEX,
     +          MAX_STATE_PROVINCE_NO,
     +          GET_MAX_STATE_PROVINCE_NO,
     +          STATE_ID_LOOKUP,
     +          STATE_2_GAS_REGION_LOOKUP,
     +          GET_STATE_PROVINCE_NAMES
!
! ADDL EXPANSION
      INTEGER (kind=2) ::  R_TRANSACTION_GROUP,
     +          GET_BELONGS_TO_GROUP,
     +          DAY_TYPE_ID(MAX_CL_UNITS),DAY_TYPE_FOR_RESOURCE,
     +          GET_MAX_DAY_TYPES,GET_LAST_DAY_TYPE,
     +          GET_MAX_TRANS_GROUPS,MAX_TRANS_DATA_BASES=0 ,
     +          GET_MAX_DATA_BASES,
     +          GET_NUNITS,
     +          MAX_DAY_TYPES=0 ,MAX_TRANS_GROUPS,DAY_ID,TRANS_ID,
     +          DAY_TYPE_COUNTER,R_DAY_TYPE,GET_DATA_BASE_FOR_UNIT,
     +          GET_DATA_BASE_FOR_TRANS,R_TRANS_GROUP,
     +          GET_TRANS_FOR_DATA_BASE,
     +          R_DATA_BASE,SET_TRANS_FOR_DATA_BASE,
     +          MAX_TRANS_ID_USED=1 ,GET_MAX_TRANS_ID_USED,
     +          UPPER_TRANS_GROUP=0 ,GET_NUMBER_OF_ACTIVE_GROUPS,
     +          GET_PURCHASE_ASSET_CLASS_ID,
     +          TG_ASSET_CLASS,TG,GET_TRANS_GROUP_POSITION,
     +          GET_CM_FROM_TG,CM,R_CM,CG,PG,
     +          RPS_COUNT,PX,
     +          GET_RPS_PROGRAM_POSITION,
     +          GET_TRANS_RPS_PROG_NUMBER,
     +          NUMBER_OF_RPS_PROGRAMS,
     +          NUM_TRANS,
     +          GET_NUM_SAVE_ACTIVE_TRANSACT,
     +          GET_NUMBER_OF_CAPACITY_MARKETS,
     +          MONTHLY_FUEL_INDEX(MAX_CL_UNITS),
     +          UNIT_BEG_FO_HR(12),UNIT_END_FO_HR(12),
     +          HG_CONTROL_DATE(MAX_CL_UNITS),
     +          OTHER3_CONTROL_DATE(MAX_CL_UNITS),
     +          NOX_CONTROL_DATE(MAX_CL_UNITS),
     +          SOX_CONTROL_DATE(MAX_CL_UNITS),
     +          SOX_CONTROL_DATE_TEMP(MAX_CL_UNITS)
      REAL ::  FUEL_MIX_PTR(MAX_CL_UNITS),
     +     PBTUCT_SAVE(MAX_CL_UNITS),
     +     SBTUCT_SAVE(MAX_CL_UNITS),
     +     PRIM_HEAT_CONTENT(MAX_CL_UNITS),
     +     FUELADJ_SAVE(MAX_CL_UNITS),
     +     VCPMWH_IN(MAX_CL_UNITS),
     +     GET_VCPMWH_IN,
     +     FIXED_COST_IN(MAX_CL_UNITS),
     +     GET_FIXED_COST_IN,
     +     VCPMWH_IN_SAVE(MAX_CL_UNITS),
     +     ESCALATE_THERMAL_VOM,
     +     ESCALATE_THERMAL_FOM,
     +     ESCALATED_MONTHLY_VALUE,
     +     START_UP_COST_ESCALAT_MTHLY_VAL,
     +     RET_VAL_FROM_ESCALATION_VECTOR,
     +     FIXED_COST_SAVE(MAX_CL_UNITS),
     +     ANNUAL_CL_FIXED_COST_SAVE(MAX_CL_UNITS),
     +     DISPADJ_SAVE(MAX_CL_UNITS),
     +     DISPADJ2_SAVE(MAX_CL_UNITS),
     +     CL_AI_CAPACITY_RATE_SAVE(MAX_CL_UNITS),
     +     CL_AI_ENERGY_RATE_SAVE(MAX_CL_UNITS),

     +     EMISS_FUEL_COST_SAVE(MAX_CL_UNITS),
     +     DISPATCH_MULT_SAVE(MAX_CL_UNITS),
     +     EXCESS_ENERGY_SALES_SAVE(MAX_CL_UNITS),
     +     CL_AI_REMAINING_LIFE_SAVE(MAX_CL_UNITS),
     +     MINIMUM_CAPACITY_FACTOR(MAX_CL_UNITS),
     +     MAXIMUM_CAPACITY_FACTOR(MAX_CL_UNITS),
     +     START_UP_COSTS(MAX_CL_UNITS),
     +     START_UP_COSTS_ESCALATION(MAX_CL_UNITS),  ! 212
     +     GET_MONTHLY_FUEL_INDEX,
     +     GET_UNIT_START_UP_COSTS,
     +     RAMP_RATE(MAX_CL_UNITS),
     +     RAMP_DOWN_RATE(MAX_CL_UNITS),
     +     MIN_DOWN_TIME(MAX_CL_UNITS),
     +     MIN_UP_TIME(MAX_CL_UNITS),
     +     FOR_FREQUENCY(MAX_CL_UNITS),
     +     FOR_DURATION(MAX_CL_UNITS),
     +     S_FUEL_DELIVERY(MAX_CL_UNITS),GET_S_FUEL_DELIVERY,
     +     S_FUEL_DELIVERY_2(MAX_CL_UNITS),GET_S_FUEL_DELIVERY_2,
     +     S_FUEL_DELIVERY_3(MAX_CL_UNITS),GET_S_FUEL_DELIVERY_3,
     +     CUBIC_HEAT_CURVE(0:3,MAX_CL_UNITS),
     +     R_A_CO,R_B_CO,R_C_CO,R_D_CO,
     +     CONSTANT_POLY,
     +     FIRST_POLY,
     +     SECOND_POLY,
     +     THIRD_POLY,
     +     MIN_SPIN_CAP(MAX_CL_UNITS),
     +     MAX_SPIN_CAP(MAX_CL_UNITS),
     +     GET_MIN_SPIN_CAP,
     +     GET_MAX_SPIN_CAP,
     +     TRANS_ANNUAL_UNSERVED_COST,
     +     WHOLESALE_MARKET_COST,
     +     GET_CL_POOL_FRAC_OWN,
     +     ANNUAL_GROSS_MARGIN(MAX_CL_UNITS,0:3),
     +     ECON_CO2_RETIRE_PRICE(MAX_CL_UNITS),
     +     ECON_CO2_RETRO_PRICE(MAX_CL_UNITS),
     +     R_GROSS_MARGIN,
     +     SAVE_ANNUAL_GROSS_MARGIN,
     +     GET_ANNUAL_GROSS_MARGIN,
     +     GET_START_UP_COSTS,
     +     GET_RAMP_RATE,
     +     GET_RAMP_DOWN_RATE,
     +     GET_MIN_UP_TIME,
     +     GET_MIN_DOWN_TIME,
     +     GET_FOR_DURATION,
     +     GET_FOR_FREQUENCY,
     +     TEMP_R4,
     +     WINTER_TOTAL_CAPACITY,
     +     R_FUEL_DELIVERY_1,
     +     R_FUEL_DELIVERY_2,
     +     R_FUEL_DELIVERY_3,
     +     GET_FUEL_DELIVERY,
     +     GET_SECOND_FUEL_DELIVERY,
     +     NOX_CONTROL_PERCENT(MAX_CL_UNITS),
     +     SOX_CONTROL_PERCENT(MAX_CL_UNITS),
     +     SOX_CONTROL_PERCENT_TEMP(MAX_CL_UNITS),
     +     EMERGENCY_CAPACITY(MAX_CL_UNITS),
     +     HG_CONTROL_PERCENT(MAX_CL_UNITS),
     +     OTHER3_CONTROL_PERCENT(MAX_CL_UNITS),
     +     GET_EMERGENCY_CAPACITY,
     +     EMERGENCY_HEATRATE(MAX_CL_UNITS),
     +     R_EMERGENCY_HEATRATE,
     +     R_NOX_CONTROL_PERCENT,
     +     R_SOX_CONTROL_PERCENT,
     +     R_CO2_CONTROL_PERCENT,
     +     R_HG_CONTROL_PERCENT,
     +     R_OTHER3_CONTROL_PERCENT,
     +     GET_PBTUCT_SAVE,
     +     GET_DISPADJ_SAVE,
     +     GET_DISPADJ2_SAVE,
     +     GET_MAINTENANCE_RATE_FOR_MONTH,
     +     PERCENT_RETAIL
      INTEGER (kind=2) ::  TEMP_I2,
     +     GET_MARKET_RESOURCE_INDEX,
     +     GET_MARKET_RESOURCE_COUNTER,
     +     MARKET_COUNT=0 ,
     +     TWO_BLOCK(2)
!
      LOGICAL (kind=1) ::  OFFSET_MAINTENANCE_VECTORS
      LOGICAL (kind=1) ::  OFFSET_MAINTENANCE_ACTIVE,
     +         CALC_ANNUAL_CAP_AND_MAINT,
     +         SAVE_DETAILED_MAINTENANCE,DETAILED_MAINTENANCE_IS_ACTIVE,
     +         DETAIL_MAINTENANCE_NOT_ACTIVE,UPDATE_PERIOD_CAPACITY,
     +         UPDATE_PERIOD_MAINTENANCE,TRANS_GROUP_ACTIVE_SWITCH,
     +         RUN_TRANSACT=.FALSE. ,YES_RUN_TRANSACT,
     +         IN_ACTIVE_THERMAL_MARKET_AREA,
     +         UNIT_ON_LINE_IN_YEAR,
     +         GET_CUBIC_HEAT_CURVE,
     +         GET_PW_UNIT_TEXT_FIELDS
      INTEGER (kind=2) ::  J,BASE_DATE,GET_YR,R_I,R_J,LAST_ACTIVE_UNIT,
     +  DATE_CHECK,
     +          BASE_YEAR_DATE,LOCAL_YEAR
      REAL ::   GET_CLASS_PEAK_NET_DSM,PERIOD_RATE,FOR_MT,
     +      MW_CAPACITY,RTEMP,GET_VAR,
     +      MAINTENANCE_RATE(*),
     +      R_MAINTENANCE_RATE(*),
     +      GET_ANNUAL_CL_CAPACITY,
     +      MONTHLY_DISPATCH_COST(2,12),
     +      MONTHLY_CAPACITY(2,12),
     +      LOCAL_COEFF(3)
      INTEGER (kind=2) ::  GGI,MONTHLY_INTEGER(12)
      INTEGER (kind=2) ::  ALLOCATION_VECTOR
      REAL (kind=4) ::  ALLOCATION_VALUE(AVAIL_DATA_YEARS)
      REAL (kind=4) ::  TOTAL_MONTHLY_MAINTENANCE(0:12),
     +       GROUP_MW_MAINTENANCE(0:12,0:MAX_REPORTING_GROUPS)
      REAL (kind=4) ::  UNSCHEDULED_MAINT_HOURS
      LOGICAL (kind=1) ::  FALSE_BYTE=.FALSE. ,
     +          STARTS_ONLY=.FALSE. 
!
! ASSET CLASS DATA
!
      LOGICAL (kind=1) ::  R_CLASS_EXISTS
      INTEGER (kind=2) ::  RETURN_NUM_CAP_LIMITED_CLASSES,
     +          R_MAX_CAP_LIMITED_CLASS_NUM,
     +          RETURN_CAP_LIMITED_POINTER,
     +          R_CLASS,CLASS,R_FT,R_ST,R_TG
      INTEGER (kind=2) ::  GET_CL_ASSET_CLASS_INFO,
     +          ASSET_CLASS_NUM(MAX_CL_UNITS),
     +          ASSET_CLASS_VECTOR(MAX_CL_UNITS),
     +          INTRA_COMPANY_CLASS_ID(MAX_CL_UNITS)
      INTEGER (kind=2) ::  R_ASSET_CLASS_NUM(*),
     +          R_ASSET_VECTOR(*),
     +          R_ASSET_CLASS,
     +          R_ALLOCATION_VECTOR,
     +          R_CL_RESOURCE_ID,
     +          GET_ASSET_CLASS_NUM,
     +          NUM_FUEL_CATEGORIES=11   ! 12/13/01.
! NUM_FUEL_CATEGORIES   1 = COAL
!                       2 = GAS
!                       3 = OIL
!                       4 = NUCLEAR
!                       5 = WATER
!                       6 = OTHER
!                       7-9 = RESERVED
!                       10 =  WHOLESALE SALES
!                       11 =  WHOLESALE PURCHASES
      CHARACTER (len=20) ::  SPECIAL_ID_NAME
      REAL ::  R_NUC_FUEL_LEASED,
     +     R_NUC_FUEL_OWNED,
     +     R_NUC_FUEL_OWNED_BURN,
     +     R_NUC_FUEL_LEASED_BURN,
     +     R_NF_BURN_IN_RATEBASE,
     +     R_PURCHASE_POWER_EXPENSE,
     +     R_FUEL_COST,
     +     R_VOM_COST,
     +     R_VARIABLE_EXPENSE,
     +     R_FIXED_EXPENSE,
     +     R_EXPENSE_COLLECTED_ADJ_CLAUSE,
     +     R_EXPENSE_COLLECTED_BASE_RATES,
     +     R_NOT_COLLECTED_IN_RATES,
     +     R_TOTAL_SALES_REVENUE,
     +     R_SALES_REVENUE_NOT_IN_RATES,
     +     R_BTL_SALES_REVENUE,
     +     R_BTL_EXPENSES
      INTEGER (kind=2) ::  NUMBER_OF_CAP_LIMITED_CLASSES,
     +            MAX_CAP_LIMITED_CLASS_ID_NUM,
     +          RETURN_MAX_CL_CLASS_NUM
!
      LOGICAL (kind=1) ::    MAINTENANCE_REPORT_NOT_OPEN=.TRUE. ,
     +            MAINTENANCE_REPORT,
     +            YES_UNIT_COMMITMENT_LOGIC,
     +            ECITIES=.FALSE. ,YES_ECITIES_UNITS_ACTIVE,
     +            ECITY_COMPANY,
     +            POWER_DERIV_REV_EXP_BY_CLASS
      REAL ::  RETURN_ECITIES_OBJ_VARS
      INTEGER (kind=2) ::    CL_MAINT_NO,MAINTENANCE_HEADER,BEGIN_DATE
      INTEGER ::  CL_MAINT_REC
      REAL ::         DAYS_PER_MONTH(13)/31.,28.,31.,30.,31.,30.,31.,
     +                              31.,30.,31.,30.,31.,365./
!
      INTEGER (kind=2) ::  RETURN_CL_FUEL_POINTERS,
     +  UPDATE_PRIMARY_FUEL_MIX_RATIO,
     +          FIRST_YEAR_PRICE_MODE=19999 ,
     +          GET_FIRST_YEAR_PRICE_MODE

      INTEGER (kind=2) ::  R_P_BTU_COST_POINTR(*),
     +          R_S_BTU_COST_POINTR(*),
     +          R_E_BTU_COST_POINTR(*)
!
      INTEGER (kind=2) ::    UPDATE_PR_ESCALATIONS,ESCALATE_ONE_VALUE,
     +  VOID_INT2,
     +            R_POINTER,ASSET_ALLOCATION_VECTOR,
     +            ESCALATE_ONE_VALUE_FOR_ADDITIONS
      INTEGER (kind=2) ::  CURRENT_YEAR,CURRENT_YEAR_COMPARISON,UNUM,
     +  ASSET_CLASS,
     +          CLASS_POINTER,COLLECTION,EXPENSE_GROUP
      CHARACTER (len=1) ::  DUMMY_TYPE
      INTEGER (kind=2) ::  CL_EXPENSES_2_ASSET_CLASSES,
     +          MON_MDS_CL_EXP_2_AC,
     +          RETURN_CL_ASSET_CLASS_EXPENSES,
     +          RETURN_NUC_CL_ASSET_CLASS_EXPENSES,
     +          RETURN_MONTHLY_NF_OP_EXPENSES,
     +          SET_UP_CL_CLASS_ARRAYS,
     +          ZERO_CL_CLASS_ARRAYS,
     +          RETURN_AMEREN_CL_CLASS_EXPENSES
      REAL (kind=4) ::    EL_BASE_REVENUE_RATE,EL_PEAK_REVENUE_RATE,
     +         R_NUC_UNIT_FUEL_ADDER_COST
      REAL (kind=4) ::  EL_BASE_RATE,EL_PEAK_RATE,
     +       BASE_MARKET_ENRG_SALES,
     +       BASE_MARKET_REVENUES,
     +       PEAK_MARKET_ENRG_SALES,
     +       PEAK_MARKET_REVENUES

      REAL ::  GET_CL_EMISSIONS,GET_CL_EMISS_FOR_CLASS
      REAL ::  ASSET_ALLOCATOR
      INTEGER (kind=2) ::  R_EMISS_TYPE
      REAL ::  R_CLASS_CL_EMISSIONS(NUMBER_OF_EMISSION_TYPES),
     +     R_MONTH_AC_WHOLESALE_PROD_COST(0:12),
     +     R_MONTH_AC_ECITY_VAR_PROD_COST(0:12),
     +     R_MONTH_AC_ECITY_NEW_FIX_COST(0:12),
     +     R_WTB(0:12),
     +     R_MONTH_AC_ECITY_SUPP(0:12),
     +     R_MONTH_AC_ECITY_REVENUE(0:12),
     +     R_MONTH_AC_ECITY_FEES(0:12),
     +     R_ECITIES_WHOLESALE_PROD_COST,
     +     R_ECITIES_VAR_PROD_COST,
     +     R_ECITIES_NEW_FIXED_COST,
     +     R_ECITIES_MARKET_ENERGY_SALES,
     +     R_ECITIES_TRANSMISSION_FEES,
     +     TOTAL_DERIV_REV,
     +     TOTAL_DERIV_EXP,
     +     WHOLESALE_PRODUCTION_COST,
     +     TOTAL_VARIABLE_COST,
     +     AVERAGE_PRODUCTION_COSTS

      LOGICAL (kind=1) ::  SHADOW_UNITS_ACTIVE
      INTEGER (kind=2) ::  MO,MY_SHADOW,NUM_OF_NUCLEAR_UNITS
!
      LOGICAL (kind=1) ::  ACCUMULATE_MAINTENANCE
      LOGICAL (kind=1) ::  MAINTENANCE_IS_ACCUMULATED
      REAL (kind=4) ::  TEMP_CL_MAINTENANCE,MAINTENANCE_CAPACITY
      INTEGER (kind=2) ::  MW_MAINT_NO
      INTEGER ::  MW_MAINT_REC
      CHARACTER (len=9) ::
     +      GROUP_NAME(0:MAX_REPORTING_GROUPS)/'Group 0',
     +           'Group 1','Group 2','Group 3','Group 4','Group 5',
     +           'Group 6','Group 7','Group 8','Group 9','Group 10',
     +           'Group 11','Group 12','Group 13','Group 14',
     +  'Group 15'/
!
! DOE AND DECOMMISSIONING VARIABLES
!
      LOGICAL (kind=1) ::  GET_BLOCK_CAP_FACTORS
      INTEGER (kind=2) ::  R_NBLOK2
      REAL (kind=4) ::  MMBTU_ENERGY,MWH_ENERGY,
     +       R_DOE_NUC_FUEL_FEE,DOE_NF_DISPOSAL_COST,
     +       R_NUC_DECOMMISSIONING_COST,NUCLEAR_DECOMMISSIONING_COST,
     +       R_DOE_R300_DISPOSAL_COST,DOE_R300_DISPOSAL_COST,
     +       NUC_MAINTENANCE(12),
     +       R_LOWER_BOUND_CAP_FACTOR(*),R_UPPER_BOUND_CAP_FACTOR(*),
     +       R_FACET_MAINTENANCE_RATE(*),
     +       R_CL_ANN_CLASS_CAPACITY(4),
     +       R_CL_ANN_CLASS_ENERGY(4),
     +       R_CL_ANN_TOTAL_CLASS_CAPACITY,
     +       R_CL_ANN_TOTAL_CLASS_ENERGY,
     +       R_NUC_ENERGY,
     +       R_STEAM_ENERGY,
     +       R_PURCHASES_ENERGY,
     +       R_ASSET_CLASS_900,
     +       R_WHOLESALE_FUEL_EXPENSE,
     +       R_WHOLESALE_VOM_EXPENSE,
     +       R_ICAP_REVENUES
      REAL (kind=8) ::  R_MMBTU_FUEL_BALANCE(*)
      INTEGER (kind=2) ::  FUEL_ID,UNITNO,R_FUEL_INVENTORY_ID(0:*)
      CHARACTER (len=6) ::  PRIM_FUEL_TYPE_STR,TEMP_PRIM_FUEL_TYPE_STR
      LOGICAL (kind=1) ::  NUC_FUEL_PRICES_FROM_FUEL_FILE(MAX_CL_UNITS),
     +          NUC_FUEL_PRICE_SOURCE_IS_NFILE,
     +          TF_FILE_EXISTS,TG_FILE_EXISTS
      CHARACTER (len=20) ::       RDI_COMPANY_NAME,
     +                  RDI_NERC_REGION,
     +                  RDI_SUB_REGION,
     +                  RDI_PM_ABBREV,
     +                  RDI_PRIME_FUEL
      CHARACTER (len=30) ::       RDI_DESC,CO2_BASIN_NAME
      REAL (kind=4) ::       RDI_DEMONSTRATED_CAPACITY_MW,
     +            RDI_INCREMENTAL_FUEL_USDMWH,
     +            RDI_AVERAGE_HEAT_RATE

!
! GADS DATA BASE FOR FOR'S AND MOR'S
!
      INTEGER (kind=2) ::  RDI_SIZE_INDEX
      INTEGER (kind=2) ::  RDI_FUEL_INDEX,GET_PRIMARY_MOVER
      REAL (kind=4) ::  GADS_FOR(8,5), GADS_MOR(8,5)
!
! UNIT SIZES (MW) 1-99 100-199 200-299 300-399 400-599 600-799 800-1000 1000+
!
      DATA GADS_FOR/ 0.1045,0.1086,0.1240,0.1472,0.1444,
     +               0.1254,0.1253,0.1555, ! COAL
     +               0.0758,0.1260,0.1061,0.1465,0.1423,
     +               0.1663,0.1154,0.1154, ! OIL
     +               0.1450,0.1450,0.1450,0.1450,0.1450,
     +               0.1450,0.1585,0.2106,! NUCLEAR (PWR)
     +               0.0641,0.1052,0.1241,0.1706,0.1322,
     +               0.1697,0.0804,0.0804,! GAS
     +               0.1241,0.1241,0.1241,0.1241,0.1241,
     +               0.1241,0.1241,0.1241/! LIGNITE
     +     GADS_MOR/ 1.1,1.3,1.3,1.3,1.3,0.7,0.6,1.4, ! COAL
     +               0.8,1.0,1.0,2.3,1.3,1.2,0.7,0.7, ! OIL
     +               6.7,6.7,6.7,6.7,6.7,6.7,6.7,6.7, ! NUCLEAR (PWR)
     +               1.3,1.6,1.5,2.0,1.8,1.6,1.5,1.5, ! GAS
     +               0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.9/ ! LIGNITE

      LOGICAL (kind=1) ::  BASE_EL_REVENUE_CASE,
     +             PEAK_EL_REVENUE_CASE
      REAL (kind=4) ::    GET_POWERDAT_PLANT_ID,
     +         R_HESI_SECOND_UNIT_ID_NUM

      INTEGER (kind=8)   ::
     +            R_I8_ID,
     +            TRANS_BUS_ID(MAX_CL_UNITS),
     +            THERMAL_PARENT_ID
      INTEGER ::      ! H2_UNIT_ID_NUM,
     +            POWERDAT_PLANT_ID(MAX_CL_UNITS),
     +            I4,HIGHEST_ID
!
      LOGICAL (kind=1) ::  CLA_SPECIAL_ID_NAME,CLA_RETURN_UNITNM
      CHARACTER (len=20) ::  R_NAME
      INTEGER (kind=2) ::  ON_LINE_MONTH_TEMP
!
! 06/13/03. FOR THE ECITY REPORT
!
      LOGICAL (kind=1) ::  ECITY_OBJ_REPORT,
     +          ECITY_OBJ_REPORT_NOT_OPEN=.TRUE. 
      INTEGER (kind=2) ::  ECITY_VARIABLE_NUMBER,
     +          ECITY_OBJ_UNIT,
     +          ECITY_OBJ_HEADER
      INTEGER ::    ECITY_OBJ_REC
      CHARACTER (len=9) ::  CL_MONTH_NAME(14)
     +                         /'January  ','February ',
     +                          'March    ','April    ',
     +                          'May      ','June     ',
     +                          'July     ','August   ',
     +                          'September','October  ',
     +                          'November ','December ',
     +                          'Annual   ','Fiscal Yr'/
!
!
! MULTI-FILE VARIABLES
!
      LOGICAL (kind=1) ::  PROCESSING_FIRST_DATA_FILE
      INTEGER (kind=2) ::  MAX_CL_FILES=36 
      INTEGER ::    FILE_ID
      INTEGER (kind=4) ::  UNIQUE_ID_NUM
      INTEGER (kind=4) ::  CL_UNIT_UNIQUE_RPT_ID(MAX_CL_UNITS)
      INTEGER (kind=4) ::  UNIQUE_REPORT_VALUE_FOR_CL_UNIT
      LOGICAL (kind=1) ::  TRANSACT_ACTIVE_IN_ENDPOINT
      INTEGER (kind=2) ::  RETURN_MONTHLY_CL_CASH_EXPENSES
      INTEGER (kind=2) ::  RETURN_MONTHLY_CL_REVENUES
      INTEGER (kind=2) ::  RETURN_MONTHLY_CL_CASH_REVENUES
!
! FE P&L VARIABLES
!
      REAL (kind=4) ::  R_PNL_FUEL_COST(*),
     +       R_PNL_PURCHASE_POWER_EXPENSE(*),
     +       R_PNL_VARIABLE_EXPENSE(*),
     +       R_PNL_FIXED_EXPENSE(*),
     +       R_PNL_TOTAL_SALES_REVENUE(*),
     +       R_PNL_CL_ANN_TOTAL_CLASS_ENERGY(*),
     +       R_PNL_CL_ANN_CLASS_CAPACITY(*),
     +       R_BAY_SHORE_FUEL_EXPENSE
      LOGICAL (KIND=4) :: EFORS0=.FALSE., SORS0=.FALSE.
! SPCapEx Additions Nov. 2005
         LOGICAL (KIND=1) :: SP_CAPEX_ACTIVE
         CHARACTER (LEN=1) :: PHASE_I_STR
         INTEGER (KIND=2) :: ID,UNIT_NAME_COUNT(MAX_CL_UNITS)
         CHARACTER (LEN=46) :: UNIT_NAME
         CHARACTER (LEN=48) :: TEMP_UNIT_NAME
         REAL (KIND=4) :: CL_CAPACITY_PLANNING_REMOVALS
         INTEGER (KIND=2) :: GET_MARKET_PRICE_YEAR
! END SPCapEx Additions
!
! ADDITONS TO CREATE CL FILE OF RESOURCES ADDTIONS 12/06/06 DR.G
!
      CHARACTER (LEN=4096) :: RECLN
      CHARACTER (LEN=256) :: BASE_FILE_DIRECTORY
      INTEGER (KIND=2) :: YEAR_CHECK,YR
      INTEGER (KIND=2) :: END_POINT_CHECK=-999,
     +                    SEQU_NO
      LOGICAL (KIND=1) :: MRX_TEXT_FILE_NOT_OPEN=.TRUE.,
     +                    VECTOR_IS_VALUE
      LOGICAL (KIND=4) :: MRX_TEXT_FILE_OPEN=.FALSE.
      CHARACTER (LEN=2) :: CAPACITY_PLANNING_METHOD,GREEN_MRX_METHOD,
     +                     UNIT_TYPE,UNIT_TYPE_CATEGORY
      CHARACTER (LEN=18) :: TEMP_EXPENSE_ASSIGNMENT,
     +                      TEMP_EXPENSE_COLLECTION
      CHARACTER (LEN=10) :: TEMP_SEC_FUEL_TYPE,
     +                      TEMP_EMISS_FUEL_TYPE
      CHARACTER (LEN=3) :: TEMP_INTRA_COMPANY_TRANSACTION
      CHARACTER (LEN=15) :: TEMP_UTILITY_OWNED
      CHARACTER (LEN=6) :: TEMP_MARKET_RESOURCE

      CHARACTER (LEN=9) :: TEMP_START_UP_LOGIC,
     +                     TEMP_EMISSION_DATA_UNITS
      CHARACTER (LEN=5) :: TEMP_UNIT_ACTIVE,
     +                     TEMP_REPORT_THIS_UNIT,
     +                     TEMP_CONTRIBUTES_TO_SPIN,
     +                     TEMP_APPLY_NOX_SEASON_DATE,
     +                     TEMP_ALLOW_DECOMMIT,
     +                     TEMP_USE_POLY_HEAT_RATES,
     +                     TEMP_AGGREGATE_THIS_UNIT,
     +                     TEMP_LINKED_BETTERMENT_OPTION
      CHARACTER (LEN=20) :: TEMP_WVPA_RATE_TRACKER,
     +                      TEMP_WVPA_RES_TRACKER,
     +                      TEMP_WVPA_FUEL_TRACKER,
     +                      TEMP_MONTE_OR_FREQ,
     +                      TEMP_WVPA_MEM_TRACKER,
     +                      TEMP_PRIMARY_FUEL_CATEGORY,
     +                      TEMP_SECONDARY_FUEL_CATEGORY,
     +                      TEMP_EMISSIONS_FUEL_CATEGORY,
     +                      TEMP_RETIREMENT_CANDIDATE,
     +                      TEMP_RETROFIT_CANDIDATE
      REAL (KIND=4) :: ESCAL_RATE
      CHARACTER (len=30) ::  SCREEN_OUTPUT
      character (len=1024) :: name_of_file

!
! END RESOURCE VARIABLE.
!
!
! END OF DATA DECLARATIONS
!
      UPPER_TRANS_GROUP = GET_NUMBER_OF_ACTIVE_GROUPS()
      CapEx_Running = YES_POWERWORLD_REPORT()
      UNIT_NAME_COUNT = 0
      INSTALLED_POINTER = 0
!
      BEGIN_DATE = 100.*(BASE_YEAR+1-1900)

      MAX_PLANNING_AREAS = GET_NUMBER_OF_PLANNING_GROUPS()

      REGIONAL_PA_MAINT_SCHEDULING = REGIONAL_PA_MAINTENANCE_LOGIC()
      REGIONAL_TG_MAINT_SCHEDULING = REGIONAL_TG_MAINTENANCE_LOGIC()
      IF(REGIONAL_PA_MAINT_SCHEDULING) THEN
         MAX_MAINT_PLANNING_AREAS = MAX_PLANNING_AREAS
      ELSEIF(REGIONAL_TG_MAINT_SCHEDULING) THEN
         MAX_MAINT_PLANNING_AREAS = UPPER_TRANS_GROUP
      ELSE
         MAX_MAINT_PLANNING_AREAS = 1
      ENDIF
!
      MAX_TRANS_GROUPS = 256
      MAX_DAY_TYPES = MIN(GET_MAX_DAY_TYPES(),GET_LAST_DAY_TYPE())
      IF(ALLOCATED(TG_2_PLANNING_AREA)) DEALLOCATE(
     +  TG_2_PLANNING_AREA)
      ALLOCATE(TG_2_PLANNING_AREA(0:MAX(1,UPPER_TRANS_GROUP)))
      IF(ALLOCATED(DAY_TYPE_TRANS_GROUP_PAIR))
     +          DEALLOCATE(DAY_TYPE_TRANS_GROUP_PAIR,
     +  DATA_BASE_POSITION)
      ALLOCATE(DAY_TYPE_TRANS_GROUP_PAIR(MAX_DAY_TYPES,
     +                                              0:MAX_TRANS_GROUPS))
      ALLOCATE(DATA_BASE_POSITION(2*MAX_DAY_TYPES,MAX_CL_UNITS))
      IF(ALLOCATED(MAINT_DAYS)) DEALLOCATE(MAINT_DAYS)
      ALLOCATE(MAINT_DAYS(MAX_CL_UNITS))
      IF(ALLOCATED(CL_ANN_CAP)) DEALLOCATE(CL_ANN_CAP,CL_TG_CAP,
     +                                     CL_TG_AFTER_PEAK,
     +                                     CL_TG_RETIRE,
     +                                     NEWGEN_CAP_BY_INDEX,
     +                                     CL_TG_CAP_MARKET_MW,
     +                                     CL_TG_CAP_MARKET_REV)
      ALLOCATE(CL_ANN_CAP(3,STUDY_PERIOD,2))
      ALLOCATE(CL_TG_CAP(0:6,0:MAX(1,UPPER_TRANS_GROUP),STUDY_PERIOD,2))
      ALLOCATE(NEWGEN_CAP_BY_INDEX(MAX_NEWGEN_INDEX,
     +                                   0:MAX(1,UPPER_TRANS_GROUP),
     +                                                  STUDY_PERIOD))
      ALLOCATE(CL_TG_AFTER_PEAK(0:UPPER_TRANS_GROUP,STUDY_PERIOD))
      ALLOCATE(CL_TG_CAP_MARKET_MW(0:UPPER_TRANS_GROUP),
     +         CL_TG_CAP_MARKET_REV(0:UPPER_TRANS_GROUP))
      ALLOCATE(CL_TG_RETIRE(0:UPPER_TRANS_GROUP,STUDY_PERIOD))
      IF(ALLOCATED(CL_ANNUAL_LOAD_REDUCTION))
     +         DEALLOCATE(CL_ANNUAL_LOAD_REDUCTION)
      ALLOCATE(CL_ANNUAL_LOAD_REDUCTION(STUDY_PERIOD))
      HIGHEST_ID = 1
!
      UNIT_FREQUENCY_DURATION = .FALSE.
!
      ZONAL_LEVEL_MARKET = YES_ZONAL_LEVEL_MARKET()
!
      UNIT_STATE_PROVINCE_INDEX = 0
      UNIT_GAS_REGION_INDEX = 0
      STATE_PROVINCE_INDEX = 0
      STATE_PROVINCE_ADDRESS = 0
      MAX_STATE_PROVINCE_NO = 0
      STATE_PROVINCE_NAMES(0) = "NA "

! 112307. HARD-WIRED TO FALSE FOR INTEGRATED PRICING


!
      YES_AOD_PRICE_THERM_VALUE_LIST =
     +        AOD_THERMAL_UNIT_VALUE_LIST(YES_AOD_COMP_THERM_VALUE_LIST)

      TURN_OFF_POLY = YES_TURN_OFF_POLY()

      NEW_BUILD_ADDITIONS_ACTIVE = YES_NEW_BUILD_ADDITIONS_ACTIVE()
      IF(NEW_BUILD_ADDITIONS_ACTIVE) THEN
         TEMP_L = GET_NEW_BUILD_PERCENTS(
     +                              UNDER_CONSTRUCTION_PERCENT,
     +                              ADVANCED_DEVELOPMENT_PERCENT,
     +                              EARLY_DEVELOPMENT_PERCENT,
     +                              PROPOSED_PERCENT,
     +                              INDEFIN_POSTPHONED_PERCENT)
      ENDIF
!
!
      TG_2_PLANNING_AREA = 0
      DO TG = 1, UPPER_TRANS_GROUP
!
         TG_2_PLANNING_AREA(TG) = GET_PA_FROM_TG(TG)
!
      ENDDO

      RETROFIT_ACTIVE = .FALSE.
      NEWGEN_INDEX = -1
      NEWGEN_COUNTER = 0

      START_UP_INDEX= 0

      RESOURCE_ID_TO_UNIT = 0

      NUMBER_OF_RESOURCE_ID = 0

      ANNUAL_GROSS_MARGIN = 0.
      ECON_RETIRE_MARGIN = 0.

      CALL RESET_NUCLEAR_UNIT_COUNTER()

      MONTHLY_MUST_RUN_VECTOR = 0
      MONTHLY_DECOMMIT_VECTOR = 0
      EMISSION_MARKET_LINK = 0
      CO2_RETRO_CAP_MULT = 0.0


      MAX_DAY_TYPES = MIN(GET_MAX_DAY_TYPES(),GET_LAST_DAY_TYPE())


      DAY_TYPE_TRANS_GROUP_PAIR = 0
      DATA_BASE_POSITION = 0.
!
!     TRACKING CENTRAL DISPATCHING FOR EVERY DAY TYPE ACROSS TRANSACTION GROUPS
!
      MAX_TRANS_DATA_BASES = MAX_DAY_TYPES
      DO M = 1, MAX_DAY_TYPES
         DAY_TYPE_TRANS_GROUP_PAIR(M,0) = M
      ENDDO
!
      SAVE_DETAILED_MAINTENANCE = .FALSE.
!
      CALL DOES_RDI_FILE_EXIST(RDI_FILE_EXISTS)
      IF(RDI_FILE_EXISTS) THEN
         CALL GET_NUM_OF_RDI_CL_RECORDS(RDI_RECORDS,RDI_CL_RECORDS)
         IF(RDI_CL_RECORDS > 0) THEN
            CALL OPEN_RDI_CL_FILES(RDI_IN_UNIT,RDI_OUT_UNIT)
            WRITE(RDI_OUT_UNIT,*) "9, "
         ENDIF
      ELSE
         RDI_RECORDS = 0
         RDI_CL_RECORDS = 0
      ENDIF

      SAVE_IGNORE_NON_UTILITY = IGNORE_NON_UTILITY()
!
      CALL RETURN_NUCLEAR_UNITS_ACTIVE(NUM_OF_NUCLEAR_UNITS)
      IF(NUM_OF_NUCLEAR_UNITS > 0) THEN
         CALL SETUP_GENERATING_INFORMATION(NUM_OF_NUCLEAR_UNITS)
      ENDIF
!
!
      CUBIC_HEAT_CURVE = 0.
!
      CL_ANN_CAP = 0.
      CL_TG_CAP = 0.
      NEWGEN_CAP_BY_INDEX = 0.
      CL_TG_AFTER_PEAK = 0.
      CL_TG_RETIRE = 0.
      RETROFIT_ACTIVE = .FALSE.
      PRIM_HEAT_CONTENT = 0.
      NOX_CONTROL_PERCENT = 0.
      SOX_CONTROL_PERCENT = 0.
      CO2_CONTROL_PERCENT = 0.
      HG_CONTROL_PERCENT = 0.
      OTHER3_CONTROL_PERCENT = 0
      CO2_CONTROL_DATE = 0
      HG_CONTROL_DATE = 0
      OTHER3_CONTROL_DATE = 0.
      EMERGENCY_CAPACITY = 0.
      EMERGENCY_HEATRATE = 0.
      CAP_MARKET_MONTH_NO = 0
      capacity_market_pointer_acl = 0
      RETROFIT_UNIT_INDEX = 0
      RETROFIT_COUNTER = 0
      RETIREMENT_COUNTER = 0

      POINTER_FOR_NEW_CL_UNIT = 0
      CL_ANNUAL_LOAD_REDUCTION = 0.
      CL_CAP_AREA_LINKED = .FALSE.
      SP_NEWGEN_UNIT_STATUS = " "
      CAP_LIMITED_CLASS_POINTER = 0
      NUMBER_OF_CAP_LIMITED_CLASSES = 0
      SAVE_DETAILED_MAINTENANCE = .FALSE.

      CALL DOES_TG_FILE_EXIST(TG_FILE_EXISTS)
      CALL DOES_TF_FILE_EXIST(TF_FILE_EXISTS)
      RUN_TRANSACT = TG_FILE_EXISTS .AND. UPPER_TRANS_GROUP > 0

      RDI_REC = 0
      RDI_CL_REC = 0
      NUNITS = 1
      UNIQUE_ID_NUM = 0
      CL_UNIT_UNIQUE_RPT_ID = 0
      TOTAL_START_UP_UNITS = 0
      PROCESSING_FIRST_DATA_FILE = .TRUE.
      PHASE_I_UNIT = .FALSE.
         if(.false.)
     +  write(3471,*) "BASECASE_PLANT_ID,"//
     +            "STATE_PROV_NAME,"//
     +            "BASECASE_UNIT_ID,"//
     +            "TRANSACTION_GROUP_ID,"//
     +            "BASECASE_MARKET_AREA_ID,"//
     +            "BASECASE_TRANS_AREA_ID"
      DO FILE_ID = 0, MAX_CL_FILES-1
!
         IF(.NOT. SP_CAPEX_ACTIVE() .AND. FILE_ID == 23) CYCLE
         CALL OPEN_CL_FILE(INT2(10),CL_RECORDS,FILE_ID)
         IF(CL_RECORDS == 0) CYCLE
         IREC = 0
!              ! Filenames agree between reference and current
               inquire(unit=10, NAME=file_name)
               call write_log_entry("cla_objt:0024",
     + "Reading " // trim(just_the_filename(file_name)) //
     + " (unit 10) ")

         DOWHILE (IREC < CL_RECORDS .OR. RDI_CL_REC < RDI_CL_RECORDS)
            IF(IREC < CL_RECORDS) THEN
               IREC = IREC + 1

               UNIQUE_ID_NUM = UNIQUE_ID_NUM + 1

!              ! First array read - HAS BAD DATA AFTER THIS READ!
               READ(10,REC=IREC,IOSTAT=IOS)DELETE,
     +  SP_UNIT_NAME(NUNITS),
     +            CL_LOAD_TYPE,
     +            EXPENSE_ASSIGNMENT(NUNITS),
     +  EXPENSE_COLLECTION(NUNITS),
     +            GENGRP(NUNITS),foshyd.CAP_FRAC_OWN(NUNITS),
     +            ON_LINE_MONTH,
     +            ON_LINE_YEAR,OFF_LINE_MONTH(NUNITS),
     +            OFF_LINE_YEAR(NUNITS),FUEL_MIX_PTR(NUNITS),
     +            PBTUCT(NUNITS),PFESCR(NUNITS),SBTUCT(NUNITS),
     +            SFESCR(NUNITS),
     +            FUELADJ(NUNITS),EFOR(NUNITS),
     +            (MNRATE(NUNITS,M),M=1,12),
     +            VCPMWH_IN(NUNITS),OMESCR(NUNITS),
     +            FIXED_COST_IN(NUNITS),FIXED_COST_ESCALATOR(NUNITS),
     +            DISPADJ(NUNITS),HR_FACTOR(NUNITS),
     +            (INPUT_MW(M,NUNITS),M=1,2),
     +            foshyd.CAP_PLANNING_FAC(NUNITS),
     +            (COEFF(M,NUNITS),M=1,3),
     +            CL_AI_CAPACITY_RATE(NUNITS),
     +            CL_AI_CAPACITY_ESCALATOR(NUNITS),
     +            CL_AI_ENERGY_RATE(NUNITS),
     +            CL_AI_ENERGY_ESCALATOR(NUNITS),
     +            AI_CL_REMAINING_LIFE(NUNITS),
     +            P_SO2(NUNITS),P_NOX(NUNITS),P_PARTICULATES(NUNITS),
     +            LOCAL_CL_POOL_FRAC_OWN(NUNITS),
     +            P_EMIS_OTH2(NUNITS),
     +            P_EMIS_OTH3(NUNITS),
     +            PHASE_I_STR,
     +            DISPADJ2(NUNITS),                  ! 55
     +            CL_RESOURCE_ID(NUNITS),
     +            FUEL_SUPPLY_ID(NUNITS),
     +            P_NOX_BK2(NUNITS),
     +            SEC_FUEL_EMISS_PTR(NUNITS),
     +            EMISS_FUEL_COST(NUNITS),           ! 60
     +            EMISS_FUEL_ESCAL(NUNITS),
     +            EMISS_FUEL_EMISS_PTR(NUNITS),
     +            EMISS_BLENDING_RATE(NUNITS),
     +            PRIM_FUEL_EMISS_PTR(NUNITS),
     +            TEMP_PRIM_FUEL_TYPE_STR,
     +            SEC_FUEL_TYPE(NUNITS),
     +            EMISS_FUEL_TYPE(NUNITS),
     +            TIE_CONSTRAINT_GROUP(NUNITS),
     +            MONTHLY_CAPACITY_POINTER(NUNITS),
     +            ANNUAL_CL_FIXED_COST(NUNITS),       ! 70
     +            ANNUAL_CL_FIXED_COST_ESC(NUNITS),
     +            MAINT_DAYS(NUNITS),
     +            DISPATCH_MULT(NUNITS),
     +            EXCESS_ENERGY_SALES(NUNITS),
     +            AI_CL_TAX_LIFE(NUNITS),
     +            AI_CL_ADR_LIFE(NUNITS),
     +            ASSET_CLASS_NUM(NUNITS),
     +            ASSET_CLASS_VECTOR(NUNITS),
     +            INTRA_COMPANY_CLASS_ID(NUNITS),
     +            INTRA_COMPANY_TRANSACTION(NUNITS),  ! 80
     +            SPECIAL_UNIT_ID(NUNITS),
     +            MINIMUM_CAPACITY_FACTOR(NUNITS),
     +            MAXIMUM_CAPACITY_FACTOR(NUNITS),
     +            TRANSACTION_GROUP_ID(NUNITS),
     +            DAY_TYPE_ID(NUNITS),    ! 85
     +            UNIT_ACTIVE,
     +            BASECASE_PLANT_ID(NUNITS),
     +            BASECASE_UNIT_ID,
     +            BASECASE_MARKET_AREA_ID(NUNITS),
     +            BASECASE_TRANS_AREA_ID,
     +            BASECASE_PLANT_NERC_SUB_ID,
     +            BASECASE_PLANT_OWNER_ID,
     +            UTILITY_OWNED,
     +            MONTHLY_FUEL_INDEX(NUNITS),
     +            START_UP_COSTS(NUNITS),
     +            START_UP_LOGIC(NUNITS),
     +            RAMP_RATE(NUNITS),
     +            MIN_DOWN_TIME(NUNITS),
     +            REPORT_THIS_UNIT(NUNITS),
     +            P_FUEL_DELIVERY_1(NUNITS),
     +            P_FUEL_DELIVERY_2(NUNITS),
     +            P_FUEL_DELIVERY_3(NUNITS),
     +            MIN_UP_TIME(NUNITS),
     +            HESI_UNIT_ID_NUM(NUNITS),
     +            FOR_FREQUENCY(NUNITS),
     +            FOR_DURATION(NUNITS),
     +            WINTER_TOTAL_CAPACITY,
     +            CONTRIBUTES_TO_SPIN(NUNITS),
     +            H2_UNIT_ID_NUM(NUNITS),
     +            POWERDAT_PLANT_ID(NUNITS),
     +            APPLY_NOX_SEASON_DATE(NUNITS),
     +            NOX_SEASON_DATE(NUNITS),
     +            RAMP_DOWN_RATE(NUNITS),
     +            NOX_CONTROL_PERCENT(NUNITS),
     +            NOX_CONTROL_DATE(NUNITS),
     +            EMERGENCY_CAPACITY(NUNITS),
     +            EMERGENCY_HEATRATE(NUNITS),
     +            MARKET_RESOURCE(NUNITS),
     +            PRIMARY_MOVER_STR(NUNITS),
     +            COUNTY,
     +            STATE_PROVINCE,
     +            NOX_VOM(NUNITS),
     +            NOX_FOM(NUNITS),
     +            S_FUEL_DELIVERY(NUNITS),
     +            S_FUEL_DELIVERY_2(NUNITS),
     +            s_fuel_delivery_3(NUNITS),
     +            CONSTANT_POLY,
     +            FIRST_POLY,
     +            SECOND_POLY,
     +            THIRD_POLY,
     +            USE_POLY_HEAT_RATES,
     +            SP_NEWGEN_UNIT_STATUS(NUNITS),
     +            MONTHLY_MUST_RUN_VECTOR(NUNITS),
     +            EMISSION_MARKET_LINK(NUNITS),
     +            WVPA_RATE_TRACKER(NUNITS),
     +            ALLOW_DECOMMIT(NUNITS),
     +            MIN_SPIN_CAP(NUNITS),
     +            MAX_SPIN_CAP(NUNITS),
     +            WVPA_RES_TRACKER(NUNITS),
     +            WVPA_FUEL_TRACKER(NUNITS),
     +            MONTE_OR_FREQ(NUNITS),
     +            WVPA_MEM_TRACKER(NUNITS),
     +            MONTHLY_DECOMMIT_VECTOR(NUNITS),
     +            TRANS_BUS_ID(NUNITS),
     +            SOX_CONTROL_PERCENT(NUNITS),
     +            SOX_CONTROL_DATE(NUNITS),
     +            SOX_VOM(NUNITS),
     +            SOX_FOM(NUNITS),
     +            LATITUDE(NUNITS),
     +            LONGITUDE(NUNITS),
     +            MW_INSIDE_FENCE,
     +            (INTER_BLOCKS(M,NUNITS),M=1,3),
! SPCapEx Variables
     +            FIRST_YEAR_DECOMM_AVAIALABLE(NUNITS),  ! 155
     +            DECOMMISSIONING_BASE_YR_COST(NUNITS),
     +            DECOM_CONT_COST_ESCALATION(NUNITS),
     +            ANNUAL_ENERGY_PLANNING_FACTOR(NUNITS),   ! 158
     +            AGGREGATE_THIS_UNIT(NUNITS),             ! 159
     +            NAME_PLATE_CAPACITY(NUNITS),
! NO, FIXED BLEND, MODEL DETERMINED IF ONLY ONE FUEL ELSE USER OPTION
     +            FUEL_BLENDING_IS(NUNITS),
     +            FuelRatio(1,Nunits),
     +            FuelRatio(2,Nunits),
     +            FuelRatio(3,Nunits),         ! 164
     +            FuelRatio(4,Nunits),
     +            FuelRatio(5,Nunits),         ! 166
     +            BlendableFuelsPtr(1,Nunits), ! 167
     +            BlendableFuelsPtr(2,Nunits), ! 168
     +            BlendableFuelsPtr(3,Nunits),
     +            BlendableFuelsPtr(4,Nunits),
     +            FuelTransportationCost(1,Nunits),
     +            FuelTransportationCost(2,Nunits),  ! 172
     +            FuelTransportationCost(3,Nunits),
     +            FuelTransportationCost(4,Nunits),
     +            BlendedEnthalpyUp(Nunits),
     +            BlendedEnthalpyLo(Nunits),
     +            BlendedSO2Up(Nunits),                  ! 177
     +            BettermentProjectID(Nunits),
     +            DECOM_CONTINUING_COST(Nunits),
     +            DECOM_CONT_COST_ESCALATION(Nunits),
     +            EnrgPatternPointer(Nunits), ! 181
     +            EMISSION_DATA_UNITS(Nunits),
     +            EmissRedRate(1,Nunits),
     +            EmissRedRate(2,Nunits),
     +            EmissRedRate(3,Nunits),
     +            EmissRedRate(4,Nunits),              ! 186
     +            EmissRedRate(5,Nunits),
     +            EmissMaxRate(1,Nunits),
     +            EmissMaxRate(2,Nunits),
     +            EmissMaxRate(3,Nunits),
     +            EmissMaxRate(4,Nunits),
     +            EmissMaxRate(5,Nunits),
     +            MaxEnergyLimit(1,Nunits),
     +            MaxEnergyLimit(2,Nunits),
     +            MaxEnergyLimit(3,Nunits),
     +            TECH_TYPE(Nunits),
     +            LINKED_BETTERMENT_OPTION(Nunits),
     +            CO2_CONTROL_PERCENT(NUNITS),
     +            HG_CONTROL_PERCENT(NUNITS),
     +            OTHER3_CONTROL_PERCENT(NUNITS),
     +            CO2_CONTROL_DATE(NUNITS),
     +            HG_CONTROL_DATE(NUNITS),
     +            OTHER3_CONTROL_DATE(NUNITS),
     +            CO2_VOM(NUNITS),
     +            CO2_FOM(NUNITS),
     +            HG_VOM(NUNITS),
     +            HG_FOM(NUNITS),
     +            OTHER3_VOM(NUNITS),
     +            OTHER3_FOM(NUNITS),
     +            MARKET_FLOOR(NUNITS),
     +            MARKET_CEILING(NUNITS),
     +            START_UP_COSTS_ESCALATION(NUNITS),  ! 212
     +            CAPACITY_MARKET_TYPE(NUNITS),
     +            CAPACITY_MARKET_MONTH(NUNITS),
     +            capacity_market_pointer_acl(NUNITS),
     +            CAPACITY_MARKET_COST_ASSIGN(NUNITS),
     +            CAPACITY_MARKET_EXP_COLLECT(NUNITS),
     +            CAPACITY_MARKET_COIN_ADJ_FACT(NUNITS),
     +            PRIMARY_FUEL_CATEGORY(NUNITS),
     +            SECONDARY_FUEL_CATEGORY(NUNITS),
     +            EMISSIONS_FUEL_CATEGORY(NUNITS),
     +            RPS_CONTRIBUTION_PERCENT(NUNITS),
     +            RETIREMENT_CANDIDATE(NUNITS),
     +            RETROFIT_CANDIDATE(NUNITS),
     +            RETROFIT_PROJECT_ID(NUNITS),
     +            CO2_BASIN_NAME,
     +            CO2_PIPELINE_DISTANCE(NUNITS),
     +            STATE_PROV_NAME,
     +            CO2_RETRO_HEAT_MULT(NUNITS),
     +            CO2_RETRO_CAP_MULT(NUNITS),
     +            THERMAL_GUID,
     +            THERMAL_PARENT_ID, ! 232
     +            THERMAL_AGGREGATED_UNIT,
     +            FIRST_RETIREMENT_YEAR(NUNITS),
     +            UNIT_TYPE,
     +            UNIT_TYPE_CATEGORY,
     +            RPS_PROGRAM_NUMBER(NUNITS)
       if(cla_bad(int(NUNITS))) then
        call end_program("cla_objt:00012 - cla_bad(" //
     + trim(itos(int(nunits))) // ")=" //
     + trim(itos(int(capacity_market_pointer_acl(nunits))))
     + // " at point of read. Filename: " //
     + trim(file_name) // ".")
       endif
       if(abs(capacity_market_pointer_acl(nunits))>=28974) then
        unit_type=unit_type ! Debugstop
        call end_program("cla_objt:0007 - capacity_market_" //
     + "pointer at " // trim(itos(int(nunits))) // " contains " //
     + "invalid value of " //
     + trim(itos(int(capacity_market_pointer_acl(nunits)))))
       endif
                 if(primary_fuel_id>=-2.89760000E+004 .and.
     + primary_fuel_id/=0)then
                call end_program("cla_objt:0003 - " //
     + "Primary Fuel ID is too high.")
            endif

       if(cla_bad(int(capacity_market_pointer_acl(max_cl_units)))) then
           call end_program("cla_objt:0004 - capacity_market_pointer_"
     + // "acl(" // trim(itos(int(max_cl_units))) //
     + ") contains invalid (too large) value.")

       endif

!      ! After first array read
       name_of_file=" "



        inquire(unit=rdi_in_unit, file=name_of_file)
         if(.false.) unit_active=unit_active
         elseIF(RDI_REC < RDI_RECORDS .AND.
     +         PROCESSING_FIRST_DATA_FILE) THEN
               RDI_REC = RDI_REC + 1
               UNIT_ACTIVE = 'T'

        inquire(unit=RDI_IN_UNIT, NAME=file_name)
        call write_log_entry("cla_objt:0017",
     + "Reading " // trim(just_the_filename(file_name)) //
     + " (unit " // trim(itos(int(RDI_IN_UNIT))) // " RDI_IN_UNIT")
        if(cla_bad(int(NUNITS))) then
           call end_program("cla_objt:0006 - capacity_market_pointer" //
     + " array contains invalid value.")
        endif


               READ(RDI_IN_UNIT,REC=RDI_REC)
     +            DELETE,
     +            RDI_COMPANY_NAME,
     +            RDI_NERC_REGION,
     +            RDI_SUB_REGION,
     +            UNITNM(NUNITS),
     +            RDI_PM_ABBREV,
     +            RDI_PRIME_FUEL,
     +            ON_LINE_YEAR,! NUMBERS ! INT
     +            ON_LINE_MONTH, ! INT
     +            foshyd.CAP_FRAC_OWN(NUNITS),
     +            RDI_DEMONSTRATED_CAPACITY_MW,
     +            RDI_INCREMENTAL_FUEL_USDMWH,
     +            PBTUCT(NUNITS),
     +            VCPMWH_IN(NUNITS),
     +            FIXED_COST_IN(NUNITS),
     +            RDI_AVERAGE_HEAT_RATE,
     +            RDI_MONTH_GENERATION, ! 12 VALUES
     +            RDI_MONTH_CAPACITY ! 12 VALUES
!
               IF(trim(RDI_PM_ABBREV) == 'HY') CYCLE ! THIS PERMITS ONE BIN FILE
!
               RDI_CL_REC = RDI_CL_REC + 1
!
               CL_LOAD_TYPE = 'B '
               EXPENSE_ASSIGNMENT(NUNITS) = 'F' !ossil Fuel
               EXPENSE_COLLECTION(NUNITS) = 'A' !djustment Clause'
               GENGRP(NUNITS) = 0
               OFF_LINE_MONTH(NUNITS) = 12
               OFF_LINE_YEAR(NUNITS) = 2050
               ON_LINE_MONTH = 1 ! HARD WIRED
!
               foshyd.CAP_FRAC_OWN(NUNITS) = 100.0 ! OWNERSHIP APPEARS TO BE ACCOUNTED FOR
!
               FUEL_MIX_PTR(NUNITS) = 1. ! FUELMX(NUNITS) = 1.
               PFESCR(NUNITS) = 0.
               SBTUCT(NUNITS) = 0.
               SFESCR(NUNITS) = 0
               FUELADJ(NUNITS) = 0.
!
               IF(RDI_DEMONSTRATED_CAPACITY_MW < 100.) THEN
                  RDI_SIZE_INDEX = 1
                  IF(RDI_PRIME_FUEL(1:4) == 'COAL') THEN
                     RDI_FUEL_INDEX = 1
                  ELSEIF(RDI_PRIME_FUEL(1:3) == 'OIL') THEN
                     RDI_FUEL_INDEX = 2
                  ELSEIF(RDI_PRIME_FUEL(1:4) == 'URAN') THEN
                     RDI_FUEL_INDEX = 3
                  ELSEIF(RDI_PRIME_FUEL(1:3) == 'GAS') THEN
                     RDI_FUEL_INDEX = 4
                  ELSE ! LIGNITE
                     RDI_FUEL_INDEX = 5
                  ENDIF
               ELSEIF(RDI_DEMONSTRATED_CAPACITY_MW >= 100. .AND.
     +                        RDI_DEMONSTRATED_CAPACITY_MW < 200.) THEN
                  RDI_SIZE_INDEX = 2
                  IF(RDI_PRIME_FUEL(1:4) == 'COAL') THEN
                     RDI_FUEL_INDEX = 1
                  ELSEIF(RDI_PRIME_FUEL(1:3) == 'OIL') THEN
                     RDI_FUEL_INDEX = 2
                  ELSEIF(RDI_PRIME_FUEL(1:4) == 'URAN') THEN
                     RDI_FUEL_INDEX = 3
                  ELSEIF(RDI_PRIME_FUEL(1:3) == 'GAS') THEN
                     RDI_FUEL_INDEX = 4
                  ELSE ! LIGNITE
                     RDI_FUEL_INDEX = 5
                  ENDIF
               ELSEIF(RDI_DEMONSTRATED_CAPACITY_MW >= 200. .AND.
     +                        RDI_DEMONSTRATED_CAPACITY_MW < 300.) THEN
                  RDI_SIZE_INDEX = 3
                  IF(RDI_PRIME_FUEL(1:4) == 'COAL') THEN
                     RDI_FUEL_INDEX = 1
                  ELSEIF(RDI_PRIME_FUEL(1:3) == 'OIL') THEN
                     RDI_FUEL_INDEX = 2
                  ELSEIF(RDI_PRIME_FUEL(1:4) == 'URAN') THEN
                     RDI_FUEL_INDEX = 3
                  ELSEIF(RDI_PRIME_FUEL(1:3) == 'GAS') THEN
                     RDI_FUEL_INDEX = 4
                  ELSE ! LIGNITE
                     RDI_FUEL_INDEX = 5
                  ENDIF
               ELSEIF(RDI_DEMONSTRATED_CAPACITY_MW >= 300. .AND.
     +                        RDI_DEMONSTRATED_CAPACITY_MW < 400.) THEN
                  RDI_SIZE_INDEX = 4
                  IF(RDI_PRIME_FUEL(1:4) == 'COAL') THEN
                     RDI_FUEL_INDEX = 1
                  ELSEIF(RDI_PRIME_FUEL(1:3) == 'OIL') THEN
                     RDI_FUEL_INDEX = 2
                  ELSEIF(RDI_PRIME_FUEL(1:4) == 'URAN') THEN
                     RDI_FUEL_INDEX = 3
                  ELSEIF(RDI_PRIME_FUEL(1:3) == 'GAS') THEN
                     RDI_FUEL_INDEX = 4
                  ELSE ! LIGNITE
                     RDI_FUEL_INDEX = 5
                  ENDIF
               ELSEIF(RDI_DEMONSTRATED_CAPACITY_MW >= 400. .AND.
     +                        RDI_DEMONSTRATED_CAPACITY_MW < 600.) THEN
                  RDI_SIZE_INDEX = 5
                  IF(RDI_PRIME_FUEL(1:4) == 'COAL') THEN
                     RDI_FUEL_INDEX = 1
                  ELSEIF(RDI_PRIME_FUEL(1:3) == 'OIL') THEN
                     RDI_FUEL_INDEX = 2
                  ELSEIF(RDI_PRIME_FUEL(1:4) == 'URAN') THEN
                     RDI_FUEL_INDEX = 3
                  ELSEIF(RDI_PRIME_FUEL(1:3) == 'GAS') THEN
                     RDI_FUEL_INDEX = 4
                  ELSE ! LIGNITE
                     RDI_FUEL_INDEX = 5
                  ENDIF
               ELSEIF(RDI_DEMONSTRATED_CAPACITY_MW >= 600. .AND.
     +                         RDI_DEMONSTRATED_CAPACITY_MW < 800.) THEN
                  RDI_SIZE_INDEX = 6
                  IF(RDI_PRIME_FUEL(1:4) == 'COAL') THEN
                     RDI_FUEL_INDEX = 1
                  ELSEIF(RDI_PRIME_FUEL(1:3) == 'OIL') THEN
                     RDI_FUEL_INDEX = 2
                  ELSEIF(RDI_PRIME_FUEL(1:4) == 'URAN') THEN
                     RDI_FUEL_INDEX = 3
                  ELSEIF(RDI_PRIME_FUEL(1:3) == 'GAS') THEN
                     RDI_FUEL_INDEX = 4
                  ELSE ! LIGNITE
                     RDI_FUEL_INDEX = 5
                  ENDIF
               ELSEIF(RDI_DEMONSTRATED_CAPACITY_MW >= 800. .AND.
     +                        RDI_DEMONSTRATED_CAPACITY_MW < 1000.) THEN
                  RDI_SIZE_INDEX = 7
                  IF(RDI_PRIME_FUEL(1:4) == 'COAL') THEN
                     RDI_FUEL_INDEX = 1
                  ELSEIF(RDI_PRIME_FUEL(1:3) == 'OIL') THEN
                     RDI_FUEL_INDEX = 2
                  ELSEIF(RDI_PRIME_FUEL(1:4) == 'URAN') THEN
                     RDI_FUEL_INDEX = 3
                  ELSEIF(RDI_PRIME_FUEL(1:3) == 'GAS') THEN
                     RDI_FUEL_INDEX = 4
                  ELSE ! LIGNITE
                     RDI_FUEL_INDEX = 5
                  ENDIF
               ELSE ! > 1000 MW
                  RDI_SIZE_INDEX = 8
                  IF(RDI_PRIME_FUEL(1:4) == 'COAL') THEN
                     RDI_FUEL_INDEX = 1
                  ELSEIF(RDI_PRIME_FUEL(1:3) == 'OIL') THEN
                     RDI_FUEL_INDEX = 2
                  ELSEIF(RDI_PRIME_FUEL(1:4) == 'URAN') THEN
                     RDI_FUEL_INDEX = 3
                  ELSEIF(RDI_PRIME_FUEL(1:3) == 'GAS') THEN
                     RDI_FUEL_INDEX = 4
                  ELSE ! LIGNITE
                     RDI_FUEL_INDEX = 5
                  ENDIF
               ENDIF
!
               EFOR(NUNITS)=GADS_FOR(RDI_SIZE_INDEX,RDI_FUEL_INDEX)*100.
               MAINT_DAYS(NUNITS) =
     +                           GADS_MOR(RDI_SIZE_INDEX,RDI_FUEL_INDEX)

               MNRATE(NUNITS,1) = 0.
               MNRATE(NUNITS,2) = 0.
               MNRATE(NUNITS,3) = 0.
               MNRATE(NUNITS,4) = 0.
               MNRATE(NUNITS,5) = 0.
               MNRATE(NUNITS,6) = 0.
               MNRATE(NUNITS,7) = 0.
               MNRATE(NUNITS,8) = 0.
               MNRATE(NUNITS,9) = 0.
               MNRATE(NUNITS,10) = 0.
               MNRATE(NUNITS,11) = 0.
               MNRATE(NUNITS,12) = 0.
               OMESCR(NUNITS) = 0.
               FIXED_COST_ESCALATOR(NUNITS) = 0.
               DISPADJ(NUNITS) = 0.
               HR_FACTOR(NUNITS) = 1.0
               INPUT_MW(1,NUNITS) = RDI_DEMONSTRATED_CAPACITY_MW
               INPUT_MW(2,NUNITS) = RDI_DEMONSTRATED_CAPACITY_MW
               foshyd.CAP_PLANNING_FAC(NUNITS) = 1.0
               COEFF(1,NUNITS) = RDI_AVERAGE_HEAT_RATE
               COEFF(2,NUNITS) = RDI_AVERAGE_HEAT_RATE
               COEFF(3,NUNITS) = RDI_AVERAGE_HEAT_RATE
               CL_AI_CAPACITY_RATE(NUNITS) = 0.
               CL_AI_CAPACITY_ESCALATOR(NUNITS) = 0
               CL_AI_ENERGY_RATE(NUNITS) = 0.
               CL_AI_ENERGY_ESCALATOR(NUNITS) = 0
               AI_CL_REMAINING_LIFE(NUNITS) = 1
               P_SO2(NUNITS) = 0.
               P_NOX(NUNITS) = 0.
               P_PARTICULATES(NUNITS) = 0.
               LOCAL_CL_POOL_FRAC_OWN(NUNITS) = 100.
               P_EMIS_OTH2(NUNITS) = 0.
               P_EMIS_OTH3(NUNITS) = 0.
               PHASE_I_UNIT(NUNITS) = .FALSE.
               DISPADJ2(NUNITS) = 0.
               CL_RESOURCE_ID(NUNITS) = 0
               FUEL_SUPPLY_ID(NUNITS) = 0
               P_NOX_BK2(NUNITS) = 0.
               SEC_FUEL_EMISS_PTR(NUNITS) = 0
               EMISS_FUEL_COST(NUNITS) =  0.
               EMISS_FUEL_ESCAL(NUNITS) =  0
               EMISS_FUEL_EMISS_PTR(NUNITS) = 0
               EMISS_BLENDING_RATE(NUNITS) = 0.
               PRIM_FUEL_EMISS_PTR(NUNITS) = 0
               IF(trim(RDI_PRIME_FUEL) == 'OIL-L') THEN
                  PRIM_FUEL_TYPE_STR = 'Oil   '
                  SEC_FUEL_TYPE(NUNITS) = 'O' !il   '
               ELSEIF(trim(RDI_PRIME_FUEL) == 'GAS') THEN
                  PRIM_FUEL_TYPE_STR = 'Gas   '
                  SEC_FUEL_TYPE(NUNITS) = 'G' !as   '
               ELSE
                  PRIM_FUEL_TYPE_STR = 'Coal  '
                  SEC_FUEL_TYPE(NUNITS) = 'C' !oal  '
               ENDIF
               EMISS_FUEL_TYPE(NUNITS) = 'C' !oal  '
               TIE_CONSTRAINT_GROUP(NUNITS) = 0.
               MONTHLY_CAPACITY_POINTER(NUNITS) = 0.
               ANNUAL_CL_FIXED_COST(NUNITS) = 0.
               ANNUAL_CL_FIXED_COST_ESC(NUNITS) = 0.

               DISPATCH_MULT(NUNITS) = 1.0
               EXCESS_ENERGY_SALES(NUNITS) = 0.
               AI_CL_TAX_LIFE(NUNITS) = 1
               AI_CL_ADR_LIFE(NUNITS) = 1
               ASSET_CLASS_NUM(NUNITS) = 0
               ASSET_CLASS_VECTOR(NUNITS) = 0
               INTRA_COMPANY_CLASS_ID(NUNITS) = 0
               INTRA_COMPANY_TRANSACTION(NUNITS) = 'N'
               RDI_DESC = trim(RDI_COMPANY_NAME)//':POWERDAT'
               SPECIAL_UNIT_ID(NUNITS) = 'NONE'
               MINIMUM_CAPACITY_FACTOR(NUNITS) = 0.
               MAXIMUM_CAPACITY_FACTOR(NUNITS) = 1.
               TRANSACTION_GROUP_ID(NUNITS) = 1
               DAY_TYPE_ID(NUNITS) = 0
! THIS CREATES A MIDAS DATA READABLE CL FILE
               WRITE(RDI_OUT_UNIT,1000)
     +            "1,",
     +            trim(UNITNM(NUNITS)),
     +            trim(CL_LOAD_TYPE),
     +            EXPENSE_ASSIGNMENT(NUNITS),
     +  EXPENSE_COLLECTION(NUNITS),
     +            GENGRP(NUNITS),foshyd.CAP_FRAC_OWN(NUNITS),
     +            ON_LINE_MONTH,
     +            ON_LINE_YEAR,OFF_LINE_MONTH(NUNITS),
     +            OFF_LINE_YEAR(NUNITS),FUEL_MIX_PTR(NUNITS), ! FUELMX(NUNITS), replaced 5/17/01 M.S.G.
     +            PBTUCT(NUNITS),PFESCR(NUNITS),SBTUCT(NUNITS),
     +            SFESCR(NUNITS),
     +            FUELADJ(NUNITS),EFOR(NUNITS),
     +            (MNRATE(NUNITS,M),M=1,12),
     +            VCPMWH_IN(NUNITS),OMESCR(NUNITS),
     +            FIXED_COST_IN(NUNITS),
     +            FIXED_COST_ESCALATOR(NUNITS),
     +            DISPADJ(NUNITS),
     +            HR_FACTOR(NUNITS),
     +            (INPUT_MW(M,NUNITS),M=1,2),
     +            foshyd.CAP_PLANNING_FAC(NUNITS),
     +            (COEFF(M,NUNITS),M=1,3),
     +            trim(RDI_DESC),
     +            P_SO2(NUNITS),
     +            P_NOX(NUNITS),
     +            P_PARTICULATES(NUNITS),
     +            LOCAL_CL_POOL_FRAC_OWN(NUNITS),
     +            P_EMIS_OTH2(NUNITS),
     +            P_EMIS_OTH3(NUNITS),
     +            PHASE_I_UNIT(NUNITS),
     +            DISPADJ2(NUNITS),
     +            CL_RESOURCE_ID(NUNITS),
     +            FUEL_SUPPLY_ID(NUNITS),
     +            P_NOX_BK2(NUNITS),
     +            SEC_FUEL_EMISS_PTR(NUNITS),
     +            EMISS_FUEL_COST(NUNITS),
     +            EMISS_FUEL_ESCAL(NUNITS),
     +            EMISS_FUEL_EMISS_PTR(NUNITS),
     +            EMISS_BLENDING_RATE(NUNITS),
     +            PRIM_FUEL_EMISS_PTR(NUNITS),
     +            trim(PRIM_FUEL_TYPE_STR),
     +            trim(SEC_FUEL_TYPE(NUNITS)),
     +            trim(EMISS_FUEL_TYPE(NUNITS)),
     +            TIE_CONSTRAINT_GROUP(NUNITS),
     +            MONTHLY_CAPACITY_POINTER(NUNITS),
     +            CL_AI_CAPACITY_RATE(NUNITS),
     +            CL_AI_CAPACITY_ESCALATOR(NUNITS),
     +            CL_AI_ENERGY_RATE(NUNITS),
     +            CL_AI_ENERGY_ESCALATOR(NUNITS),
     +            AI_CL_REMAINING_LIFE(NUNITS),
     +            ANNUAL_CL_FIXED_COST(NUNITS),
     +            ANNUAL_CL_FIXED_COST_ESC(NUNITS),
     +            MAINT_DAYS(NUNITS),
     +            DISPATCH_MULT(NUNITS),
     +            EXCESS_ENERGY_SALES(NUNITS),
     +            AI_CL_TAX_LIFE(NUNITS),
     +            AI_CL_ADR_LIFE(NUNITS),
     +            ASSET_CLASS_NUM(NUNITS),
     +            ASSET_CLASS_VECTOR(NUNITS),
     +            INTRA_COMPANY_CLASS_ID(NUNITS),
     +            INTRA_COMPANY_TRANSACTION(NUNITS),
     +            trim(SPECIAL_UNIT_ID(NUNITS)),
     +            MINIMUM_CAPACITY_FACTOR(NUNITS),
     +            MAXIMUM_CAPACITY_FACTOR(NUNITS),
     +            TRANSACTION_GROUP_ID(NUNITS),
     +            DAY_TYPE_ID(NUNITS)

 1000             FORMAT(  A,4('"',A,'"',','),
     +               I3,',',F9.2,',',4(I5,','),
     +               2(F9.2,','),I5,',',F9.2,',',I5,',',
     +               15(F9.2,','),I5,',',
     +               F9.2,',',I5,',',8(F9.2,','),'"',A,'"',',', ! DESCRIPTION
     +               6(F9.2,','),
     +               L1,',',F9.2,',',2(I5,','),F9.2,',',I5,',',
     +               F9.2,',',2(I5,','),
     +               F9.2,',',I5,',',3(A,','),
     +               F9.2,',',I5,',', !CAP DISTN POINTER
     +               F9.2,',',I5,',',F9.2,',',I5,',',I5,',', ! AI VARIABLES
     +               F9.2,',',I5,',', ! FIXED COSTS
     +               5(F9.2,','),
     +               3(I5,','),2(A,','),2(F9.2,','),2(I5,','))
!
            ELSE
               WRITE(4,*) "Untrapped error when processing"
               WRITE(4,*) "Capacity Limited Units"
               WRITE(4,*) '*** line 1949 CLA_OBJT.FOR ***'
               er_message='See WARNING MESSAGES-cla_objt-1'
               call end_program(er_message)
            ENDIF
            IF(IOS /= 0) EXIT

! CHECK FOR DUPLICATE NAME
!

!           ! TODO:  CHECK FOR DUPLICATE NAME shouldn't be a
!           ! comment. It should be a function call.
            UNIT_NAME = ADJUSTL(SP_UNIT_NAME(NUNITS))

            DO ID = 1, NUNITS-1
               IF(LEN_TRIM(SP_UNIT_NAME(ID)) ==
     +                                   LEN_TRIM(UNIT_NAME) .AND.
     +                 INDEX(SP_UNIT_NAME(ID),UNIT_NAME) /= 0) THEN
                  WRITE(TEMP_UNIT_NAME,'(A,I2.2)')
     +                     TRIM(UNIT_NAME),UNIT_NAME_COUNT(ID)
                  UNIT_NAME_COUNT(ID) = UNIT_NAME_COUNT(ID) + 1
                  WRITE(4,*) 'The unit name '//
     +                      TRIM(UNIT_NAME)//' has been used before.'
                  WRITE(4,*) 'The name has been changed to '//
     +                                           TRIM(TEMP_UNIT_NAME)
                  SP_UNIT_NAME(NUNITS) = TEMP_UNIT_NAME
                  EXIT
               ENDIF
            ENDDO
! END UNIQUE NAME FIX
! End TODO: Check for duplicate name

            UNITNM(NUNITS) = SP_UNIT_NAME(NUNITS)
            NEWGEN_UNIT_STATUS = SP_NEWGEN_UNIT_STATUS(NUNITS)
            IF(INDEX(NEWGEN_UNIT_STATUS,"SPCapEx") /= 0) THEN
               ON_LINE_YEAR = 2000
               OFF_LINE_YEAR(NUNITS) = 2200
               MW_INSIDE_FENCE = 0.
            ENDIF

            IF(ZONAL_LEVEL_MARKET) THEN
               MARKET_ID = BASECASE_MARKET_AREA_ID(NUNITS)
               TRANSACTION_GROUP_ID(NUNITS) =
     +                                     MARKET_AREA_LOOKUP(MARKET_ID)
            ENDIF
!

            TRANSACTION_GROUP_ID(NUNITS) =
     +                GET_BELONGS_TO_GROUP(TRANSACTION_GROUP_ID(NUNITS))
!

            IF(INPUT_MW(2,NUNITS) > 1. .AND. MW_INSIDE_FENCE > .1) THEN
               PERCENT_INSIDE_FENCE = MIN(1.,
     +                               MW_INSIDE_FENCE/INPUT_MW(2,NUNITS))
            ELSE
               PERCENT_INSIDE_FENCE = 0.
            ENDIF

            IF(YES_AOD_PRICE_THERM_VALUE_LIST) THEN
               IF(.NOT.
     +            AOD_THERMAL_LIST_COMPARE(HESI_UNIT_ID_NUM(NUNITS)))
     +                                                             CYCLE
            ELSEIF(YES_AOD_COMP_THERM_VALUE_LIST) THEN
               IF(.NOT.
     +            AOD_THERMAL_LIST_COMPARE(HESI_UNIT_ID_NUM(NUNITS)))
     +                                                              THEN
                  REPORT_THIS_UNIT(NUNITS) = 'F'
               ENDIF
            ENDIF
!
            TRANS_ID = TRANSACTION_GROUP_ID(NUNITS)
            IF(DELETE > 7 .OR.
     +             (RUN_TRANSACT .AND.
     +               (.NOT. TRANS_GROUP_ACTIVE_SWITCH(TRANS_ID) .OR.
     +                .NOT. IN_ACTIVE_THERMAL_MARKET_AREA(
     +                         BASECASE_MARKET_AREA_ID(NUNITS)))) .OR.
     +                                PERCENT_INSIDE_FENCE > .995) CYCLE
            CAP_MARKET_MONTH_NO(NUNITS) =
     +          CAP_MARKET_MONTH_NO_INDEX(CAPACITY_MARKET_MONTH(NUNITS))

            IF(PERCENT_INSIDE_FENCE > .005) THEN
             foshyd.CAP_FRAC_OWN(NUNITS) = foshyd.CAP_FRAC_OWN(NUNITS) *
     +                                         (1.-PERCENT_INSIDE_FENCE)
            ENDIF

            IF(MONTE_OR_FREQ(NUNITS) == 'F')
     +                                  UNIT_FREQUENCY_DURATION = .TRUE.

            IF(EMISSION_MARKET_LINK(NUNITS) == -9999) THEN
               EMISSION_MARKET_LINK(NUNITS) = ASSET_CLASS_NUM(NUNITS)
            ENDIF

            DO M = 2, 1 , -1
               IF(INPUT_MW(M,NUNITS) < 0.) THEN
                  LOCAL_MW(M) =
     +                      GET_VAR(INPUT_MW(M,NUNITS),INT2(1),
     +                                                   UNITNM(NUNITS))
                  IF(LOCAL_MW(M) < 0.) THEN
                     WRITE(4,*) "Negative value for capacity"
                     WRITE(4,*) "for unit ",UNITNM(NUNITS)
                     WRITE(4,*) " "
                  ENDIF
               ELSE
                  LOCAL_MW(M) = INPUT_MW(M,NUNITS)
               ENDIF
               IF(LOCAL_MW(M) < 1. .AND. M == 1) THEN
                  LOCAL_MW(1) = LOCAL_MW(2) * LOCAL_MW(1)
               ELSE
                  LOCAL_MW(M) = LOCAL_MW(M) *
     +                              foshyd.CAP_FRAC_OWN(NUNITS)/100.
               ENDIF
            ENDDO
!
            IF(LOCAL_MW(1) > 0. .AND. LOCAL_MW(2) > 0.) THEN
               IF(USE_POLY_HEAT_RATES == 'T' .AND.
     +                                         .NOT. TURN_OFF_POLY) THEN
!
! TOM TO ASSOCIATE VARIABLES HERE (AND IN NEW_CL_UNITS_READ).
!
!
                  COEFF(1,NUNITS) = 1000. *
     +               (CONSTANT_POLY +
     +                FIRST_POLY * LOCAL_MW(1) +
     +                SECOND_POLY *
     +                           LOCAL_MW(1)*LOCAL_MW(1) +
     +                THIRD_POLY *
     +                     LOCAL_MW(1)*LOCAL_MW(1)*
     +                                              LOCAL_MW(1))/
     +                                                LOCAL_MW(1)
                  COEFF(2,NUNITS) = 1000. *
     +                (FIRST_POLY +
     +                2. * SECOND_POLY * LOCAL_MW(1) +
     +                3. * THIRD_POLY *
     +                            LOCAL_MW(1)*LOCAL_MW(1))
                  COEFF(3,NUNITS) = 1000. *
     +                (FIRST_POLY +
     +                2. * SECOND_POLY * LOCAL_MW(2) +
     +                3. * THIRD_POLY *
     +                            LOCAL_MW(2)*LOCAL_MW(2))
!

               ENDIF
!
!
! 1/17/03.
! A+BX+CX^2
!
               IF(LOCAL_MW(2) > LOCAL_MW(1)) THEN
!
                  CUBIC_HEAT_CURVE(1,NUNITS) =
     +                     (COEFF(2,NUNITS)-COEFF(3,NUNITS))/
     +                     (-2.*(LOCAL_MW(2)-LOCAL_MW(1)))
                  CUBIC_HEAT_CURVE(2,NUNITS) =
     +                  COEFF(2,NUNITS) -
     +                             (COEFF(2,NUNITS) - COEFF(3,NUNITS)) *
     +                        LOCAL_MW(1)/
     +                     (-1.*(LOCAL_MW(2)-LOCAL_MW(1)))
                  CUBIC_HEAT_CURVE(3,NUNITS) =
     +                ((COEFF(1,NUNITS)-CUBIC_HEAT_CURVE(2,NUNITS))*
     +                               LOCAL_MW(1)) -
     +                        CUBIC_HEAT_CURVE(1,NUNITS)*
     +                             LOCAL_MW(1)*LOCAL_MW(1)
               ELSE
               ENDIF
            ELSE ! NEED TO CHECK THIS WITH TOM.
               CUBIC_HEAT_CURVE(2,NUNITS) = COEFF(1,NUNITS)
            ENDIF

            CUBIC_HEAT_CURVE(0,NUNITS) = CONSTANT_POLY
            CUBIC_HEAT_CURVE(2,NUNITS) = SECOND_POLY
            CUBIC_HEAT_CURVE(3,NUNITS) = THIRD_POLY
            IF(CONSTANT_POLY <= 0. .AND. FIRST_POLY <= 0. .AND.
     +                                           SECOND_POLY <= 0.) THEN
               IF(COEFF(1,NUNITS) > 0.) THEN
                  CUBIC_HEAT_CURVE(1,NUNITS) = COEFF(1,NUNITS)*.001
               ELSE
                  CUBIC_HEAT_CURVE(1,NUNITS) = 10.
               ENDIF
            ELSE
               CUBIC_HEAT_CURVE(1,NUNITS) = FIRST_POLY
            ENDIF

!
! MOVE EXISTING UNIT RETIREMENT DATES IN THE EXTENSION PERIOD TO
! OUTSIDE THE PERIOD
! 08/25/03. MOVED UP FOR BURESH AND A.G. EDWARDS.
!
            IF(OFF_LINE_YEAR(NUNITS) > LAST_STUDY_YEAR)
     +                                      OFF_LINE_YEAR(NUNITS) = 2200
!
            ONLINE(NUNITS) = 100*(ON_LINE_YEAR - 1900) + ON_LINE_MONTH
            OFLINE(NUNITS) = 100*(OFF_LINE_YEAR(NUNITS) - 1900) +
     +                                            OFF_LINE_MONTH(NUNITS)
            FIRST_RETIREMENT_YEAR(NUNITS) =
     +                    100*(FIRST_RETIREMENT_YEAR(NUNITS) - 1900)
!
! 05/13/03. NEW RESOURCE ADDITIONS LOGIC
! 05/16/03. MOVED INSIDE UNITS LOOP TO AVOID ADDITIONS AND REMOVALS.
! 08/25/03. MOVED ABOVE UNITS ACTIVE LOOP FOR BURESH AND A.G. EDWARDS.
!
!
            IF(NEWGEN_UNIT_STATUS(1:2) == '01' ) THEN
               NEWGEN_INDEX(NUNITS) = 1
            ELSEIF(NEWGEN_UNIT_STATUS(1:2) == '02' ) THEN
               NEWGEN_INDEX(NUNITS) = 2
            ELSEIF(NEWGEN_UNIT_STATUS(1:2) == '03' ) THEN
               NEWGEN_INDEX(NUNITS) = 3
            ELSEIF(NEWGEN_UNIT_STATUS(1:2) == '04' ) THEN
               NEWGEN_INDEX(NUNITS) = 4
            ELSEIF(NEWGEN_UNIT_STATUS(1:2) == '05') THEN
               NEWGEN_INDEX(NUNITS) = 5
            ELSEIF(NEWGEN_UNIT_STATUS(1:2) == '06' ) THEN
               NEWGEN_INDEX(NUNITS) = 6
            ELSEIF(NEWGEN_UNIT_STATUS(1:2) == '07' ) THEN
               NEWGEN_INDEX(NUNITS) = 7
            ELSEIF(NEWGEN_UNIT_STATUS(1:2) == '08' ) THEN
               NEWGEN_INDEX(NUNITS) = 8
            ELSEIF(NEWGEN_UNIT_STATUS(1:2) == '09' ) THEN
               NEWGEN_INDEX(NUNITS) = 9
            ELSEIF(NEWGEN_UNIT_STATUS(1:2) == '10' ) THEN
               NEWGEN_INDEX(NUNITS) = 10
            ELSEIF(NEWGEN_UNIT_STATUS(1:2) == '11' ) THEN
               NEWGEN_INDEX(NUNITS) = 11
            ELSEIF(NEWGEN_UNIT_STATUS(1:2) == '12' ) THEN
               NEWGEN_INDEX(NUNITS) = 12
            ELSEIF(NEWGEN_UNIT_STATUS(1:2) == '13' ) THEN
               NEWGEN_INDEX(NUNITS) = 13
            ELSE ! DEFAULT INDEX = 1
               NEWGEN_INDEX(NUNITS) = 1
            ENDIF
            NEWGEN_COUNTER(NEWGEN_INDEX(NUNITS)) =
     +                          NEWGEN_COUNTER(NEWGEN_INDEX(NUNITS)) + 1
!
            IF(NEW_BUILD_ADDITIONS_ACTIVE .AND. ONLINE(NUNITS) >
     +                                        BEGIN_DATE) THEN
! (0.0,1.0) ASSUME UNIFORM DISTRIBUTION
               IRAN32_OUT = iran32()
               RANDOM_NUMBER = FLOAT(IRAN32_OUT)/FLOAT(2147483647)
               KEEP_UNIT = .TRUE.
               IF(    NEWGEN_INDEX(NUNITS) == 2) THEN
                  IF(UNDER_CONSTRUCTION_PERCENT < RANDOM_NUMBER) THEN
                     KEEP_UNIT = .FALSE.
                  ENDIF
               ELSEIF(NEWGEN_INDEX(NUNITS) == 3) THEN
                  IF(ADVANCED_DEVELOPMENT_PERCENT < RANDOM_NUMBER) THEN
                     KEEP_UNIT = .FALSE.
                  ENDIF
               ELSEIF(NEWGEN_INDEX(NUNITS) == 4) THEN
                  IF(EARLY_DEVELOPMENT_PERCENT < RANDOM_NUMBER) THEN
                     KEEP_UNIT = .FALSE.
                  ENDIF
               ELSEIF(NEWGEN_INDEX(NUNITS) == 5) THEN
                  IF(PROPOSED_PERCENT < RANDOM_NUMBER) THEN
                     KEEP_UNIT = .FALSE.
                  ENDIF
               ELSEIF(NEWGEN_INDEX(NUNITS) == 7) THEN
                  IF(INDEFIN_POSTPHONED_PERCENT < RANDOM_NUMBER) THEN
                     KEEP_UNIT = .FALSE.
                  ENDIF
               ELSE
                  IF(UNIT_ACTIVE == 'F') CYCLE
               ENDIF
               IF(.NOT. KEEP_UNIT) THEN
                  CYCLE
               ELSE
                  KEEP_UNIT = .TRUE.
               ENDIF
            ELSE
!
               IF(UNIT_ACTIVE == 'F') CYCLE
!
            ENDIF

            CO2_CONTROL_DATE(NUNITS) = CO2_CONTROL_DATE(NUNITS) + 10000
            IF(HardWiredRetrofitProject(NUNITS)) THEN
               RETIREMENT_CANDIDATE(NUNITS) = 'F'
               FIRST_RETIREMENT_YEAR(NUNITS) = 9999
               RETROFIT_CANDIDATE(NUNITS) = 'F'
            ENDIF
            IF(RETROFIT_CANDIDATE(NUNITS) /= 'F') THEN
               RETROFIT_COUNTER = RETROFIT_COUNTER + 1
               RETROFIT_UNIT_INDEX(RETROFIT_COUNTER) = NUNITS
            ENDIF
            IF(RETIREMENT_CANDIDATE(NUNITS) /= 'F') THEN
               RETIREMENT_COUNTER = RETIREMENT_COUNTER + 1
            ENDIF
!
! 02/03/06.
!
            TEMP_STATE = STATE_PROVINCE(1:2)
            TEMP_I2 = STATE_ID_LOOKUP(TEMP_STATE)
            State_Index(NUNITS) = TEMP_I2
            TEMP_I2 = STATE_ID_LOOKUP(STATE_PROVINCE)
            State_TG_Index(NUNITS) = TEMP_I2
            IF( TEMP_I2 > 0 .AND.
     +                          STATE_PROVINCE_INDEX(TEMP_I2) == 0) THEN
               MAX_STATE_PROVINCE_NO = MAX_STATE_PROVINCE_NO + 1
               STATE_PROVINCE_ADDRESS(MAX_STATE_PROVINCE_NO) = TEMP_I2
               STATE_PROVINCE_INDEX(TEMP_I2) = MAX_STATE_PROVINCE_NO
               STATE_PROVINCE_NAMES(MAX_STATE_PROVINCE_NO) =
     +                                                    STATE_PROVINCE
            ENDIF
            UNIT_STATE_PROVINCE_INDEX(NUNITS) =
     +                                     STATE_PROVINCE_INDEX(TEMP_I2)



            TEMP_I2 = STATE_2_GAS_REGION_LOOKUP(STATE_PROVINCE)
            IF(YES_GSP_IS_ST_TG(TEMP_I2)) THEN
               TEMP_STATE = STATE_PROVINCE
            ELSE
               TEMP_STATE = STATE_PROVINCE(1:2)
            ENDIF
            UNIT_GAS_REGION_INDEX(NUNITS) =
     +                         STATE_2_GAS_REGION_LOOKUP(TEMP_STATE)

! 12/05/01
!
            IF(trim(SPECIAL_UNIT_ID(NUNITS)) /= ' ' .AND.
     +                   trim(SPECIAL_UNIT_ID(NUNITS)) /= 'None') THEN
               IF(trim(SPECIAL_UNIT_ID(NUNITS)) == 'BayShore') THEN
                  FIRST_ENERGY = .TRUE.
               ENDIF
            ENDIF

            NOX_SEASON_DATE(NUNITS) = NOX_SEASON_DATE(NUNITS) + 10000

            NOX_CONTROL_PERCENT(NUNITS) =
     +             MIN(100.,MAX(0.,1.-NOX_CONTROL_PERCENT(NUNITS)/100.))
            NOX_CONTROL_DATE(NUNITS) = NOX_CONTROL_DATE(NUNITS) + 10000
!
            SOX_CONTROL_PERCENT(NUNITS) =
     +             MIN(100.,MAX(0.,1.-SOX_CONTROL_PERCENT(NUNITS)/100.))
            SOX_CONTROL_DATE(NUNITS) = SOX_CONTROL_DATE(NUNITS) + 10000
!
            CO2_CONTROL_PERCENT(NUNITS) =
     +             MIN(100.,MAX(0.,1.-CO2_CONTROL_PERCENT(NUNITS)/100.))
            HG_CONTROL_PERCENT(NUNITS) =
     +             MIN(100.,MAX(0.,1.-HG_CONTROL_PERCENT(NUNITS)/100.))
            HG_CONTROL_DATE(NUNITS) = HG_CONTROL_DATE(NUNITS) + 10000
            OTHER3_CONTROL_PERCENT(NUNITS) =
     +             MIN(100.,
     +                   MAX(0.,1.-OTHER3_CONTROL_PERCENT(NUNITS)/100.))
            OTHER3_CONTROL_DATE(NUNITS) =
     +                               OTHER3_CONTROL_DATE(NUNITS) + 10000

!
            IF((SAVE_IGNORE_NON_UTILITY  == 'T' .AND.
     +                                        UTILITY_OWNED == 'N') .OR.
     +         (SAVE_IGNORE_NON_UTILITY  == 'K' .AND.
     +                  UTILITY_OWNED == 'N' .AND.
     +                              ON_LINE_YEAR-BASE_YEAR < 1)  ) CYCLE
!
! THIS ESTABLISHES A UNIQUE UNIT ID VALUE FOR EACH RUN
!
            CL_UNIT_UNIQUE_RPT_ID(NUNITS) = UNIQUE_ID_NUM

            CALL UPC(TEMP_PRIM_FUEL_TYPE_STR,PRIM_FUEL_TYPE_STR)
            PRIMARY_MOVER(NUNITS) =
     +                        FUEL_TYPE_2_PRIM_MOVER(PRIM_FUEL_TYPE_STR)
!
            CALL SET_ASSET_CLASSES(ASSET_CLASS_NUM(NUNITS),
     +                             NUMBER_OF_CAP_LIMITED_CLASSES,
     +                               MAX_CAP_LIMITED_CLASS_ID_NUM,
     +                             CAP_LIMITED_CLASS_POINTER)
!
            IF(TRANS_ID > 0 .AND. ON_LINE_YEAR <= LAST_STUDY_YEAR) THEN
               DAY_ID = DAY_TYPE_ID(NUNITS)
               MAX_TRANS_ID_USED = MAX(MAX_TRANS_ID_USED, TRANS_ID)
               IF(DAY_ID > 0 .AND. DAY_ID <= MAX_DAY_TYPES ) THEN
                  IF(DAY_TYPE_TRANS_GROUP_PAIR(DAY_ID,TRANS_ID)==0) THEN
                     MAX_TRANS_DATA_BASES = MAX_TRANS_DATA_BASES + 1
                     DAY_TYPE_TRANS_GROUP_PAIR(DAY_ID,TRANS_ID) =
     +                                              MAX_TRANS_DATA_BASES
                  ENDIF
!
                  DATA_BASE_POSITION(1,NUNITS) =
     +                        DAY_TYPE_TRANS_GROUP_PAIR(DAY_ID,TRANS_ID)
                  DATA_BASE_POSITION(2,NUNITS) =
     +                        DAY_TYPE_TRANS_GROUP_PAIR(DAY_ID,0)
               ELSEIF(DAY_ID < 0) THEN
                  DAY_TYPE_COUNTER = 1
                  DO
            DAY_ID=GET_VAR(DAY_TYPE_ID(NUNITS),
     +               DAY_TYPE_COUNTER,"FIND DAY TYPE")

                   IF(DAY_ID<= 0 .or. DAY_ID>MAX_DAY_TYPES) then
                    EXIT
                   endif
!      ! This IF statement not in "original" source.
       IF(int(DAY_TYPE_TRANS_GROUP_PAIR(DAY_ID,TRANS_ID))
     +  .eq. 0) THEN
                        MAX_TRANS_DATA_BASES = MAX_TRANS_DATA_BASES + 1
                        DAY_TYPE_TRANS_GROUP_PAIR(DAY_ID,TRANS_ID) =
     +                                              MAX_TRANS_DATA_BASES
                     ENDIF
!     !
                     DATA_BASE_POSITION(2*DAY_TYPE_COUNTER-1,NUNITS) =
     +                        DAY_TYPE_TRANS_GROUP_PAIR(DAY_ID,TRANS_ID)
                     DATA_BASE_POSITION(2*DAY_TYPE_COUNTER,NUNITS) =
     +                               DAY_TYPE_TRANS_GROUP_PAIR(DAY_ID,0)
                     DAY_TYPE_COUNTER = DAY_TYPE_COUNTER + 1
                  ENDDO
               ELSEIF(DAY_ID == 0) THEN
                  DO M = 1, MAX_DAY_TYPES
                     IF(DAY_TYPE_TRANS_GROUP_PAIR(M,TRANS_ID) == 0) THEN
                        MAX_TRANS_DATA_BASES = MAX_TRANS_DATA_BASES + 1
                        DAY_TYPE_TRANS_GROUP_PAIR(M,TRANS_ID) =
     +                                              MAX_TRANS_DATA_BASES
                     ENDIF
                     DATA_BASE_POSITION(2*M-1,NUNITS) =
     +                             DAY_TYPE_TRANS_GROUP_PAIR(M,TRANS_ID)
                     DATA_BASE_POSITION(2*M,NUNITS) =
     +                                    DAY_TYPE_TRANS_GROUP_PAIR(M,0)
                  ENDDO
               ELSE
                  WRITE(4,*) "DAY TYPE DESIGNATION",DAY_ID
                  WRITE(4,*) "OUTSIDE THE RANGE OF POSSIBLE OUTCOMES"
                  WRITE(4,*)"MAXIMUM DAY TYPES DEFINED = ",MAX_DAY_TYPES
                  WRITE(4,*) '*** line 2056 CLA_OBJT.FOR ***'
                  er_message='See WARNING MESSAGES-cla_objt-7'
                  call end_program(er_message)
               ENDIF
            ENDIF
!
            IF(MAINT_DAYS(NUNITS) /= 0.)
     +                                SAVE_DETAILED_MAINTENANCE = .TRUE.
            DISP_ADDER_ESCR(NUNITS) = PFESCR(NUNITS)

            LOCAL_RESOURCE_ID = CL_RESOURCE_ID(NUNITS)
            IF(NUMBER_OF_RESOURCE_ID(LOCAL_RESOURCE_ID) == 0) THEN
               RESOURCE_ID_TO_UNIT(LOCAL_RESOURCE_ID) = NUNITS ! FIRST OCCURANCE
            ENDIF
            NUMBER_OF_RESOURCE_ID(LOCAL_RESOURCE_ID) =
     +                      NUMBER_OF_RESOURCE_ID(LOCAL_RESOURCE_ID) + 1
!
            IF(INPUT_MW(1,NUNITS) == 0.)
     +                           INPUT_MW(1,NUNITS) = INPUT_MW(2,NUNITS)
            ECONOMY_TRANS_TYPE(NUNITS) = 'T'
            LDTYPE(NUNITS) = CL_LOAD_TYPE(1:1)
            PRIM_FUEL_TYPE(NUNITS) = PRIM_FUEL_TYPE_STR(1:1)
            NUC_FUEL_PRICES_FROM_FUEL_FILE(NUNITS) =
     +                      INDEX(PRIM_FUEL_TYPE_STR,'Nuc-NF') /= 0 .OR.
     +                           INDEX(PRIM_FUEL_TYPE_STR,'NUC-NF') /= 0
            IF(INDEX('123456',CL_LOAD_TYPE(2:2)) /= 0) THEN
               CL_CAP_AREA_LINKED(NUNITS) = .TRUE.
               MONTHLY_CAPACITY_POINTER(NUNITS) =
     +                                -INDEX('123456',CL_LOAD_TYPE(2:2))
               ECONOMY_TRANS_TYPE(NUNITS) = 'N'
            ELSE
               CL_CAP_AREA_LINKED(NUNITS) = .FALSE.
               IF(CL_LOAD_TYPE(2:2)=='S') ECONOMY_TRANS_TYPE(NUNITS)='S'
               IF(CL_LOAD_TYPE(2:2)=='B') ECONOMY_TRANS_TYPE(NUNITS)='B'
               IF(CL_LOAD_TYPE(2:2)=='N') ECONOMY_TRANS_TYPE(NUNITS)='N'
            ENDIF
            ON_LINE_YEAR = MIN(MAX(ON_LINE_YEAR,1901),2200)
            OFF_LINE_YEAR(NUNITS) = MIN(OFF_LINE_YEAR(NUNITS),2200)
!

            IF(EFORS0) EFOR(NUNITS) = 0.
! READ PLANNED MAINTENANCE RATES
            IF(SORS0) THEN
               MAINT_DAYS(NUNITS) = 0
               DO M = 1, 12
                  MNRATE(NUNITS,M) = 0.
               ENDDO
            ENDIF
!
! CHECK IF LAST ENERGY SOURCE
!
            IF(LDTYPE(NUNITS) == 'L') THEN
               INPUT_MW(1,NUNITS) = 0.
               INPUT_MW(2,NUNITS) = 0.
               foshyd.CAP_PLANNING_FAC(NUNITS) = 0.
               EFOR(NUNITS) = 0.
               FUEL_SUPPLY_ID(NUNITS) = 0
               DO M = 1 , 12
                  IF(MNRATE(NUNITS,M) /= 100.) MNRATE(NUNITS,M) = 0.
               ENDDO
            ENDIF
!
! 9/15/95. GAT. FOR DUKE POWER.
!
            IF(LDTYPE(NUNITS) == 'N'.AND.EXPENSE_ASSIGNMENT(NUNITS)/='O'
     +                     .AND. EXPENSE_ASSIGNMENT(NUNITS) /= 'L') THEN
               EXPENSE_ASSIGNMENT(NUNITS) = 'O'
               WRITE(4,*) "Generating unit",UNITNM(NUNITS),
     +                   "was assigned"//
     +                   " as nuclear without either an owned or leased"
               WRITE(4,*) "expense designation. This unit was "//
     +                               "reassigned as owned nuclear fuel."
               WRITE(4,*) " "
            ENDIF
            IF(LDTYPE(NUNITS) == 'N') THEN
               DO M = 1, 12
                  NUC_MAINTENANCE(M) = MNRATE(NUNITS,M)
               ENDDO
               CALL CAL_GENERATING_INFORMATION(CL_RESOURCE_ID(NUNITS),
     +                              foshyd.CAP_FRAC_OWN(NUNITS),
     +                              INPUT_MW(2,NUNITS),
     +                              MONTHLY_CAPACITY_POINTER(NUNITS),
     +                              EFOR(NUNITS),
     +                              NUC_MAINTENANCE,
     +                              ONLINE(NUNITS),
     +                              OFLINE(NUNITS),
     +                              UNITNM(NUNITS),
     +                              PRIM_FUEL_TYPE_STR)
               IF(NUC_FUEL_PRICES_FROM_FUEL_FILE(NUNITS)) THEN
                  PBTUCT(NUNITS) = 0.
                  SBTUCT(NUNITS) = 0.
                  EMISS_FUEL_COST(NUNITS) = 0.
               ENDIF
            ENDIF
!
! NEED TO SAVE PRICE INFORMATION
!
            PBTUCT_SAVE(NUNITS) = PBTUCT(NUNITS)
            SBTUCT_SAVE(NUNITS) = SBTUCT(NUNITS)
            FUELADJ_SAVE(NUNITS) = FUELADJ(NUNITS)
            VCPMWH_IN_SAVE(NUNITS) = VCPMWH_IN(NUNITS)
            FIXED_COST_SAVE(NUNITS) = FIXED_COST_IN(NUNITS)
            ANNUAL_CL_FIXED_COST_SAVE(NUNITS) =
     +                                      ANNUAL_CL_FIXED_COST(NUNITS)
            DISPADJ_SAVE(NUNITS) = DISPADJ(NUNITS)
            DISPADJ2_SAVE(NUNITS) = DISPADJ2(NUNITS)
            DISPATCH_MULT_SAVE(NUNITS) = DISPATCH_MULT(NUNITS)
            EXCESS_ENERGY_SALES_SAVE(NUNITS) =
     +                                       EXCESS_ENERGY_SALES(NUNITS)
            CL_AI_CAPACITY_RATE_SAVE(NUNITS) =
     +                                       CL_AI_CAPACITY_RATE(NUNITS)
            CL_AI_ENERGY_RATE_SAVE(NUNITS) = CL_AI_ENERGY_RATE(NUNITS)
            CL_AI_REMAINING_LIFE_SAVE(NUNITS) =
     +                                      AI_CL_REMAINING_LIFE(NUNITS)
            DISPADJ2_SAVE(NUNITS) = DISPADJ2(NUNITS)
            EMISS_FUEL_COST_SAVE(NUNITS) = EMISS_FUEL_COST(NUNITS)
            IF(ON_LINE_YEAR <= OFF_LINE_YEAR(NUNITS) .AND.
     +         ON_LINE_YEAR-BASE_YEAR <= STUDY_PERIOD .AND.
     +                          OFF_LINE_YEAR(NUNITS) > BASE_YEAR) THEN
               VOID_REAL =
     +                CL_CAPACITY_PLANNING_ADJ(ON_LINE_YEAR,
     +                                                 NUNITS,1,.FALSE.) ! REMOVED 11/02/06


               SHADOW_UNIT_NUMBER(NUNITS) = 0
               IF(FUEL_SUPPLY_ID(NUNITS)>0 .AND.
     +                       FUEL_MIX_PTR(NUNITS) > 0. .AND.
     +                               START_UP_LOGIC(NUNITS) == 'F') THEN ! FUELMX(NUNITS) >0.) THEN
                  I = NUNITS
                  NUNITS = I + 1
                  SHADOW_UNIT_NUMBER(NUNITS) = I
                  SHADOW_UNIT_NUMBER(I) = NUNITS
                  UNITNM(NUNITS) = '+'//UNITNM(I)
                  CL_UNIT_UNIQUE_RPT_ID(NUNITS) = UNIQUE_ID_NUM
                  FUEL_MIX_PTR(NUNITS) = 0. ! FUELMX(NUNITS) = 0 REPLACE 5/17/01
                  LDTYPE(NUNITS) = 'S'
                  EXPENSE_ASSIGNMENT(NUNITS) = EXPENSE_ASSIGNMENT(I)
                  EXPENSE_COLLECTION(NUNITS) = EXPENSE_COLLECTION(I)
                  ASSET_CLASS_NUM(NUNITS) = ASSET_CLASS_NUM(I)
                  ASSET_CLASS_VECTOR(NUNITS) = ASSET_CLASS_VECTOR(I)
                  INTRA_COMPANY_CLASS_ID(NUNITS) =
     +                                         INTRA_COMPANY_CLASS_ID(I)
                  INTRA_COMPANY_TRANSACTION(NUNITS) =
     +                                      INTRA_COMPANY_TRANSACTION(I)
                  SPECIAL_UNIT_ID(NUNITS) = SPECIAL_UNIT_ID(I)
                  MINIMUM_CAPACITY_FACTOR(NUNITS) =
     +                                        MINIMUM_CAPACITY_FACTOR(I)
                  MAXIMUM_CAPACITY_FACTOR(NUNITS) =
     +                                        MAXIMUM_CAPACITY_FACTOR(I)
                  TRANSACTION_GROUP_ID(NUNITS) = TRANSACTION_GROUP_ID(I)
                  REPORT_THIS_UNIT(NUNITS) = REPORT_THIS_UNIT(I)
                  DAY_TYPE_ID(NUNITS) = DAY_TYPE_ID(I)
                  ECONOMY_TRANS_TYPE(NUNITS) = ECONOMY_TRANS_TYPE(I)
                  CL_CAP_AREA_LINKED(NUNITS) = CL_CAP_AREA_LINKED(I)
                  GENGRP(NUNITS) = GENGRP(I)
                  foshyd.CAP_FRAC_OWN(NUNITS) = foshyd.CAP_FRAC_OWN(I)
                  ONLINE(NUNITS) = ONLINE(I)
                  OFLINE(NUNITS) = OFLINE(I)
                  SBTUCT(NUNITS) = SBTUCT(I)
                  SBTUCT(I) = 0.
                  SFESCR(NUNITS) = SFESCR(I)
                  SFESCR(I) = 0
                  FUELADJ(NUNITS) = FUELADJ(I)
                  EFOR(NUNITS) = EFOR(I)
                  MAINT_DAYS(NUNITS) = MAINT_DAYS(I)
                  DO M = 1, 12
                     MNRATE(NUNITS,M) = MNRATE(I,M)
                  ENDDO
                  VCPMWH_IN(NUNITS) = VCPMWH_IN(I)
                  OMESCR(NUNITS) = OMESCR(I)
                  FIXED_COST_IN(NUNITS) = 0.
                  ANNUAL_CL_FIXED_COST(NUNITS) = 0.
                  FIXED_COST_ESCALATOR(NUNITS) = 0.
                  ANNUAL_CL_FIXED_COST_ESC(NUNITS) = 0.
                  DISPADJ(NUNITS) = DISPADJ(I)
                  DISPATCH_MULT(NUNITS) = DISPATCH_MULT(I)
                  EXCESS_ENERGY_SALES(NUNITS) = EXCESS_ENERGY_SALES(I)
                  HR_FACTOR(NUNITS) = HR_FACTOR(I)
                  INPUT_MW(1,NUNITS) = INPUT_MW(1,I)
                  INPUT_MW(2,NUNITS) = INPUT_MW(2,I)
                  COEFF(1,NUNITS) = COEFF(1,I)
                  COEFF(2,NUNITS) = COEFF(2,I)
                  COEFF(3,NUNITS) = COEFF(3,I)
                  CL_AI_CAPACITY_RATE(NUNITS) = CL_AI_CAPACITY_RATE(I)
                  CL_AI_CAPACITY_ESCALATOR(NUNITS) =
     +                                       CL_AI_CAPACITY_ESCALATOR(I)
                  CL_AI_ENERGY_RATE(NUNITS) = CL_AI_ENERGY_RATE(I)
                  CL_AI_ENERGY_ESCALATOR(NUNITS) =
     +                                         CL_AI_ENERGY_ESCALATOR(I)
                  AI_CL_REMAINING_LIFE(NUNITS) = AI_CL_REMAINING_LIFE(I)
                  AI_CL_TAX_LIFE(NUNITS) = AI_CL_TAX_LIFE(I)
                  AI_CL_ADR_LIFE(NUNITS) = AI_CL_ADR_LIFE(I)
                  SEC_FUEL_EMISS_PTR(NUNITS) = SEC_FUEL_EMISS_PTR(I)
                  SEC_FUEL_EMISS_PTR(I) = 0
                  CL_RESOURCE_ID(NUNITS) = CL_RESOURCE_ID(I)
                  DISPADJ2(NUNITS) = DISPADJ2(I)
                  PHASE_I_UNIT(NUNITS) = PHASE_I_UNIT(I)
                  LOCAL_CL_POOL_FRAC_OWN(NUNITS) =
     +                                         LOCAL_CL_POOL_FRAC_OWN(I)
!
! SINCE ONLY THE SECONDARY FUEL IS USED ZERO PRIMARLY AND EMISSIONS FUEL
! DATA
!
                  FUEL_SUPPLY_ID(NUNITS) = FUEL_SUPPLY_ID(I)
                  foshyd.CAP_PLANNING_FAC(NUNITS) = 0.
                  PBTUCT(NUNITS) = PBTUCT(I)
                  PFESCR(NUNITS) = PFESCR(I)
                  DISP_ADDER_ESCR(NUNITS) = DISP_ADDER_ESCR(I)
                  P_SO2(NUNITS) = P_SO2(I)
                  P_NOX(NUNITS) = P_NOX(I)
                  P_PARTICULATES(NUNITS) = P_PARTICULATES(I)
                  P_EMIS_OTH2(NUNITS) = P_EMIS_OTH2(I)
                  P_EMIS_OTH3(NUNITS) = P_EMIS_OTH3(I)
                  P_NOX_BK2(NUNITS) = P_NOX_BK2(I)
                  EMISS_FUEL_COST(NUNITS) = EMISS_FUEL_COST(I)
                  EMISS_FUEL_ESCAL(NUNITS) = EMISS_FUEL_ESCAL(I)
                  EMISS_FUEL_EMISS_PTR(NUNITS) = 0.
                  EMISS_BLENDING_RATE(NUNITS) = EMISS_BLENDING_RATE(I)
                  PRIM_FUEL_EMISS_PTR(NUNITS) = PRIM_FUEL_EMISS_PTR(I)
! 6/10/93. GAT. NO ALLOCATION OF FUEL TYPES BETWEEN SHADOWS
                  PRIM_FUEL_TYPE(NUNITS) = " "
                  NUC_FUEL_PRICES_FROM_FUEL_FILE(NUNITS) = .FALSE.
                  SEC_FUEL_TYPE(NUNITS) = SEC_FUEL_TYPE(I)
                  SEC_FUEL_TYPE(I) = " "
                  EMISS_FUEL_TYPE(NUNITS) = " "
                  TIE_CONSTRAINT_GROUP(NUNITS) = TIE_CONSTRAINT_GROUP(I)
                  PBTUCT_SAVE(NUNITS) = PBTUCT(I)
!
                  SBTUCT_SAVE(NUNITS) = SBTUCT(NUNITS)
                  FUELADJ_SAVE(NUNITS) = FUELADJ(I)
                  VCPMWH_IN_SAVE(NUNITS) = VCPMWH_IN(I)
                  FIXED_COST_SAVE(NUNITS) = FIXED_COST_IN(I)
                  ANNUAL_CL_FIXED_COST_SAVE(NUNITS) =
     +                                           ANNUAL_CL_FIXED_COST(I)
                  DISPATCH_MULT_SAVE(NUNITS) = DISPATCH_MULT(I)
                  EXCESS_ENERGY_SALES_SAVE(NUNITS) =
     +                                            EXCESS_ENERGY_SALES(I)
                  MONTHLY_CAPACITY_POINTER(NUNITS) =
     +                                       MONTHLY_CAPACITY_POINTER(I)
                  DISPADJ_SAVE(NUNITS) = DISPADJ(I)
                  DISPADJ2_SAVE(NUNITS) = DISPADJ2(I)
                  CL_AI_CAPACITY_RATE_SAVE(NUNITS) =
     +                                            CL_AI_CAPACITY_RATE(I)
                  CL_AI_ENERGY_RATE_SAVE(NUNITS) = CL_AI_ENERGY_RATE(I)
                  CL_AI_REMAINING_LIFE_SAVE(NUNITS) =
     +                                           AI_CL_REMAINING_LIFE(I)
                  DISPADJ2_SAVE(NUNITS) = DISPADJ2(I)
                  EMISS_FUEL_COST_SAVE(NUNITS) = EMISS_FUEL_COST(I)
!
                  TRANS_ID = TRANSACTION_GROUP_ID(NUNITS)
!
                  IF(TRANS_ID > 0 .AND.
     +                             ON_LINE_YEAR <= LAST_STUDY_YEAR) THEN
                     DAY_ID = DAY_TYPE_ID(NUNITS)
                     MAX_TRANS_ID_USED = MAX(MAX_TRANS_ID_USED,TRANS_ID)
                     IF(DAY_ID > 0 .AND. DAY_ID <= MAX_DAY_TYPES ) THEN
                        IF(DAY_TYPE_TRANS_GROUP_PAIR(DAY_ID,TRANS_ID)
     +                                                        == 0) THEN
                           MAX_TRANS_DATA_BASES = MAX_TRANS_DATA_BASES+1
                           DAY_TYPE_TRANS_GROUP_PAIR(DAY_ID,TRANS_ID) =
     +                                              MAX_TRANS_DATA_BASES
                        ENDIF
!
                        DATA_BASE_POSITION(1,NUNITS) =
     +                        DAY_TYPE_TRANS_GROUP_PAIR(DAY_ID,TRANS_ID)
                        DATA_BASE_POSITION(2,NUNITS) =
     +                               DAY_TYPE_TRANS_GROUP_PAIR(DAY_ID,0)
                     ELSEIF(DAY_ID < 0) THEN
                        DAY_TYPE_COUNTER = 1
                        DO
                           DAY_ID =
     +                           INT(GET_VAR(FLOAT(DAY_TYPE_ID(NUNITS)),
     +                                DAY_TYPE_COUNTER,"FIND DAY TYPE"))
                           IF(DAY_ID <= 0 .OR.
     +                                      DAY_ID > MAX_DAY_TYPES) EXIT
                           IF(DAY_TYPE_TRANS_GROUP_PAIR(DAY_ID,TRANS_ID)
     +                                                        == 0) THEN
                              MAX_TRANS_DATA_BASES =
     +                                          MAX_TRANS_DATA_BASES + 1
                              DAY_TYPE_TRANS_GROUP_PAIR(DAY_ID,
     +                                  TRANS_ID) = MAX_TRANS_DATA_BASES
                           ENDIF
!     !
                           DATA_BASE_POSITION(
     +                                    2*DAY_TYPE_COUNTER-1,NUNITS) =
     +                        DAY_TYPE_TRANS_GROUP_PAIR(DAY_ID,TRANS_ID)
                           DATA_BASE_POSITION(2*DAY_TYPE_COUNTER,
     +                       NUNITS)=DAY_TYPE_TRANS_GROUP_PAIR(DAY_ID,0)
                           DAY_TYPE_COUNTER = DAY_TYPE_COUNTER + 1
                        ENDDO
                     ELSEIF(DAY_ID == 0) THEN
                        DO M = 1, MAX_DAY_TYPES
                           IF(DAY_TYPE_TRANS_GROUP_PAIR(M,TRANS_ID)
     +                                                        == 0) THEN
                              MAX_TRANS_DATA_BASES =
     +                                          MAX_TRANS_DATA_BASES + 1
                              DAY_TYPE_TRANS_GROUP_PAIR(M,TRANS_ID) =
     +                                              MAX_TRANS_DATA_BASES
                           ENDIF
                           DATA_BASE_POSITION(2*M-1,NUNITS) =
     +                             DAY_TYPE_TRANS_GROUP_PAIR(M,TRANS_ID)
                           DATA_BASE_POSITION(2*M,NUNITS) =
     +                                    DAY_TYPE_TRANS_GROUP_PAIR(M,0)
                        ENDDO
                     ELSE
                        WRITE(4,*) "DAY TYPE DESIGNATION",DAY_ID
                        WRITE(4,*)
     +                          "OUTSIDE THE RANGE OF POSSIBLE OUTCOMES"
                        WRITE(4,*)
     +                      "MAXIMUM DAY TYPES DEFINED = ",MAX_DAY_TYPES
                        WRITE(4,*) '*** line 2391 CLA_OBJT.FOR ***'
                        er_message='See WARNING MESSAGES-cla_objt-6'
                        call end_program(er_message)
                     ENDIF
                  ENDIF
!
               ENDIF ! SHADOW UNIT
!
               IF(START_UP_LOGIC(NUNITS) /= 'F') THEN
                  TOTAL_START_UP_UNITS = TOTAL_START_UP_UNITS + 1
                  START_UP_INDEX(NUNITS) = TOTAL_START_UP_UNITS
               ENDIF
!
               NUNITS = NUNITS + 1
            ENDIF ! CONTRIBUTES TO CAPACITY PLANNING
            IF(NUNITS > MAX_CL_UNITS) THEN

               WRITE(SCREEN_MESSAGES,'(A,I5,A)')
     +                                        'More than ',MAX_CL_UNITS,
     +                                        ' capacity-limited'
               CALL MG_LOCATE_WRITE(20,0,trim(SCREEN_MESSAGES),
     +                                                   ALL_VERSIONS,1)
               CALL MG_LOCATE_WRITE(21,0,
     +                              'units are in the capacity-limited',
     +                                                   ALL_VERSIONS,1)
               CALL MG_LOCATE_WRITE(22,0,
     +                               'file after expansion planning.',
     +                                                   ALL_VERSIONS,1)
               er_message='stop requested from Cla_objt SIID24'
               call end_program(er_message)
            ENDIF
         ENDDO ! PROCESSING WITHIN A FILE
         CLOSE(10,IOSTAT=IOS)
         IF(RDI_CL_RECORDS > 0 .AND. PROCESSING_FIRST_DATA_FILE) THEN
            CALL CLOSE_BOTH_RDI_FILES
         ENDIF
      ENDDO ! CL FILES LOOP
      NUNITS = NUNITS - 1
      BASE_CL_UNITS = NUNITS
      AVAILABLE_CL_UNITS = NUNITS
      BASE_PLUS_HARDWIRED_CL_UNITS = NUNITS
      CL_UNITS_READ = NUNITS
! TO GENERATE MONTHLY CAPACITIES
      IF(NUNITS > 0) THEN
         SAVE_DETAILED_MAINTENANCE = .TRUE.
      ENDIF
      RETIRE_RETRO_COUNTER = RETROFIT_COUNTER + RETIREMENT_COUNTER
      IF(ALLOCATED(RETIRE_RETRO_CO2_PRICE))
     +      DEALLOCATE(RETIRE_RETRO_CO2_PRICE,
     +                 RETIRE_RETRO_UPLIFT_PER_TON,
     +                 RETIRE_RETRO_CUM_CO2,
     +                 NEW_UNIT_RETIRE_VALUE_BY_CM,
     +                 RETIRE_RETRO_INDEX,
     +                 RETIRE_OR_RETRO,
     +                 RETIRE_RETRO_POSITION,
     +                 RETIRE_RETRO_ABATE_INDEX)
      ALLOCATE(RETIRE_RETRO_CO2_PRICE(RETIRE_RETRO_COUNTER),
     +         RETIRE_RETRO_UPLIFT_PER_TON(RETIRE_RETRO_COUNTER),
     +         RETIRE_RETRO_CUM_CO2(0:RETIRE_RETRO_COUNTER),
     +         NEW_UNIT_RETIRE_VALUE_BY_CM(UPPER_TRANS_GROUP),
     +         RETIRE_RETRO_INDEX(RETIRE_RETRO_COUNTER),
     +         RETIRE_OR_RETRO(RETIRE_RETRO_COUNTER),
     +         RETIRE_RETRO_POSITION(RETIRE_RETRO_COUNTER),
     +         RETIRE_RETRO_ABATE_INDEX(RETIRE_RETRO_COUNTER))
!
      RETURN

!***********************************************************************
      ENTRY INIT_CLA_UNIT_UP_YESTERDAY()
!***********************************************************************
!
! CALLED FROM PRO_COST, ONCE PER ENDPOINT
!
         CLA_UNIT_UP_YESTERDAY = .TRUE. ! MAX_CL_UNITS
!
         INIT_CLA_UNIT_UP_YESTERDAY = .TRUE.
!
      RETURN
!***********************************************************************
      ENTRY GET_CLA_UNIT_UP_YESTERDAY(R_NUNITS)
!***********************************************************************
!
! CALLED FROM TF_OBJT2, ONCE PER MONTH/TG/UNIT
!
         GET_CLA_UNIT_UP_YESTERDAY = CLA_UNIT_UP_YESTERDAY(R_NUNITS)
!
      RETURN
!***********************************************************************
      ENTRY PUT_CLA_UNIT_UP_YESTERDAY(R_NUNITS,R_LOGICAL)
!***********************************************************************
!
! CALLED FROM TF_OBJT2, ONCE PER MONTH/TG/UNIT
!
         PUT_CLA_UNIT_UP_YESTERDAY = R_LOGICAL
         CLA_UNIT_UP_YESTERDAY(R_NUNITS) = R_LOGICAL
!
      RETURN
!***********************************************************************
      ENTRY GET_STATE_PROVINCE_NAMES(R_STATE_PROVINCE,R_I)
!***********************************************************************
         R_STATE_PROVINCE = STATE_PROVINCE_NAMES(R_I)
         GET_STATE_PROVINCE_NAMES = 1
      RETURN
!***********************************************************************
      ENTRY GET_UNIT_STATE_PROVINCE_INDEX(R_NUNITS)
!***********************************************************************
         GET_UNIT_STATE_PROVINCE_INDEX =
     +                               UNIT_STATE_PROVINCE_INDEX(R_NUNITS)
      RETURN
!***********************************************************************
      ENTRY GET_UNIT_GAS_REGION_INDEX(R_NUNITS)
!***********************************************************************
         GET_UNIT_GAS_REGION_INDEX = UNIT_GAS_REGION_INDEX(R_NUNITS)
      RETURN
!***********************************************************************
      ENTRY GET_MAX_STATE_PROVINCE_NO
!***********************************************************************
         GET_MAX_STATE_PROVINCE_NO = MAX_STATE_PROVINCE_NO
      RETURN
!***********************************************************************
      ENTRY GET_MIN_SPIN_CAP(R_NUNITS)
!***********************************************************************
         GET_MIN_SPIN_CAP = MIN_SPIN_CAP(R_NUNITS)
      RETURN
!***********************************************************************
      ENTRY GET_MAX_SPIN_CAP(R_NUNITS)
!***********************************************************************
         GET_MAX_SPIN_CAP = MAX_SPIN_CAP(R_NUNITS)
      RETURN
!***********************************************************************
      ENTRY YES_ALLOW_DECOMMIT_BY_UNIT(R_NUNITS,R_MONTH)
!***********************************************************************
         IF(ALLOW_DECOMMIT(R_NUNITS) == 'T') THEN
            YES_ALLOW_DECOMMIT_BY_UNIT = .TRUE.
         ELSEIF(ALLOW_DECOMMIT(R_NUNITS) == 'M') THEN
            TEMP_R4 = GET_VAR(
     +                 FLOAT(MONTHLY_DECOMMIT_VECTOR(R_NUNITS)),R_MONTH,
     +                                                UNITNM(R_NUNITS))
            IF(TEMP_R4 > 0.) THEN
               YES_ALLOW_DECOMMIT_BY_UNIT = .TRUE.
            ELSE
               YES_ALLOW_DECOMMIT_BY_UNIT = .FALSE.
            ENDIF
         ELSE
            YES_ALLOW_DECOMMIT_BY_UNIT = .FALSE.
         ENDIF
      RETURN
!***********************************************************************
      ENTRY EXPENSE_ASSIGNMENT_IS_PURCHASE(R_NUNITS)
!***********************************************************************
         EXPENSE_ASSIGNMENT_IS_PURCHASE =
     +                               EXPENSE_ASSIGNMENT(R_NUNITS) == 'P'
      RETURN
!***********************************************************************
      ENTRY GET_THERMAL_TRACKER_INDEX(R_NUNITS)
!***********************************************************************
!
         GET_THERMAL_TRACKER_INDEX =
     +                   WVPA_TRACKING_TYPE(WVPA_RATE_TRACKER(R_NUNITS))
!
      RETURN
!***********************************************************************
      ENTRY GET_THERMAL_RES_TRACKER_INDEX(R_NUNITS)
!***********************************************************************
!
         GET_THERMAL_RES_TRACKER_INDEX =
     +           WVPA_RESOURCE_TRACKING_TYPE(WVPA_RES_TRACKER(R_NUNITS))
!
      RETURN
!***********************************************************************
      ENTRY GET_THERMAL_FUEL_TRACKER_INDEX(R_NUNITS)
!***********************************************************************
!
         GET_THERMAL_FUEL_TRACKER_INDEX =
     +              WVPA_FUEL_TRACKING_TYPE(WVPA_FUEL_TRACKER(R_NUNITS))
!
      RETURN
!***********************************************************************
      ENTRY GET_THERMAL_MEM_TRACKER_INDEX(R_NUNITS)
!***********************************************************************
!
         GET_THERMAL_MEM_TRACKER_INDEX =
     +              WVPA_MEM_TRACKING_TYPE(WVPA_MEM_TRACKER(R_NUNITS))
!
      RETURN
!***********************************************************************
      ENTRY GET_EMISSION_MARKET_LINK(R_NUNITS)
!***********************************************************************
         GET_EMISSION_MARKET_LINK = EMISSION_MARKET_LINK(R_NUNITS)
      RETURN
!***********************************************************************
      ENTRY TEST_MONTHLY_MUST_RUN(R_NUNITS,R_MONTH)
!***********************************************************************
         TEST_MONTHLY_MUST_RUN = .FALSE.
         IF(MONTHLY_MUST_RUN_VECTOR(R_NUNITS) /= 0) THEN
! FIND THE BLKNO
            DO I = 1, NBLOK
               IF(UNIT(I) /= R_NUNITS) CYCLE
               IF(BLKNO(I) > 1) CYCLE ! THIS SHOULD HAPPEN.
!
               TEMP_R4 = GET_VAR(
     +                 FLOAT(MONTHLY_MUST_RUN_VECTOR(R_NUNITS)),R_MONTH,
     +                                                UNITNM(R_NUNITS))
               IF(TEMP_R4 > 0.) THEN
                  BLKNO(I) = 0
                  TEST_MONTHLY_MUST_RUN = .TRUE.
               ELSE
                  BLKNO(I) = 1
               ENDIF
               EXIT
!
            ENDDO
         ELSE

         ENDIF
      RETURN
!***********************************************************************
      ENTRY GET_CUBIC_HEAT_CURVE(R_NUNITS,R_A_CO,R_B_CO,R_C_CO,
     +  R_D_CO)
!***********************************************************************
         GET_CUBIC_HEAT_CURVE = .TRUE.
         R_A_CO = CUBIC_HEAT_CURVE(0,R_NUNITS)
         R_B_CO = CUBIC_HEAT_CURVE(1,R_NUNITS)
         R_C_CO = CUBIC_HEAT_CURVE(2,R_NUNITS)
         R_D_CO = CUBIC_HEAT_CURVE(3,R_NUNITS)
      RETURN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ENTRY GET_PW_UNIT_TEXT_FIELDS(R_NUNITS,
     +                              R_EIA_PLANT_CODE)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         GET_PW_UNIT_TEXT_FIELDS = .TRUE.
         R_EIA_PLANT_CODE = BASECASE_PLANT_ID(R_NUNITS)

       RETURN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ENTRY GET_EMERGENCY_CAPACITY(R_NUNITS,R_EMERGENCY_HEATRATE)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         GET_EMERGENCY_CAPACITY = EMERGENCY_CAPACITY(R_NUNITS)
         R_EMERGENCY_HEATRATE = EMERGENCY_HEATRATE(R_NUNITS)
      RETURN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ENTRY NOX_ACTIVE_FOR_UNIT(R_NUNITS,R_DATE,
     +                          R_SEASON_IS_NOX_SEASON,
     +                          R_CALCULATE_NOX)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         NOX_ACTIVE_FOR_UNIT = .TRUE.
         IF(APPLY_NOX_SEASON_DATE(R_NUNITS) == 'T') THEN
            IF(R_DATE >= NOX_SEASON_DATE(R_NUNITS) .AND.
     +                                      R_SEASON_IS_NOX_SEASON) THEN
               R_CALCULATE_NOX = .TRUE.
            ELSE
               R_CALCULATE_NOX = .FALSE.
            ENDIF
         ENDIF
      RETURN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ENTRY GET_NOX_CONTROL_FOR_UNIT(
     +                          R_NUNITS,
     +                          R_DATE,
     +                          R_NOX_CONTROL_PERCENT)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         GET_NOX_CONTROL_FOR_UNIT = .FALSE.
         R_NOX_CONTROL_PERCENT = 1.0
         IF(R_DATE >= NOX_CONTROL_DATE(R_NUNITS)) THEN
            GET_NOX_CONTROL_FOR_UNIT = .TRUE.
            R_NOX_CONTROL_PERCENT = NOX_CONTROL_PERCENT(R_NUNITS)
         ENDIF
      RETURN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ENTRY SET_SOX_CONTROL_COAL_LP(R_NUNITS,
     +                              R_DATE,
     +                              R_SOX_CONTROL_PERCENT)
        SET_SOX_CONTROL_COAL_LP = .TRUE.
        SOX_CONTROL_DATE_TEMP(R_NUNITS) =  SOX_CONTROL_DATE(R_NUNITS)
        SOX_CONTROL_PERCENT_TEMP(R_NUNITS) =
     +                                     SOX_CONTROL_PERCENT(R_NUNITS)
        IF(R_DATE < SOX_CONTROL_DATE(R_NUNITS)) THEN
           SOX_CONTROL_DATE(R_NUNITS) =  R_DATE
           SOX_CONTROL_PERCENT(R_NUNITS) = R_SOX_CONTROL_PERCENT
        ENDIF
      RETURN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ENTRY RESET_SOX_CONTROL_COAL_LP(R_NUNITS)
        RESET_SOX_CONTROL_COAL_LP = .FALSE.
        SOX_CONTROL_DATE(R_NUNITS) =  SOX_CONTROL_DATE_TEMP(R_NUNITS)
        SOX_CONTROL_PERCENT(R_NUNITS) =
     +                                SOX_CONTROL_PERCENT_TEMP(R_NUNITS)
      RETURN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ENTRY GET_SOX_CONTROL_FOR_UNIT(
     +                          R_NUNITS,
     +                          R_DATE,
     +                          R_SOX_CONTROL_PERCENT)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         GET_SOX_CONTROL_FOR_UNIT = .FALSE.
         R_SOX_CONTROL_PERCENT = 1.0
         IF(R_DATE >= SOX_CONTROL_DATE(R_NUNITS)) THEN
            GET_SOX_CONTROL_FOR_UNIT = .TRUE.
            R_SOX_CONTROL_PERCENT = SOX_CONTROL_PERCENT(R_NUNITS)
         ENDIF
      RETURN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ENTRY GET_CO2_CONTROL_FOR_UNIT(
     +                          R_NUNITS,
     +                          R_DATE,
     +                          R_CO2_CONTROL_PERCENT,
     +                          R_CO2_FIRST_CONTROL_MONTH,
     +                          R_CO2_FIRST_CONTROL_YEAR,
     +                          R_CO2_YEAR_BEFORE_CONTROL)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         GET_CO2_CONTROL_FOR_UNIT = .FALSE.
         R_CO2_FIRST_CONTROL_MONTH = .FALSE.
         R_CO2_YEAR_BEFORE_CONTROL = .FALSE.
         R_CO2_CONTROL_PERCENT = 1.0
         R_CO2_FIRST_CONTROL_YEAR = .FALSE.
         IF(R_DATE == CO2_CONTROL_DATE(R_NUNITS) - 100) THEN
            R_CO2_YEAR_BEFORE_CONTROL = .TRUE.
         ENDIF
         IF(R_DATE >= CO2_CONTROL_DATE(R_NUNITS)) THEN
            GET_CO2_CONTROL_FOR_UNIT = .TRUE.
            R_CO2_CONTROL_PERCENT = CO2_CONTROL_PERCENT(R_NUNITS)
            IF(R_DATE == CO2_CONTROL_DATE(R_NUNITS)) THEN
               R_CO2_FIRST_CONTROL_MONTH = .TRUE.
            ENDIF
            IF(R_DATE/100 == CO2_CONTROL_DATE(R_NUNITS)/100) THEN
               R_CO2_FIRST_CONTROL_YEAR = .TRUE.
            ENDIF
         ENDIF
      RETURN
!***********************************************************************
! 102909. TO GET MRX TO ADD CAPACITY CORRECTLY
!
      ENTRY ADJUST_CO2_RETRO_PLAN_CAP(R_YEAR)
!
!***********************************************************************
         ADJUST_CO2_RETRO_PLAN_CAP = .TRUE.
         CURRENT_YEAR = BASE_YEAR + R_YEAR
         BASE_DATE = (CURRENT_YEAR - 1900) * 100
         DO I = 1, NUNITS

            IF(BASE_DATE/100 >= CO2_CONTROL_DATE(I)/100) THEN
               RETRO_CAP_CO2_MULT = CO2_RETRO_CAP_MULT(I)
! RETIRE CURRENT CAPACITY
               TEMP_R4 = CL_CAPACITY_PLANNING_ADJ(
     +                                   CURRENT_YEAR,I,2,.TRUE.)
!
               IF(INPUT_MW(2,I) < -.01) THEN
! NEED TO 'POKE' A NEW CAPACITY
               ELSE
                  INPUT_MW(2,I) = INPUT_MW(2,I) * RETRO_CAP_CO2_MULT
               ENDIF
! ADD NEW CAPACITY
               TEMP_L = RESURRECT_RETROFIT_UNIT(CURRENT_YEAR,I)
               TEMP_R4 = CL_CAPACITY_PLANNING_ADJ(
     +                                  CURRENT_YEAR,I,2,.FALSE.)
               IF(RETRO_CAP_CO2_MULT > .01) THEN
                  IF(INPUT_MW(2,I) < -.01) THEN
                  ELSE
                     INPUT_MW(2,I) = INPUT_MW(2,I)/RETRO_CAP_CO2_MULT
                  ENDIF
               ENDIF
            ENDIF
         ENDDO
      RETURN
!***********************************************************************
! 102909. TO GET GRX TO ADD CAPACITY CORRECTLY
!
      ENTRY ADJUST_GRX_CO2_RETRO_PLAN_CAP(R_YEAR,R_UNITNO)
!
!***********************************************************************
         ADJUST_GRX_CO2_RETRO_PLAN_CAP = .TRUE.
         CURRENT_YEAR = BASE_YEAR + R_YEAR

         RETRO_CAP_CO2_MULT = CO2_RETRO_CAP_MULT(I)
         IF(GRX_RETROFITED_UNIT(R_UNITNO,RETRO_CAP_CO2_MULT)) THEN
! RETIRE CURRENT CAPACITY
            TEMP_R4 = CL_CAPACITY_PLANNING_ADJ(
     +                                   CURRENT_YEAR,R_UNITNO,2,.TRUE.)
!
            IF(INPUT_MW(2,R_UNITNO) < -.01) THEN
! NEED TO 'POKE' A NEW CAPACITY
            ELSE
               INPUT_MW(2,R_UNITNO) = INPUT_MW(2,R_UNITNO)
     +                                              * RETRO_CAP_CO2_MULT
            ENDIF
! ADD NEW CAPACITY
            TEMP_L = RESURRECT_RETROFIT_UNIT(CURRENT_YEAR,R_UNITNO)
            TEMP_R4 = CL_CAPACITY_PLANNING_ADJ(
     +                                  CURRENT_YEAR,R_UNITNO,2,.FALSE.)
            IF(RETRO_CAP_CO2_MULT > .01) THEN
               IF(INPUT_MW(2,R_UNITNO) < -.01) THEN
               ELSE
                  INPUT_MW(2,R_UNITNO) = INPUT_MW(2,R_UNITNO)/
     +                                               RETRO_CAP_CO2_MULT
               ENDIF
            ENDIF
         ENDIF
      RETURN
!***********************************************************************
      ENTRY GET_CO2_RETRO_HEAT_MULT(R_NUNITS)
!***********************************************************************
         GET_CO2_RETRO_HEAT_MULT = CO2_RETRO_HEAT_MULT(R_NUNITS)
      RETURN
!***********************************************************************
      ENTRY GET_CO2_RETRO_CAP_MULT(R_NUNITS)
!***********************************************************************
         GET_CO2_RETRO_CAP_MULT = CO2_RETRO_CAP_MULT(R_NUNITS)
      RETURN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ENTRY GET_HG_CONTROL_FOR_UNIT(
     +                          R_NUNITS,
     +                          R_DATE,
     +                          R_HG_CONTROL_PERCENT)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         GET_HG_CONTROL_FOR_UNIT = .FALSE.
         R_HG_CONTROL_PERCENT = 1.0
         IF(R_DATE >= HG_CONTROL_DATE(R_NUNITS)) THEN
            GET_HG_CONTROL_FOR_UNIT = .TRUE.
            R_HG_CONTROL_PERCENT = HG_CONTROL_PERCENT(R_NUNITS)
         ENDIF
      RETURN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ENTRY GET_OTHER3_CONTROL_FOR_UNIT(
     +                          R_NUNITS,
     +                          R_DATE,
     +                          R_OTHER3_CONTROL_PERCENT)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         GET_OTHER3_CONTROL_FOR_UNIT = .FALSE.
         R_OTHER3_CONTROL_PERCENT = 1.0
         IF(R_DATE >= OTHER3_CONTROL_DATE(R_NUNITS)) THEN
            GET_OTHER3_CONTROL_FOR_UNIT = .TRUE.
            R_OTHER3_CONTROL_PERCENT = OTHER3_CONTROL_PERCENT(R_NUNITS)
         ENDIF
      RETURN


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ENTRY GET_POWERDAT_PLANT_ID(R_NUNITS)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         GET_POWERDAT_PLANT_ID = FLOAT(POWERDAT_PLANT_ID(R_NUNITS))
      RETURN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 10/1/00 MOVED FROM MARGNOBJ.FOR
!
      ENTRY GET_RESOURCE_ID_TO_UNIT(R_ID,R_NUMBER_OF_RESOURCE_ID) ! I2
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
         GET_RESOURCE_ID_TO_UNIT = RESOURCE_ID_TO_UNIT(R_ID)
         R_NUMBER_OF_RESOURCE_ID = NUMBER_OF_RESOURCE_ID(R_ID)
         IF(GET_RESOURCE_ID_TO_UNIT > 0) THEN
            TEMP_R4 = TEMP_R4
         ENDIF
      RETURN
! 063007.
!***********************************************************************
      ENTRY GET_I8_ID_TO_UNIT(R_I8_ID) ! I8
!***********************************************************************
! 021910 CHANGED TO GET CORRECT RETURN
         GET_I8_ID_TO_UNIT = 0
         DO I = 1, NUNITS
            IF(HESI_UNIT_ID_NUM(I) /= R_I8_ID) CYCLE
            GET_I8_ID_TO_UNIT = I
            EXIT
         END DO
      RETURN
!***********************************************************************
      ENTRY UNIQUE_REPORT_VALUE_FOR_CL_UNIT(R_NUNITS)  ! I4
!***********************************************************************
         UNIQUE_REPORT_VALUE_FOR_CL_UNIT =
     +                                   CL_UNIT_UNIQUE_RPT_ID(R_NUNITS)
      RETURN
!***********************************************************************
      ENTRY REPORT_THIS_CL_UNIT(R_NUNITS)  ! L1
!***********************************************************************
         REPORT_THIS_CL_UNIT = REPORT_THIS_UNIT(R_NUNITS) == 'T'
      RETURN
!***********************************************************************
      ENTRY GET_VCPMWH_IN(R_NUNITS)  !R4
!***********************************************************************
         GET_VCPMWH_IN = VCPMWH_IN(R_NUNITS)
      RETURN
!***********************************************************************
      ENTRY ESCALATE_THERMAL_VOM(R_YEAR,R_MONTH,R_NUNITS)  !R4
!***********************************************************************
         IF(GRX_ITERATIONS == 0 .OR. R_NUNITS > GRX_NUNITS) THEN
            VCPMWH_IN(R_NUNITS) =
     +                      ESCALATED_MONTHLY_VALUE(VCPMWH_IN(R_NUNITS),
     +                          OMESCR(R_NUNITS),R_YEAR,R_MONTH,INT2(1))
         ENDIF
         ESCALATE_THERMAL_VOM = VCPMWH_IN(R_NUNITS)
      RETURN
!***********************************************************************
      ENTRY ESCALATE_THERMAL_FOM(R_YEAR,R_MONTH,R_NUNITS)  !R4
!***********************************************************************
         IF(GRX_ITERATIONS == 0 .OR. R_NUNITS > GRX_NUNITS) THEN
            FIXED_COST_IN(R_NUNITS) =
     +                  ESCALATED_MONTHLY_VALUE(FIXED_COST_IN(R_NUNITS),
     +                          FIXED_COST_ESCALATOR(R_NUNITS),
     +                                           R_YEAR,R_MONTH,INT2(1))
         ENDIF
         ESCALATE_THERMAL_FOM = FIXED_COST_IN(R_NUNITS)
      RETURN
!***********************************************************************
      ENTRY GET_FIXED_COST_IN(R_NUNITS)  !R4
!***********************************************************************
         GET_FIXED_COST_IN = FIXED_COST_IN(R_NUNITS)
      RETURN
!***********************************************************************
      ENTRY GET_NOX_VOM(R_NUNITS)  !R4
!***********************************************************************
         GET_NOX_VOM = NOX_VOM(R_NUNITS)
      RETURN
!***********************************************************************
      ENTRY GET_NOX_FOM(R_NUNITS)  !R4
!***********************************************************************
         GET_NOX_FOM = NOX_FOM(R_NUNITS)
      RETURN
!***********************************************************************
      ENTRY GET_SOX_VOM(R_NUNITS)  !R4
!***********************************************************************
         GET_SOX_VOM = SOX_VOM(R_NUNITS)
      RETURN
!***********************************************************************
      ENTRY GET_SOX_FOM(R_NUNITS)  !R4
!***********************************************************************
         GET_SOX_FOM = SOX_FOM(R_NUNITS)
      RETURN
!***********************************************************************
      ENTRY GET_CO2_VOM(R_NUNITS)  !R4
!***********************************************************************
         GET_CO2_VOM = CO2_VOM(R_NUNITS)
      RETURN
!***********************************************************************
      ENTRY GET_CO2_FOM(R_NUNITS)  !R4
!***********************************************************************
         GET_CO2_FOM = CO2_FOM(R_NUNITS)
      RETURN
!***********************************************************************
      ENTRY GET_HG_VOM(R_NUNITS)  !R4
!***********************************************************************
         GET_HG_VOM = HG_VOM(R_NUNITS)
      RETURN
!***********************************************************************
      ENTRY GET_HG_FOM(R_NUNITS)  !R4
!***********************************************************************
         GET_HG_FOM = HG_FOM(R_NUNITS)
      RETURN
!***********************************************************************
      ENTRY GET_OTHER3_VOM(R_NUNITS)  !R4
!***********************************************************************
         GET_OTHER3_VOM = OTHER3_VOM(R_NUNITS)
      RETURN
!***********************************************************************
      ENTRY GET_OTHER3_FOM(R_NUNITS)  !R4
!***********************************************************************
         GET_OTHER3_FOM = OTHER3_FOM(R_NUNITS)
      RETURN
!***********************************************************************
      ENTRY GET_MARKET_FLOOR(R_NUNITS)
!***********************************************************************
         GET_MARKET_FLOOR = MARKET_FLOOR(R_NUNITS)
      RETURN
!***********************************************************************
      ENTRY GET_MARKET_CEILING(R_NUNITS)
!***********************************************************************
         GET_MARKET_CEILING = MARKET_CEILING(R_NUNITS)
      RETURN
!***********************************************************************
      ENTRY GET_PBTUCT_SAVE(R_NUNITS)  !R4
!***********************************************************************
         GET_PBTUCT_SAVE = PBTUCT_SAVE(R_NUNITS)
      RETURN
!***********************************************************************
      ENTRY GET_DISPADJ_SAVE(R_NUNITS)  !R4
!***********************************************************************
         GET_DISPADJ_SAVE = DISPADJ_SAVE(R_NUNITS)
      RETURN
!***********************************************************************
      ENTRY GET_DISPADJ2_SAVE(R_NUNITS)  !R4
!***********************************************************************
         GET_DISPADJ2_SAVE = DISPADJ2_SAVE(R_NUNITS)
      RETURN
!***********************************************************************
      ENTRY GET_S_FUEL_DELIVERY(R_NUNITS)  !R4
!***********************************************************************
         GET_S_FUEL_DELIVERY = S_FUEL_DELIVERY(R_NUNITS)
      RETURN
!***********************************************************************
      ENTRY GET_S_FUEL_DELIVERY_2(R_NUNITS)   !R4
!***********************************************************************
         GET_S_FUEL_DELIVERY_2 = S_FUEL_DELIVERY_2(R_NUNITS)
      RETURN
!***********************************************************************
      ENTRY GET_S_FUEL_DELIVERY_3(R_NUNITS)  !R4
!***********************************************************************
         GET_S_FUEL_DELIVERY_3 = S_FUEL_DELIVERY_3(R_NUNITS)
      RETURN

!***********************************************************************
       ENTRY GET_START_UP_LOGIC(R_NUNITS,R_START_UP_TYPE) !L1
!***********************************************************************
          R_START_UP_TYPE = START_UP_LOGIC(R_NUNITS)
          GET_START_UP_LOGIC = .TRUE.
       RETURN
!***********************************************************************
       ENTRY GET_SPIN_STATUS(R_NUNITS)
!***********************************************************************
          GET_SPIN_STATUS = CONTRIBUTES_TO_SPIN(R_NUNITS) /= 'F'
       RETURN
!***********************************************************************
      ENTRY GET_TOTAL_START_UP_UNITS()
!***********************************************************************
         GET_TOTAL_START_UP_UNITS = TOTAL_START_UP_UNITS
      RETURN
!***********************************************************************
      ENTRY GET_NEWGEN_INDEX(R_NUNITS) ! INTEGER*2
!***********************************************************************
         GET_NEWGEN_INDEX = NEWGEN_INDEX(R_NUNITS)
      RETURN
!***********************************************************************
      ENTRY GET_START_UP_INDEX(R_NUNITS)
!***********************************************************************
         GET_START_UP_INDEX = START_UP_INDEX(R_NUNITS)
      RETURN
!***********************************************************************
      ENTRY GET_ANNUAL_GROSS_MARGIN(R_NUNITS)
!***********************************************************************
         GET_ANNUAL_GROSS_MARGIN = ANNUAL_GROSS_MARGIN(R_NUNITS,3)
      RETURN
!***********************************************************************
      ENTRY GET_UNIT_START_UP_COSTS(R_YEAR,R_NUNITS,R_MONTH)
!***********************************************************************
         IF(START_UP_COSTS(R_NUNITS) < 0.) THEN
            GET_UNIT_START_UP_COSTS =
     +                 START_UP_COST_ESCALAT_MTHLY_VAL(
     +                              INT2(ABS(START_UP_COSTS(R_NUNITS))),
     +                              R_YEAR)
         ELSE
!
            IF(START_UP_COSTS_ESCALATION(R_NUNITS) < 0.) THEN
               GET_UNIT_START_UP_COSTS = 1. ! START_UP_COSTS(R_NUNITS)
               VECTOR_IS_VALUE = .FALSE.
               DO YR = R_YEAR ,1,-1
                  TEMP_I2 = INT2(START_UP_COSTS_ESCALATION(R_NUNITS))
                  ESCAL_RATE = RET_VAL_FROM_ESCALATION_VECTOR(
     +                               YR,TEMP_I2,VECTOR_IS_VALUE,R_MONTH)
                  IF(VECTOR_IS_VALUE) THEN

                     GET_UNIT_START_UP_COSTS =
     +                  GET_UNIT_START_UP_COSTS * ESCAL_RATE

                     EXIT
                  ELSE
                     GET_UNIT_START_UP_COSTS = GET_UNIT_START_UP_COSTS *
     +                                           (1.0 + ESCAL_RATE/100.)
                  ENDIF
               ENDDO ! YR
               IF(.NOT. VECTOR_IS_VALUE) THEN
                  GET_UNIT_START_UP_COSTS = GET_UNIT_START_UP_COSTS *
     +                                          START_UP_COSTS(R_NUNITS)
               ENDIF
            ELSEIF(START_UP_COSTS_ESCALATION(R_NUNITS) > 0.) THEN
               ESCAL_RATE =  START_UP_COSTS_ESCALATION(R_NUNITS)/100.
               GET_UNIT_START_UP_COSTS = START_UP_COSTS(R_NUNITS)
               DO YR = 1, R_YEAR
                  GET_UNIT_START_UP_COSTS = GET_UNIT_START_UP_COSTS *
     +                                                (1.0 + ESCAL_RATE)
               ENDDO
            ELSE
               GET_UNIT_START_UP_COSTS = START_UP_COSTS(R_NUNITS)
            ENDIF
         ENDIF
      RETURN
!***********************************************************************
      ENTRY GET_START_UP_COSTS(R_NUNITS)
!***********************************************************************
         GET_START_UP_COSTS = START_UP_COSTS(R_NUNITS)
      RETURN
!***********************************************************************
      ENTRY GET_RAMP_RATE(R_NUNITS)
!***********************************************************************
         GET_RAMP_RATE = RAMP_RATE(R_NUNITS)
      RETURN
!***********************************************************************
      ENTRY GET_RAMP_DOWN_RATE(R_NUNITS)
!***********************************************************************
         GET_RAMP_DOWN_RATE = RAMP_DOWN_RATE(R_NUNITS)
      RETURN
!***********************************************************************
      ENTRY GET_MIN_UP_TIME(R_NUNITS)
!***********************************************************************
         GET_MIN_UP_TIME = MIN_UP_TIME(R_NUNITS)
      RETURN
!***********************************************************************
      ENTRY GET_FOR_DURATION(R_NUNITS)
!***********************************************************************
         GET_FOR_DURATION = FOR_DURATION(R_NUNITS)
      RETURN
!***********************************************************************
      ENTRY GET_FOR_FREQUENCY(R_NUNITS)
!***********************************************************************
         GET_FOR_FREQUENCY = FOR_FREQUENCY(R_NUNITS)
      RETURN
!***********************************************************************
      ENTRY GET_MIN_DOWN_TIME(R_NUNITS)
!***********************************************************************
         GET_MIN_DOWN_TIME = MIN_DOWN_TIME(R_NUNITS)
      RETURN
!***********************************************************************
      ENTRY UNIT_ON_LINE_IN_YEAR(R_NUNITS,R_YEAR)
!***********************************************************************
         BASE_DATE = (BASE_YEAR + R_YEAR - 1900) * 100
         DATE1 = BASE_DATE + 1
         DATE2 = BASE_DATE + 12
      IF(ONLINE(R_NUNITS) > DATE2 .OR.
     +  OFLINE(R_NUNITS) < DATE1) THEN
            UNIT_ON_LINE_IN_YEAR = .FALSE.
         ELSE
            UNIT_ON_LINE_IN_YEAR = .TRUE.
         ENDIF
      RETURN
!***********************************************************************
      ENTRY INIT_ANNUAL_GROSS_MARGIN()
!***********************************************************************
!
         STRATEGIC_RETIRMENTS_LOGIC =
     +         GET_STRATEGIC_RETIRMENTS_LOGIC(MINIMUM_MARGIN_PER_KW)
         INIT_ANNUAL_GROSS_MARGIN = MINIMUM_MARGIN_PER_KW
         RETIREMENTS_CO2 = 0.0
         RETIREMENTS_MW = 0.0
         RETIREMENTS_MWH = 0.0
         RETIREMENTS_UPLIFT_PER_TON = 0.0
         RETIREMENTS_CUM_CO2 = 0.0
         RETIREMENTS_CO2_PRICE = 999.0 ! COST OF UNMET CO2
!
         CM_RETIREMENT_KW = 999999.0
         CM_RETIREMENT_UNIT = 0
         NEW_UNIT_RETIRE_VALUE_BY_CM = 0.0
!
       IF(GRX_ITERATIONS == 0) ECON_CO2_RETIRE_PRICE = 999999.0

      RETURN
!***********************************************************************
      ENTRY GET_CM_RETIREMENT_KW(R_CM,R_NUNITS)
!***********************************************************************
         IF(R_CM > 0 .AND. R_CM < 1001) THEN
            IF(CM_RETIREMENT_KW(R_CM) > 999998.0) THEN
               GET_CM_RETIREMENT_KW = 0.0
               R_NUNITS = 0
            ELSE
               GET_CM_RETIREMENT_KW = CM_RETIREMENT_KW(R_CM)
               R_NUNITS = CM_RETIREMENT_UNIT(R_CM)
               ANNUAL_MARGIN_PER_KW =
     +               1000.0 * (ECON_RETIRE_MARGIN(R_NUNITS,0))/
     +                                          (MW(2,R_NUNITS)*4.0)
               WRITE(4,*)
     +          'At GET_CM_RETIREMENT_KW   ',
     +          'RET T-3',ECON_RETIRE_MARGIN(R_NUNITS,1),
     +          'RET T-2',ECON_RETIRE_MARGIN(R_NUNITS,2),
     +          'RET T-1',ECON_RETIRE_MARGIN(R_NUNITS,3),
     +          'RET T  ',ECON_RETIRE_MARGIN(R_NUNITS,4),
     +          'MW     ',MW(2,R_NUNITS),
     +          'RET SUM',ECON_RETIRE_MARGIN(R_NUNITS,0),
     +          'R_NUNITS',R_NUNITS,
     +          'R_CM',R_CM,
     +          'IN-CM_RETIRE_KW',CM_RETIREMENT_KW(R_CM),
     +          'ANNUAL_MARGIN_PER_KW',ANNUAL_MARGIN_PER_KW,
     +          'ReCalCM_RETIRE_KW',MIN(CM_RETIREMENT_KW(R_CM),
     +                                         -ANNUAL_MARGIN_PER_KW)
            ENDIF
         ENDIF
      RETURN
!***********************************************************************
      ENTRY SAVE_ANNUAL_GROSS_MARGIN(  R_NUNITS,
     +                                 R_MW,
     +                                 R_GROSS_MARGIN,
     +                                 R_CO2,R_MWH,
     +                                 R_CO2_COST,
     +                                 R_CAP_COST)
!***********************************************************************

         LAST_ITER_GROSS_MARGIN(R_NUNITS) = R_GROSS_MARGIN
         IF(GRX_ITERATIONS == 0) then
            IF(USED_4_YR_EBITDA_TOTAL .OR. USE_EACH_YR_EBITDA_NEG) THEN
               ECON_RETIRE_MARGIN(R_NUNITS,1) =
     +                                    ECON_RETIRE_MARGIN(R_NUNITS,2)
               ECON_RETIRE_MARGIN(R_NUNITS,2) =
     +                                    ECON_RETIRE_MARGIN(R_NUNITS,3)
               ECON_RETIRE_MARGIN(R_NUNITS,3) =
     +                                    ECON_RETIRE_MARGIN(R_NUNITS,4)
               ECON_RETIRE_MARGIN(R_NUNITS,4) = R_GROSS_MARGIN
               ITER_0_ECON_RETIRE_MARGIN(R_NUNITS) =
     +                                 ECON_RETIRE_MARGIN(R_NUNITS,1) +
     +                                 ECON_RETIRE_MARGIN(R_NUNITS,2) +
     +                                 ECON_RETIRE_MARGIN(R_NUNITS,3) +
     +                                 ECON_RETIRE_MARGIN(R_NUNITS,4)
            ELSE
               ITER_0_ECON_RETIRE_MARGIN(R_NUNITS) = R_GROSS_MARGIN
               ECON_RETIRE_MARGIN(R_NUNITS,4) =
     +                                  LAST_ITER_GROSS_MARGIN(R_NUNITS)
            ENDIF
         ELSE
            ECON_RETIRE_MARGIN(R_NUNITS,4) =
     +                                  LAST_ITER_GROSS_MARGIN(R_NUNITS)
         ENDIF
         ECON_RETIRE_MARGIN(R_NUNITS,0) =
     +                                 ECON_RETIRE_MARGIN(R_NUNITS,1) +
     +                                 ECON_RETIRE_MARGIN(R_NUNITS,2) +
     +                                 ECON_RETIRE_MARGIN(R_NUNITS,3) +
     +                                 ECON_RETIRE_MARGIN(R_NUNITS,4)
         IF(R_MWH < .001) THEN
            RETIREMENT_MEASURE = ITER_0_ECON_RETIRE_MARGIN(R_NUNITS) ! RESET TO 0 ITER
         ELSEIF(USED_4_YR_EBITDA_TOTAL .OR. USE_EACH_YR_EBITDA_NEG) THEN
            RETIREMENT_MEASURE = ECON_RETIRE_MARGIN(R_NUNITS,1) +
     +                           ECON_RETIRE_MARGIN(R_NUNITS,2) +
     +                           ECON_RETIRE_MARGIN(R_NUNITS,3) +
     +                           R_GROSS_MARGIN
         ELSE
            RETIREMENT_MEASURE = R_GROSS_MARGIN
         ENDIF
         RETIRE_THIS_UNIT(R_NUNITS) = RETIREMENT_MEASURE < 0.
         IF(USE_EACH_YR_EBITDA_NEG) THEN
            RETIRE_THIS_UNIT(R_NUNITS) =
     +                      ECON_RETIRE_MARGIN(R_NUNITS,1) < 0.
     +                      .AND. ECON_RETIRE_MARGIN(R_NUNITS,2)  < 0.
     +                      .AND. ECON_RETIRE_MARGIN(R_NUNITS,3) < 0.
     +                      .AND. R_GROSS_MARGIN < 0.
         ENDIF
         IF(RETIRE_THIS_UNIT(R_NUNITS) .AND.
     +                                    YEAR + BASE_YEAR == 2032) THEN
            RETIREMENT_MEASURE = RETIREMENT_MEASURE
         ENDIF
!
!
! retirement unit price is set here. retros are in
         ECON_CO2_RETIRE_PRICE(R_NUNITS) = RETIREMENT_MEASURE
         CO2_K = 0.0000005 * 7000.0 * 130.0
         if(ABS(R_CO2 - CO2_K * R_MWH) < 0.01) then
            ECON_CO2_RETRO_PRICE(R_NUNITS) = RETIREMENT_MEASURE
         else
! the retro fit comparison should be based on the change in theunit CO2 output
            ECON_CO2_RETRO_PRICE(R_NUNITS) =
     +                              RETIREMENT_MEASURE* 1000000.0 /
     +                                           (R_CO2 - CO2_K * R_MWH)
         endif
!
         RETIREMENTS_CO2(R_NUNITS) = R_CO2
         RETIREMENTS_CO2_COST(R_NUNITS) = RETIREMENT_MEASURE ! R_CO2_COST
         RETIREMENTS_CAP_COST(R_NUNITS) = R_CAP_COST
         RETIREMENTS_MW(R_NUNITS) = R_MW
         RETIREMENTS_MWH(R_NUNITS) = R_MWH
!
         ANNUAL_GROSS_MARGIN(R_NUNITS,3) = R_GROSS_MARGIN
         IF(GRX_ITERATIONS == 0) then
            ANNUAL_GROSS_MARGIN(R_NUNITS,1) =
     +                                   ANNUAL_GROSS_MARGIN(R_NUNITS,2)
            ANNUAL_GROSS_MARGIN(R_NUNITS,2) =
     +                                   ANNUAL_GROSS_MARGIN(R_NUNITS,3)
            ANNUAL_GROSS_MARGIN(R_NUNITS,0) =
     +                                 ANNUAL_GROSS_MARGIN(R_NUNITS,1) +
     +                                 ANNUAL_GROSS_MARGIN(R_NUNITS,2) +
     +                                 ANNUAL_GROSS_MARGIN(R_NUNITS,3)
         ENDIF
         IF(R_MWH < .001) THEN
            UNIT_ANNUAL_GROSS_MARGIN = ANNUAL_GROSS_MARGIN(R_NUNITS,0)
         ELSE
            UNIT_ANNUAL_GROSS_MARGIN = R_GROSS_MARGIN +
     +                                 ANNUAL_GROSS_MARGIN(R_NUNITS,1) +
     +                                 ANNUAL_GROSS_MARGIN(R_NUNITS,2)
         ENDIF
         IF(STRATEGIC_RETIRMENTS_LOGIC .AND. R_MW > 0.) THEN
            ANNUAL_MARGIN_PER_KW = UNIT_ANNUAL_GROSS_MARGIN/(.036*R_MW)
            CURRENT_YEAR = YEAR+BASE_YEAR
            IF(ANNUAL_MARGIN_PER_KW < MINIMUM_MARGIN_PER_KW) THEN
!
               TEMP_R4 = CL_CAPACITY_PLANNING_ADJ(
     +                                   CURRENT_YEAR,R_NUNITS,2,.TRUE.)
               TEMP_I2 = CL_CAPACITY_TEMP_RETIRE_UNIT(CURRENT_YEAR,
     +                                                R_NUNITS)
               WRITE(9,1007) "Retirement",UNITNM(R_NUNITS),CURRENT_YEAR,
     +                                    ANNUAL_MARGIN_PER_KW
            ENDIF
         ENDIF
         TG = GET_TRANS_GROUP_POSITION(
     +                                   TRANSACTION_GROUP_ID(R_NUNITS))
         CM = GET_CM_FROM_TG(TG)

         IF(MW(2,R_NUNITS) > 0.001 .AND. R_MWH > 0.001 .AND.
     +            ECON_RETIRE_MARGIN(R_NUNITS,4) < 0.0 .AND.
     +            ECON_RETIRE_MARGIN(R_NUNITS,3) < 0.0 .AND.
     +            ECON_RETIRE_MARGIN(R_NUNITS,2) < 0.0 .AND.
     +            ECON_RETIRE_MARGIN(R_NUNITS,1) < 0.0 .AND.
     +                          REPORT_THIS_UNIT(R_NUNITS) == 'T' .AND.
     +            PRIMARY_MOVER_STR(R_NUNITS) /= 'FC    ' .AND.
     +            PRIMARY_MOVER_STR(R_NUNITS) /= 'GE    ' .AND.
     +            PRIMARY_MOVER_STR(R_NUNITS) /= 'BI    ' .AND.
     +            PRIMARY_MOVER_STR(R_NUNITS) /= 'LF    ' .AND.
     +            PRIMARY_MOVER_STR(R_NUNITS) /= 'ZZ    ') THEN
            ANNUAL_MARGIN_PER_KW =
     +               1000.0 * (ECON_RETIRE_MARGIN(R_NUNITS,0))/
     +                                          (MW(2,R_NUNITS)*4.0)
            TEMP_REAL = CM_RETIREMENT_KW(CM)
            IF(ANNUAL_MARGIN_PER_KW < MINIMUM_MARGIN_PER_KW) THEN
!

               IF(-ANNUAL_MARGIN_PER_KW < CM_RETIREMENT_KW(CM)) THEN
                  CM_RETIREMENT_KW(CM) = MIN(CM_RETIREMENT_KW(CM),
     +                                     -ANNUAL_MARGIN_PER_KW)
                  CM_RETIREMENT_UNIT(CM) = R_NUNITS
               ENDIF
            ENDIF
               WRITE(4,*)
     +          'At SAVE_ANNUAL_GROSS_MARGIN   ',
     +          'RET T-3',ECON_RETIRE_MARGIN(R_NUNITS,1),
     +          'RET T-2',ECON_RETIRE_MARGIN(R_NUNITS,2),
     +          'RET T-1',ECON_RETIRE_MARGIN(R_NUNITS,3),
     +          'RET T  ',ECON_RETIRE_MARGIN(R_NUNITS,4),
     +          'MW     ',MW(2,R_NUNITS),
     +          'RET SUM',ECON_RETIRE_MARGIN(R_NUNITS,0),
     +          'R_NUNITS',R_NUNITS,
     +          'R_CM',CM,
     +          'OUT-CM_RETIRE_KW',CM_RETIREMENT_KW(CM),
     +          'ANNUAL_MARGIN_PER_KW',ANNUAL_MARGIN_PER_KW,
     +          'IN-CM_RETIRE_KW',TEMP_REAL
         ENDIF

         IF( 1 == 2 .AND. R_NUNITS > BASE_CL_UNITS .AND.
     +                  R_MW > 0.0 .AND.
     +                        R_GROSS_MARGIN < 0.0 .AND.
     +                              CM > 0 .AND.
     +                                    CM <= UPPER_TRANS_GROUP) THEN
            NEW_UNIT_RETIRE_VALUE_BY_CM(CM) =
     +               MAX(NEW_UNIT_RETIRE_VALUE_BY_CM(CM),
     +                                      -1000.0*R_GROSS_MARGIN/R_MW)
         ENDIF
!
!
!
         SAVE_ANNUAL_GROSS_MARGIN = R_GROSS_MARGIN
      RETURN
 1007 FORMAT(5X,A,5X,A,5X,I4,5X,F7.2)
!***********************************************************************
      ENTRY CALC_MRX_RPS_CURVES(R_YEAR)
!***********************************************************************
         CALC_MRX_RPS_CURVES = .FALSE.


         PG = NUMBER_OF_RPS_PROGRAMS()
         NUM_TRANS = GET_NUM_SAVE_ACTIVE_TRANSACT()
         IF(ALLOCATED(MRX_RPS_CL_CURVE_MWH))
     +          DEALLOCATE(MRX_RPS_CL_CURVE_MWH,
     +                      MRX_RPS_CL_CURVE_CUM_MWH,
     +                      MRX_RPS_CL_CURVE_MARGIN,
     +                      MRX_RPS_CL_CURVE_CUM_MARGIN,
     +                      MRX_RPS_CL_CURVE_INDEX,
     +                      MRX_RPS_CL_UNIT_NO,
     +                      MRX_RPS_CL_UNIT_INDEX,
     +                      MRX_RPS_DV_CURVE_MWH,
     +                      MRX_RPS_DV_CURVE_CUM_MWH,
     +                      MRX_RPS_DV_CURVE_MARGIN,
     +                      MRX_RPS_DV_CURVE_CUM_MARGIN,
     +                      MRX_RPS_DV_CURVE_INDEX,
     +                      MRX_RPS_DV_UNIT_NO,
     +                      MRX_RPS_DV_UNIT_INDEX)
         ALLOCATE(MRX_RPS_CL_CURVE_MWH(NUNITS,PG),
     +            MRX_RPS_CL_CURVE_CUM_MWH(NUNITS,PG),
     +            MRX_RPS_CL_CURVE_MARGIN(NUNITS,PG),
     +            MRX_RPS_CL_CURVE_CUM_MARGIN(NUNITS,PG),
     +            MRX_RPS_CL_CURVE_INDEX(NUNITS,PG),
     +            MRX_RPS_CL_UNIT_NO(PG),
     +            MRX_RPS_CL_UNIT_INDEX(NUNITS,PG),
     +            MRX_RPS_DV_CURVE_MWH(NUM_TRANS,PG),
     +            MRX_RPS_DV_CURVE_CUM_MWH(NUM_TRANS,PG),
     +            MRX_RPS_DV_CURVE_MARGIN(NUM_TRANS,PG),
     +            MRX_RPS_DV_CURVE_CUM_MARGIN(NUM_TRANS,PG),
     +            MRX_RPS_DV_CURVE_INDEX(NUM_TRANS,PG),
     +            MRX_RPS_DV_UNIT_NO(PG),
     +            MRX_RPS_DV_UNIT_INDEX(NUM_TRANS,PG))
         CALC_MRX_RPS_CURVES = .TRUE.
!
         MRX_RPS_CL_UNIT_NO = 0
         MRX_RPS_CL_UNIT_INDEX = 0
         MRX_RPS_CL_CURVE_INDEX = 0
         MRX_RPS_CL_CURVE_MWH = 0.0
         MRX_RPS_CL_CURVE_CUM_MWH = 0.0
         MRX_RPS_CL_CURVE_MARGIN = 0.0
         MRX_RPS_CL_CURVE_CUM_MARGIN = 0.0
         DO U = 1, GRX_NUNITS ! 071220. CHANGED FROM NUNITS.
! DOUBLE INDEX
            PA = RPS_PROGRAM_NUMBER(U)
            IF(PA == 0) CYCLE
            RPS_COUNT = 1
            DO
               PX = GET_RPS_PROGRAM_POSITION(PA,RPS_COUNT)
               IF(PX == 0) EXIT
               RPS_COUNT = RPS_COUNT + 1
!
               LOCAL_RPS_CONTRIB_PERCENT = RPS_CONTRIBUTION_PERCENT(U)
               IF(LOCAL_RPS_CONTRIB_PERCENT < -0.01) THEN
                  LOCAL_RPS_CONTRIB_PERCENT = 0.01 *
     +               ESCALATED_MONTHLY_VALUE(LOCAL_RPS_CONTRIB_PERCENT,
     +                    ABS(INT2(RPS_CONTRIBUTION_PERCENT(U))),
     +                                          R_YEAR,INT2(1),INT2(1))
               ELSE
                  LOCAL_RPS_CONTRIB_PERCENT =
     +                                  0.01 * LOCAL_RPS_CONTRIB_PERCENT
               ENDIF
               TEMP_ENRG = MRX_RPS_CL_CURVE_UMWH(U) *
     +                                         LOCAL_RPS_CONTRIB_PERCENT
!
               IF(TEMP_ENRG < 0.0001) CYCLE
               MRX_RPS_CL_UNIT_NO(PX) = MRX_RPS_CL_UNIT_NO(PX) + 1
               TEMP_I2 = MRX_RPS_CL_UNIT_NO(PX)
               IF(TEMP_I2 < 1 .OR. TEMP_I2 > NUNITS) THEN
                  TEMP_I2 = TEMP_I2
               ENDIF
               MRX_RPS_CL_UNIT_INDEX(TEMP_I2,PX) = U
               MRX_RPS_CL_CURVE_INDEX(TEMP_I2,PX) =
     +                                            MRX_RPS_CL_UNIT_NO(PX)
               MRX_RPS_CL_CURVE_MWH(TEMP_I2,PX) = TEMP_ENRG
               MRX_RPS_CL_CURVE_MARGIN(TEMP_I2,PX) =
     +                 1000.0 * (MRX_RPS_CL_CURVE_REVENUE(U) -
     +                               MRX_RPS_CL_CURVE_COST(U))/TEMP_ENRG
            ENDDO
         ENDDO
         DO PA = 1, PG
            IF(MRX_RPS_CL_UNIT_NO(PA) > 1) THEN
               CALL SortIncrPos(MRX_RPS_CL_UNIT_NO(PA),
     +                          MRX_RPS_CL_CURVE_INDEX(1,PA),
     +                          MRX_RPS_CL_CURVE_MARGIN(1,PA))
            ENDIF
            DO I = MRX_RPS_CL_UNIT_NO(PA), 1, -1
               J = MRX_RPS_CL_CURVE_INDEX(I,PA)
               U = MRX_RPS_CL_UNIT_INDEX(J,PA)
               IF(I == MRX_RPS_CL_UNIT_NO(PA)) THEN
                  MRX_RPS_CL_CURVE_CUM_MWH(I,PA) =
     +                                        MRX_RPS_CL_CURVE_MWH(J,PA)
               ELSE
                  MRX_RPS_CL_CURVE_CUM_MWH(I,PA) =
     +                  MRX_RPS_CL_CURVE_CUM_MWH(I+1,PA) +
     +                                     MRX_RPS_CL_CURVE_MWH(J,PA)
               ENDIF
               MRX_RPS_CL_CURVE_CUM_MARGIN(I,PA) =
     +                                    MRX_RPS_CL_CURVE_MARGIN(J,PA)
            ENDDO
         ENDDO

         MRX_RPS_DV_UNIT_NO = 0
         MRX_RPS_DV_UNIT_INDEX = 0
         MRX_RPS_DV_CURVE_INDEX = 0
         MRX_RPS_DV_CURVE_MWH = 0.0
         MRX_RPS_DV_CURVE_CUM_MWH = 0.0
         MRX_RPS_DV_CURVE_MARGIN = 0.0
         MRX_RPS_DV_CURVE_CUM_MARGIN = 0.0
         DO U = 1, NUM_TRANS
! DOUBLE INDEX
            PA = GET_TRANS_RPS_PROG_NUMBER(U)
            IF(PA == 0) CYCLE
            RPS_COUNT = 1
            DO
               PX = GET_RPS_PROGRAM_POSITION(PA,RPS_COUNT)
               IF(PX == 0) EXIT
               RPS_COUNT = RPS_COUNT + 1
!
               LOCAL_RPS_CONTRIB_PERCENT =
     +                                   GET_TRANS_RPS_PERCENT(U,R_YEAR)
               TEMP_ENRG = MRX_RPS_DV_CURVE_UMWH(U) *
     +                                         LOCAL_RPS_CONTRIB_PERCENT
               IF(TEMP_ENRG < 0.0001) CYCLE
               MRX_RPS_DV_UNIT_NO(PX) = MRX_RPS_DV_UNIT_NO(PX) + 1
               TEMP_I2 = MRX_RPS_DV_UNIT_NO(PX)
               MRX_RPS_DV_UNIT_INDEX(TEMP_I2,PX) = U
               MRX_RPS_DV_CURVE_INDEX(TEMP_I2,PX) =
     +                                            MRX_RPS_DV_UNIT_NO(PX)
               MRX_RPS_DV_CURVE_MWH(TEMP_I2,PX) = TEMP_ENRG
               MRX_RPS_DV_CURVE_MARGIN(TEMP_I2,PX) =
     +                 1000.0 * (MRX_RPS_DV_CURVE_REVENUE(U) -
     +                               MRX_RPS_DV_CURVE_COST(U))/TEMP_ENRG
            ENDDO
         ENDDO
         DO PA = 1, PG
            IF(MRX_RPS_DV_UNIT_NO(PA) > 1) THEN
               CALL SortIncrPos(MRX_RPS_DV_UNIT_NO(PA),
     +                          MRX_RPS_DV_CURVE_INDEX(1,PA),
     +                          MRX_RPS_DV_CURVE_MARGIN(1,PA))
            ENDIF
            DO I = MRX_RPS_DV_UNIT_NO(PA), 1, -1
               J = MRX_RPS_DV_CURVE_INDEX(I,PA)
               U = MRX_RPS_DV_UNIT_INDEX(J,PA)
               IF(I == MRX_RPS_DV_UNIT_NO(PA)) THEN
                  MRX_RPS_DV_CURVE_CUM_MWH(I,PA) =
     +                                        MRX_RPS_DV_CURVE_MWH(J,PA)
               ELSE
                  MRX_RPS_DV_CURVE_CUM_MWH(I,PA) =
     +                  MRX_RPS_DV_CURVE_CUM_MWH(I+1,PA) +
     +                                        MRX_RPS_DV_CURVE_MWH(J,PA)
               ENDIF
               MRX_RPS_DV_CURVE_CUM_MARGIN(I,PA) =
     +                                    MRX_RPS_DV_CURVE_MARGIN(J,PA)
            ENDDO
         ENDDO
      RETURN
!***********************************************************************
      ENTRY GET_NEW_UNIT_RETIRE_VALUE_BY_CM(R_CM)
!***********************************************************************
         IF(ALLOCATED(NEW_UNIT_RETIRE_VALUE_BY_CM) .AND. CM > 0 .AND.
     +                                     CM <= UPPER_TRANS_GROUP) THEN
            GET_NEW_UNIT_RETIRE_VALUE_BY_CM =
     +                                 NEW_UNIT_RETIRE_VALUE_BY_CM(R_CM)
         ELSE
            GET_NEW_UNIT_RETIRE_VALUE_BY_CM = 0.0
         ENDIF
      RETURN
!***********************************************************************
      ENTRY ANNUAL_CO2_RETROFIT_PROCESS(R_YEAR)
!***********************************************************************
         TEMP_L = RETROFIT_LOGIC_ACTIVE(TEMP_R4,TEMP_I2)
         ANN_RETROFIT_COUNTER = 0

         IF(.NOT. TEMP_L) THEN
            ANNUAL_CO2_RETROFIT_PROCESS = .FALSE.
            RETURN
         ENDIF
         IF(.NOT. GRX_CONVERGED) THEN
            CALL  GRX_RETROFIT_OPTONS(RESTORE)!RESETS THE UNITS RETIRED IN THE LOOP
         ENDIF
         CO2_RETROFIT_ABATEMENT_INDEX = 0
         CUM_CO2_EMISSIONS = 0.0
         RETROFIT_UNIT_IS_ACTIVE = .FALSE.
         DO I = 1, RETROFIT_COUNTER
            U = RETROFIT_UNIT_INDEX(I)
            IF(RETROFIT_CANDIDATE(U) /= 'T') CYCLE
            IF(RETIREMENTS_CO2(U) < 0.01 .OR.
     +                  RETIREMENTS_MWH(U) < 0.01 .OR.
     +                         RETIREMENTS_MW(U) < 0.0001) CYCLE
            IF(.NOT. GRX_CONVERGED) THEN
               CALL GRX_CAPACITY_TEMP_RETRO_UNIT(U)
            ENDIF

            ANN_RETROFIT_COUNTER = ANN_RETROFIT_COUNTER + 1
            RETROFIT_UNIT_IS_ACTIVE(U) = .TRUE.
            CALL ANNUAL_RETROFIT_PROJECT(RETROFIT_PROJECT_ID(U),
     +                                   R_YEAR,
     +                                   RETIREMENTS_CO2(U),
     +                                   RETIREMENTS_MW(U),
     +                                   RETIREMENTS_MWH(U),
     +                                   ANNUAL_GROSS_MARGIN(U,3),
     +                                   RETROFIT_UPLIFT_PER_TON(U),
     +                                   RETROFIT_CO2)
            RETROFIT_CO2_PRICE(U) = RETROFIT_UPLIFT_PER_TON(U)
            CUM_CO2_EMISSIONS = CUM_CO2_EMISSIONS + RETROFIT_CO2
         END DO
! SORT FROM WORST TO BEST RETIREMENT

         ASCENDING = .FALSE. ! DECENDING
         CALL INDEXED_SORT_ASCENDING(RETROFIT_UPLIFT_PER_TON,
     +                               RETROFIT_UNIT_INDEX,
     +                               RETROFIT_COUNTER,
     +                               ASCENDING)
         NEXT_MRX_RETROFIT = 1
         DO COUNTER = 1, RETROFIT_COUNTER
             TEMP_I2 = RETROFIT_UNIT_INDEX(RETROFIT_COUNTER-COUNTER+1)
             CO2_RETROFIT_ABATEMENT_INDEX(COUNTER) = TEMP_I2
             IF(COUNTER > 1) THEN
               RETROFIT_CUM_CO2(TEMP_I2) =
     +             RETROFIT_CUM_CO2(
     +                        CO2_RETROFIT_ABATEMENT_INDEX(COUNTER-1)) +
     +                                          RETIREMENTS_CO2(TEMP_I2)
             ELSE
               RETROFIT_CUM_CO2(TEMP_I2) = RETIREMENTS_CO2(TEMP_I2)
             ENDIF
             RETROFIT_CO2_PRICE(TEMP_I2) =
     +                               RETROFIT_UPLIFT_PER_TON(TEMP_I2)
         END DO

         ANNUAL_CO2_RETROFIT_PROCESS = .TRUE.
      RETURN
!***********************************************************************
      ENTRY ANNUAL_CO2_RETIREMENTS_PROCESS(R_YEAR)
!***********************************************************************
         RETIREMENTS_INDEX = 0
         CO2_ABATEMENT_INDEX = 0
         ANN_ECON_RETIRE_COUNTER = 0
         CUM_CO2_EMISSIONS = 0.0

         CLASS = 0
         MONTH = 7
         INT4_EMIS_TYPE = 3
         ANNUAL_CO2_RETIREMENTS_PROCESS = .FALSE.
         IF(.NOT. GET_CO2_RETIREMENTS_LOGIC()) RETURN  !MSG AUG 09

         IF(.NOT. GRX_CONVERGED)
     +                          RETIRED_UNITS = RESET_CL_RETIRED_UNITS() !RESETS THE UNITS RETIRED IN THE LOOP
         CURRENT_YEAR = 100*(BASE_YEAR + R_YEAR - 1900)

         DO U = 1, NUNITS

            IF(FIRST_RETIREMENT_YEAR(U) > CURRENT_YEAR) CYCLE
            IF(RETIREMENT_CANDIDATE(U) /= 'T' .OR.
     +                RETIREMENTS_CO2(U) < 0.01 .OR.
     +                     OFLINE(U) < CURRENT_YEAR .OR.
     +                                 RETIREMENTS_MW(U) < 0.0001) CYCLE
            ANN_ECON_RETIRE_COUNTER = ANN_ECON_RETIRE_COUNTER + 1
            RETIREMENTS_INDEX(ANN_ECON_RETIRE_COUNTER) = U

!
            IF(.NOT. GRX_CONVERGED) THEN
               CALL
     +           GRX_CAPACITY_RETIRE_OPTIONS(U,ANNUAL_GROSS_MARGIN(U,3))
            ENDIF
            ANNUAL_CAPITAL_COST = 0.17 * 1050.0 * 1000.0 *
     +                                                 RETIREMENTS_MW(U)

! 031710. NEW MEASURE OF PROFITABILITY
            RETIREMENTS_UPLIFT_PER_TON(U) = ECON_CO2_RETIRE_PRICE(U)

            CUM_CO2_EMISSIONS = CUM_CO2_EMISSIONS +
     +                                                RETIREMENTS_CO2(U)
         END DO
! SORT FROM WORST TO BEST RETIREMENT
         ASCENDING = .FALSE. ! DECENDING
         CALL INDEXED_SORT_ASCENDING(RETIREMENTS_UPLIFT_PER_TON,
     +                               RETIREMENTS_INDEX,
     +                               ANN_ECON_RETIRE_COUNTER,
     +                               ASCENDING)
         NEXT_MRX_RETIREMENT = 1
         DO COUNTER = 1, ANN_ECON_RETIRE_COUNTER
             TEMP_I2 =
     +              RETIREMENTS_INDEX(ANN_ECON_RETIRE_COUNTER-COUNTER+1)
             CO2_ABATEMENT_INDEX(COUNTER) = TEMP_I2
             IF(COUNTER > 1) THEN
               RETIREMENTS_CUM_CO2(TEMP_I2) =
     +             RETIREMENTS_CUM_CO2(CO2_ABATEMENT_INDEX(COUNTER-1)) +
     +                                          RETIREMENTS_CO2(TEMP_I2)
             ELSE
               RETIREMENTS_CUM_CO2(TEMP_I2) = RETIREMENTS_CO2(TEMP_I2)
             ENDIF
             RETIREMENTS_CO2_PRICE(TEMP_I2) =
     +                               RETIREMENTS_UPLIFT_PER_TON(TEMP_I2)
         END DO
         ANNUAL_CO2_RETIREMENTS_PROCESS = .TRUE.
      RETURN
!***********************************************************************
      ENTRY ANNUAL_RETIRE_RETRO_PROCESS(R_YEAR)
!***********************************************************************
         ANNUAL_RETIRE_RETRO_PROCESS = .FALSE.  !MSG AUG 09
         RETIRE_RETRO_COUNTER = 0               !MSG AUG 09
         IF(CO2_RETROFIT_LOGIC_ACTIVE()) THEN
            RETIRE_RETRO_COUNTER = RETIRE_RETRO_COUNTER+RETROFIT_COUNTER !MSG AUG 09
            ABATE_RETROFIT_COUNTER = RETROFIT_COUNTER
         ELSE
            ABATE_RETROFIT_COUNTER = 0
         ENDIF
         IF(GET_CO2_RETIREMENTS_LOGIC())
     +             RETIRE_RETRO_COUNTER = RETIRE_RETRO_COUNTER
     +                                    + ANN_ECON_RETIRE_COUNTER  !MSG AUG 09
          CO2_MARKET_PTS = 0

         IF(RETIRE_RETRO_COUNTER <= 0) RETURN
         ANNUAL_RETIRE_RETRO_PROCESS = .TRUE.
!
         YES_CO2_ABATEMENT_REPORT = CO2_ABATEMENT_REPORT_ACTIVE()
         IF(  YES_CO2_ABATEMENT_REPORT .AND.
     +                 CO2_ABATEMENT_REPORT_NOT_OPEN) THEN
            CO2_ABATEMENT_VAR_NUM = 12
            CO2_ABATEMENT_NO = CO2_ABATEMENT_RPT_HEADER(
     +                                          CO2_ABATEMENT_VAR_NUM,
     +                                          CO2_ABATEMENT_REC)
            CO2_ABATEMENT_REPORT_NOT_OPEN = .FALSE.
         ENDIF
         CURRENT_YEAR = R_YEAR+BASE_YEAR
!
         RETIRE_RETRO_INDEX = 0
         RETIRE_OR_RETRO = 0
         RETIRE_RETRO_POSITION = 0
         RETIRE_RETRO_ABATE_INDEX = 0
! COMBINE RETIRE AND RETRO AND SORT INTO ONE MASTER LIST
         DO I = 1, ANN_ECON_RETIRE_COUNTER
            RETIRE_RETRO_POSITION(I) = I
            RETIRE_RETRO_INDEX(I) = RETIREMENTS_INDEX(I) ! = U
            RETIRE_OR_RETRO(I) = 1 ! RETIRE = 1
! CHANGING INDEX DEFINITION
            RETIRE_RETRO_UPLIFT_PER_TON(I) =
     +                       RETIREMENTS_CO2_PRICE(RETIREMENTS_INDEX(I))
         END DO
         I = ANN_ECON_RETIRE_COUNTER
         DO J = 1, ABATE_RETROFIT_COUNTER
            U = RETROFIT_UNIT_INDEX(J)
            IF(.NOT. RETROFIT_UNIT_IS_ACTIVE(U)) CYCLE
            IF(RETROFIT_CANDIDATE(U) /= 'T') CYCLE

            IF(ALLOCATED(GRX_BOP_RETROFIT_ACTIVE)) THEN
               IF(GRX_BOP_RETROFIT_ACTIVE(U)) CYCLE
            ENDIF
            IF(HardWiredRetrofitProject(U) .AND.
     +                  .NOT. HWRetrofitProjectThisYear(U))CYCLE
            IF(OFLINE(U) < 100*(BASE_YEAR + R_YEAR - 1900)) CYCLE
            I = I + 1
            RETIRE_RETRO_POSITION(I) = I
            RETIRE_RETRO_INDEX(I) = RETROFIT_UNIT_INDEX(J)
            IF(HWRetrofitProjectThisYear(U)) THEN
               RETIRE_OR_RETRO(I) = 4 ! RETRO = IS Hard Wired
            ELSE
               RETIRE_OR_RETRO(I) = 2 ! RETRO = 2
            ENDIF
            RETIRE_RETRO_UPLIFT_PER_TON(I) = RETROFIT_CO2_PRICE(U)
         END DO
       IF(.FALSE. ) THEN
         DO J = 1, CO2_MARKET_PTS
            I = I +1
            RETIRE_RETRO_POSITION(I) = I
            RETIRE_RETRO_INDEX(I) = J
            RETIRE_OR_RETRO(I) = 3 ! MARKET SOURCE = 2
            RETIRE_RETRO_UPLIFT_PER_TON(I) = 0. ! GRX_CO2_MARKET_PRICE(J)
         ENDDO
       ENDIF ! REMOVE MARKET SOURCE WHICH ISN'T ACTIVE
       RETIRE_RETRO_COUNTER = I
! SORT FROM WORST TO BEST RETIREMENT
         ASCENDING = .FALSE. ! DECENDING
         CALL INDEXED_SORT_ASCENDING(RETIRE_RETRO_UPLIFT_PER_TON,
     +                               RETIRE_RETRO_POSITION,
     +                               RETIRE_RETRO_COUNTER,
     +                               ASCENDING)
         CUM_MW = 0.0
         CUM_MWH = 0.0
         RETIRE_RETRO_COUNT_AFTER_SORT = 0
         RETIRE_RETRO_USED = 0
         LAST_COUNTER = 0
         RETIRE_RETRO_CUM_CO2(0) =  0.
         DO COUNTER = 1, RETIRE_RETRO_COUNTER
            TEMP_I2 =
     +             RETIRE_RETRO_POSITION(RETIRE_RETRO_COUNTER-COUNTER+1)
            U = RETIRE_RETRO_INDEX(TEMP_I2)

! 031910. THIS FILTERS-OUT MORE EXPENSIVE OPTIONS.

               RETIRE_RETRO_USED(U) = RETIRE_RETRO_USED(U) + 1

            RETIRE_RETRO_COUNT_AFTER_SORT =
     +                                 RETIRE_RETRO_COUNT_AFTER_SORT + 1
            RETIRE_RETRO_ABATE_INDEX(COUNTER) = U
            IF(RETIRE_OR_RETRO(TEMP_I2) == 3) THEN ! MARKET POSITION
               RPT_UNIT_NAME = "CO2 Market Purchase-"//
     +               CONVERT_2_STR(RETIRE_RETRO_UPLIFT_PER_TON(TEMP_I2))   ! 48 CHARACTERS
               RPT_RETIREMENTS_CO2 = 0. ! GRX_CO2_MARKET_TONS(U)
               RPT_RETIREMENTS_MW = 0.
               RPT_RETIREMENTS_MWH = 0.
            ELSE
               IF(RETIRE_OR_RETRO(TEMP_I2) == 2) THEN
                 RPT_UNIT_NAME = TRIM(SP_UNIT_NAME(U))//'-Retro'
               ELSE
                 RPT_UNIT_NAME = SP_UNIT_NAME(U)
               ENDIF
               RPT_RETIREMENTS_CO2 = RETIREMENTS_CO2(U)
               CUM_MW = CUM_MW + RETIREMENTS_MW(U)
               CUM_MWH = CUM_MWH + RETIREMENTS_MWH(U)
               RPT_RETIREMENTS_MW = RETIREMENTS_MW(U)
               RPT_RETIREMENTS_MWH = RETIREMENTS_MWH(U)
            ENDIF
            RETIRE_RETRO_CUM_CO2(COUNTER) =
     +             RETIRE_RETRO_CUM_CO2(COUNTER-1) + RPT_RETIREMENTS_CO2
            RETIRE_RETRO_CO2_PRICE(COUNTER) =
     +                              RETIRE_RETRO_UPLIFT_PER_TON(TEMP_I2)
            UNIT_Decision_CO2_Price(U) = RETIRE_RETRO_CO2_PRICE(COUNTER)
!
            IF(YES_CO2_ABATEMENT_REPORT) THEN
               IF(CAPACITY_PLANNING_METHOD() == 'MX'  .AND.
     +                                  GREEN_MRX_METHOD() == 'GX') THEN
                  WRITE(CO2_ABATEMENT_NO,
     +                  REC=CO2_ABATEMENT_REC)
     +                        PRT_ENDPOINT(),
     +                        FLOAT(CURRENT_YEAR),
     +                        RPT_UNIT_NAME,
     +                        FLOAT(GRX_ITERATIONS),
     +                        FLOAT(COUNTER),
     +                        FLOAT(RETIRE_OR_RETRO(TEMP_I2)),
     +                        RETIRE_RETRO_CUM_CO2(COUNTER)/1000000.,
     +                        RETIRE_RETRO_CO2_PRICE(COUNTER),
     +                        CUM_MW,
     +                        CUM_MWH,
     +                        RPT_RETIREMENTS_CO2/1000000.,
     +                        RPT_RETIREMENTS_MW,
     +                        RPT_RETIREMENTS_MWH,
     +                        CURRENT_CO2_DISPATCH_COST,
     +                        RETIREMENT_CO2_COST_PER_TON(U)
               ELSE
                  WRITE(CO2_ABATEMENT_NO,
     +                  REC=CO2_ABATEMENT_REC)
     +                        PRT_ENDPOINT(),
     +                        FLOAT(CURRENT_YEAR),
     +                        RPT_UNIT_NAME,
     +                        FLOAT(COUNTER),
     +                        FLOAT(RETIRE_OR_RETRO(TEMP_I2)),
     +                        RETIRE_RETRO_CUM_CO2(COUNTER)/1000000.,
     +                        RETIRE_RETRO_CO2_PRICE(COUNTER),
     +                        CUM_MW,
     +                        CUM_MWH,
     +                        RPT_RETIREMENTS_CO2/1000000.,
     +                        RPT_RETIREMENTS_MW,
     +                        RPT_RETIREMENTS_MWH,
     +                        CURRENT_CO2_DISPATCH_COST,
     +                        RETIREMENT_CO2_COST_PER_TON(U)
               ENDIF
               CO2_ABATEMENT_REC = CO2_ABATEMENT_REC + 1
            ENDIF
         END DO
         IF(YES_CO2_ABATEMENT_REPORT) CALL FLUSH(CO2_ABATEMENT_NO)
         J = J
      RETURN
!***********************************************************************
      ENTRY GET_CO2_RETIREMENT_PRICE(R_CO2_REDUCTION)
!***********************************************************************
! SEARCH ALGORITHM WOULD BE BETTER.
       IF(R_CO2_REDUCTION < 0.01) THEN
          GET_CO2_RETIREMENT_PRICE = 0.0
       ELSE
         IF(R_CO2_REDUCTION <
     +                 RETIREMENTS_CUM_CO2(CO2_ABATEMENT_INDEX(1))) THEN
            COUNTER = 1
         ELSEIF(R_CO2_REDUCTION >
     +       RETIREMENTS_CUM_CO2(CO2_ABATEMENT_INDEX(
     +                                   ANN_ECON_RETIRE_COUNTER))) THEN
            COUNTER = ANN_ECON_RETIRE_COUNTER
         ELSE

            DO COUNTER = 1, ANN_ECON_RETIRE_COUNTER
               IF(R_CO2_REDUCTION >
     +              RETIREMENTS_CUM_CO2(
     +                              CO2_ABATEMENT_INDEX(COUNTER))) CYCLE
               EXIT

            ENDDO
         ENDIF
         GET_CO2_RETIREMENT_PRICE = RETIREMENTS_CO2_PRICE(
     +                                     CO2_ABATEMENT_INDEX(COUNTER))
        ENDIF
      RETURN
!***********************************************************************
      ENTRY GET_CO2_RETIRE_RETRO_PRICE(R_CO2_REDUCTION)
!***********************************************************************
! SEARCH ALGORITHM WOULD BE BETTER.
         IF(R_CO2_REDUCTION < RETIRE_RETRO_CUM_CO2(1)) THEN
            COUNTER = 1
         ELSEIF(R_CO2_REDUCTION >
     +        RETIRE_RETRO_CUM_CO2(RETIRE_RETRO_COUNT_AFTER_SORT) ) THEN
            COUNTER = RETIRE_RETRO_COUNT_AFTER_SORT
         ELSE

            DO COUNTER = 1, RETIRE_RETRO_COUNT_AFTER_SORT
               IF(R_CO2_REDUCTION >
     +              RETIRE_RETRO_CUM_CO2(COUNTER) ) CYCLE
               EXIT

            ENDDO
         ENDIF
         GET_CO2_RETIREMENT_PRICE = RETIRE_RETRO_CO2_PRICE(COUNTER)
      RETURN
!***********************************************************************
      ENTRY GET_NEXT_MRX_RETIRE_RETRO(    R_CO2_COUNTER,
     +                                    R_RETIRE_OR_RETRO,
     +                                    R_CO2_NUNIT,
     +                                    R_CO2_TG,
     +                                    R_CO2_UNIT_MW,
     +                                    R_CO2_UNIT_MWH,
     +                                    R_CO2_UNIT_CO2,
     +                                    R_CO2_UNIT_PRICE,
     +                                    R_CO2_STRIKE_PRICE,
     +                                    R_CO2_UNIT_MW_AFTER,
     +                                    R_CO2_STRIKE_PRICE_AFTER,
     +                                    R_CO2_UNIT_CO2_AFTER,
     +                                    R_CO2_END_LIST)
!***********************************************************************
! FOR RETRO NEED:
!  1. CAPACITY BEFORE
!  2. CAPACITY AFTER
!  3. STRIKE BEFORE
!  4. STRIKE AFTER
!
         GET_NEXT_MRX_RETIRE_RETRO = .FALSE.
         IF(R_CO2_COUNTER > RETIRE_RETRO_COUNT_AFTER_SORT) RETURN
         COUNTER = R_CO2_COUNTER ! ASSUMES THE CORRECT COUNTER IS PASSED IN
         TEMP_I2 =
     +             RETIRE_RETRO_POSITION(RETIRE_RETRO_COUNTER-COUNTER+1)
         R_RETIRE_OR_RETRO = RETIRE_OR_RETRO(TEMP_I2)
         IF(R_RETIRE_OR_RETRO == 3) THEN ! CO2 MARKET
         ELSE
            R_CO2_NUNIT = RETIRE_RETRO_ABATE_INDEX(COUNTER) ! CO2_ABATEMENT_INDEX(COUNTER)
            R_CO2_TG = GET_TRANS_GROUP_POSITION(
     +                                TRANSACTION_GROUP_ID(R_CO2_NUNIT))
            R_CO2_UNIT_CO2 = RETIREMENTS_CO2(R_CO2_NUNIT)
            R_CO2_UNIT_MW = RETIREMENTS_MW(R_CO2_NUNIT)
            R_CO2_UNIT_MWH = RETIREMENTS_MWH(R_CO2_NUNIT)
            R_CO2_UNIT_PRICE = RETIRE_RETRO_CO2_PRICE(COUNTER)
            TEMP_I2 = 2
            R_CO2_STRIKE_PRICE = GET_TOTAL_INCREMENTAL_COST(R_CO2_NUNIT,
     +                                                      TEMP_I2)
            TEMP_I2 = 1
            R_CO2_STRIKE_PRICE =
     +               MAX(R_CO2_STRIKE_PRICE,
     +                      GET_TOTAL_INCREMENTAL_COST(R_CO2_NUNIT,
     +                                                      TEMP_I2))

            IF(R_RETIRE_OR_RETRO == 2) THEN ! .AND. 1 == 2) THEN
               CALL RETURN_RETROFIT_PROJECT_IMPACTS(
     +                        RETROFIT_PROJECT_ID(R_CO2_NUNIT),
     +                                      Min_Cap_Change,
     +                                      Max_Cap_Change,
     +                                      Ave_Heat_Rate_Mult,
     +                                      CO2_Cntl_Percent)
               R_CO2_UNIT_MW_AFTER = R_CO2_UNIT_MW * Max_Cap_Change
! THIS NEEDS TO BE REDEFINED
               R_CO2_STRIKE_PRICE_AFTER = R_CO2_STRIKE_PRICE *
     +                                                Ave_Heat_Rate_Mult
               R_CO2_UNIT_CO2_AFTER = R_CO2_UNIT_CO2 *
     +                                     (1.0 - CO2_Cntl_Percent*0.01)
            ELSE
               R_CO2_UNIT_MW_AFTER = 0.0
               R_CO2_STRIKE_PRICE_AFTER = 0.0
               R_CO2_UNIT_CO2_AFTER = 0.0
            ENDIF
         ENDIF ! CO2 MARKET

         R_CO2_COUNTER = R_CO2_COUNTER + 1
         IF(R_CO2_COUNTER <= RETIRE_RETRO_COUNT_AFTER_SORT) THEN
            R_CO2_END_LIST = .FALSE.
         ELSE
            R_CO2_END_LIST = .TRUE.
         ENDIF
         GET_NEXT_MRX_RETIRE_RETRO = .TRUE.
      RETURN
!***********************************************************************
      ENTRY GET_NEXT_MRX_RETIREMENT(
     +                                    R_CO2_COUNTER,
     +                                    R_CO2_NUNIT,
     +                                    R_CO2_UNIT_MW,
     +                                    R_CO2_UNIT_MWH,
     +                                    R_CO2_UNIT_CO2,
     +                                    R_CO2_UNIT_PRICE,
     +                                    R_CO2_END_LIST)
!***********************************************************************
!
         GET_NEXT_MRX_RETIREMENT = .FALSE.
!
         IF(NEXT_MRX_RETIREMENT == ANN_ECON_RETIRE_COUNTER) THEN
            CURRENT_YEAR = CURRENT_YEAR
         ENDIF
!
         CURRENT_YEAR = YEAR+BASE_YEAR

         DO COUNTER =  NEXT_MRX_RETIREMENT, ANN_ECON_RETIRE_COUNTER

            IF(CO2_ABATEMENT_INDEX(COUNTER) <= 0) CYCLE
            R_CO2_COUNTER = COUNTER
            R_CO2_NUNIT = CO2_ABATEMENT_INDEX(COUNTER)

            R_CO2_UNIT_CO2 = RETIREMENTS_CO2(R_CO2_NUNIT)
            R_CO2_UNIT_MW = RETIREMENTS_MW(R_CO2_NUNIT)
            R_CO2_UNIT_MWH = RETIREMENTS_MWH(R_CO2_NUNIT)
            R_CO2_UNIT_PRICE = RETIREMENTS_CO2_PRICE(R_CO2_NUNIT)

            EXIT
         END DO
         NEXT_MRX_RETIREMENT = COUNTER+1

         IF(COUNTER <= ANN_ECON_RETIRE_COUNTER) THEN
            MRX_RETIRE_THIS_UNIT = .TRUE.
            R_CO2_END_LIST = .FALSE.
         ELSE
            MRX_RETIRE_THIS_UNIT = .FALSE.
            R_CO2_END_LIST = .TRUE.
         ENDIF
! THIS CAN BE MOVED AND BUILD A LIST OF RETIREMENTS TO TEST
! AND THEN RETIRE IN A SEPARATE ROUTINE.
         IF(MRX_RETIRE_THIS_UNIT) THEN

            TEMP_R4 = CL_CAPACITY_PLANNING_ADJ(
     +                                CURRENT_YEAR,R_CO2_NUNIT,2,.TRUE.)
            TEMP_I2 = CL_CAPACITY_TEMP_RETIRE_UNIT(CURRENT_YEAR,
     +                                             R_CO2_NUNIT)
            WRITE(9,1008) "Retirement",UNITNM(R_CO2_NUNIT),
     +                                    CURRENT_YEAR,
     +                                    R_CO2_UNIT_MW,
     +                                    R_CO2_UNIT_CO2,
     +                                    R_CO2_UNIT_PRICE,
     +                                    R_CO2_UNIT_MWH
         ENDIF
!
      RETURN
 1008 FORMAT(3X,A,3X,A,3X,I4,3X,F7.2,3X,F12.0,3X,F12.2)
 1009 FORMAT(1X,I4,5X,I4,4X,I4,2X,A,2X,A,1X,F9.1,F12.0,F12.2)
!***********************************************************************
      ENTRY RETIRE_CO2_THERMAL_UNIT(      R_CO2_NUNIT,
     +                                    R_CO2_UNIT_MW,
     +                                    R_CO2_UNIT_CO2,
     +                                    R_CO2_UNIT_PRICE)
!***********************************************************************
!
         CURRENT_YEAR = YEAR+BASE_YEAR
         TEMP_I2 =
     +            CL_CAPACITY_TEMP_RETIRE_UNIT(CURRENT_YEAR,R_CO2_NUNIT)

         IF(.NOT. GRX_RETIRE_THIS_YEAR) THEN
            TEMP_R4 = CL_CAPACITY_PLANNING_ADJ(
     +                                CURRENT_YEAR,R_CO2_NUNIT,2,.TRUE.)
         ENDIF
         WRITE(9,1009) INT4(PRT_ENDPOINT()),
     +                 CURRENT_YEAR,
     +                 CURRENT_YEAR,
     +                 "Retirement",
     +                 UNITNM(R_CO2_NUNIT),
     +                 R_CO2_UNIT_MW,
     +                 R_CO2_UNIT_CO2,
     +                 R_CO2_UNIT_PRICE
!
      RETURN
!***********************************************************************
      ENTRY RETROFIT_ACTIVE_ID(RR_CO2_NUNIT)
!***********************************************************************
        IF(RETROFIT_ACTIVE(RR_CO2_NUNIT)) THEN
            RETROFIT_ACTIVE_ID = RETROFIT_PROJECT_ID(RR_CO2_NUNIT)
        ELSE
            RETROFIT_ACTIVE_ID = 0
        ENDIF
      RETURN
!***********************************************************************
      ENTRY RETROFIT_PROJECT_ACTIVE(RR_CO2_NUNIT)
!***********************************************************************
        IF(RETROFIT_ACTIVE(RR_CO2_NUNIT)) THEN
            RETROFIT_ACTIVE_ID = RETROFIT_PROJECT_ID(RR_CO2_NUNIT)
            CALL RETRO_PROJECT_AVAILABLE(RETROFIT_ACTIVE_ID,YEAR,
     +                                   TEMP_L1)
            RETROFIT_PROJECT_ACTIVE = TEMP_L1
        ELSE
            RETROFIT_ACTIVE_ID = 0
            RETROFIT_PROJECT_ACTIVE= .FALSE.
        ENDIF
      RETURN
!***********************************************************************
      ENTRY RETROFIT_CO2_THERMAL_UNIT(R_CO2_NUNIT,
     +                                    R_CO2_UNIT_MW,
     +                                    R_CO2_UNIT_CO2,
     +                                    R_CO2_UNIT_PRICE,
     +                                    R_CO2_RETIRE_OR_RETRO)
!***********************************************************************
         RETROFIT_ACTIVE(R_CO2_NUNIT) = .TRUE.
         RETROFIT_CO2_THERMAL_UNIT = .TRUE.
         CALL GRX_RETROFIT_UNIT_LIST(R_CO2_NUNIT,
     +                               R_CO2_RETIRE_OR_RETRO,
     +                               R_CO2_UNIT_PRICE)
         CURRENT_YEAR = YEAR+BASE_YEAR
         WRITE(9,1009)  INT4(PRT_ENDPOINT()),
     +                  CURRENT_YEAR,
     +                  CURRENT_YEAR,
     +                  "Retrofit",
     +                  UNITNM(R_CO2_NUNIT),
     +                  R_CO2_UNIT_MW,
     +                  R_CO2_UNIT_CO2,
     +                  R_CO2_UNIT_PRICE

      RETURN
!C***********************************************************************
      ENTRY TRANS_ANNUAL_UNIT_RETIREMENT() ! (R_NUNITS)
!***********************************************************************
!
! POSSIBLE RETIREMENT CRITERIA:
!
!     LOSES $ X MILLIONS GROSS MARGIN PLUS ANNUALIZED CAPITAL COSTS.
!     SAME AS ABOVE, BUT OVER Y YEARS.
!
!        TEST CRITERIA
!        IF CRITERIA MET:
!           CHANGE OFF LINE YEAR
!           CALL CL_PLANNING_REMOVALS
!
         TRANS_ANNUAL_UNIT_RETIREMENT = .FALSE.
      RETURN
!***********************************************************************
      ENTRY GET_THERMAL_STATE_INDEX(R_NUNITS)
!***********************************************************************
         GET_THERMAL_STATE_INDEX = State_Index(R_NUNITS)
      RETURN
!***********************************************************************
      ENTRY GET_THERMAL_RPS_DATA(R_NUNITS,R_PM,R_ST_TG,R_ST,
     +                                                    R_RPS_CONTRIB)
!***********************************************************************
         R_PM =  PRIMARY_MOVER_INDEX_DB(PRIMARY_MOVER_STR(R_NUNITS))
         R_ST_TG = State_TG_Index(R_NUNITS)
         R_ST = State_Index(R_NUNITS)
         R_RPS_CONTRIB = RPS_CONTRIBUTION_PERCENT(R_NUNITS)* 0.01
         GET_THERMAL_RPS_DATA = .TRUE.
      RETURN
!***********************************************************************
      ENTRY PUT_THERMAL_RPS_ENRG_CAP(R_NUNITS,R_ENRG,R_CAP,R_YEAR,
     +                                                     R_ISEAS)
!***********************************************************************
         TEMP_RPS_NO = RPS_PROGRAM_NUMBER(R_NUNITS)
         TEMP_PM = PRIMARY_MOVER_INDEX_DB(PRIMARY_MOVER_STR(R_NUNITS))
         LOCAL_RPS_CONTRIB_PERCENT = RPS_CONTRIBUTION_PERCENT(R_NUNITS)
         IF(LOCAL_RPS_CONTRIB_PERCENT < -0.01) THEN
            LOCAL_RPS_CONTRIB_PERCENT = 0.01 *
     +               ESCALATED_MONTHLY_VALUE(LOCAL_RPS_CONTRIB_PERCENT,
     +                    ABS(INT2(RPS_CONTRIBUTION_PERCENT(R_NUNITS))),
     +                                           R_YEAR,INT2(1),INT2(1))
         ELSE
            LOCAL_RPS_CONTRIB_PERCENT =
     +                                  0.01 * LOCAL_RPS_CONTRIB_PERCENT
         ENDIF
         TEMP_ENRG = R_ENRG * LOCAL_RPS_CONTRIB_PERCENT
         TEMP_CAP = R_CAP * LOCAL_RPS_CONTRIB_PERCENT
     +
         TEMP_L1 = PUT_RPS_ENRG_CAP(TEMP_RPS_NO,TEMP_PM,
     +                              TEMP_ENRG,TEMP_CAP)
         PUT_THERMAL_RPS_ENRG_CAP = .TRUE.
      RETURN
!***********************************************************************
      ENTRY GET_CL_BASECASE_MARKET_ID(R_NUNITS,R_MARKET_ID)  ! L1
!***********************************************************************
         R_MARKET_ID = BASECASE_MARKET_AREA_ID(R_NUNITS)
         GET_CL_BASECASE_MARKET_ID = .TRUE.
      RETURN
!***********************************************************************
      ENTRY GET_MONTHLY_FUEL_INDEX(R_NUNITS,R_ISEAS)
!***********************************************************************
         IF(MONTHLY_FUEL_INDEX(R_NUNITS) == 0) THEN
            GET_MONTHLY_FUEL_INDEX = 1.
         ELSE
            GET_MONTHLY_FUEL_INDEX =
     +            GET_VAR(FLOAT(ABS(MONTHLY_FUEL_INDEX(R_NUNITS))),
     +                                         R_ISEAS,UNITNM(R_NUNITS))
         ENDIF
      RETURN
!***********************************************************************
      ENTRY GET_PRIMARY_MOVER(R_NUNITS)
!***********************************************************************
         GET_PRIMARY_MOVER = PRIMARY_MOVER(R_NUNITS)
      RETURN
!***********************************************************************
      ENTRY SET_TRANS_FOR_DATA_BASE()
!***********************************************************************
         IF(ALLOCATED(TRANS_GROUP_FOR_DATA_BASE))
     +                             DEALLOCATE(TRANS_GROUP_FOR_DATA_BASE)
         ALLOCATE(TRANS_GROUP_FOR_DATA_BASE(MAX_TRANS_DATA_BASES))
         TRANS_GROUP_FOR_DATA_BASE = 0
         DO M = 1, MAX_DAY_TYPES
            DO I = 1, MAX_TRANS_GROUPS
               RTEMP = DAY_TYPE_TRANS_GROUP_PAIR(M,I)
               IF( RTEMP > 0) THEN
                  TRANS_GROUP_FOR_DATA_BASE(RTEMP) = I
               ENDIF
            ENDDO
         ENDDO
         SET_TRANS_FOR_DATA_BASE = MAX_TRANS_DATA_BASES
      RETURN
!***********************************************************************
      ENTRY GET_ASSET_CLASS_NUM(R_NUNITS)
!***********************************************************************
         GET_ASSET_CLASS_NUM = ASSET_CLASS_NUM(R_NUNITS)
      RETURN
!***********************************************************************
      ENTRY GET_TRANS_FOR_DATA_BASE(R_DATA_BASE)
!***********************************************************************
         GET_TRANS_FOR_DATA_BASE =
     +                            TRANS_GROUP_FOR_DATA_BASE(R_DATA_BASE)
      RETURN
!***********************************************************************
      ENTRY GET_DATA_BASE_FOR_TRANS(R_TRANS_GROUP,R_DAY_TYPE)
!***********************************************************************
         GET_DATA_BASE_FOR_TRANS =
     +               DAY_TYPE_TRANS_GROUP_PAIR(R_DAY_TYPE,R_TRANS_GROUP)
      RETURN
!***********************************************************************
      ENTRY GET_DATA_BASE_FOR_UNIT(R_NUNITS,R_DAY_TYPE)
!***********************************************************************
         IF(R_DAY_TYPE <= 2*MAX_DAY_TYPES) THEN
            GET_DATA_BASE_FOR_UNIT =
     +                           DATA_BASE_POSITION(R_DAY_TYPE,R_NUNITS)
         ELSE
            GET_DATA_BASE_FOR_UNIT = 0
         ENDIF
      RETURN
!***********************************************************************
      ENTRY GET_MAX_TRANS_ID_USED()
!***********************************************************************
         GET_MAX_TRANS_ID_USED =  MAX_TRANS_ID_USED
      RETURN
!***********************************************************************
      ENTRY GET_MAX_DATA_BASES()
!***********************************************************************
         GET_MAX_DATA_BASES = MAX_TRANS_DATA_BASES
      RETURN
!***********************************************************************
      ENTRY GET_NUNITS()
!***********************************************************************
         GET_NUNITS = NUNITS
      RETURN
!***********************************************************************
      ENTRY TRANSACTION_GROUP(R_NUNITS)
     +                 RESULT(R_TRANSACTION_GROUP)
!***********************************************************************
         R_TRANSACTION_GROUP = TRANSACTION_GROUP_ID(R_NUNITS)
      RETURN
!***********************************************************************
      ENTRY DAY_TYPE_FOR_RESOURCE(R_NUNITS)
!***********************************************************************
         DAY_TYPE_FOR_RESOURCE = DAY_TYPE_ID(R_NUNITS)
      RETURN
!***********************************************************************
      ENTRY NUC_FUEL_PRICE_SOURCE_IS_NFILE(R_NUNITS)
!***********************************************************************
         NUC_FUEL_PRICE_SOURCE_IS_NFILE =
     +                          NUC_FUEL_PRICES_FROM_FUEL_FILE(R_NUNITS)
      RETURN
!***********************************************************************
      ENTRY RETURN_SHADOW_UNIT_NUMBER(R_NUNITS)
!***********************************************************************
         RETURN_SHADOW_UNIT_NUMBER = SHADOW_UNIT_NUMBER(R_NUNITS)
      RETURN
!***********************************************************************
      ENTRY NUMBER_OF_CL_UNITS
!***********************************************************************
         NUMBER_OF_CL_UNITS = NUNITS
      RETURN
!***********************************************************************
      ENTRY RESET_CL_UNITS
!***********************************************************************
         DO I = 1, INSTALLED_POINTER
            UNIT_NO = TEMP_RETIRED_UNIT_NO(I)
            OFLINE(UNIT_NO) = TEMP_RETIRED_OFF_LINE(I)
            RETIREMENT_CANDIDATE(UNIT_NO) = TEMP_RETIREMENT_SWITCH(I)
            FIRST_RETIREMENT_YEAR(UNIT_NO) =
     +                                     TEMP_FIRST_RETIREMENT_YEAR(I)
         ENDDO
         NUNITS = AVAILABLE_CL_UNITS
         RESET_CL_UNITS = NUNITS
         INSTALLED_POINTER = 0
      RETURN
!***********************************************************************
      ENTRY RETURN_CL_UNITS_B4_ADDITIONS
!***********************************************************************
         RETURN_CL_UNITS_B4_ADDITIONS = BASE_CL_UNITS
      RETURN
!***********************************************************************
      ENTRY RETURN_UNIT_ADDITIONS
!***********************************************************************
         RETURN_UNIT_ADDITIONS = max(0,NUNITS - BASE_CL_UNITS)
      RETURN
!********************************************************************
      ENTRY INCREMENT_HARDWIRED_CL_UNITS
!***********************************************************************
         BASE_PLUS_HARDWIRED_CL_UNITS = NUNITS
         INCREMENT_HARDWIRED_CL_UNITS = BASE_PLUS_HARDWIRED_CL_UNITS
      RETURN
!********************************************************************
      ENTRY INCREMENT_AVAILABLE_CL_UNITS
!***********************************************************************
         AVAILABLE_CL_UNITS = NUNITS
         INCREMENT_AVAILABLE_CL_UNITS = AVAILABLE_CL_UNITS
      RETURN
!********************************************************************
      ENTRY ADD_NEW_CL_UNIT(UNIT_ON_LINE_YEAR,
     +                      OPERATION_LIFE,
     +                      POINTER,
     +                      R_ON_LINE_MONTH,
     +                      R_ASSET_CLASS,
     +                      R_ALLOCATION_VECTOR,
     +                      R_CL_RESOURCE_ID, ! PER ANDERSON. 3/13/98.GAT
     +                      R_POINTER) ! FOR ICAP REPORT. 4/5/02.
!***********************************************************************
!

            ADD_NEW_CL_UNIT = 0
!
!
            NUNITS = NUNITS + 1
            IF(END_POINT == 2 .AND. YEAR == 23 .AND.
     +                                             NUNITS == 12008) THEN
               NUNITS = NUNITS
            ENDIF
            IF(NUNITS > MAX_CL_UNITS) THEN

               WRITE(SCREEN_MESSAGES,'(A,I5,A)')
     +                     'More than ',MAX_CL_UNITS,' capacity-limited'
               CALL MG_LOCATE_WRITE(20,0,trim(SCREEN_MESSAGES),
     +                                                   ALL_VERSIONS,1)
               CALL MG_LOCATE_WRITE(21,0,
     +                              'units are in the capacity-limited',
     +                                                   ALL_VERSIONS,1)
               WRITE(SCREEN_MESSAGES,'(A,I5)')
     +            'file after expansion planning for endpoint',END_POINT
               CALL MG_LOCATE_WRITE(22,0,trim(SCREEN_MESSAGES),
     +                                                   ALL_VERSIONS,1)
               er_message='stop requested from Cla_objt SIID25'
               call end_program(er_message)
            ENDIF
            INQUIRE(UNIT=13,OPENED=NEW_CL_UNIT_OPENED)
            IF(.NOT. NEW_CL_UNIT_OPENED) THEN
               WRITE(4,*) "READ ERROR IN EXPANSION OPERATIONS FILE"
               WRITE(4,*) '*** line 2789 CLA_OBJT.FOR ***'
               er_message='See WARNING MESSAGES-cla_objt-5'
               call end_program(er_message)
            ENDIF
            file_name=get_filename_from_unit(13)
            call write_log_entry("cla_objt:0102", "Reading from " //
     +          trim(file_name) // " (unit 13).")

           READ(13,REC=POINTER) DELETE,UNITNM(NUNITS),
     +                     CL_LOAD_TYPE,
     +                     TEMP_EXPENSE_ASSIGNMENT,  ! EXPENSE_ASSIGNMENT(NUNITS),
     +                     TEMP_EXPENSE_COLLECTION,  ! EXPENSE_COLLECTION(NUNITS),
     +                     GENGRP(NUNITS),foshyd.CAP_FRAC_OWN(NUNITS),
     +                     ON_LINE_MONTH_TEMP,OFF_LINE_MONTH(NUNITS),
     +                     FUEL_MIX_PTR(NUNITS), ! FUELMX(NUNITS), REPLACED 5/17/01 MSG
     +                     PBTUCT(NUNITS),PFESCR(NUNITS),SBTUCT(NUNITS),
     +                     SFESCR(NUNITS),
     +                     FUELADJ(NUNITS),EFOR(NUNITS),
     +                     (MNRATE(NUNITS,M),M=1,12),
     +                     VCPMWH_IN(NUNITS),OMESCR(NUNITS),
     +                     FIXED_COST_IN(NUNITS),
     +                     FIXED_COST_ESCALATOR(NUNITS),
     +                     DISPADJ(NUNITS),HR_FACTOR(NUNITS),
     +                     (INPUT_MW(M,NUNITS),M=1,2),
     +                     foshyd.CAP_PLANNING_FAC(NUNITS),
     +                     (COEFF(M,NUNITS),M=1,3),
     +                     CL_AI_CAPACITY_RATE(NUNITS),
     +                     CL_AI_CAPACITY_ESCALATOR(NUNITS),
     +                     CL_AI_ENERGY_RATE(NUNITS),
     +                     CL_AI_ENERGY_ESCALATOR(NUNITS),
     +                     P_SO2(NUNITS),P_NOX(NUNITS),
     +                     P_PARTICULATES(NUNITS),
     +                     LOCAL_CL_POOL_FRAC_OWN(NUNITS),
     +                     P_EMIS_OTH2(NUNITS),
     +                     P_EMIS_OTH3(NUNITS),
     +                     DISPADJ2(NUNITS),
     +                     CL_RESOURCE_ID(NUNITS),
     +                     FUEL_SUPPLY_ID(NUNITS),
     +                     P_NOX_BK2(NUNITS),
     +                     SEC_FUEL_EMISS_PTR(NUNITS),
     +                     EMISS_FUEL_COST(NUNITS),
     +                     EMISS_FUEL_ESCAL(NUNITS),
     +                     EMISS_FUEL_EMISS_PTR(NUNITS),
     +                     EMISS_BLENDING_RATE(NUNITS),
     +                     PRIM_FUEL_EMISS_PTR(NUNITS),
     +                     TEMP_PRIM_FUEL_TYPE_STR,
     +                     TEMP_SEC_FUEL_TYPE,  ! SEC_FUEL_TYPE(NUNITS),
     +                     TEMP_EMISS_FUEL_TYPE,  ! EMISS_FUEL_TYPE(NUNITS),
     +                     TIE_CONSTRAINT_GROUP(NUNITS),
     +                     MONTHLY_CAPACITY_POINTER(NUNITS),
     +                     ANNUAL_CL_FIXED_COST(NUNITS),
     +                     ANNUAL_CL_FIXED_COST_ESC(NUNITS),
     +                     MAINT_DAYS(NUNITS),
     +                     DISPATCH_MULT(NUNITS),
     +                     EXCESS_ENERGY_SALES(NUNITS),
     +                     AI_CL_REMAINING_LIFE(NUNITS),
     +                     AI_CL_TAX_LIFE(NUNITS),
     +                     AI_CL_ADR_LIFE(NUNITS),
     +                     ASSET_CLASS_NUM(NUNITS),
     +                     ASSET_CLASS_VECTOR(NUNITS),
     +                     INTRA_COMPANY_CLASS_ID(NUNITS),
     +                     TEMP_INTRA_COMPANY_TRANSACTION, ! INTRA_COMPANY_TRANSACTION(NUNITS),
     +                     SPECIAL_UNIT_ID(NUNITS),
     +                     MINIMUM_CAPACITY_FACTOR(NUNITS),
     +                     MAXIMUM_CAPACITY_FACTOR(NUNITS),
     +                     TRANSACTION_GROUP_ID(NUNITS),
     +                     DAY_TYPE_ID(NUNITS),
     +                     TEMP_UNIT_ACTIVE,
     +                     BASECASE_PLANT_ID(NUNITS),
     +                     BASECASE_UNIT_ID,
     +                     BASECASE_MARKET_AREA_ID(NUNITS),
     +                     BASECASE_TRANS_AREA_ID,
     +                     BASECASE_PLANT_NERC_SUB_ID,
     +                     BASECASE_PLANT_OWNER_ID,
     +                     TEMP_UTILITY_OWNED, ! UTILITY_OWNED,
     +                     MONTHLY_FUEL_INDEX(NUNITS),
     +                     START_UP_COSTS(NUNITS),
     +                     TEMP_START_UP_LOGIC, ! START_UP_LOGIC(NUNITS),
     +                     RAMP_RATE(NUNITS),
     +                     MIN_DOWN_TIME(NUNITS),
     +                     TEMP_REPORT_THIS_UNIT, ! REPORT_THIS_UNIT(NUNITS),
     +                     P_FUEL_DELIVERY_1(NUNITS),
     +                     P_FUEL_DELIVERY_2(NUNITS),
     +                     P_FUEL_DELIVERY_3(NUNITS),
     +                     MIN_UP_TIME(NUNITS),
     +                     HESI_UNIT_ID_NUM(NUNITS), ! PLACEHOLDER
     +                     FOR_FREQUENCY(NUNITS),
     +                     FOR_DURATION(NUNITS),
     +                     WINTER_TOTAL_CAPACITY,
     +                     TEMP_CONTRIBUTES_TO_SPIN, ! CONTRIBUTES_TO_SPIN(NUNITS),
     +                     H2_UNIT_ID_NUM(NUNITS),
     +                     POWERDAT_PLANT_ID(NUNITS),
     +                     TEMP_APPLY_NOX_SEASON_DATE, ! APPLY_NOX_SEASON_DATE(NUNITS),
     +                     NOX_SEASON_DATE(NUNITS),
     +                     RAMP_DOWN_RATE(NUNITS),
     +                     NOX_CONTROL_PERCENT(NUNITS),
     +                     NOX_CONTROL_DATE(NUNITS),
     +                     EMERGENCY_CAPACITY(NUNITS),
     +                     EMERGENCY_HEATRATE(NUNITS),
     +                     TEMP_MARKET_RESOURCE, ! MARKET_RESOURCE(NUNITS),
     +                     PRIMARY_MOVER_STR(NUNITS),
     +                     COUNTY,
     +                     STATE_PROVINCE,
     +                     NOX_VOM(NUNITS),
     +                     NOX_FOM(NUNITS),
     +                     S_FUEL_DELIVERY(NUNITS),
     +                     S_FUEL_DELIVERY_2(NUNITS),
     +                     s_fuel_delivery_3(NUNITS),
     +                     CONSTANT_POLY,
     +                     FIRST_POLY,
     +                     SECOND_POLY,
     +                     THIRD_POLY,
     +                     TEMP_USE_POLY_HEAT_RATES, ! USE_POLY_HEAT_RATES,
     +                     NEWGEN_UNIT_STATUS,
     +                     MONTHLY_MUST_RUN_VECTOR(NUNITS),
     +                     EMISSION_MARKET_LINK(NUNITS),
     +                     TEMP_WVPA_RATE_TRACKER, ! WVPA_RATE_TRACKER(NUNITS),
     +                     TEMP_ALLOW_DECOMMIT, ! ALLOW_DECOMMIT(NUNITS),
     +                     MIN_SPIN_CAP(NUNITS),
     +                     MAX_SPIN_CAP(NUNITS),
     +                     TEMP_WVPA_RES_TRACKER, ! WVPA_RES_TRACKER(NUNITS),
     +                     TEMP_WVPA_FUEL_TRACKER, !WVPA_FUEL_TRACKER(NUNITS),
     +                     TEMP_MONTE_OR_FREQ, ! MONTE_OR_FREQ(NUNITS),
     +                     TEMP_WVPA_MEM_TRACKER, ! WVPA_MEM_TRACKER(NUNITS),
     +                     MONTHLY_DECOMMIT_VECTOR(NUNITS),
     +                     TRANS_BUS_ID(NUNITS),
     +                     SOX_CONTROL_PERCENT(NUNITS),
     +                     SOX_CONTROL_DATE(NUNITS),
     +                     SOX_VOM(NUNITS),
     +                     SOX_FOM(NUNITS),
     +                     LATITUDE(NUNITS),
     +                     LONGITUDE(NUNITS),
     +                     MW_INSIDE_FENCE,
     +                     (INTER_BLOCKS(M,NUNITS),M=1,3),
     +                     FIRST_YEAR_DECOMM_AVAIALABLE(NUNITS),  ! 155
     +                     DECOMMISSIONING_BASE_YR_COST(NUNITS),
     +                     DECOM_CONT_COST_ESCALATION(NUNITS),
     +                     ANNUAL_ENERGY_PLANNING_FACTOR(NUNITS),   ! 158
     +                     TEMP_AGGREGATE_THIS_UNIT, ! AGGREGATE_THIS_UNIT(NUNITS),             ! 159
     +                     NAME_PLATE_CAPACITY(NUNITS),
     +                     FUEL_BLENDING_IS(NUNITS),   ! NO, FIXED BLEND, MODEL DETERMINED IF ONLY ONE FUEL ELSE USER OPTION
     +                     FuelRatio(1,Nunits),
     +                     FuelRatio(2,Nunits),
     +                     FuelRatio(3,Nunits),         ! 164
     +                     FuelRatio(4,Nunits),
     +                     FuelRatio(5,Nunits),         ! 166
     +                     BlendableFuelsPtr(1,Nunits), ! 167
     +                     BlendableFuelsPtr(2,Nunits), ! 168
     +                     BlendableFuelsPtr(3,Nunits),
     +                     BlendableFuelsPtr(4,Nunits),
     +                     FuelTransportationCost(1,Nunits),
     +                     FuelTransportationCost(2,Nunits),  ! 172
     +                     FuelTransportationCost(3,Nunits),
     +                     FuelTransportationCost(4,Nunits),
     +                     BlendedEnthalpyUp(Nunits),
     +                     BlendedEnthalpyLo(Nunits),
     +                     BlendedSO2Up(Nunits),                  ! 177
     +                     BettermentProjectID(Nunits),
     +                     DECOM_CONTINUING_COST(Nunits),
     +                     DECOM_CONT_COST_ESCALATION(Nunits),
     +                     EnrgPatternPointer(Nunits), ! 181
     +                     TEMP_EMISSION_DATA_UNITS, ! EMISSION_DATA_UNITS(Nunits),
     +                     EmissRedRate(1,Nunits),
     +                     EmissRedRate(2,Nunits),
     +                     EmissRedRate(3,Nunits),
     +                     EmissRedRate(4,Nunits),              ! 186
     +                     EmissRedRate(5,Nunits),
     +                     EmissMaxRate(1,Nunits),
     +                     EmissMaxRate(2,Nunits),
     +                     EmissMaxRate(3,Nunits),
     +                     EmissMaxRate(4,Nunits),
     +                     EmissMaxRate(5,Nunits),
     +                     MaxEnergyLimit(1,Nunits),
     +                     MaxEnergyLimit(2,Nunits),
     +                     MaxEnergyLimit(3,Nunits),
     +                     TECH_TYPE(Nunits),
     +                     TEMP_LINKED_BETTERMENT_OPTION, ! LINKED_BETTERMENT_OPTION(Nunits),
     +                     CO2_CONTROL_PERCENT(NUNITS),
     +                     HG_CONTROL_PERCENT(NUNITS),
     +                     OTHER3_CONTROL_PERCENT(NUNITS),
     +                     CO2_CONTROL_DATE(NUNITS),
     +                     HG_CONTROL_DATE(NUNITS),
     +                     OTHER3_CONTROL_DATE(NUNITS),
     +                     CO2_VOM(NUNITS),
     +                     CO2_FOM(NUNITS),
     +                     HG_VOM(NUNITS),
     +                     HG_FOM(NUNITS),
     +                     OTHER3_VOM(NUNITS),
     +                     OTHER3_FOM(NUNITS),
     +                     MARKET_FLOOR(NUNITS),
     +                     MARKET_CEILING(NUNITS),
     +                     START_UP_COSTS_ESCALATION(NUNITS),  ! 212
     +                     CAPACITY_MARKET_TYPE(NUNITS),
     +                     CAPACITY_MARKET_MONTH(NUNITS),
     +                     capacity_market_pointer_acl(NUNITS),
     +                     CAPACITY_MARKET_COST_ASSIGN(NUNITS),
     +                     CAPACITY_MARKET_EXP_COLLECT(NUNITS),
     +                     CAPACITY_MARKET_COIN_ADJ_FACT(NUNITS),
     +                     TEMP_PRIMARY_FUEL_CATEGORY,
     +                     TEMP_SECONDARY_FUEL_CATEGORY,
     +                     TEMP_EMISSIONS_FUEL_CATEGORY,
     +                     RPS_CONTRIBUTION_PERCENT(NUNITS),
     +                     TEMP_RETIREMENT_CANDIDATE,
     +                     TEMP_RETROFIT_CANDIDATE,
     +                     RETROFIT_PROJECT_ID(NUNITS),
     +                     CO2_BASIN_NAME,
     +                     CO2_PIPELINE_DISTANCE(NUNITS),
     +                     STATE_PROV_NAME, ! 228
     +                     CO2_RETRO_HEAT_MULT(NUNITS),
     +                     CO2_RETRO_CAP_MULT(NUNITS),
     +                     THERMAL_GUID,
     +                     THERMAL_PARENT_ID, ! 232
     +                     THERMAL_AGGREGATED_UNIT,
     +                     FIRST_RETIREMENT_YEAR(NUNITS),
     +                     UNIT_TYPE,
     +                     UNIT_TYPE_CATEGORY,
     +                     RPS_PROGRAM_NUMBER(NUNITS)
       if (cla_bad(int(nunits))) then
        call end_program("cla_objt:00014 - capacity_market_" //
     + "pointer_acl(" //trim(itos(int(nunits))) // ") is a " //
     + "bad value: " //
     + trim(itos(int(capacity_market_pointer_acl(nunits)))))

       endif
!      ! second array read

            IF(INDEX(TEMP_PRIMARY_FUEL_CATEGORY,"Coal")/=0)THEN
               EXPANSION_UNIT_LOCATION(NUNITS) = POINTER
            ENDIF
!
! 10/03/02. FOR REGIONAL CONSOLIDATION. NOTE REASSIGNMENT.
!
            TRANSACTION_GROUP_ID(NUNITS) =
     +                GET_BELONGS_TO_GROUP(TRANSACTION_GROUP_ID(NUNITS))
!
!
            TRANS_ID = TRANSACTION_GROUP_ID(NUNITS)
            IF(DELETE > 7 .OR. TEMP_UNIT_ACTIVE(1:1) == 'F') THEN

               NUNITS = NUNITS - 1
!
               RETURN
            ENDIF
            FIRST_RETIREMENT_YEAR(NUNITS) =
     +                    100*(FIRST_RETIREMENT_YEAR(NUNITS) - 1900)
            CAP_MARKET_MONTH_NO(NUNITS) =
     +          CAP_MARKET_MONTH_NO_INDEX(CAPACITY_MARKET_MONTH(NUNITS))
! CONVERT TO PERCENT

            IF(INPUT_MW(2,NUNITS) > 1. .AND. MW_INSIDE_FENCE > .1) THEN
               PERCENT_INSIDE_FENCE = MIN(1.,
     +                               MW_INSIDE_FENCE/INPUT_MW(2,NUNITS))
            ELSE
               PERCENT_INSIDE_FENCE = 0.
            ENDIF
!
            IF(RUN_TRANSACT .AND.
     +                  (.NOT.  TRANS_GROUP_ACTIVE_SWITCH(TRANS_ID) .OR.
     +                   .NOT. IN_ACTIVE_THERMAL_MARKET_AREA(
     +                         BASECASE_MARKET_AREA_ID(NUNITS))) .OR.
     +                                       PERCENT_INSIDE_FENCE > .995
     +                                                            ) THEN
               WRITE(4,*) UNITNM(NUNITS)," was not added to expansion"
               WRITE(4,*) "because it did not have a valid Transaction"
               WRITE(4,*) "Group ID = ",TRANSACTION_GROUP_ID(NUNITS)

               NUNITS = NUNITS - 1
!
               RETURN
            ENDIF

            IF(PERCENT_INSIDE_FENCE > .005) THEN
             foshyd.CAP_FRAC_OWN(NUNITS) = foshyd.CAP_FRAC_OWN(NUNITS) *
     +                                         (1.-PERCENT_INSIDE_FENCE)
            ENDIF

            IF(EMISSION_MARKET_LINK(NUNITS) == -9999) THEN
               EMISSION_MARKET_LINK(NUNITS) = R_ASSET_CLASS
            ENDIF
!
            POINTER_FOR_NEW_CL_UNIT(NUNITS) = R_POINTER
!
! ADDED SO THAT SP COMPATIBLE CL FILE OUTPUT
!
            EXPENSE_ASSIGNMENT(NUNITS) = TEMP_EXPENSE_ASSIGNMENT(1:1)
            EXPENSE_COLLECTION(NUNITS) = TEMP_EXPENSE_COLLECTION(1:1)
            SEC_FUEL_TYPE(NUNITS) = TEMP_SEC_FUEL_TYPE(1:1)
            EMISS_FUEL_TYPE(NUNITS) = TEMP_EMISS_FUEL_TYPE(1:1)
            INTRA_COMPANY_TRANSACTION(NUNITS) =
     +                               TEMP_INTRA_COMPANY_TRANSACTION(1:1)
            START_UP_LOGIC(NUNITS) = TEMP_START_UP_LOGIC(1:1)
            REPORT_THIS_UNIT(NUNITS) = TEMP_REPORT_THIS_UNIT(1:1)
            CONTRIBUTES_TO_SPIN(NUNITS) = TEMP_CONTRIBUTES_TO_SPIN(1:1)
            APPLY_NOX_SEASON_DATE(NUNITS) =
     +                                   TEMP_APPLY_NOX_SEASON_DATE(1:1)
            MARKET_RESOURCE(NUNITS) = TEMP_MARKET_RESOURCE(1:1)
            USE_POLY_HEAT_RATES = TEMP_USE_POLY_HEAT_RATES(1:1)
            WVPA_RATE_TRACKER(NUNITS) = TEMP_WVPA_RATE_TRACKER(1:1)
            ALLOW_DECOMMIT(NUNITS) = TEMP_ALLOW_DECOMMIT(1:1)
            WVPA_RES_TRACKER(NUNITS) = TEMP_WVPA_RES_TRACKER(1:1)
            WVPA_FUEL_TRACKER(NUNITS) = TEMP_WVPA_FUEL_TRACKER(1:1)
            MONTE_OR_FREQ(NUNITS) = TEMP_MONTE_OR_FREQ(1:1)
            WVPA_MEM_TRACKER(NUNITS) = TEMP_WVPA_MEM_TRACKER(1:1)
            AGGREGATE_THIS_UNIT(NUNITS) = TEMP_AGGREGATE_THIS_UNIT(1:1)
            EMISSION_DATA_UNITS(NUNITS) = TEMP_EMISSION_DATA_UNITS(1:1)
            LINKED_BETTERMENT_OPTION(NUNITS) =
     +                                TEMP_LINKED_BETTERMENT_OPTION(1:1)
            PRIMARY_FUEL_CATEGORY(NUNITS) =
     +                                   TEMP_PRIMARY_FUEL_CATEGORY(1:1)
            SECONDARY_FUEL_CATEGORY(NUNITS) =
     +                                 TEMP_SECONDARY_FUEL_CATEGORY(1:1)
            EMISSIONS_FUEL_CATEGORY(NUNITS) =
     +                                 TEMP_EMISSIONS_FUEL_CATEGORY(1:1)
            RETIREMENT_CANDIDATE(NUNITS) =
     +                                    TEMP_RETIREMENT_CANDIDATE(1:1)

            RETROFIT_CANDIDATE(NUNITS) =
     +                                    TEMP_RETROFIT_CANDIDATE(1:1)

            R_ON_LINE_MONTH = ON_LINE_MONTH_TEMP
            ON_LINE_MONTH = ABS(ON_LINE_MONTH_TEMP)
            WRITE(YEAR_CHR,'(I4)') UNIT_ON_LINE_YEAR
            IF(NUNITS < 10) THEN
               WRITE(NUNITS_CHR,'(I1)') NUNITS
            ELSEIF(NUNITS < 100) THEN
               WRITE(NUNITS_CHR,'(I2)') NUNITS
            ELSEIF(NUNITS < 1000) THEN
               WRITE(NUNITS_CHR,'(I3)') NUNITS
            ELSEIF(NUNITS < 1000) THEN
               WRITE(NUNITS_CHR,'(I4)') NUNITS
            ELSE
               WRITE(NUNITS_CHR,'(I5)') NUNITS
            ENDIF

            MARKETSYM_UNIT = MIN(14000,MAX_CL_UNITS,NUNITS)
            MARKETSYM_UNIT_NAME(MARKETSYM_UNIT) = UNITNM(NUNITS)

            UNITNM(NUNITS) = YEAR_CHR(3:)//'/'//trim(NUNITS_CHR)//
     +                                     ' '//trim(UNITNM(NUNITS))

            TG = GET_TRANS_GROUP_POSITION(TRANSACTION_GROUP_ID(NUNITS))

            IF(((CAPACITY_PLANNING_METHOD() == 'MX' .AND.
     +                         .NOT. GREEN_MRX_METHOD() == 'GX') .OR.
     +          (CAPACITY_PLANNING_METHOD() == 'MX' .AND.
     +             GREEN_MRX_METHOD() == 'GX' .AND. GRX_ITERATIONS > 0))
     +         .AND.   (SAVE_SCENARIO_MRX_PLANS == 'A' .OR.
     +               (SAVE_SCENARIO_MRX_PLANS == 'G' .AND.
     +                       SAVE_MRX_EXPANSION_PLAN(TG)=='T'))) THEN
               IF(END_POINT > END_POINT_CHECK .AND. .FALSE. .AND.
     +                                .NOT. MRX_TEXT_FILE_OPEN) THEN
                  CPO_REC = RPTREC(9979_2)
                  WRITE(9979,'(A1)',REC=CPO_REC) ENDFILE_MARK
                  CLOSE(9979,IOSTAT=IOS)
                  END_POINT_CHECK = END_POINT
                  MRX_TEXT_FILE_OPEN = .TRUE.
               ENDIF
               INQUIRE(UNIT=9979,OPENED=MRX_TEXT_FILE_OPEN)
               IF(.NOT. MRX_TEXT_FILE_OPEN) THEN
                  SEQU_NO = MRX_SEQUENCE_NUMBER + END_POINT - 1
                  file_name=get_mrx_full_filename(
     +                  mrx_expansion_plan_file_code,sequ_no)

                  OPEN(9979,FILE=FILE_NAME,ACCESS='DIRECT',
     +                     FORM='FORMATTED',RECL=2048, STATUS='REPLACE')
                  WRITE(9979,"(A,I4,',')",REC=1)"9,    ,",YEAR+BASE_YEAR
                  CPO_REC = 2
                  YEAR_CHECK = YEAR
                  IREC_SAVED = RPTREC(9979_2,SAVE_REC=CPO_REC,
     +                            REC_LENGHT=2048_2,FILE_NAME=FILE_NAME)
                  END_POINT_CHECK = END_POINT
                  MRX_TEXT_FILE_OPEN = .FALSE.
                  RESET_REC = 2
               ELSEIF(RPTREC(9979_2,CURRENT_REC=.TRUE.)==2) THEN
                  WRITE(9979,"(A,I4,',')",REC=1)"9,    ,",YEAR+BASE_YEAR
                  YEAR_CHECK = YEAR
               ELSEIF(YEAR > YEAR_CHECK .OR.
     +                RPTREC(9979_2,CURRENT_REC=.TRUE.)==RESET_REC) THEN
                  RESET_REC = RPTREC(9979_2)
                  WRITE(9979,"(A,I4,',')",REC=RESET_REC)
     +                                    CRLF//"7,    ,",YEAR+BASE_YEAR

                  YEAR_CHECK = YEAR
               ENDIF
               RECLN = '1,'//TRIM(CONVERT_2_STR(UNITNM(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(CL_LOAD_TYPE))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(TEMP_EXPENSE_ASSIGNMENT))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(TEMP_EXPENSE_COLLECTION))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(GENGRP(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(foshyd.CAP_FRAC_OWN(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(ON_LINE_MONTH_TEMP))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(UNIT_ON_LINE_YEAR))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(OFF_LINE_MONTH(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(UNIT_ON_LINE_YEAR
     +                                               + OPERATION_LIFE)) ! 10
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(FUEL_MIX_PTR(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(PBTUCT(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(PFESCR(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(SBTUCT(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(SFESCR(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(FUELADJ(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(EFOR(NUNITS)))
               DO M = 1, 12
                  RECLN = TRIM(RECLN)//','//
     +                             TRIM(CONVERT_2_STR(MNRATE(NUNITS,M)))
               ENDDO

               if(cla_bad(int(nunits))) then
                   call end_program("cla_objt:00045 - " //
     + "capacity_market_pointer_acl(" // trim(itos(int(nunits))) //
     + " is an invalid value (" //trim(itos(int(
     + capacity_market_pointer_acl(nunits)))))
               endif


               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(VCPMWH_IN(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(OMESCR(NUNITS)))               ! 31
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(FIXED_COST_IN(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(FIXED_COST_ESCALATOR(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(DISPADJ(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(HR_FACTOR(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(INPUT_MW(1,NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(INPUT_MW(2,NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +          TRIM(CONVERT_2_STR(foshyd.CAP_PLANNING_FAC(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(COEFF(1,NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(COEFF(2,NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(COEFF(3,NUNITS)))
               RECLN = TRIM(RECLN)//','//','//   ! 42 extra comment is for the comment space
     +            TRIM(CONVERT_2_STR(P_SO2(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(P_NOX(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(P_PARTICULATES(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(LOCAL_CL_POOL_FRAC_OWN(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(P_EMIS_OTH2(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(P_EMIS_OTH3(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            "False"  !  PHASE_I_STR))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(DISPADJ2(NUNITS)))  ! 50
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(CL_RESOURCE_ID(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(FUEL_SUPPLY_ID(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(P_NOX_BK2(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(SEC_FUEL_EMISS_PTR(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(EMISS_FUEL_COST(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(EMISS_FUEL_ESCAL(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(EMISS_FUEL_EMISS_PTR(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(EMISS_BLENDING_RATE(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(PRIM_FUEL_EMISS_PTR(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(TEMP_PRIM_FUEL_TYPE_STR)           ! 60
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(TEMP_SEC_FUEL_TYPE))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(TEMP_EMISS_FUEL_TYPE))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(TIE_CONSTRAINT_GROUP(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(MONTHLY_CAPACITY_POINTER(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(CL_AI_CAPACITY_RATE(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(CL_AI_CAPACITY_ESCALATOR(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(CL_AI_ENERGY_RATE(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(CL_AI_ENERGY_ESCALATOR(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(AI_CL_REMAINING_LIFE(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(ANNUAL_CL_FIXED_COST(NUNITS))) ! 70
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(ANNUAL_CL_FIXED_COST_ESC(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(MAINT_DAYS(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(DISPATCH_MULT(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(EXCESS_ENERGY_SALES(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(AI_CL_TAX_LIFE(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(AI_CL_ADR_LIFE(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(ASSET_CLASS_NUM(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(ASSET_CLASS_VECTOR(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(INTRA_COMPANY_CLASS_ID(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(TEMP_INTRA_COMPANY_TRANSACTION)) ! 80
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(SPECIAL_UNIT_ID(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(MINIMUM_CAPACITY_FACTOR(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(MAXIMUM_CAPACITY_FACTOR(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(TRANSACTION_GROUP_ID(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(DAY_TYPE_ID(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(TEMP_UNIT_ACTIVE))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(BASECASE_PLANT_ID(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(BASECASE_UNIT_ID))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(BASECASE_MARKET_AREA_ID(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(BASECASE_TRANS_AREA_ID))  ! 90
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(BASECASE_PLANT_NERC_SUB_ID))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(BASECASE_PLANT_OWNER_ID))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(TEMP_UTILITY_OWNED))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(MONTHLY_FUEL_INDEX(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(START_UP_COSTS(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(TEMP_START_UP_LOGIC))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(RAMP_RATE(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(MIN_DOWN_TIME(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(TEMP_REPORT_THIS_UNIT))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(P_FUEL_DELIVERY_1(NUNITS)))   ! 100
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(P_FUEL_DELIVERY_2(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(P_FUEL_DELIVERY_3(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(MIN_UP_TIME(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(HESI_UNIT_ID_NUM(NUNITS))) ! PLACEHOLDER
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(FOR_FREQUENCY(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(FOR_DURATION(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(WINTER_TOTAL_CAPACITY))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(TEMP_CONTRIBUTES_TO_SPIN))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(H2_UNIT_ID_NUM(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(POWERDAT_PLANT_ID(NUNITS)))  ! 110
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(TEMP_APPLY_NOX_SEASON_DATE))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(NOX_SEASON_DATE(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(RAMP_DOWN_RATE(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(NOX_CONTROL_PERCENT(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(NOX_CONTROL_DATE(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(EMERGENCY_CAPACITY(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(EMERGENCY_HEATRATE(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(TEMP_MARKET_RESOURCE))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(PRIMARY_MOVER_STR(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(COUNTY))                     ! 120
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(STATE_PROVINCE))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(NOX_VOM(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(NOX_FOM(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(S_FUEL_DELIVERY(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(S_FUEL_DELIVERY_2(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(S_FUEL_DELIVERY_3(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(CONSTANT_POLY))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(FIRST_POLY))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(SECOND_POLY))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(THIRD_POLY))               ! 130
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(TEMP_USE_POLY_HEAT_RATES))
               RECLN = TRIM(RECLN)//','//
     +            "07-Forecasted"  ! NEWGEN_UNIT_STATUS,
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(MONTHLY_MUST_RUN_VECTOR(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(EMISSION_MARKET_LINK(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(TEMP_WVPA_RATE_TRACKER))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(TEMP_ALLOW_DECOMMIT))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(MIN_SPIN_CAP(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(MAX_SPIN_CAP(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(TEMP_WVPA_RES_TRACKER))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(TEMP_WVPA_FUEL_TRACKER))   ! 140
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(TEMP_MONTE_OR_FREQ))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(TEMP_WVPA_MEM_TRACKER))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(MONTHLY_DECOMMIT_VECTOR(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(TRANS_BUS_ID(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(SOX_CONTROL_PERCENT(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(SOX_CONTROL_DATE(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(SOX_VOM(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(SOX_FOM(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(LATITUDE(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(LONGITUDE(NUNITS)))        ! 150
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(MW_INSIDE_FENCE))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(INTER_BLOCKS(1,NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(INTER_BLOCKS(2,NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(INTER_BLOCKS(3,NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(
     +                           FIRST_YEAR_DECOMM_AVAIALABLE(NUNITS)))  ! 155
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(
     +                           DECOMMISSIONING_BASE_YR_COST(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(
     +                       DECOM_CONT_COST_ESCALATION(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(
     +                          ANNUAL_ENERGY_PLANNING_FACTOR(NUNITS)))   ! 158
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(TEMP_AGGREGATE_THIS_UNIT))             ! 159
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(NAME_PLATE_CAPACITY(NUNITS)))    ! 160
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(FUEL_BLENDING_IS(NUNITS)))   ! NO)) FIXED BLEND)) MODEL DETERMINED IF ONLY ONE FUEL ELSE USER OPTION
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(FuelRatio(1,Nunits)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(FuelRatio(2,Nunits)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(FuelRatio(3,Nunits)))         ! 164
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(FuelRatio(4,Nunits)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(FuelRatio(5,Nunits)))         ! 166
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(BlendableFuelsPtr(1,Nunits))) ! 167
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(BlendableFuelsPtr(2,Nunits))) ! 168
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(BlendableFuelsPtr(3,Nunits)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(BlendableFuelsPtr(4,Nunits)))  ! 170
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(
     +                               FuelTransportationCost(1,Nunits)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(
     +                               FuelTransportationCost(2,Nunits)))  ! 172
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(
     +                               FuelTransportationCost(3,Nunits)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(
     +                               FuelTransportationCost(4,Nunits)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(BlendedEnthalpyUp(Nunits)))      ! 175
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(BlendedEnthalpyLo(Nunits)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(BlendedSO2Up(Nunits)))                  ! 177
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(BettermentProjectID(Nunits)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(DECOM_CONTINUING_COST(Nunits)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(
     +                       DECOM_CONT_COST_ESCALATION(Nunits)))   ! 180
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(EnrgPatternPointer(Nunits))) ! 181
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(TEMP_EMISSION_DATA_UNITS))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(EmissRedRate(1,Nunits)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(EmissRedRate(2,Nunits)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(EmissRedRate(3,Nunits)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(EmissRedRate(4,Nunits)))              ! 186
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(EmissRedRate(5,Nunits)))    ! 187
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(EmissMaxRate(1,Nunits)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(EmissMaxRate(2,Nunits)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(EmissMaxRate(3,Nunits)))    ! 190
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(EmissMaxRate(4,Nunits)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(EmissMaxRate(5,Nunits)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(MaxEnergyLimit(1,Nunits)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(MaxEnergyLimit(2,Nunits)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(MaxEnergyLimit(3,Nunits)))  ! 195
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(TECH_TYPE(Nunits)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(TEMP_LINKED_BETTERMENT_OPTION))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(CO2_CONTROL_PERCENT(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(HG_CONTROL_PERCENT(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(OTHER3_CONTROL_PERCENT(NUNITS))) ! 200
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(CO2_CONTROL_DATE(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(HG_CONTROL_DATE(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(OTHER3_CONTROL_DATE(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(CO2_VOM(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(CO2_FOM(NUNITS)))   ! 205
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(HG_VOM(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(HG_FOM(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(OTHER3_VOM(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(OTHER3_FOM(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(MARKET_FLOOR(NUNITS)))  ! 210
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(MARKET_CEILING(NUNITS)))  ! 211
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(START_UP_COSTS_ESCALATION(NUNITS)))  ! 212
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(CAPACITY_MARKET_TYPE(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(CAPACITY_MARKET_MONTH(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +         TRIM(CONVERT_2_STR(capacity_market_pointer_acl(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(
     +                             CAPACITY_MARKET_COST_ASSIGN(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(
     +                             CAPACITY_MARKET_EXP_COLLECT(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(
     +                           CAPACITY_MARKET_COIN_ADJ_FACT(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(
     +                           TEMP_PRIMARY_FUEL_CATEGORY))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(
     +                           TEMP_SECONDARY_FUEL_CATEGORY))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(
     +                           TEMP_EMISSIONS_FUEL_CATEGORY))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(
     +                           RPS_CONTRIBUTION_PERCENT(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(
     +                           TEMP_RETIREMENT_CANDIDATE))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(
     +                           TEMP_RETROFIT_CANDIDATE))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(
     +                     RETROFIT_PROJECT_ID(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(
     +                     CO2_BASIN_NAME))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(
     +                     CO2_PIPELINE_DISTANCE(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(
     +                     STATE_PROV_NAME))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(
     +                     CO2_RETRO_HEAT_MULT(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(
     +                     CO2_RETRO_CAP_MULT(NUNITS)))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(
     +                     THERMAL_GUID))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(
     +                     THERMAL_PARENT_ID))
               RECLN = TRIM(RECLN)//','//
     +            TRIM(CONVERT_2_STR(
     +                     THERMAL_AGGREGATED_UNIT))
               RECLN = CRLF//TRIM(RECLN)//',,,,,,,,,,,,,,,,,,,,,'
               CPO_REC = RPTREC(9979_2)
               WRITE(9979,'(A)',REC=CPO_REC) TRIM(RECLN)
            ENDIF

            IF(LOCAL_MW(1) > 0. .AND. LOCAL_MW(2) > 0.) THEN
               IF(USE_POLY_HEAT_RATES == 'T' .AND.
     +                                         .NOT. TURN_OFF_POLY) THEN

                  COEFF(1,NUNITS) = 1000. *
     +               (CONSTANT_POLY +
     +                FIRST_POLY * LOCAL_MW(1) +
     +                SECOND_POLY *
     +                           LOCAL_MW(1)*LOCAL_MW(1) +
     +                THIRD_POLY *
     +                     LOCAL_MW(1)*LOCAL_MW(1)*
     +                                              LOCAL_MW(1))/
     +                                                LOCAL_MW(1)
                  COEFF(2,NUNITS) = 1000. *
     +                (FIRST_POLY +
     +                2. * SECOND_POLY * LOCAL_MW(1) +
     +                3. * THIRD_POLY *
     +                            LOCAL_MW(1)*LOCAL_MW(1))
                  COEFF(3,NUNITS) = 1000. *
     +                (FIRST_POLY +
     +                2. * SECOND_POLY * LOCAL_MW(2) +
     +                3. * THIRD_POLY *
     +                            LOCAL_MW(2)*LOCAL_MW(2))

               ENDIF
!
!
! 1/17/03.
! A+BX+CX^2
!
               IF(LOCAL_MW(2) > LOCAL_MW(1)) THEN
!
                  CUBIC_HEAT_CURVE(1,NUNITS) =
     +                     (COEFF(2,NUNITS)-COEFF(3,NUNITS))/
     +                     (-2.*(LOCAL_MW(2)-LOCAL_MW(1)))
                  CUBIC_HEAT_CURVE(2,NUNITS) =
     +                  COEFF(2,NUNITS) -
     +                             (COEFF(2,NUNITS) - COEFF(3,NUNITS)) *
     +                        LOCAL_MW(1)/
     +                     (-1.*(LOCAL_MW(2)-LOCAL_MW(1)))
                  CUBIC_HEAT_CURVE(3,NUNITS) =
     +                ((COEFF(1,NUNITS)-CUBIC_HEAT_CURVE(2,NUNITS))*
     +                               LOCAL_MW(1)) -
     +                        CUBIC_HEAT_CURVE(1,NUNITS)*
     +                             LOCAL_MW(1)*LOCAL_MW(1)
               ELSE
               ENDIF
            ELSE ! NEED TO CHECK THIS WITH TOM.
               CUBIC_HEAT_CURVE(2,NUNITS) = COEFF(1,NUNITS)
            ENDIF

            CUBIC_HEAT_CURVE(0,NUNITS) = CONSTANT_POLY
            CUBIC_HEAT_CURVE(2,NUNITS) = SECOND_POLY
            CUBIC_HEAT_CURVE(3,NUNITS) = THIRD_POLY
            IF(CONSTANT_POLY <= 0. .AND. FIRST_POLY <= 0. .AND.
     +                                           SECOND_POLY <= 0.) THEN
               IF(COEFF(1,NUNITS) > 0.) THEN
                  CUBIC_HEAT_CURVE(1,NUNITS) = COEFF(1,NUNITS)*.001
               ELSE
                  CUBIC_HEAT_CURVE(1,NUNITS) = 10.
               ENDIF
            ELSE
               CUBIC_HEAT_CURVE(1,NUNITS) = FIRST_POLY
            ENDIF

            TEMP_STATE = STATE_PROVINCE(1:2)
            TEMP_I2 = STATE_ID_LOOKUP(TEMP_STATE)
            State_Index(NUNITS) = TEMP_I2
            TEMP_I2 = STATE_ID_LOOKUP(STATE_PROVINCE)
            State_TG_Index(NUNITS) = TEMP_I2
            IF( TEMP_I2 > 0 .AND.
     +                          STATE_PROVINCE_INDEX(TEMP_I2) == 0) THEN
               MAX_STATE_PROVINCE_NO = MAX_STATE_PROVINCE_NO + 1
               STATE_PROVINCE_ADDRESS(MAX_STATE_PROVINCE_NO) = TEMP_I2
               STATE_PROVINCE_INDEX(TEMP_I2) = MAX_STATE_PROVINCE_NO
               STATE_PROVINCE_NAMES(MAX_STATE_PROVINCE_NO) =
     +                                                    STATE_PROVINCE
            ENDIF
            UNIT_STATE_PROVINCE_INDEX(NUNITS) =
     +                                     STATE_PROVINCE_INDEX(TEMP_I2)

            TEMP_I2 = STATE_2_GAS_REGION_LOOKUP(STATE_PROVINCE)
            IF(YES_GSP_IS_ST_TG(TEMP_I2)) THEN
               TEMP_STATE = STATE_PROVINCE
            ELSE
               TEMP_STATE = STATE_PROVINCE(1:2)
            ENDIF
            UNIT_GAS_REGION_INDEX(NUNITS) =
     +                             STATE_2_GAS_REGION_LOOKUP(TEMP_STATE)

            ON_LINE_YEAR = UNIT_ON_LINE_YEAR
            OFF_LINE_YEAR(NUNITS) = UNIT_ON_LINE_YEAR + OPERATION_LIFE
!
            ONLINE(NUNITS) = 100*(ON_LINE_YEAR-1900) + ON_LINE_MONTH
            OFLINE(NUNITS) = 100*(OFF_LINE_YEAR(NUNITS)-1900) +
     +                                            OFF_LINE_MONTH(NUNITS)
            IF(MAINT_DAYS(NUNITS) /= 0.)
     +                                SAVE_DETAILED_MAINTENANCE = .TRUE.
            ASSET_CLASS_NUM(NUNITS) = R_ASSET_CLASS
            ASSET_CLASS_VECTOR(NUNITS) = R_ALLOCATION_VECTOR

            CL_RESOURCE_ID(NUNITS) = R_CL_RESOURCE_ID ! 3/13/98. GAT.

            NOX_SEASON_DATE(NUNITS) = NOX_SEASON_DATE(NUNITS) + 10000

            NOX_CONTROL_PERCENT(NUNITS) =
     +             MIN(100.,MAX(0.,1.-NOX_CONTROL_PERCENT(NUNITS)/100.))
            NOX_CONTROL_DATE(NUNITS) = NOX_CONTROL_DATE(NUNITS) + 10000

            SOX_CONTROL_PERCENT(NUNITS) =
     +             MIN(100.,MAX(0.,1.-SOX_CONTROL_PERCENT(NUNITS)/100.))
            SOX_CONTROL_DATE(NUNITS) = SOX_CONTROL_DATE(NUNITS) + 10000
            CO2_CONTROL_PERCENT(NUNITS) =
     +             MIN(100.,MAX(0.,1.-CO2_CONTROL_PERCENT(NUNITS)/100.))
            CO2_CONTROL_DATE(NUNITS) = CO2_CONTROL_DATE(NUNITS) + 10000
            HG_CONTROL_PERCENT(NUNITS) =
     +             MIN(100.,MAX(0.,1.-HG_CONTROL_PERCENT(NUNITS)/100.))
            HG_CONTROL_DATE(NUNITS) = HG_CONTROL_DATE(NUNITS) + 10000
            OTHER3_CONTROL_PERCENT(NUNITS) =
     +             MIN(100.,
     +                   MAX(0.,1.-OTHER3_CONTROL_PERCENT(NUNITS)/100.))
            OTHER3_CONTROL_DATE(NUNITS) =
     +                               OTHER3_CONTROL_DATE(NUNITS) + 10000

            LOCAL_RESOURCE_ID = CL_RESOURCE_ID(NUNITS)
            IF(NUMBER_OF_RESOURCE_ID(LOCAL_RESOURCE_ID) == 0) THEN
               RESOURCE_ID_TO_UNIT(LOCAL_RESOURCE_ID) = NUNITS ! FIRST OCCURANCE
            ENDIF
            NUMBER_OF_RESOURCE_ID(LOCAL_RESOURCE_ID) =
     +                      NUMBER_OF_RESOURCE_ID(LOCAL_RESOURCE_ID) + 1
!
            IF(START_UP_LOGIC(NUNITS) /= 'F') THEN
               TOTAL_START_UP_UNITS = TOTAL_START_UP_UNITS + 1
               START_UP_INDEX(NUNITS) = TOTAL_START_UP_UNITS
            ENDIF
!
            CALL SET_ASSET_CLASSES(ASSET_CLASS_NUM(NUNITS),
     +                             NUMBER_OF_CAP_LIMITED_CLASSES,
     +                               MAX_CAP_LIMITED_CLASS_ID_NUM,
     +                             CAP_LIMITED_CLASS_POINTER)

            CALL UPC(TEMP_PRIM_FUEL_TYPE_STR,PRIM_FUEL_TYPE_STR)
            PRIMARY_MOVER(NUNITS) =
     +                        FUEL_TYPE_2_PRIM_MOVER(PRIM_FUEL_TYPE_STR)

            TRANS_ID = TRANSACTION_GROUP_ID(NUNITS)
            IF(TRANS_ID > 0 .AND. ON_LINE_YEAR <= LAST_STUDY_YEAR) THEN
               DAY_ID = DAY_TYPE_ID(NUNITS)
               MAX_TRANS_ID_USED = MAX(MAX_TRANS_ID_USED, TRANS_ID)
               IF(DAY_ID > 0 .AND. DAY_ID <= MAX_DAY_TYPES ) THEN
                  IF(DAY_TYPE_TRANS_GROUP_PAIR(DAY_ID,TRANS_ID) == 0)
     +                                                              THEN
                     MAX_TRANS_DATA_BASES = MAX_TRANS_DATA_BASES + 1
                     DAY_TYPE_TRANS_GROUP_PAIR(DAY_ID,TRANS_ID) =
     +                                              MAX_TRANS_DATA_BASES
                  ENDIF
!
                  DATA_BASE_POSITION(1,NUNITS) =
     +                        DAY_TYPE_TRANS_GROUP_PAIR(DAY_ID,TRANS_ID)
                  DATA_BASE_POSITION(2,NUNITS) =
     +                        DAY_TYPE_TRANS_GROUP_PAIR(DAY_ID,0)
               ELSEIF(DAY_ID < 0) THEN
                  DAY_TYPE_COUNTER = 1
                  DO
                     DAY_ID = INT(GET_VAR(FLOAT(DAY_TYPE_ID(NUNITS)),
     +                                DAY_TYPE_COUNTER,"FIND DAY TYPE"))
                     IF(DAY_ID <= 0 .OR. DAY_ID > MAX_DAY_TYPES) EXIT
                     IF(DAY_TYPE_TRANS_GROUP_PAIR(DAY_ID,TRANS_ID) == 0)
     +                                                              THEN
                        MAX_TRANS_DATA_BASES = MAX_TRANS_DATA_BASES + 1
                        DAY_TYPE_TRANS_GROUP_PAIR(DAY_ID,TRANS_ID) =
     +                                              MAX_TRANS_DATA_BASES
                     ENDIF
!     !
                     DATA_BASE_POSITION(2*DAY_TYPE_COUNTER-1,NUNITS) =
     +                        DAY_TYPE_TRANS_GROUP_PAIR(DAY_ID,TRANS_ID)
                     DATA_BASE_POSITION(2*DAY_TYPE_COUNTER,NUNITS) =
     +                               DAY_TYPE_TRANS_GROUP_PAIR(DAY_ID,0)
                     DAY_TYPE_COUNTER = DAY_TYPE_COUNTER + 1
                  ENDDO
               ELSEIF(DAY_ID == 0) THEN
                  DO M = 1, MAX_DAY_TYPES
                     IF(DAY_TYPE_TRANS_GROUP_PAIR(M,TRANS_ID) == 0) THEN
                        MAX_TRANS_DATA_BASES = MAX_TRANS_DATA_BASES + 1
                        DAY_TYPE_TRANS_GROUP_PAIR(M,TRANS_ID) =
     +                                              MAX_TRANS_DATA_BASES
                     ENDIF
                     DATA_BASE_POSITION(2*M-1,NUNITS) =
     +                             DAY_TYPE_TRANS_GROUP_PAIR(M,TRANS_ID)
                     DATA_BASE_POSITION(2*M,NUNITS) =
     +                                    DAY_TYPE_TRANS_GROUP_PAIR(M,0)
                  ENDDO
               ELSE
                  WRITE(4,*) "DAY TYPE DESIGNATION",DAY_ID
                  WRITE(4,*) "OUTSIDE THE RANGE OF POSSIBLE OUTCOMES"
                  WRITE(4,*) "MAXIMUM DAY TYPES DEFINED = ",
     +                                                     MAX_DAY_TYPES
                  WRITE(4,*) '*** line 3020 CLA_OBJT.FOR ***'
                  er_message='See WARNING MESSAGES-cla_objt-4'
                  call end_program(er_message)
               ENDIF
            ENDIF
!
            DISP_ADDER_ESCR(NUNITS) = PFESCR(NUNITS)
            PRIM_FUEL_TYPE(NUNITS) = PRIM_FUEL_TYPE_STR(1:1)
            NUC_FUEL_PRICES_FROM_FUEL_FILE(NUNITS) =
     +                      INDEX(PRIM_FUEL_TYPE_STR,'Nuc-NF') /= 0 .OR.
     +                           INDEX(PRIM_FUEL_TYPE_STR,'NUC-NF') /= 0
            LDTYPE(NUNITS) = CL_LOAD_TYPE(1:1)
            ECONOMY_TRANS_TYPE(NUNITS) = 'T'
            IF(CL_LOAD_TYPE(2:2) == 'S') ECONOMY_TRANS_TYPE(NUNITS)='S'
            IF(CL_LOAD_TYPE(2:2) == 'B') ECONOMY_TRANS_TYPE(NUNITS)='B'
            IF(CL_LOAD_TYPE(2:2) == 'N') ECONOMY_TRANS_TYPE(NUNITS)='N'

            IF(EFORS0) EFOR(NUNITS) = 0.
            IF(SORS0) THEN
               MAINT_DAYS(NUNITS) = 0
               DO M = 1, 12
                  MNRATE(NUNITS,M) = 0.
               ENDDO
            ENDIF
            PBTUCT_SAVE(NUNITS) = PBTUCT(NUNITS)
            SBTUCT_SAVE(NUNITS) = SBTUCT(NUNITS)
            FUELADJ_SAVE(NUNITS) = FUELADJ(NUNITS)
            VCPMWH_IN_SAVE(NUNITS) = VCPMWH_IN(NUNITS)
            FIXED_COST_SAVE(NUNITS) = FIXED_COST_IN(NUNITS)
            ANNUAL_CL_FIXED_COST_SAVE(NUNITS) =
     +                                      ANNUAL_CL_FIXED_COST(NUNITS)
            DISPATCH_MULT_SAVE(NUNITS) = DISPATCH_MULT(NUNITS)
            EXCESS_ENERGY_SALES_SAVE(NUNITS) =
     +                                       EXCESS_ENERGY_SALES(NUNITS)
            DISPADJ_SAVE(NUNITS) = DISPADJ(NUNITS)
            DISPADJ2_SAVE(NUNITS) = DISPADJ2(NUNITS)
            CL_AI_CAPACITY_RATE_SAVE(NUNITS)=CL_AI_CAPACITY_RATE(NUNITS)
            CL_AI_ENERGY_RATE_SAVE(NUNITS) = CL_AI_ENERGY_RATE(NUNITS)
            CL_AI_REMAINING_LIFE_SAVE(NUNITS) =
     +                                      AI_CL_REMAINING_LIFE(NUNITS)
            DISPADJ2_SAVE(NUNITS) = DISPADJ2(NUNITS)
            EMISS_FUEL_COST_SAVE(NUNITS) = EMISS_FUEL_COST(NUNITS)

            ADD_NEW_CL_UNIT = NUNITS

!

            SHADOW_UNIT_NUMBER(NUNITS) = 0
            IF(FUEL_SUPPLY_ID(NUNITS) > 0 .AND.
     +                    FUEL_MIX_PTR(NUNITS) > 0. .AND.
     +                               START_UP_LOGIC(NUNITS) == 'F') THEN ! FUELMX(NUNITS) >0.) THEN
               I = NUNITS
               NUNITS = I + 1
               SHADOW_UNIT_NUMBER(NUNITS) = I
               SHADOW_UNIT_NUMBER(I) = NUNITS
               UNITNM(NUNITS) = '+'//UNITNM(I)
               FUEL_MIX_PTR(NUNITS) = 0. ! FUELMX(NUNITS) = 0 replaced 5/17/01
               LDTYPE(NUNITS) = 'S'
               EXPENSE_ASSIGNMENT(NUNITS) = EXPENSE_ASSIGNMENT(I)
               EXPENSE_COLLECTION(NUNITS) = EXPENSE_COLLECTION(I)
               ASSET_CLASS_NUM(NUNITS) = ASSET_CLASS_NUM(I)
               ASSET_CLASS_VECTOR(NUNITS) = ASSET_CLASS_VECTOR(I)
               INTRA_COMPANY_CLASS_ID(NUNITS)=INTRA_COMPANY_CLASS_ID(I)
               INTRA_COMPANY_TRANSACTION(NUNITS) =
     +                                      INTRA_COMPANY_TRANSACTION(I)
               SPECIAL_UNIT_ID(NUNITS) = SPECIAL_UNIT_ID(I)
               MINIMUM_CAPACITY_FACTOR(NUNITS) =
     +                                        MINIMUM_CAPACITY_FACTOR(I)
               MAXIMUM_CAPACITY_FACTOR(NUNITS) =
     +                                        MAXIMUM_CAPACITY_FACTOR(I)
               TRANSACTION_GROUP_ID(NUNITS) = TRANSACTION_GROUP_ID(I)
               REPORT_THIS_UNIT(NUNITS) = REPORT_THIS_UNIT(I)
               ECONOMY_TRANS_TYPE(NUNITS) = ECONOMY_TRANS_TYPE(I)
               CL_CAP_AREA_LINKED(NUNITS) = CL_CAP_AREA_LINKED(I)
               GENGRP(NUNITS) = GENGRP(I)
               foshyd.CAP_FRAC_OWN(NUNITS) = foshyd.CAP_FRAC_OWN(I)
               ONLINE(NUNITS) = ONLINE(I)
               OFLINE(NUNITS) = OFLINE(I)
               OFF_LINE_YEAR(NUNITS) = OFF_LINE_YEAR(I)
               SBTUCT(NUNITS) = SBTUCT(I)
               SBTUCT(I) = 0.
               SFESCR(NUNITS) = SFESCR(I)
               SFESCR(I) = 0
               FUELADJ(NUNITS) = FUELADJ(I)
               EFOR(NUNITS) = EFOR(I)
               MAINT_DAYS(NUNITS) = MAINT_DAYS(I)
               DO M = 1, 12
                  MNRATE(NUNITS,M) = MNRATE(I,M)
               ENDDO
               VCPMWH_IN(NUNITS) = VCPMWH_IN(I)
               OMESCR(NUNITS) = OMESCR(I)
               FIXED_COST_IN(NUNITS) = 0.
               FIXED_COST_SAVE(NUNITS) = 0.
               ANNUAL_CL_FIXED_COST(NUNITS) = 0.
               ANNUAL_CL_FIXED_COST_SAVE(NUNITS) = 0.
               FIXED_COST_ESCALATOR(NUNITS) = 0.
               ANNUAL_CL_FIXED_COST_ESC(NUNITS) = 0.
               DISPADJ(NUNITS) = DISPADJ(I)
               DISPATCH_MULT(NUNITS) = DISPATCH_MULT(I)
               EXCESS_ENERGY_SALES(NUNITS) = EXCESS_ENERGY_SALES(I)
               HR_FACTOR(NUNITS) = HR_FACTOR(I)
               INPUT_MW(1,NUNITS) = INPUT_MW(1,I)
               INPUT_MW(2,NUNITS) = INPUT_MW(2,I)
               COEFF(1,NUNITS) = COEFF(1,I)
               COEFF(2,NUNITS) = COEFF(2,I)
               COEFF(3,NUNITS) = COEFF(3,I)
               CL_AI_CAPACITY_RATE(NUNITS) = CL_AI_CAPACITY_RATE(I)
               CL_AI_CAPACITY_ESCALATOR(NUNITS) =
     +                                       CL_AI_CAPACITY_ESCALATOR(I)
               CL_AI_ENERGY_RATE(NUNITS) = CL_AI_ENERGY_RATE(I)
               CL_AI_ENERGY_ESCALATOR(NUNITS)=CL_AI_ENERGY_ESCALATOR(I)
               AI_CL_REMAINING_LIFE(NUNITS) = AI_CL_REMAINING_LIFE(I)
               AI_CL_TAX_LIFE(NUNITS) = AI_CL_TAX_LIFE(I)
               AI_CL_ADR_LIFE(NUNITS) = AI_CL_ADR_LIFE(I)
               SEC_FUEL_EMISS_PTR(NUNITS) = SEC_FUEL_EMISS_PTR(I)
               SEC_FUEL_EMISS_PTR(I) = 0
               CL_RESOURCE_ID(NUNITS) = CL_RESOURCE_ID(I)
               DISPADJ2(NUNITS) = DISPADJ2(I)
               PHASE_I_UNIT(NUNITS) = PHASE_I_UNIT(I)
               LOCAL_CL_POOL_FRAC_OWN(NUNITS) =
     +                                         LOCAL_CL_POOL_FRAC_OWN(I)
!
! SINCE ONLY THE SECONDARY FUEL IS USED ZERO PRIMARLY AND EMISSIONS FUEL
! DATA
!
               FUEL_SUPPLY_ID(NUNITS) = FUEL_SUPPLY_ID(I)
               foshyd.CAP_PLANNING_FAC(NUNITS) = 0.
               PBTUCT(NUNITS) = PBTUCT(I)
               PFESCR(NUNITS) = PFESCR(I)
               DISP_ADDER_ESCR(NUNITS) = DISP_ADDER_ESCR(I)
               P_SO2(NUNITS) = P_SO2(I)
               P_NOX(NUNITS) = P_NOX(I)
               P_PARTICULATES(NUNITS) = P_PARTICULATES(I)
               P_EMIS_OTH2(NUNITS) = P_EMIS_OTH2(I)
               P_EMIS_OTH3(NUNITS) = P_EMIS_OTH3(I)
               P_NOX_BK2(NUNITS) = P_NOX_BK2(I)
               EMISS_FUEL_COST(NUNITS) = EMISS_FUEL_COST(I)
               EMISS_FUEL_ESCAL(NUNITS) = EMISS_FUEL_ESCAL(I)
               EMISS_FUEL_EMISS_PTR(NUNITS) = 0.
               EMISS_BLENDING_RATE(NUNITS) = EMISS_BLENDING_RATE(I)
               PRIM_FUEL_EMISS_PTR(NUNITS) = PRIM_FUEL_EMISS_PTR(I)
! 6/10/93. GAT. NO ALLOCATION OF FUEL TYPES BETWEEN SHADOWS
               PRIM_FUEL_TYPE(NUNITS) = " "
               NUC_FUEL_PRICES_FROM_FUEL_FILE(NUNITS) = .FALSE.
               SEC_FUEL_TYPE(NUNITS) = SEC_FUEL_TYPE(I)
               SEC_FUEL_TYPE(I) = " "
               EMISS_FUEL_TYPE(NUNITS) = " "
               TIE_CONSTRAINT_GROUP(NUNITS) = TIE_CONSTRAINT_GROUP(I)
               PBTUCT_SAVE(NUNITS) = PBTUCT(I)
!
               SBTUCT_SAVE(NUNITS) = SBTUCT(NUNITS)
               FUELADJ_SAVE(NUNITS) = FUELADJ(I)
               VCPMWH_IN_SAVE(NUNITS) = VCPMWH_IN(I)
!
!
               DISPATCH_MULT_SAVE(NUNITS) = DISPATCH_MULT(I)
               EXCESS_ENERGY_SALES_SAVE(NUNITS) = EXCESS_ENERGY_SALES(I)
               MONTHLY_CAPACITY_POINTER(NUNITS) =
     +                                       MONTHLY_CAPACITY_POINTER(I)
               DISPADJ_SAVE(NUNITS) = DISPADJ(I)
               DISPADJ2_SAVE(NUNITS) = DISPADJ2(I)
               CL_AI_CAPACITY_RATE_SAVE(NUNITS) = CL_AI_CAPACITY_RATE(I)
               CL_AI_ENERGY_RATE_SAVE(NUNITS) = CL_AI_ENERGY_RATE(I)
               CL_AI_REMAINING_LIFE_SAVE(NUNITS)=AI_CL_REMAINING_LIFE(I)
               DISPADJ2_SAVE(NUNITS) = DISPADJ2(I)
               EMISS_FUEL_COST_SAVE(NUNITS) = EMISS_FUEL_COST(I)
!

               TRANS_ID = TRANSACTION_GROUP_ID(NUNITS)
!
               IF(TRANS_ID > 0 .AND.
     +                             ON_LINE_YEAR <= LAST_STUDY_YEAR) THEN
                  DAY_ID = DAY_TYPE_ID(NUNITS)
                  MAX_TRANS_ID_USED = MAX(MAX_TRANS_ID_USED,TRANS_ID)
                  IF(DAY_ID > 0 .AND. DAY_ID <= MAX_DAY_TYPES ) THEN
                     IF(DAY_TYPE_TRANS_GROUP_PAIR(DAY_ID,TRANS_ID)
     +                                                        == 0) THEN
                        MAX_TRANS_DATA_BASES = MAX_TRANS_DATA_BASES + 1
                        DAY_TYPE_TRANS_GROUP_PAIR(DAY_ID,TRANS_ID) =
     +                                              MAX_TRANS_DATA_BASES
                     ENDIF
!
                     DATA_BASE_POSITION(1,NUNITS) =
     +                        DAY_TYPE_TRANS_GROUP_PAIR(DAY_ID,TRANS_ID)
                     DATA_BASE_POSITION(2,NUNITS) =
     +                               DAY_TYPE_TRANS_GROUP_PAIR(DAY_ID,0)
                  ELSEIF(DAY_ID < 0) THEN
                     DAY_TYPE_COUNTER = 1
                     DO
                        DAY_ID = INT(GET_VAR(FLOAT(DAY_TYPE_ID(NUNITS)),
     +                                DAY_TYPE_COUNTER,"FIND DAY TYPE"))
                        IF(DAY_ID <= 0 .OR. DAY_ID > MAX_DAY_TYPES) EXIT
                        IF(DAY_TYPE_TRANS_GROUP_PAIR(DAY_ID,TRANS_ID)
     +                                                        == 0) THEN
                           MAX_TRANS_DATA_BASES =
     +                                          MAX_TRANS_DATA_BASES + 1
                           DAY_TYPE_TRANS_GROUP_PAIR(DAY_ID,TRANS_ID) =
     +                                              MAX_TRANS_DATA_BASES
                        ENDIF
!     !
                        DATA_BASE_POSITION(
     +                                    2*DAY_TYPE_COUNTER-1,NUNITS) =
     +                        DAY_TYPE_TRANS_GROUP_PAIR(DAY_ID,TRANS_ID)
                        DATA_BASE_POSITION(2*DAY_TYPE_COUNTER,NUNITS) =
     +                               DAY_TYPE_TRANS_GROUP_PAIR(DAY_ID,0)
                        DAY_TYPE_COUNTER = DAY_TYPE_COUNTER + 1
                     ENDDO
                  ELSEIF(DAY_ID == 0) THEN
                     DO M = 1, MAX_DAY_TYPES
                        IF(DAY_TYPE_TRANS_GROUP_PAIR(M,TRANS_ID)
     +                                                        == 0) THEN
                           MAX_TRANS_DATA_BASES =
     +                                          MAX_TRANS_DATA_BASES + 1
                           DAY_TYPE_TRANS_GROUP_PAIR(M,TRANS_ID) =
     +                                              MAX_TRANS_DATA_BASES
                        ENDIF
                        DATA_BASE_POSITION(2*M-1,NUNITS) =
     +                             DAY_TYPE_TRANS_GROUP_PAIR(M,TRANS_ID)
                        DATA_BASE_POSITION(2*M,NUNITS) =
     +                                    DAY_TYPE_TRANS_GROUP_PAIR(M,0)
                     ENDDO
                  ELSE
                     WRITE(4,*) "DAY TYPE DESIGNATION",DAY_ID
                     WRITE(4,*) "OUTSIDE THE RANGE OF POSSIBLE OUTCOMES"
                     WRITE(4,*)
     +                      "MAXIMUM DAY TYPES DEFINED = ",MAX_DAY_TYPES
                     WRITE(4,*) '*** line 3278 CLA_OBJT.FOR ***'
                     er_message='See WARNING MESSAGES-cla_objt-3'
                     call end_program(er_message)
                  ENDIF
               ENDIF
            ENDIF ! SHADOW UNITS
!

! 051310. TO GENERATE MONTHLY CAPACITIES
         IF(NUNITS > 0) THEN
            SAVE_DETAILED_MAINTENANCE = .TRUE.
         ENDIF
      RETURN
!***************************************************************
      ENTRY INIT_CAP_MARKET_REVENUE()
!***************************************************************
!
! 091307. CAPACITY MARKET
!
         MON_CAP_SALE_MW_FROM = 0.0
         MON_CAP_SALE_REV_FROM = 0.0
         MON_CAP_PUCH_MW_FROM = 0.0
         MON_CAP_PUCH_COST_FROM = 0.0
!
         CL_TG_CAP_MARKET_MW = 0.0
         CL_TG_CAP_MARKET_REV = 0.0
!
         INIT_CAP_MARKET_REVENUE = .TRUE.
      RETURN
!***************************************************************
      ENTRY CALC_CL_CAP_MARKETS(R_YEAR,R_MONTH)
!***************************************************************
         IF(.NOT. ALLOCATED(ANNUAL_CL_CAPACITY)) THEN
            WRITE(4,*) 'CANNOT CALCULATE CAPACITY MARKETS'
            WRITE(4,*) 'THE MONTHLY CAPACITY IS NOT ALLOCATED'
            IF(.NOT. SAVE_DETAILED_MAINTENANCE) THEN
               WRITE(4,*) 'NO DETAILED MAINTENANCE'
            ENDIF
         ENDIF
         DATE_CHECK = 100*(R_YEAR+BASE_YEAR-1900) + R_MONTH
         DO I = 1, NUNITS
            IF(capacity_market_pointer_acl(I) <= 0 .OR.
     +                          CAPACITY_MARKET_TYPE(I) == 'NONE') CYCLE
            CAP_MARKET_SALE =  CAPACITY_MARKET_COST_ASSIGN(I) ==
     +                                            'Capacity Sales      '
            PRIMARY_FUEL_ID =
     +          -1.*(10000. + capacity_market_pointer_acl(I))


       if(abs(primary_fuel_id)>=2.89760000E+004)then
                call end_program("cla_objt:0005 - " //
     + "Primary Fuel ID is too high.")
            endif



            CALL GET_A_CAP_MARKET_PRICE(
     +        real(PRIMARY_FUEL_ID),
     +        CAP_MARKET_RATE,
     +                         R_MONTH,
     +                         R_YEAR)
!
! IF AN ANNUAL CAPACITY CHARGE IN A GIVEN MONTH
!
            IF(CAP_MARKET_MONTH_NO(I) /= 13) THEN
               TEMP_I2 = CAP_MARKET_MONTH_NO(I)
            ELSE

               TEMP_I2 = R_MONTH
            ENDIF
            CAP_MARKET_RATE = CAP_MARKET_RATE ! CONVERSION TO $mm
            IF(CAPACITY_MARKET_TYPE(I) == 'ICAP')THEN
               PLAN_FACTOR = 1.0
            ELSE
               IF(foshyd.CAP_PLANNING_FAC(I) < 0.) THEN
                  LOCAL_YEAR = R_YEAR - BASE_YEAR
                  PLAN_FACTOR =
     +               GET_VAR(foshyd.CAP_PLANNING_FAC(I),
     +               LOCAL_YEAR, UNITNM(I))
               ELSE
                  PLAN_FACTOR = foshyd.CAP_PLANNING_FAC(I)
               ENDIF

            ENDIF
!
            PLAN_FACTOR = PLAN_FACTOR *
     +                          CAPACITY_MARKET_COIN_ADJ_FACT(I)

            IF(ONLINE(I) > DATE_CHECK .OR. OFLINE(I) < DATE_CHECK) THEN
               TEMP_R4 = 0.
            ELSE
               TEMP_R4 = ANNUAL_CL_CAPACITY(2,I,TEMP_I2)
            ENDIF
!
            IF(CAP_MARKET_SALE)THEN            ! L*1
               MON_CAP_SALE_MW_FROM(I) = TEMP_R4 * PLAN_FACTOR
               MON_CAP_SALE_REV_FROM(I) = 1000.*
     +                 MON_CAP_SALE_MW_FROM(I) * CAP_MARKET_RATE
            ELSE
               MON_CAP_PUCH_MW_FROM(I) = TEMP_R4 * PLAN_FACTOR
               MON_CAP_PUCH_COST_FROM(I) = 1000.*
     +                 MON_CAP_PUCH_MW_FROM(I) * CAP_MARKET_RATE
            ENDIF
! TRANSACT C REPORTING
            TG = TRANSACTION_GROUP_ID(I)
            TG = GET_TRANS_GROUP_POSITION(TG)
            CL_TG_CAP_MARKET_MW(TG) = CL_TG_CAP_MARKET_MW(TG) +
     +                             MON_CAP_PUCH_MW_FROM(I) +
     +                             MON_CAP_SALE_MW_FROM(I)
            CL_TG_CAP_MARKET_REV(TG) = CL_TG_CAP_MARKET_REV(TG) +
     +            (MON_CAP_SALE_REV_FROM(I) +
     +                                MON_CAP_PUCH_COST_FROM(I))
         ENDDO
         CALC_CL_CAP_MARKETS = .TRUE.
      RETURN
!***************************************************************
      ENTRY GET_CAP_MARKET_REVENUE(R_UNIT_NO)
!***************************************************************
         GET_CAP_MARKET_REVENUE = .000001 *
     +            (MON_CAP_SALE_REV_FROM(R_UNIT_NO) +
     +                                MON_CAP_PUCH_COST_FROM(R_UNIT_NO))
      RETURN
!***************************************************************
      ENTRY GET_CL_TG_CAP_MARKET_MW(R_TG)
!***************************************************************
         GET_CL_TG_CAP_MARKET_MW =  CL_TG_CAP_MARKET_MW(R_TG)
      RETURN
!***************************************************************
      ENTRY GET_CL_TG_CAP_MARKET_REV(R_TG)
!***************************************************************
         GET_CL_TG_CAP_MARKET_REV =  CL_TG_CAP_MARKET_REV(R_TG)
      RETURN
!***************************************************************
      ENTRY CAP_MARKET_ACTIVE(R_UNIT_NO)
!***************************************************************
         IF(capacity_market_pointer_acl(R_UNIT_NO) > 0 .AND.
     +                   CAPACITY_MARKET_TYPE(R_UNIT_NO) /= 'NONE') THEN
            CAP_MARKET_ACTIVE = .TRUE.
         ELSE
            CAP_MARKET_ACTIVE = .FALSE.
         ENDIF
      RETURN
!***************************************************************
      ENTRY GET_PRIMARY_MOVER_INDEX(R_UNIT_NO)
!***************************************************************
         GET_PRIMARY_MOVER_INDEX =
     +             PRIMARY_MOVER_INDEX_DB(PRIMARY_MOVER_STR(R_UNIT_NO))
         IF(GET_PRIMARY_MOVER_INDEX == 10 .AND.
     +                               PRIMARY_MOVER(R_UNIT_NO) > 3 ) THEN
            GET_PRIMARY_MOVER_INDEX = 7
         ENDIF
      RETURN
!***************************************************************
      ENTRY GET_POINTER_FOR_NEW_CL_UNIT(R_UNIT_NO)
!***************************************************************
         GET_POINTER_FOR_NEW_CL_UNIT =
     +                                POINTER_FOR_NEW_CL_UNIT(R_UNIT_NO)
      RETURN
!***********************************************************************
      ENTRY GET_CL_AFTER_PEAK(R_TG,R_YEAR)
!***********************************************************************

         GET_CL_AFTER_PEAK = CL_TG_AFTER_PEAK(R_TG,
     +                                         MIN(R_YEAR,STUDY_PERIOD))

      RETURN
!***********************************************************************
      ENTRY GET_CL_TG_RETIRE(R_TG,R_YEAR)
!***********************************************************************

         GET_CL_TG_RETIRE = CL_TG_RETIRE(R_TG,
     +                                         MIN(R_YEAR,STUDY_PERIOD))
      RETURN
!***********************************************************************
      ENTRY GET_CL_TG_CAP(R_FT,R_TG,R_YEAR,R_ST_P)
!***********************************************************************

         GET_CL_TG_CAP = CL_TG_CAP(R_FT,R_TG,
     +                                  MIN(R_YEAR,STUDY_PERIOD),R_ST_P)
      RETURN
!***********************************************************************
      ENTRY GET_NEWGEN_CAP_BY_INDEX(R_NG,R_TG,R_YEAR)
!***********************************************************************

         GET_NEWGEN_CAP_BY_INDEX = NEWGEN_CAP_BY_INDEX(R_NG,R_TG,
     +                                         MIN(R_YEAR,STUDY_PERIOD))
      RETURN
!***************************************************************
      ENTRY FIRST_YEAR_CL_CAPACITY(R_YEAR,R_UNIT_NO)
!***************************************************************
         UNIT_NO = R_UNIT_NO
         UNIT_CAPACITY = INPUT_MW(2,UNIT_NO)
         YEAR_START = R_YEAR - BASE_YEAR
         YEAR_END = OFF_LINE_YEAR(UNIT_NO)

         CAP_MW = UNIT_CAPACITY
         IF(CAP_MW < 0.) THEN
            CAP_MW = GET_CL_MW_FROM_POINTR(CAP_MW,YEAR_START)
! ADDED PER JONES. 3/13/98. GAT.
            IF(CAP_MW < 0.)
     +         CAP_MW =
     +              GET_CL_MW_FROM_POINTR(CAP_MW,PEAK_MONTH(YEAR_START))
!
            CAP_MW = MAX(0.,CAP_MW)
         ELSE
!
! IF NO POINTER HAS BEEN SPECIFIED FOR THE PEAK MONTH
!
            CAP_MW = MAX(0.,CAP_MW)
         ENDIF

         IF(MONTHLY_CAPACITY_POINTER(UNIT_NO) /= 0 .AND.
     +                           .NOT. CL_CAP_AREA_LINKED(UNIT_NO)) THEN
            R_POINTR = FLOAT(ABS(MONTHLY_CAPACITY_POINTER(UNIT_NO)))
            MONTH_MULT = GET_CL_MW_FROM_POINTR(R_POINTR,
     +                                           PEAK_MONTH(YEAR_START))
            IF(MONTH_MULT > 2. .AND. CAP_MW /= 0.) THEN
               CAP_MW = MONTH_MULT
            ELSE
               CAP_MW = MONTH_MULT * CAP_MW
            ENDIF
         ELSEIF(MONTHLY_CAPACITY_POINTER(UNIT_NO) < 0 .AND.
     +                                 CL_CAP_AREA_LINKED(UNIT_NO)) THEN
            LOCAL_NAME = UNITNM(UNIT_NO)
            LOCAL_POINTER = ABS(MONTHLY_CAPACITY_POINTER(UNIT_NO))
            CAP_MW = MIN(GET_CLASS_PEAK_NET_DSM_FOR_CAP(
     +                      YEAR_START,LOCAL_NAME,LOCAL_POINTER),CAP_MW)
         ENDIF
!
! EFFECTIVE MW'S OF PLANNING CAPACITY
!

         IF(foshyd.CAP_PLANNING_FAC(UNIT_NO) < 0.) THEN
            PLAN_FACTOR =
     +                  GET_VAR(foshyd.CAP_PLANNING_FAC(UNIT_NO),
     +                                        YEAR,
     +                                        UNITNM(UNIT_NO))
         ELSE
            PLAN_FACTOR = foshyd.CAP_PLANNING_FAC(UNIT_NO)
         ENDIF
         CAP_MW = CAP_MW * foshyd.CAP_FRAC_OWN(UNIT_NO) * 
     +        PLAN_FACTOR * 0.01

         FIRST_YEAR_CL_CAPACITY = CAP_MW
      RETURN
!***************************************************************
      ENTRY CL_PLANNING_CAPACITY(R_CAP_TYPE,R_YEAR)
!***************************************************************
         IF(ALLOCATED(CL_ANN_CAP)) THEN
            IF(R_YEAR > STUDY_PERIOD) THEN
               CL_PLANNING_CAPACITY =
     +            CL_ANN_CAP(R_CAP_TYPE,STUDY_PERIOD,1) +
     +            CL_ANN_CAP(R_CAP_TYPE,STUDY_PERIOD,2)
            ELSE
               CL_PLANNING_CAPACITY = CL_ANN_CAP(R_CAP_TYPE,R_YEAR,1) +
     +                                CL_ANN_CAP(R_CAP_TYPE,R_YEAR,2)
            ENDIF
         ELSE
            CL_PLANNING_CAPACITY = 0.
         ENDIF
      RETURN
!***************************************************************
      ENTRY NEW_CL_PLANNING_CAPACITY(R_CAP_TYPE,R_YEAR)
!***************************************************************
         IF(ALLOCATED(CL_ANN_CAP)) THEN
            IF(R_YEAR > STUDY_PERIOD) THEN
               NEW_CL_PLANNING_CAPACITY =
     +            CL_ANN_CAP(R_CAP_TYPE,STUDY_PERIOD,2)
            ELSE
               NEW_CL_PLANNING_CAPACITY =
     +            CL_ANN_CAP(R_CAP_TYPE,R_YEAR,2)
            ENDIF
         ELSE
            NEW_CL_PLANNING_CAPACITY = 0.
         ENDIF
      RETURN
!***************************************************************
      ENTRY CL_PLANNING_LOAD_REDUCTION(R_YEAR)
!***************************************************************
         IF(ALLOCATED(CL_ANNUAL_LOAD_REDUCTION)) THEN
            IF(R_YEAR > STUDY_PERIOD) THEN
               CL_PLANNING_LOAD_REDUCTION =
     +               CL_ANNUAL_LOAD_REDUCTION(STUDY_PERIOD)
            ELSE
               CL_PLANNING_LOAD_REDUCTION =
     +               CL_ANNUAL_LOAD_REDUCTION(R_YEAR)
            ENDIF
         ELSE
            CL_PLANNING_LOAD_REDUCTION = 0.
         ENDIF
      RETURN
!***************************************************************
      ENTRY OLD_CL_PLANNING_CAPACITY(R_CAP_TYPE,R_YEAR)
!***************************************************************
         IF(ALLOCATED(CL_ANN_CAP)) THEN
            IF(R_YEAR > STUDY_PERIOD) THEN
               OLD_CL_PLANNING_CAPACITY =
     +            CL_ANN_CAP(R_CAP_TYPE,STUDY_PERIOD,1)
            ELSE
               OLD_CL_PLANNING_CAPACITY =
     +            CL_ANN_CAP(R_CAP_TYPE,R_YEAR,1)
            ENDIF
         ELSE
            OLD_CL_PLANNING_CAPACITY = 0.
         ENDIF
      RETURN
!***************************************************************
      ENTRY SET_AI_CL_REMAINING_LIFE(AI_REMAINING_LIFE)
!***************************************************************
         AI_CL_REMAINING_LIFE(NUNITS) = AI_REMAINING_LIFE
         CL_AI_REMAINING_LIFE_SAVE(NUNITS) = AI_REMAINING_LIFE
         SET_AI_CL_REMAINING_LIFE = AI_CL_REMAINING_LIFE(NUNITS)
      RETURN
!***************************************************************
      ENTRY CL_RESET_PRICES
!******************************************************************
         DO I = 1, NUNITS
            PBTUCT(I) = PBTUCT_SAVE(I)
            SBTUCT(I) = SBTUCT_SAVE(I)
            FUELADJ(I) = FUELADJ_SAVE(I)
            VCPMWH_IN(I) = VCPMWH_IN_SAVE(I)
            FIXED_COST_IN(I) = FIXED_COST_SAVE(I)
            ANNUAL_CL_FIXED_COST(I) = ANNUAL_CL_FIXED_COST_SAVE(I)
            DISPATCH_MULT(I) = DISPATCH_MULT_SAVE(I)
            DISPADJ(I) = DISPADJ_SAVE(I)
            DISPADJ2(I) = DISPADJ2_SAVE(I)
            CL_AI_CAPACITY_RATE(I) = CL_AI_CAPACITY_RATE_SAVE(I)
            CL_AI_ENERGY_RATE(I) = CL_AI_ENERGY_RATE_SAVE(I)
            AI_CL_REMAINING_LIFE(I) = CL_AI_REMAINING_LIFE_SAVE(I)
            DISPADJ2(I) = DISPADJ2_SAVE(I)
            EMISS_FUEL_COST(I) = EMISS_FUEL_COST_SAVE(I)
         ENDDO
         CL_RESET_PRICES = .TRUE.
      RETURN
!******************************************************************
      ENTRY RETURN_CL_FUEL_POINTERS(R_P_BTU_COST_POINTR,
     +                              R_S_BTU_COST_POINTR,
     +                              R_E_BTU_COST_POINTR)
!******************************************************************
         DO I = 1, NUNITS
            R_P_BTU_COST_POINTR(I) = PBTUCT_SAVE(I)
            R_S_BTU_COST_POINTR(I) = SBTUCT_SAVE(I)
            R_E_BTU_COST_POINTR(I) = EMISS_FUEL_COST_SAVE(I)
         ENDDO
         RETURN_CL_FUEL_POINTERS = NUNITS
      RETURN
!******************************************************************
      ENTRY UPDATE_PRIMARY_FUEL_MIX_RATIO(R_YEAR)
!******************************************************************
         UPDATE_PRIMARY_FUEL_MIX_RATIO = 0
         DO I = 1, NUNITS
            IF(FUEL_MIX_PTR(I) <= -2.) THEN
               IF(R_YEAR > AVAIL_DATA_YEARS) THEN
                  FUELMX(I) = GET_VAR(FUEL_MIX_PTR(I),AVAIL_DATA_YEARS,
     +                                                        UNITNM(I))
               ELSE
                  FUELMX(I) = GET_VAR(FUEL_MIX_PTR(I),R_YEAR,UNITNM(I))
               ENDIF
               UPDATE_PRIMARY_FUEL_MIX_RATIO = 1 +
     +                                     UPDATE_PRIMARY_FUEL_MIX_RATIO
            ELSE
               FUELMX(I) = FUEL_MIX_PTR(I)
            ENDIF
         ENDDO
      RETURN
!******************************************************************
      ENTRY CL_PLANNING_ADDITIONS
!******************************************************************
         CL_PLANNING_ADDITIONS = AVAILABLE_CL_UNITS -
     +                                      BASE_PLUS_HARDWIRED_CL_UNITS
      RETURN
!******************************************************************
      ENTRY RETURN_CL_UNITS_AFTER_HARD_ADDS
!******************************************************************
         RETURN_CL_UNITS_AFTER_HARD_ADDS = BASE_PLUS_HARDWIRED_CL_UNITS
      RETURN
!******************************************************************
      ENTRY DETAILED_MAINTENANCE_IS_ACTIVE
!******************************************************************
         DETAILED_MAINTENANCE_IS_ACTIVE = SAVE_DETAILED_MAINTENANCE
      RETURN
!******************************************************************
      ENTRY DETAIL_MAINTENANCE_NOT_ACTIVE
!******************************************************************
         SAVE_DETAILED_MAINTENANCE = .FALSE.
         DETAIL_MAINTENANCE_NOT_ACTIVE = SAVE_DETAILED_MAINTENANCE
      RETURN
!******************************************************************
      ENTRY CALC_ANNUAL_CAP_AND_MAINT(R_YEAR)
!******************************************************************
         MAINTENANCE_IS_ACCUMULATED = ACCUMULATE_MAINTENANCE()

         MAINTENANCE_IS_ACCUMULATED = .FALSE.
!
         IF(.NOT. (MAINTENANCE_REPORT() .OR.
     +               SAVE_DETAILED_MAINTENANCE .OR.
     +                     YES_UNIT_COMMITMENT_LOGIC() .OR.
     +                           MAINTENANCE_IS_ACCUMULATED) .OR.

     +                                                   NUNITS==0) THEN
            CALC_ANNUAL_CAP_AND_MAINT = .FALSE.
            RETURN
         ENDIF
         IF(ALLOCATED(ANNUAL_CL_CAPACITY))
     +         DEALLOCATE(ANNUAL_CL_CAPACITY,ANNUAL_CL_MAINTENANCE,
     +                     ANNUAL_CL_MAINT_MW,MAINT_DIVISIBLE)
         ALLOCATE(ANNUAL_CL_CAPACITY(2,NUNITS,12))
      ALLOCATE(ANNUAL_CL_MAINTENANCE(NUNITS,13))
      ALLOCATE(ANNUAL_CL_MAINT_MW(NUNITS))
      ALLOCATE(MAINT_DIVISIBLE(NUNITS))
!
         IF(ALLOCATED(MAINT_INDEX)) DEALLOCATE(MAINT_INDEX,
     +                                         MAINT_INDEX_MW,
     +                                         MAINT_INDEX_DAYS,
     +                                         MAINT_INDEX_MONTHLY_MW)
         ALLOCATE(MAINT_INDEX(NUNITS))
         ALLOCATE(MAINT_INDEX_MW(NUNITS))
         ALLOCATE(MAINT_INDEX_DAYS(NUNITS))
         ALLOCATE(MAINT_INDEX_MONTHLY_MW(NUNITS,13))
!
!
         IF(ALLOCATED(MAINTENANCE_DAYS)) DEALLOCATE(MAINTENANCE_DAYS)
         ALLOCATE(MAINTENANCE_DAYS(NUNITS))
         MAINTENANCE_DAYS = 0.
         ANNUAL_CL_CAPACITY = 0.
         ANNUAL_CL_MAINTENANCE = 0.
         ANNUAL_CL_MAINT_MW = 0.
         MAINT_DIVISIBLE = FALSE_BYTE
         OFFSET_MAINTENANCE_ACTIVE = OFFSET_MAINTENANCE_VECTORS()
         BASE_DATE = (BASE_YEAR + R_YEAR - 1900) * 100
         BASE_YEAR_DATE = (BASE_YEAR - 1900) * 100
! MARKET_RESOURCE 7/5/02. FOR TVA
         IF(ALLOCATED(MARKET_RESOURCE_INDEX))
     +                                 DEALLOCATE(MARKET_RESOURCE_INDEX,
     +                                          MARKET_RESOURCE_COUNTER)
         ALLOCATE(MARKET_RESOURCE_INDEX(NUNITS))
         ALLOCATE(MARKET_RESOURCE_COUNTER(NUNITS))

         MARKET_RESOURCE_COUNTER = 0
         MARKET_COUNT = 0
!

         LAST_ACTIVE_UNIT = NUNITS
!


!
         TEMP_L = GET_MONTHLY_MAINTENANCE_PENALTY(MAINTENANCE_PENALTY)
         YES_REGIONAL_OUTAGES_ACTIVE = REGIONAL_PARAMS_ACTIVE()



         FREQUENCY_DURATION = YES_FREQUENCY_DURATION()
!
         IF(FREQUENCY_DURATION .OR. UNIT_FREQUENCY_DURATION) THEN
            MAX_FO_PER_MONTH = 8

         ELSE
            MAX_FO_PER_MONTH = 1
         ENDIF
!
         CALL INIT_MAINT_SCHEDULE(MAX_FO_PER_MONTH,NUNITS)
!
         DO PA = 1, MAX_MAINT_PLANNING_AREAS
            MAINT_INDEX_MW = 0.
            MAINT_INDEX_DAYS = 0.
            MAINT_INDEX_MONTHLY_MW = 0.

            MAINT_INDEX = 0
            K = 0

            IF(REGIONAL_PA_MAINT_SCHEDULING) THEN

               DO I = 1, 12
                  MONTHLY_MAINTENANCE_PEAKS(I) =
     +                        GET_PA_PEAK(PA,I) * MAINTENANCE_PENALTY(I)
               ENDDO

            ELSEIF(REGIONAL_TG_MAINT_SCHEDULING) THEN
               DO I = 1, 12
                  MONTHLY_MAINTENANCE_PEAKS(I) =
     +               GET_TRANS_GROUP_PEAK(PA,I) * MAINTENANCE_PENALTY(I)
               ENDDO

            ELSE
               MONTHLY_MAINTENANCE_PEAKS(1) = FUT_PEAK(3,1) *
     +                                            MAINTENANCE_PENALTY(1)
               MONTHLY_MAINTENANCE_PEAKS(2) = FUT_PEAK(3,2) *
     +                                            MAINTENANCE_PENALTY(2)
               MONTHLY_MAINTENANCE_PEAKS(3) = FUT_PEAK(3,3) *
     +                                            MAINTENANCE_PENALTY(3)
               MONTHLY_MAINTENANCE_PEAKS(4) = FUT_PEAK(3,4) *
     +                                            MAINTENANCE_PENALTY(4)
               MONTHLY_MAINTENANCE_PEAKS(5) = FUT_PEAK(3,5) *
     +                                            MAINTENANCE_PENALTY(5)
               MONTHLY_MAINTENANCE_PEAKS(6) = FUT_PEAK(3,6) *
     +                                            MAINTENANCE_PENALTY(6)
               MONTHLY_MAINTENANCE_PEAKS(7) = FUT_PEAK(3,7) *
     +                                            MAINTENANCE_PENALTY(7)
               MONTHLY_MAINTENANCE_PEAKS(8) = FUT_PEAK(3,8) *
     +                                            MAINTENANCE_PENALTY(8)
               MONTHLY_MAINTENANCE_PEAKS(9) = FUT_PEAK(3,9) *
     +                                            MAINTENANCE_PENALTY(9)
               MONTHLY_MAINTENANCE_PEAKS(10) = FUT_PEAK(3,10) *
     +                                           MAINTENANCE_PENALTY(10)
               MONTHLY_MAINTENANCE_PEAKS(11) = FUT_PEAK(3,11) *
     +                                           MAINTENANCE_PENALTY(11)
               MONTHLY_MAINTENANCE_PEAKS(12) = FUT_PEAK(3,12) *
     +                                           MAINTENANCE_PENALTY(12)
            ENDIF
!
            DO I = 1, NUNITS
!
! 2/13/96 CORRECTION FOR SHADOW UNITS
!
!
               IF(LDTYPE(I) == 'S') THEN
                  SHADOW_UNITS_ACTIVE = .TRUE.
                  CYCLE
               ENDIF
!
!
!
               IF(REGIONAL_PA_MAINT_SCHEDULING) THEN
                  TG = TRANSACTION_GROUP_ID(I)
                  TG = GET_TRANS_GROUP_POSITION(TG)
                  IF(TG_2_PLANNING_AREA(TG) /= PA) CYCLE
               ELSEIF(REGIONAL_TG_MAINT_SCHEDULING) THEN
                  TG = TRANSACTION_GROUP_ID(I)
                  TG = GET_TRANS_GROUP_POSITION(TG)
                  IF(TG /= PA) CYCLE
               ENDIF
!


               DATE1 = BASE_DATE + 1
               DATE2 = BASE_DATE + 12
               IF(ONLINE(I) > DATE2 .OR. OFLINE(I) < DATE1) CYCLE

               IF(MAINT_DAYS(I) /= 0.) THEN
                  IF(MAINT_DAYS(I) < 0.) THEN
                     MAINTENANCE_DAYS(I) = GET_VAR(MAINT_DAYS(I),YEAR,
     +                                                        UNITNM(I))
                  ELSE
                     MAINTENANCE_DAYS(I) = MAINT_DAYS(I)
                  ENDIF
               ENDIF

               IF(MARKET_RESOURCE(I) == 'T' .OR.
     +                    MARKET_RESOURCE(I) == 'G' .OR.
     +                                   MARKET_RESOURCE(I) == 'D') THEN
                  MARKET_COUNT = MARKET_COUNT + 1
                  MARKET_RESOURCE_COUNTER(I) = MARKET_COUNT
                  MARKET_RESOURCE_INDEX(MARKET_COUNT) = I
               ENDIF
!

               IF(MAINTENANCE_DAYS(I) > 52.14) THEN
                  WRITE(4,*) "AUTOMATIC MAINTENANCE FOR CL UNIT ",
     +                                                         UNITNM(I)
                  WRITE(4,*) "IN YEAR ",YEAR+BASE_YEAR,
     +                                               " EXCEEDS 52 WEEKS"
                  WRITE(4,*) "MAINTENANCE RESET TO 52 WEEKS"
                  WRITE(4,*) " "
                  MAINTENANCE_DAYS(I) = 52.14
               ENDIF
!
               IF(MAINTENANCE_DAYS(I) < 0.) THEN
                  WRITE(4,*) "AUTOMATIC MAINTENANCE FOR CL UNIT ",
     +                                                         UNITNM(I)
                  WRITE(4,*) "IN YEAR ",YEAR+BASE_YEAR," IS LESS THAN 0"
                  WRITE(4,*) "MAINTENANCE RESET TO 0 WEEKS"
                  WRITE(4,*) " "
                  MAINTENANCE_DAYS(I) = 0.
               ENDIF

               DO J = 1 , 12 ! CALCULATE CAPACITIES FOR ALL MONTHS
                  IF(MONTHLY_CAPACITY_POINTER(I) /= 0) THEN
                     IF(CL_CAP_AREA_LINKED(I)) THEN
                        PERIOD_RATE =
     +                              GET_CLASS_PEAK_NET_DSM(J,UNITNM(I),
     +                                 ABS(MONTHLY_CAPACITY_POINTER(I)))
                     ELSE
                        PERIOD_RATE = GET_VAR(FLOAT(
     +                    ABS(MONTHLY_CAPACITY_POINTER(I))),J,UNITNM(I))
                     ENDIF
                     DO M = 2 , 1, -1
                        IF(INPUT_MW(M,I) < 0.) THEN
                           MW_CAPACITY = GET_VAR(INPUT_MW(M,I),YEAR,
     +                                                        UNITNM(I))
                           IF(MW_CAPACITY < 0.) THEN
                              MW_CAPACITY = GET_VAR(MW_CAPACITY,J,
     +                                                        UNITNM(I))
                           ENDIF
                        ELSE
                           MW_CAPACITY = INPUT_MW(M,I)
                        ENDIF
                        IF(PERIOD_RATE > 2.) THEN
                           IF(MW_CAPACITY > 0.) THEN
                              PERIOD_RATE = PERIOD_RATE/MW_CAPACITY
                           ELSE
                              MW_CAPACITY = PERIOD_RATE
                              PERIOD_RATE = 1.
                           ENDIF
                        ENDIF
                        IF(MW_CAPACITY < 1. .AND. M == 1) THEN
                           ANNUAL_CL_CAPACITY(M,I,J) =
     +                                    MW_CAPACITY * UNIT_CAPACITY
                        ELSE
                           ANNUAL_CL_CAPACITY(M,I,J) =  MW_CAPACITY *
     +                        foshyd.CAP_FRAC_OWN(I)/100. * PERIOD_RATE
                        ENDIF
                        IF(M == 2)
     +                           UNIT_CAPACITY=ANNUAL_CL_CAPACITY(M,I,J)
                     ENDDO
                  ELSE
                     DO M = 2, 1 , -1
                        IF(INPUT_MW(M,I) < 0.) THEN
                           ANNUAL_CL_CAPACITY(M,I,J) =
     +                             GET_VAR(INPUT_MW(M,I),YEAR,UNITNM(I))
                           IF(ANNUAL_CL_CAPACITY(M,I,J) < 0.) THEN
                              ANNUAL_CL_CAPACITY(M,I,J) =
     +                           GET_VAR(ANNUAL_CL_CAPACITY(M,I,J),J,
     +                                                        UNITNM(I))
                           ENDIF
                        ELSE
                           ANNUAL_CL_CAPACITY(M,I,J) = INPUT_MW(M,I)
                        ENDIF
                        IF(ANNUAL_CL_CAPACITY(M,I,J) < 1. .AND.
     +                                                        M==1) THEN
                           ANNUAL_CL_CAPACITY(1,I,J) =
     +                       ANNUAL_CL_CAPACITY(2,I,J) *
     +                                         ANNUAL_CL_CAPACITY(1,I,J)
                        ELSE
                           ANNUAL_CL_CAPACITY(M,I,J) =
     +                              ANNUAL_CL_CAPACITY(M,I,J)*
     +                                      foshyd.CAP_FRAC_OWN(I)/100.
                        ENDIF
                     ENDDO
                     UNIT_CAPACITY = ANNUAL_CL_CAPACITY(2,I,J)
                  ENDIF
                  DATE1 = BASE_DATE + J
                  IF(RETROFIT_ACTIVE(I)) THEN
                     TEMP_I2 = RETROFIT_PROJECT_ID(I)
                     CALL RETROFIT_MINCAP_IMPACT(
     +                                        TEMP_I2,
     +                                        TEMP_R4)
                     ANNUAL_CL_CAPACITY(1,I,J) =
     +                               ANNUAL_CL_CAPACITY(1,I,J) * TEMP_R4
                     CALL RETROFIT_MAXCAP_IMPACT(
     +                                        TEMP_I2,
     +                                        TEMP_R4)
                     ANNUAL_CL_CAPACITY(2,I,J) =
     +                               ANNUAL_CL_CAPACITY(2,I,J) * TEMP_R4
                     UNIT_CAPACITY = ANNUAL_CL_CAPACITY(2,I,J)

                  ELSEIF(DATE1 >= CO2_CONTROL_DATE(I) .AND.
     +                        CO2_CONTROL_DATE(I) > BASE_YEAR_DATE) THEN
                     TEMP_R4 = CO2_RETRO_CAP_MULT(I)
                     ANNUAL_CL_CAPACITY(1,I,J) =
     +                               ANNUAL_CL_CAPACITY(1,I,J) * TEMP_R4
                     ANNUAL_CL_CAPACITY(2,I,J) =
     +                               ANNUAL_CL_CAPACITY(2,I,J) * TEMP_R4
                     UNIT_CAPACITY = ANNUAL_CL_CAPACITY(2,I,J)
                  ENDIF
                  IF(ONLINE(I) <= DATE1 .AND. OFLINE(I) >= DATE1 .AND.
     +                  (UNIT_CAPACITY > 0. .OR. LDTYPE(I) == 'L')) THEN
                     IF(MNRATE(I,J) < 0.) THEN
                        IF(OFFSET_MAINTENANCE_ACTIVE .AND.
     +                                           I > BASE_CL_UNITS) THEN
                           GET_YR =
     +                           YEAR-(ONLINE(I)/100+1900 - BASE_YEAR)+1
                        ELSE
                           GET_YR = YEAR
                        ENDIF
                        MN_RATE = GET_VAR(MNRATE(I,J),GET_YR,UNITNM(I))
!
                        TEMP_CL_MAINTENANCE = MIN(100.,MN_RATE)/100.
!
                     ELSE
                        TEMP_CL_MAINTENANCE = MIN(100.,MNRATE(I,J))/100.
                     ENDIF
                  ELSE
                     TEMP_CL_MAINTENANCE = 1.0
                     MAINTENANCE_DAYS(I) =
     +                     MIN(52.14,MAINTENANCE_DAYS(I) +
     +                       TEMP_CL_MAINTENANCE * DAYS_PER_MONTH(J)/7.)
                  ENDIF
                  IF(MAINTENANCE_IS_ACCUMULATED .AND.
     +               ONLINE(I) <= DATE1 .AND. OFLINE(I) >= DATE1 .AND.
     +               .NOT. (LDTYPE(I)=='N' .OR.
     +                                 EXPENSE_ASSIGNMENT(I)=='P')) THEN
                     MAINTENANCE_DAYS(I) = MAINTENANCE_DAYS(I) +
     +                        TEMP_CL_MAINTENANCE * DAYS_PER_MONTH(J)/7.
                  ELSE
                     ANNUAL_CL_MAINTENANCE(I,J) = TEMP_CL_MAINTENANCE
                  ENDIF
                  ANNUAL_CL_MAINTENANCE(I,13) =
     +               ANNUAL_CL_MAINTENANCE(I,13) +
     +                    ANNUAL_CL_MAINTENANCE(I,J) * DAYS_PER_MONTH(J)
                  ANNUAL_CL_MAINT_MW(I) = MAX(ANNUAL_CL_MAINT_MW(I),
     +                                        ANNUAL_CL_CAPACITY(2,I,J))
               ENDDO ! SEASONS
!
               K = K + 1
               MAINT_INDEX(K) = I
               MAINT_INDEX_DAYS(K) = MAINTENANCE_DAYS(I)
               MAINT_INDEX_MW(K) = ANNUAL_CL_MAINT_MW(I)
               MAINT_INDEX_MONTHLY_MW(K,1) = ANNUAL_CL_MAINTENANCE(I,1)
               MAINT_INDEX_MONTHLY_MW(K,2) = ANNUAL_CL_MAINTENANCE(I,2)
               MAINT_INDEX_MONTHLY_MW(K,3) = ANNUAL_CL_MAINTENANCE(I,3)
               MAINT_INDEX_MONTHLY_MW(K,4) = ANNUAL_CL_MAINTENANCE(I,4)
               MAINT_INDEX_MONTHLY_MW(K,5) = ANNUAL_CL_MAINTENANCE(I,5)
               MAINT_INDEX_MONTHLY_MW(K,6) = ANNUAL_CL_MAINTENANCE(I,6)
               MAINT_INDEX_MONTHLY_MW(K,7) = ANNUAL_CL_MAINTENANCE(I,7)
               MAINT_INDEX_MONTHLY_MW(K,8) = ANNUAL_CL_MAINTENANCE(I,8)
               MAINT_INDEX_MONTHLY_MW(K,9) = ANNUAL_CL_MAINTENANCE(I,9)
               MAINT_INDEX_MONTHLY_MW(K,10) =
     +                                       ANNUAL_CL_MAINTENANCE(I,10)
               MAINT_INDEX_MONTHLY_MW(K,11) =
     +                                       ANNUAL_CL_MAINTENANCE(I,11)
               MAINT_INDEX_MONTHLY_MW(K,12) =
     +                                       ANNUAL_CL_MAINTENANCE(I,12)
               MAINT_INDEX_MONTHLY_MW(K,13) =
     +                                       ANNUAL_CL_MAINTENANCE(I,13)
!

            ENDDO ! UNITS
!
!  SIMPLIFICATIONS FOR MAINTENANCE SCHEDULER:
!
!     (1) WABASH_VALLEY GIBSON CAPACITY IS TAKEN OUT
!     (2) DAILY SCHEDULING ONLY
!     (3) NO SPLITTING THE MAINTENANCE
!     (4) THE ALGORITHM IS LOOKING FOR SCHEDULED WEEKS, NOT DAYS
!     (5) MAINT SCHEDULED BEYOND DECEMBER 31 IT MOVES TO JAN 1 SAME YEAR
!
            IF(SAVE_DETAILED_MAINTENANCE .OR.
     +                     YES_UNIT_COMMITMENT_LOGIC() .OR.   ! TEST. 8/15/02. GAT.
     +                      MAINTENANCE_IS_ACCUMULATED) THEN
!

!


               LAST_ACTIVE_UNIT = K

               SCHEDULE_FO = .TRUE.


               DETAILED_MAINT = YES_DETAILED_MAINTENANCE()

!
               CALL SW_MAINT_DERATING(SCHEDULE_FO)
!
               SCHEDULE_FO = YES_DETAILED_FOR() ! PER 4/3/01 THIELKE E-MAIL.

               VOID_LOGICAL = PICK_FOR_SEED_OPTIONS(FOR_SEED_OPTIONS)
!   !
               IF(FOR_SEED_OPTIONS == 'Y') THEN
                  YEAR_OR_SCEN = R_YEAR
               ELSE
                  YEAR_OR_SCEN = END_POINT
               ENDIF
!

               IF(LAST_ACTIVE_UNIT == 0) CYCLE
!
               CALL MAINT_SCHEDULER(
     +                           NUNITS,
     +                           LAST_ACTIVE_UNIT,
     +                           MAINT_DIVISIBLE,
     +                           'D',
     +                           MAINT_INDEX_MW,
     +                           MAINT_INDEX_DAYS,
     +                           MAINT_INDEX_MONTHLY_MW,
     +                           MAINT_INDEX,
     +                           SCHEDULE_FO,
     +                           DETAILED_MAINT,
     +                           MAX_FO_PER_MONTH,
     +                           MONTHLY_MAINTENANCE_PEAKS) ! ADDED 9/27/02.
!
! MAINT_INDEX above is needed even if active items in arrays are compressed;
! NUNITS above needed only for properly dimensioning MAINT_INDEX_MONTHLY_MW;
! if subscript order on MAINT_INDEX_MONTHLY_MW were reversed, we could avoid
! passing NUNITS, making MAINT_SCHEDULER simpler and more elegant.
!
!
!
! 11/17/98. GAT. FOR SCENARIO MAKER.
!
               DO J = 1, 12
                  MONTHLY_NUCLEAR_AVAIL_MULT(J) =
     +                              GET_SCENARIO_NUCLEAR_AVAIL(R_YEAR,J)
                  MONTHLY_COAL_AVAIL_MULT(J) =
     +                                 GET_SCENARIO_COAL_AVAIL(R_YEAR,J)
                  MONTHLY_GAS_AVAIL_MULT(J) =
     +                                 GET_SCENARIO_GAS_AVAIL(R_YEAR,J)
                  MONTHLY_OIL_AVAIL_MULT(J) =
     +                                 GET_SCENARIO_OIL_AVAIL(R_YEAR,J)
                  MONTHLY_OTHER_AVAIL_MULT(J) =
     +                                GET_SCENARIO_OTHER_AVAIL(R_YEAR,J)
                  MONTHLY_HYDRO_WATER_YEAR_MULT(J) =
     +                         GET_SCENARIO_HYDRO_WATER_YEAR(R_YEAR,J)
               ENDDO
!
               REGIONAL_MONTHLY_MULT = 1.
!
               DO K = 1, LAST_ACTIVE_UNIT
                  I = MAINT_INDEX(K)
                  ANNUAL_CL_MAINTENANCE(I,1) =
     +                                       MAINT_INDEX_MONTHLY_MW(K,1)
                  ANNUAL_CL_MAINTENANCE(I,2) =
     +                                       MAINT_INDEX_MONTHLY_MW(K,2)
                  ANNUAL_CL_MAINTENANCE(I,3) =
     +                                       MAINT_INDEX_MONTHLY_MW(K,3)
                  ANNUAL_CL_MAINTENANCE(I,4) =
     +                                       MAINT_INDEX_MONTHLY_MW(K,4)
                  ANNUAL_CL_MAINTENANCE(I,5) =
     +                                       MAINT_INDEX_MONTHLY_MW(K,5)
                  ANNUAL_CL_MAINTENANCE(I,6) =
     +                                       MAINT_INDEX_MONTHLY_MW(K,6)
                  ANNUAL_CL_MAINTENANCE(I,7) =
     +                                       MAINT_INDEX_MONTHLY_MW(K,7)
                  ANNUAL_CL_MAINTENANCE(I,8) =
     +                                       MAINT_INDEX_MONTHLY_MW(K,8)
                  ANNUAL_CL_MAINTENANCE(I,9) =
     +                                       MAINT_INDEX_MONTHLY_MW(K,9)
                  ANNUAL_CL_MAINTENANCE(I,10) =
     +                                      MAINT_INDEX_MONTHLY_MW(K,10)
                  ANNUAL_CL_MAINTENANCE(I,11) =
     +                                      MAINT_INDEX_MONTHLY_MW(K,11)
                  ANNUAL_CL_MAINTENANCE(I,12) =
     +                                      MAINT_INDEX_MONTHLY_MW(K,12)
                  ANNUAL_CL_MAINTENANCE(I,13) =
     +                                      MAINT_INDEX_MONTHLY_MW(K,13)


                  FT = PRIMARY_MOVER(I)
!
                  IF(YES_REGIONAL_OUTAGES_ACTIVE) THEN

                    TG = TRANSACTION_GROUP_ID(I)
                     DO J = 1, 12
                        REGIONAL_MONTHLY_MULT(J) =
     +                               GET_MONTHLY_REGIONAL_OUTAGE(R_YEAR,
     +                                                          J,TG,FT)
                     ENDDO
                  ENDIF
!
!
                  IF(EFOR(I) < 0.) THEN
                     TEMP_R4 = GET_VAR(EFOR(I),R_YEAR,UNITNM(I))
                     DO J = 1, 12
                        IF(TEMP_R4 < 0.) THEN
                           MONTHLY_EAVAIL(J) =
     +                        (100. - GET_VAR(TEMP_R4,J,UNITNM(I)))/100.
                        ELSE
                           MONTHLY_EAVAIL(J) = (100. - TEMP_R4)/100.
                        ENDIF
!
                        IF(FT == 1) THEN
                           MONTHLY_EAVAIL(J) =
     +                        MIN(MONTHLY_EAVAIL(J) *
     +                           REGIONAL_MONTHLY_MULT(J) *
     +                                   MONTHLY_COAL_AVAIL_MULT(J),1.0)
                        ELSEIF(FT ==2) THEN
                           MONTHLY_EAVAIL(J) =
     +                        MIN(MONTHLY_EAVAIL(J) *
     +                           REGIONAL_MONTHLY_MULT(J) *
     +                                   MONTHLY_GAS_AVAIL_MULT(J),1.0)
                        ELSEIF(FT == 3) THEN
                           MONTHLY_EAVAIL(J) =
     +                        MIN(MONTHLY_EAVAIL(J) *
     +                           REGIONAL_MONTHLY_MULT(J) *
     +                                   MONTHLY_OIL_AVAIL_MULT(J),1.0)
                        ELSEIF(FT == 4) THEN
                           MONTHLY_EAVAIL(J) =
     +                        MIN(MONTHLY_EAVAIL(J) *
     +                           REGIONAL_MONTHLY_MULT(J) *
     +                                MONTHLY_NUCLEAR_AVAIL_MULT(J),1.0)
                        ELSEIF(FT == 5) THEN
                           MONTHLY_EAVAIL(J) =
     +                         MONTHLY_EAVAIL(J) *
     +                           REGIONAL_MONTHLY_MULT(J) *
     +                                  MONTHLY_HYDRO_WATER_YEAR_MULT(J)
                        ELSEIF(FT == 6) THEN
                           MONTHLY_EAVAIL(J) =
     +                        MIN(MONTHLY_EAVAIL(J) *
     +                           REGIONAL_MONTHLY_MULT(J) *
     +                                  MONTHLY_OTHER_AVAIL_MULT(J),1.0)
                        ENDIF
                     ENDDO
                  ELSE
                     DO J = 1, 12
!
                        MONTHLY_EAVAIL(J) = (100. - EFOR(I))/100.
!
                        IF(FT == 1) THEN
                           MONTHLY_EAVAIL(J) =
     +                        MIN(MONTHLY_EAVAIL(J) *
     +                           REGIONAL_MONTHLY_MULT(J) *
     +                                   MONTHLY_COAL_AVAIL_MULT(J),1.0)
                        ELSEIF(FT ==2) THEN
                           MONTHLY_EAVAIL(J) =
     +                        MIN(MONTHLY_EAVAIL(J) *
     +                           REGIONAL_MONTHLY_MULT(J) *
     +                                   MONTHLY_GAS_AVAIL_MULT(J),1.0)
                        ELSEIF(FT == 3) THEN
                           MONTHLY_EAVAIL(J) =
     +                        MIN(MONTHLY_EAVAIL(J) *
     +                           REGIONAL_MONTHLY_MULT(J) *
     +                                   MONTHLY_OIL_AVAIL_MULT(J),1.0)
                        ELSEIF(FT == 4) THEN
                           MONTHLY_EAVAIL(J) =
     +                        MIN(MONTHLY_EAVAIL(J) *
     +                           REGIONAL_MONTHLY_MULT(J) *
     +                                MONTHLY_NUCLEAR_AVAIL_MULT(J),1.0)
                        ELSEIF(FT == 5) THEN
                           MONTHLY_EAVAIL(J) =
     +                         MONTHLY_EAVAIL(J) *
     +                           REGIONAL_MONTHLY_MULT(J) *
     +                                  MONTHLY_HYDRO_WATER_YEAR_MULT(J)
                        ELSEIF(FT == 6) THEN
                           MONTHLY_EAVAIL(J) =
     +                        MIN(MONTHLY_EAVAIL(J) *
     +                           REGIONAL_MONTHLY_MULT(J) *
     +                                  MONTHLY_OTHER_AVAIL_MULT(J),1.0)
                        ENDIF
                     ENDDO

                  ENDIF

!
!
!
! 11/10/03. DETAILED CONDITIONS.
!
! 02/10/04. TOOK OUT DETAILED_MAIN CONDITION
!
!
! 070806. ADDED EXTRA CONDITION

                  IF( (FREQUENCY_DURATION .AND.
     +                              MONTE_OR_FREQ(I) == 'G').OR.
     +                 (SCHEDULE_FO .AND.MONTE_OR_FREQ(I) == 'F') ) THEN
                     CALL GET_FO_HRS2(
     +                         I,
     +                         K,
     +                         FOR_FREQUENCY(I),
     +                         FOR_DURATION(I),
     +                         MONTHLY_EAVAIL(INT2(1)))

                  ELSEIF(SCHEDULE_FO) THEN ! DO I REALLY NEED TO DO DETAILED REGARDLESS OF DETAILED SWITCH?

                     CALL GET_FO_HRS1(
     +                         I,
     +                         K,
     +                         MONTHLY_EAVAIL,
     +                         YEAR_OR_SCEN)! 11/24/03. K INDEX
                  ENDIF

               ENDDO ! K COUNTER FOR ACTIVE UNITS
               IF(SHADOW_UNITS_ACTIVE) THEN
                  DO I = 1, NUNITS
                     IF(LDTYPE(I) == 'S') THEN
                        MY_SHADOW = SHADOW_UNIT_NUMBER(I)
                        ANNUAL_CL_MAINT_MW(I) =
     +                                     ANNUAL_CL_MAINT_MW(MY_SHADOW)
                        DO MO = 1, 12  ! 7/31/96. GAT. ADDED ANNUAL_CL_CAPACITY.
!
                           ANNUAL_CL_CAPACITY(1,I,MO) =
     +                                ANNUAL_CL_CAPACITY(1,MY_SHADOW,MO)
                           ANNUAL_CL_CAPACITY(2,I,MO) =
     +                                ANNUAL_CL_CAPACITY(2,MY_SHADOW,MO)
!
                           ANNUAL_CL_MAINTENANCE(I,MO) =
     +                               ANNUAL_CL_MAINTENANCE(MY_SHADOW,MO)
                        ENDDO
                        ANNUAL_CL_MAINTENANCE(I,13) =
     +                               ANNUAL_CL_MAINTENANCE(MY_SHADOW,13)
                     ENDIF
                  ENDDO
               ENDIF ! SHADOW UNITS
            ENDIF ! ENDIF FOR MAINT_SCHEDULER

            IF(MAINTENANCE_REPORT() .AND. .NOT. TESTING_PLAN) THEN
               CALL WRITE_YEARS_CMD_FILE(MAINT_INDEX)
            ENDIF
         ENDDO ! PA PLANNING AREA LOOP
!
         IF(1 == 2) THEN

            IF(MAINTENANCE_REPORT_NOT_OPEN) THEN
               MAINTENANCE_REPORT_NOT_OPEN = .FALSE.
               CL_MAINT_NO=MAINTENANCE_HEADER(MW_MAINT_NO,
     +                                        CL_MAINT_REC,
     +                                        MW_MAINT_REC)
            ENDIF
            GROUP_MW_MAINTENANCE = 0.
            TOTAL_MONTHLY_MAINTENANCE = 0.
            DO I = 1, NUNITS
!
! CALCULATE MONTHLY MAINTENANCE CAPACITY BY REPORTING GROUP
!
               IF(ONLINE(I) <= DATE1 .AND. OFLINE(I) >= DATE1) THEN
                  GGI = GENGRP(I)
                  IF(GGI < 1 .OR. GGI > MAX_REPORTING_GROUPS) GGI = 0
                  DO MO = 1, 12
                     MAINTENANCE_CAPACITY = ANNUAL_CL_MAINTENANCE(I,MO)*
     +                                        ANNUAL_CL_CAPACITY(2,I,MO)
                     GROUP_MW_MAINTENANCE(MO,GGI)=MAINTENANCE_CAPACITY +
     +                                      GROUP_MW_MAINTENANCE(MO,GGI)
                     TOTAL_MONTHLY_MAINTENANCE(MO)=MAINTENANCE_CAPACITY+
     +                                     TOTAL_MONTHLY_MAINTENANCE(MO)
                     GROUP_MW_MAINTENANCE(0,GGI)=MAINTENANCE_CAPACITY *
     +                                              DAYS_PER_MONTH(MO) +
     +                                       GROUP_MW_MAINTENANCE(0,GGI)
                     TOTAL_MONTHLY_MAINTENANCE(0)=MAINTENANCE_CAPACITY *
     +                                              DAYS_PER_MONTH(MO) +
     +                                      TOTAL_MONTHLY_MAINTENANCE(0)
                  ENDDO
               ENDIF
!
               IF(EFOR(I) < 0.) THEN
                  FOR_MT = (100. - GET_VAR(EFOR(I),YEAR,UNITNM(I)))/100.
               ELSE
                  FOR_MT = (100. - EFOR(I))/100.
               ENDIF
               WRITE(CL_MAINT_NO,REC=CL_MAINT_REC)
     +              PRT_ENDPOINT(),
     +              FLOAT(YEAR+BASE_YEAR),
     +              UNITNM(I),
     +              (ANNUAL_CL_MAINTENANCE(I,J)*100.,J=1,12),
     +              100.*ANNUAL_CL_MAINTENANCE(I,13)/DAYS_PER_MONTH(13),
     +              (DAYS_PER_MONTH(J) *
     +                          ANNUAL_CL_MAINTENANCE(I,J),J=1,12),
     +              ANNUAL_CL_MAINTENANCE(I,13),
     +              (100.*(1. - ANNUAL_CL_MAINTENANCE(I,J))*
     +                                                   FOR_MT,J=1,12),
     +              (1.-ANNUAL_CL_MAINTENANCE(I,13)/DAYS_PER_MONTH(13))
     +                                                     *100.* FOR_MT
               CL_MAINT_REC = CL_MAINT_REC + 1
            ENDDO
            DO GGI = 0,MAX_REPORTING_GROUPS
               GROUP_MW_MAINTENANCE(0,GGI)=GROUP_MW_MAINTENANCE(0,GGI)/
     +                                                DAYS_PER_MONTH(13)
               WRITE(MW_MAINT_NO,REC=MW_MAINT_REC)
     +              PRT_ENDPOINT(),
     +              FLOAT(YEAR+BASE_YEAR),
     +              GROUP_NAME(GGI),
     +              (GROUP_MW_MAINTENANCE(MO,GGI),MO=0,12)
               MW_MAINT_REC = MW_MAINT_REC + 1
            ENDDO
            TOTAL_MONTHLY_MAINTENANCE(0)= TOTAL_MONTHLY_MAINTENANCE(0)/
     +                                                DAYS_PER_MONTH(13)
            WRITE(MW_MAINT_NO,REC=MW_MAINT_REC)
     +             PRT_ENDPOINT(),
     +              FLOAT(YEAR+BASE_YEAR),
     +              'Totals   ',
     +              (TOTAL_MONTHLY_MAINTENANCE(MO),MO=0,12)
            MW_MAINT_REC = MW_MAINT_REC + 1
         ENDIF
!
         CALC_ANNUAL_CAP_AND_MAINT = .TRUE.
      RETURN
!
!***********************************************************************
      ENTRY GET_MAX_FO_PER_MONTH
!***********************************************************************
         GET_MAX_FO_PER_MONTH = MAX_FO_PER_MONTH
      RETURN
! CALLED ANNUALLY FROM TRANSOBJ.FOR
!***********************************************************************
      ENTRY PROCESS_MARKET_RESOURCES(R_TG,R_AREA_PRICE)
!***********************************************************************
!
! 09/23/03. CHANGED TO CYCLE OVER MULTIPLE TRANSACTION GROUPS (R_TG).
! ONLY ACTIVE IN TRANSACT C.
!
         PROCESS_MARKET_RESOURCES = .FALSE.
         DO TG = 1, R_TG
            IF(YES_RUN_TRANSACT() .AND. (MARKET_COUNT > 0 .OR.
     +                       GET_NUM_ANNUAL_DERIVATIVES(TG) > 0)) THEN
!
               IF(R_AREA_PRICE) THEN
                  FILE_NAME = trim(PRB_FILE_DIRECTORY())//"PRB"//
     +                       trim(GET_HOURLY_PRICE_NAME(TG))//
     +                        LOAD_FILE_CHAR_EXT(END_POINT)//".P"//
     +                   LOAD_FILE_CHAR_EXT(GET_MARKET_PRICE_YEAR(YEAR))
!
! 10/02/03. ADDED FOR FE. GO GET THE FIRST END_POINT AGAIN.
!
                  INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
                  IF(.NOT. FILE_EXISTS .AND. END_POINT > 1) THEN
                     FILE_NAME = trim(PRB_FILE_DIRECTORY())//"PRB"//
     +                       trim(GET_HOURLY_PRICE_NAME(TG))//
     +                        LOAD_FILE_CHAR_EXT(INT2(1))//".P"//
     +                   LOAD_FILE_CHAR_EXT(GET_MARKET_PRICE_YEAR(YEAR))
                  ENDIF
!
               ELSE
                  CALL GET_NEW_MARKET_PRICE_NAME(MARKET_PRICE_NAME)
                  FILE_NAME = trim(PRB_FILE_DIRECTORY())//"PRB"//
     +                              trim(MARKET_PRICE_NAME)//".P"//
     +                   LOAD_FILE_CHAR_EXT(GET_MARKET_PRICE_YEAR(YEAR))
               ENDIF
!
               CALL READ_USER_MARKET_ANNUAL(YEAR,
     +                              YEAR+BASE_YEAR,
     +                              TG, ! TRANS_GROUP
     +                              FILE_NAME, ! PRICE FILE NAME
     +                              END_POINT,
     +                              R_AREA_PRICE) ! ENDPOINT
!

!
               TEMP_L = ANNUAL_CALL_PUT_CAPACITY(YEAR+BASE_YEAR,TG)

!
               IF(MARKET_COUNT > 0) THEN
                  CALL PickHrStrInitArrays(MARKET_COUNT,
     +                               INT2(2),
     +                               INT2(8800))
                  CALL SortPricesForYear(YEAR+BASE_YEAR)
                  DO I = 1, MARKET_COUNT
                     J = MARKET_RESOURCE_INDEX(I)
!
                     LOCAL_COEFF(1) = COEFF(1,J)
                     LOCAL_COEFF(2) = COEFF(2,J)
                     LOCAL_COEFF(3) = COEFF(3,J)
!
                     CALL GET_MONTHLY_DISP_COST_BY_UNIT(
     +                                            J,
     +                                            LOCAL_COEFF,
     +                                            MONTHLY_DISPATCH_COST)

                     DO MO = 1, 12
                        MONTHLY_CAPACITY(1,MO) =
     +                                        ANNUAL_CL_CAPACITY(1,J,MO)
                        MONTHLY_CAPACITY(2,MO) =
     +                     MAX(0.,ANNUAL_CL_CAPACITY(2,J,MO) -
     +                                       ANNUAL_CL_CAPACITY(1,J,MO))
                        IF(ANNUAL_CL_MAINTENANCE(J,MO) == 1.) THEN
                           MONTHLY_INTEGER(MO) = 0
                        ELSE
                           IF(MONTHLY_CAPACITY_POINTER(J) < 0) THEN
!
                              TEMP_R4 = GET_VAR(FLOAT(
     +                                     MONTHLY_CAPACITY_POINTER(J)),
     +                                                     MO,UNITNM(J))
                              IF(TEMP_R4 < .001) THEN
                                 MONTHLY_INTEGER(MO) = 0
                              ELSE
                                 MONTHLY_INTEGER(MO) = 1
                              ENDIF
                           ELSE
                              MONTHLY_INTEGER(MO) = 1
                           ENDIF
                        ENDIF
                     ENDDO
!
! 10/29/03. TESTING.
!
                     STARTS_ONLY = .TRUE.
!
                     CALL PickStrikesForUnit(
     +                  I,
     +                  2_2,
     +                  MONTHLY_CAPACITY,
     +                  MONTHLY_DISPATCH_COST,
     +                  INT2(MINIMUM_CAPACITY_FACTOR(J)),
     +                  INT2(MAXIMUM_CAPACITY_FACTOR(J)),
     +                  MONTHLY_INTEGER,
     +                  STARTS_ONLY)
                  ENDDO
               ENDIF
               PROCESS_MARKET_RESOURCES = .TRUE.
            ENDIF
         ENDDO
       RETURN
!***********************************************************************
      ENTRY IS_A_MARKET_RESOURCE(R_UNIT_NO)
!***********************************************************************
         IS_A_MARKET_RESOURCE = MARKET_RESOURCE(R_UNIT_NO) == 'T' .OR.
     +                                 MARKET_RESOURCE(R_UNIT_NO) == 'D'
      RETURN
!***********************************************************************
      ENTRY IS_STRICT_MARKET_RESOURCE(R_UNIT_NO,R_MARKET_RESOURCE_STR)
!***********************************************************************

         R_MARKET_RESOURCE_STR = MARKET_RESOURCE(R_UNIT_NO)
         IF(MARKET_RESOURCE(R_UNIT_NO) == 'M') THEN
            IS_STRICT_MARKET_RESOURCE = .TRUE.
         ELSE
            IS_STRICT_MARKET_RESOURCE = .FALSE.
         ENDIF
      RETURN
!***********************************************************************
      ENTRY GET_MARKET_RESOURCE_INDEX(R_UNIT_NO)
!***********************************************************************
         GET_MARKET_RESOURCE_INDEX = MARKET_RESOURCE_INDEX(R_UNIT_NO)
      RETURN
!***********************************************************************
      ENTRY GET_MARKET_RESOURCE_COUNTER(R_UNIT_NO)
!***********************************************************************
         IF(ALLOCATED(MARKET_RESOURCE_COUNTER) .AND.
     +                                         R_UNIT_NO <= NUNITS) THEN
            GET_MARKET_RESOURCE_COUNTER =
     +                               MARKET_RESOURCE_COUNTER(R_UNIT_NO)
         ELSE
            GET_MARKET_RESOURCE_COUNTER = 0
         ENDIF
      RETURN
!***********************************************************************
      ENTRY GET_FUEL_DELIVERY(  R_I,
     +                          R_FUEL_DELIVERY_1,
     +                          R_FUEL_DELIVERY_2,
     +                          R_FUEL_DELIVERY_3)
!***********************************************************************
         R_FUEL_DELIVERY_1 = P_FUEL_DELIVERY_1(R_I)
         R_FUEL_DELIVERY_2 = P_FUEL_DELIVERY_2(R_I)
         R_FUEL_DELIVERY_3 = P_FUEL_DELIVERY_3(R_I)
         GET_FUEL_DELIVERY = PBTUCT(R_I)
      RETURN
!***********************************************************************
      ENTRY GET_SECOND_FUEL_DELIVERY(
     +                          R_I,
     +                          R_FUEL_DELIVERY_1,
     +                          R_FUEL_DELIVERY_2,
     +                          R_FUEL_DELIVERY_3)
!***********************************************************************
         R_FUEL_DELIVERY_1 = S_FUEL_DELIVERY(R_I)
         R_FUEL_DELIVERY_2 = S_FUEL_DELIVERY_2(R_I)
         R_FUEL_DELIVERY_3 = s_fuel_delivery_3(R_I)
         GET_SECOND_FUEL_DELIVERY = SBTUCT(R_I)
      RETURN
!***********************************************************************
      ENTRY UNSCHEDULED_MAINT_HOURS(R_UNIT_NO)
!***********************************************************************
         IF(MAINT_DAYS(R_UNIT_NO) /= 0.) THEN
            IF(MAINT_DAYS(R_UNIT_NO) < 0.) THEN
               UNSCHEDULED_MAINT_HOURS = GET_VAR(MAINT_DAYS(R_UNIT_NO),
     +                                           YEAR,UNITNM(R_UNIT_NO))
            ELSE
               UNSCHEDULED_MAINT_HOURS = MAINT_DAYS(R_UNIT_NO)
            ENDIF
         ENDIF
         UNSCHEDULED_MAINT_HOURS = UNSCHEDULED_MAINT_HOURS*7.*24.
      RETURN
!***********************************************************************
      ENTRY UPDATE_PERIOD_CAPACITY(R_I,R_J)
!***********************************************************************
         UPDATE_PERIOD_CAPACITY = .FALSE.
         IF(.NOT. ALLOCATED(ANNUAL_CL_CAPACITY)) RETURN
         I = R_I
         J = R_J
         IF(I/=NUNITS) THEN
            UPDATE_PERIOD_CAPACITY = .FALSE.
            RETURN
         ENDIF

         DO J = 1, NUNITS
            DO I = 1, 2
              MW(I,J) = ANNUAL_CL_CAPACITY(I,J,R_J)
            ENDDO
         ENDDO
         UPDATE_PERIOD_CAPACITY = .TRUE.
      RETURN
!***********************************************************************
      ENTRY GET_ANNUAL_CL_CAPACITY(R_I,R_J,R_MONTH)
!***********************************************************************
         IF(ALLOCATED(ANNUAL_CL_CAPACITY) .AND. R_J <= NUNITS) THEN
            GET_ANNUAL_CL_CAPACITY = ANNUAL_CL_CAPACITY(R_I,R_J,R_MONTH)
         ELSE
            GET_ANNUAL_CL_CAPACITY = 0.
         ENDIF
      RETURN
!***********************************************************************
      ENTRY GET_MAINTENANCE_RATE_FOR_MONTH(R_UNIT_NO,R_MONTH)
!***********************************************************************
         IF(ALLOCATED(ANNUAL_CL_MAINTENANCE)) THEN
            GET_MAINTENANCE_RATE_FOR_MONTH =
     +                          ANNUAL_CL_MAINTENANCE(R_UNIT_NO,R_MONTH)
         ELSE
            GET_MAINTENANCE_RATE_FOR_MONTH = 0.
         ENDIF
      RETURN
!***********************************************************************
      ENTRY UPDATE_PERIOD_MAINTENANCE(R_I,R_J,MAINTENANCE_RATE)
!***********************************************************************
         I = R_I
         J = R_J
         IF(I/=NUNITS) THEN
            UPDATE_PERIOD_MAINTENANCE = .FALSE.
            RETURN
         ENDIF

         DO I = 1, NUNITS
            MAINTENANCE_RATE(I) = ANNUAL_CL_MAINTENANCE(I,J)
         ENDDO
         UPDATE_PERIOD_MAINTENANCE = .TRUE.
      RETURN
!***********************************************************************
      ENTRY UPDATE_PR_ESCALATIONS(R_YEAR,R_POINTER)
!***********************************************************************
!
!
      VOID_INT2 =
     +     ESCALATE_ONE_VALUE_FOR_ADDITIONS(PBTUCT(R_POINTER),R_POINTER,
     +                       PFESCR(R_POINTER),UNITNM(R_POINTER),R_YEAR)
      VOID_INT2 =
     +     ESCALATE_ONE_VALUE_FOR_ADDITIONS(SBTUCT(R_POINTER),R_POINTER,
     +                       SFESCR(R_POINTER),UNITNM(R_POINTER),R_YEAR)
      VOID_INT2 =
     +      ESCALATE_ONE_VALUE_FOR_ADDITIONS(EMISS_FUEL_COST(R_POINTER),
     +                     R_POINTER,EMISS_FUEL_ESCAL(R_POINTER),
     +                                         UNITNM(R_POINTER),R_YEAR)
      VOID_INT2 =
     + ESCALATE_ONE_VALUE_FOR_ADDITIONS(VCPMWH_IN(R_POINTER),
     +  R_POINTER,
     +                       OMESCR(R_POINTER),UNITNM(R_POINTER),R_YEAR)
      VOID_INT2 =
     +    ESCALATE_ONE_VALUE_FOR_ADDITIONS(CL_AI_ENERGY_RATE(R_POINTER),
     +               R_POINTER,CL_AI_ENERGY_ESCALATOR(R_POINTER),
     +                                         UNITNM(R_POINTER),R_YEAR)
      VOID_INT2 =
     +  ESCALATE_ONE_VALUE_FOR_ADDITIONS(CL_AI_CAPACITY_RATE(
     +  R_POINTER),
     +            R_POINTER,CL_AI_CAPACITY_ESCALATOR(R_POINTER),
     +                                         UNITNM(R_POINTER),R_YEAR)
      VOID_INT2 =
     + ESCALATE_ONE_VALUE_FOR_ADDITIONS(FIXED_COST_IN(R_POINTER),
     +                        R_POINTER,FIXED_COST_ESCALATOR(R_POINTER),
     +                        UNITNM(R_POINTER),R_YEAR)
      VOID_INT2 =
     + ESCALATE_ONE_VALUE_FOR_ADDITIONS(ANNUAL_CL_FIXED_COST(
     +  R_POINTER),
     +            R_POINTER,ANNUAL_CL_FIXED_COST_ESC(R_POINTER),
     +                                         UNITNM(R_POINTER),R_YEAR)

      UPDATE_PR_ESCALATIONS = R_POINTER
!
      RETURN
!***********************************************************************

      ENTRY INIT_MON_MDS_CL_UNITS(R_NUNITS)
!***********************************************************************
!

            IF(ALLOCATED(MON_MDS_CL_UNIT_FUEL_COST))
     +         DEALLOCATE(MON_MDS_CL_UNIT_EMISSIONS, ! DONE
     +                    MON_MDS_CL_UNIT_EMISSIONS_COST,
     +                    MON_MDS_CL_UNIT_CAPACITY, ! DONE
     +                    MON_MDS_CL_UNIT_ENERGY, ! DONE
     +                    MON_MDS_CL_UNIT_FUEL_COST, ! DONE
     +                    MON_MDS_CL_UNIT_VAR_COST, ! DONE
     +                    MON_MDS_CL_UNIT_FIXED_COST, ! DONE
     +                    MON_MDS_NUC_FUEL_ADDER_COST, ! DONE
     +                    MON_MDS_ECO_SALES_REV_FROM, ! DONE
     +                    MON_MDS_ECO_SALES_ENRG_FROM, ! DONE
     +                    MON_MDS_ECO_PUCH_COST_FROM, ! DONE
     +                    MON_MDS_ECO_PUCH_ENRG_FROM, ! DONE
     +                    MON_MDS_ICAP_REVENUES,
     +                    MON_MDS_CL_UNIT_MMBTUS) ! DONE
!
            ALLOCATE(MON_MDS_CL_UNIT_EMISSIONS(
     +                          NUMBER_OF_EMISSION_TYPES,R_NUNITS,0:12))
            ALLOCATE(MON_MDS_CL_UNIT_EMISSIONS_COST(
     +                          NUMBER_OF_EMISSION_TYPES,R_NUNITS,0:12))
            ALLOCATE(MON_MDS_CL_UNIT_CAPACITY(R_NUNITS,0:12))
            ALLOCATE(MON_MDS_CL_UNIT_ENERGY(R_NUNITS,0:12))
            ALLOCATE(MON_MDS_CL_UNIT_FUEL_COST(R_NUNITS,0:12))
            ALLOCATE(MON_MDS_CL_UNIT_VAR_COST(R_NUNITS,0:12))
            ALLOCATE(MON_MDS_CL_UNIT_FIXED_COST(R_NUNITS,0:12))
            ALLOCATE(MON_MDS_NUC_FUEL_ADDER_COST(R_NUNITS,0:12))
            ALLOCATE(MON_MDS_ECO_SALES_REV_FROM(R_NUNITS,0:12))
            ALLOCATE(MON_MDS_ECO_SALES_ENRG_FROM(R_NUNITS,0:12))
            ALLOCATE(MON_MDS_ECO_PUCH_COST_FROM(R_NUNITS,0:12))
            ALLOCATE(MON_MDS_ECO_PUCH_ENRG_FROM(R_NUNITS,0:12))
            ALLOCATE(MON_MDS_ICAP_REVENUES(R_NUNITS,0:12))
            ALLOCATE(MON_MDS_CL_UNIT_MMBTUS(R_NUNITS,0:12))

         MON_MDS_CL_UNIT_CAPACITY = 0.
         MON_MDS_CL_UNIT_ENERGY = 0.
         MON_MDS_CL_UNIT_FUEL_COST = 0.
         MON_MDS_CL_UNIT_VAR_COST = 0.
         MON_MDS_CL_UNIT_FIXED_COST = 0.
         MON_MDS_NUC_FUEL_ADDER_COST = 0.
         MON_MDS_ECO_SALES_REV_FROM = 0.
         MON_MDS_ECO_SALES_ENRG_FROM = 0.
         MON_MDS_ECO_PUCH_COST_FROM = 0.
         MON_MDS_ECO_PUCH_ENRG_FROM = 0.
         MON_MDS_ICAP_REVENUES = 0.
!
         MON_MDS_CL_UNIT_MMBTUS = 0.0D0
!
         MON_MDS_CL_UNIT_EMISSIONS = 0.
         MON_MDS_CL_UNIT_EMISSIONS_COST = 0.
!
         INIT_MON_MDS_CL_UNITS = .TRUE.

         PRICE_ONLY_WHOLESALE_REV = APPLY_TRANS_REV_TO_WHOLESALE()

         DETAILED_TRANSFER_PRICING = YES_DETAILED_TRANSFER_PRICING()
!
      RETURN
!***********************************************************************
      ENTRY MON_MDS_CL_VAR(R_ISEAS,R_YR,R_UNITNO,R_FUEL_COST,
     +                     R_ENRG,R_HEAT,R_EMIS,R_VOM_COST)
!***********************************************************************
! MONTH
          MON_MDS_CL_UNIT_ENERGY(R_UNITNO,R_ISEAS) = R_ENRG +
     +                     MON_MDS_CL_UNIT_ENERGY(R_UNITNO,R_ISEAS)
          MON_MDS_CL_UNIT_FUEL_COST(R_UNITNO,R_ISEAS) =
     +                MON_MDS_CL_UNIT_FUEL_COST(R_UNITNO,R_ISEAS) +
     +                                                  R_FUEL_COST
          MON_MDS_CL_UNIT_VAR_COST(R_UNITNO,R_ISEAS) =
     +             MON_MDS_CL_UNIT_VAR_COST(R_UNITNO,R_ISEAS) +
     +                                R_VOM_COST
          MON_MDS_CL_UNIT_MMBTUS(R_UNITNO,R_ISEAS) =
     +                   MON_MDS_CL_UNIT_MMBTUS(R_UNITNO,R_ISEAS) +
     +                                                       R_HEAT
          MON_MDS_CL_UNIT_EMISSIONS(:,R_UNITNO,R_ISEAS) =
     +                MON_MDS_CL_UNIT_EMISSIONS(:,R_UNITNO,R_ISEAS)
     +                + R_EMIS(:)

         DO I4 =  1, 5
        MON_MDS_CL_UNIT_EMISSIONS_COST(I4,R_UNITNO,R_ISEAS) =
     +      MON_MDS_CL_UNIT_EMISSIONS_COST(I4,R_UNITNO,R_ISEAS)
     +           + R_EMIS(I4) * 0.002 *
     +     TRANS_DISPATCH_EMIS_ADDER(I4,R_ISEAS,R_YR,R_UNITNO)
          MON_MDS_CL_UNIT_EMISSIONS_COST(I4,R_UNITNO,0) =
     +      MON_MDS_CL_UNIT_EMISSIONS_COST(I4,R_UNITNO,0)
     +       + R_EMIS(I4) * 0.002 *
     +      TRANS_DISPATCH_EMIS_ADDER(I4,R_ISEAS,R_YR,R_UNITNO)
         ENDDO
! ANNUAL
               MON_MDS_CL_UNIT_ENERGY(R_UNITNO,0) = R_ENRG
     +                              + MON_MDS_CL_UNIT_ENERGY(R_UNITNO,0)
               MON_MDS_CL_UNIT_FUEL_COST(R_UNITNO,0) =
     +                             MON_MDS_CL_UNIT_FUEL_COST(R_UNITNO,0)
     +                             + R_FUEL_COST
               MON_MDS_CL_UNIT_VAR_COST(R_UNITNO,0) =
     +                              MON_MDS_CL_UNIT_VAR_COST(R_UNITNO,0)
     +                              + R_VOM_COST
               MON_MDS_CL_UNIT_MMBTUS(R_UNITNO,0) =
     +                                MON_MDS_CL_UNIT_MMBTUS(R_UNITNO,0)
     +                                + R_HEAT
               MON_MDS_CL_UNIT_EMISSIONS(:,R_UNITNO,0) =
     +                   MON_MDS_CL_UNIT_EMISSIONS(:,R_UNITNO,0)
     +                   + R_EMIS(:)

            MON_MDS_CL_VAR = .TRUE.
!
      RETURN
!
!***********************************************************************
      ENTRY MON_MDS_CL_FIXED(R_ISEAS,R_UNITNO,R_FIXED_COST,R_CAP)
!***********************************************************************
! MONTH
        MON_MDS_CL_UNIT_CAPACITY(R_UNITNO,R_ISEAS) = R_CAP +
     +                        MON_MDS_CL_UNIT_CAPACITY(R_UNITNO,R_ISEAS)
        MON_MDS_CL_UNIT_FIXED_COST(R_UNITNO,R_ISEAS) =
     +                  MON_MDS_CL_UNIT_FIXED_COST(R_UNITNO,R_ISEAS) +
     +                                                      R_FIXED_COST
!
!     SALES
         MON_MDS_ECO_SALES_REV_FROM(R_UNITNO,R_ISEAS) =
     +                    MON_MDS_ECO_SALES_REV_FROM(R_UNITNO,R_ISEAS) +
     +                                  MON_ECO_SALES_REV_FROM(R_UNITNO)
         MON_MDS_ECO_SALES_ENRG_FROM(R_UNITNO,R_ISEAS) =
     +                   MON_MDS_ECO_SALES_ENRG_FROM(R_UNITNO,R_ISEAS) +
     +                                 MON_ECO_SALES_ENRG_FROM(R_UNITNO)
!     PURCHASES

         MON_MDS_ECO_PUCH_COST_FROM(R_UNITNO,R_ISEAS) =
     +                    MON_MDS_ECO_PUCH_COST_FROM(R_UNITNO,R_ISEAS) +
     +                                  MON_ECO_PUCH_COST_FROM(R_UNITNO)
         MON_MDS_ECO_PUCH_ENRG_FROM(R_UNITNO,R_ISEAS) =
     +                    MON_MDS_ECO_PUCH_ENRG_FROM(R_UNITNO,R_ISEAS) +
     +                                  MON_ECO_PUCH_ENRG_FROM(R_UNITNO)

! ANNUAL
        MON_MDS_CL_UNIT_CAPACITY(R_UNITNO,0) = R_CAP +
     +                        MON_MDS_CL_UNIT_CAPACITY(R_UNITNO,0)
        MON_MDS_CL_UNIT_FIXED_COST(R_UNITNO,0) =
     +                  MON_MDS_CL_UNIT_FIXED_COST(R_UNITNO,0) +
     +                                                      R_FIXED_COST
!
!     SALES
         MON_MDS_ECO_SALES_REV_FROM(R_UNITNO,0) =
     +                    MON_MDS_ECO_SALES_REV_FROM(R_UNITNO,0) +
     +                                  MON_ECO_SALES_REV_FROM(R_UNITNO)
         MON_MDS_ECO_SALES_ENRG_FROM(R_UNITNO,0) =
     +                   MON_MDS_ECO_SALES_ENRG_FROM(R_UNITNO,0) +
     +                                 MON_ECO_SALES_ENRG_FROM(R_UNITNO)
!     PURCHASES

         MON_MDS_ECO_PUCH_COST_FROM(R_UNITNO,0) =
     +                    MON_MDS_ECO_PUCH_COST_FROM(R_UNITNO,0) +
     +                                  MON_ECO_PUCH_COST_FROM(R_UNITNO)
         MON_MDS_ECO_PUCH_ENRG_FROM(R_UNITNO,0) =
     +                    MON_MDS_ECO_PUCH_ENRG_FROM(R_UNITNO,0) +
     +                                  MON_ECO_PUCH_ENRG_FROM(R_UNITNO)

! ICAP REVENUES
         MON_MDS_ICAP_REVENUES(R_UNITNO,R_ISEAS) =
     +                MON_MDS_ICAP_REVENUES(R_UNITNO,R_ISEAS) +
     +                                   MONTHLY_ICAP_REVENUES(R_UNITNO)
         MON_MDS_ICAP_REVENUES(R_UNITNO,0) =
     +                MON_MDS_ICAP_REVENUES(R_UNITNO,0) +
     +                                   MONTHLY_ICAP_REVENUES(R_UNITNO)
!
         MON_MDS_CL_FIXED = .TRUE.
      RETURN
!***********************************************************************
      ENTRY MON_MDS_NUC_ADDER(R_ISEAS,R_UNITNO,
     +                        R_NUC_UNIT_FUEL_ADDER_COST)
!***********************************************************************
! MONTHLY
         MON_MDS_NUC_FUEL_ADDER_COST(R_UNITNO,R_ISEAS) =
     +                                        R_NUC_UNIT_FUEL_ADDER_COST
! ANNUAL
         MON_MDS_NUC_FUEL_ADDER_COST(R_UNITNO,0) =
     +                  MON_MDS_NUC_FUEL_ADDER_COST(R_UNITNO,0) +
     +                     MON_MDS_NUC_FUEL_ADDER_COST(R_UNITNO,R_ISEAS)
         MON_MDS_NUC_ADDER = .TRUE.
      RETURN
!***********************************************************************
      ENTRY RETURN_MAX_CL_CLASS_NUM
!***********************************************************************
         RETURN_MAX_CL_CLASS_NUM = MAX_CAP_LIMITED_CLASS_ID_NUM
      RETURN
!***********************************************************************
      ENTRY GET_CL_ASSET_CLASS_INFO(R_ASSET_CLASS_NUM,R_ASSET_VECTOR)
!***********************************************************************
         DO I = 1, NUNITS
            R_ASSET_CLASS_NUM(I) = ASSET_CLASS_NUM(I)
            R_ASSET_VECTOR(I) = ASSET_CLASS_VECTOR(I)
         ENDDO
         GET_CL_ASSET_CLASS_INFO = NUMBER_OF_CAP_LIMITED_CLASSES
      RETURN
!***********************************************************************
      ENTRY SET_UP_CL_CLASS_ARRAYS
!***********************************************************************
!
         IF(ALLOCATED(ASSET_CLASS_POINTER))
     +                                   DEALLOCATE(ASSET_CLASS_POINTER)
         CALL RETURN_INITIALIZATION_CLASSES(
     +                                    NUMBER_OF_CAP_LIMITED_CLASSES,
     +                                     MAX_CAP_LIMITED_CLASS_ID_NUM)
         IF(MAX_CAP_LIMITED_CLASS_ID_NUM > 0) THEN
            ALLOCATE(ASSET_CLASS_POINTER(MAX_CAP_LIMITED_CLASS_ID_NUM))
            CALL RETURN_INITIALIZATION_POINTER(ASSET_CLASS_POINTER)
         ENDIF
         IF(ALLOCATED(CL_ANN_CLASS_FUEL_COST))
     +                    DEALLOCATE(CL_ANN_CLASS_FUEL_COST,
     +                               CL_ANN_MULT_FUEL_COST,
     +                               FE_ANN_MULT_FUEL_COST,
     +                               NF_FUEL_LEASED_BY_CLASS,
     +                               NUC_FUEL_LEASED_BURN_BY_CLASS,
     +                               NUC_FUEL_OWNED_BURN_BY_CLASS,
     +                               NUCLEAR_MWH_BY_CLASS,
     +                               NUCLEAR_MMBTU_BY_CLASS,
     +                               NF_FUEL_OWNED_BY_CLASS,
     +                               CL_ANN_CLASS_VAR_COST,
     +                               CL_ANN_CLASS_FIXED_COST,
     +                               CL_ANN_CLASS_REVENUE,
     +                               CL_ANN_CLASS_CAPACITY,
     +                               CL_ANN_CLASS_PURCHASES,
     +                               CL_ANN_CLASS_ENERGY,
     +                               CL_ANN_CLASS_EMISSIONS,
     +                               CL_ANN_MULT_VAR_COST,
     +                               CL_ANN_MULT_FIXED_COST,
     +                               CL_ANN_MULT_REVENUE,
     +                               CL_ANN_MULT_CAPACITY,
     +                               CL_ANN_MULT_PURCHASES,
     +                               CL_ANN_MULT_ENERGY,
     +                               INTRA_COMPANY_REVENUES,
     +                               INTRA_COMPANY_NF_BURN,
     +                               MON_MDS_CL_CLASS_FUEL_COST,
     +                               MON_MDS_CL_MULT_FUEL_COST,
     +                               MON_MDS_CL_MULT_VAR_COST,
     +                               MON_MDS_CL_MULT_FIXED_COST,
     +                               MON_MDS_CL_MULT_CAPACITY,
     +                               MON_MDS_CL_MULT_ENERGY,
     +                               MON_MDS_CL_MULT_PURCHASES,
     +                               MON_MDS_CL_CLASS_VAR_COST,
     +                               MON_MDS_CL_CLASS_FIXED_COST,
     +                               MON_MDS_CL_CLASS_REVENUE,
     +                               MON_MDS_CL_CAP_REVENUE,
     +                               MON_MDS_CL_CLASS_CAPACITY,
     +                               MON_MDS_CL_CLASS_PURCHASES,
     +                               MON_MDS_CL_CAP_PURCHASES,
     +                               MON_MDS_CL_CLASS_ENERGY,
     +                               MON_MDS_CL_CLASS_EMISSIONS,
     +                               MON_MDS_CL_CLASS_EMISSIONS_COST,
     +                               MON_MDS_INTRA_COMPANY_REVENUES,
     +                               MON_MDS_INTRA_COMPANY_NF_BURN,
     +                               MON_MDS_NF_FUEL_LEASED_BC,
     +                               MON_MDS_NF_FUEL_OWNED_BC,
     +                               MON_MDS_NUC_FUEL_LEASE_BURN_BC,
     +                               MON_MDS_NUC_FUEL_OWNED_BURN_BC,
     +                               MON_MDS_NUCLEAR_MWH_BY_CLASS,
     +                               MON_MDS_NUCLEAR_MMBTU_BY_CLASS,
     +                               MON_MDS_ICAP_REV_BY_CLASS,
     +                               MONTHLY_AC_WHOLESALE_PROD_COST,
     +                               MONTHLY_AC_ECITY_VAR_PROD_COST,
     +                               MONTHLY_AC_ECITY_NEW_FIX_COST)
         ALLOCATE(
     +       CL_ANN_CLASS_FUEL_COST(0:NUMBER_OF_CAP_LIMITED_CLASSES,4))
         ALLOCATE(CL_ANN_MULT_FUEL_COST(0:NUMBER_OF_CAP_LIMITED_CLASSES,
     +                                             NUM_FUEL_CATEGORIES))
         ALLOCATE(FE_ANN_MULT_FUEL_COST(0:NUMBER_OF_CAP_LIMITED_CLASSES,
     +                                             NUM_FUEL_CATEGORIES))
         ALLOCATE(NF_FUEL_LEASED_BY_CLASS(
     +                               0:NUMBER_OF_CAP_LIMITED_CLASSES,4))
         ALLOCATE(NF_FUEL_OWNED_BY_CLASS(
     +                               0:NUMBER_OF_CAP_LIMITED_CLASSES,4))
         ALLOCATE(CL_ANN_CLASS_VAR_COST(
     +                               0:NUMBER_OF_CAP_LIMITED_CLASSES,4))
         ALLOCATE(CL_ANN_CLASS_FIXED_COST(
     +                               0:NUMBER_OF_CAP_LIMITED_CLASSES,4))
         ALLOCATE(CL_ANN_CLASS_CAPACITY(
     +                               0:NUMBER_OF_CAP_LIMITED_CLASSES,4))
         ALLOCATE(CL_ANN_CLASS_ENERGY(
     +                               0:NUMBER_OF_CAP_LIMITED_CLASSES,4))
         ALLOCATE(CL_ANN_CLASS_REVENUE(
     +                               0:NUMBER_OF_CAP_LIMITED_CLASSES,5))
         ALLOCATE(CL_ANN_CLASS_PURCHASES(
     +                               0:NUMBER_OF_CAP_LIMITED_CLASSES,5))
         ALLOCATE(CL_ANN_CLASS_EMISSIONS(NUMBER_OF_EMISSION_TYPES,
     +                                 0:NUMBER_OF_CAP_LIMITED_CLASSES))
         ALLOCATE(CL_ANN_MULT_VAR_COST(0:NUMBER_OF_CAP_LIMITED_CLASSES,
     +                                             NUM_FUEL_CATEGORIES))
         ALLOCATE(
     +           CL_ANN_MULT_FIXED_COST(0:NUMBER_OF_CAP_LIMITED_CLASSES,
     +                                             NUM_FUEL_CATEGORIES))
         ALLOCATE(CL_ANN_MULT_REVENUE(0:NUMBER_OF_CAP_LIMITED_CLASSES,
     +                                             NUM_FUEL_CATEGORIES))
         ALLOCATE(CL_ANN_MULT_CAPACITY(0:NUMBER_OF_CAP_LIMITED_CLASSES,
     +                                             NUM_FUEL_CATEGORIES))
         ALLOCATE(CL_ANN_MULT_ENERGY(0:NUMBER_OF_CAP_LIMITED_CLASSES,
     +                                             NUM_FUEL_CATEGORIES))
         ALLOCATE(CL_ANN_MULT_PURCHASES(0:
     +  NUMBER_OF_CAP_LIMITED_CLASSES,
     +                                             NUM_FUEL_CATEGORIES))
         ALLOCATE(NUC_FUEL_LEASED_BURN_BY_CLASS(
     +                                 0:NUMBER_OF_CAP_LIMITED_CLASSES))
         ALLOCATE(NUC_FUEL_OWNED_BURN_BY_CLASS(
     +                                 0:NUMBER_OF_CAP_LIMITED_CLASSES))
         ALLOCATE(NUCLEAR_MWH_BY_CLASS(0:NUMBER_OF_CAP_LIMITED_CLASSES))
         ALLOCATE(NUCLEAR_MMBTU_BY_CLASS(
     +                                 0:NUMBER_OF_CAP_LIMITED_CLASSES))
         ALLOCATE(INTRA_COMPANY_REVENUES(0:1024,0:3))
         ALLOCATE(INTRA_COMPANY_NF_BURN(0:1024))
         ALLOCATE(MON_MDS_CL_CLASS_FUEL_COST
     +                         (0:NUMBER_OF_CAP_LIMITED_CLASSES,4,0:12))
         ALLOCATE(MON_MDS_CL_MULT_FUEL_COST
     +                         (0:NUMBER_OF_CAP_LIMITED_CLASSES,
     +                                        NUM_FUEL_CATEGORIES,0:12))
         ALLOCATE(MON_MDS_CL_MULT_VAR_COST
     +                         (0:NUMBER_OF_CAP_LIMITED_CLASSES,
     +                                        NUM_FUEL_CATEGORIES,0:12))
         ALLOCATE(MON_MDS_CL_MULT_FIXED_COST
     +                         (0:NUMBER_OF_CAP_LIMITED_CLASSES,
     +                                        NUM_FUEL_CATEGORIES,0:12))
         ALLOCATE(MON_MDS_CL_MULT_CAPACITY
     +                         (0:NUMBER_OF_CAP_LIMITED_CLASSES,
     +                                        NUM_FUEL_CATEGORIES,0:12))
         ALLOCATE(MON_MDS_CL_MULT_ENERGY
     +                         (0:NUMBER_OF_CAP_LIMITED_CLASSES,
     +                                        NUM_FUEL_CATEGORIES,0:12))
         ALLOCATE(MON_MDS_CL_MULT_PURCHASES
     +                         (0:NUMBER_OF_CAP_LIMITED_CLASSES,
     +                                        NUM_FUEL_CATEGORIES,0:12))
         ALLOCATE(MON_MDS_CL_CLASS_VAR_COST
     +                         (0:NUMBER_OF_CAP_LIMITED_CLASSES,4,0:12))
         ALLOCATE(MON_MDS_CL_CLASS_FIXED_COST
     +                         (0:NUMBER_OF_CAP_LIMITED_CLASSES,4,0:12))
         ALLOCATE(MON_MDS_CL_CLASS_REVENUE
     +                         (0:NUMBER_OF_CAP_LIMITED_CLASSES,5,0:12))
         ALLOCATE(MON_MDS_CL_CAP_REVENUE
     +                         (0:NUMBER_OF_CAP_LIMITED_CLASSES,5,0:12))
         ALLOCATE(MON_MDS_CL_CLASS_CAPACITY
     +                         (0:NUMBER_OF_CAP_LIMITED_CLASSES,4,0:12))
         ALLOCATE(MON_MDS_CL_CLASS_PURCHASES
     +                         (0:NUMBER_OF_CAP_LIMITED_CLASSES,5,0:12))
         ALLOCATE(MON_MDS_CL_CAP_PURCHASES
     +                         (0:NUMBER_OF_CAP_LIMITED_CLASSES,5,0:12))
         ALLOCATE(MON_MDS_CL_CLASS_ENERGY
     +                         (0:NUMBER_OF_CAP_LIMITED_CLASSES,4,0:12))
         ALLOCATE(MON_MDS_CL_CLASS_EMISSIONS(NUMBER_OF_EMISSION_TYPES,
     +                            0:NUMBER_OF_CAP_LIMITED_CLASSES,0:12))
         ALLOCATE(MON_MDS_CL_CLASS_EMISSIONS_COST(
     +         NUMBER_OF_EMISSION_TYPES,0:NUMBER_OF_CAP_LIMITED_CLASSES,
     +                                                            0:12))
!
         ALLOCATE(MON_MDS_INTRA_COMPANY_REVENUES(0:1024,0:3,0:12))
         ALLOCATE(MON_MDS_INTRA_COMPANY_NF_BURN(0:1024,0:12))
         ALLOCATE(MON_MDS_NF_FUEL_LEASED_BC
     +                         (0:NUMBER_OF_CAP_LIMITED_CLASSES,4,0:12))
         ALLOCATE(MON_MDS_NF_FUEL_OWNED_BC
     +                         (0:NUMBER_OF_CAP_LIMITED_CLASSES,4,0:12))
         ALLOCATE(MON_MDS_NUC_FUEL_LEASE_BURN_BC
     +                           (0:NUMBER_OF_CAP_LIMITED_CLASSES,0:12))
         ALLOCATE(MON_MDS_NUC_FUEL_OWNED_BURN_BC
     +                           (0:NUMBER_OF_CAP_LIMITED_CLASSES,0:12))
         ALLOCATE(MON_MDS_NUCLEAR_MWH_BY_CLASS
     +                           (0:NUMBER_OF_CAP_LIMITED_CLASSES,0:12))
         ALLOCATE(MON_MDS_NUCLEAR_MMBTU_BY_CLASS
     +                           (0:NUMBER_OF_CAP_LIMITED_CLASSES,0:12))
         ALLOCATE(MON_MDS_ICAP_REV_BY_CLASS
     +                           (0:NUMBER_OF_CAP_LIMITED_CLASSES,0:12))
         ALLOCATE(MONTHLY_AC_WHOLESALE_PROD_COST
     +                           (0:NUMBER_OF_CAP_LIMITED_CLASSES,0:12))
         ALLOCATE(MONTHLY_AC_ECITY_VAR_PROD_COST
     +                           (0:NUMBER_OF_CAP_LIMITED_CLASSES,0:12))
         ALLOCATE(MONTHLY_AC_ECITY_NEW_FIX_COST
     +                           (0:NUMBER_OF_CAP_LIMITED_CLASSES,0:12))
!
         SET_UP_CL_CLASS_ARRAYS = MAX_CAP_LIMITED_CLASS_ID_NUM

         IF(UPPER_TRANS_GROUP > 0) THEN
            IF(ALLOCATED(PURCHASE_ASSET_CLASS_ID))
     +                               DEALLOCATE(PURCHASE_ASSET_CLASS_ID,
     +                                          PURCHASE_POWER_ASSIGN)
            ALLOCATE(PURCHASE_ASSET_CLASS_ID(0:UPPER_TRANS_GROUP))
            ALLOCATE(PURCHASE_POWER_ASSIGN(0:UPPER_TRANS_GROUP))
            DO I = 1, UPPER_TRANS_GROUP
!
! NEED TO CHECK WHETHER CLASS EXISTS
!
               PURCHASE_ASSET_CLASS_ID(I) =
     +                                    GET_PURCHASE_ASSET_CLASS_ID(I)
               VOID_LOGICAL = GET_PURCHASE_POWER_ASSIGN(I,
     +                                         PURCHASE_POWER_ASSIGN(I))
            ENDDO
         ENDIF
!
      RETURN
!***********************************************************************
      ENTRY ZERO_CL_CLASS_ARRAYS
!***********************************************************************
!
! MONTHLY INITIALIZATION. NECESSARY TO ACCUMULATE ANNUAL VALUES.
!
!
         MON_MDS_NF_FUEL_LEASED_BC = 0.
         MON_MDS_NF_FUEL_OWNED_BC = 0.
         MON_MDS_INTRA_COMPANY_REVENUES = 0.
         MON_MDS_INTRA_COMPANY_NF_BURN = 0.
         MON_MDS_NUC_FUEL_LEASE_BURN_BC = 0.
         MON_MDS_NUC_FUEL_OWNED_BURN_BC = 0.
         MON_MDS_NUCLEAR_MWH_BY_CLASS = 0.
         MON_MDS_NUCLEAR_MMBTU_BY_CLASS = 0.
         MON_MDS_ICAP_REV_BY_CLASS = 0.
         MONTHLY_AC_WHOLESALE_PROD_COST = 0.
         MONTHLY_AC_ECITY_VAR_PROD_COST = 0.
         MONTHLY_AC_ECITY_NEW_FIX_COST = 0.
         MON_MDS_CL_CLASS_VAR_COST = 0.
         MON_MDS_CL_CLASS_FUEL_COST = 0.
         MON_MDS_CL_CLASS_FIXED_COST = 0.
         MON_MDS_CL_CLASS_CAPACITY = 0.
         MON_MDS_CL_CLASS_ENERGY = 0.
         MON_MDS_CL_CLASS_REVENUE = 0.
         MON_MDS_CL_CAP_REVENUE = 0.
         MON_MDS_CL_CLASS_PURCHASES = 0.
         MON_MDS_CL_CAP_PURCHASES = 0.
         MON_MDS_CL_CLASS_EMISSIONS = 0.
         MON_MDS_CL_CLASS_EMISSIONS_COST = 0.

         MON_MDS_CL_MULT_FUEL_COST = 0.
         MON_MDS_CL_MULT_VAR_COST = 0.
         MON_MDS_CL_MULT_FIXED_COST = 0.
         MON_MDS_CL_MULT_CAPACITY = 0.
         MON_MDS_CL_MULT_ENERGY = 0.
         MON_MDS_CL_MULT_PURCHASES = 0.
!
         FIRST_YEAR_PRICE_MODE = GET_FIRST_YEAR_PRICE_MODE()

!
         ZERO_CL_CLASS_ARRAYS = NUMBER_OF_CAP_LIMITED_CLASSES
!
      RETURN
!***********************************************************************
      ENTRY MON_MDS_CL_EXP_2_AC(R_ISEAS)
!***********************************************************************

         ALLOCATE(ASSET_CLASS_LIST(AVAIL_DATA_YEARS))
         ALLOCATE(ASSET_ALLOCATION_LIST(AVAIL_DATA_YEARS))
         CURRENT_YEAR = YEAR+BASE_YEAR
         CURRENT_YEAR_COMPARISON = (CURRENT_YEAR-1900)*100

         ECITIES = ECITY_COMPANY() ! YES_ECITIES_UNITS_ACTIVE()
!
         MAX_INTRA_CLASS = 0
         INTRA_FOSSIL_FUEL_EXPENSES = 0.
         INTRA_LEASED_NUC_FUEL_EXPENSES = 0.
         INTRA_OWNED_NUC_FUEL_EXPENSES = 0.
         INTRA_NUC_OWN_BURN = 0.
         INTRA_NUC_LEASE_BURN = 0.
         INTRA_PURCHASE_EXPENSES = 0.
         BASE_MARKET_ENRG_SALES = 0.
         BASE_MARKET_REVENUES = 0.
         PEAK_MARKET_ENRG_SALES = 0.
         PEAK_MARKET_REVENUES = 0.
         MON_MDS_CL_EXP_2_AC = UNUM
!
         DO UNUM = 1, NUNITS
!
            IF(ONLINE(UNUM) - CURRENT_YEAR_COMPARISON > 12  .OR.
     +                CURRENT_YEAR_COMPARISON - OFLINE(UNUM) > 12) CYCLE

            CLASS_POINTER = 1

            IF(MON_MDS_CL_UNIT_ENERGY(UNUM,R_ISEAS) > .45) THEN
               PERCENT_RETAIL = 1. -
     +               MON_MDS_ECO_SALES_ENRG_FROM(UNUM,R_ISEAS)/
     +                              MON_MDS_CL_UNIT_ENERGY(UNUM,R_ISEAS)
            ELSE
               PERCENT_RETAIL = 1. ! 100 %
            ENDIF

            IF(PRICE_ONLY_WHOLESALE_REV) THEN
               OLD_ADJ_LOGIC = .FALSE. ! THIS WILL INVOKE THE IPL FAC
            ELSE
               OLD_ADJ_LOGIC = .TRUE.
            ENDIF
!
            IF(INTRA_COMPANY_CLASS_ID(UNUM) >= 0 .AND.
     +                      INTRA_COMPANY_TRANSACTION(UNUM) == 'Y') THEN
               ASSET_CLASS = INTRA_COMPANY_CLASS_ID(UNUM)
               CALL CHECK_IF_CLASS_DEFINED(ASSET_CLASS)
               ASSET_CLASS = ASSET_CLASS + 1
               MAX_INTRA_CLASS = MAX(MAX_INTRA_CLASS,ASSET_CLASS)
!
               SELECT CASE (EXPENSE_ASSIGNMENT(UNUM))
               CASE('F')  ! FUEL REVENUES
                  COLLECTION = MAX(1,
     +                           INDEX('BA',(EXPENSE_COLLECTION(UNUM))))
                  IF(COLLECTION /=1 .OR. PERCENT_RETAIL > .9999 .OR.
     +                                               OLD_ADJ_LOGIC) THEN
                     MON_MDS_INTRA_COMPANY_REVENUES(
     +                                 ASSET_CLASS,COLLECTION,R_ISEAS) =
     +                  MON_MDS_INTRA_COMPANY_REVENUES(
     +                                 ASSET_CLASS,COLLECTION,R_ISEAS) +
     +                           MON_MDS_CL_UNIT_FUEL_COST(UNUM,R_ISEAS)
     +                                      *CL_POOL_FRAC_OWN(UNUM)/100.
                  ELSE
                     MON_MDS_INTRA_COMPANY_REVENUES(
     +                                 ASSET_CLASS,COLLECTION,R_ISEAS) =
     +                  MON_MDS_INTRA_COMPANY_REVENUES(
     +                                 ASSET_CLASS,COLLECTION,R_ISEAS) +
     +                           MON_MDS_CL_UNIT_FUEL_COST(UNUM,R_ISEAS)
     +                       *PERCENT_RETAIL*CL_POOL_FRAC_OWN(UNUM)/100.
                     MON_MDS_INTRA_COMPANY_REVENUES(
     +                                 ASSET_CLASS,2,R_ISEAS) =
     +                  MON_MDS_INTRA_COMPANY_REVENUES(
     +                                 ASSET_CLASS,2,R_ISEAS) +
     +                           MON_MDS_CL_UNIT_FUEL_COST(UNUM,R_ISEAS)
     +                  *(1.-PERCENT_RETAIL)*CL_POOL_FRAC_OWN(UNUM)/100.
                  ENDIF
               CASE('L','O')  ! FUEL REVENUES
                  COLLECTION = MAX(1,
     +                           INDEX('BA',(EXPENSE_COLLECTION(UNUM))))
                  IF(COLLECTION /=1 .OR. PERCENT_RETAIL > .9999 .OR.
     +                                               OLD_ADJ_LOGIC) THEN
                     MON_MDS_INTRA_COMPANY_REVENUES(
     +                                 ASSET_CLASS,COLLECTION,R_ISEAS) =
     +                  MON_MDS_INTRA_COMPANY_REVENUES(
     +                                 ASSET_CLASS,COLLECTION,R_ISEAS) +
     +                           MON_MDS_CL_UNIT_FUEL_COST(UNUM,R_ISEAS)
     +                                      *CL_POOL_FRAC_OWN(UNUM)/100.
                  ELSE
                     MON_MDS_INTRA_COMPANY_REVENUES(
     +                                 ASSET_CLASS,COLLECTION,R_ISEAS) =
     +                  MON_MDS_INTRA_COMPANY_REVENUES(
     +                                 ASSET_CLASS,COLLECTION,R_ISEAS) +
     +                           MON_MDS_CL_UNIT_FUEL_COST(UNUM,R_ISEAS)
     +                       *PERCENT_RETAIL*CL_POOL_FRAC_OWN(UNUM)/100.
                     MON_MDS_INTRA_COMPANY_REVENUES(
     +                                 ASSET_CLASS,2,R_ISEAS) =
     +                  MON_MDS_INTRA_COMPANY_REVENUES(
     +                                 ASSET_CLASS,2,R_ISEAS) +
     +                           MON_MDS_CL_UNIT_FUEL_COST(UNUM,R_ISEAS)
     +                  *(1.-PERCENT_RETAIL)*CL_POOL_FRAC_OWN(UNUM)/100.
                  ENDIF
                  MON_MDS_INTRA_COMPANY_NF_BURN(ASSET_CLASS,R_ISEAS) =
     +              MON_MDS_INTRA_COMPANY_NF_BURN(ASSET_CLASS,R_ISEAS) +
     +                      (MON_MDS_CL_UNIT_FUEL_COST(UNUM,R_ISEAS) -
     +                        MON_MDS_NUC_FUEL_ADDER_COST(UNUM,R_ISEAS))
     +                                      *CL_POOL_FRAC_OWN(UNUM)/100.
               CASE('P')  ! PURCHASE REVENUES
                  MON_MDS_INTRA_COMPANY_REVENUES(
     +                                          ASSET_CLASS,3,R_ISEAS) =
     +                   MON_MDS_INTRA_COMPANY_REVENUES(
     +                                          ASSET_CLASS,3,R_ISEAS) +
     +                        (MON_MDS_CL_UNIT_FUEL_COST(UNUM,R_ISEAS) +
     +                          MON_MDS_CL_UNIT_VAR_COST(UNUM,R_ISEAS) +
     +                         MON_MDS_CL_UNIT_FIXED_COST(UNUM,R_ISEAS))
     +                                      *CL_POOL_FRAC_OWN(UNUM)/100.
               CASE DEFAULT
               END SELECT
!
               SELECT CASE (EXPENSE_ASSIGNMENT(UNUM))
               CASE('F')  ! FUEL EXPENSES
                  INTRA_FOSSIL_FUEL_EXPENSES =
     +                   INTRA_FOSSIL_FUEL_EXPENSES +
     +                           MON_MDS_CL_UNIT_FUEL_COST(UNUM,R_ISEAS)
     +                                      *CL_POOL_FRAC_OWN(UNUM)/100.
               CASE('L')  ! NUCLEAR FUEL EXPENSES
                  INTRA_LEASED_NUC_FUEL_EXPENSES =
     +                      INTRA_LEASED_NUC_FUEL_EXPENSES +
     +                           MON_MDS_CL_UNIT_FUEL_COST(UNUM,R_ISEAS)
     +                                      *CL_POOL_FRAC_OWN(UNUM)/100.
                  INTRA_NUC_LEASE_BURN = INTRA_NUC_LEASE_BURN +
     +                        (MON_MDS_CL_UNIT_FUEL_COST(UNUM,R_ISEAS) -
     +                        MON_MDS_NUC_FUEL_ADDER_COST(UNUM,R_ISEAS))
     +                                      *CL_POOL_FRAC_OWN(UNUM)/100.
               CASE('O')  ! NUCLEAR FUEL EXPENSES
                  INTRA_OWNED_NUC_FUEL_EXPENSES =
     +                    INTRA_OWNED_NUC_FUEL_EXPENSES +
     +                           MON_MDS_CL_UNIT_FUEL_COST(UNUM,R_ISEAS)
     +                                      *CL_POOL_FRAC_OWN(UNUM)/100.
                  INTRA_NUC_OWN_BURN = INTRA_NUC_OWN_BURN +
     +                      (MON_MDS_CL_UNIT_FUEL_COST(UNUM,R_ISEAS) -
     +                        MON_MDS_NUC_FUEL_ADDER_COST(UNUM,R_ISEAS))
     +                                      *CL_POOL_FRAC_OWN(UNUM)/100.
               CASE('P')  ! PURCHASE EXPENSES
                  INTRA_PURCHASE_EXPENSES = INTRA_PURCHASE_EXPENSES +
     +                        (MON_MDS_CL_UNIT_FUEL_COST(UNUM,R_ISEAS) +
     +                          MON_MDS_CL_UNIT_VAR_COST(UNUM,R_ISEAS) +
     +                         MON_MDS_CL_UNIT_FIXED_COST(UNUM,R_ISEAS))
     +                                      *CL_POOL_FRAC_OWN(UNUM)/100.
               CASE DEFAULT
               END SELECT
            ENDIF
!
            COLLECTION = INDEX('ABNX',(EXPENSE_COLLECTION(UNUM)))
            IF(COLLECTION == 0) COLLECTION = 3
            ASSET_CLASS = ASSET_CLASS_NUM(UNUM)
            ASSET_ALLOCATION_VECTOR = ASSET_CLASS_VECTOR(UNUM)
!
            VOID_LOGICAL=RETURN_ASSET_CLASS_LISTS(ASSET_CLASS,
     +                                          ASSET_CLASS_LIST,
     +                                          ASSET_ALLOCATION_VECTOR,
     +                                          ASSET_ALLOCATION_LIST)
!
            FT = PRIMARY_MOVER(UNUM)

            if(FT==0)  FT = 11
            TT = 10
!
!
            DO
               ASSET_CLASS = ASSET_CLASS_LIST(CLASS_POINTER)
               CALL CHECK_IF_CLASS_DEFINED(ASSET_CLASS)
!
! IDENTIFYING BASE AND PEAK EL REVENUE CLASSES
!
               BASE_EL_REVENUE_CASE = (ASSET_CLASS == 10)
               PEAK_EL_REVENUE_CASE = (ASSET_CLASS == 19)
!
               ASSET_CLASS = ASSET_CLASS + 1
               IF(ASSET_CLASS <= 0 .OR.
     +                 ASSET_CLASS > MAX_CAP_LIMITED_CLASS_ID_NUM) CYCLE
               ASSET_CLASS = ASSET_CLASS_POINTER(ASSET_CLASS)
               IF(ASSET_ALLOCATION_LIST(CLASS_POINTER) < 0.) THEN
                  ALLOCATION_VECTOR =
     +                         ABS(ASSET_ALLOCATION_LIST(CLASS_POINTER))
                  CALL GET_ASSET_VAR(ALLOCATION_VECTOR,
     +                                      DUMMY_TYPE,ALLOCATION_VALUE)
                  ASSET_ALLOCATOR = CL_POOL_FRAC_OWN(UNUM)/100. *
     +                 ALLOCATION_VALUE(MIN(YEAR,AVAIL_DATA_YEARS))/100.
               ELSE
                  ASSET_ALLOCATOR =
     +                       ASSET_ALLOCATION_LIST(CLASS_POINTER)/100. *
     +                       CL_POOL_FRAC_OWN(UNUM)/100.
               ENDIF
!
               DO I = 1, NUMBER_OF_EMISSION_TYPES
                  MON_MDS_CL_CLASS_EMISSIONS(I,ASSET_CLASS,R_ISEAS) =
     +                 MON_MDS_CL_CLASS_EMISSIONS(I,ASSET_CLASS,R_ISEAS)
     +                 + ASSET_ALLOCATOR *
     +                         MON_MDS_CL_UNIT_EMISSIONS(I,UNUM,R_ISEAS)
               ENDDO
               MON_MDS_CL_CLASS_EMISSIONS_COST(:,ASSET_CLASS,R_ISEAS) =
     +            MON_MDS_CL_CLASS_EMISSIONS_COST(:,ASSET_CLASS,R_ISEAS)
     +            + ASSET_ALLOCATOR *
     +                    MON_MDS_CL_UNIT_EMISSIONS_COST(:,UNUM,R_ISEAS)

               IF(EXPENSE_ASSIGNMENT(UNUM) == 'L') THEN
                  IF(COLLECTION /=1 .OR. PERCENT_RETAIL > .9999 .OR.
     +                                               OLD_ADJ_LOGIC) THEN
                     MON_MDS_NF_FUEL_LEASED_BC(
     +                                 ASSET_CLASS,COLLECTION,R_ISEAS) =
     +                 MON_MDS_NF_FUEL_LEASED_BC(
     +                                 ASSET_CLASS,COLLECTION,R_ISEAS) +
     +                  ASSET_ALLOCATOR *
     +                           MON_MDS_CL_UNIT_FUEL_COST(UNUM,R_ISEAS)
                  ELSE
                     MON_MDS_NF_FUEL_LEASED_BC(
     +                                 ASSET_CLASS,COLLECTION,R_ISEAS) =
     +                 MON_MDS_NF_FUEL_LEASED_BC(
     +                                 ASSET_CLASS,COLLECTION,R_ISEAS) +
     +                  ASSET_ALLOCATOR * PERCENT_RETAIL *
     +                           MON_MDS_CL_UNIT_FUEL_COST(UNUM,R_ISEAS)
                     MON_MDS_NF_FUEL_LEASED_BC(
     +                                 ASSET_CLASS,2,R_ISEAS) =
     +                 MON_MDS_NF_FUEL_LEASED_BC(
     +                                 ASSET_CLASS,2,R_ISEAS) +
     +                  ASSET_ALLOCATOR * (1.-PERCENT_RETAIL) *
     +                           MON_MDS_CL_UNIT_FUEL_COST(UNUM,R_ISEAS)
                  ENDIF
                  MON_MDS_NUC_FUEL_LEASE_BURN_BC(ASSET_CLASS,R_ISEAS) =
     +                MON_MDS_NUC_FUEL_LEASE_BURN_BC(
     +                                            ASSET_CLASS,R_ISEAS) +
     +                ASSET_ALLOCATOR *
     +                      (MON_MDS_CL_UNIT_FUEL_COST(UNUM,R_ISEAS) -
     +                        MON_MDS_NUC_FUEL_ADDER_COST(UNUM,R_ISEAS))
                  MON_MDS_NUCLEAR_MWH_BY_CLASS(ASSET_CLASS,R_ISEAS) =
     +                     MON_MDS_NUCLEAR_MWH_BY_CLASS(
     +                                            ASSET_CLASS,R_ISEAS) +
     +                     ASSET_ALLOCATOR *
     +                              MON_MDS_CL_UNIT_ENERGY(UNUM,R_ISEAS)
                  MON_MDS_NUCLEAR_MMBTU_BY_CLASS(ASSET_CLASS,R_ISEAS) =
     +                  MON_MDS_NUCLEAR_MMBTU_BY_CLASS(
     +                                            ASSET_CLASS,R_ISEAS) +
     +                     ASSET_ALLOCATOR *
     +                     MON_MDS_CL_UNIT_MMBTUS(UNUM,R_ISEAS)
               ELSEIF(EXPENSE_ASSIGNMENT(UNUM) == 'O') THEN
                  IF(COLLECTION /=1 .OR. PERCENT_RETAIL > .9999 .OR.
     +                                               OLD_ADJ_LOGIC) THEN
                     MON_MDS_NF_FUEL_OWNED_BC(
     +                                 ASSET_CLASS,COLLECTION,R_ISEAS) =
     +                  MON_MDS_NF_FUEL_OWNED_BC(
     +                                 ASSET_CLASS,COLLECTION,R_ISEAS) +
     +                  ASSET_ALLOCATOR *
     +                           MON_MDS_CL_UNIT_FUEL_COST(UNUM,R_ISEAS)
                  ELSE
                     MON_MDS_NF_FUEL_OWNED_BC(
     +                                 ASSET_CLASS,COLLECTION,R_ISEAS) =
     +                  MON_MDS_NF_FUEL_OWNED_BC(
     +                                 ASSET_CLASS,COLLECTION,R_ISEAS) +
     +                  ASSET_ALLOCATOR * PERCENT_RETAIL *
     +                           MON_MDS_CL_UNIT_FUEL_COST(UNUM,R_ISEAS)
                     MON_MDS_NF_FUEL_OWNED_BC(
     +                                 ASSET_CLASS,2,R_ISEAS) =
     +                  MON_MDS_NF_FUEL_OWNED_BC(
     +                                 ASSET_CLASS,2,R_ISEAS) +
     +                  ASSET_ALLOCATOR * (1.-PERCENT_RETAIL) *
     +                           MON_MDS_CL_UNIT_FUEL_COST(UNUM,R_ISEAS)
                  ENDIF
                  MON_MDS_NUC_FUEL_OWNED_BURN_BC(ASSET_CLASS,R_ISEAS) =
     +                MON_MDS_NUC_FUEL_OWNED_BURN_BC(
     +                                            ASSET_CLASS,R_ISEAS) +
     +                ASSET_ALLOCATOR *
     +                        (MON_MDS_CL_UNIT_FUEL_COST(UNUM,R_ISEAS) -
     +                        MON_MDS_NUC_FUEL_ADDER_COST(UNUM,R_ISEAS))
                  MON_MDS_NUCLEAR_MWH_BY_CLASS(ASSET_CLASS,R_ISEAS) =
     +               MON_MDS_NUCLEAR_MWH_BY_CLASS(ASSET_CLASS,R_ISEAS) +
     +                     ASSET_ALLOCATOR *
     +                     MON_MDS_CL_UNIT_ENERGY(UNUM,R_ISEAS)
                  MON_MDS_NUCLEAR_MMBTU_BY_CLASS(ASSET_CLASS,R_ISEAS) =
     +                  MON_MDS_NUCLEAR_MMBTU_BY_CLASS(
     +                                            ASSET_CLASS,R_ISEAS) +
     +                     ASSET_ALLOCATOR *
     +                     MON_MDS_CL_UNIT_MMBTUS(UNUM,R_ISEAS)
               ELSEIF(EXPENSE_ASSIGNMENT(UNUM) == 'P') THEN
                  IF(COLLECTION /=1 .OR. PERCENT_RETAIL > .9999 .OR.
     +                                               OLD_ADJ_LOGIC) THEN
                     MON_MDS_CL_CLASS_PURCHASES(
     +                                 ASSET_CLASS,COLLECTION,R_ISEAS) =
     +                  MON_MDS_CL_CLASS_PURCHASES(
     +                                 ASSET_CLASS,COLLECTION,R_ISEAS) +
     +                        ASSET_ALLOCATOR *
     +                        (MON_MDS_CL_UNIT_FUEL_COST(UNUM,R_ISEAS) +
     +                          MON_MDS_CL_UNIT_VAR_COST(UNUM,R_ISEAS) +
     +                         MON_MDS_CL_UNIT_FIXED_COST(UNUM,R_ISEAS))
                  ELSE
                     MON_MDS_CL_CLASS_PURCHASES(
     +                                 ASSET_CLASS,COLLECTION,R_ISEAS) =
     +                  MON_MDS_CL_CLASS_PURCHASES(
     +                                 ASSET_CLASS,COLLECTION,R_ISEAS) +
     +                        ASSET_ALLOCATOR * PERCENT_RETAIL *
     +                        (MON_MDS_CL_UNIT_FUEL_COST(UNUM,R_ISEAS) +
     +                          MON_MDS_CL_UNIT_VAR_COST(UNUM,R_ISEAS) +
     +                         MON_MDS_CL_UNIT_FIXED_COST(UNUM,R_ISEAS))
                     MON_MDS_CL_CLASS_PURCHASES(
     +                                 ASSET_CLASS,2,R_ISEAS) =
     +                  MON_MDS_CL_CLASS_PURCHASES(
     +                                 ASSET_CLASS,2,R_ISEAS) +
     +                        ASSET_ALLOCATOR * (1.-PERCENT_RETAIL) *
     +                        (MON_MDS_CL_UNIT_FUEL_COST(UNUM,R_ISEAS) +
     +                          MON_MDS_CL_UNIT_VAR_COST(UNUM,R_ISEAS) +
     +                         MON_MDS_CL_UNIT_FIXED_COST(UNUM,R_ISEAS))
                  ENDIF
                  MON_MDS_CL_CLASS_CAPACITY(ASSET_CLASS,2,R_ISEAS) =
     +                MON_MDS_CL_CLASS_CAPACITY(ASSET_CLASS,2,R_ISEAS) +
     +                   ASSET_ALLOCATOR *
     +                     MON_MDS_CL_UNIT_CAPACITY(UNUM,R_ISEAS)
                  MON_MDS_CL_CLASS_ENERGY(ASSET_CLASS,2,R_ISEAS) =
     +                  MON_MDS_CL_CLASS_ENERGY(ASSET_CLASS,2,R_ISEAS) +
     +                     ASSET_ALLOCATOR *
     +                     MON_MDS_CL_UNIT_ENERGY(UNUM,R_ISEAS)

                  MON_MDS_CL_MULT_PURCHASES(ASSET_CLASS,FT,R_ISEAS) =
     +               MON_MDS_CL_MULT_PURCHASES(ASSET_CLASS,FT,R_ISEAS) +
     +                        ASSET_ALLOCATOR *
     +                        (MON_MDS_CL_UNIT_FUEL_COST(UNUM,R_ISEAS) +
     +                         MON_MDS_CL_UNIT_VAR_COST(UNUM,R_ISEAS) +
     +                         MON_MDS_CL_UNIT_FIXED_COST(UNUM,R_ISEAS))
!
               ELSE
                  IF(COLLECTION /=1 .OR. PERCENT_RETAIL > .9999 .OR.
     +                                               OLD_ADJ_LOGIC) THEN
                     MON_MDS_CL_CLASS_FUEL_COST(
     +                                 ASSET_CLASS,COLLECTION,R_ISEAS) =
     +                  MON_MDS_CL_CLASS_FUEL_COST(
     +                                 ASSET_CLASS,COLLECTION,R_ISEAS) +
     +                  ASSET_ALLOCATOR *
     +                           MON_MDS_CL_UNIT_FUEL_COST(UNUM,R_ISEAS)
                  ELSE
! FOR IPL, KCPL AND OTHERS
                     MON_MDS_CL_CLASS_FUEL_COST(
     +                                 ASSET_CLASS,COLLECTION,R_ISEAS) =
     +                  MON_MDS_CL_CLASS_FUEL_COST(
     +                                 ASSET_CLASS,COLLECTION,R_ISEAS) +
     +                  ASSET_ALLOCATOR * PERCENT_RETAIL *
     +                           MON_MDS_CL_UNIT_FUEL_COST(UNUM,R_ISEAS)
!
! ASSIGN THE BALANCE OF THE ADJ TO UNCOLLECTED PER KATHY ANDERSON.
!
                     MON_MDS_CL_CLASS_FUEL_COST(
     +                                 ASSET_CLASS,2,R_ISEAS) =
     +                  MON_MDS_CL_CLASS_FUEL_COST(
     +                                 ASSET_CLASS,2,R_ISEAS) +
     +                  ASSET_ALLOCATOR * (1.-PERCENT_RETAIL) *
     +                           MON_MDS_CL_UNIT_FUEL_COST(UNUM,R_ISEAS)
                  ENDIF

                  MON_MDS_CL_MULT_FUEL_COST(
     +                                 ASSET_CLASS,FT,R_ISEAS) =
     +                  MON_MDS_CL_MULT_FUEL_COST(
     +                                 ASSET_CLASS,FT,R_ISEAS) +
     +                  ASSET_ALLOCATOR *
     +                           MON_MDS_CL_UNIT_FUEL_COST(UNUM,R_ISEAS)
               ENDIF
               IF(EXPENSE_ASSIGNMENT(UNUM) /= 'P') THEN
                  MON_MDS_CL_CLASS_VAR_COST(
     +                                 ASSET_CLASS,COLLECTION,R_ISEAS) =
     +                MON_MDS_CL_CLASS_VAR_COST(
     +                                 ASSET_CLASS,COLLECTION,R_ISEAS) +
     +                   ASSET_ALLOCATOR *
     +                            MON_MDS_CL_UNIT_VAR_COST(UNUM,R_ISEAS)
                  MON_MDS_CL_CLASS_FIXED_COST(
     +                                  ASSET_CLASS,COLLECTION,R_ISEAS)=
     +                 MON_MDS_CL_CLASS_FIXED_COST(
     +                                 ASSET_CLASS,COLLECTION,R_ISEAS) +
     +                     ASSET_ALLOCATOR *
     +                          MON_MDS_CL_UNIT_FIXED_COST(UNUM,R_ISEAS)
                  MON_MDS_CL_CLASS_CAPACITY(ASSET_CLASS,1,R_ISEAS) =
     +                MON_MDS_CL_CLASS_CAPACITY(ASSET_CLASS,1,R_ISEAS) +
     +                         ASSET_ALLOCATOR *
     +                         MON_MDS_CL_UNIT_CAPACITY(UNUM,R_ISEAS)
                  MON_MDS_CL_CLASS_ENERGY(ASSET_CLASS,1,R_ISEAS) =
     +                  MON_MDS_CL_CLASS_ENERGY(ASSET_CLASS,1,R_ISEAS) +
     +                     ASSET_ALLOCATOR *
     +                     MON_MDS_CL_UNIT_ENERGY(UNUM,R_ISEAS)
                  MON_MDS_ICAP_REV_BY_CLASS(ASSET_CLASS,R_ISEAS) =
     +                MON_MDS_ICAP_REV_BY_CLASS(ASSET_CLASS,R_ISEAS) +
     +                     ASSET_ALLOCATOR *
     +                     MON_MDS_ICAP_REVENUES(UNUM,R_ISEAS)
!
                  MON_MDS_CL_MULT_VAR_COST(ASSET_CLASS,FT,R_ISEAS) =
     +                MON_MDS_CL_MULT_VAR_COST(ASSET_CLASS,FT,R_ISEAS) +
     +                      ASSET_ALLOCATOR *
     +                            MON_MDS_CL_UNIT_VAR_COST(UNUM,R_ISEAS)
                  MON_MDS_CL_MULT_FIXED_COST(
     +                                  ASSET_CLASS,FT,R_ISEAS)=
     +                 MON_MDS_CL_MULT_FIXED_COST(
     +                                 ASSET_CLASS,FT,R_ISEAS) +
     +                     ASSET_ALLOCATOR *
     +                          MON_MDS_CL_UNIT_FIXED_COST(UNUM,R_ISEAS)
                  MON_MDS_CL_MULT_CAPACITY(ASSET_CLASS,FT,R_ISEAS) =
     +                MON_MDS_CL_MULT_CAPACITY(ASSET_CLASS,FT,R_ISEAS) +
     +                         ASSET_ALLOCATOR *
     +                         MON_MDS_CL_UNIT_CAPACITY(UNUM,R_ISEAS)
                  MON_MDS_CL_MULT_ENERGY(ASSET_CLASS,FT,R_ISEAS) =
     +                  MON_MDS_CL_MULT_ENERGY(ASSET_CLASS,FT,R_ISEAS) +
     +                     ASSET_ALLOCATOR *
     +                     MON_MDS_CL_UNIT_ENERGY(UNUM,R_ISEAS)
               ENDIF
               IF(MON_MDS_ECO_SALES_ENRG_FROM(UNUM,R_ISEAS) /= 0.) THEN
                  MON_MDS_CL_CLASS_REVENUE(
     +                                 ASSET_CLASS,COLLECTION,R_ISEAS) =
     +                MON_MDS_CL_CLASS_REVENUE(
     +                                 ASSET_CLASS,COLLECTION,R_ISEAS) +
     +                        ASSET_ALLOCATOR *
     +                         MON_MDS_ECO_SALES_REV_FROM(UNUM,R_ISEAS)
                  MON_MDS_CL_CLASS_ENERGY(ASSET_CLASS,3,R_ISEAS) =
     +                  MON_MDS_CL_CLASS_ENERGY(ASSET_CLASS,3,R_ISEAS) +
     +                       ASSET_ALLOCATOR *
     +                         MON_MDS_ECO_SALES_ENRG_FROM(UNUM,R_ISEAS)
                  IF(MON_MDS_CL_UNIT_ENERGY(UNUM,R_ISEAS) > 0.) THEN
!
                     TT_PERCENT =
     +                     MON_MDS_ECO_SALES_ENRG_FROM(UNUM,R_ISEAS)/
     +                              MON_MDS_CL_UNIT_ENERGY(UNUM,R_ISEAS)
!
                     MON_MDS_CL_MULT_FUEL_COST(ASSET_CLASS,TT,R_ISEAS) =
     +                  MON_MDS_CL_MULT_FUEL_COST(
     +                                         ASSET_CLASS,TT,R_ISEAS) +
     +                  ASSET_ALLOCATOR *
     +                       MON_MDS_CL_UNIT_FUEL_COST(UNUM,R_ISEAS) *
     +                                                        TT_PERCENT
                     MON_MDS_CL_MULT_VAR_COST(ASSET_CLASS,TT,R_ISEAS) =
     +                  MON_MDS_CL_MULT_VAR_COST(
     +                                         ASSET_CLASS,TT,R_ISEAS) +
     +                 ASSET_ALLOCATOR *
     +                          MON_MDS_CL_UNIT_VAR_COST(UNUM,R_ISEAS) *
     +                                                        TT_PERCENT
                     MON_MDS_CL_MULT_CAPACITY(ASSET_CLASS,TT,R_ISEAS) =
     +                MON_MDS_CL_MULT_CAPACITY(ASSET_CLASS,TT,R_ISEAS) +
     +                         ASSET_ALLOCATOR *
     +                         MON_MDS_CL_UNIT_CAPACITY(UNUM,R_ISEAS) *
     +                                                        TT_PERCENT
                     MON_MDS_CL_MULT_ENERGY(ASSET_CLASS,TT,R_ISEAS) =
     +                  MON_MDS_CL_MULT_ENERGY(ASSET_CLASS,TT,R_ISEAS) +
     +                     ASSET_ALLOCATOR *
     +                     MON_MDS_CL_UNIT_ENERGY(UNUM,R_ISEAS) *
     +                                                        TT_PERCENT
                  ENDIF
                  IF(BASE_EL_REVENUE_CASE) THEN
                     BASE_MARKET_ENRG_SALES = BASE_MARKET_ENRG_SALES +
     +                       ASSET_ALLOCATOR *
     +                         MON_MDS_ECO_SALES_ENRG_FROM(UNUM,R_ISEAS)
                     BASE_MARKET_REVENUES = BASE_MARKET_REVENUES +
     +                        ASSET_ALLOCATOR *
     +                        MON_MDS_ECO_SALES_REV_FROM(UNUM,R_ISEAS)
                  ELSEIF(PEAK_EL_REVENUE_CASE) THEN
                     PEAK_MARKET_ENRG_SALES = PEAK_MARKET_ENRG_SALES +
     +                       ASSET_ALLOCATOR *
     +                       MON_MDS_ECO_SALES_ENRG_FROM(UNUM,R_ISEAS)
                     PEAK_MARKET_REVENUES = PEAK_MARKET_REVENUES +
     +                        ASSET_ALLOCATOR *
     +                       MON_MDS_ECO_SALES_REV_FROM(UNUM,R_ISEAS)
                  ENDIF
               ENDIF ! MON_MDS_ECO_SALES_ENRG_FROM /= 0.
!
! 091407. FOR CAPACITY_MARKETS
!
               IF(ABS(MON_CAP_SALE_MW_FROM(UNUM)) >  0.1) THEN
                  IF(CAPACITY_MARKET_COST_ASSIGN(UNUM) ==
     +                                 'Capacity Sales      ') THEN
                     LOCAL_COLLECTION =
     +                 INDEX('ABNX',(
     +                          CAPACITY_MARKET_EXP_COLLECT(UNUM)(1:1)))
                     MON_MDS_CL_CAP_REVENUE(
     +                           ASSET_CLASS,LOCAL_COLLECTION,R_ISEAS) =
     +                  MON_MDS_CL_CAP_REVENUE(
     +                           ASSET_CLASS,LOCAL_COLLECTION,R_ISEAS) +
     +                        ASSET_ALLOCATOR *
     +                              MON_CAP_SALE_REV_FROM(UNUM)
                  ENDIF
               ENDIF
               IF(ABS(MON_CAP_PUCH_MW_FROM(UNUM)) >  0.1) THEN
                  LOCAL_COLLECTION =
     +                 INDEX('ABNX',(
     +                          CAPACITY_MARKET_EXP_COLLECT(UNUM)(1:1)))
                  IF(CAPACITY_MARKET_COST_ASSIGN(UNUM) ==
     +                                 'Capacity Purchases  ') THEN
                     MON_MDS_CL_CAP_PURCHASES(
     +                           ASSET_CLASS,LOCAL_COLLECTION,R_ISEAS) =
     +                  MON_MDS_CL_CAP_PURCHASES(
     +                           ASSET_CLASS,LOCAL_COLLECTION,R_ISEAS) -
     +                        ASSET_ALLOCATOR *
     +                             MON_CAP_PUCH_COST_FROM(UNUM)
                  ELSEIF(CAPACITY_MARKET_COST_ASSIGN(UNUM) ==
     +                                 'Purchased Power     ') THEN
                     MON_MDS_CL_CLASS_PURCHASES(
     +                           ASSET_CLASS,LOCAL_COLLECTION,R_ISEAS) =
     +                  MON_MDS_CL_CLASS_PURCHASES(
     +                           ASSET_CLASS,LOCAL_COLLECTION,R_ISEAS) -
     +                        ASSET_ALLOCATOR *
     +                             MON_CAP_PUCH_COST_FROM(UNUM)
! ELSE NOT COLLECTED
                  ENDIF
               ENDIF
!

               IF(MON_ECO_PUCH_ENRG_FROM(UNUM) /= 0.) THEN

                  IF(UPPER_TRANS_GROUP > 0) THEN
                     TG = TRANSACTION_GROUP_ID(UNUM) ! NOTE DOUBLE ASSIGNMENT
                     TG = GET_TRANS_GROUP_POSITION(TG)
                     IF(PURCHASE_POWER_ASSIGN(TG) /= 'U') THEN
                        TG_ASSET_CLASS = PURCHASE_ASSET_CLASS_ID(TG)

                        CALL CHECK_IF_CLASS_DEFINED(TG_ASSET_CLASS)
                        TG_ASSET_CLASS = TG_ASSET_CLASS + 1
                        IF(TG_ASSET_CLASS > 0 .AND. TG_ASSET_CLASS <=
     +                                MAX_CAP_LIMITED_CLASS_ID_NUM) THEN
                           TG_ASSET_CLASS =
     +                               ASSET_CLASS_POINTER(TG_ASSET_CLASS)
                           MON_MDS_CL_CLASS_PURCHASES(TG_ASSET_CLASS,
     +                                             COLLECTION,R_ISEAS) =
     +                        MON_MDS_CL_CLASS_PURCHASES(TG_ASSET_CLASS,
     +                                             COLLECTION,R_ISEAS) +
     +                        MON_MDS_ECO_PUCH_COST_FROM(UNUM,R_ISEAS)

                           MON_ECO_PUCH_COST_FROM(UNUM) = 0.
                        ENDIF
                     ENDIF
                  ENDIF
!
!
                  IF(COLLECTION /=1 .OR. PERCENT_RETAIL > .9999 .OR.
     +                                               OLD_ADJ_LOGIC) THEN
                     MON_MDS_CL_CLASS_PURCHASES(
     +                                 ASSET_CLASS,COLLECTION,R_ISEAS) =
     +                     MON_MDS_CL_CLASS_PURCHASES(
     +                                 ASSET_CLASS,COLLECTION,R_ISEAS) +
     +                    ASSET_ALLOCATOR * MON_ECO_PUCH_COST_FROM(UNUM)
                  ELSE
                     MON_MDS_CL_CLASS_PURCHASES(
     +                                 ASSET_CLASS,2,R_ISEAS) =
     +                     MON_MDS_CL_CLASS_PURCHASES(
     +                                 ASSET_CLASS,2,R_ISEAS) +
     +                    ASSET_ALLOCATOR * MON_ECO_PUCH_COST_FROM(UNUM)
                  ENDIF
                  MON_MDS_CL_CLASS_ENERGY(ASSET_CLASS,4,R_ISEAS) =
     +                  MON_MDS_CL_CLASS_ENERGY(ASSET_CLASS,4,R_ISEAS) +
     +                    ASSET_ALLOCATOR * MON_ECO_PUCH_ENRG_FROM(UNUM)
!
                  IF(MON_ECO_PUCH_ENRG_FROM(UNUM) +
     +                   MON_MDS_CL_UNIT_ENERGY(UNUM,R_ISEAS) > 0.) THEN
!
                        TT_PERCENT =  MON_ECO_PUCH_ENRG_FROM(UNUM)/
     +                                   (MON_ECO_PUCH_ENRG_FROM(UNUM) +
     +                                      ANNUAL_CL_UNIT_ENERGY(UNUM))
!
                        MON_MDS_CL_MULT_CAPACITY(
     +                                        ASSET_CLASS,TT+1,R_ISEAS)=
     +                            MON_MDS_CL_MULT_CAPACITY(
     +                                         ASSET_CLASS,TT+1,R_ISEAS)
     +                              + ASSET_ALLOCATOR *
     +                          MON_MDS_CL_UNIT_CAPACITY(UNUM,R_ISEAS) *
     +                                                        TT_PERCENT
                        MON_MDS_CL_MULT_ENERGY(
     +                                       ASSET_CLASS,TT+1,R_ISEAS) =
     +                          MON_MDS_CL_MULT_ENERGY(
     +                                         ASSET_CLASS,TT+1,R_ISEAS)
     +                                 + ASSET_ALLOCATOR *
     +                                      MON_ECO_PUCH_ENRG_FROM(UNUM)


                        MON_MDS_CL_MULT_PURCHASES(
     +                                       ASSET_CLASS,TT+1,R_ISEAS) =
     +                        MON_MDS_CL_MULT_PURCHASES(
     +                                       ASSET_CLASS,TT+1,R_ISEAS) +
     +                    ASSET_ALLOCATOR * MON_ECO_PUCH_COST_FROM(UNUM)
!
                  ENDIF
!
               ENDIF
!
               IF(ECITIES) THEN
!
                  TOTAL_VARIABLE_COST =
     +               ASSET_ALLOCATOR *
     +                         (MON_MDS_CL_UNIT_VAR_COST(UNUM,R_ISEAS) +
     +                 MON_MDS_CL_UNIT_FUEL_COST(UNUM,R_ISEAS))/1000000.

                  IF(ASSET_ALLOCATOR *
     +                   MON_MDS_CL_UNIT_ENERGY(UNUM,R_ISEAS) > .1) THEN
                     AVERAGE_PRODUCTION_COSTS = TOTAL_VARIABLE_COST/
     +                           (ASSET_ALLOCATOR *
     +                             MON_MDS_CL_UNIT_ENERGY(UNUM,R_ISEAS))
                  ELSE
                     AVERAGE_PRODUCTION_COSTS = 0.
                  ENDIF
                  WHOLESALE_PRODUCTION_COST =
     +                  AVERAGE_PRODUCTION_COSTS *
     +                                     MON_ECO_SALES_ENRG_FROM(UNUM)
                  MONTHLY_AC_WHOLESALE_PROD_COST(
     +                                        ASSET_CLASS,R_ISEAS) =
     +                  MONTHLY_AC_WHOLESALE_PROD_COST(
     +                                        ASSET_CLASS,R_ISEAS) +
     +                                         WHOLESALE_PRODUCTION_COST
                  MONTHLY_AC_WHOLESALE_PROD_COST(ASSET_CLASS,0) =
     +                  MONTHLY_AC_WHOLESALE_PROD_COST(
     +                                           ASSET_CLASS,0) +
     +                                         WHOLESALE_PRODUCTION_COST
                  IF( (LDTYPE(UNUM) /= 'NUC' .AND.
     +                 SPECIAL_UNIT_ID(UNUM) == 'NONE') .OR.
     +                             EXPENSE_ASSIGNMENT(UNUM) == 'P') THEN
                     MONTHLY_AC_ECITY_VAR_PROD_COST(
     +                                        ASSET_CLASS,R_ISEAS) =
     +                  MONTHLY_AC_ECITY_VAR_PROD_COST(
     +                                        ASSET_CLASS,R_ISEAS) +
     +                                               TOTAL_VARIABLE_COST
                     MONTHLY_AC_ECITY_VAR_PROD_COST(
     +                                            ASSET_CLASS,0) =
     +                  MONTHLY_AC_ECITY_VAR_PROD_COST(
     +                                            ASSET_CLASS,0) +
     +                                               TOTAL_VARIABLE_COST
                  ENDIF
                  IF(MON_ECO_PUCH_ENRG_FROM(UNUM) /= 0.) THEN

                     MONTHLY_AC_ECITY_VAR_PROD_COST(
     +                                            ASSET_CLASS,R_ISEAS) =
     +                  MONTHLY_AC_ECITY_VAR_PROD_COST(
     +                                            ASSET_CLASS,R_ISEAS) +
     +                   ASSET_ALLOCATOR * MON_ECO_PUCH_COST_FROM(UNUM)/
     +                                                          1000000.
                     MONTHLY_AC_ECITY_VAR_PROD_COST(
     +                                            ASSET_CLASS,0) =
     +                  MONTHLY_AC_ECITY_VAR_PROD_COST(
     +                                            ASSET_CLASS,0) +
     +                   ASSET_ALLOCATOR * MON_ECO_PUCH_COST_FROM(UNUM)/
     +                                                          1000000.
                  ENDIF
                  IF(ONLINE(UNUM) > FIRST_YEAR_PRICE_MODE) THEN
                     MONTHLY_AC_ECITY_NEW_FIX_COST(
     +                                        ASSET_CLASS,R_ISEAS) =
     +                  MONTHLY_AC_ECITY_NEW_FIX_COST(
     +                                        ASSET_CLASS,R_ISEAS) +
     +                     ASSET_ALLOCATOR *
     +                     MON_MDS_CL_UNIT_FIXED_COST(UNUM,R_ISEAS)/
     +                                                          1000000.
                     MONTHLY_AC_ECITY_NEW_FIX_COST(
     +                                        ASSET_CLASS,0) =
     +                  MONTHLY_AC_ECITY_NEW_FIX_COST(
     +                                        ASSET_CLASS,0) +
     +                   ASSET_ALLOCATOR *
     +                     MON_MDS_CL_UNIT_FIXED_COST(UNUM,R_ISEAS)/
     +                                                          1000000.
                  ENDIF
               ENDIF
!
!
               CLASS_POINTER = CLASS_POINTER + 1
               IF(CLASS_POINTER > AVAIL_DATA_YEARS) EXIT
               IF(ASSET_CLASS_LIST(CLASS_POINTER) == 0. .OR.
     +                     ASSET_CLASS_LIST(CLASS_POINTER) == -99.) EXIT
            ENDDO ! ASSET CLASSES
         ENDDO ! CL UNITS
!
! 09/02/01. AMEREN TRANSFER PRICING CAPABILITY. TWO PARTY FOR NOW.
!           NEED A SWITCH TO DEFINE.
!
         IF(DETAILED_TRANSFER_PRICING) THEN
            CALL GET_MONTHLY_TRANSFER_REV_COST(
     +                                    R_ISEAS,
     +                                    R1_MONTHLY_TRANSFER_REV,
     +                                    R1_MONTHLY_TRANSFER_COST,
     +                                    R2_MONTHLY_TRANSFER_REV,
     +                                    R2_MONTHLY_TRANSFER_COST)
            TG = 1 ! ASSUME UE FOR NOW.
            TG_ASSET_CLASS = PURCHASE_ASSET_CLASS_ID(TG)
            CALL CHECK_IF_CLASS_DEFINED(TG_ASSET_CLASS)
            COLLECTION = 5 ! ASSUME
            TG_ASSET_CLASS = TG_ASSET_CLASS + 1
            IF(TG_ASSET_CLASS > 0 .AND. TG_ASSET_CLASS <=
     +                                MAX_CAP_LIMITED_CLASS_ID_NUM) THEN
               TG_ASSET_CLASS = ASSET_CLASS_POINTER(TG_ASSET_CLASS)
               MON_MDS_CL_CLASS_PURCHASES(
     +                              TG_ASSET_CLASS,COLLECTION,R_ISEAS) =
     +                      MON_MDS_CL_CLASS_PURCHASES(TG_ASSET_CLASS,
     +                                             COLLECTION,R_ISEAS) +
     +                                          R1_MONTHLY_TRANSFER_COST ! FROM TRANSACT MODULE
               MON_MDS_CL_CLASS_REVENUE(
     +                                 ASSET_CLASS,COLLECTION,R_ISEAS) =
     +                  MON_MDS_CL_CLASS_REVENUE(
     +                                 ASSET_CLASS,COLLECTION,R_ISEAS) +
     +                                          R1_MONTHLY_TRANSFER_REV ! FROM TRANSACT MODULE
            ENDIF
            TG = 2 ! ASSUME UE FOR NOW.
            TG_ASSET_CLASS = PURCHASE_ASSET_CLASS_ID(TG)
            CALL CHECK_IF_CLASS_DEFINED(TG_ASSET_CLASS)
            COLLECTION = 5 ! ASSUME
            TG_ASSET_CLASS = TG_ASSET_CLASS + 1
            IF(TG_ASSET_CLASS > 0 .AND. TG_ASSET_CLASS <=
     +                                MAX_CAP_LIMITED_CLASS_ID_NUM) THEN
               TG_ASSET_CLASS = ASSET_CLASS_POINTER(TG_ASSET_CLASS)
               MON_MDS_CL_CLASS_PURCHASES(
     +                              TG_ASSET_CLASS,COLLECTION,R_ISEAS) =
     +                      MON_MDS_CL_CLASS_PURCHASES(TG_ASSET_CLASS,
     +                                             COLLECTION,R_ISEAS) +
     +                                          R2_MONTHLY_TRANSFER_COST ! FROM TRANSACT MODULE
               MON_MDS_CL_CLASS_REVENUE(
     +                                 ASSET_CLASS,COLLECTION,R_ISEAS) =
     +                  MON_MDS_CL_CLASS_REVENUE(
     +                                 ASSET_CLASS,COLLECTION,R_ISEAS) +
     +                                          R2_MONTHLY_TRANSFER_REV ! FROM TRANSACT MODULE
            ENDIF
         ENDIF
!

!
! 08/07/01. CHANGE VARIABLE LIST TO WORK WITH MONTHLY MIDAS.
!        GET COST OF SUPPLYING CAPACITY FOR A "SHORT" SYSTEM.
!
         RUN_TRANSACT = YES_RUN_TRANSACT() ! 10/28/03.
!
         IF(UPPER_TRANS_GROUP > 0 .AND. RUN_TRANSACT) THEN

            DO TG = 1, UPPER_TRANS_GROUP
               TEMP_I2 = 0
               CALL GET_ANNUAL_UNSERVED_COST(
     +                                    TG,
     +                                    TRANS_ANNUAL_UNSERVED_COST,
     +                                    TEMP_R4, ! UNSERVED MWH'S
     +                                    R_ISEAS) ! ANNUAL CALL 6/25/01.

                  IF(PURCHASE_POWER_ASSIGN(TG) == 'A') THEN
                     COLLECTION = 1
                  ELSEIF(PURCHASE_POWER_ASSIGN(TG) == 'B') THEN
                     COLLECTION = 2
                  ELSEIF(PURCHASE_POWER_ASSIGN(TG) == 'C') THEN
                     COLLECTION = 4
                  ELSE
                     COLLECTION = 3
                  ENDIF
                  TG_ASSET_CLASS = PURCHASE_ASSET_CLASS_ID(TG)

                  CALL CHECK_IF_CLASS_DEFINED(TG_ASSET_CLASS)
                  TG_ASSET_CLASS = TG_ASSET_CLASS + 1
                  IF(TG_ASSET_CLASS > 0 .AND. TG_ASSET_CLASS <=
     +                                MAX_CAP_LIMITED_CLASS_ID_NUM) THEN
                     TG_ASSET_CLASS =
     +                               ASSET_CLASS_POINTER(TG_ASSET_CLASS)
                     MON_MDS_CL_CLASS_PURCHASES(
     +                              TG_ASSET_CLASS,COLLECTION,R_ISEAS) =
     +                      MON_MDS_CL_CLASS_PURCHASES(TG_ASSET_CLASS,
     +                                             COLLECTION,R_ISEAS) +
     +                                        TRANS_ANNUAL_UNSERVED_COST ! FROM TRANSACT MODULE
                     IF(WABASH_VALLEY .AND. TG == 1) THEN
                        CALL CL_REPORT_WHOLE_MARKET_COST(
     +                                          R_ISEAS,
     +                                          WHOLESALE_MARKET_COST)
!
                        MON_MDS_CL_CLASS_PURCHASES(
     +                              TG_ASSET_CLASS,COLLECTION,R_ISEAS) =
     +                      MON_MDS_CL_CLASS_PURCHASES(TG_ASSET_CLASS,
     +                                             COLLECTION,R_ISEAS) +
     +                                             WHOLESALE_MARKET_COST ! FROM TRANSACT MODULE
                        COLLECTION = 2
                        MON_MDS_CL_CLASS_PURCHASES(
     +                              TG_ASSET_CLASS,COLLECTION,R_ISEAS) =
     +                      MON_MDS_CL_CLASS_PURCHASES(TG_ASSET_CLASS,
     +                                             COLLECTION,R_ISEAS) -
     +                                             WHOLESALE_MARKET_COST ! FROM TRANSACT MODULE
                     ENDIF
                  ENDIF

            ENDDO
         ENDIF
         DEALLOCATE(ASSET_CLASS_LIST,ASSET_ALLOCATION_LIST)
!
! MARKET REVENUE PROXY FOR UNION EL UNITS 8/7/97
!
         EL_BASE_RATE = 0.
         EL_PEAK_RATE = 0.
         IF(BASE_MARKET_ENRG_SALES > 50 .AND.
     +                                  BASE_MARKET_REVENUES > 100) THEN
            EL_BASE_RATE = BASE_MARKET_REVENUES/BASE_MARKET_ENRG_SALES
         ENDIF
         IF(PEAK_MARKET_ENRG_SALES > 25 .AND.
     +                                   PEAK_MARKET_REVENUES > 50) THEN
            EL_PEAK_RATE = PEAK_MARKET_REVENUES/PEAK_MARKET_ENRG_SALES
         ENDIF
!
         DO COLLECTION = 0, 3
            DO CLASS = 1, MAX_INTRA_CLASS
               MON_MDS_INTRA_COMPANY_REVENUES(0,COLLECTION,R_ISEAS) =
     +                  MON_MDS_INTRA_COMPANY_REVENUES(
     +                                           0,COLLECTION,R_ISEAS) +
     +                        MON_MDS_INTRA_COMPANY_REVENUES(
     +                                         CLASS,COLLECTION,R_ISEAS)
            ENDDO
         ENDDO
         DO CLASS = 1, MAX_INTRA_CLASS
            MON_MDS_INTRA_COMPANY_NF_BURN(0,R_ISEAS) =
     +               MON_MDS_INTRA_COMPANY_NF_BURN(0,R_ISEAS) +
     +                      MON_MDS_INTRA_COMPANY_NF_BURN(CLASS,R_ISEAS)
         ENDDO
!
         DO CLASS = 1, NUMBER_OF_CAP_LIMITED_CLASSES
! UNITS
            MON_MDS_NUC_FUEL_LEASE_BURN_BC(CLASS,R_ISEAS) =
     +            MON_MDS_NUC_FUEL_LEASE_BURN_BC(CLASS,R_ISEAS)/1000000.
            MON_MDS_NUC_FUEL_OWNED_BURN_BC(CLASS,R_ISEAS) =
     +            MON_MDS_NUC_FUEL_OWNED_BURN_BC(CLASS,R_ISEAS)/1000000.
! TOTAL ACROSS CLASSES
            MON_MDS_NUC_FUEL_LEASE_BURN_BC(0,R_ISEAS) =
     +               MON_MDS_NUC_FUEL_LEASE_BURN_BC(0,R_ISEAS) +
     +                     MON_MDS_NUC_FUEL_LEASE_BURN_BC(CLASS,R_ISEAS)
            MON_MDS_NUC_FUEL_OWNED_BURN_BC(0,R_ISEAS) =
     +              MON_MDS_NUC_FUEL_OWNED_BURN_BC(0,R_ISEAS) +
     +                     MON_MDS_NUC_FUEL_OWNED_BURN_BC(CLASS,R_ISEAS)
            MON_MDS_NUCLEAR_MWH_BY_CLASS(0,R_ISEAS) =
     +         MON_MDS_NUCLEAR_MWH_BY_CLASS(0,R_ISEAS) +
     +                       MON_MDS_NUCLEAR_MWH_BY_CLASS(CLASS,R_ISEAS)
            MON_MDS_NUCLEAR_MMBTU_BY_CLASS(0,R_ISEAS) =
     +            MON_MDS_NUCLEAR_MMBTU_BY_CLASS(0,R_ISEAS) +
     +                     MON_MDS_NUCLEAR_MMBTU_BY_CLASS(CLASS,R_ISEAS)
            MON_MDS_ICAP_REV_BY_CLASS(0,R_ISEAS) =
     +               MON_MDS_ICAP_REV_BY_CLASS(0,R_ISEAS)/1000000.
! TOTAL ACROSS MONTHS
            MON_MDS_NUC_FUEL_LEASE_BURN_BC(CLASS,0) =
     +               MON_MDS_NUC_FUEL_LEASE_BURN_BC(CLASS,0) +
     +                     MON_MDS_NUC_FUEL_LEASE_BURN_BC(CLASS,R_ISEAS)
            MON_MDS_NUC_FUEL_OWNED_BURN_BC(CLASS,0) =
     +              MON_MDS_NUC_FUEL_OWNED_BURN_BC(CLASS,0) +
     +                     MON_MDS_NUC_FUEL_OWNED_BURN_BC(CLASS,R_ISEAS)
            MON_MDS_NUCLEAR_MWH_BY_CLASS(CLASS,0) =
     +         MON_MDS_NUCLEAR_MWH_BY_CLASS(CLASS,0) +
     +                       MON_MDS_NUCLEAR_MWH_BY_CLASS(CLASS,R_ISEAS)
            MON_MDS_NUCLEAR_MMBTU_BY_CLASS(CLASS,0) =
     +            MON_MDS_NUCLEAR_MMBTU_BY_CLASS(CLASS,0) +
     +                     MON_MDS_NUCLEAR_MMBTU_BY_CLASS(CLASS,R_ISEAS)
            MON_MDS_ICAP_REV_BY_CLASS(CLASS,R_ISEAS) =
     +                  MON_MDS_ICAP_REV_BY_CLASS(CLASS,R_ISEAS) +
     +                       MON_MDS_ICAP_REV_BY_CLASS(CLASS,R_ISEAS)
! TOTAL ACROSS CLASSES AND MONTHS
            MON_MDS_NUC_FUEL_LEASE_BURN_BC(0,0) =
     +               MON_MDS_NUC_FUEL_LEASE_BURN_BC(0,0) +
     +                     MON_MDS_NUC_FUEL_LEASE_BURN_BC(CLASS,R_ISEAS)
            MON_MDS_NUC_FUEL_OWNED_BURN_BC(0,0) =
     +              MON_MDS_NUC_FUEL_OWNED_BURN_BC(0,0) +
     +                     MON_MDS_NUC_FUEL_OWNED_BURN_BC(CLASS,R_ISEAS)
            MON_MDS_NUCLEAR_MWH_BY_CLASS(0,0) =
     +         MON_MDS_NUCLEAR_MWH_BY_CLASS(0,0) +
     +                       MON_MDS_NUCLEAR_MWH_BY_CLASS(CLASS,R_ISEAS)
            MON_MDS_NUCLEAR_MMBTU_BY_CLASS(0,0) =
     +            MON_MDS_NUCLEAR_MMBTU_BY_CLASS(0,0) +
     +                     MON_MDS_NUCLEAR_MMBTU_BY_CLASS(CLASS,R_ISEAS)
            MON_MDS_ICAP_REV_BY_CLASS(0,0) =
     +                  MON_MDS_ICAP_REV_BY_CLASS(0,0) +
     +                          MON_MDS_ICAP_REV_BY_CLASS(CLASS,R_ISEAS)
!
!
            DO EMISS_TYPE = 1, NUMBER_OF_EMISSION_TYPES
! TOTAL ACROSS CLASSES
               MON_MDS_CL_CLASS_EMISSIONS(EMISS_TYPE,0,R_ISEAS) =
     +             MON_MDS_CL_CLASS_EMISSIONS(EMISS_TYPE,0,R_ISEAS) +
     +              MON_MDS_CL_CLASS_EMISSIONS(EMISS_TYPE,CLASS,R_ISEAS)
! TOTAL ACROSS MONTHS
               MON_MDS_CL_CLASS_EMISSIONS(EMISS_TYPE,CLASS,0) =
     +             MON_MDS_CL_CLASS_EMISSIONS(EMISS_TYPE,CLASS,0) +
     +              MON_MDS_CL_CLASS_EMISSIONS(EMISS_TYPE,CLASS,R_ISEAS)
! TOTAL ACROSS CLASSES AND MONTHS
               MON_MDS_CL_CLASS_EMISSIONS(EMISS_TYPE,0,0) =
     +             MON_MDS_CL_CLASS_EMISSIONS(EMISS_TYPE,0,0) +
     +              MON_MDS_CL_CLASS_EMISSIONS(EMISS_TYPE,CLASS,R_ISEAS)
            ENDDO
!
            DO I = 1, 4
! UNITS
               MON_MDS_CL_CLASS_VAR_COST(CLASS,I,R_ISEAS) =
     +               MON_MDS_CL_CLASS_VAR_COST(CLASS,I,R_ISEAS)/1000000.
               MON_MDS_CL_CLASS_FUEL_COST(CLASS,I,R_ISEAS)  =
     +              MON_MDS_CL_CLASS_FUEL_COST(CLASS,I,R_ISEAS)/1000000.
               MON_MDS_NF_FUEL_LEASED_BC(CLASS,I,R_ISEAS) =
     +               MON_MDS_NF_FUEL_LEASED_BC(CLASS,I,R_ISEAS)/1000000.
               MON_MDS_NF_FUEL_OWNED_BC(CLASS,I,R_ISEAS) =
     +                MON_MDS_NF_FUEL_OWNED_BC(CLASS,I,R_ISEAS)/1000000.
               MON_MDS_CL_CLASS_FIXED_COST(CLASS,I,R_ISEAS) =
     +             MON_MDS_CL_CLASS_FIXED_COST(CLASS,I,R_ISEAS)/1000000.
               MON_MDS_CL_CLASS_REVENUE(CLASS,I,R_ISEAS) =
     +                MON_MDS_CL_CLASS_REVENUE(CLASS,I,R_ISEAS)/1000000.
               MON_MDS_CL_CAP_REVENUE(CLASS,I,R_ISEAS) =
     +                MON_MDS_CL_CAP_REVENUE(CLASS,I,R_ISEAS)/1000000.
               MON_MDS_CL_CLASS_CAPACITY(CLASS,I,R_ISEAS) =
     +               MON_MDS_CL_CLASS_CAPACITY(CLASS,I,R_ISEAS)/1000000.
               MON_MDS_CL_CLASS_ENERGY(CLASS,I,R_ISEAS) =
     +                 MON_MDS_CL_CLASS_ENERGY(CLASS,I,R_ISEAS)/1000000.
               MON_MDS_CL_CLASS_PURCHASES(CLASS,I,R_ISEAS) =
     +              MON_MDS_CL_CLASS_PURCHASES(CLASS,I,R_ISEAS)/1000000.
               MON_MDS_CL_CAP_PURCHASES(CLASS,I,R_ISEAS) =
     +              MON_MDS_CL_CAP_PURCHASES(CLASS,I,R_ISEAS)/1000000.
! TOTAL ACROSS CLASSES
               MON_MDS_CL_CLASS_VAR_COST(0,I,R_ISEAS) =
     +               MON_MDS_CL_CLASS_VAR_COST(0,I,R_ISEAS) +
     +                        MON_MDS_CL_CLASS_VAR_COST(CLASS,I,R_ISEAS)
               MON_MDS_CL_CLASS_FUEL_COST(0,I,R_ISEAS) =
     +                   MON_MDS_CL_CLASS_FUEL_COST(0,I,R_ISEAS) +
     +                       MON_MDS_CL_CLASS_FUEL_COST(CLASS,I,R_ISEAS)
               MON_MDS_NF_FUEL_LEASED_BC(0,I,R_ISEAS) =
     +                   MON_MDS_NF_FUEL_LEASED_BC(0,I,R_ISEAS) +
     +                        MON_MDS_NF_FUEL_LEASED_BC(CLASS,I,R_ISEAS)
               MON_MDS_NF_FUEL_OWNED_BC(0,I,R_ISEAS) =
     +                  MON_MDS_NF_FUEL_OWNED_BC(0,I,R_ISEAS) +
     +                         MON_MDS_NF_FUEL_OWNED_BC(CLASS,I,R_ISEAS)
               MON_MDS_CL_CLASS_FIXED_COST(0,I,R_ISEAS) =
     +               MON_MDS_CL_CLASS_FIXED_COST(0,I,R_ISEAS) +
     +                      MON_MDS_CL_CLASS_FIXED_COST(CLASS,I,R_ISEAS)
               MON_MDS_CL_CLASS_REVENUE(0,I,R_ISEAS) =
     +               MON_MDS_CL_CLASS_REVENUE(0,I,R_ISEAS) +
     +                         MON_MDS_CL_CLASS_REVENUE(CLASS,I,R_ISEAS)
               MON_MDS_CL_CAP_REVENUE(0,I,R_ISEAS) =
     +               MON_MDS_CL_CAP_REVENUE(0,I,R_ISEAS) +
     +                         MON_MDS_CL_CAP_REVENUE(CLASS,I,R_ISEAS)
               MON_MDS_CL_CLASS_CAPACITY(0,I,R_ISEAS) =
     +                  MON_MDS_CL_CLASS_CAPACITY(0,I,R_ISEAS) +
     +                        MON_MDS_CL_CLASS_CAPACITY(CLASS,I,R_ISEAS)
               MON_MDS_CL_CLASS_ENERGY(0,I,R_ISEAS) =
     +                  MON_MDS_CL_CLASS_ENERGY(0,I,R_ISEAS) +
     +                          MON_MDS_CL_CLASS_ENERGY(CLASS,I,R_ISEAS)
               MON_MDS_CL_CLASS_PURCHASES(0,I,R_ISEAS) =
     +                  MON_MDS_CL_CLASS_PURCHASES(0,I,R_ISEAS) +
     +                       MON_MDS_CL_CLASS_PURCHASES(CLASS,I,R_ISEAS)
               MON_MDS_CL_CAP_PURCHASES(0,I,R_ISEAS) =
     +                  MON_MDS_CL_CAP_PURCHASES(0,I,R_ISEAS) +
     +                       MON_MDS_CL_CAP_PURCHASES(CLASS,I,R_ISEAS)
! TOTAL ACROSS MONTHS
               MON_MDS_CL_CLASS_VAR_COST(CLASS,I,0) =
     +               MON_MDS_CL_CLASS_VAR_COST(CLASS,I,0) +
     +                        MON_MDS_CL_CLASS_VAR_COST(CLASS,I,R_ISEAS)
               MON_MDS_CL_CLASS_FUEL_COST(CLASS,I,0) =
     +                   MON_MDS_CL_CLASS_FUEL_COST(CLASS,I,0) +
     +                       MON_MDS_CL_CLASS_FUEL_COST(CLASS,I,R_ISEAS)
               MON_MDS_NF_FUEL_LEASED_BC(CLASS,I,0) =
     +                   MON_MDS_NF_FUEL_LEASED_BC(CLASS,I,0) +
     +                        MON_MDS_NF_FUEL_LEASED_BC(CLASS,I,R_ISEAS)
               MON_MDS_NF_FUEL_OWNED_BC(CLASS,I,0) =
     +                  MON_MDS_NF_FUEL_OWNED_BC(CLASS,I,0) +
     +                         MON_MDS_NF_FUEL_OWNED_BC(CLASS,I,R_ISEAS)
               MON_MDS_CL_CLASS_FIXED_COST(CLASS,I,0) =
     +               MON_MDS_CL_CLASS_FIXED_COST(CLASS,I,0) +
     +                      MON_MDS_CL_CLASS_FIXED_COST(CLASS,I,R_ISEAS)
               MON_MDS_CL_CLASS_REVENUE(CLASS,I,0) =
     +               MON_MDS_CL_CLASS_REVENUE(CLASS,I,0) +
     +                         MON_MDS_CL_CLASS_REVENUE(CLASS,I,R_ISEAS)
               MON_MDS_CL_CAP_REVENUE(CLASS,I,0) =
     +               MON_MDS_CL_CAP_REVENUE(CLASS,I,0) +
     +                         MON_MDS_CL_CAP_REVENUE(CLASS,I,R_ISEAS)
               MON_MDS_CL_CLASS_CAPACITY(CLASS,I,0) =
     +                  MON_MDS_CL_CLASS_CAPACITY(CLASS,I,0) +
     +                        MON_MDS_CL_CLASS_CAPACITY(CLASS,I,R_ISEAS)
               MON_MDS_CL_CLASS_ENERGY(CLASS,I,0) =
     +                  MON_MDS_CL_CLASS_ENERGY(CLASS,I,0) +
     +                          MON_MDS_CL_CLASS_ENERGY(CLASS,I,R_ISEAS)
               MON_MDS_CL_CLASS_PURCHASES(CLASS,I,0) =
     +                  MON_MDS_CL_CLASS_PURCHASES(CLASS,I,0) +
     +                       MON_MDS_CL_CLASS_PURCHASES(CLASS,I,R_ISEAS)
               MON_MDS_CL_CAP_PURCHASES(CLASS,I,0) =
     +                  MON_MDS_CL_CAP_PURCHASES(CLASS,I,0) +
     +                       MON_MDS_CL_CAP_PURCHASES(CLASS,I,R_ISEAS)
! TOTAL ACROSS CLASSES AND MONTHS
               MON_MDS_CL_CLASS_VAR_COST(0,I,0) =
     +               MON_MDS_CL_CLASS_VAR_COST(0,I,0) +
     +                        MON_MDS_CL_CLASS_VAR_COST(CLASS,I,R_ISEAS)
               MON_MDS_CL_CLASS_FUEL_COST(0,I,0) =
     +                   MON_MDS_CL_CLASS_FUEL_COST(0,I,0) +
     +                       MON_MDS_CL_CLASS_FUEL_COST(CLASS,I,R_ISEAS)
               MON_MDS_NF_FUEL_LEASED_BC(0,I,0) =
     +                   MON_MDS_NF_FUEL_LEASED_BC(0,I,0) +
     +                        MON_MDS_NF_FUEL_LEASED_BC(CLASS,I,R_ISEAS)
               MON_MDS_NF_FUEL_OWNED_BC(0,I,0) =
     +                  MON_MDS_NF_FUEL_OWNED_BC(0,I,0) +
     +                         MON_MDS_NF_FUEL_OWNED_BC(CLASS,I,R_ISEAS)
               MON_MDS_CL_CLASS_FIXED_COST(0,I,0) =
     +               MON_MDS_CL_CLASS_FIXED_COST(0,I,0) +
     +                      MON_MDS_CL_CLASS_FIXED_COST(CLASS,I,R_ISEAS)
               MON_MDS_CL_CLASS_REVENUE(0,I,0) =
     +               MON_MDS_CL_CLASS_REVENUE(0,I,0) +
     +                         MON_MDS_CL_CLASS_REVENUE(CLASS,I,R_ISEAS)
               MON_MDS_CL_CAP_REVENUE(0,I,0) =
     +               MON_MDS_CL_CAP_REVENUE(0,I,0) +
     +                         MON_MDS_CL_CAP_REVENUE(CLASS,I,R_ISEAS)
               MON_MDS_CL_CLASS_CAPACITY(0,I,0) =
     +                  MON_MDS_CL_CLASS_CAPACITY(0,I,0) +
     +                        MON_MDS_CL_CLASS_CAPACITY(CLASS,I,R_ISEAS)
               MON_MDS_CL_CLASS_ENERGY(0,I,0) =
     +                  MON_MDS_CL_CLASS_ENERGY(0,I,0) +
     +                          MON_MDS_CL_CLASS_ENERGY(CLASS,I,R_ISEAS)
               MON_MDS_CL_CLASS_PURCHASES(0,I,0) =
     +                  MON_MDS_CL_CLASS_PURCHASES(0,I,0) +
     +                       MON_MDS_CL_CLASS_PURCHASES(CLASS,I,R_ISEAS)
               MON_MDS_CL_CAP_PURCHASES(0,I,0) =
     +                  MON_MDS_CL_CAP_PURCHASES(0,I,0) +
     +                       MON_MDS_CL_CAP_PURCHASES(CLASS,I,R_ISEAS)
            ENDDO
            DO I = 1, NUM_FUEL_CATEGORIES
! UNITS
               MON_MDS_CL_MULT_FUEL_COST(CLASS,I,R_ISEAS)  =
     +               MON_MDS_CL_MULT_FUEL_COST(CLASS,I,R_ISEAS)/1000000.
!
!
!
!
!
!
!
               MON_MDS_CL_MULT_VAR_COST(CLASS,I,R_ISEAS) =
     +                MON_MDS_CL_MULT_VAR_COST(CLASS,I,R_ISEAS)/1000000.
               MON_MDS_CL_MULT_FIXED_COST(CLASS,I,R_ISEAS) =
     +              MON_MDS_CL_MULT_FIXED_COST(CLASS,I,R_ISEAS)/1000000.
               MON_MDS_CL_MULT_CAPACITY(CLASS,I,R_ISEAS) =
     +              MON_MDS_CL_MULT_CAPACITY(CLASS,I,R_ISEAS)/1000000.
               MON_MDS_CL_MULT_ENERGY(CLASS,I,R_ISEAS) =
     +              MON_MDS_CL_MULT_ENERGY(CLASS,I,R_ISEAS)/1000000.
               MON_MDS_CL_MULT_PURCHASES(CLASS,I,R_ISEAS) =
     +              MON_MDS_CL_MULT_PURCHASES(CLASS,I,R_ISEAS)/1000000.
!

! CLASSES
!
               MON_MDS_CL_MULT_FUEL_COST(0,I,R_ISEAS) =
     +               MON_MDS_CL_MULT_FUEL_COST(0,I,R_ISEAS) +
     +                        MON_MDS_CL_MULT_FUEL_COST(CLASS,I,R_ISEAS)
               MON_MDS_CL_MULT_VAR_COST(0,I,R_ISEAS) =
     +               MON_MDS_CL_MULT_VAR_COST(0,I,R_ISEAS) +
     +                        MON_MDS_CL_MULT_VAR_COST(CLASS,I,R_ISEAS)
               MON_MDS_CL_MULT_FIXED_COST(0,I,R_ISEAS) =
     +               MON_MDS_CL_MULT_FIXED_COST(0,I,R_ISEAS) +
     +                       MON_MDS_CL_MULT_FIXED_COST(CLASS,I,R_ISEAS)
               MON_MDS_CL_MULT_CAPACITY(0,I,R_ISEAS) =
     +               MON_MDS_CL_MULT_CAPACITY(0,I,R_ISEAS) +
     +                         MON_MDS_CL_MULT_CAPACITY(CLASS,I,R_ISEAS)
               MON_MDS_CL_MULT_ENERGY(0,I,R_ISEAS) =
     +               MON_MDS_CL_MULT_ENERGY(0,I,R_ISEAS) +
     +                           MON_MDS_CL_MULT_ENERGY(CLASS,I,R_ISEAS)
               MON_MDS_CL_MULT_PURCHASES(0,I,R_ISEAS) =
     +               MON_MDS_CL_MULT_PURCHASES(0,I,R_ISEAS) +
     +                        MON_MDS_CL_MULT_PURCHASES(CLASS,I,R_ISEAS)

               MON_MDS_CL_MULT_FUEL_COST(CLASS,I,0) =
     +               MON_MDS_CL_MULT_FUEL_COST(CLASS,I,0) +
     +                        MON_MDS_CL_MULT_FUEL_COST(CLASS,I,R_ISEAS)
               MON_MDS_CL_MULT_VAR_COST(CLASS,I,0) =
     +               MON_MDS_CL_MULT_VAR_COST(CLASS,I,0) +
     +                         MON_MDS_CL_MULT_VAR_COST(CLASS,I,R_ISEAS)
               MON_MDS_CL_MULT_FIXED_COST(CLASS,I,0) =
     +               MON_MDS_CL_MULT_FIXED_COST(CLASS,I,0) +
     +                       MON_MDS_CL_MULT_FIXED_COST(CLASS,I,R_ISEAS)
               MON_MDS_CL_MULT_CAPACITY(CLASS,I,0) =
     +               MON_MDS_CL_MULT_CAPACITY(CLASS,I,0) +
     +                       MON_MDS_CL_MULT_CAPACITY(CLASS,I,R_ISEAS)
               MON_MDS_CL_MULT_ENERGY(CLASS,I,0) =
     +               MON_MDS_CL_MULT_ENERGY(CLASS,I,0) +
     +                       MON_MDS_CL_MULT_ENERGY(CLASS,I,R_ISEAS)
               MON_MDS_CL_MULT_PURCHASES(CLASS,I,0) =
     +               MON_MDS_CL_MULT_PURCHASES(CLASS,I,0) +
     +                       MON_MDS_CL_MULT_PURCHASES(CLASS,I,R_ISEAS)
!
! CLASSES AND MONTHS
!
               MON_MDS_CL_MULT_FUEL_COST(0,I,0) =
     +               MON_MDS_CL_MULT_FUEL_COST(0,I,0) +
     +                        MON_MDS_CL_MULT_FUEL_COST(CLASS,I,R_ISEAS)
               MON_MDS_CL_MULT_VAR_COST(0,I,0) =
     +               MON_MDS_CL_MULT_VAR_COST(0,I,0) +
     +                         MON_MDS_CL_MULT_VAR_COST(CLASS,I,R_ISEAS)
               MON_MDS_CL_MULT_FIXED_COST(0,I,0) =
     +               MON_MDS_CL_MULT_FIXED_COST(0,I,0) +
     +                       MON_MDS_CL_MULT_FIXED_COST(CLASS,I,R_ISEAS)
               MON_MDS_CL_MULT_CAPACITY(0,I,0) =
     +               MON_MDS_CL_MULT_CAPACITY(0,I,0) +
     +                       MON_MDS_CL_MULT_CAPACITY(CLASS,I,R_ISEAS)
               MON_MDS_CL_MULT_ENERGY(0,I,0) =
     +               MON_MDS_CL_MULT_ENERGY(0,I,0) +
     +                       MON_MDS_CL_MULT_ENERGY(CLASS,I,R_ISEAS)
               MON_MDS_CL_MULT_PURCHASES(0,I,0) =
     +               MON_MDS_CL_MULT_PURCHASES(0,I,0) +
     +                       MON_MDS_CL_MULT_PURCHASES(CLASS,I,R_ISEAS)
            ENDDO
         ENDDO
      RETURN
!***********************************************************************
      ENTRY RETURN_ECITIES_OBJ_VARS(R_CLASS,
     +                              R_ECITIES_WHOLESALE_PROD_COST,  ! 684  C-PROD
     +                              R_ECITIES_VAR_PROD_COST,        ! 685  C-SUPP
     +                              R_ECITIES_NEW_FIXED_COST,       ! 686  C-CAP
     +                              R_ECITIES_MARKET_ENERGY_SALES,  ! 687  R
     +                              R_ECITIES_TRANSMISSION_FEES)     ! 688  C-FEES
!***********************************************************************
         IF(R_CLASS <= MAX_CAP_LIMITED_CLASS_ID_NUM .AND. ECITIES) THEN
            IF(R_CLASS ==0) THEN
               CLASS = 0
            ELSE
               CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(CLASS > 0 .OR. R_CLASS == 0) THEN
               THIS_YEAR = YEAR + BASE_YEAR
               DO MO = 0, 12
!
                  TEMP_L =
     +                  POWER_DERIV_REV_EXP_BY_CLASS( R_CLASS,
     +                                             MO,
     +                                             TOTAL_DERIV_REV,
     +                                             TOTAL_DERIV_EXP)
!
                  R_MONTH_AC_WHOLESALE_PROD_COST(MO) =
     +                   MONTHLY_AC_WHOLESALE_PROD_COST(CLASS,MO)
                  R_MONTH_AC_ECITY_VAR_PROD_COST(MO) =
     +                   MONTHLY_AC_ECITY_VAR_PROD_COST(CLASS,MO)
                  R_MONTH_AC_ECITY_NEW_FIX_COST(MO) =
     +                    MONTHLY_AC_ECITY_NEW_FIX_COST(CLASS,MO)
                  R_MONTH_AC_ECITY_SUPP(MO) =

     +                  MONTHLY_AC_ECITY_VAR_PROD_COST(CLASS,MO)
                  R_MONTH_AC_ECITY_REVENUE(MO) =
     +                  MON_MDS_CL_CLASS_REVENUE(CLASS,1,MO)
     +                  + MON_MDS_CL_CLASS_REVENUE(CLASS,2,MO)
     +                  + MON_MDS_CL_CLASS_REVENUE(CLASS,3,MO)
     +                  + MON_MDS_CL_CLASS_REVENUE(CLASS,4,MO)
     +                  + TOTAL_DERIV_REV
                  R_MONTH_AC_ECITY_FEES(MO) = TOTAL_DERIV_EXP
!
!  CHECK UNITS, KATHY TO PASS EXAMPLE
!
                  R_WTB(MO) =  R_MONTH_AC_ECITY_REVENUE(MO)  ! DONE.
     +                         - R_MONTH_AC_WHOLESALE_PROD_COST(MO)  ! DONE.
     +                         - R_MONTH_AC_ECITY_SUPP(MO)  ! DONE.
     +                         - R_MONTH_AC_ECITY_FEES(MO)  ! DONE.
     +                         - R_MONTH_AC_ECITY_NEW_FIX_COST(MO) ! DONE.
!
                  ECITY_OBJ_REPORT = .TRUE. ! HARD-WIRED FOR NOW.
                  IF(ECITY_OBJ_REPORT) THEN
!
                     IF(ECITY_OBJ_REPORT_NOT_OPEN) THEN
                        ECITY_OBJ_REPORT_NOT_OPEN = .FALSE.
                        ECITY_VARIABLE_NUMBER = 8
                        ECITY_OBJ_UNIT =
     +                     ECITY_OBJ_HEADER(ECITY_VARIABLE_NUMBER,
     +                                                    ECITY_OBJ_REC)
                     ENDIF
!
                     IF(MO > 0) THEN
                        LOCAL_MONTH = MO
                     ELSE
                        LOCAL_MONTH = 13
                     ENDIF
!
                     WRITE(ECITY_OBJ_UNIT,REC=ECITY_OBJ_REC)
     +                        PRT_ENDPOINT(),
     +                        FLOAT(THIS_YEAR),
     +                        FLOAT(R_CLASS-1),
     +                        CL_MONTH_NAME(LOCAL_MONTH),
     +                        R_WTB(MO),
     +                        R_MONTH_AC_ECITY_REVENUE(MO),
     +                        R_MONTH_AC_WHOLESALE_PROD_COST(MO),
     +                        R_MONTH_AC_ECITY_SUPP(MO),
     +                        R_MONTH_AC_ECITY_FEES(MO),
     +                        R_MONTH_AC_ECITY_NEW_FIX_COST(MO),
     +                        TOTAL_DERIV_REV,
     +                        TOTAL_DERIV_EXP
                     ECITY_OBJ_REC = ECITY_OBJ_REC + 1
!
                  ENDIF
               ENDDO
!
            ELSE ! ASSET CLASSES NOT ACTIVE
!
               DO MO = 0, 12
                  R_WTB(MO) = 0.
                  R_MONTH_AC_ECITY_REVENUE(MO) = 0.  ! DONE.
                  R_MONTH_AC_WHOLESALE_PROD_COST(MO) = 0.  ! DONE.
                  R_MONTH_AC_ECITY_SUPP(MO) = 0.  ! DONE.
                  R_MONTH_AC_ECITY_FEES(MO) = 0.  ! DONE.
                  R_MONTH_AC_ECITY_NEW_FIX_COST(MO) = 0. ! DONE.
               ENDDO
!
            ENDIF
            RETURN_ECITIES_OBJ_VARS = R_WTB(0)
            R_ECITIES_WHOLESALE_PROD_COST =
     +                                 R_MONTH_AC_WHOLESALE_PROD_COST(0)  ! 684  C-PROD
            R_ECITIES_VAR_PROD_COST = R_MONTH_AC_ECITY_SUPP(0)        ! 685  C-SUPP
            R_ECITIES_NEW_FIXED_COST = R_MONTH_AC_ECITY_NEW_FIX_COST(0)       ! 686  C-CAP
            R_ECITIES_MARKET_ENERGY_SALES = R_MONTH_AC_ECITY_REVENUE(0)  ! 687  R
            R_ECITIES_TRANSMISSION_FEES = R_MONTH_AC_ECITY_FEES(0)     ! 688  C-FEES
         ELSE
            RETURN_ECITIES_OBJ_VARS = 0.
            R_ECITIES_WHOLESALE_PROD_COST = 0.
            R_ECITIES_VAR_PROD_COST = 0.
            R_ECITIES_NEW_FIXED_COST = 0.
            R_ECITIES_MARKET_ENERGY_SALES = 0.
            R_ECITIES_TRANSMISSION_FEES = 0.
         ENDIF
      RETURN
!***********************************************************************
      ENTRY CL_EXPENSES_2_ASSET_CLASSES()
!***********************************************************************

         ALLOCATE(ASSET_CLASS_LIST(AVAIL_DATA_YEARS))
         ALLOCATE(ASSET_ALLOCATION_LIST(AVAIL_DATA_YEARS))
         CURRENT_YEAR = YEAR+BASE_YEAR
         CURRENT_YEAR_COMPARISON = (CURRENT_YEAR-1900)*100
         CL_ANN_CLASS_VAR_COST = 0.
         CL_ANN_CLASS_FUEL_COST = 0.
         NF_FUEL_LEASED_BY_CLASS = 0.
         NF_FUEL_OWNED_BY_CLASS = 0.
         CL_ANN_CLASS_FIXED_COST = 0.
         CL_ANN_CLASS_CAPACITY = 0.
         CL_ANN_CLASS_ENERGY = 0.
! SLOT 5 RESERVED FOR AMEREN 12/11/01
         CL_ANN_CLASS_PURCHASES = 0.
         CL_ANN_CLASS_REVENUE = 0.
         CL_ANN_CLASS_EMISSIONS = 0.
         INTRA_COMPANY_REVENUES = 0.
         INTRA_COMPANY_NF_BURN = 0.
         NUC_FUEL_LEASED_BURN_BY_CLASS = 0.
         NUC_FUEL_OWNED_BURN_BY_CLASS = 0.
         NUCLEAR_MWH_BY_CLASS = 0.
         NUCLEAR_MMBTU_BY_CLASS = 0.
!
         MAX_INTRA_CLASS = 0
         INTRA_FOSSIL_FUEL_EXPENSES = 0.
         INTRA_LEASED_NUC_FUEL_EXPENSES = 0.
         INTRA_OWNED_NUC_FUEL_EXPENSES = 0.
         INTRA_NUC_OWN_BURN = 0.
         INTRA_NUC_LEASE_BURN = 0.
         INTRA_PURCHASE_EXPENSES = 0.
         BASE_MARKET_ENRG_SALES = 0.
         BASE_MARKET_REVENUES = 0.
         PEAK_MARKET_ENRG_SALES = 0.
         PEAK_MARKET_REVENUES = 0.
         CL_EXPENSES_2_ASSET_CLASSES = UNUM
!
         CL_ANN_MULT_FUEL_COST = 0.
         FE_ANN_MULT_FUEL_COST = 0.
         CL_ANN_MULT_VAR_COST = 0.
         CL_ANN_MULT_FIXED_COST = 0.
         CL_ANN_MULT_REVENUE = 0.
         CL_ANN_MULT_CAPACITY = 0.
         CL_ANN_MULT_ENERGY = 0.
         CL_ANN_MULT_PURCHASES = 0.
!
         DO UNUM = 1, NUNITS
!
            IF(ONLINE(UNUM) - CURRENT_YEAR_COMPARISON > 12  .OR.
     +                CURRENT_YEAR_COMPARISON - OFLINE(UNUM) > 12) CYCLE
!
!
            IF(INTRA_COMPANY_CLASS_ID(UNUM) >= 0 .AND.
     +                      INTRA_COMPANY_TRANSACTION(UNUM) == 'Y') THEN
               ASSET_CLASS = INTRA_COMPANY_CLASS_ID(UNUM)
               CALL CHECK_IF_CLASS_DEFINED(ASSET_CLASS)
               ASSET_CLASS = ASSET_CLASS + 1
               MAX_INTRA_CLASS = MAX(MAX_INTRA_CLASS,ASSET_CLASS)
!
               SELECT CASE (EXPENSE_ASSIGNMENT(UNUM))
               CASE('F')  ! FUEL REVENUES
                  COLLECTION = MAX(1,
     +                           INDEX('BA',(EXPENSE_COLLECTION(UNUM))))
                  INTRA_COMPANY_REVENUES(ASSET_CLASS,COLLECTION) =
     +                  INTRA_COMPANY_REVENUES(ASSET_CLASS,COLLECTION) +
     +                                    ANNUAL_CL_UNIT_FUEL_COST(UNUM)
     +                                      *CL_POOL_FRAC_OWN(UNUM)/100.
               CASE('L','O')  ! FUEL REVENUES
                  COLLECTION = MAX(1,
     +                           INDEX('BA',(EXPENSE_COLLECTION(UNUM))))
                  INTRA_COMPANY_REVENUES(ASSET_CLASS,COLLECTION) =
     +                  INTRA_COMPANY_REVENUES(ASSET_CLASS,COLLECTION) +
     +                                    ANNUAL_CL_UNIT_FUEL_COST(UNUM)
     +                                      *CL_POOL_FRAC_OWN(UNUM)/100.
                  INTRA_COMPANY_NF_BURN(ASSET_CLASS) =
     +                           INTRA_COMPANY_NF_BURN(ASSET_CLASS) +
     +                           (ANNUAL_CL_UNIT_FUEL_COST(UNUM)-
     +                            ANNUAL_NUC_UNIT_FUEL_ADDER_COST(UNUM))
     +                                      *CL_POOL_FRAC_OWN(UNUM)/100.
               CASE('P')  ! PURCHASE REVENUES
                  INTRA_COMPANY_REVENUES(ASSET_CLASS,3) =
     +                   INTRA_COMPANY_REVENUES(ASSET_CLASS,3) +
     +                                  (ANNUAL_CL_UNIT_FUEL_COST(UNUM)+
     +                                   ANNUAL_CL_UNIT_VAR_COST(UNUM) +
     +                                  ANNUAL_CL_UNIT_FIXED_COST(UNUM))
     +                                      *CL_POOL_FRAC_OWN(UNUM)/100.
               CASE DEFAULT
               END SELECT
!
               SELECT CASE (EXPENSE_ASSIGNMENT(UNUM))
               CASE('F')  ! FUEL EXPENSES
                  INTRA_FOSSIL_FUEL_EXPENSES =
     +                                    INTRA_FOSSIL_FUEL_EXPENSES +
     +                                    ANNUAL_CL_UNIT_FUEL_COST(UNUM)
     +                                      *CL_POOL_FRAC_OWN(UNUM)/100.
               CASE('L')  ! NUCLEAR FUEL EXPENSES
                  INTRA_LEASED_NUC_FUEL_EXPENSES =
     +                                  INTRA_LEASED_NUC_FUEL_EXPENSES +
     +                                  ANNUAL_CL_UNIT_FUEL_COST(UNUM)
     +                                      *CL_POOL_FRAC_OWN(UNUM)/100.
                  INTRA_NUC_LEASE_BURN = INTRA_NUC_LEASE_BURN +
     +                           (ANNUAL_CL_UNIT_FUEL_COST(UNUM)-
     +                            ANNUAL_NUC_UNIT_FUEL_ADDER_COST(UNUM))
     +                                      *CL_POOL_FRAC_OWN(UNUM)/100.
               CASE('O')  ! NUCLEAR FUEL EXPENSES
                  INTRA_OWNED_NUC_FUEL_EXPENSES =
     +                                   INTRA_OWNED_NUC_FUEL_EXPENSES +
     +                                   ANNUAL_CL_UNIT_FUEL_COST(UNUM)
     +                                      *CL_POOL_FRAC_OWN(UNUM)/100.
                  INTRA_NUC_OWN_BURN = INTRA_NUC_OWN_BURN +
     +                           (ANNUAL_CL_UNIT_FUEL_COST(UNUM)-
     +                            ANNUAL_NUC_UNIT_FUEL_ADDER_COST(UNUM))
     +                                      *CL_POOL_FRAC_OWN(UNUM)/100.
               CASE('P')  ! PURCHASE EXPENSES
                  INTRA_PURCHASE_EXPENSES = INTRA_PURCHASE_EXPENSES +
     +                                  (ANNUAL_CL_UNIT_FUEL_COST(UNUM)+
     +                                   ANNUAL_CL_UNIT_VAR_COST(UNUM) +
     +                                  ANNUAL_CL_UNIT_FIXED_COST(UNUM))
     +                                      *CL_POOL_FRAC_OWN(UNUM)/100.
               CASE DEFAULT
               END SELECT
            ENDIF
!
            COLLECTION = INDEX('ABNX',(EXPENSE_COLLECTION(UNUM)))
            IF(COLLECTION == 0) COLLECTION = 3
!
            ASSET_CLASS = ASSET_CLASS_NUM(UNUM)
            ASSET_ALLOCATION_VECTOR = ASSET_CLASS_VECTOR(UNUM)
!
!
            VOID_LOGICAL=RETURN_ASSET_CLASS_LISTS(ASSET_CLASS,
     +                                          ASSET_CLASS_LIST,
     +                                          ASSET_ALLOCATION_VECTOR,
     +                                          ASSET_ALLOCATION_LIST)
!
            FT = PRIMARY_MOVER(UNUM)

            if(ft == 0) ft = 11
            TT = 10

            CLASS_POINTER = 1
            DO
               ASSET_CLASS = ASSET_CLASS_LIST(CLASS_POINTER)
               CALL CHECK_IF_CLASS_DEFINED(ASSET_CLASS)
!
! IDENTIFYING BASE AND PEAK EL REVENUE CLASSED
!
               BASE_EL_REVENUE_CASE = ASSET_CLASS == 10
               PEAK_EL_REVENUE_CASE = ASSET_CLASS == 19
!
               ASSET_CLASS = ASSET_CLASS + 1
               IF(ASSET_CLASS <= 0 .OR.
     +                 ASSET_CLASS > MAX_CAP_LIMITED_CLASS_ID_NUM) CYCLE
               ASSET_CLASS = ASSET_CLASS_POINTER(ASSET_CLASS)
               IF(ASSET_ALLOCATION_LIST(CLASS_POINTER) < 0.) THEN
                  ALLOCATION_VECTOR =
     +                         ABS(ASSET_ALLOCATION_LIST(CLASS_POINTER))
                  CALL GET_ASSET_VAR(ALLOCATION_VECTOR,
     +                                      DUMMY_TYPE,ALLOCATION_VALUE)
                  ASSET_ALLOCATOR = CL_POOL_FRAC_OWN(UNUM)/100. *
     +                 ALLOCATION_VALUE(MIN(YEAR,AVAIL_DATA_YEARS))/100.
               ELSE
                  ASSET_ALLOCATOR =
     +                       ASSET_ALLOCATION_LIST(CLASS_POINTER)/100. *
     +                       CL_POOL_FRAC_OWN(UNUM)/100.
               ENDIF
!
               DO I = 1, NUMBER_OF_EMISSION_TYPES
                  CL_ANN_CLASS_EMISSIONS(I,ASSET_CLASS) =
     +                           CL_ANN_CLASS_EMISSIONS(I,ASSET_CLASS) +
     +                              ASSET_ALLOCATOR *
     +                                  ANNUAL_CL_UNIT_EMISSIONS(I,UNUM)
               ENDDO
               IF(EXPENSE_ASSIGNMENT(UNUM) == 'L') THEN
                  NF_FUEL_LEASED_BY_CLASS(ASSET_CLASS,COLLECTION) =
     +                 NF_FUEL_LEASED_BY_CLASS(ASSET_CLASS,COLLECTION) +
     +                  ASSET_ALLOCATOR * ANNUAL_CL_UNIT_FUEL_COST(UNUM)
                  NUC_FUEL_LEASED_BURN_BY_CLASS(ASSET_CLASS) =
     +                NUC_FUEL_LEASED_BURN_BY_CLASS(ASSET_CLASS) +
     +                ASSET_ALLOCATOR * (ANNUAL_CL_UNIT_FUEL_COST(UNUM)-
     +                            ANNUAL_NUC_UNIT_FUEL_ADDER_COST(UNUM))
                  NUCLEAR_MWH_BY_CLASS(ASSET_CLASS) =
     +                     NUCLEAR_MWH_BY_CLASS(ASSET_CLASS) +
     +                     ASSET_ALLOCATOR * ANNUAL_CL_UNIT_ENERGY(UNUM)
                  NUCLEAR_MMBTU_BY_CLASS(ASSET_CLASS) =
     +                     NUCLEAR_MMBTU_BY_CLASS(ASSET_CLASS) +
     +                     ASSET_ALLOCATOR * ANNUAL_CL_UNIT_MMBTUS(UNUM)
               ELSEIF(EXPENSE_ASSIGNMENT(UNUM) == 'O') THEN
                  NF_FUEL_OWNED_BY_CLASS(ASSET_CLASS,COLLECTION) =
     +                  NF_FUEL_OWNED_BY_CLASS(ASSET_CLASS,COLLECTION) +
     +                  ASSET_ALLOCATOR *
     +                                   ANNUAL_CL_UNIT_FUEL_COST(UNUM)
                  NUC_FUEL_OWNED_BURN_BY_CLASS(ASSET_CLASS) =
     +                NUC_FUEL_OWNED_BURN_BY_CLASS(ASSET_CLASS) +
     +                ASSET_ALLOCATOR * (
     +                                   ANNUAL_CL_UNIT_FUEL_COST(UNUM)-
     +                            ANNUAL_NUC_UNIT_FUEL_ADDER_COST(UNUM))
                  NUCLEAR_MWH_BY_CLASS(ASSET_CLASS) =
     +                     NUCLEAR_MWH_BY_CLASS(ASSET_CLASS) +
     +                     ASSET_ALLOCATOR * ANNUAL_CL_UNIT_ENERGY(UNUM)
                  NUCLEAR_MMBTU_BY_CLASS(ASSET_CLASS) =
     +                     NUCLEAR_MMBTU_BY_CLASS(ASSET_CLASS) +
     +                     ASSET_ALLOCATOR * ANNUAL_CL_UNIT_MMBTUS(UNUM)
               ELSEIF(EXPENSE_ASSIGNMENT(UNUM) == 'P') THEN
                  CL_ANN_CLASS_PURCHASES(ASSET_CLASS,COLLECTION) =
     +                  CL_ANN_CLASS_PURCHASES(ASSET_CLASS,COLLECTION) +
     +                        ASSET_ALLOCATOR *
     +                             (ANNUAL_CL_UNIT_FUEL_COST(UNUM) +
     +                                ANNUAL_CL_UNIT_VAR_COST(UNUM) +
     +                                  ANNUAL_CL_UNIT_FIXED_COST(UNUM))
                  CL_ANN_CLASS_CAPACITY(ASSET_CLASS,2) =
     +                            CL_ANN_CLASS_CAPACITY(ASSET_CLASS,2) +
     +                   ASSET_ALLOCATOR * ANNUAL_CL_UNIT_CAPACITY(UNUM)
                  CL_ANN_CLASS_ENERGY(ASSET_CLASS,2) =
     +                              CL_ANN_CLASS_ENERGY(ASSET_CLASS,2) +
     +                     ASSET_ALLOCATOR * ANNUAL_CL_UNIT_ENERGY(UNUM)

               ELSE
                  CL_ANN_CLASS_FUEL_COST(ASSET_CLASS,COLLECTION) =
     +                  CL_ANN_CLASS_FUEL_COST(ASSET_CLASS,COLLECTION) +
     +                  ASSET_ALLOCATOR * ANNUAL_CL_UNIT_FUEL_COST(UNUM)
                  CL_ANN_MULT_FUEL_COST(ASSET_CLASS,FT) =
     +                  CL_ANN_MULT_FUEL_COST(ASSET_CLASS,FT) +
     +                  ASSET_ALLOCATOR * ANNUAL_CL_UNIT_FUEL_COST(UNUM)
                  IF(FIRST_ENERGY) THEN
                     IF(SPECIAL_UNIT_ID(UNUM) == 'BayShore') THEN
                        FE_ANN_MULT_FUEL_COST(ASSET_CLASS,3) =
     +                     FE_ANN_MULT_FUEL_COST(ASSET_CLASS,3) +
     +                     ASSET_ALLOCATOR *
     +                                    ANNUAL_CL_UNIT_FUEL_COST(UNUM)

                     ELSEIF(PRIMARY_MOVER(UNUM) == 3 .OR.
     +                                    PRIMARY_MOVER(UNUM) == 5) THEN
                        FE_ANN_MULT_FUEL_COST(ASSET_CLASS,6) =
     +                     FE_ANN_MULT_FUEL_COST(ASSET_CLASS,6) +
     +                     ASSET_ALLOCATOR *
     +                                    ANNUAL_CL_UNIT_FUEL_COST(UNUM)
                     ELSE
                        FE_ANN_MULT_FUEL_COST(ASSET_CLASS,FT) =
     +                     FE_ANN_MULT_FUEL_COST(ASSET_CLASS,FT) +
     +                     ASSET_ALLOCATOR *
     +                                    ANNUAL_CL_UNIT_FUEL_COST(UNUM)
                     ENDIF
                  ENDIF
               ENDIF
               IF(EXPENSE_ASSIGNMENT(UNUM) /= 'P') THEN
                  CL_ANN_CLASS_VAR_COST(ASSET_CLASS,COLLECTION) =
     +                   CL_ANN_CLASS_VAR_COST(ASSET_CLASS,COLLECTION) +
     +                   ASSET_ALLOCATOR * ANNUAL_CL_UNIT_VAR_COST(UNUM)
                  CL_ANN_CLASS_FIXED_COST(ASSET_CLASS,COLLECTION)=
     +                 CL_ANN_CLASS_FIXED_COST(ASSET_CLASS,COLLECTION) +
     +                     ASSET_ALLOCATOR *
     +                                   ANNUAL_CL_UNIT_FIXED_COST(UNUM)
                  CL_ANN_CLASS_CAPACITY(ASSET_CLASS,1) =
     +                            CL_ANN_CLASS_CAPACITY(ASSET_CLASS,1) +
     +                   ASSET_ALLOCATOR * ANNUAL_CL_UNIT_CAPACITY(UNUM)
                  CL_ANN_CLASS_ENERGY(ASSET_CLASS,1) =
     +                              CL_ANN_CLASS_ENERGY(ASSET_CLASS,1) +
     +                     ASSET_ALLOCATOR * ANNUAL_CL_UNIT_ENERGY(UNUM)
!
                  CL_ANN_MULT_VAR_COST(ASSET_CLASS,FT) =
     +                   CL_ANN_MULT_VAR_COST(ASSET_CLASS,FT) +
     +                   ASSET_ALLOCATOR * ANNUAL_CL_UNIT_VAR_COST(UNUM)
                  CL_ANN_MULT_FIXED_COST(ASSET_CLASS,FT)=
     +                 CL_ANN_MULT_FIXED_COST(ASSET_CLASS,FT) +
     +                     ASSET_ALLOCATOR *
     +                                   ANNUAL_CL_UNIT_FIXED_COST(UNUM)
                  CL_ANN_MULT_CAPACITY(ASSET_CLASS,FT) =
     +                            CL_ANN_MULT_CAPACITY(ASSET_CLASS,FT) +
     +                   ASSET_ALLOCATOR * ANNUAL_CL_UNIT_CAPACITY(UNUM)
                  CL_ANN_MULT_ENERGY(ASSET_CLASS,FT) =
     +                              CL_ANN_MULT_ENERGY(ASSET_CLASS,FT) +
     +                     ASSET_ALLOCATOR * ANNUAL_CL_UNIT_ENERGY(UNUM)
               ENDIF
               IF(ECO_SALES_ENRG_FROM(UNUM) /= 0.) THEN
                  CL_ANN_CLASS_REVENUE(ASSET_CLASS,COLLECTION) =
     +                    CL_ANN_CLASS_REVENUE(ASSET_CLASS,COLLECTION) +
     +                        ASSET_ALLOCATOR * ECO_SALES_REV_FROM(UNUM)
                  CL_ANN_CLASS_ENERGY(ASSET_CLASS,3) =
     +                              CL_ANN_CLASS_ENERGY(ASSET_CLASS,3) +
     +                       ASSET_ALLOCATOR * ECO_SALES_ENRG_FROM(UNUM)
!
                  IF(ANNUAL_CL_UNIT_ENERGY(UNUM) > 0.) THEN
!
                     TT_PERCENT =  ECO_SALES_ENRG_FROM(UNUM)/
     +                                       ANNUAL_CL_UNIT_ENERGY(UNUM)
!
                     CL_ANN_MULT_FUEL_COST(ASSET_CLASS,TT) =
     +                  CL_ANN_MULT_FUEL_COST(ASSET_CLASS,TT) +
     +                  ASSET_ALLOCATOR *
     +                       ANNUAL_CL_UNIT_FUEL_COST(UNUM) * TT_PERCENT
                     CL_ANN_MULT_VAR_COST(ASSET_CLASS,TT) =
     +                   CL_ANN_MULT_VAR_COST(ASSET_CLASS,TT) +
     +                 ASSET_ALLOCATOR * ANNUAL_CL_UNIT_VAR_COST(UNUM) *
     +                                                        TT_PERCENT
                     CL_ANN_MULT_CAPACITY(ASSET_CLASS,TT) =
     +                            CL_ANN_MULT_CAPACITY(ASSET_CLASS,TT) +
     +                 ASSET_ALLOCATOR * ANNUAL_CL_UNIT_CAPACITY(UNUM) *
     +                                                        TT_PERCENT
                     CL_ANN_MULT_ENERGY(ASSET_CLASS,TT) =
     +                              CL_ANN_MULT_ENERGY(ASSET_CLASS,TT) +
     +                   ASSET_ALLOCATOR * ANNUAL_CL_UNIT_ENERGY(UNUM) *
     +                                                        TT_PERCENT
                  ENDIF
!
                  IF(BASE_EL_REVENUE_CASE) THEN
                     BASE_MARKET_ENRG_SALES = BASE_MARKET_ENRG_SALES +
     +                       ASSET_ALLOCATOR * ECO_SALES_ENRG_FROM(UNUM)
                     BASE_MARKET_REVENUES = BASE_MARKET_REVENUES +
     +                        ASSET_ALLOCATOR * ECO_SALES_REV_FROM(UNUM)
                  ELSEIF(PEAK_EL_REVENUE_CASE) THEN
                     PEAK_MARKET_ENRG_SALES = PEAK_MARKET_ENRG_SALES +
     +                       ASSET_ALLOCATOR * ECO_SALES_ENRG_FROM(UNUM)
                     PEAK_MARKET_REVENUES = PEAK_MARKET_REVENUES +
     +                        ASSET_ALLOCATOR * ECO_SALES_REV_FROM(UNUM)
                  ENDIF
               ENDIF
               IF(ECO_PUCH_ENRG_FROM(UNUM) /= 0.) THEN
! THIS WILL WORK ONLY FOR ASSET ANALYST.
                  IF(UPPER_TRANS_GROUP > 0) THEN
                     TG = TRANSACTION_GROUP_ID(UNUM) ! NOTE DOUBLE ASSIGNMENT
                     TG = GET_TRANS_GROUP_POSITION(TG)
                     IF(PURCHASE_POWER_ASSIGN(TG) /= 'U') THEN
                        TG_ASSET_CLASS = PURCHASE_ASSET_CLASS_ID(TG)
!
                        CALL CHECK_IF_CLASS_DEFINED(TG_ASSET_CLASS)
                        TG_ASSET_CLASS = TG_ASSET_CLASS + 1
                        IF(TG_ASSET_CLASS > 0 .AND. TG_ASSET_CLASS <=
     +                                MAX_CAP_LIMITED_CLASS_ID_NUM) THEN
                           TG_ASSET_CLASS =
     +                               ASSET_CLASS_POINTER(TG_ASSET_CLASS)
                           CL_ANN_CLASS_PURCHASES(TG_ASSET_CLASS,
     +                                                     COLLECTION) =
     +                        CL_ANN_CLASS_PURCHASES(TG_ASSET_CLASS,
     +                                                     COLLECTION) +
     +                                          ECO_PUCH_COST_FROM(UNUM)
                           ECO_PUCH_COST_FROM(UNUM) = 0.
                        ENDIF
                     ENDIF
                  ENDIF
!
!
                  CL_ANN_CLASS_PURCHASES(ASSET_CLASS,COLLECTION) =
     +                  CL_ANN_CLASS_PURCHASES(ASSET_CLASS,COLLECTION) +
     +                        ASSET_ALLOCATOR * ECO_PUCH_COST_FROM(UNUM)
                  CL_ANN_CLASS_ENERGY(ASSET_CLASS,4) =
     +                              CL_ANN_CLASS_ENERGY(ASSET_CLASS,4) +
     +                        ASSET_ALLOCATOR * ECO_PUCH_ENRG_FROM(UNUM)

               ENDIF
!
               CLASS_POINTER = CLASS_POINTER + 1
               IF(CLASS_POINTER > AVAIL_DATA_YEARS) EXIT
               IF(ASSET_CLASS_LIST(CLASS_POINTER) == 0. .OR.
     +                     ASSET_CLASS_LIST(CLASS_POINTER) == -99.) EXIT
            ENDDO ! ASSET CLASSES
         ENDDO ! CL UNITS
!
! 09/02/01. AMEREN TRANSFER PRICING CAPABILITY. TWO PARTY FOR NOW.
!           NEED A SWITCH TO DEFINE.
!
         IF(DETAILED_TRANSFER_PRICING) THEN
            TEMP_I2 = 0
            CALL GET_MONTHLY_TRANSFER_REV_COST(
     +                                    TEMP_I2,
     +                                    R1_MONTHLY_TRANSFER_REV,
     +                                    R1_MONTHLY_TRANSFER_COST,
     +                                    R2_MONTHLY_TRANSFER_REV,
     +                                    R2_MONTHLY_TRANSFER_COST)
            TG = 1 ! ASSUME UE FOR NOW.
            TG_ASSET_CLASS = PURCHASE_ASSET_CLASS_ID(TG)
            CALL CHECK_IF_CLASS_DEFINED(TG_ASSET_CLASS)
            COLLECTION = 5 ! ASSUME
            TG_ASSET_CLASS = TG_ASSET_CLASS + 1
            IF(TG_ASSET_CLASS > 0 .AND. TG_ASSET_CLASS <=
     +                                MAX_CAP_LIMITED_CLASS_ID_NUM) THEN
               TG_ASSET_CLASS = ASSET_CLASS_POINTER(TG_ASSET_CLASS)
               CL_ANN_CLASS_PURCHASES(TG_ASSET_CLASS,COLLECTION) =
     +               CL_ANN_CLASS_PURCHASES(TG_ASSET_CLASS,COLLECTION) +
     +                                          R1_MONTHLY_TRANSFER_COST ! FROM TRANSACT MODULE
               CL_ANN_CLASS_REVENUE(TG_ASSET_CLASS,COLLECTION) =
     +                 CL_ANN_CLASS_REVENUE(TG_ASSET_CLASS,COLLECTION) +
     +                                          R1_MONTHLY_TRANSFER_REV ! FROM TRANSACT MODULE
            ENDIF
            TG = 2 ! ASSUME UE FOR NOW.
            TG_ASSET_CLASS = PURCHASE_ASSET_CLASS_ID(TG)
            CALL CHECK_IF_CLASS_DEFINED(TG_ASSET_CLASS)
            COLLECTION = 5 ! ASSUME
            TG_ASSET_CLASS = TG_ASSET_CLASS + 1
            IF(TG_ASSET_CLASS > 0 .AND. TG_ASSET_CLASS <=
     +                                MAX_CAP_LIMITED_CLASS_ID_NUM) THEN
               TG_ASSET_CLASS = ASSET_CLASS_POINTER(TG_ASSET_CLASS)
               CL_ANN_CLASS_PURCHASES(TG_ASSET_CLASS,COLLECTION) =
     +               CL_ANN_CLASS_PURCHASES(TG_ASSET_CLASS,COLLECTION) +
     +                                          R2_MONTHLY_TRANSFER_COST ! FROM TRANSACT MODULE
               CL_ANN_CLASS_REVENUE(TG_ASSET_CLASS,COLLECTION) =
     +                 CL_ANN_CLASS_REVENUE(TG_ASSET_CLASS,COLLECTION) +
     +                                          R2_MONTHLY_TRANSFER_REV ! FROM TRANSACT MODULE
            ENDIF
         ENDIF
!
! GET COST OF SUPPLYING CAPACITY FOR A "SHORT" SYSTEM.
!
         RUN_TRANSACT = YES_RUN_TRANSACT() ! 10/28/03.
!
         IF(UPPER_TRANS_GROUP > 0 .AND. RUN_TRANSACT) THEN

            DO TG = 1, UPPER_TRANS_GROUP

               TEMP_I2 = 0
               CALL GET_ANNUAL_UNSERVED_COST(
     +                                    TG,
     +                                    TRANS_ANNUAL_UNSERVED_COST,
     +                                    TEMP_R4, ! UNSERVED MWH'S
     +                                    TEMP_I2) ! ANNUAL CALL 6/25/01.

                  IF(PURCHASE_POWER_ASSIGN(TG) == 'A') THEN
                     COLLECTION = 1
                  ELSEIF(PURCHASE_POWER_ASSIGN(TG) == 'B') THEN
                     COLLECTION = 2
                  ELSEIF(PURCHASE_POWER_ASSIGN(TG) == 'C') THEN
                     COLLECTION = 4
                  ELSE
                     COLLECTION = 3
                  ENDIF
                  TG_ASSET_CLASS = PURCHASE_ASSET_CLASS_ID(TG)

                  CALL CHECK_IF_CLASS_DEFINED(TG_ASSET_CLASS)
                  TG_ASSET_CLASS = TG_ASSET_CLASS + 1
                  IF(TG_ASSET_CLASS > 0 .AND. TG_ASSET_CLASS <=
     +                                MAX_CAP_LIMITED_CLASS_ID_NUM) THEN
                     TG_ASSET_CLASS =
     +                               ASSET_CLASS_POINTER(TG_ASSET_CLASS)
                     CL_ANN_CLASS_PURCHASES(TG_ASSET_CLASS,COLLECTION) =
     +                      CL_ANN_CLASS_PURCHASES(TG_ASSET_CLASS,
     +                                                     COLLECTION) +
     +                                        TRANS_ANNUAL_UNSERVED_COST ! FROM TRANSACT MODULE
                  ENDIF

            ENDDO
         ENDIF
!
         DEALLOCATE(ASSET_CLASS_LIST,ASSET_ALLOCATION_LIST)
!
! MARKET REVENUE PROXY FOR UNION EL UNITS 8/7/97
!
         EL_BASE_RATE = 0.
         EL_PEAK_RATE = 0.
         IF(BASE_MARKET_ENRG_SALES > 50 .AND.
     +                                  BASE_MARKET_REVENUES > 100) THEN
            EL_BASE_RATE = BASE_MARKET_REVENUES/BASE_MARKET_ENRG_SALES
         ENDIF
         IF(PEAK_MARKET_ENRG_SALES > 25 .AND.
     +                                   PEAK_MARKET_REVENUES > 50) THEN
            EL_PEAK_RATE = PEAK_MARKET_REVENUES/PEAK_MARKET_ENRG_SALES
         ENDIF
!
         DO COLLECTION = 0, 3
            DO CLASS = 1, MAX_INTRA_CLASS
               INTRA_COMPANY_REVENUES(0,COLLECTION) =
     +                          INTRA_COMPANY_REVENUES(0,COLLECTION) +
     +                          INTRA_COMPANY_REVENUES(CLASS,COLLECTION)
            ENDDO
         ENDDO
         DO CLASS = 1, MAX_INTRA_CLASS
            INTRA_COMPANY_NF_BURN(0) = INTRA_COMPANY_NF_BURN(0) +
     +                                      INTRA_COMPANY_NF_BURN(CLASS)
         ENDDO
!
         DO CLASS = 1, NUMBER_OF_CAP_LIMITED_CLASSES
            NUC_FUEL_LEASED_BURN_BY_CLASS(CLASS) =
     +                     NUC_FUEL_LEASED_BURN_BY_CLASS(CLASS)/1000000.
            NUC_FUEL_LEASED_BURN_BY_CLASS(0) =
     +                              NUC_FUEL_LEASED_BURN_BY_CLASS(0) +
     +                              NUC_FUEL_LEASED_BURN_BY_CLASS(CLASS)
            NUC_FUEL_OWNED_BURN_BY_CLASS(CLASS) =
     +                      NUC_FUEL_OWNED_BURN_BY_CLASS(CLASS)/1000000.
            NUC_FUEL_OWNED_BURN_BY_CLASS(0) =
     +                               NUC_FUEL_OWNED_BURN_BY_CLASS(0) +
     +                               NUC_FUEL_OWNED_BURN_BY_CLASS(CLASS)
            NUCLEAR_MWH_BY_CLASS(0) = NUCLEAR_MWH_BY_CLASS(0) +
     +                                       NUCLEAR_MWH_BY_CLASS(CLASS)
            NUCLEAR_MMBTU_BY_CLASS(0) = NUCLEAR_MMBTU_BY_CLASS(0) +
     +                                     NUCLEAR_MMBTU_BY_CLASS(CLASS)
            DO EMISS_TYPE = 1, NUMBER_OF_EMISSION_TYPES
               CL_ANN_CLASS_EMISSIONS(EMISS_TYPE,0) =
     +                          CL_ANN_CLASS_EMISSIONS(EMISS_TYPE,0) +
     +                          CL_ANN_CLASS_EMISSIONS(EMISS_TYPE,CLASS)
            ENDDO
            DO I = 1, 4
               CL_ANN_CLASS_VAR_COST(CLASS,I) =
     +                           CL_ANN_CLASS_VAR_COST(CLASS,I)/1000000.
               CL_ANN_CLASS_FUEL_COST(CLASS,I)  =
     +                          CL_ANN_CLASS_FUEL_COST(CLASS,I)/1000000.
               NF_FUEL_LEASED_BY_CLASS(CLASS,I) =
     +                         NF_FUEL_LEASED_BY_CLASS(CLASS,I)/1000000.
               NF_FUEL_OWNED_BY_CLASS(CLASS,I) =
     +                          NF_FUEL_OWNED_BY_CLASS(CLASS,I)/1000000.
               CL_ANN_CLASS_FIXED_COST(CLASS,I) =
     +                         CL_ANN_CLASS_FIXED_COST(CLASS,I)/1000000.
               CL_ANN_CLASS_REVENUE(CLASS,I) =
     +                            CL_ANN_CLASS_REVENUE(CLASS,I)/1000000.
               CL_ANN_CLASS_CAPACITY(CLASS,I) =
     +                           CL_ANN_CLASS_CAPACITY(CLASS,I)/1000000.
               CL_ANN_CLASS_ENERGY(CLASS,I) =
     +                             CL_ANN_CLASS_ENERGY(CLASS,I)/1000000.
               CL_ANN_CLASS_PURCHASES(CLASS,I) =
     +                          CL_ANN_CLASS_PURCHASES(CLASS,I)/1000000.
!
               CL_ANN_CLASS_VAR_COST(0,I) = CL_ANN_CLASS_VAR_COST(0,I) +
     +                                    CL_ANN_CLASS_VAR_COST(CLASS,I)
               CL_ANN_CLASS_FUEL_COST(0,I)=CL_ANN_CLASS_FUEL_COST(0,I) +
     +                                   CL_ANN_CLASS_FUEL_COST(CLASS,I)
               NF_FUEL_LEASED_BY_CLASS(0,I) =
     +                                  NF_FUEL_LEASED_BY_CLASS(0,I) +
     +                                  NF_FUEL_LEASED_BY_CLASS(CLASS,I)
               NF_FUEL_OWNED_BY_CLASS(0,I)=NF_FUEL_OWNED_BY_CLASS(0,I) +
     +                                   NF_FUEL_OWNED_BY_CLASS(CLASS,I)
               CL_ANN_CLASS_FIXED_COST(0,I) =
     +                                  CL_ANN_CLASS_FIXED_COST(0,I) +
     +                                  CL_ANN_CLASS_FIXED_COST(CLASS,I)
               CL_ANN_CLASS_REVENUE(0,I) = CL_ANN_CLASS_REVENUE(0,I) +
     +                                     CL_ANN_CLASS_REVENUE(CLASS,I)
               CL_ANN_CLASS_CAPACITY(0,I) = CL_ANN_CLASS_CAPACITY(0,I) +
     +                                    CL_ANN_CLASS_CAPACITY(CLASS,I)
               CL_ANN_CLASS_ENERGY(0,I) = CL_ANN_CLASS_ENERGY(0,I) +
     +                                      CL_ANN_CLASS_ENERGY(CLASS,I)
               CL_ANN_CLASS_PURCHASES(0,I)=CL_ANN_CLASS_PURCHASES(0,I) +
     +                                   CL_ANN_CLASS_PURCHASES(CLASS,I)
            ENDDO
            DO I = 1, NUM_FUEL_CATEGORIES
               CL_ANN_MULT_FUEL_COST(CLASS,I)  =
     +                           CL_ANN_MULT_FUEL_COST(CLASS,I)/1000000.
               IF(FIRST_ENERGY) THEN
                  FE_ANN_MULT_FUEL_COST(CLASS,I)  =
     +                           FE_ANN_MULT_FUEL_COST(CLASS,I)/1000000.
                  FE_ANN_MULT_FUEL_COST(0,I) =
     +                           FE_ANN_MULT_FUEL_COST(0,I) +
     +                                    FE_ANN_MULT_FUEL_COST(CLASS,I)
               ENDIF
               CL_ANN_MULT_VAR_COST(CLASS,I) =
     +                           CL_ANN_MULT_VAR_COST(CLASS,I)/1000000.
               CL_ANN_MULT_FIXED_COST(CLASS,I) =
     +                         CL_ANN_MULT_FIXED_COST(CLASS,I)/1000000.
               CL_ANN_MULT_REVENUE(CLASS,I) =
     +                            CL_ANN_MULT_REVENUE(CLASS,I)/1000000.
               CL_ANN_MULT_CAPACITY(CLASS,I) =
     +                           CL_ANN_MULT_CAPACITY(CLASS,I)/1000000.
               CL_ANN_MULT_ENERGY(CLASS,I) =
     +                             CL_ANN_MULT_ENERGY(CLASS,I)/1000000.
               CL_ANN_MULT_PURCHASES(CLASS,I) =
     +                          CL_ANN_MULT_PURCHASES(CLASS,I)/1000000.
!
               CL_ANN_MULT_FUEL_COST(0,I)=CL_ANN_MULT_FUEL_COST(0,I) +
     +                                    CL_ANN_MULT_FUEL_COST(CLASS,I)
               CL_ANN_MULT_VAR_COST(0,I) = CL_ANN_MULT_VAR_COST(0,I) +
     +                                    CL_ANN_MULT_VAR_COST(CLASS,I)
               CL_ANN_MULT_FIXED_COST(0,I) =
     +                                  CL_ANN_MULT_FIXED_COST(0,I) +
     +                                  CL_ANN_MULT_FIXED_COST(CLASS,I)
               CL_ANN_MULT_REVENUE(0,I) = CL_ANN_MULT_REVENUE(0,I) +
     +                                     CL_ANN_MULT_REVENUE(CLASS,I)
               CL_ANN_MULT_CAPACITY(0,I) = CL_ANN_MULT_CAPACITY(0,I) +
     +                                    CL_ANN_MULT_CAPACITY(CLASS,I)
               CL_ANN_MULT_ENERGY(0,I) = CL_ANN_MULT_ENERGY(0,I) +
     +                                      CL_ANN_MULT_ENERGY(CLASS,I)
               CL_ANN_MULT_PURCHASES(0,I) = CL_ANN_MULT_PURCHASES(0,I) +
     +                                   CL_ANN_MULT_PURCHASES(CLASS,I)
            ENDDO
         ENDDO
      RETURN
!***********************************************************************
      ENTRY GET_CL_ENERGY_BY_TYPE(  R_ISEAS,
     +                              R_NUC_ENERGY,
     +                              R_STEAM_ENERGY,
     +                              R_PURCHASES_ENERGY,
     +                              R_ASSET_CLASS_900)
!***********************************************************************
         R_NUC_ENERGY = 0.
         R_STEAM_ENERGY = 0.
         R_PURCHASES_ENERGY = 0.
!
         ASSET_CLASS = 0 ! SYSTEM CLASS
!
         R_NUC_ENERGY = R_NUC_ENERGY +
     +           MON_MDS_NUCLEAR_MWH_BY_CLASS(ASSET_CLASS,R_ISEAS)/1000.
         R_PURCHASES_ENERGY = R_PURCHASES_ENERGY +
     +            MON_MDS_CL_CLASS_ENERGY(ASSET_CLASS,2,R_ISEAS) * 1000.
!
!
         R_ASSET_CLASS_900 = 0.
         DO I = 900, MAX_CAP_LIMITED_CLASS_ID_NUM
!
            ASSET_CLASS = ASSET_CLASS_POINTER(I)
            IF(ASSET_CLASS == 0) CYCLE
            R_ASSET_CLASS_900 = R_ASSET_CLASS_900 +
     +            MON_MDS_CL_CLASS_ENERGY(ASSET_CLASS,1,R_ISEAS) * 1000.
         ENDDO
         ASSET_CLASS = 0 ! SYSTEM CLASS
         R_STEAM_ENERGY = R_STEAM_ENERGY +
     +          MON_MDS_CL_CLASS_ENERGY(ASSET_CLASS,1,R_ISEAS) * 1000. -
     +                                  R_NUC_ENERGY - R_ASSET_CLASS_900
!
         GET_CL_ENERGY_BY_TYPE = 1
      RETURN
!***********************************************************************
      ENTRY EL_BASE_REVENUE_RATE
!***********************************************************************
         EL_BASE_REVENUE_RATE = EL_BASE_RATE
      RETURN
!***********************************************************************
      ENTRY EL_PEAK_REVENUE_RATE
!***********************************************************************
         EL_PEAK_REVENUE_RATE = EL_PEAK_RATE
      RETURN
!***********************************************************************
      ENTRY RETURN_CL_ASSET_CLASS_PROD(R_CLASS,
     +                                 R_CL_ANN_CLASS_CAPACITY,
     +                                 R_CL_ANN_CLASS_ENERGY)
!***********************************************************************
         RETURN_CL_ASSET_CLASS_PROD = .FALSE.
         DO I = 1, 4
            R_CL_ANN_CLASS_CAPACITY(I) = 0.
            R_CL_ANN_CLASS_ENERGY(I) = 0.
         ENDDO
!
! NOTE: 1 == PRODUCTION FROM RESOURCE TO MEET NATIVE LOAD
!       2 == DEFINED PURCHASE TO MEET NATIVE LOAD
!       3 == ECONOMY PURCHASE TO MEET NATIVE LOAD
!       4 == ECONOMY SALES WHICH INCREASE LOAD
!
         IF(R_CLASS <= MAX_CAP_LIMITED_CLASS_ID_NUM) THEN
            IF(R_CLASS ==0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN
               RETURN_CL_ASSET_CLASS_PROD = .TRUE.
               R_CL_ANN_CLASS_CAPACITY(1) =
     +                              CL_ANN_CLASS_CAPACITY(ASSET_CLASS,1)
               R_CL_ANN_CLASS_CAPACITY(2) =
     +                              CL_ANN_CLASS_CAPACITY(ASSET_CLASS,2)
               R_CL_ANN_CLASS_CAPACITY(3) =
     +                              CL_ANN_CLASS_CAPACITY(ASSET_CLASS,3)
               R_CL_ANN_CLASS_CAPACITY(4) =
     +                              CL_ANN_CLASS_CAPACITY(ASSET_CLASS,4)
!
               R_CL_ANN_CLASS_ENERGY(1) =
     +                                CL_ANN_CLASS_ENERGY(ASSET_CLASS,1)
               R_CL_ANN_CLASS_ENERGY(2) =
     +                                CL_ANN_CLASS_ENERGY(ASSET_CLASS,2)
               R_CL_ANN_CLASS_ENERGY(3) =
     +                                CL_ANN_CLASS_ENERGY(ASSET_CLASS,3)
               R_CL_ANN_CLASS_ENERGY(4) =
     +                                CL_ANN_CLASS_ENERGY(ASSET_CLASS,4)
            ENDIF
         ENDIF
!
      RETURN
!***********************************************************************
      ENTRY RETURN_CL_CLASS_TOTAL_PROD(R_CLASS,
     +                                    R_CL_ANN_TOTAL_CLASS_CAPACITY,
     +                                    R_CL_ANN_TOTAL_CLASS_ENERGY)
!***********************************************************************
         RETURN_CL_CLASS_TOTAL_PROD = .FALSE.
         R_CL_ANN_TOTAL_CLASS_CAPACITY = 0.
         R_CL_ANN_TOTAL_CLASS_ENERGY = 0.
!
! NOTE: 1 == PRODUCTION FROM RESOURCE TO MEET NATIVE LOAD
!       2 == DEFINED PURCHASE TO MEET NATIVE LOAD
!       3 == ECONOMY PURCHASE TO MEET NATIVE LOAD
!       4 == ECONOMY SALES WHICH INCREASE LOAD
!
         IF(R_CLASS <= MAX_CAP_LIMITED_CLASS_ID_NUM) THEN
            IF(R_CLASS ==0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN
               RETURN_CL_CLASS_TOTAL_PROD = .TRUE.
               R_CL_ANN_TOTAL_CLASS_CAPACITY =
     +                            CL_ANN_CLASS_CAPACITY(ASSET_CLASS,1) +
     +                            CL_ANN_CLASS_CAPACITY(ASSET_CLASS,2) +
     +                            CL_ANN_CLASS_CAPACITY(ASSET_CLASS,3)

               R_CL_ANN_TOTAL_CLASS_ENERGY =
     +                              CL_ANN_CLASS_ENERGY(ASSET_CLASS,1) +
     +                              CL_ANN_CLASS_ENERGY(ASSET_CLASS,2) +
     +                              CL_ANN_CLASS_ENERGY(ASSET_CLASS,3)

            ENDIF
         ENDIF
      RETURN
!***********************************************************************
      ENTRY RETURN_FE_PNL_EXPENSES(R_CLASS,
     +                             R_PNL_FUEL_COST,
     +                             R_PNL_PURCHASE_POWER_EXPENSE,
     +                             R_PNL_VARIABLE_EXPENSE,
     +                             R_PNL_FIXED_EXPENSE,
     +                             R_PNL_TOTAL_SALES_REVENUE,
     +                             R_PNL_CL_ANN_TOTAL_CLASS_ENERGY,
     +                             R_PNL_CL_ANN_CLASS_CAPACITY,
     +                             R_BAY_SHORE_FUEL_EXPENSE)
!***********************************************************************
         RETURN_FE_PNL_EXPENSES = .FALSE.
         IF(R_CLASS <= MAX_CAP_LIMITED_CLASS_ID_NUM) THEN
            IF(R_CLASS ==0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN
               RETURN_FE_PNL_EXPENSES = .TRUE.
!
!              NEED TO GET NUCLEAR FUEL AND EMISSIONS CREDITS ELSEWHERE
!
               R_BAY_SHORE_FUEL_EXPENSE =
     +                              FE_ANN_MULT_FUEL_COST(ASSET_CLASS,3)

               DO FT = 1, 6

                  R_PNL_FUEL_COST(FT) =
     +                             CL_ANN_MULT_FUEL_COST(ASSET_CLASS,FT)
!
                  R_PNL_VARIABLE_EXPENSE(FT) =
     +                              CL_ANN_MULT_VAR_COST(ASSET_CLASS,FT)

                  R_PNL_FIXED_EXPENSE(FT) =
     +                         CL_ANN_MULT_FIXED_COST(ASSET_CLASS,FT)
!
                  R_PNL_TOTAL_SALES_REVENUE(FT) =
     +                               CL_ANN_MULT_REVENUE(ASSET_CLASS,FT)
               ENDDO
!
               DO FT = 1, NUM_FUEL_CATEGORIES
                  R_PNL_CL_ANN_TOTAL_CLASS_ENERGY(FT) =
     +                          MON_MDS_CL_MULT_ENERGY(ASSET_CLASS,FT,0)

                  R_PNL_CL_ANN_CLASS_CAPACITY(FT) =
     +                        MON_MDS_CL_MULT_CAPACITY(ASSET_CLASS,FT,0) ! 02/04/04

                  R_PNL_PURCHASE_POWER_EXPENSE(FT) =
     +                       MON_MDS_CL_MULT_PURCHASES(ASSET_CLASS,FT,0)
                  IF(R_PNL_PURCHASE_POWER_EXPENSE(FT) > 0.1) THEN
                     TEMP_R4 = TEMP_R4
                  ENDIF

               ENDDO
!
            ENDIF
         ENDIF
!
      RETURN
!***********************************************************************
      ENTRY RETURN_AMEREN_CL_CLASS_EXPENSES(R_CLASS,
     +                                      R_TOTAL_SALES_REVENUE, ! SECONDARY_SALES_REVENUES,
     +                                      R_WHOLESALE_FUEL_EXPENSE,
     +                                      R_WHOLESALE_VOM_EXPENSE)
!***********************************************************************
         R_TOTAL_SALES_REVENUE = 0.
         R_WHOLESALE_FUEL_EXPENSE = 0.
         R_WHOLESALE_VOM_EXPENSE = 0.
         IF(R_CLASS <= MAX_CAP_LIMITED_CLASS_ID_NUM) THEN
            IF(R_CLASS ==0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN
               R_TOTAL_SALES_REVENUE =
     +                             CL_ANN_CLASS_REVENUE(ASSET_CLASS,1)
     +                             + CL_ANN_CLASS_REVENUE(ASSET_CLASS,2)
     +                             + CL_ANN_CLASS_REVENUE(ASSET_CLASS,3)
!
               TT = 10
               R_WHOLESALE_FUEL_EXPENSE =
     +                             CL_ANN_MULT_FUEL_COST(ASSET_CLASS,TT)
!
               R_WHOLESALE_VOM_EXPENSE =
     +                              CL_ANN_MULT_VAR_COST(ASSET_CLASS,TT)
            ENDIF
         ENDIF
         RETURN_AMEREN_CL_CLASS_EXPENSES = 1
      RETURN
!***********************************************************************
      ENTRY RETURN_CL_ASSET_CLASS_EXPENSES(R_CLASS,R_CLASS_EXISTS,
     +                                   R_FUEL_COST,
     +                                   R_PURCHASE_POWER_EXPENSE,
     +                                   R_VARIABLE_EXPENSE,
     +                                   R_FIXED_EXPENSE,
     +                                   R_EXPENSE_COLLECTED_ADJ_CLAUSE,
     +                                   R_EXPENSE_COLLECTED_BASE_RATES,
     +                                   R_NOT_COLLECTED_IN_RATES,
     +                                   R_BTL_SALES_REVENUE,
     +                                   R_TOTAL_SALES_REVENUE,
     +                                   R_BTL_EXPENSES,
     +                                   R_WHOLESALE_FUEL_EXPENSE,
     +                                   R_WHOLESALE_VOM_EXPENSE,
     +                                   R_ICAP_REVENUES,
     +                                   R_WVPA_EMISSIONS_EXPENSE,
     +                                 R_CAPACITY_SALES_TO_LEVEL_RM,    ! 726
     +                                 R_CAPACITY_PURCHASES_TO_LEVEL_RM)  ! 725
!***********************************************************************
         R_CLASS_EXISTS = .FALSE.
         IF(R_CLASS <= MAX_CAP_LIMITED_CLASS_ID_NUM) THEN
            IF(R_CLASS ==0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN
               R_CLASS_EXISTS = .TRUE.
               MO = 0
!
               DO I = 1, NUMBER_OF_EMISSION_TYPES
                  MON_MDS_CL_CLASS_EMISSIONS_COST(I,ASSET_CLASS,0) =
     +                 SUM(MON_MDS_CL_CLASS_EMISSIONS_COST(I,
     +                                                  ASSET_CLASS,1:))
               ENDDO
               IF(WVPA()) R_WVPA_EMISSIONS_EXPENSE =
     +             SUM(MON_MDS_CL_CLASS_EMISSIONS_COST(:,ASSET_CLASS,0))
!
               R_ICAP_REVENUES = R_ICAP_REVENUES
     +                        + MON_MDS_ICAP_REV_BY_CLASS(ASSET_CLASS,0)
!
               R_PURCHASE_POWER_EXPENSE = R_PURCHASE_POWER_EXPENSE
     +                    + MON_MDS_CL_CLASS_PURCHASES(ASSET_CLASS,1,MO)
     +                    + MON_MDS_CL_CLASS_PURCHASES(ASSET_CLASS,2,MO)
     +                    + MON_MDS_CL_CLASS_PURCHASES(ASSET_CLASS,3,MO)
               R_CAPACITY_PURCHASES_TO_LEVEL_RM =
     +                      MON_MDS_CL_CAP_PURCHASES(ASSET_CLASS,1,MO)
     +                    + MON_MDS_CL_CAP_PURCHASES(ASSET_CLASS,2,MO)
     +                    + MON_MDS_CL_CAP_PURCHASES(ASSET_CLASS,3,MO)

               R_FUEL_COST = R_FUEL_COST
     +                    + MON_MDS_CL_CLASS_FUEL_COST(ASSET_CLASS,1,MO)
     +                    + MON_MDS_CL_CLASS_FUEL_COST(ASSET_CLASS,2,MO)
     +                    + MON_MDS_CL_CLASS_FUEL_COST(ASSET_CLASS,3,MO)

               R_VARIABLE_EXPENSE = R_VARIABLE_EXPENSE
     +                     + MON_MDS_CL_CLASS_VAR_COST(ASSET_CLASS,1,MO)
     +                     + MON_MDS_CL_CLASS_VAR_COST(ASSET_CLASS,2,MO)
     +                     + MON_MDS_CL_CLASS_VAR_COST(ASSET_CLASS,3,MO)

               R_FIXED_EXPENSE = R_FIXED_EXPENSE
     +                   + MON_MDS_CL_CLASS_FIXED_COST(ASSET_CLASS,1,MO)
     +                   + MON_MDS_CL_CLASS_FIXED_COST(ASSET_CLASS,2,MO)
     +                   + MON_MDS_CL_CLASS_FIXED_COST(ASSET_CLASS,3,MO)

               R_EXPENSE_COLLECTED_ADJ_CLAUSE =
     +                    R_EXPENSE_COLLECTED_ADJ_CLAUSE
     +                    + MON_MDS_CL_CLASS_PURCHASES(ASSET_CLASS,1,MO)
     +                    + MON_MDS_CL_CAP_PURCHASES(ASSET_CLASS,1,MO)
     +                    + MON_MDS_NF_FUEL_LEASED_BC(ASSET_CLASS,1,MO)
     +                    + MON_MDS_NF_FUEL_OWNED_BC(ASSET_CLASS,1,MO)
     +                    + MON_MDS_CL_CLASS_FUEL_COST(ASSET_CLASS,1,MO)

               R_EXPENSE_COLLECTED_BASE_RATES =
     +                   R_EXPENSE_COLLECTED_BASE_RATES
     +                   + MON_MDS_CL_CLASS_PURCHASES(ASSET_CLASS,2,MO)
     +                   + MON_MDS_CL_CAP_PURCHASES(ASSET_CLASS,2,MO)
     +                   + MON_MDS_CL_CLASS_VAR_COST(ASSET_CLASS,2,MO)
     +                   + MON_MDS_CL_CLASS_FIXED_COST(ASSET_CLASS,2,MO)
     +                   + MON_MDS_NF_FUEL_LEASED_BC(ASSET_CLASS,2,MO)
     +                   + MON_MDS_NF_FUEL_OWNED_BC(ASSET_CLASS,2,MO)
     +                   + MON_MDS_CL_CLASS_FUEL_COST(ASSET_CLASS,2,MO)


               R_NOT_COLLECTED_IN_RATES = R_NOT_COLLECTED_IN_RATES
     +                   + MON_MDS_CL_CLASS_PURCHASES(ASSET_CLASS,3,MO)
     +                   + MON_MDS_CL_CAP_PURCHASES(ASSET_CLASS,3,MO)
     +                   + MON_MDS_CL_CLASS_VAR_COST(ASSET_CLASS,3,MO)
     +                   + MON_MDS_CL_CLASS_FIXED_COST(ASSET_CLASS,3,MO)
     +                   + MON_MDS_NF_FUEL_LEASED_BC(ASSET_CLASS,3,MO)
     +                   + MON_MDS_NF_FUEL_OWNED_BC(ASSET_CLASS,3,MO)
     +                   + MON_MDS_CL_CLASS_FUEL_COST(ASSET_CLASS,3,MO)

               R_TOTAL_SALES_REVENUE = R_TOTAL_SALES_REVENUE
     +                      + MON_MDS_CL_CLASS_REVENUE(ASSET_CLASS,1,MO)
     +                      + MON_MDS_CL_CLASS_REVENUE(ASSET_CLASS,2,MO)
     +                      + MON_MDS_CL_CLASS_REVENUE(ASSET_CLASS,3,MO)
               R_CAPACITY_SALES_TO_LEVEL_RM =
     +                        MON_MDS_CL_CAP_REVENUE(ASSET_CLASS,1,MO)
     +                      + MON_MDS_CL_CAP_REVENUE(ASSET_CLASS,2,MO)
     +                      + MON_MDS_CL_CAP_REVENUE(ASSET_CLASS,3,MO)

               R_SALES_REVENUE_NOT_IN_RATES =
     +                      R_SALES_REVENUE_NOT_IN_RATES
     +                      + MON_MDS_CL_CLASS_REVENUE(ASSET_CLASS,3,MO)
     +                      + MON_MDS_CL_CAP_REVENUE(ASSET_CLASS,3,MO)

               R_BTL_SALES_REVENUE = R_BTL_SALES_REVENUE
     +                      + MON_MDS_CL_CLASS_REVENUE(ASSET_CLASS,4,MO)
     +                      + MON_MDS_CL_CAP_REVENUE(ASSET_CLASS,4,MO)

               R_BTL_EXPENSES = R_BTL_EXPENSES
     +                   + MON_MDS_CL_CLASS_PURCHASES(ASSET_CLASS,4,MO)
     +                   + MON_MDS_CL_CAP_PURCHASES(ASSET_CLASS,4,MO)
     +                   + MON_MDS_CL_CLASS_VAR_COST(ASSET_CLASS,4,MO)
     +                   + MON_MDS_CL_CLASS_FIXED_COST(ASSET_CLASS,4,MO)
     +                   + MON_MDS_NF_FUEL_LEASED_BC(ASSET_CLASS,4,MO)
     +                   + MON_MDS_NF_FUEL_OWNED_BC(ASSET_CLASS,4,MO)
     +                   + MON_MDS_CL_CLASS_FUEL_COST(ASSET_CLASS,4,MO)

               TT = 10
               R_WHOLESALE_FUEL_EXPENSE =
     +                      MON_MDS_CL_MULT_FUEL_COST(ASSET_CLASS,TT,MO)

               R_WHOLESALE_VOM_EXPENSE =
     +                      MON_MDS_CL_MULT_VAR_COST(ASSET_CLASS,TT,MO)

           ENDIF
         ENDIF
         RETURN_CL_ASSET_CLASS_EXPENSES = 1
      RETURN
!***********************************************************************
      ENTRY RETURN_NUC_CL_ASSET_CLASS_EXPENSES(R_CLASS,R_CLASS_EXISTS,
     +                                   R_NUC_FUEL_OWNED,
     +                                   R_NUC_FUEL_LEASED,
     +                                   R_NF_BURN_IN_RATEBASE,
     +                                   R_NUC_FUEL_OWNED_BURN,
     +                                   R_NUC_FUEL_LEASED_BURN,
     +                                   R_DOE_NUC_FUEL_FEE,
     +                                   R_NUC_DECOMMISSIONING_COST,
     +                                   R_DOE_R300_DISPOSAL_COST)
!***********************************************************************
         R_CLASS_EXISTS = .FALSE.
         R_NUC_FUEL_OWNED_BURN = 0.
         R_NUC_FUEL_LEASED_BURN = 0.
         R_DOE_R300_DISPOSAL_COST = 0.
         IF(R_CLASS <= MAX_CAP_LIMITED_CLASS_ID_NUM) THEN
            IF(R_CLASS ==0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN
               R_CLASS_EXISTS = .TRUE.
               MO = 0
!
               R_NUC_FUEL_OWNED_BURN =
     +                      MON_MDS_NF_FUEL_OWNED_BC(ASSET_CLASS,1,MO)
     +                      + MON_MDS_NF_FUEL_OWNED_BC(ASSET_CLASS,2,MO)
     +                      + MON_MDS_NF_FUEL_OWNED_BC(ASSET_CLASS,3,MO)

               R_NUC_FUEL_LEASED_BURN =
     +                     MON_MDS_NF_FUEL_LEASED_BC(ASSET_CLASS,1,MO)
     +                     + MON_MDS_NF_FUEL_LEASED_BC(ASSET_CLASS,2,MO)
     +                     + MON_MDS_NF_FUEL_LEASED_BC(ASSET_CLASS,3,MO)

               MMBTU_ENERGY =
     +                    MON_MDS_NUCLEAR_MMBTU_BY_CLASS(ASSET_CLASS,MO)
               MWH_ENERGY =
     +                      MON_MDS_NUCLEAR_MWH_BY_CLASS(ASSET_CLASS,MO)
               R_DOE_NUC_FUEL_FEE = R_DOE_NUC_FUEL_FEE +
     +                     DOE_NF_DISPOSAL_COST(MMBTU_ENERGY,MWH_ENERGY)
               R_DOE_R300_DISPOSAL_COST =
     +                   DOE_R300_DISPOSAL_COST(MMBTU_ENERGY,MWH_ENERGY)
               R_NUC_DECOMMISSIONING_COST = R_NUC_DECOMMISSIONING_COST +
     +             NUCLEAR_DECOMMISSIONING_COST(MMBTU_ENERGY,MWH_ENERGY)
!
               R_NF_BURN_IN_RATEBASE = R_NF_BURN_IN_RATEBASE
     +                      + MON_MDS_NF_FUEL_OWNED_BC(ASSET_CLASS,1,MO)
     +                      + MON_MDS_NF_FUEL_OWNED_BC(ASSET_CLASS,2,MO)
     +                      + MON_MDS_NF_FUEL_OWNED_BC(ASSET_CLASS,3,MO)

               R_NUC_FUEL_LEASED = R_NUC_FUEL_LEASED
     +                     + MON_MDS_NF_FUEL_LEASED_BC(ASSET_CLASS,1,MO)
     +                     + MON_MDS_NF_FUEL_LEASED_BC(ASSET_CLASS,2,MO)
     +                     + MON_MDS_NF_FUEL_LEASED_BC(ASSET_CLASS,3,MO)

               R_NUC_FUEL_OWNED = R_NUC_FUEL_OWNED
     +                      + MON_MDS_NF_FUEL_OWNED_BC(ASSET_CLASS,1,MO)
     +                      + MON_MDS_NF_FUEL_OWNED_BC(ASSET_CLASS,2,MO)
     +                      + MON_MDS_NF_FUEL_OWNED_BC(ASSET_CLASS,3,MO)

           ENDIF
         ENDIF
         RETURN_NUC_CL_ASSET_CLASS_EXPENSES = 1
      RETURN
!***********************************************************************
      ENTRY RETURN_MONTHLY_CL_EXPENSES(R_CLASS,MONTH_VARS)
!***********************************************************************
!
         RETURN_MONTHLY_CL_EXPENSES = 1
         IF(.NOT. ALLOCATED(MON_MDS_NF_FUEL_OWNED_BC)) RETURN
         IF(R_CLASS <= MAX_CAP_LIMITED_CLASS_ID_NUM) THEN
            IF(R_CLASS ==0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN
!
               DO MO = 0, 12
                  IF(WVPA()) THEN
                     MONTH_VARS(MO,EmissionCredits) =
     +                      MONTH_VARS(MO,EmissionCredits)
     +                      + SUM(MON_MDS_CL_CLASS_EMISSIONS_COST(:,
     +                                                  ASSET_CLASS,MO))
                  ENDIF
                  MONTH_VARS(MO,OwnedNuclearFuel) =
     +                      MONTH_VARS(MO,OwnedNuclearFuel)
     +                      + MON_MDS_NF_FUEL_OWNED_BC(ASSET_CLASS,1,MO)
     +                      + MON_MDS_NF_FUEL_OWNED_BC(ASSET_CLASS,2,MO)
     +                      + MON_MDS_NF_FUEL_OWNED_BC(ASSET_CLASS,3,MO)
                  MONTH_VARS(MO,LeasedNuclearFuel) =
     +                     MONTH_VARS(MO,LeasedNuclearFuel)
     +                     + MON_MDS_NF_FUEL_LEASED_BC(ASSET_CLASS,1,MO)
     +                     + MON_MDS_NF_FUEL_LEASED_BC(ASSET_CLASS,2,MO)
     +                     + MON_MDS_NF_FUEL_LEASED_BC(ASSET_CLASS,3,MO)
                  MMBTU_ENERGY =
     +                    MON_MDS_NUCLEAR_MMBTU_BY_CLASS(ASSET_CLASS,MO)
                  MWH_ENERGY =
     +                      MON_MDS_NUCLEAR_MWH_BY_CLASS(ASSET_CLASS,MO)
! I ASSUME THAT THE FOLLOWING THREE VARIABLES ARE TIME INDEPENDENT. GAT. 5/26/98.
                  MONTH_VARS(MO,DOEDisposal) =
     +                   MONTH_VARS(MO,DOEDisposal) +
     +                   DOE_NF_DISPOSAL_COST(MMBTU_ENERGY,MWH_ENERGY) +
     +                   DOE_R300_DISPOSAL_COST(MMBTU_ENERGY,MWH_ENERGY)
                  MONTH_VARS(MO,DOEDecommissioning) =
     +             MONTH_VARS(MO,DOEDecommissioning) +
     +             NUCLEAR_DECOMMISSIONING_COST(MMBTU_ENERGY,MWH_ENERGY)
!
                  MONTH_VARS(MO,PurchasedPower) =
     +                    MONTH_VARS(MO,PurchasedPower)
     +                    + MON_MDS_CL_CLASS_PURCHASES(ASSET_CLASS,1,MO)
     +                    + MON_MDS_CL_CLASS_PURCHASES(ASSET_CLASS,2,MO)
     +                    + MON_MDS_CL_CLASS_PURCHASES(ASSET_CLASS,3,MO)
                  MONTH_VARS(MO,
     +               mty_xpns_rsv_mgn_cap_pchs) =
     +                    + MON_MDS_CL_CAP_PURCHASES(ASSET_CLASS,1,MO)
     +                    + MON_MDS_CL_CAP_PURCHASES(ASSET_CLASS,2,MO)
     +                    + MON_MDS_CL_CAP_PURCHASES(ASSET_CLASS,3,MO)
!
                  MONTH_VARS(MO,FossilFuel) =
     +                    MONTH_VARS(MO,FossilFuel)
     +                    + MON_MDS_CL_CLASS_FUEL_COST(ASSET_CLASS,1,MO)
     +                    + MON_MDS_CL_CLASS_FUEL_COST(ASSET_CLASS,2,MO)
     +                    + MON_MDS_CL_CLASS_FUEL_COST(ASSET_CLASS,3,MO)
!
                  MONTH_VARS(MO,VariableOandM) =
     +                     MONTH_VARS(MO,VariableOandM)
     +                     + MON_MDS_CL_CLASS_VAR_COST(ASSET_CLASS,1,MO)
     +                     + MON_MDS_CL_CLASS_VAR_COST(ASSET_CLASS,2,MO)
     +                     + MON_MDS_CL_CLASS_VAR_COST(ASSET_CLASS,3,MO)
!
                  MONTH_VARS(MO,FixedOandM) =
     +                   MONTH_VARS(MO,FixedOandM)
     +                   + MON_MDS_CL_CLASS_FIXED_COST(ASSET_CLASS,1,MO)
     +                   + MON_MDS_CL_CLASS_FIXED_COST(ASSET_CLASS,2,MO)
     +                   + MON_MDS_CL_CLASS_FIXED_COST(ASSET_CLASS,3,MO)
!
                  MONTH_VARS(MO,BTLExpenses) =
     +                   MONTH_VARS(MO,BTLExpenses)
     +                   + MON_MDS_CL_CLASS_PURCHASES(ASSET_CLASS,4,MO)
     +                   + MON_MDS_CL_CAP_PURCHASES(ASSET_CLASS,4,MO)
     +                   + MON_MDS_CL_CLASS_VAR_COST(ASSET_CLASS,4,MO)
     +                   + MON_MDS_CL_CLASS_FIXED_COST(ASSET_CLASS,4,MO)
     +                   + MON_MDS_NF_FUEL_LEASED_BC(ASSET_CLASS,4,MO)
     +                   + MON_MDS_NF_FUEL_OWNED_BC(ASSET_CLASS,4,MO)
     +                   + MON_MDS_CL_CLASS_FUEL_COST(ASSET_CLASS,4,MO)

               ENDDO
            ENDIF
         ENDIF
      RETURN
!***********************************************************************
      ENTRY RETURN_MONTHLY_NF_OP_EXPENSES(R_CLASS,
     +                                 R_MONTHLY_OWNED_NF)
!***********************************************************************
!
         RETURN_MONTHLY_NF_OP_EXPENSES = 1
         R_MONTHLY_OWNED_NF(:) = 0.
         IF(R_CLASS <= MAX_CAP_LIMITED_CLASS_ID_NUM .AND.
     +                         ALLOCATED(MON_MDS_NF_FUEL_OWNED_BC)) THEN
            IF(R_CLASS ==0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN
!

               R_MONTHLY_OWNED_NF(:) =
     +                       MON_MDS_NF_FUEL_OWNED_BC(ASSET_CLASS,1,:)
     +                       + MON_MDS_NF_FUEL_OWNED_BC(ASSET_CLASS,2,:)
     +                       + MON_MDS_NF_FUEL_OWNED_BC(ASSET_CLASS,3,:)

            ENDIF
         ENDIF
      RETURN
!***********************************************************************
      ENTRY RETURN_MONTHLY_CL_CASH_EXPENSES(R_CLASS,MONTH_VARS)
!***********************************************************************
!
         RETURN_MONTHLY_CL_CASH_EXPENSES = 1
         IF(.NOT. ALLOCATED(MON_MDS_NF_FUEL_LEASED_BC)) RETURN
         IF(R_CLASS <= MAX_CAP_LIMITED_CLASS_ID_NUM) THEN
            IF(R_CLASS ==0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN
!
               DO MO = 0, 12
                  IF(WVPA()) THEN
                     MONTH_VARS(MO,CashEmissionCredits) =
     +                      MONTH_VARS(MO,CashEmissionCredits)
     +                      + SUM(MON_MDS_CL_CLASS_EMISSIONS_COST(:,
     +                                                  ASSET_CLASS,MO))
                  ENDIF

                  MONTH_VARS(MO,Cash_Leased_Nuclear_Fuel) =
     +                    MONTH_VARS(MO,Cash_Leased_Nuclear_Fuel) +
     +                    MON_MDS_NF_FUEL_LEASED_BC(ASSET_CLASS,1,MO) +
     +                    MON_MDS_NF_FUEL_LEASED_BC(ASSET_CLASS,2,MO) +
     +                    MON_MDS_NF_FUEL_LEASED_BC(ASSET_CLASS,3,MO)
! I ASSUME THAT THE FOLLOWING THREE VARIABLES ARE TIME INDEPENDENT. GAT. 5/26/98.
                  MMBTU_ENERGY =
     +                    MON_MDS_NUCLEAR_MMBTU_BY_CLASS(ASSET_CLASS,MO)
                  MWH_ENERGY =
     +                      MON_MDS_NUCLEAR_MWH_BY_CLASS(ASSET_CLASS,MO)
                  MONTH_VARS(MO,CashDOEDisposal) =
     +                   MONTH_VARS(MO,CashDOEDisposal) +
     +                   DOE_NF_DISPOSAL_COST(MMBTU_ENERGY,MWH_ENERGY) +
     +                   DOE_R300_DISPOSAL_COST(MMBTU_ENERGY,MWH_ENERGY)
                  MONTH_VARS(MO,CashDOEDecommissioning) =
     +             MONTH_VARS(MO,CashDOEDecommissioning) +
     +             NUCLEAR_DECOMMISSIONING_COST(MMBTU_ENERGY,MWH_ENERGY)
!
                  MONTH_VARS(MO,Cash_Purchased_Power) =
     +                    MONTH_VARS(MO,Cash_Purchased_Power) +
     +                    MON_MDS_CL_CLASS_PURCHASES(ASSET_CLASS,1,MO) +
     +                    MON_MDS_CL_CLASS_PURCHASES(ASSET_CLASS,2,MO) +
     +                    MON_MDS_CL_CLASS_PURCHASES(ASSET_CLASS,3,MO)
                  MONTH_VARS(MO,
     +                  csh_expns_rsv_mgn_cap_purch) =
     +                MONTH_VARS(MO,
     +                    csh_expns_rsv_mgn_cap_purch)
     +                +    MON_MDS_CL_CAP_PURCHASES(ASSET_CLASS,1,MO)
     +                +    MON_MDS_CL_CAP_PURCHASES(ASSET_CLASS,2,MO)
     +                +    MON_MDS_CL_CAP_PURCHASES(ASSET_CLASS,3,MO)
!
                  MONTH_VARS(MO,Cash_Fossil_Fuel) =
     +                    MONTH_VARS(MO,Cash_Fossil_Fuel) +
     +                    MON_MDS_CL_CLASS_FUEL_COST(ASSET_CLASS,1,MO) +
     +                    MON_MDS_CL_CLASS_FUEL_COST(ASSET_CLASS,2,MO) +
     +                    MON_MDS_CL_CLASS_FUEL_COST(ASSET_CLASS,3,MO)
!
                  MONTH_VARS(MO,Cash_Variable_OandM) =
     +                     MONTH_VARS(MO,Cash_Variable_OandM) +
     +                     MON_MDS_CL_CLASS_VAR_COST(ASSET_CLASS,1,MO) +
     +                     MON_MDS_CL_CLASS_VAR_COST(ASSET_CLASS,2,MO) +
     +                     MON_MDS_CL_CLASS_VAR_COST(ASSET_CLASS,3,MO)
!
                  MONTH_VARS(MO,Cash_Fixed_OandM) =
     +                   MONTH_VARS(MO,Cash_Fixed_OandM) +
     +                   MON_MDS_CL_CLASS_FIXED_COST(ASSET_CLASS,1,MO) +
     +                   MON_MDS_CL_CLASS_FIXED_COST(ASSET_CLASS,2,MO) +
     +                   MON_MDS_CL_CLASS_FIXED_COST(ASSET_CLASS,3,MO)
                  MONTH_VARS(MO,CashBTLExpenses) =
     +                   MONTH_VARS(MO,CashBTLExpenses) +
     +                   MON_MDS_CL_CLASS_PURCHASES(ASSET_CLASS,4,MO) +
     +                   MON_MDS_CL_CAP_PURCHASES(ASSET_CLASS,4,MO) +
     +                   MON_MDS_CL_CLASS_VAR_COST(ASSET_CLASS,4,MO) +
     +                   MON_MDS_CL_CLASS_FIXED_COST(ASSET_CLASS,4,MO) +
     +                   MON_MDS_NF_FUEL_LEASED_BC(ASSET_CLASS,4,MO) +
     +                   MON_MDS_NF_FUEL_OWNED_BC(ASSET_CLASS,4,MO) +
     +                   MON_MDS_CL_CLASS_FUEL_COST(ASSET_CLASS,4,MO)
               ENDDO
            ENDIF
         ENDIF
      RETURN
!***********************************************************************
      ENTRY RETURN_MONTHLY_CL_REVENUES(R_CLASS,MONTH_VARS)
!***********************************************************************
!
         RETURN_MONTHLY_CL_REVENUES = 1
         IF(.NOT. ALLOCATED(MON_MDS_CL_CLASS_PURCHASES)) RETURN
         IF(R_CLASS <= MAX_CAP_LIMITED_CLASS_ID_NUM) THEN
            IF(R_CLASS ==0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN
!
               DO MO = 0, 12
                  MONTH_VARS(MO,AdjustmentClause) =
     +                    MONTH_VARS(MO,AdjustmentClause)
     +                    + MON_MDS_CL_CLASS_PURCHASES(ASSET_CLASS,1,MO)
     +                    + MON_MDS_CL_CAP_PURCHASES(ASSET_CLASS,1,MO)
     +                    + MON_MDS_NF_FUEL_LEASED_BC(ASSET_CLASS,1,MO)
     +                    + MON_MDS_NF_FUEL_OWNED_BC(ASSET_CLASS,1,MO)
     +                    + MON_MDS_CL_CLASS_FUEL_COST(ASSET_CLASS,1,MO)
                  MONTH_VARS(MO,SecondarySales) =
     +                      MONTH_VARS(MO,SecondarySales)
     +                      + MON_MDS_CL_CLASS_REVENUE(ASSET_CLASS,1,MO)
     +                      + MON_MDS_CL_CLASS_REVENUE(ASSET_CLASS,2,MO)
     +                      + MON_MDS_CL_CLASS_REVENUE(ASSET_CLASS,3,MO)
                  MONTH_VARS(MO,CapacitySales) =
     +                      MONTH_VARS(MO,CapacitySales)
     +                      + MON_MDS_CL_CAP_REVENUE(ASSET_CLASS,1,MO)
     +                      + MON_MDS_CL_CAP_REVENUE(ASSET_CLASS,2,MO)
     +                      + MON_MDS_CL_CAP_REVENUE(ASSET_CLASS,3,MO)
                  MONTH_VARS(MO,BTL_monthly_other_income) =
     +                      MONTH_VARS(MO,BTL_monthly_other_income)
     +                      + MON_MDS_CL_CLASS_REVENUE(ASSET_CLASS,4,MO)
     +                      + MON_MDS_CL_CAP_REVENUE(ASSET_CLASS,4,MO)
                  MONTH_VARS(MO,ICAP_Revs_mth) =
     +                       MONTH_VARS(MO,ICAP_Revs_mth)
     +                       + MON_MDS_ICAP_REV_BY_CLASS(ASSET_CLASS,MO)
               ENDDO
            ENDIF
         ENDIF
      RETURN
!***********************************************************************
      ENTRY RETURN_MONTHLY_CL_CASH_REVENUES(R_CLASS,MONTH_VARS)
!***********************************************************************
!
         RETURN_MONTHLY_CL_CASH_REVENUES = 1
         IF(.NOT. ALLOCATED(MON_MDS_CL_CLASS_PURCHASES)) RETURN
         IF(R_CLASS <= MAX_CAP_LIMITED_CLASS_ID_NUM) THEN
            IF(R_CLASS ==0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN
!
               DO MO = 0, 12
                  MONTH_VARS(MO,Cash_Adjustment_Clause) =
     +                    MONTH_VARS(MO,Cash_Adjustment_Clause) +
     +                    MON_MDS_CL_CLASS_PURCHASES(ASSET_CLASS,1,MO) +
     +                    MON_MDS_CL_CAP_PURCHASES(ASSET_CLASS,1,MO) +
     +                    MON_MDS_NF_FUEL_LEASED_BC(ASSET_CLASS,1,MO) +
     +                    MON_MDS_NF_FUEL_OWNED_BC(ASSET_CLASS,1,MO) +
     +                    MON_MDS_CL_CLASS_FUEL_COST(ASSET_CLASS,1,MO)
                  MONTH_VARS(MO,CashSecondarySales) =
     +                      MONTH_VARS(MO,CashSecondarySales) +
     +                      MON_MDS_CL_CLASS_REVENUE(ASSET_CLASS,1,MO) +
     +                      MON_MDS_CL_CLASS_REVENUE(ASSET_CLASS,2,MO) +
     +                      MON_MDS_CL_CLASS_REVENUE(ASSET_CLASS,3,MO)
                  MONTH_VARS(MO,
     +                      csh_income_rsv_mgn_cap_sales) =
     +                  MONTH_VARS(MO,
     +                      csh_income_rsv_mgn_cap_sales)
     +                  + MON_MDS_CL_CAP_REVENUE(ASSET_CLASS,1,MO)
     +                  + MON_MDS_CL_CAP_REVENUE(ASSET_CLASS,2,MO)
     +                  + MON_MDS_CL_CAP_REVENUE(ASSET_CLASS,3,MO)
                  MONTH_VARS(MO,CashBTLRevenues) =
     +                     MONTH_VARS(MO,CashBTLRevenues) +
     +                     MON_MDS_CL_CLASS_REVENUE(ASSET_CLASS,4,MO) +
     +                     MON_MDS_CL_CAP_REVENUE(ASSET_CLASS,4,MO)
                  MONTH_VARS(MO,CashICAPRevenues) =
     +                       MONTH_VARS(MO,CashICAPRevenues)
     +                       + MON_MDS_ICAP_REV_BY_CLASS(ASSET_CLASS,MO)
               ENDDO
            ENDIF
         ENDIF
      RETURN
!***********************************************************************
      ENTRY GET_CL_EMISS_FOR_CLASS(R_EMISS_TYPE,R_CLASS)
!***********************************************************************
         GET_CL_EMISS_FOR_CLASS = 0
         IF(R_CLASS <= MAX_CAP_LIMITED_CLASS_ID_NUM) THEN
            IF(R_CLASS ==0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN
               GET_CL_EMISS_FOR_CLASS =
     +               CL_ANN_CLASS_EMISSIONS(R_EMISS_TYPE,ASSET_CLASS)
            ENDIF
         ENDIF
      RETURN
!***********************************************************************
      ENTRY GET_CL_EMISSIONS(R_CLASS,R_CLASS_CL_EMISSIONS)
!***********************************************************************
         R_CLASS_CL_EMISSIONS(1:NUMBER_OF_EMISSION_TYPES) = 0.
         IF(R_CLASS <= MAX_CAP_LIMITED_CLASS_ID_NUM) THEN
            IF(R_CLASS ==0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN
               R_CLASS_CL_EMISSIONS(1:NUMBER_OF_EMISSION_TYPES) =
     +               CL_ANN_CLASS_EMISSIONS(1:NUMBER_OF_EMISSION_TYPES,
     +                                                      ASSET_CLASS)
            ENDIF
         ENDIF
         GET_CL_EMISSIONS = R_CLASS_CL_EMISSIONS(1)
      RETURN
!***********************************************************************
      ENTRY RETURN_CL_INTRA_CLASS_REVENUES(R_CLASS,
     +                                     R_INTRA_BASE_REVENUES,
     +                                     R_INTRA_ADJ_REVENUES,
     +                                     R_INTRA_SALES_REVENUES,
     +                                     R_INTRA_OTHER_REVENUES,
     +                                     R_INTRA_COMPANY_NF_BURN)
!***********************************************************************
!
         R_INTRA_COMPANY_NF_BURN = 0.
         IF(R_CLASS <= MAX_INTRA_CLASS) THEN
            RETURN_CL_INTRA_CLASS_REVENUES = .TRUE.
            R_INTRA_BASE_REVENUES = R_INTRA_BASE_REVENUES +
     +                        INTRA_COMPANY_REVENUES(R_CLASS,1)/1000000.
            R_INTRA_ADJ_REVENUES = R_INTRA_ADJ_REVENUES +
     +                        INTRA_COMPANY_REVENUES(R_CLASS,2)/1000000.
            R_INTRA_SALES_REVENUES = R_INTRA_SALES_REVENUES +
     +                        INTRA_COMPANY_REVENUES(R_CLASS,3)/1000000.
            R_INTRA_OTHER_REVENUES = R_INTRA_OTHER_REVENUES +
     +                        INTRA_COMPANY_REVENUES(R_CLASS,0)/1000000.
            R_INTRA_COMPANY_NF_BURN =
     +                           INTRA_COMPANY_NF_BURN(R_CLASS)/1000000.

         ELSE
            RETURN_CL_INTRA_CLASS_REVENUES = .FALSE.
         ENDIF
      RETURN
!***********************************************************************
      ENTRY RETURN_CL_INTRA_EXPENSES(R_INTRA_FOSSIL_FUEL_EXPENSES,
     +                               R_INTRA_LEASED_NUCLEAR_FUEL,
     +                               R_INTRA_OWNED_NUC_FUEL_EXPENSES,
     +                               R_INTRA_PURCHASE_EXPENSES,
     +                               R_INTRA_NUC_OWN_BURN,
     +                               R_INTRA_NUC_LEASE_BURN)
!***********************************************************************
!
         RETURN_CL_INTRA_EXPENSES = .TRUE.
         R_INTRA_FOSSIL_FUEL_EXPENSES = R_INTRA_FOSSIL_FUEL_EXPENSES +
     +                               INTRA_FOSSIL_FUEL_EXPENSES/1000000.
         R_INTRA_LEASED_NUCLEAR_FUEL = R_INTRA_LEASED_NUCLEAR_FUEL +
     +                           INTRA_LEASED_NUC_FUEL_EXPENSES/1000000.
         R_INTRA_OWNED_NUC_FUEL_EXPENSES =
     +                            R_INTRA_OWNED_NUC_FUEL_EXPENSES +
     +                            INTRA_OWNED_NUC_FUEL_EXPENSES/1000000.
         R_INTRA_NUC_OWN_BURN = R_INTRA_NUC_OWN_BURN +
     +                                       INTRA_NUC_OWN_BURN/1000000.
         R_INTRA_NUC_OWN_BURN = R_INTRA_NUC_OWN_BURN +
     +                                       INTRA_NUC_OWN_BURN/1000000.
         R_INTRA_NUC_LEASE_BURN = R_INTRA_NUC_LEASE_BURN +
     +                                     INTRA_NUC_LEASE_BURN/1000000.
         R_INTRA_PURCHASE_EXPENSES = R_INTRA_PURCHASE_EXPENSES
     +                               + INTRA_PURCHASE_EXPENSES/1000000.
      RETURN
      ENTRY GET_BLOCK_CAP_FACTORS(R_LOWER_BOUND_CAP_FACTOR,
     +                            R_UPPER_BOUND_CAP_FACTOR,R_NBLOK2,
     +                            R_FACET_MAINTENANCE_RATE,
     +                            R_FUEL_INVENTORY_ID,
     +                            R_MMBTU_FUEL_BALANCE,
     +                            R_MAINTENANCE_RATE)
         DO I = 1, R_NBLOK2
!
            UNITNO = UNIT(I)
            R_LOWER_BOUND_CAP_FACTOR(I) =
     +                                   MINIMUM_CAPACITY_FACTOR(UNITNO)
            R_UPPER_BOUND_CAP_FACTOR(I) =
     +                                   MAXIMUM_CAPACITY_FACTOR(UNITNO)
!
         ENDDO
         DO UNITNO = 1, NUNITS
            IF(SHADOW_UNIT_NUMBER(UNITNO) /= 0) THEN
               FUEL_ID = R_FUEL_INVENTORY_ID(FUEL_SUPPLY_ID(UNITNO))
               IF(UNITNO > SHADOW_UNIT_NUMBER(UNITNO) .AND.
     +                          R_MMBTU_FUEL_BALANCE(FUEL_ID) > 0.) THEN
                  R_FACET_MAINTENANCE_RATE(UNITNO) = 1.0
               ELSEIF(UNITNO < SHADOW_UNIT_NUMBER(UNITNO) .AND.
     +                      R_MMBTU_FUEL_BALANCE(FUEL_ID) <= 0. D0) THEN
                  R_FACET_MAINTENANCE_RATE(UNITNO) = 1.0
               ELSE
                  R_FACET_MAINTENANCE_RATE(UNITNO) =
     +                                        R_MAINTENANCE_RATE(UNITNO)
               ENDIF
            ELSE
               R_FACET_MAINTENANCE_RATE(UNITNO) =
     +                                        R_MAINTENANCE_RATE(UNITNO)
            ENDIF
         ENDDO
         GET_BLOCK_CAP_FACTORS = .TRUE.
      RETURN
!***********************************************************************
      ENTRY CLA_SPECIAL_ID_NAME(R_NUNITS,R_NAME)
!***********************************************************************
         R_NAME = SPECIAL_UNIT_ID(R_NUNITS)
         CLA_SPECIAL_ID_NAME = .TRUE.
      RETURN
!***********************************************************************
      ENTRY CLA_RETURN_UNITNM(R_NUNITS,R_NAME) ! character 20 used in tf_objt, fuelused, clreport
!***********************************************************************
         R_NAME = UNITNM(R_NUNITS)
         CLA_RETURN_UNITNM = .TRUE.
      RETURN
      END function cl_units_read
!***********************************************************************
      REAL FUNCTION GET_CL_POOL_FRAC_OWN(R_NUNITS)
!***********************************************************************
      USE CL_UNITS_READ_DATA
      INTEGER (KIND=2) :: R_NUNITS
         GET_CL_POOL_FRAC_OWN = LOCAL_CL_POOL_FRAC_OWN(R_NUNITS)
      END FUNCTION

!***********************************************************************
      FUNCTION SPECIAL_ID_NAME(R_NUNITS)  ! character 20 used in margnobj and catawba
!***********************************************************************
!
      CHARACTER (len=20) ::  SPECIAL_ID_NAME,RETURN_UNITNM,R_NAME
      LOGICAL (kind=1) ::  VOID_LOGICAL
      LOGICAL (kind=1) ::  CLA_SPECIAL_ID_NAME,CLA_RETURN_UNITNM
      INTEGER (kind=2) ::  R_NUNITS
         VOID_LOGICAL = CLA_SPECIAL_ID_NAME(R_NUNITS,R_NAME)  ! character 20 used in margnobj and catawba
         SPECIAL_ID_NAME = R_NAME
      RETURN
!***********************************************************************
      ENTRY RETURN_UNITNM(R_NUNITS) ! character 20 used in tf_objt, fuelused, clreport
!***********************************************************************
         VOID_LOGICAL = CLA_RETURN_UNITNM(R_NUNITS,R_NAME)
         RETURN_UNITNM = R_NAME
      RETURN
      END
!***********************************************************************
      FUNCTION CL_UNIQUE_RPT_STR(R_UNIT)
!***********************************************************************
!
         CHARACTER (len=5) ::  CL_UNIQUE_RPT_STR,RPT_STR
         INTEGER (kind=2) ::  R_UNIT,CL_NUM
         INTEGER (kind=4) ::  UNIQUE_REPORT_VALUE_FOR_CL_UNIT
         IF(UNIQUE_REPORT_VALUE_FOR_CL_UNIT(R_UNIT) < 32000) THEN
            CL_NUM = UNIQUE_REPORT_VALUE_FOR_CL_UNIT(R_UNIT)
         ELSE
            CL_NUM = 32000
         ENDIF
         IF(CL_NUM <= 0) THEN
            RPT_STR = ' '
         ELSE
            WRITE(RPT_STR,'(I5)') CL_NUM
         ENDIF
         CL_UNIQUE_RPT_STR = RPT_STR
      RETURN
      END


!***********************************************************************
      FUNCTION CAP_MARKET_MONTH_NO_INDEX(R_UNIT_STR)
!***********************************************************************
      INTEGER (kind=2) ::  CAP_MARKET_MONTH_NO_INDEX
      CHARACTER (len=5) ::  R_UNIT_STR
! END DATA DECLARATIONS
         CAP_MARKET_MONTH_NO_INDEX = 1
         SELECT CASE(trim(R_UNIT_STR))
            CASE ('JAN')
               CAP_MARKET_MONTH_NO_INDEX = 1
            CASE ('FEB')
               CAP_MARKET_MONTH_NO_INDEX = 2
            CASE ('MAR')
               CAP_MARKET_MONTH_NO_INDEX = 3
            CASE ('APR')
               CAP_MARKET_MONTH_NO_INDEX = 4
            CASE ('MAY')
               CAP_MARKET_MONTH_NO_INDEX = 5
            CASE ('JUN')
               CAP_MARKET_MONTH_NO_INDEX = 6
            CASE ('JUL')
               CAP_MARKET_MONTH_NO_INDEX = 7
            CASE ('AUG')
               CAP_MARKET_MONTH_NO_INDEX = 8
            CASE ('SEP')
               CAP_MARKET_MONTH_NO_INDEX = 9
            CASE ('OCT')
               CAP_MARKET_MONTH_NO_INDEX = 10
            CASE ('NOT')
               CAP_MARKET_MONTH_NO_INDEX = 11
            CASE ('DEC')
               CAP_MARKET_MONTH_NO_INDEX = 12
            CASE ('MONTH')
               CAP_MARKET_MONTH_NO_INDEX = 13
         END SELECT
      RETURN
      END
!***********************************************************************

! Primary_mover_index_db routine moved to prim_mover_idx module.c-----
      subroutine CheckO3P(c_0,C_1,C_2,C_3,x0,x1)
! Greg:  feel free to rename this as desired
!     determines whether cubic curve y=c0+c1*x+c2*x^2+c3*x^3 is monotonically
!     increasing (i.e., has positive 2nd derivative at the left endpoint x0);
!     if not, it linearizes the curve between the endpoints x0,x1.  The
!     curve's 1st derivative is s(x)=c1+2*c2*x+3*c3*x^2; the
!     curve's 2nd derivative is t(x)=2*c2+6*c3*x.  Note that for the curves
!     of interest here (heat input as y versus MW load output as x), we check
!     for the concave-upward property only at the left (minimal-load) point.
!     Attempts at simplifying the curve by merely zeroing c3, or by assigning
!     c3 to the value at which t0==0, both failed to approximate the curve.
!     notes by AGT on 20030728
      real (kind=4) ::  x0,x1,y0,y1,s0,s1,t0,C_0,C_1,C_2,C_3
      logical (kind=1) ::  MonotoneIncr
!
      s0=c_1+x0*(2.0*c_2+x0*3.0*c_3)
      s1=c_1+x1*(2.0*c_2+x1*3.0*c_3)
      t0=2.0*c_2+6.0*c_3*x0
      MonotoneIncr=(s0>0.0).and.(s1>0.0).and.(t0>0.0)
      if(.not.MonotoneIncr .and. x1 > x0) then ! linearize y(x) on the domain [x0,x1]
        y0=c_0+x0*(c_1+x0*(c_2+x0*c_3))
        y1=c_0+x1*(c_1+x1*(c_2+x1*c_3))
        c_1=(y1-y0)/(x1-x0)
        c_0= y1-c_1*x1
        c_2=0.0
        c_3=0.0
      end if
      RETURN
      end subroutine CheckO3P
!***********************************************************************
!
      FUNCTION CL_CAPACITY_PLANNING_ADJ(R_YEAR,R_UNIT_NO,
     +                                          ST,SUBTRACT_IT)
     +                        RESULT(R_CL_CAPACITY_PLANNING_ADJ)
      use end_routine, only: end_program, er_message
      use logging
!***************************************************************
!
      use SpinDriftLib
      use prod_arrays_dimensions
      USE CLA_OBJT_ARRAYS
      use p_fuel
      USE SIZECOM
      use SpinDriftLib
      use prod_arrays_dimensions
      use globecom
      use foshydi2com
      use aitvaprod
      use prod2com
      use prodcom
      SAVE

!
      CHARACTER (LEN=2) :: GREEN_MRX_METHOD
      REAL (KIND=4) :: R_CL_CAPACITY_PLANNING_ADJ
      REAL (KIND=4) :: UNIT_CAPACITY,CAP_MW,VOID_REAL4,R_POINTR,
     +                 MONTH_MULT,FIRST_YEAR_CAPACITY
      INTEGER (KIND=2) :: UNIT_NO,R_UNIT_NO,YEAR_START,YEAR_END,
     +  R_YEAR,
     +                    ON_LINE_MONTH,TG,FT,
     +                    UNIT_OFF_YEAR,PT_YR,
     +                    LOCAL_POINTER,NG
      LOGICAL (KIND=4) :: SUBTRACT_IT
      CHARACTER (LEN=20) :: MONTH_NAME,LOCAL_NAME
      LOGICAL (kind=1) ::  RESURRECT_RETROFIT_UNIT
      INTEGER (KIND=4) :: ST
      INTEGER (KIND=2) , PARAMETER :: MAX_NEWGEN_INDEX = 10

      INTEGER (KIND=2) :: PEAK_MONTH,GET_TRANS_GROUP_POSITION,
     +                    GET_NUMBER_OF_ACTIVE_GROUPS
      REAL (KIND=4) :: GET_CL_MW_FROM_POINTR,
     +                 GET_CLASS_PEAK_NET_DSM_FOR_CAP,
     +                 PLAN_FACTOR,GET_VAR
!
! BEGINNING OF THE CAPACITY PLANNING SECTION

      ON_LINE_MONTH = ONLINE(R_UNIT_NO) -
     +                                   100*INT(ONLINE(R_UNIT_NO)/100.)
      IF(.NOT. SUBTRACT_IT) THEN
         YEAR_START = R_YEAR - BASE_YEAR
         IF(YEAR_START < 1) THEN
            YEAR_START = 1
            ON_LINE_MONTH = 1
         ENDIF

            IF(ON_LINE_MONTH > PEAK_MONTH(YEAR_START)) THEN
               WRITE(4,*) 'The on-line month, ',
     +                    TRIM(MONTH_NAME(ON_LINE_MONTH)),
     +                    ', for CL unit, ',TRIM(UNITNM(R_UNIT_NO))
               WRITE(4,"('&',A,I4,A)") ' is after the peak month in ',
     +                     R_YEAR,' of '//
     +           TRIM(MONTH_NAME(PEAK_MONTH(YEAR_START)))//'.'
               WRITE(4,*) "The capacity of this unit is not in ",
     +                    "the planning capacity for its on-line year."
               WRITE(4,*)


               TG = TRANSACTION_GROUP_ID(R_UNIT_NO)
               TG = GET_TRANS_GROUP_POSITION(TG)
               FT= PRIMARY_MOVER(R_UNIT_NO)
               UNIT_CAPACITY = INPUT_MW(2,R_UNIT_NO)
               CAP_MW = UNIT_CAPACITY
               PT_YR = 1
               UNIT_NO = R_UNIT_NO
               IF(CAP_MW < 0.) THEN
                  CAP_MW = GET_CL_MW_FROM_POINTR(CAP_MW,PT_YR)
                  IF(CAP_MW < 0.)
     +                CAP_MW =
     +                   GET_CL_MW_FROM_POINTR(CAP_MW,PEAK_MONTH(PT_YR))
                  CAP_MW = MAX(0.,CAP_MW)
               ELSE
                  CAP_MW = MAX(0.,CAP_MW)
               ENDIF

               IF(MONTHLY_CAPACITY_POINTER(UNIT_NO) /= 0 .AND.
     +                           .NOT. CL_CAP_AREA_LINKED(UNIT_NO)) THEN
                  R_POINTR =
     +                     FLOAT(ABS(MONTHLY_CAPACITY_POINTER(UNIT_NO)))
                  MONTH_MULT = GET_CL_MW_FROM_POINTR(R_POINTR,
     +                                           PEAK_MONTH(PT_YR))
                  IF(MONTH_MULT > 2. .AND. CAP_MW /= 0.) THEN
                     CAP_MW = MONTH_MULT
                  ELSE
                     CAP_MW = MONTH_MULT * CAP_MW
                  ENDIF
               ELSEIF(MONTHLY_CAPACITY_POINTER(UNIT_NO) < 0 .AND.
     +                                 CL_CAP_AREA_LINKED(UNIT_NO)) THEN
                  LOCAL_NAME = UNITNM(UNIT_NO)
                  LOCAL_POINTER = ABS(MONTHLY_CAPACITY_POINTER(UNIT_NO))
                  CAP_MW = MIN(GET_CLASS_PEAK_NET_DSM_FOR_CAP(
     +                           PT_YR,LOCAL_NAME,LOCAL_POINTER),CAP_MW)
               ENDIF
               IF(foshyd.CAP_PLANNING_FAC(R_UNIT_NO) < 0.) THEN
                PLAN_FACTOR=GET_VAR(foshyd.CAP_PLANNING_FAC(R_UNIT_NO),
     +                                           PT_YR,
     +                                           UNITNM(R_UNIT_NO))
               ELSE
                  PLAN_FACTOR = foshyd.CAP_PLANNING_FAC(R_UNIT_NO)
               ENDIF
               CAP_MW = CAP_MW * foshyd.CAP_FRAC_OWN(UNIT_NO) *
     +                                              PLAN_FACTOR * 0.01

               IF (FT > 0 .AND. FT < 7) THEN
                  CL_TG_AFTER_PEAK(TG,YEAR_START) =      ! MODIFIED 11/13/06 BY DR.G
     +                        CL_TG_AFTER_PEAK(TG,YEAR_START) + CAP_MW
                  CL_TG_AFTER_PEAK(0,YEAR_START) =
     +                        CL_TG_AFTER_PEAK(0,YEAR_START) + CAP_MW
               ENDIF
               YEAR_START = YEAR_START + 1
            ENDIF

         IF(OFF_LINE_YEAR(R_UNIT_NO) > LAST_STUDY_YEAR) THEN
            YEAR_END = LAST_STUDY_YEAR - BASE_YEAR
         ELSE
            YEAR_END = OFF_LINE_YEAR(R_UNIT_NO) - BASE_YEAR
            IF(YEAR_END < 1 .OR. R_UNIT_NO < 1 .OR. R_UNIT_NO
     +                                              > MAX_CL_UNITS) THEN
               WRITE(4,*) "The unit ",UNITNM(R_UNIT_NO)
               WRITE(4,*) "has an off-line year of ",
     +                                          OFF_LINE_YEAR(R_UNIT_NO)
               WRITE(4,*) "while the base year is ",BASE_YEAR
               WRITE(4,*) "First, check to make sure that the "
               WRITE(4,*) "Unit Production pointers in your "
               WRITE(4,*) "Capacity Options file are valid."
               WRITE(4,*) "If running Transact, make sure that the"
               WRITE(4,*) "resource options in your Expansion Plant"
               WRITE(4,*) "Operations file all have active"
               WRITE(4,*) "Transact Groups."
               WRITE(4,*) '*** line 3318 CLA_OBJT.FOR ***'
               er_message='See WARNING MESSAGES-cla_objt-2'
               call end_program(er_message)
            ENDIF
            IF(OFF_LINE_MONTH(R_UNIT_NO) < PEAK_MONTH(YEAR_END)) THEN
               WRITE(4,*) 'The off-line month, ',
     +                    TRIM(MONTH_NAME(OFF_LINE_MONTH(R_UNIT_NO))),
     +                    ', for CL unit, ',TRIM(UNITNM(R_UNIT_NO))
               WRITE(4,"('&',A,I4,A)")', is before the peak month in ',
     +                     OFF_LINE_YEAR(R_UNIT_NO),' of '//
     +          TRIM(MONTH_NAME(PEAK_MONTH(OFF_LINE_YEAR(R_UNIT_NO))))//
     +                                                               '.'
               WRITE(4,*) "The capacity of this unit is not in ",
     +                   "the planning capacity for it's off-line year."
               WRITE(4,*)
               YEAR_END = YEAR_END - 1
            ENDIF
         ENDIF
         UNIT_CAPACITY = INPUT_MW(2,R_UNIT_NO)
         UNIT_OFF_YEAR = OFF_LINE_YEAR(R_UNIT_NO) - BASE_YEAR

         UNIT_NO = R_UNIT_NO ! 4/11/95. GAT.
      ELSE !   GOTO 10
         UNIT_NO = R_UNIT_NO
         UNIT_CAPACITY = INPUT_MW(2,UNIT_NO)
         YEAR_START = R_YEAR - BASE_YEAR
         YEAR_END=MIN(LAST_STUDY_YEAR,OFF_LINE_YEAR(UNIT_NO))-BASE_YEAR
         IF(.NOT. GREEN_MRX_METHOD() == 'GX')
     +         OFLINE(UNIT_NO)=100*(R_YEAR-1900)+OFF_LINE_MONTH(UNIT_NO)
      ENDIF ! 10 CONTINUE
         DO PT_YR = YEAR_START, YEAR_END

               CAP_MW = UNIT_CAPACITY                 ! TMS 7/15/05 - move out of loop since loop doesnt execute for units retiring in first year


            IF(CAP_MW < 0.) THEN
               CAP_MW = GET_CL_MW_FROM_POINTR(CAP_MW,PT_YR)
! ADDED PER JONES. 3/13/98. GAT.
               IF(CAP_MW < 0.)
     +                CAP_MW =
     +                   GET_CL_MW_FROM_POINTR(CAP_MW,PEAK_MONTH(PT_YR))
!
               CAP_MW = MAX(0.,CAP_MW)
            ELSE
!
! IF NO POINTER HAS BEEN SPECIFIED FOR THE PEAK MONTH
!
               CAP_MW = MAX(0.,CAP_MW)
            ENDIF

            IF(MONTHLY_CAPACITY_POINTER(UNIT_NO) /= 0 .AND.
     +                           .NOT. CL_CAP_AREA_LINKED(UNIT_NO)) THEN
               R_POINTR = FLOAT(ABS(MONTHLY_CAPACITY_POINTER(UNIT_NO)))
               MONTH_MULT = GET_CL_MW_FROM_POINTR(R_POINTR,
     +                                           PEAK_MONTH(PT_YR))
               IF(MONTH_MULT > 2. .AND. CAP_MW /= 0.) THEN
                  CAP_MW = MONTH_MULT
               ELSE
                  CAP_MW = MONTH_MULT * CAP_MW
               ENDIF
            ELSEIF(MONTHLY_CAPACITY_POINTER(UNIT_NO) < 0 .AND.
     +                                 CL_CAP_AREA_LINKED(UNIT_NO)) THEN
               LOCAL_NAME = UNITNM(UNIT_NO)
               LOCAL_POINTER = ABS(MONTHLY_CAPACITY_POINTER(UNIT_NO))
               CAP_MW = MIN(GET_CLASS_PEAK_NET_DSM_FOR_CAP(
     +                           PT_YR,LOCAL_NAME,LOCAL_POINTER),CAP_MW)
            ENDIF
!
! EFFECTIVE MW'S OF PLANNING CAPACITY
!
            IF(foshyd.CAP_PLANNING_FAC(R_UNIT_NO) < 0.) THEN
              PLAN_FACTOR = GET_VAR(foshyd.CAP_PLANNING_FAC(R_UNIT_NO),
     +                                                PT_YR,
     +                                                UNITNM(R_UNIT_NO))
            ELSE
               PLAN_FACTOR = foshyd.CAP_PLANNING_FAC(R_UNIT_NO)
            ENDIF
            CAP_MW = CAP_MW * foshyd.CAP_FRAC_OWN(UNIT_NO) *
     +                                              PLAN_FACTOR * 0.01

            IF(SUBTRACT_IT) CAP_MW = -CAP_MW
!
! THIS SECTION ADDS MW'S OF PLANNING CAPACITY
!
            IF(LDTYPE(UNIT_NO)=='M' .OR.
     +              LDTYPE(UNIT_NO)=='B' .OR. LDTYPE(UNIT_NO)=='N') THEN
               CL_ANN_CAP(1,PT_YR,ST) = CL_ANN_CAP(1,PT_YR,ST) + CAP_MW
               CL_ANN_CAP(2,PT_YR,ST) = CL_ANN_CAP(2,PT_YR,ST) + CAP_MW
               CL_ANN_CAP(3,PT_YR,ST) = CL_ANN_CAP(3,PT_YR,ST) + CAP_MW
            ELSE IF(LDTYPE(UNIT_NO) == 'C') THEN
               CL_ANN_CAP(2,PT_YR,ST) = CL_ANN_CAP(2,PT_YR,ST) + CAP_MW
               CL_ANN_CAP(3,PT_YR,ST) = CL_ANN_CAP(3,PT_YR,ST) + CAP_MW
            ELSE IF(LDTYPE(UNIT_NO) == 'D') THEN
               CL_ANNUAL_LOAD_REDUCTION(PT_YR) =
     +                        CL_ANNUAL_LOAD_REDUCTION(PT_YR) + CAP_MW
            ELSE
               CL_ANN_CAP(3,PT_YR,ST) = CL_ANN_CAP(3,PT_YR,ST) + CAP_MW
            ENDIF
! 01/11/02. FOR NEW TG RESERVE MARGIN REPORT.
            TG=TRANSACTION_GROUP_ID(UNIT_NO)
            TG = GET_TRANS_GROUP_POSITION(TG)
            FT= PRIMARY_MOVER(UNIT_NO)
            NG= NEWGEN_INDEX(UNIT_NO)
            IF(TG < 1 .OR. TG > GET_NUMBER_OF_ACTIVE_GROUPS()) THEN
               WRITE(4,*) "BAD TRANSACTION GROUP ID IN THERMAL UNITS",
     +                                                   UNITNM(UNIT_NO)
            ENDIF
            IF(NG > 10) NG = 1 ! 070609. PER DOUG.
            IF (FT > 0 .AND. FT < 7) THEN
               CL_TG_CAP(FT,TG,PT_YR,ST) =
     +                                CL_TG_CAP(FT,TG,PT_YR,ST) + CAP_MW
               CL_TG_CAP(0,TG,PT_YR,ST) =
     +                                 CL_TG_CAP(0,TG,PT_YR,ST) + CAP_MW
            ELSEIF(FT > 6) THEN ! 110807 FOR PAC: .OR. FT < 1) THEN
               CL_TG_CAP(6,TG,PT_YR,ST) =
     +                                CL_TG_CAP(6,TG,PT_YR,ST) + CAP_MW
               CL_TG_CAP(0,TG,PT_YR,ST) =
     +                                 CL_TG_CAP(0,TG,PT_YR,ST) + CAP_MW
            ENDIF
            IF (NG > 0 .AND. NG < MAX_NEWGEN_INDEX) THEN
               NEWGEN_CAP_BY_INDEX(NG,TG,PT_YR) =
     +                         NEWGEN_CAP_BY_INDEX(NG,TG,PT_YR) + CAP_MW
            ENDIF
!
            IF(PT_YR == YEAR_START) FIRST_YEAR_CAPACITY = ABS(CAP_MW)
         ENDDO
         IF(  (UNIT_OFF_YEAR <= STUDY_PERIOD .AND.
     +         OFF_LINE_MONTH(UNIT_NO) < PEAK_MONTH(UNIT_OFF_YEAR)) .OR.
     +         (UNIT_OFF_YEAR+1 <= STUDY_PERIOD .AND.
     +             OFF_LINE_MONTH(UNIT_NO) >
     +                                  PEAK_MONTH(UNIT_OFF_YEAR))) THEN
            IF (FT > 0 .AND. FT < 10) THEN ! 7) THEN
               IF(OFF_LINE_MONTH(UNIT_NO) >
     +                                   PEAK_MONTH(UNIT_OFF_YEAR)) THEN
                  CL_TG_RETIRE(TG,UNIT_OFF_YEAR+1) =
     +                         CL_TG_RETIRE(TG,UNIT_OFF_YEAR+1) + CAP_MW
                  CL_TG_RETIRE(0,UNIT_OFF_YEAR+1) =
     +                          CL_TG_RETIRE(0,UNIT_OFF_YEAR+1) + CAP_MW
               ELSE
                  CL_TG_RETIRE(TG,UNIT_OFF_YEAR) =
     +                           CL_TG_RETIRE(TG,UNIT_OFF_YEAR) + CAP_MW
                  CL_TG_RETIRE(0,UNIT_OFF_YEAR) =
     +                            CL_TG_RETIRE(0,UNIT_OFF_YEAR) + CAP_MW
               ENDIF
            ENDIF
         ENDIF
         R_CL_CAPACITY_PLANNING_ADJ = FIRST_YEAR_CAPACITY
      RETURN
!***************************************************************
      ENTRY RESURRECT_RETROFIT_UNIT(R_YEAR,R_UNIT_NO)
!***************************************************************
!
! IMPLICITLY ASSUMES 25 YEAR LIFE
!
         OFLINE(R_UNIT_NO) = 100*(R_YEAR + 25 - 1900) +
     +                                         OFF_LINE_MONTH(R_UNIT_NO)
         ONLINE(R_UNIT_NO) = 100*INT(ONLINE(R_UNIT_NO)/100.) +
     +            CO2_CONTROL_DATE(R_UNIT_NO) -
     +                      100*INT(CO2_CONTROL_DATE(R_UNIT_NO)/100.)
         RESURRECT_RETROFIT_UNIT = .TRUE.
      RETURN
      END
!
!
