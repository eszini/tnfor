!
!     ******************************************************************
!     CN_OBJT.FOR
!     Copyright(c) M.S.Gerber & Associates 2000
!
!     Created: 7/7/2003 2:05:02 PM
!     Author : GREG TURK
!     Last change: msg 2/14/2022 4:57:23 PM
! .... (see repository) .....
!            MSG 2/23/2015 2:23:46 PM
!     ******************************************************************

      SUBROUTINE CN_OBJECT
      use end_routine, only: end_program, er_message
      use SpinDriftLib
      use prod_arrays_dimensions
      USE SIZECOM
      use globecom
!
      CHARACTER (len=1) ::  YES_OR_NO_FILLER,RECORD_ACTIVE
      INTEGER (kind=2) ::  NUMBER_OF_BC_CLASSES=0 , &
                MAX_BC_CLASS_ID_NUM=0
      INTEGER (kind=2) ::  NUMBER_OF_OL_CLASSES=0 , &
                MAX_OL_CLASS_ID_NUM=0
      INTEGER (kind=2) ::  OPTIONS_BC_ASSET_CLASS_POINTER(:), &
                OPTIONS_OL_ASSET_CLASS_POINTER(:), &
                TEMP_ASSET_CLASS_POINTER(:)
      ALLOCATABLE :: OPTIONS_BC_ASSET_CLASS_POINTER, &
                     OPTIONS_OL_ASSET_CLASS_POINTER, &
                     TEMP_ASSET_CLASS_POINTER
      SAVE OPTIONS_BC_ASSET_CLASS_POINTER, &
           OPTIONS_OL_ASSET_CLASS_POINTER
      INTEGER (kind=2) ::  R_NUM_OF_OPTIONS_CLASSES
      INTEGER (kind=2) ::  R_MAX_OPTIONS_CLASS_NUM, &
                R_OPTIONS_CLASS_POINTERS(*),GRX_RESOURCE_LINK_ID
      INTEGER (kind=4) ::  IOS,IOS_BASE
!
      INTEGER (kind=2) ::  IREC,INUNIT,DELETE,LRECL=193 ,I
      INTEGER ::  UNIT_NO
      CHARACTER (len=5) ::  CAP_STACK_FILE,OVERLAY_FAMILY_NAME
      CHARACTER (len=10) ::  RESOURCE_TYPE
      CHARACTER (LEN=40) :: OVN_CAPITAL_COST_STOCASTIC
      CHARACTER (len=30) ::  COMMENT
      CHARACTER (len=256) ::  FILE_NAME
      CHARACTER (len=256) ::  BASE_FILE_DIRECTORY,OUTPUT_DIRECTORY
      LOGICAL (kind=4) ::  FILE_EXISTS
! DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER (len=1024) ::  RECLN
! DECLARATIONS OF CAPACITY STACK FILE
      INTEGER (kind=2) ::  FIRST_YEAR_AVAILABLE,LAST_YEAR_AVAILABLE, &
         OPERATION_LIFE,LEAD_TIME,ANNUAL_UNITS,PRODUCTION_DATA_POINTER, &
         INVESTMENT_DATA_POINTER,CUMULATIVE_UNITS,ESCALATION_VECTOR, &
! 1/12/93 ADDED TO CAPACITY OPTION & !  1/12/93 ADDED TO CAPACITY OPTIONS
         MUTUALLY_EXCLUSIVE_UNIT, &
         POINTER_TO_DEPENDENT_UNIT, &
         HARD_WIRED_YEAR, &
         RESOURCE_ID_NUM,RESOURCE_GROUP_NUM, &
         MAX_RESOURCE_ID,TOTAL_ALL_OPTIONS, &
! 11/2/94 ADDED DECISON TABLE POINTERS AND ADDITIONAL INVESTMEN & !  11/2/94 ADDED DECISON TABLE POINTERS AND ADDITIONAL INVESTMENT
         ADDITIONAL_INVESTMENT_POINTER,OVER_CAPACITY_TABLE, &
         UNDER_CAPACITY_TABLE
! 4/18/95
      REAL ::  UNIT_CAP,MAX_UNIT_CAPACITY,MRX_RISK_ADJUSTMENT
      REAL ::  CONSTRUCTION_COSTS,CON_COST_PER_KW,CCR_FOR_SCREENING, &
               CON_COST_PER_MWH, &
               RENEWABLE_ENERGY_PERCENT
      CHARACTER (len=20) ::  UNIT_NAME,LOAD_TYPE*1
      CHARACTER (len=25) ::  FILE_TYPE='Expansion-Candidate Units'
      CHARACTER (len=2) ::  CAP_STACK_OL='BC'
      CHARACTER (len=2) ::  FILE_SOURCE,R_CAP_STACK_OL
      INTEGER (kind=2) ::  BASE_OPTIONS,CYCLE_OPTIONS,PEAK_OPTIONS, &
                LOAD_OPTIONS,HARD_WIRED_OPTIONS,FILL_OPTIONS
      INTEGER (kind=2) ::  TOTAL_ACTIVE_OPTIONS,STORE_BCPL_OPTIONS, &
                STORE_BC_BCPL_OPTIONS, &
                PRIMARY_ASSET_CLASS,PRIMARY_ALLOCATION_VECTOR, &
                ADDITIONAL_ASSET_CLASS,ADDITIONAL_ALLOCATION_VECTOR
      LOGICAL (kind=1) ::  LAHEY_LF95
      CHARACTER (len=50) ::  SCREEN_OUTPUT

!***********************************************************************
!
!          ROUTINE TO CONVERT METAFILE FILES TO DIRECT-ACESS BINARY
!          COPYRIGHT (C) 1983, 84, 85  M.S. GERBER & ASSOCIATES, INC.
!
!***********************************************************************
!
! CONVERT THE EXPANSION-CANDIDATE FILE
      ENTRY CN_MAKEBIN
!
      FILE_NAME = trim(BASE_FILE_DIRECTORY())// &
                                 "CNB"//trim(CAP_STACK_FILE())//".DAT"
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      BASE_OPTIONS = 0
      CONSTRUCTION_COSTS = 0.
      CYCLE_OPTIONS = 0
      PEAK_OPTIONS = 0
      LOAD_OPTIONS = 0
      FILL_OPTIONS = 0
      HARD_WIRED_OPTIONS = 0
      MAX_RESOURCE_ID = 0
      TOTAL_ALL_OPTIONS = 0
      ADDITIONAL_INVESTMENT_POINTER = 0
      PRIMARY_ASSET_CLASS = 0
      PRIMARY_ALLOCATION_VECTOR = 0
      ADDITIONAL_ASSET_CLASS = 0
      ADDITIONAL_ALLOCATION_VECTOR = 0
      OVER_CAPACITY_TABLE = 1
      UNDER_CAPACITY_TABLE = 1
      CCR_FOR_SCREENING = .17
      MRX_RISK_ADJUSTMENT = 0.0
      RECORD_ACTIVE = 'T'
      IF(FILE_EXISTS) THEN
         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//CAP_STACK_FILE()
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
            CALL MG_LOCATE_WRITE(16,30,CAP_STACK_FILE(),ALL_VERSIONS,0)
         ENDIF
         ALLOCATE(TEMP_ASSET_CLASS_POINTER(1024))
         TEMP_ASSET_CLASS_POINTER = 0
         OPEN(10,FILE=FILE_NAME)
         OPEN(11,FILE=trim(OUTPUT_DIRECTORY())//"BCCNWCP.BIN", &
                            ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
         IREC = 1
         READ(10,*) DELETE
         DO
            MUTUALLY_EXCLUSIVE_UNIT = 0
            POINTER_TO_DEPENDENT_UNIT = 0
            HARD_WIRED_YEAR = 0
            CON_COST_PER_KW = 0.0
            CON_COST_PER_MWH = 0.0
            RENEWABLE_ENERGY_PERCENT = 0.0
            FILE_SOURCE = 'CL'
            RESOURCE_ID_NUM = -9999
            MAX_UNIT_CAPACITY = -9999.
            RESOURCE_GROUP_NUM = 0
            YES_OR_NO_FILLER = 'N'
            RESOURCE_TYPE = 'Expansion'
            OVN_CAPITAL_COST_STOCASTIC = "Not Active"
            GRX_RESOURCE_LINK_ID = 0
            PRODUCTION_DATA_POINTER = 0
            DO
               READ(10,1000,IOSTAT=IOS) RECLN
               IF(IOS /= 0) EXIT
               IF(RECLN(1:1) == '7') EXIT
               RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,'
               READ(RECLN,*,IOSTAT=IOS) DELETE,UNIT_NAME,LOAD_TYPE, &
                  UNIT_CAP,CONSTRUCTION_COSTS,ESCALATION_VECTOR, &
                  FIRST_YEAR_AVAILABLE,LAST_YEAR_AVAILABLE, &
                  OPERATION_LIFE,LEAD_TIME,ANNUAL_UNITS, &
                  CUMULATIVE_UNITS,PRODUCTION_DATA_POINTER, &
                  INVESTMENT_DATA_POINTER,COMMENT, &
                  MUTUALLY_EXCLUSIVE_UNIT,HARD_WIRED_YEAR, &
                  CON_COST_PER_KW,FILE_SOURCE,RESOURCE_ID_NUM, &
                  RESOURCE_GROUP_NUM,ADDITIONAL_INVESTMENT_POINTER, &
                  OVER_CAPACITY_TABLE,UNDER_CAPACITY_TABLE, &
                  PRIMARY_ASSET_CLASS,PRIMARY_ALLOCATION_VECTOR, &
                  ADDITIONAL_ASSET_CLASS,ADDITIONAL_ALLOCATION_VECTOR, &
                  MAX_UNIT_CAPACITY,CCR_FOR_SCREENING,YES_OR_NO_FILLER, &
                  RECORD_ACTIVE,POINTER_TO_DEPENDENT_UNIT, &
                  MRX_RISK_ADJUSTMENT,RESOURCE_TYPE, &
                  OVN_CAPITAL_COST_STOCASTIC, & !  35
                  GRX_RESOURCE_LINK_ID,CON_COST_PER_MWH, &
                  RENEWABLE_ENERGY_PERCENT
               IF(IOS /= 0) THEN
!                  CALL LOCATE(20,0)
!                  WRITE(6,1010) trim(RECLN)
                  CALL MG_LOCATE_WRITE(20,0,trim(RECLN), &
                                                         ALL_VERSIONS,0)
                  er_message='stop requested from CN_OBJT SIID34'
                  call end_program(er_message)
               ENDIF
!
             CALL SET_ASSET_CLASSES(PRIMARY_ASSET_CLASS, &
                                      NUMBER_OF_BC_CLASSES, &
                                       MAX_BC_CLASS_ID_NUM, &
                                      TEMP_ASSET_CLASS_POINTER)
             CALL SET_ASSET_CLASSES(ADDITIONAL_ASSET_CLASS, &
                                      NUMBER_OF_BC_CLASSES, &
                                       MAX_BC_CLASS_ID_NUM, &
                                      TEMP_ASSET_CLASS_POINTER)
!
               IF(RESOURCE_ID_NUM == -9999) RESOURCE_ID_NUM = IREC
               IF(MAX_UNIT_CAPACITY == -9999.)MAX_UNIT_CAPACITY=UNIT_CAP ! 4/18/95
               WRITE(11,REC=IREC) DELETE,UNIT_NAME,LOAD_TYPE, &
                  UNIT_CAP,CONSTRUCTION_COSTS,ESCALATION_VECTOR, &
                  FIRST_YEAR_AVAILABLE,LAST_YEAR_AVAILABLE, &
                  OPERATION_LIFE,LEAD_TIME,ANNUAL_UNITS, &
                  CUMULATIVE_UNITS, &
                  PRODUCTION_DATA_POINTER,INVESTMENT_DATA_POINTER, &
                  MUTUALLY_EXCLUSIVE_UNIT,HARD_WIRED_YEAR, &
                  CON_COST_PER_KW, &
                  FILE_SOURCE,RESOURCE_ID_NUM,RESOURCE_GROUP_NUM, &
                  ADDITIONAL_INVESTMENT_POINTER,OVER_CAPACITY_TABLE, &
                  UNDER_CAPACITY_TABLE, &
                  PRIMARY_ASSET_CLASS,PRIMARY_ALLOCATION_VECTOR, &
                  ADDITIONAL_ASSET_CLASS,ADDITIONAL_ALLOCATION_VECTOR, &
                  MAX_UNIT_CAPACITY,CCR_FOR_SCREENING,YES_OR_NO_FILLER, &
                  RECORD_ACTIVE,POINTER_TO_DEPENDENT_UNIT, &
                  MRX_RISK_ADJUSTMENT,RESOURCE_TYPE , &
                  OVN_CAPITAL_COST_STOCASTIC, & !  35
                  GRX_RESOURCE_LINK_ID,CON_COST_PER_MWH, &
                  RENEWABLE_ENERGY_PERCENT
               IREC = IREC + 1
               TOTAL_ALL_OPTIONS =TOTAL_ALL_OPTIONS + 1
               IF(DELETE > 7 .OR. &
                     RECORD_ACTIVE == 'F' .OR. &
               ((FIRST_YEAR_AVAILABLE > LAST_STUDY_YEAR .OR. &
                    LAST_YEAR_AVAILABLE <= BASE_YEAR .OR. &
                     ANNUAL_UNITS == 0 .OR. CUMULATIVE_UNITS  == 0  .OR. & !  3/30/95. GAT.
                     LAST_YEAR_AVAILABLE < FIRST_YEAR_AVAILABLE) .AND. &
                          HARD_WIRED_YEAR-LEAD_TIME <= BASE_YEAR)) CYCLE
!
               MAX_RESOURCE_ID = MAX(MAX_RESOURCE_ID, &
                                  ABS(RESOURCE_ID_NUM), &
                                  ABS(MUTUALLY_EXCLUSIVE_UNIT))
!
               IF(HARD_WIRED_YEAR > BASE_YEAR) THEN
                  IF(HARD_WIRED_YEAR >= BASE_YEAR+LEAD_TIME .AND. &
                      HARD_WIRED_YEAR-LEAD_TIME <= LAST_STUDY_YEAR) THEN
                     HARD_WIRED_OPTIONS = HARD_WIRED_OPTIONS + 1
                  ENDIF
               ELSEIF(YES_OR_NO_FILLER == 'Y') THEN
                  FILL_OPTIONS = FILL_OPTIONS + 1
               ELSEIF(LOAD_TYPE == 'P') THEN
                  PEAK_OPTIONS = PEAK_OPTIONS + 1
               ELSEIF(LOAD_TYPE == 'C') THEN
                  CYCLE_OPTIONS = CYCLE_OPTIONS + 1
               ELSEIF(LOAD_TYPE == 'L') THEN
                  LOAD_OPTIONS = LOAD_OPTIONS + 1
               ELSE
                  BASE_OPTIONS = BASE_OPTIONS + 1
               ENDIF
            ENDDO
            IF(IOS /= 0) EXIT
         ENDDO
         TOTAL_ACTIVE_OPTIONS = STORE_BCPL_OPTIONS(BASE_OPTIONS, &
                                                   CYCLE_OPTIONS, &
                                                   PEAK_OPTIONS, &
                                                   LOAD_OPTIONS, &
                                                   FILL_OPTIONS, &
                                                   HARD_WIRED_OPTIONS, &
                                                   MAX_RESOURCE_ID, &
                                                   TOTAL_ALL_OPTIONS)
         TOTAL_ACTIVE_OPTIONS = STORE_BC_BCPL_OPTIONS()
         CLOSE(10)
!         ENDFILE(11)
         CLOSE(11)
         IF(MAX_BC_CLASS_ID_NUM > 0) THEN
            ALLOCATE(OPTIONS_BC_ASSET_CLASS_POINTER (MAX_BC_CLASS_ID_NUM))
            DO I = 1, MAX_BC_CLASS_ID_NUM
               OPTIONS_BC_ASSET_CLASS_POINTER(I) = TEMP_ASSET_CLASS_POINTER(I)
            ENDDO
!
!
!
         ENDIF
         DEALLOCATE(TEMP_ASSET_CLASS_POINTER)
      ELSE IF(INDEX(CAP_STACK_FILE(),'NONE') == 0) THEN
         CALL STOP_NOFILE(FILE_TYPE,FILE_NAME)
      ENDIF
      RETURN

!***********************************************************************
!
!          ROUTINE TO CREATE OVERLAY FILES
!          COPYRIGHT (C) 1984-88  M.S. GERBER & ASSOCIATES, INC.
!          COPYRIGHT (C) 1991-92  M.S. GERBER & ASSOCIATES, INC.
!
!***********************************************************************
!
! OVERLAY THE EXPANSION-CANDIDATE FILE
      ENTRY CN_MAKEOVL(OVERLAY_FAMILY_NAME)
!
      IF(LAHEY_LF95()) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSE
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         CALL LOCATE(10,51)
      ENDIF
      FILE_NAME = trim(OUTPUT_DIRECTORY())//"CNO"// &
                                     trim(OVERLAY_FAMILY_NAME)//".DAT"
      OPEN(10,FILE=FILE_NAME)
      READ(10,*) DELETE
      INUNIT = 12
      IF(CAP_STACK_OL == 'BC') THEN
         OPEN(11,FILE=trim(OUTPUT_DIRECTORY())//"BCCNWCP.BIN",ACCESS="DIRECT",RECL=LRECL)
         INUNIT = 11
      ENDIF
      OPEN(12,FILE=trim(OUTPUT_DIRECTORY())//"OLCNWCP.BIN",ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
      ALLOCATE(TEMP_ASSET_CLASS_POINTER(1024))
      TEMP_ASSET_CLASS_POINTER = 0
      IREC = 0
      BASE_OPTIONS = 0
      CYCLE_OPTIONS = 0
      PEAK_OPTIONS = 0
      LOAD_OPTIONS = 0
      FILL_OPTIONS = 0
      HARD_WIRED_OPTIONS = 0
      MAX_RESOURCE_ID = 0
      TOTAL_ALL_OPTIONS = 0
      DO
         DO
            READ(10,1000,IOSTAT=IOS) RECLN
            IF(RECLN(1:1) == '7') EXIT
            IREC = IREC + 1
            READ(INUNIT,REC=IREC,IOSTAT=IOS_BASE) DELETE,UNIT_NAME, &
               LOAD_TYPE,UNIT_CAP,CONSTRUCTION_COSTS,ESCALATION_VECTOR, &
               FIRST_YEAR_AVAILABLE,LAST_YEAR_AVAILABLE,OPERATION_LIFE, &
               LEAD_TIME,ANNUAL_UNITS,CUMULATIVE_UNITS, &
               PRODUCTION_DATA_POINTER,INVESTMENT_DATA_POINTER, &
               MUTUALLY_EXCLUSIVE_UNIT,HARD_WIRED_YEAR, &
               CON_COST_PER_KW,FILE_SOURCE, &
               RESOURCE_ID_NUM,RESOURCE_GROUP_NUM, &
               ADDITIONAL_INVESTMENT_POINTER,OVER_CAPACITY_TABLE, &
               UNDER_CAPACITY_TABLE, &
               PRIMARY_ASSET_CLASS,PRIMARY_ALLOCATION_VECTOR, &
               ADDITIONAL_ASSET_CLASS,ADDITIONAL_ALLOCATION_VECTOR, &
               MAX_UNIT_CAPACITY,CCR_FOR_SCREENING,YES_OR_NO_FILLER, &
               RECORD_ACTIVE,POINTER_TO_DEPENDENT_UNIT, &
               MRX_RISK_ADJUSTMENT,RESOURCE_TYPE , &
               OVN_CAPITAL_COST_STOCASTIC, & !  35
               GRX_RESOURCE_LINK_ID,CON_COST_PER_MWH, &
               RENEWABLE_ENERGY_PERCENT
            IF(IOS_BASE /= 0) EXIT
            IF(IOS == 0) THEN
               RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               READ(RECLN,*,IOSTAT=IOS) DELETE,UNIT_NAME,LOAD_TYPE, &
                  UNIT_CAP,CONSTRUCTION_COSTS,ESCALATION_VECTOR, &
                  FIRST_YEAR_AVAILABLE,LAST_YEAR_AVAILABLE, &
                  OPERATION_LIFE,LEAD_TIME,ANNUAL_UNITS, &
                  CUMULATIVE_UNITS,PRODUCTION_DATA_POINTER, &
                  INVESTMENT_DATA_POINTER,COMMENT, &
                  MUTUALLY_EXCLUSIVE_UNIT,HARD_WIRED_YEAR, &
                  CON_COST_PER_KW,FILE_SOURCE, &
                  RESOURCE_ID_NUM,RESOURCE_GROUP_NUM, &
                  ADDITIONAL_INVESTMENT_POINTER,OVER_CAPACITY_TABLE, &
                  UNDER_CAPACITY_TABLE, &
                  PRIMARY_ASSET_CLASS,PRIMARY_ALLOCATION_VECTOR, &
                  ADDITIONAL_ASSET_CLASS,ADDITIONAL_ALLOCATION_VECTOR, &
                  MAX_UNIT_CAPACITY,CCR_FOR_SCREENING,YES_OR_NO_FILLER, &
                  RECORD_ACTIVE,POINTER_TO_DEPENDENT_UNIT, &
                  MRX_RISK_ADJUSTMENT,RESOURCE_TYPE , &
                  OVN_CAPITAL_COST_STOCASTIC, & !  35
                  GRX_RESOURCE_LINK_ID,CON_COST_PER_MWH, &
                  RENEWABLE_ENERGY_PERCENT
               IF(IOS /= 0) THEN

                  CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,0)
                  er_message='stop requested from CN_OBJT SIID35'
                  call end_program(er_message)
               ENDIF
            ENDIF
          CALL SET_ASSET_CLASSES(PRIMARY_ASSET_CLASS, &
                                   NUMBER_OF_OL_CLASSES, &
                                    MAX_OL_CLASS_ID_NUM, &
                                   TEMP_ASSET_CLASS_POINTER)
          CALL SET_ASSET_CLASSES(ADDITIONAL_ASSET_CLASS, &
                                   NUMBER_OF_OL_CLASSES, &
                                    MAX_OL_CLASS_ID_NUM, &
                                   TEMP_ASSET_CLASS_POINTER)
            WRITE(12,REC=IREC) DELETE,UNIT_NAME,LOAD_TYPE, &
               UNIT_CAP,CONSTRUCTION_COSTS,ESCALATION_VECTOR, &
               FIRST_YEAR_AVAILABLE,LAST_YEAR_AVAILABLE,OPERATION_LIFE, &
               LEAD_TIME,ANNUAL_UNITS,CUMULATIVE_UNITS, &
               PRODUCTION_DATA_POINTER,INVESTMENT_DATA_POINTER, &
               MUTUALLY_EXCLUSIVE_UNIT,HARD_WIRED_YEAR, &
               CON_COST_PER_KW,FILE_SOURCE, &
               RESOURCE_ID_NUM,RESOURCE_GROUP_NUM, &
               ADDITIONAL_INVESTMENT_POINTER,OVER_CAPACITY_TABLE, &
               UNDER_CAPACITY_TABLE, &
               PRIMARY_ASSET_CLASS,PRIMARY_ALLOCATION_VECTOR, &
               ADDITIONAL_ASSET_CLASS,ADDITIONAL_ALLOCATION_VECTOR, &
               MAX_UNIT_CAPACITY,CCR_FOR_SCREENING,YES_OR_NO_FILLER, &
               RECORD_ACTIVE,POINTER_TO_DEPENDENT_UNIT, &
               MRX_RISK_ADJUSTMENT,RESOURCE_TYPE , &
               OVN_CAPITAL_COST_STOCASTIC, & !  35
               GRX_RESOURCE_LINK_ID,CON_COST_PER_MWH, &
               RENEWABLE_ENERGY_PERCENT
!
            TOTAL_ALL_OPTIONS = TOTAL_ALL_OPTIONS + 1
            IF(DELETE > 7 .OR. &
                     RECORD_ACTIVE == 'F' .OR. &
                ((FIRST_YEAR_AVAILABLE > LAST_STUDY_YEAR .OR. &
                    LAST_YEAR_AVAILABLE <= BASE_YEAR .OR. &
                     ANNUAL_UNITS == 0 .OR. CUMULATIVE_UNITS  == 0  .OR. & !  3/30/95. GAT.
                      LAST_YEAR_AVAILABLE < FIRST_YEAR_AVAILABLE) .AND. &
                          HARD_WIRED_YEAR-LEAD_TIME <= BASE_YEAR)) CYCLE
!
            MAX_RESOURCE_ID = MAX(MAX_RESOURCE_ID,ABS(RESOURCE_ID_NUM),ABS(MUTUALLY_EXCLUSIVE_UNIT))
!
            IF(HARD_WIRED_YEAR > BASE_YEAR) THEN
               IF(HARD_WIRED_YEAR >= BASE_YEAR+LEAD_TIME .AND. &
                      HARD_WIRED_YEAR-LEAD_TIME <= LAST_STUDY_YEAR) THEN
                  HARD_WIRED_OPTIONS = HARD_WIRED_OPTIONS + 1
               ENDIF
            ELSEIF(YES_OR_NO_FILLER == 'Y') THEN
               FILL_OPTIONS = FILL_OPTIONS + 1
            ELSEIF(LOAD_TYPE == 'P') THEN
               PEAK_OPTIONS = PEAK_OPTIONS + 1
            ELSEIF(LOAD_TYPE == 'C') THEN
               CYCLE_OPTIONS = CYCLE_OPTIONS + 1
            ELSEIF(LOAD_TYPE == 'L') THEN
               LOAD_OPTIONS = LOAD_OPTIONS + 1
            ELSE
               BASE_OPTIONS = BASE_OPTIONS + 1
            ENDIF
         ENDDO
         IF(IOS_BASE /= 0) EXIT
      ENDDO
      TOTAL_ACTIVE_OPTIONS = STORE_BCPL_OPTIONS(BASE_OPTIONS, &
                                                CYCLE_OPTIONS, &
                                                PEAK_OPTIONS, &
                                                LOAD_OPTIONS, &
                                                FILL_OPTIONS, &
                                                HARD_WIRED_OPTIONS, &
                                                MAX_RESOURCE_ID, &
                                                TOTAL_ALL_OPTIONS)
      CLOSE(10)
!      ENDFILE(12)
      CLOSE(12)
      IF(CAP_STACK_OL == 'BC') CLOSE(11)
      CAP_STACK_OL = 'OL'
      IF(ALLOCATED(OPTIONS_OL_ASSET_CLASS_POINTER)) DEALLOCATE(OPTIONS_OL_ASSET_CLASS_POINTER)
      IF(MAX_OL_CLASS_ID_NUM > 0) THEN
         ALLOCATE(OPTIONS_OL_ASSET_CLASS_POINTER (MAX_OL_CLASS_ID_NUM))
         DO I = 1, MAX_OL_CLASS_ID_NUM
            OPTIONS_OL_ASSET_CLASS_POINTER(I) = TEMP_ASSET_CLASS_POINTER(I)
         ENDDO

      ENDIF
      DEALLOCATE(TEMP_ASSET_CLASS_POINTER)
      RETURN
!
!***********************************************************************
      ENTRY GET_CAP_STACK_OL(R_CAP_STACK_OL)
!***********************************************************************
         R_CAP_STACK_OL = CAP_STACK_OL
      RETURN
!
!***********************************************************************
      ENTRY OPEN_CAPACITY_OPTIONS_FILE(UNIT_NO)
!***********************************************************************
         OPEN(UNIT_NO,FILE=trim(OUTPUT_DIRECTORY())//CAP_STACK_OL//"CNWCP.BIN",ACCESS="DIRECT",RECL=LRECL)
      RETURN
!
!***********************************************************************
      ENTRY RESET_CAP_STACK_OL
!***********************************************************************
         CAP_STACK_OL = 'BC'
      RETURN
!***********************************************************************
      ENTRY RETURN_NUM_OF_OPTIONS_CLASSES(R_NUM_OF_OPTIONS_CLASSES,R_MAX_OPTIONS_CLASS_NUM)
!***********************************************************************
         IF(CAP_STACK_OL == 'OL') THEN
            R_NUM_OF_OPTIONS_CLASSES = NUMBER_OF_OL_CLASSES
            R_MAX_OPTIONS_CLASS_NUM = MAX_OL_CLASS_ID_NUM
         ELSE
            R_NUM_OF_OPTIONS_CLASSES = NUMBER_OF_BC_CLASSES
            R_MAX_OPTIONS_CLASS_NUM = MAX_BC_CLASS_ID_NUM
         ENDIF
      RETURN
!***********************************************************************
      ENTRY RETURN_OPTIONS_CLASS_POINTER(R_OPTIONS_CLASS_POINTERS)
!***********************************************************************
         IF(CAP_STACK_OL == 'OL') THEN
            DO I = 1, MAX_OL_CLASS_ID_NUM
               R_OPTIONS_CLASS_POINTERS(I) = OPTIONS_OL_ASSET_CLASS_POINTER(I)
            ENDDO

         ELSE
            DO I = 1, MAX_BC_CLASS_ID_NUM
               R_OPTIONS_CLASS_POINTERS(I) = OPTIONS_BC_ASSET_CLASS_POINTER(I)
            ENDDO

         ENDIF
      RETURN
!
 1000 FORMAT(A)
 1010 FORMAT('&',A)
      END
! 031210.
!
!

!***********************************************************************
!
! CAPACITY OPTINS OBJECT 9/2/93 MSG
!

      FUNCTION CAPACITY_OPTIONS_OBJECT()
      use end_routine, only: end_program, er_message
      use CAPACITY_OPTIONS_ALLOC_VARS
!***********************************************************************
!
      use SpinDriftLib
      use prod_arrays_dimensions
      USE CAPACITY_OPTIONS_ALLOC_VARS
      USE CAPACITY_OPTIONS_FIXED_VARS
      USE CLA_OBJT_ARRAYS
      USE IREC_ENDPOINT_CONTROL
      use grx_planning_routines
      USE SIZECOM
      use globecom
      implicit none
      SAVE

      integer :: astat
      CHARACTER (LEN=20) :: PRIMARY_FUEL_CATEGORY
      INTEGER (KIND=2) :: GetMaxGenericCoalBuilds,GenericCoalUnitsMaxBuilds
      LOGICAL (kind=1) ::  CHECK_CL_OPTIONS,SALT_RIVER_PROJECT
      REAL ::  R_COST,R_CAPACITY,TEMP_REAL
      CHARACTER (len=20) ::  GET_OPTIONS,GET_OPTIONS_RES_TYPE
      CHARACTER (len=2) ::  R_FILE_SOURCE,GREEN_MRX_METHOD
      INTEGER ::  CAPACITY_OPTIONS_OBJECT
      LOGICAL (kind=1) ::  CAPACITY_PLANNING_ACTIVE, &
                POSSIBLE_OPTIONS_EXIST, &
                DEPENDENCY_ACTIVE, &
                DSM_RESET_OPTIONS, &
                CONTRACT_RESET_OPTIONS, &
                VALID_OPTION,EL_OPTION_CHECK, &
                RENEWABLE_OPTION_CHECK, &
                TRANSMISSION_OPTION_CHECK, &
                EFFICIENCY_OPTION_CHECK, &
                ANN_DECOMP_ACTIVE, &
                VOID_LOGICAL, &
                TEST_START_STOP_YEARS, &
                SP_CAPEX_ACTIVE
      CHARACTER (len=1) ::  CHECK_CAP_PLANNING_SWTCHES
      INTEGER (kind=2) ::  READ_CAPACITY_OPTIONS_FILE,L, &
                AN_DECOMP_REPORT_HEADER, &
                AVAILABLE_UNITS, &
                GET_FILE_SOURCE_INDEX, &
                GET_RESOURCE_TYPE_INDEX, &
                YR1,YR2
      INTEGER (kind=2) ::  I,IREC,J,DELETE
      INTEGER (kind=4) ::  IOS,MaxProductionProinter
      INTEGER (kind=2) ::  RESET_CONTRACT_ADDITIONS,RESET_CONTRACT_COSTS
      REAL ::  ADD_NEW_CT_UNIT,CT_CAPACITY_PLANNING_ADDITIONS
      INTEGER (kind=2) ::  VOID_INT2,INCREMENT_HARDWIRED_CL_UNITS,INCREMENT_HARDWIRED_EL_UNITS
      INTEGER (kind=2) ::  CURRENT_YEAR
      INTEGER (kind=2) ::  R_BASE_OPTIONS
      INTEGER (kind=2) ::  R_CYCLE_OPTIONS,R_PEAK_OPTIONS,R_PEAK_REDUC_OPTIONS,R_HARD_WIRED_OPTIONS,R_FILL_OPTIONS
!
      INTEGER (KIND=2) :: RESET_NUMBER_OF_OPTIONS_TO_BC
      CHARACTER (len=2) ::  CAP_STACK_OL
!
      INTEGER (kind=2) ::  R_POINTER,R_YEAR, &
                R_P, &
                A_POINTER, &
                R_A_POINTER, &
                L_POINTER,L_YEAR, &
                W_POINTER,W_YEAR, &
                GET_PRODUCTION_DATA_POINTER, &
                GET_GRX_RESOURCE_LINK_ID, &
                NUMBER_OF_POSSIBLE_OPTIONS
      INTEGER (kind=2) ::  R_MAX_RESOURCE_ID
      REAL ::  R_ANN_CAP,R_PEAK,PERCENT_COMPLETED
      INTEGER (kind=2) ::  INCREMENT_AVAILABLE_CL_UNITS,INCREMENT_AVAILABLE_EL_UNITS
      INTEGER (kind=2) ::  ANNUAL_UNITS_FOR_OPTION
      INTEGER (kind=2) ::  CUMULATIVE_UNITS_FOR_OPTION
      INTEGER (kind=2) ::  BASE_POINTR,CYCL_POINTR,POINTER, &
                PEAK_POINTR,LOAD_POINTR,FILL_POINTR, &
                HARD_WIRE_POINTR,CURRENT_ACTIVE_OPTIONS, &
                FIRST_POINTER,LAST_POINTER,START_POINTER
      LOGICAL (kind=1) ::  CL_UNIT_ADDED, &
                WRITE_TO_EXPANSION_REPORT, &
                EXPANSION_REPORT,L_TEMP,GET_NEXT_ADDITION, &
                FOUND_NEXT_ADDITION,CHECK_OPTIONS_LIST, &
                CT_OPTION_CHECK, &
                WRITE_2_EXPANSION_PATTERN_REPT, &
                DEPENDENT_UNIT_AVAILABLE
      REAL ::  REAL_CURRENT_YEAR
      REAL ::  CAPACITY_ADDED,FIRST_YEAR_CL_CAPACITY,ADD_NEW_EL_UNIT,MUT_EXCLUSIVE_CAPACITY_RETIRED
!
      INTEGER (kind=2) ::  ADD_NEW_CL_UNIT,CL_UNIT_POSITION
      INTEGER (kind=2) ::  ADD_HARD_WIRED_UNIT,HARD_WIRED_UNITS_ADDED,ADD_ALL_HARD_WIRED_UNITS,PR_ADD_HARD_WIRED_UNIT
      LOGICAL (kind=1) ::  ADD_OPTIM_UNIT,R_ABANDON_IT
      REAL ::  FIRST_YEAR_CAPACITY
!
      INTEGER (kind=2) ::  GENGRP
      REAL ::  CAP_FRAC_OWN
      CHARACTER (len=20) ::  CL_UNIT_NAME
      CHARACTER (len=2) ::  CL_LOAD_TYPE
      CHARACTER (len=1) ::  EXPENSE_ASSIGNMENT,EXPENSE_COLLECTION
!
      INTEGER (KIND=2) :: RETURN_CL_OPTIONS_NO
!
      INTEGER (kind=2) ::  R_COMMITTED_OPTION_POINTERS(*), &
                R_COMMITTED_OPTION_START_YEAR(*), &
                R_COMMITTED_OPTION_ON_LINE_YR(*), &
                R_COMMITTED_ORG_ON_LINE_YR(*), &
                ADD_HARD_WIRED_UNITS_2_NE_PLAN
      CHARACTER (len=35) ::  unit_name
      CHARACTER (len=15) ::  LEFT_JUSTIFY_I2_IN_STR
      CHARACTER (len=1) ::  CAP_TYPE
      CHARACTER (len=1) ::  R_MOST_NEEDED_CAP_TYPE, &
                  RECORD_ACTIVE
      INTEGER (kind=2) ::  R_OPERATION_LIFE,IN_SERVICE_YR,R_UNIT_NO
      INTEGER (kind=2) ::  R_OPTION_TIMES_DELAYED
      INTEGER (kind=2) ::  RESET_CUMULATIVE_UNITS_4_OPTNS
      INTEGER (kind=2) ::  RESET_ANNUAL_UNITS_LEFT, &
                REDUCE_ANNUAL_UNITS_LEFT, &
                LEAD_TIME_FOR_OPTION
      REAL ::  ADD_THIS_FINANCIAL_INVESTMENT, &
           ADD_THIS_UNIT_2_CAPACITY, &
           ADJUST_FINANCIAL_INVESTMENT, &
           ADD_THIS_UNIT
      INTEGER (kind=2) ::  R_I,YR,I2_ONE=1
      INTEGER (kind=2) ::  R_CURRENT_YEAR,R_AVAILABLE_LEAD_TIME,R_ICAP, &
                GET_FILL_UNITS_NEEDED,ROUNDING_UNIT,TRANS_ID
      REAL (kind=4) ::     R_CAPACITY_NEEDED,CAPACITY_NEEDED
      LOGICAL (kind=1) ::  CAPACITY_TYPE_EXISTS
      CHARACTER (len=2) ::   CAPACITY_PLANNING_METHOD
      LOGICAL (kind=1) ::  HARD_WIRED_UNITS_IN, &
                TRANS_GROUP_ACTIVE_SWITCH, &
                RUN_TRANSACT_PLANNING
      INTEGER (kind=2) ::  OFF_LINE_DATE, &
                EL_CAPACITY_TEMP_RETIRE_UNIT, &
                OVER_CAPACITY_TABLE_NUM, &
                UNDER_CAPACITY_TABLE_NUM
! THRESHOLD_CAPACITY VARIABLES:
      INTEGER (kind=2) ::    R_OPTION
      INTEGER (kind=2) ::    NUMBER_OF_THRESHOLD_UNITS_LEFT,I_YEAR,UPDATE_THRESHOLD,RETIRE_YEAR
      LOGICAL (kind=1) ::    THRESHOLD_EXCEEDED
!
!
! ANNUAL DECOMPOSITION VARIABLES 10/4/93 MSG
!
      REAL ::  GET_OPTION_CAPACITY
      CHARACTER (len=1) ::  GET_OPTION_LOADING_TYPE
      LOGICAL (kind=1) ::  CN_GET_OPTIONS,CN_GET_OPTIONS_RES_TYPE,CN_GET_OPTION_LOADING_TYPE
      INTEGER (kind=2) ::  GET_OPTION_LEAD_TIME
      INTEGER (kind=2) ::  RETURN_EL_UNIT_ON_LINE_MONTH
      INTEGER (kind=2) ::  RETURN_ACTIVE_OPTIONS_LIST
      INTEGER (kind=2) ::  R_OPTION_POINTR(*),R_OPTION_ANN_UNITS(*),R_OPTION_AVAIL_UNITS(*)
      CHARACTER (len=1) ::  R_OPTION_LOADING_TYPE(*),UTILITY_TYPE
      LOGICAL (kind=1) ::  DEPENDED_UNIT_FOUND,R_MUT_EXC_UNIT_ACTIVE
      REAL ::  R_OPTION_CAP(*)
!
      REAL ::  EL_CAPACITY_PLANNING_ADDITIONS, &
           EL_CAPACITY_PLANNING_REMOVALS, &
           CL_CAPACITY_PLANNING_ADJ, &
           ADD_UNIT_TEMPORARILY, &
           RETURN_RESOURCE_ID, &
           CON_COST_DOLLARS, &
           GET_DAILY_PUMPING_LIMIT, &
           STORAGE_MWH, &
           TEMP1_R4, &
           TEMP2_R4, &
           TEMP3_R4, &
           ESCALATED_MONTHLY_VALUE
      INTEGER (kind=2) ::  HARD_WIRED_YEAR,SAVED_MAX_LEAD_TIME,GET_MAX_LEAD_TIME
      LOGICAL (kind=1) ::  I_FALSE
      PARAMETER(I_FALSE = .FALSE.)
      LOGICAL (kind=1) ::  OPTIONS_REPORT_HEADER=.FALSE.
      INTEGER (kind=2) ::  WRITE_OPTIONS_REPORT_HEADER
      REAL ::  CON_COST_PER_KW,CON_COST_PER_MWH
      LOGICAL (kind=1) ::    RESOURCE_AVAILABLE
      LOGICAL (kind=1) ::    CALC_SCREEN_SLOPE_INTERCEPT,READ_CL_SCREEN_DATA,SUCCESFUL_READ,INIT_SCREEN_CAP_COST
      INTEGER (kind=2) ::  RETURN_TOTAL_ALL_OPTIONS
      INTEGER (kind=2) ::  INITIALIZE_SCREEN_DATA,RETURN_CN_OP_LIFE
      REAL ::   R_SLOPE,R_INTERCEPT,RETURN_CL_SCREEN_FIXED_COST, &
            RETURN_CL_SCREEN_VARI_COST, &
            SCREEN_CAP_COST, &
            R_INCREMENTAL_CAPACITY, &
            GET_SCREEN_CAPACITY, &
            RETURN_SCREEN_CAP_COST, &
            RETURN_SCREEN_OP_LIFE, &
            RETURN_RISK_ADJ_S_CAP_COST

      INTEGER (kind=2) ::  R_POINTR
!
! EXPANSION PATTERN REPORT VARIABLES
!
      CHARACTER (len=10) ::  R_RESOUCE_STATUS
      INTEGER (kind=2) ::  EXPANSION_PATTERN_NO
      INTEGER (kind=2) ::  EXPANSION_PATTERN_HEADER, &
                R_OPTION_POSITION(*)
      LOGICAL (kind=1) ::  EXPANSION_PATTERN_RPT_NOT_OPEN=.TRUE.
      REAL ::  R_PLANNING_PEAK,GET_NUMBER_OF_CL_ADDITIONS,GET_NUMBER_OF_EL_ADDITIONS
! SCREENING REPORT VARIABLES
      LOGICAL (kind=1) ::  GET_OPTION_NAMES
      INTEGER (kind=2) ::  R_TOTAL_ALL_OPTIONS
      CHARACTER (len=20) ::  R_OPTION_NAME(*)
      INTEGER ::  NEXT_REC_1502,EXPANSION_PATTERN_REC
!
!
! END DATA DECLARATIONS
!
!
      CAPACITY_OPTIONS_OBJECT = 1
      GenericCoalUnitsMaxBuilds = 0
      RETURN
!***********************************************************************
      ENTRY GET_FILL_UNITS_NEEDED(R_POINTR, &
                                  R_CURRENT_YEAR, &
                                  R_AVAILABLE_LEAD_TIME, &
                                  R_CAPACITY_NEEDED, &
                                  R_MOST_NEEDED_CAP_TYPE, &
                                  R_INCREMENTAL_CAPACITY, &
                                  R_A_POINTER) ! INTEGER
!***********************************************************************
!
         GET_FILL_UNITS_NEEDED = 0
         R_INCREMENTAL_CAPACITY = 0.
         ANN_DECOMP_ACTIVE = CHECK_CAP_PLANNING_SWTCHES() == 'A'
         I = -1  ! R_POINTR ! START AT NEXT I AFTER R_POINTR
         DO
            IF(I == -1) THEN ! FIRST TIME IN THIS LOOP
               IF(R_POINTR == 0 .OR. & !  FIRST TIME FOR THIS TECH
                                  R_POINTR == TOTAL_ACTIVE_OPTIONS) THEN
                  I = 1
               ELSEIF(R_POINTR < TOTAL_ACTIVE_OPTIONS) THEN ! START WITH THE NEXT VALUE OF R_POINTR
                  I = R_POINTR + 1
               ENDIF
               IF(R_POINTR == 0) R_POINTR = TOTAL_ACTIVE_OPTIONS
            ELSE
               IF(I == R_POINTR) THEN
                  EXIT
               ELSE
                  IF(I < TOTAL_ACTIVE_OPTIONS) THEN
                     I = I + 1
                  ELSE
                     I = 1
                  ENDIF
               ENDIF
            ENDIF
!
            A_POINTER = ACTIVE_OPTION_LIST(I)
            IF(YES_OR_NO_FILLER(A_POINTER) == 'N' .OR. &
               (R_MOST_NEEDED_CAP_TYPE /= LOADING_TYPE(A_POINTER) &
                              .AND. .NOT. ANN_DECOMP_ACTIVE ).OR. & !  ANN_DECOM DOESN'T KNOW ABOUT LOADING_TYPE
                 R_AVAILABLE_LEAD_TIME <= LEAD_TIME(A_POINTER) .OR. &
                    CUMULATIVE_UNITS(A_POINTER) <= 0 .OR. &
                       R_CURRENT_YEAR < &
                             FIRST_YEAR_AVAILABLE(A_POINTER) .OR. &
                           R_CURRENT_YEAR > &
                                    LAST_YEAR_AVAILABLE(A_POINTER) .OR. &
                              ANNUAL_UNITS_LEFT(A_POINTER) <= 0 .OR. &
                                 CUMULATIVE_UNITS(A_POINTER) <= 0 .OR. &
                                    UNIT_CAP(A_POINTER) <= 0. .OR. &
                                          R_CAPACITY_NEEDED <= 0.) CYCLE
!
            IF(MAX_UNIT_CAPACITY(A_POINTER) == 0.) THEN
               CAPACITY_NEEDED = 0.
               WRITE(4,*) "FILLER UNIT DOES NOT HAVE A MAXIMUM CAPACITY"
               WRITE(4,*) "UNIT NAME = ",unit_names(A_POINTER)
               WRITE(4,*) " "
            ELSE
               CAPACITY_NEEDED = MAX_UNIT_CAPACITY(A_POINTER) * (R_CAPACITY_NEEDED/MAX_UNIT_CAPACITY(A_POINTER) - &
                        INT(R_CAPACITY_NEEDED/MAX_UNIT_CAPACITY(A_POINTER)))
               R_INCREMENTAL_CAPACITY = CAPACITY_NEEDED
               IF(CAPACITY_NEEDED > 0.) THEN
                  ROUNDING_UNIT = 1
               ELSE
                  ROUNDING_UNIT = 0
               ENDIF
            ENDIF
!
            GET_FILL_UNITS_NEEDED = ROUNDING_UNIT + INT(CAPACITY_NEEDED/UNIT_CAP(A_POINTER))
            R_A_POINTER = A_POINTER
            R_POINTR = I
            EXIT
!
         ENDDO
!
! NOT SURE WHY R_INCREMENTAL_CAPACITY = 0. AT THE TOP, BUT
! THIS FIX WILL ALLOW UNITS OF OTHER TYPES TO BE ADDED IF
! THE CURRENT TYPE IS NOT DEFINED, AND DOES NOT MESS-UP THE
! CURRENT FILLER LOGIC. GAT. 9/1/96.
!
         IF(GET_FILL_UNITS_NEEDED == 0.) R_INCREMENTAL_CAPACITY = 999.
      RETURN
!***********************************************************************
      ENTRY READ_CAPACITY_OPTIONS_FILE ! INTEGER
!***********************************************************************
         CALL OPEN_CAPACITY_OPTIONS_FILE(10)
         CALL GET_CAP_STACK_OL(CAP_STACK_OL)
         IF(CAP_STACK_OL == 'BC') &
                             VOID_INT2 = RESET_NUMBER_OF_OPTIONS_TO_BC()
         IF(ALLOCATED(UNIT_NAMES)) THEN
            DEALLOCATE(UNIT_NAMES,LOADING_TYPE,FILE_SOURCE, &
             RESOURCE_ID_NUM,UNIT_IS_IDENTICAL_TO,RESOURCE_GROUP_NUM, &
             UNIT_CAP,MAX_UNIT_CAPACITY,CCR_FOR_SCREENING, &
             FIRST_YEAR_AVAILABLE,LAST_YEAR_AVAILABLE, &
             OPERATION_LIFE,LEAD_TIME,ANNUAL_UNITS,ANNUAL_UNITS_LEFT, &
             PRODUCTION_DATA_POINTER,INVESTMENT_DATA_POINTER, &
             CUMULATIVE_UNITS,TOTAL_UNITS_ADDED, &
             SAVED_CUMULATIVE_UNITS,GRX_SAVED_CUMULATIVE_UNITS, &
             ESCALATION_VECTOR,COMPOUND_ESC,DEPEND_UNIT_NO, &
             MUT_EXC_UNIT,POINTER_TO_DEPENDENT_UNIT,DEPENDENT, &
             INDEPENDENT,MRX_RISK_ADJUSTMENT,RESOURCE_TYPE, &
             DEPEND_RESOURCE_ID_2_I,INDEPENDENT_UNIT_STATUS, &
             INDPENDENT_UNIT,CONTROLS_DEPEND,IS_A_HARD_WIRED_UNIT, &
             HAS_BEEN_SELECTED,CONSTRUCTION_COSTS, &
             OVN_CAPITAL_COST_STOCASTIC,RENEWABLE_ENERGY_PERCENT, &
             GRX_RESOURCE_LINK_ID,HARD_WIRED_UNIT_AVAILABLE, &
             ADDITIONAL_INVESTMENT_POINTER,PRIMARY_ASSET_CLASS, &
             PRIMARY_ALLOCATION_VECTOR,ADDITIONAL_ASSET_CLASS, &
             ADDITIONAL_ALLOCATION_VECTOR,OVER_CAPACITY_TABLE, &
             UNDER_CAPACITY_TABLE,YES_OR_NO_FILLER)
         ENDIF

         IF(ALLOCATED(BASE_OPTION_LIST)) DEALLOCATE(BASE_OPTION_LIST)
         IF(ALLOCATED(CYCL_OPTION_LIST)) DEALLOCATE(CYCL_OPTION_LIST)
         IF(ALLOCATED(PEAK_OPTION_LIST)) DEALLOCATE(PEAK_OPTION_LIST)
         IF(ALLOCATED(LOAD_OPTION_LIST)) DEALLOCATE(LOAD_OPTION_LIST)
         IF(ALLOCATED(FILL_OPTION_LIST)) DEALLOCATE(FILL_OPTION_LIST)
         IF(ALLOCATED(ACTIVE_OPTION_LIST))DEALLOCATE(ACTIVE_OPTION_LIST)
         IF(ALLOCATED(HARD_WIRED_LIST)) DEALLOCATE(HARD_WIRED_LIST, &
                                                HARD_WIRED_ON_LINE_YEAR)
         FOR_ALL_OPTIONS = MAX(INT(1,2),TOTAL_ALL_OPTIONS)
         MAX_ID_FOR_ALL_OPTIONS = MAX(INT(1,2), &
                                      MAX_RESOURCE_ID,FOR_ALL_OPTIONS)
         YR1 = BASE_YEAR + 1
         YR2 = BASE_YEAR + STUDY_PERIOD

         allocate(ESCALATION_VECTOR(FOR_ALL_OPTIONS), STAT=astat)
         if(astat/=0) then
             er_message="cn_objt:0001 - unable to allocate " // &
       "escalation vector."
             call end_program(er_message)
         endif

         escalation_vector=0

         ALLOCATE(unit_names(FOR_ALL_OPTIONS),LOADING_TYPE(FOR_ALL_OPTIONS),FILE_SOURCE(FOR_ALL_OPTIONS), &
               RESOURCE_ID_NUM(FOR_ALL_OPTIONS),UNIT_IS_IDENTICAL_TO(FOR_ALL_OPTIONS),RESOURCE_GROUP_NUM(FOR_ALL_OPTIONS), &
               UNIT_CAP(FOR_ALL_OPTIONS),MAX_UNIT_CAPACITY(FOR_ALL_OPTIONS),CCR_FOR_SCREENING(FOR_ALL_OPTIONS), &
               FIRST_YEAR_AVAILABLE(FOR_ALL_OPTIONS),LAST_YEAR_AVAILABLE(FOR_ALL_OPTIONS),OPERATION_LIFE(FOR_ALL_OPTIONS), &
               LEAD_TIME(FOR_ALL_OPTIONS),ANNUAL_UNITS(FOR_ALL_OPTIONS),ANNUAL_UNITS_LEFT(FOR_ALL_OPTIONS), &
               PRODUCTION_DATA_POINTER(FOR_ALL_OPTIONS),INVESTMENT_DATA_POINTER(FOR_ALL_OPTIONS), &
               CUMULATIVE_UNITS(FOR_ALL_OPTIONS),TOTAL_UNITS_ADDED(FOR_ALL_OPTIONS),SAVED_CUMULATIVE_UNITS(FOR_ALL_OPTIONS), &
               GRX_SAVED_CUMULATIVE_UNITS(FOR_ALL_OPTIONS),COMPOUND_ESC(FOR_ALL_OPTIONS),DEPEND_UNIT_NO(FOR_ALL_OPTIONS), &
               MUT_EXC_UNIT(FOR_ALL_OPTIONS),POINTER_TO_DEPENDENT_UNIT(TOTAL_ALL_OPTIONS),DEPENDENT(TOTAL_ALL_OPTIONS), &
               INDEPENDENT(TOTAL_ALL_OPTIONS),MRX_RISK_ADJUSTMENT(TOTAL_ALL_OPTIONS),RESOURCE_TYPE(TOTAL_ALL_OPTIONS), &
               OVN_CAPITAL_COST_STOCASTIC(TOTAL_ALL_OPTIONS), & !  35
               RENEWABLE_ENERGY_PERCENT(TOTAL_ALL_OPTIONS),GRX_RESOURCE_LINK_ID(TOTAL_ALL_OPTIONS), &
               DEPEND_RESOURCE_ID_2_I(MAX_ID_FOR_ALL_OPTIONS),INDEPENDENT_UNIT_STATUS(TOTAL_ALL_OPTIONS), &
               INDPENDENT_UNIT(TOTAL_ALL_OPTIONS),CONTROLS_DEPEND(MAX_ID_FOR_ALL_OPTIONS),IS_A_HARD_WIRED_UNIT(FOR_ALL_OPTIONS), &
               HAS_BEEN_SELECTED(FOR_ALL_OPTIONS),CONSTRUCTION_COSTS(FOR_ALL_OPTIONS,YR1:YR2), &
               HARD_WIRED_UNIT_AVAILABLE(STUDY_PERIOD+15),ADDITIONAL_INVESTMENT_POINTER(FOR_ALL_OPTIONS), &
               PRIMARY_ASSET_CLASS(FOR_ALL_OPTIONS),PRIMARY_ALLOCATION_VECTOR(FOR_ALL_OPTIONS), &
               ADDITIONAL_ASSET_CLASS(FOR_ALL_OPTIONS),ADDITIONAL_ALLOCATION_VECTOR(FOR_ALL_OPTIONS), &
               OVER_CAPACITY_TABLE(FOR_ALL_OPTIONS),UNDER_CAPACITY_TABLE(FOR_ALL_OPTIONS),YES_OR_NO_FILLER(FOR_ALL_OPTIONS))
!
         IF(ALLOCATED(THRESHOLD_CAPACITY)) &
                                          DEALLOCATE(THRESHOLD_CAPACITY)
         ALLOCATE(THRESHOLD_CAPACITY(FOR_ALL_OPTIONS, &
                                                  MAX_SIMULATION_YEARS))
         THRESHOLD_CAPACITY = 0.
!
         unit_names=" "
         ANNUAL_UNITS = 0
         ANNUAL_UNITS_LEFT = 0
         CUMULATIVE_UNITS = 0

         IF(BASE_OPTIONS > 0) ALLOCATE(BASE_OPTION_LIST(BASE_OPTIONS))
         IF(CYCLE_OPTIONS > 0) ALLOCATE(CYCL_OPTION_LIST(CYCLE_OPTIONS))
         IF(PEAK_OPTIONS > 0) ALLOCATE(PEAK_OPTION_LIST(PEAK_OPTIONS))
         IF(PEAK_REDUC_OPTIONS > 0) ALLOCATE(LOAD_OPTION_LIST(PEAK_REDUC_OPTIONS))
         IF(FILL_OPTIONS > 0) ALLOCATE(FILL_OPTION_LIST(FILL_OPTIONS))
         IF(TOTAL_ACTIVE_OPTIONS > 0) ALLOCATE(ACTIVE_OPTION_LIST(TOTAL_ACTIVE_OPTIONS))
         IF(HARD_WIRED_OPTIONS > 0) THEN
            ALLOCATE(HARD_WIRED_LIST(HARD_WIRED_OPTIONS),HARD_WIRED_ON_LINE_YEAR(HARD_WIRED_OPTIONS))
         ENDIF
         IF(ALLOCATED(SAVE_NO_CL_ADDITIONS)) DEALLOCATE(SAVE_NO_CL_ADDITIONS)
         ALLOCATE(SAVE_NO_CL_ADDITIONS(MAX_SIMULATION_YEARS))
         SAVE_NO_CL_ADDITIONS = 0
         IF(ALLOCATED(SAVE_NO_EL_ADDITIONS)) DEALLOCATE(SAVE_NO_EL_ADDITIONS)
         ALLOCATE(SAVE_NO_EL_ADDITIONS(MAX_SIMULATION_YEARS))
         SAVE_NO_EL_ADDITIONS = 0
!
! SET CONTROLS_DEPEND TO FALSE
!
         CONTROLS_DEPEND = I_FALSE
         IS_A_HARD_WIRED_UNIT = I_FALSE
         HAS_BEEN_SELECTED = I_FALSE
         DEPEND_UNIT_NO = 0
         HARD_WIRED_UNIT_AVAILABLE = I_FALSE
! 08/19/04
         INDEPENDENT_UNIT_STATUS = 0
         INDPENDENT_UNIT = 0
         DEPEND_RESOURCE_ID_2_I = 0
! 010420
         DEPENDENT = 0
         INDEPENDENT = 0
         RENEWABLE_ENERGY_PERCENT = 0.0
!
         CHECK_CL_OPTIONS = UTILITY_TYPE() == 'T' .OR. SALT_RIVER_PROJECT()
         RUN_TRANSACT_PLANNING = CAPACITY_PLANNING_METHOD() == 'TR' ! IMPOSES SPECIFIC PLANNING METHOD
         I = 1
         IREC = 0
         DSM_OPTIONS = 0
         DSM_ADDITIONS = 0
         CONTRACT_OPTIONS = 0
         RENEWABLE_OPTIONS = 0
         EFFICIENCY_OPTIONS=0
         CONTRACT_ADDITIONS = 0
         CL_OPTIONS = 0
         BASE_POINTR = 0
         CYCL_POINTR = 0
         PEAK_POINTR = 0
         LOAD_POINTR = 0
         FILL_POINTR = 0
         HARD_WIRE_POINTR = 0
         CURRENT_ACTIVE_OPTIONS = 0
         CONSTRUCTION_COSTS = 0.0
         COMPOUND_ESC = 1.0
         PRODUCTION_DATA_POINTER = 0.0
         FILE_SOURCE = " "
         DO WHILE (I <= TOTAL_ALL_OPTIONS)
            IREC = IREC + 1
            READ(10,REC=IREC,IOSTAT=IOS) DELETE,unit_names(I), &
               LOADING_TYPE(I),UNIT_CAP(I),CON_COST_DOLLARS, & !  CONSTRUCTION_COSTS(I),
               ESCALATION_VECTOR(I),FIRST_YEAR_AVAILABLE(I), &
               LAST_YEAR_AVAILABLE(I),OPERATION_LIFE(I),LEAD_TIME(I), &
               ANNUAL_UNITS(I),CUMULATIVE_UNITS(I), &
               PRODUCTION_DATA_POINTER(I),INVESTMENT_DATA_POINTER(I), &
               MUT_EXC_UNIT(I),HARD_WIRED_YEAR,CON_COST_PER_KW, &
               FILE_SOURCE(I),RESOURCE_ID_NUM(I), &
               RESOURCE_GROUP_NUM(I),ADDITIONAL_INVESTMENT_POINTER(I), &
               OVER_CAPACITY_TABLE(I),UNDER_CAPACITY_TABLE(I), &
               PRIMARY_ASSET_CLASS(I),PRIMARY_ALLOCATION_VECTOR(I), &
               ADDITIONAL_ASSET_CLASS(I), &
               ADDITIONAL_ALLOCATION_VECTOR(I),MAX_UNIT_CAPACITY(I), &
               CCR_FOR_SCREENING(I),YES_OR_NO_FILLER(I),RECORD_ACTIVE, &
               POINTER_TO_DEPENDENT_UNIT(I),MRX_RISK_ADJUSTMENT(I), &
               RESOURCE_TYPE(I),OVN_CAPITAL_COST_STOCASTIC(I), & !  35
               GRX_RESOURCE_LINK_ID(I),CON_COST_PER_MWH, &
               RENEWABLE_ENERGY_PERCENT(I)
            IF(IOS /= 0) EXIT
            IF(FILE_SOURCE(I) /= 'CL' .and. &
                  FILE_SOURCE(I) /= 'De' .and. &
                     FILE_SOURCE(I) /= 'Tr' .and. &
                                       GREEN_MRX_METHOD() /= 'GX') CYCLE
            IF(DELETE > 7 .OR. RECORD_ACTIVE == 'F') CYCLE
            IF(.NOT. (HARD_WIRED_YEAR >= BASE_YEAR+LEAD_TIME(I) .AND. &
                  HARD_WIRED_YEAR-LEAD_TIME(I) <= LAST_STUDY_YEAR)) THEN
               IF(FIRST_YEAR_AVAILABLE(I) > LAST_STUDY_YEAR .OR. &
                  LAST_YEAR_AVAILABLE(I) <= BASE_YEAR .OR. &
                  ANNUAL_UNITS(I) == 0 .OR. &
                  CUMULATIVE_UNITS(I) == 0 .OR. &
                  LAST_YEAR_AVAILABLE(I) < FIRST_YEAR_AVAILABLE(I))CYCLE
            ENDIF
            IF(FILE_SOURCE(I) == 'CL') THEN
               CALL CL_CAPACITY_AND_TRANS_CHECK(unit_names(I),PRODUCTION_DATA_POINTER(I),UNIT_CAP(I),TRANS_ID,PRIMARY_FUEL_CATEGORY)
               IF(INDEX(PRIMARY_FUEL_CATEGORY,"Coal")/=0) THEN
                  IF(HARD_WIRED_YEAR >= BASE_YEAR+LEAD_TIME(I) .AND. HARD_WIRED_YEAR-LEAD_TIME(I) <= LAST_STUDY_YEAR) THEN
                      GenericCoalUnitsMaxBuilds = GenericCoalUnitsMaxBuilds + 1
                  ELSE
                     GenericCoalUnitsMaxBuilds = GenericCoalUnitsMaxBuilds + MIN(CUMULATIVE_UNITS(I),STUDY_PERIOD * ANNUAL_UNITS(I))
                  ENDIF
               ENDIF
               IF(RUN_TRANSACT_PLANNING .AND. .NOT. TRANS_GROUP_ACTIVE_SWITCH(TRANS_ID)) THEN
                  IF(HARD_WIRED_YEAR >= BASE_YEAR+LEAD_TIME(I) .AND. HARD_WIRED_YEAR-LEAD_TIME(I) <= LAST_STUDY_YEAR) &
!
                              HARD_WIRED_OPTIONS = HARD_WIRED_OPTIONS -1
                  CYCLE
               ENDIF
            ENDIF
!
!
! CHECK FOR CONSISTENT DATA SET
!
            SAVED_MAX_LEAD_TIME = MAX(SAVED_MAX_LEAD_TIME,LEAD_TIME(I))
            IF(FILE_SOURCE(I) == 'EL') THEN
               VALID_OPTION=EL_OPTION_CHECK(PRODUCTION_DATA_POINTER(I))
               IF(.NOT. VALID_OPTION) THEN
                  WRITE(4,*) 'Energy-limited expansion option'
                  WRITE(4,*) trim(unit_names(I)),' does not have'
                  WRITE(4,*) 'a valid production pointer.'
                  CYCLE
               ENDIF
            ENDIF
            IF(FILE_SOURCE(I) == 'De') THEN
               VALID_OPTION=RENEWABLE_OPTION_CHECK(GRX_RESOURCE_LINK_ID(I))
!
               IF(.NOT. VALID_OPTION) THEN
                  WRITE(4,*) 'Renewable or efficiency expansion option'
                  WRITE(4,*) trim(unit_names(I)),' does not have'
                  WRITE(4,*) 'a valid production pointer.'
                  CYCLE
               ENDIF
            ENDIF
            IF(FILE_SOURCE(I) == 'Tr') THEN
               VALID_OPTION=TRANSMISSION_OPTION_CHECK(GRX_RESOURCE_LINK_ID(I))
!
               IF(.NOT. VALID_OPTION) THEN
                  WRITE(4,*) 'Transmission expansion option'
                  WRITE(4,*) trim(unit_names(I)),' does not have'
                  WRITE(4,*) 'a valid GRX Resource Link.'
                  CYCLE
               ENDIF
            ENDIF

            IF(FILE_SOURCE(I) == 'LM') THEN
               CALL LM_OPTION_CHECK(PRODUCTION_DATA_POINTER(I),VALID_OPTION)
               IF(.NOT. VALID_OPTION) THEN
                  WRITE(4,*)'DSM resource option ',trim(unit_names(I))
                  WRITE(4,*)' does not have a valid production pointer.'
                  CYCLE
               ENDIF
            ENDIF
            IF(FILE_SOURCE(I) == 'CT') THEN
               VALID_OPTION=CT_OPTION_CHECK(PRODUCTION_DATA_POINTER(I))
               IF(.NOT. VALID_OPTION) THEN
                  WRITE(4,*)'Contract resource option ',trim(unit_names(I))
                  WRITE(4,*)' does not have a valid production pointer.'
                  CYCLE
               ENDIF
            ENDIF
! 081816. GAT. FOR KATHY.

            STORAGE_MWH = &
                        GET_DAILY_PUMPING_LIMIT(GRX_RESOURCE_LINK_ID(I))
            RENEWABLE_ENERGY_PERCENT(I) = .01 * RENEWABLE_ENERGY_PERCENT(I)

            IF(CON_COST_PER_KW > -0.001 .AND. CON_COST_PER_MWH > -0.001 .AND. CON_COST_DOLLARS > -0.001) THEN
               CONSTRUCTION_COSTS(I,:) = CON_COST_DOLLARS + UNIT_CAP(I) * CON_COST_PER_KW/1000. + STORAGE_MWH * CON_COST_PER_MWH
            ELSE
               DO YR = 1, STUDY_PERIOD
                  IF(CON_COST_DOLLARS <= -0.001) THEN
                     TEMP1_R4 = ABS(CON_COST_DOLLARS)
                     TEMP1_R4 = ESCALATED_MONTHLY_VALUE(TEMP1_R4,INT(TEMP1_R4,2),YR,INT(1,2),INT(1,2))
                  ELSE
                     TEMP1_R4 = CON_COST_DOLLARS
                  ENDIF
                  IF(CON_COST_PER_KW <= -0.001) THEN
                     TEMP2_R4 = ABS(CON_COST_PER_KW)
                     TEMP2_R4 = 0.001 * UNIT_CAP(I) * ESCALATED_MONTHLY_VALUE(TEMP2_R4,INT(TEMP2_R4,2),YR,INT(1,2),INT(1,2))
                  ELSE
                     TEMP2_R4 = UNIT_CAP(I) * CON_COST_PER_KW/1000.
                  ENDIF
                  IF(CON_COST_PER_MWH <= -0.001) THEN
                     TEMP3_R4 = ABS(CON_COST_PER_MWH)
                     TEMP3_R4 = 0.000001 * UNIT_CAP(I) * ESCALATED_MONTHLY_VALUE(TEMP3_R4,INT(TEMP3_R4,2),YR,INT(1,2),INT(1,2))
                  ELSE
                     TEMP3_R4 = &
                               STORAGE_MWH * CON_COST_PER_MWH
                  ENDIF
                  CONSTRUCTION_COSTS(I,YR+BASE_YEAR) = &
                                          TEMP1_R4 + TEMP2_R4 + TEMP3_R4
               ENDDO
            ENDIF

            ANNUAL_UNITS(I) = MIN(ANNUAL_UNITS(I),CUMULATIVE_UNITS(I))
            UNIT_IS_IDENTICAL_TO(I) = RESOURCE_ID_NUM(I)
            IF(HARD_WIRED_YEAR > BASE_YEAR) THEN
               IF(HARD_WIRED_YEAR >= BASE_YEAR+LEAD_TIME(I) .AND. HARD_WIRED_YEAR-LEAD_TIME(I) <= LAST_STUDY_YEAR) THEN
                  HARD_WIRE_POINTR = HARD_WIRE_POINTR + 1
                  HARD_WIRED_LIST(HARD_WIRE_POINTR) = I
                  HARD_WIRED_ON_LINE_YEAR(HARD_WIRE_POINTR) = HARD_WIRED_YEAR
                  HARD_WIRED_YEAR = HARD_WIRED_YEAR - BASE_YEAR
                  HARD_WIRED_UNIT_AVAILABLE(HARD_WIRED_YEAR) = .TRUE.
                  IS_A_HARD_WIRED_UNIT(I) = .TRUE.
               ELSE
                  CYCLE
               ENDIF
            ELSE
               IF(EXPANSION_REPORT()) THEN
                  IF(.NOT. OPTIONS_REPORT_HEADER) THEN
                     VOID_INT2 = WRITE_OPTIONS_REPORT_HEADER(NEXT_REC_1502)
                     OPTIONS_REPORT_HEADER = .TRUE.
                  ENDIF
                  WRITE(1502,REC=NEXT_REC_1502) PRT_ENDPOINT(), &
                              unit_names(I), &
                              FLOAT(RESOURCE_ID_NUM(I)), &
                              FLOAT(RESOURCE_GROUP_NUM(I)), &
                              UNIT_CAP(I), &
                              CONSTRUCTION_COSTS(I,I2_ONE+BASE_YEAR), &
                              FLOAT(ESCALATION_VECTOR(I)), &
                              FLOAT(PRODUCTION_DATA_POINTER(I)), &
                              FLOAT(INVESTMENT_DATA_POINTER(I))
                  NEXT_REC_1502 = NEXT_REC_1502 + 1
               ENDIF
!
! CHECK FOR IDENTICAL UNIT
!
               DO J = 1, I-1
                  IF(IS_A_HARD_WIRED_UNIT(J)) CYCLE
!
                  IF(CONSTRUCTION_COSTS(I,YR1)== &
                                      CONSTRUCTION_COSTS(J,YR1) .AND. &
                      ESCALATION_VECTOR(I) == ESCALATION_VECTOR(J) .AND. &
                       OPERATION_LIFE(I) == OPERATION_LIFE(J) .AND. &
                        FILE_SOURCE(I) == FILE_SOURCE(J) .AND. &
                         LEAD_TIME(I) == LEAD_TIME(J) .AND. &
                          PRODUCTION_DATA_POINTER(I) == &
                                       PRODUCTION_DATA_POINTER(J) .AND. &
                            INVESTMENT_DATA_POINTER(I) == &
                                        INVESTMENT_DATA_POINTER(J)) THEN
!
                     UNIT_IS_IDENTICAL_TO(I) = RESOURCE_ID_NUM(J)
                     EXIT
                  ENDIF
               ENDDO
               IF(YES_OR_NO_FILLER(I) == 'Y') THEN
                  FILL_POINTR = FILL_POINTR + 1
                  FILL_OPTION_LIST(FILL_POINTR) = I
               ELSEIF(LOADING_TYPE(I) == 'B') THEN
                  BASE_POINTR = BASE_POINTR + 1
                  BASE_OPTION_LIST(BASE_POINTR) = I
               ELSEIF(LOADING_TYPE(I) == 'C') THEN
                  CYCL_POINTR = CYCL_POINTR + 1
                  CYCL_OPTION_LIST(CYCL_POINTR) = I
               ELSEIF(LOADING_TYPE(I) == 'P') THEN
                  PEAK_POINTR = PEAK_POINTR + 1
                  PEAK_OPTION_LIST(PEAK_POINTR) = I
               ELSEIF(LOADING_TYPE(I) == 'L') THEN
                  LOAD_POINTR = LOAD_POINTR + 1
                  LOAD_OPTION_LIST(LOAD_POINTR) = I
               ENDIF
               CURRENT_ACTIVE_OPTIONS = CURRENT_ACTIVE_OPTIONS + 1
               ACTIVE_OPTION_LIST(CURRENT_ACTIVE_OPTIONS) = I
               IF(FILE_SOURCE(I) == 'CL' ) CL_OPTIONS = CL_OPTIONS + 1
               IF(FILE_SOURCE(I) == 'LM' ) DSM_OPTIONS = DSM_OPTIONS+1
               IF(FILE_SOURCE(I) == 'CT' ) CONTRACT_OPTIONS = 1 + CONTRACT_OPTIONS
               IF(FILE_SOURCE(I) == 'De' ) RENEWABLE_OPTIONS = RENEWABLE_OPTIONS + 1
!
            ENDIF
!
! 10/1/93 ADDED TO FORCE EL, CT, AND DSM OPTIONS TO REFERENCE ONLY
!  ONE OBJECT EVEN FOR HARDWIRED OPTIONS
!
            IF(FILE_SOURCE(I) == 'CT' .OR. FILE_SOURCE(I) == 'LM' ) THEN
               ANNUAL_UNITS(I) = 1
               CUMULATIVE_UNITS(I) = 1
            ENDIF
!
! 1/8/93. GAT.
!
            MUT_EXC_UNIT(I) = ABS(MUT_EXC_UNIT(I))
            POINTER_TO_DEPENDENT_UNIT(I) = ABS(POINTER_TO_DEPENDENT_UNIT(I))

            IF(MUT_EXC_UNIT(I) /= 0 .AND. MUT_EXC_UNIT(I) <= MAX_RESOURCE_ID) THEN
               CONTROLS_DEPEND(MUT_EXC_UNIT(I)) = .TRUE.
            ENDIF
!
            ANNUAL_UNITS_LEFT(I) = ANNUAL_UNITS(I)
            SAVED_CUMULATIVE_UNITS(I) = CUMULATIVE_UNITS(I)
            TOTAL_UNITS_ADDED(I) = 0.
            I = I + 1
!
         ENDDO
         CLOSE(10)
         TOTAL_ALL_OPTIONS = MIN(I,TOTAL_ALL_OPTIONS)
         DO I = 1, TOTAL_ALL_OPTIONS
            IF(POINTER_TO_DEPENDENT_UNIT(I) == 0) CYCLE
            DO J = 1, TOTAL_ALL_OPTIONS
                IF(I == J .OR. RESOURCE_ID_NUM(J)== 0) CYCLE
                IF(POINTER_TO_DEPENDENT_UNIT(I) == & !  DEFINES THIS AS A DEPENDENT UNIT
                                                RESOURCE_ID_NUM(J)) THEN
! 122919.
                  DEPENDENT(I) = J
                  INDEPENDENT(J) = I
!
                   DEPEND_RESOURCE_ID_2_I(RESOURCE_ID_NUM(J)) = J
                   INDPENDENT_UNIT(J) = 1 ! DEFINES THIS AS AN INDEPENDENT UNIT
                ENDIF
            END DO
         END DO
!
         IF(CHECK_CL_OPTIONS) THEN
            CALL CL_CAPACITY_CHECK(TOTAL_ALL_OPTIONS,UNIT_NAME,PRODUCTION_DATA_POINTER,UNIT_CAP,FILE_SOURCE)
            CALL OPTIONS_FIN_CHECK(TOTAL_ALL_OPTIONS,UNIT_NAME,INVESTMENT_DATA_POINTER,LEAD_TIME,FILE_SOURCE)
         ENDIF
!
         IF(DSM_OPTIONS > 0) THEN
            IF(ALLOCATED(ADDED_LM_PROGRAMS)) DEALLOCATE(ADDED_LM_PROGRAMS)
            ALLOCATE(ADDED_LM_PROGRAMS(DSM_OPTIONS))
            ADDED_LM_PROGRAMS = -1
         ENDIF
!
         IF(CONTRACT_OPTIONS > 0) THEN
            IF(ALLOCATED(ADDED_CONTRACT_PROGRAMS)) DEALLOCATE(ADDED_CONTRACT_PROGRAMS)
            ALLOCATE(ADDED_CONTRACT_PROGRAMS(CONTRACT_OPTIONS))
            ADDED_CONTRACT_PROGRAMS = -1
         ENDIF
!
         READ_CAPACITY_OPTIONS_FILE = TOTAL_ALL_OPTIONS
!
      RETURN
!***************************************************************
      ENTRY GetMaxGenericCoalBuilds()
         GetMaxGenericCoalBuilds = GenericCoalUnitsMaxBuilds
      RETURN
!***********************************************************************
      ENTRY GET_RESOURCE_TYPE_INDEX(R_POINTER)
!***********************************************************************
         IF(RESOURCE_TYPE(R_POINTER) == 'Expansion') THEN
            GET_RESOURCE_TYPE_INDEX = 1
         ELSEIF(RESOURCE_TYPE(R_POINTER) == 'Renewable') THEN
            GET_RESOURCE_TYPE_INDEX = 2
         ELSEIF(RESOURCE_TYPE(R_POINTER) == 'Efficiency') THEN
            GET_RESOURCE_TYPE_INDEX = 3
         ELSEIF(RESOURCE_TYPE(R_POINTER) == 'Retirement') THEN
            GET_RESOURCE_TYPE_INDEX = 4
         ELSEIF(RESOURCE_TYPE(R_POINTER) == 'Retrofit') THEN
            GET_RESOURCE_TYPE_INDEX = 5
         ELSE
            GET_RESOURCE_TYPE_INDEX = 1
         ENDIF
      RETURN
!***********************************************************************
      ENTRY GET_FILE_SOURCE_INDEX(R_POINTER)
!***********************************************************************
         IF(FILE_SOURCE(R_POINTER) == 'CL') THEN
            GET_FILE_SOURCE_INDEX = 1
         ELSEIF(FILE_SOURCE(R_POINTER) == 'EL') THEN
            GET_FILE_SOURCE_INDEX = 2
         ELSEIF(FILE_SOURCE(R_POINTER) == 'De') THEN
            GET_FILE_SOURCE_INDEX = 3
         ELSEIF(FILE_SOURCE(R_POINTER) == 'Tr') THEN ! TRANSMISSION EXPANSION
            GET_FILE_SOURCE_INDEX = 4
         ELSE
            GET_FILE_SOURCE_INDEX = 1
         ENDIF
      RETURN
!***********************************************************************
      ENTRY GET_MAX_LEAD_TIME ! INTEGER
!***********************************************************************
         GET_MAX_LEAD_TIME = SAVED_MAX_LEAD_TIME
      RETURN
!***********************************************************************
      ENTRY RETURN_RESOURCE_ID(R_POINTER) ! INTEGER
!***********************************************************************
         RETURN_RESOURCE_ID = FLOAT(RESOURCE_ID_NUM(R_POINTER))
      RETURN
!***********************************************************************
      ENTRY ANNUAL_UNITS_FOR_OPTION(R_POINTER) ! INTEGER
!***********************************************************************
         ANNUAL_UNITS_FOR_OPTION = ANNUAL_UNITS(R_POINTER)
      RETURN
!***********************************************************************
      ENTRY LEAD_TIME_FOR_OPTION(R_POINTER) ! INTEGER
!***********************************************************************
         LEAD_TIME_FOR_OPTION = LEAD_TIME(R_POINTER)
      RETURN
!***********************************************************************
      ENTRY CUMULATIVE_UNITS_FOR_OPTION(R_POINTER) ! INTEGER
!***********************************************************************
         CUMULATIVE_UNITS_FOR_OPTION = CUMULATIVE_UNITS(R_POINTER)
      RETURN
!***********************************************************************
      ENTRY RESET_CUMULATIVE_UNITS_4_OPTNS ! INTEGER
!***********************************************************************
         RESET_CUMULATIVE_UNITS_4_OPTNS = FOR_ALL_OPTIONS
         DO I = 1, FOR_ALL_OPTIONS
            CUMULATIVE_UNITS(I) = SAVED_CUMULATIVE_UNITS(I)
         ENDDO
      RETURN
!***********************************************************************
      ENTRY RESET_ANNUAL_UNITS_LEFT ! INTEGER
!***********************************************************************
         RESET_ANNUAL_UNITS_LEFT = FOR_ALL_OPTIONS
         DO I = 1, FOR_ALL_OPTIONS
            ANNUAL_UNITS_LEFT(I) = ANNUAL_UNITS(I)
         ENDDO
      RETURN
!***********************************************************************
      ENTRY REDUCE_ANNUAL_UNITS_LEFT(R_POINTER) ! INTEGER
!***********************************************************************
         ANNUAL_UNITS_LEFT(R_POINTER) = ANNUAL_UNITS_LEFT(R_POINTER) - 1
         REDUCE_ANNUAL_UNITS_LEFT = ANNUAL_UNITS_LEFT(R_POINTER)
      RETURN
!***********************************************************************
      ENTRY HARD_WIRED_UNITS_IN(R_YEAR) ! INTEGER
!***********************************************************************
         HARD_WIRED_UNITS_IN = HARD_WIRED_UNIT_AVAILABLE(R_YEAR)
      RETURN
!***********************************************************************
      ENTRY ADD_HARD_WIRED_UNIT(R_YEAR,R_ANN_CAP,R_PEAK) ! INTEGER
!***********************************************************************
         HARD_WIRED_UNITS_ADDED = 0
         DO I = 1, HARD_WIRED_OPTIONS
            IF(HARD_WIRED_ON_LINE_YEAR(I) /= R_YEAR) CYCLE
            FIRST_YEAR_CAPACITY = ADD_THIS_UNIT(HARD_WIRED_LIST(I), &
                                                R_YEAR)
            IF(LOADING_TYPE(I) == 'L') THEN
               R_PEAK = R_PEAK - FIRST_YEAR_CAPACITY
            ELSE
               R_ANN_CAP = R_ANN_CAP + FIRST_YEAR_CAPACITY
            ENDIF
            HARD_WIRED_UNITS_ADDED = HARD_WIRED_UNITS_ADDED + 1
         ENDDO
         ADD_HARD_WIRED_UNIT = HARD_WIRED_UNITS_ADDED
      RETURN
!***********************************************************************
      ENTRY PR_ADD_HARD_WIRED_UNIT(R_YEAR) ! INTEGER
!***********************************************************************
         HARD_WIRED_UNITS_ADDED = 0
         DO I = 1, HARD_WIRED_OPTIONS
            IF(HARD_WIRED_ON_LINE_YEAR(I) /= R_YEAR) CYCLE
            FIRST_YEAR_CAPACITY = ADD_THIS_UNIT(HARD_WIRED_LIST(I), &
                                                R_YEAR)
            HARD_WIRED_UNITS_ADDED = HARD_WIRED_UNITS_ADDED + 1
         ENDDO
         PR_ADD_HARD_WIRED_UNIT = HARD_WIRED_UNITS_ADDED
      RETURN
!***********************************************************************
      ENTRY ADD_HARD_WIRED_UNITS_2_NE_PLAN(R_COMMITTED_OPTION_POINTERS, &
                                      R_COMMITTED_OPTION_START_YEAR, &
                                      R_COMMITTED_OPTION_ON_LINE_YR, &
                                      R_COMMITTED_ORG_ON_LINE_YR) ! INTEGER
!***********************************************************************
         HARD_WIRED_UNITS_ADDED = 0
         DO I = 1, HARD_WIRED_OPTIONS
            J = HARD_WIRED_LIST(I)
            R_COMMITTED_OPTION_POINTERS(I) = J
            R_COMMITTED_OPTION_START_YEAR(I) = &
                               HARD_WIRED_ON_LINE_YEAR(I) - LEAD_TIME(J)
            R_COMMITTED_OPTION_ON_LINE_YR(I)=HARD_WIRED_ON_LINE_YEAR(I)
            R_COMMITTED_ORG_ON_LINE_YR(I) = HARD_WIRED_ON_LINE_YEAR(I)
            HARD_WIRED_UNITS_ADDED = HARD_WIRED_UNITS_ADDED + 1
         ENDDO
         ADD_HARD_WIRED_UNITS_2_NE_PLAN = HARD_WIRED_UNITS_ADDED
      RETURN
!***********************************************************************
      ENTRY ADD_ALL_HARD_WIRED_UNITS     ! INTEGER
!***********************************************************************
         HARD_WIRED_UNITS_ADDED = 0
         DO I = 1, HARD_WIRED_OPTIONS
            J = HARD_WIRED_LIST(I)
            FIRST_YEAR_CAPACITY = ADD_THIS_UNIT(J, &
                                             HARD_WIRED_ON_LINE_YEAR(I))
            IF(FIRST_YEAR_CAPACITY >  0.) THEN
               HARD_WIRED_UNITS_ADDED = HARD_WIRED_UNITS_ADDED + 1
               IF(FILE_SOURCE(J) == 'CL') THEN
                  VOID_INT2 = INCREMENT_HARDWIRED_CL_UNITS()
               ELSEIF(FILE_SOURCE(J) == 'EL') THEN
                  VOID_INT2 = INCREMENT_HARDWIRED_EL_UNITS()
!              ELSEIF(FILE_SOURCE(J) == 'LM') THEN
!
! BECAUSE LM AND CT CAN ONLY HAVE 1 FOR THE ANNUAL CUMMULATIVE UNITS
!  TO BUILDS, THE FOLLOWING ROUTINES ARE NOT NEEDED. 4/18/94 MSG
!
!
!              ELSEIF(FILE_SOURCE(J) == 'CT') THEN
!
               ENDIF
            ENDIF
         ENDDO
         ADD_ALL_HARD_WIRED_UNITS = HARD_WIRED_UNITS_ADDED
      RETURN
!***********************************************************************
      ENTRY RETURN_CL_OPTIONS_NO()  ! INTEGER
!***********************************************************************
         RETURN_CL_OPTIONS_NO = CL_OPTIONS
      RETURN
!***********************************************************************
      ENTRY INITIALIZE_SCREEN_DATA(MaxProductionProinter) ! INTEGER
!***********************************************************************
! 7/28/00. FROM TOTAL_ALL_OPTIONS TO TOTAL_ACTIVE_OPTIONS
         INITIALIZE_SCREEN_DATA = TOTAL_ACTIVE_OPTIONS
         MaxProductionProinter = MAXVAL(PRODUCTION_DATA_POINTER)
         IF(CL_OPTIONS > 0) THEN
            SUCCESFUL_READ = READ_CL_SCREEN_DATA(PRODUCTION_DATA_POINTER,FILE_SOURCE,CL_OPTIONS,TOTAL_ALL_OPTIONS, &
                                    IS_A_HARD_WIRED_UNIT)
         ENDIF

      RETURN
!***********************************************************************
      ENTRY RETURN_SCREEN_CAP_COST(R_POINTER,R_YEAR)
!***********************************************************************
         IF(ALLOCATED(CCR_FOR_SCREENING)) THEN
            RETURN_SCREEN_CAP_COST = CCR_FOR_SCREENING(R_POINTER) * CONSTRUCTION_COSTS(R_POINTER,R_YEAR+BASE_YEAR) * &
                       COMPOUND_ESC(R_POINTER)
         ELSE
            RETURN_SCREEN_CAP_COST = 0.0
         ENDIF
      RETURN
!***********************************************************************
      ENTRY RETURN_SCREEN_OP_LIFE(R_POINTER,R_YEAR)
!***********************************************************************
         IF(ALLOCATED(OPERATION_LIFE)) THEN
            IF(R_POINTER < 1 .OR. R_POINTER > TOTAL_ALL_OPTIONS) THEN
               WRITE(4,*) 'In GRX, when accessing op life, index out of'
               WRITE(4,*) 'range. Pointer = ',R_POINTER,R_YEAR
            ENDIF
            RETURN_SCREEN_OP_LIFE = OPERATION_LIFE(R_POINTER)
         ELSE
            RETURN_SCREEN_OP_LIFE = 0.0
         ENDIF
      RETURN
!***********************************************************************
      ENTRY RETURN_RISK_ADJ_S_CAP_COST(R_POINTER,R_YEAR)
!***********************************************************************
         RETURN_RISK_ADJ_S_CAP_COST = (CCR_FOR_SCREENING(R_POINTER) + MRX_RISK_ADJUSTMENT(R_POINTER)) * &
                       CONSTRUCTION_COSTS(R_POINTER,R_YEAR+BASE_YEAR) * COMPOUND_ESC(R_POINTER)
      RETURN
!***********************************************************************

!***********************************************************************
      ENTRY GET_SCREEN_CAPACITY(R_POINTER)  ! INTEGER
!***********************************************************************
         GET_SCREEN_CAPACITY = UNIT_CAP(R_POINTER)
      RETURN
!***********************************************************************
      ENTRY RETURN_TOTAL_ALL_OPTIONS() ! INTEGER
!***********************************************************************
         RETURN_TOTAL_ALL_OPTIONS = TOTAL_ALL_OPTIONS
      RETURN
!***********************************************************************
      ENTRY RETURN_CN_OP_LIFE(R_POINTER,R_YEAR)
!***********************************************************************
         IF(ALLOCATED(OPERATION_LIFE)) THEN
            IF(R_POINTER < 1 .OR. R_POINTER > TOTAL_ALL_OPTIONS) THEN
               WRITE(4,*) 'In GRX, when accessing op life, index out of'
               WRITE(4,*) 'range. Pointer = ',R_POINTER,R_YEAR
               er_message='Stop requested from CN_OBJT SIID37'
               call end_program(er_message)
            ENDIF
            RETURN_CN_OP_LIFE = OPERATION_LIFE(R_POINTER)
         ELSE
            RETURN_CN_OP_LIFE = 0.0
         ENDIF
      RETURN
!********************************************************************
      ENTRY GET_NUMBER_OF_CL_ADDITIONS(R_YEAR) ! INTEGER
!********************************************************************
         GET_NUMBER_OF_CL_ADDITIONS = 0
         IF(R_YEAR <= STUDY_PERIOD .AND. &
                                   ALLOCATED(SAVE_NO_CL_ADDITIONS)) THEN
            GET_NUMBER_OF_CL_ADDITIONS = &
                                     FLOAT(SAVE_NO_CL_ADDITIONS(R_YEAR))
         ENDIF
      RETURN
!********************************************************************
      ENTRY GET_NUMBER_OF_EL_ADDITIONS(R_YEAR) ! INTEGER
!********************************************************************
         GET_NUMBER_OF_EL_ADDITIONS = 0
         IF(R_YEAR <= STUDY_PERIOD .AND. &
                                   ALLOCATED(SAVE_NO_EL_ADDITIONS)) THEN
            GET_NUMBER_OF_EL_ADDITIONS = &
                                     FLOAT(SAVE_NO_EL_ADDITIONS(R_YEAR))
         ENDIF
      RETURN

!***********************************************************************
      ENTRY RETURN_ACTIVE_OPTIONS_LIST(R_YEAR, &
                                       R_OPTION_POINTR, &
                                       R_OPTION_CAP, &
                                       R_OPTION_ANN_UNITS, &
                                       R_OPTION_AVAIL_UNITS, &
                                       R_OPTION_LOADING_TYPE)
!     +                                 R_OPTION_MAX_CAPACITY)  ! INTEGER
!***********************************************************************
!
         J = 0
         CURRENT_YEAR = BASE_YEAR + R_YEAR
         DO I = 1, TOTAL_ALL_OPTIONS
            IF(.NOT. IS_A_HARD_WIRED_UNIT(I) .AND. YES_OR_NO_FILLER(I) /= 'Y' .AND. &
                 ANNUAL_UNITS(I) > 0 .AND. CUMULATIVE_UNITS(I) > 0 .AND. FIRST_YEAR_AVAILABLE(I) <= CURRENT_YEAR .AND. &
                        LAST_YEAR_AVAILABLE(I) >= CURRENT_YEAR .AND. R_YEAR > LEAD_TIME(I)) THEN
               IF(CONTROLS_DEPEND(RESOURCE_ID_NUM(I))) THEN
                  DEPENDED_UNIT_FOUND = .FALSE.
!
! ALL UNITS WHICH REFERENCE THE DEPENDENT UNIT MUST BE ON-LINE BEFORE
! THE DEPENDENT UNIT CAN BE AN OPTION. ALSO IT IS ASSUMED THAT A
! REFERENCED UNIT AND A DEPENDENT UNIT CANNOT COME ON LINE IN THE SAME
! YEAR.
!
                  DO L = 1, TOTAL_ALL_OPTIONS
                     IF(MUT_EXC_UNIT(L) == RESOURCE_ID_NUM(I)) THEN
                        DEPENDED_UNIT_FOUND = HAS_BEEN_SELECTED(L)
                     ENDIF
                  ENDDO
                  IF(.NOT. DEPENDED_UNIT_FOUND) CYCLE
               ENDIF
               J = J + 1
               R_OPTION_POINTR(J) = I
               R_OPTION_CAP(J) = UNIT_CAP(I)
               R_OPTION_ANN_UNITS(J) = ANNUAL_UNITS(I)
               R_OPTION_AVAIL_UNITS(J) = CUMULATIVE_UNITS(I)
               R_OPTION_LOADING_TYPE(J) = LOADING_TYPE(I)
            ENDIF
         ENDDO
         RETURN_ACTIVE_OPTIONS_LIST = J
      RETURN
!***********************************************************************
      ENTRY DSM_RESET_OPTIONS   ! INTEGER
!***********************************************************************
         DSM_RESET_OPTIONS = DSM_ADDITIONS > 0
         IF(DSM_RESET_OPTIONS) THEN
            CALL RESET_LM_ADDITIONS(ADDED_LM_PROGRAMS,DSM_ADDITIONS)
            ADDED_LM_PROGRAMS = -1
         ELSE
            CALL RESET_LM_COSTS
         ENDIF
         DSM_ADDITIONS = 0
      RETURN
!***********************************************************************
      ENTRY CONTRACT_RESET_OPTIONS  ! INTEGER
!***********************************************************************
         CONTRACT_RESET_OPTIONS = CONTRACT_ADDITIONS > 0
         IF(CONTRACT_RESET_OPTIONS) THEN
            CONTRACT_ADDITIONS = RESET_CONTRACT_ADDITIONS(ADDED_CONTRACT_PROGRAMS,CONTRACT_ADDITIONS)
            ADDED_CONTRACT_PROGRAMS = -1
         ELSE
            CONTRACT_ADDITIONS = RESET_CONTRACT_COSTS()
         ENDIF
      RETURN
!***********************************************************************
      ENTRY GET_GRX_RESOURCE_LINK_ID(R_POINTER)
!***********************************************************************
         GET_GRX_RESOURCE_LINK_ID = GRX_RESOURCE_LINK_ID(R_POINTER)
         IF(GET_GRX_RESOURCE_LINK_ID < 1) &
                  GET_GRX_RESOURCE_LINK_ID = -1 ! DOESN'T EXIST
      RETURN
!***********************************************************************
      ENTRY GET_PRODUCTION_DATA_POINTER(R_POINTER) ! INTEGER
!***********************************************************************
         GET_PRODUCTION_DATA_POINTER = &
                                      PRODUCTION_DATA_POINTER(R_POINTER)
      RETURN
!***********************************************************************
      ENTRY GET_OPTION_LEAD_TIME(R_POINTER) ! INTEGER
!***********************************************************************
         GET_OPTION_LEAD_TIME = LEAD_TIME(R_POINTER)
      RETURN
!***********************************************************************
      ENTRY OVER_CAPACITY_TABLE_NUM(R_POINTER) ! INTEGER
!***********************************************************************
         OVER_CAPACITY_TABLE_NUM = OVER_CAPACITY_TABLE(R_POINTER)
      RETURN
!***********************************************************************
      ENTRY UNDER_CAPACITY_TABLE_NUM(R_POINTER) ! INTEGER
!***********************************************************************
         UNDER_CAPACITY_TABLE_NUM = UNDER_CAPACITY_TABLE(R_POINTER)
      RETURN
!***********************************************************************
!
! THRESHOLD_CAPACITY LOGIC
!
      ENTRY NUMBER_OF_THRESHOLD_UNITS_LEFT(R_OPTION,R_YEAR) ! INTEGER
!***********************************************************************
         IF(MAX_UNIT_CAPACITY(R_OPTION) <= UNIT_CAP(R_OPTION)) THEN
            NUMBER_OF_THRESHOLD_UNITS_LEFT = 9999 ! NOT A THRESHOLD CONSTRAINT
         ELSE
            NUMBER_OF_THRESHOLD_UNITS_LEFT =       & !  IS A THRESHOLD CONSTRAINT
                  INT( (MAX_UNIT_CAPACITY(R_OPTION)- THRESHOLD_CAPACITY(R_OPTION,R_YEAR))/UNIT_CAP(R_OPTION) )
         ENDIF
         IF(NUMBER_OF_THRESHOLD_UNITS_LEFT < 1) THEN
            WRITE(4,*) "THRESHOLD CAPACITY COUNT IS OFF FOR OPTION",R_OPTION,"IN YEAR ",R_YEAR+BASE_YEAR
            WRITE(4,*) "  "
         ENDIF
      RETURN
!
      ENTRY UPDATE_THRESHOLD(R_OPTION,R_YEAR)
         IF(MAX_UNIT_CAPACITY(R_OPTION) <= UNIT_CAP(R_OPTION) .OR. UNIT_CAP(R_OPTION) == 0) THEN
            UPDATE_THRESHOLD = 0
         ELSE
!
            DO I_YEAR = R_YEAR, R_YEAR + OPERATION_LIFE(R_OPTION)
               THRESHOLD_CAPACITY(R_OPTION,I_YEAR) =  & !  ADD TO THRESHOLD CAPACITY
                THRESHOLD_CAPACITY(R_OPTION,I_YEAR) + UNIT_CAP(R_OPTION)
            ENDDO
            UPDATE_THRESHOLD = INT(THRESHOLD_CAPACITY(R_OPTION,R_YEAR)/UNIT_CAP(R_OPTION))
         ENDIF
      RETURN
!
      ENTRY THRESHOLD_EXCEEDED(R_OPTION,R_YEAR)
         IF(MAX_UNIT_CAPACITY(R_OPTION) <= UNIT_CAP(R_OPTION)) THEN
            THRESHOLD_EXCEEDED = .TRUE. ! NO NEED TO RESET
         ELSEIF(THRESHOLD_CAPACITY(R_OPTION,R_YEAR) + UNIT_CAP(R_OPTION) > MAX_UNIT_CAPACITY(R_OPTION)) THEN
            THRESHOLD_EXCEEDED = .TRUE.

         ENDIF
      RETURN
!***********************************************************************
!
! LOGICAL SECTION
!
!***********************************************************************
      ENTRY CAPACITY_TYPE_EXISTS(R_ICAP) ! LOGICAL
!***********************************************************************
         IF(R_ICAP == 1) CAPACITY_TYPE_EXISTS = BASE_OPTIONS > 0
         IF(R_ICAP == 2) CAPACITY_TYPE_EXISTS = CYCLE_OPTIONS > 0
         IF(R_ICAP == 3) CAPACITY_TYPE_EXISTS = PEAK_OPTIONS > 0
      RETURN
!***********************************************************************
      ENTRY CAPACITY_PLANNING_ACTIVE  ! LOGICAL
!***********************************************************************
         CAPACITY_PLANNING_ACTIVE = ALL_OPTIONS_GT_ZERO .AND. .NOT. SP_CAPEX_ACTIVE()
      RETURN
!***********************************************************************
      ENTRY POSSIBLE_OPTIONS_EXIST ! LOGICAL
!***********************************************************************
         POSSIBLE_OPTIONS_EXIST = OPTIONS_GT_ZERO
      RETURN
!
!***********************************************************************
      ENTRY GET_OPTION_NAMES(R_OPTION_NAME, &
                             R_OPTION_POSITION, &
                             R_TOTAL_ALL_OPTIONS) ! LOGICAL
!***********************************************************************
         J = 1
         DO I = 1, TOTAL_ALL_OPTIONS
            IF(IS_A_HARD_WIRED_UNIT(I)) CYCLE
            R_OPTION_NAME(J) = unit_names(I)
            R_OPTION_POSITION(J) = I ! RESOURCE_ID_NUM(I)
            IF(J >= R_TOTAL_ALL_OPTIONS) EXIT
            J = J+1
         ENDDO
         GET_OPTION_NAMES = .TRUE.
      RETURN
!***********************************************************************
      ENTRY ADD_OPTIM_UNIT(R_POINTER,R_YEAR,R_ANN_CAP,R_PEAK)  ! LOGICAL
!***********************************************************************
         A_POINTER = ACTIVE_OPTION_LIST(R_POINTER)
         IF(CUMULATIVE_UNITS(A_POINTER) > 0) THEN
            FIRST_YEAR_CAPACITY = ADD_THIS_UNIT(A_POINTER,R_YEAR)
            IF(LOADING_TYPE(A_POINTER) == 'L') THEN
               R_PEAK = R_PEAK - FIRST_YEAR_CAPACITY
            ELSE
               R_ANN_CAP = R_ANN_CAP + FIRST_YEAR_CAPACITY
            ENDIF
            ADD_OPTIM_UNIT = FIRST_YEAR_CAPACITY > 0.
         ELSE
            ADD_OPTIM_UNIT = .FALSE.
         ENDIF
      RETURN
!***********************************************************************
      ENTRY RESOURCE_AVAILABLE(R_POINTER,R_YEAR) ! LOGICAL
!***********************************************************************
         IF(R_YEAR >= BASE_YEAR + LEAD_TIME(R_POINTER) .AND. &
                  R_YEAR - LEAD_TIME(R_POINTER) <= LAST_STUDY_YEAR .AND. .NOT. IS_A_HARD_WIRED_UNIT(R_POINTER)) THEN
            RESOURCE_AVAILABLE = .TRUE.
         ELSE
            RESOURCE_AVAILABLE = .FALSE.
         ENDIF
      RETURN
!***********************************************************************
      ENTRY CALC_SCREEN_SLOPE_INTERCEPT(R_POINTER,R_YEAR,R_SLOPE,R_INTERCEPT) ! LOGICAL
!***********************************************************************
         R_SLOPE = 9999.
         R_INTERCEPT = 9999.
         CALC_SCREEN_SLOPE_INTERCEPT = .FALSE.
         IF(FILE_SOURCE(R_POINTER) == 'CL') THEN
            IF(PRODUCTION_DATA_POINTER(R_POINTER) > 0) THEN
! 3/17/99. GAT. R_INTERCEPT IS NOW $'S.
! 08/05/04. REMOVED THE YEAR INDEX.
               R_INTERCEPT = RETURN_CL_SCREEN_FIXED_COST(PRODUCTION_DATA_POINTER(R_POINTER))

               R_SLOPE = RETURN_CL_SCREEN_VARI_COST(PRODUCTION_DATA_POINTER(R_POINTER),R_YEAR)
               IF(INVESTMENT_DATA_POINTER(R_POINTER) > 0) THEN
!
                  SCREEN_CAP_COST = 1000000. * (CCR_FOR_SCREENING(R_POINTER) + MRX_RISK_ADJUSTMENT(R_POINTER)) * &
                          CONSTRUCTION_COSTS(R_POINTER,R_YEAR+BASE_YEAR)

                  R_INTERCEPT = R_INTERCEPT + SCREEN_CAP_COST
!
               ENDIF
            ENDIF
         ELSEIF(FILE_SOURCE(R_POINTER) == 'EL') THEN
         ELSEIF(FILE_SOURCE(R_POINTER) == 'LM') THEN
         ELSEIF(FILE_SOURCE(R_POINTER) == 'CT') THEN
         ENDIF
         CALC_SCREEN_SLOPE_INTERCEPT = .TRUE.
      RETURN
!***********************************************************************
      ENTRY GET_NEXT_ADDITION(R_POINTER,CAP_TYPE,R_CURRENT_YEAR,START_POINTER,R_AVAILABLE_LEAD_TIME,R_POINTR) ! LOGICAL
!***********************************************************************
         FOUND_NEXT_ADDITION = .FALSE.
         CHECK_OPTIONS_LIST = .TRUE.
         DO WHILE( .NOT. FOUND_NEXT_ADDITION .AND. CHECK_OPTIONS_LIST)
            IF(R_POINTER < START_POINTER) THEN
               FIRST_POINTER = 1
               LAST_POINTER=MIN(CURRENT_ACTIVE_OPTIONS,START_POINTER- INT(1,2))
            ELSE
               FIRST_POINTER = R_POINTER
               LAST_POINTER = CURRENT_ACTIVE_OPTIONS
            ENDIF
            DO I = FIRST_POINTER, LAST_POINTER
               A_POINTER = ACTIVE_OPTION_LIST(I)
               IF(LOADING_TYPE(A_POINTER) /= CAP_TYPE .OR. R_AVAILABLE_LEAD_TIME <= LEAD_TIME(A_POINTER) .OR. &
                       CUMULATIVE_UNITS(A_POINTER) <= 0 .OR. R_CURRENT_YEAR < FIRST_YEAR_AVAILABLE(A_POINTER) .OR. &
                           R_CURRENT_YEAR > LAST_YEAR_AVAILABLE(A_POINTER) .OR. YES_OR_NO_FILLER(A_POINTER) == 'Y') CYCLE

! 2/8/94. GAT.
               IF(ANNUAL_UNITS_LEFT(A_POINTER) > 0 .AND. CUMULATIVE_UNITS(A_POINTER) > 0) THEN
                  R_POINTR = A_POINTER
                  R_POINTER = I
                  FOUND_NEXT_ADDITION = .TRUE.
                  EXIT
               ENDIF
            ENDDO
            IF( .NOT. FOUND_NEXT_ADDITION) THEN
               IF(R_POINTER > 1 .AND. R_POINTER >= START_POINTER) THEN
                  R_POINTER = 1
               ELSE
                  CHECK_OPTIONS_LIST = .FALSE.
               ENDIF
            ENDIF
         ENDDO
         GET_NEXT_ADDITION = FOUND_NEXT_ADDITION
      RETURN
!***********************************************************************
      ENTRY TEST_START_STOP_YEARS(R_POINTER,R_CURRENT_YEAR)  ! LOGICAL
!***********************************************************************
         TEST_START_STOP_YEARS = .FALSE.
         IF(CUMULATIVE_UNITS(R_POINTER) > 0 .AND. R_CURRENT_YEAR >= FIRST_YEAR_AVAILABLE(R_POINTER) .AND. R_CURRENT_YEAR <= &
                                   LAST_YEAR_AVAILABLE(R_POINTER)) THEN
            TEST_START_STOP_YEARS = .TRUE.
         ENDIF
      RETURN
!***********************************************************************
      ENTRY WRITE_TO_EXPANSION_REPORT(L_POINTER,L_YEAR) ! LOGICAL
!***********************************************************************
         W_YEAR = L_YEAR
         W_POINTER = L_POINTER
         WRITE(9,"(1X,I4,5X,I4,4X,I4,2X,A,2X,A,1X,F9.1,F7.0)") END_POINT,W_YEAR,W_YEAR-LEAD_TIME(W_POINTER),'Addition', &
                                   unit_names(W_POINTER),CONSTRUCTION_COSTS(W_POINTER,YR1),UNIT_CAP(W_POINTER)
         WRITE_TO_EXPANSION_REPORT = .TRUE.
      RETURN
!***********************************************************************
      ENTRY WRITE_2_EXPANSION_PATTERN_REPT(L_POINTER,R_RESOUCE_STATUS) ! LOGICAL
!***********************************************************************
         IF(EXPANSION_PATTERN_RPT_NOT_OPEN) THEN
            EXPANSION_PATTERN_NO = EXPANSION_PATTERN_HEADER(EXPANSION_PATTERN_REC)
            EXPANSION_PATTERN_RPT_NOT_OPEN = .FALSE.
         ENDIF
         TOTAL_UNITS_ADDED(L_POINTER) = TOTAL_UNITS_ADDED(L_POINTER) + 1
         unit_name = trim(unit_names(L_POINTER))//' '//LEFT_JUSTIFY_I2_IN_STR(TOTAL_UNITS_ADDED(L_POINTER))
         unit_name = trim(unit_name)//' '//R_RESOUCE_STATUS
         REAL_CURRENT_YEAR = FLOAT(BASE_YEAR+YEAR)
         WRITE(EXPANSION_PATTERN_NO,REC=EXPANSION_PATTERN_REC) &
                                  PRT_ENDPOINT(), &
                                  REAL_CURRENT_YEAR, &
                                  REAL_CURRENT_YEAR- &
                                            FLOAT(LEAD_TIME(L_POINTER)), &
                                  unit_name, &
                                  REAL_CURRENT_YEAR- &
                                            FLOAT(LEAD_TIME(L_POINTER)), &
                                  REAL_CURRENT_YEAR, &
                                  CONSTRUCTION_COSTS(L_POINTER,YR1), &
                                  UNIT_CAP(L_POINTER)
         EXPANSION_PATTERN_REC = EXPANSION_PATTERN_REC + 1
         WRITE_2_EXPANSION_PATTERN_REPT = .TRUE.
      RETURN
!***********************************************************************
!
! REAL SECTION
!
! TODO:  Find block close and Fix/remove
!$if defined(test_it)
!***********************************************************************
      ENTRY DEPENDENT_UNIT_AVAILABLE(R_P)
!***********************************************************************
        DEPENDENT_UNIT_AVAILABLE = .TRUE.
        IF(POINTER_TO_DEPENDENT_UNIT(R_P) > 0) THEN
! NOTE DOUBLE INDEX
           I = POINTER_TO_DEPENDENT_UNIT(R_P)
           I = DEPEND_RESOURCE_ID_2_I(I)
           IF(INDEPENDENT_UNIT_STATUS(I) < 1) THEN
              DEPENDENT_UNIT_AVAILABLE = .FALSE.
           ENDIF
        ENDIF
      RETURN
!***********************************************************************
      ENTRY ADD_THIS_UNIT_2_CAPACITY(R_POINTER,R_YEAR)  ! REAL
!***********************************************************************
         IF(R_YEAR >= BASE_YEAR + LEAD_TIME(R_POINTER) .AND. R_YEAR - LEAD_TIME(R_POINTER) <= LAST_STUDY_YEAR) THEN
!
! DON'T RETIRE UNITS IN THE EXTENSION PERIOD
!
            IF(R_YEAR+OPERATION_LIFE(R_POINTER) > LAST_STUDY_YEAR  .OR. MUT_EXC_UNIT(R_POINTER) > 0) THEN
               R_OPERATION_LIFE = 99
            ELSE
               R_OPERATION_LIFE = OPERATION_LIFE(R_POINTER)
            ENDIF
            IF(FILE_SOURCE(R_POINTER) == 'CL') THEN
               CL_UNIT_POSITION = ADD_NEW_CL_UNIT(R_YEAR,R_OPERATION_LIFE,PRODUCTION_DATA_POINTER(R_POINTER),ON_LINE_MONTH, &
                                   PRIMARY_ASSET_CLASS(R_POINTER),PRIMARY_ALLOCATION_VECTOR(R_POINTER),RESOURCE_ID_NUM(R_POINTER), &
                                   R_POINTER)
               IF(CL_UNIT_POSITION > 0) THEN
                  CAPACITY_ADDED = CL_CAPACITY_PLANNING_ADJ(R_YEAR,CL_UNIT_POSITION,2,.FALSE.)
                  HAS_BEEN_SELECTED(R_POINTER) = .TRUE.
                  AVAILABLE_UNITS = INCREMENT_AVAILABLE_CL_UNITS()
! GAT. 2/9/94. ATTEMPT TO TAKE CARE OF HARD-WIRED/MUTUALLY EXCLUSIVE PROBLEM
                  IF(MUT_EXC_UNIT(R_POINTER) > 0) THEN
                     J = 0
                     DO
                        J = J + 1
                        IF(J > HARD_WIRED_OPTIONS) THEN
                           DEPEND_UNIT_NO(R_POINTER) = AVAILABLE_UNITS
                           EXIT
                        ENDIF
                        IF(MUT_EXC_UNIT(R_POINTER) /= RESOURCE_ID_NUM(HARD_WIRED_LIST(J))) CYCLE
                        MUT_EXCLUSIVE_CAPACITY_RETIRED = CL_CAPACITY_PLANNING_ADJ(HARD_WIRED_ON_LINE_YEAR(J), &
                                             AVAILABLE_UNITS,2,.TRUE.)
                     ENDDO
                  ENDIF
                  IF(CONTROLS_DEPEND(RESOURCE_ID_NUM(R_POINTER))) THEN
                     DEPENDED_UNIT_FOUND = .FALSE.
                     DO I = 1, TOTAL_ALL_OPTIONS
                        IF(DEPEND_UNIT_NO(I) > 0 .AND.MUT_EXC_UNIT(I)== RESOURCE_ID_NUM(R_POINTER)) THEN
                           MUT_EXCLUSIVE_CAPACITY_RETIRED = CL_CAPACITY_PLANNING_ADJ(R_YEAR,DEPEND_UNIT_NO(I),2,.TRUE.)
                           DEPEND_UNIT_NO(I) = 0
                           DEPENDED_UNIT_FOUND = .TRUE.
                        ENDIF
                     ENDDO
                     IF(.NOT. DEPENDED_UNIT_FOUND .AND. .NOT. IS_A_HARD_WIRED_UNIT(R_POINTER)) THEN
                        WRITE(4,"(1X,A,I4,A,I4)") 'No depended units found for unit'//trim(unit_names(R_POINTER))// &
                                    'while runing endpoint number',END_POINT,' in year',R_YEAR
                     ENDIF
                  ENDIF
               ENDIF
            ELSEIF(FILE_SOURCE(R_POINTER) == 'EL') THEN
               CAPACITY_ADDED = ADD_NEW_EL_UNIT(R_YEAR, &
                                   PRODUCTION_DATA_POINTER(R_POINTER), &
                                   R_OPERATION_LIFE, &
                                   ON_LINE_MONTH, &
                                   PRIMARY_ASSET_CLASS(R_POINTER), &
                                   PRIMARY_ALLOCATION_VECTOR(R_POINTER))
               IF(CAPACITY_ADDED /= 0.) THEN
                  CAPACITY_ADDED = EL_CAPACITY_PLANNING_ADDITIONS(2)
                  AVAILABLE_UNITS = INCREMENT_AVAILABLE_EL_UNITS()
                  HAS_BEEN_SELECTED(R_POINTER) = .TRUE.
                  IF(MUT_EXC_UNIT(R_POINTER) > 0) &
                             DEPEND_UNIT_NO(R_POINTER) = AVAILABLE_UNITS
                  IF(CONTROLS_DEPEND(RESOURCE_ID_NUM(R_POINTER))) THEN
                     DEPENDED_UNIT_FOUND = .FALSE.
                     DO I = 1, TOTAL_ALL_OPTIONS
                        IF(DEPEND_UNIT_NO(I) > 0 .AND.MUT_EXC_UNIT(I)== RESOURCE_ID_NUM(R_POINTER)) THEN
                           MUT_EXCLUSIVE_CAPACITY_RETIRED = EL_CAPACITY_PLANNING_REMOVALS(R_YEAR,DEPEND_UNIT_NO(I),2)
                           DEPEND_UNIT_NO(I) = 0
                           DEPENDED_UNIT_FOUND = .TRUE.
                        ENDIF
                     ENDDO
                     IF(.NOT. DEPENDED_UNIT_FOUND) THEN
                        WRITE(4,"(1X,A,I4,A,I4)") 'No depended units found for unit'//trim(unit_names(R_POINTER))// &
                                    'while runing endpoint number',END_POINT,' in year',R_YEAR
                     ENDIF
                  ENDIF
               ENDIF
            ELSEIF(FILE_SOURCE(R_POINTER) == 'LM') THEN
               CALL ADD_NEW_LM_UNIT(R_YEAR,PRODUCTION_DATA_POINTER(R_POINTER),R_OPERATION_LIFE,.TRUE.)
!
               CALL LM_CAPACITY_PLANNING_ADDITIONS(PRODUCTION_DATA_POINTER(R_POINTER),CAPACITY_ADDED)
            ELSEIF(FILE_SOURCE(R_POINTER) == 'CT') THEN
               CAPACITY_ADDED = ADD_NEW_CT_UNIT(R_YEAR,PRODUCTION_DATA_POINTER(R_POINTER),R_OPERATION_LIFE)
               CAPACITY_ADDED = CT_CAPACITY_PLANNING_ADDITIONS(PRODUCTION_DATA_POINTER(R_POINTER))
            ENDIF
            CUMULATIVE_UNITS(R_POINTER) = MAX(CUMULATIVE_UNITS(R_POINTER) - 1,0)
            ADD_THIS_UNIT_2_CAPACITY = CAPACITY_ADDED
         ELSE
            ADD_THIS_UNIT_2_CAPACITY = 0.
         ENDIF
      RETURN
! TODO:  Get rid of if not needed
!
!***********************************************************************
      ENTRY ADD_THIS_FINANCIAL_INVESTMENT(R_POINTER,R_YEAR,R_I,R_CURRENT_YEAR) ! REAL
!***********************************************************************
!
! ADD THE FINANCIAL DATA
!
            ANN_DECOMP_ACTIVE = CHECK_CAP_PLANNING_SWTCHES() == 'A'
            IF(INVESTMENT_DATA_POINTER(R_POINTER) > 0) THEN
!
! ONLY CL AND EL NEED FINANCIAL RECORDS CT AND DSM ARE SELF CONTAINED
!
               IF(FILE_SOURCE(R_POINTER) == 'CL' .OR. &
                                  FILE_SOURCE(R_POINTER) == 'EL') THEN
                  IN_SERVICE_YR = R_YEAR
!
                  POINTER = PRODUCTION_DATA_POINTER(R_POINTER)
                  IF(POINTER > 0) THEN
                     IF(FILE_SOURCE(R_POINTER) == 'CL') THEN
                        READ(13,REC=POINTER) DELETE,CL_UNIT_NAME, &
                                             CL_LOAD_TYPE, &
                                             EXPENSE_ASSIGNMENT, &
                                             EXPENSE_COLLECTION, &
                                             GENGRP,CAP_FRAC_OWN, &
                                             ON_LINE_MONTH
                     ELSEIF(FILE_SOURCE(R_POINTER) == 'EL') THEN
                        ON_LINE_MONTH = &
                                   RETURN_EL_UNIT_ON_LINE_MONTH(POINTER)
                     ENDIF
                  ELSE
                     ON_LINE_MONTH = 7
                  ENDIF
                  CALL ADD_NEXT_UNIT_FINANCIAL(IN_SERVICE_YR, &
                                   ON_LINE_MONTH, &
                                   OPERATION_LIFE(R_POINTER), &
                                   LEAD_TIME(R_POINTER), &
                                   INVESTMENT_DATA_POINTER(R_POINTER), &
                                   ESCALATION_VECTOR(R_POINTER), &
                                   CONSTRUCTION_COSTS(R_POINTER,R_YEAR), &
                                   FILE_SOURCE(R_POINTER), &
                                   PRIMARY_ASSET_CLASS(R_POINTER), &
                                   PRIMARY_ALLOCATION_VECTOR(R_POINTER), &
                            OVN_CAPITAL_COST_STOCASTIC(R_POINTER), &
                                   PERCENT_COMPLETED,R_I, &
                                   R_CURRENT_YEAR)
!
                  IF(ANN_DECOMP_ACTIVE) &
                                       CALL INC_PROJECTS_IN_PROCESS_RECS
               ENDIF
               ADD_THIS_FINANCIAL_INVESTMENT = PERCENT_COMPLETED
            ELSE
               ADD_THIS_FINANCIAL_INVESTMENT = 0.
            ENDIF
      RETURN
!***********************************************************************
      ENTRY ADJUST_FINANCIAL_INVESTMENT(R_POINTER,R_YEAR,R_I,R_ABANDON_IT,R_CURRENT_YEAR,R_OPTION_TIMES_DELAYED)  ! REAL
!***********************************************************************
!
! ADD THE FINANCIAL DATA
!
         ADJUST_FINANCIAL_INVESTMENT = 0.
         IF(R_I > 0 .AND. (FILE_SOURCE(R_POINTER) == 'CL' .OR. FILE_SOURCE(R_POINTER) == 'EL')) THEN
!
            CALL ADJUST_NEW_UNIT_FINANCIAL(R_I,R_YEAR,PERCENT_COMPLETED,R_ABANDON_IT,R_CURRENT_YEAR,R_OPTION_TIMES_DELAYED)
!
            ADJUST_FINANCIAL_INVESTMENT = PERCENT_COMPLETED
         ENDIF
      RETURN
!***********************************************************************
      ENTRY ADD_UNIT_TEMPORARILY(R_POINTER,R_YEAR)  ! REAL
!***********************************************************************
!
! FOR TESTING THE UNIT DOESN'T RETIRE
!
         R_OPERATION_LIFE = 99
!
         IF(FILE_SOURCE(R_POINTER) == 'CL') THEN
            CL_UNIT_POSITION = ADD_NEW_CL_UNIT(R_YEAR, &
                                   R_OPERATION_LIFE, &
                                   PRODUCTION_DATA_POINTER(R_POINTER), &
                                   ON_LINE_MONTH, &
                                   PRIMARY_ASSET_CLASS(R_POINTER), &
                                   PRIMARY_ALLOCATION_VECTOR(R_POINTER), &
                                   RESOURCE_ID_NUM(R_POINTER), &
                                   R_POINTER)
            CAPACITY_ADDED = FIRST_YEAR_CL_CAPACITY(R_YEAR,CL_UNIT_POSITION)
            IF(CAPACITY_ADDED > 0.) THEN
               IF(CONTROLS_DEPEND(RESOURCE_ID_NUM(R_POINTER))) THEN
                  DEPENDED_UNIT_FOUND = .FALSE.
                  DO I = 1, TOTAL_ALL_OPTIONS
                     IF(DEPEND_UNIT_NO(I) > 0 .AND. MUT_EXC_UNIT(I)== RESOURCE_ID_NUM(R_POINTER)) THEN
                        OFF_LINE_DATE = CL_CAPACITY_TEMP_RETIRE_UNIT(R_YEAR,DEPEND_UNIT_NO(I))
                        DEPENDED_UNIT_FOUND = .TRUE.
                     ENDIF
                  ENDDO
               ENDIF
            ENDIF
         ELSEIF(FILE_SOURCE(R_POINTER) == 'EL') THEN
            CAPACITY_ADDED = ADD_NEW_EL_UNIT(R_YEAR,PRODUCTION_DATA_POINTER(R_POINTER),R_OPERATION_LIFE,ON_LINE_MONTH, &
                                   PRIMARY_ASSET_CLASS(R_POINTER),PRIMARY_ALLOCATION_VECTOR(R_POINTER))
            IF(CAPACITY_ADDED > 0.) THEN
               IF(CONTROLS_DEPEND(RESOURCE_ID_NUM(R_POINTER))) THEN
                  DEPENDED_UNIT_FOUND = .FALSE.
                  DO I = 1, TOTAL_ALL_OPTIONS
                     IF(DEPEND_UNIT_NO(I) > 0 .AND. MUT_EXC_UNIT(I)== RESOURCE_ID_NUM(R_POINTER)) THEN
                        OFF_LINE_DATE = EL_CAPACITY_TEMP_RETIRE_UNIT(R_YEAR,DEPEND_UNIT_NO(I))
                        DEPENDED_UNIT_FOUND = .TRUE.
                     ENDIF
                  ENDDO
               ENDIF
            ENDIF
         ELSEIF(FILE_SOURCE(R_POINTER) == 'LM') THEN
            CALL ADD_NEW_LM_UNIT(R_YEAR,PRODUCTION_DATA_POINTER(R_POINTER),R_OPERATION_LIFE,.FALSE.)
            CAPACITY_ADDED = UNIT_CAP(R_POINTER)
            DSM_ADDITIONS = DSM_ADDITIONS + 1
            ADDED_LM_PROGRAMS(DSM_ADDITIONS) = PRODUCTION_DATA_POINTER(R_POINTER)
         ELSEIF(FILE_SOURCE(R_POINTER) == 'CT') THEN
            CAPACITY_ADDED = ADD_NEW_CT_UNIT(R_YEAR,PRODUCTION_DATA_POINTER(R_POINTER),R_OPERATION_LIFE)
            CAPACITY_ADDED = UNIT_CAP(R_POINTER)
            CONTRACT_ADDITIONS = CONTRACT_ADDITIONS + 1
            ADDED_CONTRACT_PROGRAMS(CONTRACT_ADDITIONS) = PRODUCTION_DATA_POINTER(R_POINTER)
         ENDIF
!
! ADD THE FINANCIAL DATA
!
         IF(INVESTMENT_DATA_POINTER(R_POINTER) > 0) THEN
!
! ONLY CL AND EL NEED FINANCIAL RECORDS CT AND DSM ARE SELF CONTAINED
!
            IF(FILE_SOURCE(R_POINTER) == 'CL' .OR. FILE_SOURCE(R_POINTER) == 'EL') THEN
               IN_SERVICE_YR = R_YEAR
               DO WHILE (IN_SERVICE_YR <= LAST_EXTENSION_YEAR)
                  CALL ADD_NEW_UNIT_FINANCIAL(IN_SERVICE_YR, &
                                   ON_LINE_MONTH, &
                                   OPERATION_LIFE(R_POINTER), &
                                   LEAD_TIME(R_POINTER), &
                                   INVESTMENT_DATA_POINTER(R_POINTER), &
                                   ESCALATION_VECTOR(R_POINTER), &
                                   CONSTRUCTION_COSTS(R_POINTER,R_YEAR), &
                                   FILE_SOURCE(R_POINTER), &
                                   PRIMARY_ASSET_CLASS(R_POINTER), &
                                   PRIMARY_ALLOCATION_VECTOR(R_POINTER), &
                            OVN_CAPITAL_COST_STOCASTIC(R_POINTER))
                  CALL INC_PROJECTS_IN_PROCESS_RECS
                  IN_SERVICE_YR = IN_SERVICE_YR + OPERATION_LIFE(R_POINTER)
               ENDDO
            ENDIF
         ENDIF
         ADD_UNIT_TEMPORARILY = CAPACITY_ADDED
      RETURN
!***********************************************************************
      ENTRY GET_OPTION_CAPACITY(R_POINTER)  ! REAL
!***********************************************************************
         GET_OPTION_CAPACITY = UNIT_CAP(R_POINTER)
      RETURN
!***********************************************************************
!
! CHARACTER SECTION
!
!***********************************************************************
      ENTRY CN_GET_OPTIONS(R_CAPACITY,R_COST,R_POINTER,GET_OPTIONS)
!***********************************************************************
         R_COST = CONSTRUCTION_COSTS(R_POINTER,YR1)
         R_CAPACITY = UNIT_CAP(R_POINTER)
         GET_OPTIONS = unit_names(R_POINTER)
         CN_GET_OPTIONS = .TRUE.
      RETURN
!***********************************************************************
      ENTRY CN_GET_OPTIONS_RES_TYPE(R_FILE_SOURCE,R_POINTER,R_MUT_EXC_UNIT_ACTIVE,GET_OPTIONS_RES_TYPE)
!***********************************************************************
         R_FILE_SOURCE = FILE_SOURCE(R_POINTER)
         R_MUT_EXC_UNIT_ACTIVE = MUT_EXC_UNIT(R_POINTER) > 0
         GET_OPTIONS_RES_TYPE = unit_names(R_POINTER)
         CN_GET_OPTIONS_RES_TYPE = .TRUE.
      RETURN
!***********************************************************************
      ENTRY CN_GET_OPTION_LOADING_TYPE(R_POINTER,GET_OPTION_LOADING_TYPE)
!***********************************************************************
         GET_OPTION_LOADING_TYPE = LOADING_TYPE(R_POINTER)
         CN_GET_OPTION_LOADING_TYPE = .TRUE.
      RETURN
!***********************************************************************
      END
!***********************************************************************
!
! CHARACTER SECTION
!
!***********************************************************************
      FUNCTION GET_OPTIONS(R_CAPACITY,R_COST,R_POINTER) ! CHARACTER
!***********************************************************************
!
      LOGICAL (kind=1) ::  CN_GET_OPTIONS,CN_GET_OPTIONS_RES_TYPE,CN_GET_OPTION_LOADING_TYPE
      CHARACTER (len=20) ::  GET_OPTIONS,GET_OPTIONS_RES_TYPE
      CHARACTER (len=20) ::  R_GET_OPTIONS,R_GET_OPTIONS_RES_TYPE
      CHARACTER (len=2) ::  R_FILE_SOURCE
      CHARACTER (len=1) ::  GET_OPTION_LOADING_TYPE
      CHARACTER (len=1) ::  R_GET_OPTION_LOADING_TYPE
      INTEGER (kind=2) ::  R_POINTER,R_HARD_WIRED_OPTIONS
      LOGICAL (kind=1) ::  VOID_LOGICAL,R_MUT_EXC_UNIT_ACTIVE
      REAL ::  R_COST,R_CAPACITY
!
         VOID_LOGICAL = CN_GET_OPTIONS(R_CAPACITY,R_COST,R_POINTER,R_GET_OPTIONS)
         GET_OPTIONS = R_GET_OPTIONS
      RETURN
!***********************************************************************
      ENTRY GET_OPTIONS_RES_TYPE(R_FILE_SOURCE,R_POINTER,R_MUT_EXC_UNIT_ACTIVE)
!***********************************************************************
         VOID_LOGICAL=CN_GET_OPTIONS_RES_TYPE(R_FILE_SOURCE,R_POINTER,R_MUT_EXC_UNIT_ACTIVE,R_GET_OPTIONS_RES_TYPE)
         GET_OPTIONS_RES_TYPE = R_GET_OPTIONS_RES_TYPE
      RETURN
!***********************************************************************
      ENTRY GET_OPTION_LOADING_TYPE(R_POINTER)  ! CHARACTER
!***********************************************************************
         VOID_LOGICAL = CN_GET_OPTION_LOADING_TYPE(R_POINTER,R_GET_OPTION_LOADING_TYPE)
         GET_OPTION_LOADING_TYPE = R_GET_OPTION_LOADING_TYPE
      RETURN
      END
!***********************************************************************
!
!          OBJECT TO CONVERT DATA FILES TO DIRECT ACESS BINARY
!         COPYRIGHT (C) 1983, 84, 85, 93 M.S. GERBER & ASSOCIATES, INC.
!
!***********************************************************************
!
      SUBROUTINE CR_OBJECT
      use end_routine, only: end_program, er_message
      use SpinDriftLib
      use prod_arrays_dimensions
      USE SIZECOM
      use globecom
!
      INTEGER (kind=2) ::  IREC,INUNIT,DELETE,YR,LRECL=64
      INTEGER ::  UNIT_NO
      INTEGER (kind=4) ::  IOS
      LOGICAL (kind=4) ::  CAP_RATIOS_FILE_EXISTS=.FALSE.
      LOGICAL (kind=1) ::  R_RATIOS_FILE_ACTIVE
      CHARACTER (len=5) ::  CAP_RATIOS_FILE,OVERLAY_FAMILY_NAME
      CHARACTER (len=50) ::  COMMENT
      CHARACTER (len=256) ::  FILE_NAME
      CHARACTER (len=256) ::  BASE_FILE_DIRECTORY,OUTPUT_DIRECTORY
      REAL ::  NEW_VALUES(7),OLD_VALUES(3)
! DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER (len=1024) ::  RECLN
! DECLARATION FOR LOCALS
      CHARACTER (len=15) ::  FILE_TYPE='Capacity Ratios'
      CHARACTER (len=2) ::  CAPACITY_RATIOS_OL='BC'
      LOGICAL (kind=1) ::  LAHEY_LF95
      CHARACTER (len=30) ::  SCREEN_OUTPUT
!
!***********************************************************************
      ENTRY CR_MAKEBIN
!***********************************************************************
!
      FILE_NAME = trim(BASE_FILE_DIRECTORY())//"CRB"//trim(CAP_RATIOS_FILE())//".DAT"
      INQUIRE(FILE=FILE_NAME,EXIST=CAP_RATIOS_FILE_EXISTS)
      IF(CAP_RATIOS_FILE_EXISTS) THEN
         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//CAP_RATIOS_FILE()
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
            CALL MG_LOCATE_WRITE(16,30,CAP_RATIOS_FILE(),ALL_VERSIONS,0)
         ENDIF
         OLD_VALUES(1) = .6
         OLD_VALUES(2) = 1.
         OLD_VALUES(3) = 1.2
         NEW_VALUES(1) = 0.
         NEW_VALUES(2) = 0.
         NEW_VALUES(3) = 0.
         NEW_VALUES(4) = 0.
         NEW_VALUES(5) = 0.
         NEW_VALUES(6) = 999999.
         NEW_VALUES(7) = 999999.
         OPEN(11,FILE=trim(OUTPUT_DIRECTORY())//"BCCAPRTS.BIN",ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
         OPEN(10,FILE=FILE_NAME)
         IREC = 1
         READ(10,*) DELETE
         DO
            READ(10,1000,IOSTAT=IOS) RECLN
            IF(IOS /=0) EXIT
            RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
            READ(RECLN,*,ERR=300) DELETE,YR,OLD_VALUES,COMMENT,NEW_VALUES
            WRITE(11,REC=IREC) YR,OLD_VALUES,NEW_VALUES
            IREC = IREC + 1
            IF(IREC .GT. LRECL) EXIT
         ENDDO
         CLOSE(10)
!
         CLOSE(11)
      ELSE IF(INDEX(CAP_RATIOS_FILE(),'NONE') == 0) THEN
         CALL STOP_NOFILE(FILE_TYPE,FILE_NAME) !'Capacity-Planning Ratios'
      ENDIF
      RETURN
!***********************************************************************
! OVERLAY THE TARGET CAPACITY-RATIOS FILE
      ENTRY CR_MAKEOVL(OVERLAY_FAMILY_NAME)
!***********************************************************************
!
      IF(LAHEY_LF95()) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSE
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         CALL LOCATE(10,51)
      ENDIF
      FILE_NAME = trim(OUTPUT_DIRECTORY())//"CRO"//trim(OVERLAY_FAMILY_NAME)//".DAT"
      OPEN(10,FILE=FILE_NAME)
      READ(10,*) DELETE
      INUNIT = 12
      IF(CAPACITY_RATIOS_OL == 'BC') THEN
         OPEN(11,FILE=trim(OUTPUT_DIRECTORY())//"BCCAPRTS.BIN",ACCESS="DIRECT",RECL=LRECL)
         INUNIT = 11
      ENDIF
      OPEN(12,FILE=trim(OUTPUT_DIRECTORY())//"OLCAPRTS.BIN",ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
      IREC = 0
      DO
         IREC = IREC + 1
         READ(INUNIT,REC=IREC,IOSTAT=IOS) YR,OLD_VALUES,NEW_VALUES
         IF(IOS /= 0) EXIT
         READ(10,1000,IOSTAT=IOS) RECLN
         IF(IOS == 0) THEN
            RECLN = trim(RECLN)//',,,,,,,,,,,,,'
            READ(RECLN,*,ERR=300) DELETE,YR,OLD_VALUES,COMMENT,NEW_VALUES
         ENDIF
         WRITE(12,REC=IREC) YR,OLD_VALUES,NEW_VALUES
      ENDDO
      CLOSE(10)
      CLOSE(12)
      IF(CAPACITY_RATIOS_OL == 'BC') CLOSE(11)
      CAPACITY_RATIOS_OL = 'OL'
      RETURN
!
!
  300 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from CN_OBJT SIID37'
      call end_program(er_message)
!
      ENTRY RESET_CAPACITY_RATIOS_OL
         CAPACITY_RATIOS_OL = 'BC'
      RETURN
!
      ENTRY OPEN_CAPACITY_RATIOS_FILE(UNIT_NO,R_RATIOS_FILE_ACTIVE)
         IF(CAP_RATIOS_FILE_EXISTS) THEN
            OPEN(UNIT_NO,FILE=trim(OUTPUT_DIRECTORY())// &
                                       CAPACITY_RATIOS_OL//"CAPRTS.BIN",ACCESS="DIRECT",RECL=LRECL)
         ELSE
            WRITE(4,'(1X,3A,I4,A)') '*** WARNING *** Capacity Ratios ', &
            'file did not exist.  Expansion planning was not done for ','end point',END_POINT,'.'
         ENDIF
         R_RATIOS_FILE_ACTIVE = CAP_RATIOS_FILE_EXISTS
      RETURN
 1000 FORMAT(A)
 1010 FORMAT('&',A)
      END
!***********************************************************************
!
! RATIOS FUNCTIONS
!
!***********************************************************************
      FUNCTION READ_CAP_RATIOS_FILE()
!
      USE SIZECOM
      use globecom

!
      INTEGER (kind=2) ::  DELETE,I,R_YEAR
      INTEGER (kind=4) ::  IOS,ICAP
      INTEGER (kind=1) ::  J
      LOGICAL (kind=1) ::  READ_CAP_RATIOS_FILE
      LOGICAL (kind=1) ::  RATIOS_FILE_ACTIVE=.FALSE.
      REAL ::  ADJUSTMENT_PEAK,TARGET_RATIO
      REAL ::  ADJUSTMENT_BASE_CAPACITY,ADJUSTMENT_PEAK_CAPACITY
      REAL ::  ADJ_PK2,ADJ_PK1,PLANNING_PEAK_GROWTH_RATE
      REAL ::  MIN_CAPACITY_TESTING_MARGIN,MAX_CAPACITY_TESTING_MARGIN
      REAL ::  TARGETS(:,:),ADJ_BASE_CAP(:),ADJ_PEAK_CAP(:), &
           PEAK_ADJUSTMENTS(:),ADJUSTMENT_CAPACITY, &
           PEAK_GROWTH_RATE(:), &
           MINIMUM_TESTING_RM(:), &
           MAXIMUM_TESTING_RM(:)
      ALLOCATABLE :: TARGETS,ADJ_BASE_CAP,ADJ_PEAK_CAP,PEAK_ADJUSTMENTS,PEAK_GROWTH_RATE,MINIMUM_TESTING_RM,MAXIMUM_TESTING_RM
      SAVE TARGETS,ADJ_BASE_CAP,ADJ_PEAK_CAP,PEAK_ADJUSTMENTS,PEAK_GROWTH_RATE,MINIMUM_TESTING_RM,MAXIMUM_TESTING_RM
!
         CALL OPEN_CAPACITY_RATIOS_FILE(10,RATIOS_FILE_ACTIVE)
         IF(.NOT. RATIOS_FILE_ACTIVE) THEN
            READ_CAP_RATIOS_FILE = .FALSE.
         ELSE
            IF(ALLOCATED(TARGETS)) THEN
               DEALLOCATE(TARGETS,ADJ_BASE_CAP,ADJ_PEAK_CAP,PEAK_ADJUSTMENTS,PEAK_GROWTH_RATE,MINIMUM_TESTING_RM,MAXIMUM_TESTING_RM)
            ENDIF
            ALLOCATE(TARGETS(3,STUDY_PERIOD), &
                     ADJ_BASE_CAP(STUDY_PERIOD), &
                     ADJ_PEAK_CAP(STUDY_PERIOD), &
                     PEAK_ADJUSTMENTS(STUDY_PERIOD), &
                     PEAK_GROWTH_RATE(STUDY_PERIOD), &
                     MINIMUM_TESTING_RM(STUDY_PERIOD), &
                     MAXIMUM_TESTING_RM(STUDY_PERIOD))
            I = 0
            DO WHILE (I < STUDY_PERIOD)
               I = I + 1
               READ(10,REC=I,IOSTAT=IOS) DELETE,(TARGETS(J,I),J=1,3), &
                                        ADJ_BASE_CAP(I), &
                                        ADJ_PK1,ADJ_PEAK_CAP(I),ADJ_PK2, &
                                        PEAK_GROWTH_RATE(I), &
                                        MINIMUM_TESTING_RM(I), &
                                        MAXIMUM_TESTING_RM(I)
               IF(IOS /= 0) EXIT
               IF(MINIMUM_TESTING_RM(I) > 99999.) &
                                    MINIMUM_TESTING_RM(I) = TARGETS(3,I)
               IF(MAXIMUM_TESTING_RM(I) > 99999.) &
                                MAXIMUM_TESTING_RM(I) = 1.2*TARGETS(3,I)
               PEAK_ADJUSTMENTS(I) = ADJ_PK1 + ADJ_PK2
            ENDDO
            CLOSE(10)
            READ_CAP_RATIOS_FILE = I == STUDY_PERIOD
         ENDIF
      RETURN
!***********************************************************************
      ENTRY ADJUSTMENT_BASE_CAPACITY(R_YEAR)
!***********************************************************************
         IF(R_YEAR <= STUDY_PERIOD) THEN
            ADJUSTMENT_BASE_CAPACITY = ADJ_BASE_CAP(R_YEAR)
         ELSE
            ADJUSTMENT_BASE_CAPACITY = ADJ_BASE_CAP(STUDY_PERIOD)
         ENDIF
      RETURN
!***********************************************************************
      ENTRY TARGET_RATIO(ICAP,R_YEAR)
!***********************************************************************
         IF(R_YEAR <= STUDY_PERIOD) THEN
            TARGET_RATIO = TARGETS(ICAP,R_YEAR)
         ELSE
            TARGET_RATIO = TARGETS(ICAP,STUDY_PERIOD)
         ENDIF
      RETURN
!***********************************************************************
      ENTRY ADJUSTMENT_PEAK_CAPACITY(R_YEAR)
!***********************************************************************
         IF(R_YEAR <= STUDY_PERIOD) THEN
            ADJUSTMENT_PEAK_CAPACITY = ADJ_PEAK_CAP(R_YEAR)
         ELSE
            ADJUSTMENT_PEAK_CAPACITY = ADJ_PEAK_CAP(STUDY_PERIOD)
         ENDIF
      RETURN
!***********************************************************************
      ENTRY ADJUSTMENT_CAPACITY(R_YEAR)
!***********************************************************************
         IF(RATIOS_FILE_ACTIVE) THEN
            IF(R_YEAR <= STUDY_PERIOD) THEN
               ADJUSTMENT_CAPACITY = ADJ_BASE_CAP(R_YEAR) + ADJ_PEAK_CAP(R_YEAR)
            ELSE
               ADJUSTMENT_CAPACITY = ADJ_BASE_CAP(STUDY_PERIOD) + ADJ_PEAK_CAP(STUDY_PERIOD)
            ENDIF
         ELSE
            ADJUSTMENT_CAPACITY = 0.
         ENDIF
      RETURN
!***********************************************************************
      ENTRY ADJUSTMENT_PEAK(R_YEAR)
!***********************************************************************
         IF(RATIOS_FILE_ACTIVE) THEN
            IF(R_YEAR <= STUDY_PERIOD) THEN
               ADJUSTMENT_PEAK = PEAK_ADJUSTMENTS(R_YEAR)
            ELSE
               ADJUSTMENT_PEAK = PEAK_ADJUSTMENTS(STUDY_PERIOD)
            ENDIF
         ELSE
            ADJUSTMENT_PEAK = 0.
         ENDIF
      RETURN
!***********************************************************************
      ENTRY PLANNING_PEAK_GROWTH_RATE(R_YEAR)
!***********************************************************************
         IF(RATIOS_FILE_ACTIVE) THEN
            IF(R_YEAR <= STUDY_PERIOD) THEN
               PLANNING_PEAK_GROWTH_RATE = PEAK_GROWTH_RATE(R_YEAR)
            ELSE
               PLANNING_PEAK_GROWTH_RATE=PEAK_GROWTH_RATE(STUDY_PERIOD)
            ENDIF
         ELSE
            PLANNING_PEAK_GROWTH_RATE = 0.
         ENDIF
      RETURN
!***********************************************************************
      ENTRY MAX_CAPACITY_TESTING_MARGIN(R_YEAR)
!***********************************************************************
         IF(RATIOS_FILE_ACTIVE) THEN
            IF(R_YEAR <= STUDY_PERIOD) THEN
               MAX_CAPACITY_TESTING_MARGIN = MAXIMUM_TESTING_RM(R_YEAR)
            ELSE
               MAX_CAPACITY_TESTING_MARGIN = MAXIMUM_TESTING_RM(STUDY_PERIOD)
            ENDIF
         ELSE
            MAX_CAPACITY_TESTING_MARGIN = 9999.
         ENDIF
      RETURN
!***********************************************************************
      ENTRY MIN_CAPACITY_TESTING_MARGIN(R_YEAR)
!***********************************************************************
         IF(RATIOS_FILE_ACTIVE) THEN
            IF(R_YEAR <= STUDY_PERIOD) THEN
               MIN_CAPACITY_TESTING_MARGIN = MINIMUM_TESTING_RM(R_YEAR)
            ELSE
               MIN_CAPACITY_TESTING_MARGIN = MINIMUM_TESTING_RM(STUDY_PERIOD)
            ENDIF
         ELSE
            MIN_CAPACITY_TESTING_MARGIN = 0.
         ENDIF
      RETURN
      END
!***********************************************************************
!
!          OBJECT TO CONVERT DATA FILES TO DIRECT ACESS BINARY
!         COPYRIGHT (C) 1983, 84, 85, 93 M.S. GERBER & ASSOCIATES, INC.
!
!***********************************************************************
!
      SUBROUTINE ICAP_OBJECT
      use end_routine, only: end_program, er_message
      use SpinDriftLib
      use prod_arrays_dimensions
      USE SIZECOM
      use globecom
!
      INTEGER (kind=2) ::  IREC,INUNIT,DELETE,YR,LRECL=146
      INTEGER ::  UNIT_NO
      INTEGER (kind=4) ::  IOS
      LOGICAL (kind=4) ::  ICAP_FILE_EXISTS=.FALSE.
      LOGICAL (kind=1) ::  R_ICAP_FILE_ACTIVE
      CHARACTER (len=5) ::  ICAP_FILE,OVERLAY_FAMILY_NAME
      CHARACTER (len=10) ::  Type_of_New_Entry,Type_of_EAS_Revenue
      CHARACTER (len=20) ::  REGIONAL_PA_NAME
      CHARACTER (len=256) ::  FILE_NAME
      CHARACTER (len=256) ::  BASE_FILE_DIRECTORY,OUTPUT_DIRECTORY
      INTEGER (kind=2) ::  REGIONAL_PLANNING_AREA
      REAL ::  MIN_ICAP,MAX_ICAP,ICAP_COST(10),ICAP_RATIO(10),PEAK_COIN_FACTOR,LEVELIZED_ICAP_MARKET,EAS_Revenue_Input
! DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER (len=1024) ::  RECLN
! DECLARATION FOR LOCALS
      CHARACTER (len=15) ::  FILE_TYPE='Capacity ICAP'
      CHARACTER (len=2) ::  CAPACITY_ICAP_OL='BC'
      LOGICAL (kind=1) ::  LAHEY_LF95
      CHARACTER (len=30) ::  SCREEN_OUTPUT
!
!***********************************************************************
      ENTRY ICAP_MAKEBIN
!***********************************************************************
!
      FILE_NAME = trim(BASE_FILE_DIRECTORY())//"ICB"//trim(ICAP_FILE())//".DAT"
      INQUIRE(FILE=FILE_NAME,EXIST=ICAP_FILE_EXISTS)
      IF(ICAP_FILE_EXISTS) THEN
         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//ICAP_FILE()
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
            CALL MG_LOCATE_WRITE(16,30,ICAP_FILE(),ALL_VERSIONS,0)
         ENDIF
!
! INITILIZE REGIONAL PLANNING PARAMETERS PER DOUG
!
         MIN_ICAP = 1.150
         MAX_ICAP = 1.400
         ICAP_COST(1)  = 1.300
         ICAP_RATIO(1) = 1.000
         ICAP_COST(2)  = 1.200
         ICAP_RATIO(2) = 1.035
         ICAP_COST(3)  = 1.100
         ICAP_RATIO(3) = 1.070
         ICAP_COST(4)  = 1.000
         ICAP_RATIO(4) = 1.105
         ICAP_COST(5)  = 1.000
         ICAP_RATIO(5) = 1.140
         ICAP_COST(6)  = 0.090
         ICAP_RATIO(6) = 1.175
         ICAP_COST(7)  = 0.550
         ICAP_RATIO(7) = 1.210
         ICAP_COST(8)  = 0.250
         ICAP_RATIO(8) = 1.245
         ICAP_COST(9)  = 0.050
         ICAP_RATIO(9) = 1.280
         ICAP_COST(10) = 0.000
         ICAP_RATIO(10)= 1.315
         REGIONAL_PA_NAME = 'AREA'
         Type_of_New_Entry = 'MAXIMUM'
         EAS_Revenue_Input = 0.0
         Type_of_EAS_Revenue = 'MRX'

         OPEN(11,FILE=trim(OUTPUT_DIRECTORY())//"BCICAP.BIN",ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
         OPEN(10,FILE=FILE_NAME)
         IREC = 1
         READ(10,*) DELETE
         DO
            READ(10,1000,IOSTAT=IOS) RECLN
            IF(IOS /=0) EXIT
            RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
!
            IF(RECLN(1:1) == '7' .OR. IOS /= 0) CYCLE
!
            READ(RECLN,*,ERR=300) DELETE,YR, &
                    REGIONAL_PLANNING_AREA,MIN_ICAP,MAX_ICAP, &
                                          ICAP_COST,ICAP_RATIO, &
                                          PEAK_COIN_FACTOR, &
                                          LEVELIZED_ICAP_MARKET, &
                                          REGIONAL_PA_NAME, &
                                          Type_of_New_Entry, &
                                          EAS_Revenue_Input, &
                                          Type_of_EAS_Revenue
            WRITE(11,REC=IREC) YR, &
                    REGIONAL_PLANNING_AREA,MIN_ICAP,MAX_ICAP, &
                                          ICAP_COST,ICAP_RATIO, &
                                          PEAK_COIN_FACTOR, &
                                          LEVELIZED_ICAP_MARKET, &
                                          REGIONAL_PA_NAME, &
                                          Type_of_New_Entry, &
                                          EAS_Revenue_Input, &
                                          Type_of_EAS_Revenue
                 IREC = IREC + 1
!
         ENDDO
         CLOSE(10)
         CLOSE(11)
      ELSE IF(INDEX(ICAP_FILE(),'NONE') == 0) THEN
         CALL STOP_NOFILE(FILE_TYPE,FILE_NAME) !'Capacity-Planning ICAP'
      ENDIF
      RETURN
!***********************************************************************
! OVERLAY THE TARGET CAPACITY-ICAP FILE
      ENTRY ICAP_MAKEOVL(OVERLAY_FAMILY_NAME)
!***********************************************************************
!
      IF(LAHEY_LF95()) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSE
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         CALL LOCATE(10,51)
      ENDIF
      FILE_NAME = trim(OUTPUT_DIRECTORY())//"ICO"//trim(OVERLAY_FAMILY_NAME)//".DAT"
      OPEN(10,FILE=FILE_NAME)
      READ(10,*) DELETE
      INUNIT = 12
      IF(CAPACITY_ICAP_OL == 'BC') THEN
         OPEN(11,FILE=trim(OUTPUT_DIRECTORY())//"BCICAP.BIN",ACCESS="DIRECT",RECL=LRECL)
         INUNIT = 11
      ENDIF
      OPEN(12,FILE=trim(OUTPUT_DIRECTORY())//"OLICAP.BIN",ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
      IREC = 0
      DO
         IREC = IREC + 1
         READ(INUNIT,REC=IREC,IOSTAT=IOS) YR, &
                    REGIONAL_PLANNING_AREA,MIN_ICAP,MAX_ICAP, &
                                          ICAP_COST,ICAP_RATIO, &
                                          PEAK_COIN_FACTOR, &
                                          LEVELIZED_ICAP_MARKET, &
                                          REGIONAL_PA_NAME, &
                                          Type_of_New_Entry, &
                                          EAS_Revenue_Input, &
                                          Type_of_EAS_Revenue
         IF(IOS /= 0) EXIT
         DO
            READ(10,1000,IOSTAT=IOS) RECLN
            IF(RECLN(1:1) == '7') CYCLE
            EXIT
         ENDDO
         IF(IOS == 0) THEN
            RECLN = trim(RECLN)//',,,,,,,,,,,,,'
            READ(RECLN,*,ERR=300) DELETE,YR, &
                    REGIONAL_PLANNING_AREA,MIN_ICAP,MAX_ICAP, &
                                          ICAP_COST,ICAP_RATIO, &
                                          PEAK_COIN_FACTOR, &
                                          LEVELIZED_ICAP_MARKET, &
                                          REGIONAL_PA_NAME, &
                                          Type_of_New_Entry, &
                                          EAS_Revenue_Input, &
                                          Type_of_EAS_Revenue
         ENDIF
         WRITE(12,REC=IREC) YR, &
                    REGIONAL_PLANNING_AREA,MIN_ICAP,MAX_ICAP, &
                                          ICAP_COST,ICAP_RATIO, &
                                          PEAK_COIN_FACTOR, &
                                          LEVELIZED_ICAP_MARKET, &
                                          REGIONAL_PA_NAME, &
                                          Type_of_New_Entry, &
                                          EAS_Revenue_Input, &
                                          Type_of_EAS_Revenue
      ENDDO
      CLOSE(10)
      CLOSE(12)
      IF(CAPACITY_ICAP_OL == 'BC') CLOSE(11)
      CAPACITY_ICAP_OL = 'OL'
      RETURN
!
!
  300 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from CN_OBJT SIID38'
      call end_program(er_message)
!
!***********************************************************************
      ENTRY RESET_ICAP_OL
!***********************************************************************
         CAPACITY_ICAP_OL = 'BC'
      RETURN
!
!***********************************************************************
      ENTRY OPEN_CAPACITY_ICAP_FILE(UNIT_NO,R_ICAP_FILE_ACTIVE)
!***********************************************************************
         IF(ICAP_FILE_EXISTS) THEN
            OPEN(UNIT_NO,FILE=trim(OUTPUT_DIRECTORY())//CAPACITY_ICAP_OL//"ICAP.BIN",ACCESS="DIRECT",RECL=LRECL)
         ELSE
            WRITE(4,'(1X,3A,I4,A)') '*** WARNING *** Capacity ICAP ', &
            'file did not exist.  Expansion planning was not done for ','end point',END_POINT,'.'
         ENDIF
         R_ICAP_FILE_ACTIVE = ICAP_FILE_EXISTS
      RETURN
 1000 FORMAT(A)
 1010 FORMAT('&',A)
      END
!***********************************************************************
!
! ICAP FUNCTIONS
!
!***********************************************************************
      FUNCTION READ_ICAP_FILE()
      use end_routine, only: end_program, er_message
!
      USE CAPACITY_OPTIONS_ALLOC_VARS
      USE ICAP_RESULTS  ! VARIABLE USED IN CLREPORT AND HERE
      USE SIZECOM
      use globecom

!
      INTEGER (kind=2) :: DELETE,I,R_YEAR, &
                  LOCAL_YEAR, &
                  R_PA, &
                  MAX_PA, &
                  PA, &
                  CM, &
                  MAX_CM, &
                  K, &
                  GET_PLANNING_AREA_POSITION, &
                  GET_CAPACITY_MARKET_POSITION, &
                  GET_REGIONAL_PA_NAME, &
                  GET_REGIONAL_CM_NAME, &
                  GET_ICAP_SWITCH_INDEX, &
                  R_MRX_MARGINAL_ICAP, &
                  REG_PARAM_ID(9999)
      INTEGER (kind=4) ::  IOS,ICAP
      INTEGER (kind=1) ::  J
      LOGICAL (kind=1) ::  READ_ICAP_FILE,ICAP_FILE_ACTIVE=.FALSE. , &
                YES_REGIONAL_PARAMS_EXIST, &
                ACTIVE_PA(:), &
                ACTIVE_CM(:), &
               GET_ICAP_REPORT_VALUES, &
               CALC_ANN_ICAP_VALUES, &
               GREEN_MRX=.FALSE. , &
               MARGINAL_ICAP
      CHARACTER (len=2) ::  GREEN_MRX_METHOD
      REAL ::  ADJUSTMENT_PEAK,TARGET_RATIO
      REAL ::  ADJUSTMENT_BASE_CAPACITY,ADJUSTMENT_PEAK_CAPACITY
      REAL ::  ADJ_PK2,ADJ_PK1,PLANNING_PEAK_GROWTH_RATE
      REAL ::  ICAP_MIN_TESTING_MARGIN,ICAP_MAX_TESTING_MARGIN, &
           LOCAL_ICAP_CAPACITY_COST(10),LOCAL_ICAP_CAPACITY_RATIO(10), &
           GET_ICAP_REVENUE_MULT,LOCAL_RATIO,R_RESERVE_MARGIN, &
           LOCAL_WEIGHT,MAX_TESTING_MARGIN,MIN_TESTING_MARGIN, &
           R_CURRENT_TARGET_RATIO,PEAK_COIN_FACTOR, &
           LEVELIZED_ICAP_MARKET,LOCAL_MINIMUM_TESTING_RM, &
           LOCAL_MAXIMUM_TESTING_RM,R_ICAP_CONE_VALUE, &
           R_INSTALLED_CAPACITY_VALUE,R_EAS_REVENUE_OFFSET, &
           R_MRX_EAS,R_ADJUSTED_CAPACITY_VALUE,R_ICAP_MAX_MRX_OR_CONE, &
           R_ICAP_MRX_CONE,R_ICAP_RESERVE_MARGIN,R_ICAP_DEMAND_POINT, &
           R_ICAP_VALUE_POINT,RESERVE,RATIO, &
           LOWER_X,LOWER_Y,INCREMENT_ABOVE_LOWER,DELTA_X,DELTA_Y, &
           DOWN_FROM_LOWER_Y,Local_EAS_Revenue_Input,TEMP_R4
!
      REAL ::  TARGETS(:,:),ADJ_BASE_CAP(:),ADJ_PEAK_CAP(:), &
           PEAK_ADJUSTMENTS(:),ADJUSTMENT_CAPACITY, &
           PEAK_GROWTH_RATE(:), &
           MINIMUM_TESTING_RM(:,:), &
           MAXIMUM_TESTING_RM(:,:), &
           ICAP_CONE_VALUE(:,:), &
           EAS_Revenue_Input(:,:)
      CHARACTER (len=10) ::  Local_Type_of_New_Entry, &
                   Local_Type_of_EAS_Revenue, &
                   Type_of_New_Entry(:,:), &
                   Type_of_EAS_Revenue(:,:)

      CHARACTER (len=20) ::  REGIONAL_PA_NAME(:), &
                   LOCAL_REGIONAL_PA_NAME, &
                   R_REGIONAL_PA_NAME
      CHARACTER (len=35) ::  & !  092011.
                   REGIONAL_CM_NAME(:), &
                   R_REGIONAL_CM_NAME
      ALLOCATABLE :: TARGETS,ADJ_BASE_CAP,ADJ_PEAK_CAP,PEAK_ADJUSTMENTS, &
           PEAK_GROWTH_RATE,MINIMUM_TESTING_RM,MAXIMUM_TESTING_RM, &
           ICAP_CONE_VALUE, &
           ACTIVE_PA, &
           ACTIVE_CM, &
           REGIONAL_PA_NAME, &
           REGIONAL_CM_NAME, &
           Type_of_New_Entry, &
           Type_of_EAS_Revenue, &
           EAS_Revenue_Input
      SAVE TARGETS,ADJ_BASE_CAP,ADJ_PEAK_CAP,PEAK_ADJUSTMENTS, &
           PEAK_GROWTH_RATE,MINIMUM_TESTING_RM,MAXIMUM_TESTING_RM, &
           ICAP_CONE_VALUE, &
           Type_of_New_Entry, &
           Type_of_EAS_Revenue, &
           EAS_Revenue_Input, &
           ACTIVE_PA, &
           ACTIVE_CM, &
           REGIONAL_PA_NAME, &
           REGIONAL_CM_NAME
!
!  REGIONAL PLANNING AND ICAP INFORMATION
!  ONE REGION FOR NOW
!
      INTEGER (kind=2) ::  REGIONAL_PLANNING_AREA_ID, &
                GET_NUMBER_OF_PLANNING_GROUPS, &
                GET_NUMBER_OF_CAPACITY_MARKETS, &
                PG=1 ,CG=1 ,CG_COUNT=0
      REAL ::  MIN_CAP2PEAK_RATIO(:,:),MAX_CAP2PEAK_RATIO(:,:), &
           ICAP_CAPACITY_COST(:,:,:), &
           ICAP_CAPACITY_RATIO(:,:,:), &
           ICAP_ANN_RESERVE(:), &
           ICAP_ANN_DEMAND(:)
      ALLOCATABLE :: MIN_CAP2PEAK_RATIO,MAX_CAP2PEAK_RATIO, &
           ICAP_CAPACITY_COST, &
           ICAP_CAPACITY_RATIO, &
           ICAP_ANN_RESERVE, &
           ICAP_ANN_DEMAND
      SAVE MIN_CAP2PEAK_RATIO,MAX_CAP2PEAK_RATIO, &
           ICAP_CAPACITY_COST, &
           ICAP_CAPACITY_RATIO, &
           ICAP_ANN_RESERVE, &
           ICAP_ANN_DEMAND, &
           MAX_PA
!
! END DATA DECLARATIONS
!
!
         CALL OPEN_CAPACITY_ICAP_FILE(10,ICAP_FILE_ACTIVE)
         MAX_PA = 0
         IF(.NOT. ICAP_FILE_ACTIVE) THEN
            READ_ICAP_FILE = .FALSE.
         ELSE
!
            PG = MAX(GET_NUMBER_OF_PLANNING_GROUPS(),INT(1,2))
            CG = 3* MAX(GET_NUMBER_OF_CAPACITY_MARKETS(),INT(1,2)) ! 3 * test 050510
!
            IF(ALLOCATED(TARGETS)) THEN
               DEALLOCATE(TARGETS,ADJ_BASE_CAP,ADJ_PEAK_CAP, &
                          PEAK_ADJUSTMENTS,PEAK_GROWTH_RATE, &
                          MINIMUM_TESTING_RM,MAXIMUM_TESTING_RM, &
                          ICAP_CONE_VALUE, &
                          Type_of_New_Entry, &
                          Type_of_EAS_Revenue, &
                          EAS_Revenue_Input, &
                          MIN_CAP2PEAK_RATIO,MAX_CAP2PEAK_RATIO, &
                          ICAP_CAPACITY_COST,ICAP_CAPACITY_RATIO, &
                          ICAP_ANN_RESERVE,ICAP_ANN_DEMAND, &
                          ACTIVE_PA, &
                          ACTIVE_CM, &
                          REGIONAL_PA_NAME, &
                          REGIONAL_CM_NAME)
            ENDIF
            ALLOCATE(TARGETS(3,STUDY_PERIOD), &
             ADJ_BASE_CAP(STUDY_PERIOD),ADJ_PEAK_CAP(STUDY_PERIOD), &
             PEAK_ADJUSTMENTS(STUDY_PERIOD), &
             PEAK_GROWTH_RATE(STUDY_PERIOD), &
             MINIMUM_TESTING_RM(STUDY_PERIOD,PG), &
             MAXIMUM_TESTING_RM(STUDY_PERIOD,PG),REGIONAL_PA_NAME(PG), &
             MIN_CAP2PEAK_RATIO(PG,STUDY_PERIOD), & !  ONE PLANNING AREA FOR NOW
             MAX_CAP2PEAK_RATIO(PG,STUDY_PERIOD),ACTIVE_PA(PG), &
             ICAP_CONE_VALUE(STUDY_PERIOD,CG), &
             Type_of_New_Entry(STUDY_PERIOD,CG), &
             Type_of_EAS_Revenue(STUDY_PERIOD,CG), &
             EAS_Revenue_Input(STUDY_PERIOD,CG), &
             ICAP_CAPACITY_COST(CG,10,STUDY_PERIOD), & !  10 ICAP VALUES
             ICAP_CAPACITY_RATIO(CG,10,STUDY_PERIOD), &
             ICAP_ANN_RESERVE(CG),ICAP_ANN_DEMAND(CG),ACTIVE_CM(CG), &
             REGIONAL_CM_NAME(CG))
            K = 0
!
            ICAP_CAPACITY_COST = 0.
            ICAP_CAPACITY_RATIO = 0.
            REG_PARAM_ID = 0
!
!
            DO I = 1, PG
               ACTIVE_PA(I) = .FALSE.
            ENDDO
            DO I = 1, CG
               ACTIVE_CM(I) = .FALSE.
            END DO
!
            CG_COUNT = 0
!
!
            DO
               K = K + 1
               READ(10,REC=K,IOSTAT=IOS) DELETE, &
                                        REGIONAL_PLANNING_AREA_ID, &
                                        LOCAL_MINIMUM_TESTING_RM, &
                                        LOCAL_MAXIMUM_TESTING_RM, &
                                        LOCAL_ICAP_CAPACITY_COST, &
                                        LOCAL_ICAP_CAPACITY_RATIO, &
                                        PEAK_COIN_FACTOR, &
                                        LEVELIZED_ICAP_MARKET, &
                                        LOCAL_REGIONAL_PA_NAME, &
                                        Local_Type_of_New_Entry, &
                                        Local_EAS_Revenue_Input, &
                                        Local_Type_of_EAS_Revenue

               IF(IOS /= 0) EXIT
!
               I = MAX(INT(1,2),DELETE - BASE_YEAR)
!
               IF(I > STUDY_PERIOD) CYCLE
!
               PA = REGIONAL_PLANNING_AREA_ID
               PA = GET_PLANNING_AREA_POSITION(PA)
               IF(I == 1) THEN
                  IF(REGIONAL_PLANNING_AREA_ID > 0 .AND. REGIONAL_PLANNING_AREA_ID < 10000) THEN
                     IF(REG_PARAM_ID(REGIONAL_PLANNING_AREA_ID) > 0) THEN
                        WRITE(4,*) 'IN THE REGIONAL PLANNING PARMETERS'
                        WRITE(4,*) 'FILE, THERE IS A REDUNANT PLANNING'
                        WRITE(4,*) 'AREA ID',REGIONAL_PLANNING_AREA_ID
                     ENDIF
                     REG_PARAM_ID(REGIONAL_PLANNING_AREA_ID) = REG_PARAM_ID(REGIONAL_PLANNING_AREA_ID) + 1
                  ELSE
                        WRITE(4,*) 'IN THE REGIONAL PLANNING PARMETERS'
                        WRITE(4,*) 'FILE, THERE IS AN INVALID PLANNING'
                        WRITE(4,*) 'AREA ID',REGIONAL_PLANNING_AREA_ID
                        er_message='Stop requested from CN_OBJT SIID40'
                        call end_program(er_message)
                  ENDIF
               ENDIF
               IF(PA > 0 .AND. PA <= PG) THEN
                  ACTIVE_PA(PA) = .TRUE.
!
! DOUBLE INDEX MATCHES CALLING ROUTINES.
!
                  MAX_PA = MAX(MAX_PA,PA)
!
                  IF(I == 1) THEN ! INDEX ON THE FIRST ENCOUNTER
                     REGIONAL_PA_NAME(PA) = LOCAL_REGIONAL_PA_NAME
                  ENDIF
                  MINIMUM_TESTING_RM(I,PA) = LOCAL_MINIMUM_TESTING_RM
                  MAXIMUM_TESTING_RM(I,PA) = LOCAL_MAXIMUM_TESTING_RM
!
                  IF(MINIMUM_TESTING_RM(I,PA) > 99999.) MINIMUM_TESTING_RM(I,PA) = TARGETS(3,I)
                  IF(MAXIMUM_TESTING_RM(I,PA) > 99999.) MAXIMUM_TESTING_RM(I,PA) = 1.4*TARGETS(3,I)
               ENDIF
!
               CM = REGIONAL_PLANNING_AREA_ID
               CM = GET_CAPACITY_MARKET_POSITION(CM)
               IF(CM > 0 .AND. CM <= CG) THEN
                  ACTIVE_CM(CM) = .TRUE.
!
! DOUBLE INDEX MATCHES CALLING ROUTINES.
!
                  MAX_CM = MAX(MAX_CM,CM)
!
                  IF(I == 1) THEN ! INDEX ON THE FIRST ENCOUNTER
                     REGIONAL_CM_NAME(CM) = LOCAL_REGIONAL_PA_NAME
                     CG_COUNT = CG_COUNT + 1
                  ENDIF
!
                  DO J = 1, 10
                     ICAP_CAPACITY_COST(CM,J,I) = LOCAL_ICAP_CAPACITY_COST(J)
                     ICAP_CAPACITY_RATIO(CM,J,I) = LOCAL_ICAP_CAPACITY_RATIO(J)
                  ENDDO
!
                  ICAP_CONE_VALUE(I,CM) = LEVELIZED_ICAP_MARKET
                  CALL MAKEUPPER(Local_Type_of_New_Entry,INT(10,2))
                  Type_of_New_Entry(I,CM) = Local_Type_of_New_Entry
                  Type_of_EAS_Revenue(I,CM) = Local_Type_of_EAS_Revenue
                  EAS_Revenue_Input(I,CM) = Local_EAS_Revenue_Input
               ENDIF ! CM
!
!
!
            ENDDO

            CLOSE(10)
            GREEN_MRX =  GREEN_MRX_METHOD() == 'GX'
            READ_ICAP_FILE = I == STUDY_PERIOD
         ENDIF
      RETURN
!***********************************************************************
      ENTRY CALC_ANN_ICAP_VALUES(R_YEAR)
!***********************************************************************
         IF(ALLOCATED(ICAP_CONE_VALUE)) THEN

            DO CM = 1, CG_COUNT ! CG

                  CALL GET_CAPACITY_MARKET_MARGIN(CM,RESERVE)

               ICAP_ANN_RESERVE(CM) = RESERVE
               IF(RESERVE < -998.0) THEN ! PER BURESH. 04/30/08.
                  ICAP_ANN_DEMAND(CM) = 1.0
               ELSEIF(RESERVE <= ICAP_CAPACITY_RATIO(CM,1,R_YEAR)) THEN
                  J = 1
                  ICAP_ANN_DEMAND(CM) = ICAP_CAPACITY_COST(CM,J,R_YEAR)
               ELSEIF(RESERVE >= ICAP_CAPACITY_RATIO(CM,10,R_YEAR)) THEN
                  J = 10
                  ICAP_ANN_DEMAND(CM) = ICAP_CAPACITY_COST(CM,J,R_YEAR)
               ELSE
                  DO J = 2, 10
                     IF(RESERVE > ICAP_CAPACITY_RATIO(CM,J,R_YEAR)) CYCLE
                     DELTA_Y =  ICAP_CAPACITY_COST(CM,J,R_YEAR) - ICAP_CAPACITY_COST(CM,J-1,R_YEAR)
                     IF(ABS(DELTA_Y) < .0001) THEN
                        ICAP_ANN_DEMAND(CM) = ICAP_CAPACITY_COST(CM,J,R_YEAR)
                        EXIT
                     ENDIF
!
                     LOWER_X = ICAP_CAPACITY_RATIO(CM,J-1,R_YEAR)
                     LOWER_Y = ICAP_CAPACITY_COST(CM,J-1,R_YEAR)
                     INCREMENT_ABOVE_LOWER = RESERVE - LOWER_X
! NEED MONOTONIC X
                     DELTA_X =  ICAP_CAPACITY_RATIO(CM,J,R_YEAR) - ICAP_CAPACITY_RATIO(CM,J-1,R_YEAR)
                     IF(DELTA_X > 0.) THEN
                        DOWN_FROM_LOWER_Y = INCREMENT_ABOVE_LOWER*DELTA_Y/DELTA_X

                        ICAP_ANN_DEMAND(CM) = DOWN_FROM_LOWER_Y + LOWER_Y
                        EXIT
                     ELSE
                        WRITE(4,*) 'ICAP RESERVE FUNCTION NOT MONOTONIC'
                        ICAP_ANN_DEMAND(CM) = 0.0
                     ENDIF
!
                  ENDDO
                  IF(J == 11) THEN
                     WRITE(4,*) 'ICAP PRICING DID NOT FIND A VALID POINT'
                     er_message='Stop requested from CN_OBJT SIID41'
                     call end_program(er_message)
                  ENDIF
               ENDIF
            END DO
            CALC_ANN_ICAP_VALUES = .TRUE.
         ELSE
!
            CALC_ANN_ICAP_VALUES = .FALSE.
         ENDIF
      RETURN
!***********************************************************************
      ENTRY GET_ICAP_SWITCH_INDEX(  R_PA,R_YEAR,R_INSTALLED_CAPACITY_VALUE)
!***********************************************************************
         IF(ALLOCATED(ICAP_CONE_VALUE).AND. R_PA > 0 .AND. R_PA <= MAX_CM) THEN
            If(INDEX(Type_of_New_Entry(R_YEAR,R_PA),'CONE')/= 0) Then
               GET_ICAP_SWITCH_INDEX = 1
               If(INDEX(Type_of_New_Entry(R_YEAR,R_PA),'MRX') /= 0) Then
                  GET_ICAP_SWITCH_INDEX = 3
                  ICAP_CONE_VALUE(R_YEAR,R_PA) = LOWEST_CONE_COST_BY_CM(R_PA)/12.
               endif
               R_INSTALLED_CAPACITY_VALUE = 12. * ICAP_CONE_VALUE(R_YEAR,R_PA)
            Elseif(TRIM(Type_of_New_Entry(R_YEAR,R_PA)) == 'NONE') Then
               GET_ICAP_SWITCH_INDEX = 0
               R_INSTALLED_CAPACITY_VALUE = 0.0
            Else ! ASSUME CALCULATE BASED ON MRX PRICE
               GET_ICAP_SWITCH_INDEX = 2
               R_INSTALLED_CAPACITY_VALUE = 0.0
            Endif
         ELSE
            GET_ICAP_SWITCH_INDEX = 2
            R_INSTALLED_CAPACITY_VALUE = 0.0
         ENDIF
      RETURN
!***********************************************************************
      ENTRY GET_ICAP_REPORT_VALUES(R_PA, &
                                   R_YEAR, &
                                   R_ICAP_CONE_VALUE, &
                                   R_ICAP_MRX_CONE, &
                                   R_ICAP_RESERVE_MARGIN, &
                                   R_ICAP_DEMAND_POINT, &
                                   R_ICAP_VALUE_POINT, &
                                   R_INSTALLED_CAPACITY_VALUE, &
                                   R_EAS_REVENUE_OFFSET, &
                                   R_ADJUSTED_CAPACITY_VALUE, &
                                   R_MRX_EAS, &
                                   R_MRX_MARGINAL_ICAP)
!***********************************************************************
! NOW LOOPS ON CM NOT PA
         GET_ICAP_REPORT_VALUES = .FALSE.
!         CALL GET_PLANNING_RESERVE_MARGIN(R_PA,RESERVE)
         PA = R_PA
         R_MRX_MARGINAL_ICAP = 0
         IF(ALLOCATED(ICAP_CONE_VALUE) .AND. PA > 0 .AND. PA <= MAX_CM) THEN

            IF(GREEN_MRX) THEN
               If(INDEX(Type_of_New_Entry(R_YEAR,PA),'CONE') /= 0) Then
                  If(INDEX( &
                        Type_of_New_Entry(R_YEAR,PA),'MRX') /= 0) Then
                     ICAP_CONE_VALUE(R_YEAR,PA)  = &
                                        LOWEST_CONE_COST_BY_CM(PA)/12.
!
! 042617.
!
                     R_MRX_MARGINAL_ICAP = 1
                  ELSE
                     R_EAS_REVENUE_OFFSET = 0.0
                  endif
                  R_INSTALLED_CAPACITY_VALUE = &
                                              ICAP_CONE_VALUE(R_YEAR,PA)
!
! 042617. NEEDS TO BE SAVED BY CM.
!
                  R_ICAP_CONE_VALUE = ICAP_CONE_VALUE(R_YEAR,PA)

                  R_ICAP_DEMAND_POINT = ICAP_ANN_DEMAND(PA) ! 1s to be used in the demand adjustment curve.
                  R_ADJUSTED_CAPACITY_VALUE = R_INSTALLED_CAPACITY_VALUE
                  R_ICAP_VALUE_POINT = R_ICAP_DEMAND_POINT * R_ADJUSTED_CAPACITY_VALUE
               Elseif(INDEX(Type_of_New_Entry(R_YEAR,PA),'MARGINAL') /= 0) Then
                  CALL GET_INSTALL_CAP_VALUE_LAST_CM(PA,TEMP_R4)
                  R_INSTALLED_CAPACITY_VALUE = TEMP_R4
                  R_MRX_MARGINAL_ICAP = 1
                  R_EAS_REVENUE_OFFSET = R_MRX_EAS
                  R_ADJUSTED_CAPACITY_VALUE = MAX(0.0, &
                     R_INSTALLED_CAPACITY_VALUE - R_EAS_REVENUE_OFFSET)
                  R_ICAP_CONE_VALUE = ICAP_CONE_VALUE(R_YEAR,PA)
                  R_ICAP_DEMAND_POINT = ICAP_ANN_DEMAND(PA)
                  R_ICAP_VALUE_POINT = R_ICAP_DEMAND_POINT * R_ADJUSTED_CAPACITY_VALUE
! ONLY THREE OPTIONS
               Else ! if(TRIM(Type_of_New_Entry(R_YEAR,PA)) == 'NONE') Then
                  R_INSTALLED_CAPACITY_VALUE = 0.0
                  R_EAS_REVENUE_OFFSET = 0.0
                  R_ADJUSTED_CAPACITY_VALUE = 0.0
                  R_ICAP_CONE_VALUE = ICAP_CONE_VALUE(R_YEAR,PA)
                  R_ICAP_DEMAND_POINT = ICAP_ANN_DEMAND(PA)
                  R_ICAP_VALUE_POINT = 0.0
               Endif
               IF(ICAP_ANN_RESERVE(PA) > 0.001) THEN
                  R_ICAP_RESERVE_MARGIN = (ICAP_ANN_RESERVE(PA) - 1.0)*100.0
               ELSE
                  R_ICAP_RESERVE_MARGIN = 0.0
               ENDIF
!
            ELSE ! NOT GRX
               If(INDEX(Type_of_New_Entry(R_YEAR,PA),'CONE')/=0) Then
                  If(INDEX(Type_of_New_Entry(R_YEAR,PA),'MRX') /= 0) Then
                     ICAP_CONE_VALUE(R_YEAR,PA)  = LOWEST_CONE_COST_BY_CM(PA)/12.
                  endif
                  R_INSTALLED_CAPACITY_VALUE = ICAP_CONE_VALUE(R_YEAR,PA)
               Elseif(TRIM(Type_of_New_Entry(R_YEAR,PA)) == 'MRX') Then
                  R_INSTALLED_CAPACITY_VALUE = R_ICAP_MRX_CONE
               Elseif(TRIM(Type_of_New_Entry(R_YEAR,PA)) == 'MAXIMUM') Then
                  R_INSTALLED_CAPACITY_VALUE = max(R_ICAP_MRX_CONE,ICAP_CONE_VALUE(R_YEAR,PA))
               Elseif(TRIM(Type_of_New_Entry(R_YEAR,PA)) == 'MINIMUM') Then
                  R_INSTALLED_CAPACITY_VALUE = min(R_ICAP_MRX_CONE,ICAP_CONE_VALUE(R_YEAR,PA))
               Elseif(TRIM(Type_of_New_Entry(R_YEAR,PA)) == 'AVERAGE') Then
                  R_INSTALLED_CAPACITY_VALUE = (R_ICAP_MRX_CONE + ICAP_CONE_VALUE(R_YEAR,PA))* 0.5
               Elseif(TRIM(Type_of_New_Entry(R_YEAR,PA)) == 'MARGINAL') Then
!
                  CALL GET_INSTALL_CAP_VALUE_LAST_CM(PA,TEMP_R4)
                  R_INSTALLED_CAPACITY_VALUE = TEMP_R4
                  R_MRX_MARGINAL_ICAP = 1
               Elseif(TRIM(Type_of_New_Entry(R_YEAR,PA)) == 'NONE') Then
                  R_INSTALLED_CAPACITY_VALUE = 0.0
               Endif
!
               If(TRIM(Type_of_EAS_Revenue(R_YEAR,PA)) == 'EAS') Then
                  R_EAS_REVENUE_OFFSET = EAS_Revenue_Input(R_YEAR,PA)
               Elseif(TRIM(Type_of_EAS_Revenue(R_YEAR,PA)) == 'MRX') Then
                  R_EAS_REVENUE_OFFSET = R_MRX_EAS
               Elseif(TRIM(Type_of_EAS_Revenue(R_YEAR,PA)) == 'MAXIMUM') Then
                  R_EAS_REVENUE_OFFSET = max(R_MRX_EAS,ICAP_CONE_VALUE(R_YEAR,PA))
               Elseif(TRIM(Type_of_EAS_Revenue(R_YEAR,PA)) == 'MINIMUM') Then
                  R_EAS_REVENUE_OFFSET = min(R_MRX_EAS,ICAP_CONE_VALUE(R_YEAR,PA))
               Elseif(TRIM(Type_of_EAS_Revenue(R_YEAR,PA)) == 'AVERAGE') Then
                  R_EAS_REVENUE_OFFSET = (R_MRX_EAS + ICAP_CONE_VALUE(R_YEAR,PA))* 0.5
               Elseif(TRIM(Type_of_EAS_Revenue(R_YEAR,PA)) == 'NONE') Then
                  R_EAS_REVENUE_OFFSET = 0.0
               Endif
               R_ADJUSTED_CAPACITY_VALUE = MAX(0.0,R_INSTALLED_CAPACITY_VALUE - R_EAS_REVENUE_OFFSET)
               R_ICAP_CONE_VALUE = ICAP_CONE_VALUE(R_YEAR,PA)

               R_ICAP_DEMAND_POINT = ICAP_ANN_DEMAND(PA)
               R_ICAP_VALUE_POINT = R_ICAP_DEMAND_POINT * R_ADJUSTED_CAPACITY_VALUE

! FIND THE INTERVAL FOR THE CAP RATIO
               IF(ICAP_ANN_RESERVE(PA) > 0.001) THEN
                  R_ICAP_RESERVE_MARGIN = (ICAP_ANN_RESERVE(PA) - 1.0)*100.0
               ELSE
                  R_ICAP_RESERVE_MARGIN = 0.0
               ENDIF
            ENDIF
         ELSE
            R_ICAP_CONE_VALUE = 0.0
            R_ICAP_CONE_VALUE = MAX(R_ICAP_CONE_VALUE,0.0)
            IF(RESERVE > 0.001 .AND. ALLOCATED(ICAP_ANN_RESERVE)) THEN
               R_ICAP_RESERVE_MARGIN = (ICAP_ANN_RESERVE(PA) - 1.0)*100.0
            ELSE
               R_ICAP_RESERVE_MARGIN = 0.0
            ENDIF
            R_ICAP_DEMAND_POINT = 0.0
            R_ICAP_VALUE_POINT = 0.0
         ENDIF
      RETURN
!***********************************************************************
      ENTRY MARGINAL_ICAP(R_YEAR,R_PA)
!***********************************************************************
! NOW LOOPS ON CM NOT PA
         IF(ALLOCATED(ICAP_CONE_VALUE) .AND. R_PA > 0 .AND. R_PA <= MAX_CM) THEN
            PA = R_PA
!
            if(TRIM(Type_of_New_Entry(R_YEAR,PA)) == 'MARGINAL' .OR. (INDEX(Type_of_New_Entry(R_YEAR,PA),'CONE') /= 0 .AND. &
                   INDEX(Type_of_New_Entry(R_YEAR,PA),'MRX') /= 0)) Then
               MARGINAL_ICAP = .TRUE.
            Else
               MARGINAL_ICAP = .FALSE.
            Endif
         ENDIF
      RETURN
!***********************************************************************
      ENTRY GET_REGIONAL_PA_NAME(R_PA,R_REGIONAL_PA_NAME)
!***********************************************************************
         IF(R_PA > 0 .AND. R_PA <= MAX_PA) THEN
            IF(REGIONAL_PA_NAME(R_PA)(1:6) /= 'AREA  ') THEN  ! Regional PA Name initialized to "AREA"
               GET_REGIONAL_PA_NAME = 1
               R_REGIONAL_PA_NAME = REGIONAL_PA_NAME(R_PA)
            ELSE
               GET_REGIONAL_PA_NAME = 0
            ENDIF
         ELSE
            GET_REGIONAL_PA_NAME = 0
            R_REGIONAL_PA_NAME = 'INVALID PA NAME'
         ENDIF
      RETURN
!***********************************************************************
      ENTRY GET_REGIONAL_CM_NAME(R_PA,R_REGIONAL_CM_NAME)
!***********************************************************************
         IF(R_PA > 0 .AND. R_PA <= MAX_CM) THEN
            IF(REGIONAL_CM_NAME(R_PA)(1:6) /= 'AREA  ') THEN
! Regional CM Name initialized to "AREA"
               GET_REGIONAL_CM_NAME = 1
               R_REGIONAL_CM_NAME = REGIONAL_CM_NAME(R_PA)
            ELSE
               GET_REGIONAL_CM_NAME = 0
            ENDIF
         ELSE
            GET_REGIONAL_CM_NAME = 0
            R_REGIONAL_CM_NAME = 'INVALID CM NAME'
         ENDIF
      RETURN
!***********************************************************************
      ENTRY GET_ICAP_REVENUE_MULT(R_YEAR,R_PA,R_RESERVE_MARGIN)
!***********************************************************************
! NOW COMES IN AS CM (CAPACITY MARKET)
         GET_ICAP_REVENUE_MULT = 0.
         IF(ICAP_FILE_ACTIVE .AND. R_PA > 0 .AND. R_PA <= MAX_CM) THEN
            If(TRIM(Type_of_New_Entry(R_YEAR,PA)) /= 'NONE') Then
               IF(R_YEAR > STUDY_PERIOD) THEN
                  LOCAL_YEAR = STUDY_PERIOD
               ELSE
                  LOCAL_YEAR = R_YEAR
               ENDIF
               LOCAL_RATIO = R_RESERVE_MARGIN
               IF(LOCAL_RATIO < &
                            ICAP_CAPACITY_RATIO(R_PA,1,LOCAL_YEAR)) THEN
                  GET_ICAP_REVENUE_MULT = ICAP_CAPACITY_COST(R_PA,1,LOCAL_YEAR)
               ELSEIF(LOCAL_RATIO > ICAP_CAPACITY_RATIO(R_PA,10,LOCAL_YEAR)) THEN
                  GET_ICAP_REVENUE_MULT = ICAP_CAPACITY_COST(R_PA,10,LOCAL_YEAR)
               ELSE
                  DO I = 1, 9
                     IF(LOCAL_RATIO >= ICAP_CAPACITY_RATIO(R_PA,I+1,LOCAL_YEAR)) CYCLE
                     IF(ICAP_CAPACITY_RATIO(R_PA,I+1,LOCAL_YEAR) /= ICAP_CAPACITY_RATIO(R_PA,I,LOCAL_YEAR))THEN
                        LOCAL_WEIGHT = (LOCAL_RATIO - ICAP_CAPACITY_RATIO(R_PA,I,LOCAL_YEAR)) / &
                             (ICAP_CAPACITY_RATIO(R_PA,I+1,LOCAL_YEAR) - ICAP_CAPACITY_RATIO(R_PA,I,LOCAL_YEAR))
                        ELSE
                           LOCAL_WEIGHT = 0.
                     ENDIF
                     GET_ICAP_REVENUE_MULT = (1.- LOCAL_WEIGHT) * ICAP_CAPACITY_COST(R_PA,I,LOCAL_YEAR) + LOCAL_WEIGHT * &
                                ICAP_CAPACITY_COST(R_PA,I+1,LOCAL_YEAR)
                     EXIT
                  ENDDO
               ENDIF
            endif ! type == 'None'
!
!
         ENDIF
      RETURN
!***********************************************************************
      ENTRY YES_REGIONAL_PARAMS_EXIST
!***********************************************************************
         YES_REGIONAL_PARAMS_EXIST = ICAP_FILE_ACTIVE
      RETURN
!***********************************************************************
      ENTRY ICAP_MAX_TESTING_MARGIN(R_YEAR,R_CURRENT_TARGET_RATIO,R_PA)
!***********************************************************************
         IF(ICAP_FILE_ACTIVE .AND. R_PA > 0 .AND. R_PA <= MAX_PA) THEN
            IF(ACTIVE_PA(R_PA)) THEN
               IF(R_YEAR <= STUDY_PERIOD) THEN
                  ICAP_MAX_TESTING_MARGIN = MAXIMUM_TESTING_RM(R_YEAR,R_PA)
               ELSE
                  ICAP_MAX_TESTING_MARGIN = MAXIMUM_TESTING_RM(STUDY_PERIOD,R_PA)
               ENDIF
            ELSE
               ICAP_MAX_TESTING_MARGIN = R_CURRENT_TARGET_RATIO
            ENDIF
         ELSE
            ICAP_MAX_TESTING_MARGIN = R_CURRENT_TARGET_RATIO
         ENDIF
      RETURN
!***********************************************************************
      ENTRY ICAP_MIN_TESTING_MARGIN(R_YEAR,R_CURRENT_TARGET_RATIO,R_PA)
!***********************************************************************
         IF(ICAP_FILE_ACTIVE .AND. R_PA > 0 .AND. R_PA <= MAX_PA) THEN
            IF(ACTIVE_PA(R_PA)) THEN
               IF(R_YEAR <= STUDY_PERIOD) THEN
                  ICAP_MIN_TESTING_MARGIN = MINIMUM_TESTING_RM(R_YEAR,R_PA)
               ELSE
                  ICAP_MIN_TESTING_MARGIN = MINIMUM_TESTING_RM(STUDY_PERIOD,R_PA)
               ENDIF
            ELSE
               ICAP_MIN_TESTING_MARGIN = R_CURRENT_TARGET_RATIO ! 06/03/03. PER BURESH
            ENDIF
         ELSE
            ICAP_MIN_TESTING_MARGIN = R_CURRENT_TARGET_RATIO ! 06/03/03. PER BURESH
         ENDIF
      RETURN
      END
!***********************************************************************
      FUNCTION ADD_THIS_UNIT(R_POINTER,R_YEAR) & ! REAL
                                                 RESULT(R_ADD_THIS_UNIT)
!***********************************************************************
      use SpinDriftLib
      use prod_arrays_dimensions
      USE CAPACITY_OPTIONS_ALLOC_VARS
      USE CAPACITY_OPTIONS_FIXED_VARS
      USE SIZECOM
      use globecom

      INTEGER (KIND=2) :: R_POINTER,R_YEAR,IND, &
                          INDEP_POINTER,GET_GRX_INDEP_POINTER
      REAL (KIND=4) :: R_ADD_THIS_UNIT
      INTEGER (KIND=2) :: J,I,AVAILABLE_UNITS,IN_SERVICE_YR, &
                          R_OPERATION_LIFE
      INTEGER (KIND=2) :: ADD_NEW_CL_UNIT,CL_UNIT_POSITION, &
                          INCREMENT_AVAILABLE_CL_UNITS, &
                          INCREMENT_AVAILABLE_EL_UNITS
      REAL (KIND=4) :: CAPACITY_ADDED,ADD_NEW_EL_UNIT, &
                       CL_CAPACITY_PLANNING_ADJ, &
                       MUT_EXCLUSIVE_CAPACITY_RETIRED, &
                       EL_CAPACITY_PLANNING_ADDITIONS, &
                       EL_CAPACITY_PLANNING_REMOVALS, &
                       ADD_NEW_CT_UNIT, &
                       CT_CAPACITY_PLANNING_ADDITIONS, &
                       ENERGY_PRODUCTS_PLANNING_ADD, &
                       TRANSMISSION_PLANNING_ADD
      LOGICAL (KIND=1) :: DEPENDED_UNIT_FOUND, &
                          WRITE_TO_EXPANSION_REPORT, &
                          L_TEMP, &
                          EXPANSION_REPORT
      INTEGER (KIND=2) :: ESCALATIONS_UPDATED,UPDATE_PR_ESCALATIONS

!
         IF(R_YEAR >= BASE_YEAR + LEAD_TIME(R_POINTER) .AND. &
                  R_YEAR - LEAD_TIME(R_POINTER) <= LAST_STUDY_YEAR) THEN
!
! DON'T RETIRE UNITS IN THE EXTENSION PERIOD
!
            IF(PRODUCTION_DATA_POINTER(R_POINTER) > 0 .OR. (FILE_SOURCE(R_POINTER) == 'De' .AND. &
                              GRX_RESOURCE_LINK_ID(R_POINTER) > 0)) THEN
               IF(R_YEAR+OPERATION_LIFE(R_POINTER)>LAST_STUDY_YEAR .OR. MUT_EXC_UNIT(R_POINTER) > 0) THEN
                  R_OPERATION_LIFE = 99
               ELSEIF(R_YEAR+OPERATION_LIFE(R_POINTER)== LAST_STUDY_YEAR .AND. OPERATION_LIFE(R_POINTER) < 1) THEN
                  R_OPERATION_LIFE = 99
               ELSE
                  R_OPERATION_LIFE = OPERATION_LIFE(R_POINTER)
               ENDIF
               IF(FILE_SOURCE(R_POINTER) == 'CL') THEN
                  CL_UNIT_POSITION = &
                           ADD_NEW_CL_UNIT(R_YEAR, &
                                   R_OPERATION_LIFE, &
                                   PRODUCTION_DATA_POINTER(R_POINTER), &
                                   ON_LINE_MONTH, &
                                   PRIMARY_ASSET_CLASS(R_POINTER), &
                                   PRIMARY_ALLOCATION_VECTOR(R_POINTER), &
                                   RESOURCE_ID_NUM(R_POINTER), &
                                   R_POINTER)
                  IF(CL_UNIT_POSITION > 0) THEN
                     CAPACITY_ADDED = CL_CAPACITY_PLANNING_ADJ(R_YEAR,CL_UNIT_POSITION,2,.FALSE.)

                     SAVE_NO_CL_ADDITIONS(R_YEAR-BASE_YEAR) = SAVE_NO_CL_ADDITIONS(R_YEAR-BASE_YEAR) + 1
                     HAS_BEEN_SELECTED(R_POINTER) = .TRUE.
                     AVAILABLE_UNITS = INCREMENT_AVAILABLE_CL_UNITS()
! GAT. 2/9/94. ATTEMPT TO TAKE CARE OF HARD-WIRED/MUTUALLY EXCLUSIVE PROBLEM
                     IF(MUT_EXC_UNIT(R_POINTER) > 0) THEN
                        J = 0
                        DO
                           J = J + 1
                           IF(J > HARD_WIRED_OPTIONS) THEN
                              DEPEND_UNIT_NO(R_POINTER)=AVAILABLE_UNITS
                              EXIT
                           ENDIF
                           IF(MUT_EXC_UNIT(R_POINTER) /= RESOURCE_ID_NUM(HARD_WIRED_LIST(J))) CYCLE
                           MUT_EXCLUSIVE_CAPACITY_RETIRED = CL_CAPACITY_PLANNING_ADJ(HARD_WIRED_ON_LINE_YEAR(J), &
                                             AVAILABLE_UNITS,2,.TRUE.)
                        ENDDO
                     ENDIF
! 08/19/04.
! IF THIS IS A CC, THEN TELL THE DUCT FIRING UNIT ITS AVAILABLE
                     IF(INDPENDENT_UNIT(R_POINTER) > 0)THEN
                        INDEPENDENT_UNIT_STATUS(R_POINTER) = INDEPENDENT_UNIT_STATUS(R_POINTER) + 1
                     ENDIF
! IF THIS IS A DUCT FIRING UNIT, THEN TELL THE CC ITS NO LONGER AVAILABLE
                     IF(POINTER_TO_DEPENDENT_UNIT(R_POINTER) > 0) THEN
! NOTE DOUBLE INDEX
                        I = POINTER_TO_DEPENDENT_UNIT(R_POINTER)
                        I = DEPEND_RESOURCE_ID_2_I(I)
                        INDEPENDENT_UNIT_STATUS(I) = INDEPENDENT_UNIT_STATUS(I) - 1
                     ENDIF
                     IF(CONTROLS_DEPEND(RESOURCE_ID_NUM(R_POINTER)))THEN
                        DEPENDED_UNIT_FOUND = .FALSE.
                        DO I = 1, TOTAL_ALL_OPTIONS
                           IF(DEPEND_UNIT_NO(I) > 0 .AND. MUT_EXC_UNIT(I)== RESOURCE_ID_NUM(R_POINTER)) THEN

                              MUT_EXCLUSIVE_CAPACITY_RETIRED = CL_CAPACITY_PLANNING_ADJ(R_YEAR,DEPEND_UNIT_NO(I),2,.TRUE.)
                              DEPEND_UNIT_NO(I) = 0
                              DEPENDED_UNIT_FOUND = .TRUE.
                           ENDIF
                        ENDDO
                        IF(.NOT. DEPENDED_UNIT_FOUND .AND. .NOT. IS_A_HARD_WIRED_UNIT(R_POINTER)) THEN
                           WRITE(4,"(1X,A,I4,A,I4)") 'No depended units found for unit'//trim(unit_names(R_POINTER))// &
                                    'while runing endpoint number',END_POINT,' in year',R_YEAR
                        ENDIF
                     ENDIF
!
!
! 12/17/03. TEST OF UPDATE_PR_ESCALATIONS.
!
                     IF(YEAR > 1) THEN
                      ESCALATIONS_UPDATED = UPDATE_PR_ESCALATIONS(R_YEAR-BASE_YEAR-INT(1,2)-LEAD_TIME(R_POINTER),CL_UNIT_POSITION)
                     ENDIF
!
                  ENDIF ! CL_UNIT_POSITION > 0
               ELSEIF(FILE_SOURCE(R_POINTER) == 'EL') THEN
                  CAPACITY_ADDED = ADD_NEW_EL_UNIT(R_YEAR, &
                                   PRODUCTION_DATA_POINTER(R_POINTER), &
                                   R_OPERATION_LIFE, &
                                   ON_LINE_MONTH, &
                                   PRIMARY_ASSET_CLASS(R_POINTER), &
                                   PRIMARY_ALLOCATION_VECTOR(R_POINTER))
                  IF(CAPACITY_ADDED /= 0.) THEN
                     CAPACITY_ADDED = EL_CAPACITY_PLANNING_ADDITIONS(2)
                     SAVE_NO_EL_ADDITIONS(R_YEAR-BASE_YEAR) = SAVE_NO_EL_ADDITIONS(R_YEAR-BASE_YEAR) + 1
                     AVAILABLE_UNITS = INCREMENT_AVAILABLE_EL_UNITS()
                     HAS_BEEN_SELECTED(R_POINTER) = .TRUE.
                     IF(MUT_EXC_UNIT(R_POINTER) > 0) DEPEND_UNIT_NO(R_POINTER) = AVAILABLE_UNITS
                     IF(CONTROLS_DEPEND(RESOURCE_ID_NUM(R_POINTER)))THEN
                        DEPENDED_UNIT_FOUND = .FALSE.
                        DO I = 1, TOTAL_ALL_OPTIONS
                           IF(DEPEND_UNIT_NO(I) > 0 .AND. MUT_EXC_UNIT(I)== RESOURCE_ID_NUM(R_POINTER)) THEN
                              MUT_EXCLUSIVE_CAPACITY_RETIRED = EL_CAPACITY_PLANNING_REMOVALS(R_YEAR,DEPEND_UNIT_NO(I),2)
                              DEPEND_UNIT_NO(I) = 0
                              DEPENDED_UNIT_FOUND = .TRUE.
                           ENDIF
                        ENDDO
                        IF(.NOT. DEPENDED_UNIT_FOUND) THEN
                           WRITE(4,"(1X,A,I4,A,I4)") 'No depended units found for unit'//trim(unit_names(R_POINTER))// &
                                    'while runing endpoint number',END_POINT,' in year',R_YEAR
                        ENDIF
                     ENDIF
                  ENDIF
               ELSEIF(FILE_SOURCE(R_POINTER) == 'LM') THEN
                  CALL ADD_NEW_LM_UNIT(R_YEAR,PRODUCTION_DATA_POINTER(R_POINTER),R_OPERATION_LIFE,.TRUE.)

                  CALL LM_CAPACITY_PLANNING_ADDITIONS(PRODUCTION_DATA_POINTER(R_POINTER),CAPACITY_ADDED)
               ELSEIF(FILE_SOURCE(R_POINTER) == 'CT') THEN
                  CAPACITY_ADDED = ADD_NEW_CT_UNIT(R_YEAR,PRODUCTION_DATA_POINTER(R_POINTER),R_OPERATION_LIFE)
                  CAPACITY_ADDED = CT_CAPACITY_PLANNING_ADDITIONS(PRODUCTION_DATA_POINTER(R_POINTER))
               ELSEIF(FILE_SOURCE(R_POINTER) == 'De') THEN
                  CAPACITY_ADDED = ENERGY_PRODUCTS_PLANNING_ADD(GRX_RESOURCE_LINK_ID(R_POINTER),R_YEAR,R_OPERATION_LIFE)
! 021422. ADDED SO THAT INDEPENDENT CAPACITY IS COUNTED FOR PLANNING.
! (Probably code that was later deleted.)

               ELSEIF(FILE_SOURCE(R_POINTER) == 'Tr') THEN
                  CAPACITY_ADDED = TRANSMISSION_PLANNING_ADD(GRX_RESOURCE_LINK_ID(R_POINTER),R_YEAR,R_OPERATION_LIFE)
               ENDIF
               CUMULATIVE_UNITS(R_POINTER) = MAX(CUMULATIVE_UNITS(R_POINTER) - 1,0)
            ELSE
               CAPACITY_ADDED = 0.
            ENDIF
            IF(EXPANSION_REPORT()) L_TEMP = WRITE_TO_EXPANSION_REPORT(R_POINTER,R_YEAR)
!
! ADD THE FINANCIAL DATA
!
            IF(INVESTMENT_DATA_POINTER(R_POINTER) > 0) THEN
!
! ONLY CL AND EL NEED FINANCIAL RECORDS CT AND DSM ARE SELF CONTAINED
!
               IF(FILE_SOURCE(R_POINTER) == 'CL' .OR. FILE_SOURCE(R_POINTER) == 'EL') THEN
                  IN_SERVICE_YR = R_YEAR
!
! IF THE UNIT RETIRES DURING THE STUDY PERIOD BUILD IT ONCE
!  ELSE BUILD IT THROUGHT THE EXTENSION PERIOD
!
!
! 090514. GAT.
!
                  IF(IN_SERVICE_YR + OPERATION_LIFE(R_POINTER) <= LAST_STUDY_YEAR) THEN
                     CALL ADD_NEW_UNIT_FINANCIAL(IN_SERVICE_YR, &
                                   ON_LINE_MONTH, &
                                   OPERATION_LIFE(R_POINTER), &
                                   LEAD_TIME(R_POINTER), &
                                   INVESTMENT_DATA_POINTER(R_POINTER), &
                                   ESCALATION_VECTOR(R_POINTER), &
                                   CONSTRUCTION_COSTS(R_POINTER,R_YEAR), &
                                   FILE_SOURCE(R_POINTER), &
                                   PRIMARY_ASSET_CLASS(R_POINTER), &
                                   PRIMARY_ALLOCATION_VECTOR(R_POINTER), &
                            OVN_CAPITAL_COST_STOCASTIC(R_POINTER))
                     CALL INC_PROJECTS_ACCEPTED_RECS
                  ELSE
                     DO WHILE (IN_SERVICE_YR <= LAST_EXTENSION_YEAR)
                        CALL ADD_NEW_UNIT_FINANCIAL(IN_SERVICE_YR, &
                                   ON_LINE_MONTH, &
                                   OPERATION_LIFE(R_POINTER), &
                                   LEAD_TIME(R_POINTER), &
                                   INVESTMENT_DATA_POINTER(R_POINTER), &
                                   ESCALATION_VECTOR(R_POINTER), &
                                   CONSTRUCTION_COSTS(R_POINTER,R_YEAR), &
                                   FILE_SOURCE(R_POINTER), &
                                   PRIMARY_ASSET_CLASS(R_POINTER), &
                                   PRIMARY_ALLOCATION_VECTOR(R_POINTER), &
                            OVN_CAPITAL_COST_STOCASTIC(R_POINTER))
!
                        CALL INC_PROJECTS_ACCEPTED_RECS
                        IN_SERVICE_YR = IN_SERVICE_YR + OPERATION_LIFE(R_POINTER)
                     ENDDO
                  ENDIF
               ENDIF
            ENDIF
            R_ADD_THIS_UNIT = CAPACITY_ADDED
         ELSE
            R_ADD_THIS_UNIT = 0.
         ENDIF
      RETURN
      END
!***********************************************************************
      FUNCTION RESET_NUMBER_OF_OPTIONS_TO_BC() ! INTEGER
!***********************************************************************
      USE CAPACITY_OPTIONS_FIXED_VARS
      INTEGER (KIND=2) :: RESET_NUMBER_OF_OPTIONS_TO_BC

         BASE_OPTIONS = BC_BASE_OPTIONS
         CYCLE_OPTIONS = BC_CYCLE_OPTIONS
         PEAK_OPTIONS = BC_PEAK_OPTIONS
         PEAK_REDUC_OPTIONS = BC_PEAK_REDUC_OPTIONS
         FILL_OPTIONS = BC_FILL_OPTIONS
         HARD_WIRED_OPTIONS = BC_HARD_WIRED_OPTIONS
         MAX_RESOURCE_ID = BC_MAX_RESOURCE_ID
         TOTAL_ACTIVE_OPTIONS = BASE_OPTIONS &
                                + CYCLE_OPTIONS &
                                + PEAK_OPTIONS &
                                + PEAK_REDUC_OPTIONS &
                                + FILL_OPTIONS
         TOTAL_ALL_OPTIONS = TOTAL_ACTIVE_OPTIONS &
                             + HARD_WIRED_OPTIONS
         ALL_OPTIONS_GT_ZERO = TOTAL_ALL_OPTIONS > 0 ! 8/23/96. GAT. ARTMAN PROBLEM
         TOTAL_POSSIBLE_OPTIONS = BC_TOTAL_POSSIBLE_OPTIONS
         OPTIONS_GT_ZERO = TOTAL_POSSIBLE_OPTIONS > 0
         RESET_NUMBER_OF_OPTIONS_TO_BC = TOTAL_ACTIVE_OPTIONS
      RETURN
      END
!***********************************************************************
      FUNCTION STORE_BC_BCPL_OPTIONS()   ! INTEGER
!***********************************************************************
      USE CAPACITY_OPTIONS_FIXED_VARS
      INTEGER (KIND=2) :: STORE_BC_BCPL_OPTIONS
         BC_BASE_OPTIONS = BASE_OPTIONS
         BC_CYCLE_OPTIONS = CYCLE_OPTIONS
         BC_PEAK_OPTIONS = PEAK_OPTIONS
         BC_PEAK_REDUC_OPTIONS = PEAK_REDUC_OPTIONS
         BC_FILL_OPTIONS = FILL_OPTIONS
         BC_HARD_WIRED_OPTIONS = HARD_WIRED_OPTIONS
         BC_MAX_RESOURCE_ID = MAX_RESOURCE_ID
         BC_TOTAL_ACTIVE_OPTIONS = BC_BASE_OPTIONS &
                                   + BC_CYCLE_OPTIONS &
                                   + BC_PEAK_OPTIONS &
                                   + BC_PEAK_REDUC_OPTIONS &
                                   + BC_FILL_OPTIONS
         BC_TOTAL_POSSIBLE_OPTIONS = TOTAL_POSSIBLE_OPTIONS
         BC_TOTAL_ALL_OPTIONS = BC_TOTAL_ACTIVE_OPTIONS &
                                + BC_HARD_WIRED_OPTIONS
         STORE_BC_BCPL_OPTIONS = BC_TOTAL_ACTIVE_OPTIONS
      RETURN
      END
!
!***********************************************************************
      FUNCTION STORE_BCPL_OPTIONS(R_BASE_OPTIONS, &
                                  R_CYCLE_OPTIONS, &
                                  R_PEAK_OPTIONS, &
                                  R_PEAK_REDUC_OPTIONS, &
                                  R_FILL_OPTIONS, &
                                  R_HARD_WIRED_OPTIONS, &
                                  R_MAX_RESOURCE_ID, &
                                  R_TOTAL_POSSIBLE_OPTIONS)
!***********************************************************************
      USE CAPACITY_OPTIONS_FIXED_VARS
      INTEGER (KIND=2) :: STORE_BCPL_OPTIONS
      INTEGER (KIND=2) :: R_BASE_OPTIONS, &
                          R_CYCLE_OPTIONS, &
                          R_PEAK_OPTIONS, &
                          R_PEAK_REDUC_OPTIONS, &
                          R_FILL_OPTIONS, &
                          R_HARD_WIRED_OPTIONS, &
                          R_MAX_RESOURCE_ID, &
                          R_TOTAL_POSSIBLE_OPTIONS
!
         BASE_OPTIONS = R_BASE_OPTIONS
         CYCLE_OPTIONS = R_CYCLE_OPTIONS
         PEAK_OPTIONS = R_PEAK_OPTIONS
         PEAK_REDUC_OPTIONS = R_PEAK_REDUC_OPTIONS
         FILL_OPTIONS = R_FILL_OPTIONS
         HARD_WIRED_OPTIONS = R_HARD_WIRED_OPTIONS
         MAX_RESOURCE_ID = R_MAX_RESOURCE_ID
         TOTAL_ACTIVE_OPTIONS = BASE_OPTIONS+CYCLE_OPTIONS+PEAK_OPTIONS + PEAK_REDUC_OPTIONS + FILL_OPTIONS
         TOTAL_POSSIBLE_OPTIONS = R_TOTAL_POSSIBLE_OPTIONS
         TOTAL_ALL_OPTIONS = TOTAL_ACTIVE_OPTIONS + HARD_WIRED_OPTIONS
         ALL_OPTIONS_GT_ZERO = TOTAL_ALL_OPTIONS > 0 ! 8/23/96. GAT. ARTMAN PROBLEM
         OPTIONS_GT_ZERO = TOTAL_POSSIBLE_OPTIONS > 0
         STORE_BCPL_OPTIONS = TOTAL_ACTIVE_OPTIONS
      RETURN
      END
!***********************************************************************
      FUNCTION NUMBER_OF_ACTIVE_OPTIONS()  ! INTEGER
!***********************************************************************
      USE CAPACITY_OPTIONS_FIXED_VARS
      INTEGER (KIND=2) :: NUMBER_OF_ACTIVE_OPTIONS
         NUMBER_OF_ACTIVE_OPTIONS = TOTAL_ACTIVE_OPTIONS
      RETURN
      END
!***********************************************************************
      FUNCTION NUMBER_OF_POSSIBLE_OPTIONS() ! INTEGER
!***********************************************************************
      USE CAPACITY_OPTIONS_FIXED_VARS
      INTEGER (KIND=2) :: NUMBER_OF_POSSIBLE_OPTIONS
         NUMBER_OF_POSSIBLE_OPTIONS = TOTAL_POSSIBLE_OPTIONS
      RETURN
      END
!***********************************************************************
      FUNCTION NUMBER_OF_ACTIVE_BASE_OPTIONS() ! INTEGER
!***********************************************************************
      USE CAPACITY_OPTIONS_FIXED_VARS
      INTEGER (KIND=2) :: NUMBER_OF_ACTIVE_BASE_OPTIONS
         NUMBER_OF_ACTIVE_BASE_OPTIONS = BASE_OPTIONS
      RETURN
      END
!***********************************************************************
      FUNCTION NUMBER_OF_ACTIVE_CYCL_OPTIONS ()! INTEGER
!***********************************************************************
      USE CAPACITY_OPTIONS_FIXED_VARS
      INTEGER (KIND=2) :: NUMBER_OF_ACTIVE_CYCL_OPTIONS
         NUMBER_OF_ACTIVE_CYCL_OPTIONS = CYCLE_OPTIONS
      RETURN
      END
!***********************************************************************
      FUNCTION NUMBER_OF_ACTIVE_PEAK_OPTIONS() ! INTEGER
!***********************************************************************
      USE CAPACITY_OPTIONS_FIXED_VARS
      INTEGER (KIND=2) :: NUMBER_OF_ACTIVE_PEAK_OPTIONS
         NUMBER_OF_ACTIVE_PEAK_OPTIONS = PEAK_OPTIONS
      RETURN
      END
!***********************************************************************
      FUNCTION NUMBER_OF_ACTIVE_FILL_OPTIONS()  ! INTEGER
!***********************************************************************
      USE CAPACITY_OPTIONS_FIXED_VARS
      INTEGER (KIND=2) :: NUMBER_OF_ACTIVE_FILL_OPTIONS
         NUMBER_OF_ACTIVE_FILL_OPTIONS = FILL_OPTIONS
      RETURN
      END
!***********************************************************************
      FUNCTION NUMBER_OF_ACTIVE_PEAK_REDUCS() ! INTEGER
!***********************************************************************
      USE CAPACITY_OPTIONS_FIXED_VARS
      INTEGER (KIND=2) :: NUMBER_OF_ACTIVE_PEAK_REDUCS
         NUMBER_OF_ACTIVE_PEAK_REDUCS = PEAK_REDUC_OPTIONS
      RETURN
      END
!***********************************************************************
      FUNCTION NUMBER_OF_HARD_WIRED_OPTIONS() ! INTEGER
!***********************************************************************
      USE CAPACITY_OPTIONS_FIXED_VARS
      INTEGER (KIND=2) :: NUMBER_OF_HARD_WIRED_OPTIONS
         NUMBER_OF_HARD_WIRED_OPTIONS = HARD_WIRED_OPTIONS
      RETURN
      END
!***********************************************************************
      FUNCTION OPTIONS_MAX_RESOURCE_ID() ! INTEGER
!***********************************************************************
      USE CAPACITY_OPTIONS_FIXED_VARS
      INTEGER (KIND=2) :: OPTIONS_MAX_RESOURCE_ID
         OPTIONS_MAX_RESOURCE_ID = MAX_RESOURCE_ID
      RETURN
      END
!
!
