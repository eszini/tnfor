!***********************************************************************
!
!        PROGRAM TO READ MULTI-TAB INFORMATION ON WEEKLY HYDRO
!                 AND CONVERT TO BINARY FORMAT
!                       COPYRIGHT (C) 2001
!      ALL RIGHTS RESERVED M.S. GERBER & ASSOCIATES, INC.
!
!***********************************************************************
!
      SUBROUTINE WEEKLY_HYDRO_OBJECT
      use end_routine, only: end_program, er_message
!
!***********************************************************************
!
      INCLUDE 'SpinLib.MON'
      USE SIZECOM
      INTEGER (kind=1) ::  FORECAST_GROWTH_YEARS=AVAIL_DATA_YEARS 
      LOGICAL (kind=1) ::  SAVE_WH_FILE_EXISTS=.FALSE. 
      LOGICAL (kind=1) ::  R_WH_FILE_EXISTS
      LOGICAL (kind=1) ::  R_WH_FILE_USED
      LOGICAL (kind=1) ::  SAVE_WH_FILE_USED=.FALSE. 
      CHARACTER (len=6) ::  BASECASE_MARKET_AREA_ID
      CHARACTER (len=6) ::  BASECASE_TRANS_AREA_ID
      CHARACTER (len=6) ::  BASECASE_NERC_SUB_ID
      CHARACTER (len=20) ::  HYDRO_VARIABLES,HYDRO_TYPE
      CHARACTER (len=1) ::  EXTENSION_PERIOD_GROWTH_SWITCH='X' 
      CHARACTER (len=1) ::  TABLE_ACTIVE
      INTEGER (kind=2) ::  INUNIT
      INTEGER (kind=2) ::  IREC
      INTEGER (kind=2) ::  DELETE
      INTEGER (kind=2) ::  LRECL=350 
      INTEGER (kind=2) ::  BASE_YEAR
      INTEGER (kind=2) ::  R_LRECL
      INTEGER (kind=2) ::  SAVE_TRANS_LOAD_TABLES=0 
      INTEGER (kind=2) ::  R_TRANS_LOAD_TABLES
      INTEGER (kind=2) ::  TEMP_YEAR
      INTEGER (kind=2) ::  R_NUM_OF_TRANS_CLASSES
      INTEGER (kind=2) ::  NUM_OF_OL_TRANS_CLASSES=0 
      INTEGER (kind=2) ::  R_MAX_TRANS_CLASS_NUM
      INTEGER (kind=2) ::  MAX_OL_TRANS_CLASS_ID_NUM=0 
      INTEGER (kind=2) ::  R_NUM_OF_ASSET_CLASSES
      INTEGER (kind=2) ::  NUM_OF_OL_ASSET_CLASSES=0 
      INTEGER (kind=2) ::  R_MAX_ASSET_CLASS_NUM
      INTEGER (kind=2) ::  MAX_OL_ASSET_CLASS_ID_NUM=0 
      INTEGER (kind=2) ::  R_NUM_OF_CUST_CLASSES
      INTEGER (kind=2) ::  NUM_OF_OL_CUST_CLASSES=0 
      INTEGER (kind=2) ::  R_MAX_CUST_CLASS_NUM
      INTEGER (kind=2) ::  MAX_OL_CUST_CLASS_ID_NUM=0 
      INTEGER (kind=2) ::  NUM_OF_BC_TRANS_CLASSES=0 
      INTEGER (kind=2) ::  MAX_BC_TRANS_CLASS_ID_NUM=0 
      INTEGER (kind=2) ::  NUM_OF_BC_ASSET_CLASSES=0 
      INTEGER (kind=2) ::  MAX_BC_ASSET_CLASS_ID_NUM=0 
      INTEGER (kind=2) ::  NUM_OF_BC_CUST_CLASSES=0 
      INTEGER (kind=2) ::  MAX_BC_CUST_CLASS_ID_NUM=0 
      INTEGER (kind=2) ::  TEMP_TRANS_CLASS_POINTER(:)
      INTEGER (kind=2) ::  TEMP_ASSET_CLASS_POINTER(:)
      INTEGER (kind=2) ::  TEMP_CUST_CLASS_POINTER(:)
      INTEGER (kind=4) ::  IOS
      ALLOCATABLE ::
     +            TEMP_TRANS_CLASS_POINTER,
     +            TEMP_ASSET_CLASS_POINTER,
     +            TEMP_CUST_CLASS_POINTER
      CHARACTER (len=5) ::  WEEKLY_HYDRO_FILE
      CHARACTER (len=5) ::  OVERLAY_FAMILY_NAME
      CHARACTER (len=5) ::  BSYRLOAD
      CHARACTER (len=5) ::  REFERENCE_LOAD_NAME
      CHARACTER (len=256) ::  FILE_NAME
      CHARACTER (len=256) ::  OUTPUT_DIRECTORY
      CHARACTER (len=256) ::  BASE_FILE_DIRECTORY
      CHARACTER (len=152) ::  MESSAGE
      LOGICAL (kind=4) ::  FILE_EXISTS
! DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER (len=1024) ::  RECLN
! DECLARATION FOR SYSTEM FORECAST VARIABLES
      INTEGER (kind=2) ::  YEAR,UNIT_NUM=10 ,REFERENCE_LOAD_NUMBER
!      REAL TSY_FC_DATA(4,12)
!      REAL MARKET_RATES_CUSTOMERS(4)
!
! SIMULATION VARIABLES
!
      INTEGER (kind=2) ::
!     +                     YEAR,
     +                     CUSTOMER_GROUP,
     +                     TRANSACTION_GROUP
      REAL (kind=4) ::
     +                     MARKET_ENERGY_PRICE,
     +                     MONTHLY_ENERGY_PRICE_PATTERN,
     +                     MARKET_DEMAND_PRICE,
     +                     MONTHLY_DEMAND_PRICE_PATTERN,
     +                     MARKET_CUSTOMER_PRICE,
     +                     ASSET_CLASS_ID,
     +                     ASSET_CLASS_REV_ALLOC_VECTOR,
     +                     ANNUAL_ENERGY,
     +                     ANNUAL_PEAK,
     +                     ANNUAL_CUSTOMERS,
     +                     ANNUAL_MULTIPLIER,
     +                     WEEKLY_VALUES(52),
     +                     DIST_ENERGY_LOSS_FACTOR,
     +                     TRANS_ENERGY_LOSS_FACTOR,
     +                     PEAK_LOSS_FACTOR,
     +                     PEAK_COIN_FACTOR,
     +                     DISTRIBUTION_PRICE,
     +                     TRANSMISSION_PRICE,
     +                     MINIMUM_MARKET_PRICE,
     +                     MAXIMUM_MARKET_PRICE,
     +                     INDEXED_ENERGY_PRICE,
     +                     PUMPING_CAP_MW,
     +                     PUMPING_STORAGE_EFFICIENCY
      CHARACTER (len=17) ::  FILE_TYPE='Weekly Hydro     ' 
      CHARACTER (len=2) ::  WEEKLY_HYDRO_OL='BC' ,R_WEEKLY_HYDRO_OL
      LOGICAL (kind=1) ::  LAHEY_LF95
      CHARACTER (len=30) ::  SCREEN_OUTPUT
!
      INTEGER (kind=2) ::  WH_YEAR,STUDY_BASE_YEAR
!
      SAVE STUDY_BASE_YEAR
!
!***********************************************************************
!
!          ROUTINE TO CONVERT METAFILE FILES TO DIRECT-ACESS BINARY
!          COPYRIGHT (C) 1983, 84, 85  M.S. GERBER & ASSOCIATES, INC.
!
!***********************************************************************
!
! CONVERT THE SYSTEM-FORECAST FILE
!
!
!***********************************************************************
      ENTRY WEEKLY_HYDRO_MAKEBIN
!***********************************************************************
      STUDY_BASE_YEAR = BASE_YEAR()
      WH_YEAR = 0
      FILE_NAME = trim(BASE_FILE_DIRECTORY())//
     +                      "WHB"//trim(WEEKLY_HYDRO_FILE())//".DAT"
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      IF(EXTENSION_PERIOD_GROWTH_SWITCH == 'F')
     +                          FORECAST_GROWTH_YEARS = AVAIL_DATA_YEARS
      IF(FILE_EXISTS) THEN
         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//WEEKLY_HYDRO_FILE()
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_LOCATE_WRITE(16,30,WEEKLY_HYDRO_FILE(),
     +                                                   ALL_VERSIONS,0)
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         ENDIF
!
         CUSTOMER_GROUP = 0
!
         NUM_OF_BC_TRANS_CLASSES = 0
         MAX_BC_TRANS_CLASS_ID_NUM = 0
         NUM_OF_BC_ASSET_CLASSES = 0
         MAX_BC_ASSET_CLASS_ID_NUM = 0
         NUM_OF_BC_CUST_CLASSES = 0
         MAX_BC_CUST_CLASS_ID_NUM = 0
!
         ALLOCATE(   TEMP_TRANS_CLASS_POINTER(0:1023),
     +               TEMP_ASSET_CLASS_POINTER(0:1023),
     +               TEMP_CUST_CLASS_POINTER(0:1023))
         TEMP_TRANS_CLASS_POINTER = 0
         TEMP_ASSET_CLASS_POINTER = 0
         TEMP_CUST_CLASS_POINTER = 0
!
         SAVE_WH_FILE_EXISTS = .TRUE.
!
         WEEKLY_VALUES = 0.
         OPEN(10,FILE=FILE_NAME)
         OPEN(11,FILE=trim(OUTPUT_DIRECTORY())//"BCWSYFC.BIN",
     +                      ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
!
         TRANSACTION_GROUP = 1
         MARKET_ENERGY_PRICE = 0
         MONTHLY_ENERGY_PRICE_PATTERN = 0.
         MARKET_DEMAND_PRICE = 0
         MONTHLY_DEMAND_PRICE_PATTERN = 0.
         MARKET_CUSTOMER_PRICE = 0.
         ASSET_CLASS_ID = 0.
         ASSET_CLASS_REV_ALLOC_VECTOR = 0.
!
         SAVE_TRANS_LOAD_TABLES = 0
!
         TABLE_ACTIVE = 'T'
         BASECASE_MARKET_AREA_ID = 'BLANK '
         BASECASE_TRANS_AREA_ID = 'BLANK '
         BASECASE_NERC_SUB_ID = 'BLANK '
!
         HYDRO_TYPE = "Pondage"
         PUMPING_CAP_MW = 0.
         PUMPING_STORAGE_EFFICIENCY = 0.
!
         REFERENCE_LOAD_NAME = BSYRLOAD()
         REFERENCE_LOAD_NUMBER = 0
!
         IREC = 0
         READ(10,*) DELETE
         DO
            WH_YEAR = 1
            DO
               READ(10,1000,IOSTAT=IOS) RECLN

               IF(RECLN(1:1) == '7' .OR. IOS /= 0) THEN
                  IF(WH_YEAR <= AVAIL_DATA_YEARS) THEN
                     DO WH_YEAR = WH_YEAR, AVAIL_DATA_YEARS
                        IREC = IREC + 1
                        WRITE(11,REC=IREC) DELETE,
     +                     WH_YEAR+STUDY_BASE_YEAR,
     +                     TABLE_ACTIVE,
     +                     HYDRO_VARIABLES,
     +                     TRANSACTION_GROUP,
     +                     BASECASE_MARKET_AREA_ID,
     +                     BASECASE_TRANS_AREA_ID,
     +                     BASECASE_NERC_SUB_ID,
     +                     MARKET_ENERGY_PRICE,
     +                     MONTHLY_ENERGY_PRICE_PATTERN,
     +                     MARKET_DEMAND_PRICE,
     +                     MONTHLY_DEMAND_PRICE_PATTERN,
     +                     MARKET_CUSTOMER_PRICE,
     +                     DISTRIBUTION_PRICE,
     +                     TRANSMISSION_PRICE,
     +                     ASSET_CLASS_ID,
     +                     ASSET_CLASS_REV_ALLOC_VECTOR,
     +                     WEEKLY_VALUES,
     +                     HYDRO_TYPE,
     +                     PUMPING_CAP_MW,
     +                     PUMPING_STORAGE_EFFICIENCY,
     +                     REFERENCE_LOAD_NAME,
     +                     REFERENCE_LOAD_NUMBER
                     ENDDO
                  ENDIF
!
                  EXIT
!
               ENDIF
               RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               READ(RECLN,*,ERR=200) DELETE,
     +                     YEAR,
     +                     TABLE_ACTIVE,
     +                     HYDRO_VARIABLES,
     +                     TRANSACTION_GROUP,
     +                     BASECASE_MARKET_AREA_ID,
     +                     BASECASE_TRANS_AREA_ID,
     +                     BASECASE_NERC_SUB_ID,
     +                     MARKET_ENERGY_PRICE,
     +                     MONTHLY_ENERGY_PRICE_PATTERN,
     +                     MARKET_DEMAND_PRICE,
     +                     MONTHLY_DEMAND_PRICE_PATTERN,
     +                     MARKET_CUSTOMER_PRICE,
     +                     DISTRIBUTION_PRICE,
     +                     TRANSMISSION_PRICE,
     +                     ASSET_CLASS_ID,
     +                     ASSET_CLASS_REV_ALLOC_VECTOR,
     +                     WEEKLY_VALUES,
     +                     HYDRO_TYPE,
     +                     PUMPING_CAP_MW,
     +                     PUMPING_STORAGE_EFFICIENCY,
     +                     REFERENCE_LOAD_NAME,
     +                     REFERENCE_LOAD_NUMBER
!
               IREC = IREC + 1
!
               READ(RECLN,*,ERR=200) DELETE,YEAR
               TEMP_YEAR = MIN(AVAIL_DATA_YEARS,YEAR - STUDY_BASE_YEAR)
!
               IF(TEMP_TRANS_CLASS_POINTER(TRANSACTION_GROUP) == 0) THEN
                  NUM_OF_BC_TRANS_CLASSES = NUM_OF_BC_TRANS_CLASSES + 1
                  MAX_BC_TRANS_CLASS_ID_NUM = MAX(
     +                      MAX_BC_TRANS_CLASS_ID_NUM,TRANSACTION_GROUP)
                  TEMP_TRANS_CLASS_POINTER(TRANSACTION_GROUP) = 1
               ENDIF
               IF(TEMP_ASSET_CLASS_POINTER(ASSET_CLASS_ID) == 0) THEN
                  NUM_OF_BC_ASSET_CLASSES = NUM_OF_BC_ASSET_CLASSES + 1
                  MAX_BC_ASSET_CLASS_ID_NUM = MAX(
     +                      MAX_BC_ASSET_CLASS_ID_NUM,ASSET_CLASS_ID)
                  TEMP_ASSET_CLASS_POINTER(ASSET_CLASS_ID) = 1
               ENDIF
               IF(TEMP_CUST_CLASS_POINTER(CUSTOMER_GROUP) == 0) THEN
                  NUM_OF_BC_CUST_CLASSES = NUM_OF_BC_CUST_CLASSES + 1
                  MAX_BC_CUST_CLASS_ID_NUM = MAX(
     +                      MAX_BC_CUST_CLASS_ID_NUM,CUSTOMER_GROUP)
                  TEMP_CUST_CLASS_POINTER(CUSTOMER_GROUP) = 1
               ENDIF
!
               IF(TEMP_YEAR > WH_YEAR) THEN
                  IF(IREC == 1) THEN
                     WRITE(4,*) "The first year of the first table in"
                     WRITE(4,*) "the Transact Forecast file is ",YEAR
                     WRITE(4,*) "while the base year set in project"
                     WRITE(4,*) "information is ",STUDY_BASE_YEAR
                     WRITE(4,*) "First forecast year in the Transact"
                     WRITE(4,*) "Forecast file first year must be one"
                     WRITE(4,*) "year greater than the base year."
                     WRITE(4,*) " "
                     er_message='See WARNINGS MESSAGES file.'
                     call end_program(er_message)
                  ENDIF
                  DO WH_YEAR = WH_YEAR, TEMP_YEAR - 1
                     WRITE(11,REC=IREC) DELETE,
     +                     WH_YEAR+STUDY_BASE_YEAR,
     +                     TABLE_ACTIVE,
     +                     HYDRO_VARIABLES,
     +                     TRANSACTION_GROUP,
     +                     BASECASE_MARKET_AREA_ID,
     +                     BASECASE_TRANS_AREA_ID,
     +                     BASECASE_NERC_SUB_ID,
     +                     MARKET_ENERGY_PRICE,
     +                     MONTHLY_ENERGY_PRICE_PATTERN,
     +                     MARKET_DEMAND_PRICE,
     +                     MONTHLY_DEMAND_PRICE_PATTERN,
     +                     MARKET_CUSTOMER_PRICE,
     +                     DISTRIBUTION_PRICE,
     +                     TRANSMISSION_PRICE,
     +                     ASSET_CLASS_ID,
     +                     ASSET_CLASS_REV_ALLOC_VECTOR,
     +                     WEEKLY_VALUES,
     +                     HYDRO_TYPE,
     +                     PUMPING_CAP_MW,
     +                     PUMPING_STORAGE_EFFICIENCY,
     +                     REFERENCE_LOAD_NAME,
     +                     REFERENCE_LOAD_NUMBER
                     IREC = IREC + 1
                  ENDDO

!                  WH_YEAR = WH_YEAR + 1
               ENDIF

!               IF(WH_YEAR /= YEAR - STUDY_BASE_YEAR) THEN
!                  WRITE(4,*) 'The Base TSYtem Forecast File in'
!                  WRITE(4,*) 'Record',WH_YEAR,'  and Year',YEAR
!                  WRITE(4,*) 'is inconsistent with the Base Year',
!     +                                                 STUDY_BASE_YEAR
!                  WRITE(4,*) 'in Set Parameters. Either reset Base '
!                  WRITE(4,*) 'Year in Set Parameters or the Year in'
!                  WRITE(4,*) 'System Forecast.'
!                  WRITE(4,*) ' '
!               ENDIF
               WRITE(11,REC=IREC) DELETE,
     +                     WH_YEAR+STUDY_BASE_YEAR,
     +                     TABLE_ACTIVE,
     +                     HYDRO_VARIABLES,
     +                     TRANSACTION_GROUP,
     +                     BASECASE_MARKET_AREA_ID,
     +                     BASECASE_TRANS_AREA_ID,
     +                     BASECASE_NERC_SUB_ID,
     +                     MARKET_ENERGY_PRICE,
     +                     MONTHLY_ENERGY_PRICE_PATTERN,
     +                     MARKET_DEMAND_PRICE,
     +                     MONTHLY_DEMAND_PRICE_PATTERN,
     +                     MARKET_CUSTOMER_PRICE,
     +                     DISTRIBUTION_PRICE,
     +                     TRANSMISSION_PRICE,
     +                     ASSET_CLASS_ID,
     +                     ASSET_CLASS_REV_ALLOC_VECTOR,
     +                     WEEKLY_VALUES,
     +                     HYDRO_TYPE,
     +                     PUMPING_CAP_MW,
     +                     PUMPING_STORAGE_EFFICIENCY,
     +                     REFERENCE_LOAD_NAME,
     +                     REFERENCE_LOAD_NUMBER
               WH_YEAR = WH_YEAR + 1
!               CALL CALCULATE_TSYTEM_PEAKS(TSY_FC_DATA,WH_YEAR)
            ENDDO
            SAVE_TRANS_LOAD_TABLES = SAVE_TRANS_LOAD_TABLES + 1
            IF(IOS /= 0) EXIT
         ENDDO
         CLOSE(10)
      ELSE IF(INDEX(WEEKLY_HYDRO_FILE(),'NONE') == 0) THEN
         CALL STOP_NOFILE(FILE_TYPE,FILE_NAME)
      ELSE
         OPEN(11,FILE=trim(OUTPUT_DIRECTORY())//"BCWSYFC.BIN",
     +                      ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
!
         SAVE_WH_FILE_EXISTS = .FALSE.
!
      ENDIF
!      ENDFILE(11)
      CLOSE(11)
      IF(ALLOCATED(TEMP_TRANS_CLASS_POINTER))
     +      DEALLOCATE(    TEMP_TRANS_CLASS_POINTER,
     +                     TEMP_ASSET_CLASS_POINTER,
     +                     TEMP_CUST_CLASS_POINTER)
      RETURN
!  200 CALL LOCATE(20,0)
!      WRITE(6,1010) trim(RECLN)
  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from wh_objt SIID346'
      call end_program(er_message)
!***********************************************************************
!
!          ROUTINE TO CREATE OVERLAY FILES
!          COPYRIGHT (C) 1984-88  M.S. GERBER & ASSOCIATES, INC.
!          COPYRIGHT (C) 1991-92  M.S. GERBER & ASSOCIATES, INC.
!
!***********************************************************************
!
!***********************************************************************
      ENTRY WEEKLY_HYDRO_MAKEOVL(OVERLAY_FAMILY_NAME)
!***********************************************************************
      IF(LAHEY_LF95()) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSE
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         CALL LOCATE(10,51)
      ENDIF
      FILE_NAME=trim(OUTPUT_DIRECTORY())//"WHO"//
     +                               trim(OVERLAY_FAMILY_NAME)//".DAT"
      OPEN(10,FILE=FILE_NAME)
      READ(10,*) DELETE
      WH_YEAR = 0
      INUNIT = 12
      IF(WEEKLY_HYDRO_OL == 'BC') THEN
         OPEN(11,FILE=trim(OUTPUT_DIRECTORY())//"BCWSYFC.BIN",
     +                                       ACCESS="DIRECT",RECL=LRECL)
         INUNIT = 11
      ENDIF
!     INQUIRE(UNIT=12,OPENED=FILE_EXISTS)
      FILE_NAME = trim(OUTPUT_DIRECTORY())//"OLWSYFC.BIN"
      OPEN(12,FILE=FILE_NAME,ACCESS="DIRECT",
     +                           STATUS="UNKNOWN",RECL=LRECL,IOSTAT=IOS)
      IF(IOS /= 0) THEN
         CALL IOSTAT_MSG(IOS,MESSAGE)
         WRITE(4,*) trim(MESSAGE)
         er_message='in WEEKLY_HYDRO_MAKEOVL see WARNINGS messages'
         call end_program(er_message)
      ENDIF
      IREC = 0
      DELETE = 1
      READ(10,1000,IOSTAT=IOS) RECLN
!
      DOWHILE(RECLN(1:1) == '7')
!
         READ(10,1000,IOSTAT=IOS) RECLN
!
      ENDDO
      READ(RECLN,*,ERR=200) DELETE,WH_YEAR
!
!
!
      NUM_OF_OL_TRANS_CLASSES = 0
      MAX_OL_TRANS_CLASS_ID_NUM = 0
      NUM_OF_OL_ASSET_CLASSES = 0
      MAX_OL_ASSET_CLASS_ID_NUM = 0
      NUM_OF_OL_CUST_CLASSES = 0
      MAX_OL_CUST_CLASS_ID_NUM = 0
!
      ALLOCATE(TEMP_TRANS_CLASS_POINTER(0:1023),
     +         TEMP_ASSET_CLASS_POINTER(0:1023),
     +         TEMP_CUST_CLASS_POINTER(0:1023))
      TEMP_TRANS_CLASS_POINTER = 0
      TEMP_ASSET_CLASS_POINTER = 0
      TEMP_CUST_CLASS_POINTER = 0
!
!
!
      DO
         IREC = IREC + 1
         READ(INUNIT,REC=IREC,IOSTAT=IOS)
     +                     DELETE,
     +                     YEAR,
     +                     TABLE_ACTIVE,
     +                     HYDRO_VARIABLES,
     +                     TRANSACTION_GROUP,
     +                     BASECASE_MARKET_AREA_ID,
     +                     BASECASE_TRANS_AREA_ID,
     +                     BASECASE_NERC_SUB_ID,
     +                     MARKET_ENERGY_PRICE,
     +                     MONTHLY_ENERGY_PRICE_PATTERN,
     +                     MARKET_DEMAND_PRICE,
     +                     MONTHLY_DEMAND_PRICE_PATTERN,
     +                     MARKET_CUSTOMER_PRICE,
     +                     DISTRIBUTION_PRICE,
     +                     TRANSMISSION_PRICE,
     +                     ASSET_CLASS_ID,
     +                     ASSET_CLASS_REV_ALLOC_VECTOR,
     +                     WEEKLY_VALUES,
     +                     HYDRO_TYPE,
     +                     PUMPING_CAP_MW,
     +                     PUMPING_STORAGE_EFFICIENCY,
     +                     REFERENCE_LOAD_NAME,
     +                     REFERENCE_LOAD_NUMBER
         IF(IOS /= 0) EXIT
!
         IF(YEAR == WH_YEAR) THEN
            RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
            READ(RECLN,*,ERR=200)
     +                     DELETE,
     +                     WH_YEAR,
     +                     TABLE_ACTIVE,
     +                     HYDRO_VARIABLES,
     +                     TRANSACTION_GROUP,
     +                     BASECASE_MARKET_AREA_ID,
     +                     BASECASE_TRANS_AREA_ID,
     +                     BASECASE_NERC_SUB_ID,
     +                     MARKET_ENERGY_PRICE,
     +                     MONTHLY_ENERGY_PRICE_PATTERN,
     +                     MARKET_DEMAND_PRICE,
     +                     MONTHLY_DEMAND_PRICE_PATTERN,
     +                     MARKET_CUSTOMER_PRICE,
     +                     DISTRIBUTION_PRICE,
     +                     TRANSMISSION_PRICE,
     +                     ASSET_CLASS_ID,
     +                     ASSET_CLASS_REV_ALLOC_VECTOR,
     +                     WEEKLY_VALUES,
     +                     HYDRO_TYPE,
     +                     PUMPING_CAP_MW,
     +                     PUMPING_STORAGE_EFFICIENCY,
     +                     REFERENCE_LOAD_NAME,
     +                     REFERENCE_LOAD_NUMBER
            READ(10,1000,IOSTAT=IOS) RECLN
!
            DOWHILE(RECLN(1:1) == '7')
!
               READ(10,1000,IOSTAT=IOS) RECLN
!
            ENDDO
            READ(RECLN,*,ERR=200) DELETE,WH_YEAR
         ENDIF
!
         IF(TEMP_TRANS_CLASS_POINTER(TRANSACTION_GROUP) == 0) THEN
            NUM_OF_OL_TRANS_CLASSES = NUM_OF_OL_TRANS_CLASSES + 1
            MAX_OL_TRANS_CLASS_ID_NUM = MAX(
     +                      MAX_OL_TRANS_CLASS_ID_NUM,TRANSACTION_GROUP)
            TEMP_TRANS_CLASS_POINTER(TRANSACTION_GROUP) = 1
         ENDIF
         IF(TEMP_ASSET_CLASS_POINTER(ASSET_CLASS_ID) == 0) THEN
            NUM_OF_OL_ASSET_CLASSES = NUM_OF_OL_ASSET_CLASSES + 1
            MAX_OL_ASSET_CLASS_ID_NUM = MAX(
     +                      MAX_OL_ASSET_CLASS_ID_NUM,ASSET_CLASS_ID)
            TEMP_ASSET_CLASS_POINTER(ASSET_CLASS_ID) = 1
         ENDIF
         IF(TEMP_CUST_CLASS_POINTER(CUSTOMER_GROUP) == 0) THEN
            NUM_OF_OL_CUST_CLASSES = NUM_OF_OL_CUST_CLASSES + 1
            MAX_OL_CUST_CLASS_ID_NUM = MAX(
     +                      MAX_OL_CUST_CLASS_ID_NUM,CUSTOMER_GROUP)
            TEMP_CUST_CLASS_POINTER(CUSTOMER_GROUP) = 1
         ENDIF
!
         WRITE(12,REC=IREC)  DELETE,
     +                     YEAR,
     +                     TABLE_ACTIVE,
     +                     HYDRO_VARIABLES,
     +                     TRANSACTION_GROUP,
     +                     BASECASE_MARKET_AREA_ID,
     +                     BASECASE_TRANS_AREA_ID,
     +                     BASECASE_NERC_SUB_ID,
     +                     MARKET_ENERGY_PRICE,
     +                     MONTHLY_ENERGY_PRICE_PATTERN,
     +                     MARKET_DEMAND_PRICE,
     +                     MONTHLY_DEMAND_PRICE_PATTERN,
     +                     MARKET_CUSTOMER_PRICE,
     +                     DISTRIBUTION_PRICE,
     +                     TRANSMISSION_PRICE,
     +                     ASSET_CLASS_ID,
     +                     ASSET_CLASS_REV_ALLOC_VECTOR,
     +                     WEEKLY_VALUES,
     +                     HYDRO_TYPE,
     +                     PUMPING_CAP_MW,
     +                     PUMPING_STORAGE_EFFICIENCY,
     +                     REFERENCE_LOAD_NAME,
     +                     REFERENCE_LOAD_NUMBER
!            CALL CALCULATE_TSYTEM_PEAKS(TSY_FC_DATA,WH_YEAR)
      ENDDO
      CLOSE(10)
      CLOSE(12)
      IF(WEEKLY_HYDRO_OL == 'BC') CLOSE(11)
      WEEKLY_HYDRO_OL = 'OL'
      IF(ALLOCATED(TEMP_TRANS_CLASS_POINTER))
     +      DEALLOCATE(    TEMP_TRANS_CLASS_POINTER,
     +                     TEMP_ASSET_CLASS_POINTER,
     +                     TEMP_CUST_CLASS_POINTER)
      RETURN
!
!
!***********************************************************************
      ENTRY RESET_WEEKLY_HYDRO_OL
!***********************************************************************
         WEEKLY_HYDRO_OL = 'BC'
         SAVE_WH_FILE_USED = .FALSE.
      RETURN
!
!***********************************************************************
      ENTRY RETURN_WEEKLY_HYDRO_OL(R_WEEKLY_HYDRO_OL,R_LRECL)
!***********************************************************************
         R_WEEKLY_HYDRO_OL = WEEKLY_HYDRO_OL
         R_LRECL = LRECL
      RETURN
!***********************************************************************
      ENTRY DOES_WEEKLY_HYDRO_FILE_EXIST(R_WH_FILE_EXISTS)
!***********************************************************************
         R_WH_FILE_EXISTS = SAVE_WH_FILE_EXISTS
      RETURN
!***********************************************************************
      ENTRY GET_WEEKLY_HYDRO_TABLES(R_TRANS_LOAD_TABLES,
     +                              R_NUM_OF_TRANS_CLASSES)
!***********************************************************************
         R_TRANS_LOAD_TABLES = SAVE_TRANS_LOAD_TABLES
         IF(WEEKLY_HYDRO_OL == 'OL') THEN
            R_NUM_OF_TRANS_CLASSES = NUM_OF_OL_TRANS_CLASSES
         ELSE
            R_NUM_OF_TRANS_CLASSES = NUM_OF_BC_TRANS_CLASSES
         ENDIF
      RETURN
!***********************************************************************
      ENTRY WH_FILE_USED_THIS_ENDPOINT(R_WH_FILE_USED)
!***********************************************************************
         R_WH_FILE_USED = SAVE_WH_FILE_USED
         SAVE_WH_FILE_USED = .TRUE.
      RETURN
!***********************************************************************
      ENTRY OPEN_WEEKLY_HYDRO_FILE
!***********************************************************************
         OPEN(UNIT_NUM,
     +        FILE=trim(OUTPUT_DIRECTORY())//WEEKLY_HYDRO_OL//
     +        "WSYFC.BIN",ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
      RETURN
!
!***********************************************************************
      ENTRY CLOSE_WEEKLY_HYDRO_FILE
!***********************************************************************
         CLOSE(UNIT_NUM)
      RETURN
!***********************************************************************
      ENTRY RETURN_WH_GROUP_INFO(R_NUM_OF_TRANS_CLASSES,
     +                           R_MAX_TRANS_CLASS_NUM,
     +                           R_NUM_OF_ASSET_CLASSES,
     +                           R_MAX_ASSET_CLASS_NUM,
     +                           R_NUM_OF_CUST_CLASSES,
     +                           R_MAX_CUST_CLASS_NUM)
!***********************************************************************
         IF(WEEKLY_HYDRO_OL == 'OL') THEN
            R_NUM_OF_TRANS_CLASSES = NUM_OF_OL_TRANS_CLASSES
            R_MAX_TRANS_CLASS_NUM = MAX_OL_TRANS_CLASS_ID_NUM
            R_NUM_OF_ASSET_CLASSES = NUM_OF_OL_ASSET_CLASSES
            R_MAX_ASSET_CLASS_NUM = MAX_OL_ASSET_CLASS_ID_NUM
            R_NUM_OF_CUST_CLASSES = NUM_OF_OL_CUST_CLASSES
            R_MAX_CUST_CLASS_NUM = MAX_OL_CUST_CLASS_ID_NUM
         ELSE
            R_NUM_OF_TRANS_CLASSES = NUM_OF_BC_TRANS_CLASSES
            R_MAX_TRANS_CLASS_NUM = MAX_BC_TRANS_CLASS_ID_NUM
            R_NUM_OF_ASSET_CLASSES = NUM_OF_BC_ASSET_CLASSES
            R_MAX_ASSET_CLASS_NUM = MAX_BC_ASSET_CLASS_ID_NUM
            R_NUM_OF_CUST_CLASSES = NUM_OF_BC_CUST_CLASSES
            R_MAX_CUST_CLASS_NUM = MAX_BC_CUST_CLASS_ID_NUM
         ENDIF
      RETURN
!
 1000 FORMAT(A)
 1010 FORMAT('&',A)
      END
!
!
!
!***********************************************************************
!
!        PROGRAM TO MANAGE WEEKLY HYDRO FORECASTS, REVENUES AND COSTS
!           BY TRANSACTION GROUP ASSET CLASS AND CUSTOMER GROUP
!
!                       COPYRIGHT (C) 2001
!          ALL RIGHTS RESERVED M.S. GERBER & ASSOCIATES, INC.
!
!***********************************************************************
!
      FUNCTION MANAGE_WEEKLY_HYDRO_FORECASTS()
!
!
!***********************************************************************
!
!
! LOCAL DATA LIST
!
      USE SIZECOM
      INCLUDE 'SpinLib.MON'
      use globecom
      SAVE

      LOGICAL (kind=1) ::  READ_WEEKLY_HYDRO_DATA
      LOGICAL (kind=1) ::  SAVE_WEEKLY_HYDRO_STATUS=.FALSE. 
      LOGICAL (kind=1) ::  MANAGE_WEEKLY_HYDRO_FORECASTS
      LOGICAL (kind=1) ::  GET_WEEKLY_HYDRO_FORECASTS
      LOGICAL (kind=1) ::  GET_TG_2_HYDRO_WEEK

      LOGICAL (kind=1) ::       DOES_WH_FILE_EXIST
      INTEGER (kind=2) ::  YR
      INTEGER (kind=2) ::  MAKER_TABLES=0 
      INTEGER (kind=2) ::  DELETE
      INTEGER (kind=2) ::  R_MONTH
      INTEGER (kind=2) ::  MAKER_MONTHS
      INTEGER (kind=2) ::  R_DAY
      INTEGER (kind=2) ::  R_YEAR
      INTEGER (kind=2) ::  R_LOAD_UNIT
      INTEGER (kind=2) ::  LOCAL_LOAD_UNIT=3246 
      INTEGER (kind=2) ::  WK
      INTEGER (kind=2) ::  LOCAL_WEEKS
      INTEGER (kind=2) ::  LOCAL_YEARS
      INTEGER (kind=2) ::  NUM_SCEN_VAR
      INTEGER (kind=2) ::  IREC
      INTEGER (kind=2) ::  NUM_HYDRO_TYPES
      INTEGER (kind=2) ::  HYDRO_INDEX
      INTEGER (kind=2) ::  DAY
      INTEGER (kind=2) ::  LREC
      INTEGER (kind=2) ::  R_LREC
      INTEGER (kind=2) ::  LOCAL_LREC
      INTEGER (kind=2) ::  TIMZON
      INTEGER (kind=2) ::  TEMPER
      INTEGER (kind=2) ::  DELTMP
      INTEGER (kind=2) ::  LDE_MONTH
      INTEGER (kind=2) ::  LDE_DAY
      INTEGER (kind=2) ::  LDE_YEAR
      INTEGER (kind=2) ::  DAY_OF_WEEK
      INTEGER (kind=2) ::  MAX_TRANS_GROUP_NUMBER=0 
      INTEGER (kind=2) ::  GET_MAX_TRANS_GROUP_NUMBER
      INTEGER (kind=2) ::  REFERENCE_LOAD_NUMBER
      INTEGER (kind=4) ::       DAILY_LOADS_I4(24)
!
      CHARACTER (len=5) ::     REFERENCE_LOAD_NAME
      CHARACTER (len=8) ::     EEICODE
!
      INTEGER (kind=2) ::  CUM_WEEK_DAYS_IN_YEAR(0:52)
      INTEGER (kind=2) ::  CUM_MONTH_DAYS_FOR_YEAR(13)
      DATA           CUM_WEEK_DAYS_IN_YEAR/0,7,14,21,28,35,42,49,56,
     +                                 63,70,77,84,91,98,105,112,
     +                                 119,126,133,140,147,154,161,
     +                                 168,175,182,189,196,203,210,
     +                                 217,224,231,238,245,252,259,
     +                                 266,273,280,287,294,301,308,
     +                                 315,322,329,336,343,350,357,
     +                                 364/,
     +               CUM_MONTH_DAYS_FOR_YEAR/0,31,59,90,120,151,181,212,
     +                                       243,273,304,334,365/

!
      INTEGER (kind=2) ::       TRANSACTION_GROUP
      INTEGER (kind=2) ::  NUM_OF_TRANS_CLASSES
      INTEGER (kind=2) ::  TG
      INTEGER (kind=2) ::  R_WK
      INTEGER (kind=2) ::  R_YR
      INTEGER (kind=2) ::  R_TG
      INTEGER (kind=2) ::  CURRENT_DAY_IN_YEAR
      INTEGER (kind=2) ::  CURRENT_WEEK_IN_YEAR
      INTEGER (kind=2) ::  HR
      INTEGER (kind=2) ::  LOCAL_DAY
      INTEGER (kind=2) ::  R_HOUR_IN_WEEK
      INTEGER (kind=2) ::  SAVE_HOUR_IN_WEEK=0 
      INTEGER (kind=2) ::  TG_2_HYDRO_WEEK(:)
      REAL (kind=4) ::
     +                     MARKET_ENERGY_PRICE,
     +                     MONTHLY_ENERGY_PRICE_PATTERN,
     +                     MARKET_DEMAND_PRICE,
     +                     MONTHLY_DEMAND_PRICE_PATTERN,
     +                     MARKET_CUSTOMER_PRICE,
     +                     DISTRIBUTION_PRICE,
     +                     TRANSMISSION_PRICE,
     +                     ASSET_CLASS_ID,
     +                     ASSET_CLASS_REV_ALLOC_VECTOR,
     +                     WEEKLY_VALUES(52),
     +                     R_ENERGY,
     +                     R_MINIMUM_MW,
     +                     R_MAXIMUM_MW,
     +                     REMAIN,
     +                     R_WEEKLY_LOAD(168),
     +                     WEEKLY_REFERENCE_LOAD(168),
     +                     PUMP_WEEKLY_LOAD(168),
     +                     SUM_24_I4,PEAK_24_I4,BASE_24_I4,
     +                     SUM_24_R4,PEAK_24_R4,BASE_24_R4,
     +                     WEEKLY_REFERENCE_PEAK,
     +                     WEEKLY_REFERENCE_BASE,
     +                     WEEKLY_REFERENCE_ENERGY,
     +                     HYDRO_WATER_YEAR_MULT,
     +                     GET_SCENARIO_HYDRO_WATER_YEAR,
     +                     GET_HYDRO_WEEK_PLANNING_PEAK,
     +                     PUMPING_CAP_MW,
     +                     PUMPING_STORAGE_EFFICIENCY,
     +                     PUMP_AVG_GEN_CAP_MW,
     +                     PUMP_MIN_GEN_CAP_MW,
     +                     PUMP_MAX_GEN_CAP_MW
      CHARACTER (len=6) ::
     +                     BASECASE_MARKET_AREA_ID,
     +                     BASECASE_TRANS_AREA_ID,
     +                     BASECASE_NERC_SUB_ID
!
! INPUT DATA LIST
!
      INTEGER (kind=4) ::          VALUES_2_ZERO

      INTEGER (kind=2) ::
     +                  SCENARIO_YEAR,
     +                  TABLE,
     +                  SCENARIO_INDEX
      CHARACTER (len=1) ::
     +                  RECORD_IS_ACTIVE
      CHARACTER (len=20) ::
     +                  HYDRO_VARIABLES,
     +                  HYDRO_TYPE
      REAL (kind=4) ::
     +                  WEEK_HYDRO_VARIABLE(:,:,:,:,:)
      ALLOCATABLE ::
     +                  WEEK_HYDRO_VARIABLE,TG_2_HYDRO_WEEK
      LOGICAL (kind=1) ::  LAHEY_LF95
      INTEGER (kind=2) ::  LAST_VALID_REC
!
!      SAVE WEEK_HYDRO_VARIABLE,TG_2_HYDRO_WEEK
!
! END DATA DECLARATIONS
!
!
!
!
         READ_WEEKLY_HYDRO_DATA = .FALSE.
!
         CALL DOES_WEEKLY_HYDRO_FILE_EXIST(DOES_WH_FILE_EXIST)
         IF(DOES_WH_FILE_EXIST) THEN
            CALL OPEN_WEEKLY_HYDRO_FILE
!
            CALL GET_WEEKLY_HYDRO_TABLES(MAKER_TABLES,
     +                                             NUM_OF_TRANS_CLASSES)
!
            LOCAL_WEEKS = 52
            LOCAL_YEARS = 30
            NUM_SCEN_VAR = 3
            NUM_HYDRO_TYPES = 2
!
            MAX_TRANS_GROUP_NUMBER = GET_MAX_TRANS_GROUP_NUMBER()
!
            IF( ALLOCATED(WEEK_HYDRO_VARIABLE) )
     +                            DEALLOCATE(WEEK_HYDRO_VARIABLE,
     +                                       TG_2_HYDRO_WEEK)
            ALLOCATE(
     +            WEEK_HYDRO_VARIABLE(NUM_HYDRO_TYPES,
     +                                NUM_SCEN_VAR,LOCAL_WEEKS,
     +                                LOCAL_YEARS,NUM_OF_TRANS_CLASSES),
     +                          TG_2_HYDRO_WEEK(MAX_TRANS_GROUP_NUMBER))
            VALUES_2_ZERO =
     +                   INT(NUM_HYDRO_TYPES * NUM_SCEN_VAR *
     +                                 LOCAL_WEEKS * LOCAL_YEARS *
     +                                             NUM_OF_TRANS_CLASSES)

            WEEK_HYDRO_VARIABLE = 0.
!
            TG_2_HYDRO_WEEK = 0
!
            TG = 0
!
            DO TABLE = 1, MAKER_TABLES
               DO YR = 1, AVAIL_DATA_YEARS
                  IREC = (TABLE-1)*AVAIL_DATA_YEARS + YR
!                 IF(LAHEY_LF95()) IREC = IREC + 1
                  READ(10,REC=IREC) DELETE,
     +                     SCENARIO_YEAR,
     +                     RECORD_IS_ACTIVE,
     +                     HYDRO_VARIABLES,
     +                     TRANSACTION_GROUP,
     +                     BASECASE_MARKET_AREA_ID,
     +                     BASECASE_TRANS_AREA_ID,
     +                     BASECASE_NERC_SUB_ID,
     +                     MARKET_ENERGY_PRICE,
     +                     MONTHLY_ENERGY_PRICE_PATTERN,
     +                     MARKET_DEMAND_PRICE,
     +                     MONTHLY_DEMAND_PRICE_PATTERN,
     +                     MARKET_CUSTOMER_PRICE,
     +                     DISTRIBUTION_PRICE,
     +                     TRANSMISSION_PRICE,
     +                     ASSET_CLASS_ID,
     +                     ASSET_CLASS_REV_ALLOC_VECTOR,
     +                     WEEKLY_VALUES,
     +                     HYDRO_TYPE,
     +                     PUMPING_CAP_MW,
     +                     PUMPING_STORAGE_EFFICIENCY,
     +                     REFERENCE_LOAD_NAME,
     +                     REFERENCE_LOAD_NUMBER
!
                  IF(RECORD_IS_ACTIVE == 'F') CYCLE
!
                  SELECT CASE(trim(HYDRO_VARIABLES))
                     CASE ('Energy_GWh')
                        SCENARIO_INDEX = 1
                     CASE ('Mininum_MW')
                        SCENARIO_INDEX = 2
                     CASE ('Maximum_MW')
                        SCENARIO_INDEX = 3
                  END SELECT
!
                  SELECT CASE(trim(HYDRO_TYPE))
                     CASE ('Pondage')
                        HYDRO_INDEX = 1
                     CASE ('Pumped Storage')
                        HYDRO_INDEX = 2
                     CASE DEFAULT
                        HYDRO_INDEX = 1
                  END SELECT
!
                  PUMPING_STORAGE_EFFICIENCY =
     +                                    PUMPING_STORAGE_EFFICIENCY*.01
!
                  IF(TG_2_HYDRO_WEEK(TRANSACTION_GROUP) == 0) THEN
                     TG = TG + 1
                     TG_2_HYDRO_WEEK(TRANSACTION_GROUP) = TG
                  ENDIF
!
!                  TG = GET_TRANS_GROUP_POSITION(TRANSACTION_GROUP)
!
                  DO WK = 1, 52
                     WEEK_HYDRO_VARIABLE(HYDRO_INDEX,
     +                                      SCENARIO_INDEX,WK,YR,TG) =
     +                                                 WEEKLY_VALUES(WK)
                  ENDDO
               ENDDO
!
            ENDDO
            READ_WEEKLY_HYDRO_DATA = .TRUE.
            CALL CLOSE_WEEKLY_HYDRO_FILE
         ENDIF
!
         SAVE_WEEKLY_HYDRO_STATUS = READ_WEEKLY_HYDRO_DATA
         MANAGE_WEEKLY_HYDRO_FORECASTS = .TRUE.
      RETURN
!
!***********************************************************************
      ENTRY GET_TG_2_HYDRO_WEEK(R_TG)
!***********************************************************************
         IF(R_TG > 0 .AND. R_TG <= MAX_TRANS_GROUP_NUMBER) THEN
            GET_TG_2_HYDRO_WEEK =    TG_2_HYDRO_WEEK(R_TG) > 0
         ELSE
            GET_TG_2_HYDRO_WEEK = .FALSE.
         ENDIF
      RETURN
!
!***********************************************************************
      ENTRY GET_HYDRO_WEEK_PLANNING_PEAK(R_TG,R_YR,R_MONTH)
!***********************************************************************

         IF(R_TG > 0 .AND. R_TG <= MAX_TRANS_GROUP_NUMBER .AND.
     +                                   TG_2_HYDRO_WEEK(R_TG) > 0) THEN
            IF(R_MONTH == 1) THEN
               GET_HYDRO_WEEK_PLANNING_PEAK = MAX(
     +             WEEK_HYDRO_VARIABLE(1,3,1,R_YR,R_TG),
     +             WEEK_HYDRO_VARIABLE(1,3,2,R_YR,R_TG),
     +             WEEK_HYDRO_VARIABLE(1,3,3,R_YR,R_TG),
     +             WEEK_HYDRO_VARIABLE(1,3,4,R_YR,R_TG))
            ELSEIF(R_MONTH == 2) THEN
               GET_HYDRO_WEEK_PLANNING_PEAK = MAX(
     +             WEEK_HYDRO_VARIABLE(1,3,5,R_YR,R_TG),
     +             WEEK_HYDRO_VARIABLE(1,3,6,R_YR,R_TG),
     +             WEEK_HYDRO_VARIABLE(1,3,7,R_YR,R_TG),
     +             WEEK_HYDRO_VARIABLE(1,3,8,R_YR,R_TG))
            ELSEIF(R_MONTH == 3) THEN
               GET_HYDRO_WEEK_PLANNING_PEAK = MAX(
     +             WEEK_HYDRO_VARIABLE(1,3,9,R_YR,R_TG),
     +             WEEK_HYDRO_VARIABLE(1,3,10,R_YR,R_TG),
     +             WEEK_HYDRO_VARIABLE(1,3,11,R_YR,R_TG),
     +             WEEK_HYDRO_VARIABLE(1,3,12,R_YR,R_TG))
            ELSEIF(R_MONTH == 4) THEN
               GET_HYDRO_WEEK_PLANNING_PEAK = MAX(
     +             WEEK_HYDRO_VARIABLE(1,3,13,R_YR,R_TG),
     +             WEEK_HYDRO_VARIABLE(1,3,14,R_YR,R_TG),
     +             WEEK_HYDRO_VARIABLE(1,3,15,R_YR,R_TG),
     +             WEEK_HYDRO_VARIABLE(1,3,16,R_YR,R_TG),
     +             WEEK_HYDRO_VARIABLE(1,3,17,R_YR,R_TG))
            ELSEIF(R_MONTH == 5) THEN
               GET_HYDRO_WEEK_PLANNING_PEAK = MAX(
     +             WEEK_HYDRO_VARIABLE(1,3,18,R_YR,R_TG),
     +             WEEK_HYDRO_VARIABLE(1,3,19,R_YR,R_TG),
     +             WEEK_HYDRO_VARIABLE(1,3,20,R_YR,R_TG),
     +             WEEK_HYDRO_VARIABLE(1,3,21,R_YR,R_TG))
            ELSEIF(R_MONTH == 6) THEN
               GET_HYDRO_WEEK_PLANNING_PEAK = MAX(
     +             WEEK_HYDRO_VARIABLE(1,3,22,R_YR,R_TG),
     +             WEEK_HYDRO_VARIABLE(1,3,23,R_YR,R_TG),
     +             WEEK_HYDRO_VARIABLE(1,3,24,R_YR,R_TG),
     +             WEEK_HYDRO_VARIABLE(1,3,25,R_YR,R_TG))
            ELSEIF(R_MONTH == 7) THEN
               GET_HYDRO_WEEK_PLANNING_PEAK = MAX(
     +             WEEK_HYDRO_VARIABLE(1,3,26,R_YR,R_TG),
     +             WEEK_HYDRO_VARIABLE(1,3,27,R_YR,R_TG),
     +             WEEK_HYDRO_VARIABLE(1,3,28,R_YR,R_TG),
     +             WEEK_HYDRO_VARIABLE(1,3,29,R_YR,R_TG),
     +             WEEK_HYDRO_VARIABLE(1,3,30,R_YR,R_TG))
            ELSEIF(R_MONTH == 8) THEN
               GET_HYDRO_WEEK_PLANNING_PEAK = MAX(
     +             WEEK_HYDRO_VARIABLE(1,3,31,R_YR,R_TG),
     +             WEEK_HYDRO_VARIABLE(1,3,32,R_YR,R_TG),
     +             WEEK_HYDRO_VARIABLE(1,3,33,R_YR,R_TG),
     +             WEEK_HYDRO_VARIABLE(1,3,34,R_YR,R_TG))
            ELSEIF(R_MONTH == 9) THEN
               GET_HYDRO_WEEK_PLANNING_PEAK = MAX(
     +             WEEK_HYDRO_VARIABLE(1,3,35,R_YR,R_TG),
     +             WEEK_HYDRO_VARIABLE(1,3,36,R_YR,R_TG),
     +             WEEK_HYDRO_VARIABLE(1,3,37,R_YR,R_TG),
     +             WEEK_HYDRO_VARIABLE(1,3,38,R_YR,R_TG),
     +             WEEK_HYDRO_VARIABLE(1,3,39,R_YR,R_TG))
            ELSEIF(R_MONTH == 10) THEN
               GET_HYDRO_WEEK_PLANNING_PEAK = MAX(
     +             WEEK_HYDRO_VARIABLE(1,3,40,R_YR,R_TG),
     +             WEEK_HYDRO_VARIABLE(1,3,41,R_YR,R_TG),
     +             WEEK_HYDRO_VARIABLE(1,3,42,R_YR,R_TG),
     +             WEEK_HYDRO_VARIABLE(1,3,43,R_YR,R_TG))
            ELSEIF(R_MONTH == 11) THEN
               GET_HYDRO_WEEK_PLANNING_PEAK = MAX(
     +             WEEK_HYDRO_VARIABLE(1,3,44,R_YR,R_TG),
     +             WEEK_HYDRO_VARIABLE(1,3,45,R_YR,R_TG),
     +             WEEK_HYDRO_VARIABLE(1,3,46,R_YR,R_TG),
     +             WEEK_HYDRO_VARIABLE(1,3,47,R_YR,R_TG))
            ELSEIF(R_MONTH == 12) THEN
               GET_HYDRO_WEEK_PLANNING_PEAK = MAX(
     +             WEEK_HYDRO_VARIABLE(1,3,48,R_YR,R_TG),
     +             WEEK_HYDRO_VARIABLE(1,3,49,R_YR,R_TG),
     +             WEEK_HYDRO_VARIABLE(1,3,50,R_YR,R_TG),
     +             WEEK_HYDRO_VARIABLE(1,3,51,R_YR,R_TG),
     +             WEEK_HYDRO_VARIABLE(1,3,52,R_YR,R_TG))
            ENDIF
         ELSE
            GET_HYDRO_WEEK_PLANNING_PEAK = 0.
         ENDIF
      RETURN
!
!***********************************************************************
      ENTRY GET_WEEKLY_HYDRO_FORECASTS(
     +                                 R_DAY,
     +                                 R_MONTH,
     +                                 R_YR,
     +                                 R_TG,
     +                                 R_LOAD_UNIT,
     +                                 R_ENERGY,
     +                                 R_MINIMUM_MW,
     +                                 R_MAXIMUM_MW,
     +                                 R_WEEKLY_LOAD,
     +                                 R_HOUR_IN_WEEK,
     +                                 R_LREC)
!***********************************************************************
!
! DOUBLE INDEX
!         TG = TG_2_HYDRO_WEEK(R_TG)
!         TG =
!
!         SAVE_HOUR_IN_WEEK = R_HOUR_IN_WEEK
!
         SCENARIO_INDEX = -99
!
         CALL OPEN_TRANS_HOURLY_LOAD_FILE(
     +                                     REFERENCE_LOAD_NAME,
     +                                     REFERENCE_LOAD_NUMBER,
     +                                     R_YR,LOCAL_LOAD_UNIT,R_MONTH,
     +                                     SCENARIO_INDEX)
!
         CURRENT_DAY_IN_YEAR = R_DAY +
     +                              CUM_MONTH_DAYS_FOR_YEAR(R_MONTH) - 1
         CURRENT_WEEK_IN_YEAR = CURRENT_DAY_IN_YEAR/7 + 1
         REMAIN = MOD(FLOAT(CURRENT_DAY_IN_YEAR),7.)

         R_ENERGY =     0.
         R_MINIMUM_MW = 0.
         R_MAXIMUM_MW = 0.
!
         GET_WEEKLY_HYDRO_FORECASTS = .FALSE.
!
         IF( REMAIN < .00001 .OR. R_DAY == 1) THEN
!
            IF(R_DAY == 1 .AND. REMAIN > .00001) THEN
               LOCAL_LREC = LREC + NINT(REMAIN)
            ELSE
               LOCAL_LREC = R_LREC
            ENDIF
!
            IF(R_TG > 0 .AND. R_TG <= MAX_TRANS_GROUP_NUMBER) THEN
               TG = TG_2_HYDRO_WEEK(R_TG)
            ELSE
               TG = 0
            ENDIF
            IF(TG > 0) THEN
!
               R_WEEKLY_LOAD = 0.
!
               HYDRO_WATER_YEAR_MULT =
     +                       GET_SCENARIO_HYDRO_WATER_YEAR(R_YR,R_MONTH)
!
               R_HOUR_IN_WEEK = 0
               R_MAXIMUM_MW =
     +             WEEK_HYDRO_VARIABLE(1,3,CURRENT_WEEK_IN_YEAR,R_YR,TG)
!
               R_ENERGY = MIN( R_MAXIMUM_MW * 168.,
     +            WEEK_HYDRO_VARIABLE(
     +                              1,1,CURRENT_WEEK_IN_YEAR,R_YR,TG) *
     +                                                          1000. *
     +                                            HYDRO_WATER_YEAR_MULT)
!
               R_MINIMUM_MW = MIN( R_MAXIMUM_MW * 168.,
     +             WEEK_HYDRO_VARIABLE(
     +                              1,2,CURRENT_WEEK_IN_YEAR,R_YR,TG) *
     +                                            HYDRO_WATER_YEAR_MULT)
!
               WEEKLY_REFERENCE_ENERGY = 0.
               WEEKLY_REFERENCE_PEAK = 0.
               WEEKLY_REFERENCE_BASE = 999999.
!
               LOCAL_DAY = 0
!
               IF(LAHEY_LF95()) THEN
                  LAST_VALID_REC = 366
               ELSE
                  LAST_VALID_REC = 365
               ENDIF
               DO DAY = LOCAL_LREC, LOCAL_LREC+6
!
                  LOCAL_DAY = LOCAL_DAY + 1
!
                  IF(DAY <= LAST_VALID_REC) THEN
                     LREC = DAY
                  ELSE
                     LREC = DAY - LAST_VALID_REC
                  ENDIF
!                  READ(R_LOAD_UNIT,REC=LREC)
                  READ(LOCAL_LOAD_UNIT,REC=LREC)
     +                               LDE_MONTH,LDE_DAY,LDE_YEAR,
     +                               EEICODE,DAY_OF_WEEK,
     +                               TIMZON,TEMPER,
     +                               DELTMP,DAILY_LOADS_I4
!
!                    WEEKLY_REFERENCE_ENERGY = WEEKLY_REFERENCE_ENERGY +
!     +                                         SUM_24_I4(DAILY_LOADS_I4)
                  WEEKLY_REFERENCE_PEAK = MAX(WEEKLY_REFERENCE_PEAK ,
     +                                       PEAK_24_I4(DAILY_LOADS_I4))
                  WEEKLY_REFERENCE_BASE = MIN(WEEKLY_REFERENCE_BASE ,
     +                                       BASE_24_I4(DAILY_LOADS_I4))
!
                  HR = (LOCAL_DAY-1)*24 + 1
!
                  WEEKLY_REFERENCE_LOAD(HR) = DAILY_LOADS_I4(1)
                  WEEKLY_REFERENCE_LOAD(HR+1) = DAILY_LOADS_I4(2)
                  WEEKLY_REFERENCE_LOAD(HR+2) = DAILY_LOADS_I4(3)
                  WEEKLY_REFERENCE_LOAD(HR+3) = DAILY_LOADS_I4(4)
                  WEEKLY_REFERENCE_LOAD(HR+4) = DAILY_LOADS_I4(5)
                  WEEKLY_REFERENCE_LOAD(HR+5) = DAILY_LOADS_I4(6)
                  WEEKLY_REFERENCE_LOAD(HR+6) = DAILY_LOADS_I4(7)
                  WEEKLY_REFERENCE_LOAD(HR+7) = DAILY_LOADS_I4(8)
                  WEEKLY_REFERENCE_LOAD(HR+8) = DAILY_LOADS_I4(9)
                  WEEKLY_REFERENCE_LOAD(HR+9) = DAILY_LOADS_I4(10)
                  WEEKLY_REFERENCE_LOAD(HR+10) = DAILY_LOADS_I4(11)
                  WEEKLY_REFERENCE_LOAD(HR+11) = DAILY_LOADS_I4(12)
                  WEEKLY_REFERENCE_LOAD(HR+12) = DAILY_LOADS_I4(13)
                  WEEKLY_REFERENCE_LOAD(HR+13) = DAILY_LOADS_I4(14)
                  WEEKLY_REFERENCE_LOAD(HR+14) = DAILY_LOADS_I4(15)
                  WEEKLY_REFERENCE_LOAD(HR+15) = DAILY_LOADS_I4(16)
                  WEEKLY_REFERENCE_LOAD(HR+16) = DAILY_LOADS_I4(17)
                  WEEKLY_REFERENCE_LOAD(HR+17) = DAILY_LOADS_I4(18)
                  WEEKLY_REFERENCE_LOAD(HR+18) = DAILY_LOADS_I4(19)
                  WEEKLY_REFERENCE_LOAD(HR+19) = DAILY_LOADS_I4(20)
                  WEEKLY_REFERENCE_LOAD(HR+20) = DAILY_LOADS_I4(21)
                  WEEKLY_REFERENCE_LOAD(HR+21) = DAILY_LOADS_I4(22)
                  WEEKLY_REFERENCE_LOAD(HR+22) = DAILY_LOADS_I4(23)
                  WEEKLY_REFERENCE_LOAD(HR+23) = DAILY_LOADS_I4(24)
!
               ENDDO
!
               IF(R_ENERGY > .1 .AND. R_MAXIMUM_MW > R_MINIMUM_MW) THEN
!
!                 Linear mapping above cannot meet all the constraints
!
!                  call NonlinearlyMap(
!     +               WEEKLY_REFERENCE_LOAD,
!     +               WEEKLY_REFERENCE_BASE,
!     +               WEEKLY_REFERENCE_PEAK,
!     +               R_WEEKLY_LOAD,
!     +               R_MINIMUM_MW,
!     +               R_MAXIMUM_MW,
!     +               R_ENERGY,INT2(168))
                  call NonlinearlyMap_R4(
     +               WEEKLY_REFERENCE_LOAD,
     +               WEEKLY_REFERENCE_BASE,
     +               WEEKLY_REFERENCE_PEAK,
     +               R_WEEKLY_LOAD,
     +               R_MINIMUM_MW,
     +               R_MAXIMUM_MW,
     +               R_ENERGY,INT2(168))
               ENDIF
!
! 11/26/02. WEEKLY PUMPED STORAGE HOOKED-UP FOR TVA.
!
               IF(PUMPING_CAP_MW > 0. .AND.
     +                             PUMPING_STORAGE_EFFICIENCY > 0.) THEN
                  PUMP_MAX_GEN_CAP_MW =
     +                        WEEK_HYDRO_VARIABLE(
     +                              2,3,CURRENT_WEEK_IN_YEAR,R_YR,TG)
!
                  PUMP_AVG_GEN_CAP_MW = MIN( PUMP_MAX_GEN_CAP_MW * 168.,
     +                        WEEK_HYDRO_VARIABLE(
     +                            2,1,CURRENT_WEEK_IN_YEAR,R_YR,TG) *
     +                                                        1000. *
     +                                       HYDRO_WATER_YEAR_MULT)/168.
!
                  PUMP_MIN_GEN_CAP_MW = MIN( PUMP_MAX_GEN_CAP_MW * 168.,
     +                        WEEK_HYDRO_VARIABLE(
     +                              2,2,CURRENT_WEEK_IN_YEAR,R_YR,TG) *
     +                                            HYDRO_WATER_YEAR_MULT)
!
                  IF(PUMP_MAX_GEN_CAP_MW > 0. .AND.
     +                                    PUMP_AVG_GEN_CAP_MW > 0.) THEN
                     call OptimizeWeeksPumpedStorage(
     +                  PUMPING_CAP_MW,
     +                  PUMP_MIN_GEN_CAP_MW,
     +                  PUMP_MAX_GEN_CAP_MW,
     +                  PUMP_AVG_GEN_CAP_MW,
     +                  PUMPING_STORAGE_EFFICIENCY,
     +                  WEEKLY_REFERENCE_LOAD,
     +                  PUMP_WEEKLY_LOAD)
                     DO YR = 1, 168
                        R_WEEKLY_LOAD(YR) = R_WEEKLY_LOAD(YR) +
     +                           PUMP_WEEKLY_LOAD(YR) -
     +                                         WEEKLY_REFERENCE_LOAD(YR)
                     ENDDO
                  ENDIF
               ENDIF
!
               GET_WEEKLY_HYDRO_FORECASTS = .TRUE.
!
            ENDIF
         ENDIF
!
         CALL CLOSE_TRANS_HOURLY_LOAD_FILE(LOCAL_LOAD_UNIT)
!
!         R_HOUR_IN_WEEK = SAVE_HOUR_IN_WEEK + 24
!
      RETURN
!
      END
!-----
!     Note to GAT from AGT:  this is the driver used to test NonlinearlyMap
!     with two cases of extreme targets on 20010612:
!     program RescaleTest
!     integer*2 i
!     real*4 x(168),y(168),xMin,xMax
!
!     xMin=100.0
!     xMax=1000.0
!     do i=1,168
!       x(i)=xMin+(xMax-xMin)*float(i-1)/167.0
!     end do
!     first we test with ySum a little greater than nCases*yMin
!     call NonlinearlyMap(x,xMin,xMax,y,100.0,2000.0,20000.0,168)
!     next we test with ySum a little less than nCases*yMax
!     call NonlinearlyMap(x,xMin,xMax,y,100.0,2000.0,300000.0,168)
!     stop 'end of Rescale Test'
!     end
!-----
!
!
!
!***********************************************************************
      SUBROUTINE CONVERT_MONTHLY_NONLIN_LOADS(
!     +               R_MINIMUM_MW,
     +               R_MAXIMUM_MW,
     +               R_ENERGY,
     +               R_MONTH_HOURS)
!***********************************************************************
!
! SOMEWHAT COMPLEX. REQUIRES FOUR CALL POINTS
!
      USE ArrayAllocationInterface
      INTEGER (kind=2) ::  I
      INTEGER (kind=2) ::  J
      INTEGER (kind=2) ::  R_DAY
      INTEGER (kind=2) ::  R_MONTH_HOURS
         INTEGER (kind=4) ::    R_I4_LOADS(24)
         REAL (kind=4) ::
     +               R_MAXIMUM_MW,
     +               R_ENERGY,
     +               R_R4_LOADS(24)
         REAL (kind=8) ::
     +               MONTHLY_REFERENCE_LOAD(:),
     +               MONTHLY_REFERENCE_BASE,
     +               MONTHLY_REFERENCE_PEAK,
     +               MONTHLY_FINAL_LOAD(:),
     +               MINIMUM_MW,
     +               MAXIMUM_MW,
     +               ENERGY
         ALLOCATABLE ::
     +               MONTHLY_REFERENCE_LOAD,
     +               MONTHLY_FINAL_LOAD
!
! END DATA DECLARATIONS
!
!
         MAXIMUM_MW = R_MAXIMUM_MW
         ENERGY = R_ENERGY
!
         IF( ABS(MONTHLY_REFERENCE_PEAK) > .1) THEN
            MINIMUM_MW = R_MAXIMUM_MW * (MONTHLY_REFERENCE_BASE/
     +                                           MONTHLY_REFERENCE_PEAK)
         ELSE
            MINIMUM_MW = 0.
         ENDIF
!
         call MonthNonlinearlyMap(
     +               MONTHLY_REFERENCE_LOAD,
     +               MONTHLY_REFERENCE_BASE,
     +               MONTHLY_REFERENCE_PEAK,
     +               MONTHLY_FINAL_LOAD,
     +               MINIMUM_MW,
     +               MAXIMUM_MW,
     +               ENERGY,
     +               R_MONTH_HOURS)
      RETURN
!***********************************************************************
      ENTRY INIT_DAILY_NONLIN_LOADS(R_MONTH_HOURS)
!***********************************************************************
!         IF(ALLOCATED(MONTHLY_FINAL_LOAD))
!     +                                    DEALLOCATE(MONTHLY_FINAL_LOAD)
!         ALLOCATE(MONTHLY_FINAL_LOAD(R_MONTH_HOURS))
!         IF(ALLOCATED(MONTHLY_REFERENCE_LOAD))
!     +                                DEALLOCATE(MONTHLY_REFERENCE_LOAD)
!         ALLOCATE(MONTHLY_REFERENCE_LOAD(R_MONTH_HOURS))
         CALL AllocateArray(MONTHLY_FINAL_LOAD,R_MONTH_HOURS)
         CALL AllocateArray(MONTHLY_REFERENCE_LOAD,R_MONTH_HOURS)
         MONTHLY_REFERENCE_BASE = 999999.
         MONTHLY_REFERENCE_PEAK = 0.
      RETURN
!***********************************************************************
      ENTRY ACCUM_DAILY_I4_NONLIN_LOADS(R_DAY,R_I4_LOADS)
!***********************************************************************
         DO I = 1, 24
            J = (R_DAY-1)*24 + I
            MONTHLY_REFERENCE_LOAD(J) = DBLE(R_I4_LOADS(I))
!
            MONTHLY_REFERENCE_BASE = MIN(MONTHLY_REFERENCE_BASE,
     +                                        MONTHLY_REFERENCE_LOAD(J))
            MONTHLY_REFERENCE_PEAK = MAX(MONTHLY_REFERENCE_PEAK,
     +                                        MONTHLY_REFERENCE_LOAD(J))
!
         ENDDO
      RETURN
!***********************************************************************
      ENTRY GET_DAILY_R4_NONLIN_LOADS(R_DAY,R_R4_LOADS)
!***********************************************************************
         DO I = 1, 24
            J = (R_DAY-1)*24 + I
            R_R4_LOADS(I) = MONTHLY_FINAL_LOAD(J)
         ENDDO
      RETURN
!
      END
!
!
!
!***********************************************************************
      subroutine NonlinearlyMap(x,xMin,xMax,y,yMin,yMax,ySum,nCases)
      use end_routine, only: end_program, er_message
!***********************************************************************
!     for feasibility, we require yMin<(ySum/nCases)<yMax
      logical (kind=1) ::  yDecreasing
      integer (kind=2) ::  nCases,iCase,iExpo,nxUnique
      real (kind=8) ::
     +  x(*),y(*),yMin,yMax,ySum,xMin,xMax,
     +  PrevSumPower,ThisSumPower,TargSumPower,ErrorSumPowr,
     +  PrevExponent,ThisExponent,NextExponent,xSpan,ySpan,
     +  LoopGain,ErrToler,z(744)
      parameter(ErrToler=0.000001d0)
!
!     Using a transformation form of
!       zi=(yi-yMin)/(yMax-yMin)=[(xi-xMin)/(xMax-xMin)]**b,
!     use Newton's Method to converge on exponent b which makes
!       sum(yi,over i on [1,nCases])=ySum, or equivalently
!       sum(yi-yMin,over i on [1,nCases])=ySum-nCases*yMin, or
!       sum(zi,over i on [1,nCases])=(ySum-nCases*yMin)/(yMax-yMin),
!     subject (by construction) to
!       xMin=>yMin and
!       xMax=>yMax
!     and assuming the sequence in array x is monotonic
      if(nCases>744)
     +  stop 'array-size limit exceeded in NonlinearlyMap'
      xSpan=xMax-xMin
      ySpan=yMax-yMin

      if(ySpan<0.0d0) then
        er_message='Domain=range void zero NonlinearlyMap SIID349'
        call end_program(er_message)
      endif
!     begin with an exponent whose effects are simple
      nxUnique=1
      if(xSpan>0.0d0) then
        PrevExponent=1.0d0
        PrevSumPower=0.0d0
        do iCase=1,nCases
          z(iCase)=(x(iCase)-xMin)/xSpan
          PrevSumPower=PrevSumPower+z(iCase)
          if(iCase==1) cycle
          if(z(iCase).ne.z(iCase-1)) nxUnique=nxUnique+1
        end do
      end if
      if(ySpan>0.0d0) then
        if(nxUnique<=2) then
!
!
!
          yDecreasing=(y(1)>yMin)
          do iCase=1,nCases
            if(yDecreasing) then
              z(iCase)=dble(nCases-iCase)/dble(nCases-1)
            else
              z(iCase)=dble(iCase-1)/dble(nCases-1)
            end if
            PrevSumPower=PrevSumPower+z(iCase)
          end do
        end if
        TargSumPower=(ySum-yMin*dble(nCases))/ySpan
!       write(4,'(i4,4x,2f9.4,2f12.6,a)') nCases,
!    +    yMin,ySpan,ySum,TargSumPower,' SumPow target'
!     else assume incoming ySum=nCases*yMin=nCases*yMax
      end if
      if((TargSumPower<ErrToler).or.(ySpan<=0.0d0)) then
        iExpo=-1
      else
!     ! perturb PrevExponent slightly to get an initial slope
        ThisExponent=1.01d0
        do iExpo=1,32
          ThisSumPower=0.0d0
          do iCase=1,nCases
!           if(x(iCase)>xMin) ThisSumPower=
            if(z(iCase)>0.0d0)ThisSumPower=
     +                        ThisSumPower+z(iCase)**ThisExponent
          end do
          ErrorSumPowr=ThisSumPower-TargSumPower
!         write(4,'(2i4,2f9.4,3f12.6,a)') nCases,iExpo,
!    +      PrevExponent,ThisExponent,
!    +      PrevSumPower,ThisSumPower,ErrorSumPowr,' SumPow error'
          if(dabs(ErrorSumPowr)<=ErrToler*TargSumPower) exit
          if(dabs(ThisSumPower-PrevSumPower)<=ErrToler*PrevSumPower)exit
          if(ThisExponent*PrevExponent<0.0) then
            LoopGain=0.5d0
          else
            LoopGain=1.0d0
          end if
          NextExponent=ThisExponent-ErrorSumPowr*LoopGain*
     +      (ThisExponent-PrevExponent)/
     +      (ThisSumPower-PrevSumPower)
          PrevExponent=ThisExponent
          PrevSumPower=ThisSumPower
          ThisExponent=NextExponent
        end do
      end if
      if(ThisExponent<0.0001d0) then
!     ! iExpo=0
!     ! stop 'negative exponent in NonlinearlyMap => infeasible request'
        iExpo=0
      end if
!
!     having determined the best-fit ThisExponent, assign output values
!     ThisSumPower=0.0d0
      do iCase=1,nCases
        if(iExpo>0) then
          if(z(iCase)>0.0d0) then
            y(iCase)=yMin+ySpan*z(iCase)**ThisExponent
          else
            y(iCase)=yMin
          end if
        elseif(iExpo==0) then
          if(x(iCase)>xMin) then
            y(iCase)=yMax
          else
            y(iCase)=ySum-dble(nCases-1)*yMax
          end if
!     ! else iExpo==-1; retain incoming or assigned curve's shape
        end if
!       ThisSumPower=ThisSumPower+y(iCase)
!       write(4,'(2i4,f8.5,f11.3,2f13.3,a)') nxUnique,iCase,z(iCase),
!    +    y(iCase),ThisSumPower,ThisSumPower-ySum,' after NonlMap'
      end do
      end
!-----
!***********************************************************************
      subroutine NonlinearlyMap_R4(x,xMin,xMax,y,yMin,yMax,ySum,nCases)
      use end_routine, only: end_program, er_message
!***********************************************************************
!     for feasibility, we require yMin<(ySum/nCases)<yMax
      logical (kind=1) ::  yDecreasing
      integer (kind=2) ::  nCases,iCase,iExpo,nxUnique
      real (kind=4) ::  x(*),y(*),yMin,yMax,ySum,xMin,xMax
      real (kind=8) ::
!     +  x(*),y(*),yMin,yMax,ySum,xMin,xMax,
     +  PrevSumPower,ThisSumPower,TargSumPower,ErrorSumPowr,
     +  PrevExponent,ThisExponent,NextExponent,xSpan,ySpan,
     +  LoopGain,ErrToler,z(744)
      parameter(ErrToler=0.000001d0)
!
!     Using a transformation form of
!       zi=(yi-yMin)/(yMax-yMin)=[(xi-xMin)/(xMax-xMin)]**b,
!     use Newton's Method to converge on exponent b which makes
!       sum(yi,over i on [1,nCases])=ySum, or equivalently
!       sum(yi-yMin,over i on [1,nCases])=ySum-nCases*yMin, or
!       sum(zi,over i on [1,nCases])=(ySum-nCases*yMin)/(yMax-yMin),
!     subject (by construction) to
!       xMin=>yMin and
!       xMax=>yMax
!     and assuming the sequence in array x is monotonic
      if(nCases>744) then
        call end_program('array-size limit exceeded in NonlinearlyMap')
      endif
      xSpan=xMax-xMin
      ySpan=yMax-yMin
!     write(4,'(5e14.6,i6,a)') xMin,xMax,yMin,yMax,ySum,nCases,' NLM'
!     if((xSpan<=0.0d0).or.(ySpan<=0.0d0)) stop
!    +  'Domain or range is void or zero in NonlinearlyMap'
      if(ySpan<0.0d0) then
         er_message='Stop requested from wh_objt SIID350'
         call end_program(er_message)
      endif

!     begin with an exponent whose effects are simple
      nxUnique=1
      if(xSpan>0.0d0) then
        PrevExponent=1.0d0
        PrevSumPower=0.0d0
        do iCase=1,nCases
          z(iCase)=(x(iCase)-xMin)/xSpan
          PrevSumPower=PrevSumPower+z(iCase)
          if(iCase==1) cycle
          if(z(iCase).ne.z(iCase-1)) nxUnique=nxUnique+1
        end do
      end if
      if(ySpan>0.0d0) then
        if(nxUnique<=2) then
!
!
!
          yDecreasing=(y(1)>yMin)
          do iCase=1,nCases
            if(yDecreasing) then
              z(iCase)=dble(nCases-iCase)/dble(nCases-1)
            else
              z(iCase)=dble(iCase-1)/dble(nCases-1)
            end if
            PrevSumPower=PrevSumPower+z(iCase)
          end do
        end if
        TargSumPower=(ySum-yMin*dble(nCases))/ySpan
!       write(4,'(i4,4x,2f9.4,2f12.6,a)') nCases,
!    +    yMin,ySpan,ySum,TargSumPower,' SumPow target'
!     else assume incoming ySum=nCases*yMin=nCases*yMax
      end if
      if((TargSumPower<ErrToler).or.(ySpan<=0.0d0)) then
        iExpo=-1
      else
!     ! perturb PrevExponent slightly to get an initial slope
        ThisExponent=1.01d0
        do iExpo=1,32
          ThisSumPower=0.0d0
          do iCase=1,nCases
!           if(x(iCase)>xMin) ThisSumPower=
            if(z(iCase)>0.0d0)ThisSumPower=
     +                        ThisSumPower+z(iCase)**ThisExponent
          end do
          ErrorSumPowr=ThisSumPower-TargSumPower
!         write(4,'(2i4,2f9.4,3f12.6,a)') nCases,iExpo,
!    +      PrevExponent,ThisExponent,
!    +      PrevSumPower,ThisSumPower,ErrorSumPowr,' SumPow error'
          if(dabs(ErrorSumPowr)<=ErrToler*TargSumPower) exit
          if(dabs(ThisSumPower-PrevSumPower)<=ErrToler*PrevSumPower)exit
          if(ThisExponent*PrevExponent<0.0) then
            LoopGain=0.5d0
          else
            LoopGain=1.0d0
          end if
          NextExponent=ThisExponent-ErrorSumPowr*LoopGain*
     +      (ThisExponent-PrevExponent)/
     +      (ThisSumPower-PrevSumPower)
          PrevExponent=ThisExponent
          PrevSumPower=ThisSumPower
          ThisExponent=NextExponent
        end do
      end if
      if(ThisExponent<0.0001d0) then
!     ! iExpo=0
!     ! stop 'negative exponent in NonlinearlyMap => infeasible request'
        iExpo=0
      end if
!
!     having determined the best-fit ThisExponent, assign output values
!     ThisSumPower=0.0d0
      do iCase=1,nCases
        if(iExpo>0) then
          if(z(iCase)>0.0d0) then
            y(iCase)=yMin+ySpan*z(iCase)**ThisExponent
          else
            y(iCase)=yMin
          end if
        elseif(iExpo==0) then
          if(x(iCase)>xMin) then
            y(iCase)=yMax
          else
            y(iCase)=ySum-dble(nCases-1)*yMax
          end if
!     ! else iExpo==-1; retain incoming or assigned curve's shape
        end if
!       ThisSumPower=ThisSumPower+y(iCase)
!       write(4,'(2i4,f8.5,f11.3,2f13.3,a)') nxUnique,iCase,z(iCase),
!    +    y(iCase),ThisSumPower,ThisSumPower-ySum,' after NonlMap'
      end do
      end
!-----
!***********************************************************************
      subroutine MonthNonlinearlyMap(
     +                              x,xMin,xMax,y,yMin,yMax,ySum,nCases)
      use end_routine, only: end_program, er_message
!***********************************************************************
!     for feasibility, we require yMin<(ySum/nCases)<yMax
      logical (kind=1) ::  Feasible
      logical (kind=1) ::  yDecreasing,L1_FALSE=.false. ,MappingFailed
      integer (kind=2) ::  nCases
      integer (kind=2) ::  iCase
      integer (kind=2) ::  jCase
      integer (kind=2) ::  pCase
      integer (kind=2) ::  iExpo
      integer (kind=2) ::  nxUnique
      integer (kind=2) ::  nz0
      integer (kind=2) ::  nz1
      integer (kind=2) ::  nHalfCycles
      integer (kind=2) ::  iHalfCycle
      integer (kind=2) ::  modulus
      integer (kind=2) ::  OrgOrder(:)
      real (kind=8) ::
     +  x(*),y(*),yMin,yMax,ySum,xMin,xMax,
     +  PrevSumPower,ThisSumPower,TargSumPower,ErrorSumPowr,
     +  PrevExponent,ThisExponent,NextExponent,xSpan,ySpan,z(:),
     +  xDiff,pDiff,LoopGain,ErrToler,LimExponent
!
      parameter(
     +  ErrToler=0.000001d0,
     +  LimExponent=999.9d0)
      allocatable :: z,OrgOrder
!
! END DECLARATIONS
!
!     Using a transformation form of
!       zi=(yi-yMin)/(yMax-yMin)=[(xi-xMin)/(xMax-xMin)]**b,
!     use Newton's Method to converge on exponent b which makes
!       sum(yi,over i on [1,nCases])=ySum, or equivalently
!       sum(yi-yMin,over i on [1,nCases])=ySum-nCases*yMin, or
!       sum(zi,over i on [1,nCases])=(ySum-nCases*yMin)/(yMax-yMin),
!     subject (by construction) to
!       xMin=>yMin and
!       xMax=>yMax
!     and where the sequence in array x may be non-monotonic (even cyclic)
      MappingFailed = .false.
      if(allocated(z)) deallocate(z,OrgOrder)
      allocate(z(nCases))
      allocate(OrgOrder(nCases))
      do iCase=1,nCases
        OrgOrder(iCase)=iCase
      end do
      call Indexed_SortR8_Ascending(x,OrgOrder,nCases,L1_FALSE)
      nxUnique=1
      nHalfCycles=1
      pCase=OrgOrder(1)
      pDiff=0.0d0
      do jCase=2,nCases
        iCase=OrgOrder(jCase)
        if(x(iCase).ne.x(pCase)) nxUnique=nxUnique+1
        xDiff=x(jCase)-x(jCase-1)
        if(xDiff*pDiff<0.0d0) nHalfCycles=nHalfCycles+1
!       write(4,'(4i4,2f9.4,2f12.6,a)')jCase,iCase,nHalfCycles,nxUnique,
!    +    x(jCase),x(iCase),pDiff,xDiff,' p/xDiff'
        pCase=iCase
        if(dabs(xDiff)>0.0d0) pDiff=xDiff
      end do
      xSpan=xMax-xMin
      ySpan=yMax-yMin
      if(ySpan<0.0d0) then
        er_message='Output range negative: MonthNonlinearlyMap SIID351'
        call end_program(er_message)
      endif

!     begin with an exponent whose effects are simple
      PrevExponent=1.0d0
      PrevSumPower=0.0d0
      nz0=0
      nz1=0
      if(xSpan>0.0d0) then
        do iCase=1,nCases
          z(iCase)=(x(iCase)-xMin)/xSpan
!         if(z(iCase)<0.000001d0) nz0=nz0+1
!         if(z(iCase)>0.999999d0) nz1=nz1+1
          PrevSumPower=PrevSumPower+z(iCase)
        end do
!       PrevSumPower=PrevSumPower-dble(nz1)
      end if
      if(ySpan>0.0d0) then
        if((nxUnique<=2)
!
!
!
     +    .or.((nxUnique<nHalfCycles*3).and.(nHalfCycles==nCases/12)))
     +    then
          yDecreasing=(y(1)>yMin)
          modulus=nCases
          if(nHalfCycles>1) modulus=nCases/nHalfCycles
          PrevSumPower=0.0d0
          do iCase=1,nCases
!           if(yDecreasing) then
!             z(iCase)=dble(nCases-iCase)/dble(nCases-1)
!           else
!             z(iCase)=dble(iCase-1)/dble(nCases-1)
!           end if
            if((nxUnique>2).and.(x(iCase).ne.xMin)
     +                     .and.(x(iCase).ne.xMax)) then
              continue
            elseif(yDecreasing) then
              z(iCase)=dble(mod(nCases-iCase,modulus))/dble(modulus-1)
            else
              z(iCase)=dble(mod(iCase-1,modulus))/dble(modulus-1)
            end if
            iHalfCycle=1+(iCase-1)/modulus
            if(z(iCase)<0.001d0) nz0=nz0+1
            if(z(iCase)>0.999d0) nz1=nz1+1
            if(mod(iHalfCycle,2)==0) z(iCase)=1.0d0-z(iCase)
            PrevSumPower=PrevSumPower+z(iCase)
!           write(4,'(4i4,f9.6,f12.6,a)') iHalfCycle,iCase,nz0,nz1,
!    +        z(iCase),PrevSumPower,' PSP'
          end do
!         PrevSumPower=PrevSumPower-dble(nz1)
        end if
        TargSumPower=(ySum-yMin*dble(nCases))/ySpan
!    +    -dble(nz1)
!       write(4,'(4i4,3f9.4,2f13.6,a)') nHalfCycles,nCases,nz0,nz1,
!    +    yMin,yMax,ySpan,ySum,TargSumPower,' SumPow target'
        Feasible=.true.
        if(TargSumPower>PrevSumPower) then
          Feasible=(TargSumPower<dble(nCases-nz0))
          if(.not.Feasible) then
!
            nz1=nCases-nz0
            yMax=(ySum-yMin*dble(nz0))/dble(nz1)
            write(4,'(f9.4,a)') yMax,' Infeasible MNLM yMax relaxed.'
          end if
        else
          Feasible=(TargSumPower>dble(nz1))
          if(.not.Feasible) then
!
            nz0=nCases-nz1
            yMin=(ySum-yMax*dble(nz1))/dble(nz0)
            write(4,'(f9.4,a)') yMin,' Infeasible MNLM yMin relaxed.'
          end if
        end if
        if(.not.Feasible) then
          do jCase=1,nCases
            iCase=OrgOrder(jCase)
            if(jCase<=nz1) then
              y(iCase)=yMax
            else
              y(iCase)=yMin
            end if
          end do
          return
        end if
!     else assume incoming ySum=nCases*yMin=nCases*yMax
      end if
      if((TargSumPower<ErrToler).or.(ySpan<=0.0d0)) then
        iExpo=-1
      else
!     ! perturb PrevExponent slightly to get an initial slope
        ThisExponent=1.01d0
        do iExpo=1,32
          ThisSumPower=0.0d0
          do iCase=1,nCases
!           if(x(iCase)>xMin) ThisSumPower=
            if(z(iCase)>0.0d0)ThisSumPower=
     +                        ThisSumPower+z(iCase)**ThisExponent
          end do
          ErrorSumPowr=ThisSumPower-TargSumPower
!         write(4,'(2i4,2f9.4,3f12.6,a)') nCases,iExpo,
!    +      PrevExponent,ThisExponent,
!    +      PrevSumPower,ThisSumPower,ErrorSumPowr,' SumPow error'
          if(dabs(ErrorSumPowr)<ErrToler*TargSumPower) exit
          if(dabs(ThisSumPower-PrevSumPower)<ErrToler*PrevSumPower) then
            write(4,'(4i4,3f9.4,2f13.6,a)') nHalfCycles,nCases,nz0,nz1,
     +        yMin,yMax,ySpan,ySum,TargSumPower,' SumPow target'
            write(4,'(a)') ' MonthNonLinearlyMap failed; check inputs.'
            MappingFailed = .true.
            exit
          end if
          if(ThisExponent*PrevExponent<0.0d0) then
            LoopGain=0.5d0
          else
            LoopGain=1.0d0
          end if
          NextExponent=ThisExponent-ErrorSumPowr*LoopGain*
     +      (ThisExponent-PrevExponent)/
     +      (ThisSumPower-PrevSumPower)
          if(ThisExponent*NextExponent<0.0d0)
     +      NextExponent=NextExponent/16.0d0
          PrevExponent=ThisExponent
          PrevSumPower=ThisSumPower
          ThisExponent=NextExponent
          if(dabs(ThisExponent)>=LimExponent) exit
        end do
      end if
!     ThisSumPower=0.0d0
      do iCase=1,nCases
!
! 04/15/02. ThisExponent TOO BIG
!c      if(x(iCase)>xMin) then
!c      write(4,*) iCase,z(iCase),ThisExponent
!c      x1 = ySpan
!c      x2 = z(iCase)
!c      x3 = z(iCase)**ThisExponent
!       if(x(iCase)>xMin.and.ThisExponent<LimExponent.and.
!    +                       ThisExponent>0.0d0) then
!         y(iCase)=yMin+ySpan*z(iCase)**ThisExponent
!c       elseif(ThisExponent>10.0d0.or.ThisExponent<=0.0d0) then
!c         y(iCase)=yMin
!       else
!         y(iCase)=yMin
!       end if
!       ThisSumPower=ThisSumPower+y(iCase)
!       write(4,'(i4,2f11.3,f13.3,a)') iCase,x(iCase),y(iCase),
!    +    ThisSumPower,' aft MNonlMap'
! 061709
!       after 20040227:
        if(MappingFailed) then
          y(iCase) = x(iCase)
        elseif(iExpo>0) then
          if(z(iCase)>0.0d0) then
            y(iCase)=yMin+ySpan*z(iCase)**ThisExponent
          else
            y(iCase)=yMin
          end if
        elseif(iExpo==0) then
          if(x(iCase)>xMin) then
            y(iCase)=yMax
          else
            y(iCase)=ySum-dble(nCases-1)*yMax
          end if
!     ! else iExpo==-1; retain incoming or assigned curve's shape
        end if
!       ThisSumPower=ThisSumPower+y(iCase)
!       write(4,'(i4,f11.3,f8.5,2f13.3,a)') iCase,y(iCase),z(iCase),
!    +    ThisSumPower,ThisSumPower-ySum,' after MNonlMap'
      end do
      end
!-----
!***********************************************************************
      subroutine SquareUpPattern(x,xMin,xMax,y,yMin,yMax,ySum,nCases)
!***********************************************************************
!     for feasibility, we require yMin<(ySum/nCases)<yMax
      logical (kind=1) ::  L1_FALSE=.false. 
      integer (kind=2) ::  nCases
      integer (kind=2) ::  iCase
      integer (kind=2) ::  jCase
      integer (kind=2) ::  pCase
      integer (kind=2) ::  nxUnique
      integer (kind=2) ::  nyAtMax
      integer (kind=2) ::  jIntermediate
      integer (kind=2) ::  iIntermediate
      integer (kind=2) ::  OrgOrder(:)
      real (kind=8) ::  x(*),y(*),yMin,yMax,ySum,xMin,xMax,ThisySum
      allocatable OrgOrder
!
! END DECLARATIONS
!
!     Move the nearest intermediate values to either extreme to make
!       sum(yi,over i on [1,nCases])=ySum, or equivalently
!       sum(yi-yMin,over i on [1,nCases])=ySum-nCases*yMin
!     subject (by construction) to
!       xMin=>yMin and
!       xMax=>yMax
!     and where the sequence in array x may be non-monotonic (even cyclic)
      if((xMax<=xMin).or.(yMax<=yMin)) return
      if(allocated(OrgOrder)) deallocate(OrgOrder)
      allocate(OrgOrder(nCases))
      do iCase=1,nCases
        OrgOrder(iCase)=iCase
      end do
      call Indexed_SortR8_Ascending(x,OrgOrder,nCases,L1_FALSE)
      nyAtMax=(ySum-yMin*dble(nCases))/(yMax-yMin)
      jIntermediate=nyAtMax+1
      iIntermediate=OrgOrder(jIntermediate)
      nxUnique=1
      pCase=OrgOrder(1)
      y(pCase)=yMax
      do jCase=2,nCases
        iCase=OrgOrder(jCase)
        if(x(iCase).ne.x(pCase)) nxUnique=nxUnique+1
        if(jCase<=nyAtMax) then
          y(iCase)=yMax
        else
          y(iCase)=yMin
        end if
!       write(4,'(3i4,2f12.3)') jCase,iCase,nxUnique,x(iCase),y(iCase)
        pCase=iCase
      end do
      deallocate(OrgOrder)
      if(nxUnique<=2) then
!     ! use exponential transformation of interpolates
        call MonthNonlinearlyMap(x,xMin,xMax,y,yMin,yMax,ySum,nCases)
        return
      end if
      ThisySum=yMin*dble(nCases)+(yMax-yMin)*dble(nyAtMax)
      y(iIntermediate)=y(iIntermediate)-(ThisySum-ySum)
!     write(4,'(2i4,3f10.3,a)') jIntermediate,iIntermediate,
!    +  y(iIntermediate),ySum,ThisySum,' penultimate ySum'
      end
!-----
!***********************************************************************
      subroutine Indexed_SortR8_Ascending(a,Ofs,nItems,Ascending)
!***********************************************************************
!     sorts nItems items into Ascending order(Ofs array); from EvaluInc.fpp
      integer (kind=2) ::  nItems,i,j,k,Gap,Ofs(*),Hold
      logical (kind=1) ::  Ascending
      real (kind=8) ::  a(*)
!
      Gap=nItems/2
      do while(Gap>0)
        do i=Gap+1,nItems
          j=i-Gap
          do while(j>0)
            k=j+Gap
            if((      Ascending .and.(a(Ofs(j))<=a(Ofs(k)))).or.
     +         ((.not.Ascending).and.(a(Ofs(j))>=a(Ofs(k))))) then
              j=0
            else
!             write(*,'(a,2i3,2f7.2)') ' interchanging',
!    +          Ofs(j),Ofs(k),a(Ofs(j)),a(Ofs(k))
              Hold=Ofs(j)
              Ofs(j)=Ofs(k)
              Ofs(k)=Hold
            end if
            j=j-Gap
          end do
        end do
        Gap=Gap/2
      end do
      end
!-----
!***********************************************************************
!
!        PROGRAM TO READ MULTI-TAB INFORMATION ON WEATHER DEMAND
!                 AND CONVERT TO BINARY FORMAT
!                       COPYRIGHT (C) 2001
!      ALL RIGHTS RESERVED M.S. GERBER & ASSOCIATES, INC.
!
!***********************************************************************
!
      SUBROUTINE WEATHER_DEMAND_OBJECT
      use end_routine, only: end_program, er_message
!
!***********************************************************************
!
      INCLUDE 'SpinLib.MON'
      USE SIZECOM
      LOGICAL (kind=1) ::  SAVE_WD_FILE_EXISTS=.FALSE. 
      LOGICAL (kind=1) ::  R_WD_FILE_EXISTS
      LOGICAL (kind=1) ::  R_WD_FILE_USED
      LOGICAL (kind=1) ::  SAVE_WD_FILE_USED=.FALSE. 
      INTEGER (kind=2) ::  INUNIT
      INTEGER (kind=2) ::  IREC
      INTEGER (kind=2) ::  DELETE
      INTEGER (kind=2) ::  LRECL=229 
      INTEGER (kind=2) ::  BASE_YEAR
      INTEGER (kind=2) ::  R_LRECL
      INTEGER (kind=2) ::  SAVE_TRANS_LOAD_RECORDS=0 
      INTEGER (kind=2) ::  R_TRANS_LOAD_RECORDS
      INTEGER (kind=2) ::  TEMP_YEAR
      INTEGER (kind=2) ::  R_NUM_OF_TRANS_CLASSES
      INTEGER (kind=2) ::  NUM_OF_OL_TRANS_CLASSES=0 
      INTEGER (kind=2) ::  R_MAX_TRANS_CLASS_NUM
      INTEGER (kind=2) ::  MAX_OL_TRANS_CLASS_ID_NUM=0 
      INTEGER (kind=2) ::  R_NUM_OF_ASSET_CLASSES
      INTEGER (kind=2) ::  NUM_OF_OL_ASSET_CLASSES=0 
      INTEGER (kind=2) ::  R_MAX_ASSET_CLASS_NUM
      INTEGER (kind=2) ::  MAX_OL_ASSET_CLASS_ID_NUM=0 
      INTEGER (kind=2) ::  R_NUM_OF_CUST_CLASSES
      INTEGER (kind=2) ::  NUM_OF_OL_CUST_CLASSES=0 
      INTEGER (kind=2) ::  R_MAX_CUST_CLASS_NUM
      INTEGER (kind=2) ::  MAX_OL_CUST_CLASS_ID_NUM=0 
      INTEGER (kind=2) ::  NUM_OF_BC_TRANS_CLASSES=0 
      INTEGER (kind=2) ::  MAX_BC_TRANS_CLASS_ID_NUM=0 
      INTEGER (kind=2) ::  NUM_OF_BC_ASSET_CLASSES=0 
      INTEGER (kind=2) ::  MAX_BC_ASSET_CLASS_ID_NUM=0 
      INTEGER (kind=2) ::  NUM_OF_BC_CUST_CLASSES=0 
      INTEGER (kind=2) ::  MAX_BC_CUST_CLASS_ID_NUM=0 
      INTEGER (kind=2) ::  TEMP_TRANS_CLASS_POINTER(:)
      INTEGER (kind=2) ::  TEMP_ASSET_CLASS_POINTER(:)
      INTEGER (kind=2) ::  TEMP_CUST_CLASS_POINTER(:)
      INTEGER (kind=2) ::  TRANSACTION_GROUP
      INTEGER (kind=2) ::  CUSTOMER_GROUP
      INTEGER (kind=4) ::  IOS,IOS_BASE
      ALLOCATABLE ::
     +            TEMP_TRANS_CLASS_POINTER,
     +            TEMP_ASSET_CLASS_POINTER,
     +            TEMP_CUST_CLASS_POINTER
      CHARACTER (len=5) ::  WEATHER_DEMAND_FILE
      CHARACTER (len=5) ::  OVERLAY_FAMILY_NAME,BSYRLOAD
      CHARACTER (len=256) ::  FILE_NAME
      CHARACTER (len=256) ::  OUTPUT_DIRECTORY
      CHARACTER (len=256) ::  BASE_FILE_DIRECTORY
      CHARACTER (len=152) ::  MESSAGE
      LOGICAL (kind=4) ::  FILE_EXISTS
! DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER (len=1024) ::  RECLN
! DECLARATION FOR SYSTEM FORECAST VARIABLES
      INTEGER (kind=2) ::  YEAR,UNIT_NUM=10 
!
! SIMULATION VARIABLES
!
      CHARACTER (len=1) ::
     +                     RECORD_IS_ACTIVE
      CHARACTER (len=20) ::
     +                     FORECAST_SOURCE
      CHARACTER (len=32) ::
     +                     MARKET_AREA_COMPANY_NAME
      CHARACTER (len=5) ::
     +                     MARKET_AREA_COMPANY_ID,
     +                     REFERENCE_LOAD_NAME
      INTEGER (kind=2) ::
     +                     REFERENCE_LOAD_NUMBER
      INTEGER (kind=4) ::
     +                     DATE_OF_FORECAST_ACTUAL_DATA
      CHARACTER (len=9) ::
     +                     DAY_OF_WEEK
      REAL (kind=4) ::
     +                     HOUR_OF_DAY(24),
     +                     ASSET_CLASS_ID,
     +                     ASSET_CLASS_REV_ALLOC_VECTOR
      CHARACTER (len=50) ::
     +                     COMMENT
!
      CHARACTER (len=17) ::  FILE_TYPE='Weather Demand   ' 
      CHARACTER (len=2) ::  WEATHER_DEMAND_OL='BC' ,R_WEATHER_DEMAND_OL
      LOGICAL (kind=1) ::  LAHEY_LF95
      CHARACTER (len=30) ::  SCREEN_OUTPUT
!
      INTEGER (kind=2) ::  STUDY_BASE_YEAR
!
      SAVE STUDY_BASE_YEAR
!
!***********************************************************************
!
!          ROUTINE TO CONVERT METAFILE FILES TO DIRECT-ACESS BINARY
!          COPYRIGHT (C) 1983, 84, 85  M.S. GERBER & ASSOCIATES, INC.
!
!***********************************************************************
!
!          CREATE THE BASE FILE
!
!***********************************************************************
      ENTRY WEATHER_DEMAND_MAKEBIN
!***********************************************************************
      STUDY_BASE_YEAR = BASE_YEAR()
      FILE_NAME = trim(BASE_FILE_DIRECTORY())//
     +                      "WDB"//trim(WEATHER_DEMAND_FILE())//".DAT"
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
!
      IF(FILE_EXISTS) THEN
         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT=trim(FILE_TYPE)//'-'//WEATHER_DEMAND_FILE()
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_LOCATE_WRITE(16,30,WEATHER_DEMAND_FILE(),
     +                                                   ALL_VERSIONS,0)
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         ENDIF
!
         NUM_OF_BC_TRANS_CLASSES = 0
         MAX_BC_TRANS_CLASS_ID_NUM = 0
         NUM_OF_BC_ASSET_CLASSES = 0
         MAX_BC_ASSET_CLASS_ID_NUM = 0
         NUM_OF_BC_CUST_CLASSES = 0
         MAX_BC_CUST_CLASS_ID_NUM = 0
!
         TRANSACTION_GROUP = 1
!
         ALLOCATE(   TEMP_TRANS_CLASS_POINTER(0:1023),
     +               TEMP_ASSET_CLASS_POINTER(0:1023),
     +               TEMP_CUST_CLASS_POINTER(0:1023))
         TEMP_TRANS_CLASS_POINTER = 0
         TEMP_ASSET_CLASS_POINTER = 0
         TEMP_CUST_CLASS_POINTER = 0
!
         SAVE_WD_FILE_EXISTS = .TRUE.
!
         OPEN(10,FILE=FILE_NAME)
         OPEN(11,FILE=trim(OUTPUT_DIRECTORY())//"BCWDYFC.BIN",
     +                      ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
!
         ASSET_CLASS_ID = 0.
         ASSET_CLASS_REV_ALLOC_VECTOR = 0.
!
         SAVE_TRANS_LOAD_RECORDS = 0
!
         CUSTOMER_GROUP = 0
!
         IREC = 0
         READ(10,*) DELETE
         DO
            DO
               READ(10,1000,IOSTAT=IOS) RECLN

               IF(RECLN(1:1) == '7' .OR. IOS /= 0) THEN
!
                  EXIT
!
               ENDIF
               RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               READ(RECLN,*,ERR=200) DELETE,
     +                     RECORD_IS_ACTIVE,
     +                     FORECAST_SOURCE,
     +                     MARKET_AREA_COMPANY_NAME,
     +                     MARKET_AREA_COMPANY_ID,
     +                     REFERENCE_LOAD_NAME,
     +                     REFERENCE_LOAD_NUMBER,
     +                     DATE_OF_FORECAST_ACTUAL_DATA,
     +                     DAY_OF_WEEK,
     +                     HOUR_OF_DAY,
     +                     COMMENT,
     +                     TRANSACTION_GROUP
!
               IREC = IREC + 1
!
!
               IF(TEMP_TRANS_CLASS_POINTER(TRANSACTION_GROUP) == 0) THEN
                  NUM_OF_BC_TRANS_CLASSES = NUM_OF_BC_TRANS_CLASSES + 1
                  MAX_BC_TRANS_CLASS_ID_NUM = MAX(
     +                      MAX_BC_TRANS_CLASS_ID_NUM,TRANSACTION_GROUP)
                  TEMP_TRANS_CLASS_POINTER(TRANSACTION_GROUP) = 1
               ENDIF
               IF(TEMP_ASSET_CLASS_POINTER(ASSET_CLASS_ID) == 0) THEN
                  NUM_OF_BC_ASSET_CLASSES = NUM_OF_BC_ASSET_CLASSES + 1
                  MAX_BC_ASSET_CLASS_ID_NUM = MAX(
     +                      MAX_BC_ASSET_CLASS_ID_NUM,ASSET_CLASS_ID)
                  TEMP_ASSET_CLASS_POINTER(ASSET_CLASS_ID) = 1
               ENDIF
               IF(TEMP_CUST_CLASS_POINTER(CUSTOMER_GROUP) == 0) THEN
                  NUM_OF_BC_CUST_CLASSES = NUM_OF_BC_CUST_CLASSES + 1
                  MAX_BC_CUST_CLASS_ID_NUM = MAX(
     +                      MAX_BC_CUST_CLASS_ID_NUM,CUSTOMER_GROUP)
                  TEMP_CUST_CLASS_POINTER(CUSTOMER_GROUP) = 1
               ENDIF
!
               WRITE(11,REC=IREC) DELETE,
     +                     RECORD_IS_ACTIVE,
     +                     FORECAST_SOURCE,
     +                     MARKET_AREA_COMPANY_NAME,
     +                     MARKET_AREA_COMPANY_ID,
     +                     REFERENCE_LOAD_NAME,
     +                     REFERENCE_LOAD_NUMBER,
     +                     DATE_OF_FORECAST_ACTUAL_DATA,
     +                     DAY_OF_WEEK,
     +                     HOUR_OF_DAY,
     +                     COMMENT,
     +                     TRANSACTION_GROUP
            ENDDO
            IF(IOS /= 0) EXIT
         ENDDO
         CLOSE(10)
      ELSE IF(INDEX(WEATHER_DEMAND_FILE(),'NONE') == 0) THEN
         CALL STOP_NOFILE(FILE_TYPE,FILE_NAME)
      ELSE
         OPEN(11,FILE=trim(OUTPUT_DIRECTORY())//"BCWDYFC.BIN",
     +                      ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
!
         SAVE_WD_FILE_EXISTS = .FALSE.
!
      ENDIF
!      ENDFILE(11)
      CLOSE(11)
      IF(ALLOCATED(TEMP_TRANS_CLASS_POINTER))
     +      DEALLOCATE(    TEMP_TRANS_CLASS_POINTER,
     +                     TEMP_ASSET_CLASS_POINTER,
     +                     TEMP_CUST_CLASS_POINTER)
!!
      SAVE_TRANS_LOAD_RECORDS = IREC
!!
      RETURN

  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from wh_objt SIID351'
      call end_program(er_message)
!***********************************************************************
!
!          ROUTINE TO CREATE OVERLAY FILES
!          COPYRIGHT (C) 1984-88  M.S. GERBER & ASSOCIATES, INC.
!          COPYRIGHT (C) 1991-92  M.S. GERBER & ASSOCIATES, INC.
!
!***********************************************************************
!
!***********************************************************************
      ENTRY WEATHER_DEMAND_MAKEOVL(OVERLAY_FAMILY_NAME)
!***********************************************************************
      IF(LAHEY_LF95()) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSE
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         CALL LOCATE(10,51)
      ENDIF
      FILE_NAME=trim(OUTPUT_DIRECTORY())//"WDO"//
     +                               trim(OVERLAY_FAMILY_NAME)//".DAT"
      OPEN(10,FILE=FILE_NAME)
      READ(10,*) DELETE
      INUNIT = 12
      IF(WEATHER_DEMAND_OL == 'BC') THEN
         OPEN(11,FILE=trim(OUTPUT_DIRECTORY())//"BCWDYFC.BIN",
     +                                       ACCESS="DIRECT",RECL=LRECL)
         INUNIT = 11
      ENDIF
!     INQUIRE(UNIT=12,OPENED=FILE_EXISTS)
      FILE_NAME = trim(OUTPUT_DIRECTORY())//"OLWDYFC.BIN"
      OPEN(12,FILE=FILE_NAME,ACCESS="DIRECT",
     +                           STATUS="UNKNOWN",RECL=LRECL,IOSTAT=IOS)
      IF(IOS /= 0) THEN
         CALL IOSTAT_MSG(IOS,MESSAGE)
         WRITE(4,*) trim(MESSAGE)
         er_message='in WEEKLY_DEMAND_MAKEOVL see WARNINGS messages'
         call end_program(er_message)
      ENDIF
      IREC = 0
      DELETE = 1
!
      NUM_OF_OL_TRANS_CLASSES = 0
      MAX_OL_TRANS_CLASS_ID_NUM = 0
      NUM_OF_OL_ASSET_CLASSES = 0
      MAX_OL_ASSET_CLASS_ID_NUM = 0
      NUM_OF_OL_CUST_CLASSES = 0
      MAX_OL_CUST_CLASS_ID_NUM = 0
!
      ALLOCATE(TEMP_TRANS_CLASS_POINTER(0:1023),
     +         TEMP_ASSET_CLASS_POINTER(0:1023),
     +         TEMP_CUST_CLASS_POINTER(0:1023))
      TEMP_TRANS_CLASS_POINTER = 0
      TEMP_ASSET_CLASS_POINTER = 0
      TEMP_CUST_CLASS_POINTER = 0
!
!
!
      DO
         DO
            READ(10,1000,IOSTAT=IOS) RECLN
            IF(RECLN(1:1) == '7') EXIT
            IREC = IREC + 1
            READ(INUNIT,REC=IREC,IOSTAT=IOS_BASE)
     +                        DELETE,
     +                        RECORD_IS_ACTIVE,
     +                        FORECAST_SOURCE,
     +                        MARKET_AREA_COMPANY_NAME,
     +                        MARKET_AREA_COMPANY_ID,
     +                        REFERENCE_LOAD_NAME,
     +                        REFERENCE_LOAD_NUMBER,
     +                        DATE_OF_FORECAST_ACTUAL_DATA,
     +                        DAY_OF_WEEK,
     +                        HOUR_OF_DAY,
     +                        COMMENT,
     +                        TRANSACTION_GROUP
            IF(IOS_BASE /= 0) EXIT
            IF(IOS == 0) THEN
               RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               READ(RECLN,*,ERR=200) DELETE,
     +                     RECORD_IS_ACTIVE,
     +                     FORECAST_SOURCE,
     +                     MARKET_AREA_COMPANY_NAME,
     +                     MARKET_AREA_COMPANY_ID,
     +                     REFERENCE_LOAD_NAME,
     +                     REFERENCE_LOAD_NUMBER,
     +                     DATE_OF_FORECAST_ACTUAL_DATA,
     +                     DAY_OF_WEEK,
     +                     HOUR_OF_DAY,
     +                     COMMENT,
     +                     TRANSACTION_GROUP
            ENDIF
!
!
            IF(TEMP_TRANS_CLASS_POINTER(TRANSACTION_GROUP) == 0) THEN
               NUM_OF_OL_TRANS_CLASSES = NUM_OF_OL_TRANS_CLASSES + 1
               MAX_OL_TRANS_CLASS_ID_NUM = MAX(
     +                      MAX_OL_TRANS_CLASS_ID_NUM,TRANSACTION_GROUP)
               TEMP_TRANS_CLASS_POINTER(TRANSACTION_GROUP) = 1
            ENDIF
            IF(TEMP_ASSET_CLASS_POINTER(ASSET_CLASS_ID) == 0) THEN
               NUM_OF_OL_ASSET_CLASSES = NUM_OF_OL_ASSET_CLASSES + 1
               MAX_OL_ASSET_CLASS_ID_NUM = MAX(
     +                         MAX_OL_ASSET_CLASS_ID_NUM,ASSET_CLASS_ID)
               TEMP_ASSET_CLASS_POINTER(ASSET_CLASS_ID) = 1
            ENDIF
            IF(TEMP_CUST_CLASS_POINTER(CUSTOMER_GROUP) == 0) THEN
               NUM_OF_OL_CUST_CLASSES = NUM_OF_OL_CUST_CLASSES + 1
               MAX_OL_CUST_CLASS_ID_NUM = MAX(
     +                          MAX_OL_CUST_CLASS_ID_NUM,CUSTOMER_GROUP)
               TEMP_CUST_CLASS_POINTER(CUSTOMER_GROUP) = 1
            ENDIF
!
            WRITE(12,REC=IREC) DELETE,
     +                         RECORD_IS_ACTIVE,
     +                         FORECAST_SOURCE,
     +                         MARKET_AREA_COMPANY_NAME,
     +                         MARKET_AREA_COMPANY_ID,
     +                         REFERENCE_LOAD_NAME,
     +                         REFERENCE_LOAD_NUMBER,
     +                         DATE_OF_FORECAST_ACTUAL_DATA,
     +                         DAY_OF_WEEK,
     +                         HOUR_OF_DAY,
     +                         COMMENT,
     +                         TRANSACTION_GROUP
         ENDDO
         IF(IOS_BASE /= 0) EXIT
      ENDDO
      CLOSE(10)
      CLOSE(12)
      IF(WEATHER_DEMAND_OL == 'BC') CLOSE(11)
      WEATHER_DEMAND_OL = 'OL'
      IF(ALLOCATED(TEMP_TRANS_CLASS_POINTER))
     +      DEALLOCATE(    TEMP_TRANS_CLASS_POINTER,
     +                     TEMP_ASSET_CLASS_POINTER,
     +                     TEMP_CUST_CLASS_POINTER)
      RETURN
!
!
!***********************************************************************
      ENTRY RESET_WEATHER_DEMAND_OL
!***********************************************************************
         WEATHER_DEMAND_OL = 'BC'
         SAVE_WD_FILE_USED = .FALSE.
      RETURN
!
!***********************************************************************
      ENTRY RETURN_WEATHER_DEMAND_OL(R_WEATHER_DEMAND_OL,R_LRECL)
!***********************************************************************
         R_WEATHER_DEMAND_OL = WEATHER_DEMAND_OL
         R_LRECL = LRECL
      RETURN
!***********************************************************************
      ENTRY DOES_WEATHER_DEMAND_FILE_EXIST(R_WD_FILE_EXISTS)
!***********************************************************************
         R_WD_FILE_EXISTS = SAVE_WD_FILE_EXISTS
      RETURN
!***********************************************************************
      ENTRY GET_WEATHER_DEMAND_RECORDS(R_TRANS_LOAD_RECORDS)
!***********************************************************************
         R_TRANS_LOAD_RECORDS = SAVE_TRANS_LOAD_RECORDS
      RETURN
!***********************************************************************
      ENTRY WD_FILE_USED_THIS_ENDPOINT(R_WD_FILE_USED)
!***********************************************************************
         R_WD_FILE_USED = SAVE_WD_FILE_USED
         SAVE_WD_FILE_USED = .TRUE.
      RETURN
!***********************************************************************
      ENTRY OPEN_WEATHER_DEMAND_FILE
!***********************************************************************
         OPEN(UNIT_NUM,
     +        FILE=trim(OUTPUT_DIRECTORY())//WEATHER_DEMAND_OL//
     +        "WDYFC.BIN",ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
      RETURN
!
!***********************************************************************
      ENTRY CLOSE_WEATHER_DEMAND_FILE
!***********************************************************************
         CLOSE(UNIT_NUM)
      RETURN
!***********************************************************************
      ENTRY RETURN_WD_GROUP_INFO(R_NUM_OF_TRANS_CLASSES,
     +                           R_MAX_TRANS_CLASS_NUM,
     +                           R_NUM_OF_ASSET_CLASSES,
     +                           R_MAX_ASSET_CLASS_NUM,
     +                           R_NUM_OF_CUST_CLASSES,
     +                           R_MAX_CUST_CLASS_NUM)
!***********************************************************************
         IF(WEATHER_DEMAND_OL == 'OL') THEN
            R_NUM_OF_TRANS_CLASSES = NUM_OF_OL_TRANS_CLASSES
            R_MAX_TRANS_CLASS_NUM = MAX_OL_TRANS_CLASS_ID_NUM
            R_NUM_OF_ASSET_CLASSES = NUM_OF_OL_ASSET_CLASSES
            R_MAX_ASSET_CLASS_NUM = MAX_OL_ASSET_CLASS_ID_NUM
            R_NUM_OF_CUST_CLASSES = NUM_OF_OL_CUST_CLASSES
            R_MAX_CUST_CLASS_NUM = MAX_OL_CUST_CLASS_ID_NUM
         ELSE
            R_NUM_OF_TRANS_CLASSES = NUM_OF_BC_TRANS_CLASSES
            R_MAX_TRANS_CLASS_NUM = MAX_BC_TRANS_CLASS_ID_NUM
            R_NUM_OF_ASSET_CLASSES = NUM_OF_BC_ASSET_CLASSES
            R_MAX_ASSET_CLASS_NUM = MAX_BC_ASSET_CLASS_ID_NUM
            R_NUM_OF_CUST_CLASSES = NUM_OF_BC_CUST_CLASSES
            R_MAX_CUST_CLASS_NUM = MAX_BC_CUST_CLASS_ID_NUM
         ENDIF
      RETURN
!
 1000 FORMAT(A)
 1010 FORMAT('&',A)
      END
!
!
!
!***********************************************************************
!
!             PROGRAM TO MANAGE SHORT-TERM DEMAND FORECASTS
!           BY TRANSACTION GROUP ASSET CLASS AND CUSTOMER GROUP
!
!                       COPYRIGHT (C) 2001
!          ALL RIGHTS RESERVED M.S. GERBER & ASSOCIATES, INC.
!
!***********************************************************************
!
      FUNCTION MANAGE_WEATHER_DEMAND_FORECASTS()
      use end_routine, only: end_program, er_message
!
!***********************************************************************
!
!
! LOCAL DATA LIST
!
      USE SIZECOM
      INCLUDE 'SpinLib.MON'
      use globecom
      SAVE

      LOGICAL (kind=1) ::  SAVE_WEATHER_DEMAND_STATUS=.FALSE. 
      LOGICAL (kind=1) ::  MANAGE_WEATHER_DEMAND_FORECASTS
      LOGICAL (kind=1) ::  GET_WD_LOAD
      LOGICAL (kind=1) ::       DOES_WD_FILE_EXIST
      INTEGER (kind=2) ::  YR
      INTEGER (kind=2) ::  MAKER_TABLES=0 
      INTEGER (kind=2) ::  DELETE
      INTEGER (kind=2) ::  R_MONTH
      INTEGER (kind=2) ::  MAKER_MONTHS
      INTEGER (kind=2) ::  R_YEAR
      INTEGER (kind=2) ::  R_LOAD_UNIT
      INTEGER (kind=2) ::  WK
      INTEGER (kind=2) ::  LOCAL_WEEKS
      INTEGER (kind=2) ::  LOCAL_YEARS
      INTEGER (kind=2) ::  NUM_SCEN_VAR
      INTEGER (kind=2) ::  IREC
!
!
      CHARACTER (len=1) ::
     +                     RECORD_IS_ACTIVE
      CHARACTER (len=20) ::
     +                     FORECAST_SOURCE
      CHARACTER (len=32) ::
     +                     MARKET_AREA_COMPANY_NAME
      CHARACTER (len=5) ::
     +                     MARKET_AREA_COMPANY_ID,
     +                     REFERENCE_LOAD_NAME
      INTEGER (kind=2) ::
     +                     J,
     +                     REFERENCE_LOAD_NUMBER,
     +                     TRANSACTION_GROUP,
     +                     WEATHER_DEMAND_RECORDS,
     +                     I_HOUR,
     +                     LOCAL_MONTH,
     +                     LOCAL_DAY,
     +                     LOCAL_YEAR,
     +                     MARKET_AREA_COMPANY_ID_NUM,
     +                     MARKET_AREA_LOOKUP,
     +                     R_ID_NUM,R_DAY
      INTEGER (kind=4) ::
     +                     DATE_OF_FORECAST_ACTUAL_DATA,
     +                     R_DAILY_I4(24)
      CHARACTER (len=9) ::
     +                     DAY_OF_WEEK
      REAL (kind=4) ::
     +                     HOUR_OF_DAY(24),WD_LOAD(:,:)
      ALLOCATABLE ::
     +                     WD_LOAD
      CHARACTER (len=50) ::
     +                     COMMENT
!
!
! INPUT DATA LIST
!
      INTEGER (kind=4) ::          VALUES_2_ZERO

      INTEGER (kind=2) ::  SCENARIO_YEAR
      INTEGER (kind=2) ::  TABLE
      INTEGER (kind=2) ::  SCENARIO_INDEX
      INTEGER (kind=2) ::  LOCAL_MAX_NUM_MARKET_AREAS=0 
      INTEGER (kind=2) ::  LOCAL_MAX_HOURS=24 
      INTEGER (kind=2) ::  LOCAL_MAX_DAYS=31 
      INTEGER (kind=2) ::  LOCAL_MAX_MONTHS=12 
      INTEGER (kind=2) ::  LOCAL_MAX_YEARS=12 
      INTEGER (kind=2) ::   MIN_YEAR=9999 
      INTEGER (kind=2) ::  MAX_YEAR=0 
      REAL (kind=4) ::
     +                  R_MONTHLY_SLOPE,R_MONTHLY_INTERCEPT
      CHARACTER (len=20) ::
     +                  HYDRO_VARIABLES
      INTEGER (kind=2) ::
     +                  WD_INDEX(:,:,:,:)
      ALLOCATABLE ::
     +                  WD_INDEX
!
!      SAVE WD_LOAD,WD_INDEX
!
! END DATA DECLARATIONS
!

!
         CALL DOES_WEATHER_DEMAND_FILE_EXIST(DOES_WD_FILE_EXIST)
         IF(DOES_WD_FILE_EXIST) THEN
            CALL OPEN_WEATHER_DEMAND_FILE
!
            CALL GET_WEATHER_DEMAND_RECORDS(WEATHER_DEMAND_RECORDS)
!
            LOCAL_WEEKS = 52
            NUM_SCEN_VAR = 3

            IF( ALLOCATED(WD_LOAD) )
     +                DEALLOCATE(WD_LOAD)
            ALLOCATE(
     +            WD_LOAD(LOCAL_MAX_HOURS,WEATHER_DEMAND_RECORDS))
! 031108
            WD_LOAD = -999.0

!
!
!     DETERMINE LOCAL_MAX_NUM_MARKET_AREAS, MIN_YEAR, MAX_YEAR
!
            DO IREC = 1, WEATHER_DEMAND_RECORDS
               READ(10,REC=IREC) DELETE,
     +                     RECORD_IS_ACTIVE,
     +                     FORECAST_SOURCE,
     +                     MARKET_AREA_COMPANY_NAME,
     +                     MARKET_AREA_COMPANY_ID,
     +                     REFERENCE_LOAD_NAME,
     +                     REFERENCE_LOAD_NUMBER,
     +                     DATE_OF_FORECAST_ACTUAL_DATA,
     +                     DAY_OF_WEEK,
     +                     HOUR_OF_DAY,
     +                     COMMENT,
     +                     TRANSACTION_GROUP
!
               IF(RECORD_IS_ACTIVE == 'F') CYCLE
!
!
               LOCAL_MONTH = DATE_OF_FORECAST_ACTUAL_DATA/10000
               LOCAL_DAY = (DATE_OF_FORECAST_ACTUAL_DATA -
     +                     LOCAL_MONTH*10000) / 100
!
               LOCAL_YEAR = DATE_OF_FORECAST_ACTUAL_DATA -
     +                LOCAL_MONTH * 10000 - LOCAL_DAY * 100
!
!     CALENDAR CORRECT YEAR
!
               IF(LOCAL_YEAR <= 50) THEN
                     LOCAL_YEAR = LOCAL_YEAR + 2000 - BASE_YEAR
                  ELSE
                     LOCAL_YEAR = LOCAL_YEAR + 1900 - BASE_YEAR
               ENDIF

               IF(LOCAL_YEAR > MAX_YEAR) MAX_YEAR = LOCAL_YEAR
               IF(LOCAL_YEAR < MIN_YEAR) MIN_YEAR = LOCAL_YEAR

               MARKET_AREA_COMPANY_ID_NUM =
     +                        MARKET_AREA_LOOKUP(MARKET_AREA_COMPANY_ID)
               LOCAL_MAX_NUM_MARKET_AREAS =
     +                     MAX(LOCAL_MAX_NUM_MARKET_AREAS,
     +                              MARKET_AREA_COMPANY_ID_NUM+1)
!
            ENDDO
!
            IF( ALLOCATED(WD_INDEX) )
     +                DEALLOCATE(WD_INDEX)
            IF(MAX_YEAR <= 0 .OR. LOCAL_MAX_NUM_MARKET_AREAS <= 0) THEN
            ENDIF
            ALLOCATE(
     +            WD_INDEX(0:LOCAL_MAX_NUM_MARKET_AREAS-1,
     +               LOCAL_MAX_MONTHS,
     +               LOCAL_MAX_DAYS,
     +               LOCAL_MAX_YEARS))


            WD_INDEX = 0

            J = 0
            DO IREC = 1, WEATHER_DEMAND_RECORDS
               READ(10,REC=IREC) DELETE,
     +                     RECORD_IS_ACTIVE,
     +                     FORECAST_SOURCE,
     +                     MARKET_AREA_COMPANY_NAME,
     +                     MARKET_AREA_COMPANY_ID,
     +                     REFERENCE_LOAD_NAME,
     +                     REFERENCE_LOAD_NUMBER,
     +                     DATE_OF_FORECAST_ACTUAL_DATA,
     +                     DAY_OF_WEEK,
     +                     HOUR_OF_DAY,
     +                     COMMENT,
     +                     TRANSACTION_GROUP
!
               IF(RECORD_IS_ACTIVE == 'F' .OR.
     +                                    RECORD_IS_ACTIVE == 'N') CYCLE
               J = J + 1
!
               DO I_HOUR = 1, 24
                  WD_LOAD(I_HOUR,J) = HOUR_OF_DAY(I_HOUR)
               ENDDO
!
               LOCAL_MONTH = DATE_OF_FORECAST_ACTUAL_DATA/10000
               LOCAL_DAY = (DATE_OF_FORECAST_ACTUAL_DATA -
     +                      LOCAL_MONTH*10000) / 100
               LOCAL_YEAR = DATE_OF_FORECAST_ACTUAL_DATA -
     +                LOCAL_MONTH * 10000 - LOCAL_DAY * 100
!
!     CALENDAR CORRECT YEAR
!
               IF(LOCAL_YEAR <= 50) THEN
                     LOCAL_YEAR = LOCAL_YEAR + 2000 - BASE_YEAR
                  ELSE
                     LOCAL_YEAR = LOCAL_YEAR + 1900 - BASE_YEAR
               ENDIF

               MARKET_AREA_COMPANY_ID_NUM =
     +            MARKET_AREA_LOOKUP(MARKET_AREA_COMPANY_ID)
               IF(MARKET_AREA_COMPANY_ID_NUM < 0 .OR.
     +                  MARKET_AREA_COMPANY_ID_NUM >
     +                           LOCAL_MAX_NUM_MARKET_AREAS-1) THEN
                  WRITE(4,*) 'BAD MARKET AREA ID IN THE SHORT-TERM'
                  WRITE(4,*) 'FORECAST MODEL',MARKET_AREA_COMPANY_ID_NUM
                  WRITE(4,*) 'MARKET AREA',MARKET_AREA_COMPANY_NAME
                  er_message='Stop requested from wh_objt SIID354'
                  call end_program(er_message)
               ENDIF
               IF(LOCAL_DAY > LOCAL_MAX_DAYS .OR. LOCAL_DAY < 1) THEN
                  WRITE(4,*) 'BAD DAY INDEX IN THE SHORT-TERM'
                  WRITE(4,*) 'FORECAST MODEL',LOCAL_DAY
                  WRITE(4,*) 'MARKET AREA',MARKET_AREA_COMPANY_NAME
                  er_message='Stop requested from wh_objt SIID355'
                  call end_program(er_message)
               ENDIF
               IF(LOCAL_MONTH > LOCAL_MAX_MONTHS .OR.
     +                                             LOCAL_MONTH < 1) THEN
                  WRITE(4,*) 'BAD MONTH INDEX IN THE SHORT-TERM'
                  WRITE(4,*) 'FORECAST MODEL',LOCAL_MONTH
                  WRITE(4,*) 'MARKET AREA',MARKET_AREA_COMPANY_NAME
                  er_message='Stop requested from wh_objt SIID356'
                  call end_program(er_message)
               ENDIF
               IF(LOCAL_YEAR > LOCAL_MAX_YEARS .OR. LOCAL_YEAR < 1) THEN
                  WRITE(4,*) 'BAD YEAR INDEX IN THE SHORT-TERM'
                  WRITE(4,*) 'FORECAST MODEL',LOCAL_YEAR
                  WRITE(4,*) 'MARKET AREA',MARKET_AREA_COMPANY_NAME
                  er_message='Stop requested from wh_objt SIID357'
                  call end_program(er_message)
               ENDIF
               WD_INDEX(MARKET_AREA_COMPANY_ID_NUM,LOCAL_MONTH,
     +                  LOCAL_DAY,LOCAL_YEAR) = J

!
            ENDDO
            CALL CLOSE_WEATHER_DEMAND_FILE
            MANAGE_WEATHER_DEMAND_FORECASTS = .TRUE.
         ENDIF
!
      RETURN
!
!***********************************************************************
!
      ENTRY GET_WD_LOAD(R_ID_NUM,R_YEAR,R_MONTH,R_DAY,R_DAILY_I4,
     +                   R_MONTHLY_SLOPE,R_MONTHLY_INTERCEPT)
!
!***********************************************************************

         GET_WD_LOAD = .FALSE.
         IF( (R_YEAR >= MIN_YEAR .AND. R_YEAR <= MAX_YEAR) .AND.
     +      R_MONTH <= LOCAL_MAX_MONTHS .AND.
     +      R_DAY <= LOCAL_MAX_DAYS .AND.
     +      R_ID_NUM <= LOCAL_MAX_NUM_MARKET_AREAS-1) THEN


            J = WD_INDEX(R_ID_NUM,R_MONTH,R_DAY,R_YEAR)

            IF(J > 0 .AND. WD_LOAD(1,J) > -998.0) THEN
                  R_DAILY_I4(1) = INT(WD_LOAD(1,J))
                  R_DAILY_I4(2) = INT(WD_LOAD(2,J))
                  R_DAILY_I4(3) = INT(WD_LOAD(3,J))
                  R_DAILY_I4(4) = INT(WD_LOAD(4,J))
                  R_DAILY_I4(5) = INT(WD_LOAD(5,J))
                  R_DAILY_I4(6) = INT(WD_LOAD(6,J))
                  R_DAILY_I4(7) = INT(WD_LOAD(7,J))
                  R_DAILY_I4(8) = INT(WD_LOAD(8,J))
                  R_DAILY_I4(9) = INT(WD_LOAD(9,J))
                  R_DAILY_I4(10) = INT(WD_LOAD(10,J))
                  R_DAILY_I4(11) = INT(WD_LOAD(11,J))
                  R_DAILY_I4(12) = INT(WD_LOAD(12,J))
                  R_DAILY_I4(13) = INT(WD_LOAD(13,J))
                  R_DAILY_I4(14) = INT(WD_LOAD(14,J))
                  R_DAILY_I4(15) = INT(WD_LOAD(15,J))
                  R_DAILY_I4(16) = INT(WD_LOAD(16,J))
                  R_DAILY_I4(17) = INT(WD_LOAD(17,J))
                  R_DAILY_I4(18) = INT(WD_LOAD(18,J))
                  R_DAILY_I4(19) = INT(WD_LOAD(19,J))
                  R_DAILY_I4(20) = INT(WD_LOAD(20,J))
                  R_DAILY_I4(21) = INT(WD_LOAD(21,J))
                  R_DAILY_I4(22) = INT(WD_LOAD(22,J))
                  R_DAILY_I4(23) = INT(WD_LOAD(23,J))
                  R_DAILY_I4(24) = INT(WD_LOAD(24,J))
                  R_MONTHLY_SLOPE = 1.0
                  R_MONTHLY_INTERCEPT = 0.0
                  GET_WD_LOAD = .TRUE.
            ELSE
                  GET_WD_LOAD = .FALSE.
            ENDIF
         ENDIF
      RETURN
!
      END
!***********************************************************************
!
!     TODO: convert to dictionary lookup.
      FUNCTION STATE_NAME_LOOKUP(R_STATE_NAME_NUM)
!
!***********************************************************************
      CHARACTER (len=20) ::  STATE_NAME_LOOKUP
      INTEGER (kind=2) ::  R_STATE_NAME_NUM
         SELECT CASE(R_STATE_NAME_NUM)
            CASE (1)
               STATE_NAME_LOOKUP = 'Alabama'
            CASE (2)
               STATE_NAME_LOOKUP = 'Alaska'
            CASE (3)
               STATE_NAME_LOOKUP = 'Arizona'
            CASE (4)
               STATE_NAME_LOOKUP = 'Arkansas'
            CASE (5)
               STATE_NAME_LOOKUP = 'California'
            CASE (6)
               STATE_NAME_LOOKUP = 'Colorado'
            CASE (7)
               STATE_NAME_LOOKUP = 'Connecticut'
            CASE (8)
               STATE_NAME_LOOKUP = 'Delaware'
            CASE (9)
               STATE_NAME_LOOKUP = 'District of Columbia'
            CASE (10)
               STATE_NAME_LOOKUP = 'Florida'
            CASE (11)
               STATE_NAME_LOOKUP = 'Georgia'
            CASE (12)
               STATE_NAME_LOOKUP = 'Hawaii'
            CASE (13)
               STATE_NAME_LOOKUP = 'Idaho'
            CASE (14)
               STATE_NAME_LOOKUP = 'Illinois'
            CASE (15)
               STATE_NAME_LOOKUP = 'Indiana'
            CASE (16)
               STATE_NAME_LOOKUP = 'Iowa'
            CASE (17)
               STATE_NAME_LOOKUP = 'Kansas'
            CASE (18)
               STATE_NAME_LOOKUP = 'Kentucky'
            CASE (19)
               STATE_NAME_LOOKUP = 'Louisiana'
            CASE (20)
               STATE_NAME_LOOKUP = 'Maine'
            CASE (21)
               STATE_NAME_LOOKUP = 'Maryland'
            CASE (22)
               STATE_NAME_LOOKUP = 'Massachusetts'
            CASE (23)
               STATE_NAME_LOOKUP = 'Michigan'
            CASE (24)
               STATE_NAME_LOOKUP = 'Minnesota'
            CASE (25)
               STATE_NAME_LOOKUP = 'Mississippi'
            CASE (26)
               STATE_NAME_LOOKUP = 'Missouri'
            CASE (27)
               STATE_NAME_LOOKUP = 'Montana'
            CASE (28)
               STATE_NAME_LOOKUP = 'Nebraska'
            CASE (29)
               STATE_NAME_LOOKUP = 'Nevada'
            CASE (30)
               STATE_NAME_LOOKUP = 'New Hampshire'
            CASE (31)
               STATE_NAME_LOOKUP = 'New Jersey'
            CASE (32)
               STATE_NAME_LOOKUP = 'New Mexico'
            CASE (33)
               STATE_NAME_LOOKUP = 'New York'
            CASE (34)
               STATE_NAME_LOOKUP = 'North Carolina'
            CASE (35)
               STATE_NAME_LOOKUP = 'North Dakota'
            CASE (36)
               STATE_NAME_LOOKUP = 'Ohio'
            CASE (37)
               STATE_NAME_LOOKUP = 'Oklahoma'
            CASE (38)
               STATE_NAME_LOOKUP = 'Oregon'
            CASE (39)
               STATE_NAME_LOOKUP = 'Pennsylvania'
            CASE (40)
               STATE_NAME_LOOKUP = 'Rhode Island'
            CASE (41)
               STATE_NAME_LOOKUP = 'South Carolina'
            CASE (42)
               STATE_NAME_LOOKUP = 'South Dakota'
            CASE (43)
               STATE_NAME_LOOKUP = 'Tennessee'
            CASE (44)
               STATE_NAME_LOOKUP = 'Texas'
            CASE (45)
               STATE_NAME_LOOKUP = 'Utah'
            CASE (46)
               STATE_NAME_LOOKUP = 'Vermont'
            CASE (47)
               STATE_NAME_LOOKUP = 'Virginia'
            CASE (48)
               STATE_NAME_LOOKUP = 'Washington'
            CASE (49)
               STATE_NAME_LOOKUP = 'West Virginia'
            CASE (50)
               STATE_NAME_LOOKUP = 'Wisconsin'
            CASE (51)
               STATE_NAME_LOOKUP = 'Wyoming'
            CASE (52)
               STATE_NAME_LOOKUP = 'Alberta'
            CASE (53)
               STATE_NAME_LOOKUP = 'British Columbia'
            CASE (54)
               STATE_NAME_LOOKUP = 'Manitoba'
            CASE (55)
               STATE_NAME_LOOKUP = 'New Brunswick'
            CASE (56)
               STATE_NAME_LOOKUP = 'Newfoundland'
            CASE (57)
               STATE_NAME_LOOKUP = 'Nova Scotia'
            CASE (58)
               STATE_NAME_LOOKUP = 'Ontario'
            CASE (59)
               STATE_NAME_LOOKUP = 'Prince Edward'
            CASE (60)
               STATE_NAME_LOOKUP = 'Quebec'
            CASE (61)
               STATE_NAME_LOOKUP = 'Saskatchewan'
          END SELECT
       RETURN
       END
!***********************************************************************
!
      FUNCTION STATE_ID_LOOKUP(STATE_ID_NUM)
!
!***********************************************************************
      INTEGER (KIND=2) :: STATE_ID_LOOKUP,CHECK_NEW_STATE_IDS
      CHARACTER (LEN=6) :: STATE_ID_NUM
         SELECT CASE(TRIM(STATE_ID_NUM))
            CASE ('AL')
               STATE_ID_LOOKUP = 1
            CASE ('AK')
               STATE_ID_LOOKUP = 2
            CASE ('AZ')
               STATE_ID_LOOKUP = 3
            CASE ('AR')
               STATE_ID_LOOKUP = 4
            CASE ('CA')
               STATE_ID_LOOKUP = 5
            CASE ('CO')
               STATE_ID_LOOKUP = 6
            CASE ('CT')
               STATE_ID_LOOKUP = 7
            CASE ('DE')
               STATE_ID_LOOKUP = 8
            CASE ('DC')
               STATE_ID_LOOKUP = 9
            CASE ('FL')
               STATE_ID_LOOKUP = 10
            CASE ('GA')
               STATE_ID_LOOKUP = 11
            CASE ('HI')
               STATE_ID_LOOKUP = 12
            CASE ('ID')
               STATE_ID_LOOKUP = 13
            CASE ('IL')
               STATE_ID_LOOKUP = 14
            CASE ('IN')
               STATE_ID_LOOKUP = 15
            CASE ('IA')
               STATE_ID_LOOKUP = 16
            CASE ('KS')
               STATE_ID_LOOKUP = 17
            CASE ('KY')
               STATE_ID_LOOKUP = 18
            CASE ('LA')
               STATE_ID_LOOKUP = 19
            CASE ('ME')
               STATE_ID_LOOKUP = 20
            CASE ('MD')
               STATE_ID_LOOKUP = 21
            CASE ('MA')
               STATE_ID_LOOKUP = 22
            CASE ('MI')
               STATE_ID_LOOKUP = 23
            CASE ('MN')
               STATE_ID_LOOKUP = 24
            CASE ('MS')
               STATE_ID_LOOKUP = 25
            CASE ('MO')
               STATE_ID_LOOKUP = 26
            CASE ('MT')
               STATE_ID_LOOKUP = 27
            CASE ('NE')
               STATE_ID_LOOKUP = 28
            CASE ('NV')
               STATE_ID_LOOKUP = 29
            CASE ('NH')
               STATE_ID_LOOKUP = 30
            CASE ('NJ')
               STATE_ID_LOOKUP = 31
            CASE ('NM')
               STATE_ID_LOOKUP = 32
            CASE ('NY')
               STATE_ID_LOOKUP = 33
            CASE ('NC')
               STATE_ID_LOOKUP = 34
            CASE ('ND')
               STATE_ID_LOOKUP = 35
            CASE ('OH')
               STATE_ID_LOOKUP = 36
            CASE ('OK')
               STATE_ID_LOOKUP = 37
            CASE ('OR')
               STATE_ID_LOOKUP = 38
            CASE ('PA')
               STATE_ID_LOOKUP = 39
            CASE ('RI')
               STATE_ID_LOOKUP = 40
            CASE ('SC')
               STATE_ID_LOOKUP = 41
            CASE ('SD')
               STATE_ID_LOOKUP = 42
            CASE ('TN')
               STATE_ID_LOOKUP = 43
            CASE ('TX')
               STATE_ID_LOOKUP = 44
            CASE ('UT')
               STATE_ID_LOOKUP = 45
            CASE ('VT')
               STATE_ID_LOOKUP = 46
            CASE ('VA')
               STATE_ID_LOOKUP = 47
            CASE ('WA')
               STATE_ID_LOOKUP = 48
            CASE ('WV')
               STATE_ID_LOOKUP = 49
            CASE ('WI')
               STATE_ID_LOOKUP = 50
            CASE ('WY')
               STATE_ID_LOOKUP = 51
            CASE ('AB')
               STATE_ID_LOOKUP = 52
            CASE ('BC')
               STATE_ID_LOOKUP = 53
            CASE ('MB')
               STATE_ID_LOOKUP = 54
            CASE ('NB')
               STATE_ID_LOOKUP = 55
            CASE ('NL')
               STATE_ID_LOOKUP = 56
            CASE ('NS')
               STATE_ID_LOOKUP = 57
            CASE ('ON')
               STATE_ID_LOOKUP = 58
            CASE ('PE')
               STATE_ID_LOOKUP = 59
            CASE ('QC')
               STATE_ID_LOOKUP = 60
            CASE ('SK')
               STATE_ID_LOOKUP = 61
            CASE ('BCN')
               STATE_ID_LOOKUP = 62
            CASE ('BCS')
               STATE_ID_LOOKUP = 63
            CASE ('CAM')
               STATE_ID_LOOKUP = 64
            CASE ('CHS')
               STATE_ID_LOOKUP = 65
            CASE ('CHI')
               STATE_ID_LOOKUP = 66
            CASE ('COA')
               STATE_ID_LOOKUP = 67
            CASE ('COL')
               STATE_ID_LOOKUP = 68
            CASE ('DGO')
               STATE_ID_LOOKUP = 69
            CASE ('GTO')
               STATE_ID_LOOKUP = 70
            CASE ('GRO')
               STATE_ID_LOOKUP = 71
            CASE ('HGO')
               STATE_ID_LOOKUP = 72
            CASE ('JAL')
               STATE_ID_LOOKUP = 73
            CASE ('MEX')
               STATE_ID_LOOKUP = 74
            CASE ('MIC')
               STATE_ID_LOOKUP = 75
            CASE ('NAY')
               STATE_ID_LOOKUP = 76
            CASE ('NLN')
               STATE_ID_LOOKUP = 77
            CASE ('OAX')
               STATE_ID_LOOKUP = 78
            CASE ('PUE')
               STATE_ID_LOOKUP = 79
            CASE ('QRO')
               STATE_ID_LOOKUP = 80
            CASE ('QTR')
               STATE_ID_LOOKUP = 81
            CASE ('SLP')
               STATE_ID_LOOKUP = 82
            CASE ('SIN')
               STATE_ID_LOOKUP = 83
            CASE ('AB_52')
               STATE_ID_LOOKUP = 84
            CASE ('AB_53')
               STATE_ID_LOOKUP = 85
            CASE ('AL_45')
               STATE_ID_LOOKUP = 86
            CASE ('AL_46')
               STATE_ID_LOOKUP = 87
            CASE ('AR_43')
               STATE_ID_LOOKUP = 88
            CASE ('AR_50')
               STATE_ID_LOOKUP = 89
            CASE ('AZ_54')
               STATE_ID_LOOKUP = 90
            CASE ('AZ_65')
               STATE_ID_LOOKUP = 91
            CASE ('AZ_72')
               STATE_ID_LOOKUP = 92
            CASE ('AZ_73')
               STATE_ID_LOOKUP = 93
            CASE ('AZ_79')
               STATE_ID_LOOKUP = 94
            CASE ('BC_52')
               STATE_ID_LOOKUP = 95
            CASE ('BC_55')
               STATE_ID_LOOKUP = 96
            CASE ('BCN_68')
               STATE_ID_LOOKUP = 97
            CASE ('BCN_79')
               STATE_ID_LOOKUP = 98
            CASE ('CA_56')
               STATE_ID_LOOKUP = 99
            CASE ('CA_57')
               STATE_ID_LOOKUP = 100
            CASE ('CA_58')
               STATE_ID_LOOKUP = 101
            CASE ('CA_59')
               STATE_ID_LOOKUP = 102
            CASE ('CA_65')
               STATE_ID_LOOKUP = 103
            CASE ('CA_66')
               STATE_ID_LOOKUP = 104
            CASE ('CA_70')
               STATE_ID_LOOKUP = 105
            CASE ('CA_73')
               STATE_ID_LOOKUP = 106
            CASE ('CHI_68')
               STATE_ID_LOOKUP = 107
            CASE ('CO_61')
               STATE_ID_LOOKUP = 108
            CASE ('CO_62')
               STATE_ID_LOOKUP = 109
            CASE ('CT_86')
               STATE_ID_LOOKUP = 110
            CASE ('CT_87')
               STATE_ID_LOOKUP = 111
            CASE ('CT_88')
               STATE_ID_LOOKUP = 112
            CASE ('DC_16')
               STATE_ID_LOOKUP = 113
            CASE ('DE_15')
               STATE_ID_LOOKUP = 114
            CASE ('FL_14')
               STATE_ID_LOOKUP = 115
            CASE ('FL_45')
               STATE_ID_LOOKUP = 116
            CASE ('GA_45')
               STATE_ID_LOOKUP = 117
            CASE ('IA_17')
               STATE_ID_LOOKUP = 118
            CASE ('IA_21')
               STATE_ID_LOOKUP = 119
            CASE ('IA_23')
               STATE_ID_LOOKUP = 120
            CASE ('IA_25')
               STATE_ID_LOOKUP = 121
            CASE ('ID_63')
               STATE_ID_LOOKUP = 122
            CASE ('ID_64')
               STATE_ID_LOOKUP = 123
            CASE ('ID_71')
               STATE_ID_LOOKUP = 124
            CASE ('IL_18')
               STATE_ID_LOOKUP = 125
            CASE ('IL_19')
               STATE_ID_LOOKUP = 126
            CASE ('IL_21')
               STATE_ID_LOOKUP = 127
            CASE ('IL_46')
               STATE_ID_LOOKUP = 128
            CASE ('IN_02')
               STATE_ID_LOOKUP = 129
            CASE ('IN_03')
               STATE_ID_LOOKUP = 130
            CASE ('IN_17')
               STATE_ID_LOOKUP = 131
            CASE ('IN_18')
               STATE_ID_LOOKUP = 132
            CASE ('KS_51')
               STATE_ID_LOOKUP = 133
            CASE ('KY_02')
               STATE_ID_LOOKUP = 134
            CASE ('KY_03')
               STATE_ID_LOOKUP = 135
            CASE ('KY_05')
               STATE_ID_LOOKUP = 136
            CASE ('KY_46')
               STATE_ID_LOOKUP = 137
            CASE ('LA_43')
               STATE_ID_LOOKUP = 138
            CASE ('LA_49')
               STATE_ID_LOOKUP = 139
            CASE ('LA_50')
               STATE_ID_LOOKUP = 140
            CASE ('MA_86')
               STATE_ID_LOOKUP = 141
            CASE ('MA_87')
               STATE_ID_LOOKUP = 142
            CASE ('MA_89')
               STATE_ID_LOOKUP = 143
            CASE ('MB_22')
               STATE_ID_LOOKUP = 144
            CASE ('MD_01')
               STATE_ID_LOOKUP = 145
            CASE ('MD_15')
               STATE_ID_LOOKUP = 146
            CASE ('MD_16')
               STATE_ID_LOOKUP = 147
            CASE ('ME_27')
               STATE_ID_LOOKUP = 148
            CASE ('ME_85')
               STATE_ID_LOOKUP = 149
            CASE ('MI_02')
               STATE_ID_LOOKUP = 150
            CASE ('MI_06')
               STATE_ID_LOOKUP = 151
            CASE ('MI_20')
               STATE_ID_LOOKUP = 152
            CASE ('MN_17')
               STATE_ID_LOOKUP = 153
            CASE ('MN_23')
               STATE_ID_LOOKUP = 154
            CASE ('MN_25')
               STATE_ID_LOOKUP = 155
            CASE ('MO_19')
               STATE_ID_LOOKUP = 156
            CASE ('MO_42')
               STATE_ID_LOOKUP = 157
            CASE ('MO_50')
               STATE_ID_LOOKUP = 158
            CASE ('MO_51')
               STATE_ID_LOOKUP = 159
            CASE ('MS_43')
               STATE_ID_LOOKUP = 160
            CASE ('MS_45')
               STATE_ID_LOOKUP = 161
            CASE ('MS_46')
               STATE_ID_LOOKUP = 162
            CASE ('MT_25')
               STATE_ID_LOOKUP = 163
            CASE ('MT_67')
               STATE_ID_LOOKUP = 164
            CASE ('NB_27')
               STATE_ID_LOOKUP = 165
            CASE ('NC_44')
               STATE_ID_LOOKUP = 166
            CASE ('NC_47')
               STATE_ID_LOOKUP = 167
            CASE ('ND_23')
               STATE_ID_LOOKUP = 168
            CASE ('ND_25')
               STATE_ID_LOOKUP = 169
            CASE ('NE_24')
               STATE_ID_LOOKUP = 170
            CASE ('NE_75')
               STATE_ID_LOOKUP = 171
            CASE ('NH_87')
               STATE_ID_LOOKUP = 172
            CASE ('NJ_15')
               STATE_ID_LOOKUP = 173
            CASE ('NJ_38')
               STATE_ID_LOOKUP = 174
            CASE ('NM_50')
               STATE_ID_LOOKUP = 175
            CASE ('NM_61')
               STATE_ID_LOOKUP = 176
            CASE ('NM_69')
               STATE_ID_LOOKUP = 177
            CASE ('NS_27')
               STATE_ID_LOOKUP = 178
            CASE ('NV_57')
               STATE_ID_LOOKUP = 179
            CASE ('NV_70')
               STATE_ID_LOOKUP = 180
            CASE ('NV_73')
               STATE_ID_LOOKUP = 181
            CASE ('NY_31')
               STATE_ID_LOOKUP = 182
            CASE ('NY_32')
               STATE_ID_LOOKUP = 183
            CASE ('NY_33')
               STATE_ID_LOOKUP = 184
            CASE ('NY_34')
               STATE_ID_LOOKUP = 185
            CASE ('NY_38')
               STATE_ID_LOOKUP = 186
            CASE ('NY_39')
               STATE_ID_LOOKUP = 187
            CASE ('NY_95')
               STATE_ID_LOOKUP = 188
            CASE ('NY_96')
               STATE_ID_LOOKUP = 189
            CASE ('OH_01')
               STATE_ID_LOOKUP = 190
            CASE ('OH_02')
               STATE_ID_LOOKUP = 191
            CASE ('OH_03')
               STATE_ID_LOOKUP = 192
            CASE ('OH_04')
               STATE_ID_LOOKUP = 193
            CASE ('OK_42')
               STATE_ID_LOOKUP = 194
            CASE ('OK_50')
               STATE_ID_LOOKUP = 195
            CASE ('OK_78')
               STATE_ID_LOOKUP = 196
            CASE ('ON_80')
               STATE_ID_LOOKUP = 197
            CASE ('ON_81')
               STATE_ID_LOOKUP = 198
            CASE ('ON_82')
               STATE_ID_LOOKUP = 199
            CASE ('ON_83')
               STATE_ID_LOOKUP = 200
            CASE ('OR_60')
               STATE_ID_LOOKUP = 201
            CASE ('OR_71')
               STATE_ID_LOOKUP = 202
            CASE ('PA_01')
               STATE_ID_LOOKUP = 203
            CASE ('PA_04')
               STATE_ID_LOOKUP = 204
            CASE ('PA_15')
               STATE_ID_LOOKUP = 205
            CASE ('PA_16')
               STATE_ID_LOOKUP = 206
            CASE ('PA_95')
               STATE_ID_LOOKUP = 207
            CASE ('PE_27')
               STATE_ID_LOOKUP = 208
            CASE ('QC_41')
               STATE_ID_LOOKUP = 209
            CASE ('RI_87')
               STATE_ID_LOOKUP = 210
            CASE ('SC_44')
               STATE_ID_LOOKUP = 211
            CASE ('SD_23')
               STATE_ID_LOOKUP = 212
            CASE ('SD_25')
               STATE_ID_LOOKUP = 213
            CASE ('SD_75')
               STATE_ID_LOOKUP = 214
            CASE ('SK_26')
               STATE_ID_LOOKUP = 215
            CASE ('TN_46')
               STATE_ID_LOOKUP = 216
            CASE ('TX_07')
               STATE_ID_LOOKUP = 217
            CASE ('TX_08')
               STATE_ID_LOOKUP = 218
            CASE ('TX_09')
               STATE_ID_LOOKUP = 219
            CASE ('TX_10')
               STATE_ID_LOOKUP = 220
            CASE ('TX_11')
               STATE_ID_LOOKUP = 221
            CASE ('TX_12')
               STATE_ID_LOOKUP = 222
            CASE ('TX_13')
               STATE_ID_LOOKUP = 223
            CASE ('TX_43')
               STATE_ID_LOOKUP = 224
            CASE ('TX_50')
               STATE_ID_LOOKUP = 225
            CASE ('TX_69')
               STATE_ID_LOOKUP = 226
            CASE ('TX_76')
               STATE_ID_LOOKUP = 227
            CASE ('TX_77')
               STATE_ID_LOOKUP = 228
            CASE ('UT_74')
               STATE_ID_LOOKUP = 229
            CASE ('VA_02')
               STATE_ID_LOOKUP = 230
            CASE ('VA_15')
               STATE_ID_LOOKUP = 231
            CASE ('VA_16')
               STATE_ID_LOOKUP = 232
            CASE ('VA_47')
               STATE_ID_LOOKUP = 233
            CASE ('VT_86')
               STATE_ID_LOOKUP = 234
            CASE ('VT_87')
               STATE_ID_LOOKUP = 235
            CASE ('WA_71')
               STATE_ID_LOOKUP = 236
            CASE ('WI_20')
               STATE_ID_LOOKUP = 237
            CASE ('WI_23')
               STATE_ID_LOOKUP = 238
            CASE ('WV_01')
               STATE_ID_LOOKUP = 239
            CASE ('WV_02')
               STATE_ID_LOOKUP = 240
            CASE ('WV_47')
               STATE_ID_LOOKUP = 241
            CASE ('WY_25')
               STATE_ID_LOOKUP = 242
            CASE ('WY_63')
               STATE_ID_LOOKUP = 243
            CASE ('WY_74')
               STATE_ID_LOOKUP = 244
            CASE ('WY_75')
               STATE_ID_LOOKUP = 245
            CASE ('AB_98')
               STATE_ID_LOOKUP = 246
            CASE ('AZ_94')
               STATE_ID_LOOKUP = 247
            CASE ('AZ_99')
               STATE_ID_LOOKUP = 248
            CASE ('BC_98')
               STATE_ID_LOOKUP = 249
            CASE ('CA_99')
               STATE_ID_LOOKUP = 250
            CASE ('ID_29')
               STATE_ID_LOOKUP = 251
            CASE ('KS_61')
               STATE_ID_LOOKUP = 252
            CASE ('NY_40')
               STATE_ID_LOOKUP = 253
            CASE ('NY_97')
               STATE_ID_LOOKUP = 254
            CASE ('OK_08')
               STATE_ID_LOOKUP = 255
            CASE ('ON_40')
               STATE_ID_LOOKUP = 256
            CASE ('WY_29')
               STATE_ID_LOOKUP = 257
            CASE ('AR_42')
               STATE_ID_LOOKUP = 258
            CASE ('BCN_58')
               STATE_ID_LOOKUP = 259
            CASE ('BCS_68')
               STATE_ID_LOOKUP = 260
            CASE ('CA_71')
               STATE_ID_LOOKUP = 261
            CASE ('CA_76')
               STATE_ID_LOOKUP = 262
            CASE ('CA_77')
               STATE_ID_LOOKUP = 263
            CASE ('CA_94')
               STATE_ID_LOOKUP = 264
            CASE ('DC_12')
               STATE_ID_LOOKUP = 265
            CASE ('KY_19')
               STATE_ID_LOOKUP = 266
            CASE ('KY_91')
               STATE_ID_LOOKUP = 267
            CASE ('MD_12')
               STATE_ID_LOOKUP = 268
            CASE ('MN_21')
               STATE_ID_LOOKUP = 269
            CASE ('MN_90')
               STATE_ID_LOOKUP = 270
            CASE ('MT_90')
               STATE_ID_LOOKUP = 271
            CASE ('ND_90')
               STATE_ID_LOOKUP = 272
            CASE ('OH_91')
               STATE_ID_LOOKUP = 273
            CASE ('PA_02')
               STATE_ID_LOOKUP = 274
            CASE ('PA_09')
               STATE_ID_LOOKUP = 275
            CASE ('PA_13')
               STATE_ID_LOOKUP = 276
            CASE ('SD_90')
               STATE_ID_LOOKUP = 277
            CASE ('VA_12')
               STATE_ID_LOOKUP = 278
            CASE ('WY_61')
               STATE_ID_LOOKUP = 279
            CASE('GA_44')
               STATE_ID_LOOKUP = 280
            CASE('GA_46')
               STATE_ID_LOOKUP = 281
            CASE('IA_19')
               STATE_ID_LOOKUP = 282
            CASE('ID_74')
               STATE_ID_LOOKUP = 283
            CASE('KY_04')
               STATE_ID_LOOKUP = 284
            CASE('MD_13')
               STATE_ID_LOOKUP = 285
            CASE('MT_71')
               STATE_ID_LOOKUP = 286
            CASE('NC_46')
               STATE_ID_LOOKUP = 287
            CASE('ND_67')
               STATE_ID_LOOKUP = 288
            CASE('NH_86')
               STATE_ID_LOOKUP = 289
            CASE('NL_41')
               STATE_ID_LOOKUP = 290
            CASE('OH_05')
               STATE_ID_LOOKUP = 292
            CASE('OR_29')
               STATE_ID_LOOKUP = 293
            CASE('PA_97')
               STATE_ID_LOOKUP = 294
            CASE('SC_45')
               STATE_ID_LOOKUP = 295
            CASE('UT_29')
               STATE_ID_LOOKUP = 296
            CASE('WV_03')
               STATE_ID_LOOKUP = 297
            CASE('MO_16')
               STATE_ID_LOOKUP = 298
            CASE('IL_16')
               STATE_ID_LOOKUP = 299
            CASE('MA_84')
               STATE_ID_LOOKUP = 300
            CASE('NH_89')
               STATE_ID_LOOKUP = 301
            CASE('RI_89')
               STATE_ID_LOOKUP = 302
            CASE('CT_89')
               STATE_ID_LOOKUP = 303
            CASE('MS_48')
               STATE_ID_LOOKUP = 304
            CASE DEFAULT
               STATE_ID_LOOKUP = CHECK_NEW_STATE_IDS(STATE_ID_NUM)
            END SELECT
      RETURN
      END FUNCTION
!***********************************************************************
      FUNCTION CHECK_NEW_STATE_IDS(STATE_ID_NUM) RESULT(STATE_ID)
!***********************************************************************
      USE PROD_ARRAYS_DIMENSIONS
      CHARACTER (LEN=6) :: STATE_ID_NUM
      CHARACTER (LEN=6) , SAVE :: StateAdds(305:MAX_STATE_LOOKUP_IDS)
      INTEGER (KIND=2) ,SAVE :: ActiveStateIDs=304 
      INTEGER (KIND=2) :: I,STATE_ID
         IF(TRIM(STATE_ID_NUM) <> " " .AND.
     +                                SCAN(STATE_ID_NUM,'#/') == 0) THEN
            IF(ActiveStateIDs == 304) StateAdds(305:) = " "
            DO I = 305, ActiveStateIDs
               IF(TRIM(STATE_ID_NUM) == TRIM(StateAdds(I))) EXIT
            ENDDO
            IF(I > ActiveStateIDs) THEN
               ActiveStateIDs = ActiveStateIDs + 1
               StateAdds(ActiveStateIDs) = STATE_ID_NUM
            ENDIF
            STATE_ID = I
         ELSE
            STATE_ID = -1
         ENDIF
      END FUNCTION
!***********************************************************************
!
      FUNCTION STATE_2_GAS_REGION_LOOKUP(R_STATE_ID_NUM)
!
!***********************************************************************
      INTEGER (kind=2) ::  STATE_2_GAS_REGION_LOOKUP
      CHARACTER (len=6) ::  R_STATE_ID_NUM
!
! THIS WILL PROBABLY NEED TO BE AN EXTERNALLY DEFINED TABLE.
!
! 082207.
!
         SELECT CASE(trim(R_STATE_ID_NUM))
            CASE ('AK')
               STATE_2_GAS_REGION_LOOKUP = 1
            CASE ('AL')
               STATE_2_GAS_REGION_LOOKUP = 2
            CASE ('AR')
               STATE_2_GAS_REGION_LOOKUP = 3
            CASE ('AZ')
               STATE_2_GAS_REGION_LOOKUP = 4
            CASE ('CA')
               STATE_2_GAS_REGION_LOOKUP = 5
            CASE ('CO')
               STATE_2_GAS_REGION_LOOKUP = 6
            CASE ('CT')
               STATE_2_GAS_REGION_LOOKUP = 7
            CASE ('DC')
               STATE_2_GAS_REGION_LOOKUP = 8
            CASE ('DE')
               STATE_2_GAS_REGION_LOOKUP = 9
            CASE ('FL')
               STATE_2_GAS_REGION_LOOKUP = 10
            CASE ('GA')
               STATE_2_GAS_REGION_LOOKUP = 11
            CASE ('HI')
               STATE_2_GAS_REGION_LOOKUP = 12
            CASE ('ID')
               STATE_2_GAS_REGION_LOOKUP = 14
            CASE ('IL')
               STATE_2_GAS_REGION_LOOKUP = 15
            CASE ('IN')
               STATE_2_GAS_REGION_LOOKUP = 16
            CASE ('IA')
               STATE_2_GAS_REGION_LOOKUP = 13
            CASE ('KS')
               STATE_2_GAS_REGION_LOOKUP = 17
            CASE ('KY')
               STATE_2_GAS_REGION_LOOKUP = 18
            CASE ('LA')
               STATE_2_GAS_REGION_LOOKUP = 19
            CASE ('ME')
               STATE_2_GAS_REGION_LOOKUP = 22
            CASE ('MD')
               STATE_2_GAS_REGION_LOOKUP = 21
            CASE ('MA')
               STATE_2_GAS_REGION_LOOKUP = 20
            CASE ('MI')
               STATE_2_GAS_REGION_LOOKUP = 23
            CASE ('MN')
               STATE_2_GAS_REGION_LOOKUP = 24
            CASE ('MS')
               STATE_2_GAS_REGION_LOOKUP = 26
            CASE ('MO')
               STATE_2_GAS_REGION_LOOKUP = 25
            CASE ('MT')
               STATE_2_GAS_REGION_LOOKUP = 27
            CASE ('NE')
               STATE_2_GAS_REGION_LOOKUP = 30
            CASE ('NV')
               STATE_2_GAS_REGION_LOOKUP = 34
            CASE ('NH')
               STATE_2_GAS_REGION_LOOKUP = 31
            CASE ('NJ')
               STATE_2_GAS_REGION_LOOKUP = 32
            CASE ('NM')
               STATE_2_GAS_REGION_LOOKUP = 33
            CASE ('NY')
               STATE_2_GAS_REGION_LOOKUP = 35
            CASE ('NC')
               STATE_2_GAS_REGION_LOOKUP = 28
            CASE ('ND')
               STATE_2_GAS_REGION_LOOKUP = 29
            CASE ('OH')
               STATE_2_GAS_REGION_LOOKUP = 36
            CASE ('OK')
               STATE_2_GAS_REGION_LOOKUP = 37
            CASE ('OR')
               STATE_2_GAS_REGION_LOOKUP = 38
            CASE ('PA')
               STATE_2_GAS_REGION_LOOKUP = 39
            CASE ('RI')
               STATE_2_GAS_REGION_LOOKUP = 40
            CASE ('SC')
               STATE_2_GAS_REGION_LOOKUP = 41
            CASE ('SD')
               STATE_2_GAS_REGION_LOOKUP = 42
            CASE ('TN')
               STATE_2_GAS_REGION_LOOKUP = 43
            CASE ('TX')
               STATE_2_GAS_REGION_LOOKUP = 44
            CASE ('UT')
               STATE_2_GAS_REGION_LOOKUP = 45
            CASE ('VT')
               STATE_2_GAS_REGION_LOOKUP = 47
            CASE ('VA')
               STATE_2_GAS_REGION_LOOKUP = 46
            CASE ('WA')
               STATE_2_GAS_REGION_LOOKUP = 48
            CASE ('WV')
               STATE_2_GAS_REGION_LOOKUP = 50
            CASE ('WI')
               STATE_2_GAS_REGION_LOOKUP = 49
            CASE ('WY')
               STATE_2_GAS_REGION_LOOKUP = 51
            CASE ('AB')
               STATE_2_GAS_REGION_LOOKUP = 61
            CASE ('BC')
               STATE_2_GAS_REGION_LOOKUP = 62
            CASE ('MB')
               STATE_2_GAS_REGION_LOOKUP = 63
            CASE ('NB')
               STATE_2_GAS_REGION_LOOKUP = 64
            CASE ('NL')
               STATE_2_GAS_REGION_LOOKUP = 65
            CASE ('NS')
               STATE_2_GAS_REGION_LOOKUP = 66
            CASE ('ON')
               STATE_2_GAS_REGION_LOOKUP = 67
            CASE ('PE')
               STATE_2_GAS_REGION_LOOKUP = 68
            CASE ('QC')
               STATE_2_GAS_REGION_LOOKUP = 69
            CASE ('SK')
               STATE_2_GAS_REGION_LOOKUP = 70
            CASE ('BCN')
               STATE_2_GAS_REGION_LOOKUP = 71
            CASE ('BCS')
               STATE_2_GAS_REGION_LOOKUP = 63
            CASE ('CAM')
               STATE_2_GAS_REGION_LOOKUP = 64
            CASE ('CHS')
               STATE_2_GAS_REGION_LOOKUP = 65
            CASE ('CHI')
               STATE_2_GAS_REGION_LOOKUP = 66
            CASE ('COA')
               STATE_2_GAS_REGION_LOOKUP = 67
            CASE ('COL')
               STATE_2_GAS_REGION_LOOKUP = 68
            CASE ('DGO')
               STATE_2_GAS_REGION_LOOKUP = 69
            CASE ('GTO')
               STATE_2_GAS_REGION_LOOKUP = 70
            CASE ('GRO')
               STATE_2_GAS_REGION_LOOKUP = 71
            CASE ('HGO')
               STATE_2_GAS_REGION_LOOKUP = 72
            CASE ('JAL')
               STATE_2_GAS_REGION_LOOKUP = 73
            CASE ('MEX')
               STATE_2_GAS_REGION_LOOKUP = 74
            CASE ('MIC')
               STATE_2_GAS_REGION_LOOKUP = 75
            CASE ('NAY')
               STATE_2_GAS_REGION_LOOKUP = 76
            CASE ('NLN')
               STATE_2_GAS_REGION_LOOKUP = 77
            CASE ('OAX')
               STATE_2_GAS_REGION_LOOKUP = 78
            CASE ('PUE')
               STATE_2_GAS_REGION_LOOKUP = 79
            CASE ('QRO')
               STATE_2_GAS_REGION_LOOKUP = 80
            CASE ('QTR')
               STATE_2_GAS_REGION_LOOKUP = 81
            CASE ('SLP')
               STATE_2_GAS_REGION_LOOKUP = 82
            CASE ('SIN')
               STATE_2_GAS_REGION_LOOKUP = 83
            CASE ('AB_52')
               STATE_2_GAS_REGION_LOOKUP = 303
            CASE ('AB_53')
               STATE_2_GAS_REGION_LOOKUP = 303
            CASE ('AL_45')
               STATE_2_GAS_REGION_LOOKUP = 289
            CASE ('AL_46')
               STATE_2_GAS_REGION_LOOKUP = 289
            CASE ('AR_43')
               STATE_2_GAS_REGION_LOOKUP = 88
            CASE ('AR_50')
               STATE_2_GAS_REGION_LOOKUP = 89
            CASE ('AZ_54')
               STATE_2_GAS_REGION_LOOKUP = 90
            CASE ('AZ_65')
               STATE_2_GAS_REGION_LOOKUP = 91
            CASE ('AZ_72')
               STATE_2_GAS_REGION_LOOKUP = 92
            CASE ('AZ_73')
               STATE_2_GAS_REGION_LOOKUP = 93
            CASE ('AZ_79')
               STATE_2_GAS_REGION_LOOKUP = 94
            CASE ('BC_52')
               STATE_2_GAS_REGION_LOOKUP = 95
            CASE ('BC_55')
               STATE_2_GAS_REGION_LOOKUP = 96
            CASE ('BCN_68')
               STATE_2_GAS_REGION_LOOKUP = 97
            CASE ('BCN_79')
               STATE_2_GAS_REGION_LOOKUP = 98
            CASE ('CA_56')
               STATE_2_GAS_REGION_LOOKUP = 300
            CASE ('CA_57')
               STATE_2_GAS_REGION_LOOKUP = 302
            CASE ('CA_58')
               STATE_2_GAS_REGION_LOOKUP = 302
            CASE ('CA_59')
               STATE_2_GAS_REGION_LOOKUP = 302
            CASE ('CA_65')
               STATE_2_GAS_REGION_LOOKUP = 302
            CASE ('CA_66')
               STATE_2_GAS_REGION_LOOKUP = 302
            CASE ('CA_70')
               STATE_2_GAS_REGION_LOOKUP = 300
            CASE ('CA_73')
! 101709. REASSIGNED FOR TOPO TO CA_99 PER BURESH
               STATE_2_GAS_REGION_LOOKUP = 300
!               STATE_2_GAS_REGION_LOOKUP = 106
            CASE ('CHI_68')
               STATE_2_GAS_REGION_LOOKUP = 107
            CASE ('CO_61')
               STATE_2_GAS_REGION_LOOKUP = 297
            CASE ('CO_62')
               STATE_2_GAS_REGION_LOOKUP = 297
            CASE ('CT_86')
               STATE_2_GAS_REGION_LOOKUP = 281
            CASE ('CT_87')
               STATE_2_GAS_REGION_LOOKUP = 281
            CASE ('CT_88')
               STATE_2_GAS_REGION_LOOKUP = 281
            CASE ('DC_16')
               STATE_2_GAS_REGION_LOOKUP = 285
            CASE ('DE_15')
               STATE_2_GAS_REGION_LOOKUP = 284
            CASE ('FL_14')
               STATE_2_GAS_REGION_LOOKUP = 115
            CASE ('FL_45')
               STATE_2_GAS_REGION_LOOKUP = 116
            CASE ('GA_45')
               STATE_2_GAS_REGION_LOOKUP = 117
            CASE ('IA_17')
               STATE_2_GAS_REGION_LOOKUP = 294
            CASE ('IA_21')
               STATE_2_GAS_REGION_LOOKUP = 294
            CASE ('IA_23')
               STATE_2_GAS_REGION_LOOKUP = 294
            CASE ('IA_25')
               STATE_2_GAS_REGION_LOOKUP = 294
            CASE ('ID_63')
               STATE_2_GAS_REGION_LOOKUP = 298
            CASE ('ID_64')
               STATE_2_GAS_REGION_LOOKUP = 298
            CASE ('ID_71')
               STATE_2_GAS_REGION_LOOKUP = 298
            CASE ('IL_18')
               STATE_2_GAS_REGION_LOOKUP = 293
            CASE ('IL_19')
               STATE_2_GAS_REGION_LOOKUP = 293
            CASE ('IL_21')
               STATE_2_GAS_REGION_LOOKUP = 293
            CASE ('IL_46')
               STATE_2_GAS_REGION_LOOKUP = 293
            CASE ('IN_02')
               STATE_2_GAS_REGION_LOOKUP = 129
            CASE ('IN_03')
               STATE_2_GAS_REGION_LOOKUP = 130
            CASE ('IN_17')
               STATE_2_GAS_REGION_LOOKUP = 131
            CASE ('IN_18')
               STATE_2_GAS_REGION_LOOKUP = 132
            CASE ('KS_51')
               STATE_2_GAS_REGION_LOOKUP = 296
            CASE ('KY_02')
               STATE_2_GAS_REGION_LOOKUP = 287
            CASE ('KY_03')
               STATE_2_GAS_REGION_LOOKUP = 287
            CASE ('KY_05')
               STATE_2_GAS_REGION_LOOKUP = 287
            CASE ('KY_46')
               STATE_2_GAS_REGION_LOOKUP = 287
            CASE ('LA_43')
               STATE_2_GAS_REGION_LOOKUP = 138
            CASE ('LA_49')
               STATE_2_GAS_REGION_LOOKUP = 139
            CASE ('LA_50')
               STATE_2_GAS_REGION_LOOKUP = 140
            CASE ('MA_86')
               STATE_2_GAS_REGION_LOOKUP = 281
            CASE ('MA_87')
               STATE_2_GAS_REGION_LOOKUP = 281
            CASE ('MA_89')
               STATE_2_GAS_REGION_LOOKUP = 281
            CASE ('MB_22')
               STATE_2_GAS_REGION_LOOKUP = 144
            CASE ('MD_01')
               STATE_2_GAS_REGION_LOOKUP = 285
            CASE ('MD_15')
               STATE_2_GAS_REGION_LOOKUP = 285
            CASE ('MD_16')
               STATE_2_GAS_REGION_LOOKUP = 285
            CASE ('ME_27')
               STATE_2_GAS_REGION_LOOKUP = 280
            CASE ('ME_85')
               STATE_2_GAS_REGION_LOOKUP = 280
            CASE ('MI_02')
               STATE_2_GAS_REGION_LOOKUP = 150
            CASE ('MI_06')
               STATE_2_GAS_REGION_LOOKUP = 151
            CASE ('MI_20')
               STATE_2_GAS_REGION_LOOKUP = 152
            CASE ('MN_17')
               STATE_2_GAS_REGION_LOOKUP = 294
            CASE ('MN_23')
               STATE_2_GAS_REGION_LOOKUP = 294
            CASE ('MN_25')
               STATE_2_GAS_REGION_LOOKUP = 294
            CASE ('MO_19')
               STATE_2_GAS_REGION_LOOKUP = 156
            CASE ('MO_42')
               STATE_2_GAS_REGION_LOOKUP = 157
            CASE ('MO_50')
               STATE_2_GAS_REGION_LOOKUP = 158
            CASE ('MO_51')
               STATE_2_GAS_REGION_LOOKUP = 159
            CASE ('MS_43')
               STATE_2_GAS_REGION_LOOKUP = 289
            CASE ('MS_45')
               STATE_2_GAS_REGION_LOOKUP = 289
            CASE ('MS_46')
               STATE_2_GAS_REGION_LOOKUP = 289
            CASE ('MT_25')
               STATE_2_GAS_REGION_LOOKUP = 295
            CASE ('MT_67')
               STATE_2_GAS_REGION_LOOKUP = 295
            CASE ('NB_27')
               STATE_2_GAS_REGION_LOOKUP = 304
            CASE ('NC_44')
               STATE_2_GAS_REGION_LOOKUP = 288
            CASE ('NC_47')
               STATE_2_GAS_REGION_LOOKUP = 288
            CASE ('ND_23')
               STATE_2_GAS_REGION_LOOKUP = 295
            CASE ('ND_25')
               STATE_2_GAS_REGION_LOOKUP = 295
            CASE ('NE_24')
               STATE_2_GAS_REGION_LOOKUP = 170
            CASE ('NE_75')
               STATE_2_GAS_REGION_LOOKUP = 171
            CASE ('NH_87')
               STATE_2_GAS_REGION_LOOKUP = 280
            CASE ('NJ_15')
               STATE_2_GAS_REGION_LOOKUP = 284
            CASE ('NJ_38')
               STATE_2_GAS_REGION_LOOKUP = 284
            CASE ('NM_50')
               STATE_2_GAS_REGION_LOOKUP = 292
            CASE ('NM_61')
               STATE_2_GAS_REGION_LOOKUP = 292
            CASE ('NM_69')
               STATE_2_GAS_REGION_LOOKUP = 292
            CASE ('NS_27')
               STATE_2_GAS_REGION_LOOKUP = 304
            CASE ('NV_57')
               STATE_2_GAS_REGION_LOOKUP = 179
            CASE ('NV_70')
               STATE_2_GAS_REGION_LOOKUP = 299
            CASE ('NV_73')
               STATE_2_GAS_REGION_LOOKUP = 301
            CASE ('NY_31')
               STATE_2_GAS_REGION_LOOKUP = 282
            CASE ('NY_32')
               STATE_2_GAS_REGION_LOOKUP = 282
            CASE ('NY_33')
               STATE_2_GAS_REGION_LOOKUP = 282
            CASE ('NY_34')
               STATE_2_GAS_REGION_LOOKUP = 282
            CASE ('NY_38')
               STATE_2_GAS_REGION_LOOKUP = 283
            CASE ('NY_39')
               STATE_2_GAS_REGION_LOOKUP = 282
            CASE ('NY_95')
               STATE_2_GAS_REGION_LOOKUP = 282
            CASE ('NY_96')
               STATE_2_GAS_REGION_LOOKUP = 282
            CASE ('OH_01')
               STATE_2_GAS_REGION_LOOKUP = 286
            CASE ('OH_02')
               STATE_2_GAS_REGION_LOOKUP = 286
            CASE ('OH_03')
               STATE_2_GAS_REGION_LOOKUP = 286
            CASE ('OH_04')
               STATE_2_GAS_REGION_LOOKUP = 286
            CASE ('OK_42')
               STATE_2_GAS_REGION_LOOKUP = 296
            CASE ('OK_50')
               STATE_2_GAS_REGION_LOOKUP = 296
            CASE ('OK_78')
               STATE_2_GAS_REGION_LOOKUP = 296
            CASE ('ON_80')
               STATE_2_GAS_REGION_LOOKUP = 197
            CASE ('ON_81')
               STATE_2_GAS_REGION_LOOKUP = 198
            CASE ('ON_82')
               STATE_2_GAS_REGION_LOOKUP = 199
            CASE ('ON_83')
               STATE_2_GAS_REGION_LOOKUP = 200
            CASE ('OR_60')
               STATE_2_GAS_REGION_LOOKUP = 298
            CASE ('OR_71')
               STATE_2_GAS_REGION_LOOKUP = 298
            CASE ('PA_01')
               STATE_2_GAS_REGION_LOOKUP = 203
            CASE ('PA_04')
               STATE_2_GAS_REGION_LOOKUP = 204
            CASE ('PA_15')
               STATE_2_GAS_REGION_LOOKUP = 205
            CASE ('PA_16')
               STATE_2_GAS_REGION_LOOKUP = 206
            CASE ('PA_95')
               STATE_2_GAS_REGION_LOOKUP = 207
            CASE ('PE_27')
               STATE_2_GAS_REGION_LOOKUP = 208
            CASE ('QC_41')
               STATE_2_GAS_REGION_LOOKUP = 209
            CASE ('RI_87')
               STATE_2_GAS_REGION_LOOKUP = 281
            CASE ('SC_44')
               STATE_2_GAS_REGION_LOOKUP = 288
            CASE ('SD_23')
               STATE_2_GAS_REGION_LOOKUP = 295
            CASE ('SD_25')
               STATE_2_GAS_REGION_LOOKUP = 295
            CASE ('SD_75')
               STATE_2_GAS_REGION_LOOKUP = 295
            CASE ('SK_26')
               STATE_2_GAS_REGION_LOOKUP = 303
            CASE ('TN_46')
               STATE_2_GAS_REGION_LOOKUP = 287
            CASE ('TX_07')
               STATE_2_GAS_REGION_LOOKUP = 291
            CASE ('TX_08')
               STATE_2_GAS_REGION_LOOKUP = 290
            CASE ('TX_09')
               STATE_2_GAS_REGION_LOOKUP = 219
            CASE ('TX_10')
               STATE_2_GAS_REGION_LOOKUP = 291
            CASE ('TX_11')
               STATE_2_GAS_REGION_LOOKUP = 292
            CASE ('TX_12')
               STATE_2_GAS_REGION_LOOKUP = 222
            CASE ('TX_13')
               STATE_2_GAS_REGION_LOOKUP = 223
            CASE ('TX_43')
               STATE_2_GAS_REGION_LOOKUP = 291
            CASE ('TX_50')
               STATE_2_GAS_REGION_LOOKUP = 290
            CASE ('TX_69')
               STATE_2_GAS_REGION_LOOKUP = 292
            CASE ('TX_76')
               STATE_2_GAS_REGION_LOOKUP = 227
            CASE ('TX_77')
               STATE_2_GAS_REGION_LOOKUP = 228
            CASE ('UT_74')
               STATE_2_GAS_REGION_LOOKUP = 297
            CASE ('VA_02')
               STATE_2_GAS_REGION_LOOKUP = 285
            CASE ('VA_15')
               STATE_2_GAS_REGION_LOOKUP = 285
            CASE ('VA_16')
               STATE_2_GAS_REGION_LOOKUP = 285
            CASE ('VA_47')
               STATE_2_GAS_REGION_LOOKUP = 285
            CASE ('VT_86')
               STATE_2_GAS_REGION_LOOKUP = 280
            CASE ('VT_87')
               STATE_2_GAS_REGION_LOOKUP = 280
            CASE ('WA_71')
               STATE_2_GAS_REGION_LOOKUP = 298
            CASE ('WI_20')
               STATE_2_GAS_REGION_LOOKUP = 293
            CASE ('WI_23')
               STATE_2_GAS_REGION_LOOKUP = 293
            CASE ('WV_01')
               STATE_2_GAS_REGION_LOOKUP = 286
            CASE ('WV_02')
               STATE_2_GAS_REGION_LOOKUP = 286
            CASE ('WV_47')
               STATE_2_GAS_REGION_LOOKUP = 286
            CASE ('WY_25')
               STATE_2_GAS_REGION_LOOKUP = 297
            CASE ('WY_63')
               STATE_2_GAS_REGION_LOOKUP = 297
            CASE ('WY_74')
               STATE_2_GAS_REGION_LOOKUP = 297
            CASE ('WY_75')
               STATE_2_GAS_REGION_LOOKUP = 297
            CASE ('AB_98')
               STATE_2_GAS_REGION_LOOKUP = 303
            CASE ('AZ_94')
               STATE_2_GAS_REGION_LOOKUP = 247
            CASE ('AZ_99')
               STATE_2_GAS_REGION_LOOKUP = 248
            CASE ('BC_98')
               STATE_2_GAS_REGION_LOOKUP = 249
            CASE ('CA_99')
               STATE_2_GAS_REGION_LOOKUP = 250
            CASE ('ID_29')
               STATE_2_GAS_REGION_LOOKUP = 298
            CASE ('KS_61')
               STATE_2_GAS_REGION_LOOKUP = 296
            CASE ('NY_40')
               STATE_2_GAS_REGION_LOOKUP = 282
            CASE ('NY_97')
               STATE_2_GAS_REGION_LOOKUP = 282
            CASE ('OK_08')
               STATE_2_GAS_REGION_LOOKUP = 296
            CASE ('ON_40')
               STATE_2_GAS_REGION_LOOKUP = 256
            CASE ('WY_29')
               STATE_2_GAS_REGION_LOOKUP = 297
            CASE ('AR_42')
               STATE_2_GAS_REGION_LOOKUP = 258
            CASE ('BCN_58')
               STATE_2_GAS_REGION_LOOKUP = 259
            CASE ('BCS_68')
               STATE_2_GAS_REGION_LOOKUP = 260
            CASE ('CA_71')
               STATE_2_GAS_REGION_LOOKUP = 300
            CASE ('CA_76')
               STATE_2_GAS_REGION_LOOKUP = 300
            CASE ('CA_77')
               STATE_2_GAS_REGION_LOOKUP = 300
            CASE ('CA_94')
               STATE_2_GAS_REGION_LOOKUP = 302
            CASE ('DC_12')
               STATE_2_GAS_REGION_LOOKUP = 285
            CASE ('KY_19')
               STATE_2_GAS_REGION_LOOKUP = 287
            CASE ('KY_91')
               STATE_2_GAS_REGION_LOOKUP = 287
            CASE ('MD_12')
               STATE_2_GAS_REGION_LOOKUP = 285
            CASE ('MN_21')
               STATE_2_GAS_REGION_LOOKUP = 294
            CASE ('MN_90')
               STATE_2_GAS_REGION_LOOKUP = 294
            CASE ('MT_90')
               STATE_2_GAS_REGION_LOOKUP = 295
            CASE ('ND_90')
               STATE_2_GAS_REGION_LOOKUP = 295
            CASE ('OH_91')
               STATE_2_GAS_REGION_LOOKUP = 286
            CASE ('PA_02')
               STATE_2_GAS_REGION_LOOKUP = 274
            CASE ('PA_09')
               STATE_2_GAS_REGION_LOOKUP = 275
            CASE ('PA_13')
               STATE_2_GAS_REGION_LOOKUP = 276
            CASE ('SD_90')
               STATE_2_GAS_REGION_LOOKUP = 295
            CASE ('VA_12')
               STATE_2_GAS_REGION_LOOKUP = 285
            CASE ('WY_61')
               STATE_2_GAS_REGION_LOOKUP = 297
            CASE('MENHVT')
               STATE_2_GAS_REGION_LOOKUP = 280
            CASE('MARICT')
               STATE_2_GAS_REGION_LOOKUP = 281
            CASE('NY STA')
               STATE_2_GAS_REGION_LOOKUP = 282
            CASE('NY CIT')
               STATE_2_GAS_REGION_LOOKUP = 283
            CASE('NJ DE')
               STATE_2_GAS_REGION_LOOKUP = 284
            CASE('MDDCVA')
               STATE_2_GAS_REGION_LOOKUP = 285
!            CASE('PA')
!               STATE_2_GAS_REGION_LOOKUP = 279
            CASE('OH WV')
               STATE_2_GAS_REGION_LOOKUP = 286
            CASE('KY TN')
               STATE_2_GAS_REGION_LOOKUP = 287
            CASE('NC SC')
               STATE_2_GAS_REGION_LOOKUP = 288
!            CASE('GA')
!               STATE_2_GAS_REGION_LOOKUP = 279
            CASE('AL MS')
               STATE_2_GAS_REGION_LOOKUP = 289
!            CASE('FL')
!               STATE_2_GAS_REGION_LOOKUP = 279
!            CASE('LA')
!               STATE_2_GAS_REGION_LOOKUP = 279
            CASE('TX N')
               STATE_2_GAS_REGION_LOOKUP = 290
            CASE('TX S')
               STATE_2_GAS_REGION_LOOKUP = 291
            CASE('TXW NM')
               STATE_2_GAS_REGION_LOOKUP = 292
!            CASE('AZ')
!               STATE_2_GAS_REGION_LOOKUP = 279
!            CASE('AR')
!               STATE_2_GAS_REGION_LOOKUP = 279
!            CASE('MO')
!               STATE_2_GAS_REGION_LOOKUP = 279
!            CASE('IN')
!               STATE_2_GAS_REGION_LOOKUP = 279
!            CASE('MI')
!               STATE_2_GAS_REGION_LOOKUP = 279
            CASE('IL WI')
               STATE_2_GAS_REGION_LOOKUP = 293
            CASE('IA MN')
               STATE_2_GAS_REGION_LOOKUP = 294
            CASE('MTNDSD')
               STATE_2_GAS_REGION_LOOKUP = 295
!            CASE('NE')
!               STATE_2_GAS_REGION_LOOKUP = 279
            CASE('KS OK')
               STATE_2_GAS_REGION_LOOKUP = 296
            CASE('COWYUT')
               STATE_2_GAS_REGION_LOOKUP = 297
            CASE('WAORID')
               STATE_2_GAS_REGION_LOOKUP = 298
            CASE('NV N')
               STATE_2_GAS_REGION_LOOKUP = 299
            CASE('CA N')
               STATE_2_GAS_REGION_LOOKUP = 300
            CASE('NV S')
               STATE_2_GAS_REGION_LOOKUP = 301
            CASE('CA S')
               STATE_2_GAS_REGION_LOOKUP = 302
            CASE('AB SK')
               STATE_2_GAS_REGION_LOOKUP = 303
!            CASE('BC')
!               STATE_2_GAS_REGION_LOOKUP = 279
!            CASE('MN')
!               STATE_2_GAS_REGION_LOOKUP = 279
            CASE('NB NS')
               STATE_2_GAS_REGION_LOOKUP = 304
!            CASE('ON')
!               STATE_2_GAS_REGION_LOOKUP = 279
!            CASE('QB')
!               STATE_2_GAS_REGION_LOOKUP = 279
            CASE('MX STX')
               STATE_2_GAS_REGION_LOOKUP = 279
            CASE('MX WTX')
               STATE_2_GAS_REGION_LOOKUP = 279
            CASE('MX AZ')
               STATE_2_GAS_REGION_LOOKUP = 279
            CASE('MX CA')
               STATE_2_GAS_REGION_LOOKUP = 279
            CASE DEFAULT
               STATE_2_GAS_REGION_LOOKUP = -1
            END SELECT
      RETURN
      END
!***********************************************************************
!
      FUNCTION MARKET_AREA_LOOKUP(R_MARKET_AREA_COMPANY_ID_NUM)
!
!***********************************************************************
      INTEGER (kind=2) ::  MARKET_AREA_LOOKUP
      CHARACTER (len=5) ::  R_MARKET_AREA_COMPANY_ID_NUM
      CHARACTER (len=3) ::  STR_NUM,MARKET_AREA
         MARKET_AREA = R_MARKET_AREA_COMPANY_ID_NUM(1:2)
         MARKET_AREA_LOOKUP = -1
         IF(MARKET_AREA == 'MK' .OR. MARKET_AREA == 'ZN') THEN
            STR_NUM = R_MARKET_AREA_COMPANY_ID_NUM(3:)
            READ(STR_NUM,'(I3)') MARKET_AREA_LOOKUP
            IF(MARKET_AREA_LOOKUP < 0 .OR. MARKET_AREA_LOOKUP > 599)
     +                                           MARKET_AREA_LOOKUP = -1
         ENDIF
!         RETURN
      RETURN
      END
! 020107. NEW VARIABLES FOR AOD
!***********************************************************************
!
!        PROGRAM TO READ MULTI-TAB INFORMATION ON MARKET GROUPS
!                 AND CONVERT TO BINARY FORMAT
!                       COPYRIGHT (C) 2001
!      ALL RIGHTS RESERVED M.S. GERBER & ASSOCIATES, INC.
!
!***********************************************************************
!
      SUBROUTINE MK_OBJECT
      use end_routine, only: end_program, er_message
!
!***********************************************************************
!
      INCLUDE 'SpinLib.MON'
      USE SIZECOM
      LOGICAL (kind=1) ::  SAVE_MK_FILE_EXISTS=.FALSE. 
      LOGICAL ::  R_MK_FILE_EXISTS
      INTEGER (kind=2) ::  UNIT_NUM=10 
      INTEGER (kind=2) ::  INUNIT
      INTEGER (kind=2) ::  IREC
      INTEGER (kind=2) ::  DELETE
      INTEGER (kind=2) ::  LRECL=46 
      INTEGER (kind=2) ::  SAVE_MARKET_GROUPS_TABLES=0 
      INTEGER (kind=2) ::  R_MARKET_GROUPS_TABLES
      INTEGER (kind=2) ::  SAVE_MARKET_GROUPS_RECORDS=0 
      INTEGER (kind=2) ::  R_MARKET_GROUPS_RECORDS
      INTEGER (kind=2) ::  TRANSACTION_GROUP_ID
      INTEGER ::  IOS
      CHARACTER (len=5) ::  MARKET_GROUPS_FILE,OVERLAY_FAMILY_NAME
      CHARACTER (len=3) ::  HOURLY_PRICE_NAME
      CHARACTER (len=256) ::  FILE_NAME,FILE_NAME_OVL
      CHARACTER (len=256) ::  OUTPUT_DIRECTORY
      CHARACTER (len=256) ::  BASE_FILE_DIRECTORY
      CHARACTER (len=152) ::  MESSAGE
      LOGICAL (kind=4) ::  FILE_EXISTS
! DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER (len=1024) ::  RECLN
!
! SIMULATION VARIABLES
!
      CHARACTER (len=1) ::  MARKET_AREA_ACTIVE
      CHARACTER (len=1) ::  REPORT_THIS_GROUP
      CHARACTER (len=1) ::  MARKET_LOADS_ACTIVE
      CHARACTER (len=1) ::  REPORT_MARKET_LOADS
      CHARACTER (len=1) ::  MARKET_THERMAL_ACTIVE
      CHARACTER (len=1) ::  REPORT_MARKET_THERMAL
      CHARACTER (len=1) ::  MARKET_HYDRO_ACTIVE
      CHARACTER (len=1) ::  REPORT_MARKET_HYDRO
      CHARACTER (len=1) ::  MARKET_CONTRACTS_ACTIVE
      CHARACTER (len=1) ::  REPORT_MARKET_CONTRACTS
      CHARACTER (len=6) ::  PORTFOLIO_MARKET_AREA_ABBREV
      CHARACTER (len=6) ::  PORTFOLIO_MARKET_AREA_ID
      CHARACTER (len=20) ::    PORTFOLIO_MARKET_AREA_NAME
!      CHARACTER*50 COMMENT
!
! FILE MANAGEMENT VARIABLES
!
      CHARACTER (len=17) ::  FILE_TYPE='Market Group     ' 
      CHARACTER (len=2) ::  MKROUP_OL='BC' ,R_MKROUP_OL
      LOGICAL (kind=1) ::  LAHEY_LF95
      CHARACTER (len=30) ::  SCREEN_OUTPUT
!
!
!
!
!***********************************************************************
!
!          ROUTINE TO CONVERT FROM ASCII TO DIRECT-ACESS BINARY
!          COPYRIGHT (C) 1983-98  M.S. GERBER & ASSOCIATES, INC.
!
!***********************************************************************
!
! CONVERT THE MARKET GROUPS FILE
!
!
!***********************************************************************
      ENTRY MK_MAKEBIN
!***********************************************************************
!      CALL LOCATE(16,30)
!      WRITE(6,1010) MARKET_GROUPS_FILE()
!      CALL CLS(17,9,36)
!      CALL LOCATE(17,9)
!      WRITE(6,1010) FILE_TYPE
      FILE_NAME = trim(BASE_FILE_DIRECTORY())//
     +                      "MKB"//trim(MARKET_GROUPS_FILE())//".DAT"
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      IF(FILE_EXISTS) THEN
         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//MARKET_GROUPS_FILE()
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_LOCATE_WRITE(16,30,MARKET_GROUPS_FILE(),
     +                                                   ALL_VERSIONS,0)
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         ENDIF
!
         SAVE_MK_FILE_EXISTS = .TRUE.
!
         OPEN(10,FILE=FILE_NAME)
         OPEN(11,FILE=trim(OUTPUT_DIRECTORY())//"BCMKROUP.BIN",
     +                      ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
!
!
         SAVE_MARKET_GROUPS_TABLES = 0
         MARKET_AREA_ACTIVE = 'T'
         REPORT_THIS_GROUP = 'T'
         MARKET_LOADS_ACTIVE = 'T'
         REPORT_MARKET_LOADS = 'T'
         MARKET_THERMAL_ACTIVE = 'T'
         REPORT_MARKET_THERMAL = 'T'
         MARKET_HYDRO_ACTIVE = 'T'
         REPORT_MARKET_HYDRO = 'T'
         MARKET_CONTRACTS_ACTIVE = 'T'
         REPORT_MARKET_CONTRACTS = 'T'
!
         IREC = 0
         READ(10,*) DELETE
         DO
            DO
               READ(10,1000,IOSTAT=IOS) RECLN
!
               IF(IOS /= 0) EXIT
!
               IF(RECLN(1:1) == '7') THEN
                  EXIT
               ENDIF
!
               RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               READ(RECLN,*,ERR=200)   DELETE,
     +                                 PORTFOLIO_MARKET_AREA_NAME,
     +                                 PORTFOLIO_MARKET_AREA_ABBREV,
     +                                 PORTFOLIO_MARKET_AREA_ID,
     +                                 MARKET_AREA_ACTIVE,
     +                                 REPORT_THIS_GROUP,
     +                                 TRANSACTION_GROUP_ID,
     +                                 MARKET_LOADS_ACTIVE,
     +                                 REPORT_MARKET_LOADS,
     +                                 MARKET_THERMAL_ACTIVE,
     +                                 REPORT_MARKET_THERMAL,
     +                                 MARKET_HYDRO_ACTIVE,
     +                                 REPORT_MARKET_HYDRO,
     +                                 MARKET_CONTRACTS_ACTIVE,
     +                                 REPORT_MARKET_CONTRACTS
!
               IREC = IREC + 1
               WRITE(11,REC=IREC)      DELETE,
     +                                 PORTFOLIO_MARKET_AREA_NAME,
     +                                 PORTFOLIO_MARKET_AREA_ABBREV,
     +                                 PORTFOLIO_MARKET_AREA_ID,
     +                                 MARKET_AREA_ACTIVE,
     +                                 REPORT_THIS_GROUP,
     +                                 TRANSACTION_GROUP_ID,
     +                                 MARKET_LOADS_ACTIVE,
     +                                 REPORT_MARKET_LOADS,
     +                                 MARKET_THERMAL_ACTIVE,
     +                                 REPORT_MARKET_THERMAL,
     +                                 MARKET_HYDRO_ACTIVE,
     +                                 REPORT_MARKET_HYDRO,
     +                                 MARKET_CONTRACTS_ACTIVE,
     +                                 REPORT_MARKET_CONTRACTS
            ENDDO
            SAVE_MARKET_GROUPS_TABLES = SAVE_MARKET_GROUPS_TABLES + 1
            IF(IOS /= 0) EXIT
         ENDDO
         CLOSE(10)
      ELSE IF(INDEX(MARKET_GROUPS_FILE(),'NONE') == 0) THEN
         CALL STOP_NOFILE(FILE_TYPE,FILE_NAME)
      ELSE
         OPEN(11,FILE=trim(OUTPUT_DIRECTORY())//"BCMKROUP.BIN",
     +                      ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
!
         SAVE_MK_FILE_EXISTS = .FALSE.
!
      ENDIF
      SAVE_MARKET_GROUPS_RECORDS = IREC
!      ENDFILE(11)
      CLOSE(11)
      RETURN
!***********************************************************************
!
!          ROUTINE TO CREATE OVERLAY FILES
!          COPYRIGHT (C) 1984-88  M.S. GERBER & ASSOCIATES, INC.
!          COPYRIGHT (C) 1991-92  M.S. GERBER & ASSOCIATES, INC.
!
!***********************************************************************
!
! OVERLAY THE SYSTEM-FORECAST FILE
!***********************************************************************
      ENTRY MK_MAKEOVL(OVERLAY_FAMILY_NAME)
!***********************************************************************
      IF(LAHEY_LF95()) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSE
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         CALL LOCATE(10,51)
      ENDIF
      FILE_NAME_OVL=trim(OUTPUT_DIRECTORY())//"MKO"//
     +                               trim(OVERLAY_FAMILY_NAME)//".DAT"
      OPEN(10,FILE=FILE_NAME_OVL)
      READ(10,*) DELETE
      INUNIT = 12
      IF(MKROUP_OL == 'BC') THEN
         OPEN(11,FILE=trim(OUTPUT_DIRECTORY())//"BCMKROUP.BIN",
     +                                       ACCESS="DIRECT",RECL=LRECL)
         INUNIT = 11
      ENDIF
!     INQUIRE(UNIT=12,OPENED=FILE_EXISTS)
      FILE_NAME = trim(OUTPUT_DIRECTORY())//"OLMKROUP.BIN"
!     INQUIRE(FILE=FILE_NAME,OPENED=FILE_EXISTS,NUMBER=UNIT_NUMBER)
      OPEN(12,FILE=FILE_NAME,ACCESS="DIRECT",
     +                           STATUS="UNKNOWN",RECL=LRECL,IOSTAT=IOS)
      IF(IOS /= 0) THEN
         CALL IOSTAT_MSG(IOS,MESSAGE)
         WRITE(4,*) trim(MESSAGE)
         er_message='in MK_MAKEOVL see WARNINGS messages'
         call end_program(er_message)
      ENDIF
      IREC = 0
      DELETE = 1
!
      READ(10,1000,IOSTAT=IOS) RECLN
      DOWHILE(RECLN(1:1) == '7')
!
         READ(10,1000,IOSTAT=IOS) RECLN
!
      ENDDO
!
      DO
         IREC = IREC + 1
         READ(INUNIT,REC=IREC,IOSTAT=IOS)  DELETE,
     +                                 PORTFOLIO_MARKET_AREA_NAME,
     +                                 PORTFOLIO_MARKET_AREA_ABBREV,
     +                                 PORTFOLIO_MARKET_AREA_ID,
     +                                 MARKET_AREA_ACTIVE,
     +                                 REPORT_THIS_GROUP,
     +                                 TRANSACTION_GROUP_ID,
     +                                 MARKET_LOADS_ACTIVE,
     +                                 REPORT_MARKET_LOADS,
     +                                 MARKET_THERMAL_ACTIVE,
     +                                 REPORT_MARKET_THERMAL,
     +                                 MARKET_HYDRO_ACTIVE,
     +                                 REPORT_MARKET_HYDRO,
     +                                 MARKET_CONTRACTS_ACTIVE,
     +                                 REPORT_MARKET_CONTRACTS
         IF(IOS /= 0) EXIT
!        READ(10,1000,IOSTAT=IOS) RECLN
!        IF(IOS == 0) THEN
         RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
         READ(RECLN,*,ERR=200)         DELETE,
     +                                 PORTFOLIO_MARKET_AREA_NAME,
     +                                 PORTFOLIO_MARKET_AREA_ABBREV,
     +                                 PORTFOLIO_MARKET_AREA_ID,
     +                                 MARKET_AREA_ACTIVE,
     +                                 REPORT_THIS_GROUP,
     +                                 TRANSACTION_GROUP_ID,
     +                                 MARKET_LOADS_ACTIVE,
     +                                 REPORT_MARKET_LOADS,
     +                                 MARKET_THERMAL_ACTIVE,
     +                                 REPORT_MARKET_THERMAL,
     +                                 MARKET_HYDRO_ACTIVE,
     +                                 REPORT_MARKET_HYDRO,
     +                                 MARKET_CONTRACTS_ACTIVE,
     +                                 REPORT_MARKET_CONTRACTS
!
         READ(10,1000,IOSTAT=IOS) RECLN
         DOWHILE(RECLN(1:1) == '7')
!
            READ(10,1000,IOSTAT=IOS) RECLN
!
         ENDDO
!         ENDIF
!
         WRITE(12,REC=IREC)            DELETE,
     +                                 PORTFOLIO_MARKET_AREA_NAME,
     +                                 PORTFOLIO_MARKET_AREA_ABBREV,
     +                                 PORTFOLIO_MARKET_AREA_ID,
     +                                 MARKET_AREA_ACTIVE,
     +                                 REPORT_THIS_GROUP,
     +                                 TRANSACTION_GROUP_ID,
     +                                 MARKET_LOADS_ACTIVE,
     +                                 REPORT_MARKET_LOADS,
     +                                 MARKET_THERMAL_ACTIVE,
     +                                 REPORT_MARKET_THERMAL,
     +                                 MARKET_HYDRO_ACTIVE,
     +                                 REPORT_MARKET_HYDRO,
     +                                 MARKET_CONTRACTS_ACTIVE,
     +                                 REPORT_MARKET_CONTRACTS
      ENDDO
      IF(IREC /= SAVE_MARKET_GROUPS_RECORDS) THEN
         WRITE(4,*) "MARKET GROUP OVERLAY DIFFERENT LENGTH"
         WRITE(4,*) "THAN THE BASE FILE. OVERLAY MUST BE THE SAME"
         WRITE(4,*) "LENGTH. ",FILE_NAME_OVL
         WRITE(4,*) "BASE RECORDS ",SAVE_MARKET_GROUPS_RECORDS
         WRITE(4,*) "OVERLAY RECORDS ",IREC
      ENDIF
      CLOSE(10)
      CLOSE(12)
      IF(MKROUP_OL == 'BC') CLOSE(11)
      MKROUP_OL = 'OL'
      RETURN
!
!***********************************************************************
      ENTRY RESET_MKROUP_OL
!***********************************************************************
         MKROUP_OL = 'BC'
      RETURN
!
!***********************************************************************
      ENTRY RETURN_MKROUP_OL(R_MKROUP_OL)
!***********************************************************************
         R_MKROUP_OL = MKROUP_OL
      RETURN
!***********************************************************************
      ENTRY DOES_MK_FILE_EXIST(R_MK_FILE_EXISTS)
!***********************************************************************
         R_MK_FILE_EXISTS = SAVE_MK_FILE_EXISTS
      RETURN
!***********************************************************************
      ENTRY GET_MARKET_GROUPS_TABLES(R_MARKET_GROUPS_TABLES)
!***********************************************************************
         R_MARKET_GROUPS_TABLES = SAVE_MARKET_GROUPS_TABLES
      RETURN
!***********************************************************************
      ENTRY GET_MARKET_GROUPS_RECORDS(R_MARKET_GROUPS_RECORDS)
!***********************************************************************
         R_MARKET_GROUPS_RECORDS = SAVE_MARKET_GROUPS_RECORDS
      RETURN
!***********************************************************************
      ENTRY OPEN_MK_FILE
!***********************************************************************
         OPEN(UNIT_NUM,FILE=trim(OUTPUT_DIRECTORY())//MKROUP_OL//
     +         "MKROUP.BIN",ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
      RETURN
!
!***********************************************************************
      ENTRY CLOSE_MK_FILE
!***********************************************************************
         CLOSE(UNIT_NUM)
      RETURN
!

  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from wh_objt SIID358'
      call end_program(er_message)
!
 1000 FORMAT(A)
 1010 FORMAT('&',A)
      END
!
!
!
!
!
! 6/8/01. CALL BEFORE RESOURCES OR CUSTOMERS ARE READ.
!***********************************************************************
!
      FUNCTION READ_MARKET_GROUPS_DATA()
!
!***********************************************************************
!
!
!
!
      USE IREC_ENDPOINT_CONTROL
      use grx_planning_routines
      INCLUDE 'SpinLib.MON'
      USE SIZECOM
      use globecom
      SAVE

      LOGICAL (kind=1) ::  SAVE_MK_FILE_EXISTS=.FALSE. 
      INTEGER (kind=4) ::  VALUES_2_ZERO
      INTEGER (kind=2) ::  DELETE
      INTEGER (kind=2) ::  CURRENT_RECORD
      INTEGER (kind=2) ::  MARKET_GROUP
      INTEGER (kind=2) ::  GET_MARKET_GROUPS
      INTEGER (kind=2) ::  MARKET_GROUPS_RECORDS
      INTEGER (kind=2) ::  R_MARKET_GROUP
      INTEGER (kind=2) ::  MAX_MARKET_GROUPS=599 
      INTEGER (kind=2) ::  SAVE_MARKET_GROUPS_RECORDS=0 
      INTEGER (kind=2) ::  TEMP_I
      INTEGER (kind=2) ::  I
      INTEGER (kind=2) ::  HG
      LOGICAL (kind=1) ::  READ_MARKET_GROUPS_DATA
      LOGICAL (kind=1) ::  MARKET_GROUP_ACTIVE_SWITCH
      LOGICAL (kind=1) ::  GET_MARKET_SPINNING_CAPACITY
      LOGICAL (kind=1) ::  GET_MARKET_RAMP_RATES
      LOGICAL (kind=1) ::  HYDRO_AGGREGATION=.FALSE. 
      REAL (kind=4) ::  R_MARKET_AREA_INDEX
      REAL (kind=4) ::  SCENARIO_NUMBER
      REAL (kind=4) ::  GET_SCENARIO_LOAD_SHAPE
      LOGICAL (kind=4) ::  MK_FILE_EXISTS


      CHARACTER (len=3) ::  R_GET_HOURLY_PRICE_NAME
      CHARACTER (len=20) ::  R_GET_MARKETACTION_GROUP_NAME
      CHARACTER (len=35) ::  GET_GROUP_NAME
      LOGICAL (kind=1) ::  GET_TF_HOURLY_PRICE_NAME
      LOGICAL (kind=1) ::  GET_TF_GROUP_NAME
      LOGICAL (kind=1) ::  GET_TF_MARKETACTION_GROUP_NAME
      LOGICAL (kind=1) ::  GET_TF_MARKET_GROUP_NOX_SEASON
      LOGICAL (kind=1) ::  YES_PORT_MARKET_AREAS_REPORT
      LOGICAL (kind=1) ::  PORT_MARKET_AREAS_REPORT
      LOGICAL (kind=1) ::  PORT_MARKET_AREAS_REPORT_OPEN
!
! SIMULATION VARIABLES
!
!
! SIMULATION VARIABLES
!
      LOGICAL (kind=1) ::  IN_ACTIVE_MARKET_AREA
      LOGICAL (kind=1) ::  IN_ACTIVE_THERMAL_MARKET_AREA
      LOGICAL (kind=1) ::  IN_ACTIVE_HYDRO_MARKET_AREA
      LOGICAL (kind=1) ::  IN_ACTIVE_LOADS_MARKET_AREA
      LOGICAL (kind=1) ::  IN_ACTIVE_CONTR_MARKET_AREA
      LOGICAL (kind=1) ::  REPORT_MARKET_AREA
      LOGICAL (kind=1) ::  GET_PORTFOLIO_MARKET_AREA_NAME
      LOGICAL (kind=1) ::  GET_PORT_MARKET_AREA_ABBREV
      LOGICAL (kind=1) ::  MOD_TRANS_GROUP_ACTIVE_SWITCH
      INTEGER (kind=2) ::  USE_MARKET_AREA(0:599)
      INTEGER (kind=2) ::  MARKET_AREA_POSITION(0:599)
      INTEGER (kind=2) ::  MARKET_AREA_LOOKUP
      INTEGER (kind=2) ::  LOCAL_INDEX
      INTEGER (kind=2) ::  TG_POSITION
      INTEGER (kind=2) ::  GET_TRANS_GROUP_POSITION
      CHARACTER (len=1) ::  MARKET_AREA_ACTIVE(:)
      CHARACTER (len=1) ::  REPORT_THIS_GROUP(0:599)
      CHARACTER (len=1) ::  TEMP_REPORT_THIS_GROUP
      CHARACTER (len=1) ::  QUOTE/'"'/
      CHARACTER (len=1) ::  MARKET_LOADS_ACTIVE(0:599)
      CHARACTER (len=1) ::  REPORT_MARKET_LOADS(0:599)
      CHARACTER (len=1) ::  MARKET_THERMAL_ACTIVE(0:599)
      CHARACTER (len=1) ::  REPORT_MARKET_THERMAL(0:599)
      CHARACTER (len=1) ::  MARKET_HYDRO_ACTIVE(0:599)
      CHARACTER (len=1) ::  REPORT_MARKET_HYDRO(0:599)
      CHARACTER (len=1) ::  MARKET_CONTRACTS_ACTIVE(0:599)
      CHARACTER (len=1) ::  REPORT_MARKET_CONTRACTS(0:599)
      CHARACTER (len=1) ::  TEMP_MARKET_LOADS_ACTIVE
      CHARACTER (len=1) ::  TEMP_REPORT_MARKET_LOADS
      CHARACTER (len=1) ::  TEMP_MARKET_THERMAL_ACTIVE
      CHARACTER (len=1) ::  TEMP_REPORT_MARKET_THERMAL
      CHARACTER (len=1) ::  TEMP_MARKET_HYDRO_ACTIVE
      CHARACTER (len=1) ::  TEMP_REPORT_MARKET_HYDRO
      CHARACTER (len=1) ::  TEMP_MARKET_CONTRACTS_ACTIVE
      CHARACTER (len=1) ::  TEMP_REPORT_MARKET_CONTRACTS
      CHARACTER (len=5) ::     LOCAL_NAME,GET_SCENAME
      CHARACTER (len=6) ::  PORTFOLIO_MARKET_AREA_ABBREV(:)
      CHARACTER (len=6) ::  PORTFOLIO_MARKET_AREA_ID(:)
      CHARACTER (len=6) ::  R_BASECASE_MARKET_AREA_ID
      CHARACTER (len=6) ::  R_PORTFOLIO_MARKET_AREA_ABBREV
      CHARACTER (len=64) ::    FILE_NAME
      CHARACTER (len=20) ::  PORTFOLIO_MARKET_AREA_NAME(:)
      CHARACTER (len=20) ::  R_PORTFOLIO_MARKET_AREA_NAME
      CHARACTER (len=1024) ::  PW_REC
      ALLOCATABLE ::
     +               PORTFOLIO_MARKET_AREA_NAME,
     +               PORTFOLIO_MARKET_AREA_ABBREV,
     +               PORTFOLIO_MARKET_AREA_ID,
     +               MARKET_AREA_ACTIVE


         READ_MARKET_GROUPS_DATA = .FALSE.
         SAVE_MK_FILE_EXISTS = .FALSE.
!
         USE_MARKET_AREA = 1
         MARKET_AREA_POSITION = 0
!
         CALL DOES_MK_FILE_EXIST(MK_FILE_EXISTS)
!
         IF(.NOT. MK_FILE_EXISTS) RETURN
!
         CALL GET_MARKET_GROUPS_RECORDS(MARKET_GROUPS_RECORDS)
!         MARKET_GROUPS_RECORDS = 600
         SAVE_MARKET_GROUPS_RECORDS = MARKET_GROUPS_RECORDS

!
         CALL OPEN_MK_FILE
         IF(ALLOCATED(PORTFOLIO_MARKET_AREA_NAME))
     +            DEALLOCATE(
     +               PORTFOLIO_MARKET_AREA_NAME,
     +               PORTFOLIO_MARKET_AREA_ABBREV,
     +               PORTFOLIO_MARKET_AREA_ID,
     +               MARKET_AREA_ACTIVE)
         ALLOCATE(
     +               PORTFOLIO_MARKET_AREA_NAME(MAX_MARKET_GROUPS),
     +               PORTFOLIO_MARKET_AREA_ABBREV(MAX_MARKET_GROUPS),
     +               PORTFOLIO_MARKET_AREA_ID(MAX_MARKET_GROUPS),
     +               MARKET_AREA_ACTIVE(MAX_MARKET_GROUPS))
!
         PORT_MARKET_AREAS_REPORT = YES_PORT_MARKET_AREAS_REPORT()
!
         MARKET_THERMAL_ACTIVE = 'T'
!
         DO CURRENT_RECORD = 1, MARKET_GROUPS_RECORDS
            READ(10,REC=CURRENT_RECORD) DELETE,
     +               PORTFOLIO_MARKET_AREA_NAME(CURRENT_RECORD),
     +               PORTFOLIO_MARKET_AREA_ABBREV(CURRENT_RECORD),
     +               PORTFOLIO_MARKET_AREA_ID(CURRENT_RECORD),
     +               MARKET_AREA_ACTIVE(CURRENT_RECORD),
     +               TEMP_REPORT_THIS_GROUP,
     +               TRANSACTION_GROUP_ID,
     +               TEMP_MARKET_LOADS_ACTIVE,
     +               TEMP_REPORT_MARKET_LOADS,
     +               TEMP_MARKET_THERMAL_ACTIVE,
     +               TEMP_REPORT_MARKET_THERMAL,
     +               TEMP_MARKET_HYDRO_ACTIVE,
     +               TEMP_REPORT_MARKET_HYDRO,
     +               TEMP_MARKET_CONTRACTS_ACTIVE,
     +               TEMP_REPORT_MARKET_CONTRACTS
!
            IF(.NOT.
     +        MOD_TRANS_GROUP_ACTIVE_SWITCH(TRANSACTION_GROUP_ID)) CYCLE
            LOCAL_NAME =
     +                   PORTFOLIO_MARKET_AREA_ID(CURRENT_RECORD)
            LOCAL_INDEX = MARKET_AREA_LOOKUP(LOCAL_NAME)
            IF(LOCAL_INDEX >= 0 .AND. LOCAL_INDEX < 600) THEN
               MARKET_AREA_POSITION(LOCAL_INDEX) = CURRENT_RECORD
            ENDIF
            IF(LOCAL_INDEX >= 0 .AND. LOCAL_INDEX < 600) THEN
               IF(MARKET_AREA_ACTIVE(CURRENT_RECORD) == 'F') THEN
                  USE_MARKET_AREA(LOCAL_INDEX) = 0
               ENDIF
               REPORT_THIS_GROUP(LOCAL_INDEX) = TEMP_REPORT_THIS_GROUP
!
               MARKET_LOADS_ACTIVE(LOCAL_INDEX) =
     +                                          TEMP_MARKET_LOADS_ACTIVE
               REPORT_MARKET_LOADS(LOCAL_INDEX) =
     +                                          TEMP_REPORT_MARKET_LOADS
               MARKET_THERMAL_ACTIVE(LOCAL_INDEX) =
     +                                        TEMP_MARKET_THERMAL_ACTIVE
               REPORT_MARKET_THERMAL(LOCAL_INDEX) =
     +                                        TEMP_REPORT_MARKET_THERMAL
               MARKET_HYDRO_ACTIVE(LOCAL_INDEX) =
     +                                          TEMP_MARKET_HYDRO_ACTIVE
               REPORT_MARKET_HYDRO(LOCAL_INDEX) =
     +                                          TEMP_REPORT_MARKET_HYDRO
               MARKET_CONTRACTS_ACTIVE(LOCAL_INDEX) =
     +                                      TEMP_MARKET_CONTRACTS_ACTIVE
               REPORT_MARKET_CONTRACTS(LOCAL_INDEX) =
     +                                      TEMP_REPORT_MARKET_CONTRACTS
!
            ELSE
               WRITE(4,*) "BAD MARKET AREA ID IN THE MARKET GROUPS FILE"
               WRITE(4,*) "MARKET AREA ID = ",LOCAL_NAME
            ENDIF
            IF( MARKET_AREA_ACTIVE(CURRENT_RECORD) == 'F') THEN
!
! 07/24/03. MOVED OUT OF THE CONDITIONAL ON MARKET_AREA_ACTIVE
!
!               IF(LOCAL_INDEX >= 0 .AND. LOCAL_INDEX < 400) THEN
!                  USE_MARKET_AREA(LOCAL_INDEX) = 0
!                  REPORT_THIS_GROUP(LOCAL_INDEX) =
!     +                                            TEMP_REPORT_THIS_GROUP
!               ELSE
!                  WRITE(4,*) "BAD NAME IN THE MARKET GROUPS FILE"
!                  WRITE(4,*) "NAME= ",LOCAL_NAME
!               ENDIF
            ELSE
               IF(PORT_MARKET_AREAS_REPORT) THEN
                  IF(.NOT. PORT_MARKET_AREAS_REPORT_OPEN) THEN
                     PORT_MARKET_AREAS_REPORT_OPEN = .TRUE.
                     FILE_NAME = 'PMB'//TRIM(GET_SCENAME())//'.CSV'
                     OPEN(4446,FILE=FILE_NAME)
                     WRITE(4446,4445)
     +                  'ENDPOINT,'//
     +                  'MARKET_AREA_ID,'//
     +                  'MARKET_AREA_NAME,'//
     +                  'MARKET_AREA_ABBREV,'//
     +                  'TRANS_GROUP_ID,'//
     +                  'TRANS_GROUP_NAME,'//
     +                  'LOAD_SHAPE_YEAR,'
!
                     SCENARIO_NUMBER =
     +                          GET_SCENARIO_LOAD_SHAPE(INT2(1),INT2(1))
!
                  ENDIF
                  PW_REC = ' '
!                  WRITE(PW_REC,4447)
                  TG_POSITION = MAX(1,
     +                   GET_TRANS_GROUP_POSITION(TRANSACTION_GROUP_ID))
                  WRITE(4446,4447)
     +               PRT_ENDPOINT(),',',
     +               FLOAT(LOCAL_INDEX),',',
     +               QUOTE,
     +                  PORTFOLIO_MARKET_AREA_NAME(CURRENT_RECORD),
     +                     QUOTE,',',
     +               QUOTE,
     +                  PORTFOLIO_MARKET_AREA_ABBREV(CURRENT_RECORD),
     +                     QUOTE,',',
     +               FLOAT(TRANSACTION_GROUP_ID),',',
     +               QUOTE,
     +                  GET_GROUP_NAME(TG_POSITION),
     +                     QUOTE,',',
     +               SCENARIO_NUMBER,','
!

               ENDIF
            ENDIF
!
         ENDDO
         IF(PORT_MARKET_AREAS_REPORT) THEN
            CLOSE(4446)
         ENDIF
!
         CALL CLOSE_MK_FILE
         SAVE_MK_FILE_EXISTS = .TRUE.
         READ_MARKET_GROUPS_DATA = .TRUE.
      RETURN
4445  FORMAT(1X,A)
4447  FORMAT (           1X,F5.0,   A,
     +                   1X,F5.0,   A,
     +                   1X,A,A,A,A,
     +                   1X,A,A,A,A,
     +                   1X,F5.0,   A,
     +                   1X,A,A,A,A,
     +                   1X,F6.0,   A)
!***********************************************************************
      ENTRY GET_PORTFOLIO_MARKET_AREA_NAME(R_PORTFOLIO_MARKET_AREA_NAME,
     +                                     R_MARKET_AREA_INDEX)
!***********************************************************************
         IF(SAVE_MK_FILE_EXISTS) THEN
!            LOCAL_NAME = R_BASECASE_MARKET_AREA_ID
            LOCAL_INDEX = INT(R_MARKET_AREA_INDEX)
            IF(LOCAL_INDEX >= 0 .AND. LOCAL_INDEX < 600) THEN
               GET_PORTFOLIO_MARKET_AREA_NAME = .TRUE.
! NOTE: DOUBLE LOCAL INDEX
               LOCAL_INDEX = MARKET_AREA_POSITION(LOCAL_INDEX)
               R_PORTFOLIO_MARKET_AREA_NAME =
     +                       PORTFOLIO_MARKET_AREA_NAME(LOCAL_INDEX)
            ELSE
               GET_PORTFOLIO_MARKET_AREA_NAME = .FALSE.
               R_PORTFOLIO_MARKET_AREA_NAME =  'PORTF NAME NOT FOUND'
            ENDIF
         ELSE
            GET_PORTFOLIO_MARKET_AREA_NAME = .FALSE.
            R_PORTFOLIO_MARKET_AREA_NAME =  'PORTF NAME NOT FOUND'
         ENDIF
      RETURN
!***********************************************************************
      ENTRY GET_PORT_MARKET_AREA_ABBREV(
     +                              R_PORTFOLIO_MARKET_AREA_ABBREV,
     +                              R_MARKET_AREA_INDEX)
!***********************************************************************
         IF(SAVE_MK_FILE_EXISTS) THEN
            LOCAL_INDEX = INT(R_MARKET_AREA_INDEX)
            IF(LOCAL_INDEX >= 0 .AND. LOCAL_INDEX < 600) THEN
               GET_PORT_MARKET_AREA_ABBREV = .TRUE.
! NOTE: DOUBLE LOCAL INDEX
               LOCAL_INDEX = MARKET_AREA_POSITION(LOCAL_INDEX)
               R_PORTFOLIO_MARKET_AREA_ABBREV =
     +                       PORTFOLIO_MARKET_AREA_ABBREV(LOCAL_INDEX)
            ELSE
               GET_PORT_MARKET_AREA_ABBREV = .FALSE.
               R_PORTFOLIO_MARKET_AREA_ABBREV =  'NONE  '
            ENDIF
         ELSE
            GET_PORT_MARKET_AREA_ABBREV = .FALSE.
            R_PORTFOLIO_MARKET_AREA_ABBREV =  'NONE  '
         ENDIF
      RETURN
!***********************************************************************
      ENTRY IN_ACTIVE_MARKET_AREA(R_BASECASE_MARKET_AREA_ID)
!***********************************************************************
         IF(SAVE_MK_FILE_EXISTS) THEN
            LOCAL_NAME = R_BASECASE_MARKET_AREA_ID
            LOCAL_INDEX = MARKET_AREA_LOOKUP(LOCAL_NAME)
            IF(LOCAL_INDEX >= 0 .AND. LOCAL_INDEX < 600) THEN
               IN_ACTIVE_MARKET_AREA =
     +                                 USE_MARKET_AREA(LOCAL_INDEX) == 1
            ELSE
               IN_ACTIVE_MARKET_AREA = .TRUE.
            ENDIF
         ELSE
            IN_ACTIVE_MARKET_AREA = .TRUE.
         ENDIF
      RETURN
!***********************************************************************
      ENTRY IN_ACTIVE_THERMAL_MARKET_AREA(R_BASECASE_MARKET_AREA_ID)
!***********************************************************************
         IF(SAVE_MK_FILE_EXISTS) THEN
            LOCAL_NAME = R_BASECASE_MARKET_AREA_ID
            LOCAL_INDEX = MARKET_AREA_LOOKUP(LOCAL_NAME)
            IF(LOCAL_INDEX >= 0 .AND. LOCAL_INDEX < 600) THEN
               IN_ACTIVE_THERMAL_MARKET_AREA =
     +                        USE_MARKET_AREA(LOCAL_INDEX) == 1 .AND.
     +                  MARKET_THERMAL_ACTIVE(LOCAL_INDEX) == 'T'
            ELSE
               IN_ACTIVE_THERMAL_MARKET_AREA = .TRUE.
            ENDIF
         ELSE
            IN_ACTIVE_THERMAL_MARKET_AREA = .TRUE.
         ENDIF
      RETURN
!***********************************************************************
      ENTRY IN_ACTIVE_HYDRO_MARKET_AREA(R_BASECASE_MARKET_AREA_ID)
!***********************************************************************
         IF(SAVE_MK_FILE_EXISTS) THEN
            LOCAL_NAME = R_BASECASE_MARKET_AREA_ID
            LOCAL_INDEX = MARKET_AREA_LOOKUP(LOCAL_NAME)
            IF(LOCAL_INDEX >= 0 .AND. LOCAL_INDEX < 600) THEN
               IN_ACTIVE_HYDRO_MARKET_AREA =
     +                        USE_MARKET_AREA(LOCAL_INDEX) == 1 .AND.
     +                    MARKET_HYDRO_ACTIVE(LOCAL_INDEX) == 'T'
            ELSE
               IN_ACTIVE_HYDRO_MARKET_AREA = .TRUE.
            ENDIF
         ELSE
            IN_ACTIVE_HYDRO_MARKET_AREA = .TRUE.
         ENDIF
      RETURN
!***********************************************************************
      ENTRY IN_ACTIVE_LOADS_MARKET_AREA(R_BASECASE_MARKET_AREA_ID)
!***********************************************************************
         IF(SAVE_MK_FILE_EXISTS) THEN
            LOCAL_NAME = R_BASECASE_MARKET_AREA_ID
            LOCAL_INDEX = MARKET_AREA_LOOKUP(LOCAL_NAME)
            IF(LOCAL_INDEX >= 0 .AND. LOCAL_INDEX < 600) THEN
               IN_ACTIVE_LOADS_MARKET_AREA =
     +                        USE_MARKET_AREA(LOCAL_INDEX) == 1 .AND.
     +                    MARKET_LOADS_ACTIVE(LOCAL_INDEX) == 'T'
            ELSE
               IN_ACTIVE_LOADS_MARKET_AREA = .TRUE.
            ENDIF
         ELSE
            IN_ACTIVE_LOADS_MARKET_AREA = .TRUE.
         ENDIF
      RETURN
!***********************************************************************
      ENTRY IN_ACTIVE_CONTR_MARKET_AREA(R_BASECASE_MARKET_AREA_ID)
!***********************************************************************
         IF(SAVE_MK_FILE_EXISTS) THEN
            LOCAL_NAME = R_BASECASE_MARKET_AREA_ID
            LOCAL_INDEX = MARKET_AREA_LOOKUP(LOCAL_NAME)
            IF(LOCAL_INDEX >= 0 .AND. LOCAL_INDEX < 600) THEN
               IN_ACTIVE_CONTR_MARKET_AREA =
     +                        USE_MARKET_AREA(LOCAL_INDEX) == 1 .AND.
     +                    MARKET_CONTRACTS_ACTIVE(LOCAL_INDEX) == 'T'
            ELSE
               IN_ACTIVE_CONTR_MARKET_AREA = .TRUE.
            ENDIF
         ELSE
            IN_ACTIVE_CONTR_MARKET_AREA = .TRUE.
         ENDIF
      RETURN
!***********************************************************************
      ENTRY REPORT_MARKET_AREA(R_BASECASE_MARKET_AREA_ID)
!***********************************************************************
         IF(SAVE_MK_FILE_EXISTS) THEN
            LOCAL_NAME = R_BASECASE_MARKET_AREA_ID
            LOCAL_INDEX = MARKET_AREA_LOOKUP(LOCAL_NAME)
            IF(LOCAL_INDEX >= 0 .AND. LOCAL_INDEX < 600) THEN
               REPORT_MARKET_AREA =
     +                  USE_MARKET_AREA(LOCAL_INDEX) == 1 .AND.
     +                             REPORT_THIS_GROUP(LOCAL_INDEX) /= 'F'
            ELSE
               REPORT_MARKET_AREA = .TRUE.
            ENDIF
         ELSE
            REPORT_MARKET_AREA = .TRUE.
         ENDIF
      RETURN
      END
!***********************************************************************
!
      FUNCTION SUM_24_I4(R_DAILY_LOADS_I4)
!
!***********************************************************************
      REAL (kind=4) ::  SUM_24_I4
      INTEGER (kind=4) ::  R_DAILY_LOADS_I4(24)
         SUM_24_I4 =
     +                        FLOAT(
     +                               R_DAILY_LOADS_I4(1) +
     +                               R_DAILY_LOADS_I4(2) +
     +                               R_DAILY_LOADS_I4(3) +
     +                               R_DAILY_LOADS_I4(4) +
     +                               R_DAILY_LOADS_I4(5) +
     +                               R_DAILY_LOADS_I4(6) +
     +                               R_DAILY_LOADS_I4(7) +
     +                               R_DAILY_LOADS_I4(8) +
     +                               R_DAILY_LOADS_I4(9) +
     +                               R_DAILY_LOADS_I4(10) +
     +                               R_DAILY_LOADS_I4(11) +
     +                               R_DAILY_LOADS_I4(12) +
     +                               R_DAILY_LOADS_I4(13) +
     +                               R_DAILY_LOADS_I4(14) +
     +                               R_DAILY_LOADS_I4(15) +
     +                               R_DAILY_LOADS_I4(16) +
     +                               R_DAILY_LOADS_I4(17) +
     +                               R_DAILY_LOADS_I4(18) +
     +                               R_DAILY_LOADS_I4(19) +
     +                               R_DAILY_LOADS_I4(20) +
     +                               R_DAILY_LOADS_I4(21) +
     +                               R_DAILY_LOADS_I4(22) +
     +                               R_DAILY_LOADS_I4(23) +
     +                               R_DAILY_LOADS_I4(24))
      RETURN
      END
!***********************************************************************
!
      FUNCTION PEAK_24_I4(R_DAILY_LOADS_I4)
!
!***********************************************************************
      REAL (kind=4) ::  PEAK_24_I4
      INTEGER (kind=4) ::  R_DAILY_LOADS_I4(24)
         PEAK_24_I4 =  MAX(
     +                               R_DAILY_LOADS_I4(1) ,
     +                               R_DAILY_LOADS_I4(2) ,
     +                               R_DAILY_LOADS_I4(3) ,
     +                               R_DAILY_LOADS_I4(4) ,
     +                               R_DAILY_LOADS_I4(5) ,
     +                               R_DAILY_LOADS_I4(6) ,
     +                               R_DAILY_LOADS_I4(7) ,
     +                               R_DAILY_LOADS_I4(8) ,
     +                               R_DAILY_LOADS_I4(9) ,
     +                               R_DAILY_LOADS_I4(10) ,
     +                               R_DAILY_LOADS_I4(11) ,
     +                               R_DAILY_LOADS_I4(12) ,
     +                               R_DAILY_LOADS_I4(13) ,
     +                               R_DAILY_LOADS_I4(14) ,
     +                               R_DAILY_LOADS_I4(15) ,
     +                               R_DAILY_LOADS_I4(16) ,
     +                               R_DAILY_LOADS_I4(17) ,
     +                               R_DAILY_LOADS_I4(18) ,
     +                               R_DAILY_LOADS_I4(19) ,
     +                               R_DAILY_LOADS_I4(20) ,
     +                               R_DAILY_LOADS_I4(21) ,
     +                               R_DAILY_LOADS_I4(22) ,
     +                               R_DAILY_LOADS_I4(23) ,
     +                               R_DAILY_LOADS_I4(24))
      RETURN
      END
!***********************************************************************
!
      FUNCTION PEAK_24_R4(R_DAILY_LOADS_R4)
!
!***********************************************************************
!
      REAL (kind=4) ::  PEAK_24_R4,R_DAILY_LOADS_R4(24)
!
         PEAK_24_R4 =  MAX(
     +                               R_DAILY_LOADS_R4(1) ,
     +                               R_DAILY_LOADS_R4(2) ,
     +                               R_DAILY_LOADS_R4(3) ,
     +                               R_DAILY_LOADS_R4(4) ,
     +                               R_DAILY_LOADS_R4(5) ,
     +                               R_DAILY_LOADS_R4(6) ,
     +                               R_DAILY_LOADS_R4(7) ,
     +                               R_DAILY_LOADS_R4(8) ,
     +                               R_DAILY_LOADS_R4(9) ,
     +                               R_DAILY_LOADS_R4(10) ,
     +                               R_DAILY_LOADS_R4(11) ,
     +                               R_DAILY_LOADS_R4(12) ,
     +                               R_DAILY_LOADS_R4(13) ,
     +                               R_DAILY_LOADS_R4(14) ,
     +                               R_DAILY_LOADS_R4(15) ,
     +                               R_DAILY_LOADS_R4(16) ,
     +                               R_DAILY_LOADS_R4(17) ,
     +                               R_DAILY_LOADS_R4(18) ,
     +                               R_DAILY_LOADS_R4(19) ,
     +                               R_DAILY_LOADS_R4(20) ,
     +                               R_DAILY_LOADS_R4(21) ,
     +                               R_DAILY_LOADS_R4(22) ,
     +                               R_DAILY_LOADS_R4(23) ,
     +                               R_DAILY_LOADS_R4(24))
      RETURN
      END
!***********************************************************************
!
      FUNCTION BASE_24_I4(R_DAILY_LOADS_I4)
!
!***********************************************************************
      REAL (kind=4) ::  BASE_24_I4
      INTEGER (kind=4) ::  R_DAILY_LOADS_I4(24)
         BASE_24_I4 =  MIN(
     +                               R_DAILY_LOADS_I4(1) ,
     +                               R_DAILY_LOADS_I4(2) ,
     +                               R_DAILY_LOADS_I4(3) ,
     +                               R_DAILY_LOADS_I4(4) ,
     +                               R_DAILY_LOADS_I4(5) ,
     +                               R_DAILY_LOADS_I4(6) ,
     +                               R_DAILY_LOADS_I4(7) ,
     +                               R_DAILY_LOADS_I4(8) ,
     +                               R_DAILY_LOADS_I4(9) ,
     +                               R_DAILY_LOADS_I4(10) ,
     +                               R_DAILY_LOADS_I4(11) ,
     +                               R_DAILY_LOADS_I4(12) ,
     +                               R_DAILY_LOADS_I4(13) ,
     +                               R_DAILY_LOADS_I4(14) ,
     +                               R_DAILY_LOADS_I4(15) ,
     +                               R_DAILY_LOADS_I4(16) ,
     +                               R_DAILY_LOADS_I4(17) ,
     +                               R_DAILY_LOADS_I4(18) ,
     +                               R_DAILY_LOADS_I4(19) ,
     +                               R_DAILY_LOADS_I4(20) ,
     +                               R_DAILY_LOADS_I4(21) ,
     +                               R_DAILY_LOADS_I4(22) ,
     +                               R_DAILY_LOADS_I4(23) ,
     +                               R_DAILY_LOADS_I4(24))
      RETURN
      END
!***********************************************************************
!
      FUNCTION SUM_24_R4(R_DAILY_LOADS_R4)
!
!***********************************************************************
      REAL (kind=4) ::  SUM_24_R4
      REAL (kind=4) ::  R_DAILY_LOADS_R4(24)
         SUM_24_R4 =
     +                               R_DAILY_LOADS_R4(1) +
     +                               R_DAILY_LOADS_R4(2) +
     +                               R_DAILY_LOADS_R4(3) +
     +                               R_DAILY_LOADS_R4(4) +
     +                               R_DAILY_LOADS_R4(5) +
     +                               R_DAILY_LOADS_R4(6) +
     +                               R_DAILY_LOADS_R4(7) +
     +                               R_DAILY_LOADS_R4(8) +
     +                               R_DAILY_LOADS_R4(9) +
     +                               R_DAILY_LOADS_R4(10) +
     +                               R_DAILY_LOADS_R4(11) +
     +                               R_DAILY_LOADS_R4(12) +
     +                               R_DAILY_LOADS_R4(13) +
     +                               R_DAILY_LOADS_R4(14) +
     +                               R_DAILY_LOADS_R4(15) +
     +                               R_DAILY_LOADS_R4(16) +
     +                               R_DAILY_LOADS_R4(17) +
     +                               R_DAILY_LOADS_R4(18) +
     +                               R_DAILY_LOADS_R4(19) +
     +                               R_DAILY_LOADS_R4(20) +
     +                               R_DAILY_LOADS_R4(21) +
     +                               R_DAILY_LOADS_R4(22) +
     +                               R_DAILY_LOADS_R4(23) +
     +                               R_DAILY_LOADS_R4(24)
      RETURN
      END
!***********************************************************************
!
      FUNCTION BASE_24_R4(R_DAILY_LOADS_R4)
!
!***********************************************************************
      REAL (kind=4) ::  BASE_24_R4
      REAL (kind=4) ::  R_DAILY_LOADS_R4(24)
         BASE_24_R4 =  MIN(
     +                               R_DAILY_LOADS_R4(1) ,
     +                               R_DAILY_LOADS_R4(2) ,
     +                               R_DAILY_LOADS_R4(3) ,
     +                               R_DAILY_LOADS_R4(4) ,
     +                               R_DAILY_LOADS_R4(5) ,
     +                               R_DAILY_LOADS_R4(6) ,
     +                               R_DAILY_LOADS_R4(7) ,
     +                               R_DAILY_LOADS_R4(8) ,
     +                               R_DAILY_LOADS_R4(9) ,
     +                               R_DAILY_LOADS_R4(10) ,
     +                               R_DAILY_LOADS_R4(11) ,
     +                               R_DAILY_LOADS_R4(12) ,
     +                               R_DAILY_LOADS_R4(13) ,
     +                               R_DAILY_LOADS_R4(14) ,
     +                               R_DAILY_LOADS_R4(15) ,
     +                               R_DAILY_LOADS_R4(16) ,
     +                               R_DAILY_LOADS_R4(17) ,
     +                               R_DAILY_LOADS_R4(18) ,
     +                               R_DAILY_LOADS_R4(19) ,
     +                               R_DAILY_LOADS_R4(20) ,
     +                               R_DAILY_LOADS_R4(21) ,
     +                               R_DAILY_LOADS_R4(22) ,
     +                               R_DAILY_LOADS_R4(23) ,
     +                               R_DAILY_LOADS_R4(24))
      RETURN
      END
!***********************************************************************
      RECURSIVE FUNCTION GET_SCARCITY_INFO(R_TG,
     +                           R_FIRST_CAP,
     +                           R_FIRST_PERCENT,
     +                           R_SECOND_CAP,
     +                           R_SECOND_PERCENT,
     +                           R_THIRD_CAP,
     +                           R_THIRD_PERCENT,
     +                           R_MONTH,
     +                           R_YEAR,
     +                           R_CAPACITY_ADDER,
     +                           R_ADDITIONAL_VALUE,
     +                           R_ADDITIONAL_PERCENT)
!***********************************************************************
!
      LOGICAL (kind=1) ::  GET_SCARCITY_INFO,STORE_TG_SCARCITY_INFO
      INTEGER (kind=2) ::  R_TG
      INTEGER (kind=2) ::  R_MONTH
      INTEGER (kind=2) ::  R_YEAR
      INTEGER (kind=2) ::  TRANS_GROUPS_RECORDS
      INTEGER (kind=2) ::  I
      INTEGER (kind=2) ::  J
      INTEGER (kind=2) ::  TRANS_GROUP
      INTEGER (kind=2) ::  TEMP_I
      INTEGER (kind=2) ::  GET_TRANS_GROUP_POSITION
      REAL ::  ESCALATED_MONTHLY_VALUE
      REAL ::  R_ADDITIONAL_VALUE(7)
      REAL ::  R_ADDITIONAL_PERCENT(7)
      REAL ::  R_FIRST_CAP
      REAL ::  R_FIRST_PERCENT
      REAL ::  R_SECOND_CAP
      REAL ::  R_SECOND_PERCENT
      REAL ::  R_THIRD_CAP
      REAL ::  R_THIRD_PERCENT
      REAL ::  R_CAPACITY_ADDER
      REAL ::  R_FIRST_CAPACITY_VALUE(0:TRANS_GROUPS_RECORDS)
      REAL ::  R_FIRST_CAPACITY_PERCENT(0:TRANS_GROUPS_RECORDS)
      REAL ::  R_SECOND_CAPACITY_VALUE(0:TRANS_GROUPS_RECORDS)
      REAL ::  R_SECOND_CAPACITY_PERCENT(0:TRANS_GROUPS_RECORDS)
      REAL ::  R_THIRD_CAPACITY_VALUE(0:TRANS_GROUPS_RECORDS)
      REAL ::  R_THIRD_CAPACITY_PERCENT(0:TRANS_GROUPS_RECORDS)
      REAL ::  R_CAPACITY_ADDER_ARRAY(TRANS_GROUPS_RECORDS)
      REAL ::  R_ADDITIONAL_CAPACITY_VALUE(0:TRANS_GROUPS_RECORDS,7)
      REAL ::  R_ADDITIONAL_CAPACITY_PERCENT(0:TRANS_GROUPS_RECORDS,7)
      REAL ::  GLOBAL_SCARCITY
      REAL ::  GET_GLOBAL_SCARCITY
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
      END
