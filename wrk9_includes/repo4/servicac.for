!     Last change: MSG 1/4/2013 8:20:40 AM
!******************************************************************
!
!                    CONVERT THE SERVICE-TRANSACTIONS FILE
!                             COPYRIGHT (C) 1992
!                        M.S. GERBER & ASSOCIATES, INC.
!                             ALL RIGHTS RESERVED
!
!******************************************************************
      SUBROUTINE TR_OBJECT
      use end_routine, only: end_program, er_message
      use filename_tracker
!******************************************************************
!
      use SpinDriftLib
      use prod_arrays_dimensions
      USE SIZECOM

      INTEGER (kind=2) ::  DELETE,IREC,INUNIT,LRECL/270/
      INTEGER (kind=4) ::  IOS,IOS_BASE
      INTEGER (kind=2) ::  UNIT_NUM/10/
      INTEGER (kind=2) ::  NUM_SERVICE_TRANS/0/,R_NUM_SERVICE_TRANS
      INTEGER (kind=4) ::  R_UNIT_NUM
      CHARACTER (len=5) ::  BASE_FILE_NAME,OVERLAY_FAMILY_NAME,
     +            CAP_ENRG_TRANSACTIONS_FILE
      CHARACTER (len=20) ::  REVENUE_CLASSIFICATION
      CHARACTER (len=20) ::  EXPENSE_CLASSIFICATION
      CHARACTER (len=50) ::  COMMENT
      CHARACTER (len=256) ::  FILE_NAME
      CHARACTER (len=256) ::  BASE_FILE_DIRECTORY
      CHARACTER (len=256) ::  DATA_DRIVE,OUTPUT_DIRECTORY
      LOGICAL (kind=4) ::  FILE_EXISTS/.FALSE./
      LOGICAL (kind=4) ::  R_SERVICE_TRANSACTIONS_ACTIVE
! DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER (len=1024) ::  RECLN
! DECLARATION FOR THE SERVICE TRANSACTIONS FILE
      INTEGER (kind=2) ::  SERVICE_REPORTING_GROUP,
     +          MONTH_SERVICE_AVAILABLE,YEAR_SERVICE_AVAILABLE,
     +          MONTH_SERVICE_ENDS,YEAR_SERVICE_ENDS,UPDATE_MONTH,
     +          RESOURCE_POINTER,CAP_ESCALATION_VECTOR,
     +          ENRG_ESCALATION_VECTOR
      INTEGER (kind=4) ::  SERVICE_ID_NUMBER
      REAL (kind=4) ::  ANNUAL_CAPACITY,CAPACITY_DISTRIBUTION_PATTERN,
     +     CAPACITY_CHARGE,ANNUAL_ENERGY,ENERGY_DISTRIBUTION_PATTERN,
     +     ENERGY_CHARGE
      CHARACTER (len=20) ::  SERVICE_NAME
      CHARACTER (len=20) ::  FILE_TYPE/'Service Transactions'/
      CHARACTER (len=1) ::  TYPE_OF_SERVICE,COST_ASSIGNMENT,
     +            INTRA_COMPANY_TRANSACTION,
     +            SERVICE_ACTIVE,
     +            WVPA_RATE_TRACKER,
     +            WVPA_RES_TRACKER,
     +            WVPA_FUEL_TRACKER,
     +            WVPA_MEM_TRACKER
      INTEGER (kind=2) ::  INTRA_ASSET_CLASS_ID
      INTEGER (kind=2) ::  INTRA_ASSET_CLASS_ALLOC_VECTOR
      CHARACTER (len=20) ::  INTRA_ACCOUNT_CLASSIFICATION
      CHARACTER (len=3) ::  INTRA_EXPENSE_COLLECTION,EXPENSE_COLLECTION
      CHARACTER (len=2) ::  SERVICE_TRANS_OL/'BC'/
      CHARACTER (len=5) ::  LINK_TYPE
      CHARACTER (len=1) ::  ENERGY_TO_USE
! ASSET CLASS VARIABLES
      INTEGER (kind=2) ::  ASSET_CLASS_NUM,ASSET_CLASS_VECTOR
      LOGICAL (kind=1) ::  LAHEY_LF95
      CHARACTER (len=30) ::  SCREEN_OUTPUT
! END OF DATA DECLARATIONS
!******************************************************************
      ENTRY TR_MAKEBIN
!******************************************************************
!
      BASE_FILE_NAME = CAP_ENRG_TRANSACTIONS_FILE()
      DATA_DRIVE = OUTPUT_DIRECTORY()
      FILE_NAME = get_trb_filename(base_file_name)
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      NUM_SERVICE_TRANS = 0
      IF(FILE_EXISTS) THEN
         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//BASE_FILE_NAME
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
            CALL MG_LOCATE_WRITE(16,30,BASE_FILE_NAME,ALL_VERSIONS,0)
         ENDIF
         OPEN(10,FILE=FILE_NAME)
         OPEN(11,FILE=trim(DATA_DRIVE)//"BCTRANS.BIN",ACCESS="DIRECT",
     +                                     STATUS="UNKNOWN",RECL=LRECL)
         IREC = 0
         READ(10,*) DELETE
!
         DO
! ASSET CLASS INITIALIZE
            ASSET_CLASS_NUM = 0
            ASSET_CLASS_VECTOR = 0
            REVENUE_CLASSIFICATION = ' '
            INTRA_COMPANY_TRANSACTION = 'N'
            INTRA_ASSET_CLASS_ID = 0
            INTRA_ASSET_CLASS_ALLOC_VECTOR = 0
            INTRA_ACCOUNT_CLASSIFICATION = ' '
            INTRA_EXPENSE_COLLECTION = ' '
            EXPENSE_CLASSIFICATION = 'Service'
            TYPE_OF_SERVICE = 'T' ! ADDED 9/4/97. GAT.
            UPDATE_MONTH = 1
            MONTH_SERVICE_AVAILABLE = 1
            YEAR_SERVICE_AVAILABLE = 1990
            MONTH_SERVICE_ENDS = 12
            YEAR_SERVICE_ENDS = 2100
            SERVICE_ACTIVE = 'T'
            ENERGY_TO_USE = 'B'
            WVPA_RATE_TRACKER = 'N'
            WVPA_RES_TRACKER = 'N'
            WVPA_FUEL_TRACKER = 'N'
            WVPA_MEM_TRACKER = 'M'
!
            DO
               READ(10,1000,IOSTAT=IOS) RECLN
               IF(IOS .NE. 0) EXIT
               IF(RECLN(1:1) == '7') EXIT
               RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               READ(RECLN,*,ERR=200) DELETE,SERVICE_NAME,
     +                            SERVICE_ID_NUMBER,
     +                            TYPE_OF_SERVICE,COST_ASSIGNMENT,
     +                            EXPENSE_COLLECTION,
     +                            SERVICE_REPORTING_GROUP,
     +                            MONTH_SERVICE_AVAILABLE,
     +                            YEAR_SERVICE_AVAILABLE,
     +                            MONTH_SERVICE_ENDS,YEAR_SERVICE_ENDS,
     +                            UPDATE_MONTH,
     +                            LINK_TYPE,
     +                            RESOURCE_POINTER,
     +                            ANNUAL_CAPACITY,
     +                            CAPACITY_DISTRIBUTION_PATTERN,
     +                            CAPACITY_CHARGE,
     +                            CAP_ESCALATION_VECTOR,
     +                            ANNUAL_ENERGY,
     +                            ENERGY_DISTRIBUTION_PATTERN,
     +                            ENERGY_CHARGE,
     +                            ENRG_ESCALATION_VECTOR,
     +                            COMMENT,
     +                            ASSET_CLASS_NUM,
     +                            ASSET_CLASS_VECTOR,
     +                            REVENUE_CLASSIFICATION,
     +                            INTRA_COMPANY_TRANSACTION, ! 26
     +                            INTRA_ASSET_CLASS_ID,
     +                            INTRA_ASSET_CLASS_ALLOC_VECTOR,
     +                            INTRA_ACCOUNT_CLASSIFICATION, ! 29
     +                            INTRA_EXPENSE_COLLECTION,
     +                            EXPENSE_CLASSIFICATION, !31
     +                            SERVICE_ACTIVE,
     +                            ENERGY_TO_USE,
     +                            WVPA_RATE_TRACKER,
     +                            WVPA_RES_TRACKER,
     +                            WVPA_FUEL_TRACKER,
     +                            WVPA_MEM_TRACKER
               IREC = IREC + 1
              WRITE(11,REC=IREC) DELETE,SERVICE_NAME,SERVICE_ID_NUMBER,
     +                         TYPE_OF_SERVICE,COST_ASSIGNMENT,
     +                         EXPENSE_COLLECTION,
     +                         SERVICE_REPORTING_GROUP,
     +                         MONTH_SERVICE_AVAILABLE,
     +                         YEAR_SERVICE_AVAILABLE,
     +                         MONTH_SERVICE_ENDS,YEAR_SERVICE_ENDS,
     +                         UPDATE_MONTH,
     +                         LINK_TYPE,
     +                         RESOURCE_POINTER,
     +                         ANNUAL_CAPACITY,
     +                         CAPACITY_DISTRIBUTION_PATTERN,
     +                         CAPACITY_CHARGE,
     +                         CAP_ESCALATION_VECTOR,
     +                         ANNUAL_ENERGY,
     +                         ENERGY_DISTRIBUTION_PATTERN,
     +                         ENERGY_CHARGE,
     +                         ENRG_ESCALATION_VECTOR,
     +                         ASSET_CLASS_NUM,
     +                         ASSET_CLASS_VECTOR,
     +                         REVENUE_CLASSIFICATION,
     +                         INTRA_COMPANY_TRANSACTION,
     +                         INTRA_ASSET_CLASS_ID,
     +                         INTRA_ASSET_CLASS_ALLOC_VECTOR,
     +                         INTRA_ACCOUNT_CLASSIFICATION, ! 29
     +                         INTRA_EXPENSE_COLLECTION, !30
     +                         EXPENSE_CLASSIFICATION, !31
     +                         SERVICE_ACTIVE,
     +                         ENERGY_TO_USE,
     +                         WVPA_RATE_TRACKER,
     +                         WVPA_RES_TRACKER,
     +                         WVPA_FUEL_TRACKER,
     +                         WVPA_MEM_TRACKER
            ENDDO
            IF(IOS /= 0) EXIT
         ENDDO
         CLOSE(10)
         CLOSE(11)
         NUM_SERVICE_TRANS = IREC
      ELSE IF(INDEX(BASE_FILE_NAME,'NONE') == 0) THEN
         CALL STOP_NOFILE(FILE_TYPE,FILE_NAME)
      ENDIF
      RETURN
!
! OVERLAY THE SERVICE-TRANSACTIONS FILE
!******************************************************************
      ENTRY TR_MAKEOVL(OVERLAY_FAMILY_NAME)
!******************************************************************
!
      IF(LAHEY_LF95()) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSE
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         CALL LOCATE(10,51)
      ENDIF
      DATA_DRIVE = OUTPUT_DIRECTORY()
      FILE_NAME = trim(DATA_DRIVE)//"TRO"//
     +                               trim(OVERLAY_FAMILY_NAME)//".DAT"
      OPEN(10,FILE=FILE_NAME)
      READ(10,*) DELETE
      INUNIT = 12
      IF(SERVICE_TRANS_OL == 'BC') THEN
         OPEN(11,FILE=trim(DATA_DRIVE)//"BCTRANS.BIN",
     +                                      ACCESS="DIRECT",RECL=LRECL)
         INUNIT = 11
      ENDIF
      OPEN(12,FILE=trim(DATA_DRIVE)//"OLTRANS.BIN",ACCESS="DIRECT",
     +                                     STATUS="UNKNOWN",RECL=LRECL)
      IREC = 0
      DO
         DO
            READ(10,1000,IOSTAT=IOS) RECLN
            IF(RECLN(1:1) == '7') EXIT
            IREC = IREC + 1
            READ(INUNIT,REC=IREC,IOSTAT=IOS_BASE) DELETE,SERVICE_NAME,
     +                         SERVICE_ID_NUMBER,
     +                         TYPE_OF_SERVICE,COST_ASSIGNMENT,
     +                         EXPENSE_COLLECTION,
     +                         SERVICE_REPORTING_GROUP,
     +                         MONTH_SERVICE_AVAILABLE,
     +                         YEAR_SERVICE_AVAILABLE,
     +                         MONTH_SERVICE_ENDS,YEAR_SERVICE_ENDS,
     +                         UPDATE_MONTH,
     +                         LINK_TYPE,
     +                         RESOURCE_POINTER,
     +                         ANNUAL_CAPACITY,
     +                         CAPACITY_DISTRIBUTION_PATTERN,
     +                         CAPACITY_CHARGE,
     +                         CAP_ESCALATION_VECTOR,
     +                         ANNUAL_ENERGY,
     +                         ENERGY_DISTRIBUTION_PATTERN,
     +                         ENERGY_CHARGE,
     +                         ENRG_ESCALATION_VECTOR,
     +                         ASSET_CLASS_NUM,
     +                         ASSET_CLASS_VECTOR,
     +                         REVENUE_CLASSIFICATION,
     +                         INTRA_COMPANY_TRANSACTION,
     +                         INTRA_ASSET_CLASS_ID,
     +                         INTRA_ASSET_CLASS_ALLOC_VECTOR,
     +                         INTRA_ACCOUNT_CLASSIFICATION, ! 29
     +                         INTRA_EXPENSE_COLLECTION, !30
     +                         EXPENSE_CLASSIFICATION, !31
     +                         SERVICE_ACTIVE,
     +                         ENERGY_TO_USE,
     +                         WVPA_RATE_TRACKER,
     +                         WVPA_RES_TRACKER,
     +                         WVPA_FUEL_TRACKER,
     +                         WVPA_MEM_TRACKER
            IF(IOS_BASE /= 0) EXIT
            IF(IOS == 0) THEN
               RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               READ(RECLN,*,ERR=200) DELETE,SERVICE_NAME,
     +                            SERVICE_ID_NUMBER,
     +                            TYPE_OF_SERVICE,COST_ASSIGNMENT,
     +                            EXPENSE_COLLECTION,
     +                            SERVICE_REPORTING_GROUP,
     +                            MONTH_SERVICE_AVAILABLE,
     +                            YEAR_SERVICE_AVAILABLE,
     +                            MONTH_SERVICE_ENDS,
     +                            YEAR_SERVICE_ENDS,
     +                            UPDATE_MONTH,
     +                            LINK_TYPE,
     +                            RESOURCE_POINTER,
     +                            ANNUAL_CAPACITY,
     +                            CAPACITY_DISTRIBUTION_PATTERN,
     +                            CAPACITY_CHARGE,
     +                            CAP_ESCALATION_VECTOR,
     +                            ANNUAL_ENERGY,
     +                            ENERGY_DISTRIBUTION_PATTERN,
     +                            ENERGY_CHARGE,
     +                            ENRG_ESCALATION_VECTOR,
     +                            COMMENT,
     +                            ASSET_CLASS_NUM,
     +                            ASSET_CLASS_VECTOR,
     +                            REVENUE_CLASSIFICATION,
     +                            INTRA_COMPANY_TRANSACTION,
     +                            INTRA_ASSET_CLASS_ID,
     +                            INTRA_ASSET_CLASS_ALLOC_VECTOR,
     +                            INTRA_ACCOUNT_CLASSIFICATION, ! 29
     +                            INTRA_EXPENSE_COLLECTION, !30
     +                            EXPENSE_CLASSIFICATION, !31
     +                            SERVICE_ACTIVE,
     +                            ENERGY_TO_USE,
     +                            WVPA_RATE_TRACKER,
     +                            WVPA_RES_TRACKER,
     +                            WVPA_FUEL_TRACKER,
     +                            WVPA_MEM_TRACKER
            ENDIF
            WRITE(12,REC=IREC) DELETE,SERVICE_NAME,SERVICE_ID_NUMBER,
     +                      TYPE_OF_SERVICE,COST_ASSIGNMENT,
     +                      EXPENSE_COLLECTION,
     +                      SERVICE_REPORTING_GROUP,
     +                      MONTH_SERVICE_AVAILABLE,
     +                      YEAR_SERVICE_AVAILABLE,
     +                      MONTH_SERVICE_ENDS,YEAR_SERVICE_ENDS,
     +                      UPDATE_MONTH,
     +                      LINK_TYPE,
     +                      RESOURCE_POINTER,
     +                      ANNUAL_CAPACITY,
     +                      CAPACITY_DISTRIBUTION_PATTERN,
     +                      CAPACITY_CHARGE,
     +                      CAP_ESCALATION_VECTOR,
     +                      ANNUAL_ENERGY,
     +                      ENERGY_DISTRIBUTION_PATTERN,
     +                      ENERGY_CHARGE,
     +                      ENRG_ESCALATION_VECTOR,
     +                      ASSET_CLASS_NUM,
     +                      ASSET_CLASS_VECTOR,
     +                      REVENUE_CLASSIFICATION,
     +                      INTRA_COMPANY_TRANSACTION,
     +                      INTRA_ASSET_CLASS_ID,
     +                      INTRA_ASSET_CLASS_ALLOC_VECTOR,
     +                      INTRA_ACCOUNT_CLASSIFICATION, ! 29
     +                      INTRA_EXPENSE_COLLECTION, !30
     +                      EXPENSE_CLASSIFICATION, !31
     +                      SERVICE_ACTIVE,
     +                      ENERGY_TO_USE,
     +                      WVPA_RATE_TRACKER,
     +                      WVPA_RES_TRACKER,
     +                      WVPA_FUEL_TRACKER,
     +                      WVPA_MEM_TRACKER
         ENDDO
         IF(IOS_BASE /= 0) EXIT
      ENDDO
      CLOSE(10)
      CLOSE(12)
      IF(SERVICE_TRANS_OL == 'BC') CLOSE(11)
      SERVICE_TRANS_OL = 'OL'
      RETURN
!
!  200 CALL LOCATE(20,0)
!      WRITE(6,1010) trim(RECLN),IREC
  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from Servicac SIID265'
      call end_program(er_message)
!
!******************************************************************
      ENTRY RESET_SERVICE_TRANS_OL
!******************************************************************
         SERVICE_TRANS_OL = 'BC'
      RETURN
!
!******************************************************************
      ENTRY OPEN_SERVICE_TRANS_FILE(R_UNIT_NUM,
     +                              R_SERVICE_TRANSACTIONS_ACTIVE,
     +                              R_NUM_SERVICE_TRANS)
!******************************************************************
         UNIT_NUM = R_UNIT_NUM
         R_NUM_SERVICE_TRANS = NUM_SERVICE_TRANS
         IF(NUM_SERVICE_TRANS > 0) THEN
            R_SERVICE_TRANSACTIONS_ACTIVE = FILE_EXISTS
            IF(FILE_EXISTS) OPEN(UNIT_NUM,
     +                           FILE=trim(OUTPUT_DIRECTORY())//
     +                                   SERVICE_TRANS_OL//"TRANS.BIN",
     +                                      ACCESS="DIRECT",RECL=LRECL)
         ELSE
            R_SERVICE_TRANSACTIONS_ACTIVE = .FALSE.
         ENDIF
      RETURN
!
!******************************************************************
      ENTRY CLOSE_SERVICE_TRANS_FILE
!******************************************************************
         IF(FILE_EXISTS) CLOSE(UNIT_NUM)
      RETURN
!
!******************************************************************
      ENTRY GET_NUMBER_OF_SERVICES (R_NUM_SERVICE_TRANS)
!******************************************************************
         R_NUM_SERVICE_TRANS = NUM_SERVICE_TRANS
      RETURN
!
 1000 FORMAT(A)
 1010 FORMAT('&',A,I4)
      END
!**********************************************************************
!
!                         SERVICE TRANSACTIONS MODULE
!                              COPYRIGHT (C) 1992
!                        M.S. GERBER & ASSOCIATES, INC.
!                              ALL RIGHTS RESERVED
!
!**********************************************************************
!
      RECURSIVE SUBROUTINE READ_SERVICE_TRANS(NUNITS,
     +                                     SERVICE_TRANSACTIONS_ACTIVE)
!
      USE IREC_ENDPOINT_CONTROL

      use grx_planning_routines
      use SpinDriftLib
      use prod_arrays_dimensions
      use eco
      use cls_load
      USE SIZECOM
      use globecom
      use servcom
      SAVE

      INCLUDE 'NAMESCOM.MON'
      INCLUDE 'MTHNMCOM.MON'
!
      LOGICAL (KIND=1) :: INTO_EXTENSION_PERIOD
      INTEGER (kind=4) ::  VALUES_2_ZERO
      INTEGER (kind=2) ::  EXP_TYPE,REV_TYPE,INCOME_STATEMENT_POSITION,
     +            R_TRANS_NO
      REAL (kind=4) ::  TRANSACTION_AMOUNT
      CHARACTER (len=24) ::  SERVICE_TITLE(MAX_SERVICE_ITEMS)
      REAL (kind=4) ::  SUM_OF_SERVICE(0:MAX_SERVICE_GROUPS)
      REAL (kind=4) ::  TOTAL,
     +       RPT_ENERGY_CHARGE,
     +       RPT_CAPACITY_CHARGE,
     +       R_RES_TRACKER,
     +       R_FUEL_TRACKER,
     +       R_RATE_TRACKER,
     +       R_MEM_TRACKER
      LOGICAL (kind=1) ::    KEPCO
      LOGICAL (kind=1) ::    WABASH_VALLEY_ACTIVE/.FALSE./
      LOGICAL (kind=1) ::    WRITE_IT_MONTHLY,
     +            FIRST_EL_SERVICE_WRITE/.TRUE./,
     +            FIRST_CL_SERVICE_WRITE/.TRUE./,
     +            FIRST_CT_SERVICE_WRITE/.TRUE./,
     +            FIRST_OT_SERVICE_WRITE/.TRUE./,
     +            WVPA
      LOGICAL (kind=4) ::  SERVICE_TRANSACTIONS_ACTIVE
      INTEGER (kind=2) ::  GROUPS
!      INTEGER*2 EL_RESOURCE_ID
      INTEGER ::  EL_RESOURCE_ID ! ALTERED 07/14/03.
      REAL (kind=4) ::  SEASON_SYSTEM_PEAK,SEASON_SYSTEM_ENERGY
      REAL (kind=4) ::  ST_REVENUE,ST_EXPENSE
!
      CHARACTER (len=1) ::  CLASS_CHR,UTILITY_TYPE
      INTEGER (kind=2) ::  DELETE
      INTEGER (kind=2) ::  IREC,SERVICE_TRANS,I,YR,CLASS,DATE1,DATE2,
     +            R_SERVICE_TRANS
      INTEGER (kind=4) ::  IOS
      INTEGER (kind=2) ::  NUM_SERVICE_TRANS
!      REAL*4 ESCALATION_RATES(-1:MAX_ESCALATION_VECTRS)
      REAL (kind=4) ::  get_escalated_value,ESCALATED_MONTHLY_VALUE
      REAL (kind=4) ::  GET_VAR
! 
      INTEGER (kind=2) ::   MONTH_SERVICE_AVAILABLE,
     +           YEAR_SERVICE_AVAILABLE,
     +           MONTH_SERVICE_ENDS,
     +           YEAR_SERVICE_ENDS
      REAL (kind=4) ::  ANNUAL_PEAK
      REAL (kind=4) ::  ANNUAL_COINCIDENT_PEAK(6)
      REAL (kind=4) ::  ANNUAL_NONCOINCIDENT_PEAK(6)
      REAL (kind=4) ::  ANNUAL_ENERGY,ANNUAL_CAPACITY
      REAL (kind=4) ::  CLASS_FORECAST
!
      INTEGER (kind=2) ::  NUNITS,ISEAS
      REAL (kind=4) ::  ENERGY(2*NUNITS),CAPACITY(NUNITS)
      INTEGER (kind=2) ::  L,RESOURCE_ID(NUNITS),
     +          ONLINE(NUNITS),OFLINE(NUNITS)
      REAL (kind=4) ::  SEASONAL_ENERGY_CHARGE,SEASONAL_CAPACITY_CHARGE,
     +     PEAK_RESERVE,ENER_RESERVE,SEASONAL_ENERGY,SEASONAL_CAPACITY,
     +     GET_CUST_GROUP_ENERGY,GET_CUST_GROUP_PEAK
!
! MONTHLY SERVICE REPORT VARIABLES
!
      LOGICAL (kind=1) ::  MONTHLY_SERVICE_REPORT_ACTIVE/.FALSE./,
     +          LOGICAL1_TRUE/.TRUE./,
     +          INCLUDE_FIXED_COSTS_ADJ_CLAUSE,
     +          FIXED_COSTS_IN_ADJ_CLAUSE
!
      CHARACTER (len=1) ::  R_COST_ASSIGN(R_SERVICE_TRANS),
     +            R_TRAN_TYPE(R_SERVICE_TRANS),
     +            R_EXPENSE_ASSIGN(R_SERVICE_TRANS)
      CHARACTER (len=20) ::  R_NAME(R_SERVICE_TRANS)
      CHARACTER (len=1) ::  SERVICE_REPORT
!
! WORKING ALLOCATABLE ARRAYS
!
      REAL (kind=4) ::  CAPACITY_CHARGE(:),
     +     ENERGY_CHARGE(:),
     +     ANNUAL_CAPACITY_CHARGE(:),
     +     ANNUAL_ENERGY_CHARGE(:),
     +     CAPACITY_MULTIPLIER(:),
     +     ENERGY_MULTIPLIER(:)
      ALLOCATABLE :: CAPACITY_CHARGE,
     +     ENERGY_CHARGE,
     +     ANNUAL_CAPACITY_CHARGE,
     +     ANNUAL_ENERGY_CHARGE,
     +     CAPACITY_MULTIPLIER,
     +     ENERGY_MULTIPLIER
!
! SERVICE TRANSACTION VARIABLES
!
      CHARACTER (len=1) ::  TYPE_OF_SERVICE(:)
      CHARACTER (len=1) ::  SERVICE_COST_ASSIGNMENT(:)
      CHARACTER (len=5) ::   SERVICE_LINK_TYPE(:)
      CHARACTER (len=20) ::  SERVICE_NAME(:)
      INTEGER (kind=2) ::  SERVICE_REPORTING_GROUP(:),
     +          SERVICE_AVAILABLE(:),
     +          SERVICE_ENDS(:),
     +          SERVICE_UPDATE_SEASON(:),
     +          SERVICE_RESOURCE_PONTR(:),
     +          SERVICE_CAP_ESCALATION_VECTOR(:),
     +          SERVICE_ENRG_ESCALATION_VECTOR(:),
!     +          WVPA_RATE_TRACKER_INDEX(:),
     +          WVPA_TRACKING_TYPE,
     +          WVPA_RESOURCE_TRACKING_TYPE,
     +          WVPA_FUEL_TRACKING_TYPE,
     +          WVPA_MEM_TRACKING_TYPE
      INTEGER (kind=4) ::  SERVICE_ID_NUMBER(:)
      REAL (kind=4) ::  SERVICE_ANNUAL_CAPACITY(:)
      REAL (kind=4) ::  SERVICE_CAPACITY_PATTERN(:),
     +     SERVICE_CAPACITY_CHARGE(:),SERVICE_ANNUAL_ENERGY(:),
     +     SERVICE_ENERGY_PATTERN(:),SERVICE_ENERGY_CHARGE(:),
     +     WVPA_RATE_TRACKER_INDEX(:),
     +     WVPA_RES_TRACKER_INDEX(:),
     +     WVPA_FUEL_TRACKER_INDEX(:),
     +     WVPA_MEM_TRACKER_INDEX(:)
      REAL (KIND=4), ALLOCATABLE :: SERVICE_FUEL_CHARGE(:),
     +                              SERVICE_FUEL_ESCALATION_VECTOR(:)
      CHARACTER (len=20) ::  SERVICE_REVENUE_CLASSIFICATION(:)
      CHARACTER (len=20) ::  SERVICE_EXPENSE_CLASSIFICATION(:)
      CHARACTER (len=1) ::  SERVICE_INTRA_COMPY_TRANSACTION(:),
     +            SERVICE_ACTIVE,
     +            ENERGY_TO_USE(:),
     +            WVPA_RATE_TRACKER(:),
     +            WVPA_RES_TRACKER,
     +            WVPA_FUEL_TRACKER,
     +            WVPA_MEM_TRACKER
      INTEGER (kind=2) ::  INTRA_ASSET_CLASS_ID(:),
     +          INTRA_ASSET_CLASS_ALLOC_VECTOR(:)
      CHARACTER (len=20) ::  INTRA_ACCOUNT_CLASSIFICATION(:)
      CHARACTER (len=3) ::  INTRA_EXPENSE_COLLECTION(:),
     +            SERVICE_EXPENSE_COLLECTION(:)
      ALLOCATABLE :: SERVICE_NAME,
     +            SERVICE_ID_NUMBER,
     +            TYPE_OF_SERVICE,
     +            SERVICE_COST_ASSIGNMENT,
     +            SERVICE_EXPENSE_COLLECTION,
     +            SERVICE_REPORTING_GROUP,
     +            SERVICE_AVAILABLE,
     +            SERVICE_ENDS,
     +            SERVICE_UPDATE_SEASON,
     +            SERVICE_ANNUAL_CAPACITY,
     +            SERVICE_LINK_TYPE,
     +            SERVICE_RESOURCE_PONTR,
     +            SERVICE_CAPACITY_PATTERN,
     +            SERVICE_CAPACITY_CHARGE,
     +            SERVICE_CAP_ESCALATION_VECTOR,
     +            SERVICE_ANNUAL_ENERGY,
     +            SERVICE_ENERGY_PATTERN,
     +            SERVICE_ENERGY_CHARGE,
     +            SERVICE_ENRG_ESCALATION_VECTOR,
     +            SERVICE_REVENUE_CLASSIFICATION,
     +            SERVICE_INTRA_COMPY_TRANSACTION,
     +            ENERGY_TO_USE,
     +            WVPA_RATE_TRACKER,
     +            WVPA_RATE_TRACKER_INDEX,
     +            WVPA_RES_TRACKER_INDEX,
     +            WVPA_FUEL_TRACKER_INDEX,
     +            WVPA_MEM_TRACKER_INDEX,
     +            INTRA_ASSET_CLASS_ID,
     +            INTRA_ASSET_CLASS_ALLOC_VECTOR,
     +            INTRA_ACCOUNT_CLASSIFICATION,
     +            INTRA_EXPENSE_COLLECTION,
     +            SERVICE_EXPENSE_CLASSIFICATION
!
      INTEGER (kind=2) ::  SEASON_HOURS(12),HOURS_IN_PERIOD
!
! ASSET ALLOCATION STUFF
!
      INTEGER (kind=2) ::  NUMBER_OF_SERVICE_CLASSES,UNIT_NO,
     +           MAX_SERVICE_CLASS_ID_NUM,
     +          SERVICE_ASSET_CLASS_POINTER(:)
      ALLOCATABLE :: SERVICE_ASSET_CLASS_POINTER
!
! ASSET ALLOCATION MANAGER STUFF
!
      REAL (kind=4) ::  VAR_COST,ST_FIXED_COST,TRANS_ENERGY
!
      REAL (kind=4) ::  TRANS_CAPACITY,ST_SO2_EMIS
!
      INTEGER (kind=2) ::  NUMBER_OF_SERVICES
!******************************************************************
      INTEGER (kind=2) ::  CURRENT_YEAR,CURRENT_YEAR_COMPARISON,
     +          PRODUCTION_PERIODS,UNIT,CLASS_NUM
      REAL (kind=4) ::  LAST_SEASON
      REAL (kind=4) ::  ANNUAL_ST_CAPACITY(:),ANNUAL_ST_ENERGY(:),
     +     ANNUAL_ST_VAR_COST(:),ANNUAL_ST_FIXED_COST(:),
     +     ANNUAL_ST_SO2_EMIS(:)
      ALLOCATABLE :: ANNUAL_ST_CAPACITY,ANNUAL_ST_ENERGY,
     +               ANNUAL_ST_VAR_COST,ANNUAL_ST_FIXED_COST,
     +               ANNUAL_ST_SO2_EMIS
      REAL (kind=4) ::  MONTHLY_ST_CAPACITY(:,:),MONTHLY_ST_ENERGY(:,:),
     +     MONTHLY_ST_VAR_COST(:,:),MONTHLY_ST_FIXED_COST(:,:),
     +     MONTHLY_ST_SO2_EMIS(:,:)
      ALLOCATABLE :: MONTHLY_ST_CAPACITY,MONTHLY_ST_ENERGY,
     +               MONTHLY_ST_VAR_COST,MONTHLY_ST_FIXED_COST,
     +               MONTHLY_ST_SO2_EMIS
!
! ASSET ALLOCATION STUFF
!
      CHARACTER (len=1) ::  DUMMY_TYPE
      INTEGER (kind=2) ::  CLASS_POINTER,
     +          ASSET_CLASS,
     +          ASSET_ALLOCATION_VECTOR
      INTEGER (kind=2) ::   J
      REAL (kind=4) ::  ASSET_CLASS_LIST(:),ASSET_ALLOCATION_LIST(:)
      REAL (kind=4) ::  ST_ANN_CLASS_ATL_EXPENSE(:,:),
     +     ST_ANN_CLASS_BTL_EXPENSE(:,:),
     +     ST_ANN_CLASS_ADJ_CLAUSE(:,:),
     +     ST_BTL_LEASE_PAYMENT(:,:),
     +     ST_NF_RATEBASE(:,:),
     +     ST_ANN_CLASS_EXPENSE_CAPACITY(:,:),
     +     ST_ANN_CLASS_EXPENSE_ENERGY(:,:),
     +     ST_ANN_CLASS_REVENUE_ENERGY(:,:),
     +     ST_ANN_CLASS_REVENUE_CAPACITY(:,:),
     +     ST_ANN_CLASS_REVENUE(:,:,:),
     +     ST_ANN_CLASS_EXPENSE(:,:,:)
      REAL (kind=4) ::  ASSET_ALLOCATOR
!     ! NO NEED FOR ALLOCATION
      INTEGER (kind=2) ::  ASSET_CLASS_NUM(:),ASSET_CLASS_VECTOR(:)
      ALLOCATABLE :: ASSET_CLASS_NUM,ASSET_CLASS_VECTOR,
     +               ASSET_CLASS_LIST,
     +               ASSET_ALLOCATION_LIST,
     +               ST_ANN_CLASS_ATL_EXPENSE,
     +               ST_ANN_CLASS_BTL_EXPENSE,
     +               ST_ANN_CLASS_ADJ_CLAUSE,
     +               ST_BTL_LEASE_PAYMENT,
     +               ST_NF_RATEBASE,
     +               ST_ANN_CLASS_EXPENSE_CAPACITY,
     +               ST_ANN_CLASS_EXPENSE_ENERGY,
     +               ST_ANN_CLASS_REVENUE_ENERGY,
     +               ST_ANN_CLASS_REVENUE_CAPACITY,
     +               ST_ANN_CLASS_REVENUE,
     +               ST_ANN_CLASS_EXPENSE
!
! EXPENSE TRANSFER TO ASSET CLASS MODULE
!
      INTEGER (kind=2) ::  R_CLASS
      REAL (kind=4) ::  R_ATL_SERVICE_TRANSACTIONS_EXP,
     +     R_BTL_SERVICE_TRANSACTIONS_EXP,
     +     R_EXPENSE_COLLECTED_ADJ_CLAUSE,
     +     R_BTL_SALES_REVENUE,
     +     R_BASE_REVENUES,
     +     R_SECONDARY_SALES_REVENUE,
     +     R_CAPACITY_REVENUES,
     +     R_ADJ_CLAUSE_REVENUE,
     +     R_OTHER_REVENUE
      INTEGER (kind=2) ::  ATL,BTL
      CHARACTER (len=1) ::  REVENUE
      PARAMETER(ATL=1,BTL=2,REVENUE='R')
      INTEGER (kind=2) ::  CURRENT_MONTH
!
!
      INTEGER (kind=2) ::    TRANSACTION_GROUP,
     +            BEGIN_TEST_DATE,
     +            END_TEST_DATE
      REAL (kind=4) ::  REVENUE_GENERATING_CAPACITY(*),
     +       EXPENSE_GENERATING_CAPACITY(2,*),
     +       CL_POOL_FRAC_OWN(*)
!
      LOGICAL (kind=1) ::  TRANSACTION_FOUND
      INTEGER (kind=2) ::  MO,HYDRO_TRANS_GROUP
      INTEGER (kind=2) ::  R_ISEAS
      LOGICAL (kind=1) ::  SERVICE_TRANS_REPORT_NOT_OPEN/.TRUE./
      INTEGER (kind=2) ::  TRANS_RPT_NO/0/,ANNUAL_SERVICE_TRANS_HEADER
      INTEGER ::  TRANS_RPT_REC
      CHARACTER (len=4) ::  LEFT_JUSTIFY_I2_IN_STR*15
      CHARACTER (len=4) ::  TRANSACTION_DESCRIPTION*25,
     +            COST_ASSIGNMENT_NAME*7
      REAL (kind=4) ::  UNIT_ENERGY_COST,
     +       UNIT_CAPACITY_COST
      INTEGER (kind=2) ::  ALLOCATION_VECTOR
      REAL (kind=4) ::  ALLOCATION_VALUE(AVAIL_DATA_YEARS)
      REAL (kind=4) ::  MONTH_VARS(0:12,*)
      REAL (kind=4) ::  R_FUEXP,R_PREXP,R_OPEXP,
     +       R_MNEXP,R_OTHER1,R_OTHER2,
     +       R_OTHER3,R_NFOWN,R_NFLEASE,
     +       R_ADJ_EXP,
     +       R_NF_RATEBASE,R_DSM_EXPENSE,
     +       R_DSM_REBATE,
     +       R_ADJUSTMENT_CLAUSE_REVENUES,
     +       R_BASE_RATES_REVENUES,
     +       R_SECONDARY_SALES_REVENUES,
     +       R_OTHER_REVENUES,
     +       R_BTL_REVENUES,
     +       R_BTL_EXPENSE,
     +       R_ATL_LEASE_EXP,
     +       R_BTL_LEASE_EXP,
     +       R_SERVICE_TRANSACTIONS,
     +       R_EMISSION_CREDITS,
     +       R_DOE_DISPOSAL,
     +       R_DOE_DECOMMISSIONING,
     +       R_CATAWBA_REVENUES,
     +       R_CATAWBA_EXPENSES
!
      LOGICAL (KIND=1) :: TRANSFER_TRANSACT_ANL_RESULTS
!
      LAST_SEASON = FLOAT(PRODUCTION_PERIODS())
      CALL OPEN_SERVICE_TRANS_FILE(10,SERVICE_TRANSACTIONS_ACTIVE,
     +                             NUM_SERVICE_TRANS)
      IF(.NOT. SERVICE_TRANSACTIONS_ACTIVE) RETURN
!
!
! END OF DATA DECLARATIONS
!
! ASSET CLASS INITIALIZATION. 6/16/95. GAT.
!
      IF(ALLOCATED(SERVICE_ASSET_CLASS_POINTER))
     +       DEALLOCATE(SERVICE_ASSET_CLASS_POINTER,ASSET_CLASS_NUM,
     +                                              ASSET_CLASS_VECTOR)
      ALLOCATE(SERVICE_ASSET_CLASS_POINTER(1024),
     +         ASSET_CLASS_NUM(NUM_SERVICE_TRANS),
     +         ASSET_CLASS_VECTOR(NUM_SERVICE_TRANS))
      ASSET_CLASS_NUM = 0
      ASSET_CLASS_VECTOR = 0
      SERVICE_ASSET_CLASS_POINTER = 0
      NUMBER_OF_SERVICE_CLASSES = 0
      MAX_SERVICE_CLASS_ID_NUM = 0
!
      BEGIN_TEST_DATE = 100*(BASE_YEAR+1 - 1900) + 1
      END_TEST_DATE = 100*(LAST_STUDY_YEAR - 1900) + 12
!
      IF(ALLOCATED(SERVICE_NAME)) CALL CLOSE_SERVICE_TRANSACTIONS
      ALLOCATE(SERVICE_NAME(NUM_SERVICE_TRANS),
     +         SERVICE_ID_NUMBER(NUM_SERVICE_TRANS),
     +         TYPE_OF_SERVICE(NUM_SERVICE_TRANS),
     +         SERVICE_COST_ASSIGNMENT(NUM_SERVICE_TRANS),
     +         SERVICE_EXPENSE_COLLECTION(NUM_SERVICE_TRANS),
     +         SERVICE_REPORTING_GROUP(NUM_SERVICE_TRANS),
     +         SERVICE_AVAILABLE(NUM_SERVICE_TRANS),
     +         SERVICE_ENDS(NUM_SERVICE_TRANS),
     +         SERVICE_UPDATE_SEASON(NUM_SERVICE_TRANS),
     +         SERVICE_LINK_TYPE(NUM_SERVICE_TRANS),
     +         SERVICE_RESOURCE_PONTR(NUM_SERVICE_TRANS),
     +         SERVICE_ANNUAL_CAPACITY(NUM_SERVICE_TRANS),
     +         SERVICE_CAPACITY_PATTERN(NUM_SERVICE_TRANS),
     +         SERVICE_CAPACITY_CHARGE(NUM_SERVICE_TRANS),
     +         SERVICE_CAP_ESCALATION_VECTOR(NUM_SERVICE_TRANS),
     +         SERVICE_ANNUAL_ENERGY(NUM_SERVICE_TRANS),
     +         SERVICE_ENERGY_PATTERN(NUM_SERVICE_TRANS),
     +         SERVICE_ENERGY_CHARGE(NUM_SERVICE_TRANS),
     +         SERVICE_ENRG_ESCALATION_VECTOR(NUM_SERVICE_TRANS),
     +         SERVICE_REVENUE_CLASSIFICATION(NUM_SERVICE_TRANS),
     +         SERVICE_INTRA_COMPY_TRANSACTION(NUM_SERVICE_TRANS),
     +         ENERGY_TO_USE(NUM_SERVICE_TRANS),
     +         WVPA_RATE_TRACKER(NUM_SERVICE_TRANS),
     +         WVPA_RATE_TRACKER_INDEX(NUM_SERVICE_TRANS),
     +         WVPA_RES_TRACKER_INDEX(NUM_SERVICE_TRANS),
     +         WVPA_FUEL_TRACKER_INDEX(NUM_SERVICE_TRANS),
     +         WVPA_MEM_TRACKER_INDEX(NUM_SERVICE_TRANS),
     +         INTRA_ASSET_CLASS_ID(NUM_SERVICE_TRANS),
     +         INTRA_ASSET_CLASS_ALLOC_VECTOR(NUM_SERVICE_TRANS),
     +         INTRA_ACCOUNT_CLASSIFICATION(NUM_SERVICE_TRANS),
     +         INTRA_EXPENSE_COLLECTION(NUM_SERVICE_TRANS),
     +         SERVICE_EXPENSE_CLASSIFICATION(NUM_SERVICE_TRANS),
     +         SERVICE_FUEL_CHARGE(NUM_SERVICE_TRANS),
     +         SERVICE_FUEL_ESCALATION_VECTOR(NUM_SERVICE_TRANS),
     +         STAT=IOS)
!
      WVPA_RATE_TRACKER_INDEX = 0.
!
! 11/20/03. NOTE USING LF95 INITIALIZATION.
!
      WVPA_RES_TRACKER_INDEX = 0.
      WVPA_FUEL_TRACKER_INDEX = 0.
      WVPA_MEM_TRACKER_INDEX = 0.
!
      SERVICE_TRANS = 1
      IREC = 0
      DOWHILE (SERVICE_TRANS <= NUM_SERVICE_TRANS)
         IREC = IREC + 1
         READ(10,REC=IREC,IOSTAT=IOS) DELETE,
     +         SERVICE_NAME(SERVICE_TRANS),
     +         SERVICE_ID_NUMBER(SERVICE_TRANS),
     +         TYPE_OF_SERVICE(SERVICE_TRANS),
     +         SERVICE_COST_ASSIGNMENT(SERVICE_TRANS),
     +         SERVICE_EXPENSE_COLLECTION(SERVICE_TRANS),
     +         SERVICE_REPORTING_GROUP(SERVICE_TRANS),
     +         MONTH_SERVICE_AVAILABLE,
     +         YEAR_SERVICE_AVAILABLE,
     +         MONTH_SERVICE_ENDS,
     +         YEAR_SERVICE_ENDS,
     +         SERVICE_UPDATE_SEASON(SERVICE_TRANS),
     +         SERVICE_LINK_TYPE(SERVICE_TRANS),
     +         SERVICE_RESOURCE_PONTR(SERVICE_TRANS),
     +         SERVICE_ANNUAL_CAPACITY(SERVICE_TRANS),
     +         SERVICE_CAPACITY_PATTERN(SERVICE_TRANS),
     +         SERVICE_CAPACITY_CHARGE(SERVICE_TRANS),
     +         SERVICE_CAP_ESCALATION_VECTOR(SERVICE_TRANS),
     +         SERVICE_ANNUAL_ENERGY(SERVICE_TRANS),
     +         SERVICE_ENERGY_PATTERN(SERVICE_TRANS),
     +         SERVICE_ENERGY_CHARGE(SERVICE_TRANS),
     +         SERVICE_ENRG_ESCALATION_VECTOR(SERVICE_TRANS),
     +         ASSET_CLASS_NUM(SERVICE_TRANS),
     +         ASSET_CLASS_VECTOR(SERVICE_TRANS),
     +         SERVICE_REVENUE_CLASSIFICATION(SERVICE_TRANS),
     +         SERVICE_INTRA_COMPY_TRANSACTION(SERVICE_TRANS),
     +         INTRA_ASSET_CLASS_ID(SERVICE_TRANS),
     +         INTRA_ASSET_CLASS_ALLOC_VECTOR(SERVICE_TRANS),
     +         INTRA_ACCOUNT_CLASSIFICATION(SERVICE_TRANS),
     +         INTRA_EXPENSE_COLLECTION(SERVICE_TRANS),
     +         SERVICE_EXPENSE_CLASSIFICATION(SERVICE_TRANS),
     +         SERVICE_ACTIVE,
     +         ENERGY_TO_USE(SERVICE_TRANS),
     +         WVPA_RATE_TRACKER(SERVICE_TRANS),
     +         WVPA_RES_TRACKER,
     +         WVPA_FUEL_TRACKER,
     +         WVPA_MEM_TRACKER
!     +         SERVICE_FUEL_CHARGE(SERVICE_TRANS),
!     +         SERVICE_FUEL_ESCALATION_VECTOR(SERVICE_TRANS),
         IF(IOS /= 0) EXIT
         IF(DELETE >= 8 .OR. SERVICE_ACTIVE == 'F') CYCLE
!         IF(DELETE >= 8) CYCLE
!
         SERVICE_AVAILABLE(SERVICE_TRANS) = MONTH_SERVICE_AVAILABLE +
     +                            100 * (YEAR_SERVICE_AVAILABLE - 1900)
         SERVICE_ENDS(SERVICE_TRANS) = MONTH_SERVICE_ENDS +
     +                                 100 * (YEAR_SERVICE_ENDS - 1900)
         IF(SERVICE_AVAILABLE(SERVICE_TRANS) > END_TEST_DATE .OR.
     +             SERVICE_ENDS(SERVICE_TRANS) < BEGIN_TEST_DATE) CYCLE
!
!        CLASS_NUM = ASSET_CLASS_NUM(SERVICE_TRANS) + 1
         CLASS_NUM = ASSET_CLASS_NUM(SERVICE_TRANS)
         CALL SET_ASSET_CLASSES(CLASS_NUM,
     +                          NUMBER_OF_SERVICE_CLASSES,
     +                           MAX_SERVICE_CLASS_ID_NUM,
     +                          SERVICE_ASSET_CLASS_POINTER)
         IF(SERVICE_INTRA_COMPY_TRANSACTION(SERVICE_TRANS) == 'Y') THEN

            CLASS_NUM = INTRA_ASSET_CLASS_ID(SERVICE_TRANS)
            CALL SET_ASSET_CLASSES(CLASS_NUM,
     +                             NUMBER_OF_SERVICE_CLASSES,
     +                              MAX_SERVICE_CLASS_ID_NUM,
     +                             SERVICE_ASSET_CLASS_POINTER)
         ENDIF
!
         WVPA_RATE_TRACKER_INDEX(SERVICE_TRANS) =
     +      FLOAT(WVPA_TRACKING_TYPE(WVPA_RATE_TRACKER(SERVICE_TRANS)))
! 11/20/03. FOR REPORTING PURPOSES (NOTE: R*4)
         WVPA_RES_TRACKER_INDEX(SERVICE_TRANS) =
     +             FLOAT(WVPA_RESOURCE_TRACKING_TYPE(WVPA_RES_TRACKER))
         WVPA_FUEL_TRACKER_INDEX(SERVICE_TRANS) =
     +                FLOAT(WVPA_FUEL_TRACKING_TYPE(WVPA_FUEL_TRACKER))
         WVPA_MEM_TRACKER_INDEX(SERVICE_TRANS) =
     +                 FLOAT(WVPA_MEM_TRACKING_TYPE(WVPA_MEM_TRACKER))
!
         IF(SERVICE_ENERGY_CHARGE(SERVICE_TRANS) <= 0.)
     +                SERVICE_ENRG_ESCALATION_VECTOR(SERVICE_TRANS) = 0
         IF(SERVICE_CAPACITY_CHARGE(SERVICE_TRANS) <= 0.)
     +                 SERVICE_CAP_ESCALATION_VECTOR(SERVICE_TRANS) = 0
         SERVICE_TRANS = SERVICE_TRANS + 1
      ENDDO
      CALL CLOSE_SERVICE_TRANS_FILE
      SERVICE_TRANS = MIN(SERVICE_TRANS-1,NUM_SERVICE_TRANS)
      ALLOCATE(CAPACITY_CHARGE(SERVICE_TRANS),
     +         ENERGY_CHARGE(SERVICE_TRANS),
     +         ANNUAL_CAPACITY_CHARGE(SERVICE_TRANS),
     +         ANNUAL_ENERGY_CHARGE(SERVICE_TRANS),
     +         CAPACITY_MULTIPLIER(SERVICE_TRANS),
     +         ENERGY_MULTIPLIER(SERVICE_TRANS),
     +         STAT=IOS)
!
      DO I = 1, 12
         SEASON_HOURS(I) = HOURS_IN_PERIOD(I)
      ENDDO
!
! ESTABLISH THE CURRENT PRICE
!
      DO I = 1, SERVICE_TRANS
         IF(SERVICE_CAPACITY_CHARGE(I) < 0.) THEN
            CAPACITY_CHARGE(I) = GET_VAR(SERVICE_CAPACITY_CHARGE(I),
     +                                   INT2(1),SERVICE_NAME(I))
!
!PROBLEMWITH USING A POINTER FOR THE COST AND HAVE THE UPDATE MONTH NOT
!  BE JANUARY.
!
         ELSE
            CAPACITY_CHARGE(I) = SERVICE_CAPACITY_CHARGE(I)
         ENDIF
         IF(SERVICE_ENERGY_CHARGE(I) < 0.) THEN
           ENERGY_CHARGE(I) = GET_VAR(SERVICE_ENERGY_CHARGE(I),INT2(1),
     +                                 SERVICE_NAME(I))
         ELSE
            ENERGY_CHARGE(I) = SERVICE_ENERGY_CHARGE(I)
         ENDIF
      ENDDO
!
      WABASH_VALLEY_ACTIVE = UTILITY_TYPE() == 'R'
      IF(.NOT. WABASH_VALLEY_ACTIVE) THEN
         FIRST_EL_SERVICE_WRITE = .FALSE.
         FIRST_CL_SERVICE_WRITE = .FALSE.
         FIRST_CT_SERVICE_WRITE = .FALSE.
         FIRST_OT_SERVICE_WRITE = .FALSE. ! OTHER SERVICES
      ENDIF
!
      CALL SET_UP_ST_CLASS_ARRAYS
!
      RETURN
!******************************************************************
      ENTRY INITIALIZE_SERVICE_VARS(YR)
!******************************************************************
         TRANSMISSION_CHARGES = 0.
         WHEELING_CHARGES = 0.
         STAND_BY_TRANS_CHARGES= 0.
         DISPATCHING_CHARGES = 0.
         OTHER_SERVICE_CHARGES = 0.
         SERVICE_REVENUES = 0.
         SERVICE_EXPENSES = 0.
         SERVICE_ADJ_CLAUSE_EXPENSES = 0.
         SERVICE_BASE_RATE_EXPENSES = 0.
         SUM_OF_SERVICE(0) = 0.
         DO CLASS = 1, MAX_SERVICE_GROUPS
            ANNUAL_COINCIDENT_PEAK(CLASS) = 0.
            ANNUAL_NONCOINCIDENT_PEAK(CLASS) = 0.
            SUM_OF_SERVICE(CLASS) = 0.
            DO I = 0, MAX_SERVICE_ITEMS
               SERVICE_GROUP_COSTS(I,CLASS) = 0.
            ENDDO
            DO I = 1, 12
              CLASS_FORECAST = MAX(FORECAST_COINCIDENT_PEAK(1,I,CLASS),
     +                           FORECAST_COINCIDENT_PEAK(2,I,CLASS))
               ANNUAL_COINCIDENT_PEAK(CLASS) = MAX(CLASS_FORECAST *
     +                                        CLASS_COIN_FACTOR(CLASS),
     +                                ANNUAL_NONCOINCIDENT_PEAK(CLASS))
               ANNUAL_NONCOINCIDENT_PEAK(CLASS) = MAX(CLASS_FORECAST,
     +                                ANNUAL_NONCOINCIDENT_PEAK(CLASS))
            ENDDO
         ENDDO
         DO I = 1, SERVICE_TRANS
            ANNUAL_CAPACITY_CHARGE(I) = 0.
            ANNUAL_ENERGY_CHARGE(I) = 0.
         ENDDO
      RETURN ! INITIALIZE_SERVICE_VARS
!
! UPDATE SERVICE CHARGES AND CAPACITY AND ENERGY PATTERN DISTRIBUTIONS
!
!******************************************************************
      ENTRY SERVICE_UPDATE_COSTS(ISEAS,YR)
!******************************************************************
      DO I = 1, SERVICE_TRANS
         IF(SERVICE_CAPACITY_PATTERN(I) < 0.) THEN
            CAPACITY_MULTIPLIER(I)=GET_VAR(SERVICE_CAPACITY_PATTERN(I),
     +                                     ISEAS,SERVICE_NAME(I))
         ELSEIF(SERVICE_CAPACITY_PATTERN(I) > 0.) THEN
            CAPACITY_MULTIPLIER(I) = SERVICE_CAPACITY_PATTERN(I)
         ELSE
            CAPACITY_MULTIPLIER(I) = 1.
         ENDIF
         IF(SERVICE_ENERGY_PATTERN(I) < 0.) THEN
            ENERGY_MULTIPLIER(I) = GET_VAR(SERVICE_ENERGY_PATTERN(I),
     +                                     ISEAS,SERVICE_NAME(I))
         ELSEIF(SERVICE_ENERGY_PATTERN(I) > 0.) THEN
            ENERGY_MULTIPLIER(I) = SERVICE_ENERGY_PATTERN(I)
         ELSE
            ENERGY_MULTIPLIER(I) = 1.
         ENDIF
!
! 10/21/98. GAT. RE-WROTE FOR WVPA
!
         IF(ISEAS == SERVICE_UPDATE_SEASON(I) .OR.
     +                                      YR > AVAIL_DATA_YEARS) THEN
            IF(SERVICE_CAPACITY_CHARGE(I) < 0.) THEN
               CAPACITY_CHARGE(I) =
     +                           GET_VAR(SERVICE_CAPACITY_CHARGE(I),YR,
     +                                                 SERVICE_NAME(I))
            ELSE
               CAPACITY_CHARGE(I) = SERVICE_CAPACITY_CHARGE(I)
            ENDIF
            IF(SERVICE_ENERGY_CHARGE(I) < 0.) THEN
               ENERGY_CHARGE(I) = GET_VAR(SERVICE_ENERGY_CHARGE(I),YR,
     +                                                 SERVICE_NAME(I))
            ELSE
               ENERGY_CHARGE(I) = SERVICE_ENERGY_CHARGE(I)
            ENDIF
         ENDIF
         CAPACITY_CHARGE(I) =
     +               ESCALATED_MONTHLY_VALUE(
     +                              CAPACITY_CHARGE(I),
     +                              SERVICE_CAP_ESCALATION_VECTOR(I),
     +                              YR,ISEAS,SERVICE_UPDATE_SEASON(I))
         ENERGY_CHARGE(I) =
     +               ESCALATED_MONTHLY_VALUE(
     +                              ENERGY_CHARGE(I),
     +                              SERVICE_ENRG_ESCALATION_VECTOR(I),
     +                              YR,ISEAS,SERVICE_UPDATE_SEASON(I))

      ENDDO
      RETURN
!
! ENERGY LIMITED RESOURCE CALCULATIONS
!
!******************************************************************
      ENTRY EL_SERVICE_TRANS_CALCULATIONS(NUNITS,ENERGY,
     +                                    CAPACITY,
     +                                    ONLINE,OFLINE,
     +                                    DATE1,DATE2,ISEAS)
!******************************************************************
         DO I = 1, SERVICE_TRANS
            IF(INDEX(SERVICE_LINK_TYPE(I),'EL') /= 0 .OR.
     +                        trim(SERVICE_LINK_TYPE(I)) == 'TG') THEN
               IF(FIRST_EL_SERVICE_WRITE .AND.
     +                           MONTHLY_SERVICE_REPORT_ACTIVE) THEN
! TAKE ALL EL SERVICES FOR FIRST WRITE
               ELSEIF(  .NOT. WABASH_VALLEY_ACTIVE .AND.
     +                  (SERVICE_AVAILABLE(I) > DATE2 .OR.
     +                                  SERVICE_ENDS(I) < DATE1)) THEN
                  CYCLE
               ENDIF
               DO L = 1, NUNITS
                  IF((trim(SERVICE_LINK_TYPE(I)) == 'EL' .AND.
     +              INT(EL_RESOURCE_ID(L)) ==
     +                                  SERVICE_RESOURCE_PONTR(I)) .OR.
     +            (INDEX(SERVICE_LINK_TYPE(I),'TG') /= 0 .AND.
     +            HYDRO_TRANS_GROUP(L)==SERVICE_RESOURCE_PONTR(I)))THEN

                     IF(SERVICE_AVAILABLE(I) > DATE2 .OR.
     +                                    SERVICE_ENDS(I) < DATE1) THEN
                        SEASONAL_ENERGY = 0.
                        SEASONAL_CAPACITY = 0.
                        SEASONAL_ENERGY_CHARGE = 0.
                        SEASONAL_CAPACITY_CHARGE = 0.
                     ELSE
                       SEASONAL_ENERGY = ENERGY(L)*ENERGY_MULTIPLIER(I)
                        SEASONAL_CAPACITY = CAPACITY(L) *
     +                                           CAPACITY_MULTIPLIER(I)
                        SEASONAL_ENERGY_CHARGE = ENERGY_CHARGE(I) *
     +                                                 SEASONAL_ENERGY
                        SEASONAL_CAPACITY_CHARGE = CAPACITY_CHARGE(I) *
     +                                                SEASONAL_CAPACITY
                     ENDIF

                     IF(TYPE_OF_SERVICE(I) == 'T') THEN
                        CALL TRANSACTION_MANAGER(ISEAS,I,
     +                                    SERVICE_COST_ASSIGNMENT(I),
     +                                    TYPE_OF_SERVICE(I),
     +                                    SERVICE_NAME(I),
     +                                    SEASONAL_ENERGY,
     +                                    SEASONAL_ENERGY_CHARGE/1000.,
     +                                    ENERGY_CHARGE(I),
     +                                    SEASONAL_CAPACITY,
     +                                    SEASONAL_CAPACITY_CHARGE,
     +                                    CAPACITY_CHARGE(I),
     +                                   SERVICE_EXPENSE_COLLECTION(I),
     +                                    SERVICE_REPORTING_GROUP(I),
     +                                   MONTHLY_SERVICE_REPORT_ACTIVE,
     +                                    LOGICAL1_TRUE,
     +                                    WVPA_RATE_TRACKER_INDEX(I),
     +                                    WVPA_RES_TRACKER_INDEX(I),
     +                                    WVPA_FUEL_TRACKER_INDEX(I),
     +                                    WVPA_MEM_TRACKER_INDEX(I),
     +                               SERVICE_EXPENSE_CLASSIFICATION(I))
                     ENDIF
                     CALL STORE_ST_COST_AND_REVENUE_DATA(I,
     +                                    SEASONAL_ENERGY_CHARGE/1000.,
     +                                     SEASONAL_CAPACITY_CHARGE,
     +                                     SEASONAL_ENERGY,
     +                                     SEASONAL_CAPACITY)
                 ENDIF
               ENDDO ! EL UNITS COUNTER
            ENDIF ! YES EL UNIT
         ENDDO ! EL SERVICES COUNTER
         IF(FIRST_EL_SERVICE_WRITE) FIRST_EL_SERVICE_WRITE = .FALSE.
      RETURN
!
! CAPACITY LIMITED RESOURCE CALCULATIONS
!
!******************************************************************
      ENTRY CL_SERVICE_TRANS_CALCULATIONS(NUNITS,ENERGY,
     +                                    RESOURCE_ID,
     +                                    ONLINE,OFLINE,
     +                                    DATE1,DATE2,ISEAS,
     +                                    REVENUE_GENERATING_CAPACITY,
     +                                    EXPENSE_GENERATING_CAPACITY,
     +                                    CL_POOL_FRAC_OWN)
!******************************************************************
!
      DO I = 1, SERVICE_TRANS
         IF(INDEX(SERVICE_LINK_TYPE(I),'CL') /= 0 .OR.
     +                  trim(SERVICE_LINK_TYPE(I)) == 'TG') THEN
            IF(INDEX(SERVICE_LINK_TYPE(I),'CL') /= 0 .AND.
     +              FIRST_CL_SERVICE_WRITE .AND.
     +                              MONTHLY_SERVICE_REPORT_ACTIVE) THEN
!              TAKE ALL CL SERVICES FOR FIRST WRITE
            ELSEIF(  .NOT. WABASH_VALLEY_ACTIVE .AND.
     +                  (SERVICE_AVAILABLE(I) > DATE2 .OR.
     +                                   SERVICE_ENDS(I) < DATE1)) THEN

               CYCLE
            ENDIF
            DO L = 1, NUNITS
               IF((trim(SERVICE_LINK_TYPE(I)) == 'CL' .AND.
     +                RESOURCE_ID(L) == SERVICE_RESOURCE_PONTR(I)) .OR.
     +            (INDEX(SERVICE_LINK_TYPE(I),'TG') /= 0 .AND.
     +            TRANSACTION_GROUP(L)==SERVICE_RESOURCE_PONTR(I)))THEN
                  IF(.NOT. WABASH_VALLEY_ACTIVE .AND.
     +                (ONLINE(L) > DATE2 .OR. OFLINE(L) < DATE1)) CYCLE
                  IF(SERVICE_AVAILABLE(I) > DATE2 .OR.
     +                                    SERVICE_ENDS(I) < DATE1) THEN

                     SEASONAL_ENERGY = 0.
                     SEASONAL_CAPACITY = 0.
                     SEASONAL_ENERGY_CHARGE = 0.
                     SEASONAL_CAPACITY_CHARGE = 0.
                  ELSE

                     SEASONAL_ENERGY = (ENERGY(2*L-1)+ENERGY(2*L)) *
     +                                              SEASON_HOURS(ISEAS)
                     IF(ENERGY_TO_USE(I) == 'R') THEN
                        SEASONAL_ENERGY = SEASONAL_ENERGY
     +                                    - MON_ECO_SALES_ENRG_FROM(L)
                     ELSEIF(ENERGY_TO_USE(I) == 'W') THEN
                        SEASONAL_ENERGY = MON_ECO_SALES_ENRG_FROM(L)

                     ENDIF
                     SEASONAL_ENERGY = SEASONAL_ENERGY *
     +                                 ENERGY_MULTIPLIER(I) *
     +                                 CL_POOL_FRAC_OWN(L)/100.
                     IF(SERVICE_COST_ASSIGNMENT(I) == REVENUE) THEN
                        SEASONAL_CAPACITY = CAPACITY_MULTIPLIER(I) *
     +                                   REVENUE_GENERATING_CAPACITY(L)
                     ELSE
                        SEASONAL_CAPACITY = CAPACITY_MULTIPLIER(I) *
     +                                 EXPENSE_GENERATING_CAPACITY(2,L)
                     ENDIF
                     SEASONAL_CAPACITY = SEASONAL_CAPACITY *
     +                                         CL_POOL_FRAC_OWN(L)/100.
                     SEASONAL_ENERGY_CHARGE = ENERGY_CHARGE(I) *
     +                                                  SEASONAL_ENERGY
                     SEASONAL_CAPACITY_CHARGE = CAPACITY_CHARGE(I) *
     +                                                SEASONAL_CAPACITY
                  ENDIF

                  IF(TYPE_OF_SERVICE(I) == 'T') THEN
                     CALL TRANSACTION_MANAGER(ISEAS,I,
     +                                    SERVICE_COST_ASSIGNMENT(I),
     +                                    TYPE_OF_SERVICE(I),
     +                                    SERVICE_NAME(I),
     +                                    SEASONAL_ENERGY,
     +                                    SEASONAL_ENERGY_CHARGE/1000.,
     +                                    ENERGY_CHARGE(I),
     +                                    SEASONAL_CAPACITY,
     +                                    SEASONAL_CAPACITY_CHARGE,
     +                                    CAPACITY_CHARGE(I),
     +                                   SERVICE_EXPENSE_COLLECTION(I),
     +                                    SERVICE_REPORTING_GROUP(I),
     +                                   MONTHLY_SERVICE_REPORT_ACTIVE,
     +                                    LOGICAL1_TRUE,
     +                                    WVPA_RATE_TRACKER_INDEX(I),
     +                                    WVPA_RES_TRACKER_INDEX(I),
     +                                    WVPA_FUEL_TRACKER_INDEX(I),
     +                                    WVPA_MEM_TRACKER_INDEX(I),
     +                               SERVICE_EXPENSE_CLASSIFICATION(I))
                  ENDIF
                  CALL STORE_ST_COST_AND_REVENUE_DATA(I,
     +                                    SEASONAL_ENERGY_CHARGE/1000.,
     +                                     SEASONAL_CAPACITY_CHARGE,
     +                                     SEASONAL_ENERGY,
     +                                     SEASONAL_CAPACITY)

               ENDIF
            ENDDO ! CL UNITS COUNTER
         ENDIF ! YES CL UNIT
      ENDDO ! TRANSACTIONS COUNTER
      IF(FIRST_CL_SERVICE_WRITE) FIRST_CL_SERVICE_WRITE = .FALSE.
      RETURN
!
! CONTRACT RESOURCE CALCULATIONS
!
!******************************************************************
      ENTRY CT_SERVICE_TRANS_CALCULATIONS(NUNITS,ENERGY,
     +                                    CAPACITY,RESOURCE_ID,
     +                                    DATE1,DATE2,ISEAS)
!******************************************************************
      DO I = 1, SERVICE_TRANS
         IF(INDEX(SERVICE_LINK_TYPE(I),'CT')/= 0) THEN
            IF(FIRST_CT_SERVICE_WRITE .AND.
     +                              MONTHLY_SERVICE_REPORT_ACTIVE) THEN
! TAKE ALL CT SERVICES FOR FIRST WRITE
            ELSEIF(  .NOT. WABASH_VALLEY_ACTIVE .AND.
     +                  (SERVICE_AVAILABLE(I) > DATE2 .OR.
     +                                   SERVICE_ENDS(I) < DATE1)) THEN
               CYCLE
            ENDIF
            DO L = 1, NUNITS
               IF(RESOURCE_ID(L) ==
     +                          SERVICE_RESOURCE_PONTR(I)) THEN
                  IF(SERVICE_AVAILABLE(I) > DATE2 .OR.
     +                                    SERVICE_ENDS(I) < DATE1) THEN
                     SEASONAL_ENERGY = 0.
                     SEASONAL_CAPACITY = 0.
                     SEASONAL_ENERGY_CHARGE = 0.
                     SEASONAL_CAPACITY_CHARGE = 0.
                  ELSE
                    SEASONAL_ENERGY =ENERGY(L) * ENERGY_MULTIPLIER(I) *
     +                                              SEASON_HOURS(ISEAS)
                     SEASONAL_CAPACITY = CAPACITY(L) *
     +                                           CAPACITY_MULTIPLIER(I)
                     SEASONAL_ENERGY_CHARGE = ENERGY_CHARGE(I) *
     +                                                 SEASONAL_ENERGY
                     SEASONAL_CAPACITY_CHARGE = CAPACITY_CHARGE(I) *
     +                                                SEASONAL_CAPACITY
                  ENDIF

                  IF(TYPE_OF_SERVICE(I) == 'T') THEN
                     CALL TRANSACTION_MANAGER(ISEAS,I,
     +                                    SERVICE_COST_ASSIGNMENT(I),
     +                                    TYPE_OF_SERVICE(I),
     +                                    SERVICE_NAME(I),
     +                                    SEASONAL_ENERGY,
     +                                    SEASONAL_ENERGY_CHARGE/1000.,
     +                                    ENERGY_CHARGE(I),
     +                                    SEASONAL_CAPACITY,
     +                                    SEASONAL_CAPACITY_CHARGE,
     +                                    CAPACITY_CHARGE(I),
     +                                   SERVICE_EXPENSE_COLLECTION(I),
     +                                    SERVICE_REPORTING_GROUP(I),
     +                                   MONTHLY_SERVICE_REPORT_ACTIVE,
     +                                    LOGICAL1_TRUE,
     +                                    WVPA_RATE_TRACKER_INDEX(I),
     +                                    WVPA_RES_TRACKER_INDEX(I),
     +                                    WVPA_FUEL_TRACKER_INDEX(I),
     +                                    WVPA_MEM_TRACKER_INDEX(I),
     +                               SERVICE_EXPENSE_CLASSIFICATION(I))
                  ENDIF
                  CALL STORE_ST_COST_AND_REVENUE_DATA(I,
     +                                    SEASONAL_ENERGY_CHARGE/1000.,
     +                                     SEASONAL_CAPACITY_CHARGE,
     +                                     SEASONAL_ENERGY,
     +                                     SEASONAL_CAPACITY)
               ENDIF
            ENDDO ! CONTRACTS COUNTER
         ENDIF ! YES CONTRACT
      ENDDO ! TRANSACTIONS COUNTER
      IF(FIRST_CT_SERVICE_WRITE) FIRST_CT_SERVICE_WRITE = .FALSE.
      RETURN
!
! CALCULATE MONTHLY SERVICE COSTS FOR NON RESOURCE SPECIFIC REFERENCES
!
!******************************************************************
      ENTRY SERVICE_TRANS_CALCULATIONS(ISEAS,YR,DATE1,DATE2)
!******************************************************************
!
      DO I = 1, SERVICE_TRANS
!
! CAPACITY COSTING
!
!        ! ONLY STORE VALUES IF TRANSACTION TYPE FOUND
         TRANSACTION_FOUND = .FALSE.
!        ! ONLY WRITE NON-SPECIFIC TRANSACTIONS
         WRITE_IT_MONTHLY = .FALSE.
         IF(FIRST_OT_SERVICE_WRITE .AND.
     +                              MONTHLY_SERVICE_REPORT_ACTIVE) THEN
! TAKE ALL OT SERVICES FOR FIRST WRITE
         ELSEIF(  .NOT. WABASH_VALLEY_ACTIVE .AND.
     +                  (SERVICE_AVAILABLE(I) > DATE2 .OR.
     +                                   SERVICE_ENDS(I) < DATE1)) THEN
            CYCLE
         ENDIF
!
         SEASONAL_CAPACITY_CHARGE = 0.
         SEASONAL_ENERGY_CHARGE = 0.
         SEASONAL_ENERGY = 0.
         SEASONAL_CAPACITY = 0.
         SELECT CASE (trim(SERVICE_LINK_TYPE(I)))
!
         CASE ('CG')
!
            IF(SERVICE_AVAILABLE(I) > DATE2 .OR.
     +                                    SERVICE_ENDS(I) < DATE1) THEN
               SEASONAL_ENERGY = 0.
               SEASONAL_CAPACITY = 0.
            ELSE
               SEASONAL_ENERGY = ENERGY_MULTIPLIER(I) *
     +           GET_CUST_GROUP_ENERGY(SERVICE_RESOURCE_PONTR(I),ISEAS)

               SEASONAL_CAPACITY = CAPACITY_MULTIPLIER(I) *
     +             GET_CUST_GROUP_PEAK(SERVICE_RESOURCE_PONTR(I),ISEAS)

            ENDIF

            SEASONAL_CAPACITY_CHARGE = CAPACITY_CHARGE(I) *
     +                                               SEASONAL_CAPACITY
           SEASONAL_ENERGY_CHARGE = ENERGY_CHARGE(I) * SEASONAL_ENERGY
            WRITE_IT_MONTHLY = .TRUE.
            TRANSACTION_FOUND = .TRUE.
         CASE ('D1','D2','D3','D4','D5','D6')
            CLASS_CHR = SERVICE_LINK_TYPE(I)(2:2)
            CLASS = INDEX('123456',CLASS_CHR)
            CALL DSM_PEAK_ENER_RESERVE_ALLOC(PEAK_RESERVE,
     +                                        ENER_RESERVE,CLASS,ISEAS)
!
            IF(SERVICE_AVAILABLE(I) > DATE2 .OR.
     +                                    SERVICE_ENDS(I) < DATE1) THEN
               SEASONAL_ENERGY = 0.
               SEASONAL_CAPACITY = 0.
            ELSE
               SEASONAL_ENERGY = ENERGY_MULTIPLIER(I) *
     +                               (FORECAST_ENERGY(1,ISEAS,CLASS) +
     +                                 FORECAST_ENERGY(2,ISEAS,CLASS) +
     +                                                    ENER_RESERVE)
               SEASONAL_CAPACITY = CAPACITY_MULTIPLIER(I) *
     +                   (MAX(FORECAST_COINCIDENT_PEAK(1,ISEAS,CLASS),
     +                       FORECAST_COINCIDENT_PEAK(2,ISEAS,CLASS)) +
     +                                                    PEAK_RESERVE)
            ENDIF
!
            SEASONAL_ENERGY_CHARGE = ENERGY_CHARGE(I) * SEASONAL_ENERGY
            SEASONAL_CAPACITY_CHARGE = CAPACITY_CHARGE(I) *
     +                                                SEASONAL_CAPACITY
            WRITE_IT_MONTHLY = .TRUE.
            TRANSACTION_FOUND = .TRUE.
         CASE ('C1','C2','C3','C4','C5','C6',
     +                                   'P1','P2','P3','P4','P5','P6')
            IF( .NOT. WABASH_VALLEY_ACTIVE .AND.
     +                  (SERVICE_AVAILABLE(I) > DATE2 .OR.
     +                                  SERVICE_ENDS(I) < DATE1)) CYCLE
            CLASS_CHR = SERVICE_LINK_TYPE(I)(2:2)
            CLASS = INDEX('123456',CLASS_CHR)
            CLASS_CHR = SERVICE_LINK_TYPE(I)(1:1)
            IF(CLASS_CHR == 'C') THEN
               SEASONAL_CAPACITY_CHARGE = CAPACITY_CHARGE(I) *
     +                                CAPACITY_MULTIPLIER(I) *
     +                   MAX(FORECAST_COINCIDENT_PEAK(1,ISEAS,CLASS),
     +                       FORECAST_COINCIDENT_PEAK(2,ISEAS,CLASS))
            ELSE
               ANNUAL_CAPACITY_CHARGE(I) = ANNUAL_CAPACITY_CHARGE(I) +
     +                                       CAPACITY_MULTIPLIER(I) *
     +                                               CAPACITY_CHARGE(I)
            ENDIF
            SEASONAL_ENERGY_CHARGE = ENERGY_CHARGE(I) *
     +                               ENERGY_MULTIPLIER(I) *
     +                              (FORECAST_ENERGY(1,ISEAS,CLASS) +
     +                                FORECAST_ENERGY(2,ISEAS,CLASS))
            TRANSACTION_FOUND = .TRUE.
         CASE ('SF')
!
            IF(SERVICE_AVAILABLE(I) > DATE2 .OR.
     +                                    SERVICE_ENDS(I) < DATE1) THEN
               SEASONAL_ENERGY = 0.
               SEASONAL_CAPACITY = 0.
            ELSE
               SEASONAL_ENERGY = ENERGY_MULTIPLIER(I) *
     +                                      SEASON_SYSTEM_ENERGY(ISEAS)
               SEASONAL_CAPACITY = CAPACITY_MULTIPLIER(I) *
     +                                       SEASON_SYSTEM_PEAK(ISEAS)
            ENDIF
!
            SEASONAL_CAPACITY_CHARGE = CAPACITY_CHARGE(I) *
     +                                                SEASONAL_CAPACITY
            SEASONAL_ENERGY_CHARGE = ENERGY_CHARGE(I) * SEASONAL_ENERGY
            WRITE_IT_MONTHLY = .TRUE.
            TRANSACTION_FOUND = .TRUE.
         CASE ('PA')
            IF(SERVICE_AVAILABLE(I) > DATE2 .OR.
     +                                   SERVICE_ENDS(I) < DATE1) CYCLE
            ANNUAL_CAPACITY_CHARGE(I) = ANNUAL_CAPACITY_CHARGE(I) +
     +                                       CAPACITY_MULTIPLIER(I) *
     +                                               CAPACITY_CHARGE(I)
            SEASONAL_ENERGY_CHARGE = ENERGY_CHARGE(I) *
     +                                 ENERGY_MULTIPLIER(I) *
     +                                    SEASON_SYSTEM_ENERGY(ISEAS)
!         CASE DEFAULT
        CASE ('None','N','No')
            IF(SERVICE_ANNUAL_CAPACITY(I) < 0.) THEN
                  ANNUAL_CAPACITY = GET_VAR(SERVICE_ANNUAL_CAPACITY(I),
     +                                              YR,SERVICE_NAME(I))
            ELSE
               ANNUAL_CAPACITY = SERVICE_ANNUAL_CAPACITY(I)
            ENDIF
            IF(SERVICE_ANNUAL_ENERGY(I) < 0.) THEN
               ANNUAL_ENERGY = GET_VAR(SERVICE_ANNUAL_ENERGY(I),YR,
     +                                                 SERVICE_NAME(I))
            ELSE
               ANNUAL_ENERGY = SERVICE_ANNUAL_ENERGY(I)
            ENDIF
!
            IF(SERVICE_AVAILABLE(I) > DATE2 .OR.
     +                                    SERVICE_ENDS(I) < DATE1) THEN
               SEASONAL_ENERGY = 0.
               SEASONAL_CAPACITY = 0.
            ELSE
               SEASONAL_ENERGY =ANNUAL_ENERGY * ENERGY_MULTIPLIER(I)/12.
               SEASONAL_CAPACITY=ANNUAL_CAPACITY*CAPACITY_MULTIPLIER(I)
            ENDIF
!
            SEASONAL_ENERGY_CHARGE = ENERGY_CHARGE(I) * SEASONAL_ENERGY
            SEASONAL_CAPACITY_CHARGE = CAPACITY_CHARGE(I) *
     +                                                SEASONAL_CAPACITY
            WRITE_IT_MONTHLY = .TRUE.
            TRANSACTION_FOUND = .TRUE.
         END SELECT
!
! force in WVPA actual values
!
         RPT_ENERGY_CHARGE = ENERGY_CHARGE(I)
         RPT_CAPACITY_CHARGE = CAPACITY_CHARGE(I)

         IF(TYPE_OF_SERVICE(I) == 'T') THEN
            CALL TRANSACTION_MANAGER(ISEAS,I,
     +                               SERVICE_COST_ASSIGNMENT(I),
     +                               TYPE_OF_SERVICE(I),
     +                               SERVICE_NAME(I),
     +                               SEASONAL_ENERGY,
     +                               SEASONAL_ENERGY_CHARGE/1000.,
     +                               RPT_ENERGY_CHARGE,
     +                               SEASONAL_CAPACITY,
     +                               SEASONAL_CAPACITY_CHARGE,
     +                               RPT_CAPACITY_CHARGE,
     +                               SERVICE_EXPENSE_COLLECTION(I),
     +                               SERVICE_REPORTING_GROUP(I),
     +                               MONTHLY_SERVICE_REPORT_ACTIVE,
     +                               WRITE_IT_MONTHLY,
     +                               WVPA_RATE_TRACKER_INDEX(I),
     +                               WVPA_RES_TRACKER_INDEX(I),
     +                               WVPA_FUEL_TRACKER_INDEX(I),
     +                               WVPA_MEM_TRACKER_INDEX(I),
     +                               SERVICE_EXPENSE_CLASSIFICATION(I))
         ENDIF
         IF(TRANSACTION_FOUND) THEN
            CALL STORE_ST_COST_AND_REVENUE_DATA(I,
     +                                    SEASONAL_ENERGY_CHARGE/1000.,
     +                                     SEASONAL_CAPACITY_CHARGE,
     +                                     SEASONAL_ENERGY,
     +                                     SEASONAL_CAPACITY)
         ENDIF
      ENDDO ! TRANSACTIONS COUNTER
      IF(FIRST_OT_SERVICE_WRITE) FIRST_OT_SERVICE_WRITE = .FALSE.
      RETURN
!
!******************************************************************
      ENTRY GET_SERVICE_TRANS_TRACKERS(R_TRANS_NO,
     +                                 R_RES_TRACKER,
     +                                 R_FUEL_TRACKER,
     +                                 R_RATE_TRACKER,
     +                                 R_MEM_TRACKER)
!******************************************************************
         R_RES_TRACKER = WVPA_RES_TRACKER_INDEX(R_TRANS_NO)
         R_FUEL_TRACKER = WVPA_FUEL_TRACKER_INDEX(R_TRANS_NO)
         R_MEM_TRACKER = WVPA_MEM_TRACKER_INDEX(R_TRANS_NO)
      RETURN
!
!
!
!
! CALCULATE ANNUAL PEAK COSTS AND NON-LINKED COSTS
!
!******************************************************************
      ENTRY ANNUAL_SERVICE_TRANSACTIONS(YR,ANNUAL_PEAK,DATE1,DATE2)
!******************************************************************
!
! CALCULATE THE ANNUAL SERVICE TRANSACTION COSTS
!
      DO I = 1, SERVICE_TRANS
        IF(SERVICE_AVAILABLE(I)>DATE2 .OR. SERVICE_ENDS(I)<DATE1) CYCLE
         SELECT CASE (SERVICE_LINK_TYPE(I))
         CASE ('PA','P1','P2','P3','P4','P5','P6')
            IF(SERVICE_LINK_TYPE(I)(1:2) == 'PA') THEN
               SEASONAL_CAPACITY_CHARGE = ANNUAL_CAPACITY_CHARGE(I) *
     +                                                      ANNUAL_PEAK
               ANNUAL_ENERGY = 0.
            ELSEIF(SERVICE_LINK_TYPE(I)(1:1) == 'P') THEN
               CLASS_CHR = SERVICE_LINK_TYPE(I)(2:2)
               CLASS = INDEX('123456',CLASS_CHR)
               SEASONAL_CAPACITY_CHARGE = ANNUAL_CAPACITY_CHARGE(I) *
     +                                    ANNUAL_COINCIDENT_PEAK(CLASS)
               ANNUAL_ENERGY = 0.
            ENDIF

            IF(TYPE_OF_SERVICE(I) == 'T') THEN
               MO = 0
               CALL TRANSACTION_MANAGER(MO,I,
     +                                  SERVICE_COST_ASSIGNMENT(I),
     +                                  TYPE_OF_SERVICE(I),
     +                                  SERVICE_NAME(I),
     +                                  SEASONAL_ENERGY,
     +                                  SEASONAL_ENERGY_CHARGE/1000.,
     +                                  ENERGY_CHARGE(I),
     +                                  SEASONAL_CAPACITY,
     +                                  SEASONAL_CAPACITY_CHARGE,
     +                                  CAPACITY_CHARGE(I),
     +                                  SERVICE_EXPENSE_COLLECTION(I),
     +                                  SERVICE_REPORTING_GROUP(I),
     +                                  MONTHLY_SERVICE_REPORT_ACTIVE,
     +                                  LOGICAL1_TRUE,
     +                                  WVPA_RATE_TRACKER_INDEX(I),
     +                                  WVPA_RES_TRACKER_INDEX(I),
     +                                  WVPA_FUEL_TRACKER_INDEX(I),
     +                                  WVPA_MEM_TRACKER_INDEX(I),
     +                               SERVICE_EXPENSE_CLASSIFICATION(I))
            ENDIF
            CALL STORE_ANNUAL_ST_COST_DATA(I,
     +                                    SEASONAL_ENERGY_CHARGE/1000.,
     +                                     SEASONAL_CAPACITY_CHARGE,
     +                                     SEASONAL_ENERGY,
     +                                     SEASONAL_CAPACITY)
         END SELECT
      ENDDO
      RETURN
!
! END POINT FINISHED DEALLOCATE ARRAYS
!
!******************************************************************
      ENTRY CLOSE_SERVICE_TRANSACTIONS
!******************************************************************
      DEALLOCATE(CAPACITY_CHARGE,
     +           ENERGY_CHARGE,
     +           ANNUAL_CAPACITY_CHARGE,
     +           ANNUAL_ENERGY_CHARGE,
     +           CAPACITY_MULTIPLIER,
     +           ENERGY_MULTIPLIER,
     +           STAT=IOS)
      DEALLOCATE(SERVICE_NAME,
     +           SERVICE_ID_NUMBER,
     +           TYPE_OF_SERVICE,
     +           SERVICE_COST_ASSIGNMENT,
     +           SERVICE_EXPENSE_COLLECTION,
     +           SERVICE_REPORTING_GROUP,
!
     +           SERVICE_AVAILABLE,
     +           SERVICE_ENDS,
!
     +           SERVICE_UPDATE_SEASON,
     +           SERVICE_ANNUAL_CAPACITY,
     +           SERVICE_LINK_TYPE,
     +           SERVICE_RESOURCE_PONTR,
     +           SERVICE_CAPACITY_PATTERN,
     +           SERVICE_CAPACITY_CHARGE,
     +           SERVICE_CAP_ESCALATION_VECTOR,
     +           SERVICE_ANNUAL_ENERGY,
     +           SERVICE_ENERGY_PATTERN,
     +           SERVICE_ENERGY_CHARGE,
     +           SERVICE_ENRG_ESCALATION_VECTOR,
     +           SERVICE_REVENUE_CLASSIFICATION,
!
     +           SERVICE_INTRA_COMPY_TRANSACTION,
     +           ENERGY_TO_USE,
     +           WVPA_RATE_TRACKER,
     +           WVPA_RATE_TRACKER_INDEX,
     +           WVPA_RES_TRACKER_INDEX,
     +           WVPA_FUEL_TRACKER_INDEX,
     +           WVPA_MEM_TRACKER_INDEX,
     +           INTRA_ASSET_CLASS_ID,
     +           INTRA_ASSET_CLASS_ALLOC_VECTOR,
     +           INTRA_ACCOUNT_CLASSIFICATION,
     +           INTRA_EXPENSE_COLLECTION,
     +           SERVICE_EXPENSE_CLASSIFICATION,
     +           SERVICE_FUEL_CHARGE,
     +           SERVICE_FUEL_ESCALATION_VECTOR,
     +           STAT=IOS)
      RETURN
!
! ANNUAL SERVICE COST REPORT
!
!******************************************************************
      ENTRY SERVICE_COST_ANNUAL_REPORT(KEPCO)
!******************************************************************
      WRITE(9,'(//38X,A/)')
     +                'Transmission, Wheeling, Dispatching, etc. Costs'
      IF(KEPCO) THEN
         WRITE(9,1100) '   Service   ',
     +                 '   Total',
     +                 '     WPE',
     +                 '    KP&L',
     +                 '    KG&E',
     +                 '   KCP&L',
     +                 '     EDE',
     +                 '     SFE'
         GROUPS = 6
      ELSE
         WRITE(9,1100) '   Service   ',
     +                 '   Total',
     +                 ' Group 1',
     +                 ' Group 2',
     +                 ' Group 3',
     +                 ' Group 4',
     +                 ' Group 5',
     +                 ' Group 6'
         GROUPS = MAX_SERVICE_GROUPS
      ENDIF
      SERVICE_TITLE(1) = 'Transmission'
      SERVICE_TITLE(2) = 'Stand-by Transmission'
      SERVICE_TITLE(3) = 'Wheeling'
      SERVICE_TITLE(4) = 'Dispatching'
      SERVICE_TITLE(5) = 'Other'
      DO I = 1, MAX_SERVICE_ITEMS
         TOTAL = 0.
         DO CLASS = 1, MAX_SERVICE_GROUPS
            TOTAL = TOTAL + SERVICE_GROUP_COSTS(I,CLASS)
           SERVICE_GROUP_COSTS(I,CLASS) = SERVICE_GROUP_COSTS(I,CLASS)/
     +                                                1000.
            SUM_OF_SERVICE(CLASS) = SUM_OF_SERVICE(CLASS) +
     +                                     SERVICE_GROUP_COSTS(I,CLASS)
         ENDDO
         TOTAL = TOTAL/1000.
         SUM_OF_SERVICE(0) = SUM_OF_SERVICE(0) + TOTAL
         WRITE(9,1000) SERVICE_TITLE(I),TOTAL,
     +                 (SERVICE_GROUP_COSTS(I,CLASS),CLASS=1,GROUPS)
      ENDDO
      WRITE(9,1000) '      Total of Services',
     +                           (SUM_OF_SERVICE(CLASS),CLASS=0,GROUPS)
      RETURN
 1000 FORMAT(1X,A24,10F8.2)
 1100 FORMAT(1X,A20,T26,10(A))
!******************************************************************
      ENTRY UPDATE_SERVICE_REPORT_SWITCH
!******************************************************************
         IF(.NOT. TESTING_PLAN) THEN
            MONTHLY_SERVICE_REPORT_ACTIVE = SERVICE_REPORT() /= 'F'
         ELSE
            MONTHLY_SERVICE_REPORT_ACTIVE = .FALSE.
         ENDIF
      RETURN
!******************************************************************
      ENTRY GET_NO_OF_SERVICE_TRANS(R_SERVICE_TRANS)
!******************************************************************
         R_SERVICE_TRANS = SERVICE_TRANS
      RETURN
!******************************************************************
      ENTRY GET_TRANS_VARIABLES(R_SERVICE_TRANS,R_NAME,R_COST_ASSIGN,
     +                                    R_EXPENSE_ASSIGN,R_TRAN_TYPE)
!******************************************************************
! 11/03/03. MAJOR REWRITE
!
         DO I = 1, SERVICE_TRANS
            R_NAME(I) = SERVICE_NAME(I)
            R_COST_ASSIGN(I) = SERVICE_COST_ASSIGNMENT(I)
            R_EXPENSE_ASSIGN(I) =  SERVICE_EXPENSE_COLLECTION(I)
            R_TRAN_TYPE(I) = TYPE_OF_SERVICE(I)
         ENDDO
      RETURN
!
!******************************************************************
      ENTRY STORE_ST_COST_AND_REVENUE_DATA(UNIT_NO,
     +                                     VAR_COST,
     +                                     ST_FIXED_COST,
     +                                     TRANS_ENERGY,
     +                                     TRANS_CAPACITY)
!**********************************************************************
!
!
         ST_SO2_EMIS = 0.
         IF(.NOT. ALLOCATED(ANNUAL_ST_ENERGY)) THEN
            CALL GET_NUMBER_OF_SERVICES (NUMBER_OF_SERVICES)
            ALLOCATE(ANNUAL_ST_ENERGY(NUMBER_OF_SERVICES),
     +               ANNUAL_ST_CAPACITY(NUMBER_OF_SERVICES),
     +               ANNUAL_ST_VAR_COST(NUMBER_OF_SERVICES),
     +               ANNUAL_ST_FIXED_COST(NUMBER_OF_SERVICES),
     +               ANNUAL_ST_SO2_EMIS(NUMBER_OF_SERVICES))
            ANNUAL_ST_ENERGY = 0.
            ANNUAL_ST_CAPACITY = 0.
            ANNUAL_ST_VAR_COST = 0.
            ANNUAL_ST_SO2_EMIS = 0.
            ANNUAL_ST_FIXED_COST = 0.
         ENDIF
         ANNUAL_ST_ENERGY(UNIT_NO) = ANNUAL_ST_ENERGY(UNIT_NO) +
     +                                                     TRANS_ENERGY
         ANNUAL_ST_CAPACITY(UNIT_NO) = ANNUAL_ST_CAPACITY(UNIT_NO) +
     +                                       TRANS_CAPACITY/LAST_SEASON
         ANNUAL_ST_VAR_COST(UNIT_NO) = ANNUAL_ST_VAR_COST(UNIT_NO) +
     +                                                         VAR_COST
        ANNUAL_ST_FIXED_COST(UNIT_NO) = ANNUAL_ST_FIXED_COST(UNIT_NO) +
     +                                                    ST_FIXED_COST
         ANNUAL_ST_SO2_EMIS(UNIT_NO) = ANNUAL_ST_SO2_EMIS(UNIT_NO) +
     +                                                      ST_SO2_EMIS
!
!
! MONTHLY VALUES
!
         IF(.NOT. ALLOCATED(MONTHLY_ST_ENERGY)) THEN
            ALLOCATE(MONTHLY_ST_ENERGY(0:12,NUMBER_OF_SERVICES),
     +               MONTHLY_ST_CAPACITY(0:12,NUMBER_OF_SERVICES),
     +               MONTHLY_ST_VAR_COST(0:12,NUMBER_OF_SERVICES),
     +               MONTHLY_ST_FIXED_COST(0:12,NUMBER_OF_SERVICES),
     +               MONTHLY_ST_SO2_EMIS(0:12,NUMBER_OF_SERVICES))
            VALUES_2_ZERO = INT(13*NUMBER_OF_SERVICES)
            MONTHLY_ST_ENERGY = 0.
            MONTHLY_ST_CAPACITY = 0.
            MONTHLY_ST_VAR_COST = 0.
            MONTHLY_ST_SO2_EMIS = 0.
            MONTHLY_ST_FIXED_COST = 0.
         ENDIF
         MO = CURRENT_MONTH

            MONTHLY_ST_ENERGY(MO,UNIT_NO) =
     +                                    MONTHLY_ST_ENERGY(MO,UNIT_NO)
     +                                     + TRANS_ENERGY
            MONTHLY_ST_CAPACITY(MO,UNIT_NO) =
     +                                  MONTHLY_ST_CAPACITY(MO,UNIT_NO)
     +                                   + TRANS_CAPACITY/LAST_SEASON
            MONTHLY_ST_VAR_COST(MO,UNIT_NO) =
     +                                  MONTHLY_ST_VAR_COST(MO,UNIT_NO)
     +                                   + VAR_COST
            MONTHLY_ST_FIXED_COST(MO,UNIT_NO) =
     +                               MONTHLY_ST_FIXED_COST(MO,UNIT_NO)
     +                                 + ST_FIXED_COST
            MONTHLY_ST_SO2_EMIS(MO,UNIT_NO) =
     +                                  MONTHLY_ST_SO2_EMIS(MO,UNIT_NO)
     +                                   + ST_SO2_EMIS
            IF(CURRENT_MONTH /= 0) THEN
               MO = 0 ! KEEP A RUNNING ANNUAL TOTAL
               MONTHLY_ST_ENERGY(MO,UNIT_NO) =
     +                                    MONTHLY_ST_ENERGY(MO,UNIT_NO)
     +                                     + TRANS_ENERGY
               MONTHLY_ST_CAPACITY(MO,UNIT_NO) =
     +                                  MONTHLY_ST_CAPACITY(MO,UNIT_NO)
     +                                   + TRANS_CAPACITY/LAST_SEASON
               MONTHLY_ST_VAR_COST(MO,UNIT_NO) =
     +                                  MONTHLY_ST_VAR_COST(MO,UNIT_NO)
     +                                   + VAR_COST
               MONTHLY_ST_FIXED_COST(MO,UNIT_NO) =
     +                               MONTHLY_ST_FIXED_COST(MO,UNIT_NO)
     +                                 + ST_FIXED_COST
               MONTHLY_ST_SO2_EMIS(MO,UNIT_NO) =
     +                                  MONTHLY_ST_SO2_EMIS(MO,UNIT_NO)
     +                                   + ST_SO2_EMIS
            ENDIF
      RETURN

!******************************************************************
      ENTRY STORE_ANNUAL_ST_COST_DATA(UNIT_NO,
     +                                VAR_COST,
     +                                ST_FIXED_COST,
     +                                TRANS_ENERGY,
     +                                TRANS_CAPACITY)
!**********************************************************************
!
!
         ST_SO2_EMIS = 0.
         IF(.NOT. ALLOCATED(ANNUAL_ST_ENERGY)) THEN
            CALL GET_NUMBER_OF_SERVICES (NUMBER_OF_SERVICES)
            ALLOCATE(ANNUAL_ST_ENERGY(NUMBER_OF_SERVICES),
     +               ANNUAL_ST_CAPACITY(NUMBER_OF_SERVICES),
     +               ANNUAL_ST_VAR_COST(NUMBER_OF_SERVICES),
     +               ANNUAL_ST_FIXED_COST(NUMBER_OF_SERVICES),
     +               ANNUAL_ST_SO2_EMIS(NUMBER_OF_SERVICES))
            ANNUAL_ST_ENERGY = 0.
            ANNUAL_ST_CAPACITY = 0.
            ANNUAL_ST_VAR_COST = 0.
            ANNUAL_ST_SO2_EMIS = 0.
            ANNUAL_ST_FIXED_COST = 0.
         ENDIF
         ANNUAL_ST_ENERGY(UNIT_NO) = ANNUAL_ST_ENERGY(UNIT_NO) +
     +                                                     TRANS_ENERGY
         ANNUAL_ST_CAPACITY(UNIT_NO) = ANNUAL_ST_CAPACITY(UNIT_NO) +
     +                                       TRANS_CAPACITY/LAST_SEASON
         ANNUAL_ST_VAR_COST(UNIT_NO) = ANNUAL_ST_VAR_COST(UNIT_NO) +
     +                                                         VAR_COST
        ANNUAL_ST_FIXED_COST(UNIT_NO) = ANNUAL_ST_FIXED_COST(UNIT_NO) +
     +                                                    ST_FIXED_COST
         ANNUAL_ST_SO2_EMIS(UNIT_NO) = ANNUAL_ST_SO2_EMIS(UNIT_NO) +
     +                                                      ST_SO2_EMIS
!
!
! MONTHLY VALUES
!
         IF(.NOT. ALLOCATED(MONTHLY_ST_ENERGY)) THEN
            ALLOCATE(MONTHLY_ST_ENERGY(0:12,NUMBER_OF_SERVICES),
     +               MONTHLY_ST_CAPACITY(0:12,NUMBER_OF_SERVICES),
     +               MONTHLY_ST_VAR_COST(0:12,NUMBER_OF_SERVICES),
     +               MONTHLY_ST_FIXED_COST(0:12,NUMBER_OF_SERVICES),
     +               MONTHLY_ST_SO2_EMIS(0:12,NUMBER_OF_SERVICES))
            MONTHLY_ST_ENERGY = 0.
            MONTHLY_ST_CAPACITY = 0.
            MONTHLY_ST_VAR_COST = 0.
            MONTHLY_ST_SO2_EMIS = 0.
            MONTHLY_ST_FIXED_COST = 0.
         ENDIF
         MO = 0
         MONTHLY_ST_ENERGY(MO,UNIT_NO) = MONTHLY_ST_ENERGY(MO,UNIT_NO)
     +                                   + TRANS_ENERGY
         MONTHLY_ST_CAPACITY(MO,UNIT_NO) =
     +                                MONTHLY_ST_CAPACITY(MO,UNIT_NO)
     +                                + TRANS_CAPACITY/LAST_SEASON
         MONTHLY_ST_VAR_COST(MO,UNIT_NO) =
     +                                  MONTHLY_ST_VAR_COST(MO,UNIT_NO)
     +                                   + VAR_COST
         MONTHLY_ST_FIXED_COST(MO,UNIT_NO) =
     +                               MONTHLY_ST_FIXED_COST(MO,UNIT_NO)
     +                                 + ST_FIXED_COST
         MONTHLY_ST_SO2_EMIS(MO,UNIT_NO) =
     +                                  MONTHLY_ST_SO2_EMIS(MO,UNIT_NO)
     +                                   + ST_SO2_EMIS
         DO MO = 1, 12
            MONTHLY_ST_ENERGY(MO,UNIT_NO)=MONTHLY_ST_ENERGY(MO,UNIT_NO)
     +                                    + TRANS_ENERGY/12.
            MONTHLY_ST_CAPACITY(MO,UNIT_NO) =
     +                                MONTHLY_ST_CAPACITY(MO,UNIT_NO)
     +                               + TRANS_CAPACITY/(12.*LAST_SEASON)
            MONTHLY_ST_VAR_COST(MO,UNIT_NO) =
     +                                  MONTHLY_ST_VAR_COST(MO,UNIT_NO)
     +                                   + VAR_COST/12.
            MONTHLY_ST_FIXED_COST(MO,UNIT_NO) =
     +                               MONTHLY_ST_FIXED_COST(MO,UNIT_NO)
     +                                 + ST_FIXED_COST/12.
            MONTHLY_ST_SO2_EMIS(MO,UNIT_NO) =
     +                                  MONTHLY_ST_SO2_EMIS(MO,UNIT_NO)
     +                                   + ST_SO2_EMIS/12.
         ENDDO

      RETURN
!**********************************************************************
      ENTRY STORE_SERVICE_MONTH(R_ISEAS)
!**********************************************************************
         CURRENT_MONTH = R_ISEAS
      RETURN
!**********************************************************************
      ENTRY CALC_ST_ANN_ASSET_CLASS
!**********************************************************************
!
!
         IF(SERVICE_TRANS <= 0) RETURN
         IF(.NOT. ALLOCATED(ST_ANN_CLASS_ATL_EXPENSE)) RETURN
         IF(.NOT. ALLOCATED(MONTHLY_ST_ENERGY)) RETURN
         CURRENT_YEAR = YEAR+BASE_YEAR
         CURRENT_YEAR_COMPARISON = (CURRENT_YEAR-1900)*100
!
         ST_ANN_CLASS_EXPENSE_CAPACITY = 0.
         ST_ANN_CLASS_EXPENSE_ENERGY = 0.
         ST_ANN_CLASS_REVENUE_ENERGY = 0.
         ST_ANN_CLASS_REVENUE_CAPACITY = 0.
!
         ST_ANN_CLASS_ATL_EXPENSE = 0.
         ST_ANN_CLASS_BTL_EXPENSE = 0.
         ST_ANN_CLASS_ADJ_CLAUSE = 0.
         ST_BTL_LEASE_PAYMENT = 0.
         ST_NF_RATEBASE = 0.
!
         ST_ANN_CLASS_REVENUE = 0.
         ST_ANN_CLASS_EXPENSE = 0.
!
         IF(MONTHLY_SERVICE_REPORT_ACTIVE .AND.
     +                              SERVICE_TRANS_REPORT_NOT_OPEN) THEN
            TRANS_RPT_NO = ANNUAL_SERVICE_TRANS_HEADER(TRANS_RPT_REC)
            SERVICE_TRANS_REPORT_NOT_OPEN = .FALSE.
         ENDIF
!
         INCLUDE_FIXED_COSTS_ADJ_CLAUSE = FIXED_COSTS_IN_ADJ_CLAUSE()
!
         MO = 0
         DO UNIT = 1, SERVICE_TRANS
!
          IF(SERVICE_AVAILABLE(UNIT) - CURRENT_YEAR_COMPARISON>12 .OR.
     +         CURRENT_YEAR_COMPARISON - SERVICE_ENDS(UNIT) > 12) CYCLE
!
!
            IF(MONTHLY_SERVICE_REPORT_ACTIVE) THEN
               TRANSACTION_DESCRIPTION =
     +                       trim(LEFT_JUSTIFY_I2_IN_STR(UNIT))//' '//
     +                                               SERVICE_NAME(UNIT)
               IF(SERVICE_COST_ASSIGNMENT(UNIT) == 'R') THEN
                  COST_ASSIGNMENT_NAME = 'Revenue'
               ELSE
                  COST_ASSIGNMENT_NAME = 'Expense'
               ENDIF
               UNIT_ENERGY_COST = 0.
               UNIT_CAPACITY_COST = 0.
               IF(ANNUAL_ST_ENERGY(UNIT) /= 0.) THEN
                  UNIT_ENERGY_COST = ANNUAL_ST_VAR_COST(UNIT)/
     +                                           ANNUAL_ST_ENERGY(UNIT)
               ENDIF
               IF(ANNUAL_ST_CAPACITY(UNIT) /= 0.) THEN
                  UNIT_CAPACITY_COST = ANNUAL_ST_FIXED_COST(UNIT)/
     +                                         ANNUAL_ST_CAPACITY(UNIT)
               ENDIF
!
               WRITE(TRANS_RPT_NO,REC=TRANS_RPT_REC)
     +                     PRT_ENDPOINT(),
     +                     FLOAT(CURRENT_YEAR),
     +                     TRANSACTION_DESCRIPTION,
     +                     COST_ASSIGNMENT_NAME,
     +                     ANNUAL_ST_ENERGY(UNIT),
     +                     1000.*UNIT_ENERGY_COST,
     +                     ANNUAL_ST_VAR_COST(UNIT),
     +                     ANNUAL_ST_CAPACITY(UNIT),
     +                     UNIT_CAPACITY_COST/12.,
     +                     ANNUAL_ST_FIXED_COST(UNIT),
     +                     ANNUAL_ST_VAR_COST(UNIT) +
     +                                       ANNUAL_ST_FIXED_COST(UNIT)
               TRANS_RPT_REC = TRANS_RPT_REC + 1
            ENDIF
!
             IF(SERVICE_INTRA_COMPY_TRANSACTION(UNIT) == 'Y') THEN
! ALLOCATE TO TOTAL COMPANY AND ASSET CLASSES
               TRANSACTION_AMOUNT = MONTHLY_ST_VAR_COST(MO,UNIT) +
     +                                   MONTHLY_ST_FIXED_COST(MO,UNIT)
                IF(SERVICE_COST_ASSIGNMENT(UNIT) == REVENUE) THEN
                   REV_TYPE = INCOME_STATEMENT_POSITION(
     +                            SERVICE_REVENUE_CLASSIFICATION(UNIT))
                    EXP_TYPE = INCOME_STATEMENT_POSITION(
     +                              INTRA_ACCOUNT_CLASSIFICATION(UNIT))
                 IF(INDEX(INTRA_EXPENSE_COLLECTION(UNIT),'BTL')/=0)THEN
                     IF(EXP_TYPE == 22) THEN ! Lease Expense
                       ST_BTL_LEASE_PAYMENT(MO,-1) = TRANSACTION_AMOUNT
     +                                    + ST_BTL_LEASE_PAYMENT(MO,-1)
!                    ! Nuc Fuel Expense
                     ELSEIF(EXP_TYPE == 17 .OR. EXP_TYPE == 18) THEN
                        ST_NF_RATEBASE(MO,-1) = TRANSACTION_AMOUNT +
     +                                            ST_NF_RATEBASE(MO,-1)
                     ENDIF
                     EXP_TYPE = 28
                  ENDIF
                ELSE
                   REV_TYPE = INCOME_STATEMENT_POSITION(
     +                              INTRA_ACCOUNT_CLASSIFICATION(UNIT))
                     EXP_TYPE = INCOME_STATEMENT_POSITION(
     +                            SERVICE_EXPENSE_CLASSIFICATION(UNIT))
                  IF(INDEX(SERVICE_EXPENSE_COLLECTION(UNIT),'BTL')
     +                                                       /= 0) THEN
                     IF(EXP_TYPE == 22) THEN
                       ST_BTL_LEASE_PAYMENT(MO,-1) = TRANSACTION_AMOUNT
     +                                    + ST_BTL_LEASE_PAYMENT(MO,-1)
!                    ! Nuc Fuel Expense
                     ELSEIF(EXP_TYPE == 17 .OR. EXP_TYPE == 18) THEN
                        ST_NF_RATEBASE(MO,-1) = TRANSACTION_AMOUNT
     +                                          + ST_NF_RATEBASE(MO,-1)
                     ENDIF
                     EXP_TYPE = 28
                  ENDIF
                ENDIF
              ST_ANN_CLASS_REVENUE(MO,-1,REV_TYPE) = TRANSACTION_AMOUNT
     +                           + ST_ANN_CLASS_REVENUE(MO,-1,REV_TYPE)
              ST_ANN_CLASS_EXPENSE(MO,-1,EXP_TYPE) = TRANSACTION_AMOUNT
     +                           + ST_ANN_CLASS_EXPENSE(MO,-1,EXP_TYPE)
!
                IF(INTRA_ASSET_CLASS_ID(UNIT) < 0.) THEN
                   CALL GET_ASSET_VAR(ABS(INTRA_ASSET_CLASS_ID(UNIT)),
     +                                      DUMMY_TYPE,ASSET_CLASS_LIST)
                   CALL GET_ASSET_VAR(
     +                        ABS(INTRA_ASSET_CLASS_ALLOC_VECTOR(UNIT)),
     +                                 DUMMY_TYPE,ASSET_ALLOCATION_LIST)
                ELSE
                   ASSET_CLASS_LIST(1) = INTRA_ASSET_CLASS_ID(UNIT)
                   ASSET_CLASS_LIST(2) = 0
                   ASSET_ALLOCATION_LIST(1) = 100.
                   ASSET_ALLOCATION_LIST(2) = 0.
                ENDIF
!
                CLASS_POINTER = 1
                DO
                   ASSET_CLASS = ASSET_CLASS_LIST(CLASS_POINTER)
                   CALL CHECK_IF_CLASS_DEFINED(ASSET_CLASS)
                   ASSET_CLASS = ASSET_CLASS + 1

                 ASSET_ALLOCATOR = ASSET_ALLOCATION_LIST(CLASS_POINTER)
                  IF(ASSET_ALLOCATOR < 0.) THEN
                     ALLOCATION_VECTOR = ABS(ASSET_ALLOCATOR)
                     CALL GET_ASSET_VAR(ALLOCATION_VECTOR,
     +                                     DUMMY_TYPE,ALLOCATION_VALUE)
                     ASSET_ALLOCATOR =
     +                     ALLOCATION_VALUE(MIN(AVAIL_DATA_YEARS,YEAR))
                  ENDIF
!
                  ASSET_ALLOCATOR = ASSET_ALLOCATOR/100.
                  TRANSACTION_AMOUNT = (MONTHLY_ST_VAR_COST(MO,UNIT) +
     +                                 MONTHLY_ST_FIXED_COST(MO,UNIT))*
     +                                                  ASSET_ALLOCATOR
!
                   IF(SERVICE_COST_ASSIGNMENT(UNIT) == REVENUE) THEN
                     ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS,EXP_TYPE) =
     +                    ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS,EXP_TYPE)
     +                     + TRANSACTION_AMOUNT
                     IF(INDEX(INTRA_EXPENSE_COLLECTION(UNIT),'Adj')
     +                                                      /= 0) THEN
                        IF(INCLUDE_FIXED_COSTS_ADJ_CLAUSE) THEN
                           ST_ANN_CLASS_ADJ_CLAUSE(MO,ASSET_CLASS) =
     +                          ST_ANN_CLASS_ADJ_CLAUSE(MO,ASSET_CLASS)
     +                           + TRANSACTION_AMOUNT
                        ELSE
                           ST_ANN_CLASS_ADJ_CLAUSE(MO,ASSET_CLASS) =
     +                          ST_ANN_CLASS_ADJ_CLAUSE(MO,ASSET_CLASS)
     +                           + ASSET_ALLOCATOR *
     +                                     MONTHLY_ST_VAR_COST(MO,UNIT)
                        ENDIF
                     ENDIF
                   ELSE
                     ST_ANN_CLASS_REVENUE(MO,ASSET_CLASS,REV_TYPE) =
     +                    ST_ANN_CLASS_REVENUE(MO,ASSET_CLASS,REV_TYPE)
     +                     + TRANSACTION_AMOUNT
                   ENDIF
!
                   CLASS_POINTER = CLASS_POINTER + 1
                   IF(ASSET_CLASS_LIST(CLASS_POINTER) == 0 .OR.
     +                          CLASS_POINTER > AVAIL_DATA_YEARS) EXIT
                ENDDO
             ENDIF ! END INTEA-COMPANY STUFF
!
!
!
!
            ASSET_CLASS = ASSET_CLASS_NUM(UNIT)
            ASSET_ALLOCATION_VECTOR = ASSET_CLASS_VECTOR(UNIT)
!
            IF(ASSET_CLASS < 0) THEN
               CALL GET_ASSET_VAR(ABS(ASSET_CLASS),
     +                                 DUMMY_TYPE,ASSET_CLASS_LIST)
               CALL GET_ASSET_VAR(ABS(ASSET_ALLOCATION_VECTOR),
     +                                DUMMY_TYPE,ASSET_ALLOCATION_LIST)
            ELSE
               ASSET_CLASS_LIST(1) = ASSET_CLASS
               ASSET_CLASS_LIST(2) = 0.
               ASSET_ALLOCATION_LIST(1) = 100.
               ASSET_ALLOCATION_LIST(2) = 0.
            ENDIF
            CLASS_POINTER = 1
!
            DO
               ASSET_CLASS = ASSET_CLASS_LIST(CLASS_POINTER)
               CALL CHECK_IF_CLASS_DEFINED(ASSET_CLASS)
               ASSET_CLASS = ASSET_CLASS + 1

               ASSET_ALLOCATOR = ASSET_ALLOCATION_LIST(CLASS_POINTER)
               IF(ASSET_ALLOCATOR < 0.) THEN
                  ALLOCATION_VECTOR = ABS(ASSET_ALLOCATOR)
                  CALL GET_ASSET_VAR(ALLOCATION_VECTOR,
     +                                     DUMMY_TYPE,ALLOCATION_VALUE)
                  ASSET_ALLOCATOR =
     +                     ALLOCATION_VALUE(MIN(AVAIL_DATA_YEARS,YEAR))
               ENDIF
!
               ASSET_ALLOCATOR = ASSET_ALLOCATOR/100.
!
               DO MO = 1, 12
                  IF(.NOT. TRANSFER_TRANSACT_ANL_RESULTS(MO)
     +                                              .AND. WVPA()) CYCLE

                  IF(SERVICE_COST_ASSIGNMENT(UNIT) == REVENUE) THEN
                     ST_ANN_CLASS_REVENUE_CAPACITY(MO,ASSET_CLASS) =
     +                  ST_ANN_CLASS_REVENUE_CAPACITY(MO,ASSET_CLASS)
     +                 + ASSET_ALLOCATOR * MONTHLY_ST_CAPACITY(MO,UNIT)
                     ST_ANN_CLASS_REVENUE_ENERGY(MO,ASSET_CLASS) =
     +                    ST_ANN_CLASS_REVENUE_ENERGY(MO,ASSET_CLASS)
     +                   + ASSET_ALLOCATOR * MONTHLY_ST_ENERGY(MO,UNIT)
!
                     ST_REVENUE = ASSET_ALLOCATOR *
     +                                  (MONTHLY_ST_VAR_COST(MO,UNIT) +
     +                                  MONTHLY_ST_FIXED_COST(MO,UNIT))
                      REV_TYPE = INCOME_STATEMENT_POSITION(
     +                            SERVICE_REVENUE_CLASSIFICATION(UNIT))
!
                     ST_ANN_CLASS_REVENUE(MO,ASSET_CLASS,REV_TYPE) =
     +                    ST_ANN_CLASS_REVENUE(MO,ASSET_CLASS,REV_TYPE)
     +                     + ST_REVENUE
                  ELSE
!
                     ST_ANN_CLASS_EXPENSE_CAPACITY(MO,ASSET_CLASS) =
     +                  ST_ANN_CLASS_EXPENSE_CAPACITY(MO,ASSET_CLASS)
     +                 + ASSET_ALLOCATOR * MONTHLY_ST_CAPACITY(MO,UNIT)
                     ST_ANN_CLASS_EXPENSE_ENERGY(MO,ASSET_CLASS) =
     +                    ST_ANN_CLASS_EXPENSE_ENERGY(MO,ASSET_CLASS)
     +                   + ASSET_ALLOCATOR * MONTHLY_ST_ENERGY(MO,UNIT)
                     ST_EXPENSE = ASSET_ALLOCATOR *
     +                                 (MONTHLY_ST_VAR_COST(MO,UNIT) +
     +                                  MONTHLY_ST_FIXED_COST(MO,UNIT))
                       EXP_TYPE = INCOME_STATEMENT_POSITION(
     +                            SERVICE_EXPENSE_CLASSIFICATION(UNIT))
                     IF(INDEX('BTL,X,N',
     +                     SERVICE_EXPENSE_COLLECTION(UNIT)) /= 0) THEN
                        IF(EXP_TYPE == 22) THEN
                           ST_BTL_LEASE_PAYMENT(MO,ASSET_CLASS) =
     +                             ST_BTL_LEASE_PAYMENT(MO,ASSET_CLASS)
     +                              +  ST_EXPENSE
!                       ! Nuc Fuel Expense
                        ELSEIF(EXP_TYPE == 17 .OR. EXP_TYPE == 18) THEN
                           ST_NF_RATEBASE(MO,ASSET_CLASS) =
     +                                   ST_NF_RATEBASE(MO,ASSET_CLASS)
     +                                    + TRANSACTION_AMOUNT
                        ENDIF
                        EXP_TYPE = 28
                     ENDIF
!
                     SELECT CASE(
     +                         trim(SERVICE_EXPENSE_COLLECTION(UNIT)))
                        CASE ('BTL','X','N')
                           IF(EXP_TYPE == 22) THEN
                              ST_BTL_LEASE_PAYMENT(MO,ASSET_CLASS) =
     +                             ST_BTL_LEASE_PAYMENT(MO,ASSET_CLASS)
     +                              + ST_EXPENSE
                           ENDIF
                           EXP_TYPE = 28
                        CASE ('Adj','A')
                           IF(INCLUDE_FIXED_COSTS_ADJ_CLAUSE) THEN
                           ST_ANN_CLASS_ADJ_CLAUSE(MO,ASSET_CLASS) =
     +                          ST_ANN_CLASS_ADJ_CLAUSE(MO,ASSET_CLASS)
     +                           + TRANSACTION_AMOUNT
                           ELSE
                             ST_ANN_CLASS_ADJ_CLAUSE(MO,ASSET_CLASS) =
     +                          ST_ANN_CLASS_ADJ_CLAUSE(MO,ASSET_CLASS)
     +                           + ASSET_ALLOCATOR *
     +                                     MONTHLY_ST_VAR_COST(MO,UNIT)
                           ENDIF
                     END SELECT
                     ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS,EXP_TYPE) =
     +                    ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS,EXP_TYPE)
     +                     + ST_EXPENSE
                  ENDIF
               ENDDO
!
               CLASS_POINTER = CLASS_POINTER + 1
               IF(ASSET_CLASS_LIST(CLASS_POINTER) == 0. .OR.
     +                           CLASS_POINTER > AVAIL_DATA_YEARS) EXIT
            ENDDO ! ASSET CLASSES
!
         ENDDO ! SERVICE RESOURCES
         IF(ALLOCATED(ANNUAL_ST_ENERGY))
     +         DEALLOCATE(ANNUAL_ST_ENERGY,
     +                    ANNUAL_ST_CAPACITY,
     +                    ANNUAL_ST_VAR_COST,
     +                    ANNUAL_ST_FIXED_COST,
     +                    ANNUAL_ST_SO2_EMIS)
         IF(ALLOCATED(MONTHLY_ST_ENERGY))
     +          DEALLOCATE(MONTHLY_ST_ENERGY,
     +                     MONTHLY_ST_CAPACITY,
     +                     MONTHLY_ST_VAR_COST,
     +                     MONTHLY_ST_FIXED_COST,
     +                     MONTHLY_ST_SO2_EMIS)
!
! SUM TO ANNUAL TOTALS
!
         ST_ANN_CLASS_ATL_EXPENSE(0,:) =
     +                        SUM(ST_ANN_CLASS_ATL_EXPENSE(1:,:),DIM=1)
         ST_ANN_CLASS_BTL_EXPENSE(0,:) =
     +                        SUM(ST_ANN_CLASS_BTL_EXPENSE(1:,:),DIM=1)
         ST_ANN_CLASS_ADJ_CLAUSE(0,:) =
     +                         SUM(ST_ANN_CLASS_ADJ_CLAUSE(1:,:),DIM=1)
         ST_BTL_LEASE_PAYMENT(0,:) =
     +                            SUM(ST_BTL_LEASE_PAYMENT(1:,:),DIM=1)
         ST_NF_RATEBASE(0,:) = SUM(ST_NF_RATEBASE(1:,:),DIM=1)
         ST_ANN_CLASS_REVENUE_CAPACITY(0,:) =
     +                   SUM(ST_ANN_CLASS_REVENUE_CAPACITY(1:,:),DIM=1)
         ST_ANN_CLASS_REVENUE_ENERGY(0,:) =
     +                     SUM(ST_ANN_CLASS_REVENUE_ENERGY(1:,:),DIM=1)
         ST_ANN_CLASS_EXPENSE_CAPACITY(0,:) =
     +                   SUM(ST_ANN_CLASS_EXPENSE_CAPACITY(1:,:),DIM=1)
         ST_ANN_CLASS_EXPENSE_ENERGY(0,:) =
     +                     SUM(ST_ANN_CLASS_EXPENSE_ENERGY(1:,:),DIM=1)
!
         ST_ANN_CLASS_REVENUE(0,:,:) =
     +                          SUM(ST_ANN_CLASS_REVENUE(1:,:,:),DIM=1)
         ST_ANN_CLASS_EXPENSE(0,:,11:) =
     +                        SUM(ST_ANN_CLASS_EXPENSE(1:,:,11:),DIM=1)
!
! SCALE
!
         ST_ANN_CLASS_BTL_EXPENSE = ST_ANN_CLASS_BTL_EXPENSE/1000.
         ST_ANN_CLASS_ATL_EXPENSE = ST_ANN_CLASS_ATL_EXPENSE/1000.
         ST_ANN_CLASS_ADJ_CLAUSE = ST_ANN_CLASS_ADJ_CLAUSE/1000.
         ST_BTL_LEASE_PAYMENT = ST_BTL_LEASE_PAYMENT/1000.
         ST_NF_RATEBASE = ST_NF_RATEBASE/1000.
         ST_ANN_CLASS_REVENUE = ST_ANN_CLASS_REVENUE/1000.
         ST_ANN_CLASS_EXPENSE = ST_ANN_CLASS_EXPENSE/1000.

      RETURN ! CALC_ST_ANN_ASSET_CLASS

!******************************************************************
      ENTRY MONTHLY_SERVICE_TRANS_EXPENSES(R_CLASS,
     +                                     MONTH_VARS)
!******************************************************************
!
!
         IF(SERVICE_TRANS <= 0) RETURN
!
         IF(R_CLASS <= MAX_SERVICE_CLASS_ID_NUM) THEN
            ASSET_CLASS = R_CLASS
            IF(ASSET_CLASS > 0 .OR. R_CLASS == -1) THEN
               DO MO = 0, 12
!
! EXPENSES
!
                  MONTH_VARS(MO,FossilFuel) =
     +                    MONTH_VARS(MO,FossilFuel) +
     +                          ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS,11)
!
                  MONTH_VARS(MO,PurchasedPower) =
     +                    MONTH_VARS(MO,PurchasedPower) +
     +                          ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS,12)
!
                  MONTH_VARS(MO,VariableOandM) =
     +                     MONTH_VARS(MO,VariableOandM) +
     +                          ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS,13)
!
                  MONTH_VARS(MO,FixedOandM) =
     +                   MONTH_VARS(MO,FixedOandM) +
     +                          ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS,14)
!
                  MONTH_VARS(MO,OtherOandM) =
     +                    MONTH_VARS(MO,OtherOandM) +
     +                          ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS,15)
!
                  MONTH_VARS(MO,PurchasedGas) =
     +                    MONTH_VARS(MO,PurchasedGas) +
     +                          ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS,16)
!
                  MONTH_VARS(MO,Other) =
     +                    MONTH_VARS(MO,Other) +
     +                          ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS,17)
!
                  MONTH_VARS(MO,OwnedNuclearFuel) =
     +                    MONTH_VARS(MO,OwnedNuclearFuel) +
     +                          ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS,18)
!
                  MONTH_VARS(MO,LeasedNuclearFuel) =
     +                    MONTH_VARS(MO,LeasedNuclearFuel) +
     +                          ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS,19)
!
                  MONTH_VARS(MO,DSMExpense) =
     +                    MONTH_VARS(MO,DSMExpense) +
     +                          ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS,20)
!
                  MONTH_VARS(MO,DSMRebate) =
     +                    MONTH_VARS(MO,DSMRebate) +
     +                          ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS,21)
!
                  MONTH_VARS(MO,ATLBookLeaseExpense) =
     +                    MONTH_VARS(MO,ATLBookLeaseExpense) +
     +                          ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS,22)
!
                  MONTH_VARS(MO,ServiceTransactions) =
     +                    MONTH_VARS(MO,ServiceTransactions) +
     +                          ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS,23)
!
                  MONTH_VARS(MO,EmissionCredits) =
     +                    MONTH_VARS(MO,EmissionCredits) +
     +                          ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS,24)
!
                  MONTH_VARS(MO,DOEDecommissioning) =
     +                    MONTH_VARS(MO,DOEDecommissioning) +
     +                          ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS,25)
!
                  MONTH_VARS(MO,DOEDisposal) =
     +                    MONTH_VARS(MO,DOEDisposal) +
     +                          ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS,26)
!
                  MONTH_VARS(MO,CatawbaExpenses) =
     +                    MONTH_VARS(MO,CatawbaExpenses) +
     +                          ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS,27)
!
                  MONTH_VARS(MO,BTLExpenses) =
     +                   MONTH_VARS(MO,BTLExpenses) +
     +                          ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS,28)
!
                  MONTH_VARS(MO,BTLLeaseCash) =
     +                    MONTH_VARS(MO,BTLLeaseCash) +
     +                             ST_BTL_LEASE_PAYMENT(MO,ASSET_CLASS)
!
                  MONTH_VARS(MO,Exp Collection in Adj Clause) =
     +                    MONTH_VARS(MO,Exp Collection in Adj Clause) +
     +                          ST_ANN_CLASS_ADJ_CLAUSE(MO,ASSET_CLASS)

               ENDDO
            ENDIF
         ENDIF
      RETURN
!******************************************************************
      ENTRY MONTHLY_SERVICE_TRANS_REVENUES(R_CLASS,
     +                                     MONTH_VARS)
!******************************************************************
!
         IF(SERVICE_TRANS <= 0) RETURN
!
         IF(R_CLASS <= MAX_SERVICE_CLASS_ID_NUM) THEN
            ASSET_CLASS = R_CLASS
            IF(ASSET_CLASS > 0 .OR. R_CLASS == -1) THEN
!               DO MO = 1, 12
!
! REVENUES
!
                  MONTH_VARS(:,BaseRates) =
     +                    MONTH_VARS(:,BaseRates) +
     +                            ST_ANN_CLASS_REVENUE(:,ASSET_CLASS,1)
                  MONTH_VARS(:,AdjustmentClause) =
     +                    MONTH_VARS(:,AdjustmentClause) +
     +                            ST_ANN_CLASS_REVENUE(:,ASSET_CLASS,2)
                  MONTH_VARS(:,SecondarySales) =
     +                    MONTH_VARS(:,SecondarySales) +
     +                            ST_ANN_CLASS_REVENUE(:,ASSET_CLASS,3)
                  MONTH_VARS(:,OtherRevenue) =
     +                    MONTH_VARS(:,OtherRevenue) +
     +                            ST_ANN_CLASS_REVENUE(:,ASSET_CLASS,4)
                  MONTH_VARS(:,BTLRevenues) =
     +                    MONTH_VARS(:,BTLRevenues) +
     +                            ST_ANN_CLASS_REVENUE(:,ASSET_CLASS,5)
                  MONTH_VARS(:,CatawbaRevenues) =
     +                    MONTH_VARS(:,CatawbaRevenues) +
     +                            ST_ANN_CLASS_REVENUE(:,ASSET_CLASS,6)
                  MONTH_VARS(:,GasRevenues) =
     +                    MONTH_VARS(:,GasRevenues) +
     +                            ST_ANN_CLASS_REVENUE(:,ASSET_CLASS,7)
                  MONTH_VARS(:,UnbilledRevenues) =
     +                    MONTH_VARS(:,UnbilledRevenues) +
     +                            ST_ANN_CLASS_REVENUE(:,ASSET_CLASS,8)
                  MONTH_VARS(:,DeferredRevenues) =
     +                    MONTH_VARS(:,DeferredRevenues) +
     +                            ST_ANN_CLASS_REVENUE(:,ASSET_CLASS,9)
                  MONTH_VARS(:,RelationshipRevenues) =
     +                    MONTH_VARS(:,RelationshipRevenues) +
     +                           ST_ANN_CLASS_REVENUE(:,ASSET_CLASS,10)
                  MONTH_VARS(:,Residential) =
     +                    MONTH_VARS(:,Residential) +
     +                           ST_ANN_CLASS_REVENUE(:,ASSET_CLASS,11)
                  MONTH_VARS(:,Commercial) =
     +                    MONTH_VARS(:,Commercial) +
     +                           ST_ANN_CLASS_REVENUE(:,ASSET_CLASS,12)
                  MONTH_VARS(:,Industrial) =
     +                    MONTH_VARS(:,Industrial) +
     +                           ST_ANN_CLASS_REVENUE(:,ASSET_CLASS,13)
                  MONTH_VARS(:,Lighting) =
     +                    MONTH_VARS(:,Lighting) +
     +                           ST_ANN_CLASS_REVENUE(:,ASSET_CLASS,14)
                  MONTH_VARS(:,BulkPower) =
     +                    MONTH_VARS(:,BulkPower) +
     +                           ST_ANN_CLASS_REVENUE(:,ASSET_CLASS,15)
                  MONTH_VARS(:,NetofTaxBTLRevenues) =
     +                    MONTH_VARS(:,NetofTaxBTLRevenues) +
     +                           ST_ANN_CLASS_REVENUE(:,ASSET_CLASS,16)
                  MONTH_VARS(:,CapacitySales) =
     +                    MONTH_VARS(:,CapacitySales) +
     +                           ST_ANN_CLASS_REVENUE(:,ASSET_CLASS,17)
                  MONTH_VARS(:,Government) =
     +                    MONTH_VARS(:,Government) +
     +                           ST_ANN_CLASS_REVENUE(:,ASSET_CLASS,18)
!               ENDDO
            ENDIF
         ENDIF
      RETURN
!******************************************************************
      ENTRY MONTHLY_SERVICE_TRANS_CASH(R_CLASS,
     +                                 MONTH_VARS)
!******************************************************************
!
         IF(SERVICE_TRANS <= 0) RETURN
!
         CALL MONTHLY_SERVICE_TRANS_REVENUES(R_CLASS,MONTH_VARS)
!
         IF(R_CLASS <= MAX_SERVICE_CLASS_ID_NUM) THEN
            ASSET_CLASS = R_CLASS
            IF(ASSET_CLASS > 0 .OR. R_CLASS == -1) THEN
!               DO MO = 1, 12
!
! EXPENSES
!
                  MONTH_VARS(:,Cash_Fossil_Fuel) =
     +                    MONTH_VARS(:,Cash_Fossil_Fuel) +
     +                           ST_ANN_CLASS_EXPENSE(:,ASSET_CLASS,11)
!
                  MONTH_VARS(:,Cash_Purchased_Power) =
     +                    MONTH_VARS(:,Cash_Purchased_Power) +
     +                           ST_ANN_CLASS_EXPENSE(:,ASSET_CLASS,12)
!
                  MONTH_VARS(:,Cash_Variable_OandM) =
     +                     MONTH_VARS(:,Cash_Variable_OandM) +
     +                           ST_ANN_CLASS_EXPENSE(:,ASSET_CLASS,13)
!
                  MONTH_VARS(:,Cash_Fixed_OandM) =
     +                   MONTH_VARS(:,Cash_Fixed_OandM) +
     +                           ST_ANN_CLASS_EXPENSE(:,ASSET_CLASS,14)
!
                  MONTH_VARS(:,Cash_Other_OandM) =
     +                    MONTH_VARS(:,Cash_Other_OandM) +
     +                           ST_ANN_CLASS_EXPENSE(:,ASSET_CLASS,15)
!
                  MONTH_VARS(:,CashPurchasedGas) =
     +                    MONTH_VARS(:,CashPurchasedGas) +
     +                           ST_ANN_CLASS_EXPENSE(:,ASSET_CLASS,16)
!
                  MONTH_VARS(:,Cash_Other) =
     +                    MONTH_VARS(:,Cash_Other) +
     +                           ST_ANN_CLASS_EXPENSE(:,ASSET_CLASS,17)
!
                  MONTH_VARS(:,Cash_Leased_Nuclear_Fuel) =
     +                    MONTH_VARS(:,Cash_Leased_Nuclear_Fuel) +
     +                           ST_ANN_CLASS_EXPENSE(:,ASSET_CLASS,19)
!
                  MONTH_VARS(:,CashDSMExpense) =
     +                    MONTH_VARS(:,CashDSMExpense) +
     +                           ST_ANN_CLASS_EXPENSE(:,ASSET_CLASS,20)
!
                  MONTH_VARS(:,CashDSMRebate) =
     +                    MONTH_VARS(:,CashDSMRebate) +
     +                           ST_ANN_CLASS_EXPENSE(:,ASSET_CLASS,21)
!
                  MONTH_VARS(:,CashLeaseExpense) =
     +                    MONTH_VARS(:,CashLeaseExpense) +
     +                           ST_ANN_CLASS_EXPENSE(:,ASSET_CLASS,22)
!
                  MONTH_VARS(:,CashServiceTransactions) =
     +                    MONTH_VARS(:,CashServiceTransactions) +
     +                           ST_ANN_CLASS_EXPENSE(:,ASSET_CLASS,23)
!
                  MONTH_VARS(:,CashEmissionCredits) =
     +                    MONTH_VARS(:,CashEmissionCredits) +
     +                           ST_ANN_CLASS_EXPENSE(:,ASSET_CLASS,24)
!
                  MONTH_VARS(:,CashDOEDecommissioning) =
     +                    MONTH_VARS(:,CashDOEDecommissioning) +
     +                           ST_ANN_CLASS_EXPENSE(:,ASSET_CLASS,25)
!
                  MONTH_VARS(:,CashDOEDisposal) =
     +                    MONTH_VARS(:,CashDOEDisposal) +
     +                           ST_ANN_CLASS_EXPENSE(:,ASSET_CLASS,26)
!
                  MONTH_VARS(:,cash_catawba_expenses) =
     +                    MONTH_VARS(:,cash_catawba_expenses) +
     +                           ST_ANN_CLASS_EXPENSE(:,ASSET_CLASS,27)
!
                  MONTH_VARS(:,CashBTLExpenses) =
     +                   MONTH_VARS(:,CashBTLExpenses) +
     +                           ST_ANN_CLASS_EXPENSE(:,ASSET_CLASS,28)
!
                  MONTH_VARS(:,Cash_BTL_Lease_Cash) =
     +                    MONTH_VARS(:,Cash_BTL_Lease_Cash) +
     +                              ST_BTL_LEASE_PAYMENT(:,ASSET_CLASS)

            ENDIF
         ENDIF
      RETURN
!******************************************************************
      ENTRY SERVICE_TRANS_INFO(R_CLASS,
     +                         R_FUEXP,R_PREXP,R_OPEXP,
     +                         R_MNEXP,R_OTHER1,R_OTHER2,
     +                         R_OTHER3,R_NFOWN,R_NFLEASE,
     +                         R_ADJ_EXP,
     +                         R_NF_RATEBASE,R_DSM_EXPENSE,
     +                         R_DSM_REBATE,
     +                         R_ADJUSTMENT_CLAUSE_REVENUES,
     +                         R_BASE_RATES_REVENUES,
     +                         R_SECONDARY_SALES_REVENUES,
     +                         R_OTHER_REVENUES,
     +                         R_BTL_REVENUES,
     +                         R_BTL_EXPENSE,
     +                         R_ATL_LEASE_EXP,
     +                         R_BTL_LEASE_EXP,
     +                         R_SERVICE_TRANSACTIONS,
     +                         R_EMISSION_CREDITS,
     +                         R_DOE_DISPOSAL,
     +                         R_DOE_DECOMMISSIONING,
     +                         R_CATAWBA_REVENUES,
     +                         R_CATAWBA_EXPENSES,
     +                         R_CAPACITY_REVENUES)
!******************************************************************
!
         IF(SERVICE_TRANS <= 0) RETURN
!
         IF(R_CLASS <= MAX_SERVICE_CLASS_ID_NUM) THEN
            ASSET_CLASS = R_CLASS
            IF(ASSET_CLASS > 0 .OR. R_CLASS == -1) THEN
               MO = 0
!
! REVENUES
!
               R_BASE_RATES_REVENUES = R_BASE_RATES_REVENUES +
     +                           ST_ANN_CLASS_REVENUE(MO,ASSET_CLASS,1)
               R_ADJUSTMENT_CLAUSE_REVENUES =
     +                                   R_ADJUSTMENT_CLAUSE_REVENUES +
     +                           ST_ANN_CLASS_REVENUE(MO,ASSET_CLASS,2)
              R_SECONDARY_SALES_REVENUES = R_SECONDARY_SALES_REVENUES +
     +                           ST_ANN_CLASS_REVENUE(MO,ASSET_CLASS,3)
               R_OTHER_REVENUES = R_OTHER_REVENUES +
     +                           ST_ANN_CLASS_REVENUE(MO,ASSET_CLASS,4)
               R_BTL_REVENUES = R_BTL_REVENUES +
     +                           ST_ANN_CLASS_REVENUE(MO,ASSET_CLASS,5)
               R_CATAWBA_REVENUES = R_CATAWBA_REVENUES +
     +                           ST_ANN_CLASS_REVENUE(MO,ASSET_CLASS,6)
               R_CAPACITY_REVENUES = R_CAPACITY_REVENUES +
     +                          ST_ANN_CLASS_REVENUE(MO,ASSET_CLASS,17)
!
! EXPENSES
!
               R_FUEXP = R_FUEXP +
     +                          ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS,11)
               R_PREXP = R_PREXP +
     +                          ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS,12)
               R_OPEXP = R_OPEXP +
     +                          ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS,13)
               R_MNEXP = R_MNEXP +
     +                          ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS,14)
               R_OTHER1 = R_OTHER1 +
     +                          ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS,15)
               R_OTHER2 = R_OTHER2 +
     +                          ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS,16)
               R_OTHER3 = R_OTHER3 +
     +                          ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS,17)
               R_NFOWN = R_NFOWN +
     +                          ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS,18)
               R_NFLEASE = R_NFLEASE +
     +                          ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS,19)
               R_DSM_EXPENSE = R_DSM_EXPENSE +
     +                          ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS,20)
               R_DSM_REBATE =  R_DSM_REBATE +
     +                          ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS,21)
               R_ATL_LEASE_EXP = R_ATL_LEASE_EXP +
     +                          ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS,22)
!
               R_SERVICE_TRANSACTIONS = R_SERVICE_TRANSACTIONS +
     +                          ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS,23)
               R_EMISSION_CREDITS = R_EMISSION_CREDITS +
     +                          ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS,24)
               R_DOE_DECOMMISSIONING = R_DOE_DECOMMISSIONING +
     +                          ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS,25)
               R_DOE_DISPOSAL =  R_DOE_DISPOSAL +
     +                          ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS,26)
               R_CATAWBA_EXPENSES = R_CATAWBA_EXPENSES +
     +                          ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS,27)
               R_BTL_EXPENSE = R_BTL_EXPENSE +
     +                          ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS,28)
!
! SPECIAL EXPENSE ITEMS
!
               R_ADJ_EXP = R_ADJ_EXP +
     +                          ST_ANN_CLASS_ADJ_CLAUSE(MO,ASSET_CLASS)
               R_BTL_LEASE_EXP = R_BTL_LEASE_EXP +
     +                             ST_BTL_LEASE_PAYMENT(MO,ASSET_CLASS)
               R_NF_RATEBASE = R_NF_RATEBASE +
     +                                   ST_NF_RATEBASE(MO,ASSET_CLASS)
            ENDIF
         ENDIF
      RETURN
!******************************************************************
      ENTRY SET_UP_ST_CLASS_ARRAYS
!******************************************************************
!
         IF(ALLOCATED(ST_ANN_CLASS_ATL_EXPENSE))
     +                     DEALLOCATE(ST_ANN_CLASS_ATL_EXPENSE,
     +                                ST_ANN_CLASS_BTL_EXPENSE,
     +                                ST_ANN_CLASS_ADJ_CLAUSE,
     +                                ST_BTL_LEASE_PAYMENT,
     +                                ST_NF_RATEBASE,
     +                                ST_ANN_CLASS_EXPENSE_CAPACITY,
     +                                ST_ANN_CLASS_EXPENSE_ENERGY,
     +                                ST_ANN_CLASS_REVENUE_ENERGY,
     +                                ST_ANN_CLASS_REVENUE_CAPACITY,
     +                                ST_ANN_CLASS_REVENUE,
     +                                ST_ANN_CLASS_EXPENSE)
         ALLOCATE(ST_ANN_CLASS_ATL_EXPENSE(0:12,
     +                                    -1:MAX_SERVICE_CLASS_ID_NUM),
     +        ST_ANN_CLASS_BTL_EXPENSE(0:12,
     +                                    -1:MAX_SERVICE_CLASS_ID_NUM),
     +       ST_ANN_CLASS_ADJ_CLAUSE(0:12,-1:MAX_SERVICE_CLASS_ID_NUM),
     +        ST_BTL_LEASE_PAYMENT(0:12,-1:MAX_SERVICE_CLASS_ID_NUM),
     +        ST_NF_RATEBASE(0:12,-1:MAX_SERVICE_CLASS_ID_NUM),
     +        ST_ANN_CLASS_EXPENSE_CAPACITY(0:12,
     +                                     0:MAX_SERVICE_CLASS_ID_NUM),
     +        ST_ANN_CLASS_EXPENSE_ENERGY(0:12,
     +                                     0:MAX_SERVICE_CLASS_ID_NUM),
     +        ST_ANN_CLASS_REVENUE_ENERGY(0:12,
     +                                     0:MAX_SERVICE_CLASS_ID_NUM),
     +        ST_ANN_CLASS_REVENUE_CAPACITY(0:12,
     +                                     0:MAX_SERVICE_CLASS_ID_NUM),
     +        ST_ANN_CLASS_REVENUE(0:12,-1:MAX_SERVICE_CLASS_ID_NUM,
     +                                               LAST_INCOME_LINE),
     +        ST_ANN_CLASS_EXPENSE(0:12,-1:MAX_SERVICE_CLASS_ID_NUM,
     +                                              LAST_EXPENSE_ITEM))
!
         IF(ALLOCATED(ASSET_CLASS_LIST))
     +               DEALLOCATE(ASSET_CLASS_LIST,ASSET_ALLOCATION_LIST)
         ALLOCATE(ASSET_CLASS_LIST(AVAIL_DATA_YEARS),
     +                         ASSET_ALLOCATION_LIST(AVAIL_DATA_YEARS))
      RETURN
      END
!**********************************************************************
!
!                   SERVICE TRANSACTIONS ALLOCATION MODULE
!                              COPYRIGHT (C) 1992
!                        M.S. GERBER & ASSOCIATES, INC.
!                              ALL RIGHTS RESERVED
!
!**********************************************************************
!
      SUBROUTINE ALLOCATE_SERVICE_COSTS(TYPE_OF_SERVICE,SERVICE_CHARGE,
     +                                  SERVICE_COST_ASSIGNMENT,
     +                                  SERVICE_EXPENSE_COLLECTION,
     +                                  SERVICE_REPORTING_GROUP)
!
      use servcom

      CHARACTER (len=1) ::  TYPE_OF_SERVICE,
     +            SERVICE_COST_ASSIGNMENT
      CHARACTER (len=3) ::  SERVICE_EXPENSE_COLLECTION
      INTEGER (kind=2) ::  SERVICE_REPORTING_GROUP,SERVICE_ITEM
      REAL (kind=4) ::  SERVICE_CHARGE
!
      IF(SERVICE_COST_ASSIGNMENT == 'R') THEN
         SERVICE_REVENUES = SERVICE_REVENUES + SERVICE_CHARGE
         IF(INDEX(SERVICE_EXPENSE_COLLECTION,'Bas') /= 0) THEN
            SERVICE_BASE_REVENUE_OFFSET = SERVICE_BASE_REVENUE_OFFSET +
     +                                                   SERVICE_CHARGE
         ENDIF
         IF(INDEX(SERVICE_EXPENSE_COLLECTION,'Adj') /= 0) THEN
            SERVICE_ADJ_CLAUSE_OFFSET = SERVICE_ADJ_CLAUSE_OFFSET +
     +                                                   SERVICE_CHARGE
         ENDIF
      ELSE
!
         SELECT CASE (TYPE_OF_SERVICE)
         CASE ('T')
           TRANSMISSION_CHARGES = TRANSMISSION_CHARGES + SERVICE_CHARGE
         CASE ('S')
           STAND_BY_TRANS_CHARGES=STAND_BY_TRANS_CHARGES+SERVICE_CHARGE
         CASE ('W')
            WHEELING_CHARGES = WHEELING_CHARGES + SERVICE_CHARGE
         CASE ('D')
            DISPATCHING_CHARGES = DISPATCHING_CHARGES + SERVICE_CHARGE
         CASE ('O')
           OTHER_SERVICE_CHARGES = OTHER_SERVICE_CHARGES+SERVICE_CHARGE
         END SELECT
!
         SERVICE_EXPENSES = SERVICE_EXPENSES + SERVICE_CHARGE
         IF(INDEX(SERVICE_EXPENSE_COLLECTION,'Adj') /= 0) THEN
           SERVICE_ADJ_CLAUSE_EXPENSES = SERVICE_ADJ_CLAUSE_EXPENSES +
     +                                                   SERVICE_CHARGE
         ENDIF
         IF(INDEX(SERVICE_EXPENSE_COLLECTION,'Bas') /= 0) THEN
             SERVICE_BASE_RATE_EXPENSES = SERVICE_BASE_RATE_EXPENSES +
     +                                                   SERVICE_CHARGE
         ENDIF
      ENDIF
      SERVICE_ITEM = INDEX('TSWDO',TYPE_OF_SERVICE)
      IF(SERVICE_ITEM /= 0 .AND.
     +       SERVICE_REPORTING_GROUP > 0  .AND.
     +              SERVICE_REPORTING_GROUP <= MAX_SERVICE_GROUPS) THEN
         SERVICE_GROUP_COSTS(SERVICE_ITEM,SERVICE_REPORTING_GROUP) =
     +         SERVICE_CHARGE +
     +        SERVICE_GROUP_COSTS(SERVICE_ITEM,SERVICE_REPORTING_GROUP)
         SERVICE_GROUP_COSTS(0,SERVICE_REPORTING_GROUP) =
     +                SERVICE_CHARGE +
     +                   SERVICE_GROUP_COSTS(0,SERVICE_REPORTING_GROUP)
      ENDIF
      RETURN
      END
!*************************************************************
      SUBROUTINE TRANSACTION_MANAGER(R_SEASON,
     +                               R_TRANS_NO,
     +                               R_COST_ASSIGNMENT,
     +                               R_TRANSACTION_TYPE,
     +                               R_TRANSACTION_DESCRIPTION,
     +                               R_ENERGY,
     +                               R_ENERGY_CHARGE,
     +                               R_UNIT_ENERGY_COST,
     +                               R_CAPACITY,
     +                               R_CAPACITY_CHARGE,
     +                               R_UNIT_CAPACITY_COST,
     +                               R_EXPENSE_COLLECTION,
     +                               R_REPORTING_GROUP,
     +                               R_MONTHLY_REPORT_ACTIVE,
     +                               R_WRITE_IT_MONTHLY,
     +                               RATE_TRACKER,
     +                               RES_TRACKER,
     +                               FUEL_TRACKER,
     +                               MEM_TRACKER,
     +                               R_SERVICE_EXPENSE_CLASS)
!*************************************************************
!
      use SpinDriftLib
      use prod_arrays_dimensions
      USE IREC_ENDPOINT_CONTROL
      use grx_planning_routines
      USE SIZECOM
      use globecom
      use servcom

         LOGICAL (kind=1) ::    SERVICE_TRANS_REPORT_NOT_OPEN/.TRUE./,
     +               TRANSACTION_ACTIVE(:),FALSE_BYTE,
     +               MONTHLY_REPORT_ACTIVE,R_MONTHLY_REPORT_ACTIVE,
     +               WRITE_IT_MONTHLY,R_WRITE_IT_MONTHLY
         INTEGER (kind=2) ::    SEASON
         INTEGER (kind=2) ::    R_SEASON
         INTEGER (kind=2) ::    TRANS_RPT_NO,SERVICE_TRANS_HEADER,
     +               PRODUCTION_PERIODS,LAST_SEASON,I,
     +               NO_TRANS_TYPES/5/,TRANS_TYPE_NO,COST_NO,
     +               NO_OF_TRANS/0/,TRANS_NO,R_TRANS_NO,
     +               TOTAL_ANNUAL_RECORDS,COST_POSITION,
     +               REPORTING_GROUP,R_REPORTING_GROUP,
     +               TYPE,
     +               TRACKER,
     +               R_TYPE,
     +               R_TRACKER_TYPE,
     +               R_COST
!     +               GET_SERVI_RES_TRACKER_INDEX,
!     +               GET_SERVI_FUEL_TRACKER_INDEX
         REAL (kind=4) ::  RES_TRACKER
         REAL (kind=4) ::  FUEL_TRACKER,RATE_TRACKER,MEM_TRACKER
         INTEGER ::  TRANS_RPT_REC
         CHARACTER (len=1) ::  COST_ASSIGNMENT,R_COST_ASSIGNMENT,
     +               TRANSACTION_TYPE,R_TRANSACTION_TYPE
         CHARACTER (len=3) ::  R_EXPENSE_COLLECTION,EXPENSE_COLLECTION
         CHARACTER (len=7) ::  COST_ASSIGNMENT_NAME,
     +                  ASSIGNMENT_NAME(2)/'Revenue','Expense'/
     +
         CHARACTER (len=9) ::  CL_MONTH_NAME(13)
         CHARACTER (len=12) ::  TRANS_TYPE,TRANS_TYPE_NAME(5)/
     +                     'Transmission','Stand-By','Wheeling',
     +                                    'Dispatching','Other'/
         CHARACTER (len=1) ::  TRAN_TYPE(:)
         CHARACTER (len=1) ::  COST_ASSIGN(:),EXPENSE_ASSIGN(:)
        CHARACTER (len=20) ::  TRANSACTION_DESCRIPTION
        CHARACTER (len=20) ::  R_TRANSACTION_DESCRIPTION,
     +                  MONTH_NAME,TRANS_NAME(:),
     +                  R_SERVICE_EXPENSE_CLASS,
     +                  WVPA_COST_ASSIGN(2)/'Total Service Rev.  ',
     +                                      'Total Service Cost  '/
         REAL (kind=4) ::   ENERGY_CHARGE,R_ENERGY_CHARGE,
     +         VARIABLE_OM_CHARGE,
     +         CAPACITY_CHARGE,R_CAPACITY_CHARGE,
     +         UNIT_ENERGY_COST,R_UNIT_ENERGY_COST,
     +         UNIT_CAPACITY_COST,R_UNIT_CAPACITY_COST,
     +         IMPUTTED_ENERGY,IMPUTTED_CAPACITY,
     +         SEASONAL_TYPE_ELEMENTS(4,5,2),SEASONAL_COST_ELEMENTS(6),
     +         ANNUAL_COSTS(:,:),
     +         TEMP_R,
     +        ENERGY,R_ENERGY,CAPACITY,R_CAPACITY,SUM_REPORT_ITEMS(20),
     +         SEASONAL_FUEL_ENERGY,SEASONAL_FUEL_COST,
     +         ANNUAL_FUEL_ENERGY,ANNUAL_FUEL_COST,
     +         R_MM_DB_BY_SEASON(0:12,0:5)
!
         ALLOCATABLE::  ANNUAL_COSTS,TRANS_NAME,COST_ASSIGN,TRAN_TYPE,
     +                  TRANSACTION_ACTIVE,EXPENSE_ASSIGN
!
         SAVE  TRANS_RPT_NO,CL_MONTH_NAME,SEASONAL_TYPE_ELEMENTS,
     +         ANNUAL_COSTS,LAST_SEASON,TRANS_NAME,TRAN_TYPE,
     +         COST_ASSIGN,TOTAL_ANNUAL_RECORDS,TRANSACTION_ACTIVE,
     +         EXPENSE_ASSIGN,
     +         SEASONAL_FUEL_ENERGY,SEASONAL_FUEL_COST,
     +         ANNUAL_FUEL_ENERGY,ANNUAL_FUEL_COST
         SAVE TRANS_RPT_REC
!
!
         CHARACTER (len=1) ::  WABASH_POWER_COST_RPT
         LOGICAL (kind=1) ::    PRINT_WABASH_COST_REPORT/.FALSE./,
     +               WABASH_REPORT_NOT_USED/.TRUE./,
     +               WVPA
         INTEGER (kind=2) ::    WABASH_REPORT_UNIT_NO/0/,
     +               WABASH_VALLEY_POWER_COST_NO
         INTEGER ::      WABASH_REC_COUNTER
         REAL (kind=4) ::      KW_CAPACITY
         REAL (kind=4) ::      CAP_FACTOR,AVE_VAR_COST,AVE_TOTAL_COST,
     +               RPT_CAPACITY,RPT_ENERGY,FUEL_CHARGE_RATE,
     +               FUEL_COST
         INTEGER (kind=2) ::  BASE_DATE,HISTORICAL_PRODUCTION_DATE
!
! END DATA DECLARATIONS
!
! STANDARD RESULTS MODULE STUFF
!
         BASE_DATE = (BASE_YEAR + YEAR - 1900) * 100
         SEASON = R_SEASON
         TRANS_NO = R_TRANS_NO
         COST_ASSIGNMENT = R_COST_ASSIGNMENT
         UNIT_ENERGY_COST = R_UNIT_ENERGY_COST
         UNIT_CAPACITY_COST = R_UNIT_CAPACITY_COST
         ENERGY_CHARGE = R_ENERGY_CHARGE
         CAPACITY_CHARGE = R_CAPACITY_CHARGE
         ENERGY = R_ENERGY
         CAPACITY = R_CAPACITY
         TRANSACTION_TYPE = R_TRANSACTION_TYPE
         EXPENSE_COLLECTION = R_EXPENSE_COLLECTION
         TRANSACTION_DESCRIPTION = R_TRANSACTION_DESCRIPTION
         REPORTING_GROUP = R_REPORTING_GROUP
         MONTHLY_REPORT_ACTIVE = R_MONTHLY_REPORT_ACTIVE
         WRITE_IT_MONTHLY = R_WRITE_IT_MONTHLY
!
!         RATE_TRACKER = FLOAT(TRACKER)

!
         CALL ALLOCATE_SERVICE_COSTS(TRANSACTION_TYPE,
     +                               ENERGY_CHARGE
     +                               + CAPACITY_CHARGE,
     +                               COST_ASSIGNMENT,
     +                               EXPENSE_COLLECTION,
     +                               REPORTING_GROUP)

         IF(.NOT. MONTHLY_REPORT_ACTIVE .OR.
     +                                   .NOT. WRITE_IT_MONTHLY) RETURN
!
         IF(WABASH_REPORT_NOT_USED) THEN
            WABASH_REPORT_UNIT_NO = WABASH_VALLEY_POWER_COST_NO()
            WABASH_REPORT_NOT_USED = .FALSE.
         ENDIF
!
         PRINT_WABASH_COST_REPORT = WVPA() .AND.
     +                        INDEX('M,B,A',WABASH_POWER_COST_RPT())/=0
!
         FALSE_BYTE = .FALSE.
         IF(COST_ASSIGNMENT == 'R') THEN
            COST_NO = 1
         ELSE
            COST_NO = 2
         ENDIF
         COST_ASSIGNMENT_NAME = ASSIGNMENT_NAME(COST_NO)
!
         IF(SERVICE_TRANS_REPORT_NOT_OPEN) THEN
            TRANS_RPT_NO = SERVICE_TRANS_HEADER(TRANS_RPT_REC)
            SERVICE_TRANS_REPORT_NOT_OPEN = .FALSE.
!
            LAST_SEASON = PRODUCTION_PERIODS()
            DO I = 1, LAST_SEASON
               CL_MONTH_NAME(I) = MONTH_NAME(I)
            ENDDO
            CL_MONTH_NAME(LAST_SEASON+1) = 'Annual'
!
            CALL GET_NO_OF_SERVICE_TRANS(NO_OF_TRANS)

         ENDIF
!
         TRANSACTION_ACTIVE(TRANS_NO) = .TRUE.
!
!
         IF(UNIT_CAPACITY_COST == 0.) THEN
            RPT_CAPACITY = 0.
         ELSE
            RPT_CAPACITY = CAPACITY
         ENDIF
         IF(UNIT_ENERGY_COST == 0.) THEN
            RPT_ENERGY = 0.
         ELSE
            RPT_ENERGY = ENERGY
         ENDIF
!
         IF(INDEX(EXPENSE_COLLECTION,'Adj') /= 0) THEN
            SEASONAL_FUEL_ENERGY = SEASONAL_FUEL_ENERGY + RPT_ENERGY
            SEASONAL_FUEL_COST = SEASONAL_FUEL_COST
     +                           + ENERGY_CHARGE
            ANNUAL_FUEL_ENERGY = ANNUAL_FUEL_ENERGY + RPT_ENERGY
            ANNUAL_FUEL_COST = ANNUAL_FUEL_COST
     +                         + ENERGY_CHARGE
            ANNUAL_COSTS(5,TRANS_NO) = ANNUAL_COSTS(5,TRANS_NO)
     +                                 + RPT_ENERGY
            ANNUAL_COSTS(6,TRANS_NO) = ANNUAL_COSTS(6,TRANS_NO)
     +                                 + ENERGY_CHARGE
         ELSE
            ANNUAL_COSTS(1,TRANS_NO) = ANNUAL_COSTS(1,TRANS_NO)
     +                                 + RPT_ENERGY
            ANNUAL_COSTS(2,TRANS_NO) = ANNUAL_COSTS(2,TRANS_NO)
     +                                 + ENERGY_CHARGE

         ENDIF
         ANNUAL_COSTS(3,TRANS_NO) = ANNUAL_COSTS(3,TRANS_NO)
     +                              + RPT_CAPACITY
         ANNUAL_COSTS(4,TRANS_NO) = ANNUAL_COSTS(4,TRANS_NO)
     +                              + CAPACITY_CHARGE
!
         TRANS_TYPE_NO = 0
         SELECT CASE (TRANSACTION_TYPE)
         CASE ('T')
            TRANS_TYPE_NO = 1
         CASE ('S')
            TRANS_TYPE_NO = 2
         CASE ('W')
            TRANS_TYPE_NO = 3
         CASE ('D')
            TRANS_TYPE_NO = 4
         CASE ('O')
            TRANS_TYPE_NO = 5
         END SELECT
         IF(TRANS_TYPE_NO > 0 .AND. TRANS_TYPE_NO < 6) THEN
            SEASONAL_TYPE_ELEMENTS(1,TRANS_TYPE_NO,COST_NO) =
     +         SEASONAL_TYPE_ELEMENTS(1,TRANS_TYPE_NO,COST_NO) +
     +                                                       RPT_ENERGY
            SEASONAL_TYPE_ELEMENTS(2,TRANS_TYPE_NO,COST_NO) =
     +                  SEASONAL_TYPE_ELEMENTS(2,TRANS_TYPE_NO,COST_NO)
     +                   + ENERGY_CHARGE
            SEASONAL_TYPE_ELEMENTS(3,TRANS_TYPE_NO,COST_NO) =
     +         SEASONAL_TYPE_ELEMENTS(3,TRANS_TYPE_NO,COST_NO) +
     +                                                     RPT_CAPACITY
            SEASONAL_TYPE_ELEMENTS(4,TRANS_TYPE_NO,COST_NO) =
     +         SEASONAL_TYPE_ELEMENTS(4,TRANS_TYPE_NO,COST_NO) +
     +                                                  CAPACITY_CHARGE
            TRANS_TYPE = TRANS_TYPE_NAME(TRANS_TYPE_NO)
         ELSE
            TRANS_TYPE = ' '
         ENDIF
!
! 05/20/03. NOTE THE SUMS.
!
         IF(INDEX(EXPENSE_COLLECTION,'Adj') /= 0) THEN
               FUEL_COST = ENERGY_CHARGE
               VARIABLE_OM_CHARGE = 0.
            ELSE
               FUEL_CHARGE_RATE = 0.
               VARIABLE_OM_CHARGE = ENERGY_CHARGE
            ENDIF
!
         TYPE = 4 ! SERVICE TRANSACTION TYPE
!
!         CALL GET_STRANS_TRACKER_INDEX(R_TRANS_NO,TRACKER)
!
         IF(BASE_DATE + R_SEASON >= HISTORICAL_PRODUCTION_DATE() .AND.
     +         INDEX(R_SERVICE_EXPENSE_CLASS,
     +                                  'Service') /= 0 .AND.
     +                                          COST_ASSIGNMENT /= 'R')
     +       CALL WVPA_STORE_CL_TRACKER_DATE_BASE(R_SEASON,
     +                                        TYPE,
     +                                  INT2(FUEL_TRACKER), ! TRACKER,
     +                                        1000.*FUEL_COST,
     +                                        1000.*VARIABLE_OM_CHARGE,
     +                                        1000.*CAPACITY_CHARGE,
     +                                        INT2(MEM_TRACKER))
!
         IF(SEASON < 13 .AND. PRINT_WABASH_COST_REPORT) THEN
            IF(UNIT_CAPACITY_COST == 0.) THEN
               RPT_CAPACITY = 0.
            ELSE
               RPT_CAPACITY = CAPACITY
            ENDIF
            IF(UNIT_ENERGY_COST == 0.) THEN
               RPT_ENERGY = 0.
            ELSE
               RPT_ENERGY = ENERGY
            ENDIF
            KW_CAPACITY = RPT_CAPACITY*1000.
            CAP_FACTOR = 0.
            IF(RPT_ENERGY > 0.) THEN
               AVE_VAR_COST = 1000.*ENERGY_CHARGE/RPT_ENERGY
               AVE_TOTAL_COST = 1000.*
     +                       (ENERGY_CHARGE+CAPACITY_CHARGE)/RPT_ENERGY
            ELSE
               AVE_VAR_COST = 0.
               AVE_TOTAL_COST = 0.
            ENDIF
            IF(INDEX(EXPENSE_COLLECTION,'Adj') /= 0) THEN
               FUEL_CHARGE_RATE = UNIT_ENERGY_COST
               FUEL_COST = ENERGY_CHARGE
               UNIT_ENERGY_COST = 0.
               ENERGY_CHARGE = 0.
            ELSE
               FUEL_CHARGE_RATE = 0.
               FUEL_COST = 0.
            ENDIF
            INQUIRE(UNIT=WABASH_REPORT_UNIT_NO,
     +                                      NEXTREC=WABASH_REC_COUNTER)
            WRITE(WABASH_REPORT_UNIT_NO,REC=WABASH_REC_COUNTER)
     +                     PRT_ENDPOINT(),
     +                     FLOAT(YEAR+BASE_YEAR),
     +                     CL_MONTH_NAME(SEASON),
     +                     TRANSACTION_DESCRIPTION,
     +                     KW_CAPACITY,
     +                     RPT_ENERGY,
     +                     0.,
     +                     KW_CAPACITY,
     +                     RPT_ENERGY,
     +                     UNIT_CAPACITY_COST,
     +                     CAPACITY_CHARGE,
     +                     FUEL_CHARGE_RATE,
     +                     FUEL_COST,
     +                     UNIT_ENERGY_COST,
     +                     ENERGY_CHARGE,
     +                     FUEL_COST+ENERGY_CHARGE+CAPACITY_CHARGE,
     +                     KW_CAPACITY,
     +                     KW_CAPACITY,
     +                     0.,
     +                     CAP_FACTOR,
     +                     AVE_TOTAL_COST,
     +                     AVE_VAR_COST,
     +                     RES_TRACKER,
     +                     FUEL_TRACKER,
     +                     RATE_TRACKER,
     +                     MEM_TRACKER,
     +                     0.,  ! EMISSION AMOUNT
     +                     0.   ! AVERAGE EMISSION RATE
         ENDIF
         IF(SEASON < 13) THEN
            WRITE(TRANS_RPT_NO,REC=TRANS_RPT_REC)
     +                     PRT_ENDPOINT(),FLOAT(YEAR+BASE_YEAR),
     +                     CL_MONTH_NAME(SEASON),
     +                     TRANSACTION_DESCRIPTION,
     +                     COST_ASSIGNMENT_NAME,
     +                     TRANS_TYPE,
     +                     ENERGY,
     +                     UNIT_ENERGY_COST,ENERGY_CHARGE,
     +                     CAPACITY,
     +                     UNIT_CAPACITY_COST,CAPACITY_CHARGE,
     +                     ENERGY_CHARGE+CAPACITY_CHARGE
            TRANS_RPT_REC = TRANS_RPT_REC + 1
         ENDIF
      RETURN
!
!******************************************************************
      ENTRY INIT_TRANSACTION_MANAGER
!******************************************************************
! 11/6/01. GAT. MOVED FOR MCKEE.
!
            CALL GET_NO_OF_SERVICE_TRANS(NO_OF_TRANS)
            IF(NO_OF_TRANS > 0) THEN
               IF(ALLOCATED(ANNUAL_COSTS)) DEALLOCATE(ANNUAL_COSTS)
               TOTAL_ANNUAL_RECORDS =
     +                             NO_OF_TRANS + 2*(NO_TRANS_TYPES + 1)
               ALLOCATE(ANNUAL_COSTS(6,TOTAL_ANNUAL_RECORDS))
               ANNUAL_COSTS = 0.
!
               IF(ALLOCATED(TRANS_NAME))
     +                     DEALLOCATE(TRANS_NAME,COST_ASSIGN,
     +                                 EXPENSE_ASSIGN,TRAN_TYPE,
     +                                 TRANSACTION_ACTIVE)
               ALLOCATE(TRANS_NAME(NO_OF_TRANS),
     +               COST_ASSIGN(NO_OF_TRANS),
     +               EXPENSE_ASSIGN(NO_OF_TRANS),
     +               TRAN_TYPE(NO_OF_TRANS),
     +               TRANSACTION_ACTIVE(NO_OF_TRANS))
               CALL GET_TRANS_VARIABLES(NO_OF_TRANS,TRANS_NAME,
     +                    COST_ASSIGN,EXPENSE_ASSIGN,TRAN_TYPE)
               TRANSACTION_ACTIVE = FALSE_BYTE
!
! 05/20/03.
!
!
            ENDIF
!
      RETURN
!******************************************************************
      ENTRY INIT_TRANS_SEASON_TYPE
!******************************************************************
         SEASONAL_TYPE_ELEMENTS = 0.
         SEASONAL_FUEL_ENERGY = 0.
         SEASONAL_FUEL_COST = 0.
      RETURN
!******************************************************************
      ENTRY WRITE_TRANS_SEASON_TYPE(R_SEASON)
!******************************************************************
         IF(SERVICE_TRANS_REPORT_NOT_OPEN) RETURN
         SEASON = R_SEASON
! 11/26/03. MOVED TO ACCOMODATE REVENUES
         SEASONAL_COST_ELEMENTS(1) = 0.
         SEASONAL_COST_ELEMENTS(2) = 0.
         SEASONAL_COST_ELEMENTS(3) = 0.
         SEASONAL_COST_ELEMENTS(4) = 0.
         SEASONAL_COST_ELEMENTS(5) = SEASONAL_FUEL_ENERGY
         SEASONAL_COST_ELEMENTS(6) = SEASONAL_FUEL_COST
!
         DO COST_NO = 1 , 2
            IF(COST_NO == 1) THEN
               TEMP_R = -1.0
            ELSE
               TEMP_R =  1.0
            ENDIF
            DO I = 1 , NO_TRANS_TYPES

               IF(SEASONAL_TYPE_ELEMENTS(1,I,COST_NO) > 0.) THEN
                  IMPUTTED_ENERGY =
     +                  1000.*SEASONAL_TYPE_ELEMENTS(2,I,COST_NO)/
     +                              SEASONAL_TYPE_ELEMENTS(1,I,COST_NO)
               ELSE
                  IMPUTTED_ENERGY = 0.
               ENDIF
               IF(SEASONAL_TYPE_ELEMENTS(3,I,COST_NO) > 0.) THEN
                  IMPUTTED_CAPACITY =
     +                  SEASONAL_TYPE_ELEMENTS(4,I,COST_NO)/
     +                              SEASONAL_TYPE_ELEMENTS(3,I,COST_NO)
               ELSE
                  IMPUTTED_CAPACITY = 0.
               ENDIF
               WRITE(TRANS_RPT_NO,REC=TRANS_RPT_REC) PRT_ENDPOINT(),
     +                  FLOAT(YEAR+BASE_YEAR),
     +                  CL_MONTH_NAME(SEASON),
     +                  'Total               ',
     +                  ASSIGNMENT_NAME(COST_NO),
     +                  TRANS_TYPE_NAME(I),
     +                  SEASONAL_TYPE_ELEMENTS(1,I,COST_NO),
     +                  IMPUTTED_ENERGY,
     +                  SEASONAL_TYPE_ELEMENTS(2,I,COST_NO),
     +                  SEASONAL_TYPE_ELEMENTS(3,I,COST_NO),
     +                  IMPUTTED_CAPACITY,
     +                  SEASONAL_TYPE_ELEMENTS(4,I,COST_NO),
     +                  SEASONAL_TYPE_ELEMENTS(2,I,COST_NO)+
     +                              SEASONAL_TYPE_ELEMENTS(4,I,COST_NO)
               TRANS_RPT_REC = TRANS_RPT_REC + 1
               SEASONAL_COST_ELEMENTS(1) =
     +                    SEASONAL_COST_ELEMENTS(1) +
     +                       SEASONAL_TYPE_ELEMENTS(1,I,COST_NO)*TEMP_R
               SEASONAL_COST_ELEMENTS(2) =
     +                    SEASONAL_COST_ELEMENTS(2) +
     +                       SEASONAL_TYPE_ELEMENTS(2,I,COST_NO)*TEMP_R
               SEASONAL_COST_ELEMENTS(3) =
     +                    SEASONAL_COST_ELEMENTS(3) +
     +                       SEASONAL_TYPE_ELEMENTS(3,I,COST_NO)*TEMP_R
               SEASONAL_COST_ELEMENTS(4) =
     +                    SEASONAL_COST_ELEMENTS(4) +
     +                       SEASONAL_TYPE_ELEMENTS(4,I,COST_NO)*TEMP_R
            ENDDO ! TRANSACTIONS COUNTER
            IF(ABS(SEASONAL_COST_ELEMENTS(1) +
     +                                 SEASONAL_COST_ELEMENTS(5))
     +                                                     > 0.01) THEN
               IMPUTTED_ENERGY = 1000.*
     +               (SEASONAL_COST_ELEMENTS(2) +
     +                                      SEASONAL_COST_ELEMENTS(6))/
     +                      (SEASONAL_COST_ELEMENTS(1) +
     +                                       SEASONAL_COST_ELEMENTS(5))
            ELSE
               IMPUTTED_ENERGY = 0.
            ENDIF
            IF( ABS(SEASONAL_COST_ELEMENTS(3)) > 0.01) THEN
               IMPUTTED_CAPACITY =
     +              SEASONAL_COST_ELEMENTS(4)/SEASONAL_COST_ELEMENTS(3)
            ELSE
               IMPUTTED_CAPACITY = 0.
            ENDIF
            WRITE(TRANS_RPT_NO,REC=TRANS_RPT_REC) PRT_ENDPOINT(),
     +                  FLOAT(YEAR+BASE_YEAR),
     +                  CL_MONTH_NAME(SEASON),
     +                  'Total               ',
     +                  ASSIGNMENT_NAME(COST_NO),
     +                  'Total       ',
     +                  SEASONAL_COST_ELEMENTS(1),
     +                  IMPUTTED_ENERGY,
     +                  SEASONAL_COST_ELEMENTS(2),
     +                  SEASONAL_COST_ELEMENTS(3),
     +                  IMPUTTED_CAPACITY,
     +                  SEASONAL_COST_ELEMENTS(4),
     +                  SEASONAL_COST_ELEMENTS(2)+
     +                               SEASONAL_COST_ELEMENTS(4)
            TRANS_RPT_REC = TRANS_RPT_REC + 1
            IF(PRINT_WABASH_COST_REPORT .AND. COST_NO == 2) THEN
               KW_CAPACITY =  SEASONAL_COST_ELEMENTS(3)*1000.
               IF(KW_CAPACITY > 0.) THEN
                  UNIT_CAPACITY_COST = SEASONAL_COST_ELEMENTS(4)/
     +                                        SEASONAL_COST_ELEMENTS(3)
               ELSE
                  UNIT_CAPACITY_COST = 0.
               ENDIF
               CAP_FACTOR = 0.
               IF( SEASONAL_COST_ELEMENTS(1) > 0.) THEN
                  AVE_VAR_COST = 1000.* SEASONAL_COST_ELEMENTS(2)/
     +                                        SEASONAL_COST_ELEMENTS(1)
                  AVE_TOTAL_COST = 1000.*
     +                            (SEASONAL_COST_ELEMENTS(2) +
     +                                 SEASONAL_COST_ELEMENTS(4))/
     +                                       SEASONAL_COST_ELEMENTS(1)
               ELSE
                  AVE_VAR_COST = 0.
                  AVE_TOTAL_COST = 0.
               ENDIF
               IF( SEASONAL_COST_ELEMENTS(5) > 0.) THEN
                  FUEL_CHARGE_RATE = 1000.* SEASONAL_COST_ELEMENTS(6)/
     +                                        SEASONAL_COST_ELEMENTS(5)
               ELSE
                  FUEL_CHARGE_RATE = 0.
               ENDIF
               IF(SEASONAL_COST_ELEMENTS(1) -
     +                             SEASONAL_COST_ELEMENTS(5) > 0.) THEN
                  UNIT_ENERGY_COST = (SEASONAL_COST_ELEMENTS(2) -
     +                              SEASONAL_COST_ELEMENTS(6)) /
     +                          (SEASONAL_COST_ELEMENTS(1) -
     +                                       SEASONAL_COST_ELEMENTS(5))
               ELSE
                  UNIT_ENERGY_COST = 0.
               ENDIF
!
! TOTAL TRANSACTIONS BY SEASON
!
               INQUIRE(UNIT=WABASH_REPORT_UNIT_NO,
     +                                      NEXTREC=WABASH_REC_COUNTER)
               WRITE(WABASH_REPORT_UNIT_NO,REC=WABASH_REC_COUNTER)
     +                     PRT_ENDPOINT(),
     +                     FLOAT(YEAR+BASE_YEAR),
     +                     CL_MONTH_NAME(SEASON),
     +                WVPA_COST_ASSIGN(COST_NO),   !TOTAL SERVICE COST
     +                     KW_CAPACITY,
     +                     SEASONAL_COST_ELEMENTS(1),
     +                     0.,
     +                     KW_CAPACITY,
     +                     SEASONAL_COST_ELEMENTS(1),
     +                     UNIT_CAPACITY_COST,
     +                 SEASONAL_COST_ELEMENTS(4), ! TOTAL CAPACITY COST
     +                     FUEL_CHARGE_RATE, ! FUEL $/MWH
     +                     SEASONAL_COST_ELEMENTS(6), ! FUEL COST $
     +                     UNIT_ENERGY_COST, ! ENERGY CHARGE $/MWH
     +                     SEASONAL_COST_ELEMENTS(2) -
     +                           SEASONAL_COST_ELEMENTS(6), ! ENERGY $
     +                     SEASONAL_COST_ELEMENTS(2) +
     +                          SEASONAL_COST_ELEMENTS(4), ! TOTAL COST
     +                     KW_CAPACITY,
     +                     KW_CAPACITY,
     +                     0.,
     +                     CAP_FACTOR,
     +                     AVE_TOTAL_COST,
     +                     AVE_VAR_COST,
     +                     0., ! RES_TRACKER,
     +                     0., ! FUEL_TRACKER
     +                     0.,  ! RATE_TRACKER
     +                     0.,  ! MEM_TRACKER
     +                     0.,  ! EMISSION AMOUNT
     +                     0.   ! AVERAGE EMISSION RATE
            ENDIF
         ENDDO
         IF(PRINT_WABASH_COST_REPORT) THEN
            CALL RETURN_SUM_REPORT_ITEMS(SUM_REPORT_ITEMS)
            IF(SUM_REPORT_ITEMS(1) > 0.) THEN
               UNIT_CAPACITY_COST = 1000.*(SUM_REPORT_ITEMS(7) +
     +                   SEASONAL_COST_ELEMENTS(4))/SUM_REPORT_ITEMS(1)
            ELSE
               UNIT_CAPACITY_COST = 0.
            ENDIF
            IF(SUM_REPORT_ITEMS(5) > 0.) THEN
               UNIT_ENERGY_COST =
     +                     1000.*(SEASONAL_COST_ELEMENTS(2) +
     +                                      SUM_REPORT_ITEMS(11))/
     +                                              SUM_REPORT_ITEMS(5)
               AVE_VAR_COST = 1000.*(SEASONAL_COST_ELEMENTS(2) +
     +                SUM_REPORT_ITEMS(9) + SUM_REPORT_ITEMS(11))/
     +                                              SUM_REPORT_ITEMS(2)
               AVE_TOTAL_COST =
     +                     1000.*(SEASONAL_COST_ELEMENTS(2) +
     +                           SEASONAL_COST_ELEMENTS(4) +
     +                                SUM_REPORT_ITEMS(12))/
     +                                              SUM_REPORT_ITEMS(2)
            ELSE
               UNIT_ENERGY_COST = 0.
               AVE_VAR_COST = 0.
               AVE_TOTAL_COST = 0.
            ENDIF
            INQUIRE(UNIT=WABASH_REPORT_UNIT_NO,
     +                                      NEXTREC=WABASH_REC_COUNTER)
            WRITE(WABASH_REPORT_UNIT_NO,REC=WABASH_REC_COUNTER)
     +                     PRT_ENDPOINT(),
     +                     FLOAT(YEAR+BASE_YEAR),
     +                     CL_MONTH_NAME(SEASON),
     +                     'Total Cost          ',
     +                     SUM_REPORT_ITEMS(1),             !(1)
     +                     SUM_REPORT_ITEMS(2),             !(2)
     +                     SUM_REPORT_ITEMS(3),             !(3)
     +                     SUM_REPORT_ITEMS(4),             !(4)
     +                     SUM_REPORT_ITEMS(5),             !(5)
     +                     UNIT_CAPACITY_COST,              !(6)!
     +                     SEASONAL_COST_ELEMENTS(4) +
     +                              SUM_REPORT_ITEMS(7),    !(7)
     +              SUM_REPORT_ITEMS(8),             !(8) ! FUEL $/MWH
     +           SEASONAL_COST_ELEMENTS(6) +      !(9) ! FUEL COST $
     +                               SUM_REPORT_ITEMS(9),
     +          UNIT_ENERGY_COST,                !(10)! ENERGY $/MWH
     +                     SEASONAL_COST_ELEMENTS(2) -
     +                        SEASONAL_COST_ELEMENTS(6) +
     +                         SUM_REPORT_ITEMS(11),   !(11)! ENERGY $
     +                     SEASONAL_COST_ELEMENTS(2) +
     +                          SEASONAL_COST_ELEMENTS(4) +
     +                           SUM_REPORT_ITEMS(12),  !(12)! TOTAL $
     +                     SUM_REPORT_ITEMS(13),            !(13)
     +                     SUM_REPORT_ITEMS(14),            !(14)
     +                     SUM_REPORT_ITEMS(15),            !(15)
     +                     SUM_REPORT_ITEMS(16),            !(16)
     +                     AVE_TOTAL_COST,                  !(17)!
     +                     AVE_VAR_COST,                    !(18)!
     +                     0., ! RES_TRACKER,
     +                     0., ! FUEL_TRACKER
     +                     0.,  ! RATE_TRACKER
     +                     0.,  ! MEM_TRACKER
     +               SUM_REPORT_ITEMS(19),            ! EMISSION AMOUNT
     +         SUM_REPORT_ITEMS(20)             ! AVERAGE EMISSION RATE
         ENDIF
      RETURN
!
!******************************************************************
!     ! CALLED AT END OF PRODUCTION LOOP IN PROCOST
      ENTRY WRITE_TRANS_ANNUAL_TYPE
!******************************************************************
         IF(SERVICE_TRANS_REPORT_NOT_OPEN) RETURN
         SEASON = LAST_SEASON
         DO TRANS_NO = 1 , NO_OF_TRANS
            IF(.NOT. TRANSACTION_ACTIVE(TRANS_NO)) CYCLE
            IF( (ANNUAL_COSTS(1,TRANS_NO)+ANNUAL_COSTS(5,TRANS_NO))
     +                                                     > 0.) THEN
               IMPUTTED_ENERGY = 1000. *
     +           (ANNUAL_COSTS(2,TRANS_NO) + ANNUAL_COSTS(6,TRANS_NO))/
     +            (ANNUAL_COSTS(1,TRANS_NO) + ANNUAL_COSTS(5,TRANS_NO))
            ELSE
               IMPUTTED_ENERGY = 0.
            ENDIF
            IF(ANNUAL_COSTS(3,TRANS_NO) > 0.) THEN
               IMPUTTED_CAPACITY =
     +               ANNUAL_COSTS(4,TRANS_NO)/ANNUAL_COSTS(3,TRANS_NO)
            ELSE
               IMPUTTED_CAPACITY = 0.
            ENDIF
            IF(COST_ASSIGN(TRANS_NO) == 'R') THEN
               COST_NO = 1
            ELSE
               COST_NO = 2
            ENDIF
            SELECT CASE (TRAN_TYPE(TRANS_NO))
            CASE ('T')
               TRANS_TYPE_NO = 1
            CASE ('S')
               TRANS_TYPE_NO = 2
            CASE ('W')
               TRANS_TYPE_NO = 3
            CASE ('D')
               TRANS_TYPE_NO = 4
            CASE ('O')
               TRANS_TYPE_NO = 5
            END SELECT
            TRANS_TYPE = TRANS_TYPE_NAME(TRANS_TYPE_NO)
!
            IF(COST_NO == 1) THEN
               COST_POSITION = NO_OF_TRANS+TRANS_TYPE_NO
            ELSE
               COST_POSITION=NO_OF_TRANS+NO_TRANS_TYPES+TRANS_TYPE_NO
            ENDIF
            ANNUAL_COSTS(1,COST_POSITION) =
     +                                 ANNUAL_COSTS(1,COST_POSITION) +
     +                                         ANNUAL_COSTS(1,TRANS_NO)
            ANNUAL_COSTS(2,COST_POSITION) =
     +                                 ANNUAL_COSTS(2,COST_POSITION) +
     +                                         ANNUAL_COSTS(2,TRANS_NO)
            ANNUAL_COSTS(3,COST_POSITION) =
     +                                 ANNUAL_COSTS(3,COST_POSITION) +
     +                                         ANNUAL_COSTS(3,TRANS_NO)
            ANNUAL_COSTS(4,COST_POSITION) =
     +                                 ANNUAL_COSTS(4,COST_POSITION) +
     +                                         ANNUAL_COSTS(4,TRANS_NO)
            ANNUAL_COSTS(5,COST_POSITION) =
     +                                 ANNUAL_COSTS(5,COST_POSITION) +
     +                                         ANNUAL_COSTS(5,TRANS_NO)
            ANNUAL_COSTS(6,COST_POSITION) =
     +                                 ANNUAL_COSTS(6,COST_POSITION) +
     +                                         ANNUAL_COSTS(6,TRANS_NO)
!
            COST_ASSIGNMENT_NAME = ASSIGNMENT_NAME(COST_NO)
! WRITE EACH TRANSACTION
            WRITE(TRANS_RPT_NO,REC=TRANS_RPT_REC) PRT_ENDPOINT(),
     +                  FLOAT(YEAR+BASE_YEAR),
     +                  CL_MONTH_NAME(SEASON+1),
     +                  TRANS_NAME(TRANS_NO),
     +                  COST_ASSIGNMENT_NAME,
     +                  TRANS_TYPE,
     +                  ANNUAL_COSTS(1,TRANS_NO),
     +                  IMPUTTED_ENERGY,
     +                  ANNUAL_COSTS(2,TRANS_NO),
     +                  ANNUAL_COSTS(3,TRANS_NO),
     +                  IMPUTTED_CAPACITY,
     +                  ANNUAL_COSTS(4,TRANS_NO),
     +                  ANNUAL_COSTS(2,TRANS_NO)+
     +                               ANNUAL_COSTS(4,TRANS_NO)
            TRANS_RPT_REC = TRANS_RPT_REC + 1
!
            IF(PRINT_WABASH_COST_REPORT) THEN

               IF(ANNUAL_COSTS(4,TRANS_NO) == 0.) THEN
                  RPT_CAPACITY = 0.
               ELSE
                  RPT_CAPACITY = ANNUAL_COSTS(3,TRANS_NO)
               ENDIF
               IF(ANNUAL_COSTS(2,TRANS_NO) +
     +                             ANNUAL_COSTS(6,TRANS_NO) == 0.) THEN
                  RPT_ENERGY = 0.
               ELSE
                  RPT_ENERGY = ANNUAL_COSTS(1,TRANS_NO) +
     +                                         ANNUAL_COSTS(5,TRANS_NO)
               ENDIF
               KW_CAPACITY = RPT_CAPACITY*1000.
               CAP_FACTOR = 0.
               IF(RPT_ENERGY > 0.) THEN
                  AVE_VAR_COST = 1000.*
     +             (ANNUAL_COSTS(2,TRANS_NO)+ANNUAL_COSTS(6,TRANS_NO))/
     +                                                       RPT_ENERGY
                  AVE_TOTAL_COST = 1000.*(ANNUAL_COSTS(2,TRANS_NO) +
     +                              ANNUAL_COSTS(6,TRANS_NO) +
     +                             ANNUAL_COSTS(4,TRANS_NO))/RPT_ENERGY
               ELSE
                  AVE_VAR_COST = 0.
                  AVE_TOTAL_COST = 0.
               ENDIF
               IF(EXPENSE_ASSIGN(TRANS_NO) == 'A') THEN
                  FUEL_CHARGE_RATE = IMPUTTED_ENERGY
                  FUEL_COST = ANNUAL_COSTS(6,TRANS_NO)
                  UNIT_ENERGY_COST = 0.
                  ENERGY_CHARGE = 0.
               ELSE
                  UNIT_ENERGY_COST = IMPUTTED_ENERGY
                  ENERGY_CHARGE = ANNUAL_COSTS(2,TRANS_NO)
                  FUEL_CHARGE_RATE = 0.
                  FUEL_COST = 0.
               ENDIF
!
               CALL GET_SERVICE_TRANS_TRACKERS(TRANS_NO,
     +                                             RES_TRACKER,
     +                                             FUEL_TRACKER,
     +                                             RATE_TRACKER,
     +                                             MEM_TRACKER)
!
               INQUIRE(UNIT=WABASH_REPORT_UNIT_NO,
     +                                      NEXTREC=WABASH_REC_COUNTER)
               WRITE(WABASH_REPORT_UNIT_NO,REC=WABASH_REC_COUNTER)
     +                     PRT_ENDPOINT(),
     +                     FLOAT(YEAR+BASE_YEAR),
     +                     CL_MONTH_NAME(SEASON+1),
     +                     TRANS_NAME(TRANS_NO),
     +                     KW_CAPACITY,
     +                     RPT_ENERGY,
     +                     0.,
     +                     KW_CAPACITY,
     +                     RPT_ENERGY,
     +                     IMPUTTED_CAPACITY,
     +                     ANNUAL_COSTS(4,TRANS_NO),
     +                     FUEL_CHARGE_RATE,
     +                     FUEL_COST,
     +                     UNIT_ENERGY_COST,
     +                     ENERGY_CHARGE,
     +                     ANNUAL_COSTS(2,TRANS_NO) +
     +                               ANNUAL_COSTS(4,TRANS_NO) +
     +                                        ANNUAL_COSTS(6,TRANS_NO),
     +                     KW_CAPACITY,
     +                     KW_CAPACITY,
     +                     0.,
     +                     CAP_FACTOR,
     +                     AVE_TOTAL_COST,
     +                     AVE_VAR_COST,
     +                     RES_TRACKER,
     +                     FUEL_TRACKER,
     +                     RATE_TRACKER,
     +                     MEM_TRACKER,
     +                     0.,  ! EMISSION AMOUNT
     +                     0.   ! AVERAGE EMISSION RATE
            ENDIF ! PRINT_WABASH_COST_REPORT
         ENDDO ! TRANSACTIONS LOOP
!
!
! 11/26/03. MOVED UP
!
         SEASONAL_COST_ELEMENTS(1) = 0.
         SEASONAL_COST_ELEMENTS(2) = 0.
         SEASONAL_COST_ELEMENTS(3) = 0.
         SEASONAL_COST_ELEMENTS(4) = 0.
!         IF(COST_NO == 2) THEN
            SEASONAL_COST_ELEMENTS(5) = ANNUAL_FUEL_ENERGY
            SEASONAL_COST_ELEMENTS(6) = ANNUAL_FUEL_COST

         DO COST_NO = 1 , 2
            IF(COST_NO == 1) THEN
               TEMP_R = -1.0
            ELSE
               TEMP_R =  1.0
            ENDIF
            DO TRANS_TYPE_NO = 1 , NO_TRANS_TYPES
               IF(COST_NO == 1) THEN
                  COST_POSITION = NO_OF_TRANS+TRANS_TYPE_NO
               ELSE
                  COST_POSITION = NO_OF_TRANS + NO_TRANS_TYPES +
     +                                                    TRANS_TYPE_NO
               ENDIF

               IF(ANNUAL_COSTS(1,COST_POSITION) +
     +                         ANNUAL_COSTS(5,COST_POSITION) > 0.) THEN
                  IMPUTTED_ENERGY =
     +                  1000.* (ANNUAL_COSTS(2,COST_POSITION) +
     +                              ANNUAL_COSTS(6,COST_POSITION))/
     +                         (ANNUAL_COSTS(1,COST_POSITION) +
     +                                   ANNUAL_COSTS(5,COST_POSITION))
               ELSE
                  IMPUTTED_ENERGY = 0.
                  ANNUAL_COSTS(1,COST_POSITION) = 0.
               ENDIF
               IF(ANNUAL_COSTS(3,COST_POSITION) > 0.) THEN
                  IMPUTTED_CAPACITY =
     +                  ANNUAL_COSTS(4,COST_POSITION)/
     +                               ANNUAL_COSTS(3,COST_POSITION)
               ELSE
                  IMPUTTED_CAPACITY = 0.
                  ANNUAL_COSTS(3,COST_POSITION) = 0.
               ENDIF
! WRITE TOTAL BY TRANSACTION TYPE
               WRITE(TRANS_RPT_NO,REC=TRANS_RPT_REC) PRT_ENDPOINT(),
     +                  FLOAT(YEAR+BASE_YEAR),
     +                  CL_MONTH_NAME(SEASON+1),
     +                  'Total               ',
     +                  ASSIGNMENT_NAME(COST_NO),
     +                  TRANS_TYPE_NAME(TRANS_TYPE_NO),
     +                  ANNUAL_COSTS(1,COST_POSITION),
     +                  IMPUTTED_ENERGY,
     +                  ANNUAL_COSTS(2,COST_POSITION),
     +                  ANNUAL_COSTS(3,COST_POSITION),
     +                  IMPUTTED_CAPACITY,
     +                  ANNUAL_COSTS(4,COST_POSITION),
     +                  ANNUAL_COSTS(2,COST_POSITION) +
     +                                ANNUAL_COSTS(4,COST_POSITION)
               TRANS_RPT_REC = TRANS_RPT_REC + 1
               SEASONAL_COST_ELEMENTS(1) =
     +                    SEASONAL_COST_ELEMENTS(1) +
     +                             ANNUAL_COSTS(1,COST_POSITION)*TEMP_R
               SEASONAL_COST_ELEMENTS(2) =
     +                    SEASONAL_COST_ELEMENTS(2) +
     +                             ANNUAL_COSTS(2,COST_POSITION)*TEMP_R
               SEASONAL_COST_ELEMENTS(3) =
     +                    SEASONAL_COST_ELEMENTS(3) +
     +                             ANNUAL_COSTS(3,COST_POSITION)*TEMP_R
               SEASONAL_COST_ELEMENTS(4) =
     +                    SEASONAL_COST_ELEMENTS(4) +
     +                             ANNUAL_COSTS(4,COST_POSITION)*TEMP_R
            ENDDO ! TRANSACTION_TYPES LOOP
            IF(SEASONAL_COST_ELEMENTS(1) > 0.) THEN
               IMPUTTED_ENERGY = 1000.*
     +              SEASONAL_COST_ELEMENTS(2)/SEASONAL_COST_ELEMENTS(1)
            ELSE
               IMPUTTED_ENERGY = 0.
            ENDIF
            IF(SEASONAL_COST_ELEMENTS(3) > 0.) THEN
               IMPUTTED_CAPACITY =
     +              SEASONAL_COST_ELEMENTS(4)/SEASONAL_COST_ELEMENTS(3)
            ELSE
               IMPUTTED_CAPACITY = 0.
            ENDIF
! WRITE TOTAL BY COST TYPE
            WRITE(TRANS_RPT_NO,REC=TRANS_RPT_REC) PRT_ENDPOINT(),
     +                  FLOAT(YEAR+BASE_YEAR),
     +                  CL_MONTH_NAME(SEASON+1),
     +                  'Total               ',
     +                  ASSIGNMENT_NAME(COST_NO),
     +                  'Total       ',
     +                  SEASONAL_COST_ELEMENTS(1),
     +                  IMPUTTED_ENERGY,
     +                  SEASONAL_COST_ELEMENTS(2),
     +                  SEASONAL_COST_ELEMENTS(3),
     +                  IMPUTTED_CAPACITY,
     +                  SEASONAL_COST_ELEMENTS(4),
     +                  SEASONAL_COST_ELEMENTS(2)+
     +                               SEASONAL_COST_ELEMENTS(4)
            TRANS_RPT_REC = TRANS_RPT_REC + 1
!
            IF(PRINT_WABASH_COST_REPORT .AND. COST_NO == 2) THEN
               IF(SEASONAL_COST_ELEMENTS(4) == 0.) THEN
                  RPT_CAPACITY = 0.
               ELSE
                  RPT_CAPACITY = SEASONAL_COST_ELEMENTS(3)
               ENDIF
               KW_CAPACITY = RPT_CAPACITY*1000.
               CAP_FACTOR = 0.
               IF( SEASONAL_COST_ELEMENTS(1) > 0.) THEN
                  AVE_VAR_COST = 1000.* SEASONAL_COST_ELEMENTS(2)/
     +                                        SEASONAL_COST_ELEMENTS(1)
                  AVE_TOTAL_COST = 1000.*
     +                            (SEASONAL_COST_ELEMENTS(2) +
     +                                 SEASONAL_COST_ELEMENTS(4))/
     +                                       SEASONAL_COST_ELEMENTS(1)
               ELSE
                  AVE_VAR_COST = 0.
                  AVE_TOTAL_COST = 0.
               ENDIF
               IF( SEASONAL_COST_ELEMENTS(5) > 0.) THEN
                  FUEL_CHARGE_RATE = 1000.* SEASONAL_COST_ELEMENTS(6)/
     +                                        SEASONAL_COST_ELEMENTS(5)
               ELSE
                  FUEL_CHARGE_RATE = 0.
               ENDIF
               IF(SEASONAL_COST_ELEMENTS(1) -
     +                             SEASONAL_COST_ELEMENTS(5) > 0.) THEN
                  UNIT_ENERGY_COST = (SEASONAL_COST_ELEMENTS(2) -
     +                              SEASONAL_COST_ELEMENTS(6)) /
     +                          (SEASONAL_COST_ELEMENTS(1) -
     +                                       SEASONAL_COST_ELEMENTS(5))
               ELSE
                  UNIT_ENERGY_COST = 0.
               ENDIF
               INQUIRE(UNIT=WABASH_REPORT_UNIT_NO,
     +                                      NEXTREC=WABASH_REC_COUNTER)
               WRITE(WABASH_REPORT_UNIT_NO,REC=WABASH_REC_COUNTER)
     +                     PRT_ENDPOINT(),
     +                     FLOAT(YEAR+BASE_YEAR),
     +                     CL_MONTH_NAME(SEASON+1),
     +                     WVPA_COST_ASSIGN(COST_NO),
     +                     KW_CAPACITY,
     +                     SEASONAL_COST_ELEMENTS(1),
     +                     0.,
     +                     KW_CAPACITY,
     +                     SEASONAL_COST_ELEMENTS(1),
     +                     IMPUTTED_CAPACITY,
     +                     SEASONAL_COST_ELEMENTS(4),
     +                     FUEL_CHARGE_RATE,
     +                     SEASONAL_COST_ELEMENTS(6),
     +                     UNIT_ENERGY_COST,
     +                     SEASONAL_COST_ELEMENTS(2),
     +                     SEASONAL_COST_ELEMENTS(2) +
     +                              SEASONAL_COST_ELEMENTS(6) +
     +                                       SEASONAL_COST_ELEMENTS(4),
     +                     KW_CAPACITY,
     +                     KW_CAPACITY,
     +                     0.,
     +                     CAP_FACTOR,
     +                     AVE_TOTAL_COST,
     +                     AVE_VAR_COST,
     +                     0., ! RES_TRACKER,
     +                     0., ! FUEL_TRACKER
     +                     0., ! RATE_TRACKER
     +                     0.,  ! MEM_TRACKER
     +                     0.,  ! EMISSION AMOUNT
     +                     0.   ! AVERAGE EMISSION RATE
            ENDIF ! PRINT_WABASH_COST_REPORT .AND. COST_NO == 2
         ENDDO ! EXPENSE OR REVENUE
!
         IF(PRINT_WABASH_COST_REPORT) THEN
            CALL RETURN_ANN_SUM_REPORT_ITEMS(SUM_REPORT_ITEMS)
            IF(SUM_REPORT_ITEMS(1) > 0.) THEN
               UNIT_CAPACITY_COST = 1000.*(SUM_REPORT_ITEMS(7) +
     +                   SEASONAL_COST_ELEMENTS(4))/SUM_REPORT_ITEMS(1)
            ELSE
               UNIT_CAPACITY_COST = 0.
            ENDIF
            IF(SUM_REPORT_ITEMS(5) > 0.) THEN
               UNIT_ENERGY_COST =
     +                     1000.*(SEASONAL_COST_ELEMENTS(2) +
     +                                      SUM_REPORT_ITEMS(11))/
     +                                              SUM_REPORT_ITEMS(5)
               AVE_VAR_COST = 1000.*(SEASONAL_COST_ELEMENTS(2) +
     +                SUM_REPORT_ITEMS(9) + SUM_REPORT_ITEMS(11))/
     +                                              SUM_REPORT_ITEMS(2)
               AVE_TOTAL_COST =
     +                     1000.*(SEASONAL_COST_ELEMENTS(2) +
     +                           SEASONAL_COST_ELEMENTS(4) +
     +                                SUM_REPORT_ITEMS(12))/
     +                                              SUM_REPORT_ITEMS(2)
            ELSE
               UNIT_ENERGY_COST = 0.
               AVE_VAR_COST = 0.
               AVE_TOTAL_COST = 0.
            ENDIF
            INQUIRE(UNIT=WABASH_REPORT_UNIT_NO,
     +                                      NEXTREC=WABASH_REC_COUNTER)
            WRITE(WABASH_REPORT_UNIT_NO,REC=WABASH_REC_COUNTER)
     +                     PRT_ENDPOINT(),
     +                     FLOAT(YEAR+BASE_YEAR),
     +                     CL_MONTH_NAME(SEASON+1),
     +                     'Total Cost          ',
     +                     SUM_REPORT_ITEMS(1),             !(1)
     +                     SUM_REPORT_ITEMS(2),             !(2)
     +                     SUM_REPORT_ITEMS(3),             !(3)
     +                     SUM_REPORT_ITEMS(4),             !(4)
     +                     SUM_REPORT_ITEMS(5),             !(5)
     +                     UNIT_CAPACITY_COST,              !(6)!
     +                     SEASONAL_COST_ELEMENTS(4) +
     +                              SUM_REPORT_ITEMS(7),    !(7)
     +                     SUM_REPORT_ITEMS(8),             !(8)
     +                     SUM_REPORT_ITEMS(9) +
     +                    SEASONAL_COST_ELEMENTS(6),!(9) ! FUEL COST $
     +            UNIT_ENERGY_COST,                !(10)! ENERGY $/MWH
     +                     SEASONAL_COST_ELEMENTS(2) +
     +                         SUM_REPORT_ITEMS(11),   !(11)! ENERGY $
     +                     SEASONAL_COST_ELEMENTS(2) +
     +                         SEASONAL_COST_ELEMENTS(4) +
     +                             SEASONAL_COST_ELEMENTS(6) +
     +                            SUM_REPORT_ITEMS(12),  !(12)! TOTAL $
     +                     SUM_REPORT_ITEMS(13),            !(13)
     +                     SUM_REPORT_ITEMS(14),            !(14)
     +                     SUM_REPORT_ITEMS(15),            !(15)
     +                     SUM_REPORT_ITEMS(16),            !(16)
     +                     AVE_TOTAL_COST,                  !(17)!
     +                     AVE_VAR_COST,                     !(18)!
     +                     0., ! RES_TRACKER,
     +                     0., ! FUEL_TRACKER
     +                     0., ! RATE_TRACKER
     +                     0.,  ! MEM_TRACKER
     +                     SUM_REPORT_ITEMS(19),  ! EMISSION AMOUNT
     +                   SUM_REPORT_ITEMS(20)   ! AVERAGE EMISSION RATE
         ENDIF ! PRINT_WABASH_COST_REPORT
!
      RETURN
!
!******************************************************************
      ENTRY INIT_TRANS_ANNUAL_COSTS
!******************************************************************
         IF(.NOT. ALLOCATED(ANNUAL_COSTS)) RETURN
         ANNUAL_COSTS = 0.
         TRANSACTION_ACTIVE = FALSE_BYTE
         ANNUAL_FUEL_ENERGY = 0.
         ANNUAL_FUEL_COST = 0.
      RETURN
      END
!******************************************************************
!     FUNCTION GET_DATA_BASE()
!******************************************************************
!
!     INTEGER*2 GET_DATA_BASE
!        GET_DATA_BASE = 2
!     RETURN
!     END
