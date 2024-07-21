C     Last change: MSG 1/4/2013 8:20:40 AM


C                    CONVERT THE SERVICE-TRANSACTIONS FILE
C                             COPYRIGHT (C) 1992
C                        M.S. GERBER & ASSOCIATES, INC.
C                             ALL RIGHTS RESERVED


      SUBROUTINE TR_OBJECT
      use end_routine, only: end_program, er_message
      use filename_tracker
      use service_decs


      use SpinDriftLib
      use prod_arrays_dimensions
      USE PROD_ARRAYS_DIMENSIONS
      use sizecom
      real (kind=4) :: energy_charge_ord
      INTEGER*2 DELETE,IREC,INUNIT,LRECL/270/
      INTEGER*4 IOS,IOS_BASE
      INTEGER*2 UNIT_NUM/10/
      INTEGER*2 NUM_SERVICE_TRANS/0/,R_NUM_SERVICE_TRANS
      INTEGER*4 R_UNIT_NUM
      CHARACTER*5 BASE_FILE_NAME,OVERLAY_FAMILY_NAME,
     +            CAP_ENRG_TRANSACTIONS_FILE
      CHARACTER*20 REVENUE_CLASSIFICATION
      CHARACTER*20 EXPENSE_CLASSIFICATION
      CHARACTER*50 COMMENT
      CHARACTER*256 FILE_NAME
      CHARACTER*256 BASE_FILE_DIRECTORY
      CHARACTER*256 DATA_DRIVE,OUTPUT_DIRECTORY
      LOGICAL*4 FILE_EXISTS/.FALSE./,R_SERVICE_TRANSACTIONS_ACTIVE
C DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER*1024 RECLN
C DECLARATION FOR THE SERVICE TRANSACTIONS FILE
      INTEGER*2 SERVICE_REPORTING_GROUP,
     +          MONTH_SERVICE_AVAILABLE,YEAR_SERVICE_AVAILABLE,
     +          MONTH_SERVICE_ENDS,YEAR_SERVICE_ENDS,UPDATE_MONTH,
     +          RESOURCE_POINTER,CAP_ESCALATION_VECTOR,
     +          ENRG_ESCALATION_VECTOR
      INTEGER*4 SERVICE_ID_NUMBER
      REAL*4 ANNUAL_CAPACITY,CAPACITY_DISTRIBUTION_PATTERN,
     +     capacity_charge_ord,ANNUAL_ENERGY,ENERGY_DISTRIBUTION_PATTERN
      CHARACTER*20 SERVICE_NAME
      CHARACTER*20 FILE_TYPE/'Service Transactions'/
      CHARACTER*1 TYPE_OF_SERVICE_ord,COST_ASSIGNMENT,
     +            INTRA_COMPANY_TRANSACTION,
     +            SERVICE_ACTIVE,
     +            WVPA_RATE_TRACKER,
     +            WVPA_RES_TRACKER,
     +            WVPA_FUEL_TRACKER,
     +            WVPA_MEM_TRACKER
      INTEGER*2 INTRA_ASSET_CLASS_ID,INTRA_ASSET_CLASS_ALLOC_VECTOR
      CHARACTER*20 INTRA_ACCOUNT_CLASSIFICATION
      CHARACTER*3 INTRA_EXPENSE_COLLECTION,EXPENSE_COLLECTION
      CHARACTER*2 SERVICE_TRANS_OL/'BC'/
      CHARACTER*5 LINK_TYPE
      CHARACTER*1 energy_to_use_ord
C ASSET CLASS VARIABLES
      INTEGER*2 ASSET_CLASS_NUM,ASSET_CLASS_VECTOR
      LOGICAL*1 LAHEY_LF95
      CHARACTER*30 SCREEN_OUTPUT
! END OF DATA DECLARATIONS

      ENTRY TR_MAKEBIN


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

         DO
C ASSET CLASS INITIALIZE
            ASSET_CLASS_NUM = 0
            ASSET_CLASS_VECTOR = 0
            REVENUE_CLASSIFICATION = ' '
            INTRA_COMPANY_TRANSACTION = 'N'
            INTRA_ASSET_CLASS_ID = 0
            INTRA_ASSET_CLASS_ALLOC_VECTOR = 0
            INTRA_ACCOUNT_CLASSIFICATION = ' '
            INTRA_EXPENSE_COLLECTION = ' '
            EXPENSE_CLASSIFICATION = 'Service'
            TYPE_OF_SERVICE_ord = 'T' ! ADDED 9/4/97. GAT.
            UPDATE_MONTH = 1
            MONTH_SERVICE_AVAILABLE = 1
            YEAR_SERVICE_AVAILABLE = 1990
            MONTH_SERVICE_ENDS = 12
            YEAR_SERVICE_ENDS = 2100
            SERVICE_ACTIVE = 'T'
            energy_to_use_ord = 'B'
            WVPA_RATE_TRACKER = 'N'
            WVPA_RES_TRACKER = 'N'
            WVPA_FUEL_TRACKER = 'N'
            WVPA_MEM_TRACKER = 'M'
C
            DO
               READ(10,1000,IOSTAT=IOS) RECLN
               IF(IOS .NE. 0) EXIT
               IF(RECLN(1:1) == '7') EXIT
               RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               READ(RECLN,*,ERR=200) DELETE,SERVICE_NAME,
     +                            SERVICE_ID_NUMBER,
     +                            TYPE_OF_SERVICE_ord,COST_ASSIGNMENT,
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
     +                            capacity_charge_ord,
     +                            CAP_ESCALATION_VECTOR,
     +                            ANNUAL_ENERGY,
     +                            ENERGY_DISTRIBUTION_PATTERN,
     +                            ENERGY_CHARGE_ord,
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
     +                            energy_to_use_ord,
     +                            WVPA_RATE_TRACKER,
     +                            WVPA_RES_TRACKER,
     +                            WVPA_FUEL_TRACKER,
     +                            WVPA_MEM_TRACKER
               IREC = IREC + 1
              WRITE(11,REC=IREC) DELETE,SERVICE_NAME,SERVICE_ID_NUMBER,
     +                         TYPE_OF_SERVICE_ord,COST_ASSIGNMENT,
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
     +                         capacity_charge_ord,
     +                         CAP_ESCALATION_VECTOR,
     +                         ANNUAL_ENERGY,
     +                         ENERGY_DISTRIBUTION_PATTERN,
     +                         ENERGY_CHARGE_ord,
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
     +                         energy_to_use_ord,
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

C OVERLAY THE SERVICE-TRANSACTIONS FILE

      ENTRY TR_MAKEOVL(OVERLAY_FAMILY_NAME)


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
     +                         TYPE_OF_SERVICE_ord,COST_ASSIGNMENT,
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
     +                         capacity_charge_ord,
     +                         CAP_ESCALATION_VECTOR,
     +                         ANNUAL_ENERGY,
     +                         ENERGY_DISTRIBUTION_PATTERN,
     +                         ENERGY_CHARGE_ord,
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
     +                         energy_to_use_ord,
     +                         WVPA_RATE_TRACKER,
     +                         WVPA_RES_TRACKER,
     +                         WVPA_FUEL_TRACKER,
     +                         WVPA_MEM_TRACKER
            IF(IOS_BASE /= 0) EXIT
            IF(IOS == 0) THEN
               RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               READ(RECLN,*,ERR=200) DELETE,SERVICE_NAME,
     +                            SERVICE_ID_NUMBER,
     +                            TYPE_OF_SERVICE_ord,COST_ASSIGNMENT,
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
     +                            capacity_charge_ord,
     +                            CAP_ESCALATION_VECTOR,
     +                            ANNUAL_ENERGY,
     +                            ENERGY_DISTRIBUTION_PATTERN,
     +                            ENERGY_CHARGE_ord,
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
     +                            energy_to_use_ord,
     +                            WVPA_RATE_TRACKER,
     +                            WVPA_RES_TRACKER,
     +                            WVPA_FUEL_TRACKER,
     +                            WVPA_MEM_TRACKER
            ENDIF
            WRITE(12,REC=IREC) DELETE,SERVICE_NAME,SERVICE_ID_NUMBER,
     +                      TYPE_OF_SERVICE_ord,COST_ASSIGNMENT,
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
     +                      capacity_charge_ord,
     +                      CAP_ESCALATION_VECTOR,
     +                      ANNUAL_ENERGY,
     +                      ENERGY_DISTRIBUTION_PATTERN,
     +                      ENERGY_CHARGE_ord,
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
     +                      energy_to_use_ord,
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

  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from Servicac SIID265'
      call end_program(er_message)


      ENTRY RESET_SERVICE_TRANS_OL

         SERVICE_TRANS_OL = 'BC'
      RETURN


      ENTRY OPEN_SERVICE_TRANS_FILE(R_UNIT_NUM,
     +                              R_SERVICE_TRANSACTIONS_ACTIVE,
     +                              R_NUM_SERVICE_TRANS)

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


      ENTRY CLOSE_SERVICE_TRANS_FILE

         IF(FILE_EXISTS) CLOSE(UNIT_NUM)
      RETURN


      ENTRY GET_NUMBER_OF_SERVICES (R_NUM_SERVICE_TRANS)

         R_NUM_SERVICE_TRANS = NUM_SERVICE_TRANS
      RETURN

 1000 FORMAT(A)
 1010 FORMAT('&',A,I4)
      END SUBROUTINE TR_OBJECT
C**********************************************************************

C                         SERVICE TRANSACTIONS MODULE
C                              COPYRIGHT (C) 1992
C                        M.S. GERBER & ASSOCIATES, INC.
C                              ALL RIGHTS RESERVED

C**********************************************************************

      RECURSIVE SUBROUTINE READ_SERVICE_TRANS(nr_NUNITS,
     +                                     SERVICE_TRANSACTIONS_ACTIVE)

      use res_decs
      USE IREC_ENDPOINT_CONTROL
      use prod_arrays_dimensions
      USE PROD_ARRAYS_DIMENSIONS
      use sizecom
      use servcom
      use globecom
      use enrg_helper
      use service_decs
      use grx_planning_routines
      use SpinDriftLib
      use prod_arrays_dimensions
      use eco
      use lamcom
      use forecast
      use production
      use namescom
      use month_extra
      INCLUDE 'mthnmcom.mon'
      
      integer (kind=2), allocatable :: res_id_loc(:)
      logical(kind=1) :: kpcval
      LOGICAL (KIND=1) :: INTO_EXTENSION_PERIOD
      INTEGER*4 VALUES_2_ZERO
      INTEGER*2 EXP_TYPE,REV_TYPE,INCOME_STATEMENT_POSITION,
     +            R_TRANS_NO
      REAL*4 TRANSACTION_AMOUNT
      CHARACTER*24 SERVICE_TITLE(MAX_SERVICE_ITEMS)
      REAL*4 SUM_OF_SERVICE(0:MAX_SERVICE_GROUPS)
      REAL*4 TOTAL,
     +       RPT_ENERGY_CHARGE,
     +       RPT_CAPACITY_CHARGE,
     +       R_RES_TRACKER,
     +       R_FUEL_TRACKER,
     +       R_RATE_TRACKER,
     +       R_MEM_TRACKER
      LOGICAL*1   WABASH_VALLEY_ACTIVE/.FALSE./,WRITE_IT_MONTHLY,
     +            FIRST_EL_SERVICE_WRITE/.TRUE./,
     +            FIRST_CL_SERVICE_WRITE/.TRUE./,
     +            FIRST_CT_SERVICE_WRITE/.TRUE./,
     +            FIRST_OT_SERVICE_WRITE/.TRUE./,
     +            WVPA
      LOGICAL*4 SERVICE_TRANSACTIONS_ACTIVE
      INTEGER*2 GROUPS

      INTEGER EL_RESOURCE_ID ! ALTERED 07/14/03.

      REAL*4 ST_REVENUE,ST_EXPENSE

      CHARACTER*1 CLASS_CHR,UTILITY_TYPE
      INTEGER*2 DELETE,IREC,SERVICE_TRANS,I,YR,CLASS,DATE1,DATE2,
     +            R_SERVICE_TRANS
      INTEGER*4 IOS
      INTEGER*2 NUM_SERVICE_TRANS

      REAL*4 get_escalated_value,ESCALATED_MONTHLY_VALUE
      REAL*4 GET_VAR

      INTEGER*2  MONTH_SERVICE_AVAILABLE,
     +           YEAR_SERVICE_AVAILABLE,
     +           MONTH_SERVICE_ENDS,
     +           YEAR_SERVICE_ENDS
      REAL*4 ANNUAL_PEAK

      REAL*4 ANNUAL_ENERGY,ANNUAL_CAPACITY
      REAL*4 CLASS_FORECAST

      INTEGER*2 nr_NUNITS,ISEAS
      REAL*4 ENERGY(2*nr_NUNITS),CAPACITY(nr_NUNITS)
      INTEGER*2 L
      REAL*4 SEASONAL_ENERGY_CHARGE,SEASONAL_CAPACITY_CHARGE,
     +     PEAK_RESERVE,ENER_RESERVE,SEASONAL_ENERGY,
     +     GET_CUST_GROUP_ENERGY,GET_CUST_GROUP_PEAK
      real(kind=4) :: sec_arg_pass
C MONTHLY SERVICE REPORT VARIABLES

      LOGICAL*1 MONTHLY_SERVICE_REPORT_ACTIVE/.FALSE./,
     +          LOGICAL1_TRUE/.TRUE./,
     +          INCLUDE_FIXED_COSTS_ADJ_CLAUSE,
     +          FIXED_COSTS_IN_ADJ_CLAUSE
C
      CHARACTER*1 R_COST_ASSIGN(R_SERVICE_TRANS),
     +            R_TRAN_TYPE(R_SERVICE_TRANS),
     +            R_EXPENSE_ASSIGN(R_SERVICE_TRANS)
      CHARACTER*20 R_NAME(R_SERVICE_TRANS)
      CHARACTER*1 SERVICE_REPORT



      CHARACTER*5  SERVICE_LINK_TYPE(:)

      INTEGER*2
     +          SERVICE_AVAILABLE(:),
     +          SERVICE_ENDS(:),
     +          SERVICE_UPDATE_SEASON(:),
     +          SERVICE_RESOURCE_PONTR(:),
     +          SERVICE_CAP_ESCALATION_VECTOR(:),
     +          SERVICE_ENRG_ESCALATION_VECTOR(:),
     +          WVPA_TRACKING_TYPE,
     +          WVPA_RESOURCE_TRACKING_TYPE,
     +          WVPA_FUEL_TRACKING_TYPE,
     +          WVPA_MEM_TRACKING_TYPE
      INTEGER*4 SERVICE_ID_NUMBER(:)
      REAL*4 SERVICE_ANNUAL_CAPACITY(:),SERVICE_CAPACITY_PATTERN(:),
     +     SERVICE_CAPACITY_CHARGE(:),SERVICE_ANNUAL_ENERGY(:),
     +     SERVICE_ENERGY_PATTERN(:),SERVICE_ENERGY_CHARGE(:)


      REAL (KIND=4), ALLOCATABLE :: SERVICE_FUEL_CHARGE(:),
     +                              SERVICE_FUEL_ESCALATION_VECTOR(:)
      CHARACTER*20 SERVICE_REVENUE_CLASSIFICATION(:)

      CHARACTER*1 SERVICE_INTRA_COMPY_TRANSACTION(:),
     +            SERVICE_ACTIVE,
     +            ENERGY_TO_USE(:),
     +            WVPA_RATE_TRACKER(:),
     +            WVPA_RES_TRACKER,
     +            WVPA_FUEL_TRACKER,
     +            WVPA_MEM_TRACKER
      INTEGER*2 INTRA_ASSET_CLASS_ID(:),
     +          INTRA_ASSET_CLASS_ALLOC_VECTOR(:)
      CHARACTER*20 INTRA_ACCOUNT_CLASSIFICATION(:)
      CHARACTER*3 INTRA_EXPENSE_COLLECTION(:)
      ALLOCATABLE ::
     +            SERVICE_ID_NUMBER,
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
     +            INTRA_ASSET_CLASS_ID,
     +            INTRA_ASSET_CLASS_ALLOC_VECTOR,
     +            INTRA_ACCOUNT_CLASSIFICATION,
     +            INTRA_EXPENSE_COLLECTION

      INTEGER*2 SEASON_HOURS(12)
C
C ASSET ALLOCATION STUFF

      INTEGER*2 NUMBER_OF_SERVICE_CLASSES,UNIT_NO,
     +           MAX_SERVICE_CLASS_ID_NUM,
     +          SERVICE_ASSET_CLASS_POINTER(:)
      ALLOCATABLE :: SERVICE_ASSET_CLASS_POINTER

C ASSET ALLOCATION MANAGER STUFF

      REAL*4 VAR_COST,ST_FIXED_COST,TRANS_ENERGY

      REAL*4 TRANS_CAPACITY,ST_SO2_EMIS

      INTEGER*2 NUMBER_OF_SERVICES

      INTEGER*2 CURRENT_YEAR,CURRENT_YEAR_COMPARISON,
     +          UNIT,CLASS_NUM
      REAL*4 LAST_SEASON
      REAL*4 ANNUAL_ST_CAPACITY(:),ANNUAL_ST_ENERGY(:),
     +     ANNUAL_ST_VAR_COST(:),ANNUAL_ST_FIXED_COST(:),
     +     ANNUAL_ST_SO2_EMIS(:)
      ALLOCATABLE :: ANNUAL_ST_CAPACITY,ANNUAL_ST_ENERGY,
     +               ANNUAL_ST_VAR_COST,ANNUAL_ST_FIXED_COST,
     +               ANNUAL_ST_SO2_EMIS
      REAL*4 MONTHLY_ST_CAPACITY(:,:),MONTHLY_ST_ENERGY(:,:),
     +     MONTHLY_ST_VAR_COST(:,:),MONTHLY_ST_FIXED_COST(:,:),
     +     MONTHLY_ST_SO2_EMIS(:,:)
      ALLOCATABLE :: MONTHLY_ST_CAPACITY,MONTHLY_ST_ENERGY,
     +               MONTHLY_ST_VAR_COST,MONTHLY_ST_FIXED_COST,
     +               MONTHLY_ST_SO2_EMIS
C
C ASSET ALLOCATION STUFF

      CHARACTER*1 DUMMY_TYPE
      INTEGER*2 CLASS_POINTER,
     +          ASSET_CLASS_loc,
     +          ASSET_ALLOCATION_VECTOR
      INTEGER*2  J
      REAL*4 ASSET_CLASS_LIST(:),ASSET_ALLOCATION_LIST(:)
      REAL*4 ST_ANN_CLASS_ATL_EXPENSE(:,:),
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
      REAL*4 ASSET_ALLOCATOR
      ! NO NEED FOR ALLOCATION
      INTEGER*2 ASSET_CLASS_NUM(:),ASSET_CLASS_VECTOR(:)
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

C EXPENSE TRANSFER TO ASSET CLASS MODULE

      INTEGER*2 R_CLASS
      REAL*4 R_ATL_SERVICE_TRANSACTIONS_EXP,
     +     R_BTL_SERVICE_TRANSACTIONS_EXP,
     +     R_EXPENSE_COLLECTED_ADJ_CLAUSE,
     +     R_BTL_SALES_REVENUE,
     +     R_BASE_REVENUES,
     +     R_SECONDARY_SALES_REVENUE,
     +     R_CAPACITY_REVENUES,
     +     R_ADJ_CLAUSE_REVENUE,
     +     R_OTHER_REVENUE
      INTEGER*2 ATL,BTL
      CHARACTER*1 REVENUE
      PARAMETER(ATL=1,BTL=2,REVENUE='R')
      INTEGER*2 CURRENT_MONTH
C

      INTEGER*2   BEGIN_TEST_DATE,
     +            END_TEST_DATE


      LOGICAL*1 TRANSACTION_FOUND
      INTEGER*2 MO,HYDRO_TRANS_GROUP
      INTEGER*2 R_ISEAS
      LOGICAL*1 SERVICE_TRANS_REPORT_NOT_OPEN/.TRUE./
      INTEGER*2 TRANS_RPT_NO/0/,ANNUAL_SERVICE_TRANS_HEADER
      INTEGER TRANS_RPT_REC
      CHARACTER*4 LEFT_JUSTIFY_I2_IN_STR*15,TRANSACTION_DESCRIPTION*25,
     +            COST_ASSIGNMENT_NAME*7
      REAL*4 UNIT_ENERGY_COST,
     +       UNIT_CAPACITY_COST
      INTEGER*2 ALLOCATION_VECTOR
      REAL*4 ALLOCATION_VALUE(AVAIL_DATA_YEARS)
      REAL*4 MONTH_VARS(0:12,*)
      REAL*4 R_FUEXP,R_PREXP,R_OPEXP,
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

      LOGICAL (KIND=1) :: TRANSFER_TRANSACT_ANALYST_RESULTS

      LAST_SEASON = real(get_PRODUCTION_PERIODS_in(),4)
      CALL OPEN_SERVICE_TRANS_FILE(10,SERVICE_TRANSACTIONS_ACTIVE,
     +                             NUM_SERVICE_TRANS)
      IF(.NOT. SERVICE_TRANSACTIONS_ACTIVE) RETURN


! END OF DATA DECLARATIONS

C ASSET CLASS INITIALIZATION. 6/16/95. GAT.

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

      BEGIN_TEST_DATE = 100*(BASE_YEAR+1 - 1900) + 1
      END_TEST_DATE = 100*(LAST_STUDY_YEAR - 1900) + 12

      IF(ALLOCATED(ns_service_decs%SERVICE_NAME)) then
        CALL CLOSE_SERVICE_TRANSACTIONS
      endif

      ALLOCATE(ns_service_decs%SERVICE_NAME(NUM_SERVICE_TRANS),
     + SERVICE_ID_NUMBER(NUM_SERVICE_TRANS),
     + ns_service_decs%TYPE_OF_SERVICE(NUM_SERVICE_TRANS),
     + ns_service_decs%SERVICE_COST_ASSIGNMENT(NUM_SERVICE_TRANS),
     + ns_service_decs%SERVICE_EXPENSE_COLLECTION(NUM_SERVICE_TRANS),
     + ns_service_decs%SERVICE_REPORTING_GROUP(NUM_SERVICE_TRANS),
     + SERVICE_AVAILABLE(NUM_SERVICE_TRANS),
     + SERVICE_ENDS(NUM_SERVICE_TRANS),
     + SERVICE_UPDATE_SEASON(NUM_SERVICE_TRANS),
     + SERVICE_LINK_TYPE(NUM_SERVICE_TRANS),
     + SERVICE_RESOURCE_PONTR(NUM_SERVICE_TRANS),
     + SERVICE_ANNUAL_CAPACITY(NUM_SERVICE_TRANS),
     + SERVICE_CAPACITY_PATTERN(NUM_SERVICE_TRANS),
     + SERVICE_CAPACITY_CHARGE(NUM_SERVICE_TRANS),
     + SERVICE_CAP_ESCALATION_VECTOR(NUM_SERVICE_TRANS),
     + SERVICE_ANNUAL_ENERGY(NUM_SERVICE_TRANS),
     + SERVICE_ENERGY_PATTERN(NUM_SERVICE_TRANS),
     + SERVICE_ENERGY_CHARGE(NUM_SERVICE_TRANS),
     + SERVICE_ENRG_ESCALATION_VECTOR(NUM_SERVICE_TRANS),
     + SERVICE_REVENUE_CLASSIFICATION(NUM_SERVICE_TRANS),
     + SERVICE_INTRA_COMPY_TRANSACTION(NUM_SERVICE_TRANS),
     + ENERGY_TO_USE(NUM_SERVICE_TRANS),
     + WVPA_RATE_TRACKER(NUM_SERVICE_TRANS),
     + ns_service_decs%WVPA_RATE_TRACKER_INDEX(NUM_SERVICE_TRANS),
     + ns_service_decs%WVPA_RES_TRACKER_INDEX(NUM_SERVICE_TRANS),
     + ns_service_decs%WVPA_FUEL_TRACKER_INDEX(NUM_SERVICE_TRANS),
     + ns_service_decs%WVPA_MEM_TRACKER_INDEX(NUM_SERVICE_TRANS),
     + INTRA_ASSET_CLASS_ID(NUM_SERVICE_TRANS),
     + INTRA_ASSET_CLASS_ALLOC_VECTOR(NUM_SERVICE_TRANS),
     + INTRA_ACCOUNT_CLASSIFICATION(NUM_SERVICE_TRANS),
     + INTRA_EXPENSE_COLLECTION(NUM_SERVICE_TRANS),
     + ns_service_decs%SERVICE_EXPENSE_CLASSIFICATION(
     + NUM_SERVICE_TRANS),
     + SERVICE_FUEL_CHARGE(NUM_SERVICE_TRANS),
     + SERVICE_FUEL_ESCALATION_VECTOR(NUM_SERVICE_TRANS),
     + STAT=IOS)

      ns_service_decs%WVPA_RATE_TRACKER_INDEX = 0.
!
! 11/20/03. NOTE USING LF95 INITIALIZATION.

      ns_service_decs%WVPA_RES_TRACKER_INDEX = 0.
      ns_service_decs%WVPA_FUEL_TRACKER_INDEX = 0.
      ns_service_decs%WVPA_MEM_TRACKER_INDEX = 0.

      SERVICE_TRANS = 1
      IREC = 0
      DOWHILE (SERVICE_TRANS <= NUM_SERVICE_TRANS)
         IREC = IREC + 1
         READ(10,REC=IREC,IOSTAT=IOS) DELETE,
     +         ns_service_decs%SERVICE_NAME(SERVICE_TRANS),
     +         SERVICE_ID_NUMBER(SERVICE_TRANS),
     +         ns_service_decs%TYPE_OF_SERVICE(SERVICE_TRANS),
     +     ns_service_decs%SERVICE_COST_ASSIGNMENT(SERVICE_TRANS),
     +        ns_service_decs%SERVICE_EXPENSE_COLLECTION(SERVICE_TRANS),
     +         ns_service_decs%SERVICE_REPORTING_GROUP(SERVICE_TRANS),
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
     +   ns_service_decs%SERVICE_EXPENSE_CLASSIFICATION(SERVICE_TRANS),
     +         SERVICE_ACTIVE,
     +         ENERGY_TO_USE(SERVICE_TRANS),
     +         WVPA_RATE_TRACKER(SERVICE_TRANS),
     +         WVPA_RES_TRACKER,
     +         WVPA_FUEL_TRACKER,
     +         WVPA_MEM_TRACKER

         IF(IOS /= 0) EXIT
         IF(DELETE >= 8 .OR. SERVICE_ACTIVE == 'F') CYCLE


         SERVICE_AVAILABLE(SERVICE_TRANS) = MONTH_SERVICE_AVAILABLE +
     +                            100 * (YEAR_SERVICE_AVAILABLE - 1900)
         SERVICE_ENDS(SERVICE_TRANS) = MONTH_SERVICE_ENDS +
     +                                 100 * (YEAR_SERVICE_ENDS - 1900)
         IF(SERVICE_AVAILABLE(SERVICE_TRANS) > END_TEST_DATE .OR.
     +             SERVICE_ENDS(SERVICE_TRANS) < BEGIN_TEST_DATE) CYCLE

C        CLASS_NUM = ASSET_CLASS_NUM(SERVICE_TRANS) + 1
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

         ns_service_decs%WVPA_RATE_TRACKER_INDEX(SERVICE_TRANS) =
     +      FLOAT(WVPA_TRACKING_TYPE(WVPA_RATE_TRACKER(SERVICE_TRANS)))
! 11/20/03. FOR REPORTING PURPOSES (NOTE: R*4)
         ns_service_decs%WVPA_RES_TRACKER_INDEX(SERVICE_TRANS) =
     +             FLOAT(WVPA_RESOURCE_TRACKING_TYPE(WVPA_RES_TRACKER))
         ns_service_decs%WVPA_FUEL_TRACKER_INDEX(SERVICE_TRANS) =
     +                FLOAT(WVPA_FUEL_TRACKING_TYPE(WVPA_FUEL_TRACKER))
         ns_service_decs%WVPA_MEM_TRACKER_INDEX(SERVICE_TRANS) =
     +                 FLOAT(WVPA_MEM_TRACKING_TYPE(WVPA_MEM_TRACKER))

         IF(SERVICE_ENERGY_CHARGE(SERVICE_TRANS) <= 0.)
     +                SERVICE_ENRG_ESCALATION_VECTOR(SERVICE_TRANS) = 0
         IF(SERVICE_CAPACITY_CHARGE(SERVICE_TRANS) <= 0.)
     +                 SERVICE_CAP_ESCALATION_VECTOR(SERVICE_TRANS) = 0
         SERVICE_TRANS = SERVICE_TRANS + 1
      ENDDO
      CALL CLOSE_SERVICE_TRANS_FILE
      SERVICE_TRANS = MIN(SERVICE_TRANS-1,NUM_SERVICE_TRANS)
      ALLOCATE(ns_service_decs%CAPACITY_CHARGE(SERVICE_TRANS),
     +         ns_service_decs%ENERGY_CHARGE(SERVICE_TRANS),
     +         ns_service_decs%ANNUAL_CAPACITY_CHARGE(SERVICE_TRANS),
     +         ns_service_decs%ANNUAL_ENERGY_CHARGE(SERVICE_TRANS),
     +         ns_service_decs%CAPACITY_MULTIPLIER(SERVICE_TRANS),
     +         ns_service_decs%ENERGY_MULTIPLIER(SERVICE_TRANS),
     +         STAT=IOS)

      DO I = 1, 12
         SEASON_HOURS(I) = get_HOURS_IN_PERIOD(I)
      ENDDO

C ESTABLISH THE CURRENT PRICE

      DO I = 1, SERVICE_TRANS
         IF(SERVICE_CAPACITY_CHARGE(I) < 0.) THEN
            ns_service_decs%CAPACITY_CHARGE(I) =
     + GET_VAR(SERVICE_CAPACITY_CHARGE(I),
     +   INT2(1),ns_service_decs%SERVICE_NAME(I))

CPROBLEMWITH USING A POINTER FOR THE COST AND HAVE THE UPDATE MONTH NOT
C  BE JANUARY.

         ELSE
         ns_service_decs%CAPACITY_CHARGE(I) = SERVICE_CAPACITY_CHARGE(I)
         ENDIF
         IF(SERVICE_ENERGY_CHARGE(I) < 0.) THEN
           ns_service_decs%ENERGY_CHARGE(I) =
     + GET_VAR(SERVICE_ENERGY_CHARGE(I),INT2(1),
     +   ns_service_decs%SERVICE_NAME(I))
         ELSE
            ns_service_decs%ENERGY_CHARGE(I) = SERVICE_ENERGY_CHARGE(I)
         ENDIF
      ENDDO
C
      WABASH_VALLEY_ACTIVE = UTILITY_TYPE() == 'R'
      IF(.NOT. WABASH_VALLEY_ACTIVE) THEN
         FIRST_EL_SERVICE_WRITE = .FALSE.
         FIRST_CL_SERVICE_WRITE = .FALSE.
         FIRST_CT_SERVICE_WRITE = .FALSE.
         FIRST_OT_SERVICE_WRITE = .FALSE. ! OTHER SERVICES
      ENDIF
C
      CALL SET_UP_ST_CLASS_ARRAYS

      RETURN

      ENTRY INITIALIZE_SERVICE_VARS(YR)

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
            ns_service_decs%ANNUAL_CAPACITY_CHARGE(I) = 0.
            ns_service_decs%ANNUAL_ENERGY_CHARGE(I) = 0.
         ENDDO
      RETURN ! INITIALIZE_SERVICE_VARS

C UPDATE SERVICE CHARGES AND CAPACITY AND ENERGY PATTERN DISTRIBUTIONS


      ENTRY SERVICE_UPDATE_COSTS(ISEAS,YR)

      DO I = 1, SERVICE_TRANS
         IF(SERVICE_CAPACITY_PATTERN(I) < 0.) THEN
            ns_service_decs%CAPACITY_MULTIPLIER(I)=
     + GET_VAR(SERVICE_CAPACITY_PATTERN(I),
     +          ISEAS,ns_service_decs%SERVICE_NAME(I))
         ELSEIF(SERVICE_CAPACITY_PATTERN(I) > 0.) THEN
            ns_service_decs%CAPACITY_MULTIPLIER(I) =
     + SERVICE_CAPACITY_PATTERN(I)
         ELSE
            ns_service_decs%CAPACITY_MULTIPLIER(I) = 1.
         ENDIF
         IF(SERVICE_ENERGY_PATTERN(I) < 0.) THEN
            ns_service_decs%ENERGY_MULTIPLIER(I) =
     + GET_VAR(SERVICE_ENERGY_PATTERN(I),
     +       ISEAS,ns_service_decs%SERVICE_NAME(I))
         ELSEIF(SERVICE_ENERGY_PATTERN(I) > 0.) THEN
            ns_service_decs%ENERGY_MULTIPLIER(I) =
     + SERVICE_ENERGY_PATTERN(I)
         ELSE
            ns_service_decs%ENERGY_MULTIPLIER(I) = 1.
         ENDIF

! 10/21/98. GAT. RE-WROTE FOR WVPA

         IF(ISEAS == SERVICE_UPDATE_SEASON(I) .OR.
     +                                      YR > AVAIL_DATA_YEARS) THEN
            IF(SERVICE_CAPACITY_CHARGE(I) < 0.) THEN
               ns_service_decs%CAPACITY_CHARGE(I) =
     +                           GET_VAR(SERVICE_CAPACITY_CHARGE(I),YR,
     + ns_service_decs%SERVICE_NAME(I))
            ELSE
         ns_service_decs%CAPACITY_CHARGE(I) = SERVICE_CAPACITY_CHARGE(I)
            ENDIF
            IF(SERVICE_ENERGY_CHARGE(I) < 0.) THEN
       ns_service_decs%ENERGY_CHARGE(I) =
     + GET_VAR(SERVICE_ENERGY_CHARGE(I),YR,
     +                          ns_service_decs%SERVICE_NAME(I))
            ELSE
           ns_service_decs%ENERGY_CHARGE(I) = SERVICE_ENERGY_CHARGE(I)
            ENDIF
         ENDIF
         ns_service_decs%CAPACITY_CHARGE(I) =
     +               ESCALATED_MONTHLY_VALUE(
     +                              ns_service_decs%CAPACITY_CHARGE(I),
     +                              SERVICE_CAP_ESCALATION_VECTOR(I),
     +                              YR,ISEAS,SERVICE_UPDATE_SEASON(I))
         ns_service_decs%ENERGY_CHARGE(I) =
     +               ESCALATED_MONTHLY_VALUE(
     +                              ns_service_decs%ENERGY_CHARGE(I),
     +                              SERVICE_ENRG_ESCALATION_VECTOR(I),
     +                              YR,ISEAS,SERVICE_UPDATE_SEASON(I))

      ENDDO
      RETURN

C ENERGY LIMITED RESOURCE CALCULATIONS


      ENTRY EL_SERVICE_TRANS_CALCULATIONS(nr_NUNITS,ENERGY,
     +                                    CAPACITY,
     +                                    DATE1,DATE2,ISEAS)

         DO I = 1, SERVICE_TRANS
            IF(INDEX(SERVICE_LINK_TYPE(I),'EL') /= 0 .OR.
     +                        trim(SERVICE_LINK_TYPE(I)) == 'TG') THEN
               IF(FIRST_EL_SERVICE_WRITE .AND.
     +                           MONTHLY_SERVICE_REPORT_ACTIVE) THEN
C TAKE ALL EL SERVICES FOR FIRST WRITE
               ELSEIF(  .NOT. WABASH_VALLEY_ACTIVE .AND.
     +                  (SERVICE_AVAILABLE(I) > DATE2 .OR.
     +                                  SERVICE_ENDS(I) < DATE1)) THEN
                  CYCLE
               ENDIF
               DO L = 1, nr_NUNITS
                  IF((trim(SERVICE_LINK_TYPE(I)) == 'EL' .AND.
     +              INT(EL_RESOURCE_ID(L)) ==
     +                                  SERVICE_RESOURCE_PONTR(I)) .OR.
     +            (INDEX(SERVICE_LINK_TYPE(I),'TG') /= 0 .AND.
     +            HYDRO_TRANS_GROUP(L)==SERVICE_RESOURCE_PONTR(I)))THEN

                     IF(SERVICE_AVAILABLE(I) > DATE2 .OR.
     +                                    SERVICE_ENDS(I) < DATE1) THEN
                        SEASONAL_ENERGY = 0.
                        ns_service_decs%SEASONAL_CAPACITY = 0.
                        SEASONAL_ENERGY_CHARGE = 0.
                        SEASONAL_CAPACITY_CHARGE = 0.
                ELSE
        SEASONAL_ENERGY = ENERGY(L)*ns_service_decs%ENERGY_MULTIPLIER(I)
                   ns_service_decs%SEASONAL_CAPACITY = CAPACITY(L) *
     +                         ns_service_decs%CAPACITY_MULTIPLIER(I)
       SEASONAL_ENERGY_CHARGE = ns_service_decs%ENERGY_CHARGE(I) *
     +                                            SEASONAL_ENERGY
         SEASONAL_CAPACITY_CHARGE = ns_service_decs%CAPACITY_CHARGE(I) *
     +                            ns_service_decs%SEASONAL_CAPACITY
                     ENDIF

      IF(ns_service_decs%TYPE_OF_SERVICE(I) == 'T') THEN

        sec_arg_pass=SEASONAL_ENERGY_CHARGE/1000.

        CALL TRANSACTION_MANAGER(int(ISEAS,2),
     + I,
     +     ns_service_decs%SERVICE_COST_ASSIGNMENT(I),
     +     ns_service_decs%TYPE_OF_SERVICE(I),
     +     ns_service_decs%SERVICE_NAME(I),
     +     SEASONAL_ENERGY,
     +     sec_arg_pass,
     +     ns_service_decs%ENERGY_CHARGE(I),
     +     ns_service_decs%SEASONAL_CAPACITY,
     +     ns_service_decs%SEASONAL_CAPACITY_CHARGE,
     +     ns_service_decs%CAPACITY_CHARGE(I),
     +     ns_service_decs%SERVICE_EXPENSE_COLLECTION(I),
     +     ns_service_decs%SERVICE_REPORTING_GROUP(I),
     +     MONTHLY_SERVICE_REPORT_ACTIVE,
     +     LOGICAL1_TRUE,
     +     ns_service_decs%WVPA_RATE_TRACKER_INDEX(I),
     +     ns_service_decs%WVPA_RES_TRACKER_INDEX(I),
     +     ns_service_decs%WVPA_FUEL_TRACKER_INDEX(I),
     +     ns_service_decs%WVPA_MEM_TRACKER_INDEX(I),
     +   ns_service_decs%SERVICE_EXPENSE_CLASSIFICATION(I))

      ENDIF


                     CALL STORE_ST_COST_AND_REVENUE_DATA(I,
     +                                    SEASONAL_ENERGY_CHARGE/1000.,
     +                                     SEASONAL_CAPACITY_CHARGE,
     +                                     SEASONAL_ENERGY,
     +                         ns_service_decs%SEASONAL_CAPACITY)
                 ENDIF
               ENDDO ! EL UNITS COUNTER
            ENDIF ! YES EL UNIT
         ENDDO ! EL SERVICES COUNTER
         IF(FIRST_EL_SERVICE_WRITE) then
            FIRST_EL_SERVICE_WRITE = .FALSE.
         endif
      RETURN

C CAPACITY LIMITED RESOURCE CALCULATIONS
C


C CONTRACT RESOURCE CALCULATIONS


      ENTRY CT_SERVICE_TRANS_CALCULATIONS(nr_NUNITS,ENERGY,
     +                                    CAPACITY,res_id_loc,
     +                                    DATE1,DATE2,ISEAS)

      DO I = 1, SERVICE_TRANS
         IF(INDEX(SERVICE_LINK_TYPE(I),'CT')/= 0) THEN
            IF(FIRST_CT_SERVICE_WRITE .AND.
     +                              MONTHLY_SERVICE_REPORT_ACTIVE) THEN
C TAKE ALL CT SERVICES FOR FIRST WRITE
            ELSEIF(  .NOT. WABASH_VALLEY_ACTIVE .AND.
     +                  (SERVICE_AVAILABLE(I) > DATE2 .OR.
     +                                   SERVICE_ENDS(I) < DATE1)) THEN
               CYCLE
            ENDIF
            DO L = 1, nr_NUNITS
               IF(res_id_loc(L) ==
     +                          SERVICE_RESOURCE_PONTR(I)) THEN
                  IF(SERVICE_AVAILABLE(I) > DATE2 .OR.
     +                                    SERVICE_ENDS(I) < DATE1) THEN
                     SEASONAL_ENERGY = 0.
                     ns_service_decs%SEASONAL_CAPACITY = 0.
                     SEASONAL_ENERGY_CHARGE = 0.
                     SEASONAL_CAPACITY_CHARGE = 0.
                  ELSE
       SEASONAL_ENERGY =ENERGY(L) *
     + ns_service_decs%ENERGY_MULTIPLIER(I) *
     +                                 SEASON_HOURS(ISEAS)
        ns_service_decs%SEASONAL_CAPACITY = CAPACITY(L) *
     +                        ns_service_decs%CAPACITY_MULTIPLIER(I)
            SEASONAL_ENERGY_CHARGE = ns_service_decs%ENERGY_CHARGE(I) *
     +                                                 SEASONAL_ENERGY
         SEASONAL_CAPACITY_CHARGE = ns_service_decs%CAPACITY_CHARGE(I) *
     +                               ns_service_decs%SEASONAL_CAPACITY
                  ENDIF

       sec_arg_pass=SEASONAL_ENERGY_CHARGE/1000.

                  IF(ns_service_decs%TYPE_OF_SERVICE(I) == 'T') THEN
                     CALL TRANSACTION_MANAGER(ISEAS,I,
     +        ns_service_decs%SERVICE_COST_ASSIGNMENT(I),
     +        ns_service_decs%TYPE_OF_SERVICE(I),
     +        ns_service_decs%SERVICE_NAME(I),
     +        SEASONAL_ENERGY,
     +        sec_arg_pass,
     +        ns_service_decs%ENERGY_CHARGE(I),
     +        ns_service_decs%SEASONAL_CAPACITY,
     +        SEASONAL_CAPACITY_CHARGE,
     +        ns_service_decs%CAPACITY_CHARGE(I),
     +       ns_service_decs%SERVICE_EXPENSE_COLLECTION(I),
     +        ns_service_decs%SERVICE_REPORTING_GROUP(I),
     +       MONTHLY_SERVICE_REPORT_ACTIVE,
     +        LOGICAL1_TRUE,
     +        ns_service_decs%WVPA_RATE_TRACKER_INDEX(I),
     +        ns_service_decs%WVPA_RES_TRACKER_INDEX(I),
     +        ns_service_decs%WVPA_FUEL_TRACKER_INDEX(I),
     +        ns_service_decs%WVPA_MEM_TRACKER_INDEX(I),
     +   ns_service_decs%SERVICE_EXPENSE_CLASSIFICATION(I))
                  ENDIF
                  CALL STORE_ST_COST_AND_REVENUE_DATA(I,
     +                                    SEASONAL_ENERGY_CHARGE/1000.,
     +                                     SEASONAL_CAPACITY_CHARGE,
     +                                     SEASONAL_ENERGY,
     +                        ns_service_decs%SEASONAL_CAPACITY)
               ENDIF
            ENDDO ! CONTRACTS COUNTER
         ENDIF ! YES CONTRACT
      ENDDO ! TRANSACTIONS COUNTER
      IF(FIRST_CT_SERVICE_WRITE) FIRST_CT_SERVICE_WRITE = .FALSE.
      RETURN

C CALCULATE MONTHLY SERVICE COSTS FOR NON RESOURCE SPECIFIC REFERENCES


      ENTRY SERVICE_TRANS_CALCULATIONS(ISEAS,YR,DATE1,DATE2)


      DO I = 1, SERVICE_TRANS

C CAPACITY COSTING

         ! ONLY STORE VALUES IF TRANSACTION TYPE FOUND
         TRANSACTION_FOUND = .FALSE.
         ! ONLY WRITE NON-SPECIFIC TRANSACTIONS
         WRITE_IT_MONTHLY = .FALSE.
         IF(FIRST_OT_SERVICE_WRITE .AND.
     +                              MONTHLY_SERVICE_REPORT_ACTIVE) THEN
C TAKE ALL OT SERVICES FOR FIRST WRITE
         ELSEIF(  .NOT. WABASH_VALLEY_ACTIVE .AND.
     +                  (SERVICE_AVAILABLE(I) > DATE2 .OR.
     +                                   SERVICE_ENDS(I) < DATE1)) THEN
            CYCLE
         ENDIF

         SEASONAL_CAPACITY_CHARGE = 0.
         SEASONAL_ENERGY_CHARGE = 0.
         SEASONAL_ENERGY = 0.
         ns_service_decs%SEASONAL_CAPACITY = 0.
         SELECT CASE (trim(SERVICE_LINK_TYPE(I)))
C
         CASE ('CG')
C
            IF(SERVICE_AVAILABLE(I) > DATE2 .OR.
     +                                    SERVICE_ENDS(I) < DATE1) THEN
               SEASONAL_ENERGY = 0.
               ns_service_decs%SEASONAL_CAPACITY = 0.
            ELSE
               SEASONAL_ENERGY = ns_service_decs%ENERGY_MULTIPLIER(I) *
     +           GET_CUST_GROUP_ENERGY(SERVICE_RESOURCE_PONTR(I),ISEAS)

            ns_service_decs%SEASONAL_CAPACITY =
     + ns_service_decs%CAPACITY_MULTIPLIER(I) *
     +             GET_CUST_GROUP_PEAK(SERVICE_RESOURCE_PONTR(I),ISEAS)

            ENDIF

         SEASONAL_CAPACITY_CHARGE = ns_service_decs%CAPACITY_CHARGE(I) *
     +                                ns_service_decs%SEASONAL_CAPACITY
           SEASONAL_ENERGY_CHARGE = ns_service_decs%ENERGY_CHARGE(I) *
     + SEASONAL_ENERGY
            WRITE_IT_MONTHLY = .TRUE.
            TRANSACTION_FOUND = .TRUE.
         CASE ('D1','D2','D3','D4','D5','D6')
            CLASS_CHR = SERVICE_LINK_TYPE(I)(2:2)
            CLASS = INDEX('123456',CLASS_CHR)
            CALL DSM_PEAK_ENER_RESERVE_ALLOC(PEAK_RESERVE,
     +                                        ENER_RESERVE,CLASS,ISEAS)
C
            IF(SERVICE_AVAILABLE(I) > DATE2 .OR.
     +                                    SERVICE_ENDS(I) < DATE1) THEN
               SEASONAL_ENERGY = 0.
               ns_service_decs%SEASONAL_CAPACITY = 0.
            ELSE
               SEASONAL_ENERGY = ns_service_decs%ENERGY_MULTIPLIER(I) *
     +                               (FORECAST_ENERGY(1,ISEAS,CLASS) +
     +                                 FORECAST_ENERGY(2,ISEAS,CLASS) +
     +                                                    ENER_RESERVE)
            ns_service_decs%SEASONAL_CAPACITY = 
     + ns_service_decs%CAPACITY_MULTIPLIER(I) *
     +                   (MAX(FORECAST_COINCIDENT_PEAK(1,ISEAS,CLASS),
     +                       FORECAST_COINCIDENT_PEAK(2,ISEAS,CLASS)) +
     +                                                    PEAK_RESERVE)
            ENDIF

            SEASONAL_ENERGY_CHARGE = ns_service_decs%ENERGY_CHARGE(I)
     +  * SEASONAL_ENERGY
         SEASONAL_CAPACITY_CHARGE = ns_service_decs%CAPACITY_CHARGE(I) *
     +                                ns_service_decs%SEASONAL_CAPACITY
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
        SEASONAL_CAPACITY_CHARGE = ns_service_decs%CAPACITY_CHARGE(I) *
     +                        ns_service_decs%CAPACITY_MULTIPLIER(I) *
     +                   MAX(FORECAST_COINCIDENT_PEAK(1,ISEAS,CLASS),
     +                       FORECAST_COINCIDENT_PEAK(2,ISEAS,CLASS))
            ELSE
               ns_service_decs%ANNUAL_CAPACITY_CHARGE(I) = 
     + ns_service_decs%ANNUAL_CAPACITY_CHARGE(I) +
     +                         ns_service_decs%CAPACITY_MULTIPLIER(I) *
     +                              ns_service_decs%CAPACITY_CHARGE(I)
            ENDIF
            SEASONAL_ENERGY_CHARGE = ns_service_decs%ENERGY_CHARGE(I) *
     +                        ns_service_decs%ENERGY_MULTIPLIER(I) *
     +                              (FORECAST_ENERGY(1,ISEAS,CLASS) +
     +                                FORECAST_ENERGY(2,ISEAS,CLASS))
            TRANSACTION_FOUND = .TRUE.
         CASE ('SF')
C
            IF(SERVICE_AVAILABLE(I) > DATE2 .OR.
     +                                    SERVICE_ENDS(I) < DATE1) THEN
               SEASONAL_ENERGY = 0.
               ns_service_decs%SEASONAL_CAPACITY = 0.
            ELSE
               SEASONAL_ENERGY = ns_service_decs%ENERGY_MULTIPLIER(I) *
     +                                      SEASON_SYSTEM_ENERGY(ISEAS)
            ns_service_decs%SEASONAL_CAPACITY = 
     + ns_service_decs%CAPACITY_MULTIPLIER(I) *
     +                                       SEASON_SYSTEM_PEAK(ISEAS)
            ENDIF

         SEASONAL_CAPACITY_CHARGE = ns_service_decs%CAPACITY_CHARGE(I) *
     +       ns_service_decs%SEASONAL_CAPACITY
            SEASONAL_ENERGY_CHARGE = ns_service_decs%ENERGY_CHARGE(I)
     + * SEASONAL_ENERGY
            WRITE_IT_MONTHLY = .TRUE.
            TRANSACTION_FOUND = .TRUE.
         CASE ('PA')
            IF(SERVICE_AVAILABLE(I) > DATE2 .OR.
     +                                   SERVICE_ENDS(I) < DATE1) CYCLE
            ns_service_decs%ANNUAL_CAPACITY_CHARGE(I) = 
     + ns_service_decs%ANNUAL_CAPACITY_CHARGE(I) +
     +                         ns_service_decs%CAPACITY_MULTIPLIER(I) *
     +                            ns_service_decs%CAPACITY_CHARGE(I)
            SEASONAL_ENERGY_CHARGE = ns_service_decs%ENERGY_CHARGE(I) *
     +                   ns_service_decs%ENERGY_MULTIPLIER(I) *
     +                                    SEASON_SYSTEM_ENERGY(ISEAS)

        CASE ('None','N','No')
            IF(SERVICE_ANNUAL_CAPACITY(I) < 0.) THEN
                  ANNUAL_CAPACITY = GET_VAR(SERVICE_ANNUAL_CAPACITY(I),
     +                           YR,ns_service_decs%SERVICE_NAME(I))
            ELSE
               ANNUAL_CAPACITY = SERVICE_ANNUAL_CAPACITY(I)
            ENDIF
            IF(SERVICE_ANNUAL_ENERGY(I) < 0.) THEN
               ANNUAL_ENERGY = GET_VAR(SERVICE_ANNUAL_ENERGY(I),YR,
     +        ns_service_decs%SERVICE_NAME(I))
            ELSE
               ANNUAL_ENERGY = SERVICE_ANNUAL_ENERGY(I)
            ENDIF
C
            IF(SERVICE_AVAILABLE(I) > DATE2 .OR.
     +                                    SERVICE_ENDS(I) < DATE1) THEN
               SEASONAL_ENERGY = 0.
               ns_service_decs%SEASONAL_CAPACITY = 0.
            ELSE
               SEASONAL_ENERGY =ANNUAL_ENERGY * 
     + ns_service_decs%ENERGY_MULTIPLIER(I)/12.
               ns_service_decs%SEASONAL_CAPACITY=
     + ANNUAL_CAPACITY*ns_service_decs%CAPACITY_MULTIPLIER(I)
            ENDIF

            SEASONAL_ENERGY_CHARGE = ns_service_decs%ENERGY_CHARGE(I)
     + * SEASONAL_ENERGY
         SEASONAL_CAPACITY_CHARGE = ns_service_decs%CAPACITY_CHARGE(I) *
     +                               ns_service_decs%SEASONAL_CAPACITY
            WRITE_IT_MONTHLY = .TRUE.
            TRANSACTION_FOUND = .TRUE.
         END SELECT

c force in WVPA actual values

         RPT_ENERGY_CHARGE = ns_service_decs%ENERGY_CHARGE(I)
         RPT_CAPACITY_CHARGE = ns_service_decs%CAPACITY_CHARGE(I)

         IF(ns_service_decs%TYPE_OF_SERVICE(I) == 'T') THEN
            CALL TRANSACTION_MANAGER(ISEAS,I,
     +   ns_service_decs%SERVICE_COST_ASSIGNMENT(I),
     +   ns_service_decs%TYPE_OF_SERVICE(I),
     +   ns_service_decs%SERVICE_NAME(I),
     +   SEASONAL_ENERGY,
     +   SEASONAL_ENERGY_CHARGE/1000.,
     +   RPT_ENERGY_CHARGE,
     +   ns_service_decs%SEASONAL_CAPACITY,
     +   SEASONAL_CAPACITY_CHARGE,
     +   RPT_CAPACITY_CHARGE,
     +   ns_service_decs%SERVICE_EXPENSE_COLLECTION(I),
     +   ns_service_decs%SERVICE_REPORTING_GROUP(I),
     +   MONTHLY_SERVICE_REPORT_ACTIVE,
     +   WRITE_IT_MONTHLY,
     +   ns_service_decs%WVPA_RATE_TRACKER_INDEX(I),
     +   ns_service_decs%WVPA_RES_TRACKER_INDEX(I),
     +   ns_service_decs%WVPA_FUEL_TRACKER_INDEX(I),
     +   ns_service_decs%WVPA_MEM_TRACKER_INDEX(I),
     +   ns_service_decs%SERVICE_EXPENSE_CLASSIFICATION(I))
         ENDIF
         IF(TRANSACTION_FOUND) THEN
            CALL STORE_ST_COST_AND_REVENUE_DATA(I,
     +                                    SEASONAL_ENERGY_CHARGE/1000.,
     +                                     SEASONAL_CAPACITY_CHARGE,
     +                                     SEASONAL_ENERGY,
     +                         ns_service_decs%SEASONAL_CAPACITY)
         ENDIF
      ENDDO ! TRANSACTIONS COUNTER
      IF(FIRST_OT_SERVICE_WRITE) FIRST_OT_SERVICE_WRITE = .FALSE.
      RETURN


      ENTRY GET_SERVICE_TRANS_TRACKERS(R_TRANS_NO,
     +                                 R_RES_TRACKER,
     +                                 R_FUEL_TRACKER,
     +                                 R_RATE_TRACKER,
     +                                 R_MEM_TRACKER)

         R_RES_TRACKER =
     + ns_service_decs%WVPA_RES_TRACKER_INDEX(R_TRANS_NO)
         R_FUEL_TRACKER = 
     + ns_service_decs%WVPA_FUEL_TRACKER_INDEX(R_TRANS_NO)
         R_MEM_TRACKER = 
     + ns_service_decs%WVPA_MEM_TRACKER_INDEX(R_TRANS_NO)
      RETURN




C CALCULATE ANNUAL PEAK COSTS AND NON-LINKED COSTS


      ENTRY ANNUAL_SERVICE_TRANSACTIONS(YR,ANNUAL_PEAK,DATE1,DATE2)


C CALCULATE THE ANNUAL SERVICE TRANSACTION COSTS

      DO I = 1, SERVICE_TRANS
        IF(SERVICE_AVAILABLE(I)>DATE2 .OR. SERVICE_ENDS(I)<DATE1) CYCLE
         SELECT CASE (SERVICE_LINK_TYPE(I))
         CASE ('PA','P1','P2','P3','P4','P5','P6')
            IF(SERVICE_LINK_TYPE(I)(1:2) == 'PA') THEN
               SEASONAL_CAPACITY_CHARGE = 
     + ns_service_decs%ANNUAL_CAPACITY_CHARGE(I) *  ANNUAL_PEAK
               ANNUAL_ENERGY = 0.
            ELSEIF(SERVICE_LINK_TYPE(I)(1:1) == 'P') THEN
               CLASS_CHR = SERVICE_LINK_TYPE(I)(2:2)
               CLASS = INDEX('123456',CLASS_CHR)
               SEASONAL_CAPACITY_CHARGE = 
     + ns_service_decs%ANNUAL_CAPACITY_CHARGE(I) *
     +               ANNUAL_COINCIDENT_PEAK(CLASS)
               ANNUAL_ENERGY = 0.
            ENDIF

            IF(ns_service_decs%TYPE_OF_SERVICE(I) == 'T') THEN
               MO = 0
               CALL TRANSACTION_MANAGER(MO,I,
     +      ns_service_decs%SERVICE_COST_ASSIGNMENT(I),
     +      ns_service_decs%TYPE_OF_SERVICE(I),
     +      ns_service_decs%SERVICE_NAME(I),
     +      SEASONAL_ENERGY,
     +      SEASONAL_ENERGY_CHARGE/1000.,
     +      ns_service_decs%ENERGY_CHARGE(I),
     +      ns_service_decs%SEASONAL_CAPACITY,
     +      SEASONAL_CAPACITY_CHARGE,
     +      ns_service_decs%CAPACITY_CHARGE(I),
     +      ns_service_decs%SERVICE_EXPENSE_COLLECTION(I),
     +      ns_service_decs%SERVICE_REPORTING_GROUP(I),
     +      MONTHLY_SERVICE_REPORT_ACTIVE,
     +      LOGICAL1_TRUE,
     +      ns_service_decs%WVPA_RATE_TRACKER_INDEX(I),
     +      ns_service_decs%WVPA_RES_TRACKER_INDEX(I),
     +      ns_service_decs%WVPA_FUEL_TRACKER_INDEX(I),
     +      ns_service_decs%WVPA_MEM_TRACKER_INDEX(I),
     +   ns_service_decs%SERVICE_EXPENSE_CLASSIFICATION(I))
            ENDIF
            CALL STORE_ANNUAL_ST_COST_DATA(I,
     +                                    SEASONAL_ENERGY_CHARGE/1000.,
     +                                     SEASONAL_CAPACITY_CHARGE,
     +                                     SEASONAL_ENERGY,
     +                         ns_service_decs%SEASONAL_CAPACITY)
         END SELECT
      ENDDO
      RETURN

C END POINT FINISHED DEALLOCATE ARRAYS


      ENTRY CLOSE_SERVICE_TRANSACTIONS

      DEALLOCATE(ns_service_decs%CAPACITY_CHARGE,
     +           ns_service_decs%ENERGY_CHARGE,
     +           ns_service_decs%ANNUAL_CAPACITY_CHARGE,
     +           ns_service_decs%ANNUAL_ENERGY_CHARGE,
     +           ns_service_decs%CAPACITY_MULTIPLIER,
     +           ns_service_decs%ENERGY_MULTIPLIER,
     +           STAT=IOS)
      DEALLOCATE(ns_service_decs%SERVICE_NAME,
     +           SERVICE_ID_NUMBER,
     +           ns_service_decs%TYPE_OF_SERVICE,
     +           ns_service_decs%SERVICE_COST_ASSIGNMENT,
     +           ns_service_decs%SERVICE_EXPENSE_COLLECTION,
     +           ns_service_decs%SERVICE_REPORTING_GROUP,
C
     +           SERVICE_AVAILABLE,
     +           SERVICE_ENDS,

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

     +           SERVICE_INTRA_COMPY_TRANSACTION,
     +           ENERGY_TO_USE,
     +           WVPA_RATE_TRACKER,
     +           ns_service_decs%WVPA_RATE_TRACKER_INDEX,
     +           ns_service_decs%WVPA_RES_TRACKER_INDEX,
     +           ns_service_decs%WVPA_FUEL_TRACKER_INDEX,
     +           ns_service_decs%WVPA_MEM_TRACKER_INDEX,
     +           INTRA_ASSET_CLASS_ID,
     +           INTRA_ASSET_CLASS_ALLOC_VECTOR,
     +           INTRA_ACCOUNT_CLASSIFICATION,
     +           INTRA_EXPENSE_COLLECTION,
     +           ns_service_decs%SERVICE_EXPENSE_CLASSIFICATION,
     +           SERVICE_FUEL_CHARGE,
     +           SERVICE_FUEL_ESCALATION_VECTOR,
     +           STAT=IOS)
      RETURN

C ANNUAL SERVICE COST REPORT


      ENTRY SERVICE_COST_ANNUAL_REPORT(KPCVAL)

      WRITE(9,'(//38X,A/)')
     +                'Transmission, Wheeling, Dispatching, etc. Costs'
      IF(kpcval) THEN
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

      ENTRY UPDATE_SERVICE_REPORT_SWITCH

         IF(.NOT. TESTING_PLAN) THEN
            MONTHLY_SERVICE_REPORT_ACTIVE = SERVICE_REPORT() /= 'F'
         ELSE
            MONTHLY_SERVICE_REPORT_ACTIVE = .FALSE.
         ENDIF
      RETURN

      ENTRY GET_NO_OF_SERVICE_TRANS(R_SERVICE_TRANS)

         R_SERVICE_TRANS = SERVICE_TRANS
      RETURN

      ENTRY GET_TRANS_VARIABLES(R_SERVICE_TRANS,R_NAME,R_COST_ASSIGN,
     +                                    R_EXPENSE_ASSIGN,R_TRAN_TYPE)

! 11/03/03. MAJOR REWRITE

         DO I = 1, SERVICE_TRANS
            R_NAME(I) = ns_service_decs%SERVICE_NAME(I)
           R_COST_ASSIGN(I) = ns_service_decs%SERVICE_COST_ASSIGNMENT(I)
            R_EXPENSE_ASSIGN(I) =  
     + ns_service_decs%SERVICE_EXPENSE_COLLECTION(I)
            R_TRAN_TYPE(I) = ns_service_decs%TYPE_OF_SERVICE(I)
         ENDDO
      RETURN


      ENTRY STORE_ST_COST_AND_REVENUE_DATA(UNIT_NO,
     +                                     VAR_COST,
     +                                     ST_FIXED_COST,
     +                                     TRANS_ENERGY,
     +                                     TRANS_CAPACITY)
C**********************************************************************


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


C MONTHLY VALUES

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


      ENTRY STORE_ANNUAL_ST_COST_DATA(UNIT_NO,
     +                                VAR_COST,
     +                                ST_FIXED_COST,
     +                                TRANS_ENERGY,
     +                                TRANS_CAPACITY)
C**********************************************************************


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


C MONTHLY VALUES

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
C**********************************************************************
      ENTRY STORE_SERVICE_MONTH(R_ISEAS)
C**********************************************************************
         CURRENT_MONTH = R_ISEAS
      RETURN
C**********************************************************************
      ENTRY CALC_ST_ANN_ASSET_CLASS
C**********************************************************************


         IF(SERVICE_TRANS <= 0) RETURN
         IF(.NOT. ALLOCATED(ST_ANN_CLASS_ATL_EXPENSE)) RETURN
         IF(.NOT. ALLOCATED(MONTHLY_ST_ENERGY)) RETURN
         CURRENT_YEAR = YEAR+BASE_YEAR
         CURRENT_YEAR_COMPARISON = (CURRENT_YEAR-1900)*100

         ST_ANN_CLASS_EXPENSE_CAPACITY = 0.
         ST_ANN_CLASS_EXPENSE_ENERGY = 0.
         ST_ANN_CLASS_REVENUE_ENERGY = 0.
         ST_ANN_CLASS_REVENUE_CAPACITY = 0.

         ST_ANN_CLASS_ATL_EXPENSE = 0.
         ST_ANN_CLASS_BTL_EXPENSE = 0.
         ST_ANN_CLASS_ADJ_CLAUSE = 0.
         ST_BTL_LEASE_PAYMENT = 0.
         ST_NF_RATEBASE = 0.

         ST_ANN_CLASS_REVENUE = 0.
         ST_ANN_CLASS_EXPENSE = 0.

         IF(MONTHLY_SERVICE_REPORT_ACTIVE .AND.
     +                              SERVICE_TRANS_REPORT_NOT_OPEN) THEN
            TRANS_RPT_NO = ANNUAL_SERVICE_TRANS_HEADER(TRANS_RPT_REC)
            SERVICE_TRANS_REPORT_NOT_OPEN = .FALSE.
         ENDIF

         INCLUDE_FIXED_COSTS_ADJ_CLAUSE = FIXED_COSTS_IN_ADJ_CLAUSE()

         MO = 0
         DO UNIT = 1, SERVICE_TRANS

          IF(SERVICE_AVAILABLE(UNIT) - CURRENT_YEAR_COMPARISON>12 .OR.
     +         CURRENT_YEAR_COMPARISON - SERVICE_ENDS(UNIT) > 12) CYCLE


            IF(MONTHLY_SERVICE_REPORT_ACTIVE) THEN
               TRANSACTION_DESCRIPTION =
     +                       trim(LEFT_JUSTIFY_I2_IN_STR(UNIT))//' '//
     +  ns_service_decs%SERVICE_NAME(UNIT)
       IF(ns_service_decs%SERVICE_COST_ASSIGNMENT(UNIT) == 'R') THEN
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

             IF(SERVICE_INTRA_COMPY_TRANSACTION(UNIT) == 'Y') THEN
C ALLOCATE TO TOTAL COMPANY AND ASSET CLASSES
               TRANSACTION_AMOUNT = MONTHLY_ST_VAR_COST(MO,UNIT) +
     +                                   MONTHLY_ST_FIXED_COST(MO,UNIT)
       IF(ns_service_decs%SERVICE_COST_ASSIGNMENT(UNIT) == REVENUE) THEN
                   REV_TYPE = INCOME_STATEMENT_POSITION(
     +                            SERVICE_REVENUE_CLASSIFICATION(UNIT))
                    EXP_TYPE = INCOME_STATEMENT_POSITION(
     +                              INTRA_ACCOUNT_CLASSIFICATION(UNIT))
                 IF(INDEX(INTRA_EXPENSE_COLLECTION(UNIT),'BTL')/=0)THEN
                     IF(EXP_TYPE == 22) THEN ! Lease Expense
                       ST_BTL_LEASE_PAYMENT(MO,-1) = TRANSACTION_AMOUNT
     +                                    + ST_BTL_LEASE_PAYMENT(MO,-1)
                     ! Nuc Fuel Expense
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
     +      ns_service_decs%SERVICE_EXPENSE_CLASSIFICATION(UNIT))
                  IF(INDEX(
     + ns_service_decs%SERVICE_EXPENSE_COLLECTION(UNIT),'BTL')
     +                                                       /= 0) THEN
                     IF(EXP_TYPE == 22) THEN
                       ST_BTL_LEASE_PAYMENT(MO,-1) = TRANSACTION_AMOUNT
     +                                    + ST_BTL_LEASE_PAYMENT(MO,-1)
                     ! Nuc Fuel Expense
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

                CLASS_POINTER = 1
                DO
                   ASSET_CLASS_loc = ASSET_CLASS_LIST(CLASS_POINTER)
                   CALL CHECK_IF_CLASS_DEFINED(ASSET_CLASS_loc)
                   ASSET_CLASS_loc = ASSET_CLASS_loc + 1

                 ASSET_ALLOCATOR = ASSET_ALLOCATION_LIST(CLASS_POINTER)
                  IF(ASSET_ALLOCATOR < 0.) THEN
                     ALLOCATION_VECTOR = ABS(ASSET_ALLOCATOR)
                     CALL GET_ASSET_VAR(ALLOCATION_VECTOR,
     +                                     DUMMY_TYPE,ALLOCATION_VALUE)
                     ASSET_ALLOCATOR =
     +                     ALLOCATION_VALUE(MIN(AVAIL_DATA_YEARS,YEAR))
                  ENDIF

                  ASSET_ALLOCATOR = ASSET_ALLOCATOR/100.
                  TRANSACTION_AMOUNT = (MONTHLY_ST_VAR_COST(MO,UNIT) +
     +                                 MONTHLY_ST_FIXED_COST(MO,UNIT))*
     +                                                  ASSET_ALLOCATOR

       IF(ns_service_decs%SERVICE_COST_ASSIGNMENT(UNIT) == REVENUE) THEN
                     ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS_loc,EXP_TYPE) =
     + ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS_loc,EXP_TYPE)
     +                     + TRANSACTION_AMOUNT
                     IF(INDEX(INTRA_EXPENSE_COLLECTION(UNIT),'Adj')
     +                                                      /= 0) THEN
                        IF(INCLUDE_FIXED_COSTS_ADJ_CLAUSE) THEN
                           ST_ANN_CLASS_ADJ_CLAUSE(MO,ASSET_CLASS_loc) =
     +     ST_ANN_CLASS_ADJ_CLAUSE(MO,ASSET_CLASS_loc)
     +                           + TRANSACTION_AMOUNT
                        ELSE
                          ST_ANN_CLASS_ADJ_CLAUSE(MO,ASSET_CLASS_loc) =
     +                      ST_ANN_CLASS_ADJ_CLAUSE(MO,ASSET_CLASS_loc)
     +                           + ASSET_ALLOCATOR *
     +                                     MONTHLY_ST_VAR_COST(MO,UNIT)
                        ENDIF
                     ENDIF
                   ELSE
                     ST_ANN_CLASS_REVENUE(MO,ASSET_CLASS_loc,REV_TYPE) =
     +                 ST_ANN_CLASS_REVENUE(MO,ASSET_CLASS_loc,REV_TYPE)
     +                     + TRANSACTION_AMOUNT
                   ENDIF

                   CLASS_POINTER = CLASS_POINTER + 1
                   IF(ASSET_CLASS_LIST(CLASS_POINTER) == 0 .OR.
     +                           CLASS_POINTER > AVAIL_DATA_YEARS) then
					EXIT
				   endif
                ENDDO
             ENDIF ! END INTEA-COMPANY STUFF



C
            ASSET_CLASS_loc = ASSET_CLASS_NUM(UNIT)
            ASSET_ALLOCATION_VECTOR = ASSET_CLASS_VECTOR(UNIT)

            IF(ASSET_CLASS_loc < 0) THEN
               CALL GET_ASSET_VAR(ABS(ASSET_CLASS_loc),
     +                                 DUMMY_TYPE,ASSET_CLASS_LIST)
               CALL GET_ASSET_VAR(ABS(ASSET_ALLOCATION_VECTOR),
     +                                DUMMY_TYPE,ASSET_ALLOCATION_LIST)
            ELSE
               ASSET_CLASS_LIST(1) = ASSET_CLASS_loc
               ASSET_CLASS_LIST(2) = 0.
               ASSET_ALLOCATION_LIST(1) = 100.
               ASSET_ALLOCATION_LIST(2) = 0.
            ENDIF
            CLASS_POINTER = 1

            DO
               ASSET_CLASS_loc = ASSET_CLASS_LIST(CLASS_POINTER)
               CALL CHECK_IF_CLASS_DEFINED(ASSET_CLASS_loc)
               ASSET_CLASS_loc = ASSET_CLASS_loc + 1

               ASSET_ALLOCATOR = ASSET_ALLOCATION_LIST(CLASS_POINTER)
               IF(ASSET_ALLOCATOR < 0.) THEN
                  ALLOCATION_VECTOR = ABS(ASSET_ALLOCATOR)
                  CALL GET_ASSET_VAR(ALLOCATION_VECTOR,
     +                                     DUMMY_TYPE,ALLOCATION_VALUE)
                  ASSET_ALLOCATOR =
     +                     ALLOCATION_VALUE(MIN(AVAIL_DATA_YEARS,YEAR))
               ENDIF

               ASSET_ALLOCATOR = ASSET_ALLOCATOR/100.

               DO MO = 1, 12
                  IF(.NOT. TRANSFER_TRANSACT_ANALYST_RESULTS(MO)
     +                                              .AND. WVPA()) CYCLE

       IF(ns_service_decs%SERVICE_COST_ASSIGNMENT(UNIT) == REVENUE) THEN
                     ST_ANN_CLASS_REVENUE_CAPACITY(MO,ASSET_CLASS_loc) =
     +                 ST_ANN_CLASS_REVENUE_CAPACITY(MO,ASSET_CLASS_loc)
     +                 + ASSET_ALLOCATOR * MONTHLY_ST_CAPACITY(MO,UNIT)
                     ST_ANN_CLASS_REVENUE_ENERGY(MO,ASSET_CLASS_loc) =
     +                   ST_ANN_CLASS_REVENUE_ENERGY(MO,ASSET_CLASS_loc)
     +                   + ASSET_ALLOCATOR * MONTHLY_ST_ENERGY(MO,UNIT)

                     ST_REVENUE = ASSET_ALLOCATOR *
     +                                  (MONTHLY_ST_VAR_COST(MO,UNIT) +
     +                                  MONTHLY_ST_FIXED_COST(MO,UNIT))
                      REV_TYPE = INCOME_STATEMENT_POSITION(
     +                            SERVICE_REVENUE_CLASSIFICATION(UNIT))
C
                     ST_ANN_CLASS_REVENUE(MO,ASSET_CLASS_loc,REV_TYPE) =
     +                 ST_ANN_CLASS_REVENUE(MO,ASSET_CLASS_loc,REV_TYPE)
     +                     + ST_REVENUE
                  ELSE

                     ST_ANN_CLASS_EXPENSE_CAPACITY(MO,ASSET_CLASS_loc) =
     +               ST_ANN_CLASS_EXPENSE_CAPACITY(MO,ASSET_CLASS_loc)
     +                 + ASSET_ALLOCATOR * MONTHLY_ST_CAPACITY(MO,UNIT)
                     ST_ANN_CLASS_EXPENSE_ENERGY(MO,ASSET_CLASS_loc) =
     +                  ST_ANN_CLASS_EXPENSE_ENERGY(MO,ASSET_CLASS_loc)
     +                   + ASSET_ALLOCATOR * MONTHLY_ST_ENERGY(MO,UNIT)
                     ST_EXPENSE = ASSET_ALLOCATOR *
     +                                 (MONTHLY_ST_VAR_COST(MO,UNIT) +
     +                                  MONTHLY_ST_FIXED_COST(MO,UNIT))
                       EXP_TYPE = INCOME_STATEMENT_POSITION(
     +             ns_service_decs%SERVICE_EXPENSE_CLASSIFICATION(UNIT))
                     IF(INDEX('BTL,X,N',
     +      ns_service_decs%SERVICE_EXPENSE_COLLECTION(UNIT)) /= 0) THEN
                        IF(EXP_TYPE == 22) THEN
                           ST_BTL_LEASE_PAYMENT(MO,ASSET_CLASS_loc) =
     +                          ST_BTL_LEASE_PAYMENT(MO,ASSET_CLASS_loc)
     +                              +  ST_EXPENSE
                        ! Nuc Fuel Expense
                        ELSEIF(EXP_TYPE == 17 .OR. EXP_TYPE == 18) THEN
                           ST_NF_RATEBASE(MO,ASSET_CLASS_loc) =
     +                                ST_NF_RATEBASE(MO,ASSET_CLASS_loc)
     +                                    + TRANSACTION_AMOUNT
                        ENDIF
                        EXP_TYPE = 28
                     ENDIF

                     SELECT CASE(
     +           trim(ns_service_decs%SERVICE_EXPENSE_COLLECTION(UNIT)))
                        CASE ('BTL','X','N')
                           IF(EXP_TYPE == 22) THEN
                            ST_BTL_LEASE_PAYMENT(MO,ASSET_CLASS_loc) =
     +                          ST_BTL_LEASE_PAYMENT(MO,ASSET_CLASS_loc)
     +                              + ST_EXPENSE
                           ENDIF
                           EXP_TYPE = 28
                        CASE ('Adj','A')
                           IF(INCLUDE_FIXED_COSTS_ADJ_CLAUSE) THEN
                           ST_ANN_CLASS_ADJ_CLAUSE(MO,ASSET_CLASS_loc) =
     +                      ST_ANN_CLASS_ADJ_CLAUSE(MO,ASSET_CLASS_loc)
     +                           + TRANSACTION_AMOUNT
                           ELSE
                          ST_ANN_CLASS_ADJ_CLAUSE(MO,ASSET_CLASS_loc) =
     +                       ST_ANN_CLASS_ADJ_CLAUSE(MO,ASSET_CLASS_loc)
     +                           + ASSET_ALLOCATOR *
     +                                     MONTHLY_ST_VAR_COST(MO,UNIT)
                           ENDIF
                     END SELECT
                     ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS_loc,EXP_TYPE) =
     +                ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS_loc,EXP_TYPE)
     +                     + ST_EXPENSE
                  ENDIF
               ENDDO

               CLASS_POINTER = CLASS_POINTER + 1
               IF(ASSET_CLASS_LIST(CLASS_POINTER) == 0. .OR.
     +                           CLASS_POINTER > AVAIL_DATA_YEARS) EXIT
            ENDDO ! ASSET CLASSES

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

C SUM TO ANNUAL TOTALS
C
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

         ST_ANN_CLASS_REVENUE(0,:,:) =
     +                          SUM(ST_ANN_CLASS_REVENUE(1:,:,:),DIM=1)
         ST_ANN_CLASS_EXPENSE(0,:,11:) =
     +                        SUM(ST_ANN_CLASS_EXPENSE(1:,:,11:),DIM=1)

C SCALE

         ST_ANN_CLASS_BTL_EXPENSE = ST_ANN_CLASS_BTL_EXPENSE/1000.
         ST_ANN_CLASS_ATL_EXPENSE = ST_ANN_CLASS_ATL_EXPENSE/1000.
         ST_ANN_CLASS_ADJ_CLAUSE = ST_ANN_CLASS_ADJ_CLAUSE/1000.
         ST_BTL_LEASE_PAYMENT = ST_BTL_LEASE_PAYMENT/1000.
         ST_NF_RATEBASE = ST_NF_RATEBASE/1000.
         ST_ANN_CLASS_REVENUE = ST_ANN_CLASS_REVENUE/1000.
         ST_ANN_CLASS_EXPENSE = ST_ANN_CLASS_EXPENSE/1000.

      RETURN ! CALC_ST_ANN_ASSET_CLASS


      ENTRY MONTHLY_SERVICE_TRANS_EXPENSES(R_CLASS,
     +                                     MONTH_VARS)



         IF(SERVICE_TRANS <= 0) RETURN

         IF(R_CLASS <= MAX_SERVICE_CLASS_ID_NUM) THEN
            ASSET_CLASS_loc = R_CLASS
            IF(ASSET_CLASS_loc > 0 .OR. R_CLASS == -1) THEN
               DO MO = 0, 12

C EXPENSES

        MONTH_VARS(MO,Fossil Fuel) =
     +          MONTH_VARS(MO,Fossil Fuel) +
     +             ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS_loc,11)
        MONTH_VARS(MO,Purchased Power) =
     +          MONTH_VARS(MO,Purchased Power) +
     +                ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS_loc,12)
        MONTH_VARS(MO,Variable OandM) =
     +           MONTH_VARS(MO,Variable OandM) +
     +                ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS_loc,13)
           MONTH_VARS(MO,Variable OandM) =
     +         MONTH_VARS(MO,Fixed OandM) +
     +                ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS_loc,14)

          MONTH_VARS(MO,Variable OandM) =
     +          MONTH_VARS(MO,Other OandM) +
     +                ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS_loc,15)

            MONTH_VARS(MO,Purchased Gas) =
     +          MONTH_VARS(MO,Purchased Gas) +
     +                ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS_loc,16)

              MONTH_VARS(MO,Other) =
     +          MONTH_VARS(MO,Other) +
     +                           ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS,17)
C
                  MONTH_VARS(MO,Owned Nuclear Fuel) =
     +          MONTH_VARS(MO,Owned Nuclear Fuel) +
     +                ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS_loc,18)
            MONTH_VARS(MO,Leased Nuclear Fuel) =
     +          MONTH_VARS(MO,Leased Nuclear Fuel) +
     +                           ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS,19)
C
                  MONTH_VARS(MO,DSM Expense) =
     +          MONTH_VARS(MO,DSM Expense) +
     +                           ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS,20)
C
                  MONTH_VARS(MO,DSM Rebate) =
     +          MONTH_VARS(MO,DSM Rebate) +
     +                           ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS,21)
C
                  MONTH_VARS(MO,ATL Book Lease Expense) =
     +          MONTH_VARS(MO,ATL Book Lease Expense) +
     +                           ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS,22)
C
                  MONTH_VARS(MO,Service Transactions) =
     +          MONTH_VARS(MO,Service Transactions) +
     +                           ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS,23)
C
                  MONTH_VARS(MO,Emission Credits) =
     +          MONTH_VARS(MO,Emission Credits) +
     +                ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS_loc,24)

                  MONTH_VARS(MO,DOE Decommissioning) =
     +                    MONTH_VARS(MO,DOE Decommissioning) +
     + ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS_loc,25)

                  MONTH_VARS(MO,DOE Disposal) =
     +                    MONTH_VARS(MO,DOE Disposal) +
     +                     ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS_loc,26)

                  MONTH_VARS(MO,Catawba Expenses) =
     +                    MONTH_VARS(MO,Catawba Expenses) +
     +                   ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS_loc,27)

                  MONTH_VARS(MO,BTL Expenses) =
     +                   MONTH_VARS(MO,BTL Expenses) +
     +                    ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS_loc,28)

                  MONTH_VARS(MO,BTL Lease Cash) =
     +                    MONTH_VARS(MO,BTL Lease Cash) +
     +                          ST_BTL_LEASE_PAYMENT(MO,ASSET_CLASS_loc)

                  MONTH_VARS(MO,Exp Collection in Adj Clause) =
     +                    MONTH_VARS(MO,Exp Collection in Adj Clause) +
     +                      ST_ANN_CLASS_ADJ_CLAUSE(MO,ASSET_CLASS_loc)

               ENDDO
            ENDIF
         ENDIF
      RETURN

      ENTRY MONTHLY_SERVICE_TRANS_REVENUES(R_CLASS,
     +                                     MONTH_VARS)


         IF(SERVICE_TRANS <= 0) RETURN

         IF(R_CLASS <= MAX_SERVICE_CLASS_ID_NUM) THEN
            ASSET_CLASS_loc = R_CLASS
            IF(ASSET_CLASS_loc > 0 .OR. R_CLASS == -1) THEN
c               DO MO = 1, 12

C REVENUES

                  MONTH_VARS(:,Base Rates) =
     +                    MONTH_VARS(:,Base Rates) +
     +                         ST_ANN_CLASS_REVENUE(:,ASSET_CLASS_loc,1)
                  MONTH_VARS(:,adjustment_clause) =
     +                    MONTH_VARS(:,adjustment_clause) +
     +                         ST_ANN_CLASS_REVENUE(:,ASSET_CLASS_loc,2)
                  MONTH_VARS(:,secondary_sales) =
     +                    MONTH_VARS(:,secondary_sales) +
     +                         ST_ANN_CLASS_REVENUE(:,ASSET_CLASS_loc,3)
                  MONTH_VARS(:,Other Revenue) =
     +                    MONTH_VARS(:,Other Revenue) +
     +                         ST_ANN_CLASS_REVENUE(:,ASSET_CLASS_loc,4)
                  MONTH_VARS(:,BTL Revenues) =
     +                    MONTH_VARS(:,BTL Revenues) +
     +                         ST_ANN_CLASS_REVENUE(:,ASSET_CLASS_loc,5)
                  MONTH_VARS(:,Catawba Revenues) =
     +                 MONTH_VARS(:,Catawba Revenues) +
     +                         ST_ANN_CLASS_REVENUE(:,ASSET_CLASS_loc,6)
                  MONTH_VARS(:,Gas Revenues) =
     +                    MONTH_VARS(:,Gas Revenues) +
     +                         ST_ANN_CLASS_REVENUE(:,ASSET_CLASS_loc,7)
                  MONTH_VARS(:,Unbilled Revenues) =
     +                    MONTH_VARS(:,Unbilled Revenues) +
     +                         ST_ANN_CLASS_REVENUE(:,ASSET_CLASS_loc,8)
                  MONTH_VARS(:,Deferred Revenues) =
     +                    MONTH_VARS(:,Deferred Revenues) +
     +                         ST_ANN_CLASS_REVENUE(:,ASSET_CLASS_loc,9)
                  MONTH_VARS(:,Relationship Revenues) =
     +                    MONTH_VARS(:,Relationship Revenues) +
     +                        ST_ANN_CLASS_REVENUE(:,ASSET_CLASS_loc,10)
                  MONTH_VARS(:,Residential) =
     +            MONTH_VARS(:,Residential) +
     +                   ST_ANN_CLASS_REVENUE(:,ASSET_CLASS_loc,11)
          MONTH_VARS(:,Commercial) =
     +            MONTH_VARS(:,Commercial) +
     +                   ST_ANN_CLASS_REVENUE(:,ASSET_CLASS_loc,12)
          MONTH_VARS(:,Industrial) =
     +            MONTH_VARS(:,Industrial) +
     +                   ST_ANN_CLASS_REVENUE(:,ASSET_CLASS_loc,13)
          MONTH_VARS(:,Lighting) =
     +            MONTH_VARS(:,Lighting) +
     +                   ST_ANN_CLASS_REVENUE(:,ASSET_CLASS_loc,14)
          MONTH_VARS(:,Bulk Power) =
     +            MONTH_VARS(:,Bulk Power) +
     +                   ST_ANN_CLASS_REVENUE(:,ASSET_CLASS_loc,15)
          MONTH_VARS(:,Net of Tax BTL Revenues) =
     +            MONTH_VARS(:,Net of Tax BTL Revenues) +
     +                   ST_ANN_CLASS_REVENUE(:,ASSET_CLASS_loc,16)
          MONTH_VARS(:,Capacity Sales) =
     +            MONTH_VARS(:,Capacity Sales) +
     +                   ST_ANN_CLASS_REVENUE(:,ASSET_CLASS_loc,17)
          MONTH_VARS(:,Government) =
     +            MONTH_VARS(:,Government) +
     +                   ST_ANN_CLASS_REVENUE(:,ASSET_CLASS_loc,18)

            ENDIF
         ENDIF
      RETURN

      ENTRY MONTHLY_SERVICE_TRANS_CASH(R_CLASS,
     +                                 MONTH_VARS)


         IF(SERVICE_TRANS <= 0) RETURN

         CALL MONTHLY_SERVICE_TRANS_REVENUES(R_CLASS,MONTH_VARS)

         IF(R_CLASS <= MAX_SERVICE_CLASS_ID_NUM) THEN
            ASSET_CLASS_loc = R_CLASS
            IF(ASSET_CLASS_loc > 0 .OR. R_CLASS == -1) THEN
c               DO MO = 1, 12

C EXPENSES

                  MONTH_VARS(:,Cash Fossil Fuel) =
     +         MONTH_VARS(:,Cash Fossil Fuel) +
     +                ST_ANN_CLASS_EXPENSE(:,ASSET_CLASS_loc,11)
       MONTH_VARS(:,Cash Purchased Power) =
     +         MONTH_VARS(:,Cash Purchased Power) +
     +                ST_ANN_CLASS_EXPENSE(:,ASSET_CLASS_loc,12)
       MONTH_VARS(:,Cash Variable OandM) =
     +          MONTH_VARS(:,Cash Variable OandM) +
     +                ST_ANN_CLASS_EXPENSE(:,ASSET_CLASS_loc,13)
       MONTH_VARS(:,Cash Fixed OandM) =
     +        MONTH_VARS(:,Cash Fixed OandM) +
     +                ST_ANN_CLASS_EXPENSE(:,ASSET_CLASS_loc,14)
       MONTH_VARS(:,Cash Other OandM) =
     +         MONTH_VARS(:,Cash Other OandM) +
     +                ST_ANN_CLASS_EXPENSE(:,ASSET_CLASS_loc,15)
       MONTH_VARS(:,Cash Purchased Gas) =
     +         MONTH_VARS(:,Cash Purchased Gas) +
     +                ST_ANN_CLASS_EXPENSE(:,ASSET_CLASS_loc,16)
       MONTH_VARS(:,Cash Other) =
     +         MONTH_VARS(:,Cash Other) +
     +                ST_ANN_CLASS_EXPENSE(:,ASSET_CLASS_loc,17)
       MONTH_VARS(:,Cash Leased Nuclear Fuel) =
     +         MONTH_VARS(:,Cash Leased Nuclear Fuel) +
     +                ST_ANN_CLASS_EXPENSE(:,ASSET_CLASS_loc,19)
       MONTH_VARS(:,Cash DSM Expense) =
     +         MONTH_VARS(:,Cash DSM Expense) +
     +                ST_ANN_CLASS_EXPENSE(:,ASSET_CLASS_loc,20)
       MONTH_VARS(:,Cash DSM Rebate) =
     +         MONTH_VARS(:,Cash DSM Rebate) +
     +                ST_ANN_CLASS_EXPENSE(:,ASSET_CLASS_loc,21)
       MONTH_VARS(:,Cash Lease Expense) =
     +         MONTH_VARS(:,Cash Lease Expense) +
     +                ST_ANN_CLASS_EXPENSE(:,ASSET_CLASS_loc,22)

        MONTH_VARS(:,Cash Service Transactions) =
     +          MONTH_VARS(:,Cash Service Transactions) +
     +                 ST_ANN_CLASS_EXPENSE(:,ASSET_CLASS_loc,23)
        MONTH_VARS(:,Cash Emission Credits) =
     +          MONTH_VARS(:,Cash Emission Credits) +
     +                 ST_ANN_CLASS_EXPENSE(:,ASSET_CLASS_loc,24)
        MONTH_VARS(:,Cash DOE Decommissioning) =
     +          MONTH_VARS(:,Cash DOE Decommissioning) +
     +                 ST_ANN_CLASS_EXPENSE(:,ASSET_CLASS_loc,25)
        MONTH_VARS(:,Cash DOE Disposal) =
     +          MONTH_VARS(:,Cash DOE Disposal) +
     +                 ST_ANN_CLASS_EXPENSE(:,ASSET_CLASS_loc,26)
        MONTH_VARS(:,cash_catawba_expenses) =
     +          MONTH_VARS(:,cash_catawba_expenses) +
     +                 ST_ANN_CLASS_EXPENSE(:,ASSET_CLASS_loc,27)
        MONTH_VARS(:,Cash BTL Expenses) =
     +         MONTH_VARS(:,Cash BTL Expenses) +
     +                           ST_ANN_CLASS_EXPENSE(:,ASSET_CLASS,28)
C
                  MONTH_VARS(:,Cash BTL Lease Cash) =
     +          MONTH_VARS(:,Cash BTL Lease Cash) +
     +                         ST_BTL_LEASE_PAYMENT(:,ASSET_CLASS_loc)

            ENDIF
         ENDIF
      RETURN

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


         IF(SERVICE_TRANS <= 0) RETURN

         IF(R_CLASS <= MAX_SERVICE_CLASS_ID_NUM) THEN
            ASSET_CLASS_loc = R_CLASS
            IF(ASSET_CLASS_loc > 0 .OR. R_CLASS == -1) THEN
               MO = 0

C REVENUES

        R_BASE_RATES_REVENUES = R_BASE_RATES_REVENUES +
     +                    ST_ANN_CLASS_REVENUE(MO,ASSET_CLASS_loc,1)
        R_ADJUSTMENT_CLAUSE_REVENUES =
     +                            R_ADJUSTMENT_CLAUSE_REVENUES +
     +                    ST_ANN_CLASS_REVENUE(MO,ASSET_CLASS_loc,2)
        R_SECONDARY_SALES_REVENUES = R_SECONDARY_SALES_REVENUES +
     +                    ST_ANN_CLASS_REVENUE(MO,ASSET_CLASS_loc,3)
        R_OTHER_REVENUES = R_OTHER_REVENUES +
     +                    ST_ANN_CLASS_REVENUE(MO,ASSET_CLASS_loc,4)
        R_BTL_REVENUES = R_BTL_REVENUES +
     +                    ST_ANN_CLASS_REVENUE(MO,ASSET_CLASS_loc,5)
        R_CATAWBA_REVENUES = R_CATAWBA_REVENUES +
     +                    ST_ANN_CLASS_REVENUE(MO,ASSET_CLASS_loc,6)
        R_CAPACITY_REVENUES = R_CAPACITY_REVENUES +
     +                   ST_ANN_CLASS_REVENUE(MO,ASSET_CLASS_loc,17)

C EXPENSES

               R_FUEXP = R_FUEXP +
     +                  ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS_loc,11)
       R_PREXP = R_PREXP +
     +                  ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS_loc,12)
       R_OPEXP = R_OPEXP +
     +                  ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS_loc,13)
       R_MNEXP = R_MNEXP +
     +                  ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS_loc,14)
       R_OTHER1 = R_OTHER1 +
     +                  ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS_loc,15)
       R_OTHER2 = R_OTHER2 +
     +                  ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS_loc,16)
       R_OTHER3 = R_OTHER3 +
     +                  ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS_loc,17)
       R_NFOWN = R_NFOWN +
     +                  ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS_loc,18)
       R_NFLEASE = R_NFLEASE +
     +                  ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS_loc,19)
       R_DSM_EXPENSE = R_DSM_EXPENSE +
     +                  ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS_loc,20)
       R_DSM_REBATE =  R_DSM_REBATE +
     +                  ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS_loc,21)
       R_ATL_LEASE_EXP = R_ATL_LEASE_EXP +
     +                  ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS_loc,22)
       R_SERVICE_TRANSACTIONS = R_SERVICE_TRANSACTIONS +
     +                  ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS_loc,23)
       R_EMISSION_CREDITS = R_EMISSION_CREDITS +
     +                  ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS_loc,24)
       R_DOE_DECOMMISSIONING = R_DOE_DECOMMISSIONING +
     +                  ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS_loc,25)
       R_DOE_DISPOSAL =  R_DOE_DISPOSAL +
     +                  ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS_loc,26)
       R_CATAWBA_EXPENSES = R_CATAWBA_EXPENSES +
     +                  ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS_loc,27)
       R_BTL_EXPENSE = R_BTL_EXPENSE +
     +                  ST_ANN_CLASS_EXPENSE(MO,ASSET_CLASS_loc,28)

C SPECIAL EXPENSE ITEMS

               R_ADJ_EXP = R_ADJ_EXP +
     +                   ST_ANN_CLASS_ADJ_CLAUSE(MO,ASSET_CLASS_loc)
        R_BTL_LEASE_EXP = R_BTL_LEASE_EXP +
     +                      ST_BTL_LEASE_PAYMENT(MO,ASSET_CLASS_loc)
        R_NF_RATEBASE = R_NF_RATEBASE +
     +                            ST_NF_RATEBASE(MO,ASSET_CLASS_loc)
            ENDIF
         ENDIF
      RETURN

      ENTRY SET_UP_ST_CLASS_ARRAYS


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
C
         IF(ALLOCATED(ASSET_CLASS_LIST))
     +               DEALLOCATE(ASSET_CLASS_LIST,ASSET_ALLOCATION_LIST)
         ALLOCATE(ASSET_CLASS_LIST(AVAIL_DATA_YEARS),
     +                         ASSET_ALLOCATION_LIST(AVAIL_DATA_YEARS))
      RETURN
      END subroutine READ_SERVICE_TRANS
C**********************************************************************

C                   SERVICE TRANSACTIONS ALLOCATION MODULE
C                              COPYRIGHT (C) 1992
C                        M.S. GERBER & ASSOCIATES, INC.
C                              ALL RIGHTS RESERVED

C**********************************************************************

      SUBROUTINE ALLOCATE_SERVICE_COSTS(TYPE_OF_SERVICE,SERVICE_CHARGE,
     + SERVICE_COST_ASSIGNMENT,
     +                                  SERVICE_EXPENSE_cln_loc,
     + SERVICE_REPORTING_GROUP)

      use servcom
      CHARACTER*1 TYPE_OF_SERVICE,
     +            SERVICE_COST_ASSIGNMENT
      CHARACTER*3 SERVICE_EXPENSE_cln_loc
      INTEGER*2 SERVICE_REPORTING_GROUP,SERVICE_ITEM
      REAL*4 SERVICE_CHARGE

      IF(SERVICE_COST_ASSIGNMENT == 'R') THEN
         SERVICE_REVENUES = SERVICE_REVENUES + SERVICE_CHARGE
         IF(INDEX(SERVICE_EXPENSE_cln_loc,'Bas') /= 0) THEN
            SERVICE_BASE_REVENUE_OFFSET = SERVICE_BASE_REVENUE_OFFSET +
     +                                                   SERVICE_CHARGE
         ENDIF
         IF(INDEX(SERVICE_EXPENSE_cln_loc,'Adj') /= 0) THEN
            SERVICE_ADJ_CLAUSE_OFFSET = SERVICE_ADJ_CLAUSE_OFFSET +
     +                                                   SERVICE_CHARGE
         ENDIF
      ELSE

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

         SERVICE_EXPENSES = SERVICE_EXPENSES + SERVICE_CHARGE
         IF(INDEX(SERVICE_EXPENSE_cln_loc,'Adj') /= 0) THEN
           SERVICE_ADJ_CLAUSE_EXPENSES = SERVICE_ADJ_CLAUSE_EXPENSES +
     +                                                   SERVICE_CHARGE
         ENDIF
         IF(INDEX(SERVICE_EXPENSE_cln_loc,'Bas') /= 0) THEN
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
      END SUBROUTINE ALLOCATE_SERVICE_COSTS
C*************************************************************
