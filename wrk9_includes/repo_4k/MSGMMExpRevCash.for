C Last change: MSG 1/9/2012 5:14:11 PM
C***********************************************************************
C*                                                                     *
C*                           E X P E N S E S                           *
C*                                                                     *
C*          COPYRIGHT (C) 1982,1994 M.S. GERBER & ASSOCIATES, INC      *
C*                         ALL RIGHTS RESERVED                         *
C*                                                                     *
C***********************************************************************
      SUBROUTINE EX_OBJECT
      use end_routine, only: end_program, er_message
      use grx_planning_routines
      use SpinDriftLib
      use prod_arrays_dimensions
      USE PROD_ARRAYS_DIMENSIONS
      use sizecom
      INTEGER*2 IREC,INUNIT,LRECL/2048/
      INTEGER*4 IOS,IOS_BASE
      INTEGER*2 NUMBER_OF_BC_EXPENSE_CLASSES/0/,
     +          MAX_BC_EXPENSE_CLASS_ID_NUM/0/
      INTEGER*2 NUMBER_OF_OL_EXPENSE_CLASSES/0/,
     +          MAX_OL_EXPENSE_CLASS_ID_NUM/0/
      INTEGER*2 UNIT_NUM/10/,
     +          ASSET_CLASS_NUM,ASSET_CLASS_VECTOR
      INTEGER*2 R_NUM_OF_EXPENSE_CLASSES,R_MAX_EXPENSE_CLASS_NUM
c      INTEGER*2 R_EXPENSE_CLASS_POINTERS(*)
      INTEGER*2 I
      INTEGER R_UNIT_NUM
      CHARACTER*1 DATA_TYPE,
     +            TAX_APPLICATION,
     +            ACCOUNT_ACTIVE_STATUS
      CHARACTER*3 EXPENSE_cln_local
      CHARACTER*30 EXPENSE_CLASSIFICATION,
     +             REVENUE_CLASSIFICATION,
     +             TAX_CLASSIFICATION
      CHARACTER*20 COMMODITY_DISTRIBUTION_PTR
      CHARACTER*12 TAX_STATUS,TAX_STATUS_STR
      CHARACTER*1 ACCOUNT_TYPE*12,INTRA_COMPANY_TRANSACTION
      INTEGER*2 INTRA_ASSET_CLASS_ID,INTRA_ASSET_CLASS_ALLOCATION
      CHARACTER*30 INTRA_ACCOUNT_CLASSIFICATION
      CHARACTER*3 INTRA_EXPENSE_COLLECTION
      CHARACTER*5 BASE_FILE_NAME,OVERLAY_FAMILY_NAME,EXPENFIL,DUMMY_STR
      CHARACTER*30 COMMENT,DESC,TEMP_DESC,
     +             CASH_LAG_PATTERN,
     +             CASH_ACCRUAL_AMOUNT,
     +             ACCT_SUBCLASS,
     +             INTRA_ACCT_SUBCLASS
      CHARACTER*256 FILE_NAME
      CHARACTER*256 BASE_FILE_DIRECTORY,OUTPUT_DIRECTORY
      CHARACTER*256 DATA_DRIVE
      LOGICAL*4 FILE_EXISTS
C DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER*2048 RECLN
C DECLARATION FOR /EXPENSE FILE/
      INTEGER*2 DELETE,ESCALATION_VECTOR
      INTEGER ACCTNO
      REAL*4 EXPEN(AVAIL_DATA_YEARS+1),
     +       CASH_VALUES_VECTOR
      CHARACTER*16 FILE_TYPE/'Expense Accounts'/
      INTEGER*2 EXPENSE_BC_ASSET_CLASS_POINTER(:),
     +          EXPENSE_OL_ASSET_CLASS_POINTER(:)
C     +          TEMP_ASSET_CLASS_POINTER(:)
      ALLOCATABLE :: EXPENSE_BC_ASSET_CLASS_POINTER,
     +               EXPENSE_OL_ASSET_CLASS_POINTER
C     +               TEMP_ASSET_CLASS_POINTER
      INTEGER*2 BOOK_VALUES_VECTOR
      CHARACTER*1 CASH_TYPE
      REAL*4 MIDAS_MONTHLY_BOOKED(60)
      CHARACTER*1 MIDAS_MONTHLY_DATA_UNITS(5)
      CHARACTER*4 MIDAS_LAST_MONTH(5)
      REAL*4 ALLOCATION_ADJUSTMENT ! 55  PLACE HOLDERS
      REAL MIDAS_MONTHLY_BUDGET_VALUES(60), !60 VALUES
     +     MIDAS_ANNUAL_BUDGET_VALES(5),   ! 5 VALUES LAST ONE 194
     +     CASH_VALUES_MONTHLY(24) ! 198-221
      CHARACTER*10 CASH_VALUES_TYPE(2)     ! 222-223
      INTEGER WVPA_ACCOUNTING_UNIT,  ! 195
     +        WVPA_SUB_ACCOUNT_NUMBER  ! 196
      CHARACTER*20 WVPA_TRACKING_TYPE
C
C MULTI-EXPENSE FILES VARIABLES
C
      INTEGER FILE_NUMBER,FILE_ID
      LOGICAL R_FILE_OPENED
      INTEGER MAX_EX_FILES,R_MAX_EX_FILES
      PARAMETER (MAX_EX_FILES=6)
      CHARACTER*2 EXPENOL(0:MAX_EX_FILES-1)/MAX_EX_FILES*'BC'/
      LOGICAL*1 R_EXPENSE_OL_FILES_ACTIVE
      CHARACTER*5 EX_FILE_BASE_NAMES(0:MAX_EX_FILES-1),
     +            VOID_CHR
      CHARACTER*2 EX_FILE_CODES(0:MAX_EX_FILES-1)/
     +                                   'EX','X1','X2','X3','X4','X5'/,
c     +                                'EX','EX','EX','EX','EX','EX'/,
     +            FILE_CODE
      CHARACTER*6 EX_FILE_BINARY_NAMES(0:MAX_EX_FILES-1)/'EXPEN0',
     +                                                   'EXPEN1',
     +                                                   'EXPEN2',
     +                                                   'EXPEN3',
     +                                                   'EXPEN4',
     +                                                   'EXPEN5'/,
     +            BINARY_FILE_NAME
      LOGICAL ACTIVE_BASE_EX_FILES(0:MAX_EX_FILES-1)/
     +                                            MAX_EX_FILES*.FALSE./,
     +        ACTIVE_OVERLAY_EX_FILES(0:MAX_EX_FILES-1)/
     +                                            MAX_EX_FILES*.FALSE./,
     +        EX_FILES_ARE_ACTIVE/.FALSE./,
     +        EXPENSE_OVERLAYS_ACTIVE/.FALSE./
      LOGICAL*1 LAHEY_LF95
      CHARACTER*30 SCREEN_OUTPUT
      SAVE EXPENSE_BC_ASSET_CLASS_POINTER,
     +     EXPENSE_OL_ASSET_CLASS_POINTER
      INTEGER (KIND=2) :: WVPA_COMPANY_ID      ! 224
C
C***********************************************************************
C
C          ROUTINE TO CONVERT METAFILE FILES TO DIRECT-ACESS BINARY
C          COPYRIGHT (C) 1983, 84, 85  M.S. GERBER & ASSOCIATES, INC.
C
C***********************************************************************
C
C CONVERT THE EXPENSE FILE
C***********************************************************************
      ENTRY EX_MAKEBIN

      VOID_CHR = EXPENFIL(EX_FILE_BASE_NAMES)

      IF(.NOT. LAHEY_LF95())
     +        CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
      DATA_DRIVE = OUTPUT_DIRECTORY()
      DO FILE_ID = 0, MAX_EX_FILES-1
         BASE_FILE_NAME = EX_FILE_BASE_NAMES(FILE_ID)
         ACTIVE_BASE_EX_FILES(FILE_ID) = .FALSE.
         IF(INDEX(BASE_FILE_NAME,'NONE') /= 0) CYCLE
         FILE_CODE = EX_FILE_CODES(FILE_ID)
         FILE_NAME = TRIM(BASE_FILE_DIRECTORY())//FILE_CODE//
     +                               "B"//TRIM(BASE_FILE_NAME)//".DAT"
         INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
         IF(.NOT. FILE_EXISTS) THEN
            FILE_NAME = TRIM(BASE_FILE_DIRECTORY())//
     +                             "EXB"//TRIM(BASE_FILE_NAME)//".DAT"
            INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
         ENDIF
         IF(FILE_EXISTS) THEN
            IF(LAHEY_LF95() .AND. FILE_EXISTS) THEN
               SCREEN_OUTPUT = TRIM(FILE_TYPE)//'-'//BASE_FILE_NAME
               CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
            ELSE
               CALL MG_LOCATE_WRITE(16,30,BASE_FILE_NAME,ALL_VERSIONS,0)
            ENDIF
            ACTIVE_BASE_EX_FILES(FILE_ID) = .TRUE.
c            ALLOCATE(TEMP_ASSET_CLASS_POINTER(1024))
c            CALL CINITW(TEMP_ASSET_CLASS_POINTER,INT(1024),INT2(0))
            OPEN(10,FILE=FILE_NAME)
            BINARY_FILE_NAME = EX_FILE_BINARY_NAMES(FILE_ID)
            OPEN(11,FILE=TRIM(DATA_DRIVE)//
     +                  "BC"//TRIM(BINARY_FILE_NAME)//".BIN",
     +                      ACCESS="DIRECT",STATUS="REPLACE",RECL=LRECL)
            IREC = 0
            READ(10,*) DELETE
            DO
               DATA_TYPE = 'D'
               ESCALATION_VECTOR = 0
               ASSET_CLASS_NUM = 0
               ASSET_CLASS_VECTOR = 0
               ACCOUNT_TYPE = 'Not Active'
               ACCOUNT_ACTIVE_STATUS = 'A'
               REVENUE_CLASSIFICATION = "Base Rates"
               TAX_CLASSIFICATION = " "
               TAX_STATUS_STR = "XXX"
               INTRA_COMPANY_TRANSACTION = 'N'
               INTRA_ASSET_CLASS_ID = 0
               INTRA_ASSET_CLASS_ALLOCATION = 0
               INTRA_ACCOUNT_CLASSIFICATION = 'Other'
               INTRA_EXPENSE_COLLECTION = 'Bas'
               CASH_TYPE = 'B'
               BOOK_VALUES_VECTOR = 0
               CASH_VALUES_VECTOR = 0.
               CASH_ACCRUAL_AMOUNT = "0."
               TAX_APPLICATION = 'B'
               EXPENSE_cln_local = 'Bas'
               ALLOCATION_ADJUSTMENT = 100.
               CASH_LAG_PATTERN = '100'   ! 57
               ACCT_SUBCLASS = '0'   ! 58
               INTRA_ACCT_SUBCLASS = '0' ! 59
               COMMODITY_DISTRIBUTION_PTR = 'Not Active'
               MIDAS_MONTHLY_DATA_UNITS(:) = 'A'
               MIDAS_LAST_MONTH(:) = 'None'
               MIDAS_MONTHLY_BUDGET_VALUES = 0. !60 VALUES
               MIDAS_ANNUAL_BUDGET_VALES = 0   ! 5 VALUES LAST ONE 194
               WVPA_ACCOUNTING_UNIT = 0  ! 195
               WVPA_SUB_ACCOUNT_NUMBER = 0  ! 196
               WVPA_TRACKING_TYPE = 'Not Tracked' ! 197
               CASH_VALUES_MONTHLY = 0. ! 198-221
               CASH_VALUES_TYPE = 'Not Active' ! 222-223
               WVPA_COMPANY_ID = 1      ! 224
C
C
C
               DO
                  READ(10,1000,IOSTAT=IOS) RECLN
                  IF(IOS /=0) EXIT
                  IF(RECLN(1:1) == '7') EXIT
                  RECLN = TRIM(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
     +                                 //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
     +                                 //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
     +                                 //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
     +                                 //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
     +                                 //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
     +                                 //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
     +                                 //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
     +                                 //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
     +                                 //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
                  EXPEN =  -999999.
                  MIDAS_MONTHLY_BOOKED =  -999999.
c                  IF(FILE_ID == 0) THEN
                     READ(RECLN,*,ERR=200) DELETE,ACCTNO,
     +                     EXPENSE_CLASSIFICATION,
     +                     EXPENSE_cln_local,EXPEN,
     +                     DESC,COMMENT,DATA_TYPE,ESCALATION_VECTOR,
     +                     ASSET_CLASS_NUM,
     +                     ASSET_CLASS_VECTOR,
     +                     ACCOUNT_TYPE,
     +                     REVENUE_CLASSIFICATION,
     +                     INTRA_COMPANY_TRANSACTION,
     +                     INTRA_ASSET_CLASS_ID,
     +                     INTRA_ASSET_CLASS_ALLOCATION,
     +                     INTRA_ACCOUNT_CLASSIFICATION,
     +                     INTRA_EXPENSE_COLLECTION,
     +                     BOOK_VALUES_VECTOR,
     +                     CASH_TYPE,
     +                     CASH_VALUES_VECTOR,
     +                     CASH_ACCRUAL_AMOUNT,
     +                     TAX_CLASSIFICATION,
     +                     TAX_APPLICATION,
     +                     TAX_STATUS_STR, ! 54
     +                     ALLOCATION_ADJUSTMENT, ! 55
     +                     ACCOUNT_ACTIVE_STATUS, ! 56
     +                     CASH_LAG_PATTERN, ! 57
     +                     ACCT_SUBCLASS, ! 58
     +                     COMMODITY_DISTRIBUTION_PTR, ! 59
     +                     MIDAS_MONTHLY_BOOKED, ! 60 VALUES
     +                     MIDAS_MONTHLY_DATA_UNITS, ! 5 VALUES
     +                     MIDAS_LAST_MONTH,   ! 5 VALUES
     +                     MIDAS_MONTHLY_BUDGET_VALUES, !60 VALUES
     +            MIDAS_ANNUAL_BUDGET_VALES,   ! 5 VALUES LAST ONE 194
     +                     WVPA_ACCOUNTING_UNIT,  ! 195
     +                     WVPA_SUB_ACCOUNT_NUMBER,  ! 196
     +                     WVPA_TRACKING_TYPE,  ! 197
     +                     CASH_VALUES_MONTHLY, ! 198-221
     +                     CASH_VALUES_TYPE,     ! 222-223
     +                     WVPA_COMPANY_ID      ! 224

                  IF(EXPEN(1) == -999999.) EXPEN(1) = 0.
                  DO I = 2, AVAIL_DATA_YEARS+1
                     IF(EXPEN(I) == -999999.) EXPEN(I) = EXPEN(I-1)
                  ENDDO
                  IF(INDEX(TAX_STATUS_STR,'XXX') /= 0) THEN
                     IF(INDEX(ACCOUNT_TYPE,'Cash') /= 0) THEN
                        TAX_STATUS = 'Non-Taxable'
                     ELSE
                        TAX_STATUS = 'Taxable'
                     ENDIF
                  ELSE
                     TAX_STATUS = TAX_STATUS_STR
                  ENDIF
                  IREC = IREC + 1
                  WRITE(11,REC=IREC) DELETE,DESC,
     +  ACCTNO,EXPENSE_CLASSIFICATION,
     +  EXPENSE_cln_local,EXPEN,
     +  DATA_TYPE,ESCALATION_VECTOR,
     +  ASSET_CLASS_NUM,
     +  ASSET_CLASS_VECTOR,
     +  ACCOUNT_TYPE,
     +  REVENUE_CLASSIFICATION,
     +  INTRA_COMPANY_TRANSACTION,
     +  INTRA_ASSET_CLASS_ID,
     +  INTRA_ASSET_CLASS_ALLOCATION,
     +  INTRA_ACCOUNT_CLASSIFICATION,
     +  INTRA_EXPENSE_COLLECTION,
     +  BOOK_VALUES_VECTOR,
     +  CASH_TYPE,
     +  CASH_VALUES_VECTOR,
     +  CASH_ACCRUAL_AMOUNT,
     +  TAX_CLASSIFICATION,
     +  TAX_APPLICATION,
     +  TAX_STATUS, ! 54
     +  ALLOCATION_ADJUSTMENT, ! 55  PLACE HOLDERS
     +  ACCOUNT_ACTIVE_STATUS, ! 56
     +  CASH_LAG_PATTERN, ! 57
     +  ACCT_SUBCLASS, ! 58
     +  COMMODITY_DISTRIBUTION_PTR, ! 59
     +  MIDAS_MONTHLY_BOOKED, ! 60 VALUES
     +  MIDAS_MONTHLY_DATA_UNITS, ! 5 VALUES
     +  MIDAS_LAST_MONTH,   ! 5 VALUES
     +  MIDAS_MONTHLY_BUDGET_VALUES, !60 VALUES
     +  MIDAS_ANNUAL_BUDGET_VALES,   ! 5 VALUES LAST ONE 194
     +  WVPA_ACCOUNTING_UNIT,  ! 195
     +  WVPA_SUB_ACCOUNT_NUMBER,  ! 196
     +  WVPA_TRACKING_TYPE,  ! 197
     +  CASH_VALUES_MONTHLY, ! 198-221
     +  CASH_VALUES_TYPE,     ! 222-223
     +  INTRA_ACCT_SUBCLASS ! after budget
               ENDDO
               IF(IOS /= 0) EXIT
            ENDDO
            CLOSE(10)

            CLOSE(11)

         ELSE IF(INDEX(BASE_FILE_NAME,'NONE') == 0) THEN
            CALL STOP_NOFILE(FILE_TYPE,FILE_NAME)
         ENDIF
      ENDDO ! EXPENSE FILE LOOP
      RETURN

C***********************************************************************
C
C          ROUTINE TO CREATE OVERLAY FILES
C          COPYRIGHT (C) 1984-88  M.S. GERBER & ASSOCIATES, INC.
C          COPYRIGHT (C) 1991-92  M.S. GERBER & ASSOCIATES, INC.
C
C***********************************************************************
C
C OVERLAY THE EXPENSE FILE
C***********************************************************************
      ENTRY EX_MAKEOVL(OVERLAY_FAMILY_NAME,FILE_NUMBER)
C***********************************************************************
      IF(.NOT. ACTIVE_BASE_EX_FILES(FILE_NUMBER)) RETURN
      FILE_CODE = EX_FILE_CODES(FILE_NUMBER)
      BINARY_FILE_NAME = EX_FILE_BINARY_NAMES(FILE_NUMBER)
      IF(LAHEY_LF95()) THEN
         SCREEN_OUTPUT = TRIM(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSE
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         CALL LOCATE(10,51)
      ENDIF
      DATA_DRIVE = OUTPUT_DIRECTORY()
      FILE_NAME=TRIM(DATA_DRIVE)//FILE_CODE//"O"//
     +                               TRIM(OVERLAY_FAMILY_NAME)//".DAT"
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      IF(.NOT. FILE_EXISTS) THEN
         FILE_NAME = TRIM(BASE_FILE_DIRECTORY())//
     +                        "EXO"//TRIM(OVERLAY_FAMILY_NAME)//".DAT"
      ENDIF
      OPEN(10,FILE=FILE_NAME,STATUS="OLD",IOSTAT=IOS)
      IF(IOS /= 0) THEN
         WRITE(4,*) 'Expense Overlay file ',TRIM(FILE_NAME),
     +              ' was not found.'
         RETURN
      ENDIF
      ACTIVE_OVERLAY_EX_FILES(FILE_NUMBER) = .TRUE.
      READ(10,*) DELETE
      INUNIT = 12
      IF(EXPENOL(FILE_NUMBER) == 'BC') THEN
         OPEN(11,FILE=TRIM(DATA_DRIVE)//"BC"//
     +                      TRIM(BINARY_FILE_NAME)//".BIN",
     +                      ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
         INUNIT = 11
      ENDIF
      IF(INUNIT == 11) THEN
         OPEN(12,FILE=TRIM(DATA_DRIVE)//"OL"//
     +                  TRIM(BINARY_FILE_NAME)//".BIN",
     +                      ACCESS="DIRECT",STATUS="REPLACE",RECL=LRECL)
      ELSE
         OPEN(12,FILE=TRIM(DATA_DRIVE)//"OL"//
     +                      TRIM(BINARY_FILE_NAME)//".BIN",
     +                          ACCESS="DIRECT",STATUS="OLD",RECL=LRECL)
      ENDIF
c      ALLOCATE(TEMP_ASSET_CLASS_POINTER(1024))
c      CALL CINITW(TEMP_ASSET_CLASS_POINTER,INT(1024),INT2(0))
      NUMBER_OF_OL_EXPENSE_CLASSES = 0
      MAX_OL_EXPENSE_CLASS_ID_NUM = 0
      IREC = 0
      DO
         DO
            READ(10,1000,IOSTAT=IOS) RECLN
            IF(RECLN(1:1) == '7') EXIT
            IREC = IREC + 1
            READ(INUNIT,REC=IREC,IOSTAT=IOS_BASE) DELETE,
     + DESC,ACCTNO,
     + EXPENSE_CLASSIFICATION,
     + EXPENSE_cln_local,EXPEN,
     + DATA_TYPE,ESCALATION_VECTOR,
     + ASSET_CLASS_NUM,
     + ASSET_CLASS_VECTOR,
     + ACCOUNT_TYPE,
     + REVENUE_CLASSIFICATION,
     + INTRA_COMPANY_TRANSACTION,
     + INTRA_ASSET_CLASS_ID,
     + INTRA_ASSET_CLASS_ALLOCATION,
     + INTRA_ACCOUNT_CLASSIFICATION,
     + INTRA_EXPENSE_COLLECTION,
     + BOOK_VALUES_VECTOR,
     + CASH_TYPE,
     + CASH_VALUES_VECTOR,
     + CASH_ACCRUAL_AMOUNT,
     + TAX_CLASSIFICATION,
     + TAX_APPLICATION,
     + TAX_STATUS, ! 54
     + ALLOCATION_ADJUSTMENT, ! 55  PLACE HOLDERS
     + ACCOUNT_ACTIVE_STATUS, ! 56
     + CASH_LAG_PATTERN, ! 57
     + ACCT_SUBCLASS, ! 58
     + COMMODITY_DISTRIBUTION_PTR, ! 59
     + MIDAS_MONTHLY_BOOKED, ! 60 VALUES
     + MIDAS_MONTHLY_DATA_UNITS, ! 5 VALUES
     + MIDAS_LAST_MONTH,   ! 5 VALUES
     + MIDAS_MONTHLY_BUDGET_VALUES, !60 VALUES
     + MIDAS_ANNUAL_BUDGET_VALES,   ! 5 VALUES LAST ONE 194
     + WVPA_ACCOUNTING_UNIT,  ! 195
     + WVPA_SUB_ACCOUNT_NUMBER,  ! 196
     + WVPA_TRACKING_TYPE,  ! 197
     + CASH_VALUES_MONTHLY, ! 198-221
     + CASH_VALUES_TYPE,     ! 222-223
     + INTRA_ACCT_SUBCLASS ! after budget
            IF(IOS_BASE /= 0) EXIT
            IF(IOS == 0) THEN
               RECLN = TRIM(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
     +                              //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
     +                              //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
     +                              //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
     +                              //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
     +                              //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               READ(RECLN,*,ERR=200) DELETE,ACCTNO,
     + EXPENSE_CLASSIFICATION,
     + EXPENSE_cln_local,EXPEN,
     + TEMP_DESC,COMMENT,
     + DATA_TYPE,ESCALATION_VECTOR,
     + ASSET_CLASS_NUM,
     + ASSET_CLASS_VECTOR,
     + ACCOUNT_TYPE,
     + REVENUE_CLASSIFICATION,
     + INTRA_COMPANY_TRANSACTION,
     + INTRA_ASSET_CLASS_ID,
     + INTRA_ASSET_CLASS_ALLOCATION,
     + INTRA_ACCOUNT_CLASSIFICATION,
     + INTRA_EXPENSE_COLLECTION,
     + BOOK_VALUES_VECTOR,
     + CASH_TYPE,
     + CASH_VALUES_VECTOR,
     + CASH_ACCRUAL_AMOUNT,
     + TAX_CLASSIFICATION,
     + TAX_APPLICATION,
     + TAX_STATUS, ! 54
     + ALLOCATION_ADJUSTMENT, ! 55  PLACE HOLDERS
     + ACCOUNT_ACTIVE_STATUS, ! 56
     + CASH_LAG_PATTERN, ! 57
     + ACCT_SUBCLASS, ! 58
     + COMMODITY_DISTRIBUTION_PTR, ! 59
     + MIDAS_MONTHLY_BOOKED, ! 60 VALUES
     + MIDAS_MONTHLY_DATA_UNITS, ! 5 VALUES
     + MIDAS_LAST_MONTH,   ! 5 VALUES
     + MIDAS_MONTHLY_BUDGET_VALUES, !60 VALUES
     + MIDAS_ANNUAL_BUDGET_VALES,   ! 5 VALUES LAST ONE 194
     + WVPA_ACCOUNTING_UNIT,  ! 195
     + WVPA_SUB_ACCOUNT_NUMBER,  ! 196
     + WVPA_TRACKING_TYPE,  ! 197
     + CASH_VALUES_MONTHLY, ! 198-221
     + CASH_VALUES_TYPE     ! 222-223

            ENDIF

            WRITE(12,REC=IREC) DELETE,DESC,
     + ACCTNO,EXPENSE_CLASSIFICATION,
     + EXPENSE_cln_local,EXPEN,
     + DATA_TYPE,ESCALATION_VECTOR,
     + ASSET_CLASS_NUM,
     + ASSET_CLASS_VECTOR,
     + ACCOUNT_TYPE,
     + REVENUE_CLASSIFICATION,
     + INTRA_COMPANY_TRANSACTION,
     + INTRA_ASSET_CLASS_ID,
     + INTRA_ASSET_CLASS_ALLOCATION,
     + INTRA_ACCOUNT_CLASSIFICATION,
     + INTRA_EXPENSE_COLLECTION,
     + BOOK_VALUES_VECTOR,
     + CASH_TYPE,
     + CASH_VALUES_VECTOR,
     + CASH_ACCRUAL_AMOUNT,
     + TAX_CLASSIFICATION,
     + TAX_APPLICATION,
     + TAX_STATUS, ! 54
     + ALLOCATION_ADJUSTMENT, ! 55  PLACE HOLDERS
     + ACCOUNT_ACTIVE_STATUS, ! 56
     + CASH_LAG_PATTERN, ! 57
     + ACCT_SUBCLASS, ! 58
     + COMMODITY_DISTRIBUTION_PTR, !59
     + MIDAS_MONTHLY_BOOKED, ! 60 VALUES
     + MIDAS_MONTHLY_DATA_UNITS, ! 5 VALUES
     + MIDAS_LAST_MONTH,   ! 5 VALUES
     + MIDAS_MONTHLY_BUDGET_VALUES, !60 VALUES
     + MIDAS_ANNUAL_BUDGET_VALES,   ! 5 VALUES LAST ONE 194
     + WVPA_ACCOUNTING_UNIT,  ! 195
     + WVPA_SUB_ACCOUNT_NUMBER,  ! 196
     + WVPA_TRACKING_TYPE,  ! 197
     + CASH_VALUES_MONTHLY, ! 198-221
     + CASH_VALUES_TYPE,     ! 222-223
     + INTRA_ACCT_SUBCLASS !after budget
         ENDDO
         IF(IOS_BASE /= 0) EXIT
      ENDDO
      CLOSE(10)
      CLOSE(12)
      IF(EXPENOL(FILE_NUMBER) == 'BC') CLOSE(11)
      EXPENOL(FILE_NUMBER) = 'OL'

      RETURN
C
  200 CALL MG_LOCATE_WRITE(20,0,TRIM(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from MSGMMExpRevCash SIID205'
      call end_program(er_message)
C
C***********************************************************************
      ENTRY RESET_EXPENOL
C***********************************************************************
         EXPENSE_OVERLAYS_ACTIVE = .FALSE.
         DO FILE_ID = 0, MAX_EX_FILES-1
            EXPENOL(FILE_ID) = 'BC'
            ACTIVE_OVERLAY_EX_FILES(FILE_ID) = .FALSE.
         ENDDO
      RETURN
C
C***********************************************************************
      ENTRY GET_EXPENSES_ACTIVE(R_EXPENSE_OL_FILES_ACTIVE)
C***********************************************************************
         R_EXPENSE_OL_FILES_ACTIVE = .FALSE.
         DO FILE_ID = 0, MAX_EX_FILES-1
            R_EXPENSE_OL_FILES_ACTIVE = R_EXPENSE_OL_FILES_ACTIVE .OR.
     +                                  ACTIVE_OVERLAY_EX_FILES(FILE_ID)
         ENDDO
      RETURN

      ENTRY OPEN_EXPENSE_FILE(R_UNIT_NUM,FILE_NUMBER,R_FILE_OPENED)
C***********************************************************************
         OPEN(R_UNIT_NUM,FILE=TRIM(OUTPUT_DIRECTORY())//
     +        EXPENOL(FILE_NUMBER)//
     +            TRIM(EX_FILE_BINARY_NAMES(FILE_NUMBER))//".BIN",
     +               ACCESS="DIRECT",STATUS="OLD",RECL=LRECL,IOSTAT=IOS)
         R_FILE_OPENED = IOS == 0
         UNIT_NUM = R_UNIT_NUM
      RETURN
C
C***********************************************************************
      ENTRY OPEN_EXPENSE_BASE_CASE_FILE(R_UNIT_NUM)
C***********************************************************************
         OPEN(R_UNIT_NUM,FILE=TRIM(OUTPUT_DIRECTORY())//
     +                                "BC_EXPEN.BIN",FORM='UNFORMATTED')
      RETURN
C***********************************************************************
      ENTRY CLOSE_EXPENSE_FILE
C***********************************************************************
         CLOSE(UNIT_NUM)
      RETURN

 1000 FORMAT(A)
 1010 FORMAT('&',A)
      END
C***********************************************************************
C*                                                                     *
C*                           E X P E N S E S                           *
C*                                                                     *
C*                    COPYRIGHT (C) 1982,1994, 1998                    *
C*                     M.S. GERBER & ASSOCIATES, INC                   *
C*                         ALL RIGHTS RESERVED                         *
C*                                                                     *
C***********************************************************************
C                                                                      *
C     PURPOSE:                                                         *
C        EXPENS AGGREGATES  THE EXPENSE ITEMS INTO THE FIVE            *
C        EXPENSE CATAGORIES:                                           *
C               1  OPERATION                                           *
C               2  MAINTENANCE                                         *
C               3  FOSSIL FUEL                                         *
C               4  PURCHASED POWER                                     *
C               5  PURCHASED GAS                                       *
C                                                                      *
C***********************************************************************
C
      RECURSIVE SUBROUTINE EXPENSE(SAVE_BASE_CASE)
C
      USE IREC_ENDPOINT_CONTROL
      USE DRILLING_REPT_PARAMETERS
      use enrg_helper
      use grx_planning_routines
      use spindriftlib
      use globecom
      use sizecom
	  use namescom
	  use exp_rev_cash
	  use enrglimit
	  
	  
	  ! Stores "entry-shared" variables from this routine.
	  use exp_rev_cash
      
      SAVE

      
      INCLUDE 'mthnmcom.mon'
      INCLUDE 'FINACSTW.MON'
C
      CHARACTER*1 DUMMY_TYPE
      LOGICAL*1 SAVE_BASE_CASE,R_ZERO_VALUES,WVPA
      INTEGER*2 I,TACCTS,DELETE,ESCAL_VECT,CLASS_POINTER,
     +          NUM_OF_ASSET_CLASSES,FINANCIAL_SIMULATION_YEARS,J
      INTEGER*4 IOS,IN_REC,IREC
      CHARACTER*1 DATA_TYPE
      INTEGER*2 :: MAX_ASSET_CLASS_NUM
      REAL*4 R_FUEXP,R_PREXP,R_OPEXP,R_MNEXP,R_OTHER1,R_OTHER2,
     +       R_OTHER3,R_NFOWN,R_NFLEASE,
     +       R_DSM_EXPENSE,R_DSM_REBATE,R_BTL_EXPENSE,
     +       R_ATL_LEASE_EXP,R_BTL_LEASE_EXP,R_SERVICE_TRANSACTIONS,
     +       R_EMISSION_CREDITS,
     +       R_DOE_DISPOSAL,R_DOE_DECOMMISSIONING,
     +       R_CATAWBA_EXPENSES
      INTEGER*2 R_YR,R_CLASS,R_PERIOD
      LOGICAL*1 R_CLASS_EXISTS
      CHARACTER*30 EXPENSE_CLASSIFICATION,
     +             REVENUE_CLASSIFICATION,
     +             SAVE_REVENUE_CLASSIFICATION,
     +             SAVE_EXPENSE_CLASSIFICATION,
     +             TAX_CLASSIFICATION,
     +             DESC,
     +             CASH_LAG_PATTERN, ! 57
     +             CASH_ACCRUAL_AMOUNT,
     +             ACCT_SUBCLASS, ! 58
     +             INTRA_ACCT_SUBCLASS ! after budget
      CHARACTER*12 TAX_STATUS
      CHARACTER*3 EXPENSE_cln_local,R_EXPENSE_COLLECTION
      CHARACTER*1 ACCOUNT_TYPE,ACCOUNT_TYPE_STR*12,
     +            INTRA_COMPANY_TRANSACTION,
     +            TAX_APPLICATION,SAVE_ACCOUNT_TYPE
      INTEGER*2 INTRA_ASSET_CLASS_ID,INTRA_ASSET_CLASS_ALLOCATION
      CHARACTER*30 INTRA_ACCOUNT_CLASSIFICATION
      CHARACTER*3 INTRA_EXPENSE_COLLECTION
      INTEGER*2 BOOK_VALUES_VECTOR
      CHARACTER*1 CASH_TYPE
      REAL*4 CASH_VALUES_VECTOR

      INTEGER*2 :: EXTYPE,ASSET_ALLOCATION_VECTOR,
     +          RUN_YEARS,EXTENSION_YEARS,
     +          INCOME_STATEMENT_POSITION,CASH_POSITION,
     +          CASH_STATEMENT_POSITION,
     +          ACCOUNT_SUB_CLASSIFICATION
      INTEGER ACCTNO
      REAL*4 ASSET_ALLOCATOR


      REAL*4 :: MONTH_VARS(0:12,1:*),
     +       INC_MONTH_VARS(0:12,1:*),
     +       CASH_MONTH_VARS(0:12,1:*),
     +       TEMP_CASH_VARS(0:12,CASH_VARS)
C
C REVENUE SECTION
C
      REAL*4 R_ADJUSTMENT_CLAUSE_REVENUES,
     +     R_BASE_RATES_REVENUES,
     +     R_SECONDARY_SALES_REVENUES,
     +     R_SECONDARY_SALES_NOT_IN_RATES,
     +     R_CAPACITY_REVENUES,
     +     R_OTHER_REVENUES,
     +     R_BTL_REVENUES,
     +     R_CATAWBA_REVENUES,
     +     R_GAS_REVENUES,
     +     R_TRANSMISSION_OPERATION,
     +     R_TRANSMISSION_MAINTENANCE,
     +     R_DISTRIBUTION_OPERATION,
     +     R_DISTRIBUTION_MAINTENANCE,
     +     R_CUSTOMER_ACCOUNTS,
     +     R_CUSTOMER_SERVICES,
     +     R_SALES_EXPENSE,
     +     R_AG_OPERATIONS,
     +     R_AG_MAINTENANCE,
     +     R_Exp_PreferredDividends,
     +     R_AFDCEquity,
     +     R_AFDCBorrowed
      REAL*4 R_UNBILLED_REVENUES,
     +       R_ATL_DEFERRED_REVENUES,
     +       R_DEFERRED_REVENUES,
     +       R_RELATIONSHIP_REVENUES,
     +       R_RESIDENTIAL_REVENUES,
     +       R_COMMERCIAL_REVENUES,
     +       R_INDUSTRIAL_REVENUES,
     +       R_LIGHTING_REVENUES,
     +       R_BULK_POWER_REVENUES,
     +       R_WVPA_MEMBER_ACCRUED_REVENUES,
     +       WVPA_NON_MEMBER_COST_OF_POWER,
     +       wvpa_member_cost_of_power_merc,
     +       WVPA_MEMBER_COST_OF_SERVICES,
     +       WVPA_NON_MEMBER_COST_OF_SERVICES,
     +       PROPERTY_TAXES,
     +       WVPA_PROPERTY_TAXES_IN_POWER_COSTS,
     +       CLASS_ADDENDUM_2_OTHER_TAXES,
     +       ADJUSTMENT_2_OP_REV_TAX,
     +       R_REGULATED_REVENUES_13,
     +       R_NON_REG_REVENUES_10
      REAL*4 ALLOCATION_VALUE(AVAIL_DATA_YEARS)
      INTEGER*2 ALLOCATION_VECTOR
      REAL*4 SECONDARY_SALES_NOT_IN_RATES(:,:,:),
     +       OTHER_REVENUES_NOT_IN_RATES(:,:,:)
      ALLOCATABLE :: SECONDARY_SALES_NOT_IN_RATES,
     +               OTHER_REVENUES_NOT_IN_RATES
      INTEGER*2 R_ASSET_CLASS,ASSET_CLASS_ID
      CHARACTER*(*) R_EXPENSE_CLASSIFICATION
      CHARACTER*(*) R_ACCOUNT_CLASSIFICATION
      CHARACTER*(*) R_ACCT_SUBCLASS
      INTEGER*2 REV_TYPE
      REAL*4 R_ANNUAL_REVENUES(0:*),
     +       R_ANNUAL_EXPENSES(0:*),
     +       R_ANNUAL_CASH(0:*)
      REAL*4 R_MONTHLY_REVENUES(12,*),
     +       R_MONTHLY_EXPENSES(12,*),
     +       R_MONTHLY_CASH(12,*)
C
      REAL*4 MIDAS_MONTHLY_BOOKED(12,LAST_AVAILABLE_MONTHLY_YEAR),
     +       MIDAS_MONTHLY_CASH(12,LAST_AVAILABLE_MONTHLY_YEAR),
     +       MONTHLY_ALLOCATED_VALUES(12,LAST_AVAILABLE_MONTHLY_YEAR),
     +       MONTHLY_ALLOCATED_CASH(12,LAST_AVAILABLE_MONTHLY_YEAR)
      REAL (KIND=4), ALLOCATABLE :: MIDAS_ANNUAL_CASH(:)
c     LOGICAL*1 MONTHLY_MIDAS_ACTIVE/.TRUE./
      CHARACTER*1 MIDAS_MONTHLY_DATA_UNITS(LAST_AVAILABLE_MONTHLY_YEAR)
      CHARACTER*4 MIDAS_LAST_MONTH(LAST_AVAILABLE_MONTHLY_YEAR)
      INTEGER*2 MONTH_ENDING(LAST_AVAILABLE_MONTHLY_YEAR),YR,MO,TAX_TYPE
C
      LOGICAL*1 EXPENSES_IN_FINANCIAL_DRILLING,EXPENSE_REVENUES_REPORT
      CHARACTER*1 DRILLING_REPRT_LEVEL,FINANCIAL_DRILLING
      REAL*4 NOT_AVAIL
      PARAMETER(NOT_AVAIL=-999999.)
      CHARACTER*30 DRILLING_NAME,CASH_DRILLING_NAME,
     +             BALANCE_DRILLING_NAME
      CHARACTER*20 COMMODITY_DISTRIBUTION_PTR
      LOGICAL*1 REPORT_ALL_ACCOUNTS
      CHARACTER*6 SHORT_MONTH_NAMES
      INTEGER*2 R_TAX_TYPE
      INTEGER*2 INCOME_DRILLING_UNIT_NO,INCOME_DRILLING_REPRT_UNIT,
     +          CASH_DRILLING_UNIT_NO,CASH_DRILLING_REPRT_UNIT,
     +          BALANCE_DRILLING_REPRT_UNIT,BALANCE_DRILLING_UNIT_NO
      CHARACTER*1 R_ACCOUNT_TYPE
      LOGICAL*1 VOID_LOGICAL,SET_DRILLING_DATA_INCOME_TRUE,
     +          SET_DRILLING_DATA_CASH_TRUE,
     +          WVPA_TRACKER_DATABASE_INFO
      REAL*4 ALLOCATION_ADJUSTMENT
      CHARACTER*1 ACCOUNT_ACTIVE_STATUS
      INTEGER*2 RETURN_MONTHLY_CL_REVENUES
      INTEGER*2 VOID_INT2,RETURN_MONTHLY_CL_EXPENSES,
     +          RETURN_MONTH_CUSTOMER_VARIABLES,
     +          RETURN_MONTH_CUSTOMER_REVENUES
      REAL*4 R_COUNTY_PAYMENT,
     +       R_SUPPLEMENTAL_TRANSFER,
     +       R_STATE_ELECTRIC_TAXES,
     +       R_PSC_TAX
      REAL*4 ETX_EL_STATE_TAX_RATE
      REAL*4 R_AMORTIZATION_ADJUSTMENT,
     +       R_DEFERRED_REVENUE_AMORTIZATION,
     +       R_ATL_LEASE_AMORTIZATION,
     +       R_BTL_LEASE_AMORTIZATION,
     +       R_BOOK_DEPRECIATION,
     +       R_GOVERNMENT_REVENUES,
     +       R_NET_OF_TAX_BTL_REVENUES,
     +       R_BTL_AMORTIZATION_EXP
      REAL*4 TOTAL_CASH_RECEPITS,
     +       TOTAL_CASH_PAYMENTS,
     +       TOTAL_BOOK_REVENUES,
     +       TOTAL_BOOK_EXPENSES,
     +       PROSYM_BOOK_EXPENSES,
     +       PROSYM_BOOK_REVENUES,
     +       R_CHANGE_IN_RECEIVABLES,
     +       R_CHANGE_IN_PAYABLES
      REAL*4 R_ATL_LEASE_INTEREST,
     +       R_BTL_LEASE_INTEREST,
     +       R_BTL_NUC_FUEL_DECOM_LIABILITY,
     +       R_DEFERRED_FUEL_EXPENSE,
     +       R_VACATION_PAY,
     +       R_PENSION_EXPENSE,
     +       R_STORM_EXPENSE,
     +       R_CLASS_GAS_ADJ_CLAUSE_REVENUE,
     +       R_STD_INTEREST,
     +       R_LTD_INTEREST
      INTEGER*2 PERIOD,DATA_POS
      REAL*4 R_VACATION_PAY_CASH,
     +       R_PENSION_EXPENSE_CASH,
     +       R_STORM_EXPENSE_CASH,
     +       R_ExecBenefitsCash,
     +       R_IncentiveCompensationCash
      INTEGER*2 RETURN_MONTHLY_CL_CASH_REVENUES,
     +          RETURN_MONTHLY_CL_CASH_EXPENSES,
     +          RETURN_CUSTOMER_CASH_REVENUES
      REAL*4 R_FUEL_COST(0:12),R_PURCHASE_COST(0:12),
     +       R_NUC_DECOM_COSTS(0:12)
      REAL*4 R_MONTHLY_CHANGE_IN_RECEIVABLES(0:12),
     +       R_MONTHLY_CHANGE_IN_PAYABLES(0:12)
      REAL*4 R_MONTHLY_BOOK_DEPRECIATION(0:12),
     +       R_MONTHLY_BOOK_AMORT(0:12),
     +       R_STD_INTEREST_ADJ(0:12),
     +       R_STD_CASH_INTEREST_ADJ(0:12),
     +       R_LTD_INTEREST_ADJ(0:12),
     +       R_LTD_CASH_INTEREST_ADJ(0:12),
     +       TF_MONTH_PURCHASED_POWER(0:12),
     +       RC_MONTH_PURCHASED_POWER(0:12)
      REAL (KIND=4):: TF_MONTH_PURCHASED_CAPACITY(0:12)
      LOGICAL*1 CONTINUE_PRINTING
      INTEGER*2 Budget Position
      LOGICAL*1 RETURN_ASSET_CLASS_LISTS,ACTIVE_EXPENSE_FILES
      REAL*4 OPREV_TAX_RATE
      LOGICAL*1 EXPENSE_TYPE_IS_CASH
      INTEGER*2 CASH_POS
      INTEGER FILE_ID
      INTEGER MAX_EX_FILES
      PARAMETER (MAX_EX_FILES=6)
      LOGICAL FILE_OPENED
      INTEGER MaxSubClass/0/
      INTEGER SubClass
      REAL R_VARIABLE(0:*)
      LOGICAL*1 LF95,LAHEY_LF95
      INTEGER INCOME_DRILLING_REPRT_REC,
     +        CASH_DRILLING_REPRT_REC,
     +        BALANCE_DRILLING_REPRT_REC
      INTEGER*2 SCENARIO_INDEX,GET_SCENARIO_INDEX
      REAL*4 GET_SCENARIO_BY_INDEX
      REAL*4 R_PHYS_DERIVATIVES_VAR_REVENUE,
     +       R_PHYS_DERIVATIVES_FIX_REVENUE,
     +       R_PHYS_DERIVATIVES_VAR_EXPENSE,
     +       R_PHYS_DERIVATIVES_FIX_EXPENSE,
     +       R_FIN_DERIVATIVES_VAR_REVENUE,
     +       R_FIN_DERIVATIVES_FIX_REVENUE,
     +       R_FIN_DERIVATIVES_VAR_EXPENSE,
     +       R_FIN_DERIVATIVES_FIX_EXPENSE
      INTEGER*2 RETURN_MONTHLY_DERIV_INCOME_VARIABLES,
     +          RETURN_MONTHLY_DERIV_CASH_VARIABLES,
     +          RETURN_MONTHLY_FUEL_DERIV_INCOME_VARS,
     +          RETURN_MONTHLY_FUEL_DERIV_CASH_VARS
      REAL MIDAS_MONTHLY_BUDGET_VALUES(60), !60 VALUES
     +     MIDAS_ANNUAL_BUDGET_VALES(5)   ! 5 VALUES LAST ONE 194
      INTEGER WVPA_ACCOUNTING_UNIT,  ! 195
     +        WVPA_SUB_ACCOUNT_NUMBER  ! 196
      CHARACTER*20 WVPA_TRACKING_TYPE
      REAL MONTHLY_CASH_ACTUAL(12,2)   ! 198-221
      CHARACTER*10 MONTHLY_CASH_VALUES_TYPE(2) ! 222-223
      LOGICAL (KIND=1) :: PROPERTY_TAX_RECORD
      INTEGER (KIND=2) :: START_YR
      REAL (KIND=4) :: AVERAGE_MULTIPLIER

      LF95 = LAHEY_LF95()
      FINANCIAL_SIMULATION_YEARS=MAX(LAST_AVAILABLE_MONTHLY_YEAR+1,
     +                                    RUN_YEARS()+EXTENSION_YEARS())
      CALL SET_UP_EXPENSE_ARRAYS
C
      ALLOCATE(READ_EXPEN(0:AVAIL_DATA_YEARS))
      ALLOCATE(EXPENSES(0:FINANCIAL_SIMULATION_YEARS))
      ALLOCATE(MIDAS_ANNUAL_CASH(0:FINANCIAL_SIMULATION_YEARS))
      ALLOCATE(ACCOUNT_PAYABLE_VALUES(0:FINANCIAL_SIMULATION_YEARS))
      ALLOCATE(OUTPUT_VALUE(0:FINANCIAL_SIMULATION_YEARS))
      ALLOCATE(ALLOC_EXPEN(0:FINANCIAL_SIMULATION_YEARS))
      ALLOCATE(ALLOC_CASH_EXPEN(0:FINANCIAL_SIMULATION_YEARS))
      ALLOCATE(ASSET_CLASS_LIST(AVAIL_DATA_YEARS))
      ALLOCATE(ASSET_ALLOCATION_LIST(AVAIL_DATA_YEARS))
C
C CHECK FOR INCLUDING EXPENSES & REVENUES IN FINANCIAL DRILLING
C
      EXPENSES_IN_FINANCIAL_DRILLING =
     +                      EXPENSE_REVENUES_REPORT(REPORT_ALL_ACCOUNTS)
     +                                       .AND.  .NOT. SAVE_BASE_CASE
      IF(EXPENSES_IN_FINANCIAL_DRILLING) THEN
         INCOME_DRILLING_REPRT_UNIT = INCOME_DRILLING_UNIT_NO()
         CASH_DRILLING_REPRT_UNIT = CASH_DRILLING_UNIT_NO()
         BALANCE_DRILLING_REPRT_UNIT = BALANCE_DRILLING_UNIT_NO()
         DRILLING_REPRT_LEVEL = FINANCIAL_DRILLING()
         EXPENSES_IN_FINANCIAL_DRILLING=DRILLING_REPRT_LEVEL/= 'O' .AND.
     +                                    EXPENSES_IN_FINANCIAL_DRILLING
      ENDIF
      TACCTS = 0
      ACCT_SUBCLASS = '0'
      ACTIVE_EXPENSE_FILES = .FALSE.
      IF(LF95) THEN
         WRITE(SCREEN_MESSAGES,"(A)") "Exp/Rev Accounts "
         CALL MG_LOCATE_WRITE(18,70,TRIM(SCREEN_MESSAGES),3,2)
      ENDIF
      DO FILE_ID = 0, MAX_EX_FILES-1

         CALL OPEN_EXPENSE_FILE(11,FILE_ID,FILE_OPENED)
         IF(.NOT. FILE_OPENED) CYCLE
         ACTIVE_EXPENSE_FILES = .TRUE.
         IN_REC = 0
C
         DO
            IN_REC = IN_REC + 1
            READ(11,REC=IN_REC,IOSTAT=IOS) DELETE,DESC,ACCTNO,
     + EXPENSE_CLASSIFICATION,
     + EXPENSE_cln_local,
     + READ_EXPEN,DATA_TYPE,ESCAL_VECT,
     + ASSET_CLASS_ID,
     + ASSET_ALLOCATION_VECTOR,
     + ACCOUNT_TYPE_STR,
     + REVENUE_CLASSIFICATION,
     + INTRA_COMPANY_TRANSACTION,
     + INTRA_ASSET_CLASS_ID,
     + INTRA_ASSET_CLASS_ALLOCATION,
     + INTRA_ACCOUNT_CLASSIFICATION,
     + INTRA_EXPENSE_COLLECTION,
     + BOOK_VALUES_VECTOR, ! 48 NOT USED
     + CASH_TYPE,
     + CASH_VALUES_VECTOR,
     + CASH_ACCRUAL_AMOUNT,
     + TAX_CLASSIFICATION, ! 52 NOT USED
     + TAX_APPLICATION, ! 53 NOT USED
     + TAX_STATUS, ! 54
     + ALLOCATION_ADJUSTMENT, ! 55  PLACE HOLDERS
     + ACCOUNT_ACTIVE_STATUS, ! 56
     + CASH_LAG_PATTERN, ! 57
     + ACCT_SUBCLASS, ! 58
     + COMMODITY_DISTRIBUTION_PTR, ! 59
     + MIDAS_MONTHLY_BOOKED, ! 60 VALUES
     + MIDAS_MONTHLY_DATA_UNITS, ! 5 VALUES
     + MIDAS_LAST_MONTH,   ! 5 VALUES
     + MIDAS_MONTHLY_BUDGET_VALUES, !60 VALUES
     + MIDAS_ANNUAL_BUDGET_VALES,   ! 5 VALUES LAST ONE 194
     + WVPA_ACCOUNTING_UNIT,  ! 195
     + WVPA_SUB_ACCOUNT_NUMBER,  ! 196
     + WVPA_TRACKING_TYPE,  ! 197
     + MONTHLY_CASH_ACTUAL, ! 198-221
     + MONTHLY_CASH_VALUES_TYPE,     ! 222-223
     + INTRA_ACCT_SUBCLASS !after budget

            IF(IOS /= 0) EXIT
            TACCTS = TACCTS + 1
            IF(DELETE > 7) CYCLE
            MIDAS_ANNUAL_CASH = 0.
            IF(LF95) THEN
               WRITE(SCREEN_MESSAGES,'(I4,A)') TACCTS,"-"//DESC
               CALL MG_CLEAR_LINE_WRITE(17,70,73,
     +                                      TRIM(SCREEN_MESSAGES),3,0)
            ELSE
               WRITE(SCREEN_MESSAGES,'(I4)') TACCTS
               CALL MG_CLEAR_LINE_WRITE(17,70,73,
     +                                          TRIM(SCREEN_MESSAGES),
     +                                                   ALL_VERSIONS,0)
            ENDIF
            IF( 
     +          (INDEX(ACCOUNT_TYPE_STR,'Not') /= 0) .OR. 
     +          (ACCOUNT_ACTIVE_STATUS == 'N')
     +        )
     +       then
                CYCLE ! Not Active CYCLE
            endif
            IF(INDEX(ACCOUNT_TYPE_STR,'Cash') /= 0) THEN
               IF(INDEX(ACCOUNT_TYPE_STR,'Pay') /= 0) THEN
                  ACCOUNT_TYPE = 'P'
               ELSE
                  ACCOUNT_TYPE = 'I'
               ENDIF
            ELSE
               ACCOUNT_TYPE = ACCOUNT_TYPE_STR(1:1)
            ENDIF
C
            ESCAL_VECT = ABS(ESCAL_VECT)
            TAX_TYPE = Item is taxed
            IF(TAX_STATUS(1:1) == 'N') TAX_TYPE = Item is not taxed
            IF(ACCOUNT_TYPE == 'E' .AND.
     +         INDEX(EXPENSE_CLASSIFICATION,'Owned Nuclear Fuel') /= 0)
     +                                                   CASH_TYPE = 'N'
C
            CALL CREATE_DOLLAR_STREAM(FINANCIAL_SIMULATION_YEARS,
     +                         DATA_TYPE,ESCAL_VECT,READ_EXPEN,EXPENSES)

C
C MONTHLY INFORMATION
C
C
C RIPPLE THE MONTHLY VALUES
C
            IF(.NOT. MONTHLY_MIDAS_ACTIVE) MIDAS_MONTHLY_DATA_UNITS ='A'
            IF(MONTHLY_MIDAS_ACTIVE) THEN
C
C MONTHLY BOOK VALUES
C
               CALL RIPPLE_MONTHLY_DATA(EXPENSES(1),
     +                                             MIDAS_MONTHLY_BOOKED)
               CALL MAP_LAST_MONTH(MIDAS_LAST_MONTH,MONTH_ENDING)
               CALL MONTHLY_BOOK_VALUES_IN_DOLLARS(EXPENSES(1),
     +                                         MIDAS_MONTHLY_BOOKED,
     +                                         MIDAS_MONTHLY_DATA_UNITS,
     +                                         MONTH_ENDING)
C
C MONTHLY CASH VALUES
C
               PROPERTY_TAX_RECORD = ACCOUNT_TYPE == 'E' .AND.
     +                   INDEX(EXPENSE_CLASSIFICATION,'Property Tax')/=0
               CALL MONTHLY_CASH_VALUES_IN_DOLLARS(EXPENSES,
     + MIDAS_MONTHLY_BOOKED,
     + MIDAS_MONTHLY_CASH,
     + MIDAS_ANNUAL_CASH,
     + CASH_TYPE,
     + CASH_VALUES_VECTOR,
     + CASH_ACCRUAL_AMOUNT,
     + CASH_LAG_PATTERN,
     + MONTHLY_CASH_ACTUAL, ! 198-221
     + MONTHLY_CASH_VALUES_TYPE,     ! 222-223
     + FINANCIAL_SIMULATION_YEARS,
     + PROPERTY_TAX_RECORD)
            ENDIF
C
C MODIFY QUANTITIES USING LATIN HYPERCUBE MODIFIED 10/10/06 Dr.G
C
            IF(INDEX(COMMODITY_DISTRIBUTION_PTR,"Not Active") == 0) THEN

               SCENARIO_INDEX =
     +                    GET_SCENARIO_INDEX(COMMODITY_DISTRIBUTION_PTR)
               IF(SCENARIO_INDEX <= 0) EXIT
               START_YR = 1
               IF(MONTHLY_MIDAS_ACTIVE) THEN
                  DO YR = 1, LAST_AVAILABLE_MONTHLY_YEAR
                     DO MO = 1, 12
                        MIDAS_MONTHLY_BOOKED(MO,YR) =
     +                       GET_SCENARIO_BY_INDEX(YR,MO,SCENARIO_INDEX)
     +                        * MIDAS_MONTHLY_BOOKED(MO,YR)
                        MIDAS_MONTHLY_CASH(MO,YR) =
     +                       GET_SCENARIO_BY_INDEX(YR,MO,SCENARIO_INDEX)
     +                        * MIDAS_MONTHLY_CASH(MO,YR)
                     ENDDO
                     EXPENSES(YR) = SUM(MIDAS_MONTHLY_BOOKED(:,YR))
                     MIDAS_ANNUAL_CASH(YR) =
     +                                     SUM(MIDAS_MONTHLY_CASH(:,YR))
                  ENDDO
                  START_YR = YR
               ENDIF
               DO YR = START_YR, FINANCIAL_SIMULATION_YEARS
                  AVERAGE_MULTIPLIER = 0.
                  DO MO = 1, 12
                     AVERAGE_MULTIPLIER = AVERAGE_MULTIPLIER
     +                     + GET_SCENARIO_BY_INDEX(YR,MO,SCENARIO_INDEX)
                  ENDDO
                  AVERAGE_MULTIPLIER = AVERAGE_MULTIPLIER/12.
                  EXPENSES(YR) = EXPENSES(YR) * AVERAGE_MULTIPLIER
                  IF(YR <= MIN(FINANCIAL_SIMULATION_YEARS,
     +                                           AVAIL_DATA_YEARS)) THEN
                     MIDAS_ANNUAL_CASH(YR) = MIDAS_ANNUAL_CASH(YR)
     +                                              * AVERAGE_MULTIPLIER
                  ENDIF
               ENDDO
            ENDIF
C END LHC MODIFICATIONS
C ADJUST AMOUNTS FOR OWNERSHIP FOR CP&L 4/9/99 MSG
            IF(ALLOCATION_ADJUSTMENT /= 100.) THEN
               ALLOCATION_ADJUSTMENT = ALLOCATION_ADJUSTMENT/100.
               DO YR = 1, FINANCIAL_SIMULATION_YEARS
                  EXPENSES(YR) = ALLOCATION_ADJUSTMENT * EXPENSES(YR)
               ENDDO
               DO YR=0,MIN(FINANCIAL_SIMULATION_YEARS,AVAIL_DATA_YEARS)
                  MIDAS_ANNUAL_CASH(YR) = ALLOCATION_ADJUSTMENT *
     +                                             MIDAS_ANNUAL_CASH(YR)
               ENDDO
               DO YR = 1, LAST_AVAILABLE_MONTHLY_YEAR
                  DO MO = 1, 12
                     MIDAS_MONTHLY_BOOKED(MO,YR)=ALLOCATION_ADJUSTMENT *
     +                                       MIDAS_MONTHLY_BOOKED(MO,YR)
                     MIDAS_MONTHLY_CASH(MO,YR) = ALLOCATION_ADJUSTMENT *
     +                                         MIDAS_MONTHLY_CASH(MO,YR)
                  ENDDO
               ENDDO
            ENDIF
C
C FINANCIAL DRILLING AT ANNUAL OR MONTHLY LEVEL
C
            IF(EXPENSES_IN_FINANCIAL_DRILLING .AND.
     +                (DRILLING_REPRT_LEVEL == 'A' .OR.
     +                                DRILLING_REPRT_LEVEL == 'M') .AND.
     +                       (REPORT_ALL_ACCOUNTS .OR. DELETE > 1)) THEN
               IF(INTRA_COMPANY_TRANSACTION == 'Y') THEN
                  DRILLING_ACCOUNT_NAME = '* '//DESC
               ELSE
                  DRILLING_ACCOUNT_NAME = DESC
               ENDIF
               IF(WVPA()) THEN
                  DRILLING_ACCOUNT_NAME =
     +                      WVPA_REPORTING_NAME(DRILLING_ACCOUNT_NAME,
     +                                          ACCTNO,
     +                                          WVPA_ACCOUNTING_UNIT,
     +                                          WVPA_SUB_ACCOUNT_NUMBER)
               ENDIF
               CONTINUE_PRINTING = .TRUE.
               SAVE_ACCOUNT_TYPE = ACCOUNT_TYPE
               SAVE_REVENUE_CLASSIFICATION = REVENUE_CLASSIFICATION
               SAVE_EXPENSE_CLASSIFICATION = EXPENSE_CLASSIFICATION
C
            DO
               IF(ACCOUNT_TYPE == 'R') THEN
                  DRILLING_NAME = REVENUE_CLASSIFICATION
                  CASH_DRILLING_NAME = 'Revenue Receipts'
                  BALANCE_DRILLING_NAME = 'Change in Receivables'
                  VOID_LOGICAL = SET_DRILLING_DATA_INCOME_TRUE()
                  DO YR = 1, FINANCIAL_SIMULATION_YEARS
                     ACCOUNT_PAYABLE_VALUES(YR) = EXPENSES(YR)
     +                                           - MIDAS_ANNUAL_CASH(YR)
                  ENDDO
               ELSEIF(ACCOUNT_TYPE == 'E') THEN
                  DRILLING_NAME = EXPENSE_CLASSIFICATION
                  CASH_DRILLING_NAME = 'Expense Payments'
                  BALANCE_DRILLING_NAME = 'Change in Payables'
                  VOID_LOGICAL = SET_DRILLING_DATA_INCOME_TRUE()
                  SELECT CASE (TRIM(EXPENSE_CLASSIFICATION))
                     CASE ('Vacation Pay','Funded Pension',
     +                     'Storm Expense')
                        CASH_DRILLING_NAME = 'Balance Sheet Payment'
                        DO YR = 1, FINANCIAL_SIMULATION_YEARS
                           ACCOUNT_PAYABLE_VALUES(YR) = 0.
                        ENDDO
                     CASE DEFAULT
                        DO YR = 1, FINANCIAL_SIMULATION_YEARS
                           ACCOUNT_PAYABLE_VALUES(YR) = EXPENSES(YR)
     +                                           - MIDAS_ANNUAL_CASH(YR)
                        ENDDO
                  END SELECT
               ELSE
                  DRILLING_NAME = ACCOUNT_TYPE_STR
                  VOID_LOGICAL = SET_DRILLING_DATA_CASH_TRUE()
                  IF(ACCOUNT_TYPE == 'P') THEN
                     BALANCE_DRILLING_NAME = 'Change in Payables'
                  ELSE
                     BALANCE_DRILLING_NAME = 'Change in Receivables'
                  ENDIF
                  DO YR = 1, FINANCIAL_SIMULATION_YEARS
                     ACCOUNT_PAYABLE_VALUES(YR) = -EXPENSES(YR)
                  ENDDO
               ENDIF

               IF(DRILLING_REPRT_LEVEL == 'A') THEN
                  IF(ACCOUNT_TYPE == 'R' .OR. ACCOUNT_TYPE == 'E') THEN
                     IREC = INCOME_DRILLING_REPRT_REC()
                     WRITE(INCOME_DRILLING_REPRT_UNIT,REC=IREC)
     +                                       PRT_ENDPOINT(),
     +                                       DRILLING_NAME,
     +                                       DRILLING_ACCOUNT_NAME,
     +                                     (EXPENSES(YR),YR = 1,
     +                                       FINANCIAL_SIMULATION_YEARS)
                     IF(CASH_TYPE /= 'N') THEN
                        VOID_LOGICAL = SET_DRILLING_DATA_CASH_TRUE()
                        IREC = CASH_DRILLING_REPRT_REC()
                        WRITE(CASH_DRILLING_REPRT_UNIT,REC=IREC)
     +                                          PRT_ENDPOINT(),
     +                                          CASH_DRILLING_NAME,
     +                                          DRILLING_ACCOUNT_NAME,
     +                                   (MIDAS_ANNUAL_CASH(YR),YR=1,
     +                                      LAST_AVAILABLE_MONTHLY_YEAR)
                     ELSEIF(INDEX(EXPENSE_CLASSIFICATION,
     +                                      'Funded Pension') /= 0) THEN
                        CASH_DRILLING_NAME = 'Funded Pension'
                        VOID_LOGICAL = SET_DRILLING_DATA_CASH_TRUE()
                        IREC = CASH_DRILLING_REPRT_REC()
                        WRITE(CASH_DRILLING_REPRT_UNIT,REC=IREC)
     +                                          PRT_ENDPOINT(),
     +                                          CASH_DRILLING_NAME,
     +                                          DRILLING_ACCOUNT_NAME,
     +                                   (MIDAS_ANNUAL_CASH(YR),YR=1,
     +                                      LAST_AVAILABLE_MONTHLY_YEAR)
                     ENDIF
                  ELSE
                     IREC = CASH_DRILLING_REPRT_REC()
                     WRITE(CASH_DRILLING_REPRT_UNIT,REC=IREC)
     +                                       PRT_ENDPOINT(),
     +                                       DRILLING_NAME,
     +                                       DRILLING_ACCOUNT_NAME,
     +                                   (EXPENSES(YR),YR=1,
     +                                      LAST_AVAILABLE_MONTHLY_YEAR)
                  ENDIF
                  IREC = BALANCE_DRILLING_REPRT_REC()
                  WRITE(BALANCE_DRILLING_REPRT_UNIT,REC=IREC)
     +                                PRT_ENDPOINT(),
     +                                BALANCE_DRILLING_NAME,
     +                                DRILLING_ACCOUNT_NAME,
     +                               (ACCOUNT_PAYABLE_VALUES(YR),YR = 1,
     +                                       FINANCIAL_SIMULATION_YEARS)
               ELSEIF(DRILLING_REPRT_LEVEL == 'M') THEN
                  IF(ACCOUNT_TYPE == 'R' .OR. ACCOUNT_TYPE == 'E') THEN
                     IREC = INCOME_DRILLING_REPRT_REC()
                     WRITE(INCOME_DRILLING_REPRT_UNIT,REC=IREC)
     +                                       PRT_ENDPOINT(),
     +                                       DRILLING_NAME,
     +                                       SHORT_MONTH_NAMES(INT2(0)),
     +                                       DRILLING_ACCOUNT_NAME,
     +                                   (EXPENSES(YR),YR = 1,
     +                                       FINANCIAL_SIMULATION_YEARS)
                     DO MO = 1, 12
                        DO YR = 1, FINANCIAL_SIMULATION_YEARS
                           IF(YR >= 1 .AND.
     +                           YR <= LAST_AVAILABLE_MONTHLY_YEAR) THEN
                              OUTPUT_VALUE(YR) =
     +                                       MIDAS_MONTHLY_BOOKED(MO,YR)
                           ELSE
                              OUTPUT_VALUE(YR) = NOT_AVAIL
                           ENDIF
                        ENDDO
                       IREC = INCOME_DRILLING_REPRT_REC()
                        WRITE(INCOME_DRILLING_REPRT_UNIT,REC=IREC)
     +                                          PRT_ENDPOINT(),
     +                                          DRILLING_NAME,
     +                                          SHORT_MONTH_NAMES(MO),
     +                                          DRILLING_ACCOUNT_NAME,
     +                                      (OUTPUT_VALUE(YR),YR=1,
     +                                       FINANCIAL_SIMULATION_YEARS)
                     ENDDO
                     IF(CASH_TYPE /= 'N') THEN
                        VOID_LOGICAL = SET_DRILLING_DATA_CASH_TRUE()
                        IREC = CASH_DRILLING_REPRT_REC()
                        WRITE(CASH_DRILLING_REPRT_UNIT,REC=IREC)
     +                                    PRT_ENDPOINT(),
     +                                       CASH_DRILLING_NAME,
     +                                       SHORT_MONTH_NAMES(INT2(0)),
     +                                            DRILLING_ACCOUNT_NAME,
     +                                   (MIDAS_ANNUAL_CASH(YR),YR=1,
     +                                      LAST_AVAILABLE_MONTHLY_YEAR)
                        DO MO = 1, 12
                           IREC = CASH_DRILLING_REPRT_REC()
                           WRITE(CASH_DRILLING_REPRT_UNIT,REC=IREC)
     +                                            PRT_ENDPOINT(),
     +                                            CASH_DRILLING_NAME,
     +                                            SHORT_MONTH_NAMES(MO),
     +                                            DRILLING_ACCOUNT_NAME,
     +                                  (MIDAS_MONTHLY_CASH(MO,YR),YR=1,
     +                                      LAST_AVAILABLE_MONTHLY_YEAR)
                        ENDDO
                     ENDIF
                  ELSE
                     IREC = CASH_DRILLING_REPRT_REC()
                     WRITE(CASH_DRILLING_REPRT_UNIT,REC=IREC)
     +                                   PRT_ENDPOINT(),
     +                                       DRILLING_NAME,
     +                                       SHORT_MONTH_NAMES(INT2(0)),
     +                                       DRILLING_ACCOUNT_NAME,
     +                                  (EXPENSES(YR),YR = 1,
     +                                      LAST_AVAILABLE_MONTHLY_YEAR)
                     DO MO = 1, 12
                        IREC = CASH_DRILLING_REPRT_REC()
                        WRITE(CASH_DRILLING_REPRT_UNIT,REC=IREC)
     +                                 PRT_ENDPOINT(),
     +                                            DRILLING_NAME,
     +                                            SHORT_MONTH_NAMES(MO),
     +                                            DRILLING_ACCOUNT_NAME,
     +                                (MIDAS_MONTHLY_BOOKED(MO,YR),YR=1,
     +                                      LAST_AVAILABLE_MONTHLY_YEAR)
                     ENDDO
                  ENDIF
               ENDIF
               IF(INTRA_COMPANY_TRANSACTION == 'Y' .AND.
     +                                           CONTINUE_PRINTING) THEN
                  CONTINUE_PRINTING = .FALSE.
                  IF(ACCOUNT_TYPE == 'R') THEN
                     ACCOUNT_TYPE = 'E'
                     EXPENSE_CLASSIFICATION=INTRA_ACCOUNT_CLASSIFICATION
                  ELSEIF(ACCOUNT_TYPE == 'E') THEN
                     ACCOUNT_TYPE = 'R'
                     REVENUE_CLASSIFICATION=INTRA_ACCOUNT_CLASSIFICATION
                  ELSEIF(ACCOUNT_TYPE == 'P') THEN
                     ACCOUNT_TYPE = 'I'
                  ELSEIF(ACCOUNT_TYPE == 'I') THEN
                     ACCOUNT_TYPE = 'P'
                  ELSE
                     EXIT
                  ENDIF
               ELSE
                  EXIT
               ENDIF
            ENDDO ! INTRA LOOP
               ACCOUNT_TYPE = SAVE_ACCOUNT_TYPE
               REVENUE_CLASSIFICATION = SAVE_REVENUE_CLASSIFICATION
               EXPENSE_CLASSIFICATION = SAVE_EXPENSE_CLASSIFICATION
            ENDIF
C
C TRACK FUEL, PURCHASE POWER FOR CP&L BUDGET NEEDS
C
            IF(INTRA_COMPANY_TRANSACTION /= 'Y' .AND.
     +                                         ACCOUNT_TYPE == 'E') THEN
C
               EXTYPE=INCOME_STATEMENT_POSITION(EXPENSE_CLASSIFICATION)
               Budget Position = 0
               IF(EXTYPE == Fossil Fuel) THEN
                  Budget Position = 1 ! Fossil Fuel-10
               ELSEIF(EXTYPE == Purchased Power) THEN
                  Budget Position = 2 ! Purchased Power-10
               ENDIF
C
               IF(Budget Position /= 0) THEN
                  DO YR = 1,FINANCIAL_SIMULATION_YEARS
                     BUDGET_EXPENSE(0,YR,Budget Position) =
     +                              BUDGET_EXPENSE(0,YR,Budget Position)
     +                              + EXPENSES(YR)
                     IF(YR <= LAST_AVAILABLE_MONTHLY_YEAR) THEN
                        DO MO = 1, 12
                           BUDGET_EXPENSE(MO,YR,Budget Position) =
     +                             BUDGET_EXPENSE(MO,YR,Budget Position)
     +                             + MIDAS_MONTHLY_BOOKED(MO,YR)
                      ENDDO
                     ENDIF
                  ENDDO
               ENDIF
            ENDIF
C
C INTRA-COMPANY TRACKING
C
            IF(INTRA_COMPANY_TRANSACTION == 'Y' .AND.
     +                                 .NOT. (ACCOUNT_TYPE == 'T')) THEN
C ALLOCATOTAL COMPANY AND ASSET CLASSES
               IF(ACCOUNT_TYPE == 'R') THEN
                  EXTYPE = INCOME_STATEMENT_POSITION(
     +                                     INTRA_ACCOUNT_CLASSIFICATION)
                  IF(EXTYPE == Purchased Power) THEN
                     WRITE(4,*) 'Intra-company Purchased Power'
                     WRITE(4,*) IN_REC,DESC
                  ENDIF

                  CALL EXPENSE_ACCUMLATION(INT2(-1),EXPENSES,
     + INTRA_ACCOUNT_CLASSIFICATION,
     + INTRA_EXPENSE_COLLECTION, ! DON'T ELIM THE ADJ COLLECTION
     + MIDAS_MONTHLY_BOOKED,
     + TAX_TYPE,
     + INTRA_ACCT_SUBCLASS)

                  CALL REVENUE_ACCUMLATION(INT2(-1),EXPENSES,
     +                                  REVENUE_CLASSIFICATION,
     +                                  'XXX',
     +                                  MIDAS_MONTHLY_BOOKED,
     +                                  TAX_TYPE,
     +                                  ACCT_SUBCLASS)
                  IF(CASH_TYPE /= 'N') THEN
                     CALL CASH_ACCUMLATION(ACCOUNT_TYPE,INT2(-1),
     +                                  MIDAS_ANNUAL_CASH,
     +                                  MIDAS_MONTHLY_CASH,
     +                                  REVENUE_CLASSIFICATION)
                     IF(INDEX(INTRA_EXPENSE_COLLECTION,'BTL') /= 0) THEN
                        CALL CASH_ACCUMLATION('E',INT2(-1),
     +                                     MIDAS_ANNUAL_CASH,
     +                                     MIDAS_MONTHLY_CASH,
     +                                     INTRA_EXPENSE_COLLECTION)
                     ELSE
                        CALL CASH_ACCUMLATION('E',INT2(-1),
     +                                     MIDAS_ANNUAL_CASH,
     +                                     MIDAS_MONTHLY_CASH,
     +                                     INTRA_ACCOUNT_CLASSIFICATION)
                     ENDIF
                  ENDIF
               ELSEIF(ACCOUNT_TYPE == 'E') THEN
                  EXTYPE = INCOME_STATEMENT_POSITION(
     +                                           EXPENSE_CLASSIFICATION)
                  IF(EXTYPE == Purchased Power) THEN
                     WRITE(4,*) 'Intra-company Purchased Power'
                     WRITE(4,*) IN_REC,DESC
                  ENDIF
                  CALL REVENUE_ACCUMLATION(INT2(-1),EXPENSES,
     +                                  INTRA_ACCOUNT_CLASSIFICATION,
     +                                  'XXX',
     +                                  MIDAS_MONTHLY_BOOKED,
     +                                  TAX_TYPE,
     +                                  INTRA_ACCT_SUBCLASS)
                  CALL EXPENSE_ACCUMLATION(INT2(-1),EXPENSES,
     +                                  EXPENSE_CLASSIFICATION,
     +                                  EXPENSE_cln_local, 
     +                                  MIDAS_MONTHLY_BOOKED,
     +                                  TAX_TYPE,
     +                                  ACCT_SUBCLASS)

                  IF(CASH_TYPE /= 'N') THEN
                     IF(INDEX(EXPENSE_cln_local,'BTL') /= 0) THEN
                        CALL CASH_ACCUMLATION(ACCOUNT_TYPE,INT2(-1),
     +                                     MIDAS_ANNUAL_CASH,
     +                                     MIDAS_MONTHLY_CASH,
     +                                     EXPENSE_cln_local)
                     ELSE
                        CALL CASH_ACCUMLATION(ACCOUNT_TYPE,INT2(-1),
     +                                     MIDAS_ANNUAL_CASH,
     +                                     MIDAS_MONTHLY_CASH,
     +                                     EXPENSE_CLASSIFICATION)
                     ENDIF
                     CALL CASH_ACCUMLATION('R',INT2(-1),
     +                                  MIDAS_ANNUAL_CASH,
     +                                  MIDAS_MONTHLY_CASH,
     +                                  INTRA_ACCOUNT_CLASSIFICATION)
                  ENDIF
               ELSEIF(ACCOUNT_TYPE == 'P') THEN
                  CALL CASH_PAYMENT_ACCUMLATION('I',INT2(-1),
     +                                       MIDAS_ANNUAL_CASH,
     +                                       MIDAS_MONTHLY_CASH)
               ELSEIF(ACCOUNT_TYPE == 'I') THEN
                  CALL CASH_PAYMENT_ACCUMLATION('P',INT2(-1),
     +                                       MIDAS_ANNUAL_CASH,
     +                                       MIDAS_MONTHLY_CASH)
               ENDIF
               VOID_LOGICAL = RETURN_ASSET_CLASS_LISTS(
     +                                     INTRA_ASSET_CLASS_ID,
     +                                     ASSET_CLASS_LIST,
     +                                     INTRA_ASSET_CLASS_ALLOCATION,
     +                                     ASSET_ALLOCATION_LIST)
C
               CLASS_POINTER = 1
               DO
                  ASSET_CLASS = ASSET_CLASS_LIST(CLASS_POINTER)
                  CALL CHECK_IF_CLASS_DEFINED(ASSET_CLASS)
                  ASSET_CLASS = ASSET_CLASS + 1
                  IF(ASSET_CLASS > 0) ASSET_CLASS =
     +                                  ASSET_CLASS_POINTER(ASSET_CLASS)
                  IF(ASSET_ALLOCATION_LIST(CLASS_POINTER) < 0.) THEN
                     MONTHLY_ALLOCATED_VALUES = 0.
                     MONTHLY_ALLOCATED_CASH = 0.
                     ALLOCATION_VECTOR =
     +                         ABS(ASSET_ALLOCATION_LIST(CLASS_POINTER))
                     CALL GET_ASSET_VAR(ALLOCATION_VECTOR,
     +                                      DUMMY_TYPE,ALLOCATION_VALUE)
                     ALLOC_EXPEN(0)=EXPENSES(0)*ALLOCATION_VALUE(1)/100.
C
                     IF(CASH_TYPE /= 'N') THEN ! .AND.
C    +               (ACCOUNT_TYPE == 'R' .OR. ACCOUNT_TYPE == 'E'))THEN
                        ALLOC_CASH_EXPEN(0) = MIDAS_ANNUAL_CASH(0) *
     +                                          ALLOCATION_VALUE(1)/100.
                     ENDIF
C
                     DO I = 1, FINANCIAL_SIMULATION_YEARS
                        IF(I <= AVAIL_DATA_YEARS)
     +                        ASSET_ALLOCATOR = ALLOCATION_VALUE(I)/100.
                        ALLOC_EXPEN(I) = EXPENSES(I) * ASSET_ALLOCATOR
                        IF(MONTHLY_MIDAS_ACTIVE .AND. (I >= 1 .AND.
     +                           I <= LAST_AVAILABLE_MONTHLY_YEAR)) THEN
c                           DO MO = 1, 12
                           MONTHLY_ALLOCATED_VALUES(1:,I) =
     +                               ASSET_ALLOCATOR *
     +                                        MIDAS_MONTHLY_BOOKED(1:,I)
c                           ENDDO
                        ENDIF
                        IF(CASH_TYPE /= 'N') THEN 
                           ALLOC_CASH_EXPEN(I) = MIDAS_ANNUAL_CASH(I) *
     +                                                   ASSET_ALLOCATOR
                           IF(MONTHLY_MIDAS_ACTIVE .AND. (I >= 1 .AND.
     +                           I <= LAST_AVAILABLE_MONTHLY_YEAR)) THEN
c                              DO MO = 1, 12
                              MONTHLY_ALLOCATED_CASH(1:,I) =
     +                                       ASSET_ALLOCATOR *
     +                                          MIDAS_MONTHLY_CASH(1:,I)
c                              ENDDO
                           ENDIF
                        ENDIF
                     ENDDO
                  ELSE
                     ASSET_ALLOCATOR =
     +                         ASSET_ALLOCATION_LIST(CLASS_POINTER)/100.
C
                     DO I = 0, FINANCIAL_SIMULATION_YEARS
                        ALLOC_EXPEN(I) = EXPENSES(I) * ASSET_ALLOCATOR
                        IF(CASH_TYPE /= 'N') ! .AND.
C    +                   (ACCOUNT_TYPE == 'R' .OR. ACCOUNT_TYPE == 'E'))
     +                      ALLOC_CASH_EXPEN(I) = MIDAS_ANNUAL_CASH(I) *
     +                                                   ASSET_ALLOCATOR
                     ENDDO
                     IF(MONTHLY_MIDAS_ACTIVE) THEN
                        DO YR = 1, LAST_AVAILABLE_MONTHLY_YEAR
                           DO MO = 1, 12
                              MONTHLY_ALLOCATED_VALUES(MO,YR) =
     +                                   ASSET_ALLOCATOR *
     +                                       MIDAS_MONTHLY_BOOKED(MO,YR)
                              IF(CASH_TYPE /= 'N') ! .AND.
C    +                        (ACCOUNT_TYPE == 'R' .OR.
C    +                                             ACCOUNT_TYPE == 'E'))
     +                              MONTHLY_ALLOCATED_CASH(MO,YR) =
     +                                      ASSET_ALLOCATOR *
     +                                         MIDAS_MONTHLY_CASH(MO,YR)
                           ENDDO
                        ENDDO
                     ELSE
                        MONTHLY_ALLOCATED_VALUES = 0.
                        MONTHLY_ALLOCATED_CASH = 0.
                     ENDIF
                  ENDIF
c              IF(ACCOUNT_TYPE == 'R') THEN
c                 CALL EXPENSE_ACCUMLATION(ASSET_CLASS,ALLOC_EXPEN,
c    +                                     INTRA_ACCOUNT_CLASSIFICATION,
c    +                                     INTRA_EXPENSE_COLLECTION,
c    +                                     MONTHLY_ALLOCATED_VALUES,
c    +                                     TAX_TYPE,
c    +                                     ACCT_SUBCLASS)
c              ELSE
c                 CALL REVENUE_ACCUMLATION(ASSET_CLASS,ALLOC_EXPEN,
c    +                                     INTRA_ACCOUNT_CLASSIFICATION,
c    +                                     INTRA_EXPENSE_COLLECTION,
c    +                                     MONTHLY_ALLOCATED_VALUES,
c    +                                     TAX_TYPE,
c    +                                     ACCT_SUBCLASS)
c              ENDIF
                  IF(ACCOUNT_TYPE == 'E') THEN
                     CALL REVENUE_ACCUMLATION(ASSET_CLASS,ALLOC_EXPEN,
     +                                     INTRA_ACCOUNT_CLASSIFICATION,
     +                                     INTRA_EXPENSE_COLLECTION,
     +                                     MONTHLY_ALLOCATED_VALUES,
     +                                     TAX_TYPE,
     +                                     INTRA_ACCT_SUBCLASS)
                     IF(CASH_TYPE /= 'N') THEN
                        CALL CASH_ACCUMLATION('R',ASSET_CLASS,
     +                                     ALLOC_CASH_EXPEN,
     +                                     MONTHLY_ALLOCATED_CASH,
     +                                     INTRA_ACCOUNT_CLASSIFICATION)
                     ENDIF
                  ELSEIF(ACCOUNT_TYPE == 'R') THEN
                     CALL EXPENSE_ACCUMLATION(ASSET_CLASS,ALLOC_EXPEN,
     +                                     INTRA_ACCOUNT_CLASSIFICATION,
     +                                     INTRA_EXPENSE_COLLECTION,
     +                                     MONTHLY_ALLOCATED_VALUES,
     +                                     TAX_TYPE,
     +                                     INTRA_ACCT_SUBCLASS)
                     IF(CASH_TYPE /= 'N') THEN
                        IF(INDEX(INTRA_EXPENSE_COLLECTION,'BTL')/=0)THEN
                           CALL CASH_ACCUMLATION('E',ASSET_CLASS,
     +                                        ALLOC_CASH_EXPEN,
     +                                        MONTHLY_ALLOCATED_CASH,
     +                                        INTRA_EXPENSE_COLLECTION)
                        ELSE
                           CALL CASH_ACCUMLATION('E',ASSET_CLASS,
     +                                     ALLOC_CASH_EXPEN,
     +                                     MONTHLY_ALLOCATED_CASH,
     +                                     INTRA_ACCOUNT_CLASSIFICATION)
                        ENDIF
                     ENDIF
                  ELSEIF(ACCOUNT_TYPE == 'P') THEN
                     CALL CASH_PAYMENT_ACCUMLATION('I',ASSET_CLASS,
     +                                          ALLOC_CASH_EXPEN,
     +                                          MONTHLY_ALLOCATED_CASH)
                  ELSEIF(ACCOUNT_TYPE == 'I') THEN
                     CALL CASH_PAYMENT_ACCUMLATION('P',ASSET_CLASS,
     +                                          ALLOC_CASH_EXPEN,
     +                                          MONTHLY_ALLOCATED_CASH)
                  ENDIF
C
                  CLASS_POINTER = CLASS_POINTER + 1
                  IF(CLASS_POINTER > AVAIL_DATA_YEARS) EXIT
                  IF(ASSET_CLASS_LIST(CLASS_POINTER) == 0. .OR.
     +                      ASSET_CLASS_LIST(CLASS_POINTER) == -99.)EXIT
               ENDDO
            ENDIF ! END INTEA-COMPANY STUFF
C
C WVPA TRACKER DATA BASE TRANSFER
C
            IF(WVPA() .AND. .NOT. SAVE_BASE_CASE) THEN
               VOID_LOGICAL = WVPA_TRACKER_DATABASE_INFO(DESC,
     +                                       WVPA_TRACKING_TYPE,
     +                                       FINANCIAL_SIMULATION_YEARS,
     +                                       MIDAS_MONTHLY_BOOKED)
            ENDIF
C
C ALLOCATE TO TOTAL COMPANY AND ASSET CLASSES
C
            VOID_LOGICAL=RETURN_ASSET_CLASS_LISTS(ASSET_CLASS_ID,
     +                                         ASSET_CLASS_LIST,
     +                                         ASSET_ALLOCATION_VECTOR,
     +                                         ASSET_ALLOCATION_LIST)
C
            CLASS_POINTER = 1
            DO
               ASSET_CLASS = ASSET_CLASS_LIST(CLASS_POINTER)
               CALL CHECK_IF_CLASS_DEFINED(ASSET_CLASS)
               ASSET_CLASS_ID = ASSET_CLASS + 1
               IF(ASSET_CLASS >= 0) ASSET_CLASS =
     +                               ASSET_CLASS_POINTER(ASSET_CLASS_ID)
               IF(ASSET_ALLOCATION_LIST(CLASS_POINTER) < 0.) THEN
                  MONTHLY_ALLOCATED_VALUES = 0.
                  MONTHLY_ALLOCATED_CASH = 0.
                  ALLOCATION_VECTOR =
     +                         ABS(ASSET_ALLOCATION_LIST(CLASS_POINTER))
                  CALL GET_ASSET_VAR(ALLOCATION_VECTOR,
     +                                      DUMMY_TYPE,ALLOCATION_VALUE)
                  ALLOC_EXPEN(0) = EXPENSES(0)*ALLOCATION_VALUE(1)/100.
C
                  IF(CASH_TYPE /= 'N') THEN ! .AND.
C    +               (ACCOUNT_TYPE == 'R' .OR. ACCOUNT_TYPE == 'E'))THEN
                     ALLOC_CASH_EXPEN(0) = MIDAS_ANNUAL_CASH(0) *
     +                                          ALLOCATION_VALUE(1)/100.
                  ENDIF
                  DO I = 1, FINANCIAL_SIMULATION_YEARS
                     IF(I <= AVAIL_DATA_YEARS)
     +                   ASSET_ALLOCATOR = ALLOCATION_VALUE(I)/100.
                     ALLOC_EXPEN(I) = EXPENSES(I) * ASSET_ALLOCATOR
                     IF(MONTHLY_MIDAS_ACTIVE .AND. (I >= 1 .AND.
     +                           I <= LAST_AVAILABLE_MONTHLY_YEAR)) THEN
                        DO MO = 1, 12
                           MONTHLY_ALLOCATED_VALUES(MO,I) =
     +                               ASSET_ALLOCATOR *
     +                                        MIDAS_MONTHLY_BOOKED(MO,I)
                        ENDDO
                     ENDIF
                     IF(CASH_TYPE /= 'N') THEN ! .AND.
C    +               (ACCOUNT_TYPE == 'R' .OR. ACCOUNT_TYPE == 'E'))THEN
                        ALLOC_CASH_EXPEN(I) = MIDAS_ANNUAL_CASH(I) *
     +                                                   ASSET_ALLOCATOR
                        IF(MONTHLY_MIDAS_ACTIVE .AND. (I >= 1 .AND.
     +                           I <= LAST_AVAILABLE_MONTHLY_YEAR)) THEN
                           DO MO = 1, 12
                              MONTHLY_ALLOCATED_CASH(MO,I) =
     +                        ASSET_ALLOCATOR * MIDAS_MONTHLY_CASH(MO,I)
                           ENDDO
                        ENDIF
                     ENDIF
                  ENDDO
               ELSE
                  ASSET_ALLOCATOR =
     +                         ASSET_ALLOCATION_LIST(CLASS_POINTER)/100.
C
                  DO I = 0, FINANCIAL_SIMULATION_YEARS
                     ALLOC_EXPEN(I) = EXPENSES(I) * ASSET_ALLOCATOR
                     IF(CASH_TYPE /= 'N') ! .AND.
C    +                   (ACCOUNT_TYPE == 'R' .OR. ACCOUNT_TYPE == 'E'))
     +                      ALLOC_CASH_EXPEN(I) = MIDAS_ANNUAL_CASH(I) *
     +                                                   ASSET_ALLOCATOR
                  ENDDO
                  IF(MONTHLY_MIDAS_ACTIVE) THEN
                     DO YR = 1, LAST_AVAILABLE_MONTHLY_YEAR
                        DO MO = 1, 12
                           MONTHLY_ALLOCATED_VALUES(MO,YR) =
     +                                 ASSET_ALLOCATOR*
     +                                       MIDAS_MONTHLY_BOOKED(MO,YR)
                           IF(CASH_TYPE /= 'N') ! .AND.
C    +                   (ACCOUNT_TYPE == 'R' .OR. ACCOUNT_TYPE == 'E'))
     +                        MONTHLY_ALLOCATED_CASH(MO,YR) =
     +                                      ASSET_ALLOCATOR *
     +                                         MIDAS_MONTHLY_CASH(MO,YR)
                        ENDDO
                     ENDDO
                  ELSE
                     MONTHLY_ALLOCATED_VALUES = 0.
                     MONTHLY_ALLOCATED_CASH = 0.
                  ENDIF
               ENDIF
               IF(ACCOUNT_TYPE == 'R') THEN
                  CALL REVENUE_ACCUMLATION(ASSET_CLASS,ALLOC_EXPEN,
     +                                  REVENUE_CLASSIFICATION,
     +                                  EXPENSE_cln_local,
     +                                  MONTHLY_ALLOCATED_VALUES,
     +                                  TAX_TYPE,
     +                                  ACCT_SUBCLASS)
                  IF(CASH_TYPE /= 'N') THEN
                     CALL CASH_ACCUMLATION(ACCOUNT_TYPE,ASSET_CLASS,
     +                                  ALLOC_CASH_EXPEN,
     +                                  MONTHLY_ALLOCATED_CASH,
     +                                  REVENUE_CLASSIFICATION)
                  ENDIF
               ELSEIF(ACCOUNT_TYPE == 'E') THEN
                  CALL EXPENSE_ACCUMLATION(ASSET_CLASS,ALLOC_EXPEN,
     +                                  EXPENSE_CLASSIFICATION,
     +                                  EXPENSE_cln_local,
     +                                  MONTHLY_ALLOCATED_VALUES,
     +                                  TAX_TYPE,
     +                                  ACCT_SUBCLASS)
                  IF(CASH_TYPE /= 'N') THEN
                     IF(INDEX(EXPENSE_cln_local,'BTL') /= 0) THEN
                        CALL CASH_ACCUMLATION(ACCOUNT_TYPE,ASSET_CLASS,
     +                                     ALLOC_CASH_EXPEN,
     +                                     MONTHLY_ALLOCATED_CASH,
     +                                     EXPENSE_cln_local)
                     ELSE
                        CALL CASH_ACCUMLATION(ACCOUNT_TYPE,ASSET_CLASS,
     +                                     ALLOC_CASH_EXPEN,
     +                                     MONTHLY_ALLOCATED_CASH,
     +                                     EXPENSE_CLASSIFICATION)

                        IF(INDEX(EXPENSE_cln_local,'PGA') /= 0) THEN
                           CALL CASH_ACCUMLATION('R',
     +                                     ASSET_CLASS,
     +                                     ALLOC_CASH_EXPEN,
     +                                     MONTHLY_ALLOCATED_CASH,
     +                                     'Adjustment Clause')
                        ELSEIF(INDEX(EXPENSE_cln_local,'Adj') /= 0 .OR.
     +                         INDEX(EXPENSE_cln_local,'Fue') /= 0)THEN
                           CALL CASH_ACCUMLATION('R',
     +                                     ASSET_CLASS,
     +                                     ALLOC_CASH_EXPEN,
     +                                     MONTHLY_ALLOCATED_CASH,
     +                                     'Adjustment Clause')
                        ENDIF
                     ENDIF
                  ENDIF
               ELSEIF(ACCOUNT_TYPE == 'P' .OR. ACCOUNT_TYPE == 'I') THEN
                  CALL CASH_PAYMENT_ACCUMLATION(ACCOUNT_TYPE,
     +                                          ASSET_CLASS,
     +                                          ALLOC_CASH_EXPEN,
     +                                          MONTHLY_ALLOCATED_CASH)
               ENDIF
               CLASS_POINTER = CLASS_POINTER + 1
               IF(CLASS_POINTER > AVAIL_DATA_YEARS) EXIT
               IF(ASSET_CLASS_LIST(CLASS_POINTER) == 0. .OR.
     +                      ASSET_CLASS_LIST(CLASS_POINTER) == -99) EXIT
            ENDDO
         ENDDO
C
         CALL CLOSE_EXPENSE_FILE
      ENDDO ! END OF FILES
      IF(LF95 .AND. TACCTS > 0) THEN
         WRITE(SCREEN_MESSAGES,'(I5,A)') TACCTS,
     +                                       "-Expense/Revenue Accounts"
         CALL MG_CLEAR_LINE_WRITE(17,70,73,TRIM(SCREEN_MESSAGES),
     +                                                   ALL_VERSIONS,1)
      ENDIF
C
C     SUM TOTAL COMPANY EXPENSES
C
      IF(ACTIVE_EXPENSE_FILES) THEN
         DO ASSET_CLASS = 1, NUM_OF_ASSET_CLASSES
            DO I = 1, FINANCIAL_SIMULATION_YEARS
               DO J = 10, LAST_EXPENSE_ITEM
                  DO TAX_TYPE = 1, 2
                     DO SubClass = 1, MaxSubClass
                        DO MO = 1, 12
                           EXPENSES_MONTHLY(MO,I,0,J,TAX_TYPE,SubClass)=
     +                      EXPENSES_MONTHLY(MO,I,0,J,TAX_TYPE,SubClass)
     +                      + EXPENSES_MONTHLY(MO,I,ASSET_CLASS,J,
     +                                                TAX_TYPE,SubClass)
                           EXPENSES_MONTHLY(0,I,0,J,TAX_TYPE,SubClass)=
     +                       EXPENSES_MONTHLY(0,I,0,J,TAX_TYPE,SubClass)
     +                       + EXPENSES_MONTHLY(MO,I,ASSET_CLASS,J,
     +                                                TAX_TYPE,SubClass)
C
C SUM SUB-CLASS INTO THE O CLASS
C
                           EXPENSES_MONTHLY(MO,I,ASSET_CLASS,J,
     +                                                     TAX_TYPE,0) =
     +                        EXPENSES_MONTHLY(MO,I,ASSET_CLASS,J,
     +                                                       TAX_TYPE,0)
     +                        + EXPENSES_MONTHLY(MO,I,ASSET_CLASS,J,
     +                                                TAX_TYPE,SubClass)
                           EXPENSES_MONTHLY(0,I,ASSET_CLASS,J,
     +                                                     TAX_TYPE,0) =
     +                        EXPENSES_MONTHLY(0,I,ASSET_CLASS,J,
     +                                                       TAX_TYPE,0)
     +                        + EXPENSES_MONTHLY(MO,I,ASSET_CLASS,J,
     +                                                TAX_TYPE,SubClass)
                        ENDDO
                     ENDDO
                  ENDDO
               ENDDO
C
C REVENUE SECTION
C
               DO J = 1, LAST_INCOME_LINE
                  DO TAX_TYPE = 1, 2
                     DO SubClass = 1, MaxSubClass
                        DO MO = 1, 12
                           REVENUES_MONTHLY(0,I,0,J,TAX_TYPE,SubClass) =
     +                       REVENUES_MONTHLY(0,I,0,J,TAX_TYPE,SubClass)
     +                       + REVENUES_MONTHLY(MO,I,ASSET_CLASS,J,
     +                                                TAX_TYPE,SubClass)
     +                       - REVENUES_MONTHLY(MO,I,-1,J,
     +                                                TAX_TYPE,SubClass)
C
C SUM SUB-CLASS INTO THE O CLASS
C
                           REVENUES_MONTHLY(MO,I,ASSET_CLASS,J,
     +                                                     TAX_TYPE,0) =
     +                       REVENUES_MONTHLY(MO,I,ASSET_CLASS,J,
     +                                                       TAX_TYPE,0)
     +                       + REVENUES_MONTHLY(MO,I,ASSET_CLASS,J,
     +                                                TAX_TYPE,SubClass)
                           REVENUES_MONTHLY(0,I,ASSET_CLASS,J,
     +                                                     TAX_TYPE,0) =
     +                       REVENUES_MONTHLY(0,I,ASSET_CLASS,J,
     +                                                       TAX_TYPE,0)
     +                       + REVENUES_MONTHLY(MO,I,ASSET_CLASS,J,
     +                                                TAX_TYPE,SubClass)
                        ENDDO
                     ENDDO
                  ENDDO
               ENDDO
               DO MO = 1, 12
                  SECONDARY_SALES_NOT_IN_RATES(0,I,0) =
     +                  SECONDARY_SALES_NOT_IN_RATES(MO,I,ASSET_CLASS) +
     +                  SECONDARY_SALES_NOT_IN_RATES(0,I,0) -
     +                  SECONDARY_SALES_NOT_IN_RATES(MO,I,-1)
                  OTHER_REVENUES_NOT_IN_RATES(0,I,0) =
     +                   OTHER_REVENUES_NOT_IN_RATES(0,I,0) +
     +                   OTHER_REVENUES_NOT_IN_RATES(MO,I,ASSET_CLASS) -
     +                   OTHER_REVENUES_NOT_IN_RATES(MO,I,-1)
               ENDDO
            ENDDO
         ENDDO
         DO I = 1,FINANCIAL_SIMULATION_YEARS
            DO J = 10, LAST_EXPENSE_ITEM
               DO TAX_TYPE = 1, 2
                  DO SubClass = 0, MaxSubClass
                     DO MO = 0, 12
                        EXPENSES_MONTHLY(MO,I,0,J,TAX_TYPE,SubClass) =
     +                   EXPENSES_MONTHLY(MO,I,0,J,TAX_TYPE,SubClass)
     +                   - EXPENSES_MONTHLY(MO,I,-1,J,TAX_TYPE,SubClass)
                     ENDDO
                  ENDDO
               ENDDO
            ENDDO
C
C REVENUE SECTION
C
            DO J = 1, LAST_INCOME_LINE
               DO TAX_TYPE = 1, 2
                  DO SubClass = 0, MaxSubClass
                     DO MO = 0, 12
                        REVENUES_MONTHLY(MO,I,0,J,TAX_TYPE,SubClass) =
     +                             REVENUES_MONTHLY(MO,I,0,J,
     +                                                TAX_TYPE,SubClass)
     +                             - REVENUES_MONTHLY(MO,I,-1,J,
     +                                                TAX_TYPE,SubClass)
                     ENDDO
                  ENDDO
               ENDDO
            ENDDO
C
            DO MO = 0, 12
               SECONDARY_SALES_NOT_IN_RATES(MO,I,0) =
     +                            SECONDARY_SALES_NOT_IN_RATES(MO,I,0) -
     +                            SECONDARY_SALES_NOT_IN_RATES(MO,I,-1)
               OTHER_REVENUES_NOT_IN_RATES(MO,I,0) =
     +                             OTHER_REVENUES_NOT_IN_RATES(MO,I,0) -
     +                             OTHER_REVENUES_NOT_IN_RATES(MO,I,-1)
            ENDDO
         ENDDO ! END OF RECORDS IN FILES
      ENDIF ! FILES ARE ACTIVE
      DEALLOCATE(READ_EXPEN,ALLOC_EXPEN,EXPENSES,OUTPUT_VALUE,
     +           ASSET_CLASS_LIST,ASSET_ALLOCATION_LIST,
     +           ALLOC_CASH_EXPEN,ACCOUNT_PAYABLE_VALUES,
     +           MIDAS_ANNUAL_CASH)
C
      IF(SAVE_BASE_CASE) THEN
         CALL OPEN_EXPENSE_BASE_CASE_FILE(10)
         WRITE(10) SECONDARY_SALES_NOT_IN_RATES,
     +             OTHER_REVENUES_NOT_IN_RATES,
     +             EXPENSES_MONTHLY,
     +             REVENUES_MONTHLY,
     +             CASH_MONTHLY,
     +             CASH_REV_EXP_MONTHLY,
     +             BUDGET_EXPENSE
         CLOSE(10)
      ENDIF
      RETURN
C***********************************************************************
      ENTRY EXPENSE_ACCUMLATION(R_ASSET_CLASS,
     +                                    R_ANNUAL_EXPENSES,
     +                                    R_EXPENSE_CLASSIFICATION,
     +                                    R_EXPENSE_COLLECTION,
     +                                    R_MONTHLY_EXPENSES,
     +                                    R_TAX_TYPE,
     +                                    R_ACCT_SUBCLASS)
C***********************************************************************
C
C
         ASSET_CLASS = R_ASSET_CLASS
C
         EXTYPE = INCOME_STATEMENT_POSITION(R_EXPENSE_CLASSIFICATION)
         SubClass = 0 ! ACCOUNT_SUB_CLASSIFICATION(R_ACCT_SUBCLASS)
         IF(INDEX(R_EXPENSE_COLLECTION,'BTL') /= 0) THEN
            IF(EXPENSE_TYPE_IS_CASH(EXTYPE)) EXTYPE = BTL Expenses
            IF(EXTYPE == Amortization) EXTYPE = BTL Amortization
         ENDIF
         IF(EXTYPE < 10 .OR. EXTYPE > LAST_EXPENSE_ITEM) RETURN
C
         IF(INDEX(R_EXPENSE_COLLECTION,'Adj') /= 0 .OR.
     +               INDEX(R_EXPENSE_COLLECTION,'Fue') /= 0 .OR.
     +                      INDEX(R_EXPENSE_COLLECTION,'PGA') /= 0) THEN
            CALL RETURN_OPREV_TAX_RATE(YR,ASSET_CLASS,OPREV_TAX_RATE)
            IF(INDEX(R_EXPENSE_CLASSIFICATION,'Gas') /= 0) THEN
               DO YR = 0, FINANCIAL_SIMULATION_YEARS
                  REVENUES_MONTHLY(0,YR,ASSET_CLASS,PGA Adjustment,
     +                                            R_TAX_TYPE,SubClass) =
     +                       R_ANNUAL_EXPENSES(YR)/(1.-OPREV_TAX_RATE)
     +                       + REVENUES_MONTHLY(0,YR,ASSET_CLASS,
     +                                           PGA Adjustment,
     +                                              R_TAX_TYPE,SubClass)
                  IF(YR>=1 .AND. YR<=LAST_AVAILABLE_MONTHLY_YEAR) THEN
                     DO MO = 1, 12
                        REVENUES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                      PGA Adjustment,
     +                                            R_TAX_TYPE,SubClass) =
     +                     R_MONTHLY_EXPENSES(MO,YR)/(1.-OPREV_TAX_RATE)
     +                     + REVENUES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                            PGA Adjustment,
     +                                              R_TAX_TYPE,SubClass)
                     ENDDO
                  ENDIF
               ENDDO
            ELSE
               DO YR = 0, FINANCIAL_SIMULATION_YEARS
                  REVENUES_MONTHLY(0,YR,ASSET_CLASS,adjustment_clause,
     +                                            R_TAX_TYPE,SubClass) =
     +                       R_ANNUAL_EXPENSES(YR)/(1.-OPREV_TAX_RATE)
     +                       + REVENUES_MONTHLY(0,YR,ASSET_CLASS,
     +                                     adjustment_clause,
     +                                              R_TAX_TYPE,SubClass)
                  IF(YR>=1 .AND. YR<=LAST_AVAILABLE_MONTHLY_YEAR) THEN
                     DO MO = 1, 12
                        REVENUES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                       adjustment_clause,
     +                                            R_TAX_TYPE,SubClass) =
     +                     R_MONTHLY_EXPENSES(MO,YR)/(1.-OPREV_TAX_RATE)
     +                     + REVENUES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                         adjustment_clause,
     +                                              R_TAX_TYPE,SubClass)
                     ENDDO
                  ENDIF
               ENDDO
            ENDIF
         ENDIF
C
         DO YR = 0, FINANCIAL_SIMULATION_YEARS
            EXPENSES_MONTHLY(0,YR,ASSET_CLASS,EXTYPE,
     +                                            R_TAX_TYPE,SubClass) =
     +                           R_ANNUAL_EXPENSES(YR)
     +                           + EXPENSES_MONTHLY(0,YR,ASSET_CLASS,
     +                                       EXTYPE,R_TAX_TYPE,SubClass)
            IF(YR >= 1 .AND. YR <= LAST_AVAILABLE_MONTHLY_YEAR) THEN
               DO MO = 1, 12
                  EXPENSES_MONTHLY(MO,YR,ASSET_CLASS,EXTYPE,
     +                                            R_TAX_TYPE,SubClass) =
     +                           R_MONTHLY_EXPENSES(MO,YR)
     +                           + EXPENSES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                       EXTYPE,R_TAX_TYPE,SubClass)
               ENDDO
            ENDIF
         ENDDO
      RETURN
C***********************************************************************
      ENTRY REVENUE_ACCUMLATION(R_ASSET_CLASS,
     +                          R_ANNUAL_REVENUES,
     +                          R_ACCOUNT_CLASSIFICATION,
     +                          R_EXPENSE_COLLECTION,
     +                          R_MONTHLY_REVENUES,
     +                          R_TAX_TYPE,
     +                          R_ACCT_SUBCLASS)
C***********************************************************************
C
         ASSET_CLASS = R_ASSET_CLASS
         REV_TYPE =
     +         INCOME_STATEMENT_POSITION(R_ACCOUNT_CLASSIFICATION)
         SubClass = 0 ! ACCOUNT_SUB_CLASSIFICATION(R_ACCT_SUBCLASS)
         IF(REV_TYPE >= 1 .AND. REV_TYPE <= LAST_INCOME_LINE) THEN
            DO YR = 0, FINANCIAL_SIMULATION_YEARS
               IF(YR >= 1 .AND. YR <= LAST_AVAILABLE_MONTHLY_YEAR) THEN
                  REVENUES_MONTHLY(0,YR,ASSET_CLASS,REV_TYPE,
     +                                            R_TAX_TYPE,SubClass) =
     +                           R_ANNUAL_REVENUES(YR)
     +                           + REVENUES_MONTHLY(0,YR,ASSET_CLASS,
     +                                     REV_TYPE,R_TAX_TYPE,SubClass)
                  DO MO = 1, 12
                     REVENUES_MONTHLY(MO,YR,ASSET_CLASS,REV_TYPE,
     +                                            R_TAX_TYPE,SubClass) =
     +                            R_MONTHLY_REVENUES(MO,YR)
     +                            + REVENUES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                     REV_TYPE,R_TAX_TYPE,SubClass)
                  ENDDO
               ELSE
                 REVENUES_MONTHLY(0,YR,ASSET_CLASS,REV_TYPE,
     +                                            R_TAX_TYPE,SubClass) =
     +                R_ANNUAL_REVENUES(YR)
     +                + REVENUES_MONTHLY(0,YR,ASSET_CLASS,REV_TYPE,
     +                                              R_TAX_TYPE,SubClass)
               ENDIF
            ENDDO
            IF(R_EXPENSE_COLLECTION(1:1) == 'N') THEN
               IF(REV_TYPE == 3) THEN

                  DO YR = 0, FINANCIAL_SIMULATION_YEARS
                     IF(YR >= 1 .AND.
     +                           YR <= LAST_AVAILABLE_MONTHLY_YEAR) THEN
                        SECONDARY_SALES_NOT_IN_RATES(0,YR,ASSET_CLASS) =
     +                           R_ANNUAL_REVENUES(YR) +
     +                              SECONDARY_SALES_NOT_IN_RATES(0,YR,
     +                                                      ASSET_CLASS)
                        DO MO = 1, 12
                           SECONDARY_SALES_NOT_IN_RATES(MO,YR,
     +                                                    ASSET_CLASS) =
     +                            R_MONTHLY_REVENUES(MO,YR) +
     +                              SECONDARY_SALES_NOT_IN_RATES(MO,YR,
     +                                                      ASSET_CLASS)
                        ENDDO
                     ELSE
                        SECONDARY_SALES_NOT_IN_RATES(0,YR,ASSET_CLASS) =
     +                           R_ANNUAL_REVENUES(YR) +
     +                              SECONDARY_SALES_NOT_IN_RATES(0,YR,
     +                                                      ASSET_CLASS)
                     ENDIF
                  ENDDO
               ELSEIF(REV_TYPE == 4) THEN
C        CASE ('Other Revenue')
                  DO YR = 0, FINANCIAL_SIMULATION_YEARS
                     IF(YR >= 1 .AND.
     +                           YR <= LAST_AVAILABLE_MONTHLY_YEAR) THEN
                        OTHER_REVENUES_NOT_IN_RATES(0,YR,ASSET_CLASS) =
     +                               R_ANNUAL_REVENUES(YR) +
     +                               OTHER_REVENUES_NOT_IN_RATES(0,YR,
     +                                                      ASSET_CLASS)
                        DO MO = 1, 12
                           OTHER_REVENUES_NOT_IN_RATES(MO,YR,
     +                                                    ASSET_CLASS) =
     +                               R_MONTHLY_REVENUES(MO,YR) +
     +                               OTHER_REVENUES_NOT_IN_RATES(MO,YR,
     +                                                      ASSET_CLASS)
                        ENDDO
                     ELSE
                        OTHER_REVENUES_NOT_IN_RATES(0,YR,ASSET_CLASS) =
     +                    R_ANNUAL_REVENUES(YR) +
     +                     OTHER_REVENUES_NOT_IN_RATES(0,YR,ASSET_CLASS)
                     ENDIF
                  ENDDO
               ENDIF
            ENDIF
         ENDIF
      RETURN
C***********************************************************************
      ENTRY CASH_PAYMENT_ACCUMLATION(R_ACCOUNT_TYPE,
     +                               R_ASSET_CLASS,
     +                               R_ANNUAL_CASH,
     +                               R_MONTHLY_CASH)
C***********************************************************************
C
C
         ASSET_CLASS = R_ASSET_CLASS
C
         CASH_POS = CASH_POSITION(R_ACCOUNT_TYPE)
         IF(CASH_POS < 1 .OR. CASH_POS > LAST_CASH_ITEM) RETURN
C
C NOTE: ONLY CASH RECEIPTS AND CASH PAYMENTS ARE USED ELSEWHERE IN 
C THE CODE
C IF REVENUE AND EXPENSE CASH IS USED THEN MODIFY CASH_POSITION TO 
C NOT INCLUDE
C NON-CASH ITEMS AND TAXES. MOVE CASH_POSITION HERE AND CHECK INCOME 
C POSITON.
C MSG 8/1/04

         DO YR = 0, FINANCIAL_SIMULATION_YEARS
            CASH_MONTHLY(0,YR,ASSET_CLASS,CASH_POS) =
     +                         R_ANNUAL_CASH(YR)
     +                         + CASH_MONTHLY(0,YR,ASSET_CLASS,CASH_POS)
            IF(YR >= 1 .AND. YR <= LAST_AVAILABLE_MONTHLY_YEAR) THEN
c             c DO MO = 1, 12
               CASH_MONTHLY(1:,YR,ASSET_CLASS,CASH_POS) =
     +                        R_MONTHLY_CASH(1:,YR)
     +                        + CASH_MONTHLY(1:,YR,ASSET_CLASS,CASH_POS)
c               ENDDO
            ENDIF
         ENDDO
      RETURN
C***********************************************************************
      ENTRY CASH_ACCUMLATION(R_ACCOUNT_TYPE,
     +                       R_ASSET_CLASS,
     +                       R_ANNUAL_CASH,
     +                       R_MONTHLY_CASH,
     +                       R_ACCOUNT_CLASSIFICATION)
C***********************************************************************
C
         CALL CASH_PAYMENT_ACCUMLATION(R_ACCOUNT_TYPE,
     +                                 R_ASSET_CLASS,
     +                                 R_ANNUAL_CASH,
     +                                 R_MONTHLY_CASH)
         ASSET_CLASS = R_ASSET_CLASS
C
         IF(R_ACCOUNT_TYPE == 'R') THEN
            CASH_POS =
     +               CASH_STATEMENT_POSITION(R_ACCOUNT_CLASSIFICATION)
         ELSE
            IF(INDEX(R_ACCOUNT_CLASSIFICATION,'BTL') /= 0) THEN
               CASH_POS = Cash BTL Expenses 
            ELSE
               CASH_POS =
     +                 CASH_STATEMENT_POSITION(R_ACCOUNT_CLASSIFICATION)
            ENDIF
         ENDIF
         IF(CASH_POS < 1 .OR. CASH_POS > CASH_VARS) RETURN
C
         DO YR = 0, FINANCIAL_SIMULATION_YEARS
            CASH_REV_EXP_MONTHLY(0,YR,ASSET_CLASS,CASH_POS) =
     +                   CASH_REV_EXP_MONTHLY(0,YR,ASSET_CLASS,CASH_POS)
     +                   + R_ANNUAL_CASH(YR)
            IF(YR >= 1 .AND. YR <= LAST_AVAILABLE_MONTHLY_YEAR) THEN
               DO MO = 1, 12
                  CASH_REV_EXP_MONTHLY(MO,YR,ASSET_CLASS,CASH_POS) =
     +                  CASH_REV_EXP_MONTHLY(MO,YR,ASSET_CLASS,CASH_POS)
     +                  + R_MONTHLY_CASH(MO,YR)
               ENDDO
            ENDIF
         ENDDO
      RETURN
C***********************************************************************
      ENTRY  SET_UP_EXPENSE_ARRAYS
C***********************************************************************
         CALL RETURN_INITIALIZATION_CLASSES(NUM_OF_ASSET_CLASSES,
     +                                      MAX_ASSET_CLASS_NUM)
         IF(ALLOCATED(ASSET_CLASS_POINTER))
     +                                   DEALLOCATE(ASSET_CLASS_POINTER)
         IF(MAX_ASSET_CLASS_NUM > 0) THEN
            ALLOCATE(ASSET_CLASS_POINTER(MAX_ASSET_CLASS_NUM))
            CALL RETURN_INITIALIZATION_POINTER(ASSET_CLASS_POINTER)
         ENDIF
C
         IF(ALLOCATED(EXPENSES_MONTHLY)) DEALLOCATE(EXPENSES_MONTHLY)
         ALLOCATE(EXPENSES_MONTHLY(0:12,0:FINANCIAL_SIMULATION_YEARS,
     +                            -1:NUM_OF_ASSET_CLASSES,
     +                             LAST_EXPENSE_ITEM,
     +                             1:3,  ! tax type
     +                             0:MaxSubClass)) ! sub-account
         EXPENSES_MONTHLY = 0.
C
C BUGET SECTION
C
         IF(ALLOCATED(BUDGET_EXPENSE)) DEALLOCATE(BUDGET_EXPENSE)
         ALLOCATE(BUDGET_EXPENSE(0:12,0:FINANCIAL_SIMULATION_YEARS,10))
         BUDGET_EXPENSE = 0.
C
C REVENUE SECTION
C
         IF(ALLOCATED(REVENUES_MONTHLY))
     +          DEALLOCATE(REVENUES_MONTHLY,
     +                     SECONDARY_SALES_NOT_IN_RATES,
     +                     OTHER_REVENUES_NOT_IN_RATES)
         ALLOCATE(REVENUES_MONTHLY(0:12,0:FINANCIAL_SIMULATION_YEARS,
     +                            -1:NUM_OF_ASSET_CLASSES,
     +                             1:LAST_INCOME_LINE,
     +                             1:3,  ! tax type
     +                             0:MaxSubClass)) ! sub-account
         ALLOCATE(SECONDARY_SALES_NOT_IN_RATES(0:12,
     +                                     0:FINANCIAL_SIMULATION_YEARS,
     +                                         -1:NUM_OF_ASSET_CLASSES))
         ALLOCATE(OTHER_REVENUES_NOT_IN_RATES(0:12,
     +                                     0:FINANCIAL_SIMULATION_YEARS,
     +                                         -1:NUM_OF_ASSET_CLASSES))
C
         REVENUES_MONTHLY = 0.
         SECONDARY_SALES_NOT_IN_RATES = 0.
         OTHER_REVENUES_NOT_IN_RATES  = 0.
C
C CASH SECTION
C
         IF(ALLOCATED(CASH_MONTHLY)) DEALLOCATE(CASH_MONTHLY)
         ALLOCATE(CASH_MONTHLY(0:12,
     +                         0:FINANCIAL_SIMULATION_YEARS,
     +                         -1:NUM_OF_ASSET_CLASSES,
     +                          1:LAST_CASH_ITEM))
         CASH_MONTHLY = 0.
         IF(ALLOCATED(CASH_REV_EXP_MONTHLY))
     +                                  DEALLOCATE(CASH_REV_EXP_MONTHLY)
         ALLOCATE(CASH_REV_EXP_MONTHLY(0:12,
     +                           0:FINANCIAL_SIMULATION_YEARS,
     +                          -1:NUM_OF_ASSET_CLASSES,
     +                           1:CASH_VARS))
         CASH_REV_EXP_MONTHLY = 0.
C
C SET TAX ITEMS
C
      RETURN
C***********************************************************************
      ENTRY READ_EXPENSE_BASE_CASE
C***********************************************************************
         CALL RESET_EXPENOL
         CALL SET_UP_EXPENSE_ARRAYS
         CALL OPEN_EXPENSE_BASE_CASE_FILE(10)
         READ(10) SECONDARY_SALES_NOT_IN_RATES,
     +            OTHER_REVENUES_NOT_IN_RATES,
     +            EXPENSES_MONTHLY,
     +            REVENUES_MONTHLY,
     +            CASH_MONTHLY,
     +            CASH_REV_EXP_MONTHLY,
     +            BUDGET_EXPENSE
         CLOSE(10)
      RETURN
C**********************************************************************
      ENTRY OUC_TAXES(R_YR,R_ASSET_CLASS,
     +                R_COUNTY_PAYMENT,
     +                R_SUPPLEMENTAL_TRANSFER,
     +                R_STATE_ELECTRIC_TAXES,
     +                R_PSC_TAX)
C**********************************************************************
C
C
         R_COUNTY_PAYMENT = 0.
         R_SUPPLEMENTAL_TRANSFER = 0.
         R_STATE_ELECTRIC_TAXES = 0.
         R_PSC_TAX = 0.
      RETURN
C***********************************************************************
c     ENTRY EXPENSE_REVENUE_INFO(R_YR,R_PERIOD,R_CLASS,R_CLASS_EXISTS,
      ENTRY EXPENSE_REVENUE_INFO(R_YR,R_CLASS,R_CLASS_EXISTS,R_VARIABLE,
     +                           R_FUEXP,R_PREXP,R_OPEXP,
     +                           R_MNEXP,R_OTHER1,R_OTHER2,
     +                           R_OTHER3,R_NFOWN,R_NFLEASE,
     +                           R_DSM_EXPENSE,
     +                           R_DSM_REBATE,
     +                           R_ADJUSTMENT_CLAUSE_REVENUES,
     +                           R_BASE_RATES_REVENUES,
     +                           R_SECONDARY_SALES_REVENUES,
     +                           R_SECONDARY_SALES_NOT_IN_RATES,
     +                           R_OTHER_REVENUES,
     +                           R_BTL_REVENUES,
     +                           R_BTL_EXPENSE,
     +                           R_ATL_LEASE_EXP,
     +                           R_BTL_LEASE_EXP,
     +                           R_SERVICE_TRANSACTIONS,
     +                           R_EMISSION_CREDITS,
     +                           R_DOE_DISPOSAL,
     +                           R_DOE_DECOMMISSIONING,
     +                           R_CATAWBA_REVENUES,
     +                           R_CATAWBA_EXPENSES,
     +                           R_GAS_REVENUES,
     +                           R_TRANSMISSION_OPERATION,
     +                           R_TRANSMISSION_MAINTENANCE,
     +                           R_DISTRIBUTION_OPERATION,
     +                           R_DISTRIBUTION_MAINTENANCE,
     +                           R_CUSTOMER_ACCOUNTS,
     +                           R_CUSTOMER_SERVICES,
     +                           R_SALES_EXPENSE,
     +                           R_AG_OPERATIONS,
     +                           R_AG_MAINTENANCE,
     +                           R_UNBILLED_REVENUES,
     +                           R_ATL_DEFERRED_REVENUES,
     +                           R_RELATIONSHIP_REVENUES,
     +                           R_RESIDENTIAL_REVENUES,
     +                           R_COMMERCIAL_REVENUES,
     +                           R_INDUSTRIAL_REVENUES,
     +                           R_LIGHTING_REVENUES,
     +                           R_BULK_POWER_REVENUES,
     +                           R_AMORTIZATION_ADJUSTMENT,
     +                           R_DEFERRED_REVENUE_AMORTIZATION,
     +                           R_ATL_LEASE_AMORTIZATION,
     +                           R_BTL_LEASE_AMORTIZATION,
     +                           R_BOOK_DEPRECIATION,
     +                           R_NET_OF_TAX_BTL_REVENUES,
     +                           R_CAPACITY_REVENUES,
     +                           R_GOVERNMENT_REVENUES,
     +                           R_BTL_AMORTIZATION_EXP,
     +                           R_CHANGE_IN_RECEIVABLES,
     +                           R_CHANGE_IN_PAYABLES,
     +                           R_ATL_LEASE_INTEREST,
     +                           R_BTL_LEASE_INTEREST,
     +                           R_BTL_NUC_FUEL_DECOM_LIABILITY,
     +                           R_DEFERRED_FUEL_EXPENSE,
     +                           R_VACATION_PAY,
     +                           R_PENSION_EXPENSE,
     +                           R_STORM_EXPENSE,
     +                           R_CLASS_GAS_ADJ_CLAUSE_REVENUE,
     +                           R_STD_INTEREST,
     +                           R_LTD_INTEREST,
     +                           R_WVPA_MEMBER_ACCRUED_REVENUES,
     +                           WVPA_NON_MEMBER_COST_OF_POWER,
     +                           wvpa_member_cost_of_power_merc,
     +                           WVPA_MEMBER_COST_OF_SERVICES,
     +                           WVPA_NON_MEMBER_COST_OF_SERVICES,
     +                           R_REGULATED_REVENUES_13,
     +                           R_NON_REG_REVENUES_10,
     +                           R_Exp_PreferredDividends,
     +                           R_AFDCBorrowed,
     +                           R_AFDCEquity)
C***********************************************************************
C
         R_CLASS_EXISTS = .FALSE.
C
         R_BTL_REVENUES = 0.
         R_BTL_EXPENSE = 0.
         R_ATL_LEASE_EXP = 0.
         R_BTL_LEASE_EXP = 0.
         R_OTHER_REVENUES = 0.
         R_CATAWBA_REVENUES = 0.
         R_CATAWBA_EXPENSES = 0.
         R_ADJUSTMENT_CLAUSE_REVENUES = 0.
         R_BASE_RATES_REVENUES = 0.
C
         R_FUEXP = 0.
         R_PREXP = 0.
         R_OPEXP = 0.
         R_MNEXP = 0.
         R_OTHER1 = 0.
         R_OTHER2 = 0.
         R_OTHER3 = 0.
         R_NFOWN = 0.
         R_NFLEASE = 0.
         R_DSM_EXPENSE = 0.
         R_DSM_REBATE = 0.
         R_SECONDARY_SALES_REVENUES = 0.
         R_CAPACITY_REVENUES = 0.
         R_SECONDARY_SALES_NOT_IN_RATES = 0.
         R_SERVICE_TRANSACTIONS = 0.
         R_EMISSION_CREDITS = 0.
         R_DOE_DISPOSAL = 0.
         R_DOE_DECOMMISSIONING = 0.
         R_GAS_REVENUES = 0.
         R_TRANSMISSION_OPERATION = 0.
         R_TRANSMISSION_MAINTENANCE = 0.
         R_DISTRIBUTION_OPERATION = 0.
         R_DISTRIBUTION_MAINTENANCE = 0.
         R_CUSTOMER_ACCOUNTS = 0.
         R_CUSTOMER_SERVICES = 0.
         R_SALES_EXPENSE = 0.
         R_AG_OPERATIONS = 0.
         R_AG_MAINTENANCE = 0.
         R_UNBILLED_REVENUES = 0.
         R_DEFERRED_REVENUES = 0.
         R_RELATIONSHIP_REVENUES = 0.
         R_ATL_DEFERRED_REVENUES = 0.
         R_RELATIONSHIP_REVENUES = 0.
         R_RESIDENTIAL_REVENUES = 0.
         R_COMMERCIAL_REVENUES = 0.
         R_INDUSTRIAL_REVENUES = 0.
         R_LIGHTING_REVENUES = 0.
         R_GOVERNMENT_REVENUES = 0.
         R_BULK_POWER_REVENUES = 0.
         R_BOOK_DEPRECIATION = 0.
         R_NET_OF_TAX_BTL_REVENUES = 0.
         R_BTL_AMORTIZATION_EXP = 0.
         R_CHANGE_IN_RECEIVABLES = 0.
         R_CHANGE_IN_PAYABLES = 0.
         R_ATL_LEASE_INTEREST = 0.
         R_BTL_LEASE_INTEREST = 0.
         R_BTL_NUC_FUEL_DECOM_LIABILITY = 0.
         R_DEFERRED_FUEL_EXPENSE = 0.
         R_VACATION_PAY = 0.
         R_PENSION_EXPENSE = 0.
         R_STORM_EXPENSE = 0.
         R_CLASS_GAS_ADJ_CLAUSE_REVENUE = 0.
         R_STD_INTEREST = 0.
         R_LTD_INTEREST = 0.
         R_WVPA_MEMBER_ACCRUED_REVENUES = 0.
         WVPA_NON_MEMBER_COST_OF_POWER = 0.
         wvpa_member_cost_of_power_merc = 0.
         WVPA_MEMBER_COST_OF_SERVICES = 0.
         WVPA_NON_MEMBER_COST_OF_SERVICES = 0.
         R_REGULATED_REVENUES_13 = 0.
         R_NON_REG_REVENUES_10 = 0.
         R_VARIABLE(800:900) = 0.
         R_VARIABLE(786) = 0.
         R_Exp_PreferredDividends = 0.
         R_AFDCEquity = 0.
         R_AFDCBorrowed = 0.
C
         IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
            IF(R_CLASS == 0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN
               R_CLASS_EXISTS = .TRUE.
C
               YR = R_YR - 1
               PERIOD = 0 ! R_PERIOD
               SubClass = 0
               IF(YR > LAST_AVAILABLE_MONTHLY_YEAR) PERIOD = 0
               R_SECONDARY_SALES_NOT_IN_RATES =
     +               SECONDARY_SALES_NOT_IN_RATES(PERIOD,YR,ASSET_CLASS)
               DO TAX_TYPE = 1, 2
                  R_BASE_RATES_REVENUES = R_BASE_RATES_REVENUES +
     +                           REVENUES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                            Base Rates,
     +                                                TAX_TYPE,SubClass)
                  R_ADJUSTMENT_CLAUSE_REVENUES =
     +                      R_ADJUSTMENT_CLAUSE_REVENUES +
     +                           REVENUES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                                adjustment_clause,
     +                                                TAX_TYPE,SubClass)
                  R_CLASS_GAS_ADJ_CLAUSE_REVENUE =
     +                         R_CLASS_GAS_ADJ_CLAUSE_REVENUE
     +                         + REVENUES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                                   PGA Adjustment,
     +                                                TAX_TYPE,SubClass)
                  R_SECONDARY_SALES_REVENUES=R_SECONDARY_SALES_REVENUES+
     +                           REVENUES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                                secondary_sales,
     +                                                TAX_TYPE,SubClass)
                  R_OTHER_REVENUES = R_OTHER_REVENUES +
     +                           REVENUES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                                Other Revenue,
     +                                                TAX_TYPE,SubClass)
                  R_BTL_REVENUES = R_BTL_REVENUES +
     +                           REVENUES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                                BTL Revenues,
     +                                                TAX_TYPE,SubClass)
                  R_CATAWBA_REVENUES = R_CATAWBA_REVENUES +
     +                           REVENUES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                                Catawba Revenues,
     +                                                TAX_TYPE,SubClass)
                  R_GAS_REVENUES = R_GAS_REVENUES +
     +                           REVENUES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                                Gas Revenues,
     +                                                TAX_TYPE,SubClass)
                  R_CAPACITY_REVENUES = R_CAPACITY_REVENUES +
     +                           REVENUES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                                Capacity Sales,
     +                                                TAX_TYPE,SubClass)
C
                  IF(WVPA()) THEN
                     R_WVPA_MEMBER_ACCRUED_REVENUES =
     +                         R_WVPA_MEMBER_ACCRUED_REVENUES
     +                         + REVENUES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                                Unbilled Revenues,
     +                                                TAX_TYPE,SubClass)
                  ELSE
                     R_UNBILLED_REVENUES = R_UNBILLED_REVENUES +
     +                           REVENUES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                                Unbilled Revenues,
     +                                                TAX_TYPE,SubClass)
                  ENDIF
                  R_DEFERRED_REVENUES = R_DEFERRED_REVENUES +
     +                           REVENUES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                                Deferred Revenues,
     +                                                TAX_TYPE,SubClass)
                  R_RELATIONSHIP_REVENUES = R_RELATIONSHIP_REVENUES +
     +                           REVENUES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                            Relationship Revenues,
     +                                                TAX_TYPE,SubClass)
                  R_ATL_DEFERRED_REVENUES = R_ATL_DEFERRED_REVENUES +
     +                           REVENUES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                                Deferred Revenues,
     +                                                TAX_TYPE,SubClass)
                  R_RESIDENTIAL_REVENUES = R_RESIDENTIAL_REVENUES +
     +                           REVENUES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                                      Residential,
     +                                                TAX_TYPE,SubClass)
                  R_COMMERCIAL_REVENUES = R_COMMERCIAL_REVENUES +
     +                           REVENUES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                                Commercial,
     +                                                TAX_TYPE,SubClass)
                  R_INDUSTRIAL_REVENUES = R_INDUSTRIAL_REVENUES +
     +                           REVENUES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                                Industrial,
     +                                                TAX_TYPE,SubClass)
                  R_LIGHTING_REVENUES = R_LIGHTING_REVENUES +
     +                           REVENUES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                                Lighting,
     +                                                TAX_TYPE,SubClass)
                  R_GOVERNMENT_REVENUES = R_GOVERNMENT_REVENUES +
     +                           REVENUES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                                       Government,
     +                                                TAX_TYPE,SubClass)
                  R_BULK_POWER_REVENUES = R_BULK_POWER_REVENUES +
     +                           REVENUES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                                Bulk Power,
     +                                                TAX_TYPE,SubClass)
                  R_NET_OF_TAX_BTL_REVENUES = R_NET_OF_TAX_BTL_REVENUES+
     +                           REVENUES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                          Net of Tax BTL Revenues,
     +                                                TAX_TYPE,SubClass)
                  R_VARIABLE(786) = R_VARIABLE(786)
     +                         + REVENUES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                              MarkToMarket,
     +                                              TAX_TYPE,SubClass)
                  R_VARIABLE(787) = R_VARIABLE(787)
     +                         + REVENUES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                              PayrollTaxes,
     +                                              TAX_TYPE,SubClass)
                  R_AFDCEquity = R_AFDCEquity
     +                         + REVENUES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                              AFUDCEquity,
     +                                              TAX_TYPE,SubClass)
                  R_AFDCBorrowed = R_AFDCBorrowed
     +                         + REVENUES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                              AFUDCBorrowed,
     +                                              TAX_TYPE,SubClass)
C
C EXPENSE LINES
C
                  R_BTL_EXPENSE = R_BTL_EXPENSE +
     +                         EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +  BTL Expenses, ! = 28,
     +                                                TAX_TYPE,SubClass)

                  R_BTL_LEASE_EXP = R_BTL_LEASE_EXP +
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     + BTL Lease Cash, ! = 10,
     +                                                TAX_TYPE,SubClass)

                  R_FUEXP = R_FUEXP +
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     + Fossil Fuel, ! = 11,
     +                                                TAX_TYPE,SubClass)
                  R_PREXP = R_PREXP +
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     + Purchased Power, ! = 12,
     +                                                TAX_TYPE,SubClass)
                  IF(WVPA()) THEN
                     WVPA_NON_MEMBER_COST_OF_POWER =
     +                         WVPA_NON_MEMBER_COST_OF_POWER
     +                         + EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                        WVPA Non Member Purchases,
     +                                                TAX_TYPE,SubClass)
                     wvpa_member_cost_of_power_merc =
     +                         wvpa_member_cost_of_power_merc
     +             + EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                               WVPA Member Purchases ,
     +                                      TAX_TYPE,SubClass)
                     WVPA_MEMBER_COST_OF_SERVICES =
     +                         WVPA_MEMBER_COST_OF_SERVICES
     +                         + EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                            WVPA Member Services,
     +                                                TAX_TYPE,SubClass)
                     WVPA_NON_MEMBER_COST_OF_SERVICES =
     +                         WVPA_NON_MEMBER_COST_OF_SERVICES
     +                         + EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                         WVPA Non Member Services,
     +                                                TAX_TYPE,SubClass)
                  ENDIF
                  R_OPEXP = R_OPEXP +
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     + Variable OandM, ! = 13,
     +                                                TAX_TYPE,SubClass)
                  R_MNEXP = R_MNEXP +
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +   Fixed OandM, ! = 14,
     +                                                TAX_TYPE,SubClass)
                  R_OTHER1 = R_OTHER1 +
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +   Other OandM, ! = 15,
     +                                                TAX_TYPE,SubClass)
                  R_OTHER2 = R_OTHER2 +
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +   Purchased Gas, ! = 16,
     +                                                TAX_TYPE,SubClass)
                  R_OTHER3 = R_OTHER3 +
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     + Other, ! = 17,
     +                                                TAX_TYPE,SubClass)
                  R_NFOWN = R_NFOWN +
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     + Owned Nuclear Fuel, ! = 18,
     +                                                TAX_TYPE,SubClass)
                  R_NFLEASE = R_NFLEASE +
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     + Leased Nuclear Fuel, ! = 19,
     +                                                TAX_TYPE,SubClass)
                  R_DSM_EXPENSE = R_DSM_EXPENSE +
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     + DSM Expense, ! = 20,
     +                                                TAX_TYPE,SubClass)
                  R_DSM_REBATE = R_DSM_REBATE +
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     + DSM Rebate, ! = 21,
     +                                                TAX_TYPE,SubClass)
                  R_ATL_LEASE_EXP = R_ATL_LEASE_EXP +
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     + ATL Book Lease Expense, ! = 22,
     +                                                TAX_TYPE,SubClass)
                  R_SERVICE_TRANSACTIONS = R_SERVICE_TRANSACTIONS +
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     + Service Transactions, ! = 23,
     +                                                TAX_TYPE,SubClass)
                  R_EMISSION_CREDITS = R_EMISSION_CREDITS +
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     + Emission Credits, ! = 24,
     +                                                TAX_TYPE,SubClass)
                  R_DOE_DECOMMISSIONING = R_DOE_DECOMMISSIONING +
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     + DOE Decommissioning, ! = 25,
     +                                                TAX_TYPE,SubClass)
                  R_BTL_NUC_FUEL_DECOM_LIABILITY =
     +                       R_BTL_NUC_FUEL_DECOM_LIABILITY +
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     + BTL Decommissioning Liability,   ! = 8
     + TAX_TYPE,SubClass)
                  R_DOE_DISPOSAL = R_DOE_DISPOSAL +
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     + DOE Disposal, ! = 26,
     +                                                TAX_TYPE,SubClass)
                  R_CATAWBA_EXPENSES = R_CATAWBA_EXPENSES +
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     + Catawba Expenses, ! = 27,
     +                                                TAX_TYPE,SubClass)
                  R_TRANSMISSION_OPERATION = R_TRANSMISSION_OPERATION +
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                           Transmission Operation,
     +                                                TAX_TYPE,SubClass)
                  R_TRANSMISSION_MAINTENANCE =
     +                          R_TRANSMISSION_MAINTENANCE +
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                         Transmission Maintenance,
     +                                                TAX_TYPE,SubClass)
                  R_DISTRIBUTION_OPERATION = R_DISTRIBUTION_OPERATION +
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                           Distribution Operation,
     +                                                TAX_TYPE,SubClass)
                  R_DISTRIBUTION_MAINTENANCE =
     +                          R_DISTRIBUTION_MAINTENANCE +
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                         Distribution Maintenance,
     +                                                TAX_TYPE,SubClass)
                  R_CUSTOMER_ACCOUNTS = R_CUSTOMER_ACCOUNTS +
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                                Customer Accounts,
     +                                                TAX_TYPE,SubClass)
                  R_CUSTOMER_SERVICES = R_CUSTOMER_SERVICES +
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                                Customer Services,
     +                                                TAX_TYPE,SubClass)
                  R_SALES_EXPENSE =  R_SALES_EXPENSE +
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                                    Sales Expense,
     +                                                TAX_TYPE,SubClass)
                  R_AG_OPERATIONS = R_AG_OPERATIONS +
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                                    AG Operations,
     +                                                TAX_TYPE,SubClass)
                  R_AG_MAINTENANCE = R_AG_MAINTENANCE +
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                                   AG Maintenance,
     +                                                TAX_TYPE,SubClass)
                  R_AMORTIZATION_ADJUSTMENT = R_AMORTIZATION_ADJUSTMENT+
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                                    Amortization,
     +                                                TAX_TYPE,SubClass)
                  R_BTL_AMORTIZATION_EXP = R_BTL_AMORTIZATION_EXP +
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                                 BTL Amortization,
     +                                                TAX_TYPE,SubClass)
                  R_DEFERRED_REVENUE_AMORTIZATION =
     +                                 R_DEFERRED_REVENUE_AMORTIZATION +
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                    Deferred Revenue Amortization,
     +                                                TAX_TYPE,SubClass)
                  R_ATL_LEASE_AMORTIZATION = R_ATL_LEASE_AMORTIZATION +
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                           ATL Lease Amortization,
     +                                                TAX_TYPE,SubClass)
                  R_BTL_LEASE_AMORTIZATION = R_BTL_LEASE_AMORTIZATION +
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                           BTL Lease Amortization,
     +                                                TAX_TYPE,SubClass)
                  R_BOOK_DEPRECIATION = R_BOOK_DEPRECIATION +
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                                Book Depreciation,
     +                                                TAX_TYPE,SubClass)
                  R_ATL_LEASE_INTEREST = R_ATL_LEASE_INTEREST +
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                               ATL Lease Interest,
     +                                                TAX_TYPE,SubClass)
                  R_BTL_LEASE_INTEREST = R_BTL_LEASE_INTEREST +
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                               BTL Lease Interest,
     +                                                TAX_TYPE,SubClass)
                  R_DEFERRED_FUEL_EXPENSE = R_DEFERRED_FUEL_EXPENSE +
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                                    Deferred Fuel,
     +                                                TAX_TYPE,SubClass)
                  R_VACATION_PAY = R_VACATION_PAY +
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                                     Vacation Pay,
     +                                                TAX_TYPE,SubClass)
                  R_PENSION_EXPENSE = R_PENSION_EXPENSE +
     +                         EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                                  Pension Expense,
     +                                                TAX_TYPE,SubClass)
     +                         + EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                         Unfunded Pension Expense,
     +                                                TAX_TYPE,SubClass)
                  R_STORM_EXPENSE = R_STORM_EXPENSE +
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                                    STORM Expense,
     +                                                TAX_TYPE,SubClass)
                  R_STD_INTEREST = R_STD_INTEREST
     +                         + EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                                     STD Interest,
     +                                                TAX_TYPE,SubClass)

                  R_LTD_INTEREST = R_LTD_INTEREST
     +                         + EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                                     LTD Interest,
     +                                                TAX_TYPE,SubClass)
                  R_Exp_PreferredDividends = R_Exp_PreferredDividends
     +                         + EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                                     LTD Interest,
     +                                                TAX_TYPE,SubClass)
               ENDDO
C
C ProSym Interface 7/9/04
C
               PROSYM_BOOK_REVENUES = 0.
               IF(.NOT. WVPA())
     +               CALL RETURN_ProSymAnnualRevenues(YR,ASSET_CLASS,
     +                                       PROSYM_BOOK_REVENUES,
     +                                       R_SECONDARY_SALES_REVENUES,
     +                                       R_CAPACITY_REVENUES,
     +                                       R_OTHER_REVENUES,
     +                                       R_RELATIONSHIP_REVENUES)
C
C BOOK RECEIPTS AND PAYMENTS
C
               TOTAL_BOOK_REVENUES = PROSYM_BOOK_REVENUES
               MO = 0
               DO DATA_POS = Base Rates, Government
                  IF(DATA_POS == Unbilled Revenues) CYCLE
                  IF(DATA_POS == Deferred Revenues) CYCLE
                  DO TAX_TYPE = 1, 2
                     TOTAL_BOOK_REVENUES = TOTAL_BOOK_REVENUES
     +                          + REVENUES_MONTHLY(0,YR,ASSET_CLASS,
     +                                       DATA_POS,TAX_TYPE,SubClass)
                  ENDDO
               ENDDO
               DO TAX_TYPE = 1, 2
                  TOTAL_BOOK_REVENUES = TOTAL_BOOK_REVENUES
     +                   + REVENUES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                Derivative Financial Rev Variable,
     +                                                TAX_TYPE,SubClass)
     +                   + REVENUES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                   Derivative Financial Rev Fixed,
     +                                                TAX_TYPE,SubClass)
     +                   + REVENUES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                    Derivative Physical Rev Fixed,
     +                                                TAX_TYPE,SubClass)
     +                   + REVENUES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                 Derivative Physical Rev Variable,
     +                                                TAX_TYPE,SubClass)
               ENDDO
C
C PROSYM INTERFACE
C
               PROSYM_BOOK_EXPENSES = 0.
               IF(.NOT. WVPA())
     +               CALL RETURN_ProSymAnnualExpenses(YR,ASSET_CLASS,
     +                                             PROSYM_BOOK_EXPENSES,
     +                                             R_FUEXP,
     +                                             R_MNEXP,
     +                                             R_OPEXP,
     +                                             R_EMISSION_CREDITS)
               TOTAL_BOOK_EXPENSES = PROSYM_BOOK_EXPENSES
               DO DATA_POS = BTL Lease Cash, AG Maintenance
                  IF(DATA_POS == Owned Nuclear Fuel) CYCLE
                  IF(DATA_POS == DOE Decommissioning) CYCLE
                  DO TAX_TYPE = 1, 2
                     TOTAL_BOOK_EXPENSES = TOTAL_BOOK_EXPENSES
     +                           + EXPENSES_MONTHLY(0,YR,ASSET_CLASS,
     +                                       DATA_POS,TAX_TYPE,SubClass)
                  ENDDO
               ENDDO
               DO TAX_TYPE = 1, 2
                  TOTAL_BOOK_EXPENSES = TOTAL_BOOK_EXPENSES
     +                  + EXPENSES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                   Derivative Financial Exp Fixed,
     +                                                TAX_TYPE,SubClass)
     +                  + EXPENSES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                Derivative Financial Exp Variable,
     +                                                TAX_TYPE,SubClass)
     +                  + EXPENSES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                    Derivative Physical Exp Fixed,
     +                                                TAX_TYPE,SubClass)
     +                  + EXPENSES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                 Derivative Physical Exp Variable,
     +                                                TAX_TYPE,SubClass)
                  IF(WVPA()) THEN
                     TOTAL_BOOK_EXPENSES = TOTAL_BOOK_EXPENSES
     +                           + EXPENSES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                            WVPA Member Purchases,
     +                                                TAX_TYPE,SubClass)
     +                           + EXPENSES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                        WVPA Non Member Purchases,
     +                                                TAX_TYPE,SubClass)
     +                           + EXPENSES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                             WVPA Member Services,
     +                                                TAX_TYPE,SubClass)
     +                           + EXPENSES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                         WVPA Non Member Services,
     +                                                TAX_TYPE,SubClass)
                  ENDIF
               ENDDO
C
C CASH RECEITS AND PAYMENTS
C
               TOTAL_CASH_RECEPITS = PROSYM_BOOK_REVENUES ! 0.
               DO DATA_POS = Cash Base Rates, Cash Government
                  IF(DATA_POS == Cash Unbilled Revenues) CYCLE
                  IF(DATA_POS == Cash Deferred Revenues) CYCLE
                  TOTAL_CASH_RECEPITS = TOTAL_CASH_RECEPITS
     +                 + CASH_REV_EXP_MONTHLY(0,YR,ASSET_CLASS,DATA_POS)
               ENDDO
               TOTAL_CASH_RECEPITS = TOTAL_CASH_RECEPITS
     +  + CASH_MONTHLY(MO,YR,ASSET_CLASS,Cash Receipts) ! 3
     + + CASH_REV_EXP_MONTHLY(MO,YR,ASSET_CLASS,
     +                  Derivative Physical Rev Fixed)
     + + CASH_REV_EXP_MONTHLY(MO,YR,ASSET_CLASS,
     +               Derivative Physical Rev Variable)
     + + CASH_REV_EXP_MONTHLY(MO,YR,ASSET_CLASS,
     +                 Derivative Financial Rev Fixed)
     + + CASH_REV_EXP_MONTHLY(MO,YR,ASSET_CLASS,
     +              Derivative Financial Rev Variable)
C
               TOTAL_CASH_PAYMENTS = PROSYM_BOOK_EXPENSES ! 0.
               DO DATA_POS = Cash BTL Lease Cash, Cash AG Maintenance
                  TOTAL_CASH_PAYMENTS = TOTAL_CASH_PAYMENTS
     +                 + CASH_REV_EXP_MONTHLY(0,YR,ASSET_CLASS,DATA_POS)
               ENDDO
               TOTAL_CASH_PAYMENTS = TOTAL_CASH_PAYMENTS
     + + CASH_MONTHLY(MO,YR,ASSET_CLASS,Cash Payments) ! 4
     + + CASH_REV_EXP_MONTHLY(MO,YR,ASSET_CLASS,
     +                Cash Derative Financial Exp Var)
     + + CASH_REV_EXP_MONTHLY(MO,YR,ASSET_CLASS,
     +              Cash Derative Financial Exp Fixed)
     + + CASH_REV_EXP_MONTHLY(MO,YR,ASSET_CLASS,
     +               Cash Derative Physical Exp Fixed)
     + + CASH_REV_EXP_MONTHLY(MO,YR,ASSET_CLASS,
     +                 Cash Derative Physical Exp Var)
               IF(WVPA()) THEN
                  TOTAL_CASH_PAYMENTS = TOTAL_CASH_PAYMENTS
     +                   + CASH_REV_EXP_MONTHLY(MO,YR,ASSET_CLASS,
     +                                       Cash WVPA Member Purchases)
     +                   + CASH_REV_EXP_MONTHLY(MO,YR,ASSET_CLASS,
     +                                   Cash WVPA Non Member Purchases)
     +                   + CASH_REV_EXP_MONTHLY(MO,YR,ASSET_CLASS,
     +                                        Cash WVPA Member Services)
     +                   + CASH_REV_EXP_MONTHLY(MO,YR,ASSET_CLASS,
     +                                    Cash WVPA Non Member Services)
               ENDIF
C
               R_CHANGE_IN_RECEIVABLES = TOTAL_BOOK_REVENUES
     +                                   - TOTAL_CASH_RECEPITS
               R_CHANGE_IN_PAYABLES =  TOTAL_BOOK_EXPENSES
     +                                 - TOTAL_CASH_PAYMENTS
            ENDIF
         ENDIF
      RETURN
C***********************************************************************
      ENTRY FE_EXPENSE_SUB_CLASSES(R_YR,R_CLASS,R_VARIABLE)
C***********************************************************************
         IF(MaxSubClass < 6) RETURN
         IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
            IF(R_CLASS == 0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN
C
               YR = R_YR - 1
               PERIOD = 0 ! R_PERIOD
               IF(YR > LAST_AVAILABLE_MONTHLY_YEAR) PERIOD = 0
               DO TAX_TYPE = 1, 2

                  R_VARIABLE(511) = R_VARIABLE(511)
     +                        + EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     + Fossil Fuel, ! = 11,
     +  TAX_TYPE,1) ! COAL
                  R_VARIABLE(512) = R_VARIABLE(512)
     ++ EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                              Fossil Fuel, ! = 11,
     +                               TAX_TYPE,2) ! GAS
                  R_VARIABLE(513) = R_VARIABLE(513)
     +                        + EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     + Fossil Fuel, ! = 11,
     +  TAX_TYPE,4) ! STEAM
                  R_VARIABLE(514) = R_VARIABLE(514)
     +                        + EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +  Fossil Fuel, ! = 11,
     +   TAX_TYPE,5) ! EAS NET
                  R_VARIABLE(515) = R_VARIABLE(515)
     +                        + EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +       Fossil Fuel, ! = 11,
     +        TAX_TYPE,6) ! OTHER

                  R_VARIABLE(523) = R_VARIABLE(523)
     +                         + EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +       Owned Nuclear Fuel, ! = 18,
     +               TAX_TYPE,0)
     +                         + EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +       Leased Nuclear Fuel, ! = 19,
     +    TAX_TYPE,0)
                  R_VARIABLE(516) = R_VARIABLE(516)
     +             +  EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                       Purchased Power, ! = 12,
     +                                            TAX_TYPE,0) ! ALL PP
       R_VARIABLE(517) = R_VARIABLE(517)
     +            +   EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                        Variable OandM, ! = 13,
     +                                            TAX_TYPE,1) ! COAL
       R_VARIABLE(518) = R_VARIABLE(518)
     +            +   EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                        Variable OandM, ! = 13,
     +                                            TAX_TYPE,2) ! GAS
       R_VARIABLE(519) = R_VARIABLE(519)
     + +   EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                        Variable OandM, ! = 13,
     +                                            TAX_TYPE,3) ! NUCLEAR
       R_VARIABLE(520) = R_VARIABLE(520)
     +             +  EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                           Fixed OandM, ! = 14,
     +                                            TAX_TYPE,1) ! COAL
       R_VARIABLE(521) = R_VARIABLE(521)
     +             +  EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                           Fixed OandM, ! = 14,
     +                                            TAX_TYPE,2) ! GAS
       R_VARIABLE(522) = R_VARIABLE(522)
     +             +  EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                           Fixed OandM, ! = 14,
     +                                    TAX_TYPE,3) ! NUCLEAR
C
C REVENUES
C
                  R_VARIABLE(526) = R_VARIABLE(526)
     +                         + REVENUES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                                secondary_sales,
     +              TAX_TYPE,1) ! SALES TO UTILITES
       R_VARIABLE(527) = R_VARIABLE(527)
     +              + REVENUES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                     secondary_sales,
     +               TAX_TYPE,2) ! COMPETITIVE SALES
       R_VARIABLE(528) = R_VARIABLE(528)
     +              + REVENUES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                     secondary_sales,
     +                            TAX_TYPE,3) ! OPPORTUNITY SALES
               ENDDO
            ENDIF
         ENDIF
      RETURN
C***********************************************************************
      ENTRY RETURN_ANNUL_EXP_DERIV_VARS(R_YR,R_CLASS,
     +                                  R_PHYS_DERIVATIVES_VAR_REVENUE,
     +                                  R_PHYS_DERIVATIVES_FIX_REVENUE,
     +                                  R_PHYS_DERIVATIVES_VAR_EXPENSE,
     +                                  R_PHYS_DERIVATIVES_FIX_EXPENSE,
     +                                  R_FIN_DERIVATIVES_VAR_REVENUE,
     +                                  R_FIN_DERIVATIVES_FIX_REVENUE,
     +                                  R_FIN_DERIVATIVES_VAR_EXPENSE,
     +                                  R_FIN_DERIVATIVES_FIX_EXPENSE)
C***********************************************************************
C
         IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
            IF(R_CLASS == 0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN
C
               YR = R_YR - 1
               PERIOD = 0 ! R_PERIOD
               SubClass = 0
               IF(YR > LAST_AVAILABLE_MONTHLY_YEAR) PERIOD = 0
               DO TAX_TYPE = 1, 2
                  R_PHYS_DERIVATIVES_VAR_EXPENSE =
     +                   R_PHYS_DERIVATIVES_VAR_EXPENSE
     +                   + EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                 Derivative Physical Exp Variable,
     +                                                TAX_TYPE,SubClass)
                  R_PHYS_DERIVATIVES_FIX_EXPENSE =
     +                   R_PHYS_DERIVATIVES_FIX_EXPENSE
     +                   + EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                    Derivative Physical Exp Fixed,
     +                                                TAX_TYPE,SubClass)
                  R_FIN_DERIVATIVES_VAR_EXPENSE =
     +                   R_FIN_DERIVATIVES_VAR_EXPENSE
     +                   + EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                Derivative Financial Exp Variable,
     +                                                TAX_TYPE,SubClass)
                  R_FIN_DERIVATIVES_FIX_EXPENSE =
     +                   R_FIN_DERIVATIVES_FIX_EXPENSE
     +                   + EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                   Derivative Financial Exp Fixed,
     +                                                TAX_TYPE,SubClass)
C
                  R_PHYS_DERIVATIVES_VAR_REVENUE =
     +                   R_PHYS_DERIVATIVES_VAR_REVENUE
     +                   + REVENUES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                 Derivative Physical Rev Variable,
     +                                                TAX_TYPE,SubClass)
                  R_PHYS_DERIVATIVES_FIX_REVENUE =
     +                   R_PHYS_DERIVATIVES_FIX_REVENUE
     +                   + REVENUES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                    Derivative Physical Rev Fixed,
     +                                                TAX_TYPE,SubClass)
                  R_FIN_DERIVATIVES_VAR_REVENUE =
     +                   R_FIN_DERIVATIVES_VAR_REVENUE
     +                   + REVENUES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                Derivative Financial Rev Variable,
     +                                                TAX_TYPE,SubClass)
                  R_FIN_DERIVATIVES_FIX_REVENUE =
     +                   R_FIN_DERIVATIVES_FIX_REVENUE
     +                   + REVENUES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                   Derivative Financial Rev Fixed,
     +                                                TAX_TYPE,SubClass)
               ENDDO
            ENDIF
         ENDIF
      RETURN
C***********************************************************************
      ENTRY CASH_FROM_DEFERRED_EXP_ACCOUNTS(R_YR,R_CLASS,
     +                                      R_PENSION_EXPENSE_CASH,
     +                                      R_STORM_EXPENSE_CASH,
     +                                      R_VACATION_PAY_CASH,
     +                                      R_ExecBenefitsCash,
     +                                      R_IncentiveCompensationCash)
C***********************************************************************
C
         R_VACATION_PAY_CASH = 0.
         R_PENSION_EXPENSE_CASH = 0.
         R_STORM_EXPENSE_CASH = 0.
         R_ExecBenefitsCash = 0.
         R_IncentiveCompensationCash = 0.
C
         IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
            IF(R_CLASS == 0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN
C
               YR = R_YR - 1
               PERIOD = 0 ! R_PERIOD
C
               IF(MONTHLY_MIDAS_ACTIVE) THEN
                  R_VACATION_PAY_CASH = R_VACATION_PAY_CASH
     +                 + CASH_REV_EXP_MONTHLY(PERIOD,YR,ASSET_CLASS,153)
                  R_PENSION_EXPENSE_CASH = R_PENSION_EXPENSE_CASH
     +                 + CASH_REV_EXP_MONTHLY(PERIOD,YR,ASSET_CLASS,68)
                  R_STORM_EXPENSE_CASH = R_STORM_EXPENSE_CASH
     +                 + CASH_REV_EXP_MONTHLY(PERIOD,YR,ASSET_CLASS,152)
               ELSEif(.false.) then
                  DO TAX_TYPE = 1, 2
                     R_VACATION_PAY_CASH = R_VACATION_PAY_CASH
     +                       + EXPENSES_MONTHLY(PERIOD,R_YR,ASSET_CLASS,
     +                                                     Vacation Pay,
     +                                                TAX_TYPE,SubClass)
                     R_PENSION_EXPENSE_CASH = R_PENSION_EXPENSE_CASH
     +                       + EXPENSES_MONTHLY(PERIOD,R_YR,ASSET_CLASS,
     +                                                  Pension Expense,
     +                                                TAX_TYPE,SubClass)
                     R_STORM_EXPENSE_CASH = R_STORM_EXPENSE_CASH
     +                       + EXPENSES_MONTHLY(PERIOD,R_YR,ASSET_CLASS,
     +                                                    STORM Expense,
     +                                                TAX_TYPE,SubClass)
                  ENDDO
               ENDIF
            ENDIF
         ENDIF
      RETURN
C***********************************************************************
      ENTRY EXPENSE_FILE_TAXES(R_YR,R_CLASS,
     +                         PROPERTY_TAXES,
     +                         WVPA_PROPERTY_TAXES_IN_POWER_COSTS,
     +                         CLASS_ADDENDUM_2_OTHER_TAXES,
     +                         ADJUSTMENT_2_OP_REV_TAX)
C***********************************************************************
         WVPA_PROPERTY_TAXES_IN_POWER_COSTS = 0.
         PROPERTY_TAXES = 0.
         IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
            IF(R_CLASS == 0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN
C
               YR = R_YR
               PROPERTY_TAXES =
     +                 EXPENSES_MONTHLY(0,YR,ASSET_CLASS,62,1,0)
     +                 + EXPENSES_MONTHLY(0,YR,ASSET_CLASS,62,2,0)
               WVPA_PROPERTY_TAXES_IN_POWER_COSTS =
     +                 EXPENSES_MONTHLY(0,YR,ASSET_CLASS,61,1,0)
     +                 + EXPENSES_MONTHLY(0,YR,ASSET_CLASS,61,2,0)
              CLASS_ADDENDUM_2_OTHER_TAXES =
     +                       CLASS_ADDENDUM_2_OTHER_TAXES
     +                       + EXPENSES_MONTHLY(0,YR,ASSET_CLASS,63,1,0)
     +                       + EXPENSES_MONTHLY(0,YR,ASSET_CLASS,63,2,0)
              ADJUSTMENT_2_OP_REV_TAX =
     +                       ADJUSTMENT_2_OP_REV_TAX
     +                       + EXPENSES_MONTHLY(0,YR,ASSET_CLASS,64,1,0)
     +                       + EXPENSES_MONTHLY(0,YR,ASSET_CLASS,64,2,0)
            ENDIF
         ENDIF
      RETURN
C***********************************************************************
      ENTRY MONTHLY_EXPENSE_FILE_TAXES(R_YR,R_CLASS,
     +                                 INC_MONTH_VARS,
     +                                 CASH_MONTH_VARS)
C***********************************************************************
C
         IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
            IF(R_CLASS == 0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN
C
               YR = R_YR
               INC_MONTH_VARS(:,Monthly Exp File Property Taxes) =
     +                 INC_MONTH_VARS(:,Monthly Exp File Property Taxes)
     +                 + EXPENSES_MONTHLY(:,YR,ASSET_CLASS,62,1,0)
     +                 + EXPENSES_MONTHLY(:,YR,ASSET_CLASS,62,2,0)
               INC_MONTH_VARS(:,WVPA Property Taxes in Power Cost) =
     +               INC_MONTH_VARS(:,WVPA Property Taxes in Power Cost)
     +               + EXPENSES_MONTHLY(:,YR,ASSET_CLASS,61,1,0)
     +               + EXPENSES_MONTHLY(:,YR,ASSET_CLASS,61,2,0)
C
               INC_MONTH_VARS(:,Monthly Exp File Other Taxes) =
     +               INC_MONTH_VARS(:,Monthly Exp File Other Taxes)
     +               + EXPENSES_MONTHLY(:,YR,ASSET_CLASS,63,1,0)
     +               + EXPENSES_MONTHLY(:,YR,ASSET_CLASS,63,2,0)
               INC_MONTH_VARS(:,monthly_exp_file_operating_rev_tax)=
     +                  INC_MONTH_VARS(:,
     +                           monthly_exp_file_operating_rev_tax)
     +                  + EXPENSES_MONTHLY(:,YR,ASSET_CLASS,64,1,0)
     +                  + EXPENSES_MONTHLY(:,YR,ASSET_CLASS,64,2,0)
C
               CASH_MONTH_VARS(:,Cash Exp File Property Taxes) =
     +                   CASH_MONTH_VARS(:,Cash Exp File Property Taxes)
     +                   + CASH_REV_EXP_MONTHLY(:,YR,ASSET_CLASS,92)
     +                   + CASH_REV_EXP_MONTHLY(:,YR,ASSET_CLASS,91)
               CASH_MONTH_VARS(:,
     +                      WVPA_CPTICOP) =
     +                 CASH_MONTH_VARS(:,
     +                        WVPA_CPTICOP)
     +                 + CASH_REV_EXP_MONTHLY(:,YR,ASSET_CLASS,91)

               CASH_MONTH_VARS(:,Cash Exp File Other Taxes) =
     +                      CASH_MONTH_VARS(:,Cash Exp File Other Taxes)
     +                      + CASH_REV_EXP_MONTHLY(:,YR,ASSET_CLASS,93)
               CASH_MONTH_VARS(:,Cash Exp File Operating Revenue Tax) =
     +            CASH_MONTH_VARS(:,Cash Exp File Operating Revenue Tax)
     +            + CASH_REV_EXP_MONTHLY(:,YR,ASSET_CLASS,94)
            ENDIF
         ENDIF
      RETURN
C***********************************************************************
c     ENTRY EXPENSE_REVENUE_ADJUSTMENTS(R_YR,R_PERIOD,R_ZERO_VALUES,
      ENTRY EXPENSE_REVENUE_ADJUSTMENTS(R_YR,R_ZERO_VALUES,
     +                                  R_FUEXP,R_PREXP,R_OPEXP,
     +                                  R_MNEXP,R_OTHER1,R_OTHER2,
     +                                  R_OTHER3,
     +                                  R_NFOWN,
     +                                  R_NFLEASE,
     +                                  R_DSM_EXPENSE,
     +                                  R_DSM_REBATE,
     +                                  R_ADJUSTMENT_CLAUSE_REVENUES,
     +                                  R_BASE_RATES_REVENUES,
     +                                  R_SECONDARY_SALES_REVENUES,
     +                                  R_SECONDARY_SALES_NOT_IN_RATES,
     +                                  R_OTHER_REVENUES,
     +                                  R_BTL_REVENUES,
     +                                  R_BTL_EXPENSE,
     +                                  R_ATL_LEASE_EXP,
     +                                  R_BTL_LEASE_EXP,
     +                                  R_SERVICE_TRANSACTIONS,
     +                                  R_EMISSION_CREDITS,
     +                                  R_DOE_DISPOSAL,
     +                                  R_DOE_DECOMMISSIONING,
     +                                  R_CATAWBA_REVENUES,
     +                                  R_CATAWBA_EXPENSES,
     +                                  R_GAS_REVENUES,
     +                                  R_TRANSMISSION_OPERATION,
     +                                  R_TRANSMISSION_MAINTENANCE,
     +                                  R_DISTRIBUTION_OPERATION,
     +                                  R_DISTRIBUTION_MAINTENANCE,
     +                                  R_CUSTOMER_ACCOUNTS,
     +                                  R_CUSTOMER_SERVICES,
     +                                  R_SALES_EXPENSE,
     +                                  R_AG_OPERATIONS,
     +                                  R_AG_MAINTENANCE,
     +                                  R_UNBILLED_REVENUES,
     +                                  R_ATL_DEFERRED_REVENUES,
     +                                  R_RELATIONSHIP_REVENUES,
     +                                  R_RESIDENTIAL_REVENUES,
     +                                  R_COMMERCIAL_REVENUES,
     +                                  R_INDUSTRIAL_REVENUES,
     +                                  R_LIGHTING_REVENUES,
     +                                  R_BULK_POWER_REVENUES,
     +                                  R_AMORTIZATION_ADJUSTMENT,
     +                                  R_DEFERRED_REVENUE_AMORTIZATION,
     +                                  R_ATL_LEASE_AMORTIZATION,
     +                                  R_BTL_LEASE_AMORTIZATION,
     +                                  R_BOOK_DEPRECIATION,
     +                                  R_NET_OF_TAX_BTL_REVENUES,
     +                                  R_CAPACITY_REVENUES,
     +                                  R_GOVERNMENT_REVENUES,
     +                                  R_BTL_AMORTIZATION_EXP,
     +                                  R_ATL_LEASE_INTEREST,
     +                                  R_BTL_LEASE_INTEREST,
     +                                  R_BTL_NUC_FUEL_DECOM_LIABILITY,
     +                                  R_CHANGE_IN_RECEIVABLES,
     +                                  R_CHANGE_IN_PAYABLES,
     +                                  R_VACATION_PAY,
     +                                  R_PENSION_EXPENSE,
     +                                  R_STORM_EXPENSE,
     +                                  R_WVPA_MEMBER_ACCRUED_REVENUES)
C**********************************************************************
C
C
         IF(R_ZERO_VALUES .and. .false.) THEN
            R_FUEXP = 0.
            R_PREXP = 0.
            R_OPEXP = 0.
            R_MNEXP = 0.
            R_OTHER1 = 0.
            R_OTHER2 = 0.
            R_OTHER3 = 0.
            R_NFOWN = 0.
            R_NFLEASE = 0.
            R_DSM_EXPENSE = 0.
            R_DSM_REBATE = 0.
            R_ADJUSTMENT_CLAUSE_REVENUES = 0.
            R_BASE_RATES_REVENUES = 0.
            R_SECONDARY_SALES_REVENUES = 0.
            R_CAPACITY_REVENUES = 0.
            R_SECONDARY_SALES_NOT_IN_RATES= 0.
            R_OTHER_REVENUES = 0.
            R_BTL_REVENUES = 0.
            R_BTL_EXPENSE = 0.
            R_ATL_LEASE_EXP = 0.
            R_BTL_LEASE_EXP = 0.
            R_SERVICE_TRANSACTIONS = 0.
            R_EMISSION_CREDITS = 0.
            R_CATAWBA_REVENUES = 0.
            R_CATAWBA_EXPENSES = 0.
            R_DOE_DISPOSAL = 0.
            R_DOE_DECOMMISSIONING = 0.
            R_GAS_REVENUES = 0.
            R_TRANSMISSION_OPERATION = 0.
            R_TRANSMISSION_MAINTENANCE = 0.
            R_DISTRIBUTION_OPERATION = 0.
            R_DISTRIBUTION_MAINTENANCE = 0.
            R_CUSTOMER_ACCOUNTS = 0.
            R_CUSTOMER_SERVICES = 0.
            R_SALES_EXPENSE = 0.
            R_AG_OPERATIONS = 0.
            R_AG_MAINTENANCE = 0.
            R_UNBILLED_REVENUES = 0.
            R_DEFERRED_REVENUES = 0.
            R_RELATIONSHIP_REVENUES = 0.
            R_ATL_DEFERRED_REVENUES = 0.
            R_RELATIONSHIP_REVENUES = 0.
            R_RESIDENTIAL_REVENUES = 0.
            R_COMMERCIAL_REVENUES = 0.
            R_INDUSTRIAL_REVENUES = 0.
            R_LIGHTING_REVENUES = 0.
            R_GOVERNMENT_REVENUES = 0.
            R_BULK_POWER_REVENUES = 0.
            R_BOOK_DEPRECIATION = 0.
            R_NET_OF_TAX_BTL_REVENUES = 0.
            R_BTL_AMORTIZATION_EXP = 0.
            R_ATL_LEASE_INTEREST = 0.
            R_BTL_LEASE_INTEREST = 0.
            R_BTL_NUC_FUEL_DECOM_LIABILITY = 0.
            R_VACATION_PAY = 0.
            R_PENSION_EXPENSE = 0.
            R_STORM_EXPENSE = 0.
            R_WVPA_MEMBER_ACCRUED_REVENUES = 0.
         ENDIF
C
               YR = R_YR - 1
               PERIOD = 0 ! R_PERIOD
               ASSET_CLASS = -1
               SubClass = 0
               IF(YR > LAST_AVAILABLE_MONTHLY_YEAR) PERIOD = 0
               R_SECONDARY_SALES_NOT_IN_RATES =
     +               R_SECONDARY_SALES_NOT_IN_RATES +
     +               SECONDARY_SALES_NOT_IN_RATES(PERIOD,YR,ASSET_CLASS)
               DO TAX_TYPE = 1, 2
                  R_BASE_RATES_REVENUES = R_BASE_RATES_REVENUES +
     +                           REVENUES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                                Base Rates,
     +                                                TAX_TYPE,SubClass)
                  R_ADJUSTMENT_CLAUSE_REVENUES =
     +                      R_ADJUSTMENT_CLAUSE_REVENUES +
     +                           REVENUES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                                adjustment_clause,
     +                                                TAX_TYPE,SubClass)
                  R_SECONDARY_SALES_REVENUES=R_SECONDARY_SALES_REVENUES+
     +                           REVENUES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                                secondary_sales,
     +                                                TAX_TYPE,SubClass)
                  R_OTHER_REVENUES = R_OTHER_REVENUES +
     +                           REVENUES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                                Other Revenue,
     +                                                TAX_TYPE,SubClass)
                  IF(WVPA()) THEN
                     R_WVPA_MEMBER_ACCRUED_REVENUES =
     +                         R_WVPA_MEMBER_ACCRUED_REVENUES
     +                         + REVENUES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                                Unbilled Revenues,
     +                                                TAX_TYPE,SubClass)
                  ELSE
                     R_UNBILLED_REVENUES = R_UNBILLED_REVENUES +
     +                           REVENUES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                                Unbilled Revenues,
     +                                                TAX_TYPE,SubClass)
                  ENDIF
                  R_DEFERRED_REVENUES = R_DEFERRED_REVENUES +
     +                           REVENUES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                                Deferred Revenues,
     +                                                TAX_TYPE,SubClass)
                  R_RELATIONSHIP_REVENUES = R_RELATIONSHIP_REVENUES +
     +                           REVENUES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                            Relationship Revenues,
     +                                                TAX_TYPE,SubClass)
                  R_ATL_DEFERRED_REVENUES = R_ATL_DEFERRED_REVENUES +
     +                           REVENUES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                                Deferred Revenues,
     +                                                TAX_TYPE,SubClass)
                  R_RESIDENTIAL_REVENUES = R_RESIDENTIAL_REVENUES +
     +                           REVENUES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                                      Residential,
     +                                                TAX_TYPE,SubClass)
                  R_COMMERCIAL_REVENUES = R_COMMERCIAL_REVENUES +
     +                           REVENUES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                                       Commercial,
     +                                                TAX_TYPE,SubClass)
                  R_INDUSTRIAL_REVENUES = R_INDUSTRIAL_REVENUES +
     +                           REVENUES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                                       Industrial,
     +                                                TAX_TYPE,SubClass)
                  R_LIGHTING_REVENUES = R_LIGHTING_REVENUES +
     +                           REVENUES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                                         Lighting,
     +                                                TAX_TYPE,SubClass)
                  R_GOVERNMENT_REVENUES = R_GOVERNMENT_REVENUES +
     +                           REVENUES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                                       Government,
     +                                                TAX_TYPE,SubClass)
                  R_BULK_POWER_REVENUES = R_BULK_POWER_REVENUES +
     +                           REVENUES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                                       Bulk Power,
     +                                                TAX_TYPE,SubClass)
                  R_NET_OF_TAX_BTL_REVENUES = R_NET_OF_TAX_BTL_REVENUES+
     +                           REVENUES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                          Net of Tax BTL Revenues,
     +                                                TAX_TYPE,SubClass)
                  R_BTL_REVENUES = R_BTL_REVENUES +
     +                           REVENUES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                                BTL Revenues,
     +                                                TAX_TYPE,SubClass)
                  R_CATAWBA_REVENUES = R_CATAWBA_REVENUES +
     +                           REVENUES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                                Catawba Revenues,
     +                                                TAX_TYPE,SubClass)
                  R_GAS_REVENUES = R_GAS_REVENUES +
     +                           REVENUES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                                Gas Revenues,
     +                                                TAX_TYPE,SubClass)
                  R_CAPACITY_REVENUES = R_CAPACITY_REVENUES +
     +                           REVENUES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                                Capacity Sales,
     +                                                TAX_TYPE,SubClass)
C
C EXPENSE LINES
C
                  R_BTL_EXPENSE = R_BTL_EXPENSE
     +                         + EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     + BTL Expenses, ! = 28,
     +                                                TAX_TYPE,SubClass)
     +                         + EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     + BTL Lease Cash, ! = 10,
     +                                                TAX_TYPE,SubClass)
                  R_BTL_LEASE_EXP = R_BTL_LEASE_EXP +
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     + BTL Lease Cash, ! = 10,
     +                                                TAX_TYPE,SubClass)

                  R_FUEXP = R_FUEXP +
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +  Fossil Fuel, ! = 11,
     +                                                TAX_TYPE,SubClass)
                  R_PREXP = R_PREXP +
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     + Purchased Power, ! = 12,
     +                                                TAX_TYPE,SubClass)
                  R_OPEXP = R_OPEXP +
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     + Variable OandM, ! = 13,
     +                                                TAX_TYPE,SubClass)
                  R_MNEXP = R_MNEXP +
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     + Fixed OandM, ! = 14,
     +                                                TAX_TYPE,SubClass)
                  R_OTHER1 = R_OTHER1 +
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     + Other OandM, ! = 15,
     +                                                TAX_TYPE,SubClass)
                  R_OTHER2 = R_OTHER2 +
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +  Purchased Gas, ! = 16,
     +                                                TAX_TYPE,SubClass)
                  R_OTHER3 = R_OTHER3 +
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +   Other, ! = 17,
     +                                                TAX_TYPE,SubClass)
                  R_NFOWN = R_NFOWN +
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +  Owned Nuclear Fuel, ! = 18,
     +                                                TAX_TYPE,SubClass)
                  R_NFLEASE = R_NFLEASE +
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +  Leased Nuclear Fuel, ! = 19,
     +                                                TAX_TYPE,SubClass)
                  R_DSM_EXPENSE= R_DSM_EXPENSE +
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     + DSM Expense, ! = 20,
     +                                                TAX_TYPE,SubClass)
                  R_DSM_REBATE = R_DSM_REBATE +
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     + DSM Rebate, ! = 21,
     +                                                TAX_TYPE,SubClass)
                  R_ATL_LEASE_EXP = R_ATL_LEASE_EXP +
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     + ATL Book Lease Expense, ! = 22,
     +                                                TAX_TYPE,SubClass)
                  R_SERVICE_TRANSACTIONS = R_SERVICE_TRANSACTIONS +
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     + Service Transactions, ! = 23,
     +                                                TAX_TYPE,SubClass)
                  R_EMISSION_CREDITS = R_EMISSION_CREDITS +
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     + Emission Credits, ! = 24,
     +                                                TAX_TYPE,SubClass)
                  R_DOE_DECOMMISSIONING = R_DOE_DECOMMISSIONING +
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     + DOE Decommissioning, ! = 25,
     +                                                TAX_TYPE,SubClass)
                  R_BTL_NUC_FUEL_DECOM_LIABILITY =
     +                     R_BTL_NUC_FUEL_DECOM_LIABILITY +
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     + BTL Decommissioning Liability,   ! = 8
     +                                                TAX_TYPE,SubClass)
                  R_DOE_DISPOSAL = R_DOE_DISPOSAL +
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     + DOE Disposal, ! = 26,
     +                                                TAX_TYPE,SubClass)
                  R_CATAWBA_EXPENSES = R_CATAWBA_EXPENSES +
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     + Catawba Expenses, ! = 27,
     +                                                TAX_TYPE,SubClass)
                  R_TRANSMISSION_OPERATION = R_TRANSMISSION_OPERATION +
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                           Transmission Operation,
     +                                                TAX_TYPE,SubClass)
                  R_TRANSMISSION_MAINTENANCE =
     +                          R_TRANSMISSION_MAINTENANCE +
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                         Transmission Maintenance,
     +                                                TAX_TYPE,SubClass)
                  R_DISTRIBUTION_OPERATION = R_DISTRIBUTION_OPERATION +
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                           Distribution Operation,
     +                                                TAX_TYPE,SubClass)
                  R_DISTRIBUTION_MAINTENANCE =
     +                          R_DISTRIBUTION_MAINTENANCE +
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                         Distribution Maintenance,
     +                                                TAX_TYPE,SubClass)
                  R_CUSTOMER_ACCOUNTS = R_CUSTOMER_ACCOUNTS +
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                                Customer Accounts,
     +                                                TAX_TYPE,SubClass)
                  R_CUSTOMER_SERVICES = R_CUSTOMER_SERVICES +
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                                Customer Services,
     +                                                TAX_TYPE,SubClass)
                  R_SALES_EXPENSE =  R_SALES_EXPENSE +
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                                    Sales Expense,
     +                                                TAX_TYPE,SubClass)
                  R_AG_OPERATIONS = R_AG_OPERATIONS +
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                                    AG Operations,
     +                                                TAX_TYPE,SubClass)
                  R_AG_MAINTENANCE = R_AG_MAINTENANCE +
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                                   AG Maintenance,
     +                                                TAX_TYPE,SubClass)
                  R_AMORTIZATION_ADJUSTMENT = R_AMORTIZATION_ADJUSTMENT+
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                                    Amortization,
     +                                                TAX_TYPE,SubClass)
                  R_BTL_AMORTIZATION_EXP = R_BTL_AMORTIZATION_EXP +
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                                 BTL Amortization,
     +                                                TAX_TYPE,SubClass)
                  R_DEFERRED_REVENUE_AMORTIZATION =
     +                                 R_DEFERRED_REVENUE_AMORTIZATION +
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                    Deferred Revenue Amortization,
     +                                                TAX_TYPE,SubClass)
                  R_ATL_LEASE_AMORTIZATION = R_ATL_LEASE_AMORTIZATION +
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                           ATL Lease Amortization,
     +                                                TAX_TYPE,SubClass)
                  R_BTL_LEASE_AMORTIZATION = R_BTL_LEASE_AMORTIZATION +
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                           BTL Lease Amortization,
     +                                                TAX_TYPE,SubClass)
                  R_BOOK_DEPRECIATION = R_BOOK_DEPRECIATION +
     +                           EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                                Book Depreciation,
     +                                                TAX_TYPE,SubClass)
                  R_ATL_LEASE_INTEREST = R_ATL_LEASE_INTEREST
     +                         + EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                               ATL Lease Interest,
     +                                                TAX_TYPE,SubClass)
                  R_BTL_LEASE_INTEREST = R_BTL_LEASE_INTEREST
     +                         + EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                               BTL Lease Interest,
     +                                                TAX_TYPE,SubClass)
                  R_VACATION_PAY = R_VACATION_PAY
     +                         + EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                                     Vacation Pay,
     +                                                TAX_TYPE,SubClass)
                  R_PENSION_EXPENSE = R_PENSION_EXPENSE
     +                         + EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                                  Pension Expense,
     +                                                TAX_TYPE,SubClass)
     +                         + EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                         Unfunded Pension Expense,
     +                                                TAX_TYPE,SubClass)
                  R_STORM_EXPENSE = R_STORM_EXPENSE
     +                         + EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                                                    STORM Expense,
     +                                                TAX_TYPE,SubClass)
               ENDDO
C
C INTRA-COMPANY BOOK RECEIPTS AND PAYMENTS
C
               MO = 0
               TOTAL_BOOK_REVENUES = 0.
               DO DATA_POS = Base Rates, Government
                  IF(DATA_POS == Unbilled Revenues) CYCLE
                  IF(DATA_POS == Deferred Revenues) CYCLE
                  DO TAX_TYPE = 1, 2
                     TOTAL_BOOK_REVENUES = TOTAL_BOOK_REVENUES
     +                          + REVENUES_MONTHLY(0,YR,ASSET_CLASS,
     +                                       DATA_POS,TAX_TYPE,SubClass)
                  ENDDO
               ENDDO
               DO TAX_TYPE = 1, 2
                  TOTAL_BOOK_REVENUES = TOTAL_BOOK_REVENUES
     +                   + REVENUES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                Derivative Financial Rev Variable,
     +                                                TAX_TYPE,SubClass)
     +                   + REVENUES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                   Derivative Financial Rev Fixed,
     +                                                TAX_TYPE,SubClass)
     +                   + REVENUES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                    Derivative Physical Rev Fixed,
     +                                                TAX_TYPE,SubClass)
     +                   + REVENUES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                 Derivative Physical Rev Variable,
     +                                                TAX_TYPE,SubClass)
               ENDDO
               TOTAL_BOOK_EXPENSES = 0.
               DO DATA_POS = BTL Lease Cash, AG Maintenance
                  IF(DATA_POS == Owned Nuclear Fuel) CYCLE
                  IF(DATA_POS == DOE Decommissioning) CYCLE
                  DO TAX_TYPE = 1, 2
                     TOTAL_BOOK_EXPENSES = TOTAL_BOOK_EXPENSES
     +                           + EXPENSES_MONTHLY(0,YR,ASSET_CLASS,
     +                                       DATA_POS,TAX_TYPE,SubClass)
                  ENDDO
               ENDDO
               DO TAX_TYPE = 1, 2
                  TOTAL_BOOK_EXPENSES = TOTAL_BOOK_EXPENSES
     +                  + EXPENSES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                   Derivative Financial Exp Fixed,
     +                                                TAX_TYPE,SubClass)
     +                  + EXPENSES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                Derivative Financial Exp Variable,
     +                                                TAX_TYPE,SubClass)
     +                  + EXPENSES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                    Derivative Physical Exp Fixed,
     +                                                TAX_TYPE,SubClass)
     +                  + EXPENSES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                 Derivative Physical Exp Variable,
     +                                                TAX_TYPE,SubClass)
               ENDDO
C
C CASH RECEITS AND PAYMENTS
C
               TOTAL_CASH_RECEPITS = 0.
               DO DATA_POS = Cash Base Rates, Cash Government
                  IF(DATA_POS == Cash Unbilled Revenues) CYCLE
                  IF(DATA_POS == Cash Deferred Revenues) CYCLE
                  TOTAL_CASH_RECEPITS = TOTAL_CASH_RECEPITS +
     +                   CASH_REV_EXP_MONTHLY(0,YR,ASSET_CLASS,DATA_POS)
               ENDDO
               TOTAL_CASH_RECEPITS = TOTAL_CASH_RECEPITS
     +   + CASH_MONTHLY(MO,YR,ASSET_CLASS,Cash Receipts) ! 3
     +   + CASH_REV_EXP_MONTHLY(MO,YR,ASSET_CLASS,
     +                    Derivative Physical Rev Fixed)
     +   + CASH_REV_EXP_MONTHLY(MO,YR,ASSET_CLASS,
     +                 Derivative Physical Rev Variable)
     +   + CASH_REV_EXP_MONTHLY(MO,YR,ASSET_CLASS,
     +                   Derivative Financial Rev Fixed)
     +   + CASH_REV_EXP_MONTHLY(MO,YR,ASSET_CLASS,
     +                Derivative Financial Rev Variable)
C
               TOTAL_CASH_PAYMENTS = 0.
               DO DATA_POS = Cash BTL Lease Cash, Cash AG Maintenance
                  TOTAL_CASH_PAYMENTS = TOTAL_CASH_PAYMENTS +
     +                   CASH_REV_EXP_MONTHLY(0,YR,ASSET_CLASS,DATA_POS)
               ENDDO
               TOTAL_CASH_PAYMENTS = TOTAL_CASH_PAYMENTS
     + + CASH_MONTHLY(MO,YR,ASSET_CLASS,Cash Payments) ! 4
     + + CASH_REV_EXP_MONTHLY(MO,YR,ASSET_CLASS,
     +               Cash Derative Financial Exp Var)
     + + CASH_REV_EXP_MONTHLY(MO,YR,ASSET_CLASS,
     +              Cash Derative Financial Exp Fixed)
     + + CASH_REV_EXP_MONTHLY(MO,YR,ASSET_CLASS,
     +               Cash Derative Physical Exp Fixed)
     + + CASH_REV_EXP_MONTHLY(MO,YR,ASSET_CLASS,
     +                 Cash Derative Physical Exp Var)
C
               R_CHANGE_IN_RECEIVABLES = R_CHANGE_IN_RECEIVABLES
     +                                   + TOTAL_BOOK_REVENUES
     +                                   - TOTAL_CASH_RECEPITS
               R_CHANGE_IN_PAYABLES =  R_CHANGE_IN_PAYABLES
     +                                 + TOTAL_BOOK_EXPENSES
     +                                 - TOTAL_CASH_PAYMENTS
      RETURN
C***********************************************************************
      ENTRY MONTHLY_REVENUES(R_YR,R_CLASS,MONTH_VARS)
C***********************************************************************
C
         call RETURN_MONTHLY_CL_REVENUES(R_CLASS,MONTH_VARS)
         CALL RETURN_MONTHLY_EL_REVENUES(R_CLASS,MONTH_VARS)
C
         CALL RETURN_MONTHY_RC_REVENUES(R_CLASS,MONTH_VARS)
C
         CALL MONTHLY_SERVICE_TRANS_REVENUES(R_CLASS,MONTH_VARS)
         VOID_INT2 = RETURN_MONTH_CUSTOMER_REVENUES(R_CLASS,MONTH_VARS)
C
         IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
            SubClass = 0
            IF(R_CLASS == 0) THEN
               ASSET_CLASS = 0
            ELSEIF(R_CLASS == -1) THEN
               ASSET_CLASS = -1
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0 .OR. R_CLASS == -1)THEN
C
               CALL RETURN_ProSymMonthlyRevsExpsBook(R_YR,
     +                                               ASSET_CLASS,
     +                                               MONTH_VARS)
               YR = R_YR ! - 1
               DO MO = 0, 12
                  DO DATA_POS = 1, LAST_INCOME_LINE
                     IF(DATA_POS == Unbilled Revenues) CYCLE
                     DO TAX_TYPE = 1, 2
                        IF(DATA_POS > EXP_OFFSET_LINE) THEN
                        ELSE
                           MONTH_VARS(MO,DATA_POS) =
     +                             MONTH_VARS(MO,DATA_POS)
     +                             + REVENUES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                       DATA_POS,TAX_TYPE,SubClass)
                        ENDIF
                     ENDDO
                  ENDDO
               ENDDO
C
               IF(WVPA()) THEN
                  MONTH_VARS(:,Monthly WVPA Accrued Member Revenues) =
     +                MONTH_VARS(:,Monthly WVPA Accrued Member Revenues)
     +                + REVENUES_MONTHLY(:,YR,ASSET_CLASS,
     +                                                Unbilled Revenues,
     +                                                       1,SubClass)
     +                + REVENUES_MONTHLY(:,YR,ASSET_CLASS,
     +                                                Unbilled Revenues,
     +                                                       2,SubClass)
               ELSE
                  MONTH_VARS(:,Unbilled Revenues) =
     +                   MONTH_VARS(:,Unbilled Revenues)
     +                   + REVENUES_MONTHLY(:,YR,ASSET_CLASS,
     +                                                Unbilled Revenues,
     +                                                       1,SubClass)
     +                   + REVENUES_MONTHLY(:,YR,ASSET_CLASS,
     +                                                Unbilled Revenues,
     +                                                       2,SubClass)
               ENDIF
               MONTH_VARS(:,Monthly Gas Wholesale Revenues) =
     +                   MONTH_VARS(:,Monthly Gas Wholesale Revenues)
     +                   + REVENUES_MONTHLY(:,YR,ASSET_CLASS,
     +                                           Gas Wholesale Revenues,
     +                                                       1,SubClass)
     +                   + REVENUES_MONTHLY(:,YR,ASSET_CLASS,
     +                                           Gas Wholesale Revenues,
     +                                                       2,SubClass)
            ENDIF
         ENDIF
      RETURN
C***********************************************************************
      ENTRY MONTHLY_ELIM_REVENUES(R_YR,R_CLASS,MONTH_VARS)
C***********************************************************************
C
         YR = R_YR ! - 1
         SubClass = 0
         ASSET_CLASS =  R_CLASS
         DO
            CALL RETURN_MONTHY_RC_REVENUES(R_CLASS,MONTH_VARS)
C
            CALL MONTHLY_SERVICE_TRANS_REVENUES(R_CLASS,MONTH_VARS)
            VOID_INT2=RETURN_MONTH_CUSTOMER_REVENUES(R_CLASS,MONTH_VARS)
C
            IF(ASSET_CLASS <= MAX_ASSET_CLASS_NUM) THEN
               IF(ASSET_CLASS > 0) THEN
                  ASSET_CLASS = ASSET_CLASS_POINTER(ASSET_CLASS)
               ENDIF
               IF(ASSET_CLASS >= -1) THEN
C
                  DO DATA_POS = 1, EXP_OFFSET_LINE
                     MONTH_VARS(:,DATA_POS) =
     +                             MONTH_VARS(:,DATA_POS)
     +                             + REVENUES_MONTHLY(:,YR,ASSET_CLASS,
     +                                              DATA_POS,1,SubClass)
     +                             + REVENUES_MONTHLY(:,YR,ASSET_CLASS,
     +                                              DATA_POS,2,SubClass)
                  ENDDO
C Gas Wholesale Revenues 31
                  MONTH_VARS(:,31) =
     +                   MONTH_VARS(:,31)
     +                   + REVENUES_MONTHLY(:,YR,ASSET_CLASS,31,
     +                                                       1,SubClass)
     +                   + REVENUES_MONTHLY(:,YR,ASSET_CLASS,31,
     +                                                       2,SubClass)
               ENDIF
            ENDIF
            IF(ASSET_CLASS == -1) THEN
               CALL ELIMINATION_CLASS_ID(ASSET_CLASS)
               IF(ASSET_CLASS < 0) EXIT
            ELSE
               EXIT
            ENDIF
         enddo
      RETURN
C***********************************************************************
      ENTRY MONTHLY_CASH_REV_EXP(R_YR,R_CLASS,MONTH_VARS)
C***********************************************************************


         TEMP_CASH_VARS = 0.
         VOID_INT2 = RETURN_MONTHLY_CL_CASH_REVENUES(R_CLASS,
     +                                               TEMP_CASH_VARS)
         CALL RETURN_MONTHLY_EL_CASH_REVENUES(R_CLASS,TEMP_CASH_VARS)
         VOID_INT2 = RETURN_MONTHLY_CL_CASH_EXPENSES(R_CLASS,
     +                                               TEMP_CASH_VARS)
         CALL RETURN_MONTHLY_EL_CASH_EXPENSES(R_CLASS,TEMP_CASH_VARS)
         VOID_INT2 = RETURN_MONTHLY_FUEL_DERIV_CASH_VARS(R_CLASS,
     +                                                   TEMP_CASH_VARS)
         VOID_INT2 = RETURN_MONTHLY_DERIV_CASH_VARIABLES(R_CLASS,
     +                                                   TEMP_CASH_VARS)
         CALL RETURN_ProSymMonthlyRevsExpsCash(R_YR,
     +                                         R_CLASS,
     +                                         MONTH_VARS)

      ENTRY MONTHLY_CASH_ELIM_REV_EXP(R_YR,R_CLASS,MONTH_VARS)
C
         CALL RETURN_MONTHY_RC_CASH_REVENUES(R_CLASS,MONTH_VARS)
         CALL RETURN_MONTHY_RC_CASH_EXPENSES(R_CLASS,MONTH_VARS,
     +                                       RC_MONTH_PURCHASED_POWER)
C
         CALL MONTHLY_SERVICE_TRANS_CASH(R_CLASS,TEMP_CASH_VARS)
         VOID_INT2 = RETURN_CUSTOMER_CASH_REVENUES(R_CLASS,MONTH_VARS,
     +                                         TF_MONTH_PURCHASED_POWER)
C
c NEEDE FOR PURCHASED rm EXPENSE 11/12/07 DrG
         TEMP_CASH_VARS(:,
     +                  csh_xpns_rsrv_mgn_cap_pchs) =
     +         TEMP_CASH_VARS(:,
     +                    csh_xpns_rsrv_mgn_cap_pchs)
     +         + TF_MONTH_PURCHASED_CAPACITY(:)
c
         IF(USE_BUDGET_VARIABLE_OM) THEN
            TEMP_CASH_VARS(:,Cash Variable OandM) = 0.
            TEMP_CASH_VARS(:,Cash Fixed OandM) = 0.
            IF(USE_BUDGET_FUEL_PURCHASE) THEN
               TEMP_CASH_VARS(:,Cash Fossil Fuel) = 0.
               TEMP_CASH_VARS(:,Cash Purchased Power) = 0.
            ENDIF
         ENDIF
C
         IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
            IF(R_CLASS == 0) THEN
               ASSET_CLASS = 0
            ELSEIF(R_CLASS == -1) THEN
               ASSET_CLASS = -1
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            CALL LAG_PRODUCTION_CASH_EXPENSES(ASSET_CLASS,
     +                                        TEMP_CASH_VARS,
     +                                        MONTH_VARS)
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0 .OR. R_CLASS == -1)THEN
C
               YR = R_YR ! - 1
               DO MO = 0, 12
                  DO DATA_POS = Cash Base Rates,Cash Total Base Revenues
                     MONTH_VARS(MO,DATA_POS) = MONTH_VARS(MO,DATA_POS) +
     +                  CASH_REV_EXP_MONTHLY(MO,YR,ASSET_CLASS,DATA_POS)
                  ENDDO
                  DATA_POS = Cash BTL Lease Cash   ! 40
                  MONTH_VARS(MO,DATA_POS) = MONTH_VARS(MO,DATA_POS) +
     +                  CASH_REV_EXP_MONTHLY(MO,YR,ASSET_CLASS,DATA_POS)
                  IF(.NOT. USE_PRODUCTION_MODULE_EXPENSE) THEN
                     DO DATA_POS = Cash Fossil Fuel, Cash Fixed OandM
                        MONTH_VARS(MO,DATA_POS)=MONTH_VARS(MO,DATA_POS)+
     +                  CASH_REV_EXP_MONTHLY(MO,YR,ASSET_CLASS,DATA_POS)
                        IF(DATA_POS == Cash Purchased Power) THEN ! 41
                           MONTH_VARS(MO,DATA_POS) =
     +                                    MONTH_VARS(MO,DATA_POS)
     +                                    + RC_MONTH_PURCHASED_POWER(MO)
     +                                    + TF_MONTH_PURCHASED_POWER(MO)
                        ENDIF
                     ENDDO
                  ENDIF
                  DO DATA_POS = Cash Other OandM, Cash Other
                     MONTH_VARS(MO,DATA_POS) = MONTH_VARS(MO,DATA_POS) +
     +                  CASH_REV_EXP_MONTHLY(MO,YR,ASSET_CLASS,DATA_POS)
                  ENDDO
                  DO DATA_POS = Cash Leased Nuclear Fuel,   
     +                                      Cash Lease Interest Payments
                     IF(DATA_POS == Cash Post Retirement Payments)CYCLE
                     MONTH_VARS(MO,DATA_POS) = MONTH_VARS(MO,DATA_POS) +
     +                  CASH_REV_EXP_MONTHLY(MO,YR,ASSET_CLASS,DATA_POS)
                  ENDDO
C
                  MONTH_VARS(MO,Cash Unfunded Retirement Payments) =   
     +                  MONTH_VARS(MO,Cash Unfunded Retirement Payments)
     +                  + CASH_REV_EXP_MONTHLY(MO,YR,ASSET_CLASS,
     + Cash Post Retirement Payments) 

                  MONTH_VARS(MO,Cash Paid on Accounts Payable) =        
     +                   MONTH_VARS(MO,Cash Paid on Accounts Payable)
     +                   + CASH_MONTHLY(MO,YR,ASSET_CLASS,Cash Payments)
                  MONTH_VARS(MO,Cash Received Accounts Receivable) =    
     +                  MONTH_VARS(MO,Cash Received Accounts Receivable)
     +                  + CASH_MONTHLY(MO,YR,ASSET_CLASS,Cash Receipts) 
                  MONTH_VARS(MO,Cash BTL Lease Interest Payments) =     
     +                  MONTH_VARS(MO,Cash BTL Lease Interest Payments)
     +                  + CASH_REV_EXP_MONTHLY(MO,YR,ASSET_CLASS,
     +                                 Cash BTL Lease Interest Payments)
                  MONTH_VARS(MO,Cash Gas Wholesale Revenues) =          
     +                  MONTH_VARS(MO,Cash Gas Wholesale Revenues)
     +                  + CASH_REV_EXP_MONTHLY(MO,YR,ASSET_CLASS,
     +                                           Gas Wholesale Revenues)
                  MONTH_VARS(MO,WVPA_CMCOP) =        ! 198
     +                  MONTH_VARS(MO,WVPA_CMCOP)
     +                  + CASH_REV_EXP_MONTHLY(MO,YR,ASSET_CLASS,
     +                                       Cash WVPA Member Purchases)
                  MONTH_VARS(MO,WVPA Cash NonMember Cost of Power) =    
     +                  MONTH_VARS(MO,WVPA Cash NonMember Cost of Power)
     +                  + CASH_REV_EXP_MONTHLY(MO,YR,ASSET_CLASS,
     +                                   Cash WVPA Non Member Purchases)
                  MONTH_VARS(MO,WVPA Cash Member Cost of Services) =    
     +                  MONTH_VARS(MO,WVPA Cash Member Cost of Services)
     +                  + CASH_REV_EXP_MONTHLY(MO,YR,ASSET_CLASS,
     +                                        Cash WVPA Member Services)
                  MONTH_VARS(MO,WVPA_C_NMCOS) =  ! 206
     +               MONTH_VARS(MO,WVPA_C_NMCOS)
     +                  + CASH_REV_EXP_MONTHLY(MO,YR,ASSET_CLASS,
     +                                    Cash WVPA Non Member Services)
               ENDDO
            ENDIF
         ENDIF
      RETURN
C***********************************************************************
      ENTRY BUDGET_FUEL_PURCHASES(R_YR,R_FUEL_COST,R_PURCHASE_COST)
C***********************************************************************
C
C
         YR = R_YR
C
         IF(YR <= LAST_AVAILABLE_MONTHLY_YEAR) THEN
            R_FUEL_COST(1:) = BUDGET_EXPENSE(1:,YR,1) ! Fossil Fuel
            R_PURCHASE_COST(1:) = BUDGET_EXPENSE(1:,YR,2) 
            R_FUEL_COST(0) = SUM(R_FUEL_COST(1:))
            R_PURCHASE_COST(0) = SUM(R_PURCHASE_COST(1:))
         ELSE
            R_FUEL_COST(0) = BUDGET_EXPENSE(0,YR,1) ! Fossil Fuel
            R_PURCHASE_COST(0) = BUDGET_EXPENSE(0,YR,2) 
         ENDIF
      RETURN
C***********************************************************************
      ENTRY RETURN_SRP_NUC_DECOM_COST(R_YR,R_CLASS,R_NUC_DECOM_COSTS)
C***********************************************************************
C
         R_NUC_DECOM_COSTS = 0.
         IF(R_CLASS <= MAX_ASSET_CLASS_NUM .AND.
     +                                 ALLOCATED(EXPENSES_MONTHLY)) THEN
            IF(R_CLASS == 0) THEN
               ASSET_CLASS = 0
            ELSEIF(R_CLASS == -1) THEN
               ASSET_CLASS = -1
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0 .OR. R_CLASS == -1)THEN
C
C THE FOLLOW CALL GETS 12 MONTHS OF PRODUCTION VALUES
C
               YR = R_YR
               SubClass = 0
               R_NUC_DECOM_COSTS(:) =
     +                EXPENSES_MONTHLY(:,YR,ASSET_CLASS,25,1,SubClass)
     +                + EXPENSES_MONTHLY(:,YR,ASSET_CLASS,25,2,SubClass)
            ENDIF
         ENDIF
      RETURN
C***********************************************************************
      ENTRY MONTHLY_EXPENSES(R_YR,R_CLASS,MONTH_VARS)
C***********************************************************************
C
         VOID_INT2 = RETURN_MONTHLY_CL_EXPENSES(R_CLASS,MONTH_VARS)
         CALL RETURN_MONTHLY_EL_EXPENSES(R_CLASS,MONTH_VARS)
         CALL MONTHLY_SERVICE_TRANS_EXPENSES(R_CLASS,MONTH_VARS)

         CALL RETURN_MONTHY_RC_EXPENSES(R_CLASS,MONTH_VARS,
     +                                         RC_MONTH_PURCHASED_POWER)
         VOID_INT2 = RETURN_MONTH_CUSTOMER_VARIABLES(R_CLASS,
     +                                      TF_MONTH_PURCHASED_POWER,
     +                                      TF_MONTH_PURCHASED_CAPACITY)
     
        ! TODO: Compiler giving Subscript Out of Range warning for both
        ! usages of MONTH_VARS here. May be spurious, may result in 
        ! runtime errors.  Started with reintegration after refactoring
        ! financial subsystem. Debugmod
        MONTH_VARS(0:12,monthly_expense_reserve_mgn_cap_purch-30)  =
     +        MONTH_VARS(0:12,
     +              monthly_expense_reserve_mgn_cap_purch-30)
     +        + TF_MONTH_PURCHASED_CAPACITY(0:12)

         IF(USE_BUDGET_VARIABLE_OM) THEN
            MONTH_VARS(:,Variable OandM) = 0.
            MONTH_VARS(:,Fixed OandM) = 0.
            IF(USE_BUDGET_FUEL_PURCHASE) THEN
               MONTH_VARS(:,Fossil Fuel) = 0.
               MONTH_VARS(:,Purchased Power) = 0.
            ENDIF
         ENDIF
C
         IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
            IF(R_CLASS == 0) THEN
               ASSET_CLASS = 0
            ELSEIF(R_CLASS == -1) THEN
               ASSET_CLASS = -1
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0 .OR. R_CLASS == -1)THEN
C
C THE FOLLOW CALL GETS 12 MONTHS OF PRODUCTION VALUES
C
               YR = R_YR ! - 1
               SubClass = 0
               DO MO = 0, 12
                  DO DATA_POS = 1, LAST_EXPENSE_ITEM
                     IF(DATA_POS == 61) CYCLE
                     IF(DATA_POS == 62) CYCLE
                     IF(DATA_POS == WVPA Member Purchases) THEN
                        MONTH_VARS(MO,
     +                              WVPA Cost of Power for Members-30) =
     +                           EXPENSES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                              DATA_POS,1,SubClass)
     +                           + EXPENSES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                              DATA_POS,2,SubClass)
                        CYCLE
                     ENDIF
                     IF(DATA_POS == WVPA Non Member Purchases) THEN
                        MONTH_VARS(MO,
     +                          WVPA Cost of Power for Non Members-30) =
     +                           EXPENSES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                              DATA_POS,1,SubClass)
     +                           + EXPENSES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                              DATA_POS,2,SubClass)
                        CYCLE
                     ENDIF
                     IF(DATA_POS == WVPA Member Services) THEN
                        MONTH_VARS(MO,WVPA Member Cost of Services-30) =
     +                           EXPENSES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                              DATA_POS,1,SubClass)
     +                           + EXPENSES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                              DATA_POS,2,SubClass)
                        CYCLE
                     ENDIF
                     IF(DATA_POS == WVPA Non Member Services) THEN
                        MONTH_VARS(MO,
     +                            WVPA Non Member Cost of Services-30) =
     +                           EXPENSES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                              DATA_POS,1,SubClass)
     +                           + EXPENSES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                              DATA_POS,2,SubClass)
                        CYCLE
                     ENDIF
                     IF(DATA_POS == Deferred Fuel) THEN
                        MONTH_VARS(MO,Monthly Deferred Fuel Expense-30)=
     +                           EXPENSES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                              DATA_POS,1,SubClass)
     +                           + EXPENSES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                              DATA_POS,2,SubClass)
                        CYCLE
                     ENDIF
                     IF(DATA_POS == Amortization) THEN
                        MONTH_VARS(MO,Monthly Other ATL Amort-30) =
     +                           EXPENSES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                              DATA_POS,1,SubClass)
     +                           + EXPENSES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                              DATA_POS,2,SubClass)
                        CYCLE
                     ENDIF
                     IF(DATA_POS == ATL Book Lease Expense) THEN
                        MONTH_VARS(MO,Monthly ATL Lease Payment-30) =
     +                           EXPENSES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                              DATA_POS,1,SubClass)
     +                           + EXPENSES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                              DATA_POS,2,SubClass)
                        CYCLE
                     ENDIF
                     IF(DATA_POS == BTL Amortization) THEN
                        MONTH_VARS(MO,Monthly Other BTL Amort-30) =
     +                           EXPENSES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                              DATA_POS,1,SubClass)
     +                           + EXPENSES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                              DATA_POS,2,SubClass)
                        CYCLE
                     ENDIF
                     IF(USE_PRODUCTION_MODULE_EXPENSE) THEN
                        IF(DATA_POS == Variable OandM) CYCLE
                        IF(DATA_POS == Fixed OandM) CYCLE
                        IF(DATA_POS == Fossil Fuel) CYCLE
                        IF(DATA_POS == Purchased Power) CYCLE
                     ENDIF
                     IF(DATA_POS == Purchased Power) THEN
                        MONTH_VARS(MO,DATA_POS)=MONTH_VARS(MO,DATA_POS)
     +                            + EXPENSES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                              DATA_POS,1,SubClass)
     +                            + EXPENSES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                              DATA_POS,2,SubClass)
     +                            + TF_MONTH_PURCHASED_POWER(MO)
     +                            + RC_MONTH_PURCHASED_POWER(MO)
                        IF(WVPA()) THEN
                           MONTH_VARS(MO,DATA_POS) =
     +                           MONTH_VARS(MO,DATA_POS)
     +                           + EXPENSES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                        WVPA Non Member Purchases,
     +                                                       1,SubClass)
     +                           + EXPENSES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                        WVPA Non Member Purchases,
     +                                                       2,SubClass)
     +                           + EXPENSES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                            WVPA Member Purchases,
     +                                                       1,SubClass)
     +                           + EXPENSES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                            WVPA Member Purchases,
     +                                                       2,SubClass)
                        ENDIF
                        CYCLE
                     ENDIF
                     IF(DATA_POS == Service Transactions) THEN
                        MONTH_VARS(MO,DATA_POS)=MONTH_VARS(MO,DATA_POS)
     +                           + EXPENSES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                              DATA_POS,1,SubClass)
     +                           + EXPENSES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                              DATA_POS,2,SubClass)
                        IF(WVPA()) THEN
                           MONTH_VARS(MO,DATA_POS) =
     +                           MONTH_VARS(MO,DATA_POS)
     +                           + EXPENSES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                         WVPA Non Member Services,
     +                                                       1,SubClass)
     +                           + EXPENSES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                         WVPA Non Member Services,
     +                                                       2,SubClass)
     +                           + EXPENSES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                             WVPA Member Services,
     +                                                       1,SubClass)
     +                           + EXPENSES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                             WVPA Member Services,
     +                                                       2,SubClass)
                        ENDIF
                        CYCLE
                     ENDIF
                     MONTH_VARS(MO,DATA_POS)=MONTH_VARS(MO,DATA_POS)
     +                            + EXPENSES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                              DATA_POS,1,SubClass)
     +                            + EXPENSES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                              DATA_POS,2,SubClass)



                  ENDDO
               ENDDO
               MONTH_VARS(:,ATL Book Lease Expense) =

     +                 + MONTH_VARS(:,ATL Lease Amortization)
     +                 + MONTH_VARS(:,ATL Lease Interest)
               MONTH_VARS(:,BTL Expenses) =
     +                 MONTH_VARS(:,BTL Expenses)
     +                 + MONTH_VARS(:,BTL Lease Amortization)
     +                 + MONTH_VARS(:,BTL Lease Interest)
     +                 + MONTH_VARS(:,BTL Lease Cash)
            ENDIF
         ENDIF
      RETURN
C***********************************************************************
      ENTRY MONTHLY_ELIM_EXPENSES(R_YR,R_CLASS,MONTH_VARS)
C***********************************************************************
C
         YR = R_YR ! - 1
         SubClass = 0
         ASSET_CLASS =  R_CLASS
         do
            CALL RETURN_MONTHY_RC_EXPENSES(R_CLASS,MONTH_VARS,
     +                                         RC_MONTH_PURCHASED_POWER)
C
            IF(ASSET_CLASS <= MAX_ASSET_CLASS_NUM) THEN
               IF(ASSET_CLASS > 0) THEN
                  ASSET_CLASS = ASSET_CLASS_POINTER(ASSET_CLASS)
               ENDIF
               IF(ASSET_CLASS >= -1) THEN
C
C THE FOLLOW CALL GETS 12 MONTHS OF PRODUCTION VALUES
C
                  DO DATA_POS = 1, LAST_EXPENSE_ITEM
                     IF(DATA_POS == Deferred Fuel) THEN
                        MONTH_VARS(:,Monthly Deferred Fuel Expense-30)=
     +                           MONTH_VARS(:,
     +                                 Monthly Deferred Fuel Expense-30)
     +                           + EXPENSES_MONTHLY(:,YR,ASSET_CLASS,
     +                                              DATA_POS,1,SubClass)
     +                           + EXPENSES_MONTHLY(:,YR,ASSET_CLASS,
     +                                              DATA_POS,2,SubClass)
                        CYCLE
                     ENDIF
                     IF(DATA_POS == Amortization) THEN
                        MONTH_VARS(:,Monthly Other ATL Amort-30) =
     +                         MONTH_VARS(:,Monthly Other ATL Amort-30)
     +                         + EXPENSES_MONTHLY(:,YR,ASSET_CLASS,
     +                                              DATA_POS,1,SubClass)
     +                         + EXPENSES_MONTHLY(:,YR,ASSET_CLASS,
     +                                              DATA_POS,2,SubClass)
                        CYCLE
                     ENDIF
                     IF(DATA_POS == ATL Book Lease Expense) THEN
                        MONTH_VARS(:,Monthly ATL Lease Payment-30) =
     +                       MONTH_VARS(:,Monthly ATL Lease Payment-30)
     +                       + EXPENSES_MONTHLY(:,YR,ASSET_CLASS,
     +                                              DATA_POS,1,SubClass)
     +                       + EXPENSES_MONTHLY(:,YR,ASSET_CLASS,
     +                                              DATA_POS,2,SubClass)
                        CYCLE
                     ENDIF
                     IF(DATA_POS == BTL Amortization) THEN
                        MONTH_VARS(:,Monthly Other BTL Amort-30) =
     +                         MONTH_VARS(:,Monthly Other BTL Amort-30)
     +                         + EXPENSES_MONTHLY(:,YR,ASSET_CLASS,
     +                                              DATA_POS,1,SubClass)
     +                         + EXPENSES_MONTHLY(:,YR,ASSET_CLASS,
     +                                              DATA_POS,2,SubClass)
                        CYCLE
                     ENDIF
                     IF(DATA_POS == Purchased Power) THEN
                        MONTH_VARS(:,DATA_POS)=MONTH_VARS(:,DATA_POS)
     +                            + EXPENSES_MONTHLY(:,YR,ASSET_CLASS,
     +                                              DATA_POS,1,SubClass)
     +                            + EXPENSES_MONTHLY(:,YR,ASSET_CLASS,
     +                                              DATA_POS,2,SubClass)
     +                            + RC_MONTH_PURCHASED_POWER(:)
                        CYCLE
                     ENDIF
                     MONTH_VARS(:,DATA_POS)=MONTH_VARS(:,DATA_POS)
     +                            + EXPENSES_MONTHLY(:,YR,ASSET_CLASS,
     +                                              DATA_POS,1,SubClass)
     +                            + EXPENSES_MONTHLY(:,YR,ASSET_CLASS,
     +                                              DATA_POS,2,SubClass)
                  ENDDO
               ENDIF
            ENDIF
            IF(ASSET_CLASS == -1) THEN
               CALL ELIMINATION_CLASS_ID(ASSET_CLASS)
               IF(ASSET_CLASS < 0) EXIT
            ELSE
               EXIT
            ENDIF
         enddo
         MONTH_VARS(:,Total Nuclear Fuel) =
     +                               MONTH_VARS(:,Owned Nuclear Fuel)
     +                               + MONTH_VARS(:,Leased Nuclear Fuel)
         MONTH_VARS(:,ATL Book Lease Expense) =
     +                              MONTH_VARS(:,ATL Lease Amortization)
     +                              + MONTH_VARS(:,ATL Lease Interest)
         MONTH_VARS(:,BTL Expenses) =
     +                            MONTH_VARS(:,BTL Expenses)
     +                            + MONTH_VARS(:,BTL Lease Amortization)
     +                            + MONTH_VARS(:,BTL Lease Interest)
     +                            + MONTH_VARS(:,BTL Lease Cash)
      RETURN
C***********************************************************************
      ENTRY RETURN_MONTHLY_DERIVATIVE_VALS(R_YR,R_CLASS,
     +                                     INC_MONTH_VARS,
     +                                     CASH_MONTH_VARS)
C***********************************************************************
C
         VOID_INT2 = RETURN_MONTHLY_DERIV_INCOME_VARIABLES(R_CLASS,
     +                                                   INC_MONTH_VARS)
         VOID_INT2 = RETURN_MONTHLY_FUEL_DERIV_INCOME_VARS(R_CLASS,
     +                                                   INC_MONTH_VARS)
         IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
            IF(R_CLASS == 0) THEN
               ASSET_CLASS = 0
            ELSEIF(R_CLASS == -1) THEN
               ASSET_CLASS = -1
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0 .OR. R_CLASS == -1)THEN
C
C THE FOLLOW CALL GETS 12 MONTHS OF PRODUCTION VALUES
C
               YR = R_YR ! - 1
               SubClass = 0
               DO MO = 0, 12
                  DO TAX_TYPE = 1, 2
                     INC_MONTH_VARS(MO,Physical Revenue Variable) =
     +                   INC_MONTH_VARS(MO,Physical Revenue Variable)
     +                   + REVENUES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                 Derivative Physical Rev Variable,
     +                                                TAX_TYPE,SubClass)
                     INC_MONTH_VARS(MO,Physical Revenue Fixed) =
     +                   INC_MONTH_VARS(MO,Physical Revenue Fixed)
     +                   + REVENUES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                    Derivative Physical Rev Fixed,
     +                                                TAX_TYPE,SubClass)
                     INC_MONTH_VARS(MO,Financial Revenue Variable) =
     +                   INC_MONTH_VARS(MO,Financial Revenue Variable)
     +                   + REVENUES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                Derivative Financial Rev Variable,
     +                                                TAX_TYPE,SubClass)
                     INC_MONTH_VARS(MO,Financial Revenue Fixed) =
     +                   INC_MONTH_VARS(MO,Financial Revenue Fixed)
     +                   + REVENUES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                   Derivative Financial Rev Fixed,
     +                                                TAX_TYPE,SubClass)
C
                     INC_MONTH_VARS(MO,Physical Expense Variable) =
     +                  INC_MONTH_VARS(MO,Physical Expense Variable)
     +                  + EXPENSES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                 Derivative Physical Exp Variable,
     +                                                TAX_TYPE,SubClass)
C
                     INC_MONTH_VARS(MO,Physical Expense Fixed) =
     +                  INC_MONTH_VARS(MO,Physical Expense Fixed)
     +                  + EXPENSES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                    Derivative Physical Exp Fixed,
     +                                                TAX_TYPE,SubClass)
C
                     INC_MONTH_VARS(MO,Financial Expense Variable) =
     +                  INC_MONTH_VARS(MO,Financial Expense Variable)
     +                  + EXPENSES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                Derivative Financial Exp Variable,
     +                                                TAX_TYPE,SubClass)
C
                     INC_MONTH_VARS(MO,Financial Expense Fixed) =
     +                  INC_MONTH_VARS(MO,Financial Expense Fixed)
     +                  + EXPENSES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                   Derivative Financial Exp Fixed,
     +                                                TAX_TYPE,SubClass)
C
     +                  + EXPENSES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                   Derivative Financial Exp Fixed,
     +                                                TAX_TYPE,SubClass)
     +                  + EXPENSES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                Derivative Financial Exp Variable,
     +                                                TAX_TYPE,SubClass)
     +                  + EXPENSES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                    Derivative Physical Exp Fixed,
     +                                                TAX_TYPE,SubClass)
     +                  + EXPENSES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                 Derivative Physical Exp Variable,
     +                                                TAX_TYPE,SubClass)
                  ENDDO
C
C CASH
C
                  CASH_MONTH_VARS(MO,Cash Physical Revenue Variable) =
     +                CASH_MONTH_VARS(MO,Cash Physical Revenue Variable)
     +                  + CASH_REV_EXP_MONTHLY(MO,YR,ASSET_CLASS,
     +                                 Derivative Physical Rev Variable)
                  CASH_MONTH_VARS(MO,Cash Physical Revenue Fixed) =
     +                   CASH_MONTH_VARS(MO,Cash Physical Revenue Fixed)
     +                  + CASH_REV_EXP_MONTHLY(MO,YR,ASSET_CLASS,
     +                                    Derivative Physical Rev Fixed)
                  CASH_MONTH_VARS(MO,Cash Physical Expense Variable) =
     +                CASH_MONTH_VARS(MO,Cash Physical Expense Variable)
     +                  + CASH_REV_EXP_MONTHLY(MO,YR,ASSET_CLASS,
     +                                  Cash Derative Financial Exp Var)
                  CASH_MONTH_VARS(MO,Cash Physical Expense Fixed) =
     +                   CASH_MONTH_VARS(MO,Cash Physical Expense Fixed)
     +                  + CASH_REV_EXP_MONTHLY(MO,YR,ASSET_CLASS,
     +                                 Cash Derative Physical Exp Fixed)
                  CASH_MONTH_VARS(MO,Cash Financial Revenue Variable) =
     +               CASH_MONTH_VARS(MO,Cash Financial Revenue Variable)
     +                  + CASH_REV_EXP_MONTHLY(MO,YR,ASSET_CLASS,
     +                                Derivative Financial Rev Variable)
                  CASH_MONTH_VARS(MO,Cash Financial Revenue Fixed) =
     +                  CASH_MONTH_VARS(MO,Cash Financial Revenue Fixed)
     +                  + CASH_REV_EXP_MONTHLY(MO,YR,ASSET_CLASS,
     +                                   Derivative Financial Rev Fixed)
                  CASH_MONTH_VARS(MO,Cash Financial Expense Variable) =
     +               CASH_MONTH_VARS(MO,Cash Financial Expense Variable)
     +                  + CASH_REV_EXP_MONTHLY(MO,YR,ASSET_CLASS,
     +                                Cash Derative Financial Exp Var)
                  CASH_MONTH_VARS(MO,Cash Financial Expense Fixed) =
     +                  CASH_MONTH_VARS(MO,Cash Financial Expense Fixed)
     +                  + CASH_REV_EXP_MONTHLY(MO,YR,ASSET_CLASS,
     +                                Cash Derative Financial Exp Fixed)
C
               ENDDO
            ENDIF
         ENDIF
         DO MO = 0, 12
            INC_MONTH_VARS(MO,Total Derivative Revenues) =
     +                   INC_MONTH_VARS(MO,Total Derivative Revenues)
     +                   + INC_MONTH_VARS(MO,Physical Revenue Variable)
     +                   + INC_MONTH_VARS(MO,Physical Revenue Fixed)
     +                   + INC_MONTH_VARS(MO,Financial Revenue Variable)
     +                   + INC_MONTH_VARS(MO,Financial Revenue Fixed)
            INC_MONTH_VARS(MO,Total Derivative Expenses) =
     +                   INC_MONTH_VARS(MO,Total Derivative Expenses)
     +                   + INC_MONTH_VARS(MO,Physical Expense Variable)
     +                   + INC_MONTH_VARS(MO,Physical Expense Fixed)
     +                   + INC_MONTH_VARS(MO,Financial Expense Variable)
     +                   + INC_MONTH_VARS(MO,Financial Expense Fixed)
            INC_MONTH_VARS(MO,Net Derivative Margin) =
     +                    INC_MONTH_VARS(MO,Total Derivative Revenues)
     +                    - INC_MONTH_VARS(MO,Total Derivative Expenses)
C
            INC_MONTH_VARS(MO,Total Fuel Derivative Revenues) =
     +                 INC_MONTH_VARS(MO,Total Fuel Derivative Revenues)
     +                 + INC_MONTH_VARS(MO,PhysicalFuelDerivRevVar)
     +                 + INC_MONTH_VARS(MO,PhysicalFuelDerivRevFix)
     +                 + INC_MONTH_VARS(MO,Financial Fuel Deriv Rev Var)
     +                 + INC_MONTH_VARS(MO,Financial Fuel Deriv Rev Fix)
            INC_MONTH_VARS(MO,Total Fuel Derivative Expenses) =
     +                 INC_MONTH_VARS(MO,Total Fuel Derivative Expenses)
     +                 + INC_MONTH_VARS(MO,Physical Fuel Deriv Exp Var)
     +                 + INC_MONTH_VARS(MO,Physical Fuel Deriv Exp Fix)
     +                 + INC_MONTH_VARS(MO,Financial Fuel Deriv Exp Var)
     +                 + INC_MONTH_VARS(MO,Financial Fuel Deriv Exp Fix)
            INC_MONTH_VARS(MO,Net Fuel Derivative Margin) =
     +               INC_MONTH_VARS(MO,Total Fuel Derivative Revenues)
     +               - INC_MONTH_VARS(MO,Total Fuel Derivative Expenses)
            INC_MONTH_VARS(MO,Total Power Fuel Derivation Margin) =
     +                     INC_MONTH_VARS(MO,Net Fuel Derivative Margin)
     +                     + INC_MONTH_VARS(MO,Net Derivative Margin)
C
            CASH_MONTH_VARS(MO,Total Cash Derivative Revenues) =
     +             CASH_MONTH_VARS(MO,Total Cash Derivative Revenues)
     +             + CASH_MONTH_VARS(MO,Cash Physical Revenue Variable)
     +             + CASH_MONTH_VARS(MO,Cash Physical Revenue Fixed)
     +             + CASH_MONTH_VARS(MO,Cash Financial Revenue Variable)
     +             + CASH_MONTH_VARS(MO,Cash Financial Revenue Fixed)
C
            CASH_MONTH_VARS(MO,Total Cash Derivative Expenses) =
     +             CASH_MONTH_VARS(MO,Total Cash Derivative Expenses)
     +             + CASH_MONTH_VARS(MO,Cash Physical Expense Variable)
     +             + CASH_MONTH_VARS(MO,Cash Physical Expense Fixed)
     +             + CASH_MONTH_VARS(MO,Cash Financial Expense Variable)
     +             + CASH_MONTH_VARS(MO,Cash Financial Expense Fixed)
C
            CASH_MONTH_VARS(MO,Total Cash Fuel Derivative Revenues) =
     +           CASH_MONTH_VARS(MO,Total Cash Fuel Derivative Revenues)
     +           + CASH_MONTH_VARS(MO,Cash PhysicalFuelDerivRevVar)
     +           + CASH_MONTH_VARS(MO,Cash PhysicalFuelDerivRevFix)
     +           + CASH_MONTH_VARS(MO,Cash Financial Fuel Deriv Rev Var)
     +           + CASH_MONTH_VARS(MO,Cash Financial Fuel Deriv Rev Fix)
C
            CASH_MONTH_VARS(MO,TCFDevExp) =
     +           CASH_MONTH_VARS(MO,TCFDevExp)
     +           + CASH_MONTH_VARS(MO,Cash Physical Fuel Deriv Exp Var)
     +           + CASH_MONTH_VARS(MO,Cash Physical Fuel Deriv Exp Fix)
     +           + CASH_MONTH_VARS(MO,Cash Financial Fuel Deriv Exp Var)
     +           + CASH_MONTH_VARS(MO,Cash Financial Fuel Deriv Exp Fix)
C
         ENDDO
      RETURN
C***********************************************************************
      ENTRY MONTHLY_INTEREST_EXPENSES(R_CLASS,R_YR,
     +                                R_STD_INTEREST_ADJ,
     +                                R_STD_CASH_INTEREST_ADJ,
     +                                R_LTD_INTEREST_ADJ,
     +                                R_LTD_CASH_INTEREST_ADJ)
C***********************************************************************

         IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
            IF(R_CLASS == 0) THEN
               ASSET_CLASS = 0
            ELSEIF(R_CLASS == -1) THEN
               ASSET_CLASS = -1
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0 .OR. R_CLASS == -1)THEN
C
C THE FOLLOW CALL GETS 12 MONTHS OF PRODUCTION VALUES
C
               YR = R_YR ! - 1
               SubClass = 0
               DO MO = 0, 12
                  R_STD_CASH_INTEREST_ADJ(MO) =
     +                           CASH_REV_EXP_MONTHLY(MO,YR,ASSET_CLASS,
     +                                                Cash STD Interest)
                  R_LTD_CASH_INTEREST_ADJ(MO) =
     +                           CASH_REV_EXP_MONTHLY(MO,YR,ASSET_CLASS,
     +                                                Cash LTD Interest)
                  DO TAX_TYPE = 1, 2
                     R_STD_INTEREST_ADJ(MO) =
     +                             R_STD_INTEREST_ADJ(MO)
     +                             + EXPENSES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                                STD Interest,
     +                                                TAX_TYPE,SubClass)
                      R_LTD_INTEREST_ADJ(MO) =
     +                             R_LTD_INTEREST_ADJ(MO)
     +                             + EXPENSES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                                LTD Interest,
     +                                                TAX_TYPE,SubClass)
                  ENDDO
               ENDDO
            ENDIF
         ENDIF
      RETURN
C***********************************************************************
      ENTRY MON_DELTA_RECEIVABLES_PAYABLES(R_YR,R_CLASS,
     +                                  R_MONTHLY_CHANGE_IN_RECEIVABLES,
     +                                  R_MONTHLY_CHANGE_IN_PAYABLES)
C***********************************************************************
C
C
         IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
            IF(R_CLASS <= 0) THEN
               ASSET_CLASS = R_CLASS
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS <= 0) THEN
C
               YR = R_YR ! - 1
               SubClass = 0
C
C BOOK RECEIPTS AND PAYMENTS
C
               DO MO = 0, 12
                  TOTAL_BOOK_REVENUES = 0.
                  DO DATA_POS = Base Rates, Government
                     IF(DATA_POS == Unbilled Revenues) CYCLE
                     IF(DATA_POS == Deferred Revenues) CYCLE
                     TOTAL_BOOK_REVENUES = TOTAL_BOOK_REVENUES
     +                            + REVENUES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                              DATA_POS,1,SubClass)
     +                            + REVENUES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                              DATA_POS,2,SubClass)
                  ENDDO
                  DO TAX_TYPE = 1, 2
                     TOTAL_BOOK_REVENUES = TOTAL_BOOK_REVENUES
     +                   + REVENUES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                Derivative Financial Rev Variable,
     +                                                TAX_TYPE,SubClass)
     +                   + REVENUES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                   Derivative Financial Rev Fixed,
     +                                                TAX_TYPE,SubClass)
     +                   + REVENUES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                    Derivative Physical Rev Fixed,
     +                                                TAX_TYPE,SubClass)
     +                   + REVENUES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                 Derivative Physical Rev Variable,
     +                                                TAX_TYPE,SubClass)
                  ENDDO
C
                  TOTAL_BOOK_EXPENSES = 0.
                  DO DATA_POS = BTL Lease Cash, AG Maintenance
                     IF(DATA_POS == Owned Nuclear Fuel) CYCLE
                     IF(DATA_POS == DOE Decommissioning) CYCLE
                     DO TAX_TYPE = 1, 2
                        TOTAL_BOOK_EXPENSES = TOTAL_BOOK_EXPENSES
     +                            + EXPENSES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                       DATA_POS,TAX_TYPE,SubClass)
                     ENDDO
                  ENDDO
                  DO TAX_TYPE = 1, 2
                     TOTAL_BOOK_EXPENSES = TOTAL_BOOK_EXPENSES
     +                  + EXPENSES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                   Derivative Financial Exp Fixed,
     +                                                TAX_TYPE,SubClass)
     +                  + EXPENSES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                Derivative Financial Exp Variable,
     +                                                TAX_TYPE,SubClass)
     +                  + EXPENSES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                    Derivative Physical Exp Fixed,
     +                                                TAX_TYPE,SubClass)
     +                  + EXPENSES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                 Derivative Physical Exp Variable,
     +                                                TAX_TYPE,SubClass)
                     IF(WVPA()) THEN
                        TOTAL_BOOK_EXPENSES = TOTAL_BOOK_EXPENSES
     +                           + EXPENSES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                            WVPA Member Purchases,
     +                                                TAX_TYPE,SubClass)
     +                           + EXPENSES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                        WVPA Non Member Purchases,
     +                                                TAX_TYPE,SubClass)
     +                           + EXPENSES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                             WVPA Member Services,
     +                                                TAX_TYPE,SubClass)
     +                           + EXPENSES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                         WVPA Non Member Services,
     +                                                TAX_TYPE,SubClass)
                     ENDIF
                  ENDDO
C
C CASH RECEITS AND PAYMENTS
C
                  TOTAL_CASH_RECEPITS = 0.
                  DO DATA_POS = Cash Base Rates, Cash Government
                     IF(DATA_POS == Cash Unbilled Revenues) CYCLE
                     IF(DATA_POS == Cash Deferred Revenues) CYCLE
                     TOTAL_CASH_RECEPITS = TOTAL_CASH_RECEPITS +
     +                  CASH_REV_EXP_MONTHLY(MO,YR,ASSET_CLASS,DATA_POS)
                  ENDDO
                  TOTAL_CASH_RECEPITS = TOTAL_CASH_RECEPITS
     +  + CASH_MONTHLY(MO,YR,ASSET_CLASS,Cash Receipts) ! 3
     +  + CASH_REV_EXP_MONTHLY(MO,YR,ASSET_CLASS,
     +                   Derivative Physical Rev Fixed)
     +  + CASH_REV_EXP_MONTHLY(MO,YR,ASSET_CLASS,
     +                Derivative Physical Rev Variable)
     +  + CASH_REV_EXP_MONTHLY(MO,YR,ASSET_CLASS,
     +                  Derivative Financial Rev Fixed)
     +  + CASH_REV_EXP_MONTHLY(MO,YR,ASSET_CLASS,
     +               Derivative Financial Rev Variable)
C
                  TOTAL_CASH_PAYMENTS = 0.
                  DO DATA_POS = Cash BTL Lease Cash, Cash AG Maintenance
                     TOTAL_CASH_PAYMENTS = TOTAL_CASH_PAYMENTS +
     +                  CASH_REV_EXP_MONTHLY(MO,YR,ASSET_CLASS,DATA_POS)
                  ENDDO
       TOTAL_CASH_PAYMENTS = TOTAL_CASH_PAYMENTS
     +  + CASH_MONTHLY(MO,YR,ASSET_CLASS,Cash Payments) ! 4
     +  + CASH_REV_EXP_MONTHLY(MO,YR,ASSET_CLASS,
     +               Cash Derative Financial Exp Var)
     +  + CASH_REV_EXP_MONTHLY(MO,YR,ASSET_CLASS,
     +               Cash Derative Financial Exp Fixed)
     +  + CASH_REV_EXP_MONTHLY(MO,YR,ASSET_CLASS,
     +                Cash Derative Physical Exp Fixed)
     +  + CASH_REV_EXP_MONTHLY(MO,YR,ASSET_CLASS,
     +                                  Cash Derative Financial Exp Var)
                  IF(WVPA()) THEN
         TOTAL_CASH_PAYMENTS = TOTAL_CASH_PAYMENTS
     +       + CASH_REV_EXP_MONTHLY(MO,YR,ASSET_CLASS,
     +                           Cash WVPA Member Purchases)
     +       + CASH_REV_EXP_MONTHLY(MO,YR,ASSET_CLASS,
     +                       Cash WVPA Non Member Purchases)
     +       + CASH_REV_EXP_MONTHLY(MO,YR,ASSET_CLASS,
     +                            Cash WVPA Member Services)
     +       + CASH_REV_EXP_MONTHLY(MO,YR,ASSET_CLASS,
     +   Cash WVPA Non Member Services)
                  ENDIF
C
C
                  R_MONTHLY_CHANGE_IN_RECEIVABLES(MO) =
     +                               R_MONTHLY_CHANGE_IN_RECEIVABLES(MO)
     +                               + TOTAL_BOOK_REVENUES
     +                               - TOTAL_CASH_RECEPITS
                  R_MONTHLY_CHANGE_IN_PAYABLES(MO) =
     +                                  R_MONTHLY_CHANGE_IN_PAYABLES(MO)
     +                                  + TOTAL_BOOK_EXPENSES
     +                                  - TOTAL_CASH_PAYMENTS
C
               ENDDO ! MONTH LOOP
            ENDIF
         ENDIF
      RETURN
C***********************************************************************
      ENTRY MONTHLY_DEP_AMORT_EXPENSES(R_YR,R_CLASS,
     +                                 R_MONTHLY_BOOK_DEPRECIATION,
     +                                 R_MONTHLY_BOOK_AMORT)
C***********************************************************************
C
C
         IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
            IF(R_CLASS == 0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN
C
C THE FOLLOW CALL GETS 12 MONTHS OF PRODUCTION VALUES
C
               YR = R_YR ! - 1
               SubClass = 0
               DO MO = 0, 12
                  DO TAX_TYPE = 1, 2
                     R_MONTHLY_BOOK_DEPRECIATION(MO) =
     +                    R_MONTHLY_BOOK_DEPRECIATION(MO)
     +                    + EXPENSES_MONTHLY(MO,YR,ASSET_CLASS,
     +                              Book Depreciation,TAX_TYPE,SubClass)
                     R_MONTHLY_BOOK_AMORT(MO) =
     +                    R_MONTHLY_BOOK_AMORT(MO)
     +                    + EXPENSES_MONTHLY(MO,YR,ASSET_CLASS,
     +                                   Amortization,TAX_TYPE,SubClass)
     +                    + EXPENSES_MONTHLY(PERIOD,YR,ASSET_CLASS,
     +                               BTL Amortization,TAX_TYPE,SubClass)
                  ENDDO
               ENDDO
            ENDIF
         ENDIF
      RETURN
      END
C**********************************************************************
      FUNCTION INCOME_STATEMENT_POSITION(R_INCOME_STATEMENT_TITLE)
C**********************************************************************
C
      INTEGER*2 INCOME_STATEMENT_POSITION,FE_INCOME_STATEMENT_POSITION
      CHARACTER*(*) R_INCOME_STATEMENT_TITLE

         IF(INDEX(TRIM(R_INCOME_STATEMENT_TITLE),
     +                                         'Competitive') /= 0) THEN
            INCOME_STATEMENT_POSITION = 21
            IF(INDEX(TRIM(R_INCOME_STATEMENT_TITLE),'1') /= 0)
     +                                    INCOME_STATEMENT_POSITION = 22
            IF(INDEX(TRIM(R_INCOME_STATEMENT_TITLE),'2') /= 0)
     +                                    INCOME_STATEMENT_POSITION = 23
            IF(INDEX(TRIM(R_INCOME_STATEMENT_TITLE),'3') /= 0)
     +                                    INCOME_STATEMENT_POSITION = 24
            IF(INDEX(TRIM(R_INCOME_STATEMENT_TITLE),'4') /= 0)
     +                                    INCOME_STATEMENT_POSITION = 25
            IF(INDEX(TRIM(R_INCOME_STATEMENT_TITLE),'5') /= 0)
     +                                    INCOME_STATEMENT_POSITION = 26
            IF(INDEX(TRIM(R_INCOME_STATEMENT_TITLE),'6') /= 0)
     +                                    INCOME_STATEMENT_POSITION = 27
            IF(INDEX(TRIM(R_INCOME_STATEMENT_TITLE),'7') /= 0)
     +                                    INCOME_STATEMENT_POSITION = 28
            IF(INDEX(TRIM(R_INCOME_STATEMENT_TITLE),'8') /= 0)
     +                                    INCOME_STATEMENT_POSITION = 29
            IF(INDEX(TRIM(R_INCOME_STATEMENT_TITLE),'9') /= 0)
     +                                    INCOME_STATEMENT_POSITION = 30

         ELSEIF(INDEX(TRIM(R_INCOME_STATEMENT_TITLE),
     +                                       'Utility Sales') /= 0) THEN
            INCOME_STATEMENT_POSITION = 20
C
         ELSE
            SELECT CASE (TRIM(R_INCOME_STATEMENT_TITLE))
C
C REGULATED REVENUES
C
         CASE ('Residential','Distribution')
               INCOME_STATEMENT_POSITION = 11  ! V329
         CASE ('Commercial','Transmission','SDI')
               INCOME_STATEMENT_POSITION = 12  ! V330
         CASE ('Industrial','CTC','Stranded')
               INCOME_STATEMENT_POSITION = 13  ! V331
         CASE ('Government','Facilities Group')
               INCOME_STATEMENT_POSITION = 18  ! V382
         CASE ('Lighting','GenerationGross Receipts - CAT Taxes',
     +                                           'Standard Offer','JTS')
               INCOME_STATEMENT_POSITION = 14  ! V332
         CASE ('Unbilled Revenues','Accrued Member Revenues')
               INCOME_STATEMENT_POSITION = 8  ! V326
         CASE ('Base Rates','Chapter 307')
               INCOME_STATEMENT_POSITION = 1  ! V252
C
C NON REGULATED REVENUES
C
         CASE ('Adjustment Clause','Fuel Adjustment Clause')
               INCOME_STATEMENT_POSITION = 2  ! V1
         CASE ('Secondary Sales','Secondary Sale','NonMember',
     +                                                     'Non-Member')
               INCOME_STATEMENT_POSITION = 3  ! V503 totaled in V2
         CASE ('Other Revenue','Other Miscellaneous Revenue')
               INCOME_STATEMENT_POSITION = 4  ! V3
         CASE ('BTL Revenues')
               INCOME_STATEMENT_POSITION = 5  ! V33
         CASE ('Catawba Revenues','PA & NCEMC Revenues','Lighting Fuel')
               INCOME_STATEMENT_POSITION = 6
         CASE ('Gas Revenues','Gas Retail Revenues')
               INCOME_STATEMENT_POSITION = 7  ! V307
         CASE ('Deferred Revenues')
               INCOME_STATEMENT_POSITION = 9  ! V32
         CASE ('Relationship Revenues','Relationship Revenue',
     +         'Customer Revenues','Customer Revenue')
               INCOME_STATEMENT_POSITION = 10  ! V328
         CASE ('Bulk Power')  ! V333
               INCOME_STATEMENT_POSITION = 15
         CASE ('Net of Tax BTL Revenues','Net of Tax BTL Rev')
               INCOME_STATEMENT_POSITION = 16  ! V
         CASE ('Capacity Sales')
               INCOME_STATEMENT_POSITION = 17  ! V329
         CASE ('PGA Clause')
               INCOME_STATEMENT_POSITION = 19  ! V474
         CASE ('Gas Wholesale Revenues')
               INCOME_STATEMENT_POSITION = 31
         CASE ('Physical Variable-Rev')
               INCOME_STATEMENT_POSITION = 32
         CASE ('Physical Fixed-Rev')
               INCOME_STATEMENT_POSITION = 33
         CASE ('Financial Variable-Rev')
               INCOME_STATEMENT_POSITION = 34
         CASE ('Financial Fixed-Rev')
               INCOME_STATEMENT_POSITION = 35
         CASE ('WVPA Memeber Revenues','Member')
               INCOME_STATEMENT_POSITION = 36
C
C EXPENSES POSITIONS
C
         CASE ('BTL Lease-Cash')
            INCOME_STATEMENT_POSITION = 10
         CASE ('Fossil Fuel','1 Fossil Fuel','1')
            INCOME_STATEMENT_POSITION = 11
         CASE ('Purchased Power','2 Purchased Power','2')
            INCOME_STATEMENT_POSITION = 12
         CASE ('Variable O&M','3 Variable O&M','3','Fossil Non-fuel')
            INCOME_STATEMENT_POSITION = 13
         CASE ('Fixed O&M','4 Fixed O&M','4','Nuclear Non-fuel')
            INCOME_STATEMENT_POSITION = 14
         CASE ('Other O&M','5 Other O&M','5')
            INCOME_STATEMENT_POSITION = 15
         CASE ('Purchased Gas','6 Purchased Gas','6')
            INCOME_STATEMENT_POSITION = 16
         CASE ('Other','7 Other','7','GENCO Rental')
            INCOME_STATEMENT_POSITION = 17
         CASE ('Owned Nuclear Fuel','8 Owned Nuclear Fuel','8')
            INCOME_STATEMENT_POSITION = 18
         CASE ('Leased Nuclear Fuel','9 Leased Nuclear Fuel','9')
            INCOME_STATEMENT_POSITION = 19
         CASE ('DSM Expense','10 DSM Expense','10','Salaries & Wages')
            INCOME_STATEMENT_POSITION = 20
         CASE ('DSM Rebate','11 DSM Rebate','11')
            INCOME_STATEMENT_POSITION = 21
         CASE ('Lease Expense','ATL-Lease Cash','12 Lease Expense','12')
            INCOME_STATEMENT_POSITION = 22
         CASE ('Service Transactions','13 Service Transactions','13',
     +         'FSG COGS')
            INCOME_STATEMENT_POSITION = 23
         CASE ('Emission Credits','14 Emission Credits','14')
            INCOME_STATEMENT_POSITION = 24
         CASE ('DOE Decommissioning')
            INCOME_STATEMENT_POSITION = 25
         CASE ('DOE Deposal','DOE Disposal')
            INCOME_STATEMENT_POSITION = 26
         CASE ('Catawba Expenses','PA & NCEMC Expenses','Special',
     +                                              'Regulatory Credit')
            INCOME_STATEMENT_POSITION = 27
         CASE ('BTL Expenses')
            INCOME_STATEMENT_POSITION = 28
         CASE ('Transmission-Operation')
            INCOME_STATEMENT_POSITION = 29
         CASE ('Transmission-Maintenance','ATSI Charges')
            INCOME_STATEMENT_POSITION = 30
         CASE ('Distribution-Operation')
            INCOME_STATEMENT_POSITION = 31
         CASE ('Distribution-Maintenance','O&M Transfers to CAPEX')
            INCOME_STATEMENT_POSITION = 32

         CASE ('Customer Accounts','PIP/Uncollectibles')
            INCOME_STATEMENT_POSITION = 33
         CASE ('Customer Services')
            INCOME_STATEMENT_POSITION = 34
         CASE ('Sales Expense','SMES')
            INCOME_STATEMENT_POSITION = 35
         CASE ('A&G Operations')
            INCOME_STATEMENT_POSITION = 36
         CASE ('A&G Maintenance','Business Support')
            INCOME_STATEMENT_POSITION = 37
         CASE ('Amortization','ATL Amortization')
            INCOME_STATEMENT_POSITION = 38
         CASE ('Deferred Revenue-Amortization')
            INCOME_STATEMENT_POSITION = 39
         CASE ('ATL-Lease Amortization')
            INCOME_STATEMENT_POSITION = 40
         CASE ('BTL Lease-Amortization')
            INCOME_STATEMENT_POSITION = 41
         CASE ('Book Depreciation')
            INCOME_STATEMENT_POSITION = 42
         CASE ('BTL Amortization')
            INCOME_STATEMENT_POSITION = 43
         CASE ('BTL Lease Interest')
            INCOME_STATEMENT_POSITION = 44
         CASE ('ATL Lease Interest')
            INCOME_STATEMENT_POSITION = 45
         CASE ('Deferred Fuel')
            INCOME_STATEMENT_POSITION = 46
         CASE ('Vacation Pay')
            INCOME_STATEMENT_POSITION = 47
         CASE ('Pension Expense','Funded Pension')
            INCOME_STATEMENT_POSITION = 48
         CASE ('Unfunded Pension','Pension/OPEB')
            INCOME_STATEMENT_POSITION = 56
         CASE ('Storm Expense','Turbine Overhaul Expense')
            INCOME_STATEMENT_POSITION = 49
         CASE ('STD Interest')
            INCOME_STATEMENT_POSITION = 50
         CASE ('LTD Interest')
            INCOME_STATEMENT_POSITION = 51
         CASE ('Physical Variable')
            INCOME_STATEMENT_POSITION = 52
         CASE ('Physical Fixed')
            INCOME_STATEMENT_POSITION = 53
         CASE ('Financial Variable')
            INCOME_STATEMENT_POSITION = 54
         CASE ('Financial Fixed')
            INCOME_STATEMENT_POSITION = 55
         CASE ('Purchases Member')
               INCOME_STATEMENT_POSITION = 60
         CASE ('Purchases Non-Member')
               INCOME_STATEMENT_POSITION = 57
         CASE ('Services Member')
               INCOME_STATEMENT_POSITION = 58
         CASE ('Services Non-Member')
               INCOME_STATEMENT_POSITION = 59
         CASE ('Property Taxes in Power Costs')
               INCOME_STATEMENT_POSITION = 61
         CASE ('Property Taxes')
               INCOME_STATEMENT_POSITION = 62
         CASE ('Other Taxes')
               INCOME_STATEMENT_POSITION = 63
         CASE ('Revenue Taxes')
               INCOME_STATEMENT_POSITION = 64
         CASE ('Payroll')
               INCOME_STATEMENT_POSITION = 65
         CASE ('Mark to Market')
               INCOME_STATEMENT_POSITION = 66
         CASE ('AFUDC Equity')
               INCOME_STATEMENT_POSITION = 67
         CASE ('AFUDC Borrowed')
               INCOME_STATEMENT_POSITION = 68
         CASE ('Preferred')
               INCOME_STATEMENT_POSITION = 69
         CASE DEFAULT
            INCOME_STATEMENT_POSITION = 1
         END SELECT
         ENDIF
      RETURN
      END
C**********************************************************************
      FUNCTION CASH_STATEMENT_POSITION(R_CASH_STATEMENT_TITLE)
C**********************************************************************
C
      INTEGER*2 CASH_STATEMENT_POSITION,FE_CASH_STATEMENT_POSITION
      CHARACTER*(*) R_CASH_STATEMENT_TITLE
      LOGICAL*1 EXPENSE_TYPE_IS_CASH
      INTEGER*2 R_EXTYPE
C
         IF(INDEX(TRIM(R_CASH_STATEMENT_TITLE),
     +                                         'Competitive') /= 0) THEN
            CASH_STATEMENT_POSITION = 21
            IF(INDEX(TRIM(R_CASH_STATEMENT_TITLE),'1') /= 0)
     +                                    CASH_STATEMENT_POSITION = 22
            IF(INDEX(TRIM(R_CASH_STATEMENT_TITLE),'2') /= 0)
     +                                    CASH_STATEMENT_POSITION = 23
            IF(INDEX(TRIM(R_CASH_STATEMENT_TITLE),'3') /= 0)
     +                                    CASH_STATEMENT_POSITION = 24
            IF(INDEX(TRIM(R_CASH_STATEMENT_TITLE),'4') /= 0)
     +                                    CASH_STATEMENT_POSITION = 25
            IF(INDEX(TRIM(R_CASH_STATEMENT_TITLE),'5') /= 0)
     +                                    CASH_STATEMENT_POSITION = 26
            IF(INDEX(TRIM(R_CASH_STATEMENT_TITLE),'6') /= 0)
     +                                    CASH_STATEMENT_POSITION = 27
            IF(INDEX(TRIM(R_CASH_STATEMENT_TITLE),'7') /= 0)
     +                                    CASH_STATEMENT_POSITION = 28
            IF(INDEX(TRIM(R_CASH_STATEMENT_TITLE),'8') /= 0)
     +                                    CASH_STATEMENT_POSITION = 29
            IF(INDEX(TRIM(R_CASH_STATEMENT_TITLE),'9') /= 0)
     +                                    CASH_STATEMENT_POSITION = 30

         ELSEIF(INDEX(TRIM(R_CASH_STATEMENT_TITLE),
     +                                       'Utility Sales') /= 0) THEN
            CASH_STATEMENT_POSITION = 20
C
         ELSE
            SELECT CASE (TRIM(R_CASH_STATEMENT_TITLE))
C
C REVENUES MONTHLY AND ANNUAL POSITIONS ARE THE SAME FOR REVENUES
C
            CASE ('Base Rates','Chapter 307')
                  CASH_STATEMENT_POSITION = 1
            CASE ('Adjustment Clause','Fuel Adjustment Clause')
                  CASH_STATEMENT_POSITION = 2
            CASE ('Secondary Sales','NonMember','Non-Member')
                  CASH_STATEMENT_POSITION = 3
            CASE ('Other Revenue')
                  CASH_STATEMENT_POSITION = 4
            CASE ('BTL Revenues')
                  CASH_STATEMENT_POSITION = 5
            CASE ('Catawba Revenues','PA & NCEMC Revenues',
     +                                                  'Lighting Fuel')
                  CASH_STATEMENT_POSITION = 6
            CASE ('Gas Revenues')
                  CASH_STATEMENT_POSITION = 7
            CASE ('Unbilled Revenues','Accrued Member Revenues')
                  CASH_STATEMENT_POSITION = 8
            CASE ('Deferred Revenues')
                  CASH_STATEMENT_POSITION = 9
            CASE ('Relationship Revenues','Relationship Revenue')
                  CASH_STATEMENT_POSITION = 10
            CASE ('Residential','Distribution')
                  CASH_STATEMENT_POSITION = 11
            CASE ('Commercial','Transmission')
                  CASH_STATEMENT_POSITION = 12
            CASE ('Industrial','CTC','Stranded')
                  CASH_STATEMENT_POSITION = 13
            CASE ('Lighting','Generation','Standard Offer')
                  CASH_STATEMENT_POSITION = 14
            CASE ('Bulk Power')
                  CASH_STATEMENT_POSITION = 15
            CASE ('Net of Tax BTL Revenues','Net of Tax BTL Rev')
                  CASH_STATEMENT_POSITION = 16
            CASE ('Capacity Sales')
                  CASH_STATEMENT_POSITION = 17
            CASE ('Government','Facilities Group')
                  CASH_STATEMENT_POSITION = 18
            CASE ('PGA Clause')
                  CASH_STATEMENT_POSITION = 19
            CASE ('Gas Wholesale Revenues')
                  CASH_STATEMENT_POSITION = 31
            CASE ('Physical Variable-Rev')
                  CASH_STATEMENT_POSITION = 32
            CASE ('Physical Fixed-Rev')
                  CASH_STATEMENT_POSITION = 33
            CASE ('Financial Variable-Rev')
                  CASH_STATEMENT_POSITION = 34
            CASE ('Financial Fixed-Rev')
                  CASH_STATEMENT_POSITION = 35
            CASE ('WVPA Memeber Revenues','Member')
                  CASH_STATEMENT_POSITION = 36 ! INCOME POSITION = 36
C
C EXPENSES POSITIONS
C
            CASE ('BTL Lease-Cash')
               CASH_STATEMENT_POSITION = 40 ! INCOME Position=10
            CASE ('Fossil Fuel','1 Fossil Fuel','1')
               CASH_STATEMENT_POSITION = 41 ! INCOME Position=11
            CASE ('Purchased Power','2 Purchased Power','2')
               CASH_STATEMENT_POSITION = 42 ! INCOME Position=12
            CASE ('Variable O&M','3 Variable O&M','3','Fossi Non-fuel')
               CASH_STATEMENT_POSITION = 43 ! INCOME Position=13
            CASE ('Fixed O&M','4 Fixed O&M','4','Nuclear Non-fuel')
               CASH_STATEMENT_POSITION = 44 ! INCOME Position=14
            CASE ('Other O&M','5 Other O&M','5')
               CASH_STATEMENT_POSITION = 45 ! INCOME Position=15
            CASE ('Purchased Gas','6 Purchased Gas','6')
               CASH_STATEMENT_POSITION = 46 ! INCOME Position=16
            CASE ('Other','7 Other','7','GENCO Rental')
               CASH_STATEMENT_POSITION = 47 ! INCOME Position=17
c           CASE ('Owned Nuclear Fuel','8 Owned Nuclear Fuel','8')
c               CASH_STATEMENT_POSITION = 48 ! INCOME Position=18
            CASE ('Leased Nuclear Fuel','9 Leased Nuclear Fuel','9')
               CASH_STATEMENT_POSITION = 49 ! INCOME Position=19
            CASE ('DSM Expense','10 DSM Expense','10',
     +                                               'Salaries & Wages')
               CASH_STATEMENT_POSITION = 50 ! INCOME Position=20
            CASE ('DSM Rebate','11 DSM Rebate','11')
               CASH_STATEMENT_POSITION = 51 ! INCOME Position=21
            CASE ('Lease Expense','ATL-Lease Cash',
     +                                          '12 Lease Expense','12')
               CASH_STATEMENT_POSITION = 52 ! INCOME Position=22
            CASE ('Service Transactions','13 Service Transactions','13',
     +            'FSG COGS')
               CASH_STATEMENT_POSITION = 53 ! INCOME Position=23
            CASE ('Emission Credits','14 Emission Credits','14')
               CASH_STATEMENT_POSITION = 54 ! INCOME Position=24
            CASE ('DOE Decommissioning')
               CASH_STATEMENT_POSITION = -1 
            CASE ('DOE Deposal','DOE Disposal')
               CASH_STATEMENT_POSITION = 56 ! INCOME Position=26
            CASE ('Catawba Expenses','PA & NCEMC Expenses','Special',
     +                                              'Regulatory Credit')
               CASH_STATEMENT_POSITION = 57 ! INCOME Position=27
            CASE ('BTL Expenses')
               CASH_STATEMENT_POSITION = 58 ! INCOME Position=28
            CASE ('Transmission-Operation')
               CASH_STATEMENT_POSITION = 59 ! INCOME Position=29
            CASE ('Transmission-Maintenance','ATSI Charges')
               CASH_STATEMENT_POSITION = 60 ! INCOME Position=30
            CASE ('Distribution-Operation')
               CASH_STATEMENT_POSITION = 61 ! INCOME Position=31
            CASE ('Distribution-Maintenance','O&M Transfers to CAPEX')
               CASH_STATEMENT_POSITION = 62 ! INCOME Position=32
            CASE ('Customer Accounts','PIP/Uncollectibles')
               CASH_STATEMENT_POSITION = 63 ! INCOME Position=33
            CASE ('Customer Services')
               CASH_STATEMENT_POSITION = 64 ! INCOME Position=34
            CASE ('Sales Expense','SMES')
               CASH_STATEMENT_POSITION = 65 ! INCOME Position=35
            CASE ('A&G Operations')
               CASH_STATEMENT_POSITION = 66 ! INCOME Position=36
            CASE ('A&G Maintenance','Business Support')
               CASH_STATEMENT_POSITION = 67 ! INCOME Position=37
            CASE ('BTL Lease Interest')
               CASH_STATEMENT_POSITION = 69 ! INCOME Position=44
            CASE ('ATL Lease Interest')
               CASH_STATEMENT_POSITION = 69 ! INCOME Position=45
c           CASE ('Vacation Pay')
c               CASH_STATEMENT_POSITION = 153 ! INCOME Position=47
            CASE ('Funded Pension','Pension')
               CASH_STATEMENT_POSITION = 68 ! INCOME Position=48
c           CASE ('Storm Expense')
c               CASH_STATEMENT_POSITION = 152 ! INCOME Position=49
            CASE ('STD Interest')
               CASH_STATEMENT_POSITION = 82 ! INCOME Position=50
            CASE ('LTD Interest')
               CASH_STATEMENT_POSITION = 81  ! INCOME Position=51
            CASE ('Physical Variable')
               CASH_STATEMENT_POSITION = 83 ! INCOME Position=52
            CASE ('Physical Fixed')
               CASH_STATEMENT_POSITION = 84 ! INCOME Position=53
            CASE ('Financial Variable')
               CASH_STATEMENT_POSITION = 85 ! INCOME Position=54
            CASE ('Financial Fixed')
               CASH_STATEMENT_POSITION = 86 ! INCOME Position=55
            CASE ('Purchases Member')
               CASH_STATEMENT_POSITION = 87 ! INCOME POSITION = 60
            CASE ('Purchases Non-Member')
               CASH_STATEMENT_POSITION = 88 ! INCOME POSITION = 57
            CASE ('Services Member')
               CASH_STATEMENT_POSITION = 89 ! INCOME POSITION = 58
            CASE ('Services Non-Member')
               CASH_STATEMENT_POSITION = 90 ! INCOME POSITION = 59
            CASE ('Property Taxes in Power Costs')
               CASH_STATEMENT_POSITION = 91 ! INCOME_POSITION = 61
            CASE ('Property Taxes')
               CASH_STATEMENT_POSITION = 92 ! INCOME_POSITION = 62
            CASE ('Other Taxes')
               CASH_STATEMENT_POSITION = 93 ! INCOME_POSITION = 63
            CASE ('Revenue Taxes')
               CASH_STATEMENT_POSITION = 94 ! INCOME_POSITION = 64
            CASE DEFAULT
               CASH_STATEMENT_POSITION = -1
            END SELECT
         ENDIF
      RETURN
C**********************************************************************
      ENTRY EXPENSE_TYPE_IS_CASH(R_EXTYPE)
C**********************************************************************
C
         SELECT CASE (R_EXTYPE)
         CASE (38:43,66:68)
            EXPENSE_TYPE_IS_CASH = .FALSE.
         CASE DEFAULT
            EXPENSE_TYPE_IS_CASH = .TRUE.
         END SELECT
      RETURN
      END
C**********************************************************************
      FUNCTION CASH_POSITION(R_CASH_TITLE)
C**********************************************************************
C
      INTEGER*2 CASH_POSITION
      CHARACTER*(*) R_CASH_TITLE
C
         SELECT CASE (TRIM(R_CASH_TITLE))
C
C REVENUES
C
         CASE ('R')  ! Revenue Receipts = 1
            CASH_POSITION = 1
         CASE ('E')  ! Expense Payments = 2
            CASH_POSITION = 2
         CASE ('I')  ! Cash Receipts = 3
            CASH_POSITION = 3
         CASE ('P')  ! Cash Payments = 4
            CASH_POSITION = 4
         CASE DEFAULT
            CASH_POSITION = 1
         END SELECT
      RETURN
      END
C**********************************************************************
      FUNCTION ACCOUNT_SUB_CLASSIFICATION(R_ACCT_SUB_CLASS)
C**********************************************************************
C
      INTEGER*2 ACCOUNT_SUB_CLASSIFICATION
      CHARACTER*(*) R_ACCT_SUB_CLASS
C
C SUB ACCOUT POSITIONS
C
c         ACCOUNT_SUB_CLASSIFICATION = 6 
         ACCOUNT_SUB_CLASSIFICATION = 0
         IF(INDEX(TRIM(R_ACCT_SUB_CLASS),'1') /= 0)
     +                                    ACCOUNT_SUB_CLASSIFICATION = 1
         IF(INDEX(TRIM(R_ACCT_SUB_CLASS),'2') /= 0)
     +                                    ACCOUNT_SUB_CLASSIFICATION = 2
         IF(INDEX(TRIM(R_ACCT_SUB_CLASS),'3') /= 0)
     +                                    ACCOUNT_SUB_CLASSIFICATION = 3
         IF(INDEX(TRIM(R_ACCT_SUB_CLASS),'4') /= 0)
     +                                    ACCOUNT_SUB_CLASSIFICATION = 4
         IF(INDEX(TRIM(R_ACCT_SUB_CLASS),'5') /= 0)
     +                                    ACCOUNT_SUB_CLASSIFICATION = 5
      RETURN
      END
C**********************************************************************
      SUBROUTINE MONTHLY_BOOK_VALUES_NO_ADD(ANNUAL_VALUES,
     +  MONTHLY_VALUES, 
     +  MONTHLY_DATA_UNITS, ! 5 VALUES
     +  MONTH_ENDING,   ! 5 VALUES
     +  R_READ_THE_ANNUAL_SUM)
C**********************************************************************
C
       use SpinDriftLib
      use prod_arrays_dimensions
      USE PROD_ARRAYS_DIMENSIONS
      use sizecom
      use globecom
      REAL*4 ANNUAL_VALUES(LAST_AVAILABLE_MONTHLY_YEAR),
     +       MONTHLY_VALUES(12,LAST_AVAILABLE_MONTHLY_YEAR),AMOUNT
      INTEGER*2 MO,YR,MONTH_END
      CHARACTER*1 MONTHLY_DATA_UNITS(LAST_AVAILABLE_MONTHLY_YEAR),
     +            ANNUAL_VALUE_STATUS(LAST_AVAILABLE_MONTHLY_YEAR)
      INTEGER*2 MONTH_ENDING(LAST_AVAILABLE_MONTHLY_YEAR)
      REAL*4 UNDISTRIBUTED_BALANCE
      LOGICAL*1 RESUM_ANNUAL(LAST_AVAILABLE_MONTHLY_YEAR),
     +          READ_THE_ANNUAL_SUM/.TRUE./,
     +          R_READ_THE_ANNUAL_SUM,KCPL
      CHARACTER*1 DOLLARS,PERCENT,NORMALIZE,AVERAGE,TREND,LAST_MONTH,
     +            USE_ANNUAL_VALUES,RATE,TRENDED_RATE,VALUES,
     +            END_OF_PERIOD_VALUE
      PARAMETER(DOLLARS='D',PERCENT='P',NORMALIZE='N',
     +          AVERAGE='A',TREND='T',LAST_MONTH='L',VALUES='V',
     +          USE_ANNUAL_VALUES='U',RATE='R',TRENDED_RATE='E',
     +          END_OF_PERIOD_VALUE='S')
      REAL*4 TEMP_TREND_NORM,TREND_NORM
      REAL*4 MONTHLY_DATA_OUT(0:12,0:*),
     +       ANNUAL_DATA_IN(*),
     +       MONTHLY_DATA_IN(1:12,*)
      INTEGER*2 TREND_YEAR,SIMULATION_PERIOD
      LOGICAL*1 TREND_FOUND
C
         READ_THE_ANNUAL_SUM = R_READ_THE_ANNUAL_SUM
C
C**********************************************************************
      ENTRY MONTHLY_ACTUAL_VALUES(ANNUAL_VALUES,
     +                            MONTHLY_VALUES, 
     +                            MONTHLY_DATA_UNITS, ! 5 VALUES
     +                            MONTH_ENDING,   ! 5 VALUES
     +                            ANNUAL_VALUE_STATUS)
C**********************************************************************
C
         DO YR = 1, LAST_AVAILABLE_MONTHLY_YEAR
            IF(MONTHLY_DATA_UNITS(YR) == LAST_MONTH .AND.
     +                              ANNUAL_VALUE_STATUS(YR) /= 'N') THEN
               MONTH_END = MONTH_ENDING(YR)
               IF(MONTH_END == 12) THEN
                  ANNUAL_VALUES(YR) = SUM(MONTHLY_VALUES(:,YR))
               ELSE
                  IF(ANNUAL_VALUE_STATUS(YR) /= 'I')
     +                                       ANNUAL_VALUES(YR) = -99999.
                  DO MO = MONTH_END+1, 12
                     MONTHLY_VALUES(MO,YR) = -99999.
                  ENDDO
               ENDIF
            ELSE
               IF(ANNUAL_VALUE_STATUS(YR) == 'N') THEN
                  ANNUAL_VALUES(YR) = -99999.
                  MONTHLY_VALUES(:,YR) = -99999.
               ELSE
                  IF(MONTHLY_DATA_UNITS(YR) == DOLLARS) THEN
                     ANNUAL_VALUES(YR) = SUM(MONTHLY_VALUES(:,YR))
                  ELSE ! USE AVERAGE FOR NOW 1/19/00
                     MONTHLY_VALUES(:,YR) = ANNUAL_VALUES(YR)/12.
                  ENDIF
               ENDIF
            ENDIF
         ENDDO
      RETURN
C**********************************************************************
      ENTRY MONTHLY_BOOK_VALUES_IN_DOLLARS(ANNUAL_VALUES,
     +  MONTHLY_VALUES, !((MIDAS_MONTHLY_BOOKED(MO,YR),MO=12),YR=1,5),
     +  MONTHLY_DATA_UNITS, ! 5 VALUES
     +  MONTH_ENDING)   ! 5 VALUES
C**********************************************************************
C

         DO YR = 1, LAST_AVAILABLE_MONTHLY_YEAR
            RESUM_ANNUAL(YR) = .TRUE.
            IF(MONTHLY_DATA_UNITS(YR) == DOLLARS) THEN
               RESUM_ANNUAL(YR) = READ_THE_ANNUAL_SUM
               CYCLE
            ENDIF
            IF(MONTHLY_DATA_UNITS(YR) == RATE) THEN
               RESUM_ANNUAL(YR) = .FALSE.
               CYCLE
            ENDIF
            IF(MONTHLY_DATA_UNITS(YR) == END_OF_PERIOD_VALUE) THEN
               RESUM_ANNUAL(YR) = .FALSE.
               ANNUAL_VALUES(YR) = MONTHLY_VALUES(12,YR)
               CYCLE
            ENDIF
            IF(MONTHLY_DATA_UNITS(YR) == VALUES) THEN
               RESUM_ANNUAL(YR) = .FALSE.
               CYCLE
            ENDIF
            IF(MONTHLY_DATA_UNITS(YR) == USE_ANNUAL_VALUES) THEN
               RESUM_ANNUAL(YR) = .FALSE.
               AMOUNT = ANNUAL_VALUES(YR)
               DO MO = 1, 12
                  MONTHLY_VALUES(MO,YR) = AMOUNT
               ENDDO
               CYCLE
            ENDIF
            IF(MONTHLY_DATA_UNITS(YR) == PERCENT) THEN
               RESUM_ANNUAL(YR) = READ_THE_ANNUAL_SUM
               AMOUNT = ANNUAL_VALUES(YR)/100.
C              ANNUAL_VALUES(YR) = 0.
               DO MO = 1, 12
                  MONTHLY_VALUES(MO,YR) = AMOUNT * MONTHLY_VALUES(MO,YR)
C                 ANNUAL_VALUES(YR) = ANNUAL_VALUES(YR) +
C    +                                             MONTHLY_VALUES(MO,YR)
               ENDDO
               CYCLE
            ENDIF
            IF(MONTHLY_DATA_UNITS(YR) == AVERAGE) THEN
               AMOUNT = ANNUAL_VALUES(YR)/12.
               RESUM_ANNUAL(YR) = .FALSE.
               DO MO = 1, 12
                  MONTHLY_VALUES(MO,YR) = AMOUNT
               ENDDO
               CYCLE
            ENDIF
            IF(MONTHLY_DATA_UNITS(YR) == LAST_MONTH) THEN
               RESUM_ANNUAL(YR) = .FALSE.
               AMOUNT = 0.
               IF(KCPL()) THEN
                  MONTH_END = 11
               ELSE
                  MONTH_END = MONTH_ENDING(YR)
               ENDIF
               DO MO = 1, MONTH_END
                  AMOUNT = AMOUNT + MONTHLY_VALUES(MO,YR)
               ENDDO
               IF(MONTH_END < 12) THEN
                  UNDISTRIBUTED_BALANCE = ANNUAL_VALUES(YR) - AMOUNT
                  AMOUNT = 0.
                  DO MO = MONTH_END+1,12
                     AMOUNT = AMOUNT + MONTHLY_VALUES(MO,YR)
                  ENDDO
                  IF(AMOUNT /= 0.) THEN
                     AMOUNT = UNDISTRIBUTED_BALANCE/AMOUNT
                     DO MO = MONTH_END+1,12
                        MONTHLY_VALUES(MO,YR) = AMOUNT *
     +                                             MONTHLY_VALUES(MO,YR)
                     ENDDO
                  ELSE
                     AMOUNT = UNDISTRIBUTED_BALANCE/(12-MONTH_END)
                     DO MO = MONTH_END+1,12
                        MONTHLY_VALUES(MO,YR) = AMOUNT
                     ENDDO
                  ENDIF
               ELSE
                  ANNUAL_VALUES(YR) = AMOUNT
               ENDIF
               CYCLE
            ENDIF
            IF(MONTHLY_DATA_UNITS(YR) == NORMALIZE) THEN
               AMOUNT = 0.
               DO MO = 1, 12
                  AMOUNT = AMOUNT + MONTHLY_VALUES(MO,YR)
               ENDDO
               IF(AMOUNT /= 0.) THEN
                  AMOUNT = ANNUAL_VALUES(YR)/AMOUNT
                  DO MO = 1,12
                     MONTHLY_VALUES(MO,YR) = AMOUNT *
     +                                             MONTHLY_VALUES(MO,YR)
                  ENDDO
               ELSE
                  AMOUNT = ANNUAL_VALUES(YR)/12.
                  DO MO = 1,12
                     MONTHLY_VALUES(MO,YR) = AMOUNT
                  ENDDO
               ENDIF
               CYCLE
            ENDIF
            IF(MONTHLY_DATA_UNITS(YR) == TREND .AND. YR > 1) THEN
               AMOUNT = 0.
               DO MO = 1, 12
                  AMOUNT = AMOUNT + MONTHLY_VALUES(MO,YR-1)
               ENDDO
               IF(AMOUNT /= 0.) THEN
                  AMOUNT = ANNUAL_VALUES(YR)/AMOUNT
                  DO MO = 1, 12
                     MONTHLY_VALUES(MO,YR) = AMOUNT *
     +                                           MONTHLY_VALUES(MO,YR-1)
                  ENDDO
               ELSE
                  AMOUNT = ANNUAL_VALUES(YR)/12.
                  DO MO = 1, 12
                     MONTHLY_VALUES(MO,YR) = AMOUNT
                  ENDDO
               ENDIF
               CYCLE
            ENDIF
            IF(MONTHLY_DATA_UNITS(YR) == TRENDED_RATE .AND. YR > 1) THEN
               RESUM_ANNUAL(YR) = .FALSE.
               IF(ANNUAL_VALUES(YR-1) /= 0.) THEN
                  AMOUNT = ANNUAL_VALUES(YR)/ANNUAL_VALUES(YR-1)
                  DO MO = 1, 12
                     MONTHLY_VALUES(MO,YR) = AMOUNT *
     +                                           MONTHLY_VALUES(MO,YR-1)
                  ENDDO
               ELSE
                  AMOUNT = ANNUAL_VALUES(YR)
                  DO MO = 1, 12
                     MONTHLY_VALUES(MO,YR) = AMOUNT
                  ENDDO
               ENDIF
               CYCLE
            ENDIF
         ENDDO
         DO YR = 1, LAST_AVAILABLE_MONTHLY_YEAR
            MONTHLY_DATA_UNITS(YR) = DOLLARS
            IF(RESUM_ANNUAL(YR)) THEN
C               AMOUNT = 0
C               DO MO = 1, 12
C                  AMOUNT = AMOUNT + MONTHLY_VALUES(MO,YR)
C               ENDDO
               ANNUAL_VALUES(YR) = SUM(MONTHLY_VALUES(1:12,YR))
            ENDIF
         ENDDO
         READ_THE_ANNUAL_SUM = .TRUE.
      RETURN
C**********************************************************************
      ENTRY TRANSFER_VECTOR_VALUES_2(MONTHLY_DATA_OUT,
     +                               ANNUAL_DATA_IN,
     +                               MONTHLY_DATA_IN,
     +                               SIMULATION_PERIOD)
C**********************************************************************
C USED TO TRANSFER VECTOR VALUES TO SINGLE MONTHLY ARRAYS
C THIS WORKS WITH ARRAYS THAT HAVE THE BASE YEAR AS THE ZERO YEAR
C ON THE OUT SIDE
C**********************************************************************
C
C
         TREND_NORM = 1.
         TREND_YEAR = 1
         TREND_FOUND = .FALSE.
         DO YR = 1, SIMULATION_PERIOD
            TEMP_TREND_NORM = 0.
            IF(YR <= AVAIL_DATA_YEARS) THEN
               MONTHLY_DATA_OUT(0,YR) = ANNUAL_DATA_IN(YR)
            ELSE
               MONTHLY_DATA_OUT(0,YR) = ANNUAL_DATA_IN(AVAIL_DATA_YEARS)
            ENDIF
            DO MO = 1, 12
               IF(YR <= LAST_AVAILABLE_MONTHLY_YEAR) THEN
                  MONTHLY_DATA_OUT(MO,YR) = MONTHLY_DATA_IN(MO,YR)
               ELSEIF(TREND_FOUND) THEN
                  MONTHLY_DATA_OUT(MO,YR) = MONTHLY_DATA_OUT(0,YR) *
     +                                   MONTHLY_DATA_IN(MO,TREND_YEAR)/
     +                                           TREND_NORM
               ELSE
                  MONTHLY_DATA_OUT(MO,YR) = MONTHLY_DATA_OUT(0,YR)/12.
               ENDIF
               IF(YR <= LAST_AVAILABLE_MONTHLY_YEAR) THEN
                  TEMP_TREND_NORM = TEMP_TREND_NORM
     +                              + MONTHLY_DATA_IN(MO,YR)
                  IF(TEMP_TREND_NORM /= 0.) THEN
                     TREND_FOUND = .TRUE.
                     TREND_YEAR = YR
                     TREND_NORM = TEMP_TREND_NORM
                  ENDIF
               ENDIF
            ENDDO
         ENDDO
      RETURN
      END
C**********************************************************************
      SUBROUTINE MONTHLY_CASH_VALUES_IN_DOLLARS(ANNUAL_BOOKED,
     +                                       MONTHLY_BOOKED,
     +                                       MONTHLY_CASH,
     +                                       ANNUAL_CASH,
     +                                       CASH_TYPE,
     +                                       CASH_VALUES_VECTOR,
     +                                       CASH_ACCRUAL_AMOUNT,
     +                                       CASH_LAG_PATTERN,
     +                                       MONTHLY_CASH_ACTUAL,
     +                                       MONTHLY_CASH_VALUES_TYPE,
     +                                       FINANCIAL_SIMULATION_YEARS,
     +                                       PROPERTY_TAX_RECORD)
C**********************************************************************
C
       use SpinDriftLib
      use prod_arrays_dimensions
      USE PROD_ARRAYS_DIMENSIONS
      use sizecom
      use globecom
      INCLUDE 'mthnmcom.mon'
      INTEGER*2 FINANCIAL_SIMULATION_YEARS
      INTEGER*2 LAST_MONTH(LAST_AVAILABLE_MONTHLY_YEAR)
      REAL*4 ANNUAL_BOOKED(0:FINANCIAL_SIMULATION_YEARS),
     +       MONTHLY_BOOKED(12,LAST_AVAILABLE_MONTHLY_YEAR),AMOUNT,
     +       MONTHLY_CASH(12,LAST_AVAILABLE_MONTHLY_YEAR),
     +       ANNUAL_CASH(0:FINANCIAL_SIMULATION_YEARS), 
     +       FIVE_YR_CASH_DISTRIBUTION(12*LAST_AVAILABLE_MONTHLY_YEAR),
     +       FIVE_YR_BOOK_DISTRIBUTION(12*LAST_AVAILABLE_MONTHLY_YEAR)
      REAL MONTHLY_CASH_ACTUAL(12,2)   ! 198-221
      CHARACTER*10 MONTHLY_CASH_VALUES_TYPE(2) ! 222-223
      CHARACTER*1 MONTHLY_DATA_UNITS(LAST_AVAILABLE_MONTHLY_YEAR),
     +            CASH_TYPE
      INTEGER*2 MO,YR,CASH_VECTOR,MO1,LAG_MO
      INTEGER*2 CASH_LAST_MONTH,GET_MONTH_NUMBER
      REAL*4 CASH_VALUES_VECTOR
      CHARACTER*1 LAGGED,BOOKED,VECTOR,NOCASH
      PARAMETER (LAGGED='L',BOOKED='B',VECTOR='V',NOCASH='N')
      REAL*4 CASH_VECTOR_VALUES(0:30)
      CHARACTER*1 DATA_TYPE,VECTOR_TYPE*30,CASH_ACCRUAL_AMOUNT*30,
     +            LAGGED_PATTERN*40,CASH_LAG_PATTERN*30
      REAL*4 LAG_BY_MONTH(13),
     +       TOTAL_BOOKED_AMOUNT,TOTAL_CASH_AMOUNT,
     +       ACCRUED_CASH_BY_MONTH(12),
     +       ANNUAL_CASH_CARRYOVER,
     +       RATIO_CASH_2_BOOK
      LOGICAL (KIND=1) :: PROPERTY_TAX_RECORD,
     +                    END_ACCUMULATION_PERIOD(12),
     +                    ACC_PERIOD(12),
     +                    DONT_LAG_DECEMBER
      INTEGER (KIND=2) :: LAG_PERIOD,GET_ACCUMULATION_PERIOD_INFO
      REAL (KIND=4) :: ACCUMULATED_VALUES
      LOGICAL*1 GreatRiver
C
         IF(CASH_TYPE == VECTOR) THEN
            CASH_VECTOR = INT2(CASH_VALUES_VECTOR)
            CALL GET_MONTHLY_ANNUAL_VALUES(CASH_VECTOR,
     +                                     DATA_TYPE,
     +                                     VECTOR_TYPE,
     +                                     CASH_VECTOR_VALUES(1),
     +                                     MONTHLY_CASH(1,1),
     +                                     MONTHLY_DATA_UNITS,
     +                                     LAST_MONTH)
            CALL RIPPLE_MONTHLY_DATA(CASH_VECTOR_VALUES(1),MONTHLY_CASH)
C
            CALL MONTHLY_BOOK_VALUES_IN_DOLLARS(CASH_VECTOR_VALUES(1),
     +                                          MONTHLY_CASH,
     +                                          MONTHLY_DATA_UNITS,
     +                                          LAST_MONTH)
            DO YR = 1, FINANCIAL_SIMULATION_YEARS
               IF(YR <= AVAIL_DATA_YEARS) THEN
                  ANNUAL_CASH(YR) = CASH_VECTOR_VALUES(YR)
               ELSE
                  ANNUAL_CASH(YR) = CASH_VECTOR_VALUES(AVAIL_DATA_YEARS)
               ENDIF
            ENDDO
         ELSEIF(PROPERTY_TAX_RECORD) THEN 
            LAG_PERIOD = GET_ACCUMULATION_PERIOD_INFO(
     +                                          payment_property_taxes,
     +                                          DONT_LAG_DECEMBER,
     +                                          END_ACCUMULATION_PERIOD)
            MONTHLY_CASH = 0.
            ACCUMULATED_VALUES = 0.
            DO YR = 1, LAST_AVAILABLE_MONTHLY_YEAR
               DO MO = 1, 12
                  ACCUMULATED_VALUES = ACCUMULATED_VALUES
     +                                 + MONTHLY_BOOKED(MO,YR)
                  IF(END_ACCUMULATION_PERIOD(MO)) THEN
                     IF(MO == 12 .AND. DONT_LAG_DECEMBER) THEN
                        MONTHLY_CASH(MO,YR) = ACCUMULATED_VALUES
                     ELSEIF(MO + LAG_PERIOD <= 12) THEN
                        MONTHLY_CASH(MO,YR) = ACCUMULATED_VALUES
                     ELSEIF(YR < LAST_AVAILABLE_MONTHLY_YEAR) THEN
                        MONTHLY_CASH(MO+LAG_PERIOD-12,YR+1) =
     +                               MONTHLY_CASH(MO+LAG_PERIOD-12,YR+1)
     +                               + ACCUMULATED_VALUES
                     ENDIF
                     ACCUMULATED_VALUES = 0.
                  ENDIF
               ENDDO
               ANNUAL_CASH(YR) = SUM(MONTHLY_CASH(1:,YR))
            ENDDO
         ELSEIF(CASH_TYPE == NOCASH) THEN
            MONTHLY_CASH = 0.
            ANNUAL_CASH = 0.
         ELSEIF(CASH_TYPE == BOOKED) THEN
            MONTHLY_CASH = MONTHLY_BOOKED
            ANNUAL_CASH = ANNUAL_BOOKED
         ELSEIF(CASH_TYPE == LAGGED) THEN
            LAG_BY_MONTH = 0.

               LAGGED_PATTERN=TRIM(CASH_LAG_PATTERN)//',,,,,,,,,,,,,,,,'
               READ(LAGGED_PATTERN,*) LAG_BY_MONTH
               LAG_BY_MONTH  = LAG_BY_MONTH/100.

            LAG_BY_MONTH(13) = 1. - SUM(LAG_BY_MONTH(1:12))
            DO YR = 1, LAST_AVAILABLE_MONTHLY_YEAR
               DO MO = 1, 12
                  FIVE_YR_BOOK_DISTRIBUTION(12*(YR-1)+MO) =
     +                                             MONTHLY_BOOKED(MO,YR)
               ENDDO
            ENDDO
            FIVE_YR_CASH_DISTRIBUTION = 0.
            DO MO = 1, 12*LAST_AVAILABLE_MONTHLY_YEAR
               LAG_MO = 1
               DO MO1 = MO, MIN(MO + 11,12*LAST_AVAILABLE_MONTHLY_YEAR)
                  FIVE_YR_CASH_DISTRIBUTION(MO1) =
     +                                   FIVE_YR_CASH_DISTRIBUTION(MO1)
     +                                   + FIVE_YR_BOOK_DISTRIBUTION(MO)
     +                                            * LAG_BY_MONTH(LAG_MO)
                  LAG_MO = LAG_MO + 1
               ENDDO
               IF(MO1 <= 12*LAST_AVAILABLE_MONTHLY_YEAR) THEN
                  FIVE_YR_CASH_DISTRIBUTION(MO1) =
     +                                   FIVE_YR_CASH_DISTRIBUTION(MO1)
     +                                   + FIVE_YR_BOOK_DISTRIBUTION(MO)
     +                                            * LAG_BY_MONTH(LAG_MO)
               ENDIF
            ENDDO
            TOTAL_BOOKED_AMOUNT = 0.
            TOTAL_CASH_AMOUNT = 0.
            DO YR = 1, LAST_AVAILABLE_MONTHLY_YEAR
               DO MO = 1, 12
                  TOTAL_BOOKED_AMOUNT = TOTAL_BOOKED_AMOUNT
     +                                  + MONTHLY_BOOKED(MO,YR)
                  MONTHLY_CASH(MO,YR) =
     +                           FIVE_YR_CASH_DISTRIBUTION(12*(YR-1)+MO)
                  TOTAL_CASH_AMOUNT = TOTAL_CASH_AMOUNT
     +                                + MONTHLY_CASH(MO,YR)
               ENDDO
               ANNUAL_CASH(YR) = SUM(MONTHLY_CASH(1:,YR))
            ENDDO
            ANNUAL_CASH_CARRYOVER = TOTAL_BOOKED_AMOUNT
     +                              - TOTAL_CASH_AMOUNT
            IF(ABS(TOTAL_BOOKED_AMOUNT) < .0001) THEN
               RATIO_CASH_2_BOOK = TOTAL_CASH_AMOUNT/
     +                            TOTAL_BOOKED_AMOUNT
               DO YR = LAST_AVAILABLE_MONTHLY_YEAR+1,
     +                                        FINANCIAL_SIMULATION_YEARS
                  ANNUAL_CASH(YR) = RATIO_CASH_2_BOOK*ANNUAL_BOOKED(YR)
     +                             + ANNUAL_CASH_CARRYOVER
                  ANNUAL_CASH_CARRYOVER =
     +                        (1.-RATIO_CASH_2_BOOK) * ANNUAL_BOOKED(YR)
               ENDDO
            ELSE
               DO YR = LAST_AVAILABLE_MONTHLY_YEAR+1,
     +                                        FINANCIAL_SIMULATION_YEARS
                  ANNUAL_CASH(YR) = ANNUAL_BOOKED(YR)
               ENDDO
            ENDIF
         ENDIF
C
C OVER RIDE FOR THE FIRST TWO YEARS OF CASH
C
         IF(INDEX(MONTHLY_CASH_VALUES_TYPE(1),'Not') == 0) THEN
            CASH_LAST_MONTH =
     +                     GET_MONTH_NUMBER(MONTHLY_CASH_VALUES_TYPE(1))
            IF(CASH_LAST_MONTH /= -1) THEN
               MONTHLY_CASH(1:CASH_LAST_MONTH,1) =
     +                          MONTHLY_CASH_ACTUAL(1:CASH_LAST_MONTH,1)
               ANNUAL_CASH(1) = SUM(MONTHLY_CASH(:,1))
            ENDIF
            IF(CASH_LAST_MONTH == 12 .AND.
     +               INDEX(MONTHLY_CASH_VALUES_TYPE(2),'Not') == 0) THEN
               CASH_LAST_MONTH =
     +                     GET_MONTH_NUMBER(MONTHLY_CASH_VALUES_TYPE(2))
               IF(CASH_LAST_MONTH /= -1) THEN
                  MONTHLY_CASH(1:CASH_LAST_MONTH,2) =
     +                          MONTHLY_CASH_ACTUAL(1:CASH_LAST_MONTH,2)
                  ANNUAL_CASH(2) = SUM(MONTHLY_CASH(:,2))
               ENDIF
            ENDIF
         ENDIF
C
C ADD BASE YEAR PAYMENTS
C
         LAGGED_PATTERN =
     +               TRIM(CASH_ACCRUAL_AMOUNT)//',,,,,,,,,,,,,,,,,,,,'
         READ(LAGGED_PATTERN,*) ACCRUED_CASH_BY_MONTH
         MONTHLY_CASH(:,1) = MONTHLY_CASH(:,1)
     +                           + ACCRUED_CASH_BY_MONTH(:)
         ANNUAL_CASH(1) = ANNUAL_CASH(1)
     +                       + SUM(ACCRUED_CASH_BY_MONTH)
      RETURN
      END
C***********************************************************************
      SUBROUTINE MAP_LAST_MONTH(MONTHLY_LAST_MONTH_STR,
     +                          MONTHLY_LAST_MONTH_INT)
C***********************************************************************
C
      use globecom
      INTEGER*2 YR,MONTHLY_LAST_MONTH_INT(LAST_AVAILABLE_MONTHLY_YEAR)
      CHARACTER*4 MONTHLY_LAST_MONTH_STR(LAST_AVAILABLE_MONTHLY_YEAR)
      INTEGER*2 GET_MONTH_NUMBER
C
            DO YR = 1, LAST_AVAILABLE_MONTHLY_YEAR
               MONTHLY_LAST_MONTH_INT(YR) =
     +                      GET_MONTH_NUMBER(MONTHLY_LAST_MONTH_STR(YR))
               IF(MONTHLY_LAST_MONTH_INT(YR) == -1)
     +                                   MONTHLY_LAST_MONTH_INT(YR) = 12
            ENDDO
      RETURN
      END
C***********************************************************************
      SUBROUTINE LAG_PRODUCTION_CASH_EXPENSES(R_CLASS,
     +                                        BOOKED_VALUES,
     +                                        MONTH_VARS)
C***********************************************************************
      INCLUDE 'mthnmcom.mon'
      REAL (KIND=4) BOOKED_VALUES(0:12,CASH_VARS),
     +              MONTH_VARS(0:12,CASH_VARS)
      REAL (KIND=4), SAVE :: CASH_LAG_VALUES(0:24,CASH_VARS)=0.
      REAL (KIND=4), ALLOCATABLE, SAVE :: ANNUAL_CASH_ROLL_OVER(:,:,:)
      INTEGER*2 R_CLASS,PARENT_CLASS_LOC
      INTEGER*2, SAVE :: CLASS_POS=0,LAG_PERIOD
      INTEGER*2 NUM_OF_ASSET_CLASSES,MAX_ASSET_CLASS_NUM,MO
      REAL (KIND=4), SAVE :: CASH_EXPENSES(0:12),BOOK_EXPENSES(0:12),
     +                       CASH_REVENUES(0:12),BOOKED_REVENUES(0:12),
     +                       ACTUAL_CASH_VALUES(12,6),
     +                       FUEL_LAG_ALLOCATORS(1:12)
     +                          /.33,.67,0.,0.,0.,0.,0.,0.,0.,0.,0.,0./
      REAL (KIND=4) :: R_CHANGE_IN_RECEIVABLES,
     +                 R_CHANGE_IN_PAYABLES,
     +                 R_MONTHLY_CHANGE_IN_RECEIVABLES(0:12),
     +                 R_MONTHLY_CHANGE_IN_PAYABLES(0:12),
     +                 TEMP_LAG_CASH(1:24)
      LOGICAL*1 WVPA,IPALCO
C
         CLASS_POS = R_CLASS
         CASH_LAG_VALUES = 0.
         IF(LAG_PERIOD == 1) THEN
            CASH_LAG_VALUES(2:12,:) = BOOKED_VALUES(1:11,:)
            CASH_LAG_VALUES(13,:) = BOOKED_VALUES(12,:)
         ELSEIF(LAG_PERIOD == 2) THEN
            CASH_LAG_VALUES(13:14,:) = BOOKED_VALUES(11:12,:)
         ELSE
            CASH_LAG_VALUES(1:12,:) = BOOKED_VALUES(1:12,:)
         ENDIF
         IF(IPALCO()) THEN
            TEMP_LAG_CASH = 0.
            DO MO = 1, 12
               TEMP_LAG_CASH(MO:MO+11) = TEMP_LAG_CASH(MO:MO+11)
     +                           + FUEL_LAG_ALLOCATORS(1:12)*
     +                             BOOKED_VALUES(MO,Monthly Fossil Fuel)
            ENDDO
            CASH_LAG_VALUES(1:24,Monthly Fossil Fuel) =
     +                                               TEMP_LAG_CASH(1:24)
         ENDIF
         CASH_LAG_VALUES(1:12,:) = CASH_LAG_VALUES(1:12,:)
     +                           + ANNUAL_CASH_ROLL_OVER(1:12,:,R_CLASS)
         CASH_LAG_VALUES(0,:) = SUM(CASH_LAG_VALUES(1:12,:),DIM=1)
C
C CL AND EL REVENUES
C
         MONTH_VARS(:,cash_adjustment_clause) =
     +            MONTH_VARS(:,cash_adjustment_clause)
     +            + CASH_LAG_VALUES(0:12,cash_adjustment_clause)
         MONTH_VARS(:,Cash Secondary Sales) =
     +            MONTH_VARS(:,Cash Secondary Sales)
     +            + CASH_LAG_VALUES(0:12,Cash Secondary Sales)
         MONTH_VARS(:,Cash Capacity Sales) =
     +            MONTH_VARS(:,Cash Capacity Sales)
     +            + CASH_LAG_VALUES(0:12,Cash Capacity Sales)
         MONTH_VARS(:,cls_incm_rsrv_mgn_cap_sales) =
     +           MONTH_VARS(:,cls_incm_rsrv_mgn_cap_sales)
     +           + CASH_LAG_VALUES(0:12,
     +                        cls_incm_rsrv_mgn_cap_sales)
         MONTH_VARS(:,Cash BTL Revenues) =
     +            MONTH_VARS(:,Cash BTL Revenues)
     +            + CASH_LAG_VALUES(0:12,Cash BTL Revenues)
         MONTH_VARS(:,Cash ICAP Revenues) =
     +            MONTH_VARS(:,Cash ICAP Revenues)
     +            + CASH_LAG_VALUES(0:12,Cash ICAP Revenues)
C
C SERVICE TRANSACTION REVENUES
C
         MONTH_VARS(:,Cash Other Revenue) =
     +       MONTH_VARS(:,Cash Other Revenue)
     +       + CASH_LAG_VALUES(0:12,Cash Other Revenue)
         MONTH_VARS(:,cash_catawba_revenues:Cash PGA Adjustment) =
     +       MONTH_VARS(:,cash_catawba_revenues:Cash PGA Adjustment)
     +       + CASH_LAG_VALUES(0:12,
     +                        cash_catawba_revenues:Cash PGA Adjustment)
C
C DERIVAIVES REVENUE
C
         MONTH_VARS(:,Cash Physical Revenue Variable) =
     +       MONTH_VARS(:,Cash Physical Revenue Variable)
     +       + CASH_LAG_VALUES(0:12,Cash Physical Revenue Variable)
         MONTH_VARS(:,Cash Physical Revenue Fixed) =
     +          MONTH_VARS(:,Cash Physical Revenue Fixed)
     +          + CASH_LAG_VALUES(0:12,Cash Physical Revenue Fixed)
         MONTH_VARS(:,Cash Financial Revenue Variable) =
     +      MONTH_VARS(:,Cash Financial Revenue Variable)
     +      + CASH_LAG_VALUES(0:12,Cash Financial Revenue Variable)
         MONTH_VARS(:,Cash Financial Revenue Fixed) =
     +         MONTH_VARS(:,Cash Financial Revenue Fixed)
     +         + CASH_LAG_VALUES(0:12,Cash Financial Revenue Fixed)
C
C CHANGE IN ACCOUNTS RECEIVABLE
C
         CASH_REVENUES(:) = SUM(CASH_LAG_VALUES(0:12,
     +                cash_adjustment_clause:Cash PGA Adjustment),DIM=2)
     +           + CASH_LAG_VALUES(0:12,Cash ICAP Revenues)
     +           + CASH_LAG_VALUES(0:12,Cash Physical Revenue Variable)
     +           + CASH_LAG_VALUES(0:12,Cash Physical Revenue Fixed)
     +           + CASH_LAG_VALUES(0:12,Cash Financial Revenue Variable)
     +           + CASH_LAG_VALUES(0:12,Cash Financial Revenue Fixed)
     +           + CASH_LAG_VALUES(0:12,
     +                        cls_incm_rsrv_mgn_cap_sales)
         CASH_REVENUES(0) = SUM(CASH_REVENUES(1:12))
         BOOKED_REVENUES(:) = SUM(BOOKED_VALUES(:,
     +                cash_adjustment_clause:Cash PGA Adjustment),DIM=2)
     +                + BOOKED_VALUES(:,Cash ICAP Revenues)
     +                + BOOKED_VALUES(:,Cash Physical Revenue Variable)
     +                + BOOKED_VALUES(:,Cash Physical Revenue Fixed)
     +                + BOOKED_VALUES(:,Cash Financial Revenue Variable)
     +                + BOOKED_VALUES(:,Cash Financial Revenue Fixed)
     +                + BOOKED_VALUES(:,
     +                        cls_incm_rsrv_mgn_cap_sales)
         BOOKED_REVENUES(0) = SUM(BOOKED_REVENUES(1:12))
C
C CALCUALTE TOTAL REVENUES
C

C
C CL EXPENSES
C
         MONTH_VARS(:,Cash Leased Nuclear Fuel) =
     +            MONTH_VARS(:,Cash Leased Nuclear Fuel)
     +            + CASH_LAG_VALUES(0:12,Cash Leased Nuclear Fuel)
C
         MONTH_VARS(:,Cash DOE Disposal) =
     +            MONTH_VARS(:,Cash DOE Disposal)
     +            + CASH_LAG_VALUES(0:12,Cash DOE Disposal)
C
         MONTH_VARS(:,Cash DOE Decommissioning) =
     +            MONTH_VARS(:,Cash DOE Decommissioning)
     +            + CASH_LAG_VALUES(0:12,Cash DOE Decommissioning)
C
         MONTH_VARS(:,Cash Purchased Power) =
     +            MONTH_VARS(:,Cash Purchased Power)
     +            + CASH_LAG_VALUES(0:12,Cash Purchased Power)
C
         MONTH_VARS(:,Cash Fossil Fuel) =
     +            MONTH_VARS(:,Cash Fossil Fuel)
     +            + CASH_LAG_VALUES(0:12,Cash Fossil Fuel)
C
         MONTH_VARS(:,Cash Variable OandM) =
     +            MONTH_VARS(:,Cash Variable OandM)
     +            + CASH_LAG_VALUES(0:12,Cash Variable OandM)
C
         MONTH_VARS(:,Cash Fixed OandM) =
     +            MONTH_VARS(:,Cash Fixed OandM)
     +            + CASH_LAG_VALUES(0:12,Cash Fixed OandM)
C
         MONTH_VARS(:,Cash BTL Expenses) =
     +            MONTH_VARS(:,Cash BTL Expenses)
     +            + CASH_LAG_VALUES(0:12,Cash BTL Expenses)
C
         MONTH_VARS(:,csh_xpns_rsrv_mgn_cap_pchs) =
     +           MONTH_VARS(:,
     +                    csh_xpns_rsrv_mgn_cap_pchs)
     +           + CASH_LAG_VALUES(0:12,
     +                    csh_xpns_rsrv_mgn_cap_pchs)
C
C SERVICE TRANSACTIONS EXPENSE
C
         MONTH_VARS(:,Cash Other OandM) =
     +            MONTH_VARS(:,Cash Other OandM)
     +            + CASH_LAG_VALUES(0:12,Cash Other OandM)
C
         MONTH_VARS(:,Cash Purchased Gas) =
     +            MONTH_VARS(:,Cash Purchased Gas)
     +            + CASH_LAG_VALUES(0:12,Cash Purchased Gas)
C
         MONTH_VARS(:,Cash Other) =
     +            MONTH_VARS(:,Cash Other)
     +            + CASH_LAG_VALUES(0:12,Cash Other)
C
         MONTH_VARS(:,Cash DSM Expense) =
     +            MONTH_VARS(:,Cash DSM Expense)
     +            + CASH_LAG_VALUES(0:12,Cash DSM Expense)
C
         MONTH_VARS(:,Cash DSM Rebate) =
     +            MONTH_VARS(:,Cash DSM Rebate)
     +            + CASH_LAG_VALUES(0:12,Cash DSM Rebate)
C
         MONTH_VARS(:,Cash Lease Expense) =
     +            MONTH_VARS(:,Cash Lease Expense)
     +            + CASH_LAG_VALUES(0:12,Cash Lease Expense)
C
         MONTH_VARS(:,Cash Service Transactions) =
     +            MONTH_VARS(:,Cash Service Transactions)
     +            + CASH_LAG_VALUES(0:12,Cash Service Transactions)
C
         MONTH_VARS(:,Cash Emission Credits) =
     +            MONTH_VARS(:,Cash Emission Credits)
     +            + CASH_LAG_VALUES(0:12,Cash Emission Credits)
C
         MONTH_VARS(:,cash_catawba_expenses) =
     +            MONTH_VARS(:,cash_catawba_expenses)
     +            + CASH_LAG_VALUES(0:12,cash_catawba_expenses)
C
         MONTH_VARS(:,Cash BTL Lease Cash) =
     +            MONTH_VARS(:,Cash BTL Lease Cash)
     +            + CASH_LAG_VALUES(0:12,Cash BTL Lease Cash)
C
C DERIVATIVE
C
         MONTH_VARS(:,Cash Physical Expense Variable) =
     +       MONTH_VARS(:,Cash Physical Expense Variable)
     +       + CASH_LAG_VALUES(0:12,Cash Physical Expense Variable)
         MONTH_VARS(:,Cash Physical Expense Fixed) =
     +          MONTH_VARS(:,Cash Physical Expense Fixed)
     +          + CASH_LAG_VALUES(0:12,Cash Physical Expense Fixed)
         MONTH_VARS(:,Cash Financial Expense Variable) =
     +      MONTH_VARS(:,Cash Financial Expense Variable)
     +      + CASH_LAG_VALUES(0:12,Cash Financial Expense Variable)
         MONTH_VARS(:,Cash Financial Expense Fixed) =
     +         MONTH_VARS(:,Cash Financial Expense Fixed)
     +         + CASH_LAG_VALUES(0:12,Cash Financial Expense Fixed)

C RECEIVABLES AND PAYABLES
C
         CASH_EXPENSES(:) =
     +           CASH_LAG_VALUES(0:12,Cash Leased Nuclear Fuel)
     +           + CASH_LAG_VALUES(0:12,Cash DOE Disposal)
     +           + CASH_LAG_VALUES(0:12,Cash DOE Decommissioning)
     +           + CASH_LAG_VALUES(0:12,Cash Purchased Power)
     +           + CASH_LAG_VALUES(0:12,Cash Fossil Fuel)
     +           + CASH_LAG_VALUES(0:12,Cash Variable OandM)
     +           + CASH_LAG_VALUES(0:12,Cash Fixed OandM)
     +           + CASH_LAG_VALUES(0:12,Cash BTL Expenses)
     +           + CASH_LAG_VALUES(0:12,Cash Other OandM)
     +           + CASH_LAG_VALUES(0:12,Cash Purchased Gas)
     +           + CASH_LAG_VALUES(0:12,Cash Other)
     +           + CASH_LAG_VALUES(0:12,Cash DSM Expense)
     +           + CASH_LAG_VALUES(0:12,Cash DSM Rebate)
     +           + CASH_LAG_VALUES(0:12,Cash Lease Expense)
     +           + CASH_LAG_VALUES(0:12,Cash Service Transactions)
     +           + CASH_LAG_VALUES(0:12,Cash Emission Credits)
     +           + CASH_LAG_VALUES(0:12,cash_catawba_expenses)
     +           + CASH_LAG_VALUES(0:12,Cash BTL Lease Cash)
     +           + CASH_LAG_VALUES(0:12,Cash Physical Expense Variable)
     +           + CASH_LAG_VALUES(0:12,Cash Physical Expense Fixed)
     +           + CASH_LAG_VALUES(0:12,Cash Financial Expense Variable)
     +           + CASH_LAG_VALUES(0:12,Cash Financial Expense Fixed)
     +           + CASH_LAG_VALUES(0:12,
     +                    csh_xpns_rsrv_mgn_cap_pchs)

         CASH_EXPENSES(0) = SUM(CASH_EXPENSES(1:12))
         BOOK_EXPENSES(:) = BOOKED_VALUES(:,Cash Leased Nuclear Fuel)
     +                + BOOKED_VALUES(:,Cash DOE Disposal)
     +                + BOOKED_VALUES(:,Cash DOE Decommissioning)
     +                + BOOKED_VALUES(:,Cash Purchased Power)
     +                + BOOKED_VALUES(:,Cash Fossil Fuel)
     +                + BOOKED_VALUES(:,Cash Variable OandM)
     +                + BOOKED_VALUES(:,Cash Fixed OandM)
     +                + BOOKED_VALUES(:,Cash BTL Expenses)
     +                + BOOKED_VALUES(:,Cash Other OandM)
     +                + BOOKED_VALUES(:,Cash Purchased Gas)
     +                + BOOKED_VALUES(:,Cash Other)
     +                + BOOKED_VALUES(:,Cash DSM Expense)
     +                + BOOKED_VALUES(:,Cash DSM Rebate)
     +                + BOOKED_VALUES(:,Cash Lease Expense)
     +                + BOOKED_VALUES(:,Cash Service Transactions)
     +                + BOOKED_VALUES(:,Cash Emission Credits)
     +                + BOOKED_VALUES(:,cash_catawba_expenses)
     +                + BOOKED_VALUES(:,Cash BTL Lease Cash)
     +                + BOOKED_VALUES(:,Cash Physical Expense Variable)
     +                + BOOKED_VALUES(:,Cash Physical Expense Fixed)
     +                + BOOKED_VALUES(:,Cash Financial Expense Variable)
     +                + BOOKED_VALUES(:,Cash Financial Expense Fixed)
     +                + BOOKED_VALUES(:,
     +                    csh_xpns_rsrv_mgn_cap_pchs)

         BOOK_EXPENSES(0) = SUM(BOOK_EXPENSES(1:12))
         BOOKED_VALUES = 0.
      RETURN
C***********************************************************************
      ENTRY SAVE_LAG_PRODUCTION_CASH()
C***********************************************************************
C
         IF(ALLOCATED(ANNUAL_CASH_ROLL_OVER)) THEN
            ANNUAL_CASH_ROLL_OVER(1:12,:,CLASS_POS) = 0.
            ANNUAL_CASH_ROLL_OVER(1:12,:,CLASS_POS) =
     +                                          CASH_LAG_VALUES(13:24,:)
         ENDIF
      RETURN
C***********************************************************************
      ENTRY INIT_PRODUCTION_CASH_LAG()
C***********************************************************************
C
         IF(ALLOCATED(ANNUAL_CASH_ROLL_OVER))
     +                                 DEALLOCATE(ANNUAL_CASH_ROLL_OVER)
         CALL RETURN_INITIALIZATION_CLASSES(NUM_OF_ASSET_CLASSES,
     +                                      MAX_ASSET_CLASS_NUM)
         ALLOCATE(ANNUAL_CASH_ROLL_OVER(1:12,CASH_VARS,
     +                                  -1:NUM_OF_ASSET_CLASSES))
         ANNUAL_CASH_ROLL_OVER = 0.
         LAG_PERIOD = 0
         IF(WVPA() .OR. IPALCO()) LAG_PERIOD = 1
         IF(WVPA()) THEN
            CALL PARENT_CLASS_ID_LOCATION(PARENT_CLASS_LOC)
C            PARENT_CLASS_LOC = PARENT_CLASS_LOC - 1
            CALL WVPA_GET_ACTUAL_CASH_DATABASE(ACTUAL_CASH_VALUES)
            ANNUAL_CASH_ROLL_OVER(1:12,Cash Fossil Fuel,
     +                    PARENT_CLASS_LOC) = ACTUAL_CASH_VALUES(1:12,1)
            ANNUAL_CASH_ROLL_OVER(1:12,Cash Variable OandM,
     +                    PARENT_CLASS_LOC) = ACTUAL_CASH_VALUES(1:12,2)
            ANNUAL_CASH_ROLL_OVER(1:12,Cash Fixed OandM,
     +                    PARENT_CLASS_LOC) = ACTUAL_CASH_VALUES(1:12,3)
            ANNUAL_CASH_ROLL_OVER(1:12,WVPA_CMCOP,
     +                    PARENT_CLASS_LOC) = ACTUAL_CASH_VALUES(1:12,4)
            ANNUAL_CASH_ROLL_OVER(1:12,
     +          WVPA_CF_NMPS,PARENT_CLASS_LOC) =
     +                                        ACTUAL_CASH_VALUES(1:12,5)
            ANNUAL_CASH_ROLL_OVER(1:12,
     +                                WVPA Cash NonMember Cost of Power,
     +                    PARENT_CLASS_LOC) = ACTUAL_CASH_VALUES(1:12,6)
         ELSE
C CODE TO GET STARTING BALANCES
         ENDIF
      RETURN
C***********************************************************************
      ENTRY ANNUAL_PRODUCTION_REC_PAYABLES(R_CHANGE_IN_RECEIVABLES,
     +                                     R_CHANGE_IN_PAYABLES)
C***********************************************************************
C
         R_CHANGE_IN_RECEIVABLES = BOOKED_REVENUES(0)
     +                             - CASH_REVENUES(0)
         R_CHANGE_IN_PAYABLES =  BOOK_EXPENSES(0)
     +                           - CASH_EXPENSES(0)
         IF(WVPA()) THEN
         R_CHANGE_IN_PAYABLES =  R_CHANGE_IN_PAYABLES
         ENDIF
      RETURN
C***********************************************************************
      ENTRY MONTHLY_PRODUCTION_RECEIVABLES_PAYABLES(
     +                                  R_MONTHLY_CHANGE_IN_RECEIVABLES,
     +                                  R_MONTHLY_CHANGE_IN_PAYABLES)
C***********************************************************************
C
         R_MONTHLY_CHANGE_IN_RECEIVABLES(:) =
     +                                R_MONTHLY_CHANGE_IN_RECEIVABLES(:)
     +                                + BOOKED_REVENUES(:)
     +                                - CASH_REVENUES(:)
         R_MONTHLY_CHANGE_IN_PAYABLES(:) =
     +                                   R_MONTHLY_CHANGE_IN_PAYABLES(:)
     +                                   + BOOK_EXPENSES(:)
     +                                   - CASH_EXPENSES(:)
      RETURN
      END
C***********************************************************************
      SUBROUTINE FE_PCA_LEGAL_REPORTS(YEAR,ASSET_CLASS_NAME,IREC,
     +                                VARIABLES)
C***********************************************************************
      REAL (KIND=4) :: VARIABLES(0:*),YEAR
      INTEGER (KIND=4) :: IREC ! USING THE SAME REC# AS AID FILE
      CHARACTER (LEN=38) :: ASSET_CLASS_NAME
         return
      END SUBROUTINE
C***********************************************************************
      SUBROUTINE FE_MONTHLY_PCA_LEGAL_REPORTS(IREC,MO,YEAR,
     +                                        ASSET_CLASS_NAME,
     +                                        MONTH_VARS,
     +                                        PensionExpCash, 
     +                                        OUTPUT_CLASS_ID)
C***********************************************************************
      REAL (KIND=4) :: YEAR,OUTPUT_CLASS_ID,PensionExpCash
      REAL (KIND=4) :: MONTH_VARS(0:12,0:*)
      INTEGER (KIND=4) :: IREC
      INTEGER (KIND=2) :: MO
      CHARACTER (LEN=38) :: ASSET_CLASS_NAME
         return
      END SUBROUTINE
C***********************************************************************
      SUBROUTINE FE_ZERO_PCA_LEGAL_RPTS(IREC,MO,
     +                                  YEAR,
     +                                  ASSET_CLASS_NAME,
     +                                  OUTPUT_CLASS_ID)
C***********************************************************************
      REAL (KIND=4) :: YEAR,OUTPUT_CLASS_ID
      INTEGER (KIND=4) :: IREC
      INTEGER (KIND=2) :: MO
      CHARACTER (LEN=38) :: ASSET_CLASS_NAME
         return
      END SUBROUTINE
