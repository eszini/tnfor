C     Last change: MSG 1/29/2007 3:53:38 PM
C***********************************************************************
C*                                                                     *
C*                           ADDENDUMS                                 *
C*                                                                     *
C*          COPYRIGHT (C) 1997,1994 M.S. GERBER & ASSOCIATES, INC      *
C*                         ALL RIGHTS RESERVED                         *
C*                                                                     *
C***********************************************************************
      SUBROUTINE ADDENDUMS_OBJECT
      use end_routine, only: end_program, er_message
C
      use SpinDriftLib
      use prod_arrays_dimensions
      USE PROD_ARRAYS_DIMENSIONS
      use sizecom
      
      INTEGER*2 MO,YR
      INTEGER*2 IREC,INUNIT,LRECL/1024/
      INTEGER*4 IOS,IOS_BASE
      INTEGER*2 NUMBER_OF_BC_ADDENDUM_CLASSES/0/,
     +          MAX_BC_ADDENDUM_CLASS_ID_NUM/0/
      INTEGER*2 NUMBER_OF_OL_ADDENDUM_CLASSES/0/,
     +          MAX_OL_ADDENDUM_CLASS_ID_NUM/0/
      INTEGER*2 UNIT_NUM/10/,
     +          ASSET_CLASS_NUM,ASSET_CLASS_VECTOR
      INTEGER*2 R_NUM_OF_ADDENDUM_CLASSES,R_MAX_ADDENDUM_CLASS_NUM,
     +          R_ADDENDUM_CLASS_POINTERS(*)
      INTEGER*2 R_UNIT_NUM,I
      CHARACTER*1 DATA_TYPE,DUMMY
      CHARACTER*30 ADDENDUM_CLASSIFICATION
      CHARACTER*30 PAYMENTS_CLASSIFICATION,
     +             INVESTMENT_CLASSIFICATION,
     +             RATE_BASE_CLASSIFICATION,
     +             CD_CIAC_CLASSIFICATION,
     +             WORKING_CAPITAL_CLASSIFICATION,
     +             SALE_REMOVAL_CLASSIFICATION,
     +             SHAREHOLDER_CLASSIFICATION,
     +             TRANSFER_CLASSIFICATION,
     +             OTHER_CLASSIFICATION,
     +             FUNDS_CLASSIFICATION,
     +             ACTUALS_CLASSIFICATION,
     +             FASB143_ARO_CLASSIFICATION
      CHARACTER*1 INTRA_COMPANY_TRANSACTION
      INTEGER*2 INTRA_ASSET_CLASS_ID,INTRA_ASSET_CLASS_ALLOCATION
      CHARACTER*30 INTRA_ACCOUNT_CLASSIFICATION
      CHARACTER*3 INTRA_ADDENDUM_COLLECTION
      CHARACTER*5 BASE_FILE_NAME,OVERLAY_FAMILY_NAME,ADDENDUM_FILE
      CHARACTER*30 COMMENT,DESCRIPTION,TEMP_DESCRIPTION
      CHARACTER*256 FILE_NAME
      CHARACTER*256 BASE_FILE_DIRECTORY,OUTPUT_DIRECTORY
      CHARACTER*256 DATA_DRIVE
      LOGICAL*4 FILE_EXISTS,R_FILE_EXISTS
      SAVE FILE_EXISTS
C DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER*2048 RECLN
C DECLARATION FOR /ADDENDUM FILE/
      INTEGER*2 DELETE,ESCALATION_VECTOR
      INTEGER ACCTNO
      REAL*4 ADDENDUM_VALUES(AVAIL_DATA_YEARS+1),
     +       CASH_ACCRUAL_AMOUNT,CASH_VALUES_VECTOR
      CHARACTER*16 FILE_TYPE/'Addendums'/
      CHARACTER*2 ADDENDUM_OL/'BC'/,R_ADDENDUM_OL
      INTEGER*2 ADDENDUM_BC_ASSET_CLASS_POINTER(:),
     +          ADDENDUM_OL_ASSET_CLASS_POINTER(:),      
     +          TEMP_ASSET_CLASS_POINTER(:)
      ALLOCATABLE :: ADDENDUM_BC_ASSET_CLASS_POINTER,
     +               ADDENDUM_OL_ASSET_CLASS_POINTER,
     +               TEMP_ASSET_CLASS_POINTER
      SAVE ADDENDUM_BC_ASSET_CLASS_POINTER,
     +     ADDENDUM_OL_ASSET_CLASS_POINTER
      INTEGER*2 BOOK_VALUES_VECTOR,
     +          WVPA_COMPANY_ID  ! 141
      CHARACTER*1 CASH_TYPE
      REAL*4 MIDAS_MONTHLY_BOOKED(12,5)
      CHARACTER*1 ANNUAL_VALUE_STATUS(5)
      CHARACTER*30 MIDAS_MONTHLY_DATA_UNITS(5)
      CHARACTER*4 MIDAS_LAST_MONTH(5)
      CHARACTER*1 ACCOUNT_ACTIVE
      LOGICAL*1 BANGOR
      LOGICAL*1 LAHEY_LF95
C
C***********************************************************************
C
C          ROUTINE TO CONVERT METAFILE FILES TO DIRECT-ACESS BINARY
C          COPYRIGHT (C) 1983, 84, 85  M.S. GERBER & ASSOCIATES, INC.
C
C***********************************************************************
C
C CONVERT THE ADDENDUM FILE
C***********************************************************************
      ENTRY ADDENDUMS_MAKEBIN
C***********************************************************************
      BASE_FILE_NAME = ADDENDUM_FILE()
      DATA_DRIVE = OUTPUT_DIRECTORY()
      FILE_NAME = trim(BASE_FILE_DIRECTORY())//
     +                            "ADB"//trim(BASE_FILE_NAME)//".DAT"
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      IF(FILE_EXISTS) THEN
         IF(LAHEY_LF95()) THEN
            SCREEN_MESSAGES = trim(FILE_TYPE)//'-'//BASE_FILE_NAME
            CALL MG_LOCATE_WRITE(16,30,SCREEN_MESSAGES,ALL_VERSIONS,0)
         ELSE
            CALL MG_LOCATE_WRITE(16,30,BASE_FILE_NAME,ALL_VERSIONS,0)
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         ENDIF
         ALLOCATE(TEMP_ASSET_CLASS_POINTER(1024))
         TEMP_ASSET_CLASS_POINTER = 0
         OPEN(10,FILE=FILE_NAME)
         OPEN(11,FILE=trim(DATA_DRIVE)//"BCADDEN.BIN",ACCESS="DIRECT",
     +           STATUS="UNKNOWN",RECL=LRECL)
         IREC = 0
         READ(10,*) DELETE
         DO
            DATA_TYPE = 'D'
            ESCALATION_VECTOR = 0
            ASSET_CLASS_NUM = 0
            ASSET_CLASS_VECTOR = 0
            INTRA_COMPANY_TRANSACTION = 'N'
            INTRA_ASSET_CLASS_ID = 0
            INTRA_ASSET_CLASS_ALLOCATION = 0
            INTRA_ACCOUNT_CLASSIFICATION = 'Other'
            INTRA_ADDENDUM_COLLECTION = 'Bas'
            CASH_TYPE = 'N'
            BOOK_VALUES_VECTOR = 0
            CASH_VALUES_VECTOR = 0.
            CASH_ACCRUAL_AMOUNT = 0.
C
            MIDAS_MONTHLY_DATA_UNITS(1) = 'A'
            MIDAS_LAST_MONTH(1) = 'None'
            ANNUAL_VALUE_STATUS(1) = 'I'
            MIDAS_MONTHLY_DATA_UNITS(2:) = 'T'
            MIDAS_LAST_MONTH(2:) = 'None'
            ANNUAL_VALUE_STATUS(2:) = 'I'
            ACCOUNT_ACTIVE = 'A'
            WVPA_COMPANY_ID = 1  ! 141
C
C
C                      
            DO
               READ(10,1000,IOSTAT=IOS) RECLN
               IF(IOS /=0) EXIT
               IF(RECLN(1:1) == '7') EXIT
               RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
     +                              //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
     +                              //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
     +                              //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
     +                              //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
     +                              //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
     +                              //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
     +                              //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
     +                              //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
     +                              //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
     +                              //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
     +                              //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
     +                              //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
     +                              //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
     +                              //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               ADDENDUM_VALUES = -999999.
               MIDAS_MONTHLY_BOOKED = -999999.
               READ(RECLN,*,ERR=200) DELETE,ACCTNO,
     +                                  CD_CIAC_CLASSIFICATION,
     +                                  WORKING_CAPITAL_CLASSIFICATION,
     +                                  ADDENDUM_VALUES,
     +                                  DESCRIPTION,
     +                                  COMMENT,
     +                                  DATA_TYPE,
     +                                  ESCALATION_VECTOR,
     +                                  ASSET_CLASS_NUM,
     +                                  ASSET_CLASS_VECTOR,
     +                                  ADDENDUM_CLASSIFICATION,
     +                                  PAYMENTS_CLASSIFICATION,
     +                                  INTRA_COMPANY_TRANSACTION,
     +                                  INTRA_ASSET_CLASS_ID,
     +                                  INTRA_ASSET_CLASS_ALLOCATION,
     +                                  INTRA_ACCOUNT_CLASSIFICATION,
     +                                  INTRA_ADDENDUM_COLLECTION,
     +                                  BOOK_VALUES_VECTOR,
     +                                  CASH_TYPE,
     +                                  CASH_VALUES_VECTOR,
     +                                  CASH_ACCRUAL_AMOUNT,
     +                                  INVESTMENT_CLASSIFICATION,
     +                                  RATE_BASE_CLASSIFICATION,
     +                                  SALE_REMOVAL_CLASSIFICATION,
     +                                  SHAREHOLDER_CLASSIFICATION,
     +                                  TRANSFER_CLASSIFICATION,
     +                                  OTHER_CLASSIFICATION,
     +                                  FUNDS_CLASSIFICATION,
     +                                  ACTUALS_CLASSIFICATION,
     +                                  ACCOUNT_ACTIVE, ! 60 VALUES
     +                                  FASB143_ARO_CLASSIFICATION,
     +                                  DUMMY,
     +                                  DUMMY,
     +                                  DUMMY,
     +                                  DUMMY,
     +                                  MIDAS_MONTHLY_BOOKED, ! 66 VALUES
     +                                  MIDAS_MONTHLY_DATA_UNITS, ! 5 VALUES
     +                                  MIDAS_LAST_MONTH,   ! 5 VALUES
     +                                  ANNUAL_VALUE_STATUS, ! 5 VALUES
     +                                  WVPA_COMPANY_ID  ! 141

C
               IF(.NOT. (DELETE >= 8 .OR. ACCOUNT_ACTIVE == 'N'))
     +	          CALL SET_ASSET_CLASSES(ASSET_CLASS_NUM,
     +                                    NUMBER_OF_BC_ADDENDUM_CLASSES,
     +	                                 MAX_BC_ADDENDUM_CLASS_ID_NUM,
     +                                    TEMP_ASSET_CLASS_POINTER)
C
               IF(ADDENDUM_VALUES(1) == -999999.) ADDENDUM_VALUES(1)=0.
               DO I = 2, AVAIL_DATA_YEARS+1
                  IF(ADDENDUM_VALUES(I) == -999999.)
     +                         ADDENDUM_VALUES(I) = ADDENDUM_VALUES(I-1)
               ENDDO

               IREC = IREC + 1
               WRITE(11,REC=IREC) DELETE,ACCTNO,
     +                            CD_CIAC_CLASSIFICATION,
     +                            WORKING_CAPITAL_CLASSIFICATION,
     +                            ADDENDUM_VALUES,
     +                            DATA_TYPE,
     +                            ESCALATION_VECTOR,
     +                            ASSET_CLASS_NUM,
     +                            ASSET_CLASS_VECTOR,
     +                            ADDENDUM_CLASSIFICATION,
     +                            PAYMENTS_CLASSIFICATION,
     +                            INTRA_COMPANY_TRANSACTION,
     +                            INTRA_ASSET_CLASS_ID,
     +                            INTRA_ASSET_CLASS_ALLOCATION,
     +                            INTRA_ACCOUNT_CLASSIFICATION,
     +                            INTRA_ADDENDUM_COLLECTION,
     +                            BOOK_VALUES_VECTOR,
     +                            CASH_TYPE,
     +                            CASH_VALUES_VECTOR,
     +                            CASH_ACCRUAL_AMOUNT,
     +                            INVESTMENT_CLASSIFICATION,
     +                            RATE_BASE_CLASSIFICATION,
     +                            SALE_REMOVAL_CLASSIFICATION,
     +                            SHAREHOLDER_CLASSIFICATION,
     +                            TRANSFER_CLASSIFICATION,
     +                            OTHER_CLASSIFICATION,
     +                            FUNDS_CLASSIFICATION,
     +                            ACTUALS_CLASSIFICATION,
     +                            FASB143_ARO_CLASSIFICATION,
     +                            ACCOUNT_ACTIVE,
     +                            DESCRIPTION,
     +                            MIDAS_MONTHLY_BOOKED, ! 60 VALUES
     +                            MIDAS_MONTHLY_DATA_UNITS, ! 5 VALUES
     +                            MIDAS_LAST_MONTH,   ! 5 VALUES
     +                            ANNUAL_VALUE_STATUS, ! 5 VALUES
     +                            WVPA_COMPANY_ID  ! 141
            ENDDO
            IF(IOS /= 0) EXIT
         ENDDO
         CLOSE(10)
c         ENDFILE(11)
         CLOSE(11)
         IF(MAX_BC_ADDENDUM_CLASS_ID_NUM > 0) THEN
            ALLOCATE(ADDENDUM_BC_ASSET_CLASS_POINTER
     +                                   (MAX_BC_ADDENDUM_CLASS_ID_NUM))
            ADDENDUM_BC_ASSET_CLASS_POINTER =
     +          TEMP_ASSET_CLASS_POINTER(1:MAX_BC_ADDENDUM_CLASS_ID_NUM)
         ENDIF
         DEALLOCATE(TEMP_ASSET_CLASS_POINTER)
C
C PROCESS THE ADDENDUMS
C
c         CALL ADDENDUMS(.TRUE.)
      ELSE IF(INDEX(BASE_FILE_NAME,'NONE') == 0) THEN
         CALL STOP_NOFILE(FILE_TYPE,FILE_NAME)
      ENDIF
      RETURN

C***********************************************************************
C
C          ROUTINE TO CREATE OVERLAY FILES
C          COPYRIGHT (C) 1984-88  M.S. GERBER & ASSOCIATES, INC.
C          COPYRIGHT (C) 1991-92  M.S. GERBER & ASSOCIATES, INC.
C
C***********************************************************************
C
C OVERLAY THE ADDENDUM FILE
C***********************************************************************
      ENTRY ADDENDUMS_MAKEOVL(OVERLAY_FAMILY_NAME)
C***********************************************************************
      IF(LAHEY_LF95()) THEN
         SCREEN_MESSAGES = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_MESSAGES,ALL_VERSIONS,0)
      ELSE
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         CALL LOCATE(10,51)
      ENDIF
      DATA_DRIVE = OUTPUT_DIRECTORY()
      FILE_NAME=trim(DATA_DRIVE)//"ADO"//
     +                               trim(OVERLAY_FAMILY_NAME)//".DAT"
      OPEN(10,FILE=FILE_NAME)
      READ(10,*) DELETE
      INUNIT = 12
      IF(ADDENDUM_OL == 'BC') THEN
         OPEN(11,FILE=trim(DATA_DRIVE)//"BCADDEN.BIN",
     +                                       ACCESS="DIRECT",RECL=LRECL)
         INUNIT = 11
      ENDIF
      OPEN(12,FILE=trim(DATA_DRIVE)//"OLADDEN.BIN",ACCESS="DIRECT",
     +                                      STATUS="UNKNOWN",RECL=LRECL)
      ALLOCATE(TEMP_ASSET_CLASS_POINTER(1024))
      TEMP_ASSET_CLASS_POINTER = 0
      NUMBER_OF_OL_ADDENDUM_CLASSES = 0
      MAX_OL_ADDENDUM_CLASS_ID_NUM = 0
      IREC = 0
      DO
         DO
            READ(10,1000,IOSTAT=IOS) RECLN
            IF(RECLN(1:1) == '7') EXIT
            IREC = IREC + 1
            READ(INUNIT,REC=IREC,IOSTAT=IOS_BASE) DELETE,ACCTNO,
     +                               CD_CIAC_CLASSIFICATION,
     +                               WORKING_CAPITAL_CLASSIFICATION,
     +                               ADDENDUM_VALUES,
     +                               DATA_TYPE,
     +                               ESCALATION_VECTOR,
     +                               ASSET_CLASS_NUM,
     +                               ASSET_CLASS_VECTOR,
     +                               ADDENDUM_CLASSIFICATION,
     +                               PAYMENTS_CLASSIFICATION,
     +                               INTRA_COMPANY_TRANSACTION,
     +                               INTRA_ASSET_CLASS_ID,
     +                               INTRA_ASSET_CLASS_ALLOCATION,
     +                               INTRA_ACCOUNT_CLASSIFICATION,
     +                               INTRA_ADDENDUM_COLLECTION,
     +                               BOOK_VALUES_VECTOR,
     +                               CASH_TYPE,
     +                               CASH_VALUES_VECTOR,
     +                               CASH_ACCRUAL_AMOUNT,
     +                               INVESTMENT_CLASSIFICATION,
     +                               RATE_BASE_CLASSIFICATION,
     +                               SALE_REMOVAL_CLASSIFICATION,
     +                               SHAREHOLDER_CLASSIFICATION,
     +                               TRANSFER_CLASSIFICATION,
     +                               OTHER_CLASSIFICATION,
     +                               FUNDS_CLASSIFICATION,
     +                               ACTUALS_CLASSIFICATION,
     +                               FASB143_ARO_CLASSIFICATION,
     +                               ACCOUNT_ACTIVE,
     +                               DESCRIPTION,
     +                               MIDAS_MONTHLY_BOOKED, ! 60 VALUES
     +                               MIDAS_MONTHLY_DATA_UNITS, ! 5 VALUES
     +                               MIDAS_LAST_MONTH,   ! 5 VALUES
     +                               ANNUAL_VALUE_STATUS, ! 5 VALUES
     +                               WVPA_COMPANY_ID  ! 141
            IF(IOS_BASE /= 0) EXIT
            IF(IOS == 0) THEN
               RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
     +                              //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
     +                              //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
     +                              //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
     +                              //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
     +                              //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
     +                              //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
     +                              //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               READ(RECLN,*,ERR=200) DELETE,ACCTNO,
     +                                  CD_CIAC_CLASSIFICATION,
     +                                  WORKING_CAPITAL_CLASSIFICATION,
     +                                  ADDENDUM_VALUES,
     +                                  TEMP_DESCRIPTION,
     +                                  COMMENT,
     +                                  DATA_TYPE,
     +                                  ESCALATION_VECTOR,
     +                                  ASSET_CLASS_NUM,
     +                                  ASSET_CLASS_VECTOR,
     +                                  ADDENDUM_CLASSIFICATION,
     +                                  PAYMENTS_CLASSIFICATION,
     +                                  INTRA_COMPANY_TRANSACTION,
     +                                  INTRA_ASSET_CLASS_ID,
     +                                  INTRA_ASSET_CLASS_ALLOCATION,
     +                                  INTRA_ACCOUNT_CLASSIFICATION,
     +                                  INTRA_ADDENDUM_COLLECTION,
     +                                  BOOK_VALUES_VECTOR,
     +                                  CASH_TYPE,
     +                                  CASH_VALUES_VECTOR,
     +                                  CASH_ACCRUAL_AMOUNT,
     +                                  INVESTMENT_CLASSIFICATION,
     +                                  RATE_BASE_CLASSIFICATION,
     +                                  SALE_REMOVAL_CLASSIFICATION,
     +                                  SHAREHOLDER_CLASSIFICATION,
     +                                  TRANSFER_CLASSIFICATION,
     +                                  OTHER_CLASSIFICATION,
     +                                  FUNDS_CLASSIFICATION,
     +                                  ACTUALS_CLASSIFICATION,
     +                                  ACCOUNT_ACTIVE, ! 60 VALUES
     +                                  FASB143_ARO_CLASSIFICATION,
     +                                  DUMMY,
     +                                  DUMMY,
     +                                  DUMMY,
     +                                  DUMMY,
     +                                  MIDAS_MONTHLY_BOOKED, ! 66 VALUES
     +                                  MIDAS_MONTHLY_DATA_UNITS, ! 5 VALUES
     +                                  MIDAS_LAST_MONTH,   ! 5 VALUES
     +                                  ANNUAL_VALUE_STATUS, ! 5 VALUES
     +                                  WVPA_COMPANY_ID  ! 141
            ENDIF
            IF(.NOT. (DELETE >= 8 .OR. ACCOUNT_ACTIVE == 'N'))
     +             CALL SET_ASSET_CLASSES(ASSET_CLASS_NUM,
     +                                    NUMBER_OF_OL_ADDENDUM_CLASSES,
     +	                                 MAX_OL_ADDENDUM_CLASS_ID_NUM,
     +                                    TEMP_ASSET_CLASS_POINTER)
            WRITE(12,REC=IREC) DELETE,ACCTNO,
     +                         CD_CIAC_CLASSIFICATION,
     +                         WORKING_CAPITAL_CLASSIFICATION,
     +                         ADDENDUM_VALUES,
     +                         DATA_TYPE,
     +                         ESCALATION_VECTOR,
     +                         ASSET_CLASS_NUM,
     +                         ASSET_CLASS_VECTOR,
     +                         ADDENDUM_CLASSIFICATION,
     +                         PAYMENTS_CLASSIFICATION,
     +                         INTRA_COMPANY_TRANSACTION,
     +                         INTRA_ASSET_CLASS_ID,
     +                         INTRA_ASSET_CLASS_ALLOCATION,
     +                         INTRA_ACCOUNT_CLASSIFICATION,
     +                         INTRA_ADDENDUM_COLLECTION,
     +                         BOOK_VALUES_VECTOR,
     +                         CASH_TYPE,
     +                         CASH_VALUES_VECTOR,
     +                         CASH_ACCRUAL_AMOUNT,
     +                         INVESTMENT_CLASSIFICATION,
     +                         RATE_BASE_CLASSIFICATION,
     +                         SALE_REMOVAL_CLASSIFICATION,
     +                         SHAREHOLDER_CLASSIFICATION,
     +                         TRANSFER_CLASSIFICATION,
     +                         OTHER_CLASSIFICATION,
     +                         FUNDS_CLASSIFICATION,
     +                         ACTUALS_CLASSIFICATION,
     +                         FASB143_ARO_CLASSIFICATION,
     +                         ACCOUNT_ACTIVE,
     +                         DESCRIPTION,
     +                         MIDAS_MONTHLY_BOOKED, ! 60 VALUES
     +                         MIDAS_MONTHLY_DATA_UNITS, ! 5 VALUES
     +                         MIDAS_LAST_MONTH,   ! 5 VALUES
     +                         ANNUAL_VALUE_STATUS, ! 5 VALUES
     +                         WVPA_COMPANY_ID  ! 141
         ENDDO
         IF(IOS_BASE /= 0) EXIT
      ENDDO
      CLOSE(10)
      CLOSE(12)
      IF(ADDENDUM_OL == 'BC') CLOSE(11)
      ADDENDUM_OL= 'OL'
      IF(ALLOCATED(ADDENDUM_OL_ASSET_CLASS_POINTER))
     +                       DEALLOCATE(ADDENDUM_OL_ASSET_CLASS_POINTER)
      IF(MAX_OL_ADDENDUM_CLASS_ID_NUM > 0) THEN
         ALLOCATE(ADDENDUM_OL_ASSET_CLASS_POINTER
     +                                   (MAX_OL_ADDENDUM_CLASS_ID_NUM))
         ADDENDUM_OL_ASSET_CLASS_POINTER =
     +          TEMP_ASSET_CLASS_POINTER(1:MAX_OL_ADDENDUM_CLASS_ID_NUM)
      ENDIF
      DEALLOCATE(TEMP_ASSET_CLASS_POINTER)

      RETURN

  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from MSGmmadm SIID201'
      call end_program(er_message)
C
C***********************************************************************
      ENTRY RESET_ADDENDUM_OL
C***********************************************************************
         ADDENDUM_OL = 'BC'
      RETURN
C
C***********************************************************************
      ENTRY GET_ADDENDUM_OL(R_ADDENDUM_OL,R_FILE_EXISTS)
C***********************************************************************
         R_ADDENDUM_OL = ADDENDUM_OL
         R_FILE_EXISTS = FILE_EXISTS
      RETURN
C
C***********************************************************************
      ENTRY OPEN_ADDENDUM_FILE(R_UNIT_NUM)
C***********************************************************************
         OPEN(R_UNIT_NUM,FILE=trim(OUTPUT_DIRECTORY())//ADDENDUM_OL//
     +          "ADDEN.BIN",ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
         UNIT_NUM = R_UNIT_NUM
      RETURN
C
C***********************************************************************
      ENTRY OPEN_ADDENDUM_BASE_CASE_FILE(R_UNIT_NUM)
C***********************************************************************
         OPEN(R_UNIT_NUM,FILE=trim(OUTPUT_DIRECTORY())//
     +                                "BC_ADDEN.BIN",FORM='UNFORMATTED')
      RETURN
C***********************************************************************
      ENTRY CLOSE_ADDENDUM_FILE
C***********************************************************************
         CLOSE(UNIT_NUM)
      RETURN
C***********************************************************************
      ENTRY RETURN_NUM_OF_ADDENDUM_CLASSES(R_NUM_OF_ADDENDUM_CLASSES,
     +                                       R_MAX_ADDENDUM_CLASS_NUM)
C***********************************************************************
         IF(ADDENDUM_OL == 'OL') THEN
            R_NUM_OF_ADDENDUM_CLASSES = NUMBER_OF_OL_ADDENDUM_CLASSES
            R_MAX_ADDENDUM_CLASS_NUM = MAX_OL_ADDENDUM_CLASS_ID_NUM
         ELSE
            R_NUM_OF_ADDENDUM_CLASSES = NUMBER_OF_BC_ADDENDUM_CLASSES
            R_MAX_ADDENDUM_CLASS_NUM = MAX_BC_ADDENDUM_CLASS_ID_NUM
         ENDIF
      RETURN
C***********************************************************************
      ENTRY RETURN_ADDENDUM_CLASS_POINTER(R_ADDENDUM_CLASS_POINTERS)
C***********************************************************************
         IF(ADDENDUM_OL == 'OL') THEN
            R_ADDENDUM_CLASS_POINTERS(1:MAX_OL_ADDENDUM_CLASS_ID_NUM) =
     +                  ADDENDUM_OL_ASSET_CLASS_POINTER(
     +                                   1:MAX_OL_ADDENDUM_CLASS_ID_NUM)
         ELSE
            R_ADDENDUM_CLASS_POINTERS(1:MAX_BC_ADDENDUM_CLASS_ID_NUM) =
     +                  ADDENDUM_BC_ASSET_CLASS_POINTER(
     +                                   1:MAX_BC_ADDENDUM_CLASS_ID_NUM)
         ENDIF
      RETURN
C
 1000 FORMAT(A)
 1010 FORMAT('&',A)
      END subroutine ADDENDUMS_OBJECT
C***********************************************************************
C*                                                                     *
C*                           ADDENDUMS                                 *
C*                                                                     *
C*          COPYRIGHT (C) 1997 M.S. GERBER & ASSOCIATES, INC           *
C*                         ALL RIGHTS RESERVED                         *
C*                                                                     *
C***********************************************************************
C                                                                      *
      RECURSIVE SUBROUTINE ADDENDUMS(SAVE_BASE_CASE)
C
      use SpinDriftLib
      use prod_arrays_dimensions
      USE PROD_ARRAYS_DIMENSIONS
      use sizecom
      use globecom
	  use namescom
      SAVE
      
      INCLUDE 'mthnmcom.mon'
      
      
      CHARACTER*1 DUMMY_TYPE
      LOGICAL*1 VECTOR_TYPE,RETURN_A_VECTOR_FOR_ALL_YEARS,RATE_FOUND
      LOGICAL*4 SAVE_BASE_CASE
      INTEGER*2 I,IREC,DELETE,ESCAL_VECT,CLASS_POINTER,
     +          NUM_OF_ASSET_CLASSES,FINANCIAL_SIMULATION_YEARS
      INTEGER*4 IOS
      CHARACTER*1 DATA_TYPE
      REAL*4 ESCALATION_RATES(:),RATE,D12
      ALLOCATABLE :: ESCALATION_RATES
      INTEGER*2 ASSET_CLASS_POINTER(:),MAX_ASSET_CLASS_NUM,YR
      ALLOCATABLE :: ASSET_CLASS_POINTER
      CHARACTER*1 ACCOUNT_TYPE,
     +            INTRA_COMPANY_TRANSACTION
      INTEGER*2 INTRA_ASSET_CLASS_ID,INTRA_ASSET_CLASS_ALLOCATION
      CHARACTER*30 INTRA_ACCOUNT_CLASSIFICATION
      CHARACTER*3 INTRA_ADDENDUM_COLLECTION
      INTEGER*2 BOOK_VALUES_VECTOR
      CHARACTER*1 CASH_TYPE
      REAL*4 CASH_ACCRUAL_AMOUNT,CASH_VALUES_VECTOR
      LOGICAL*4 BASE_FILE_EXISTS
C
C     TYPE DECLARATION FOR /FAINPT/
C
      INTEGER*2 ASSET_ALLOCATION_VECTOR,
     +          ASSET_CLASS,RUN_YEARS,EXTENSION_YEARS,
     +          WVPA_COMPANY_ID  ! 141
      INTEGER ACCTNO
      REAL*4 ESCALATION_MULTIPLIER,ASSET_ALLOCATOR,MULTIPLIER
C
C     TYPE DECLARATION FOR /WKARRY/
C
      REAL*4 ADDENDUM_VALUES(:),ALLOC_EXPEN(:,:),
     +       ADDENDUM(:),ASSET_CLASS_LIST(:),ASSET_ALLOCATION_LIST(:)
      ALLOCATABLE :: ALLOC_EXPEN,ADDENDUM_VALUES,
     +               ADDENDUM,ASSET_CLASS_LIST,ASSET_ALLOCATION_LIST
C
C  SECTION
C
      REAL*4 ANNUAL_ADDENDUM(0:LAST_AVAILABLE_MONTHLY_YEAR)
      REAL*4 ALLOCATION_VALUE(AVAIL_DATA_YEARS)
      INTEGER*2 ALLOCATION_VECTOR,R_CLASS
      INTEGER*2 R_ASSET_CLASS,ASSET_CLASS_ID,R_YR
      CHARACTER*2 ADDENDUM_OL
C
C
      CHARACTER*30 PAYMENTS_CLASSIFICATION,
     +             INVESTMENT_CLASSIFICATION,
     +             RATE_BASE_CLASSIFICATION,
     +             CD_CIAC_CLASSIFICATION,
     +             WORKING_CAPITAL_CLASSIFICATION,
     +             SALE_REMOVAL_CLASSIFICATION,
     +             TRANSFER_CLASSIFICATION,
     +             OTHER_CLASSIFICATION,
     +             FUNDS_CLASSIFICATION,
     +             SHAREHOLDER_CLASSIFICATION,
     +             ADDENDUM_CLASSIFICATION,
     +             ACTUALS_CLASSIFICATION,
     +             FASB143_ARO_CLASSIFICATION,
     +             DESCRIPTION,
     +             DATA_BASE_CLASSIFICATION
      LOGICAL*1 IN_WVPA_DATA_BASE
C
      REAL*4, ALLOCATABLE ::  PAYMENTS(:,:,:,:),
     +                        INVESTMENTS(:,:,:,:),
     +                        RATE_BASE(:,:,:,:),
     +                        WORKING_CAPITAL(:,:,:,:),
     +                        CD_CIAC(:,:,:,:),
     +                        SALE_REMOVAL(:,:,:,:),
     +                        SHAREHOLDER_VALUE(:,:,:,:),
     +                        TRANSFER_VALUE(:,:,:,:),
     +                        CASH_MONTHLY_VALUES(:,:,:,:),
     +                        OTHER_VALUE(:,:,:,:),
     +                        FUNDS_EARNINGS(:,:,:,:),
     +                        ACTUAL_MONTHLY_VALUES(:,:,:,:),
     +                        FASB143_ARO_VALUE(:,:,:,:),
     +                        CapX_Values(:,:,:,:),
     +                        WVPA_ADDENDUM_ITEMS(:,:,:,:)
      LOGICAL*1, ALLOCATABLE :: ACTUAL_VALUE_FOUND(:,:)
C
      REAL*4 MIDAS_MONTHLY_BOOKED(12,LAST_AVAILABLE_MONTHLY_YEAR),
     +       MIDAS_MONTHLY_CASH(12,LAST_AVAILABLE_MONTHLY_YEAR),
     +       MIDAS_ANNUAL_CASH(0:AVAIL_DATA_YEARS),
     +       MONTHLY_ALLOCATED_VALUES(12,LAST_AVAILABLE_MONTHLY_YEAR)
c     LOGICAL*1 MONTHLY_MIDAS_ACTIVE/.TRUE./
      CHARACTER*1 ANNUAL_VALUE_STATUS(LAST_AVAILABLE_MONTHLY_YEAR)
      CHARACTER*1 MIDAS_MONTHLY_DATA_UNITS(LAST_AVAILABLE_MONTHLY_YEAR)
      CHARACTER*30 TEMP_MIDAS_MONTHLY_DATA_UNITS(
     +                                      LAST_AVAILABLE_MONTHLY_YEAR)
      CHARACTER*4 MIDAS_LAST_MONTH(LAST_AVAILABLE_MONTHLY_YEAR)
      INTEGER*2 MONTH_ENDING(LAST_AVAILABLE_MONTHLY_YEAR),MO,TAX_TYPE
C
C
      REAL*4 R_CASH_VALUES(0:12,*),
     +       R_INCOME_VALUES(0:12,*),
     +       R_BALANCE_SHEET_VALUES(0:12,*),
     +       R_MONTHLY_NUC_DECOM_CASH_VALUES(0:12),
     +       R_MONTHLY_NUC_DECOM_EARNINGS_ADDEM(0:12),
     +       R_MONTHLY_POST_RETIREMENT_CASH_PAYMENTS(0:12),
     +       MONTHLY_CASH_TO_POST_RETIREMENT(0:12),
     +       MONTHLY_CASH_TO_MEMBER_ACCRUED_REVS_BAL(0:12),
     +       R_MONTHLY_RETIREE_MEDICAL_CASH_PAYMENTS(0:12),
     +       R_MONTHLY_RETIREMENT_FUND_EARNINGS_ADJ(0:12),
     +       R_MONTHLY_BOOK_DEPRECIATION(0:12)
C
      REAL*4 R_BOOK_DEPRECIATION
      LOGICAL*1 CLASSIFICATON_IS_ACTUAL
      LOGICAL*1 FALSE_BYTE,WVPA
C
      INTEGER*2 R_FINANCIAL_SIMULATION_YEARS,
     +          R_NUM_OF_ASSET_CLASSES
      REAL*4 R_ADDENDUM_ARRAY(0:12,0:R_FINANCIAL_SIMULATION_YEARS,
     +                                      -1:R_NUM_OF_ASSET_CLASSES,*)
      REAL*4 R_ALLOC_EXPEN(0:R_FINANCIAL_SIMULATION_YEARS,0:12)
      CHARACTER*(*) R_ADDENDUM_CLASSIFICATION
      INTEGER*2 ADDENDUM_TYPE,ADDENDUM_POSITION
      LOGICAL*1 ADDED_TO_CURRENT_VALUE
C
      REAL*4 R_PENSION_EXPENSE_CASH,
     +       R_STORM_EXPENSE_CASH,
     +       R_VACATION_PAY_CASH
      REAL*4 R_ACTUAL_DEFERRED_TAXES_CR(0:12),
     +       R_ACTUAL_FED_INCOME_TAXES(0:12),
     +       R_ACTUAL_STATE_INCOME_TAXES(0:12),
     +       R_ACTUAL_DEFERRED_TAXES_DR(0:12)
      REAL*4 ANNUAL_AMOUNT,MONTHLY_AMOUNT
      INTEGER*2 MO2
      REAL*4 UNALLOCATED_BALANCE,ANNUAL_TARGET_VALUE,RATIO,
     +       REMAINING_ANNUAL_BALANCE
C
      REAL*4 R_DIVIDEND_SUBSIDIARY,
     +       R_NET_INCOME_SUBSIDIARY,
     +       R_INVESTMENT_IN_SUBSIDIARY,
     +       R_OTHER_INVESTMENTS,
     +       R_CAPITIALIZED_LEASE_ADDITIONS,
     +       R_CASH_FOR_ASSETS,
     +       R_CASH_CHANGE_LTI
      REAL*4 R_ASSETS_NEC_ADJUSTMENT,
     +       R_RATE_BASE_ADJUSTMENT
      REAL*4 R_EARNINGS_DECOMMISSIONING_FUND,
     +       R_EARNINGS_POST_RETIREMENT_FUND,
     +       R_EARNINGS_ST_INVESTMENTS,
     +       R_EARNINGS_LT_INVESTMENTS
      REAL*4 R_CD_BALANCE_FROM_REVENUES,
     +       R_CD_ADDENDUM,
     +       R_CIAC_CASH_CONTRIBUTIONS,
     +       R_CIAC_AMORTIZATION_RATE,
     +       R_CIAC_ADDENDUM_TO_AMORTIZATION
      REAL*4 R_ASSETS_NEC_ADDENDUM,
     +       R_LIABILITIES_NEC_ADDENDUM,
     +       R_FUEL_INVENTORY_CHANGE_IN,
     +       R_MATERIALS_SUPPLIES_IN,
     +       R_GAS_STORAGE_IN,
     +       R_FUEL_INVENTORY_OUT,
     +       R_MATERIALS_SUPPLIES_OUT,
     +       R_GAS_STORAGE_OUT,
     +       R_ACCT_RECEIVABLE_RATE,
     +       R_ACCT_RECEIVABLE_ADDEN,
     +       R_ACCT_PAYABLE_RATE,
     +       R_ACCT_PAYABLE_ADDEN
      REAL*4 R_NET_REMOVAL,
     +       R_CASH_RECEIVED,
     +       R_GROSS_BOOK_VALUE,
     +       R_CUMULATIVE_DEPRECIATION,
     +       R_DEFERRED_TAXES_ADJUSTMENT,
     +       R_ATL_AMORTIZATION,
     +       R_BTL_AMORTIZATION,
     +       R_ADDEDUM_TO_BOOK_GAIN_LOSS
      REAL*4 R_EQUITY_RISK_ADJUSTMENT,
     +       R_ECONOMIC_ASSETS_ADJUSTMENT,
     +       R_OPERATING_PROFITS_ADJUSTMENT,
     +       R_CAP_RECOVERY_EXCLUDE_OPAT
      REAL*4 R_GROSS_PLANT_VALUE,
     +       R_CUMULATED_PLANT_DEPRECIATION,
     +       R_DEFERRED_TAXES_BAL_ADJ,
     +       R_DEFERRED_DEBIT_ADJUSTMENT,
     +       R_NET_DEFERRED_DEBIT_ADJUSTMENT,
     +       R_ATL_DEBIT_AMORTIZATION,
     +       R_BTL_DEBT_AMORTIZATION,
     +       R_ANNUAL_DEFERRED_TAXES,
     +       R_DEFERRED_ITCS_BAL_ADJ,
     +       R_NET_NUCLEAR_FUEL_ADJ,
     +       R_RETAINED_EARNINGS_ADJ,
     +       R_EXTRA_ORDINARY_EXPENSE,
     +       R_LT_LIAB_BAL_ADJ,
     +       R_CWIP_BALANCE_ADJ,
     +       R_DEFERRED_TAXES_DR,
     +       R_PAID_IN_CAPITAL,
     +       R_SUBSIDIARY_INVESTMENT_ADJ,
     +       R_GOODWILL_ADJUSTMENT,
     +       R_REG_ASSESTS_ADJUSTMENT,
     +       R_FASB109_ADJUSTMENT,
     +       R_FASB133_ADJUSTMENT,
     +       R_UNAMORT_INTEREST_ADJUSTMENT,
     +       R_NUCLEAR_DECOM_FUND_BAL,
     +       R_CAP_LEASES_BAL,
     +       R_ASSETS_NEC_BAL,
     +       R_PREFERRED_STOCK_BAL,
     +       R_LTD_BAL,
     +       R_STD_BAL,
     +       R_NUC_DECOM_LIAB_BAL,
     +       R_LIABS_NEC_BAL,
     +       R_OTHER_INVESTMENT_BAL,
     +       R_NOTES_RECEIVABLE_BAL,
     +       R_POST_RETIRE_MEDICAL_BAL,
     +       R_DEFERRED_REVENUES_BAL,
     +       R_DEFERRED_FUEL_BAL,
     +       R_DEFERRED_PURCH_GAS_BAL,
     +       R_LT_INVEST_BAL,
     +       R_ST_INVEST_BAL,
     +       R_FUEL_INVENTORY_BAL,
     +       R_GAS_IN_STORAGE_BAL,
     +       R_MATRIAL_SUPPLY_BAL,
     +       R_ACCOUNTS_RECV_BAL,
     +       R_UNBILLED_REV_BAL,
     +       R_TAXES_RECEIVABLE_BAL,
     +       R_CURRENT_LT_DEBT_BAL,
     +       R_NOTES_PAYABLE_BAL,
     +       R_CUSTOMER_DEPOSIT_BAL,
     +       R_CICA_BAL,
     +       R_POST_RETIRE_PAYABLE_BAL,
     +       R_ACCRUED_PENSION_BAL,
     +       R_DEFERRED_GAINS_BAL,
     +       R_STROM_RESERVE_BAL,
     +       R_ACCOUNTS_PAYABLE_BAL,
     +       R_ARO_NET_ASSETS,
     +       R_ARO_LIABILITY,
     +       R_DEFERRED_PURCHASE_POWER_ADJ,
     +       R_COI_EARNINGS
C
      REAL*4 R_SRP_TARGET_SALES,
     +       R_GRE_PRINCIPAL_MANUAL_ADJS,
     +       R_GRE_INTEREST_PAYMENTS_MANUAL_ADJS,
     +       IPL_ELECTRIC_PLAN_FUEL_COSTS(0:12),
     +       IPL_ELECTRIC_PLAN_PUR_COSTS(0:12)
      REAL*4 GPV_ADJUSTMENT,DEF_TAX_ADJUSTMENT,
     +       DEF_DEBIT_ADJUSTMENT,
     +       CWIP_BALANCE_ADJ,
     +       LT_LIAB_BAL_ADJ,
     +       NET_NUC_FUEL_ADJ,
     +       CAPITAL_LEASE_ADJ,
     +       R_TRANSFER_VARIABLES(0:12,*),
     +       R_DEFERRED_GAIN_ADJ(0:12),
     +       R_ASSET_NEC_ADDENDUM(0:12),
     +       R_LIABS_NEC_ADDENDUM(0:12)
C
      REAL*4 R_CIAC_MONTHLY_AMORT_RATE(0:12),
     +       R_CIAC_MONTHLY_ADDENDUM_2_AMORT(0:12),
     +       R_CIAC_MONTHLY_CASH(0:12)
      REAL*4 R_DECOMMISSIONING_FUND,
     +       R_DECOMMISSIONING_LIABILITY,
     +       R_POST_RETIREMENT_MEDICAL,
     +       R_RETIREE_MEDICAL_PAYMENTS
      REAL*4 R_NucDecom_Discount_Rate,
     +       R_NucDecom_Interest_Accretion,
     +       R_ARO_Cash_Payment,
     +       R_ARO_Trust_Cash_Payment,
     +       R_MO_NucDecom_Discount_Rate(0:12),
     +       R_MO_NucDecom_Interest_Accretion(0:12),
     +       R_MO_ARO_Cash_Payment(0:12),
     +       R_MO_ARO_Trust_Cash_Payment(0:12),
     +       R_MO_ARO_NET_ASSETS(0:12),
     +       R_MO_ARO_LIABILITY(0:12),
     +       R_OCI_ADJ(0:12),
     +       R_INTANGIBLE_ASSET_ADJ(0:12),
     +       R_PENSION_LIABILITY_ADJ(0:12),
     +       R_OCI_DEFERRED_TAX_ADJ_DR(0:12)
      CHARACTER*1 ACCOUNT_ACTIVE
      REAL*4 R_STD_RATE,
     +       R_MONTHLY_STD_RATE(0:12),
     +       R_RETURN_ON_ST_INVEST,
     +       R_INTEREST_ON_CUSTOMER_DEPOSITS,
     +       R_RETURN_ON_LTI,
     +       R_RETURN_RETIREMENT_FUND,
     +       R_NUCL_DECOM_FUND_RETURN,
     +       R_OCI_NUCL_DECOM_FUND_RETURN,
     +       R_OCI_RETURN_RETIREMENT_FUND,
     +       R_MONTHLY_STI_RATE(0:12),
     +       R_MONTHLY_CUST_DEPOSITS_RATE(0:12),
     +       R_MONTHLY_LTI_RATE(0:12),
     +       R_MONTHLY_RETIREMENT_FUND_RATE(0:12),
     +       R_MONTHLY_NUC_DECOM_RATE(0:12),
     +       OCI_MONTHLY_RETIREMENT_FUND_RATE(0:12),
     +       OCI_MONTHLY_NUC_DECOM_RATE(0:12)
      REAL (KIND=4) :: CASH_TO_CASH,
     +                 CASH_TO_POST_RETIRE_MEDICAL_BAL,
     +                 CASH_TO_ACCRUED_MEMBER_REVENUES_BAL
C
      FINANCIAL_SIMULATION_YEARS = MAX(RUN_YEARS() + EXTENSION_YEARS(),
     +                             LAST_AVAILABLE_MONTHLY_YEAR)
      IF(.NOT. SAVE_BASE_CASE .AND. .NOT. WVPA()) THEN
         CALL GET_ADDENDUM_OL(ADDENDUM_OL,BASE_FILE_EXISTS)
         IF(ADDENDUM_OL == 'BC') THEN
            IF(BASE_FILE_EXISTS) CALL READ_ADDENDUM_BASE_CASE
            RETURN
         ENDIF
      ENDIF
C
      CALL SET_UP_ADDENDUM_ARRAYS
C
      ALLOCATE(ESCALATION_RATES(AVAIL_DATA_YEARS),
     +         ADDENDUM_VALUES(0:
     +                MAX(FINANCIAL_SIMULATION_YEARS,AVAIL_DATA_YEARS)),
     +         ADDENDUM(0:FINANCIAL_SIMULATION_YEARS),
     +         ALLOC_EXPEN(0:FINANCIAL_SIMULATION_YEARS,0:12),
     +         ASSET_CLASS_LIST(AVAIL_DATA_YEARS),
     +         ASSET_ALLOCATION_LIST(AVAIL_DATA_YEARS))
      IREC = 0
      CALL OPEN_ADDENDUM_FILE(INT2(11))
      WRITE(SCREEN_MESSAGES,"(A)") "Addendums "
      CALL MG_LOCATE_WRITE(18,70,trim(SCREEN_MESSAGES),3,2)
C
      DO
         IREC = IREC + 1
         READ(11,REC=IREC,IOSTAT=IOS) DELETE,ACCTNO,
     +                              CD_CIAC_CLASSIFICATION,
     +                              WORKING_CAPITAL_CLASSIFICATION,
     +                              ADDENDUM_VALUES(0:AVAIL_DATA_YEARS),
     +                              DATA_TYPE,
     +                              ESCAL_VECT,
     +                              ASSET_CLASS_ID,
     +                              ASSET_ALLOCATION_VECTOR,
     +                              ADDENDUM_CLASSIFICATION,
     +                              PAYMENTS_CLASSIFICATION,
     +                              INTRA_COMPANY_TRANSACTION,
     +                              INTRA_ASSET_CLASS_ID,
     +                              INTRA_ASSET_CLASS_ALLOCATION,
     +                              INTRA_ACCOUNT_CLASSIFICATION,
     +                              INTRA_ADDENDUM_COLLECTION,
     +                              BOOK_VALUES_VECTOR,
     +                              CASH_TYPE,
     +                              CASH_VALUES_VECTOR,
     +                              CASH_ACCRUAL_AMOUNT,
     +                              INVESTMENT_CLASSIFICATION,
     +                              RATE_BASE_CLASSIFICATION,
     +                              SALE_REMOVAL_CLASSIFICATION,
     +                              SHAREHOLDER_CLASSIFICATION,
     +                              TRANSFER_CLASSIFICATION,
     +                              OTHER_CLASSIFICATION,
     +                              FUNDS_CLASSIFICATION,
     +                              ACTUALS_CLASSIFICATION,
     +                              FASB143_ARO_CLASSIFICATION,
     +                              ACCOUNT_ACTIVE,
     +                              DESCRIPTION,
     +                              MIDAS_MONTHLY_BOOKED, ! 60 VALUES
     +                              TEMP_MIDAS_MONTHLY_DATA_UNITS, ! 5 VALUES
     +                              MIDAS_LAST_MONTH,   ! 5 VALUES
     +                              ANNUAL_VALUE_STATUS, ! 5 VALUES
     +                              WVPA_COMPANY_ID  ! 141
         IF(IOS /= 0) EXIT
         IF(DELETE > 7 .OR. ACCOUNT_ACTIVE == 'N') CYCLE
         IF(INDEX(ADDENDUM_CLASSIFICATION,'Not') /= 0) CYCLE
C
C ACCOUNT INFORMATION
C
         WRITE(SCREEN_MESSAGES,'(I4,A)') IREC,"-"//DESCRIPTION
         CALL MG_CLEAR_LINE_WRITE(17,70,73,trim(SCREEN_MESSAGES),3,0)
C
         IF(INDEX(ADDENDUM_CLASSIFICATION,'Cash') /= 0) THEN
            IF(INDEX(ADDENDUM_CLASSIFICATION,'Pay') /= 0) THEN
               ACCOUNT_TYPE = 'P'
            ELSE
               ACCOUNT_TYPE = 'I'
            ENDIF
         ELSE
            ACCOUNT_TYPE = ADDENDUM_CLASSIFICATION(1:1)   
         ENDIF
         DO I = 1, LAST_AVAILABLE_MONTHLY_YEAR 
            IF(INDEX(TEMP_MIDAS_MONTHLY_DATA_UNITS(I),'End of')/=0) THEN
               MIDAS_MONTHLY_DATA_UNITS(I) = 'S' 
            ELSE
               MIDAS_MONTHLY_DATA_UNITS(I) =
     +                                  TEMP_MIDAS_MONTHLY_DATA_UNITS(I) 
            ENDIF
         ENDDO
         CALL CREATE_DOLLAR_STREAM(FINANCIAL_SIMULATION_YEARS,
     +                    DATA_TYPE,ESCAL_VECT,ADDENDUM_VALUES,ADDENDUM)

C
C MONTHLY INFORMATION
C
C
C RIPPLE THE MONTHLY VALUES
C
         CLASSIFICATON_IS_ACTUAL =
     +                      INDEX(ADDENDUM_CLASSIFICATION,'Actual') /= 0
         IF(MONTHLY_MIDAS_ACTIVE) THEN
C
C MONTHLY BOOK VALUES                     
C
            SELECT CASE(trim(ADDENDUM_CLASSIFICATION))
            CASE ('Interest Rates')
               DO YR = 1, LAST_AVAILABLE_MONTHLY_YEAR
                  IF(MIDAS_MONTHLY_DATA_UNITS(YR) /= 'R') THEN
                     IF(MIDAS_MONTHLY_DATA_UNITS(YR) == 'A') THEN
                        MIDAS_MONTHLY_DATA_UNITS(YR) = 'U'
                     ELSEIF(MIDAS_MONTHLY_DATA_UNITS(YR) == 'T') THEN
                        MIDAS_MONTHLY_DATA_UNITS(YR) = 'E'
                     ELSE
                        MIDAS_MONTHLY_DATA_UNITS(YR) = 'R'
                     ENDIF
                  ENDIF
               ENDDO
            END SELECT
            CALL RIPPLE_MONTHLY_DATA(ADDENDUM(1),MIDAS_MONTHLY_BOOKED)
            CALL MAP_LAST_MONTH(MIDAS_LAST_MONTH,MONTH_ENDING)
            DO YR = 0, LAST_AVAILABLE_MONTHLY_YEAR
               ANNUAL_ADDENDUM(YR) = ADDENDUM(YR)
            ENDDO
            IF(CLASSIFICATON_IS_ACTUAL) THEN
               CALL MONTHLY_ACTUAL_VALUES(ANNUAL_ADDENDUM(1),
     +                                    MIDAS_MONTHLY_BOOKED,
     +                                    MIDAS_MONTHLY_DATA_UNITS,
     +                                    MONTH_ENDING,
     +                                    ANNUAL_VALUE_STATUS)
            ELSE
               CALL MONTHLY_BOOK_VALUES_IN_DOLLARS(ANNUAL_ADDENDUM(1),
     +                                         MIDAS_MONTHLY_BOOKED,
     +                                         MIDAS_MONTHLY_DATA_UNITS,
     +                                         MONTH_ENDING)
            ENDIF
C
            SELECT CASE(trim(ADDENDUM_CLASSIFICATION))
            CASE ('Working Capital','Rate Base','Interest Rates')
            CASE DEFAULT
               DO YR = 0, LAST_AVAILABLE_MONTHLY_YEAR
                  ADDENDUM(YR) = ANNUAL_ADDENDUM(YR)
               ENDDO
            END SELECT
         ENDIF

C ALLOCATE TO TOTAL COMPANY AND ASSET CLASSES
C
         IF(INDEX(TRANSFER_CLASSIFICATION,
     +                           'Plant Accumulated Depreciation') /= 0)
     +          TRANSFER_CLASSIFICATION = 'Cumulated Plant Depreciation'
         IF(.NOT. SAVE_BASE_CASE .AND. WVPA()) THEN
            IF(INDEX(ADDENDUM_CLASSIFICATION,'Sale/Removal') /= 0 .AND.
     +               INDEX(SALE_REMOVAL_CLASSIFICATION,
     +                                         'Net Removal') /= 0) THEN
               IN_WVPA_DATA_BASE = .TRUE. 
               DATA_BASE_CLASSIFICATION = SALE_REMOVAL_CLASSIFICATION
            ELSE
              IN_WVPA_DATA_BASE =
     +            (INDEX(ADDENDUM_CLASSIFICATION,'Transfers') /= 0 .AND.
     +                 (INDEX(TRANSFER_CLASSIFICATION,
     +                                      'Plant Additions') /= 0 .OR.
     +                  INDEX(TRANSFER_CLASSIFICATION,
     +                                    'Gross Plant Value') /= 0 .OR.
     +                  INDEX(TRANSFER_CLASSIFICATION,
     +                              'Depreciation Additions') /= 0  .OR.
     +                  INDEX(TRANSFER_CLASSIFICATION,
     +                            'Cumulated Plant Depreciation') /= 0))
               IF(IN_WVPA_DATA_BASE) 
     +                DATA_BASE_CLASSIFICATION = TRANSFER_CLASSIFICATION
            ENDIF
            IF(IN_WVPA_DATA_BASE) THEN
               ALLOC_EXPEN = 0
               DO I = 1, FINANCIAL_SIMULATION_YEARS
                  ALLOC_EXPEN(I,0) = ADDENDUM(I)
                  IF(I <= LAST_AVAILABLE_MONTHLY_YEAR) THEN
                     DO MO = 1, 12
                        ALLOC_EXPEN(I,MO) = MIDAS_MONTHLY_BOOKED(MO,I)
                     ENDDO
                  ENDIF
               ENDDO
               CALL WVPA_ADDENDUMS_DATA_BASE(ACCTNO,
     +                                       WVPA_COMPANY_ID,
     +                                       DATA_BASE_CLASSIFICATION,
     +                                       ALLOC_EXPEN,
     +                                       FINANCIAL_SIMULATION_YEARS)
            ENDIF
         ENDIF
         IF(ASSET_CLASS_ID < 0.) THEN
            CALL GET_ASSET_VAR(ABS(ASSET_CLASS_ID),
     +                                      DUMMY_TYPE,ASSET_CLASS_LIST)
            IF(ABS(ASSET_ALLOCATION_VECTOR) > 0) THEN
               CALL GET_ASSET_VAR(ABS(ASSET_ALLOCATION_VECTOR),
     +                                 DUMMY_TYPE,ASSET_ALLOCATION_LIST)
            ELSE
               ASSET_ALLOCATION_LIST = 100.
            ENDIF
         ELSE
            ASSET_CLASS_LIST(1) = FLOAT(ASSET_CLASS_ID)
            ASSET_CLASS_LIST(2) = 0.
            ASSET_ALLOCATION_LIST(1) = 100.
            ASSET_ALLOCATION_LIST(2) = 0.
         ENDIF
         CLASS_POINTER = 1
         ALLOC_EXPEN = 0.
         DO
            ASSET_CLASS = ASSET_CLASS_LIST(CLASS_POINTER)
            CALL CHECK_IF_CLASS_DEFINED(ASSET_CLASS)
            ASSET_CLASS_ID = ASSET_CLASS + 1
            IF(ASSET_CLASS >= 0) ASSET_CLASS = 
     +                               ASSET_CLASS_POINTER(ASSET_CLASS_ID)
            IF(ASSET_ALLOCATION_LIST(CLASS_POINTER) < 0.) THEN
               ALLOCATION_VECTOR =
     +                         ABS(ASSET_ALLOCATION_LIST(CLASS_POINTER))
               CALL GET_ASSET_VAR(ALLOCATION_VECTOR,
     +                                      DUMMY_TYPE,ALLOCATION_VALUE)
C              ALLOC_EXPEN(1,0) = ADDENDUM(1) * ALLOCATION_VALUE(1)/100. 
               DO I = 1, FINANCIAL_SIMULATION_YEARS
                  IF(I <= AVAIL_DATA_YEARS)
     +                        ASSET_ALLOCATOR = ALLOCATION_VALUE(I)/100.
                  IF(CLASSIFICATON_IS_ACTUAL) THEN
                     IF(ADDENDUM(I) == -99999.) THEN
                        ALLOC_EXPEN(I,0) = ADDENDUM(I)
                     ELSE
                        ALLOC_EXPEN(I,0) = ADDENDUM(I) * ASSET_ALLOCATOR 
                     ENDIF
                     IF(I <= LAST_AVAILABLE_MONTHLY_YEAR) THEN
                        IF(MONTHLY_MIDAS_ACTIVE) THEN
                           DO MO = 1, 12
                              IF(MIDAS_MONTHLY_BOOKED(MO,I) ==
     +                                                      -99999.)THEN
                                 ALLOC_EXPEN(I,MO) =
     +                                        MIDAS_MONTHLY_BOOKED(MO,I)
                              ELSE
                                 ALLOC_EXPEN(I,MO) = ASSET_ALLOCATOR * 
     +                                        MIDAS_MONTHLY_BOOKED(MO,I)
                              ENDIF
                           ENDDO
                        ELSE
                           ALLOC_EXPEN(I,1:) = ALLOC_EXPEN(I,0)/12.
                        ENDIF
                     ELSE
                        ALLOC_EXPEN(I,1:) = ALLOC_EXPEN(I,0)/12.
                     ENDIF
                  ELSE
                     ALLOC_EXPEN(I,0) = ADDENDUM(I) * ASSET_ALLOCATOR 
                     IF(I <= LAST_AVAILABLE_MONTHLY_YEAR .AND.
     +                                        MONTHLY_MIDAS_ACTIVE) THEN
                        DO MO = 1, 12
                           ALLOC_EXPEN(I,MO) = ASSET_ALLOCATOR *
     +                                        MIDAS_MONTHLY_BOOKED(MO,I)
                        ENDDO
c                        ALLOC_EXPEN(I,0) = SUM(ALLOC_EXPEN(I,1:))
                     ELSE
                        ALLOC_EXPEN(I,1:) = ALLOC_EXPEN(I,0)/12.
                     ENDIF
                  ENDIF
               ENDDO
            ELSE
               ASSET_ALLOCATOR=ASSET_ALLOCATION_LIST(CLASS_POINTER)/100.
C                        
C              ALLOC_EXPEN(1,0) = ADDENDUM(1) * ASSET_ALLOCATOR 
               DO I = 1, FINANCIAL_SIMULATION_YEARS
                  IF(CLASSIFICATON_IS_ACTUAL) THEN
                     IF(ADDENDUM(I) == -99999.) THEN
                        ALLOC_EXPEN(I,0) = ADDENDUM(I)
                     ELSE
                        ALLOC_EXPEN(I,0) = ADDENDUM(I) * ASSET_ALLOCATOR 
                     ENDIF
                     IF(I <= LAST_AVAILABLE_MONTHLY_YEAR .AND.
     +                                        MONTHLY_MIDAS_ACTIVE) THEN
                        DO MO = 1, 12
                           IF(MIDAS_MONTHLY_BOOKED(MO,I) == -99999.)THEN
                              ALLOC_EXPEN(I,MO) =
     +                                        MIDAS_MONTHLY_BOOKED(MO,I)
                           ELSE
                              ALLOC_EXPEN(I,MO) = ASSET_ALLOCATOR * 
     +                                        MIDAS_MONTHLY_BOOKED(MO,I)
                           ENDIF
                        ENDDO
                     ELSE
                        ALLOC_EXPEN(I,1:) = ALLOC_EXPEN(I,0)/12.
                     ENDIF
                  ELSE
                     ALLOC_EXPEN(I,0) = ADDENDUM(I) * ASSET_ALLOCATOR 
                     IF(I <= LAST_AVAILABLE_MONTHLY_YEAR .AND. 
     +                                        MONTHLY_MIDAS_ACTIVE) THEN
                        DO MO = 1, 12
                           ALLOC_EXPEN(I,MO) = ASSET_ALLOCATOR *
     +                                        MIDAS_MONTHLY_BOOKED(MO,I)
                        ENDDO
c                        ALLOC_EXPEN(I,0) = SUM(ALLOC_EXPEN(I,1:))
                     ELSE
                        ALLOC_EXPEN(I,1:) = ALLOC_EXPEN(I,0)/12.
                     ENDIF
                  ENDIF
               ENDDO
            ENDIF
            SELECT CASE(trim(ADDENDUM_CLASSIFICATION))
            CASE ('Payments')
                CALL ADDENDUM_ACCUMULATION(ASSET_CLASS,ALLOC_EXPEN,
     +                                    PAYMENTS_CLASSIFICATION,
     +                                    PAYMENTS,
     +                                    FINANCIAL_SIMULATION_YEARS,
     +                                    NUM_OF_ASSET_CLASSES)
            CASE ('Investments')
                CALL ADDENDUM_ACCUMULATION(ASSET_CLASS,ALLOC_EXPEN,
     +                                    INVESTMENT_CLASSIFICATION,
     +                                    INVESTMENTS,
     +                                    FINANCIAL_SIMULATION_YEARS,
     +                                    NUM_OF_ASSET_CLASSES)
            CASE ('Fund Earnings','Interest Rates')
                CALL ADDENDUM_ACCUMULATION(ASSET_CLASS,ALLOC_EXPEN,
     +                                    FUNDS_CLASSIFICATION,
     +                                    FUNDS_EARNINGS,
     +                                    FINANCIAL_SIMULATION_YEARS,
     +                                    NUM_OF_ASSET_CLASSES)
            CASE ('Rate Base')
                CALL ADDENDUM_ACCUMULATION(ASSET_CLASS,ALLOC_EXPEN,
     +                                    RATE_BASE_CLASSIFICATION,
     +                                    RATE_BASE,
     +                                    FINANCIAL_SIMULATION_YEARS,
     +                                    NUM_OF_ASSET_CLASSES)
            CASE ('Working Capital','Inventories')
                CALL ADDENDUM_ACCUMULATION(ASSET_CLASS,ALLOC_EXPEN,
     +                                   WORKING_CAPITAL_CLASSIFICATION,
     +                                   WORKING_CAPITAL,
     +                                   FINANCIAL_SIMULATION_YEARS,
     +                                   NUM_OF_ASSET_CLASSES)
            CASE ('CD/CIAC')
                CALL ADDENDUM_ACCUMULATION(ASSET_CLASS,ALLOC_EXPEN,
     +                                    CD_CIAC_CLASSIFICATION,
     +                                    CD_CIAC,
     +                                    FINANCIAL_SIMULATION_YEARS,
     +                                    NUM_OF_ASSET_CLASSES)
            CASE ('Sale/Removal of Assets','Sale/Removal')
                CALL ADDENDUM_ACCUMULATION(ASSET_CLASS,ALLOC_EXPEN,
     +                                    SALE_REMOVAL_CLASSIFICATION,
     +                                    SALE_REMOVAL,
     +                                    FINANCIAL_SIMULATION_YEARS,
     +                                    NUM_OF_ASSET_CLASSES)
            CASE ('Shareholder Value')
                CALL ADDENDUM_ACCUMULATION(ASSET_CLASS,ALLOC_EXPEN,
     +                                    SHAREHOLDER_CLASSIFICATION,
     +                                    SHAREHOLDER_VALUE,
     +                                    FINANCIAL_SIMULATION_YEARS,
     +                                    NUM_OF_ASSET_CLASSES)
            CASE ('Transfers')
               IF(WVPA() .AND. (INDEX(TRANSFER_CLASSIFICATION,
     +                              'Depreciation Additions') /= 0))then !  .OR.
c     +                 INDEX(TRANSFER_CLASSIFICATION,
c     +                       'Cumulated Plant Depreciation') /= 0)) THEN
                  CALL WVPA_ADDENDUM_ACC(ASSET_CLASS,ALLOC_EXPEN,
     +                                   WVPA_ADDENDUM_ITEMS,
     +                                   FINANCIAL_SIMULATION_YEARS,
     +                                   NUM_OF_ASSET_CLASSES)
               ELSE
                  CALL ADDENDUM_ACCUMULATION(ASSET_CLASS,ALLOC_EXPEN,
     +                                       TRANSFER_CLASSIFICATION,
     +                                       TRANSFER_VALUE,
     +                                       FINANCIAL_SIMULATION_YEARS,
     +                                       NUM_OF_ASSET_CLASSES)
               ENDIF
            CASE ('Cash')
                CALL ADDENDUM_ACCUMULATION(ASSET_CLASS,ALLOC_EXPEN,
     +                                     TRANSFER_CLASSIFICATION,
     +                                     CASH_MONTHLY_VALUES,
     +                                     FINANCIAL_SIMULATION_YEARS,
     +                                     NUM_OF_ASSET_CLASSES)
            CASE ('Actuals')
                CALL ACTUAL_ACCUMULATION(ASSET_CLASS,ALLOC_EXPEN,
     +                                   ACTUALS_CLASSIFICATION,
     +                                   ACTUAL_MONTHLY_VALUES,
     +                                   FINANCIAL_SIMULATION_YEARS,
     +                                   NUM_OF_ASSET_CLASSES)
            CASE ('Other','IPL ElectPlan')
                CALL ADDENDUM_ACCUMULATION(ASSET_CLASS,ALLOC_EXPEN,
     +                                     OTHER_CLASSIFICATION,
     +                                     OTHER_VALUE,
     +                                     FINANCIAL_SIMULATION_YEARS,
     +                                     NUM_OF_ASSET_CLASSES)
            CASE ('FASB 143/ARO','FASB143/ARO','FASB87')
                CALL ADDENDUM_ACCUMULATION(ASSET_CLASS,ALLOC_EXPEN,
     +                                     FASB143_ARO_CLASSIFICATION,
     +                                     FASB143_ARO_VALUE,
     +                                     FINANCIAL_SIMULATION_YEARS,
     +                                     NUM_OF_ASSET_CLASSES)
            CASE ('Capital Expenditures')
                CALL ADDENDUM_ACCUMULATION(ASSET_CLASS,ALLOC_EXPEN,
     +                                     FASB143_ARO_CLASSIFICATION,
     +                                     CapX_Values,
     +                                     FINANCIAL_SIMULATION_YEARS,
     +                                     NUM_OF_ASSET_CLASSES)
            END SELECT
            CLASS_POINTER = CLASS_POINTER + 1
            IF(CLASS_POINTER > AVAIL_DATA_YEARS) EXIT
            IF(ASSET_CLASS_LIST(CLASS_POINTER) == 0. .OR. 
     +                      ASSET_CLASS_LIST(CLASS_POINTER) == -99.)EXIT
         ENDDO
      ENDDO
C
      CALL CLOSE_ADDENDUM_FILE
      DEALLOCATE(ESCALATION_RATES,ADDENDUM_VALUES,ALLOC_EXPEN,ADDENDUM,
     +           ASSET_CLASS_LIST,ASSET_ALLOCATION_LIST)
      IF(SAVE_BASE_CASE) THEN
         CALL OPEN_ADDENDUM_BASE_CASE_FILE(INT2(10))
         WRITE(10) PAYMENTS,
     +             INVESTMENTS,
     +             RATE_BASE,
     +             WORKING_CAPITAL,
     +             CD_CIAC,
     +             SALE_REMOVAL,
     +             SHAREHOLDER_VALUE,
     +             TRANSFER_VALUE,
     +             FUNDS_EARNINGS,
     +             OTHER_VALUE,
     +             ACTUAL_MONTHLY_VALUES,
     +             ACTUAL_VALUE_FOUND,
     +             FASB143_ARO_VALUE,
     +             CapX_Values,
     +             WVPA_ADDENDUM_ITEMS,
     +             CASH_MONTHLY_VALUES

         CLOSE(10)
      ENDIF
      CALL write_scroll_line_RW(' ',2)
      CALL write_scroll_line_RW(' ',0)
      CALL DISPLAY_TIME
      RETURN
C***********************************************************************
      ENTRY SET_UP_ADDENDUM_ARRAYS
C***********************************************************************
C         CALL RETURN_NUM_OF_ADDENDUM_CLASSES(NUM_OF_ASSET_CLASSES,
C     +                                       MAX_ASSET_CLASS_NUM)
         CALL RETURN_INITIALIZATION_CLASSES(NUM_OF_ASSET_CLASSES,
     +                                      MAX_ASSET_CLASS_NUM)
         IF(ALLOCATED(ASSET_CLASS_POINTER))
     +                                   DEALLOCATE(ASSET_CLASS_POINTER)
         IF(MAX_ASSET_CLASS_NUM > 0) THEN
            ALLOCATE(ASSET_CLASS_POINTER(MAX_ASSET_CLASS_NUM))
            CALL RETURN_INITIALIZATION_POINTER(ASSET_CLASS_POINTER)
C            CALL RETURN_ADDENDUM_CLASS_POINTER(ASSET_CLASS_POINTER)
         ENDIF
C
         IF(ALLOCATED(PAYMENTS)) DEALLOCATE(PAYMENTS,
     +                                      INVESTMENTS,
     +                                      RATE_BASE,
     +                                      WORKING_CAPITAL,
     +                                      CD_CIAC,
     +                                      SALE_REMOVAL,
     +                                      SHAREHOLDER_VALUE,
     +                                      TRANSFER_VALUE,
     +                                      CASH_MONTHLY_VALUES,
     +                                      FUNDS_EARNINGS,
     +                                      ACTUAL_MONTHLY_VALUES,
     +                                      FASB143_ARO_VALUE,
     +                                      CapX_Values,
     +                                      WVPA_ADDENDUM_ITEMS,
     +                                      ACTUAL_VALUE_FOUND,
     +                                      OTHER_VALUE)
         ALLOCATE(PAYMENTS(0:12,0:FINANCIAL_SIMULATION_YEARS,
     +                       -1:NUM_OF_ASSET_CLASSES,LAST_PAYMENT_ITEM),
     +            INVESTMENTS(0:12,0:FINANCIAL_SIMULATION_YEARS,
     +                    -1:NUM_OF_ASSET_CLASSES,LAST_INVESTMENT_ITEM),
     +            RATE_BASE(0:12,0:FINANCIAL_SIMULATION_YEARS,
     +                     -1:NUM_OF_ASSET_CLASSES,LAST_RATE_BASE_ITEM),
     +            WORKING_CAPITAL(0:12,0:FINANCIAL_SIMULATION_YEARS,
     +               -1:NUM_OF_ASSET_CLASSES,LAST_WORKING_CAPITAL_ITEM),
     +            CD_CIAC(0:12,0:FINANCIAL_SIMULATION_YEARS,
     +                       -1:NUM_OF_ASSET_CLASSES,LAST_CD_CIAC_ITEM),
     +            SALE_REMOVAL(0:12,0:FINANCIAL_SIMULATION_YEARS,
     +                  -1:NUM_OF_ASSET_CLASSES,LAST_SALE_REMOVAL_ITEM),
     +            SHAREHOLDER_VALUE(0:12,0:FINANCIAL_SIMULATION_YEARS,
     +             -1:NUM_OF_ASSET_CLASSES,LAST_SHAREHOLDER_VALUE_ITEM),
     +            TRANSFER_VALUE(0:12,0:FINANCIAL_SIMULATION_YEARS,
     +                      -1:NUM_OF_ASSET_CLASSES,LAST_TRANSFER_ITEM),
     +            CASH_MONTHLY_VALUES(0:12,0:FINANCIAL_SIMULATION_YEARS,
     +                      -1:NUM_OF_ASSET_CLASSES,LAST_CASH_TO_ITEMS),
     +            FUNDS_EARNINGS(0:12,0:FINANCIAL_SIMULATION_YEARS,
     +                      -1:NUM_OF_ASSET_CLASSES,LAST_EARNINGS_ITEM),
     +            OTHER_VALUE(0:12,0:FINANCIAL_SIMULATION_YEARS,
     +                         -1:NUM_OF_ASSET_CLASSES,LAST_OTHER_ITEM),
     +            ACTUAL_MONTHLY_VALUES(0:12,
     +                      0:FINANCIAL_SIMULATION_YEARS,
     +                        -1:NUM_OF_ASSET_CLASSES,LAST_ACTUAL_ITEM),
     +            FASB143_ARO_VALUE(0:12,
     +                      0:FINANCIAL_SIMULATION_YEARS,
     +                          -1:NUM_OF_ASSET_CLASSES,LAST_FASB_ITEM),
     +            CapX_Values(0:12,0:FINANCIAL_SIMULATION_YEARS,
     +                          -1:NUM_OF_ASSET_CLASSES,LAST_CAPX_ITEM),    
     +            WVPA_ADDENDUM_ITEMS(0:12,0:FINANCIAL_SIMULATION_YEARS,
     +                          -1:NUM_OF_ASSET_CLASSES,LAST_CAPX_ITEM),    
     +            ACTUAL_VALUE_FOUND(-1:NUM_OF_ASSET_CLASSES,
     +                                                LAST_ACTUAL_ITEM))
C
         FALSE_BYTE = .FALSE.
         ACTUAL_VALUE_FOUND = FALSE_BYTE
C
         ACTUAL_MONTHLY_VALUES = -99999.
C
         FASB143_ARO_VALUE = 0.
C
         PAYMENTS = 0.
C
         INVESTMENTS = 0.
C
         RATE_BASE = 0.
C
         WORKING_CAPITAL = 0.
C
         CD_CIAC = 0.
C
         SALE_REMOVAL = 0.
C
         SHAREHOLDER_VALUE = 0.
C
         TRANSFER_VALUE = 0.
         CASH_MONTHLY_VALUES = 0.
C
         FUNDS_EARNINGS = 0.
C
         CapX_Values = 0.
C
         WVPA_ADDENDUM_ITEMS = 0.
C         
         OTHER_VALUE = 0.
      RETURN
C***********************************************************************
      ENTRY READ_ADDENDUM_BASE_CASE
C***********************************************************************
         CALL SET_UP_ADDENDUM_ARRAYS
         CALL OPEN_ADDENDUM_BASE_CASE_FILE(INT2(10))
         READ(10) PAYMENTS,
     +            INVESTMENTS,
     +            RATE_BASE,
     +            WORKING_CAPITAL,
     +            CD_CIAC,
     +            SALE_REMOVAL,
     +            SHAREHOLDER_VALUE,
     +            TRANSFER_VALUE,
     +            FUNDS_EARNINGS,
     +            OTHER_VALUE,
     +            ACTUAL_MONTHLY_VALUES,
     +            ACTUAL_VALUE_FOUND,
     +            FASB143_ARO_VALUE,
     +            CapX_Values,
     +            WVPA_ADDENDUM_ITEMS,
     +            CASH_MONTHLY_VALUES
         CLOSE(10)
      RETURN
C***********************************************************************
      ENTRY ADDENDUM_ACCUMULATION(R_ASSET_CLASS,R_ALLOC_EXPEN,
     +                            R_ADDENDUM_CLASSIFICATION,
     +                            R_ADDENDUM_ARRAY,
     +                            R_FINANCIAL_SIMULATION_YEARS,
     +                            R_NUM_OF_ASSET_CLASSES)
C***********************************************************************
C
         ADDENDUM_TYPE = ADDENDUM_POSITION(R_ADDENDUM_CLASSIFICATION,
     +                                     ADDED_TO_CURRENT_VALUE)
         IF(ADDENDUM_TYPE /= -99) THEN
            IF(ADDED_TO_CURRENT_VALUE) THEN
               DO I = 0, R_FINANCIAL_SIMULATION_YEARS
                  DO MO = 0, 12
                     R_ADDENDUM_ARRAY(MO,I,R_ASSET_CLASS,ADDENDUM_TYPE)= 
     +                R_ADDENDUM_ARRAY(MO,I,R_ASSET_CLASS,ADDENDUM_TYPE)
     +                                            +  R_ALLOC_EXPEN(I,MO)
                  ENDDO
               ENDDO
            ELSE ! REPLACE
               DO I = 0, R_FINANCIAL_SIMULATION_YEARS
                  DO MO = 0, 12
                     R_ADDENDUM_ARRAY(MO,I,R_ASSET_CLASS,ADDENDUM_TYPE)=
     +                                               R_ALLOC_EXPEN(I,MO)
                  ENDDO
               ENDDO
            ENDIF   
         ENDIF
C
      RETURN
C***********************************************************************
      ENTRY WVPA_ADDENDUM_ACC(R_ASSET_CLASS,
     +                        R_ALLOC_EXPEN,
     +                        R_ADDENDUM_ARRAY,
     +                        R_FINANCIAL_SIMULATION_YEARS,
     +                        R_NUM_OF_ASSET_CLASSES)
C***********************************************************************
C
         DO I = 0, R_FINANCIAL_SIMULATION_YEARS
c            DO MO = 0, 12
               R_ADDENDUM_ARRAY(:,I,R_ASSET_CLASS,1) = 
     +                            R_ADDENDUM_ARRAY(:,I,R_ASSET_CLASS,1) ! DEPRECIATION
     +                            +  R_ALLOC_EXPEN(I,:)
c            ENDDO
         ENDDO
      RETURN
C***********************************************************************
      ENTRY ACTUAL_ACCUMULATION(R_ASSET_CLASS,R_ALLOC_EXPEN,
     +                          R_ADDENDUM_CLASSIFICATION,
     +                          R_ADDENDUM_ARRAY,
     +                          R_FINANCIAL_SIMULATION_YEARS,
     +                          R_NUM_OF_ASSET_CLASSES)
C***********************************************************************
C
         ADDENDUM_TYPE = ADDENDUM_POSITION(R_ADDENDUM_CLASSIFICATION,
     +                                     ADDED_TO_CURRENT_VALUE)
         IF(ADDENDUM_TYPE /= -99) THEN
            IF(.NOT.ACTUAL_VALUE_FOUND(R_ASSET_CLASS,ADDENDUM_TYPE))THEN
               DO I = 1, R_FINANCIAL_SIMULATION_YEARS
                  DO MO = 0, 12
                     IF(R_ALLOC_EXPEN(I,MO) == -99999.) CYCLE
                     IF(R_ADDENDUM_ARRAY(MO,I,
     +                     R_ASSET_CLASS,ADDENDUM_TYPE) == -99999.) THEN
                        R_ADDENDUM_ARRAY(MO,I,
     +                                 R_ASSET_CLASS,ADDENDUM_TYPE) = 0.
C                       R_ADDENDUM_ARRAY(0,I,
C    +                                 R_ASSET_CLASS,ADDENDUM_TYPE) = 0.
                     ENDIF
                     R_ADDENDUM_ARRAY(MO,I,R_ASSET_CLASS,ADDENDUM_TYPE)= 
     +                R_ADDENDUM_ARRAY(MO,I,R_ASSET_CLASS,ADDENDUM_TYPE)
     +                +  R_ALLOC_EXPEN(I,MO)
                  ENDDO
               ENDDO
               ACTUAL_VALUE_FOUND(R_ASSET_CLASS,ADDENDUM_TYPE) = .TRUE.
            ENDIF
         ENDIF
C
      RETURN
C**********************************************************************
      ENTRY WVPA_SPECIAL_ADDENDUMS(R_CLASS,R_YR,R_BOOK_DEPRECIATION)
C**********************************************************************
         IF(ALLOCATED(WVPA_ADDENDUM_ITEMS)) THEN
            IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
               IF(R_CLASS == 0) THEN
                  ASSET_CLASS = 0
               ELSE
                  ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
               ENDIF
               IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN 
                  R_BOOK_DEPRECIATION = R_BOOK_DEPRECIATION
     +                       + WVPA_ADDENDUM_ITEMS(0,R_YR,ASSET_CLASS,1)
               ENDIF
            ENDIF
         ENDIF
      RETURN
C**********************************************************************
      ENTRY WVPA_MONTHLY_SPECIAL_ADDENDUMS(R_CLASS,R_YR,
     +                                     R_MONTHLY_BOOK_DEPRECIATION)
C**********************************************************************
         IF(ALLOCATED(WVPA_ADDENDUM_ITEMS)) THEN
            IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
               IF(R_CLASS == 0) THEN
                  ASSET_CLASS = 0
               ELSE
                  ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
               ENDIF
               IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN 
                  R_MONTHLY_BOOK_DEPRECIATION(:) = 
     +                       R_MONTHLY_BOOK_DEPRECIATION(:)
     +                       + WVPA_ADDENDUM_ITEMS(:,R_YR,ASSET_CLASS,1)
               ENDIF
            ENDIF
         ENDIF
      RETURN
C**********************************************************************
      ENTRY RETURN_PAYMENT_ADDENDUMS(R_YR,R_CLASS,
     +                               R_DECOMMISSIONING_FUND,
     +                               R_DECOMMISSIONING_LIABILITY,
     +                               R_POST_RETIREMENT_MEDICAL,
     +                               R_RETIREE_MEDICAL_PAYMENTS)
C**********************************************************************
C
C
         IF(ALLOCATED(PAYMENTS)) THEN
            IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
               YR = R_YR
               IF(R_CLASS == 0) THEN
                  ASSET_CLASS = 0
               ELSE
                  ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
               ENDIF
               IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN 
                  R_DECOMMISSIONING_FUND = R_DECOMMISSIONING_FUND +
     +                                      PAYMENTS(0,YR,ASSET_CLASS,1)
                  R_DECOMMISSIONING_LIABILITY =
     +                                     R_DECOMMISSIONING_LIABILITY +
     +                                      PAYMENTS(0,YR,ASSET_CLASS,2)
                  R_POST_RETIREMENT_MEDICAL=R_POST_RETIREMENT_MEDICAL +
     +                                      PAYMENTS(0,YR,ASSET_CLASS,3)
                  R_RETIREE_MEDICAL_PAYMENTS =
     +                                      R_RETIREE_MEDICAL_PAYMENTS +
     +                                      PAYMENTS(0,YR,ASSET_CLASS,4)
               ENDIF
            ENDIF
         ENDIF
      RETURN
C**********************************************************************
      ENTRY RETURN_BAL_SHEET_LIAB_PAYMENTS(R_YR,R_CLASS,
     +                                     R_PENSION_EXPENSE_CASH,
     +                                     R_STORM_EXPENSE_CASH,
     +                                     R_VACATION_PAY_CASH)
C**********************************************************************
C
         IF(ALLOCATED(PAYMENTS)) THEN
            IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
               YR = R_YR
               IF(R_CLASS == 0) THEN
                  ASSET_CLASS = 0
               ELSE
                  ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
               ENDIF
               IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN 
                  R_PENSION_EXPENSE_CASH = R_PENSION_EXPENSE_CASH
     +                                    + PAYMENTS(0,YR,ASSET_CLASS,6)
                  R_STORM_EXPENSE_CASH = R_STORM_EXPENSE_CASH
     +                                   + PAYMENTS(0,YR,ASSET_CLASS,7)
                  R_VACATION_PAY_CASH = R_VACATION_PAY_CASH
     +                                  + PAYMENTS(0,YR,ASSET_CLASS,8)
               ENDIF
            ENDIF
         ENDIF
      RETURN
C**********************************************************************
      ENTRY RETURN_ACTUAL_MONTHLY_VALUES(R_YR,R_CLASS,
     +                                   R_ACTUAL_DEFERRED_TAXES_CR,
     +                                   R_ACTUAL_FED_INCOME_TAXES,
     +                                   R_ACTUAL_STATE_INCOME_TAXES,
     +                                   R_ACTUAL_DEFERRED_TAXES_DR)
C**********************************************************************
C
C
         IF(ALLOCATED(ACTUAL_MONTHLY_VALUES)) THEN
            IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
               YR = R_YR
               IF(R_CLASS == 0) THEN
                  ASSET_CLASS = 0
               ELSE
                  ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
               ENDIF
               IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN
C
C DEFERRED TAXES CR
C
                  IF(ACTUAL_VALUE_FOUND(ASSET_CLASS,1)) THEN
                     IF(ACTUAL_MONTHLY_VALUES(0,YR,ASSET_CLASS,1) ==
     +                                                     -99999.) THEN 
                        ANNUAL_TARGET_VALUE = 
     +                                     R_ACTUAL_DEFERRED_TAXES_CR(0)
                     ELSE
                        ANNUAL_TARGET_VALUE = 
     +                         ACTUAL_MONTHLY_VALUES(0,YR,ASSET_CLASS,1)
                     ENDIF
                     ANNUAL_AMOUNT = 0.
                     DO MO = 1, 12
                        IF(ACTUAL_MONTHLY_VALUES(MO,
     +                                YR,ASSET_CLASS,1) == -99999.) THEN
                           UNALLOCATED_BALANCE = 0.
                           DO MO2 = MO, 12
                              UNALLOCATED_BALANCE = UNALLOCATED_BALANCE
     +                                 + R_ACTUAL_DEFERRED_TAXES_CR(MO2)
                           ENDDO
                           REMAINING_ANNUAL_BALANCE= ANNUAL_TARGET_VALUE
     +                                               - ANNUAL_AMOUNT
                           IF(UNALLOCATED_BALANCE /= 0.) THEN
                              RATIO = REMAINING_ANNUAL_BALANCE/
     +                                   UNALLOCATED_BALANCE
                           ELSE
                              RATIO = 1./FLOAT(13-MO)
                           ENDIF
                           DO MO2 = MO, 12
                              R_ACTUAL_DEFERRED_TAXES_CR(MO2) = RATIO *
     +                                   R_ACTUAL_DEFERRED_TAXES_CR(MO2)
                              ANNUAL_AMOUNT = ANNUAL_AMOUNT
     +                                 + R_ACTUAL_DEFERRED_TAXES_CR(MO2)
                           ENDDO
                           EXIT
                        ENDIF
                        R_ACTUAL_DEFERRED_TAXES_CR(MO) =
     +                        ACTUAL_MONTHLY_VALUES(MO,YR,ASSET_CLASS,1)
                        ANNUAL_AMOUNT = ANNUAL_AMOUNT
     +                                  + R_ACTUAL_DEFERRED_TAXES_CR(MO)
                     ENDDO
                     R_ACTUAL_DEFERRED_TAXES_CR(0) = ANNUAL_AMOUNT
                  ENDIF
C
C FEDERAL TAXES PAID
C
                  IF(ACTUAL_VALUE_FOUND(ASSET_CLASS,2)) THEN
                     ANNUAL_AMOUNT = 0.
                     DO MO = 1, 12
                        IF(ACTUAL_MONTHLY_VALUES(MO,
     +                                YR,ASSET_CLASS,2) == -99999.) THEN
                           IF(ACTUAL_MONTHLY_VALUES(0,YR,
     +                                   ASSET_CLASS,2) == -99999.) THEN 
                              DO MO2 = MO, 12
                                 ANNUAL_AMOUNT = ANNUAL_AMOUNT
     +                                   + R_ACTUAL_FED_INCOME_TAXES(MO)
                              ENDDO
                           ELSE ! MARK TO INPUT VALUE
                              UNALLOCATED_BALANCE = 0.
                              DO MO2 = MO, 12
                                 UNALLOCATED_BALANCE =
     +                                  UNALLOCATED_BALANCE
     +                                  + R_ACTUAL_FED_INCOME_TAXES(MO2)
                              ENDDO
                              REMAINING_ANNUAL_BALANCE =
     +                         ACTUAL_MONTHLY_VALUES(0,YR,ASSET_CLASS,2)
     +                         - ANNUAL_AMOUNT
                              IF(UNALLOCATED_BALANCE /= 0.) THEN
                                 RATIO = REMAINING_ANNUAL_BALANCE/
     +                                      UNALLOCATED_BALANCE
                              ELSE
                                 RATIO = 1./FLOAT(13-MO)
                              ENDIF
                              DO MO2 = MO, 12
                                 R_ACTUAL_FED_INCOME_TAXES(MO2) = RATIO*
     +                                    R_ACTUAL_FED_INCOME_TAXES(MO2)
                                 ANNUAL_AMOUNT = ANNUAL_AMOUNT
     +                                  + R_ACTUAL_FED_INCOME_TAXES(MO2)
                              ENDDO
                           ENDIF
                           EXIT
                        ENDIF
                        R_ACTUAL_FED_INCOME_TAXES(MO) =
     +                        ACTUAL_MONTHLY_VALUES(MO,YR,ASSET_CLASS,2)
                        ANNUAL_AMOUNT = ANNUAL_AMOUNT
     +                                  + R_ACTUAL_FED_INCOME_TAXES(MO)
                     ENDDO
                     R_ACTUAL_FED_INCOME_TAXES(0) = ANNUAL_AMOUNT
                  ENDIF
C
C STATE TAXES PAID
C
                  IF(ACTUAL_VALUE_FOUND(ASSET_CLASS,3)) THEN
                     ANNUAL_AMOUNT = 0.
                     DO MO = 1, 12
                        IF(ACTUAL_MONTHLY_VALUES(MO,
     +                                YR,ASSET_CLASS,3) == -99999.) THEN
                           IF(ACTUAL_MONTHLY_VALUES(0,YR,
     +                                   ASSET_CLASS,3) == -99999.) THEN 
                              DO MO2 = MO, 12
                                 ANNUAL_AMOUNT = ANNUAL_AMOUNT
     +                                 + R_ACTUAL_STATE_INCOME_TAXES(MO)
                              ENDDO
                           ELSE ! MARK TO INPUT VALUE
                              UNALLOCATED_BALANCE = 0.
                              DO MO2 = MO, 12
                                 UNALLOCATED_BALANCE =
     +                                UNALLOCATED_BALANCE
     +                                + R_ACTUAL_STATE_INCOME_TAXES(MO2)
                              ENDDO
                              REMAINING_ANNUAL_BALANCE =
     +                         ACTUAL_MONTHLY_VALUES(0,YR,ASSET_CLASS,3)
     +                         - ANNUAL_AMOUNT
                              IF(UNALLOCATED_BALANCE /= 0.) THEN
                                 RATIO = REMAINING_ANNUAL_BALANCE/
     +                                      UNALLOCATED_BALANCE
                              ELSE
                                 RATIO = 1./FLOAT(13-MO)
                              ENDIF
                              DO MO2 = MO, 12
                                 R_ACTUAL_STATE_INCOME_TAXES(MO2)=RATIO*
     +                                  R_ACTUAL_STATE_INCOME_TAXES(MO2)
                                 ANNUAL_AMOUNT = ANNUAL_AMOUNT
     +                                + R_ACTUAL_STATE_INCOME_TAXES(MO2)
                              ENDDO
                           ENDIF
                           EXIT
                        ENDIF
                        R_ACTUAL_STATE_INCOME_TAXES(MO) =
     +                        ACTUAL_MONTHLY_VALUES(MO,YR,ASSET_CLASS,3)
                        ANNUAL_AMOUNT = ANNUAL_AMOUNT
     +                                 + R_ACTUAL_STATE_INCOME_TAXES(MO)
                     ENDDO
                     R_ACTUAL_STATE_INCOME_TAXES(0)=ANNUAL_AMOUNT
                  ENDIF
C
C DEFERRED TAXES DR
C
                  IF(ACTUAL_VALUE_FOUND(ASSET_CLASS,4)) THEN
                     IF(ACTUAL_MONTHLY_VALUES(0,YR,ASSET_CLASS,4) ==
     +                                                     -99999.) THEN 
                        ANNUAL_TARGET_VALUE = 
     +                                     R_ACTUAL_DEFERRED_TAXES_DR(0)
                     ELSE
                        ANNUAL_TARGET_VALUE = 
     +                         ACTUAL_MONTHLY_VALUES(0,YR,ASSET_CLASS,4)
                     ENDIF
                     ANNUAL_AMOUNT = 0.
                     DO MO = 1, 12
                        IF(ACTUAL_MONTHLY_VALUES(MO,
     +                                YR,ASSET_CLASS,4) == -99999.) THEN
                           UNALLOCATED_BALANCE = 0.
                           DO MO2 = MO, 12
                              UNALLOCATED_BALANCE = UNALLOCATED_BALANCE
     +                                 + R_ACTUAL_DEFERRED_TAXES_DR(MO2)
                           ENDDO
                           REMAINING_ANNUAL_BALANCE= ANNUAL_TARGET_VALUE
     +                                               - ANNUAL_AMOUNT
                           IF(UNALLOCATED_BALANCE /= 0.) THEN
                              RATIO = REMAINING_ANNUAL_BALANCE/
     +                                   UNALLOCATED_BALANCE
                           ELSE
                              RATIO = 1./FLOAT(13-MO)
                           ENDIF
                           DO MO2 = MO, 12
                              R_ACTUAL_DEFERRED_TAXES_DR(MO2) = RATIO *
     +                                   R_ACTUAL_DEFERRED_TAXES_DR(MO2)
                              ANNUAL_AMOUNT = ANNUAL_AMOUNT
     +                                 + R_ACTUAL_DEFERRED_TAXES_DR(MO2)
                           ENDDO
                           EXIT
                        ENDIF
                        R_ACTUAL_DEFERRED_TAXES_DR(MO) =
     +                        ACTUAL_MONTHLY_VALUES(MO,YR,ASSET_CLASS,4)
                        ANNUAL_AMOUNT = ANNUAL_AMOUNT
     +                                  + R_ACTUAL_DEFERRED_TAXES_DR(MO)
                     ENDDO
                     R_ACTUAL_DEFERRED_TAXES_DR(0) = ANNUAL_AMOUNT
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      RETURN
C**********************************************************************
      ENTRY RETURN_INVESTMENT_ADDENDUMS(R_YR,R_CLASS,
     +                                  R_DIVIDEND_SUBSIDIARY,
     +                                  R_NET_INCOME_SUBSIDIARY,
     +                                  R_INVESTMENT_IN_SUBSIDIARY,
     +                                  R_OTHER_INVESTMENTS,
     +                                  R_CAPITIALIZED_LEASE_ADDITIONS,
     +                                  R_CASH_FOR_ASSETS,
     +                                  R_CASH_CHANGE_LTI)
C**********************************************************************
C
C
         R_CASH_FOR_ASSETS = 0.
C
         IF(ALLOCATED(INVESTMENTS)) THEN
            IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
               YR = R_YR
               IF(R_CLASS == 0) THEN
                  ASSET_CLASS = 0
               ELSE
                  ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
               ENDIF
               IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN 
                  R_DIVIDEND_SUBSIDIARY = R_DIVIDEND_SUBSIDIARY +
     +                                   INVESTMENTS(0,YR,ASSET_CLASS,1)
                  R_NET_INCOME_SUBSIDIARY = R_NET_INCOME_SUBSIDIARY +
     +                                   INVESTMENTS(0,YR,ASSET_CLASS,2)
                  R_INVESTMENT_IN_SUBSIDIARY=R_INVESTMENT_IN_SUBSIDIARY+
     +                                   INVESTMENTS(0,YR,ASSET_CLASS,3)
                  R_OTHER_INVESTMENTS = R_OTHER_INVESTMENTS +
     +                                   INVESTMENTS(0,YR,ASSET_CLASS,4)
                  R_CAPITIALIZED_LEASE_ADDITIONS =
     +                                  R_CAPITIALIZED_LEASE_ADDITIONS +
     +                                   INVESTMENTS(0,YR,ASSET_CLASS,5)
                  R_CASH_FOR_ASSETS = INVESTMENTS(0,YR,ASSET_CLASS,6)
                  R_CASH_CHANGE_LTI = INVESTMENTS(0,YR,ASSET_CLASS,7)
               ENDIF
            ENDIF
         ENDIF
      RETURN
C**********************************************************************
      ENTRY RETURN_RATE_BASE_ADDENDUMS(R_YR,R_CLASS,
     +                                 R_ASSETS_NEC_ADJUSTMENT,
     +                                 R_RATE_BASE_ADJUSTMENT)
C**********************************************************************
C
C
         IF(ALLOCATED(RATE_BASE)) THEN
            IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
               YR = R_YR
               IF(R_CLASS == 0) THEN
                  ASSET_CLASS = 0
               ELSE
                  ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
               ENDIF
               IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN 
                  R_ASSETS_NEC_ADJUSTMENT = R_ASSETS_NEC_ADJUSTMENT +
     +                                     RATE_BASE(0,YR,ASSET_CLASS,1)
                  R_RATE_BASE_ADJUSTMENT = R_RATE_BASE_ADJUSTMENT +
     +                                     RATE_BASE(0,YR,ASSET_CLASS,2)
               ENDIF
            ENDIF
         ENDIF
      RETURN
C**********************************************************************
      ENTRY RETURN_FASB_ADDENDUMS(R_YR,R_CLASS,
     +                            R_NucDecom_Discount_Rate,
     +                            R_NucDecom_Interest_Accretion,
     +                            R_ARO_Cash_Payment,
     +                            R_ARO_Trust_Cash_Payment)
C**********************************************************************
C
C
         IF(ALLOCATED(FASB143_ARO_VALUE)) THEN
            IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
               YR = R_YR
               IF(R_CLASS == 0) THEN
                  ASSET_CLASS = 0
               ELSE
                  ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
               ENDIF
               IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN 
                  R_NucDecom_Discount_Rate =
     +                             FASB143_ARO_VALUE(0,YR,ASSET_CLASS,1)
                  R_NucDecom_Interest_Accretion =
     +                             FASB143_ARO_VALUE(0,YR,ASSET_CLASS,2)
                  R_ARO_Cash_Payment =
     +                             FASB143_ARO_VALUE(0,YR,ASSET_CLASS,7)
                  R_ARO_Trust_Cash_Payment =
     +                             FASB143_ARO_VALUE(0,YR,ASSET_CLASS,8)
               ENDIF
            ENDIF
         ENDIF
      RETURN
C**********************************************************************
      ENTRY RETURN_MONTHLY_FASB_ADDENDUMS(R_YR,R_CLASS,
     +                                 R_MO_NucDecom_Discount_Rate,
     +                                 R_MO_NucDecom_Interest_Accretion,
     +                                 R_MO_ARO_Cash_Payment,
     +                                 R_MO_ARO_Trust_Cash_Payment,
     +                                 R_MO_ARO_NET_ASSETS,
     +                                 R_MO_ARO_LIABILITY)
C**********************************************************************
C
C
          R_MO_NucDecom_Discount_Rate = 0.
          R_MO_NucDecom_Interest_Accretion = 0.
          R_MO_ARO_NET_ASSETS = 0.
          R_MO_ARO_LIABILITY = 0.
          R_MO_ARO_Cash_Payment = 0.
          R_MO_ARO_Trust_Cash_Payment = 0.
C
         IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
            YR = R_YR
            IF(R_CLASS == 0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN 
               IF(ALLOCATED(FASB143_ARO_VALUE)) THEN
                  R_MO_NucDecom_Discount_Rate =
     +                             FASB143_ARO_VALUE(:,YR,ASSET_CLASS,1)
                  R_MO_NucDecom_Interest_Accretion =
     +                             FASB143_ARO_VALUE(:,YR,ASSET_CLASS,2)
                  R_MO_ARO_Cash_Payment =
     +                             FASB143_ARO_VALUE(:,YR,ASSET_CLASS,7)
                  R_MO_ARO_Trust_Cash_Payment =
     +                             FASB143_ARO_VALUE(:,YR,ASSET_CLASS,8)
               ENDIF
               IF(ALLOCATED(TRANSFER_VALUE)) THEN
                  R_MO_ARO_NET_ASSETS =
     +                               TRANSFER_VALUE(:,YR,ASSET_CLASS,56)
                  R_MO_ARO_LIABILITY =
     +                               TRANSFER_VALUE(:,YR,ASSET_CLASS,57)
               ENDIF
            ENDIF
         ENDIF
      RETURN
C**********************************************************************
      ENTRY RETURN_MONTHLY_FASB87_ADDENDUMS(R_YR,R_CLASS,
     +                                      R_OCI_ADJ,
     +                                      R_INTANGIBLE_ASSET_ADJ,
     +                                      R_PENSION_LIABILITY_ADJ,
     +                                      R_OCI_DEFERRED_TAX_ADJ_DR)
C**********************************************************************
C
         R_OCI_ADJ = 0.
         R_INTANGIBLE_ASSET_ADJ = 0.
         R_PENSION_LIABILITY_ADJ = 0.
         IF(R_CLASS <= MAX_ASSET_CLASS_NUM .AND.
     +                                ALLOCATED(FASB143_ARO_VALUE)) THEN
            YR = R_YR
            IF(R_CLASS == 0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN 
               R_OCI_ADJ = FASB143_ARO_VALUE(:,YR,ASSET_CLASS,3)
               R_INTANGIBLE_ASSET_ADJ =
     +                             FASB143_ARO_VALUE(:,YR,ASSET_CLASS,4)
               R_PENSION_LIABILITY_ADJ =
     +                             FASB143_ARO_VALUE(:,YR,ASSET_CLASS,5)
               R_OCI_DEFERRED_TAX_ADJ_DR =
     +                             FASB143_ARO_VALUE(:,YR,ASSET_CLASS,6)
            ENDIF
         ENDIF
      RETURN
C**********************************************************************
      ENTRY RETURN_MONTHLY_CAPX_ADDENDUMS(R_YR,R_CLASS,
     +                                    R_PENSION_LIABILITY_ADJ)
C**********************************************************************
C
         R_PENSION_LIABILITY_ADJ = 0.
         IF(R_CLASS <= MAX_ASSET_CLASS_NUM .AND.
     +                                      ALLOCATED(CapX_Values)) THEN
            YR = R_YR
            IF(R_CLASS == 0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN 
               R_PENSION_LIABILITY_ADJ = CapX_Values(:,YR,ASSET_CLASS,1)
            ENDIF
         ENDIF
      RETURN
C**********************************************************************
      ENTRY RETURN_CASH_TRANSFERS(R_CLASS,R_YR,
     +                            CASH_TO_CASH,
     +                            CASH_TO_POST_RETIRE_MEDICAL_BAL,
     +                            CASH_TO_ACCRUED_MEMBER_REVENUES_BAL)
C**********************************************************************
C
         CASH_TO_CASH = 0.
         CASH_TO_POST_RETIRE_MEDICAL_BAL = 0.
         IF(R_CLASS <= MAX_ASSET_CLASS_NUM .AND.
     +                              ALLOCATED(CASH_MONTHLY_VALUES)) THEN
            YR = R_YR
            IF(R_CLASS == 0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN 
               CASH_TO_CASH = CASH_MONTHLY_VALUES(0,YR,ASSET_CLASS,1)    ! Cash to Cash
               CASH_TO_POST_RETIRE_MEDICAL_BAL =
     +                           CASH_MONTHLY_VALUES(0,YR,ASSET_CLASS,2)    ! Cash to Post Retirement Fund
               CASH_TO_ACCRUED_MEMBER_REVENUES_BAL =
     +                           CASH_MONTHLY_VALUES(0,YR,ASSET_CLASS,3)    ! Cash to Accrued Member Revenue
            ENDIF
         ENDIF
      RETURN
C**********************************************************************
      ENTRY RETURN_FUNDS_EARNINGS_ADDENDUMS(R_YR,R_CLASS,
     +                                  R_EARNINGS_DECOMMISSIONING_FUND,
     +                                  R_EARNINGS_POST_RETIREMENT_FUND,
     +                                  R_EARNINGS_ST_INVESTMENTS,
     +                                  R_EARNINGS_LT_INVESTMENTS)
C**********************************************************************
C
C
         R_EARNINGS_DECOMMISSIONING_FUND = 0.
         R_EARNINGS_POST_RETIREMENT_FUND = 0.
         R_EARNINGS_ST_INVESTMENTS = 0.
         R_EARNINGS_LT_INVESTMENTS = 0.
         IF(ALLOCATED(FUNDS_EARNINGS)) THEN
            IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
               YR = R_YR
               IF(R_CLASS == 0) THEN
                  ASSET_CLASS = 0
               ELSE
                  ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
               ENDIF
               IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN 
                  R_EARNINGS_DECOMMISSIONING_FUND =
     +                                FUNDS_EARNINGS(0,YR,ASSET_CLASS,1) ! Earnings Decommissioning Fund
                  R_EARNINGS_POST_RETIREMENT_FUND =
     +                                FUNDS_EARNINGS(0,YR,ASSET_CLASS,2) ! Earnings Post Retirement Fund
                  R_EARNINGS_ST_INVESTMENTS =
     +                                FUNDS_EARNINGS(0,YR,ASSET_CLASS,3) ! Earnings ST Investments
                  R_EARNINGS_LT_INVESTMENTS =
     +                                FUNDS_EARNINGS(0,YR,ASSET_CLASS,4) ! Earnings LT Investments
               ENDIF
            ENDIF
         ENDIF
      RETURN
C**********************************************************************
      ENTRY RETURN_NEW_ISSUE_INTEREST_RATES(R_YR,R_CLASS,
     +                                  R_STD_RATE,
     +                                  R_RETURN_ON_ST_INVEST,
     +                                  R_INTEREST_ON_CUSTOMER_DEPOSITS,
     +                                  R_RETURN_ON_LTI,
     +                                  R_RETURN_RETIREMENT_FUND,
     +                                  R_NUCL_DECOM_FUND_RETURN,
     +                                  R_OCI_NUCL_DECOM_FUND_RETURN,
     +                                  R_OCI_RETURN_RETIREMENT_FUND)
C**********************************************************************
C
C
         IF(ALLOCATED(FUNDS_EARNINGS)) THEN
            IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
               YR = R_YR
               IF(R_CLASS == 0) THEN
                  ASSET_CLASS = 0
               ELSE
                  ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
               ENDIF
               IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN 
                  IF(FUNDS_EARNINGS(0,YR,ASSET_CLASS,5) /= 0.)
     +                   R_STD_RATE = FUNDS_EARNINGS(0,YR,
     +                                               ASSET_CLASS,5)/100. ! STD Rate
                  IF(FUNDS_EARNINGS(0,YR,ASSET_CLASS,6) /= 0.)
     +                   R_NUCL_DECOM_FUND_RETURN = FUNDS_EARNINGS(0,YR,
     +                                               ASSET_CLASS,6)/100.
                  IF(FUNDS_EARNINGS(0,YR,ASSET_CLASS,7) /= 0.)
     +                   R_RETURN_RETIREMENT_FUND = FUNDS_EARNINGS(0,YR,
     +                                               ASSET_CLASS,7)/100.
                  IF(FUNDS_EARNINGS(0,YR,ASSET_CLASS,8) /= 0.)
     +                   R_RETURN_ON_ST_INVEST = FUNDS_EARNINGS(0,YR,
     +                                               ASSET_CLASS,8)/100.
                  IF(FUNDS_EARNINGS(0,YR,ASSET_CLASS,9) /= 0.)
     +                   R_RETURN_ON_LTI = FUNDS_EARNINGS(0,YR,
     +                                               ASSET_CLASS,9)/100.
                  IF(FUNDS_EARNINGS(0,YR,ASSET_CLASS,10) /= 0.)
     +                   R_INTEREST_ON_CUSTOMER_DEPOSITS =
     +                          FUNDS_EARNINGS(0,YR,ASSET_CLASS,10)/100.
                  IF(FUNDS_EARNINGS(0,YR,ASSET_CLASS,11) /= 0.)
     +                    R_OCI_RETURN_RETIREMENT_FUND =
     +                          FUNDS_EARNINGS(0,YR,ASSET_CLASS,11)/100.
                  IF(FUNDS_EARNINGS(0,YR,ASSET_CLASS,12) /= 0.)
     +                    R_OCI_NUCL_DECOM_FUND_RETURN =
     +                          FUNDS_EARNINGS(0,YR,ASSET_CLASS,12)/100.
               ENDIF
            ENDIF
         ENDIF
      RETURN
C**********************************************************************
      ENTRY RETURN_MONTHLY_NEW_ISSUE_RATES(R_YR,R_CLASS,
     +                                  R_STD_RATE,
     +                                  R_RETURN_ON_ST_INVEST,
     +                                  R_INTEREST_ON_CUSTOMER_DEPOSITS,
     +                                  R_RETURN_ON_LTI,
     +                                  R_RETURN_RETIREMENT_FUND,
     +                                  R_NUCL_DECOM_FUND_RETURN,
     +                                  R_MONTHLY_STD_RATE,
     +                                  R_MONTHLY_STI_RATE,
     +                                  R_MONTHLY_CUST_DEPOSITS_RATE,
     +                                  R_MONTHLY_LTI_RATE,
     +                                  R_MONTHLY_RETIREMENT_FUND_RATE,
     +                                  R_MONTHLY_NUC_DECOM_RATE)
C**********************************************************************
C
C
         IF(ALLOCATED(FUNDS_EARNINGS)) THEN
            IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
               YR = R_YR
               IF(R_CLASS == 0) THEN
                  ASSET_CLASS = 0
               ELSE
                  ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
               ENDIF
               IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN 
C                  DO MO = 0,12
                     IF(FUNDS_EARNINGS(0,YR,ASSET_CLASS,5) /= 0.) THEN
                        R_MONTHLY_STD_RATE(:) = 
     +                               FUNDS_EARNINGS(:,YR,ASSET_CLASS,5) ! STD Rate
                     ELSE
                        CALL RETURN_MONTHLY_STD_VECTOR_RATES(RATE_FOUND,
     +                                               R_MONTHLY_STD_RATE)
                        IF(.NOT. RATE_FOUND)
     +                          R_MONTHLY_STD_RATE(:) = 100.*R_STD_RATE 
                     ENDIF
                     IF(FUNDS_EARNINGS(0,YR,ASSET_CLASS,6) /= 0.) THEN
                        R_MONTHLY_NUC_DECOM_RATE(:) = 
     +                               FUNDS_EARNINGS(:,YR,ASSET_CLASS,6) 
                     ELSE
                        CALL RETURN_MONTHLY_DECOMMISIONING_VECTOR_RATES(
     +                                         RATE_FOUND,
     +                                         R_MONTHLY_NUC_DECOM_RATE)
                        IF(.NOT. RATE_FOUND)
     +                        R_MONTHLY_NUC_DECOM_RATE(:) =
     +                                     100.*R_NUCL_DECOM_FUND_RETURN 
                     ENDIF
                     IF(FUNDS_EARNINGS(0,YR,ASSET_CLASS,7) /= 0.) THEN
                        R_MONTHLY_RETIREMENT_FUND_RATE(:) = 
     +                               FUNDS_EARNINGS(:,YR,ASSET_CLASS,7)
                     ELSE
                        CALL RETURN_MONTHLY_RETIREMENT_VECTOR_RATES(
     +                                   RATE_FOUND,
     +                                   R_MONTHLY_RETIREMENT_FUND_RATE)
                        IF(.NOT. RATE_FOUND)
     +                        R_MONTHLY_RETIREMENT_FUND_RATE(:) =
     +                                     100.*R_RETURN_RETIREMENT_FUND 
                     ENDIF
                     IF(FUNDS_EARNINGS(0,YR,ASSET_CLASS,8) /= 0.) THEN
                        R_MONTHLY_STI_RATE(:) = 
     +                               FUNDS_EARNINGS(:,YR,ASSET_CLASS,8)
                     ELSE
                        CALL RETURN_MONTHLY_STI_VECTOR_RATES(
     +                                               RATE_FOUND,
     +                                               R_MONTHLY_STI_RATE)
                        IF(.NOT. RATE_FOUND) R_MONTHLY_STI_RATE(:) =
     +                                        100.*R_RETURN_ON_ST_INVEST 
                     ENDIF
                     IF(FUNDS_EARNINGS(0,YR,ASSET_CLASS,9) /= 0.) THEN
                        R_MONTHLY_LTI_RATE(:) = 
     +                               FUNDS_EARNINGS(:,YR,ASSET_CLASS,9) 
                     ELSE
                        CALL RETURN_MONTHLY_LTI_VECTOR_RATES(
     +                                               RATE_FOUND,
     +                                               R_MONTHLY_LTI_RATE)
                        IF(.NOT. RATE_FOUND) R_MONTHLY_LTI_RATE(:) =
     +                                              100.*R_RETURN_ON_LTI 
                     ENDIF
                     IF(FUNDS_EARNINGS(0,YR,ASSET_CLASS,10) /= 0.) THEN
                        R_MONTHLY_CUST_DEPOSITS_RATE(:) = 
     +                              FUNDS_EARNINGS(:,YR,ASSET_CLASS,10)
                     ELSE
                        CALL RETURN_MONTHLY_CUSTOMER_VECTOR_RATES(
     +                                     RATE_FOUND,
     +                                     R_MONTHLY_CUST_DEPOSITS_RATE)
                        IF(.NOT. RATE_FOUND)
     +                        R_MONTHLY_CUST_DEPOSITS_RATE(:) =
     +                              100.*R_INTEREST_ON_CUSTOMER_DEPOSITS 
                     ENDIF
C                  ENDDO
               ENDIF
               D12 = 100.
               DO MO = 0, 12
                  R_MONTHLY_STD_RATE(MO) = R_MONTHLY_STD_RATE(MO)/D12 
                  R_MONTHLY_NUC_DECOM_RATE(MO) =
     +                                  R_MONTHLY_NUC_DECOM_RATE(MO)/D12 
                  R_MONTHLY_RETIREMENT_FUND_RATE(MO) =
     +                            R_MONTHLY_RETIREMENT_FUND_RATE(MO)/D12 
                  R_MONTHLY_STI_RATE(MO) = R_MONTHLY_STI_RATE(MO)/D12 
                  R_MONTHLY_LTI_RATE(MO) = R_MONTHLY_LTI_RATE(MO)/D12 
                  R_MONTHLY_CUST_DEPOSITS_RATE(MO) =
     +                              R_MONTHLY_CUST_DEPOSITS_RATE(MO)/D12
                  D12 = 1200.
               ENDDO
            ELSE
               D12 = 1.
               DO MO= 0, 12
                  R_MONTHLY_STD_RATE(MO) = R_STD_RATE/D12 
                  R_MONTHLY_NUC_DECOM_RATE(MO) =
     +                                      R_NUCL_DECOM_FUND_RETURN/D12 
                  R_MONTHLY_RETIREMENT_FUND_RATE(MO) =
     +                                      R_RETURN_RETIREMENT_FUND/D12 
                  R_MONTHLY_STI_RATE(MO) = R_RETURN_ON_ST_INVEST/D12 
                  R_MONTHLY_LTI_RATE(MO) = R_RETURN_ON_LTI/D12 
                  R_MONTHLY_CUST_DEPOSITS_RATE(MO) =
     +                               R_INTEREST_ON_CUSTOMER_DEPOSITS/D12
                  D12 = 12.
               ENDDO
            ENDIF
         ELSE
            D12 = 1.
            DO MO= 0, 12
               R_MONTHLY_STD_RATE(MO) = R_STD_RATE/D12 
               R_MONTHLY_NUC_DECOM_RATE(MO)=R_NUCL_DECOM_FUND_RETURN/D12 
               R_MONTHLY_RETIREMENT_FUND_RATE(MO) =
     +                                      R_RETURN_RETIREMENT_FUND/D12 
               R_MONTHLY_STI_RATE(MO) = R_RETURN_ON_ST_INVEST/D12 
               R_MONTHLY_LTI_RATE(MO) = R_RETURN_ON_LTI/D12 
               R_MONTHLY_CUST_DEPOSITS_RATE(MO) =
     +                               R_INTEREST_ON_CUSTOMER_DEPOSITS/D12
               D12 = 12.
            ENDDO
         ENDIF
      RETURN
C***********************************************************************
      ENTRY RETURN_MONTHLY_FUNDS_EARNING_RATES(R_YR,R_CLASS,
     +                                 R_INTEREST_ON_CUSTOMER_DEPOSITS,
     +                                 R_RETURN_RETIREMENT_FUND,
     +                                 R_NUCL_DECOM_FUND_RETURN,
     +                                 R_MONTHLY_CUST_DEPOSITS_RATE,
     +                                 R_MONTHLY_RETIREMENT_FUND_RATE,
     +                                 OCI_MONTHLY_RETIREMENT_FUND_RATE,
     +                                 R_MONTHLY_NUC_DECOM_RATE,
     +                                 OCI_MONTHLY_NUC_DECOM_RATE)
C***********************************************************************
C
         OCI_MONTHLY_RETIREMENT_FUND_RATE = 0.
         OCI_MONTHLY_NUC_DECOM_RATE = 0.
C
         IF(ALLOCATED(FUNDS_EARNINGS)) THEN
            IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
               YR = R_YR
               IF(R_CLASS == 0) THEN
                  ASSET_CLASS = 0
               ELSE
                  ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
               ENDIF
               IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN 
C                  DO MO = 0,12
                     IF(FUNDS_EARNINGS(0,YR,ASSET_CLASS,6) /= 0.) THEN
                        R_MONTHLY_NUC_DECOM_RATE(:) =
     +                                FUNDS_EARNINGS(:,YR,ASSET_CLASS,6)
                     ELSE
                        R_MONTHLY_NUC_DECOM_RATE(:) =
     +                                     100.*R_NUCL_DECOM_FUND_RETURN 
                     ENDIF
                     IF(FUNDS_EARNINGS(0,YR,ASSET_CLASS,7) /= 0.) THEN
                        R_MONTHLY_RETIREMENT_FUND_RATE(:) =
     +                               FUNDS_EARNINGS(:,YR,ASSET_CLASS,7)
                     ELSE
                        R_MONTHLY_RETIREMENT_FUND_RATE(:) =
     +                                     100.*R_RETURN_RETIREMENT_FUND 
                     ENDIF
                     IF(FUNDS_EARNINGS(0,YR,ASSET_CLASS,10) /= 0.) THEN
                        R_MONTHLY_CUST_DEPOSITS_RATE(:) =
     +                              FUNDS_EARNINGS(:,YR,ASSET_CLASS,10) ! STD Rate
                     ELSE
                        R_MONTHLY_CUST_DEPOSITS_RATE(:) =
     +                              100.*R_INTEREST_ON_CUSTOMER_DEPOSITS 
                     ENDIF
                     OCI_MONTHLY_RETIREMENT_FUND_RATE(:) =
     +                               FUNDS_EARNINGS(:,YR,ASSET_CLASS,11)
                     OCI_MONTHLY_NUC_DECOM_RATE(:) =
     +                               FUNDS_EARNINGS(:,YR,ASSET_CLASS,12)
C                  ENDDO
               ENDIF
               D12 = 100.
               DO MO= 0, 12
                  R_MONTHLY_NUC_DECOM_RATE(MO) =
     +                                  R_MONTHLY_NUC_DECOM_RATE(MO)/D12 
                  R_MONTHLY_RETIREMENT_FUND_RATE(MO) =
     +                            R_MONTHLY_RETIREMENT_FUND_RATE(MO)/D12 
                  R_MONTHLY_CUST_DEPOSITS_RATE(MO) =
     +                              R_MONTHLY_CUST_DEPOSITS_RATE(MO)/D12
                  OCI_MONTHLY_RETIREMENT_FUND_RATE(MO) =
     +                          OCI_MONTHLY_RETIREMENT_FUND_RATE(MO)/D12
                  OCI_MONTHLY_NUC_DECOM_RATE(MO) =
     +                                OCI_MONTHLY_NUC_DECOM_RATE(MO)/D12
                  D12 = 1200.
               ENDDO
            ELSE
               D12 = 1.
               DO MO= 0, 12
                  R_MONTHLY_NUC_DECOM_RATE(MO) =
     +                                      R_NUCL_DECOM_FUND_RETURN/D12 
                  R_MONTHLY_RETIREMENT_FUND_RATE(MO) =
     +                                      R_RETURN_RETIREMENT_FUND/D12 
                  R_MONTHLY_CUST_DEPOSITS_RATE(MO) =
     +                               R_INTEREST_ON_CUSTOMER_DEPOSITS/D12
                  D12 = 12.
               ENDDO
            ENDIF
         ELSE
            D12 = 1.
            DO MO= 0, 12
               R_MONTHLY_NUC_DECOM_RATE(MO)=R_NUCL_DECOM_FUND_RETURN/D12 
               R_MONTHLY_RETIREMENT_FUND_RATE(MO) =
     +                                      R_RETURN_RETIREMENT_FUND/D12 
               R_MONTHLY_CUST_DEPOSITS_RATE(MO) =
     +                               R_INTEREST_ON_CUSTOMER_DEPOSITS/D12
               D12 = 12.
            ENDDO
         ENDIF
         RETURN
C***********************************************************************
      ENTRY RETURN_CD_CIAC_ADDENDUMS(R_YR,R_CLASS,
     +                               R_CD_BALANCE_FROM_REVENUES,
     +                               R_CD_ADDENDUM,
     +                               R_CIAC_CASH_CONTRIBUTIONS,
     +                               R_CIAC_AMORTIZATION_RATE,
     +                               R_CIAC_ADDENDUM_TO_AMORTIZATION)
C***********************************************************************
C
C
         IF(ALLOCATED(CD_CIAC)) THEN
            IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
               YR = R_YR
               IF(R_CLASS == 0) THEN
                  ASSET_CLASS = 0
               ELSE
                  ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
               ENDIF
               IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN 
                  R_CD_BALANCE_FROM_REVENUES=R_CD_BALANCE_FROM_REVENUES
     +                                     + CD_CIAC(0,YR,ASSET_CLASS,1)
                  R_CD_ADDENDUM = R_CD_ADDENDUM
     +                                     + CD_CIAC(0,YR,ASSET_CLASS,2)
                  R_CIAC_CASH_CONTRIBUTIONS = R_CIAC_CASH_CONTRIBUTIONS
     +                                     + CD_CIAC(0,YR,ASSET_CLASS,3)
                  R_CIAC_AMORTIZATION_RATE = R_CIAC_AMORTIZATION_RATE
     +                                     + CD_CIAC(0,YR,ASSET_CLASS,4)
                  R_CIAC_ADDENDUM_TO_AMORTIZATION =
     +                                   R_CIAC_ADDENDUM_TO_AMORTIZATION 
     +                                   + CD_CIAC(0,YR,ASSET_CLASS,5)
               ENDIF
            ENDIF
         ENDIF
      RETURN
C**********************************************************************
      ENTRY RETURN_WORKING_CAP_ADDENDUMS(R_YR,R_CLASS,
     +                                   R_ASSETS_NEC_ADDENDUM,
     +                                   R_LIABILITIES_NEC_ADDENDUM,
     +                                   R_ACCT_RECEIVABLE_RATE,
     +                                   R_ACCT_RECEIVABLE_ADDEN,
     +                                   R_ACCT_PAYABLE_RATE,
     +                                   R_ACCT_PAYABLE_ADDEN,
     +                                   R_FUEL_INVENTORY_CHANGE_IN,
     +                                   R_MATERIALS_SUPPLIES_IN,
     +                                   R_GAS_STORAGE_IN,
     +                                   R_FUEL_INVENTORY_OUT,
     +                                   R_MATERIALS_SUPPLIES_OUT,
     +                                   R_GAS_STORAGE_OUT)
C**********************************************************************
C
C
         R_FUEL_INVENTORY_CHANGE_IN = 0.
         R_MATERIALS_SUPPLIES_IN = 0.
         R_GAS_STORAGE_IN = 0.
         R_FUEL_INVENTORY_OUT = 0.
         R_MATERIALS_SUPPLIES_OUT = 0.
         R_GAS_STORAGE_OUT = 0.
C
         IF(ALLOCATED(PAYMENTS)) THEN
            IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
               YR = R_YR
               IF(R_CLASS == 0) THEN
                  ASSET_CLASS = 0
               ELSE
                  ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
               ENDIF
               IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN 
                  R_ASSETS_NEC_ADDENDUM = R_ASSETS_NEC_ADDENDUM +
     +                               WORKING_CAPITAL(0,YR,ASSET_CLASS,1)
                  R_LIABILITIES_NEC_ADDENDUM=R_LIABILITIES_NEC_ADDENDUM+
     +                               WORKING_CAPITAL(0,YR,ASSET_CLASS,2)
                  R_FUEL_INVENTORY_CHANGE_IN =
     +                               WORKING_CAPITAL(0,YR,ASSET_CLASS,3)
                  R_MATERIALS_SUPPLIES_IN =
     +                               WORKING_CAPITAL(0,YR,ASSET_CLASS,4)
                  R_GAS_STORAGE_IN =
     +                               WORKING_CAPITAL(0,YR,ASSET_CLASS,5)
                  R_ACCT_RECEIVABLE_RATE =
     +                               WORKING_CAPITAL(0,YR,ASSET_CLASS,6)
                  R_ACCT_RECEIVABLE_ADDEN =
     +                               WORKING_CAPITAL(0,YR,ASSET_CLASS,7)
                  R_ACCT_PAYABLE_RATE =
     +                               WORKING_CAPITAL(0,YR,ASSET_CLASS,8)
                  R_ACCT_PAYABLE_ADDEN =
     +                               WORKING_CAPITAL(0,YR,ASSET_CLASS,9)
                  R_FUEL_INVENTORY_OUT =
     +                              WORKING_CAPITAL(0,YR,ASSET_CLASS,12)
                  R_MATERIALS_SUPPLIES_OUT =
     +                              WORKING_CAPITAL(0,YR,ASSET_CLASS,13)
                  R_GAS_STORAGE_OUT =
     +                              WORKING_CAPITAL(0,YR,ASSET_CLASS,14)
               ENDIF
            ENDIF
         ENDIF
      RETURN
C***********************************************************************
      ENTRY RETURN_SALE_REMOVAL_ADDENDUMS(R_YR,R_CLASS,
     +                                    R_NET_REMOVAL,
     +                                    R_CASH_RECEIVED,
     +                                    R_GROSS_BOOK_VALUE,
     +                                    R_CUMULATIVE_DEPRECIATION,
     +                                    R_DEFERRED_TAXES_ADJUSTMENT,
     +                                    R_ATL_AMORTIZATION,
     +                                    R_BTL_AMORTIZATION,
     +                                    R_ADDEDUM_TO_BOOK_GAIN_LOSS)
C***********************************************************************
C
C
         R_ADDEDUM_TO_BOOK_GAIN_LOSS = 0.
         IF(ALLOCATED(SALE_REMOVAL)) THEN
            IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
               YR = R_YR
               IF(R_CLASS == 0) THEN
                  ASSET_CLASS = 0
               ELSE
                  ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
               ENDIF
               IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN 
                  R_NET_REMOVAL = R_NET_REMOVAL -
     +                               (SALE_REMOVAL(0,YR,ASSET_CLASS,1) +
     +                                SALE_REMOVAL(0,YR,ASSET_CLASS,8) -! Cost of Removal
     +                                SALE_REMOVAL(0,YR,ASSET_CLASS,9)) ! Salvage Value
                  R_CASH_RECEIVED = R_CASH_RECEIVED +
     +                                  SALE_REMOVAL(0,YR,ASSET_CLASS,2)
                  R_GROSS_BOOK_VALUE = R_GROSS_BOOK_VALUE +
     +                                  SALE_REMOVAL(0,YR,ASSET_CLASS,3)
                  R_CUMULATIVE_DEPRECIATION=R_CUMULATIVE_DEPRECIATION +
     +                                  SALE_REMOVAL(0,YR,ASSET_CLASS,4)
                  R_DEFERRED_TAXES_ADJUSTMENT =
     +                                  R_DEFERRED_TAXES_ADJUSTMENT +
     +                                  SALE_REMOVAL(0,YR,ASSET_CLASS,5)
                  R_ATL_AMORTIZATION = R_ATL_AMORTIZATION +
     +                                  SALE_REMOVAL(0,YR,ASSET_CLASS,6)
                  R_BTL_AMORTIZATION = R_BTL_AMORTIZATION +
     +                                  SALE_REMOVAL(0,YR,ASSET_CLASS,7)
                  R_ADDEDUM_TO_BOOK_GAIN_LOSS =
     +                                 SALE_REMOVAL(0,YR,ASSET_CLASS,10)
               ENDIF
            ENDIF
         ENDIF
      RETURN
C***********************************************************************
      ENTRY RETURN_SHAREHOLDER_ADDENDUMS(R_YR,R_CLASS,
     +                                   R_EQUITY_RISK_ADJUSTMENT,
     +                                   R_ECONOMIC_ASSETS_ADJUSTMENT,
     +                                   R_OPERATING_PROFITS_ADJUSTMENT,
     +                                   R_CAP_RECOVERY_EXCLUDE_OPAT)
C***********************************************************************
C
C
         IF(ALLOCATED(SHAREHOLDER_VALUE)) THEN
            IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
               YR = R_YR
               IF(R_CLASS == 0) THEN
                  ASSET_CLASS = 0
               ELSE
                  ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
               ENDIF
               IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN 
                  R_EQUITY_RISK_ADJUSTMENT = R_EQUITY_RISK_ADJUSTMENT +
     +                             SHAREHOLDER_VALUE(0,YR,ASSET_CLASS,1)
                  R_ECONOMIC_ASSETS_ADJUSTMENT =
     +                             R_ECONOMIC_ASSETS_ADJUSTMENT +
     +                             SHAREHOLDER_VALUE(0,YR,ASSET_CLASS,2)
                  R_OPERATING_PROFITS_ADJUSTMENT =
     +                             R_OPERATING_PROFITS_ADJUSTMENT+
     +                             SHAREHOLDER_VALUE(0,YR,ASSET_CLASS,3)
                  R_CAP_RECOVERY_EXCLUDE_OPAT =
     +                             R_CAP_RECOVERY_EXCLUDE_OPAT +
     +                             SHAREHOLDER_VALUE(0,YR,ASSET_CLASS,4)
               ENDIF
            ENDIF
         ENDIF
      RETURN
C***********************************************************************
      ENTRY RETURN_TRANSFER_ADDENDUMS(R_YR,R_CLASS,
     +                                R_GROSS_PLANT_VALUE,
     +                                R_CUMULATED_PLANT_DEPRECIATION,
     +                                R_DEFERRED_TAXES_BAL_ADJ,
     +                                R_DEFERRED_DEBIT_ADJUSTMENT,
     +                                R_NET_DEFERRED_DEBIT_ADJUSTMENT,
     +                                R_ATL_DEBIT_AMORTIZATION,
     +                                R_BTL_DEBT_AMORTIZATION,
     +                                R_ANNUAL_DEFERRED_TAXES,
     +                                R_DEFERRED_ITCS_BAL_ADJ,
     +                                R_NET_NUCLEAR_FUEL_ADJ,
     +                                R_RETAINED_EARNINGS_ADJ,
     +                                R_EXTRA_ORDINARY_EXPENSE,
     +                                R_LT_LIAB_BAL_ADJ,
     +                                R_CWIP_BALANCE_ADJ,
     +                                R_DEFERRED_TAXES_DR,
     +                                R_PAID_IN_CAPITAL,
     +                                R_SUBSIDIARY_INVESTMENT_ADJ,
     +                                R_GOODWILL_ADJUSTMENT,
     +                                R_REG_ASSESTS_ADJUSTMENT,
     +                                R_FASB109_ADJUSTMENT,
     +                                R_FASB133_ADJUSTMENT,
     +                                R_UNAMORT_INTEREST_ADJUSTMENT,
     +                                R_NUCLEAR_DECOM_FUND_BAL,
     +                                R_CAP_LEASES_BAL,
     +                                R_ASSETS_NEC_BAL,
     +                                R_PREFERRED_STOCK_BAL,
     +                                R_LTD_BAL,
     +                                R_STD_BAL,
     +                                R_NUC_DECOM_LIAB_BAL,
     +                                R_LIABS_NEC_BAL,
     +                                R_OTHER_INVESTMENT_BAL,
     +                                R_NOTES_RECEIVABLE_BAL,
     +                                R_POST_RETIRE_MEDICAL_BAL,
     +                                R_DEFERRED_REVENUES_BAL,
     +                                R_DEFERRED_FUEL_BAL,
     +                                R_DEFERRED_PURCH_GAS_BAL,
     +                                R_LT_INVEST_BAL,
     +                                R_ST_INVEST_BAL,
     +                                R_FUEL_INVENTORY_BAL,
     +                                R_GAS_IN_STORAGE_BAL,
     +                                R_MATRIAL_SUPPLY_BAL,
     +                                R_ACCOUNTS_RECV_BAL,
     +                                R_UNBILLED_REV_BAL,
     +                                R_TAXES_RECEIVABLE_BAL,
     +                                R_CURRENT_LT_DEBT_BAL,
     +                                R_NOTES_PAYABLE_BAL,
     +                                R_CUSTOMER_DEPOSIT_BAL,
     +                                R_CICA_BAL,
     +                                R_POST_RETIRE_PAYABLE_BAL,
     +                                R_ACCRUED_PENSION_BAL,
     +                                R_DEFERRED_GAINS_BAL,
     +                                R_STROM_RESERVE_BAL,
     +                                R_ACCOUNTS_PAYABLE_BAL,
     +                                R_ARO_NET_ASSETS,
     +                                R_ARO_LIABILITY,
     +                                R_DEFERRED_PURCHASE_POWER_ADJ,
     +                                R_COI_EARNINGS)
C***********************************************************************
C
C
         R_DEFERRED_DEBIT_ADJUSTMENT = 0.
         R_NET_DEFERRED_DEBIT_ADJUSTMENT = 0.
         R_RETAINED_EARNINGS_ADJ = 0.
         R_LT_LIAB_BAL_ADJ = 0.
         R_CWIP_BALANCE_ADJ = 0.
         R_DEFERRED_TAXES_DR = 0.
         R_GOODWILL_ADJUSTMENT = 0.
         R_REG_ASSESTS_ADJUSTMENT = 0.
         R_FASB109_ADJUSTMENT = 0.
         R_FASB133_ADJUSTMENT = 0.
         R_UNAMORT_INTEREST_ADJUSTMENT = 0.
         R_NUCLEAR_DECOM_FUND_BAL = 0.
         R_CAP_LEASES_BAL = 0.
         R_ASSETS_NEC_BAL = 0.
         R_PREFERRED_STOCK_BAL = 0.
         R_LTD_BAL = 0.
         R_STD_BAL = 0.
         R_NUC_DECOM_LIAB_BAL = 0.
         R_LIABS_NEC_BAL = 0.
         R_OTHER_INVESTMENT_BAL = 0.
         R_NOTES_RECEIVABLE_BAL = 0.
         R_POST_RETIRE_MEDICAL_BAL = 0.
         R_DEFERRED_REVENUES_BAL = 0.
         R_DEFERRED_FUEL_BAL = 0.
         R_DEFERRED_PURCH_GAS_BAL = 0.
         R_FUEL_INVENTORY_BAL = 0.
         R_GAS_IN_STORAGE_BAL = 0.
         R_MATRIAL_SUPPLY_BAL = 0.
         R_ACCOUNTS_RECV_BAL = 0.
         R_UNBILLED_REV_BAL = 0.
         R_TAXES_RECEIVABLE_BAL = 0.
         R_CURRENT_LT_DEBT_BAL = 0.
         R_NOTES_PAYABLE_BAL = 0.
         R_CUSTOMER_DEPOSIT_BAL = 0.
         R_CICA_BAL = 0.
         R_POST_RETIRE_PAYABLE_BAL = 0.
         R_ACCRUED_PENSION_BAL = 0.
         R_DEFERRED_GAINS_BAL = 0.
         R_STROM_RESERVE_BAL = 0.
         R_ACCOUNTS_PAYABLE_BAL = 0.
         R_ARO_NET_ASSETS = 0.
         R_ARO_LIABILITY = 0.
         R_DEFERRED_PURCHASE_POWER_ADJ = 0.
         R_COI_EARNINGS = 0.
C
         IF(ALLOCATED(TRANSFER_VALUE)) THEN
            IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
               YR = R_YR
               IF(R_CLASS == 0) THEN
                  ASSET_CLASS = 0
               ELSE
                  ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
               ENDIF
               IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN 
                  R_GROSS_PLANT_VALUE = R_GROSS_PLANT_VALUE -
     +                                TRANSFER_VALUE(0,YR,ASSET_CLASS,1)
                  R_CUMULATED_PLANT_DEPRECIATION =
     +                                R_CUMULATED_PLANT_DEPRECIATION -
     +                                TRANSFER_VALUE(0,YR,ASSET_CLASS,2)
                  R_DEFERRED_TAXES_BAL_ADJ =
     +                                TRANSFER_VALUE(0,YR,ASSET_CLASS,3)
                  R_DEFERRED_DEBIT_ADJUSTMENT =
     +                                TRANSFER_VALUE(0,YR,ASSET_CLASS,4)
                  R_ATL_DEBIT_AMORTIZATION =
     +                                R_ATL_DEBIT_AMORTIZATION -
     +                                TRANSFER_VALUE(0,YR,ASSET_CLASS,5)
                  R_BTL_DEBT_AMORTIZATION =
     +                                R_BTL_DEBT_AMORTIZATION -
     +                                TRANSFER_VALUE(0,YR,ASSET_CLASS,6)
                  R_ANNUAL_DEFERRED_TAXES = R_ANNUAL_DEFERRED_TAXES +
     +                                TRANSFER_VALUE(0,YR,ASSET_CLASS,7)
                  R_NET_DEFERRED_DEBIT_ADJUSTMENT =
     +                              TRANSFER_VALUE(0,YR,ASSET_CLASS,4)
     +                              - TRANSFER_VALUE(0,YR,ASSET_CLASS,5)
     +                              - TRANSFER_VALUE(0,YR,ASSET_CLASS,6)
                  R_DEFERRED_ITCS_BAL_ADJ =
     +                                TRANSFER_VALUE(0,YR,ASSET_CLASS,8)
                  R_NET_NUCLEAR_FUEL_ADJ = R_NET_NUCLEAR_FUEL_ADJ +
     +                                TRANSFER_VALUE(0,YR,ASSET_CLASS,9)
                  R_RETAINED_EARNINGS_ADJ = R_RETAINED_EARNINGS_ADJ +
     +                               TRANSFER_VALUE(0,YR,ASSET_CLASS,10)
                  R_EXTRA_ORDINARY_EXPENSE = R_EXTRA_ORDINARY_EXPENSE +
     +                               TRANSFER_VALUE(0,YR,ASSET_CLASS,11)
                  R_LT_LIAB_BAL_ADJ = R_LT_LIAB_BAL_ADJ +
     +                               TRANSFER_VALUE(0,YR,ASSET_CLASS,12)
                  R_CWIP_BALANCE_ADJ = R_CWIP_BALANCE_ADJ +
     +                               TRANSFER_VALUE(0,YR,ASSET_CLASS,13)
                  R_DEFERRED_TAXES_DR = R_DEFERRED_TAXES_DR +
     +                               TRANSFER_VALUE(0,YR,ASSET_CLASS,14)
                  R_PAID_IN_CAPITAL =
     +                               TRANSFER_VALUE(0,YR,ASSET_CLASS,15)
                  R_SUBSIDIARY_INVESTMENT_ADJ =
     +                               TRANSFER_VALUE(0,YR,ASSET_CLASS,16)
                  R_GOODWILL_ADJUSTMENT =
     +                               TRANSFER_VALUE(0,YR,ASSET_CLASS,17) ! Goodwill
                  R_REG_ASSESTS_ADJUSTMENT =
     +                               TRANSFER_VALUE(0,YR,ASSET_CLASS,18) ! Regulatory Assets
                  R_FASB109_ADJUSTMENT =
     +                               TRANSFER_VALUE(0,YR,ASSET_CLASS,19) ! FASB 109
                  R_FASB133_ADJUSTMENT =
     +                               TRANSFER_VALUE(0,YR,ASSET_CLASS,21) ! FASB 133
                  R_UNAMORT_INTEREST_ADJUSTMENT =
     +                               TRANSFER_VALUE(0,YR,ASSET_CLASS,22) ! Unamortizated Interest
                  R_NUCLEAR_DECOM_FUND_BAL =
     +                               TRANSFER_VALUE(0,YR,ASSET_CLASS,25) ! Nuc Decom Trust Fund
                  R_POST_RETIRE_MEDICAL_BAL =
     +                               TRANSFER_VALUE(0,YR,ASSET_CLASS,26) ! Post Retirement Medical Bal
                  R_LT_INVEST_BAL = TRANSFER_VALUE(0,YR,ASSET_CLASS,30) ! Long term investment bal
                  R_ST_INVEST_BAL = TRANSFER_VALUE(0,YR,ASSET_CLASS,31) ! Short Term Investment Bal
                  R_ASSETS_NEC_BAL =
     +                               TRANSFER_VALUE(0,YR,ASSET_CLASS,39) ! assets NEC
                  R_PREFERRED_STOCK_BAL =
     +                               TRANSFER_VALUE(0,YR,ASSET_CLASS,40) ! Preferred Stock Bal
                  R_LTD_BAL = TRANSFER_VALUE(0,YR,ASSET_CLASS,41)        ! Long Term Debt Bal
                  R_NUC_DECOM_LIAB_BAL =
     +                               TRANSFER_VALUE(0,YR,ASSET_CLASS,44) ! Nuc Decom Liab
                  R_ACCRUED_PENSION_BAL =
     +                               TRANSFER_VALUE(0,YR,ASSET_CLASS,49)
                  R_LIABS_NEC_BAL = TRANSFER_VALUE(0,YR,ASSET_CLASS,54)  ! Liabs NEC
                  R_CAP_LEASES_BAL = TRANSFER_VALUE(0,YR,ASSET_CLASS,55) ! CAPITAL LEASE ADJ
                  R_ARO_NET_ASSETS = TRANSFER_VALUE(0,YR,ASSET_CLASS,56)
                  R_ARO_LIABILITY = TRANSFER_VALUE(0,YR,ASSET_CLASS,57)
                  R_COI_EARNINGS = TRANSFER_VALUE(0,YR,ASSET_CLASS,58)
                  R_DEFERRED_PURCHASE_POWER_ADJ =
     +                               TRANSFER_VALUE(0,YR,ASSET_CLASS,59)
                  IF(MONTHLY_MIDAS_ACTIVE) THEN ! ADDED TO KEEP THE ANNUAL AS A ONE YEAR ADJUSTMENT
                     DO YR = 1, R_YR-1
                        R_CWIP_BALANCE_ADJ = R_CWIP_BALANCE_ADJ
     +                             + TRANSFER_VALUE(0,YR,ASSET_CLASS,13) ! CWIP ADJUSTMENTS

                     ENDDO
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      RETURN

C***********************************************************************
      ENTRY RETURN_MONTHLY_IPL_ELECT_PLAN(R_YR,R_CLASS,
     +                                     IPL_ELECTRIC_PLAN_FUEL_COSTS,
     +                                     IPL_ELECTRIC_PLAN_PUR_COSTS)
C***********************************************************************
C
         IF(ALLOCATED(OTHER_VALUE)) THEN
            IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
               YR = R_YR
               IF(R_CLASS == 0) THEN
                  ASSET_CLASS = 0
               ELSE
                  ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
               ENDIF
               IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN 
                  IPL_ELECTRIC_PLAN_FUEL_COSTS(:) =
     +                                 IPL_ELECTRIC_PLAN_FUEL_COSTS(:)
     +                                 + OTHER_VALUE(:,YR,ASSET_CLASS,4) ! ElectPlan Fuel)
                  IPL_ELECTRIC_PLAN_PUR_COSTS(:) =
     +                                 IPL_ELECTRIC_PLAN_PUR_COSTS(:)
     +                                 + OTHER_VALUE(:,YR,ASSET_CLASS,5) !ElectPlan Pur)
               ENDIF
            ENDIF
         ENDIF
      RETURN
C***********************************************************************
      ENTRY RETURN_SRP_TOTAL_SALES(R_YR,
     +                             R_SRP_TARGET_SALES)
C***********************************************************************
C
         R_SRP_TARGET_SALES = 0.
         IF(ALLOCATED(OTHER_VALUE)) THEN
            YR = R_YR
            DO ASSET_CLASS = 1, NUM_OF_ASSET_CLASSES
               R_SRP_TARGET_SALES = R_SRP_TARGET_SALES +
     +                    OTHER_VALUE(0,YR,ASSET_CLASS,SRP Target Sales)
            ENDDO
         ENDIF
      RETURN
C***********************************************************************
      ENTRY RETURN_GRE_DSC_ADJUSTMENTS(R_YR,
     +                              R_GRE_INTEREST_PAYMENTS_MANUAL_ADJS,
     +                              R_GRE_PRINCIPAL_MANUAL_ADJS)
C***********************************************************************
C
         R_GRE_PRINCIPAL_MANUAL_ADJS = 0.
         R_GRE_INTEREST_PAYMENTS_MANUAL_ADJS = 0.
         IF(ALLOCATED(OTHER_VALUE)) THEN
            YR = R_YR
            DO ASSET_CLASS = 1, NUM_OF_ASSET_CLASSES
               R_GRE_PRINCIPAL_MANUAL_ADJS =
     +           OTHER_VALUE(0,YR,ASSET_CLASS,GRE DSC Principal Pay Adj)
               R_GRE_INTEREST_PAYMENTS_MANUAL_ADJS =
     +            OTHER_VALUE(0,YR,ASSET_CLASS,GRE DSC Interest Pay Adj)
            ENDDO
         ENDIF
      RETURN
C**********************************************************************
      ENTRY RETURN_MONTHLY_NUC_DECOM_CASH_ADDENDUMS(R_CLASS,R_YR,
     +                               R_MONTHLY_NUC_DECOM_CASH_VALUES,
     +                               R_MONTHLY_NUC_DECOM_EARNINGS_ADDEM)
C**********************************************************************
C
         IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
            IF(R_CLASS == 0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN
               IF(ALLOCATED(PAYMENTS)) 
     +                R_MONTHLY_NUC_DECOM_CASH_VALUES =
     +                                    PAYMENTS(:,R_YR,ASSET_CLASS,1)
               IF(ALLOCATED(FUNDS_EARNINGS)) 
     +                R_MONTHLY_NUC_DECOM_EARNINGS_ADDEM =
     +                              FUNDS_EARNINGS(:,R_YR,ASSET_CLASS,1)
            ENDIF
         ENDIF
      RETURN
C**********************************************************************
      ENTRY RETURN_MONTHLY_RETIREMENT_CASH_ADDENDUMS(R_CLASS,R_YR,
     +                          R_MONTHLY_POST_RETIREMENT_CASH_PAYMENTS,
     +                          R_MONTHLY_RETIREE_MEDICAL_CASH_PAYMENTS,
     +                          MONTHLY_CASH_TO_POST_RETIREMENT,
     +                          R_MONTHLY_RETIREMENT_FUND_EARNINGS_ADJ)
C**********************************************************************
C
         IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
            IF(R_CLASS == 0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN
               IF(ALLOCATED(PAYMENTS)) THEN 
                  R_MONTHLY_POST_RETIREMENT_CASH_PAYMENTS =
     +                                    PAYMENTS(:,R_YR,ASSET_CLASS,3)
                  R_MONTHLY_RETIREE_MEDICAL_CASH_PAYMENTS =
     +                                    PAYMENTS(:,R_YR,ASSET_CLASS,4)
               ENDIF
               IF(ALLOCATED(FUNDS_EARNINGS)) 
     +                R_MONTHLY_RETIREMENT_FUND_EARNINGS_ADJ =
     +                              FUNDS_EARNINGS(:,R_YR,ASSET_CLASS,2)
               IF(ALLOCATED(CASH_MONTHLY_VALUES)) THEN
                   MONTHLY_CASH_TO_POST_RETIREMENT =
     +                           CASH_MONTHLY_VALUES(:,YR,ASSET_CLASS,2) ! CASH TO POST RTIREMENT
               ENDIF
            ENDIF
         ENDIF
      RETURN
C**********************************************************************
      ENTRY RETURN_MONTHLY_CASH_ADDENDUMS(R_CLASS,R_YR,
     +                                    R_CASH_VALUES,
     +                                    R_INCOME_VALUES)
C**********************************************************************
C
C
         IF(ALLOCATED(INVESTMENTS)) THEN
            IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
               YR = R_YR
               IF(R_CLASS == 0) THEN
                  ASSET_CLASS = 0
               ELSE
                  ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
               ENDIF
               IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN
                  DO MO = 0, 12
                     R_CASH_VALUES(MO,Cash Subsidary Dividends) =
     +                                  INVESTMENTS(MO,YR,ASSET_CLASS,1)
                     R_CASH_VALUES(MO,Cash Equity to Subsidiaries) =
     +                                  INVESTMENTS(MO,YR,ASSET_CLASS,3)
                     R_CASH_VALUES(MO,Cash Net Investments) =
     +                                  INVESTMENTS(MO,YR,ASSET_CLASS,4)
                     R_CASH_VALUES(MO,Cash Capital Leases) =
     +                                  INVESTMENTS(MO,YR,ASSET_CLASS,5)
                     R_CASH_VALUES(MO,Cash Lease Receipts) =
     +                                  INVESTMENTS(MO,YR,ASSET_CLASS,5)
                     R_CASH_VALUES(MO,Cash From Sale of Assets) =
     +                               INVESTMENTS(MO,YR,ASSET_CLASS,6)
     +                               + SALE_REMOVAL(MO,YR,ASSET_CLASS,2)
                     R_CASH_VALUES(MO,Cash Change LTInvestments) =
     +                                  INVESTMENTS(MO,YR,ASSET_CLASS,7)
                     R_CASH_VALUES(MO,Cash Net Salvage) =
     +                              -(SALE_REMOVAL(MO,YR,ASSET_CLASS,1) 
     +                              + SALE_REMOVAL(MO,YR,ASSET_CLASS,8)  ! Cost of Removal
     +                              - SALE_REMOVAL(MO,YR,ASSET_CLASS,9)) ! Salvage Value
                     R_CASH_VALUES(MO,
     +                              Cash Net Investment Salvage Other) =
     +                            R_CASH_VALUES(MO,Cash Net Investments)
     +                            + R_CASH_VALUES(MO,Cash Net Salvage)
     +                            + R_CASH_VALUES(MO,Cash Net Other)
                     R_CASH_VALUES(MO,Cash CIAC) =
     +                                      CD_CIAC(MO,YR,ASSET_CLASS,3)

                     R_CASH_VALUES(MO,Cash Customer Deposits) =
     +                                      CD_CIAC(MO,YR,ASSET_CLASS,2)
                     R_CASH_VALUES(MO,Cash Fuel Inventory Change) =
     +                              WORKING_CAPITAL(MO,YR,ASSET_CLASS,3)
                     R_CASH_VALUES(MO,
     +                          Cash Materials Inventory Addition) =
     +                              WORKING_CAPITAL(MO,YR,ASSET_CLASS,4)
                     R_CASH_VALUES(MO,
     +                        Cash Gas Storage Inventory Addition) =
     +                              WORKING_CAPITAL(MO,YR,ASSET_CLASS,5)
                     R_CASH_VALUES(MO,Cash Nuc Decommissioning Fund) =
     +                                     PAYMENTS(MO,YR,ASSET_CLASS,1)
                     R_CASH_VALUES(MO,Cash Post Retirement Payments) =
     +                            PAYMENTS(MO,YR,ASSET_CLASS,3)
                     R_CASH_VALUES(MO,
     +                            wvpa_cash_accd_mbr_revenue) =
     +                          CASH_MONTHLY_VALUES(MO,YR,ASSET_CLASS,3) ! CASH TO ACCRUED MEMBER BAL
                     R_CASH_VALUES(MO,
     +                               Cash to Post Retirement Payments) =
     +                          CASH_MONTHLY_VALUES(MO,YR,ASSET_CLASS,2) ! CASH TO RETIREMENT PAYMENTS
                     R_CASH_VAlues(MO,Cash to Cash) =
     +                          CASH_MONTHLY_VALUES(MO,YR,ASSET_CLASS,1) ! CASH TO CASH
                     R_CASH_VALUES(MO,Cash Retiree Medical Payments) =
     +                                     PAYMENTS(MO,YR,ASSET_CLASS,4)
                     R_CASH_VALUES(MO,
     +                              Cash Unfunded Retirement Payments) =
     +                         R_CASH_VALUES(MO,
     +                                Cash Unfunded Retirement Payments)
     +                         + PAYMENTS(MO,YR,ASSET_CLASS,6)
                     R_CASH_VALUES(MO,Cash Storm Payments) =
     +                             R_CASH_VALUES(MO,Cash Storm Payments)
     +                             + PAYMENTS(MO,YR,ASSET_CLASS,7)
                     R_CASH_VALUES(MO,Cash Vacation Payments) =
     +                          R_CASH_VALUES(MO,Cash Vacation Payments)
     +                          + PAYMENTS(MO,YR,ASSET_CLASS,8)
c                    R_CASH_VALUES(MO,) =
C
C INCOME ITEMS
C
                     R_INCOME_VALUES(MO,Monthly Subsidiary Income) =
     +                                  INVESTMENTS(MO,YR,ASSET_CLASS,2)
                     R_INCOME_VALUES(MO,Monthly Retirement Expense) =
     +                                     PAYMENTS(MO,YR,ASSET_CLASS,3)
                     R_INCOME_VALUES(MO,Monthly Amortization) =
     +                          R_INCOME_VALUES(MO,Monthly Amortization)
     +                          + TRANSFER_VALUE(MO,YR,ASSET_CLASS,5)
                     R_INCOME_VALUES(MO,BTL Monthly Amortization) =
     +                      R_INCOME_VALUES(MO,BTL Monthly Amortization)
     +                      + TRANSFER_VALUE(MO,YR,ASSET_CLASS,6)
                     R_INCOME_VALUES(MO,Monthly Fossil Fuel) =
     +                           R_INCOME_VALUES(MO,Monthly Fossil Fuel)
     +                           + WORKING_CAPITAL(MO,YR,ASSET_CLASS,12)   ! FUEL INVENTORY AMORTIZATION
                     R_INCOME_VALUES(MO,Monthly Other OandM) =
     +                           R_INCOME_VALUES(MO,Monthly Other OandM)
     +                           + WORKING_CAPITAL(MO,YR,ASSET_CLASS,13)   ! M&S AMORTIZATION
                     R_INCOME_VALUES(MO,Monthly Purchased Gas) =
     +                         R_INCOME_VALUES(MO,Monthly Purchased Gas)
     +                         + WORKING_CAPITAL(MO,YR,ASSET_CLASS,14)   ! GAS STORAGE AMORTIZATION
                     R_INCOME_VALUES(MO,Monthly Fuel Inventory Amort) =
     +                             WORKING_CAPITAL(MO,YR,ASSET_CLASS,12)   ! FUEL INVENTORY AMORTIZATION
                     R_INCOME_VALUES(MO,
     +                         Monthly Material n Supplies Amort) =
     +                             WORKING_CAPITAL(MO,YR,ASSET_CLASS,13)   ! M&S AMORTIZATION
                     R_INCOME_VALUES(MO,Monthly Gas Storage Amort) =
     +                             WORKING_CAPITAL(MO,YR,ASSET_CLASS,14)   ! GAS STORAGE AMORTIZATION
                  ENDDO
               ENDIF
            ENDIF
         ENDIF
      RETURN
C**********************************************************************
      ENTRY MONTHLY_WORKING_CAPITAL_ADJ(R_YR,R_CLASS,
     +                                  R_ASSET_NEC_ADDENDUM,
     +                                  R_LIABS_NEC_ADDENDUM)
C**********************************************************************
C
         DO MO = 0, 12
            R_ASSET_NEC_ADDENDUM(MO) = 0.
            R_LIABS_NEC_ADDENDUM(MO) = 0.
         ENDDO   
         IF(ALLOCATED(WORKING_CAPITAL)) THEN
            IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
               IF(R_CLASS == 0) THEN
                  ASSET_CLASS = 0
               ELSE
                  ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
               ENDIF
               IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN
                  R_ASSET_NEC_ADDENDUM(12) =
     +                             WORKING_CAPITAL(0,R_YR,ASSET_CLASS,1)
                  R_LIABS_NEC_ADDENDUM(12) =
     +                             WORKING_CAPITAL(0,R_YR,ASSET_CLASS,2)
                  DO MO = 1, 11
                     R_ASSET_NEC_ADDENDUM(MO) =
     +                            WORKING_CAPITAL(MO,R_YR,ASSET_CLASS,1)
                     R_LIABS_NEC_ADDENDUM(MO) =
     +                            WORKING_CAPITAL(MO,R_YR,ASSET_CLASS,2)
                  ENDDO
               ENDIF
            ENDIF
         ENDIF
      RETURN
C**********************************************************************
      ENTRY RETURN_MONTHLY_BAL_ADDENDUMS(R_YR,R_CLASS,
     +                                   R_BALANCE_SHEET_VALUES,
     +                                   R_TRANSFER_VARIABLES,
     +                                   R_DEFERRED_GAIN_ADJ)
C**********************************************************************
C
         R_DEFERRED_GAIN_ADJ(1:12) = 0.
         IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
            IF(R_CLASS == 0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN
               GPV_ADJUSTMENT = 0
               DEF_TAX_ADJUSTMENT = 0.
               DEF_DEBIT_ADJUSTMENT = 0.
               CWIP_BALANCE_ADJ = 0.
               LT_LIAB_BAL_ADJ = 0.
               NET_NUC_FUEL_ADJ = 0.
               CAPITAL_LEASE_ADJ = 0.
               IF(ALLOCATED(TRANSFER_VALUE)) THEN
                  DO YR = 1, R_YR-1
                     GPV_ADJUSTMENT = GPV_ADJUSTMENT
     +                             + TRANSFER_VALUE(0,YR,ASSET_CLASS,1)
                     DEF_TAX_ADJUSTMENT = DEF_TAX_ADJUSTMENT
     +                              + TRANSFER_VALUE(0,YR,ASSET_CLASS,3)
                     DEF_DEBIT_ADJUSTMENT = DEF_DEBIT_ADJUSTMENT
     +                              + TRANSFER_VALUE(0,YR,ASSET_CLASS,4)
     +                              - TRANSFER_VALUE(0,YR,ASSET_CLASS,5)
     +                              - TRANSFER_VALUE(0,YR,ASSET_CLASS,6)
C                    CWIP_BALANCE_ADJ = CWIP_BALANCE_ADJ
C    +                             + TRANSFER_VALUE(0,YR,ASSET_CLASS,13) ! CWIP ADJUSTMENTS
                  ENDDO
               ENDIF
               IF(ALLOCATED(SALE_REMOVAL)) THEN
                  DO YR = 1, R_YR-1
                     GPV_ADJUSTMENT = GPV_ADJUSTMENT
     +                                - SALE_REMOVAL(0,YR,ASSET_CLASS,3)
                     DEF_TAX_ADJUSTMENT = DEF_TAX_ADJUSTMENT
     +                                - SALE_REMOVAL(0,YR,ASSET_CLASS,5)
                  ENDDO
                  YR = R_YR
                  DO MO = 1, 12
                     R_DEFERRED_GAIN_ADJ(MO) = 
     +                              SALE_REMOVAL(MO,YR,ASSET_CLASS,2)    ! Cash recieved
     +                              - SALE_REMOVAL(MO,YR,ASSET_CLASS,3)  ! GPV of sold asset
     +                              + SALE_REMOVAL(MO,YR,ASSET_CLASS,4)  ! Cum Dep of sold asset
     +                              - SALE_REMOVAL(MO,YR,ASSET_CLASS,6)  ! ATL amortization of gain
     +                              - SALE_REMOVAL(MO,YR,ASSET_CLASS,7)  ! BTL amortization of gain
     +                              + SALE_REMOVAL(MO,YR,ASSET_CLASS,10) ! addendum to gain
                  ENDDO
               ENDIF
               R_BALANCE_SHEET_VALUES(0,Monthly Gross Plant in Service)=
     +                   R_BALANCE_SHEET_VALUES(0,
     +                                   Monthly Gross Plant in Service)
     +                   + GPV_ADJUSTMENT
               R_BALANCE_SHEET_VALUES(0,Monthly Deferred Income Taxes)=
     +                   R_BALANCE_SHEET_VALUES(0,
     +                                    Monthly Deferred Income Taxes)
     +                   + DEF_TAX_ADJUSTMENT
               R_BALANCE_SHEET_VALUES(0,Monthly Net Deferred Debits) =
     +                   R_BALANCE_SHEET_VALUES(0,
     +                                      Monthly Net Deferred Debits)
     +                   + DEF_DEBIT_ADJUSTMENT
               YR = R_YR
               DO MO = 1, 12
                  IF(ALLOCATED(TRANSFER_VALUE)) THEN
                     GPV_ADJUSTMENT = GPV_ADJUSTMENT
     +                             + TRANSFER_VALUE(MO,YR,ASSET_CLASS,1)
                     DEF_TAX_ADJUSTMENT = DEF_TAX_ADJUSTMENT
     +                             + TRANSFER_VALUE(MO,YR,ASSET_CLASS,3)
                     DEF_DEBIT_ADJUSTMENT = DEF_DEBIT_ADJUSTMENT
     +                             + TRANSFER_VALUE(MO,YR,ASSET_CLASS,4)
     +                             - TRANSFER_VALUE(MO,YR,ASSET_CLASS,5)
     +                             - TRANSFER_VALUE(MO,YR,ASSET_CLASS,6)
                     R_BALANCE_SHEET_VALUES(MO,
     +                                    Monthly Net Deferred Debits) =
     +                       R_BALANCE_SHEET_VALUES(MO,
     +                                      Monthly Net Deferred Debits)
     +                       + DEF_DEBIT_ADJUSTMENT
                     CWIP_BALANCE_ADJ = CWIP_BALANCE_ADJ
     +                            + TRANSFER_VALUE(MO,YR,ASSET_CLASS,13) ! CWIP ADJUSTMENTS
                     LT_LIAB_BAL_ADJ = LT_LIAB_BAL_ADJ
     +                            + TRANSFER_VALUE(MO,YR,ASSET_CLASS,12)
                     NET_NUC_FUEL_ADJ = NET_NUC_FUEL_ADJ
     +                            + TRANSFER_VALUE(MO,YR,ASSET_CLASS,9) ! NET NUCLEAR FUEL
                     CAPITAL_LEASE_ADJ = CAPITAL_LEASE_ADJ
     +                            + TRANSFER_VALUE(MO,YR,ASSET_CLASS,55) ! CAPITAL LEASE ADJ
                  ENDIF
                  IF(ALLOCATED(SALE_REMOVAL)) THEN
                     GPV_ADJUSTMENT = GPV_ADJUSTMENT
     +                               - SALE_REMOVAL(MO,YR,ASSET_CLASS,3)
                     DEF_TAX_ADJUSTMENT = DEF_TAX_ADJUSTMENT
     +                               - SALE_REMOVAL(MO,YR,ASSET_CLASS,5)
                  ENDIF
                  R_BALANCE_SHEET_VALUES(MO,
     +                                 Monthly Gross Plant in Service) =
     +                       R_BALANCE_SHEET_VALUES(MO,
     +                                   Monthly Gross Plant in Service)
     +                       + GPV_ADJUSTMENT
                  R_BALANCE_SHEET_VALUES(MO,
     +                                    Monthly Acc Dep Adjustments) =
     +                               TRANSFER_VALUE(MO,YR,ASSET_CLASS,2)
     +                               - SALE_REMOVAL(MO,YR,ASSET_CLASS,4)
                  R_BALANCE_SHEET_VALUES(MO,
     +                                   Monthly Deferred Income Taxes)=
     +                       R_BALANCE_SHEET_VALUES(MO,
     +                                    Monthly Deferred Income Taxes)
     +                       + DEF_TAX_ADJUSTMENT
c                  R_BALANCE_SHEET_VALUES(MO,Monthly CWIP) =
c     +                           R_BALANCE_SHEET_VALUES(MO,Monthly CWIP)
                  R_TRANSFER_VARIABLES(MO,Monthly CWIP)=CWIP_BALANCE_ADJ ! CWIP ADJUSTMENTS
                  R_TRANSFER_VARIABLES(MO,Monthly Net Nuclear Fuel) =
     +                                                  NET_NUC_FUEL_ADJ
                  R_TRANSFER_VARIABLES(MO,Monthly Capitialized Leases) =
     +                                                 CAPITAL_LEASE_ADJ
                  R_TRANSFER_VARIABLES(MO,
     +                            monthly_other_long_term_liabs) =
     +                                                   LT_LIAB_BAL_ADJ
                  R_TRANSFER_VARIABLES(MO,Monthly Net Goodwill) =
     +                     R_TRANSFER_VARIABLES(MO,Monthly Net Goodwill)
     +                     + TRANSFER_VALUE(MO,YR,ASSET_CLASS,17) ! Goodwill
               ENDDO
               IF(ALLOCATED(TRANSFER_VALUE)) THEN
                  MO = 1
                  R_TRANSFER_VARIABLES(MO,monthly_common_stock) =
     +                              TRANSFER_VALUE(MO,YR,ASSET_CLASS,15)
                  R_TRANSFER_VARIABLES(MO,
     +                           monthly_coi_retained_earnings_bal) =
     +                            TRANSFER_VALUE(MO,YR,ASSET_CLASS,58)
                  R_TRANSFER_VARIABLES(MO,
     +                                  monthly_retained_earnings_bal) =
     +                           TRANSFER_VALUE(MO,YR,ASSET_CLASS,10)
                  R_TRANSFER_VARIABLES(MO,monthly_preferred_stock) =
     +                           TRANSFER_VALUE(MO,YR,ASSET_CLASS,40)
                  R_TRANSFER_VARIABLES(MO,monthly_long_term_debt) =
     +                          TRANSFER_VALUE(MO,YR,ASSET_CLASS,41)
              R_TRANSFER_VARIABLES(MO,monthly_short_term_debt_bal) =
     +                          TRANSFER_VALUE(MO,YR,ASSET_CLASS,31)
                  R_TRANSFER_VARIABLES(MO,
     +                           monthly_nucl_decommiss_liability) =
     +                         TRANSFER_VALUE(MO,YR,ASSET_CLASS,44)
              R_TRANSFER_VARIABLES(MO,Monthly Liabilities NEC) =
     +                        TRANSFER_VALUE(MO,YR,ASSET_CLASS,54)
                  R_TRANSFER_VARIABLES(MO,
     +                                Monthly Nuc Decommiss Trust Bal) =
     +                              TRANSFER_VALUE(MO,YR,ASSET_CLASS,25)
c                  R_TRANSFER_VARIABLES(MO,) =
c                  R_TRANSFER_VARIABLES(MO,) =
                  DO MO= 2, 12
                     R_TRANSFER_VARIABLES(MO,monthly_common_stock) =
     +                     R_TRANSFER_VARIABLES(MO,monthly_common_stock)
     +                     + TRANSFER_VALUE(MO,YR,ASSET_CLASS,15)
                     R_TRANSFER_VARIABLES(MO,
     +                              monthly_coi_retained_earnings_bal) =
     +                   R_TRANSFER_VARIABLES(MO,
     +                                monthly_coi_retained_earnings_bal)
     +                   + TRANSFER_VALUE(MO,YR,ASSET_CLASS,58)
                     R_TRANSFER_VARIABLES(MO,
     +                                  monthly_retained_earnings_bal) =
     +                     R_TRANSFER_VARIABLES(MO,
     +                                    monthly_retained_earnings_bal)
     +                     + TRANSFER_VALUE(MO,YR,ASSET_CLASS,10)
                     R_TRANSFER_VARIABLES(MO,
     +                                Monthly Nuc Decommiss Trust Bal) =
     +                   R_TRANSFER_VARIABLES(MO,
     +                                  Monthly Nuc Decommiss Trust Bal)
     +                   + TRANSFER_VALUE(MO,YR,ASSET_CLASS,25)
                     R_TRANSFER_VARIABLES(MO,monthly_preferred_stock) =
     +                  R_TRANSFER_VARIABLES(MO,monthly_preferred_stock)
     +                  + TRANSFER_VALUE(MO,YR,ASSET_CLASS,40)
                     R_TRANSFER_VARIABLES(MO,monthly_long_term_debt) =
     +                   R_TRANSFER_VARIABLES(MO,monthly_long_term_debt)
     +                   + TRANSFER_VALUE(MO,YR,ASSET_CLASS,41)
                     R_TRANSFER_VARIABLES(MO,
     +                                    monthly_short_term_debt_bal) =
     +                      R_TRANSFER_VARIABLES(MO,
     +                                      monthly_short_term_debt_bal)
     +                      + TRANSFER_VALUE(MO,YR,ASSET_CLASS,31)
                     R_TRANSFER_VARIABLES(MO,
     +                      monthly_nucl_decommiss_liability) =
     +                 R_TRANSFER_VARIABLES(MO,
     +                         monthly_nucl_decommiss_liability)
     +                  + TRANSFER_VALUE(MO,YR,ASSET_CLASS,44)
                 R_TRANSFER_VARIABLES(MO,Monthly Liabilities NEC) =
     +               R_TRANSFER_VARIABLES(MO,Monthly Liabilities NEC)
     +                  + TRANSFER_VALUE(MO,YR,ASSET_CLASS,54)
                  END DO
               ENDIF
               
            ENDIF
         ENDIF
      RETURN
C**********************************************************************
      ENTRY MONTHLY_ADDENDUM_CAL_VALUES(R_CLASS,R_YR,R_INCOME_VALUES)
C**********************************************************************
C
C
C
         IF(ALLOCATED(SALE_REMOVAL)) THEN
            IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
               YR = R_YR
               IF(R_CLASS == 0) THEN
                  ASSET_CLASS = 0
               ELSE
                  ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
               ENDIF
               IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN
                  DO MO = 0, 12
                     R_INCOME_VALUES(MO,Monthly ATL Gain Amort) =
     +                                 SALE_REMOVAL(MO,YR,ASSET_CLASS,6)
                     R_INCOME_VALUES(MO,Monthly BTL Gain Amort) =
     +                                 SALE_REMOVAL(MO,YR,ASSET_CLASS,7)
                  ENDDO
               ENDIF
            ENDIF
         ENDIF
      RETURN
C***********************************************************************
      ENTRY MONTHLY_ADDENDUM_CIAC_VALUES(R_CLASS,R_YR,
     +                                  R_CIAC_MONTHLY_AMORT_RATE,
     +                                  R_CIAC_MONTHLY_ADDENDUM_2_AMORT,
     +                                  R_CIAC_MONTHLY_CASH)
C***********************************************************************
C
C
         DO MO = 0, 12
            R_CIAC_MONTHLY_AMORT_RATE(MO) = 0.
            R_CIAC_MONTHLY_ADDENDUM_2_AMORT(MO) = 0.
            R_CIAC_MONTHLY_CASH(MO) = 0.
         ENDDO
C
         IF(ALLOCATED(INVESTMENTS)) THEN
            IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
               YR = R_YR
               IF(R_CLASS == 0) THEN
                  ASSET_CLASS = 0
               ELSE
                  ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
               ENDIF
               IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN
                  DO MO = 0, 12
                     R_CIAC_MONTHLY_AMORT_RATE(MO) =
     +                                      CD_CIAC(MO,YR,ASSET_CLASS,4)
                     R_CIAC_MONTHLY_ADDENDUM_2_AMORT(MO) =
     +                                      CD_CIAC(MO,YR,ASSET_CLASS,5)
                     R_CIAC_MONTHLY_CASH(MO) =
     +                                      CD_CIAC(MO,YR,ASSET_CLASS,3)
                  ENDDO
               ENDIF
            ENDIF
         ENDIF
      RETURN
      END
C**********************************************************************
      FUNCTION ADDENDUM_POSITION(R_ADDENDUM_TITLE,
     +                           R_ADDED_TO_CURRENT_VALUE)
C**********************************************************************
C
      INTEGER*2 ADDENDUM_POSITION
      CHARACTER*(*) R_ADDENDUM_TITLE
      CHARACTER*30 ADDENDUM_TITLE
      LOGICAL*1 R_ADDED_TO_CURRENT_VALUE
C
         ADDENDUM_TITLE = R_ADDENDUM_TITLE
         R_ADDED_TO_CURRENT_VALUE = .TRUE.
         SELECT CASE (trim(ADDENDUM_TITLE))
C
C PAYMENTS POSITIONS
C
         CASE ('Decommissioning Fund') !4 PAYMENTS AND FUND EARNINGS
            ADDENDUM_POSITION = 1
         CASE ('Decommissioning Liability Changes'(1:30))
            ADDENDUM_POSITION = 2
         CASE ('Post Retirement Medical')
            ADDENDUM_POSITION = 3
         CASE ('Retiree Medical Payments')
            ADDENDUM_POSITION = 4
         CASE ('Earnings on Decommissioning Fund'(1:30))
            ADDENDUM_POSITION = 5
         CASE ('Unfunded Pension','Funded Pension','Pension')
            ADDENDUM_POSITION = 6
         CASE ('Storm Repairs','Turbine Overhaul')
            ADDENDUM_POSITION = 7
         CASE ('Vacation Pay')
            ADDENDUM_POSITION = 8
C
C INVESTMENT POSITIONS
C
         CASE ('Dividend-Subsidiary')
            ADDENDUM_POSITION = 1
         CASE ('Net Income-Subsidiary')
            ADDENDUM_POSITION = 2
         CASE ('Investment in Subsidiary')
            ADDENDUM_POSITION = 3
         CASE ('Other Investments')
            ADDENDUM_POSITION = 4
         CASE ('Capitialized Lease Additions',
     +         'Capitalized Lease Additions')
            ADDENDUM_POSITION = 5
         CASE ('Cash for Assets')
            ADDENDUM_POSITION = 6
         CASE ('Long Term Investment Change')
            ADDENDUM_POSITION = 7
C
C EARNINGS ON FUNDS
C

         CASE ('Post Retirement Fund')
            ADDENDUM_POSITION = 2
         CASE ('Short Term Investments')
            ADDENDUM_POSITION = 3
         CASE ('Long Term Investments')
            ADDENDUM_POSITION = 4
         CASE ('STD Rate')
            ADDENDUM_POSITION = 5
            R_ADDED_TO_CURRENT_VALUE = .FALSE.
         CASE ('Decommissioning Rate')
            ADDENDUM_POSITION = 6
            R_ADDED_TO_CURRENT_VALUE = .FALSE.
         CASE ('Post Retirement Rate')
            ADDENDUM_POSITION = 7
            R_ADDED_TO_CURRENT_VALUE = .FALSE.
         CASE ('STI Rate')
            ADDENDUM_POSITION = 8
            R_ADDED_TO_CURRENT_VALUE = .FALSE.
         CASE ('LTI Rate')
            ADDENDUM_POSITION = 9
            R_ADDED_TO_CURRENT_VALUE = .FALSE.
         CASE ('Customer Deposits Rate')
            ADDENDUM_POSITION = 10
            R_ADDED_TO_CURRENT_VALUE = .FALSE.
         CASE ('OCI Post Retirement Rate')
            ADDENDUM_POSITION = 11
            R_ADDED_TO_CURRENT_VALUE = .FALSE.
        CASE ('OCI Decommissioning Rate')
            ADDENDUM_POSITION = 12
            R_ADDED_TO_CURRENT_VALUE = .FALSE.
C
C RATE BASE POSITIONS
C
         CASE ('Assets NEC Adjustment')
            ADDENDUM_POSITION = 1
         CASE ('Rate Base Adjustment')
            ADDENDUM_POSITION = 2
C
C CD/CIAC POSITIONS
C
         CASE ('CD Balance as % of Revenues')
            ADDENDUM_POSITION = 1
            R_ADDED_TO_CURRENT_VALUE = .FALSE.
         CASE ('CD Addendum','Customer Deposits')
            ADDENDUM_POSITION = 2
         CASE ('CIAC Cash Contributions')
            ADDENDUM_POSITION = 3
         CASE ('CIAC Amortization Rate')
            ADDENDUM_POSITION = 4
            R_ADDED_TO_CURRENT_VALUE = .FALSE.
         CASE ('CIAC Addendum to Amortization')
            ADDENDUM_POSITION = 5
C
C Working Capital POSITIONS
C
         CASE ('Assets NEC-Addendum','Addendum-Assets NEC')
            ADDENDUM_POSITION = 1
         CASE ('Liabilities NEC-Addendum','Addendum-Liabilities NEC')
            ADDENDUM_POSITION = 2
         CASE ('In-Fuel Inventory','Fuel Inventory')
            ADDENDUM_POSITION = 3
         CASE ('In-Materials&Supplies','Materials&Supplies')
            ADDENDUM_POSITION = 4
         CASE ('In-Gas Storage','Gas Storage')
            ADDENDUM_POSITION = 5
         CASE ('Rate%-Revenues Receivable')
            ADDENDUM_POSITION = 6
         CASE ('Addendum-Revenues Receivable')
            ADDENDUM_POSITION = 7
         CASE ('Rate%-Expenses Payable')
            ADDENDUM_POSITION = 8
         CASE ('Addendum-Expenses Payable')
            ADDENDUM_POSITION = 9
         CASE ('Rate%-Assets NEC')
            ADDENDUM_POSITION = 10
         CASE ('Rate%-Liabilities NEC')
            ADDENDUM_POSITION = 11
         CASE ('Out-Fuel Inventory')
            ADDENDUM_POSITION = 12
         CASE ('Out-Materials&Supplies')
            ADDENDUM_POSITION = 13
         CASE ('Out-Gas Storage')
            ADDENDUM_POSITION = 14
C
C Sale/Removal POSITIONS
C
         CASE ('Net Salvage Value','Net Removal')
            ADDENDUM_POSITION = 1
         CASE ('Cash Received','Cash From Sale')
            ADDENDUM_POSITION = 2
         CASE ('Gross Book Value')
            ADDENDUM_POSITION = 3
         CASE ('Cumulative Depreciation')
            ADDENDUM_POSITION = 4
         CASE ('Deferred Taxes-Adjustment')
            ADDENDUM_POSITION = 5
         CASE ('ATL Amortization of Gain or Loss'(1:30))
            ADDENDUM_POSITION = 6
         CASE ('BTL Amortization of Gain or Loss'(1:30))
            ADDENDUM_POSITION = 7
         CASE ('Cost of Removal')
            ADDENDUM_POSITION = 8
         CASE ('Salvage Value')
            ADDENDUM_POSITION = 9
         CASE ('Addendum to Book Gain/Loss')
            ADDENDUM_POSITION = 10
C
C Shareholder Value POSITIONS
C
         CASE ('Equity Risk Adjustment,')
            ADDENDUM_POSITION = 1
         CASE ('Economic Assets Adjustment,')
            ADDENDUM_POSITION = 2
         CASE ('Operating Profits Adjustment,')
            ADDENDUM_POSITION = 3
         CASE ('Capitial Recovery EXCLUDED form OPAT'(1:30))
            ADDENDUM_POSITION = 4
            R_ADDED_TO_CURRENT_VALUE = .FALSE.
C
C Transfers
C
         CASE ('Gross Plant Value','Plant Additions')
            ADDENDUM_POSITION = 1
         CASE ('Cumulated Plant Depreciation','Depreciation Additions')
            ADDENDUM_POSITION = 2
         CASE ('Balance Deferred Taxes','Balance Deferred Taxes-Cr')
            ADDENDUM_POSITION = 3
         CASE ('Deferred Debit Adjustment')
            ADDENDUM_POSITION = 4
         CASE ('ATL Debit Amortization')
            ADDENDUM_POSITION = 5
         CASE ('BTL Debt Amortization')
            ADDENDUM_POSITION = 6
         CASE ('Annual Deferred Taxes')
            ADDENDUM_POSITION = 7
         CASE ('Deferred ITCs Balance')
            ADDENDUM_POSITION = 8
         CASE ('Net Nuclear Fuel')
            ADDENDUM_POSITION = 9
         CASE ('Retained Earnings')
            ADDENDUM_POSITION = 10
         CASE ('Extra-Ordinary Expense')
            ADDENDUM_POSITION = 11
         CASE ('LT Liabilities Balance')
            ADDENDUM_POSITION = 12
         CASE ('CWIP Balance')
            ADDENDUM_POSITION = 13
         CASE ('Balance Deferred Taxes-Dr')
            ADDENDUM_POSITION = 14
         CASE ('Paid-in Capital')
            ADDENDUM_POSITION = 15
         CASE ('Subsidiary Investment')
            ADDENDUM_POSITION = 16
         CASE ('Goodwill')
            ADDENDUM_POSITION = 17
         CASE ('Regulatory Assets')
            ADDENDUM_POSITION = 18
         CASE ('FASB 109')
            ADDENDUM_POSITION = 19
         CASE ('FASB 133')
            ADDENDUM_POSITION = 21
         CASE ('Unamortizated Interest Bal')
            ADDENDUM_POSITION = 22
         CASE ('Nuclear Decommissioning Fund')
            ADDENDUM_POSITION = 25
         CASE ('Post Retirement Medical Bal')
            ADDENDUM_POSITION = 26
         CASE ('Short Term Investments Bal')
            ADDENDUM_POSITION = 31
         CASE ('Assets NEC')
            ADDENDUM_POSITION = 39
         CASE ('Preferred Stock Bal')
            ADDENDUM_POSITION = 40
         CASE ('Long Term Debt Bal')
            ADDENDUM_POSITION = 41
         CASE ('Nuclear Decommissioning Liab')
            ADDENDUM_POSITION = 44
         CASE ('Accrued Pension')
            ADDENDUM_POSITION = 49
         CASE ('Liabilities NEC Bal')
            ADDENDUM_POSITION = 54
         CASE ('Capitialized Lease Bal')
            ADDENDUM_POSITION = 55
         CASE ('ARO Net Asset')
            ADDENDUM_POSITION = 56
         CASE ('ARO Liability')
            ADDENDUM_POSITION = 57
         CASE ('COI Earnings')
            ADDENDUM_POSITION = 58
         CASE ('Deferred Purchased Power')
            ADDENDUM_POSITION = 59
C
C Adjustment below are not active
C
         CASE ('Other Investments Bal')
            ADDENDUM_POSITION = 23
         CASE ('Notes Receivable')
            ADDENDUM_POSITION = 24
         CASE ('Deferred Revenues')
            ADDENDUM_POSITION = 27
         CASE ('Deferred Fuel')
            ADDENDUM_POSITION = 28
         CASE ('Deferred Purchased Gas')
            ADDENDUM_POSITION = 29
         CASE ('Long Term Investments Bal')
            ADDENDUM_POSITION = 30
         CASE ('Fuel Inventory Bal')
            ADDENDUM_POSITION = 32
         CASE ('Gas in Storage Bal')
            ADDENDUM_POSITION = 33
         CASE ('Materials & Supplies')
            ADDENDUM_POSITION = 34
         CASE ('Accounts Receivable')
            ADDENDUM_POSITION = 35
         CASE ('Revenues Receivable')
            ADDENDUM_POSITION = 36
         CASE ('Unbilled Receivables')
            ADDENDUM_POSITION = 37
         CASE ('Taxes Receivable')
            ADDENDUM_POSITION = 38
         CASE ('Current LT Debt Bal')
            ADDENDUM_POSITION = 42
         CASE ('Notes Payable Bal')
            ADDENDUM_POSITION = 43
         CASE ('Short Term Debt Bal')
            ADDENDUM_POSITION = 45
         CASE ('Customer Deposits Bal')
            ADDENDUM_POSITION = 46
         CASE ('CICA')
            ADDENDUM_POSITION = 47
         CASE ('Post Retirement Payable Bal')
            ADDENDUM_POSITION = 48
         CASE ('Accrued Vacation Pay')
            ADDENDUM_POSITION = 50
         CASE ('Deferred Gains Bal')
            ADDENDUM_POSITION = 51
         CASE ('Storm Reserve Bal','Turbine Overhaul Bal')
            ADDENDUM_POSITION = 52
         CASE ('Accounts Payable Bal')
            ADDENDUM_POSITION = 53
C
C Actuals
C
         CASE ('Deferred Taxes-Cr')
            ADDENDUM_POSITION = 1
         CASE ('Federal Income Taxes','Total Federal Income Taxes')
            ADDENDUM_POSITION = 2
         CASE ('State Income Taxes','Total State Income Taxes')
            ADDENDUM_POSITION = 3
         CASE ('Deferred Taxes-Dr')
            ADDENDUM_POSITION = 4
         CASE ('ATL Federal Income Taxes')
            ADDENDUM_POSITION = 5
         CASE ('ATL State Income Taxes')
            ADDENDUM_POSITION = 6
         CASE ('BTL Federal Income Taxes')
            ADDENDUM_POSITION = 7
         CASE ('BTL State Income Taxes')
            ADDENDUM_POSITION = 8
C
C Other
C
         CASE ('SRP Operating Target Sales')
            ADDENDUM_POSITION = 1
         CASE ('STD Minimum Balance')
            ADDENDUM_POSITION = 2
         CASE ('Cash Minimum Balance')
            ADDENDUM_POSITION = 3
         CASE ('ElectPlan Fuel')
            ADDENDUM_POSITION = 4
         CASE ('ElectPlan Purchases')
            ADDENDUM_POSITION = 5
         CASE ('GRE DSC Principal Payments')
            ADDENDUM_POSITION = 6
         CASE ('GRE DSC Interest Payments')
            ADDENDUM_POSITION = 7
C
C FASB 143/ARO and FASB 87
C
         CASE ('NucDecom Discount Rate')
            ADDENDUM_POSITION = 1
         CASE ('NucDecom Liability Addendum')
            ADDENDUM_POSITION = 2
         CASE ('Other Comp Income')
            ADDENDUM_POSITION = 3
         CASE ('Intangible Asset')
            ADDENDUM_POSITION = 4
         CASE ('Pension Liab Adj')             
            ADDENDUM_POSITION = 5
         CASE ('OCI Def Tax Adj')             
            ADDENDUM_POSITION = 6
         CASE ('ARO Discount Rate')
            ADDENDUM_POSITION = 1
         CASE ('ARO Accretion Amount Addendum')
            ADDENDUM_POSITION = 2
         CASE ('ARO Cash Payment')
            ADDENDUM_POSITION = 7
         CASE ('ARO Cash to Bal Sheet Trust')
            ADDENDUM_POSITION = 8
C
C
C CAPITAL EXPENDITURES
C
         CASE ('Non-cash Pension')
            ADDENDUM_POSITION = 1
c
c Cash Additions to xxxx
c
         CASE ('Cash')
            ADDENDUM_POSITION = 1
         CASE ('Accrued Member Revenue')  ! WVPA  addtion 6/24/04
            ADDENDUM_POSITION = 3
   
C POSITIONS
C

         CASE DEFAULT
            ADDENDUM_POSITION = -99
         END SELECT
C
      RETURN
      END
