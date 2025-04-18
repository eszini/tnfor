C     Last change: MSG 1/10/2010 2:58:00 PM
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C               EXISTING ASSET ANALYSIS MODULE--MOD 1                  C
C        Copyright (c) 1982 M.S. Gerber & Associates, Inc.             C
C                      All Rights Reserved                             C
C                 Transferred to MIDAS in 1985                         C
C        MIDAS GOLD CHANGES 6/11/91; 1/15/93                           C
C        Copyright (c) 1991, 1993, 1994 M.S. Gerber & Associates, Inc. C
C                      ALL RIGHTS RESERVED                             C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE EA_OBJECT
      use end_routine, only: end_program, er_message
C
      use spindriftlib
      use prod_arrays_dimensions
      USE SIZECOM

      INTEGER(kind=2) :: IREC,INUNIT,LRECL=300,I
      INTEGER(kind=4) :: IOS,IOS_BASE
      INTEGER(kind=2) :: R_UNIT_NUM
      INTEGER(kind=2) :: NUM_OF_OL_ASSET_CLASSES=0,
     +          MAX_OL_EXISTING_CLASS_ID_NUM=0
      INTEGER(kind=2) :: NUM_OF_BC_ASSET_CLASSES=0,
     +          MAX_BC_EXISTING_CLASS_ID_NUM=0
      INTEGER(kind=2) :: UNIT_NUM=10,
     +          ASSET_CLASS_NUM,ASSET_CLASS_VECTOR
      INTEGER(kind=2) :: R_NUM_OF_EXISTING_CLASSES,
     +          R_MAX_EXISTING_CLASS_NUM,R_EXISTING_CLASS_POINTERS(*),
     +          BOOK_DEP_VECTOR
      CHARACTER(len=5) :: BASE_FILE_NAME,OVERLAY_FAMILY_NAME,EXASTFIL
      CHARACTER(len=30) :: DESC,COMMENT,TEMP_DESC
      CHARACTER(len=256) :: FILE_NAME
      CHARACTER(len=256) :: BASE_FILE_DIRECTORY,OUTPUT_DIRECTORY
      CHARACTER(len=1) :: ACCOUNT_ACTIVE,DEP_METHOD
      CHARACTER(len=256) :: DATA_DRIVE
      LOGICAL(kind=4) :: FILE_EXISTS
C DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER(len=1024) :: RECLN
C DECLARATION FOR /EXISTING ASSETS FILE/
      INTEGER(kind=2) :: SERVYR,DELETE
      INTEGER :: ACCTNO,WVPA_SUB_ACCOUNT_NUMBER,
     +        WVPA_ACCOUNTING_UNIT
      REAL(kind=4) :: BOKGPV,BOKNPV,BKDPRT,TAXGPV,TXDEP1,TXRATE,DBRATE,
     +     RETIRE(AVAIL_DATA_YEARS),TXNORM,SALVAGE_VALUE,
     +     PROPERTY_TAX_VALUE,
     +     REGULATORY_ALLOCATOR
      CHARACTER(len=23) :: FILE_TYPE='Existing-Asset Accounts'
      CHARACTER(len=4) :: DEPMTH
      CHARACTER(len=2) :: EXASTOL='BC',R_EXASTOL
      INTEGER(kind=2) :: BC_EXISTING_ASSET_CLASS_POINTER(:),
     +          OL_EXISTING_ASSET_CLASS_POINTER(:),
     +          TEMP_ASSET_CLASS_POINTER(:)
      ALLOCATABLE :: BC_EXISTING_ASSET_CLASS_POINTER,
     +               OL_EXISTING_ASSET_CLASS_POINTER,
     +               TEMP_ASSET_CLASS_POINTER
      SAVE BC_EXISTING_ASSET_CLASS_POINTER,
     +     OL_EXISTING_ASSET_CLASS_POINTER

      REAL(kind=4) :: ADR_LIFE,TOTAL_BOOK_DEP
      INTEGER(kind=2) :: RETIRE_VECTOR,WVPA_COMPANY_ID
      LOGICAL(kind=1) :: LAHEY_LF95
      CHARACTER(len=30) :: SCREEN_OUTPUT
C***********************************************************************
C
C          ROUTINE TO CONVERT METAFILE FILES TO DIRECT-ACESS BINARY
C          COPYRIGHT (C) 1983, 84, 85  M.S. GERBER & ASSOCIATES, INC.
C
C***********************************************************************
C
C CONVERT THE EXISTING-ASSET FILE
C***********************************************************************
      ENTRY EA_MAKEBIN
C***********************************************************************
      BASE_FILE_NAME = EXASTFIL()
      DATA_DRIVE = OUTPUT_DIRECTORY()
      FILE_NAME = trim(BASE_FILE_DIRECTORY())//
     +                             "EAB"//trim(BASE_FILE_NAME)//".DAT"
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      IF(FILE_EXISTS) THEN
         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//BASE_FILE_NAME
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_LOCATE_WRITE(16,30,BASE_FILE_NAME,ALL_VERSIONS,0)
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         ENDIF
         ALLOCATE(TEMP_ASSET_CLASS_POINTER(1024))
         TEMP_ASSET_CLASS_POINTER = 0
         OPEN(10,FILE=FILE_NAME)
         OPEN(11,FILE=trim(DATA_DRIVE)//"BCEXAST.BIN",ACCESS="DIRECT",
     +           STATUS="UNKNOWN",RECL=LRECL)
         IREC = 0.
         READ(10,*) DELETE
         DO
            ASSET_CLASS_NUM = 0 
            ASSET_CLASS_VECTOR = 0
            BOKGPV = 0.
            BOKNPV = 0.
            BKDPRT = 0.
            REGULATORY_ALLOCATOR = 0.
            TAXGPV = 0.
            TXDEP1 = 0.
            SERVYR = 2200
            TXRATE = 0.
            DBRATE = 100.
            TXNORM = 0.
            SALVAGE_VALUE = 0.
            PROPERTY_TAX_VALUE = 0.
            TOTAL_BOOK_DEP = 0.
            ADR_LIFE = 99.
            RETIRE_VECTOR = 0
            BOOK_DEP_VECTOR = 0
            ACCOUNT_ACTIVE = 'A'
            DEP_METHOD = 'G'
            WVPA_COMPANY_ID = 1
            DO
               READ(10,1000,IOSTAT=IOS) RECLN
               IF(IOS /= 0) EXIT
               IF(RECLN(1:1) == '7') EXIT
               RETIRE  = -999999.
               RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               READ(RECLN,*) DELETE,ACCTNO,BOKGPV,BOKNPV,    ! ,ERR=200
     +            BKDPRT,REGULATORY_ALLOCATOR,TAXGPV,TXDEP1,SERVYR,
     +            TXRATE,DEPMTH,DBRATE,TXNORM,DESC,RETIRE,COMMENT,
     +            SALVAGE_VALUE,PROPERTY_TAX_VALUE,
     +            ASSET_CLASS_NUM,ASSET_CLASS_VECTOR,
     +            RETIRE_VECTOR,ADR_LIFE,TOTAL_BOOK_DEP,
     +            BOOK_DEP_VECTOR,ACCOUNT_ACTIVE,
     +            WVPA_ACCOUNTING_UNIT,
     +            WVPA_SUB_ACCOUNT_NUMBER, ! 55
     +            DEP_METHOD,
     +            WVPA_COMPANY_ID ! 57
C
             IF(DELETE < 8) CALL SET_ASSET_CLASSES(ASSET_CLASS_NUM,
     +                                NUM_OF_BC_ASSET_CLASSES,
     +                                 MAX_BC_EXISTING_CLASS_ID_NUM,
     +                                TEMP_ASSET_CLASS_POINTER)
C
C RIPPLE RETIRE INFO DOWN
C 
               IF(RETIRE(1) == -999999.) RETIRE(1) = 0.
               DO I = 2, AVAIL_DATA_YEARS
                  IF(RETIRE(I) == -999999.) RETIRE(I) = RETIRE(I-1)
               ENDDO
C
               IREC = IREC + 1
               WRITE(11,REC=IREC) DELETE,ACCTNO,BOKGPV,BOKNPV,BKDPRT,
     +            REGULATORY_ALLOCATOR,TAXGPV,TXDEP1,SERVYR,
     +            TXRATE,DEPMTH,DBRATE,TXNORM,RETIRE,DESC,SALVAGE_VALUE,
     +            PROPERTY_TAX_VALUE,ASSET_CLASS_NUM,ASSET_CLASS_VECTOR,
     +            RETIRE_VECTOR,ADR_LIFE,TOTAL_BOOK_DEP,
     +            BOOK_DEP_VECTOR,ACCOUNT_ACTIVE,
     +            WVPA_ACCOUNTING_UNIT,
     +            WVPA_SUB_ACCOUNT_NUMBER,DEP_METHOD,
     +            WVPA_COMPANY_ID
            ENDDO
            IF(IOS /= 0) EXIT
         ENDDO
         CLOSE(10)
c        ENDFILE(11)
         CLOSE(11)
         IF(MAX_BC_EXISTING_CLASS_ID_NUM > 0) THEN
            ALLOCATE(BC_EXISTING_ASSET_CLASS_POINTER
     +                                   (MAX_BC_EXISTING_CLASS_ID_NUM))
            BC_EXISTING_ASSET_CLASS_POINTER =
     +          TEMP_ASSET_CLASS_POINTER(1:MAX_BC_EXISTING_CLASS_ID_NUM)
         ENDIF
         DEALLOCATE(TEMP_ASSET_CLASS_POINTER)
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
C OVERLAY THE EXISTING-ASSET FILE
C***********************************************************************
      ENTRY EA_MAKEOVL(OVERLAY_FAMILY_NAME)
C***********************************************************************
      IF(LAHEY_LF95()) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSE
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         CALL LOCATE(10,51)
      ENDIF
      DATA_DRIVE = OUTPUT_DIRECTORY()
      FILE_NAME=trim(DATA_DRIVE)//"EAO"//
     +                               trim(OVERLAY_FAMILY_NAME)//".DAT"
      OPEN(10,FILE=FILE_NAME)
      READ(10,*) DELETE
      INUNIT = 12
      IF(EXASTOL == 'BC') THEN
         OPEN(11,FILE=trim(DATA_DRIVE)//"BCEXAST.BIN",
     +                                       ACCESS="DIRECT",RECL=LRECL)
         INUNIT = 11
      ENDIF
      OPEN(12,FILE=trim(DATA_DRIVE)//"OLEXAST.BIN",ACCESS="DIRECT",
     +                                      STATUS="UNKNOWN",RECL=LRECL)
      IF(ALLOCATED(OL_EXISTING_ASSET_CLASS_POINTER))
     +                       DEALLOCATE(OL_EXISTING_ASSET_CLASS_POINTER)
      ALLOCATE(TEMP_ASSET_CLASS_POINTER(1024))
      TEMP_ASSET_CLASS_POINTER = 0
      IREC = 0
      NUM_OF_OL_ASSET_CLASSES = 0
      MAX_OL_EXISTING_CLASS_ID_NUM = 0
      DO
         DO
            READ(10,1000,IOSTAT=IOS) RECLN
            IF(RECLN(1:1) == '7') EXIT
            IREC = IREC + 1
            READ(INUNIT,REC=IREC,IOSTAT=IOS_BASE) DELETE,ACCTNO,BOKGPV,
     +         BOKNPV,BKDPRT,REGULATORY_ALLOCATOR,TAXGPV,TXDEP1,
     +         SERVYR,TXRATE,DEPMTH,DBRATE,TXNORM,RETIRE,DESC,
     +         SALVAGE_VALUE,PROPERTY_TAX_VALUE,
     +         ASSET_CLASS_NUM,ASSET_CLASS_VECTOR,
     +         RETIRE_VECTOR,ADR_LIFE,TOTAL_BOOK_DEP,
     +         BOOK_DEP_VECTOR,ACCOUNT_ACTIVE,
     +         WVPA_ACCOUNTING_UNIT,
     +         WVPA_SUB_ACCOUNT_NUMBER,DEP_METHOD,
     +         WVPA_COMPANY_ID
            IF(IOS_BASE /= 0) EXIT
            IF(IOS == 0) THEN
               RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               READ(RECLN,*,ERR=200) DELETE,ACCTNO,BOKGPV,BOKNPV,
     +            BKDPRT,REGULATORY_ALLOCATOR,TAXGPV,TXDEP1,SERVYR,
     +            TXRATE,DEPMTH,DBRATE,TXNORM,TEMP_DESC,RETIRE,COMMENT,
     +            SALVAGE_VALUE,PROPERTY_TAX_VALUE,
     +            ASSET_CLASS_NUM,ASSET_CLASS_VECTOR,
     +            RETIRE_VECTOR,ADR_LIFE,TOTAL_BOOK_DEP,
     +            BOOK_DEP_VECTOR,ACCOUNT_ACTIVE,
     +            WVPA_ACCOUNTING_UNIT,
     +            WVPA_SUB_ACCOUNT_NUMBER,DEP_METHOD,
     +            WVPA_COMPANY_ID
            ENDIF
C
            IF(DELETE < 8) CALL SET_ASSET_CLASSES(ASSET_CLASS_NUM,
     +                             NUM_OF_OL_ASSET_CLASSES,
     +                             MAX_OL_EXISTING_CLASS_ID_NUM,
     +                             TEMP_ASSET_CLASS_POINTER)
C
            WRITE(12,REC=IREC) DELETE,ACCTNO,BOKGPV,BOKNPV,BKDPRT,
     +         REGULATORY_ALLOCATOR,TAXGPV,TXDEP1,SERVYR,TXRATE,DEPMTH,
     +         DBRATE,TXNORM,RETIRE,DESC,SALVAGE_VALUE,
     +         PROPERTY_TAX_VALUE,
     +         ASSET_CLASS_NUM,ASSET_CLASS_VECTOR,
     +         RETIRE_VECTOR,ADR_LIFE,TOTAL_BOOK_DEP,
     +         BOOK_DEP_VECTOR,ACCOUNT_ACTIVE,
     +            WVPA_ACCOUNTING_UNIT,
     +            WVPA_SUB_ACCOUNT_NUMBER,DEP_METHOD,
     +            WVPA_COMPANY_ID
         ENDDO
         IF(IOS_BASE /= 0) EXIT
      ENDDO
      CLOSE(10)
      CLOSE(12)
      IF(ALLOCATED(OL_EXISTING_ASSET_CLASS_POINTER)) 
     +                       DEALLOCATE(OL_EXISTING_ASSET_CLASS_POINTER)
      IF(MAX_OL_EXISTING_CLASS_ID_NUM > 0) THEN
         ALLOCATE(OL_EXISTING_ASSET_CLASS_POINTER
     +                                   (MAX_OL_EXISTING_CLASS_ID_NUM))
         OL_EXISTING_ASSET_CLASS_POINTER =
     +          TEMP_ASSET_CLASS_POINTER(1:MAX_OL_EXISTING_CLASS_ID_NUM)
      ENDIF
      DEALLOCATE(TEMP_ASSET_CLASS_POINTER)
      IF(EXASTOL == 'BC') CLOSE(11)
      EXASTOL= 'OL'
C     CALL SET_ASTOLOL_OL
      RETURN
C
  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from msgmmea SIID204'
      call end_program(er_message)
C
C***********************************************************************
      ENTRY RESET_EXASTOL
C***********************************************************************
         EXASTOL = 'BC'
      RETURN
C
C***********************************************************************
      ENTRY GET_EXAST_OL(R_EXASTOL)
C***********************************************************************
         R_EXASTOL = EXASTOL
      RETURN
C
C***********************************************************************
      ENTRY OPEN_EA_OUT_FILE(R_UNIT_NUM)
C***********************************************************************
         OPEN(R_UNIT_NUM,FILE=trim(OUTPUT_DIRECTORY())//EXASTOL//
     +                             'EA_AST.BIN',ACCESS='DIRECT',RECL=64)
      RETURN
C***********************************************************************
      ENTRY OPEN_EA_BASE_CASE_FILE(R_UNIT_NUM)
C***********************************************************************
         OPEN(R_UNIT_NUM,FILE=trim(OUTPUT_DIRECTORY())//
     +                         'BC_EXAST.BIN',FORM='UNFORMATTED',
     +                          ACCESS='SEQUENTIAL')
C
      RETURN
C***********************************************************************
      ENTRY OPEN_EXISTING_ASSET_FILE(R_UNIT_NUM)
C***********************************************************************
         OPEN(R_UNIT_NUM,FILE=trim(OUTPUT_DIRECTORY())//EXASTOL//
     +          "EXAST.BIN",ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
         UNIT_NUM = R_UNIT_NUM
      RETURN
C
C***********************************************************************
      ENTRY CLOSE_EXISTING_ASSET_FILE
C***********************************************************************
         CLOSE(UNIT_NUM,IOSTAT=IOS)
      RETURN
C***********************************************************************
      ENTRY RETURN_NUM_OF_EXISTING_CLASSES(R_NUM_OF_EXISTING_CLASSES,
     +                                       R_MAX_EXISTING_CLASS_NUM)
C***********************************************************************
         IF(EXASTOL == 'OL') THEN
            R_NUM_OF_EXISTING_CLASSES = NUM_OF_OL_ASSET_CLASSES
            R_MAX_EXISTING_CLASS_NUM = MAX_OL_EXISTING_CLASS_ID_NUM
         ELSE
            R_NUM_OF_EXISTING_CLASSES = NUM_OF_BC_ASSET_CLASSES
            R_MAX_EXISTING_CLASS_NUM = MAX_BC_EXISTING_CLASS_ID_NUM
         ENDIF
      RETURN
C***********************************************************************
      ENTRY RETURN_EXISTING_CLASS_POINTER(R_EXISTING_CLASS_POINTERS)
C***********************************************************************
         IF(EXASTOL == 'OL') THEN
            R_EXISTING_CLASS_POINTERS(1:MAX_OL_EXISTING_CLASS_ID_NUM) =
     +          OL_EXISTING_ASSET_CLASS_POINTER
     +                                  (1:MAX_OL_EXISTING_CLASS_ID_NUM)
         ELSE
            R_EXISTING_CLASS_POINTERS(1:MAX_BC_EXISTING_CLASS_ID_NUM) =
     +          BC_EXISTING_ASSET_CLASS_POINTER
     +                                  (1:MAX_BC_EXISTING_CLASS_ID_NUM)
         ENDIF
      RETURN
C
 1000 FORMAT(A)
 1010 FORMAT('&',A)
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C     Purpose:  This module calculates the annual book and tax         C
C               depreciation on existing plant and the annual          C
C               plant retirements.                                     C
C               The module computes the annual totals for gross        C
C               plant value, book depreciation, tax depreciation,      C
C               and retirements.                                       C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE EXASST(EATAXDEP,TOTAL_REGULATED_TAX_DEP,
     +                  ADD_TOTAL_DEF_TAX_RATEBASE,ADD_EA_DEF_TAX,
     +                  TOTAL_EA_ACE_DEP,
     +                  PROPERTY_ESCALATION,
     +                  SAVE_BASE_CASE)
C
      use spindriftlib
      use prod_arrays_dimensions
      USE IREC_ENDPOINT_CONTROL
      USE DRILLING_REPT_PARAMETERS
      use grx_planning_routines
      USE SIZECOM
      use globecom

      INCLUDE 'NAMESCOM.MON'

      CHARACTER(len=30) :: DESC
      INTEGER(kind=2) :: I,IREC,TACCTS,DELETE,ACCOUNTS_REPORTED,
     +          ASSET_CLASS_NUM,ASSET_CLASS_VECTOR,
     +          RUN_YEARS,EXTENSION_YEARS,FINANCIAL_SIMULATION_YEARS
      REAL(kind=4) :: RETIRE_30(AVAIL_DATA_YEARS),TXNORM
      REAL(kind=4) :: EATAXDEP(*),ADD_EA_DEF_TAX(*)
c     REAL :: ADD_EA_TAX_DEP(*)
      REAL(kind=4) :: TOTAL_EA_ACE_DEP(*)
      LOGICAL(kind=1) :: RETPC,TAXES_NORMALIZED,EXISTING_ASSET_REPORT,
     +          EXISTING_ASSETS_REPORT,
     +          TOTAL_ASSET_REPORT,
     +          SAVE_BASE_CASE,
     +          REPORT_ALL_ACCOUNTS
C ADDED 4/13/92 MIDAS GOLD FOR REAL PROPERTY VALUES
      REAL(kind=4) :: PROPERTY_ESCALATION(*),PROPERTY_TAX_VALUE
C     TYPE DECLARATION FOR /FAINPT/
      CHARACTER(len=4) :: DEPMTH
      INTEGER(kind=2) :: SERVYR
      INTEGER ACCTNO,WVPA_SUB_ACCOUNT_NUMBER,
     +        WVPA_ACCOUNTING_UNIT
      CHARACTER(len=12) :: TAX_STATUS
      INTEGER(kind=4) :: IOS
      REAL(kind=4) :: BOKGPV,BOKNPV,BKDPRT,TAXGPV,TXDEP1,TXRATE,DBRATE,
     +     REGULATORY_ALLOCATOR,
     +     ADD_TOTAL_DEF_TAX_RATEBASE(*),
     +     TOTAL_REGULATED_TAX_DEP(*)
C
      REAL(kind=4) :: BOKDP(:,:),TAXDP(:),GPV(:,:),CUM_BOOK_DEP(:,:),
     +       BOKTAX(:),NPV(:,:),RETIRE(:,:)
      ALLOCATABLE :: BOKDP,TAXDP,GPV,BOKTAX,NPV,CUM_BOOK_DEP,RETIRE
      CHARACTER(len=1) :: DATA_TYPE,VECTOR_TYPE*20
      REAL(kind=4) :: VECTOR_VALUES(AVAIL_DATA_YEARS),VECTOR_VALUE
      REAL(kind=4) :: ASSET_DEFERRED_TAXES(:),
     +       ASSET_DEFERRED_TAX_BASIS(:)
      ALLOCATABLE :: ASSET_DEFERRED_TAXES,
     +               ASSET_DEFERRED_TAX_BASIS
C
      LOGICAL(kind=1) :: REPORT_HEADER_OPEN=.FALSE.
      INTEGER(kind=2) :: EA_REPORTING_UNIT,EXISTING_ASSET_RPT_HEADER,
     +          RETIRE_VECTOR,WVPA_ASSET_RPT_HEADER
      INTEGER :: EA_REPORTING_REC
      SAVE EA_REPORTING_UNIT,EA_REPORTING_REC
      REAL(kind=4) :: MONTHLY_RETIRE(12,LAST_AVAILABLE_MONTHLY_YEAR)
c     LOGICAL(kind=1) :: MONTHLY_MIDAS_ACTIVE=.TRUE.
      CHARACTER(len=1)::MONTHLY_DATA_UNITS(LAST_AVAILABLE_MONTHLY_YEAR)
      CHARACTER(len=4)::MIDAS_LAST_MONTH(LAST_AVAILABLE_MONTHLY_YEAR)
      INTEGER(kind=2) :: MONTH_ENDING(LAST_AVAILABLE_MONTHLY_YEAR),YR,
     +          MO,BOOK_DEP_VECTOR
      REAL(kind=4) :: SALVAGE_VALUE,
     +       ADR_LIFE,
     +       TOTAL_BOOK_DEP,
     +       CRET
      INTEGER(kind=4) :: VALUES_2_ZERO
      INTEGER(kind=2) :: WRITE_DRILLING_RPT,VOID_INT2
      CHARACTER(len=1) :: DRILLING_REPRT_LEVEL,FINANCIAL_DRILLING,
     +                    ACCOUNT_ACTIVE
      CHARACTER(len=30) :: DRILLING_NAME
      LOGICAL(kind=1) :: ANNUAL_INFO_ACTIVE,WVPA
      CHARACTER(len=34) :: OUTPUT_OPTION_NAME,LEFT_JUSTIFY_I2_IN_STR*15
      REAL(kind=4) :: TREND_NORM
      REAL(kind=4) :: CUM_RETIREMENTS
      REAL(kind=4) :: TEMP_TREND_NORM
      INTEGER(kind=2) :: TREND_YEAR,
     +          WVPA_COMPANY_ID
      LOGICAL(kind=1) :: LF95,LAHEY_LF95
      CHARACTER(len=6) :: SHORT_MONTH_NAMES
      CHARACTER(len=5) :: PRIMARY_ACCOUNT_NAME
      CHARACTER(len=5) :: OUTPUT_DEPARTMENT_NAME
      CHARACTER(len=1) :: BOOK_DEP_METHOD
      REAL (KIND=4) :: RATE
C
      TACCTS = 0
      ACCOUNTS_REPORTED = 0
      LF95 = LAHEY_LF95()
C
C ACCOUNT BASED REPORTING
C
      EXISTING_ASSET_REPORT =EXISTING_ASSETS_REPORT(REPORT_ALL_ACCOUNTS)
     +                                       .AND.  .NOT. SAVE_BASE_CASE
      REPORT_ALL_ACCOUNTS = 
     +                  EXISTING_ASSET_REPORT  .AND. REPORT_ALL_ACCOUNTS
C
C DRILLING REPORT
C
      IF(EXISTING_ASSET_REPORT) THEN
         DRILLING_REPRT_LEVEL = FINANCIAL_DRILLING()
         EXISTING_ASSET_REPORT = DRILLING_REPRT_LEVEL /= 'O' .AND.
     +                                             EXISTING_ASSET_REPORT
      ENDIF
C     FINANCIAL_SIMULATION_YEARS = RUN_YEARS() + EXTENSION_YEARS() + 1
      FINANCIAL_SIMULATION_YEARS = MAX(LAST_AVAILABLE_MONTHLY_YEAR,
     +                                    RUN_YEARS()+EXTENSION_YEARS())
C
      CALL SET_UP_EA_ARRAYS
C
      CALL INIT_EA_ARRAYS(ADD_TOTAL_DEF_TAX_RATEBASE,
     +                    EATAXDEP,ADD_EA_DEF_TAX,
     +                    TOTAL_EA_ACE_DEP)
C
      ALLOCATE(BOKDP(0:12,0:FINANCIAL_SIMULATION_YEARS))
      ALLOCATE(GPV(0:12,0:FINANCIAL_SIMULATION_YEARS))
      ALLOCATE(CUM_BOOK_DEP(0:12,0:FINANCIAL_SIMULATION_YEARS))
      ALLOCATE(NPV(0:12,0:FINANCIAL_SIMULATION_YEARS))
      ALLOCATE(RETIRE(0:12,0:FINANCIAL_SIMULATION_YEARS))
      ALLOCATE(TAXDP(0:FINANCIAL_SIMULATION_YEARS))
      ALLOCATE(BOKTAX(0:FINANCIAL_SIMULATION_YEARS))
      ALLOCATE(ASSET_DEFERRED_TAXES(0:FINANCIAL_SIMULATION_YEARS))
      ALLOCATE(ASSET_DEFERRED_TAX_BASIS(0:FINANCIAL_SIMULATION_YEARS))
      IREC = 0
      CALL OPEN_EXISTING_ASSET_FILE(INT(11,2))
      IF(LF95) THEN
         WRITE(SCREEN_MESSAGES,"(A)") "Existing Asset Accounts"
         CALL MG_LOCATE_WRITE(18,70,trim(SCREEN_MESSAGES),3,2)
      ENDIF
C
      DO
         GPV = 0.
         CUM_BOOK_DEP = 0.
         NPV = 0.
         RETIRE = 0.
         TAXDP = 0.
         BOKTAX = 0.
         ASSET_DEFERRED_TAXES = 0.
         ASSET_DEFERRED_TAX_BASIS = 0.
         MONTHLY_RETIRE = 0.
         IREC = IREC + 1
         READ(11,REC=IREC,IOSTAT=IOS) DELETE,ACCTNO,BOKGPV,BOKNPV,
     +                                BKDPRT,REGULATORY_ALLOCATOR,
     +                                TAXGPV,TXDEP1,SERVYR,TXRATE,
     +                                DEPMTH,DBRATE,TXNORM,
     +                                RETIRE_30,
     +                                DESC,SALVAGE_VALUE,
     +                                PROPERTY_TAX_VALUE,
     +                                ASSET_CLASS_NUM,
     +                                ASSET_CLASS_VECTOR,
     +                                RETIRE_VECTOR,
     +                                ADR_LIFE,
     +                                TOTAL_BOOK_DEP,
     +                                BOOK_DEP_VECTOR,
     +                                ACCOUNT_ACTIVE,
     +                                WVPA_ACCOUNTING_UNIT,
     +                                WVPA_SUB_ACCOUNT_NUMBER,
     +                                BOOK_DEP_METHOD,
     +                                WVPA_COMPANY_ID
         IF(IOS /= 0) EXIT
         TACCTS = TACCTS + 1
         IF(LF95) THEN
            WRITE(SCREEN_MESSAGES,"(I4,A)") TACCTS,"-"//DESC
            CALL MG_LOCATE_WRITE(15,70,trim(SCREEN_MESSAGES),3,0)
         ELSE
            WRITE(SCREEN_MESSAGES,"(I4)") TACCTS
            CALL MG_LOCATE_WRITE(15,70,trim(SCREEN_MESSAGES),
     +                                                   ALL_VERSIONS,0)
         ENDIF
         IF(DELETE > 7 .OR. ACCOUNT_ACTIVE == 'N') CYCLE
C
         REGULATORY_ALLOCATOR = REGULATORY_ALLOCATOR/100.
         IF(TOTAL_BOOK_DEP == 0.) THEN
            TOTAL_BOOK_DEP = BOKGPV - BOKNPV
         ELSE
            BOKNPV = BOKGPV - TOTAL_BOOK_DEP
         ENDIF
         DO MO = 0, 12
            GPV(MO,0) = BOKGPV
            NPV(MO,0) = BOKNPV
            CUM_BOOK_DEP(MO,0) = TOTAL_BOOK_DEP
         ENDDO
         RETPC = .FALSE.
         IF(RETIRE_VECTOR /= 0) THEN
            RETIRE_VECTOR = ABS(RETIRE_VECTOR)
            CALL GET_MONTHLY_ANNUAL_VALUES(RETIRE_VECTOR,
     +                                     DATA_TYPE,
     +                                     VECTOR_TYPE,
     +                                     RETIRE_30,
     +                                     MONTHLY_RETIRE(1,1),
     +                                     MONTHLY_DATA_UNITS,
     +                                     MONTH_ENDING)
            IF(RETIRE_30(1) < 0.0 ) THEN
               CRET = 0.0
               RATE = ABS(RETIRE_30(1))/100.
               DO YR = 1, FINANCIAL_SIMULATION_YEARS
                  RETIRE(0,YR) = RATE * (BOKGPV - CRET)
                  CRET = CRET + RETIRE(0,YR)
                  IF(YR <= AVAIL_DATA_YEARS) THEN
                     RETIRE_30(YR) = RETIRE(0,YR)
                  ENDIF
               ENDDO
            ENDIF
            IF(MONTHLY_MIDAS_ACTIVE) THEN
            
               CALL RIPPLE_MONTHLY_DATA(RETIRE_30,MONTHLY_RETIRE)
C
               CALL MONTHLY_BOOK_VALUES_IN_DOLLARS(RETIRE_30,
     +                                             MONTHLY_RETIRE,
     +                                             MONTHLY_DATA_UNITS,
     +                                             MONTH_ENDING)
     
               TREND_NORM = 1.
               TREND_YEAR = 1
               DO YR = 1, FINANCIAL_SIMULATION_YEARS
                  TEMP_TREND_NORM = 0.
                  IF(YR <= AVAIL_DATA_YEARS) THEN
                     RETIRE(0,YR) = RETIRE_30(YR)
                  ELSE   
                     RETIRE(0,YR) = RETIRE_30(AVAIL_DATA_YEARS)
                  ENDIF
                  DO MO = 1, 12
                     IF(YR <= LAST_AVAILABLE_MONTHLY_YEAR) THEN
                        RETIRE(MO,YR) = MONTHLY_RETIRE(MO,YR)
                     ELSE
                        RETIRE(MO,YR) = RETIRE(0,YR) *
     +                                   MONTHLY_RETIRE(MO,TREND_YEAR)/
     +                                               TREND_NORM
                     ENDIF
                     IF(YR <= LAST_AVAILABLE_MONTHLY_YEAR) THEN
                        TEMP_TREND_NORM = TEMP_TREND_NORM
     +                                    + MONTHLY_RETIRE(MO,YR)
                        IF(TEMP_TREND_NORM /= 0.) THEN
                           TREND_YEAR = YR
                           TREND_NORM = TEMP_TREND_NORM
                        ENDIF
                     ENDIF
                  ENDDO
               ENDDO
            ELSE
               DO YR = 1, FINANCIAL_SIMULATION_YEARS
                  IF(YR <= AVAIL_DATA_YEARS) THEN
                     RETIRE(6,YR) = RETIRE_30(YR)
                     RETIRE(0,YR) = RETIRE_30(YR)
                  ELSE   
                     RETIRE(6,YR) = RETIRE_30(AVAIL_DATA_YEARS)
                     RETIRE(0,YR) = RETIRE_30(AVAIL_DATA_YEARS)
                  ENDIF
               ENDDO
            ENDIF
         ELSE
            IF(RETIRE_30(1) < 0.0) THEN
               CRET = 0.0
               RATE = ABS(RETIRE_30(1))/100.
               DO YR = 1, FINANCIAL_SIMULATION_YEARS
                  RETIRE(0,YR) = RATE * (BOKGPV - CRET)
                  CRET = CRET + RETIRE(0,YR)
                  RETIRE(6,YR) = RETIRE(0,YR)
               ENDDO
            ELSE
               DO YR = 1, FINANCIAL_SIMULATION_YEARS
                  IF(YR <= AVAIL_DATA_YEARS) THEN
                     RETIRE(6,YR) = RETIRE_30(YR)
                     RETIRE(0,YR) = RETIRE_30(YR)
                  ELSE   
                     RETIRE(6,YR) = RETIRE_30(AVAIL_DATA_YEARS)
                     RETIRE(0,YR) = RETIRE_30(AVAIL_DATA_YEARS)
                  ENDIF
               ENDDO
            ENDIF
         ENDIF
C
C CALCULATE CHANGE IN GPV DUE BY RETIREMENTS
C
         CUM_RETIREMENTS = 0.
         GPV(12,0) = BOKGPV
         GPV(:,0) = BOKGPV
         DO YR = 1, FINANCIAL_SIMULATION_YEARS
            GPV(0,YR) = GPV(12,YR-1)
            DO MO = 1, 12
               IF(ABS(RETIRE(MO,YR)) >= ABS(GPV(MO-1,YR))) THEN
                  GPV(MO,YR) = 0.
                  RETIRE(MO,YR) = GPV(MO-1,YR) ! MAX(GPV(MO-1,YR),0.)
               ELSE
                  GPV(MO,YR) = GPV(MO-1,YR) - RETIRE(MO,YR)
               ENDIF
               CUM_RETIREMENTS = CUM_RETIREMENTS + RETIRE(MO,YR)
            ENDDO
         ENDDO
C
C     CALCULATE BOOK DEPRECIATION
C
         CALL EDPBOK(BOKDP,GPV,CUM_BOOK_DEP,TOTAL_BOOK_DEP,BKDPRT,
     +               SALVAGE_VALUE,FINANCIAL_SIMULATION_YEARS,
     +               BOOK_DEP_VECTOR)
C
         IF(RETIRE_VECTOR /= 0 .OR. CUM_RETIREMENTS /= 0.) THEN
            CUM_RETIREMENTS = 0.
            DO YR = 1, FINANCIAL_SIMULATION_YEARS
               CUM_BOOK_DEP(0,YR) = CUM_BOOK_DEP(12,YR-1)
               DO MO = 1, 12
                  CUM_RETIREMENTS = CUM_RETIREMENTS + RETIRE(MO,YR)
                  CUM_BOOK_DEP(MO,YR) = CUM_BOOK_DEP(MO,YR) -
     +                                                   CUM_RETIREMENTS
               ENDDO
            ENDDO
         ENDIF
C
C     CALCULATE TAX EXPENSES
C
         IF(TAXGPV-TXDEP1 > 0.)  THEN
            IF(INDEX(DEPMTH,'CCA4') /= 0) THEN
               TAXDP(0) = 0.
               IF(TXDEP1 > .74*TAXGPV) THEN
                  TAXDP(1) = TAXGPV - TXDEP1
                  TAXDP(2) = 0.
               ELSE
                  TAXDP(1) = 2.*(TAXGPV - TXDEP1)/3.
                  TAXDP(2) = TAXGPV - TXDEP1 - TAXDP(2)
               ENDIF
               DO YR = 3, FINANCIAL_SIMULATION_YEARS
                  TAXDP(YR)  = 0.0
               ENDDO
            ELSE
               CALL EDPTAX(SERVYR,TAXDP,TXRATE,TAXGPV,
     +                     TXDEP1,DEPMTH,DBRATE,
     +                     FINANCIAL_SIMULATION_YEARS)
            ENDIF
         ENDIF
C
C CALCULATE TAX STRAGHIT LINE DEPRECIATION FOR DEFERRED TAXES
C
         TAXES_NORMALIZED = .NOT. (TXNORM == 0. .OR. 
     +              (trim(DEPMTH) == 'SL' .AND. TXNORM == TXRATE))
         IF(TAXES_NORMALIZED) THEN
            IF(TXNORM < 0.) THEN
               CALL GET_ASSET_VAR_TYPE(INT(TXNORM,2),DATA_TYPE,
     +                                        VECTOR_VALUES,VECTOR_TYPE)
               IF(INDEX(VECTOR_TYPE,'Basis') /= 0.) THEN
                  DO I = 1, FINANCIAL_SIMULATION_YEARS 
                     IF(I < AVAIL_DATA_YEARS) THEN
                        VECTOR_VALUE = VECTOR_VALUES(I) 
                     ELSEIF(I == AVAIL_DATA_YEARS) THEN
                        RATE = VECTOR_VALUES(I-1)
                        CALL IMPLIED_ESC(RATE,VECTOR_VALUES(I))
                        VECTOR_VALUE = VECTOR_VALUES(I) 
                     ELSE
C                       VECTOR_VALUE = VECTOR_VALUES(AVAIL_DATA_YEARS) ! MODIFIED TO ESCALATE 11/21/06 DR.G
                        VECTOR_VALUE = RATE * VECTOR_VALUE 
                     ENDIF
                     ASSET_DEFERRED_TAX_BASIS(I) = VECTOR_VALUE
                     BOKTAX(I) = TAXDP(I) - VECTOR_VALUE
                     ASSET_DEFERRED_TAXES(I) = 0.
                  ENDDO
               ELSEIF(INDEX(VECTOR_TYPE,'Deferred') /= 0.) THEN
                  DO I = 1, FINANCIAL_SIMULATION_YEARS
                     IF(I < AVAIL_DATA_YEARS) THEN
                        VECTOR_VALUE = VECTOR_VALUES(I) 
                     ELSEIF(I == AVAIL_DATA_YEARS) THEN
                        RATE = VECTOR_VALUES(I-1)
                        CALL IMPLIED_ESC(RATE,VECTOR_VALUES(I))
                        VECTOR_VALUE = VECTOR_VALUES(I) 
                     ELSE
C                       VECTOR_VALUE = VECTOR_VALUES(AVAIL_DATA_YEARS) ! MODIFIED TO ESCALATE 11/21/06 DR.G
                        VECTOR_VALUE = RATE * VECTOR_VALUE 
                     ENDIF
                     ASSET_DEFERRED_TAXES(I) = VECTOR_VALUE
                     BOKTAX(I) = TAXDP(I) - VECTOR_VALUE/.33
                     ASSET_DEFERRED_TAX_BASIS(I) = 0.
                  ENDDO
               ELSE   
                  CALL ETXBOK(BOKTAX,SERVYR,TAXGPV,TXNORM,
     +                        FINANCIAL_SIMULATION_YEARS)
                  DO I = 1, FINANCIAL_SIMULATION_YEARS
                     ASSET_DEFERRED_TAX_BASIS(I) = TAXDP(I) - BOKTAX(I)
                     ASSET_DEFERRED_TAXES(I) = 0.
                  ENDDO
               ENDIF
            ELSE
               CALL ETXBOK(BOKTAX,SERVYR,TAXGPV,TXNORM,
     +                     FINANCIAL_SIMULATION_YEARS)
               DO I = 1, FINANCIAL_SIMULATION_YEARS
                  ASSET_DEFERRED_TAX_BASIS(I) = TAXDP(I) - BOKTAX(I)
                  ASSET_DEFERRED_TAXES(I) = 0.
               ENDDO
            ENDIF
         ELSE
            DO I = 1, FINANCIAL_SIMULATION_YEARS
               BOKTAX(I) = TAXDP(I)
               ASSET_DEFERRED_TAX_BASIS(I) = 0.
               ASSET_DEFERRED_TAXES(I) = 0.
            ENDDO
         ENDIF
C
C     ACCUMLULATE THE BOOK AND TAX DEPRECIATIONS, THE RETIREMENTS,
C     AND THE EXISTING GROSS PLANT VALUE FOR BOOK AND TAX
C     FOR TOTAL COMPANY
C
         CALL TOTAL(RETIRE,BOKDP,TAXDP,BOKGPV,GPV,BOKTAX,NPV,
     +            BOKNPV,REGULATORY_ALLOCATOR,TAXES_NORMALIZED,
     +            ASSET_CLASS_NUM,ASSET_CLASS_VECTOR,
     +            PROPERTY_ESCALATION,PROPERTY_TAX_VALUE,
     +            ASSET_DEFERRED_TAX_BASIS,ASSET_DEFERRED_TAXES,
     +            CUM_BOOK_DEP)
C
         IF(EXISTING_ASSET_REPORT .AND. DELETE > 1 .OR.
     +                                         REPORT_ALL_ACCOUNTS) THEN
            ACCOUNTS_REPORTED = ACCOUNTS_REPORTED + 1
            IF(IREC < 10) THEN
               OUTPUT_OPTION_NAME = DESC//'   '//
     +                                      LEFT_JUSTIFY_I2_IN_STR(IREC)
            ELSEIF(IREC < 100) THEN
               OUTPUT_OPTION_NAME = DESC//'  '//
     +                                      LEFT_JUSTIFY_I2_IN_STR(IREC)
            ELSEIF(IREC < 1000) THEN
               OUTPUT_OPTION_NAME = DESC//' '//
     +                                      LEFT_JUSTIFY_I2_IN_STR(IREC)
            ELSEIF(IREC < 10000) THEN
               OUTPUT_OPTION_NAME=DESC//LEFT_JUSTIFY_I2_IN_STR(IREC)
            ENDIF
            IF(WVPA()) THEN
               DRILLING_ACCOUNT_NAME = WVPA_REPORTING_NAME(DESC,
     +                                          ACCTNO,
     +                                          WVPA_ACCOUNTING_UNIT,
     +                                          WVPA_SUB_ACCOUNT_NUMBER)
            ELSE
               DRILLING_ACCOUNT_NAME = OUTPUT_OPTION_NAME
            ENDIF
C
            ANNUAL_INFO_ACTIVE = DRILLING_REPRT_LEVEL == 'A'
            DO YR = 1, FINANCIAL_SIMULATION_YEARS
               DO MO = 0, 12
                  NPV(MO,YR) = GPV(MO,YR) - CUM_BOOK_DEP(MO,YR)
               ENDDO
            ENDDO
           IF(FINANCIAL_DRILLING() /= 'O') THEN  
            IF(RETIRE_VECTOR /= 0 .OR. CUM_RETIREMENTS /= 0.) THEN
               DRILLING_NAME = 'Retirements'
               VOID_INT2 = WRITE_DRILLING_RPT(DRILLING_NAME,
     +                                        DRILLING_ACCOUNT_NAME,
     +                                        RETIRE,
     +                                        BALANCE_SHEET_ANNUAL_ITEM,
     +                                        ANNUAL_INFO_ACTIVE)
            ENDIF
            DRILLING_NAME = 'GPV'
            VOID_INT2 = WRITE_DRILLING_RPT(DRILLING_NAME,
     +                                     DRILLING_ACCOUNT_NAME,
     +                                     GPV,
     +                                     BALANCE_SHEET_ITEM,
     +                                     ANNUAL_INFO_ACTIVE)
            DRILLING_NAME = 'Accumulated Depreciation'
            VOID_INT2 = WRITE_DRILLING_RPT(DRILLING_NAME,
     +                                     DRILLING_ACCOUNT_NAME,
     +                                     CUM_BOOK_DEP,
     +                                     BALANCE_SHEET_ITEM,
     +                                     ANNUAL_INFO_ACTIVE)
            DRILLING_NAME = 'NPV'
            VOID_INT2 = WRITE_DRILLING_RPT(DRILLING_NAME,
     +                                     DRILLING_ACCOUNT_NAME,
     +                                     NPV,
     +                                     BALANCE_SHEET_ITEM,
     +                                     ANNUAL_INFO_ACTIVE)
            DRILLING_NAME = 'Book Depreciation'
            VOID_INT2 = WRITE_DRILLING_RPT(DRILLING_NAME,
     +                                     DRILLING_ACCOUNT_NAME,
     +                                     BOKDP,
     +                                     INCOME_REPORT_ITEM,
     +                                     ANNUAL_INFO_ACTIVE)
         ENDIF
c           
c direct account reporting
c
c$ifdefined(good_code)
            IF((EXISTING_ASSET_REPORT .OR. REPORT_ALL_ACCOUNTS) .AND.
     +                                    .NOT. REPORT_HEADER_OPEN) THEN
               IF(WVPA()) THEN
                  EA_REPORTING_UNIT = WVPA_ASSET_RPT_HEADER(
     +                                                 EA_REPORTING_REC)
               ELSE   
                  EA_REPORTING_UNIT = EXISTING_ASSET_RPT_HEADER(
     +                                                 EA_REPORTING_REC)
               ENDIF                  
               REPORT_HEADER_OPEN = .TRUE.
            ENDIF
C
            IF(WVPA()) THEN
               WRITE(PRIMARY_ACCOUNT_NAME,'(I5)') ACCTNO
               WRITE(OUTPUT_DEPARTMENT_NAME,'(A,I3.3)') 'D',
     +                                                   ASSET_CLASS_NUM

               DO I = 1, FINANCIAL_SIMULATION_YEARS
                  DO MO = 0, 12
                     NPV(MO,I) = GPV(MO,I) - CUM_BOOK_DEP(MO,I)
                     WRITE(EA_REPORTING_UNIT,REC=EA_REPORTING_REC)
     +                                        PRT_ENDPOINT(),
     +                                        FLOAT(I+BASE_YEAR),
     +                                        OUTPUT_DEPARTMENT_NAME,
     +                                        PRIMARY_ACCOUNT_NAME//' ',
     +                                        OUTPUT_OPTION_NAME,
     +                                        SHORT_MONTH_NAMES(MO),
     +                                        GPV(MO,I),
     +                                        CUM_BOOK_DEP(MO,I),
     +                                        NPV(MO,I),
     +                                        BOKDP(MO,I),
     +                                        RETIRE(MO,I)
                     EA_REPORTING_REC = EA_REPORTING_REC + 1
                  ENDDO
               ENDDO
               CALL WVPA_EA_PLANT_DATA_BASE(ACCTNO,
     +                                      WVPA_COMPANY_ID,
     +                                      GPV,
     +                                      CUM_BOOK_DEP,
     +                                      NPV,
     +                                      BOKDP,
     +                                      RETIRE)
            ELSE
               WRITE(EA_REPORTING_UNIT,REC=EA_REPORTING_REC)
     +                                     PRT_ENDPOINT(),
     +                                     OUTPUT_OPTION_NAME,
     +                                     FLOAT(BASE_YEAR),
     +                                     GPV(0,1),
     +                                     GPV(0,1)-NPV(0,1),
     +                                     NPV(0,1)
               EA_REPORTING_REC = EA_REPORTING_REC + 1
               DO I = 1, FINANCIAL_SIMULATION_YEARS
                  WRITE(EA_REPORTING_UNIT,REC=EA_REPORTING_REC)
     +                                     PRT_ENDPOINT(),
     +                                     OUTPUT_OPTION_NAME,
     +                                     FLOAT(I+BASE_YEAR),
     +                                     GPV(12,I),
     +                                     GPV(12,I)-NPV(12,I),
     +                                     NPV(12,I),
     +                                     BOKDP(0,I),
     +                                     RETIRE(0,I),
     +                                     TAXDP(I),
     +                                     BOKTAX(I),
     +                                     ASSET_DEFERRED_TAX_BASIS(I),
     +                                     ASSET_DEFERRED_TAXES(I)
                  EA_REPORTING_REC = EA_REPORTING_REC + 1
               ENDDO
            ENDIF
c$endif
         ENDIF
      ENDDO
c      CALL CLOSE_EXISTING_ASSET_FILE
      CLOSE(11)
      IF(LF95 .AND. TACCTS > 0) THEN
         WRITE(SCREEN_MESSAGES,'(I4,A)')
     +                                 TACCTS,'-Existing asset accounts'
         CALL MG_LOCATE_WRITE(0,0,trim(SCREEN_MESSAGES),
     +                                                   ALL_VERSIONS,1)
      ENDIF
      TOTAL_ASSET_REPORT = EXISTING_ASSET_REPORT .AND.
     +                                            ACCOUNTS_REPORTED /= 1
      IF(SAVE_BASE_CASE) CALL WRITE_EA_BASE_CASE
      DEALLOCATE(BOKDP,TAXDP,GPV,BOKTAX,NPV,CUM_BOOK_DEP,
     +           RETIRE,ASSET_DEFERRED_TAXES,ASSET_DEFERRED_TAX_BASIS)
      RETURN
      END
C***********************************************************************
C*                                                                     *
C*                           E D P B O K                               *
C*                                                                     *
C*          COPYRIGHT (C) 1982   M.S. GERBER & ASSOCIATES, INC         *
C*                       ALL RIGHTS RESERVED                           *
C*                                                                     *
C***********************************************************************
C                                                                      *
C     PURPOSE:                                                         *
C        EDPBOK CALCULATES THE BOOK DEPRECIATION FOR FUTURE            *
C        PLANT ENTERING SERVICE USING STRAIGHT LINE WITH               *
C        HALF YEAR CONVENTION                                          *
C                                                                      *
C***********************************************************************
C
      SUBROUTINE EDPBOK(BOOK_DEP,GPV,CUM_BOOK_DEP,TOTAL_BOOK_DEP,BKDPRT,
     +                  SALVAGE_VALUE,FINANCIAL_SIMULATION_YEARS,
     +                  BOOK_DEP_VECTOR)
C
      use spindriftlib
      use prod_arrays_dimensions
      USE SIZECOM
      use globecom

      CHARACTER(len=1) :: DATA_TYPE,VECTOR_TYPE*20,DATA_TYPE2
      INTEGER(kind=2) :: FINANCIAL_SIMULATION_YEARS,MO,YR,
     +          BOOK_DEP_VECTOR,BOOK_RATE_VECTOR
      LOGICAL(kind=1) :: GROUP_BOOK_DEPRECIATION
      REAL(kind=4) :: BOOK_DEP(0:12,0:FINANCIAL_SIMULATION_YEARS),
     +       GPV(0:12,0:FINANCIAL_SIMULATION_YEARS),
     +       CUM_BOOK_DEP(0:12,0:FINANCIAL_SIMULATION_YEARS),
     +       TOTAL_BOOK_DEP
      REAL(kind=4) :: BKDPRT,ANNUAL_BOOK_DEP,
     +       NPVFB,DEPSL,SALVAGE_VALUE
      INTEGER(kind=4) :: VALUES_2_SET
      LOGICAL(kind=1) :: USE_BOOK_RATE,BOOK_DEP_FINISHED
      INTEGER(kind=2) :: VECTOR
      REAL(kind=4) :: 
     +          MONTHLY_BOOK_DEP_VECTOR(12,LAST_AVAILABLE_MONTHLY_YEAR)
      REAL(kind=4) :: ANNUAL_BOOK_DEP_30(AVAIL_DATA_YEARS),TREND_NORM
      CHARACTER(len=1)::MONTHLY_DATA_UNITS(LAST_AVAILABLE_MONTHLY_YEAR)
      INTEGER(kind=2) :: MONTH_ENDING(LAST_AVAILABLE_MONTHLY_YEAR)
      REAL(kind=4) :: MONTHLY_BOOK_DEP_VECTOR_ADDER(:,:)
      ALLOCATABLE :: MONTHLY_BOOK_DEP_VECTOR_ADDER
      REAL(kind=4) :: VEC_DOLLAR_ADDER,VEC_RATE_ADDER
      INTEGER(kind=2) :: TREND_YR
      REAL(kind=4) :: ANNUAL_VECTOR_DEP
      INTEGER(kind=2) :: I_YEAR,I_MONTH,REMAINING_MONTHS
C
C     INITIALIZE VARIABLES
C
C NOTE: FOR CUMULATIVE VALUES THE 0 MONTH POSITION IS BEGINNING AMOUNT
C       FOR ANNUAL VALUES THE 0 MONTH POSITION CONTAINS THE ANNUAL TOTAL
C
      BOOK_DEP = 0.
C
C IF THE BOOK DEPRECIATION PERCENTAGE IS ZERO
C DEPRECIATION IS NOT TAKEN.
C
      IF(BKDPRT == 0. .AND. BOOK_DEP_VECTOR == 0) THEN
         CUM_BOOK_DEP = TOTAL_BOOK_DEP
         RETURN
      ENDIF
C
C IF THE BOOK RATE IS GREATER THAN 100% THE ASSUMPTION IS THAT
C THE TOTAL AMOUNT IS EXPENSED IN ONE YEAR
C
      IF(BKDPRT > 99.999 .AND. BKDPRT < 1990.) THEN
         BOOK_DEP(0,1) = GPV(12,0) - TOTAL_BOOK_DEP
         BOOK_DEP(1,1) = BOOK_DEP(0,1)
         CUM_BOOK_DEP =  GPV(12,0)
         CUM_BOOK_DEP(:,0) = TOTAL_BOOK_DEP
         CUM_BOOK_DEP(0,1) = TOTAL_BOOK_DEP
         RETURN
      ENDIF
C
C INITIALIZE THE BOOK DEP VALUES IF DEP VECTOR IS USED
C
      DATA_TYPE2 = 'X'
      ALLOCATE(MONTHLY_BOOK_DEP_VECTOR_ADDER(0:12,
     +                                    0:FINANCIAL_SIMULATION_YEARS))
      MONTHLY_BOOK_DEP_VECTOR_ADDER = 0.
      IF(BOOK_DEP_VECTOR /= 0) THEN
         VECTOR = ABS(BOOK_DEP_VECTOR)
         CALL GET_MONTHLY_ANNUAL_VALUES(VECTOR,
     +                                  DATA_TYPE2,
     +                                  VECTOR_TYPE,
     +                                  ANNUAL_BOOK_DEP_30,
     +                                  MONTHLY_BOOK_DEP_VECTOR,
     +                                  MONTHLY_DATA_UNITS,
     +                                  MONTH_ENDING)
         CALL RIPPLE_MONTHLY_DATA(ANNUAL_BOOK_DEP_30,
     +                            MONTHLY_BOOK_DEP_VECTOR)
         CALL MONTHLY_BOOK_VALUES_IN_DOLLARS(ANNUAL_BOOK_DEP_30,
     +                                       MONTHLY_BOOK_DEP_VECTOR,
     +                                       MONTHLY_DATA_UNITS,
     +                                       MONTH_ENDING)
         TREND_NORM = 0.
         TREND_YR = 1
         DO YR = 1, FINANCIAL_SIMULATION_YEARS
            DO MO = 1, 12 
               IF(YR <= LAST_AVAILABLE_MONTHLY_YEAR) THEN
                  IF(DATA_TYPE2 == 'D') THEN
                     MONTHLY_BOOK_DEP_VECTOR_ADDER(MO,YR) =
     +                                    MONTHLY_BOOK_DEP_VECTOR(MO,YR)
                  ELSEIF(DATA_TYPE2 == 'R') THEN
                     MONTHLY_BOOK_DEP_VECTOR_ADDER(MO,YR) =
     +                                    MONTHLY_BOOK_DEP_VECTOR(MO,YR)
                  ELSEIF(DATA_TYPE2 == 'P') THEN
                     MONTHLY_BOOK_DEP_VECTOR_ADDER(MO,YR) =
     +                               MONTHLY_BOOK_DEP_VECTOR(MO,YR)/100.
                  ELSE
                     MONTHLY_BOOK_DEP_VECTOR_ADDER(MO,YR) =
     +                                        ANNUAL_BOOK_DEP_30(YR)/12.
                  ENDIF
                  IF(ANNUAL_BOOK_DEP_30(YR) /= 0) THEN
                     IF(MO == 1) TREND_NORM = 0.
                     TREND_YR = YR
                     TREND_NORM = TREND_NORM +
     +                              MONTHLY_BOOK_DEP_VECTOR_ADDER(MO,YR)
                  ENDIF
               ELSEIF(YR <= AVAIL_DATA_YEARS) THEN
                  IF(TREND_NORM /= 0.) THEN
                     MONTHLY_BOOK_DEP_VECTOR_ADDER(MO,YR) =
     +                      ANNUAL_BOOK_DEP_30(YR) *
     +                         MONTHLY_BOOK_DEP_VECTOR(MO,TREND_YR)/
     +                                       TREND_NORM
                   ELSE
                     MONTHLY_BOOK_DEP_VECTOR_ADDER(MO,YR) =
     +                                        ANNUAL_BOOK_DEP_30(YR)/12.
                   ENDIF
               ELSEIF(YR > AVAIL_DATA_YEARS) THEN
                  IF(TREND_NORM /= 0.) THEN
                     MONTHLY_BOOK_DEP_VECTOR_ADDER(MO,YR) =
     +                      ANNUAL_BOOK_DEP_30(AVAIL_DATA_YEARS) *
     +                         MONTHLY_BOOK_DEP_VECTOR(MO,TREND_YR)/
     +                                    TREND_NORM
                   ELSE
                     MONTHLY_BOOK_DEP_VECTOR_ADDER(MO,YR) =
     +                                        ANNUAL_BOOK_DEP_30(YR)/12.
                   ENDIF
               ENDIF
            ENDDO
         ENDDO
C
      ENDIF
      IF(SALVAGE_VALUE <= -99999.) THEN
         GROUP_BOOK_DEPRECIATION = .TRUE.
         NPVFB = GPV(12,0) - TOTAL_BOOK_DEP
      ELSE
         GROUP_BOOK_DEPRECIATION = .FALSE.
         NPVFB = GPV(12,0) - TOTAL_BOOK_DEP - SALVAGE_VALUE
      ENDIF
      CUM_BOOK_DEP(:,0) = TOTAL_BOOK_DEP
      BOOK_RATE_VECTOR = BKDPRT
      IF(BKDPRT > 0. .OR. BOOK_DEP_VECTOR /= 0) THEN
         IF(BKDPRT < 0.) BKDPRT = 0.
         USE_BOOK_RATE = .FALSE.
         IF(BKDPRT > BASE_YEAR) THEN ! REMAINING LIFE/YEARS LEFT
            I_YEAR = BKDPRT
            I_MONTH = NINT((BKDPRT-FLOAT(I_YEAR)) * 100.)
            IF(I_MONTH == 0) I_MONTH = 12
            REMAINING_MONTHS = (I_YEAR - BASE_YEAR-1)*12. + I_MONTH
            DEPSL = NPVFB/FLOAT(REMAINING_MONTHS)
         ELSEIF(BKDPRT >= 1990) THEN ! DUMP IN THE FIRST YEAR
            DEPSL = NPVFB/12.
         ELSE ! IT'S A RATE  
            BKDPRT = BKDPRT/1200.
            USE_BOOK_RATE = .TRUE.
         ENDIF
C
C     CALCULATE BOOK DEPRICATION ON BRICKS AND MORTOR
C
         BOOK_DEP_FINISHED = .FALSE.
         DO YR = 1, FINANCIAL_SIMULATION_YEARS
            CUM_BOOK_DEP(0,YR) = CUM_BOOK_DEP(12,YR-1)
            ANNUAL_BOOK_DEP = 0
            DO MO = 1, 12
               IF(DATA_TYPE2 == 'R' .OR. DATA_TYPE2 == 'P') THEN
                  VEC_RATE_ADDER=MONTHLY_BOOK_DEP_VECTOR_ADDER(MO,YR)
                  VEC_DOLLAR_ADDER = 0.
               ELSE
                  VEC_DOLLAR_ADDER=MONTHLY_BOOK_DEP_VECTOR_ADDER(MO,YR) 
                  VEC_RATE_ADDER = 0.
               ENDIF
               IF(BOOK_DEP_FINISHED) THEN
                  CUM_BOOK_DEP(MO,YR) = CUM_BOOK_DEP(MO-1,YR)
                  CYCLE
               ENDIF
               IF(USE_BOOK_RATE) THEN
                  DEPSL = GPV(MO-1,YR) * (BKDPRT + VEC_RATE_ADDER)
               ENDIF
               DEPSL = DEPSL + VEC_DOLLAR_ADDER
               IF(ABS(NPVFB) <= ABS(DEPSL) .AND.
     +                               .NOT. GROUP_BOOK_DEPRECIATION) THEN
                  BOOK_DEP(MO,YR) = NPVFB
                  ANNUAL_BOOK_DEP = ANNUAL_BOOK_DEP + NPVFB
                  CUM_BOOK_DEP(MO,YR) = CUM_BOOK_DEP(MO-1,YR) + NPVFB
                  BOOK_DEP_FINISHED = .TRUE.
               ELSE
                  BOOK_DEP(MO,YR) = DEPSL
                  NPVFB = NPVFB - DEPSL
                  ANNUAL_BOOK_DEP = ANNUAL_BOOK_DEP + DEPSL
                  CUM_BOOK_DEP(MO,YR) = CUM_BOOK_DEP(MO-1,YR) + DEPSL
               ENDIF
            ENDDO
            BOOK_DEP(0,YR) = ANNUAL_BOOK_DEP
         ENDDO
      ENDIF
      IF(BOOK_RATE_VECTOR < 0.) THEN 
         VECTOR = ABS(BOOK_RATE_VECTOR)
C        CALL BOOK_DEP_USING_VECTOR(VECTOR,GPV,BOOK_DEP,CUM_BOOK_DEP)
C        CALL GET_ASSET_VAR(IVEC,DATA_TYPE,BKDEPV)
C
         CALL GET_MONTHLY_ANNUAL_VALUES(VECTOR,
     +                                  DATA_TYPE,
     +                                  VECTOR_TYPE,
     +                                  ANNUAL_BOOK_DEP_30,
     +                                  MONTHLY_BOOK_DEP_VECTOR,
     +                                  MONTHLY_DATA_UNITS,
     +                                  MONTH_ENDING)
         CALL RIPPLE_MONTHLY_DATA(ANNUAL_BOOK_DEP_30,
     +                            MONTHLY_BOOK_DEP_VECTOR)
c        CALL MAP_LAST_MONTH(MIDAS_LAST_MONTH,MONTH_ENDING)
         CALL MONTHLY_BOOK_VALUES_IN_DOLLARS(ANNUAL_BOOK_DEP_30,
     +                                         MONTHLY_BOOK_DEP_VECTOR,
     +                                         MONTHLY_DATA_UNITS,
     +                                         MONTH_ENDING)
C
         TREND_NORM = 0.
         BOOK_DEP_FINISHED = .FALSE.
         DO YR = 1, FINANCIAL_SIMULATION_YEARS
            CUM_BOOK_DEP(0,YR) = CUM_BOOK_DEP(12,YR-1)
            ANNUAL_BOOK_DEP = 0
            ANNUAL_VECTOR_DEP = 0.
            DO MO = 1, 12 
               IF(BOOK_DEP_FINISHED) THEN
                  CUM_BOOK_DEP(MO,YR) = CUM_BOOK_DEP(MO-1,YR)
                  CYCLE
               ENDIF
               IF(YR <= LAST_AVAILABLE_MONTHLY_YEAR) THEN
                  IF(DATA_TYPE == 'D') THEN
                     DEPSL = MONTHLY_BOOK_DEP_VECTOR(MO,YR)
                  ELSEIF(DATA_TYPE == 'R') THEN
                     DEPSL = GPV(MO-1,YR)*MONTHLY_BOOK_DEP_VECTOR(MO,YR)
                  ELSEIF(DATA_TYPE == 'P') THEN
                     DEPSL = GPV(MO-1,YR) *
     +                               MONTHLY_BOOK_DEP_VECTOR(MO,YR)/100.
                  ELSE
                     DEPSL = ANNUAL_BOOK_DEP_30(YR)/12.
                  ENDIF
                  MONTHLY_BOOK_DEP_VECTOR(MO,YR) = DEPSL
                  ANNUAL_VECTOR_DEP = ANNUAL_VECTOR_DEP + DEPSL
               ELSE
                  YEAR = YR
                  IF(YR > AVAIL_DATA_YEARS) YEAR = AVAIL_DATA_YEARS
                  IF(DATA_TYPE == 'D') THEN
                     IF(TREND_NORM /= 0.) THEN
                        DEPSL = ANNUAL_BOOK_DEP_30(YEAR) *
     +                             MONTHLY_BOOK_DEP_VECTOR(MO,TREND_YR)/
     +                                        TREND_NORM
                     ELSE
                        DEPSL = ANNUAL_BOOK_DEP_30(YEAR)/12.
                     ENDIF
                  ELSEIF(DATA_TYPE == 'R') THEN
                     DEPSL = GPV(MO-1,YEAR) *
     +                                      ANNUAL_BOOK_DEP_30(YEAR)/12.
                  ELSEIF(DATA_TYPE == 'P') THEN
                     DEPSL = GPV(MO-1,YEAR) *
     +                                    ANNUAL_BOOK_DEP_30(YEAR)/1200.
                  ELSE
                     DEPSL = ANNUAL_BOOK_DEP_30(YEAR)/12.
                  ENDIF
c              ELSEIF(YR > AVAIL_DATA_YEARS) THEN
c                 IF(TREND_NORM /= 0.) THEN
c                    DEPSL = ANNUAL_BOOK_DEP_30(AVAIL_DATA_YEARS) *
c    +                          MONTHLY_BOOK_DEP_VECTOR(MO,TREND_YR)/
c    +                                     TREND_NORM
c                 ELSE
c                    DEPSL = ANNUAL_BOOK_DEP_30(AVAIL_DATA_YEARS)/12.
c                 ENDIF
               ENDIF
               DEPSL = DEPSL + BOOK_DEP(MO,YR)
               IF(ABS(NPVFB) <= ABS(DEPSL) .AND.
     +                               .NOT. GROUP_BOOK_DEPRECIATION) THEN
                  BOOK_DEP(MO,YR) = NPVFB
                  ANNUAL_BOOK_DEP = ANNUAL_BOOK_DEP + NPVFB
                  CUM_BOOK_DEP(MO,YR) = CUM_BOOK_DEP(MO-1,YR) + NPVFB
                  BOOK_DEP_FINISHED = .TRUE.
                  EXIT
               ELSE
                  BOOK_DEP(MO,YR) = DEPSL
                  NPVFB = NPVFB - DEPSL
                  ANNUAL_BOOK_DEP = ANNUAL_BOOK_DEP + DEPSL
                  CUM_BOOK_DEP(MO,YR) = CUM_BOOK_DEP(MO-1,YR) + DEPSL
               ENDIF
            ENDDO
            IF(YR <= LAST_AVAILABLE_MONTHLY_YEAR) THEN
               IF(ANNUAL_VECTOR_DEP /= 0) THEN
                  TREND_YR = YR
                  TREND_NORM = ANNUAL_VECTOR_DEP
               ENDIF
            ENDIF
            BOOK_DEP(0,YR) = ANNUAL_BOOK_DEP
         ENDDO
      ENDIF
      DEALLOCATE(MONTHLY_BOOK_DEP_VECTOR_ADDER)
      RETURN
      END
C***********************************************************************
C*                                                                     *
C*                             T O T A L                               *
C*                                                                     *
C*          COPYRIGHT (C) 1982   M.S. GERBER & ASSOCIATES, INC         *
C*                       ALL RIGHTS RESERVED                           *
C*                                                                     *
C***********************************************************************
C                                                                      *
C     PURPOSE:                                                         *
C        TOTAL TOTALS THE ANNUAL BOOK AND TAX DEPRECATION,             *
C        THE RETIREMENTS, AND GROSS PLANT VALUE FOR BOOK               *
C        AND TAX PURPOSES.                                             *
C                                                                      *
C***********************************************************************
C
      RECURSIVE SUBROUTINE SET_UP_EA_ARRAYS
C
      use spindriftlib
      use prod_arrays_dimensions
      USE SIZECOM
      use globecom
      SAVE

      INCLUDE 'NAMESCOM.MON'
      INCLUDE 'MTHNMCOM.MON'

c     LOGICAL(kind=1 MONTHLY_MIDAS_ACTIVE/.TRUE./
C
C STUFF FROM THE MAIN ROUTINE
C
      REAL(kind=4) :: R_TTAXDP,R_TRETIE,R_TBOKDP,
     +     R_TGPV,R_EATAXBOKDEP,R_EA_DEF_TAX,R_CUMULATIVE_BOOK_DEP,
     +     R_TOTAL_NPV_RATEBASE,
     +     R_TOTAL_DEF_TAX_RATEBASE,
     +     R_TOTAL_REGULATED_TAX_DEP,
     +     R_TOTAL_REGULATED_BOOK_DEP,
     +     R_EA_ACE_DEPRECIATION,
     +     R_PROPERTY_TAX_GPV,
     +     R_PROPERTY_TAX_NPV,
     +     R_EA_DEF_TAX_BASIS
      INTEGER(kind=2) :: R_YR,R_CLASS,R_PERIOD,MO
      LOGICAL(kind=1) :: R_CLASS_EXISTS
C
      CHARACTER(len=1) :: DUMMY_TYPE
      INTEGER(kind=2) :: ASSET_CLASS_POINTER(:),CLASS_POINTER
      INTEGER(kind=4) :: IOS
      REAL(kind=4) :: ASSET_ALLOCATOR
      REAL(kind=4) :: ADD_TOTAL_DEF_TAX_RATEBASE(*),
     +     EATAXDEP(*),
     +     ADD_EA_DEF_TAX(*),TOTAL_EA_ACE_DEP(*)
      ALLOCATABLE :: ASSET_CLASS_POINTER
      INTEGER(kind=4) :: VALUES_TO_ZERO
      REAL(kind=4) :: ASSET_CLASS_LIST(:)
      REAL(kind=4) :: ASSET_ALLOCATION_LIST(:)
      ALLOCATABLE ASSET_CLASS_LIST,ASSET_ALLOCATION_LIST
      REAL(kind=4) :: TRETIE(:,:,:),TBOKDP(:,:,:),TTAXDP(:,:),
     +     TGPV(:,:,:),TOTAL_NPV_RATEBASE(:,:,:),
     +     CUMULATIVE_BOOK_DEP(:,:,:),
C ADDED 4/13/92 MIDAS GOLD FOR REAL PROPERTY VALUES
     +     PROPERTY_TAX_GPV(:,:),PROPERTY_TAX_NPV(:,:),
     +     EATAXBOKDEP(:,:),EANORMTAXDP(:,:),EA_DEF_TAX(:,:),
     +     TOTAL_DEF_TAX_RATEBASE(:),EA_ACE_DEPRECIATION(:),
     +     TOTAL_DEF_TAX_BASIS_RATEBASE(:),
     +     EA_DEF_TAX_BASIS(:,:)
      ALLOCATABLE :: TRETIE,TBOKDP,TGPV,TTAXDP,
     +     TOTAL_NPV_RATEBASE,CUMULATIVE_BOOK_DEP,
     +     PROPERTY_TAX_GPV,PROPERTY_TAX_NPV,EATAXBOKDEP,
     +     EANORMTAXDP,EA_DEF_TAX,
     +     TOTAL_DEF_TAX_RATEBASE,EA_ACE_DEPRECIATION,
     +     TOTAL_DEF_TAX_BASIS_RATEBASE,
     +     EA_DEF_TAX_BASIS
      INTEGER(kind=2) :: FINANCIAL_SIMULATION_YEARS=0,
     +                   RUN_YEARS,EXTENSION_YEARS
C     INTEGER(kind=2) :: EA_REPORTING_UNIT
C
      INTEGER(kind=2) :: I,ASSET_CLASS_VECTOR,R_ASSET_CLASS_VECTOR
      INTEGER(kind=2) :: ASSET_CLASS,CLASS,R_ASSET_CLASS,
     +          ASSET_CLASS_ID,DEACTIVE_YR,MO_DEACT
      REAL(kind=4) :: BOKNPV,REGULATORY_ALLOCATOR,DEPCUM
      REAL(kind=4) :: BOKGPV,TOTAL1,REGULATED_TAX_DEP
      REAL(kind=4) :: RETIRE(0:12,*),BOKDP(0:12,*),GPV(0:12,*),
     +     NPV(0:12,*),CUM_BOOK_DEP(0:12,*)
      REAL(kind=4) :: TAXDP(*),BOKTAX(*) ! These arrays are 0 based in the main routine
C                               by NOT 0 basing them here the alinement with
C                               the base year being position 1 works without
C                               having to subtract 1 form the array index 6/17/98
      REAL(kind=4)::ASSET_DEFERRED_TAX_BASIS(*),ASSET_DEFERRED_TAXES(*)
c     REAL :: TAXDP(0:*),BOKTAX(0:*)
c     REAL(kind=4) :: ASSET_DEFERRED_TAX_BASIS(0:*),
c    +                ASSET_DEFERRED_TAXES(0:*)
      REAL(kind=4) :: R_TOTAL_DEF_TAX_BASIS_RATEBASE
      LOGICAL(kind=1) :: TAXES_NORMALIZED,ASSET_REPORT
C
      INTEGER(kind=2) :: NUM_OF_ASSET_CLASSES,MAX_ASSET_CLASS_NUM
      REAL(kind=4) :: PROPERTY_TAX_VALUE,
     +     PROPERTY_ESCALATION(*),
     +     GPV_ESCALATOR,NPV_ESCALATOR
      LOGICAL(kind=1) :: VOID_LOGICAL,RETURN_ASSET_CLASS_LISTS
      REAL(kind=4) :: ALLOCATION_VALUE(0:AVAIL_DATA_YEARS,0:12),
     +       ANNUAL_VECTOR_VALUES(0:AVAIL_DATA_YEARS),
     +       VECTOR_MONTHLY_DATA(12,LAST_AVAILABLE_MONTHLY_YEAR)
      CHARACTER(len=1)::MONTHLY_DATA_UNITS(LAST_AVAILABLE_MONTHLY_YEAR)
      INTEGER(kind=2) :: MONTH_ENDING(LAST_AVAILABLE_MONTHLY_YEAR),YR
      CHARACTER(len=1) :: DATA_TYPE,VECTOR_TYPE*20
      REAL(kind=4) :: TREND_NORM
      INTEGER(kind=2) :: ALLOCATION_VECTOR
      INTEGER(kind=2) :: ALL_YR
      REAL(kind=4) :: ANNUAL_ASSET_ALLOCATOR,
     +       ANNUAL_RETIREMENTS,
     +       ANNUAL_BOOK_DEPRECIATION
      REAL(kind=4) :: MONTH_VARS(0:12,1:*)
C
c      SAVE ASSET_CLASS_POINTER
c      SAVE TRETIE,TBOKDP,TGPV,TOTAL_NPV_RATEBASE,TTAXDP,
c     +     PROPERTY_TAX_GPV,PROPERTY_TAX_NPV,
c     +     EATAXBOKDEP,EANORMTAXDP,EA_DEF_TAX,
c     +     TOTAL_DEF_TAX_RATEBASE,EA_ACE_DEPRECIATION,
c     +     TOTAL_DEF_TAX_BASIS_RATEBASE,
c     +     EA_DEF_TAX_BASIS,CUMULATIVE_BOOK_DEP
c      SAVE ASSET_CLASS_LIST,ASSET_ALLOCATION_LIST,
c     +     NUM_OF_ASSET_CLASSES,MAX_ASSET_CLASS_NUM
C
      CALL RETURN_NUM_OF_EXISTING_CLASSES(NUM_OF_ASSET_CLASSES,
     +                                      MAX_ASSET_CLASS_NUM)
      IF(ALLOCATED(ASSET_CLASS_POINTER)) DEALLOCATE(ASSET_CLASS_POINTER)
      IF(MAX_ASSET_CLASS_NUM > 0) THEN
         ALLOCATE(ASSET_CLASS_POINTER(MAX_ASSET_CLASS_NUM))
         CALL RETURN_EXISTING_CLASS_POINTER(ASSET_CLASS_POINTER)
      ENDIF
      FINANCIAL_SIMULATION_YEARS = RUN_YEARS() + EXTENSION_YEARS() + 1
      IF(ALLOCATED(TRETIE)) THEN
         DEALLOCATE(TRETIE,TBOKDP,TGPV,TOTAL_NPV_RATEBASE,
     +              TTAXDP,CUMULATIVE_BOOK_DEP,
     +              PROPERTY_TAX_GPV,PROPERTY_TAX_NPV,
     +              EATAXBOKDEP,EANORMTAXDP,EA_DEF_TAX,
     +              TOTAL_DEF_TAX_RATEBASE,
     +              EA_ACE_DEPRECIATION,
     +              TOTAL_DEF_TAX_BASIS_RATEBASE,
     +              EA_DEF_TAX_BASIS)
      ENDIF
      ALLOCATE(TRETIE(0:12,FINANCIAL_SIMULATION_YEARS,
     +                                          0:NUM_OF_ASSET_CLASSES))
      ALLOCATE(TBOKDP(0:12,FINANCIAL_SIMULATION_YEARS,
     +                                          0:NUM_OF_ASSET_CLASSES))
      ALLOCATE(TGPV(0:12,FINANCIAL_SIMULATION_YEARS,
     +                                          0:NUM_OF_ASSET_CLASSES))
      ALLOCATE(CUMULATIVE_BOOK_DEP(0:12,FINANCIAL_SIMULATION_YEARS,
     +                                          0:NUM_OF_ASSET_CLASSES))
      ALLOCATE(TOTAL_NPV_RATEBASE(0:12,FINANCIAL_SIMULATION_YEARS,
     +                                          0:NUM_OF_ASSET_CLASSES))
      ALLOCATE(TTAXDP(FINANCIAL_SIMULATION_YEARS,
     +                                          0:NUM_OF_ASSET_CLASSES))
C ADDED 4/13/92 MIDAS GOLD FOR REAL PROPERTY VALUES
      ALLOCATE(PROPERTY_TAX_GPV(FINANCIAL_SIMULATION_YEARS,
     +                                          0:NUM_OF_ASSET_CLASSES))
      ALLOCATE(PROPERTY_TAX_NPV(FINANCIAL_SIMULATION_YEARS,
     +                                          0:NUM_OF_ASSET_CLASSES))
      ALLOCATE(EATAXBOKDEP(FINANCIAL_SIMULATION_YEARS,
     +                                          0:NUM_OF_ASSET_CLASSES))
      ALLOCATE(EANORMTAXDP(FINANCIAL_SIMULATION_YEARS,
     +                                          0:NUM_OF_ASSET_CLASSES))
      ALLOCATE(EA_DEF_TAX(FINANCIAL_SIMULATION_YEARS,
     +                                          0:NUM_OF_ASSET_CLASSES))
      ALLOCATE(EA_DEF_TAX_BASIS(FINANCIAL_SIMULATION_YEARS,
     +                                          0:NUM_OF_ASSET_CLASSES))
      ALLOCATE(TOTAL_DEF_TAX_RATEBASE(FINANCIAL_SIMULATION_YEARS))
      ALLOCATE(EA_ACE_DEPRECIATION(FINANCIAL_SIMULATION_YEARS))
      ALLOCATE(TOTAL_DEF_TAX_BASIS_RATEBASE(FINANCIAL_SIMULATION_YEARS))
      EA_ACE_DEPRECIATION = 0.
      TOTAL_DEF_TAX_RATEBASE = 0.
      TOTAL_DEF_TAX_BASIS_RATEBASE = 0.
C      
      TRETIE = 0.
      TBOKDP = 0.
      TGPV = 0.
      TOTAL_NPV_RATEBASE = 0.
      CUMULATIVE_BOOK_DEP = 0.
C
      TTAXDP = 0.
      EATAXBOKDEP = 0.
      EANORMTAXDP = 0.
      EA_DEF_TAX = 0.
      EA_DEF_TAX_BASIS = 0.
      PROPERTY_TAX_GPV = 0.
      PROPERTY_TAX_NPV = 0.
      RETURN
C
C***********************************************************************
      ENTRY INIT_EA_ARRAYS(ADD_TOTAL_DEF_TAX_RATEBASE,
     +                     EATAXDEP,ADD_EA_DEF_TAX,
     +                     TOTAL_EA_ACE_DEP)
C***********************************************************************
C
c$ifdefined(good_code)
c     DO I = 1, FINANCIAL_SIMULATION_YEARS 
c        EA_ACE_DEPRECIATION(I) = TOTAL_EA_ACE_DEP(I)
c        TOTAL_DEF_TAX_RATEBASE(I) = ADD_TOTAL_DEF_TAX_RATEBASE(I)
c        TTAXDP(I,0) = EATAXDEP(I)
c        EA_DEF_TAX(I,0) = ADD_EA_DEF_TAX(I)
c     ENDDO
c$endif
C

      IF(ALLOCATED(ASSET_CLASS_LIST))
     +                DEALLOCATE(ASSET_CLASS_LIST,ASSET_ALLOCATION_LIST)
      ALLOCATE(ASSET_CLASS_LIST(AVAIL_DATA_YEARS))
      ALLOCATE(ASSET_ALLOCATION_LIST(AVAIL_DATA_YEARS))
C
      RETURN
C
C***********************************************************************
      ENTRY TOTAL(RETIRE,BOKDP,TAXDP,BOKGPV,GPV,BOKTAX,NPV,
     +            BOKNPV,REGULATORY_ALLOCATOR,TAXES_NORMALIZED,
     +            R_ASSET_CLASS,R_ASSET_CLASS_VECTOR,
     +            PROPERTY_ESCALATION,PROPERTY_TAX_VALUE,
     +            ASSET_DEFERRED_TAX_BASIS,ASSET_DEFERRED_TAXES,
     +            CUM_BOOK_DEP)
C***********************************************************************
C
c        NPV(12,0) = GPV(12,0) - CUM_BOOK_DEP(12,0)
         DO I = 1, FINANCIAL_SIMULATION_YEARS
            IF(TAXES_NORMALIZED) THEN
               TOTAL_DEF_TAX_RATEBASE(I) = TOTAL_DEF_TAX_RATEBASE(I) +
     +                   ASSET_DEFERRED_TAXES(I) * REGULATORY_ALLOCATOR
               TOTAL_DEF_TAX_BASIS_RATEBASE(I) =
     +                TOTAL_DEF_TAX_BASIS_RATEBASE(I) +
     +                ASSET_DEFERRED_TAX_BASIS(I) * REGULATORY_ALLOCATOR
            ENDIF
            DO MO = 0, 12               
               NPV(MO,I) = GPV(MO,I) - CUM_BOOK_DEP(MO,I)
            ENDDO
         ENDDO
C
C ALLOCATE TO TOTAL COMPANY AND ASSET CLASSES
C
         VOID_LOGICAL = RETURN_ASSET_CLASS_LISTS(R_ASSET_CLASS,
     +                                           ASSET_CLASS_LIST,
     +                                           R_ASSET_CLASS_VECTOR,
     +                                           ASSET_ALLOCATION_LIST)
c     IF(R_ASSET_CLASS < 0.) THEN
c        CALL GET_ASSET_VAR(ABS(R_ASSET_CLASS),
c    +                                      DUMMY_TYPE,ASSET_CLASS_LIST)
c        CALL GET_ASSET_VAR(ABS(R_ASSET_CLASS_VECTOR),
c    +                                 DUMMY_TYPE,ASSET_ALLOCATION_LIST)
c     ELSE
c        ASSET_CLASS_LIST(1) = R_ASSET_CLASS
c        ASSET_CLASS_LIST(2) = 0
c        IF(R_ASSET_CLASS_VECTOR < 0) THEN
c           ASSET_ALLOCATION_LIST(1) = FLOAT(R_ASSET_CLASS_VECTOR)
c        ELSEIF(R_ASSET_CLASS_VECTOR > 0) THEN
c           ASSET_ALLOCATION_LIST(1) = FLOAT(R_ASSET_CLASS_VECTOR)
c        ELSE
c           ASSET_ALLOCATION_LIST(1) = 100.
c        ENDIF
c        ASSET_ALLOCATION_LIST(2) = 0.
c     ENDIF
         CLASS_POINTER = 1
         DO
            ASSET_CLASS = ASSET_CLASS_LIST(CLASS_POINTER)
            CALL CHECK_IF_CLASS_DEFINED(ASSET_CLASS)
            ASSET_CLASS = ASSET_CLASS + 1
            ASSET_CLASS_ID = ASSET_CLASS
            IF(ASSET_CLASS > 0) ASSET_CLASS = 
     +                                  ASSET_CLASS_POINTER(ASSET_CLASS)
C
            IF(ASSET_ALLOCATION_LIST(CLASS_POINTER) < 0.) THEN
               ALLOCATION_VECTOR =
     +                         ABS(ASSET_ALLOCATION_LIST(CLASS_POINTER))
               IF(MONTHLY_MIDAS_ACTIVE) THEN
                  CALL GET_MONTHLY_ANNUAL_VALUES(ALLOCATION_VECTOR,
     +                                         DATA_TYPE,
     +                                         VECTOR_TYPE,
     +                                         ANNUAL_VECTOR_VALUES(1),
     +                                         VECTOR_MONTHLY_DATA(1,1),
     +                                         MONTHLY_DATA_UNITS,
     +                                         MONTH_ENDING)
                  CALL RETURN_BASE_YEAR_VECTOR_VALUES(
     +                                          ANNUAL_VECTOR_VALUES(0))
            
                  CALL RIPPLE_MONTHLY_DATA(ANNUAL_VECTOR_VALUES(1),
     +                                     VECTOR_MONTHLY_DATA)
C
C LOAD BASE YEAR ALLOCATION                  
C
                  ASSET_ALLOCATOR = ANNUAL_VECTOR_VALUES(0)/100.
                  DO MO = 0, 12
                     ALLOCATION_VALUE(0,MO) = ASSET_ALLOCATOR 
                  ENDDO
C
                  DO YR = 1, AVAIL_DATA_YEARS
                     ASSET_ALLOCATOR = ANNUAL_VECTOR_VALUES(YR)/100.
                     DO MO = 0, 12
                        IF(MO == 0) THEN
                           ALLOCATION_VALUE(YR,MO) =
     +                                     ANNUAL_VECTOR_VALUES(YR)/100.
                        ELSEIF(YR > LAST_AVAILABLE_MONTHLY_YEAR) THEN
                           ALLOCATION_VALUE(YR,MO) =
     +                                     ANNUAL_VECTOR_VALUES(YR)/100.
                        ELSE
                           ALLOCATION_VALUE(YR,MO) =
     +                                   VECTOR_MONTHLY_DATA(MO,YR)/100.
                        ENDIF
                     ENDDO
                  ENDDO
               ELSE
                  CALL GET_ASSET_VAR(ALLOCATION_VECTOR,
     +                               DUMMY_TYPE,ANNUAL_VECTOR_VALUES(1))
                  CALL RETURN_BASE_YEAR_VECTOR_VALUES(
     +                                          ANNUAL_VECTOR_VALUES(0))
C                     
                  DO YR = 0, AVAIL_DATA_YEARS
                     ASSET_ALLOCATOR = ANNUAL_VECTOR_VALUES(YR)/100.
                     DO MO = 0, 12
                        ALLOCATION_VALUE(YR,MO) = ASSET_ALLOCATOR 
                     ENDDO
                  ENDDO
               ENDIF
            ELSE
C                     
               ASSET_ALLOCATOR=ASSET_ALLOCATION_LIST(CLASS_POINTER)/100.
               DO MO = 0, 12
                  DO I = 0, AVAIL_DATA_YEARS
                     ALLOCATION_VALUE(I,MO) = ASSET_ALLOCATOR
                  ENDDO
               ENDDO
            ENDIF
C
            GPV_ESCALATOR = 1.
            IF(BOKGPV /= 0.) THEN
               GPV_ESCALATOR = PROPERTY_TAX_VALUE/BOKGPV
            ENDIF
            NPV_ESCALATOR = 1.
            IF (BOKNPV /= 0.) THEN
               NPV_ESCALATOR = PROPERTY_TAX_VALUE/BOKNPV
            ENDIF
C        ASSET_ALLOCATOR = ALLOCATION_VALUE(1)/100.
            CALL CLASS_DEACTIVATE_IN_YR(ASSET_CLASS_ID,
     +                                             DEACTIVE_YR,MO_DEACT)
C
C        CUMMULATE TOTAL COMPANY
C
            DO I = 1, FINANCIAL_SIMULATION_YEARS
C           TGPV(0,I,ASSET_CLASS) = TGPV(0,I,ASSET_CLASS) + GPV(0,I) *
C    +                                                   ASSET_ALLOCATOR            
C           CUMULATIVE_BOOK_DEP(0,I,ASSET_CLASS) =
C    +                            CUMULATIVE_BOOK_DEP(0,I,ASSET_CLASS) +
C    +                               CUM_BOOK_DEP(0,I) * ASSET_ALLOCATOR
C
               IF(I > DEACTIVE_YR + 1) EXIT
               IF(I == DEACTIVE_YR + 1) THEN
                  TGPV(12,I,ASSET_CLASS) = 0. ! -TGPV(12,I-1,ASSET_CLASS)
                  TOTAL_NPV_RATEBASE(12,I,ASSET_CLASS) = 0.
c     =                           -TOTAL_NPV_RATEBASE(12,I-1,ASSET_CLASS)
                  CUMULATIVE_BOOK_DEP(12,I,ASSET_CLASS) = 0.
c     +                          -CUMULATIVE_BOOK_DEP(12,I-1,ASSET_CLASS)
c                  EXIT
               ENDIF
               ALL_YR = MIN(I,INT(AVAIL_DATA_YEARS+1,2))
               ANNUAL_ASSET_ALLOCATOR = 0.
               ANNUAL_RETIREMENTS = 0.
               ANNUAL_BOOK_DEPRECIATION = 0.
               DO MO = 0, 12
                  ASSET_ALLOCATOR = ALLOCATION_VALUE(ALL_YR-1,MO)
                  IF(I == DEACTIVE_YR + 1 .AND. MO >= MO_DEACT) 
     +                                              ASSET_ALLOCATOR = 0.
                  IF(I == DEACTIVE_YR + 1 .AND. MO == MO_DEACT) THEN
                     TGPV(MO,I,ASSET_CLASS) = 0.
                     TOTAL_NPV_RATEBASE(MO,I,ASSET_CLASS) = 0.
                     CUMULATIVE_BOOK_DEP(MO,I,ASSET_CLASS) = 0.
                  ENDIF
                  IF(MO /= 0) THEN
                     ANNUAL_ASSET_ALLOCATOR = ANNUAL_ASSET_ALLOCATOR
     +                                        + ASSET_ALLOCATOR
                     TRETIE(MO,I,ASSET_CLASS) = TRETIE(MO,I,ASSET_CLASS)
     +                                  + RETIRE(MO,I) * ASSET_ALLOCATOR            
                     ANNUAL_RETIREMENTS = ANNUAL_RETIREMENTS
     +                                  + RETIRE(MO,I) * ASSET_ALLOCATOR            
                     TBOKDP(MO,I,ASSET_CLASS) = TBOKDP(MO,I,ASSET_CLASS)  
     +                                   + BOKDP(MO,I) * ASSET_ALLOCATOR
                     ANNUAL_BOOK_DEPRECIATION = ANNUAL_BOOK_DEPRECIATION
     +                                   + BOKDP(MO,I) * ASSET_ALLOCATOR
                  ENDIF            
                  TGPV(MO,I,ASSET_CLASS) = TGPV(MO,I,ASSET_CLASS) +
     +                                       GPV(MO,I) * ASSET_ALLOCATOR            
                  TOTAL_NPV_RATEBASE(MO,I,ASSET_CLASS) =
     +              TOTAL_NPV_RATEBASE(MO,I,ASSET_CLASS) +
     +                NPV(MO,I) * ASSET_ALLOCATOR * REGULATORY_ALLOCATOR
                  CUMULATIVE_BOOK_DEP(MO,I,ASSET_CLASS) =
     +                           CUMULATIVE_BOOK_DEP(MO,I,ASSET_CLASS) +
     +                              CUM_BOOK_DEP(MO,I) * ASSET_ALLOCATOR
               ENDDO
               TRETIE(0,I,ASSET_CLASS) = TRETIE(0,I,ASSET_CLASS)
     +                                   + ANNUAL_RETIREMENTS
               TBOKDP(0,I,ASSET_CLASS) = TBOKDP(0,I,ASSET_CLASS)  
     +                                   + ANNUAL_BOOK_DEPRECIATION
               ASSET_ALLOCATOR = ANNUAL_ASSET_ALLOCATOR/12.
               TTAXDP(I,ASSET_CLASS) = TTAXDP(I,ASSET_CLASS) + TAXDP(I)*
     +                                                   ASSET_ALLOCATOR            
               IF(TAXES_NORMALIZED) THEN
                  EANORMTAXDP(I,ASSET_CLASS)=EANORMTAXDP(I,ASSET_CLASS)+
     +                                        TAXDP(I) * ASSET_ALLOCATOR
                  EATAXBOKDEP(I,ASSET_CLASS)=EATAXBOKDEP(I,ASSET_CLASS)+
     +                                       BOKTAX(I) * ASSET_ALLOCATOR
                  EA_DEF_TAX_BASIS(I,ASSET_CLASS) =
     +                     EA_DEF_TAX_BASIS(I,ASSET_CLASS) +
     +                     ASSET_ALLOCATOR * ASSET_DEFERRED_TAX_BASIS(I) 
                  EA_DEF_TAX(I,ASSET_CLASS) = EA_DEF_TAX(I,ASSET_CLASS)+
     +                         ASSET_ALLOCATOR * ASSET_DEFERRED_TAXES(I)
               ENDIF
               IF(I > 1) THEN
                  PROPERTY_TAX_GPV(I,ASSET_CLASS) = 
     +                     PROPERTY_TAX_GPV(I,ASSET_CLASS) + GPV(12,I) *
     +                                   GPV_ESCALATOR * ASSET_ALLOCATOR            
                  GPV_ESCALATOR = GPV_ESCALATOR *
     +                                     (1. + PROPERTY_ESCALATION(I))
                  PROPERTY_TAX_NPV(I,ASSET_CLASS) = 
     +                     PROPERTY_TAX_NPV(I,ASSET_CLASS) + NPV(12,I) *
     +                                   NPV_ESCALATOR * ASSET_ALLOCATOR            
                  NPV_ESCALATOR = NPV_ESCALATOR *
     +                                     (1. + PROPERTY_ESCALATION(I))
               ENDIF                                                       
            ENDDO
            CLASS_POINTER = CLASS_POINTER + 1
            IF(CLASS_POINTER > AVAIL_DATA_YEARS) EXIT
            IF(ASSET_CLASS_LIST(CLASS_POINTER) == 0. .OR.
     +                     ASSET_CLASS_LIST(CLASS_POINTER) == -99.) EXIT
         ENDDO
      RETURN
C
C***********************************************************************
      ENTRY EXISTING_ASSET_BY_INFO(R_CLASS,R_TGPV,R_CUMULATIVE_BOOK_DEP)
C***********************************************************************
C
         R_PERIOD = 0
         R_TGPV = 0.
         R_CUMULATIVE_BOOK_DEP = 0.
         IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
            IF(R_CLASS == 0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN 
               R_TGPV = TGPV(0,1,ASSET_CLASS)
               R_CUMULATIVE_BOOK_DEP =
     +                              CUMULATIVE_BOOK_DEP(0,1,ASSET_CLASS) 
            ENDIF
         ENDIF
      RETURN
C***********************************************************************
      ENTRY EXISTING_ASSET_INFO(R_YR,R_CLASS,R_CLASS_EXISTS,
     +                               R_TTAXDP,R_TBOKDP,
     +                               R_TGPV,R_CUMULATIVE_BOOK_DEP,
     +                               R_EATAXBOKDEP,
     +                               R_EA_DEF_TAX_BASIS,
     +                               R_PROPERTY_TAX_GPV,
     +                               R_PROPERTY_TAX_NPV,
     +                               R_EA_DEF_TAX)
C***********************************************************************
C
         R_CLASS_EXISTS = .FALSE.
         R_CUMULATIVE_BOOK_DEP = 0.
         R_PROPERTY_TAX_GPV = 0.
         R_PROPERTY_TAX_NPV = 0.
         R_EA_DEF_TAX = 0.
         R_PERIOD = 0
         IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
            IF(R_CLASS == 0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN 
               R_CLASS_EXISTS = .TRUE.
C MONTHLY ITEMS
               R_TBOKDP = R_TBOKDP+TBOKDP(R_PERIOD,R_YR,ASSET_CLASS)
               IF(R_PERIOD == 0) THEN
                  R_TGPV   = R_TGPV + TGPV(12,R_YR,ASSET_CLASS)
                  R_CUMULATIVE_BOOK_DEP =
     +                          CUMULATIVE_BOOK_DEP(12,R_YR,ASSET_CLASS) 
                  R_TTAXDP = R_TTAXDP + TTAXDP(R_YR,ASSET_CLASS)
                  R_EATAXBOKDEP = R_EATAXBOKDEP +
     +                                     EATAXBOKDEP(R_YR,ASSET_CLASS)
                  R_EA_DEF_TAX_BASIS  = R_EA_DEF_TAX_BASIS +
     +                                EA_DEF_TAX_BASIS(R_YR,ASSET_CLASS)
                  R_EA_DEF_TAX = EA_DEF_TAX(R_YR,ASSET_CLASS)
C ANNAUAL ITEMS
                  R_PROPERTY_TAX_GPV =
     +                                PROPERTY_TAX_GPV(R_YR,ASSET_CLASS)
                  R_PROPERTY_TAX_NPV =
     +                                PROPERTY_TAX_NPV(R_YR,ASSET_CLASS)
               ELSE
                  R_TGPV   = R_TGPV + TGPV(R_PERIOD,R_YR,ASSET_CLASS)
                  R_CUMULATIVE_BOOK_DEP =
     +                    CUMULATIVE_BOOK_DEP(R_PERIOD,R_YR,ASSET_CLASS) 
                  R_TTAXDP = R_TTAXDP + TTAXDP(R_YR,ASSET_CLASS)/12.
                  R_EATAXBOKDEP = R_EATAXBOKDEP +
     +                                 EATAXBOKDEP(R_YR,ASSET_CLASS)/12.
                  R_EA_DEF_TAX_BASIS  = R_EA_DEF_TAX_BASIS +
     +                            EA_DEF_TAX_BASIS(R_YR,ASSET_CLASS)/12.
                  R_EA_DEF_TAX  = EA_DEF_TAX(R_YR,ASSET_CLASS)/12.
C ANNAUAL ITEMS
                  R_PROPERTY_TAX_GPV =
     +                            PROPERTY_TAX_GPV(R_YR,ASSET_CLASS)/12.
                  R_PROPERTY_TAX_NPV =
     +                            PROPERTY_TAX_NPV(R_YR,ASSET_CLASS)/12.
               ENDIF
            ENDIF
         ENDIF
      RETURN
C***********************************************************************
      ENTRY MONTHLY_EXISTING_ASSET_INFO(R_YR,R_CLASS,MONTH_VARS)
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
               YR = R_YR + 1
C MONTHLY INCOME STATEMENT ITEMS
c               DO MO = 0, 12
               MONTH_VARS(:,Monthly_Book_Depreciation) =
     +                          MONTH_VARS(:,Monthly_Book_Depreciation)
     +                          + TBOKDP(:,YR,ASSET_CLASS)
c               ENDDO
            ENDIF
         ENDIF
      RETURN
C***********************************************************************
      ENTRY MONTHLY_EXISTING_ASSET_BS_INFO(R_YR,R_CLASS,MONTH_VARS)
C***********************************************************************
C
         IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
            IF(R_CLASS == 0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN 
               YR = R_YR + 1
C MONTHLY ITEMS
c               DO MO = 0, 12
               MONTH_VARS(:,monthly_gross_plant_in_svc) =
     +                                            TGPV(:,YR,ASSET_CLASS)
               MONTH_VARS(:,monthly_retirements) =
     +                                          TRETIE(:,YR,ASSET_CLASS)
               MONTH_VARS(:,monthly_accum_depreciation) =
     +                             CUMULATIVE_BOOK_DEP(:,YR,ASSET_CLASS)
c$ifdefined(monthl_code)
c                 IF(R_PERIOD == 0) THEN
c                    R_TGPV   = R_TGPV + TGPV(12,YR,ASSET_CLASS)
c                    R_CUMULATIVE_BOOK_DEP =
c    +                          CUMULATIVE_BOOK_DEP(12,YR,ASSET_CLASS) 
c                    R_TTAXDP = R_TTAXDP + TTAXDP(YR,ASSET_CLASS)
c                    R_EATAXBOKDEP = R_EATAXBOKDEP +
c    +                                     EATAXBOKDEP(YR,ASSET_CLASS)
c                    R_EA_DEF_TAX_BASIS  = R_EA_DEF_TAX_BASIS +
c    +                                EA_DEF_TAX_BASIS(YR,ASSET_CLASS)
c                    R_EA_DEF_TAX = EA_DEF_TAX(YR,ASSET_CLASS)
C ANNAUAL ITEMS
c                    R_PROPERTY_TAX_GPV =
c    +                                PROPERTY_TAX_GPV(YR,ASSET_CLASS)
c                    R_PROPERTY_TAX_NPV =
c    +                                PROPERTY_TAX_NPV(YR,ASSET_CLASS)
c                 ELSE
c                    R_TBOKDP = R_TBOKDP+TBOKDP(MO,YR,ASSET_CLASS)
c                    R_TGPV   = R_TGPV + TGPV(MO,YR,ASSET_CLASS)
c                    R_CUMULATIVE_BOOK_DEP =
c    +                       CUMULATIVE_BOOK_DEP(MO,YR,ASSET_CLASS) 
c                    R_TTAXDP = R_TTAXDP + TTAXDP(YR,ASSET_CLASS)/12.
c                    R_EATAXBOKDEP = R_EATAXBOKDEP +
c    +                                 EATAXBOKDEP(YR,ASSET_CLASS)/12.
c                    R_EA_DEF_TAX_BASIS  = R_EA_DEF_TAX_BASIS +
c    +                            EA_DEF_TAX_BASIS(YR,ASSET_CLASS)/12.
c                    R_EA_DEF_TAX  = EA_DEF_TAX(YR,ASSET_CLASS)/12.
C ANNAUAL ITEMS
c                    R_PROPERTY_TAX_GPV =
c    +                            PROPERTY_TAX_GPV(YR,ASSET_CLASS)/12.
c                    R_PROPERTY_TAX_NPV =
c    +                            PROPERTY_TAX_NPV(YR,ASSET_CLASS)/12.
c                 ENDIF
c$endif
c               ENDDO
            ENDIF
         ENDIF
      RETURN
C***********************************************************************
      ENTRY WRITE_EA_BASE_CASE
C***********************************************************************
         CALL OPEN_EA_BASE_CASE_FILE(INT(10,2))
         WRITE(10,IOSTAT=IOS) TTAXDP,TRETIE,TBOKDP,
     +      TGPV,EATAXBOKDEP,EA_DEF_TAX,
     +      TOTAL_NPV_RATEBASE,TOTAL_DEF_TAX_RATEBASE,
     +      EA_ACE_DEPRECIATION,
     +      PROPERTY_TAX_GPV,PROPERTY_TAX_NPV,
     +      TOTAL_DEF_TAX_BASIS_RATEBASE,
     +      EA_DEF_TAX_BASIS,
     +      CUMULATIVE_BOOK_DEP
         CLOSE(10)
      RETURN
C***********************************************************************
      ENTRY READ_EA_BASE_CASE
C***********************************************************************
         CALL RESET_EXASTOL
         CALL SET_UP_EA_ARRAYS
         CALL OPEN_EA_BASE_CASE_FILE(INT(10,2))
         READ(10,IOSTAT=IOS) TTAXDP,TRETIE,TBOKDP,
     +      TGPV,EATAXBOKDEP,EA_DEF_TAX,
     +      TOTAL_NPV_RATEBASE,TOTAL_DEF_TAX_RATEBASE,
     +      EA_ACE_DEPRECIATION,
     +      PROPERTY_TAX_GPV,PROPERTY_TAX_NPV,
     +      TOTAL_DEF_TAX_BASIS_RATEBASE,
     +      EA_DEF_TAX_BASIS,
     +      CUMULATIVE_BOOK_DEP
         CLOSE(10)
      RETURN
      END
C***********************************************************************
C*                                                                     *
C*                          E D P T A X                                *
C*                                                                     *
C*          COPYRIGHT (C) 1982 M.S. GERBER & ASSOCIATES, INC.          *
C*                       ALL RIGHTS RESERVED                           *
C*                                                                     *
C***********************************************************************
C*                                                                     *
C*    PURPOSE:                                                         *
C*       EDPTAX CALCULATES THE ANNUAL DEPRICEATION FOR FEDERAL TAXES   *
C*       BASED ON THE ACCELERATED COST RECOVERY SYSTEM (ASRS) AS       *
C*       SPECIFIED IN THE ECONOMIC RECOVERY TAX ACT (ERTA) OF 1981,    *
C*       OR THE ASSET DEPRECIATION RANGE (ADR), OR STRAIGHT LINE.      *
C*       THE MEHTOD USED IS SPECIFIED FOR EACH ACCOUNT                 *
C*                                                                     *
C***********************************************************************
C
      SUBROUTINE EDPTAX(SERVYR,TAXDP,TXLIFE,TAXGPV,TXDEP1,
     +                  DEPMTH,DBRATE,FINANCIAL_SIMULATION_YEARS)
C
      use spindriftlib
      use prod_arrays_dimensions
      USE SIZECOM
C
      INTEGER(kind=2) :: DEPPCT(15,4,3),J,L,M,BASE_YEAR,SERVYR,YRSINS,
     +          SUMYD,RLIFE,IYR,INYR,LIFE,LIFECK,IDEPCT,IVEC,I
      INTEGER(kind=2) :: FINANCIAL_SIMULATION_YEARS
      REAL(kind=4) :: VECTOR_VALUES(AVAIL_DATA_YEARS),VECTOR_VALUE
      REAL(kind=4) :: NPVFT,TXDEP1,TXLIFE,TAXGPV,DBRATE,
     +     TAXDP(0:FINANCIAL_SIMULATION_YEARS),
     +     DEPSL,DEPDB,DEPBSE,DEPSYD,SL_OFFSET
      CHARACTER(len=4) :: DEPMTH,ACRS,ADR,SL,DB,SYD,DUMMY_TYPE*1
      LOGICAL(kind=1) :: NO_SWITCH,DUMP_AT_TAX_LIFE
      PARAMETER(ACRS = 'ACRS',ADR  = 'ADR ',SL   = 'SL  ',
     +          DB   = 'DB  ',SYD  = 'SYD ')
      REAL (KIND=4) :: RATE
C
      DATA DEPPCT/25,38,37,12*0,15,22,3*21,10*0,
     +            8,14,12,3*10,4*9,5*0,5,10,9,8,2*7,9*6,
     +29,47,24,12*0,18,33,25,16,8,10*0,9,19,16,14,12,10,8,6,4,2,5*0,
     +6,12,12,11,10,9,8,7,6,5,4,4,3,2,1,
     +33,45,22,12*0,20,32,24,16,8,10*0,10,18,16,14,12,10,8,6,4,2,5*0,
     +7,12,12,11,10,9,8,7,6,5,4,3,3,2,1/
C
      TAXDP(0) = TXDEP1
      IF(TXLIFE < 0) THEN
         IVEC = ABS(TXLIFE)
         CALL GET_ASSET_VAR(IVEC,DUMMY_TYPE,VECTOR_VALUES)
         DO I = 1, FINANCIAL_SIMULATION_YEARS
            IF(I < AVAIL_DATA_YEARS) THEN
               VECTOR_VALUE = VECTOR_VALUES(I) 
            ELSEIF(I == AVAIL_DATA_YEARS) THEN
               RATE = VECTOR_VALUES(I-1)
               CALL IMPLIED_ESC(RATE,VECTOR_VALUES(I))
               VECTOR_VALUE = VECTOR_VALUES(I) 
            ELSE
C               VECTOR_VALUE = VECTOR_VALUES(AVAIL_DATA_YEARS) ! MODIFIED TO ESCALATE 11/21/06 DR.G
               VECTOR_VALUE = RATE * VECTOR_VALUE 
            ENDIF
            TAXDP(I) = VECTOR_VALUE
         ENDDO
         RETURN
      ENDIF
C
C     IF THE TAX LIFE IS GREATER THAN 98 YEARS, IT IS ASSUMED
C     THAT NO TAX DEPRECIATION WILL OCURR
C
      IF(TXLIFE > 98.0 .OR. TXLIFE == 0.) RETURN
C
      IF(INDEX(DEPMTH,'ACRS') /= 0) THEN
         LIFECK = INT(TXLIFE + 0.001)
         LIFE = LIFECK/5 + 1
         IF(LIFE > 4) LIFE = 4
         IF(LIFECK > 15) LIFECK = 15
         INYR = 1
         IF(SERVYR < 1981) THEN
            WRITE(4,*) 'Check the EXISTING ASSET file for an account ',
     +                 'using ACRS depreciation and having an inservice'
            WRITE(4,*) 'year before 1981.  MIDAS has adjusted the in ',
     +                 'service year to 1981 from',SERVYR,' for that ',
     +                 'account.'
            SERVYR = 1981
         ENDIF
C
C       THE FOLLOWING TWO LINES WERE COMMENTED OUT DUE TO THE
C       1982 CHANGE IN ACRS
C
C        IF( SERVYR == 1985 ) INYR = 2
C        IF( SERVYR > 1985 ) INYR = 3
C
         YRSINS = BASE_YEAR() - SERVYR
         DO L = 1, FINANCIAL_SIMULATION_YEARS
            IYR = L + YRSINS
            IF( IYR > LIFECK ) EXIT
            IDEPCT   = DEPPCT(IYR,LIFE,INYR)
            TAXDP(L) = TAXGPV * FLOAT(IDEPCT)/100.
         ENDDO
      ELSEIF(INDEX(DEPMTH,'DB') /= 0 .OR. 
     +          INDEX(DEPMTH,'ADR') /= 0 .OR.
     +               INDEX(DEPMTH,'DBNS') /= 0 .OR. 
     +                   INDEX(DEPMTH,'DBND') /= 0 .OR. 
     +                                 INDEX(DEPMTH,'MACR') /= 0) THEN
         NO_SWITCH = INDEX(DEPMTH,'DBNS') /= 0 .OR. 
     +                                         INDEX(DEPMTH,'DBND') /= 0
         DUMP_AT_TAX_LIFE = INDEX(DEPMTH,'DBND') /= 0
         SL_OFFSET = 0.
         IF(INDEX(DEPMTH,'MACR') /= 0) SL_OFFSET = .5
         NPVFT  = TAXGPV - TXDEP1
         DBRATE = DBRATE / (100. * TXLIFE)
         YRSINS = BASE_YEAR() - SERVYR - 1
         DO L = 1, FINANCIAL_SIMULATION_YEARS
            DEPDB = DBRATE * NPVFT
            RLIFE = TXLIFE - (L + YRSINS) + SL_OFFSET
            DEPSL = NPVFT
            IF(RLIFE > 0) THEN
               DEPSL = NPVFT/FLOAT(RLIFE)
            ELSEIF(DUMP_AT_TAX_LIFE) THEN
               TAXDP(L) = NPVFT
               EXIT
            ELSE
               DEPSL = DEPDB   
            ENDIF
            IF(DEPDB >= DEPSL .OR. NO_SWITCH) THEN
               TAXDP(L) = DEPDB
               NPVFT = NPVFT - DEPDB
            ELSE
               DO M = L, FINANCIAL_SIMULATION_YEARS
                  IF(NPVFT < DEPSL) THEN
                     TAXDP(M) = NPVFT
                     EXIT
                  ELSE
                     TAXDP(M) = DEPSL
                     NPVFT = NPVFT - DEPSL
                  ENDIF
               ENDDO
               EXIT
            ENDIF
         ENDDO
         DBRATE = 100.*TXLIFE*DBRATE
C
      ELSEIF(INDEX(DEPMTH,'SL') /= 0) THEN
         DEPSL = TAXGPV / TXLIFE
         NPVFT = TAXGPV - TXDEP1
         DO L = 1, FINANCIAL_SIMULATION_YEARS
            IF(NPVFT <= DEPSL) THEN
               TAXDP(L) = NPVFT
               EXIT
            ELSE
               TAXDP(L) = DEPSL
               NPVFT = NPVFT - DEPSL
            ENDIF
         ENDDO
C
C     DEP METH IS SYD WITH SWITCH TO SL
C
      ELSEIF(INDEX(DEPMTH,'SYD') /= 0) THEN
         NPVFT  = TAXGPV - TXDEP1
         YRSINS = BASE_YEAR() - SERVYR - 1
         LIFECK = INT(TXLIFE + 0.001)
         SUMYD = LIFECK * (LIFECK + 1) / 2
         DEPBSE = TAXGPV / FLOAT(SUMYD)
         DO J = 1, FINANCIAL_SIMULATION_YEARS
            IYR = LIFECK - (J + YRSINS)
            DEPSYD = DEPBSE * FLOAT(IYR)
            DEPSL  = NPVFT
            IF(IYR > 0) DEPSL  = NPVFT/FLOAT(IYR)
            IF(DEPSYD >= DEPSL) THEN
               IF(NPVFT <= DEPSYD) THEN
                  TAXDP(J) = NPVFT
                  EXIT
               ELSE
                  TAXDP(J) = DEPSYD
                  NPVFT = NPVFT - DEPSYD
               ENDIF
            ELSE
               DO M = J, FINANCIAL_SIMULATION_YEARS
                  IF(NPVFT <= DEPSL) THEN
                     TAXDP(M) = NPVFT
                     EXIT
                  ELSE
                     TAXDP(M) = DEPSL
                     NPVFT = NPVFT - DEPSL
                  ENDIF
               ENDDO
               EXIT
            ENDIF
         ENDDO
      ENDIF
      RETURN
      END
C***********************************************************************
C*                                                                     *
C*                           E T X B O K                               *
C*                                                                     *
C*          COPYRIGHT (C) 1982   M.S. GERBER & ASSOCIATES, INC         *
C*                       ALL RIGHTS RESERVED                           *
C*          MODIFIED AND ADDED TO MIDAS 12/16/86                       *
C***********************************************************************
C
      SUBROUTINE ETXBOK(BOKTAX,SERVYR,TAXGPV,TXNORM,
     +                  FINANCIAL_SIMULATION_YEARS)
C
      use spindriftlib
      use prod_arrays_dimensions
      USE SIZECOM
C
      CHARACTER (LEN=1) :: DATA_TYPE
      REAL (KIND=4) :: TAXNPV,BOKTAX(0:*),TXNORM,TAXGPV,
     +                 DEPAMOUNT,
     +                 TAX_NORM_VECTOR(AVAIL_DATA_YEARS)
      INTEGER (KIND=2) ::  SERVYR,BASE_YEAR,I,IVEC,
     +                     FINANCIAL_SIMULATION_YEARS
      CHARACTER (LEN=20) :: VECTOR_TYPE
      INTEGER (KIND=2) :: J
C
      IF(TXNORM >= 99.) RETURN
      IF(TXNORM > 0.) THEN
         IF(TXNORM > 1.) THEN
            DEPAMOUNT = TAXGPV/TXNORM
         ELSE
            DEPAMOUNT = TAXGPV * TXNORM
         ENDIF
         BOKTAX(0) = DEPAMOUNT/2.
         DO I = SERVYR+1, BASE_YEAR()
            BOKTAX(0) = BOKTAX(0) + DEPAMOUNT
         ENDDO
         TAXNPV = MAX(TAXGPV - BOKTAX(0),0.)
C
C     CALCULATE BOOK TAX DEPRICATION
C
         DO I = 1, FINANCIAL_SIMULATION_YEARS
            IF(TAXNPV < DEPAMOUNT) THEN
               BOKTAX(I) = TAXNPV
               EXIT
            ELSE
               BOKTAX(I) = DEPAMOUNT
               TAXNPV = TAXNPV - DEPAMOUNT
            ENDIF
         ENDDO
      ELSE
         IVEC = ABS(TXNORM) + .01
         CALL GET_ASSET_VAR(IVEC,DATA_TYPE,TAX_NORM_VECTOR)
         IF(DATA_TYPE == 'R') THEN
            DEPAMOUNT = TAXGPV * TAX_NORM_VECTOR(1)
         ELSE IF(DATA_TYPE == 'P') THEN
            DEPAMOUNT = TAXGPV * TAX_NORM_VECTOR(1)/100.
         ELSE
            DEPAMOUNT = TAX_NORM_VECTOR(1)
         ENDIF
         BOKTAX(0) = DEPAMOUNT / 2.
         DO I = SERVYR+1, BASE_YEAR()
            BOKTAX(0) = BOKTAX(0) + DEPAMOUNT
         ENDDO
         TAXNPV = MAX(TAXGPV - BOKTAX(0),0.)
         BOKTAX(1) = TAXGPV - TAXNPV
         IF(TAXNPV /= 0.) THEN
            DO I = 1, FINANCIAL_SIMULATION_YEARS
               J = MIN(I,AVAIL_DATA_YEARS)
               IF(DATA_TYPE == 'R') THEN
                  DEPAMOUNT = TAXGPV * TAX_NORM_VECTOR(J)
               ELSE IF(DATA_TYPE == 'P') THEN
                  DEPAMOUNT = TAXGPV * TAX_NORM_VECTOR(J)/100.
               ELSE
                  DEPAMOUNT = TAX_NORM_VECTOR(J)
               ENDIF
               IF(TAXNPV < DEPAMOUNT) THEN
                  BOKTAX(I) = TAXNPV
                  EXIT
               ELSE
                  BOKTAX(I) = DEPAMOUNT
                  TAXNPV = TAXNPV - DEPAMOUNT
               ENDIF
            ENDDO
         ENDIF
      ENDIF
      RETURN
      END
C***********************************************************************

