!     ******************************************************************
!     finanbse.for
!     Copyright(c)  2000
!
!     Created: 8/13/2003 12:33:17 PM
!     Author : MARK S GERBER
!     Last change: MSG 8/26/2009 9:14:06 AM
!     ******************************************************************

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C                        F I N / A N C E                               C
C        Copyright (c) 1982, 1983 M.S. Gerber & Associates, Inc.       C
C                      All Rights Reserved                             C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C     Purpose:  FIN/ANCE is the heart of the FIN system.  This         C
C               module computes the liabilities portion of the         C
C               balance sheet, and the tax and interest expenses       C
C               on the income statement.                               C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE FINANCE(RUN_YEAR,WRITE_THE_RESULTS,YEARS_TO_RUN)
C
      INCLUDE 'SpinLib.MON'
      USE FINANCIAL_SWITCHES_COMMON
      INCLUDE 'SIZECOM.MON'
C     INCLUDE 'SHRTFORM.MON'
      INTEGER*2 LAST_FIN_PARM_RECORD/AVAIL_DATA_YEARS/,CLASS_NUM
      INTEGER*4 VALUES_TO_ZERO
      INTEGER*4 ERROR_CODE
      LOGICAL*4 WRITE_THE_RESULTS
      INTEGER*2 LAST_VARIABLE,LAST_OUTPUT_VARIABLE,IOS
      REAL*4 VARIABLE_VALUE(:)
      ALLOCATABLE :: VARIABLE_VALUE
      INCLUDE 'FINANCOM.MON'
      INCLUDE 'PROD2COM.MON'
      INCLUDE 'DSMFRCOM.MON'
      INCLUDE 'ENVIRCOM.MON'
      INCLUDE 'RETLSCOM.MON'
      INCLUDE 'SERVCOM.MON'
      INCLUDE 'KEPCOCOM.MON'
C
C     INTEGER*2 J
      LOGICAL*1 POOLING_TRANSACTIONS,COVERAGE_RATIO_4_TO_7_OR_10,
     +          SYSTEM_BASED_FORECAST,RUN_THIS_CODE,USE_BUDGET_EXPENSES
C
      CHARACTER*1 TYPETAXPAID
C     CHARACTER*1 COUNTRY,CANADA
C     CHARACTER*1024 RECLN
      INTEGER*2 YEARS_TO_RUN,PARM_YEAR
      INTEGER*2 I,ITER,RUN_YEARS,EXTENSION_YEARS,
     +          MXITER,SPMETH,TXMETH,RUN_YEAR,DELETE
      REAL*4 INTLTD,LTDLAST,CUM_NF_EXPENSE,CUM_NF_RATEBASE_EXP,
     +     ROR,LTDISP,PSDIVP,PSISSP,PS_DIV,
     +     FORECAST_SALES,EXTERNAL_FINANCING_REQUIRED,
     +     EXTERNAL_FINANCING,
     +     GET_ENERGY_EXCHANGE_ADJUSTMENT,
     +     NOT_AVAIL,CUSTOMER_DEPOSITS_BAL(2),
     +     R_CUSTOMER_DEPOSITS_BAL1,
     +     R_CUSTOMER_DEPOSITS_BAL2,TEMP_TAX,
     +     RESERVE_CAPACITY_PURCHASES,
     +     RESERVE_CAPACITY_SALES,
     +     NUC_FUEL_TOTAL_BURN,
     +     NF_SL_DEF_TAX_DEP,
     +     NF_DEFERRED_TAX_BASIS
      LOGICAL*1 DUKE
      REAL*4 RATEBASE_GOPINC,TRB,R_TRB,R_RATEBASE_GOPINC
      REAL*4 CUM_SALVAGE_TRANSACTIONS,AFUDC_BORROWED_RATE,
     +     TAXABLE_BTL_INCOME
C
C DUKE CATAWBA STUFF
C
      REAL*4 CATAWBA_DEFERRED_TAXES,CATAWBA_TOTAL_OTHER_NET_REVS,
     +       CATAWBA_TOTAL_LEVEL_CAP_PAYMTS
C
C CICA STUFF 7/15/93
C
      REAL*4 CIAC_BALANCE(2),
     +     CIAC_AMORTIZATION
C     REAL CIAC_CASH,CIAC_RATEBASE(2)
C
C TVA STUFF
C
      REAL*4 CUM_AI_DEP,CUM_AI_GPV,AI_LAGGED_GPV,AI_CWIP_BALANCE
C
      REAL*8 GET_TOTAL_SALES_REVENUE
      REAL*8 GET_VAR_COST,GET_FUEL_COST
      LOGICAL*1 DONEIN,DONEDV,DONETX,FIRST_PASS,FUNDS_BALANCE,
     +          IOU_UTILITY,USE_EXTERNAL_PRODUCTION_COST,
     +          SALT_RIVER_PROJECT
      LOGICAL*1 NUC_FUEL_TAX_EXP_IS_BURN/.TRUE./
C
C FP&L STUFF ADDED 5/5/93 M.S.G.
C
      REAL*4 CUSTOMER_DEPOSITS_PERCENT_REVS,
     +     ADDENDUM_TO_CUSTOMER_DEPOSITS,
     +     INTEREST_ON_CUSTOMER_DEPOSITS
      REAL*4 EXP_INC_EMIS_EXPENSE
      LOGICAL*1 TEST_FINANCE_ELIM
C
      PARAMETER(NOT_AVAIL=-999999.)
      CHARACTER (LEN=1) :: UTILITY_TYPE
C     PARAMETER(CANADA='C')
C
C
      SAVE SPMETH,CUM_NF_EXPENSE,CUM_NF_RATEBASE_EXP,IOU_UTILITY,
     +     CUM_AI_DEP,CUM_AI_GPV,CUM_SALVAGE_TRANSACTIONS,
     +     CUSTOMER_DEPOSITS_BAL,AI_LAGGED_GPV,AI_CWIP_BALANCE
      SAVE TRB,RATEBASE_GOPINC
      LOGICAL*1 WKP_ACTIVE,WKP_OBJECT,ASSET_CLASS_REPORTS
      SAVE WKP_ACTIVE
C
C
C     READ EXECUTION FILE AND OPEN ASSET FILES, OUTPUT FILES
C     IF THE BASE CASE AND FIRST YEAR
C
      MXITER = 20
    1 IF(RUN_YEAR == 1) THEN
C
C ASSET CLASS ANALYSIS
C
C        CALL INIT_ASSET_CLASS_INFO
C        CALL READ_CLASS_INITIALIZATION_FILE
C        CALL ASSET_CLASS_ANALYSIS(RUN_YEAR)
C
C READ TAX LOSSES
C
         AI_GPV = 0.
         CALL ASSET_CLASS_ANALYSIS_BY(RUN_YEAR,WRITE_THE_RESULTS) !,NPV(1))
         IF(WRITE_THE_RESULTS) THEN
            LAST_OUTPUT_VARIABLE = LAST_VARIABLE()
            ALLOCATE(VARIABLE_VALUE(0:LAST_OUTPUT_VARIABLE))
            VARIABLE_VALUE = NOT_AVAIL
            CALL RETURN_ASSET_CLASS_VALUES(VARIABLE_VALUE,
     +                                     INT2(1), ! COVERAGE_RATIO_TYPE,
     +                                     'O', ! OPERATING_METHOD,
     +                                     0.,  ! LEASE_PAYMENTS_IN_TIER,
     +                                     0.)  ! SRP_RATIO)
            WKP_ACTIVE = WKP_OBJECT(VARIABLE_VALUE)
C
C
C  WRITE-BASE YEAR DATA HERE
C
            CALL OUTPUT_RESULTS(VARIABLE_VALUE,BASE_YEAR)
            DEALLOCATE(VARIABLE_VALUE,STAT=ERROR_CODE)
         ENDIF
      ENDIF
C
C TVA STUFF
C
      IF(UTILITY_TYPE() == 'T' .AND. USE_EXTERNAL_PRODUCTION_COST())THEN
         CALL EXTERNAL_AI_ANALYSIS
         CALL RESET_UNIT_COUNT_TO_PSYM
         CALL AI_ANALYSIS(RUN_YEAR,AI_CASH,AI_DEPRECIATION,
     +                    AI_AFUDC1,AI_EXPENSE,
     +                    AI_TAX_DEPRECIATION,
     +                    AI_AMT_PREFERENCES,
     +                    AI_DEFERRED_TAX)
         CALL RESET_UNIT_COUNT_TO_MIDAS
      ELSE
         CALL AI_ANALYSIS(RUN_YEAR,AI_CASH,AI_DEPRECIATION,
     +                       AI_AFUDC1,AI_EXPENSE,
     +                       AI_TAX_DEPRECIATION,
     +                       AI_AMT_PREFERENCES,
     +                       AI_DEFERRED_TAX)
      ENDIF
      CALL ASSET_CLASS_ANALYSIS(RUN_YEAR,WRITE_THE_RESULTS)
      RETURN
      END
C**********************************************************************
C
C             THE ADDITIONS & IMPROVMENTS ANALYSIS MODULE FOR TVA
C                             COPYRIGHT (C) 1989
C                        M.S. GERBER & ASSOCIATES, INC.
C                              ALL RIGHTS RESERVED
C
C**********************************************************************
C
      SUBROUTINE AI_ANALYSIS(RUN_YEAR,AI_CASH,
     +                       AI_DEPRECIATION,
     +                       AI_AFUDC1,
     +                       AI_EXPENSE,
     +                       AI_TAX_DEPRECIATION,
     +                       AI_AMT_PREFERENCES,
     +                       AI_DEFERRED_TAX)
C
      INCLUDE 'SpinLib.MON'
      INCLUDE 'SIZECOM.MON'
      INCLUDE 'PROD2COM.MON'
      INCLUDE 'MTHNMCOM.MON'
      REAL*4 R_MONTHLY_CLASS_VALUES(0:12,*)
      INTEGER*2 MO
      REAL*4 AI_CASH,AI_DEPRECIATION,AI_AFUDC1,
     +     AI_EXPENSE,
     +     AI_TAX_DEPRECIATION,
     +     AI_AMT_PREFERENCES,
     +     AI_DEFERRED_TAX
      INTEGER*2 DATE,RUN_YEAR,START_DEP_YEAR
      REAL*4 CL_LAST_YEAR_INVESTMENT(:),EL_LAST_YEAR_INVESTMENT(:),
     +     AI_ANNUAL_DEP(:),
     +     AI_ANNUAL_TAX_DEP(:),
     +     AI_ANNUAL_DEFERRED_TAX(:),
     +     AI_ANNUAL_AMT_PREFERENCE_ITEMS(:)
      ALLOCATABLE :: CL_LAST_YEAR_INVESTMENT,
     +               EL_LAST_YEAR_INVESTMENT,
     +               AI_ANNUAL_DEP,
     +               AI_ANNUAL_TAX_DEP,
     +               AI_ANNUAL_DEFERRED_TAX,
     +               AI_ANNUAL_AMT_PREFERENCE_ITEMS
C
C CLASS VARAIBLES
C
      INTEGER*4 VALUES_2_ZERO
      INTEGER*2 MAX_AI_CLASSES,RETURN_MAX_CL_CLASS_NUM,VOID_INT2,
     +          GET_CL_ASSET_CLASS_INFO,R_CLASS,
     +          RETURN_MAX_EL_CLASS_NUM
      SAVE MAX_AI_CLASSES
      REAL*4 R_AI_ANNUAL_CLASS_DEP,
     +     R_AI_ANNUAL_CLASS_TAX_DEP,
     +     R_AI_ANNUAL_CLASS_DEFERRED_TAX,
     +     R_AI_ANNUAL_CLASS_PREF_ITEMS,
     +     R_AI_CLASS_CASH,
     +     R_AI_CLASS_EXPENSE,
     +     R_AI_CLASS_AFUDC1
      INTEGER*2 ASSET_CLASS_NUM(:),ASSET_VECTOR(:)
      ALLOCATABLE :: ASSET_CLASS_NUM,ASSET_VECTOR
      REAL*4 AI_ANNUAL_CLASS_DEP(:,:),
     +     AI_ANNUAL_CLASS_TAX_DEP(:,:),
     +     AI_ANNUAL_CLASS_DEFERRED_TAX(:,:),
     +     AI_ANNUAL_CLASS_AMT_PREF_ITEMS(:,:),
     +     AI_CLASS_CASH(:),
     +     AI_CLASS_EXPENSE(:),
     +     AI_CLASS_AFUDC1(:)
      REAL*4 R_AI_LAGGED_GPV,AI_LAGGED_GPV(:)
      ALLOCATABLE :: AI_ANNUAL_CLASS_DEP,
     +               AI_ANNUAL_CLASS_TAX_DEP,
     +               AI_ANNUAL_CLASS_DEFERRED_TAX,
     +               AI_ANNUAL_CLASS_AMT_PREF_ITEMS,
     +               AI_CLASS_CASH,
     +               AI_CLASS_EXPENSE,
     +               AI_CLASS_AFUDC1,AI_LAGGED_GPV
C
      SAVE AI_ANNUAL_CLASS_DEP,
     +     AI_ANNUAL_CLASS_TAX_DEP,
     +     AI_ANNUAL_CLASS_DEFERRED_TAX,
     +     AI_ANNUAL_CLASS_AMT_PREF_ITEMS,
     +     AI_CLASS_CASH,
     +     AI_CLASS_EXPENSE,
     +     AI_CLASS_AFUDC1,AI_LAGGED_GPV
C
      SAVE AI_ANNUAL_DEP,
     +     AI_ANNUAL_TAX_DEP,
     +     AI_ANNUAL_DEFERRED_TAX,
     +     AI_ANNUAL_AMT_PREFERENCE_ITEMS,
     +     CL_LAST_YEAR_INVESTMENT,
     +     EL_LAST_YEAR_INVESTMENT
C
      IF(RUN_YEAR == 1) THEN
         IF(ALLOCATED(AI_ANNUAL_DEP)) DEALLOCATE(AI_ANNUAL_DEP,
     +                                   AI_ANNUAL_TAX_DEP,
     +                                   AI_ANNUAL_DEFERRED_TAX,
     +                                   AI_ANNUAL_AMT_PREFERENCE_ITEMS,
     +                                   CL_LAST_YEAR_INVESTMENT,
     +                                   EL_LAST_YEAR_INVESTMENT)
         ALLOCATE(AI_ANNUAL_DEP(MAX_FINANCIAL_SIMULATION_YEARS),
     +           AI_ANNUAL_TAX_DEP(MAX_FINANCIAL_SIMULATION_YEARS),
     +           AI_ANNUAL_DEFERRED_TAX(MAX_FINANCIAL_SIMULATION_YEARS),
     +           AI_ANNUAL_AMT_PREFERENCE_ITEMS
     +                                 (MAX_FINANCIAL_SIMULATION_YEARS),
     +           CL_LAST_YEAR_INVESTMENT(MAX_CL_UNITS),
     +           EL_LAST_YEAR_INVESTMENT(MAX_EL_UNITS))
         AI_ANNUAL_DEP = 0.
         AI_ANNUAL_TAX_DEP = 0.
         AI_ANNUAL_DEFERRED_TAX = 0.
         AI_ANNUAL_AMT_PREFERENCE_ITEMS = 0.
         EL_LAST_YEAR_INVESTMENT = 0.
         CL_LAST_YEAR_INVESTMENT = 0.
C
C ASSET CLASS ADDITIONS
C
         IF(ALLOCATED(AI_ANNUAL_CLASS_DEP))
     +                  DEALLOCATE(AI_ANNUAL_CLASS_DEP,
     +                             AI_ANNUAL_CLASS_TAX_DEP,
     +                             AI_ANNUAL_CLASS_DEFERRED_TAX,
     +                             AI_ANNUAL_CLASS_AMT_PREF_ITEMS,
     +                             AI_CLASS_CASH,
     +                             AI_CLASS_EXPENSE,
     +                             AI_CLASS_AFUDC1,AI_LAGGED_GPV)
         MAX_AI_CLASSES = MAX(RETURN_MAX_CL_CLASS_NUM(),
     +                        RETURN_MAX_EL_CLASS_NUM())
         ALLOCATE(AI_ANNUAL_CLASS_DEP(MAX_FINANCIAL_SIMULATION_YEARS,
     +                                                0:MAX_AI_CLASSES),
     +           AI_ANNUAL_CLASS_TAX_DEP(MAX_FINANCIAL_SIMULATION_YEARS,
     +                                                0:MAX_AI_CLASSES),
     +           AI_ANNUAL_CLASS_DEFERRED_TAX(
     +                                   MAX_FINANCIAL_SIMULATION_YEARS,
     +                                                0:MAX_AI_CLASSES),
     +           AI_ANNUAL_CLASS_AMT_PREF_ITEMS(
     +                                   MAX_FINANCIAL_SIMULATION_YEARS,
     +                                                0:MAX_AI_CLASSES),
     +           AI_CLASS_CASH(0:MAX_AI_CLASSES),
     +           AI_CLASS_EXPENSE(0:MAX_AI_CLASSES),
     +           AI_CLASS_AFUDC1(0:MAX_AI_CLASSES),
     +           AI_LAGGED_GPV(0:MAX_AI_CLASSES))
         AI_ANNUAL_CLASS_DEP = 0.
         AI_ANNUAL_CLASS_TAX_DEP = 0.
         AI_ANNUAL_CLASS_DEFERRED_TAX = 0.
         AI_ANNUAL_CLASS_AMT_PREF_ITEMS = 0.
         AI_LAGGED_GPV = 0.
      ENDIF
C
      AI_CLASS_CASH = 0.
      AI_CLASS_EXPENSE = 0.
      AI_CLASS_AFUDC1 = 0.
C
      AI_CASH = 0.
      AI_EXPENSE = 0.
      AI_AFUDC1 = 0.
      DATE = BASE_YEAR - 1900 + YEAR
      START_DEP_YEAR = RUN_YEAR + 1
      IF(NUNITS > 0) THEN
         ALLOCATE(ASSET_CLASS_NUM(NUNITS),ASSET_VECTOR(NUNITS))
         VOID_INT2=GET_CL_ASSET_CLASS_INFO(ASSET_CLASS_NUM,ASSET_VECTOR)
         CALL AI_DEP_AFUDC(START_DEP_YEAR,DATE,AI_CASH,AI_AFUDC1,
     +                     AI_EXPENSE,
     +                     AI_ANNUAL_DEP,
     +                     AI_ANNUAL_TAX_DEP,
     +                     AI_ANNUAL_DEFERRED_TAX,
     +                     AI_ANNUAL_AMT_PREFERENCE_ITEMS,
     +                     CL_AI_INVESTMENT,AI_CL_REMAINING_LIFE,
     +                     AI_CL_TAX_LIFE,AI_CL_ADR_LIFE,
     +                     NUNITS,ONLINE,OFLINE,RUN_YEAR,
     +                     CL_LAST_YEAR_INVESTMENT,BASE_YEAR,
C ASSET CLASS ADDITIONS
     +                     ASSET_CLASS_NUM,ASSET_VECTOR,MAX_AI_CLASSES,
     +                     AI_ANNUAL_CLASS_DEP,
     +                     AI_ANNUAL_CLASS_TAX_DEP,
     +                     AI_ANNUAL_CLASS_DEFERRED_TAX,
     +                     AI_ANNUAL_CLASS_AMT_PREF_ITEMS,
     +                     AI_CLASS_CASH,
     +                     AI_CLASS_EXPENSE,
     +                     AI_CLASS_AFUDC1)
         DEALLOCATE(ASSET_CLASS_NUM,ASSET_VECTOR)
      ENDIF
      IF(HYDRO_UNITS > 0) THEN
         ALLOCATE(ASSET_CLASS_NUM(HYDRO_UNITS),
     +            ASSET_VECTOR(HYDRO_UNITS))
         CALL GET_EL_ASSET_CLASS_INFO(ASSET_CLASS_NUM,ASSET_VECTOR)
         CALL AI_DEP_AFUDC(START_DEP_YEAR,DATE,AI_CASH,AI_AFUDC1,
     +                     AI_EXPENSE,
     +                     AI_ANNUAL_DEP,
     +                     AI_ANNUAL_TAX_DEP,
     +                     AI_ANNUAL_DEFERRED_TAX,
     +                     AI_ANNUAL_AMT_PREFERENCE_ITEMS,
     +                     EL_AI_INVESTMENT,AI_EL_REMAINING_LIFE,
     +                     AI_EL_TAX_LIFE,AI_EL_ADR_LIFE,
     +                     HYDRO_UNITS,ON_LINE,OFF_LINE,RUN_YEAR,
     +                     EL_LAST_YEAR_INVESTMENT,BASE_YEAR,
C ASSET CLASS ADDITIONS
     +                     ASSET_CLASS_NUM,ASSET_VECTOR,MAX_AI_CLASSES,
     +                     AI_ANNUAL_CLASS_DEP,
     +                     AI_ANNUAL_CLASS_TAX_DEP,
     +                     AI_ANNUAL_CLASS_DEFERRED_TAX,
     +                     AI_ANNUAL_CLASS_AMT_PREF_ITEMS,
     +                     AI_CLASS_CASH,
     +                     AI_CLASS_EXPENSE,
     +                     AI_CLASS_AFUDC1)
         DEALLOCATE(ASSET_CLASS_NUM,ASSET_VECTOR)
      ENDIF
      AI_DEPRECIATION = AI_ANNUAL_DEP(RUN_YEAR)
      AI_TAX_DEPRECIATION = AI_ANNUAL_TAX_DEP(RUN_YEAR)
      AI_DEFERRED_TAX = AI_ANNUAL_DEFERRED_TAX(RUN_YEAR)
      AI_AMT_PREFERENCES = AI_ANNUAL_AMT_PREFERENCE_ITEMS(RUN_YEAR)
      RETURN
C**********************************************************************
      ENTRY RETURN_AI_ASSET_CLASS_EXPENSES(R_CLASS,RUN_YEAR,
     +                                 R_AI_ANNUAL_CLASS_DEP,
     +                                 R_AI_ANNUAL_CLASS_TAX_DEP,
     +                                 R_AI_ANNUAL_CLASS_DEFERRED_TAX,
     +                                 R_AI_ANNUAL_CLASS_PREF_ITEMS,
     +                                 R_AI_CLASS_CASH,
     +                                 R_AI_CLASS_EXPENSE,
     +                                 R_AI_CLASS_AFUDC1,
     +                                 R_AI_LAGGED_GPV)
C**********************************************************************
C
         IF(R_CLASS >= 0 .AND. R_CLASS <= MAX_AI_CLASSES) THEN
            R_AI_ANNUAL_CLASS_DEP=AI_ANNUAL_CLASS_DEP(RUN_YEAR,R_CLASS)
            R_AI_ANNUAL_CLASS_TAX_DEP =
     +                         AI_ANNUAL_CLASS_TAX_DEP(RUN_YEAR,R_CLASS)
            R_AI_ANNUAL_CLASS_DEFERRED_TAX =
     +                    AI_ANNUAL_CLASS_DEFERRED_TAX(RUN_YEAR,R_CLASS)
            R_AI_ANNUAL_CLASS_PREF_ITEMS =
     +                  AI_ANNUAL_CLASS_AMT_PREF_ITEMS(RUN_YEAR,R_CLASS)
            R_AI_CLASS_CASH = AI_CLASS_CASH(R_CLASS)
            R_AI_CLASS_EXPENSE = AI_CLASS_EXPENSE(R_CLASS)
            R_AI_CLASS_AFUDC1 = AI_CLASS_AFUDC1(R_CLASS)
            R_AI_LAGGED_GPV = AI_LAGGED_GPV(R_CLASS)
            AI_LAGGED_GPV(R_CLASS) = R_AI_CLASS_CASH + R_AI_CLASS_AFUDC1 
         ELSE
            R_AI_ANNUAL_CLASS_DEP = 0.
            R_AI_ANNUAL_CLASS_TAX_DEP = 0.
            R_AI_ANNUAL_CLASS_DEFERRED_TAX = 0.
            R_AI_ANNUAL_CLASS_PREF_ITEMS = 0.
            R_AI_CLASS_CASH = 0.
            R_AI_CLASS_EXPENSE = 0.
            R_AI_CLASS_AFUDC1 = 0.
            R_AI_LAGGED_GPV = 0.
         ENDIF
      RETURN
C**********************************************************************
      ENTRY RETURN_AI_MONTHLY_CLASS_VALUES(R_CLASS,
     +                                     RUN_YEAR, !NOTE: ADD 1 WHEN USED
     +                                     R_MONTHLY_CLASS_VALUES)
C**********************************************************************
C
         
         IF(R_CLASS >= 0 .AND. R_CLASS <= MAX_AI_CLASSES) THEN
            R_MONTHLY_CLASS_VALUES(0,Cash AI Investment) = 
     +                    R_MONTHLY_CLASS_VALUES(0,Cash AI Investment) +
     +                    AI_CLASS_CASH(R_CLASS)
            R_MONTHLY_CLASS_VALUES(0,ai_book_investment) = 
     +                    R_MONTHLY_CLASS_VALUES(0,ai_book_investment) +
     +                    AI_CLASS_CASH(R_CLASS)
            DO MO = 1, 12
               R_MONTHLY_CLASS_VALUES(MO,Cash AI Investment) = 
     +                   R_MONTHLY_CLASS_VALUES(MO,Cash AI Investment) +
     +                   AI_CLASS_CASH(R_CLASS)/12.
               R_MONTHLY_CLASS_VALUES(MO,ai_book_investment) = 
     +                   R_MONTHLY_CLASS_VALUES(MO,ai_book_investment) +
     +                   AI_CLASS_CASH(R_CLASS)/12.
            ENDDO
         ENDIF
      RETURN
      END
C**********************************************************************
C
C                   THE DEPRECIATION & AFUDC FOR TVA ON 
C                          ADDITIONS & IMPROVMENTS 
C                             COPYRIGHT (C) 1989
C                        M.S. GERBER & ASSOCIATES, INC.
C                              ALL RIGHTS RESERVED
C
C**********************************************************************
C
      SUBROUTINE AI_DEP_AFUDC(START_DEP_YEAR,DATE,AI_CASH,AI_AFUDC1,
     +                        AI_EXPENSE,
     +                        AI_ANNUAL_DEP,
     +                        AI_ANNUAL_TAX_DEP,
     +                        AI_ANNUAL_DEFERRED_TAX,
     +                        AI_ANNUAL_AMT_PREFERENCE_ITEMS,
     +                        INVESTMENT,AI_REMAINING_LIFE,
     +                        AI_TAX_LIFE,AI_ADR_TAX_LIFE,
     +                        NUNITS,ON_LINE,OFF_LINE,YEAR,
     +                        LAST_YEAR_INVESTMENT,BASE_YEAR,
     +                        ASSET_CLASS_NUM,ASSET_CLASS_VECTOR,
     +                        MAX_AI_CLASSES,
     +                        AI_ANNUAL_CLASS_DEP,
     +                        AI_ANNUAL_CLASS_TAX_DEP,
     +                        AI_ANNUAL_CLASS_DEFERRED_TAX,
     +                        AI_ANNUAL_CLASS_AMT_PREF_ITEMS,
     +                        AI_CLASS_CASH,
     +                        AI_CLASS_EXPENSE,
     +                        AI_CLASS_AFUDC1)
      use end_routine, only: end_program, er_message
      INCLUDE 'SpinLib.MON'
      INCLUDE 'SIZECOM.MON'
      INTEGER*2 NUNITS,DATE,AI_REMAINING_LIFE(*),
     +          ON_LINE(*),OFF_LINE(*),YEAR,YR
      REAL*4  AI_ANNUAL_DEP(*),
     +      AI_ANNUAL_TAX_DEP(*),
     +      AI_ANNUAL_DEFERRED_TAX(*),
     +      AI_ANNUAL_AMT_PREFERENCE_ITEMS(*)
      INTEGER*2 I,START_DEP_YEAR,J,POINTER_VALUE,BASE_YEAR
      REAL*4 AI_CASH,AI_AFUDC1,INVESTMENT(*),GET_VAR
      REAL*4 AI_TAX_LIFE(*),AI_ADR_TAX_LIFE(*)
      REAL*4 INTEREST_CAPITIALIZATION_RATE,INTEREST_CAP_RATE,
     +       AI_BOOK_VALUE
      REAL*4 LAST_YEAR_INVESTMENT(NUNITS),AI_TAX_VALUE
      REAL*4 REMAINING_LIFE,AI_INVESTMENT,AFUDC_RATE,AI_EXPENSE,
     +     ANNUAL_DEP,ANNUAL_AFUDC,AI_AFUDC_RATE
      CHARACTER*1 UTILITY_TYPE
      LOGICAL*1 UTILITY_IS_TVA,UTILITY_IS_IOU
C
      REAL*4 AI_INVESTMENT_CLASS,AI_EXPENSE_CLASS
      INTEGER*2 ASSET_CLASS_NUM(*),ASSET_CLASS_VECTOR(*),MAX_AI_CLASSES
      INTEGER*2 ASSET_CLASS,ASSET_ALLOCATION_VECTOR,CLASS_POINTER
      CHARACTER*1 DUMMY_TYPE
      REAL*4 AI_ANNUAL_CLASS_DEP(MAX_FINANCIAL_SIMULATION_YEARS,0:*),
     +     AI_ANNUAL_CLASS_TAX_DEP(MAX_FINANCIAL_SIMULATION_YEARS,0:*),
     +     AI_ANNUAL_CLASS_DEFERRED_TAX(
     +                              MAX_FINANCIAL_SIMULATION_YEARS,0:*),
     +     AI_ANNUAL_CLASS_AMT_PREF_ITEMS(
     +                              MAX_FINANCIAL_SIMULATION_YEARS,0:*),
     +     AI_CLASS_CASH(0:*),
     +     AI_CLASS_EXPENSE(0:*),
     +     AI_CLASS_AFUDC1(0:*)
      REAL*4 ASSET_CLASS_LIST(AVAIL_DATA_YEARS)
      REAL*4 ASSET_ALLOCATION_LIST(AVAIL_DATA_YEARS),ASSET_ALLOCATOR
C
      REAL*4 AI_TAX_DEP(:),
     +     AI_BOOK_DEP(:),
     +     AI_TAX_PREFERRENCE(:),
     +     AI_DEFERRED_TAXES(:)
      ALLOCATABLE :: AI_TAX_DEP,
     +               AI_BOOK_DEP,
     +               AI_TAX_PREFERRENCE,
     +               AI_DEFERRED_TAXES
C
      ALLOCATE(AI_TAX_DEP(MAX_FINANCIAL_SIMULATION_YEARS),
     +         AI_TAX_PREFERRENCE(MAX_FINANCIAL_SIMULATION_YEARS),
     +         AI_BOOK_DEP(MAX_FINANCIAL_SIMULATION_YEARS),
     +         AI_DEFERRED_TAXES(MAX_FINANCIAL_SIMULATION_YEARS))
C
      
      AFUDC_RATE = AI_AFUDC_RATE()
      INTEREST_CAPITIALIZATION_RATE = INTEREST_CAP_RATE()
      UTILITY_IS_TVA = UTILITY_TYPE() == 'T'
      UTILITY_IS_IOU = UTILITY_TYPE() == 'I'
      DO I = 1, NUNITS
         AI_TAX_DEP = 0.
         AI_BOOK_DEP = 0.
         AI_TAX_PREFERRENCE = 0.
         AI_DEFERRED_TAXES = 0.
         ANNUAL_AFUDC = 0.
         AI_INVESTMENT_CLASS = 0.
         AI_EXPENSE_CLASS = 0.
         AI_INVESTMENT = INVESTMENT(I)/1000000.
         IF(AI_REMAINING_LIFE(I) > 0) THEN
            REMAINING_LIFE = FLOAT(AI_REMAINING_LIFE(I))
            IF(ON_LINE(I)/100 <= DATE .AND. OFF_LINE(I)/100 >= DATE)
     +           AI_REMAINING_LIFE(I) = MAX(AI_REMAINING_LIFE(I) - 1,2)
         ELSEIF(AI_REMAINING_LIFE(I) < 0) THEN 
! ADDED FOR SCOTT JONES. GAT. 1/3/95.
            IF(ON_LINE(I)/100 <= DATE .AND. OFF_LINE(I)/100>=DATE) THEN
               POINTER_VALUE = MIN(DATE - MAX(ON_LINE(I)/100-1,
     +                               BASE_YEAR - 1900),AVAIL_DATA_YEARS)
               REMAINING_LIFE = GET_VAR(FLOAT(AI_REMAINING_LIFE(I)),
     +                                         POINTER_VALUE,"A&I CALC")
               IF(REMAINING_LIFE <= 0. .OR. REMAINING_LIFE > 100.) THEN
                  WRITE(4,*) "Remaining Life for Unit ",I,
     +                 " in year ",YEAR+BASE_YEAR," is ",REMAINING_LIFE
                  WRITE(4,*) "This is outside of the valid range "//
     +                                              "for this variable."
                  WRITE(4,*) '*** line 601 FINANBSE.FOR ***'
                  er_message='See WARNING MESSAGES -finanbse.for-1'
                  call end_program(er_message)
               ENDIF
            ELSE 
               REMAINING_LIFE = 0.
            ENDIF
         ELSE
            REMAINING_LIFE = 0.
         ENDIF
         IF(AI_INVESTMENT /= 0.) THEN
            IF(REMAINING_LIFE >= 1.) THEN
               AI_INVESTMENT_CLASS = AI_INVESTMENT
               AI_CASH = AI_CASH + AI_INVESTMENT
               IF(UTILITY_IS_TVA) THEN
C
C TVA ASSUMES ALL A&I CASH IS PUT INTO CWIP AT MID-YEAR
C
                  IF(YEAR == 1)THEN
                     ANNUAL_AFUDC = AI_INVESTMENT * AFUDC_RATE
                  ELSE
                     ANNUAL_AFUDC = (AI_INVESTMENT + 
     +                          LAST_YEAR_INVESTMENT(I)) * AFUDC_RATE/2.
                  ENDIF
               ELSE
                  ANNUAL_AFUDC = AI_INVESTMENT * AFUDC_RATE/2.
               ENDIF
               AI_AFUDC1 = AI_AFUDC1 + ANNUAL_AFUDC 
               AI_BOOK_VALUE = AI_INVESTMENT + ANNUAL_AFUDC
               ANNUAL_DEP = MIN(AI_BOOK_VALUE/REMAINING_LIFE,
     +                                                    AI_BOOK_VALUE)
               DO J = START_DEP_YEAR, MAX_FINANCIAL_SIMULATION_YEARS
                  IF(AI_BOOK_VALUE <= ANNUAL_DEP) THEN
                     AI_BOOK_DEP(J) = AI_BOOK_DEP(J) + AI_BOOK_VALUE
                     EXIT
                  ENDIF
                  AI_BOOK_DEP(J) = AI_BOOK_DEP(J) + ANNUAL_DEP
                  AI_BOOK_VALUE = AI_BOOK_VALUE - ANNUAL_DEP
               ENDDO
            ELSE
               AI_EXPENSE = AI_EXPENSE + AI_INVESTMENT
               AI_EXPENSE_CLASS = AI_INVESTMENT
            ENDIF
C
            IF(UTILITY_IS_IOU) THEN
               AI_TAX_VALUE = AI_INVESTMENT + ANNUAL_AFUDC *
     +                                     INTEREST_CAPITIALIZATION_RATE
               CALL AI_TAX_CALCULATIONS(START_DEP_YEAR,
     +                                  AI_TAX_VALUE,
     +                                  REMAINING_LIFE,
     +                                  AI_TAX_LIFE(I),
     +                                  AI_ADR_TAX_LIFE(I),
     +                                  AI_TAX_DEP,
     +                                  AI_TAX_PREFERRENCE,
     +                                  AI_DEFERRED_TAXES,
     +                                  MAX_FINANCIAL_SIMULATION_YEARS)
            ELSE
               AI_TAX_VALUE = 0.
            ENDIF
C
C ADD ANNUAL VALUES TO TOTAL VALUES
C
            DO YR = START_DEP_YEAR, MAX_FINANCIAL_SIMULATION_YEARS 
               AI_ANNUAL_DEP(YR) = AI_ANNUAL_DEP(YR) + AI_BOOK_DEP(YR)
               AI_ANNUAL_TAX_DEP(YR) = AI_ANNUAL_TAX_DEP(YR) +
     +                                                    AI_TAX_DEP(YR)
               AI_ANNUAL_DEFERRED_TAX(YR) = AI_ANNUAL_DEFERRED_TAX(YR) +
     +                                      AI_DEFERRED_TAXES(YR)         
               AI_ANNUAL_AMT_PREFERENCE_ITEMS(YR) = 
     +                                AI_TAX_PREFERRENCE(YR) +
     +                                AI_ANNUAL_AMT_PREFERENCE_ITEMS(YR)
            ENDDO
C
C CLASS A&I VALUES
C
            ASSET_CLASS = ASSET_CLASS_NUM(I)
            ASSET_ALLOCATION_VECTOR = ASSET_CLASS_VECTOR(I)
C
            IF(ASSET_CLASS < 0) THEN
               CALL GET_ASSET_VAR(ABS(ASSET_CLASS),
     +                                 DUMMY_TYPE,ASSET_CLASS_LIST)
               CALL GET_ASSET_VAR(ABS(ASSET_ALLOCATION_VECTOR),
     +                                 DUMMY_TYPE,ASSET_ALLOCATION_LIST)
            ELSE
               ASSET_CLASS_LIST(1) = ASSET_CLASS
               ASSET_CLASS_LIST(2) = 0.
               ASSET_ALLOCATION_LIST(1) = 100.
               ASSET_ALLOCATION_LIST(2) = 0.
            ENDIF
            CLASS_POINTER = 1
            DO
               ASSET_CLASS = ASSET_CLASS_LIST(CLASS_POINTER)
               CALL CHECK_IF_CLASS_DEFINED(ASSET_CLASS)
               ASSET_CLASS = ASSET_CLASS + 1
               IF(ASSET_CLASS <= 0 .OR. 
     +                               ASSET_CLASS > MAX_AI_CLASSES) CYCLE
               ASSET_ALLOCATOR=ASSET_ALLOCATION_LIST(CLASS_POINTER)/100. 
C
               AI_CLASS_CASH(ASSET_CLASS) = 
     +                           AI_INVESTMENT_CLASS * ASSET_ALLOCATOR +
     +                                        AI_CLASS_CASH(ASSET_CLASS)
               AI_CLASS_EXPENSE(ASSET_CLASS) =
     +                              AI_EXPENSE_CLASS * ASSET_ALLOCATOR +
     +                                     AI_CLASS_EXPENSE(ASSET_CLASS)
               AI_CLASS_AFUDC1(ASSET_CLASS) = ANNUAL_AFUDC *
     +                                                 ASSET_ALLOCATOR +
     +                                      AI_CLASS_AFUDC1(ASSET_CLASS)
C
               DO YR = START_DEP_YEAR, MAX_FINANCIAL_SIMULATION_YEARS 
                  AI_ANNUAL_CLASS_DEP(YR,ASSET_CLASS) =
     +                             AI_ANNUAL_CLASS_DEP(YR,ASSET_CLASS) +
     +                                 AI_BOOK_DEP(YR) * ASSET_ALLOCATOR
                  AI_ANNUAL_CLASS_TAX_DEP(YR,ASSET_CLASS) =
     +                         AI_ANNUAL_CLASS_TAX_DEP(YR,ASSET_CLASS) +
     +                                  AI_TAX_DEP(YR) * ASSET_ALLOCATOR
                  AI_ANNUAL_CLASS_DEFERRED_TAX(YR,ASSET_CLASS) =
     +                    AI_ANNUAL_CLASS_DEFERRED_TAX(YR,ASSET_CLASS) +
     +                           AI_DEFERRED_TAXES(YR) * ASSET_ALLOCATOR
                  AI_ANNUAL_CLASS_AMT_PREF_ITEMS(YR,ASSET_CLASS) =
     +                  AI_ANNUAL_CLASS_AMT_PREF_ITEMS(YR,ASSET_CLASS) +
     +                          AI_TAX_PREFERRENCE(YR) * ASSET_ALLOCATOR
               ENDDO
C
               CLASS_POINTER = CLASS_POINTER + 1
               IF(CLASS_POINTER > AVAIL_DATA_YEARS) EXIT
               IF(ASSET_CLASS_LIST(CLASS_POINTER) == 0. .OR. 
     +                      ASSET_CLASS_LIST(CLASS_POINTER) == -99.)EXIT
            ENDDO ! ASSET CLASSES
C
         ENDIF
         LAST_YEAR_INVESTMENT(I) = AI_INVESTMENT
      ENDDO
      DEALLOCATE(AI_TAX_DEP,
     +           AI_BOOK_DEP,
     +           AI_TAX_PREFERRENCE,
     +           AI_DEFERRED_TAXES)
      RETURN
      END
C***********************************************************************
C*                                                                     *
C*                          D E P T A X                                *
C*                                                                     *
C*          COPYRIGHT (C) 1995 M.S. GERBER & ASSOCIATES, INC.          *
C*                       ALL RIGHTS RESERVED                           *
C*                                                                     *
C***********************************************************************
C*                                                                     *
C*    PURPOSE:                                                         *
C*       DEPTAX CALCULATES THE ANNUAL DEPRECIATION FOR FEDERAL TAXES   *
C*       BASED ON THE ACCELERATED COST RECOVERY SYSTEM (ACRS) AS       *
C*       SPECIFIED IN THE ECONOMIC RECOVERY TAX ACT (ERTA) OF 1981, OR *
C*       THE ASSET DEPRECIATION RANGE (ADR), OR STRAIGHT LINE.         *
C*       THE MEHTOD USED IS SPECIFIED FOR EACH ACCOUNT                 *
C*                                                                     *
C***********************************************************************
C
      SUBROUTINE AI_TAX_CALCULATIONS(START_DEP_YEAR,
     +                               TAX_VALUE,
     +                               REMAINING_LIFE,
     +                               TAX_LIFE,
     +                               ADR_TAX_LIFE,
     +                               AI_TAX_DEP,
     +                               AI_TAX_PREFERRENCE,
     +                               AI_DEFERRED_TAXES,
     +                               FINANCIAL_SIMULATION_YEARS)
C
C     INTERNAL VARIABLES
      INCLUDE 'SPINLIB.MON'
      LOGICAL*1 NO_SWITCH
      INTEGER*2 L,M,START_DEP_YEAR
      INTEGER*2 FINANCIAL_SIMULATION_YEARS
      REAL*4 TAX_LIFE,DB_RATE,TOTAL_TAX_DEP,DEPDB,DEPSL,ADR_TAX_LIFE
      REAL*4 SL_OFFSET
      REAL*4 NPV_4_TAX,NPV_4_BOOK,REMAINING_LIFE
      INTEGER*4 VALUES_2_ZERO
C     EXTERNAL VARIABLES
      CHARACTER*4 DEPMET
      REAL*4 AI_TAX_DEP(FINANCIAL_SIMULATION_YEARS),
     +     AI_TAX_PREFERRENCE(FINANCIAL_SIMULATION_YEARS),
     +     AI_DEFERRED_TAXES(FINANCIAL_SIMULATION_YEARS),
     +     TAX_VALUE,TAX_DEP_SL,TOTAL_BOOK_DEP
C
      AI_TAX_DEP = 0.
      AI_TAX_PREFERRENCE = 0.
      AI_DEFERRED_TAXES = 0.
C
C     IF THE TAX LIFE IS GREATER THAN 98 YEARS, IT IS ASSUMED
C     THAT NO TAX DEPRECIATION WILL OCURR
C
      IF(TAX_VALUE <= 0.) RETURN
C
      SL_OFFSET = 0.
      DEPMET = 'MACR'
      IF(TAX_LIFE <= 1.0) THEN
         AI_TAX_DEP(START_DEP_YEAR) = TAX_VALUE
	ELSEIF(TAX_LIFE < 99.) THEN
C
C TAX DEPRECIATION
C
         DB_RATE = 1.5
         IF(TAX_LIFE <= 20.) DB_RATE = 2.
         SL_OFFSET = .5
C        SL_OFFSET = MACRS_TABLE(ADR_TAX_LIFE,DB_RATE,TAX_LIFE)
         NO_SWITCH = INDEX(DEPMET,'DBNS') /= 0
C
         NPV_4_TAX = TAX_VALUE/2.
         TOTAL_TAX_DEP = 0.
         L = START_DEP_YEAR
         DOWHILE (L <= FINANCIAL_SIMULATION_YEARS .AND. NPV_4_TAX > 0.)
            DEPDB = DB_RATE * NPV_4_TAX/TAX_LIFE
            DEPSL=NPV_4_TAX/(TAX_LIFE-FLOAT(L-START_DEP_YEAR)+SL_OFFSET)
            IF(DEPDB > DEPSL .OR. NO_SWITCH) THEN
C THEN TAKE THE DB VALUE FOR THE EXPENSE
               AI_TAX_DEP(L) = AI_TAX_DEP(L) + DEPDB
               TOTAL_TAX_DEP =  TOTAL_TAX_DEP + DEPDB
               NPV_4_TAX = TAX_VALUE - TOTAL_TAX_DEP
C
C ELSE TAKE THE STRAIGHT LINE UNTIL NPV IS ZERO
            ELSE
               DO M = L, FINANCIAL_SIMULATION_YEARS
                  IF(NPV_4_TAX < DEPSL) THEN
                     AI_TAX_DEP(M) = AI_TAX_DEP(M) + NPV_4_TAX
                     NPV_4_TAX = 0.
                     EXIT
                  ENDIF
                  AI_TAX_DEP(M) = AI_TAX_DEP(M) + DEPSL
                  NPV_4_TAX = NPV_4_TAX - DEPSL
                  IF(NPV_4_TAX < .0001) NPV_4_TAX = 0.
               ENDDO
               EXIT
            ENDIF
           L = L + 1
         ENDDO
      ENDIF
C
C DEFERRED TAX CALCULATION
C
	IF(REMAINING_LIFE > 0. .AND. REMAINING_LIFE < 99.) THEN
         TAX_DEP_SL = MIN(TAX_VALUE/REMAINING_LIFE,TAX_VALUE)
         AI_DEFERRED_TAXES(START_DEP_YEAR) = AI_TAX_DEP(START_DEP_YEAR)-
     +                                                     TAX_DEP_SL/2.
         TOTAL_BOOK_DEP = TAX_DEP_SL/2.
         NPV_4_BOOK = TAX_VALUE - TOTAL_BOOK_DEP 
         L = START_DEP_YEAR + 1
C
         DOWHILE (L <= FINANCIAL_SIMULATION_YEARS .AND. NPV_4_BOOK > 0.)
            TAX_DEP_SL = MIN(TAX_DEP_SL,NPV_4_BOOK)
            AI_DEFERRED_TAXES(L) = AI_TAX_DEP(L) - TAX_DEP_SL
            TOTAL_BOOK_DEP = TOTAL_BOOK_DEP + TAX_DEP_SL
            NPV_4_BOOK = MAX(TAX_VALUE - TOTAL_BOOK_DEP,0.)
            IF(NPV_4_BOOK < .0001) NPV_4_BOOK = 0.
            L = L + 1
         ENDDO
      ENDIF
C
C AMT PREFERENCE CALCULATION 
C
      IF(ADR_TAX_LIFE <= 1.0 .OR. ADR_TAX_LIFE > 98.) THEN
         IF(ADR_TAX_LIFE <= 1.0) THEN
            AI_TAX_PREFERRENCE(START_DEP_YEAR) =   
     +                            AI_TAX_DEP(START_DEP_YEAR) - TAX_VALUE
         ELSE
            AI_TAX_PREFERRENCE(START_DEP_YEAR) =
     +                                        AI_TAX_DEP(START_DEP_YEAR)  
         ENDIF
         L = START_DEP_YEAR + 1
         DO L = START_DEP_YEAR+1, FINANCIAL_SIMULATION_YEARS
            AI_TAX_PREFERRENCE(L) = AI_TAX_DEP(L)
         ENDDO 
      ELSE
         DB_RATE = 1.5
         IF(INDEX(DEPMET,'MACR') /= 0) SL_OFFSET = .5
         NO_SWITCH = INDEX(DEPMET,'DBNS') /= 0
C
         NPV_4_TAX = TAX_VALUE/2.
         TOTAL_TAX_DEP = 0.
         L = START_DEP_YEAR
         DOWHILE (L <= FINANCIAL_SIMULATION_YEARS .AND. NPV_4_TAX > 0.)
            DEPDB = DB_RATE * NPV_4_TAX/ADR_TAX_LIFE
            DEPSL = NPV_4_TAX/
     +                  (ADR_TAX_LIFE-FLOAT(L-START_DEP_YEAR)+SL_OFFSET)
            IF(DEPDB > DEPSL .OR. NO_SWITCH) THEN
C THEN TAKE THE DB VALUE FOR THE EXPENSE
               AI_TAX_PREFERRENCE(L) = AI_TAX_DEP(L) - DEPDB
               TOTAL_TAX_DEP =  TOTAL_TAX_DEP + DEPDB
               NPV_4_TAX = TAX_VALUE - TOTAL_TAX_DEP
C
C ELSE TAKE THE STRAIGHT LINE UNTIL NPV IS ZERO
            ELSE
               DO M = L, FINANCIAL_SIMULATION_YEARS
                  IF(NPV_4_TAX < DEPSL) THEN
                     AI_TAX_PREFERRENCE(M) = AI_TAX_DEP(M) - NPV_4_TAX
                     NPV_4_TAX = 0.
                     EXIT
                  ENDIF
                  AI_TAX_PREFERRENCE(M) = AI_TAX_DEP(M) - DEPSL
                  NPV_4_TAX = NPV_4_TAX - DEPSL
                  IF(NPV_4_TAX < .0001) NPV_4_TAX = 0.
               ENDDO
               EXIT
            ENDIF
            L = L + 1
         ENDDO
      ENDIF
      RETURN
      END
C**********************************************************************
C
C             THE ADDITIONS & IMPROVMENTS ANALYSIS MODULE FOR TVA
C                         EXTERNAL PRODUCTION COSTS
C                             COPYRIGHT (C) 1994
C                        M.S. GERBER & ASSOCIATES, INC.
C                              ALL RIGHTS RESERVED
C
C**********************************************************************
C
      SUBROUTINE EXTERNAL_AI
      use end_routine, only: end_program, er_message
C
      INCLUDE 'SpinLib.MON'
      INCLUDE 'SIZECOM.MON'
C     INCLUDE 'GLOBECOM.MON'
      INCLUDE 'PROD2COM.MON'
C
C     REAL CL_AI_INVESTMENT,EL_AI_INVESTMENT
C     COMMON/AITVAFIN/CL_AI_INVESTMENT(MAX_CL_UNITS),
C    +                EL_AI_INVESTMENT(MAX_EL_UNITS)
C
C     INTEGER*2 OFLINE,OFF_LINE,NUNITS,HYDRO_UNITS,
C    +            AI_CL_REMAINING_LIFE,
C    +            AI_EL_REMAINING_LIFE,
C    +            ONLINE,ON_LINE
C     REAL CAP_FRAC_OWN,CAP_PLANNING_FAC
C     COMMON/FOSHYD/ OFLINE(MAX_CL_UNITS),OFF_LINE(MAX_EL_UNITS),
C    +               NUNITS,HYDRO_UNITS,
C    +               AI_CL_REMAINING_LIFE(MAX_CL_UNITS),
C    +               AI_EL_REMAINING_LIFE(MAX_EL_UNITS),
C    +               ONLINE(MAX_CL_UNITS),ON_LINE(MAX_EL_UNITS),
C    +               CAP_FRAC_OWN(MAX_CL_UNITS),
C    +               CAP_PLANNING_FAC(MAX_CL_UNITS)
C     
      CHARACTER*1 UTILITY_TYPE
      CHARACTER*4 FILE_EXTENSION
      CHARACTER*5 SCENARIO_NAME,GET_SCENAME_AND_EXT_AI_NUM
      CHARACTER*64 FILE_NAME,BASE_FILE_DIRECTORY
      CHARACTER*1024 RECLN
      INTEGER*2 EXTERNAL_AI_NUM
      INTEGER*4 IOS
      LOGICAL*4 FILE_EXISTS
c      
      INTEGER*2 OTHER_YEAR,OTHER_ID_NUMBER,MAX_CL_ID_NUM,MAX_EL_ID_NUM,
     +         TEMP_NUNITS,TEMP_HYDRO_UNITS
      CHARACTER*1 OTHER_UNIT_TYPE
      REAL*4 OTHER_ANNUAL_GWH,
     +     OTHER_AI_ENERGY_RATE,
     +     OTHER_ANNUAL_MW_MONTHS,
     +     OTHER_AI_CAPACITY_RATE,
     +     OTHER_REMAINING_LIFE
C     
      SAVE EXTERNAL_AI_NUM,
     +         MAX_CL_ID_NUM,
     +         MAX_EL_ID_NUM,
     +         TEMP_NUNITS,
     +         TEMP_HYDRO_UNITS,
     +         OTHER_YEAR, 
     +         OTHER_UNIT_TYPE,
     +         OTHER_ID_NUMBER,
     +         OTHER_ANNUAL_GWH,
     +         OTHER_AI_ENERGY_RATE,
     +         OTHER_ANNUAL_MW_MONTHS,
     +         OTHER_AI_CAPACITY_RATE,
     +         OTHER_REMAINING_LIFE
C      
! END OF DECLARATIONS     
! GET THE FILE NAME
      ENTRY OPEN_EXTERNAL_AI_FILE
         IF(END_POINT < 10 .AND. END_POINT > 0) THEN
            FILE_EXTENSION = ".00"//CHAR(END_POINT+48)
         ELSEIF(END_POINT < 100) THEN
            FILE_EXTENSION = ".0"//CHAR(END_POINT+48)
         ELSEIF(END_POINT < 1000) THEN
            FILE_EXTENSION = "."//CHAR(END_POINT+48)
         ELSE
            WRITE(4,*) "Endpoint Number Too High For External A&I"
            WRITE(4,*) "Production Data File.  Endpoint =",END_POINT
            WRITE(4,*) '*** line 988NANBSE.FOR ***'
            er_message='See WARNING MESSAGES -finanbse.for-2'
            call end_program(er_message)
         ENDIF
         SCENARIO_NAME = 
     +             GET_SCENAME_AND_EXT_AI_NUM(EXTERNAL_AI_NUM)
         FILE_NAME = trim(BASE_FILE_DIRECTORY())//
     +                      "A&I"//trim(SCENARIO_NAME)//FILE_EXTENSION
         INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
         IF(FILE_EXISTS) THEN
            OPEN(EXTERNAL_AI_NUM,FILE=FILE_NAME)
         ELSEIF(UTILITY_TYPE() == 'T') THEN
            WRITE(4,*) "External A&I Production Data File",FILE_NAME
            WRITE(4,*) "Not In Current Project Directory"
            WRITE(4,*) '*** line 1000 FINANBSE.FOR ***'
            er_message='See WARNING MESSAGES -finanbse.for-3'
            call end_program(er_message)
         ENDIF
      RETURN
C    
      ENTRY EXTERNAL_AI_ANALYSIS
C
         MAX_CL_ID_NUM = 0
         MAX_EL_ID_NUM = 0
         TEMP_NUNITS = NUNITS
         TEMP_HYDRO_UNITS = HYDRO_UNITS
C         
         IF(YEAR==1) THEN
C
            AI_CL_REMAINING_LIFE = 0
            AI_EL_REMAINING_LIFE = 0
C
            READ(EXTERNAL_AI_NUM,1000,IOSTAT=IOS) RECLN
            IF(IOS /=0) THEN
               WRITE(4,*)"Error Reading External A&I Production File"
               WRITE(4,*) "At First Record In Year=",BASE_YEAR+YEAR
               WRITE(4,*) '*** line 1021 FINANBSE.FOR ***'
               er_message='See WARNING MESSAGES -finanbse.for-4'
               call end_program(er_message)
            ENDIF
            RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
            READ(RECLN,*,ERR=200) 
     +         OTHER_YEAR, 
     +         OTHER_UNIT_TYPE,
     +         OTHER_ID_NUMBER,
     +         OTHER_ANNUAL_GWH,
     +         OTHER_AI_ENERGY_RATE,
     +         OTHER_ANNUAL_MW_MONTHS,
     +         OTHER_AI_CAPACITY_RATE,
     +         OTHER_REMAINING_LIFE
         ENDIF
C
         DO
            IF(OTHER_YEAR == BASE_YEAR+YEAR) THEN
               IF(OTHER_UNIT_TYPE == 'C') THEN
                  CL_AI_INVESTMENT(OTHER_ID_NUMBER) = 
     +                               CL_AI_INVESTMENT(OTHER_ID_NUMBER) + 
     +                         OTHER_AI_ENERGY_RATE * OTHER_ANNUAL_GWH +
     +                         1000. * OTHER_ANNUAL_MW_MONTHS *
     +                                            OTHER_AI_CAPACITY_RATE
                  AI_CL_REMAINING_LIFE(OTHER_ID_NUMBER) = 
     +                                        NINT(OTHER_REMAINING_LIFE)
                  MAX_CL_ID_NUM = MAX(MAX_CL_ID_NUM,OTHER_ID_NUMBER)
               ELSE
                  EL_AI_INVESTMENT(OTHER_ID_NUMBER) = 
     +                               EL_AI_INVESTMENT(OTHER_ID_NUMBER) + 
     +                         OTHER_AI_ENERGY_RATE * OTHER_ANNUAL_GWH +
     +                         1000. * OTHER_ANNUAL_MW_MONTHS *
     +                                            OTHER_AI_CAPACITY_RATE
                  AI_EL_REMAINING_LIFE(OTHER_ID_NUMBER) = 
     +                                        NINT(OTHER_REMAINING_LIFE)
                  MAX_EL_ID_NUM = MAX(MAX_EL_ID_NUM,OTHER_ID_NUMBER)
               ENDIF
            ELSE
               EXIT
            ENDIF
C
            READ(EXTERNAL_AI_NUM,1000,IOSTAT=IOS) RECLN
            IF(IOS /=0) THEN
               IF(BASE_YEAR+YEAR == LAST_STUDY_YEAR
     +                                     .AND. MAX_CL_ID_NUM > 0) THEN
                  EXIT
               ELSE
                  WRITE(4,*)"Error Reading External A&I Production File"
                  WRITE(4,*) "Record In Year=",BASE_YEAR+YEAR
                  WRITE(4,*) '*** line 1070 FINANBSE.FOR ***'
                  er_message='See WARNING MESSAGES -finanbse.for-5'
                  call end_program(er_message)
               ENDIF
            ENDIF
            RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
            READ(RECLN,*,ERR=200) 
     +         OTHER_YEAR, 
     +         OTHER_UNIT_TYPE,
     +         OTHER_ID_NUMBER,
     +         OTHER_ANNUAL_GWH,
     +         OTHER_AI_ENERGY_RATE,
     +         OTHER_ANNUAL_MW_MONTHS,
     +         OTHER_AI_CAPACITY_RATE,
     +         OTHER_REMAINING_LIFE
C            
         ENDDO
      RETURN
      ENTRY RESET_UNIT_COUNT_TO_PSYM
         NUNITS = MAX_CL_ID_NUM
         HYDRO_UNITS = MAX_EL_ID_NUM
      RETURN
      ENTRY RESET_UNIT_COUNT_TO_MIDAS
         NUNITS = TEMP_NUNITS
         HYDRO_UNITS = TEMP_HYDRO_UNITS
      RETURN
!      
  200 WRITE(4,*) "Cannot Read External A&I Production File"
      WRITE(4,*) '*** line 1099 FINANBSE.FOR ***'
      er_message='See WARNING MESSAGES -finanbse.for-6'
      call end_program(er_message)
 1000 FORMAT(A)
      END
C**********************************************************************
C
C                             DEFERRED EXPENSES 
C                             COPYRIGHT (C) 1992
C                        M.S. GERBER & ASSOCIATES, INC.
C                              ALL RIGHTS RESERVED
C
C**********************************************************************
C
      SUBROUTINE DEFERRED_EXPENSES(RUN_YEAR,DEFERRED_EXPENSE_AMORT,
     +                             DEFERRED_EXPENSE_CASH,
     +                             DEFERRED_EXPENSE_AMORT_PERIOD)
C
      INCLUDE 'SpinLib.MON'
      INCLUDE 'SIZECOM.MON'
      INCLUDE 'KEPCOCOM.MON'
      INTEGER*2 RUN_YEAR,I
      REAL*4 DEFERRED_EXPENSE_AMORT_PERIOD,
     +     DEFERRED_EXPENSE_CASH,
     +     DEFERRED_EXPENSE_AMORT,
     +     AMORT_PERIOD,
     +     CURRENT_AMORT
      REAL*4 ANNUAL_AMORT,UNAMORT_DEFERRED_EXPENSE
      REAL*4 ANNUAL_AMORTIZATION(MAX_FINANCIAL_SIMULATION_YEARS)
      SAVE ANNUAL_AMORTIZATION
C
      IF(RUN_YEAR == 1) ANNUAL_AMORTIZATION = 0.
      AMORT_PERIOD = DEFERRED_EXPENSE_AMORT_PERIOD
      IF(AMORT_PERIOD <= .5) THEN
         ANNUAL_AMORTIZATION(RUN_YEAR) = ANNUAL_AMORTIZATION(RUN_YEAR) +
     +                                             DEFERRED_EXPENSE_CASH
      ELSE IF(AMORT_PERIOD <= 1.) THEN
         ANNUAL_AMORTIZATION(RUN_YEAR) = ANNUAL_AMORTIZATION(RUN_YEAR) +
     +                                          DEFERRED_EXPENSE_CASH/2.
         ANNUAL_AMORTIZATION(RUN_YEAR+1) = DEFERRED_EXPENSE_CASH/2. +
     +                                   ANNUAL_AMORTIZATION(RUN_YEAR+1)
      ELSE IF(AMORT_PERIOD < 98.) THEN
         ANNUAL_AMORT = DEFERRED_EXPENSE_CASH/AMORT_PERIOD
         UNAMORT_DEFERRED_EXPENSE=DEFERRED_EXPENSE_CASH-ANNUAL_AMORT/2.
         ANNUAL_AMORTIZATION(RUN_YEAR) = ANNUAL_AMORTIZATION(RUN_YEAR) +
     +                                                   ANNUAL_AMORT/2.
         I = RUN_YEAR + 1
         DOWHILE (UNAMORT_DEFERRED_EXPENSE > 0. .AND. 
     +                              I <= MAX_FINANCIAL_SIMULATION_YEARS)
            CURRENT_AMORT = MIN(UNAMORT_DEFERRED_EXPENSE,ANNUAL_AMORT)
            UNAMORT_DEFERRED_EXPENSE = UNAMORT_DEFERRED_EXPENSE -
     +                                                     CURRENT_AMORT
            ANNUAL_AMORTIZATION(I) = ANNUAL_AMORTIZATION(I) +
     +                                                     CURRENT_AMORT
            I = I + 1
         ENDDO
      ENDIF
      DEFERRED_EXPENSE_CASH = DEFERRED_EXPENSE_CASH +
     +                                      KEPCO_WC_DEF_MAINT_ENRG_COST
C
C 12/20/92 THE FOLLOWING LOGIC IS FOR KEPCO IT ASSUMES THAT SOME KEPCO
C DEFERRED WOLF CREEK MAINTENANCE ENERGY EXPENSE FOR THE FIRST TWO YEARS 
C IS FROM THE FINANCIAL PARAMETER FILE WHERE IT IS READ IN AS
C DEFERRED_EXPENSE_AMORT
C
      KEPCO_WC_DEF_MAINT_ENRG_AMORT = DEFERRED_EXPENSE_AMORT +
     +                                     KEPCO_WC_DEF_MAINT_ENRG_AMORT
      DEFERRED_EXPENSE_AMORT = ANNUAL_AMORTIZATION(RUN_YEAR) +
     +                                     KEPCO_WC_DEF_MAINT_ENRG_AMORT
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE EXTERNAL_PRODUCTION_RESULTS
      use end_routine, only: end_program, er_message
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      INCLUDE 'SpinLib.MON'
      INCLUDE 'SIZECOM.MON'
      INCLUDE 'GLOBECOM.MON'
         REAL*4  
     +         OTHER_YEAR,
     +         OTHER_FOSSIL_CAPACITY,
     +         OTHER_FOSSIL_GENERATION,
     +         OTHER_FOSSIL_INPUT,
     +         OTHER_FOSSIL_FUEL,
     +         OTHER_FOSSIL_VAROM,
     +         OTHER_FOSSIL_FIXOM,
     +         OTHER_NUC_CAPACITY,
     +         OTHER_NUC_GENERATION,
     +         OTHER_NUC_INPUT,
     +         OTHER_NUC_OWNED,
     +         OTHER_NUC_LEASED,
     +         OTHER_NUC_VAROM,
     +         OTHER_NUC_FIXOM,
     +         OTHER_HYDRO_CAPACITY,
     +         OTHER_HYDRO_GENERATION,
     +         OTHER_HYDRO_VAROM,
     +         OTHER_HYDRO_FIXOM,
     +         OTHER_STORAGE_PUMP,
     +         OTHER_STORAGE_GENERATION,
     +         OTHER_STORAGE_VAROM,
     +         OTHER_STORAGE_FIXOM,
     +         OTHER_PURCHASE_GENERATION,
     +         OTHER_PURCHASE_COST,
     +         OTHER_ECONOMY_PURCHASE,
     +         OTHER_ECONOMY_SALES,
     +         OTHER_ECONOMY_COST,
     +         OTHER_ECONOMY_REVENUE,
     +         OTHER_SECONDARY_SALES,
     +         OTHER_SECONDARY_SALES_REVENUES,
     +         OTHER_DSM_ENERGY_MODIFICATION,
     +         OTHER_DSM_PEAK_MODIFICATION,
     +         OTHER_DSM_EXPENSE_WO_REBATES,
     +         OTHER_DSM_REBATE_EXPENSE,
     +         OTHER_T_AND_D_EXPENSE,
     +         OTHER_EMISSIONS(TOTAL_EMISSION_GROUPS),
     +         OTHER_PEAK_BEFORE_DSM,
     +         OTHER_ANNUAL_ENERGY,
C     
     +         MIDAS_FUEL_COST,
     +         MIDAS_VAROM,
     +         MIDAS_FIXOM,
     +         MIDAS_NUC_OWNED,
     +         MIDAS_NUC_LEASED,
     +         MIDAS_PURCHASE_COST,
     +         MIDAS_SALES_REVENUES,
     +         MIDAS_DSM_EXPENSE,
     +         MIDAS_DSM_REBATES,
     +         MIDAS_OTHER3_EXPENSE
         SAVE
     +         OTHER_YEAR,
     +         OTHER_FOSSIL_CAPACITY,
     +         OTHER_FOSSIL_GENERATION,
     +         OTHER_FOSSIL_INPUT,
     +         OTHER_FOSSIL_FUEL,
     +         OTHER_FOSSIL_VAROM,
     +         OTHER_FOSSIL_FIXOM,
     +         OTHER_NUC_CAPACITY,
     +         OTHER_NUC_GENERATION,
     +         OTHER_NUC_INPUT,
     +         OTHER_NUC_OWNED,
     +         OTHER_NUC_LEASED,
     +         OTHER_NUC_VAROM,
     +         OTHER_NUC_FIXOM,
     +         OTHER_HYDRO_CAPACITY,
     +         OTHER_HYDRO_GENERATION,
     +         OTHER_HYDRO_VAROM,
     +         OTHER_HYDRO_FIXOM,
     +         OTHER_STORAGE_PUMP,
     +         OTHER_STORAGE_GENERATION,
     +         OTHER_STORAGE_VAROM,
     +         OTHER_STORAGE_FIXOM,
     +         OTHER_PURCHASE_GENERATION,
     +         OTHER_PURCHASE_COST,
     +         OTHER_ECONOMY_PURCHASE,
     +         OTHER_ECONOMY_SALES,
     +         OTHER_ECONOMY_COST,
     +         OTHER_ECONOMY_REVENUE,
     +         OTHER_SECONDARY_SALES,
     +         OTHER_SECONDARY_SALES_REVENUES,
     +         OTHER_DSM_ENERGY_MODIFICATION,
     +         OTHER_DSM_PEAK_MODIFICATION,
     +         OTHER_DSM_EXPENSE_WO_REBATES,
     +         OTHER_DSM_REBATE_EXPENSE,
     +         OTHER_T_AND_D_EXPENSE,
     +         OTHER_EMISSIONS,
     +         OTHER_PEAK_BEFORE_DSM,
     +         OTHER_ANNUAL_ENERGY
C     
      CHARACTER*4 FILE_EXTENSION
      CHARACTER*5 SCENARIO_NAME,GET_SCENAME_AND_EXT_PROD_NUM
      CHARACTER*64 FILE_NAME,BASE_FILE_DIRECTORY
      CHARACTER*1024 RECLN
      INTEGER*2 EXTERNAL_PRODUCTION_NUM,YR
      INTEGER*4 IOS
      LOGICAL*4 FILE_EXISTS
      SAVE EXTERNAL_PRODUCTION_NUM
! END OF DECLARATIONS     
! GET THE FILE NAME
      ENTRY OPEN_EXTERNAL_PRODUCTION_FILE
         IF(END_POINT < 10 .AND. END_POINT > 0) THEN
            FILE_EXTENSION = ".00"//CHAR(END_POINT+48)
         ELSEIF(END_POINT < 100) THEN
            FILE_EXTENSION = ".0"//CHAR(END_POINT+48)
         ELSEIF(END_POINT < 1000) THEN
            FILE_EXTENSION = "."//CHAR(END_POINT+48)
         ELSE
            WRITE(4,*) "Endpoint Number Too High For External"
            WRITE(4,*) "Production Data File.  Endpoint =",END_POINT
            WRITE(4,*) '*** line 1286 FINANBSE.FOR ***'
            er_message='See WARNING MESSAGES -finanbse.for-7'
            call end_program(er_message)
         ENDIF
         SCENARIO_NAME = 
     +             GET_SCENAME_AND_EXT_PROD_NUM(EXTERNAL_PRODUCTION_NUM)
         FILE_NAME = trim(BASE_FILE_DIRECTORY())//
     +                      "ANN"//trim(SCENARIO_NAME)//FILE_EXTENSION
         INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
         IF(FILE_EXISTS) THEN
            OPEN(EXTERNAL_PRODUCTION_NUM,FILE=FILE_NAME)
         ELSE
            WRITE(4,*) "External Production Data File",FILE_NAME
            WRITE(4,*) "Not In Current Project Directory"
            WRITE(4,*) '*** line 1299 FINANBSE.FOR ***'
            er_message='See WARNING MESSAGES -finanbse.for-8'
            call end_program(er_message)
         ENDIF
      RETURN
!     
      ENTRY UPDATE_EXTERNAL_PRODUCTION(YR,
     +                                 MIDAS_FUEL_COST,
     +                                 MIDAS_VAROM,
     +                                 MIDAS_FIXOM,                     
     +                                 MIDAS_NUC_OWNED,
     +                                 MIDAS_NUC_LEASED,
     +                                 MIDAS_PURCHASE_COST,
     +                                 MIDAS_SALES_REVENUES,
     +                                 MIDAS_DSM_EXPENSE,
     +                                 MIDAS_DSM_REBATES,
     +                                 MIDAS_OTHER3_EXPENSE)
         READ(EXTERNAL_PRODUCTION_NUM,1000,IOSTAT=IOS) RECLN
         IF(IOS /=0) THEN
            WRITE(4,*) "Error Reading External Production File"
            WRITE(4,*) "Record In Year=",YR
            WRITE(4,*) '*** line 1219 FINANBSE.FOR ***'
            er_message='See WARNING MESSAGES -finanbse.for-9'
            call end_program(er_message)
         ENDIF
         RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
         READ(RECLN,*,ERR=200) 
     +         OTHER_YEAR,
     +         OTHER_FOSSIL_CAPACITY,
     +         OTHER_FOSSIL_GENERATION,
     +         OTHER_FOSSIL_INPUT,
     +         OTHER_FOSSIL_FUEL,
     +         OTHER_FOSSIL_VAROM,
     +         OTHER_FOSSIL_FIXOM,
     +         OTHER_NUC_CAPACITY,
     +         OTHER_NUC_GENERATION,
     +         OTHER_NUC_INPUT,
     +         OTHER_NUC_OWNED,
     +         OTHER_NUC_LEASED,
     +         OTHER_NUC_VAROM,
     +         OTHER_NUC_FIXOM,
     +         OTHER_HYDRO_CAPACITY,
     +         OTHER_HYDRO_GENERATION,
     +         OTHER_HYDRO_VAROM,
     +         OTHER_HYDRO_FIXOM,
     +         OTHER_STORAGE_PUMP,
     +         OTHER_STORAGE_GENERATION,
     +         OTHER_STORAGE_VAROM,
     +         OTHER_STORAGE_FIXOM,
     +         OTHER_PURCHASE_GENERATION,
     +         OTHER_PURCHASE_COST,
     +         OTHER_ECONOMY_PURCHASE,
     +         OTHER_ECONOMY_SALES,
     +         OTHER_ECONOMY_COST,
     +         OTHER_ECONOMY_REVENUE,
     +         OTHER_SECONDARY_SALES,
     +         OTHER_SECONDARY_SALES_REVENUES,
     +         OTHER_DSM_ENERGY_MODIFICATION,
     +         OTHER_DSM_PEAK_MODIFICATION,
     +         OTHER_DSM_EXPENSE_WO_REBATES,
     +         OTHER_DSM_REBATE_EXPENSE,
     +         OTHER_T_AND_D_EXPENSE,
     +         OTHER_EMISSIONS,
     +         OTHER_PEAK_BEFORE_DSM,
     +         OTHER_ANNUAL_ENERGY
         IF(INT(OTHER_YEAR) /= YR+BASE_YEAR) THEN
            WRITE(4,*) "External Production File Year =",INT(OTHER_YEAR)
            WRITE(4,*) "Is Inconsistent With Model Year =",YR+BASE_YEAR
            WRITE(4,*) '*** line 1365 FINANBSE.FOR ***'
            er_message='See WARNING MESSAGES -finanbse.for-10'
            call end_program(er_message)
         ENDIF
         MIDAS_FUEL_COST = MIDAS_FUEL_COST + OTHER_FOSSIL_FUEL
         MIDAS_VAROM = MIDAS_VAROM + OTHER_FOSSIL_VAROM + 
     +                  OTHER_NUC_VAROM +
     +                           OTHER_HYDRO_VAROM + OTHER_STORAGE_VAROM
         MIDAS_FIXOM = MIDAS_FIXOM + 
     +               OTHER_FOSSIL_FIXOM + OTHER_NUC_FIXOM +
     +                           OTHER_HYDRO_FIXOM + OTHER_STORAGE_FIXOM
         MIDAS_NUC_OWNED = MIDAS_NUC_OWNED + OTHER_NUC_OWNED
         MIDAS_NUC_LEASED = MIDAS_NUC_LEASED + OTHER_NUC_LEASED
         MIDAS_PURCHASE_COST = MIDAS_PURCHASE_COST + 
     +                                             OTHER_PURCHASE_COST + 
     +                                             OTHER_ECONOMY_COST -
     +                                             OTHER_ECONOMY_REVENUE
         MIDAS_SALES_REVENUES = MIDAS_SALES_REVENUES +
     +                                    OTHER_SECONDARY_SALES_REVENUES
         MIDAS_DSM_EXPENSE = MIDAS_DSM_EXPENSE + 
     +                                      OTHER_DSM_EXPENSE_WO_REBATES
         MIDAS_DSM_REBATES = MIDAS_DSM_REBATES + 
     +                                          OTHER_DSM_REBATE_EXPENSE
         MIDAS_OTHER3_EXPENSE = MIDAS_OTHER3_EXPENSE +
     +                                             OTHER_T_AND_D_EXPENSE 
      RETURN
!      
  200 WRITE(4,*) "Cannot Read External Production File"
      WRITE(4,*) '*** line 1393 FINANBSE.FOR ***'
      er_message='See WARNING MESSAGES -finanbse.for-11'
      call end_program(er_message)
 1000 FORMAT(A)
      END


