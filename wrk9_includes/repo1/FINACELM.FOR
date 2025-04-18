!     ******************************************************************
!     FINACELM.FOR
!     Copyright(c)  2000
!
!     Created: 9/15/2003 3:09:25 PM
!     Author : MARK S GERBER
!     Last change: MSG 8/23/2009 4:41:18 PM
!     ******************************************************************

C***********************************************************************
      SUBROUTINE ELIMINATIONS_OBJECT()
      use end_routine, only: end_program, er_message
C***********************************************************************
C      
      INCLUDE 'SpinLib.MON'
      USE SIZECOM
      use globecom

      LOGICAL*1 RETURN_A_VECTOR_FOR_ALL_YEARS,VOID_LOGICAL
      INTEGER IOS_BASE,IOS
      INTEGER*4 VALUES_2_ZERO
      INTEGER*2 INUNIT,IREC,DELETE,LRECL/384/,
     +          I,L,POINTER
      CHARACTER*5 BASE_FILE_NAME,OVERLAY_FAMILY_NAME,ELIM_FILE
C     CHARACTER*50 COMMENT
      CHARACTER*256 FILE_NAME
      CHARACTER*256 BASE_FILE_DIRECTORY,OUTPUT_DIRECTORY
      CHARACTER*256 DATA_DRIVE
      CHARACTER*30 CLASSIFICATIONS(:)
      INTEGER*2 SINGLE_YEAR(:),MULTI_YEAR_ESCALAITON_VECTOR(:)
      REAL*4 SINGLE_AMOUNT(:)
      ALLOCATABLE :: CLASSIFICATIONS,SINGLE_YEAR,
     +               SINGLE_AMOUNT,MULTI_YEAR_ESCALAITON_VECTOR
      LOGICAL*4 FILE_EXISTS,ELIMINATION_FILE_EXISTS
      SAVE ELIMINATION_FILE_EXISTS
C DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER*2048 RECLN
      CHARACTER*20 FILE_TYPE/'Eliminations'/
C
      CHARACTER*2 ELIMINATION_OL/'BC'/
C
C CALCULATE CREDIT AND DEBIT INFORMATION
C
      INTEGER*2 NUM_OF_DEBITS_AND_CREDITS,MAX_ELIMINATION_NUM,
     +          R_MAX_ELIMINATION_NUM,NUM_OF_DEBITS
      PARAMETER (NUM_OF_DEBITS_AND_CREDITS=8,NUM_OF_DEBITS=4)
      SAVE MAX_ELIMINATION_NUM
      REAL ANNUAL_DATA(:),YEARLY_ELIMINATIONS_VALUES(:,:)
      ALLOCATABLE :: ANNUAL_DATA,YEARLY_ELIMINATIONS_VALUES
      SAVE YEARLY_ELIMINATIONS_VALUES
      INTEGER*2 R_YEAR
      REAL R_ELIMINATIONS_VALUES(0:*)
      REAL*4 ACCOUNTING_SIGN
      LOGICAL*1 IS_A_DEBIT,NON_CASH_ITEM
      LOGICAL*1 LAHEY_LF95
      CHARACTER*30 SCREEN_OUTPUT
C***********************************************************************
C
      RETURN
C***********************************************************************
      ENTRY EM_MAKEBIN
C***********************************************************************
C
         BASE_FILE_NAME = ELIM_FILE()
         FILE_NAME = trim(BASE_FILE_DIRECTORY())//
     +                             "EMB"//trim(BASE_FILE_NAME)//".DAT"
         INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
         IF(FILE_EXISTS) THEN
            IF(LAHEY_LF95()) THEN
               SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//BASE_FILE_NAME
               CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
            ELSE
               CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,
     +                                                   ALL_VERSIONS,0)
               CALL MG_LOCATE_WRITE(16,30,BASE_FILE_NAME,ALL_VERSIONS,0)
            ENDIF
            DATA_DRIVE = OUTPUT_DIRECTORY()
            OPEN(10,FILE=FILE_NAME)
            OPEN(11,FILE=trim(DATA_DRIVE)//"BCELIMS.BIN",
     +                      ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
            ALLOCATE(CLASSIFICATIONS(8),SINGLE_YEAR(8),
     +            SINGLE_AMOUNT(8),MULTI_YEAR_ESCALAITON_VECTOR(8))
            IREC = 0
C
            READ(10,*,IOSTAT=IOS) DELETE
            DO
               DO
                  READ(10,1000,IOSTAT=IOS) RECLN
                  IF(IOS /= 0) EXIT
                  IF(RECLN(1:1) == '7') EXIT
                  RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,'//
     +                                   ',,,,,,,,,,,,,,,,,,,,,,,,'
                  READ(RECLN,*,ERR=200) DELETE,
     +                                  CLASSIFICATIONS,
     +                                  SINGLE_YEAR,
     +                                  SINGLE_AMOUNT,
     +                                  MULTI_YEAR_ESCALAITON_VECTOR
C
C WRITE RECORD
C
                  IREC = IREC + 1
                  WRITE(11,REC=IREC) DELETE,
     +                               CLASSIFICATIONS,
     +                               SINGLE_YEAR,
     +                               SINGLE_AMOUNT,
     +                               MULTI_YEAR_ESCALAITON_VECTOR
               ENDDO
               IF(IOS /= 0) EXIT
            ENDDO
            CLOSE(10)
            CLOSE(11)
            DEALLOCATE(CLASSIFICATIONS,SINGLE_YEAR,
     +              SINGLE_AMOUNT,MULTI_YEAR_ESCALAITON_VECTOR)
         ELSE IF(INDEX(BASE_FILE_NAME,'NONE') == 0) THEN
            CALL STOP_NOFILE(FILE_TYPE,FILE_NAME)
         ENDIF
      RETURN
C***********************************************************************
      ENTRY EM_MAKEOVL(OVERLAY_FAMILY_NAME) 
C***********************************************************************
C
         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
            CALL LOCATE(10,51)
         ENDIF
         DATA_DRIVE = OUTPUT_DIRECTORY()
         FILE_NAME = trim(DATA_DRIVE)//"EMO"//
     +                               trim(OVERLAY_FAMILY_NAME)//".DAT"
         OPEN(10,FILE=FILE_NAME)
         READ(10,*) DELETE
         INUNIT = 12
         IF(ELIMINATION_OL == 'BC') THEN
            OPEN(11,FILE=trim(DATA_DRIVE)//"BCELIMS.BIN",
     +                                       ACCESS="DIRECT",RECL=LRECL)
            INUNIT = 11
         ENDIF
         OPEN(12,FILE=trim(DATA_DRIVE)//"OLELIMS.BIN",ACCESS="DIRECT",
     +                                      STATUS="UNKNOWN",RECL=LRECL)
         ALLOCATE(CLASSIFICATIONS(8),SINGLE_YEAR(8),
     +            SINGLE_AMOUNT(8),MULTI_YEAR_ESCALAITON_VECTOR(8))
         IREC = 0
         DO
            DO
               READ(10,1000,IOSTAT=IOS) RECLN
               IF(RECLN(1:1) == '7') EXIT
               IREC = IREC + 1
               READ(INUNIT,REC=IREC,IOSTAT=IOS_BASE) DELETE,
     +                                  CLASSIFICATIONS,
     +                                  SINGLE_YEAR,
     +                                  SINGLE_AMOUNT,
     +                                  MULTI_YEAR_ESCALAITON_VECTOR
C
               IF(IOS_BASE /= 0) EXIT
               IF(IOS == 0) THEN
                  RECLN = trim(RECLN)//
     +                        ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'//
     +                        ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
                  READ(RECLN,*,ERR=200) DELETE,
     +                                  CLASSIFICATIONS,
     +                                  SINGLE_YEAR,
     +                                  SINGLE_AMOUNT,
     +                                  MULTI_YEAR_ESCALAITON_VECTOR
               ENDIF
C
               WRITE(12,REC=IREC) DELETE,
     +                            CLASSIFICATIONS,
     +                            SINGLE_YEAR,
     +                            SINGLE_AMOUNT,
     +                            MULTI_YEAR_ESCALAITON_VECTOR
            ENDDO
            IF(IOS_BASE /= 0) EXIT
         ENDDO
         DEALLOCATE(CLASSIFICATIONS,SINGLE_YEAR,
     +              SINGLE_AMOUNT,MULTI_YEAR_ESCALAITON_VECTOR)
         CLOSE(10)
         CLOSE(12)
C
         IF(ELIMINATION_OL == 'BC') CLOSE(11)
         ELIMINATION_OL = 'OL'       
      RETURN
c  200 CALL LOCATE(20,0)
c      WRITE(6,1010) trim(RECLN)
  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from FINACELM SIID116'
      call end_program(er_message)
C***********************************************************************
      ENTRY RESET_ELIMINATION_OL
C***********************************************************************
         ELIMINATION_OL = 'BC'
      RETURN
C***********************************************************************
      ENTRY READ_ELIMINATIONS(R_MAX_ELIMINATION_NUM)
C***********************************************************************
         DATA_DRIVE = OUTPUT_DIRECTORY()
         FILE_NAME = trim(DATA_DRIVE)//ELIMINATION_OL//"ELIMS.BIN"
         INQUIRE(FILE=FILE_NAME,EXIST=ELIMINATION_FILE_EXISTS)
         IF(ELIMINATION_FILE_EXISTS) THEN
            MAX_ELIMINATION_NUM = R_MAX_ELIMINATION_NUM
            OPEN(10,FILE=FILE_NAME,ACCESS="DIRECT",RECL=LRECL)
            ALLOCATE(CLASSIFICATIONS(8),SINGLE_YEAR(8),
     +               SINGLE_AMOUNT(8),
     +               MULTI_YEAR_ESCALAITON_VECTOR(8),
     +               ANNUAL_DATA(AVAIL_DATA_YEARS))
            IF(ALLOCATED(YEARLY_ELIMINATIONS_VALUES))
     +                            DEALLOCATE(YEARLY_ELIMINATIONS_VALUES)
            ALLOCATE(YEARLY_ELIMINATIONS_VALUES(0:MAX_ELIMINATION_NUM,      
     +                                              0:AVAIL_DATA_YEARS))
            VALUES_2_ZERO = INT(AVAIL_DATA_YEARS+1) *
     +                                       INT(MAX_ELIMINATION_NUM+1)
            YEARLY_ELIMINATIONS_VALUES = 0.
            CALL SET_CLASSIFICATIONS_TYPES(R_MAX_ELIMINATION_NUM)
            DO
               READ(10,IOSTAT=IOS) DELETE,
     +                             CLASSIFICATIONS,
     +                             SINGLE_YEAR,
     +                             SINGLE_AMOUNT,
     +                             MULTI_YEAR_ESCALAITON_VECTOR
               IF(IOS /= 0) EXIT
               IF(DELETE > 7) CYCLE
               DO I = 1, NUM_OF_DEBITS_AND_CREDITS
                  CALL FIND_CLASSIFICATION(CLASSIFICATIONS(I),POINTER)
                  IF(POINTER < 0) CYCLE
                  IS_A_DEBIT = I <= NUM_OF_DEBITS
                  CALL FIND_OPERATOR(POINTER,IS_A_DEBIT,ACCOUNTING_SIGN)
                  
                  IF(MULTI_YEAR_ESCALAITON_VECTOR(I) /= 0) THEN
                     VOID_LOGICAL = 
     +                        RETURN_A_VECTOR_FOR_ALL_YEARS(ANNUAL_DATA,
     +                                     MULTI_YEAR_ESCALAITON_VECTOR)
                     DO L = 1, AVAIL_DATA_YEARS
                        YEARLY_ELIMINATIONS_VALUES(POINTER,L) =
     +                           YEARLY_ELIMINATIONS_VALUES(POINTER,L) +
     +                                  ACCOUNTING_SIGN * ANNUAL_DATA(L)
                        IF(POINTER == 39) THEN ! LTD INTEREST
                           YEARLY_ELIMINATIONS_VALUES(293,L) =
     +                           YEARLY_ELIMINATIONS_VALUES(293,L) +
     +                                  ACCOUNTING_SIGN * ANNUAL_DATA(L)
                        ENDIF
                     ENDDO
                     IF(NON_CASH_ITEM(POINTER)) THEN
                        DO L = 1, AVAIL_DATA_YEARS
                           YEARLY_ELIMINATIONS_VALUES(142,L) =
     +                           YEARLY_ELIMINATIONS_VALUES(142,L) +
     +                                  ACCOUNTING_SIGN * ANNUAL_DATA(L)
                        ENDDO
                     ENDIF
                  ENDIF
                  IF(SINGLE_YEAR(I) >= BASE_YEAR .AND.
     +                SINGLE_YEAR(I) <= BASE_YEAR+AVAIL_DATA_YEARS) THEN
                     L = SINGLE_YEAR(I) - BASE_YEAR
                     YEARLY_ELIMINATIONS_VALUES(POINTER,L) =
     +                           YEARLY_ELIMINATIONS_VALUES(POINTER,L) +
     +                                ACCOUNTING_SIGN * SINGLE_AMOUNT(I)
                     IF(POINTER == 39) THEN ! LTD INTEREST
                        YEARLY_ELIMINATIONS_VALUES(293,L) =
     +                           YEARLY_ELIMINATIONS_VALUES(293,L) +
     +                                ACCOUNTING_SIGN * SINGLE_AMOUNT(I)
                     ENDIF
                     IF(NON_CASH_ITEM(POINTER)) THEN
                        YEARLY_ELIMINATIONS_VALUES(142,L) =
     +                           YEARLY_ELIMINATIONS_VALUES(142,L) +
     +                                ACCOUNTING_SIGN * SINGLE_AMOUNT(I)
                     ENDIF
                  ENDIF
               ENDDO
            ENDDO
            CLOSE(10)
C           IF(ALLOCATED(CLASSIFICATION_TYPES))
C    +                                  DEALLOCATE(CLASSIFICATION_TYPES)
            DEALLOCATE(CLASSIFICATIONS,SINGLE_YEAR,ANNUAL_DATA,
     +                 SINGLE_AMOUNT,MULTI_YEAR_ESCALAITON_VECTOR)
         ENDIF
C         
      RETURN
C***********************************************************************
      ENTRY RETURN_ALL_ELIMINATIONS_FOR(R_YEAR,R_ELIMINATIONS_VALUES)
C***********************************************************************
         IF(ELIMINATION_FILE_EXISTS) THEN
            L = R_YEAR
            IF(L <= AVAIL_DATA_YEARS) THEN
               CALL COMPLETE_STATEMENTS(R_YEAR,MAX_ELIMINATION_NUM,
     +                                  YEARLY_ELIMINATIONS_VALUES(0,L))
            ELSE 
               L = AVAIL_DATA_YEARS
            ENDIF
            DO I = 0, MAX_ELIMINATION_NUM
               R_ELIMINATIONS_VALUES(I) = R_ELIMINATIONS_VALUES(I) +
     +                                   YEARLY_ELIMINATIONS_VALUES(I,L)
            ENDDO
         ENDIF
      RETURN
 1000 FORMAT(A)
 1010 FORMAT('&',A)
      END
C***********************************************************************
      SUBROUTINE FIND_CLASSIFICATION(R_CLASSIFICATIONS,R_CLASS_POSITION)
C***********************************************************************
      INTEGER*2 R_CLASS_POSITION
      CHARACTER*(*) R_CLASSIFICATIONS
      INTEGER*2 R_VARIABLE,I
      REAL*4 FIND_OPERATION
      INTEGER*2 MAX_ELIMINATION_NUM,R_MAX_ELIMINATION_NUM
      SAVE MAX_ELIMINATION_NUM
      CHARACTER*30 CLASSIFICATION_TYPES(:)
      ALLOCATABLE :: CLASSIFICATION_TYPES
      SAVE CLASSIFICATION_TYPES
      LOGICAL*1 R_DEBIT
C
         R_CLASS_POSITION = -1
         IF(INDEX(R_CLASSIFICATIONS,'Not Used') == 0) THEN
            DO I = 0, MAX_ELIMINATION_NUM
               IF(trim(CLASSIFICATION_TYPES(I)) == ' ') CYCLE 
               IF(INDEX(R_CLASSIFICATIONS,
     +                               CLASSIFICATION_TYPES(I)) /=0 ) THEN
                  R_CLASS_POSITION = I
                  EXIT
               ENDIF   
            ENDDO
         ENDIF
      RETURN
C***********************************************************************
      ENTRY FIND_OPERATOR(R_VARIABLE,R_DEBIT,FIND_OPERATION)
C***********************************************************************
         FIND_OPERATION = -1
         SELECT CASE (R_VARIABLE)
         CASE(0:3,32:34,37,41,43,307,329:333,326:328)  ! ADDITIONS ON THE INCOME STATEMENT
            IF(R_DEBIT) THEN
               FIND_OPERATION = 1.
            ELSE
               FIND_OPERATION = -1.
            ENDIF   
         CASE(69,75)  ! BALANCE ASSET SHEET ITEMS
            IF(R_DEBIT) THEN
               FIND_OPERATION = 1.
            ELSE
               FIND_OPERATION = -1.
            ENDIF   
         CASE(89,104)  ! BALANCE LIABILIITES SHEET ITEMS
            IF(R_DEBIT) THEN
               FIND_OPERATION = -1.
            ELSE
               FIND_OPERATION = 1.
            ENDIF   
         CASE DEFAULT
            IF(R_DEBIT) THEN
               FIND_OPERATION = -1.
            ELSE
               FIND_OPERATION = 1.
            ENDIF   
         END SELECT
      RETURN
C***********************************************************************
      ENTRY SET_CLASSIFICATIONS_TYPES(R_MAX_ELIMINATION_NUM)
C***********************************************************************
C
         MAX_ELIMINATION_NUM = R_MAX_ELIMINATION_NUM
         IF(ALLOCATED(CLASSIFICATION_TYPES))
     +                                  DEALLOCATE(CLASSIFICATION_TYPES)
         ALLOCATE(CLASSIFICATION_TYPES(0:MAX_ELIMINATION_NUM))
         DO I = 0, MAX_ELIMINATION_NUM
            CLASSIFICATION_TYPES(I) = " "
         ENDDO
C
C ADDITIONS TO INCOME STATEMENT
C
         CLASSIFICATION_TYPES(0) = "Base Rates"
         CLASSIFICATION_TYPES(329) = "Residential"
         CLASSIFICATION_TYPES(330) = "Commercial"
         CLASSIFICATION_TYPES(331) = "Industrial"
         CLASSIFICATION_TYPES(332) = "Lighting"
         CLASSIFICATION_TYPES(307) = "Gas Revenues"
         CLASSIFICATION_TYPES(326) = "Unbilled Revenues"
         CLASSIFICATION_TYPES(327) = "ATL Deferred Revenues"
         CLASSIFICATION_TYPES(328) = "Relationship Revenues"
         CLASSIFICATION_TYPES(333) = "Bulk Power"
         CLASSIFICATION_TYPES(1) = "Adjustment Clause"
         CLASSIFICATION_TYPES(2) = "Secondary Sales"
         CLASSIFICATION_TYPES(3) = "Other Revenue"
         CLASSIFICATION_TYPES(4) = " "
C
C REDUCTIONS TO INCOME STATEMENT
C
         CLASSIFICATION_TYPES(5) = "Fossil Fuel"
         CLASSIFICATION_TYPES(6) = "Purchased Power"
         CLASSIFICATION_TYPES(7) = "Variable O&M"
         CLASSIFICATION_TYPES(8) = "Fixed O&M"
         CLASSIFICATION_TYPES(9) = "Service Transactions"
         CLASSIFICATION_TYPES(10) = "Other O&M"
         CLASSIFICATION_TYPES(11) = "Purchased Gas"
         CLASSIFICATION_TYPES(12) = "Other Expenses"
         CLASSIFICATION_TYPES(335) = "Transmission-Operation"
         CLASSIFICATION_TYPES(336) = "Transmission-Maintenance"
         CLASSIFICATION_TYPES(337) = "Distribution-Operation"
         CLASSIFICATION_TYPES(338) = "Distribution-Maintenance"
         CLASSIFICATION_TYPES(321) = "Customer Accounts"
         CLASSIFICATION_TYPES(322) = "Customer Services"
         CLASSIFICATION_TYPES(323) = "Sales Expense"
         CLASSIFICATION_TYPES(324) = "A&G Operations"
         CLASSIFICATION_TYPES(325) = "A&G Maintenance"
         CLASSIFICATION_TYPES(60) = "Lease Expense"
         CLASSIFICATION_TYPES(219) = "Owned Nuclear Fuel"
         CLASSIFICATION_TYPES(116) = "Leased Nuclear Fuel"
         CLASSIFICATION_TYPES(13) = "DSM Expense"
         CLASSIFICATION_TYPES(14) = "DSM Rebate"

         CLASSIFICATION_TYPES(15) = "Emissions Credit"
         CLASSIFICATION_TYPES(17) = "Depreciation Expense"
         CLASSIFICATION_TYPES(18) = "Amortization Expense"
         CLASSIFICATION_TYPES(19) = "CIAC Amortization"
         CLASSIFICATION_TYPES(20) = " " ! TOTAL_EXPENSES_B4_TAXES
         CLASSIFICATION_TYPES(21) = "Operating Revenue Tax"
C        CLASSIFICATION_TYPES(22) = "Other Taxes" + "Property Taxes" + "Environmental Tax"
         CLASSIFICATION_TYPES(218) = "Other Taxes"
         CLASSIFICATION_TYPES(217) = "Property Taxes"
         CLASSIFICATION_TYPES(216) = "Environmental Tax"
         CLASSIFICATION_TYPES(23) = "Local Income Taxes"
         CLASSIFICATION_TYPES(24) = "State Income Taxes Paid"
         CLASSIFICATION_TYPES(25) = "State Tax on Capital"
         CLASSIFICATION_TYPES(26) = "Federal Income Taxes Paid"
         CLASSIFICATION_TYPES(27) = "Federal Tax on Capital"
         CLASSIFICATION_TYPES(28) = "Income Tax Deferrals Debit"
         CLASSIFICATION_TYPES(29) = "Income Tax Deferrals Credit"
         CLASSIFICATION_TYPES(50) = "Investment Tax Credit"
         CLASSIFICATION_TYPES(30) = " " ! TOTAL_EXPENSE_AND_TAXES
         CLASSIFICATION_TYPES(31) = " " ! OPERATING_INCOME
         CLASSIFICATION_TYPES(32) = "Deferred Revenues"
         CLASSIFICATION_TYPES(33) = "BTL Revenues"
         CLASSIFICATION_TYPES(34) = "Interest Income"
         CLASSIFICATION_TYPES(35) = "BTL Expenses"
         CLASSIFICATION_TYPES(36) = "BTL Income Taxes"
         CLASSIFICATION_TYPES(37) = "AFUDC Equity"
         CLASSIFICATION_TYPES(38) = " " !INCOME_B4_INTEREST
         CLASSIFICATION_TYPES(39) = "LTD Interest"
         CLASSIFICATION_TYPES(40) = "STD Interest"
         CLASSIFICATION_TYPES(41) = "AFUDC Borrowed"
         CLASSIFICATION_TYPES(42) = " " ! INCOME_AFTER_INTEREST
         CLASSIFICATION_TYPES(43) = "Subsidiary Income"
         CLASSIFICATION_TYPES(44) = "Extraordinary Items"
         CLASSIFICATION_TYPES(45) = " " ! NET_PROFIT_OR_LOSS
         CLASSIFICATION_TYPES(46) = "Preferred Dividends"
         CLASSIFICATION_TYPES(47) = " " ! EARNINGS_AVAILABLE_TO_COMMON
         CLASSIFICATION_TYPES(48) = "Common Dividends"
         CLASSIFICATION_TYPES(49) = " " ! RETAINED_EARNINGS
C
C BALANCE SHEET ITEMS
C
         CLASSIFICATION_TYPES(69)  = "Other Investments"
         CLASSIFICATION_TYPES(75)  = "Assets NEC"
         CLASSIFICATION_TYPES(89)  = "Liabilities NEC"
         CLASSIFICATION_TYPES(104) = "STD Issued"
      RETURN
      END
C***********************************************************************
      FUNCTION NON_CASH_ITEM(R_VARIABLE)
C***********************************************************************
C
      LOGICAL*1 NON_CASH_ITEM
      INTEGER*2 R_VARIABLE
C
         SELECT CASE (R_VARIABLE)
         CASE(17:19,28:29,32,37,41,43,44,326)  ! NON-CASH ITEMS ON THE INCOME STATEMENT
            NON_CASH_ITEM = .TRUE.
         CASE DEFAULT
            NON_CASH_ITEM = .FALSE.
         END SELECT
      RETURN
      END
C***********************************************************************
      SUBROUTINE COMPLETE_STATEMENTS(R_YEAR,MAX_ELIMINATION_NUM,
     +                               R_VARIABLE)
C***********************************************************************
C
      INCLUDE 'ac_elmeq.mon'
      INTEGER*2 MAX_ELIMINATION_NUM,R_YEAR,I
      REAL*4 R_VARIABLE(0:MAX_ELIMINATION_NUM)
      REAL*4 WORKING_CAPITIAL_LAST_YEAR
      REAL*4 TOTAL_DEPRECIATION,TOTAL_AMORTIZATION,
     +       TOTAL_CIAC_AMORTIZATION,TOTAL_DEFERRED_TAXES_CR,
     +       TOTAL_DEFERRED_TAXES_DR,TOTAL_RETAINED_EARNINGS,
     +       TOTAL_NUCLEAR_FUEL_BURNED,
     +       TOTAL_STD_ISSUED,
     +       TOTAL_CHANGE_IN_CASH,
     +       TOTAL_CHANGE_OTHER_ASSETS,
     +       TOTAL_CHANGE_ASSETS_NEC,
     +       TOTAL_CHANGE_LIABS_NEC,
     +       TOTAL_CHANGE_DEFERRED_REVENUES
      SAVE TOTAL_DEPRECIATION,TOTAL_AMORTIZATION,
     +     TOTAL_CIAC_AMORTIZATION,TOTAL_DEFERRED_TAXES_CR,
     +     TOTAL_DEFERRED_TAXES_DR,TOTAL_RETAINED_EARNINGS,
     +     TOTAL_NUCLEAR_FUEL_BURNED,
     +     TOTAL_STD_ISSUED,
     +     TOTAL_CHANGE_IN_CASH,
     +     TOTAL_CHANGE_OTHER_ASSETS,
     +     TOTAL_CHANGE_ASSETS_NEC,
     +     TOTAL_CHANGE_LIABS_NEC,
     +     TOTAL_CHANGE_DEFERRED_REVENUES
      SAVE WORKING_CAPITIAL_LAST_YEAR
C
C COPY ELIMINATION DATA INTO WORKING ARRAY
C
         VARIABLE(0:MAX_ELIMINATION_NUM) =
     +                                 R_VARIABLE(0:MAX_ELIMINATION_NUM)
         IF(R_YEAR == 1) THEN
            WORKING_CAPITIAL_LAST_YEAR = 0.
            TOTAL_CHANGE_OTHER_ASSETS = 0.
            TOTAL_CHANGE_ASSETS_NEC = 0.
            TOTAL_CHANGE_LIABS_NEC = 0.
            TOTAL_DEPRECIATION = 0.
            TOTAL_AMORTIZATION = 0.
            TOTAL_CIAC_AMORTIZATION = 0.
            TOTAL_DEFERRED_TAXES_CR = 0.
            TOTAL_DEFERRED_TAXES_DR = 0.
            TOTAL_RETAINED_EARNINGS = 0.
            TOTAL_NUCLEAR_FUEL_BURNED = 0.
            TOTAL_STD_ISSUED = 0.
            TOTAL_CHANGE_IN_CASH = 0.
            TOTAL_CHANGE_DEFERRED_REVENUES = 0.
         ENDIF
C
C ADJUSTMENTS TO ITEMS THAT ARE MAINTAINED AS BALANCES BUT ARE ENTERED
C  AS CHANGES
C
C ASSETS NEC
         TOTAL_CHANGE_ASSETS_NEC = ASSET_NEC + TOTAL_CHANGE_ASSETS_NEC
         ASSET_NEC = TOTAL_CHANGE_ASSETS_NEC
C LIABILITIES NEC
         TOTAL_CHANGE_LIABS_NEC = LIABILITIES_NEC +
     +                                            TOTAL_CHANGE_LIABS_NEC              
         LIABILITIES_NEC = TOTAL_CHANGE_LIABS_NEC
C OTHER ASSETS
         TOTAL_CHANGE_OTHER_ASSETS = TOTAL_CHANGE_OTHER_ASSETS  +
     +                                                      ASSETS_OTHER
         INVESTMENT_SALVAGE_OTHER = INVESTMENT_SALVAGE_OTHER +
     +                                                      ASSETS_OTHER
         ASSETS_OTHER = TOTAL_CHANGE_OTHER_ASSETS 
C
C INCOME STATEMENT
C
         REVENUES_OPERATING = BASE_RATES_REVENUES +
     +                        ADJUSTMENT_CLAUSE_REVENUES +
     +                        SECONDARY_SALES_REVENUES +
     +                        REVENUES_OTHER
         CLASS_NUCLEAR_FUEL_EXPENSE = LEASED_NF_BURN +
     +                                OWNED_NF_BURN
         TOTAL_EXPENSES_B4_TAXES = FUEL_EXPENSE +
     +                             PURCHASE_POWER_EXPENSE +
     +                             VARIABLE_EXPENSE +
     +                             FIXED_EXPENSE +
     +                             SERVICE_TRANSACTION_EXPENSE +
     +                             OTHER_EXPENSE_1 +
     +                             OTHER_EXPENSE_2 +
     +                             OTHER_EXPENSE_3 +
     +                             ATL_LEASE_PAYMENTS +
     +                             DSM_EXPENSE +
     +                             DSM_REBATE +
     +                             EMISSION_CREDIT_EXPENSE +
     +                             CLASS_NUCLEAR_FUEL_EXPENSE +
     +                             BOOK_DEPRECIATION +
     +                             AMORTIZATION_EXPENSE +
     +                             CIAC_AMORTIZATION
C
C TAX ITEMS
C
               OTHER_AND_ENVIRONMENTAL_TAXES = OTHER_TAXES +
     +                                         ENVIRONMENTAL_TAX
               TOTAL_EXPENSE_AND_TAXES = TOTAL_EXPENSES_B4_TAXES +
     +                                   OTHER_TAXES +
     +                                   ENVIRONMENTAL_TAX +
     +                                   CLASS_PROPERTY_TAXES +
     +                                   OPERATING_REVENUE_TAX +
     +                                   LOCAL_TAXES +
     +                                   STATE_TAXES +
     +                                   STATE_TAX_ON_CAPITAL +
     +                                   FEDERAL_TAXES +
     +                                   FEDERAL_TAX_ON_CAPITAL +
     +                                   DEFERRED_TAXES_DR +
     +                                   DEFERRED_TAXES_CR +
     +                                   TAX_CREDITS
C
C OPERATING INCOME
C
               OPERATING_INCOME = REVENUES_OPERATING -
     +                            TOTAL_EXPENSE_AND_TAXES
C
C INCOME B4 INTEREST
C
               INCOME_B4_INTEREST = OPERATING_INCOME
     +                              + DEFERRED_REVENUES
     +                              + CLASS_BTL_REVENUES
     +                              + NET_DERIVATIVES_INCOME
     +                              + INVESTMENT_INCOME
     +                              - CLASS_BTL_EXPENSES
     +                              - BTL_INCOME_TAXES
     +                              + AFUDC_EQUITY
C
               INCOME_AFTER_INTEREST = INCOME_B4_INTEREST -
     +                                 INTEREST_ON_LONG_TERM_DEBT -
     +                                 STD_INTEREST +
     +                                 AFUDC_BORROWED
C
               NET_PROFIT_OR_LOSS = INCOME_AFTER_INTEREST +
     +                              SUBSIDIARY_INCOME -
     +                              NET_OF_TAX_EXEXP
C
               EARNINGS_AVAILABLE_TO_COMMON = NET_PROFIT_OR_LOSS -
     +                                        PREFERRED_DIVIDENDS
C
               RETAINED_EARNINGS = EARNINGS_AVAILABLE_TO_COMMON -
     +                             COMMON_DIVIDENDS
C
C CHANGE IN CASH 
C
         INCOME_BEFORE_CAPITAL_SERVICE = INCOME_B4_INTEREST +
     +                                                    AFUDC_BORROWED
         FUNDS_FROM_OPERATIONS = INCOME_BEFORE_CAPITAL_SERVICE + 
     +                                 CLASS_NUCLEAR_FUEL_EXPENSE + 
     +                                 BOOK_DEPRECIATION + 
     +                                 AMORTIZATION_EXPENSE + 
     +                                 CIAC_AMORTIZATION - 
     +                                 DEFERRED_REVENUES - 
     +                                 AFUDC_EQUITY - 
     +                                 AFUDC_BORROWED + 
     +                                 DEFERRED_TAXES_DR + 
     +                                 DEFERRED_TAXES_CR + 
     +                                 TAX_CREDITS - 
     +                                 INTEREST_ON_LONG_TERM_DEBT - 
     +                                 STD_INTEREST - 
     +                                 PREFERRED_DIVIDENDS - 
     +                                 COMMON_DIVIDENDS + 
     +                                 CASH_FROM_SALE_OF_ASSETS + 
     +                                 DIVIDEND_FROM_SUBSIDIARY + 
     +                                 TAXES_PAID_BY_PARENT + 
     +                                 TAX_PAYMENTS_2_PARENT_FROM_SUB - 
     +                                 INCOME_TAXES_CONSOLIDATED + 
     +                                 CIAC_CASH + 
     +                                 CUSTOMER_DEPOSITS +
     +                                 ELIMINATION_CASH_ADJUSTMENT
C         INVESTMENT_SALVAGE_OTHER = CASH_OTH_ASSET + DSM_CAPITIAL -  
C     +                              SALVAGE_TRANSACTIONS 
         CONSTRUCTION_AND_NET_INVESTMENT = FA_CASH  + 
     +                                     NF_CASH  + 
     +                                     INVESTMENT_SALVAGE_OTHER + 
     +                                     NEW_SUBSIDIARY_INVESTMENT + 
     +                                     CAPITIALIZED_LEASE_ADDITIONS
         CHANGE_IN_WORKING_CAPITIAL = ASSET_NEC - LIABILITIES_NEC -   
     +                                WORKING_CAPITIAL_LAST_YEAR
         WORKING_CAPITIAL_LAST_YEAR = ASSET_NEC - LIABILITIES_NEC
         CHANGE_IN_FUNDS_B4_FINANCING = FUNDS_FROM_OPERATIONS - 
     +                                 CONSTRUCTION_AND_NET_INVESTMENT - 
     +                                 LEASED_NF_BURN - 
     +                                 NUCL_DECOM_FUND - 
     +                                 CHANGE_IN_WORKING_CAPITIAL - 
     +                                 LONG_TERM_DEBT_RETIREMENTS - 
     +                                 PREFERRED_STOCK_RETIREMENTS - 
     +                                 COMMON_STOCK_BOUGHT- 
     +                                 DEFERRED_EXPENSE_CASH  
C         LEASE_RECEIPTS = CAPITIALIZED_LEASE_ADDITIONS +
C     +                    LEASED_NF_BURN 
         EXTERNAL_FINANCING_PERFORMED = COMMON_STOCK_ISSUED + 
     +                                LONG_TERM_DEBT_ISSUED + 
     +                                PREFERRED_STOCK_ISSUED + 
     +                                SHORT_TERM_DEBT_ISSUED + 
     +                                LEASE_RECEIPTS   
         CHANGE_IN_CASH = EXTERNAL_FINANCING_PERFORMED +
     +                    CHANGE_IN_FUNDS_B4_FINANCING 
C
C BALANCE SHEET ITEMS
C
         GROSS_PLANT_VALUE_PLUS_CWIP = GROSS_PLANT_VALUE + CWIP
C      
         TOTAL_DEPRECIATION = TOTAL_DEPRECIATION + BOOK_DEPRECIATION
         CUMULATIVE_DEPRECIATION = CUMULATIVE_DEPRECIATION +
     +                                                TOTAL_DEPRECIATION 
         TOTAL_NUCLEAR_FUEL_BURNED = TOTAL_NUCLEAR_FUEL_BURNED +
     +                                        CLASS_NUCLEAR_FUEL_EXPENSE
         NET_NF_VALUE = NET_NF_VALUE - TOTAL_NUCLEAR_FUEL_BURNED  
C
         UTILITY_PLANT_NET = GROSS_PLANT_VALUE_PLUS_CWIP -
     +                       CUMULATIVE_DEPRECIATION +
     +                       NET_NF_VALUE 
C
         TOTAL_AMORTIZATION = TOTAL_AMORTIZATION + AMORTIZATION_EXPENSE
         DD_BALANCE_NET = DD_BALANCE_NET - TOTAL_AMORTIZATION 
         TOTAL_DEFERRED_TAXES_DR = TOTAL_DEFERRED_TAXES_DR +
     +                                                 DEFERRED_TAXES_DR
         DEFERRED_TAXES_DR_BAL = DEFERRED_TAXES_DR_BAL -
     +                           TOTAL_DEFERRED_TAXES_DR 
C
         TOTAL_CHANGE_IN_CASH = TOTAL_CHANGE_IN_CASH + CHANGE_IN_CASH
         CASH_BALANCE = CASH_BALANCE + TOTAL_CHANGE_IN_CASH
         TOTAL_CHANGE_DEFERRED_REVENUES = DEFERRED_REVENUES +
     +                                    TOTAL_CHANGE_DEFERRED_REVENUES
         DEFERRED_REVENUES_CUM = DEFERRED_REVENUES_CUM+DEFERRED_REVENUES
C
         TOTAL_ASSETS = UTILITY_PLANT_NET +
     +                  ASSETS_OTHER +
     +                  SUBSIDIARY_INVESTMENT +
     +                  LEASES_CAPITIALZIED +
     +                  DECOMMISIONING_FUND_BAL +
     +                  DD_BALANCE_NET +
     +                  DEFERRED_REVENUES_CUM +
     +                  DEFERRED_TAXES_DR_BAL +
     +                  CASH_BALANCE +
     +                  ASSET_NEC
C
         TOTAL_CIAC_AMORTIZATION = TOTAL_CIAC_AMORTIZATION +
     +                                                 CIAC_AMORTIZATION
         CIAC_BAL = CIAC_BAL - TOTAL_CIAC_AMORTIZATION 
         TOTAL_DEFERRED_TAXES_CR = TOTAL_DEFERRED_TAXES_CR +
     +                                                 DEFERRED_TAXES_CR
         DEF_TAX_CR_BAL = DEF_TAX_CR_BAL + TOTAL_DEFERRED_TAXES_CR 
         TOTAL_RETAINED_EARNINGS = TOTAL_RETAINED_EARNINGS +
     +                                                 RETAINED_EARNINGS
         RETAINED_EARNINGS_BAL = RETAINED_EARNINGS_BAL +
     +                           TOTAL_RETAINED_EARNINGS 
         TOTAL_STD_ISSUED = TOTAL_STD_ISSUED + SHORT_TERM_DEBT_ISSUED
         SHORT_TERM_DEBT_BAL = SHORT_TERM_DEBT_BAL + TOTAL_STD_ISSUED 
         COMMON_EQUITY_BAL = COMMON_STOCK_BAL +
     +                       RETAINED_EARNINGS_BAL
         TOTAL_CAPITIAL_BAL = COMMON_EQUITY_BAL +
     +                        PREFERRED_STOCK_BAL +
     +                        LONG_TERM_DEBT_BAL
         TOTAL_LIABILITIES = TOTAL_CAPITIAL_BAL +
     +                       OTHER_LT_LIABILITY_BAL +
     +                       SHORT_TERM_DEBT_BAL +
     +                       CUSTOMER_DEPOSITS_BALANCE +
     +                       CIAC_BAL +
     +                       DEF_TAX_CR_BAL +
     +                       DEF_ITC_BAL +
     +                       LIABILITIES_NEC
C
            R_VARIABLE(0:MAX_ELIMINATION_NUM) =
     +                                   VARIABLE(0:MAX_ELIMINATION_NUM)
      RETURN
      END
