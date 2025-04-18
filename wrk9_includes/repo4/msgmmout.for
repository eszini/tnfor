!     ******************************************************************
!     MSGMMOUT.for
!     Copyright(c) M.S. Gerber & Associates 2000
!
!     Created: 2/26/2003 11:25:24 AM
!     Author : MARK S GERBER
!     Last change: 
!			MSG 3/15/2016 7:52:36 PM
!     ******************************************************************
C
C             Monthly Module for Monthly MIDAS Gold
C                      COPYRIGHT (c) 1998
C                 M.S. GERBER & ASSOCIATES, INC.
C                      ALL RIGHTS RESERVED
C
C***********************************************************************
C
C***********************************************************************
      RECURSIVE SUBROUTINE MONTHLY_FINANCIAL_ANALYSIS(R_YR,CLASS_ID,
     +                                 CLASS_POS,
     +                                 CLASS_TYPE,
     +                                 CLASS_LEVEL,
     +                                 DONT_TALLY_THIS_CLASS,
     +                                 ANNUAL_VARS,R_ITER,
     +                                 OPENING_STD_BALANCE,
     +                                 STD_MINIMUM_BALANCE,
     +                                 MINIMUM_CASH_BALANCE,
     +                                 FUNDS_BALANCE,
     +                                 LTD_ISSUE_YR_INTEREST_BOOKED,
     +                                 PS_ISSUE_YR_DIVIDEND_BOOKED,
     +                                 LTD_ISSUE_YR_INTEREST_PAYMENT,
     +                                 PS_ISSUE_YR_DIVIDEND_PAYMENT,
     +                                 R_BTL_CASH_EARNINGS,
     +                                 R_STI_INTEREST_INCOME,
     +                                 R_STD_CASH_INTEREST,
     +                                 R_STD_INCOME_INTEREST,
     +                                 R_NON_INCOME_TAXES_ACCRUAL_ADJ,
     +                                 R_STATE_TAXES_ACCRUAL_ADJ,
     +                                 R_FEDERAL_TAXES_ACCRUAL_ADJ,
     +                                 LONG_TERM_DEBT_ISSUED,
     +                                 PREFERRED_STOCK_ISSUED,
     +                                 CUSTOMER_DEPOSITS_BAL,
     +                                 STD_ISSUED,
     +                                 MONTHLY_COMMON_DIVIDENDS,
     +                                 R_FUEL_REVENUE_RECEIVABLE,
     +                                 BOY_LONG_TERM_INVESTMENTS,
     +                                 R_LT_INVESTMENTS_EARNINGS,
     +                                 CASH_2_LT_INVESTMENTS,
     +                                 STI_CHANGE_IN_CASH,
     +                                 CATAWBA_RECEIVABLES,
     +                                 CATAWBA_PAYABLES,
     +                                 FED_STATE_CASH_TAXES_PAID,
     +                                 MAX_CASH_BALANCE,
     +                                 SUB_FED_TAXES_PAID_2_PARENT,
     +                                 SUB_STATE_TAXES_PAID_2_PARENT,
     +                                 ASSETS_NEC_BOY,
     +                                 LIABS_NEC_BOY,
     +                                 COMMON_STOCK_CASH_DIVIDENDS,
     +                                 ASSETS_NEC_EOY,
     +                                 LIABS_NEC_EOY,
     +                                 NET_UTILITY_PLANT_EOY,
     +                                 LTD_RETIREMENTS,
     +                                 PS_RETIREMENTS,
     +                                 BOY_LTD_BALANCE,
     +                                 BOY_PS_BALANCE,
     +                                 R_OTHER_TAXES,
     +                                 R_OP_REV_TAX,
     +                                 BOY_NUC_DECOM_FUND_BAL,
     +                                 R_NEW_SUB_INVESTMENT,
     +                                 R_GOODWILL_OF_NEW_SUB_INVESTMENT,
     +                                 FUEL_EXPENSE,
     +                                 VARIABLE_EXPENSE,
     +                                 FIXED_EXPENSE,
     +                                 CASH_REVENUES_RECEIVED,
     +                                 CASH_EXPENSES_PAID,
     +                                 INVESTMENTS_BOY)

C
      use spindriftlib
      use prod_arrays_dimensions
      USE IREC_ENDPOINT_CONTROL
      use grx_planning_routines
      USE SIZECOM   
      use globecom
      use class_run_switchesc
      use class_run_switchesl1
      use class_run_switchesl4
      SAVE

      INCLUDE 'FINACCOM.MON'
      INCLUDE 'NAMESCOM.MON'
      INCLUDE 'MTHNMCOM.MON'

      INTERFACE 
         FUNCTION CongestionMonthlyExpenes(AssetClass) RESULT(Expense)
            REAL (KIND=4) :: Expense(0:12)
            INTEGER (KIND=2) :: AssetClass
         END FUNCTION
      END INTERFACE
C              
      REAL (KIND=4) :: FUEL_EXPENSE,
     +                 VARIABLE_EXPENSE,
     +                 FIXED_EXPENSE,
     +                 CASH_REVENUES_RECEIVED,
     +                 CASH_EXPENSES_PAID
      INTEGER*2 VARIABLE_NUMBER,START_STUDY_ENDPOINT
      !next unused 734 RANGE FROM 0 TO VARIABLE_NUMBER-1
      PARAMETER(VARIABLE_NUMBER=950) 
      REAL*4 NOT_AVAIL
      PARAMETER(NOT_AVAIL=-999999.)
      LOGICAL*1 LAHEY_LF95,ODEC,ODEC_NA3_ACTIVE/.FALSE./,
     +          THIS_IS_REPORTING_CLASS,FirstEnergy
      REAL (KIND=4) :: ODEC_NA3_INTRA_BULK_POWER(0:12)
      REAL*4 ATL_FEDERAL_TAXES,
     +       FEDERAL_TAXES,
     +       ATL_STATE_TAXES,
     +       STATE_TAXES,
     +       OUTPUT_CLASS_ID,
     +       NET_UTILITY_PLANT_EOY,
     +       ASSETS_NEC_EOY,
     +       LIABS_NEC_EOY,
     +       INCOME_BEFORE_INTEREST,TOTAL_TAXES_EXPENSE,
     +       MONTHLY_INCOME_AFTER_INTEREST,MONTHLY_NET_INCOME,
     +       MONTHLY_TOTAL_EXPENSES_B4_TAXES,
     +       HALF_YEAR,
     +       THE_RATIO_OF_A_TO_B,
     +       THE_PERCENT_OF_A_TO_B,
     +       QUARTERLY_EARNINGS_PER_SHARE,
     +       CONSOLIDATED_FED_TAX_ADJ,
     +       CONSOLIDATED_STATE_TAX_ADJ,
     +       R_OTHER_TAXES,
     +       R_OP_REV_TAX,
     +       ENVIROMENTAL_TAX,
     +       SAVE_SBU_TAX_EXCLUSIONS,
     +       ZERO_TAX_EXCLUSIONS,
     +       BOOK_INCOME_B4_TAXES,
     +       TOTAL_INCOME_TAXES,
     +       EFFECTIVE_TAX_RATE,
     +       ADJUSTED_BOOK_INCOME,
     +       IPALCO_TAXES_PAID_CARRY_OVER
      INTEGER*2 CLASS_LINK_2,SUB_LINK_2_CLASS,
     +          ELIMINAITON_CLASS_ID_NUM
      CHARACTER*1 DPS,DPR
      INTEGER*2 BOY,EOY
      PARAMETER (BOY=1,EOY=2,DPS='S',DPR='R')
      LOGICAL*1 LEVELIZE_INCOME_TAXES,BANGOR
      REAL*4 MINIMUM_CASH_BALANCE,CHANGE_IN_FUNDS,
     +       AVAILABLE_FUNDS,CAPITAL_ISSUES,
     +       STD_BALANCE,STD_MINIMUM_BALANCE,
     +       FUNDS_BALANCE(2),OPENING_STD_BALANCE,
     +       STATE_BTL_INCOME_TAXES,
     +       FEDERAL_BTL_INCOME_TAXES,
     +       LONG_TERM_DEBT_ISSUED,
     +       PREFERRED_STOCK_ISSUED,
     +       CUSTOMER_DEPOSITS_BAL(2),
     +       STD_ISSUED,
     +       QRT_DIVIDEND_PER_SHARE(4),
     +       BOY_LONG_TERM_INVESTMENTS,
     +       R_LT_INVESTMENTS_EARNINGS,
     +       CASH_2_LT_INVESTMENTS,
     +       STI_CHANGE_IN_CASH,
     +       CATAWBA_RECEIVABLES,
     +       CATAWBA_PAYABLES,
     +       FED_STATE_CASH_TAXES_PAID,
     +       MAX_CASH_BALANCE,
     +       PREVIOUS_BAL_SHEET_INBALANCE,
     +       BOY_ISSUE_EXP_BAL,
     +       COMMON_STOCK_CASH_DIVIDENDS,
     +       BOY_DEBIT_UNAMORT_INTEREST,
     +       REAQUIRED_DEBT_BOY,
     +       REGULATORY_LIABS_BOY,
     +       DEFERRED_CREDITS_BOY,
     +       GOODWILL_BOY,
     +       ExecBenefitsBalance_BOY,
     +       IncentiveCompBalance_BOY
      REAL*4 TAX_CASH_RETIREMENT_PAYMENTS,
     +       TAX_CASH_STORM_PAYMENTS,
     +       TAX_CASH_VACATION_PAYMENTS,
     +       TAX_BOOK_RETIREMENT_PAYMENTS,
     +       TAX_BOOK_STORM_PAYMENTS,
     +       TAX_BOOK_VACATION_PAYMENTS,
     +       RPT_DEDUCTIBLE_BOOK_EXPENSES,
     +       RPT_TOTAL_DEDUCT_BOOK_EXPENSES,
     +       RPT_TAXABLE_INCOME_B4_DEDUCTS,
     +       RPT_STATE_INCOME_TAX_B4_CREDITS,
     +       RPT_FED_INCOME_TAX_B4_CREDITS,
     +       RPT_CLASS_OPERATING_REVENUES,      
     +       RPT_EXCLUDED_OTHER_TAXES_REV,      
     +       RPT_CLASS_NON_VARIABLE_EXPENSE,    
     +       RPT_EXCLUDED_OTHER_TAXES_EXP,      
     +       RPT_OTHER_TAXES_MOVING_UP,          
     +       RPT_CLASS_REVENUE_TAX_REVENUES,                
     +       RPT_CLASS_REV_TAX_EXCLUDED_REVS,               
     +       RPT_REVENUE_TAXES_MOVING_UP

      REAL*4 RPT_FEDERAL_ATL_INCOME_TAX
      REAL*4 RPT_STATE_ATL_INCOME_TAX
      REAL*4 RPT_STATE_TAX_INCOME_B4_NOLS
      REAL*4 RPT_STATE_TAXABLE_INCOME
      REAL*4 RPT_STATE_INCOME_TAX
      REAL*4 RPT_FED_TAX_INCOME_B4_NOLS
      REAL*4 RPT_FEDERAL_TAXABLE_INCOME
      REAL*4 RPT_FEDERAL_INCOME_TAX
      REAL*4 RPT_BTL_OTHER_TAXABLE_INCOME,
     +       RPT_BTL_TAXABLE_INVESTMENT,
     +       RPT_BTL_TAXABLE_EXPENSES
      REAL*4 RPT_BTL_M1_TAX_DEDUCTIONS
      REAL*4 RPT_STATE_BTL_TAXABLE_INCOME,
     +       RPT_STATE_BTL_INCOME_TAX,
     +       RPT_FEDERAL_BTL_TAXABLE_INCOME,
     +       RPT_FEDERAL_BTL_INCOME_TAX,
     +       RPT_STATE_NOLS_USED,
     +       RPT_FEDERAL_NOLS_USED,
     +       RPT_STATE_NOLS_GENERATED,
     +       RPT_FEDERAL_NOLS_GENERATED,
     +       RPT_STATE_GENERAL_CREDITS,
     +       RPT_FEDERAL_SEC29_CREDITS,
     +       RPT_FEDERAL_GENERAL_CREDITS,
     +       RPT_FEDERAL_AMT_CREDITS_USED,
     +       RPT_FED_AMT_CREDITS_GENERATED,
     +       RPT_BTL_TAXABLE_INCOME,
     +       RPT_NON_BOOK_TAXABLE_ITEMS,
     +       RPT_BTL_TAXABLE_INTEREST_INCOME,
     +       RPT_BTL_TAXABLE_INVEST_EARNINGS,
     +       RPT_BTL_TAXABLE_LTI_INCOME,
     +       RPT_BTL_TAXABLE_STI_INCOME
C
      REAL*4 LTI_EARNINGS(0:12),STI_EARNINGS(0:12),
     +       CHANGE_IN_INVESTMENT_PAYABLE(0:12)
      REAL*4 R_BTL_CASH_EARNINGS,
     +       R_STI_INTEREST_INCOME,
     +       R_STD_CASH_INTEREST,
     +       R_STD_INCOME_INTEREST,
     +       R_NON_INCOME_TAXES_ACCRUAL_ADJ,
     +       R_STATE_TAXES_ACCRUAL_ADJ,
     +       R_FEDERAL_TAXES_ACCRUAL_ADJ
      REAL*4 LTD_ISSUE_YR_INTEREST_BOOKED,
     +       PS_ISSUE_YR_DIVIDEND_BOOKED,
     +       LTD_ISSUE_YR_INTEREST_PAYMENT,
     +       PS_ISSUE_YR_DIVIDEND_PAYMENT,
     +       LTD_RETIREMENTS,
     +       PS_RETIREMENTS,
     +       LTD_ISSUED_DISTRIBUTION(0:12),
     +       MGT_DEBIT_RETIREMENT(0:12), 
     +       SUB_FED_TAX_PAYMENTS_2_PARENT(0:12)/13*0./,
     +       FED_TAXES_PAYABLE_2_PARENT(0:12),
     +       SUB_STATE_TAX_PAYMENTS_2_PARENT(0:12)/13*0./,
     +       SUB_TAXABLE_INCOME_B4_DEDUCTS(0:12)/13*0./,
     +       SUB_STATE_TAXES_PAID(0:12)/13*0./,
     +       SUB_BTL_STATE_TAXES_PAID(0:12)/13*0./,
     +       SUB_BTL_FEDERAL_TAXES_PAID(0:12)/13*0./,
     +       SUB_BTL_STATE_TAXABLE_INCOME(0:12)/13*0./,
     +       SUB_BTL_TAXABLE_OTHER_INCOME(0:12)/13*0./,   ! 31 TAX TABLE
     +       SUB_BTL_TAXABLE_INVEST_INCOME(0:12)/13*0./,  ! 32 TAX TABLE
     +       SUB_BTL_TAXABLE_EXPENSES(0:12)/13*0./,       ! 95 TAX TABLE
     +       SUB_BTL_MISC_DEDUCTIONS(0:12)/13*0./,
     +       SUB_BTL_M1_TAX_DEDUCTIONS(0:12)/13*0./,
     +       SUB_DEFERRED_TAXES_DR(0:12)/13*0./,
     +       SUB_OTHER_TAXES(0:12)/13*0./,
     +       SUB_CASH_OTHER_TAXES(0:12)/13*0./,
     +       SUB_FASB87_DEF_TAX_DR_ADJ(0:12)/13*0./,
     +       CONSOLIDATED_CASH_STATE_TAXES(0:12)/13*0./,
     +       INTRA_COMPANY_EARNINGS_RECEIVABLE(0:12),
     +       PARENT_OTHER_LT_LIABS(0:12),
     +       SUB_FED_TAXES_PAID_2_PARENT,
     +       SUB_STATE_TAXES_PAID_2_PARENT,
     +       CONSOLIDATED_INCOME_TAXES_PAID,
     +       CONSOLIDATED_FEDERAL_TAXES_PAID,
     +       CONSOLIDATED_STATE_TAXES_PAID,
     +       CONSOLIDATED_BOY_FUNDS_BALANCE,
     +       CONSOLIDATED_INVESTMENT_INCOME_RECEIVABLE,
     +       PARENT_COMMON_DIVIDEND(0:12),
     +       PARENT_COMMON_STOCK_ISSUED(0:12),
     +       PARENT_CASH_DIVIDEND_PAID(0:12),
     +       NucDecom_Discount_Rate(0:12),
     +       FASB143_MONTHLY_ARO_INTEREST_ACCREATION(0:12),
     +       FASB143_MONTHLY_ARO_CASH_PAYMENTS(0:12),
     +       FASB143_MONTHLY_ARO_TRUST_CASH_PAYMENTS(0:12),
     +       FASB87_PENSION_LIAB_ADJ(0:12),
     +       FASB87_DEFFERED_TAXES_ADJ_DR(0:12),
     +       ARO_MONTHLY_LIABILITY_BAL_ADJ(0:12),
     +       ARO_MONTHLY_NET_ASSETS_BAL_ADJ(0:12),
     +       FASB143_MONTHLY_LT_LIABILITY_VALUE(0:12), 
     +       R_FASB143_LT_LIABILITY_VALUE(2),
     +       FASB143_MONTHLY_NET_ASSET_VALUE(0:12),
     +       R_FASB143_NET_ASSET_VALUE(2),
     +       NUC_DECOMMISSIONING_COST(0:12),
     +       R_FASB143_ARO_INTEREST_ACCREATION,
     +       R_ARO_CASH_PAYMENTS,
     +       R_ARO_TRUST_CASH_PAYMENTS,
     +       CURRENT_PORTION_OF_LTD(0:12)/13*0./
      LOGICAL*1 SUM_MONTHLY_INCOME_STATEMENT,
     +          SUM_MONTHLY_INCOME_REVENUES,
     +          SUM_MONTHLY_INCOME_EXPENSES,
     +          SUM_MONTHLY_TAXES_BTL_ITEMS,
     +          MONTHLY_TAXABLE_INCOME,
     +          VOID_LOGICAL,
     +          WVPA_MNTHLY_MEMBER_ACCRUED_REVS
      REAL (KIND=4) WVPA_OFF_SYSTEM_SALES_RECEIVABLE(0:12),
     +              WVPA_OFF_SYSTEM_SALES_PAYABLE(0:12),
     +              WVPA_INTERNAL_POWER_COSTS_PAYABLE(0:12)
      LOGICAL*1 CPL_ACTIVE,EMPIRE,UI,MPS,IPALCO,
     +          WVPA,SALT_RIVER_PROJECT,KCPL,IMPA,
     +          RETAIN_POST_RETIREMENT_EARNINGS
      LOGICAL*1 CS_PAY_MONTH(12)/12*.FALSE./

      INTEGER*2 CS_DECLARATION_MONTH(4)
      INTEGER*2 SKIP_MONTH,QUARTER
      INTEGER*2 CS_DIVIDEND_PAYMENT_LAG,STI_CASH_LAG
      INTEGER*2 CLASSES,R_ITER
      REAL*4 QUARTERY_CS_DIVIDENDS(4),MONTHLY_COMMON_DIVIDENDS(0:12)
      REAL*4 MONTHLY_AMOUNT,ANNUAL_AMOUNT
      REAL*4 CALCULATE_REVENUE_TAXES,VOID_REAL,AVERAGE_VALUES(0:12),
     +       CALC_LEVEL_REVENUE_BASED_TAX
      LOGICAL*1 PASS_2_ITS_PARENT,PASS_2_NEXT_LEVEL,WRITE_MONTHLY_INFOR,
     +          USE_OP_METH_ALLOCATOR,DONT_TALLY_THIS_CLASS
      CHARACTER*1 SBU,SUBSIDIARY,PARENT,REGULATED_GROUP,
     +            CONSOLIDATED,ELIMINATION
      PARAMETER (SBU='B',SUBSIDIARY='S',PARENT='P',REGULATED_GROUP='R',
     +           CONSOLIDATED='C',ELIMINATION='E')
      REAL*4 ANNUAL_VARS(0:VARIABLE_NUMBER-1)
      REAL*4 SAVE_ANNUAL_BOY_VARIABLE(0:VARIABLE_NUMBER-1),
     +       SAVE_ANNUAL_EOY_VARIABLE(0:VARIABLE_NUMBER-1)
      INTEGER*4 THIS_YEARS_RECORD,NEXT_YEARS_START_RECORD,
     +          INCREMENT_RECORDS
      CHARACTER*38 ASSET_CLASS_NAME,CLASS_TYPE*1,R_ASSET_CLASS_NAME
      INTEGER*2 CLASS_ID,R_YR,CLASS_LEVEL,CLASS_POS,YR
      REAL*4 PARENT_CS_DIV_CARRY_OVER,
     +       CUSTOMER_DEPOSITS_INTEREST,
     +       TEMP_CS_DIV_CARRY_OVER
      INTEGER*2 NUMBER_OF_REPORTING_CLASSES
      REAL*4 CASH_BTL_INTEREST_INCOME(0:12),
     +       CHANGE_IN_DEFERRED_DEBIT_BAL(0:12),
     +       DEFERRED_GAIN_ADJ(0:12),
     +       UNMODELED_SUB_INCOME(0:12),
     +       UNMODELED_SUB_DIVIDENDS(0:12)
C
      REAL*4 BALANCE_SHEET_VARIABLES(:,:),
     +       BALANCE_SHEET_DEACTIVE_VARS(:,:,:),
     +       TRANSFER_VARIABLES(:,:),
     +       TRANSFER_LINKED_LEVEL_VARIABLES(:,:,:),
     +       INCOME_VARIABLES(:,:),
     +       CASH_VARIABLES(:,:),
     +       TAX_VARIABLES(:,:),
     +       CPL_TAX__VARIABLES(:,:,:),
     +       INCOME_LINKED_LEVEL(:,:,:),
     +       CASH_LINKED_LEVEL(:,:,:),
     +       TAXES_LINKED_LEVEL(:,:,:),
     +       TAX_TRANSFER_VARIABLES(:,:,:),
     +       BALANCE_LINKED_LEVEL(:,:,:),
     +       PAYABLE_MONTHLY_VALUES(:,:,:),
     +       TEMP_PAYABLE_MONTHLY_VALUES(:,:,:),
     +       PAYABLE_LINKED_LOWER_LEVEL(:,:),
     +       RECEIVABLES_LINKED_LOWER_LEVEL(:,:),
     +       FEDERAL_INC_TAX_CARRY_OVER(:),
     +       TEMP_FEDERAL_INC_TAX_CARRY_OVER(:)
      ALLOCATABLE :: BALANCE_SHEET_VARIABLES,
     +               BALANCE_SHEET_DEACTIVE_VARS,
     +               TRANSFER_VARIABLES,
     +               TRANSFER_LINKED_LEVEL_VARIABLES,
     +               INCOME_VARIABLES,
     +               CASH_VARIABLES,
     +               TAX_VARIABLES,
     +               CPL_TAX__VARIABLES,
     +               INCOME_LINKED_LEVEL,
     +               CASH_LINKED_LEVEL,
     +               TAX_TRANSFER_VARIABLES,
     +               TAXES_LINKED_LEVEL,
     +               BALANCE_LINKED_LEVEL,
     +               PAYABLE_MONTHLY_VALUES,
     +               TEMP_PAYABLE_MONTHLY_VALUES,
     +               PAYABLE_LINKED_LOWER_LEVEL,
     +               RECEIVABLES_LINKED_LOWER_LEVEL,
     +               FEDERAL_INC_TAX_CARRY_OVER,
     +               TEMP_FEDERAL_INC_TAX_CARRY_OVER
C
      REAL*4 OPENING_CASH_BALANCES(:)
      ALLOCATABLE :: OPENING_CASH_BALANCES

      INTEGER*4 VALUES_2_SET,ZERO_POS/0./

      INTEGER*2 I,INCOME_UNIT,MONTHLY_INCOME_UNIT_NO,
     +          CASH_UNIT,MONTHLY_CASH_UNIT_NO,
     +          BALANCE_UNIT,MONTHLY_BALANCE_UNIT_NO,
     +          CPL_TAX_UNIT,CPL_TAX_REPORTING_UNIT_NO,
     +          TAX_UNIT,MONTHLY_TAX_UNIT_NO
      CHARACTER*6 SHORT_MONTH_NAMES,BAL_SHORT_MONTH_NAMES
      CHARACTER*10 LONG_MONTH_NAMES,BAL_LONG_MONTH_NAMES
      REAL*4 CIAC_MONTHLY_AMORT_RATE(0:12),
     +       CIAC_MONTHLY_ADDENDUM_2_AMORT(0:12),
     +       CIAC_MONTHLY_CASH(0:12),
     +       CIAC_MONTHLY_AMORTIZATION(0:12),
     +       CIAC_BALANCE(0:12),
     +       ATL_GAIN_ON_SALE_AMORT(0:12),
     +       BTL_GAIN_ON_SALE_AMORT(0:12)
C
      INTEGER*2 MO,MAX_LINKED_LEVEL,VARIABLE
      REAL*4 UNALLOCATED_AMOUNT(1:INCOME_VARS),OP_METH_ALLOCATOR
      REAL*4 ALLOCATE_ACCOUNT_BALANCE
      REAL*4 MONTHLY_OTHER_TAXES,MIDAS_ISSUED_COMMON,
     +       MIDAS_COMMON_SHARES_ISSUED
      INTEGER*2 EMC_PA_CLASS
      REAL*4 CATAWBA_REVENUES(0:12),
     +       CATAWBA_EXPENSES(0:12)
      LOGICAL*1 EXCLUDE_SBU_REVENUE_TAX,
     +          EXCLUDE_SBU_OTHER_TAXES,
     +          EXCLUDE_SBU_PROPERTY_TAX,
     +          EXCLUDE_SBU_STATE_INCOME_TAX,
     +          EXCLUDE_SBU_CAPITAL_TAX
      REAL*4 CPL_MONTHLY_FUEL_REVENUES(0:12),
     +       MONTHLY_DEFERRED_FUEL_EXPENSE(0:12),
     +       CPL_CASH_FUEL_REVENUES(0:12),
     +       PURCHASE_POWER_IN_DEFERRED_FUEL,
     +       FUEL_REVENUE_RECEIVABLE,
     +       R_FUEL_REVENUE_RECEIVABLE
      REAL*4 CATAWBA_CASH_REVENUES,
     +       CATAWBA_CASH_EXPENSES
      REAL*4 LTD_PAID_ISSUED_INTEREST(0:12),
     +       LTD_BOOKED_ISSUED_INTEREST(0:12)
!            PROXY FOR CALCULATING A PATTERN
      REAL*4 ATL_TAXABLE_INCOME(0:12), 
     +       TAX_INTEREST_DEDUCTION(0:12),
     +       BTL_TAXABLE_INCOME(0:12)
      REAL*4 CONSOLIDATED_VARIABLES(0:12,80:CASH_VARS)
      REAL*4 TOTAL_ASSETS,SUM_MONTHLY_LIABILITIES,
     +       TOTAL_LIABS,SUM_MONTHLY_ASSETS,
     +       SUM_MONTHLY_BALANCE_STATEMENT
C
      REAL*4 PROPERTY_TAX_ADJUSTMENT(0:12),
     +       MONTHLY_PROPERTY_TAX_RATE(0:12),
     +       OTHER_TAX_ADJUSTMENT(0:12),
     +       REVENUE_TAX_ADJUSTMENT(0:12),
     +       REVENUE_TAX_CURRENT_LEVEL(0:12),
     +       STATE_M1_ADDITIONS(0:12),
     +       STATE_M1_DEDUCTIONS(0:12),
     +       FEDERAL_M1_ADDITIONS(0:12),
     +       FEDERAL_M1_DEDUCTIONS(0:12),
     +       STATE_BTL_MISC_DEDUCTIONS(0:12),
     +       FEDERAL_BTL_MISC_DEDUCTIONS(0:12),
     +       MONTHLY_STATE_TAX_RATE(0:12),
     +       MONTHLY_FEDERAL_TAX_RATE(0:12),
     +       STATE_DEF_TAX_DR_FROM_NOLS(0:12),
     +       FED_DEF_TAX_DR_FROM_NOLS_AMT(0:12),
     +       STATE_INCOME_TAX_ADJUSTMENT(0:12),
     +       FEDERAL_INCOME_TAX_ADJUSTMENT(0:12),
     +       MONTHLY_PREVIOUS_OPT_REVS(0:12),
     +       MONTHLY_REVENUE_TAX_RATE(0:12),
     +       MONTHLY_OTHER_REV_TAX_RATE(0:12),
     +       MONTHLY_OTHER_EXP_TAX_RATE(0:12)
      REAL*4 CON_PROPERTY_TAX_ADJUSTMENT(0:12),
     +       CON_OTHER_TAX_ADJUSTMENT(0:12),
     +       CON_REVENUE_TAX_ADJUSTMENT(0:12),
     +       CON_STATE_M1_ADDITIONS(0:12),
     +       CON_STATE_M1_DEDUCTIONS(0:12),
     +       CON_FEDERAL_M1_ADDITIONS(0:12),
     +       CON_FEDERAL_M1_DEDUCTIONS(0:12),
     +       CON_STATE_BTL_MISC_DEDUCTIONS(0:12),
     +       CON_FEDERAL_BTL_MISC_DEDUCTIONS(0:12),
     +       CON_STATE_INCOME_TAX_ADJUSTMENT(0:12),
     +       CON_FED_INCOME_TAX_ADJUSTMENT(0:12),
     +       CONSOLD_FED_INCOME_B4_DEDUC(0:12),
     +       CONSOLD_FED_INCOME_AFTR_DEDUC(0:12),
     +       CONSOLD_FED_INC_TAXES_BOOKED(0:12),
     +       CONSOL_TRANSFER_LT_LIAB(0:12)
      INTEGER*2 RETURN_MONTHLY_CL_NF_INFO,VOID_INT2
      INTEGER*2 DEACTIVE_YR,MO_DEACT,
     +          ACTIVATE_YR,MO_ACTIVATE
      REAL*4 MONTHLY_NUC_FUEL_OWNED_BURN(0:12),
     +       MONTHLY_NUC_FUEL_LEASED_BURN(0:12)
      REAL*4 ENERGY_TO_PA,
     +       ENERGY_FROM_PA
      REAL*4 TEMP_EMISSION_DATA(0:12)
      REAL*4 DIVIDEND_ALLOCATION
      REAL*4 RATIO,UNALLOCATED_DEF_TAX_DR
      REAL*4 CONVERGENCE_TEST
      INTEGER*2 ITER
      real*4 test_total_exp_taxes,
     +       test_total_op_adj
      REAL*4 EOY_LONG_TERM_INVESTMENTS
      REAL*4 FUNDS_4_ST_INVESTMENTS
      REAL*4 LTD_ISSUED_THIS_MONTH
      INTEGER*2 DUMMY_CLASS_ID
      REAL*4 CONSOLIDATED_STATE_TAXES,
     +       CONSOLIDATED_FEDERAL_TAXES
      REAL*4 R_LTD_ISSUE_YR_INTEREST_BOOKED,
     +       R_LTD_ISSUE_YR_INTEREST_PAYMENT,
     +       R_MGT_DEBIT_RETIREMENT
C
      REAL*4 BOY_ACCUMULATED_DEP,
     +       BOY_RETAINED_EARNINGS,
     +       BOY_COMMON_STOCK,
     +       BOY_LTD_BALANCE,
     +       BOY_LTD_BALANCE_WO_CURRENT_LTD,
     +       BOY_PS_BALANCE,
     +       INVESTMENTS_BOY,
     +       NOTES_RECEIVABLE_BOY,
     +       NOTES_PAYABLE_BOY,
     +       DEFERRED_PURCHASE_POWER_BOY,
     +       PENSION_LIABILITY_BOY,
     +       DEFERRED_GAIN_FROM_SALES_BOY,
     +       STORM_RESERVE_BALANCE_BOY,
     +       VACATION_PAY_BALANCE_BOY,
     +       OTHER_INVESTMENTS_BALANCE_BOY,
     +       DEFERRED_DEBITS_BOY,
     +       DEFERRED_TAXES_DR_BOY,
     +       ACCOUNTS_RECEIVABLE_BOY,
     +       ASSETS_NEC_BOY,
     +       ACCUMLATED_BOOK_DEP_BOY,
     +       CUM_DEF_REVENUES_BOY,
     +       DEFERRED_TAXES_CR_BOY,
     +       DEFERRED_ITCS_BOY,
     +       OTHER_LT_LIABILITIES_BOY,
     +       ACCOUNTS_PAYABLE_BOY,
     +       LIABS_NEC_BOY,
     +       BOY_CWIP,
     +       BOY_POST_RETIRE_MEDICAL_FUND,
     +       BOY_POST_RETIRE_MED_PAYABLE,
     +       BOY_NUC_DECOM_FUND_LIABILITY,
     +       BOY_CAPITIALZIED_LEASES,
     +       BOY_NUC_DECOM_FUND_BAL,
     +       BOY_CLASS_NET_NF_VALUE,
     +       BOY_INVESTMENT_IN_SUBSIDIARIES,
     +       CONSOLIDATED_TAX_LIB_ADJUSTMENT,
     +       R_MONTHLY_NUC_DECOM_FUND_RETURN,
     +       R_OCI_NUCL_FUND_RETURN,
     +       R_MONTHLY_NUC_DECOM_FUND_NET_OF_TAX_RETURN, 
     +       R_RETIREMENT_MEDICAL_FUND_RETURN,
     +       R_OCI_RETIREMENT_MEDICAL_FUND_RETURN,
     +       R_NEW_SUB_INVESTMENT,
     +       R_GOODWILL_OF_NEW_SUB_INVESTMENT,
     +       NEW_ACQUISITIONS_COST(0:12),
     +       NEW_SUB_INVESTMENT(0:12),
     +       GOODWILL_OF_NEW_SUB_INVESTMENT(0:12)

      REAL*4 NOTES_PAYABLE_MADE(12),
     +       NOTES_RECEIVABLE_MADE(12),
     +       UNAMORTIZED_INTEREST_BALANCE(0:12)
      REAL*4 MONTHLY_CHANGE_IN_RECEIVABLES(0:12),
     +       MONTHLY_CHANGE_IN_PAYABLES(0:12),
     +       ELIM_OF_MONTHLY_CHANGE_IN_RECEIVABLES(0:12),
     +       ELIM_OF_MONTHLY_CHANGE_IN_PAYABLES(0:12)
      REAL*4 DEFERRED_GAIN_ROLLUP,
     +       ACCOUNTS_PAYABLE_ROLLUP,
     +       ACCOUNTS_RECEIVABLE_ROLLUP
      REAL*4 PARENT_DEFERRED_TAXES_DR(0:12)
      REAL*4 CONSOL_DEFERRED_TAXES_DR(0:12)
      INTEGER*4 IREC
      REAL*4 R_OUTPUT_CLASS_ID
      REAL*4 R_CIAC_BALANCE_BOY,
     +       R_CIAC_AMORTIZATION
      REAL*4 R_MONTHLY_COMMON_DIVIDENDS(0:12),
     +       R_COMMON_STOCK_DIV_ACCRUALS,
     +       R_COMMON_STOCK_CASH_DIVIDENDS
C
      REAL*4 R_PARENT_CS_DIV_CARRY_OVER
      INTEGER*2 R_MAX_LINKED_LEVEL,R_MO,
     +          R_NUMBER_OF_REPORTING_CLASSES,
     +          R_MAX_CLASS_NUM,
     +          R_NUM_OF_ACTIVE_CLASSES,
     +          R_MASTER_CLASS_LIST(0:R_MAX_CLASS_NUM),
     +          R_CS_DECLARATION_MONTH(4),
     +          R_SHARE_MONTH,
     +          R_PARENT_CLASS_ID_NUM
      REAL*4 R_OPENING_CASH_BALANCES(0:*)
      INTEGER*2 R_CLASS_ID,R_CLASS_LEVEL
      REAL*4 R_STD_INTEREST_OWED,
     +       R_REVENUE_TAXES,
     +       R_PROPERTY_TAXES,
     +       R_STATE_INCOME_TAXES,
     +       R_FEDERAL_INCOME_TAXES,
     +       R_STATE_TAX_ON_CAPITAL,
     +       R_FEDERAL_TAX_ON_CAPITAL,
     +       R_OTHER_TAXES_PAYABLE
      REAL*4 CHANGE_IN_ACCURED_FED_TAXES
C***********************************************************************
      INTEGER*2 ATL,BTL,TOTAL,VAR_TYPE,VAR,FIXED_ISSUE_MONTH
      PARAMETER(ATL=1,BTL=2,TOTAL=3,FIXED_ISSUE_MONTH=7)
      CHARACTER*14 TAX_TYPE_REPORTED(3)/'Operating',
     +                                  'Non-Operating',
     +                                  'Total'/
      REAL*4 CASH_NEEDED,CPL_MIN_LTD_ISSUE_AMOUNT
      REAL*4 R_LTD_ISSUED_DISTRIBUTION(0:12)
      REAL*4 STATE_NOLS_GEN_BY_SUBS(0:12),
     +       STATE_NOLS_USED_BY_SUBS(0:12),
     +       PARENT_STATE_TAXES(0:12),
     +       PARENT_STATE_DR_TAX_FROM_NOLS(0:12),
     +       SUB_STATE_DR_TAX_FROM_NOLS(0:12),
     +       SUB_FED_DR_TAX_FROM_NOLS_AMT(0:12),
     +       CUM_SUB_OFFSET,
     +       CONSOL_FED_INC_TAXES_B4_CREDITS(0:12),
     +       STATE_NOLS_GEN_AT_PARENT(0:12),
     +       STATE_NOLS_USED_AT_PARENT(0:12),
     +       ASSET_NEC_ADDENDUM(0:12),
     +       LIABS_NEC_ADDENDUM(0:12),
     +       ASSETS_NEC(0:12),
     +       LIABS_NEC(0:12)
      REAL*4 SUM_NONREGULATED_REVENUES,
     +       SUM_REGULATED_REVENUES
      REAL*4 SUM_CASH_RECEIPTS,SUM_CASH_PAYMENTS,
     +       SUM_CONSTRUCTION_CASH_PAYMENTS,
     +       SUM_CASH_TOTAL_CAPITAL_REQUIREMENTS
      REAL*4 LTD_BAL,LTD_CASH_CARRY_OVER,PAY_PERIODS_PER_YR,
     +       PIBIEN
      INTEGER*2 MO1,MTGLIFE
      CHARACTER*1 PAID_WHEN
      LOGICAL THIS_IS_A_PAY_MONTH
      REAL*4 STD_INTEREST_ADJ(0:12),
     +       STD_CASH_INTEREST_ADJ(0:12),
     +       LTD_INTEREST_ADJ(0:12),
     +       LTD_CASH_INTEREST_ADJ(0:12),
     +       STD_MONTHLY_INTEREST_RATE(0:12),
     +       CUSTOMER_DEPOSIT_INTEREST_RATE(0:12),
     +       MONTHLY_RETURN_ON_ST_INVEST(0:12),
     +       MONTHLY_LTI_RATE(0:12),
     +       MONTHLY_RETIREMENT_FUND_RATE(0:12),
     +       MONTHLY_NUC_DECOM_RATE(0:12),
     +       OCI_MTHLY_RETIREMENT_FUND_RATE(0:12),
     +       OCI_MONTHLY_NUC_DECOM_RATE(0:12)
      REAL*4 GAS_STORAGE_BOY,
     +       MATERIALS_SUPPLIES_BOY,
     +       FUEL_INVENTORY_BOY
      REAL*4 MONTHLY_NUC_DECOM_CASH_VALUES(0:12),
     +       MONTHLY_NUC_DECOM_EARNINGS_ADJ(0:12),
     +       MONTHLY_NUC_DECOM_FUND_RETURN(0:12),
     +       MONTHLY_OCI_NUC_DECOM_FUND_RETURN(0:12),
     +       MONTHLY_NUC_DECOM_FUND_NET_OF_TAX_RETURN(0:12),
     +       MONTHLY_NUC_DECOM_BALANCE(0:12),
     +       FEDERAL_EPA_92_TAX_RATE
      REAL MONTHLY_POST_RETIREMENT_CASH_PAYMENTS(0:12),
     +     MONTHLY_CASH_TO_POST_RETIREMENT(0:12),
     +     MONTHLY_RETIREE_MEDICAL_CASH_PAYMENTS(0:12),
     +     MONTHLY_RETIREMENT_FUND_EARNINGS_ADJ(0:12),
     +     RETIREMENT_MEDICAL_FUND_RETURN(0:12),
     +     OCI_RETIREMENT_MEDICAL_FUND_RETURN(0:12),
     +     POST_RETIREMENT_MEDICAL_FUND_BAL
      REAL MONTHLY_CAPACITY_SALES_TO_LEVEL_RM(0:12),
     +     MONTHLY_CAPACITY_PURCHASES_TO_LEVEL_RM(0:12)
      CHARACTER*1 ADD_2_INCOME_STATEMENT
      REAL RETURN_NF_DEFERRED_TAXES
      LOGICAL*1 CLASS_IS_ACTIVE(0:12)
      REAL*4 NEW_ACQUISITION_DEF_TAXES_CR(0:12)/13*0./,
     +       NEW_ACQUISITION_ACCOUNTS_PAYABLE_BAL(0:12)/13*0./
      REAL (KIND=4) :: CumulatedBalances(0:12)
      REAL (KIND=4), ALLOCATABLE :: CLASS_GOODWILL_EOY(:)
      REAL (KIND=4) :: PurPowerFromCongestion(0:12)

C END VARIABLE DECLARATIONS
C
         PurPowerFromCongestion=0.
		 
         YR = R_YR
         
         INCOME_VARIABLES = 0.
         CASH_VARIABLES = 0.
         TAX_VARIABLES = 0.
         TRANSFER_VARIABLES = 0.
         IF(R_ITER == 1) THEN

            STI_EARNINGS = 0.
            LTI_EARNINGS = 0.
            CALL CLASS_DEACTIVATE_IN_YR(CLASS_ID,DEACTIVE_YR,MO_DEACT)
            CALL CLASS_ACTIVATE_IN_YR(CLASS_ID,ACTIVATE_YR,MO_ACTIVATE)
            CLASS_IS_ACTIVE = .TRUE.
            IF(YR < ACTIVATE_YR) CLASS_IS_ACTIVE = .FALSE. 
            IF(YR > DEACTIVE_YR) CLASS_IS_ACTIVE = .FALSE. 
            IF(YR == DEACTIVE_YR) 
     +                  CLASS_IS_ACTIVE(MIN(12,MO_DEACT+1):12) = .FALSE.
               
            IF(YR == ACTIVATE_YR)
     +                CLASS_IS_ACTIVE(0:MAX(0,MO_ACTIVATE-1)) = .FALSE. 
            BALANCE_SHEET_VARIABLES = 0.

            CALL RETURN_SBU_PASS_THROUGH_VALUES(EXCLUDE_SBU_REVENUE_TAX,
     +                                     EXCLUDE_SBU_OTHER_TAXES,
     +                                     EXCLUDE_SBU_PROPERTY_TAX,
     +                                     EXCLUDE_SBU_STATE_INCOME_TAX,
     +                                     EXCLUDE_SBU_CAPITAL_TAX)
            CPL_MONTHLY_FUEL_REVENUES = 0.
            MONTHLY_DEFERRED_FUEL_EXPENSE = 0.
            CPL_CASH_FUEL_REVENUES = 0.
            CASH_BTL_INTEREST_INCOME = 0.
            TAX_INTEREST_DEDUCTION = 0.
            MONTHLY_NUC_FUEL_OWNED_BURN = 0.
            MONTHLY_NUC_FUEL_LEASED_BURN = 0.
            STD_INTEREST_ADJ = 0.
            STD_CASH_INTEREST_ADJ = 0.
            LTD_INTEREST_ADJ = 0.
            LTD_CASH_INTEREST_ADJ = 0.
C
            FUEL_REVENUE_RECEIVABLE = 0.
            IF(USE_DEFERRED_FUEL_ACCOUNTING .AND. CPL_ACTIVE()) THEN
               CALL CPL_MONTHLY_DEFERRED_FUEL(CLASS_ID,
     +                                    CPL_MONTHLY_FUEL_REVENUES,
     +                                    MONTHLY_DEFERRED_FUEL_EXPENSE,
     +                                    CPL_CASH_FUEL_REVENUES,
     +                                    FUEL_REVENUE_RECEIVABLE)
            ENDIF
            CALL MONTHLY_INTEREST_EXPENSES(CLASS_ID,R_YR,
     +                                     STD_INTEREST_ADJ,
     +                                     STD_CASH_INTEREST_ADJ,
     +                                     LTD_INTEREST_ADJ,
     +                                     LTD_CASH_INTEREST_ADJ)
            CALL MONTHLY_TAX_ADJUSTMENTS(CLASS_ID,R_YR,
     +                                   PROPERTY_TAX_ADJUSTMENT,
     +                                   OTHER_TAX_ADJUSTMENT,
     +                                   REVENUE_TAX_ADJUSTMENT,
     +                                   STATE_M1_ADDITIONS,
     +                                   STATE_M1_DEDUCTIONS,
     +                                   FEDERAL_M1_ADDITIONS,
     +                                   FEDERAL_M1_DEDUCTIONS,
     +                                   STATE_BTL_MISC_DEDUCTIONS,
     +                                   FEDERAL_BTL_MISC_DEDUCTIONS,
     +                                   STATE_INCOME_TAX_ADJUSTMENT,
     +                                   FEDERAL_INCOME_TAX_ADJUSTMENT)
C
            CALL RETURN_DEBIT_MONTHLY_TAX_VALUES(CLASS_ID,R_YR+INT2(1),
     +                                           TAX_VARIABLES)
C
            CALL DEBT_MONTHLY_TAX_INFO(R_YR,CLASS_ID,TAX_VARIABLES)
C
            CALL MONTHLY_CLASS_OPERATION_REVS(CLASS_POS,
     +                                        MONTHLY_PREVIOUS_OPT_REVS)
            CALL MONTHLY_WORKING_CAPITAL_ADJ(R_YR,CLASS_ID,
     +                                       ASSET_NEC_ADDENDUM,
     +                                       LIABS_NEC_ADDENDUM)
C
C RESERVE MARGIN PURCHASE/SALES
C
            CALL RETURN_MONTHLY_LEVEL_SALES_PURCHASES(CLASS_ID,
     +                           MONTHLY_CAPACITY_SALES_TO_LEVEL_RM,
     +                           MONTHLY_CAPACITY_PURCHASES_TO_LEVEL_RM,
     +                           ADD_2_INCOME_STATEMENT)
C
            IF(CLASS_TYPE == SUBSIDIARY) THEN
               CLASS_LINK_2 = -1
            ELSE
               CLASS_LINK_2 = CLASS_LEVEL-1
            ENDIF
C
C MONTHLY INTEREST RATES
C
            CALL RETURN_MONTHLY_NEW_ISSUE_RATES(R_YR,CLASS_ID,
     +                                  STD_INTEREST_RATE,
     +                                  RETURN_ON_ST_INVEST,
     +                                  INTEREST_ON_CUSTOMER_DEPOSITS,
     +                                  RETURN_ON_LONG_TERM_INVESTMENTS,
     +                                  RETURN_RETIREMENT_MEDICAL_FUND,
     +                                  NUCL_DECOM_FUND_RETURN,
     +                                  STD_MONTHLY_INTEREST_RATE,
     +                                  MONTHLY_RETURN_ON_ST_INVEST,
     +                                  CUSTOMER_DEPOSIT_INTEREST_RATE,
     +                                  MONTHLY_LTI_RATE,
     +                                  MONTHLY_RETIREMENT_FUND_RATE,
     +                                  MONTHLY_NUC_DECOM_RATE)
C
C MONTHLY TAX RATES
C
            CALL MONTHLY_NON_INCOME_TAX_RATES(R_YR,   
     +                                       CLASS_ID,
     +                                       MONTHLY_REVENUE_TAX_RATE,
     +                                       MONTHLY_PROPERTY_TAX_RATE,
     +                                       MONTHLY_OTHER_REV_TAX_RATE,
     +                                       MONTHLY_OTHER_EXP_TAX_RATE,
     +                                       CLASS_TYPE)

C

               MONTHLY_STATE_TAX_RATE(:) =  STATE_TAX_RATE
               MONTHLY_FEDERAL_TAX_RATE(:) = FEDERAL_TAX_RATE
               STATE_M1_DEDUCTIONS(:) = STATE_M1_DEDUCTIONS(:)
     +                 + TAX_VARIABLES(:,monthly_atl_debit_tax_expense)
     +                 + TAX_TRANSFER_VARIABLES(:,
     +                        monthly_atl_debit_tax_expense,CLASS_LEVEL)
     +                 + TAX_VARIABLES(:,monthly_btl_debit_tax_expense)
     +                 + TAX_TRANSFER_VARIABLES(:,
     +                        monthly_btl_debit_tax_expense,CLASS_LEVEL)

               STATE_BTL_MISC_DEDUCTIONS(:) =
     +                 STATE_BTL_MISC_DEDUCTIONS(:)
     +                 + TAX_VARIABLES(:,monthly_btl_debit_tax_expense)
     +                 + TAX_TRANSFER_VARIABLES(:,
     +                        monthly_btl_debit_tax_expense,CLASS_LEVEL)
               FEDERAL_M1_DEDUCTIONS(:) = FEDERAL_M1_DEDUCTIONS(:)
     +                 + TAX_VARIABLES(:,monthly_atl_debit_tax_expense)
     +                 + TAX_TRANSFER_VARIABLES(:,
     +                        monthly_atl_debit_tax_expense,CLASS_LEVEL)
     +                 + TAX_VARIABLES(:,monthly_btl_debit_tax_expense)
     +                 + TAX_TRANSFER_VARIABLES(:,
     +                        monthly_btl_debit_tax_expense,CLASS_LEVEL)

               FEDERAL_BTL_MISC_DEDUCTIONS(:) =
     +                 FEDERAL_BTL_MISC_DEDUCTIONS(:)
     +                 + TAX_VARIABLES(:,monthly_btl_debit_tax_expense)
     +                 + TAX_TRANSFER_VARIABLES(:,
     +                        monthly_btl_debit_tax_expense,CLASS_LEVEL)
               IF(.NOT. DONT_TALLY_THIS_CLASS) THEN
                  TAX_TRANSFER_VARIABLES(:,
     +                     monthly_atl_debit_tax_expense,CLASS_LINK_2) =
     +                 TAX_TRANSFER_VARIABLES(:,
     +                       monthly_atl_debit_tax_expense,CLASS_LINK_2)
     +                 + TAX_VARIABLES(:,monthly_atl_debit_tax_expense)
     +                 + TAX_TRANSFER_VARIABLES(:,
     +                        monthly_atl_debit_tax_expense,CLASS_LEVEL)
                  TAX_TRANSFER_VARIABLES(:,
     +                     monthly_btl_debit_tax_expense,CLASS_LINK_2) =
     +                 TAX_TRANSFER_VARIABLES(:,
     +                       monthly_btl_debit_tax_expense,CLASS_LINK_2)
     +                 + TAX_VARIABLES(:,monthly_btl_debit_tax_expense)
     +                 + TAX_TRANSFER_VARIABLES(:,
     +                        monthly_btl_debit_tax_expense,CLASS_LEVEL)
               ENDIF
               TAX_TRANSFER_VARIABLES(:,
     +                   monthly_atl_debit_tax_expense,CLASS_LEVEL) = 0.
               TAX_TRANSFER_VARIABLES(:,
     +                   monthly_btl_debit_tax_expense,CLASS_LEVEL) = 0.

C
            PROPERTY_TAX_ADJUSTMENT = PROPERTY_TAX_ADJUSTMENT
     +                   + INCOME_LINKED_LEVEL(:,
     +                          mth_modl_calcd_prop_taxes,
     +                                                      CLASS_LEVEL)
            NEW_ACQUISITIONS_COST = 0.
            GOODWILL_OF_NEW_SUB_INVESTMENT = 0.
            NEW_SUB_INVESTMENT = 0.
            NEW_SUB_INVESTMENT(1) = R_NEW_SUB_INVESTMENT            
            GOODWILL_OF_NEW_SUB_INVESTMENT(1) =
     +                                  R_GOODWILL_OF_NEW_SUB_INVESTMENT
            NEW_ACQUISITIONS_COST(1:) = NEW_SUB_INVESTMENT(1:)
     +                              + GOODWILL_OF_NEW_SUB_INVESTMENT(1:)
            NEW_ACQUISITIONS_COST(0) = SUM(NEW_ACQUISITIONS_COST(1:))
            DO MO = 1, 12 

               NEW_SUB_INVESTMENT(MO) = NEW_SUB_INVESTMENT(MO-1)  
     +                                  + NEW_SUB_INVESTMENT(MO) 
            ENDDO  
         ENDIF
C
C GET THE GAIN AMORTIZATIONS
C
         CALL MONTHLY_ADDENDUM_CAL_VALUES(CLASS_ID,R_YR,
     +                                    INCOME_VARIABLES)
C
C CALCUALATE ASSETS AND LIABS NEC
C
         ASSETS_NEC(0) = ASSETS_NEC_BOY
         ASSETS_NEC(12) = ASSETS_NEC_EOY
         LIABS_NEC(0) = LIABS_NEC_BOY
         LIABS_NEC(12) = LIABS_NEC_EOY
         DO MO = 1, 11
            ASSETS_NEC(MO) = 12. * OPREV_IN_ASSETS_NEC *
     +                       INCOME_VARIABLES(MO,monthly_total_revenues)
     +                       + NPV_IN_ASSETS_NEC * NET_UTILITY_PLANT_EOY
     +                       + ASSET_NEC_ADDENDUM(MO)
            LIABS_NEC(MO) = 12. * OPREV_IN_LIBS_NEC *
     +                      INCOME_VARIABLES(MO,monthly_total_revenues)
     +                      + NPV_IN_LIBS_NEC * NET_UTILITY_PLANT_EOY
     +                      + LIABS_NEC_ADDENDUM(MO)
         ENDDO

         PAYABLE_MONTHLY_VALUES(:,:,CLASS_POS) =
     +                        TEMP_PAYABLE_MONTHLY_VALUES(:,:,CLASS_POS)
         CASH_VARIABLES(:,cash_rcd_accts_rcvbl) =
     +             PAYABLE_MONTHLY_VALUES(:,payment_revenues_receivable,
     +                                                        CLASS_POS)
         PAYABLE_MONTHLY_VALUES(:,payment_revenues_receivable,
     +                                                   CLASS_POS) = 0.
         CASH_VARIABLES(:,Cash_Paid_on_Accounts_Payable) =
     +                PAYABLE_MONTHLY_VALUES(:,payment_expenses_payable,
     +                                                        CLASS_POS)
         PAYABLE_MONTHLY_VALUES(:,payment_expenses_payable,CLASS_POS)=0.
C
C EXPENSE ITEMS 31-80 AND REVENUES ITEMS 1-30
C
         TEMP_CS_DIV_CARRY_OVER = PARENT_CS_DIV_CARRY_OVER
         IF(CLASS_TYPE/=CONSOLIDATED .AND. CLASS_TYPE/=ELIMINATION) THEN
            CALL MONTHLY_REVENUES(R_YR,CLASS_ID,INCOME_VARIABLES)
            CALL MONTHLY_EXISTING_ASSET_INFO(R_YR,CLASS_ID,
     +                                       INCOME_VARIABLES)
            CALL MONTHLY_FA_INCOME_RESULTS(R_YR,CLASS_ID,
     +                                                 INCOME_VARIABLES)
            CALL MONTHLY_EXPENSES(R_YR,CLASS_ID,
     +                           INCOME_VARIABLES(0, EXP_OFFSET_LINE+1))
c
c nuclear fuel expenses and taxes
c
            INCOME_VARIABLES(:,Mthy_Leasd_Nucl_Fuel_Expns) =
     +                  INCOME_VARIABLES(:,Monthly_Nuclear_Fuel_Expense)
            INCOME_VARIABLES(:,Monthly_Nuclear_Fuel_Expense) = 
     +         INCOME_VARIABLES(:,monthly_ownd_nuc_fl_expense)
     +         + INCOME_VARIABLES(:,Mthy_Leasd_Nucl_Fuel_Expns)
            INCOME_VARIABLES(:,monthly_nuclr_fuel_tax_expns) = 
     +           INCOME_VARIABLES(:,Mthy_Leasd_Nucl_Fuel_Expns)
     +           + INCOME_VARIABLES(:,MonthlyDOEDisposal)
             

            CALL MONTHLY_CASH_REV_EXP(R_YR,CLASS_ID,CASH_VARIABLES)
            CALL MONTHLY_FA_CASH_RESULTS(R_YR,CLASS_ID,CASH_VARIABLES)
            CALL RETURN_AI_MONTHLY_CLASS_VALUES(CLASS_ID,
                                          !NOTE: ADD 1 WHEN USED
     +                                          R_YR+INT2(1), 
     +                                          CASH_VARIABLES)
            CALL RETURN_MONTHLY_DERIVATIVE_VALS(R_YR,CLASS_ID,
     +                                          INCOME_VARIABLES,
     +                                          CASH_VARIABLES)
            CALL WVPA_MONTHLY_SPECIAL_ADDENDUMS(CLASS_ID,R_YR,
     +                    INCOME_VARIABLES(0,Monthly_Book_Depreciation))

            INCOME_VARIABLES(0:12,monthly_purchased_power) =
     +                    INCOME_VARIABLES(0:12,monthly_purchased_power)
     +                    + PurPowerFromCongestion(0:12)
            CASH_VARIABLES(0:12,Cash_Purchased_Power) =
     +                    CASH_VARIABLES(0:12,Cash_Purchased_Power)
     +                    + PurPowerFromCongestion(0:12)
            PURCHASE_POWER_IN_DEFERRED_FUEL = 0.
            IF(USE_PURCHASE_POWER_ACCOUNTING)
     +                              PURCHASE_POWER_IN_DEFERRED_FUEL = 1.
            IF(USE_DEFERRED_FUEL_ACCOUNTING) THEN
               IF(CPL_ACTIVE()) THEN
                  DO MO = 0, 12
                     INCOME_VARIABLES(MO,AdjustmentClause) =
     +                            INCOME_VARIABLES(MO,AdjustmentClause)
     +                            + CPL_MONTHLY_FUEL_REVENUES(MO)
                     INCOME_VARIABLES(MO,MonthlyDeferredFuelExpense)=
     +                     INCOME_VARIABLES(MO,
     +                                    MonthlyDeferredFuelExpense)
     +                     + MONTHLY_DEFERRED_FUEL_EXPENSE(MO)
                     CASH_VARIABLES(MO,Cash_Adjustment_Clause) =
     +                         CASH_VARIABLES(MO,Cash_Adjustment_Clause)
     +                         + CPL_CASH_FUEL_REVENUES(MO)
                  ENDDO
               ELSE
                  DO MO = 0, 12
                     INCOME_VARIABLES(MO,MonthlyDeferredFuelExpense)=
     +                 INCOME_VARIABLES(MO,AdjustmentClause)
     +                 - INCOME_VARIABLES(MO,Monthly_Fossil_Fuel)
     +                 - INCOME_VARIABLES(MO,
     +                                     Monthly_Nuclear_Fuel_Expense)
     +                 - INCOME_VARIABLES(MO,monthly_purchased_power) *
     +                                   PURCHASE_POWER_IN_DEFERRED_FUEL
                  ENDDO
               ENDIF
            ENDIF
C           
            R_FUEL_REVENUE_RECEIVABLE = FUEL_REVENUE_RECEIVABLE
            CATAWBA_RECEIVABLES = 0.
            CATAWBA_PAYABLES = 0.
            IF(CPL_ACTIVE() .AND. CLASS_ID == EMC_PA_CLASS()) THEN
              CALL CPL_MONTHLY_REV_AND_EXP(CATAWBA_REVENUES,
     +                                      CATAWBA_EXPENSES,
     +                                      ENERGY_TO_PA,
     +                                      ENERGY_FROM_PA)
C
C ADD IN THE REVENUES AND EXPENSES
C
               CATAWBA_CASH_REVENUES = 0.
               CATAWBA_CASH_EXPENSES = 0.
               DO MO = 0, 12
                  INCOME_VARIABLES(MO,MonthlyCatawbaExpenses) =
     +                     INCOME_VARIABLES(MO,MonthlyCatawbaExpenses)
     +                     + CATAWBA_EXPENSES(MO)
                  INCOME_VARIABLES(MO,MonthlyCatawbaRevenues) =
     +                     INCOME_VARIABLES(MO,MonthlyCatawbaRevenues)
     +                     + CATAWBA_REVENUES(MO)
                  IF(MO > 0) THEN
                     IF(MO < 12) THEN
                        CATAWBA_CASH_REVENUES = CATAWBA_CASH_REVENUES
     +                                          + CATAWBA_REVENUES(MO)
                        CATAWBA_CASH_EXPENSES = CATAWBA_CASH_EXPENSES
     +                                          + CATAWBA_EXPENSES(MO)
                     ENDIF
                     CATAWBA_CASH_REVENUES = CATAWBA_CASH_REVENUES
     +                          + PAYABLE_MONTHLY_VALUES(MO,
     +                               payment_catawba_revenues,CLASS_POS)
                     CATAWBA_CASH_EXPENSES = CATAWBA_CASH_EXPENSES
     +                          + PAYABLE_MONTHLY_VALUES(MO,
     +                               payment_catawba_expenses,CLASS_POS)
                  ENDIF
               ENDDO
C
               CATAWBA_RECEIVABLES = CATAWBA_REVENUES(0)
     +                               - CATAWBA_CASH_REVENUES
               CATAWBA_PAYABLES = CATAWBA_EXPENSES(0)
     +                            - CATAWBA_CASH_EXPENSES
C
               CASH_VARIABLES(1,cash_catawba_expenses) =
     +                           CASH_VARIABLES(1,cash_catawba_expenses)
     +                           + PAYABLE_MONTHLY_VALUES(1,
     +                               payment_catawba_expenses,CLASS_POS)
               CASH_VARIABLES(1,cash_catawba_revenues) =
     +                           CASH_VARIABLES(1,cash_catawba_revenues)
     +                           + PAYABLE_MONTHLY_VALUES(1,
     +                               payment_catawba_revenues,CLASS_POS)
C
               DO MO = 2, 12
                  CASH_VARIABLES(MO,cash_catawba_expenses) =
     +                          CASH_VARIABLES(MO,cash_catawba_expenses)
     +                          + CATAWBA_EXPENSES(MO-1)
     +                          + PAYABLE_MONTHLY_VALUES(MO,
     +                               payment_catawba_expenses,CLASS_POS)
C                  
                  CASH_VARIABLES(MO,cash_catawba_revenues) =
     +                          CASH_VARIABLES(MO,cash_catawba_revenues)
     +                          + CATAWBA_REVENUES(MO-1)
     +                          + PAYABLE_MONTHLY_VALUES(MO,
     +                               payment_catawba_revenues,CLASS_POS)
               ENDDO
C
               PAYABLE_MONTHLY_VALUES(1,payment_catawba_expenses,
     +                                                     CLASS_POS) = 
     +                                              CATAWBA_EXPENSES(12)
               PAYABLE_MONTHLY_VALUES(0,payment_catawba_expenses,
     +                                                   CLASS_POS) = 0.
               PAYABLE_MONTHLY_VALUES(1,payment_catawba_revenues,
     +                                                     CLASS_POS) = 
     +                                              CATAWBA_REVENUES(12)
               PAYABLE_MONTHLY_VALUES(0,payment_catawba_revenues,
     +                                                   CLASS_POS) = 0.
               CASH_VARIABLES(0,cash_catawba_expenses) =
     +                     SUM(CASH_VARIABLES(1:,cash_catawba_expenses))
               CASH_VARIABLES(0,cash_catawba_revenues) =
     +                     SUM(CASH_VARIABLES(1:,cash_catawba_revenues))
C
            ENDIF ! END CPL ACTIVE
C
C GET THE O&M AMORTIZATION
C
            CALL RETURN_DEBIT_MONTHLY_VALUES(CLASS_ID,R_YR+INT2(1),
     +                                       INCOME_VARIABLES,
     +                                       CASH_VARIABLES)
            CALL RETURN_MONTHLY_CASH_ADDENDUMS(CLASS_ID,R_YR,
     +                                         CASH_VARIABLES,
     +                                         INCOME_VARIABLES)
C
            IF(R_ITER == 1) THEN

C               DO MO = 0, 12
                  CHANGE_IN_DEFERRED_DEBIT_BAL(:) =  ! due to debit file amortizations
     +                 INCOME_VARIABLES(:,monthly_other_atl_amort)
     +                 + INCOME_VARIABLES(:,Monthly_Goodwill_Amort)
     +                 + INCOME_VARIABLES(:,
     +                                  Monthly_Regulatory_Assets_Amort)
     +                 + INCOME_VARIABLES(:,Monthly_FASB_109_Amort)
     +                 + INCOME_VARIABLES(:,Monthly_FASB_133_Amort)
     +                 + INCOME_VARIABLES(:,monthly_other_btl_amort)
     +                 + INCOME_VARIABLES(:,
     +                          mthly_debit_fl_Pchs_pwr_amort)
     +                 + INCOME_VARIABLES(:,
     +                                mty_dbt_fl_intrst_amrt)
     +                 + INCOME_VARIABLES(:,
     +                                 mt_dt_file_intrst_amrt)
C
C LOWER LEVEL DEBIT AMORTZATION WAS NOT BEING PASSED UP.
C
     +                 + INCOME_LINKED_LEVEL(:,
     +                              monthly_other_atl_amort,CLASS_LEVEL)
     +                 + INCOME_LINKED_LEVEL(:,Monthly_Goodwill_Amort,
     +                                                      CLASS_LEVEL)
     +                 + INCOME_LINKED_LEVEL(:,
     +                                  Monthly_Regulatory_Assets_Amort,
     +                                                      CLASS_LEVEL)
     +                 + INCOME_LINKED_LEVEL(:,Monthly_FASB_109_Amort,
     +                                                      CLASS_LEVEL)
     +                 + INCOME_LINKED_LEVEL(:,Monthly_FASB_133_Amort,
     +                                                      CLASS_LEVEL)
     +                 + INCOME_LINKED_LEVEL(:,
     +                              monthly_other_btl_amort,CLASS_LEVEL)
     +                 + INCOME_LINKED_LEVEL(:,
     +                       mthly_debit_fl_Pchs_pwr_amort,
     +                                                      CLASS_LEVEL)
     +                 + INCOME_LINKED_LEVEL(:,
     +                    mty_dbt_fl_intrst_amrt,CLASS_LEVEL)
C               ENDDO
            ENDIF
            CALL DEBT_MONTHLY_INCOME_INFO(R_YR,CLASS_ID,
     +                                    INCOME_VARIABLES,
     +                                    CASH_VARIABLES)
            CALL CURRENT_PORTION_OF_LTD_INFO(R_YR,CLASS_ID,
     +                                       CURRENT_PORTION_OF_LTD)
c            CURRENT_PORTION_OF_LTD(0) = LTD_RETIREMENTS
C            DO MO = 0, 12
               INCOME_VARIABLES(:,Monthly_LTD_Amort_Interest) =
     +                        INCOME_VARIABLES(:,
     +                                 mt_dt_file_intrst_amrt)
     +                        + INCOME_VARIABLES(:,
     +                                mty_dbt_fl_intrst_amrt)
     +                        + INCOME_LINKED_LEVEL(:,
     +                                mty_dbt_fl_intrst_amrt,
     +                                                      CLASS_LEVEL)
     +                        - INCOME_VARIABLES(:,
     +                             mty_dbt_fl_intrst_amrt_cr)
     +                        - INCOME_LINKED_LEVEL(:,
     +                             mty_dbt_fl_intrst_amrt_cr,
     +                                                      CLASS_LEVEL)
               INCOME_VARIABLES(0,Monthly_LTD_Amort_Interest) =
     +              SUM(INCOME_VARIABLES(1:,Monthly_LTD_Amort_Interest))
               UNMODELED_SUB_INCOME(:) = 
     +                     INCOME_VARIABLES(:,Monthly_Subsidiary_Income)
               UNMODELED_SUB_DIVIDENDS(:) =
     +                CASH_VARIABLES(:,cash_subsidiary_dividends)
     
C            ENDDO
            MIDAS_ISSUED_COMMON = ANNUAL_VARS(101)
     +                     - CASH_VARIABLES(0,cash_common_stock_issued)
     +                     + CASH_VARIABLES(0,cash_common_stock_buyback)
            CASH_VARIABLES(0,cash_common_stock_issued) = 
     +                        CASH_VARIABLES(0,cash_common_stock_issued)
     +                        + MIDAS_ISSUED_COMMON
            CASH_VARIABLES(7,cash_common_stock_issued) =
     +                        CASH_VARIABLES(7,cash_common_stock_issued)
     +                        + MIDAS_ISSUED_COMMON
            MIDAS_COMMON_SHARES_ISSUED = ANNUAL_VARS(299)
     +                          - ANNUAL_VARS(409)
     +                          - CASH_VARIABLES(0,common_shares_issued)
            CASH_VARIABLES(0,common_shares_issued) =
     +                            CASH_VARIABLES(0,common_shares_issued)
     +                            + MIDAS_COMMON_SHARES_ISSUED
            CASH_VARIABLES(7,common_shares_issued) =
     +                            CASH_VARIABLES(7,common_shares_issued)
     +                            + MIDAS_COMMON_SHARES_ISSUED
            INCOME_VARIABLES(0,Monthly_Shares_Outstanding) =
     +                                                  ANNUAL_VARS(409)
            DO MO = 1, 12
               INCOME_VARIABLES(MO,Monthly_Shares_Outstanding) =
     +                 INCOME_VARIABLES(MO-1,Monthly_Shares_Outstanding)
     +                 + CASH_VARIABLES(MO,common_shares_issued)
            ENDDO
C
            CASH_VARIABLES(7,cash_ps_issued) =
     +                                  CASH_VARIABLES(7,cash_ps_issued)
     +                                  + PREFERRED_STOCK_ISSUED
            CASH_VARIABLES(0,cash_ps_issued) =
     +                                  CASH_VARIABLES(0,cash_ps_issued)
     +                                  + PREFERRED_STOCK_ISSUED
            CASH_VARIABLES(7,cash_ltd_issued) =
     +                                 CASH_VARIABLES(7,cash_ltd_issued)
     +                                 + LONG_TERM_DEBT_ISSUED
            CASH_VARIABLES(0,cash_ltd_issued) =
     +                                 CASH_VARIABLES(0,cash_ltd_issued)
     +                                 + LONG_TERM_DEBT_ISSUED
         ENDIF ! CLASS TYPE
C
C EMISSION CREDITS
C
         IF(CLASS_TYPE == PARENT) THEN
            CLASS_TYPE = PARENT
         ENDIF
         TEMP_EMISSION_DATA(:) =
     +                 INCOME_VARIABLES(:,MonthlyEmissionCredits)
     +                 + INCOME_LINKED_LEVEL(:,MonthlyEmissionCredits,
     +                                                      CLASS_LEVEL)
         IF(ABS(ANNUAL_VARS(15) - TEMP_EMISSION_DATA(0)) > .001) THEN
            UNALLOCATED_AMOUNT(MonthlyEmissionCredits) =
     +                      ALLOCATE_ACCOUNT_BALANCE(ANNUAL_VARS(15),
     +                                               TEMP_EMISSION_DATA)
            INCOME_VARIABLES(:,MonthlyEmissionCredits) =
     +                 TEMP_EMISSION_DATA(:)
     +                 - INCOME_LINKED_LEVEL(:,MonthlyEmissionCredits,
     +                                                      CLASS_LEVEL)
         ELSE
            INCOME_VARIABLES(:,MonthlyEmissionCredits) =
     +                                             TEMP_EMISSION_DATA(:)
         ENDIF
         CASH_VARIABLES(:,CashEmissionCredits) =
     +                  CASH_VARIABLES(:,CashEmissionCredits)
     +                  + INCOME_VARIABLES(:,MonthlyEmissionCredits)

C ADD PREVIOUS LEVELS
C
         CASH_VARIABLES(:,cash_aro_payments) =
     +                              FASB143_MONTHLY_ARO_CASH_PAYMENTS(:)
         CASH_VARIABLES(:,cash_aro_trust_payments) =
     +                        FASB143_MONTHLY_ARO_TRUST_CASH_PAYMENTS(:)
         INCOME_VARIABLES(:,Monthly_FASB_143_Amort) =
     +                           FASB143_MONTHLY_ARO_INTEREST_ACCREATION
         IF(ADD_2_INCOME_STATEMENT == 'T') THEN
            INCOME_VARIABLES(:,
     +                   mthy_incm_rsv_mgn_cap_sales) =
     +          INCOME_VARIABLES(:,
     +                     mthy_incm_rsv_mgn_cap_sales)
     +          + MONTHLY_CAPACITY_SALES_TO_LEVEL_RM 
            INCOME_VARIABLES(:,
     +               mty_xpns_rsv_mgn_cap_pchs) =
     +            INCOME_VARIABLES(:,
     +                 mty_xpns_rsv_mgn_cap_pchs)
     +            + MONTHLY_CAPACITY_PURCHASES_TO_LEVEL_RM
            CASH_VARIABLES(:,
     +                  csh_expns_rsv_mgn_cap_purch) =
     +            CASH_VARIABLES(:,
     +                    csh_expns_rsv_mgn_cap_purch) 
     +            + MONTHLY_CAPACITY_PURCHASES_TO_LEVEL_RM
            CASH_VARIABLES(:,
     +                      csh_income_rsv_mgn_cap_sales) =
     +            CASH_VARIABLES(:,
     +                        csh_income_rsv_mgn_cap_sales)
     +            + MONTHLY_CAPACITY_SALES_TO_LEVEL_RM 
         ENDIF
c
         WVPA_OFF_SYSTEM_SALES_RECEIVABLE = 0.
         WVPA_OFF_SYSTEM_SALES_PAYABLE = 0.
         WVPA_INTERNAL_POWER_COSTS_PAYABLE = 0. 
         IF(WVPA() .AND. CLASS_TYPE == PARENT) THEN
            VOID_LOGICAL = WVPA_MNTHLY_MEMBER_ACCRUED_REVS(
     +                         INCOME_VARIABLES(0,
     +                            mty_wvpa_acrd_mbr_rvnues))
            CALL WVPA_MONTHLY_NON_MEMBER_SALES(INCOME_VARIABLES,
     +                                         CASH_VARIABLES)
            CALL WVPA_OFF_SYSTEM_SALES_ACCRUALS(R_YR,
     +                                WVPA_OFF_SYSTEM_SALES_RECEIVABLE,
     +                                WVPA_OFF_SYSTEM_SALES_PAYABLE,
     +                                WVPA_INTERNAL_POWER_COSTS_PAYABLE)
         ENDIF
         DO VARIABLE = 1, INCOME_VARS
C            IF(VARIABLE == MonthlyEmissionCredits) CYCLE
            IF(VARIABLE == MonthlyEmissionCredits
     +                                         .AND. .NOT. WVPA()) CYCLE
            IF(VARIABLE == Monthly_LTD_Amort_Interest) CYCLE 
            IF(VARIABLE == monthly_oth_taxes) CYCLE
            IF(VARIABLE == Monthly_Operating_Revenue_Tax) CYCLE
            IF(VARIABLE == mthy_nf_incm_tax_dfrls_cr) CYCLE
C
            IF(VARIABLE == Monthly_Property_Taxes) CYCLE
            IF(VARIABLE==MonthlyExpFilePropertyTaxes) CYCLE
            IF(VARIABLE==MonthlyExpFileOtherTaxes) CYCLE
            IF(VARIABLE==mthy_exp_file_opng_revnue_tax) CYCLE
            IF(VARIABLE == mth_modl_calcd_prop_taxes) THEN

            ELSEIF(VARIABLE == Monthly_CIAC_Amort) THEN
               INCOME_VARIABLES(:,VARIABLE) =
     +                     CIAC_MONTHLY_AMORTIZATION(:)
     +                     + INCOME_LINKED_LEVEL(:,VARIABLE,CLASS_LEVEL)
            ELSE
               INCOME_VARIABLES(:,VARIABLE) =
     +                     INCOME_VARIABLES(:,VARIABLE)
     +                     + INCOME_LINKED_LEVEL(:,VARIABLE,CLASS_LEVEL)
            ENDIF
C
         ENDDO
C
C GET EXPENSE FILE PROPERTY TAXES
C 
         CALL MONTHLY_EXPENSE_FILE_TAXES(R_YR,CLASS_ID,
     +                                   INCOME_VARIABLES,
     +                                   CASH_VARIABLES)
C
C CASH MOVE UPS
C
         DO VARIABLE = 1, CASH_VARS 
c            IF(VARIABLE == CashEmissionCredits) CYCLE
            IF(VARIABLE == CashEmissionCredits
     +                                         .AND. .NOT. WVPA()) CYCLE
            IF(VARIABLE == common_shares_issued) CYCLE
            CASH_VARIABLES(:,VARIABLE) =
     +                       CASH_VARIABLES(:,VARIABLE)
     +                       + CASH_LINKED_LEVEL(:,VARIABLE,CLASS_LEVEL)
         ENDDO
C
         IF(ODEC()) THEN
            IF(CLASS_TYPE == SUBSIDIARY .AND. 
     +                                 CLASS_ID - 1 == ODEC_NA3_ID) THEN
               ODEC_NA3_INTRA_BULK_POWER(0) = ANNUAL_VARS(253)! 15
               ODEC_NA3_INTRA_BULK_POWER(1:12) = ANNUAL_VARS(253)/12.!15
               CASH_VARIABLES(:,CashBulkPower) =
     +                                 CASH_VARIABLES(:,CashBulkPower)
     +                                 + ODEC_NA3_INTRA_BULK_POWER(:)
               INCOME_VARIABLES(:,BulkPower) = 
     +                                    INCOME_VARIABLES(:,BulkPower)
     +                                    + ODEC_NA3_INTRA_BULK_POWER(:)
               ODEC_NA3_ACTIVE = .TRUE.
            ELSEIF(CLASS_TYPE == PARENT .AND. ODEC_NA3_ACTIVE) THEN
               CASH_VARIABLES(:,Cash_Purchased_Power) =
     +                            CASH_VARIABLES(:,Cash_Purchased_Power)
     +                            + ODEC_NA3_INTRA_BULK_POWER(:)
               INCOME_VARIABLES(:,monthly_purchased_power) = 
     +                       INCOME_VARIABLES(:,monthly_purchased_power)
     +                        + ODEC_NA3_INTRA_BULK_POWER(:)
            ENDIF
         ENDIF
C
         DO MO = 0, 12
            CASH_VARIABLES(MO,Cash_TOTAL_OPERATING_RECEIPTS) =
     +                              SUM_CASH_RECEIPTS(MO,CASH_VARIABLES)
            CASH_VARIABLES(MO,Cash_TOTAL_EXPENSE_PAYMENTS) =
     +                              SUM_CASH_PAYMENTS(MO,CASH_VARIABLES)
         ENDDO
C
C STD AND CUSTOMER DEPOSIT INTEREST EXPENSE
         INCOME_VARIABLES(0,MonthlySTDInterest) = 0. ! ANNUAL_VARS(40)
C
C INTEREST ON CUSTOMER DEPOSITS
C
         CASH_VARIABLES(0,cash_customer_deposits) = 0.
         BALANCE_SHEET_VARIABLES(0,monthly_customer_deposits) =
     +                                        CUSTOMER_DEPOSITS_BAL(BOY)
         INCOME_VARIABLES(0,Monthly_Amortization) =
     +           + INCOME_VARIABLES(0,Monthly_Goodwill_Amort)
     +           + INCOME_VARIABLES(0,Monthly_Regulatory_Assets_Amort)
     +           + INCOME_VARIABLES(0,Monthly_FASB_109_Amort)
     +           + INCOME_VARIABLES(0,Monthly_FASB_133_Amort)
     +           + INCOME_VARIABLES(0,Monthly_FASB_143_Amort)
     +           + INCOME_VARIABLES(0,mthy_common_stock_issue_amort)
     +           + INCOME_VARIABLES(0,monthly_other_atl_amort)
     +           - INCOME_VARIABLES(0,Monthly_ATL_Gain_Amort)
     +           - INCOME_VARIABLES(0,Monthly_CIAC_Amort)




         INCOME_VARIABLES(0,BTL_Monthly_Amortization) =
     +               INCOME_VARIABLES(0,Monthly_Lease_BTL_Amort_Expense)
     +               + INCOME_VARIABLES(0,monthly_other_btl_amort)
     +               - INCOME_VARIABLES(0,Monthly_BTL_Gain_Amort)
         DO MO = 1, 12
C
C CALCULATE monthly_customer_deposits
C
            CUSTOMER_DEPOSITS_INTEREST =
     +       CUSTOMER_DEPOSIT_INTEREST_RATE(MO) *
     +         (CASH_VARIABLES(MO,cash_customer_deposits)/2. +
     +          BALANCE_SHEET_VARIABLES(MO-1,monthly_customer_deposits))
            BALANCE_SHEET_VARIABLES(MO,monthly_customer_deposits) =
     +           BALANCE_SHEET_VARIABLES(MO-1,monthly_customer_deposits)
     +                       + CASH_VARIABLES(MO,cash_customer_deposits)
            INCOME_VARIABLES(MO,Monthly_Amortization) =
     +           + INCOME_VARIABLES(MO,Monthly_Goodwill_Amort)
     +           + INCOME_VARIABLES(MO,Monthly_Regulatory_Assets_Amort)
     +           + INCOME_VARIABLES(MO,Monthly_FASB_109_Amort)
     +           + INCOME_VARIABLES(MO,Monthly_FASB_133_Amort)
     +           + INCOME_VARIABLES(MO,Monthly_FASB_143_Amort)
     +           + INCOME_VARIABLES(MO,mthy_common_stock_issue_amort)
     +           + INCOME_VARIABLES(MO,monthly_other_atl_amort)
     +           - INCOME_VARIABLES(MO,Monthly_ATL_Gain_Amort)
     +           - INCOME_VARIABLES(MO,Monthly_CIAC_Amort)
            INCOME_VARIABLES(MO,BTL_Monthly_Amortization) =
     +            INCOME_VARIABLES(MO,monthly_other_btl_amort)
     +            - INCOME_VARIABLES(MO,Monthly_BTL_Gain_Amort)
     +            + INCOME_VARIABLES(MO,Monthly_Lease_BTL_Amort_Expense)
C
C STD INTEREST
C            
            INCOME_VARIABLES(MO,MonthlySTDInterest) =
     +       (BALANCE_SHEET_VARIABLES(MO-1,monthly_short_term_debt_bal)
     +        + BALANCE_SHEET_VARIABLES(MO,monthly_short_term_debt_bal))
     +                                * STD_MONTHLY_INTEREST_RATE(MO)/2.
             IF(MO == 1 .AND. STI_CASH_LAG /= 0) THEN
               CASH_VARIABLES(MO,cash_std_interest) =
     +                     PAYABLE_MONTHLY_VALUES(MO,
     +                                   payment_std_interest,CLASS_POS)
            ELSE
               CASH_VARIABLES(MO,cash_std_interest) =
     +                     INCOME_VARIABLES(MO-STI_CASH_LAG,
     +                                             MonthlySTDInterest)
     +                     + PAYABLE_MONTHLY_VALUES(MO,
     +                                   payment_std_interest,CLASS_POS)
            ENDIF
            PAYABLE_MONTHLY_VALUES(MO,payment_std_interest,CLASS_POS)=0.
            INCOME_VARIABLES(MO,MonthlySTDInterest) =
     +                         INCOME_VARIABLES(MO,MonthlySTDInterest)
     +                         + CUSTOMER_DEPOSITS_INTEREST
            IF(MO == 12) THEN
               PAYABLE_MONTHLY_VALUES(1,payment_std_interest,CLASS_POS)=
     +                         INCOME_VARIABLES(MO,MonthlySTDInterest)
               PAYABLE_MONTHLY_VALUES(0,payment_std_interest,CLASS_POS)=
     +                         INCOME_VARIABLES(MO,MonthlySTDInterest)
            ENDIF
         ENDDO
         CASH_VARIABLES(0,cash_customer_deposits) =
     +                    SUM(CASH_VARIABLES(1:,cash_customer_deposits))
C
C ADD STD INTEREST EXPENSE THAT HAS BEEN LAGGED BY THE EXPENSE FILE
C
         INCOME_VARIABLES(1:,MonthlySTDInterest) =
     +                         INCOME_VARIABLES(1:,MonthlySTDInterest)
                               ! Interest from Expense File
     +                         + STD_INTEREST_ADJ(1:) 
         CASH_VARIABLES(1:,cash_std_interest) =
     +                              CASH_VARIABLES(1:,cash_std_interest)
     +                              + STD_CASH_INTEREST_ADJ(1:) ! lagged cash interest from expense file
         CASH_VARIABLES(0,cash_std_interest) =
     +                         SUM(CASH_VARIABLES(1:,cash_std_interest))
         INCOME_VARIABLES(0,MonthlySTDInterest) =
     +                    SUM(INCOME_VARIABLES(1:,MonthlySTDInterest))
C
C ALLOCATE EXPENSE ITEMS
C
         UNALLOCATED_AMOUNT = 0.
         UNALLOCATED_AMOUNT(Monthly_Book_Depreciation) =
     +          ALLOCATE_ACCOUNT_BALANCE(ANNUAL_VARS(17),
     +                    INCOME_VARIABLES(0,Monthly_Book_Depreciation))
         UNALLOCATED_AMOUNT(Monthly_Amortization) =
     +          ALLOCATE_ACCOUNT_BALANCE(ANNUAL_VARS(291),
     +                         INCOME_VARIABLES(0,Monthly_Amortization))
         UNALLOCATED_AMOUNT(BTL_Monthly_Expenses) =
     +          ALLOCATE_ACCOUNT_BALANCE(ANNUAL_VARS(35),
     +                     INCOME_VARIABLES(0,BTL_Monthly_Expenses))
C
C AFUDC ADJUSTMENTS-STILL NEEDED FOR NUCLEAR
C
         UNALLOCATED_AMOUNT(Monthly_AFUDC_Equity) =
     +          ALLOCATE_ACCOUNT_BALANCE(ANNUAL_VARS(37),
     +                     INCOME_VARIABLES(0,Monthly_AFUDC_Equity))
         UNALLOCATED_AMOUNT(Monthly_AFUDC_Borrowed) =
     +          ALLOCATE_ACCOUNT_BALANCE(ANNUAL_VARS(41),
     +                     INCOME_VARIABLES(0,Monthly_AFUDC_Borrowed))

C
C ADD ALLOCATED AMOUNTS TO TOTAL EXPENSE B4 TAXES
C
         VOID_LOGICAL = SUM_MONTHLY_INCOME_EXPENSES(INCOME_VARIABLES)

         INCOME_VARIABLES(:,BTL_Monthly_STInvestmet_Income) =
     +                                                   STI_EARNINGS(:)
         INCOME_VARIABLES(:,mty_mdl_lt_nvst_income) =
     +                                                   LTI_EARNINGS(:)
         INCOME_VARIABLES(:,BTL_Monthly_Interest_Income) =
     +              INCOME_VARIABLES(:,BTL_Monthly_STInvestmet_Income)
     +              + INCOME_VARIABLES(:,
     +                                mty_mdl_lt_nvst_income)
     +              + INCOME_VARIABLES(:,
     +                             mt_dbt_file_linvst_income)
     +              + INCOME_VARIABLES(:,
     +                                  monthly_notes_receivable_income)
         INCOME_VARIABLES(:,mty_totl_lti_income) =
     +              INCOME_VARIABLES(:,
     +                                mty_mdl_lt_nvst_income)
     +              + INCOME_VARIABLES(:,
     +                             mt_dbt_file_linvst_income)

         INCOME_VARIABLES(:,
     +       mthy_net_tax_nclr_dcm_fd_rngs) =
     +                       MONTHLY_NUC_DECOM_FUND_NET_OF_TAX_RETURN(:)
         INCOME_VARIABLES(:,mthy_rtrmt_med_fd_rngs) =
     +                                 RETIREMENT_MEDICAL_FUND_RETURN(:)
         INCOME_VARIABLES(:,Monthly_Investment_Earnings) =
     +            RETIREMENT_MEDICAL_FUND_RETURN(:)
     +            + INCOME_VARIABLES(:,Monthly_Dividend_70_Earnings)
     +            + INCOME_VARIABLES(:,mty_rglr_dvd_income)
         IF(SALT_RIVER_PROJECT()) THEN
            INCOME_VARIABLES(:,Monthly_Investment_Earnings) =
     +                   INCOME_VARIABLES(:,Monthly_Investment_Earnings)
     +                   + MONTHLY_NUC_DECOM_FUND_RETURN(:)
            INCOME_VARIABLES(:,
     +                  mthy_nclr_dcmssng_fd_rngs) =
     +                                  MONTHLY_NUC_DECOM_FUND_RETURN(:)

            INCOME_VARIABLES(:,mthly_invstmt_earngs_rcvbl) =
     +            + INCOME_VARIABLES(:,Monthly_Dividend_70_Earnings)
     +            + INCOME_VARIABLES(:,mty_rglr_dvd_income)
          ELSEIF(RETAIN_POST_RETIREMENT_EARNINGS()) THEN
             INCOME_VARIABLES(:,mthly_invstmt_earngs_rcvbl)=
     +             INCOME_VARIABLES(:,Monthly_Dividend_70_Earnings)
     +             + INCOME_VARIABLES(:,mty_rglr_dvd_income)
          ELSE
             INCOME_VARIABLES(:,mthly_invstmt_earngs_rcvbl)=
     +            RETIREMENT_MEDICAL_FUND_RETURN(:)
     +            + INCOME_VARIABLES(:,Monthly_Dividend_70_Earnings)
     +            + INCOME_VARIABLES(:,mty_rglr_dvd_income)
         ENDIF

         CASH_VARIABLES(:,cash_invest_interest_ernings) =
     +               CASH_VARIABLES(:,cash_invest_interest_ernings)
     +               + LTI_EARNINGS(:)! ADDED 5/13/04
C
C TAL TAXES ITES 81-95; TOTAL EXPENSE 150 OP INCOME 151 INCOME_VARIABLES(0:12,mthy_fed_atl_income_tax_pd)
C
         UNALLOCATED_AMOUNT(mth_modl_calcd_prop_taxes) =
     +          ALLOCATE_ACCOUNT_BALANCE(ANNUAL_VARS(435), ! NOW PROPERTY TAXES BASES ON VALUE217),
     +                       INCOME_VARIABLES(0,
     +                         mth_modl_calcd_prop_taxes))
         INCOME_VARIABLES(:,mth_modl_calcd_prop_taxes) =
     +       INCOME_VARIABLES(:,mth_modl_calcd_prop_taxes)
     +       + PROPERTY_TAX_ADJUSTMENT(:)
C
         INCOME_VARIABLES(0,Monthly_ITC_Amortization) = ANNUAL_VARS(50)
         INCOME_VARIABLES(0,mthy_state_atl_incm_tax_pd) =
     +                                                   ANNUAL_VARS(24)
         INCOME_VARIABLES(0,Monthly_State_Tax_on_Capital) =
     +                                                   ANNUAL_VARS(25)
         INCOME_VARIABLES(0,mthy_fed_atl_income_tax_pd) =
     +                                                   ANNUAL_VARS(26)
         INCOME_VARIABLES(0,Monthly_Federal_Tax_on_Capital) =
     +                                                   ANNUAL_VARS(27)
         INCOME_VARIABLES(0,Monthly_Income_Tax_Deferrals_Dr) =
     +                                                   ANNUAL_VARS(28)
C
         INCOME_VARIABLES(0,BTL_Monthly_Income_Taxes) = ANNUAL_VARS(36)
         INCOME_VARIABLES(0,BTL_Monthly_Income_Tax_Deferrals) =
     +                                                  ANNUAL_VARS(294)
         INCOME_VARIABLES(0,BTLMonthlyDeferralTaxCr) = ANNUAL_VARS(902)
         INCOME_VARIABLES(0,BTLMonthlyDeferralTaxDr) = ANNUAL_VARS(903)
         INCOME_VARIABLES(0,BTL_Monthly_Amortization) =
     +                                                  ANNUAL_VARS(350)
         INCOME_VARIABLES(0,Monthly_Subsidiary_Income) =
     +                                                   ANNUAL_VARS(43)
         INCOME_VARIABLES(0,Monthly_Extraordinary_Items) =
     +                                                   ANNUAL_VARS(44)
C
C DEFERRED TAXES CR
C
         INCOME_VARIABLES(0,mthy_ltd_ps_incm_tax_dfrls_cr) =
     +                                                  ANNUAL_VARS(681)
         INCOME_VARIABLES(1:,mthy_ltd_ps_incm_tax_dfrls_cr) = 
     +            INCOME_VARIABLES(0,
     +                       mthy_ltd_ps_incm_tax_dfrls_cr)/12.
         INCOME_VARIABLES(0,mth_othr_incm_tax_dfrls_cr) =
     +                                                  ANNUAL_VARS(682)
         INCOME_VARIABLES(1:,mth_othr_incm_tax_dfrls_cr) =
     +            INCOME_VARIABLES(0,
     +                        mth_othr_incm_tax_dfrls_cr)/12.
         INCOME_VARIABLES(0,mthy_nf_incm_tax_dfrls_cr) =
     +       RETURN_NF_DEFERRED_TAXES(INCOME_VARIABLES(0,
     +                              mthy_nf_incm_tax_dfrls_cr))
         INCOME_VARIABLES(:,mthy_nf_incm_tax_dfrls_cr) =
     +            INCOME_VARIABLES(:,mthy_nf_incm_tax_dfrls_cr) 
     +            + INCOME_LINKED_LEVEL(:,
     +                               mthy_nf_incm_tax_dfrls_cr,
     +                                                      CLASS_LEVEL)
         INCOME_VARIABLES(:,Monthly_Income_Tax_Deferrals_Cr) =
     +      INCOME_VARIABLES(:,mth_othr_incm_tax_dfrls_cr)
     +      + INCOME_VARIABLES(:,mthy_ltd_ps_incm_tax_dfrls_cr)
     +      + INCOME_VARIABLES(:,mthy_nf_incm_tax_dfrls_cr)
         INCOME_VARIABLES(0,wvpa_non_mbr_svcs_cost) =
     +        SUM(INCOME_VARIABLES(1:,wvpa_non_mbr_svcs_cost))
         INCOME_VARIABLES(0,WVPA_Member_Cost_of_Services) =
     +            SUM(INCOME_VARIABLES(1:,WVPA_Member_Cost_of_Services))
C
C COMMON DIVIDENDS BY QUARTER
C
         QUARTER = 1
         SKIP_MONTH = 0
         CASH_VARIABLES(0,cash_common_dividends) = 0.
         IF(SALT_RIVER_PROJECT()) THEN 
            INCOME_VARIABLES(:,Monthly Common Dividends) =
     +                                       MONTHLY_COMMON_DIVIDENDS(:)
            CASH_VARIABLES(:,cash_common_dividends) =
     +                                       MONTHLY_COMMON_DIVIDENDS(:)
         ELSEIF(DIVIDEND_PAYMENT_METHOD == DPS) THEN
            INCOME_VARIABLES(0,Monthly Common Dividends) =
     +                                       MONTHLY_COMMON_DIVIDENDS(0)
            DO MO = 1, 12
               INCOME_VARIABLES(MO,Monthly Common Dividends) =
     +                                      MONTHLY_COMMON_DIVIDENDS(MO)
               IF(MPS()) THEN
                  IF(CLASS_TYPE == PARENT) THEN
                     IF(CS_PAY_MONTH(MO)) THEN
                        CASH_VARIABLES(MO,cash_common_dividends) =
     +                                            TEMP_CS_DIV_CARRY_OVER
                        CASH_VARIABLES(0,cash_common_dividends) =
     +                           CASH_VARIABLES(0,cash_common_dividends)
     +                           + TEMP_CS_DIV_CARRY_OVER
                        TEMP_CS_DIV_CARRY_OVER = 0.
                     ENDIF
                     TEMP_CS_DIV_CARRY_OVER = TEMP_CS_DIV_CARRY_OVER
     +                   + INCOME_VARIABLES(MO,Monthly Common Dividends)
                  ELSE
                     CASH_VARIABLES(MO,cash_common_dividends) =
     +                     INCOME_VARIABLES(MO,Monthly Common Dividends)
                  ENDIF
               ELSE
                  IF(CLASS_TYPE == PARENT) THEN
                     IF(MO == CS_DIVIDEND_PAYMENT_LAG) THEN
                        CASH_VARIABLES(MO,cash_common_dividends) =
     +                                            TEMP_CS_DIV_CARRY_OVER
                        CASH_VARIABLES(0,cash_common_dividends) =
     +                        CASH_VARIABLES(0,cash_common_dividends)
     +                        + CASH_VARIABLES(MO,cash_common_dividends)
                     ELSEIF(MO /= SKIP_MONTH) THEN
                        CASH_VARIABLES(MO,cash_common_dividends) = 0.
                     ENDIF
                     IF(MO == CS_DECLARATION_MONTH(QUARTER)) THEN
                        IF(MO+CS_DIVIDEND_PAYMENT_LAG > 12) THEN
                           TEMP_CS_DIV_CARRY_OVER =
     +                     INCOME_VARIABLES(MO,Monthly Common Dividends)
                        ELSE
                           CASH_VARIABLES(MO+CS_DIVIDEND_PAYMENT_LAG,
     +                                          cash_common_dividends) =
     +                     INCOME_VARIABLES(MO,Monthly Common Dividends)
                           CASH_VARIABLES(0,cash_common_dividends) =
     +                         CASH_VARIABLES(0,cash_common_dividends) +
     +                     INCOME_VARIABLES(MO,Monthly Common Dividends)
                        ENDIF
                        QUARTER = MIN(4,QUARTER + 1)
                        SKIP_MONTH = MO + CS_DIVIDEND_PAYMENT_LAG
                     ENDIF
                  ELSE
                     CASH_VARIABLES(MO,cash_common_dividends) =
     +                     INCOME_VARIABLES(MO,Monthly Common Dividends)
                  ENDIF
               ENDIF
            ENDDO
         ELSE ! DIVIDEND DPR
            INCOME_VARIABLES(0,Monthly Common Dividends) = 
     +                                       MONTHLY_COMMON_DIVIDENDS(0)
            IF(CLASS_TYPE == PARENT) THEN
               IF(DIVIDEND_PAYMENT_METHOD == DPR) THEN
                  QUARTERY_CS_DIVIDENDS(1) = ANNUAL_VARS(48)/4.
                  QUARTERY_CS_DIVIDENDS(2) = QUARTERY_CS_DIVIDENDS(1)
                  QUARTERY_CS_DIVIDENDS(3) = QUARTERY_CS_DIVIDENDS(2)
                  QUARTERY_CS_DIVIDENDS(4) = QUARTERY_CS_DIVIDENDS(3)
               ELSE
                  QUARTERY_CS_DIVIDENDS(3) = ANNUAL_VARS(56) *
     +                                               ANNUAL_VARS(299)/4.
                  QUARTERY_CS_DIVIDENDS(4) = QUARTERY_CS_DIVIDENDS(3)
                  QUARTERY_CS_DIVIDENDS(1) = (ANNUAL_VARS(48) -
     +                                  (QUARTERY_CS_DIVIDENDS(3)
     +                                   + QUARTERY_CS_DIVIDENDS(4)))/2.
                  QUARTERY_CS_DIVIDENDS(2) = QUARTERY_CS_DIVIDENDS(1)
               ENDIF
            ELSE
               DIVIDEND_ALLOCATION = 2*ANNUAL_VARS(77)-ANNUAL_VARS(101) 
               IF(ANNUAL_VARS(101) == 0. .OR.
     +                                     DIVIDEND_ALLOCATION==0.) THEN ! NO COMMON STOCK ISSUED
                  QUARTERY_CS_DIVIDENDS(1) = ANNUAL_VARS(48)/4.
                  QUARTERY_CS_DIVIDENDS(2) = QUARTERY_CS_DIVIDENDS(1)
                  QUARTERY_CS_DIVIDENDS(3) = QUARTERY_CS_DIVIDENDS(2)
                  QUARTERY_CS_DIVIDENDS(4) = QUARTERY_CS_DIVIDENDS(3)
               ELSE
                  QUARTERY_CS_DIVIDENDS(3) = ANNUAL_VARS(77) * 
     +                          ANNUAL_VARS(48)/(2.*DIVIDEND_ALLOCATION)
                  QUARTERY_CS_DIVIDENDS(4) = QUARTERY_CS_DIVIDENDS(3)
                  QUARTERY_CS_DIVIDENDS(1) = (ANNUAL_VARS(48) -
     +                                  (QUARTERY_CS_DIVIDENDS(3)
     +                                   + QUARTERY_CS_DIVIDENDS(4)))/2.
                  QUARTERY_CS_DIVIDENDS(2) = QUARTERY_CS_DIVIDENDS(1)
               ENDIF
            ENDIF
            DO MO = 1, 12
               IF(CLASS_TYPE == PARENT) THEN
                  INCOME_VARIABLES(MO,Monthly Common Dividends) = 0.
                  IF(MO == CS_DIVIDEND_PAYMENT_LAG) THEN
                     CASH_VARIABLES(MO,cash_common_dividends) =
     +                                            TEMP_CS_DIV_CARRY_OVER
                     CASH_VARIABLES(0,cash_common_dividends) =
     +                           CASH_VARIABLES(0,cash_common_dividends)
     +                           + TEMP_CS_DIV_CARRY_OVER
                  ELSEIF(MO /= SKIP_MONTH) THEN
                     CASH_VARIABLES(MO,cash_common_dividends) = 0.
                  ENDIF
                  IF(MO == CS_DECLARATION_MONTH(QUARTER)) THEN
                     INCOME_VARIABLES(MO,Monthly Common Dividends) =
     +                                    QUARTERY_CS_DIVIDENDS(QUARTER) ! INCOME_VARIABLES(0,Monthly Common Dividends)/4.
                     IF(MO+CS_DIVIDEND_PAYMENT_LAG > 12) THEN
                        TEMP_CS_DIV_CARRY_OVER =
     +                                    QUARTERY_CS_DIVIDENDS(QUARTER) ! INCOME_VARIABLES(0,Monthly Common Dividends)/4.
                     ELSE
                        CASH_VARIABLES(MO+CS_DIVIDEND_PAYMENT_LAG,
     +                                          cash_common_dividends) =
     +                                    QUARTERY_CS_DIVIDENDS(QUARTER) ! INCOME_VARIABLES(0,Monthly Common Dividends)/4.
                        CASH_VARIABLES(0,cash_common_dividends) =
     +                         CASH_VARIABLES(0,cash_common_dividends) +
     +                                    QUARTERY_CS_DIVIDENDS(QUARTER) ! INCOME_VARIABLES(0,Monthly Common Dividends)/4.
                     ENDIF
                     SKIP_MONTH = MO + CS_DIVIDEND_PAYMENT_LAG
                     QUARTER = MIN(4,QUARTER + 1)
                  ENDIF
               ELSE ! NOT PARENT
                  IF(MO==3 .OR. MO==6 .OR. MO==9 .OR. MO==12) THEN
                     INCOME_VARIABLES(MO,Monthly Common Dividends) =
     +                                    QUARTERY_CS_DIVIDENDS(QUARTER) ! INCOME_VARIABLES(0,Monthly Common Dividends)/4.
                     CASH_VARIABLES(MO,cash_common_dividends) =
     +                                    QUARTERY_CS_DIVIDENDS(QUARTER) ! INCOME_VARIABLES(0,Monthly Common Dividends)/4.
                     CASH_VARIABLES(0,cash_common_dividends) =
     +                   CASH_VARIABLES(0,cash_common_dividends) +
     +                                    QUARTERY_CS_DIVIDENDS(QUARTER) ! INCOME_VARIABLES(0,Monthly Common Dividends)/4.
                     QUARTER = MIN(4,QUARTER + 1)
                  ELSE
                     INCOME_VARIABLES(MO,Monthly Common Dividends) = 0.
                     CASH_VARIABLES(MO,cash_common_dividends) = 0.
                  ENDIF
               ENDIF
            ENDDO
         ENDIF ! DIVIDEND PAYMENT METHOD
C
         CASH_VARIABLES(0,cash_common_dividends) =
     +                     SUM(CASH_VARIABLES(1:,cash_common_dividends))
         INCOME_VARIABLES(0,Monthly Common Dividends) =
     +                SUM(INCOME_VARIABLES(1:,Monthly Common Dividends))
         COMMON_STOCK_CASH_DIVIDENDS =
     +                           CASH_VARIABLES(0,cash_common_dividends)
c
         INCOME_VARIABLES(0,Monthly_Total_Taxes_Expense) =
     +         MONTHLY_TOTAL_EXPENSES_B4_TAXES(INT2(0),INCOME_VARIABLES)
     +         + TOTAL_TAXES_EXPENSE(INT2(0),INCOME_VARIABLES) 
C
C  CALCULATE ALLOCATION FACTOR FOR OP METHOD ADJUSTMENTS
C
         IF(ODEC() .AND. CLASS_TYPE == SUBSIDIARY .AND.
     +                                 CLASS_ID - 1 == ODEC_NA3_ID) THEN
            INCOME_VARIABLES(0,Operating Method Adjustment) = 0.
         ELSE
            INCOME_VARIABLES(0,Operating Method Adjustment) =
     +                                                  ANNUAL_VARS(253)
         ENDIF

         INCOME_VARIABLES(:,Prior Years Method Adjustment) =
     +                                     MONTHLY_PREVIOUS_OPT_REVS(:)

         INCOME_VARIABLES(0,TotalDerivativeRevenues) =
     +               SUM(INCOME_VARIABLES(1:,TotalDerivativeRevenues))
         INCOME_VARIABLES(0,TotalDerivativeExpenses) =
     +               SUM(INCOME_VARIABLES(1:,TotalDerivativeExpenses))
         INCOME_VARIABLES(0,TotalFuelDerivativeRevenues) =
     +          SUM(INCOME_VARIABLES(1:,TotalFuelDerivativeRevenues))
         INCOME_VARIABLES(0,TotalFuelDerivativeExpenses) =
     +          SUM(INCOME_VARIABLES(1:,TotalFuelDerivativeExpenses))

         INCOME_VARIABLES(0,Total Base Revenues) =
     +                  SUM_REGULATED_REVENUES(INT2(0),INCOME_VARIABLES)
         INCOME_VARIABLES(0,TOTAL OPERATING REVENUES) =
     +             INCOME_VARIABLES(0,Total Base Revenues)
     +             + SUM_NONREGULATED_REVENUES(INT2(0),INCOME_VARIABLES)
C
C ESTABLISH REVENUE TAX ALLOCATION
C
         VOID_REAL = CALC_LEVEL_REVENUE_BASED_TAX(INT2(0),
     +                                         INCOME_VARIABLES,
     +                                         MONTHLY_REVENUE_TAX_RATE,
     +                                         REVENUE_TAX_ADJUSTMENT)
C
C  CALCULATE OP INCOME
C
         INCOME_VARIABLES(0,Monthly_Op_Income) = 
     +          INCOME_VARIABLES(0,monthly_total_revenues) -
     +          INCOME_VARIABLES(0,Monthly_Total_Taxes_Expense)
         MO = 0
         INCOME_VARIABLES(MO,Monthly_INCOME_BEFORE_INTEREST) = 
     +                       INCOME_BEFORE_INTEREST(MO,INCOME_VARIABLES)
C
C ADJUST LTD INTEREST FOR AMORTIZATION
C
         IF(CPL_ACTIVE() .OR. MORTGAGE_DEBT) THEN
            TAX_VARIABLES(0,monthly_ltd_tax_deduction) =
     +                    INCOME_VARIABLES(0,Monthly_LTD_Total_Interest)
     +                    + LTD_BOOKED_ISSUED_INTEREST(0)
            INCOME_VARIABLES(0,Monthly_LTD_Booked_Interest) =
     +                    INCOME_VARIABLES(0,Monthly_LTD_Total_Interest)
     +                    + LTD_BOOKED_ISSUED_INTEREST(0)
            INCOME_VARIABLES(0,Monthly_LTD_Total_Interest) =
     +                 INCOME_VARIABLES(0,Monthly_LTD_Total_Interest)
     +                 + LTD_BOOKED_ISSUED_INTEREST(0)
     +                 + ANNUAL_VARS(630) ! was 384 but only debit file is included. LTD INTEREST AMORTIZATION
            DO MO = 1, 12
               INCOME_VARIABLES(MO,Monthly_LTD_Total_Interest) =
     +                   INCOME_VARIABLES(MO,Monthly_LTD_Total_Interest)
     +                   + LTD_BOOKED_ISSUED_INTEREST(MO)
               INCOME_VARIABLES(MO,Monthly_LTD_Booked_Interest) =
     +                   INCOME_VARIABLES(MO,Monthly_LTD_Total_Interest)
               TAX_VARIABLES(MO,monthly_ltd_tax_deduction) =
     +                   INCOME_VARIABLES(MO,Monthly_LTD_Total_Interest)
            ENDDO
         ELSE
            IF(CLASS_TYPE == REGULATED_GROUP) THEN
               TAX_VARIABLES(0,monthly_ltd_tax_deduction) =
     +                                      LTD_ISSUE_YR_INTEREST_BOOKED
               INCOME_VARIABLES(0,Monthly_LTD_Booked_Interest) =
     +                                      LTD_ISSUE_YR_INTEREST_BOOKED
               INCOME_VARIABLES(0,Monthly_LTD_Total_Interest) =
     +                                      LTD_ISSUE_YR_INTEREST_BOOKED
     +                                      + ANNUAL_VARS(384) ! LTD INTEREST AMORTIZATION
               HALF_YEAR = LTD_ISSUE_YR_INTEREST_PAYMENT *
     +               BOY_LTD_BALANCE/(2.*BOY_LTD_BALANCE
     +                                 - LTD_RETIREMENTS
     +                                 + LONG_TERM_DEBT_ISSUED)

                  INCOME_VARIABLES(1:6,Monthly_LTD_Total_Interest) =
     +                                                      HALF_YEAR/6.

               HALF_YEAR = LTD_ISSUE_YR_INTEREST_PAYMENT - HALF_YEAR 

                  INCOME_VARIABLES(7:12,Monthly_LTD_Total_Interest) =
     +                                                      HALF_YEAR/6.

            ELSE
               TAX_VARIABLES(0,monthly_ltd_tax_deduction) =
     +                    INCOME_VARIABLES(0,Monthly_LTD_Total_Interest)
     +                    + LTD_ISSUE_YR_INTEREST_BOOKED
               INCOME_VARIABLES(0,Monthly_LTD_Booked_Interest) =
     +                    INCOME_VARIABLES(0,Monthly_LTD_Total_Interest)
     +                    + LTD_ISSUE_YR_INTEREST_BOOKED
               INCOME_VARIABLES(0,Monthly_LTD_Total_Interest) =
     +                 INCOME_VARIABLES(0,Monthly_LTD_Total_Interest)
     +                 + LTD_ISSUE_YR_INTEREST_BOOKED
     +                 + ANNUAL_VARS(384) ! LTD INTEREST AMORTIZATION
              if(.false.) then
               INCOME_VARIABLES(9,Monthly_LTD_Total_Interest) =
     +                 INCOME_VARIABLES(9,Monthly_LTD_Total_Interest)
     +                 + LTD_ISSUE_YR_INTEREST_BOOKED/2.
               INCOME_VARIABLES(12,Monthly_LTD_Total_Interest) =
     +                 INCOME_VARIABLES(12,Monthly_LTD_Total_Interest)
     +                 + LTD_ISSUE_YR_INTEREST_BOOKED/2.
              else
               INCOME_VARIABLES(7:12,Monthly_LTD_Total_Interest) =
     +                 INCOME_VARIABLES(7:12,Monthly_LTD_Total_Interest)
     +                 + LTD_ISSUE_YR_INTEREST_BOOKED/6.
              endif
            ENDIF

               TAX_VARIABLES(1:,monthly_ltd_tax_deduction) =
     +                   INCOME_VARIABLES(1:,Monthly_LTD_Total_Interest)
               INCOME_VARIABLES(1:,Monthly_LTD_Booked_Interest) =
     +                   INCOME_VARIABLES(1:,Monthly_LTD_Total_Interest)

         ENDIF
         IF(CLASS_TYPE == REGULATED_GROUP) THEN
            INCOME_VARIABLES(0,Monthly_Booked_PS_Dividends) =
     +                                       PS_ISSUE_YR_DIVIDEND_BOOKED
            INCOME_VARIABLES(0,Monthly_Total_PS_Dividends) =
     +                                       PS_ISSUE_YR_DIVIDEND_BOOKED
     +                                       + ANNUAL_VARS(360) ! PS AMORTIZATIONS
            INCOME_VARIABLES(9,Monthly_Total_PS_Dividends) =
     +                                    PS_ISSUE_YR_DIVIDEND_BOOKED/2.
            INCOME_VARIABLES(12,Monthly_Total_PS_Dividends) =
     +                                    PS_ISSUE_YR_DIVIDEND_BOOKED/2.
         ELSE
            INCOME_VARIABLES(0,Monthly_Booked_PS_Dividends) =
     +                    INCOME_VARIABLES(0,Monthly_Total_PS_Dividends)
     +                    + PS_ISSUE_YR_DIVIDEND_BOOKED
            INCOME_VARIABLES(0,Monthly_Total_PS_Dividends) =
     +                    INCOME_VARIABLES(0,Monthly_Total_PS_Dividends)
     +                    + PS_ISSUE_YR_DIVIDEND_BOOKED
     +                    + ANNUAL_VARS(360) ! PS AMORTIZATIONS
            INCOME_VARIABLES(9,Monthly_Total_PS_Dividends) =
     +                    INCOME_VARIABLES(9,Monthly_Total_PS_Dividends)
     +                    + PS_ISSUE_YR_DIVIDEND_BOOKED/2.
            INCOME_VARIABLES(12,Monthly_Total_PS_Dividends) =
     +                   INCOME_VARIABLES(12,Monthly_Total_PS_Dividends)
     +                   + PS_ISSUE_YR_DIVIDEND_BOOKED/2.
         ENDIF
C
         INCOME_VARIABLES(0,Monthly INCOME AFTER INTEREST) = 
     +           MONTHLY_INCOME_AFTER_INTEREST(INT2(0),INCOME_VARIABLES)
         INCOME_VARIABLES(0,Monthly Net Income) = 
     +                      MONTHLY_NET_INCOME(INT2(0),INCOME_VARIABLES)
         INCOME_VARIABLES(0,Monthly_Earnings_2_Common) = 
     +          INCOME_VARIABLES(0,Monthly Net Income) -
     +          INCOME_VARIABLES(0,Monthly_Total_PS_Dividends)
         INCOME_VARIABLES(0,Monthly RETAINED EARNINGS) = 
     +          INCOME_VARIABLES(0,Monthly_Earnings_2_Common) -
     +          INCOME_VARIABLES(0,Monthly Common Dividends)


C
C SET SUMMING VARIABLE TO ZERO
C
C

          INCOME_VARIABLES(1:,mthy_state_atl_incm_tax_pd) =
     +         INCOME_VARIABLES(0,mthy_state_atl_incm_tax_pd)/12.
          INCOME_VARIABLES(1:,mthy_fed_atl_income_tax_pd) =
     +       INCOME_VARIABLES(0,mthy_fed_atl_income_tax_pd)/12.
          INCOME_VARIABLES(1:,Monthly_Income_Tax_Deferrals_Dr) =
     +           INCOME_VARIABLES(0,Monthly_Income_Tax_Deferrals_Dr)/12.

C
C FIRST ORDER STATE & FEDERAL INCOME TAXES 10/12/99
C
         STATE_BTL_INCOME_TAXES = 0.
         FEDERAL_BTL_INCOME_TAXES = 0.
         SKIP_MONTH = 0
         QUARTER = 1
         LEVELIZE_INCOME_TAXES = .FALSE.
         IF(.NOT. LEVELIZE_INCOME_TAXES .AND. R_ITER >= 1) THEN
C
C STATE TAXES BASED ON TAXABLE INCOME
C
            IF(R_ITER < 3) THEN
               UNALLOCATED_DEF_TAX_DR = 0.
            ELSE
               UNALLOCATED_DEF_TAX_DR =
     +               INCOME_VARIABLES(0,Monthly_Income_Tax_Deferrals_Dr)
     +               - STATE_DEF_TAX_DR_FROM_NOLS(0)
     +               - FED_DEF_TAX_DR_FROM_NOLS_AMT(0)
            ENDIF
            DO MO = 0, 12
               INCOME_VARIABLES(MO,mthy_state_atl_incm_tax_pd) =
     +                                      RPT_STATE_ATL_INCOME_TAX(MO)
               INCOME_VARIABLES(MO,mthy_fed_atl_income_tax_pd)=
     +                                    RPT_FEDERAL_ATL_INCOME_TAX(MO)
               INCOME_VARIABLES(MO,mty_tot_fed_incm_tax_pd) =
     +                                        RPT_FEDERAL_INCOME_TAX(MO)
               INCOME_VARIABLES(MO,mty_tot_st_incm_tax_pd)=
     +                                          RPT_STATE_INCOME_TAX(MO)
               INCOME_VARIABLES(MO,mt_fed_df_tax_dr_nols_amt) =
     +                                    STATE_DEF_TAX_DR_FROM_NOLS(MO)
               INCOME_VARIABLES(MO,mty_st_def_tax_dr_nols) =
     +                                  FED_DEF_TAX_DR_FROM_NOLS_AMT(MO)
               IF(MO == 0) THEN
                  INCOME_VARIABLES(MO,Monthly_Income_Tax_Deferrals_Dr) =
     +                                STATE_DEF_TAX_DR_FROM_NOLS(MO)
     +                                + FED_DEF_TAX_DR_FROM_NOLS_AMT(MO)
     +                                + UNALLOCATED_DEF_TAX_DR
                  UNALLOCATED_DEF_TAX_DR = UNALLOCATED_DEF_TAX_DR/12.
               ELSE
                  INCOME_VARIABLES(MO,Monthly_Income_Tax_Deferrals_Dr) =
     +                                STATE_DEF_TAX_DR_FROM_NOLS(MO)
     +                                + FED_DEF_TAX_DR_FROM_NOLS_AMT(MO)
     +                                + UNALLOCATED_DEF_TAX_DR
               ENDIF
            ENDDO
         ENDIF
C
C END INCOME TAX DISTRIBUTION
C
         CALL RETURN_ACTUAL_TAX_VALUES(R_YR,CLASS_ID,
     +          INCOME_VARIABLES(0,Monthly_Income_Tax_Deferrals_Cr),
     +          INCOME_VARIABLES(0,mty_tot_fed_incm_tax_pd),
     +          INCOME_VARIABLES(0,mty_tot_st_incm_tax_pd),
     +          INCOME_VARIABLES(0,Monthly_Income_Tax_Deferrals_Dr))
         DO MO = 0, 12
            INCOME_VARIABLES(MO,mthy_fed_atl_income_tax_pd)=
     +            INCOME_VARIABLES(MO,mty_tot_fed_incm_tax_pd)
     +            - RPT_FEDERAL_BTL_INCOME_TAX(MO)
            INCOME_VARIABLES(MO,mthy_state_atl_incm_tax_pd) =
     +          INCOME_VARIABLES(MO,mty_tot_st_incm_tax_pd)
     +          - RPT_STATE_BTL_INCOME_TAX(MO)
         ENDDO
     
C
         INCOME_VARIABLES(0,monthly_oth_taxes) = 0.
         DO MO = 1, 12
            INCOME_VARIABLES(MO,TOTAL OPERATING REVENUES) =
     +                  INCOME_VARIABLES(MO,Total Base Revenues)
     +                  + SUM_NONREGULATED_REVENUES(MO,INCOME_VARIABLES)
C
C DIVIDE BY 12 VALUES
C

            INCOME_VARIABLES(MO,Monthly_ITC_Amortization) =
     +                  INCOME_VARIABLES(0,Monthly_ITC_Amortization)/12.
            INCOME_VARIABLES(MO,Monthly_State_Tax_on_Capital) =
     +              INCOME_VARIABLES(0,Monthly_State_Tax_on_Capital)/12.
            INCOME_VARIABLES(MO,Monthly_Federal_Tax_on_Capital) =
     +            INCOME_VARIABLES(0,Monthly_Federal_Tax_on_Capital)/12.
C
C ALLOCATE OP METHOD ADJUSTMENT
C

C SET START VALUES FOR LOOP BELOW
C
            INCOME_VARIABLES(MO,Operating Method Adjustment) = 
     +               INCOME_VARIABLES(0,Operating Method Adjustment)/12.
         ENDDO 
C
         CONVERGENCE_TEST = 0.
         ITER = 1
C
C
C ALLOCATE OP METH ADJUSTMENTS 
C
         USE_OP_METH_ALLOCATOR = .TRUE.
         IF(INCOME_VARIABLES(0,Operating Method Adjustment) /= 0.) THEN
            IF(INCOME_VARIABLES(0,Monthly_Total_Taxes_Expense)/=0.) THEN
               OP_METH_ALLOCATOR =
     +                  INCOME_VARIABLES(0,Operating Method Adjustment)/
     +                   INCOME_VARIABLES(0,Monthly_Total_Taxes_Expense)
            ELSE
               USE_OP_METH_ALLOCATOR = .FALSE.
            ENDIF
         ELSE
            OP_METH_ALLOCATOR = 0.
         ENDIF
C
         DOWHILE (ITER <= 10)
            test_total_exp_taxes = 0.
            test_total_op_adj = 0.
            DO MO = 1, 12
               
C
C TOTAL THE MONTH
C            
               INCOME_VARIABLES(MO,Total Base Revenues) =
     +                       SUM_REGULATED_REVENUES(MO,INCOME_VARIABLES)
C
C REVENUES
C
               INCOME_VARIABLES(MO,TOTAL OPERATING REVENUES) =
     +                  INCOME_VARIABLES(MO,Total Base Revenues)
     +                  + SUM_NONREGULATED_REVENUES(MO,INCOME_VARIABLES)
C
C REVENUE TAXES
C
               INCOME_VARIABLES(MO,Monthly_Operating_Revenue_Tax) =
     +                 CALCULATE_REVENUE_TAXES(MO,CLASS_LEVEL,
     +                                         INCOME_VARIABLES,
     +                                         REVENUE_TAX_ADJUSTMENT,
     +                                         MONTHLY_REVENUE_TAX_RATE)
C
C OTHER TAXES
C
               INCOME_VARIABLES(MO,monthly_oth_taxes) = 
     +                          MONTHLY_OTHER_TAXES(MO,CLASS_LEVEL,
     +                                       INCOME_VARIABLES,
     +                                       OTHER_TAX_ADJUSTMENT,
     +                                       MONTHLY_OTHER_REV_TAX_RATE,
     +                                       MONTHLY_OTHER_EXP_TAX_RATE)
               IF(MO==12) INCOME_VARIABLES(MO,monthly_oth_taxes) =
     +                          INCOME_VARIABLES(MO,monthly_oth_taxes)
     +                          + ANNUAL_VARS(216) ! environmental tax
C
               INCOME_VARIABLES(MO,Monthly_Total_Taxes_Expense) =
     +              MONTHLY_TOTAL_EXPENSES_B4_TAXES(MO,INCOME_VARIABLES)
     +              + TOTAL_TAXES_EXPENSE(MO,INCOME_VARIABLES) 
C
C
C
               IF(USE_OP_METH_ALLOCATOR) THEN
                  INCOME_VARIABLES(MO,Operating Method Adjustment) = 
     +              OP_METH_ALLOCATOR *
     +                  INCOME_VARIABLES(MO,Monthly_Total_Taxes_Expense)
               ELSE
                  INCOME_VARIABLES(MO,Operating Method Adjustment) = 
     +               INCOME_VARIABLES(0,Operating Method Adjustment)/12.
               ENDIF
               test_total_op_adj = test_total_op_adj
     +                + INCOME_VARIABLES(MO,Operating Method Adjustment)
               test_total_exp_taxes = test_total_exp_taxes
     +                + INCOME_VARIABLES(MO,Monthly_Total_Taxes_Expense)
            ENDDO
            IF(USE_OP_METH_ALLOCATOR) THEN
               IF(ITER > 5 .AND. CONVERGENCE_TEST == 
     +              INCOME_VARIABLES(0,Operating Method Adjustment))EXIT
                  CONVERGENCE_TEST = test_total_op_adj
               IF(test_total_exp_taxes /= 0.) THEN
                  OP_METH_ALLOCATOR =
     +                  INCOME_VARIABLES(0,Operating Method Adjustment)/
     +                               test_total_exp_taxes
               ENDIF
            ELSE
               IF(ITER > 3) EXIT
            ENDIF
            ITER = ITER + 1
         ENDDO
C
         INCOME_VARIABLES(0,Monthly_Operating_Revenue_Tax) =
     +           SUM(INCOME_VARIABLES(1:,Monthly_Operating_Revenue_Tax))
         INCOME_VARIABLES(0,monthly_oth_taxes) =
     +                     SUM(INCOME_VARIABLES(1:,monthly_oth_taxes))
         INCOME_VARIABLES(0,Monthly_Total_Taxes_Expense) =
     +         MONTHLY_TOTAL_EXPENSES_B4_TAXES(INT2(0),INCOME_VARIABLES)
     +         + TOTAL_TAXES_EXPENSE(INT2(0),INCOME_VARIABLES) 
         INCOME_VARIABLES(0,Monthly_Op_Income) = 
     +                 INCOME_VARIABLES(0,monthly_total_revenues)
     +                 - INCOME_VARIABLES(0,Monthly_Total_Taxes_Expense)
         MO = 0
         INCOME_VARIABLES(0,Monthly_INCOME_BEFORE_INTEREST) = 
     +                       INCOME_BEFORE_INTEREST(MO,INCOME_VARIABLES)
         INCOME_VARIABLES(0,Monthly INCOME AFTER INTEREST) = 
     +                MONTHLY_INCOME_AFTER_INTEREST(MO,INCOME_VARIABLES)
         INCOME_VARIABLES(0,Monthly Net Income) = 
     +                           MONTHLY_NET_INCOME(MO,INCOME_VARIABLES)
         INCOME_VARIABLES(0,Monthly_Earnings_2_Common) = 
     +                 INCOME_VARIABLES(MO,Monthly Net Income)
     +                 - INCOME_VARIABLES(MO,Monthly_Total_PS_Dividends)
         INCOME_VARIABLES(0,Monthly RETAINED EARNINGS) = 
     +                    INCOME_VARIABLES(0,Monthly_Earnings_2_Common)
     +                    - INCOME_VARIABLES(0,Monthly Common Dividends)
     
         IF(CALCULATE_BTL_INCOME_TAXES) THEN
            DO MO = 1, 12  ! RPT_ are functions
               BTL_TAXABLE_INCOME(MO) = RPT_BTL_TAXABLE_INCOME(MO)
               INCOME_VARIABLES(MO,BTL_Monthly_Income_Taxes) =
     +                                  RPT_STATE_BTL_INCOME_TAX(MO)
     +                                  + RPT_FEDERAL_BTL_INCOME_TAX(MO)
            ENDDO
         ELSE
            INCOME_VARIABLES(:,BTL_Monthly_Income_Taxes) = 0.
            BTL_TAXABLE_INCOME(:) = 0.
         ENDIF
C
         DO MO = 1, 12
C
C  CALCULATE OP INCOME
C
            INCOME_VARIABLES(MO,Monthly_Op_Income) = 
     +                INCOME_VARIABLES(MO,monthly_total_revenues)
     +                - INCOME_VARIABLES(MO,Monthly_Total_Taxes_Expense)
C
C BTL ITEMS
C
            INCOME_VARIABLES(MO,BTL_Monthly_Income_Tax_Deferrals) =
     +          INCOME_VARIABLES(0,BTL_Monthly_Income_Tax_Deferrals)/12.
            INCOME_VARIABLES(MO,BTLMonthlyDeferralTaxCr) =
     +                   INCOME_VARIABLES(0,BTLMonthlyDeferralTaxCr)/12.
            INCOME_VARIABLES(MO,BTLMonthlyDeferralTaxDr) =
     +                   INCOME_VARIABLES(0,BTLMonthlyDeferralTaxDr)/12.
            INCOME_VARIABLES(MO,Monthly_LTD_Total_Interest) =
     +                 INCOME_VARIABLES(MO,Monthly_LTD_Total_Interest)
     +                 + INCOME_VARIABLES(MO,Monthly_LTD_Amort_Interest)

            INCOME_VARIABLES(MO,Monthly_Booked_PS_Dividends) =
     +                   INCOME_VARIABLES(MO,Monthly_Total_PS_Dividends)
            INCOME_VARIABLES(MO,Monthly_Total_PS_Dividends) =
     +                 INCOME_VARIABLES(MO,Monthly_Total_PS_Dividends)
     +                 + INCOME_VARIABLES(MO,Monthly_PS_Amort_Dividends)

            INCOME_VARIABLES(MO,Monthly_Extraordinary_Items) =
     +           INCOME_VARIABLES(0,Monthly_Extraordinary_Items)/12.
            INCOME_VARIABLES(MO,Monthly_INCOME_BEFORE_INTEREST) = 
     +                       INCOME_BEFORE_INTEREST(MO,INCOME_VARIABLES)
            INCOME_VARIABLES(MO,Monthly INCOME AFTER INTEREST) = 
     +                MONTHLY_INCOME_AFTER_INTEREST(MO,INCOME_VARIABLES)
            INCOME_VARIABLES(MO,Monthly Net Income) = 
     +                           MONTHLY_NET_INCOME(MO,INCOME_VARIABLES)
            INCOME_VARIABLES(MO,Monthly_Earnings_2_Common) = 
     +          INCOME_VARIABLES(MO,Monthly Net Income) -
     +          INCOME_VARIABLES(MO,Monthly_Total_PS_Dividends)
             INCOME_VARIABLES(MO,Monthly RETAINED EARNINGS) =
     +                   INCOME_VARIABLES(MO,Monthly_Earnings_2_Common)
     +                   - INCOME_VARIABLES(MO,Monthly Common Dividends)
         ENDDO
C
C PASS-UP CASH ITEMS
!  Deleted commented out code below this comment, which is probably no longer relevant.
         CALL LAG_CASH_VALUES(
     +                CASH_VARIABLES(0,cash_operating_revenue_tax),
     +                INCOME_VARIABLES(0,Monthly_Operating_Revenue_Tax),
     +                PAYABLE_MONTHLY_VALUES(0,
     +                         payment_operating_revenue_tax,CLASS_POS),
     +                payment_operating_revenue_tax,
     +                CLASS_ID)
         CALL LAG_CASH_VALUES(
     +                CASH_VARIABLES(0,cash_other_taxes),
     +                INCOME_VARIABLES(0,monthly_oth_taxes),
     +                PAYABLE_MONTHLY_VALUES(0,
     +                                  payment_other_taxes,CLASS_POS),
     +                payment_other_taxes,
     +                CLASS_ID)
         CALL LAG_PROPERTY_TAX_CASH_VALUES(
     +                CASH_VARIABLES(0,
     +                            cash_mdl_calcd_prop_taxes),
     +                INCOME_VARIABLES(0,
     +                         mth_modl_calcd_prop_taxes),
     +                PAYABLE_MONTHLY_VALUES(0,
     +                                payment_property_taxes,CLASS_POS),
     +                payment_property_taxes,
     +                CLASS_ID)

C mthy_state_atl_incm_tax_pd
         AVERAGE_VALUES = 0.
         AVERAGE_VALUES(0) =
     +           INCOME_VARIABLES(0,mty_tot_st_incm_tax_pd) !  ANNUAL_VARS(212)
         IF(KCPL()) THEN
            AVERAGE_VALUES(3) = SUM(INCOME_VARIABLES(1:3,
     +                             mty_tot_st_incm_tax_pd)) !  ANNUAL_VARS(212)
            AVERAGE_VALUES(5) = SUM(INCOME_VARIABLES(4:6,
     +                             mty_tot_st_incm_tax_pd)) !  ANNUAL_VARS(212)
            AVERAGE_VALUES(8) = SUM(INCOME_VARIABLES(7:9,
     +                             mty_tot_st_incm_tax_pd)) !  ANNUAL_VARS(212)
            AVERAGE_VALUES(12) = SUM(INCOME_VARIABLES(10:12,
     +                             mty_tot_st_incm_tax_pd)) !  ANNUAL_VARS(212)
         ELSE
            AVERAGE_VALUES(3) = AVERAGE_VALUES(0)/4.
            AVERAGE_VALUES(5) = AVERAGE_VALUES(0)/4.
            AVERAGE_VALUES(8) = AVERAGE_VALUES(0)/4.
            AVERAGE_VALUES(12) = AVERAGE_VALUES(0)/4.
         ENDIF
         IF(CLASS_TYPE == PARENT .OR. CLASS_TYPE == SUBSIDIARY) THEN
            CALL LAG_CASH_VALUES(
     +              CASH_VARIABLES(0,cash_st_income_taxes_pd),
     +              AVERAGE_VALUES, ! USING TAX PAYMENT FROM TAX REPORT
     +              PAYABLE_MONTHLY_VALUES(0,
     +                       payment_state_income_taxes_paid,CLASS_POS),
     +              payment_state_income_taxes_paid,
     +              CLASS_ID)
         ELSE
            CASH_VARIABLES(0,cash_st_income_taxes_pd) =
     +                                                 AVERAGE_VALUES(0)
            CASH_VARIABLES(4,cash_st_income_taxes_pd) =
     +                                                 AVERAGE_VALUES(3)
            CASH_VARIABLES(6,cash_st_income_taxes_pd) =
     +                                                 AVERAGE_VALUES(5)
            CASH_VARIABLES(9,cash_st_income_taxes_pd) =
     +                                                 AVERAGE_VALUES(8)
            CASH_VARIABLES(12,cash_st_income_taxes_pd) =
     +                                                AVERAGE_VALUES(12)
         ENDIF
C Federal Income Taxes Paid
         AVERAGE_VALUES = 0.
         AVERAGE_VALUES(0) =
     +             INCOME_VARIABLES(0,mty_tot_fed_incm_tax_pd) !  ANNUAL_VARS(211)
         IF(EMPIRE()) THEN
            AVERAGE_VALUES(3) = .10*AVERAGE_VALUES(0)
            AVERAGE_VALUES(5) = .15*AVERAGE_VALUES(0)
            AVERAGE_VALUES(8) = .25*AVERAGE_VALUES(0)
            AVERAGE_VALUES(12) = .50*AVERAGE_VALUES(0)
         ELSEIF(IPALCO().AND. CLASS_TYPE == PARENT) THEN
            AVERAGE_VALUES(3) = SUM(INCOME_VARIABLES(1:3,
     +                               mty_tot_fed_incm_tax_pd)) !  ANNUAL_VARS(211)
            AVERAGE_VALUES(5) = AVERAGE_VALUES(3)
            AVERAGE_VALUES(8) = 1.5* SUM(INCOME_VARIABLES(1:6,
     +                               mty_tot_fed_incm_tax_pd)) !  ANNUAL_VARS(211)
     +                               - AVERAGE_VALUES(3)
     +                               - AVERAGE_VALUES(5)
            AVERAGE_VALUES(12) =
     +                     (12.* SUM(INCOME_VARIABLES(1:9,
     +                           mty_tot_fed_incm_tax_pd)))/9. !  ANNUAL_VARS(211)
     +                      - AVERAGE_VALUES(3)
     +                      - AVERAGE_VALUES(5)
     +                      - AVERAGE_VALUES(8)
         ELSEIF(KCPL()) THEN
            AVERAGE_VALUES(3) = SUM(INCOME_VARIABLES(1:3,
     +                               mty_tot_fed_incm_tax_pd))
            AVERAGE_VALUES(5) = SUM(INCOME_VARIABLES(4:6,
     +                               mty_tot_fed_incm_tax_pd))
            AVERAGE_VALUES(8) = SUM(INCOME_VARIABLES(7:9,
     +                               mty_tot_fed_incm_tax_pd))
            AVERAGE_VALUES(12) = SUM(INCOME_VARIABLES(10:12,
     +                               mty_tot_fed_incm_tax_pd))
         ELSE
            IF(CLASS_TYPE == PARENT .and. .false.) THEN
               AVERAGE_VALUES(3) = AVERAGE_VALUES(0)/4.
               AVERAGE_VALUES(5) = AVERAGE_VALUES(0)/4.
               AVERAGE_VALUES(8) = AVERAGE_VALUES(0)/4.
               AVERAGE_VALUES(12) = AVERAGE_VALUES(0)/4.
            ELSE
               AVERAGE_VALUES(3) = AVERAGE_VALUES(0)/4.
               AVERAGE_VALUES(5) = AVERAGE_VALUES(0)/4.
               AVERAGE_VALUES(8) = AVERAGE_VALUES(0)/4.
               AVERAGE_VALUES(12) = AVERAGE_VALUES(0)/4.
            ENDIF
         ENDIF
         IF(IPALCO().AND. CLASS_TYPE == PARENT) THEN
            CASH_VARIABLES(4,cash_fed_income_taxs_pd) =
     +                                                 AVERAGE_VALUES(3)
            CASH_VARIABLES(6,cash_fed_income_taxs_pd) =
     +                                                 AVERAGE_VALUES(5)
            CASH_VARIABLES(9,cash_fed_income_taxs_pd) =
     +                                                 AVERAGE_VALUES(8)
            CASH_VARIABLES(12,cash_fed_income_taxs_pd) =
     +                                                AVERAGE_VALUES(12)
            TEMP_FEDERAL_INC_TAX_CARRY_OVER(CLASS_POS) =
     +                                          AVERAGE_VALUES(0)
     +                                          - (AVERAGE_VALUES(3)
     +                                             + AVERAGE_VALUES(5)
     +                                             + AVERAGE_VALUES(8)
     +                                             + AVERAGE_VALUES(12))
            CASH_VARIABLES(1:,cash_fed_income_taxs_pd) =
     +                 CASH_VARIABLES(1:,cash_fed_income_taxs_pd)
     +                  + PAYABLE_MONTHLY_VALUES(1:,
     +                      payment_federal_inc_taxes_paid,CLASS_POS)
            IF(FEDERAL_INC_TAX_CARRY_OVER(CLASS_POS) >= 0) THEN
               CASH_VARIABLES(3,cash_fed_income_taxs_pd) =
     +                  CASH_VARIABLES(3,cash_fed_income_taxs_pd)
     +                  + FEDERAL_INC_TAX_CARRY_OVER(CLASS_POS)
            ELSE
               CASH_VARIABLES(4,cash_fed_income_taxs_pd) =
     +                  CASH_VARIABLES(4,cash_fed_income_taxs_pd)
     +                  + FEDERAL_INC_TAX_CARRY_OVER(CLASS_POS)
            ENDIF 
            CASH_VARIABLES(0,cash_fed_income_taxs_pd) =
     +            SUM(CASH_VARIABLES(1:,cash_fed_income_taxs_pd))
         ELSEIF(CLASS_TYPE == PARENT) THEN
            CALL LAG_CASH_VALUES(
     +                CASH_VARIABLES(0,cash_fed_income_taxs_pd),
     +                AVERAGE_VALUES,
     +                PAYABLE_MONTHLY_VALUES(0,
     +                     payment_federal_inc_taxes_paid,CLASS_POS),
     +                     payment_federal_inc_taxes_paid,
     +                     CLASS_ID)
         ELSE
            CASH_VARIABLES(4,cash_fed_income_taxs_pd) =
     +                                                 AVERAGE_VALUES(3)
            CASH_VARIABLES(6,cash_fed_income_taxs_pd) =
     +                                                 AVERAGE_VALUES(5)
            CASH_VARIABLES(9,cash_fed_income_taxs_pd) =
     +                                                 AVERAGE_VALUES(8)
            CASH_VARIABLES(12,cash_fed_income_taxs_pd) =
     +                                                AVERAGE_VALUES(12)
            CASH_VARIABLES(0,cash_fed_income_taxs_pd) =
     +            SUM(CASH_VARIABLES(1:,cash_fed_income_taxs_pd))
         ENDIF
         FED_STATE_CASH_TAXES_PAID =
     +                  CASH_VARIABLES(0,cash_fed_income_taxs_pd)

C Monthly State Tax on Capital
         CALL LAG_CASH_VALUES(
     +                CASH_VARIABLES(0,cash_state_on_capital),
     +                INCOME_VARIABLES(0,Monthly_State_Tax_on_Capital),
     +                PAYABLE_MONTHLY_VALUES(0,
     +                          payment_state_tax_on_capital,CLASS_POS),
     +                payment_state_tax_on_capital,
     +                CLASS_ID)
C Federal Tax on Capital
         CALL LAG_CASH_VALUES(
     +               CASH_VARIABLES(0,cash_fed_tax_on_capital),
     +               INCOME_VARIABLES(0,Monthly_Federal_Tax_on_Capital),
     +               PAYABLE_MONTHLY_VALUES(0,
     +                        payment_federal_tax_on_capital,CLASS_POS),
     +               payment_federal_tax_on_capital,
     +               CLASS_ID)

C
         CASH_VARIABLES(0,cash_working_capital) = ANNUAL_VARS(118)
C
C TEMP INSTALLATION OF NUCLEAR FUEL
C
         CASH_VARIABLES(0,cash_nclr_fl_fabrication) =
     +                                                  ANNUAL_VARS(111)
C
         DO MO = 1, 12

            CASH_VARIABLES(MO,cash_working_capital) =
     +                            (ASSETS_NEC(MO)- LIABS_NEC(MO))
     +                             -(ASSETS_NEC(MO-1) - LIABS_NEC(MO-1))
            CASH_VARIABLES(MO,cash_nclr_fl_fabrication) =
     +                                              ANNUAL_VARS(111)/12.
         ENDDO
         IF(CPL_ACTIVE() .OR. MORTGAGE_DEBT) THEN
            CASH_VARIABLES(1:,cash_ltd_interest) =
     +                              CASH_VARIABLES(1:,cash_ltd_interest)
     +                              + LTD_PAID_ISSUED_INTEREST(1:)
            CASH_VARIABLES(1:,cash_ltd_retirements) =
     +                           CASH_VARIABLES(1:,cash_ltd_retirements)
     +                           + MGT_DEBIT_RETIREMENT(1:) ! Mortgage Principal Payments
            MGT_DEBIT_RETIREMENT(:) = 0. ! Mortgage Principal Payments
         ELSE

            IF(CLASS_TYPE == REGULATED_GROUP) THEN
               CASH_VARIABLES(7,cash_ltd_retirements) = LTD_RETIREMENTS
               CASH_VARIABLES(1:,cash_ltd_interest) =
     +                   INCOME_VARIABLES(1:,Monthly_LTD_Total_Interest)
            ELSE
               CASH_VARIABLES(10,cash_ltd_interest) =
     +                              CASH_VARIABLES(10,cash_ltd_interest)
     +                              + LTD_ISSUE_YR_INTEREST_PAYMENT
            ENDIF
         ENDIF
         CASH_VARIABLES(0,cash_ltd_retirements) =
     +                      SUM(CASH_VARIABLES(1:,cash_ltd_retirements))
C
         CASH_VARIABLES(0,cash_ltd_interest) =
     +                         SUM(CASH_VARIABLES(1:,cash_ltd_interest))
         IF(EMPIRE()) THEN
            CASH_VARIABLES(0,cash_ps_dividends) =
     +                               CASH_VARIABLES(0,cash_ps_dividends)
     +                               + PS_ISSUE_YR_DIVIDEND_PAYMENT
            CASH_VARIABLES(9,cash_ps_dividends) =
     +                              CASH_VARIABLES(9,cash_ps_dividends)
     +                              + PS_ISSUE_YR_DIVIDEND_PAYMENT/2.
            CASH_VARIABLES(12,cash_ps_dividends) =
     +                              CASH_VARIABLES(12,cash_ps_dividends)
     +                              + PS_ISSUE_YR_DIVIDEND_PAYMENT/2.
         ELSEIF(CLASS_TYPE == REGULATED_GROUP) THEN
            CASH_VARIABLES(7,cash_ps_retirements) = PS_RETIREMENTS
            CASH_VARIABLES(7,cash_ps_issued) = PREFERRED_STOCK_ISSUED
            CASH_VARIABLES(0,cash_ps_dividends) =
     +                                      PS_ISSUE_YR_DIVIDEND_PAYMENT
            CASH_VARIABLES(9,cash_ps_dividends) =
     +                                   PS_ISSUE_YR_DIVIDEND_PAYMENT/2.
            CASH_VARIABLES(12,cash_ps_dividends) =
     +                                   PS_ISSUE_YR_DIVIDEND_PAYMENT/2.
         ELSE
            CASH_VARIABLES(0,cash_ps_dividends) =
     +                               CASH_VARIABLES(0,cash_ps_dividends)
     +                               + PS_ISSUE_YR_DIVIDEND_PAYMENT
            CASH_VARIABLES(10,cash_ps_dividends) =
     +                              CASH_VARIABLES(10,cash_ps_dividends)
     +                              + PS_ISSUE_YR_DIVIDEND_PAYMENT
         ENDIF
C

         EOY_LONG_TERM_INVESTMENTS = BOY_LONG_TERM_INVESTMENTS
     +                  + CASH_VARIABLES(0,cash_change_ltinvestments)
     +                  + INVESTMENTS_BOY
     +                  + CASH_VARIABLES(0,cash_change_debt_investments)
         IF(EOY_LONG_TERM_INVESTMENTS < MIN_LONG_TERM_INVESTMENT) THEN
            CASH_VARIABLES(0,cash_change_ltinvestments) =
     +                       CASH_VARIABLES(0,cash_change_ltinvestments)
     +                       + MIN_LONG_TERM_INVESTMENT
     +                       - EOY_LONG_TERM_INVESTMENTS
            CASH_VARIABLES(1,cash_change_ltinvestments) =
     +                       CASH_VARIABLES(1,cash_change_ltinvestments)
     +                       + MIN_LONG_TERM_INVESTMENT
     +                       - EOY_LONG_TERM_INVESTMENTS
         ELSEIF(EOY_LONG_TERM_INVESTMENTS>MAX_LONG_TERM_INVESTMENT) THEN
            CASH_VARIABLES(0,cash_change_ltinvestments) =
     +                       CASH_VARIABLES(0,cash_change_ltinvestments)
     +                       + MIN_LONG_TERM_INVESTMENT
     +                       - EOY_LONG_TERM_INVESTMENTS
            CASH_VARIABLES(1,cash_change_ltinvestments) =
     +                       CASH_VARIABLES(1,cash_change_ltinvestments)
     +                       + MAX_LONG_TERM_INVESTMENT
     +                       - EOY_LONG_TERM_INVESTMENTS
C
         ENDIF
C
C NEW ACQUISITION COSTS
C
         CASH_VARIABLES(1:,cash_acquisition_cost) =
     +                                         NEW_ACQUISITIONS_COST(1:)
         CASH_VARIABLES(0,cash_acquisition_cost) =
     +                     SUM(CASH_VARIABLES(1:,cash_acquisition_cost))
C
C REPLACE WITH ACTUAL CASH PAYMENTS
C
         CALL GET_ACTUAL_MONTHLY_PAYABLES(R_YR,CLASS_POS,CASH_VARIABLES)
         CALL RETURN_MONTHLY_CAP_X_VALUES(
     +                       CASH_VARIABLES(0,noncash_pension_in_capx)) 
         CASH_VARIABLES(:,cash_net_plant_capx) = 
     +                      CASH_VARIABLES(:,cash_plant_construction)
     +                      - CASH_VARIABLES(:,noncash_pension_in_capx)
         IF(CLASS_TYPE == PARENT) THEN
            CALL GET_ACTUAL_CONSOLID_PAYABLES(R_YR,
     +                                           CONSOLIDATED_VARIABLES)
         ENDIF
C
C PROPERTY TAXES
C
         INCOME_VARIABLES(:,Monthly_Property_Taxes) =
     +              INCOME_VARIABLES(:,MonthlyExpFilePropertyTaxes)
     +              + INCOME_VARIABLES(:,
     +                          mth_modl_calcd_prop_taxes)
         CASH_VARIABLES(:,cash_property_taxes) =
     +            CASH_VARIABLES(:,cash_mdl_calcd_prop_taxes)  
     +            + CASH_VARIABLES(:,cash_exp_file_prop_taxes)  
C
C OTHER TAXES
C
         INCOME_VARIABLES(:,monthly_oth_taxes) =
     +                INCOME_VARIABLES(:,monthly_oth_taxes)
     +                + INCOME_VARIABLES(:,MonthlyExpFileOtherTaxes)
         CASH_VARIABLES(:,cash_other_taxes) =
     +                     CASH_VARIABLES(:,cash_other_taxes)
     +                     + CASH_VARIABLES(:,cash_exp_file_othr_taxes)
C
C REVENUE TAXES
C
         INCOME_VARIABLES(:,Monthly_Operating_Revenue_Tax) =
     +                INCOME_VARIABLES(:,Monthly_Operating_Revenue_Tax)
     +                + INCOME_VARIABLES(:,
     +                           mthy_exp_file_opng_revnue_tax)
         CASH_VARIABLES(:,cash_operating_revenue_tax) =
     +                     CASH_VARIABLES(:,cash_operating_revenue_tax)
     +                     + CASH_VARIABLES(:,
     +                              cash_exp_file_oprn_rev_tax)
C
         DO MO = 0, 12
            CASH_VARIABLES(MO,Cash_Operating_Method_Adj) =
     +                  INCOME_VARIABLES(MO,Operating Method Adjustment)
            CASH_VARIABLES(MO,Cash_Prior_Years_Method_Adj) =
     +                  INCOME_VARIABLES(MO,
     +                                    Prior Years Method Adjustment)
            CASH_VARIABLES(MO,csh_st_invstmt_incm) =
     +                                      CASH_BTL_INTEREST_INCOME(MO)

            CASH_VARIABLES(MO,Cash_TOTAL_OPERATING_RECEIPTS) =
     +                  CASH_VARIABLES(MO,Cash_TOTAL_OPERATING_RECEIPTS)
     +                  + CASH_VARIABLES(MO,
     +                                csh_st_invstmt_incm)
     +                  + CASH_VARIABLES(MO,Cash_Operating_Method_Adj)
     +                  + CASH_VARIABLES(MO,Cash_Prior_level_Method_Adj)
     +                  + CASH_VARIABLES(MO,
     +                                cash_investmt_divnd_earngs)
     +                  + CASH_VARIABLES(MO,
     +                                cash_invest_interest_ernings)
     +                  + CASH_VARIABLES(MO,Cash_Prior_Years_Method_Adj)

            CASH_VARIABLES(MO,net_cash_receipts_or_pmts) =
     +                CASH_VARIABLES(MO,Cash_TOTAL_OPERATING_RECEIPTS) -
     +                CASH_VARIABLES(MO,Cash_TOTAL_EXPENSE_PAYMENTS)
C
C CAPITAL SERVICE
C
            CASH_VARIABLES(MO,cash_capital_svc_pmts) =
     +               CASH_VARIABLES(MO,cash_ps_dividends)
     +               + CASH_VARIABLES(MO,cash_ltd_interest)
     +               + CASH_VARIABLES(MO,cash_std_interest)
     +               + CASH_VARIABLES(MO,cash_common_dividends)
     +               + CASH_VARIABLES(MO,Cash_Interest_on_Notes_Payable)
C
C TAXES 
C
            CASH_VARIABLES(MO,cash_total_tax_payments) = 
     +               CASH_VARIABLES(MO,cash_operating_revenue_tax)
     +               + CASH_VARIABLES(MO,cash_other_taxes)
     +               + CASH_VARIABLES(MO,cash_property_taxes)
     +               + CASH_VARIABLES(MO,cash_st_income_taxes_pd)
     +               + CASH_VARIABLES(MO,cash_state_on_capital)
     +               + CASH_VARIABLES(MO,cash_fed_income_taxs_pd)
     +               + CASH_VARIABLES(MO,cash_fed_tax_on_capital)
C
C
C
            IF(CLASS_TYPE == PARENT) THEN
               CASH_VARIABLES(MO,cash_parent_taxes_pd) =
     +                CASH_VARIABLES(MO,cash_fed_income_taxs_pd)

               CASH_VARIABLES(MO,cash_subsidiary_tax_pmts) =
     +                             SUB_FED_TAX_PAYMENTS_2_PARENT(MO)
     +                             + SUB_STATE_TAX_PAYMENTS_2_PARENT(MO)
               CASH_VARIABLES(MO,cash_consolidate_taxes_pd) =
     +                             CONSOLIDATED_VARIABLES(MO,
     +                                   cash_fed_income_taxs_pd)


            ENDIF
            CASH_VARIABLES(MO,total_tax_and_other) =
     +                 + CASH_VARIABLES(MO,cash_total_tax_payments)
     +                 + CASH_VARIABLES(MO,
     +                       cash_subsdry_st_taxes_pd_by_pt)
     +                 - CASH_VARIABLES(MO,cash_parent_taxes_pd)
     +                 - CASH_VARIABLES(MO,cash_subsidiary_tax_pmts)
     +                 + CASH_VARIABLES(MO,cash_consolidate_taxes_pd)
C
C TOTALS FROM OTHER SOURCES
C
            CASH_VARIABLES(MO,total_fm_other_sources) =
     +                    + CASH_VARIABLES(MO,cash_subsidiary_dividends)
     +                     + CASH_VARIABLES(MO,cash_customer_deposits)
     +                     + CASH_VARIABLES(MO,cash_ciac)
     +                     + CASH_VARIABLES(MO,cash_from_assets_sale)
     +                     + CASH_VARIABLES(MO,
     +                              cash_fm_repaid_issued_notes)
     +                     + CASH_VARIABLES(MO,
     +                                 cash_fm_notes_issd_by_othrs)
C
C CONSTRUCTION
C
            CASH_VARIABLES(MO,cash_total_construction) = 
     +                 SUM_CONSTRUCTION_CASH_PAYMENTS(MO,CASH_VARIABLES)
            CASH_VARIABLES(MO,total_capital_requirements) =
     +            SUM_CASH_TOTAL_CAPITAL_REQUIREMENTS(MO,CASH_VARIABLES)
            CASH_VARIABLES(MO,funds_fm_operations) =
     +                  CASH_VARIABLES(MO,net_cash_receipts_or_pmts)
     +                - CASH_VARIABLES(MO,cash_capital_svc_pmts)
C    +                - CASH_VARIABLES(MO,cash_total_tax_payments)
     +                - CASH_VARIABLES(MO,total_tax_and_other)
     +                + CASH_VARIABLES(MO,total_fm_other_sources)
            CASH_VARIABLES(MO,funds_change_b4_financing) =
     +                   CASH_VARIABLES(MO,funds_fm_operations)
     +                   - CASH_VARIABLES(MO,total_capital_requirements)
C
         ENDDO
C
C TAX STUFF
C
         CALL TAX_ISSUES_AND_ANALYSIS(YR,CLASS_ID,
     +                                CLASS_TYPE,
     +                                CLASS_LEVEL,
     +                                ANNUAL_VARS)
         IF(R_ITER >= 1) THEN ! Do monthly distribution on cash 
            BALANCE_SHEET_VARIABLES(0,monthly_short_term_debt_bal) =
     +                                               OPENING_STD_BALANCE
            BALANCE_SHEET_VARIABLES(0,monthly_short_lti) =
     +                                                FUNDS_BALANCE(BOY)
            BALANCE_SHEET_VARIABLES(0,
     +                             monthly_cash_lti) =
     +                                         BOY_LONG_TERM_INVESTMENTS
            CASH_VARIABLES(0,cash_period_change) = 0.
            CASH_VARIABLES(0,external_financing_pfmd) = 0.
            CASH_VARIABLES(1,cash_opening_balance) = FUNDS_BALANCE(BOY)
            CASH_VARIABLES(0,cash_opening_balance) = FUNDS_BALANCE(BOY)
            STD_BALANCE = OPENING_STD_BALANCE
            INCOME_VARIABLES(0,BTL_Monthly_Interest_Income) = 0.
            CASH_VARIABLES(0,csh_st_invstmt_incm) = 0.
c           CASH_VARIABLES(0,cash_change_ltinvestments) = 0.
            IF(CPL_ACTIVE()) THEN
               CASH_VARIABLES(7,cash_ltd_issued) =
     +                                 CASH_VARIABLES(7,cash_ltd_issued)
     +                                 - LONG_TERM_DEBT_ISSUED
            ENDIF
            CPL_MIN_LTD_ISSUE_AMOUNT = LTDMIN
            LTD_ISSUED_DISTRIBUTION(0) = LONG_TERM_DEBT_ISSUED
            CASH_VARIABLES(0,cash_ltd_issued) = 0.
C
C
            DO MO = 1, 12
               LTD_ISSUED_DISTRIBUTION(MO) = 0.
               CASH_VARIABLES(MO,cash_period_change) = 0
               CHANGE_IN_FUNDS =
     +               CASH_VARIABLES(MO,funds_change_b4_financing)
     +               + CASH_VARIABLES(MO,cash_lease_receipts)
     +               + CASH_VARIABLES(MO,cash_2_cash)
               FUNDS_4_ST_INVESTMENTS = 0.
               IF(MO >= 7 .AND.
     +                CASH_VARIABLES(MO,cash_opening_balance) <
     +                                        MINIMUM_CASH_BALANCE) THEN
                  FUNDS_4_ST_INVESTMENTS = MINIMUM_CASH_BALANCE
     +                         - CASH_VARIABLES(MO,cash_opening_balance)
                  CHANGE_IN_FUNDS = CHANGE_IN_FUNDS
     +                              - FUNDS_4_ST_INVESTMENTS
               ENDIF
               CAPITAL_ISSUES = CASH_VARIABLES(MO,cash_ps_issued)
     +                     + CASH_VARIABLES(MO,cash_ltd_issued)
     +                     + CASH_VARIABLES(MO,cash_common_stock_issued)
               IF(CPL_ACTIVE()) THEN
                  AVAILABLE_FUNDS = CHANGE_IN_FUNDS + CAPITAL_ISSUES
                  IF(AVAILABLE_FUNDS > 0.) THEN
                     CHANGE_IN_FUNDS = AVAILABLE_FUNDS
                     IF(CHANGE_IN_FUNDS >= 0. .AND.        
     +                           STD_BALANCE > STD_MINIMUM_BALANCE) THEN
                        CASH_VARIABLES(MO,cash_std_issued) =
     +                          -(MIN(CHANGE_IN_FUNDS,
     +                               STD_BALANCE - STD_MINIMUM_BALANCE))
                        AVAILABLE_FUNDS = AVAILABLE_FUNDS
     +                              + CASH_VARIABLES(MO,cash_std_issued)
                     ENDIF
                     CASH_VARIABLES(MO,cash_period_change) = 
     +                                                   AVAILABLE_FUNDS
                  ELSE ! MORE CASH IS NEEDED
                     CASH_NEEDED=ABS(CHANGE_IN_FUNDS+CAPITAL_ISSUES)
                     IF(STD_BALANCE < STD_MINIMUM_BALANCE) THEN
                        CASH_VARIABLES(MO,cash_std_issued) =
     +                              MIN(CASH_NEEDED,
     +                                  STD_MINIMUM_BALANCE-STD_BALANCE)
                        CASH_NEEDED = ABS(CASH_NEEDED -
     +                               CASH_VARIABLES(MO,cash_std_issued))
                        IF(CASH_NEEDED < .00001) CASH_NEEDED = 0.
                     ENDIF
                     IF(CASH_NEEDED > 0.) THEN
                        IF(CASH_VARIABLES(MO,cash_opening_balance) > 
     +                                        MINIMUM_CASH_BALANCE) THEN
                           CASH_VARIABLES(MO,cash_period_change) =
     +                         -MIN(CASH_NEEDED,
     +                           CASH_VARIABLES(MO,cash_opening_balance)
     +                                       - MINIMUM_CASH_BALANCE)
                           CASH_NEEDED = ABS(CASH_NEEDED +
     +                            CASH_VARIABLES(MO,cash_period_change))
                        ENDIF
                        IF(CASH_NEEDED > 0.) THEN
C THE IF THEN ELSE WAS ADDED TO MOVE THE LTD ISSUES TO THE MONTH 
C NEEDED FOR CASH ORGINAL CODE IS THE COMMENTED OUT PIECE.
C
                           IF(STD_BALANCE + CASH_NEEDED > STD_MAX .AND.
     +                                  LONG_TERM_DEBT_ISSUED > 0.) THEN
C
C NEED TO ISSUE THE MIN OF CASH_NEEDED OF UNISSUED LTD 4/19/99
C 
C
                              LTD_ISSUED_THIS_MONTH = 0
                              DOWHILE(LTD_ISSUED_THIS_MONTH<CASH_NEEDED)
                                 LTD_ISSUED_THIS_MONTH =
     +                                        LTD_ISSUED_THIS_MONTH
     +                                        + CPL_MIN_LTD_ISSUE_AMOUNT
                                 LONG_TERM_DEBT_ISSUED = MAX(0., 
     +                                    LONG_TERM_DEBT_ISSUED
     +                                       - CPL_MIN_LTD_ISSUE_AMOUNT)
                                 IF(LDUNIT <= 0.) THEN
                                    CPL_MIN_LTD_ISSUE_AMOUNT =
     +                                             LONG_TERM_DEBT_ISSUED
                                 ELSE   
                                    CPL_MIN_LTD_ISSUE_AMOUNT = LDUNIT
                                 ENDIF
                                 IF(LONG_TERM_DEBT_ISSUED == 0.) THEN
                                    IF(LTD_ISSUED_THIS_MONTH <
     +                                                 CASH_NEEDED) THEN
                                       LTD_ISSUED_THIS_MONTH =
     +                                        LTD_ISSUED_THIS_MONTH
     +                                        + CPL_MIN_LTD_ISSUE_AMOUNT
                                    ENDIF
                                    EXIT
                                 ENDIF

                              ENDDO   
                              CASH_VARIABLES(MO,cash_ltd_issued) =
     +                                CASH_VARIABLES(MO,cash_ltd_issued)
     +                                + LTD_ISSUED_THIS_MONTH
                              LTD_ISSUED_DISTRIBUTION(MO) =
     +                                             LTD_ISSUED_THIS_MONTH
                              CASH_NEEDED = CASH_NEEDED
     +                                      - LTD_ISSUED_THIS_MONTH
                              IF(CASH_NEEDED + STD_BALANCE <
     +                                         STD_MINIMUM_BALANCE) THEN
                                 CASH_VARIABLES(MO,cash_std_issued) =
     +                                   STD_MINIMUM_BALANCE-STD_BALANCE
                                 CASH_VARIABLES(MO,cash_period_change) =
     +                             CASH_VARIABLES(MO,cash_period_change)
     +                             + ABS(CASH_NEEDED -
     +                                (STD_MINIMUM_BALANCE-STD_BALANCE))
                              ELSE
                                 CASH_VARIABLES(MO,cash_std_issued) =
     +                                                       CASH_NEEDED
                              ENDIF
                           ELSE
                              CASH_VARIABLES(MO,cash_std_issued) =
     +                                CASH_VARIABLES(MO,cash_std_issued)
     +                                + CASH_NEEDED
                           ENDIF

                        ENDIF
                     ENDIF
                  ENDIF
				  
c test of fix cash balance
! There was code below this comment, so don't know whether
! it refers to the code below anymore.

               ELSEIF(EMPIRE() .OR. .TRUE.) THEN
                  IF(MO == FIXED_ISSUE_MONTH)
     +               LTD_ISSUED_DISTRIBUTION(MO) = LONG_TERM_DEBT_ISSUED
                  AVAILABLE_FUNDS = CHANGE_IN_FUNDS + CAPITAL_ISSUES
                  IF(AVAILABLE_FUNDS > 0.) THEN
                     IF(AVAILABLE_FUNDS > 0. .AND.
     +                           STD_BALANCE > STD_MINIMUM_BALANCE) THEN
                        CASH_VARIABLES(MO,cash_std_issued) =
     +                          -(MIN(AVAILABLE_FUNDS,
     +                               STD_BALANCE - STD_MINIMUM_BALANCE))
                        AVAILABLE_FUNDS = AVAILABLE_FUNDS
     +                              + CASH_VARIABLES(MO,cash_std_issued)
                     ENDIF
                     CASH_VARIABLES(MO,cash_period_change) = 
     +                                                   AVAILABLE_FUNDS
                  ELSE ! MORE CASH IS NEEDED
                     CASH_NEEDED=ABS(CHANGE_IN_FUNDS+CAPITAL_ISSUES)
                     IF(STD_BALANCE < STD_MINIMUM_BALANCE) THEN
                        CASH_VARIABLES(MO,cash_std_issued) =
     +                              MIN(CASH_NEEDED,
     +                                  STD_MINIMUM_BALANCE-STD_BALANCE)
                        CASH_NEEDED = ABS(CASH_NEEDED -
     +                               CASH_VARIABLES(MO,cash_std_issued))
                        IF(CASH_NEEDED < .00001) CASH_NEEDED = 0.
                     ENDIF
                     IF(CASH_NEEDED > 0.) THEN
                        IF(CASH_VARIABLES(MO,cash_opening_balance) > 
     +                                        MINIMUM_CASH_BALANCE) THEN
                           CASH_VARIABLES(MO,cash_period_change) =
     +                         -MIN(CASH_NEEDED,
     +                           CASH_VARIABLES(MO,cash_opening_balance)
     +                                       - MINIMUM_CASH_BALANCE)
                           CASH_NEEDED = ABS(CASH_NEEDED +
     +                            CASH_VARIABLES(MO,cash_period_change))
                        ENDIF
                        IF(CASH_NEEDED > 0.) THEN
                           CASH_VARIABLES(MO,cash_std_issued) =
     +                                CASH_VARIABLES(MO,cash_std_issued)
     +                                + CASH_NEEDED
                        ENDIF
                     ENDIF
                  ENDIF
               ELSE
                  IF(CHANGE_IN_FUNDS >= 0.) THEN
                     IF(CAPITAL_ISSUES > 0.) THEN
                        AVAILABLE_FUNDS = CHANGE_IN_FUNDS+CAPITAL_ISSUES
                        IF(STD_BALANCE > STD_MINIMUM_BALANCE) THEN
                           CASH_VARIABLES(MO,cash_std_issued) =
     +                          -(MIN(AVAILABLE_FUNDS,
     +                               STD_BALANCE - STD_MINIMUM_BALANCE))
                           AVAILABLE_FUNDS = AVAILABLE_FUNDS
     =                              + CASH_VARIABLES(MO,cash_std_issued)
                           CASH_VARIABLES(MO,cash_period_change) = 
     +                                                   AVAILABLE_FUNDS
                        ELSE
                           CASH_VARIABLES(MO,cash_std_issued) = 0.
                           CASH_VARIABLES(MO,cash_period_change) = 
     +                                                   AVAILABLE_FUNDS
                        ENDIF
                     ELSE ! CAPITAL ISSUES WERE REFUNDS
                        IF(CHANGE_IN_FUNDS + CAPITAL_ISSUES >= 0.) THEN
                           CASH_VARIABLES(MO,cash_std_issued) = 0.
                           CASH_VARIABLES(MO,cash_period_change) = 
     +                                  CHANGE_IN_FUNDS + CAPITAL_ISSUES
                        ELSE ! NEED CASH FROM STD OR CASH BALANCES
                           CASH_NEEDED =
     +                               ABS(CHANGE_IN_FUNDS+CAPITAL_ISSUES)
                           IF(STD_BALANCE < STD_MINIMUM_BALANCE) THEN
                              CASH_VARIABLES(MO,cash_std_issued) =
     +                              MIN(CASH_NEEDED,
     +                                  STD_MINIMUM_BALANCE-STD_BALANCE)
                              CASH_NEEDED = ABS(CASH_NEEDED -
     +                               CASH_VARIABLES(MO,cash_std_issued))
                              IF(CASH_NEEDED < .00001) CASH_NEEDED = 0.
                           ENDIF
                           IF(CASH_NEEDED > 0.) THEN
                              IF(CASH_VARIABLES(MO,cash_opening_balance)
     +                                      > MINIMUM_CASH_BALANCE) THEN
                                 CASH_VARIABLES(MO,cash_period_change) =
     +                              -MIN(CASH_NEEDED,
     +                                    CASH_VARIABLES(MO,
     +                                             cash_opening_balance)
     +                                    - MINIMUM_CASH_BALANCE)
                                 CASH_NEEDED = ABS(CASH_NEEDED +
     +                            CASH_VARIABLES(MO,cash_period_change))
                              ENDIF
                              IF(CASH_NEEDED > 0.) THEN
                                 CASH_VARIABLES(MO,cash_std_issued) =
     +                                CASH_VARIABLES(MO,cash_std_issued)
     +                                + CASH_NEEDED
                              ENDIF
                           ENDIF
                        ENDIF
                     ENDIF
                  ELSE ! CHANGE IN FUNDS IS NEGATIVE
                     IF(CAPITAL_ISSUES >= 0.) THEN
                        AVAILABLE_FUNDS = CHANGE_IN_FUNDS+CAPITAL_ISSUES
                        IF(AVAILABLE_FUNDS >= 0.) THEN
                           IF(STD_BALANCE > STD_MINIMUM_BALANCE) THEN
                              CASH_VARIABLES(MO,cash_std_issued) =
     +                          -(MIN(AVAILABLE_FUNDS,
     +                               STD_BALANCE - STD_MINIMUM_BALANCE))
                              AVAILABLE_FUNDS = AVAILABLE_FUNDS
     =                              + CASH_VARIABLES(MO,cash_std_issued)
                              CASH_VARIABLES(MO,cash_period_change) = 
     +                                                   AVAILABLE_FUNDS

                           ELSE
                              CASH_VARIABLES(MO,cash_std_issued) = 0.
                              CASH_VARIABLES(MO,cash_period_change) = 
     +                                  CHANGE_IN_FUNDS + CAPITAL_ISSUES
                           ENDIF
                        ELSE ! NEED MORE FUNDS
                           CASH_NEEDED =
     +                               ABS(CHANGE_IN_FUNDS+CAPITAL_ISSUES)
                           IF(STD_BALANCE < STD_MINIMUM_BALANCE) THEN
                              CASH_VARIABLES(MO,cash_std_issued) =
     +                              MIN(CASH_NEEDED,
     +                                  STD_MINIMUM_BALANCE-STD_BALANCE)
                              CASH_NEEDED = ABS(CASH_NEEDED -
     +                               CASH_VARIABLES(MO,cash_std_issued))
                              IF(CASH_NEEDED < .00001) CASH_NEEDED = 0.
                           ENDIF
                           IF(CASH_NEEDED > 0.) THEN
                              IF(CASH_VARIABLES(MO,cash_opening_balance)
     +                                      > MINIMUM_CASH_BALANCE) THEN
                                 CASH_VARIABLES(MO,cash_period_change) =
     +                           -MIN(CASH_NEEDED,
     +                           CASH_VARIABLES(MO,cash_opening_balance)
     +                                       - MINIMUM_CASH_BALANCE)
                                 CASH_NEEDED = ABS(CASH_NEEDED +
     +                            CASH_VARIABLES(MO,cash_period_change))
                              ENDIF
                              IF(CASH_NEEDED > 0.) THEN
                                 CASH_VARIABLES(MO,cash_std_issued) =
     +                                CASH_VARIABLES(MO,cash_std_issued)
     +                                + CASH_NEEDED
                              ENDIF
                           ENDIF
                        ENDIF
                     ELSE ! CAPITAL ISSUES WERE REFUNDS NEED CASH
                        CASH_NEEDED=ABS(CHANGE_IN_FUNDS+CAPITAL_ISSUES)
                        IF(STD_BALANCE < STD_MINIMUM_BALANCE) THEN
                           CASH_VARIABLES(MO,cash_std_issued) =
     +                              MIN(CASH_NEEDED,
     +                                  STD_MINIMUM_BALANCE-STD_BALANCE)
                           CASH_NEEDED = ABS(CASH_NEEDED -
     +                               CASH_VARIABLES(MO,cash_std_issued))
                              IF(CASH_NEEDED < .00001) CASH_NEEDED = 0.
                        ENDIF
                        IF(CASH_NEEDED > 0.) THEN
                           IF(CASH_VARIABLES(MO,cash_opening_balance) > 
     +                                        MINIMUM_CASH_BALANCE) THEN
                              CASH_VARIABLES(MO,cash_period_change) =
     +                           -MIN(CASH_NEEDED,
     +                           CASH_VARIABLES(MO,cash_opening_balance)
     +                                       - MINIMUM_CASH_BALANCE)
                              CASH_NEEDED = ABS(CASH_NEEDED +
     +                            CASH_VARIABLES(MO,cash_period_change))
                           ENDIF
                           IF(CASH_NEEDED > 0.) THEN
                              CASH_VARIABLES(MO,cash_std_issued) =
     +                                CASH_VARIABLES(MO,cash_std_issued)
     +                                + CASH_NEEDED
                           ENDIF
                        ENDIF
                     ENDIF
                  ENDIF
               ENDIF
               STD_BALANCE = STD_BALANCE +
     +                                CASH_VARIABLES(MO,cash_std_issued)

               LTI_EARNINGS(MO) = MONTHLY_LTI_RATE(MO) *
     +               (BALANCE_SHEET_VARIABLES(MO-1,
     +                               monthly_cash_lti)
     +                + CASH_VARIABLES(MO,cash_change_ltinvestments)/2.)
               IF(SALT_RIVER_PROJECT()) THEN  ! REPLACE WITH SWITCH
                  CASH_VARIABLES(MO,cash_change_ltinvestments) =
     +                      CASH_VARIABLES(MO,cash_change_ltinvestments)
     +                      + LTI_EARNINGS(MO)! ADDED 5/13/04
                  CASH_VARIABLES(MO,funds_change_b4_financing) =
     +               CASH_VARIABLES(MO,funds_change_b4_financing)
     +               - LTI_EARNINGS(MO)! ADDED 5/13/04
               ENDIF
               BALANCE_SHEET_VARIABLES(MO,
     +                             monthly_cash_lti) =
     +                    BALANCE_SHEET_VARIABLES(MO-1,
     +                               monthly_cash_lti)
     +                    + CASH_VARIABLES(MO,cash_change_ltinvestments)
               CASH_VARIABLES(MO,cash_period_change) =
     +               CASH_VARIABLES(MO,funds_change_b4_financing)
     +               + CASH_VARIABLES(MO,cash_ps_issued)
     +               + CASH_VARIABLES(MO,cash_ltd_issued)
     +               + CASH_VARIABLES(MO,cash_std_issued)
     +               + CASH_VARIABLES(MO,cash_common_stock_issued)
     +               + CASH_VARIABLES(MO,cash_lease_receipts)
               BALANCE_SHEET_VARIABLES(MO,monthly_short_term_debt_bal) =
     +                                                       STD_BALANCE
               CASH_VARIABLES(MO,cash_closing_balance) = 
     +                         CASH_VARIABLES(MO,cash_opening_balance)
     +                         + CASH_VARIABLES(MO,cash_period_change)
               BALANCE_SHEET_VARIABLES(MO,
     +                                 monthly_short_lti) =
     +                           CASH_VARIABLES(MO,cash_closing_balance)
               BALANCE_SHEET_VARIABLES(MO,
     +                             monthly_cash_lti) =
     +                    BALANCE_SHEET_VARIABLES(MO-1,
     +                               monthly_cash_lti)
     +                    + CASH_VARIABLES(MO,cash_change_ltinvestments)
               IF(MO < 12) THEN
                  CASH_VARIABLES(MO+1,cash_opening_balance) = 
     +                           CASH_VARIABLES(MO,cash_closing_balance)
               ENDIF
               CASH_VARIABLES(MO,external_financing_pfmd) =
     +                     CASH_VARIABLES(MO,cash_ps_issued)
     +                     + CASH_VARIABLES(MO,cash_ltd_issued)
     +                     + CASH_VARIABLES(MO,cash_std_issued)
     +                     + CASH_VARIABLES(MO,cash_common_stock_issued)
     +                     + CASH_VARIABLES(MO,cash_lease_receipts)
C
               STI_EARNINGS(MO)=(CASH_VARIABLES(MO,cash_closing_balance)
     +                        + CASH_VARIABLES(MO,cash_opening_balance))
     +                         * MONTHLY_RETURN_ON_ST_INVEST(MO)/2.

            ENDDO
            STI_EARNINGS(0) = SUM(STI_EARNINGS(1:))
            LTI_EARNINGS(0) = SUM(LTI_EARNINGS(1:))
            R_STI_INTEREST_INCOME = STI_EARNINGS(0)
            R_LT_INVESTMENTS_EARNINGS = LTI_EARNINGS(0)

            CASH_VARIABLES(:,csh_st_invstmt_incm) = 

     +               + STI_EARNINGS(:)

            CASH_BTL_INTEREST_INCOME =
     +             CASH_VARIABLES(:,csh_st_invstmt_incm)

            CASH_VARIABLES(0,cash_period_change) =
     +                        SUM(CASH_VARIABLES(1:,cash_period_change))
            CASH_VARIABLES(0,cash_std_issued) =
     +                           SUM(CASH_VARIABLES(1:,cash_std_issued))
            CASH_VARIABLES(0,cash_ltd_issued) =
     +                           SUM(CASH_VARIABLES(1:,cash_ltd_issued))
            CASH_VARIABLES(0,external_financing_pfmd) =
     +              SUM(CASH_VARIABLES(1:,external_financing_pfmd))
            CASH_VARIABLES(0,cash_closing_balance) = 
     +                           CASH_VARIABLES(12,cash_closing_balance)
C
C RE CALC 
C
            INCOME_VARIABLES(:,BTL_Monthly_STInvestmet_Income) =
     +                                                   STI_EARNINGS(:)
            INCOME_VARIABLES(:,mty_mdl_lt_nvst_income) =
     +                                                   LTI_EARNINGS(:)
            INCOME_VARIABLES(:,BTL_Monthly_Interest_Income) =
     +              INCOME_VARIABLES(:,BTL_Monthly_STInvestmet_Income)
     +              + INCOME_VARIABLES(:,
     +                                mty_mdl_lt_nvst_income)
     +              + INCOME_VARIABLES(:,
     +                             mt_dbt_file_linvst_income)
     +              + INCOME_VARIABLES(:,
     +                                 monthly_notes_receivable_income) 
            INCOME_VARIABLES(:,mty_totl_lti_income) =
     +              INCOME_VARIABLES(:,
     +                                mty_mdl_lt_nvst_income)
     +              + INCOME_VARIABLES(:,
     +                             mt_dbt_file_linvst_income)

         ENDIF
C
C RECALCULATE LTD ISSUED
C
         IF(CPL_ACTIVE()) THEN
            LONG_TERM_DEBT_ISSUED = 0.
            LTD_ISSUE_YR_INTEREST_BOOKED = 0.
            LTD_ISSUE_YR_INTEREST_PAYMENT = 0.
C
            DO MO = 1, 12
               LTD_BOOKED_ISSUED_INTEREST(MO) = LTDRTE/12. * 
     +                                    (LTD_ISSUED_DISTRIBUTION(MO)
     +                                          + LONG_TERM_DEBT_ISSUED)
               IF(MO+6 <= 12) THEN
                  LTD_PAID_ISSUED_INTEREST(MO+6) =
     +                                    LTD_BOOKED_ISSUED_INTEREST(MO)
                  LTD_ISSUE_YR_INTEREST_PAYMENT =
     +                                  LTD_ISSUE_YR_INTEREST_PAYMENT
     +                                  + LTD_BOOKED_ISSUED_INTEREST(MO)
               ENDIF
               LTD_ISSUE_YR_INTEREST_BOOKED =
     +                                  LTD_ISSUE_YR_INTEREST_BOOKED
     +                                  + LTD_BOOKED_ISSUED_INTEREST(MO)
               LONG_TERM_DEBT_ISSUED = LONG_TERM_DEBT_ISSUED
     +                                 + LTD_ISSUED_DISTRIBUTION(MO)
            ENDDO
         ELSEIF(MORTGAGE_DEBT) THEN
            LONG_TERM_DEBT_ISSUED = 0.
            LTD_ISSUE_YR_INTEREST_BOOKED = 0.
            LTD_ISSUE_YR_INTEREST_PAYMENT = 0.
            LTD_PAID_ISSUED_INTEREST = 0.
            MGT_DEBIT_RETIREMENT = 0.
            LTD_BOOKED_ISSUED_INTEREST = 0.
            PAID_WHEN = 'Q'
            MTGLIFE = MAX(1,INT(LDLIFE))
            IF(PAID_WHEN == 'M') PAY_PERIODS_PER_YR = 12.
            IF(PAID_WHEN == 'S') PAY_PERIODS_PER_YR = 2.
            IF(PAID_WHEN == 'Q') PAY_PERIODS_PER_YR = 4.
            IF(PAID_WHEN == 'A') PAY_PERIODS_PER_YR = 1.
C
            DO MO = 1, 12
               IF(LTD_ISSUED_DISTRIBUTION(MO) == 0.) CYCLE
               LTD_CASH_CARRY_OVER = 0.
               LTD_BAL = LTD_ISSUED_DISTRIBUTION(MO)
               LONG_TERM_DEBT_ISSUED = LONG_TERM_DEBT_ISSUED
     +                                 + LTD_ISSUED_DISTRIBUTION(MO)
               IF(LTDRTE /= 0.) THEN
                  PIBIEN = (LTD_BAL * LTDRTE/PAY_PERIODS_PER_YR)/
     +               (1. - (1./(1. + LTDRTE/PAY_PERIODS_PER_YR)**
     +                               (INT(PAY_PERIODS_PER_YR)*MTGLIFE)))
               ELSE
                  PIBIEN = LTD_BAL/MAX(1.,PAY_PERIODS_PER_YR*MTGLIFE)
               ENDIF
               DO MO1 = MO, 12
                  IF(THIS_IS_A_PAY_MONTH(MO,MO1,PAID_WHEN)
     +                                              .AND. MO1 > MO) THEN
                     LTD_PAID_ISSUED_INTEREST(MO1) =
     +                                     LTD_PAID_ISSUED_INTEREST(MO1)
     +                                     + LTD_CASH_CARRY_OVER

                     LTD_BAL = LTD_BAL - (PIBIEN - LTD_CASH_CARRY_OVER)
                     MGT_DEBIT_RETIREMENT(MO1) = 
     +                                  MGT_DEBIT_RETIREMENT(MO1) 
     +                                  + (PIBIEN - LTD_CASH_CARRY_OVER)

                     LTD_CASH_CARRY_OVER = 0.
                  ENDIF
C
                  LTD_BOOKED_ISSUED_INTEREST(MO1) = 
     +                                  LTD_BOOKED_ISSUED_INTEREST(MO1)  
     +                                   + LTDRTE/12. * LTD_BAL
                  LTD_CASH_CARRY_OVER = LTD_CASH_CARRY_OVER
     +                                  + LTDRTE/12. * LTD_BAL

               ENDDO
            ENDDO
            MGT_DEBIT_RETIREMENT(0) = SUM(MGT_DEBIT_RETIREMENT(1:12))
         ENDIF
         IF(CPL_ACTIVE() .OR. MORTGAGE_DEBT) THEN
            LTD_BOOKED_ISSUED_INTEREST(1:12) =
     +                                  LTD_BOOKED_ISSUED_INTEREST(1:12)
     +                                  + LTD_INTEREST_ADJ(1:12)
            LTD_PAID_ISSUED_INTEREST(1:12) =
     +                                    LTD_PAID_ISSUED_INTEREST(1:12)
     +                                    + LTD_CASH_INTEREST_ADJ(1:12)
C
            LTD_ISSUED_DISTRIBUTION(0) =
     +                                SUM(LTD_ISSUED_DISTRIBUTION(1:12))
            LTD_BOOKED_ISSUED_INTEREST(0) =
     +                             SUM(LTD_BOOKED_ISSUED_INTEREST(1:12))
            LTD_PAID_ISSUED_INTEREST(0) =
     +                               SUM(LTD_PAID_ISSUED_INTEREST(1:12))
C
            LTD_ISSUE_YR_INTEREST_BOOKED = LTD_BOOKED_ISSUED_INTEREST(0)
            LTD_ISSUE_YR_INTEREST_PAYMENT = LTD_PAID_ISSUED_INTEREST(0)
            LONG_TERM_DEBT_ISSUED = LTD_ISSUED_DISTRIBUTION(0)
         ENDIF
C
         CASH_2_LT_INVESTMENTS =
     +                       CASH_VARIABLES(0,cash_change_ltinvestments)
         FUNDS_BALANCE(EOY) = CASH_VARIABLES(12,cash_closing_balance)
         STI_CHANGE_IN_CASH = FUNDS_BALANCE(EOY) - FUNDS_BALANCE(BOY)
         STD_ISSUED = 
     +          BALANCE_SHEET_VARIABLES(12,monthly_short_term_debt_bal)
     +          - BALANCE_SHEET_VARIABLES(0,monthly_short_term_debt_bal)

         R_STD_CASH_INTEREST = CASH_VARIABLES(0,cash_std_interest)
     +                + CASH_VARIABLES(0,Cash_Interest_on_Notes_Payable)
         R_STD_INCOME_INTEREST=INCOME_VARIABLES(0,MonthlySTDInterest)
         R_NON_INCOME_TAXES_ACCRUAL_ADJ =
     +              INCOME_VARIABLES(0,monthly_oth_taxes)
     +              + INCOME_VARIABLES(0,Monthly_Property_Taxes)
     +              + INCOME_VARIABLES(0,Monthly_Operating_Revenue_Tax)
     +              + INCOME_VARIABLES(0,Monthly_State_Tax_on_Capital)
     +              + INCOME_VARIABLES(0,Monthly_Federal_Tax_on_Capital)
     +              + INCOME_VARIABLES(0,
     +                                wvpa_prop_txs_n_pwr_cst)
     +              - CASH_VARIABLES(0,cash_other_taxes)
     +              - CASH_VARIABLES(0,cash_property_taxes)  
     +              - CASH_VARIABLES(0,cash_operating_revenue_tax)  
     +              - CASH_VARIABLES(0,cash_state_on_capital)
     +              - CASH_VARIABLES(0,cash_fed_tax_on_capital)
         R_STATE_TAXES_ACCRUAL_ADJ =
     +             INCOME_VARIABLES(0,mthy_state_atl_incm_tax_pd)
     +             + RPT_STATE_BTL_INCOME_TAX(0_2)
     +             - CASH_VARIABLES(0,cash_st_income_taxes_pd)
         R_FEDERAL_TAXES_ACCRUAL_ADJ =
     +           INCOME_VARIABLES(0,mthy_fed_atl_income_tax_pd)
     +           + RPT_FEDERAL_BTL_INCOME_TAX(0_2)
     +           - CASH_VARIABLES(0,cash_fed_income_taxs_pd)
         IF(CLASS_TYPE == PARENT) THEN
            SUB_FED_TAXES_PAID_2_PARENT =
     +                                  SUB_FED_TAX_PAYMENTS_2_PARENT(0)
            SUB_STATE_TAXES_PAID_2_PARENT =
     +                                SUB_STATE_TAX_PAYMENTS_2_PARENT(0)
         ELSE
            SUB_FED_TAXES_PAID_2_PARENT = 0.
            SUB_STATE_TAXES_PAID_2_PARENT = 0.
         ENDIF
         R_OTHER_TAXES =  INCOME_VARIABLES(0,monthly_oth_taxes) 
     +                    - ANNUAL_VARS(216) ! environmental tax
         R_OP_REV_TAX=INCOME_VARIABLES(0,Monthly_Operating_Revenue_Tax)
C
C CALCULATE PROXIE FOR TAXABLE INCOME
C
         VOID_LOGICAL = MONTHLY_TAXABLE_INCOME(R_YR,CLASS_ID,CLASS_TYPE,
     +                                    ATL_TAXABLE_INCOME,
     +                                    INCOME_VARIABLES,
     +                                    CASH_VARIABLES,
     +                                    TAX_VARIABLES,
     +                                    ANNUAL_VARS,
     +                                    STATE_M1_ADDITIONS,
     +                                    STATE_M1_DEDUCTIONS,
     +                                    FEDERAL_M1_ADDITIONS,
     +                                    FEDERAL_M1_DEDUCTIONS,
     +                                    STATE_BTL_MISC_DEDUCTIONS,
     +                                    FEDERAL_BTL_MISC_DEDUCTIONS,
     +                                    MONTHLY_STATE_TAX_RATE,
     +                                    MONTHLY_FEDERAL_TAX_RATE,
     +                                    STATE_DEF_TAX_DR_FROM_NOLS,
     +                                    FED_DEF_TAX_DR_FROM_NOLS_AMT,
     +                                    STATE_INCOME_TAX_ADJUSTMENT,
     +                                    FEDERAL_INCOME_TAX_ADJUSTMENT,
     +                                    STATE_NOLS_USED_BY_SUBS,
     +                                    STATE_NOLS_GEN_BY_SUBS,
     +                                    PARENT_STATE_TAXES,
     +                                    PARENT_STATE_DR_TAX_FROM_NOLS,
     +                                    SUB_STATE_DR_TAX_FROM_NOLS,
     +                                    SUB_STATE_TAXES_PAID,
     +                                    SUB_BTL_TAXABLE_EXPENSES,
     +                                    SUB_BTL_STATE_TAXES_PAID,
     +                                    SUB_BTL_STATE_TAXABLE_INCOME,
     +                                    SUB_BTL_TAXABLE_OTHER_INCOME,
     +                                    SUB_BTL_TAXABLE_INVEST_INCOME,
     +                                    SUB_BTL_MISC_DEDUCTIONS,
     +                                    SUB_BTL_FEDERAL_TAXES_PAID)
C
C SEND THE CALCULATED MONTHLY TAXES BACK
C
         FUEL_EXPENSE = INCOME_VARIABLES(0,Monthly_Fossil_Fuel)
         VARIABLE_EXPENSE = INCOME_VARIABLES(0,monthly_variable_oandm)
         FIXED_EXPENSE = INCOME_VARIABLES(0,monthly_fixed_oandm)
         CASH_REVENUES_RECEIVED =
     +               CASH_VARIABLES(0,cash_rcd_accts_rcvbl)
         CASH_EXPENSES_PAID =
     +                   CASH_VARIABLES(0,Cash_Paid_on_Accounts_Payable)
         CHANGE_IN_INVESTMENT_PAYABLE(:) =
     +          INCOME_VARIABLES(:,
     +                           mthly_invstmt_earngs_rcvbl)
     +          + INCOME_VARIABLES(:,mty_totl_lti_income)
     +          + INCOME_VARIABLES(:,BTL_Monthly_STInvestmet_Income)
     +          - CASH_VARIABLES(:,cash_investmt_divnd_earngs)
     +          - CASH_VARIABLES(:,cash_invest_interest_ernings)
     +          - CASH_VARIABLES(:,csh_st_invstmt_incm)  ! 112
         R_BTL_CASH_EARNINGS =
     +             CASH_VARIABLES(0,csh_st_invstmt_incm)
     +             + CASH_VARIABLES(0,cash_investmt_divnd_earngs)
     +             + CASH_VARIABLES(0,cash_invest_interest_ernings)
      RETURN
C***********************************************************************
      ENTRY CONSOLIDATED_TAXES_CASH(R_YR,
     +                              CONSOLIDATED_STATE_TAXES,
     +                              CONSOLIDATED_FEDERAL_TAXES,
     +                              CONSOLIDATED_FEDERAL_TAXES_PAID,
     +                              CONSOLIDATED_STATE_TAXES_PAID)
C***********************************************************************
C
C RESET PAYABLE VALUES
C
         CONSOLIDATED_VARIABLES = 0.
         PAYABLE_MONTHLY_VALUES(:,:,-1) =
     +                               TEMP_PAYABLE_MONTHLY_VALUES(:,:,-1)
         DUMMY_CLASS_ID = -1
         AVERAGE_VALUES = 0.
         AVERAGE_VALUES(0) = CONSOLIDATED_FEDERAL_TAXES
         IF(IPALCO()) THEN
            AVERAGE_VALUES(3) = SUM(CONSOLD_FED_INC_TAXES_BOOKED(1:3))
            AVERAGE_VALUES(5) = AVERAGE_VALUES(3)
            AVERAGE_VALUES(8)=1.5*SUM(CONSOLD_FED_INC_TAXES_BOOKED(1:6))
     +                               - AVERAGE_VALUES(3)
     +                               - AVERAGE_VALUES(5)
            AVERAGE_VALUES(12) =
     +                    12.* SUM(CONSOLD_FED_INC_TAXES_BOOKED(1:9))/9.
     +                     - AVERAGE_VALUES(3)
     +                     - AVERAGE_VALUES(5)
     +                     - AVERAGE_VALUES(8)

            CONSOLIDATED_VARIABLES(4,cash_fed_income_taxs_pd) =
     +                                                 AVERAGE_VALUES(3)
            CONSOLIDATED_VARIABLES(6,cash_fed_income_taxs_pd) =
     +                                                 AVERAGE_VALUES(5)
            CONSOLIDATED_VARIABLES(9,cash_fed_income_taxs_pd) =
     +                                                 AVERAGE_VALUES(8)
            CONSOLIDATED_VARIABLES(12,cash_fed_income_taxs_pd) =
     +                                                AVERAGE_VALUES(12)
            TEMP_FEDERAL_INC_TAX_CARRY_OVER(-1) =
     +                                          AVERAGE_VALUES(0)
     +                                          - (AVERAGE_VALUES(3)
     +                                             + AVERAGE_VALUES(5)
     +                                             + AVERAGE_VALUES(8)
     +                                             + AVERAGE_VALUES(12))
            CONSOLIDATED_VARIABLES(1:,cash_fed_income_taxs_pd) =
     +                 CONSOLIDATED_VARIABLES(1:,
     +                                   cash_fed_income_taxs_pd)
     +                  + PAYABLE_MONTHLY_VALUES(1:,
     +                             payment_federal_inc_taxes_paid,-1)
            IF(FEDERAL_INC_TAX_CARRY_OVER(-1) >= 0) THEN
               CONSOLIDATED_VARIABLES(3,cash_fed_income_taxs_pd)=
     +                  CONSOLIDATED_VARIABLES(3,
     +                                   cash_fed_income_taxs_pd)
     +                  + FEDERAL_INC_TAX_CARRY_OVER(-1)
            ELSE
               CONSOLIDATED_VARIABLES(4,cash_fed_income_taxs_pd)=
     +                  CONSOLIDATED_VARIABLES(4,
     +                                   cash_fed_income_taxs_pd)
     +                  + FEDERAL_INC_TAX_CARRY_OVER(-1)
            ENDIF 
            CONSOLIDATED_VARIABLES(0,cash_fed_income_taxs_pd) =
     +            SUM(CONSOLIDATED_VARIABLES(1:,
     +                                  cash_fed_income_taxs_pd))
         ELSE
            IF(EMPIRE()) THEN
               AVERAGE_VALUES(3) = .10*AVERAGE_VALUES(0)
               AVERAGE_VALUES(5) = .15*AVERAGE_VALUES(0)
               AVERAGE_VALUES(8) = .25*AVERAGE_VALUES(0)
               AVERAGE_VALUES(12) = .50*AVERAGE_VALUES(0)
            ELSEIF(KCPL()) THEN
               AVERAGE_VALUES(3) = SUM(INCOME_VARIABLES(1:3,
     +                               mty_tot_fed_incm_tax_pd))
               AVERAGE_VALUES(5) = SUM(INCOME_VARIABLES(4:6,
     +                               mty_tot_fed_incm_tax_pd))
               AVERAGE_VALUES(8) = SUM(INCOME_VARIABLES(7:9,
     +                               mty_tot_fed_incm_tax_pd))
               AVERAGE_VALUES(12) = SUM(INCOME_VARIABLES(10:12,
     +                               mty_tot_fed_incm_tax_pd))
            ELSE
               AVERAGE_VALUES(3) = AVERAGE_VALUES(0)/4.
               AVERAGE_VALUES(5) = AVERAGE_VALUES(0)/4.
               AVERAGE_VALUES(8) = AVERAGE_VALUES(0)/4.
               AVERAGE_VALUES(12) = AVERAGE_VALUES(0)/4.
            ENDIF
            CALL LAG_CASH_VALUES(CONSOLIDATED_VARIABLES(0,
     +                                  cash_fed_income_taxs_pd),
     +                        AVERAGE_VALUES,
     +                        PAYABLE_MONTHLY_VALUES(0,
     +                            payment_federal_inc_taxes_paid,-1),
     +                        payment_federal_inc_taxes_paid,
     +                        DUMMY_CLASS_ID)
         ENDIF
C
C STATE CONSOLIDATED TAX PAYMENTS
C
         AVERAGE_VALUES = 0.
         AVERAGE_VALUES(0) = CONSOLIDATED_STATE_TAXES
         IF(KCPL()) THEN
            AVERAGE_VALUES(3) = SUM(INCOME_VARIABLES(1:3,
     +                             mty_tot_st_incm_tax_pd))
            AVERAGE_VALUES(5) = SUM(INCOME_VARIABLES(4:6,
     +                             mty_tot_st_incm_tax_pd))
            AVERAGE_VALUES(8) = SUM(INCOME_VARIABLES(7:9,
     +                             mty_tot_st_incm_tax_pd))
            AVERAGE_VALUES(12) = SUM(INCOME_VARIABLES(10:12,
     +                             mty_tot_st_incm_tax_pd))
         ELSE
            AVERAGE_VALUES(3) = AVERAGE_VALUES(0)/4.
            AVERAGE_VALUES(5) = AVERAGE_VALUES(0)/4.
            AVERAGE_VALUES(8) = AVERAGE_VALUES(0)/4.
            AVERAGE_VALUES(12) = AVERAGE_VALUES(0)/4.
         ENDIF
         CALL LAG_CASH_VALUES(CONSOLIDATED_VARIABLES(0,
     +                                  cash_st_income_taxes_pd),
     +                        AVERAGE_VALUES,
     +                        PAYABLE_MONTHLY_VALUES(0,
     +                              payment_state_income_taxes_paid,-1),
     +                        payment_state_income_taxes_paid,
     +                        DUMMY_CLASS_ID)
C
C REPLACE THE CALCULATED WITH ACTUAL
C

         CALL GET_ACTUAL_CONSOLID_PAYABLES(R_YR,CONSOLIDATED_VARIABLES)
         CONSOLIDATED_FEDERAL_TAXES_PAID = 
     +          CONSOLIDATED_VARIABLES(0,cash_fed_income_taxs_pd)
         CONSOLIDATED_STATE_TAXES_PAID = 
     +            CONSOLIDATED_VARIABLES(0,cash_st_income_taxes_pd)
C
C CALCULATE THE MONTHLY CONSOLIDATED FEDERAL INCOME TAXES
C
         CALL MONTHLY_TAX_ADJUSTMENTS(INT2(-1),R_YR,
     +                                CON_PROPERTY_TAX_ADJUSTMENT,
     +                                CON_OTHER_TAX_ADJUSTMENT,
     +                                CON_REVENUE_TAX_ADJUSTMENT,
     +                                CON_STATE_M1_ADDITIONS,
     +                                CON_STATE_M1_DEDUCTIONS,
     +                                CON_FEDERAL_M1_ADDITIONS,
     +                                CON_FEDERAL_M1_DEDUCTIONS,
     +                                CON_STATE_BTL_MISC_DEDUCTIONS,
     +                                CON_FEDERAL_BTL_MISC_DEDUCTIONS,
     +                                CON_STATE_INCOME_TAX_ADJUSTMENT,
     +                                CON_FED_INCOME_TAX_ADJUSTMENT)
         DO MO = 0, 12
            CONSOLD_FED_INCOME_B4_DEDUC(MO) = 
     +                               RPT_TAXABLE_INCOME_B4_DEDUCTS(MO)
     +                               + SUB_TAXABLE_INCOME_B4_DEDUCTS(MO)
            CONSOLD_FED_INCOME_AFTR_DEDUC(MO) = 
     +                                CONSOLD_FED_INCOME_B4_DEDUC(MO)
     +                                + CON_FEDERAL_M1_ADDITIONS(MO)
     +                                - CON_FEDERAL_M1_DEDUCTIONS(MO)
     +                                - SUB_STATE_TAXES_PAID(MO)
            CONSOLD_FED_INC_TAXES_BOOKED(MO) = FEDERAL_TAX_RATE *
     +                                 CONSOLD_FED_INCOME_AFTR_DEDUC(MO)
         ENDDO
      RETURN
C***********************************************************************
      ENTRY MONTHLY_LTD_BOOKED_NEW_ISSUES(R_MGT_DEBIT_RETIREMENT,
     +                                  R_LTD_ISSUE_YR_INTEREST_BOOKED,
     +                                  R_LTD_ISSUE_YR_INTEREST_PAYMENT)
C***********************************************************************
C
C
         R_LTD_ISSUE_YR_INTEREST_BOOKED = LTD_BOOKED_ISSUED_INTEREST(0)
         R_LTD_ISSUE_YR_INTEREST_PAYMENT = LTD_PAID_ISSUED_INTEREST(0)
         R_MGT_DEBIT_RETIREMENT = MGT_DEBIT_RETIREMENT(0)
      RETURN
C***********************************************************************
      ENTRY RETURN_MONTNLY_LTD_ISSUES(R_LTD_ISSUED_DISTRIBUTION)
C***********************************************************************
C
         R_LTD_ISSUED_DISTRIBUTION(1:) = LTD_ISSUED_DISTRIBUTION(1:)
      RETURN
C***********************************************************************
      ENTRY MONTHLY_BOY_ELIMINATIONS(ANNUAL_VARS)
C***********************************************************************
         SAVE_ANNUAL_BOY_VARIABLE = ANNUAL_VARS
      RETURN
C***********************************************************************
      ENTRY MONTHLY_EOY_ELIMINATIONS(ANNUAL_VARS)
C***********************************************************************
         SAVE_ANNUAL_EOY_VARIABLE = ANNUAL_VARS
      RETURN
C***********************************************************************
      ENTRY REPORT_MONTHLY_ANALYSIS(R_YR,CLASS_ID,
     +                              R_ASSET_CLASS_NAME,
     +                              CLASS_POS,
     +                              CLASS_TYPE,
     +                              CLASS_LEVEL,
     +                              WRITE_MONTHLY_INFOR,
     +                              DONT_TALLY_THIS_CLASS,
     +                              THIS_IS_REPORTING_CLASS,
     +                              ANNUAL_VARS,
     +                              BOY_ACCUMULATED_DEP,
     +                              BOY_RETAINED_EARNINGS,
     +                              BOY_COMMON_STOCK,
     +                              BOY_LTD_BALANCE,
     +                              BOY_PS_BALANCE,
     +                              INVESTMENTS_BOY,
     +                              NOTES_RECEIVABLE_BOY,
     +                              NOTES_PAYABLE_BOY,
     +                              DEFERRED_PURCHASE_POWER_BOY,
     +                              PENSION_LIABILITY_BOY,
     +                              DEFERRED_GAIN_FROM_SALES_BOY,
     +                              STORM_RESERVE_BALANCE_BOY,
     +                              VACATION_PAY_BALANCE_BOY,
     +                              OTHER_INVESTMENTS_BALANCE_BOY,
     +                              DEFERRED_DEBITS_BOY,
     +                              DEFERRED_TAXES_DR_BOY,
     +                              ASSETS_NEC_BOY,
     +                              ACCOUNTS_RECEIVABLE_BOY,
     +                              ACCUMLATED_BOOK_DEP_BOY,
     +                              CUM_DEF_REVENUES_BOY,
     +                              DEFERRED_TAXES_CR_BOY,
     +                              DEFERRED_ITCS_BOY,
     +                              OTHER_LT_LIABILITIES_BOY,
     +                              ACCOUNTS_PAYABLE_BOY,
     +                              LIABS_NEC_BOY,
     +                              BOY_CWIP,
     +                              BOY_POST_RETIRE_MEDICAL_FUND,
     +                              BOY_POST_RETIRE_MED_PAYABLE,
     +                              BOY_NUC_DECOM_FUND_LIABILITY,
     +                              BOY_CAPITIALZIED_LEASES,
     +                              BOY_NUC_DECOM_FUND_BAL,
     +                              BOY_CLASS_NET_NF_VALUE,
     +                              BOY_INVESTMENT_IN_SUBSIDIARIES,
     +                              CONSOLIDATED_TAX_LIB_ADJUSTMENT,
     +                              BOY_ISSUE_EXP_BAL,
     +                              FUEL_INVENTORY_BOY,
     +                              GAS_STORAGE_BOY,
     +                              MATERIALS_SUPPLIES_BOY,
     +                              BOY_DEBIT_UNAMORT_INTEREST,
     +                              REAQUIRED_DEBT_BOY,
     +                              REGULATORY_LIABS_BOY,
     +                              DEFERRED_CREDITS_BOY,
     +                              GOODWILL_BOY,
     +                              ExecBenefitsBalance_BOY,
     +                              IncentiveCompBalance_BOY)
C***********************************************************************
C
         ASSET_CLASS_NAME = R_ASSET_CLASS_NAME
         PASS_2_ITS_PARENT = .NOT. (CLASS_TYPE == SUBSIDIARY)
         PASS_2_NEXT_LEVEL = CLASS_TYPE == SBU .OR.
     +                       CLASS_TYPE == REGULATED_GROUP .OR.
     +                      (CLASS_TYPE == SUBSIDIARY .AND.
     +                                                PASS_2_ITS_PARENT)
         IF(CLASS_TYPE == SUBSIDIARY) THEN
            CLASS_LINK_2 = -1
         ELSE
            CLASS_LINK_2 = CLASS_LEVEL-1
         ENDIF
         SUB_LINK_2_CLASS = CLASS_LEVEL-1
C
C SAVE CLASS LEVEL VARIABLES
C
         OPENING_CASH_BALANCES(CLASS_POS) =
     +                          CASH_VARIABLES(12,cash_closing_balance) 
C
C PASS RESULTS TO NEXT LEVEL
C

         CALL MONTHLY_EXISTING_ASSET_BS_INFO(R_YR,CLASS_ID,
     +                                       BALANCE_SHEET_VARIABLES)
         CALL MONTHLY_FUTURE_ASSET_BS_INFO(R_YR,CLASS_ID,
     +                                     BALANCE_SHEET_VARIABLES)
         CALL RETURN_MONTHLY_BAL_ADDENDUMS(R_YR,CLASS_ID,
     +                                     BALANCE_SHEET_VARIABLES,
     +                                     TRANSFER_VARIABLES,
     +                                     DEFERRED_GAIN_ADJ)
         CALL MONTHLY_DEFERRED_DEBIT_BS_INFO(R_YR,CLASS_ID,
     +                                       BALANCE_SHEET_VARIABLES)
         NOTES_PAYABLE_MADE = 0.
         NOTES_RECEIVABLE_MADE = 0.
         UNAMORTIZED_INTEREST_BALANCE = 0.
         CALL MONTHLY_NOTES_MADE(R_YR,CLASS_ID,
     +                           NOTES_PAYABLE_MADE,
     +                           NOTES_RECEIVABLE_MADE,
     +                           UNAMORTIZED_INTEREST_BALANCE)
         CALL RETURN_MONTHLY_FASB87_VALUES(BALANCE_SHEET_VARIABLES,
     +                                     FASB87_PENSION_LIAB_ADJ,
     +                                     FASB87_DEFFERED_TAXES_ADJ_DR)
         DO MO = 12, 1, -1
            BALANCE_SHEET_VARIABLES(MO,monthly_oci_value) =
     +                   BALANCE_SHEET_VARIABLES(MO,monthly_oci_value)
     +                   + SUM(OCI_RETIREMENT_MEDICAL_FUND_RETURN(1:MO))
         ENDDO
C
C ADJUSTMENTS FOR NEW SUBSIDIARY ACQUISITIONS
C
         CumulatedBalances(0) = CLASS_GOODWILL_EOY(CLASS_POS)   ! privious EOY value makes this BOY 
         DO MO = 1, 12
            CumulatedBalances(MO) = CumulatedBalances(MO-1)
     +                     + GOODWILL_OF_NEW_SUB_INVESTMENT(MO)
     +                     + TRANSFER_VARIABLES(MO,monthly_net_goodwill)
         ENDDO
         CLASS_GOODWILL_EOY(CLASS_POS) = CumulatedBalances(12)
         BALANCE_SHEET_VARIABLES(0,monthly_net_goodwill) = GOODWILL_BOY
         BALANCE_SHEET_VARIABLES(1:12,monthly_net_goodwill) =
     +                BALANCE_SHEET_VARIABLES(1:12,monthly_net_goodwill)
     +                + CumulatedBalances(1:12)

         DO VARIABLE = 1, BAL_SHEET_VARS
            DO MO = 0, 12

                  IF(VARIABLE == monthly_aro_net_asset_value) THEN
                     BALANCE_SHEET_VARIABLES(MO,VARIABLE) =
     +                   FASB143_MONTHLY_NET_ASSET_VALUE(MO)
     +                   + BALANCE_LINKED_LEVEL(MO,VARIABLE,CLASS_LEVEL)
                  ELSEIF(VARIABLE == monthly_aro_liability_value) THEN
                     BALANCE_SHEET_VARIABLES(MO,VARIABLE) =
     +                   FASB143_MONTHLY_LT_LIABILITY_VALUE(MO)
     +                   + BALANCE_LINKED_LEVEL(MO,VARIABLE,CLASS_LEVEL)
                  ELSEIF(VARIABLE == monthly_net_goodwill) THEN
                     CYCLE
                  ELSE
                     BALANCE_SHEET_VARIABLES(MO,VARIABLE) =
     +                   BALANCE_SHEET_VARIABLES(MO,VARIABLE)
     +                   + BALANCE_LINKED_LEVEL(MO,VARIABLE,CLASS_LEVEL)
                     TRANSFER_VARIABLES(MO,VARIABLE) =
     +                   TRANSFER_VARIABLES(MO,VARIABLE)
     +                   + TRANSFER_LINKED_LEVEL_VARIABLES(MO,VARIABLE,
     +                                                      CLASS_LEVEL)
                  ENDIF

            ENDDO
            BALANCE_LINKED_LEVEL(:,VARIABLE,CLASS_LEVEL) = 0.
            TRANSFER_LINKED_LEVEL_VARIABLES(:,VARIABLE,CLASS_LEVEL) = 0.

         ENDDO
C
C THE FOLLOWING IS A FUDGE OF ACC DEP 1/30/99
C
         MONTHLY_CHANGE_IN_RECEIVABLES = 0.
         MONTHLY_CHANGE_IN_PAYABLES = 0.
         CALL MON_DELTA_RECEIVABLES_PAYABLES(R_YR,CLASS_ID,
     +                                  MONTHLY_CHANGE_IN_RECEIVABLES,
     +                                  MONTHLY_CHANGE_IN_PAYABLES)
         CALL MON_REV_FORC_RECEI_PAYABLES(CLASS_ID,
     +                                  MONTHLY_CHANGE_IN_RECEIVABLES,
     +                                  MONTHLY_CHANGE_IN_PAYABLES)
         CALL MNTHLY_PROD_AR_AP(
     +                                  MONTHLY_CHANGE_IN_RECEIVABLES,
     +                                  MONTHLY_CHANGE_IN_PAYABLES)
C ROLL-UP THE PAYABLE CHANGES

               MONTHLY_CHANGE_IN_PAYABLES(:) =
     +                 MONTHLY_CHANGE_IN_PAYABLES(:)
     +                 +  PAYABLE_LINKED_LOWER_LEVEL(:,CLASS_LEVEL)
     +                 - CASH_VARIABLES(:,Cash_Paid_on_Accounts_Payable)
               MONTHLY_CHANGE_IN_RECEIVABLES(:) =
     +             MONTHLY_CHANGE_IN_RECEIVABLES(:)
     +             +  RECEIVABLES_LINKED_LOWER_LEVEL(:,CLASS_LEVEL)
     +             - CASH_VARIABLES(:,cash_rcd_accts_rcvbl)

         IF(WVPA() .AND. CLASS_TYPE == PARENT) THEN

         ENDIF
         MONTHLY_CHANGE_IN_PAYABLES(0) =
     +                               SUM(MONTHLY_CHANGE_IN_PAYABLES(1:))
         PAYABLE_LINKED_LOWER_LEVEL(:,CLASS_LEVEL) = 0.
         RECEIVABLES_LINKED_LOWER_LEVEL(:,CLASS_LEVEL) = 0.
         DEFERRED_GAIN_ROLLUP = 0.
         DO MO = 0, 12
            IF(MO == 0) THEN
C
               BALANCE_SHEET_VARIABLES(MO,monthly_ltd_current_amount) =
     +                                        CURRENT_PORTION_OF_LTD(MO)
               FED_TAXES_PAYABLE_2_PARENT(MO) = 0.
               BALANCE_SHEET_VARIABLES(MO,
     +                               monthly_unamort_interest_bal) =
     +                                 BOY_DEBIT_UNAMORT_INTEREST

               BALANCE_SHEET_VARIABLES(MO,
     +                              mthly_unamortized_issue_exp_bal) =
     +                                                 BOY_ISSUE_EXP_BAL
               BALANCE_SHEET_VARIABLES(MO,
     +                                  monthly_subsidiary_investment) =
     +                                    BOY_INVESTMENT_IN_SUBSIDIARIES
               BALANCE_SHEET_VARIABLES(MO,monthly_capitalized_leases) =
     +                                           BOY_CAPITIALZIED_LEASES
               BALANCE_SHEET_VARIABLES(MO,
     +                                monthly_nclr_dcms_trust_bal) =
     +                                            BOY_NUC_DECOM_FUND_BAL
               BALANCE_SHEET_VARIABLES(MO,monthly_net_nuclear_fuel) =
     +                                            BOY_CLASS_NET_NF_VALUE
               BALANCE_SHEET_VARIABLES(MO,mtly_cwip) = BOY_CWIP
               BALANCE_SHEET_VARIABLES(0,
     +                               monthly_accum_depreciation) =
     +                                           ACCUMLATED_BOOK_DEP_BOY
               BALANCE_SHEET_VARIABLES(0,monthly_deferred_revenues) =
     +                                              CUM_DEF_REVENUES_BOY
               BALANCE_SHEET_VARIABLES(0,monthly_net_dfrd_debits) =
     +                                               DEFERRED_DEBITS_BOY
               BALANCE_SHEET_VARIABLES(0,
     +                               mthly_deferred_income_taxes_dr) =
     +                                             DEFERRED_TAXES_DR_BOY
               BALANCE_SHEET_VARIABLES(0,monthly_current_assets) =
     +                                                    ASSETS_NEC_BOY
               BALANCE_SHEET_VARIABLES(0,
     +                                monthly_accounts_receivable_bal) =
     +                                           ACCOUNTS_RECEIVABLE_BOY
               BALANCE_SHEET_VARIABLES(0,monthly_deferred_fuel_bal) =
     +               ANNUAL_VARS(392)
     +               + INCOME_VARIABLES(0,MonthlyDeferredFuelExpense)

               BALANCE_SHEET_VARIABLES(0,monthly_retained_earnings_bal)=
     +                                             BOY_RETAINED_EARNINGS
C
               BALANCE_SHEET_VARIABLES(0,monthly_common_stock) =
     +                                                  BOY_COMMON_STOCK
C
               BALANCE_SHEET_VARIABLES(0,monthly_long_term_debt) =
     +                                                   BOY_LTD_BALANCE
C
               BALANCE_SHEET_VARIABLES(0,monthly_preferred_stock) =
     +                                                    BOY_PS_BALANCE
C
               BALANCE_SHEET_VARIABLES(0,monthly_fuel_inventory_bal) =
     +                                                FUEL_INVENTORY_BOY

               BALANCE_SHEET_VARIABLES(0,monthly_gas_in_storage_bal) =
     +                                                   GAS_STORAGE_BOY
               BALANCE_SHEET_VARIABLES(0,
     +                                 monthly_matrls_supplies_bal) =
     +                                            MATERIALS_SUPPLIES_BOY
               BALANCE_SHEET_VARIABLES(0,monthly_notes_receivable) =
     +                                              NOTES_RECEIVABLE_BOY
               BALANCE_SHEET_VARIABLES(0,monthly_notes_payable) =
     +                                                 NOTES_PAYABLE_BOY
               BALANCE_SHEET_VARIABLES(0,
     +                             mthly_deferred_purchse_powr_bal) =
     +                                       DEFERRED_PURCHASE_POWER_BOY
               BALANCE_SHEET_VARIABLES(0,monthly_accrued_pension) =
     +                                             PENSION_LIABILITY_BOY
               DEFERRED_GAIN_ROLLUP = DEFERRED_GAIN_FROM_SALES_BOY
     +               - BALANCE_SHEET_VARIABLES(0,monthly_deferred_gains)
               BALANCE_SHEET_VARIABLES(0,monthly_deferred_gains) =
     +                                      DEFERRED_GAIN_FROM_SALES_BOY
               BALANCE_SHEET_VARIABLES(0,Monthly_Strom_Reserve) =
     +                                         STORM_RESERVE_BALANCE_BOY
               BALANCE_SHEET_VARIABLES(0,MonthlyExecBenefitsReserve) =
     +                                           ExecBenefitsBalance_BOY
               BALANCE_SHEET_VARIABLES(0,MonthlyIncentiveCompReserve) =
     +                                          IncentiveCompBalance_BOY
               BALANCE_SHEET_VARIABLES(0,mty_accrued_vaca_pay) =
     +                                          VACATION_PAY_BALANCE_BOY
               BALANCE_SHEET_VARIABLES(0,monthly_other_investments) =
     +                                     OTHER_INVESTMENTS_BALANCE_BOY
               BALANCE_SHEET_VARIABLES(0,mty_dfrd_income_taxes)=
     +                                             DEFERRED_TAXES_CR_BOY
               BALANCE_SHEET_VARIABLES(0,mty_dfrd_itc_credt) =
     +                                                 DEFERRED_ITCS_BOY
               BALANCE_SHEET_VARIABLES(0,
     +                            monthly_other_long_term_liabs) =
     +                                          OTHER_LT_LIABILITIES_BOY
               BALANCE_SHEET_VARIABLES(0,monthly_accounts_payable_bal) =
     +                                              ACCOUNTS_PAYABLE_BOY
               BALANCE_SHEET_VARIABLES(0,monthly_cica) = CIAC_BALANCE(0)

               BALANCE_SHEET_VARIABLES(0,monthly_liabilities_nec) =
     +                                    LIABS_NEC_BOY
      BALANCE_SHEET_VARIABLES(0,mthly_nucl_decommiss_liability) =  ! 37
     +                                  BOY_NUC_DECOM_FUND_LIABILITY
               BALANCE_SHEET_VARIABLES(0,
     +                             mtly_post_retre_med_payable) =  ! 43
     +                                       BOY_POST_RETIRE_MED_PAYABLE
               BALANCE_SHEET_VARIABLES(0,
     +                               monthly_post_retire_med_fnd) =  ! 11
     +                                      BOY_POST_RETIRE_MEDICAL_FUND
               BALANCE_SHEET_VARIABLES(MO,
     +                             monthly_debt_lti) =  ! 24
     +                                                   INVESTMENTS_BOY
               BALANCE_SHEET_VARIABLES(MO,
     +                                  monthly_lti) =  ! 15
     +                    BALANCE_SHEET_VARIABLES(MO,
     +                               monthly_cash_lti)  ! 23
     +                    + BALANCE_SHEET_VARIABLES(MO,
     +                               monthly_debt_lti)  ! 24
            ELSE
C
C 9/5/00 NEED TO ADD ACQUISTION OF NEW SUB.

                  BALANCE_SHEET_VARIABLES(MO,
     +                                     monthly_ltd_current_amount) =
     +                                        CURRENT_PORTION_OF_LTD(MO)
                  BALANCE_SHEET_VARIABLES(MO,
     +                               monthly_unamort_interest_bal) =
     +               BALANCE_SHEET_VARIABLES(MO-1,
     +                                 monthly_unamort_interest_bal)

     +               + INCOME_VARIABLES(MO,
     +                  mt_dbt_fl_chg_intrst_exp_amrt)
                  BALANCE_SHEET_VARIABLES(MO,
     +                              mthly_unamortized_issue_exp_bal) = ! 77
     +               BALANCE_SHEET_VARIABLES(MO-1,
     +                                mthly_unamortized_issue_exp_bal) ! 77
     +               + CASH_VARIABLES(MO,cash_common_issue_expense)
     +  - INCOME_VARIABLES(MO,mthy_common_stock_issue_amort)
     +               + INCOME_VARIABLES(MO,
     +     mt_chg_dbt_fl_issu_exp_amrt)
                  BALANCE_SHEET_VARIABLES(MO,
     +                                  monthly_subsidiary_investment) =
     +               BALANCE_SHEET_VARIABLES(MO-1,
     +                                    monthly_subsidiary_investment)
     +               + INCOME_VARIABLES(MO,Monthly_Subsidiary_Income)
     +               - CASH_VARIABLES(MO,cash_subsidiary_dividends)
     +               + CASH_VARIABLES(MO,cash_equity_to_subsidiaries)
                 BALANCE_SHEET_VARIABLES(MO,monthly_net_nuclear_fuel) =
     +            BALANCE_SHEET_VARIABLES(MO-1,monthly_net_nuclear_fuel)
     +            + CASH_VARIABLES(MO,cash_nclr_fl_fabrication)
     +            - INCOME_VARIABLES(MO,Monthly_Nuclear_Fuel_Expense)
     +            + ANNUAL_VARS(298) /12.
C
                  BALANCE_SHEET_VARIABLES(MO,
     +                                    monthly_capitalized_leases) =
     +                  BALANCE_SHEET_VARIABLES(MO-1,
     +                                      monthly_capitalized_leases)
     +                  + CASH_VARIABLES(MO,cash_lease_receipts)
     +                  - INCOME_VARIABLES(MO,
     +                               Mthly_Lease_Amort_xpns)
     +                  - INCOME_VARIABLES(MO,
     +                                  Monthly_Lease_BTL_Amort_Expense)

                  BALANCE_SHEET_VARIABLES(MO,
     +                                monthly_nclr_dcms_trust_bal) =
     +                BALANCE_SHEET_VARIABLES(MO-1,
     +                                  monthly_nclr_dcms_trust_bal)
     +                + CASH_VARIABLES(MO,cash_nuc_decommissioning_fund) 
     +                + MONTHLY_NUC_DECOM_FUND_NET_OF_TAX_RETURN(MO)

                  BALANCE_SHEET_VARIABLES(MO,
     +                                      monthly_deferred_revenues) =
     +                       BALANCE_SHEET_VARIABLES(MO-1,
     +                                        monthly_deferred_revenues)
     +                       + INCOME_VARIABLES(MO,DeferredRevenues)
     +                       - INCOME_VARIABLES(MO,
     +                            Mthly_dfrd_rev_Amortization)
     +                       + INCOME_VARIABLES(MO,
     +                             mty_wvpa_acrd_mbr_rvnues)
     +                       + CASH_VARIABLES(MO,
     +                              wvpa_cash_2_accrd_mbr_revnue)
                  IF(SALT_RIVER_PROJECT()) THEN
      BALANCE_SHEET_VARIABLES(MO,mthly_nucl_decommiss_liability) =  ! 37
     + BALANCE_SHEET_VARIABLES(MO-1,mthly_nucl_decommiss_liability)
     +                    + MONTHLY_OCI_NUC_DECOM_FUND_RETURN(MO)
                  ELSE
                     BALANCE_SHEET_VARIABLES(MO,
     + mthly_nucl_decommiss_liability) =  ! 37
     +                BALANCE_SHEET_VARIABLES(MO-1,
     +   mthly_nucl_decommiss_liability)
     +    + INCOME_VARIABLES(MO,MonthlyDOEDecommissioning)
     +     + MONTHLY_NUC_DECOM_FUND_NET_OF_TAX_RETURN(MO)

                  ENDIF
                  BALANCE_SHEET_VARIABLES(MO,
     +                             mtly_post_retre_med_payable) =  ! 43
     +                BALANCE_SHEET_VARIABLES(MO-1,
     +                               mtly_post_retre_med_payable)
     +                + CASH_VARIABLES(MO,Cash_Post_Retirement_Payments)
     +                - CASH_VARIABLES(MO,Cash_Retiree_Medical_Payments)
                  BALANCE_SHEET_VARIABLES(MO,
     +                               monthly_post_retire_med_fnd) =  ! 11
     +                BALANCE_SHEET_VARIABLES(MO-1,
     +                                 monthly_post_retire_med_fnd)
     +             + CASH_VARIABLES(MO,Cash_Post_Retirement_Payments)
     +             + CASH_VARIABLES(MO,csh_2_post_retiremt_paymentss)
     +             - CASH_VARIABLES(MO,Cash_Retiree_Medical_Payments)
     +             + OCI_RETIREMENT_MEDICAL_FUND_RETURN(MO)
               IF(SALT_RIVER_PROJECT() 
     +                      .OR. RETAIN_POST_RETIREMENT_EARNINGS()) THEN   ! ADD A RETAIN EARNINGS SWITCH HERE
                  BALANCE_SHEET_VARIABLES(MO,
     +                               monthly_post_retire_med_fnd) =  ! 11
     +                BALANCE_SHEET_VARIABLES(MO,
     +                                 monthly_post_retire_med_fnd)
     +                + RETIREMENT_MEDICAL_FUND_RETURN(MO)
               ENDIF

               BALANCE_SHEET_VARIABLES(MO,
     +                             monthly_debt_lti) =  ! 24
     +                 BALANCE_SHEET_VARIABLES(MO-1,
     +                             monthly_debt_lti)
     +                 + CASH_VARIABLES(MO,cash_change_debt_investments)
               BALANCE_SHEET_VARIABLES(MO,
     +                                  monthly_lti) =  ! 15
     +                    BALANCE_SHEET_VARIABLES(MO,
     +                               monthly_cash_lti)  ! 23
     +                    + BALANCE_SHEET_VARIABLES(MO,
     +                               monthly_debt_lti)  ! 24
                  IF(BANGOR()) THEN
                     BALANCE_SHEET_VARIABLES(MO,
     +                               mthly_deferred_income_taxes_dr) =
     +                  BALANCE_SHEET_VARIABLES(MO-1,
     +                                 mthly_deferred_income_taxes_dr)
     +                  + FASB87_DEFFERED_TAXES_ADJ_DR(MO)
                     BALANCE_SHEET_VARIABLES(MO,
     +                                  mty_dfrd_income_taxes) =
     +                  BALANCE_SHEET_VARIABLES(MO-1,
     +                                    mty_dfrd_income_taxes)
     +                  + INCOME_VARIABLES(MO,
     +                                  Monthly_Income_Tax_Deferrals_Cr)
     +                  + INCOME_VARIABLES(MO,
     +                                 BTL_Monthly_Income_Tax_Deferrals)
     +                  + INCOME_VARIABLES(MO,
     +                                  Monthly_Income_Tax_Deferrals_Dr)
                  ELSE
                     BALANCE_SHEET_VARIABLES(MO,
     +                               mthly_deferred_income_taxes_dr) =
     +                  BALANCE_SHEET_VARIABLES(MO-1,
     +                                 mthly_deferred_income_taxes_dr)
     +                  - INCOME_VARIABLES(MO,
     +                                  Monthly_Income_Tax_Deferrals_Dr)
     +                  - INCOME_VARIABLES(MO,BTLMonthlyDeferralTaxDr)
     +                  + FASB87_DEFFERED_TAXES_ADJ_DR(MO)
                     BALANCE_SHEET_VARIABLES(MO,
     +                                  mty_dfrd_income_taxes) =
     +                  BALANCE_SHEET_VARIABLES(MO-1,
     +                                    mty_dfrd_income_taxes)
     +                  + INCOME_VARIABLES(MO,
     +                                  Monthly_Income_Tax_Deferrals_Cr)
     +                  + INCOME_VARIABLES(MO,BTLMonthlyDeferralTaxCr)
                  ENDIF
                  BALANCE_SHEET_VARIABLES(MO,mtly_cwip) =
     +                        BALANCE_SHEET_VARIABLES(MO-1,mtly_cwip)
     +                      + INCOME_VARIABLES(MO,Monthly_AFUDC_Total)
     +                      + CASH_VARIABLES(MO,cash_plant_construction)
     +                      - BALANCE_SHEET_VARIABLES(MO,
     +                                       monthly_afudc_capitalized)
     +                      - BALANCE_SHEET_VARIABLES(MO,
     +                                       monthly_plant_capitalized)
                  BALANCE_SHEET_VARIABLES(MO,
     +                                    monthly_net_dfrd_debits) =
     +               BALANCE_SHEET_VARIABLES(MO-1,
     +                                      monthly_net_dfrd_debits)
     +               + CASH_VARIABLES(MO,cash_deferred_expenses)
     +               - CHANGE_IN_DEFERRED_DEBIT_BAL(MO)
     +               - INCOME_VARIABLES(MO,Monthly_Issue_Expense_Amorts)
     +               + CASH_VARIABLES(MO,cash_ltd_ps_issue_expense)
     +               + CASH_VARIABLES(MO,cash_common_issue_expense)
     +               + GOODWILL_OF_NEW_SUB_INVESTMENT(MO)               !Goodwill
     +               + TRANSFER_VARIABLES(MO,monthly_net_goodwill)      !Goodwill
                  IF(CLASS_TYPE == PARENT) THEN
                     BALANCE_SHEET_VARIABLES(MO,
     +                            monthly_other_long_term_liabs) =
     +                        BALANCE_SHEET_VARIABLES(MO-1,
     +                              monthly_other_long_term_liabs)
     +                 - CASH_VARIABLES(MO,Cash_Leased_Nuclear_Fuel)! - NUCLEAR_FUEL_LEASE_PAYMENTS
     +                     + CASH_VARIABLES(MO,cash_lease_receipts)     ! + CAPITIALIZED_LEASE_ADDITIONS
     +                     - CASH_VARIABLES(MO,CashLeaseExpense)      ! - ATL_LEASE_PAYMENTS
     +                     - CASH_VARIABLES(MO,Cash_BTL_Lease_Cash)     ! - BTL_LEASE_PAYMENTS

                  ELSE
                     BALANCE_SHEET_VARIABLES(MO,
     +                            monthly_other_long_term_liabs) =
     +                        BALANCE_SHEET_VARIABLES(MO-1,
     +                              monthly_other_long_term_liabs)
     +                     - CASH_VARIABLES(MO,Cash_Leased_Nuclear_Fuel)! - NUCLEAR_FUEL_LEASE_PAYMENTS
     +                     + CASH_VARIABLES(MO,cash_lease_receipts)     ! + CAPITIALIZED_LEASE_ADDITIONS
     +                     - CASH_VARIABLES(MO,CashLeaseExpense)      ! - ATL_LEASE_PAYMENTS
     +                     - CASH_VARIABLES(MO,Cash_BTL_Lease_Cash)     ! - BTL_LEASE_PAYMENTS
     +                     + CONSOLIDATED_TAX_LIB_ADJUSTMENT/12.
                  ENDIF

                  IF(CLASS_TYPE == PARENT) THEN
                     BALANCE_SHEET_VARIABLES(MO,
     +                                    monthly_accounts_payable_bal)=
     +                  BALANCE_SHEET_VARIABLES(MO-1,
     +                                     monthly_accounts_payable_bal)
C CHANGE IS EXPENSES PAYABLE
     +                  + MONTHLY_CHANGE_IN_PAYABLES(MO)
C STD INTEREST
     +                  + INCOME_VARIABLES(MO,MonthlySTDInterest) 
     +                  - CASH_VARIABLES(MO,cash_std_interest)
C LTD INTEREST
     +                 +INCOME_VARIABLES(MO,Monthly_LTD_Booked_Interest)
     +                  - CASH_VARIABLES(MO,cash_ltd_interest)
C COMMON DIVIDENDS
     +                  + INCOME_VARIABLES(MO,Monthly Common Dividends)
     +                  - CASH_VARIABLES(MO,cash_common_dividends)
C PS DIVIDENDS
     +               + INCOME_VARIABLES(MO,Monthly_Booked_PS_Dividends)
     +               - CASH_VARIABLES(MO,cash_ps_dividends)
C NOTES PAYABLE INTEREST
     +               + INCOME_VARIABLES(MO,
     +                                mty_intst_on_nts_pyble)
     +               - CASH_VARIABLES(MO,Cash_Interest_on_Notes_Payable)
C NON_INCOME_TAXES_ACCRUAL_ADJ
     +               + INCOME_VARIABLES(MO,monthly_oth_taxes)
     +               + INCOME_VARIABLES(MO,Monthly_Property_Taxes)
     +               + INCOME_VARIABLES(MO,
     +                                    Monthly_Operating_Revenue_Tax)
     +               + INCOME_VARIABLES(MO,Monthly_State_Tax_on_Capital)
     +               + INCOME_VARIABLES(MO,
     +                                   Monthly_Federal_Tax_on_Capital)
     +               + INCOME_VARIABLES(MO,
     +                                wvpa_prop_txs_n_pwr_cst)
     +               - CASH_VARIABLES(MO,cash_operating_revenue_tax)  
     +               - CASH_VARIABLES(MO,cash_other_taxes)  
     +               - CASH_VARIABLES(MO,cash_property_taxes)  
     +               - CASH_VARIABLES(MO,cash_state_on_capital)  
     +               - CASH_VARIABLES(MO,cash_fed_tax_on_capital)
C R_STATE_TAXES_ACCRUAL_ADJ
     +               + INCOME_VARIABLES(MO,
     +                              mty_tot_st_incm_tax_pd)
     +               - CASH_VARIABLES(MO,cash_st_income_taxes_pd)
C R_FEDERAL_TAXES_ACCRUAL_ADJ
     +               + INCOME_VARIABLES(MO,
     +                                mty_tot_fed_incm_tax_pd)
     +               + CASH_VARIABLES(MO,
     +                               cash_subsdry_fed_taxes_pd)
     +               - CONSOLIDATED_VARIABLES(MO,
     +                                   cash_fed_income_taxs_pd)
C WVPA ADDITIONS
     +               + WVPA_OFF_SYSTEM_SALES_PAYABLE(MO)
     +               + WVPA_INTERNAL_POWER_COSTS_PAYABLE(MO)
                  FED_TAXES_PAYABLE_2_PARENT(MO) = 0.
               ELSE
                  BALANCE_SHEET_VARIABLES(MO,
     +                                    monthly_accounts_payable_bal)=
     +               BALANCE_SHEET_VARIABLES(MO-1,
     +                                     monthly_accounts_payable_bal)
C CHANGE IS EXPENSES PAYABLE
     +               + MONTHLY_CHANGE_IN_PAYABLES(MO)
C STD INTEREST
     +               + INCOME_VARIABLES(MO,MonthlySTDInterest) 
     +               - CASH_VARIABLES(MO,cash_std_interest)
C LTD INTEREST
     +               + INCOME_VARIABLES(MO,Monthly_LTD_Booked_Interest)
     +               - CASH_VARIABLES(MO,cash_ltd_interest)
C COMMON DIVIDENDS
     +               + INCOME_VARIABLES(MO,Monthly Common Dividends)
     +               - CASH_VARIABLES(MO,cash_common_dividends)
C PS DIVIDENDS
     +               + INCOME_VARIABLES(MO,Monthly_Booked_PS_Dividends)
     +               - CASH_VARIABLES(MO,cash_ps_dividends)
C NOTES PAYABLE INTEREST
     +               + INCOME_VARIABLES(MO,
     +                                mty_intst_on_nts_pyble)
     +               - CASH_VARIABLES(MO,Cash_Interest_on_Notes_Payable)
C NON_INCOME_TAXES_ACCRUAL_ADJ
     +               + INCOME_VARIABLES(MO,monthly_oth_taxes)
     +               + INCOME_VARIABLES(MO,Monthly_Property_Taxes)
     +               + INCOME_VARIABLES(MO,
     +                                    Monthly_Operating_Revenue_Tax)
     +               + INCOME_VARIABLES(MO,Monthly_State_Tax_on_Capital)
     +               + INCOME_VARIABLES(MO,
     +                                   Monthly_Federal_Tax_on_Capital)
     +               + INCOME_VARIABLES(MO,
     +                                wvpa_prop_txs_n_pwr_cst)
     +               - CASH_VARIABLES(MO,cash_operating_revenue_tax)  
     +               - CASH_VARIABLES(MO,cash_other_taxes)  
     +               - CASH_VARIABLES(MO,cash_property_taxes)  
     +               - CASH_VARIABLES(MO,cash_state_on_capital)  
     +               - CASH_VARIABLES(MO,cash_fed_tax_on_capital)
C R_STATE_TAXES_ACCRUAL_ADJ
     +               + INCOME_VARIABLES(MO,
     +                              mty_tot_st_incm_tax_pd)
     +               - CASH_VARIABLES(MO,cash_st_income_taxes_pd)
C R_FEDERAL_TAXES_ACCRUAL_ADJ
     +               + INCOME_VARIABLES(MO,
     +                                mty_tot_fed_incm_tax_pd)
     +               - CASH_VARIABLES(MO,cash_fed_income_taxs_pd)
C WVPA ADDITIONS
     +               + WVPA_OFF_SYSTEM_SALES_PAYABLE(MO)
     +               + WVPA_INTERNAL_POWER_COSTS_PAYABLE(MO)

                  FED_TAXES_PAYABLE_2_PARENT(MO) =
     +               INCOME_VARIABLES(MO,
     +                                mty_tot_fed_incm_tax_pd)
     +               - CASH_VARIABLES(MO,cash_fed_income_taxs_pd)
               ENDIF
C

C
               BALANCE_SHEET_VARIABLES(MO,
     +                                monthly_accounts_receivable_bal) =
     +                BALANCE_SHEET_VARIABLES(MO-1,
     +                                  monthly_accounts_receivable_bal)
     +                + MONTHLY_CHANGE_IN_RECEIVABLES(MO)
     +                + INCOME_VARIABLES(MO,UnbilledRevenues)
     +                + INCOME_VARIABLES(MO,
     +                           mthly_invstmt_earngs_rcvbl)
     +                + INCOME_VARIABLES(MO,
     +                                   BTL_Monthly_STInvestmet_Income)
     +                + INCOME_VARIABLES(MO,
     +                                mty_mdl_lt_nvst_income)
     +                + INCOME_VARIABLES(MO,
     +                             mt_dbt_file_linvst_income)
     +                + INCOME_VARIABLES(MO,
     +                                  monthly_notes_receivable_income) 
     +                - CASH_VARIABLES(MO,
     +                                cash_investmt_divnd_earngs)
     +                - CASH_VARIABLES(MO,
     +                                cash_invest_interest_ernings)
     +                - CASH_VARIABLES(MO,
     +                                csh_st_invstmt_incm)  ! 112
C WVPA RECEIVABLES
     +                + WVPA_OFF_SYSTEM_SALES_RECEIVABLE(MO)
C
               BALANCE_SHEET_VARIABLES(MO,monthly_cica)=CIAC_BALANCE(MO)
               BALANCE_SHEET_VARIABLES(MO,monthly_liabilities_nec) =
     +                                                     LIABS_NEC(MO)
               BALANCE_SHEET_VARIABLES(MO,monthly_current_assets) =
     +                                                    ASSETS_NEC(MO)
               BALANCE_SHEET_VARIABLES(MO,mty_dfrd_itc_credt) =
     +                   BALANCE_SHEET_VARIABLES(MO-1,
     +                                      mty_dfrd_itc_credt)
     +                   + INCOME_VARIABLES(MO,Monthly_ITC_Amortization)
               BALANCE_SHEET_VARIABLES(MO,monthly_other_investments) =
     +           BALANCE_SHEET_VARIABLES(MO-1,monthly_other_investments)
     +                 + INCOME_VARIABLES(MO,MonthlyMarkToMarket)
     +                 + INCOME_VARIABLES(MO,MonthlyEarningNonCompany)
     +                 + CASH_VARIABLES(MO,cash_net_investments)
               BALANCE_SHEET_VARIABLES(MO,monthly_deferred_fuel_bal) =
     +           BALANCE_SHEET_VARIABLES(MO-1,monthly_deferred_fuel_bal)
     +           - INCOME_VARIABLES(MO,MonthlyDeferredFuelExpense)
C
               BALANCE_SHEET_VARIABLES(MO,
     +                               monthly_accum_depreciation) =
     +            + BALANCE_SHEET_VARIABLES(MO-1,
     +                                 monthly_accum_depreciation)
     +            - BALANCE_SHEET_VARIABLES(MO,monthly_retirements)
     +            + INCOME_VARIABLES(MO,Monthly_Book_Depreciation)
     +            + BALANCE_SHEET_VARIABLES(MO,
     +                                      monthly_acc_dep_adjs)
     +            - CASH_VARIABLES(MO,cash_net_salvage)
     +            + BALANCE_SHEET_DEACTIVE_VARS(MO,
     +                                 monthly_accum_depreciation,
     +                                                      CLASS_LEVEL)
C
               BALANCE_SHEET_VARIABLES(MO,monthly_fuel_inventory_bal) =
     +               BALANCE_SHEET_VARIABLES(MO-1,
     +                                       monthly_fuel_inventory_bal)
     +               + CASH_VARIABLES(MO,cash_fuel_inventory_chg)
     +               - INCOME_VARIABLES(MO,monthly_fuel_inventory_amort)
C
               BALANCE_SHEET_VARIABLES(MO,monthly_gas_in_storage_bal) =
     +                  BALANCE_SHEET_VARIABLES(MO-1,
     +                                       monthly_gas_in_storage_bal)
     +                  + CASH_VARIABLES(MO,
     +                              cash_gas_strg_inventry_addtn)
     +                  - INCOME_VARIABLES(MO,monthly_gas_storage_amort) 
C
               BALANCE_SHEET_VARIABLES(MO,
     +                                 monthly_matrls_supplies_bal) =
     +                   BALANCE_SHEET_VARIABLES(MO-1,
     +                                   monthly_matrls_supplies_bal)
     +                   + CASH_VARIABLES(MO,
     +                                class_matrls_inventry_addition)
     +                   - INCOME_VARIABLES(MO,
     +                                mnth_mtl_and_supplys_amort)
C
               BALANCE_SHEET_VARIABLES(MO,
     +                                  monthly_retained_earnings_bal) =
     +                BALANCE_SHEET_VARIABLES(MO-1,
     +                                    monthly_retained_earnings_bal)
     +                + INCOME_VARIABLES(MO,Monthly RETAINED EARNINGS)
C
               BALANCE_SHEET_VARIABLES(MO,monthly_common_stock) =
     +                BALANCE_SHEET_VARIABLES(MO-1,monthly_common_stock)
     +                + CASH_VARIABLES(MO,cash_common_stock_issued)
     +                - CASH_VARIABLES(MO,cash_common_stock_buyback)
C
               BALANCE_SHEET_VARIABLES(MO,monthly_preferred_stock) =
     +             BALANCE_SHEET_VARIABLES(MO-1,monthly_preferred_stock)
     +             + CASH_VARIABLES(MO,cash_ps_issued)
     +             - CASH_VARIABLES(MO,cash_ps_retirements)
     +             - INCOME_VARIABLES(MO,Monthly_PS_Premium_Amort)
C
               BALANCE_SHEET_VARIABLES(MO,monthly_long_term_debt) =
     +              BALANCE_SHEET_VARIABLES(MO-1,monthly_long_term_debt)
     +              + CASH_VARIABLES(MO,cash_ltd_issued)
     +              - CASH_VARIABLES(MO,cash_ltd_retirements)
     +              - INCOME_VARIABLES(MO,Monthly_LTD_Premium_Amort)
C
               BALANCE_SHEET_VARIABLES(MO,monthly_notes_receivable) =
     +            BALANCE_SHEET_VARIABLES(MO-1,monthly_notes_receivable)
     +            + NOTES_RECEIVABLE_MADE(MO)
     +            - CASH_VARIABLES(MO,
     +                              cash_fm_repaid_issued_notes)
               BALANCE_SHEET_VARIABLES(MO,monthly_notes_payable) =
     +               BALANCE_SHEET_VARIABLES(MO-1,monthly_notes_payable)
     +               + NOTES_PAYABLE_MADE(MO)
     +               - CASH_VARIABLES(MO,csh_4_redeeming_notes_owed)
               BALANCE_SHEET_VARIABLES(MO,monthly_accrued_pension) =
     +            BALANCE_SHEET_VARIABLES(MO-1,monthly_accrued_pension)
     +            + INCOME_VARIABLES(MO,MonthlyPensionExpense)
     +            - CASH_VARIABLES(MO,csh_unfundd_retrmt_pymts)
     +            + FASB87_PENSION_LIAB_ADJ(MO)
     +            + CASH_VARIABLES(MO,noncash_pension_in_capx)
               BALANCE_SHEET_VARIABLES(MO,Monthly_Strom_Reserve) =
     +               BALANCE_SHEET_VARIABLES(MO-1,Monthly_Strom_Reserve)
     +               + INCOME_VARIABLES(MO,MonthlySTORMExpense)
     +               - CASH_VARIABLES(MO,Cash_Storm_Payments)
               BALANCE_SHEET_VARIABLES(MO,MonthlyExecBenefitsReserve) =
     +               BALANCE_SHEET_VARIABLES(MO-1,
     +                                       MonthlyExecBenefitsReserve) 
     +               + INCOME_VARIABLES(MO,MonthlyExecBenefits)
     +               - CASH_VARIABLES(MO,CashExecBenefits)
               BALANCE_SHEET_VARIABLES(MO,MonthlyIncentiveCompReserve) =
     +                BALANCE_SHEET_VARIABLES(MO-1,
     +                                      MonthlyIncentiveCompReserve) 
     +                + INCOME_VARIABLES(MO,MonthlyIncentiveComp)
     +                - CASH_VARIABLES(MO,CashIncentiveCompReserve)
               BALANCE_SHEET_VARIABLES(MO,mty_accrued_vaca_pay)=
     +              BALANCE_SHEET_VARIABLES(MO-1,
     +                                     mty_accrued_vaca_pay)
     +              + INCOME_VARIABLES(MO,monthly_vacation_pay)
     +              - CASH_VARIABLES(MO,Cash_Vacation_Payments)
C
C FINISH THE FOLLOWING
C
               DEFERRED_GAIN_ROLLUP = DEFERRED_GAIN_ROLLUP
     +                                + DEFERRED_GAIN_ADJ(MO)
               BALANCE_SHEET_VARIABLES(MO,monthly_deferred_gains) =
     +            BALANCE_SHEET_VARIABLES(MO,monthly_deferred_gains)
     +            + DEFERRED_GAIN_ROLLUP

               BALANCE_SHEET_VARIABLES(MO,
     +                             mthly_deferred_purchse_powr_bal) =
     +                 BALANCE_SHEET_VARIABLES(MO-1,
     +                               mthly_deferred_purchse_powr_bal)

            ENDIF
         ENDDO
C
C ADD BALANCES TO PREVIOUSLY CALCULATED BALANCES
C
         BALANCE_SHEET_VARIABLES(:,monthly_unamort_interest_bal) =
     +               BALANCE_SHEET_VARIABLES(:,
     +                                 monthly_unamort_interest_bal)
     +               + UNAMORTIZED_INTEREST_BALANCE(:)
C
C APPLY DIRECT TRANSFERS AFTER CALCULATING THE UNADJUSTED BALANCE
C
         DO MO = 0, 12

               BALANCE_SHEET_VARIABLES(MO,mtly_cwip) =
     +                          BALANCE_SHEET_VARIABLES(MO,mtly_cwip)
     +                          + TRANSFER_VARIABLES(MO,mtly_cwip)
               BALANCE_SHEET_VARIABLES(MO,
     +                            monthly_other_long_term_liabs) =
     +                BALANCE_SHEET_VARIABLES(MO,
     +                              monthly_other_long_term_liabs)
     +                + TRANSFER_VARIABLES(MO,
     +                              monthly_other_long_term_liabs)
                  BALANCE_SHEET_VARIABLES(MO,
     +                               monthly_post_retire_med_fnd) =  ! 11
     +                BALANCE_SHEET_VARIABLES(MO,
     +                                 monthly_post_retire_med_fnd)
     +                + TRANSFER_VARIABLES(MO,
     +                                 monthly_post_retire_med_fnd)
               CONSOL_TRANSFER_LT_LIAB(MO) =
     +                    CONSOL_TRANSFER_LT_LIAB(MO)
     +                    + TRANSFER_VARIABLES(MO,
     +                              monthly_other_long_term_liabs)
               BALANCE_SHEET_VARIABLES(MO,monthly_net_nuclear_fuel) =
     +            BALANCE_SHEET_VARIABLES(MO,monthly_net_nuclear_fuel)
     +            + TRANSFER_VARIABLES(MO,monthly_net_nuclear_fuel)
               BALANCE_SHEET_VARIABLES(MO,monthly_capitalized_leases) =
     +           BALANCE_SHEET_VARIABLES(MO,monthly_capitalized_leases)
     +           + TRANSFER_VARIABLES(MO,monthly_capitalized_leases)
               BALANCE_SHEET_VARIABLES(MO,monthly_common_stock) =
     +                  BALANCE_SHEET_VARIABLES(MO,monthly_common_stock)
     +                  + TRANSFER_VARIABLES(MO,monthly_common_stock)
               BALANCE_SHEET_VARIABLES(MO,
     +                                  monthly_subsidiary_investment) = ! 7
     +                     BALANCE_SHEET_VARIABLES(MO,
     +                                    monthly_subsidiary_investment) ! 7
     +                     + NEW_SUB_INVESTMENT(MO)
c
c  Deferred Debits
C Deleted a large chunk of code below this comment.

               BALANCE_SHEET_VARIABLES(MO,mty_net_oth_debits) =
     +                  BALANCE_SHEET_VARIABLES(MO,
     +                                      monthly_net_dfrd_debits)
     +                  - BALANCE_SHEET_VARIABLES(MO,
     +                                             monthly_net_goodwill) ! 72
     +                  - BALANCE_SHEET_VARIABLES(MO,
     +                                    monthly_net_regulatory_assets) ! 73
     +                  - BALANCE_SHEET_VARIABLES(MO,
     +                                             mty_net_fasb_109) ! 74
     +                  - BALANCE_SHEET_VARIABLES(MO,
     +                                             mty_net_fasb_133) ! 75
     +                  - BALANCE_SHEET_VARIABLES(MO,
     +                                 monthly_unamort_interest_bal) ! 76
     +                  - BALANCE_SHEET_VARIABLES(MO,
     +                                mthly_unamortized_issue_exp_bal) ! 77

         ENDDO
C
         TOTAL_ASSETS =
     +            SUM_MONTHLY_BALANCE_STATEMENT(BALANCE_SHEET_VARIABLES)

         IF(CLASS_LINK_2 >= -1 .AND. .NOT. DONT_TALLY_THIS_CLASS) THEN
            DO VARIABLE = 1, BAL_SHEET_VARS
               IF(CLASS_TYPE == SBU .OR.
     +                               CLASS_TYPE == REGULATED_GROUP) THEN
                  IF(VARIABLE==monthly_cash_lti) CYCLE
                  IF(VARIABLE==monthly_debt_lti) CYCLE
                  IF(VARIABLE == monthly_short_lti) CYCLE
                  IF(VARIABLE == monthly_common_stock) CYCLE
                  IF(VARIABLE == monthly_retained_earnings_bal) CYCLE
                  IF(VARIABLE ==  Monthly_TOTAL_COMMON_EQUITY) CYCLE
                  IF(VARIABLE == monthly_preferred_stock) CYCLE
                  IF(VARIABLE == monthly_long_term_debt) CYCLE
                  IF(VARIABLE == monthly_total_capital) CYCLE
                  IF(VARIABLE == monthly_short_term_debt_bal) CYCLE
                  IF(VARIABLE == monthly_total_liabilities) CYCLE
                  IF(VARIABLE == monthly_total_assets) CYCLE
                  IF(VARIABLE == monthly_customer_deposits) CYCLE
                  IF(VARIABLE == monthly_accounts_payable_bal) CYCLE
                  IF(VARIABLE == monthly_accounts_receivable_bal) CYCLE

               ELSEIF(CLASS_TYPE == SUBSIDIARY) THEN
                  IF(VARIABLE == monthly_accounts_payable_bal) THEN
                     DO MO = 0, 12
                        IF(.NOT. CLASS_IS_ACTIVE(MO)) THEN

                           BALANCE_LINKED_LEVEL(MO,VARIABLE,
     +                                                 CLASS_LINK_2)= 0.
                        ELSE
                           BALANCE_LINKED_LEVEL(MO,VARIABLE,
     +                                                   CLASS_LINK_2) =
     +                            BALANCE_LINKED_LEVEL(MO,VARIABLE,
     +                                                     CLASS_LINK_2)
     +                            + BALANCE_SHEET_VARIABLES(MO,VARIABLE)
     +                            - FED_TAXES_PAYABLE_2_PARENT(MO)
                        ENDIF
                     ENDDO
                     CYCLE
                  ENDIF

               ENDIF
C
               DO MO = 0, 12
                  IF(.NOT. CLASS_IS_ACTIVE(MO)) THEN

                     IF(VARIABLE==monthly_accum_depreciation .AND.
     +                    R_YR == DEACTIVE_YR .AND. MO == MO_DEACT) THEN
                        BALANCE_SHEET_DEACTIVE_VARS(MO,VARIABLE,
     +                                                   CLASS_LINK_2) =
     +                        BALANCE_SHEET_DEACTIVE_VARS(MO,VARIABLE,
     +                                                     CLASS_LINK_2)
     +                        - BALANCE_SHEET_VARIABLES(MO-1,VARIABLE)
                     ENDIF

                  ELSE
                     BALANCE_LINKED_LEVEL(MO,VARIABLE,CLASS_LINK_2) =
     +                    BALANCE_LINKED_LEVEL(MO,VARIABLE,CLASS_LINK_2)
     +                    + BALANCE_SHEET_VARIABLES(MO,VARIABLE)
C
                     TRANSFER_LINKED_LEVEL_VARIABLES(MO,
     +                                          VARIABLE,CLASS_LINK_2) =
     +                        TRANSFER_LINKED_LEVEL_VARIABLES(MO,
     +                                            VARIABLE,CLASS_LINK_2)
     +                        + TRANSFER_VARIABLES(MO,VARIABLE)
                     BALANCE_SHEET_DEACTIVE_VARS(MO,VARIABLE,
     +                                                   CLASS_LINK_2) =
     +                        BALANCE_SHEET_DEACTIVE_VARS(MO,VARIABLE,
     +                                                     CLASS_LINK_2)
     +                        + BALANCE_SHEET_DEACTIVE_VARS(MO,VARIABLE,
     +                                                      CLASS_LEVEL)
                  ENDIF
               ENDDO
            ENDDO
            DO MO = 1, 12
               IF(.NOT. CLASS_IS_ACTIVE(MO)) THEN

                  PAYABLE_LINKED_LOWER_LEVEL(MO,CLASS_LINK_2) = 0.
                  RECEIVABLES_LINKED_LOWER_LEVEL(MO,CLASS_LINK_2) = 0.
               ELSE
                  PAYABLE_LINKED_LOWER_LEVEL(MO,CLASS_LINK_2) =
     +                       PAYABLE_LINKED_LOWER_LEVEL(MO,CLASS_LINK_2)
     +                       + MONTHLY_CHANGE_IN_PAYABLES(MO)
                  RECEIVABLES_LINKED_LOWER_LEVEL(MO,CLASS_LINK_2) =
     +                   RECEIVABLES_LINKED_LOWER_LEVEL(MO,CLASS_LINK_2)
     +                   + MONTHLY_CHANGE_IN_RECEIVABLES(MO)
               ENDIF
            ENDDO
            PAYABLE_LINKED_LOWER_LEVEL(0,CLASS_LINK_2) =
     +                  SUM(PAYABLE_LINKED_LOWER_LEVEL(1:,CLASS_LINK_2))
            RECEIVABLES_LINKED_LOWER_LEVEL(0,CLASS_LINK_2) =
     +              SUM(RECEIVABLES_LINKED_LOWER_LEVEL(1:,CLASS_LINK_2))
         ENDIF
C
         INCOME_LINKED_LEVEL(:,:,CLASS_LEVEL) = 0.
         CASH_LINKED_LEVEL(:,:,CLASS_LEVEL) = 0.
         DO MO = 1, 12
            IF(CLASS_IS_ACTIVE(MO)) THEN
               TEMP_PAYABLE_MONTHLY_VALUES(MO,:,CLASS_POS) =
     +                            PAYABLE_MONTHLY_VALUES(MO,:,CLASS_POS)
            ENDIF
         ENDDO
         TEMP_PAYABLE_MONTHLY_VALUES(0,:,CLASS_POS) =
     +            SUM(TEMP_PAYABLE_MONTHLY_VALUES(1:,:,CLASS_POS),DIM=1)
         BALANCE_SHEET_DEACTIVE_VARS(:,:,CLASS_LEVEL) = 0.

         IF(CLASS_LINK_2 >= -1 .AND. .NOT. DONT_TALLY_THIS_CLASS) THEN
C
C PASS-UP INCOME ITEMS
C
            IF(IPALCO())
     +           CALL IPALCO_JURISDICTIONAL_RETAIL_REVENUES_REPORT(R_YR,
     +                                          CLASS_ID,
     +                                          INCOME_VARIABLES,
     +                                          THIS_IS_REPORTING_CLASS)
            DO VARIABLE = 1, INCOME_VARS
               IF(VARIABLE == Monthly_Issue_Expense_Amorts) CYCLE
               IF(VARIABLE == Monthly_PS_Amort_Dividends) CYCLE
               IF(CLASS_TYPE == SBU .OR.
     +                               CLASS_TYPE == REGULATED_GROUP) THEN
                  IF(VARIABLE == Monthly_Investment_Earnings) CYCLE
                  IF(VARIABLE == mthly_invstmt_earngs_rcvbl)
     +                                                             CYCLE
                  IF(VARIABLE == Monthly_LTD_Total_Interest) CYCLE
                  IF(VARIABLE == MonthlySTDInterest) CYCLE
                  IF(VARIABLE == mty_intst_on_nts_pyble)CYCLE
                  IF(VARIABLE == Monthly_Total_PS_Dividends) CYCLE
                  IF(VARIABLE == Monthly Common Dividends) CYCLE
                  IF(VARIABLE == mty_totl_lti_income)CYCLE
                  IF(VARIABLE == mty_mdl_lt_nvst_income)CYCLE
                  IF(VARIABLE ==
     +                        mt_dbt_file_linvst_income)CYCLE
                  IF(VARIABLE == monthly_notes_receivable_income)CYCLE
                  IF(VARIABLE == Monthly_Dividend_70_Earnings)CYCLE
                  IF(VARIABLE == mty_rglr_dvd_income)CYCLE

                  IF(VARIABLE == BTL_Monthly_STInvestmet_Income) CYCLE
                  IF(VARIABLE == BTL_Monthly_Interest_Income) CYCLE
                  IF(VARIABLE == BTL_Monthly_Income_Taxes) CYCLE
                  IF(VARIABLE == Monthly_Shares_Outstanding) CYCLE
                  IF(VARIABLE == mthy_fed_atl_income_tax_pd)
     +                                                             CYCLE
                  IF(VARIABLE == mt_fed_btl_incm_tax_pd)
     +                                                             CYCLE
                  IF(VARIABLE == mty_tot_fed_incm_tax_pd)
     +                                                             CYCLE
                  IF(VARIABLE == Monthly_LTD_Premium_Amort) CYCLE
                  IF(VARIABLE == Monthly_PS_Premium_Amort) CYCLE

                  IF(.NOT. EXCLUDE_SBU_PROPERTY_TAX) THEN
                     IF(VARIABLE == Monthly_Property_Taxes) CYCLE
                     IF(VARIABLE==MonthlyExpFilePropertyTaxes) CYCLE
                     IF(VARIABLE == 
     +                    mth_modl_calcd_prop_taxes) CYCLE
                  ENDIF
                  IF(.NOT. EXCLUDE_SBU_REVENUE_TAX .AND. 
     +                  VARIABLE == Monthly_Operating_Revenue_Tax) CYCLE

                  IF(VARIABLE == monthly_oth_taxes) CYCLE
                  IF(.NOT. EXCLUDE_SBU_STATE_INCOME_TAX) THEN
                     IF(VARIABLE == mthy_state_atl_incm_tax_pd)
     +                                                             CYCLE
                     IF(VARIABLE == mt_st_btl_incm_tax_pd)
     +                                                             CYCLE
                     IF(VARIABLE == mty_tot_st_incm_tax_pd)
     +                                                             CYCLE
                  ENDIF
                  IF(.NOT. EXCLUDE_SBU_CAPITAL_TAX .AND. 
     +               (VARIABLE == Monthly_State_Tax_on_Capital .OR.
     +                VARIABLE == Monthly_Federal_Tax_on_Capital)) CYCLE
               ELSEIF(CLASS_TYPE == SUBSIDIARY) THEN

                  IF(VARIABLE == Monthly_Subsidiary_Income) CYCLE
                  IF(VARIABLE == Monthly_Shares_Outstanding) CYCLE
                  IF(VARIABLE == Monthly_Earnings_2_Common) THEN
                     DO MO = 1, 12
                        IF(.NOT. CLASS_IS_ACTIVE(MO)) THEN

                           INCOME_LINKED_LEVEL(MO,
     +                                      Monthly_Subsidiary_Income,
     +                                            SUB_LINK_2_CLASS) = 0.
                        ELSE
                           INCOME_LINKED_LEVEL(MO,
     +                                     Monthly_Subsidiary_Income,
     +                                               SUB_LINK_2_CLASS) =
     +                     INCOME_LINKED_LEVEL(MO,
     +                       Monthly_Subsidiary_Income,SUB_LINK_2_CLASS)
     +                     + INCOME_VARIABLES(MO,
     +                                        Monthly_Earnings_2_Common)
                        ENDIF
                     ENDDO
                     INCOME_LINKED_LEVEL(0,Monthly_Subsidiary_Income,
     +                                               SUB_LINK_2_CLASS) =
     +                     SUM(INCOME_LINKED_LEVEL(1:,
     +                                       Monthly_Subsidiary_Income,
     +                                                SUB_LINK_2_CLASS))
                     CYCLE
                  ENDIF
C    +          Monthly Earnings 2 Common,
               ENDIF  
               DO MO = 1, 12
                  IF(CLASS_IS_ACTIVE(MO)) THEN

                     IF(VARIABLE == Prior level Method Adjustment .AND.
     +                                .NOT. (CLASS_TYPE == PARENT)) THEN
                        INCOME_LINKED_LEVEL(MO,VARIABLE,CLASS_LINK_2) =
     +                     INCOME_LINKED_LEVEL(MO,VARIABLE,CLASS_LINK_2)
     +                     + INCOME_VARIABLES(MO,
     +                                    Prior level Method Adjustment)
     +                     + INCOME_VARIABLES(MO,
     +                                      Operating Method Adjustment)
     +                     + INCOME_VARIABLES(MO,
     +                                    Prior Years Method Adjustment)
                     ELSEIF(VARIABLE==Operating Method Adjustment .AND.
     +                                .NOT. (CLASS_TYPE == PARENT)) THEN
                        INCOME_LINKED_LEVEL(MO,VARIABLE,CLASS_LINK_2)=0.
                     ELSEIF(VARIABLE==Prior Years Method Adjustment.AND.
     +                                .NOT. (CLASS_TYPE == PARENT)) THEN
                        INCOME_LINKED_LEVEL(MO,VARIABLE,CLASS_LINK_2)=0.
                     ELSE
                        INCOME_LINKED_LEVEL(MO,VARIABLE,CLASS_LINK_2) =
     +                     INCOME_LINKED_LEVEL(MO,VARIABLE,CLASS_LINK_2)
     +                     + INCOME_VARIABLES(MO,VARIABLE)
                     ENDIF
                  ENDIF
               ENDDO
               INCOME_LINKED_LEVEL(0,VARIABLE,SUB_LINK_2_CLASS) =
     +            SUM(INCOME_LINKED_LEVEL(1:,VARIABLE,SUB_LINK_2_CLASS))
            ENDDO
         ENDIF
C
         IF(CLASS_LINK_2 >= -1 .AND. .NOT. DONT_TALLY_THIS_CLASS) THEN
            IF(CLASS_TYPE == SUBSIDIARY) THEN
               DO MO = 1, 12
                  IF(CLASS_IS_ACTIVE(MO)) THEN

                     SUB_FED_TAX_PAYMENTS_2_PARENT(MO) =
     +                     SUB_FED_TAX_PAYMENTS_2_PARENT(MO)
     +                     + CASH_VARIABLES(MO,
     +                                   cash_fed_income_taxs_pd)
                     SUB_STATE_TAX_PAYMENTS_2_PARENT(MO) =
     +                     SUB_STATE_TAX_PAYMENTS_2_PARENT(MO)
     +                     + CASH_VARIABLES(MO,
     +                                     cash_st_income_taxes_pd)
                     SUB_TAXABLE_INCOME_B4_DEDUCTS(MO) =
     +                               SUB_TAXABLE_INCOME_B4_DEDUCTS(MO)
     +                               + RPT_TAXABLE_INCOME_B4_DEDUCTS(MO)
                     SUB_STATE_TAXES_PAID(MO) = SUB_STATE_TAXES_PAID(MO)
     +                                        + RPT_STATE_INCOME_TAX(MO)
                  ENDIF
               ENDDO
               SUB_FED_TAX_PAYMENTS_2_PARENT(0) =
     +                            SUM(SUB_FED_TAX_PAYMENTS_2_PARENT(1:))
               SUB_STATE_TAX_PAYMENTS_2_PARENT(0) =
     +                          SUM(SUB_STATE_TAX_PAYMENTS_2_PARENT(1:))
               SUB_TAXABLE_INCOME_B4_DEDUCTS(0) =
     +                            SUM(SUB_TAXABLE_INCOME_B4_DEDUCTS(1:))
               SUB_STATE_TAXES_PAID(0) =  SUM(SUB_STATE_TAXES_PAID(1:))
            ENDIF
         ENDIF
         IF(CLASS_LINK_2 >= -1 .AND. .NOT. DONT_TALLY_THIS_CLASS) THEN
            DO VARIABLE = 1, CASH_VARS
               IF(CLASS_TYPE == SBU .OR.
     +                               CLASS_TYPE == REGULATED_GROUP) THEN
                  IF(VARIABLE==cash_fm_repaid_issued_notes)CYCLE
                  IF(VARIABLE == csh_4_redeeming_notes_owed) CYCLE
                  IF(VARIABLE == cash_4_new_notes_issued) CYCLE
                  IF(VARIABLE == cash_fm_notes_issd_by_othrs) CYCLE
                  IF(VARIABLE == Cash_Interest_on_Notes_Payable) CYCLE
                  IF(VARIABLE == cash_investmt_divnd_earngs)CYCLE
                  IF(VARIABLE == cash_invest_interest_ernings)CYCLE
                  IF(VARIABLE == cash_ltd_interest) CYCLE
                  IF(VARIABLE == cash_std_interest) CYCLE
                  IF(VARIABLE == cash_ps_dividends) CYCLE
                  IF(VARIABLE == cash_common_dividends) CYCLE
                  IF(VARIABLE == cash_st_income_taxes_pd) CYCLE
                  IF(VARIABLE == cash_fed_income_taxs_pd) CYCLE
                  IF(VARIABLE == cash_state_on_capital) CYCLE
                  IF(VARIABLE == cash_fed_tax_on_capital) CYCLE
                  IF(VARIABLE == cash_parent_taxes_pd) CYCLE
                  IF(VARIABLE == cash_subsidiary_tax_pmts) CYCLE
                  IF(VARIABLE==cash_subsdry_fed_taxes_pd) CYCLE
                  IF(VARIABLE==cash_subsdry_st_taxes_pd) CYCLE
                  IF(VARIABLE == cash_consolidate_taxes_pd) CYCLE

                  IF(VARIABLE == cash_ps_retirements) CYCLE
                  IF(VARIABLE == cash_ltd_retirements) CYCLE
                  IF(VARIABLE == cash_common_stock_buyback) CYCLE
                  IF(VARIABLE == cash_ps_issued) CYCLE
                  IF(VARIABLE == cash_ltd_issued) CYCLE
                  IF(VARIABLE == cash_std_issued) CYCLE
                  IF(VARIABLE == cash_common_stock_issued) CYCLE
                  IF(VARIABLE == cash_closing_balance) CYCLE
                  IF(VARIABLE == cash_opening_balance) CYCLE
                  IF(VARIABLE == cash_period_change) CYCLE
                  IF(VARIABLE == common_shares_issued) CYCLE
                  IF(VARIABLE == cash_common_issue_expense) CYCLE
                  IF(VARIABLE == cash_ltd_ps_issue_expense) CYCLE
                  IF(VARIABLE == cash_change_ltinvestments) CYCLE
                  IF(VARIABLE == cash_change_debt_investments) CYCLE
c
c to cover the excluded items
c
                  IF(VARIABLE ==
     +                       cash_mdl_calcd_prop_taxes) CYCLE  ! 9/9/99 as fix for double paying
                  IF(VARIABLE == cash_operating_revenue_tax) CYCLE  ! 9/9/99 as fix for double paying
                  IF(VARIABLE == cash_other_taxes) CYCLE  ! 9/9/99 as fix for double paying

                  IF(.NOT. EXCLUDE_SBU_PROPERTY_TAX) THEN
                     IF(VARIABLE == cash_property_taxes) CYCLE
                     IF(VARIABLE == cash_exp_file_prop_taxes) CYCLE
                     IF(VARIABLE == 
     +                       cash_mdl_calcd_prop_taxes) CYCLE
                  ENDIF
                  IF(.NOT. EXCLUDE_SBU_REVENUE_TAX .AND. 
     +                     VARIABLE == cash_operating_revenue_tax) CYCLE

                  IF(VARIABLE == cash_other_taxes) CYCLE
                  IF(.NOT. EXCLUDE_SBU_STATE_INCOME_TAX .AND. 
     +                   VARIABLE == cash_st_income_taxes_pd) CYCLE
                  IF(.NOT. EXCLUDE_SBU_CAPITAL_TAX .AND. 
     +                  (VARIABLE == cash_state_on_capital .OR.
     +                   VARIABLE == cash_fed_tax_on_capital)) CYCLE
               ELSEIF(CLASS_TYPE == SUBSIDIARY) THEN

                  IF(VARIABLE == cash_subsidiary_dividends) CYCLE
                  IF(VARIABLE == cash_equity_to_subsidiaries) CYCLE
                  IF(VARIABLE == common_shares_issued) CYCLE
C
C MOVE TO PARENT AND TO CONSOLIDATED
C
                  IF(VARIABLE == cash_common_dividends) THEN
                     DO MO = 1, 12
                        IF(CLASS_IS_ACTIVE(MO)) THEN

                           CASH_LINKED_LEVEL(MO,
     +                         cash_subsidiary_dividends,CLASS_LINK_2) =
     +                        CASH_LINKED_LEVEL(MO,
     +                           cash_subsidiary_dividends,CLASS_LINK_2)
     +                        + CASH_VARIABLES(MO,cash_common_dividends)
                           CASH_LINKED_LEVEL(MO,
     +                      cash_subsidiary_dividends,SUB_LINK_2_CLASS)=
     +                        CASH_LINKED_LEVEL(MO,
     +                       cash_subsidiary_dividends,SUB_LINK_2_CLASS)
     +                        + CASH_VARIABLES(MO,cash_common_dividends)
                        ENDIF
                     ENDDO
                     CASH_LINKED_LEVEL(0,
     +                         cash_subsidiary_dividends,CLASS_LINK_2) =
     +                   SUM(CASH_LINKED_LEVEL(1:,
     +                          cash_subsidiary_dividends,CLASS_LINK_2))
                     CASH_LINKED_LEVEL(0,
     +                      cash_subsidiary_dividends,SUB_LINK_2_CLASS)=
     +                   SUM(CASH_LINKED_LEVEL(1:,
     +                      cash_subsidiary_dividends,SUB_LINK_2_CLASS))
                     CYCLE
                  ENDIF
                  IF(VARIABLE == cash_common_stock_issued) THEN
                     DO MO = 1, 12
                        IF(CLASS_IS_ACTIVE(MO)) THEN

                           CASH_LINKED_LEVEL(MO,
     +                       cash_equity_to_subsidiaries,CLASS_LINK_2) =
     +                     CASH_LINKED_LEVEL(MO,
     +                         cash_equity_to_subsidiaries,CLASS_LINK_2)
     +                     + CASH_VARIABLES(MO,cash_common_stock_issued)
                           CASH_LINKED_LEVEL(MO,
     +                    cash_equity_to_subsidiaries,SUB_LINK_2_CLASS)=
     +                   CASH_LINKED_LEVEL(MO,
     +                     cash_equity_to_subsidiaries,SUB_LINK_2_CLASS)
     +                   + CASH_VARIABLES(MO,cash_common_stock_issued)
                        ENDIF
                     ENDDO
                     CASH_LINKED_LEVEL(0,
     +                       cash_equity_to_subsidiaries,CLASS_LINK_2) =
     +                     SUM(CASH_LINKED_LEVEL(1:,
     +                        cash_equity_to_subsidiaries,CLASS_LINK_2))
                     CASH_LINKED_LEVEL(0,
     +                    cash_equity_to_subsidiaries,SUB_LINK_2_CLASS)=
     +                   SUM(CASH_LINKED_LEVEL(1:,
     +                    cash_equity_to_subsidiaries,SUB_LINK_2_CLASS))
                     CYCLE
                  ENDIF
                  IF(VARIABLE == cash_subsdry_fed_taxes_pd)THEN
                     DO MO = 1, 12
                        IF(CLASS_IS_ACTIVE(MO)) THEN
                           CASH_LINKED_LEVEL(MO,
     +                               cash_subsdry_fed_taxes_pd,
     +                                               SUB_LINK_2_CLASS) =
     +                           CASH_LINKED_LEVEL(MO,
     +                               cash_subsdry_fed_taxes_pd,
     +                                                 SUB_LINK_2_CLASS)
     +                           + CASH_VARIABLES(MO,
     +                                   cash_fed_income_taxs_pd)
                        ENDIF
                     ENDDO
                     CASH_LINKED_LEVEL(0,
     +                    cash_subsdry_fed_taxes_pd,
     +                                               SUB_LINK_2_CLASS) =
     +                   SUM(CASH_LINKED_LEVEL(1:,
     +                               cash_subsdry_fed_taxes_pd,
     +                                                SUB_LINK_2_CLASS))
                     CYCLE
                  ENDIF
                  IF(VARIABLE == cash_subsdry_st_taxes_pd) THEN
                     DO MO = 1, 12
                        IF(CLASS_IS_ACTIVE(MO)) THEN
                           CASH_LINKED_LEVEL(MO,
     +                                 cash_subsdry_st_taxes_pd,
     +                                               SUB_LINK_2_CLASS) =
     +                           CASH_LINKED_LEVEL(MO,
     +                                 cash_subsdry_st_taxes_pd,
     +                                                 SUB_LINK_2_CLASS)
     +                           + CASH_VARIABLES(MO,
     +                                     cash_st_income_taxes_pd)
                        ENDIF
                     ENDDO
                     CASH_LINKED_LEVEL(0,
     +                    cash_subsdry_st_taxes_pd,
     +                                               SUB_LINK_2_CLASS) =
     +                   SUM(CASH_LINKED_LEVEL(1:,
     +                                 cash_subsdry_st_taxes_pd,
     +                                                SUB_LINK_2_CLASS))
                     CYCLE
                  ENDIF
                  IF(VARIABLE ==
     +                  cash_subsdry_st_taxes_pd_by_pt) THEN
                     DO MO = 1, 12
                        IF(CLASS_IS_ACTIVE(MO)) THEN
                           CASH_LINKED_LEVEL(MO,
     +                       cash_subsdry_st_taxes_pd_by_pt,
     +                                               SUB_LINK_2_CLASS) =
     +                     CASH_LINKED_LEVEL(MO,
     +                       cash_subsdry_st_taxes_pd_by_pt,
     +                                                 SUB_LINK_2_CLASS)
     +                     + CASH_VARIABLES(MO,
     +                                     cash_st_income_taxes_pd)
                        ENDIF
                     ENDDO
                     CASH_LINKED_LEVEL(0,
     +                       cash_subsdry_st_taxes_pd_by_pt,
     +                                               SUB_LINK_2_CLASS) =
     +                   SUM(CASH_LINKED_LEVEL(1:,
     +                       cash_subsdry_st_taxes_pd_by_pt,
     +                                                SUB_LINK_2_CLASS))
                     CYCLE
                  ENDIF
               ENDIF
               DO MO = 1, 12
                  IF(CLASS_IS_ACTIVE(MO)) THEN

                     IF(VARIABLE == Cash_Prior_level_Method_Adj .AND.
     +                                .NOT. (CLASS_TYPE == PARENT)) THEN
                        CASH_LINKED_LEVEL(MO,VARIABLE,CLASS_LINK_2) =
     +                       CASH_LINKED_LEVEL(MO,VARIABLE,CLASS_LINK_2)
     +                       + INCOME_VARIABLES(MO,
     +                                    Prior level Method Adjustment)
     +                       + INCOME_VARIABLES(MO,
     +                                      Operating Method Adjustment)
     +                       + INCOME_VARIABLES(MO,
     +                                    Prior Years Method Adjustment)
                     ELSEIF(VARIABLE == Cash_Operating_Method_Adj .AND.
     +                                .NOT. (CLASS_TYPE == PARENT)) THEN
                        CASH_LINKED_LEVEL(MO,VARIABLE,CLASS_LINK_2) = 0.
                     ELSEIF(VARIABLE==Cash_Prior_Years_Method_Adj .AND.
     +                                .NOT. (CLASS_TYPE == PARENT)) THEN
                        CASH_LINKED_LEVEL(MO,VARIABLE,CLASS_LINK_2) = 0.
                     ELSE
                        CASH_LINKED_LEVEL(MO,VARIABLE,CLASS_LINK_2) =
     +                    CASH_LINKED_LEVEL(MO,VARIABLE,CLASS_LINK_2) +
     +                    CASH_VARIABLES(MO,VARIABLE)
                     ENDIF
                  ENDIF
               ENDDO
               CASH_LINKED_LEVEL(0,VARIABLE,CLASS_LINK_2) =
     +                  SUM(CASH_LINKED_LEVEL(1:,VARIABLE,CLASS_LINK_2))
            ENDDO
         ENDIF
C
         CALL TRANSFER_MONTHLY_TAX_ITEMS(CLASS_TYPE,
     +                                   CLASS_LEVEL)
         CALL SAVE_MONTHLY_CLASS_OPT_REVS(CLASS_POS,
     +                INCOME_VARIABLES(0,Operating Method Adjustment),
     +                INCOME_VARIABLES(0,Prior Years Method Adjustment))
C***********************************************************************
         ENTRY CONSOLIDATED_MONTHLY_ANALYSIS(R_YR,CLASS_ID,
     +                            R_ASSET_CLASS_NAME,
     +                            CLASS_POS,
     +                            CLASS_TYPE,
     +                            CLASS_LEVEL,
     +                            WRITE_MONTHLY_INFOR,
     +                            DONT_TALLY_THIS_CLASS,
     +                            ELIMINAITON_CLASS_ID_NUM,
     +                            THIS_IS_REPORTING_CLASS,
     +                            ANNUAL_VARS,
     +                            DEFERRED_TAXES_DR_BOY,
     +                            OTHER_LT_LIABILITIES_BOY,
     +                            ACCOUNTS_PAYABLE_BOY,
     +                            DEFERRED_TAXES_CR_BOY,
     +                            BOY_INVESTMENT_IN_SUBSIDIARIES,
     +                            CONSOLIDATED_FED_TAX_ADJ,
     +                            CONSOLIDATED_STATE_TAX_ADJ,
     +                            BOY_RETAINED_EARNINGS,
     +                            ACCOUNTS_RECEIVABLE_BOY,
     +                            BOY_CWIP,
     +                            OTHER_INVESTMENTS_BALANCE_BOY,
     +                            DEFERRED_DEBITS_BOY,
     +                            ASSETS_NEC_BOY,
     +                            BOY_LTD_BALANCE,
     +                            CONSOLIDATED_BOY_FUNDS_BALANCE,
     +                        CONSOLIDATED_INVESTMENT_INCOME_RECEIVABLE,
     +                            BOY_LTD_BALANCE_WO_CURRENT_LTD)
C***********************************************************************
C

         OUTPUT_CLASS_ID = ANNUAL_VARS(441)
         IF(CLASS_TYPE  == PARENT) THEN
            PARENT_CS_DIV_CARRY_OVER = TEMP_CS_DIV_CARRY_OVER

               CASH_VARIABLES(:,cash_parent_taxes_pd) =
     +                CASH_VARIABLES(:,cash_fed_income_taxs_pd)

               CASH_VARIABLES(:,cash_subsidiary_tax_pmts) =
     +                             SUB_FED_TAX_PAYMENTS_2_PARENT(:)
     +                             + SUB_STATE_TAX_PAYMENTS_2_PARENT(:)
               PARENT_COMMON_DIVIDEND(:) =
     +                      INCOME_VARIABLES(:,Monthly Common Dividends)
               PARENT_CASH_DIVIDEND_PAID(:) =
     +                           CASH_VARIABLES(:,cash_common_dividends)
               PARENT_COMMON_STOCK_ISSUED(:) =
     +                        CASH_VARIABLES(:,cash_common_stock_issued)

C RETURNS THE CONSOLIDATED ACTUAL PAYMENTS AND/OR THE ADDITIONS TO THE 
C         CALCULATED PAYMENTS
C
            CALL GET_ACTUAL_CONSOLID_PAYABLES(R_YR,
     +                                           CONSOLIDATED_VARIABLES)
            CALL MONTHLY_PARENT_TAX_ITEMS(R_YR,CLASS_ID,
     +                                    PARENT_DEFERRED_TAXES_DR)

               CASH_VARIABLES(:,cash_consolidate_taxes_pd) =
     +                             CONSOLIDATED_VARIABLES(:,
     +                                   cash_fed_income_taxs_pd)

               SUB_DEFERRED_TAXES_DR(:) = INCOME_VARIABLES(:,
     +                                  Monthly_Income_Tax_Deferrals_Dr)
     +                                    - PARENT_DEFERRED_TAXES_DR(:)

         ENDIF
         IF(CLASS_TYPE == CONSOLIDATED) THEN
            OUTPUT_CLASS_ID = -1. ! ANNUAL_VARS(441)

            TEMP_PAYABLE_MONTHLY_VALUES(:,:,-1) =
     +                                    PAYABLE_MONTHLY_VALUES(:,:,-1)
            ASSET_CLASS_NAME = R_ASSET_CLASS_NAME
            YR = R_YR
            INCOME_VARIABLES = 0.
            CASH_VARIABLES = 0.
            CALL MONTHLY_ELIM_REVENUES(R_YR,INT2(-1),INCOME_VARIABLES)
            CALL MONTHLY_ELIM_EXPENSES(R_YR,INT2(-1),
     +                           INCOME_VARIABLES(0, EXP_OFFSET_LINE+1))
            CALL MONTHLY_CONSOL_TAX_ITEMS(R_YR,CONSOL_DEFERRED_TAXES_DR)
C ELIMINATE NA3 ODEC INCOME STATEMENT TRANSACTIONS
            IF(ODEC() .AND. ODEC_NA3_ACTIVE) THEN
               INCOME_VARIABLES(:,BulkPower) = 
     +                                    INCOME_VARIABLES(:,BulkPower)
     +                                    + ODEC_NA3_INTRA_BULK_POWER(:)
               INCOME_VARIABLES(:,monthly_purchased_power) = 
     +                       INCOME_VARIABLES(:,monthly_purchased_power)
     +                        + ODEC_NA3_INTRA_BULK_POWER(:)
            ENDIF

            CALL DEBT_MONTHLY_INCOME_INFO(R_YR,INT2(-1),
     +                                    INCOME_VARIABLES,
     +                                    CASH_VARIABLES)
            CALL DEBT_MONTHLY_INCOME_INFO(R_YR,ELIMINAITON_CLASS_ID_NUM,
     +                                    INCOME_VARIABLES,
     +                                    CASH_VARIABLES)
            INTRA_COMPANY_EARNINGS_RECEIVABLE(:) =
     +           INCOME_VARIABLES(:,
     +                           mthly_invstmt_earngs_rcvbl)
     +           + INCOME_VARIABLES(:,mty_totl_lti_income)
     +           + INCOME_VARIABLES(:,BTL_Monthly_STInvestmet_Income)
     +           - CASH_VARIABLES(:,cash_investmt_divnd_earngs)
     +           - CASH_VARIABLES(:,cash_invest_interest_ernings)
     +           - CASH_VARIABLES(:,csh_st_invstmt_incm)  ! 112
            CONSOLIDATED_INVESTMENT_INCOME_RECEIVABLE =
     +                              INTRA_COMPANY_EARNINGS_RECEIVABLE(0)
            NOTES_PAYABLE_MADE = 0.
            NOTES_RECEIVABLE_MADE = 0.
            UNAMORTIZED_INTEREST_BALANCE = 0.
            CALL MONTHLY_NOTES_MADE(R_YR,INT2(-1),
     +                              NOTES_PAYABLE_MADE,
     +                              NOTES_RECEIVABLE_MADE,
     +                              UNAMORTIZED_INTEREST_BALANCE)
            CALL MONTHLY_NOTES_MADE(R_YR,ELIMINAITON_CLASS_ID_NUM,
     +                              NOTES_PAYABLE_MADE,
     +                              NOTES_RECEIVABLE_MADE,
     +                              UNAMORTIZED_INTEREST_BALANCE)
            BALANCE_SHEET_VARIABLES(0,monthly_notes_receivable) =
     +                                    -SAVE_ANNUAL_BOY_VARIABLE(402)
            BALANCE_SHEET_VARIABLES(0,monthly_notes_payable) =
     +                                    -SAVE_ANNUAL_BOY_VARIABLE(403)
C
C REVERSE THE RECEIVABLES AND PAYABLES
C
            ELIM_OF_MONTHLY_CHANGE_IN_RECEIVABLES = 0.
            ELIM_OF_MONTHLY_CHANGE_IN_PAYABLES = 0.
            CALL MON_DELTA_RECEIVABLES_PAYABLES(R_YR,INT2(-1),
     +                            ELIM_OF_MONTHLY_CHANGE_IN_RECEIVABLES,
     +                            ELIM_OF_MONTHLY_CHANGE_IN_PAYABLES)
            CALL MON_DELTA_RECEIVABLES_PAYABLES(R_YR,
     +                            ELIMINAITON_CLASS_ID_NUM,
     +                            ELIM_OF_MONTHLY_CHANGE_IN_RECEIVABLES,
     +                            ELIM_OF_MONTHLY_CHANGE_IN_PAYABLES)
            CALL MON_REV_FORC_RECEI_PAYABLES(INT2(-1),
     +                            ELIM_OF_MONTHLY_CHANGE_IN_RECEIVABLES,
     +                            ELIM_OF_MONTHLY_CHANGE_IN_PAYABLES)
            CALL MON_REV_FORC_RECEI_PAYABLES(
     +                            ELIMINAITON_CLASS_ID_NUM,
     +                            ELIM_OF_MONTHLY_CHANGE_IN_RECEIVABLES,
     +                            ELIM_OF_MONTHLY_CHANGE_IN_PAYABLES)

               MONTHLY_CHANGE_IN_PAYABLES(:) =
     +                          PAYABLE_LINKED_LOWER_LEVEL(:,INT2(-1))
     +                          - ELIM_OF_MONTHLY_CHANGE_IN_PAYABLES(:)

               MONTHLY_CHANGE_IN_RECEIVABLES(:) =
     +                       RECEIVABLES_LINKED_LOWER_LEVEL(:,INT2(-1))
     +                       - ELIM_OF_MONTHLY_CHANGE_IN_RECEIVABLES(:)

               PAYABLE_LINKED_LOWER_LEVEL(:,INT2(-1)) = 0.
               RECEIVABLES_LINKED_LOWER_LEVEL(:,INT2(-1)) = 0.

            DO MO = 1, 12 
               BALANCE_SHEET_VARIABLES(MO,monthly_notes_payable) =
     +               BALANCE_SHEET_VARIABLES(MO-1,monthly_notes_payable)
     +               - NOTES_PAYABLE_MADE(MO)
     +               + CASH_VARIABLES(MO,csh_4_redeeming_notes_owed)
               BALANCE_SHEET_VARIABLES(MO,monthly_notes_receivable) =
     +            BALANCE_SHEET_VARIABLES(MO-1,monthly_notes_receivable)
     +            - NOTES_RECEIVABLE_MADE(MO)
     +            + CASH_VARIABLES(MO,
     +                              cash_fm_repaid_issued_notes)
            ENDDO
            
C
C
C PROPERTY TAXES FOR CONSOLIDATED
C
            INCOME_VARIABLES(:,Monthly_Property_Taxes) =
     +              INCOME_VARIABLES(:,MonthlyExpFilePropertyTaxes)
     +              + INCOME_VARIABLES(:,
     +                          mth_modl_calcd_prop_taxes)
C
            DO VARIABLE = 1, INCOME_VARS 
               IF(VARIABLE == Monthly_Subsidiary_Income) THEN
                  INCOME_VARIABLES(:,VARIABLE) = UNMODELED_SUB_INCOME(:)
                  CYCLE
               ENDIF
               IF(VARIABLE == monthly_oth_taxes) THEN
                  INCOME_VARIABLES(:,VARIABLE) = SUB_OTHER_TAXES(:)
                  CYCLE
               ENDIF
               IF(VARIABLE == Monthly_Income_Tax_Deferrals_Dr) THEN
                  INCOME_VARIABLES(:,VARIABLE) =
     +                                 PARENT_STATE_DR_TAX_FROM_NOLS(:)
     +                                 + SUB_STATE_DR_TAX_FROM_NOLS(:)
     +                                 + CONSOL_DEFERRED_TAXES_DR(:)
                  CYCLE
               ENDIF
               IF(VARIABLE == Monthly Common Dividends) THEN
                  INCOME_VARIABLES(:,Monthly Common Dividends) =
     +                                         PARENT_COMMON_DIVIDEND(:)
                  CYCLE
               ENDIF
               IF(VARIABLE == BTL_Monthly_Income_Taxes) THEN
                  INCOME_VARIABLES(:,BTL_Monthly_Income_Taxes) =
     +                                     SUB_BTL_FEDERAL_TAXES_PAID(:)
     +                                     + SUB_BTL_STATE_TAXES_PAID(:)
                  CYCLE
               ENDIF
C
               INCOME_VARIABLES(:,VARIABLE) =
     +                       INCOME_LINKED_LEVEL(:,VARIABLE,CLASS_LEVEL)
     +                       - INCOME_VARIABLES(:,VARIABLE)
            ENDDO
C
C ZERO 
C
            TAX_VARIABLES = 0.
            CALL TAX_ISSUES_AND_ANALYSIS(R_YR,CLASS_ID,
     +                                   CLASS_TYPE,
     +                                   CLASS_LEVEL,
     +                                   ANNUAL_VARS)
            INCOME_TAX_DEPRECIATION =  ANNUAL_VARS(130)
C
C DEGINNING OF TAX ISSUES 11/25/99
C
            CALL MONTHLY_TAX_ADJUSTMENTS(INT2(-1),R_YR,
     +                                   PROPERTY_TAX_ADJUSTMENT,
     +                                   OTHER_TAX_ADJUSTMENT,
     +                                   REVENUE_TAX_ADJUSTMENT,
     +                                   STATE_M1_ADDITIONS,
     +                                   STATE_M1_DEDUCTIONS,
     +                                   FEDERAL_M1_ADDITIONS,
     +                                   FEDERAL_M1_DEDUCTIONS,
     +                                   STATE_BTL_MISC_DEDUCTIONS,
     +                                   FEDERAL_BTL_MISC_DEDUCTIONS,
     +                                   STATE_INCOME_TAX_ADJUSTMENT,
     +                                   FEDERAL_INCOME_TAX_ADJUSTMENT)

            DO MO = 0, 12
               MONTHLY_STATE_TAX_RATE(MO) =  STATE_TAX_RATE/100.
               MONTHLY_FEDERAL_TAX_RATE(MO) = FEDERAL_TAX_RATE ! /100.
               STATE_M1_DEDUCTIONS(MO) = STATE_M1_DEDUCTIONS(MO)
     +                 + TAX_VARIABLES(MO,monthly_atl_debit_tax_expense)
     +                 + TAX_TRANSFER_VARIABLES(MO,
     +                        monthly_atl_debit_tax_expense,CLASS_LEVEL)
     +                 + TAX_VARIABLES(MO,monthly_btl_debit_tax_expense)
     +                 + TAX_TRANSFER_VARIABLES(MO,
     +                        monthly_btl_debit_tax_expense,CLASS_LEVEL)

               STATE_BTL_MISC_DEDUCTIONS(MO) =
     +                 STATE_BTL_MISC_DEDUCTIONS(MO)
     +                 + TAX_VARIABLES(MO,monthly_btl_debit_tax_expense)
     +                 + TAX_TRANSFER_VARIABLES(MO,
     +                        monthly_btl_debit_tax_expense,CLASS_LEVEL)
               FEDERAL_M1_DEDUCTIONS(MO) = FEDERAL_M1_DEDUCTIONS(MO)
     +                 + TAX_VARIABLES(MO,monthly_atl_debit_tax_expense)
     +                 + TAX_TRANSFER_VARIABLES(MO,
     +                        monthly_atl_debit_tax_expense,CLASS_LEVEL)
     +                 + TAX_VARIABLES(MO,monthly_btl_debit_tax_expense)
     +                 + TAX_TRANSFER_VARIABLES(MO,
     +                        monthly_btl_debit_tax_expense,CLASS_LEVEL)

               FEDERAL_BTL_MISC_DEDUCTIONS(MO) =
     +                 FEDERAL_BTL_MISC_DEDUCTIONS(MO)
     +                 + TAX_VARIABLES(MO,monthly_btl_debit_tax_expense)
     +                 + TAX_TRANSFER_VARIABLES(MO,
     +                        monthly_btl_debit_tax_expense,CLASS_LEVEL)

               TAX_TRANSFER_VARIABLES(MO,
     +                   monthly_atl_debit_tax_expense,CLASS_LEVEL) = 0.
               TAX_TRANSFER_VARIABLES(MO,
     +                   monthly_btl_debit_tax_expense,CLASS_LEVEL) = 0.
            ENDDO
C
C END OF TAXES
C
C            CASH_VARIABLES = 0.
            CALL MONTHLY_CASH_ELIM_REV_EXP(R_YR,INT2(-1),CASH_VARIABLES)
C ELIMINATE NA3 ODEC INCOME STATEMENT TRANSACTIONS
            IF(ODEC() .AND. ODEC_NA3_ACTIVE) THEN
               CASH_VARIABLES(:,Cash_Purchased_Power) =
     +                            CASH_VARIABLES(:,Cash_Purchased_Power)
     +                            + ODEC_NA3_INTRA_BULK_POWER(:)
               CASH_VARIABLES(:,CashBulkPower) =
     +                                 CASH_VARIABLES(:,CashBulkPower)
     +                                 + ODEC_NA3_INTRA_BULK_POWER(:)
            ENDIF
            CASH_VARIABLES(:,cash_property_taxes) =
     +            CASH_VARIABLES(:,cash_mdl_calcd_prop_taxes)  
     +            + CASH_VARIABLES(:,cash_exp_file_prop_taxes)  
            DO VARIABLE = 1, CASH_VARS 

               IF(VARIABLE == cash_st_income_taxes_pd) CYCLE
               IF(VARIABLE == cash_fed_income_taxs_pd) CYCLE
               IF(VARIABLE == cash_parent_taxes_pd) CYCLE
               IF(VARIABLE == cash_subsidiary_tax_pmts) CYCLE
               IF(VARIABLE == cash_subsdry_fed_taxes_pd ) CYCLE
               IF(VARIABLE == cash_subsdry_st_taxes_pd ) CYCLE
               IF(VARIABLE==cash_subsdry_st_taxes_pd_by_pt)
     +                                                             CYCLE
               IF(VARIABLE == cash_consolidate_taxes_pd) CYCLE
               IF(VARIABLE == cash_subsidiary_dividends) THEN
                  CASH_VARIABLES(:,cash_subsidiary_dividends) =
     +                                        UNMODELED_SUB_DIVIDENDS(:)
                  CYCLE
               ENDIF
               IF(VARIABLE == cash_other_taxes) THEN
                  CASH_VARIABLES(:,VARIABLE) = SUB_CASH_OTHER_TAXES(:)
                  CYCLE
               ENDIF
               IF(VARIABLE == cash_equity_to_subsidiaries) CYCLE
               IF(VARIABLE == common_shares_issued) THEN
                  CASH_VARIABLES(:,VARIABLE) =
     +                        CASH_LINKED_LEVEL(:,VARIABLE,CLASS_LEVEL)
     +                        - CASH_VARIABLES(:,VARIABLE)
                  CYCLE
               ENDIF
               IF(VARIABLE == cash_common_dividends) THEN
                  CASH_VARIABLES(:,cash_common_dividends) =
     +                                      PARENT_CASH_DIVIDEND_PAID(:)
                  CYCLE
               ENDIF
               IF(VARIABLE == cash_common_stock_issued) THEN
                  CASH_VARIABLES(:,cash_common_stock_issued) =
     +                                     PARENT_COMMON_STOCK_ISSUED(:)
                  CYCLE
               ENDIF
               CASH_VARIABLES(:,VARIABLE) =
     +                         CASH_LINKED_LEVEL(:,VARIABLE,CLASS_LEVEL)
     +                         - CASH_VARIABLES(:,VARIABLE)
            ENDDO
            CASH_VARIABLES(:,cash_st_income_taxes_pd) =

     +                          + CONSOLIDATED_CASH_STATE_TAXES(:)
            CASH_VARIABLES(:,cash_fed_income_taxs_pd) =
     +                           CONSOLIDATED_VARIABLES(:,
     +                                   cash_fed_income_taxs_pd)
            CONSOLIDATED_VARIABLES(:,cash_st_income_taxes_pd) = 0.
            CONSOLIDATED_VARIABLES(:,cash_fed_income_taxs_pd)=0.
C
C RESUM THE CASH FLOW STATEMENT
C
            DO MO = 0, 12
               CASH_VARIABLES(MO,Cash_TOTAL_OPERATING_RECEIPTS) =
     +            SUM_CASH_RECEIPTS(MO,CASH_VARIABLES)
C 
     +            + CASH_VARIABLES(MO,csh_st_invstmt_incm)
     +            + CASH_VARIABLES(MO,Cash_Operating_Method_Adj)
     +            + CASH_VARIABLES(MO,Cash_Prior_level_Method_Adj)
     +            + CASH_VARIABLES(MO,Cash_Prior_Years_Method_Adj)
     +            + CASH_VARIABLES(MO,cash_investmt_divnd_earngs)
     +            + CASH_VARIABLES(MO,cash_invest_interest_ernings)
C
               CASH_VARIABLES(MO,Cash_TOTAL_EXPENSE_PAYMENTS) =
     +                              SUM_CASH_PAYMENTS(MO,CASH_VARIABLES)
C
               CASH_VARIABLES(MO,net_cash_receipts_or_pmts) =
     +                CASH_VARIABLES(MO,Cash_TOTAL_OPERATING_RECEIPTS) -
     +                CASH_VARIABLES(MO,Cash_TOTAL_EXPENSE_PAYMENTS)
C
C CAPITAIL SERVICE
C
               CASH_VARIABLES(MO,cash_capital_svc_pmts) =
     +                 CASH_VARIABLES(MO,Cash_Interest_on_Notes_Payable)
     +                 + CASH_VARIABLES(MO,cash_ps_dividends)
     +                 + CASH_VARIABLES(MO,cash_ltd_interest)
     +                 + CASH_VARIABLES(MO,cash_std_interest)
     +                 + CASH_VARIABLES(MO,cash_common_dividends)
C
C TAXES 
C
               CASH_VARIABLES(MO,cash_total_tax_payments) = 
     +              CASH_VARIABLES(MO,cash_operating_revenue_tax)
     +              + CASH_VARIABLES(MO,cash_other_taxes)
     +              + CASH_VARIABLES(MO,cash_property_taxes)
     +              + CASH_VARIABLES(MO,cash_st_income_taxes_pd)
     +              + CASH_VARIABLES(MO,cash_state_on_capital)
     +              + CASH_VARIABLES(MO,cash_fed_income_taxs_pd)
     +              + CASH_VARIABLES(MO,cash_fed_tax_on_capital)
C
               CASH_VARIABLES(MO,total_tax_and_other) =
     +                 CASH_VARIABLES(MO,cash_total_tax_payments)
     +                 + CASH_VARIABLES(MO,cash_parent_taxes_pd)
     +                 + CASH_VARIABLES(MO,
     +                       cash_subsdry_st_taxes_pd_by_pt)
     +                 + CASH_VARIABLES(MO,cash_subsidiary_tax_pmts)
     +                 - CASH_VARIABLES(MO,cash_consolidate_taxes_pd)
C
C TOTALS FROM OTHER SOURCES
C
               CASH_VARIABLES(MO,total_fm_other_sources) =
     +                    + CASH_VARIABLES(MO,cash_subsidiary_dividends)
     +                     + CASH_VARIABLES(MO,cash_customer_deposits)
     +                     + CASH_VARIABLES(MO,cash_ciac)
     +                     + CASH_VARIABLES(MO,cash_from_assets_sale)
     +                     + CASH_VARIABLES(MO,
     +                              cash_fm_repaid_issued_notes)
     +                     + CASH_VARIABLES(MO,
     +                                 cash_fm_notes_issd_by_othrs)
C
C CONSTRUCTION
C
               CASH_VARIABLES(MO,cash_total_construction) = 
     +                 SUM_CONSTRUCTION_CASH_PAYMENTS(MO,CASH_VARIABLES)
               CASH_VARIABLES(MO,total_capital_requirements) =
     +            SUM_CASH_TOTAL_CAPITAL_REQUIREMENTS(MO,CASH_VARIABLES)
               CASH_VARIABLES(MO,cash_additions_to_inventories) =
     +                     CASH_VARIABLES(MO,cash_fuel_inventory_chg)
     +                     + CASH_VARIABLES(MO,
     +                              cash_gas_strg_inventry_addtn)
     +                     + CASH_VARIABLES(MO,
     +                                class_matrls_inventry_addition)
               CASH_VARIABLES(MO,funds_fm_operations) =
     +                  CASH_VARIABLES(MO,net_cash_receipts_or_pmts)
     +                - CASH_VARIABLES(MO,cash_capital_svc_pmts)
C    +                - CASH_VARIABLES(MO,cash_total_tax_payments)
     +                - CASH_VARIABLES(MO,total_tax_and_other)
     +                + CASH_VARIABLES(MO,total_fm_other_sources)
               CASH_VARIABLES(MO,funds_change_b4_financing) =
     +                     CASH_VARIABLES(MO,funds_fm_operations)
     +                   - CASH_VARIABLES(MO,total_capital_requirements)
C
C EXTERNAL FINANCING
C
               CASH_VARIABLES(MO,external_financing_pfmd) =
     +                     CASH_VARIABLES(MO,cash_ps_issued)
     +                     + CASH_VARIABLES(MO,cash_ltd_issued)
     +                     + CASH_VARIABLES(MO,cash_std_issued)
     +                     + CASH_VARIABLES(MO,cash_common_stock_issued)
     +                     + CASH_VARIABLES(MO,cash_lease_receipts)
               CASH_VARIABLES(MO,cash_period_change) =
     +               CASH_VARIABLES(MO,funds_change_b4_financing)
     +               + CASH_VARIABLES(MO,external_financing_pfmd)
            ENDDO
            CASH_VARIABLES(0,cash_opening_balance) =
     +                                    CONSOLIDATED_BOY_FUNDS_BALANCE
            CASH_VARIABLES(1,cash_opening_balance) =
     +                                    CONSOLIDATED_BOY_FUNDS_BALANCE
            DO MO = 1, 12
              CASH_VARIABLES(MO,cash_closing_balance) =
     +                         CASH_VARIABLES(MO,cash_opening_balance)
     +                         + CASH_VARIABLES(MO,cash_period_change)
               IF(MO < 12) THEN
                  CASH_VARIABLES(MO+1,cash_opening_balance) =
     +                           CASH_VARIABLES(MO,cash_closing_balance)
               ENDIF
            ENDDO
            CASH_VARIABLES(0,cash_closing_balance) =
     +                           CASH_VARIABLES(12,cash_closing_balance)
C
C FINISH THE INCOME STATEMENT TAX ITEMS
C
            IF(.NOT. LEVELIZE_INCOME_TAXES) THEN

               VOID_LOGICAL=MONTHLY_TAXABLE_INCOME(R_YR,INT2(-1),'C',
     +                                    ATL_TAXABLE_INCOME,
     +                                    INCOME_VARIABLES,
     +                                    CASH_VARIABLES,
     +                                    TAX_VARIABLES,
     +                                    ANNUAL_VARS,
     +                                    STATE_M1_ADDITIONS,
     +                                    STATE_M1_DEDUCTIONS,
     +                                    FEDERAL_M1_ADDITIONS,
     +                                    FEDERAL_M1_DEDUCTIONS,
     +                                    STATE_BTL_MISC_DEDUCTIONS,
     +                                    FEDERAL_BTL_MISC_DEDUCTIONS,
     +                                    MONTHLY_STATE_TAX_RATE,
     +                                    MONTHLY_FEDERAL_TAX_RATE,
     +                                    STATE_DEF_TAX_DR_FROM_NOLS,
     +                                    FED_DEF_TAX_DR_FROM_NOLS_AMT,
     +                                    STATE_INCOME_TAX_ADJUSTMENT,
     +                                    FEDERAL_INCOME_TAX_ADJUSTMENT,
     +                                    STATE_NOLS_USED_BY_SUBS,
     +                                    STATE_NOLS_GEN_BY_SUBS,
     +                                    PARENT_STATE_TAXES,
     +                                    PARENT_STATE_DR_TAX_FROM_NOLS,
     +                                    SUB_STATE_DR_TAX_FROM_NOLS,
     +                                    SUB_STATE_TAXES_PAID,
     +                                    SUB_BTL_TAXABLE_EXPENSES,
     +                                    SUB_BTL_STATE_TAXES_PAID,
     +                                    SUB_BTL_STATE_TAXABLE_INCOME,
     +                                    SUB_BTL_TAXABLE_OTHER_INCOME,
     +                                    SUB_BTL_TAXABLE_INVEST_INCOME,
     +                                    SUB_BTL_MISC_DEDUCTIONS,
     +                                    SUB_BTL_FEDERAL_TAXES_PAID)
C
C STATE TAXES BASED ON TAXABLE INCOME
C
               DO MO = 0, 12
                  INCOME_VARIABLES(MO,
     +                              mthy_state_atl_incm_tax_pd) =
     +                   INCOME_LINKED_LEVEL(MO,
     +                       mthy_state_atl_incm_tax_pd,INT2(-1))

                  INCOME_VARIABLES(MO,
     +                              mt_st_btl_incm_tax_pd) =
     +                   INCOME_LINKED_LEVEL(MO,
     +                       mt_st_btl_incm_tax_pd,INT2(-1))

                  INCOME_VARIABLES(MO,
     +                            mty_tot_st_incm_tax_pd) =
     +                 INCOME_LINKED_LEVEL(MO,
     +                     mty_tot_st_incm_tax_pd,INT2(-1))

                  INCOME_VARIABLES(MO,
     +                            mthy_fed_atl_income_tax_pd) =
     +                                    RPT_FEDERAL_ATL_INCOME_TAX(MO)
                  INCOME_VARIABLES(MO,
     +                              mty_tot_fed_incm_tax_pd) =
     +                                        RPT_FEDERAL_INCOME_TAX(MO)
                  INCOME_VARIABLES(MO,Monthly_Income_Tax_Deferrals_Dr) =
     +                                PARENT_STATE_DR_TAX_FROM_NOLS(MO)
     +                                + SUB_STATE_DR_TAX_FROM_NOLS(MO)
     +                                + CONSOL_DEFERRED_TAXES_DR(MO)
     +                               + FED_DEF_TAX_DR_FROM_NOLS_AMT(MO) ! 147
               ENDDO

            ELSE
               INCOME_VARIABLES(0,mthy_fed_atl_income_tax_pd)=
     +                                                   ANNUAL_VARS(26)

               DO MO = 1, 12
                  INCOME_VARIABLES(MO,
     +                            mthy_fed_atl_income_tax_pd) =
     +                  INCOME_VARIABLES(0,
     +                          mthy_fed_atl_income_tax_pd)/12.
               

               ENDDO
            ENDIF
            VOID_LOGICAL = SUM_MONTHLY_INCOME_REVENUES(INCOME_VARIABLES)
            VOID_LOGICAL = SUM_MONTHLY_INCOME_EXPENSES(INCOME_VARIABLES)
            VOID_LOGICAL = SUM_MONTHLY_TAXES_BTL_ITEMS(INCOME_VARIABLES)
            INCOME_VARIABLES(0,Monthly_Total_Taxes_Expense) =
     +         MONTHLY_TOTAL_EXPENSES_B4_TAXES(INT2(0),INCOME_VARIABLES)
     +         + TOTAL_TAXES_EXPENSE(INT2(0),INCOME_VARIABLES) 
            DO MO = 1, 12
               INCOME_VARIABLES(MO,Monthly_Total_Taxes_Expense) =
     +              MONTHLY_TOTAL_EXPENSES_B4_TAXES(MO,INCOME_VARIABLES)
     +              + TOTAL_TAXES_EXPENSE(MO,INCOME_VARIABLES) 
            ENDDO
C
C
            BALANCE_SHEET_VARIABLES(0,mty_dfrd_income_taxes)=
     +                                             DEFERRED_TAXES_CR_BOY
            BALANCE_SHEET_VARIABLES(0,
     +                               mthly_deferred_income_taxes_dr) =
     +                                             DEFERRED_TAXES_DR_BOY
            BALANCE_SHEET_VARIABLES(0,
     +                            monthly_other_long_term_liabs) =
     +                                          OTHER_LT_LIABILITIES_BOY
            DO VARIABLE = 1, BAL_SHEET_VARS
               IF(VARIABLE == monthly_common_stock) CYCLE
               IF(VARIABLE ==  Monthly_TOTAL_COMMON_EQUITY) CYCLE

               IF(VARIABLE == monthly_gain_on_reaqd_debt_bal) CYCLE
               IF(VARIABLE == mty_regty_liablties_bal) CYCLE

               IF(VARIABLE ==monthly_long_term_debt) THEN
                  BALANCE_SHEET_VARIABLES(0,monthly_long_term_debt) =
     +                                                   BOY_LTD_BALANCE
                  DO MO = 1, 12
                    BALANCE_SHEET_VARIABLES(MO,monthly_long_term_debt)=
     +                  BALANCE_SHEET_VARIABLES(MO-1,
     +                                          monthly_long_term_debt)
     +                  + CASH_VARIABLES(MO,cash_ltd_issued)
     +                  - CASH_VARIABLES(MO,cash_ltd_retirements)
     +                  - INCOME_VARIABLES(MO,Monthly_LTD_Premium_Amort)
                  ENDDO
                  CYCLE
               ENDIF
               IF(VARIABLE == mty_ttl_cptl_wo_ct_ltd) THEN
                  BALANCE_SHEET_VARIABLES(0,VARIABLE) =
     +                                    BOY_LTD_BALANCE_WO_CURRENT_LTD
                  DO MO = 1, 12
                    BALANCE_SHEET_VARIABLES(MO,VARIABLE)=
     +                  BALANCE_SHEET_VARIABLES(MO-1,VARIABLE)
     +                  + CASH_VARIABLES(MO,cash_ltd_issued)
     +                  - CASH_VARIABLES(MO,cash_ltd_retirements)
     +                  - INCOME_VARIABLES(MO,Monthly_LTD_Premium_Amort)
                  ENDDO
                  CYCLE
               ENDIF
               IF(VARIABLE == monthly_retained_earnings_bal) THEN
                  BALANCE_SHEET_VARIABLES(0,
     +                                  monthly_retained_earnings_bal) =
     +                                             BOY_RETAINED_EARNINGS
                  DO MO = 1, 12 
                     BALANCE_SHEET_VARIABLES(MO,
     +                                  monthly_retained_earnings_bal) =
     +                  BALANCE_SHEET_VARIABLES(MO-1,
     +                                    monthly_retained_earnings_bal)
     +                  + INCOME_VARIABLES(MO,Monthly RETAINED EARNINGS)

                  ENDDO
                  CYCLE
               ENDIF
               IF(VARIABLE == monthly_subsidiary_investment) THEN
                  BALANCE_SHEET_VARIABLES(0,
     +                                  monthly_subsidiary_investment) =
     +                                    BOY_INVESTMENT_IN_SUBSIDIARIES
                  DO MO = 1, 12
                     BALANCE_SHEET_VARIABLES(MO,
     +                                  monthly_subsidiary_investment) =
     +                  BALANCE_SHEET_VARIABLES(MO-1,
     +                                    monthly_subsidiary_investment)
     +                  + INCOME_VARIABLES(MO,Monthly_Subsidiary_Income)
     +                  - CASH_VARIABLES(MO,cash_subsidiary_dividends)
                  ENDDO
                  CYCLE
               ENDIF
               IF(VARIABLE == mthly_deferred_income_taxes_dr) THEN
                  IF(BANGOR()) THEN
                     DO MO = 1, 12
                        BALANCE_SHEET_VARIABLES(MO,
     +                               mthly_deferred_income_taxes_dr) =
     +                     BALANCE_SHEET_VARIABLES(MO-1,
     +                                 mthly_deferred_income_taxes_dr)
     +                     + SUB_FASB87_DEF_TAX_DR_ADJ(MO)
                     ENDDO
                  ELSE
                     DO MO = 1, 12
                        BALANCE_SHEET_VARIABLES(MO,
     +                               mthly_deferred_income_taxes_dr) =
     +                     BALANCE_SHEET_VARIABLES(MO-1,
     +                                 mthly_deferred_income_taxes_dr)
     +                     - INCOME_VARIABLES(MO,
     +                                  Monthly_Income_Tax_Deferrals_Dr)
     +                     - INCOME_VARIABLES(MO,
     +                                         BTLMonthlyDeferralTaxDr) 
     +                     + SUB_FASB87_DEF_TAX_DR_ADJ(MO)
                     ENDDO
                  ENDIF
                  CYCLE
               ENDIF
               IF(VARIABLE == monthly_other_investments) THEN
                  BALANCE_SHEET_VARIABLES(0,monthly_other_investments) =
     +                                     OTHER_INVESTMENTS_BALANCE_BOY
                  DO MO = 1, 12
                     BALANCE_SHEET_VARIABLES(MO,
     +                                      monthly_other_investments) =
     +                 BALANCE_SHEET_VARIABLES(MO-1,
     +                                        monthly_other_investments)
     +                 + CASH_VARIABLES(MO,cash_net_investments)
     +                 + INCOME_VARIABLES(MO,MonthlyMarkToMarket)
     +                 + INCOME_VARIABLES(MO,MonthlyEarningNonCompany)
                  ENDDO
                  CYCLE
               ENDIF
               IF(VARIABLE == mty_dfrd_income_taxes) THEN
                  IF(BANGOR()) THEN
                     DO MO = 1, 12
                        BALANCE_SHEET_VARIABLES(MO,
     +                                  mty_dfrd_income_taxes) =
     +                        BALANCE_SHEET_VARIABLES(MO-1,
     +                                    mty_dfrd_income_taxes)
     +                        + INCOME_VARIABLES(MO,
     +                                  Monthly_Income_Tax_Deferrals_Cr)
     +                        + INCOME_VARIABLES(MO,
     +                                 BTL_Monthly_Income_Tax_Deferrals)
     +                        + INCOME_VARIABLES(MO,
     +                                  Monthly_Income_Tax_Deferrals_Dr)
                     ENDDO
                  ELSE
                     DO MO = 1, 12
                        BALANCE_SHEET_VARIABLES(MO,
     +                                  mty_dfrd_income_taxes) =
     +                        BALANCE_SHEET_VARIABLES(MO-1,
     +                                    mty_dfrd_income_taxes)
     +                        + INCOME_VARIABLES(MO,
     +                                  Monthly_Income_Tax_Deferrals_Cr)
     +                        + INCOME_VARIABLES(MO,
     +                                         BTLMonthlyDeferralTaxCr) 
     +                        + NEW_ACQUISITION_DEF_TAXES_CR(MO)
                     ENDDO
                  ENDIF
                  CYCLE
               ENDIF
               IF(VARIABLE == monthly_other_long_term_liabs) THEN
                  CUM_SUB_OFFSET = 0
                  DO MO = 1, 12
                     CUM_SUB_OFFSET = CUM_SUB_OFFSET
     +                                + SUB_FED_DR_TAX_FROM_NOLS_AMT(MO)
                     BALANCE_SHEET_VARIABLES(MO,
     +                            monthly_other_long_term_liabs) =
     +                     BALANCE_SHEET_VARIABLES(MO-1,
     +                              monthly_other_long_term_liabs)

     +                     - CASH_VARIABLES(MO,Cash_Leased_Nuclear_Fuel)! - NUCLEAR_FUEL_LEASE_PAYMENTS
     +                     + CASH_VARIABLES(MO,cash_lease_receipts)     ! + CAPITIALIZED_LEASE_ADDITIONS
     +                     - CASH_VARIABLES(MO,CashLeaseExpense)      ! - ATL_LEASE_PAYMENTS
     +                     - CASH_VARIABLES(MO,Cash_BTL_Lease_Cash)     ! - BTL_LEASE_PAYMENTS

                  ENDDO
                  BALANCE_SHEET_VARIABLES(1:,
     +                            monthly_other_long_term_liabs) =
     +                     BALANCE_SHEET_VARIABLES(1:,
     +                              monthly_other_long_term_liabs) 
     +                     + CONSOL_TRANSFER_LT_LIAB(1:)
                  CYCLE
               ENDIF
               IF(VARIABLE == monthly_accounts_receivable_bal) THEN
                  BALANCE_SHEET_VARIABLES(0,
     +                                monthly_accounts_receivable_bal) =
     +                                           ACCOUNTS_RECEIVABLE_BOY

                  DO MO = 1, 12
                     BALANCE_SHEET_VARIABLES(MO,
     +                                monthly_accounts_receivable_bal) =
     +                BALANCE_SHEET_VARIABLES(MO-1,
     +                                  monthly_accounts_receivable_bal)
     +                + MONTHLY_CHANGE_IN_RECEIVABLES(MO)
     +                + INCOME_VARIABLES(MO,UnbilledRevenues)
     +                + INCOME_VARIABLES(MO,
     +                           mthly_invstmt_earngs_rcvbl)
     +                + INCOME_VARIABLES(MO,
     +                                   BTL_Monthly_STInvestmet_Income)
     +                + INCOME_VARIABLES(MO,
     +                                mty_mdl_lt_nvst_income)
     +                + INCOME_VARIABLES(MO,
     +                             mt_dbt_file_linvst_income)
     +                + INCOME_VARIABLES(MO,
     +                                  monthly_notes_receivable_income) 
     +                - CASH_VARIABLES(MO,
     +                                cash_investmt_divnd_earngs)
     +                - CASH_VARIABLES(MO,
     +                                cash_invest_interest_ernings)
     +                - CASH_VARIABLES(MO,
     +                                csh_st_invstmt_incm)  ! 112

C WVPA RECEIVABLES
     +                + WVPA_OFF_SYSTEM_SALES_RECEIVABLE(MO)
                  ENDDO
                  CYCLE
               ENDIF
               IF(VARIABLE == monthly_accounts_payable_bal) THEN
                  BALANCE_SHEET_VARIABLES(0,
     +                                   monthly_accounts_payable_bal) =
     +                                              ACCOUNTS_PAYABLE_BOY
                  DO MO = 1, 12
                  BALANCE_SHEET_VARIABLES(MO,
     +                                    monthly_accounts_payable_bal)=
     +               BALANCE_SHEET_VARIABLES(MO-1,
     +                                     monthly_accounts_payable_bal)
     +               + NEW_ACQUISITION_ACCOUNTS_PAYABLE_BAL(MO)
C CHANGE IS EXPENSES PAYABLE
     +               + MONTHLY_CHANGE_IN_PAYABLES(MO)
C STD INTEREST
     +               + INCOME_VARIABLES(MO,MonthlySTDInterest) 
     +               - CASH_VARIABLES(MO,cash_std_interest)
C LTD INTEREST
     +               + INCOME_VARIABLES(MO,Monthly_LTD_Booked_Interest)
     +               - CASH_VARIABLES(MO,cash_ltd_interest)
C COMMON DIVIDENDS
     +               + INCOME_VARIABLES(MO,Monthly Common Dividends)
     +               - CASH_VARIABLES(MO,cash_common_dividends)
C PS DIVIDENDS
     +               + INCOME_VARIABLES(MO,Monthly_Booked_PS_Dividends)
     +               - CASH_VARIABLES(MO,cash_ps_dividends)
C NOTES PAYABLE INTEREST
     +               + INCOME_VARIABLES(MO,
     +                                mty_intst_on_nts_pyble)
     +               - CASH_VARIABLES(MO,Cash_Interest_on_Notes_Payable)
C NON_INCOME_TAXES_ACCRUAL_ADJ
     +               + INCOME_VARIABLES(MO,monthly_oth_taxes)
     +               + INCOME_VARIABLES(MO,Monthly_Property_Taxes) ! REMOVE 8.3.04
     +               + INCOME_VARIABLES(MO,
     +                                    Monthly_Operating_Revenue_Tax)
     +               + INCOME_VARIABLES(MO,Monthly_State_Tax_on_Capital)
     +               + INCOME_VARIABLES(MO,
     +                                   Monthly_Federal_Tax_on_Capital)
     +               + INCOME_VARIABLES(MO,
     +                                wvpa_prop_txs_n_pwr_cst)
     +               - CASH_VARIABLES(MO,cash_operating_revenue_tax)  
     +               - CASH_VARIABLES(MO,cash_other_taxes)  
     +               - CASH_VARIABLES(MO,cash_property_taxes)  
     +               - CASH_VARIABLES(MO,cash_state_on_capital)  
     +               - CASH_VARIABLES(MO,cash_fed_tax_on_capital)
C R_STATE_TAXES_ACCRUAL_ADJ
     +               + INCOME_VARIABLES(MO,
     +                              mty_tot_st_incm_tax_pd)
     +               - CASH_VARIABLES(MO,cash_st_income_taxes_pd)
C R_FEDERAL_TAXES_ACCRUAL_ADJ
     +               + INCOME_VARIABLES(MO,
     +                                mty_tot_fed_incm_tax_pd)
C WVPA ADDITIONS
     +               + WVPA_OFF_SYSTEM_SALES_PAYABLE(MO)
     +               + WVPA_INTERNAL_POWER_COSTS_PAYABLE(MO)

     +               - CASH_VARIABLES(MO,cash_fed_income_taxs_pd)
     +               - CONSOLIDATED_VARIABLES(MO,
     +                                   cash_fed_income_taxs_pd)
                  ENDDO
                  CYCLE
               ENDIF
               IF(VARIABLE == monthly_notes_payable .OR.
     +                        VARIABLE == monthly_notes_receivable) THEN
                  BALANCE_SHEET_VARIABLES(:,VARIABLE) =
     +                     BALANCE_LINKED_LEVEL(:,VARIABLE,CLASS_LEVEL)
     +                     + BALANCE_SHEET_VARIABLES(:,VARIABLE)
                  BALANCE_LINKED_LEVEL(:,VARIABLE,CLASS_LEVEL) = 0.
                  CYCLE
               ENDIF
C
C DEFAULT
C
               BALANCE_SHEET_VARIABLES(:,VARIABLE) =
     +                      BALANCE_LINKED_LEVEL(:,VARIABLE,CLASS_LEVEL)
               BALANCE_LINKED_LEVEL(:,VARIABLE,CLASS_LEVEL) = 0.
            ENDDO
C
C CONSOLIDATING ADJUSTMENT USEING BALANCE SHEET VARIABLES
C 
            BALANCE_SHEET_VARIABLES(0,mtly_cwip) = BOY_CWIP
            DO MO = 1, 12 
               BALANCE_SHEET_VARIABLES(MO,mtly_cwip) =
     +                BALANCE_SHEET_VARIABLES(MO-1,mtly_cwip)
     +                + INCOME_VARIABLES(MO,Monthly_AFUDC_Total)
     +                + CASH_VARIABLES(MO,cash_plant_construction)
     +                - BALANCE_SHEET_VARIABLES(MO,
     +                                 monthly_afudc_capitalized)
     +                - BALANCE_SHEET_VARIABLES(MO,
     +                                 monthly_plant_capitalized)
            
            ENDDO
            BALANCE_SHEET_VARIABLES(0,monthly_short_lti) =
     +                            CASH_VARIABLES(0,cash_opening_balance)
            BALANCE_SHEET_VARIABLES(1:,monthly_short_lti) =
     +                           CASH_VARIABLES(1:,cash_closing_balance)
C  monthly_current_assets
            ASSETS_NEC =
     +           BALANCE_SHEET_VARIABLES(:,monthly_current_assets)
            BALANCE_SHEET_VARIABLES(0,monthly_current_assets) =
     +                                              ASSETS_NEC_BOY
            DO MO = 1, 12 
               BALANCE_SHEET_VARIABLES(MO,
     +                                   monthly_current_assets) =
     +                   BALANCE_SHEET_VARIABLES(MO-1,
     +                                     monthly_current_assets)
     +                   +  (ASSETS_NEC(MO) - ASSETS_NEC(MO-1))
            ENDDO
C monthly_net_dfrd_debits
            CALL MNTHLY_DEF_DEBIT_BS_ELIM_INFO(R_YR,
     +                                         ELIMINAITON_CLASS_ID_NUM,
     +                                          BALANCE_SHEET_VARIABLES)
            BALANCE_SHEET_VARIABLES(0,monthly_net_dfrd_debits) =
     +                                               DEFERRED_DEBITS_BOY
               BALANCE_SHEET_VARIABLES(:,mty_net_oth_debits) =
     +            BALANCE_SHEET_VARIABLES(:,monthly_net_dfrd_debits)
     +            - BALANCE_SHEET_VARIABLES(:,monthly_net_goodwill)
     +            - BALANCE_SHEET_VARIABLES(:,
     +                                    monthly_net_regulatory_assets)
     +            - BALANCE_SHEET_VARIABLES(:,mty_net_fasb_109)
     +            - BALANCE_SHEET_VARIABLES(:,mty_net_fasb_133)
     +            - BALANCE_SHEET_VARIABLES(:,
     +                               monthly_unamort_interest_bal) ! 76
     +            - BALANCE_SHEET_VARIABLES(:,
     +                                mthly_unamortized_issue_exp_bal) ! 77

            TOTAL_ASSETS =
     +            SUM_MONTHLY_BALANCE_STATEMENT(BALANCE_SHEET_VARIABLES)
            ODEC_NA3_ACTIVE = .FALSE.
         ENDIF ! end consolidated calculations
C
C OUPUT RESULTS
C
         IF(WRITE_MONTHLY_INFOR) THEN
C
            IF(CPL_ACTIVE() .AND. CLASS_TYPE == CONSOLIDATED) THEN
               CPL_TAX__VARIABLES = 0.
               CALL CPL_TAX_REPORT(ANNUAL_VARS)
            ENDIF
C
C AVERAGE CLOSING COMMON SHARES
C
            INCOME_VARIABLES(0,Monthly_Shares_Outstanding) =
     +          SUM(INCOME_VARIABLES(1:,Monthly_Shares_Outstanding))/12.

            CASH_VARIABLES(:,cash_tot_invest_intrst_earnings) =
     +             CASH_VARIABLES(:,cash_invest_interest_ernings)
     +             + CASH_VARIABLES(:,csh_st_invstmt_incm)
            CASH_VARIABLES(:,cash_tot_aro_payments) =
     +                       CASH_VARIABLES(:,cash_aro_payments)
     +                       + CASH_VARIABLES(:,cash_aro_trust_payments)
            PREVIOUS_BAL_SHEET_INBALANCE = 0.
            QUARTERLY_EARNINGS_PER_SHARE = 0.
            CASH_VARIABLES(:,Cash_Post_Retirement_Payments) =
     +              CASH_VARIABLES(:,Cash_Post_Retirement_Payments)
     +              + CASH_VARIABLES(:,csh_2_post_retiremt_paymentss)
                                    
            INCOME_VARIABLES(:,Monthly_Property_Taxes) =
     +              INCOME_VARIABLES(:,MonthlyExpFilePropertyTaxes)
     +              + INCOME_VARIABLES(:,
     +                          mth_modl_calcd_prop_taxes)
            CASH_VARIABLES(:,cash_property_taxes) =
     +            CASH_VARIABLES(:,cash_mdl_calcd_prop_taxes)  
     +            + CASH_VARIABLES(:,cash_exp_file_prop_taxes)  
C
            DO MO = 0, 12
               IF(CASH_VARIABLES(MO,common_shares_issued) /= 0) THEN
                  CASH_VARIABLES(MO,Price_of_Issued_Shares) =
     +                 (CASH_VARIABLES(MO,cash_common_stock_issued)
     +                  + CASH_VARIABLES(MO,cash_common_stock_buyback))/
     +                         CASH_VARIABLES(MO,common_shares_issued)
               ELSE
                  CASH_VARIABLES(MO,Price_of_Issued_Shares) = 0.
               ENDIF
               IF(UI()) THEN
                  TAX_CASH_RETIREMENT_PAYMENTS =
     +              CASH_VARIABLES(MO,csh_unfundd_retrmt_pymts)
                  TAX_CASH_STORM_PAYMENTS =
     +                            CASH_VARIABLES(MO,Cash_Storm_Payments)
                  TAX_CASH_VACATION_PAYMENTS =
     +                         CASH_VARIABLES(MO,Cash_Vacation_Payments)
                  TAX_BOOK_RETIREMENT_PAYMENTS = 0.
                  TAX_BOOK_STORM_PAYMENTS = 0.
                  TAX_BOOK_VACATION_PAYMENTS = 0.
               ELSE
                  TAX_BOOK_RETIREMENT_PAYMENTS =
     +                      INCOME_VARIABLES(MO,MonthlyPensionExpense)
                  TAX_BOOK_STORM_PAYMENTS =
     +                        INCOME_VARIABLES(MO,MonthlySTORMExpense)
                  TAX_BOOK_VACATION_PAYMENTS =
     +                         INCOME_VARIABLES(MO,monthly_vacation_pay)
                  TAX_CASH_RETIREMENT_PAYMENTS = 0.
                  TAX_CASH_STORM_PAYMENTS = 0.
                  TAX_CASH_VACATION_PAYMENTS = 0.
               ENDIF
               INCOME_VARIABLES(MO,Monthly_Other_Debit_Amort) =
     +                    INCOME_VARIABLES(MO,monthly_other_atl_amort)
     +                    + INCOME_VARIABLES(MO,monthly_other_btl_amort)

C
C ADJUST FED TAXES PAYABLE FOR THE PARENT
C
               IF(CLASS_TYPE == PARENT) THEN
                  CHANGE_IN_ACCURED_FED_TAXES = 
     +               + INCOME_VARIABLES(MO,
     +                                mty_tot_fed_incm_tax_pd)

     +               - CONSOLIDATED_VARIABLES(MO,
     +                                   cash_fed_income_taxs_pd)
               ELSE
                  CHANGE_IN_ACCURED_FED_TAXES = 
     +                  INCOME_VARIABLES(MO,
     +                                mty_tot_fed_incm_tax_pd)
     +                  - CASH_VARIABLES(MO,
     +                                   cash_fed_income_taxs_pd)   ! 108
               ENDIF
C
                INCOME_VARIABLES(MO,Monthly_Earnings_per_Share) =
     +              THE_RATIO_OF_A_TO_B(
     +                   INCOME_VARIABLES(MO,Monthly_Earnings_2_Common),
     +                  INCOME_VARIABLES(MO,Monthly_Shares_Outstanding))
               IF(MO==0) THEN
                  INCOME_VARIABLES(MO,Quarterly Earnings per Share) =
     +                   INCOME_VARIABLES(MO,Monthly_Earnings_per_Share)
               ELSE
                  QUARTERLY_EARNINGS_PER_SHARE =
     +                    INCOME_VARIABLES(MO,Monthly_Earnings_2_Common)
     +                    + QUARTERLY_EARNINGS_PER_SHARE
                  IF(MO==3 .OR. MO==6 .OR. MO==9 .OR. MO==12) THEN
                     INCOME_VARIABLES(MO,Quarterly Earnings per Share) =
     +                  THE_RATIO_OF_A_TO_B(
     +                                     QUARTERLY_EARNINGS_PER_SHARE,
     +                  INCOME_VARIABLES(MO,Monthly_Shares_Outstanding))
                     QUARTERLY_EARNINGS_PER_SHARE = 0.
                  ELSE
                     INCOME_VARIABLES(MO,
     +                         Quarterly Earnings per Share) = NOT_AVAIL
                  ENDIF
               ENDIF
C
               INCOME_VARIABLES(MO,BTL_Monthly_Interest_Income) =
     +              INCOME_VARIABLES(MO,BTL_Monthly_STInvestmet_Income)
     +              + INCOME_VARIABLES(MO,
     +                                mty_mdl_lt_nvst_income)
     +              + INCOME_VARIABLES(MO,
     +                             mt_dbt_file_linvst_income)
     +              + INCOME_VARIABLES(MO,
     +                                  monthly_notes_receivable_income) 
C
               IF(CALCULATE_BTL_INCOME_TAXES) THEN
                  RPT_BTL_TAXABLE_INTEREST_INCOME =
     +                  INCOME_VARIABLES(MO,BTL_Monthly_Interest_Income)   ! 33
                  RPT_BTL_TAXABLE_INVEST_EARNINGS =
     +                  INCOME_VARIABLES(MO,Monthly_Investment_Earnings)   
     +                  - .7*INCOME_VARIABLES(MO,
     +                                     Monthly_Dividend_70_Earnings) ! 34
     +                  - INCOME_VARIABLES(MO,
     +                         mthy_rtrmt_med_fd_rngs) ! 9/28/04 changes
                  RPT_BTL_TAXABLE_LTI_INCOME =
     +                     INCOME_VARIABLES(MO,
     +                                mty_totl_lti_income) ! 35
                  RPT_BTL_TAXABLE_STI_INCOME =
     +                     INCOME_VARIABLES(MO,
     +                                   BTL_Monthly_STInvestmet_Income) ! 36
               ELSE
                  RPT_BTL_TAXABLE_INTEREST_INCOME = 0.
                  RPT_BTL_TAXABLE_INVEST_EARNINGS = 0.
                  RPT_BTL_TAXABLE_LTI_INCOME = 0.
                  RPT_BTL_TAXABLE_STI_INCOME = 0.
               ENDIF
C
C
C EFFECTIVE TAX RATE
C
               TOTAL_INCOME_TAXES = 
     +                  INCOME_VARIABLES(MO,
     +                              mty_tot_st_incm_tax_pd) ! 134
     +                  + INCOME_VARIABLES(MO,
     +                                mty_tot_fed_incm_tax_pd) ! 136
     +                  + INCOME_VARIABLES(MO,
     +                                  Monthly_Income_Tax_Deferrals_Dr) ! 89
     +                  + INCOME_VARIABLES(MO,
     +                                  Monthly_Income_Tax_Deferrals_Cr) ! 90
     +                  + INCOME_VARIABLES(MO,
     +                                 BTL_Monthly_Income_Tax_Deferrals) ! 97
     +                  + INCOME_VARIABLES(MO,Monthly_ITC_Amortization)     ! 83
               ADJUSTED_BOOK_INCOME = 
     +                       INCOME_VARIABLES(MO,Monthly Net Income) ! 25
     +                       + INCOME_VARIABLES(MO,Monthly_AFUDC_Equity)         ! 108
     +                       + INCOME_VARIABLES(MO,
     +                                   BTL_Monthly_NoTax_Other_Income) ! 16

               BOOK_INCOME_B4_TAXES = TOTAL_INCOME_TAXES
     +                                + ADJUSTED_BOOK_INCOME
               EFFECTIVE_TAX_RATE = THE_PERCENT_OF_A_TO_B(
     +                                             TOTAL_INCOME_TAXES,
     +                                             BOOK_INCOME_B4_TAXES)

               CALL MONTHLY_SUMMARY_INCOME_CALCULATIONS(
     +                                                 INCOME_VARIABLES)
C
C NEW SECTION FOR OUTPUT VARIABALES THAT ARE CALCULATED
C
               IF(WVPA()) THEN
                  CALL WVPA_MONTHLY_RISK_VALUES(INCOME_VARIABLES)
               ENDIF
               
               IF(IMPA()) THEN
                  CALL IMPA_REPORT_VALUES(MO,INCOME_VARIABLES,
     +                                    CASH_VARIABLES,
     +                                    OPERATING_METHOD)
               ENDIF
C
               IF(MO == 0) THEN
                  WRITE(INCOME_UNIT,REC=THIS_YEARS_RECORD)
     +                        PRT_ENDPOINT(),
     +                        FLOAT(BASE_YEAR+R_YR),
     +                        ASSET_CLASS_NAME,
     +                        LONG_MONTH_NAMES(MO),
     +                        OUTPUT_CLASS_ID,
     +                       (INCOME_VARIABLES(MO,I),I=1,INCOME_VARS)
                  IF(FirstEnergy())
     +              CAll FE_MONTHLY_PCA_LEGAL_REPORTS(THIS_YEARS_RECORD,
     +                                        MO,FLOAT(BASE_YEAR+R_YR),
     +                                        ASSET_CLASS_NAME,
     +                                        INCOME_VARIABLES,
     +                                        CASH_VARIABLES(MO,151), ! cash variabel 151
     +                                        OUTPUT_CLASS_ID)
                  WRITE(CASH_UNIT,REC=THIS_YEARS_RECORD)
     +                       PRT_ENDPOINT(),
     +                       FLOAT(BASE_YEAR+R_YR),
     +                       ASSET_CLASS_NAME,
     +                       LONG_MONTH_NAMES(MO),
     +                       OUTPUT_CLASS_ID,
     +                      (CASH_VARIABLES(MO,I),I=1,CASH_VARS)
                  WRITE(BALANCE_UNIT,REC=THIS_YEARS_RECORD)
     +                       PRT_ENDPOINT(),
     +                       FLOAT(BASE_YEAR+R_YR),
     +                       ASSET_CLASS_NAME,
     +                       BAL_LONG_MONTH_NAMES(MO),
     +                       OUTPUT_CLASS_ID,
     +                      (BALANCE_SHEET_VARIABLES(MO,I),
     +                                              I=1,BAL_SHEET_VARS),
     +                       INCOME_VARIABLES(MO,Monthly_AFUDC_Total),
     +                       CASH_VARIABLES(MO,cash_plant_construction),
     +                       CHANGE_IN_DEFERRED_DEBIT_BAL(MO),
     +                       INCOME_VARIABLES(MO,
     +                                    Monthly_Issue_Expense_Amorts),
     +                       INCOME_VARIABLES(MO,
     +                                       Monthly_LTD_Premium_Amort), ! 95
     +                       INCOME_VARIABLES(MO,
     +                                        Monthly_PS_Premium_Amort),
     +                       INCOME_VARIABLES(MO,
     +                                     Monthly_Booked_PS_Dividends),
     +                       INCOME_VARIABLES(MO,
     +                                     Monthly_LTD_Booked_Interest),
     +                       MONTHLY_CHANGE_IN_PAYABLES(MO),            ! 99
     +                       MONTHLY_CHANGE_IN_RECEIVABLES(MO),         ! 100
C STD INTEREST
     +                       INCOME_VARIABLES(MO,MonthlySTDInterest) 
     +                       - CASH_VARIABLES(MO,cash_std_interest),    !101
C LTD INTEREST
     +                  INCOME_VARIABLES(MO,Monthly_LTD_Booked_Interest)
     +                  - CASH_VARIABLES(MO,cash_ltd_interest),         !102
C COMMON DIVIDENDS
     +                  INCOME_VARIABLES(MO,Monthly Common Dividends)
     +                  - CASH_VARIABLES(MO,cash_common_dividends),     !103
C PS DIVIDENDS
     +                  INCOME_VARIABLES(MO,Monthly_Booked_PS_Dividends)
     +                  - CASH_VARIABLES(MO,cash_ps_dividends),         !104
C NOTES PAYABLE INTEREST
     +            INCOME_VARIABLES(MO,mty_intst_on_nts_pyble)
     +            - CASH_VARIABLES(MO,Cash_Interest_on_Notes_Payable),  !105
C NON_INCOME_TAXES_ACCRUAL_ADJ
     +             INCOME_VARIABLES(MO,monthly_oth_taxes)
     +             + INCOME_VARIABLES(MO,Monthly_Property_Taxes)
     +             + INCOME_VARIABLES(MO,Monthly_Operating_Revenue_Tax)
     +             + INCOME_VARIABLES(MO,Monthly_State_Tax_on_Capital)
     +             + INCOME_VARIABLES(MO,Monthly_Federal_Tax_on_Capital)
     +             + INCOME_VARIABLES(MO,
     +                                wvpa_prop_txs_n_pwr_cst)
     +             - CASH_VARIABLES(MO,cash_operating_revenue_tax)  
     +             - CASH_VARIABLES(MO,cash_other_taxes)  
     +             - CASH_VARIABLES(MO,cash_property_taxes)  
     +             - CASH_VARIABLES(MO,cash_state_on_capital)  
     +             - CASH_VARIABLES(MO,cash_fed_tax_on_capital),    ! 106
C R_STATE_TAXES_ACCRUAL_ADJ
     +          INCOME_VARIABLES(MO,mty_tot_st_incm_tax_pd)
     +          - CASH_VARIABLES(MO,cash_st_income_taxes_pd),      ! 107
C R_FEDERAL_TAXES_ACCRUAL_ADJ
     +          CHANGE_IN_ACCURED_FED_TAXES,                             ! 108
C BALANCE SHEET BALANCED?
     +          BALANCE_SHEET_VARIABLES(MO,monthly_total_assets)
     +          - BALANCE_SHEET_VARIABLES(MO,monthly_total_liabilities), ! 109
     +          PREVIOUS_BAL_SHEET_INBALANCE,                             ! 110
     +          INCOME_VARIABLES(MO,mthy_common_stock_issue_amort),    ! 111
     +          INCOME_VARIABLES(MO,
     +                           mthly_invstmt_earngs_rcvbl)
     +          + INCOME_VARIABLES(MO,mty_totl_lti_income)
     +          + INCOME_VARIABLES(MO,BTL_Monthly_STInvestmet_Income)
     +          - CASH_VARIABLES(MO,cash_investmt_divnd_earngs)
     +          - CASH_VARIABLES(MO,cash_invest_interest_ernings)
     +          - CASH_VARIABLES(MO,csh_st_invstmt_incm),  ! 112
     +          INCOME_VARIABLES(MO,UnbilledRevenues),    !113
     +          INCOME_VARIABLES(MO,Monthly_ATL_Gain_Amort)
     +          + INCOME_VARIABLES(MO,Monthly_BTL_Gain_Amort), ! 114
     +          WVPA_OFF_SYSTEM_SALES_RECEIVABLE(MO), ! 115
     +          WVPA_OFF_SYSTEM_SALES_PAYABLE(MO), ! 116
     +          WVPA_INTERNAL_POWER_COSTS_PAYABLE(MO) ! 117
C
                  PREVIOUS_BAL_SHEET_INBALANCE =                        
     +              BALANCE_SHEET_VARIABLES(MO,monthly_total_assets)
     +    - BALANCE_SHEET_VARIABLES(MO,monthly_total_liabilities)
C
C END BALANCE SHEET OUTPUT
C
C
                  WRITE(TAX_UNIT,REC=THIS_YEARS_RECORD)
     +                       PRT_ENDPOINT(),
     +                       FLOAT(BASE_YEAR+R_YR),
     +                       ASSET_CLASS_NAME,
     +                       LONG_MONTH_NAMES(MO),
     +                       OUTPUT_CLASS_ID,
     +                       ATL_TAXABLE_INCOME(MO)
     +                       + RPT_BTL_TAXABLE_INCOME(MO),
     +                       ATL_TAXABLE_INCOME(MO),
C
     +               INCOME_VARIABLES(MO,Prior level Method Adjustment),
     +               INCOME_VARIABLES(MO,Operating Method Adjustment),
     +               INCOME_VARIABLES(MO,BaseRates),  ! 5
     +               INCOME_VARIABLES(MO,Residential),
     +               INCOME_VARIABLES(MO,Commercial),
     +               INCOME_VARIABLES(MO,Industrial),
     +               INCOME_VARIABLES(MO,Government),
     +               INCOME_VARIABLES(MO,Lighting), ! 10
     +               INCOME_VARIABLES(MO,Total Base Revenues),
     +               INCOME_VARIABLES(MO,AdjustmentClause),
     +               INCOME_VARIABLES(MO,SecondarySales),
     +               INCOME_VARIABLES(MO,CapacitySales),
     +               INCOME_VARIABLES(MO,BulkPower), ! 15
     +               INCOME_VARIABLES(MO,OtherRevenue),
     +               INCOME_VARIABLES(MO,GasRevenues),
     +               INCOME_VARIABLES(MO,RelationshipRevenues),
     +               INCOME_VARIABLES(MO,MonthlyCatawbaRevenues),
     +               INCOME_VARIABLES(MO,TOTAL OPERATING REVENUES),      ! 20
     +               INCOME_VARIABLES(MO,Prior Years Method Adjustment), ! 21
     +               INCOME_VARIABLES(MO,ICAP_Revs_mth),                 ! 22
     +               INCOME_VARIABLES(MO,TotalDerivativeRevenues),     ! 23
     +               INCOME_VARIABLES(MO,
     +                                  TotalFuelDerivativeRevenues), ! 24
     +               INCOME_VARIABLES(MO,Monthly Net Income),            ! 25
     +               INCOME_VARIABLES(MO,Monthly_AFUDC_Equity),          ! 26
     +               INCOME_VARIABLES(MO,
     +                                  BTL_Monthly_NoTax_Other_Income), ! 27
     +               INCOME_VARIABLES(MO,
     +                                 Monthly_Income_Tax_Deferrals_Dr), ! 28
     +               INCOME_VARIABLES(MO,
     +                                  Monthly_Income_Tax_Deferrals_Cr)
     +                  + INCOME_VARIABLES(MO,
     +                                BTL_Monthly_Income_Tax_Deferrals), ! 29
     +               INCOME_VARIABLES(MO,Monthly_ITC_Amortization),     ! 30
C
C BTL INCOME ITEMS
C
     +               RPT_BTL_OTHER_TAXABLE_INCOME(MO),                  ! 31
C Total Investment Income
     +               RPT_BTL_TAXABLE_INVESTMENT(MO),                    ! 32
     +               RPT_BTL_TAXABLE_INTEREST_INCOME,                   ! 33
     +               RPT_BTL_TAXABLE_INVEST_EARNINGS,                   ! 34
     +               RPT_BTL_TAXABLE_LTI_INCOME,                        ! 35
     +               RPT_BTL_TAXABLE_STI_INCOME,                        ! 36
     +               ANNUAL_VARS(129),     ! CAPITIALIZED_INTEREST/12.  ! 37
     +               INCOME_VARIABLES(MO,PGAAdjustment),               ! 38
     +               (ZERO_POS, I=39, 40),                              ! 39-40
     +               CASH_VARIABLES(MO,cash_net_salvage),               ! 41
     +               TAX_CASH_STORM_PAYMENTS,                           ! 42
     +               TAX_CASH_VACATION_PAYMENTS,                        ! 43
     +               TAX_CASH_RETIREMENT_PAYMENTS,                      ! 44
     +               RPT_NON_BOOK_TAXABLE_ITEMS(MO),                    ! 45
     +               (ZERO_POS, I=46, 49),                              ! 46-49
C TOTAL TAXABLE INCOME
     +               INCOME_VARIABLES(MO,TOTAL OPERATING REVENUES)
     +               + RPT_BTL_OTHER_TAXABLE_INCOME(MO)                  
C Total Investment Income
     +               + RPT_BTL_TAXABLE_INVESTMENT(MO) 
     +               + RPT_NON_BOOK_TAXABLE_ITEMS(MO),                   
C TAXABLE DEDUCTIONS
C EXPENSE ITEMS ARE OFFEST BY  EXP_OFFSET_LINE WHICH IS 30
     +               INCOME_VARIABLES(MO,Monthly_Fossil_Fuel),
     +               INCOME_VARIABLES(MO,
     +                                monthly_nuclr_fuel_tax_expns),  ! 52
     +               INCOME_VARIABLES(MO,MonthlyDOEDecommissioning),
     +               INCOME_VARIABLES(MO,MonthlyDOEDisposal),
     +               INCOME_VARIABLES(MO,monthly_purchased_power),
     +               INCOME_VARIABLES(MO,monthly_variable_oandm),
     +               INCOME_VARIABLES(MO,monthly_fixed_oandm),
     +               INCOME_VARIABLES(MO,MonthlyServiceTransactions),
     +               INCOME_VARIABLES(MO,monthly_other_oandm),
     +               INCOME_VARIABLES(MO,monthly_purchased_gas),         ! 60
     +               INCOME_VARIABLES(MO,Monthly Other),
     +               INCOME_VARIABLES(MO,
     +                                  MonthlyTransmissionOperation),
     +               INCOME_VARIABLES(MO,
     +                                monthly_xmsn_maintnc),
     +               INCOME_VARIABLES(MO,
     +                                  MonthlyDistributionOperation),
     +               INCOME_VARIABLES(MO,
     +                                MonthlyDistributionMaintenance),
     +               INCOME_VARIABLES(MO,MonthlyCustomerAccounts),
     +               INCOME_VARIABLES(MO,MonthlyCustomerServices),
     +               INCOME_VARIABLES(MO,MonthlySalesExpense),
     +               INCOME_VARIABLES(MO,MonthlyAGOperations),
     +               INCOME_VARIABLES(MO,MonthlyAGMaintenance),
     +              INCOME_VARIABLES(MO,Monthly_Lease_Interest_Expense), ! 71
     +               INCOME_VARIABLES(MO,MonthlyDSMExpense),
     +               INCOME_VARIABLES(MO,MonthlyDSMRebate),
     +               INCOME_VARIABLES(MO,MonthlyEmissionCredits),
     +               INCOME_VARIABLES(MO,MonthlyCatawbaExpenses),      ! 75
     +               TAX_BOOK_RETIREMENT_PAYMENTS,                       ! 76
     +               TAX_BOOK_STORM_PAYMENTS,                            ! 77
     +               TAX_BOOK_VACATION_PAYMENTS,                         ! 78
     +               INCOME_VARIABLES(MO,TotalDerivativeExpenses),      ! 79
     +               INCOME_VARIABLES(MO,Monthly_Retirement_Expense),    ! 80
C BTL ITEMS ARE NOT OFFSET
     +               INCOME_VARIABLES(MO,MonthlySTDInterest),          ! 81
     +               INCOME_VARIABLES(MO,
     +                               mty_intst_on_nts_pyble), ! 82
     +               TAX_VARIABLES(MO,monthly_ltd_tax_deduction),        ! 83
     +               INCOME_VARIABLES(MO,Monthly_LTD_Total_Interest)     
     +               - INCOME_VARIABLES(MO,Monthly_LTD_Amort_Interest),  ! 84
     +               INCOME_VARIABLES(MO,monthly_oth_taxes),           ! 85
     +               INCOME_VARIABLES(MO,Monthly_Property_Taxes),        ! 86
     +               INCOME_VARIABLES(MO,Monthly_Operating_Revenue_Tax), ! 87
     +               INCOME_VARIABLES(MO,Monthly_State_Tax_on_Capital),  ! 88
     +               INCOME_VARIABLES(MO,Monthly_LTD_Amort_Interest),    ! 89
     +              INCOME_VARIABLES(MO,TotalFuelDerivativeExpenses), ! 90
C TAKE DEDUCTIONS
     +               INCOME_TAX_DEPRECIATION,                            ! 91
     +               ANNUAL_VARS(228),     ! CONSTRUCTION_TAX_EXPENSE/12.! 92
     +               ANNUAL_VARS(216),     ! ENVIRONMENTAL_TAX/12.       ! 93
C
     +               RPT_BTL_TAXABLE_INCOME(MO),                         ! 94
     +               RPT_BTL_TAXABLE_EXPENSES(MO),                       ! 95
     +               BTL_MISC_DEDUCTIONS,                                ! 96
     +               RPT_DEDUCTIBLE_BOOK_EXPENSES(MO),                   ! 97
     +               RPT_TOTAL_DEDUCT_BOOK_EXPENSES(MO),                 ! 98
     +               RPT_TAXABLE_INCOME_B4_DEDUCTS(MO),                  ! 99
     +               CASH_VARIABLES(MO,cash_ltd_ps_issue_expense),       ! 100
     +               CASH_VARIABLES(MO,cash_common_issue_expense),       ! 101
     +               (ZERO_POS, I=102, 110),                             ! 102-110
     +               STATE_M1_ADDITIONS(MO),                             ! 111
     +               STATE_M1_DEDUCTIONS(MO),                            ! 112
     +               FEDERAL_M1_ADDITIONS(MO),                           ! 113
     +               FEDERAL_M1_DEDUCTIONS(MO),                          ! 114
     +               STATE_BTL_MISC_DEDUCTIONS(MO),                      ! 115
     +               FEDERAL_BTL_MISC_DEDUCTIONS(MO),                    ! 116
     +               RPT_STATE_TAX_INCOME_B4_NOLS(MO),                   ! 117
     +               RPT_STATE_TAXABLE_INCOME(MO),                       ! 118
     +               RPT_STATE_INCOME_TAX_B4_CREDITS(MO),                ! 119
     +               RPT_FED_TAX_INCOME_B4_NOLS(MO),                     ! 120
     +               RPT_FEDERAL_TAXABLE_INCOME(MO),                     ! 121
     +               RPT_FED_INCOME_TAX_B4_CREDITS(MO),                  ! 122
     +               100.*MONTHLY_STATE_TAX_RATE(MO),                    ! 123
     +               100.*MONTHLY_FEDERAL_TAX_RATE(MO),                  ! 124
     +               RPT_STATE_BTL_TAXABLE_INCOME(MO),                   ! 125
     +               RPT_STATE_BTL_INCOME_TAX(MO),                       ! 126
     +               INCOME_VARIABLES(MO,
     +                               mthy_state_atl_incm_tax_pd), ! 127
     +               RPT_FEDERAL_BTL_TAXABLE_INCOME(MO),                 ! 128
     +               RPT_FEDERAL_BTL_INCOME_TAX(MO),                     ! 129
     +               INCOME_VARIABLES(MO,
     +                             mthy_fed_atl_income_tax_pd), ! 130
     +               INCOME_VARIABLES(MO,
     +                              mty_tot_st_incm_tax_pd)
     +               + INCOME_VARIABLES(MO,
     +                               mty_tot_fed_incm_tax_pd), ! 131
     +               RPT_STATE_BTL_INCOME_TAX(MO)
     +               + RPT_FEDERAL_BTL_INCOME_TAX(MO),                   ! 132
     +               INCOME_VARIABLES(MO,
     +                              mty_tot_st_incm_tax_pd) 
     +               - RPT_STATE_INCOME_TAX_B4_CREDITS(MO)
     +               + RPT_STATE_GENERAL_CREDITS(MO),                    ! 133
     +               INCOME_VARIABLES(MO,
     +                             mty_tot_st_incm_tax_pd), ! 134
     +               INCOME_VARIABLES(MO,
     +                                mty_tot_fed_incm_tax_pd)
     +                  - RPT_FED_INCOME_TAX_B4_CREDITS(MO)                
     +                  + RPT_FEDERAL_SEC29_CREDITS(MO)                    
     +                  + RPT_FEDERAL_GENERAL_CREDITS(MO)                  
     +                  + RPT_FEDERAL_AMT_CREDITS_USED(MO)                 
     +                  - RPT_FED_AMT_CREDITS_GENERATED(MO),             ! 135
     +               INCOME_VARIABLES(MO,
     +                               mty_tot_fed_incm_tax_pd), ! 136
     +               RPT_STATE_NOLS_USED(MO),                            ! 137
     +               RPT_FEDERAL_NOLS_USED(MO),                          ! 138
     +               RPT_STATE_NOLS_GENERATED(MO),                       ! 139
     +               RPT_FEDERAL_NOLS_GENERATED(MO),                     ! 140
     +               RPT_STATE_GENERAL_CREDITS(MO),                      ! 141
     +               RPT_FEDERAL_SEC29_CREDITS(MO),                      ! 142
     +               RPT_FEDERAL_GENERAL_CREDITS(MO),                    ! 143
     +               RPT_FEDERAL_AMT_CREDITS_USED(MO),                   ! 144
     +               RPT_FED_AMT_CREDITS_GENERATED(MO),                  ! 145
     +               STATE_DEF_TAX_DR_FROM_NOLS(MO),                     ! 146
     +               FED_DEF_TAX_DR_FROM_NOLS_AMT(MO),                   ! 147
     +               (TAX_VARIABLES(MO,I),I=1,TAX_VARS),                 ! 148-197
C
C OTHER TAXES SECTION
C
     +               RPT_CLASS_OPERATING_REVENUES(MO),                   ! 198
     +               RPT_EXCLUDED_OTHER_TAXES_REV(MO,CLASS_LEVEL),       ! 199
     +               RPT_CLASS_NON_VARIABLE_EXPENSE(MO),                 ! 200
     +               RPT_EXCLUDED_OTHER_TAXES_EXP(MO,CLASS_LEVEL),       ! 201
     +               RPT_OTHER_TAXES_MOVING_UP(MO,CLASS_LEVEL),          ! 202
     +               OTHER_TAX_ADJUSTMENT(MO),                           ! 203
     +               MONTHLY_OTHER_REV_TAX_RATE(MO)*100.,                ! 204 
     +               MONTHLY_OTHER_EXP_TAX_RATE(MO)*100.,                ! 205
C
C REVENUE TAXES SECTION
C
     +                RPT_CLASS_REVENUE_TAX_REVENUES(MO),                ! 206
     +                RPT_CLASS_REV_TAX_EXCLUDED_REVS(MO),               ! 207
     +                RPT_REVENUE_TAXES_MOVING_UP(MO,CLASS_LEVEL),       ! 208
     +                REVENUE_TAX_ADJUSTMENT(MO),                        ! 209
     +                MONTHLY_REVENUE_TAX_RATE(MO)*100.,                 ! 210
C
C EFFECTIVE TAX RATE
C
     +                BOOK_INCOME_B4_TAXES,                              ! 211
     +                TOTAL_INCOME_TAXES,                                ! 212
     +                EFFECTIVE_TAX_RATE,                                ! 213
     +                ADJUSTED_BOOK_INCOME,                              ! 214
     + INCOME_VARIABLES(MO, mty_xpns_rsv_mgn_cap_pchs), ! 215
     +                INCOME_VARIABLES(MO,
     +                    mthy_incm_rsv_mgn_cap_sales), ! 216
     +                INCOME_VARIABLES(MO,MonthlyOtherATLExpenses)       ! 217
     
     
C

               ELSEIF(R_YR == DEACTIVE_YR .AND. MO >= MO_DEACT) THEN
                  CALL MONTHLY_ZERO_BALANCE_YR(MO,R_YR,ASSET_CLASS_NAME,
     +                                                  OUTPUT_CLASS_ID)
               ELSE
                  ENVIROMENTAL_TAX = 0.
                  IF(MO==12) ENVIROMENTAL_TAX = ANNUAL_VARS(216)
C
                  WRITE(INCOME_UNIT,REC=THIS_YEARS_RECORD+MO)
     +                        PRT_ENDPOINT(),
     +                        FLOAT(BASE_YEAR+R_YR),
     +                        ASSET_CLASS_NAME,
     +                        LONG_MONTH_NAMES(MO),
     +                        OUTPUT_CLASS_ID,
     +                       (INCOME_VARIABLES(MO,I),I=1,INCOME_VARS)
                  IF(FirstEnergy())
     +              CAll FE_MONTHLY_PCA_LEGAL_REPORTS(
     +                                        THIS_YEARS_RECORD+MO,
     +                                        MO,FLOAT(BASE_YEAR+R_YR),
     +                                        ASSET_CLASS_NAME,
     +                                        INCOME_VARIABLES,
     +                                        CASH_VARIABLES(MO,151), ! cash variabel 151
     +                                        OUTPUT_CLASS_ID)
                  WRITE(CASH_UNIT,REC=THIS_YEARS_RECORD+MO)
     +                       PRT_ENDPOINT(),
     +                       FLOAT(BASE_YEAR+R_YR),
     +                       ASSET_CLASS_NAME,
     +                       LONG_MONTH_NAMES(MO),
     +                       OUTPUT_CLASS_ID,
     +                      (CASH_VARIABLES(MO,I),I=1,CASH_VARS)
                  WRITE(BALANCE_UNIT,REC=THIS_YEARS_RECORD+MO)
     +                       PRT_ENDPOINT(),
     +                       FLOAT(BASE_YEAR+R_YR),
     +                       ASSET_CLASS_NAME,
     +                       BAL_LONG_MONTH_NAMES(MO),
     +                       OUTPUT_CLASS_ID,
     +                      (BALANCE_SHEET_VARIABLES(MO,I),
     +                                              I=1,BAL_SHEET_VARS),
     +                       INCOME_VARIABLES(MO,Monthly_AFUDC_Total),
     +                       CASH_VARIABLES(MO,cash_plant_construction),
     +                       CHANGE_IN_DEFERRED_DEBIT_BAL(MO),
     +                       INCOME_VARIABLES(MO,
     +                                    Monthly_Issue_Expense_Amorts),
     +                       INCOME_VARIABLES(MO,
     +                                       Monthly_LTD_Premium_Amort), ! 95
     +                       INCOME_VARIABLES(MO,
     +                                        Monthly_PS_Premium_Amort),
     +                       INCOME_VARIABLES(MO,
     +                                     Monthly_Booked_PS_Dividends),
     +                       INCOME_VARIABLES(MO,
     +                                     Monthly_LTD_Booked_Interest),
     +                       MONTHLY_CHANGE_IN_PAYABLES(MO),
     +                       MONTHLY_CHANGE_IN_RECEIVABLES(MO),
C STD INTEREST
     +                       INCOME_VARIABLES(MO,MonthlySTDInterest) 
     +                       - CASH_VARIABLES(MO,cash_std_interest),    !101
C LTD INTEREST
     +                  INCOME_VARIABLES(MO,Monthly_LTD_Booked_Interest)
     +                  - CASH_VARIABLES(MO,cash_ltd_interest),         !102
C COMMON DIVIDENDS
     +                  INCOME_VARIABLES(MO,Monthly Common Dividends)
     +                  - CASH_VARIABLES(MO,cash_common_dividends),     !103
C PS DIVIDENDS
     +                  INCOME_VARIABLES(MO,Monthly_Booked_PS_Dividends)
     +                  - CASH_VARIABLES(MO,cash_ps_dividends),         !104
C NOTES PAYABLE INTEREST
     +            INCOME_VARIABLES(MO,mty_intst_on_nts_pyble)
     +            - CASH_VARIABLES(MO,Cash_Interest_on_Notes_Payable),  !105
C NON_INCOME_TAXES_ACCRUAL_ADJ
     +             INCOME_VARIABLES(MO,monthly_oth_taxes)
     +             + INCOME_VARIABLES(MO,Monthly_Property_Taxes)
     +             + INCOME_VARIABLES(MO,Monthly_Operating_Revenue_Tax)
     +             + INCOME_VARIABLES(MO,Monthly_State_Tax_on_Capital)
     +             + INCOME_VARIABLES(MO,Monthly_Federal_Tax_on_Capital)
     +             + INCOME_VARIABLES(MO,
     +                                wvpa_prop_txs_n_pwr_cst)
     +             - CASH_VARIABLES(MO,cash_operating_revenue_tax)  
     +             - CASH_VARIABLES(MO,cash_other_taxes)  
     +             - CASH_VARIABLES(MO,cash_property_taxes)  
     +             - CASH_VARIABLES(MO,cash_state_on_capital)  
     +             - CASH_VARIABLES(MO,cash_fed_tax_on_capital),    ! 106
C R_STATE_TAXES_ACCRUAL_ADJ
     +          INCOME_VARIABLES(MO,mty_tot_st_incm_tax_pd)
     +          - CASH_VARIABLES(MO,cash_st_income_taxes_pd),      ! 107
C R_FEDERAL_TAXES_ACCRUAL_ADJ
     +          CHANGE_IN_ACCURED_FED_TAXES,                            ! 108
C BALANCE SHEET BALANCED?
     +          BALANCE_SHEET_VARIABLES(MO,monthly_total_assets)
     +          - BALANCE_SHEET_VARIABLES(MO,monthly_total_liabilities), ! 109
     +          BALANCE_SHEET_VARIABLES(MO,monthly_total_assets)
     +          - BALANCE_SHEET_VARIABLES(MO,monthly_total_liabilities)
     +          - PREVIOUS_BAL_SHEET_INBALANCE,                           ! 110
     +          INCOME_VARIABLES(MO,mthy_common_stock_issue_amort),     ! 111

     +          INCOME_VARIABLES(MO,
     +                           mthly_invstmt_earngs_rcvbl)
     +          + INCOME_VARIABLES(MO,mty_totl_lti_income)
     +          + INCOME_VARIABLES(MO,BTL_Monthly_STInvestmet_Income)
     +          - CASH_VARIABLES(MO,cash_investmt_divnd_earngs)
     +          - CASH_VARIABLES(MO,cash_invest_interest_ernings) 
     +          - CASH_VARIABLES(MO,csh_st_invstmt_incm),          ! 112
     +          INCOME_VARIABLES(MO,UnbilledRevenues),    !113
     +          INCOME_VARIABLES(MO,Monthly_ATL_Gain_Amort)
     +          + INCOME_VARIABLES(MO,Monthly_BTL_Gain_Amort), ! 114
     +          WVPA_OFF_SYSTEM_SALES_RECEIVABLE(MO), ! 115
     +          WVPA_OFF_SYSTEM_SALES_PAYABLE(MO), ! 116
     +          WVPA_INTERNAL_POWER_COSTS_PAYABLE(MO) ! 117
C
                 PREVIOUS_BAL_SHEET_INBALANCE =                            ! 110
     +           BALANCE_SHEET_VARIABLES(MO,monthly_total_assets)
     +  - BALANCE_SHEET_VARIABLES(MO,monthly_total_liabilities)

C
C
C
                  WRITE(TAX_UNIT,REC=THIS_YEARS_RECORD+MO)
     +               PRT_ENDPOINT(),
     +               FLOAT(BASE_YEAR+R_YR),
     +               ASSET_CLASS_NAME,
     +               LONG_MONTH_NAMES(MO),
     +               OUTPUT_CLASS_ID,
     +               ATL_TAXABLE_INCOME(MO)
     +               + RPT_BTL_TAXABLE_INCOME(MO),  ! 1
     +               ATL_TAXABLE_INCOME(MO),
C
     +               INCOME_VARIABLES(MO,Prior level Method Adjustment),
     +               INCOME_VARIABLES(MO,Operating Method Adjustment),
     +               INCOME_VARIABLES(MO,BaseRates),  ! 5
     +               INCOME_VARIABLES(MO,Residential),
     +               INCOME_VARIABLES(MO,Commercial),
     +               INCOME_VARIABLES(MO,Industrial),
     +               INCOME_VARIABLES(MO,Government),
     +               INCOME_VARIABLES(MO,Lighting), ! 10
     +               INCOME_VARIABLES(MO,Total Base Revenues),
     +               INCOME_VARIABLES(MO,AdjustmentClause),
     +               INCOME_VARIABLES(MO,SecondarySales),
     +               INCOME_VARIABLES(MO,CapacitySales),
     +               INCOME_VARIABLES(MO,BulkPower), ! 15
     +               INCOME_VARIABLES(MO,OtherRevenue),
     +               INCOME_VARIABLES(MO,GasRevenues),
     +               INCOME_VARIABLES(MO,RelationshipRevenues),
     +               INCOME_VARIABLES(MO,MonthlyCatawbaRevenues),
     +               INCOME_VARIABLES(MO,TOTAL OPERATING REVENUES),      ! 20
     +               INCOME_VARIABLES(MO,Prior Years Method Adjustment), ! 21
     +               INCOME_VARIABLES(MO,ICAP_Revs_mth),
     +               INCOME_VARIABLES(MO,TotalDerivativeRevenues),     ! 23
     +               INCOME_VARIABLES(MO,
     +                                  TotalFuelDerivativeRevenues), ! 24
     +               INCOME_VARIABLES(MO,Monthly Net Income),            ! 25
     +               INCOME_VARIABLES(MO,Monthly_AFUDC_Equity),          ! 26
     +               INCOME_VARIABLES(MO,
     +                                  BTL_Monthly_NoTax_Other_Income), ! 27
     +               INCOME_VARIABLES(MO,
     +                                 Monthly_Income_Tax_Deferrals_Dr) 
     +                   + INCOME_VARIABLES(MO,BTLMonthlyDeferralTaxCr),  ! 28
     +               INCOME_VARIABLES(MO,
     +                                  Monthly_Income_Tax_Deferrals_Cr)
     +                   + INCOME_VARIABLES(MO,BTLMonthlyDeferralTaxCr),  ! 29
     +               INCOME_VARIABLES(MO,Monthly_ITC_Amortization),     ! 30
C
C BTL INCOME ITEMS
C
     +               RPT_BTL_OTHER_TAXABLE_INCOME(MO),                  ! 31
C Total Investment Income
     +               RPT_BTL_TAXABLE_INVESTMENT(MO),                    ! 32
     +               RPT_BTL_TAXABLE_INTEREST_INCOME,                   ! 33
     +               RPT_BTL_TAXABLE_INVEST_EARNINGS,                   ! 34
     +               RPT_BTL_TAXABLE_LTI_INCOME,                        ! 35
     +               RPT_BTL_TAXABLE_STI_INCOME,                        ! 36
     +               ANNUAL_VARS(129)/12., ! CAPITIALIZED_INTEREST/12.  ! 37
     +               INCOME_VARIABLES(MO,PGAAdjustment),               ! 38
     +               INCOME_VARIABLES(MO,
     +                                BTL_Monthly_Income_Tax_Deferrals), ! 39
     +               ZERO_POS,                                          ! 40
     +               CASH_VARIABLES(MO,cash_net_salvage),               ! 41
     +               TAX_CASH_STORM_PAYMENTS,                           ! 42
     +               TAX_CASH_VACATION_PAYMENTS,                        ! 43
     +               TAX_CASH_RETIREMENT_PAYMENTS,                      ! 44
     +               RPT_NON_BOOK_TAXABLE_ITEMS(MO),                    ! 45
     +               (ZERO_POS, I=46, 49),                              ! 46-49
C TOTAL TAXABLE INCOME
     +               INCOME_VARIABLES(MO,TOTAL OPERATING REVENUES)
     +               + RPT_BTL_OTHER_TAXABLE_INCOME(MO)                  
C Total Investment Income
     +               + RPT_BTL_TAXABLE_INVESTMENT(MO) 
     +               + RPT_NON_BOOK_TAXABLE_ITEMS(MO),                   

C TAXABLE DEDUCTIONS
C EXPENSE ITEMS ARE OFFEST BY  EXP_OFFSET_LINE WHICH IS 30
     +               INCOME_VARIABLES(MO,FossilFuel+ EXP_OFFSET_LINE),  ! 51
     +               INCOME_VARIABLES(MO,
     +                                monthly_nuclr_fuel_tax_expns), ! 52
     +               INCOME_VARIABLES(MO,DOEDecommissioning
     +                                               + EXP_OFFSET_LINE), ! 53
     +               INCOME_VARIABLES(MO,DOEDisposal+ EXP_OFFSET_LINE), ! 54
     +               INCOME_VARIABLES(MO,PurchasedPower
     +                                               + EXP_OFFSET_LINE), ! 55
     +               INCOME_VARIABLES(MO,VariableOandM
     +                                               + EXP_OFFSET_LINE), ! 56
     +               INCOME_VARIABLES(MO,FixedOandM+ EXP_OFFSET_LINE),  ! 57
     +               INCOME_VARIABLES(MO,ServiceTransactions
     +                                              + EXP_OFFSET_LINE),  ! 58
     +               INCOME_VARIABLES(MO,OtherOandM+ EXP_OFFSET_LINE),  ! 59
     +               INCOME_VARIABLES(MO,PurchasedGas
     +                                               + EXP_OFFSET_LINE), ! 60
     +               INCOME_VARIABLES(MO,Other+ EXP_OFFSET_LINE),        ! 61
     +               INCOME_VARIABLES(MO,TransmissionOperation
     +                                               + EXP_OFFSET_LINE),
     +               INCOME_VARIABLES(MO,TransmissionMaintenance
     +                                               + EXP_OFFSET_LINE),
     +               INCOME_VARIABLES(MO,DistributionOperation
     +                                               + EXP_OFFSET_LINE),
     +               INCOME_VARIABLES(MO,DistributionMaintenance
     +                                               + EXP_OFFSET_LINE), ! 65
     +               INCOME_VARIABLES(MO,CustomerAccounts
     +                                               + EXP_OFFSET_LINE), ! 66
     +               INCOME_VARIABLES(MO,CustomerServices
     +                                               + EXP_OFFSET_LINE),
     +               INCOME_VARIABLES(MO,SalesExpense
     +                                               + EXP_OFFSET_LINE),
     +               INCOME_VARIABLES(MO,AGOperations
     +                                               + EXP_OFFSET_LINE), 
     +               INCOME_VARIABLES(MO,AGMaintenance
     +                                               + EXP_OFFSET_LINE), ! 70
     +              INCOME_VARIABLES(MO,Monthly_Lease_Interest_Expense), ! 71
     +               INCOME_VARIABLES(MO,DSMExpense+ EXP_OFFSET_LINE),
     +               INCOME_VARIABLES(MO,DSMRebate+ EXP_OFFSET_LINE),
     +               INCOME_VARIABLES(MO,MonthlyEmissionCredits),
     +               INCOME_VARIABLES(MO,CatawbaExpenses
     +                                               + EXP_OFFSET_LINE), ! 75
     +               TAX_BOOK_RETIREMENT_PAYMENTS,                       ! 76
     +               TAX_BOOK_STORM_PAYMENTS,                            ! 77
     +               TAX_BOOK_VACATION_PAYMENTS,                         ! 78
     +               INCOME_VARIABLES(MO,TotalDerivativeExpenses),     ! 79
     +               INCOME_VARIABLES(MO,Monthly_Retirement_Expense),    ! 80
C BTL ITEMS ARE NOT OFFSET
     +               INCOME_VARIABLES(MO,MonthlySTDInterest),          ! 81
     +               INCOME_VARIABLES(MO,
     +                               mty_intst_on_nts_pyble), ! 82
     +               TAX_VARIABLES(MO,monthly_ltd_tax_deduction),        ! 83
     +               INCOME_VARIABLES(MO,Monthly_LTD_Total_Interest)     
     +               - INCOME_VARIABLES(MO,Monthly_LTD_Amort_Interest),  ! 84
     +               INCOME_VARIABLES(MO,monthly_oth_taxes),           ! 85
     +               INCOME_VARIABLES(MO,Monthly_Property_Taxes),        ! 86
     +               INCOME_VARIABLES(MO,Monthly_Operating_Revenue_Tax), ! 87
     +               INCOME_VARIABLES(MO,Monthly_State_Tax_on_Capital),  ! 88
     +               INCOME_VARIABLES(MO,Monthly_LTD_Amort_Interest),    ! 89
     +              INCOME_VARIABLES(MO,TotalFuelDerivativeExpenses), ! 90
C TAKE DEDUCTIONS
     +               INCOME_TAX_DEPRECIATION/12.,                        ! 91
     +               ANNUAL_VARS(228)/12., ! CONSTRUCTION_TAX_EXPENSE/12.! 92
     +               ENVIROMENTAL_TAX,                                   ! 93
C
     +               RPT_BTL_TAXABLE_INCOME(MO),                         ! 94
     +               RPT_BTL_TAXABLE_EXPENSES(MO),                       ! 95
     +               BTL_MISC_DEDUCTIONS/12.,                            ! 96
     +               RPT_DEDUCTIBLE_BOOK_EXPENSES(MO),                   ! 97
     +               RPT_TOTAL_DEDUCT_BOOK_EXPENSES(MO),                 ! 98
     +               RPT_TAXABLE_INCOME_B4_DEDUCTS(MO),                  ! 99
     +               CASH_VARIABLES(MO,cash_ltd_ps_issue_expense),       ! 100
     +               CASH_VARIABLES(MO,cash_common_issue_expense),       ! 101
     +               (ZERO_POS, I=102, 110),                             ! 102-110
     +               STATE_M1_ADDITIONS(MO),                             ! 111
     +               STATE_M1_DEDUCTIONS(MO),                            ! 112
     +               FEDERAL_M1_ADDITIONS(MO),                           ! 113
     +               FEDERAL_M1_DEDUCTIONS(MO),                          ! 114
     +               STATE_BTL_MISC_DEDUCTIONS(MO),                      ! 115
     +               FEDERAL_BTL_MISC_DEDUCTIONS(MO),                    ! 116
     +               RPT_STATE_TAX_INCOME_B4_NOLS(MO),                   ! 117
     +               RPT_STATE_TAXABLE_INCOME(MO),                       ! 118
     +               RPT_STATE_INCOME_TAX_B4_CREDITS(MO),                ! 119
     +               RPT_FED_TAX_INCOME_B4_NOLS(MO),                     ! 120
     +               RPT_FEDERAL_TAXABLE_INCOME(MO),                     ! 121
     +               RPT_FED_INCOME_TAX_B4_CREDITS(MO),                  ! 122
     +               100.*MONTHLY_STATE_TAX_RATE(MO),                    ! 123
     +               100.*MONTHLY_FEDERAL_TAX_RATE(MO),                  ! 124
     +               RPT_STATE_BTL_TAXABLE_INCOME(MO),                   ! 125
     +               RPT_STATE_BTL_INCOME_TAX(MO),                       ! 126
     +               INCOME_VARIABLES(MO,
     +                               mthy_state_atl_incm_tax_pd), ! 127
     +               RPT_FEDERAL_BTL_TAXABLE_INCOME(MO),                 ! 128
     +               RPT_FEDERAL_BTL_INCOME_TAX(MO),                     ! 129
     +               INCOME_VARIABLES(MO,
     +                             mthy_fed_atl_income_tax_pd), ! 130
     +               INCOME_VARIABLES(MO,
     +                              mty_tot_st_incm_tax_pd)
     +               + INCOME_VARIABLES(MO,
     +                               mty_tot_fed_incm_tax_pd), ! 131
     +               RPT_STATE_BTL_INCOME_TAX(MO)
     +               + RPT_FEDERAL_BTL_INCOME_TAX(MO),                   ! 132
     +               INCOME_VARIABLES(MO,
     +                              mty_tot_st_incm_tax_pd) 
     +               - RPT_STATE_INCOME_TAX_B4_CREDITS(MO)
     +               + RPT_STATE_GENERAL_CREDITS(MO),                    ! 133
     +               INCOME_VARIABLES(MO,
     +                             mty_tot_st_incm_tax_pd), ! 134
     +               INCOME_VARIABLES(MO,
     +                                mty_tot_fed_incm_tax_pd)
     +                  - RPT_FED_INCOME_TAX_B4_CREDITS(MO)                
     +                  + RPT_FEDERAL_SEC29_CREDITS(MO)                    
     +                  + RPT_FEDERAL_GENERAL_CREDITS(MO)                  
     +                  + RPT_FEDERAL_AMT_CREDITS_USED(MO)                 
     +                  - RPT_FED_AMT_CREDITS_GENERATED(MO),             ! 135
     +               INCOME_VARIABLES(MO,
     +                               mty_tot_fed_incm_tax_pd), ! 136
     +               RPT_STATE_NOLS_USED(MO),                            ! 137
     +               RPT_FEDERAL_NOLS_USED(MO),                          ! 138
     +               RPT_STATE_NOLS_GENERATED(MO),                       ! 139
     +               RPT_FEDERAL_NOLS_GENERATED(MO),                     ! 140
     +               RPT_STATE_GENERAL_CREDITS(MO),                      ! 141
     +               RPT_FEDERAL_SEC29_CREDITS(MO),                      ! 142
     +               RPT_FEDERAL_GENERAL_CREDITS(MO),                    ! 143
     +               RPT_FEDERAL_AMT_CREDITS_USED(MO),                   ! 144
     +               RPT_FED_AMT_CREDITS_GENERATED(MO),                  ! 145
     +               STATE_DEF_TAX_DR_FROM_NOLS(MO),                     ! 146
     +               FED_DEF_TAX_DR_FROM_NOLS_AMT(MO),                   ! 147
     +               (TAX_VARIABLES(MO,I),I=1,TAX_VARS),                 ! 110+I
C
C OTHER TAXES SECTION
C
     +               RPT_CLASS_OPERATING_REVENUES(MO),                   ! 198
     +               RPT_EXCLUDED_OTHER_TAXES_REV(MO,CLASS_LEVEL),       ! 199
     +               RPT_CLASS_NON_VARIABLE_EXPENSE(MO),                 ! 200
     +               RPT_EXCLUDED_OTHER_TAXES_EXP(MO,CLASS_LEVEL),       ! 201
     +               RPT_OTHER_TAXES_MOVING_UP(MO,CLASS_LEVEL),          ! 202
     +               OTHER_TAX_ADJUSTMENT(MO),                           ! 203
     +               MONTHLY_OTHER_REV_TAX_RATE(MO)*100.,                ! 204 
     +               MONTHLY_OTHER_EXP_TAX_RATE(MO)*100.,                ! 205
C
C REVENUE TAXES SECTION
C
     +                RPT_CLASS_REVENUE_TAX_REVENUES(MO),                ! 206
     +                RPT_CLASS_REV_TAX_EXCLUDED_REVS(MO),               ! 207
     +                RPT_REVENUE_TAXES_MOVING_UP(MO,CLASS_LEVEL),       ! 208
     +                REVENUE_TAX_ADJUSTMENT(MO),                        ! 209
     +                MONTHLY_REVENUE_TAX_RATE(MO)*100.,                 ! 210
C
C EFFECTIVE TAX RATE
C
     +                BOOK_INCOME_B4_TAXES,                              ! 211
     +                TOTAL_INCOME_TAXES,                                ! 212
     +                EFFECTIVE_TAX_RATE,                                ! 213
     +                ADJUSTED_BOOK_INCOME                               ! 214
               ENDIF
            ENDDO
            THIS_YEARS_RECORD = THIS_YEARS_RECORD - 13
         ENDIF
C
C TRACK SUB STATE TAX ITEMS NOLs
C
         IF(CLASS_TYPE == SUBSIDIARY) THEN
            DO MO = 0, 12
               STATE_NOLS_GEN_BY_SUBS(MO) = STATE_NOLS_GEN_BY_SUBS(MO)
     +                                    + RPT_STATE_NOLS_GENERATED(MO)
               STATE_NOLS_USED_BY_SUBS(MO) = STATE_NOLS_USED_BY_SUBS(MO)
     +                                       + RPT_STATE_NOLS_USED(MO)
               SUB_STATE_DR_TAX_FROM_NOLS(MO) = 
     +                                  SUB_STATE_DR_TAX_FROM_NOLS(MO)
     +                                  + STATE_DEF_TAX_DR_FROM_NOLS(MO) ! 146
               SUB_FED_DR_TAX_FROM_NOLS_AMT(MO) = 
     +                                  SUB_FED_DR_TAX_FROM_NOLS_AMT(MO)
     +                                + FED_DEF_TAX_DR_FROM_NOLS_AMT(MO) ! 147
            ENDDO
         ENDIF
         IF(CLASS_TYPE == PARENT) THEN ! SAVE PARENT STATE TAXES
            DO MO = 0, 12
               PARENT_STATE_TAXES(MO) =
     +                               RPT_STATE_INCOME_TAX_B4_CREDITS(MO)
               PARENT_STATE_DR_TAX_FROM_NOLS(MO) =
     +                                    STATE_DEF_TAX_DR_FROM_NOLS(MO) ! 146
               PARENT_OTHER_LT_LIABS(MO) = BALANCE_SHEET_VARIABLES(MO,
     +                              monthly_other_long_term_liabs)
               STATE_NOLS_GEN_BY_SUBS(MO) = STATE_NOLS_GEN_BY_SUBS(MO)
     +                                    + RPT_STATE_NOLS_GENERATED(MO)
               STATE_NOLS_USED_BY_SUBS(MO) = STATE_NOLS_USED_BY_SUBS(MO)
     +                                       + RPT_STATE_NOLS_USED(MO)
               STATE_NOLS_GEN_AT_PARENT(MO) =
     +                                    STATE_NOLS_GEN_AT_PARENT(MO)
     +                                    + RPT_STATE_NOLS_GENERATED(MO)
               STATE_NOLS_USED_AT_PARENT(MO) =
     +                                     STATE_NOLS_USED_AT_PARENT(MO)
     +                                     + RPT_STATE_NOLS_USED(MO)
            ENDDO
         ENDIF
         IF(CLASS_TYPE == SUBSIDIARY .OR. CLASS_TYPE == PARENT) THEN

               DO MO = 1, 12
                  IF(CLASS_IS_ACTIVE(MO)) THEN

                     SUB_BTL_STATE_TAXES_PAID(MO) =
     +                                    SUB_BTL_STATE_TAXES_PAID(MO)
     +                                    + RPT_STATE_BTL_INCOME_TAX(MO)
                     SUB_BTL_STATE_TAXABLE_INCOME(MO) =
     +                                SUB_BTL_STATE_TAXABLE_INCOME(MO)
     +                                + RPT_STATE_BTL_TAXABLE_INCOME(MO)
                     SUB_BTL_TAXABLE_OTHER_INCOME(MO) =    ! 31 TAX TABLE
     +                                SUB_BTL_TAXABLE_OTHER_INCOME(MO)   ! 31 TAX TABLE
     +                                + RPT_BTL_OTHER_TAXABLE_INCOME(MO)      ! 31
                     SUB_BTL_TAXABLE_INVEST_INCOME(MO) =   ! 32 TAX TABLE
     +                                 SUB_BTL_TAXABLE_INVEST_INCOME(MO)  ! 32 TAX TABLE

     +                + INCOME_VARIABLES(MO,Monthly_Investment_Earnings)
     +                - .7*INCOME_VARIABLES(MO,
     +                                     Monthly_Dividend_70_Earnings)
     +                - INCOME_VARIABLES(MO,
     +                         mthy_rtrmt_med_fd_rngs) ! 9/28/04
     +                + INCOME_VARIABLES(MO,
     +                                mty_totl_lti_income)
     +                + INCOME_VARIABLES(MO,
     +                                   BTL_Monthly_STInvestmet_Income)
                     SUB_BTL_TAXABLE_EXPENSES(MO) =        ! 95 TAX TABLE
     +                                    SUB_BTL_TAXABLE_EXPENSES(MO)       ! 95 TAX TABLE
     +                                    + RPT_BTL_TAXABLE_EXPENSES(MO)          ! 95
                     SUB_BTL_MISC_DEDUCTIONS(MO) =
     +                                       SUB_BTL_MISC_DEDUCTIONS(MO)
                     SUB_BTL_M1_TAX_DEDUCTIONS(MO) = 
     +                                   SUB_BTL_M1_TAX_DEDUCTIONS(MO)
     +                                   + RPT_BTL_M1_TAX_DEDUCTIONS(MO)
                     SUB_BTL_FEDERAL_TAXES_PAID(MO) =
     +                                  SUB_BTL_FEDERAL_TAXES_PAID(MO)
     +                                  + RPT_FEDERAL_BTL_INCOME_TAX(MO) ! 129
                     SUB_OTHER_TAXES(MO) = SUB_OTHER_TAXES(MO)
     +                        + INCOME_VARIABLES(MO,monthly_oth_taxes)
                     SUB_CASH_OTHER_TAXES(MO) = SUB_CASH_OTHER_TAXES(MO)
     +                             + CASH_VARIABLES(MO,cash_other_taxes)
                     SUB_FASB87_DEF_TAX_DR_ADJ(MO) =
     +                                SUB_FASB87_DEF_TAX_DR_ADJ(MO)
     +                                + FASB87_DEFFERED_TAXES_ADJ_DR(MO)
                     CONSOLIDATED_CASH_STATE_TAXES(MO) =
     +                             CONSOLIDATED_CASH_STATE_TAXES(MO)
     +                             + CASH_VARIABLES(MO,
     +                                     cash_st_income_taxes_pd)
                  ENDIF
               ENDDO
               SUB_BTL_STATE_TAXES_PAID(0) =
     +                              SUM(SUB_BTL_STATE_TAXES_PAID(1:))
               SUB_BTL_STATE_TAXABLE_INCOME(0) =
     +                          SUM(SUB_BTL_STATE_TAXABLE_INCOME(1:))
               SUB_BTL_TAXABLE_OTHER_INCOME(0) =    ! 31 TAX TABLE
     +                          SUM(SUB_BTL_TAXABLE_OTHER_INCOME(1:))   ! 31 TAX TABLE
               SUB_BTL_TAXABLE_INVEST_INCOME(0) =   ! 32 TAX TABLE
     +                           SUM(SUB_BTL_TAXABLE_INVEST_INCOME(1:))  ! 32 TAX TABLE
               SUB_BTL_TAXABLE_EXPENSES(0) =        ! 95 TAX TABLE
     +                              SUM(SUB_BTL_TAXABLE_EXPENSES(1:))       ! 95 TAX TABLE
               SUB_BTL_MISC_DEDUCTIONS(0) =
     +                                 SUM(SUB_BTL_MISC_DEDUCTIONS(1:))
               SUB_BTL_M1_TAX_DEDUCTIONS(0) = 
     +                             SUM(SUB_BTL_M1_TAX_DEDUCTIONS(1:))
               SUB_BTL_FEDERAL_TAXES_PAID(0) =
     +                            SUM(SUB_BTL_FEDERAL_TAXES_PAID(1:))
               SUB_OTHER_TAXES(0) = SUM(SUB_OTHER_TAXES(1:))
               SUB_CASH_OTHER_TAXES(0) = SUM(SUB_CASH_OTHER_TAXES(1:))
               SUB_FASB87_DEF_TAX_DR_ADJ(0) =
     +                                SUM(SUB_FASB87_DEF_TAX_DR_ADJ(1:))
               CONSOLIDATED_CASH_STATE_TAXES(0) =
     +                            SUM(CONSOLIDATED_CASH_STATE_TAXES(1:))
C            ENDIF
C
C CAPTURE ACQUISTIONS
C
            IF(YR == ACTIVATE_YR .OR. YR == DEACTIVE_YR) THEN
               IF(YR == ACTIVATE_YR) THEN
                  DO MO = 1, 12 
                     IF(CLASS_IS_ACTIVE(MO)) THEN 
                        NEW_ACQUISITION_DEF_TAXES_CR(MO) =
     +                        NEW_ACQUISITION_DEF_TAXES_CR(MO)
     +                        + BALANCE_SHEET_VARIABLES(MO-1,
     +                                    mty_dfrd_income_taxes)
                        NEW_ACQUISITION_ACCOUNTS_PAYABLE_BAL(MO) =
     +                        NEW_ACQUISITION_ACCOUNTS_PAYABLE_BAL(MO)
     +                        + BALANCE_SHEET_VARIABLES(MO-1,
     +                                     monthly_accounts_payable_bal)
                        EXIT
                     ENDIF
                  ENDDO
               ENDIF
               IF(YR == DEACTIVE_YR) THEN
                  DO MO = 1, 12 
                     IF(.NOT. CLASS_IS_ACTIVE(MO)) THEN 
                        NEW_ACQUISITION_DEF_TAXES_CR(MO) =
     +                        NEW_ACQUISITION_DEF_TAXES_CR(MO)
     +                        - BALANCE_SHEET_VARIABLES(MO-1,
     +                                    mty_dfrd_income_taxes)
                        NEW_ACQUISITION_ACCOUNTS_PAYABLE_BAL(MO) =
     +                        NEW_ACQUISITION_ACCOUNTS_PAYABLE_BAL(MO)
     +                        - BALANCE_SHEET_VARIABLES(MO-1,
     +                                     monthly_accounts_payable_bal)
                        EXIT
                     ENDIF
                  ENDDO
               ENDIF
            
            ENDIF
         ENDIF
         IF(THIS_IS_REPORTING_CLASS)
     +                 CALL MONTHLY_FINANCIAL_DATABASE(OUTPUT_CLASS_ID,
     +                                                 ASSET_CLASS_NAME,
     +                                                 INCOME_VARIABLES)
         IF(CLASS_TYPE == CONSOLIDATED) THEN ! SAME AS END OF YEAR
            NEW_ACQUISITION_DEF_TAXES_CR = 0. 
            NEW_ACQUISITION_ACCOUNTS_PAYABLE_BAL = 0.
            SUB_FED_TAX_PAYMENTS_2_PARENT = 0.
            SUB_STATE_TAX_PAYMENTS_2_PARENT = 0.
            SUB_TAXABLE_INCOME_B4_DEDUCTS = 0.
            SUB_STATE_TAXES_PAID = 0.
            SUB_DEFERRED_TAXES_DR = 0.
            SUB_BTL_STATE_TAXES_PAID = 0.
            SUB_BTL_STATE_TAXABLE_INCOME = 0.
            SUB_BTL_TAXABLE_OTHER_INCOME = 0.   ! 31 TAX TABLE
            SUB_BTL_TAXABLE_INVEST_INCOME = 0.  ! 32 TAX TABLE
            SUB_BTL_TAXABLE_EXPENSES = 0.       ! 95 TAX TABLE
            SUB_BTL_MISC_DEDUCTIONS = 0.
            SUB_BTL_M1_TAX_DEDUCTIONS = 0.
            SUB_BTL_FEDERAL_TAXES_PAID = 0.
            SUB_OTHER_TAXES = 0.
            SUB_CASH_OTHER_TAXES = 0.
            SUB_FASB87_DEF_TAX_DR_ADJ = 0.
            CONSOLIDATED_CASH_STATE_TAXES = 0.
            FEDERAL_INC_TAX_CARRY_OVER=TEMP_FEDERAL_INC_TAX_CARRY_OVER
            SAVE_ANNUAL_BOY_VARIABLE = SAVE_ANNUAL_EOY_VARIABLE
         ELSE
            IF(CLASS_TYPE == SUBSIDIARY .OR. CLASS_TYPE == PARENT) THEN
               VOID_REAL = ZERO_TAX_EXCLUSIONS(CLASS_LEVEL)
            ELSE
               VOID_REAL = SAVE_SBU_TAX_EXCLUSIONS(CLASS_LEVEL,
     +                                             INCOME_VARIABLES)
            ENDIF
         ENDIF

      RETURN         
C***********************************************************************
      ENTRY MONTHLY_ZERO_ALL_OUTPUT(R_YR,R_ASSET_CLASS_NAME,
     +                              R_OUTPUT_CLASS_ID)
C***********************************************************************
         IREC = THIS_YEARS_RECORD
         ASSET_CLASS_NAME = R_ASSET_CLASS_NAME
         OUTPUT_CLASS_ID = R_OUTPUT_CLASS_ID
         DO MO = 0, 12
               WRITE(INCOME_UNIT,REC=IREC)
     +                     PRT_ENDPOINT(),
     +                     FLOAT(BASE_YEAR+R_YR),
     +                     ASSET_CLASS_NAME,
     +                     LONG_MONTH_NAMES(MO),
     +                     OUTPUT_CLASS_ID
               WRITE(CASH_UNIT,REC=IREC)
     +                    PRT_ENDPOINT(),
     +                    FLOAT(BASE_YEAR+R_YR),
     +                    ASSET_CLASS_NAME,
     +                    LONG_MONTH_NAMES(MO),
     +                    OUTPUT_CLASS_ID
               WRITE(BALANCE_UNIT,REC=IREC)
     +                    PRT_ENDPOINT(),
     +                    FLOAT(BASE_YEAR+R_YR),
     +                    ASSET_CLASS_NAME,
     +                    BAL_LONG_MONTH_NAMES(MO),
     +                    OUTPUT_CLASS_ID
               WRITE(TAX_UNIT,REC=IREC)
     +                    PRT_ENDPOINT(),
     +                    FLOAT(BASE_YEAR+R_YR),
     +                    ASSET_CLASS_NAME,
     +                    LONG_MONTH_NAMES(MO),
     +                    OUTPUT_CLASS_ID
               IF(FirstEnergy())
     +          CALL FE_ZERO_PCA_LEGAL_RPTS(IREC,MO,
     +                                      FLOAT(BASE_YEAR+R_YR),
     +                                      ASSET_CLASS_NAME,
     +                                      OUTPUT_CLASS_ID)
            IREC = IREC + 1
         ENDDO
         THIS_YEARS_RECORD = THIS_YEARS_RECORD - 13
      RETURN
C***********************************************************************
      ENTRY MONTHLY_ZERO_BALANCE_YR(R_MO,R_YR,R_ASSET_CLASS_NAME,
     +                              R_OUTPUT_CLASS_ID)
C***********************************************************************
         ASSET_CLASS_NAME = R_ASSET_CLASS_NAME
         OUTPUT_CLASS_ID = R_OUTPUT_CLASS_ID
         IREC = THIS_YEARS_RECORD+MO
               WRITE(INCOME_UNIT,REC=IREC)
     +                     PRT_ENDPOINT(),
     +                     FLOAT(BASE_YEAR+R_YR),
     +                     ASSET_CLASS_NAME,
     +                     LONG_MONTH_NAMES(MO),
     +                     OUTPUT_CLASS_ID
               WRITE(CASH_UNIT,REC=IREC)
     +                    PRT_ENDPOINT(),
     +                    FLOAT(BASE_YEAR+R_YR),
     +                    ASSET_CLASS_NAME,
     +                    LONG_MONTH_NAMES(MO),
     +                    OUTPUT_CLASS_ID
               WRITE(BALANCE_UNIT,REC=IREC)
     +                    PRT_ENDPOINT(),
     +                    FLOAT(BASE_YEAR+R_YR),
     +                    ASSET_CLASS_NAME,
     +                    BAL_LONG_MONTH_NAMES(MO),
     +                    OUTPUT_CLASS_ID
               WRITE(TAX_UNIT,REC=IREC)
     +                    PRT_ENDPOINT(),
     +                    FLOAT(BASE_YEAR+R_YR),
     +                    ASSET_CLASS_NAME,
     +                    LONG_MONTH_NAMES(MO),
     +                    OUTPUT_CLASS_ID
               IF(FirstEnergy())
     +          CALL FE_ZERO_PCA_LEGAL_RPTS(IREC,MO,
     +                                      FLOAT(BASE_YEAR+R_YR),
     +                                      ASSET_CLASS_NAME,
     +                                      OUTPUT_CLASS_ID)
      RETURN
C***********************************************************************
      ENTRY CIAC_MONTHLY_AMORTIZATION_CAL(R_YR,CLASS_ID,
     +                                    R_CIAC_BALANCE_BOY,
     +                                    R_CIAC_AMORTIZATION,
     +                                    CLASS_LEVEL)
C***********************************************************************
C
C
         CALL MONTHLY_ADDENDUM_CIAC_VALUES(CLASS_ID,R_YR,
     +                                    CIAC_MONTHLY_AMORT_RATE,
     +                                    CIAC_MONTHLY_ADDENDUM_2_AMORT,
     +                                    CIAC_MONTHLY_CASH)
C

         CIAC_BALANCE(0) = R_CIAC_BALANCE_BOY
         DO MO = 1, 12
            CIAC_MONTHLY_AMORTIZATION(MO) =
     +                 CIAC_MONTHLY_AMORT_RATE(MO)/100.*
     +                   (CIAC_BALANCE(MO-1) + CIAC_MONTHLY_CASH(MO)/2.)
     +                 + CIAC_MONTHLY_ADDENDUM_2_AMORT(MO)
            CIAC_BALANCE(MO) = CIAC_BALANCE(MO-1) 
     +                      + CIAC_MONTHLY_CASH(MO)
     +                      - CIAC_MONTHLY_AMORTIZATION(MO)

         ENDDO
         CIAC_MONTHLY_AMORTIZATION(0) =
     +                                SUM(CIAC_MONTHLY_AMORTIZATION(1:))
         R_CIAC_AMORTIZATION = CIAC_MONTHLY_AMORTIZATION(0)
     

         CIAC_BALANCE(:) = CIAC_BALANCE(:)
     +                       + BALANCE_LINKED_LEVEL(:,monthly_cica,
     +                                                      CLASS_LEVEL) 

      RETURN
C***********************************************************************
      ENTRY COMMON_STOCK_DIVIDEND_ACCRUAL(R_MONTHLY_COMMON_DIVIDENDS,
     +                                    R_COMMON_STOCK_DIV_ACCRUALS,
     +                                    R_COMMON_STOCK_CASH_DIVIDENDS)
C***********************************************************************
C
      
C
C COMMON DIVIDENDS QUARTERLY 
C
         R_COMMON_STOCK_CASH_DIVIDENDS = PARENT_CS_DIV_CARRY_OVER
         DO MO = 1, 12-CS_DIVIDEND_PAYMENT_LAG
            R_COMMON_STOCK_CASH_DIVIDENDS =
     +                                  R_COMMON_STOCK_CASH_DIVIDENDS
     +                                  + R_MONTHLY_COMMON_DIVIDENDS(MO)
         ENDDO
         R_COMMON_STOCK_DIV_ACCRUALS = R_MONTHLY_COMMON_DIVIDENDS(0)
     +                                 - R_COMMON_STOCK_CASH_DIVIDENDS
C
      RETURN
C***********************************************************************
      ENTRY M3_ALLOCATE_ARRAYS(R_MAX_LINKED_LEVEL,
     +                         R_NUMBER_OF_REPORTING_CLASSES,
     +                         R_MAX_CLASS_NUM,
     +                         R_NUM_OF_ACTIVE_CLASSES,
     +                         R_MASTER_CLASS_LIST,
     +                         R_PARENT_CS_DIV_CARRY_OVER,
     +                         R_OPENING_CASH_BALANCES,
     +                         R_CS_DECLARATION_MONTH,
     +                         R_SHARE_MONTH,
     +                         R_PARENT_CLASS_ID_NUM)
C***********************************************************************
C
         IF(R_NUMBER_OF_REPORTING_CLASSES == 1) THEN
            NUMBER_OF_REPORTING_CLASSES = R_NUMBER_OF_REPORTING_CLASSES
         ELSE
            NUMBER_OF_REPORTING_CLASSES=R_NUMBER_OF_REPORTING_CLASSES +1 !CONSOLIDATED CLASS
         ENDIF
         INCOME_UNIT = MONTHLY_INCOME_UNIT_NO()
         CASH_UNIT = MONTHLY_CASH_UNIT_NO()
         BALANCE_UNIT = MONTHLY_BALANCE_UNIT_NO()
         CPL_TAX_UNIT = CPL_TAX_REPORTING_UNIT_NO()
         TAX_UNIT = MONTHLY_TAX_UNIT_NO()
         PARENT_CS_DIV_CARRY_OVER = R_PARENT_CS_DIV_CARRY_OVER
         MAX_LINKED_LEVEL = R_MAX_LINKED_LEVEL
C
         IF(ALLOCATED(BALANCE_SHEET_VARIABLES))
     +                     DEALLOCATE(BALANCE_SHEET_VARIABLES,
     +                                BALANCE_SHEET_DEACTIVE_VARS,
     +                                INCOME_VARIABLES,
     +                                CASH_VARIABLES,
     +                                TAX_VARIABLES,
     +                                CPL_TAX__VARIABLES,
     +                                INCOME_LINKED_LEVEL,
     +                                BALANCE_LINKED_LEVEL,
     +                                TRANSFER_LINKED_LEVEL_VARIABLES,
     +                                PAYABLE_LINKED_LOWER_LEVEL,
     +                                RECEIVABLES_LINKED_LOWER_LEVEL,
     +                                TAXES_LINKED_LEVEL,
     +                                CASH_LINKED_LEVEL,
     +                                OPENING_CASH_BALANCES,
     +                                TRANSFER_VARIABLES,
     +                                TAX_TRANSFER_VARIABLES)
C
         ALLOCATE(BALANCE_SHEET_VARIABLES(0:12,BAL_SHEET_VARS))
         ALLOCATE(INCOME_VARIABLES(0:12,INCOME_VARS))
         ALLOCATE(CASH_VARIABLES(0:12,CASH_VARS))
         ALLOCATE(TAX_VARIABLES(0:12,TAX_VARS))
         ALLOCATE(CPL_TAX__VARIABLES(0:12,CPL_TAX_VARS,3))
         ALLOCATE(INCOME_LINKED_LEVEL(0:12,INCOME_VARS,
     +                                             -1:MAX_LINKED_LEVEL))
         ALLOCATE(BALANCE_LINKED_LEVEL(0:12,BAL_SHEET_VARS,
     +                                             -1:MAX_LINKED_LEVEL))
         ALLOCATE(BALANCE_SHEET_DEACTIVE_VARS(0:12,BAL_SHEET_VARS,
     +                                             -1:MAX_LINKED_LEVEL))
         ALLOCATE(TRANSFER_LINKED_LEVEL_VARIABLES(0:12,BAL_SHEET_VARS,
     +                                             -1:MAX_LINKED_LEVEL))
         ALLOCATE(TAX_TRANSFER_VARIABLES(0:12,TAX_VARS,
     +                                             -1:MAX_LINKED_LEVEL))
         ALLOCATE(PAYABLE_LINKED_LOWER_LEVEL(0:12,-1:MAX_LINKED_LEVEL))
         ALLOCATE(RECEIVABLES_LINKED_LOWER_LEVEL(0:12,
     +                                             -1:MAX_LINKED_LEVEL))
         ALLOCATE(TAXES_LINKED_LEVEL(0:12,TAX_VARS,-1:MAX_LINKED_LEVEL))
         ALLOCATE(CASH_LINKED_LEVEL(0:12,CASH_VARS,-1:MAX_LINKED_LEVEL))
         ALLOCATE(OPENING_CASH_BALANCES(0:R_NUM_OF_ACTIVE_CLASSES))
         ALLOCATE(TRANSFER_VARIABLES(0:12,BAL_SHEET_VARS))
         IF(ALLOCATED(CLASS_GOODWILL_EOY))DEALLOCATE(CLASS_GOODWILL_EOY)
         ALLOCATE(CLASS_GOODWILL_EOY(-1:R_NUM_OF_ACTIVE_CLASSES))
         CLASS_GOODWILL_EOY = 0.
         
         PAYABLE_LINKED_LOWER_LEVEL = 0.
         RECEIVABLES_LINKED_LOWER_LEVEL = 0.
         INCOME_LINKED_LEVEL = 0.
         CASH_LINKED_LEVEL = 0.
         BALANCE_LINKED_LEVEL = 0.
         BALANCE_SHEET_DEACTIVE_VARS = 0.
         TRANSFER_LINKED_LEVEL_VARIABLES = 0.
         TAXES_LINKED_LEVEL = 0.
         TAX_TRANSFER_VARIABLES = 0.
         OPENING_CASH_BALANCES = 0.
         STATE_NOLS_GEN_BY_SUBS = 0.
         STATE_NOLS_USED_BY_SUBS = 0.
         STATE_NOLS_GEN_AT_PARENT = 0.
         STATE_NOLS_USED_AT_PARENT = 0.
         PARENT_STATE_TAXES = 0.
         PARENT_STATE_DR_TAX_FROM_NOLS = 0.
         SUB_STATE_DR_TAX_FROM_NOLS = 0.
         SUB_FED_DR_TAX_FROM_NOLS_AMT = 0.
C  
C TRYING TO GET THE FIRST ENTRIES TO SET THE ORDER OF THE CLASS NAMES
C
         CS_DECLARATION_MONTH(1) = 3
         CS_DECLARATION_MONTH(2) = 6
         CS_DECLARATION_MONTH(3) = 9 
         CS_DECLARATION_MONTH(4) = 12
         STI_CASH_LAG = 1
         R_SHARE_MONTH = -1
         IF(CPL_ACTIVE()) THEN
            CS_DIVIDEND_PAYMENT_LAG = 2
         ELSEIF(EMPIRE()) THEN
            CS_DECLARATION_MONTH(1) = 1
            CS_DECLARATION_MONTH(2) = 4
            CS_DECLARATION_MONTH(3) = 7 
            CS_DECLARATION_MONTH(4) = 10
            CS_DIVIDEND_PAYMENT_LAG = 2
C            STI_CASH_LAG = 0
            R_SHARE_MONTH = 1
         ELSEIF(MPS()) THEN
            CS_DECLARATION_MONTH(1) = 3
            CS_DECLARATION_MONTH(2) = 5
            CS_DECLARATION_MONTH(3) = 9 
            CS_DECLARATION_MONTH(4) = 11
            CS_PAY_MONTH(1) = .TRUE.
            CS_PAY_MONTH(4) = .TRUE.
            CS_PAY_MONTH(7) = .TRUE.
            CS_PAY_MONTH(10) = .TRUE.
            CS_DIVIDEND_PAYMENT_LAG = 2
C            STI_CASH_LAG = 0
            R_SHARE_MONTH = 1
         ELSEIF(IPALCO()) THEN
            CS_DIVIDEND_PAYMENT_LAG = 0
         ELSE
            CS_DIVIDEND_PAYMENT_LAG = 1
         ENDIF
         R_CS_DECLARATION_MONTH(1) = CS_DECLARATION_MONTH(1)
         R_CS_DECLARATION_MONTH(2) = CS_DECLARATION_MONTH(2)
         R_CS_DECLARATION_MONTH(3) = CS_DECLARATION_MONTH(3)
         R_CS_DECLARATION_MONTH(4) = CS_DECLARATION_MONTH(4)
         IF(END_POINT == START_STUDY_ENDPOINT()) THEN
            THIS_YEARS_RECORD = 7 + 
     +                        INT(13*(NUMBER_OF_REPORTING_CLASSES - 1)) ! PARENT IS INCLUDED
            IF(LAHEY_LF95()) THIS_YEARS_RECORD = THIS_YEARS_RECORD + 1
            NEXT_YEARS_START_RECORD = THIS_YEARS_RECORD
            INCREMENT_RECORDS= INT(13*NUMBER_OF_REPORTING_CLASSES) ! PARENT IS INCLUDED
         ELSE
            NEXT_YEARS_START_RECORD = NEXT_YEARS_START_RECORD
     +                                - INCREMENT_RECORDS
            INCREMENT_RECORDS= INT(13*NUMBER_OF_REPORTING_CLASSES) ! PARENT IS INCLUDED
            NEXT_YEARS_START_RECORD = NEXT_YEARS_START_RECORD
     +                                + INCREMENT_RECORDS
         ENDIF
C
C FEDERAL INCOME TAX PAYMENT CARRY OVERS
C
         IF(ALLOCATED(FEDERAL_INC_TAX_CARRY_OVER)) 
     +                       DEALLOCATE(FEDERAL_INC_TAX_CARRY_OVER,
     +                                  TEMP_FEDERAL_INC_TAX_CARRY_OVER)
         ALLOCATE(
     +           FEDERAL_INC_TAX_CARRY_OVER(-1:R_NUM_OF_ACTIVE_CLASSES))
         ALLOCATE(
     +      TEMP_FEDERAL_INC_TAX_CARRY_OVER(-1:R_NUM_OF_ACTIVE_CLASSES))
         FEDERAL_INC_TAX_CARRY_OVER = 0.
         TEMP_FEDERAL_INC_TAX_CARRY_OVER = 0.
C
C SET-UP THE PAYMENT ARRAYS
C
         IF(ALLOCATED(PAYABLE_MONTHLY_VALUES))
     +                                DEALLOCATE(PAYABLE_MONTHLY_VALUES,
     +                                      TEMP_PAYABLE_MONTHLY_VALUES)
         ALLOCATE(PAYABLE_MONTHLY_VALUES(0:12,PAYMENT_VARS,
     +                                      -1:R_NUM_OF_ACTIVE_CLASSES))
         ALLOCATE(TEMP_PAYABLE_MONTHLY_VALUES(0:12,PAYMENT_VARS,
     +                                      -1:R_NUM_OF_ACTIVE_CLASSES))
         PAYABLE_MONTHLY_VALUES = 0.
         CALL MONTHLY_PAYABLES(PAYABLE_MONTHLY_VALUES,
     +                         R_MASTER_CLASS_LIST,
     +                         R_MAX_CLASS_NUM,
     +                         R_NUM_OF_ACTIVE_CLASSES,
     +                         R_PARENT_CLASS_ID_NUM)

         OPENING_CASH_BALANCES(0:R_NUM_OF_ACTIVE_CLASSES) = 
     +                R_OPENING_CASH_BALANCES(0:R_NUM_OF_ACTIVE_CLASSES)

         TEMP_PAYABLE_MONTHLY_VALUES(:,:,-1:R_NUM_OF_ACTIVE_CLASSES) =
     +            PAYABLE_MONTHLY_VALUES(:,:,-1:R_NUM_OF_ACTIVE_CLASSES)
C
C OPERATING METHOD PRIOR YEARS INFORMATION
C
         CALL INIT_MONTHLY_CLASS_OPT_REVS(R_NUM_OF_ACTIVE_CLASSES)
      RETURN
C***********************************************************************
      ENTRY GET_INTRS_TAXES_PAYABLE_YEAR_1(CLASS_POS,
     +                                     R_STD_INTEREST_OWED,
     +                                     R_REVENUE_TAXES,
     +                                     R_PROPERTY_TAXES,
     +                                     R_STATE_INCOME_TAXES,
     +                                     R_FEDERAL_INCOME_TAXES,
     +                                     R_STATE_TAX_ON_CAPITAL,
     +                                     R_FEDERAL_TAX_ON_CAPITAL,
     +                                     R_OTHER_TAXES_PAYABLE)
C***********************************************************************
C
C
         R_STD_INTEREST_OWED = PAYABLE_MONTHLY_VALUES(0,
     +                                  payment_std_interest,CLASS_POS)
         R_REVENUE_TAXES = PAYABLE_MONTHLY_VALUES(0,
     +                         payment_operating_revenue_tax,CLASS_POS)
         R_PROPERTY_TAXES = PAYABLE_MONTHLY_VALUES(0,
     +                                payment_property_taxes,CLASS_POS)
         R_STATE_INCOME_TAXES = PAYABLE_MONTHLY_VALUES(0,
     +                       payment_state_income_taxes_paid,CLASS_POS)
         R_FEDERAL_INCOME_TAXES = PAYABLE_MONTHLY_VALUES(0,
     +                     payment_federal_inc_taxes_paid,CLASS_POS)
         R_STATE_TAX_ON_CAPITAL = PAYABLE_MONTHLY_VALUES(0,
     +                          payment_state_tax_on_capital,CLASS_POS)
         R_FEDERAL_TAX_ON_CAPITAL = PAYABLE_MONTHLY_VALUES(0,
     +                        payment_federal_tax_on_capital,CLASS_POS)
         R_OTHER_TAXES_PAYABLE = PAYABLE_MONTHLY_VALUES(0,
     +                                   payment_other_taxes,CLASS_POS)
      RETURN
C***********************************************************************
      ENTRY M3_SET_OUTPUT_RECORD
C***********************************************************************
C
         THIS_YEARS_RECORD = NEXT_YEARS_START_RECORD
         NEXT_YEARS_START_RECORD = THIS_YEARS_RECORD + INCREMENT_RECORDS
         INCOME_LINKED_LEVEL = 0.
         CASH_LINKED_LEVEL = 0.
         BALANCE_LINKED_LEVEL = 0.
         BALANCE_SHEET_DEACTIVE_VARS = 0.
         TRANSFER_LINKED_LEVEL_VARIABLES = 0.
         TAXES_LINKED_LEVEL = 0.
         STATE_NOLS_GEN_BY_SUBS = 0.
         STATE_NOLS_USED_BY_SUBS = 0.
         STATE_NOLS_GEN_AT_PARENT = 0.
         STATE_NOLS_USED_AT_PARENT = 0.
         PARENT_STATE_TAXES = 0.
         PARENT_STATE_DR_TAX_FROM_NOLS = 0.
         SUB_STATE_DR_TAX_FROM_NOLS = 0.
         SUB_FED_DR_TAX_FROM_NOLS_AMT = 0.
         CONSOL_TRANSFER_LT_LIAB = 0. 
      RETURN
C***********************************************************************
      ENTRY M3_WRITE_OUTPUT_FILE
C***********************************************************************
C
      RETURN
C***********************************************************************
      ENTRY RETURN_PARENT_CS_CARRY_OVER(R_PARENT_CS_DIV_CARRY_OVER)
C***********************************************************************
C
         R_PARENT_CS_DIV_CARRY_OVER = PARENT_CS_DIV_CARRY_OVER
      RETURN
C***********************************************************************
      ENTRY SET_MONTHLY_PAYABLES(R_YR)
C***********************************************************************
         PAYABLE_MONTHLY_VALUES = 0.
c         TEMP_PAYABLE_MONTHLY_VALUES = 0.
         CALL MONTHLY_PAYABLES_4(R_YR,PAYABLE_MONTHLY_VALUES,
     +                           TEMP_PAYABLE_MONTHLY_VALUES)
      RETURN
C***********************************************************************
      ENTRY TAX_ISSUES_AND_ANALYSIS(R_YR,R_CLASS_ID,
     +                              CLASS_TYPE,
     +                              R_CLASS_LEVEL,
     +                              ANNUAL_VARS)
C***********************************************************************
C
C ADD PREVIOUS LEVELS

        TAX_VARIABLES(:,:) =TAX_VARIABLES(:,:)
     +                      + TAXES_LINKED_LEVEL(:,:,R_CLASS_LEVEL)

         CALL RETURN_M1_TIMING_DIFFERENCES(R_CLASS_ID,R_YR,Federal,
     +                                        TAX_VARIABLES)


      RETURN
C***********************************************************************
      ENTRY TRANSFER_MONTHLY_TAX_ITEMS(CLASS_TYPE,
     +                                 R_CLASS_LEVEL)
C***********************************************************************
C
         IF(R_CLASS_LEVEL >= 0) THEN
            TAXES_LINKED_LEVEL(:,:,R_CLASS_LEVEL) = 0.
            DO VARIABLE = 1, TAX_VARS 
               IF(VARIABLE == mthly_atl_federal_income_taxes) CYCLE
               IF(VARIABLE == mthly_btl_federal_income_taxes) CYCLE
               IF(VARIABLE == monthly_ltd_tax_deduction) THEN
                  IF(CLASS_TYPE == SUBSIDIARY .OR.
     +                                        CLASS_TYPE == PARENT) THEN
                     DO MO = 1, 12
                        IF(CLASS_IS_ACTIVE(MO)) THEN 
                           TAXES_LINKED_LEVEL(MO,VARIABLE,-1) =
     +                                TAXES_LINKED_LEVEL(MO,VARIABLE,-1)
     +                                + TAX_VARIABLES(MO,VARIABLE)
                        ENDIF
                     ENDDO
                     TAXES_LINKED_LEVEL(0,VARIABLE,-1) =
     +                           SUM(TAXES_LINKED_LEVEL(1:,VARIABLE,-1))
                  ENDIF
                  CYCLE
               ENDIF
               IF(CLASS_TYPE == SBU .OR.
     +                               CLASS_TYPE == REGULATED_GROUP) THEN
                  IF(VARIABLE == monthly_atl_state_income_taxes) CYCLE
                  IF(VARIABLE == monthly_btl_state_income_taxes) CYCLE

               ENDIF  
               DO MO = 1, 12
                  IF(CLASS_IS_ACTIVE(MO)) THEN 
                     TAXES_LINKED_LEVEL(MO,VARIABLE,R_CLASS_LEVEL-1) =
     +                   TAXES_LINKED_LEVEL(MO,VARIABLE,R_CLASS_LEVEL-1)
     +                   + TAX_VARIABLES(MO,VARIABLE)
                  ENDIF
               ENDDO
               TAXES_LINKED_LEVEL(0,VARIABLE,R_CLASS_LEVEL-1) =
     +              SUM(TAXES_LINKED_LEVEL(1:,VARIABLE,R_CLASS_LEVEL-1))
            ENDDO
         ENDIF
      RETURN
C***********************************************************************
      ENTRY MONTHLY_FUNDS_EARNINGS_CALCULATIONS(CLASS_ID,R_YR,
     +                       BOY_NUC_DECOM_FUND_BAL,
     +                       R_MONTHLY_NUC_DECOM_FUND_RETURN,
     +                       R_OCI_NUCL_FUND_RETURN,
     +                       R_MONTHLY_NUC_DECOM_FUND_NET_OF_TAX_RETURN, 
     +                       BOY_POST_RETIRE_MEDICAL_FUND,
     +                       R_RETIREMENT_MEDICAL_FUND_RETURN,
     +                       R_OCI_RETIREMENT_MEDICAL_FUND_RETURN)
C***********************************************************************
C
         CALL RET_MTHLY_FUNDS_EARNING_RATES(R_YR,CLASS_ID,
     +                                 INTEREST_ON_CUSTOMER_DEPOSITS,
     +                                 RETURN_RETIREMENT_MEDICAL_FUND,
     +                                 NUCL_DECOM_FUND_RETURN,
     +                                 CUSTOMER_DEPOSIT_INTEREST_RATE,
     +                                 MONTHLY_RETIREMENT_FUND_RATE,
     +                                 OCI_MTHLY_RETIREMENT_FUND_RATE,
     +                                 MONTHLY_NUC_DECOM_RATE,
     +                                 OCI_MONTHLY_NUC_DECOM_RATE)
C
C DECOMMISION FUND EARNINGS
C
            CALL RET_MTHLY_NUC_DECOM_CASH_ADDENS(CLASS_ID,R_YR,
     +                                   MONTHLY_NUC_DECOM_CASH_VALUES,
     +                                   MONTHLY_NUC_DECOM_EARNINGS_ADJ)
            MONTHLY_NUC_DECOM_BALANCE(0) = BOY_NUC_DECOM_FUND_BAL
            FEDERAL_EPA_92_TAX_RATE = .20
            IF(SALT_RIVER_PROJECT()) FEDERAL_EPA_92_TAX_RATE = 0.
c            MONTHLY_NUC_DECOM_RATE = MONTHLY_NUC_DECOM_RATE/100.
C
C RETIREMENT FUND RETURN
C
            CALL RET_MTHLY_RETIRMENT_CASH_ADDENS(CLASS_ID,R_YR,
     +                            MONTHLY_POST_RETIREMENT_CASH_PAYMENTS,
     +                            MONTHLY_RETIREE_MEDICAL_CASH_PAYMENTS,
     +                            MONTHLY_CASH_TO_POST_RETIREMENT,
     +                            MONTHLY_RETIREMENT_FUND_EARNINGS_ADJ)
            POST_RETIREMENT_MEDICAL_FUND_BAL =
     +                                      BOY_POST_RETIRE_MEDICAL_FUND
C
            DO MO = 1, 12 
               MONTHLY_STATE_TAX_RATE(MO) =  STATE_TAX_RATE
C
C MONTHLY RETIREMENT FUND RETURN
C
               RETIREMENT_MEDICAL_FUND_RETURN(MO) =
     +            MONTHLY_RETIREMENT_FUND_RATE(MO) *
     +              (POST_RETIREMENT_MEDICAL_FUND_BAL
     +               + (MONTHLY_POST_RETIREMENT_CASH_PAYMENTS(MO)
     +                  + MONTHLY_CASH_TO_POST_RETIREMENT(MO)
     +                  - MONTHLY_RETIREE_MEDICAL_CASH_PAYMENTS(MO))/2.)
     +            + MONTHLY_RETIREMENT_FUND_EARNINGS_ADJ(MO)
               OCI_RETIREMENT_MEDICAL_FUND_RETURN(MO) =
     +            OCI_MTHLY_RETIREMENT_FUND_RATE(MO) *
     +              (POST_RETIREMENT_MEDICAL_FUND_BAL
     +               + (MONTHLY_POST_RETIREMENT_CASH_PAYMENTS(MO)
     +                  + MONTHLY_CASH_TO_POST_RETIREMENT(MO)
     +                  - MONTHLY_RETIREE_MEDICAL_CASH_PAYMENTS(MO))/2.)
C     +            + MONTHLY_RETIREMENT_FUND_EARNINGS_ADJ(MO)
               POST_RETIREMENT_MEDICAL_FUND_BAL =
     +                       POST_RETIREMENT_MEDICAL_FUND_BAL
     +                       + MONTHLY_POST_RETIREMENT_CASH_PAYMENTS(MO)
     +                       + MONTHLY_CASH_TO_POST_RETIREMENT(MO)
     +                       - MONTHLY_RETIREE_MEDICAL_CASH_PAYMENTS(MO)
     +                       + OCI_RETIREMENT_MEDICAL_FUND_RETURN(MO)
               IF(SALT_RIVER_PROJECT() 
     +                      .OR. RETAIN_POST_RETIREMENT_EARNINGS()) THEN   ! ADD A RETAIN EARNINGS SWITCH HERE
                  POST_RETIREMENT_MEDICAL_FUND_BAL =
     +                       POST_RETIREMENT_MEDICAL_FUND_BAL
     +                       + RETIREMENT_MEDICAL_FUND_RETURN(MO)
               ENDIF
C
C NUC DECOMISSIONING FUND RETURN
C
               MONTHLY_NUC_DECOM_FUND_RETURN(MO) = 
     +                  MONTHLY_NUC_DECOM_RATE(MO) *
     +                        (MONTHLY_NUC_DECOM_BALANCE(MO-1)
     +                          + MONTHLY_NUC_DECOM_CASH_VALUES(MO)/2.)
               MONTHLY_OCI_NUC_DECOM_FUND_RETURN(MO) =
     +                  OCI_MONTHLY_NUC_DECOM_RATE(MO) *
     +                        (MONTHLY_NUC_DECOM_BALANCE(MO-1)
     +                          + MONTHLY_NUC_DECOM_CASH_VALUES(MO)/2.)
C
C THE TIMING OF THE TAX PAYMENT HAS TO BE RECONIZED IN THE EARNINGS
C CALCULATION.  THE ASSUMPTION HERE IS THAT IT IS PAID MONTHLY
C
               MONTHLY_NUC_DECOM_FUND_NET_OF_TAX_RETURN(MO) =
     +               (MONTHLY_NUC_DECOM_FUND_RETURN(MO)
     +                + MONTHLY_OCI_NUC_DECOM_FUND_RETURN(MO)) *
     +                     (1.-FEDERAL_EPA_92_TAX_RATE) *
     +                                  (1.- MONTHLY_STATE_TAX_RATE(MO))
     +               + MONTHLY_NUC_DECOM_EARNINGS_ADJ(MO)
               MONTHLY_NUC_DECOM_BALANCE(MO) = 
     +                    MONTHLY_NUC_DECOM_BALANCE(MO-1)
     +                    + MONTHLY_NUC_DECOM_CASH_VALUES(MO)
     +                    + MONTHLY_NUC_DECOM_FUND_NET_OF_TAX_RETURN(MO)
            ENDDO
            MONTHLY_NUC_DECOM_FUND_RETURN(0) = 
     +                            SUM(MONTHLY_NUC_DECOM_FUND_RETURN(1:))
            MONTHLY_OCI_NUC_DECOM_FUND_RETURN(0) =
     +                        SUM(MONTHLY_OCI_NUC_DECOM_FUND_RETURN(1:))
            MONTHLY_NUC_DECOM_FUND_NET_OF_TAX_RETURN(0) = 
     +                 SUM(MONTHLY_NUC_DECOM_FUND_NET_OF_TAX_RETURN(1:))
            RETIREMENT_MEDICAL_FUND_RETURN(0) =
     +                           SUM(RETIREMENT_MEDICAL_FUND_RETURN(1:))
            OCI_RETIREMENT_MEDICAL_FUND_RETURN(0) =
     +                       SUM(OCI_RETIREMENT_MEDICAL_FUND_RETURN(1:))
C
            R_MONTHLY_NUC_DECOM_FUND_RETURN =
     +                                  MONTHLY_NUC_DECOM_FUND_RETURN(0)
            R_OCI_NUCL_FUND_RETURN =
     +                              MONTHLY_OCI_NUC_DECOM_FUND_RETURN(0)
            R_MONTHLY_NUC_DECOM_FUND_NET_OF_TAX_RETURN = 
     +                       MONTHLY_NUC_DECOM_FUND_NET_OF_TAX_RETURN(0) 
            R_RETIREMENT_MEDICAL_FUND_RETURN =
     +                                 RETIREMENT_MEDICAL_FUND_RETURN(0)
            R_OCI_RETIREMENT_MEDICAL_FUND_RETURN =
     +                             OCI_RETIREMENT_MEDICAL_FUND_RETURN(0)
      RETURN
C**********************************************************************
      ENTRY MONTHLY_FASB143_CALCULATIONS(CLASS_ID,R_YR,
     +                                R_FASB143_ARO_INTEREST_ACCREATION,
     +                                R_FASB143_LT_LIABILITY_VALUE,
     +                                R_FASB143_NET_ASSET_VALUE,
     +                                R_ARO_CASH_PAYMENTS,
     +                                R_ARO_TRUST_CASH_PAYMENTS)
C***********************************************************************
         CALL RETURN_MONTHLY_FASB_ADDENDUMS(R_YR,CLASS_ID,
     +                          NucDecom_Discount_Rate,
     +                          FASB143_MONTHLY_ARO_INTEREST_ACCREATION,
     +                          FASB143_MONTHLY_ARO_CASH_PAYMENTS,
     +                          FASB143_MONTHLY_ARO_TRUST_CASH_PAYMENTS,
     +                          ARO_MONTHLY_NET_ASSETS_BAL_ADJ,
     +                          ARO_MONTHLY_LIABILITY_BAL_ADJ)
         NUC_DECOMMISSIONING_COST = 0.
         IF(SALT_RIVER_PROJECT()) THEN 
            CALL RETURN_SRP_NUC_DECOM_COST(R_YR,CLASS_ID,
     +                                  NUC_DECOMMISSIONING_COST)
         ELSE
c            NucDecom_Discount_Rate = 0.
         ENDIF
         FASB143_MONTHLY_LT_LIABILITY_VALUE(0) = 
     +                                   R_FASB143_LT_LIABILITY_VALUE(1)
         FASB143_MONTHLY_NET_ASSET_VALUE(0) = 
     +                                      R_FASB143_NET_ASSET_VALUE(1)
         DO MO = 1, 12 
         
            FASB143_MONTHLY_ARO_INTEREST_ACCREATION(MO) =
     +                     NucDecom_Discount_Rate(MO)/1200. *
     +                          FASB143_MONTHLY_LT_LIABILITY_VALUE(MO-1)
     +                     + FASB143_MONTHLY_ARO_INTEREST_ACCREATION(MO)
            FASB143_MONTHLY_LT_LIABILITY_VALUE(MO) =
     +                     FASB143_MONTHLY_LT_LIABILITY_VALUE(MO-1)
     +                     + FASB143_MONTHLY_ARO_INTEREST_ACCREATION(MO)
     +                     + ARO_MONTHLY_LIABILITY_BAL_ADJ(MO)
     +                     - FASB143_MONTHLY_ARO_CASH_PAYMENTS(MO)
     +                     - FASB143_MONTHLY_ARO_TRUST_CASH_PAYMENTS(MO)
            FASB143_MONTHLY_NET_ASSET_VALUE(MO) = 
     +                             FASB143_MONTHLY_NET_ASSET_VALUE(MO-1)
     +                              - NUC_DECOMMISSIONING_COST(MO)
     +                              + ARO_MONTHLY_NET_ASSETS_BAL_ADJ(MO)
         ENDDO
         R_FASB143_LT_LIABILITY_VALUE(2) =
     +                            FASB143_MONTHLY_LT_LIABILITY_VALUE(12)
         R_FASB143_NET_ASSET_VALUE(2) =
     +                               FASB143_MONTHLY_NET_ASSET_VALUE(12) 
         R_FASB143_ARO_INTEREST_ACCREATION = 
     +                  SUM(FASB143_MONTHLY_ARO_INTEREST_ACCREATION(1:))
         FASB143_MONTHLY_ARO_INTEREST_ACCREATION(0) =
     +                                 R_FASB143_ARO_INTEREST_ACCREATION
         R_ARO_CASH_PAYMENTS = FASB143_MONTHLY_ARO_CASH_PAYMENTS(0)
         R_ARO_TRUST_CASH_PAYMENTS =
     +                        FASB143_MONTHLY_ARO_TRUST_CASH_PAYMENTS(0)
      RETURN
C***********************************************************************
      ENTRY CPL_TAX_REPORT(ANNUAL_VARS)
C***********************************************************************
C
C OPERATING TAXES
C
         OUTPUT_CLASS_ID = ANNUAL_VARS(441)
         DO MO = 0, 12
C
C ATL ITEMS
C
            IF(MO == 0) THEN
               CPL_TAX__VARIABLES(MO,tax_tax_depreciation,ATL) =
     +                                                 -ANNUAL_VARS(130) ! Tax Depreciation
               CPL_TAX__VARIABLES(MO,tax_construction_expenses,ATL) =
     +                                                 -ANNUAL_VARS(228) ! Construction Expenses
               CPL_TAX__VARIABLES(MO,tax_foreign_tax_cred,ATL) = 0.
               CPL_TAX__VARIABLES(MO,tax_possessions_tax_cred,ATL)=0.
               CPL_TAX__VARIABLES(MO,tax_sec_29_creds_used,BTL) =
     +                                                  ANNUAL_VARS(373) ! Sec. 29 Federal Tax Credits Used ($M)  
               CPL_TAX__VARIABLES(MO,tax_invesment_tax_cred,ATL) = 0.
               CPL_TAX__VARIABLES(MO,tax_sect_43_creds_used,ATL) =
     +                                                  ANNUAL_VARS(341) ! Federal Tax Credits Used ($M)
               CPL_TAX__VARIABLES(MO,tax_amt_creds_used,ATL) = 0.
               CPL_TAX__VARIABLES(MO,tax_adjustments_2_taxes,ATL) =
     +                                                  ANNUAL_VARS(138) ! Federal Tax Adjustments
               CPL_TAX__VARIABLES(MO,tax_m1_current_deductions,ATL) =
     +                                                  ANNUAL_VARS(137) ! Federal Tax Deductions 
               CPL_TAX__VARIABLES(MO,tax_fed_amt_tax,TOTAL) =
     +                                                  ANNUAL_VARS(355) ! Federal Tax Deductions 
               CPL_TAX__VARIABLES(MO,tax_capitalized_interest,ATL) =
     +                                                  ANNUAL_VARS(129) ! CAPITIALIZED INTEREST
               CPL_TAX__VARIABLES(MO,tax_net_salvage,ATL) =
     +                                                 -ANNUAL_VARS(356) ! NET SALVAGE
            ELSE
               CPL_TAX__VARIABLES(MO,tax_tax_depreciation,ATL) =
     +                                             -ANNUAL_VARS(130)/12. ! Tax Depreciation
               CPL_TAX__VARIABLES(MO,tax_construction_expenses,ATL) =
     +                                             -ANNUAL_VARS(228)/12. ! Construction Expenses
               CPL_TAX__VARIABLES(MO,tax_sec_29_creds_used,BTL) =
     +                                              ANNUAL_VARS(373)/12. ! Sec. 29 Federal Tax Credits Used ($M)  
               CPL_TAX__VARIABLES(MO,tax_sect_43_creds_used,ATL) =
     +                                              ANNUAL_VARS(341)/12. ! Federal Tax Credits Used ($M)
               CPL_TAX__VARIABLES(MO,tax_adjustments_2_taxes,ATL) =
     +                                              ANNUAL_VARS(138)/12. ! Federal Tax Adjustments
               CPL_TAX__VARIABLES(MO,tax_m1_current_deductions,ATL) =
     +                                              ANNUAL_VARS(137)/12. ! Federal Tax Deductions 
               CPL_TAX__VARIABLES(MO,tax_capitalized_interest,ATL) =
     +                                              ANNUAL_VARS(129)/12. ! CAPITIALIZED INTEREST
               CPL_TAX__VARIABLES(MO,tax_net_salvage,ATL) =
     +                                             -ANNUAL_VARS(356)/12. ! NET SALVAGE
            ENDIF
            CPL_TAX__VARIABLES(MO,tax_tax_depreciation,BTL) = 0.
            CPL_TAX__VARIABLES(MO,tax_construction_expenses,BTL) = 0.
            CPL_TAX__VARIABLES(MO,tax_capitalized_interest,BTL) = 0.
            CPL_TAX__VARIABLES(MO,tax_net_salvage,BTL) = 0.
            CPL_TAX__VARIABLES(MO,tax_book_nclr_fuel,BTL) = 0.
C
C
C
            CPL_TAX__VARIABLES(MO,income_b4_income_taxes,ATL) =
     +          INCOME_VARIABLES(MO,monthly_total_revenues) -
     +          INCOME_VARIABLES(MO,mthly_pretax_expenses) -
     +          INCOME_VARIABLES(MO,Monthly_State_Tax_on_Capital) -
     +          INCOME_VARIABLES(MO,Monthly_Property_Taxes) -
     +          INCOME_VARIABLES(MO,Monthly_Federal_Tax_on_Capital) -
     +          INCOME_VARIABLES(MO,monthly_oth_taxes) -
     +          INCOME_VARIABLES(MO,Monthly_Operating_Revenue_Tax)
            CPL_TAX__VARIABLES(MO,tax_afudc_equity,ATL) = 
     +                         INCOME_VARIABLES(MO,Monthly_AFUDC_Equity)
            CPL_TAX__VARIABLES(MO,tax_afudc_borrowed,ATL) =
     +                       INCOME_VARIABLES(MO,Monthly_AFUDC_Borrowed)
            CPL_TAX__VARIABLES(MO,tax_total_afudc,ATL) = 
     +                  -(CPL_TAX__VARIABLES(MO,tax_afudc_equity,ATL) +
     +                    CPL_TAX__VARIABLES(MO,tax_afudc_borrowed,ATL))

            CPL_TAX__VARIABLES(MO,tax_interest_expenses,ATL) =
     +       -(INCOME_VARIABLES(MO,Monthly_LTD_Total_Interest) ! NEED THIS VALUE WITHOUT AMORT
     +         + INCOME_VARIABLES(MO,MonthlySTDInterest)
     +         + INCOME_VARIABLES(MO,mty_intst_on_nts_pyble))

            CPL_TAX__VARIABLES(MO,tax_interest_adjustment,ATL) = 0.
            CPL_TAX__VARIABLES(MO,
     +                         tax_interest_2_assoc_comp,ATL) = 0.
            CPL_TAX__VARIABLES(MO,tax_book_depreciation,ATL) =
     +                    INCOME_VARIABLES(MO,Monthly_Book_Depreciation)
            CPL_TAX__VARIABLES(MO,tax_amortization,ATL) =
     +                    INCOME_VARIABLES(MO,Monthly_Amortization)
            CPL_TAX__VARIABLES(MO,tax_interest_amortization,ATL) =
     +                   INCOME_VARIABLES(MO,Monthly_LTD_Amort_Interest)
            CPL_TAX__VARIABLES(MO,tax_book_nclr_fuel,ATL) =
     +                 INCOME_VARIABLES(MO,Monthly_Nuclear_Fuel_Expense)
            CPL_TAX__VARIABLES(MO,tax_state_income_tax,ATL) =
     +                  TAX_VARIABLES(MO,monthly_atl_state_income_taxes)
C
C TAX TIMING
C
            CPL_TAX__VARIABLES(MO,temp_tax_differences,ATL) =
     +                   TAX_VARIABLES(MO,monthly_temporary_atl_tax_dif)
            CPL_TAX__VARIABLES(MO,perm_tax_differences,ATL) =
     +                   TAX_VARIABLES(MO,monthly_permanent_atl_tax_dif)
c           CPL_TAX__VARIABLES(MO,,ATL) =
c    +                    INCOME_VARIABLES(MO,)
C
C BTL ITEMS
C
            CPL_TAX__VARIABLES(MO,income_b4_income_taxes,BTL) =
     +          INCOME_VARIABLES(MO,BTL_monthly_other_income)
     +          + INCOME_VARIABLES(MO,mty_totl_lti_income)
     +          + INCOME_VARIABLES(MO,BTL_Monthly_STInvestmet_Income)
     +          - INCOME_VARIABLES(MO,BTL_Monthly_Expenses)
     +          - INCOME_VARIABLES(MO,BTL_Monthly_Amortization)
            CPL_TAX__VARIABLES(MO,tax_afudc_equity,BTL) = 0.
            CPL_TAX__VARIABLES(MO,tax_afudc_borrowed,BTL) = 0.
            CPL_TAX__VARIABLES(MO,tax_total_afudc,BTL) = 0.
            CPL_TAX__VARIABLES(MO,tax_interest_expenses,BTL) = 0.
            CPL_TAX__VARIABLES(MO,tax_interest_adjustment,BTL) = 0.
            CPL_TAX__VARIABLES(MO,
     +                         tax_interest_2_assoc_comp,BTL) = 0.
            CPL_TAX__VARIABLES(MO,tax_book_depreciation,BTL) = 0.
            CPL_TAX__VARIABLES(MO,tax_amortization,BTL) =
     +                    INCOME_VARIABLES(MO,BTL_Monthly_Amortization)
            CPL_TAX__VARIABLES(MO,tax_interest_amortization,BTL) = 0.
C
            CPL_TAX__VARIABLES(MO,temp_tax_differences,BTL) =
     +                   TAX_VARIABLES(MO,monthly_temporary_btl_tax_dif)
            CPL_TAX__VARIABLES(MO,perm_tax_differences,BTL) =
     +                   TAX_VARIABLES(MO,monthly_permanent_btl_tax_dif)
            CPL_TAX__VARIABLES(MO,tax_state_income_tax,BTL) =
     +                  TAX_VARIABLES(MO,monthly_btl_state_income_taxes)
C 
C
            DO VAR_TYPE = income_b4_income_taxes, 
     +                                  tax_interest_2_assoc_comp
               CPL_TAX__VARIABLES(MO,tax_ttl_income_b4_taxes,ATL) = 
     +            CPL_TAX__VARIABLES(MO,tax_ttl_income_b4_taxes,ATL) +
     +                               CPL_TAX__VARIABLES(MO,VAR_TYPE,ATL)
               CPL_TAX__VARIABLES(MO,tax_ttl_income_b4_taxes,BTL) = 
     +            CPL_TAX__VARIABLES(MO,tax_ttl_income_b4_taxes,BTL) +
     +                               CPL_TAX__VARIABLES(MO,VAR_TYPE,BTL)
            ENDDO
C
C
C
            DO VAR_TYPE = tax_book_depreciation,
     +                                        tax_taken_4_book_totl-1
               CPL_TAX__VARIABLES(MO,tax_taken_4_book_totl,ATL) = 
     +            CPL_TAX__VARIABLES(MO,tax_taken_4_book_totl,ATL) +
     +                               CPL_TAX__VARIABLES(MO,VAR_TYPE,ATL)
               CPL_TAX__VARIABLES(MO,tax_taken_4_book_totl,BTL) = 
     +            CPL_TAX__VARIABLES(MO,tax_taken_4_book_totl,BTL) +
     +                               CPL_TAX__VARIABLES(MO,VAR_TYPE,BTL)
            ENDDO
C
C
C
            DO VAR_TYPE = tax_tax_depreciation,tax_taken_for_tax_total-1
               CPL_TAX__VARIABLES(MO,tax_taken_for_tax_total,ATL) = 
     +            CPL_TAX__VARIABLES(MO,tax_taken_for_tax_total,ATL) +
     +                               CPL_TAX__VARIABLES(MO,VAR_TYPE,ATL)
               CPL_TAX__VARIABLES(MO,tax_taken_for_tax_total,BTL) = 
     +            CPL_TAX__VARIABLES(MO,tax_taken_for_tax_total,BTL) +
     +                               CPL_TAX__VARIABLES(MO,VAR_TYPE,BTL)
            ENDDO
C
C TAX RATES
C
            DO VAR = ATL, BTL
               
               CPL_TAX__VARIABLES(MO,tax_fed_tax_rate,VAR) = 
     +                                                  ANNUAL_VARS(213) ! Federal Tax Rate
               CPL_TAX__VARIABLES(MO,tax_capl_gains_tax_rate,VAR)=20.
            
C
C REPORT SUB-TOTALS
C
               CPL_TAX__VARIABLES(MO,taxable_income_b4_state_taxes,VAR)= 
     +            CPL_TAX__VARIABLES(MO,tax_ttl_income_b4_taxes,VAR) +
     +            CPL_TAX__VARIABLES(MO,tax_taken_4_book_totl,VAR) + 
     +            CPL_TAX__VARIABLES(MO,tax_taken_for_tax_total,VAR) +
     +            CPL_TAX__VARIABLES(MO,perm_tax_differences,VAR) +
     +            CPL_TAX__VARIABLES(MO,temp_tax_differences,VAR)
C
              CPL_TAX__VARIABLES(MO,current_fedl_taxable_income,VAR)=
     +         CPL_TAX__VARIABLES(MO,taxable_income_b4_state_taxes,VAR)- 
     +         CPL_TAX__VARIABLES(MO,tax_state_income_tax,VAR) -
     +         CPL_TAX__VARIABLES(MO,tax_capital_gains_income,VAR) -    
     +         CPL_TAX__VARIABLES(MO,tax_nols_used,VAR) -
     +         CPL_TAX__VARIABLES(MO,tax_m1_current_deductions,VAR)
C
               CPL_TAX__VARIABLES(MO,tax_fed_tax_b4_creds,VAR) =
     +        CPL_TAX__VARIABLES(MO,current_fedl_taxable_income,VAR)*
     +          CPL_TAX__VARIABLES(MO,tax_fed_tax_rate,VAR)/100.+
     +        CPL_TAX__VARIABLES(MO,tax_capital_gains_income,VAR) *     
     +        CPL_TAX__VARIABLES(MO,tax_capl_gains_tax_rate,VAR)/100.
C
               CPL_TAX__VARIABLES(MO,tax_totl_creds_used,VAR) =
     +          CPL_TAX__VARIABLES(MO,tax_foreign_tax_cred,VAR) +
     +          CPL_TAX__VARIABLES(MO,tax_possessions_tax_cred,VAR) +
     +          CPL_TAX__VARIABLES(MO,tax_sec_29_creds_used,VAR) +
     +          CPL_TAX__VARIABLES(MO,tax_invesment_tax_cred,VAR) +
     +          CPL_TAX__VARIABLES(MO,tax_sect_43_creds_used,VAR) +
     +          CPL_TAX__VARIABLES(MO,tax_amt_creds_used,VAR)
C
               CPL_TAX__VARIABLES(MO,tax_fed_tax_aftr_creds,VAR)=
     +            CPL_TAX__VARIABLES(MO,tax_fed_tax_b4_creds,VAR)-
     +            CPL_TAX__VARIABLES(MO,tax_totl_creds_used,VAR) -
     +            CPL_TAX__VARIABLES(MO,tax_adjustments_2_taxes,VAR)
               
               CPL_TAX__VARIABLES(MO,tax_currnt_fed_income_tax,VAR) =
     +          CPL_TAX__VARIABLES(MO,tax_fed_tax_aftr_creds,VAR)
            ENDDO
c    +       CPL_TAX__VARIABLES(MO,,ATL) -
c    +       CPL_TAX__VARIABLES(MO,,ATL)  
C
C TOTAL ITEMS
C
            DO VAR_TYPE = 1, CPL_TAX_VARS
               IF(VAR_TYPE == tax_fed_amt_tax) CYCLE
               CPL_TAX__VARIABLES(MO,VAR_TYPE,TOTAL) = 
     +                             CPL_TAX__VARIABLES(MO,VAR_TYPE,ATL) +
     +                             CPL_TAX__VARIABLES(MO,VAR_TYPE,BTL)
               IF(VAR_TYPE == tax_fed_tax_rate) 
     +               CPL_TAX__VARIABLES(MO,tax_fed_tax_rate,TOTAL) = 
     +                                                  ANNUAL_VARS(213) ! Federal Tax Rate
               IF(VAR_TYPE == tax_capl_gains_tax_rate)     
     +               CPL_TAX__VARIABLES(MO,
     +                               tax_capl_gains_tax_rate,VAR)=20.
            ENDDO
         ENDDO
         DO MO = 0, 12
            IF(MO == 0) THEN
               WRITE(CPL_TAX_UNIT) ! ,REC=THIS_YEARS_RECORD)
     +                     PRT_ENDPOINT(),
     +                     FLOAT(BASE_YEAR+YR),
     +                     ASSET_CLASS_NAME,
     +                     LONG_MONTH_NAMES(MO),
     +                     TAX_TYPE_REPORTED(ATL),
     +                     OUTPUT_CLASS_ID,
     +                    (CPL_TAX__VARIABLES(MO,VAR_TYPE,ATL),
     +                                          VAR_TYPE=1,CPL_TAX_VARS)
C              THIS_YEARS_RECORD = THIS_YEARS_RECORD - 13
            ELSE
               WRITE(CPL_TAX_UNIT)
     +                     PRT_ENDPOINT(),
     +                     FLOAT(BASE_YEAR+YR),
     +                     ASSET_CLASS_NAME,
     +                     LONG_MONTH_NAMES(MO),
     +                     TAX_TYPE_REPORTED(ATL),
     +                     OUTPUT_CLASS_ID,
     +                    (CPL_TAX__VARIABLES(MO,VAR_TYPE,ATL),
     +                                          VAR_TYPE=1,CPL_TAX_VARS)
            ENDIF
            WRITE(CPL_TAX_UNIT)
     +                     PRT_ENDPOINT(),
     +                     FLOAT(BASE_YEAR+YR),
     +                     ASSET_CLASS_NAME,
     +                     LONG_MONTH_NAMES(MO),
     +                     TAX_TYPE_REPORTED(BTL),
     +                     OUTPUT_CLASS_ID,
     +                    (CPL_TAX__VARIABLES(MO,VAR_TYPE,BTL),
     +                                          VAR_TYPE=1,CPL_TAX_VARS)
            WRITE(CPL_TAX_UNIT)
     +                     PRT_ENDPOINT(),
     +                     FLOAT(BASE_YEAR+YR),
     +                     ASSET_CLASS_NAME,
     +                     LONG_MONTH_NAMES(MO),
     +                     TAX_TYPE_REPORTED(TOTAL),
     +                     OUTPUT_CLASS_ID,
     +                    (CPL_TAX__VARIABLES(MO,VAR_TYPE,TOTAL),
     +                                          VAR_TYPE=1,CPL_TAX_VARS)
         ENDDO
      RETURN
      END
C***********************************************************************
      FUNCTION ALLOCATE_ACCOUNT_BALANCE(ANNUAL_AMOUNT,
     +                                  OUTPUT_VARIABLE)
C***********************************************************************
C
      REAL*4 ALLOCATE_ACCOUNT_BALANCE
      REAL*4 OUTPUT_VARIABLE(0:12),ANNUAL_AMOUNT,UNALLOCATED_AMOUNT
      INTEGER*2 MO
C
         UNALLOCATED_AMOUNT = ANNUAL_AMOUNT - OUTPUT_VARIABLE(0)
         ALLOCATE_ACCOUNT_BALANCE = UNALLOCATED_AMOUNT
         IF(UNALLOCATED_AMOUNT /= 0.) THEN
            OUTPUT_VARIABLE(0) = ANNUAL_AMOUNT
c            DO MO = 1, 12
            OUTPUT_VARIABLE(1:) = OUTPUT_VARIABLE(1:) 
     +                           + UNALLOCATED_AMOUNT/12.
c            ENDDO
         ENDIF
      RETURN
      END
C***********************************************************************
      FUNCTION MONTHLY_NON_INCOME_TAX_PROCS(MAX_LINKED_LEVEL)
C***********************************************************************
C
C ALLOCATED TO THE REVENUE TAX TO  MONTHS UNTIL SPECIFIC MONTHLY INFO IS AVAILABLE
C
      use spindriftlib
      use prod_arrays_dimensions
      USE SIZECOM         

      INCLUDE 'NAMESCOM.MON'
      INCLUDE 'MTHNMCOM.MON'
      INCLUDE 'FINACCOM.MON'

      INTEGER VALUES_2_SET
      LOGICAL*1 SALT_RIVER_PROJECT
      INTEGER*2 MO,MAX_LINKED_LEVEL,CLASS_LEVEL,MONTH
      CHARACTER*1 REVENUE_TAX_BASIS
      REAL*4 EXCLUDED_OTHER_TAXES_REV(:,:),
     +       EXCLUDED_OTHER_TAXES_EXP(:,:),
     +       OTHER_TAXES_MOVING_UP(:,:)
      REAL*4 EXCLUDED_REVENUE_TAX_BASIS(:,:),
     +       EXCLUDED_REVENUE_TAX_BASIS_A(:,:),
     +       EXCLUDED_REVENUE_TAX_BASIS_B(:,:),
     +       REVENUE_TAXES_MOVING_UP(:,:)
      ALLOCATABLE :: EXCLUDED_OTHER_TAXES_REV,
     +               EXCLUDED_OTHER_TAXES_EXP,
     +               OTHER_TAXES_MOVING_UP,
     +               EXCLUDED_REVENUE_TAX_BASIS,
     +               EXCLUDED_REVENUE_TAX_BASIS_A,
     +               EXCLUDED_REVENUE_TAX_BASIS_B,
     +               REVENUE_TAXES_MOVING_UP
      REAL*4 MONTHLY_OTHER_TAXES,
     +       CLASS_NON_VARIABLE_EXPENSE(0:12),
     +       CLASS_OPERATING_REVENUES(0:12),
     +       MONTHLY_OTHER_REV_TAX_RATE(0:12),
     +       MONTHLY_OTHER_EXP_TAX_RATE(0:12)
      REAL*4 OTHER_TAXES(0:12)
      REAL*4 MONTHLY_NON_INCOME_TAX_PROCS
      REAL*4 ALLOCATE_REVENUE_TAX,
     +       CALC_LEVEL_REVENUE_BASED_TAX,
     +       CALCULATE_REVENUE_TAXES,
     +       SAVE_SBU_TAX_EXCLUSIONS,
     +       ZERO_TAX_EXCLUSIONS
      REAL*4 INCOME_VARIABLES(0:12,*),
     +       OTHER_TAX_ADJUSTMENT(0:12),
     +       REVENUE_TAX_RATE(0:12),
     +       REVENUE_TAX_ADJUSTMENT(0:12),
     +       MONTHLY_REVENUE_TAXES(0:12)
      REAL*4 MONTHLY_REVENUE_TAX,
     +       CLASS_REVENUE_TAX_REVENUES(0:12),
     +       CLASS_REVENUE_TAX_EXCLUDED_REVS(0:12),
     +       REVENUE_TAX_ALLOCATOR
      LOGICAL*1 USE_REVENUES_TO_ALLOCATE
      LOGICAL*1 EXCLUDE_SBU_REVENUE_TAX,
     +          EXCLUDE_SBU_OTHER_TAXES,
     +          EXCLUDE_SBU_PROPERTY_TAX,
     +          EXCLUDE_SBU_STATE_INCOME_TAX,
     +          EXCLUDE_SBU_CAPITAL_TAX
      REAL*4 RPT_CLASS_OPERATING_REVENUES,      
     +       RPT_EXCLUDED_OTHER_TAXES_REV,      
     +       RPT_CLASS_NON_VARIABLE_EXPENSE,    
     +       RPT_EXCLUDED_OTHER_TAXES_EXP,      
     +       RPT_OTHER_TAXES_MOVING_UP,          
     +       RPT_CLASS_REVENUE_TAX_REVENUES,                
     +       RPT_CLASS_REV_TAX_EXCLUDED_REVS,               
     +       RPT_REVENUE_TAXES_MOVING_UP
C
      SAVE MONTHLY_REVENUE_TAX,
     +     REVENUE_TAX_ALLOCATOR,
     +     USE_REVENUES_TO_ALLOCATE,
     +     CLASS_NON_VARIABLE_EXPENSE,
     +     CLASS_OPERATING_REVENUES,
     +     CLASS_REVENUE_TAX_REVENUES,
     +     CLASS_REVENUE_TAX_EXCLUDED_REVS,
     +     MONTHLY_REVENUE_TAXES
      SAVE EXCLUDED_OTHER_TAXES_REV,
     +     EXCLUDED_OTHER_TAXES_EXP,
     +     OTHER_TAXES_MOVING_UP,
     +     EXCLUDED_REVENUE_TAX_BASIS,
     +     EXCLUDED_REVENUE_TAX_BASIS_A,
     +     EXCLUDED_REVENUE_TAX_BASIS_B,
     +     REVENUE_TAXES_MOVING_UP
C
         MONTHLY_NON_INCOME_TAX_PROCS = 1.
         IF(ALLOCATED(EXCLUDED_OTHER_TAXES_REV)) 
     +                            DEALLOCATE(EXCLUDED_OTHER_TAXES_REV,
     +                                       EXCLUDED_OTHER_TAXES_EXP,
     +                                       OTHER_TAXES_MOVING_UP)
         ALLOCATE(EXCLUDED_OTHER_TAXES_REV(0:12,-1:MAX_LINKED_LEVEL))
         ALLOCATE(EXCLUDED_OTHER_TAXES_EXP(0:12,-1:MAX_LINKED_LEVEL))
         ALLOCATE(OTHER_TAXES_MOVING_UP(0:12,-1:MAX_LINKED_LEVEL))
         IF(ALLOCATED(EXCLUDED_REVENUE_TAX_BASIS))
     +                          DEALLOCATE(EXCLUDED_REVENUE_TAX_BASIS,
     +                                     EXCLUDED_REVENUE_TAX_BASIS_A,
     +                                     EXCLUDED_REVENUE_TAX_BASIS_B,
     +                                     REVENUE_TAXES_MOVING_UP)
         ALLOCATE(EXCLUDED_REVENUE_TAX_BASIS(0:12,-1:MAX_LINKED_LEVEL))
         ALLOCATE(EXCLUDED_REVENUE_TAX_BASIS_A(0:12,
     +                                             -1:MAX_LINKED_LEVEL))
         ALLOCATE(EXCLUDED_REVENUE_TAX_BASIS_B(0:12,
     +                                             -1:MAX_LINKED_LEVEL))
         ALLOCATE(REVENUE_TAXES_MOVING_UP(0:12,-1:MAX_LINKED_LEVEL))
         EXCLUDED_OTHER_TAXES_REV = 0.
         EXCLUDED_OTHER_TAXES_EXP = 0.
         OTHER_TAXES_MOVING_UP = 0.
C
         EXCLUDED_REVENUE_TAX_BASIS = 0.
         EXCLUDED_REVENUE_TAX_BASIS_A = 0.
         EXCLUDED_REVENUE_TAX_BASIS_B = 0.
         REVENUE_TAXES_MOVING_UP = 0.
C
      RETURN
C***********************************************************************
      ENTRY CALCULATE_REVENUE_TAXES(MO,CLASS_LEVEL,
     +                         INCOME_VARIABLES,
     +                         REVENUE_TAX_ADJUSTMENT,
     +                         REVENUE_TAX_RATE)
C***********************************************************************
         IF(SALT_RIVER_PROJECT()) THEN
            CLASS_REVENUE_TAX_REVENUES(MO) =
     +                          INCOME_VARIABLES(MO,Total Base Revenues)
     +                          + INCOME_VARIABLES(MO,AdjustmentClause) 
     +                          + INCOME_VARIABLES(MO,DeferredRevenues)
     +                          + INCOME_VARIABLES(MO,PGAAdjustment)
            CLASS_REVENUE_TAX_EXCLUDED_REVS(MO) =
     +                   - EXCLUDED_REVENUE_TAX_BASIS(MO,CLASS_LEVEL)
         ELSEIF(REVENUE_TAX_BASIS()=='A') THEN
            CLASS_REVENUE_TAX_REVENUES(MO) = 
     +                          INCOME_VARIABLES(MO,Total Base Revenues)
     +                          + INCOME_VARIABLES(MO,AdjustmentClause)
     +                          + INCOME_VARIABLES(MO,PGAAdjustment)
            CLASS_REVENUE_TAX_EXCLUDED_REVS(MO) =
     +                   EXCLUDED_REVENUE_TAX_BASIS_A(MO,CLASS_LEVEL)
         ELSEIF(REVENUE_TAX_BASIS() == 'B') THEN
            CLASS_REVENUE_TAX_REVENUES(MO) =
     +                          INCOME_VARIABLES(MO,Total Base Revenues)
            CLASS_REVENUE_TAX_EXCLUDED_REVS(MO) =
     +                   EXCLUDED_REVENUE_TAX_BASIS_B(MO,CLASS_LEVEL)
         ELSE
            CLASS_REVENUE_TAX_REVENUES(MO) = 
     +                   INCOME_VARIABLES(MO,TOTAL OPERATING REVENUES)
            CLASS_REVENUE_TAX_EXCLUDED_REVS(MO) =
     +                     EXCLUDED_REVENUE_TAX_BASIS(MO,CLASS_LEVEL)
         ENDIF
         MONTHLY_REVENUE_TAXES(MO) =
     +             REVENUE_TAX_RATE(MO) *
     +                    (CLASS_REVENUE_TAX_REVENUES(MO)
     +                     - CLASS_REVENUE_TAX_EXCLUDED_REVS(MO))
     +             + REVENUE_TAX_ADJUSTMENT(MO)
     +             + REVENUE_TAXES_MOVING_UP(MO,CLASS_LEVEL)
         CALCULATE_REVENUE_TAXES = MONTHLY_REVENUE_TAXES(MO)
      RETURN
C***********************************************************************
      ENTRY ALLOCATE_REVENUE_TAX(MO,INCOME_VARIABLES)
      ENTRY CALC_LEVEL_REVENUE_BASED_TAX(MO,
     +                                   INCOME_VARIABLES,
     +                                   REVENUE_TAX_RATE,
     +                                   REVENUE_TAX_ADJUSTMENT)
C***********************************************************************
         IF(SALT_RIVER_PROJECT()) THEN
            CLASS_REVENUE_TAX_REVENUES(MO) =
     +                          INCOME_VARIABLES(MO,Total Base Revenues)
     +                          + INCOME_VARIABLES(MO,AdjustmentClause)
     +                          + INCOME_VARIABLES(MO,DeferredRevenues)
     +                          + INCOME_VARIABLES(MO,PGAAdjustment)
         ELSEIF(REVENUE_TAX_BASIS()=='A') THEN
            CLASS_REVENUE_TAX_REVENUES(MO) = 
     +                          INCOME_VARIABLES(MO,Total Base Revenues)
     +                          + INCOME_VARIABLES(MO,AdjustmentClause)
     +                          + INCOME_VARIABLES(MO,PGAAdjustment)
         ELSEIF(REVENUE_TAX_BASIS() == 'B') THEN
            CLASS_REVENUE_TAX_REVENUES(MO) =
     +                       INCOME_VARIABLES(MO,Total Base Revenues)
         ELSE
            CLASS_REVENUE_TAX_REVENUES(MO) = 
     +                  INCOME_VARIABLES(MO,TOTAL OPERATING REVENUES)
         ENDIF
C
         IF(MO == 0) THEN
            USE_REVENUES_TO_ALLOCATE = .TRUE.
            IF(CLASS_REVENUE_TAX_REVENUES(MO) /= 0.) THEN
               REVENUE_TAX_ALLOCATOR = 
     +             INCOME_VARIABLES(0,Monthly_Operating_Revenue_Tax)/
     +                        CLASS_REVENUE_TAX_REVENUES(MO)
               ALLOCATE_REVENUE_TAX = REVENUE_TAX_ALLOCATOR
            ELSE
               USE_REVENUES_TO_ALLOCATE = .FALSE.
               MONTHLY_REVENUE_TAX =
     +             INCOME_VARIABLES(0,Monthly_Operating_Revenue_Tax)/12.
               ALLOCATE_REVENUE_TAX = MONTHLY_REVENUE_TAX
            ENDIF
         ELSE
            IF(USE_REVENUES_TO_ALLOCATE) THEN
               ALLOCATE_REVENUE_TAX =  ! INCOME_VARIABLES(MO,Monthly Operating Revenue Tax) =
     +            REVENUE_TAX_ALLOCATOR * CLASS_REVENUE_TAX_REVENUES(MO)
            ELSE   
               ALLOCATE_REVENUE_TAX = ! INCOME_VARIABLES(MO,Monthly Operating Revenue Tax) =
     +                                          MONTHLY_REVENUE_TAX
            ENDIF
         ENDIF
         CALC_LEVEL_REVENUE_BASED_TAX = ALLOCATE_REVENUE_TAX
      RETURN
C***********************************************************************
      ENTRY MONTHLY_OTHER_TAXES(MO,CLASS_LEVEL,
     +                          INCOME_VARIABLES,
     +                          OTHER_TAX_ADJUSTMENT,
     +                          MONTHLY_OTHER_REV_TAX_RATE,
     +                          MONTHLY_OTHER_EXP_TAX_RATE)
C***********************************************************************
C
         CLASS_NON_VARIABLE_EXPENSE(MO) =
     +              INCOME_VARIABLES(MO,Cash_Fixed_OandM)
     +              + INCOME_VARIABLES(MO,Cash_Other_OandM)
     +              + INCOME_VARIABLES(MO,CashPurchasedGas)
     +              + INCOME_VARIABLES(MO,Cash_Other)
     +              + INCOME_VARIABLES(MO,CashTransmissionOperation)
     +              + INCOME_VARIABLES(MO,CashTransmissionMaintenance)
     +              + INCOME_VARIABLES(MO,CashDistributionOperation)
     +              + INCOME_VARIABLES(MO,CashDistributionMaintenance)
     +              + INCOME_VARIABLES(MO,CashCustomerAccounts)
     +              + INCOME_VARIABLES(MO,CashCustomerServices)
     +              + INCOME_VARIABLES(MO,CashSalesExpense)
     +              + INCOME_VARIABLES(MO,CashAGOperations)
     +              + INCOME_VARIABLES(MO,Cash_AG_Maintenance)
     +              + INCOME_VARIABLES(MO,CashDSMExpense)
     +              + INCOME_VARIABLES(MO,CashDSMRebate)
C                   
         CLASS_OPERATING_REVENUES(MO) =
     +                     INCOME_VARIABLES(MO,TOTAL OPERATING REVENUES)
C
         OTHER_TAXES(MO) =
     +              MONTHLY_OTHER_REV_TAX_RATE(MO) *
     +                      (CLASS_OPERATING_REVENUES(MO)
     +                       - EXCLUDED_OTHER_TAXES_REV(MO,CLASS_LEVEL))
     +              + MONTHLY_OTHER_EXP_TAX_RATE(MO) *
     +                      (CLASS_NON_VARIABLE_EXPENSE(MO)
     +                       - EXCLUDED_OTHER_TAXES_EXP(MO,CLASS_LEVEL))
     +              + OTHER_TAX_ADJUSTMENT(MO)
     +              + OTHER_TAXES_MOVING_UP(MO,CLASS_LEVEL)
         MONTHLY_OTHER_TAXES = OTHER_TAXES(MO)
      RETURN
C***********************************************************************
      ENTRY SAVE_SBU_TAX_EXCLUSIONS(CLASS_LEVEL,INCOME_VARIABLES)
C***********************************************************************
         CALL RETURN_SBU_PASS_THROUGH_VALUES(EXCLUDE_SBU_REVENUE_TAX,
     +                                     EXCLUDE_SBU_OTHER_TAXES,
     +                                     EXCLUDE_SBU_PROPERTY_TAX,
     +                                     EXCLUDE_SBU_STATE_INCOME_TAX,
     +                                     EXCLUDE_SBU_CAPITAL_TAX)
         DO MONTH = 0, 12 
            IF(EXCLUDE_SBU_OTHER_TAXES) THEN
               EXCLUDED_OTHER_TAXES_REV(MONTH,CLASS_LEVEL-1) =
     +                     EXCLUDED_OTHER_TAXES_REV(MONTH,CLASS_LEVEL-1)
     +                     + CLASS_OPERATING_REVENUES(MONTH) 
               EXCLUDED_OTHER_TAXES_EXP(MONTH,CLASS_LEVEL-1) =
     +                     EXCLUDED_OTHER_TAXES_EXP(MONTH,CLASS_LEVEL-1)
     +                     + CLASS_NON_VARIABLE_EXPENSE(MONTH)
               OTHER_TAXES_MOVING_UP(MONTH,CLASS_LEVEL-1) =
     +                        OTHER_TAXES_MOVING_UP(MONTH,CLASS_LEVEL-1)
     +                        + OTHER_TAXES(MONTH)
            ENDIF
            IF(EXCLUDE_SBU_REVENUE_TAX) THEN
               IF(SALT_RIVER_PROJECT()) THEN
                  EXCLUDED_REVENUE_TAX_BASIS(MONTH,CLASS_LEVEL-1) =
     +                   EXCLUDED_REVENUE_TAX_BASIS(MONTH,CLASS_LEVEL-1)
     +                   + INCOME_VARIABLES(MONTH,Total Base Revenues)
     +                   + INCOME_VARIABLES(MONTH,AdjustmentClause) 
     +                   + INCOME_VARIABLES(MONTH,DeferredRevenues)
     +                   + INCOME_VARIABLES(MONTH,PGAAdjustment)
            
               ELSE
                  EXCLUDED_REVENUE_TAX_BASIS_A(MONTH,CLASS_LEVEL-1) =
     +                 EXCLUDED_REVENUE_TAX_BASIS_A(MONTH,CLASS_LEVEL-1)
     +                 + INCOME_VARIABLES(MONTH,Total Base Revenues)
     +                 + INCOME_VARIABLES(MONTH,AdjustmentClause)
     +                 + INCOME_VARIABLES(MONTH,PGAAdjustment)
                  EXCLUDED_REVENUE_TAX_BASIS_B(MONTH,CLASS_LEVEL-1) =
     +                 EXCLUDED_REVENUE_TAX_BASIS_B(MONTH,CLASS_LEVEL-1)
     +                 + INCOME_VARIABLES(MONTH,Total Base Revenues)
                  EXCLUDED_REVENUE_TAX_BASIS(MONTH,CLASS_LEVEL-1) =
     +                EXCLUDED_REVENUE_TAX_BASIS(MONTH,CLASS_LEVEL-1)
     +                + INCOME_VARIABLES(MONTH,TOTAL OPERATING REVENUES)
               ENDIF
               REVENUE_TAXES_MOVING_UP(MONTH,CLASS_LEVEL-1) =
     +                      REVENUE_TAXES_MOVING_UP(MONTH,CLASS_LEVEL-1)
     +                      + MONTHLY_REVENUE_TAXES(MONTH)
            ENDIF
         ENDDO
         SAVE_SBU_TAX_EXCLUSIONS = 1. 
         RETURN
C***********************************************************************
      ENTRY ZERO_TAX_EXCLUSIONS(CLASS_LEVEL)
C***********************************************************************
         EXCLUDED_OTHER_TAXES_REV(:,CLASS_LEVEL) = 0.
         EXCLUDED_OTHER_TAXES_EXP(:,CLASS_LEVEL) = 0.
         OTHER_TAXES_MOVING_UP(:,CLASS_LEVEL) = 0.
         EXCLUDED_REVENUE_TAX_BASIS(:,CLASS_LEVEL) = 0.
         EXCLUDED_REVENUE_TAX_BASIS_A(:,CLASS_LEVEL) = 0.
         EXCLUDED_REVENUE_TAX_BASIS_B(:,CLASS_LEVEL) = 0.
         REVENUE_TAXES_MOVING_UP(:,CLASS_LEVEL) = 0.
         ZERO_TAX_EXCLUSIONS = 1.
      RETURN
C***********************************************************************
      ENTRY RPT_CLASS_OPERATING_REVENUES(MO)
C***********************************************************************
         IF(MO == 0) THEN
            CLASS_OPERATING_REVENUES(0) =
     +                               SUM(CLASS_OPERATING_REVENUES(1:12)) 
         ENDIF
         RPT_CLASS_OPERATING_REVENUES = CLASS_OPERATING_REVENUES(MO)  
      RETURN      
C***********************************************************************
      ENTRY RPT_EXCLUDED_OTHER_TAXES_REV(MO,CLASS_LEVEL)
C***********************************************************************
         IF(MO == 0) THEN
            EXCLUDED_OTHER_TAXES_REV(MO,CLASS_LEVEL) =
     +                   SUM(EXCLUDED_OTHER_TAXES_REV(1:12,CLASS_LEVEL))
         ENDIF
         RPT_EXCLUDED_OTHER_TAXES_REV =
     +                          EXCLUDED_OTHER_TAXES_REV(MO,CLASS_LEVEL)
      RETURN      
C***********************************************************************
      ENTRY RPT_CLASS_NON_VARIABLE_EXPENSE(MO)
C***********************************************************************
         IF(MO == 0) THEN
            CLASS_NON_VARIABLE_EXPENSE(0) =
     +                             SUM(CLASS_NON_VARIABLE_EXPENSE(1:12))
         ENDIF
         RPT_CLASS_NON_VARIABLE_EXPENSE = CLASS_NON_VARIABLE_EXPENSE(MO)
      RETURN      
C***********************************************************************
      ENTRY RPT_EXCLUDED_OTHER_TAXES_EXP(MO,CLASS_LEVEL)
C***********************************************************************
         IF(MO== 0) THEN
            EXCLUDED_OTHER_TAXES_EXP(0,CLASS_LEVEL) = 
     +                   SUM(EXCLUDED_OTHER_TAXES_EXP(1:12,CLASS_LEVEL)) 
         ENDIF
         RPT_EXCLUDED_OTHER_TAXES_EXP =
     +                          EXCLUDED_OTHER_TAXES_EXP(MO,CLASS_LEVEL)
      RETURN      
C***********************************************************************
      ENTRY RPT_OTHER_TAXES_MOVING_UP(MO,CLASS_LEVEL)
C***********************************************************************
         IF(MO== 0) THEN
            OTHER_TAXES_MOVING_UP(MO,CLASS_LEVEL) =
     +                        SUM(OTHER_TAXES_MOVING_UP(1:,CLASS_LEVEL))
         ENDIF
         RPT_OTHER_TAXES_MOVING_UP = 
     +                             OTHER_TAXES_MOVING_UP(MO,CLASS_LEVEL)
      RETURN      
C***********************************************************************
      ENTRY RPT_CLASS_REVENUE_TAX_REVENUES(MO)
C***********************************************************************
         IF(MO== 0) THEN
            CLASS_REVENUE_TAX_REVENUES(0) =
     +                               SUM(CLASS_REVENUE_TAX_REVENUES(1:))
         ENDIF
         RPT_CLASS_REVENUE_TAX_REVENUES = CLASS_REVENUE_TAX_REVENUES(MO)
      RETURN      
C***********************************************************************
      ENTRY RPT_CLASS_REV_TAX_EXCLUDED_REVS(MO)
C***********************************************************************
         IF(MO== 0) THEN
            CLASS_REVENUE_TAX_EXCLUDED_REVS(0) =
     +                          SUM(CLASS_REVENUE_TAX_EXCLUDED_REVS(1:))
         ENDIF
         RPT_CLASS_REV_TAX_EXCLUDED_REVS =
     +                               CLASS_REVENUE_TAX_EXCLUDED_REVS(MO)
      RETURN      
C***********************************************************************
      ENTRY RPT_REVENUE_TAXES_MOVING_UP(MO,CLASS_LEVEL)
C***********************************************************************
         IF(MO== 0) THEN
            REVENUE_TAXES_MOVING_UP(0,CLASS_LEVEL) =
     +                      SUM(REVENUE_TAXES_MOVING_UP(1:,CLASS_LEVEL))
         ENDIF
         RPT_REVENUE_TAXES_MOVING_UP =
     +                           REVENUE_TAXES_MOVING_UP(MO,CLASS_LEVEL)
      RETURN      
      END
C***********************************************************************
      SUBROUTINE LAG_VALUES_ONE_MONTH(LAGGED_VALUES,
     +                                INPUT_VALUES,
     +                                CARRY_OVER)
C***********************************************************************
C
      REAL*4 CARRY_OVER(0:12),LAGGED_VALUES(0:12),INPUT_VALUES(0:12)
      INTEGER*2 MO
C
         DO MO = 1, 12
            IF(MO == 1) THEN
               LAGGED_VALUES(MO) = CARRY_OVER(MO)
            ELSE
               LAGGED_VALUES(MO) = INPUT_VALUES(MO-1) + CARRY_OVER(MO)
            ENDIF
            CARRY_OVER(MO) = 0.
            LAGGED_VALUES(0) = LAGGED_VALUES(0) + LAGGED_VALUES(MO)
            IF(MO == 12) CARRY_OVER(MO) = INPUT_VALUES(MO)
         ENDDO
C
      RETURN
      END
C***********************************************************************
      SUBROUTINE LAG_CASH_VALUES(LAGGED_VALUES,
     +                           INPUT_VALUES,
     +                           CARRY_OVER,
     +                           VARIABLE_TYPE,
     +                           CLASS_ID)
C***********************************************************************
C
      REAL*4 CARRY_OVER(0:12),LAGGED_VALUES(0:12),INPUT_VALUES(0:12)
      INTEGER*2 MO,LAG_PERIOD,GET_ACCUMULATION_PERIOD_INFO,
     +          VARIABLE_TYPE,CLASS_ID
      REAL*4 ACCUMULATED_VALUES
      LOGICAL*1 END_ACCUMULATION_PERIOD(12),DONT_LAG_DECEMBER,CPL_ACTIVE
C
         LAG_PERIOD=GET_ACCUMULATION_PERIOD_INFO(VARIABLE_TYPE,
     +                                          DONT_LAG_DECEMBER,
     +                                          END_ACCUMULATION_PERIOD)
         ACCUMULATED_VALUES = 0. ! CARRY_OVER(0)
         LAGGED_VALUES(0) = 0.
         DO MO = 1, 12
            ACCUMULATED_VALUES = ACCUMULATED_VALUES + INPUT_VALUES(MO)
            LAGGED_VALUES(MO) = LAGGED_VALUES(MO) + CARRY_OVER(MO)
            CARRY_OVER(MO) = 0.
            IF(END_ACCUMULATION_PERIOD(MO)) THEN
               IF(MO == 12 .AND. DONT_LAG_DECEMBER) THEN
                  LAGGED_VALUES(MO) = ACCUMULATED_VALUES
     +                                + LAGGED_VALUES(MO)
               ELSEIF(MO + LAG_PERIOD <= 12) THEN
                  LAGGED_VALUES(MO+LAG_PERIOD) = ACCUMULATED_VALUES +
     +                                      LAGGED_VALUES(MO+LAG_PERIOD)
               ELSE
                  CARRY_OVER(MO+LAG_PERIOD-12) = ACCUMULATED_VALUES
               ENDIF
               ACCUMULATED_VALUES = 0.
            ENDIF
            LAGGED_VALUES(0) = LAGGED_VALUES(0) + LAGGED_VALUES(MO)
         ENDDO
         CARRY_OVER(0) = ACCUMULATED_VALUES
C
      RETURN
C***********************************************************************
      ENTRY LAG_PROPERTY_TAX_CASH_VALUES(LAGGED_VALUES,
     +                                   INPUT_VALUES,
     +                                   CARRY_OVER,
     +                                   VARIABLE_TYPE,
     +                                   CLASS_ID)
C***********************************************************************
C
         ACCUMULATED_VALUES = CARRY_OVER(0)
         LAGGED_VALUES(0) = 0.
         IF(CPL_ACTIVE()) THEN
            DO MO = 1, 12
               ACCUMULATED_VALUES = ACCUMULATED_VALUES+INPUT_VALUES(MO)
               LAGGED_VALUES(MO) = LAGGED_VALUES(MO) + CARRY_OVER(MO)
               CARRY_OVER(MO) = 0.
               IF(MO == 12) THEN
                  LAGGED_VALUES(MO) = ACCUMULATED_VALUES/2.
     +                                + LAGGED_VALUES(MO)
                  CARRY_OVER(1) = ACCUMULATED_VALUES/2.
                  ACCUMULATED_VALUES = 0.
               ENDIF
               LAGGED_VALUES(0) = LAGGED_VALUES(0) + LAGGED_VALUES(MO)
            ENDDO
         ELSE
            LAG_PERIOD = GET_ACCUMULATION_PERIOD_INFO(VARIABLE_TYPE,
     +                                          DONT_LAG_DECEMBER,
     +                                          END_ACCUMULATION_PERIOD)
            ACCUMULATED_VALUES = 0.
            DO MO = 1, 12
               ACCUMULATED_VALUES = ACCUMULATED_VALUES+INPUT_VALUES(MO)
               LAGGED_VALUES(MO) = LAGGED_VALUES(MO) + CARRY_OVER(MO)
               CARRY_OVER(MO) = 0.
               IF(END_ACCUMULATION_PERIOD(MO)) THEN
                  IF(MO == 12 .AND. DONT_LAG_DECEMBER) THEN
                     LAGGED_VALUES(MO) = ACCUMULATED_VALUES
     +                                   + LAGGED_VALUES(MO)
                  ELSEIF(MO + LAG_PERIOD <= 12) THEN
                     LAGGED_VALUES(MO+LAG_PERIOD) = ACCUMULATED_VALUES +
     +                                      LAGGED_VALUES(MO+LAG_PERIOD)
                  ELSE
                     CARRY_OVER(MO+LAG_PERIOD-12) = ACCUMULATED_VALUES
                  ENDIF
                  ACCUMULATED_VALUES = 0.
               ENDIF
               LAGGED_VALUES(0) = LAGGED_VALUES(0) + LAGGED_VALUES(MO)
            ENDDO
         ENDIF
         CARRY_OVER(0) = ACCUMULATED_VALUES
      RETURN
      END
C***********************************************************************
      RECURSIVE FUNCTION SUM_MONTHLY_INCOME_STATEMENT(INCOME_VARIABLES)
C***********************************************************************
C
      use spindriftlib
      use prod_arrays_dimensions
      USE SIZECOM         
      use globecom
      use class_run_switchesc
      use class_run_switchesl1
      use class_run_switchesl4

      INCLUDE 'NAMESCOM.MON'
      INCLUDE 'MTHNMCOM.MON'
      INCLUDE 'FINACCOM.MON'
C
      INTEGER*2 MO
      INTEGER*2 R_MO
      REAL*4 INCOME_VARIABLES(0:12,*),
     +       CASH_VARIABLES(0:12,*),
     +       TAX_VARIABLES(0:12,*)
      LOGICAL*1 SUM_MONTHLY_INCOME_STATEMENT,
     +          SUM_MONTHLY_INCOME_REVENUES,
     +          SUM_MONTHLY_INCOME_EXPENSES,
     +          SUM_MONTHLY_TAXES_BTL_ITEMS,
     +          MONTHLY_TAXABLE_INCOME,
     +          UI,VOID_LOGICAL
      REAL*4 SUB_BTL_TAXABLE_EXPENSES(0:12),
     +       SUB_BTL_STATE_TAXES_PAID(0:12),
     +       SUB_BTL_STATE_TAXABLE_INCOME(0:12),
     +       SUB_BTL_TAXABLE_OTHER_INCOME(0:12),
     +       SUB_BTL_TAXABLE_INVEST_INCOME(0:12),
     +       SUB_BTL_MISC_DEDUCTIONS(0:12),
     +       SUB_BTL_FEDERAL_TAXES_PAID(0:12)
      REAL*4 R_ATL_TAXABLE_REVENUES(0:12),
     +       R_ATL_TAXABLE_BOOK_DEDUCTIONS(0:12),
     +       R_BTL_TAXABLE_REVENUES(0:12),
     +       R_BTL_EXPENSE_DEDUCTIONS(0:12),
     +       R_BTL_TAX_EXPENSE_DEDUCTIONS(0:12),
     +       R_BTL_MISC_DEDUCTIONS(0:12),
     +       R_NON_BOOK_DEDUCTIBLE_ITEMS(0:12),
     +       R_NON_BOOK_TAXABLE_ITEMS(0:12),
     +       R_NON_INCOME_TAX_DEDUCTIONS(0:12), 
     +       R_INTEREST_DEDUCTIONS(0:12), 
     +       R_TAXABLE_INCOME_B4_DEDUCTS(0:12),
     +       R_STATE_TAXABLE_INCOME(0:12),
     +       R_FEDERAL_TAXABLE_INCOME(0:12),
     +       R_STATE_TAXABLE_INCOME_B4_NOLS(0:12),
     +       R_FED_TAXABLE_INCOME_B4_NOLS(0:12),
     +       R_STATE_INCOME_TAX(0:12),
     +       R_FEDERAL_INCOME_TAXES(0:12),
     +       R_STATE_BTL_TAXABLE_INCOME(0:12),
     +       R_STATE_BTL_INCOME_TAX(0:12),
     +       R_FEDERAL_BTL_TAXABLE_INCOME(0:12),
     +       R_FEDERAL_BTL_INCOME_TAX(0:12),
     +       R_STATE_INCOME_TAX_B4_CREDITS(0:12),
     +       R_FED_INCOME_TAXES_B4_CREDITS(0:12),
     +       R_STATE_NOLS_USED(0:12),
     +       R_FEDERAL_NOLS_USED(0:12),
     +       R_STATE_NOLS_GENERATED(0:12),
     +       R_FEDERAL_NOLS_GENERATED(0:12),
     +       R_STATE_GENERAL_CREDITS(0:12),
     +       R_FEDERAL_SEC29_CREDITS(0:12),
     +       R_FEDERAL_GENERAL_CREDITS(0:12),
     +       R_FEDERAL_AMT_CREDITS_USED(0:12),
     +       R_FED_AMT_CREDITS_GENERATED(0:12)
      REAL*4 R_BTL_TAXABLE_INCOME(0:12),
     +       R_BTL_OTHER_TAXABLE_INCOME(0:12),
     +       R_BTL_TAXABLE_INVESTMENT(0:12)
      SAVE R_BTL_TAXABLE_INCOME
      SAVE R_ATL_TAXABLE_REVENUES,
     +     R_ATL_TAXABLE_BOOK_DEDUCTIONS,
     +     R_BTL_TAXABLE_REVENUES,
     +     R_BTL_EXPENSE_DEDUCTIONS,
     +     R_BTL_TAX_EXPENSE_DEDUCTIONS,
     +     R_BTL_MISC_DEDUCTIONS,
     +     R_NON_BOOK_DEDUCTIBLE_ITEMS,
     +     R_NON_BOOK_TAXABLE_ITEMS,
     +     R_NON_INCOME_TAX_DEDUCTIONS, 
     +     R_INTEREST_DEDUCTIONS, 
     +     R_TAXABLE_INCOME_B4_DEDUCTS,
     +     R_STATE_TAXABLE_INCOME,
     +     R_FEDERAL_TAXABLE_INCOME,
     +     R_STATE_TAXABLE_INCOME_B4_NOLS,
     +     R_FED_TAXABLE_INCOME_B4_NOLS,
     +     R_STATE_INCOME_TAX,
     +     R_FEDERAL_INCOME_TAXES,
     +     R_STATE_BTL_TAXABLE_INCOME,
     +     R_STATE_BTL_INCOME_TAX,
     +     R_FEDERAL_BTL_TAXABLE_INCOME,
     +     R_FEDERAL_BTL_INCOME_TAX,
     +     R_STATE_INCOME_TAX_B4_CREDITS,
     +     R_FED_INCOME_TAXES_B4_CREDITS,
     +     R_STATE_NOLS_USED,
     +     R_FEDERAL_NOLS_USED,
     +     R_STATE_NOLS_GENERATED,
     +     R_FEDERAL_NOLS_GENERATED,
     +     R_STATE_GENERAL_CREDITS,
     +     R_FEDERAL_SEC29_CREDITS,
     +     R_FEDERAL_GENERAL_CREDITS,
     +     R_FEDERAL_AMT_CREDITS_USED,
     +     R_FED_AMT_CREDITS_GENERATED,
     +     R_BTL_OTHER_TAXABLE_INCOME,
     +     R_BTL_TAXABLE_INVESTMENT
C
      REAL*4 RPT_STATE_BTL_TAXABLE_INCOME,
     +       RPT_STATE_BTL_INCOME_TAX,
     +       RPT_FEDERAL_BTL_TAXABLE_INCOME,
     +       RPT_FEDERAL_BTL_INCOME_TAX,
     +       MONTHLY_ATL_INCOME_STATEMENT_TAX_DEDUCTIONS
      INTEGER*2 FED_TAX_CODE,STATE_TAX_CODE
      PARAMETER (FED_TAX_CODE=2,STATE_TAX_CODE=3)
      INTEGER*2 R_YR,R_CLASS
      CHARACTER*1 R_CLASS_TYPE
      CHARACTER*1 SBU,SUBSIDIARY,PARENT,REGULATED_GROUP,
     +            CONSOLIDATED,ELIMINATION
      PARAMETER (SBU='B',SUBSIDIARY='S',PARENT='P',REGULATED_GROUP='R',
     +           CONSOLIDATED='C',ELIMINATION='E')
      REAL*4 R_ATL_TAXABLE_INCOME(0:12)
      REAL*4 R_STATE_M1_ADDITIONS(0:12),
     +       R_STATE_M1_DEDUCTIONS(0:12),
     +       R_FEDERAL_M1_ADDITIONS(0:12),
     +       R_FEDERAL_M1_DEDUCTIONS(0:12),
     +       R_STATE_BTL_MISC_DEDUCTIONS(0:12),
     +       R_FEDERAL_BTL_MISC_DEDUCTIONS(0:12),
     +       R_STATE_TAX_RATE(0:12),
     +       R_FEDERAL_TAX_RATE(0:12),
     +       R_STATE_DEF_TAX_DR_NOLS(0:12),
     +       R_FED_DEF_TAX_DR_NOLS_AMT(0:12),
     +       R_STATE_INCOME_TAX_ADJUSTMENT(0:12),
     +       R_FEDERAL_INCOME_TAX_ADJUSTMENT(0:12)

      REAL*4 STATE_NOLS_USED_BY_SUBS(0:12),
     +       STATE_NOLS_GEN_BY_SUBS(0:12),   
     +       SUB_STATE_TAXES_PAID(0:12),
     +       PARENT_STATE_TAXES(0:12),
     +       PARENT_STATE_DR_TAX_FROM_NOLS(0:12),
     +       SUB_STATE_DR_TAX_FROM_NOLS(0:12)
      REAL*4 ANNUAL_VARS(0:*)
      REAL*4 TAX_CASH_RETIREMENT_PAYMENTS,
     +       TAX_CASH_STORM_PAYMENTS,
     +       TAX_CASH_VACATION_PAYMENTS,
     +       TAX_BOOK_RETIREMENT_PAYMENTS,
     +       TAX_BOOK_STORM_PAYMENTS,
     +       TAX_BOOK_VACATION_PAYMENTS
      REAL*4 AVAILABLE_STATE_NOLS,
     +       AVAILABLE_FEDERAL_NOLS,
     +       AVAILABLE_STATE_GENERAL_CREDITS,
     +       AVAILABLE_SEC29_CREDITS,
     +       AVAILABLE_FED_GENERAL_CREDITS,
     +       AVAILABLE_AMT_CREDITS_USED,
     +       AMT_CREDITS_GENERATED
      REAL*4 STATE_INCOME_AFTER_NOLS_CHECK,
     +       ADDITIONAL_NOLS_NEEDED,
     +       FED_INCOME_AFTER_NOLS_CHECK
      LOGICAL*1 GET_FED_TAX_BENEFITS_NOW,
     +          GET_STATE_TAX_BENEFITS_NOW
      REAL*4 RPT_STATE_TAX_INCOME_B4_NOLS
      REAL*4 RPT_STATE_NOLS_USED
      REAL*4 RPT_STATE_NOLS_GENERATED
      REAL*4 RPT_STATE_TAXABLE_INCOME
      REAL*4 RPT_STATE_INCOME_TAX_B4_CREDITS
      REAL*4 RPT_FED_INCOME_TAX_B4_CREDITS
      REAL*4 RPT_STATE_INCOME_TAX
      REAL*4 RPT_FED_TAX_INCOME_B4_NOLS
      REAL*4 RPT_FEDERAL_NOLS_USED
      REAL*4 RPT_FEDERAL_NOLS_GENERATED
      REAL*4 RPT_FEDERAL_TAXABLE_INCOME
      REAL*4 RPT_FEDERAL_INCOME_TAX
      REAL*4 RPT_DEDUCTIBLE_BOOK_EXPENSES
      REAL*4 RPT_TOTAL_DEDUCT_BOOK_EXPENSES
      REAL*4 RPT_TOTAL_TAXABLE_INCOME
      REAL*4 RPT_TAXABLE_INCOME_B4_DEDUCTS
      REAL*4 RPT_STATE_ATL_INCOME_TAX
      REAL*4 RPT_STATE_GENERAL_CREDITS
      REAL*4 RPT_FEDERAL_SEC29_CREDITS
      REAL*4 RPT_FEDERAL_GENERAL_CREDITS
      REAL*4 RPT_FEDERAL_AMT_CREDITS_USED
      REAL*4 RPT_FED_AMT_CREDITS_GENERATED
      REAL*4 RPT_FEDERAL_ATL_INCOME_TAX
      REAL*4 RPT_BTL_TAXABLE_INCOME,RPT_NON_BOOK_TAXABLE_ITEMS
      REAL*4 RPT_BTL_OTHER_TAXABLE_INCOME
      REAL*4 RPT_BTL_TAXABLE_INVESTMENT
      REAL*4 RPT_BTL_TAXABLE_EXPENSES
      REAL*4 RPT_BTL_M1_TAX_DEDUCTIONS
      REAL*4 SUM_NONREGULATED_REVENUES,
     +       SUM_REGULATED_REVENUES
      REAL*4 INCOME_BEFORE_INTEREST,
     +       TOTAL_TAXES_EXPENSE,
     +       MONTHLY_INCOME_AFTER_INTEREST,
     +       MONTHLY_NET_INCOME,
     +       MONTHLY_TOTAL_EXPENSES_B4_TAXES
C
         SUM_MONTHLY_INCOME_STATEMENT = .TRUE.
      RETURN
C***********************************************************************
      ENTRY SUM_MONTHLY_INCOME_REVENUES(INCOME_VARIABLES)
C***********************************************************************
C
         SUM_MONTHLY_INCOME_REVENUES = .TRUE.
         DO MO = 0, 12
            INCOME_VARIABLES(MO,Total Base Revenues) =
     +                       SUM_REGULATED_REVENUES(MO,INCOME_VARIABLES)
            INCOME_VARIABLES(MO,TOTAL OPERATING REVENUES) =
     +              INCOME_VARIABLES(MO,Total Base Revenues)
     +              + SUM_NONREGULATED_REVENUES(MO,INCOME_VARIABLES)
         ENDDO
      RETURN
C***********************************************************************
      ENTRY MONTHLY_TAXABLE_INCOME(R_YR,R_CLASS,R_CLASS_TYPE,
     +                             R_ATL_TAXABLE_INCOME,
C     +                             R_BTL_TAXABLE_INCOME,
     +                             INCOME_VARIABLES,
     +                             CASH_VARIABLES,
     +                             TAX_VARIABLES,
     +                             ANNUAL_VARS,
     +                             R_STATE_M1_ADDITIONS,
     +                             R_STATE_M1_DEDUCTIONS,
     +                             R_FEDERAL_M1_ADDITIONS,
     +                             R_FEDERAL_M1_DEDUCTIONS,
     +                             R_STATE_BTL_MISC_DEDUCTIONS,
     +                             R_FEDERAL_BTL_MISC_DEDUCTIONS,
     +                             R_STATE_TAX_RATE,
     +                             R_FEDERAL_TAX_RATE,
     +                             R_STATE_DEF_TAX_DR_NOLS,
     +                             R_FED_DEF_TAX_DR_NOLS_AMT,
     +                             R_STATE_INCOME_TAX_ADJUSTMENT,
     +                             R_FEDERAL_INCOME_TAX_ADJUSTMENT,
     +                             STATE_NOLS_USED_BY_SUBS,
     +                             STATE_NOLS_GEN_BY_SUBS,
     +                             PARENT_STATE_TAXES,
     +                             PARENT_STATE_DR_TAX_FROM_NOLS,
     +                             SUB_STATE_DR_TAX_FROM_NOLS,
     +                             SUB_STATE_TAXES_PAID,
     +                             SUB_BTL_TAXABLE_EXPENSES,
     +                             SUB_BTL_STATE_TAXES_PAID,
     +                             SUB_BTL_STATE_TAXABLE_INCOME,
     +                             SUB_BTL_TAXABLE_OTHER_INCOME,
     +                             SUB_BTL_TAXABLE_INVEST_INCOME,
     +                             SUB_BTL_MISC_DEDUCTIONS,
     +                             SUB_BTL_FEDERAL_TAXES_PAID)
C***********************************************************************
C
C
         GET_FED_TAX_BENEFITS_NOW = .NOT. R_CLASS_TYPE == PARENT .AND.
     +                               USE_ALL_FED_TAX_BENEFITS
         GET_STATE_TAX_BENEFITS_NOW = .NOT. R_CLASS_TYPE == PARENT .AND.
     +                                USE_STATE_TAX_BENEFITS_NOW
         MONTHLY_TAXABLE_INCOME = .TRUE.
         R_ATL_TAXABLE_INCOME(0) = 0.
         R_ATL_TAXABLE_REVENUES(0) = 0.
         R_NON_BOOK_DEDUCTIBLE_ITEMS(0) = 0.
         R_NON_BOOK_TAXABLE_ITEMS(0) = 0.
         R_NON_INCOME_TAX_DEDUCTIONS(0) = 0. 
         R_INTEREST_DEDUCTIONS(0) = 0. 
         R_ATL_TAXABLE_BOOK_DEDUCTIONS(0) = 0.
         R_BTL_TAXABLE_INCOME(0) = 0.
         R_BTL_TAXABLE_REVENUES(0) = 0.
         R_BTL_EXPENSE_DEDUCTIONS(0) = 0.
         R_BTL_TAX_EXPENSE_DEDUCTIONS(0) = 0.
         R_BTL_MISC_DEDUCTIONS(0) = 0.
         R_TAXABLE_INCOME_B4_DEDUCTS(0) = 0.
         R_STATE_TAXABLE_INCOME(0) = 0.
         R_FEDERAL_TAXABLE_INCOME(0) = 0.
         R_STATE_TAXABLE_INCOME_B4_NOLS(0) = 0.
         R_FED_TAXABLE_INCOME_B4_NOLS(0) = 0.
         R_STATE_INCOME_TAX(0) = 0.
         R_FEDERAL_INCOME_TAXES(0) = 0.
         R_STATE_BTL_TAXABLE_INCOME(0) = 0.
         R_STATE_BTL_INCOME_TAX(0) = 0.
         R_FEDERAL_BTL_TAXABLE_INCOME(0) = 0.
         R_FEDERAL_BTL_INCOME_TAX(0) = 0.
         R_STATE_INCOME_TAX_B4_CREDITS(0) = 0.
         R_FED_INCOME_TAXES_B4_CREDITS(0) = 0.
         R_STATE_DEF_TAX_DR_NOLS(0) = 0.
         R_FED_DEF_TAX_DR_NOLS_AMT(0) = 0.
         R_STATE_NOLS_USED(0) = 0. ! ANNUAL_VARS(309)
         AVAILABLE_STATE_NOLS = ANNUAL_VARS(309)
         R_FEDERAL_NOLS_USED(0) = 0. ! ANNUAL_VARS(311)
         AVAILABLE_FEDERAL_NOLS = ANNUAL_VARS(311)
         R_STATE_GENERAL_CREDITS(0) = ANNUAL_VARS(346)
         AVAILABLE_STATE_GENERAL_CREDITS = ANNUAL_VARS(346)
         R_FEDERAL_SEC29_CREDITS(0) = ANNUAL_VARS(373)
         AVAILABLE_SEC29_CREDITS = ANNUAL_VARS(373)
         R_FEDERAL_GENERAL_CREDITS(0) = ANNUAL_VARS(341)
         AVAILABLE_FED_GENERAL_CREDITS = ANNUAL_VARS(341)
         R_STATE_NOLS_GENERATED(0) = 0.
         R_FEDERAL_NOLS_GENERATED(0) = 0.
         STATE_INCOME_AFTER_NOLS_CHECK = ANNUAL_VARS(132)
         FED_INCOME_AFTER_NOLS_CHECK = ANNUAL_VARS(136)
C
         DO MO = 1, 12
            IF(UI()) THEN
               TAX_CASH_RETIREMENT_PAYMENTS =
     +              CASH_VARIABLES(MO,csh_unfundd_retrmt_pymts)
               TAX_CASH_STORM_PAYMENTS =
     +                            CASH_VARIABLES(MO,Cash_Storm_Payments)
               TAX_CASH_VACATION_PAYMENTS =
     +                         CASH_VARIABLES(MO,Cash_Vacation_Payments)
               TAX_BOOK_RETIREMENT_PAYMENTS = 0.
               TAX_BOOK_STORM_PAYMENTS = 0.
               TAX_BOOK_VACATION_PAYMENTS = 0.
            ELSE
               TAX_BOOK_RETIREMENT_PAYMENTS =
     +                      INCOME_VARIABLES(MO,MonthlyPensionExpense)
               TAX_BOOK_STORM_PAYMENTS =
     +                        INCOME_VARIABLES(MO,MonthlySTORMExpense)
               TAX_BOOK_VACATION_PAYMENTS =
     +                         INCOME_VARIABLES(MO,monthly_vacation_pay)
               TAX_CASH_RETIREMENT_PAYMENTS = 0.
               TAX_CASH_STORM_PAYMENTS = 0.
               TAX_CASH_VACATION_PAYMENTS = 0.
            ENDIF
            R_ATL_TAXABLE_REVENUES(MO) =
     +                  SUM_REGULATED_REVENUES(MO,INCOME_VARIABLES)
     +                  + SUM_NONREGULATED_REVENUES(MO,INCOME_VARIABLES)
C
            R_ATL_TAXABLE_REVENUES(0) = R_ATL_TAXABLE_REVENUES(0)
     +                                  + R_ATL_TAXABLE_REVENUES(MO)
C 
            R_ATL_TAXABLE_BOOK_DEDUCTIONS(MO) =
     +          MONTHLY_ATL_INCOME_STATEMENT_TAX_DEDUCTIONS(MO,
     +                                                 INCOME_VARIABLES)
     +          + INCOME_VARIABLES(MO,monthly_nuclr_fuel_tax_expns)
     +          + TAX_BOOK_RETIREMENT_PAYMENTS
     +          + TAX_BOOK_STORM_PAYMENTS
     +          + TAX_BOOK_VACATION_PAYMENTS
C
            R_ATL_TAXABLE_BOOK_DEDUCTIONS(0) =
     +                               R_ATL_TAXABLE_BOOK_DEDUCTIONS(0)
     +                               + R_ATL_TAXABLE_BOOK_DEDUCTIONS(MO)
C
C BTL TAXES
C
             R_BTL_TAXABLE_REVENUES(MO) =
     +             INCOME_VARIABLES(MO,BTL_monthly_other_income)
     +             + INCOME_VARIABLES(MO,Monthly_Investment_Earnings)
     +             - .7*INCOME_VARIABLES(MO,
     +                                     Monthly_Dividend_70_Earnings)
     +             - INCOME_VARIABLES(MO,
     +                         mthy_rtrmt_med_fd_rngs) ! 9/28/04
     +             + INCOME_VARIABLES(MO,
     +                                mty_totl_lti_income)
     +             + INCOME_VARIABLES(MO,BTL_Monthly_STInvestmet_Income)
     +             + INCOME_VARIABLES(MO,
     +                                  monthly_notes_receivable_income)
     +             + CASH_VARIABLES(MO,CashEarningsNonCorporate)

            R_BTL_EXPENSE_DEDUCTIONS(MO) =
     +                         INCOME_VARIABLES(MO,BTL_Monthly_Expenses)
C BTL ITEMS ARE NOT OFFSET
            R_INTEREST_DEDUCTIONS(MO) = 
     +          INCOME_VARIABLES(MO,MonthlySTDInterest)
     +          + INCOME_VARIABLES(MO,mty_intst_on_nts_pyble)
     +          + TAX_VARIABLES(MO,monthly_ltd_tax_deduction)
            R_INTEREST_DEDUCTIONS(0) = R_INTEREST_DEDUCTIONS(0)
     +                                 + R_INTEREST_DEDUCTIONS(MO)
C
            R_NON_INCOME_TAX_DEDUCTIONS(MO) = 
     +              INCOME_VARIABLES(MO,monthly_oth_taxes)
     +              + INCOME_VARIABLES(MO,Monthly_Property_Taxes)
     +              + INCOME_VARIABLES(MO,Monthly_Operating_Revenue_Tax)
     +              + INCOME_VARIABLES(MO,Monthly_State_Tax_on_Capital)
     +              + INCOME_VARIABLES(MO,Monthly_State_Tax_on_Capital)
     +              + INCOME_VARIABLES(MO,MonthlyPayrollTaxes)
      
            R_NON_INCOME_TAX_DEDUCTIONS(0) = 
     +                                 R_NON_INCOME_TAX_DEDUCTIONS(0)
     +                                 + R_NON_INCOME_TAX_DEDUCTIONS(MO)
C
            R_NON_BOOK_TAXABLE_ITEMS(MO) =
     +                    ANNUAL_VARS(129)/12. 
     +                    - CASH_VARIABLES(MO,cash_net_salvage)
     +                    - TAX_CASH_RETIREMENT_PAYMENTS
     +                    - TAX_CASH_STORM_PAYMENTS
     +                    - TAX_CASH_VACATION_PAYMENTS
     +                    - CASH_VARIABLES(MO,cash_ltd_ps_issue_expense)
     +                    - CASH_VARIABLES(MO,cash_common_issue_expense)
     +                    - CASH_VARIABLES(MO,CashExecBenefits)
     +                    - CASH_VARIABLES(MO,CashIncentiveCompReserve)
            R_NON_BOOK_TAXABLE_ITEMS(0) = R_NON_BOOK_TAXABLE_ITEMS(0)
     +                                    + R_NON_BOOK_TAXABLE_ITEMS(MO)
C TAKE DEDUCTIONS
            R_NON_BOOK_DEDUCTIBLE_ITEMS(MO) =
     +                                       INCOME_TAX_DEPRECIATION/12.
     +                                       + ANNUAL_VARS(228)/12. 
            R_NON_BOOK_DEDUCTIBLE_ITEMS(0) =
     +                                 R_NON_BOOK_DEDUCTIBLE_ITEMS(0)
     +                                 + R_NON_BOOK_DEDUCTIBLE_ITEMS(MO)
               R_BTL_TAXABLE_INVESTMENT(MO) =
     +                  INCOME_VARIABLES(MO,Monthly_Investment_Earnings)
     +                  - .7*INCOME_VARIABLES(MO,
     +                                     Monthly_Dividend_70_Earnings)
     +                  - INCOME_VARIABLES(MO,
     +                         mthy_rtrmt_med_fd_rngs) ! 9/28/04
     +                  + INCOME_VARIABLES(MO,
     +                                mty_totl_lti_income)
     +                  + INCOME_VARIABLES(MO,
     +                                   BTL_Monthly_STInvestmet_Income)
     +                  + INCOME_VARIABLES(MO,
     +                                  monthly_notes_receivable_income)
C
            R_ATL_TAXABLE_INCOME(MO) = R_ATL_TAXABLE_REVENUES(MO)
     +                               + R_NON_BOOK_TAXABLE_ITEMS(MO)

     +                - R_ATL_TAXABLE_BOOK_DEDUCTIONS(MO)
     +                - R_NON_BOOK_DEDUCTIBLE_ITEMS(MO)
     +                - R_NON_INCOME_TAX_DEDUCTIONS(MO)
     +                - R_INTEREST_DEDUCTIONS(MO)
C
            R_ATL_TAXABLE_INCOME(0) = R_ATL_TAXABLE_INCOME(0)
     +                                + R_ATL_TAXABLE_INCOME(MO)
            R_TAXABLE_INCOME_B4_DEDUCTS(MO) = R_ATL_TAXABLE_INCOME(MO)
     +                                    + R_BTL_TAXABLE_REVENUES(MO)
     +                                    - R_BTL_EXPENSE_DEDUCTIONS(MO)
            R_TAXABLE_INCOME_B4_DEDUCTS(0) = 
     +                                 R_TAXABLE_INCOME_B4_DEDUCTS(0)
     +                                 + R_TAXABLE_INCOME_B4_DEDUCTS(MO)
            R_STATE_TAXABLE_INCOME_B4_NOLS(MO) =
     +                                   R_TAXABLE_INCOME_B4_DEDUCTS(MO)
     +                                   + R_STATE_M1_ADDITIONS(MO)
     +                                   - R_STATE_M1_DEDUCTIONS(MO)
            R_STATE_TAXABLE_INCOME_B4_NOLS(0) =
     +                              R_STATE_TAXABLE_INCOME_B4_NOLS(0)
     +                              + R_STATE_TAXABLE_INCOME_B4_NOLS(MO)
            IF(R_CLASS_TYPE == CONSOLIDATED) THEN
               R_STATE_NOLS_USED(MO) = STATE_NOLS_USED_BY_SUBS(MO)
               R_STATE_NOLS_GENERATED(MO) = STATE_NOLS_GEN_BY_SUBS(MO)
               R_STATE_TAXABLE_INCOME(MO) =
     +                                R_STATE_TAXABLE_INCOME_B4_NOLS(MO)
     +                                + STATE_NOLS_GEN_BY_SUBS(MO)
     +                                - STATE_NOLS_USED_BY_SUBS(MO)
               R_STATE_INCOME_TAX_B4_CREDITS(MO)=PARENT_STATE_TAXES(MO)
     +                                        + SUB_STATE_TAXES_PAID(MO)
               IF(R_STATE_TAXABLE_INCOME(MO) > 0.) THEN
                  R_STATE_TAX_RATE(MO) =
     +                               R_STATE_INCOME_TAX_B4_CREDITS(MO)/
     +                                  R_STATE_TAXABLE_INCOME(MO)
               ENDIF
C
C MUST ADD THE CREDITS ADJUSTMENTS 
C
               R_STATE_INCOME_TAX(MO) = 
     +                                 R_STATE_INCOME_TAX_B4_CREDITS(MO)
               R_STATE_DEF_TAX_DR_NOLS(MO) =
     +                                 PARENT_STATE_DR_TAX_FROM_NOLS(MO)
     +                                 + SUB_STATE_DR_TAX_FROM_NOLS(MO)
               R_STATE_INCOME_TAX(MO) = R_STATE_INCOME_TAX(MO)
     +                               + R_STATE_INCOME_TAX_ADJUSTMENT(MO)

               R_BTL_TAXABLE_REVENUES(MO) =
     +                               SUB_BTL_TAXABLE_OTHER_INCOME(MO)
     +                               + SUB_BTL_TAXABLE_INVEST_INCOME(MO)
               R_BTL_TAX_EXPENSE_DEDUCTIONS(MO) =
     +                                      SUB_BTL_TAXABLE_EXPENSES(MO)
               R_BTL_TAXABLE_INCOME(MO) = R_BTL_TAXABLE_REVENUES(MO)
     +                                - R_BTL_TAX_EXPENSE_DEDUCTIONS(MO)
               R_STATE_BTL_TAXABLE_INCOME(MO) = R_BTL_TAXABLE_INCOME(MO) 
     +                                 - R_STATE_BTL_MISC_DEDUCTIONS(MO)
               R_STATE_BTL_INCOME_TAX(MO) = SUB_BTL_STATE_TAXES_PAID(MO)
     
     
               R_STATE_DEF_TAX_DR_NOLS(0) = R_STATE_DEF_TAX_DR_NOLS(0)
     +                                   + R_STATE_DEF_TAX_DR_NOLS(MO)
               R_STATE_NOLS_USED(0) = R_STATE_NOLS_USED(0)
     +                             + R_STATE_NOLS_USED(MO)
               R_STATE_NOLS_GENERATED(0) = R_STATE_NOLS_GENERATED(0)
     +                                  + R_STATE_NOLS_GENERATED(MO)
               R_STATE_TAXABLE_INCOME(0) = R_STATE_TAXABLE_INCOME(0)
     +                                  + R_STATE_TAXABLE_INCOME(MO)
               R_STATE_INCOME_TAX_B4_CREDITS(0) =
     +                               R_STATE_INCOME_TAX_B4_CREDITS(0)
     +                               + R_STATE_INCOME_TAX_B4_CREDITS(MO)
               R_STATE_INCOME_TAX(0) = R_STATE_INCOME_TAX(0)
     +                                 + R_STATE_INCOME_TAX(MO)
               R_BTL_TAX_EXPENSE_DEDUCTIONS(0) = 
     +                                R_BTL_TAX_EXPENSE_DEDUCTIONS(0) 
     +                                + R_BTL_TAX_EXPENSE_DEDUCTIONS(MO)
               R_STATE_BTL_TAXABLE_INCOME(0) =
     +                                  R_STATE_BTL_TAXABLE_INCOME(0)
     +                                  + R_STATE_BTL_TAXABLE_INCOME(MO)
               R_STATE_BTL_INCOME_TAX(0) = R_STATE_BTL_INCOME_TAX(0)
     +                                     + R_STATE_BTL_INCOME_TAX(MO)
            ELSE
C STATE TAXES FOR ALL CLASS EXCEPT CONSOLIDATED
               IF(GET_STATE_TAX_BENEFITS_NOW) THEN
                  R_STATE_TAXABLE_INCOME(MO) =
     +                                R_STATE_TAXABLE_INCOME_B4_NOLS(MO)
     +                                - AVAILABLE_STATE_NOLS
                  R_STATE_NOLS_USED(MO) = AVAILABLE_STATE_NOLS
                  AVAILABLE_STATE_NOLS = 0.
                  R_STATE_NOLS_GENERATED(MO) = 0.
               ELSEIF(R_STATE_TAXABLE_INCOME_B4_NOLS(MO) <= 0.) THEN
                  IF(R_CLASS_TYPE == SBU .OR.
     +                             R_CLASS_TYPE == REGULATED_GROUP) THEN
                     R_STATE_TAXABLE_INCOME(MO) =
     +                                R_STATE_TAXABLE_INCOME_B4_NOLS(MO)
                     R_STATE_NOLS_GENERATED(MO) =  0.
                  ELSE
                     R_STATE_TAXABLE_INCOME(MO) = 0.
c    +                                R_STATE_TAXABLE_INCOME_B4_NOLS(MO)
                     R_STATE_NOLS_GENERATED(MO) =
     +                           ABS(R_STATE_TAXABLE_INCOME_B4_NOLS(MO))
                  ENDIF
                  R_STATE_NOLS_USED(MO) = 0.
                  AVAILABLE_STATE_NOLS = AVAILABLE_STATE_NOLS
     +                                + R_STATE_NOLS_GENERATED(MO)
               ELSE

                  R_STATE_NOLS_GENERATED(MO) = 0.
                  IF(R_STATE_TAXABLE_INCOME_B4_NOLS(MO) < 0.) THEN
                     R_STATE_NOLS_GENERATED(MO) =
     +                           ABS(R_STATE_TAXABLE_INCOME_B4_NOLS(MO))
                     R_STATE_TAXABLE_INCOME(MO) =
     +                                R_STATE_TAXABLE_INCOME_B4_NOLS(MO)
                     R_STATE_NOLS_USED(MO) = 0.
                  ELSEIF(R_STATE_TAXABLE_INCOME_B4_NOLS(MO) >=
     +                                        AVAILABLE_STATE_NOLS) THEN
                     R_STATE_TAXABLE_INCOME(MO) =
     +                                R_STATE_TAXABLE_INCOME_B4_NOLS(MO)
     +                                - AVAILABLE_STATE_NOLS
                     R_STATE_NOLS_USED(MO) = AVAILABLE_STATE_NOLS
                     AVAILABLE_STATE_NOLS = 0.
                  ELSE
                     R_STATE_TAXABLE_INCOME(MO) = 0.
                     AVAILABLE_STATE_NOLS = AVAILABLE_STATE_NOLS
     +                              - R_STATE_TAXABLE_INCOME_B4_NOLS(MO)
                     R_STATE_NOLS_USED(MO) =
     +                                R_STATE_TAXABLE_INCOME_B4_NOLS(MO)
                  ENDIF
               ENDIF

C
C BALANCE NOLS GENERTED DURING THE YEAR WHICH WOULD OFFSET TAXABLE INCOME
C THAT OCCURRED BEFORE THE NOL WAS GENERATED.
C
               IF(MO == 12 .AND. (R_STATE_TAXABLE_INCOME(0)
     +                         + R_STATE_TAXABLE_INCOME(MO)) >
     +                               STATE_INCOME_AFTER_NOLS_CHECK) THEN
                  ADDITIONAL_NOLS_NEEDED = R_STATE_TAXABLE_INCOME(0)
     +                                  + R_STATE_TAXABLE_INCOME(MO)
     +                                  - STATE_INCOME_AFTER_NOLS_CHECK
                  R_STATE_NOLS_USED(MO) = R_STATE_NOLS_USED(MO)
     +                                 + ADDITIONAL_NOLS_NEEDED
                  R_STATE_TAXABLE_INCOME(MO)=R_STATE_TAXABLE_INCOME(MO)
     +                                       - ADDITIONAL_NOLS_NEEDED
               ENDIF
C
C CALCUALTE THE STATE TAX
C
               R_STATE_INCOME_TAX_B4_CREDITS(MO) = R_STATE_TAX_RATE(MO)*
     +                                        R_STATE_TAXABLE_INCOME(MO)
C
C THEN CREDITS
C
               R_STATE_GENERAL_CREDITS(MO) = 0.
               IF(GET_STATE_TAX_BENEFITS_NOW) THEN
                  R_STATE_INCOME_TAX(MO) = 
     +                                 R_STATE_INCOME_TAX_B4_CREDITS(MO)
     +                                 - AVAILABLE_STATE_GENERAL_CREDITS
                  R_STATE_GENERAL_CREDITS(MO) =
     +                                   AVAILABLE_STATE_GENERAL_CREDITS
                  AVAILABLE_STATE_GENERAL_CREDITS = 0.
               ELSEIF(R_STATE_INCOME_TAX_B4_CREDITS(MO) > 0.) THEN
                  IF(AVAILABLE_STATE_GENERAL_CREDITS >=
     +                           R_STATE_INCOME_TAX_B4_CREDITS(MO)) THEN
                     R_STATE_INCOME_TAX(MO) = 0.
                     R_STATE_GENERAL_CREDITS(MO) =
     +                                 R_STATE_INCOME_TAX_B4_CREDITS(MO)
                     AVAILABLE_STATE_GENERAL_CREDITS =
     +                               AVAILABLE_STATE_GENERAL_CREDITS
     +                               - R_STATE_INCOME_TAX_B4_CREDITS(MO)
                  ELSE
                     R_STATE_INCOME_TAX(MO) = 
     +                                 R_STATE_INCOME_TAX_B4_CREDITS(MO)
     +                                 - AVAILABLE_STATE_GENERAL_CREDITS
                     R_STATE_GENERAL_CREDITS(MO) =
     +                                   AVAILABLE_STATE_GENERAL_CREDITS
                     AVAILABLE_STATE_GENERAL_CREDITS = 0.
                  ENDIF
               ELSE
                  R_STATE_INCOME_TAX(MO) =
     +                                 R_STATE_INCOME_TAX_B4_CREDITS(MO)
               ENDIF
               R_STATE_INCOME_TAX(MO) = R_STATE_INCOME_TAX(MO)
     +                               + R_STATE_INCOME_TAX_ADJUSTMENT(MO)
               R_STATE_TAXABLE_INCOME(0) = R_STATE_TAXABLE_INCOME(0)
     +                                  + R_STATE_TAXABLE_INCOME(MO)
               R_STATE_INCOME_TAX_B4_CREDITS(0) =
     +                               R_STATE_INCOME_TAX_B4_CREDITS(0)
     +                               + R_STATE_INCOME_TAX_B4_CREDITS(MO)
               R_STATE_INCOME_TAX(0) = R_STATE_INCOME_TAX(0)
     +                              + R_STATE_INCOME_TAX(MO)
               R_BTL_TAX_EXPENSE_DEDUCTIONS(MO) = 
     +                                      R_BTL_EXPENSE_DEDUCTIONS(MO)
               R_BTL_TAX_EXPENSE_DEDUCTIONS(0) = 
     +                                R_BTL_TAX_EXPENSE_DEDUCTIONS(0) 
     +                                + R_BTL_TAX_EXPENSE_DEDUCTIONS(MO)
               R_BTL_OTHER_TAXABLE_INCOME(MO) =
     +                     INCOME_VARIABLES(MO,BTL_monthly_other_income)
               R_BTL_TAXABLE_INVESTMENT(MO) =
     +                  INCOME_VARIABLES(MO,Monthly_Investment_Earnings)
     +                  - .7*INCOME_VARIABLES(MO,
     +                                     Monthly_Dividend_70_Earnings)
     +                  - INCOME_VARIABLES(MO,
     +                         mthy_rtrmt_med_fd_rngs) ! 9/28/04
     +                  + INCOME_VARIABLES(MO,
     +                                mty_totl_lti_income)
     +                  + INCOME_VARIABLES(MO,
     +                                   BTL_Monthly_STInvestmet_Income)
     +                  + INCOME_VARIABLES(MO,
     +                                  monthly_notes_receivable_income)
               IF(CALCULATE_BTL_INCOME_TAXES) THEN
                  R_BTL_MISC_DEDUCTIONS(MO) = BTL_MISC_DEDUCTIONS/12.
                  R_BTL_TAXABLE_REVENUES(MO) =
     +                                    R_BTL_OTHER_TAXABLE_INCOME(MO)
     +                                    + R_BTL_TAXABLE_INVESTMENT(MO)
                  R_BTL_TAXABLE_INCOME(MO) = R_BTL_TAXABLE_REVENUES(MO)
     +                                    - R_BTL_EXPENSE_DEDUCTIONS(MO)
     +                                    - R_BTL_MISC_DEDUCTIONS(MO)
                  R_BTL_MISC_DEDUCTIONS(0) = R_BTL_MISC_DEDUCTIONS(0)
     +                                       + R_BTL_MISC_DEDUCTIONS(MO)
                  R_BTL_TAXABLE_INCOME(0)= R_BTL_TAXABLE_INCOME(0)
     +                                     + R_BTL_TAXABLE_INCOME(MO)
C
                  R_STATE_BTL_TAXABLE_INCOME(MO) =
     +                                 R_BTL_TAXABLE_REVENUES(MO)
     +                                 - R_BTL_EXPENSE_DEDUCTIONS(MO)
     +                                 - R_STATE_BTL_MISC_DEDUCTIONS(MO)
                  R_STATE_BTL_TAXABLE_INCOME(0) =
     +                                  R_STATE_BTL_TAXABLE_INCOME(0)
     +                                  + R_STATE_BTL_TAXABLE_INCOME(MO)
                  R_STATE_BTL_INCOME_TAX(MO) = 
     +                                  R_STATE_BTL_TAXABLE_INCOME(MO) *
     +                                              R_STATE_TAX_RATE(MO)
                  R_STATE_BTL_INCOME_TAX(0) = R_STATE_BTL_INCOME_TAX(0)
     +                                      + R_STATE_BTL_INCOME_TAX(MO)
               ELSE
                  R_BTL_TAXABLE_INCOME(MO) = 0.
                  R_STATE_BTL_TAXABLE_INCOME(MO) = 0.
                  R_STATE_BTL_INCOME_TAX(MO) = 0.
                  R_STATE_BTL_MISC_DEDUCTIONS(MO) = 0.
                  R_BTL_MISC_DEDUCTIONS(MO) = 0.
               ENDIF
               R_STATE_DEF_TAX_DR_NOLS(MO) = R_STATE_TAX_RATE(MO) * 
     +                                    (R_STATE_NOLS_USED(MO)
     +                                     - R_STATE_NOLS_GENERATED(MO))
               R_STATE_DEF_TAX_DR_NOLS(0) = R_STATE_DEF_TAX_DR_NOLS(0)
     +                                   + R_STATE_DEF_TAX_DR_NOLS(MO)
               R_STATE_NOLS_USED(0) = R_STATE_NOLS_USED(0)
     +                             + R_STATE_NOLS_USED(MO)
               R_STATE_NOLS_GENERATED(0) = R_STATE_NOLS_GENERATED(0)
     +                                  + R_STATE_NOLS_GENERATED(MO)
            ENDIF ! CONSOLIDATED OR REGULAR CLASS
         ENDDO !END OF STATE INCOME TAXES
C
C CHECK FOR ACTUAL STATE INCOME TAXES
C
         CALL ACTUAL_MONTHLY_INCOME_TAX_ITEMS(R_YR,R_CLASS,
     +                                        R_STATE_INCOME_TAX,
     +                                        STATE_TAX_CODE)
C
C FEDERAL INCOME TAXES
C
         DO MO = 1, 12
            R_FEDERAL_SEC29_CREDITS(MO) = 0.
            R_FEDERAL_GENERAL_CREDITS(MO) = 0.
            R_FEDERAL_AMT_CREDITS_USED(MO) = 0.
            R_FED_AMT_CREDITS_GENERATED(MO) = 0.
C
            R_FED_TAXABLE_INCOME_B4_NOLS(MO) =
     +                                   R_TAXABLE_INCOME_B4_DEDUCTS(MO)
     +                                   + R_FEDERAL_M1_ADDITIONS(MO)
     +                                   - R_FEDERAL_M1_DEDUCTIONS(MO)
     +                                   - R_STATE_INCOME_TAX(MO)
            R_FED_TAXABLE_INCOME_B4_NOLS(0) =
     +                                R_FED_TAXABLE_INCOME_B4_NOLS(0)
     +                                + R_FED_TAXABLE_INCOME_B4_NOLS(MO)
            IF(GET_FED_TAX_BENEFITS_NOW) THEN
               R_FEDERAL_NOLS_GENERATED(MO) = 0.
               R_FEDERAL_TAXABLE_INCOME(MO) =
     +                                  R_FED_TAXABLE_INCOME_B4_NOLS(MO)
     +                                  - AVAILABLE_FEDERAL_NOLS
               R_FEDERAL_NOLS_USED(MO) = AVAILABLE_FEDERAL_NOLS
               AVAILABLE_FEDERAL_NOLS = 0.
            ELSEIF(R_FED_TAXABLE_INCOME_B4_NOLS(MO) <= 0.) THEN
               IF(R_CLASS_TYPE == SBU .OR.
     +                             R_CLASS_TYPE == REGULATED_GROUP) THEN
                  R_FEDERAL_TAXABLE_INCOME(MO) =
     +                                  R_FED_TAXABLE_INCOME_B4_NOLS(MO)
                  R_FEDERAL_NOLS_GENERATED(MO) = 0.
               ELSE
                  R_FEDERAL_TAXABLE_INCOME(MO) = 0.
C    +                                  R_FED_TAXABLE_INCOME_B4_NOLS(MO)
                  R_FEDERAL_NOLS_GENERATED(MO) =
     +                             ABS(R_FED_TAXABLE_INCOME_B4_NOLS(MO))
               ENDIF
               R_FEDERAL_NOLS_USED(MO) = 0.

               AVAILABLE_FEDERAL_NOLS = AVAILABLE_FEDERAL_NOLS
     +                                  + R_FEDERAL_NOLS_GENERATED(MO)
            ELSE
               R_FEDERAL_NOLS_GENERATED(MO) = 0.
               IF(R_FED_TAXABLE_INCOME_B4_NOLS(MO) >=
     +                                      AVAILABLE_FEDERAL_NOLS) THEN
                  R_FEDERAL_TAXABLE_INCOME(MO) =
     +                                  R_FED_TAXABLE_INCOME_B4_NOLS(MO)
     +                                  - AVAILABLE_FEDERAL_NOLS
                  R_FEDERAL_NOLS_USED(MO) = AVAILABLE_FEDERAL_NOLS
                  AVAILABLE_FEDERAL_NOLS = 0.
               ELSE
                  R_FEDERAL_TAXABLE_INCOME(MO) = 0.
                  AVAILABLE_FEDERAL_NOLS = AVAILABLE_FEDERAL_NOLS
     +                                - R_FED_TAXABLE_INCOME_B4_NOLS(MO)
                  R_FEDERAL_NOLS_USED(MO) =
     +                                  R_FED_TAXABLE_INCOME_B4_NOLS(MO)
               ENDIF
            ENDIF
C
C BALANCE NOLS GENERTED DURING THE YEAR WHICH WOULD OFFSET TAXABLE INCOME
C THAT OCCURRED BEFORE THE NOL WAS GENERATED.
C
            IF(MO == 12 .AND. (R_FEDERAL_TAXABLE_INCOME(0)
     +                         + R_FEDERAL_TAXABLE_INCOME(MO)) >
     +                                 FED_INCOME_AFTER_NOLS_CHECK) THEN
               ADDITIONAL_NOLS_NEEDED = R_FEDERAL_TAXABLE_INCOME(0)
     +                                  + R_FEDERAL_TAXABLE_INCOME(MO)
     +                                  - FED_INCOME_AFTER_NOLS_CHECK
               R_FEDERAL_NOLS_USED(MO) = R_FEDERAL_NOLS_USED(MO)
     +                                 + ADDITIONAL_NOLS_NEEDED
               R_FEDERAL_TAXABLE_INCOME(MO) =
     +                                      R_FEDERAL_TAXABLE_INCOME(MO)
     +                                      - ADDITIONAL_NOLS_NEEDED
            ENDIF
            R_FED_INCOME_TAXES_B4_CREDITS(MO)=R_FEDERAL_TAX_RATE(MO)*
     +                                      R_FEDERAL_TAXABLE_INCOME(MO)     

            R_FEDERAL_INCOME_TAXES(MO) =
     +                                 R_FED_INCOME_TAXES_B4_CREDITS(MO)

            IF(R_CLASS_TYPE == CONSOLIDATED .AND.
     +                AVAILABLE_SEC29_CREDITS > 0. .AND.
     +                             R_FEDERAL_INCOME_TAXES(MO) > 0.) THEN
               IF(AVAILABLE_SEC29_CREDITS >
     +                                  R_FEDERAL_INCOME_TAXES(MO)) THEN
                  R_FEDERAL_SEC29_CREDITS(MO) =
     +                                        R_FEDERAL_INCOME_TAXES(MO)
                  AVAILABLE_SEC29_CREDITS = AVAILABLE_SEC29_CREDITS
     +                                      - R_FEDERAL_INCOME_TAXES(MO)
                  R_FEDERAL_INCOME_TAXES(MO) = 0.
               ELSE
                  R_FEDERAL_SEC29_CREDITS(MO) = AVAILABLE_SEC29_CREDITS
                  R_FEDERAL_INCOME_TAXES(MO) =
     +                                 MAX(0.,R_FEDERAL_INCOME_TAXES(MO)
     +                                        - AVAILABLE_SEC29_CREDITS)
                  AVAILABLE_SEC29_CREDITS = 0.
               ENDIF
            ELSEIF(AVAILABLE_SEC29_CREDITS > 0.) THEN ! always use 29 credits
               R_FEDERAL_SEC29_CREDITS(MO) = AVAILABLE_SEC29_CREDITS
               R_FEDERAL_INCOME_TAXES(MO) = R_FEDERAL_INCOME_TAXES(MO)
     +                                      - AVAILABLE_SEC29_CREDITS
               AVAILABLE_SEC29_CREDITS = 0.
            ENDIF
            IF(AVAILABLE_FED_GENERAL_CREDITS > 0.) THEN
               IF(GET_FED_TAX_BENEFITS_NOW) THEN
                  R_FEDERAL_GENERAL_CREDITS(MO) =
     +                                     AVAILABLE_FED_GENERAL_CREDITS
                  R_FEDERAL_INCOME_TAXES(MO) =
     +                                   R_FEDERAL_INCOME_TAXES(MO)
     +                                   - AVAILABLE_FED_GENERAL_CREDITS
                  AVAILABLE_FED_GENERAL_CREDITS = 0.
               ELSEIF(R_FEDERAL_INCOME_TAXES(MO) > 0.) THEN
                  IF(AVAILABLE_FED_GENERAL_CREDITS >
     +                                  R_FEDERAL_INCOME_TAXES(MO)) THEN
                     R_FEDERAL_GENERAL_CREDITS(MO) =
     +                                        R_FEDERAL_INCOME_TAXES(MO)
                     AVAILABLE_FED_GENERAL_CREDITS =
     +                                     AVAILABLE_FED_GENERAL_CREDITS
     +                                     - R_FEDERAL_INCOME_TAXES(MO)
                     R_FEDERAL_INCOME_TAXES(MO) = 0.
                  ELSE
                     R_FEDERAL_GENERAL_CREDITS(MO) =
     +                                     AVAILABLE_FED_GENERAL_CREDITS
                     R_FEDERAL_INCOME_TAXES(MO) =
     +                           MAX(0.,R_FEDERAL_INCOME_TAXES(MO)
     +                                  - AVAILABLE_FED_GENERAL_CREDITS)
                     AVAILABLE_FED_GENERAL_CREDITS = 0.
                  ENDIF
               ENDIF
            ENDIF
C
            R_FEDERAL_INCOME_TAXES(MO) = R_FEDERAL_INCOME_TAXES(MO)
     +                             + R_FEDERAL_INCOME_TAX_ADJUSTMENT(MO)
            R_FEDERAL_TAXABLE_INCOME(0) = R_FEDERAL_TAXABLE_INCOME(0)
     +                                    + R_FEDERAL_TAXABLE_INCOME(MO)
            R_FED_INCOME_TAXES_B4_CREDITS(0) =
     +                               R_FED_INCOME_TAXES_B4_CREDITS(0)
     +                               + R_FED_INCOME_TAXES_B4_CREDITS(MO)
C
C CALCULATE BTL NET TAXABLE INCOME
C
            
            IF(R_CLASS_TYPE == CONSOLIDATED) THEN
               R_FEDERAL_BTL_TAXABLE_INCOME(MO) =
     +                               R_BTL_TAXABLE_REVENUES(MO)
     +                               - R_BTL_EXPENSE_DEDUCTIONS(MO)
     +                               - R_STATE_BTL_INCOME_TAX(MO)
     +                               - R_FEDERAL_BTL_MISC_DEDUCTIONS(MO)
               R_FEDERAL_BTL_INCOME_TAX(MO) =
     +                                    SUB_BTL_FEDERAL_TAXES_PAID(MO)
               R_BTL_TAXABLE_INVESTMENT(MO) =
     +                                 SUB_BTL_TAXABLE_INVEST_INCOME(MO)
            ELSEIF(CALCULATE_BTL_INCOME_TAXES) THEN
               R_FEDERAL_BTL_TAXABLE_INCOME(MO) =
     +                               R_BTL_TAXABLE_REVENUES(MO)
     +                               - R_BTL_EXPENSE_DEDUCTIONS(MO)
     +                               - R_STATE_BTL_INCOME_TAX(MO)
     +                               - R_FEDERAL_BTL_MISC_DEDUCTIONS(MO)
               R_FEDERAL_BTL_INCOME_TAX(MO) =
     +                                R_FEDERAL_BTL_TAXABLE_INCOME(MO) *
     +                                            R_FEDERAL_TAX_RATE(MO)
            ELSE
               R_FEDERAL_BTL_TAXABLE_INCOME(MO) = 0.
               R_FEDERAL_BTL_INCOME_TAX(MO) = 0.
            ENDIF
C
C CALCULATE DEFERRED TAXES DR CAUSED BY NOLs
C
            R_FED_DEF_TAX_DR_NOLS_AMT(MO) = R_FEDERAL_TAX_RATE(MO) *
     +                                  (R_FEDERAL_NOLS_USED(MO)
     +                                   - R_FEDERAL_NOLS_GENERATED(MO))
            R_FEDERAL_NOLS_USED(0) = R_FEDERAL_NOLS_USED(0)
     +                               + R_FEDERAL_NOLS_USED(MO)
            R_FEDERAL_NOLS_GENERATED(0) = R_FEDERAL_NOLS_GENERATED(0)
     +                                    + R_FEDERAL_NOLS_GENERATED(MO)
         ENDDO
C
C AMT ADJUSTMENTS FOR FEDERAL INCOME TAXES
C
         R_FEDERAL_AMT_CREDITS_USED(0) = ANNUAL_VARS(400)
         R_FEDERAL_AMT_CREDITS_USED(12) = ANNUAL_VARS(400)
         R_FED_AMT_CREDITS_GENERATED(0) = ANNUAL_VARS(401)
         R_FED_AMT_CREDITS_GENERATED(12) = ANNUAL_VARS(401)
         R_FEDERAL_INCOME_TAXES(12) = R_FEDERAL_INCOME_TAXES(12)
     +                                - R_FEDERAL_AMT_CREDITS_USED(12)
     +                                + R_FED_AMT_CREDITS_GENERATED(12)
         R_FED_DEF_TAX_DR_NOLS_AMT(12) = R_FED_DEF_TAX_DR_NOLS_AMT(12)
     +                                 + R_FEDERAL_AMT_CREDITS_USED(12)
     +                                 - R_FED_AMT_CREDITS_GENERATED(12)
C
         R_FEDERAL_INCOME_TAXES(0) = SUM(R_FEDERAL_INCOME_TAXES(1:))
         R_FED_DEF_TAX_DR_NOLS_AMT(0)=SUM(R_FED_DEF_TAX_DR_NOLS_AMT(1:))
         R_BTL_TAXABLE_REVENUES(0) = SUM(R_BTL_TAXABLE_REVENUES(1:))
         R_BTL_EXPENSE_DEDUCTIONS(0) = SUM(R_BTL_EXPENSE_DEDUCTIONS(1:))
         R_FEDERAL_BTL_TAXABLE_INCOME(0) =
     +                             SUM(R_FEDERAL_BTL_TAXABLE_INCOME(1:))
         R_FEDERAL_BTL_INCOME_TAX(0)= SUM(R_FEDERAL_BTL_INCOME_TAX(1:))
C
C CHECK FOR ACTUAL FEDERAL INCOME TAXES
C
         CALL ACTUAL_MONTHLY_INCOME_TAX_ITEMS(R_YR,R_CLASS,
     +                                        R_FEDERAL_INCOME_TAXES,
     +                                        FED_TAX_CODE)
      RETURN
C***********************************************************************
      ENTRY RPT_STATE_TAX_INCOME_B4_NOLS(R_MO)
C***********************************************************************
C
         RPT_STATE_TAX_INCOME_B4_NOLS =
     +                              R_STATE_TAXABLE_INCOME_B4_NOLS(R_MO)
      RETURN
C************************************************************************
      ENTRY RPT_STATE_NOLS_USED(R_MO)
C************************************************************************
C
         RPT_STATE_NOLS_USED = R_STATE_NOLS_USED(R_MO)
      RETURN
C************************************************************************
      ENTRY RPT_STATE_NOLS_GENERATED(R_MO)
C************************************************************************
C
         RPT_STATE_NOLS_GENERATED = R_STATE_NOLS_GENERATED(R_MO)
      RETURN
C************************************************************************
      ENTRY RPT_STATE_TAXABLE_INCOME(R_MO)
C***********************************************************************
C
         RPT_STATE_TAXABLE_INCOME = R_STATE_TAXABLE_INCOME(R_MO)
      RETURN
C***********************************************************************
      ENTRY RPT_STATE_INCOME_TAX_B4_CREDITS(R_MO)
C***********************************************************************
C
         RPT_STATE_INCOME_TAX_B4_CREDITS =
     +                               R_STATE_INCOME_TAX_B4_CREDITS(R_MO)
      RETURN
C***********************************************************************
      ENTRY RPT_FED_INCOME_TAX_B4_CREDITS(R_MO)
C***********************************************************************
C
         RPT_FED_INCOME_TAX_B4_CREDITS =
     +                               R_FED_INCOME_TAXES_B4_CREDITS(R_MO)
      RETURN
C***********************************************************************
      ENTRY RPT_STATE_INCOME_TAX(R_MO)
C***********************************************************************
C
         RPT_STATE_INCOME_TAX = R_STATE_INCOME_TAX(R_MO)
      RETURN
C***********************************************************************
      ENTRY RPT_FED_TAX_INCOME_B4_NOLS(R_MO)
C***********************************************************************
C
         RPT_FED_TAX_INCOME_B4_NOLS = R_FED_TAXABLE_INCOME_B4_NOLS(R_MO)
      RETURN
C***********************************************************************
      ENTRY RPT_FEDERAL_NOLS_USED(R_MO)
C***********************************************************************
C
         RPT_FEDERAL_NOLS_USED = R_FEDERAL_NOLS_USED(R_MO)
      RETURN
C***********************************************************************
      ENTRY RPT_FEDERAL_NOLS_GENERATED(R_MO)
C***********************************************************************
C
         RPT_FEDERAL_NOLS_GENERATED = R_FEDERAL_NOLS_GENERATED(R_MO)
      RETURN
C***********************************************************************
      ENTRY RPT_FEDERAL_TAXABLE_INCOME(R_MO)
C***********************************************************************
C
C
         RPT_FEDERAL_TAXABLE_INCOME = R_FEDERAL_TAXABLE_INCOME(R_MO)
      RETURN
C***********************************************************************
      ENTRY RPT_FEDERAL_INCOME_TAX(R_MO)
C***********************************************************************
C
C
         RPT_FEDERAL_INCOME_TAX = R_FEDERAL_INCOME_TAXES(R_MO)
      RETURN
C***********************************************************************
      ENTRY RPT_DEDUCTIBLE_BOOK_EXPENSES(R_MO)
C***********************************************************************
C
C
         RPT_DEDUCTIBLE_BOOK_EXPENSES = 
     +                               R_ATL_TAXABLE_BOOK_DEDUCTIONS(R_MO)
     +                               + R_NON_INCOME_TAX_DEDUCTIONS(R_MO) 
     +                               + R_INTEREST_DEDUCTIONS(R_MO) 
     +                               + R_BTL_EXPENSE_DEDUCTIONS(R_MO)
      RETURN
C***********************************************************************
      ENTRY RPT_TOTAL_DEDUCT_BOOK_EXPENSES(R_MO)
C***********************************************************************
C
         RPT_TOTAL_DEDUCT_BOOK_EXPENSES = 
     +                               R_ATL_TAXABLE_BOOK_DEDUCTIONS(R_MO)
     +                               + R_NON_INCOME_TAX_DEDUCTIONS(R_MO) 
     +                               + R_INTEREST_DEDUCTIONS(R_MO) 
     +                               + R_BTL_EXPENSE_DEDUCTIONS(R_MO)
     +                               + R_NON_BOOK_DEDUCTIBLE_ITEMS(R_MO)
      RETURN
C***********************************************************************
      ENTRY RPT_TOTAL_TAXABLE_INCOME(R_MO)
C***********************************************************************
C
C
         RPT_TOTAL_TAXABLE_INCOME = R_ATL_TAXABLE_REVENUES(R_MO)
     +                             + R_BTL_TAXABLE_REVENUES(R_MO)
     +                             + R_NON_BOOK_TAXABLE_ITEMS(R_MO)
     +                             - R_ATL_TAXABLE_BOOK_DEDUCTIONS(R_MO)
     +                             - R_NON_BOOK_DEDUCTIBLE_ITEMS(R_MO)
     +                             - R_NON_INCOME_TAX_DEDUCTIONS(R_MO) 
     +                             - R_INTEREST_DEDUCTIONS(R_MO) 
     +                             - R_BTL_EXPENSE_DEDUCTIONS(R_MO)

      RETURN
C***********************************************************************
      ENTRY RPT_BTL_TAXABLE_INCOME(R_MO)
C***********************************************************************
          RPT_BTL_TAXABLE_INCOME = R_BTL_TAXABLE_INCOME(R_MO)
      RETURN
C***********************************************************************
      ENTRY RPT_NON_BOOK_TAXABLE_ITEMS(R_MO)
C***********************************************************************
          RPT_NON_BOOK_TAXABLE_ITEMS = R_NON_BOOK_TAXABLE_ITEMS(R_MO)
      RETURN
C***********************************************************************
      ENTRY RPT_BTL_OTHER_TAXABLE_INCOME(R_MO)
C***********************************************************************
         IF(R_MO == 0) THEN
            R_BTL_OTHER_TAXABLE_INCOME(0) =
     +                               SUM(R_BTL_OTHER_TAXABLE_INCOME(1:))
         ENDIF
         RPT_BTL_OTHER_TAXABLE_INCOME = R_BTL_OTHER_TAXABLE_INCOME(R_MO)
      RETURN
C***********************************************************************
      ENTRY RPT_BTL_TAXABLE_INVESTMENT(R_MO)
C***********************************************************************
         IF(R_MO == 0) THEN
            R_BTL_TAXABLE_INVESTMENT(0) =
     +                                 SUM(R_BTL_TAXABLE_INVESTMENT(1:))
         ENDIF
         RPT_BTL_TAXABLE_INVESTMENT =  R_BTL_TAXABLE_INVESTMENT(R_MO)
      RETURN
C***********************************************************************
      ENTRY RPT_BTL_TAXABLE_EXPENSES(R_MO)
C***********************************************************************
         RPT_BTL_TAXABLE_EXPENSES = R_BTL_TAX_EXPENSE_DEDUCTIONS(R_MO) 
      RETURN
C***********************************************************************
      ENTRY RPT_TAXABLE_INCOME_B4_DEDUCTS(R_MO)
C***********************************************************************
C
C
         RPT_TAXABLE_INCOME_B4_DEDUCTS =
     +                                 R_TAXABLE_INCOME_B4_DEDUCTS(R_MO)

      RETURN
C***********************************************************************
      ENTRY RPT_STATE_BTL_TAXABLE_INCOME(R_MO)
C***********************************************************************
C
         RPT_STATE_BTL_TAXABLE_INCOME = R_STATE_BTL_TAXABLE_INCOME(R_MO)

      RETURN
C***********************************************************************
      ENTRY RPT_BTL_M1_TAX_DEDUCTIONS(R_MO)
C***********************************************************************
C
         RPT_BTL_M1_TAX_DEDUCTIONS = R_BTL_MISC_DEDUCTIONS(R_MO) 
      RETURN 
C***********************************************************************
      ENTRY RPT_STATE_BTL_INCOME_TAX(R_MO)
C***********************************************************************
C
         RPT_STATE_BTL_INCOME_TAX = R_STATE_BTL_INCOME_TAX(R_MO)

      RETURN
C***********************************************************************
      ENTRY RPT_STATE_ATL_INCOME_TAX(R_MO)
C***********************************************************************
C
         RPT_STATE_ATL_INCOME_TAX = R_STATE_INCOME_TAX(R_MO)
     +                              - R_STATE_BTL_INCOME_TAX(R_MO)

      RETURN
C***********************************************************************
      ENTRY RPT_FEDERAL_BTL_TAXABLE_INCOME(R_MO)
C***********************************************************************
C
         RPT_FEDERAL_BTL_TAXABLE_INCOME =
     +                                R_FEDERAL_BTL_TAXABLE_INCOME(R_MO)

      RETURN
C***********************************************************************
      ENTRY RPT_FEDERAL_BTL_INCOME_TAX(R_MO)
C***********************************************************************
C
         RPT_FEDERAL_BTL_INCOME_TAX = R_FEDERAL_BTL_INCOME_TAX(R_MO)

      RETURN
C***********************************************************************
      ENTRY RPT_STATE_GENERAL_CREDITS(R_MO)
C***********************************************************************
C
         RPT_STATE_GENERAL_CREDITS = R_STATE_GENERAL_CREDITS(R_MO)
      RETURN
C***********************************************************************
      ENTRY RPT_FEDERAL_SEC29_CREDITS(R_MO)
C***********************************************************************
C
         RPT_FEDERAL_SEC29_CREDITS = R_FEDERAL_SEC29_CREDITS(R_MO)
      RETURN
C***********************************************************************
      ENTRY RPT_FEDERAL_GENERAL_CREDITS(R_MO)
C***********************************************************************
C
         RPT_FEDERAL_GENERAL_CREDITS = R_FEDERAL_GENERAL_CREDITS(R_MO)
      RETURN
C***********************************************************************
      ENTRY RPT_FEDERAL_AMT_CREDITS_USED(R_MO)
C***********************************************************************
C
         RPT_FEDERAL_AMT_CREDITS_USED = R_FEDERAL_AMT_CREDITS_USED(R_MO)
      RETURN
C***********************************************************************
      ENTRY RPT_FED_AMT_CREDITS_GENERATED(R_MO)
C***********************************************************************
C
         RPT_FED_AMT_CREDITS_GENERATED=R_FED_AMT_CREDITS_GENERATED(R_MO)
      RETURN
C***********************************************************************
      ENTRY RPT_FEDERAL_ATL_INCOME_TAX(R_MO)
C***********************************************************************
C
         RPT_FEDERAL_ATL_INCOME_TAX = R_FEDERAL_INCOME_TAXES(R_MO)
     +                                - R_FEDERAL_BTL_INCOME_TAX(R_MO)

      RETURN
C***********************************************************************
      ENTRY SUM_MONTHLY_INCOME_EXPENSES(INCOME_VARIABLES)
C***********************************************************************
C
C NOTE THAT THE VARIABLE POSITIONS ARE OFFSET BY 30 TO USE THE ANNUAL LOCATION
C VALUES.  
C 
C
         SUM_MONTHLY_INCOME_EXPENSES = .TRUE.
         DO MO = 0, 12
            INCOME_VARIABLES(MO,mthly_pretax_expenses) =
     +              MONTHLY_TOTAL_EXPENSES_B4_TAXES(MO,INCOME_VARIABLES)
         ENDDO
      RETURN
C
C***********************************************************************
      ENTRY SUM_MONTHLY_TAXES_BTL_ITEMS(INCOME_VARIABLES)
C***********************************************************************
C
         SUM_MONTHLY_TAXES_BTL_ITEMS = .TRUE.
         DO MO = 0, 12
            INCOME_VARIABLES(MO,Monthly_Total_Taxes_Expense) =
     +              MONTHLY_TOTAL_EXPENSES_B4_TAXES(MO,INCOME_VARIABLES)
     +              + TOTAL_TAXES_EXPENSE(MO,INCOME_VARIABLES) 
            INCOME_VARIABLES(MO,Monthly_Op_Income) = 
     +          INCOME_VARIABLES(MO,monthly_total_revenues)
     +          - INCOME_VARIABLES(MO,Monthly_Total_Taxes_Expense)
            INCOME_VARIABLES(MO,Monthly_INCOME_BEFORE_INTEREST) = 
     +                       INCOME_BEFORE_INTEREST(MO,INCOME_VARIABLES)
            INCOME_VARIABLES(MO,Monthly INCOME AFTER INTEREST) = 
     +                MONTHLY_INCOME_AFTER_INTEREST(MO,INCOME_VARIABLES)
            INCOME_VARIABLES(MO,Monthly Net Income) = 
     +                           MONTHLY_NET_INCOME(MO,INCOME_VARIABLES)
            INCOME_VARIABLES(MO,Monthly_Earnings_2_Common) = 
     +          INCOME_VARIABLES(MO,Monthly Net Income)
     +          - INCOME_VARIABLES(MO,Monthly_Total_PS_Dividends)
            INCOME_VARIABLES(MO,Monthly RETAINED EARNINGS) = 
     +          INCOME_VARIABLES(MO,Monthly_Earnings_2_Common)
     +          - INCOME_VARIABLES(MO,Monthly Common Dividends)
         ENDDO
      RETURN
      END
C***********************************************************************
      FUNCTION INCOME_BEFORE_INTEREST(MO,INCOME_VARIABLES)
C***********************************************************************
C
      use class_run_switchesc
      use class_run_switchesl1
      use class_run_switchesl4

      INCLUDE 'MTHNMCOM.MON'
      INCLUDE 'NAMESCOM.MON'

      REAL*4 INCOME_BEFORE_INTEREST,TOTAL_TAXES_EXPENSE,
     +       MONTHLY_INCOME_AFTER_INTEREST,MONTHLY_NET_INCOME,
     +       MONTHLY_TOTAL_EXPENSES_B4_TAXES
      REAL*4 INCOME_VARIABLES(0:12,*)
      REAL*4 SUM_REGULATED_REVENUES,SUM_NONREGULATED_REVENUES,
     +       MONTHLY_ATL_INCOME_STATEMENT_TAX_DEDUCTIONS
      INTEGER*2 MO
      LOGICAL*1 WVPA,IMPA
C      
         INCOME_VARIABLES(MO,BTL_Monthly_Interest_Income) =       ! 101
     +              INCOME_VARIABLES(MO,BTL_Monthly_STInvestmet_Income)
     +              + INCOME_VARIABLES(MO,
     +                                mty_mdl_lt_nvst_income)
     +              + INCOME_VARIABLES(MO,
     +                             mt_dbt_file_linvst_income)
     +              + INCOME_VARIABLES(MO,
     +                                  monthly_notes_receivable_income) 
         INCOME_BEFORE_INTEREST = 
     +          INCOME_VARIABLES(MO,Monthly_Op_Income)
     +          + INCOME_VARIABLES(MO,btl_monthly_deferred_revenues)
     +          + INCOME_VARIABLES(MO,BTL_monthly_other_income)
     +          + INCOME_VARIABLES(MO,MonthlyMarkToMarket)
     +          + INCOME_VARIABLES(MO,MonthlyEarningNonCompany)
     +          + INCOME_VARIABLES(MO,BTL_Monthly_NoTax_Other_Income)
     +          + INCOME_VARIABLES(MO,Monthly_Investment_Earnings)       ! 157
     +          + INCOME_VARIABLES(MO,BTL_Monthly_Interest_Income)       ! 101

     +          - INCOME_VARIABLES(MO,BTL_Monthly_Expenses)              ! 58
     +          - INCOME_VARIABLES(MO,BTL_Monthly_Income_Taxes)
     +          - INCOME_VARIABLES(MO,BTL_Monthly_Income_Tax_Deferrals)
     +          - INCOME_VARIABLES(MO,BTL_Monthly_Amortization)
     +          + INCOME_VARIABLES(MO,Monthly_AFUDC_Equity)
      RETURN
C***********************************************************************
      ENTRY TOTAL_TAXES_EXPENSE(MO,INCOME_VARIABLES)
C***********************************************************************
            TOTAL_TAXES_EXPENSE = 
     +          INCOME_VARIABLES(MO,monthly_oth_taxes)
     +          + INCOME_VARIABLES(MO,Monthly_ITC_Amortization)

     +          + INCOME_VARIABLES(MO,MonthlyExpFilePropertyTaxes)
     +          + INCOME_VARIABLES(MO,
     +                          mth_modl_calcd_prop_taxes)
     +          + INCOME_VARIABLES(MO,Monthly_Operating_Revenue_Tax)
     +          + INCOME_VARIABLES(MO,mthy_state_atl_incm_tax_pd)
     +          + INCOME_VARIABLES(MO,Monthly_State_Tax_on_Capital)
     +          + INCOME_VARIABLES(MO,
     +                              mthy_fed_atl_income_tax_pd)
     +          + INCOME_VARIABLES(MO,Monthly_Federal_Tax_on_Capital)
     +          + INCOME_VARIABLES(MO,Monthly_Income_Tax_Deferrals_Dr)
     +          + INCOME_VARIABLES(MO,Monthly_Income_Tax_Deferrals_Cr)
     +          + INCOME_VARIABLES(MO,MonthlyPayrollTaxes)
      RETURN
C***********************************************************************
      ENTRY MONTHLY_INCOME_AFTER_INTEREST(MO,INCOME_VARIABLES)
C***********************************************************************
         MONTHLY_INCOME_AFTER_INTEREST =  
     +          INCOME_VARIABLES(MO,Monthly_INCOME_BEFORE_INTEREST)
     +          - INCOME_VARIABLES(MO,Monthly_LTD_Total_Interest)
     +          - INCOME_VARIABLES(MO,MonthlySTDInterest)
     +          - INCOME_VARIABLES(MO,mty_intst_on_nts_pyble)
     +          + INCOME_VARIABLES(MO,Monthly_AFUDC_Borrowed)
      RETURN
C***********************************************************************
      ENTRY MONTHLY_NET_INCOME(MO,INCOME_VARIABLES) 
C***********************************************************************
C
         MONTHLY_NET_INCOME =  
     +             INCOME_VARIABLES(MO,Monthly INCOME AFTER INTEREST)
     +             + INCOME_VARIABLES(MO,Monthly_Unused_Fed_Tax_Credits)
     +             + INCOME_VARIABLES(MO,Monthly_Subsidiary_Income)
     +             - INCOME_VARIABLES(MO,Monthly_Extraordinary_Items)
      RETURN
C***********************************************************************
      ENTRY MONTHLY_TOTAL_EXPENSES_B4_TAXES(MO,INCOME_VARIABLES)
C***********************************************************************
C
c
c atl tax expense
c
         MONTHLY_TOTAL_EXPENSES_B4_TAXES =
     +           MONTHLY_ATL_INCOME_STATEMENT_TAX_DEDUCTIONS(MO,
     +                                                 INCOME_VARIABLES)
     +           + INCOME_VARIABLES(MO,MonthlyDeferredFuelExpense)
     +           + INCOME_VARIABLES(MO,Monthly_Nuclear_Fuel_Expense)

     +           + INCOME_VARIABLES(MO,MonthlyDOEDecommissioning)
     +           + INCOME_VARIABLES(MO,MonthlyDOEDisposal)
     +           + INCOME_VARIABLES(MO,Monthly_Amortization)
     +           + INCOME_VARIABLES(MO,
     +                               Mthly_Lease_Amort_xpns)
     +           + INCOME_VARIABLES(MO,
     +                            Mthly_dfrd_rev_Amortization)
     +           + INCOME_VARIABLES(MO,Monthly_Book_Depreciation)
     +           + INCOME_VARIABLES(MO,monthly_vacation_pay)
     +           + INCOME_VARIABLES(MO,MonthlyPensionExpense)
     +           + INCOME_VARIABLES(MO,MonthlySTORMExpense)
     +           + INCOME_VARIABLES(MO,MonthlyExecBenefits)
     +           + INCOME_VARIABLES(MO,MonthlyIncentiveComp)

      RETURN
C***********************************************************************
      ENTRY SUM_REGULATED_REVENUES(MO,INCOME_VARIABLES)
C***********************************************************************
C
         SUM_REGULATED_REVENUES =   
     +            INCOME_VARIABLES(MO,Prior level Method Adjustment)
     +            + INCOME_VARIABLES(MO,Operating Method Adjustment)
     +            + INCOME_VARIABLES(MO,Prior Years Method Adjustment)
     +            + INCOME_VARIABLES(MO,BaseRates)
     +            + INCOME_VARIABLES(MO,UnbilledRevenues)
     +            + INCOME_VARIABLES(MO,Residential)
     +            + INCOME_VARIABLES(MO,Commercial)
     +            + INCOME_VARIABLES(MO,Industrial)
     +            + INCOME_VARIABLES(MO,Government)
     +            + INCOME_VARIABLES(MO,Lighting)
     +            + INCOME_VARIABLES(MO,MonthlyOthRegRevenues)
         IF(WVPA()) SUM_REGULATED_REVENUES =
     +                     SUM_REGULATED_REVENUES
     +                     + INCOME_VARIABLES(MO,
     +                             mty_wvpa_acrd_mbr_rvnues)
      RETURN
C***********************************************************************
      ENTRY SUM_NONREGULATED_REVENUES(MO,INCOME_VARIABLES)
C***********************************************************************
C
         IF(MO == 0) THEN
            INCOME_VARIABLES(0,TotalDerivativeRevenues) =
     +               SUM(INCOME_VARIABLES(1:,TotalDerivativeRevenues))
            INCOME_VARIABLES(0,TotalFuelDerivativeRevenues) =
     +          SUM(INCOME_VARIABLES(1:,TotalFuelDerivativeRevenues))
         ENDIF
         SUM_NONREGULATED_REVENUES = 
     +                INCOME_VARIABLES(MO,AdjustmentClause)
     +                + INCOME_VARIABLES(MO,PGAAdjustment)
     +                + INCOME_VARIABLES(MO,SecondarySales)
     +                + INCOME_VARIABLES(MO,CapacitySales)
     +                + INCOME_VARIABLES(MO,BulkPower)
     +                + INCOME_VARIABLES(MO,OtherRevenue)
     +                + INCOME_VARIABLES(MO,GasRevenues)
     +                + INCOME_VARIABLES(MO,RelationshipRevenues)
     +                + INCOME_VARIABLES(MO,MonthlyCatawbaRevenues)
     +                + INCOME_VARIABLES(MO,ICAP_Revs_mth)
     +                + INCOME_VARIABLES(MO,TotalDerivativeRevenues)
     +                + INCOME_VARIABLES(MO,
     +                                   TotalFuelDerivativeRevenues)
     +                + INCOME_VARIABLES(MO,
     +                                   MonthlyGasWholesaleRevenues)
     +                + INCOME_VARIABLES(MO,
     +                     mthy_incm_rsv_mgn_cap_sales)
     +                + INCOME_VARIABLES(MO,MonthlyOthATLRevenues)
         IF(WVPA()) SUM_NONREGULATED_REVENUES =
     +                 SUM_NONREGULATED_REVENUES
     +                 + INCOME_VARIABLES(MO,WVPA_NonMember_Power_Sales)
      RETURN
      END
C***********************************************************************
      FUNCTION MONTHLY_ATL_INCOME_STATEMENT_TAX_DEDUCTIONS(MO,
     +                                                 INCOME_VARIABLES)
C***********************************************************************
C
      INCLUDE 'MTHNMCOM.MON'

      REAL*4 INCOME_VARIABLES(0:12,*)
      REAL*4 MONTHLY_ATL_INCOME_STATEMENT_TAX_DEDUCTIONS
      INTEGER*2 MO
      LOGICAL*1 WVPA
C      
         IF(MO == 0) THEN
            INCOME_VARIABLES(0,TotalDerivativeExpenses) =
     +               SUM(INCOME_VARIABLES(1:,TotalDerivativeExpenses))
            INCOME_VARIABLES(0,TotalFuelDerivativeExpenses) =
     +          SUM(INCOME_VARIABLES(1:,TotalFuelDerivativeExpenses))
         ENDIF

         MONTHLY_ATL_INCOME_STATEMENT_TAX_DEDUCTIONS =
     +           INCOME_VARIABLES(MO,Monthly_Fossil_Fuel)
     +           + INCOME_VARIABLES(MO,monthly_purchased_power)
     +           + INCOME_VARIABLES(MO,monthly_variable_oandm)
     +           + INCOME_VARIABLES(MO,monthly_fixed_oandm)
     +           + INCOME_VARIABLES(MO,MonthlyServiceTransactions)
     +           + INCOME_VARIABLES(MO,monthly_other_oandm)
     +           + INCOME_VARIABLES(MO,monthly_purchased_gas)
     +           + INCOME_VARIABLES(MO,Monthly Other)
     +           + INCOME_VARIABLES(MO,MonthlyTransmissionOperation)
     +           + INCOME_VARIABLES(MO,monthly_xmsn_maintnc)
     +           + INCOME_VARIABLES(MO,MonthlyDistributionOperation)
     +           + INCOME_VARIABLES(MO,MonthlyDistributionMaintenance)
     +           + INCOME_VARIABLES(MO,MonthlyCustomerAccounts)
     +           + INCOME_VARIABLES(MO,MonthlyCustomerServices)
     +           + INCOME_VARIABLES(MO,MonthlySalesExpense) 
     +           + INCOME_VARIABLES(MO,MonthlyAGOperations) 
     +           + INCOME_VARIABLES(MO,MonthlyAGMaintenance)
     +           + INCOME_VARIABLES(MO,Monthly_Lease_Interest_Expense)
     +           + INCOME_VARIABLES(MO,MonthlyDSMExpense)
     +           + INCOME_VARIABLES(MO,MonthlyDSMRebate)
     +           + INCOME_VARIABLES(MO,MonthlyEmissionCredits)
     +           + INCOME_VARIABLES(MO,MonthlyCatawbaExpenses)
     +           + INCOME_VARIABLES(MO,TotalDerivativeExpenses)
     +           + INCOME_VARIABLES(MO,TotalFuelDerivativeExpenses)
     +           + INCOME_VARIABLES(MO,Monthly_Retirement_Expense)
     +           + INCOME_VARIABLES(MO,
     +                 mty_xpns_rsv_mgn_cap_pchs)
     +           + INCOME_VARIABLES(MO,
     +                                wvpa_prop_txs_n_pwr_cst)
     +           + INCOME_VARIABLES(MO,MonthlyAssessedOPEB)
     +           + INCOME_VARIABLES(MO,MonthlyOtherATLExpenses)

      RETURN
      END
C***********************************************************************
      FUNCTION SUM_MONTHLY_BALANCE_STATEMENT(BALANCE_SHEET_VARIABLES)
C***********************************************************************
C
      REAL*4 BALANCE_SHEET_VARIABLES(0:12,*)
      REAL*4 SUM_MONTHLY_BALANCE_STATEMENT
      REAL*4 SUM_MONTHLY_LIABILITIES,SUM_MONTHLY_ASSETS
C
         SUM_MONTHLY_BALANCE_STATEMENT =
     +                  SUM_MONTHLY_LIABILITIES(BALANCE_SHEET_VARIABLES)
         SUM_MONTHLY_BALANCE_STATEMENT =
     +                SUM_MONTHLY_ASSETS(BALANCE_SHEET_VARIABLES)
      RETURN
      END
C***********************************************************************
      FUNCTION SUM_MONTHLY_ASSETS(BALANCE_SHEET_VARIABLES)
C***********************************************************************
C
      INCLUDE 'NAMESCOM.MON'
      INCLUDE 'MTHNMCOM.MON'

      INTEGER*2 MO,I
      REAL*4 BALANCE_SHEET_VARIABLES(0:12,*)
      REAL*4 SUM_MONTHLY_LIABILITIES,SUM_MONTHLY_ASSETS
      REAL*4 ASSET_TOTAL,LIABS_TOTAL
C
         DO MO = 0, 12
            BALANCE_SHEET_VARIABLES(MO,monthly_total_utility_plant) =
     +                        BALANCE_SHEET_VARIABLES(MO,
     +                                   monthly_gross_plant_in_svc)
     +                        + BALANCE_SHEET_VARIABLES(MO,mtly_cwip)
            BALANCE_SHEET_VARIABLES(MO,monthly_net_utility_plant) =
     +           BALANCE_SHEET_VARIABLES(MO,monthly_total_utility_plant)
     +           - BALANCE_SHEET_VARIABLES(MO,
     +                                 monthly_accum_depreciation)
     +           + BALANCE_SHEET_VARIABLES(MO,monthly_net_nuclear_fuel)
            ASSET_TOTAL =
     +             BALANCE_SHEET_VARIABLES(MO,monthly_net_utility_plant)
            DO I = monthly_accounts_receivable_bal,
     +                                            monthly_total_assets-3 ! 27
               IF(I == monthly_retirements) CYCLE ! NOT PART OF TOTAL
               IF(I == monthly_acc_dep_adjs) CYCLE ! NOT PART OF TOTAL
               IF(I == monthly_lti) CYCLE ! NOT PART OF TOTAL
               ASSET_TOTAL = ASSET_TOTAL + BALANCE_SHEET_VARIABLES(MO,I)
            ENDDO
            ASSET_TOTAL = ASSET_TOTAL
     +                    + BALANCE_SHEET_VARIABLES(MO,
     +                                       monthly_gas_in_storage_bal)
     +                    + BALANCE_SHEET_VARIABLES(MO,
     +                                   monthly_matrls_supplies_bal)
     +                    + BALANCE_SHEET_VARIABLES(MO,
     +                                      monthly_aro_net_asset_value)
     +                    + BALANCE_SHEET_VARIABLES(MO,
     +                                              mty_net_fasb_87)

C     
            BALANCE_SHEET_VARIABLES(MO,monthly_total_assets)=ASSET_TOTAL
C
C
         ENDDO
         SUM_MONTHLY_ASSETS =
     +                   BALANCE_SHEET_VARIABLES(0,monthly_total_assets)
      RETURN
C***********************************************************************
      ENTRY SUM_MONTHLY_LIABILITIES(BALANCE_SHEET_VARIABLES)
C***********************************************************************
C
         BALANCE_SHEET_VARIABLES(:, Monthly_TOTAL_COMMON_EQUITY) =
     +          BALANCE_SHEET_VARIABLES(:,monthly_retained_earnings_bal)
     +          + BALANCE_SHEET_VARIABLES(:,monthly_common_stock)
     +          + BALANCE_SHEET_VARIABLES(:,monthly_oci_value)
C
         BALANCE_SHEET_VARIABLES(:,monthly_total_capital) =
     +       BALANCE_SHEET_VARIABLES(:, Monthly_TOTAL_COMMON_EQUITY)
     +        + BALANCE_SHEET_VARIABLES(:,monthly_preferred_stock)
     +       + BALANCE_SHEET_VARIABLES(:,monthly_long_term_debt)
C
         BALANCE_SHEET_VARIABLES(:,
     +                          mty_ltd_bal_wo_ct_amt) =
     +           BALANCE_SHEET_VARIABLES(:,monthly_long_term_debt)
     +           - BALANCE_SHEET_VARIABLES(:,monthly_ltd_current_amount)
         BALANCE_SHEET_VARIABLES(:,
     +                           mty_ttl_cptl_wo_ct_ltd) =
     +           BALANCE_SHEET_VARIABLES(:,monthly_total_capital)
     +           - BALANCE_SHEET_VARIABLES(:,monthly_ltd_current_amount)
C
         BALANCE_SHEET_VARIABLES(:,mty_tot_dfrd_creds) =
     +          BALANCE_SHEET_VARIABLES(:,monthly_deferred_gains)
     +          + BALANCE_SHEET_VARIABLES(:,monthly_cica)
     +          + BALANCE_SHEET_VARIABLES(:,
     +                                    mty_dfrd_income_taxes)
     +          + BALANCE_SHEET_VARIABLES(:,mty_dfrd_itc_credt)
     +          + BALANCE_SHEET_VARIABLES(:,
     +                               monthly_gain_on_reaqd_debt_bal)
     +          + BALANCE_SHEET_VARIABLES(:,
     +                                mty_regty_liablties_bal)
     +          + BALANCE_SHEET_VARIABLES(:,
     +                                   monthly_other_deferred_credits)


         DO MO = 0, 12
            BALANCE_SHEET_VARIABLES(MO,monthly_total_liabilities) =
     +                 BALANCE_SHEET_VARIABLES(MO,monthly_total_capital)
     +                 + BALANCE_SHEET_VARIABLES(MO,
     +                                      monthly_aro_liability_value)
     +                 + BALANCE_SHEET_VARIABLES(MO,
     +                               monthly_gain_on_reaqd_debt_bal)
     +                 + BALANCE_SHEET_VARIABLES(MO,
     +                                mty_regty_liablties_bal)
     +                 + BALANCE_SHEET_VARIABLES(MO,
     +                                   monthly_other_deferred_credits)
     +                 + SUM(BALANCE_SHEET_VARIABLES(MO,
     +                                 monthly_accounts_payable_bal:
     +                                           monthly_notes_payable))
     +                 + BALANCE_SHEET_VARIABLES(MO,
     +                                       MonthlyExecBenefitsReserve)
     +                 + BALANCE_SHEET_VARIABLES(MO,
     +                                      MonthlyIncentiveCompReserve)
         ENDDO
         SUM_MONTHLY_LIABILITIES = 
     +              BALANCE_SHEET_VARIABLES(0,monthly_total_liabilities)
      RETURN
      END
C***********************************************************************
      SUBROUTINE MONTHLY_CLASS_OPERATION_REVS(R_CLASS_POS,
     +                                        R_MONTHLY_REVS)
C***********************************************************************
      use spindriftlib
      use prod_arrays_dimensions

      REAL MONTHLY_PRIOR_OPT_METHOD_REVS(:,:)
      ALLOCATABLE :: MONTHLY_PRIOR_OPT_METHOD_REVS
      SAVE MONTHLY_PRIOR_OPT_METHOD_REVS
      INTEGER*2 R_NUM_OF_ACTIVE_CLASSES,R_CLASS_POS,MO
      INTEGER VALUES_2_SET
      REAL R_MONTHLY_REVS(0:12),R_MONTHLY_REVS_PRIOR_YR(0:12)
C
         R_MONTHLY_REVS(:) =
     +                     MONTHLY_PRIOR_OPT_METHOD_REVS(:,R_CLASS_POS)
      RETURN
C***********************************************************************
      ENTRY SAVE_MONTHLY_CLASS_OPT_REVS(R_CLASS_POS,
     +                                  R_MONTHLY_REVS,
     +                                  R_MONTHLY_REVS_PRIOR_YR)
C***********************************************************************
         MONTHLY_PRIOR_OPT_METHOD_REVS(:,R_CLASS_POS) =
     +                     MONTHLY_PRIOR_OPT_METHOD_REVS(:,R_CLASS_POS)
     +                     + R_MONTHLY_REVS(:)
      RETURN
C***********************************************************************
      ENTRY INIT_MONTHLY_CLASS_OPT_REVS(R_NUM_OF_ACTIVE_CLASSES)
C***********************************************************************
         IF(ALLOCATED(MONTHLY_PRIOR_OPT_METHOD_REVS))
     +                         DEALLOCATE(MONTHLY_PRIOR_OPT_METHOD_REVS)
         ALLOCATE(MONTHLY_PRIOR_OPT_METHOD_REVS(0:12,
     +                                      -1:R_NUM_OF_ACTIVE_CLASSES))
         MONTHLY_PRIOR_OPT_METHOD_REVS = 0.
      RETURN
      END
C***********************************************************************
      FUNCTION SUM_CASH_RECEIPTS(R_MO,CASH_VARIABLES)
C***********************************************************************
C
      INCLUDE 'MTHNMCOM.MON'

      INTEGER*2 R_MO
      REAL*4 SUM_CASH_RECEIPTS,SUM_CASH_PAYMENTS,
     +       SUM_CONSTRUCTION_CASH_PAYMENTS,
     +       SUM_CASH_TOTAL_CAPITAL_REQUIREMENTS
      REAL*4 CASH_VARIABLES(0:12,*)
      LOGICAL*1 SALT_RIVER_PROJECT,WVPA
C
         SUM_CASH_RECEIPTS =
     +        CASH_VARIABLES(R_MO,Cash_Base_Rates)
     +        + CASH_VARIABLES(R_MO,Cash_Adjustment_Clause)
     +        + CASH_VARIABLES(R_MO,CashSecondarySales)
     +        + CASH_VARIABLES(R_MO,Cash_Other_Revenue)
     +        + CASH_VARIABLES(R_MO,cash_catawba_revenues)
     +        + CASH_VARIABLES(R_MO,Cash_Gas_Revenues)
     +        + CASH_VARIABLES(R_MO,Cash_Relationship_Revenues)
     +        + CASH_VARIABLES(R_MO,Cash_Residential)
     +        + CASH_VARIABLES(R_MO,Cash_Commercial)
     +        + CASH_VARIABLES(R_MO,Cash_Industrial)
     +        + CASH_VARIABLES(R_MO,Cash_Lighting)
     +        + CASH_VARIABLES(R_MO,CashBulkPower)
     +        + CASH_VARIABLES(R_MO,CashCapacitySales)
     +        + CASH_VARIABLES(R_MO,Cash_Government)
     +        + CASH_VARIABLES(R_MO,CashBTLRevenues)
     +        + CASH_VARIABLES(R_MO,CashNetofTaxBTLRevenues)
     +        + CASH_VARIABLES(R_MO,cash_rcd_accts_rcvbl)
     +        + CASH_VARIABLES(R_MO,CashICAPRevenues)
     +        + CASH_VARIABLES(R_MO,TotalCashDerivativeRevenues)
     +        + CASH_VARIABLES(R_MO,tot_cash_fuel_deriv_revenues)
     +        + CASH_VARIABLES(R_MO,CashGasWholesaleRevenues)
     +        + CASH_VARIABLES(R_MO,
     +                        csh_income_rsv_mgn_cap_sales)
     +        + CASH_VARIABLES(R_MO,
     +                             wvpa_cash_fm_nonmbr_pwr_sales)
     +        + CASH_VARIABLES(R_MO,cash_atl_other_rev)
c    +        + CASH_VARIABLES(R_MO,)
      RETURN
C***********************************************************************
      ENTRY SUM_CASH_PAYMENTS(R_MO,CASH_VARIABLES)
C***********************************************************************
C
         SUM_CASH_PAYMENTS =  
     +        CASH_VARIABLES(R_MO,csh_btl_lease_intrst_pymts)
     +        + CASH_VARIABLES(R_MO,Cash_Fossil_Fuel)
     +        + CASH_VARIABLES(R_MO,Cash_Purchased_Power)
     +        + CASH_VARIABLES(R_MO,CashServiceTransactions)
     +        + CASH_VARIABLES(R_MO,Cash_Variable_OandM)
     +        + CASH_VARIABLES(R_MO,Cash_Fixed_OandM)
     +        + CASH_VARIABLES(R_MO,Cash_Other_OandM)
     +        + CASH_VARIABLES(R_MO,CashPurchasedGas)
     +        + CASH_VARIABLES(R_MO,TotalCashDerivativeExpenses)
     +        + CASH_VARIABLES(R_MO,tot_cash_fuel_deriv_expenses)
     +        + CASH_VARIABLES(R_MO,Cash_Other)
     +        + CASH_VARIABLES(R_MO,Cash_Leased_Nuclear_Fuel)
     +        + CASH_VARIABLES(R_MO,CashDSMExpense)
     +        + CASH_VARIABLES(R_MO,CashDSMRebate)
     +        + CASH_VARIABLES(R_MO,CashLeaseInterestPayments)
     +        + CASH_VARIABLES(R_MO,CashEmissionCredits)
c    +          + CASH_VARIABLES(R_MO,CashDOEDecommissioning) ! 9/7/00 removed to match annual
     +        + CASH_VARIABLES(R_MO,CashDOEDisposal)
     +        + CASH_VARIABLES(R_MO,cash_catawba_expenses)
     +        + CASH_VARIABLES(R_MO,CashBTLExpenses)
     +        + CASH_VARIABLES(R_MO,CashTransmissionOperation)
     +        + CASH_VARIABLES(R_MO,CashTransmissionMaintenance)
     +        + CASH_VARIABLES(R_MO,CashDistributionOperation)
     +        + CASH_VARIABLES(R_MO,CashDistributionMaintenance)
     +        + CASH_VARIABLES(R_MO,CashCustomerAccounts)
     +        + CASH_VARIABLES(R_MO,CashCustomerServices)
     +        + CASH_VARIABLES(R_MO,CashSalesExpense)
     +        + CASH_VARIABLES(R_MO,CashAGOperations)
     +        + CASH_VARIABLES(R_MO,Cash_AG_Maintenance)
     +        + CASH_VARIABLES(R_MO,csh_unfundd_retrmt_pymts)
     +        + CASH_VARIABLES(R_MO,Cash_Storm_Payments)
     +        + CASH_VARIABLES(R_MO,Cash_Vacation_Payments)
     +        + CASH_VARIABLES(R_MO,Cash_Post_Retirement_Payments)
     +        + CASH_VARIABLES(R_MO,Cash_Paid_on_Accounts_Payable)
     +        + CASH_VARIABLES(R_MO,csh_2_post_retiremt_paymentss)
     +        + CASH_VARIABLES(R_MO,
     +                    csh_expns_rsv_mgn_cap_purch)
     +        + CASH_VARIABLES(R_MO,CashExecBenefits)
     +        + CASH_VARIABLES(R_MO,CashIncentiveCompReserve)
     +        + CASH_VARIABLES(R_MO,CashATLExpenses)
     +        + CASH_VARIABLES(R_MO,CashAssessedOPEB)
     +        + CASH_VARIABLES(R_MO,CashEarningsNonCorporate)
         IF(WVPA()) THEN
            SUM_CASH_PAYMENTS =
     +          SUM_CASH_PAYMENTS
     +          + CASH_VARIABLES(R_MO,WVPA_Cash_Member_Cost_of_Power) ! these are zero 5/3/04
     +          + CASH_VARIABLES(R_MO,wvpa_cash_nonmbr_pwr_cost) ! these are zero 5/3/04
     +          + CASH_VARIABLES(R_MO,wvpa_cash_mbr_svc_cost) ! 205
     +          + CASH_VARIABLES(R_MO,
     +                             wvpa_cash_nonmbr_svc_cost) ! 206
     +          + CASH_VARIABLES(R_MO,
     +                              wvpa_cash_2_accrd_mbr_revnue) ! 209

         ENDIF

      RETURN
C***********************************************************************
      ENTRY SUM_CONSTRUCTION_CASH_PAYMENTS(R_MO,CASH_VARIABLES)
C***********************************************************************
C
         CASH_VARIABLES(R_MO,class_net_invest_slvg_othr) =
     +               CASH_VARIABLES(R_MO,cash_net_investments)
     +               + CASH_VARIABLES(R_MO,cash_net_salvage)
     +               + CASH_VARIABLES(R_MO,cash_net_other)
         SUM_CONSTRUCTION_CASH_PAYMENTS =  
     +          CASH_VARIABLES(R_MO,cash_net_plant_capx)
     +          + CASH_VARIABLES(R_MO,cash_ai_investment)
     +          + CASH_VARIABLES(R_MO,cash_nclr_fl_fabrication)
     +          + CASH_VARIABLES(R_MO,class_net_invest_slvg_othr)
     +          + CASH_VARIABLES(R_MO,cash_equity_to_subsidiaries)
     +          + CASH_VARIABLES(R_MO,cash_capital_leases)
     +          + CASH_VARIABLES(R_MO,cash_acquisition_cost)
      RETURN
C***********************************************************************
      ENTRY SUM_CASH_TOTAL_CAPITAL_REQUIREMENTS(R_MO,CASH_VARIABLES)
C***********************************************************************
C
         CASH_VARIABLES(R_MO,cash_additions_to_inventories) =
     +        CASH_VARIABLES(R_MO,cash_fuel_inventory_chg)
     +        + CASH_VARIABLES(R_MO,cash_gas_strg_inventry_addtn)
     +        + CASH_VARIABLES(R_MO,class_matrls_inventry_addition)
C
         SUM_CASH_TOTAL_CAPITAL_REQUIREMENTS =  
     +              CASH_VARIABLES(R_MO,cash_total_construction)
     +              + CASH_VARIABLES(R_MO,cash_nuc_decommissioning_fund)
     +              + CASH_VARIABLES(R_MO,cash_working_capital)
     +              + CASH_VARIABLES(R_MO,cash_fuel_inventory_chg)
     +              + CASH_VARIABLES(R_MO,cash_ltd_retirements)
     +              + CASH_VARIABLES(R_MO,cash_ps_retirements)
     +              + CASH_VARIABLES(R_MO,cash_ltd_ps_issue_expense)
     +              + CASH_VARIABLES(R_MO,cash_common_issue_expense)
     +              + CASH_VARIABLES(R_MO,cash_common_stock_buyback)
     +              + CASH_VARIABLES(R_MO,cash_deferred_expenses)
     +              + CASH_VARIABLES(R_MO,cash_change_ltinvestments)
     +              + CASH_VARIABLES(R_MO,cash_change_debt_investments)
     +              + CASH_VARIABLES(R_MO,csh_4_redeeming_notes_owed)
     +              + CASH_VARIABLES(R_MO,cash_4_new_notes_issued)
     +              + CASH_VARIABLES(R_MO,CashLeaseExpense)
     +              + CASH_VARIABLES(R_MO,Cash_BTL_Lease_Cash)
     +              + CASH_VARIABLES(R_MO,
     +                              cash_gas_strg_inventry_addtn)
     +              + CASH_VARIABLES(R_MO,
     +                                class_matrls_inventry_addition)
     +              + CASH_VARIABLES(R_MO,cash_aro_payments)
     +              + CASH_VARIABLES(R_MO,cash_aro_trust_payments)

      RETURN
      END
C***********************************************************************
      FUNCTION THIS_IS_A_PAY_MONTH(ISSUE_MO,CURRENT_MO,PAID_WHEN)
C***********************************************************************
C
      LOGICAL THIS_IS_A_PAY_MONTH
      INTEGER*2 ISSUE_MO,CURRENT_MO
      CHARACTER*1 PAID_WHEN
C
         THIS_IS_A_PAY_MONTH = .FALSE. 
         IF(PAID_WHEN == 'M') THEN
            THIS_IS_A_PAY_MONTH = .TRUE. 
         ELSEIF(PAID_WHEN == 'Q' .OR. PAID_WHEN == 'C') THEN
            IF(CURRENT_MO == ISSUE_MO .OR.      
     +             CURRENT_MO == ISSUE_MO+3 .OR. 
     +                CURRENT_MO == ISSUE_MO+6 .OR. 
     +                   CURRENT_MO == ISSUE_MO+9)
     +                                      THIS_IS_A_PAY_MONTH = .TRUE.    
            IF(CURRENT_MO == ISSUE_MO-3 .OR. 
     +            CURRENT_MO == ISSUE_MO-6 .OR. 
     +               CURRENT_MO == ISSUE_MO-9)
     +                                      THIS_IS_A_PAY_MONTH = .TRUE.    
         ELSEIF(PAID_WHEN == 'S') THEN      
            IF(CURRENT_MO == ISSUE_MO .OR. 
     +            CURRENT_MO == ISSUE_MO+6 .OR.
     +               CURRENT_MO == ISSUE_MO-6)
     +                                      THIS_IS_A_PAY_MONTH = .TRUE.    
         ELSEIF(PAID_WHEN == 'A') THEN
            IF(CURRENT_MO == ISSUE_MO) THIS_IS_A_PAY_MONTH = .TRUE. 
         ENDIF
      RETURN
      END                  
C**********************************************************************
      SUBROUTINE MONTHLY_FASB87_CALCULATIONS(CLASS_ID,R_YR,
     +                             R_OTHER_COMPREHENSIVE_INCOME_BALANCE,
     +                             R_FASB_87_INTANGIBLE_ASSETS,
     +                             R_PENSION_LIABILITY,
     +                             R_OCI_DEFERRED_TAX_ADJ_DR,
     +                             R_DEFFER_TAX_EFFECTIVE_RATE)
C***********************************************************************

      INCLUDE 'MTHNMCOM.MON'

      INTEGER*2 CLASS_ID,R_YR,MO
      REAL R_OTHER_COMPREHENSIVE_INCOME_BALANCE,
     +     R_FASB_87_INTANGIBLE_ASSETS,
     +     R_PENSION_LIABILITY,
     +     R_OCI_DEFERRED_TAX_ADJ_DR
      REAL, SAVE :: MONTHLY_OCI_BALANCE(0:12),
     +              MONTHLY_FASB_87_BALANCE(0:12),
     +              PENSION_LIABILITY_ADJ(0:12),
     +              OCI_ADJ(0:12),
     +              INTANGIBLE_ASSET_ADJ(0:12),
     +              OCI_DEFERRED_TAXES(0:12),
     +              OCI_DEFERRED_TAX_ADJ_DR(0:12)
      REAL TEMP_SUM
      REAL R_MONTHLY_OCI_BALANCE(0:12),
     +     R_MONTHLY_FASB_87_BALANCE(0:12),
     +     R_FASB87_PENSION_LIAB_ADJ(0:12),
     +     R_FASB87_DEFERRED_TAX_ADJ_DR(0:12)
      REAL R_BALANCE_SHEET_VARIABLES(0:12,BAL_SHEET_VARS),
     +     R_DEFFER_TAX_EFFECTIVE_RATE
C
         MONTHLY_OCI_BALANCE(0) = R_OTHER_COMPREHENSIVE_INCOME_BALANCE
         MONTHLY_FASB_87_BALANCE(0) = R_FASB_87_INTANGIBLE_ASSETS
C
         CALL RETURN_MONTHLY_FASB87_ADDENDUMS(R_YR,CLASS_ID,
     +                                        OCI_ADJ,
     +                                        INTANGIBLE_ASSET_ADJ,
     +                                        PENSION_LIABILITY_ADJ,
     +                                        OCI_DEFERRED_TAX_ADJ_DR)

         OCI_DEFERRED_TAXES(:) = OCI_DEFERRED_TAX_ADJ_DR(:)
         R_OTHER_COMPREHENSIVE_INCOME_BALANCE = OCI_ADJ(0)
     +                            + R_OTHER_COMPREHENSIVE_INCOME_BALANCE
     +                            + OCI_DEFERRED_TAXES(0)
         R_FASB_87_INTANGIBLE_ASSETS = R_FASB_87_INTANGIBLE_ASSETS
     +                                 + INTANGIBLE_ASSET_ADJ(0)
         R_PENSION_LIABILITY = R_PENSION_LIABILITY
     +                         + PENSION_LIABILITY_ADJ(0)
         R_OCI_DEFERRED_TAX_ADJ_DR = OCI_DEFERRED_TAXES(0)
C
         DO MO = 1, 12 
            MONTHLY_OCI_BALANCE(MO) = MONTHLY_OCI_BALANCE(MO-1)
     +                                + OCI_ADJ(MO)
     +                                + OCI_DEFERRED_TAXES(MO)
            MONTHLY_FASB_87_BALANCE(MO) = MONTHLY_FASB_87_BALANCE(MO-1)
     +                                    + INTANGIBLE_ASSET_ADJ(MO)
         ENDDO
      RETURN
C***********************************************************************
      ENTRY RETURN_MONTHLY_FASB87_VALUES(R_BALANCE_SHEET_VARIABLES,
     +                                   R_FASB87_PENSION_LIAB_ADJ,
     +                                   R_FASB87_DEFERRED_TAX_ADJ_DR)
C***********************************************************************
C
         R_BALANCE_SHEET_VARIABLES(:,monthly_oci_value) =
     +                                            MONTHLY_OCI_BALANCE(:)
         R_BALANCE_SHEET_VARIABLES(:,mty_net_fasb_87) =
     +                                        MONTHLY_FASB_87_BALANCE(:)
         R_FASB87_PENSION_LIAB_ADJ(:) = PENSION_LIABILITY_ADJ(:)  
         R_FASB87_DEFERRED_TAX_ADJ_DR(:) = OCI_DEFERRED_TAXES(:)
      RETURN
      END
C***********************************************************************
      SUBROUTINE MONTHLY_CAP_X_ADJUSTMENTS(CLASS_ID,R_YR,
     +                                     R_EOY_NON_CASH_PENSION_ADJ)
C***********************************************************************
      INTEGER*2 CLASS_ID,R_YR,MO
      REAL*4 R_EOY_NON_CASH_PENSION_ADJ
      REAL, SAVE :: NON_CASH_PENSION_ADJ(0:12)
      REAL :: R_MONTHLY_NON_CASH_PENSION_ADJ(0:12)
C
         CALL RETURN_MONTHLY_CAPX_ADDENDUMS(R_YR,CLASS_ID,
     +                                      NON_CASH_PENSION_ADJ)
         R_EOY_NON_CASH_PENSION_ADJ = NON_CASH_PENSION_ADJ(0)
      RETURN
C***********************************************************************
      ENTRY RETURN_MONTHLY_CAP_X_VALUES(R_MONTHLY_NON_CASH_PENSION_ADJ)
C***********************************************************************
         R_MONTHLY_NON_CASH_PENSION_ADJ = NON_CASH_PENSION_ADJ
      RETURN
      END 
C***********************************************************************
      FUNCTION MONTHLY_NF_DEFERRED_TAXES_CR(R_CLASS,R_YR,
     +                                      R_NORMALIZATION_TAX_RATE,
     +                                      R_NF_DEFERRED_TAX_BASIS,
     +                                      R_NF_TAX_DEPRECIATION,
     +                                      R_LEASOR_NF_TAX_DEP,
     +                                      R_LEASOR_DEFERRED_TAX_BASIS)

C***********************************************************************
C
      use class_run_switchesc
      use class_run_switchesl1
      use class_run_switchesl4

      INCLUDE 'MTHNMCOM.MON'

      INTEGER*2 R_CLASS,RETURN_MONTHLY_NF_OP_EXPENSES,VOID_INT2,R_YR
      REAL R_NORMALIZATION_TAX_RATE,
     +     R_NF_DEFERRED_TAX_BASIS,
     +     R_LEASOR_NF_TAX_DEP,
     +     R_LEASOR_DEFERRED_TAX_BASIS,
     +     R_NF_TAX_DEPRECIATION
      REAL R_NF_DEFERRED_TAXES_CR
      REAL, SAVE :: MONTHLY_OWNED_NF(0:12),
     +              MONTHLY_LEASED_NF(0:12),
     +              MONTHLY_DOE_DISPOSAL(0:12),
     +              MONTHLY_DOE_DECOMMISSIONING(0:12),
     +              MONTHLY_NF_SL_DEF_TAX_DEP(0:12),
     +              MONTHLY_NF_TAX_EXPENSE(0:12),
     +              MONTHLY_NF_DEFERRED_TAX_BASIS(0:12),
     +              MONTHLY_NF_TAX_DEPRECIATION(0:12),
     +              MONTHLY_NF_DEFERRED_TAXES(0:12),
     +              MONTHLY_NUC_FUEL_OWNED_BURN(0:12)

      REAL MONTHLY_NF_DEFERRED_TAXES_CR,
     +     INCOME_VARIABLES(0:12,INCOME_VARS)
      LOGICAL*1 DOE_FEES_IN_NUC_FUEL_COST,DUKE
      REAL R_MONTHLY_NF_VALUES(0:12),RETURN_NF_DEFERRED_TAXES
C
         
         DOE_FEES_IN_NUC_FUEL_COST = DUKE()
         MONTHLY_NF_DEFERRED_TAXES_CR = R_NORMALIZATION_TAX_RATE *
     +                                           R_NF_DEFERRED_TAX_BASIS
         VOID_INT2 = RETURN_MONTHLY_NF_OP_EXPENSES(R_CLASS,
     +                                      MONTHLY_NUC_FUEL_OWNED_BURN)
C
         INCOME_VARIABLES = 0.
         CALL MONTHLY_EXPENSES(R_YR,R_CLASS,INCOME_VARIABLES(0,31))
         MONTHLY_OWNED_NF(:) = INCOME_VARIABLES(:,
     +                               monthly_ownd_nuc_fl_expense)
         MONTHLY_LEASED_NF(:) = INCOME_VARIABLES(:,
     +                              Mthy_Leasd_Nucl_Fuel_Expns)
         MONTHLY_DOE_DISPOSAL(:) = INCOME_VARIABLES(:,
     +                                             MonthlyDOEDisposal)
         MONTHLY_DOE_DECOMMISSIONING(:) = INCOME_VARIABLES(:,
     +                                      MonthlyDOEDecommissioning)
         MONTHLY_NF_SL_DEF_TAX_DEP(:) = INCOME_VARIABLES(:, 
     +                               monthly_ownd_nuc_fl_expense)
         IF(USE_BURN_4_NUC_FUEL_TAX_EXP) THEN
            MONTHLY_NF_TAX_EXPENSE(:) = MONTHLY_LEASED_NF(:)
     +                                  + MONTHLY_OWNED_NF(:)
            IF(DOE_FEES_IN_NUC_FUEL_COST) THEN
               MONTHLY_NF_SL_DEF_TAX_DEP(:) =
     +                                      MONTHLY_NF_SL_DEF_TAX_DEP(:)
     +                                      - MONTHLY_DOE_DISPOSAL(:)
            ELSE
               MONTHLY_NF_TAX_EXPENSE(:) = MONTHLY_NF_TAX_EXPENSE(:)
     +                                     + MONTHLY_DOE_DISPOSAL(:)
            ENDIF
            MONTHLY_NF_TAX_DEPRECIATION(1:) = R_LEASOR_NF_TAX_DEP/12.
            MONTHLY_NF_DEFERRED_TAX_BASIS(1:) =
     +                                   R_LEASOR_DEFERRED_TAX_BASIS/12.
            MONTHLY_NF_TAX_DEPRECIATION(0) = R_LEASOR_NF_TAX_DEP
            MONTHLY_NF_DEFERRED_TAX_BASIS(0) =
     +                                       R_LEASOR_DEFERRED_TAX_BASIS
         ELSE
            IF(DOE_FEES_IN_NUC_FUEL_COST) THEN
               MONTHLY_NF_TAX_EXPENSE(:) = MONTHLY_LEASED_NF(:)
     +                                  + MONTHLY_OWNED_NF(:)
     +                                  - MONTHLY_NUC_FUEL_OWNED_BURN(:)
               MONTHLY_NF_SL_DEF_TAX_DEP(:) =
     +                                      MONTHLY_NF_SL_DEF_TAX_DEP(:)
     +                                      - MONTHLY_DOE_DISPOSAL(:)
            ELSE
               MONTHLY_NF_TAX_EXPENSE(:) = MONTHLY_LEASED_NF(:)
     +                                     + MONTHLY_DOE_DISPOSAL(:)
            ENDIF
            MONTHLY_NF_TAX_DEPRECIATION(1:) = (R_NF_TAX_DEPRECIATION
     +                                        - R_LEASOR_NF_TAX_DEP)/12.
            MONTHLY_NF_TAX_DEPRECIATION(0) = R_NF_TAX_DEPRECIATION
     +                                       - R_LEASOR_NF_TAX_DEP
            MONTHLY_NF_DEFERRED_TAX_BASIS(1:) = 
     +                                 MONTHLY_NF_TAX_DEPRECIATION(1:)
     +                                 - MONTHLY_NF_SL_DEF_TAX_DEP(1:)
     +                                 + R_LEASOR_DEFERRED_TAX_BASIS/12.
            MONTHLY_NF_DEFERRED_TAX_BASIS(0) = 
     +                                    MONTHLY_NF_TAX_DEPRECIATION(0)
     +                                    - MONTHLY_NF_SL_DEF_TAX_DEP(0)
     +                                    + R_LEASOR_DEFERRED_TAX_BASIS
            MONTHLY_NF_TAX_DEPRECIATION(1:) = R_NF_TAX_DEPRECIATION/12.
            MONTHLY_NF_TAX_DEPRECIATION(0) = R_NF_TAX_DEPRECIATION
         ENDIF
         MONTHLY_NF_DEFERRED_TAXES(:) = R_NORMALIZATION_TAX_RATE *
     +                                  MONTHLY_NF_DEFERRED_TAX_BASIS(:)
         MONTHLY_NF_DEFERRED_TAXES_CR = MONTHLY_NF_DEFERRED_TAXES(0) 
      RETURN
C***********************************************************************
      ENTRY RETURN_NF_DEFERRED_TAXES(R_MONTHLY_NF_VALUES)
C***********************************************************************
C
           R_MONTHLY_NF_VALUES(:) = MONTHLY_NF_DEFERRED_TAXES(:)
           RETURN_NF_DEFERRED_TAXES = MONTHLY_NF_DEFERRED_TAXES(0)  
      RETURN
      END
C***********************************************************************
      SUBROUTINE MONTHLY_SUMMARY_INCOME_CALCULATIONS(INCOME_VARIABLES)
C***********************************************************************
C
      INCLUDE 'MTHNMCOM.MON'

      REAL*4 INCOME_VARIABLES(0:12,*)
c
c EBITAD and EBIT
c
         INCOME_VARIABLES(:,Monthly EBIT) =
     +         INCOME_VARIABLES(:,Monthly_INCOME_BEFORE_INTEREST)
     +         + INCOME_VARIABLES(:,Monthly_ITC_Amortization)
     +         + INCOME_VARIABLES(:,mthy_state_atl_incm_tax_pd)
     +         + INCOME_VARIABLES(:,Monthly_State_Tax_on_Capital)
     +         + INCOME_VARIABLES(:,mthy_fed_atl_income_tax_pd)
     +         + INCOME_VARIABLES(:,Monthly_Federal_Tax_on_Capital)
     +         + INCOME_VARIABLES(:,Monthly_Income_Tax_Deferrals_Dr)
     +         + INCOME_VARIABLES(:,Monthly_Income_Tax_Deferrals_Cr)
     +         + INCOME_VARIABLES(:,Monthly_AFUDC_Borrowed)
     +         + INCOME_VARIABLES(:,BTL_Monthly_Income_Taxes)
     +         + INCOME_VARIABLES(:,BTL_Monthly_Income_Tax_Deferrals)
         INCOME_VARIABLES(:,Monthly EBITAD) =
     +                   INCOME_VARIABLES(:,Monthly EBIT)
     +                   + INCOME_VARIABLES(:,Monthly_Book_Depreciation)
     +                   + INCOME_VARIABLES(:,Monthly_Amortization)
     +                   + INCOME_VARIABLES(:,BTL_Monthly_Amortization)
      END SUBROUTINE
C***********************************************************************
      SUBROUTINE IMPA_REPORT_VALUES(MO,INCOME_VARIABLES,CASH_VARIABLES,
     +                              OPERATING_METHOD)
C***********************************************************************
C
      INCLUDE 'MTHNMCOM.MON'

      REAL*4 INCOME_VARIABLES(0:12,*),SUM_VALUES,
     +       CASH_VARIABLES(0:12,*)
      REAL (KIND=4), PARAMETER :: NA=-999999.
      INTEGER (KIND=2) :: MO
      CHARACTER (LEN=1) :: OPERATING_METHOD
c
         INCOME_VARIABLES(MO,mty_impa_net_incm_cvrg) = NA
c         IF(OPERATING_METHOD == 'C') THEN
            INCOME_VARIABLES(MO,IMPA_Depreciation_Add_Back) =
     +               INCOME_VARIABLES(MO,Monthly_Book_Depreciation)
     +               + INCOME_VARIABLES(MO,Monthly_LTD_Amort_Interest)
     +               - CASH_VARIABLES(MO,cash_ltd_retirements)
            INCOME_VARIABLES(MO,Monthly Net Income) =
     +               INCOME_VARIABLES(MO,Monthly Net Income)  
     +               + INCOME_VARIABLES(MO,Monthly_Book_Depreciation)
     +               + INCOME_VARIABLES(MO,Monthly_LTD_Amort_Interest)
     +               - CASH_VARIABLES(MO,cash_ltd_retirements)
            INCOME_VARIABLES(MO,Monthly_Earnings_2_Common) = 
     +               INCOME_VARIABLES(MO,Monthly Net Income)
     +               - INCOME_VARIABLES(MO,Monthly_Total_PS_Dividends)
            INCOME_VARIABLES(MO,Monthly RETAINED EARNINGS) =
     +                 INCOME_VARIABLES(MO,Monthly_Earnings_2_Common)
     +                 - INCOME_VARIABLES(MO,Monthly Common Dividends)
c         ENDIF
C         DO  MO = 0, 12
            SUM_VALUES = CASH_VARIABLES(MO,cash_ltd_retirements) 
     +                 + INCOME_VARIABLES(MO,monthly_other_atl_amort)
     +                 + INCOME_VARIABLES(MO,Monthly_LTD_Total_Interest)
     +                 - INCOME_VARIABLES(MO,Monthly_LTD_Amort_Interest)
            IF(SUM_VALUES /= 0.) THEN
               INCOME_VARIABLES(MO,mty_impa_net_incm_cvrg) =
     +            1. + INCOME_VARIABLES(MO,Monthly Net Income)/
     +                                       SUM_VALUES
            ENDIF
C         ENDDO
      END SUBROUTINE


















