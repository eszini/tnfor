module namescom
!     ******************************************************************
!     Namescom.mon
! Converted to module
!
        ! DRILLING REPORT NAMES

      CHARACTER (kind=1), parameter :: BALANCE_SHEET_ITEM='B'
      CHARACTER (kind=1), parameter :: INCOME_REPORT_ITEM='I'
      CHARACTER (kind=1), parameter :: CASH_REPORT_ITEM='C'
      CHARACTER (kind=1), parameter :: TAX_REPORT_ITEM='T'
      CHARACTER (kind=1), parameter :: BALANCE_SHEET_ANNUAL_ITEM='A'

      ! NOTE IF THE EXPENSE FOR REVENUE IS IN THE EXPENSE FILE PUT THE 
	  ! NAME HERE

      INTEGER (KIND=2), PARAMETER ::ODEC_NA3_ID=401! ,BTL=2,ATL=1,Reg=0
      INTEGER (KIND=2), PARAMETER :: LAST_INCOME_LINE=80 !30)
      INTEGER (KIND=4), PARAMETER :: EXP_OFFSET_LINE=30

        ! REVENUES TYPES

      INTEGER (KIND=2), PARAMETER :: Base Rates = 1
      
      INTEGER (KIND=2), PARAMETER :: secondary_sales = 3
      integer (kind=2), parameter :: Other Revenue = 4
      integer (kind=2), parameter ::BTL Revenues = 5
      integer (kind=2), parameter ::Catawba Revenues = 6
      integer (kind=2), parameter ::Gas Revenues = 7
      integer (kind=2), parameter ::Unbilled Revenues = 8
      integer (kind=2), parameter ::Deferred Revenues = 9
      integer (kind=2), parameter ::Relationship Revenues = 10
      integer (kind=2), parameter ::Customer Revenues = 10
      integer (kind=2), parameter ::Residential = 11
      integer (kind=2), parameter ::Commercial = 12
      integer (kind=2), parameter ::Industrial = 13
      integer (kind=2), parameter ::Lighting = 14
      integer (kind=2), parameter ::Bulk Power = 15
      integer (kind=2), parameter ::Net of Tax BTL Revenues = 16
      integer (kind=2), parameter ::Capacity Sales = 17
      integer (kind=2), parameter ::Government = 18
      integer (kind=2), parameter ::PGA Adjustment = 19
      integer (kind=2), parameter ::Competitive Sales = 20
      integer (kind=2), parameter ::Utility Sales = 21
      !LAST_INCOME_LINE - 4,   ! 26
      integer (kind=2), parameter ::Prior Years Method Adjustment = 26
      !LAST_INCOME_LINE - 3,   ! 27
      integer (kind=2), parameter ::Prior level Method Adjustment = 27
      !LAST_INCOME_LINE - 2,        ! 28
      integer (kind=2), parameter ::Operating Method Adjustment = 28
      ! LAST_INCOME_LINE - 1 29
      integer (kind=2), parameter ::Total Base Revenues = 29
      ! LAST_INCOME_LINE)                ! 30
      integer (kind=2), parameter ::TOTAL OPERATING REVENUES = 30
      integer (kind=2), parameter ::Gas Wholesale Revenues = 31
      integer (kind=2), parameter::Derivative Physical Rev Variable = 32
      integer (kind=2), parameter ::Derivative Physical Rev Fixed = 33
      integer (kind=2), parameter ::Derivative Financial Rev Variable=34
      integer (kind=2), parameter::Derivative Financial Rev Fixed = 35
      integer (kind=2), parameter ::PayrollTaxes = 65
      integer (kind=2), parameter ::MarkToMarket = 66
      integer (kind=2), parameter ::AFUDCEquity = 67
      integer (kind=2), parameter ::AFUDCBorrowed = 68
      integer (kind=2), parameter ::PCA BTL Revenues = 69
      ! reused variable for FE
      integer (kind=2), parameter ::Associated Company Interest = 7
      integer (kind=2), parameter ::PJM MISO Retail Transmission = 10
      integer (kind=2), parameter ::PJM MISO FES Transmission = 19
      integer (kind=2), parameter ::Associated Company = 31
      integer (kind=2), parameter ::PJM MISO Revenues = 36
      ! new FE variables
      integer (kind=2), parameter ::Forfeited Discounts Rents = 37
      integer (kind=2), parameter ::Miscellaneous Service Revenues = 38
      integer (kind=2), parameter ::Retail Generation = 39
      integer (kind=2), parameter ::Wholesale Generation = 40
      integer (kind=2), parameter ::Energy Efficiency = 41
      integer (kind=2), parameter ::Net Interco Sale Leaseback = 42
      integer (kind=2), parameter ::Transitions = 43
      integer (kind=2), parameter ::SLB Wholesale Associated Co = 44
      integer (kind=2), parameter ::PJM Contra= 45
      integer (kind=2), parameter ::Interest Dividend Income = 46

      ! EXPENSE TYPES

      INTEGER (KIND=2), PARAMETER :: LAST_EXPENSE_ITEM=100
      INTEGER (KIND=2), PARAMETER :: Total Expenses B4 Taxes = 1
        integer (kind=2), parameter ::BTL Decommissioning Liability = 8
        integer (kind=2), parameter ::Exp Collection in Adj Clause = 9
        integer (kind=2), parameter ::         BTL Lease Cash = 10
        integer (kind=2), parameter ::          Fossil Fuel = 11
        integer (kind=2), parameter ::          Purchased Power = 12
        integer (kind=2), parameter ::          Variable OandM = 13
        integer (kind=2), parameter ::          Fixed OandM = 14
        integer (kind=2), parameter ::          Other OandM = 15
        integer (kind=2), parameter ::          Purchased Gas = 16
      integer (kind=2), parameter ::Purchased Capacity to Level RM = 16
        integer (kind=2), parameter ::          Other = 17
        integer (kind=2), parameter ::         Owned Nuclear Fuel = 18
        integer (kind=2), parameter ::         Leased Nuclear Fuel = 19
        integer (kind=2), parameter ::          Total Nuclear Fuel = 19
        integer (kind=2), parameter ::          DSM Expense = 20
        integer (kind=2), parameter ::          DSM Rebate = 21
        integer (kind=2), parameter ::      ATL Book Lease Expense = 22
        integer (kind=2), parameter ::        Service Transactions = 23
        integer (kind=2), parameter ::          Emission Credits = 24
       integer (kind=2), parameter ::          DOE Decommissioning = 25
        integer (kind=2), parameter ::          DOE Disposal = 26
        integer (kind=2), parameter ::          Catawba Expenses = 27
        integer (kind=2), parameter ::          BTL Expenses = 28
        integer (kind=2), parameter ::      Transmission Operation = 29
        integer (kind=2), parameter ::    Transmission Maintenance = 30
        integer (kind=2), parameter ::      Distribution Operation = 31
        integer (kind=2), parameter ::    Distribution Maintenance = 32
        integer (kind=2), parameter ::          Customer Accounts = 33
        integer (kind=2), parameter ::          Customer Services = 34
        integer (kind=2), parameter ::          Sales Expense = 35
        integer (kind=2), parameter ::          AG Operations = 36
        integer (kind=2), parameter ::          AG Maintenance = 37
        integer (kind=2), parameter ::          Amortization = 38
        integer (kind=2), parameter::Deferred Revenue Amortization = 39
        integer (kind=2), parameter ::      ATL Lease Amortization = 40
        integer (kind=2), parameter ::      BTL Lease Amortization = 41
        integer (kind=2), parameter ::          Book Depreciation = 42
        integer (kind=2), parameter ::          BTL Amortization = 43
        integer (kind=2), parameter ::        BTL Lease Interest = 44
        integer (kind=2), parameter ::        ATL Lease Interest = 45
        integer (kind=2), parameter ::          Deferred Fuel = 46
        integer (kind=2), parameter ::          Vacation Pay = 47
        integer (kind=2), parameter ::          Pension Expense = 48
        integer (kind=2), parameter ::    Unfunded Pension Expense = 56
        integer (kind=2), parameter ::          Storm Expense = 49
        integer (kind=2), parameter ::          STD Interest = 50
        integer (kind=2), parameter ::          LTD Interest = 51
        integer (kind=2), parameter ::         Preferred Dividends = 69
      integer(kind=2), parameter ::Derivative Physical Exp Variable = 52
        integer(kind=2), parameter::  Derivative Physical Exp Fixed = 53
      integer(kind=2), parameter::Derivative Financial Exp Variable = 54
      integer(kind=2), parameter::   Derivative Financial Exp Fixed = 55
        integer (kind=2), parameter ::       WVPA Member Purchases = 60
        integer (kind=2), parameter ::   WVPA Non Member Purchases = 57
        integer (kind=2), parameter ::       WVPA Member Services = 58
        integer (kind=2), parameter ::    WVPA Non Member Services = 59
        integer (kind=2), parameter::Cash Derative Physical Exp Var = 83
       integer(kind=2), parameter::Cash Derative Physical Exp Fixed = 84
        integer(kind=2), parameter::Cash Derative Financial Exp Var = 85
      integer(kind=2), parameter::Cash Derative Financial Exp Fixed = 86
      integer(kind=2), parameter::Cash WVPA Member Purchases = 87
      integer(kind=2), parameter::  Cash WVPA Non Member Purchases = 88
      integer(kind=2), parameter::       Cash WVPA Member Services = 89
      integer(kind=2), parameter::    Cash WVPA Non Member Services = 90

      ! TAX NAMES

      INTEGER (KIND=2), PARAMETER :: Item is taxed = 1
      INTEGER (KIND=2), PARAMETER :: Item is not taxed = 2
      INTEGER (KIND=2), PARAMETER :: Both = 1
      INTEGER (KIND=2), PARAMETER :: Federal = 2
      INTEGER (KIND=2), PARAMETER :: State = 3
      INTEGER (KIND=2), PARAMETER :: LAST_TAX_NOL_ITEM=6
      INTEGER (KIND=2), PARAMETER :: Federal NOLs = 35
      INTEGER (KIND=2), PARAMETER :: AMT NOLs = 36
      INTEGER (KIND=2), PARAMETER :: General Carry Tax Credits = 37
      INTEGER (KIND=2), PARAMETER :: Sec 42 Carry Tax Credits = 38
      INTEGER (KIND=2), PARAMETER :: state_nols = 39
      INTEGER (KIND=2), PARAMETER :: state_credits_carried_forward = 40
      INTEGER (KIND=2), PARAMETER :: LAST_TAX_ITEM=50
      INTEGER (KIND=2), PARAMETER :: Other Taxes = 1

      INTEGER (KIND=2), PARAMETER ::Property Taxes = 2
        INTEGER (KIND=2), PARAMETER :: Revenue Taxes = 3
        INTEGER (KIND=2), PARAMETER :: Income Tax Credits = 4
        INTEGER (KIND=2), PARAMETER :: Income Tax Adjustments = 5
        INTEGER (KIND=2), PARAMETER :: M1 Additions = 6
        INTEGER (KIND=2), PARAMETER :: M1 Deductions = 7
        INTEGER (KIND=2), PARAMETER :: Deferred Taxes Cr = 8
        INTEGER (KIND=2), PARAMETER :: Deferred Taxes Dr = 9
        INTEGER (KIND=2), PARAMETER :: ITC Amortization = 10
        INTEGER (KIND=2), PARAMETER :: BTL Tax Deductions = 11
        INTEGER (KIND=2), PARAMETER :: Tax Depreciation = 12
        INTEGER (KIND=2), PARAMETER :: AMT ACE Tax Depreciation = 13
        INTEGER (KIND=2), PARAMETER :: AMT Depreciation Preference = 14
        INTEGER (KIND=2), PARAMETER :: Sec 29 Credits = 15
        INTEGER (KIND=2), PARAMETER :: Sec 42 Credits = 16
        INTEGER (KIND=2), PARAMETER :: Federal Income Tax = 17
        INTEGER (KIND=2), PARAMETER :: ITC Amortization Rate = 18
        INTEGER (KIND=2), PARAMETER :: Local Tax Rate = 19
        INTEGER (KIND=2), PARAMETER :: Operating Revenue Tax Rate = 20
        INTEGER (KIND=2), PARAMETER :: Property Tax Rate = 21
        INTEGER (KIND=2), PARAMETER :: Other Taxes of Revenues Rate = 22
        INTEGER (KIND=2), PARAMETER :: Other Taxes of Expenses Rate= 23
        INTEGER (KIND=2), PARAMETER :: Federal Capitial Tax Rate = 24
        INTEGER (KIND=2), PARAMETER :: Local Capitial Tax Rate = 25
      INTEGER (KIND=2), PARAMETER :: Taxable Income Deductions Rate = 26
        INTEGER (KIND=2), PARAMETER :: AMT Income Addendum = 32
        INTEGER (KIND=2), PARAMETER :: Addendum Federal Capitial Tax=33
        INTEGER (KIND=2), PARAMETER :: Addendum Local Capitial Tax=34
      INTEGER (KIND=2), PARAMETER :: Federal Capitial Tax Deduction = 40
        INTEGER (KIND=2), PARAMETER :: Local Capitial Tax Deduction = 41
       INTEGER (KIND=2), PARAMETER :: Temporary ATL Tax Differences = 42
       INTEGER (KIND=2), PARAMETER :: Temporary BTL Tax Differences = 43
       INTEGER (KIND=2), PARAMETER :: Permanent ATL Tax Differences = 44
       INTEGER (KIND=2), PARAMETER :: Permanent BTL Tax Differences = 45
        INTEGER (KIND=2), PARAMETER :: BTL Deferred Taxes Cr = 46
        INTEGER (KIND=2), PARAMETER :: BTL Deferred Taxes Dr = 47

        ! PAYMENT NAMES FOR ORLANDO THESE ARE RESTRICTED FUND PAYMENTS
      INTEGER (KIND=2), PARAMETER :: LAST_PAYMENT_ITEM=10
      INTEGER (KIND=2), PARAMETER :: Decommissioning Fund = 1
      INTEGER (KIND=2), PARAMETER ::Decommissioning Liability Chan = 2
      INTEGER (KIND=2), PARAMETER ::Post Retirement Medical = 3
      INTEGER (KIND=2), PARAMETER :: Retiree Medical Payments = 4
      INTEGER (KIND=2), PARAMETER :: OUC Bond Sinking Fund=2
      INTEGER (KIND=2), PARAMETER :: OUC Long Term Investment=3
      INTEGER (KIND=2), PARAMETER :: OUC Debt Service Reserve=4
      INTEGER (KIND=2), PARAMETER :: OUC Customer Deposit Fund=5
      INTEGER (KIND=2), PARAMETER :: OUC Construction Trust Fund=6
      INTEGER (KIND=2), PARAMETER :: OUC Renewal Replacement Fund=7

      ! INVESTMENT NAMES FOR ORLANDO THESE ARE STABILIZATION/OTHER FUND
      ! DEPOSITS

      INTEGER (KIND=2), PARAMETER :: LAST_INVESTMENT_ITEM=10
      INTEGER (KIND=2), PARAMETER :: Dividend Subsidiary = 1
      INTEGER (KIND=2), PARAMETER :: Net Income Subsidiary = 2
      INTEGER (KIND=2), PARAMETER :: Investment in Subsidiary = 3
      INTEGER (KIND=2), PARAMETER :: Other Investments = 4
      INTEGER (KIND=2), PARAMETER :: Capitialized Lease Additions = 5
      INTEGER (KIND=2), PARAMETER :: Cash for Assets = 6
      INTEGER (KIND=2), PARAMETER :: Change in LTInvestments=7
      INTEGER (KIND=2), PARAMETER :: OUC Liability Reduction Fund = 1
      INTEGER (KIND=2), PARAMETER :: OUC Self Insurance Fund = 2
      INTEGER (KIND=2), PARAMETER :: OUC Base Stabilization Fund = 3
      INTEGER (KIND=2), PARAMETER :: OUC Fuel Stabilization Fund = 4
      INTEGER (KIND=2), PARAMETER :: OUC Water Stabilization Fund = 5
      INTEGER (KIND=2), PARAMETER :: OUC Customer Retention Fund = 6
      INTEGER (KIND=2), PARAMETER :: OUC Other = 7
      INTEGER (KIND=2), PARAMETER :: OUC Withdraw Liab Reduct Fd = 8


        ! RATE BASE NAMES

      INTEGER (KIND=2), PARAMETER :: LAST_RATE_BASE_ITEM=5
      INTEGER (KIND=2), PARAMETER :: Assets NEC Adjustment = 1
      INTEGER (KIND=2), PARAMETER :: Rate Base Adjustment = 2
      INTEGER (KIND=2), PARAMETER :: Deferred Tax Adjustment = 3
      INTEGER (KIND=2), PARAMETER :: Plant in Service Adjustment = 4

        ! CD/CIAC NAMES

      INTEGER (KIND=2), PARAMETER :: LAST_CD_CIAC_ITEM=5
      INTEGER (KIND=2), PARAMETER :: CD Balance as PC of Revenues = 1
      INTEGER (KIND=2), PARAMETER :: CD Addendum = 2
      INTEGER (KIND=2), PARAMETER :: CIAC Cash Contributions = 3
      INTEGER (KIND=2), PARAMETER :: CIAC Amortization Rate = 4
      INTEGER (KIND=2), PARAMETER :: CIAC Addendum to Amortization = 5

        ! Working Capital NAMES

      INTEGER (KIND=2), PARAMETER :: LAST_WORKING_CAPITAL_ITEM=15
      INTEGER (KIND=2), PARAMETER :: Assets NEC Addendum = 1
      INTEGER (KIND=2), PARAMETER :: Liabilities NEC Addendum = 2
      INTEGER (KIND=2), PARAMETER :: Fuel Inventory Change = 3
      INTEGER (KIND=2), PARAMETER :: Materials Supplies = 4
      INTEGER (KIND=2), PARAMETER :: Gas Storage = 5
      INTEGER (KIND=2), PARAMETER :: Rate Receivable Accounts = 6
      INTEGER (KIND=2), PARAMETER :: Addendum Receivable Accounts =7
      INTEGER (KIND=2), PARAMETER :: Rate Payable Accounts = 8
      INTEGER (KIND=2), PARAMETER :: Addendum Payable Accounts = 9
      INTEGER (KIND=2), PARAMETER :: Rate Assets NEC = 10
      INTEGER (KIND=2), PARAMETER :: Rate Liabilities NEC = 11
      INTEGER (KIND=2), PARAMETER :: Materials Supplies Out = 13
      INTEGER (KIND=2), PARAMETER :: Gas Storage Out = 14

        ! Earnings on Funds NAMES

      INTEGER (KIND=2), PARAMETER :: LAST_EARNINGS_ITEM=15
      INTEGER (KIND=2), PARAMETER :: Earnings Decommissioning Fund=1
      INTEGER(KIND=2), PARAMETER ::     Earnings Post Retirement Fund=2
      INTEGER(KIND=2), PARAMETER ::       Earnings ST Investments=3
      INTEGER(KIND=2), PARAMETER ::       Earnings LT Investments=4
      INTEGER(KIND=2), PARAMETER ::       STD Interest Rate=5
      INTEGER(KIND=2), PARAMETER ::       Decommissioning Rate=6
      INTEGER(KIND=2), PARAMETER ::       Post Retirement Rate=7
      INTEGER(KIND=2), PARAMETER ::       STI Rate=8
      INTEGER(KIND=2), PARAMETER ::       LTI Rate=9
      INTEGER(KIND=2), PARAMETER ::       Customer Deposits Rate=10
      INTEGER(KIND=2), PARAMETER ::       OCI Post Retirement Rate=11
      INTEGER(KIND=2), PARAMETER ::       OCI Decommissioning Rate=12

      ! Sale/Removal NAMES

      INTEGER (KIND=2), PARAMETER :: LAST_SALE_REMOVAL_ITEM=10
      INTEGER (KIND=2), PARAMETER :: Net Removal = 1
      INTEGER (KIND=2), PARAMETER :: Cash Received = 2
      INTEGER (KIND=2), PARAMETER :: Gross Book Value = 3
      INTEGER (KIND=2), PARAMETER :: Cumulative Depreciation = 4
      INTEGER (KIND=2), PARAMETER :: Sale Deferred Taxes Adjustment = 5
      INTEGER (KIND=2), PARAMETER :: Sale ATL Amortization = 6
      INTEGER (KIND=2), PARAMETER :: Sale BTL Amortization = 7
      INTEGER (KIND=2), PARAMETER :: Cost of Removal = 8
      INTEGER (KIND=2), PARAMETER :: Salvage Value = 9
      INTEGER (KIND=2), PARAMETER :: Addedum to Book Gain Loss = 10

        ! Shareholder Value NAMES
      INTEGER (KIND=2), PARAMETER :: LAST_SHAREHOLDER_VALUE_ITEM=4
      INTEGER (KIND=2), PARAMETER :: Equity Risk Adjustment = 1
      INTEGER (KIND=2), PARAMETER :: Economic Assets Adjustment = 2
      INTEGER (KIND=2), PARAMETER :: Operating Profits Adjustment = 3
      INTEGER (KIND=2), PARAMETER :: Capitial Recovery EXCLUDED OPAT = 4

        ! FASB Names

      INTEGER (KIND=2), PARAMETER :: LAST_FASB_ITEM = 15
      INTEGER (KIND=2), PARAMETER :: NucDecom Discount Rate=1
      
      INTEGER (KIND=2), PARAMETER :: NucDecom Liability Addendum=2
      INTEGER (KIND=2), PARAMETER :: OCI Adjustment=3
      INTEGER (KIND=2), PARAMETER :: FASB87 Intangible Asset Adj=4
      INTEGER (KIND=2), PARAMETER :: FABS87 Pension Liab Adj=5
      INTEGER (KIND=2), PARAMETER :: OCI Deferred Tax Adj=6
      INTEGER (KIND=2), PARAMETER :: ARO Discount Rate=1
      INTEGER (KIND=2), PARAMETER :: ARO Accretion Amount=2
      INTEGER (KIND=2), PARAMETER :: ARO Cash Payment=7
      INTEGER (KIND=2), PARAMETER :: ARO Cash to Bal Sheet Trust=8

        ! CapX Names

      INTEGER*2, PARAMETER :: LAST_CAPX_ITEM=6
      INTEGER*2, PARAMETER :: Non Cash Pension=1

        ! CASH NAMES

      INTEGER (KIND=2), PARAMETER :: LAST_CASH_ITEM = 5
      INTEGER (KIND=2), PARAMETER :: Revenue Receipts = 1
      INTEGER (KIND=2), PARAMETER :: Expense Payments = 2
      INTEGER (KIND=2), PARAMETER :: Cash Receipts = 3
      INTEGER (KIND=2), PARAMETER :: Cash Payments = 4

        ! Transfers

      INTEGER (KIND=2), PARAMETER :: LAST_TRANSFER_ITEM = 64
      INTEGER (KIND=2), PARAMETER :: Gross Plant Value = 1
      
        INTEGER (KIND=2), PARAMETER :: Cumulated Plant Depreciation = 2
        INTEGER (KIND=2), PARAMETER :: Balance Deferred Taxes = 3
        INTEGER (KIND=2), PARAMETER :: Deferred Debit Adjustment = 4
        INTEGER (KIND=2), PARAMETER :: ATL Debit Amortization = 5
        INTEGER (KIND=2), PARAMETER :: BTL Debt Amortization = 6
        INTEGER (KIND=2), PARAMETER :: Annual Deferred Taxes = 7
        INTEGER (KIND=2), PARAMETER :: Deferred ITCs Balance = 8
        INTEGER (KIND=2), PARAMETER :: Net Nuclear Fuel = 9
        INTEGER (KIND=2), PARAMETER :: Retained Earnings = 10
        INTEGER (KIND=2), PARAMETER :: Extra Ordinary Expense = 11
        INTEGER (KIND=2), PARAMETER :: LT Liabilities Balance = 12
        INTEGER (KIND=2), PARAMETER :: Balance Deferred Taxes Dr = 13
        INTEGER (KIND=2), PARAMETER :: CWIP Balance = 14
        INTEGER (KIND=2), PARAMETER :: Paid in Capital = 15
        INTEGER (KIND=2), PARAMETER :: ARO Net Assets = 16
        INTEGER (KIND=2), PARAMETER :: ARO Liabilities = 17
        INTEGER (KIND=2), PARAMETER :: Other Deferred Credits = 18
        INTEGER (KIND=2), PARAMETER :: Regulatory Deferred Credits = 19
        INTEGER (KIND=2), PARAMETER :: Reaquired Debt Gain = 20

        ! Cash to xxx

      INTEGER (KIND=2), PARAMETER :: LAST_CASH_TO_ITEMS = 10

        ! Actuals

        INTEGER (KIND=2), PARAMETER :: LAST_ACTUAL_ITEM = 15
        INTEGER (KIND=2), PARAMETER :: LAST_ACTUAL_TAX_ITEM=15
        INTEGER (KIND=2), PARAMETER :: Actual Deferred Taxes Cr = 1
        INTEGER (KIND=2), PARAMETER :: Actual Fed Income Taxes = 2
        INTEGER (KIND=2), PARAMETER :: Actual State Income Taxes = 3
        INTEGER (KIND=2), PARAMETER :: Actual Deferred Taxes Dr = 4
        INTEGER (KIND=2), PARAMETER :: Actual ATL Fed Income Taxes = 5
       INTEGER (KIND=2), PARAMETER :: Actual ATL State Income Taxes = 6
        INTEGER (KIND=2), PARAMETER :: Actual BTL Fed Income Taxes = 7
       INTEGER (KIND=2), PARAMETER :: Actual BTL State Income Taxes = 8

 ! OTHER ADDENDUM ITEMS

        INTEGER (KIND=2), PARAMETER :: LAST_OTHER_ITEM=10
        INTEGER (KIND=2), PARAMETER ::  SRP Target Sales=1
        INTEGER (KIND=2), PARAMETER ::  STD Minimum Balance=2
        INTEGER (KIND=2), PARAMETER ::  Cash Minimum Balance=3
        INTEGER (KIND=2), PARAMETER ::  OUC Fuel Inventory = 1
        INTEGER (KIND=2), PARAMETER ::  OUC Prepayments = 2
        INTEGER (KIND=2), PARAMETER ::  OUC Materials Inventory = 3
        INTEGER (KIND=2), PARAMETER ::  GRE DSC Principal Pay Adj = 6
        INTEGER (KIND=2), PARAMETER ::  GRE DSC Interest Pay Adj = 7

        ! INVESTMENT ACCOUNTS USED IN DEBT FILE

        INTEGER (KIND=2), PARAMETER :: NUM_INVESTMENT_ACCOUNTS=15
        INTEGER (KIND=2), PARAMETER :: NUM_OF_NOTE_ACCOUNTS=15
        INTEGER (KIND=2), PARAMETER :: Dividend 70=1
        INTEGER (KIND=2), PARAMETER :: Dividend=2
        INTEGER (KIND=2), PARAMETER :: Interest Payments=2
        INTEGER (KIND=2), PARAMETER :: Interest Earnings=3
        INTEGER (KIND=2), PARAMETER :: Additions=4
        INTEGER (KIND=2), PARAMETER :: Reductions=5
        INTEGER (KIND=2), PARAMETER :: Cash Interest Payments=6
        INTEGER (KIND=2), PARAMETER :: Cash Interest Earnings=7
        INTEGER (KIND=2), PARAMETER :: Cash Dividend Earnings=8
        INTEGER (KIND=2), PARAMETER :: Intra Company Interest Earnings=9
        INTEGER (KIND=2), PARAMETER :: Minibond Interest Payment = 6
        INTEGER (KIND=2), PARAMETER :: Minibond Principal Payment = 7
        INTEGER (KIND=2), PARAMETER :: Minibond Interest Deposit = 8
        INTEGER (KIND=2), PARAMETER :: Minibond Principal Deposit = 9
        INTEGER (KIND=2), PARAMETER :: Annualized Interest = 10
        INTEGER (KIND=2), PARAMETER :: Minibond Annualized Interest = 10
        INTEGER (KIND=2), PARAMETER :: BAAN Additions = 1
        INTEGER (KIND=2), PARAMETER :: BAAN Reductions = 2
        INTEGER (KIND=2), PARAMETER :: BAAN Interest Payments = 3
        INTEGER (KIND=2), PARAMETER :: BAAN Annualized Interest = 4
end module namescom