C     Last change: MSG 4/29/2014 11:22:23 AM
C***********************************************************************
C
C                          F I N A N
C
C        COPYRIGHT (C) 1982  M.S. GERBER & ASSOCIATES, INC.
C                          ALL RIGHTS RESERVED
C
C***********************************************************************
C
      RECURSIVE SUBROUTINE ASSET_FINANCING(ITER,OPERATING_METHOD,
     +                           DIVIDEND_PAYMENT_METHOD,
     +                           EQUITY_DEFINITION,
     +                           FUNDS,
     +                           RETURN_ON_RATEBASE,
     +                           PS_BALANCE_RETIREMENTS,
     +                           DEBT_FILE_PREFERRED_DIVIDENDS,
     +                           COMEQU,STDCUM,CAPITL,INVESTMENTS,
     +                           COMMON_STOCK_BALANCE,
     +                           PS_ROLLOVER_DIVIVEND,
     +                           CLASS_TYPE,
C VARIABLES FOR REGULATED GROUP
     +                           RATE_BASE,PSCUM,LTDCUM,
     +                           PREFERRED_STOCK_RETIREMENTS,
     +                           LONG_TERM_DEBT_RETIREMENTS,
     +                           PS_PREM_ISSUE_EXP_AMORT,
     +                           COMMON_STOCK_DIV_ACCRUALS)
C
      INCLUDE 'SpinLib.MON'
      USE SIZECOM
      INCLUDE 'ACRSLCOM.MON'
      INCLUDE 'FINACCOM.MON'
      SAVE
C
C CONVERGENCE VARIABLES
C
      REAL (KIND=4) :: R_VARIABLE(0:*)
      LOGICAL*1 ORLANDO_UC,IMPA
      REAL*4 R_GOPINC,TEMP_PREFERRED_DIVIDENDS,GRE_TARGET_RATIO
      REAL COM_EQUITY(2),CAPITAL_WITH_STD,TARGET_OPT_REVENUES
      REAL*4 ESLOPE,OPREVP,ONI,OOPINC,PS_ROLLOVER_DIVIVEND,
     +       DENOM,SLOPE,GOPINP,SEC32B_AMT,SEC32A_AMT,
     +       OPERATING_REVENUE_TAX,PS_PREM_ISSUE_EXP_AMORT,
     +       COMMON_STOCK_DIV_ACCRUALS,
     +       NF_DEFERRED_TAXES_CR,AFUDC_BORROWED,
     +       LTD_INTEREST_CASH_PAYMENTS
      INTEGER*2 COVERAGE_RATIO
      CHARACTER*1 OPERATING_METHOD,DIVIDEND_PAYMENT_METHOD,
     +            AFUDC_RETURN_POLICY
      LOGICAL*1 INCOME_CONVERGED,CONVERGE_TEST,EQUITY_TEST,
     +          FUNDS_BALANCE,DIVIDEND_PAYOUT_RATIO,USE_LT_FINANCING,
     +          INCOME_HAS_CONVERGED
      LOGICAL*1 SALT_RIVER_PROJECT
      REAL DENUMRATR,COVERAGE,STD_INTEREST
      REAL COMPARE1,INTEREST_ON_LONG_TERM_DEBT
      REAL DEBT_SERVICE_TARGET,TIER_TARGET,NUMERATOR,DEBT_SERVICE_RATIO
      PARAMETER (ESLOPE=1.03)
C
      REAL PS_BALANCE_RETIREMENTS,DEBT_FILE_PREFERRED_DIVIDENDS
      REAL COMEQU(2),STDCUM(2),CAPITL(2),INVESTMENTS(2)
      REAL FUNDS,PSDIV1,PROJECTED_OPERATING_REVENUES,
     +     PSDIV2,RETURN_ON_RATEBASE,EARNINGS_AVAILABLE_TO_COMMON,
     +     COMMON_STOCK_BALANCE,SAVE_COMMON_PAYOUT_RATIO
      CHARACTER*1 EQUITY_DEFINITION,UTILITY_TYPE
      INTEGER*2 TIMES,ITER
      CHARACTER*1 CLASS_TYPE
      CHARACTER*1 SBU,SUBSIDIARY,PARENT,REGULATED_GROUP
      PARAMETER (SBU='B',SUBSIDIARY='S',PARENT='P',REGULATED_GROUP='R')
      CHARACTER*1 DPS,DPR,CASH
      CHARACTER*1 DPS_CASH,DPR_CASH
      PARAMETER(DPS='S',DPR='R',CASH='C',DPS_CASH='H',DPR_CASH='P')
      CHARACTER*1 DIVIDEND_ONLY,ALL_CASH,DIVIDEND_PLUS_CASH,
     +            DIVIDEND_CASH_POLICY,
     +            DIVIDEND_PLUS_CURRENT_CASH
      PARAMETER(DIVIDEND_ONLY='D',ALL_CASH='A',DIVIDEND_PLUS_CASH='P',
     +          DIVIDEND_PLUS_CURRENT_CASH='C')
      REAL*4 BTL_AMORTIZATION_IN_EXPENSES,R_BTL_AMORTIZATION_IN_EXPENSES
      REAL*4 OTHER_TAXES,R_OTHER_TAXES,CASH_FROM_CURRENT_OPERATIONS
C      REAL*4 DEF_TAXES_AMT_CREDITS
C      SAVE DEF_TAXES_AMT_CREDITS
      REAL*4 RATE_BASE,PSCUM(2),LTDCUM(2),
     +       PREFERRED_STOCK_RETIREMENTS,
     +       LONG_TERM_DEBT_RETIREMENTS,
     +       MTG_DEBT_RETIREMENTS,
     +       CHANGE_IN_CAPITAL
C      REAL*4 CHANGE_IN_COMMON_EQUITY
      REAL*4 AVAILABLE_CASH,DIVIDENDS_CASH,FUNDS_NEEDED
      REAL*4 R_DEF_TAXES_AMT_CREDITS,LTD_PS_DEFERRED_TAX_CR
      REAL (KIND=4) :: RE_ODEC_COVERAGE,INPUT_COVERAGE_RATIO
c      SAVE OOPINC,ONI,CONVERGE_TEST,EQUITY_TEST,
c     +     TIER_TARGET
c      SAVE BTL_AMORTIZATION_IN_EXPENSES,OTHER_TAXES
C
C INITIALIZE VARIABLES
C
      FUNDS_NEEDED = FUNDS
      DIVIDEND_CASH_POLICY = DIVIDEND_ONLY
      DIVIDEND_PAYOUT_RATIO = DIVIDEND_PAYMENT_METHOD == DPR .OR.
     +                        DIVIDEND_PAYMENT_METHOD == DPR_CASH .OR.
     +                        DIVIDEND_PAYMENT_METHOD == CASH
      IF(DIVIDEND_PAYMENT_METHOD == CASH) THEN
         DIVIDEND_CASH_POLICY = ALL_CASH
         SAVE_COMMON_PAYOUT_RATIO = COMMON_PAYOUT_RATIO
         COMMON_PAYOUT_RATIO = 0.
      ENDIF
         
      IF(DIVIDEND_PAYMENT_METHOD == DPR_CASH .OR.
     +                         DIVIDEND_PAYMENT_METHOD == DPS_CASH) THEN
         DIVIDEND_CASH_POLICY = DIVIDEND_PLUS_CASH
      ENDIF
c      DIVIDEND_CASH_POLICY = DIVIDEND_PLUS_CURRENT_CASH
C
C***********************************************************************
C
C               MODEL'S FINANCING EQUATIONS BEGIN HERE
C
C***********************************************************************
C
C IF THE CAPITIAL STRUCTURE IS REGULATED
C
C$ifdefined(reg_capital_structur)
C      ENTRY REGULATED_FINANCING(RATE_BASE,COMEQU,PSCUM,LTDCUM)
C
      IF(CLASS_TYPE == REGULATED_GROUP) THEN
C
C PREFERRED SECTION
C
         PSCUM(2) = RATE_BASE * PSRATO
         PREFERRED_STOCK_ISSUED = MAX(0.,PSCUM(2)-PSCUM(1))
         PREFERRED_STOCK_RETIREMENTS = MAX(0.,PSCUM(1)-PSCUM(2))
         PREFERRED_DIVIDENDS = PSCUM(2) * PSRATE
C
C LTD SECTION 
C
         LTDCUM(2) = RATE_BASE * (1. - (EQRATO + PSRATO))
         LONG_TERM_DEBT_ISSUED = MAX(0.,LTDCUM(2)-LTDCUM(1))
         LONG_TERM_DEBT_RETIREMENTS = MAX(0.,LTDCUM(1)-LTDCUM(2))
         LTD_PS_RETIREMENTS = LONG_TERM_DEBT_RETIREMENTS
     +                        + PREFERRED_STOCK_RETIREMENTS
C
C COMMON SECTION
C
         COMEQU(2) = RATE_BASE * EQRATO
         EARNINGS_AVAILABLE_TO_COMMON = NET_INCOME-PREFERRED_DIVIDENDS -
     +                                  PS_PREM_ISSUE_EXP_AMORT         
         COMMON_DIVIDENDS = COMMON_PAYOUT_RATIO *
     +                                      EARNINGS_AVAILABLE_TO_COMMON
         RETAINED_EARNINGS = EARNINGS_AVAILABLE_TO_COMMON -
     +                                                  COMMON_DIVIDENDS
         COMMON_STOCK_ISSUED = COMEQU(2) - COMEQU(1) - RETAINED_EARNINGS
         CHANGE_IN_CAPITAL = (PREFERRED_STOCK_ISSUED +
     +                        COMMON_STOCK_ISSUED +
     +                        LONG_TERM_DEBT_ISSUED) -
     +                        LTD_PS_RETIREMENTS - 
     +                        FUNDS +
     +                        RETAINED_EARNINGS
         INVESTMENTS(2) = INVESTMENTS(1) + CHANGE_IN_CAPITAL
         IF(INVESTMENTS(2) < 0.) THEN
            STDISS = ABS(INVESTMENTS(2))
            INVESTMENTS(2) = 0.
         ELSE
            STDISS = 0.
         ENDIF
         STDCUM(2) = STDCUM(1) + STDISS
         RETURN   
      ENDIF
C$endif
C
C IF THE UTILITY IS NOT AN IOU SKIP THE PS AND CS FINANCING LOGIC
C
      IF(UTILITY_TYPE() /= 'I' .AND. .NOT. ORLANDO_UC()) THEN
C
C THE ADJUSTMENT TO RETAINED_EARNINGS WAS ADDED 5/2/89 FOR SRP
C LARRY KEYS
C
C        RETAINED_EARNINGS = NET_INCOME     PRE ADJUSTMENT
C        COMMON_DIVIDENDS = 0.
         IF(OPERATING_METHOD == 'E') THEN
            NET_INCOME = COMEQU(2) * ROEQU + REA_MARGIN_ADDENDUM
         ENDIF
         COMMON_DIVIDENDS = NET_INCOME*COMMON_PAYOUT_RATIO +
     +                                                DIVIDEND_PER_SHARE
         RETAINED_EARNINGS = NET_INCOME - COMMON_DIVIDENDS
         IF(EQRATO > .9999 .AND. CLASS_TYPE == SUBSIDIARY .AND. 
     +                                        SALT_RIVER_PROJECT()) THEN
            COMMON_STOCK_ISSUED = MAX(FUNDS - RETAINED_EARNINGS,0.)
         ELSE
            COMMON_STOCK_ISSUED = 0.
         ENDIF
         COMMON_STOCK_ISSUED = MAX(CSMIN,COMMON_STOCK_ISSUED)
         PREFERRED_STOCK_ISSUED = 0.
         IF(STDCUM(1)+FUNDS-COMMON_STOCK_ISSUED-RETAINED_EARNINGS <=
     +                                                     STD_MAX) THEN
            LONG_TERM_DEBT_ISSUED = MAX(LTDMIN,DEBT_FILE_LTD_ISSUED)
         ELSE
C 
C HIGH REQUIREMENT FOR FUNDS
C 
            STDCUM(2) = STDMIN
            STDISS = STDCUM(2) - STDCUM(1)
C
C ISSUE LONG TERM DEBT
C
            LONG_TERM_DEBT_ISSUED = FUNDS - RETAINED_EARNINGS - STDISS -
     +                                               COMMON_STOCK_ISSUED
            IF(LDUNIT /= 0.) LONG_TERM_DEBT_ISSUED = LDUNIT *
     +                         AINT(0.0001+LONG_TERM_DEBT_ISSUED/LDUNIT)
            IF(LONG_TERM_DEBT_ISSUED>LTDMAX)LONG_TERM_DEBT_ISSUED=LTDMAX
            IF(LONG_TERM_DEBT_ISSUED<LTDMIN)LONG_TERM_DEBT_ISSUED=LTDMIN
            IF(LONG_TERM_DEBT_ISSUED<DEBT_FILE_LTD_ISSUED)
     +                      LONG_TERM_DEBT_ISSUED = DEBT_FILE_LTD_ISSUED
         ENDIF
         COMEQU(2) = COMEQU(1) + RETAINED_EARNINGS + COMMON_STOCK_ISSUED
         STDISS = FUNDS - LONG_TERM_DEBT_ISSUED - RETAINED_EARNINGS -
     +                                               COMMON_STOCK_ISSUED
         STDCUM(2) = STDCUM(1) + STDISS
         INVESTMENTS(2) = MIN_ST_INVESTMENTS
         IF(STDCUM(2) <= STDMIN) THEN
            INVESTMENTS(2) = MIN_ST_INVESTMENTS + STDMIN - STDCUM(2)
            STDISS = STDMIN - STDCUM(1)
            STDCUM(2) = STDMIN
         ENDIF
         RETURN
      ENDIF
C
C FINANCING FOR IOU UTILITIES
C
C 
C LOW REQUIREMENT FOR FUNDS MINIMUM AMOUNTS OF DEBT ISSUED
C
      USE_LT_FINANCING = .TRUE. 
      IF((STDCUM(1) + FUNDS - RETAINED_EARNINGS) <= STD_MAX) THEN
C
C COMMON TO ALL INCOME DRIVER METHODS
C
         IF(CSMIN < 0. .AND. CSMAX < 0.) THEN
            COMMON_STOCK_ISSUED  = CSMAX
         ELSE
            COMMON_STOCK_ISSUED  = MAX(CSMIN,0.)
         ENDIF
         PREFERRED_STOCK_ISSUED = PSMIN
         LONG_TERM_DEBT_ISSUED = LTDMIN
         STDISS = 0.
         IF(PREFERRED_STOCK_ISSUED < DEBT_FILE_PREFERRED_ISSUED)
     +              PREFERRED_STOCK_ISSUED = DEBT_FILE_PREFERRED_ISSUED
         IF(LONG_TERM_DEBT_ISSUED < DEBT_FILE_LTD_ISSUED)
     +                      LONG_TERM_DEBT_ISSUED = DEBT_FILE_LTD_ISSUED
C        PREFERRED_DIVIDENDS = DEBT_FILE_PREFERRED_DIVIDENDS + PSRATE *
C    +            (PREFERRED_STOCK_ISSUED-DEBT_FILE_PREFERRED_ISSUED)/2.
         TEMP_PREFERRED_DIVIDENDS = DEBT_FILE_PREFERRED_DIVIDENDS
     +            + PSRATE/2.*
     +               (PREFERRED_STOCK_ISSUED-DEBT_FILE_PREFERRED_ISSUED)
     +            + PS_ROLLOVER_DIVIVEND 
     +            + PS_PREM_ISSUE_EXP_AMORT
C 
C RETURN ON EQUITY WAS SPECIFIED FOR INCOME BASE
C
       IF(CLASS_TYPE == SBU) THEN  ! PASS THE CASH
         IF(EQRATO > .9999) THEN
            COMEQU(2) = COMEQU(1) + FUNDS
         ELSE
            COMEQU(2) = EQRATO*(FUNDS + CAPITL(1) - LTD_PS_RETIREMENTS)
         ENDIF
         IF(OPERATING_METHOD == 'E') THEN
            IF(DIVIDEND_PAYOUT_RATIO) THEN
               COMMON_STOCK_ISSUED = (COMEQU(2) - COMEQU(1)) -
     +             RETURN_ON_RATEBASE * 
     +                 (1.-COMMON_PAYOUT_RATIO)*(COMEQU(2)+COMEQU(1))/2.
            ELSE
               COMMON_STOCK_ISSUED=COMEQU(2)*(1.-RETURN_ON_RATEBASE/2.)-
     +                            COMEQU(1)*(1.+RETURN_ON_RATEBASE/2.) +
     +                                                  COMMON_DIVIDENDS
            ENDIF
            IF(CSUNIT /= 0.) COMMON_STOCK_ISSUED = CSUNIT *
     +                           AINT(0.0001+COMMON_STOCK_ISSUED/CSUNIT)
            IF(CSMAX > 0.) THEN
               IF(COMMON_STOCK_ISSUED > CSMAX) COMMON_STOCK_ISSUED=CSMAX
               IF(CSMIN > 0. .AND. COMMON_STOCK_ISSUED < CSMIN)
     +                                       COMMON_STOCK_ISSUED = CSMIN
               IF(COMMON_STOCK_ISSUED < 0.) THEN
                  COMMON_STOCK_ISSUED =
     +                MAX(COMMON_STOCK_ISSUED,-ABS(MAX_COMMON_BUY_BACK))
                  IF(COMMON_STOCK_BALANCE+COMMON_STOCK_ISSUED <
     +                                MINIMUM_COMMON_STOCK_BALANCE) THEN
                     COMMON_STOCK_ISSUED = MINIMUM_COMMON_STOCK_BALANCE-
     +                                              COMMON_STOCK_BALANCE
                  ENDIF
               ENDIF
            ELSE
               COMMON_STOCK_ISSUED = CSMAX
            ENDIF
            IF(DIVIDEND_PAYOUT_RATIO) THEN
               IF(EQUITY_DEFINITION == 'E') THEN
                  COMEQU(2) = (COMMON_STOCK_ISSUED + COMEQU(1))/
     +                  (1.-RETURN_ON_RATEBASE*(1.-COMMON_PAYOUT_RATIO))
                  EARNINGS_AVAILABLE_TO_COMMON = RETURN_ON_RATEBASE *
     +                                                         COMEQU(2)
               ELSE
                  COMEQU(2) = (COMMON_STOCK_ISSUED + COMEQU(1) *
     +             (1.+RETURN_ON_RATEBASE*(1.-COMMON_PAYOUT_RATIO)/2.))/
     +               (1.-RETURN_ON_RATEBASE*(1.-COMMON_PAYOUT_RATIO)/2.)
                  EARNINGS_AVAILABLE_TO_COMMON = RETURN_ON_RATEBASE *
     +                                        (COMEQU(2) + COMEQU(1))/2.
               ENDIF
               RETAINED_EARNINGS = (1.-COMMON_PAYOUT_RATIO) *
     +                                      EARNINGS_AVAILABLE_TO_COMMON
               COMMON_DIVIDENDS = EARNINGS_AVAILABLE_TO_COMMON -
     +                                                 RETAINED_EARNINGS
            ELSE
               IF(EQUITY_DEFINITION == 'E') THEN
                  COMEQU(2) = (COMMON_STOCK_ISSUED + COMEQU(1) -
     +                         COMMON_DIVIDENDS)/(1.-RETURN_ON_RATEBASE)
                  EARNINGS_AVAILABLE_TO_COMMON = RETURN_ON_RATEBASE *
     +                                                         COMEQU(2)
               ELSE
                  COMEQU(2) = (COMMON_STOCK_ISSUED + (COMEQU(1) *
     +                  (1.+RETURN_ON_RATEBASE/2.)-
     +                     COMMON_DIVIDENDS))/(1.-RETURN_ON_RATEBASE/2.)
                  EARNINGS_AVAILABLE_TO_COMMON = RETURN_ON_RATEBASE *
     +                                        (COMEQU(2) + COMEQU(1))/2.
               ENDIF
               RETAINED_EARNINGS = EARNINGS_AVAILABLE_TO_COMMON -
     +                                                  COMMON_DIVIDENDS
            ENDIF
            NET_INCOME = RETAINED_EARNINGS + COMMON_DIVIDENDS +
     +                                          TEMP_PREFERRED_DIVIDENDS
         ELSE ! RETURN ON RATE BASE 
            IF(DIVIDEND_PAYOUT_RATIO) THEN
               IF(NET_INCOME - TEMP_PREFERRED_DIVIDENDS > 0.) THEN
                  RETAINED_EARNINGS = (1.-COMMON_PAYOUT_RATIO) *
     +                             (NET_INCOME-TEMP_PREFERRED_DIVIDENDS)
                  COMMON_DIVIDENDS  = NET_INCOME -
     +                                    TEMP_PREFERRED_DIVIDENDS -
     +                                                 RETAINED_EARNINGS
               ELSE
                  RETAINED_EARNINGS=NET_INCOME-TEMP_PREFERRED_DIVIDENDS
                  COMMON_DIVIDENDS  = 0.
               ENDIF
            ELSE ! DIVIDEND_PAYMENT_METHOD == DPS
               RETAINED_EARNINGS = NET_INCOME-TEMP_PREFERRED_DIVIDENDS -
     +                                                  COMMON_DIVIDENDS
            ENDIF
            COMMON_STOCK_ISSUED = COMEQU(2) - COMEQU(1) -
     +                                                 RETAINED_EARNINGS
            IF(CSUNIT /= 0.0) COMMON_STOCK_ISSUED = CSUNIT *
     +                         AINT(0.0001 + COMMON_STOCK_ISSUED/CSUNIT)
            IF(CSMAX > 0.) THEN
               IF(COMMON_STOCK_ISSUED > CSMAX) COMMON_STOCK_ISSUED=CSMAX
               IF(CSMIN > 0. .AND. COMMON_STOCK_ISSUED < CSMIN)
     +                                       COMMON_STOCK_ISSUED = CSMIN
               IF(COMMON_STOCK_ISSUED < 0.) THEN
                  COMMON_STOCK_ISSUED =
     +                MAX(COMMON_STOCK_ISSUED,-ABS(MAX_COMMON_BUY_BACK))
                  IF(COMMON_STOCK_BALANCE+COMMON_STOCK_ISSUED <
     +                                MINIMUM_COMMON_STOCK_BALANCE) THEN
                     COMMON_STOCK_ISSUED = MINIMUM_COMMON_STOCK_BALANCE-
     +                                              COMMON_STOCK_BALANCE
                  ENDIF
               ENDIF
            ELSE
               COMMON_STOCK_ISSUED = CSMAX
            ENDIF
            COMEQU(2) = COMEQU(1)+COMMON_STOCK_ISSUED+RETAINED_EARNINGS
         ENDIF
        ELSE  !SUB OR PARENT NEED DIVIDENDS AND RETAINED EARNINGS
         IF(OPERATING_METHOD == 'E') THEN
            IF(DIVIDEND_PAYOUT_RATIO) THEN
               IF(EQUITY_DEFINITION == 'E') THEN
                  COMEQU(2) = (COMMON_STOCK_ISSUED + COMEQU(1))/
     +                  (1.-RETURN_ON_RATEBASE*(1.-COMMON_PAYOUT_RATIO))
                  EARNINGS_AVAILABLE_TO_COMMON = RETURN_ON_RATEBASE *
     +                                                         COMEQU(2)
               ELSE
                  COMEQU(2) = (COMMON_STOCK_ISSUED + COMEQU(1) *
     +             (1.+RETURN_ON_RATEBASE*(1.-COMMON_PAYOUT_RATIO)/2.))/
     +               (1.-RETURN_ON_RATEBASE*(1.-COMMON_PAYOUT_RATIO)/2.)
                  EARNINGS_AVAILABLE_TO_COMMON = RETURN_ON_RATEBASE *
     +                                        (COMEQU(2) + COMEQU(1))/2.
               ENDIF
               RETAINED_EARNINGS = (1.-COMMON_PAYOUT_RATIO) *
     +                                      EARNINGS_AVAILABLE_TO_COMMON
               COMMON_DIVIDENDS = EARNINGS_AVAILABLE_TO_COMMON -
     +                                                 RETAINED_EARNINGS
            ELSE
               IF(EQUITY_DEFINITION == 'E') THEN
                  COMEQU(2) = (COMMON_STOCK_ISSUED + COMEQU(1) -
     +                         COMMON_DIVIDENDS)/(1.-RETURN_ON_RATEBASE)
                  EARNINGS_AVAILABLE_TO_COMMON = RETURN_ON_RATEBASE *
     +                                                         COMEQU(2)
               ELSE
                  COMEQU(2) = (COMMON_STOCK_ISSUED + (COMEQU(1) *
     +                  (1.+RETURN_ON_RATEBASE/2.)-
     +                     COMMON_DIVIDENDS))/(1.-RETURN_ON_RATEBASE/2.)
                  EARNINGS_AVAILABLE_TO_COMMON = RETURN_ON_RATEBASE *
     +                                        (COMEQU(2) + COMEQU(1))/2.
               ENDIF
               RETAINED_EARNINGS = EARNINGS_AVAILABLE_TO_COMMON -
     +                                                  COMMON_DIVIDENDS
            ENDIF
            NET_INCOME = RETAINED_EARNINGS + COMMON_DIVIDENDS +
     +                                          TEMP_PREFERRED_DIVIDENDS
         ELSE ! RETURN ON RATE BASE 
            IF(DIVIDEND_PAYOUT_RATIO) THEN
               IF(NET_INCOME - TEMP_PREFERRED_DIVIDENDS > 0.) THEN
                  RETAINED_EARNINGS = (1.-COMMON_PAYOUT_RATIO) *
     +                             (NET_INCOME-TEMP_PREFERRED_DIVIDENDS)
                  COMMON_DIVIDENDS  = NET_INCOME -
     +                                    TEMP_PREFERRED_DIVIDENDS -
     +                                                 RETAINED_EARNINGS
               ELSE
                  RETAINED_EARNINGS=NET_INCOME-TEMP_PREFERRED_DIVIDENDS
                  COMMON_DIVIDENDS  = 0.
               ENDIF
            ELSE ! DIVIDEND_PAYMENT_METHOD == DPS
               RETAINED_EARNINGS = NET_INCOME-TEMP_PREFERRED_DIVIDENDS -
     +                                                  COMMON_DIVIDENDS
            ENDIF
            COMEQU(2) = COMEQU(1)+COMMON_STOCK_ISSUED+RETAINED_EARNINGS
         ENDIF
        ENDIF ! CLASS_TYPE == SBU OR REG
         STDISS = STDISS + (FUNDS - PREFERRED_STOCK_ISSUED -
     +                   COMMON_STOCK_ISSUED - LONG_TERM_DEBT_ISSUED -
     +                                                RETAINED_EARNINGS)
         STDCUM(2) = STDCUM(1) + STDISS
         USE_LT_FINANCING = STDCUM(2) > STD_MAX
         IF(.NOT. USE_LT_FINANCING) THEN
            PREFERRED_DIVIDENDS = TEMP_PREFERRED_DIVIDENDS -
     +                            PS_PREM_ISSUE_EXP_AMORT
         ENDIF
      ENDIF ! SHORT TERM FINANCING
C
C HIGH REQUIREMENT FOR FUNDS
C
      IF(USE_LT_FINANCING) THEN
         IF(MONTHLY_MIDAS_ACTIVE) THEN
            STDCUM(2) = STDMIN
            STDISS = STDCUM(2) - STDCUM(1)
         ELSE
            STDCUM(2) = STDMIN
            STDISS = STDCUM(2) - STDCUM(1)
         ENDIF
         FUNDS = FUNDS - STDISS
         IF(EQRATO > .9999) THEN
            COMEQU(2) = COMEQU(1) + FUNDS
         ELSE
            COMEQU(2) = EQRATO*(FUNDS + CAPITL(1) - LTD_PS_RETIREMENTS)
         ENDIF
         IF(OPERATING_METHOD == 'E') THEN
C
C ISSUE COMMON EQUITY
C
            IF(DIVIDEND_PAYOUT_RATIO) THEN
               COMMON_STOCK_ISSUED = (COMEQU(2) - COMEQU(1)) -
     +             RETURN_ON_RATEBASE * 
     +                 (1.-COMMON_PAYOUT_RATIO)*(COMEQU(2)+COMEQU(1))/2.
            ELSE
               COMMON_STOCK_ISSUED=COMEQU(2)*(1.-RETURN_ON_RATEBASE/2.)-
     +                            COMEQU(1)*(1.+RETURN_ON_RATEBASE/2.) +
     +                                                  COMMON_DIVIDENDS
            ENDIF
            IF(CSUNIT /= 0.) COMMON_STOCK_ISSUED = CSUNIT *
     +                           AINT(0.0001+COMMON_STOCK_ISSUED/CSUNIT)
            IF(CSMAX > 0.) THEN
               IF(COMMON_STOCK_ISSUED > CSMAX) COMMON_STOCK_ISSUED=CSMAX
               IF(CSMIN > 0. .AND. COMMON_STOCK_ISSUED < CSMIN)
     +                                       COMMON_STOCK_ISSUED = CSMIN
               IF(COMMON_STOCK_ISSUED < 0.) THEN
                  COMMON_STOCK_ISSUED =
     +                MAX(COMMON_STOCK_ISSUED,-ABS(MAX_COMMON_BUY_BACK))
                  IF(COMMON_STOCK_BALANCE+COMMON_STOCK_ISSUED <
     +                                MINIMUM_COMMON_STOCK_BALANCE) THEN
                     COMMON_STOCK_ISSUED = MINIMUM_COMMON_STOCK_BALANCE-
     +                                              COMMON_STOCK_BALANCE
                  ENDIF
               ENDIF
            ELSE
               COMMON_STOCK_ISSUED = CSMAX
            ENDIF
            IF(DIVIDEND_PAYOUT_RATIO) THEN
               COMEQU(2) = (COMMON_STOCK_ISSUED + COMEQU(1) *
     +             (1.+RETURN_ON_RATEBASE*(1.-COMMON_PAYOUT_RATIO)/2.))/
     +               (1.-RETURN_ON_RATEBASE*(1.-COMMON_PAYOUT_RATIO)/2.)
               RETAINED_EARNINGS = RETURN_ON_RATEBASE *
     +               (1.-COMMON_PAYOUT_RATIO)*(COMEQU(2) + COMEQU(1))/2.
               COMMON_DIVIDENDS=RETURN_ON_RATEBASE*COMMON_PAYOUT_RATIO *
     +                                        (COMEQU(2) + COMEQU(1))/2.
            ELSE
               COMEQU(2) = (COMMON_STOCK_ISSUED + (COMEQU(1) *
     +                  (1.+RETURN_ON_RATEBASE/2.) -
     +                     COMMON_DIVIDENDS))/(1.-RETURN_ON_RATEBASE/2.)
               RETAINED_EARNINGS = RETURN_ON_RATEBASE *
     +                         (COMEQU(2)+COMEQU(1))/2.-COMMON_DIVIDENDS
            ENDIF
C
C ISSUE PREFERRED STOCK
C
            PREFERRED_STOCK_ISSUED = (PSRATO+EQRATO) * (FUNDS+CAPITL(1)-
     +                LTD_PS_RETIREMENTS) - COMEQU(2) -
     +                                            PS_BALANCE_RETIREMENTS
            IF(PSUNIT /= 0.) PREFERRED_STOCK_ISSUED = PSUNIT *
     +                      AINT(0.0001 + PREFERRED_STOCK_ISSUED/PSUNIT)
            IF(PREFERRED_STOCK_ISSUED > PSMAX)
     +                                    PREFERRED_STOCK_ISSUED = PSMAX
            IF(PREFERRED_STOCK_ISSUED < PSMIN)
     +                                    PREFERRED_STOCK_ISSUED = PSMIN
            IF(PREFERRED_STOCK_ISSUED < DEBT_FILE_PREFERRED_ISSUED)
     +               PREFERRED_STOCK_ISSUED = DEBT_FILE_PREFERRED_ISSUED
C           PREFERRED_DIVIDENDS = DEBT_FILE_PREFERRED_DIVIDENDS+PSRATE *
C    +            (PREFERRED_STOCK_ISSUED-DEBT_FILE_PREFERRED_ISSUED)/2.
            PREFERRED_DIVIDENDS = DEBT_FILE_PREFERRED_DIVIDENDS+ PSRATE*
     +           (PREFERRED_STOCK_ISSUED-DEBT_FILE_PREFERRED_ISSUED)/2.+
     +                                              PS_ROLLOVER_DIVIVEND
C
C ISSUE LONG TERM DEBT
C
            LONG_TERM_DEBT_ISSUED = FUNDS - PREFERRED_STOCK_ISSUED - 
     +                           COMMON_STOCK_ISSUED - RETAINED_EARNINGS
            IF(LDUNIT /= 0.) LONG_TERM_DEBT_ISSUED = LDUNIT *
     +                     AINT(0.0001 + LONG_TERM_DEBT_ISSUED / LDUNIT)
            IF(LONG_TERM_DEBT_ISSUED>LTDMAX)LONG_TERM_DEBT_ISSUED=LTDMAX
            IF(LONG_TERM_DEBT_ISSUED<LTDMIN)LONG_TERM_DEBT_ISSUED=LTDMIN
            IF(LONG_TERM_DEBT_ISSUED<DEBT_FILE_LTD_ISSUED)
     +                     LONG_TERM_DEBT_ISSUED = DEBT_FILE_LTD_ISSUED
            NET_INCOME = RETAINED_EARNINGS + COMMON_DIVIDENDS +
     +                   PREFERRED_DIVIDENDS + PS_PREM_ISSUE_EXP_AMORT
         ELSE
C***********************************************************************
C
C            END OF FINANCING IF ROE OPERATION MODE
C
C***********************************************************************
C
C***********************************************************************
C
C         NET PLANT VALUE, OPERATING INCOME, PRICE, OR COVERAGE RATIO 
C         WAS SPECIFIED FOR INCOME BASE
C
C***********************************************************************
C
C     ISSUED COMMON AND PREFERRED STOCK
C
            TIMES = 0
            PSDIV1 = 0.
            IF(ITER == 1) THEN
               PSDIV2 = DEBT_FILE_PREFERRED_DIVIDENDS +
     +                                              PS_ROLLOVER_DIVIVEND
     +                  + PS_PREM_ISSUE_EXP_AMORT
            ELSE
               PSDIV2 = PREFERRED_DIVIDENDS
     +                  + PS_PREM_ISSUE_EXP_AMORT
            ENDIF
C
C     CODE MODIFICATION TO REMOVE THE PROBLEM OF THE COMEQU
C     VALUE CHANGING BUT NOT THE VALUE OF EQRATO * FUNDS ETC.
C
            DO
               PREFERRED_STOCK_ISSUED = (EQRATO + PSRATO) *
     +            (FUNDS + CAPITL(1) - LTD_PS_RETIREMENTS) - COMEQU(2) -
     +                                            PS_BALANCE_RETIREMENTS
               PREFERRED_STOCK_ISSUED = PSRATO * (FUNDS + CAPITL(1) -
     +                      LTD_PS_RETIREMENTS) - PS_BALANCE_RETIREMENTS
               IF(PSUNIT /= 0.0) PREFERRED_STOCK_ISSUED = PSUNIT *
     +                      AINT(0.0001+PREFERRED_STOCK_ISSUED / PSUNIT)
               IF(PREFERRED_STOCK_ISSUED > PSMAX)
     +                                    PREFERRED_STOCK_ISSUED = PSMAX
               IF(PREFERRED_STOCK_ISSUED < PSMIN)
     +                                    PREFERRED_STOCK_ISSUED = PSMIN
               IF(PREFERRED_STOCK_ISSUED < DEBT_FILE_PREFERRED_ISSUED)
     +               PREFERRED_STOCK_ISSUED = DEBT_FILE_PREFERRED_ISSUED
               IF(ITER == 1) PSDIV2 = DEBT_FILE_PREFERRED_DIVIDENDS +
     +                                PSRATE * (PREFERRED_STOCK_ISSUED -
     +                                    DEBT_FILE_PREFERRED_ISSUED)/2.
     +                                + PS_PREM_ISSUE_EXP_AMORT
               IF(TIMES > 20) EXIT
               IF(TIMES == 0) THEN
                  IF(DIVIDEND_PAYOUT_RATIO) THEN
                     IF(NET_INCOME - PSDIV2 > 0.) THEN
                        COMMON_STOCK_ISSUED = COMEQU(2) - COMEQU(1) -
     +                      (1.-COMMON_PAYOUT_RATIO)*(NET_INCOME-PSDIV2)
                        COMMON_DIVIDENDS = COMMON_PAYOUT_RATIO *
     +                                               (NET_INCOME-PSDIV2)
                     ELSE
                        COMMON_STOCK_ISSUED = COMEQU(2)-COMEQU(1)-
     +                                               (NET_INCOME-PSDIV2)
                        COMMON_DIVIDENDS = 0.
                     ENDIF
                  ELSE 
                     COMMON_STOCK_ISSUED = COMEQU(2) - COMEQU(1) -
     +                          (NET_INCOME - PSDIV2 - COMMON_DIVIDENDS)
                  ENDIF
                  IF(CSUNIT /= 0.0) COMMON_STOCK_ISSUED = CSUNIT *
     +                        AINT(0.0001 + COMMON_STOCK_ISSUED/CSUNIT)
                  IF(CSMAX > 0.) THEN
                     IF(COMMON_STOCK_ISSUED > CSMAX)
     +                                       COMMON_STOCK_ISSUED = CSMAX
                     IF(CSMIN > 0. .AND. COMMON_STOCK_ISSUED < CSMIN)
     +                                       COMMON_STOCK_ISSUED = CSMIN
                     IF(COMMON_STOCK_ISSUED < 0.) THEN
                        COMMON_STOCK_ISSUED =  MAX(COMMON_STOCK_ISSUED,
     +                                        -ABS(MAX_COMMON_BUY_BACK))
                        IF(COMMON_STOCK_BALANCE+COMMON_STOCK_ISSUED <
     +                                MINIMUM_COMMON_STOCK_BALANCE) THEN
                           COMMON_STOCK_ISSUED =
     +                                    MINIMUM_COMMON_STOCK_BALANCE -
     +                                              COMMON_STOCK_BALANCE
                        ENDIF
                     ENDIF
                  ELSE
                     COMMON_STOCK_ISSUED = CSMAX
                  ENDIF
               ENDIF
               COMEQU(2) = COMEQU(1) + COMMON_STOCK_ISSUED +
     +                            NET_INCOME - PSDIV2 - COMMON_DIVIDENDS
               TIMES = TIMES + 1 
               IF(ABS(PSDIV1 - PSDIV2) <= 0.01 * ABS(PSDIV1)) EXIT
               PSDIV1 = PSDIV2
            ENDDO 
            PREFERRED_DIVIDENDS = DEBT_FILE_PREFERRED_DIVIDENDS+PSRATE *
     +           (PREFERRED_STOCK_ISSUED-DEBT_FILE_PREFERRED_ISSUED)/2.+
     +                                              PS_ROLLOVER_DIVIVEND
            RETAINED_EARNINGS = NET_INCOME - PREFERRED_DIVIDENDS -
     +                                                  COMMON_DIVIDENDS
     +                          - PS_PREM_ISSUE_EXP_AMORT
C
C ISSUE LONG TERM DEBT
C
            LONG_TERM_DEBT_ISSUED = FUNDS - PREFERRED_STOCK_ISSUED -
     +                           COMMON_STOCK_ISSUED - RETAINED_EARNINGS
            IF(LDUNIT > 0.001) THEN
               LONG_TERM_DEBT_ISSUED =
     +                  LDUNIT*AINT(0.0001+LONG_TERM_DEBT_ISSUED/LDUNIT)
            ENDIF
            IF(LONG_TERM_DEBT_ISSUED > LTDMAX)
     +                                    LONG_TERM_DEBT_ISSUED = LTDMAX
            IF(LONG_TERM_DEBT_ISSUED < LTDMIN)
     +                                    LONG_TERM_DEBT_ISSUED = LTDMIN
            IF(LONG_TERM_DEBT_ISSUED < DEBT_FILE_LTD_ISSUED)
     +                      LONG_TERM_DEBT_ISSUED = DEBT_FILE_LTD_ISSUED
         ENDIF
C         STDISS = STDISS + (FUNDS - PREFERRED_STOCK_ISSUED -
C     +                   COMMON_STOCK_ISSUED - LONG_TERM_DEBT_ISSUED -
C     +                                                RETAINED_EARNINGS)
C         STDCUM(2) = STDCUM(1) + STDISS
      ENDIF
C***********************************************************************
C
C            END OF FINANCING FOR NPV OPERATING MODE
C
C***********************************************************************
C
C***********************************************************************
C 
C     ALL FINANCING BRANCHES CONVERGE HERE;
C     ISSUE DEBT AND COMPUTE RETAINED EARNINGS
C 
C***********************************************************************
      CASH_FROM_CURRENT_OPERATIONS = FUNDS_NEEDED 
     +                               - LONG_TERM_DEBT_ISSUED 
     +                               - PREFERRED_STOCK_ISSUED
     +                               - RETAINED_EARNINGS
     +                               - COMMON_STOCK_ISSUED
     +                               - STDISS
      IF(CASH_FROM_CURRENT_OPERATIONS >= 0.) THEN
         STDISS = STDISS + CASH_FROM_CURRENT_OPERATIONS 
      ELSE
         STDISS = FUNDS_NEEDED 
     +            - LONG_TERM_DEBT_ISSUED 
     +            - PREFERRED_STOCK_ISSUED
     +            - RETAINED_EARNINGS
     +            - COMMON_STOCK_ISSUED
      ENDIF
      STDCUM(2) = STDCUM(1) + STDISS
      INVESTMENTS(2) = MIN_ST_INVESTMENTS
      IF(STDCUM(2) < STDMIN) THEN
         INVESTMENTS(2) = MIN_ST_INVESTMENTS + STDMIN - STDCUM(2)
C
         STDISS = STDMIN - STDCUM(1)
         STDCUM(2) = STDMIN
      ENDIF
C
C IF A SUBSIDIARY NEED TO PASS ALL THE CASH UP THE STACK SO THAT
C FINANCING IS MINIMUMIZED 1/10/98 COPYRIGHT (C) 1997
C THE FOLLOWING CODE WAS COPIED FROM ST-FIN WHICH IS COPYRIGHTED BY
C M.S. GERBER & ASSOCAITES, INC.
C
      CASH_FROM_CURRENT_OPERATIONS = FUNDS_NEEDED 
     +                               - LONG_TERM_DEBT_ISSUED 
     +                               - PREFERRED_STOCK_ISSUED
     +                               - RETAINED_EARNINGS
     +                               - COMMON_STOCK_ISSUED
     +                               - STDISS

      IF((CLASS_TYPE == SUBSIDIARY .OR. CLASS_TYPE == PARENT) .AND.
     +                            DIVIDEND_CASH_POLICY == ALL_CASH) THEN  ! PASS THE CASH
         EARNINGS_AVAILABLE_TO_COMMON = NET_INCOME -PREFERRED_DIVIDENDS-
     +                                  PS_PREM_ISSUE_EXP_AMORT ! COMMON_DIVIDENDS + RETAINED_EARNINGS
         AVAILABLE_CASH = 0.
         IF(INVESTMENTS(2) >
     +                MIN_ST_INVESTMENTS+COMMON_STOCK_DIV_ACCRUALS) THEN
            AVAILABLE_CASH = AVAILABLE_CASH
     +                       + INVESTMENTS(2) 
     +                       - MIN_ST_INVESTMENTS
     +                       - COMMON_STOCK_DIV_ACCRUALS
            INVESTMENTS(2) = MIN_ST_INVESTMENTS
         ENDIF
         IF(COMMON_STOCK_ISSUED > 0. .AND.
     +                                 COMMON_STOCK_ISSUED > CSMIN) THEN
            DIVIDENDS_CASH = MAX(COMMON_DIVIDENDS+AVAILABLE_CASH,
     +                                                   AVAILABLE_CASH)
            IF(COMMON_STOCK_ISSUED <= DIVIDENDS_CASH) THEN
               COMMON_DIVIDENDS = DIVIDENDS_CASH - COMMON_STOCK_ISSUED
               COMMON_STOCK_ISSUED = 0.
            ELSE
               COMMON_STOCK_ISSUED = COMMON_STOCK_ISSUED-DIVIDENDS_CASH
               COMMON_DIVIDENDS = 0.
            ENDIF
         ELSE ! PASS CASH BALANCE
            COMMON_DIVIDENDS = COMMON_DIVIDENDS + AVAILABLE_CASH
         ENDIF
         RETAINED_EARNINGS = EARNINGS_AVAILABLE_TO_COMMON -
     +                                                  COMMON_DIVIDENDS
         COMMON_PAYOUT_RATIO = SAVE_COMMON_PAYOUT_RATIO
      ELSEIF((CLASS_TYPE == SUBSIDIARY .OR. CLASS_TYPE == PARENT) .AND. 
     +          DIVIDEND_CASH_POLICY == DIVIDEND_PLUS_CASH .AND.
     +                         INVESTMENTS(2) > MIN_ST_INVESTMENTS) THEN
         COMMON_DIVIDENDS = COMMON_DIVIDENDS
     +                      + INVESTMENTS(2)
     +                      - MIN_ST_INVESTMENTS
         INVESTMENTS(2) = MIN_ST_INVESTMENTS
         RETAINED_EARNINGS = NET_INCOME
     +                       - PREFERRED_DIVIDENDS 
     +                       - PS_PREM_ISSUE_EXP_AMORT
     +                       - COMMON_DIVIDENDS
      ELSEIF((CLASS_TYPE == SUBSIDIARY .OR. CLASS_TYPE == PARENT) .AND. 
     +          DIVIDEND_CASH_POLICY == DIVIDEND_PLUS_CURRENT_CASH .AND.
     +                           CASH_FROM_CURRENT_OPERATIONS > 0.) THEN
         COMMON_DIVIDENDS = COMMON_DIVIDENDS
     +                      + CASH_FROM_CURRENT_OPERATIONS
         AVAILABLE_CASH = INVESTMENTS(2)
     +                    - INVESTMENTS(1)
     +                    - COMMON_STOCK_DIV_ACCRUALS
         INVESTMENTS(2) = INVESTMENTS(1)
         RETAINED_EARNINGS = NET_INCOME
     +                       - PREFERRED_DIVIDENDS 
     +                       - PS_PREM_ISSUE_EXP_AMORT
     +                       - COMMON_DIVIDENDS
      ENDIF                             
      COMEQU(2) = COMEQU(1) + COMMON_STOCK_ISSUED +
     +                            NET_INCOME - PSDIV2 - COMMON_DIVIDENDS
      LTDCUM(2) = LTDCUM(1) 
     +            - LONG_TERM_DEBT_RETIREMENTS
     +            + LONG_TERM_DEBT_ISSUED
      RETURN 
C***********************************************************************
C                                                                      C
C                         C O N V E R                                  C
C                                                                      C
C               COPYRIGHT (C) 1982,1983,1984,1985, 1995                C
C                 M.S. GERBER & ASSOCIATES, INC.                       C
C                     ALL RIGHTS RESERVED                              C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C     PURPOSE:  CONVER CHECKS FOR CONVERGENCE OF GROSS OPERATING       C
C               INCOME, DIVIDENDS PER SHARE AND ITC TAKEN FOR EACH     C
C               YEAR.  IF THE OPERATING METHOD IS NOT NPV OR NOT       C
C               DPS THE ITC CONVERGENCE TAKES PRIORITY.                C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
      ENTRY INCOME_CONVERGENCE(ITER,INCOME_CONVERGED,OPERATING_METHOD,
     +                         AFUDC_RETURN_POLICY,
     +                         COVERAGE_RATIO,FUNDS_BALANCE,
     +                         COM_EQUITY,CAPITAL_WITH_STD,
     +                         TARGET_OPT_REVENUES,
     +                         STD_INTEREST,
     +                         OPERATING_REVENUE_TAX,
     +                         R_BTL_AMORTIZATION_IN_EXPENSES,
     +                         R_OTHER_TAXES,
     +                         R_DEF_TAXES_AMT_CREDITS,
     +                         INTEREST_ON_LONG_TERM_DEBT,
     +                         LTD_PS_DEFERRED_TAX_CR,
     +                         NF_DEFERRED_TAXES_CR,
     +                         LTD_INTEREST_CASH_PAYMENTS,
     +                         LONG_TERM_DEBT_RETIREMENTS,
     +                         AFUDC_BORROWED,
     +                         MTG_DEBT_RETIREMENTS,
     +                         R_VARIABLE)
C
C***********************************************************************
C                                                                      *
C                  GOPINC-TO-INTEREST CONVERGENCE ROUTINE              *
C                       USED WHEN OPERATING_METHOD = <C>OVERAGE        *
C                                                                      *
C***********************************************************************
C
      BTL_AMORTIZATION_IN_EXPENSES = R_BTL_AMORTIZATION_IN_EXPENSES
      OTHER_TAXES = R_OTHER_TAXES
C      DEF_TAXES_AMT_CREDITS = R_DEF_TAXES_AMT_CREDITS
      IF(OPERATING_METHOD == 'C') THEN
         IF(COVERAGE_RATIO == 1) THEN
C USE CODE SPECIFIC TO TVA
            IF(ITER == 1) THEN
C ESTIMATE NET INCOME NI
               CALL CALCULATE_NET_INCOME
               GOPINP = 0.
               INCOME_CONVERGED = .FALSE.
               CONVERGE_TEST = .FALSE.
            ELSEIF(ITER == 2) THEN
               CALL CALCULATE_NET_INCOME
               GOPINC = GOPINP
               INCOME_CONVERGED = .FALSE.
            ELSE
C
C COMPUTE CALCULATED OPINCO FROM LAST ITERATION
C
               GOPINP = MIN_COV_RATIO * INTEREST
C
C CHECK FOR CONVERGENCE
C
               IF(ABS(GOPINP - GOPINC) <= .001 .OR. CONVERGE_TEST)THEN
C
C SECTION 3.2 PART A (RATE TEST) (CASH FLOW TEST)
C
                  INCOME_CONVERGED = .FALSE.
                  CONVERGE_TEST = .TRUE.
                  SEC32A_AMT = NET_INCOME + DEPAMT - 20.
                  IF(SEC32A_AMT >= 60.) THEN
C
C SECTION 3.2 PART B
C
                     SEC32B_AMT = SEC32A_AMT + INTEREST
                     IF(SEC32B_AMT >= 60.) THEN
                        INCOME_CONVERGED = .TRUE.
                     ELSE
                        NET_INCOME = 80. - DEPAMT - INTEREST
                     ENDIF
                  ELSE
                     NET_INCOME = 80. - DEPAMT
                  ENDIF
                  CALL CALCULATE_GROSS_OPT_INCOME(GOPINC,
     +                                            OPERATING_METHOD)
                  GOPINP = GOPINC 
               ELSE
                  GOPINC = GOPINP
                  CALL CALCULATE_NET_INCOME
                  INCOME_CONVERGED = .FALSE.
               ENDIF
            ENDIF
C         ELSEIF(COVERAGE_RATIO == 2 .OR. COVERAGE_RATIO == 3 .OR.
C     +                                        COVERAGE_RATIO == 13) THEN
         ELSEIF(COVERAGE_RATIO == 2 .OR. COVERAGE_RATIO == 3) THEN
C           USE CODE SPECIFIC TO COVERAGE_RATIO OF 2 OR 3
            IF(ITER == 1) THEN
C              ESTIMATE NET_INCOME
C              AFUDC METHOD IS 1
               CALL CALCULATE_NET_INCOME
               INCOME_CONVERGED = .FALSE.
            ELSE
            CALL CALCULATE_GROSS_OPT_INCOME(GOPINP,OPERATING_METHOD)
C CHECK FOR CONVERGENCE
! 031207.
               COVERAGE = 0.0
               IF(COVERAGE_RATIO == 2)DENUMRATR = INTEREST - AFUDC
               IF(COVERAGE_RATIO == 3) DENUMRATR = INTEREST 
               IF(DENUMRATR == 0.) THEN
                  COVERAGE = 0.
               ELSE
                  COVERAGE = COVERAGE 
     +                       +  GOPINP
     +                       + INVESTMENT_INCOME
c     +                       + NET_DERIVATIVES_INCOME
     +                       + CLASS_BTL_REVENUES
     +                       + ITC_USED - ITC_AMORTIZATION 
     +                       + ATL_INCOME_TAXES      ! FDTAXPAID + STBOKTAX - BTL_INCOME_TAXES
     +                       - CLASS_BTL_EXPENSES
                  COVERAGE = COVERAGE/DENUMRATR
               ENDIF
               IF((ABS(MIN_COV_RATIO - COVERAGE) <= .0001 .OR.  
     +                               COVERAGE == 0.).AND. ITER > 3) THEN
                  INCOME_CONVERGED = .TRUE.
               ELSE
                  IF(ITER < 4) THEN
                     ONI = GOPINP
                     OOPINC = COVERAGE
                     GOPINC = GOPINP + (MIN_COV_RATIO - COVERAGE) * 
     +                                 (1.-STATE_TAX_RATE) *
     +                                  (1.-FEDERAL_TAX_RATE) * INTEREST
                  ELSE
                     DENOM = COVERAGE - OOPINC
                     OOPINC = COVERAGE
                     IF(DENOM<1.E-9 .AND. DENOM>=0.) DENOM=1.E-9
                     IF(DENOM>-1.E-9 .AND. DENOM<=0.) DENOM=-1.E-9
                     SLOPE= (GOPINP - ONI - DENOM) / DENOM
                     ONI = GOPINP
                     DENOM = MIN_COV_RATIO - COVERAGE
                     IF(SLOPE == -1.) SLOPE = -.9
                     GOPINC = GOPINP + DENOM + DENOM * SLOPE
                  ENDIF
                  CALL CALCULATE_NET_INCOME
                  INCOME_CONVERGED = .FALSE.
               ENDIF
            ENDIF
         ELSE IF(COVERAGE_RATIO == 4 .OR. COVERAGE_RATIO == 5) THEN
C USE CODE SPECIFIC TO COVERAGE_RATIO 4 OR 5
            IF(ITER == 1) THEN
C              ESTIMATE NET_INCOME
C              AFUDC METHOD IS 1
               CALL CALCULATE_NET_INCOME
               EQUITY_TEST = .FALSE.
               INCOME_CONVERGED = .FALSE.
            ELSE
               CALL CALCULATE_GROSS_OPT_INCOME(GOPINP,
     +                                         OPERATING_METHOD)
C CHECK FOR CONVERGENCE
               IF(.NOT. EQUITY_TEST) THEN
                  DENUMRATR = INTEREST + LTD_PS_RETIREMENTS
     +                                
                  COVERAGE = GOPINP + CLASS_BTL_REVENUES
c     +                       + NET_DERIVATIVES_INCOME
     +                       + INVESTMENT_INCOME + DEPAMT
     +                       + ITC_USED
     +                       + DEFERRED_TAXES_CR
     +                       + LTD_PS_DEFERRED_TAX_CR
     +                       + NF_DEFERRED_TAXES_CR
     +                       - ITC_AMORTIZATION + DEFERRED_TAXES_DR -
     +                       BTL_INCOME_TAXES - CLASS_BTL_EXPENSES
               ELSE
                  COVERAGE = COM_EQUITY(2)
                  DENUMRATR = CAPITAL_WITH_STD
               ENDIF
               IF(DENUMRATR == 0.) THEN
                  COVERAGE = 0.
               ELSE
                  COVERAGE = COVERAGE/DENUMRATR
               ENDIF
               IF(ABS(MIN_COV_RATIO - COVERAGE) <= .0001 .AND. 
     +                             ITER /= 2 .AND. FUNDS_BALANCE) THEN
                  IF(.NOT. EQUITY_TEST .AND. COVERAGE_RATIO == 5) THEN
C CHECK FOR THE EQUITY RATIO TO BE SATISFIED.  
                     DENUMRATR = CAPITAL_WITH_STD
                     IF(DENUMRATR /= 0.) THEN
                        IF(COM_EQUITY(2)/DENUMRATR >= 
     +                                         TARGET_EQUITY_RATIO) THEN
                           INCOME_CONVERGED = .TRUE.
                        ELSE
C NEED TO SET THE EQUITY TEST ON
                           EQUITY_TEST = .TRUE.
                           ITER = 2
                           ONI = GOPINP
                           OOPINC = COM_EQUITY(2)/DENUMRATR
                           MIN_COV_RATIO = TARGET_EQUITY_RATIO
                           GOPINC = GOPINP + (MIN_COV_RATIO - COVERAGE)*
     +                                              DENUMRATR
                           CALL CALCULATE_NET_INCOME
                           INCOME_CONVERGED = .FALSE.
                        ENDIF
                     ENDIF
                  ELSE
                     INCOME_CONVERGED = .TRUE.
                  ENDIF
               ELSE
                  IF(ITER == 2) THEN
                     ONI = GOPINP
                     OOPINC = COVERAGE
                     GOPINC = GOPINP + (MIN_COV_RATIO - COVERAGE) * 
     +                                 (1.-STATE_TAX_RATE) *
     +                                  (1.-FEDERAL_TAX_RATE) * INTEREST
                  ELSE
                     DENOM = COVERAGE - OOPINC
                     OOPINC = COVERAGE
                     IF(DENOM<1.E-9 .AND. DENOM>=0.) DENOM=1.E-9
                     IF(DENOM>-1.E-9 .AND. DENOM<=0.) DENOM=-1.E-9
                     SLOPE= (GOPINP - ONI - DENOM) / DENOM
                     ONI = GOPINP
                     DENOM = MIN_COV_RATIO - COVERAGE
                     IF(SLOPE == -1.) SLOPE = -.9
                     GOPINC = GOPINP + DENOM + DENOM * SLOPE
                  ENDIF
                  CALL CALCULATE_NET_INCOME
                  INCOME_CONVERGED = .FALSE.
               ENDIF
            ENDIF
         ELSE IF(COVERAGE_RATIO == 6 .OR. COVERAGE_RATIO == 7) THEN
C
C ADDED 9/29/92 FOR SRP ADDED PROPERTY TAX BACK INTO THE COVERAGE RATIO
C      AND REMOVED STD INTEREST FROM THE RATIO
C USE CODE SPECIFIC TO COVERAGE_RATIO 6 OR 7
C
            IF(ITER == 1) THEN
C              ESTIMATE NET_INCOME
C              AFUDC METHOD IS 1
               CALL CALCULATE_NET_INCOME
               EQUITY_TEST = .FALSE.
               INCOME_CONVERGED = .FALSE.
            ELSE
               CALL CALCULATE_GROSS_OPT_INCOME(GOPINP,OPERATING_METHOD)
C CHECK FOR CONVERGENCE
               IF(.NOT. EQUITY_TEST) THEN
                  DENUMRATR = INTEREST-STD_INTEREST+LTD_PS_RETIREMENTS
                  COVERAGE = GOPINP + CLASS_BTL_REVENUES
c     +                       + NET_DERIVATIVES_INCOME
     +                       + INVESTMENT_INCOME +
     +                       CLASS_PROPERTY_TAXES - STD_INTEREST + 
     +                       BOOK_DEPRECIATION +
     +                       AMORTIZATION_EXPENSE +
     +                       CIAC_AMORTIZATION -
     +                       CLASS_BTL_EXPENSES
               ELSE
                  COVERAGE = COM_EQUITY(2)
                  DENUMRATR = CAPITAL_WITH_STD
               ENDIF
               IF(DENUMRATR == 0.) THEN
                  COVERAGE = 0.
               ELSE
                  COVERAGE = COVERAGE/DENUMRATR
               ENDIF
               IF(.NOT. EQUITY_TEST) SRP_RATIO = COVERAGE
               IF(ABS(MIN_COV_RATIO - COVERAGE) <= .0001 .AND. 
     +                             ITER /= 2 .AND. FUNDS_BALANCE) THEN
                  IF(.NOT. EQUITY_TEST .AND. COVERAGE_RATIO == 7) THEN
C CHECK FOR THE EQUITY RATIO TO BE SATISFIED.  
                     DENUMRATR = CAPITAL_WITH_STD
                     IF(DENUMRATR /= 0.) THEN
                        IF(COM_EQUITY(2)/DENUMRATR >= 
     +                                         TARGET_EQUITY_RATIO) THEN
                           INCOME_CONVERGED = .TRUE.
                        ELSE
C NEED TO SET THE EQUITY TEST ON
                           EQUITY_TEST = .TRUE.
                           ITER = 2
                           ONI = GOPINP
                           OOPINC = COM_EQUITY(2)/DENUMRATR
                           MIN_COV_RATIO = TARGET_EQUITY_RATIO
                           GOPINC = GOPINP + (MIN_COV_RATIO - COVERAGE)*
     +                                              DENUMRATR
                           CALL CALCULATE_NET_INCOME
                           INCOME_CONVERGED = .FALSE.
                        ENDIF
                     ENDIF
                  ELSE
                     INCOME_CONVERGED = .TRUE.
                  ENDIF
               ELSE
                  IF(ITER == 2) THEN
                     ONI = GOPINP
                     OOPINC = COVERAGE
                     GOPINC = GOPINP + (MIN_COV_RATIO - COVERAGE) * 
     +                                 (1.-STATE_TAX_RATE) *
     +                                  (1.-FEDERAL_TAX_RATE) * INTEREST
                  ELSE
                     DENOM = COVERAGE - OOPINC
                     OOPINC = COVERAGE
                     IF(DENOM<1.E-9 .AND. DENOM>=0.) DENOM=1.E-9
                     IF(DENOM>-1.E-9 .AND. DENOM<=0.) DENOM=-1.E-9
                     SLOPE= (GOPINP - ONI - DENOM) / DENOM
                     ONI = GOPINP
                     DENOM = MIN_COV_RATIO - COVERAGE
                     IF(SLOPE == -1.) SLOPE = -.9
                     GOPINC = GOPINP + DENOM + DENOM * SLOPE
                  ENDIF
                  CALL CALCULATE_NET_INCOME
                  INCOME_CONVERGED = .FALSE.
               ENDIF
            ENDIF
         ELSE IF(COVERAGE_RATIO == 8) THEN ! .OR.COVERAGE_RATIO==9) THEN
C USE CODE SPECIFIC TO COVERAGE_RATIO OF 8 OR 9 FOR UPA3/26/93
            DEBT_SERVICE_TARGET = TARGET_EQUITY_RATIO
            IF(ITER == 1) THEN
C              ESTIMATE NET_INCOME
C              AFUDC METHOD IS 1
               CALL CALCULATE_NET_INCOME
               INCOME_CONVERGED = .FALSE.
               CONVERGE_TEST = .FALSE.
               TIER_TARGET = MIN_COV_RATIO
            ELSE
               CALL CALCULATE_GROSS_OPT_INCOME(GOPINP,OPERATING_METHOD)
C CHECK FOR CONVERGENCE
               DENUMRATR = INTEREST - AFUDC +
     +               MAX(0.,LEASE_PAYMENTS_IN_TIER-.02*COM_EQUITY(2))/3.
               IF(COVERAGE_RATIO == 9) DENUMRATR = DENUMRATR + AFUDC
               IF(DENUMRATR == 0.) THEN
                  COVERAGE = 0.
               ELSE
                  NUMERATOR = GOPINP + INVESTMENT_INCOME +
     +                        CLASS_BTL_REVENUES
c     +                        + NET_DERIVATIVES_INCOME
     +                        + ITC_USED - ITC_AMORTIZATION +
     +                        ATL_INCOME_TAXES - 
     +                        CLASS_BTL_EXPENSES +
     +               MAX(0.,LEASE_PAYMENTS_IN_TIER-.02*COM_EQUITY(2))/3.
                  COVERAGE = NUMERATOR/DENUMRATR
               ENDIF
               IF(ABS(TIER_TARGET - COVERAGE) <= .0001 .AND. 
     +                                                   ITER /= 2) THEN
                  DEBT_SERVICE_RATIO = (NUMERATOR + DEPAMT)/
     +                                  (DENUMRATR + LTD_PS_RETIREMENTS)
                  IF(.NOT. CONVERGE_TEST .AND. 
     +                   DEBT_SERVICE_RATIO >= DEBT_SERVICE_TARGET) THEN
                     INCOME_CONVERGED = .TRUE.
                  ELSE
                     CONVERGE_TEST = .TRUE.
                     IF(ABS(DEBT_SERVICE_RATIO - DEBT_SERVICE_TARGET) <
     +                                                        .005) THEN
                        INCOME_CONVERGED = .TRUE.
                     ELSE
                        TIER_TARGET = TIER_TARGET * (1. +
     +                       (DEBT_SERVICE_TARGET - DEBT_SERVICE_RATIO)/
     +                                    DEBT_SERVICE_RATIO)
                        ITER = 2
                     ENDIF
                  ENDIF
               ENDIF
               IF(.NOT. INCOME_CONVERGED) THEN
                  IF(ITER == 2) THEN
                     ONI = GOPINP
                     OOPINC = COVERAGE
                     GOPINC = GOPINP + (TIER_TARGET - COVERAGE) * 
     +                                 (1.-STATE_TAX_RATE) *
     +                                  (1.-FEDERAL_TAX_RATE) * INTEREST
                  ELSE
                     DENOM = COVERAGE - OOPINC
                     OOPINC = COVERAGE
                     IF(DENOM < 1.E-9 .AND. DENOM >= 0.) DENOM=1.E-9
                     IF(DENOM > -1.E-9 .AND. DENOM <= 0.) DENOM=-1.E-9
                     SLOPE= (GOPINP - ONI - DENOM) / DENOM
                     ONI = GOPINP
                     DENOM = TIER_TARGET - COVERAGE
                     IF(SLOPE == -1.) SLOPE = -.9
                     GOPINC = GOPINP + DENOM + DENOM * SLOPE
                  ENDIF
                  CALL CALCULATE_NET_INCOME
                  INCOME_CONVERGED = .FALSE.
               ENDIF
            ENDIF
         ELSEIF(COVERAGE_RATIO == 10) THEN
C USE CODE SPECIFIC TO DSCR OF FOR WVPA 7/2004
            IF(ITER == 1) THEN
C              ESTIMATE NET_INCOME
C              AFUDC METHOD IS 1
               CALL CALCULATE_NET_INCOME
               INCOME_CONVERGED = .FALSE.
            ELSE
               CALL CALCULATE_GROSS_OPT_INCOME(GOPINP,OPERATING_METHOD)
C CHECK FOR CONVERGENCE
               DENUMRATR = LTD_INTEREST_CASH_PAYMENTS
     +                     + LONG_TERM_DEBT_RETIREMENTS
     +                     + MTG_DEBT_RETIREMENTS
               IF(DENUMRATR == 0.) THEN
                  COVERAGE = 0.
               ELSE
                  COVERAGE = INTEREST_ON_LONG_TERM_DEBT
     +                       + INTEREST_AMORTIZATION
     +                       - AFUDC_BORROWED     ! net interest booked
     +                       + NET_INCOME
     +                       + BOOK_DEPRECIATION
                  COVERAGE = COVERAGE/DENUMRATR
               ENDIF
               IF(ABS(MIN_COV_RATIO - COVERAGE) <= .0001 .AND. 
     +                                                    ITER > 3) THEN
                  INCOME_CONVERGED = .TRUE.
               ELSE
                  IF(ITER < 4) THEN
                     ONI = GOPINP
                     OOPINC = COVERAGE
                     GOPINC = GOPINP + (MIN_COV_RATIO - COVERAGE) * 
     +                                 (1.-STATE_TAX_RATE) *
     +                                  (1.-FEDERAL_TAX_RATE) * INTEREST
                  ELSE
                     DENOM = COVERAGE - OOPINC
                     OOPINC = COVERAGE
                     IF(DENOM<1.E-9 .AND. DENOM>=0.) DENOM=1.E-9
                     IF(DENOM>-1.E-9 .AND. DENOM<=0.) DENOM=-1.E-9
                     SLOPE= (GOPINP - ONI - DENOM) / DENOM
                     ONI = GOPINP
                     DENOM = MIN_COV_RATIO - COVERAGE
                     IF(SLOPE == -1.) SLOPE = -.9
                     GOPINC = GOPINP + DENOM + DENOM * SLOPE
                  ENDIF
                  CALL CALCULATE_NET_INCOME
                  INCOME_CONVERGED = .FALSE.
               ENDIF
            ENDIF
         ELSEIF(COVERAGE_RATIO == 11) THEN
C USE CODE SPECIFIC TO COVERAGE_RATIO OF 11 FOR WVPA 8/1/2002
            IF(ITER == 1) THEN
C              ESTIMATE NET_INCOME
C              AFUDC METHOD IS 1
               CALL CALCULATE_NET_INCOME
               INCOME_CONVERGED = .FALSE.
            ELSE
               CALL CALCULATE_GROSS_OPT_INCOME(GOPINP,OPERATING_METHOD)
C CHECK FOR CONVERGENCE
               DENUMRATR = INTEREST_ON_LONG_TERM_DEBT
               DENUMRATR = INTEREST_ON_LONG_TERM_DEBT
     +                     + INTEREST_AMORTIZATION
     +                     - AFUDC_BORROWED     ! net interest booked
               IF(DENUMRATR == 0.) THEN
                  COVERAGE = 0.
               ELSE
                  COVERAGE = DENUMRATR + NET_INCOME
                  COVERAGE = COVERAGE/DENUMRATR
               ENDIF
               IF(ABS(MIN_COV_RATIO - COVERAGE) <= .0001 .AND. 
     +                                                    ITER > 3) THEN
                  INCOME_CONVERGED = .TRUE.
               ELSE
                  IF(ITER < 4) THEN
                     ONI = GOPINP
                     OOPINC = COVERAGE
                     GOPINC = GOPINP + (MIN_COV_RATIO - COVERAGE) * 
     +                                 (1.-STATE_TAX_RATE) *
     +                                  (1.-FEDERAL_TAX_RATE) * INTEREST
                  ELSE
                     DENOM = COVERAGE - OOPINC
                     OOPINC = COVERAGE
                     IF(DENOM<1.E-9 .AND. DENOM>=0.) DENOM=1.E-9
                     IF(DENOM>-1.E-9 .AND. DENOM<=0.) DENOM=-1.E-9
                     SLOPE= (GOPINP - ONI - DENOM) / DENOM
                     ONI = GOPINP
                     DENOM = MIN_COV_RATIO - COVERAGE
                     IF(SLOPE == -1.) SLOPE = -.9
                     GOPINC = GOPINP + DENOM + DENOM * SLOPE
                  ENDIF
                  CALL CALCULATE_NET_INCOME
                  INCOME_CONVERGED = .FALSE.
               ENDIF
            ENDIF
         ELSE IF(COVERAGE_RATIO == 12) THEN
C USE CODE SPECIFIC TO COVERAGE_RATIO OF 8 OR 9 FOR UPA3/26/93
            IF(ITER == 1) THEN
C              ESTIMATE NET_INCOME
C              AFUDC METHOD IS 1
               CALL CALCULATE_NET_INCOME
               INCOME_CONVERGED = .FALSE.
               CONVERGE_TEST = .FALSE.
               GRE_TARGET_RATIO = MIN_COV_RATIO
            ELSE
               CALL CALCULATE_GROSS_OPT_INCOME(GOPINP,OPERATING_METHOD)
C CHECK FOR CONVERGENCE
               CALL GRE_DSC_REPORT(R_VARIABLE)
               COVERAGE = R_VARIABLE(773)
C
               IF((ABS(GRE_TARGET_RATIO-COVERAGE) <= .0001 .AND.
     +                                ITER > 3) .OR. CONVERGE_TEST) THEN
                  INCOME_CONVERGED = .FALSE. ! .TRUE.
                  IF(R_VARIABLE(775) >= 2.) THEN
                     INCOME_CONVERGED = .TRUE.
                  ELSEIF(.NOT. CONVERGE_TEST) THEN
                     R_VARIABLE(0) = 1.02 * R_VARIABLE(776)
                     R_VARIABLE(4) = R_VARIABLE(0)
     +                               + R_VARIABLE(485)
                     R_VARIABLE(769) = R_VARIABLE(0)  ! GRE_NET_MARGIN), ! 769
     +                               - R_VARIABLE(770) ! COST OF SERVICE
     +                               + R_VARIABLE(771)  !
     +                               + R_VARIABLE(772)
                     ITER = 3
                  ENDIF
                  CONVERGE_TEST = .TRUE.
                  COVERAGE = 1. + R_VARIABLE(775)/100.
               ELSEIF(.NOT. INCOME_CONVERGED) THEN
                 R_VARIABLE(769) = ! GRE_NET_MARGIN), ! 769
     +                 GRE_TARGET_RATIO * (R_VARIABLE(766)      ! GRE_MEMBER_REVENUE_REQUIREMENT), ! 773
     +                                     + R_VARIABLE(768))   ! GRE_TOTAL_INTEREST_PAYMENTS), ! 768
     +                 - R_VARIABLE(761)   ! GRE_TOTAL_INTEREST_EXPENSE), ! 761
     +                 - R_VARIABLE(763)   ! GRE_TOTAL_DEPRECIATION), ! 763
                 R_VARIABLE(0) = R_VARIABLE(769)  ! GRE_NET_MARGIN), ! 769
     +                        + R_VARIABLE(770) ! COST OF SERVICE
     +                        - R_VARIABLE(771)  !
     +                        - R_VARIABLE(772)
                 R_VARIABLE(4) = R_VARIABLE(0)
     +                        + R_VARIABLE(485)
               ENDIF
                 CALL CALCULATE_GRE_GOPINC(OPERATING_REVENUE_TAX,
     +                                     R_VARIABLE(4),
     +                                     LTD_PS_DEFERRED_TAX_CR,
     +                                     NF_DEFERRED_TAXES_CR)
                 CALL CALCULATE_NET_INCOME
              if(.false.) then
               IF(.NOT. INCOME_CONVERGED) THEN
                  IF(ITER < 25) THEN
                     ONI = GOPINP
                     OOPINC = COVERAGE
                     GOPINC = GOPINP + (GRE_TARGET_RATIO - COVERAGE) *
     +                                 (1.-STATE_TAX_RATE) *
     +                                  (1.-FEDERAL_TAX_RATE) * INTEREST
                  ELSE
                     DENOM = COVERAGE - OOPINC
                     OOPINC = COVERAGE
                     IF(DENOM<1.E-9 .AND. DENOM>=0.) DENOM=1.E-9
                     IF(DENOM>-1.E-9 .AND. DENOM<=0.) DENOM=-1.E-9
                     SLOPE= (GOPINP - ONI - DENOM) / DENOM
                     ONI = GOPINP
                     DENOM = GRE_TARGET_RATIO - COVERAGE
                     IF(SLOPE == -1.) SLOPE = -.9
                     GOPINC = GOPINP + DENOM + DENOM * SLOPE
                  ENDIF
                  CALL CALCULATE_NET_INCOME
                  INCOME_CONVERGED = .FALSE.
               ENDIF
              endif
            ENDIF
         ELSEIF(COVERAGE_RATIO == 13 .OR. COVERAGE_RATIO == 16 .OR.
     +                                        COVERAGE_RATIO == 17) THEN !ODEC speicific driver
            IF(ITER == 1) THEN
C
C FOR ITERATION 1, ESTIMATED NET_INCOME IS CALCULATED HERE
C
               CALL CALCULATE_NET_INCOME
               GOPINP = 0.
               RE_ODEC_COVERAGE = 0.
               INCOME_CONVERGED = .FALSE.
               INCOME_HAS_CONVERGED = .FALSE.
               INPUT_COVERAGE_RATIO = COMMON_PAYOUT_RATIO
            ELSE
C
C COMPUTE CALCULATED OPINCO FROM LAST ITERATION
C
               CALL CALCULATE_GROSS_OPT_INCOME(GOPINP,OPERATING_METHOD)
C
C CHECK FOR CONVERGENCE
C
               COMPARE1 = GOPINP - GOPINC
               IF(ITER > 10) THEN
                  IF(COVERAGE_RATIO == 16 .OR. COVERAGE_RATIO == 17 .OR.
     +                                        COVERAGE_RATIO == 18) THEN
                     INCOME_HAS_CONVERGED = .TRUE.
c                     INCOME_CONVERGED = .FALSE.
                     IF(CAPITAL_WITH_STD == 0.) THEN
                        COVERAGE = 0.
                     ELSE
                        COVERAGE = (COM_EQUITY(2)+RE_ODEC_COVERAGE)/
     +                            (CAPITAL_WITH_STD+RE_ODEC_COVERAGE)
                     ENDIF
                     IF(COVERAGE > ODEC_MaxEquityRatio) THEN
                        IF(COVERAGE_RATIO == 16) THEN                 
                           RE_ODEC_COVERAGE = COM_EQUITY(1) + NET_INCOME
     +                          + ODEC_MaxEquityRatio * 
     +                            (CAPITAL_WITH_STD - COM_EQUITY(2))/
     +                                  (ODEC_MaxEquityRatio - 1.)   
                        ELSEIF(COVERAGE_RATIO == 17) THEN
                           RE_ODEC_COVERAGE = ODEC_PercentREReduction * 
     +                                                     COM_EQUITY(1)
     +                                        + ODEC_DollarREReduction
                        ENDIF
                        COMMON_PAYOUT_RATIO = RE_ODEC_COVERAGE/
     +                                           NET_INCOME 
                        IF(COVERAGE_RATIO == 16 .OR. 
     +                                        COVERAGE_RATIO == 18) THEN
                           COMMON_PAYOUT_RATIO=MIN(COMMON_PAYOUT_RATIO,
     +                                             ODEC_MaxPayoutRatio)
                           COMMON_PAYOUT_RATIO=MAX(COMMON_PAYOUT_RATIO,
     +                                             ODEC_MinPayoutRatio)
                        ENDIF
                        NET_INCOME = (MIN_COV_RATIO -1.) * (INTEREST     
     +                                          + INTEREST_AMORTIZATION)
                        CALL CALCULATE_GROSS_OPT_INCOME(GOPINC,
     +                                                 OPERATING_METHOD)
                     ELSEIF(COVERAGE < TARGET_EQUITY_RATIO/100.) THEN
                        NET_INCOME = (MIN_COV_RATIO -1.) * (INTEREST     
     +                                          + INTEREST_AMORTIZATION)
     +                            +(TARGET_EQUITY_RATIO/100. 
     +                              * CAPITAL_WITH_STD - COM_EQUITY(2))/
     +                                   (1. - TARGET_EQUITY_RATIO/100.)
                        CALL CALCULATE_GROSS_OPT_INCOME(GOPINC,
     +                                                 OPERATING_METHOD)
                     ELSE
                        IF(ABS(COMPARE1) < .0005 .AND. ITER > 15) THEN
                           INCOME_CONVERGED = .TRUE.
                        ELSE
                           NET_INCOME = (MIN_COV_RATIO -1.) * (INTEREST     
     +                                          + INTEREST_AMORTIZATION)
                           CALL CALCULATE_GROSS_OPT_INCOME(GOPINC,
     +                                                 OPERATING_METHOD)
                           IF(RE_ODEC_COVERAGE /= 0.) THEN
                              COMMON_PAYOUT_RATIO = RE_ODEC_COVERAGE/
     +                                                 NET_INCOME 
                           ENDIF
                        ENDIF
                     ENDIF
                  ELSEIF(ABS(COMPARE1) < .0005) THEN
                     INCOME_CONVERGED = .TRUE.
                  ELSE
                     NET_INCOME = (MIN_COV_RATIO -1.) * (INTEREST     
     +                                          + INTEREST_AMORTIZATION)                 
                     CALL CALCULATE_GROSS_OPT_INCOME(GOPINC,
     +                                               OPERATING_METHOD)
                  ENDIF   
               ELSE
C
C NET_INCOME ESTIMATE
C
                  NET_INCOME = (MIN_COV_RATIO -1.) * (INTEREST     
     +                                          + INTEREST_AMORTIZATION)                 
                  CALL CALCULATE_GROSS_OPT_INCOME(GOPINC,
     +                                            OPERATING_METHOD)
               ENDIF
            ENDIF
         ELSEIF(COVERAGE_RATIO == 14) THEN !ODEC speicific driver
            IF(ITER == 1) THEN
C
C FOR ITERATION 1, ESTIMATED NET_INCOME IS CALCULATED HERE
C
               CALL CALCULATE_NET_INCOME
               GOPINP = 0.
               INCOME_CONVERGED = .FALSE.
            ELSE
C
C COMPUTE CALCULATED OPINCO FROM LAST ITERATION
C
               CALL CALCULATE_GROSS_OPT_INCOME(GOPINP,OPERATING_METHOD)
C
C CHECK FOR CONVERGENCE
C
               COMPARE1 = GOPINP - GOPINC
               IF(ABS(COMPARE1) < .0005 .AND. ITER >= 10) THEN
                  INCOME_CONVERGED = .TRUE.
               ELSE
C
C NET_INCOME ESTIMATE
C
                  NET_INCOME = (MIN_COV_RATIO -1.) * (INTEREST     
     +                                          + INTEREST_AMORTIZATION)                 
     +                         + ROEQU*(COM_EQUITY(1)+COM_EQUITY(2))/2.
                  CALL CALCULATE_GROSS_OPT_INCOME(GOPINC,
     +                                            OPERATING_METHOD)
               ENDIF
            ENDIF
         ELSEIF(COVERAGE_RATIO == 15) THEN !IMPA speicific driver
            IF(ITER == 1) THEN
C
C FOR ITERATION 1, ESTIMATED NET_INCOME IS CALCULATED HERE
C
               NET_INCOME = (MIN_COV_RATIO - 1.) *
     +                                       (INTEREST_ON_LONG_TERM_DEBT     ! Interest on LTD (#293
     +                                        + R_VARIABLE(497)   ! Self Funded Capital Assets (#497)             
     +                                        + R_VARIABLE(119))  ! LTD retirement Payments (#119)
               GOPINP = 0.
               INCOME_CONVERGED = .FALSE.
            ELSE
C
C COMPUTE CALCULATED OPINCO FROM LAST ITERATION
C
               CALL CALCULATE_GROSS_OPT_INCOME(GOPINP,OPERATING_METHOD)
C
C CHECK FOR CONVERGENCE
C
               COMPARE1 = GOPINP - GOPINC
               IF(ABS(COMPARE1) < .0005 .AND. ITER >= 20) THEN
                  INCOME_CONVERGED = .TRUE.
               ELSE
C
C NET_INCOME ESTIMATE
C
               NET_INCOME = (MIN_COV_RATIO - 1.) *
     +                                       (INTEREST_ON_LONG_TERM_DEBT     ! Interest on LTD (#293
     +                                        + R_VARIABLE(497)   ! Self Funded Capital Assets (#497)             
     +                                        + R_VARIABLE(119))  ! LTD retirement Payments (#119)
                CALL CALCULATE_GROSS_OPT_INCOME(GOPINC,OPERATING_METHOD)
               ENDIF
            ENDIF
         ENDIF
C
C***********************************************************************
C                                                                      *
C                  OPERATING REVENUE CONVERGENCE ROUTINE               *
C                       USED WHEN OPERATING_METHOD = OPREV             *
C                                                                      *
C***********************************************************************
C
      ELSEIF(OPERATING_METHOD == 'O' .OR. OPERATING_METHOD == 'P') THEN
         IF(ITER == 1) THEN
C
C FOR ITERATION 1, ESTIMATED NET_INCOME IS CALCULATED HERE
C
            GOPINC = TARGET_OPT_REVENUES - OPERATING_REVENUE_TAX -
     +                ATL_INCOME_TAXES -            
     +                OTHER_TAXES - CLASS_PROPERTY_TAXES -
     +                ENVIRONMENTAL_TAX -
     +                TOTAL_EXPENSES_B4_TAXES -
     +                DEFERRED_TAXES_DR
     +                - DEFERRED_TAXES_CR
     +                - LTD_PS_DEFERRED_TAX_CR
     +                - NF_DEFERRED_TAXES_CR
C    +                + DEF_TAXES_AMT_CREDITS
     +                - ITC_USED  + ITC_AMORTIZATION -
     +                STATE_TAX_ON_CAPITAL - FEDERAL_TAX_ON_CAPITAL
  
            CALL CALCULATE_NET_INCOME
            OPREVP = 0.
            INCOME_CONVERGED = .FALSE.
         ELSE 
C
C COMPUTE CALCULATED OPINCO FROM LAST ITERATION
C
            CALL CALCULATE_TOTAL_REVENUES(OPERATING_REVENUE_TAX,OPREVP,
     +                                    LTD_PS_DEFERRED_TAX_CR,
     +                                    NF_DEFERRED_TAXES_CR,
     +                                    OPERATING_METHOD)
C
C CHECK FOR CONVERGENCE
C
            COMPARE1 = OPREVP - TARGET_OPT_REVENUES
            IF(ABS(COMPARE1) < .0005) THEN
               INCOME_CONVERGED = .TRUE.
            ELSE !ENDIF
C
C FOR ITERATION 2, USE FIRST NET_INCOME ESTIMATE AND ESTIMATE OF SLOPE
C
               IF(ITER == 2) THEN
                  ONI = NET_INCOME
                  OOPINC = OPREVP
                  NET_INCOME = NET_INCOME+(TARGET_OPT_REVENUES-OPREVP) *
     +                                    (1.-STATE_TAX_RATE)*
     +                                    (1.-FEDERAL_TAX_RATE) * ESLOPE
                  INCOME_CONVERGED = .FALSE.
               ELSE 
C
C FOR ITERATIONS BEYOND 2, FIND SLOPE USING LAST 2 OPREVP
C ESTIMATES, THEN USE THIS SLOPE AND THE LAST NET_INCOME ESTIMATE TO GET
C NEW ESTIMATE.
C
                  DENOM = (OPREVP - OOPINC) * (1.-STATE_TAX_RATE) *
     +                                             (1.-FEDERAL_TAX_RATE)
                  IF(DENOM <  1.E-9 .AND. DENOM >= 0.) DENOM =  1.E-9
                  IF(DENOM > -1.E-9 .AND. DENOM <= 0.) DENOM = -1.E-9
                  SLOPE= (NET_INCOME - ONI - DENOM) / DENOM
                  ONI = NET_INCOME
                  OOPINC = OPREVP
                  DENOM = (TARGET_OPT_REVENUES - OPREVP) *
     +                                           (1.-STATE_TAX_RATE) *
     +                                             (1.-FEDERAL_TAX_RATE)
                  IF(SLOPE == -1.) SLOPE = -.9
                  NET_INCOME = NET_INCOME + DENOM * (1. + SLOPE)
                  INCOME_CONVERGED = .FALSE.
               ENDIF
            ENDIF
         ENDIF
C
C***********************************************************************
C                                                                      *
C                  RETURN ON NPV CONVERGENCE ROUTINE                   *
C                       USED WHEN OPERATING_METHOD = NPV               *
C                                                                      *
C***********************************************************************
C
      ELSEIF(INDEX('Rr',OPERATING_METHOD) /= 0) THEN
         IF(ITER == 1) THEN
C
C FOR ITERATION 1, ESTIMATED NET_INCOME IS CALCULATED HERE
C
C           AFUDC METHOD IS 1
            CALL CALCULATE_NET_INCOME
            IF(AFUDC_RETURN_POLICY == '2' ) NET_INCOME=NET_INCOME-AFUDC
            GOPINP = 0.
            INCOME_CONVERGED = .FALSE.
         ELSE
C
C COMPUTE CALCULATED OPINCO FROM LAST ITERATION
C
            CALL CALCULATE_GROSS_OPT_INCOME(GOPINP,OPERATING_METHOD)
            IF(AFUDC_RETURN_POLICY == '2') GOPINP = GOPINP + AFUDC
C
C CHECK FOR CONVERGENCE
C
            COMPARE1 = GOPINP - GOPINC
            IF(ABS(COMPARE1) < .0005) THEN
               INCOME_CONVERGED = .TRUE.
            ELSE
C
C FOR ITERATION 2, USE FIRST NET_INCOME ESTIMATE AND ESTIMATE OF SLOPE
C
               IF(ITER == 2) THEN
                  ONI = NET_INCOME
                  OOPINC = GOPINP
                  NET_INCOME = NET_INCOME + (GOPINC - GOPINP) * ESLOPE
                  INCOME_CONVERGED = .FALSE.
               ELSE
C
C FOR ITERATIONS BEYOND 2, FIND SLOPE USING LAST 2 GOPINP ESTIMATES,
C THEN USE THIS SLOPE AND THE LAST NET_INCOME ESTIMATE TO GET A NEW ESTIMATE.
C
                  DENOM = GOPINP - OOPINC
                  IF(DENOM <  1.E-9 .AND. DENOM >= 0.) DENOM =  1.E-9
                  IF(DENOM > -1.E-9 .AND. DENOM <= 0.) DENOM = -1.E-9
                  SLOPE= (NET_INCOME - ONI - DENOM) / DENOM
                  ONI = NET_INCOME
                  OOPINC = GOPINP
                  DENOM = GOPINC - GOPINP
                  IF(SLOPE == -1.) SLOPE = -.9
                  NET_INCOME = NET_INCOME + DENOM + DENOM * SLOPE
                  INCOME_CONVERGED = .FALSE.
               ENDIF
            ENDIF
         ENDIF
      ENDIF
C
C OPERATING_METHOD IS RETURN ON EQUITY
C
      IF(OPERATING_METHOD == 'E') THEN
         INCOME_CONVERGED = .FALSE.
         IF(ITER > 6) THEN
            IF((COM_EQUITY(2) + COM_EQUITY(1)) /= 0.) ONI = 2. * 
     +                         (COMMON_DIVIDENDS + RETAINED_EARNINGS)/
     +                                (COM_EQUITY(2)+COM_EQUITY(1))
            IF(ABS(ROEQU - ONI) < .00005) INCOME_CONVERGED = .TRUE.
         ENDIF
      ENDIF
      RETURN
C***********************************************************************
      ENTRY CALCULATE_NET_INCOME
C***********************************************************************
         NET_INCOME = GOPINC
     +                + AFUDC
     +                - INTEREST
     +                + CLASS_BTL_REVENUES
     +                - CLASS_BTL_EXPENSES
     +                + INVESTMENT_INCOME
     +                + DEFERRED_REVENUES
     +                + SUBSIDIARY_INCOME
     +                - NET_OF_TAX_EXEXP
     +                - BTL_INCOME_TAXES
     +                - INTEREST_AMORTIZATION
     +                - BTL_DEF_TAXES_CR
     +                - BTL_DEFERRED_TAXES_DR
     +                - BTL_AMORTIZATION_IN_EXPENSES
      RETURN
C***********************************************************************
      ENTRY CALCULATE_GROSS_OPT_INCOME(R_GOPINC,OPERATING_METHOD)
C***********************************************************************
         R_GOPINC = NET_INCOME
     +              + NET_OF_TAX_EXEXP   ! 44 EXTRAORDIANRY EXPENSE
     +              - SUBSIDIARY_INCOME
     +              + INTEREST
     +              + INTEREST_AMORTIZATION  ! 384
     +              - AFUDC
     +              - Mark_to_Market
     +              - FE_NonCashEarningsNonCorp
     +              + BTL_AMORTIZATION_IN_EXPENSES  ! 350
     +              + BTL_DEF_TAXES_CR
     +              + BTL_DEFERRED_TAXES_DR
     +              + BTL_INCOME_TAXES
     +              + CLASS_BTL_EXPENSES   ! 35
     +              - INVESTMENT_INCOME    ! 34
     +              - CLASS_BTL_REVENUES    ! 33
     +              - DEFERRED_REVENUES  ! 32
c         IF(IMPA() .AND. OPERATING_METHOD == 'C') 
c     +                        R_GOPINC = R_GOPINC- INTEREST_AMORTIZATION 
      RETURN
C***********************************************************************
      ENTRY CALCULATE_TOTAL_REVENUES(OPERATING_REVENUE_TAX,
     +                               PROJECTED_OPERATING_REVENUES,
     +                               LTD_PS_DEFERRED_TAX_CR,
     +                               NF_DEFERRED_TAXES_CR,
     +                               OPERATING_METHOD)
C***********************************************************************
         CALL CALCULATE_GROSS_OPT_INCOME(GOPINC,OPERATING_METHOD)
         PROJECTED_OPERATING_REVENUES = GOPINC
     +                                  + ATL_INCOME_TAXES
     +                                  + OTHER_TAXES
     +                                  + CLASS_PROPERTY_TAXES
     +                                  + Payroll_Taxes
     +                                  + ENVIRONMENTAL_TAX
     +                                  + TOTAL_EXPENSES_B4_TAXES
     +                                  + DEFERRED_TAXES_DR
     +                                  + DEFERRED_TAXES_CR
     +                                  + LTD_PS_DEFERRED_TAX_CR
     +                                  + NF_DEFERRED_TAXES_CR
C    +                                  - DEF_TAXES_AMT_CREDITS
     +                                  + ITC_USED
     +                                  - ITC_AMORTIZATION
     +                                  + STATE_TAX_ON_CAPITAL
     +                                  + FEDERAL_TAX_ON_CAPITAL
     +                                  + OPERATING_REVENUE_TAX
C
      RETURN
C***********************************************************************
      ENTRY CALCULATE_GRE_GOPINC(OPERATING_REVENUE_TAX,
     +                           PROJECTED_OPERATING_REVENUES,
     +                           LTD_PS_DEFERRED_TAX_CR,
     +                           NF_DEFERRED_TAXES_CR)
C***********************************************************************
         GOPINC = PROJECTED_OPERATING_REVENUES
     +            - (ATL_INCOME_TAXES
     +               + OTHER_TAXES
     +               + CLASS_PROPERTY_TAXES
     +               + ENVIRONMENTAL_TAX
     +               + TOTAL_EXPENSES_B4_TAXES
     +               + DEFERRED_TAXES_DR
     +               + DEFERRED_TAXES_CR
     +               + LTD_PS_DEFERRED_TAX_CR
     +               + NF_DEFERRED_TAXES_CR
C    +               - DEF_TAXES_AMT_CREDITS
     +               + ITC_USED
     +               - ITC_AMORTIZATION
     +               + STATE_TAX_ON_CAPITAL
     +               + FEDERAL_TAX_ON_CAPITAL
     +               + OPERATING_REVENUE_TAX)
C
      RETURN
      END
C***********************************************************************
C
C CHECKS TO SEE IF DIVIDENDS-PER-SHARE MATCHES THE REQUIRED VALUE;
C IF SO, RETURNS DONE = YES SO THAT THE VALUES COMPUTED ON THE LAST
C ITERATION ARE STORED.  IF NOT, RETURNS DONE = NO,
C CAUSING COMMON DIVIDENDS TO BE RE-CALCULATED.
C
C***********************************************************************
      FUNCTION DIVIDENDS_CONVERGED(ITER,AVERAGE_SHARES,
     +                             DIVIDEND_PAYMENT_METHOD)
C***********************************************************************
C
      INCLUDE 'SpinLib.MON'
      USE SIZECOM
      INCLUDE 'FINACCOM.MON'
      INCLUDE 'ACRSLCOM.MON'
      LOGICAL*1 DIVIDENDS_CONVERGED,TAXES_CONVERGED
      LOGICAL*1 DIVIDENDS_CONVERGED_STATUS,TAXES_CONVERGED_STATUS
      LOGICAL*1 DIVIDENDS_CONVERGED_STATE,TAXES_CONVERGED_STATE
      SAVE DIVIDENDS_CONVERGED_STATUS,TAXES_CONVERGED_STATUS
      INTEGER*2 ITER
      CHARACTER*1 DIVIDEND_PAYMENT_METHOD,DPS,UTILITY_TYPE
      PARAMETER (DPS = 'S')
      REAL AVERAGE_SHARES,CALCULATED_DPS
      REAL FINCOMCK,SINCOMCK
      SAVE FINCOMCK,SINCOMCK
C
         DIVIDENDS_CONVERGED = .FALSE.
         IF(DIVIDEND_PAYMENT_METHOD == DPS .AND.
     +                                       UTILITY_TYPE() == 'I') THEN
            IF(ITER > 1 .AND. AVERAGE_SHARES /= 0.) THEN
               CALCULATED_DPS = COMMON_DIVIDENDS/AVERAGE_SHARES
               IF(ABS(DIVIDEND_PER_SHARE-CALCULATED_DPS) < .0001)
     +                                      DIVIDENDS_CONVERGED = .TRUE.
            ENDIF
         ELSEIF(ITER > 6) THEN
            DIVIDENDS_CONVERGED = .TRUE.
         ENDIF
         DIVIDENDS_CONVERGED_STATUS = DIVIDENDS_CONVERGED
      RETURN
C
C***********************************************************************
C                                                                      *
C                    TAX CONVERGENCE ROUTINE                           *
C                       USED ALL THE TIME                              *
C                                                                      *
C***********************************************************************
C
C     IF NPV AND DPS ARE NOT USED, INCREMENT THE NUMBER OF ITERATIONS
C
      ENTRY TAXES_CONVERGED(ITER)
C
         TAXES_CONVERGED = .FALSE.
         IF(UTILITY_TYPE() == 'I') THEN
            IF(ITER == 1) THEN
               FINCOMCK = -999999.
               SINCOMCK = -999999.
            ELSE
               IF(ABS(FINCOMCK - FDBKINCO) < .0005 .AND.
     +                         ABS(SINCOMCK - STBOKINCO) < .0005)
     +                                          TAXES_CONVERGED = .TRUE.
               FINCOMCK = FDBKINCO
               SINCOMCK = STBOKINCO
            ENDIF
         ELSE
            TAXES_CONVERGED = .TRUE.
         ENDIF
         TAXES_CONVERGED_STATUS = TAXES_CONVERGED
      RETURN
C***********************************************************************
      ENTRY DIVIDENDS_CONVERGED_STATE
C***********************************************************************
         DIVIDENDS_CONVERGED_STATE = DIVIDENDS_CONVERGED_STATUS
      RETURN
C***********************************************************************
      ENTRY TAXES_CONVERGED_STATE
C***********************************************************************
         TAXES_CONVERGED_STATE = TAXES_CONVERGED_STATUS
      RETURN
      END
C***********************************************************************
C
      SUBROUTINE RAMORT(BALORG,LTDLIFE,LTDRTE,BAL,SFA,INTRST,IYEAR,NYRS)
C     COMPUTES AN 'MTG' AMORTIZATION SCHEDULE ASSUMING CONSTANT 
C     SEMI-ANNUAL PAYMENTS OF PRINCIPAL AND INTEREST 
C
      INTEGER*2 J,MTGLIF,IYEAR,NYRS,MO
      REAL BALORG,LTDRTE,PIBIEN,LTDLIFE,
     +     BAL(0:*),SFA(0:*),INTRST(0:*)
      REAL*4 CURRENT_BAL,CURRENT_INTEREST,
     +       LTD_ISSUE_YR_INTEREST_BOOKED,
     +       LTD_ISSUE_YR_INTEREST_PAYMENT,
     +       MTG_DEBT_RETIREMENTS,
     +       PAY_PERIODS_PER_YR/12./
C
C     COMPUTE THE BIENNUAL PAYMENT NEEDED TO AMORTIZE BALORG
C     WHEN THE FIRST PAYMENT IS DUE ON 31 DECEMBER OF ISSUE YEAR
      MTGLIF = INT(LTDLIFE)
      PAY_PERIODS_PER_YR = 12.
      
      IF(LTDRTE /= 0.) THEN
         PIBIEN = (BALORG * LTDRTE/PAY_PERIODS_PER_YR)/
     +      (1. - (1./(1. + LTDRTE/PAY_PERIODS_PER_YR)**
     +                                (INT(PAY_PERIODS_PER_YR)*MTGLIF)))
      ELSE
         PIBIEN = BALORG/MAX(1.,PAY_PERIODS_PER_YR*MTGLIF)
      ENDIF
C     FOR DEVELOPMENT OF AMORTIZATION FACTOR ABOVE, SEE *FOOTNOTE BELOW;
      CURRENT_BAL = BALORG
      DO MO = 8, 12
         CURRENT_INTEREST = LTDRTE/PAY_PERIODS_PER_YR * CURRENT_BAL
         INTRST(IYEAR) = INTRST(IYEAR) + CURRENT_INTEREST
         CURRENT_BAL = MAX(0.,CURRENT_BAL - (PIBIEN - CURRENT_INTEREST))
      ENDDO
      SFA(IYEAR) = BALORG - CURRENT_BAL
      BAL(IYEAR) = CURRENT_BAL
C     FILL OUT THE AMORTIZATION SCHEDULE, ASSUMING LTDRTE REMAINS CONSTANT
C     ASSUMING ANNUAL PAYMENT ON DECEMBER 31.
      DO J=IYEAR+1,MIN(IYEAR+MTGLIF,NYRS)
         DO MO = 1, 12
            CURRENT_INTEREST = LTDRTE/PAY_PERIODS_PER_YR * CURRENT_BAL
            INTRST(J) = INTRST(J) + CURRENT_INTEREST
            CURRENT_BAL = MAX(0.,CURRENT_BAL-(PIBIEN-CURRENT_INTEREST))
         END DO
         BAL(J) = CURRENT_BAL
         SFA(J) = BAL(J-1) - BAL(J)
         IF(BAL(J) <= 0.) EXIT
      ENDDO
c      DO J=IYEAR+1,MIN(IYEAR+MTGLIF,NYRS)
c         INTRST(J) = LTDRTE * BAL(J-1)
c         SFA(J) = PIBIEN   - INTRST(J)
c         BAL(J) = MAX(0., BAL(J-1) - SFA(J))
c      ENDDO
C     ACCOUNT FOR THE HALF-YEAR OVERFLOW INTO THE YEAR OF MATURITY
c      IF (J <= NYRS .AND. BAL(J-1) > 0.) THEN
c         INTRST(J) = LTDRTE * BAL(J-1)
c         SFA(J) = BAL(J-1)
c         BAL(J) = 0.
c      ENDIF
      RETURN
C***********************************************************************
      ENTRY MTG_PRIN_INTR(LTDRTE,LTDLIFE,BALORG,
     +                    LTD_ISSUE_YR_INTEREST_BOOKED,
     +                    LTD_ISSUE_YR_INTEREST_PAYMENT,
     +                    MTG_DEBT_RETIREMENTS)
C***********************************************************************
C
         MTGLIF = INT(LTDLIFE)
         IF(LTDRTE /= 0.) THEN
            PIBIEN = (BALORG * LTDRTE/PAY_PERIODS_PER_YR)/
     +         (1. - (1./(1. + LTDRTE/PAY_PERIODS_PER_YR)**
     +                                (INT(PAY_PERIODS_PER_YR)*MTGLIF)))
         ELSE
            PIBIEN = BALORG/MAX(1.,PAY_PERIODS_PER_YR*MTGLIF)
         ENDIF
C     FOR DEVELOPMENT OF AMORTIZATION FACTOR ABOVE, SEE *FOOTNOTE BELOW;
         CURRENT_BAL = BALORG
         LTD_ISSUE_YR_INTEREST_BOOKED = 0.
         DO MO = 8, 12
            CURRENT_INTEREST = LTDRTE/PAY_PERIODS_PER_YR * CURRENT_BAL
            LTD_ISSUE_YR_INTEREST_BOOKED = LTD_ISSUE_YR_INTEREST_BOOKED
     +                                     + CURRENT_INTEREST
            CURRENT_BAL = MAX(0.,CURRENT_BAL-(PIBIEN-CURRENT_INTEREST))
         ENDDO
         MTG_DEBT_RETIREMENTS = BALORG - CURRENT_BAL
         LTD_ISSUE_YR_INTEREST_PAYMENT = LTD_ISSUE_YR_INTEREST_BOOKED
      RETURN
C
C  *FOOTNOTE ON AMORTIZATION FORMULA [ APPLICABLE ONLY FOR INTEGER M ]:
C     Using the notation that
C       r = annual interest rate
C       M = duration of loan in years
C       A = initial amount of loan
C       P = fixed annual payment of principal & interest
C       S(x,j,k) = the sum of powers x^i, where i ranges from j to k
C
C     One can show from the Present-Worth formula for payments made at
C     the end of years 1 through M
C       A = S(P * (1+r)^-i ,1,M)
C
C     and from the sum-of-powers formula
C       S(x      ,0,M-1) = (   x  ^ M - 1 ) / (   x      - 1 ) or
C       S(1/(1+r),0,M-1) = ( (1+r)^-M - 1 ) / ( (1+r)^-1 - 1 )
C
C     that
C       P = (A * r) / (1 - (1+r)^-M )
C
      END
