      MODULE DRILLING_REPT_PARAMETERS
      implicit none
         INTEGER (KIND=2), PARAMETER :: Drilling Account Name Width=50
         CHARACTER (LEN=Drilling Account Name Width)
     +                                             DRILLING_ACCOUNT_NAME
      CONTAINS
C***********************************************************************
         FUNCTION WVPA_REPORTING_NAME(ACCOUNT_DESCRIPTION,
     +                                WVPA_ACCTNO,
     +                                WVPA_ACCOUNTING_UNIT,
     +                                WVPA_SUB_ACCOUNT_NUMBER)
C***********************************************************************
C
            CHARACTER (LEN=Drilling Account Name Width)
     +                                        WVPA_REPORTING_NAME,
     +                                        WVPA_OUTPUT_REPORTING_NAME
            INTEGER WVPA_ACCTNO,
     +              WVPA_ACCOUNTING_UNIT,
     +              WVPA_SUB_ACCOUNT_NUMBER
            CHARACTER (LEN=*) :: ACCOUNT_DESCRIPTION
            CHARACTER*12 TEMP_STR
C
            WRITE(TEMP_STR,'(I4)') WVPA_ACCOUNTING_UNIT
            WVPA_OUTPUT_REPORTING_NAME = TEMP_STR
            WRITE(TEMP_STR,'(I6)') WVPA_ACCTNO
            WVPA_OUTPUT_REPORTING_NAME =TRIM(WVPA_OUTPUT_REPORTING_NAME)
     +                                           //' '//TEMP_STR
            WRITE(TEMP_STR,'(I7)') WVPA_SUB_ACCOUNT_NUMBER
            WVPA_OUTPUT_REPORTING_NAME =TRIM(WVPA_OUTPUT_REPORTING_NAME)
     +                                             //' '//TEMP_STR
            WVPA_OUTPUT_REPORTING_NAME =TRIM(WVPA_OUTPUT_REPORTING_NAME)
     +                                        //' '//ACCOUNT_DESCRIPTION
            WVPA_REPORTING_NAME = WVPA_OUTPUT_REPORTING_NAME
         END FUNCTION WVPA_REPORTING_NAME
C***********************************************************************
         FUNCTION WVPA_DEPART_EXP_REPORT_NAME(ACCOUNT_DESCRIPTION,
     +                                WVPA_ACCTNO,
     +                                WVPA_ACCOUNTING_UNIT,
     +                                WVPA_WORK_ORDER_NUMBER)
C***********************************************************************
C
            CHARACTER (LEN=Drilling Account Name Width)
     +                                      WVPA_DEPART_EXP_REPORT_NAME,
     +                                      WVPA_OUTPUT_REPORTING_NAME
            INTEGER WVPA_ACCTNO,
     +              WVPA_ACCOUNTING_UNIT
            CHARACTER (LEN=*) :: ACCOUNT_DESCRIPTION
            CHARACTER (LEN=10) :: WVPA_WORK_ORDER_NUMBER
            CHARACTER*12 TEMP_STR
C
            WRITE(TEMP_STR,'(I4)') WVPA_ACCOUNTING_UNIT
            WVPA_OUTPUT_REPORTING_NAME = TEMP_STR
            WRITE(TEMP_STR,'(I6)') WVPA_ACCTNO
            WVPA_OUTPUT_REPORTING_NAME =TRIM(WVPA_OUTPUT_REPORTING_NAME)
     +                                           //' '//TEMP_STR
            TEMP_STR = WVPA_WORK_ORDER_NUMBER(1:7)
            WVPA_OUTPUT_REPORTING_NAME =TRIM(WVPA_OUTPUT_REPORTING_NAME)
     +                                             //' '//TEMP_STR
            WVPA_OUTPUT_REPORTING_NAME =TRIM(WVPA_OUTPUT_REPORTING_NAME)
     +                                        //' '//ACCOUNT_DESCRIPTION
            WVPA_DEPART_EXP_REPORT_NAME = WVPA_OUTPUT_REPORTING_NAME
         END FUNCTION WVPA_DEPART_EXP_REPORT_NAME
C***********************************************************************
         FUNCTION WVPA_WORK_ORDER_REPORT_NAME(ACCOUNT_DESCRIPTION,
     +                                WVPA_ACCTNO,
     +                                WVPA_ACCOUNTING_UNIT,
     +                                WVPA_WORK_ORDER_NUMBER)
C***********************************************************************
C
            CHARACTER (LEN=Drilling Account Name Width)
     +                                      WVPA_WORK_ORDER_REPORT_NAME,
     +                                      WVPA_OUTPUT_REPORTING_NAME
            INTEGER WVPA_ACCTNO,
     +              WVPA_ACCOUNTING_UNIT
            CHARACTER (LEN=*) :: ACCOUNT_DESCRIPTION
            CHARACTER (LEN=10) :: WVPA_WORK_ORDER_NUMBER
            CHARACTER*12 TEMP_STR_ACCTNO
C
            WRITE(TEMP_STR_ACCTNO,'(I6)') WVPA_ACCTNO
            WVPA_OUTPUT_REPORTING_NAME =
     +                            TRIM(WVPA_WORK_ORDER_NUMBER(1:7))//','
            WVPA_OUTPUT_REPORTING_NAME =TRIM(WVPA_OUTPUT_REPORTING_NAME)
     +                                      //' '//TRIM(TEMP_STR_ACCTNO)
            WVPA_OUTPUT_REPORTING_NAME =TRIM(WVPA_OUTPUT_REPORTING_NAME)
     +                                        //' '//ACCOUNT_DESCRIPTION
            WVPA_WORK_ORDER_REPORT_NAME = WVPA_OUTPUT_REPORTING_NAME
         END FUNCTION WVPA_WORK_ORDER_REPORT_NAME
      END MODULE DRILLING_REPT_PARAMETERS