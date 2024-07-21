C     Last change: MSG 9/7/2011 11:52:50 AM
!     ******************************************************************
!     IN_OBJT.FOR
!     Created: 10/16/02 3:25:38 PM
!     Author : msg
!     Last change: MSG 10/22/02 1:56:11 PM
!     ******************************************************************

      SUBROUTINE IN_OBJECT
      use end_routine, only: end_program, er_message
      use filename_tracker
C
      INCLUDE 'SpinLib.MON'
      use kepcocom
      USE SIZECOM
      
      INTEGER*2 INUNIT,DELETE,IREC,I
      INTEGER*4 IOS,IOS_BASE
      LOGICAL*4 FILE_EXISTS,UNIT_10_OPEN
      LOGICAL*1 INIT_FILE_ACTIVE/.FALSE./,R_INIT_FILE_ACTIVE
      CHARACTER*28 COMPANY_NAME,CLASS_NAME*30,CLASS_TYPE*1,
     +             TEMP_CLASS_NAME*30,CLASS_TYPE_STR*15
      CHARACTER*5 BASE_FILE_NAME,OVERLAY_FAMILY_NAME,INITFIL
      CHARACTER*50 COMMENT
      CHARACTER*256 FILE_NAME
      CHARACTER*256 BASE_FILE_DIRECTORY
      CHARACTER*256 DATA_DRIVE,OUTPUT_DIRECTORY
C DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER*1024 RECLN
C DECLARATION FOR REAL VALUES
      INTEGER*2 YEAR,CLASS_NUM,LINKED_CLASS_ID,MOVE_TO_CLASS_ID
      INTEGER*2 INIT_VALUES,LRECL,LAST_DATA_NUMBER,
     +          VARIABLES_NOT_IN_LAST_NUMBER
      PARAMETER(LAST_DATA_NUMBER = 58,VARIABLES_NOT_IN_LAST_NUMBER=6)
C     PARAMETER(INIT_VALUES=34-27,
      PARAMETER(INIT_VALUES=LAST_DATA_NUMBER-52, ! INCLS. INIT_PLANNING_PEAK
     +          LRECL=1024)
c     +          LRECL=64 + 4*(LAST_DATA_NUMBER)+22)
      REAL VALUES43_45(3),VALUES_27(27),VALUES_11(11),
     +     VALUES47_51(5),VALUES53_58(6)
      REAL ALL_VALUES(LAST_DATA_NUMBER-VARIABLES_NOT_IN_LAST_NUMBER) ! CLASS INFO NOT COUNTED
      EQUIVALENCE (ALL_VALUES(1),VALUES_27(1)),
     +            (ALL_VALUES(28),VALUES_11(1)),
     +            (ALL_VALUES(39),VALUES43_45(1)),
     +            (ALL_VALUES(42),VALUES47_51(1)),
     +            (ALL_VALUES(47),VALUES53_58(1))
      CHARACTER*1 CURRENCY_DENOMINATION*20,
     +            REPORT_CLASS,
     +            FED_TAX_EXEMPT,
     +            ACCOUNT_ACTIVE
C DECLARATION FOR LOCALS
      CHARACTER*14 FILE_TYPE/'Initialization'/ 
      CHARACTER*19 MUNI_FILE_TYPE/'Municipal Init File'/
      INTEGER*2 BASE_YEAR
      REAL TOTAL_RESERVE_BALANCE,FIXED_ASSETS,
     +     SYSTEM_AVERAGE_RATE,
     +     SPECIAL_RESERVE_BALANCE,
     +     LAST_YEAR_SALES_REVENUE,
     +     CALAVERAS_RESERVE_BALANCE,
     +     ACCOUNTS_RECEIVABLE,
     +     ACCOUNTS_PAYABLE,
     +     STD_INTEREST_PAYMENT,
     +     FUEL_INVENTORY,
     +     DEFERRED_FUEL_BALANCE,
     +     ACQUISITION_PREMIUM,
     +     OUC_BOND_SINKING_FUND_BAL,
     +     OUC_DEBT_SERV_RESV_BAL,
     +     OUC_RENEWAL_REPLACEMENT_BAL,
     +     OUC_OP_CONSTRUCT_FUND_BAL,
     +     OUC_SP_CONSTRUCT_FUND_BAL,
     +     OUC_SR_CAPTLZED_INT_FD_BAL,
     +     OUC_JR_CAPTLZED_INT_FD_BAL,
     +     OUC_MINI_BOND_PRIN_SKFUND_BAL,
     +     OUC_MINI_BOND_INT_SKFUND_BAL,
     +     OUC_LIABILITY_REDUCTION_BAL,
     +     OUC_FUEL_INVENTORY_BAL,
     +     OUC_MATERIALS_INVENTORY_BAL,
     +     OUC_PREPAID_EXP_BAL,
     +     OUC_BASE_EL_STAB_FUND_BAL,
     +     OUC_BASE_WATER_STAB_FUND_BAL,
     +     OUC_FUEL_STAB_FUND_BAL,
     +     OUC_CUST_RETENTION_FUND_BAL,
     +     OUC_TTL_UNRESTR_CUR_LIAB_BAL,
     +     OUC_CASH_POOLED_INVEST_BAL,
     +     OUC_OTHER_LIAB_AND_CRED_BAL,
     +     OUC_MINI_BOND_DEBT_BAL,
     +     OUC_OTHER_CURRENT_ASSETS_BAL,
     +     OUC_BOY_BOND_PRINCIPL_DUE,
     +     FASB_87_INTANGIBLE_ASSETS,
     +     OTHER_COMPREHENSIVE_INCOME_BALANCE,
     +     OptRevenueTaxBasis
     
C              
      INTEGER*2 STD_INTEREST_VECTOR,            !93
     +          LTD_INTEREST_VECTOR,
     +          PS_DIVIDENDS_VECTOR,
     +          STI_INTEREST_VECTOR,
     +          LTI_INTEREST_VECTOR,
     +          CUSTOMER_DEPOSIT_INTREST_VECTOR,
     +          DECOM_FUND_INTEREST_VECTOR,
     +          RETIREMENT_FUND_INTEREST_VECTOR,
     +          CURRENCY_CONVERSION_VECTOR,
     +          COMPANY_ID                       ! 105
C 
      REAL MUNI_INIT_VALUES(5)
      CHARACTER*2 INITOL/'BC'/
      INTEGER*2 NUM_OF_OL_INIT_CLASSES/0/,
     +          MAX_OL_INIT_CLASS_ID_NUM/0/
      INTEGER*2 NUM_OF_BC_INIT_CLASSES/0/,
     +          MAX_BC_INIT_CLASS_ID_NUM/0/
      INTEGER*2 R_NUM_OF_INIT_CLASSES,R_MAX_INIT_CLASS_NUM,
     +          R_INIT_CLASS_POINTERS(*)
      CHARACTER*1 BC_CLASS_TYPE(1024)/1024*'X'/
      CHARACTER*1 OL_CLASS_TYPE(1024)/1024*'X'/
      INTEGER*2 BC_INIT_CLASS_POINTER(:),
     +          OL_INIT_CLASS_POINTER(:),
     +          BC_CLASS_LINKAGE(:),
     +          OL_CLASS_LINKAGE(:),
     +          TEMP_ASSET_CLASS_POINTER(:),
     +          SBU_LINKED_COUNTER(:)
      ALLOCATABLE :: BC_INIT_CLASS_POINTER,
     +               OL_INIT_CLASS_POINTER,
     +               BC_CLASS_LINKAGE,
     +               OL_CLASS_LINKAGE,
     +               TEMP_ASSET_CLASS_POINTER,
     +               SBU_LINKED_COUNTER
      SAVE BC_INIT_CLASS_POINTER,
     +     OL_INIT_CLASS_POINTER,
     +     BC_CLASS_LINKAGE,
     +     OL_CLASS_LINKAGE
      INTEGER*2 R_ASSET_CLASS,R_LINKED_CLASS,POINTR
      INTEGER*2 BC_MAX_LINKED_SBU,OL_MAX_LINKED_SBU,R_MAX_LINKED_SBU
      SAVE BC_MAX_LINKED_SBU,OL_MAX_LINKED_SBU
      INTEGER*2 BC_MASTER_DEFINED_CLASS_IDS(:),BC_MAX_DEFINED_ID,
     +          BC_PARENT_CLASS_ID_NUM
      ALLOCATABLE :: BC_MASTER_DEFINED_CLASS_IDS
      SAVE BC_MASTER_DEFINED_CLASS_IDS,BC_MAX_DEFINED_ID,
     +     BC_PARENT_CLASS_ID_NUM
      INTEGER*2 OL_MASTER_DEFINED_CLASS_IDS(:),OL_MAX_DEFINED_ID,
     +          OL_PARENT_CLASS_ID_NUM,DEACTIVATE_IN_YEAR,
     +          ACTIVATE_IN_YEAR
      ALLOCATABLE :: OL_MASTER_DEFINED_CLASS_IDS
      SAVE OL_MASTER_DEFINED_CLASS_IDS,OL_MAX_DEFINED_ID,
     +     OL_PARENT_CLASS_ID_NUM
      INTEGER*2 R_PARENT_CLASS_ID,PARENT_CLASS_ID_NUM
      SAVE PARENT_CLASS_ID_NUM
      INTEGER*2 OL_ELIMINAITON_CLASS_ID_NUM/-30000/,
     +          BC_ELIMINAITON_CLASS_ID_NUM/-30000/,
     +          R_ELIMINAITON_CLASS_ID_NUM
      LOGICAL*1 DUPLICATE_ID_FOUND
      INTEGER*2 DUPLICATE_CLASS_ID_CHECK(0:1024)
      CHARACTER*1 COMPANY_CODE,
     +            PLANT_ID,
     +            UNIT_ID
      LOGICAL*1 LAHEY_LF95
      CHARACTER*30 SCREEN_OUTPUT
C***********************************************************************
C
C          SUBROUTINE TO CONVERT METAFILE FILES TO DIRECT ACESS BINARY
C          COPYRIGHT (C) 1983, 84, 85  M.S. GERBER & ASSOCIATES, INC.
C
C***********************************************************************
C
C CONVERT THE INITIALIZATION FILE
C***********************************************************************
      ENTRY IN_MAKEBIN
C***********************************************************************
         DATA_DRIVE = OUTPUT_DIRECTORY()
         BASE_FILE_NAME = INITFIL()
         FILE_NAME = trim(BASE_FILE_DIRECTORY())//
     +                             "INB"//trim(BASE_FILE_NAME)//".DAT"
         INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
         OPEN(11,FILE=trim(DATA_DRIVE)//"BCINITAL.BIN",
     +                      ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
C
         DELETE = 1
C
         LINKED_CLASS_ID = 0
         CLASS_NUM = 0
         CLASS_TYPE = 'P'
         CLASS_NAME = COMPANY_NAME()
         ALL_VALUES = -.000001 
         VALUES_11(8) = -9999. ! TRAP FOR BLANK VALUE
         VALUES_11(9) = -9999.
         VALUES_11(10) = -9999.
         ALL_VALUES(51) = 1.
         ALL_VALUES(52) = -9999.
         BC_MAX_LINKED_SBU = 0
         BC_MAX_DEFINED_ID = 0
         BC_PARENT_CLASS_ID_NUM = 0
         PARENT_CLASS_ID_NUM = 1
         DUPLICATE_ID_FOUND = .FALSE.
         INIT_FILE_ACTIVE = FILE_EXISTS
         IF(FILE_EXISTS) THEN
            IF(LAHEY_LF95()) THEN
               SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//BASE_FILE_NAME
               CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
            ELSE
               CALL MG_LOCATE_WRITE(16,30,BASE_FILE_NAME,ALL_VERSIONS,0)
               CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,
     +                                                   ALL_VERSIONS,0)
            ENDIF
            ALLOCATE(TEMP_ASSET_CLASS_POINTER(1024),
     +               SBU_LINKED_COUNTER(1024),
     +               BC_CLASS_LINKAGE(0:1024),
     +               BC_MASTER_DEFINED_CLASS_IDS(0:1024))
            TEMP_ASSET_CLASS_POINTER = 0
            SBU_LINKED_COUNTER = 0
            BC_CLASS_LINKAGE = -99
            BC_MASTER_DEFINED_CLASS_IDS = -99
            DUPLICATE_CLASS_ID_CHECK = -99
C
            OPEN(10,FILE=FILE_NAME)
            READ(10,*,IOSTAT=IOS) DELETE
            IREC = 0
            DO
               MOVE_TO_CLASS_ID = -99
               ACCOUNTS_RECEIVABLE = 0.
               ACCOUNTS_PAYABLE = 0.
               STD_INTEREST_PAYMENT = 0.
               FUEL_INVENTORY = 0.
               DEFERRED_FUEL_BALANCE = 0.
               ACQUISITION_PREMIUM = 0.
               REPORT_CLASS = 'X'
               FED_TAX_EXEMPT = 'X'
               DEACTIVATE_IN_YEAR = 2200
               ACTIVATE_IN_YEAR = 1995
               OUC_BOND_SINKING_FUND_BAL = 0
               OUC_DEBT_SERV_RESV_BAL = 0
               OUC_RENEWAL_REPLACEMENT_BAL = 0
               OUC_OP_CONSTRUCT_FUND_BAL = 0
               OUC_SP_CONSTRUCT_FUND_BAL = 0
               OUC_SR_CAPTLZED_INT_FD_BAL = 0
               OUC_JR_CAPTLZED_INT_FD_BAL = 0
               OUC_MINI_BOND_PRIN_SKFUND_BAL = 0
               OUC_MINI_BOND_INT_SKFUND_BAL = 0
               OUC_LIABILITY_REDUCTION_BAL = 0
               OUC_FUEL_INVENTORY_BAL = 0
               OUC_MATERIALS_INVENTORY_BAL = 0
               OUC_PREPAID_EXP_BAL = 0
               OUC_BASE_EL_STAB_FUND_BAL = 0
               OUC_BASE_WATER_STAB_FUND_BAL = 0
               OUC_FUEL_STAB_FUND_BAL = 0
               OUC_CUST_RETENTION_FUND_BAL = 0
               OUC_TTL_UNRESTR_CUR_LIAB_BAL = 0
               OUC_CASH_POOLED_INVEST_BAL = 0
               OUC_OTHER_LIAB_AND_CRED_BAL = 0
               OUC_MINI_BOND_DEBT_BAL = 0
               OUC_OTHER_CURRENT_ASSETS_BAL = 0
               OUC_BOY_BOND_PRINCIPL_DUE = 0.
               ACCOUNT_ACTIVE = 'A'
               COMPANY_CODE = " "
               PLANT_ID = " "
               UNIT_ID = " "
               STD_INTEREST_VECTOR = 0.            !93
               LTD_INTEREST_VECTOR = 0.
               PS_DIVIDENDS_VECTOR = 0.
               STI_INTEREST_VECTOR = 0.
               LTI_INTEREST_VECTOR = 0.
               CUSTOMER_DEPOSIT_INTREST_VECTOR = 0.
               DECOM_FUND_INTEREST_VECTOR = 0.
               RETIREMENT_FUND_INTEREST_VECTOR = 0.
               CURRENCY_CONVERSION_VECTOR = 0.
               FASB_87_INTANGIBLE_ASSETS = 0.
               OTHER_COMPREHENSIVE_INCOME_BALANCE = 0.
               OptRevenueTaxBasis = 0.
               COMPANY_ID = -99                       ! 105
C              
C
               DO
                  READ(10,1000,IOSTAT=IOS) RECLN
                  IF(IOS /=0) EXIT
                  IF(RECLN(1:1) == '7') EXIT
                  RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
     +                              //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
                  CURRENCY_DENOMINATION = 'Dollars'
                  READ(RECLN,*,IOSTAT=IOS) DELETE,
     +                               VALUES_27,COMMENT,YEAR,
     +                               VALUES_11,CLASS_NAME,CLASS_NUM,
     +                               VALUES43_45,CLASS_TYPE_STR,
     +                               VALUES47_51,
     +                               LINKED_CLASS_ID,VALUES53_58,
     +                               CURRENCY_DENOMINATION,
     +                               REPORT_CLASS,
     +                               FED_TAX_EXEMPT,
     +                               DEACTIVATE_IN_YEAR,
     +                               ACTIVATE_IN_YEAR,
     +                               ACCOUNTS_RECEIVABLE,
     +                               ACCOUNTS_PAYABLE,
     +                               COMPANY_CODE,
     +                               PLANT_ID,
     +                               UNIT_ID,
     +                               STD_INTEREST_PAYMENT,
     +                               FUEL_INVENTORY,
     +                               DEFERRED_FUEL_BALANCE,
     +                               MOVE_TO_CLASS_ID,
     +                               OUC_BOND_SINKING_FUND_BAL,     ! 73 Pension Liability
     +                               OUC_DEBT_SERV_RESV_BAL,        !    Deferred Gain
     +                               OUC_RENEWAL_REPLACEMENT_BAL,   ! 75 Storm Reserve
     +                               OUC_OP_CONSTRUCT_FUND_BAL,     !    Accrued Vacation Pay
     +                               OUC_SP_CONSTRUCT_FUND_BAL,     !    RB INCLUDE Accumulated Deferred Tax (Dr)
     +                               OUC_SR_CAPTLZED_INT_FD_BAL,    !    RB INCLUDE Deferred Revenues
     +                               OUC_JR_CAPTLZED_INT_FD_BAL,    !    RB EXCLUDED Pension Liability
     +                               OUC_MINI_BOND_PRIN_SKFUND_BAL, !80 RB EXCLUDED Deferred Gain
     +                               OUC_MINI_BOND_INT_SKFUND_BAL,  !   RB EXCLUDED Storm Reserve
     +                               OUC_LIABILITY_REDUCTION_BAL,   !   RB EXCLUDED Accrued Vacation Pay
     +                               OUC_FUEL_INVENTORY_BAL,        !   Other Materials & Supplies
     +                               OUC_MATERIALS_INVENTORY_BAL,   !   Gas Storage in ground
     +                               OUC_PREPAID_EXP_BAL,           ! 85 Deferred Purchase Gas 
     +                               OUC_BASE_EL_STAB_FUND_BAL,     ! 86 Deferred Purchase Power
     +                               ACCOUNT_ACTIVE,                ! 87
     +                               ACQUISITION_PREMIUM,           ! 88
     +                               OUC_BASE_WATER_STAB_FUND_BAL,  ! 89 FASB 143/ARO NUC DECOM ASSET BALANCE THE OUC COUNT IS OFF ONE
     +                               OUC_FUEL_STAB_FUND_BAL,        ! 90 FASB 143/ARO NUC DECOM LIAB 
     +                               OUC_CUST_RETENTION_FUND_BAL,   ! 91 PROPERTY TAX BASIS BASE YEAR
     +                               OUC_TTL_UNRESTR_CUR_LIAB_BAL,  ! 92 PROPERTY TAX BASIS BY-1
     +                               STD_INTEREST_VECTOR,            !93
     +                               LTD_INTEREST_VECTOR,
     +                               PS_DIVIDENDS_VECTOR,
     +                               STI_INTEREST_VECTOR,
     +                               LTI_INTEREST_VECTOR,
     +                               CUSTOMER_DEPOSIT_INTREST_VECTOR,
     +                               DECOM_FUND_INTEREST_VECTOR,
     +                               RETIREMENT_FUND_INTEREST_VECTOR,
     +                               CURRENCY_CONVERSION_VECTOR,
     +                               FASB_87_INTANGIBLE_ASSETS,
     +                               OTHER_COMPREHENSIVE_INCOME_BALANCE,
     +                               OptRevenueTaxBasis,
     +                               COMPANY_ID                       ! 105
                  CLASS_TYPE = CLASS_TYPE_STR(1:1)
C              
  
                  IF(IOS /= 0) THEN
                     GOTO 200
                  ENDIF
                  IF(INDEX(REPORT_CLASS,'Y') == 0 .AND.
     +                  INDEX(REPORT_CLASS,'N') == 0) REPORT_CLASS = 'Y'
                  IF(INDEX(FED_TAX_EXEMPT,'Y') == 0 .AND.
     +              INDEX(FED_TAX_EXEMPT,'N') == 0) FED_TAX_EXEMPT = 'N'
                  IF(DELETE >= 8 .OR. ACCOUNT_ACTIVE == 'N' .OR.
     +                                           CLASS_TYPE == 'N') THEN
                     IF(DUPLICATE_CLASS_ID_CHECK(CLASS_NUM) == -99) THEN
                        IF(CLASS_NUM+1 > 0) THEN
                           IF(TEMP_ASSET_CLASS_POINTER(CLASS_NUM+1) ==
     +                                                           0) THEN
                              NUM_OF_BC_INIT_CLASSES =
     +                                        NUM_OF_BC_INIT_CLASSES + 1
                              TEMP_ASSET_CLASS_POINTER(CLASS_NUM+1) =
     +                                            NUM_OF_BC_INIT_CLASSES
                              MAX_BC_INIT_CLASS_ID_NUM=MAX(CLASS_NUM+1,
     +                                         MAX_BC_INIT_CLASS_ID_NUM)
                           ENDIF
                        ENDIF
                     endif
                  ELSE
                     IF(DUPLICATE_CLASS_ID_CHECK(CLASS_NUM) == -99) THEN
                        IF(CLASS_TYPE /=  'N') THEN
                           DUPLICATE_CLASS_ID_CHECK(CLASS_NUM) = 1
                        ENDIF
                     ELSEIF(CLASS_TYPE /=  'N') THEN
                        WRITE(4,*) 'Duplicate Class ID found for',
     +                                          CLASS_NUM,' ',CLASS_NAME
                        DUPLICATE_ID_FOUND = .TRUE.
                     ENDIF
                     IF(CLASS_TYPE /= 'E') THEN !  .AND. CLASS_TYPE /= 'C') THEN
                        IF(CLASS_NUM+1 > 0) THEN
                           IF(TEMP_ASSET_CLASS_POINTER(CLASS_NUM+1) ==
     +                                                           0) THEN
                              NUM_OF_BC_INIT_CLASSES =
     +                                        NUM_OF_BC_INIT_CLASSES + 1
                              TEMP_ASSET_CLASS_POINTER(CLASS_NUM+1) =
     +                                            NUM_OF_BC_INIT_CLASSES
                              MAX_BC_INIT_CLASS_ID_NUM=MAX(CLASS_NUM+1,
     +                                         MAX_BC_INIT_CLASS_ID_NUM)
                           ENDIF
                        ENDIF
c                  CALL SET_INIT_CLASSES(CLASS_NUM,
c    +                                      NUM_OF_BC_INIT_CLASSES,
c    +                                       MAX_BC_INIT_CLASS_ID_NUM,
c    +                                      TEMP_ASSET_CLASS_POINTER)
                        IF(CLASS_TYPE == 'P') THEN
c                          CALL STORE_CONSOLIDATED_INIT_RECORD(IREC)
                           PARENT_CLASS_ID_NUM = CLASS_NUM ! + 1
                        ELSE !IF(CLASS_TYPE=='B' .OR. CLASS_TYPE=='R' .OR.
C    +                                             CLASS_TYPE=='S') THEN
                           IF(LINKED_CLASS_ID+1 > 0) THEN
                              BC_CLASS_LINKAGE(NUM_OF_BC_INIT_CLASSES) =
     +                                                   LINKED_CLASS_ID
                              BC_CLASS_TYPE(NUM_OF_BC_INIT_CLASSES) =
     +                                                        CLASS_TYPE
                              SBU_LINKED_COUNTER(LINKED_CLASS_ID+1) = 1+
     +                             SBU_LINKED_COUNTER(LINKED_CLASS_ID+1)
                              BC_MAX_LINKED_SBU = MAX(BC_MAX_LINKED_SBU,
     +                            SBU_LINKED_COUNTER(LINKED_CLASS_ID+1))
                           ENDIF
                        ENDIF
                     ELSEIF(CLASS_TYPE == 'E') THEN !  .AND. CLASS_TYPE /= 'C') THEN
                        IF(CLASS_NUM+1 > 0) THEN
                           IF(TEMP_ASSET_CLASS_POINTER(CLASS_NUM+1) ==
     +                                                           0) THEN
                              NUM_OF_BC_INIT_CLASSES =
     +                                        NUM_OF_BC_INIT_CLASSES + 1
                              TEMP_ASSET_CLASS_POINTER(CLASS_NUM+1) =
     +                                            NUM_OF_BC_INIT_CLASSES
                              MAX_BC_INIT_CLASS_ID_NUM=MAX(CLASS_NUM+1,
     +                                         MAX_BC_INIT_CLASS_ID_NUM)
                           ENDIF
                        ENDIF
                     ENDIF
C
C ESTABLISH A LIST OF DEFINED CLASSES
C
C                     IF(CLASS_TYPE /= 'C') THEN
                        BC_MAX_DEFINED_ID =
     +                                  MAX(BC_MAX_DEFINED_ID,CLASS_NUM)
                        BC_MASTER_DEFINED_CLASS_IDS(CLASS_NUM)=CLASS_NUM
                        IF(CLASS_TYPE == 'P') THEN
                           BC_PARENT_CLASS_ID_NUM = CLASS_NUM
                        ENDIF
C                     ENDIF
                     IF(CLASS_TYPE == 'E')
     +                           BC_ELIMINAITON_CLASS_ID_NUM = CLASS_NUM
                  ENDIF
                  IREC = IREC + 1
                  WRITE(11,REC=IREC) DELETE,CLASS_NAME,
     +                               CLASS_NUM,CLASS_TYPE,
     +                               CLASS_TYPE_STR,
     +                               LINKED_CLASS_ID,
     +                               REPORT_CLASS,
     +                               MOVE_TO_CLASS_ID,
     +                               ACCOUNT_ACTIVE,                ! 87 
     +                               COMPANY_ID,                       ! 105
     +                               ALL_VALUES,
     +                               CURRENCY_DENOMINATION,
     +                               FED_TAX_EXEMPT,
     +                               DEACTIVATE_IN_YEAR,
     +                               ACTIVATE_IN_YEAR,
     +                               ACCOUNTS_RECEIVABLE,
     +                               ACCOUNTS_PAYABLE,
     +                               STD_INTEREST_PAYMENT,
     +                               FUEL_INVENTORY,
     +                               DEFERRED_FUEL_BALANCE,
     +                               OUC_BOND_SINKING_FUND_BAL,
     +                               OUC_DEBT_SERV_RESV_BAL,
     +                               OUC_RENEWAL_REPLACEMENT_BAL,
     +                               OUC_OP_CONSTRUCT_FUND_BAL,
     +                               OUC_SP_CONSTRUCT_FUND_BAL,
     +                               OUC_SR_CAPTLZED_INT_FD_BAL,
     +                               OUC_JR_CAPTLZED_INT_FD_BAL,
     +                               OUC_MINI_BOND_PRIN_SKFUND_BAL,
     +                               OUC_MINI_BOND_INT_SKFUND_BAL,
     +                               OUC_LIABILITY_REDUCTION_BAL,
     +                               OUC_FUEL_INVENTORY_BAL,
     +                               OUC_MATERIALS_INVENTORY_BAL,
     +                               OUC_PREPAID_EXP_BAL,
     +                               OUC_BASE_EL_STAB_FUND_BAL,
     +                               ACQUISITION_PREMIUM,
     +                               OUC_BASE_WATER_STAB_FUND_BAL,
     +                               OUC_FUEL_STAB_FUND_BAL,
     +                               OUC_CUST_RETENTION_FUND_BAL,
     +                               OUC_TTL_UNRESTR_CUR_LIAB_BAL,
     +                               STD_INTEREST_VECTOR,            !93
     +                               LTD_INTEREST_VECTOR,
     +                               PS_DIVIDENDS_VECTOR,
     +                               STI_INTEREST_VECTOR,
     +                               LTI_INTEREST_VECTOR,
     +                               CUSTOMER_DEPOSIT_INTREST_VECTOR,
     +                               DECOM_FUND_INTEREST_VECTOR,
     +                               RETIREMENT_FUND_INTEREST_VECTOR,
     +                               CURRENCY_CONVERSION_VECTOR,
     +                               FASB_87_INTANGIBLE_ASSETS,
     +                               OTHER_COMPREHENSIVE_INCOME_BALANCE,
     +                               OptRevenueTaxBasis
C

               ENDDO
               IF(IOS /= 0) EXIT
            ENDDO
            CLOSE(10)
            IF(MAX_BC_INIT_CLASS_ID_NUM > 0) THEN
               ALLOCATE(BC_INIT_CLASS_POINTER(MAX_BC_INIT_CLASS_ID_NUM))
               BC_INIT_CLASS_POINTER =
     +              TEMP_ASSET_CLASS_POINTER(1:MAX_BC_INIT_CLASS_ID_NUM)
            ENDIF
            DEALLOCATE(TEMP_ASSET_CLASS_POINTER,SBU_LINKED_COUNTER)
         ELSE
            WRITE(11,REC=1) DELETE,CLASS_NAME,CLASS_NUM,CLASS_TYPE,
     +                                    CLASS_TYPE_STR,
     +                                    LINKED_CLASS_ID,
     +                                    REPORT_CLASS,
     +                                    MOVE_TO_CLASS_ID,
     +                                    ACCOUNT_ACTIVE,                ! 87 
     +                                    COMPANY_ID,                       ! 105
     +                                    ALL_VALUES,
     +                                    CURRENCY_DENOMINATION,
     +                                    FED_TAX_EXEMPT,
     +                                    DEACTIVATE_IN_YEAR,
     +                                    ACTIVATE_IN_YEAR,
     +                                    ACCOUNTS_RECEIVABLE,
     +                                    ACCOUNTS_PAYABLE,
     +                                    STD_INTEREST_PAYMENT,
     +                                    FUEL_INVENTORY,
     +                                    DEFERRED_FUEL_BALANCE,
     +                                    OUC_BOND_SINKING_FUND_BAL,
     +                                    OUC_DEBT_SERV_RESV_BAL,    
     +                                    OUC_RENEWAL_REPLACEMENT_BAL,
     +                                    OUC_OP_CONSTRUCT_FUND_BAL,
     +                                    OUC_SP_CONSTRUCT_FUND_BAL,
     +                                    OUC_SR_CAPTLZED_INT_FD_BAL,
     +                                    OUC_JR_CAPTLZED_INT_FD_BAL,
     +                                    OUC_MINI_BOND_PRIN_SKFUND_BAL,
     +                                    OUC_MINI_BOND_INT_SKFUND_BAL,
     +                                    OUC_LIABILITY_REDUCTION_BAL,
     +                                    OUC_FUEL_INVENTORY_BAL,
     +                                    OUC_MATERIALS_INVENTORY_BAL,
     +                                    OUC_PREPAID_EXP_BAL,
     +                                    OUC_BASE_EL_STAB_FUND_BAL,
     +                                    ACQUISITION_PREMIUM,
     +                                    OUC_BASE_WATER_STAB_FUND_BAL,
     +                                    OUC_FUEL_STAB_FUND_BAL,
     +                                    OUC_CUST_RETENTION_FUND_BAL,
     +                                    OUC_TTL_UNRESTR_CUR_LIAB_BAL,
     +                                    STD_INTEREST_VECTOR,            !93
     +                                    LTD_INTEREST_VECTOR,
     +                                    PS_DIVIDENDS_VECTOR,
     +                                    STI_INTEREST_VECTOR,
     +                                    LTI_INTEREST_VECTOR,
     +                                  CUSTOMER_DEPOSIT_INTREST_VECTOR,
     +                                    DECOM_FUND_INTEREST_VECTOR,
     +                                  RETIREMENT_FUND_INTEREST_VECTOR,
     +                                    CURRENCY_CONVERSION_VECTOR,
     +                               FASB_87_INTANGIBLE_ASSETS,
     +                               OTHER_COMPREHENSIVE_INCOME_BALANCE,
     +                               OptRevenueTaxBasis
C

         ENDIF
         CLOSE(11)

      RETURN
C
C OVERLAY THE INITIALIZATION FILE
C
C***********************************************************************
      ENTRY IN_MAKEOVL(OVERLAY_FAMILY_NAME)
C***********************************************************************
         DATA_DRIVE = OUTPUT_DIRECTORY()
         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
            CALL LOCATE(10,51)
         ENDIF
         FILE_NAME = trim(DATA_DRIVE)//"INO"//
     +                               trim(OVERLAY_FAMILY_NAME)//".DAT"
         OPEN(10,FILE=FILE_NAME)
         READ(10,*) DELETE
         INUNIT = 12
         IF(INITOL == 'BC') THEN
            OPEN(11,FILE=trim(DATA_DRIVE)//"BCINITAL.BIN",
     +                                       ACCESS="DIRECT",RECL=LRECL)
            INUNIT = 11
         ENDIF
         OPEN(12,FILE=trim(DATA_DRIVE)//"OLINITAL.BIN",
     +                      ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
         NUM_OF_OL_INIT_CLASSES = 0
         MAX_OL_INIT_CLASS_ID_NUM = 0
         OL_MAX_DEFINED_ID = 0
         OL_ELIMINAITON_CLASS_ID_NUM = -30000
         IF(ALLOCATED(OL_INIT_CLASS_POINTER))
     +                                 DEALLOCATE(OL_INIT_CLASS_POINTER)
         IF(ALLOCATED(OL_CLASS_LINKAGE)) DEALLOCATE(OL_CLASS_LINKAGE)
         IF(ALLOCATED(OL_MASTER_DEFINED_CLASS_IDS))
     +                           DEALLOCATE(OL_MASTER_DEFINED_CLASS_IDS)
         ALLOCATE(TEMP_ASSET_CLASS_POINTER(1024),
     +            SBU_LINKED_COUNTER(1024),
     +            OL_CLASS_LINKAGE(0:1024),
     +            OL_MASTER_DEFINED_CLASS_IDS(0:1024))
         TEMP_ASSET_CLASS_POINTER = 0
         SBU_LINKED_COUNTER = 0
         OL_MASTER_DEFINED_CLASS_IDS = -99
         OL_CLASS_LINKAGE = -99
         DUPLICATE_CLASS_ID_CHECK = -99
         DUPLICATE_ID_FOUND = .FALSE.
         OL_MAX_LINKED_SBU = 0
         IREC = 0
C
         DO
            DO
               READ(10,1000,IOSTAT=IOS) RECLN
               IF(RECLN(1:1) == '7') EXIT
               IREC = IREC + 1
               READ(INUNIT,REC=IREC,IOSTAT=IOS_BASE) 
     +                            DELETE,CLASS_NAME,CLASS_NUM,
     +                            CLASS_TYPE,
     +                            CLASS_TYPE_STR,
     +                            LINKED_CLASS_ID,
     +                            REPORT_CLASS,
     +                            MOVE_TO_CLASS_ID,
     +                            ACCOUNT_ACTIVE,                ! 87 
     +                            COMPANY_ID,                       ! 105
     +                            ALL_VALUES,
     +                            CURRENCY_DENOMINATION,
     +                            FED_TAX_EXEMPT,
     +                            DEACTIVATE_IN_YEAR,
     +                            ACTIVATE_IN_YEAR,
     +                            ACCOUNTS_RECEIVABLE,
     +                            ACCOUNTS_PAYABLE,
     +                            STD_INTEREST_PAYMENT,
     +                            FUEL_INVENTORY,
     +                            DEFERRED_FUEL_BALANCE,
     +                            OUC_BOND_SINKING_FUND_BAL,
     +                            OUC_DEBT_SERV_RESV_BAL,
     +                            OUC_RENEWAL_REPLACEMENT_BAL,
     +                            OUC_OP_CONSTRUCT_FUND_BAL,
     +                            OUC_SP_CONSTRUCT_FUND_BAL,
     +                            OUC_SR_CAPTLZED_INT_FD_BAL,
     +                            OUC_JR_CAPTLZED_INT_FD_BAL,
     +                            OUC_MINI_BOND_PRIN_SKFUND_BAL,
     +                            OUC_MINI_BOND_INT_SKFUND_BAL,
     +                            OUC_LIABILITY_REDUCTION_BAL,
     +                            OUC_FUEL_INVENTORY_BAL,
     +                            OUC_MATERIALS_INVENTORY_BAL,
     +                            OUC_PREPAID_EXP_BAL,
     +                            OUC_BASE_EL_STAB_FUND_BAL,
     +                            ACQUISITION_PREMIUM,
     +                            OUC_BASE_WATER_STAB_FUND_BAL,
     +                            OUC_FUEL_STAB_FUND_BAL,
     +                            OUC_CUST_RETENTION_FUND_BAL,
     +                            OUC_TTL_UNRESTR_CUR_LIAB_BAL,
     +                            STD_INTEREST_VECTOR,            !93
     +                            LTD_INTEREST_VECTOR,
     +                            PS_DIVIDENDS_VECTOR,
     +                            STI_INTEREST_VECTOR,
     +                            LTI_INTEREST_VECTOR,
     +                            CUSTOMER_DEPOSIT_INTREST_VECTOR,
     +                            DECOM_FUND_INTEREST_VECTOR,
     +                            RETIREMENT_FUND_INTEREST_VECTOR,
     +                            CURRENCY_CONVERSION_VECTOR,
     +                            FASB_87_INTANGIBLE_ASSETS,
     +                            OTHER_COMPREHENSIVE_INCOME_BALANCE,
     +                            OptRevenueTaxBasis
C

               IF(IOS_BASE /= 0) EXIT
               IF(IOS == 0) THEN
                  RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
                  READ(RECLN,*,ERR=200) DELETE,VALUES_27,COMMENT,YEAR,
     +                            VALUES_11,TEMP_CLASS_NAME,CLASS_NUM,
     +                           VALUES43_45,CLASS_TYPE_STR,VALUES47_51,
     +                            LINKED_CLASS_ID,VALUES53_58,
     +                            CURRENCY_DENOMINATION,
     +                            REPORT_CLASS,
     +                            FED_TAX_EXEMPT,
     +                            DEACTIVATE_IN_YEAR,
     +                            ACTIVATE_IN_YEAR,
     +                            ACCOUNTS_RECEIVABLE,
     +                            ACCOUNTS_PAYABLE,
     +                            COMPANY_CODE,
     +                            PLANT_ID,
     +                            UNIT_ID,
     +                            STD_INTEREST_PAYMENT,
     +                            FUEL_INVENTORY,
     +                            DEFERRED_FUEL_BALANCE,
     +                            MOVE_TO_CLASS_ID,
     +                            OUC_BOND_SINKING_FUND_BAL,
     +                            OUC_DEBT_SERV_RESV_BAL,
     +                            OUC_RENEWAL_REPLACEMENT_BAL,
     +                            OUC_OP_CONSTRUCT_FUND_BAL,
     +                            OUC_SP_CONSTRUCT_FUND_BAL,
     +                            OUC_SR_CAPTLZED_INT_FD_BAL,
     +                            OUC_JR_CAPTLZED_INT_FD_BAL,
     +                            OUC_MINI_BOND_PRIN_SKFUND_BAL,
     +                            OUC_MINI_BOND_INT_SKFUND_BAL,
     +                            OUC_LIABILITY_REDUCTION_BAL,
     +                            OUC_FUEL_INVENTORY_BAL,
     +                            OUC_MATERIALS_INVENTORY_BAL,
     +                            OUC_PREPAID_EXP_BAL,
     +                            OUC_BASE_EL_STAB_FUND_BAL,
     +                            ACCOUNT_ACTIVE,                ! 87 
     +                            ACQUISITION_PREMIUM,           ! 88
     +                            OUC_BASE_WATER_STAB_FUND_BAL,
     +                            OUC_FUEL_STAB_FUND_BAL,
     +                            OUC_CUST_RETENTION_FUND_BAL,
     +                            OUC_TTL_UNRESTR_CUR_LIAB_BAL,
     +                            STD_INTEREST_VECTOR,            !93
     +                            LTD_INTEREST_VECTOR,
     +                            PS_DIVIDENDS_VECTOR,
     +                            STI_INTEREST_VECTOR,
     +                            LTI_INTEREST_VECTOR,
     +                            CUSTOMER_DEPOSIT_INTREST_VECTOR,
     +                            DECOM_FUND_INTEREST_VECTOR,
     +                            RETIREMENT_FUND_INTEREST_VECTOR,
     +                            CURRENCY_CONVERSION_VECTOR,
     +                            FASB_87_INTANGIBLE_ASSETS,
     +                            OTHER_COMPREHENSIVE_INCOME_BALANCE,
     +                            OptRevenueTaxBasis,
     +                            COMPANY_ID                       ! 105
C              
                  CLASS_TYPE = CLASS_TYPE_STR(1:1)
               ENDIF
               IF(DELETE >= 8 .OR. ACCOUNT_ACTIVE == 'N' .OR.
     +                                           CLASS_TYPE == 'N') THEN
                  IF(DUPLICATE_CLASS_ID_CHECK(CLASS_NUM) == -99) THEN
                     IF(CLASS_NUM+1 > 0) THEN
                        IF(TEMP_ASSET_CLASS_POINTER(CLASS_NUM+1)==0)THEN
                           NUM_OF_OL_INIT_CLASSES =
     +                                        NUM_OF_OL_INIT_CLASSES + 1
                           TEMP_ASSET_CLASS_POINTER(CLASS_NUM+1) =
     +                                            NUM_OF_OL_INIT_CLASSES
                           MAX_OL_INIT_CLASS_ID_NUM = MAX(CLASS_NUM+1,
     +                                        MAX_OL_INIT_CLASS_ID_NUM)
                        ENDIF
                     ENDIF
                  ENDIF
               else
                  IF(DUPLICATE_CLASS_ID_CHECK(CLASS_NUM) == -99) THEN
                     IF(CLASS_TYPE /=  'N') THEN
                        DUPLICATE_CLASS_ID_CHECK(CLASS_NUM) = 1
                     ENDIF
                  ELSEIF(CLASS_TYPE /=  'N') THEN
                     WRITE(4,*) 'Duplicate Class ID found for',
     +                                          CLASS_NUM,' ',CLASS_NAME
                     DUPLICATE_ID_FOUND = .TRUE.
                  ENDIF
                  IF(CLASS_TYPE /= 'E') THEN !  .AND. CLASS_TYPE /= 'C') THEN
                     IF(CLASS_NUM+1 > 0) THEN
                        IF(TEMP_ASSET_CLASS_POINTER(CLASS_NUM+1) ==
     +                                                        0) THEN
                           NUM_OF_OL_INIT_CLASSES =
     +                                        NUM_OF_OL_INIT_CLASSES + 1
                           TEMP_ASSET_CLASS_POINTER(CLASS_NUM+1) =
     +                                            NUM_OF_OL_INIT_CLASSES
                           MAX_OL_INIT_CLASS_ID_NUM = MAX(CLASS_NUM+1,
     +                                        MAX_OL_INIT_CLASS_ID_NUM)
                        ENDIF
                     ENDIF
C               CALL SET_INIT_CLASSES(CLASS_NUM,
C    +                                   NUM_OF_OL_INIT_CLASSES,
C    +                                    MAX_OL_INIT_CLASS_ID_NUM,
C    +                                   TEMP_ASSET_CLASS_POINTER)
                     IF(CLASS_TYPE == 'P') THEN
c                       CALL STORE_CONSOLIDATED_INIT_RECORD(IREC)
                        PARENT_CLASS_ID_NUM = CLASS_NUM ! + 1
                     ELSE !IF(CLASS_TYPE=='B' .OR. CLASS_TYPE=='R' .OR.
C    +                                             CLASS_TYPE=='S') THEN
                        IF(LINKED_CLASS_ID+1 > 0) THEN
                           OL_CLASS_LINKAGE(NUM_OF_OL_INIT_CLASSES) =
     +                                                   LINKED_CLASS_ID
                           OL_CLASS_TYPE(NUM_OF_OL_INIT_CLASSES) =
     +                                                        CLASS_TYPE
                           SBU_LINKED_COUNTER(LINKED_CLASS_ID+1) = 1 +
     +                           SBU_LINKED_COUNTER(LINKED_CLASS_ID+1)
                           OL_MAX_LINKED_SBU = MAX(OL_MAX_LINKED_SBU,
     +                         SBU_LINKED_COUNTER(LINKED_CLASS_ID+1))
                        ENDIF
                     ENDIF
                  ELSEIF(CLASS_TYPE == 'E') THEN !  .AND. CLASS_TYPE /= 'C') THEN
                     IF(CLASS_NUM+1 > 0) THEN
                        IF(TEMP_ASSET_CLASS_POINTER(CLASS_NUM+1) ==
     +                                                           0) THEN
                           NUM_OF_OL_INIT_CLASSES =
     +                                        NUM_OF_OL_INIT_CLASSES + 1
                           TEMP_ASSET_CLASS_POINTER(CLASS_NUM+1) =
     +                                            NUM_OF_OL_INIT_CLASSES
                           MAX_OL_INIT_CLASS_ID_NUM = MAX(CLASS_NUM+1,
     +                                         MAX_OL_INIT_CLASS_ID_NUM)
                        ENDIF
                     ENDIF
                  ENDIF
C
C ESTABLISH A LIST OF DEFINED CLASSES
C
C                  IF(CLASS_TYPE /= 'C') THEN
                     OL_MAX_DEFINED_ID=MAX(OL_MAX_DEFINED_ID,CLASS_NUM)
                     OL_MASTER_DEFINED_CLASS_IDS(CLASS_NUM) = CLASS_NUM
                     IF(CLASS_TYPE == 'P') THEN
                        OL_PARENT_CLASS_ID_NUM = CLASS_NUM
                     ENDIF
C                  ENDIF
                  IF(CLASS_TYPE == 'E')
     +                           OL_ELIMINAITON_CLASS_ID_NUM = CLASS_NUM
               ENDIF
               WRITE(12,REC=IREC) DELETE,CLASS_NAME,
     +                            CLASS_NUM,CLASS_TYPE,
     +                            CLASS_TYPE_STR,
     +                            LINKED_CLASS_ID,
     +                            REPORT_CLASS,
     +                            MOVE_TO_CLASS_ID,
     +                            ACCOUNT_ACTIVE,                ! 87 
     +                            COMPANY_ID,                       ! 105
     +                            ALL_VALUES,
     +                            CURRENCY_DENOMINATION,
     +                            FED_TAX_EXEMPT,
     +                            DEACTIVATE_IN_YEAR,
     +                            ACTIVATE_IN_YEAR,
     +                            ACCOUNTS_RECEIVABLE,
     +                            ACCOUNTS_PAYABLE,
     +                            STD_INTEREST_PAYMENT,
     +                            FUEL_INVENTORY,
     +                            DEFERRED_FUEL_BALANCE,
     +                            OUC_BOND_SINKING_FUND_BAL,
     +                            OUC_DEBT_SERV_RESV_BAL,
     +                            OUC_RENEWAL_REPLACEMENT_BAL,
     +                            OUC_OP_CONSTRUCT_FUND_BAL,
     +                            OUC_SP_CONSTRUCT_FUND_BAL,
     +                            OUC_SR_CAPTLZED_INT_FD_BAL,
     +                            OUC_JR_CAPTLZED_INT_FD_BAL,
     +                            OUC_MINI_BOND_PRIN_SKFUND_BAL,
     +                            OUC_MINI_BOND_INT_SKFUND_BAL,
     +                            OUC_LIABILITY_REDUCTION_BAL,
     +                            OUC_FUEL_INVENTORY_BAL,
     +                            OUC_MATERIALS_INVENTORY_BAL,
     +                            OUC_PREPAID_EXP_BAL,
     +                            OUC_BASE_EL_STAB_FUND_BAL,
     +                            ACQUISITION_PREMIUM,
     +                            OUC_BASE_WATER_STAB_FUND_BAL,
     +                            OUC_FUEL_STAB_FUND_BAL,
     +                            OUC_CUST_RETENTION_FUND_BAL,
     +                            OUC_TTL_UNRESTR_CUR_LIAB_BAL,
     +                            STD_INTEREST_VECTOR,            !93
     +                            LTD_INTEREST_VECTOR,
     +                            PS_DIVIDENDS_VECTOR,
     +                            STI_INTEREST_VECTOR,
     +                            LTI_INTEREST_VECTOR,
     +                            CUSTOMER_DEPOSIT_INTREST_VECTOR,
     +                            DECOM_FUND_INTEREST_VECTOR,
     +                            RETIREMENT_FUND_INTEREST_VECTOR, ! 100
     +                            CURRENCY_CONVERSION_VECTOR,
     +                            FASB_87_INTANGIBLE_ASSETS,
     +                            OTHER_COMPREHENSIVE_INCOME_BALANCE,
     +                            OptRevenueTaxBasis
C

            ENDDO
            IF(IOS_BASE /= 0) EXIT
         ENDDO
         CLOSE(10)
         CLOSE(12)
         IF(MAX_OL_INIT_CLASS_ID_NUM > 0) THEN
            ALLOCATE(OL_INIT_CLASS_POINTER(MAX_OL_INIT_CLASS_ID_NUM))
            OL_INIT_CLASS_POINTER =
     +              TEMP_ASSET_CLASS_POINTER(1:MAX_OL_INIT_CLASS_ID_NUM)
         ENDIF
         DEALLOCATE(TEMP_ASSET_CLASS_POINTER,SBU_LINKED_COUNTER)
         IF(INITOL == 'BC') CLOSE(11)
         INITOL = 'OL'

      RETURN
C
C***********************************************************************
      ENTRY MUNI_IN_MAKEBIN
C***********************************************************************
         CALL MG_LOCATE_WRITE(16,30,INITFIL(),ALL_VERSIONS,0)
         CALL MG_CLEAR_LINE_WRITE(17,9,36,MUNI_FILE_TYPE,ALL_VERSIONS,0)
         FILE_NAME = get_inb_filename()
         INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
         OPEN(11,FILE=trim(OUTPUT_DIRECTORY())//"BCINITAL.BIN",
     +                      ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
         DELETE= 1
         YEAR = BASE_YEAR()
         TOTAL_RESERVE_BALANCE = 0.
         FIXED_ASSETS = 0.
         SYSTEM_AVERAGE_RATE = 0.
         SPECIAL_RESERVE_BALANCE = 0.
         LAST_YEAR_SALES_REVENUE = 0.
         CALAVERAS_RESERVE_BALANCE = 0.
         WRITE(11,REC=1) DELETE,YEAR,TOTAL_RESERVE_BALANCE,
     +        FIXED_ASSETS,SYSTEM_AVERAGE_RATE,SPECIAL_RESERVE_BALANCE,
     +        LAST_YEAR_SALES_REVENUE,CALAVERAS_RESERVE_BALANCE
         IF(FILE_EXISTS) THEN
                OPEN(10,FILE=FILE_NAME)
            READ(10,*) DELETE
            READ(10,1000,IOSTAT=IOS) RECLN
            IF(IOS == 0) THEN
                RECLN = trim(RECLN)//',,,,,,,,,,,,,,'
                READ(RECLN,*,ERR=200) DELETE,YEAR,
     +               TOTAL_RESERVE_BALANCE,
     +               FIXED_ASSETS,SYSTEM_AVERAGE_RATE,
     +               SPECIAL_RESERVE_BALANCE,
     +               LAST_YEAR_SALES_REVENUE,CALAVERAS_RESERVE_BALANCE
                ENDIF
            CLOSE(10)
         ENDIF
         WRITE(11,REC=1) DELETE,YEAR,TOTAL_RESERVE_BALANCE,
     +        FIXED_ASSETS,SYSTEM_AVERAGE_RATE,SPECIAL_RESERVE_BALANCE,
     +        LAST_YEAR_SALES_REVENUE,CALAVERAS_RESERVE_BALANCE
         CLOSE(11)
C
      RETURN
C
C MUNICIPAL OVERLAY
C
       ENTRY MUNI_IN_MAKEOVL(OVERLAY_FAMILY_NAME)
       DATA_DRIVE = OUTPUT_DIRECTORY()
         CALL MG_CLEAR_LINE_WRITE(17,9,36,MUNI_FILE_TYPE,ALL_VERSIONS,0)
         FILE_NAME = get_ino_filename(data_drive, overlay_family_name)
         
         OPEN(10,FILE=FILE_NAME)
         READ(10,*) DELETE
         INUNIT = 12
         IF(INITOL == 'BC') THEN
            OPEN(11,FILE=trim(DATA_DRIVE)//"BCINITAL.BIN",
     +                      ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
            INUNIT = 11
         ENDIF
         OPEN(12,FILE=DATA_DRIVE//"OLINITAL.BIN",ACCESS="DIRECT",
     +                                      STATUS="UNKNOWN",RECL=LRECL)
         READ(INUNIT,REC=1) DELETE,YEAR,MUNI_INIT_VALUES
         READ(10,1000,IOSTAT=IOS) RECLN
         IF(IOS == 0) THEN
            RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
            READ(RECLN,*,ERR=200) DELETE,YEAR,MUNI_INIT_VALUES
            WRITE(12,REC=1) DELETE,YEAR,MUNI_INIT_VALUES
         ENDIF
         CLOSE(10)
         CLOSE(12)
         IF(INITOL == 'BC') CLOSE(11)
         INITOL = 'OL'
      RETURN
C
  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from In_objt SIID182'
      call end_program(er_message)
C
C***********************************************************************
      ENTRY RESET_INITOL
C***********************************************************************
         INITOL = 'BC'
      RETURN
C***********************************************************************
      ENTRY GET_MAX_LINKED_SBUS(R_MAX_LINKED_SBU,R_MAX_INIT_CLASS_NUM)
C***********************************************************************
         IF(INITOL == 'OL') THEN
            R_MAX_INIT_CLASS_NUM = MAX_OL_INIT_CLASS_ID_NUM
            R_MAX_LINKED_SBU = OL_MAX_LINKED_SBU
         ELSE
            R_MAX_INIT_CLASS_NUM = MAX_BC_INIT_CLASS_ID_NUM
            R_MAX_LINKED_SBU = BC_MAX_LINKED_SBU
         ENDIF
      RETURN
C***********************************************************************
      ENTRY OPEN_INITIZATION_FILE
C***********************************************************************
         INQUIRE(UNIT=10,OPENED=UNIT_10_OPEN)
         OPEN(10,FILE=trim(OUTPUT_DIRECTORY())//INITOL//
     +                          "INITAL.BIN",ACCESS="DIRECT",RECL=LRECL)
      RETURN
C***********************************************************************
      ENTRY CLOSE_INIIZATION_FILE
C***********************************************************************
         CLOSE(10)
      RETURN
C***********************************************************************
      ENTRY RETURN_INITIALIZATION_CLASSES(R_NUM_OF_INIT_CLASSES,
     +                                       R_MAX_INIT_CLASS_NUM)
C***********************************************************************
         IF(INITOL == 'OL') THEN
            R_NUM_OF_INIT_CLASSES = NUM_OF_OL_INIT_CLASSES
            R_MAX_INIT_CLASS_NUM = MAX_OL_INIT_CLASS_ID_NUM
         ELSE
            R_NUM_OF_INIT_CLASSES = NUM_OF_BC_INIT_CLASSES
            R_MAX_INIT_CLASS_NUM = MAX_BC_INIT_CLASS_ID_NUM
         ENDIF
      RETURN
C***********************************************************************
      ENTRY RETURN_INITIALIZATION_POINTER(R_INIT_CLASS_POINTERS)
C***********************************************************************
         IF(INITOL == 'OL') THEN
c            CALL CMOVE(OL_INIT_CLASS_POINTER,
c     +                        R_INIT_CLASS_POINTERS,
c     +                             INT(2*MAX_OL_INIT_CLASS_ID_NUM))
c            DO I = 1, MAX_OL_INIT_CLASS_ID_NUM
               R_INIT_CLASS_POINTERS(1:MAX_OL_INIT_CLASS_ID_NUM) =
     +                                             OL_INIT_CLASS_POINTER ! (1:MAX_OL_INIT_CLASS_ID_NUM)
c            ENDDO
         ELSE
c            CALL CMOVE(BC_INIT_CLASS_POINTER,
c     +                        R_INIT_CLASS_POINTERS,
c     +                             INT(2*MAX_BC_INIT_CLASS_ID_NUM))
c            DO I = 1, MAX_BC_INIT_CLASS_ID_NUM
               R_INIT_CLASS_POINTERS(1:MAX_BC_INIT_CLASS_ID_NUM) =
     +                                             BC_INIT_CLASS_POINTER !(1:MAX_BC_INIT_CLASS_ID_NUM)
c            ENDDO
         ENDIF
      RETURN
C***********************************************************************
      ENTRY GET_LINKED_CLASS(R_ASSET_CLASS,R_LINKED_CLASS)
C***********************************************************************
         R_LINKED_CLASS = -99.
         IF(.NOT.((INITOL == 'OL' .AND.
     +            R_ASSET_CLASS == OL_ELIMINAITON_CLASS_ID_NUM) .OR.
     +     (INITOL == 'BC' .AND.
     +              R_ASSET_CLASS == BC_ELIMINAITON_CLASS_ID_NUM))) THEN
C
            IF(INITOL == 'OL' .AND.
     +          R_ASSET_CLASS+1 <= MAX_OL_INIT_CLASS_ID_NUM .AND.
     +                            ALLOCATED(OL_INIT_CLASS_POINTER)) THEN
               POINTR = OL_INIT_CLASS_POINTER(R_ASSET_CLASS+1)
               R_LINKED_CLASS = OL_CLASS_LINKAGE(POINTR)
            ELSEIF(INITOL == 'BC' .AND.
     +          R_ASSET_CLASS+1 <= MAX_BC_INIT_CLASS_ID_NUM .AND.
     +                            ALLOCATED(BC_INIT_CLASS_POINTER)) THEN
               POINTR = BC_INIT_CLASS_POINTER(R_ASSET_CLASS+1)
               R_LINKED_CLASS = BC_CLASS_LINKAGE(POINTR)
            ENDIF
            IF(R_LINKED_CLASS == R_ASSET_CLASS) R_LINKED_CLASS = -99
         ENDIF
      RETURN
C***********************************************************************
      ENTRY GET_DEBT_LINKED_CLASS(R_ASSET_CLASS,R_LINKED_CLASS)
C***********************************************************************
         R_LINKED_CLASS = -99.
         IF(INITOL == 'OL' .AND.
     +          R_ASSET_CLASS+1 <= MAX_OL_INIT_CLASS_ID_NUM .AND.
     +                            ALLOCATED(OL_INIT_CLASS_POINTER)) THEN
            IF(R_ASSET_CLASS == OL_ELIMINAITON_CLASS_ID_NUM) RETURN
            POINTR = OL_INIT_CLASS_POINTER(R_ASSET_CLASS+1)
            R_LINKED_CLASS = OL_CLASS_LINKAGE(POINTR)
            CLASS_TYPE = OL_CLASS_TYPE(POINTR)
         ELSEIF(INITOL == 'BC' .AND.
     +          R_ASSET_CLASS+1 <= MAX_BC_INIT_CLASS_ID_NUM .AND.
     +                            ALLOCATED(BC_INIT_CLASS_POINTER)) THEN
            IF(R_ASSET_CLASS == BC_ELIMINAITON_CLASS_ID_NUM) RETURN
            POINTR = BC_INIT_CLASS_POINTER(R_ASSET_CLASS+1)
            R_LINKED_CLASS = BC_CLASS_LINKAGE(POINTR)
            CLASS_TYPE = BC_CLASS_TYPE(POINTR)
         ENDIF
         IF(R_LINKED_CLASS == R_ASSET_CLASS .OR.
     +           CLASS_TYPE == 'S' .OR. CLASS_TYPE == 'N')
     +                                              R_LINKED_CLASS = -99
      RETURN
C***********************************************************************   
      ENTRY CHECK_IF_CLASS_DEFINED(R_ASSET_CLASS)
C***********************************************************************
         IF(INITOL == 'OL') THEN
            IF(R_ASSET_CLASS >= 0 .AND.
     +                 ALLOCATED(OL_MASTER_DEFINED_CLASS_IDS) .AND.
     +                          R_ASSET_CLASS <= OL_MAX_DEFINED_ID) THEN
               R_ASSET_CLASS=OL_MASTER_DEFINED_CLASS_IDS(R_ASSET_CLASS)
               IF(R_ASSET_CLASS == -99 .OR. REALLY_KEPCO) 
     +                            R_ASSET_CLASS = OL_PARENT_CLASS_ID_NUM
            ELSE
               R_ASSET_CLASS = OL_PARENT_CLASS_ID_NUM
            ENDIF
         ELSE
            IF(R_ASSET_CLASS >= 0 .AND.
     +                 ALLOCATED(BC_MASTER_DEFINED_CLASS_IDS) .AND.
     +                          R_ASSET_CLASS <= BC_MAX_DEFINED_ID) THEN
               R_ASSET_CLASS=BC_MASTER_DEFINED_CLASS_IDS(R_ASSET_CLASS)
               IF(R_ASSET_CLASS == -99 .OR. REALLY_KEPCO) 
     +                            R_ASSET_CLASS = BC_PARENT_CLASS_ID_NUM
            ELSE
               R_ASSET_CLASS = BC_PARENT_CLASS_ID_NUM
            ENDIF
         ENDIF
      RETURN
C***********************************************************************   
      ENTRY PARENT_CLASS_ID(R_PARENT_CLASS_ID)
C***********************************************************************
         R_PARENT_CLASS_ID = PARENT_CLASS_ID_NUM
      RETURN
C***********************************************************************   
      ENTRY PARENT_CLASS_ID_LOCATION(R_PARENT_CLASS_ID)
C***********************************************************************
         IF(INITOL == 'OL') THEN
            R_PARENT_CLASS_ID =
     +                      OL_INIT_CLASS_POINTER(PARENT_CLASS_ID_NUM+1)
         ELSE
            R_PARENT_CLASS_ID = 
     +                      BC_INIT_CLASS_POINTER(PARENT_CLASS_ID_NUM+1)
         ENDIF
      RETURN
C***********************************************************************
      ENTRY ELIMINATION_CLASS_ID(R_ELIMINAITON_CLASS_ID_NUM)
C***********************************************************************
         IF(INITOL == 'OL') THEN
            R_ELIMINAITON_CLASS_ID_NUM = OL_ELIMINAITON_CLASS_ID_NUM+1  ! +1 TO BE CONSISTENT WITH CLASS POSITIONS
         ELSE
            R_ELIMINAITON_CLASS_ID_NUM = BC_ELIMINAITON_CLASS_ID_NUM+1
         ENDIF
 1000 FORMAT(A)
 1010 FORMAT('&',A)
      END
C***********************************************************************   
      FUNCTION INIT_FILE_IS_ACTIVE()
C***********************************************************************
      LOGICAL*1 INIT_FILE_IS_ACTIVE
      LOGICAL*4 FILE_EXISTS
      CHARACTER*256 FILE_NAME,BASE_FILE_DIRECTORY
      CHARACTER*5 INITFIL
      LOGICAL (KIND=1) :: SP_CAPEX_ACTIVE
         FILE_NAME = trim(BASE_FILE_DIRECTORY())//
     +                                  "INB"//trim(INITFIL())//".DAT"
         INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
         INIT_FILE_IS_ACTIVE = FILE_EXISTS .AND. .NOT. SP_CAPEX_ACTIVE()
      RETURN
      END
