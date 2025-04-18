!     Last change: msg 11/11/2018 12:12:47 PM

!
!          SUBROUTINE TO FIND OVERLAY FILES
!          COPYRIGHT (C) 1984, 85, 86 M.S. GERBER & ASSOCIATES, INC.
!
!***********************************************************************
!
      RECURSIVE SUBROUTINE FINDOVLS(OVLNAME,MATCH_FOUND,END_POINT,   &
                                    RUN_FUTURE_ASSETS)
!
      use spindriftlib
      USE ProSymModule
      use end_routine
      use filename_tracker
      USE SIZECOM
      SAVE

!     NOTE CODE 376 IS AVAILABLE CODES 377-388 ARE USED BY HORIZONS
      INTEGER (KIND=2) , PARAMETER ::  NUMBER_OF_DATA_FILES=388
      character (len=256) :: filename
!
! NOTE THAT FILE NUMBER 7 IS USED FOR EXPENSE AND REVENUE NAMES BUT IS NOT "OVERLAYABLE" 11/27/01 MSG
!
      LOGICAL (kind=1) ::  SYSTEM_BASED_FORECAST,RUN_FUTURE_ASSETS,CLASS_BASED_FORECAST,WVPA
!
      LOGICAL (kind=1) ::  OVERLAY_BOOK_ACTIVE,OVERLAY_BOOK_IS_ACTIVE
      LOGICAL (kind=4) ::  BIN_FILE_EXISTS,THIS_FILE_EXISTS
      INTEGER (kind=2) ::  J
      CHARACTER (len=1) ::  UTILITY_TYPE
      CHARACTER (len=5) ::  OVLNAME,R_OVLNAME
      CHARACTER (len=256) ::  SAVED_OVLNAME

      CHARACTER (len=256) ::  FILE_IN,FILE_SEARCH,OUTPUT_DIRECTORY
      INTEGER (kind=2) ::  I,END_POINT,RETURN_CODE,ISERROR
      CHARACTER (len=*) :: R_PROJECT_NAME
      LOGICAL (kind=1) ::  R_MATCH_FOUND,R_FILE_EXISTS(*)
      INTEGER (kind=2) ::  DELETE,FAMILY_POSITION,NUMBER_OF_DEFINITIONS
      INTEGER ::  IOS
      LOGICAL (kind=1) ::  FAMILY_DEFINITION_EXISTS,RUN_COAL_MODEL_ONLY
      LOGICAL (kind=1) ::  OVERLAY_FILE_EXISTS(:,:)
      LOGICAL (kind=1) ::  OVERLAY_FAMILY_ACTIVE(:)
      CHARACTER (len=5) ::  OVL_FILE_NAME
      CHARACTER (len=4096) ::  RECLN
      CHARACTER (len=5) ::  FAMILY_NAMES(:),OVERLAY_FILES_IN_FAMILY(:,:),OVERLAY_FILE_NAMES(0:16)
      ALLOCATABLE :: FAMILY_NAMES,OVERLAY_FILES_IN_FAMILY,OVERLAY_FILE_EXISTS,OVERLAY_FAMILY_ACTIVE
      CHARACTER (LEN=256) ::  ProSym_Interface_File
      LOGICAL (KIND=1) :: ACTIVE_FILES_EXIST(0:10)
      CHARACTER (LEN=256) :: ACTIVE_FILES_LIST(0:10)
!
! TYPE DECLARATION FOR CHECKING FILE EXISTANCES
!
      CHARACTER (len=5) ::  OVERLAY_FILE_NAME(NUMBER_OF_DATA_FILES),R_OVERLAY_FILE_NAMES(NUMBER_OF_DATA_FILES)
      CHARACTER (len=5) ::  EAE_FILE
      CHARACTER (len=5) ::  DDE_FILE,DBE_FILE,FAE_FILE,NFE_FILE,   &
                  FX_FUTURE_ASSETS_CAPEX_FILE,INE_FILE,TAX_FILE,   &
                  EPE_FILE,EXPENSE_NAMES_RESERVED,PRF_FILE,   &
                  INT_FILE,FIN_FILE,ESC_FILE,FOS_FILE,HYD_FILE,   &
                  UPE_FILE,SFC_FILE,CFC_FILE,RFC_FILE,IFC_FILE,O1F_FILE,   &
                  O2F_FILE,O3F_FILE,CLS_FILE,CLH_FILE,LMG_FILE,LMS_FILE,   &
                  PFD_FILE,PNP_FILE,FNP_FILE,CNP_FILE,CRA_FILE,COS_FILE,   &
                  LFI_FILE,ENV_FILE,ECO_FILE,ASTRT_FILE,ASTVEC_FILE,   &
                  CNT_FILE,PRODP_FILE,AREA_FILE,FUEL_INVENTORY_FILE,   &
                  FUEL_PRICE_FILE,FUEL_EMISSIONS_FILE,   &
                  SERVICE_TRANSACTIONS_FILE,CLASS_RUN_SPECS_FILE,   &
                  CLASS_REVENUE_FORECAST_FILE,ELIMINATIONS_FILE,   &
                  DETAILED_REPORTS_FILE,CATAWBA2_CONTRACT_FILE,   &
                  NEW_RUN_SPECS_FILE,REFERENCE_LOAD_DATA_FILE,   &
                  OVER_UNDER_TABLE_FILE,   &
                  UNIT_OUTAGE_FILE,   &
                  TRANSACT_TIE_FILE,   &
                  MONTHLY_PAYABLES_FILE,   &
                  DAY_TYPE_FILE
      CHARACTER (LEN=5) :: SCENARIO_MAKER_FILE,   &
                  ENERGY_PRODUCTS_FILE,   &
                  MARKET_PRICE_FILE,   &
                  CPL_CONTRACT_FILE,   &
                  RDI_POWERDAT_FILE,   &
                  RDI_BASECASE_FILE,   &
                  TRANS_FORECAST_FILE,   &
                  TAX_INFO_FILE,   &
                  ADDENDUM_FILE,   &
                  TRANS_GROUPS_FILE,   &
                  TRANS_PATHS_FILE,   &
                  TRANS_CONSTRAINT_FILE,   &
                  SCENARIO_PARAMETER,   &
                  NEW_MARKET_PRICE_FILE,   &
                  SCENARIO_HOURLY_FILE,   &
                  WEEKLY_HYDRO_FILE,   &
                  WEATHER_DEMAND_FILE,   &
                  FORWARD_OPTION_FILE,   &
                  MARKET_GROUPS_FILE,   &
                  FUEL_PRICE_F1_FILE
      CHARACTER (LEN=5) :: FUEL_PRICE_F2_FILE,   &
                  FUEL_PRICE_F3_FILE,   &
                  FUEL_PRICE_F4_FILE,   &
                  FUEL_PRICE_F5_FILE,   &
                  FUEL_PRICE_F6_FILE,   &
                  FUEL_PRICE_F7_FILE,   &
                  FUEL_PRICE_F8_FILE,   &
                  FUEL_PRICE_F9_FILE,   &
                  CL_UNITS_C1_FILE,   &
                  CL_UNITS_C2_FILE,   &
                  CL_UNITS_C3_FILE,   &
                  CL_UNITS_C4_FILE,   &
                  CL_UNITS_C5_FILE,   &
                  CL_UNITS_C6_FILE,   &
                  CL_UNITS_C7_FILE,   &
                  CL_UNITS_C8_FILE,   &
                  CL_UNITS_C9_FILE,   &
                  CL_UNITS_C0_FILE,   &
                  CL_UNITS_CA_FILE,   &
                  CL_UNITS_CB_FILE   
      CHARACTER (LEN=5) :: CL_UNITS_CC_FILE,   &
                  CL_UNITS_CD_FILE,   &
                  CL_UNITS_CE_FILE,   &
                  CL_UNITS_CG_FILE,   &
                  CL_UNITS_CI_FILE,   &
                  CL_UNITS_CJ_FILE,   &
                  CL_UNITS_CK_FILE,   &
                  CL_UNITS_CM_FILE,   &
                  CL_UNITS_CP_FILE,   &
                  CL_UNITS_CQ_FILE,   &
                  EL_UNITS_E1_FILE,   &
                  EL_UNITS_E2_FILE,   &
                  EL_UNITS_E3_FILE,   &
                  EL_UNITS_E4_FILE,   &
                  EL_UNITS_E5_FILE,   &
                  EL_UNITS_E6_FILE,   &
                  EL_UNITS_E7_FILE,   &
                  EL_UNITS_E8_FILE,   &
                  EL_UNITS_E9_FILE,   &
                  EL_UNITS_E0_FILE
      CHARACTER (LEN=5) :: EL_UNITS_EB_FILE,   &
                  EL_UNITS_ED_FILE,   &
                  EL_UNITS_EE_FILE,   &
                  EL_UNITS_EF_FILE,   &
                  EL_UNITS_EG_FILE,   &
                  EL_UNITS_EH_FILE,   &
                  EL_UNITS_EI_FILE,   &
                  TRANS_FORECAST_T1_FILE,   &
                  TRANS_FORECAST_T2_FILE,   &
                  TRANS_FORECAST_T3_FILE,   &
                  TRANS_FORECAST_T4_FILE,   &
                  TRANS_FORECAST_T5_FILE,   &
                  TRANS_FORECAST_T6_FILE,   &
                  TRANS_FORECAST_T7_FILE,   &
                  TRANS_FORECAST_T8_FILE,   &
                  TRANS_FORECAST_T9_FILE,   &
                  TRANS_FORECAST_T0_FILE,   &
                  TRANS_FORECAST_TB_FILE,   &
                  TRANS_FORECAST_TD_FILE,   &
                  TRANS_FORECAST_TH_FILE
      CHARACTER (LEN=5) :: TRANS_FORECAST_TI_FILE,   &
                  REGIONAL_OUTAGES_FILE,   &
                  EXPENSE_FILE_X1,   &
                  EXPENSE_FILE_X2,   &
                  EXPENSE_FILE_X3,   &
                  EXPENSE_FILE_X4,   &
                  EXPENSE_FILE_X5,   &
                  SCENARIO_VECTOR_FILE,   &
                  SM_UNITS_S1,   &
                  SM_UNITS_S2,   &
                  SM_UNITS_S3,   &
                  SM_UNITS_S4,   &
                  SM_UNITS_S5,   &
                  SM_UNITS_S6,   &
                  SM_UNITS_S7,   &
                  SM_UNITS_S8,   &
                  SM_UNITS_S9, & !  184
                  USER_DAY_FILE,   &
                  ICAP_FILE,   &
                  REGIONAL_PARAMETER_FILE  ! 187
      CHARACTER (LEN=5) :: FUEL_DERIVATIVES_FILE,   &
                  FINANCIAL_HISTORY_FILE, & !  189
                  MONTHLY_CAPITIAL_RATES_FILE, & !  190
                  WVPA_RATES_STRUCTURES,   &
                  WVPA_TRACKER_FILE,  & !  192
                  WVPA_COOP_REVENUE_FILE, & !  193
                  RM_UNITS_R1,   &
                  RM_UNITS_R2,   &
                  RM_UNITS_R3,         & !  196
                  LH_GLOBAL,   &
                  ELECT_HOUR_MULT_FILE,   &
                  GAS_HOUR_MULT_FILE,   &
                  OIL_HOUR_MULT_FILE, & !  200
                  ProSym_Data_FILE, & !  101
                  ENERGY_PRODUCTS_FILE_P2,   &
                  ENERGY_PRODUCTS_FILE_P3,   &
                  ENERGY_PRODUCTS_FILE_P4,   &
                  ENERGY_PRODUCTS_FILE_P5,   &
                  ENERGY_PRODUCTS_FILE_P6,   &
                  GAS_NODE_FILE
      CHARACTER (LEN=5) :: GAS_TRANSPORT_FILE,   &
                  TRAN_EXP_FILE,   &
                  GAS_DEMAND_FORECAST_FILE,   &
                  GAS_SUPPLY_FORECAST_FILE,   &
                  GAS_FORECAST_T1,   &
                  GAS_FORECAST_T2,   &
                  GAS_FORECAST_T3,   &
                  GAS_FORECAST_T4,   &
                  GAS_FORECAST_T5,   &
                  GAS_FORECAST_T6,   &
                  GAS_FORECAST_T7,   &
                  GAS_FORECAST_T8,   &
                  GAS_FORECAST_T9,   &
                  GAS_WEATHER_DEMAND_FILE,   &
                  GAS_STORAGE_FILE,   &
                  FUEL_PRICE_F0_FILE,   &
                  GQB_GAS_THERMAL_FILE,   &
                  CORES_AND_MACHINES_FILE,   &
                  RPS_REQUIREMENTS_FILE,   &
                  GQB_GAS_THERMAL !  353
      CHARACTER (LEN=5) :: COAL_NODE_FILE,  & !  354
                  COAL_TRANSPORT_FILE,       & !  355
                  COAL_SUPPLY_FORECAST_FILE, & !  356
                  COAL_DEMAND_FORECAST_FILE, & !  357
                  COAL_FORECAST_T1,           & !  358
                  RPS_STATE_DEMAND, & !  359
                  THERMAL_RETROFIT, & !  360
                  COAL_CONTRACTS,   & !  361
                  CO2_PARAM,         & !  362
                  USER_DAY_D1,      & !  363
                  USER_DAY_D2,      & !  364
                  USER_DAY_D3,      & !  365
                  USER_DAY_D4,      & !  366
                  USER_DAY_D5,      & !  367
                  USER_DAY_D6,      & !  368
                  USER_DAY_D7,      & !  369
                  USER_DAY_D8,      & !  370
                  USER_DAY_D9,       & !  371
                  COAL_MODEL_POINTER_FILE,      & !  372
                  COAL_MODEL_SO2_INFO_FILE,   & !  373
                  COAL_MODEL_GENERIC_TRANSPORT_FILE,  & !  374
                  COAL_MODEL_GENERIC_DEMAND_FILE,  & !  375
                  RPS_PROGRAM_FILE ! 376
!
      CHARACTER (len=3) ::  FILE_TYPES(NUMBER_OF_DATA_FILES),TEST_STRING
      LOGICAL (kind=1) ::  FILE_EXISTS(NUMBER_OF_DATA_FILES)
      LOGICAL (kind=1) ::  MATCH_FOUND,   &
                THIS_OVERLAY_FILE_EXISTS(0:16)
      LOGICAL (kind=1) ::  INT_EXISTS
      LOGICAL (kind=1) ::  DBE_EXISTS,EAE_EXISTS,DDE_EXISTS,EPE_EXISTS,   &
                FAE_EXISTS,   &
                FX_FUTURE_ASSETS_CAPEX_EXISTS,   &
                FOS_EXISTS,PRF_EXISTS,CLH_EXISTS,RFC_EXISTS,   &
                IFC_EXISTS,CFC_EXISTS,O1F_EXISTS,SFC_EXISTS,O2F_EXISTS,   &
                O3F_EXISTS,CLS_EXISTS,LMG_EXISTS,PFD_EXISTS,CNP_EXISTS,   &
                CRA_EXISTS,ESC_EXISTS,COS_EXISTS,HYD_EXISTS,NFE_EXISTS,   &
                FIN_EXISTS,FNP_EXISTS,PNP_EXISTS,LMS_EXISTS,UPE_EXISTS,   &
                INE_EXISTS,PRA_EXISTS,TAX_EXISTS,LFI_EXISTS,ENV_EXISTS,   &
                ECO_EXISTS,ASTRT_EXISTS,ASTVEC_EXISTS,   &
                CNT_EXISTS,PRODP_EXISTS,   &
                AREA_EXISTS,FUEL_INVENTORY_EXISTS,FUEL_PRICE_EXISTS,   &
                FUEL_EMISSIONS_FILE_EXISTS,   &
                SERVICE_TRANSACTIONS_EXISTS,   &
                CLASS_RUN_SPECS_EXISTS,   &
                CLASS_REVENUE_FORECAST_EXISTS,   &
                ELIMINATIONS_EXISTS,   &
                DETAILED_REPORTS_EXISTS,   &
                CATAWBA2_CONTRACT_EXISTS,   &
                REFERENCE_LOAD_DATA_EXISTS
      LOGICAL (kind=1) :: OVER_UNDER_TABLE_EXISTS,   &
                NEW_RUN_SPECS_EXISTS,   &
                UNIT_OUTAGE_EXISTS,   &
                TRANSACT_TIE_EXISTS,   &
                MONTHLY_PAYABLES_EXISTS,   &
                DAY_TYPE_EXISTS,   &
                SCENARIO_MAKER_EXISTS,   &
                ENERGY_PRODUCTS_EXISTS,   &
                MARKET_PRICE_EXISTS,   &
                CPL_CONTRACT_EXISTS,   &
                RDI_POWERDAT_EXISTS,   &
                RDI_BASECASE_EXISTS,   &
                TRANS_FORECAST_EXISTS,   &
                TAX_INFO_FILE_EXISTS,   &
                ADDENDUM_FILE_EXISTS,   &
                TRANS_GROUPS_FILE_EXISTS,   &
                TRANS_PATHS_FILE_EXISTS,   &
                TRANS_CONSTRAINT_FILE_EXISTS,   &
                SCENARIO_PARAMETER_EXISTS,   &
                NEW_MARKET_PRICE_EXISTS,   &
                SCENARIO_HOURLY_EXISTS
      LOGICAL (kind=1) :: WEEKLY_HYDRO_EXISTS,   &
                WEATHER_DEMAND_EXISTS,   &
                FORWARD_OPTION_EXISTS,   &
                MARKET_GROUPS_EXISTS,   &
                FUEL_PRICE_F1_EXISTS,   &
                FUEL_PRICE_F2_EXISTS,   &
                FUEL_PRICE_F3_EXISTS,   &
                FUEL_PRICE_F4_EXISTS,   &
                FUEL_PRICE_F5_EXISTS,   &
                FUEL_PRICE_F6_EXISTS,   &
                FUEL_PRICE_F7_EXISTS,   &
                FUEL_PRICE_F8_EXISTS,   &
                FUEL_PRICE_F9_EXISTS,   &
                CL_UNITS_C1_EXISTS,   &
                CL_UNITS_C2_EXISTS,   &
                CL_UNITS_C3_EXISTS,   &
                CL_UNITS_C4_EXISTS,   &
                CL_UNITS_C5_EXISTS,   &
                CL_UNITS_C6_EXISTS,   &
                CL_UNITS_C7_EXISTS,   &
                ProSym_Data_EXISTS,   &
                CL_UNITS_C8_EXISTS,   &
                CL_UNITS_C9_EXISTS,   &
                CL_UNITS_C0_EXISTS,   &
                CL_UNITS_CA_EXISTS,   &
                CL_UNITS_CB_EXISTS,   &
                CL_UNITS_CC_EXISTS,   &
                CL_UNITS_CD_EXISTS,   &
                CL_UNITS_CE_EXISTS,   &
                CL_UNITS_CG_EXISTS   
      LOGICAL (kind=1) :: CL_UNITS_CI_EXISTS,   &
                CL_UNITS_CJ_EXISTS,   &
                CL_UNITS_CK_EXISTS,   &
                CL_UNITS_CM_EXISTS,   &
                CL_UNITS_CP_EXISTS,   &
                CL_UNITS_CQ_EXISTS,   &
                EL_UNITS_E1_EXISTS,   &
                EL_UNITS_E2_EXISTS,   &
                EL_UNITS_E3_EXISTS,   &
                EL_UNITS_E4_EXISTS,   &
                EL_UNITS_E5_EXISTS,   &
                EL_UNITS_E6_EXISTS,   &
                EL_UNITS_E7_EXISTS,   &
                EL_UNITS_E8_EXISTS,   &
                EL_UNITS_E9_EXISTS,   &
                EL_UNITS_E0_EXISTS,   &
                EL_UNITS_EB_EXISTS,   &
                EL_UNITS_ED_EXISTS,   &
                EL_UNITS_EE_EXISTS,   &
                EL_UNITS_EF_EXISTS,   &
                EL_UNITS_EG_EXISTS,   &
                EL_UNITS_EH_EXISTS,   &
                EL_UNITS_EI_EXISTS,   &
                TRANS_FORECAST_T1_EXISTS,   &
                TRANS_FORECAST_T2_EXISTS,   &
                TRANS_FORECAST_T3_EXISTS,   &
                TRANS_FORECAST_T4_EXISTS,   &
                TRANS_FORECAST_T5_EXISTS,   &
                TRANS_FORECAST_T6_EXISTS,   &
                TRANS_FORECAST_T7_EXISTS
      LOGICAL (kind=1) :: TRANS_FORECAST_T8_EXISTS,   &
                TRANS_FORECAST_T9_EXISTS,   &
                TRANS_FORECAST_T0_EXISTS,   &
                TRANS_FORECAST_TB_EXISTS,   &
                TRANS_FORECAST_TD_EXISTS,   &
                TRANS_FORECAST_TH_EXISTS,   &
                TRANS_FORECAST_TI_EXISTS,   &
                REGIONAL_OUTAGES_EXISTS,   &
                EXPENSE_FILE_X1_EXISTS,   &
                EXPENSE_FILE_X2_EXISTS,   &
                EXPENSE_FILE_X3_EXISTS,   &
                EXPENSE_FILE_X4_EXISTS,   &
                EXPENSE_FILE_X5_EXISTS,   &
                SCENARIO_VECTOR_EXISTS,   &
                SM_UNITS_S1_EXISTS,   &
                SM_UNITS_S2_EXISTS,   &
                SM_UNITS_S3_EXISTS,   &
                SM_UNITS_S4_EXISTS,   &
                SM_UNITS_S5_EXISTS,   &
                SM_UNITS_S6_EXISTS,   &
                SM_UNITS_S7_EXISTS,   &
                SM_UNITS_S8_EXISTS,   &
                SM_UNITS_S9_EXISTS, & !  184
                USER_DAY_EXISTS,   &
                ICAP_EXISTS,   &
                REGIONAL_PARAMETER_EXISTS, & !  187
                FUEL_DERIVATIVES_EXISTS,   &
                FINANCIAL_HISTORY_EXISTS,   &
                MONTHLY_CAPITIAL_RATES_EXISTS,  & !  190
                WVPA_RATES_STRUCTURES_EXISTS,   &
                WVPA_TRACKER_FILE_EXISTS,  & !  192
                WVPA_COOP_REVENUE_FILE_EXISTS, & !  193
                RM_UNITS_R1_EXISTS,   &
                RM_UNITS_R2_EXISTS,   &
                RM_UNITS_R3_EXISTS,         & !  196
                LH_GLOBAL_EXISTS,   &
                ELECT_HOUR_MULT_FILE_EXISTS,   &
                GAS_HOUR_MULT_FILE_EXISTS,   &
                OIL_HOUR_MULT_FILE_EXISTS !  200
      LOGICAL (kind=1) :: ENERGY_PRODUCTS_FILE_P2_EXISTS,   &
                ENERGY_PRODUCTS_FILE_P3_EXISTS,   &
                ENERGY_PRODUCTS_FILE_P4_EXISTS,   &
                ENERGY_PRODUCTS_FILE_P5_EXISTS,   &
                ENERGY_PRODUCTS_FILE_P6_EXISTS,   &
                GAS_NODE_FILE_EXISTS,   &
                GAS_TRANSPORT_FILE_EXISTS,   &
                TRAN_EXP_FILE_EXISTS,   &
                GAS_DEMAND_FORECAST_FILE_EXISTS,   &
                GAS_SUPPLY_FORECAST_FILE_EXISTS,   &
                GAS_FORECAST_T1_EXISTS,   &
                GAS_FORECAST_T2_EXISTS,   &
                GAS_FORECAST_T3_EXISTS,   &
                GAS_FORECAST_T4_EXISTS,   &
                GAS_FORECAST_T5_EXISTS,   &
                GAS_FORECAST_T6_EXISTS,   &
                GAS_FORECAST_T7_EXISTS,   &
                GAS_FORECAST_T8_EXISTS,   &
                GAS_FORECAST_T9_EXISTS,   &
                GAS_WEATHER_DEMAND_EXISTS,   &
                GAS_STORAGE_FILE_EXISTS,   &
                FUEL_PRICE_F0_EXISTS,   &
                CORES_AND_MACHINES_EXISTS,   &
                RPS_REQUIREMENTS_EXISTS,   &
                GQB_GAS_THERMAL_EXISTS,  & !  353
                COAL_NODE_FILE_EXISTS,  & !  354
                COAL_TRANSPORT_FILE_EXISTS,       & !  355
                COAL_SUPPLY_FORECAST_FILE_EXISTS, & !  356
                COAL_DEMAND_FORECAST_FILE_EXISTS, & !  357
                COAL_FORECAST_T1_EXISTS,           & !  358
                RPS_STATE_DEMAND_EXISTS,   &
                THERMAL_RETROFIT_EXISTS,           & !  360
                COAL_CONTRACTS_EXISTS,             & !  361
                CO2_PARAM_EXISTS !  362
      LOGICAL (kind=1) :: USER_DAY_D1_EXISTS,      & !  363
                USER_DAY_D2_EXISTS,      & !  364
                USER_DAY_D3_EXISTS,      & !  365
                USER_DAY_D4_EXISTS,      & !  366
                USER_DAY_D5_EXISTS,      & !  367
                USER_DAY_D6_EXISTS,      & !  368
                USER_DAY_D7_EXISTS,      & !  369
                USER_DAY_D8_EXISTS,      & !  370
                USER_DAY_D9_EXISTS,      & !  371
                RPS_PROGRAM_EXISTS       ! 376
!
      EQUIVALENCE (FILE_EXISTS(1),EAE_EXISTS),(FILE_EXISTS(2),DDE_EXISTS),(FILE_EXISTS(3),DBE_EXISTS),(FILE_EXISTS(4),FAE_EXISTS), &
                  (FILE_EXISTS(5),NFE_EXISTS),(FILE_EXISTS(6),EPE_EXISTS),(FILE_EXISTS(7),PRA_EXISTS),(FILE_EXISTS(8),PRF_EXISTS), &
                  (FILE_EXISTS(9),INE_EXISTS),(FILE_EXISTS(10),TAX_EXISTS),(FILE_EXISTS(11),INT_EXISTS), &
                  (FILE_EXISTS(12),FIN_EXISTS),(FILE_EXISTS(13),ESC_EXISTS),(FILE_EXISTS(14),FOS_EXISTS), &
                  (FILE_EXISTS(15),HYD_EXISTS),(FILE_EXISTS(16),UPE_EXISTS),(FILE_EXISTS(17),SFC_EXISTS), &
                  (FILE_EXISTS(18),CFC_EXISTS),(FILE_EXISTS(19),RFC_EXISTS),(FILE_EXISTS(20),IFC_EXISTS), &
                  (FILE_EXISTS(21),O1F_EXISTS),(FILE_EXISTS(22),O2F_EXISTS),(FILE_EXISTS(23),O3F_EXISTS), &
                  (FILE_EXISTS(24),CLS_EXISTS),(FILE_EXISTS(25),CLH_EXISTS),(FILE_EXISTS(26),LMG_EXISTS), &
                  (FILE_EXISTS(27),LMS_EXISTS),(FILE_EXISTS(28),PFD_EXISTS),(FILE_EXISTS(29),PNP_EXISTS), &
                  (FILE_EXISTS(30),FNP_EXISTS),(FILE_EXISTS(31),CNP_EXISTS),(FILE_EXISTS(32),CRA_EXISTS), &
                  (FILE_EXISTS(33),COS_EXISTS),(FILE_EXISTS(34),REFERENCE_LOAD_DATA_EXISTS),(FILE_EXISTS(35),LFI_EXISTS), &
                  (FILE_EXISTS(36),ENV_EXISTS),(FILE_EXISTS(37),ECO_EXISTS),(FILE_EXISTS(38),ASTRT_EXISTS), &
                  (FILE_EXISTS(39),ASTVEC_EXISTS),(FILE_EXISTS(40),CNT_EXISTS),(FILE_EXISTS(41),PRODP_EXISTS), &
                  (FILE_EXISTS(42),AREA_EXISTS),(FILE_EXISTS(43),FUEL_INVENTORY_EXISTS),(FILE_EXISTS(44),FUEL_PRICE_EXISTS), &
                  (FILE_EXISTS(45),SERVICE_TRANSACTIONS_EXISTS),(FILE_EXISTS(46),FUEL_EMISSIONS_FILE_EXISTS), &
                  (FILE_EXISTS(47),OVER_UNDER_TABLE_EXISTS),(FILE_EXISTS(48),CLASS_RUN_SPECS_EXISTS), &
                  (FILE_EXISTS(49),CLASS_REVENUE_FORECAST_EXISTS),(FILE_EXISTS(50),ELIMINATIONS_EXISTS), &
                  (FILE_EXISTS(51),DETAILED_REPORTS_EXISTS),(FILE_EXISTS(52),NEW_RUN_SPECS_EXISTS), &
                  (FILE_EXISTS(53),CATAWBA2_CONTRACT_EXISTS),(FILE_EXISTS(54),UNIT_OUTAGE_EXISTS), &
                  (FILE_EXISTS(55),TRANSACT_TIE_EXISTS),(FILE_EXISTS(56),MONTHLY_PAYABLES_EXISTS), &
                  (FILE_EXISTS(57),DAY_TYPE_EXISTS),(FILE_EXISTS(58),SCENARIO_MAKER_EXISTS), &
                  (FILE_EXISTS(59),ENERGY_PRODUCTS_EXISTS),(FILE_EXISTS(60),MARKET_PRICE_EXISTS), &
                  (FILE_EXISTS(61),CPL_CONTRACT_EXISTS),   &
                  (FILE_EXISTS(62),RDI_POWERDAT_EXISTS),   &
                  (FILE_EXISTS(63),RDI_BASECASE_EXISTS),   &
                  (FILE_EXISTS(64),TRANS_FORECAST_EXISTS),   &
                  (FILE_EXISTS(65),TAX_INFO_FILE_EXISTS),   &
                  (FILE_EXISTS(66),ADDENDUM_FILE_EXISTS),   &
                  (FILE_EXISTS(67),TRANS_GROUPS_FILE_EXISTS),   &
                  (FILE_EXISTS(68),TRANS_PATHS_FILE_EXISTS),   &
                  (FILE_EXISTS(69),TRANS_CONSTRAINT_FILE_EXISTS),   &
                  (FILE_EXISTS(70),SCENARIO_PARAMETER_EXISTS),   &
                  (FILE_EXISTS(71),NEW_MARKET_PRICE_EXISTS),   &
                  (FILE_EXISTS(72),WEEKLY_HYDRO_EXISTS),   &
                  (FILE_EXISTS(73),SCENARIO_HOURLY_EXISTS),   &
                  (FILE_EXISTS(74),FORWARD_OPTION_EXISTS),   &
                  (FILE_EXISTS(78),WEATHER_DEMAND_EXISTS),   &
                  (FILE_EXISTS(84),MARKET_GROUPS_EXISTS),   &
                  (FILE_EXISTS(85),FUEL_PRICE_F1_EXISTS),   &
                  (FILE_EXISTS(86),FUEL_PRICE_F2_EXISTS),   &
                  (FILE_EXISTS(87),FUEL_PRICE_F3_EXISTS),   &
                  (FILE_EXISTS(88),FUEL_PRICE_F4_EXISTS),   &
                  (FILE_EXISTS(89),FUEL_PRICE_F5_EXISTS),   &
                  (FILE_EXISTS(90),FUEL_PRICE_F6_EXISTS),   &
                  (FILE_EXISTS(91),FUEL_PRICE_F7_EXISTS),   &
                  (FILE_EXISTS(92),FUEL_PRICE_F8_EXISTS),   &
                  (FILE_EXISTS(93),FUEL_PRICE_F9_EXISTS),   &
                  (FILE_EXISTS(94),CL_UNITS_C1_EXISTS),   &
                  (FILE_EXISTS(95),CL_UNITS_C2_EXISTS),   &
                  (FILE_EXISTS(96),CL_UNITS_C3_EXISTS),   &
                  (FILE_EXISTS(97),CL_UNITS_C4_EXISTS),   &
                  (FILE_EXISTS(98),CL_UNITS_C5_EXISTS),   &
                  (FILE_EXISTS(99),CL_UNITS_C6_EXISTS),   &
                  (FILE_EXISTS(100),CL_UNITS_C7_EXISTS),   &
                  (FILE_EXISTS(101),ProSym_Data_EXISTS),   &
                  (FILE_EXISTS(130),CL_UNITS_C8_EXISTS),   &
                  (FILE_EXISTS(131),CL_UNITS_C9_EXISTS),   &
                  (FILE_EXISTS(132),CL_UNITS_C0_EXISTS),   &
                  (FILE_EXISTS(133),CL_UNITS_CA_EXISTS),   &
                  (FILE_EXISTS(134),CL_UNITS_CB_EXISTS),   &
                  (FILE_EXISTS(135),CL_UNITS_CC_EXISTS),   &
                  (FILE_EXISTS(136),CL_UNITS_CD_EXISTS),   &
                  (FILE_EXISTS(137),EL_UNITS_E1_EXISTS),   &
                  (FILE_EXISTS(138),EL_UNITS_E2_EXISTS),   &
                  (FILE_EXISTS(139),EL_UNITS_E3_EXISTS),   &
                  (FILE_EXISTS(140),EL_UNITS_E4_EXISTS),   &
                  (FILE_EXISTS(141),EL_UNITS_E5_EXISTS),   &
                  (FILE_EXISTS(142),EL_UNITS_E6_EXISTS),   &
                  (FILE_EXISTS(142),EL_UNITS_E7_EXISTS),   &
                  (FILE_EXISTS(144),EL_UNITS_E8_EXISTS),   &
                  (FILE_EXISTS(145),EL_UNITS_E9_EXISTS),   &
                  (FILE_EXISTS(146),EL_UNITS_E0_EXISTS),   &
                  (FILE_EXISTS(147),EL_UNITS_EB_EXISTS),   &
                  (FILE_EXISTS(148),EL_UNITS_ED_EXISTS),   &
                  (FILE_EXISTS(149),EL_UNITS_EE_EXISTS),   &
                  (FILE_EXISTS(150),EL_UNITS_EF_EXISTS),   &
                  (FILE_EXISTS(151),TRANS_FORECAST_T1_EXISTS),   &
                  (FILE_EXISTS(152),TRANS_FORECAST_T2_EXISTS),   &
                  (FILE_EXISTS(153),TRANS_FORECAST_T3_EXISTS),   &
                  (FILE_EXISTS(154),TRANS_FORECAST_T4_EXISTS),   &
                  (FILE_EXISTS(155),TRANS_FORECAST_T5_EXISTS),   &
                  (FILE_EXISTS(156),TRANS_FORECAST_T6_EXISTS),   &
                  (FILE_EXISTS(157),TRANS_FORECAST_T7_EXISTS),   &
                  (FILE_EXISTS(158),TRANS_FORECAST_T8_EXISTS),   &
                  (FILE_EXISTS(159),TRANS_FORECAST_T9_EXISTS),   &
                  (FILE_EXISTS(160),TRANS_FORECAST_T0_EXISTS),   &
                  (FILE_EXISTS(161),TRANS_FORECAST_TB_EXISTS),   &
                  (FILE_EXISTS(162),TRANS_FORECAST_TD_EXISTS),   &
                  (FILE_EXISTS(163),TRANS_FORECAST_TH_EXISTS),   &
                  (FILE_EXISTS(164),TRANS_FORECAST_TI_EXISTS),   &
                  (FILE_EXISTS(165),CL_UNITS_CE_EXISTS),   &
                  (FILE_EXISTS(166),CL_UNITS_CG_EXISTS),   &
                  (FILE_EXISTS(167),EL_UNITS_EG_EXISTS),   &
                  (FILE_EXISTS(168),EL_UNITS_EH_EXISTS),   &
                  (FILE_EXISTS(169),REGIONAL_OUTAGES_EXISTS),   &
                  (FILE_EXISTS(170),EXPENSE_FILE_X1_EXISTS),   &
                  (FILE_EXISTS(171),EXPENSE_FILE_X2_EXISTS),   &
                  (FILE_EXISTS(172),EXPENSE_FILE_X3_EXISTS),   &
                  (FILE_EXISTS(173),EXPENSE_FILE_X4_EXISTS),   &
                  (FILE_EXISTS(174),EXPENSE_FILE_X5_EXISTS),   &
                  (FILE_EXISTS(175),SCENARIO_VECTOR_EXISTS),   &
                  (FILE_EXISTS(176),SM_UNITS_S1_EXISTS),   &
                  (FILE_EXISTS(177),SM_UNITS_S2_EXISTS),   &
                  (FILE_EXISTS(178),SM_UNITS_S3_EXISTS),   &
                  (FILE_EXISTS(179),SM_UNITS_S4_EXISTS),   &
                  (FILE_EXISTS(180),SM_UNITS_S5_EXISTS),   &
                  (FILE_EXISTS(181),SM_UNITS_S6_EXISTS),   &
                  (FILE_EXISTS(182),SM_UNITS_S7_EXISTS),   &
                  (FILE_EXISTS(183),SM_UNITS_S8_EXISTS),   &
                  (FILE_EXISTS(184),SM_UNITS_S9_EXISTS),   &
                  (FILE_EXISTS(185),USER_DAY_EXISTS),   &
                  (FILE_EXISTS(186),ICAP_EXISTS),   &
                  (FILE_EXISTS(187),REGIONAL_PARAMETER_EXISTS),   &
                  (FILE_EXISTS(188),FUEL_DERIVATIVES_EXISTS),   &
                  (FILE_EXISTS(189),FINANCIAL_HISTORY_EXISTS), & !  189
                  (FILE_EXISTS(190),MONTHLY_CAPITIAL_RATES_EXISTS),  & !  190
                  (FILE_EXISTS(191),WVPA_RATES_STRUCTURES_EXISTS),   &
                  (FILE_EXISTS(192),WVPA_TRACKER_FILE_EXISTS),  & !  192
                  (FILE_EXISTS(193),WVPA_COOP_REVENUE_FILE_EXISTS),  & !  193
                  (FILE_EXISTS(194),RM_UNITS_R1_EXISTS),   &
                  (FILE_EXISTS(195),RM_UNITS_R2_EXISTS),   &
                  (FILE_EXISTS(196),RM_UNITS_R3_EXISTS),        & !  196
                  (FILE_EXISTS(197),LH_GLOBAL_EXISTS),   &
                  (FILE_EXISTS(198),ELECT_HOUR_MULT_FILE_EXISTS),   &
                  (FILE_EXISTS(199),GAS_HOUR_MULT_FILE_EXISTS),   &
                  (FILE_EXISTS(200),OIL_HOUR_MULT_FILE_EXISTS), & !  200
                  (FILE_EXISTS(201),ENERGY_PRODUCTS_FILE_P2_EXISTS),   &
                  (FILE_EXISTS(202),ENERGY_PRODUCTS_FILE_P3_EXISTS),   &
                  (FILE_EXISTS(203),ENERGY_PRODUCTS_FILE_P4_EXISTS),   &
                  (FILE_EXISTS(204),ENERGY_PRODUCTS_FILE_P5_EXISTS),   &
                  (FILE_EXISTS(205),CL_UNITS_CI_EXISTS),   &
                  (FILE_EXISTS(206),CL_UNITS_CJ_EXISTS),   &
                  (FILE_EXISTS(207),CL_UNITS_CK_EXISTS),   &
                  (FILE_EXISTS(208),CL_UNITS_CM_EXISTS),   &
                  (FILE_EXISTS(209),CL_UNITS_CP_EXISTS),   &
                  (FILE_EXISTS(210),CL_UNITS_CQ_EXISTS),   &
                  (FILE_EXISTS(211),EL_UNITS_EI_EXISTS),   &
                  (FILE_EXISTS(212),ENERGY_PRODUCTS_FILE_P6_EXISTS),   &
                  (FILE_EXISTS(213),FX_FUTURE_ASSETS_CAPEX_EXISTS),   &
                  (FILE_EXISTS(258),TRAN_EXP_FILE_EXISTS),   &
                  (FILE_EXISTS(290),GAS_NODE_FILE_EXISTS),   &
                  (FILE_EXISTS(291),GAS_TRANSPORT_FILE_EXISTS),   &
                  (FILE_EXISTS(292),GAS_DEMAND_FORECAST_FILE_EXISTS),   &
                  (FILE_EXISTS(293),GAS_SUPPLY_FORECAST_FILE_EXISTS),   &
                  (FILE_EXISTS(294),GAS_FORECAST_T1_EXISTS),   &
                  (FILE_EXISTS(295),GAS_FORECAST_T2_EXISTS),   &
                  (FILE_EXISTS(296),GAS_FORECAST_T3_EXISTS),   &
                  (FILE_EXISTS(297),GAS_FORECAST_T4_EXISTS),   &
                  (FILE_EXISTS(298),GAS_FORECAST_T5_EXISTS),   &
                  (FILE_EXISTS(299),GAS_FORECAST_T6_EXISTS),   &
                  (FILE_EXISTS(300),GAS_FORECAST_T7_EXISTS),   &
                  (FILE_EXISTS(301),GAS_FORECAST_T8_EXISTS),   &
                  (FILE_EXISTS(302),GAS_FORECAST_T9_EXISTS),   &
                  (FILE_EXISTS(303),GAS_WEATHER_DEMAND_EXISTS),   &
                  (FILE_EXISTS(334),GAS_STORAGE_FILE_EXISTS),   &
                  (FILE_EXISTS(335),FUEL_PRICE_F0_EXISTS),   &
                  (FILE_EXISTS(351),CORES_AND_MACHINES_EXISTS),   &
                  (FILE_EXISTS(352),RPS_REQUIREMENTS_EXISTS),   &
                  (FILE_EXISTS(353),GQB_GAS_THERMAL_EXISTS),   &
                  (FILE_EXISTS(354),COAL_NODE_FILE_EXISTS),   &
                  (FILE_EXISTS(355),COAL_TRANSPORT_FILE_EXISTS),   &
                  (FILE_EXISTS(356),COAL_SUPPLY_FORECAST_FILE_EXISTS),   &
                  (FILE_EXISTS(357),COAL_DEMAND_FORECAST_FILE_EXISTS),   &
                  (FILE_EXISTS(358),COAL_FORECAST_T1_EXISTS),   &
                  (FILE_EXISTS(359),RPS_STATE_DEMAND_EXISTS),   &
                  (FILE_EXISTS(360),THERMAL_RETROFIT_EXISTS),   &
                  (FILE_EXISTS(361),COAL_CONTRACTS_EXISTS),   &
                  (FILE_EXISTS(362),CO2_PARAM_EXISTS),   &
                  (FILE_EXISTS(363),USER_DAY_D1_EXISTS),   &
                  (FILE_EXISTS(364),USER_DAY_D2_EXISTS),   &
                  (FILE_EXISTS(365),USER_DAY_D3_EXISTS),   &
                  (FILE_EXISTS(366),USER_DAY_D4_EXISTS),   &
                  (FILE_EXISTS(367),USER_DAY_D5_EXISTS),   &
                  (FILE_EXISTS(368),USER_DAY_D6_EXISTS),   &
                  (FILE_EXISTS(369),USER_DAY_D7_EXISTS),   &
                  (FILE_EXISTS(370),USER_DAY_D8_EXISTS),   &
                  (FILE_EXISTS(371),USER_DAY_D9_EXISTS),   &
                  (FILE_EXISTS(376),RPS_PROGRAM_EXISTS)

      EQUIVALENCE (OVERLAY_FILE_NAME(1),EAE_FILE),(OVERLAY_FILE_NAME(2),DDE_FILE),(OVERLAY_FILE_NAME(3),DBE_FILE),   &
                  (OVERLAY_FILE_NAME(4),FAE_FILE),(OVERLAY_FILE_NAME(5),NFE_FILE),(OVERLAY_FILE_NAME(6),EPE_FILE),   &
                  (OVERLAY_FILE_NAME(7),EXPENSE_NAMES_RESERVED),(OVERLAY_FILE_NAME(8),PRF_FILE),(OVERLAY_FILE_NAME(9),INE_FILE),  &
                  (OVERLAY_FILE_NAME(10),TAX_FILE),(OVERLAY_FILE_NAME(11),INT_FILE),(OVERLAY_FILE_NAME(12),FIN_FILE),   &
                  (OVERLAY_FILE_NAME(13),ESC_FILE),(OVERLAY_FILE_NAME(14),FOS_FILE),(OVERLAY_FILE_NAME(15),HYD_FILE),   &
                  (OVERLAY_FILE_NAME(16),UPE_FILE),(OVERLAY_FILE_NAME(17),SFC_FILE),(OVERLAY_FILE_NAME(18),CFC_FILE),   &
                  (OVERLAY_FILE_NAME(19),RFC_FILE),(OVERLAY_FILE_NAME(20),IFC_FILE),(OVERLAY_FILE_NAME(21),O1F_FILE),   &
                  (OVERLAY_FILE_NAME(22),O2F_FILE),(OVERLAY_FILE_NAME(23),O3F_FILE),(OVERLAY_FILE_NAME(24),CLS_FILE),   &
                  (OVERLAY_FILE_NAME(25),CLH_FILE),(OVERLAY_FILE_NAME(26),LMG_FILE),(OVERLAY_FILE_NAME(27),LMS_FILE),   &
                  (OVERLAY_FILE_NAME(28),PFD_FILE),(OVERLAY_FILE_NAME(29),PNP_FILE),(OVERLAY_FILE_NAME(30),FNP_FILE),   &
                  (OVERLAY_FILE_NAME(31),CNP_FILE),(OVERLAY_FILE_NAME(32),CRA_FILE),(OVERLAY_FILE_NAME(33),COS_FILE),   &
                  (OVERLAY_FILE_NAME(34),REFERENCE_LOAD_DATA_FILE),   &
                  (OVERLAY_FILE_NAME(35),LFI_FILE),   &
                  (OVERLAY_FILE_NAME(36),ENV_FILE),   &
                  (OVERLAY_FILE_NAME(37),ECO_FILE),   &
                  (OVERLAY_FILE_NAME(38),ASTRT_FILE),   &
                  (OVERLAY_FILE_NAME(39),ASTVEC_FILE),   &
                  (OVERLAY_FILE_NAME(40),CNT_FILE),   &
                  (OVERLAY_FILE_NAME(41),PRODP_FILE),   &
                  (OVERLAY_FILE_NAME(42),AREA_FILE),   &
                  (OVERLAY_FILE_NAME(43),FUEL_INVENTORY_FILE),   &
                  (OVERLAY_FILE_NAME(44),FUEL_PRICE_FILE),   &
                  (OVERLAY_FILE_NAME(45),SERVICE_TRANSACTIONS_FILE),   &
                  (OVERLAY_FILE_NAME(46),FUEL_EMISSIONS_FILE),   &
                  (OVERLAY_FILE_NAME(47),OVER_UNDER_TABLE_FILE),   &
                  (OVERLAY_FILE_NAME(48),CLASS_RUN_SPECS_FILE),   &
                  (OVERLAY_FILE_NAME(49),CLASS_REVENUE_FORECAST_FILE),   &
                  (OVERLAY_FILE_NAME(50),ELIMINATIONS_FILE),   &
                  (OVERLAY_FILE_NAME(51),DETAILED_REPORTS_FILE),   &
                  (OVERLAY_FILE_NAME(52),NEW_RUN_SPECS_FILE),   &
                  (OVERLAY_FILE_NAME(53),CATAWBA2_CONTRACT_FILE),   &
                  (OVERLAY_FILE_NAME(54),UNIT_OUTAGE_FILE),   &
                  (OVERLAY_FILE_NAME(55),TRANSACT_TIE_FILE),   &
                  (OVERLAY_FILE_NAME(56),MONTHLY_PAYABLES_FILE),   &
                  (OVERLAY_FILE_NAME(57),DAY_TYPE_FILE),   &
                  (OVERLAY_FILE_NAME(58),SCENARIO_MAKER_FILE),   &
                  (OVERLAY_FILE_NAME(59),ENERGY_PRODUCTS_FILE),   &
                  (OVERLAY_FILE_NAME(60),MARKET_PRICE_FILE),   &
                  (OVERLAY_FILE_NAME(61),CPL_CONTRACT_FILE),   &
                  (OVERLAY_FILE_NAME(62),RDI_POWERDAT_FILE),   &
                  (OVERLAY_FILE_NAME(63),RDI_BASECASE_FILE),   &
                  (OVERLAY_FILE_NAME(64),TRANS_FORECAST_FILE),   &
                  (OVERLAY_FILE_NAME(65),TAX_INFO_FILE),   &
                  (OVERLAY_FILE_NAME(66),ADDENDUM_FILE),   &
                  (OVERLAY_FILE_NAME(67),TRANS_GROUPS_FILE),   &
                  (OVERLAY_FILE_NAME(68),TRANS_PATHS_FILE),   &
                  (OVERLAY_FILE_NAME(69),TRANS_CONSTRAINT_FILE),   &
                  (OVERLAY_FILE_NAME(70),SCENARIO_PARAMETER),   &
                  (OVERLAY_FILE_NAME(71),NEW_MARKET_PRICE_FILE),   &
                  (OVERLAY_FILE_NAME(72),WEEKLY_HYDRO_FILE),   &
                  (OVERLAY_FILE_NAME(73),SCENARIO_HOURLY_FILE),   &
                  (OVERLAY_FILE_NAME(74),FORWARD_OPTION_FILE),   &
                  (OVERLAY_FILE_NAME(78),WEATHER_DEMAND_FILE),   &
                  (OVERLAY_FILE_NAME(84),MARKET_GROUPS_FILE),   &
                  (OVERLAY_FILE_NAME(85),FUEL_PRICE_F1_FILE),   &
                  (OVERLAY_FILE_NAME(86),FUEL_PRICE_F2_FILE),   &
                  (OVERLAY_FILE_NAME(87),FUEL_PRICE_F3_FILE),   &
                  (OVERLAY_FILE_NAME(88),FUEL_PRICE_F4_FILE),   &
                  (OVERLAY_FILE_NAME(89),FUEL_PRICE_F5_FILE),   &
                  (OVERLAY_FILE_NAME(90),FUEL_PRICE_F6_FILE),   &
                  (OVERLAY_FILE_NAME(91),FUEL_PRICE_F7_FILE),   &
                  (OVERLAY_FILE_NAME(92),FUEL_PRICE_F8_FILE),   &
                  (OVERLAY_FILE_NAME(93),FUEL_PRICE_F9_FILE),   &
                  (OVERLAY_FILE_NAME(94),CL_UNITS_C1_FILE),   &
                  (OVERLAY_FILE_NAME(95),CL_UNITS_C2_FILE),   &
                  (OVERLAY_FILE_NAME(96),CL_UNITS_C3_FILE),   &
                  (OVERLAY_FILE_NAME(97),CL_UNITS_C4_FILE),   &
                  (OVERLAY_FILE_NAME(98),CL_UNITS_C5_FILE),   &
                  (OVERLAY_FILE_NAME(99),CL_UNITS_C6_FILE),   &
                  (OVERLAY_FILE_NAME(100),CL_UNITS_C7_FILE),   &
                  (OVERLAY_FILE_NAME(101),ProSym_Data_FILE),   &
                  (OVERLAY_FILE_NAME(130),CL_UNITS_C8_FILE),   &
                  (OVERLAY_FILE_NAME(131),CL_UNITS_C9_FILE),   &
                  (OVERLAY_FILE_NAME(132),CL_UNITS_C0_FILE),   &
                  (OVERLAY_FILE_NAME(133),CL_UNITS_CA_FILE),   &
                  (OVERLAY_FILE_NAME(134),CL_UNITS_CB_FILE),   &
                  (OVERLAY_FILE_NAME(135),CL_UNITS_CC_FILE),   &
                  (OVERLAY_FILE_NAME(136),CL_UNITS_CD_FILE),   &
                  (OVERLAY_FILE_NAME(137),EL_UNITS_E1_FILE),   &
                  (OVERLAY_FILE_NAME(138),EL_UNITS_E2_FILE),   &
                  (OVERLAY_FILE_NAME(139),EL_UNITS_E3_FILE),   &
                  (OVERLAY_FILE_NAME(140),EL_UNITS_E4_FILE),   &
                  (OVERLAY_FILE_NAME(141),EL_UNITS_E5_FILE),   &
                  (OVERLAY_FILE_NAME(142),EL_UNITS_E6_FILE),   &
                  (OVERLAY_FILE_NAME(142),EL_UNITS_E7_FILE),   &
                  (OVERLAY_FILE_NAME(144),EL_UNITS_E8_FILE),   &
                  (OVERLAY_FILE_NAME(145),EL_UNITS_E9_FILE),   &
                  (OVERLAY_FILE_NAME(146),EL_UNITS_E0_FILE),   &
                  (OVERLAY_FILE_NAME(147),EL_UNITS_EB_FILE),   &
                  (OVERLAY_FILE_NAME(148),EL_UNITS_ED_FILE),   &
                  (OVERLAY_FILE_NAME(149),EL_UNITS_EE_FILE),   &
                  (OVERLAY_FILE_NAME(150),EL_UNITS_EF_FILE),   &
                  (OVERLAY_FILE_NAME(151),TRANS_FORECAST_T1_FILE),   &
                  (OVERLAY_FILE_NAME(152),TRANS_FORECAST_T2_FILE),   &
                  (OVERLAY_FILE_NAME(153),TRANS_FORECAST_T3_FILE),   &
                  (OVERLAY_FILE_NAME(154),TRANS_FORECAST_T4_FILE),   &
                  (OVERLAY_FILE_NAME(155),TRANS_FORECAST_T5_FILE),   &
                  (OVERLAY_FILE_NAME(156),TRANS_FORECAST_T6_FILE),   &
                  (OVERLAY_FILE_NAME(157),TRANS_FORECAST_T7_FILE),   &
                  (OVERLAY_FILE_NAME(158),TRANS_FORECAST_T8_FILE),   &
                  (OVERLAY_FILE_NAME(159),TRANS_FORECAST_T9_FILE),   &
                  (OVERLAY_FILE_NAME(160),TRANS_FORECAST_T0_FILE),   &
                  (OVERLAY_FILE_NAME(161),TRANS_FORECAST_TB_FILE),   &
                  (OVERLAY_FILE_NAME(162),TRANS_FORECAST_TD_FILE),   &
                  (OVERLAY_FILE_NAME(163),TRANS_FORECAST_TH_FILE),   &
                  (OVERLAY_FILE_NAME(164),TRANS_FORECAST_TI_FILE),   &
                  (OVERLAY_FILE_NAME(165),CL_UNITS_CE_FILE),   &
                  (OVERLAY_FILE_NAME(166),CL_UNITS_CG_FILE),   &
                  (OVERLAY_FILE_NAME(167),EL_UNITS_EG_FILE),   &
                  (OVERLAY_FILE_NAME(168),EL_UNITS_EH_FILE),   &
                  (OVERLAY_FILE_NAME(169),REGIONAL_OUTAGES_FILE),   &
                  (OVERLAY_FILE_NAME(170),EXPENSE_FILE_X1),   &
                  (OVERLAY_FILE_NAME(171),EXPENSE_FILE_X2),   &
                  (OVERLAY_FILE_NAME(172),EXPENSE_FILE_X3),   &
                  (OVERLAY_FILE_NAME(173),EXPENSE_FILE_X4),   &
                  (OVERLAY_FILE_NAME(174),EXPENSE_FILE_X5),   &
                  (OVERLAY_FILE_NAME(175),SCENARIO_VECTOR_FILE),   &
                  (OVERLAY_FILE_NAME(176),SM_UNITS_S1),   &
                  (OVERLAY_FILE_NAME(177),SM_UNITS_S2),   &
                  (OVERLAY_FILE_NAME(178),SM_UNITS_S3),   &
                  (OVERLAY_FILE_NAME(179),SM_UNITS_S4),   &
                  (OVERLAY_FILE_NAME(180),SM_UNITS_S5),   &
                  (OVERLAY_FILE_NAME(181),SM_UNITS_S6),   &
                  (OVERLAY_FILE_NAME(182),SM_UNITS_S7),   &
                  (OVERLAY_FILE_NAME(183),SM_UNITS_S8),   &
                  (OVERLAY_FILE_NAME(184),SM_UNITS_S9),   &
                  (OVERLAY_FILE_NAME(185),USER_DAY_FILE),   &
                  (OVERLAY_FILE_NAME(186),ICAP_FILE),   &
                  (OVERLAY_FILE_NAME(187),REGIONAL_PARAMETER_FILE),   &
                  (OVERLAY_FILE_NAME(188),FUEL_DERIVATIVES_FILE),   &
                  (OVERLAY_FILE_NAME(189),FINANCIAL_HISTORY_FILE), & !  189
                  (OVERLAY_FILE_NAME(190),MONTHLY_CAPITIAL_RATES_FILE), & !  190
                  (OVERLAY_FILE_NAME(191),WVPA_RATES_STRUCTURES),   &
                  (OVERLAY_FILE_NAME(192),WVPA_TRACKER_FILE),  & !  192
                  (OVERLAY_FILE_NAME(193),WVPA_COOP_REVENUE_FILE),  & !  193
                  (OVERLAY_FILE_NAME(194),RM_UNITS_R1),   &
                  (OVERLAY_FILE_NAME(195),RM_UNITS_R2),   &
                  (OVERLAY_FILE_NAME(196),RM_UNITS_R3), & !  196
                  (OVERLAY_FILE_NAME(197),LH_GLOBAL),   &
                  (OVERLAY_FILE_NAME(198),ELECT_HOUR_MULT_FILE),   &
                  (OVERLAY_FILE_NAME(199),GAS_HOUR_MULT_FILE),   &
                  (OVERLAY_FILE_NAME(200),OIL_HOUR_MULT_FILE),   &
                  (OVERLAY_FILE_NAME(201),ENERGY_PRODUCTS_FILE_P2),   &
                  (OVERLAY_FILE_NAME(202),ENERGY_PRODUCTS_FILE_P3),   &
                  (OVERLAY_FILE_NAME(203),ENERGY_PRODUCTS_FILE_P4),   &
                  (OVERLAY_FILE_NAME(204),ENERGY_PRODUCTS_FILE_P5),   &
                  (OVERLAY_FILE_NAME(205),CL_UNITS_CI_FILE),   &
                  (OVERLAY_FILE_NAME(206),CL_UNITS_CJ_FILE),   &
                  (OVERLAY_FILE_NAME(207),CL_UNITS_CK_FILE),   &
                  (OVERLAY_FILE_NAME(208),CL_UNITS_CM_FILE),   &
                  (OVERLAY_FILE_NAME(209),CL_UNITS_CP_FILE),   &
                  (OVERLAY_FILE_NAME(210),CL_UNITS_CQ_FILE),   &
                  (OVERLAY_FILE_NAME(211),EL_UNITS_EI_FILE),   &
                  (OVERLAY_FILE_NAME(212),ENERGY_PRODUCTS_FILE_P6),   &
                  (OVERLAY_FILE_NAME(213),FX_FUTURE_ASSETS_CAPEX_FILE),   &
                  (OVERLAY_FILE_NAME(258),TRAN_EXP_FILE),   &
                  (OVERLAY_FILE_NAME(290),GAS_NODE_FILE),   &
                  (OVERLAY_FILE_NAME(291),GAS_TRANSPORT_FILE),   &
                  (OVERLAY_FILE_NAME(292),GAS_DEMAND_FORECAST_FILE),   &
                  (OVERLAY_FILE_NAME(293),GAS_SUPPLY_FORECAST_FILE),   &
                  (OVERLAY_FILE_NAME(294),GAS_FORECAST_T1),   &
                  (OVERLAY_FILE_NAME(295),GAS_FORECAST_T2),   &
                  (OVERLAY_FILE_NAME(296),GAS_FORECAST_T3),   &
                  (OVERLAY_FILE_NAME(297),GAS_FORECAST_T4),   &
                  (OVERLAY_FILE_NAME(298),GAS_FORECAST_T5),   &
                  (OVERLAY_FILE_NAME(299),GAS_FORECAST_T6),   &
                  (OVERLAY_FILE_NAME(300),GAS_FORECAST_T7),   &
                  (OVERLAY_FILE_NAME(301),GAS_FORECAST_T8),   &
                  (OVERLAY_FILE_NAME(302),GAS_FORECAST_T9),   &
                  (OVERLAY_FILE_NAME(303),GAS_WEATHER_DEMAND_FILE),   &
                  (OVERLAY_FILE_NAME(334),GAS_STORAGE_FILE),   &
                  (OVERLAY_FILE_NAME(335),FUEL_PRICE_F0_FILE),   &
                  (OVERLAY_FILE_NAME(351),CORES_AND_MACHINES_FILE),   &
                  (OVERLAY_FILE_NAME(352),RPS_REQUIREMENTS_FILE),   &
                  (OVERLAY_FILE_NAME(353),GQB_GAS_THERMAL_FILE),   &
                  (OVERLAY_FILE_NAME(354),COAL_NODE_FILE),   &
                  (OVERLAY_FILE_NAME(355),COAL_TRANSPORT_FILE),   &
                  (OVERLAY_FILE_NAME(356),COAL_SUPPLY_FORECAST_FILE),   &
                  (OVERLAY_FILE_NAME(357),COAL_DEMAND_FORECAST_FILE),   &
                  (OVERLAY_FILE_NAME(358),COAL_FORECAST_T1),   &
                  (OVERLAY_FILE_NAME(359),RPS_STATE_DEMAND),   &
                  (OVERLAY_FILE_NAME(360),THERMAL_RETROFIT),   &
                  (OVERLAY_FILE_NAME(361),COAL_CONTRACTS),   &
                  (OVERLAY_FILE_NAME(362),CO2_PARAM),   &
                  (OVERLAY_FILE_NAME(363),USER_DAY_D1),      & !  363
                  (OVERLAY_FILE_NAME(364),USER_DAY_D2),      & !  364
                  (OVERLAY_FILE_NAME(365),USER_DAY_D3),      & !  365
                  (OVERLAY_FILE_NAME(366),USER_DAY_D4),      & !  366
                  (OVERLAY_FILE_NAME(367),USER_DAY_D5),      & !  367
                  (OVERLAY_FILE_NAME(368),USER_DAY_D6),      & !  368
                  (OVERLAY_FILE_NAME(369),USER_DAY_D7),      & !  369
                  (OVERLAY_FILE_NAME(370),USER_DAY_D8),      & !  370
                  (OVERLAY_FILE_NAME(371),USER_DAY_D9),       & !  371
                  (OVERLAY_FILE_NAME(372),COAL_MODEL_POINTER_FILE),       & !  372
                  (OVERLAY_FILE_NAME(373),COAL_MODEL_SO2_INFO_FILE), & !  373
                  (OVERLAY_FILE_NAME(374),   &
                                     COAL_MODEL_GENERIC_TRANSPORT_FILE),  & !  374
                  (OVERLAY_FILE_NAME(375),   &
                                        COAL_MODEL_GENERIC_DEMAND_FILE),  & !  375
                  (OVERLAY_FILE_NAME(376),RPS_PROGRAM_FILE)

      DATA FILE_TYPES/'EAO','DDO','DBO','FAO','NFO','EXO','PAO','PFO','INO','TXO','ITO','FRO','ESO','CLO','ELO','UPO',   &
                      'SFO','CFO','RFO','IFO','O1O','O2O','O3O','CSO','CHO','LGO','LSO','FPO','PNO','FNO','CNO','CRO',   &
                      'COO','LDE','LFO','ENO','ECO','ARO','FVO','CTO','PPO','AAO','FIO','FCO','TRO','FEO','DAO','ASO',   &
                      'RCO','EMO','DEO','RSO','C2O','TEO','TTO','MPO','DTO','SMO','TPO','MPO','PAO','RDO','RBO','TFO',   &
                      'TIO','ADO','TGO','IPO','TNO','SPO','PRO','WHO','SHO','FOO','FBO','WCO','WTO','WDO','79O','80O',   &
                      '81O','82O','83O','84O','F1O','F2O','F3O','F4O','F5O','F6O','F7O','F8O','F9O','C1O','C2O','C3O',   &
                      'C4O','C5O','C6O','C7O','PSO','XXX','XXX','XXX','XXX','XXX','XXX','XXX','XXX','XXX','XXX','XXX',   &
                      'XXX','XXX','XXX','XXX','XXX','XXX','XXX','XXX','XXX','XXX','XXX','XXX','XXX','XXX','XXX','XXX',   &
                      'XXX','C8O','C9O','C0O','CAO','CBO','CCO','CDO','E1O','E2O','E3O','E4O','E5O','E6O','E7O','E8O',   &
                      'E9O','E0O','EBO','EDO','EEO','EFO','T1O','T2O','T3O','T4O','T5O','T6O','T7O','T8O','T9O','T0O',   &
                      'TBO','TDO','THO','TIO','CEO','CGO','EGO','EHO','ROO','X1O','X2O','X3O','X4O','X5O','SVO','S1O',   &
                      'S2O','S3O','S4O','S5O','S6O','S7O','S8O','S9O','UDO','ICO','RMO','GPO','FHO','MRO','WRO','RTO',   &
                      'RCO','R1O','R2O','R3O','SGO','EHO','GHO','OHO','P2O','P3O','P4O','P5O','CIO','CJO','CKO','CMO',   &
                      'CPO','CQO','EIO','P6O','FXO','XXX','XXX','XXX','XXX','XXX','XXX','XXX','XXX','XXX','XXX','XXX',   &
                      'XXX','XXX','XXX','XXX','XXX','XXX','XXX','XXX','XXX','XXX','XXX','XXX','XXX','XXX','XXX','XXX',   & !  240
                      'XXX','XXX','XXX','XXX','XXX','XXX','XXX','XXX',   & !  248
                      'XXX','XXX','XXX','XXX','XXX','XXX','XXX','XXX',   & !  256
                      'XXX','ATO','XXX','XXX','XXX','XXX','XXX','XXX',   & !  264
                      'XXX','XXX','XXX','XXX','XXX','XXX','XXX','XXX',   & !  272
                      'XXX','XXX','XXX','XXX','XXX','XXX','XXX','XXX',   & !  280
                      'XXX','XXX','XXX','XXX','XXX','XXX','XXX','XXX',   & !  288
                      'XXX','GNO','GTO','GDO','GSO','G1O','G2O','G3O',   & !  296
                      'G4O','G5O','G6O','G7O','G8O','G9O','GWO','XXX',   & !  304
                      'XXX','XXX','CSO','CUO','CVO','CWO','CXO','CYO',   & !  312
                      'N1O','N2O','N3O','N4O','U1O','U2O','U3O','U4O',   & !  320
                      'LTO','LNO','LDO','L1O','L2O','L3O','L4O','L5O',   &
                      'L6O','L7O','LCO','GTO','GKO','GGO','F0O','XXX',   & !  336
                      'XXX','XXX','XXX','XXX','XXX','XXX','XXX','N5O',   & !  344
                      'U5O','U6O','U7O','U8O','U9O','XXX','MCO','RRO',   & !  352
                      'GQO','HNO','HTO','HSO','H1O','H2O','RPO','RTO',   & !  360
                      'FTO','2OO','D1O','D2O','D3O','D4O','D5O','D6O',   & !  368
                      'D7O','D8O','D9O','CZO','ZCO','ZDO','ZEO','RQO',   & !  376
                      'TAO','TCO','EJO','EKO','EOO','EPO','ERO','ETO',   & !  384
                      'P0O','P1O','MXO','MYO'/                           ! 388

!  CHECK FOR EXISTENCE OF OVERLAY FILES
!
      FILE_EXISTS = .FALSE.
      MATCH_FOUND = .FALSE.
      SAVED_OVLNAME = OVLNAME
      OVERLAY_BOOK_IS_ACTIVE = OVERLAY_BOOK_ACTIVE()
      CALL RETURN_OVERLAY_FAMILY_LIST(OVLNAME,MATCH_FOUND,OVERLAY_FILE_NAME,FILE_EXISTS)
      IF(MATCH_FOUND) THEN
! 12/14/10 COAL PRICE MODEL CALLS
!
         CALL COAL_BASIN_MAKEOVL(COAL_NODE_FILE)                     ! 354
         CALL COAL_TRANSPORT_LINKS_MAKEOVL(COAL_TRANSPORT_FILE,'HT') ! 355
         CALL COAL_SUPPLY_MAKEOVL(COAL_SUPPLY_FORECAST_FILE)         ! 356
         CALL PLANT_DEMAND_MAKEOVL(COAL_DEMAND_FORECAST_FILE)        ! 357
         CALL COAL_CONTRACTS_MAKEOVL(COAL_CONTRACTS)                 ! 361
         CALL COAL_ESCALATIONS_MAKEOVL(COAL_MODEL_POINTER_FILE)      ! 372
         CALL COAL_SO2_INFO_MAKEOVL(COAL_MODEL_SO2_INFO_FILE)        ! 373
         IF(RUN_COAL_MODEL_ONLY()) RETURN
         CALL COAL_TRANSPORT_LINKS_MAKEOVL(COAL_MODEL_GENERIC_TRANSPORT_FILE,'ZD')  ! 374
         CALL GENERIC_DEMAND_MAKEOVL(COAL_MODEL_GENERIC_DEMAND_FILE)  ! 375
!
! CONVERT THE FOSSIL FILE
!
         IF(FOS_EXISTS) CALL CL_MAKEOVL(FOS_FILE,0)
         IF(CL_UNITS_C1_EXISTS) CALL CL_MAKEOVL(CL_UNITS_C1_FILE,1)
         IF(CL_UNITS_C2_EXISTS) CALL CL_MAKEOVL(CL_UNITS_C2_FILE,2)
         IF(CL_UNITS_C3_EXISTS) CALL CL_MAKEOVL(CL_UNITS_C3_FILE,3)
         IF(CL_UNITS_C4_EXISTS) CALL CL_MAKEOVL(CL_UNITS_C4_FILE,4)
         IF(CL_UNITS_C5_EXISTS) CALL CL_MAKEOVL(CL_UNITS_C5_FILE,5)
         IF(CL_UNITS_C6_EXISTS) CALL CL_MAKEOVL(CL_UNITS_C6_FILE,6)
         IF(CL_UNITS_C7_EXISTS) CALL CL_MAKEOVL(CL_UNITS_C7_FILE,7)
         IF(CL_UNITS_C8_EXISTS) CALL CL_MAKEOVL(CL_UNITS_C8_FILE,8)
         IF(CL_UNITS_C9_EXISTS) CALL CL_MAKEOVL(CL_UNITS_C9_FILE,9)
         IF(CL_UNITS_C0_EXISTS) CALL CL_MAKEOVL(CL_UNITS_C0_FILE,10)
         IF(CL_UNITS_CA_EXISTS) CALL CL_MAKEOVL(CL_UNITS_CA_FILE,11)
         IF(CL_UNITS_CB_EXISTS) CALL CL_MAKEOVL(CL_UNITS_CB_FILE,12)
         IF(CL_UNITS_CC_EXISTS) CALL CL_MAKEOVL(CL_UNITS_CC_FILE,13)
         IF(CL_UNITS_CD_EXISTS) CALL CL_MAKEOVL(CL_UNITS_CD_FILE,14)
         IF(CL_UNITS_CE_EXISTS) CALL CL_MAKEOVL(CL_UNITS_CE_FILE,15)
         IF(CL_UNITS_CG_EXISTS) CALL CL_MAKEOVL(CL_UNITS_CG_FILE,16)
         IF(CL_UNITS_CI_EXISTS) CALL CL_MAKEOVL(CL_UNITS_CI_FILE,17)
         IF(CL_UNITS_CJ_EXISTS) CALL CL_MAKEOVL(CL_UNITS_CJ_FILE,18)
         IF(CL_UNITS_CK_EXISTS) CALL CL_MAKEOVL(CL_UNITS_CK_FILE,19)
         IF(CL_UNITS_CM_EXISTS) CALL CL_MAKEOVL(CL_UNITS_CM_FILE,20)
         IF(CL_UNITS_CP_EXISTS) CALL CL_MAKEOVL(CL_UNITS_CP_FILE,21)
         IF(CL_UNITS_CQ_EXISTS) CALL CL_MAKEOVL(CL_UNITS_CQ_FILE,22)

         IF(FILE_EXISTS(307)) CALL CL_MAKEOVL(OVERLAY_FILE_NAME(307),24)  ! CL_UNITS_CS
         IF(FILE_EXISTS(308)) CALL CL_MAKEOVL(OVERLAY_FILE_NAME(308),25)  ! CL_UNITS_CU
         IF(FILE_EXISTS(309)) CALL CL_MAKEOVL(OVERLAY_FILE_NAME(309),26)  ! CL_UNITS_CV
         IF(FILE_EXISTS(310)) CALL CL_MAKEOVL(OVERLAY_FILE_NAME(310),27)  ! CL_UNITS_CW
         IF(FILE_EXISTS(311)) CALL CL_MAKEOVL(OVERLAY_FILE_NAME(311),28)  ! CL_UNITS_CX
         IF(FILE_EXISTS(312)) CALL CL_MAKEOVL(OVERLAY_FILE_NAME(312),29)  ! CL_UNITS_CY
         IF(FILE_EXISTS(379)) CALL CL_MAKEOVL(OVERLAY_FILE_NAME(379),30)
         IF(FILE_EXISTS(380)) CALL CL_MAKEOVL(OVERLAY_FILE_NAME(380),31)
         IF(FILE_EXISTS(383)) CALL CL_MAKEOVL(OVERLAY_FILE_NAME(383),32)
         IF(FILE_EXISTS(384)) CALL CL_MAKEOVL(OVERLAY_FILE_NAME(384),33)
         IF(FILE_EXISTS(387)) CALL CL_MAKEOVL(OVERLAY_FILE_NAME(387),34)
         IF(FILE_EXISTS(388)) CALL CL_MAKEOVL(OVERLAY_FILE_NAME(388),35)


!
! CHECKING OVERLAY OF THE ASSET VECTOR FILE SO THAT CLASS ID VECTORS ARE
! AVAILABLE 1/17/95
!
         IF(ASTVEC_EXISTS) CALL FV_MAKEOVL(ASTVEC_FILE)
         CALL OPEN_ASSET_VECTOR_FILE(82)
         IF(PRODP_EXISTS) CALL PP_MAKEOVL(PRODP_FILE) ! IN FRONT OF CL_MAKEOVL
!
! OVERLAY THE FINANCIAL PARAMETER FILE
!
         IF(UTILITY_TYPE() == 'P') THEN
            IF(PRF_EXISTS) CALL MUNI_PF_MAKEOVL(PRF_FILE)
!
! INITIALIZATION FILE OVERLAY
!
            IF(INE_EXISTS) CALL MUNI_IN_MAKEOVL(INE_FILE)
         ELSE
            IF(PRF_EXISTS) CALL PF_MAKEOVL(PRF_FILE)
!
! INITIALIZATION FILE OVERLAY
!
            IF(INE_EXISTS) CALL IN_MAKEOVL(INE_FILE)
         ENDIF
!
! MONTHLY FINANCAIL FILES PAYMENTS
!
         IF(MONTHLY_PAYABLES_EXISTS) CALL MONTHLY_PAYABLES_MAKEOVL(MONTHLY_PAYABLES_FILE)
!
! NEW PLANT FINANCIAL DATA
!
         IF(FNP_EXISTS) CALL FN_MAKEOVL(FNP_FILE)

!
! NEW PLANT PRODUCTION DATA
!
         IF(PNP_EXISTS) CALL PN_MAKEOVL(PNP_FILE)
!
! CAPITAL RATES DATA
!
         IF(MONTHLY_CAPITIAL_RATES_EXISTS) CALL MR_MAKEOVL(MONTHLY_CAPITIAL_RATES_FILE)
!  CONVERT DEBT FILE TO BINARY BASE CASE FORM
!
         IF(DBE_EXISTS) CALL DB_MAKEOVL(DBE_FILE)
!
!  EXISTING ASSET FILE CONVERSION
!
         IF(EAE_EXISTS) CALL EA_MAKEOVL(EAE_FILE)
!
!  CONVERT DEBIT FILES
!
         IF(DDE_EXISTS) CALL DD_MAKEOVL(DDE_FILE)
!
! CONVERT EXPENSE FILE
!
         IF(EPE_EXISTS) CALL EX_MAKEOVL(EPE_FILE,0)
         IF(EXPENSE_FILE_X1_EXISTS) CALL EX_MAKEOVL(EXPENSE_FILE_X1,1)
         IF(EXPENSE_FILE_X2_EXISTS) CALL EX_MAKEOVL(EXPENSE_FILE_X2,2)
         IF(EXPENSE_FILE_X3_EXISTS) CALL EX_MAKEOVL(EXPENSE_FILE_X3,3)
         IF(EXPENSE_FILE_X4_EXISTS) CALL EX_MAKEOVL(EXPENSE_FILE_X4,4)
         IF(EXPENSE_FILE_X5_EXISTS) CALL EX_MAKEOVL(EXPENSE_FILE_X5,5)
!
! CONVERT ADDEMDUM FILE
!
         IF(ADDENDUM_FILE_EXISTS) CALL ADDENDUMS_MAKEOVL(ADDENDUM_FILE)
!
! NUCLEAR FUEL FILE
!
         IF(NFE_EXISTS) CALL NF_MAKEOVL(NFE_FILE)
!
! TAX LOSS FILE
!
         IF(TAX_EXISTS) CALL TX_MAKEOVL(TAX_FILE)
!
! FUTASSET ASSET FILE
!
         IF(FAE_EXISTS) CALL FA_MAKEOVL(FAE_FILE,0)
         IF(FX_FUTURE_ASSETS_CAPEX_EXISTS) CALL FA_MAKEOVL(FX_FUTURE_ASSETS_CAPEX_FILE,1)
!
! CONVERSION OF THE EXPANSION CANIDATE FILE
!
         IF(CNP_EXISTS) CALL CN_MAKEOVL(CNP_FILE)
!
!     ENERGY LIMITED RESOURCE FILE
!
         CALL EL_MAKEOVL(OVERLAY_FILE_NAME,FILE_EXISTS)

!
!     LOAD MANAGEMENT APPLICATION FILE
!
         IF(LMG_EXISTS) CALL LG_MAKEOVL(LMG_FILE)
!
!     LOAD MANAGEMENT DEVICE FILE
!
         IF(LMS_EXISTS) CALL LS_MAKEOVL(LMS_FILE)
!
! CONVERT LOAD MANAGEMENT FINANCIAL OVERLAY FILE TO BINARY
! LFO ENABLED 3/12/90
!
         IF(LFI_EXISTS) CALL LF_MAKEOVL(LFI_FILE)
!
! ESCALATION RATES FILE
!
         IF(ESC_EXISTS) CALL ES_MAKEOVL(ESC_FILE,0)
         IF(FILE_EXISTS(313)) CALL ES_MAKEOVL(OVERLAY_FILE_NAME(313),1)
         IF(FILE_EXISTS(314)) CALL ES_MAKEOVL(OVERLAY_FILE_NAME(314),2)
         IF(FILE_EXISTS(315)) CALL ES_MAKEOVL(OVERLAY_FILE_NAME(315),3)
         IF(FILE_EXISTS(316)) CALL ES_MAKEOVL(OVERLAY_FILE_NAME(316),4)
         IF(FILE_EXISTS(344)) CALL ES_MAKEOVL(OVERLAY_FILE_NAME(344),5)
!
! ECONOMY INTERCHANGE FILE
!
         IF(ECO_EXISTS) THEN
            INQUIRE(FILE=trim(OUTPUT_DIRECTORY())//"BCECINT.BIN",EXIST=BIN_FILE_EXISTS)
            IF(BIN_FILE_EXISTS) CALL EC_MAKEOVL(ECO_FILE)
         ENDIF
!
! SYSTEM FORECAST FILE
!
         IF(SFC_EXISTS .AND. SYSTEM_BASED_FORECAST()) THEN
            CALL SF_MAKEOVL(SFC_FILE)
         ELSEIF(.NOT. SYSTEM_BASED_FORECAST()) THEN
            CALL CF_MAKEOVL(RFC_FILE,IFC_FILE,CFC_FILE,   &
                            O1F_FILE,O2F_FILE,O3F_FILE,   &
                            RFC_EXISTS,IFC_EXISTS,CFC_EXISTS,   &
                            O1F_EXISTS,O2F_EXISTS,O3F_EXISTS)
            IF(CLASS_BASED_FORECAST()) THEN
               IF(CLS_EXISTS) CALL CS_MAKEOVL(CLS_FILE)
               IF(CLH_EXISTS) CALL CH_MAKEOVL(CLH_FILE)
            ENDIF
         ENDIF
!
! CONVERT GENERATING UNIT PARAMETER FILE TO BINARY AND RANDOM ACCESS
!
         IF(UPE_EXISTS) CALL UP_MAKEOVL(UPE_FILE,0)
         IF(FILE_EXISTS(317)) CALL UP_MAKEOVL(OVERLAY_FILE_NAME(317),1)
         IF(FILE_EXISTS(318)) CALL UP_MAKEOVL(OVERLAY_FILE_NAME(318),2)
         IF(FILE_EXISTS(319)) CALL UP_MAKEOVL(OVERLAY_FILE_NAME(319),3)
         IF(FILE_EXISTS(320)) CALL UP_MAKEOVL(OVERLAY_FILE_NAME(320),4)
         IF(FILE_EXISTS(345)) CALL UP_MAKEOVL(OVERLAY_FILE_NAME(345),5)
         IF(FILE_EXISTS(346)) CALL UP_MAKEOVL(OVERLAY_FILE_NAME(346),6)
         IF(FILE_EXISTS(347)) CALL UP_MAKEOVL(OVERLAY_FILE_NAME(347),7)
         IF(FILE_EXISTS(348)) CALL UP_MAKEOVL(OVERLAY_FILE_NAME(348),8)
         IF(FILE_EXISTS(349)) CALL UP_MAKEOVL(OVERLAY_FILE_NAME(349),9)
!
! CONVERT COST ALLOCATION TO BINARY AND RANDOM ACCESS
!
         IF(COS_EXISTS) CALL CO_MAKEOVL(COS_FILE)
!
! PRICE AND ELASTICITY FILE
!
         IF(PFD_EXISTS) CALL FP_MAKEOVL(PFD_FILE)
!
! DETAILED_REPORTS OVERLAY
!
         IF(DETAILED_REPORTS_EXISTS) CALL DE_MAKEOVL(DETAILED_REPORTS_FILE)
!
! CATAWBA_CONTRACT OVERLAY
!
         IF(CATAWBA2_CONTRACT_EXISTS) CALL CAT2_MAKEOVL(CATAWBA2_CONTRACT_FILE)
!
! CPL CONTRACT OVERLAY
!
         IF(CPL_CONTRACT_EXISTS) CALL CPL_MAKEOVL(CPL_CONTRACT_FILE)
!
! RDI POWERDAT OVERLAY
!
         IF(RDI_POWERDAT_EXISTS) CALL RDI_MAKEOVL(RDI_POWERDAT_FILE)
!
! RDI BASECASE OVERLAY
!
         IF(RDI_BASECASE_EXISTS) CALL RBC_MAKEOVL(RDI_BASECASE_FILE)
!
! TRANSACTION FORECAST OVERLAY
!
         THIS_OVERLAY_FILE_EXISTS(0) = TRANS_FORECAST_EXISTS
         THIS_OVERLAY_FILE_EXISTS(1) = TRANS_FORECAST_T1_EXISTS
         THIS_OVERLAY_FILE_EXISTS(2) = TRANS_FORECAST_T2_EXISTS
         THIS_OVERLAY_FILE_EXISTS(3) = TRANS_FORECAST_T3_EXISTS
         THIS_OVERLAY_FILE_EXISTS(4) = TRANS_FORECAST_T4_EXISTS
         THIS_OVERLAY_FILE_EXISTS(5) = TRANS_FORECAST_T5_EXISTS
         THIS_OVERLAY_FILE_EXISTS(6) = TRANS_FORECAST_T6_EXISTS
         THIS_OVERLAY_FILE_EXISTS(7) = TRANS_FORECAST_T7_EXISTS
         THIS_OVERLAY_FILE_EXISTS(8) = TRANS_FORECAST_T8_EXISTS
         THIS_OVERLAY_FILE_EXISTS(9) = TRANS_FORECAST_T9_EXISTS
         THIS_OVERLAY_FILE_EXISTS(10) = TRANS_FORECAST_T0_EXISTS
         THIS_OVERLAY_FILE_EXISTS(11) = TRANS_FORECAST_TB_EXISTS
         THIS_OVERLAY_FILE_EXISTS(12) = TRANS_FORECAST_TD_EXISTS
         THIS_OVERLAY_FILE_EXISTS(13) = TRANS_FORECAST_TH_EXISTS
         THIS_OVERLAY_FILE_EXISTS(14) = TRANS_FORECAST_TI_EXISTS
         THIS_OVERLAY_FILE_EXISTS(15) = FILE_EXISTS(377)
         THIS_OVERLAY_FILE_EXISTS(16) = FILE_EXISTS(378)
!
! TEST FOR AN OVERLAY
!
         DO I = 0, 14
            IF(THIS_OVERLAY_FILE_EXISTS(I)) THEN
               OVERLAY_FILE_NAMES(0) = TRANS_FORECAST_FILE
               OVERLAY_FILE_NAMES(1) = TRANS_FORECAST_T1_FILE
               OVERLAY_FILE_NAMES(2) = TRANS_FORECAST_T2_FILE
               OVERLAY_FILE_NAMES(3) = TRANS_FORECAST_T3_FILE
               OVERLAY_FILE_NAMES(4) = TRANS_FORECAST_T4_FILE
               OVERLAY_FILE_NAMES(5) = TRANS_FORECAST_T5_FILE
               OVERLAY_FILE_NAMES(6) = TRANS_FORECAST_T6_FILE
               OVERLAY_FILE_NAMES(7) = TRANS_FORECAST_T7_FILE
               OVERLAY_FILE_NAMES(8) = TRANS_FORECAST_T8_FILE
               OVERLAY_FILE_NAMES(9) = TRANS_FORECAST_T9_FILE
               OVERLAY_FILE_NAMES(10) = TRANS_FORECAST_T0_FILE
               OVERLAY_FILE_NAMES(11) = TRANS_FORECAST_TB_FILE
               OVERLAY_FILE_NAMES(12) = TRANS_FORECAST_TD_FILE
               OVERLAY_FILE_NAMES(13) = TRANS_FORECAST_TH_FILE
               OVERLAY_FILE_NAMES(14) = TRANS_FORECAST_TI_FILE
               OVERLAY_FILE_NAMES(15) = OVERLAY_FILE_NAME(377)
               OVERLAY_FILE_NAMES(16) = OVERLAY_FILE_NAME(378)
               CALL TF_MAKEOVL(OVERLAY_FILE_NAMES,THIS_OVERLAY_FILE_EXISTS)
               EXIT
            ENDIF
         ENDDO
!
! TRANSACTION PATHS OVERLAY! 4/1/98. GAT.
!
         IF(TRANS_PATHS_FILE_EXISTS) CALL PATH_MAKEOVL(TRANS_PATHS_FILE)
!
! TRANSACTION CONSTRAINTS OVERLAY! 4/11/98. GAT.
!
         IF(TRANS_CONSTRAINT_FILE_EXISTS) CALL CONSTRAINT_MAKEOVL(TRANS_CONSTRAINT_FILE)
!
! TRANSACTION GROUPS OVERLAY! 1/9/98. GAT.
!
         IF(TRANS_GROUPS_FILE_EXISTS) CALL TG_MAKEOVL(TRANS_GROUPS_FILE)
!
! TRANSACT TIE OVERLAY
!
         IF(TRANSACT_TIE_EXISTS) CALL TIE_MAKEOVL(TRANSACT_TIE_FILE)
!
! DAY TYPE OVERLAY ! 4/7/97. GAT.
!
         IF(DAY_TYPE_EXISTS) CALL DAY_TYPE_MAKEOVL(DAY_TYPE_FILE)
!
! USER DAY OVERLAY ! 04/15/02. GAT. TMS.
!
         IF(USER_DAY_EXISTS) CALL USER_DAY_MAKEOVL(USER_DAY_FILE,0)
         IF(USER_DAY_D1_EXISTS) CALL USER_DAY_MAKEOVL(USER_DAY_D1,1)
         IF(USER_DAY_D2_EXISTS) CALL USER_DAY_MAKEOVL(USER_DAY_D2,2)
         IF(USER_DAY_D3_EXISTS) CALL USER_DAY_MAKEOVL(USER_DAY_D3,3)
         IF(USER_DAY_D4_EXISTS) CALL USER_DAY_MAKEOVL(USER_DAY_D4,4)
         IF(USER_DAY_D5_EXISTS) CALL USER_DAY_MAKEOVL(USER_DAY_D5,5)
         IF(USER_DAY_D6_EXISTS) CALL USER_DAY_MAKEOVL(USER_DAY_D6,6)
         IF(USER_DAY_D7_EXISTS) CALL USER_DAY_MAKEOVL(USER_DAY_D7,7)
         IF(USER_DAY_D8_EXISTS) CALL USER_DAY_MAKEOVL(USER_DAY_D8,8)
         IF(USER_DAY_D9_EXISTS) CALL USER_DAY_MAKEOVL(USER_DAY_D9,9)
!
! ICAP OVERLAY ! 04/26/02. GAT. TMS.
!
         IF(ICAP_EXISTS) CALL ICAP_MAKEOVL(ICAP_FILE)
!
! UNIT OUTAGE OVERLAY ! 3/4/99. GAT. JLG.
!
         IF(UNIT_OUTAGE_EXISTS) CALL OUTAGE_MAKEOVL(UNIT_OUTAGE_FILE)
!
! TRANSACTION FORECAST OVERLAY
!
         THIS_OVERLAY_FILE_EXISTS(0) = GAS_DEMAND_FORECAST_FILE_EXISTS
         THIS_OVERLAY_FILE_EXISTS(1) = GAS_FORECAST_T1_EXISTS
         THIS_OVERLAY_FILE_EXISTS(2) = GAS_FORECAST_T2_EXISTS
         THIS_OVERLAY_FILE_EXISTS(3) = GAS_FORECAST_T3_EXISTS
         THIS_OVERLAY_FILE_EXISTS(4) = GAS_FORECAST_T4_EXISTS
         THIS_OVERLAY_FILE_EXISTS(5) = GAS_FORECAST_T5_EXISTS
         THIS_OVERLAY_FILE_EXISTS(6) = GAS_FORECAST_T6_EXISTS
         THIS_OVERLAY_FILE_EXISTS(7) = GAS_FORECAST_T7_EXISTS
         THIS_OVERLAY_FILE_EXISTS(8) = GAS_FORECAST_T8_EXISTS
         THIS_OVERLAY_FILE_EXISTS(9) = GAS_FORECAST_T9_EXISTS
!
! TEST FOR AN OVERLAY
!
         DO I = 0, 14
            IF(THIS_OVERLAY_FILE_EXISTS(I)) THEN
               OVERLAY_FILE_NAMES(0) = GAS_DEMAND_FORECAST_FILE
               OVERLAY_FILE_NAMES(1) = GAS_FORECAST_T1
               OVERLAY_FILE_NAMES(2) = GAS_FORECAST_T2
               OVERLAY_FILE_NAMES(3) = GAS_FORECAST_T3
               OVERLAY_FILE_NAMES(4) = GAS_FORECAST_T4
               OVERLAY_FILE_NAMES(5) = GAS_FORECAST_T5
               OVERLAY_FILE_NAMES(6) = GAS_FORECAST_T6
               OVERLAY_FILE_NAMES(7) = GAS_FORECAST_T7
               OVERLAY_FILE_NAMES(8) = GAS_FORECAST_T8
               OVERLAY_FILE_NAMES(9) = GAS_FORECAST_T9
               CALL GAS_DEMAND_FORECAST_MAKEOVL(OVERLAY_FILE_NAMES,THIS_OVERLAY_FILE_EXISTS)
               EXIT
            ENDIF
         ENDDO
!
!
         IF(TRAN_EXP_FILE_EXISTS) CALL TRAN_EXP_MAKEOVL(TRAN_EXP_FILE)
!
! 071406.
!
         IF(GAS_STORAGE_FILE_EXISTS) CALL GAS_STORAGE_MAKEOVL(GAS_STORAGE_FILE)
         IF(GAS_NODE_FILE_EXISTS) CALL GAS_NODE_MAKEOVL(GAS_NODE_FILE)
         IF(GAS_WEATHER_DEMAND_EXISTS) CALL GAS_WEATHER_DEMAND_MAKEOVL(GAS_WEATHER_DEMAND_FILE)
         IF(GAS_TRANSPORT_FILE_EXISTS) CALL GAS_LINK_MAKEOVL(GAS_TRANSPORT_FILE)
         IF(GAS_DEMAND_FORECAST_FILE_EXISTS) CALL GAS_DEMAND_FORECAST_MAKEOVL(GAS_DEMAND_FORECAST_FILE)
!
! 070806.
!
         IF(GAS_SUPPLY_FORECAST_FILE_EXISTS) CALL GAS_SUPPLY_FORECAST_MAKEOVL(GAS_SUPPLY_FORECAST_FILE)
!

!
! NEW_MARKET_PRICE OVERLAY ! 6/29/99. GAT.
!
         IF(trim(NEW_MARKET_PRICE_FILE) /= 'NONE' .AND. OVERLAY_BOOK_IS_ACTIVE) CALL NEW_MARKET_PRICE_MAKEOVL(NEW_MARKET_PRICE_FILE)
!
! LH_GLOBAL FILE OVERLAY ! 01/27/04.
! CHANGED 083006.
!
         IF(LH_GLOBAL_EXISTS) THEN
            CALL LH_GLOBAL_MAKEOVL(LH_GLOBAL)
         ENDIF
!
! ELECT_HOUR_MULT_FILE OVERLAY ! 10/23/03
!
         IF(trim(ELECT_HOUR_MULT_FILE) /= 'NONE' .AND. OVERLAY_BOOK_IS_ACTIVE) CALL EH_MAKEOVL(ELECT_HOUR_MULT_FILE)
!
! GAS_HOUR_MULT_FILE OVERLAY ! 10/23/03
!
         IF(trim(GAS_HOUR_MULT_FILE) /= 'NONE' .AND. OVERLAY_BOOK_IS_ACTIVE) CALL EH_MAKEOVL(GAS_HOUR_MULT_FILE)
!
!
! ELECT_HOUR_MULT_FILE OVERLAY ! 10/23/03
!
         IF(trim(OIL_HOUR_MULT_FILE) /= 'NONE' .AND. OVERLAY_BOOK_IS_ACTIVE) CALL EH_MAKEOVL(OIL_HOUR_MULT_FILE)
!
! SCENARIO_HOURLY OVERLAY ! 11/7/99. GAT.
!
         IF(trim(SCENARIO_HOURLY_FILE) /= 'NONE' .AND. OVERLAY_BOOK_IS_ACTIVE) CALL SCENARIO_HOURLY_MAKEOVL(SCENARIO_HOURLY_FILE)
!
! WEEKLY_HYDRO OVERLAY ! 8/17/00. TS, GAT. ACTIVATED 4/5/01.
!
         IF(trim(WEEKLY_HYDRO_FILE) /= 'NONE' .AND. OVERLAY_BOOK_IS_ACTIVE) CALL WEEKLY_HYDRO_MAKEOVL(WEEKLY_HYDRO_FILE)
!
! RPS REQUIREMENTS OVERLAY ! 042209
!
         IF(trim(RPS_REQUIREMENTS_FILE) /= 'NONE' .AND. OVERLAY_BOOK_IS_ACTIVE) CALL RPS_REQUIREMENTS_MAKEOVL(RPS_REQUIREMENTS_FILE)
!
! RPS STATE DEMAND OVERLAY ! 042709
!
         IF(trim(RPS_STATE_DEMAND) /= 'NONE' .AND. OVERLAY_BOOK_IS_ACTIVE) CALL RPS_STATE_DEMAND_MAKEOVL(RPS_STATE_DEMAND)
!
! THERMAL RETROFIT OVERLAY ! 052609
!
         IF(trim(THERMAL_RETROFIT) /= 'NONE' .AND. OVERLAY_BOOK_IS_ACTIVE) CALL THERMAL_RETROFIT_MAKEOVL(THERMAL_RETROFIT)
!
! RPS_PROGRAM OVERLAY ! 110218
!
         IF(trim(RPS_PROGRAM_FILE) /= 'NONE' .AND. OVERLAY_BOOK_IS_ACTIVE) CALL RPS_PROGRAM_MAKEOVL(RPS_PROGRAM_FILE)
!
! CO2_PARAM OVERLAY ! 120809
!
         IF(trim(CO2_PARAM) /= 'NONE' .AND. OVERLAY_BOOK_IS_ACTIVE) CALL CO2_PARAM_MAKEOVL(CO2_PARAM)
!
! WEATHER_DEMAND OVERLAY !040601. TS
!
         IF(trim(WEATHER_DEMAND_FILE) /= 'NONE' .AND. OVERLAY_BOOK_IS_ACTIVE) CALL WEATHER_DEMAND_MAKEOVL(WEATHER_DEMAND_FILE)
!
! MARKET_GROUPS OVERLAY ! 6/8/01. GAT.
!
         IF(trim(MARKET_GROUPS_FILE) /= 'NONE' .AND. OVERLAY_BOOK_IS_ACTIVE) CALL MK_MAKEOVL(MARKET_GROUPS_FILE)
!
! SCENARIO MAKER OVERLAY ! 11/12/98. GAT.
!
         IF(SCENARIO_MAKER_EXISTS) CALL SCENARIO_MAKER_MAKEOVL(SCENARIO_MAKER_FILE,0)
         IF(SM_UNITS_S1_EXISTS) CALL SCENARIO_MAKER_MAKEOVL(SM_UNITS_S1,1)
         IF(SM_UNITS_S2_EXISTS) CALL SCENARIO_MAKER_MAKEOVL(SM_UNITS_S2,2)
         IF(SM_UNITS_S3_EXISTS) CALL SCENARIO_MAKER_MAKEOVL(SM_UNITS_S3,3)
         IF(SM_UNITS_S4_EXISTS) CALL SCENARIO_MAKER_MAKEOVL(SM_UNITS_S4,4)
         IF(SM_UNITS_S5_EXISTS) CALL SCENARIO_MAKER_MAKEOVL(SM_UNITS_S5,5)
         IF(SM_UNITS_S6_EXISTS) CALL SCENARIO_MAKER_MAKEOVL(SM_UNITS_S6,6)
         IF(SM_UNITS_S7_EXISTS) CALL SCENARIO_MAKER_MAKEOVL(SM_UNITS_S7,7)
         IF(SM_UNITS_S8_EXISTS) CALL SCENARIO_MAKER_MAKEOVL(SM_UNITS_S8,8)
         IF(SM_UNITS_S9_EXISTS) CALL SCENARIO_MAKER_MAKEOVL(SM_UNITS_S9,9)
!
! REG_OUT_PARAM OVERLAY ! 06/12/03. GAT.
!
         IF(REGIONAL_PARAMETER_EXISTS) CALL REG_OUT_PARAM_MAKEOVL(REGIONAL_PARAMETER_FILE,0)
         IF(RM_UNITS_R1_EXISTS) CALL REG_OUT_PARAM_MAKEOVL(RM_UNITS_R1,1)
         IF(RM_UNITS_R2_EXISTS) CALL REG_OUT_PARAM_MAKEOVL(RM_UNITS_R2,2)
         IF(RM_UNITS_R3_EXISTS) CALL REG_OUT_PARAM_MAKEOVL(RM_UNITS_R3,3)
         IF(REGIONAL_OUTAGES_EXISTS) CALL REG_OUT_PARAM_MAKEOVL(REGIONAL_OUTAGES_FILE,4)
!
! FUEL DERIVATIVES OVERLAY ! 12/23/02. GAT.
!
         IF(FUEL_DERIVATIVES_EXISTS) CALL FUEL_DERIVATIVES_MAKEOVL(FUEL_DERIVATIVES_FILE)
!
! ENERGY PRODUCTS OVERLAY ! 6/13/97. GAT.
!
         ACTIVE_FILES_EXIST(0) = ENERGY_PRODUCTS_EXISTS
         ACTIVE_FILES_EXIST(1:2) = FILE_EXISTS(385:386)
         ACTIVE_FILES_EXIST(3:6) = FILE_EXISTS(201:204)
         ACTIVE_FILES_EXIST(7) = FILE_EXISTS(212)
         ACTIVE_FILES_LIST(0) = ENERGY_PRODUCTS_FILE
         ACTIVE_FILES_LIST(1:2) = OVERLAY_FILE_NAME(385:386)
         ACTIVE_FILES_LIST(3:6) = OVERLAY_FILE_NAME(201:204)
         ACTIVE_FILES_LIST(7) = OVERLAY_FILE_NAME(212)
         CALL ENERGY_PRODUCTS_MAKEOVL(ACTIVE_FILES_LIST,   &
                                      ACTIVE_FILES_EXIST)
!
! MARKET PRICE OVERLAY ! 8/7/97. GAT.
!
         IF(trim(MARKET_PRICE_FILE) /= 'NONE' .AND. OVERLAY_BOOK_IS_ACTIVE) CALL MARKET_PRICE_MAKEOVL(MARKET_PRICE_FILE)
!
! REFERENCE LOAD OVERLAY ! 1/16/98. GAT.
!
         IF(trim(REFERENCE_LOAD_DATA_FILE) /= 'NONE' .AND. OVERLAY_BOOK_IS_ACTIVE)   &
                   CALL REFERENCE_LOAD_MAKEOVL(REFERENCE_LOAD_DATA_FILE)
!
! CAPACITY PLANNING RATIOS
!
         IF(CRA_EXISTS) CALL CR_MAKEOVL(CRA_FILE)
!
! CONVERT THE ENVIRONMENTAL OVERLAY FILE TO BINARY
!
         IF(ENV_EXISTS) CALL EN_MAKEOVL(ENV_FILE)
!
! CONVERT THE PURCHASES OVERLAY FILE TO BINARY
!
         IF(CNT_EXISTS) CALL CT_MAKEOVL(CNT_FILE)
         IF(ELIMINATIONS_EXISTS) CALL EM_MAKEOVL(ELIMINATIONS_FILE)
!
         IF(ASTRT_EXISTS) CALL AR_MAKEOVL(ASTRT_FILE)
         IF(AREA_EXISTS) CALL AA_MAKEOVL(AREA_FILE)
         IF(FUEL_INVENTORY_EXISTS) CALL FI_MAKEOVL(FUEL_INVENTORY_FILE)
!
! MULTI-FUEL FILE SECTION
!
         IF(FUEL_PRICE_EXISTS) CALL FC_MAKEOVL(FUEL_PRICE_FILE,0)
         IF(FUEL_PRICE_F1_EXISTS) CALL FC_MAKEOVL(FUEL_PRICE_F1_FILE,1)
         IF(FUEL_PRICE_F2_EXISTS) CALL FC_MAKEOVL(FUEL_PRICE_F2_FILE,2)
         IF(FUEL_PRICE_F3_EXISTS) CALL FC_MAKEOVL(FUEL_PRICE_F3_FILE,3)
         IF(FUEL_PRICE_F4_EXISTS) CALL FC_MAKEOVL(FUEL_PRICE_F4_FILE,4)
         IF(FUEL_PRICE_F5_EXISTS) CALL FC_MAKEOVL(FUEL_PRICE_F5_FILE,5)
         IF(FUEL_PRICE_F6_EXISTS) CALL FC_MAKEOVL(FUEL_PRICE_F6_FILE,6)
         IF(FUEL_PRICE_F7_EXISTS) CALL FC_MAKEOVL(FUEL_PRICE_F7_FILE,7)
         IF(FUEL_PRICE_F8_EXISTS) CALL FC_MAKEOVL(FUEL_PRICE_F8_FILE,8)
         IF(FUEL_PRICE_F9_EXISTS) CALL FC_MAKEOVL(FUEL_PRICE_F9_FILE,9)
         IF(FUEL_PRICE_F0_EXISTS) CALL FC_MAKEOVL(FUEL_PRICE_F0_FILE,10)
!
         IF(FUEL_EMISSIONS_FILE_EXISTS) CALL FE_MAKEOVL(FUEL_EMISSIONS_FILE)
         IF(SERVICE_TRANSACTIONS_EXISTS) CALL TR_MAKEOVL(SERVICE_TRANSACTIONS_FILE)
         IF(CLASS_RUN_SPECS_EXISTS) CALL CLASS_RUN_SWITCHES_MAKEOVL(CLASS_RUN_SPECS_FILE)
         IF(CLASS_REVENUE_FORECAST_EXISTS .AND. .NOT. WVPA()) CALL CLASS_SALES_MAKEOVL(CLASS_REVENUE_FORECAST_FILE)
         CALL CLOSE_ASSET_VECTOR_FILE
      ENDIF

      IF(.NOT. MATCH_FOUND) THEN
          IF(OVERLAY_BOOK_ACTIVE()) THEN
             WRITE(4,*) 'For endpoint ',END_POINT,' no overlay ','files in the overlay family ',trim(OVLNAME),' were found.'
          ELSE
             WRITE(4,*) 'For endpoint ',END_POINT,' no overlay ','files with the name ',trim(OVLNAME),' were found.'
          ENDIF
      ENDIF


! 05/15/03. CLEARS NAME LINE AT BOTTOM OF FILE
      CALL MG_CLEAR_LINE(17,9,36,0,0)
      CALL MG_CLEAR_LINE(16,9,36,0,0)
      RUN_FUTURE_ASSETS = RUN_FUTURE_ASSETS .OR. FAE_EXISTS .OR. DBE_EXISTS .OR. EAE_EXISTS .OR. DDE_EXISTS .OR.   &
                 EPE_EXISTS .OR. NFE_EXISTS .OR. ASTRT_EXISTS .OR. ASTVEC_EXISTS
      RETURN

!***********************************************************************
      ENTRY RETURN_CURRENT_OVERLAY_NAME(R_OVLNAME)
!***********************************************************************
!
         R_OVLNAME = SAVED_OVLNAME
      RETURN
!***********************************************************************
      ENTRY SET_UP_OVERLAY_FAMILIES(R_PROJECT_NAME)
!***********************************************************************
!
!

       filename=get_osf_filename(R_PROJECT_NAME)


         OPEN(10,FILE=filename,   &
                                                STATUS='OLD',IOSTAT=IOS)

         if(len_trim(r_project_name)==0) then
            call end_program("R_PROJECT_NAME is empty in " //" set_up_overlay_families entry.")
         endif

         OPEN(10,FILE=filename,STATUS='OLD',IOSTAT=IOS)

         FAMILY_DEFINITION_EXISTS = IOS == 0
         IF(FAMILY_DEFINITION_EXISTS) THEN
!
            READ(10,*) DELETE ! HEADER IGNORING NUMBER_OF_DEFINITIIONS
            NUMBER_OF_DEFINITIONS = 0
            READ(10,*,IOSTAT=IOS) DELETE ! FIRST DEFINITION
            DO WHILE(IOS == 0)
               NUMBER_OF_DEFINITIONS = NUMBER_OF_DEFINITIONS + 1
               READ(10,*,IOSTAT=IOS) DELETE ! SECOND THROUGH N DEFINITION
            ENDDO
            REWIND(10)
!
            READ(10,*) DELETE ! HEADER RECORD
!
            ALLOCATE(FAMILY_NAMES(NUMBER_OF_DEFINITIONS),OVERLAY_FILES_IN_FAMILY(NUMBER_OF_DATA_FILES,NUMBER_OF_DEFINITIONS),   &
                     OVERLAY_FILE_EXISTS(NUMBER_OF_DATA_FILES,NUMBER_OF_DEFINITIONS),OVERLAY_FAMILY_ACTIVE(NUMBER_OF_DEFINITIONS))
            DO I = 1, NUMBER_OF_DEFINITIONS
               FAMILY_NAMES(I) = 'NONE'
               OVERLAY_FAMILY_ACTIVE(I) = .FALSE.
               OVERLAY_FILES_IN_FAMILY(:,I) = 'NONE'
               OVERLAY_FILE_EXISTS(:,I) = .FALSE.
!
               READ(10,'(A)') RECLN
               RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'   &
                                    //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'   &
                                    //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'   &
                                    //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'   &
                                    //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'   &
                                    //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'   &
                                    //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'   &
                                    //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'   &
                                    //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'   &
                                    //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'   &
                                    //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'   &
                                    //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'   &
                                    //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'   &
                                    //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               READ(RECLN,*) DELETE,FAMILY_NAMES(I),(OVERLAY_FILES_IN_FAMILY(J,I),J=1,NUMBER_OF_DATA_FILES)
               DO J = 1, NUMBER_OF_DATA_FILES
                  IF(FILE_TYPES(J) == 'XXX') CYCLE
                  OVL_FILE_NAME = OVERLAY_FILES_IN_FAMILY(J,I)
!
                  IF(INDEX(OVL_FILE_NAME,'NONE') == 0 .AND. INDEX('NONE',trim(OVL_FILE_NAME)) == 0) THEN
                     INQUIRE(FILE=FILE_TYPES(J)//trim(OVL_FILE_NAME)//'.DAT',EXIST=THIS_FILE_EXISTS)
                     IF((J >= 170 .AND. J <= 174) .AND. .NOT. THIS_FILE_EXISTS) THEN
                        INQUIRE(FILE='EXO'//trim(OVL_FILE_NAME)//'.DAT',EXIST=THIS_FILE_EXISTS)
                     ENDIF
                     OVERLAY_FILE_EXISTS(J,I) = THIS_FILE_EXISTS
                     OVERLAY_FAMILY_ACTIVE(I) = OVERLAY_FAMILY_ACTIVE(I) .OR. OVERLAY_FILE_EXISTS(J,I)
                  ENDIF
               ENDDO
            ENDDO
            CLOSE(10)
         ENDIF
      RETURN

!***********************************************************************
      ENTRY RETURN_OVERLAY_FAMILY_LIST(R_OVLNAME,R_MATCH_FOUND,R_OVERLAY_FILE_NAMES,R_FILE_EXISTS)
!***********************************************************************
!
         R_MATCH_FOUND = .FALSE.
         IF(FAMILY_DEFINITION_EXISTS) THEN
            FAMILY_POSITION = 0
            DO I = 1, NUMBER_OF_DEFINITIONS
               IF(INDEX(FAMILY_NAMES(I),R_OVLNAME) /=0 .AND. INDEX(R_OVLNAME,FAMILY_NAMES(I)) /=0) THEN
                  FAMILY_POSITION = I
                  EXIT
               ENDIF
            ENDDO
            IF(FAMILY_POSITION /= 0) THEN
               R_MATCH_FOUND = OVERLAY_FAMILY_ACTIVE(FAMILY_POSITION)
               IF(R_MATCH_FOUND) THEN
                     R_OVERLAY_FILE_NAMES(:) = OVERLAY_FILES_IN_FAMILY(:,FAMILY_POSITION)
                     R_FILE_EXISTS(1:NUMBER_OF_DATA_FILES) = OVERLAY_FILE_EXISTS(:,FAMILY_POSITION)
               ENDIF
            ENDIF
         ENDIF
      RETURN
      END
!
!
