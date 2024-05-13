module overlay
    use bfil_obj
    use base_file_names_from_bfil_obj
    implicit none
    INTEGER (KIND=2), PARAMETER ::  NUMBER_OF_DATA_FILES=388
    CHARACTER*5 :: OVERLAY_FILE_NAME_ARRAY(NUMBER_OF_DATA_FILES)
    CHARACTER*256 :: FXE_FILE="NONE", &  ! 213 CAPEX EXPANSION FUTURE ASSETS
             CL_UNITS_CS="NONE", &
             CL_UNITS_CU="NONE", &
            CL_UNITS_CV="NONE", &
            CL_UNITS_CW="NONE", &
            CL_UNITS_CX="NONE", &
            CL_UNITS_CY="NONE", &
            CO2_PARAM="NONE",  &      ! 362
            EPE_FILE="NONE", &
            EXPENSE_FILE_X1="NONE", &
            EXPENSE_FILE_X2="NONE", &
            EXPENSE_FILE_X3="NONE", &
            EXPENSE_FILE_X4="NONE", &
            EXPENSE_FILE_X5="NONE" ! 174



                          
    CHARACTER*5 :: character*5 :: EXPENSE_NAMES_RESERVED='NONE' & ! 7
			EAE_FILE="NONE",DDE_FILE="NONE", &
			DBE_FILE="NONE",FAE_FILE="NONE", &
		  NFE_FILE="NONE", & 
		  PRF_FILE="NONE", & 
		  INE_FILE="NONE",TAX_FILE="NONE", & 
		  INT_FILE="NONE",FIN_FILE="NONE", & 
		  OLD_FIN_FILE="NONE", & 
		  ESC_FILE="NONE",FOS_FILE="NONE", & 
		  HYD_FILE="NONE",UPE_FILE="NONE", & 
		  SFC_FILE="NONE",CFC_FILE="NONE", & 
		  RFC_FILE="NONE",IFC_FILE="NONE", & 
		  O1F_FILE="NONE",O2F_FILE="NONE", & 
		  O3F_FILE="NONE",CLS_FILE="NONE", & 
		  CLH_FILE="NONE",LMG_FILE="NONE", & 
		  LMS_FILE="NONE",PFD_FILE="NONE", & 
		  PNP_FILE="NONE",FNP_FILE="NONE", & 
		  CNP_FILE="NONE",CRA_FILE="NONE", & 
		  COS_FILE="NONE",LFI_FILE="NONE", & 
		  ENV_FILE="NONE",ECO_FILE="NONE", & 
		  ASTRT_FILE="NONE",ASTVEC_FILE="NONE", & 
		  CNT_FILE="NONE",PRODP_FILE="NONE", & 
		  AREA_FILE="NONE",FUEL_INVENT_FILE="NONE", & 
		  FUEL_PRICES_FILE="NONE", & 
		  SERVICE_TRANSACTIONS_FILE="NONE", & 
		  FUEL_EMISSIONS_DATA_FILE="NONE", & 
		  BASE_YEAR_LOAD="NONE", & 
		  OVER_UNDER_DECISIONS_TABLES="NONE", & 
		  ASSET_CLASS_RUN_SWITCHES="NONE", & 
		  ASSET_CLASS_SALES_FORECAST="NONE", & 
		  ASSET_CLASS_ELIMINATIONS="NONE", & 
		  DETAILED_REPORTS="NONE", & 
		  CATAWBA2_CONTRACT="NONE", & 
		  UNIT_OUTAGE="NONE", & 
		  TRANSACT_TIE="NONE", & 
		  MONTHLY_PAYABLES="NONE", & 
		  DAY_TYPE="NONE", & 
		  SCENARIO_MAKER="NONE", & 
		  ENERGY_PRODUCTS="NONE", & 
		  MARKET_PRICE="NONE", & 
		  CPL_CONTRACT="NONE", & 
		  CPL_DEFERRED_FUEL="NONE", & 
		  RDI_POWERDAT="NONE", & 
		  RDI_BASECASE="NONE", & 
		  TRANS_FORECAST="NONE", &
		  TAX_INFO="NONE", & ! USING FOR CP&L TAX 9/13/98
		  ADDENDUM_INFO="NONE", & 
		  TRANS_GROUPS_INFO="NONE", & 
		  TRANS_PATHS_INFO="NONE", & 
		  TRANS_CONSTRAINT_INFO="NONE", & 
		  SCENARIO_PARAMETER="NONE", & 
		  NEW_MARKET_PRICE="NONE", & 
		  WEEKLY_HYDRO="NONE", & 
		  WEATHER_DEMAND="NONE", & 
		  SCENARIO_HOURLY="NONE", & 
		  FORWARD_OPTION="NONE", &
		  PLACE_HOLDER="NONE", & ! 75-83
		  MARKET_GROUPS="NONE", & ! 84
		  FUEL_PRICE_F1="NONE", & 
		  FUEL_PRICE_F2="NONE", & 
		  FUEL_PRICE_F3="NONE", & 
		  FUEL_PRICE_F4="NONE", & 
		  FUEL_PRICE_F5="NONE", & 
		  FUEL_PRICE_F6="NONE", & 
		  FUEL_PRICE_F7="NONE", & 
		  FUEL_PRICE_F8="NONE", &
		  FUEL_PRICE_F9="NONE", & ! 93
		  FUEL_PRICE_F0="NONE", & ! 335
		  RDI_PLACE_HOLDER="NONE", & 
		  WVPA_ACTUAL_PURCHASES="NONE", & 
		  CL_UNITS_C1="NONE", & 
		  CL_UNITS_C2="NONE", & 
		  CL_UNITS_C3="NONE", & 
		  CL_UNITS_C4="NONE", & 
		  CL_UNITS_C5="NONE", & 
		  CL_UNITS_C6="NONE", & !99
		  CL_UNITS_C7="NONE", & !100
		  CL_UNITS_C8="NONE", & ! 130
		  CL_UNITS_C9="NONE", & 
		  CL_UNITS_C0="NONE", & 
		  CL_UNITS_CA="NONE", & 
		  CL_UNITS_CB="NONE", & 
		  CL_UNITS_CC="NONE", & 
		  CL_UNITS_CD="NONE", & ! 136
		  EL_UNITS_E1="NONE", & ! 137
		  EL_UNITS_E2="NONE", &
		  EL_UNITS_E3="NONE", &
		  EL_UNITS_E4="NONE",  & ! 140
		  EL_UNITS_E5="NONE", & 
		  EL_UNITS_E6="NONE", & 
		  EL_UNITS_E7="NONE", & 
		  EL_UNITS_E8="NONE", & 
		  EL_UNITS_E9="NONE", & ! 145
		  EL_UNITS_E0="NONE", & 
		  EL_UNITS_EB="NONE", & 
		  EL_UNITS_ED="NONE", & 
		  EL_UNITS_EE="NONE", & 
		  EL_UNITS_EF="NONE", & ! 150
		  TF_UNITS_T1="NONE", & ! 151
		  TF_UNITS_T2="NONE", & 
		  TF_UNITS_T3="NONE", & 
		  TF_UNITS_T4="NONE", & 
		  TF_UNITS_T5="NONE", & ! 155
		  TF_UNITS_T6="NONE", & 
		  TF_UNITS_T7="NONE", & 
		  TF_UNITS_T8="NONE", & 
		  TF_UNITS_T9="NONE", & 
		  TF_UNITS_T0="NONE", & ! 160
		  TF_UNITS_TB="NONE", & 
		  TF_UNITS_TD="NONE", & 
		  TF_UNITS_TH="NONE", & 
		  TF_UNITS_TI="NONE",  & ! 164
		  TF_UNITS_TA="NONE",  & ! 377
		  TF_UNITS_TC="NONE",  & ! 378
		  CL_UNITS_CE="NONE",  & ! 165
		  CL_UNITS_CG="NONE", &  ! 166
		  EL_UNITS_EG="NONE", &  ! 167
		  EL_UNITS_EH="NONE", &  ! 168
		  EL_UNITS_EI="NONE", &  ! 211
		  EL_UNITS_EO="NONE", &  ! 381
		  EL_UNITS_EP="NONE",  & ! 382
		  REGIONAL_OUTAGES="NONE", & ! 169
		  SCENARIO_VECTOR="NONE",  & ! 175
		  SM_UNITS_S1="NONE", & 
		  SM_UNITS_S2="NONE", & 
		  SM_UNITS_S3="NONE", & 
		  SM_UNITS_S4="NONE", & 
		  SM_UNITS_S5="NONE", & 
		  SM_UNITS_S6="NONE", & 
		  SM_UNITS_S7="NONE", & 
		  SM_UNITS_S8="NONE", & 
		  SM_UNITS_S9="NONE", & 
		  USER_DAY="NONE", & ! 185
		  ICAP="NONE", &
		  REGIONAL_PARAMETER="NONE", & !187
		  FUEL_DERIVATIVES="NONE", &
		  FINANCIAL_HISTORY="NONE", &! 189
		  MONTHLY_CAPITIAL_RATES="NONE", & ! 190
		  WVPA_RATES_STRUCTURES="NONE", &
		  WVPA_TRACKER_FILE="NONE",  & ! 192
		  WVPA_COOP_FORECAST_FILE="NONE", &  ! 193
		  RM_UNITS_R1="NONE", & ! 194
		  RM_UNITS_R2="NONE", & !195
		  RM_UNITS_R3="NONE",  &  ! 196
		  LH_GLOBAL="NONE", & 
		  ELECT_HOUR_MULT="NONE", &
		  GAS_HOUR_MULT="NONE", &
		  OIL_HOUR_MULT="NONE", & ! 200
		  ENERGY_PRODUCTS_P2="NONE", & ! 201
		  ENERGY_PRODUCTS_P3="NONE", &
		  ENERGY_PRODUCTS_P4="NONE", &
		  ENERGY_PRODUCTS_P5="NONE",  &  ! 204
		  ENERGY_PRODUCTS_P6="NONE",  & ! 212
		  ENERGY_PRODUCTS_P0="NONE",  &  ! 385
		  ENERGY_PRODUCTS_P1="NONE",  &  ! 386
		  TRANS_EXP="NONE",    &      ! 258
		  GAS_NODE="NONE",      &     ! 290
		  GAS_STORAGE="NONE",     &     ! 334
		  GAS_TRANSPORT="NONE",     &   ! 291
		  GAS_DEMAND_FORECAST="NONE",  & ! 292
		  GAS_SUPPLY_FORECAST="NONE",  & ! 293
		  GF_UNITS_T1="NONE",   &! 294
		  GF_UNITS_T2="NONE", & 
		  GF_UNITS_T3="NONE", & 
		  GF_UNITS_T4="NONE", & 
		  GF_UNITS_T5="NONE",  & 
		  GF_UNITS_T6="NONE", & 
		  GF_UNITS_T7="NONE", & 
		  GF_UNITS_T8="NONE", & 
		  GF_UNITS_T9="NONE", & ! 302
		  GAS_WEATHER_DEMAND="NONE", &  ! 303
		  CF_UNITS_T1="NONE",           & ! 358
		  RPS_STATE_DEMAND="NONE",   &    ! 359
		  THERMAL_RETROFIT="NONE",     &  ! 360
		  CL_UNITS_CI="NONE", & 
		  CL_UNITS_CJ="NONE", & 
		  CL_UNITS_CK="NONE", & 
		  CL_UNITS_CM="NONE", & 
		  CL_UNITS_CP="NONE", & 
		  CL_UNITS_CQ="NONE", &  ! 210
		  CL_UNITS_OPTIONS_CX="NONE", &  ! 263
		  USER_DAY_D1="NONE",     &   ! 363
		  USER_DAY_D2="NONE",      &  ! 364
		  USER_DAY_D3="NONE",    &    ! 365
		  USER_DAY_D4="NONE",     &   ! 366
		  USER_DAY_D5="NONE",    &    ! 367
		  USER_DAY_D6="NONE",     &   ! 368
		  USER_DAY_D7="NONE",   &     ! 369
		  USER_DAY_D8="NONE",    &    ! 370
		  USER_DAY_D9="NONE",     &    ! 371
		  CL_UNITS_EJ="NONE",    &    ! 379
		  CL_UNITS_EK="NONE",     &   ! 380
		  CL_UNITS_ER="NONE",      &  ! 383
		  CL_UNITS_ET="NONE", &       ! 384
		  CL_UNITS_MX="NONE",  &      ! 387
		  CL_UNITS_MY="NONE",  &      ! 388
		  RPS_PROGRAM="NONE"        ! 376

      CHARACTER*5 :: EAE_FILE,DDE_FILE,DBE_FILE,FAE_FILE,NFE_FILE, & 
                  FX_FUTURE_ASSETS_CAPEX_FILE,INE_FILE,TAX_FILE, & 
                  PRF_FILE, & 
                  INT_FILE,FIN_FILE,ESC_FILE,FOS_FILE,HYD_FILE, & 
                  UPE_FILE,SFC_FILE,CFC_FILE,RFC_FILE,IFC_FILE,O1F_FILE, & 
                  O2F_FILE,O3F_FILE,CLS_FILE,CLH_FILE,LMG_FILE,LMS_FILE, & 
                  PFD_FILE,PNP_FILE,FNP_FILE,CNP_FILE,CRA_FILE,COS_FILE, & 
                  LFI_FILE,ENV_FILE,ECO_FILE,ASTRT_FILE,ASTVEC_FILE, & 
                  CNT_FILE,PRODP_FILE,AREA_FILE,FUEL_INVENTORY_FILE, & 
                  FUEL_PRICE_FILE,FUEL_EMISSIONS_FILE, & 
                  SERVICE_TRANSACTIONS_FILE,CLASS_RUN_SPECS_FILE, & 
                  CLASS_REVENUE_FORECAST_FILE,ELIMINATIONS_FILE, & 
                  DETAILED_REPORTS_FILE,CATAWBA2_CONTRACT_FILE, & 
                  NEW_RUN_SPECS_FILE,REFERENCE_LOAD_DATA_FILE, & 
                  OVER_UNDER_TABLE_FILE, & 
                  UNIT_OUTAGE_FILE, & 
                  TRANSACT_TIE_FILE, & 
                  MONTHLY_PAYABLES_FILE, & 
                  DAY_TYPE_FILE, & 
                  SCENARIO_MAKER_FILE, & 
                  ENERGY_PRODUCTS_FILE, & 
                  MARKET_PRICE_FILE, & 
                  CPL_CONTRACT_FILE, & 
                  RDI_POWERDAT_FILE, & 
                  RDI_BASECASE_FILE, & 
                  TRANS_FORECAST_FILE, & 
                  TAX_INFO_FILE, & 
                  ADDENDUM_FILE, & 
                  TRANS_GROUPS_FILE, & 
                  TRANS_PATHS_FILE, & 
                  TRANS_CONSTRAINT_FILE, & 
                  SCENARIO_PARAMETER, & 
                  NEW_MARKET_PRICE_FILE, & 
                  SCENARIO_HOURLY_FILE, & 
                  WEEKLY_HYDRO_FILE, & 
                  WEATHER_DEMAND_FILE, & 
                  FORWARD_OPTION_FILE, & 
                  MARKET_GROUPS_FILE, & 
                  FUEL_PRICE_F1_FILE, & 
                  FUEL_PRICE_F2_FILE, & 
                  FUEL_PRICE_F3_FILE, & 
                  FUEL_PRICE_F4_FILE, & 
                  FUEL_PRICE_F5_FILE, & 
                  FUEL_PRICE_F6_FILE, & 
                  FUEL_PRICE_F7_FILE, & 
                  FUEL_PRICE_F8_FILE, & 
                  FUEL_PRICE_F9_FILE, & 
                  CL_UNITS_C1_FILE, & 
                  CL_UNITS_C2_FILE, & 
                  CL_UNITS_C3_FILE, & 
                  CL_UNITS_C4_FILE, & 
                  CL_UNITS_C5_FILE, & 
                  CL_UNITS_C6_FILE, & 
                  CL_UNITS_C7_FILE, & 
                  CL_UNITS_C8_FILE, & 
                  CL_UNITS_C9_FILE, & 
                  CL_UNITS_C0_FILE, & 
                  CL_UNITS_CA_FILE, & 
                  CL_UNITS_CB_FILE, & 
                  CL_UNITS_CC_FILE, & 
                  CL_UNITS_CD_FILE, & 
                  CL_UNITS_CE_FILE, & 
                  CL_UNITS_CG_FILE, & 
                  CL_UNITS_CI_FILE, & 
                  CL_UNITS_CJ_FILE, & 
                  CL_UNITS_CK_FILE, & 
                  CL_UNITS_CM_FILE, & 
                  CL_UNITS_CP_FILE, & 
                  CL_UNITS_CQ_FILE, & 
                  EL_UNITS_E1_FILE, & 
                  EL_UNITS_E2_FILE, & 
                  EL_UNITS_E3_FILE, & 
                  EL_UNITS_E4_FILE, & 
                  EL_UNITS_E5_FILE, & 
                  EL_UNITS_E6_FILE, & 
                  EL_UNITS_E7_FILE, & 
                  EL_UNITS_E8_FILE, & 
                  EL_UNITS_E9_FILE, & 
                  EL_UNITS_E0_FILE, & 
                  EL_UNITS_EB_FILE, & 
                  EL_UNITS_ED_FILE, & 
                  EL_UNITS_EE_FILE, & 
                  EL_UNITS_EF_FILE, & 
                  EL_UNITS_EG_FILE, & 
                  EL_UNITS_EH_FILE, & 
                  EL_UNITS_EI_FILE, & 
                  TRANS_FORECAST_T1_FILE, & 
                  TRANS_FORECAST_T2_FILE, & 
                  TRANS_FORECAST_T3_FILE, & 
                  TRANS_FORECAST_T4_FILE, & 
                  TRANS_FORECAST_T5_FILE, & 
                  TRANS_FORECAST_T6_FILE, & 
                  TRANS_FORECAST_T7_FILE, & 
                  TRANS_FORECAST_T8_FILE, & 
                  TRANS_FORECAST_T9_FILE, & 
                  TRANS_FORECAST_T0_FILE, & 
                  TRANS_FORECAST_TB_FILE, & 
                  TRANS_FORECAST_TD_FILE, & 
                  TRANS_FORECAST_TH_FILE, & 
                  TRANS_FORECAST_TI_FILE, & 
                  REGIONAL_OUTAGES_FILE, & 
                  EXPENSE_FILE_X1, & 
                  EXPENSE_FILE_X2, & 
                  EXPENSE_FILE_X3, & 
                  EXPENSE_FILE_X4, & 
                  EXPENSE_FILE_X5, & 
                  SCENARIO_VECTOR_FILE, & 
                  SM_UNITS_S1, & 
                  SM_UNITS_S2, & 
                  SM_UNITS_S3, & 
                  SM_UNITS_S4, & 
                  SM_UNITS_S5, & 
                  SM_UNITS_S6, & 
                  SM_UNITS_S7, & 
                  SM_UNITS_S8, & 
                  SM_UNITS_S9, & 
                  USER_DAY_FILE, & 
                  ICAP_FILE, & 
                  REGIONAL_PARAMETER_FILE, !187 & 
                  FUEL_DERIVATIVES_FILE, & 
                  FINANCIAL_HISTORY_FILE, ! 189 & 
                  MONTHLY_CAPITIAL_RATES_FILE, ! 190 & 
                  WVPA_RATES_STRUCTURES, & 
                  WVPA_TRACKER_FILE,  ! 192 & 
                  WVPA_COOP_REVENUE_FILE, ! 193 & 
                  RM_UNITS_R1, & 
                  RM_UNITS_R2, & 
                  RM_UNITS_R3,         ! 196 & 
                  LH_GLOBAL, & 
                  ELECT_HOUR_MULT_FILE, & 
                  GAS_HOUR_MULT_FILE, & 
                  OIL_HOUR_MULT_FILE, ! 200 & 
                  ProSym_Data_FILE, ! 101 & 
                  ENERGY_PRODUCTS_FILE_P2 
   character*5 :: ENERGY_PRODUCTS_FILE_P3, & 
                  ENERGY_PRODUCTS_FILE_P4, & 
                  ENERGY_PRODUCTS_FILE_P5, & 
                  ENERGY_PRODUCTS_FILE_P6, & 
                  GAS_NODE_FILE, & 
                  GAS_TRANSPORT_FILE, & 
                  TRAN_EXP_FILE, & 
                  GAS_DEMAND_FORECAST_FILE, & 
                  GAS_SUPPLY_FORECAST_FILE, & 
                  GAS_FORECAST_T1, & 
                  GAS_FORECAST_T2, & 
                  GAS_FORECAST_T3, & 
                  GAS_FORECAST_T4, & 
                  GAS_FORECAST_T5, & 
                  GAS_FORECAST_T6, & 
                  GAS_FORECAST_T7, & 
                  GAS_FORECAST_T8, & 
                  GAS_FORECAST_T9, & 
                  GAS_WEATHER_DEMAND_FILE, & 
                  GAS_STORAGE_FILE, & 
                  FUEL_PRICE_F0_FILE, & 
                  GQB_GAS_THERMAL_FILE, & 
                  CORES_AND_MACHINES_FILE, & 
                  RPS_REQUIREMENTS_FILE, & 
                  GQB_GAS_THERMAL, & ! 353 & 
                  COAL_NODE_FILE,  &  ! 354 & 
                  COAL_TRANSPORT_FILE,  &       ! 355 & 
                  COAL_SUPPLY_FORECAST_FILE,  & ! 356 & 
                  COAL_DEMAND_FORECAST_FILE,  & 
                  COAL_FORECAST_T1,          &   ! 358 & 
                  RPS_STATE_DEMAND,  & ! 359 & 
                  THERMAL_RETROFIT, &  ! 360 & 
                  COAL_CONTRACTS,   &  ! 361 & 
                  CO2_PARAM,        &   ! 362 & 
                  USER_DAY_D1,    &    ! 363 & 
                  USER_DAY_D2,    &    ! 364 & 
                  USER_DAY_D3,    &    ! 365 & 
                  USER_DAY_D4,   &     ! 366 & 
                  USER_DAY_D5,    &    ! 367 & 
                  USER_DAY_D6,    &    ! 368 & 
                  USER_DAY_D7,     &   ! 369 & 
                  USER_DAY_D8,      &  ! 370 & 
                  USER_DAY_D9, &        ! 371 & 
                  COAL_MODEL_POINTER_FILE,  &      ! 372 & 
                  COAL_MODEL_SO2_INFO_FILE,   &  ! 373 & 
                  COAL_MODEL_GENERIC_TRANSPORT_FILE,  &  ! 374 & 
                  COAL_MODEL_GENERIC_DEMAND_FILE,  & ! 375 & 
                  RPS_PROGRAM_FILE  ! 376
     
      EQUIVALENCE (OVERLAY_FILE_NAME_ARRAY(1),EAE_FILE) 
	  equivalence (OVERLAY_FILE_NAME_ARRAY(2),DDE_FILE)
	  equivalence (OVERLAY_FILE_NAME_ARRAY(3),DBE_FILE)
	  equivalence (OVERLAY_FILE_NAME_ARRAY(4),FAE_FILE)
	  equivalence (OVERLAY_FILE_NAME_ARRAY(5),NFE_FILE)
                  
    equivalence   (OVERLAY_FILE_NAME_ARRAY(6),EPE_FILE)
    
    equivalence   (OVERLAY_FILE_NAME_ARRAY(7),EXPENSE_NAMES_RESERVED)
	
     equivalence (OVERLAY_FILE_NAME_ARRAY(8),PRF_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(9),INE_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(10),TAX_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(11),INT_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(12),FIN_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(13),ESC_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(14),FOS_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(15),HYD_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(16),UPE_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(17),SFC_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(18),CFC_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(19),RFC_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(20),IFC_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(21),O1F_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(22),O2F_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(23),O3F_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(24),CLS_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(25),CLH_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(26),LMG_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(27),LMS_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(28),PFD_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(29),PNP_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(30),FNP_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(31),CNP_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(32),CRA_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(33),COS_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(34),REFERENCE_LOAD_DATA_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(35),LFI_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(36),ENV_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(37),ECO_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(38),ASTRT_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(39),ASTVEC_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(40),CNT_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(41),PRODP_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(42),AREA_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(43),FUEL_INVENTORY_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(44),FUEL_PRICE_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(45),SERVICE_TRANSACTIONS_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(46),FUEL_EMISSIONS_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(47),OVER_UNDER_TABLE_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(48),CLASS_RUN_SPECS_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(49),CLASS_REVENUE_FORECAST_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(50),ELIMINATIONS_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(51),DETAILED_REPORTS_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(52),NEW_RUN_SPECS_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(53),CATAWBA2_CONTRACT_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(54),UNIT_OUTAGE_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(55),TRANSACT_TIE_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(56),MONTHLY_PAYABLES_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(57),DAY_TYPE_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(58),SCENARIO_MAKER_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(59),ENERGY_PRODUCTS_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(60),MARKET_PRICE_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(61),CPL_CONTRACT_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(62),RDI_POWERDAT_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(63),RDI_BASECASE_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(64),TRANS_FORECAST_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(65),TAX_INFO_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(66),ADDENDUM_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(67),TRANS_GROUPS_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(68),TRANS_PATHS_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(69),TRANS_CONSTRAINT_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(70),SCENARIO_PARAMETER)
     equivalence (OVERLAY_FILE_NAME_ARRAY(71),NEW_MARKET_PRICE_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(72),WEEKLY_HYDRO_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(73),SCENARIO_HOURLY_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(74),FORWARD_OPTION_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(78),WEATHER_DEMAND_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(84),MARKET_GROUPS_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(85),FUEL_PRICE_F1_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(86),FUEL_PRICE_F2_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(87),FUEL_PRICE_F3_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(88),FUEL_PRICE_F4_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(89),FUEL_PRICE_F5_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(90),FUEL_PRICE_F6_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(91),FUEL_PRICE_F7_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(92),FUEL_PRICE_F8_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(93),FUEL_PRICE_F9_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(94),CL_UNITS_C1_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(95),CL_UNITS_C2_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(96),CL_UNITS_C3_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(97),CL_UNITS_C4_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(98),CL_UNITS_C5_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(99),CL_UNITS_C6_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(100),CL_UNITS_C7_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(101),ProSym_Data_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(130),CL_UNITS_C8_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(131),CL_UNITS_C9_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(132),CL_UNITS_C0_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(133),CL_UNITS_CA_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(134),CL_UNITS_CB_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(135),CL_UNITS_CC_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(136),CL_UNITS_CD_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(137),EL_UNITS_E1_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(138),EL_UNITS_E2_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(139),EL_UNITS_E3_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(140),EL_UNITS_E4_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(141),EL_UNITS_E5_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(142),EL_UNITS_E6_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(142),EL_UNITS_E7_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(144),EL_UNITS_E8_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(145),EL_UNITS_E9_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(146),EL_UNITS_E0_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(147),EL_UNITS_EB_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(148),EL_UNITS_ED_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(149),EL_UNITS_EE_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(150),EL_UNITS_EF_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(151),TRANS_FORECAST_T1_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(152),TRANS_FORECAST_T2_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(153),TRANS_FORECAST_T3_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(154),TRANS_FORECAST_T4_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(155),TRANS_FORECAST_T5_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(156),TRANS_FORECAST_T6_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(157),TRANS_FORECAST_T7_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(158),TRANS_FORECAST_T8_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(159),TRANS_FORECAST_T9_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(160),TRANS_FORECAST_T0_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(161),TRANS_FORECAST_TB_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(162),TRANS_FORECAST_TD_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(163),TRANS_FORECAST_TH_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(164),TRANS_FORECAST_TI_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(165),CL_UNITS_CE_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(166),CL_UNITS_CG_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(167),EL_UNITS_EG_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(168),EL_UNITS_EH_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(169),REGIONAL_OUTAGES_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(170),EXPENSE_FILE_X1)
     equivalence (OVERLAY_FILE_NAME_ARRAY(171),EXPENSE_FILE_X2)
     equivalence (OVERLAY_FILE_NAME_ARRAY(172),EXPENSE_FILE_X3)
     equivalence (OVERLAY_FILE_NAME_ARRAY(173),EXPENSE_FILE_X4)
     equivalence (OVERLAY_FILE_NAME_ARRAY(174),EXPENSE_FILE_X5)
     equivalence (OVERLAY_FILE_NAME_ARRAY(175),SCENARIO_VECTOR_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(176),SM_UNITS_S1)
     equivalence (OVERLAY_FILE_NAME_ARRAY(177),SM_UNITS_S2)
     equivalence (OVERLAY_FILE_NAME_ARRAY(178),SM_UNITS_S3)
     equivalence (OVERLAY_FILE_NAME_ARRAY(179),SM_UNITS_S4)
     equivalence (OVERLAY_FILE_NAME_ARRAY(180),SM_UNITS_S5)
     equivalence (OVERLAY_FILE_NAME_ARRAY(181),SM_UNITS_S6)
     equivalence (OVERLAY_FILE_NAME_ARRAY(182),SM_UNITS_S7)
     equivalence (OVERLAY_FILE_NAME_ARRAY(183),SM_UNITS_S8)
     equivalence (OVERLAY_FILE_NAME_ARRAY(184),SM_UNITS_S9)
     equivalence (OVERLAY_FILE_NAME_ARRAY(185),USER_DAY_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(186),ICAP_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(187),REGIONAL_PARAMETER_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(188),FUEL_DERIVATIVES_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(189),FINANCIAL_HISTORY_FILE) ! 189  
	 
     equivalence (OVERLAY_FILE_NAME_ARRAY(190),MONTHLY_CAPITIAL_RATES_FILE) ! 190  
     equivalence (OVERLAY_FILE_NAME_ARRAY(191),WVPA_RATES_STRUCTURES)
     equivalence (OVERLAY_FILE_NAME_ARRAY(192),WVPA_TRACKER_FILE) ! 192  
     equivalence (OVERLAY_FILE_NAME_ARRAY(193),WVPA_COOP_REVENUE_FILE) ! 193  
     equivalence (OVERLAY_FILE_NAME_ARRAY(194),RM_UNITS_R1)
     equivalence (OVERLAY_FILE_NAME_ARRAY(195),RM_UNITS_R2)
     equivalence (OVERLAY_FILE_NAME_ARRAY(196),RM_UNITS_R3) ! 196 
     equivalence (OVERLAY_FILE_NAME_ARRAY(197),LH_GLOBAL)
     equivalence (OVERLAY_FILE_NAME_ARRAY(198),ELECT_HOUR_MULT_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(199),GAS_HOUR_MULT_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(200),OIL_HOUR_MULT_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(201),ENERGY_PRODUCTS_FILE_P2)
     equivalence (OVERLAY_FILE_NAME_ARRAY(202),ENERGY_PRODUCTS_FILE_P3)
     equivalence (OVERLAY_FILE_NAME_ARRAY(203),ENERGY_PRODUCTS_FILE_P4)
     equivalence (OVERLAY_FILE_NAME_ARRAY(204),ENERGY_PRODUCTS_FILE_P5)
     equivalence (OVERLAY_FILE_NAME_ARRAY(205),CL_UNITS_CI_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(206),CL_UNITS_CJ_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(207),CL_UNITS_CK_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(208),CL_UNITS_CM_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(209),CL_UNITS_CP_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(210),CL_UNITS_CQ_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(211),EL_UNITS_EI_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(212),ENERGY_PRODUCTS_FILE_P6)
     equivalence (OVERLAY_FILE_NAME_ARRAY(213),FX_FUTURE_ASSETS_CAPEX_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(258),TRAN_EXP_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(290),GAS_NODE_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(291),GAS_TRANSPORT_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(292),GAS_DEMAND_FORECAST_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(293),GAS_SUPPLY_FORECAST_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(294),GAS_FORECAST_T1)
     equivalence (OVERLAY_FILE_NAME_ARRAY(295),GAS_FORECAST_T2)
     equivalence (OVERLAY_FILE_NAME_ARRAY(296),GAS_FORECAST_T3)
     equivalence (OVERLAY_FILE_NAME_ARRAY(297),GAS_FORECAST_T4)
     equivalence (OVERLAY_FILE_NAME_ARRAY(298),GAS_FORECAST_T5)
     equivalence (OVERLAY_FILE_NAME_ARRAY(299),GAS_FORECAST_T6)
     equivalence (OVERLAY_FILE_NAME_ARRAY(300),GAS_FORECAST_T7)
     equivalence (OVERLAY_FILE_NAME_ARRAY(301),GAS_FORECAST_T8)
     equivalence (OVERLAY_FILE_NAME_ARRAY(302),GAS_FORECAST_T9)
     equivalence (OVERLAY_FILE_NAME_ARRAY(303),GAS_WEATHER_DEMAND_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(334),GAS_STORAGE_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(335),FUEL_PRICE_F0_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(351),CORES_AND_MACHINES_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(352),RPS_REQUIREMENTS_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(353),GQB_GAS_THERMAL_FILE)
     equivalence (OVERLAY_FILE_NAME_ARRAY(354),COAL_NODE_FILE)
				  
      equivalence (OVERLAY_FILE_NAME_ARRAY(355),COAL_TRANSPORT_FILE) 
                  
	 equivalence  (OVERLAY_FILE_NAME_ARRAY(356),COAL_SUPPLY_FORECAST_FILE)
				  
     equivalence  (OVERLAY_FILE_NAME_ARRAY(357),COAL_DEMAND_FORECAST_FILE)
				  
     equivalence  (OVERLAY_FILE_NAME_ARRAY(358),COAL_FORECAST_T1), & 
                  (OVERLAY_FILE_NAME_ARRAY(359),RPS_STATE_DEMAND), & 
                  (OVERLAY_FILE_NAME_ARRAY(360),THERMAL_RETROFIT), & 
                  (OVERLAY_FILE_NAME_ARRAY(361),COAL_CONTRACTS), & 
                  (OVERLAY_FILE_NAME_ARRAY(362),CO2_PARAM), & 
                  (OVERLAY_FILE_NAME_ARRAY(363),USER_DAY_D1),   &   ! 363  
                  (OVERLAY_FILE_NAME_ARRAY(364),USER_DAY_D2),   &  ! 364  
                  (OVERLAY_FILE_NAME_ARRAY(365),USER_DAY_D3),   &     ! 365  
                  (OVERLAY_FILE_NAME_ARRAY(366),USER_DAY_D4),    &    ! 366  
                  (OVERLAY_FILE_NAME_ARRAY(367),USER_DAY_D5),   &     ! 367  
                  (OVERLAY_FILE_NAME_ARRAY(368),USER_DAY_D6),  &      ! 368  
                  (OVERLAY_FILE_NAME_ARRAY(369),USER_DAY_D7),  &      ! 369  
                  (OVERLAY_FILE_NAME_ARRAY(370),USER_DAY_D8),   &     ! 370  
                  (OVERLAY_FILE_NAME_ARRAY(371),USER_DAY_D9),    &     ! 371 
                  (OVERLAY_FILE_NAME_ARRAY(372),COAL_MODEL_POINTER_FILE),  &       ! 372 & 
                  (OVERLAY_FILE_NAME_ARRAY(373),COAL_MODEL_SO2_INFO_FILE), &  ! 373 & 
                  (OVERLAY_FILE_NAME_ARRAY(374), COAL_MODEL_GENERIC_TRANSPORT_FILE), &   ! 374 & 
                  (OVERLAY_FILE_NAME_ARRAY(375), COAL_MODEL_GENERIC_DEMAND_FILE),  &  ! 375  
                  (OVERLAY_FILE_NAME_ARRAY(376),RPS_PROGRAM)    
end module overlay