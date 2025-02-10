! **********************************************************************
      SUBROUTINE SCREENING_OBJECT
      use end_routine, only: end_program, er_message
      use SpinDriftLib
      use prod_arrays_dimensions
      use mod_fuel_types
      use rps_data
      USE IREC_ENDPOINT_CONTROL
      USE GRX_PLANNING_ROUTINES
      USE CAPACITY_OPTIONS_ALLOC_VARS
      USE TRANS_GROUP_VARIABLES
      USE ICAP_RESULTS
      USE ABB_CapMarketRptData
      use capacity_arrays
      USE CapOptionsFileResourceIndexes
      use CAPACITY_OPTIONS_ALLOC_VARS
      USE SIZECOM
      use rptreccontrol
      use grxModules
      use annual_cl_unit
      use mwunih
      use dr_booth_modules
      use rps_index_translate_mod
      use globecom
      use prodcom


      CHARACTER (LEN=2) ::  CAPACITY_PLANNING_METHOD
      CHARACTER (LEN=2) ::  GREEN_MRX_METHOD
      INTEGER (kind=2) ::   MAX_SCREENING_INTERVALS
      INTEGER (kind=2) ::   R_YEAR,upper_trans_group
      PARAMETER(MAX_SCREENING_INTERVALS = 79)
      logical :: ifres1, ifres2
      integer :: ignored_result
      LOGICAL (kind=1) ::  RESOURCE_AVAILABLE
      LOGICAL (kind=1) ::  DEPENDENT_UNIT_AVAILABLE
      LOGICAL (kind=1) ::  TEST_CAP_VALUE_W_ADD=.TRUE.
      LOGICAL (kind=1) ::  CALC_SCREEN_SLOPE_INTERCEPT
      LOGICAL (kind=1) ::  SCREENING_NOT_OPEN=.TRUE.
      LOGICAL (kind=1) ::  CAPTURED_OPTIONS_NAMES
      LOGICAL (kind=1) ::  GET_OPTION_NAMES
      LOGICAL (kind=1) ::  RUN_MULTIAREA
      LOGICAL (kind=1) ::  RUN_PRICE_MODE
      LOGICAL (kind=1) ::  RUN_AREA_PRICE_MODE
      LOGICAL (kind=1) ::  YES_MULTI_AREA_PRICE
      LOGICAL (kind=1) ::  YES_STRICT_MARKET_PRICE
      LOGICAL (kind=1) ::  YES_RUN_MULTIAREA_TRANSACT
      LOGICAL (kind=1) ::  YES_RUN_TRANSACT
      LOGICAL (kind=1) ::  BUILD_TO_MARKET=.TRUE.
      LOGICAL (kind=1) ::  ANNUAL_MARKET_OPTION_SWITCH=.FALSE.
      LOGICAL (kind=1) ::  ANNUAL_RESOURCES_AVAILABLE
      LOGICAL (kind=1) ::  CX_ACTIVE=.true.
      LOGICAL (kind=1) ::  LOCAL_RESOURCES_AVAILABLE
      LOGICAL (kind=1) ::  TEST_START_STOP_YEARS
      LOGICAL (kind=1) ::  FUEL_PRICE_DATA_AVAILABLE=.FALSE.
      LOGICAL (kind=1) ::  PLANNING_PER_MWH
      LOGICAL (kind=1) ::  YES_PLAN_TO_MWH_OR_KWYR
      LOGICAL (kind=1) ::  REGIONAL_PARAMS_EXIST=.FALSE.
      LOGICAL (kind=1) ::  YES_REGIONAL_PARAMS_EXIST
      LOGICAL (kind=1) ::  MRX_SORTED_MIX
      LOGICAL (kind=1) ::  USE_EMIS_IN_MRX
      LOGICAL (kind=1) ::  YES_USE_EMIS_IN_MRX
      LOGICAL (kind=1) ::  SAME_TRANSACTION
      LOGICAL (kind=1) ::  HOUR_PATH_ACTIVE
      LOGICAL (kind=1) ::  CHRONO_MARKET_PRICES
      LOGICAL (kind=1) ::  CX_ALTER_MX_CHRONO_PRICES
      LOGICAL (kind=1) ::  CX_ANNUAL_CONTRACT
      LOGICAL (kind=1) ::  CX_DailyOperAnPumpedStorage
      LOGICAL (kind=1) ::  CX_DailyOperAnPS2
      LOGICAL (kind=1) ::  ANNUAL_STORAGE_SORT
      LOGICAL (kind=1) ::  CX_UNIT
      LOGICAL (kind=1) ::  GRX_UNIT_LINK_ACTIVE=.TRUE.
      LOGICAL (kind=1) ::  MUST_RUN_UNIT(:,:)
      LOGICAL (kind=1) ::  UPDATE_TRANS_FIXED_COSTS
      LOGICAL (kind=1) ::  OPTIMAL_ORDER
      LOGICAL (kind=1) ::  S_YES_MONTHLY_MUST_RUN
      LOGICAL (kind=1) ::  RESET_MRX_RPS_TEST_REQ
      LOGICAL (kind=1) ::  GET_RPS_NAME
      LOGICAL (kind=1) ::  RPS_CROSS_OVER
      INTEGER (kind=2) ::  RETURN_TOTAL_ALL_OPTIONS
      INTEGER (kind=2) ::  BEST_RESOURCE(MAX_SCREENING_INTERVALS)
      INTEGER (kind=2) ::  I
      INTEGER (kind=2) ::  J
      INTEGER (kind=2) ::  K
      INTEGER (kind=2) ::  L
      INTEGER (kind=2) ::  TG
      INTEGER (kind=2) ::  PA
      INTEGER (kind=2) ::  INITIALIZE_SCREEN_DATA
      INTEGER (kind=2) ::  THIS_YEAR
      INTEGER (kind=2) ::  THIS_YEAR_PLUS_LEAD_TIME(:)
      INTEGER (kind=2) ::  UPDATE_CL_SCREEN_COSTS
      INTEGER (kind=2) ::  VOID_INT2
      INTEGER (kind=2) ::  SCREENING_NO=0
      INTEGER (kind=2) ::  SCREENING_HEADER
      INTEGER (kind=2) ::  HIGHEST_OPTION
      INTEGER (kind=2) ::  NET_MARGIN_INTERVAL(:)
      INTEGER (kind=2) ::  TRANSACTION_GROUP(:)
      INTEGER (kind=2) ::  STATE_PROVINCE(:)
      INTEGER (kind=2) ::  GET_TRANS_STATE_INDEX
      INTEGER (kind=2) ::  GET_SCREEN_STATE_INDEX
      INTEGER (kind=2) ::  LOCAL_ANNUAL_UNITS(:)
      INTEGER (kind=2) ::  LOCAL_LEAD_TIME(:)
      INTEGER (kind=2) ::  FILE_SOURCE_INDEX(:)
      INTEGER (kind=2) ::  GET_FILE_SOURCE_INDEX
      INTEGER (kind=2) ::  OPTION_POSITION(:)
      INTEGER (kind=2) ::  ACTIVE_OPTION(:)
      INTEGER (kind=2) ::  FIRST_CN(:)
      INTEGER (kind=2) ::  AO
      INTEGER (kind=2) ::  TOTAL_ACTIVE_INSTANCES
      INTEGER (kind=2) ::  AI
      INTEGER (kind=2) ::  M
      INTEGER (kind=2) ::  CX_ITER
      INTEGER (kind=2) ::  MAX_CX_ITER
      INTEGER (kind=2) ::  ACTIVE_INSTANCE(:)
      INTEGER (kind=2) ::  LAST_RESOURCE_ADDED(:)
      INTEGER (kind=2) ::  LAST_PA_RESOURCE_ADDED(:)
      INTEGER (kind=2) ::  LAST_CM_RESOURCE_ADDED(:)
      INTEGER (kind=2) ::  FIRST_CT_4_EAS_INDEX(:)
      INTEGER (kind=2) ::  LOWEST_FIXED_COST_PA(:)
      INTEGER (kind=2) ::  LAST_SORTED_POSITION=0
      INTEGER (kind=2) ::  HIGHEST_OPTION_FOR_TG(:)
      INTEGER (kind=2) ::  LSP
      INTEGER (kind=2) ::  CN
      INTEGER (kind=2) ::  ConeLink
      INTEGER (kind=2) ::  HIGHEST_INTERVAL
      INTEGER (kind=2) ::  MX_RES_P_YEAR
      INTEGER (kind=2) ::  MAX_RESOURCES_PER_YEAR_GROUP
      INTEGER (kind=2) ::  MAX_RESOURCES_PER_GROUP
      INTEGER (kind=2) ::  PROD_POINTER
      INTEGER (kind=2) ::  HYBRID_PROD_POINTER
      INTEGER (kind=2) ::  HYBRID_OPT_POINTER
      INTEGER (kind=2) ::  GET_TRANS_FOR_GRX_ID
      INTEGER (kind=2) ::  GET_TRANSMISSION_FOR_GRX_ID
      INTEGER (kind=2) ::  GET_GRX_RESOURCE_LINK_ID
      INTEGER (kind=2) ::  GET_TRANS_GROUP_FOR_TRANS
      INTEGER (kind=2) ::  GET_TG_FOR_TRANSMISSION_BUY
      INTEGER (kind=2) ::  GET_TG_FOR_TRANSMISSION_SELL
      INTEGER (kind=2) ::  GET_PRODUCTION_DATA_POINTER
      INTEGER (kind=2) ::  HIGHEST_J
      INTEGER (kind=2) ::  LOCAL_TIME_OF_DAY=1
      INTEGER (kind=2) ::  SELLER
      INTEGER (kind=2) ::  BUYER
      INTEGER (kind=2) ::  HR
      INTEGER (kind=2) ::  SELLER_CM
      INTEGER (kind=2) ::  BUYER_CM
      INTEGER (kind=2) ::  SELLER_PA
      INTEGER (kind=2) ::  BUYER_PA
      INTEGER (kind=2) ::  SELLER_TG
      INTEGER (kind=2) ::  BUYER_TG
      INTEGER (kind=2) ::  BEST_SELLER_TG
      INTEGER (kind=2) ::  BEST_SELLER_CM
      INTEGER (kind=2) ::  BEST_SELLER_PA
      INTEGER (kind=2) ::  BEST_BUYER_TG
      INTEGER (kind=2) ::  BEST_BUYER_CM
      INTEGER (kind=2) ::  BEST_BUYER_PA
      INTEGER (kind=2) ::  NUMBER_DIRECT_INTERCONNECTS(:)
      INTEGER (kind=2) ::  DIRECT_INTERCONNECT_TG(:,:)
      INTEGER (kind=2) ::  RETURN_SCREEN_OP_LIFE
      INTEGER (kind=2) ::  RETURN_CN_OP_LIFE
      INTEGER (kind=2) ::  OP_LIFE(:)
      INTEGER (kind=2) ::  GET_ICAP_SWITCH_INDEX
      INTEGER (kind=2) ::  ICAP_SWITCH_INDEX(:)
      INTEGER (kind=2) ::  U
      INTEGER (kind=2) ::  INT2_ONE=1
      INTEGER (kind=2) ::  GET_RESOURCE_ID_TO_UNIT
      INTEGER (kind=2) ::  GET_PRIMARY_MOVER_INDEX
      INTEGER (kind=2) ::  LAST_SELLER
      INTEGER (kind=2) ::  LAST_BUYER
      INTEGER (kind=2) ::  GET_NUMBER_OF_ACTIVE_GROUPS
      INTEGER (kind=2) ::  GET_TRANS_GROUP_INDEX
      INTEGER (kind=2) ::  GET_TRANS_GROUP_POSITION
      INTEGER (kind=2) ::  RETURN_S_TRANS_GROUP_ID
      INTEGER (kind=2) ::  TRANS_GROUP_4_PRICING
      INTEGER (kind=2) ::  ANNUAL_UNITS_FOR_OPTION
      INTEGER (kind=2) ::  CUMULATIVE_UNITS_FOR_OPTION
      INTEGER (kind=2) ::  MIN_UP_TIME(:)
      INTEGER (kind=2) ::  MIN_DOWN_TIME(:)
      INTEGER (kind=2) ::  MRX_RPS_MX_UNIT_CLEARED(:)
      INTEGER (kind=2) ::  GET_S_RPS_PROGRAM_NUMBER
      INTEGER (kind=2) ::  GET_TRANS_RPS_PROG_NUMBER
      INTEGER (kind=2) ::  LEAD_TIME_FOR_OPTION
      INTEGER (kind=2) ::  RPS_NO
      INTEGER (kind=2) ::  RPS_COUNT
      INTEGER (kind=2) ::  PX
      PARAMETER (MX_RES_P_YEAR = MAX_CL_UNITS) 
      ! 6000) 080306. UP FOR NAMER
      INTEGER (kind=2) ::  HGST_OPT_INDEX(MX_RES_P_YEAR)
      INTEGER (kind=2) ::  SORTED_OPTIONS(MX_RES_P_YEAR)
      INTEGER (kind=2) ::  RPS_RESOURCE_ADDED(MX_RES_P_YEAR)
      INTEGER (kind=2) ::  LAST_SORT_INSTANCE(MX_RES_P_YEAR)
      INTEGER (kind=2) ::  TEMP_INSTANCE(MX_RES_P_YEAR)
      INTEGER (kind=2) ::  HO
      INTEGER (kind=2) ::  EU
      INTEGER (kind=2) ::  NEXT_CN
      INTEGER (kind=2) ::  LAST_HO
      INTEGER (kind=2) ::  GET_REGIONAL_PA_NAME
      INTEGER (kind=2) ::  GET_REGIONAL_CM_NAME
      INTEGER (kind=2) ::  TEMP_I2
      INTEGER (kind=2) ::  INT2_PM
      INTEGER (kind=2) ::  RPS_PM_INDEX
      INTEGER (kind=2) ::  PM
      REAL (kind=4) ::  HGST_NET_MARG_ADDED(MX_RES_P_YEAR)
      REAL (kind=4) ::  GET_S_MIN_UP_TIME
      REAL (kind=4) ::  GET_S_MIN_DOWN_TIME
      INTEGER :: SCREENING_REC
      SAVE SCREENING_REC
      REAL (kind=4) ::  ConeLinkCapacity
      REAL (kind=4) ::  CONE_AnnualFixedCosts
      REAL (kind=4) ::  BEST_SLOPE(MAX_SCREENING_INTERVALS)
      REAL (kind=4) ::  BEST_INTERCEPT(MAX_SCREENING_INTERVALS)
      REAL (kind=4) ::  LOWEST_FIXED_COST_BY_PA(:)
      REAL (kind=4) ::  LOWEST_FIXED_COST_BY_CM(:)
      REAL (kind=4) ::  MARGINAL_ICAP_PER_KWYR_BY_TG(:)
      REAL (kind=4) ::  SCREEN_SLOPE(:)
      REAL (kind=4) ::  SCREEN_INTERCEPT(:)
      REAL (kind=4) ::  SCREEN_CAPACITY(:)
      REAL (kind=4) ::  ICAP_CONE_PRICE(:)
      REAL (kind=4) ::  EQUIVALENT_CAPACITY(:)
      REAL (kind=4) ::  BLOCK_CAPACITY(:,:)
      REAL (kind=4) ::  BLOCK_HEAT_RATE(:,:)
      REAL (kind=4) ::  ANNUAL_FIXED_COST(:)
      REAL (kind=4) ::  ANNUAL_VARIABLE_COST(:)
      REAL (kind=4) ::  VARIABLE_COST_MM(:)
      REAL (kind=4) ::  FIXED_COST_MM(:)
      REAL (kind=4) ::  HIGHEST_SCREEN_CAPACITY(:)
      REAL (kind=4) ::  NET_MARGIN(:)
      REAL (kind=4) ::  NET_MARGIN_PER_KWYR(:)
      REAL (kind=4) ::  MARKET_REVENUE(:)
      REAL (kind=4) ::  HIGHEST_ENERGY(:)
      REAL (kind=4) ::  MARKET_COST(:)
      REAL (kind=4) ::  RFT(:)
      REAL (kind=4) ::  RPM(:)
      REAL (kind=4) ::  START_UP_COSTS(:,:)
      REAL (kind=4) ::  GET_UNIT_START_UP_COSTS
      REAL (kind=4) ::  HIGHEST_RFT(:)
      REAL (kind=4) ::  HIGHEST_RPM(:)
      REAL (kind=4) ::  INTERVAL_MULTIPLIER
      REAL (kind=4) ::  R_SLOPE
      REAL (kind=4) ::  R_INTERCEPT
      REAL (kind=4) ::  SCREEN_VALUE(MAX_SCREENING_INTERVALS)
      REAL (kind=4) ::  BEST_VALUE(MAX_SCREENING_INTERVALS)
      REAL (kind=4) ::  HIGHEST_NET_MARGIN
      REAL (kind=4) ::  HIGHEST_NET_MARGIN_PER_MWH
      REAL (kind=4) ::  HIGHEST_NET_MARGIN_PER_KWYR
      REAL (kind=4) ::  HIGHEST_ICAP_DEMAND_CURVE_MULT
      REAL (kind=4) ::  HIGHEST_EAS_PROFIT_PER_KWYR
      REAL (kind=4) ::  HIGHEST_CONE_COST_PER_KWYR
      REAL (kind=4) ::  HIGHEST_CONE_DEM_ADJ_PER_KWYR
      REAL (kind=4) ::  HIGHEST_ICAP_REVENUE_PER_KWYR
      REAL (kind=4) ::  HIGHEST_CM_CAPACITY_MW
      REAL (kind=4) ::  HIGHEST_CM_RESERVE_MARGIN
      REAL (kind=4) ::  HIGHEST_CF(:)
      REAL (kind=4) ::  HIGHEST_ICAP_REVENUE
      REAL (kind=4) ::  HIGHEST_ENERGY_REVENUE
      REAL (kind=4) ::  MINIMUM_NET_MARGIN
      REAL (kind=4) ::  GET_ADDITIONS_PROFIT_PER_MWH
      REAL (kind=4) ::  GET_ANNUAL_VECTOR_VALUE
      REAL (kind=4) ::  TEMP_NET_MARGIN
      REAL (kind=4) ::  TEMP_NET_MARGIN_PER_KWYR
      REAL (kind=4) ::  TOTAL_AVERAGE_COST
      REAL (kind=4) ::  TOP_CAP_PERCENT
      REAL (kind=4) ::  GET_SCREEN_CAPACITY
      REAL (kind=4) ::  MARKET_CUM_REVENUE(:)
      REAL (kind=4) ::  MARKET_DURATION(:)
      REAL (kind=4) ::  MARKET_PRICE(:)
      REAL (kind=4) ::  STRIKE_PRICE(:,:)
      REAL (kind=4) ::  ANNUAL_LEVELIZED_CAPITAL(:)
      REAL (kind=4) ::  HIGHEST_ANN_LEVEL_CAP(:)
      REAL (kind=4) ::  ANNUAL_FUEL_COST(:)
      REAL (kind=4) ::  ANNUAL_VOM_COST
      REAL (kind=4) ::  ANNUAL_EMIS_COST
      REAL (kind=4) ::  HIGHEST_FUEL_COST(:)
      REAL (kind=4) ::  FUEL_COST_PER_MWH(:,:)
      REAL (kind=4) ::  MONTHLY_CAP_MULT(:,:)
      REAL (kind=4) ::  AVERAGE_FUEL_PRICE(:)
      REAL (kind=4) ::  VAR_OM_PER_MWH(:)
      REAL (kind=4) ::  FUEL_COST_PER_MMBTU(:)
      REAL (kind=4) ::  REVENUE_PER_MWH(:)
      REAL (kind=4) ::  HIGHEST_STRIKE_PRICE(:)
      REAL (kind=4) ::  HIGHEST_MRX_MARGIN(:)
      REAL (kind=4) ::  HIGHEST_MRX_CF(:)
      REAL (kind=4) ::  HIGHEST_MRX_ENERGY(:)
      REAL (kind=4) ::  HIGHEST_MARKET_COST(:)
      REAL (kind=4) ::  ENERGY_REVENUE_PER_MWH(:)
      REAL (kind=4) ::  HIGHEST_ENERGY_REV_PER_MWH(:)
      REAL (kind=4) ::  CAPACITY_REVENUE_PER_MWH(:)
      REAL (kind=4) ::  CAPACITY_REVENUE_MM(:)
      REAL (kind=4) ::  RPS_REVENUE_MM(:)
      REAL (kind=4) ::  RPS_REVENUE_PER_MWH(:)
      REAL (kind=4) ::  GET_RPS_REV_BY_OPTION
      REAL (kind=4) ::  PUT_RPS_PRICE_BY_OPTION
      REAL (kind=4) ::  PUT_RPS_PRICE_BY_PROGRAM
      REAL (kind=4) ::  ENERGY_REVENUE_MM(:)
      REAL (kind=4) ::  EMISSIONS_PER_MWH(:)
      REAL (kind=4) ::  FOM_PER_MWH(:)
      REAL (kind=4) ::  LEVELIZED_CAP_PER_MWH(:)
      REAL (kind=4) ::  FIXED_COST_PER_MWH(:)
      REAL (kind=4) ::  VOM_COST_MM(:)
      REAL (kind=4) ::  EMISSIONS_COST_MM(:)
      REAL (kind=4) ::  FOM_COST_MM(:)
      REAL (kind=4) ::  TRANS_GROUP_ID(:)
      REAL (kind=4) ::  TG_CAPACITY_MW(:)
      REAL (kind=4) ::  TG_PEAK_MW(:)
      REAL (kind=4) ::  FirmTransfer(:,:)
      REAL (kind=4) ::  CM_Net_Transfer(:)
      REAL (kind=4) ::  PA_Net_Transfer(:)
      REAL (kind=4) ::  TG_Net_Transfer(:)
      REAL (kind=4) ::  HIGHEST_TG_ICAP_REV_PER_KWYR(:)
      REAL (kind=4) ::  TransmissionConstraint(:,:)
      REAL (kind=4) ::  MaxTransmissionConstraint(:,:)
      REAL (kind=4) ::  GetCapTransMultiplier
      REAL (kind=4) ::  TransferValue(:,:)
      REAL (kind=4) ::  CM_INTERUPTIBLRESERVE_MARGIN
      REAL (kind=4) ::  FIRST_CT_EAS(:)
      REAL (kind=4) ::  TG_RESERVE_MARGIN(:)
      REAL (kind=4) ::  PLANNING_AREA_ID(:)
      REAL (kind=4) ::  PA_CAPACITY_MW(:)
      REAL (kind=4) ::  CM_CAPACITY_MW(:)
      REAL (kind=4) ::  PA_PEAK_MW(:)
      REAL (kind=4) ::  CM_PEAK_MW(:)
      REAL (kind=4) ::  PA_RESERVE_MARGIN(:)
      REAL (kind=4) ::  CM_RESERVE_MARGIN(:)
      REAL (kind=4) ::  CM_MAX_NET_ENRG_MARG_PER_KWYR(:)
      REAL (kind=4) ::  HEAT_RATE(:)
      REAL (kind=4) ::  HIGHEST_HEAT_RATE(:)
      REAL (kind=4) ::  HIGHEST_ANNUAL_UNITS(:)
      REAL (kind=4) ::  HIGHEST_MARKET_REVENUE(:)
      REAL (kind=4) ::  HIGHEST_MARGIN_PER_KWYR(:)
      REAL (kind=4) ::  HIGHEST_FUEL_COST_PER_MWH(:)
      REAL (kind=4) ::  HIGHEST_VAR_OM_PER_MWH(:)
      REAL (kind=4) ::  HIGHEST_FUEL_COST_PER_MMBTU(:)
      REAL (kind=4) ::  HIGHEST_EMISSIONS_PER_MWH(:)
      REAL (kind=4) ::  HIGHEST_FOM_PER_MWH(:)
      REAL (kind=4) ::  HIGHEST_LEVEL_CAP_PER_MWH(:)
      REAL (kind=4) ::  HIGHEST_VOM_COST_MM(:)
      REAL (kind=4) ::  HIGHEST_EMISSIONS_COST_MM(:)
      REAL (kind=4) ::  HIGHEST_FOM_COST_MM(:)
      REAL (kind=4) ::  FIX_OM_COST(:)
      REAL (kind=4) ::  CAPACITY_PLANNING_MW(:)
      REAL (kind=4) ::  GET_TRANS_RPS_PERCENT
      REAL (kind=4) ::  GET_S_RPS_PERCENT
      REAL (kind=4) ::  RPS_PERCENT(:)
      REAL (kind=4) ::  GET_TRANS_CAP_PLAN_FAC
      REAL (kind=4) ::  FIRST_CF(:)
      REAL (kind=4) ::  CF_FACTOR
      REAL (kind=4) ::  RETURN_OTHER_VAR_COMPONENT
      REAL (kind=4) ::  RETURN_CL_SCREEN_FIXED_COST
      REAL (kind=4) ::  GET_S_CAP_PLANNING_FAC
      REAL (kind=4) ::  MARKET_HOURS
      REAL (kind=4) ::  MARKET_AVE_REVENUE
      REAL (kind=4) ::  CF
      REAL (kind=4) ::  ANNUAL_ENERGY
      REAL (kind=4) ::  ANNUAL_STRIKES
      REAL (kind=4) ::  RETURN_RISK_ADJ_S_CAP_COST
      REAL (kind=4) ::  RETURN_CL_SCREEN_FUEL_COST
      REAL (kind=4) ::  RETURN_CL_SCREEN_EMIS_COST
      REAL (kind=4) ::  HIGHEST_TOP_CAP_PERCENT
      REAL (kind=4) ::  FIRST_YEAR_CAPACITY
      REAL (kind=4) ::  ADD_THIS_UNIT
      REAL (kind=4) ::  PEAK
      REAL (kind=4) ::  UPDATE_NET_PLANNING_PEAK
      REAL (kind=4) ::  GET_VAR
      REAL (kind=4) ::  RETURN_CL_SCREEN_EFOR
      REAL (kind=4) ::  SCENARIO_CAP_COST_MULT
      REAL (kind=4) ::  GET_SCENARIO_EXPAND_CAP_COST
      REAL (kind=4) ::  SCENARIO_RESERVE_MARGIN
      REAL (kind=4) ::  GET_SCENARIO_RESERVE_MARGIN
      REAL (kind=4) ::  GET_ICAP_REVENUE_MULT
      REAL (kind=4) ::  ANNUAL_ICAP_REVENUE
      REAL (kind=4) ::  ANNUAL_ENERGY_REVENUE(:)
      REAL (kind=4) ::  NOX_VOM
      REAL (kind=4) ::  NOX_FOM
      REAL (kind=4) ::  SOX_VOM
      REAL (kind=4) ::  SOX_FOM
      REAL (kind=4) ::  CO2_VOM
      REAL (kind=4) ::  CO2_FOM
      REAL (kind=4) ::  HG_VOM
      REAL (kind=4) ::  HG_FOM
      REAL (kind=4) ::  OTHER3_VOM
      REAL (kind=4) ::  OTHER3_FOM
      REAL (kind=4) ::  NOX_CONTROL_MULT
      REAL (kind=4) ::  SOX_CONTROL_MULT
      REAL (kind=4) ::  CO2_CONTROL_MULT
      REAL (kind=4) ::  HG_CONTROL_MULT
      REAL (kind=4) ::  OTHER3_CONTROL_MULT
      REAL (kind=4) ::  INSTALL_CAP_VALUE_LAST_PA_UNIT(:)
      REAL (kind=4) ::  INSTALL_CAP_VALUE_LAST_CM_UNIT(:)
      REAL (kind=4) ::  R_MRX_CAPACITY
      REAL (kind=4) ::  R_MRX_ENERGY
      REAL (kind=4) ::  R_MRX_TOTAL_REVENUE
      REAL (kind=4) ::  R_MRX_TOTAL_VARIABLE
      REAL (kind=4) ::  R_MRX_FUEL
      REAL (kind=4) ::  R_MRX_VOM
      REAL (kind=4) ::  R_MRX_EMISSIONS
      REAL (kind=4) ::  R_MRX_TOTAL_COST
      REAL (kind=4) ::  R_MRX_ANN_CAP_COST
      REAL (kind=4) ::  R_MRX_NET_MARGIN_MM
      REAL (kind=4) ::  R_MRX_NET_MARGIN_PER_MWH
      REAL (kind=4) ::  R_MRX_NET_MARGIN_PER_KWMO
      REAL (kind=4) ::  R_MRX_CONE_PER_KWMO
      REAL (kind=4) ::  R_MRX_EAS_REV_PER_KWMO
      REAL (kind=4) ::  R_MRX_ICAP_VALUE_POINT
      REAL (kind=4) ::  PRICE_OF_TRANSFER
      REAL (kind=4) ::  PRICE_CEILING
      REAL (kind=4) ::  QUANTITY_OF_TRANSFER
      REAL (kind=4) ::  R_TG_TRANSFER_CAPACITY
      REAL (kind=4) ::  MRX_ICAP_INDEX(:)
      REAL (kind=4) ::  LOCAL_CHRONO_MARKET_PRICES(:,:)
      REAL (kind=4) ::  RESET_CHRONO_MARKET_PRICES(:,:)
      REAL (kind=4) ::  LOCAL_CHRONO_MW_USAGE(8784)
      REAL (kind=4) ::  LOCAL_CHRONO_STRIKE_PRICE(8784)
      REAL (kind=4) ::  HIGHEST_CHRONO_MW_USAGE(8784)
      REAL (kind=4) ::  LOCAL_HYBRID_MW_CHARGE(8784)
      REAL (kind=4) ::  INDEPENDENT_MW_USAGE(8784)
      REAL (kind=4) ::  ADD_HYBRID_MW_CHARGE(8784)
      REAL (kind=4) ::  LOCAL_HYBRID_MW_DISCHARGE(8784)
      REAL (kind=4) ::  LOCAL_HYBRID_HR_MARGIN(8784)
      REAL (kind=4) ::  CONSTRAIN_HYBRID_MARGIN(8784)
      REAL (kind=4) ::  CONSTRAIN_HYBRID_MW_USAGE(8784)
      REAL (kind=4) ::  MARKET_HYBRID_MW_USAGE(8784)
      REAL (kind=4) ::  HYBRID_MARKET_MW_USAGE(8784)
      REAL (kind=4) ::  LOCAL_HYBRID_PROFIT(8784)
      REAL (kind=4) ::  INDEPENDENT_ANNUAL_ENERGY
      REAL (kind=4) ::  ANNUAL_CHARGE
      REAL (kind=4) ::  CONSTRAIN_ANNUAL_ENERGY
      REAL (kind=4) ::  CONSTRAIN_ANNUAL_CHARGE
      REAL (kind=4) ::  HYBRID_MARKET_ANNUAL_ENERGY
      REAL (kind=4) ::  HYBRID_MARKET_ANNUAL_CHARGE
      REAL (kind=4) ::  HYBRID_MARKET_ANNUAL_REVENUE
      REAL (kind=4) ::  INDEPENDENT_FUEL_COST
      REAL (kind=4) ::  INDEPENDENT_VOM_COST
      REAL (kind=4) ::  CONSTRAIN_VOM_COST
      REAL (kind=4) ::  HYBRID_MARKET_VOM_COST
      REAL (kind=4) ::  INDEPENDENT_ANNUAL_REVENUE
      REAL (kind=4) ::  CONSTRAIN_ANNUAL_REVENUE
      REAL (kind=4) ::  INDEPENDENT_EMIS_COST
      REAL (kind=4) ::  HYBRID_MARKET_ENERGY_LIM
      REAL (kind=4) ::  CALC_GRX_FIX_COST_PER_UNIT
      CHARACTER (len=5) ::   GET_SCENAME,CL_UNIQUE_RPT_STR
      CHARACTER (len=20) ::  OPTION_NAME(:)
      CHARACTER (len=20) ::  PA_NAME
      CHARACTER (len=20) ::  CX_NAME
      CHARACTER (len=20) ::  RPS_NAME*50
      CHARACTER (len=25) ::  CURVE_NAME,TEMP_CURVE_NAME
      CHARACTER (len=35) ::  MULTI_AREA_NAME(:)
      CHARACTER (len=35) ::  GET_GROUP_NAME
      CHARACTER (len=35) ::  CM_NAME
      CHARACTER (len=22) ::  LOCAL_NAME
      CHARACTER (len=256) :: TEMP_STR
      ALLOCATABLE :: SCREEN_SLOPE,SCREEN_INTERCEPT,OPTION_NAME, &
                        OPTION_POSITION, &
                        ACTIVE_OPTION, &
                        FIRST_CN, &
                        ACTIVE_INSTANCE, &
                        LAST_RESOURCE_ADDED, &
                        LAST_PA_RESOURCE_ADDED, &
                        LAST_CM_RESOURCE_ADDED, &
                        FIRST_CT_EAS, &
                        FIRST_CT_4_EAS_INDEX, &
                        LOWEST_FIXED_COST_PA, &
                        LOWEST_FIXED_COST_BY_PA, &
                        LOWEST_FIXED_COST_BY_CM, &
                        MARGINAL_ICAP_PER_KWYR_BY_TG, &
                        INSTALL_CAP_VALUE_LAST_PA_UNIT, &
                        INSTALL_CAP_VALUE_LAST_CM_UNIT, &
                        SCREEN_CAPACITY, &
                        ANNUAL_ENERGY_REVENUE, &
                        EQUIVALENT_CAPACITY, &
                        BLOCK_CAPACITY, &
                        BLOCK_HEAT_RATE, &
                        ANNUAL_FIXED_COST, &
                        ANNUAL_VARIABLE_COST, &
                        VARIABLE_COST_MM, &
                        FIXED_COST_MM, &
                        HIGHEST_SCREEN_CAPACITY, &
                        NET_MARGIN, &
                        NET_MARGIN_PER_KWYR, &
                        NET_MARGIN_INTERVAL, &
                        THIS_YEAR_PLUS_LEAD_TIME, &
                        MARKET_REVENUE, &
                        HIGHEST_ENERGY, &
                        MARKET_COST, &
                        MARKET_CUM_REVENUE, &
                        MARKET_DURATION, &
                        MARKET_PRICE, &
                        STRIKE_PRICE, &
                        TRANSACTION_GROUP, &
                        STATE_PROVINCE, &
                        LOCAL_ANNUAL_UNITS, &
                        RFT, &
                        MIN_UP_TIME, &
                        MIN_DOWN_TIME, &
                        START_UP_COSTS, &
                        MUST_RUN_UNIT, &
                        RPM, &
                        HIGHEST_RFT, &
                        HIGHEST_RPM, &
                        LOCAL_LEAD_TIME, &
                        FILE_SOURCE_INDEX, &
                        ANNUAL_LEVELIZED_CAPITAL, &
                        OP_LIFE, &
                        ICAP_SWITCH_INDEX, &
                        ICAP_CONE_PRICE, &
                        HIGHEST_ANN_LEVEL_CAP, &
                        ANNUAL_FUEL_COST, &
                        HIGHEST_FUEL_COST, &
                        FUEL_COST_PER_MWH, &
                        MONTHLY_CAP_MULT, &
                        AVERAGE_FUEL_PRICE, &
                        VAR_OM_PER_MWH, &
                        FUEL_COST_PER_MMBTU, &
                        REVENUE_PER_MWH, &
                        CAPACITY_REVENUE_PER_MWH, &
                        CAPACITY_REVENUE_MM, &
                        RPS_REVENUE_MM, &
                        MRX_RPS_MX_UNIT_CLEARED, &
                        RPS_REVENUE_PER_MWH, &
                        ENERGY_REVENUE_MM, &
                        ENERGY_REVENUE_PER_MWH, &
                        HIGHEST_ENERGY_REV_PER_MWH, &
                        HIGHEST_STRIKE_PRICE, &
                        HIGHEST_MRX_MARGIN, &
                        HIGHEST_MRX_ENERGY, &
                        HIGHEST_MARKET_COST, &
                        HIGHEST_MRX_CF, &
                        EMISSIONS_PER_MWH, &
                        FOM_PER_MWH, &
                        LEVELIZED_CAP_PER_MWH, &
                        FIXED_COST_PER_MWH, &
                        MRX_ICAP_INDEX, &
                        LOCAL_CHRONO_MARKET_PRICES, &
                        RESET_CHRONO_MARKET_PRICES, &
                        VOM_COST_MM, &
                        EMISSIONS_COST_MM, &
                        FOM_COST_MM, &
                        TRANS_GROUP_ID, &
                        TG_CAPACITY_MW, &
                        TG_PEAK_MW, &
                        FirmTransfer, &
                        CM_Net_Transfer, &
                        TG_Net_Transfer, &
                        PA_Net_Transfer, &
                        NUMBER_DIRECT_INTERCONNECTS, &
                        DIRECT_INTERCONNECT_TG, &
                        HIGHEST_TG_ICAP_REV_PER_KWYR, &
                        HIGHEST_OPTION_FOR_TG, &
                        TransmissionConstraint, &
                        MaxTransmissionConstraint, &
                        TransferValue, &
                        TG_RESERVE_MARGIN, &
                        PLANNING_AREA_ID, &
                        PA_CAPACITY_MW, &
                        CM_CAPACITY_MW, &
                        PA_PEAK_MW, &
                        CM_PEAK_MW, &
                        PA_RESERVE_MARGIN, &
                        CM_RESERVE_MARGIN, &
                        CM_MAX_NET_ENRG_MARG_PER_KWYR, &
                        HIGHEST_CF, &
                        HEAT_RATE, &
                        HIGHEST_HEAT_RATE, &
                        HIGHEST_ANNUAL_UNITS, &
                        HIGHEST_MARKET_REVENUE, &
                        HIGHEST_MARGIN_PER_KWYR, &
                        HIGHEST_FUEL_COST_PER_MWH, &
                        HIGHEST_VAR_OM_PER_MWH, &
                        HIGHEST_FUEL_COST_PER_MMBTU, &
                        HIGHEST_EMISSIONS_PER_MWH, &
                        HIGHEST_FOM_PER_MWH, &
                        HIGHEST_LEVEL_CAP_PER_MWH, &
                        HIGHEST_VOM_COST_MM, &
                        HIGHEST_EMISSIONS_COST_MM, &
                        HIGHEST_FOM_COST_MM, &
                        FIX_OM_COST, &
                        CAPACITY_PLANNING_MW, &
                        RPS_PERCENT, &
                        FIRST_CF, &
                        MULTI_AREA_NAME
         SAVE &
               ANNUAL_FUEL_COST, &
               OPTION_NAME, &
               OPTION_POSITION, &
               LAST_RESOURCE_ADDED, &
               LAST_PA_RESOURCE_ADDED, &
               LAST_CM_RESOURCE_ADDED, &
               FIRST_CT_EAS, &
               FIRST_CT_4_EAS_INDEX, &
               LOWEST_FIXED_COST_PA, &
               LOWEST_FIXED_COST_BY_PA, &
               LOWEST_FIXED_COST_BY_CM, &
               MARGINAL_ICAP_PER_KWYR_BY_TG, &
               INSTALL_CAP_VALUE_LAST_PA_UNIT, &
               INSTALL_CAP_VALUE_LAST_CM_UNIT, &
               HIGHEST_NET_MARGIN, & !  3
               VARIABLE_COST_MM, & !  7
               FIXED_COST_MM, & !  8
               HIGHEST_SCREEN_CAPACITY, & !  9
               HIGHEST_ENERGY, & !  10
               HIGHEST_ANN_LEVEL_CAP, & !  12
               HIGHEST_FUEL_COST, & !  13
               MARKET_REVENUE, & !  16
               NET_MARGIN_PER_KWYR, & !  17
               VOM_COST_MM, & !  26
               EMISSIONS_COST_MM ! 27
!
      LOGICAL (kind=1) ::  MX_SUMMARY_REPORT
      LOGICAL (kind=1) ::  EXPANSION_REPORT
      LOGICAL (kind=1) ::  MX_REPORT_NOT_OPEN=.TRUE.
      LOGICAL (kind=1) ::  YES_INCLUDE_ICAP_REVENUE
      LOGICAL (kind=1) ::  INCLUDE_ICAP_REVENUE
      LOGICAL (kind=1) ::  VN_REPORT_NOT_OPEN=.TRUE.
      LOGICAL (kind=1) ::  RPS_CURVE_REPORT
      LOGICAL (kind=1) ::  YES_RPS_CURVE_REPORT
      INTEGER (kind=2) ::  MX_REPORT_VARIABLES=53
      INTEGER (kind=2) ::  MX_ANNUAL_ALT_NO=0
      INTEGER (kind=2) ::  MX_ANNUAL_HEADER
      INTEGER (kind=2) ::  VN_REPORT_VARIABLES=9
      INTEGER (kind=2) ::  VN_ANNUAL_ALT_NO=0
      INTEGER (kind=2) ::  RPS_CURVE_RPT_HEADER
      INTEGER ::  MX_ANNUAL_ALT_REC
      INTEGER ::  VN_ANNUAL_ALT_REC
      SAVE MX_ANNUAL_ALT_REC, &
                  VN_ANNUAL_ALT_REC
!
      LOGICAL (kind=1) ::  GRX_8761_REPORT
      LOGICAL (kind=1) ::  GRX_8761_REPORT_ACTIVE
      LOGICAL (kind=1) ::  GRX_8761_REPORT_NOT_OPEN=.TRUE.
      INTEGER (kind=2) ::  GRX_8761_REPORT_VARIABLES
      INTEGER (kind=2) ::  GRX_8761_ALT_NO=0
      INTEGER (kind=2) ::  GRX_8761_HEADER
      INTEGER :: GRX_8761_ALT_REC=0 ,ITER,MAX_ITER
      SAVE GRX_8761_ALT_REC
!
      LOGICAL (kind=1) ::  FUEL_POINTERS_USED
      LOGICAL (kind=1) ::  EXISTING_UNIT
      LOGICAL (kind=1) ::  IS_IN_RPS_PROGRAM
      INTEGER (kind=2) ::  N
      INTEGER (kind=2) ::  FT
      INTEGER (kind=2) ::  MO
      INTEGER (kind=2) ::  GET_NUNITS_POSITION
      INTEGER (kind=2) ::  FUEL_INVENTORY_ID(0:1024)
      INTEGER (kind=2) ::  GET_PRIMARY_MOVER
      INTEGER (kind=2) ::  GET_S_PRIMARY_MOVER
      INTEGER (kind=2) ::  GET_S_PRIMARY_MOVER_INDEX
      INTEGER (kind=2) ::  GET_DERIV_PRIM_MOVER
      INTEGER (kind=2) ::  GET_DERIV_PRIM_MOVER_INDEX
      INTEGER (kind=2) ::  CM_UNIT_NO
      INTEGER (kind=2) ::  ST
      INTEGER (kind=2) ::  NUM_RPS_REGIONS
      INTEGER (kind=2) ::  REGION
      INTEGER (kind=2) ::  RPS_PROGRAMS
      INTEGER (kind=2) ::  NUMBER_OF_RPS_PROGRAMS
      INTEGER (kind=2) ::  GET_RPS_PROGRAM_POSITION
      INTEGER (kind=2) ::  RPS_SYSTEM
      INTEGER (kind=2) ::  RPS_STATE_REGION(0:61)
      INTEGER (kind=2) ::  RPS_REGION_POSITION(0:99)
      INTEGER (kind=2) ::  OPTION_REGION
      INTEGER (kind=2) ::  D
      INTEGER (kind=2) ::  LAST_EU
      INTEGER (kind=2) ::  BID_POS
      INTEGER (kind=2) ::  ED
      INTEGER (kind=2) ::  LAST_ED
      INTEGER (kind=2) ::  LAST_EN
      INTEGER (kind=2) ::  EN
      INTEGER (kind=2) ::  FN
      REAL (kind=4) ::  GET_SCENARIO_COAL_PRICES
      REAL (kind=4) ::  GET_SCENARIO_GAS_PRICES
      REAL (kind=4) ::  GET_SCENARIO_OIL_PRICES
      REAL (kind=4) ::  GET_SCENARIO_URANIUM_PRICES
      REAL (kind=4) ::  DISP_BTU_COST(MAX_CL_UNITS)
      REAL (kind=4) ::  BLENDED_BTU_COST(MAX_CL_UNITS)
      REAL (kind=4) ::  FUEL_SCEN_MULT(5)
      REAL (kind=4) ::  RTEMP
      REAL (kind=4) ::  PUT_MRX_DELIVERY_COST
      REAL (kind=4) ::  GET_MONTHLY_FUEL_INDEX
      REAL (kind=4) ::  R_DELIVERY_COST
      REAL (kind=4) ::  VOID_R4
      REAL (kind=4) ::  MAXIMUM_ANNUAL_MRX_CAP(:)
      REAL (kind=4) ::  PA_ICAP_REVENUE_MULT(:)
      REAL (kind=4) ::  CM_ICAP_REVENUE_MULT(:)
      REAL (kind=4) ::  MINIMUM_ANNUAL_MRX_CAP(:)
      REAL (kind=4) ::  GET_MIN_CAP_TESTING_RATIO
      REAL (kind=4) ::  GET_MAX_CAP_TESTING_RATIO
      REAL (kind=4) ::  MINIMUM_TG_MRX_CAP(:)
      REAL (kind=4) ::  MAXIMUM_TG_MRX_CAP(:)
      REAL (kind=4) ::  GET_MAXIMUM_ANNUAL_MRX_CAP
      REAL (kind=4) ::  TOTAL_ANNUAL_CAPACITY_ADDED(:)
      REAL (kind=4) ::  CM_TOT_ANN_CAPACITY_ADDED(:)
      REAL (kind=4) ::  TG_TOTAL_ANNUAL_CAPACITY_ADDED(:)
      REAL (kind=4) ::  GET_CM_RETIREMENT_KW
      REAL (kind=4) ::  GET_NEW_UNIT_RETIRE_VALUE_BY_CM
      REAL (kind=4) ::  ANN_CAP
      REAL (kind=4) ::  EL_PLANNING_CAPACITY
      REAL (kind=4) ::  CL_PLANNING_CAPACITY
      REAL (kind=4) ::  CT_PLANNING_CAPACITY
      REAL (kind=4) ::  LM_PLANNING_CAPACITY
      REAL (kind=4) ::  ADJUSTMENT_CAPACITY
      REAL (kind=4) ::  DERIV_CAPACITY_PLANNING
      REAL (kind=4) ::  DERIV_CAPACITY_PLANNING_BY_TG
      REAL (kind=4) ::  DERIV_NEW_CAP_PLAN_BY_ALL
      REAL (kind=4) ::  CURRENT_TARGET_RATIO
      REAL (kind=4) ::  CURRENT_TARGET_RATIO_PA(:)
      REAL (kind=4) ::  TARGET_RATIO
      REAL (kind=4) ::  RETURN_RESERVE_MARGIN_RATIO
      REAL (kind=4) ::  MAXIMUM_RM
      REAL (kind=4) ::  MAXIMUM_RM_PA(:)
      REAL (kind=4) ::  MAX_CAPACITY_TESTING_MARGIN
      REAL (kind=4) ::  ICAP_MIN_TESTING_MARGIN
      REAL (kind=4) ::  ICAP_MAX_TESTING_MARGIN
      REAL (kind=4) ::  TOTAL_REGIONAL_CAPACITY_NEEDED
      REAL (kind=4) ::  GET_EMISS_CAP_FOR_CLASS
      REAL (kind=4) ::  CO2_EMISSIONS_CAP(:)
      REAL (kind=4) ::  CO2_EMISS_REDUCTION_REQUIRED(:)
      REAL (kind=4) ::  CO2_EMISSIONS(:)
      REAL (kind=4) ::  GET_CL_EMISS_FOR_CLASS
      REAL (kind=4) ::  CO2_UNIT_MW
      REAL (kind=4) ::  CO2_UNIT_MWH
      REAL (kind=4) ::  CO2_UNIT_CO2
      REAL (kind=4) ::  UNIT_CO2
      REAL (kind=4) ::  CO2_UNIT_PRICE
      REAL (kind=4) ::  CO2_STRIKE_PRICE
      REAL (kind=4) ::  CO2_STRIKE_PRICE_AFTER
      REAL (kind=4) ::  CO2_UNIT_MW_AFTER
      REAL (kind=4) ::  UNIT_CO2_AFTER
      REAL (kind=4) ::  GET_CL_CO2_TON_PER_MWH
      REAL (kind=4) ::  LOWEST_TRANSFER_COST
      REAL (kind=4) ::  LOWEST_TRANSFER_CAPACITY
      REAL (kind=4) ::  STATE_SURPLUS_VARS(0:7)
      REAL (kind=4) ::  SYSTEM_SURPLUS_VARS(0:7)
      REAL (kind=4) ::  RPS_PROGRAM_BALANCE
      REAL (kind=4) ::  RPS_ALT_PRICE
      REAL (kind=4) ::  RPS_ALT_CAP_PRICE
      REAL (kind=4) ::  BREAK_EVEN_REV_REQ
      REAL (kind=4) ::  RPS_REC_PRICE
      REAL (kind=4) ::  RPS_GEN_WEIGHTED_REV
      REAL (kind=4) ::  RPS_GEN_WEIGHTED_PRICE
      REAL (kind=4) ::  RPS_ACP_REV
      REAL (kind=4) ::  CAPACITY_REV
      REAL (kind=4) ::  GET_LOCAL_ENDING_BALANCE
      REAL (kind=4) ::  GET_ANNUAL_RPS_PROG_REQ
      REAL (kind=4) ::  PROG_REQ
      REAL (kind=4) ::  ZERO_BID_MWH
      REAL (kind=4) ::  GET_QUALIFYING_GEN_DB
      REAL (kind=4) ::  GET_RPS_ALT_COMPLIANCE_PRICE
      REAL (kind=4) ::  RPS_ALT_COMPLIANCE_PRICE
      REAL (kind=4) ::  MIN_BID
      REAL (kind=4) ::  MAX_BID
      REAL (kind=4) ::  NEW_BID
      REAL (kind=4) ::  EXISTING_CL_BID
      REAL (kind=4) ::  EXISTING_DV_BID
      REAL (kind=4) ::  TEMP_BID
      REAL (kind=4) ::  TEMP_REVENUE
      REAL (kind=4) ::  TEMP_COST
      REAL (kind=4) ::  TEMP_MWH
      REAL (kind=4) ::  CUMULATIVE_MWH
      REAL (kind=4) ::  PA_BID(30000)
      REAL (kind=4) ::  PA_COST(30000)
      REAL (kind=4) ::  PA_REVENUE(30000)
      REAL (kind=4) ::  PA_CUM_MWH(30000)
      CHARACTER (len=1) ::   USE_MINIMUM_RM
      LOGICAL (kind=1) ::  USE_MAXIMUM_RM_LOGIC
      LOGICAL (kind=1) ::  USE_MINIMUM_RM_LOGIC
      LOGICAL (kind=1) ::  USE_REGIONAL_MINIMUM_RM_LOGIC
      LOGICAL (kind=1) ::  USE_REGIONAL_MAXIMUM_RM_LOGIC
      LOGICAL (kind=1) ::  TARGET_MET
      LOGICAL (kind=1) ::  YES_USE_MINIMUM_RM
      LOGICAL (kind=1) ::  VOID_LOGICAL
      LOGICAL (kind=1) ::  RESOURCE_ADDITION
      LOGICAL (kind=1) ::  TEMP_L
      LOGICAL (kind=1) ::  CALC_MRX_RPS_CURVES
      LOGICAL (kind=1) ::  GET_TRANS_NAME
      LOGICAL (kind=1) ::  STORAGE_IS_ACTIVE=.TRUE.
      LOGICAL (kind=1) ::  CONTRACT_IS_STORAGE
      LOGICAL (kind=1) ::  GET_S_BLOCK_CAP_AND_HR
      LOGICAL (kind=1) ::  INIT_HOUR_PATH_LIMIT
      LOGICAL (kind=1) ::  ADJUST_CO2_RETRO_PLAN_CAP
      LOGICAL (kind=1) ::  GET_NEXT_MRX_RETIREMENT
      LOGICAL (kind=1) ::  CO2_END_LIST
      LOGICAL (kind=1) ::  GET_CO2_RETIREMENTS_LOGIC
      LOGICAL (kind=1) ::  CO2_RETIREMENTS_LOGIC
      LOGICAL (kind=1) ::  GET_NEXT_MRX_RETIRE_RETRO
      LOGICAL (kind=1) ::  RETIRE_CO2_THERMAL_UNIT
      LOGICAL (kind=1) ::  RETROFIT_CO2_THERMAL_UNIT
      LOGICAL (kind=1) ::  TARGET_IS_PA
      LOGICAL (kind=1) ::  TG_MW_TRANSFERS
      LOGICAL (kind=1) ::  MARGINAL_ICAP
      LOGICAL (kind=1) ::  GRX_RPS_MODULE_IS_ACTIVE
      LOGICAL (kind=1) ::  GRX_RPS_MODULE_ACTIVE
      LOGICAL (kind=1) ::  RPS_ONLY_SWITCH_ACTIVE
      LOGICAL (kind=1) ::  RPS_ONLY_SWITCH
!
! DECLARATION FOR RESERVE MARGIN BY TG
!
      INTEGER (kind=2) :: &
           GET_PA_FROM_TG, &
           GET_CM_FROM_TG, &
           TG_2_PLANNING_AREA(:), &
           TG_2_CAPACITY_MARKET(:), &
           PG,CG,CM,CM_INDEX, &
           GET_CM_INDEX_FROM_TG, &
           R_PA, &
           R_TG, &
           TEMP_PA, &
           TEMP_CM, &
           GET_NUMBER_OF_PLANNING_GROUPS, &
           GET_NUMBER_OF_CAPACITY_MARKETS, &
           CO2_PLN=3 , &
           CO2_COUNTER, &
           CO2_RETIRE_OR_RETRO, &
           CO2_NUNIT, &
           CO2_TG, &
           BASE_DATE, &
           ZERO_I2=0
      REAL ::  PA_PLANNING_CAPACITY(:)
      REAL ::  CM_PLANNING_CAPACITY(:)
      REAL ::  CM_IOTA(:)
      REAL ::  IOTA_BY_CN(:)
      REAL ::  TG_PLANNING_CAPACITY(:)
      REAL ::  PEAK_AFTER_INTERRUPTIBLE_TG(:)
      REAL ::  TransferConvergence=0.01
      REAL ::  SELLER_CAPACITY
      REAL ::  BUYER_CAPACITY
      REAL ::  I_CAPACITY
      REAL ::  J_CAPACITY
      REAL ::  TEMP_R
      REAL ::  MaxTransferValue
      REAL ::  HOUR_TIE_LIMIT
      REAL ::  R_RESERVE
      REAL ::  TG_PLANNING_RESERVE_MARGIN(:)
      REAL ::  PA_PEAK_AFTER_INTERRUPTIBLE(:)
      REAL ::  CM_PEAK_AFTER_INTERRUPTIBLE(:)
      REAL ::  EXISTING_PLANNING_CAPACITY(0:6)
      REAL ::  NEW_PLANNING_CAPACITY(0:6)
      REAL ::  AFTER_PEAK_CAPACITY
      REAL ::  RETIREMENT_CAPACITY
      REAL ::  GET_ANNUAL_PEAK
      REAL ::  ENRG_LIMITED_COST
      REAL ::  PA_PLANNING_RESERVE_MARGIN(:)
      REAL ::  CM_PLANNING_RESERVE_MARGIN(:)
      REAL ::  PLANNING_RM_BEFORE_ADDITIONS
      REAL ::  PLANNING_PEAK_BEFORE_ADDITIONS
      REAL ::  GET_CL_TG_CAP
      REAL ::  GET_GROUP_PEAK_ON_PEAK_MONTH
      REAL ::  GET_EL_TG_CAP
      REAL ::  GET_CL_AFTER_PEAK
      REAL ::  GET_CL_TG_RETIRE
      REAL ::  GET_PEAK_ON_PA_PEAK_MONTH
      REAL ::  ICAP_DEMAND_CURVE_MULT_K
      REAL ::  EAS_PROFIT_PER_KWYR_K
      REAL ::  CONE_COST_PER_KWYR_K
      REAL ::  CONE_DEM_CURVE_ADJ_PER_KWYR_K
      REAL ::  ICAP_REVENUE_PER_KWYR_K
      REAL ::  CAPACITY_REVENUE_MM_K
      REAL ::  IOTA=0.0
      REAL ::  Y_BEFORE_SELLER
      REAL ::  X_BEFORE_SELLER
      REAL ::  Y_BEFORE_BUYER
      REAL ::  X_BEFORE_BUYER
      REAL ::  X_AFTER_SELLER
      REAL ::  Y_AFTER_SELLER
      REAL ::  X_AFTER_BUYER
      REAL ::  Y_AFTER_BUYER
      REAL ::  B_BEFORE
      REAL ::  A_BEFORE
      REAL ::  B_AFTER
      REAL ::  A_AFTER
      REAL ::  X_FINAL
      REAL ::  Y_FINAL
      REAL ::  X_SEARCH
      REAL ::  HALF
      ALLOCATABLE :: PA_PLANNING_RESERVE_MARGIN, &
                     CM_PLANNING_RESERVE_MARGIN, &
                     PA_PEAK_AFTER_INTERRUPTIBLE, &
                     PEAK_AFTER_INTERRUPTIBLE_TG, &
                     TG_PLANNING_RESERVE_MARGIN, &
                     CM_PEAK_AFTER_INTERRUPTIBLE, &
                     PA_PLANNING_CAPACITY, &
                     CM_PLANNING_CAPACITY, &
                     CM_IOTA, &
                     IOTA_BY_CN, &
                     TG_PLANNING_CAPACITY, &
                     TOTAL_ANNUAL_CAPACITY_ADDED, &
                     CM_TOT_ANN_CAPACITY_ADDED, &
                     TG_TOTAL_ANNUAL_CAPACITY_ADDED, &
                     MAXIMUM_ANNUAL_MRX_CAP, &
                     PA_ICAP_REVENUE_MULT, &
                     CM_ICAP_REVENUE_MULT, &
                     MINIMUM_ANNUAL_MRX_CAP, &
                     MINIMUM_TG_MRX_CAP, &
                     MAXIMUM_TG_MRX_CAP, &
                     CO2_EMISSIONS_CAP, &
                     CO2_EMISS_REDUCTION_REQUIRED, &
                     CO2_EMISSIONS, &
                     TG_2_PLANNING_AREA, &
                     TG_2_CAPACITY_MARKET, &
                     CURRENT_TARGET_RATIO_PA, &
                     MAXIMUM_RM_PA
      REAL :: &
            INTERRUPTIBLE_LOAD, &
            GET_ANNUAL_INTER_CAPACITY, &
            ZERO,NEAR_ZERO, &
            CAPACITY_PLANNING_PEAK, &
            TEMP_PEAK, &
            CO2_RETIREMENT_PRICE, &
            GET_CO2_RETIREMENT_PRICE, &
            TEMP_R4, &
            BASE_CAP_REV_MM, &
            MAX_CAP_REV_MM, &
            MAX_RPS_REV_MM, &
            ENERGY_UP_LIFT_MM
      PARAMETER (ZERO=0.,NEAR_ZERO=.0000001)
      SAVE PA_PLANNING_RESERVE_MARGIN,CM_PLANNING_RESERVE_MARGIN
      REAL (kind=8) ::  CO2_COEFF
      REAL (kind=8) ::  CO2_POWER
      REAL (kind=8) ::  CO2_PERCENT_OF_CAP
      INTEGER (KIND=2) ::  MAX_ID_RESOURCE_OPTIONS
      INTEGER (KIND=2) ::  OPTIONS_MAX_RESOURCE_ID
      INTEGER (KIND=2) , ALLOCATABLE :: ResourceIDtoOptionPosPrt(:)
      INTEGER (KIND=4) :: MaxProductionProinter
!
! END DATA DECLARATIONS
!

! **********************************************************************
      ENTRY INIT_SCREENING_OBJECT
! **********************************************************************
!
         CALL FUEL_PRICE_FILES_ACTIVE(FUEL_PRICE_DATA_AVAILABLE)
!
         LAST_SORTED_POSITION = 0
!
         UPPER_TRANS_GROUP = GET_NUMBER_OF_ACTIVE_GROUPS()
         ! TODO: Individually deallocate each array.  Following the
         ! deallocation actions, individually allocate each array
         ! and call check_alloc after the allocation. cap_objt:ra4
         IF(ALLOCATED(LAST_RESOURCE_ADDED)) &
                              DEALLOCATE(LAST_RESOURCE_ADDED, &
                                         LAST_PA_RESOURCE_ADDED, &
                                         LAST_CM_RESOURCE_ADDED, &
                                         FIRST_CT_EAS, &
                                         FIRST_CT_4_EAS_INDEX, &
                                         LOWEST_FIXED_COST_PA, &
                                         LOWEST_FIXED_COST_BY_PA, &
                                         LOWEST_FIXED_COST_BY_CM, &
                                         LOWEST_CONE_COST_BY_CM, &
                                         EAS_REVENUE_OFFSET_BY_CM, &
                                         MARGINAL_ICAP_PER_KWYR_BY_TG, &
                                       INSTALL_CAP_VALUE_LAST_PA_UNIT, &
                                         INSTALL_CAP_VALUE_LAST_CM_UNIT)
         ! TODO: Individually allocate each array
         ! and call check_alloc after the allocation. cap_objt:ra5
        ALLOCATE(LAST_RESOURCE_ADDED(MAX(INT(1,2),UPPER_TRANS_GROUP)), &
              LAST_PA_RESOURCE_ADDED(MAX(INT(1,2),UPPER_TRANS_GROUP)), &
              LAST_CM_RESOURCE_ADDED(MAX(INT(1,2),UPPER_TRANS_GROUP)), &
                 FIRST_CT_EAS(MAX(INT(1,2),UPPER_TRANS_GROUP)), &
                FIRST_CT_4_EAS_INDEX(MAX(INT(1,2),UPPER_TRANS_GROUP)), &
                LOWEST_FIXED_COST_PA(MAX(INT(1,2),UPPER_TRANS_GROUP)), &
             LOWEST_FIXED_COST_BY_PA(MAX(INT(1,2),UPPER_TRANS_GROUP)), &
             LOWEST_FIXED_COST_BY_CM(MAX(INT(1,2),UPPER_TRANS_GROUP)), &
              LOWEST_CONE_COST_BY_CM(MAX(INT(1,2),UPPER_TRANS_GROUP)), &
            EAS_REVENUE_OFFSET_BY_CM(MAX(INT(1,2),UPPER_TRANS_GROUP)), &
        MARGINAL_ICAP_PER_KWYR_BY_TG(MAX(INT(1,2),UPPER_TRANS_GROUP)), &
      INSTALL_CAP_VALUE_LAST_PA_UNIT(MAX(INT(1,2),UPPER_TRANS_GROUP)), &
        INSTALL_CAP_VALUE_LAST_CM_UNIT(MAX(INT(1,2),UPPER_TRANS_GROUP)))
         LAST_RESOURCE_ADDED = 0
         LAST_CM_RESOURCE_ADDED = 0
         FIRST_CT_EAS = 0.0
         FIRST_CT_4_EAS_INDEX = 0
         INSTALL_CAP_VALUE_LAST_PA_UNIT = 0.0
         INSTALL_CAP_VALUE_LAST_CM_UNIT = 0.0
!
         TOTAL_ALL_OPTIONS=INITIALIZE_SCREEN_DATA(MaxProductionProinter)
         MAX_ID_RESOURCE_OPTIONS =  OPTIONS_MAX_RESOURCE_ID() ! INTEGER
         IF(ALLOCATED(OPTION_NAME)) &
            DEALLOCATE(OPTION_NAME,OPTION_POSITION, &
                                            ResourceIDtoOptionPosPrt)
         IF(TOTAL_ALL_OPTIONS > 0) THEN
            ALLOCATE(OPTION_NAME(TOTAL_ALL_OPTIONS), &
                                     OPTION_POSITION(TOTAL_ALL_OPTIONS))
            OPTION_POSITION = -1
            CAPTURED_OPTIONS_NAMES = GET_OPTION_NAMES(OPTION_NAME, &
                                      OPTION_POSITION,TOTAL_ALL_OPTIONS)
            ALLOCATE(ResourceIDtoOptionPosPrt(MaxProductionProinter))
         ENDIF
         ANNUAL_MARKET_OPTION_SWITCH = .FALSE.
      RETURN

! **********************************************************************
      ENTRY ANNUAL_SCREENING(R_YEAR)
! **********************************************************************
         IF(TOTAL_ALL_OPTIONS < 1) RETURN
         ALLOCATE(SCREEN_SLOPE(TOTAL_ALL_OPTIONS), &
                  SCREEN_INTERCEPT(TOTAL_ALL_OPTIONS))
         SCREEN_SLOPE = 9999.
         SCREEN_INTERCEPT = 9999.
!
         IF(SCREENING_NOT_OPEN) THEN
            SCREENING_NOT_OPEN = .FALSE.
            SCREENING_NO = SCREENING_HEADER(SCREENING_REC)
         ENDIF
!
         DO J = 1, MAX_SCREENING_INTERVALS
            BEST_SLOPE(J) = 9999.
            BEST_INTERCEPT(J) = 9999.
            BEST_VALUE(J) = 99999.
            BEST_RESOURCE(J) = 0
         ENDDO
         INTERVAL_MULTIPLIER = &
             1./FLOAT(MAX(MAX_SCREENING_INTERVALS,INT(2,2))-INT(1,2))
         THIS_YEAR = R_YEAR + BASE_YEAR
!
         VOID_INT2 = UPDATE_CL_SCREEN_COSTS(R_YEAR)
!
         ignored_result=UPDATE_SCREEN_CAP_COSTS()

         TEMP_L = UPDATE_TRANS_FIXED_COSTS(R_YEAR)
!
         DO I = 1, TOTAL_ALL_OPTIONS
            IF( .NOT. RESOURCE_AVAILABLE(I,THIS_YEAR)) CYCLE
            IF( .NOT. &
                 CALC_SCREEN_SLOPE_INTERCEPT(I,R_YEAR, &
                                             R_SLOPE,R_INTERCEPT)) CYCLE
            SCREEN_SLOPE(I) = R_SLOPE
            SCREEN_INTERCEPT(I) = R_INTERCEPT
!
            DO J = 1, MAX_SCREENING_INTERVALS
!
               SCREEN_VALUE(J) = SCREEN_INTERCEPT(I) + &
                            FLOAT(J)*INTERVAL_MULTIPLIER*SCREEN_SLOPE(I)
!
               IF(SCREEN_VALUE(J) >= BEST_VALUE(J)) CYCLE
!
               BEST_VALUE(J) = SCREEN_VALUE(J)
               BEST_SLOPE(J) = SCREEN_SLOPE(I)
               BEST_INTERCEPT(J) = SCREEN_INTERCEPT(I)
               BEST_RESOURCE(J) = I
!
            ENDDO ! SCREENING_INTERVALS
!
            WRITE(SCREENING_NO,REC=SCREENING_REC) &
                     PRT_ENDPOINT(), &
                     FLOAT(YEAR+BASE_YEAR), &
                     OPTION_NAME(I), &
                     SCREEN_VALUE
            SCREENING_REC = SCREENING_REC + 1
!
         ENDDO ! OPTIONS
         WRITE(SCREENING_NO,REC=SCREENING_REC) &
                     PRT_ENDPOINT(), &
                     FLOAT(YEAR+BASE_YEAR), &
                     'Least Cost Resource ', &
                     BEST_VALUE
         SCREENING_REC = SCREENING_REC + 1
! MAKE CALL TO SCREENING REPORT
         IF(ALLOCATED(SCREEN_SLOPE)) &
                               DEALLOCATE(SCREEN_SLOPE,SCREEN_INTERCEPT)
      RETURN ! ANNUAL_SCREENING

! **********************************************************************
      ENTRY ANNUAL_MARKET_OPTION_COST(R_YEAR)
! **********************************************************************
!
! THIS ROUTINE NEEDS TO BE ADDED AFTER PRICES HAVE BEEN CALCULATED
! AND BEFORE THE NEXT YEAR'S SIMULATION.
!
! THIS PROGRAM IS COPIED HEAVILY FROM ANNUAL_SCREENING ABOVE.
! THE PURPOSE OF THIS PROGRAM IS TO CACULATE THE ANNUAL COSTS OF EACH
! OPTION FOR CAPACITY FACTORS FROM 0% TO 100%.
!
! THE TOTAL COST AT EACH CAPACITY FACTOR IS TOTAL FIXED (INTERCEPT)
! PLUS THE SLOPE (FUEL + VOM) AT EACH CAPACITY FACTOR POINT.
!
! CALLS FOR THE FIXED, VARIABLE AND CAPACITY ARE MADE TO CLA_OBJT.FOR.
! THERE ARE A NUMBER OF WAYS THAT THE CODE NEEDS TO BE MADE MORE ROBUST:
!     - FUEL PRICES SHOULD BE CALLABLE FROM THE FUEL PRICE FILE
!     - COSTS OF THE RESOURCE SHOULD BE CALCULATED MONTHLY, ESP.
!       FOR GAS UNITS
!     - CAPACITIES NEED TO CHANGE MONTH/ANNUAL
!
! ALSO, TVA WANTS TO BE ABLE TO RETIRE AS WELL AS ADD RESOURCES.
!
!
!
         IF(R_YEAR == 6) THEN
           I = I
         ENDIF
!
         MX_SUMMARY_REPORT = EXPANSION_REPORT()
         GRX_8761_REPORT = GRX_8761_REPORT_ACTIVE()
         RUN_PRICE_MODE = YES_STRICT_MARKET_PRICE()
         RUN_AREA_PRICE_MODE = YES_MULTI_AREA_PRICE() ! ADDED 07/30/03.
         RUN_MULTIAREA = YES_RUN_MULTIAREA_TRANSACT()
         INCLUDE_ICAP_REVENUE = YES_INCLUDE_ICAP_REVENUE()
         GRX_RPS_MODULE_ACTIVE = GRX_RPS_MODULE_IS_ACTIVE()
         RPS_ONLY_SWITCH_ACTIVE = RPS_ONLY_SWITCH()
         RPS_PROGRAMS = NUMBER_OF_RPS_PROGRAMS()
!
         IF(.NOT. GRX_WITH_MRX_PLANNING() .AND. .FALSE.) THEN
            TEMP_L = ADJUST_CO2_RETRO_PLAN_CAP(R_YEAR)
         ENDIF
!
         CO2_RETIREMENTS_LOGIC = GET_CO2_RETIREMENTS_LOGIC()
!
         PEAK =  UPDATE_NET_PLANNING_PEAK(R_YEAR)
         ANN_CAP =  EL_PLANNING_CAPACITY(3,R_YEAR) + &
                    CL_PLANNING_CAPACITY(3,R_YEAR) + &
                    CT_PLANNING_CAPACITY(3,R_YEAR) + &
                    LM_PLANNING_CAPACITY(R_YEAR)   + &
                    ADJUSTMENT_CAPACITY(R_YEAR)    + &
                    DERIV_CAPACITY_PLANNING(R_YEAR)
!
         IF(CAPACITY_PLANNING_METHOD() == 'MX'  .AND. &
                                        GREEN_MRX_METHOD() == 'GX') THEN
            IF(GRX_ITERATIONS == 0) THEN
               VOID_INT2 = UPDATE_CL_SCREEN_COSTS(R_YEAR)

            ignored_result = UPDATE_SCREEN_CAP_COSTS()
               TEMP_L = UPDATE_TRANS_FIXED_COSTS(R_YEAR)
            ENDIF
         ELSE
            VOID_INT2 = UPDATE_CL_SCREEN_COSTS(R_YEAR)

            ignored_result = UPDATE_SCREEN_CAP_COSTS()
            TEMP_L = UPDATE_TRANS_FIXED_COSTS(R_YEAR)
         ENDIF
!
         IF(TOTAL_ALL_OPTIONS < 1 .OR. &
                      .NOT. (RUN_MULTIAREA .OR. RUN_PRICE_MODE .OR. &
                                           RUN_AREA_PRICE_MODE) ) RETURN
         HIGHEST_INTERVAL = MAX_SCREENING_INTERVALS
         ! TODO: Individually check and deallocate each array.  Following the
         ! deallocation actions, individually allocate each array
         ! and call check_alloc after the allocation. cap_objt:ra6
         IF(ALLOCATED(SCREEN_SLOPE)) &
            DEALLOCATE( &
                  SCREEN_SLOPE, &
                  SCREEN_INTERCEPT, &
                  SCREEN_CAPACITY, &
                  ANNUAL_ENERGY_REVENUE, &
                  EQUIVALENT_CAPACITY, &
                  BLOCK_CAPACITY, &
                  BLOCK_HEAT_RATE, &
                  ANNUAL_FIXED_COST, &
                  STRIKE_PRICE, &
                  TRANSACTION_GROUP, &
                  STATE_PROVINCE, &
                  LOCAL_ANNUAL_UNITS, &
                  RFT, &
                  ACTIVE_OPTION, &
                  FIRST_CN, &
                  ACTIVE_INSTANCE, &
                  MIN_UP_TIME, &
                  MIN_DOWN_TIME, &
                  RPS_PROGRAM_NUMBER, &
                  START_UP_COSTS, &
                  MUST_RUN_UNIT, &
                  RPM, &
                  HIGHEST_RFT, &
                  HIGHEST_RPM, &
                  FILE_SOURCE_INDEX, &
                  LOCAL_LEAD_TIME, &
                  ANNUAL_LEVELIZED_CAPITAL, &
                  OP_LIFE, &
                  ICAP_SWITCH_INDEX, &
                  ICAP_CONE_PRICE, &
                  HIGHEST_ANN_LEVEL_CAP, &
                  ANNUAL_FUEL_COST, &
                  HIGHEST_FUEL_COST, &
                  FUEL_COST_PER_MWH, &
                  MONTHLY_CAP_MULT, &
                  AVERAGE_FUEL_PRICE, &
                  VAR_OM_PER_MWH, &
                  FUEL_COST_PER_MMBTU, &
                  REVENUE_PER_MWH, &
                  CAPACITY_REVENUE_PER_MWH, &
                  CAPACITY_REVENUE_MM, &
                  RPS_REVENUE_MM, &
                  MRX_RPS_MX_UNIT_CLEARED, &
                  RPS_REVENUE_PER_MWH, &
                  ENERGY_REVENUE_MM, &
                  ENERGY_REVENUE_PER_MWH, &
                  HIGHEST_ENERGY_REV_PER_MWH, &
                  HIGHEST_STRIKE_PRICE, &
                  HIGHEST_MRX_MARGIN, &
                  HIGHEST_MRX_CF, &
                  HIGHEST_MARKET_COST, &
                  HIGHEST_MRX_ENERGY, &
                  EMISSIONS_PER_MWH, &
                  FOM_PER_MWH, &
                  LEVELIZED_CAP_PER_MWH, &
                  FIXED_COST_PER_MWH, &
                  MRX_ICAP_INDEX, &
                  LOCAL_CHRONO_MARKET_PRICES, &
                  RESET_CHRONO_MARKET_PRICES, &
                  VOM_COST_MM, &
                  EMISSIONS_COST_MM, &
                  FOM_COST_MM, &
                  IOTA_BY_CN, &
                  TRANS_GROUP_ID, &
                  TG_CAPACITY_MW, &
                  TG_PEAK_MW, &
                  FirmTransfer, &
                  CM_Net_Transfer, &
                  TG_Net_Transfer, &
                  PA_Net_Transfer, &
                  NUMBER_DIRECT_INTERCONNECTS, &
                  DIRECT_INTERCONNECT_TG, &
                  HIGHEST_TG_ICAP_REV_PER_KWYR, &
                  HIGHEST_OPTION_FOR_TG, &
                  TransmissionConstraint, &
                  MaxTransmissionConstraint, &
                  TransferValue, &
                  TG_RESERVE_MARGIN, &
                  PLANNING_AREA_ID, &
                  PA_CAPACITY_MW, &
                  CM_CAPACITY_MW, &
                  PA_PEAK_MW, &
                  CM_PEAK_MW, &
                  PA_RESERVE_MARGIN, &
                  CM_RESERVE_MARGIN, &
                  CM_MAX_NET_ENRG_MARG_PER_KWYR, &
                  ICAP_DEMAND_CURVE_MULT, &
                  HIGHEST_CF, &
                  CONE_COST_PER_KWYR, &
                  CONE_DEMAND_CURVE_ADJ_PER_KWYR, &
                  EAS_PROFIT_PER_KWYR, &
                  NET_MARGIN_PER_KWYR, &
                  ICAP_REVENUE_PER_KWYR, &
                  HEAT_RATE, &
                  HIGHEST_HEAT_RATE, &
                  HIGHEST_ANNUAL_UNITS, &
                  HIGHEST_MARKET_REVENUE, &
                  HIGHEST_MARGIN_PER_KWYR, &
                  HIGHEST_FUEL_COST_PER_MWH, &
                  HIGHEST_VAR_OM_PER_MWH, &
                  HIGHEST_FUEL_COST_PER_MMBTU, &
                  HIGHEST_EMISSIONS_PER_MWH, &
                  HIGHEST_FOM_PER_MWH, &
                  HIGHEST_LEVEL_CAP_PER_MWH, &
                  HIGHEST_VOM_COST_MM, &
                  HIGHEST_EMISSIONS_COST_MM, &
                  HIGHEST_FOM_COST_MM, &
                  FIX_OM_COST, &
                  CAPACITY_PLANNING_MW, &
                  RPS_PERCENT, &
                  FIRST_CF, &
                  ANNUAL_VARIABLE_COST, &
                  VARIABLE_COST_MM, &
                  FIXED_COST_MM, &
                  HIGHEST_SCREEN_CAPACITY, &
                  MARKET_REVENUE, &
                  HIGHEST_ENERGY, &
                  MARKET_COST, &
                  NET_MARGIN, &
                  NET_MARGIN_INTERVAL, &
                  THIS_YEAR_PLUS_LEAD_TIME, &
                  MARKET_CUM_REVENUE, &
                  MARKET_DURATION, &
                  MARKET_PRICE)
         ALLOCATE(SCREEN_SLOPE(TOTAL_ALL_OPTIONS), &
                  SCREEN_INTERCEPT(TOTAL_ALL_OPTIONS), &
                  SCREEN_CAPACITY(TOTAL_ALL_OPTIONS), &
                  ANNUAL_ENERGY_REVENUE(TOTAL_ALL_OPTIONS), &
                  EQUIVALENT_CAPACITY(TOTAL_ALL_OPTIONS), &
                  BLOCK_CAPACITY(5,TOTAL_ALL_OPTIONS), &
                  BLOCK_HEAT_RATE(5,TOTAL_ALL_OPTIONS), &
                  ANNUAL_FIXED_COST(TOTAL_ALL_OPTIONS), &
                  STRIKE_PRICE(TOTAL_ALL_OPTIONS,13), &
                  TRANSACTION_GROUP(TOTAL_ALL_OPTIONS), &
                  STATE_PROVINCE(TOTAL_ALL_OPTIONS), &
                  LOCAL_ANNUAL_UNITS(TOTAL_ALL_OPTIONS), &
                  RFT(TOTAL_ALL_OPTIONS), &
                  ACTIVE_OPTION(TOTAL_ALL_OPTIONS), &
                  FIRST_CN(TOTAL_ALL_OPTIONS), &
                  ACTIVE_INSTANCE(MX_RES_P_YEAR), &
                  MIN_UP_TIME(TOTAL_ALL_OPTIONS), &
                  MIN_DOWN_TIME(TOTAL_ALL_OPTIONS), &
                  RPS_PROGRAM_NUMBER(TOTAL_ALL_OPTIONS), &
                  START_UP_COSTS(12,TOTAL_ALL_OPTIONS), &
                  MUST_RUN_UNIT(12,TOTAL_ALL_OPTIONS), &
                  RPM(TOTAL_ALL_OPTIONS), &
                  LOCAL_LEAD_TIME(TOTAL_ALL_OPTIONS), &
                  FILE_SOURCE_INDEX(TOTAL_ALL_OPTIONS), &
                  ANNUAL_LEVELIZED_CAPITAL(TOTAL_ALL_OPTIONS), &
                  OP_LIFE(TOTAL_ALL_OPTIONS), &
                  ICAP_SWITCH_INDEX(0:UPPER_TRANS_GROUP), &
                  ICAP_CONE_PRICE(0:UPPER_TRANS_GROUP), &
                  HIGHEST_ANN_LEVEL_CAP(MX_RES_P_YEAR), &
                  ANNUAL_FUEL_COST(TOTAL_ALL_OPTIONS), &
                  HIGHEST_FUEL_COST(MX_RES_P_YEAR), &
                  FUEL_COST_PER_MWH(12,TOTAL_ALL_OPTIONS), &
                  MONTHLY_CAP_MULT(12,TOTAL_ALL_OPTIONS), &
                  AVERAGE_FUEL_PRICE(TOTAL_ALL_OPTIONS), &
                  VAR_OM_PER_MWH(TOTAL_ALL_OPTIONS), &
                  FUEL_COST_PER_MMBTU(TOTAL_ALL_OPTIONS), &
                  REVENUE_PER_MWH(MX_RES_P_YEAR), &
                  CAPACITY_REVENUE_PER_MWH(MX_RES_P_YEAR), &
                  CAPACITY_REVENUE_MM(MX_RES_P_YEAR), &
                  RPS_REVENUE_MM(MX_RES_P_YEAR), &
                  MRX_RPS_MX_UNIT_CLEARED(MX_RES_P_YEAR), &
                  RPS_REVENUE_PER_MWH(MX_RES_P_YEAR), &
                  ENERGY_REVENUE_MM(MX_RES_P_YEAR), &
                  ENERGY_REVENUE_PER_MWH(TOTAL_ALL_OPTIONS), &
                  HIGHEST_ENERGY_REV_PER_MWH(MX_RES_P_YEAR), &
                  HIGHEST_STRIKE_PRICE(MX_RES_P_YEAR), &
                  HIGHEST_MRX_MARGIN(MX_RES_P_YEAR), &
                  HIGHEST_MRX_CF(MX_RES_P_YEAR), &
                  HIGHEST_MARKET_COST(MX_RES_P_YEAR), &
                  HIGHEST_MRX_ENERGY(MX_RES_P_YEAR), &
                  HIGHEST_RFT(MX_RES_P_YEAR), &
                  HIGHEST_RPM(MX_RES_P_YEAR), &
                  EMISSIONS_PER_MWH(TOTAL_ALL_OPTIONS), &
                  FOM_PER_MWH(TOTAL_ALL_OPTIONS), &
                  LEVELIZED_CAP_PER_MWH(TOTAL_ALL_OPTIONS), &
                  FIXED_COST_PER_MWH(MX_RES_P_YEAR), &
                  MRX_ICAP_INDEX(MX_RES_P_YEAR), &
                  LOCAL_CHRONO_MARKET_PRICES(8784,UPPER_TRANS_GROUP), &
                  RESET_CHRONO_MARKET_PRICES(8784,UPPER_TRANS_GROUP), &
                  VOM_COST_MM(TOTAL_ALL_OPTIONS), &
                  EMISSIONS_COST_MM(TOTAL_ALL_OPTIONS), &
                  FOM_COST_MM(TOTAL_ALL_OPTIONS), &
                  IOTA_BY_CN(TOTAL_ALL_OPTIONS), &
                  TRANS_GROUP_ID(TOTAL_ALL_OPTIONS), &
                  TG_CAPACITY_MW(0:UPPER_TRANS_GROUP), &
                  TG_PEAK_MW(0:UPPER_TRANS_GROUP), &
                FirmTransfer(0:UPPER_TRANS_GROUP,0:UPPER_TRANS_GROUP), &
                  CM_Net_Transfer(0:UPPER_TRANS_GROUP), &
                  TG_Net_Transfer(0:UPPER_TRANS_GROUP), &
                  PA_Net_Transfer(0:UPPER_TRANS_GROUP), &
                  NUMBER_DIRECT_INTERCONNECTS(0:UPPER_TRANS_GROUP), &
                  DIRECT_INTERCONNECT_TG(0:UPPER_TRANS_GROUP, &
                                               0:UPPER_TRANS_GROUP), &
                  HIGHEST_TG_ICAP_REV_PER_KWYR(0:UPPER_TRANS_GROUP), &
                  HIGHEST_OPTION_FOR_TG(0:UPPER_TRANS_GROUP), &
                  TransmissionConstraint(0:UPPER_TRANS_GROUP, &
                                                 0:UPPER_TRANS_GROUP), &
                  MaxTransmissionConstraint(0:UPPER_TRANS_GROUP, &
                                                 0:UPPER_TRANS_GROUP), &
                  TransferValue(0:UPPER_TRANS_GROUP, &
                                                 0:UPPER_TRANS_GROUP), &
                  TG_RESERVE_MARGIN(0:UPPER_TRANS_GROUP), &
                  PLANNING_AREA_ID(TOTAL_ALL_OPTIONS), &
                  PA_CAPACITY_MW(0:UPPER_TRANS_GROUP), &
                  CM_CAPACITY_MW(0:UPPER_TRANS_GROUP), &
                  PA_PEAK_MW(0:UPPER_TRANS_GROUP), &
                  CM_PEAK_MW(0:UPPER_TRANS_GROUP), &
                  PA_RESERVE_MARGIN(0:UPPER_TRANS_GROUP), &
                  CM_RESERVE_MARGIN(0:UPPER_TRANS_GROUP), &
                  CM_MAX_NET_ENRG_MARG_PER_KWYR(0:UPPER_TRANS_GROUP), &
                  ICAP_DEMAND_CURVE_MULT(MX_RES_P_YEAR), &
                  HIGHEST_CF(TOTAL_ALL_OPTIONS), &
                  CONE_COST_PER_KWYR(MX_RES_P_YEAR), &
                  CONE_DEMAND_CURVE_ADJ_PER_KWYR( &
                                                MX_RES_P_YEAR), &
                  EAS_PROFIT_PER_KWYR(MX_RES_P_YEAR), &
                  NET_MARGIN_PER_KWYR(TOTAL_ALL_OPTIONS), &
                  ICAP_REVENUE_PER_KWYR(MX_RES_P_YEAR), &
                  HEAT_RATE(TOTAL_ALL_OPTIONS), &
                  HIGHEST_HEAT_RATE(MX_RES_P_YEAR), &
                  HIGHEST_ANNUAL_UNITS(MX_RES_P_YEAR), &
                  HIGHEST_MARKET_REVENUE(MX_RES_P_YEAR), &
                  HIGHEST_MARGIN_PER_KWYR(MX_RES_P_YEAR), &
                  HIGHEST_FUEL_COST_PER_MWH(MX_RES_P_YEAR), &
                  HIGHEST_VAR_OM_PER_MWH(MX_RES_P_YEAR), &
                  HIGHEST_FUEL_COST_PER_MMBTU(MX_RES_P_YEAR), &
                  HIGHEST_EMISSIONS_PER_MWH(MX_RES_P_YEAR), &
                  HIGHEST_FOM_PER_MWH(MX_RES_P_YEAR), &
                  HIGHEST_LEVEL_CAP_PER_MWH(MX_RES_P_YEAR), &
                  HIGHEST_VOM_COST_MM(MX_RES_P_YEAR), &
                  HIGHEST_EMISSIONS_COST_MM(MX_RES_P_YEAR), &
                  HIGHEST_FOM_COST_MM(MX_RES_P_YEAR), &
                  FIX_OM_COST(TOTAL_ALL_OPTIONS), &
                  CAPACITY_PLANNING_MW(TOTAL_ALL_OPTIONS), &
                  RPS_PERCENT(TOTAL_ALL_OPTIONS), &
                  FIRST_CF(TOTAL_ALL_OPTIONS), &
                  ANNUAL_VARIABLE_COST(TOTAL_ALL_OPTIONS), &
                  VARIABLE_COST_MM(MX_RES_P_YEAR), &
                  FIXED_COST_MM(MX_RES_P_YEAR), &
                  HIGHEST_SCREEN_CAPACITY(MX_RES_P_YEAR), &
                  MARKET_REVENUE(TOTAL_ALL_OPTIONS), &
                  HIGHEST_ENERGY(-1:TOTAL_ALL_OPTIONS), &
                  MARKET_COST(TOTAL_ALL_OPTIONS), &
                  NET_MARGIN(TOTAL_ALL_OPTIONS), &
                  NET_MARGIN_INTERVAL(TOTAL_ALL_OPTIONS), &
                  THIS_YEAR_PLUS_LEAD_TIME(TOTAL_ALL_OPTIONS), &
                  MARKET_CUM_REVENUE(0:HIGHEST_INTERVAL), &
                  MARKET_DURATION(0:HIGHEST_INTERVAL), &
                  MARKET_PRICE(0:HIGHEST_INTERVAL))
         TG_Net_Transfer = 0.0 ! 091111 MOVED UP.
         SCREEN_SLOPE = 9999.
         SCREEN_INTERCEPT = 9999.
         SCREEN_CAPACITY = 0.
         EQUIVALENT_CAPACITY = 0.
         BLOCK_CAPACITY = 0.
         BLOCK_HEAT_RATE = 0.
         ANNUAL_FIXED_COST = 0.
         ANNUAL_VARIABLE_COST = 0.
         VARIABLE_COST_MM = 0.
         FIXED_COST_MM = 0.
         HIGHEST_SCREEN_CAPACITY = 0.
         STRIKE_PRICE = 9999.
         TRANSACTION_GROUP = 0
         STATE_PROVINCE = 0
         LOCAL_ANNUAL_UNITS = 9999
         RFT = 0.0
         START_UP_COSTS = 0.0
         MUST_RUN_UNIT = .FALSE.
         MIN_UP_TIME = 0
         MIN_DOWN_TIME = 0
         RPS_PROGRAM_NUMBER = 0
         ACTIVE_OPTION = 0
         FIRST_CN = 0
         ACTIVE_INSTANCE = 0
         RPM = 0.0
         HIGHEST_RFT = 0.0
         HIGHEST_RPM = 0.0
         LOCAL_LEAD_TIME = 0
         FILE_SOURCE_INDEX = 0
         ANNUAL_LEVELIZED_CAPITAL = 0.
         OP_LIFE = 99
         ICAP_SWITCH_INDEX = 2
         ICAP_CONE_PRICE = 0.0
         HIGHEST_ANN_LEVEL_CAP = 0.
         ANNUAL_FUEL_COST = 0.
         HIGHEST_FUEL_COST = 0.
         FUEL_COST_PER_MWH = 0.
         MONTHLY_CAP_MULT = 0.
         AVERAGE_FUEL_PRICE = 0.
! 110809. TEST
         LAST_PA_RESOURCE_ADDED = 0
         LAST_CM_RESOURCE_ADDED = 0
         FIRST_CT_EAS = 0.0
         FIRST_CT_4_EAS_INDEX = 0
         LOWEST_FIXED_COST_PA = 0
         LOWEST_FIXED_COST_BY_PA = 999999.0
         LOWEST_FIXED_COST_BY_CM = 999999.0
         LOWEST_CONE_COST_BY_CM = 999999.0
         EAS_REVENUE_OFFSET_BY_CM = 0.0
         MARGINAL_ICAP_PER_KWYR_BY_TG = 999999.0
!
         VAR_OM_PER_MWH = 0.
         FUEL_COST_PER_MMBTU = 0.
         REVENUE_PER_MWH = 0.
         CAPACITY_REVENUE_PER_MWH = 0.
         CAPACITY_REVENUE_MM = 0.
         RPS_REVENUE_MM = 0.0
         RPS_REVENUE_PER_MWH = 0.0
         ENERGY_REVENUE_MM = 0.
         ENERGY_REVENUE_PER_MWH = 0.
         HIGHEST_ENERGY_REV_PER_MWH = 0.
         HIGHEST_STRIKE_PRICE = 0.
         HIGHEST_MRX_MARGIN = 0.
         HIGHEST_MRX_CF = 0.
         HIGHEST_MARKET_COST = 0.
         HIGHEST_FUEL_COST_PER_MWH = 0.
         HIGHEST_MRX_ENERGY = 0.
         EMISSIONS_PER_MWH = 0.
         FOM_PER_MWH = 0.
         LEVELIZED_CAP_PER_MWH = 0.
         FIXED_COST_PER_MWH = 0.
         MRX_ICAP_INDEX = 0.
         VOM_COST_MM = 0.
         EMISSIONS_COST_MM = 0.
         FOM_COST_MM = 0.
         TRANS_GROUP_ID = 0.
         TG_CAPACITY_MW = 0.
         TG_PEAK_MW = 0.
         TG_RESERVE_MARGIN = 0.
         PLANNING_AREA_ID = 0.
         PA_CAPACITY_MW = 0.
         CM_CAPACITY_MW = 0.
         PA_PEAK_MW = 0.
         CM_PEAK_MW = 0.
         PA_RESERVE_MARGIN = 0.
         CM_RESERVE_MARGIN = 0.
         CM_MAX_NET_ENRG_MARG_PER_KWYR = -999999.0
         ICAP_DEMAND_CURVE_MULT = 0.
         HIGHEST_CF = 0.
         CONE_COST_PER_KWYR = 0.
         CONE_DEMAND_CURVE_ADJ_PER_KWYR = 0.
         EAS_PROFIT_PER_KWYR = 0.
         ICAP_REVENUE_PER_KWYR = 0.
         HEAT_RATE = 0.
         HIGHEST_HEAT_RATE = 0.
         HIGHEST_ANNUAL_UNITS = 0.
         HIGHEST_MARKET_REVENUE = 0.
         HIGHEST_MARGIN_PER_KWYR = 0.
         HIGHEST_VAR_OM_PER_MWH = 0.
         HIGHEST_FUEL_COST_PER_MMBTU = 0.
         HIGHEST_EMISSIONS_PER_MWH = 0.
         HIGHEST_FOM_PER_MWH = 0.
         HIGHEST_LEVEL_CAP_PER_MWH = 0.
         HIGHEST_VOM_COST_MM = 0.
         HIGHEST_EMISSIONS_COST_MM = 0.
         HIGHEST_FOM_COST_MM = 0.
         FIX_OM_COST = 0.
         CAPACITY_PLANNING_MW = 0.0
         RPS_PERCENT = 1.0
         THIS_YEAR_PLUS_LEAD_TIME = 0

         CO2_RETIREMENT_PRICE = -9999.
!
         PG = MAX(GET_NUMBER_OF_PLANNING_GROUPS(),INT(1,2))
         CG = MAX(GET_NUMBER_OF_CAPACITY_MARKETS(),INT(1,2))
!
         SCENARIO_CAP_COST_MULT = &
                           GET_SCENARIO_EXPAND_CAP_COST(R_YEAR,INT(1,2))
         SCENARIO_RESERVE_MARGIN = &
                            GET_SCENARIO_RESERVE_MARGIN(R_YEAR,INT(1,2))
!
         MINIMUM_NET_MARGIN = GET_ADDITIONS_PROFIT_PER_MWH()
         MAXIMUM_RM = MAX_CAPACITY_TESTING_MARGIN(R_YEAR)
         VOID_LOGICAL = YES_USE_MINIMUM_RM(USE_MINIMUM_RM)
!
         PLANNING_PER_MWH = .TRUE.
!
         USE_MINIMUM_RM_LOGIC = .FALSE.
         USE_REGIONAL_MINIMUM_RM_LOGIC = .FALSE.
         USE_REGIONAL_MAXIMUM_RM_LOGIC = .FALSE.
         MRX_SORTED_MIX =  USE_MINIMUM_RM == 'X'
         IF(USE_MINIMUM_RM == 'U' .OR. USE_MINIMUM_RM == 'B') THEN
            USE_MINIMUM_RM_LOGIC = .TRUE.
         ELSEIF(USE_MINIMUM_RM == 'R' .OR. USE_MINIMUM_RM == 'X') THEN
            USE_REGIONAL_MINIMUM_RM_LOGIC = .TRUE.
         ENDIF
!
         IF(USE_MINIMUM_RM == 'A' .OR. &
                      USE_MINIMUM_RM == 'B' .OR. &
                                 USE_MINIMUM_RM == 'R' .OR. &
                                             USE_MINIMUM_RM == 'X') THEN
            USE_MAXIMUM_RM_LOGIC = .TRUE.
         ELSE
            USE_MAXIMUM_RM_LOGIC = .FALSE.
         ENDIF
!
         PLANNING_PER_MWH = YES_PLAN_TO_MWH_OR_KWYR()
         REGIONAL_PARAMS_EXIST = YES_REGIONAL_PARAMS_EXIST()
         YES_USE_EMIS_IN_MRX = USE_EMIS_IN_MRX()
         THIS_YEAR = R_YEAR + BASE_YEAR
!
! FIRST, GET FIXED AND VARIABLE COST BY OPTION BY INTERVAL
!
         IF(.NOT. ANNUAL_MARKET_OPTION_SWITCH &
           .and. YES_RUN_TRANSACT()) THEN
               ANNUAL_MARKET_OPTION_SWITCH = .TRUE.
            RETURN
         ENDIF
!
! INITIALIZE RESOURCE EXPANSION OPTIONS IN THIS YEAR.
!
         TEMP_L = CALC_MRX_RPS_CURVES(R_YEAR)
         ANNUAL_RESOURCES_AVAILABLE = .FALSE.
!
         N = 0
         AO = 0
!
         ResourceIDtoOptionPosPrt = 0
         IF(THIS_YEAR >= 2018) then
           i= total_all_options
         endif
         DO CN = 1, TOTAL_ALL_OPTIONS 
         ! INDEX FROM THE CAPACITY OPTIONS FILE
            IF(OPTION_POSITION(CN) == -1) THEN
               TOTAL_ALL_OPTIONS = MAX(1,CN-1)
               WRITE(4,*) 'MISCOUNT IN CAPACITY OPTIONS FILE'
               EXIT
            ENDIF
!
            I = OPTION_POSITION(CN)
            FILE_SOURCE_INDEX(CN) = GET_FILE_SOURCE_INDEX(I)
            THIS_YEAR_PLUS_LEAD_TIME(CN) = &
                                     THIS_YEAR + LEAD_TIME_FOR_OPTION(I)
!
            IF( .NOT. RESOURCE_AVAILABLE(I, &
                                    THIS_YEAR_PLUS_LEAD_TIME(CN)) .OR. &
                  .NOT. TEST_START_STOP_YEARS(I, &
                                    THIS_YEAR_PLUS_LEAD_TIME(CN))) CYCLE
            SCREEN_CAPACITY(CN) = GET_SCREEN_CAPACITY(I)
            OP_LIFE(CN) = RETURN_CN_OP_LIFE(CN,R_YEAR)
            IF(FILE_SOURCE_INDEX(CN) == CapLimited) THEN
               PROD_POINTER = GET_PRODUCTION_DATA_POINTER(I)
               ResourceIDtoOptionPosPrt(PROD_POINTER) = CN
!
               TRANSACTION_GROUP(CN) = &
                                   RETURN_S_TRANS_GROUP_ID(PROD_POINTER)
               STATE_PROVINCE(CN) = GET_SCREEN_STATE_INDEX(PROD_POINTER)
!
               IF(GET_TRANS_GROUP_POSITION(TRANSACTION_GROUP(CN)) <= &
                                                                0) CYCLE
!
               AO = AO + 1
               ACTIVE_OPTION(AO) = CN
               N = CN
!
               FT = GET_S_PRIMARY_MOVER(PROD_POINTER)
               FUEL_SCEN_MULT(5) = 1.0
!
               IF(FT > 4 .OR. FT < 1) FT = 5
!
               RFT(CN) = &
                  FLOAT(MAX(INT(1,2), &
                      MIN(GET_S_PRIMARY_MOVER(PROD_POINTER),INT(11,2))))
               RPM(CN) = FLOAT(GET_S_PRIMARY_MOVER_INDEX(PROD_POINTER))
!
               R_DELIVERY_COST = -999999.
!
               VOID_R4 = PUT_MRX_DELIVERY_COST(R_DELIVERY_COST)
!
               MIN_UP_TIME(CN) = &
                                  INT(GET_S_MIN_UP_TIME(PROD_POINTER),2)
               MIN_DOWN_TIME(CN) = &
                                INT(GET_S_MIN_DOWN_TIME(PROD_POINTER),2)
               RPS_PERCENT(CN) = &
                              GET_S_RPS_PERCENT(PROD_POINTER,R_YEAR)
               RPS_PROGRAM_NUMBER(CN) = &
                                  GET_S_RPS_PROGRAM_NUMBER(PROD_POINTER)
!
! 12/17/04. MAKE FUEL AND VOM VARIABLES WITH A SWITCH.
!
               DO MO= 1, 12
                  START_UP_COSTS(MO,CN) = &
                         GET_UNIT_START_UP_COSTS(R_YEAR,PROD_POINTER,MO)
                  MUST_RUN_UNIT(MO,CN) = &
                                 S_YES_MONTHLY_MUST_RUN(PROD_POINTER,MO)
                  IF(FUEL_PRICE_DATA_AVAILABLE) THEN
                     FUEL_SCEN_MULT(1) = &
                                  GET_SCENARIO_COAL_PRICES(R_YEAR,MO)
                     FUEL_SCEN_MULT(2) = &
                                  GET_SCENARIO_GAS_PRICES(R_YEAR,MO)
                     FUEL_SCEN_MULT(3) = &
                                  GET_SCENARIO_OIL_PRICES(R_YEAR,MO)
                     FUEL_SCEN_MULT(4) = &
                              GET_SCENARIO_URANIUM_PRICES(R_YEAR,MO)
!
                     RTEMP = GET_MONTHLY_FUEL_INDEX(N,MO)
!
! ASSUME NO FUEL INVENTORIES.
! THE TWO CRITICAL ROUTINES.  THEY ARE HOOKED-UP TO EXISTING UNITS.
! THE INDICES IN THESE TWO ROUTINES ARE FOR EXISTING UNITS.
!
                     FUEL_POINTERS_USED = .FALSE.
                     EXISTING_UNIT = .FALSE.
!
                     CALL GET_MRX_DELIVERY_COST(PROD_POINTER, &
                                          R_DELIVERY_COST, &
                                          MO,R_YEAR, &
                                          FUEL_POINTERS_USED, &
                                          EXISTING_UNIT)
!
                     IF(FUEL_POINTERS_USED) THEN
                        VOID_R4 = PUT_MRX_DELIVERY_COST(R_DELIVERY_COST)
                     ENDIF
                  ENDIF
!
                  FUEL_COST_PER_MWH(MO,CN) = &
                       RETURN_CL_SCREEN_FUEL_COST(PROD_POINTER,R_YEAR, &
                                              HEAT_RATE(CN), &
                                            FUEL_COST_PER_MMBTU(CN)) * &
                                              FUEL_SCEN_MULT(FT) * RTEMP

!
! 03/17/05. ADDED FOR PAC AND BURESH
!
                  IF(YES_USE_EMIS_IN_MRX) THEN
                     CALL CALCULATE_NOX_SOX_VOM( &
                              PROD_POINTER, &
                              R_YEAR, &
                              MO, &
                              NOX_VOM, &
                              NOX_FOM, &
                              SOX_VOM, &
                              SOX_FOM, &
                              CO2_VOM, &
                              CO2_FOM, &
                              HG_VOM, &
                              HG_FOM, &
                              OTHER3_VOM, &
                              OTHER3_FOM, &
                              NOX_CONTROL_MULT, &
                              SOX_CONTROL_MULT, &
                              CO2_CONTROL_MULT, &
                              HG_CONTROL_MULT, &
                              OTHER3_CONTROL_MULT, &
                              TRANSACTION_GROUP(CN))
                     EMISSIONS_PER_MWH(CN) = &
                            RETURN_CL_SCREEN_EMIS_COST(PROD_POINTER, &
                                                  R_YEAR,MO, &
                                                  NOX_CONTROL_MULT, &
                                                  SOX_CONTROL_MULT, &
                                                  CO2_CONTROL_MULT, &
                                                  HG_CONTROL_MULT, &
                                                  OTHER3_CONTROL_MULT, &
                                               CO2_RETIREMENT_PRICE) + &
                                           NOX_VOM + SOX_VOM + &
                                           CO2_VOM + HG_VOM + OTHER3_VOM

                  ENDIF

               ENDDO ! MO

               MO = 7 ! FOR NOW.

               VAR_OM_PER_MWH(CN) = &
                                RETURN_OTHER_VAR_COMPONENT(PROD_POINTER)
               EQUIVALENT_CAPACITY(CN) = SCREEN_CAPACITY(CN) * &
                    (1. - RETURN_CL_SCREEN_EFOR(PROD_POINTER,R_YEAR, &
                                             MONTHLY_CAP_MULT(1,CN)))
               TEMP_L = GET_S_BLOCK_CAP_AND_HR(PROD_POINTER, &
                                               BLOCK_CAPACITY(1,CN), &
                                               BLOCK_HEAT_RATE(1,CN), &
                                               R_YEAR)

               FIX_OM_COST(CN) = &
                               RETURN_CL_SCREEN_FIXED_COST(PROD_POINTER)
               CAPACITY_PLANNING_MW(CN) = SCREEN_CAPACITY(CN) * &
                             GET_S_CAP_PLANNING_FAC(PROD_POINTER,R_YEAR)
            ELSEIF(FILE_SOURCE_INDEX(CN) == Derivative) THEN
               PROD_POINTER = GET_GRX_RESOURCE_LINK_ID(I)
               IF(PROD_POINTER < 1) THEN
                  WRITE(4,*) 'BAD GRX RESOURCE ID LINK IN OPTIONS FILE'
                  WRITE(4,*) 'FOR ',OPTION_NAME(I)
                  CYCLE
               ENDIF
               PROD_POINTER = GET_TRANS_FOR_GRX_ID(PROD_POINTER)
               EMISSIONS_PER_MWH(CN) =  0.0 ! COULD ADD EMISSION COST
               IF(PROD_POINTER < 1) THEN
                  WRITE(4,*) &
                          'BAD GRX RESOURCE ID LINK IN DERIVATIVE FILE'
                  WRITE(4,*) 'FOR ',OPTION_NAME(I)
                  WRITE(4,*) 'RESOURCE ID LINK ', &
                                             GET_GRX_RESOURCE_LINK_ID(I)
                  WRITE(4,*) 'TOTAL ALL OPTIONS ',TOTAL_ALL_OPTIONS
                  WRITE(4,*) 'CURRENT COUNT OF OPTIONS',CN
                  WRITE(4,*) 'CURRENT POSITION ',I
                  CYCLE
               ENDIF
               TRANSACTION_GROUP(CN) = &
                                 GET_TRANS_GROUP_FOR_TRANS(PROD_POINTER)
               STATE_PROVINCE(CN) = GET_TRANS_STATE_INDEX(PROD_POINTER)
               IF(TRANSACTION_GROUP(CN) < 1) THEN
                  WRITE(4,*) &
                          'BAD TRANSACTION GROUP IN DERIVATIVE FILE'
                  WRITE(4,*) 'FOR ',OPTION_NAME(I)
                  CYCLE
               ENDIF
               IF(GET_TRANS_GROUP_POSITION(TRANSACTION_GROUP(CN)) <= &
                                                                0) CYCLE
               AO = AO + 1
               ACTIVE_OPTION(AO) = CN
               N = CN
               EQUIVALENT_CAPACITY(CN) = SCREEN_CAPACITY(CN)
!
! VARIABLE COST, FIXED COST, RFT, RPM, EMISSIONS COST IF SET, TG,
! HEAT RATE
!
               FIX_OM_COST(CN) = &
                           CALC_GRX_FIX_COST_PER_UNIT( &
                                       PROD_POINTER, &
                                       R_YEAR, &
                                       1_2, &
                                       EQUIVALENT_CAPACITY(CN), &
                                       ANNUAL_ENERGY)
!
               RFT(CN) = &
                 FLOAT(MAX(INT(1,2), &
                     MIN(GET_DERIV_PRIM_MOVER(PROD_POINTER),INT(11,2))))
               RPM(CN) = FLOAT(GET_DERIV_PRIM_MOVER_INDEX(PROD_POINTER))
               CAPACITY_PLANNING_MW(CN) = SCREEN_CAPACITY(CN) * &
                             GET_TRANS_CAP_PLAN_FAC(PROD_POINTER,R_YEAR)
               RPS_PERCENT(CN) = &
                              GET_TRANS_RPS_PERCENT(PROD_POINTER,R_YEAR)
               RPS_PROGRAM_NUMBER(CN) = &
                                 GET_TRANS_RPS_PROG_NUMBER(PROD_POINTER)
            ELSEIF(FILE_SOURCE_INDEX(CN) == Transmission) THEN
               PROD_POINTER = GET_GRX_RESOURCE_LINK_ID(I)
               IF(PROD_POINTER < 1) THEN
                  WRITE(4,*) 'BAD GRX RESOURCE ID LINK IN OPTIONS FILE'
                  WRITE(4,*) 'FOR ',OPTION_NAME(I)
                  CYCLE
               ENDIF
               PROD_POINTER = GET_TRANSMISSION_FOR_GRX_ID(PROD_POINTER)
               EMISSIONS_PER_MWH(CN) =  0.0 ! COULD ADD EMISSION COST
               IF(PROD_POINTER < 1) THEN
                  WRITE(4,*) &
                   'BAD GRX RESOURCE ID LINK IN TRANSMISSION PATHS FILE'
                  WRITE(4,*) 'FOR ',OPTION_NAME(I)
                  WRITE(4,*) 'RESOURCE ID LINK ', &
                                             GET_GRX_RESOURCE_LINK_ID(I)
                  WRITE(4,*) 'TOTAL ALL OPTIONS ',TOTAL_ALL_OPTIONS
                  WRITE(4,*) 'CURRENT COUNT OF OPTIONS',CN
                  WRITE(4,*) 'CURRENT POSITION ',I
                  CYCLE
               ENDIF
               TRANSACTION_GROUP(CN) = &
                               GET_TG_FOR_TRANSMISSION_BUY(PROD_POINTER)
               STATE_PROVINCE(CN) = 0 ! FOR NOW
               IF(TRANSACTION_GROUP(CN) < 1) THEN
                  WRITE(4,*) &
                          'BAD TRANSACTION GROUP IN DERIVATIVE FILE'
                  WRITE(4,*) 'FOR ',OPTION_NAME(I)
                  CYCLE
               ENDIF
               IF(GET_TRANS_GROUP_POSITION(TRANSACTION_GROUP(CN)) <= &
                                                                0) CYCLE
               AO = AO + 1
               ACTIVE_OPTION(AO) = CN
               N = CN
               EQUIVALENT_CAPACITY(CN) = SCREEN_CAPACITY(CN)
!
! VARIABLE COST, FIXED COST, RFT, RPM, EMISSIONS COST IF SET, TG,
! HEAT RATE
!
               FIX_OM_COST(CN) = 0 ! FOR NOW
               RFT(CN) = 0.0
               RPM(CN) = 0.0
               CAPACITY_PLANNING_MW(CN) = SCREEN_CAPACITY(CN) ! FOR NOW
               RPS_PERCENT(CN) = 0 ! FOR NOW

            ENDIF ! THERMAL, DERIVATIVE OR TRANSMISSION
!
            ANNUAL_LEVELIZED_CAPITAL(CN) = &
                              RETURN_RISK_ADJ_S_CAP_COST(I,R_YEAR) * &
                                                  SCENARIO_CAP_COST_MULT
            IF(SCREEN_CAPACITY(CN) <= 0.) THEN
               WRITE(4,*) "EXPANSION CAPACITY NEGATIVE IN MRX"
               WRITE(4,*) "FOR RESOURCE",OPTION_NAME(CN)
               WRITE(4,*) "OPTION CAPACITY RESET TO 100 MW"
               SCREEN_CAPACITY(CN) = 100.
            ENDIF
!
            FOM_COST_MM(CN) = FIX_OM_COST(CN)* 0.000001
!
            ANNUAL_FIXED_COST(CN) = &
                        1000000.*ANNUAL_LEVELIZED_CAPITAL(CN) + &
                                                         FIX_OM_COST(CN)
!
! 041110 MOVED UP. CHANGED TO CM. NEED TO USE THIS FOR CAPACITY PAYMENT
!
            IF(SCREEN_CAPACITY(CN) > 0.01) THEN
               TG = GET_TRANS_GROUP_POSITION(TRANSACTION_GROUP(CN))
               CM = GET_CM_FROM_TG(TG)
               TEMP_R4 = ANNUAL_FIXED_COST(CN)* 0.001/ &
                                            SCREEN_CAPACITY(CN)
!
               IF(TEMP_R4 < LOWEST_FIXED_COST_BY_CM(CM) .AND. &
                                                   OP_LIFE(CN) > 0) THEN
                  LOWEST_FIXED_COST_BY_CM(CM) = MAX(0.0,TEMP_R4)
               ENDIF
            ENDIF
!
            NET_MARGIN(CN) = -999999.
            NET_MARGIN_PER_KWYR(CN) = -999999.
            NET_MARGIN_INTERVAL(CN) = -1
!
! 03/05/04. BIG DEFINITIONAL CHANGE FOR STRIKE_PRICE
!
            DO MO = 1, 12
               AVERAGE_FUEL_PRICE(CN) = &
                     AVERAGE_FUEL_PRICE(CN) + &
                        FUEL_COST_PER_MWH(MO,CN)
            END DO
            AVERAGE_FUEL_PRICE(CN) = AVERAGE_FUEL_PRICE(CN) / 12.0
!
! 080112. CONSISTENT DEFINIATION OF FUEL PRICE.
!
            IF(HEAT_RATE(CN) > 0.001) THEN
             FUEL_COST_PER_MMBTU(CN) = AVERAGE_FUEL_PRICE(CN) * 1000./ &
                                                   HEAT_RATE(CN)
            ENDIF

            MO = 7

            STRIKE_PRICE(CN,MO) = AVERAGE_FUEL_PRICE(CN) + &
                                       EMISSIONS_PER_MWH(CN) + &
                                                      VAR_OM_PER_MWH(CN)
!
            LOCAL_ANNUAL_UNITS(CN) = ANNUAL_UNITS_FOR_OPTION(I)
            LOCAL_LEAD_TIME(CN) = LEAD_TIME_FOR_OPTION(I)
            IF(LOCAL_ANNUAL_UNITS(CN) < 0) THEN 
            ! ASSUME IT IS A POINTER PER PACIFICORP.
               LOCAL_ANNUAL_UNITS(CN) = &
                     INT(GET_VAR(FLOAT(LOCAL_ANNUAL_UNITS(CN)),R_YEAR, &
                                                     OPTION_NAME(CN)),2)
            ENDIF
            LOCAL_ANNUAL_UNITS(CN) = &
                            MIN(CUMULATIVE_UNITS_FOR_OPTION(I), &
                                                 LOCAL_ANNUAL_UNITS(CN))
            IF(LOCAL_ANNUAL_UNITS(CN) > 0) THEN
               ANNUAL_RESOURCES_AVAILABLE = .TRUE.
            ENDIF
!
         ENDDO ! OPTIONS
         TOTAL_ACTIVE_OPTIONS = AO
!
! NEXT, ASSUME ALL RESOURCES ARE ANNUAL CALL OPTIONS WHERE THE VARIABLE
! COST IS THE STRIKE PRICE AND
! UTILIZE THE CALL OPTIONS WITH THE HIGHEST NET MARGIN.
!
         IF(N == 0) RETURN
!
! 011920. COMBINE HYBRID: FIXED AND CAPITAL COSTS
!
         DO CN = 1, TOTAL_ALL_OPTIONS
            IF(INDEPENDENT(CN) == 0) CYCLE
            HYBRID_PROD_POINTER = INDEPENDENT(CN)
            FIX_OM_COST(CN) = FIX_OM_COST(CN) + &
                                        FIX_OM_COST(HYBRID_PROD_POINTER)
            FOM_COST_MM(CN) = FOM_COST_MM(CN) + &
                                        FOM_COST_MM(HYBRID_PROD_POINTER)
            ANNUAL_FIXED_COST(CN) = ANNUAL_FIXED_COST(CN) + &
                                  ANNUAL_FIXED_COST(HYBRID_PROD_POINTER)
            ANNUAL_LEVELIZED_CAPITAL(CN) = &
                     ANNUAL_LEVELIZED_CAPITAL(CN) + &
                           ANNUAL_LEVELIZED_CAPITAL(HYBRID_PROD_POINTER)
         ENDDO
!
         UPPER_TRANS_GROUP = GET_NUMBER_OF_ACTIVE_GROUPS()
         DO TG = 1, UPPER_TRANS_GROUP 
         ! INDEX FROM THE CAPACITY OPTIONS FILE
!
             IF(MRX_ICAP_UNIT_LINK(TG) .NE. 0) THEN
               IF(MRX_ICAP_UNIT_LINK(TG) < 0) THEN
                  ConeLink = GET_ANNUAL_VECTOR_VALUE( &
                                          MRX_ICAP_UNIT_LINK(TG),R_YEAR)
               ELSE
                  ConeLink = MRX_ICAP_UNIT_LINK(TG)
               ENDIF
               ConeLink = ResourceIDtoOptionPosPrt(ConeLink)
               IF(ConeLink == 0) CYCLE
               IF( .NOT. RESOURCE_AVAILABLE(ConeLink, &
                              THIS_YEAR_PLUS_LEAD_TIME(ConeLink)) .OR. &
                  .NOT. TEST_START_STOP_YEARS(ConeLink, &
                              THIS_YEAR_PLUS_LEAD_TIME(ConeLink))) CYCLE
              IF(GET_TRANS_GROUP_POSITION(TRANSACTION_GROUP(ConeLink)) &
                                                             <= 0) CYCLE
               ConeLinkCapacity = SCREEN_CAPACITY(ConeLink)
!
               IF(ConeLinkCapacity > 0.01) THEN
                  CM = GET_CM_FROM_TG(TG)
                  TEMP_R4 = ANNUAL_FIXED_COST(ConeLink)* 0.001/ &
                                            SCREEN_CAPACITY(ConeLink)
                  IF(TEMP_R4 < LOWEST_CONE_COST_BY_CM(CM) .AND. &
                            RETURN_CN_OP_LIFE(ConeLink,R_YEAR) > 0) THEN
                     LOWEST_CONE_COST_BY_CM(CM) = MAX(0.0,TEMP_R4)
                   ENDIF
                ELSE
                  WRITE(4,*) 'IN MRX, THE RESOURCE LINK IS NOT FOUND'
                  WRITE(4,*) 'LINK = ',MRX_ICAP_UNIT_LINK(TG)
                  er_message='Stop requested from CAP_OBJT SIID4'
                  call end_program(er_message)
               ENDIF
            ENDIF
         ENDDO
!
         CURRENT_TARGET_RATIO = TARGET_RATIO(3,R_YEAR) * &
                                                 SCENARIO_RESERVE_MARGIN
         ! TODO: Individually check and deallocate each array.  Following the
         ! deallocation actions, individually allocate each array
         ! and call check_alloc after the allocation. cap_objt:ra7
         IF(ALLOCATED(MULTI_AREA_NAME)) DEALLOCATE(MULTI_AREA_NAME, &
                                           PA_PLANNING_RESERVE_MARGIN, &
                                           CM_PLANNING_RESERVE_MARGIN, &
                                        PA_PEAK_AFTER_INTERRUPTIBLE, &
                                        PEAK_AFTER_INTERRUPTIBLE_TG, &
                                        TG_PLANNING_RESERVE_MARGIN, &
                                        CM_PEAK_AFTER_INTERRUPTIBLE, &
                                        PA_PLANNING_CAPACITY, &
                                        CM_PLANNING_CAPACITY, &
                                           CM_IOTA, &
                                           TG_PLANNING_CAPACITY, &
                                           TG_2_PLANNING_AREA, &
                                           TG_2_CAPACITY_MARKET, &
                                           CURRENT_TARGET_RATIO_PA, &
                                           MAXIMUM_RM_PA, &
                                          TOTAL_ANNUAL_CAPACITY_ADDED, &
                                           CM_TOT_ANN_CAPACITY_ADDED, &
                                       TG_TOTAL_ANNUAL_CAPACITY_ADDED, &
                                           MINIMUM_ANNUAL_MRX_CAP, &
                                           MINIMUM_TG_MRX_CAP, &
                                           MAXIMUM_TG_MRX_CAP, &
                                           CO2_EMISSIONS_CAP, &
                                         CO2_EMISS_REDUCTION_REQUIRED, &
                                           CO2_EMISSIONS, &
                                           MAXIMUM_ANNUAL_MRX_CAP, &
                                           PA_ICAP_REVENUE_MULT, &
                                           CM_ICAP_REVENUE_MULT)
         ALLOCATE(MULTI_AREA_NAME(0:UPPER_TRANS_GROUP), &
                      PA_PLANNING_RESERVE_MARGIN(0:UPPER_TRANS_GROUP), &
                      CM_PLANNING_RESERVE_MARGIN(0:UPPER_TRANS_GROUP), &
                     PA_PEAK_AFTER_INTERRUPTIBLE(0:UPPER_TRANS_GROUP), &
                     PEAK_AFTER_INTERRUPTIBLE_TG(0:UPPER_TRANS_GROUP), &
                      TG_PLANNING_RESERVE_MARGIN(0:UPPER_TRANS_GROUP), &
                     CM_PEAK_AFTER_INTERRUPTIBLE(0:UPPER_TRANS_GROUP), &
                        PA_PLANNING_CAPACITY(0:UPPER_TRANS_GROUP), &
                        CM_PLANNING_CAPACITY(0:UPPER_TRANS_GROUP), &
                        CM_IOTA(0:UPPER_TRANS_GROUP), &
                           MAXIMUM_RM_PA(0:UPPER_TRANS_GROUP), &
                         CURRENT_TARGET_RATIO_PA(0:UPPER_TRANS_GROUP), &
                            PA_ICAP_REVENUE_MULT(0:UPPER_TRANS_GROUP), &
                            CM_ICAP_REVENUE_MULT(0:UPPER_TRANS_GROUP), &
                       CM_TOT_ANN_CAPACITY_ADDED(0:UPPER_TRANS_GROUP), &
                           TG_PLANNING_CAPACITY(0:UPPER_TRANS_GROUP), &
                           TG_2_PLANNING_AREA(0:UPPER_TRANS_GROUP), &
                           TG_2_CAPACITY_MARKET(0:UPPER_TRANS_GROUP), &
                     TOTAL_ANNUAL_CAPACITY_ADDED(0:UPPER_TRANS_GROUP), &
                  TG_TOTAL_ANNUAL_CAPACITY_ADDED(0:UPPER_TRANS_GROUP), &
                          MAXIMUM_ANNUAL_MRX_CAP(0:UPPER_TRANS_GROUP), &
                          MINIMUM_ANNUAL_MRX_CAP(0:UPPER_TRANS_GROUP), &
                            MINIMUM_TG_MRX_CAP(0:UPPER_TRANS_GROUP), &
                            MAXIMUM_TG_MRX_CAP(0:UPPER_TRANS_GROUP), &
                            CO2_EMISSIONS_CAP(0:UPPER_TRANS_GROUP), &
                    CO2_EMISS_REDUCTION_REQUIRED(0:UPPER_TRANS_GROUP), &
                            CO2_EMISSIONS(0:UPPER_TRANS_GROUP))
         TG_TOTAL_ANNUAL_CAPACITY_ADDED = 0.0
         MULTI_AREA_NAME(0) = 'System'
         DO L = 1, UPPER_TRANS_GROUP
            MULTI_AREA_NAME(L) = GET_GROUP_NAME(L)
         ENDDO
!
         HO = 0
         LAST_HO = -1
!
! 111507. CHANGED FOR TVA.
!
         HGST_NET_MARG_ADDED = MINIMUM_NET_MARGIN - 0.001 ! 0.
         SORTED_OPTIONS = 0
         HGST_OPT_INDEX = 0
!
! SYSTEM CO2
!
         CO2_EMISSIONS_CAP = 999999999999.0
         CO2_EMISS_REDUCTION_REQUIRED = 0.0
         CO2_EMISSIONS = 0.0
         TG = 0
         CO2_EMISSIONS_CAP(TG) = GET_EMISS_CAP_FOR_CLASS(CO2_PLN,TG)
         CO2_EMISSIONS(TG) = GET_CL_EMISS_FOR_CLASS(CO2_PLN,TG)
         CO2_EMISS_REDUCTION_REQUIRED(TG) = MAX(0.0, &
                              CO2_EMISSIONS(TG) - CO2_EMISSIONS_CAP(TG))

         RESOURCE_ADDITION = .TRUE.
!
! BEGIN CO2 RETIREMENTS
!
         IF(CO2_RETIREMENTS_LOGIC .and. .false.) THEN
            TEMP_R4 = CO2_EMISS_REDUCTION_REQUIRED(TG)
            CO2_RETIREMENT_PRICE = GET_CO2_RETIREMENT_PRICE(TEMP_R4)
            CO2_END_LIST = .FALSE.
            DO WHILE(.NOT. CO2_END_LIST .AND. &
                               CO2_EMISS_REDUCTION_REQUIRED(TG) > 0.001)
               TEMP_L = GET_NEXT_MRX_RETIRE_RETRO( &
                                          CO2_COUNTER, &
                                          CO2_RETIRE_OR_RETRO, &
                                          CO2_NUNIT, &
                                          CO2_TG, &
                                          CO2_UNIT_MW, &
                                          CO2_UNIT_MWH, &
                                          UNIT_CO2, &
                                          CO2_UNIT_PRICE, &
                                          CO2_STRIKE_PRICE, &
                                          CO2_UNIT_MW_AFTER, &
                                          CO2_STRIKE_PRICE_AFTER, &
                                          UNIT_CO2_AFTER, &
                                          CO2_END_LIST)
               IF(CO2_RETIRE_OR_RETRO == 1) THEN ! RETIRE
                  TEMP_L = RETIRE_CO2_THERMAL_UNIT( &
                                          CO2_NUNIT, &
                                          CO2_UNIT_MW, &
                                          UNIT_CO2, &
                                          CO2_UNIT_PRICE)
               ELSE
                  TEMP_L = RETROFIT_CO2_THERMAL_UNIT( &
                                          CO2_NUNIT, &
                                          CO2_UNIT_MW, &
                                          UNIT_CO2, &
                                          CO2_UNIT_PRICE, &
                                          CO2_RETIRE_OR_RETRO)
               ENDIF
               CO2_EMISS_REDUCTION_REQUIRED(0) = &
                         CO2_EMISS_REDUCTION_REQUIRED(0) - UNIT_CO2 + &
                                                          UNIT_CO2_AFTER
            ENDDO
         ENDIF
!
! END SYSTEM CO2
!
         IF(RUN_PRICE_MODE) BUILD_TO_MARKET = .FALSE.
!
            CM_PLANNING_RESERVE_MARGIN = ZERO
            CM_PEAK_AFTER_INTERRUPTIBLE = ZERO
            CM_PLANNING_CAPACITY = ZERO
            CM_ICAP_REVENUE_MULT = ZERO
            TG_2_CAPACITY_MARKET = ZERO
            CM_TOT_ANN_CAPACITY_ADDED = ZERO
!
            PA_PEAK_AFTER_INTERRUPTIBLE = ZERO
            TOTAL_ANNUAL_CAPACITY_ADDED = 0.
            MINIMUM_ANNUAL_MRX_CAP = 0.
            PA_PLANNING_CAPACITY = ZERO
!
            DO PA = 0, PG
               PA_PLANNING_RESERVE_MARGIN(PA) = ZERO

               TG_2_PLANNING_AREA(PA) = 0

               MAXIMUM_ANNUAL_MRX_CAP(PA) = 999999.
               PA_ICAP_REVENUE_MULT(PA) = 0.

!
! CURRENT_TARGET_RATIO
! STILL NEED TO PUSH THE VALUES BELOW FOR ALL MINIMUM AND MAXIMUM RM'S.
!
               IF(REGIONAL_PARAMS_EXIST) THEN
                  CURRENT_TARGET_RATIO_PA(PA) = &
                           ICAP_MIN_TESTING_MARGIN( &
                                     R_YEAR,CURRENT_TARGET_RATIO,PA) * &
                                                 SCENARIO_RESERVE_MARGIN
!
! NEED TO CHANGE THE ORDER FOR MAX
!
                  MAXIMUM_RM_PA(PA) = &
                           ICAP_MAX_TESTING_MARGIN( &
                                      R_YEAR,MAXIMUM_RM,PA) * &
                                                 SCENARIO_RESERVE_MARGIN
               ELSE
                  CURRENT_TARGET_RATIO_PA(PA) = CURRENT_TARGET_RATIO
                  MAXIMUM_RM_PA(PA) = MAXIMUM_RM * &
                                                 SCENARIO_RESERVE_MARGIN
               ENDIF
            ENDDO
!
            MAXIMUM_ANNUAL_MRX_CAP(0) = GET_MAXIMUM_ANNUAL_MRX_CAP()
            TG_PLANNING_RESERVE_MARGIN = 0.0
            PEAK_AFTER_INTERRUPTIBLE_TG = 0.0
!
            DO TG = 1, UPPER_TRANS_GROUP
               PLANNING_RM_BEFORE_ADDITIONS = ZERO
               TG_2_PLANNING_AREA(TG) = GET_PA_FROM_TG(TG)
               TG_2_CAPACITY_MARKET(TG) = GET_CM_FROM_TG(TG)
               PA = TG_2_PLANNING_AREA(TG)
               CM = TG_2_CAPACITY_MARKET(TG)
               CM_INDEX = GET_CM_INDEX_FROM_TG(CM)
!
               EXISTING_PLANNING_CAPACITY(0) = &
                           GET_CL_TG_CAP( &
                                    INT(0,2),TG,R_YEAR,INT(1,2)) + &
                           GET_EL_TG_CAP(TG,R_YEAR,INT(1,2)) + &
                             DERIV_CAPACITY_PLANNING_BY_TG(TG,R_YEAR)
!
               NEW_PLANNING_CAPACITY(0) = &
                              GET_CL_TG_CAP( &
                                    INT(0,2),TG,R_YEAR,INT(2,2)) + &
                            GET_EL_TG_CAP(TG,R_YEAR,INT(2,2)) + &
                           DERIV_NEW_CAP_PLAN_BY_ALL(ZERO_I2,TG,R_YEAR)
               PA_PLANNING_CAPACITY(PA) = PA_PLANNING_CAPACITY(PA) + &
                           EXISTING_PLANNING_CAPACITY(0) + &
                                                NEW_PLANNING_CAPACITY(0)
               CM_PLANNING_CAPACITY(CM) = CM_PLANNING_CAPACITY(CM) + &
                           EXISTING_PLANNING_CAPACITY(0) + &
                                                NEW_PLANNING_CAPACITY(0)
               IF(MRX_SORTED_MIX) THEN 
               ! I THINK THIS IS WHAT STEVE WANTS.
                  CAPACITY_PLANNING_PEAK = &
                                        GET_GROUP_PEAK_ON_PEAK_MONTH(TG)
               ELSEIF(USE_REGIONAL_MINIMUM_RM_LOGIC) THEN
                  CAPACITY_PLANNING_PEAK = &
                                        GET_PEAK_ON_PA_PEAK_MONTH(TG,PA)
               ELSE
                  CAPACITY_PLANNING_PEAK = &
                                        GET_GROUP_PEAK_ON_PEAK_MONTH(TG)
               ENDIF
!
               INTERRUPTIBLE_LOAD = GET_ANNUAL_INTER_CAPACITY(TG)
               PA_PEAK_AFTER_INTERRUPTIBLE(PA) = &
                     PA_PEAK_AFTER_INTERRUPTIBLE(PA) + &
                        MAX(0., &
                            CAPACITY_PLANNING_PEAK - INTERRUPTIBLE_LOAD)
               PA_PEAK_MW(PA) = PA_PEAK_AFTER_INTERRUPTIBLE(PA)
               CM_PEAK_AFTER_INTERRUPTIBLE(CM) = &
                     CM_PEAK_AFTER_INTERRUPTIBLE(CM) + &
                        MAX(0., &
                            CAPACITY_PLANNING_PEAK - INTERRUPTIBLE_LOAD)
               CM_PEAK_MW(CM) = CM_PEAK_AFTER_INTERRUPTIBLE(CM)
!
! 090707. MIN AND MAX RM BY TG
!
               TG_PLANNING_CAPACITY(TG) = &
                           EXISTING_PLANNING_CAPACITY(0) + &
                                                NEW_PLANNING_CAPACITY(0)
               PEAK_AFTER_INTERRUPTIBLE_TG(TG) = &
                        MAX(0., &
                            CAPACITY_PLANNING_PEAK - INTERRUPTIBLE_LOAD)
               IF(PEAK_AFTER_INTERRUPTIBLE_TG(TG) > NEAR_ZERO)  THEN
                  TG_PLANNING_RESERVE_MARGIN(TG) = &
                        1. + (TG_PLANNING_CAPACITY(TG) - &
                                  PEAK_AFTER_INTERRUPTIBLE_TG(TG)) / &
                                         PEAK_AFTER_INTERRUPTIBLE_TG(TG)
                  MINIMUM_TG_MRX_CAP(TG) = &
                           MAX(0., &
                              (GET_MIN_CAP_TESTING_RATIO(TG)* &
                                             SCENARIO_RESERVE_MARGIN - &
                                  TG_PLANNING_RESERVE_MARGIN(TG))* &
                                        PEAK_AFTER_INTERRUPTIBLE_TG(TG))
!
                  MAXIMUM_TG_MRX_CAP(TG) = &
                           MIN(999999., &
                                (GET_MAX_CAP_TESTING_RATIO(TG)* &
                                             SCENARIO_RESERVE_MARGIN - &
                                     TG_PLANNING_RESERVE_MARGIN(TG)) * &
                                        PEAK_AFTER_INTERRUPTIBLE_TG(TG))
                  TG_PEAK_MW(TG) = PEAK_AFTER_INTERRUPTIBLE_TG(TG)
               ENDIF

!
            ENDDO ! TRANSACTION GROUPS
!
! 06/10/03. CHANGED TO PG COUNTER
!
         DO PA = 1, PG
!
               PA_PEAK_AFTER_INTERRUPTIBLE(0) = &
                  PA_PEAK_AFTER_INTERRUPTIBLE(0) + &
                     PA_PEAK_AFTER_INTERRUPTIBLE(PA)
               PA_PLANNING_CAPACITY(0) = &
                  PA_PLANNING_CAPACITY(0) + &
                     PA_PLANNING_CAPACITY(PA)
!
               IF(PA_PEAK_AFTER_INTERRUPTIBLE(PA) > NEAR_ZERO)  THEN
                  PA_PLANNING_RESERVE_MARGIN(PA) = &
                        1. + (PA_PLANNING_CAPACITY(PA) - &
                                 PA_PEAK_AFTER_INTERRUPTIBLE(PA)) / &
                                      PA_PEAK_AFTER_INTERRUPTIBLE(PA)
                  MINIMUM_ANNUAL_MRX_CAP(PA) = MAX(0., &
                        (CURRENT_TARGET_RATIO_PA(PA) - &
                           PA_PLANNING_RESERVE_MARGIN(PA))* &
                                        PA_PEAK_AFTER_INTERRUPTIBLE(PA))
!
                  IF(USE_MAXIMUM_RM_LOGIC) THEN
                     MAXIMUM_ANNUAL_MRX_CAP(PA) = &
                       MIN(MAXIMUM_ANNUAL_MRX_CAP(PA), &
                           MAX(0., &
                                MAXIMUM_RM_PA(PA) - &
                                     PA_PLANNING_RESERVE_MARGIN(PA)) * &
                                       PA_PEAK_AFTER_INTERRUPTIBLE(PA))
                  ENDIF
               ELSE
                  PA_PLANNING_RESERVE_MARGIN(PA) = ZERO
               ENDIF
!
! 022210. TAKEN OUT: PA TO CM
!
               PA_ICAP_REVENUE_MULT(PA) = GET_ICAP_REVENUE_MULT( &
                               R_YEAR,PA,PA_PLANNING_RESERVE_MARGIN(PA))
         ENDDO
         IF(PA_PEAK_AFTER_INTERRUPTIBLE(0) > NEAR_ZERO)  THEN
            PA_PLANNING_RESERVE_MARGIN(0) = &
                        1. + (PA_PLANNING_CAPACITY(0) - &
                                 PA_PEAK_AFTER_INTERRUPTIBLE(0)) / &
                                      PA_PEAK_AFTER_INTERRUPTIBLE(0)
         ENDIF
!
         IF(R_YEAR >= 5) Then
           CM = CM
         Endif
         DO CM = 1, CG
!
               ICAP_SWITCH_INDEX(CM) = &
                    GET_ICAP_SWITCH_INDEX(CM, &
                                          R_YEAR, &
                                          TEMP_R4)
               ICAP_CONE_PRICE(CM) = TEMP_R4 
               ! MONTHLY TO ANNUAL IN GET_ICAP... *12.0
!
               IF(CM_PEAK_AFTER_INTERRUPTIBLE(CM) > NEAR_ZERO)  THEN
                  CM_PLANNING_RESERVE_MARGIN(CM) = &
                        1. + (CM_PLANNING_CAPACITY(CM) - &
                                 CM_PEAK_AFTER_INTERRUPTIBLE(CM)) / &
                                      CM_PEAK_AFTER_INTERRUPTIBLE(CM)
               ELSE
                  CM_PLANNING_RESERVE_MARGIN(CM) = ZERO
               ENDIF
!
               CM_ICAP_REVENUE_MULT(CM) = GET_ICAP_REVENUE_MULT( &
                               R_YEAR,CM,CM_PLANNING_RESERVE_MARGIN(CM))
         ENDDO
!
         FirmTransfer = 0.0
         CM_Net_Transfer = 0.0
         PA_Net_Transfer = 0.0
         NUMBER_DIRECT_INTERCONNECTS = 0
         DIRECT_INTERCONNECT_TG = 0
         TransmissionConstraint = 0.0
         TransferConvergence = 0.01
         MO = 7
         TEMP_L = INIT_HOUR_PATH_LIMIT(LOCAL_TIME_OF_DAY, &
                                       MO, &
                                       R_YEAR)
         DO I = 1, UPPER_TRANS_GROUP
            DO J = I-1,1,-1 ! DON'T CHECK DIAGONAL
               IF(HOUR_PATH_ACTIVE(I,J)) THEN
                  NUMBER_DIRECT_INTERCONNECTS(J) = &
                                      NUMBER_DIRECT_INTERCONNECTS(J) + 1
                  DIRECT_INTERCONNECT_TG(J, &
                                     NUMBER_DIRECT_INTERCONNECTS(J)) = I
                  TransmissionConstraint(I,J) = HOUR_TIE_LIMIT(I,J)  * &
                                       GetCapTransMultiplier(I,J,R_YEAR)
               ENDIF
               IF(HOUR_PATH_ACTIVE(J,I)) THEN
                  NUMBER_DIRECT_INTERCONNECTS(I) = &
                                      NUMBER_DIRECT_INTERCONNECTS(I) + 1
                  DIRECT_INTERCONNECT_TG(I, &
                                     NUMBER_DIRECT_INTERCONNECTS(I)) = J
                  TransmissionConstraint(J,I) = HOUR_TIE_LIMIT(J,I)  * &
                                       GetCapTransMultiplier(J,I,R_YEAR)
               ENDIF
            END DO
         END DO
         MaxTransmissionConstraint = TransmissionConstraint
         MaxTransferValue = 999.0
         LAST_SELLER = -1
         LAST_BUYER = -1
!
! TEST
!
         TARGET_IS_PA = .FALSE.
         TG_MW_TRANSFERS = .TRUE.
         MAX_ITER = 20 * UPPER_TRANS_GROUP
         ITER = 0
         DO
            ITER = ITER + 1
            IF(ITER == MAX_ITER) EXIT
            TransferValue = -999.0
            MaxTransferValue = -999.0
            SELLER = 0
            BUYER = 0
!
! IDENTIFY MaxTransferValue, SELLER, BUYER
!
            DO I = 1, UPPER_TRANS_GROUP
               DO J = I-1,1,-1 ! DON'T CHECK DIAGONAL
                  IF(TG_PEAK_MW(I) > NEAR_ZERO) THEN
                     CM = TG_2_CAPACITY_MARKET(I)
                     TEMP_R = &
                           (TG_Net_Transfer(I) + &
                                             TG_PLANNING_CAPACITY(I))/ &
                                                       TG_PEAK_MW(I)
                     TransferValue(I,J) = TEMP_R

                  ELSE
                     TransferValue(I,J) = 999.0 ! INFINITY
                  ENDIF
                  IF(TG_PEAK_MW(J) > NEAR_ZERO) THEN
                     CM = TG_2_CAPACITY_MARKET(J)
                     TEMP_R = &
                           (TG_Net_Transfer(J) + &
                                             TG_PLANNING_CAPACITY(J))/ &
                                                       TG_PEAK_MW(J)
                     TransferValue(J,I) = TEMP_R

                  ELSE
                     TransferValue(J,I) = 999.0 ! INFINITY
                  ENDIF
                  PA = TG_2_PLANNING_AREA(I)
                  IF(TARGET_IS_PA) THEN
                     TEMP_R = PA_PLANNING_RESERVE_MARGIN(PA)
                  ELSE
                     TEMP_R = PA_PLANNING_RESERVE_MARGIN(0)
                  ENDIF
                  I_CAPACITY = MAX(0.0, &
                         TG_Net_Transfer(I) + &
                         TG_PLANNING_CAPACITY(I)- &
                                     CURRENT_TARGET_RATIO_PA(PA)* &
                                                     TG_PEAK_MW(I) )

                  PA = TG_2_PLANNING_AREA(J)
                  IF(TARGET_IS_PA) THEN
                     TEMP_R = PA_PLANNING_RESERVE_MARGIN(PA)
                  ELSE
                     TEMP_R = PA_PLANNING_RESERVE_MARGIN(0)
                  ENDIF
                  J_CAPACITY = MAX(0.0, &
                         TG_Net_Transfer(J) + &
                         TG_PLANNING_CAPACITY(J)- &
                                     CURRENT_TARGET_RATIO_PA(PA)* &
                                                     TG_PEAK_MW(J) )

                  IF(I_CAPACITY > 0.1 .OR. J_CAPACITY > 0.1) THEN
                     IF(TransferValue(I,J) > TransferValue(J,I) .AND. &
                                 I_CAPACITY > 0.1 .AND. &
                                 TransmissionConstraint(I,J) > 0.1) THEN
                        TEMP_R = TransferValue(I,J) - TransferValue(J,I)
                        IF(I == LAST_SELLER .AND. &
                                          J == LAST_BUYER) THEN
                           SAME_TRANSACTION = .TRUE.
                        ELSE
                           SAME_TRANSACTION = .FALSE.
                        ENDIF
                        IF(TEMP_R > MaxTransferValue .AND. &
                                             .NOT. SAME_TRANSACTION)THEN
                           SELLER = I
                           BUYER = J
                           SELLER_CAPACITY = I_CAPACITY
                           BUYER_CAPACITY = J_CAPACITY
                           MaxTransferValue = TEMP_R
                        ENDIF
                     ELSEIF(TransmissionConstraint(J,I)> 0.1 .AND. &
                                                  J_CAPACITY > 0.1) THEN
                        TEMP_R = TransferValue(J,I) - TransferValue(I,J)
                        IF(J == LAST_SELLER .AND. &
                                          I == LAST_BUYER) THEN
                           SAME_TRANSACTION = .TRUE.
                        ELSE
                           SAME_TRANSACTION = .FALSE.
                        ENDIF
                        IF(TEMP_R > MaxTransferValue .AND. &
                                             .NOT. SAME_TRANSACTION)THEN
                           SELLER = J
                           BUYER = I
                           SELLER_CAPACITY = J_CAPACITY
                           BUYER_CAPACITY = I_CAPACITY
                           MaxTransferValue = TEMP_R
                        ENDIF ! CHECK FOR IMPROVED SOLUTION
                     ENDIF ! I OR J SELLER
                  ENDIF ! CAPACITY CHECK
               END DO
            END DO
!
            IF(SELLER == 0 .OR. BUYER == 0 .OR. &
                            MaxTransferValue < TransferConvergence) EXIT
!
! IDENTIFY QUANTITY TO TRANSFER
!
            TEMP_R = MIN( &
                         SELLER_CAPACITY, &
                         TransmissionConstraint(SELLER,BUYER))
!
            X_SEARCH = TEMP_R
            HALF = X_SEARCH
            Y_BEFORE_SELLER = TransferValue(SELLER,BUYER)
            X_BEFORE_SELLER = TEMP_R
            Y_BEFORE_BUYER = TransferValue(BUYER,SELLER)
            X_BEFORE_BUYER = 0.0
            DO I = 1, 10
               X_AFTER_SELLER = TEMP_R - X_SEARCH
               Y_AFTER_SELLER = &
                      (TG_Net_Transfer(SELLER) + &
                       TG_PLANNING_CAPACITY(SELLER)-X_SEARCH)/ &
                                                 TG_PEAK_MW(SELLER)
               X_AFTER_BUYER = X_SEARCH
               Y_AFTER_BUYER = &
                      (TG_Net_Transfer(BUYER) + &
                       TG_PLANNING_CAPACITY(BUYER)+X_SEARCH)/ &
                                               TG_PEAK_MW(BUYER)
               IF(ABS(Y_AFTER_SELLER - Y_AFTER_BUYER) < 0.01) THEN
                  TEMP_R = X_SEARCH
                  EXIT
               ELSE
                  HALF = HALF * 0.5
                  IF(Y_AFTER_SELLER > Y_AFTER_BUYER) THEN
                     IF(I == 1) THEN
                        TEMP_R = X_SEARCH
                        EXIT
                     ELSE ! X_SEARCH IS TOO SMALL
                        X_SEARCH = X_SEARCH + HALF
                     ENDIF
                  ELSE ! X_SEARCH IS TOO BIG
                     X_SEARCH = X_SEARCH - HALF
                  ENDIF
               ENDIF
            ENDDO


            IF(FirmTransfer(BUYER,SELLER) < 0.001) THEN
               FirmTransfer(SELLER,BUYER) = &
                     FirmTransfer(SELLER,BUYER) + TEMP_R
            ELSE
               IF(FirmTransfer(BUYER,SELLER) > TEMP_R) THEN
                  FirmTransfer(BUYER,SELLER) = &
                                     FirmTransfer(BUYER,SELLER) - TEMP_R
               ELSE
                  FirmTransfer(SELLER,BUYER) = &
                                     TEMP_R - FirmTransfer(BUYER,SELLER)
                  FirmTransfer(BUYER,SELLER) = 0.0
               ENDIF
            ENDIF
            TransmissionConstraint(SELLER,BUYER) = &
                        MaxTransmissionConstraint(SELLER,BUYER) - &
                                              FirmTransfer(SELLER,BUYER)
            TransmissionConstraint(BUYER,SELLER) = &
                        MaxTransmissionConstraint(BUYER,SELLER) - &
                                              FirmTransfer(BUYER,SELLER)

            TG_Net_Transfer(SELLER) = TG_Net_Transfer(SELLER) - TEMP_R
            TG_Net_Transfer(BUYER) = TG_Net_Transfer(BUYER) + TEMP_R
            SELLER_CM = TG_2_CAPACITY_MARKET(SELLER)
            BUYER_CM = TG_2_CAPACITY_MARKET(BUYER)
            SELLER_PA = TG_2_PLANNING_AREA(SELLER)
            BUYER_PA = TG_2_PLANNING_AREA(BUYER)
            IF(SELLER_CM /= BUYER_CM) THEN
             CM_Net_Transfer(SELLER_CM) = CM_Net_Transfer(SELLER_CM) - &
                                                                  TEMP_R
               CM_Net_Transfer(BUYER_CM) = CM_Net_Transfer(BUYER_CM) + &
                                                                  TEMP_R
            ENDIF
            IF(SELLER_PA /= BUYER_PA) THEN
             PA_Net_Transfer(SELLER_PA) = PA_Net_Transfer(SELLER_PA) - &
                                                                  TEMP_R
               PA_Net_Transfer(BUYER_PA) = PA_Net_Transfer(BUYER_PA) + &
                                                                  TEMP_R
            ENDIF
            LAST_SELLER = SELLER
            LAST_BUYER = BUYER
         END DO

         IF(GRX_8761_REPORT) THEN
!
            IF(GRX_8761_REPORT_NOT_OPEN) THEN
               GRX_8761_REPORT_NOT_OPEN = .FALSE.
               GRX_8761_REPORT_VARIABLES = UPPER_TRANS_GROUP+1
               GRX_8761_ALT_NO = GRX_8761_HEADER( &
                                  GRX_8761_REPORT_VARIABLES, &
                                  GRX_8761_ALT_REC)
!
               OPEN(10,FILE="MSG"//TRIM(GET_SCENAME())//".GP1")
               DO J = 0,  UPPER_TRANS_GROUP
                  WRITE(TEMP_STR,'(I6)') J
                  TEMP_STR = '"'//TRIM(MULTI_AREA_NAME(J))//'"'// &
                             ',V2,'//TRIM(TEMP_STR)// &
                           ',,S,,,,,,,,,"Help Message"'
                  WRITE(10,'(A)') TRIM(TEMP_STR)
               END DO
               CLOSE(10)
            ENDIF
            DO I = 0, UPPER_TRANS_GROUP
               GRX_8761_ALT_REC = RPTREC(GRX_8761_ALT_NO)
               WRITE(GRX_8761_ALT_NO,REC=GRX_8761_ALT_REC) &
                        PRT_ENDPOINT(), &
                        FLOAT(THIS_YEAR), &
                        'Firm Transfer       ', &
                        MULTI_AREA_NAME(I), &
                        (FirmTransfer(I,J),J=0,UPPER_TRANS_GROUP)
               GRX_8761_ALT_REC = GRX_8761_ALT_REC + 1
!
               GRX_8761_ALT_REC = RPTREC(GRX_8761_ALT_NO)
               WRITE(GRX_8761_ALT_NO,REC=GRX_8761_ALT_REC) &
                        PRT_ENDPOINT(), &
                        FLOAT(THIS_YEAR), &
                        'Capacity Available  ', &
                        MULTI_AREA_NAME(I), &
                        (TransmissionConstraint(I,J), &
                                                  J=0,UPPER_TRANS_GROUP)
               GRX_8761_ALT_REC = GRX_8761_ALT_REC + 1
               GRX_8761_ALT_REC = RPTREC(GRX_8761_ALT_NO)
               WRITE(GRX_8761_ALT_NO,REC=GRX_8761_ALT_REC) &
                        PRT_ENDPOINT(), &
                        FLOAT(THIS_YEAR), &
                        'Max Trans Constraint', &
                        MULTI_AREA_NAME(I), &
                        (MaxTransmissionConstraint(I,J), &
                                                  J=0,UPPER_TRANS_GROUP)
               GRX_8761_ALT_REC = GRX_8761_ALT_REC + 1
               GRX_8761_ALT_REC = RPTREC(GRX_8761_ALT_NO)
               WRITE(GRX_8761_ALT_NO,REC=GRX_8761_ALT_REC) &
                        PRT_ENDPOINT(), &
                        FLOAT(THIS_YEAR), &
                        'Transfer Value      ', &
                        MULTI_AREA_NAME(I), &
                        (TransferValue(I,J),J=0,UPPER_TRANS_GROUP)
               GRX_8761_ALT_REC = GRX_8761_ALT_REC + 1
            END DO
         ENDIF


         MAX_RESOURCES_PER_GROUP = 0
         MAX_RESOURCES_PER_YEAR_GROUP = &
                                MX_RES_P_YEAR/UPPER_TRANS_GROUP
!
         HIGHEST_ENERGY = 0.

         IF(MX_SUMMARY_REPORT .AND. MX_REPORT_NOT_OPEN) THEN
            MX_REPORT_NOT_OPEN = .FALSE.
            MX_ANNUAL_ALT_NO = MX_ANNUAL_HEADER( &
                                  MX_REPORT_VARIABLES,MX_ANNUAL_ALT_REC)
         ENDIF
!
         RPS_CURVE_REPORT = YES_RPS_CURVE_REPORT()
         IF(RPS_CURVE_REPORT .AND. VN_REPORT_NOT_OPEN) THEN
            VN_REPORT_NOT_OPEN = .FALSE.
            VN_ANNUAL_ALT_NO = RPS_CURVE_RPT_HEADER( &
                                  VN_REPORT_VARIABLES,VN_ANNUAL_ALT_REC)
         ENDIF
         CX_ACTIVE = .TRUE.
         IF(.NOT. CX_ACTIVE) GOTO 1234
!
         LOCAL_CHRONO_MARKET_PRICES = 0.0
         DO L = 1, UPPER_TRANS_GROUP
            IF(CX_ACTIVE) THEN
               CALL CX_TRANS_ANNUAL_USER_MARKET( &
                                      L,LOCAL_CHRONO_MARKET_PRICES(1,L))
            ENDIF
         ENDDO
         RESET_CHRONO_MARKET_PRICES = LOCAL_CHRONO_MARKET_PRICES
         NET_MARGIN = -999999.
         NET_MARGIN_PER_KWYR = -999999.
         LOCAL_RESOURCES_AVAILABLE = .FALSE.
!
         AI = 0
         TOTAL_ACTIVE_INSTANCES = 0
         DO  AO = 1, TOTAL_ACTIVE_OPTIONS
            CN = ACTIVE_OPTION(AO)
            IF(DEPENDENT(CN) > 0) CYCLE ! 011920.
            DO J = 1, LOCAL_ANNUAL_UNITS(CN)
               AI = AI + 1
               IF(AI > MX_RES_P_YEAR) EXIT
               ACTIVE_INSTANCE(AI) = AO
               SORTED_OPTIONS(AI) = AI
            ENDDO
            IF(AI > MX_RES_P_YEAR) EXIT
         ENDDO
!
         TOTAL_ACTIVE_INSTANCES = AI
         LOCAL_CHRONO_MW_USAGE = 0.0
         LOCAL_CHRONO_STRIKE_PRICE = 0.0
         CX_ITER = 0
         IF(THIS_YEAR == 2021) THEN
            THIS_YEAR = THIS_YEAR
         ENDIF
!
         MAX_CX_ITER = 5
         OPTIMAL_ORDER = .FALSE.
         TG_TOTAL_ANNUAL_CAPACITY_ADDED = 0.
!
         DO WHILE(CX_ITER < MAX_CX_ITER .AND. .NOT. OPTIMAL_ORDER)
            HGST_NET_MARG_ADDED = -999999. ! MAY NEED TO CHANGE
            TEMP_L = RESET_MRX_RPS_TEST_REQ() ! 032319
            DO AI = 1, TOTAL_ACTIVE_INSTANCES
               J = TOTAL_ACTIVE_INSTANCES - AI + 1
               J = SORTED_OPTIONS(J)
               AO = ACTIVE_INSTANCE(J)
               CN = ACTIVE_OPTION(AO)
               I = OPTION_POSITION(CN)
               IF( .NOT. RESOURCE_AVAILABLE(I, &
                                    THIS_YEAR_PLUS_LEAD_TIME(CN)) .OR. &
                      .NOT. TEST_START_STOP_YEARS(I, &
                                    THIS_YEAR_PLUS_LEAD_TIME(CN)) .OR. &
                               .NOT. DEPENDENT_UNIT_AVAILABLE(CN)) CYCLE
!
               IF(CX_ITER == 0) THEN
                  IF(FIRST_CN(CN) == 0) THEN
                     FIRST_CN(CN) = 1
                  ELSE
                     CYCLE
                  ENDIF
               ENDIF
               TG = TRANSACTION_GROUP(CN)
               L = GET_TRANS_GROUP_POSITION(TG)
               CM = GET_CM_FROM_TG(L)
               PA = TG_2_PLANNING_AREA(L)
               LOCAL_RESOURCES_AVAILABLE = .TRUE.
!
! ENERGY CALCULATIONS.
!
               IF(CX_ACTIVE) THEN
                  IF(FILE_SOURCE_INDEX(CN) == Transmission) THEN
                     PROD_POINTER = GET_GRX_RESOURCE_LINK_ID(I)
                     IF(PROD_POINTER < 1) THEN
                        CYCLE ! NEED TO CLEAN UP
                     ENDIF
                     PROD_POINTER = &
                            GET_TRANSMISSION_FOR_GRX_ID(PROD_POINTER)
                     BUYER = GET_TG_FOR_TRANSMISSION_BUY(PROD_POINTER)
                     SELLER = GET_TG_FOR_TRANSMISSION_SELL(PROD_POINTER)
                     IF(BUYER < 1 .OR. SELLER < 1) THEN
                        CYCLE ! NEED TO CLEAN UP
                     ENDIF
                     ANNUAL_STRIKES = 0.0
                     ANNUAL_ENERGY = 0.0
                     ANNUAL_ENERGY_REVENUE(CN) = 0.0
                     DO HR = 1, 8760
                        IF(LOCAL_CHRONO_MARKET_PRICES(HR,SELLER) > &
                           LOCAL_CHRONO_MARKET_PRICES(HR,BUYER) + &
                                                               .01) THEN
                           ANNUAL_STRIKES = ANNUAL_STRIKES + 1
                           ANNUAL_ENERGY = ANNUAL_ENERGY + &
                                                    SCREEN_CAPACITY(CN)
                           ANNUAL_ENERGY_REVENUE(CN) = &
                              ANNUAL_ENERGY_REVENUE(CN) + &
                                               SCREEN_CAPACITY(CN) * &
                              (LOCAL_CHRONO_MARKET_PRICES(HR,SELLER) - &
                               LOCAL_CHRONO_MARKET_PRICES(HR,BUYER))
                        ENDIF
                     ENDDO

                     ANNUAL_VARIABLE_COST(CN) = 0.0


                     VOM_COST_MM(CN) = ANNUAL_VOM_COST * 0.000001


                     ANNUAL_VOM_COST = ANNUAL_VARIABLE_COST(CN)
                     STRIKE_PRICE(CN,:) = VAR_OM_PER_MWH(CN) + &
                                                   EMISSIONS_PER_MWH(CN)
                     ANNUAL_EMIS_COST = 0.0
!
                  ELSEIF(FILE_SOURCE_INDEX(CN) == Derivative) THEN
                     PROD_POINTER = GET_GRX_RESOURCE_LINK_ID(I)
                     IF(PROD_POINTER < 1) THEN
                        CYCLE ! NEED TO CLEAN UP
                     ENDIF
                     PROD_POINTER = GET_TRANS_FOR_GRX_ID(PROD_POINTER)
                     IF(PROD_POINTER < 1) THEN
                        CYCLE ! NEED TO CLEAN UP
                     ENDIF
                     IF(CONTRACT_IS_STORAGE(PROD_POINTER) .AND. &
                                                 STORAGE_IS_ACTIVE) THEN
                        ANNUAL_EMIS_COST = 0.0
                        LOCAL_HYBRID_MW_DISCHARGE = 999999.0
                        LOCAL_HYBRID_HR_MARGIN = 0.0
                        LOCAL_HYBRID_MW_CHARGE = 999999.0
                        TEMP_L = CX_DailyOperAnPS2( &
                                        PROD_POINTER, & !  TRANS
                                      LOCAL_CHRONO_MARKET_PRICES(1,L), &
                                      !  8784, TG
                                        LOCAL_CHRONO_MW_USAGE, & !  8784
                                        ANNUAL_ENERGY, &
                                        ANNUAL_CHARGE, &
                                        ANNUAL_ENERGY_REVENUE(CN), &
                                        ANNUAL_VOM_COST, &
                                        LOCAL_HYBRID_MW_CHARGE, &
                                        LOCAL_HYBRID_MW_DISCHARGE, &
                                        LOCAL_HYBRID_HR_MARGIN, &
                                        LOCAL_HYBRID_PROFIT)

                     ELSE
                        TEMP_L = CX_ANNUAL_CONTRACT( &
                                        PROD_POINTER, &
                                      LOCAL_CHRONO_MARKET_PRICES(1,L), &
                                        MARKET_HOURS, &
                                        ANNUAL_STRIKES, & !  NOT USED
                                        ANNUAL_ENERGY, &
                                        ANNUAL_ENERGY_REVENUE(CN), &
                                        ANNUAL_VOM_COST, &
                                        ANNUAL_EMIS_COST, &
                                        LOCAL_CHRONO_MW_USAGE, &
                                        LOCAL_CHRONO_STRIKE_PRICE, &
                                        EMISSIONS_PER_MWH(CN))
                     ENDIF
                     ANNUAL_VARIABLE_COST(CN) = ANNUAL_VOM_COST + &
                                                        ANNUAL_EMIS_COST
                     VOM_COST_MM(CN) = ANNUAL_VOM_COST * 0.000001
                     IF(ANNUAL_ENERGY > 0.000001) THEN
                        VAR_OM_PER_MWH(CN) = &
                                          ANNUAL_VOM_COST/ ANNUAL_ENERGY
                     ENDIF

                     STRIKE_PRICE(CN,:) = VAR_OM_PER_MWH(CN) + &
                                                   EMISSIONS_PER_MWH(CN)
                  ELSEIF(FILE_SOURCE_INDEX(CN) == CapLimited) THEN
                           PROD_POINTER = GET_PRODUCTION_DATA_POINTER(I)
                     TEMP_L = .TRUE.
                     TEMP_I2 = -99
                     WRITE(CX_NAME,1011) &
                                            OPTION_NAME(CN)(1:14),CN,HO
 1011  FORMAT(A,I3,I3)
!
                     TEMP_L =  CX_UNIT( &
                                        TEMP_L, & !  ANNUAL SIMULATION
                                        CX_ITER, & !  032017
                                        CX_NAME, &
                                        BLOCK_CAPACITY(1,CN), &
                                        BLOCK_HEAT_RATE(1,CN), &
                                        FUEL_COST_PER_MWH(1,CN), &
                                        VAR_OM_PER_MWH(CN), &
                                        EMISSIONS_PER_MWH(CN), &
                                        MONTHLY_CAP_MULT(1,CN), &
                                        MIN_UP_TIME(CN), &
                                        MIN_DOWN_TIME(CN), &
                                        START_UP_COSTS(1,CN), &
                                        MUST_RUN_UNIT(1,CN), &
                                        PROD_POINTER, &
                                        TEMP_I2, & !  DUMMY FOR MONTH
                                      LOCAL_CHRONO_MARKET_PRICES(1,L), &
                                        LOCAL_CHRONO_MW_USAGE, &
                                        LOCAL_CHRONO_STRIKE_PRICE, &
                                        MARKET_HOURS, &
                                        ANNUAL_STRIKES, &
                                        ANNUAL_ENERGY, &
                                        ANNUAL_ENERGY_REVENUE(CN), &
                                        ANNUAL_VARIABLE_COST(CN), &
                                        ANNUAL_FUEL_COST(CN), &
                                        ANNUAL_VOM_COST, &
                                        ANNUAL_EMIS_COST)
                     TEMP_R4 = ANNUAL_VARIABLE_COST(CN) - &
                                        ANNUAL_FUEL_COST(CN) - &
                                        ANNUAL_VOM_COST - &
                                        ANNUAL_EMIS_COST

                  ENDIF ! TYPE OF AI RESOURCE
                  IF(INDEPENDENT(CN) > 0) THEN
!
! CONSTRAINED CHARGE, UNCONSTRAINED DISCHARGE.
!
                     INDEPENDENT_ANNUAL_ENERGY = ANNUAL_ENERGY
                     INDEPENDENT_ANNUAL_REVENUE = &
                                               ANNUAL_ENERGY_REVENUE(CN)
                     INDEPENDENT_MW_USAGE = LOCAL_CHRONO_MW_USAGE
                     INDEPENDENT_FUEL_COST = ANNUAL_FUEL_COST(CN)
                     INDEPENDENT_VOM_COST = ANNUAL_VOM_COST
                     INDEPENDENT_EMIS_COST = ANNUAL_EMIS_COST
! TRIPLE INDEX.
                     HYBRID_OPT_POINTER = INDEPENDENT(CN)
                     HYBRID_PROD_POINTER = &
                           GET_GRX_RESOURCE_LINK_ID(HYBRID_OPT_POINTER)
                     HYBRID_PROD_POINTER = &
                               GET_TRANS_FOR_GRX_ID(HYBRID_PROD_POINTER)
                     LOCAL_HYBRID_MW_DISCHARGE = 999999.0
                     LOCAL_HYBRID_HR_MARGIN = 0.0
                     LOCAL_HYBRID_MW_CHARGE = LOCAL_CHRONO_MW_USAGE

                     TEMP_L = CX_DailyOperAnPS2( &
                                        HYBRID_PROD_POINTER, & !  TRANS
                                      LOCAL_CHRONO_MARKET_PRICES(1,L), &
                                      !  8784, TG
                                        LOCAL_CHRONO_MW_USAGE, & ! 8784
                                        CONSTRAIN_ANNUAL_ENERGY, &
                                        CONSTRAIN_ANNUAL_CHARGE, &
                                        CONSTRAIN_ANNUAL_REVENUE, &
                                        CONSTRAIN_VOM_COST, &
                                        LOCAL_HYBRID_MW_CHARGE, &
                                        LOCAL_HYBRID_MW_DISCHARGE, &
                                        LOCAL_HYBRID_HR_MARGIN, &
                                        LOCAL_HYBRID_PROFIT)

                     CONSTRAIN_HYBRID_MARGIN = LOCAL_HYBRID_HR_MARGIN
                     CONSTRAIN_HYBRID_MW_USAGE = LOCAL_CHRONO_MW_USAGE
! 030720.
                     IF(RENEWABLE_ENERGY_PERCENT(HYBRID_OPT_POINTER) &
                                                          > 0.0001) THEN
                        HYBRID_MARKET_ENERGY_LIM = &
                           - CONSTRAIN_ANNUAL_CHARGE * &
                                 (1.0/RENEWABLE_ENERGY_PERCENT( &
                                              HYBRID_OPT_POINTER) - 1.0)
                     ELSE
                        HYBRID_MARKET_ENERGY_LIM = 999999.0
                     ENDIF
                     IF(HYBRID_MARKET_ENERGY_LIM > 0.0001) THEN
!
! UNCONSTRAINED CHARGE, UNCONSTRAINED DISCHARGE.
!
                        LOCAL_HYBRID_MW_DISCHARGE = 999999.0
                        LOCAL_HYBRID_HR_MARGIN = 0.0
                        LOCAL_HYBRID_MW_CHARGE = 999999.0

                        TEMP_L = CX_DailyOperAnPS2( &
                                        HYBRID_PROD_POINTER, & !  TRANS
                                      LOCAL_CHRONO_MARKET_PRICES(1,L), &
                                      !  8784, TG
                                        LOCAL_CHRONO_MW_USAGE, & ! 8784
                                        HYBRID_MARKET_ANNUAL_ENERGY, &
                                        HYBRID_MARKET_ANNUAL_CHARGE, &
                                        HYBRID_MARKET_ANNUAL_REVENUE, &
                                        HYBRID_MARKET_VOM_COST, &
                                        LOCAL_HYBRID_MW_CHARGE, &
                                        LOCAL_HYBRID_MW_DISCHARGE, &
                                        LOCAL_HYBRID_HR_MARGIN, &
                                        LOCAL_HYBRID_PROFIT)
                        HYBRID_MARKET_MW_USAGE = &
                                                   LOCAL_CHRONO_MW_USAGE
!
! FIND MOST PROFITABLE STORAGE DISCHARGE ENERGY GIVEN RENEWABLE ENERGY
! PERCENT
                        LOCAL_HYBRID_HR_MARGIN = 0.0
                        ADD_HYBRID_MW_CHARGE = 999999.0

                        TEMP_L = ANNUAL_STORAGE_SORT( &
                                        HYBRID_PROD_POINTER, &
                                        HYBRID_MARKET_ENERGY_LIM, &
                                        HYBRID_MARKET_MW_USAGE, &
                                        CONSTRAIN_HYBRID_MW_USAGE, &
                                        CONSTRAIN_HYBRID_MARGIN, &
                                        LOCAL_CHRONO_MW_USAGE)
!
! UNCONSTRAINED CHARGE, CONSTRAINED DISCHARGE.
!
                        LOCAL_HYBRID_MW_DISCHARGE = &
                                                   LOCAL_CHRONO_MW_USAGE
                        LOCAL_HYBRID_MW_CHARGE = 999999.0

                        TEMP_L = CX_DailyOperAnPS2( &
                                        HYBRID_PROD_POINTER, & !  TRANS
                                      LOCAL_CHRONO_MARKET_PRICES(1,L), &
                                      !  8784, TG
                                        LOCAL_CHRONO_MW_USAGE, & !  8784
                                        HYBRID_MARKET_ANNUAL_ENERGY, &
                                        HYBRID_MARKET_ANNUAL_CHARGE, &
                                        HYBRID_MARKET_ANNUAL_REVENUE, &
                                        HYBRID_MARKET_VOM_COST, &
                                        LOCAL_HYBRID_MW_CHARGE, &
                                        LOCAL_HYBRID_MW_DISCHARGE, &
                                        LOCAL_HYBRID_HR_MARGIN, &
                                        LOCAL_HYBRID_PROFIT)

                        MARKET_HYBRID_MW_USAGE = LOCAL_CHRONO_MW_USAGE
!
                        CONSTRAIN_HYBRID_MW_USAGE = &
                                       CONSTRAIN_HYBRID_MW_USAGE + &
                                                  MARKET_HYBRID_MW_USAGE
                     ELSE
                        HYBRID_MARKET_ANNUAL_ENERGY = 0.0
                        HYBRID_MARKET_ANNUAL_CHARGE = 0.0
                        HYBRID_MARKET_ANNUAL_REVENUE = 0.0
                        HYBRID_MARKET_VOM_COST = 0.0
                     ENDIF ! HYBRID MARKET ENERGY > 0
!
! COMBINED HOURLY ENERGY.
!
                     LOCAL_CHRONO_MW_USAGE = INDEPENDENT_MW_USAGE + &
                                               CONSTRAIN_HYBRID_MW_USAGE
!
                     ANNUAL_ENERGY = INDEPENDENT_ANNUAL_ENERGY + &
                               CONSTRAIN_ANNUAL_ENERGY + &
                                    CONSTRAIN_ANNUAL_CHARGE + &
                                         HYBRID_MARKET_ANNUAL_ENERGY

!
                     ANNUAL_ENERGY_REVENUE(CN) = &
                                     INDEPENDENT_ANNUAL_REVENUE + &
                                       CONSTRAIN_ANNUAL_REVENUE - &
                                       CONSTRAIN_VOM_COST + &
                                       HYBRID_MARKET_ANNUAL_REVENUE
                     ANNUAL_VOM_COST = INDEPENDENT_VOM_COST + &
                                                  HYBRID_MARKET_VOM_COST
                     ANNUAL_VARIABLE_COST(CN) = ANNUAL_VOM_COST + &
                                                   INDEPENDENT_EMIS_COST
                     VOM_COST_MM(CN) = ANNUAL_VOM_COST * 0.000001
                     IF(ANNUAL_ENERGY > 0.000001) THEN
                        VAR_OM_PER_MWH(CN) = &
                                          ANNUAL_VOM_COST/ ANNUAL_ENERGY
                     ENDIF

                  ENDIF ! HYBRID RESOURCE
               ENDIF ! AI ACTIVE
!
               IF(SCREEN_CAPACITY(CN) > 0.0) THEN
                  CF = ANNUAL_ENERGY /(SCREEN_CAPACITY(CN)*87.60)
               ELSE
                  CF = 0.0
               ENDIF
!
               IF( ANNUAL_ENERGY < -.0001) THEN
                  ANNUAL_ENERGY = ANNUAL_ENERGY
                  WRITE(4,*) 'ANNUAL ENERGY WHEN NEGATIVE'
                  er_message='Stop requested from CAP_OBJT SIID5'
                  call end_program(er_message)
               END IF
               IF(TG_MW_TRANSFERS) THEN
                  CM_CAPACITY_MW(CM) = &
                           CM_PLANNING_CAPACITY(CM) + &
                                 TG_TOTAL_ANNUAL_CAPACITY_ADDED(L)  + &
                                    CM_Net_Transfer(CM)
               ELSE
                  CM_CAPACITY_MW(CM) = &
                           CM_PLANNING_CAPACITY(CM) + &
                                 TG_TOTAL_ANNUAL_CAPACITY_ADDED(L)
               ENDIF
               IF(CM_PEAK_MW(CM) > .01) THEN
                  CM_RESERVE_MARGIN(CM) = 100. * &
                              (CM_CAPACITY_MW(CM)/CM_PEAK_MW(CM) - 1.0)
                  CM_INTERUPTIBLRESERVE_MARGIN = 100. * &
                         (CM_CAPACITY_MW(CM) - &
                                    CM_PEAK_AFTER_INTERRUPTIBLE(CM)) / &
                                CM_PEAK_AFTER_INTERRUPTIBLE(CM)
               ELSE
                  CM_RESERVE_MARGIN(CM) = 100.
               ENDIF
               TEMP_R4 = CM_RESERVE_MARGIN(CM)*0.01 + 1.0
               ICAP_DEMAND_CURVE_MULT_K = &
                                GET_ICAP_REVENUE_MULT(R_YEAR,CM,TEMP_R4)
!
! 031019. ADD AVAILABLE RPS_REVENUE AT THIS POINT TO GIVE RPS RESOURCE
!         FULL OPPORTUNITY TO RECEIVE VALUE OF REVENUE. SHOULD BE
!         AUTOMATICALLY REDUCED BELOW IF CAPACITY REVENUES ARE AVAILABLE
!
               TEMP_I2 = RPS_PROGRAM_NUMBER(CN)
               IF(ABS(TEMP_I2) > 0 .AND. CX_ITER /= 0) THEN
                  IF(R_YEAR == 7 .AND. GRX_ITERATIONS > 0) THEN
                     R_YEAR = R_YEAR
                  ENDIF
                  TEMP_R4 = ANNUAL_ENERGY * RPS_PERCENT(CN)
                  RPS_REVENUE_MM(AI) = &
                              GET_RPS_REV_BY_OPTION( &
                                                 TEMP_I2,TEMP_R4,R_YEAR)
               ELSE
                  RPS_REVENUE_MM(AI) = 0.0
               ENDIF
               MARKET_REVENUE(CN) = ANNUAL_ENERGY_REVENUE(CN) + &
                                            1000000.0*RPS_REVENUE_MM(AI)
               MARKET_COST(CN) = ANNUAL_FIXED_COST(CN) + &
                                               ANNUAL_VARIABLE_COST(CN)
               IF(ANNUAL_ENERGY > 0.0001) THEN
                  ENERGY_REVENUE_PER_MWH(CN) = &
                                       ANNUAL_ENERGY_REVENUE(CN)/ &
                                                         ANNUAL_ENERGY
                  MARKET_AVE_REVENUE = MARKET_REVENUE(CN) / &
                                                         ANNUAL_ENERGY
                  TOTAL_AVERAGE_COST = MARKET_COST(CN) / &
                                                         ANNUAL_ENERGY
               ELSE
                  ENERGY_REVENUE_PER_MWH(CN) = 0.0
                  MARKET_AVE_REVENUE = 0.0
                  TOTAL_AVERAGE_COST = 0.0
               ENDIF
               NET_MARGIN(CN) = MARKET_AVE_REVENUE - TOTAL_AVERAGE_COST
               HIGHEST_FUEL_COST_PER_MMBTU(AI) = &
                                     FUEL_COST_PER_MMBTU(CN)
               HIGHEST_VAR_OM_PER_MWH(AI) = &
                                     VAR_OM_PER_MWH(CN)
               HIGHEST_EMISSIONS_PER_MWH(AI) = &
                                       EMISSIONS_PER_MWH(CN)
!
               IF(SCREEN_CAPACITY(CN) > 0.) THEN
                  NET_MARGIN_PER_KWYR(CN) = &
                      .001*(MARKET_REVENUE(CN) - MARKET_COST(CN)) / &
                                                     SCREEN_CAPACITY(CN)
                  IF(CX_ITER == 0) THEN
                     CM_MAX_NET_ENRG_MARG_PER_KWYR(CM) = &
                        MAX(CM_MAX_NET_ENRG_MARG_PER_KWYR(CM), 0.001 * &
                       (ANNUAL_ENERGY_REVENUE(CN) - MARKET_COST(CN)) / &
                                                    SCREEN_CAPACITY(CN))
                  ENDIF
               ELSE
                  NET_MARGIN_PER_KWYR(CN) = -999999.0
               ENDIF
!
               VARIABLE_COST_MM(AI) = 0.000001 * &
                                               ANNUAL_VARIABLE_COST(CN)
               FIXED_COST_MM(AI) = 0.000001 * &
                                    ANNUAL_FIXED_COST(CN)
               HIGHEST_FOM_COST_MM(AI) = 0.000001 * FIX_OM_COST(CN)
               HIGHEST_ANN_LEVEL_CAP(AI) = ANNUAL_LEVELIZED_CAPITAL(CN)
               ENERGY_REVENUE_MM(AI) = 0.000001 * &
                                               ANNUAL_ENERGY_REVENUE(CN)
!
               HIGHEST_MARKET_REVENUE(AI) = ENERGY_REVENUE_MM(AI)
               HIGHEST_ENERGY_REV_PER_MWH(AI) = &
                                              ENERGY_REVENUE_PER_MWH(CN)
               HIGHEST_MRX_ENERGY(AI) = ANNUAL_ENERGY * 0.001
               HIGHEST_SCREEN_CAPACITY(AI) = SCREEN_CAPACITY(CN)
               HIGHEST_MRX_CF(AI) = CF
               HIGHEST_FUEL_COST(AI) = &
                              ANNUAL_FUEL_COST(CN)/1000000.0
               HIGHEST_VOM_COST_MM(AI) = 0.000001 * ANNUAL_VOM_COST
!
               HIGHEST_EMISSIONS_COST_MM(AI) = 0.000001 * &
                                                        ANNUAL_EMIS_COST
               HIGHEST_HEAT_RATE(AI) = &
                                       HEAT_RATE(CN)
               IF(ANNUAL_ENERGY > 0.01) THEN
                  HIGHEST_FOM_PER_MWH(AI) = FIX_OM_COST(CN)/ &
                                                  MAX(.01,ANNUAL_ENERGY)
                  HIGHEST_LEVEL_CAP_PER_MWH(AI) = &
                           ANNUAL_LEVELIZED_CAPITAL(CN) * 1000000.0 / &
                                                  MAX(.01,ANNUAL_ENERGY)
                 HIGHEST_FUEL_COST_PER_MWH(AI) = ANNUAL_FUEL_COST(CN)/ &
                                                  MAX(.01,ANNUAL_ENERGY)
                  FIXED_COST_PER_MWH(AI) = &
                              HIGHEST_LEVEL_CAP_PER_MWH(AI) + &
                                          HIGHEST_FOM_PER_MWH(AI)
                  REVENUE_PER_MWH(AI) = ANNUAL_ENERGY_REVENUE(CN)/ &
                                                  MAX(.01,ANNUAL_ENERGY)
               ELSE
                  HIGHEST_FUEL_COST_PER_MWH(AI) = AVERAGE_FUEL_PRICE(CN)
               ENDIF
               HIGHEST_STRIKE_PRICE(AI) = &
                        HIGHEST_FUEL_COST_PER_MWH(AI) + &
                              HIGHEST_VAR_OM_PER_MWH(AI) + &
                                     HIGHEST_EMISSIONS_PER_MWH(AI)
               HIGHEST_RPM(AI) = RPM(CN)
               HIGHEST_RFT(AI) = RFT(CN)
               HIGHEST_ANNUAL_UNITS(AI) =  FLOAT(LOCAL_ANNUAL_UNITS(CN))
               HIGHEST_MRX_MARGIN(AI) = NET_MARGIN(CN)
               HIGHEST_MARGIN_PER_KWYR(AI) = NET_MARGIN_PER_KWYR(CN)
!
               IF(CX_ITER /= 0) THEN
                  IF(PLANNING_PER_MWH) THEN
                     HGST_NET_MARG_ADDED(AI) = NET_MARGIN(CN)
                  ELSE
                     HGST_NET_MARG_ADDED(AI) = &
                                                 NET_MARGIN_PER_KWYR(CN)
                  ENDIF
               ENDIF
!
               HGST_OPT_INDEX(AI) = CN
!
               IF(CX_ACTIVE .AND. CX_ITER > 0) THEN ! .AND.
                  IF(TG == 0) THEN
                     TEMP_PEAK =  UPDATE_NET_PLANNING_PEAK(R_YEAR)
                  ELSE
                     TEMP_PEAK = TG_PEAK_MW(L) 
                     ! GET_GROUP_PEAK_ON_PEAK_MONTH(L)
                  ENDIF
                  TEMP_L = CX_ALTER_MX_CHRONO_PRICES( &
                                    TG_PLANNING_CAPACITY(L), &
                                    LOCAL_CHRONO_MW_USAGE, &
                                    LOCAL_CHRONO_STRIKE_PRICE, &
                                    LOCAL_CHRONO_MARKET_PRICES(1,L), &
                                    TEMP_PEAK)
               ELSE ! PROPAGATE SOLUTION TO OTHER INSTANCES OF SAME CN
                  DO J = 1, TOTAL_ACTIVE_INSTANCES
                     K = ACTIVE_INSTANCE(J)
                     IF(K == AO) THEN
                        IF(PLANNING_PER_MWH) THEN
                           HGST_NET_MARG_ADDED(J) = NET_MARGIN(CN)
                        ELSE
                           HGST_NET_MARG_ADDED(J) = &
                                                 NET_MARGIN_PER_KWYR(CN)
                        ENDIF
                     ENDIF
                  ENDDO
               ENDIF
            ENDDO ! OPTIONS/INSTANCES
            CALL SortIncrPos(TOTAL_ACTIVE_INSTANCES, &
                             SORTED_OPTIONS, &
                             HGST_NET_MARG_ADDED)
            IF(CX_ITER > 0) THEN
               OPTIMAL_ORDER = .TRUE.
               DO J = 1, TOTAL_ACTIVE_INSTANCES-1
                  IF(SORTED_OPTIONS(J) > SORTED_OPTIONS(J+1)) THEN
                     CYCLE
                  ELSE
                     OPTIMAL_ORDER = .FALSE.
                     EXIT
                  ENDIF
               ENDDO
! RESTORE SORTED_OPTIONS BACK TO POSITION AND INSTANCE
               TEMP_INSTANCE = SORTED_OPTIONS
               DO AI = TOTAL_ACTIVE_INSTANCES, 1, -1
                  M = TEMP_INSTANCE(AI)
                  K = TOTAL_ACTIVE_INSTANCES - M + 1
                  J = LAST_SORT_INSTANCE(K)
                  SORTED_OPTIONS(AI) = J
!
                  AO = ACTIVE_INSTANCE(J)
                  CN = ACTIVE_OPTION(AO)
                  HGST_OPT_INDEX(M) = CN
!
                  TG = TRANSACTION_GROUP(CN)
                  L = GET_TRANS_GROUP_POSITION(TG)
                  CM = GET_CM_FROM_TG(L)
                  CM_INDEX = GET_CM_INDEX_FROM_TG(CM)
                  PA = TG_2_PLANNING_AREA(L)
!
                  IF(MX_SUMMARY_REPORT .AND. &
                           (CX_ITER == MAX_CX_ITER - 1 .OR. &
                                                   OPTIMAL_ORDER) ) THEN
!
                     IF(TG_MW_TRANSFERS) THEN
                        TG_CAPACITY_MW(L) = &
                           TG_PLANNING_CAPACITY(L) + &
                                 TG_TOTAL_ANNUAL_CAPACITY_ADDED(L) + &
                                    TG_Net_Transfer(L)
                     ELSE
                        TG_CAPACITY_MW(L) = &
                           TG_PLANNING_CAPACITY(L) + &
                                 TG_TOTAL_ANNUAL_CAPACITY_ADDED(L)! +
                     ENDIF
                     IF(TG_PEAK_MW(L) > .01) THEN
                        TG_RESERVE_MARGIN(L) = 100. * &
                              (TG_CAPACITY_MW(L)/TG_PEAK_MW(L) - 1.0)
                     ELSE
                        TG_RESERVE_MARGIN(L) = 100.
                     ENDIF
                     IF(TG_MW_TRANSFERS) THEN
                        CM_CAPACITY_MW(CM) = &
                           CM_PLANNING_CAPACITY(CM) + &
                                 TG_TOTAL_ANNUAL_CAPACITY_ADDED(L)  + &
                                    CM_Net_Transfer(CM)
                     ELSE
                        CM_CAPACITY_MW(CM) = &
                           CM_PLANNING_CAPACITY(CM) + &
                                 TG_TOTAL_ANNUAL_CAPACITY_ADDED(L) ! +
                     ENDIF
                     IF(CM_PEAK_MW(CM) > .01) THEN
                        CM_RESERVE_MARGIN(CM) = 100. * &
                              (CM_CAPACITY_MW(CM)/CM_PEAK_MW(CM) - 1.0)

                        CM_INTERUPTIBLRESERVE_MARGIN = 100. * &
                         (CM_CAPACITY_MW(CM) - &
                                    CM_PEAK_AFTER_INTERRUPTIBLE(CM)) / &
                                CM_PEAK_AFTER_INTERRUPTIBLE(CM)
                     ELSE
                        CM_RESERVE_MARGIN(CM) = 100.
                     ENDIF
                     TEMP_R4 = CM_RESERVE_MARGIN(CM)*0.01 + 1.0
                     TEMP_R4 = CM_INTERUPTIBLRESERVE_MARGIN*0.01 + 1.0
!
                     ICAP_DEMAND_CURVE_MULT(M) = &
                                GET_ICAP_REVENUE_MULT(R_YEAR,CM,TEMP_R4)
!
                     IF(SCREEN_CAPACITY(CN) > 0.001) THEN
!
                        IF(ICAP_SWITCH_INDEX(CM) == 2) THEN
                           CONE_COST_PER_KWYR(M) = &
                                    LOWEST_FIXED_COST_BY_CM(CM)
                        ELSE
                           CONE_COST_PER_KWYR(M) = ICAP_CONE_PRICE(CM)
                        ENDIF
                        CONE_DEMAND_CURVE_ADJ_PER_KWYR(M) = &
                                    CONE_COST_PER_KWYR(M) * &
                                    ICAP_DEMAND_CURVE_MULT(M)

                        IF(ICAP_SWITCH_INDEX(CM) == 2 .OR.   & !MARGINAL
                                        ICAP_SWITCH_INDEX(CM) == 3) THEN
                                        ! CONE-MRX
                           CAPACITY_REVENUE_MM(M) = &
                              MIN(CONE_DEMAND_CURVE_ADJ_PER_KWYR(M) * &
                                  CAPACITY_PLANNING_MW(CN)*0.001, &
                                MAX(0., &
                                 (VARIABLE_COST_MM(M) + &
                                  FIXED_COST_MM(M) - &
                                       RPS_REVENUE_MM(M) - &
                                               ENERGY_REVENUE_MM(M))))
                           ICAP_REVENUE_PER_KWYR(M) = &
                              MAX(0.0,CAPACITY_REVENUE_MM(M)*1000.0/ &
                                                    SCREEN_CAPACITY(CN))
                        ELSE
                           ICAP_REVENUE_PER_KWYR(M) = &
                                 MIN(ICAP_CONE_PRICE(CM), &
                                     CONE_DEMAND_CURVE_ADJ_PER_KWYR(M))
                           CAPACITY_REVENUE_MM(M) = &
                              ICAP_REVENUE_PER_KWYR(M) * &
                                 CAPACITY_PLANNING_MW(CN) * 0.001
                        ENDIF
                        EAS_PROFIT_PER_KWYR(M) = &
                                          HIGHEST_EAS_PROFIT_PER_KWYR
                        EAS_PROFIT_PER_KWYR(M) = MAX(0.0, &
                                  CONE_DEMAND_CURVE_ADJ_PER_KWYR(M) - &
                                              ICAP_REVENUE_PER_KWYR(M))
                        HIGHEST_MARKET_REVENUE(M) = &
                                    HIGHEST_MARKET_REVENUE(M) + &
                                          RPS_REVENUE_MM(M) + &
                                                  CAPACITY_REVENUE_MM(M)
                        HIGHEST_MARGIN_PER_KWYR(M) = 1000.0 * &
                              (HIGHEST_MARKET_REVENUE(M) - &
                                 VARIABLE_COST_MM(M) - &
                                       FIXED_COST_MM(M))/ &
                                          SCREEN_CAPACITY(CN)
                     ELSE
                        EAS_PROFIT_PER_KWYR(M) = 0.0
                        CONE_COST_PER_KWYR(M) = 0.0
                        CONE_DEMAND_CURVE_ADJ_PER_KWYR(M) = 0.0
                        ICAP_REVENUE_PER_KWYR(M) = 0.0
                        CAPACITY_REVENUE_MM(M) = 0.0
                        CAPACITY_REVENUE_PER_MWH(M) = 0.0
                        HIGHEST_MARGIN_PER_KWYR(M) = 0.0
                     ENDIF
                     TEMP_R = HIGHEST_MRX_ENERGY(M) * &
                                       RPS_PERCENT(CN)
                     IF(TEMP_R > 0.0) THEN
                        RPS_REVENUE_PER_MWH(M) = &
                              1000.0 * RPS_REVENUE_MM(M) / TEMP_R
                     ELSE
                        RPS_REVENUE_PER_MWH(M) = 0.0
                     ENDIF
                     IF(HIGHEST_MRX_ENERGY(M) > 0.01) THEN
                        CAPACITY_REVENUE_PER_MWH(M) = &
                            CAPACITY_REVENUE_MM(M)*1000.0/ &
                                          HIGHEST_MRX_ENERGY(M)
                        HIGHEST_MRX_MARGIN(M) = 1000.0 * &
                              (HIGHEST_MARKET_REVENUE(M) - &
                                 VARIABLE_COST_MM(M) - &
                                       FIXED_COST_MM(M))/ &
                                          HIGHEST_MRX_ENERGY(M)
                        REVENUE_PER_MWH(M) = 1000.0 * &
                                   HIGHEST_MARKET_REVENUE(M)/ &
                                          HIGHEST_MRX_ENERGY(M)
                     ELSE
                        CAPACITY_REVENUE_PER_MWH(M) = 0.0
                        HIGHEST_MRX_MARGIN(M) = -999999.
                        REVENUE_PER_MWH(M) = 0.0
                     ENDIF
! 081316. END INSERT.
                     LOCAL_ANNUAL_UNITS(CN) = LOCAL_ANNUAL_UNITS(CN) - 1
                     HIGHEST_ANNUAL_UNITS(M) = &
                               FLOAT(LOCAL_ANNUAL_UNITS(CN))
                     MRX_ICAP_INDEX(M) = 0
                     WRITE(LOCAL_NAME,1010) OPTION_NAME(CN)(1:17),AI
                     MX_ANNUAL_ALT_REC = RPTREC(MX_ANNUAL_ALT_NO)
!
                     WRITE(MX_ANNUAL_ALT_NO,REC=MX_ANNUAL_ALT_REC) &
                        PRT_ENDPOINT(), &
                        FLOAT(THIS_YEAR), &
                        MULTI_AREA_NAME(L), &
                        LOCAL_NAME, &
                        HIGHEST_STRIKE_PRICE(M), &
                        HIGHEST_ENERGY_REV_PER_MWH(M), & 
                        !  111509. REDEFINED.
                        HIGHEST_MRX_MARGIN(M), &
                        HIGHEST_MRX_CF(M), &
                        ENERGY_REVENUE_MM(M), &
                        HIGHEST_MARKET_COST(M), & !  5
                        VARIABLE_COST_MM(M), &
                        FIXED_COST_MM(M), &
                        SCREEN_CAPACITY(CN), &
                        HIGHEST_MRX_ENERGY(M), & !  10
                        TOTAL_AVERAGE_COST, & !  NEED DEFINITION
                        HIGHEST_ANN_LEVEL_CAP(M), &
                        HIGHEST_FUEL_COST(M), &
                        HIGHEST_ANNUAL_UNITS(M), &
                        CAPACITY_REVENUE_MM(M), &
                        HIGHEST_MARKET_REVENUE(M), & !  16 TOTAL REVENUE
                        HIGHEST_MARGIN_PER_KWYR(M), &
                        HIGHEST_FUEL_COST_PER_MWH(M), &
                        HIGHEST_VAR_OM_PER_MWH(M), & !  19
                        HIGHEST_FUEL_COST_PER_MMBTU(M), &
                        REVENUE_PER_MWH(M), &
                        CAPACITY_REVENUE_PER_MWH(M), &
                        HIGHEST_EMISSIONS_PER_MWH(M), &
                        HIGHEST_FOM_PER_MWH(M), & !  24
                        HIGHEST_LEVEL_CAP_PER_MWH(M), &
                        HIGHEST_VOM_COST_MM(M), &
                        HIGHEST_EMISSIONS_COST_MM(M), & !  27
                        HIGHEST_FOM_COST_MM(M), &
                        FLOAT(TG), &
                        TG_CAPACITY_MW(L), &
                        TG_PEAK_MW(L), &
                        TG_RESERVE_MARGIN(L), & !  32
                        FLOAT(CM_INDEX), &
                        CM_CAPACITY_MW(CM), &
                        CM_PEAK_MW(CM), &
                        CM_RESERVE_MARGIN(CM), &
                        ICAP_DEMAND_CURVE_MULT(M), &
                        CONE_COST_PER_KWYR(M), & !  38
                        CONE_DEMAND_CURVE_ADJ_PER_KWYR(M), &
                        EAS_PROFIT_PER_KWYR(M), &
                        ICAP_REVENUE_PER_KWYR(M) , &
                        HIGHEST_HEAT_RATE(M), & !  42
                        FIXED_COST_PER_MWH(M), &
                        HIGHEST_RFT(M), &
                        HIGHEST_RPM(M), &
                        TG_Net_Transfer(L), &
                        CM_Net_Transfer(CM), &
                        MRX_ICAP_INDEX(M), &
                        CM_INTERUPTIBLRESERVE_MARGIN, &
                        CM_PEAK_AFTER_INTERRUPTIBLE(CM), &
                        CAPACITY_PLANNING_MW(CN), &
                        RPS_REVENUE_MM(M), &
                        RPS_REVENUE_PER_MWH(M)
                     MX_ANNUAL_ALT_REC = MX_ANNUAL_ALT_REC + 1
! 081316.
                     TOTAL_ANNUAL_CAPACITY_ADDED(PA) = &
                           TOTAL_ANNUAL_CAPACITY_ADDED(PA) + &
                                    CAPACITY_PLANNING_MW(CN)
                     CM_TOT_ANN_CAPACITY_ADDED(CM) = &
                           CM_TOT_ANN_CAPACITY_ADDED(CM) + &
                                    CAPACITY_PLANNING_MW(CN)
                     TG_TOTAL_ANNUAL_CAPACITY_ADDED(L) = &
                           TG_TOTAL_ANNUAL_CAPACITY_ADDED(L) + &
                                    CAPACITY_PLANNING_MW(CN)
!
                  ENDIF ! IF REPORT AND OPTIMAL_ORDER
               ENDDO
            ENDIF
            LAST_SORT_INSTANCE = SORTED_OPTIONS
            LOCAL_CHRONO_MARKET_PRICES = RESET_CHRONO_MARKET_PRICES
!
! EVALUATE LIST. COMPARE WITH PREVIOUS ITERATION
!
            CX_ITER = CX_ITER + 1
         ENDDO ! ITERATE ON ACTIVE INSTANCE SOLUTION
         WRITE(9,*) 'CX_ITER = ',CX_ITER
         IF(.NOT. OPTIMAL_ORDER) THEN
            WRITE(9,*) 'NOT OPTIMAL ORDER IN ',THIS_YEAR,' AND ITER ', &
                                                          GRX_ITERATIONS
         ENDIF
!
 1234    CONTINUE
         HO = TOTAL_ACTIVE_INSTANCES
         IF(CX_ACTIVE) GOTO 5678
         DO L = 1, UPPER_TRANS_GROUP
!
            TG = GET_TRANS_GROUP_INDEX(L)
!
            PA = TG_2_PLANNING_AREA(L) ! FOR ICAP REVENUE
            CM = TG_2_CAPACITY_MARKET(L) ! FOR ICAP REVENUE
            CM_INDEX = GET_CM_INDEX_FROM_TG(CM)
            IOTA = 0.0
!
            HIGHEST_OPTION = -1
!
! GET THE INITIAL MARKET DURATION CURVE. DO NOT CALL THIS ROUTINE AGAIN.
!
            IF(BUILD_TO_MARKET) THEN
               TRANS_GROUP_4_PRICING = L
               CALL MX_USER_PRICE_CURVE(      L, &
                                              HIGHEST_INTERVAL, &
                                              MARKET_DURATION, &
                                              MARKET_CUM_REVENUE, &
                                              MARKET_PRICE, &
                                              INT(13,2))
            ELSE
               TRANS_GROUP_4_PRICING = 0
               CALL MX_PRICE_CURVE( &
                                              HIGHEST_INTERVAL, &
                                              MARKET_DURATION, &
                                              MARKET_CUM_REVENUE, &
                                              MARKET_PRICE, &
                                              INT(13,2))
            ENDIF
!
            IF(CX_ACTIVE) THEN
               CALL CX_TRANS_ANNUAL_USER_MARKET( &
                                      L,LOCAL_CHRONO_MARKET_PRICES(1,L))

            ENDIF
!
! AT THIS POINT, I CANNOT GET BACK TO A CUM DISTN FUNCTION ON REV AND 
! PRICE
            K = 1
            I = 1
!
            ANNUAL_RESOURCES_AVAILABLE = .TRUE.
!
            PA_CAPACITY_MW = 0.0
            CM_CAPACITY_MW = 0.0
!
! 9/10/02. REVERT BACK TO PREVIOUS LOGIC IF NOT REGIONAL. OTHERWISE,
!           ENSURE THAT RESOURCES ARE TESTED IN ALL REGIONS
!
            IF(USE_REGIONAL_MINIMUM_RM_LOGIC) THEN
               MAX_RESOURCES_PER_GROUP = &
                        MAX_RESOURCES_PER_GROUP + &
                                MAX_RESOURCES_PER_YEAR_GROUP
            ELSE
               MAX_RESOURCES_PER_GROUP = MX_RES_P_YEAR
            ENDIF
!
            FIRST_CF = -1.0
!
            WRITE(4,*) "MAX_RESOURCES_PER_GROUP",MAX_RESOURCES_PER_GROUP
            DO WHILE(HO < MAX_RESOURCES_PER_GROUP .AND. &
                           ANNUAL_RESOURCES_AVAILABLE .AND. &
                         (HIGHEST_NET_MARGIN > MINIMUM_NET_MARGIN .OR. &
                                             USE_MINIMUM_RM_LOGIC .OR. &
                                    USE_REGIONAL_MINIMUM_RM_LOGIC .OR. &
                                                                K == 1))
!
               IF(HO / MX_RES_P_YEAR > &
                                             L / UPPER_TRANS_GROUP) THEN
                  WRITE(4,*) "MAX RESOURCES HIT IN MRX."
                  WRITE(4,*) "RESOURCES TESTED K AND PASSED HO",K,HO
                  WRITE(4,*) "TRANSACTION GROUP ",MULTI_AREA_NAME(L)
                  EXIT
               ELSE
                  LAST_HO = HO
               ENDIF
!
               HIGHEST_CHRONO_MW_USAGE = 0.0
               ANNUAL_ENERGY = 0.
               HIGHEST_NET_MARGIN = -999999.
               HIGHEST_ICAP_REVENUE = -999999.
               HIGHEST_ENERGY_REVENUE = -999999.
               HIGHEST_NET_MARGIN_PER_MWH = -999999.
               HIGHEST_NET_MARGIN_PER_KWYR = -999999.
               HIGHEST_ICAP_DEMAND_CURVE_MULT = 0.0
               HIGHEST_EAS_PROFIT_PER_KWYR = 0.0
               HIGHEST_CONE_COST_PER_KWYR = 0.0
               HIGHEST_CONE_DEM_ADJ_PER_KWYR = 0.0
               HIGHEST_ICAP_REVENUE_PER_KWYR = 0.0
               HIGHEST_CM_CAPACITY_MW = 0.0
               HIGHEST_CM_RESERVE_MARGIN = 0.0
!
               LOCAL_RESOURCES_AVAILABLE = .FALSE.
!
               DO CN = 1, TOTAL_ALL_OPTIONS
!
                  I = OPTION_POSITION(CN)
!
! 08/19/04. ADDED DEPENDENT CONDITION FOR PAC CORP DUCT FIRING.
!
                  IF( .NOT. RESOURCE_AVAILABLE(I, &
                                    THIS_YEAR_PLUS_LEAD_TIME(CN)) .OR. &
                      .NOT. TEST_START_STOP_YEARS(I, &
                                    THIS_YEAR_PLUS_LEAD_TIME(CN)) .OR. &
                                 TRANSACTION_GROUP(CN) /= TG  .OR. &
                                       LOCAL_ANNUAL_UNITS(CN) < 1 .OR. &
                               .NOT. DEPENDENT_UNIT_AVAILABLE(CN)) CYCLE
!
                  NET_MARGIN(CN) = -999999.
                  NET_MARGIN_PER_KWYR(CN) = -999999.
                  NET_MARGIN_INTERVAL(CN) = -1
!
                  DO J = HIGHEST_INTERVAL, 1, -1
                     IF(MARKET_PRICE(J) < STRIKE_PRICE(CN,MO)) EXIT
                  ENDDO
!
                  LOCAL_RESOURCES_AVAILABLE = .TRUE.
                  IF(J < HIGHEST_INTERVAL) THEN
!
                     IF(J == 0) THEN
                        TOP_CAP_PERCENT = 0.0
                        J = 1
                     ELSEIF(MARKET_PRICE(J+1)  - &
                                 MARKET_PRICE(J) /= 0.0) THEN
                        TOP_CAP_PERCENT = &
                           (STRIKE_PRICE(CN,MO) - MARKET_PRICE(J)) / &
                                   (MARKET_PRICE(J+1) - MARKET_PRICE(J))
                     ELSE
                        TOP_CAP_PERCENT = 0.0
                     ENDIF

                     MARKET_HOURS = &
                        (TOP_CAP_PERCENT * MARKET_DURATION(J+1) + &
                        (1.-TOP_CAP_PERCENT) * MARKET_DURATION(J))

!
! ASSUMES 8784 HOURS PER YEAR.
!
                     IF(SCREEN_CAPACITY(CN) > 0.0 .AND. &
                                               FIRST_CF(CN) < -0.5) THEN
                        ANNUAL_ENERGY = MARKET_HOURS * &
                                                 EQUIVALENT_CAPACITY(CN)
                        FIRST_CF(CN) = ANNUAL_ENERGY / &
                                             (SCREEN_CAPACITY(CN)*87.84)
                     ENDIF
!
                     IF(GRX_UNIT_LINK_ACTIVE) THEN
                        U = GRX_RESOURCE_LINK_ID(CN)
                        IF(U > 0) THEN
                           U = GET_RESOURCE_ID_TO_UNIT(U,INT2_ONE)
                        ENDIF
                        IF(U > 0) THEN
                           IF(ANNUAL_CL_UNIT_CAPACITY(U) > 0.0001) THEN
!
                              ANNUAL_ENERGY = MARKET_HOURS * &
                                                 EQUIVALENT_CAPACITY(CN)
                              IF(MARKET_HOURS < -0.001) THEN
                                WRITE(4,*) 'MARKET HOURS WENT NEGATIVE'
                         er_message="Stop requested from CAP OBJTSIID6"
                                call end_program(er_message)
                              ENDIF
                              CF = ANNUAL_ENERGY / &
                                             (SCREEN_CAPACITY(CN)*87.84)
                              IF(FIRST_CF(CN) - CF > 0.001) THEN
                                 CF_FACTOR = MIN(1.0, &
                                  1.0 - (FIRST_CF(CN) - CF)*.01 )
                              ELSE
                                 CF_FACTOR = 1.0
                              ENDIF
!
                              TEMP_R = CF_FACTOR * &
                                 EQUIVALENT_CAPACITY(CN) / &
                                             ANNUAL_CL_UNIT_CAPACITY(U)
                              ANNUAL_ENERGY = &
                                 ANNUAL_CL_UNIT_ENERGY(U) * TEMP_R
                              ANNUAL_ENERGY_REVENUE = &
                                 ECO_SALES_REV_FROM(U) * TEMP_R
                              IF(ANNUAL_ENERGY == 0.) THEN
                                 ECO_SALES_REV_FROM(U) = 0.
                              ENDIF
                              ANNUAL_ENERGY_REVENUE = &
                                 ECO_SALES_REV_FROM(U) * TEMP_R
                           ELSE
                              ANNUAL_ENERGY = MARKET_HOURS * &
                                                 EQUIVALENT_CAPACITY(CN)
                              ANNUAL_ENERGY_REVENUE = &
                                    (TOP_CAP_PERCENT * &
                                             MARKET_CUM_REVENUE(J+1) + &
                                    (1 -TOP_CAP_PERCENT) * &
                                              MARKET_CUM_REVENUE(J)) * &
                                               EQUIVALENT_CAPACITY(CN)
                           ENDIF
                        ELSE
                           ANNUAL_ENERGY = MARKET_HOURS * &
                                                 EQUIVALENT_CAPACITY(CN)
                           ANNUAL_ENERGY_REVENUE = &
                              (TOP_CAP_PERCENT * &
                                             MARKET_CUM_REVENUE(J+1) + &
                              (1.-TOP_CAP_PERCENT) * &
                                              MARKET_CUM_REVENUE(J)) * &
                                               EQUIVALENT_CAPACITY(CN)
                        ENDIF
                     ELSE
                        ANNUAL_ENERGY = MARKET_HOURS * &
                                                 EQUIVALENT_CAPACITY(CN)
                        ANNUAL_ENERGY_REVENUE = &
                              (TOP_CAP_PERCENT * &
                                             MARKET_CUM_REVENUE(J+1) + &
                              (1.-TOP_CAP_PERCENT) * &
                                              MARKET_CUM_REVENUE(J)) * &
                                               EQUIVALENT_CAPACITY(CN)
                     ENDIF
                     IF(SCREEN_CAPACITY(CN) > 0.0) THEN
                        CF = ANNUAL_ENERGY /(SCREEN_CAPACITY(CN)*87.84)
                     ELSE
                        CF = 0.0
                     ENDIF

                     IF(ANNUAL_ENERGY > 0.) THEN
                        ENERGY_REVENUE_PER_MWH(CN) = &
                                       ANNUAL_ENERGY_REVENUE(CN)/ &
                                                  MAX(.01,ANNUAL_ENERGY)
                     ELSE
                        ENERGY_REVENUE_PER_MWH(CN) = 0.
                     ENDIF
!
                     IF( ANNUAL_ENERGY < -.0001) THEN
                        ANNUAL_ENERGY = ANNUAL_ENERGY
                        WRITE(4,*) 'ANNUAL ENERGY WHEN NEGATIVE'
                        er_message='Stop requested from CAP_OBJT SIID7'
                        call end_program(er_message)
                     END IF
!
                     ANNUAL_VARIABLE_COST(CN) = STRIKE_PRICE(CN,MO) * &
                                                           ANNUAL_ENERGY
!
                     IF(TG_MW_TRANSFERS) THEN
                        CM_CAPACITY_MW(CM) = &
                           CM_PLANNING_CAPACITY(CM) + &
                                 TG_TOTAL_ANNUAL_CAPACITY_ADDED(L)  + &
                                    CM_Net_Transfer(CM)
                     ELSE
                        CM_CAPACITY_MW(CM) = &
                           CM_PLANNING_CAPACITY(CM) + &
                                 TG_TOTAL_ANNUAL_CAPACITY_ADDED(L)
                     ENDIF

                     IF(CM_PEAK_MW(CM) > .01) THEN
                        CM_RESERVE_MARGIN(CM) = 100. * &
                              (CM_CAPACITY_MW(CM)/CM_PEAK_MW(CM) - 1.0)
                        CM_INTERUPTIBLRESERVE_MARGIN = 100. * &
                         (CM_CAPACITY_MW(CM) - &
                                    CM_PEAK_AFTER_INTERRUPTIBLE(CM)) / &
                                CM_PEAK_AFTER_INTERRUPTIBLE(CM)
                     ELSE
                        CM_RESERVE_MARGIN(CM) = 100.
                     ENDIF
!
                     TEMP_R4 = CM_RESERVE_MARGIN(CM)*0.01 + 1.0
                     TEMP_R4 = CM_INTERUPTIBLRESERVE_MARGIN*0.01 + 1.0
!
                     ICAP_DEMAND_CURVE_MULT_K = &
                                GET_ICAP_REVENUE_MULT(R_YEAR,CM,TEMP_R4)
                     IF(SCREEN_CAPACITY(CN) > 0.01) THEN
                        EAS_PROFIT_PER_KWYR_K = 0.001 * &
                              (ANNUAL_ENERGY_REVENUE(CN) - &
                                  ANNUAL_VARIABLE_COST(CN))/ &
                                         SCREEN_CAPACITY(CN)
                        IF(EAS_PROFIT_PER_KWYR_K < -0.0001) THEN
                           EAS_PROFIT_PER_KWYR_K = EAS_PROFIT_PER_KWYR_K
                        ENDIF
                        CONE_COST_PER_KWYR_K = 0.001 * &
                                   ANNUAL_FIXED_COST(CN)/ &
                                         SCREEN_CAPACITY(CN)
                        CONE_DEM_CURVE_ADJ_PER_KWYR_K = &
                                    CONE_COST_PER_KWYR_K * &
                                  ICAP_DEMAND_CURVE_MULT_K
                        ICAP_REVENUE_PER_KWYR_K = &
                           MAX(0.0,CONE_DEM_CURVE_ADJ_PER_KWYR_K - &
                                                EAS_PROFIT_PER_KWYR_K)
                     ELSE
                        EAS_PROFIT_PER_KWYR_K = 0.0
                        CONE_COST_PER_KWYR_K = 0.0
                        CONE_DEM_CURVE_ADJ_PER_KWYR_K = 0.0
                        ICAP_REVENUE_PER_KWYR_K = 0.0
                     ENDIF
!
                     ANNUAL_ICAP_REVENUE = 1000.0 * &
                                 ICAP_REVENUE_PER_KWYR_K * &
                                                CAPACITY_PLANNING_MW(CN)
!
                     MARKET_REVENUE(CN) = ANNUAL_ENERGY_REVENUE(CN) ! +
!
                     IF(ANNUAL_ENERGY > 0.01) THEN
                        FOM_PER_MWH(CN) = FIX_OM_COST(CN)/ &
                                                  MAX(.01,ANNUAL_ENERGY)
                        LEVELIZED_CAP_PER_MWH(CN) = &
                           ANNUAL_LEVELIZED_CAPITAL(CN) * 1000000.0 / &
                                                  MAX(.01,ANNUAL_ENERGY)
                     ENDIF
                     VOM_COST_MM(CN) = VAR_OM_PER_MWH(CN) * &
                                            MAX(.01,ANNUAL_ENERGY) * &
                                                                0.000001

                     MARKET_COST(CN) = ANNUAL_FIXED_COST(CN) + &
                                               ANNUAL_VARIABLE_COST(CN)

                     ANNUAL_FUEL_COST(CN) = &
                             AVERAGE_FUEL_PRICE(CN) * ANNUAL_ENERGY
                     EMISSIONS_COST_MM(CN) = &
                          EMISSIONS_PER_MWH(CN) * ANNUAL_ENERGY*0.000001
                     MARKET_AVE_REVENUE = MARKET_REVENUE(CN) / &
                                                  MAX(.01,ANNUAL_ENERGY)
                     TOTAL_AVERAGE_COST = MARKET_COST(CN) / &
                                                  MAX(.01,ANNUAL_ENERGY)
                     TEMP_NET_MARGIN = &
                                 MARKET_AVE_REVENUE - TOTAL_AVERAGE_COST
                     IF(EQUIVALENT_CAPACITY(CN) > 0.) THEN
                        TEMP_NET_MARGIN_PER_KWYR = &
                         .001*(MARKET_REVENUE(CN) - MARKET_COST(CN)) / &
                                                 EQUIVALENT_CAPACITY(CN)
                     ELSE
                        TEMP_NET_MARGIN_PER_KWYR = -999999.0
                     ENDIF
                  ELSE
                     ANNUAL_ENERGY_REVENUE(CN) = 0.
                        MARKET_REVENUE(CN) = 0.
!
                     MARKET_COST(CN) = ANNUAL_FIXED_COST(CN)
                     MARKET_AVE_REVENUE = 0.0
                     CF = 0.0
                     ANNUAL_ENERGY = 0.0
                     ANNUAL_FUEL_COST(CN) = 0.0
                     ANNUAL_VARIABLE_COST(CN) = 0.0
                     TOTAL_AVERAGE_COST = ANNUAL_FIXED_COST(CN)
!
                     TEMP_NET_MARGIN = -999999.0
                     IF(EQUIVALENT_CAPACITY(CN) > 0.) THEN
                        TEMP_NET_MARGIN_PER_KWYR = &
                          001*(MARKET_REVENUE(CN) - MARKET_COST(CN)) / &
                                                 EQUIVALENT_CAPACITY(CN)
                     ELSE
                        TEMP_NET_MARGIN_PER_KWYR = -999999.0
                     ENDIF
                  ENDIF
!
                  IF(TEMP_NET_MARGIN > NET_MARGIN(CN)) THEN
                     NET_MARGIN(CN) =  TEMP_NET_MARGIN
                     NET_MARGIN_INTERVAL(CN) = J
                  ENDIF
                  IF(TEMP_NET_MARGIN_PER_KWYR > &
                                           NET_MARGIN_PER_KWYR(CN)) THEN
                     NET_MARGIN_PER_KWYR(CN) =  TEMP_NET_MARGIN_PER_KWYR
                  ENDIF
!
                  IF(PLANNING_PER_MWH) THEN
                     IF(NET_MARGIN(CN) + .01 > HIGHEST_NET_MARGIN) THEN
                        IF(CX_ACTIVE) THEN
                           HIGHEST_CHRONO_MW_USAGE = &
                                         LOCAL_CHRONO_MW_USAGE
                        ENDIF
                        HIGHEST_NET_MARGIN = NET_MARGIN(CN)
                        HIGHEST_OPTION = CN
                        HIGHEST_J = J
                        HIGHEST_TOP_CAP_PERCENT = TOP_CAP_PERCENT
                        HIGHEST_ENERGY(CN) = ANNUAL_ENERGY
                        HIGHEST_CF(CN) = CF
                        HIGHEST_ICAP_REVENUE = ANNUAL_ICAP_REVENUE
                        HIGHEST_ENERGY_REVENUE = &
                                               ANNUAL_ENERGY_REVENUE(CN)
                        HIGHEST_NET_MARGIN_PER_MWH = TEMP_NET_MARGIN
                        HIGHEST_NET_MARGIN_PER_KWYR = &
                                                TEMP_NET_MARGIN_PER_KWYR
                        HIGHEST_ICAP_DEMAND_CURVE_MULT = &
                                                ICAP_DEMAND_CURVE_MULT_K
                        HIGHEST_EAS_PROFIT_PER_KWYR = &
                                                EAS_PROFIT_PER_KWYR_K
                        HIGHEST_CONE_COST_PER_KWYR = &
                                                CONE_COST_PER_KWYR_K
                        HIGHEST_CONE_DEM_ADJ_PER_KWYR = &
                                          CONE_DEM_CURVE_ADJ_PER_KWYR_K
                        HIGHEST_ICAP_REVENUE_PER_KWYR = &
                                          ICAP_REVENUE_PER_KWYR_K
                        HIGHEST_CM_CAPACITY_MW = CM_CAPACITY_MW(CM)
                        HIGHEST_CM_RESERVE_MARGIN = &
                                                   CM_RESERVE_MARGIN(CM)
                     ENDIF
                  ELSE
                     IF(NET_MARGIN_PER_KWYR(CN) + .01 > &
                                                HIGHEST_NET_MARGIN) THEN
                        IF(CX_ACTIVE) THEN
                           HIGHEST_CHRONO_MW_USAGE = &
                                         LOCAL_CHRONO_MW_USAGE
                        ENDIF
                        HIGHEST_NET_MARGIN = NET_MARGIN_PER_KWYR(CN)
                        HIGHEST_OPTION = CN
                        HIGHEST_J = J
                        HIGHEST_TOP_CAP_PERCENT = TOP_CAP_PERCENT
                        HIGHEST_ENERGY(CN) = ANNUAL_ENERGY
                        HIGHEST_CF(CN) = CF
                        HIGHEST_ICAP_REVENUE = ANNUAL_ICAP_REVENUE
                        HIGHEST_ENERGY_REVENUE = &
                                               ANNUAL_ENERGY_REVENUE(CN)
                        HIGHEST_NET_MARGIN_PER_MWH = TEMP_NET_MARGIN
                        HIGHEST_NET_MARGIN_PER_KWYR = &
                                                TEMP_NET_MARGIN_PER_KWYR
                        HIGHEST_ICAP_DEMAND_CURVE_MULT = &
                                                ICAP_DEMAND_CURVE_MULT_K
                        HIGHEST_EAS_PROFIT_PER_KWYR = &
                                                EAS_PROFIT_PER_KWYR_K
                        HIGHEST_CONE_COST_PER_KWYR = &
                                                CONE_COST_PER_KWYR_K
                        HIGHEST_CONE_DEM_ADJ_PER_KWYR = &
                                          CONE_DEM_CURVE_ADJ_PER_KWYR_K
                        HIGHEST_ICAP_REVENUE_PER_KWYR = &
                                          ICAP_REVENUE_PER_KWYR_K
                        HIGHEST_CM_CAPACITY_MW = CM_CAPACITY_MW(CM)
                        HIGHEST_CM_RESERVE_MARGIN = &
                                                   CM_RESERVE_MARGIN(CM)
                     ENDIF
                  ENDIF
!
                  K = K + 1
!
               ENDDO ! OPTIONS
!
               IF( (RUN_MULTIAREA .OR. RUN_PRICE_MODE .OR. &
                                            RUN_AREA_PRICE_MODE) .AND. &
                     HO < MAX_RESOURCES_PER_GROUP .AND. &
                         ( (HIGHEST_NET_MARGIN > MINIMUM_NET_MARGIN &
                         .AND. HIGHEST_ENERGY(HIGHEST_OPTION)> 0 ).OR. &
                              USE_MINIMUM_RM_LOGIC .OR. &
                              USE_REGIONAL_MINIMUM_RM_LOGIC) .AND. &
                                       LOCAL_RESOURCES_AVAILABLE .AND. &
                                                HIGHEST_OPTION > 0) THEN
!
                  LOCAL_ANNUAL_UNITS(HIGHEST_OPTION) = &
                                  LOCAL_ANNUAL_UNITS(HIGHEST_OPTION) - 1
!
                  HO = HO + 1
                  IF( HO > MX_RES_P_YEAR) THEN
                     WRITE(4,*) "In the MRX Resource Expansion"
                     WRITE(4,*) "Algorithm, more than ", &
                                 MX_RES_P_YEAR," resources"
                     WRITE(4,*) "were added to meet total system"
                     WRITE(4,*) "market conditions."
                     HO = MX_RES_P_YEAR
                  ENDIF
!
                  HGST_OPT_INDEX(HO) = HIGHEST_OPTION
                  HGST_NET_MARG_ADDED(HO)  = HIGHEST_NET_MARGIN
                  SORTED_OPTIONS(HO) = HO
!
                  HIGHEST_RFT(HO) = RFT(HIGHEST_OPTION)
                  HIGHEST_RPM(HO) = RPM(HIGHEST_OPTION)
!
                  IF(MX_SUMMARY_REPORT) THEN
!
                     WRITE(LOCAL_NAME,1010) &
                                    OPTION_NAME(HIGHEST_OPTION)(1:17),HO
 1017                FORMAT(A,A,I4)
 1010                FORMAT(A,I4)
!
                     HIGHEST_STRIKE_PRICE(HO) = &
                                         STRIKE_PRICE(HIGHEST_OPTION,MO)
                     HIGHEST_ENERGY_REV_PER_MWH(HO) = &
                                  ENERGY_REVENUE_PER_MWH(HIGHEST_OPTION)
                     HIGHEST_FUEL_COST_PER_MWH(HO) = &
                                      AVERAGE_FUEL_PRICE(HIGHEST_OPTION)
                     HIGHEST_MRX_ENERGY(HO) = &
                                   HIGHEST_ENERGY(HIGHEST_OPTION)/1000.0
                     IF(HIGHEST_MRX_ENERGY(HO) > 0.) THEN
                        MARKET_AVE_REVENUE = &
                              MARKET_REVENUE(HIGHEST_OPTION) / &
                                          HIGHEST_MRX_ENERGY(HO)
                        TOTAL_AVERAGE_COST = &
                              MARKET_COST(HIGHEST_OPTION) / &
                                          HIGHEST_MRX_ENERGY(HO)
                     ELSE
                        MARKET_AVE_REVENUE = 0.
                        TOTAL_AVERAGE_COST = 0.
                     ENDIF
!
                     IF(TG_MW_TRANSFERS) THEN
                        TG_CAPACITY_MW(L) = &
                           TG_PLANNING_CAPACITY(L) + &
                                 TG_TOTAL_ANNUAL_CAPACITY_ADDED(L) + &
                                    TG_Net_Transfer(L)
                     ELSE
                        TG_CAPACITY_MW(L) = &
                           TG_PLANNING_CAPACITY(L) + &
                                 TG_TOTAL_ANNUAL_CAPACITY_ADDED(L)! +
                     ENDIF

                     IF(TG_PEAK_MW(L) > .01) THEN
                        TG_RESERVE_MARGIN(L) = 100. * &
                              (TG_CAPACITY_MW(L)/TG_PEAK_MW(L) - 1.0)
                     ELSE
                        TG_RESERVE_MARGIN(L) = 100.
                     ENDIF
!
                     PA_CAPACITY_MW(PA) = &
                           PA_PLANNING_CAPACITY(PA) + &
                                 TG_TOTAL_ANNUAL_CAPACITY_ADDED(L) ! +
                     IF(PA_PEAK_MW(PA) > .01) THEN
                        PA_RESERVE_MARGIN(PA) = 100. * &
                              (PA_CAPACITY_MW(PA)/PA_PEAK_MW(PA) - 1.0)
                     ELSE
                        PA_RESERVE_MARGIN(PA) = 100.
                     ENDIF
!
                     IF(TG_MW_TRANSFERS) THEN
                        CM_CAPACITY_MW(CM) = &
                           CM_PLANNING_CAPACITY(CM) + &
                                 TG_TOTAL_ANNUAL_CAPACITY_ADDED(L)  + &
                                    CM_Net_Transfer(CM)
                     ELSE
                        CM_CAPACITY_MW(CM) = &
                           CM_PLANNING_CAPACITY(CM) + &
                                 TG_TOTAL_ANNUAL_CAPACITY_ADDED(L) ! +
                     ENDIF

                     IF(CM_PEAK_MW(CM) > .01) THEN
                        CM_RESERVE_MARGIN(CM) = 100. * &
                              (CM_CAPACITY_MW(CM)/CM_PEAK_MW(CM) - 1.0)

                        CM_INTERUPTIBLRESERVE_MARGIN = 100. * &
                         (CM_CAPACITY_MW(CM) - &
                                    CM_PEAK_AFTER_INTERRUPTIBLE(CM)) / &
                                CM_PEAK_AFTER_INTERRUPTIBLE(CM)
                     ELSE
                        CM_RESERVE_MARGIN(CM) = 100.
                     ENDIF
                     TEMP_R4 = CM_RESERVE_MARGIN(CM)*0.01 + 1.0
                     TEMP_R4 = CM_INTERUPTIBLRESERVE_MARGIN*0.01 + 1.0
!
                     ICAP_DEMAND_CURVE_MULT(HO) = &
                                GET_ICAP_REVENUE_MULT(R_YEAR,CM,TEMP_R4)
                     IF(SCREEN_CAPACITY(HIGHEST_OPTION) > 0.01) THEN
!
                        IF(ICAP_SWITCH_INDEX(CM) == 2) THEN
                           CONE_COST_PER_KWYR(HO) = &
                                    LOWEST_FIXED_COST_BY_CM(CM)
                        ELSE
                           CONE_COST_PER_KWYR(HO) = ICAP_CONE_PRICE(CM)
                        ENDIF
                        CONE_DEMAND_CURVE_ADJ_PER_KWYR(HO) = &
                                    CONE_COST_PER_KWYR(HO) * &
                                    ICAP_DEMAND_CURVE_MULT(HO)
!
                        IF(ICAP_SWITCH_INDEX(CM) == 2 .OR.   & !MARGINAL
                                        ICAP_SWITCH_INDEX(CM) == 3) THEN
                                        ! CONE-MRX
                           CAPACITY_REVENUE_MM(HO) = &
                              MIN(CONE_DEMAND_CURVE_ADJ_PER_KWYR(HO) * &
                                SCREEN_CAPACITY(HIGHEST_OPTION)*0.001, &
                                MAX(0.,0.000001 * &
                               (ANNUAL_VARIABLE_COST(HIGHEST_OPTION) + &
                                  ANNUAL_FIXED_COST(HIGHEST_OPTION) - &
                                               HIGHEST_ENERGY_REVENUE)))
                           ICAP_REVENUE_PER_KWYR(HO) = &
                              MAX(0.0,CAPACITY_REVENUE_MM(HO)*1000.0/ &
                                       SCREEN_CAPACITY(HIGHEST_OPTION))
                        ELSE
                           ICAP_REVENUE_PER_KWYR(HO) = &
                                 MIN(ICAP_CONE_PRICE(CM), &
                                     CONE_DEMAND_CURVE_ADJ_PER_KWYR(HO))
                           CAPACITY_REVENUE_MM(HO) = &
                              ICAP_REVENUE_PER_KWYR(HO) * &
                                 SCREEN_CAPACITY(HIGHEST_OPTION) * 0.001
                        ENDIF
                        EAS_PROFIT_PER_KWYR(HO) = &
                                          HIGHEST_EAS_PROFIT_PER_KWYR
                        EAS_PROFIT_PER_KWYR(HO) = MAX(0.0, &
                                  CONE_DEMAND_CURVE_ADJ_PER_KWYR(HO) - &
                                              ICAP_REVENUE_PER_KWYR(HO))
                     ELSE
                        EAS_PROFIT_PER_KWYR(HO) = 0.0
                        CONE_COST_PER_KWYR(HO) = 0.0
                        CONE_DEMAND_CURVE_ADJ_PER_KWYR(HO) = 0.0
                        ICAP_REVENUE_PER_KWYR(HO) = 0.0
                        CAPACITY_REVENUE_MM(HO) = 0.0
                     ENDIF
!
                     ENERGY_REVENUE_MM(HO) = &
                                         HIGHEST_ENERGY_REVENUE/1000000.
                     HIGHEST_MARKET_REVENUE(HO) = &
                                ENERGY_REVENUE_MM(HO) + &
                                 RPS_REVENUE_MM(HO) + &
                                     CAPACITY_REVENUE_MM(HO)
!
! END TESTING
!
                     IF(HIGHEST_MRX_ENERGY(HO) > 0.01) THEN
                        CAPACITY_REVENUE_PER_MWH(HO) = &
                            CAPACITY_REVENUE_MM(HO)*1000.0/ &
                                          HIGHEST_MRX_ENERGY(HO)
                        NET_MARGIN(HIGHEST_OPTION) = &
                                 HIGHEST_MARKET_REVENUE(HO) / &
                                          HIGHEST_MRX_ENERGY(HO)
                     ELSE
                        CAPACITY_REVENUE_PER_MWH(HO) = 0.0
                        NET_MARGIN(HIGHEST_OPTION) = -999999.
                     ENDIF
                     REVENUE_PER_MWH(HO) = &
                           HIGHEST_ENERGY_REV_PER_MWH(HO) + &
                                CAPACITY_REVENUE_PER_MWH(HO)
                     VARIABLE_COST_MM(HO) = 0.000001 * &
                                    ANNUAL_VARIABLE_COST(HIGHEST_OPTION)
                     FIXED_COST_MM(HO) = 0.000001 * &
                                    ANNUAL_FIXED_COST(HIGHEST_OPTION)
                     HIGHEST_SCREEN_CAPACITY(HO) = &
                                         SCREEN_CAPACITY(HIGHEST_OPTION)
                     FIXED_COST_PER_MWH(HO) = &
                        FOM_PER_MWH(HIGHEST_OPTION) + &
                              LEVELIZED_CAP_PER_MWH(HIGHEST_OPTION)
                     HIGHEST_MRX_MARGIN(HO) = &
                              REVENUE_PER_MWH(HO) - &
                                 HIGHEST_STRIKE_PRICE(HO) - &
                                    FIXED_COST_PER_MWH(HO)
                     HIGHEST_MRX_CF(HO) = HIGHEST_CF(HIGHEST_OPTION)
                     HIGHEST_MARKET_COST(HO) = &
                                   MARKET_COST(HIGHEST_OPTION)/1000000.0
                     HIGHEST_ANN_LEVEL_CAP(HO) = &
                                ANNUAL_LEVELIZED_CAPITAL(HIGHEST_OPTION)
                     IF(HIGHEST_SCREEN_CAPACITY(HO) > 0.01) THEN
                        NET_MARGIN_PER_KWYR(HIGHEST_OPTION) = 1000.0 * &
                              (HIGHEST_MARKET_REVENUE(HO) - &
                                 VARIABLE_COST_MM(HO) - &
                                       FIXED_COST_MM(HO))/ &
                                 HIGHEST_SCREEN_CAPACITY(HO)
                     ELSE
                        NET_MARGIN_PER_KWYR(HIGHEST_OPTION) = -999999.0
                     ENDIF
!
                     HIGHEST_FUEL_COST(HO) = &
                              ANNUAL_FUEL_COST(HIGHEST_OPTION)/1000000.0
                     HIGHEST_HEAT_RATE(HO) = &
                                       HEAT_RATE(HIGHEST_OPTION)
                     HIGHEST_ANNUAL_UNITS(HO) = &
                               FLOAT(LOCAL_ANNUAL_UNITS(HIGHEST_OPTION))
                     HIGHEST_MARGIN_PER_KWYR(HO) = &
                                     NET_MARGIN_PER_KWYR(HIGHEST_OPTION)
                     HIGHEST_VAR_OM_PER_MWH(HO) = &
                                   VAR_OM_PER_MWH(HIGHEST_OPTION) ! 19
                     HIGHEST_FUEL_COST_PER_MMBTU(HO) = &
                                     FUEL_COST_PER_MMBTU(HIGHEST_OPTION)
                     HIGHEST_EMISSIONS_PER_MWH(HO) = &
                                       EMISSIONS_PER_MWH(HIGHEST_OPTION)
                     HIGHEST_FOM_PER_MWH(HO) = &
                                             FOM_PER_MWH(HIGHEST_OPTION)
                     HIGHEST_LEVEL_CAP_PER_MWH(HO) = &
                                   LEVELIZED_CAP_PER_MWH(HIGHEST_OPTION)
                     HIGHEST_VOM_COST_MM(HO) = &
                                             VOM_COST_MM(HIGHEST_OPTION)
                     HIGHEST_EMISSIONS_COST_MM(HO) = &
                                       EMISSIONS_COST_MM(HIGHEST_OPTION)
                     HIGHEST_FOM_COST_MM(HO) = &
                                             FOM_COST_MM(HIGHEST_OPTION)
!
                     MRX_ICAP_INDEX(HO) = 0
!
                     MXVars(1:13,L,26) = CONE_COST_PER_KWYR(HO)
                     MX_ANNUAL_ALT_REC = RPTREC(MX_ANNUAL_ALT_NO)
                     WRITE(MX_ANNUAL_ALT_NO,REC=MX_ANNUAL_ALT_REC) &
                        PRT_ENDPOINT(), &
                        FLOAT(THIS_YEAR), &
                        MULTI_AREA_NAME(L), &
                        LOCAL_NAME, &
                        HIGHEST_STRIKE_PRICE(HO), & !  0
                        HIGHEST_ENERGY_REV_PER_MWH(HO), & 
                        ! 111509. REDEFINED.
                        HIGHEST_MRX_MARGIN(HO), &
                        HIGHEST_MRX_CF(HO), &
                        ENERGY_REVENUE_MM(HO), &
                        HIGHEST_MARKET_COST(HO), & !  5
                        VARIABLE_COST_MM(HO), &
                        FIXED_COST_MM(HO), &
                        HIGHEST_SCREEN_CAPACITY(HO), &
                        HIGHEST_MRX_ENERGY(HO), & !
                        TOTAL_AVERAGE_COST, & !  10 NEED DEFINITION
                        HIGHEST_ANN_LEVEL_CAP(HO), &
                        HIGHEST_FUEL_COST(HO), &
                        HIGHEST_ANNUAL_UNITS(HO), &
                        CAPACITY_REVENUE_MM(HO), &
                        HIGHEST_MARKET_REVENUE(HO), & 
                        ! 15 TOTAL REVENUE
                        HIGHEST_MARGIN_PER_KWYR(HO), &
                        HIGHEST_FUEL_COST_PER_MWH(HO), &
                        HIGHEST_VAR_OM_PER_MWH(HO), & !  18
                        HIGHEST_FUEL_COST_PER_MMBTU(HO), &
                        REVENUE_PER_MWH(HO),  & !  20
                        CAPACITY_REVENUE_PER_MWH(HO), &
                        HIGHEST_EMISSIONS_PER_MWH(HO), &
                        HIGHEST_FOM_PER_MWH(HO), &
                        HIGHEST_LEVEL_CAP_PER_MWH(HO), &
                        HIGHEST_VOM_COST_MM(HO),  & !  25
                        HIGHEST_EMISSIONS_COST_MM(HO), &
                        HIGHEST_FOM_COST_MM(HO), &
                        FLOAT(TG), & !  TRANS_GROUP_ID(HIGHEST_OPTION),
                        TG_CAPACITY_MW(L), &
                        TG_PEAK_MW(L),  & !  30
                        TG_RESERVE_MARGIN(L), &
                        FLOAT(CM_INDEX), & 
                        ! PLANNING_AREA_ID(HIGHEST_OPTION),
                        CM_CAPACITY_MW(CM), &
                        CM_PEAK_MW(CM), &
                        CM_RESERVE_MARGIN(CM),  & !  35
                        ICAP_DEMAND_CURVE_MULT(HO), &
                        CONE_COST_PER_KWYR(HO), &
                        CONE_DEMAND_CURVE_ADJ_PER_KWYR(HO), &
                        EAS_PROFIT_PER_KWYR(HO), &
                        ICAP_REVENUE_PER_KWYR(HO) ,  & !  40
                        HIGHEST_HEAT_RATE(HO), &
                        FIXED_COST_PER_MWH(HO), &
                        HIGHEST_RFT(HO), &
                        HIGHEST_RPM(HO), &
                        TG_Net_Transfer(L), & !  45
                        CM_Net_Transfer(CM), &
                        MRX_ICAP_INDEX(HO), &
                        CM_INTERUPTIBLRESERVE_MARGIN, &
                        CM_PEAK_AFTER_INTERRUPTIBLE(CM), &
                        CAPACITY_PLANNING_MW(HIGHEST_OPTION), &
                        RPS_REVENUE_MM(HO), &
                        RPS_REVENUE_PER_MWH(HO)
                     MX_ANNUAL_ALT_REC = MX_ANNUAL_ALT_REC + 1
                  ENDIF
!
                  CALL ALTER_MX_PRICE_CURVE(  HIGHEST_INTERVAL, &
                                           MARKET_DURATION, &
                                           MARKET_CUM_REVENUE, &
                                           MARKET_PRICE, &
                                           EQUIVALENT_CAPACITY( &
                                                      HIGHEST_OPTION), &
                                           HIGHEST_ENERGY( &
                                                      HIGHEST_OPTION), &
                                           STRIKE_PRICE( &
                                                   HIGHEST_OPTION,MO), &
                                           HIGHEST_J, &
                                           HIGHEST_TOP_CAP_PERCENT, &
                                           MAX(HIGHEST_ENERGY_REVENUE, &
                                                                 0.0), &
                                           TRANS_GROUP_4_PRICING, &
                                           RESOURCE_ADDITION)
                  IF(CX_ACTIVE) THEN ! .AND.
                     IF(TG == 0) THEN
                        TEMP_PEAK =  UPDATE_NET_PLANNING_PEAK(R_YEAR)
                     ELSE
                        TEMP_PEAK = GET_GROUP_PEAK_ON_PEAK_MONTH(L)
                     ENDIF
                     TEMP_L = CX_ALTER_MX_CHRONO_PRICES( &
                                    TG_PLANNING_CAPACITY(L), &
                                    HIGHEST_CHRONO_MW_USAGE, &
                                    LOCAL_CHRONO_MARKET_PRICES(1,L), &
                                    TEMP_PEAK)
                  ENDIF
!
! 040611. NOTE THE ON FIRST_CT_EAS IS $/KWYR
!
                     U = GET_PRODUCTION_DATA_POINTER(HIGHEST_OPTION)
                     RPM = FLOAT(GET_S_PRIMARY_MOVER_INDEX(U))
                     IF(FIRST_CT_4_EAS_INDEX(CM) <= 0 .AND. &
                                    HIGHEST_RPM(HO) == 4 .AND. &
                            SCREEN_CAPACITY(HIGHEST_OPTION) > 0.01) THEN
                        FIRST_CT_4_EAS_INDEX(CM) = HO
                        FIRST_CT_EAS(CM) = 1000.* &
                                    ENERGY_REVENUE_MM(HO)/ &
                                         SCREEN_CAPACITY(HIGHEST_OPTION)
                     ENDIF
!
! FANCY DYNAMIC UPDATING.
!
                  TOTAL_ANNUAL_CAPACITY_ADDED(PA) = &
                           TOTAL_ANNUAL_CAPACITY_ADDED(PA) + &
                                         SCREEN_CAPACITY(HIGHEST_OPTION)
                  CM_TOT_ANN_CAPACITY_ADDED(CM) = &
                           CM_TOT_ANN_CAPACITY_ADDED(CM) + &
                                         SCREEN_CAPACITY(HIGHEST_OPTION)
                  TG_TOTAL_ANNUAL_CAPACITY_ADDED(L) = &
                           TG_TOTAL_ANNUAL_CAPACITY_ADDED(L) + &
                                         SCREEN_CAPACITY(HIGHEST_OPTION)
                  IF(PA_PEAK_AFTER_INTERRUPTIBLE(PA) > NEAR_ZERO)  THEN
                     PA_PLANNING_RESERVE_MARGIN(PA) = &
                        1. + (PA_PLANNING_CAPACITY(PA) + &
                           TOTAL_ANNUAL_CAPACITY_ADDED(PA) - &
                                 PA_PEAK_AFTER_INTERRUPTIBLE(PA)) / &
                                      PA_PEAK_AFTER_INTERRUPTIBLE(PA)
                  ELSE
                     PA_PLANNING_RESERVE_MARGIN(PA) = ZERO
                  ENDIF
                  PA_ICAP_REVENUE_MULT(PA) = GET_ICAP_REVENUE_MULT( &
                              R_YEAR,PA,PA_PLANNING_RESERVE_MARGIN(PA))
                  IF(CM_PEAK_AFTER_INTERRUPTIBLE(CM) > NEAR_ZERO)  THEN
                     CM_PLANNING_RESERVE_MARGIN(CM) = &
                        1. + (CM_PLANNING_CAPACITY(CM) + &
                                 CM_Net_Transfer(CM) + & !  100410.
                           CM_TOT_ANN_CAPACITY_ADDED(CM) - &
                                 CM_PEAK_AFTER_INTERRUPTIBLE(CM)) / &
                                      CM_PEAK_AFTER_INTERRUPTIBLE(CM)
                  ELSE
                     CM_PLANNING_RESERVE_MARGIN(CM) = ZERO
                  ENDIF
                  CM_ICAP_REVENUE_MULT(CM) = GET_ICAP_REVENUE_MULT( &
                              R_YEAR,CM,CM_PLANNING_RESERVE_MARGIN(CM))
!
               ENDIF
!
               ANNUAL_RESOURCES_AVAILABLE = .FALSE.
               DO CN = 1, TOTAL_ALL_OPTIONS
                  I = OPTION_POSITION(CN)
                  IF(.NOT. CX_ACTIVE .AND. &
                              FILE_SOURCE_INDEX(CN) == Derivative) CYCLE
                  IF( .NOT. RESOURCE_AVAILABLE(I, &
                                    THIS_YEAR_PLUS_LEAD_TIME(CN)) .OR. &
                      .NOT. TEST_START_STOP_YEARS(I, &
                                    THIS_YEAR_PLUS_LEAD_TIME(CN)) .OR. &
                                 TRANSACTION_GROUP(CN) /= TG  .OR. &
                                       LOCAL_ANNUAL_UNITS(CN) < 1 .OR. &
                               .NOT. DEPENDENT_UNIT_AVAILABLE(CN)) CYCLE
                  ANNUAL_RESOURCES_AVAILABLE = .TRUE.
                  EXIT
               ENDDO
!
            ENDDO ! ADD UNITS WHILE STILL MAKING MONEY
         ENDDO ! TRANSACTION GROUP
!
         CALL SortIncrPos(                HO, &
                                          SORTED_OPTIONS, &
                                          HGST_NET_MARG_ADDED)
!
         IF(USE_MAXIMUM_RM_LOGIC) THEN
            MAXIMUM_ANNUAL_MRX_CAP(0) = &
                           MAX(0.,MIN(MAXIMUM_ANNUAL_MRX_CAP(0), &
                                      (MAXIMUM_RM-ANN_CAP/PEAK) * PEAK))
         ENDIF
!
         IF(USE_MINIMUM_RM_LOGIC) THEN
            MINIMUM_ANNUAL_MRX_CAP(0) = MAX(0., &
                        (CURRENT_TARGET_RATIO-ANN_CAP/PEAK)*PEAK)
         ELSE
            MINIMUM_ANNUAL_MRX_CAP(0) = 0.
         ENDIF
!
! USED UP ABOVE FOR ICAP REVENUES
!
         TOTAL_ANNUAL_CAPACITY_ADDED = 0.
         CM_TOT_ANN_CAPACITY_ADDED = 0.
         TG_TOTAL_ANNUAL_CAPACITY_ADDED = 0.
!
         IF(MRX_SORTED_MIX) THEN
            LSP = LAST_SORTED_POSITION
            HO = MX_RES_P_YEAR
!
            DO J = HO, 1, -1
               DO K = 1, TOTAL_ALL_OPTIONS
                  IF(LSP < TOTAL_ALL_OPTIONS) THEN
                     LSP = LSP + 1
                  ELSE
                     LSP = 1
                  ENDIF
                  I = OPTION_POSITION(LSP)
                  IF( .NOT. RESOURCE_AVAILABLE(I, &
                                   THIS_YEAR_PLUS_LEAD_TIME(LSP)) .OR. &
                         .NOT. TEST_START_STOP_YEARS(I, &
                                   THIS_YEAR_PLUS_LEAD_TIME(LSP)) .OR. &
                              .NOT. DEPENDENT_UNIT_AVAILABLE(LSP)) CYCLE
                  PROD_POINTER = GET_PRODUCTION_DATA_POINTER(I)
                  TRANSACTION_GROUP(LSP) = &
                                   RETURN_S_TRANS_GROUP_ID(PROD_POINTER)
                IF(GET_TRANS_GROUP_POSITION(TRANSACTION_GROUP(LSP)) <= &
                                                                0) CYCLE
!
! FOUND A GOOD OPTION
!
                  EXIT
               ENDDO
               SORTED_OPTIONS(J) = J
               HGST_OPT_INDEX(J) = LSP
            ENDDO
         ENDIF
!
! 072110. FIND CHEAPEST $/KW RESOURCE PER TRANSACTION GROUP
!
         HIGHEST_TG_ICAP_REV_PER_KWYR = -999999.0
         HIGHEST_OPTION_FOR_TG = 0
         DO I = HO, 1, -1
            NEXT_CN = SORTED_OPTIONS(I)
            HIGHEST_OPTION = HGST_OPT_INDEX(NEXT_CN)
!
            IF(.NOT. DEPENDENT_UNIT_AVAILABLE(HIGHEST_OPTION)) CYCLE
!
            TG = GET_TRANS_GROUP_POSITION( &
                                      TRANSACTION_GROUP(HIGHEST_OPTION))
            HIGHEST_OPTION_FOR_TG(TG) = HIGHEST_OPTION
            HIGHEST_TG_ICAP_REV_PER_KWYR(TG) = &
                        MAX(HIGHEST_TG_ICAP_REV_PER_KWYR(TG), &
                                  ICAP_REVENUE_PER_KWYR(HIGHEST_OPTION))
         END DO
!
! IDENTIFY THE MW AND $/KW*YR OF ADJACENT TG'S
! IDENTIFY THE LOWEST COST TG TRANSFER SET THAT CAN MEET THE SCREEN
! CAPACITY
! USE THE MIN(LOWEST COST TRANSFER,INTERNAL COST) AS THE TRIGGER
! IF UNIT IS STILL BUILT, OK
! IF UNIT IS NOT BUILT, PERFORM AND RECORD THE TRANSFER
! ADJUST THE CAPACITIES OF THE NET TG'S AND CM'S
! NEXT
!
         DO I = 1, UPPER_TRANS_GROUP
            LOWEST_TRANSFER_COST = 999999.0
            LOWEST_TRANSFER_CAPACITY = 0.0
            DO J = 1, UPPER_TRANS_GROUP
               IF(TransmissionConstraint(J,I) < 0.01) CYCLE
               IF(LOWEST_TRANSFER_COST < &
                                  HIGHEST_TG_ICAP_REV_PER_KWYR(J)) CYCLE
               LOWEST_TRANSFER_COST = HIGHEST_TG_ICAP_REV_PER_KWYR(J)
               LOWEST_TRANSFER_CAPACITY = &
                                            TransmissionConstraint(J,I)
!
! BUYER TG AND PA CAN'T EXCEED MAX RESERVE MARGIN
! SELLER TG AND PA CAN'T EXCEED MIN RESERVE MARGIN
!
            END DO
            HIGHEST_OPTION = HIGHEST_OPTION_FOR_TG(I)
!
! FIND CROSS-OVER POINT
!
         END DO
!
 5678    CONTINUE
!
         IF(R_YEAR == 5 .AND. GRX_ITERATIONS == 3) THEN
            R_YEAR = R_YEAR
         ENDIF
!
         RPS_REVENUE_MM = 0.0
         MRX_RPS_MX_UNIT_CLEARED = 0
         IF( (GRX_RPS_MODULE_ACTIVE .OR. RPS_ONLY_SWITCH_ACTIVE) &
                                            .AND. RPS_PROGRAMS > 0) THEN
!
! 070320. MAY WANT TO MOVE THIS UP TO SAVE TIME.
!
         ! TODO: Individually check and deallocate each array.  Following the
         ! deallocation actions, individually allocate each array
         ! and call check_alloc after the allocation. cap_objt:ra8
            IF(ALLOCATED(MRX_RPS_MX_CURVE_MARGIN)) &
                DEALLOCATE(MRX_RPS_MX_CURVE_MARGIN, &
                           MRX_RPS_MX_CURVE_REVENUE, &
                           MRX_RPS_MX_CURVE_COST, &
                           MRX_RPS_MX_CURVE_UMWH, &
                           MRX_RPS_MX_CURVE_MWH, &
                           MRX_RPS_MX_OPTION_NO, &
                           MRX_RPS_MX_CURVE_INDEX, &
                           MRX_RPS_MX_OPTION_INDEX, &
                           MRX_RPS_MX_NEXT_CN_INDEX, &
                           RPS_REC_PRICE_BY, &
                           RPS_GEN_PRICE_BY)
!
            PA = RPS_PROGRAMS
            ALLOCATE(MRX_RPS_MX_CURVE_MARGIN(HO,PA), &
                     MRX_RPS_MX_CURVE_REVENUE(HO,PA), &
                     MRX_RPS_MX_CURVE_COST(HO,PA), &
                     MRX_RPS_MX_CURVE_UMWH(HO,PA), &
                     MRX_RPS_MX_CURVE_MWH(HO,PA), &
                     MRX_RPS_MX_OPTION_NO(PA), &
                     MRX_RPS_MX_CURVE_INDEX(HO,PA), &
                     MRX_RPS_MX_OPTION_INDEX(HO,PA), &
                     MRX_RPS_MX_NEXT_CN_INDEX(HO,PA), &
                     RPS_REC_PRICE_BY(PA), &
                     RPS_GEN_PRICE_BY(PA))
!
            MRX_RPS_MX_OPTION_NO = 0
            MRX_RPS_MX_CURVE_INDEX = 0
            MRX_RPS_MX_OPTION_INDEX = 0
            MRX_RPS_MX_NEXT_CN_INDEX = 0
            MRX_RPS_MX_CURVE_MARGIN = 0
            MRX_RPS_MX_CURVE_REVENUE = 0.0
            MRX_RPS_MX_CURVE_COST = 0.0
            MRX_RPS_MX_CURVE_UMWH = 0.0
!
! 072620. RPS_REVENUE SOLVED IN CAPACITY PRICE SECTION BELOW.
!
            RPS_REC_PRICE_BY = 0.0
            RPS_GEN_PRICE_BY = 0.0
!
            DO I = 1, HO
               NEXT_CN = SORTED_OPTIONS(I)
               HIGHEST_OPTION = HGST_OPT_INDEX(NEXT_CN)
               PA = RPS_PROGRAM_NUMBER(HIGHEST_OPTION)
               IF(PA == 0) CYCLE
               RPS_COUNT = 1
               DO
                  PX = GET_RPS_PROGRAM_POSITION(PA,RPS_COUNT)
                  IF(PX == 0) EXIT
                  RPS_COUNT = RPS_COUNT + 1
                  TEMP_R4 = 1000.0*HIGHEST_MRX_ENERGY(NEXT_CN) * &
                                             RPS_PERCENT(HIGHEST_OPTION)
                  IF(TEMP_R4 < 0.0001) CYCLE
!
                  MRX_RPS_MX_OPTION_NO(PX) = &
                                            MRX_RPS_MX_OPTION_NO(PX) + 1
                  J = MRX_RPS_MX_OPTION_NO(PX)
                  MRX_RPS_MX_CURVE_UMWH(J,PX) = TEMP_R4
                  MRX_RPS_MX_CURVE_INDEX(J,PX) = J
                  MRX_RPS_MX_OPTION_INDEX(J,PX) = HIGHEST_OPTION
                  MRX_RPS_MX_NEXT_CN_INDEX(J,PX) = NEXT_CN
                  MRX_RPS_MX_CURVE_REVENUE(J,PX) = 1000. * &
                        ENERGY_REVENUE_MM(NEXT_CN) / &
                                             HIGHEST_MRX_ENERGY(NEXT_CN)
                  MRX_RPS_MX_CURVE_COST(J,PX) = 1000. * &
                        (FIXED_COST_MM(NEXT_CN) + &
                                      VARIABLE_COST_MM(NEXT_CN))/ &
                                             HIGHEST_MRX_ENERGY(NEXT_CN)
                  MRX_RPS_MX_CURVE_MARGIN(J,PX) = &
                        MRX_RPS_MX_CURVE_REVENUE(J,PX) - &
                                             MRX_RPS_MX_CURVE_COST(J,PX)
               ENDDO
            ENDDO
            DO PA = 1, RPS_PROGRAMS
               IF(MRX_RPS_MX_OPTION_NO(PA) > 1) THEN
                  CALL SortIncrPos(MRX_RPS_MX_OPTION_NO(PA), &
                                MRX_RPS_MX_CURVE_INDEX(1,PA), &
                                MRX_RPS_MX_CURVE_MARGIN(1,PA))
               ENDIF
            ENDDO
            RPS_RESOURCE_ADDED = 0
            DO PA = 1, RPS_PROGRAMS
               PROG_REQ = GET_ANNUAL_RPS_PROG_REQ(PA,R_YEAR)
               TEMP_L = GET_RPS_NAME(RPS_NAME,PA)
               RPS_ALT_COMPLIANCE_PRICE = &
                        GET_RPS_ALT_COMPLIANCE_PRICE(PA,R_YEAR)
!
              ZERO_BID_MWH = GET_QUALIFYING_GEN_DB(PA,INT(5,2),INT(1,2))
               J = 0
               K = 0
               L = 0
               M = 0
               I = MRX_RPS_MX_OPTION_NO(PA)
               U = MRX_RPS_CL_UNIT_NO(PA)
               D = MRX_RPS_DV_UNIT_NO(PA)
               LAST_EN = 0
               LAST_EU = 0
               LAST_ED = 0
               TEMP_COST = 0.0
               TEMP_REVENUE = 0.0
               TEMP_MWH = 0.0
               CUMULATIVE_MWH = 0.0
               PA_BID = 0.0
               PA_COST = 0.0
               PA_REVENUE = 0.0
               PA_CUM_MWH = 0.0
               RPS_CROSS_OVER = .FALSE.
               RPS_GEN_WEIGHTED_REV = 0.0
               RPS_GEN_WEIGHTED_PRICE = 0.0
!
! NEED BIDS FOR RESOURCES BY MWH NOT KW*YR
!
               DO
                  IF(I+U+D <= 0) EXIT
                  IF(I > 0) THEN
                     M = MRX_RPS_MX_CURVE_INDEX(I,PA)
                     FN = MRX_RPS_MX_NEXT_CN_INDEX(M,PA)
                     EN = MRX_RPS_MX_OPTION_INDEX(M,PA)
                     LAST_EN = MAX(EN,LAST_EN)
                     NEW_BID = MAX(0.0, &
                                      -1.*MRX_RPS_MX_CURVE_MARGIN(M,PA))
                  ELSE
                     NEW_BID = 999999.
                  ENDIF
                  IF(U > 0) THEN
                     J = MRX_RPS_CL_CURVE_INDEX(U,PA)
                     EU = MRX_RPS_CL_UNIT_INDEX(J,PA)
                     LAST_EU = MAX(EU,LAST_EU)
                     EXISTING_CL_BID = MAX(0.0, &
                                      -1.*MRX_RPS_CL_CURVE_MARGIN(J,PA))
                  ELSE
                     EXISTING_CL_BID = 999999.
                  ENDIF
                  IF(D > 0) THEN
                     L = MRX_RPS_DV_CURVE_INDEX(D,PA)
                     ED = MRX_RPS_DV_UNIT_INDEX(L,PA)
                     LAST_ED = MAX(ED,LAST_ED)
                     EXISTING_DV_BID = MAX(0.0, &
                                      -1.*MRX_RPS_DV_CURVE_MARGIN(L,PA))
                  ELSE
                     EXISTING_DV_BID = 999999.
                  ENDIF
!
! DETERMINE ORDER OF NEW_BID, EXISTING_CL_BID, EXISTING_DV_BID,
! INCLUDING NO RESOURCES LEFT.
! 100810. I ONLY AFTER U AND D.
!
                  BID_POS = 0
                  IF(U+D > 0) THEN
                     IF( U>0 .AND. &
                         (( EXISTING_CL_BID <= EXISTING_DV_BID) .OR. &
                                                            D==0) ) THEN
                        BID_POS = 1
                     ELSE
                        BID_POS = 2
                     ENDIF

                  ELSEIF(I > 0) THEN
                     BID_POS = 3
                  ELSE
                     WRITE(9,*) 'INFEASIBLE RPS SUPPLY CURVE'
                     er_message="INFEASIBLE RPS SUPPLY CURVE"
                     call end_program(er_message)
                  ENDIF
                  K = K + 1
                  IF(K == 1) THEN
                     IF(ZERO_BID_MWH < 0.0001) CYCLE
                     CURVE_NAME = 'Zero Bid Capacity       '
                     TEMP_BID = 0.0
                     TEMP_REVENUE = 0.0
                     TEMP_COST = 0.0
                     TEMP_MWH = ZERO_BID_MWH
                     CM = 0
                     CM_INDEX = 0
                  ELSEIF( BID_POS == 3 ) THEN
                     WRITE(CURVE_NAME,1010) &
                                OPTION_NAME(EN)(1:20),I
                     TEMP_BID = NEW_BID
                     TEMP_MWH = MRX_RPS_MX_CURVE_UMWH(M,PA)
                     TEMP_REVENUE = MRX_RPS_MX_CURVE_REVENUE(M,PA)
                     TEMP_COST = MRX_RPS_MX_CURVE_COST(M,PA)
                     I = I - 1
                  ELSEIF( BID_POS == 1 ) THEN
                     CURVE_NAME = UNITNM(EU)//CL_UNIQUE_RPT_STR(EU)
                     TEMP_BID = EXISTING_CL_BID
                     TEMP_MWH = MRX_RPS_CL_CURVE_MWH(J,PA)
                     TEMP_REVENUE = 1000. * &
                                         MRX_RPS_CL_CURVE_REVENUE(EU)/ &
                                             MRX_RPS_CL_CURVE_MWH(J,PA)
                     TEMP_COST = 1000. * MRX_RPS_CL_CURVE_COST(EU)/ &
                                             MRX_RPS_CL_CURVE_MWH(J,PA)
                     U = U - 1
                  ELSE
                     TEMP_L = GET_TRANS_NAME(ED,TEMP_CURVE_NAME)
                     CURVE_NAME = TEMP_CURVE_NAME
                     TEMP_BID = EXISTING_DV_BID
                     TEMP_MWH = MRX_RPS_DV_CURVE_MWH(L,PA)
                     TEMP_REVENUE = 1000. * &
                                         MRX_RPS_DV_CURVE_REVENUE(ED)/ &
                                             MRX_RPS_DV_CURVE_MWH(L,PA)
                     TEMP_COST = 1000. * MRX_RPS_DV_CURVE_COST(ED)/ &
                                             MRX_RPS_DV_CURVE_MWH(L,PA)
                     D = D - 1
                  ENDIF
                  CUMULATIVE_MWH = CUMULATIVE_MWH + TEMP_MWH
                  PA_BID(K) = TEMP_BID
                  PA_COST(K) = TEMP_COST
                  PA_REVENUE(K) = TEMP_REVENUE
                  PA_CUM_MWH(K) = CUMULATIVE_MWH
                  RPS_GEN_WEIGHTED_REV = RPS_GEN_WEIGHTED_REV + &
                                                    TEMP_BID * TEMP_MWH
                  IF(CUMULATIVE_MWH > 0.001) THEN
                     RPS_GEN_WEIGHTED_PRICE = RPS_GEN_WEIGHTED_REV/ &
                                                      CUMULATIVE_MWH
                  ELSE
                     RPS_GEN_WEIGHTED_PRICE = 0.0
                  ENDIF
                  IF(K == 1) THEN
                     RPS_REC_PRICE = 0.0
                  ELSEIF(RPS_CROSS_OVER) THEN
                     RPS_REC_PRICE = RPS_REC_PRICE
                  ELSEIF(CUMULATIVE_MWH > 0.001) THEN
                     RPS_REC_PRICE = PA_BID(K)

                  ELSE
                     RPS_REC_PRICE = PA_BID(K)
                  ENDIF
                  IF(.NOT. RPS_CROSS_OVER) THEN
                     RPS_REC_PRICE_BY(PA) = RPS_REC_PRICE
                     RPS_GEN_PRICE_BY(PA) = RPS_GEN_WEIGHTED_PRICE
                  ENDIF
                  IF(.NOT. RPS_CROSS_OVER .AND. BID_POS == 3 ) THEN
                     MRX_RPS_MX_UNIT_CLEARED(FN) = 1
                  ENDIF
                  IF(CUMULATIVE_MWH >= PROG_REQ .AND. &
                                              .NOT. RPS_CROSS_OVER) THEN
                     RPS_CROSS_OVER = .TRUE.
                     IF(RPS_CURVE_REPORT) THEN
                        VN_ANNUAL_ALT_REC = RPTREC(VN_ANNUAL_ALT_NO)
                        WRITE(VN_ANNUAL_ALT_NO,REC=VN_ANNUAL_ALT_REC) &
                           PRT_ENDPOINT(), &
                           FLOAT(THIS_YEAR), &
                           RPS_NAME, &
                           'REC PRICE                ', &
                           PA_BID(K), &
                           PA_CUM_MWH(K), &
                           PA_REVENUE(K), &
                           PA_COST(K), &
                           PROG_REQ, &
                           FLOAT(K), &
                           TEMP_MWH, &
                           RPS_REC_PRICE, &
                           RPS_GEN_WEIGHTED_PRICE
                        VN_ANNUAL_ALT_REC = VN_ANNUAL_ALT_REC + 1
                     ENDIF ! YES_RPS_CURVE_REPORT
                  ENDIF
                  IF(RPS_CURVE_REPORT) THEN
                     VN_ANNUAL_ALT_REC = RPTREC(VN_ANNUAL_ALT_NO)
                     WRITE(VN_ANNUAL_ALT_NO,REC=VN_ANNUAL_ALT_REC) &
                        PRT_ENDPOINT(), &
                        FLOAT(THIS_YEAR), &
                        RPS_NAME, &
                        CURVE_NAME, &
                        PA_BID(K), &
                        PA_CUM_MWH(K), &
                        PA_REVENUE(K), &
                        PA_COST(K), &
                        PROG_REQ, &
                        FLOAT(K), &
                        TEMP_MWH, &
                        RPS_REC_PRICE, &
                        RPS_GEN_WEIGHTED_PRICE
                     VN_ANNUAL_ALT_REC = VN_ANNUAL_ALT_REC + 1
                  ENDIF ! YES_RPS_CURVE_REPORT
                  IF(I+U+D < 1 .AND. .NOT. RPS_CROSS_OVER) THEN
                     RPS_REC_PRICE = RPS_ALT_COMPLIANCE_PRICE
                     IF(RPS_CURVE_REPORT) THEN
                        VN_ANNUAL_ALT_REC = RPTREC(VN_ANNUAL_ALT_NO)
                        WRITE(VN_ANNUAL_ALT_NO,REC=VN_ANNUAL_ALT_REC) &
                           PRT_ENDPOINT(), &
                           FLOAT(THIS_YEAR), &
                           RPS_NAME, &
                           'REC PRICE                ', &
                           PA_BID(K), &
                           PA_CUM_MWH(K), &
                           PA_REVENUE(K), &
                           PA_COST(K), &
                           PROG_REQ, &
                           FLOAT(K), &
                           TEMP_MWH, &
                           RPS_REC_PRICE, &
                           RPS_GEN_WEIGHTED_PRICE
                        VN_ANNUAL_ALT_REC = VN_ANNUAL_ALT_REC + 1
                     ENDIF ! YES_RPS_CURVE_REPORT
                     EXIT
                  ENDIF
               ENDDO ! I OR U OR D
            ENDDO ! RPS_PROGRAM
         ENDIF ! RPS ONLY SWITCH)
!
!
! 062120. END RPS CURVE ROUTINE HERE.
!
!
         IF(R_YEAR == 4) THEN
            R_YEAR = R_YEAR
         ENDIF
         CM_IOTA = 0.0
         CM_TOT_ANN_CAPACITY_ADDED = 0.
         IF(CX_ACTIVE) THEN
            SORTED_OPTIONS = TEMP_INSTANCE
         ENDIF
!
         IF( (GRX_RPS_MODULE_ACTIVE .OR. RPS_ONLY_SWITCH_ACTIVE) &
                                            .AND. RPS_PROGRAMS > 0) THEN
            DO I = HO, 1, -1
               NEXT_CN = SORTED_OPTIONS(I)
               HIGHEST_OPTION = HGST_OPT_INDEX(NEXT_CN)
!
               IF(.NOT. DEPENDENT_UNIT_AVAILABLE(HIGHEST_OPTION)) CYCLE
               RPS_NO = RPS_PROGRAM_NUMBER(HIGHEST_OPTION)
               IF(RPS_NO /= 0) THEN
                  RPS_COUNT = 1
                  RPS_NO = GET_RPS_PROGRAM_POSITION(RPS_NO,RPS_COUNT)
                  IF(RPS_NO /= 0) THEN
                     RPS_REVENUE_MM(NEXT_CN) = &
                           RPS_REC_PRICE_BY(RPS_NO) * &
                                0.001 * HIGHEST_MRX_ENERGY(NEXT_CN) * &
                                             RPS_PERCENT(HIGHEST_OPTION)
                     IF(SCREEN_CAPACITY(HIGHEST_OPTION) > 0.01) THEN
                        HGST_NET_MARG_ADDED(NEXT_CN) = &
                             1000.0 * &
                                (ENERGY_REVENUE_MM(NEXT_CN) + &
                                   RPS_REVENUE_MM(NEXT_CN) + &
                                     CAPACITY_REVENUE_MM(NEXT_CN) - &
                                      VARIABLE_COST_MM(NEXT_CN) - &
                                       FIXED_COST_MM(NEXT_CN))/ &
                                         SCREEN_CAPACITY(HIGHEST_OPTION)
                     ENDIF
                  ENDIF
               ENDIF
            ENDDO
!
            CALL SortIncrPos(             HO, &
                                          SORTED_OPTIONS, &
                                          HGST_NET_MARG_ADDED)
         ENDIF ! RPS REVENUE
!
         TEMP_L = RESET_MRX_RPS_TEST_REQ()
!
         TOTAL_ANNUAL_CAPACITY_ADDED = 0.
         CM_TOT_ANN_CAPACITY_ADDED = 0.
         TG_TOTAL_ANNUAL_CAPACITY_ADDED = 0.
!
         DO I = HO, 1, -1
!
! MAXIMUM RESERVE MARGIN CONDITION
!
            NEXT_CN = SORTED_OPTIONS(I)
            HIGHEST_OPTION = HGST_OPT_INDEX(NEXT_CN)
!
            IF(.NOT. DEPENDENT_UNIT_AVAILABLE(HIGHEST_OPTION)) CYCLE

!
            TG = GET_TRANS_GROUP_POSITION( &
                                      TRANSACTION_GROUP(HIGHEST_OPTION))
            PA = TG_2_PLANNING_AREA(TG)
            CM = TG_2_CAPACITY_MARKET(TG)
            CM_INDEX = GET_CM_INDEX_FROM_TG(CM)
!
! 042710. RECALCULATE HGST_NET_MARG_ADDED BASED ON CURRENT ICAP
! 021819. TEST ADDING RPS_REVENUE
!
           IF(TG_MW_TRANSFERS) THEN
               TG_CAPACITY_MW(TG) = &
                           TG_PLANNING_CAPACITY(TG) + &
                             TG_TOTAL_ANNUAL_CAPACITY_ADDED(TG) + & ! -
                                          TG_Net_Transfer(TG)
            ELSE
               TG_CAPACITY_MW(TG) = &
                           TG_PLANNING_CAPACITY(TG) + &
                                 TG_TOTAL_ANNUAL_CAPACITY_ADDED(TG)
            ENDIF
            IF(TG_PEAK_MW(TG) > .01) THEN
               TG_RESERVE_MARGIN(TG) = 100. * &
                              (TG_CAPACITY_MW(TG)/TG_PEAK_MW(TG) - 1.0)
            ELSE
               TG_RESERVE_MARGIN(TG) = 100.
            ENDIF
            IF(TEST_CAP_VALUE_W_ADD) THEN
               TEMP_R4 = CAPACITY_PLANNING_MW(HIGHEST_OPTION)
            ELSE
               TEMP_R4 = 0.0
            ENDIF
            IF(TG_MW_TRANSFERS) THEN
               CM_CAPACITY_MW(CM) = &
                           CM_PLANNING_CAPACITY(CM) + &
                                 CM_TOT_ANN_CAPACITY_ADDED(CM) + &
                                    CM_Net_Transfer(CM) + &
                                       TEMP_R4
            ELSE
               CM_CAPACITY_MW(CM) = &
                           CM_PLANNING_CAPACITY(CM) + &
                                 CM_TOT_ANN_CAPACITY_ADDED(CM) + &
                                       TEMP_R4
            ENDIF
!
            IF(CM_PEAK_MW(CM) > .01) THEN
               CM_RESERVE_MARGIN(CM) = 100. * &
                              (CM_CAPACITY_MW(CM)/CM_PEAK_MW(CM) - 1.0)
               CM_INTERUPTIBLRESERVE_MARGIN = 100. * &
                         (CM_CAPACITY_MW(CM) - &
                                    CM_PEAK_AFTER_INTERRUPTIBLE(CM)) / &
                                CM_PEAK_AFTER_INTERRUPTIBLE(CM)
            ELSE
               CM_RESERVE_MARGIN(CM) = 100.
            ENDIF
            TEMP_R4 = CM_RESERVE_MARGIN(CM)*0.01 + 1.0
            TEMP_R4 = CM_INTERUPTIBLRESERVE_MARGIN*0.01 + 1.0
!
            ICAP_DEMAND_CURVE_MULT(NEXT_CN) = &
                                GET_ICAP_REVENUE_MULT(R_YEAR,CM,TEMP_R4)
!
            IF(SCREEN_CAPACITY(HIGHEST_OPTION) > 0.01) THEN
              IF(ICAP_SWITCH_INDEX(CM) == 2) THEN
                 CONE_COST_PER_KWYR(NEXT_CN) = &
                                             LOWEST_FIXED_COST_BY_CM(CM)
              ELSE
                 CONE_COST_PER_KWYR(NEXT_CN) = ICAP_CONE_PRICE(CM)
              ENDIF
              CONE_DEMAND_CURVE_ADJ_PER_KWYR(NEXT_CN) = &
                      CONE_COST_PER_KWYR(NEXT_CN) * &
                                    ICAP_DEMAND_CURVE_MULT(NEXT_CN)

               IF(ICAP_SWITCH_INDEX(CM) == 2 .OR.   & !  MARGINAL
                                        ICAP_SWITCH_INDEX(CM) == 3) THEN
                                        ! CONE-MRX
                  CAPACITY_REVENUE_MM(NEXT_CN) = &
                         MIN(CONE_DEMAND_CURVE_ADJ_PER_KWYR(NEXT_CN) * &
                           CAPACITY_PLANNING_MW(HIGHEST_OPTION)*0.001, &
                                MAX(0., &
                                 FIXED_COST_MM(NEXT_CN) + &
                                    VARIABLE_COST_MM(NEXT_CN) - &
                                       RPS_REVENUE_MM(NEXT_CN) - &
                                         ENERGY_REVENUE_MM(NEXT_CN)))
                  ICAP_REVENUE_PER_KWYR(NEXT_CN) = &
                          MAX(0.0,CAPACITY_REVENUE_MM(NEXT_CN)*1000.0/ &
                                        SCREEN_CAPACITY(HIGHEST_OPTION))
               ELSE
                  ICAP_REVENUE_PER_KWYR(NEXT_CN) = &
                           MIN(ICAP_CONE_PRICE(CM), &
                                CONE_DEMAND_CURVE_ADJ_PER_KWYR(NEXT_CN))

                  CAPACITY_REVENUE_MM(NEXT_CN) = &
                       ICAP_REVENUE_PER_KWYR(NEXT_CN) * &
                            CAPACITY_PLANNING_MW(HIGHEST_OPTION)*0.001
               ENDIF
!
               EAS_PROFIT_PER_KWYR(NEXT_CN) = 0.001 * &
                        1000000. * (ENERGY_REVENUE_MM(NEXT_CN) - &
                                           VARIABLE_COST_MM(NEXT_CN))/ &
                                         SCREEN_CAPACITY(HIGHEST_OPTION)
               EAS_PROFIT_PER_KWYR(NEXT_CN) = MAX(0.0, &
                             CONE_DEMAND_CURVE_ADJ_PER_KWYR(NEXT_CN) - &
                                         ICAP_REVENUE_PER_KWYR(NEXT_CN))
            ELSE
               EAS_PROFIT_PER_KWYR(NEXT_CN) = 0.0
               CONE_COST_PER_KWYR(NEXT_CN) = 0.0
               CONE_DEMAND_CURVE_ADJ_PER_KWYR(NEXT_CN) = 0.0
               ICAP_REVENUE_PER_KWYR(NEXT_CN) = 0.0
               CAPACITY_REVENUE_MM(NEXT_CN) = 0.0
               RPS_REVENUE_MM(NEXT_CN) = 0.0
            ENDIF
            HIGHEST_MARKET_REVENUE(NEXT_CN) = &
                                ENERGY_REVENUE_MM(NEXT_CN) + &
                                   RPS_REVENUE_MM(NEXT_CN) + &
                                     CAPACITY_REVENUE_MM(NEXT_CN)
            NET_MARGIN_PER_KWYR(HIGHEST_OPTION) = 1000.0 * &
                              (HIGHEST_MARKET_REVENUE(NEXT_CN) - &
                                 VARIABLE_COST_MM(NEXT_CN) - &
                                       FIXED_COST_MM(NEXT_CN))/ &
                                         SCREEN_CAPACITY(HIGHEST_OPTION)
!
            IF( ABS(NET_MARGIN_PER_KWYR(HIGHEST_OPTION)) < 0.01) THEN
               HGST_NET_MARG_ADDED(NEXT_CN) = 0.0
               HGST_NET_MARG_ADDED(NEXT_CN) = &
                      NET_MARGIN_PER_KWYR(HIGHEST_OPTION) + &
                                                   CM_IOTA(CM)
               CM_IOTA(CM) = CM_IOTA(CM) - 0.001
            ELSE
               HGST_NET_MARG_ADDED(NEXT_CN) = &
                      NET_MARGIN_PER_KWYR(HIGHEST_OPTION)
            ENDIF
            TG_TOTAL_ANNUAL_CAPACITY_ADDED(TG) = &
                           TG_TOTAL_ANNUAL_CAPACITY_ADDED(TG) + &
                                    CAPACITY_PLANNING_MW(HIGHEST_OPTION)
            IF(CM > 0) THEN
               CM_TOT_ANN_CAPACITY_ADDED(CM) = &
                           CM_TOT_ANN_CAPACITY_ADDED(CM) + &
                                    CAPACITY_PLANNING_MW(HIGHEST_OPTION)
            ENDIF
!
            HIGHEST_MARGIN_PER_KWYR(NEXT_CN) = &
                                       HGST_NET_MARG_ADDED(NEXT_CN)
!
            IF(MARGINAL_ICAP_PER_KWYR_BY_TG(TG) > 999998.0) THEN
               MARGINAL_ICAP_PER_KWYR_BY_TG(TG) = &
                                          ICAP_REVENUE_PER_KWYR(NEXT_CN)
            ENDIF
!
            TEMP_R = HIGHEST_MRX_ENERGY(NEXT_CN)
            IF( TEMP_R > 0.0) THEN
               REVENUE_PER_MWH(NEXT_CN) = &
                          HIGHEST_MARKET_REVENUE(NEXT_CN)*1000.0/TEMP_R
               CAPACITY_REVENUE_PER_MWH(NEXT_CN) = &
                             CAPACITY_REVENUE_MM(NEXT_CN)*1000.0/TEMP_R
               HIGHEST_MRX_MARGIN(NEXT_CN) = 1000.0 * &
                              (HIGHEST_MARKET_REVENUE(NEXT_CN) - &
                                 VARIABLE_COST_MM(NEXT_CN) - &
                                        FIXED_COST_MM(NEXT_CN))/TEMP_R
               RPS_REVENUE_PER_MWH(NEXT_CN) = &
                               1000.0 * RPS_REVENUE_MM(NEXT_CN)/TEMP_R
            ELSE
               REVENUE_PER_MWH(NEXT_CN) = 0.0
               CAPACITY_REVENUE_PER_MWH(NEXT_CN) = 0.0
               HIGHEST_MRX_MARGIN(NEXT_CN) = 0.0
            ENDIF
!
            IF(MX_SUMMARY_REPORT) THEN
               TEMP_I2 = GET_REGIONAL_CM_NAME(CM,CM_NAME)
               WRITE(CM_NAME,1010) 'CAP_'//CM_NAME(1:31)
               WRITE(LOCAL_NAME,1010) &
                            OPTION_NAME(HIGHEST_OPTION)(1:17),HO - I + 1
!
               TEMP_R4 = 20
               MX_ANNUAL_ALT_REC = RPTREC(MX_ANNUAL_ALT_NO)
               WRITE(MX_ANNUAL_ALT_NO,REC=MX_ANNUAL_ALT_REC) &
                        PRT_ENDPOINT(), &
                        FLOAT(THIS_YEAR), &
                        CM_NAME, &
                        LOCAL_NAME, &
                        HIGHEST_STRIKE_PRICE(NEXT_CN), &
                        HIGHEST_ENERGY_REV_PER_MWH(NEXT_CN), & 
                        ! 110309. REDEFINED.
                        HIGHEST_MRX_MARGIN(NEXT_CN), &
                        HIGHEST_MRX_CF(NEXT_CN), &
                        ENERGY_REVENUE_MM(NEXT_CN), &
                        HIGHEST_MARKET_COST(NEXT_CN), & !  6
                        VARIABLE_COST_MM(NEXT_CN), &
                        FIXED_COST_MM(NEXT_CN), &
                        SCREEN_CAPACITY(HIGHEST_OPTION), &
                        HIGHEST_MRX_ENERGY(NEXT_CN), & !  10
                        TOTAL_AVERAGE_COST, & !  NEED DEFINITION
                        HIGHEST_ANN_LEVEL_CAP(NEXT_CN), &
                        HIGHEST_FUEL_COST(NEXT_CN), &
                        HIGHEST_ANNUAL_UNITS(NEXT_CN), &
                        CAPACITY_REVENUE_MM(NEXT_CN), &
                        HIGHEST_MARKET_REVENUE(NEXT_CN), & 
                        ! 16 TOTAL REVENUE
                        HIGHEST_MARGIN_PER_KWYR(NEXT_CN), &
                        HIGHEST_FUEL_COST_PER_MWH(NEXT_CN), &
                        HIGHEST_VAR_OM_PER_MWH(NEXT_CN), & !  19
                        HIGHEST_FUEL_COST_PER_MMBTU(NEXT_CN), &
                        REVENUE_PER_MWH(NEXT_CN), &
                        CAPACITY_REVENUE_PER_MWH(NEXT_CN), &
                        HIGHEST_EMISSIONS_PER_MWH(NEXT_CN), &
                        HIGHEST_FOM_PER_MWH(NEXT_CN), & !  24
                        HIGHEST_LEVEL_CAP_PER_MWH(NEXT_CN), &
                        HIGHEST_VOM_COST_MM(NEXT_CN), &
                        HIGHEST_EMISSIONS_COST_MM(NEXT_CN), & !  27
                        HIGHEST_FOM_COST_MM(NEXT_CN), &
                        FLOAT(TRANSACTION_GROUP(HIGHEST_OPTION)), &
                        TG_CAPACITY_MW(TG), &
                        TG_PEAK_MW(TG), &
                        TG_RESERVE_MARGIN(TG), &
                        FLOAT(CM_INDEX), & !  PLANNING_AREA_ID(NEXT_CN),
                        CM_CAPACITY_MW(CM), &
                        CM_PEAK_MW(CM), &
                        CM_RESERVE_MARGIN(CM), &
                        ICAP_DEMAND_CURVE_MULT(NEXT_CN), &
                        CONE_COST_PER_KWYR(NEXT_CN), & !  38
                        CONE_DEMAND_CURVE_ADJ_PER_KWYR(NEXT_CN), &
                        EAS_PROFIT_PER_KWYR(NEXT_CN), &
                        ICAP_REVENUE_PER_KWYR(NEXT_CN), &
                        HIGHEST_HEAT_RATE(NEXT_CN), & !  42
                        FIXED_COST_PER_MWH(NEXT_CN), &
                        HIGHEST_RFT(NEXT_CN), &
                        HIGHEST_RPM(NEXT_CN), &
                        TG_Net_Transfer(TG), &
                        CM_Net_Transfer(CM), &
                        TEMP_R4, & !  MRX_ICAP_INDEX(NEXT_CN),
                        CM_INTERUPTIBLRESERVE_MARGIN, &
                        CM_PEAK_AFTER_INTERRUPTIBLE(CM), &
                        CAPACITY_PLANNING_MW(HIGHEST_OPTION), &
                        RPS_REVENUE_MM(NEXT_CN), &
                        RPS_REVENUE_PER_MWH(NEXT_CN)
               MX_ANNUAL_ALT_REC = MX_ANNUAL_ALT_REC + 1
!
            ENDIF
         ENDDO ! HO LIST ! 051210. BIG TEST
!
         CALL SortIncrPos(                HO, &
                                          SORTED_OPTIONS, &
                                          HGST_NET_MARG_ADDED)
!
         TOTAL_ANNUAL_CAPACITY_ADDED = 0.
         CM_TOT_ANN_CAPACITY_ADDED = 0.
         TG_TOTAL_ANNUAL_CAPACITY_ADDED = 0.
!
! 082716. NEW RPS BUILD SECTION
! STATE_SURPLUS_VARS 0:6 WHERE ZERO IS TOTAL
! USED TO TRACK RPS ADDITIONS
!
      IF(1 == 2) THEN
      RPS_RESOURCE_ADDED = 0
      IF(GRX_RPS_MODULE_ACTIVE) THEN
         CALL GET_ANNUAL_STATE_SURPLUS_DATA(ZERO,SYSTEM_SURPLUS_VARS)
         CALL GET_ANNUAL_REGION_DATA( &
                                      NUM_RPS_REGIONS, &
                                      RPS_STATE_REGION, &
                                      RPS_REGION_POSITION)
         ST = 62
         REGION = 0
         RPS_SYSTEM = 0
         DO
            IF(ST > 1) THEN
               ST = ST - 1
               IF(RPS_STATE_REGION(ST)> 0) THEN
                  CYCLE
               ENDIF
               CALL GET_ANNUAL_STATE_SURPLUS_DATA(ST,STATE_SURPLUS_VARS)
            ELSE
               ST = 0
               IF(REGION < NUM_RPS_REGIONS) THEN
                  REGION = REGION + 1
                  CALL GET_ANNUAL_STATE_SURPLUS_DATA(-REGION, &
                                                     STATE_SURPLUS_VARS)
               ELSEIF(RPS_SYSTEM < 1) THEN
                  REGION = NUM_RPS_REGIONS + 1
                  RPS_SYSTEM = 1
                  STATE_SURPLUS_VARS = SYSTEM_SURPLUS_VARS
               ELSE
                  EXIT
               ENDIF
            ENDIF

            DO J = 7, 0, -1 ! DEFICIT RPS TO PRIM MOVER LIST (ZERO=ALL)
               IF(STATE_SURPLUS_VARS(J) > -0.0001) CYCLE ! SURPLUS
               RPS_PM_INDEX = J + 1
               RPS_PM_INDEX = RPS_INDEX_TRANSLATE(RPS_PM_INDEX)
               DO I = HO, 1, -1
                  IF(STATE_SURPLUS_VARS(J) > -0.0001) EXIT ! SURPLUS
                  NEXT_CN = SORTED_OPTIONS(I)
                  PM = HIGHEST_RPM(NEXT_CN)
                  HIGHEST_OPTION = HGST_OPT_INDEX(NEXT_CN)
                  OPTION_REGION = RPS_STATE_REGION( &
                                         STATE_PROVINCE(HIGHEST_OPTION))
                  OPTION_REGION = RPS_REGION_POSITION(OPTION_REGION)
                  IF( (J > 0 .AND. ABS(RPS_PM_INDEX - PM) > 0 ) .OR. &
                       (ST > 0 .AND. &
                    ABS(STATE_PROVINCE(HIGHEST_OPTION) - ST) > 0) .OR. &
                     (REGION > 0 .AND. REGION <= NUM_RPS_REGIONS .AND. &
                      ABS(OPTION_REGION - REGION) > 0) .OR. &
                       HIGHEST_MRX_ENERGY(NEXT_CN) * &
                              RPS_PERCENT(HIGHEST_OPTION) < 0.001 .OR. &
                                  RPS_RESOURCE_ADDED(NEXT_CN) > 0 .OR. &
                                     .NOT. IS_A_VALID_RPS_PM(PM) ) CYCLE
                  TG = GET_TRANS_GROUP_POSITION( &
                                      TRANSACTION_GROUP(HIGHEST_OPTION))
                  PA = TG_2_PLANNING_AREA(TG)
                  CM = TG_2_CAPACITY_MARKET(TG)
                  CM_INDEX = GET_CM_INDEX_FROM_TG(CM)
                  IF(TG_MW_TRANSFERS) THEN
                     TG_CAPACITY_MW(TG) = &
                           TG_PLANNING_CAPACITY(TG) + &
                              TG_TOTAL_ANNUAL_CAPACITY_ADDED(TG) + & ! -
                                          TG_Net_Transfer(TG)
                  ELSE
                     TG_CAPACITY_MW(TG) = &
                           TG_PLANNING_CAPACITY(TG) + &
                                 TG_TOTAL_ANNUAL_CAPACITY_ADDED(TG)
                  ENDIF
                  IF(TG_PEAK_MW(TG) > .01) THEN
                     TG_RESERVE_MARGIN(TG) = 100. * &
                              (TG_CAPACITY_MW(TG)/TG_PEAK_MW(TG) - 1.0)
                  ELSE
                     TG_RESERVE_MARGIN(TG) = 100.
                  ENDIF
!
                  IF(TEST_CAP_VALUE_W_ADD) THEN
                     TEMP_R4 = CAPACITY_PLANNING_MW(HIGHEST_OPTION)
                  ELSE
                     TEMP_R4 = 0.0
                  ENDIF
                  IF(TG_MW_TRANSFERS) THEN
                     CM_CAPACITY_MW(CM) = &
                           CM_PLANNING_CAPACITY(CM) + &
                                 CM_TOT_ANN_CAPACITY_ADDED(CM) + &
                                    CM_Net_Transfer(CM) + &
                                       TEMP_R4
                  ELSE
                     CM_CAPACITY_MW(CM) = &
                           CM_PLANNING_CAPACITY(CM) + &
                              CM_TOT_ANN_CAPACITY_ADDED(CM) + &
                                       TEMP_R4
                  ENDIF
                  IF(CM_PEAK_MW(CM) > .01) THEN
                     CM_RESERVE_MARGIN(CM) = 100. * &
                              (CM_CAPACITY_MW(CM)/CM_PEAK_MW(CM) - 1.0)
                     CM_INTERUPTIBLRESERVE_MARGIN = 100. * &
                         (CM_CAPACITY_MW(CM) - &
                                    CM_PEAK_AFTER_INTERRUPTIBLE(CM)) / &
                                CM_PEAK_AFTER_INTERRUPTIBLE(CM)
                  ELSE
                     CM_RESERVE_MARGIN(CM) = 100.
                  ENDIF
!
                  FIRST_YEAR_CAPACITY = &
                     ADD_THIS_UNIT( &
                           OPTION_POSITION(HIGHEST_OPTION),THIS_YEAR + &
                                        LOCAL_LEAD_TIME(HIGHEST_OPTION))
!
                  IF(INDEPENDENT(HIGHEST_OPTION) > 0) THEN
                     HYBRID_PROD_POINTER = INDEPENDENT(HIGHEST_OPTION)
                     FIRST_YEAR_CAPACITY = &
                        ADD_THIS_UNIT( &
                      OPTION_POSITION(HYBRID_PROD_POINTER),THIS_YEAR + &
                                   LOCAL_LEAD_TIME(HYBRID_PROD_POINTER))
                  ENDIF
!
                  RPS_RESOURCE_ADDED(NEXT_CN) = 1
                  IF(MX_SUMMARY_REPORT) THEN
                     TEMP_I2 = GET_REGIONAL_CM_NAME(CM,CM_NAME)
                     IF(INDEPENDENT(HIGHEST_OPTION) > 0) THEN
                        WRITE(LOCAL_NAME,1017) &
                           OPTION_NAME(HIGHEST_OPTION)(1:15), &
                                                         ' H',HO - I + 1
                     ELSE
                        WRITE(LOCAL_NAME,1010) &
                            OPTION_NAME(HIGHEST_OPTION)(1:17),HO - I + 1
                     ENDIF
                     IF(ST == 0) THEN
                        MRX_ICAP_INDEX(NEXT_CN) = 10
                     ELSE
                        MRX_ICAP_INDEX(NEXT_CN) = 11
                     ENDIF
                     MX_ANNUAL_ALT_REC = RPTREC(MX_ANNUAL_ALT_NO)
                     WRITE(MX_ANNUAL_ALT_NO,REC=MX_ANNUAL_ALT_REC) &
                        PRT_ENDPOINT(), &
                        FLOAT(THIS_YEAR), &
                        CM_NAME, &
                        LOCAL_NAME, &
                        HIGHEST_STRIKE_PRICE(NEXT_CN), &
                        HIGHEST_ENERGY_REV_PER_MWH(NEXT_CN), & 
                        ! 110309. REDEFINED.
                        HIGHEST_MRX_MARGIN(NEXT_CN), &
                        HIGHEST_MRX_CF(NEXT_CN), &
                        ENERGY_REVENUE_MM(NEXT_CN), &
                        HIGHEST_MARKET_COST(NEXT_CN), & !  6
                        VARIABLE_COST_MM(NEXT_CN), &
                        FIXED_COST_MM(NEXT_CN), &
                        SCREEN_CAPACITY(HIGHEST_OPTION), &
                        HIGHEST_MRX_ENERGY(NEXT_CN), & !  10
                        TOTAL_AVERAGE_COST, & !  NEED DEFINITION
                        HIGHEST_ANN_LEVEL_CAP(NEXT_CN), &
                        HIGHEST_FUEL_COST(NEXT_CN), &
                        HIGHEST_ANNUAL_UNITS(NEXT_CN), &
                        CAPACITY_REVENUE_MM(NEXT_CN), &
                        HIGHEST_MARKET_REVENUE(NEXT_CN), & 
                        ! 16 TOTAL REVENUE
                        HIGHEST_MARGIN_PER_KWYR(NEXT_CN), &
                        HIGHEST_FUEL_COST_PER_MWH(NEXT_CN), &
                        HIGHEST_VAR_OM_PER_MWH(NEXT_CN), & !  19
                        HIGHEST_FUEL_COST_PER_MMBTU(NEXT_CN), &
                        REVENUE_PER_MWH(NEXT_CN), &
                        CAPACITY_REVENUE_PER_MWH(NEXT_CN), &
                        HIGHEST_EMISSIONS_PER_MWH(NEXT_CN), &
                        HIGHEST_FOM_PER_MWH(NEXT_CN), & !  24
                        HIGHEST_LEVEL_CAP_PER_MWH(NEXT_CN), &
                        HIGHEST_VOM_COST_MM(NEXT_CN), &
                        HIGHEST_EMISSIONS_COST_MM(NEXT_CN), & !  27
                        HIGHEST_FOM_COST_MM(NEXT_CN), &
                        FLOAT(TRANSACTION_GROUP(HIGHEST_OPTION)), &
                        TG_CAPACITY_MW(TG), &
                        TG_PEAK_MW(TG), &
                        TG_RESERVE_MARGIN(TG), &
                        FLOAT(CM_INDEX), & !  PLANNING_AREA_ID(NEXT_CN),
                        CM_CAPACITY_MW(CM), &
                        CM_PEAK_MW(CM), &
                        CM_RESERVE_MARGIN(CM), &
                        ICAP_DEMAND_CURVE_MULT(NEXT_CN), &
                        CONE_COST_PER_KWYR(NEXT_CN), & !  38
                        CONE_DEMAND_CURVE_ADJ_PER_KWYR(NEXT_CN), &
                        EAS_PROFIT_PER_KWYR(NEXT_CN), &
                        ICAP_REVENUE_PER_KWYR(NEXT_CN), &
                        HIGHEST_HEAT_RATE(NEXT_CN), & !  42
                        FIXED_COST_PER_MWH(NEXT_CN), &
                        HIGHEST_RFT(NEXT_CN), &
                        HIGHEST_RPM(NEXT_CN), &
                        TG_Net_Transfer(TG), &
                        CM_Net_Transfer(CM), &
                        MRX_ICAP_INDEX(NEXT_CN), &
                        CM_INTERUPTIBLRESERVE_MARGIN, &
                        CM_PEAK_AFTER_INTERRUPTIBLE(CM), &
                        CAPACITY_PLANNING_MW(HIGHEST_OPTION), &
                        RPS_REVENUE_MM(NEXT_CN), &
                        RPS_REVENUE_PER_MWH(NEXT_CN)
                     MX_ANNUAL_ALT_REC = MX_ANNUAL_ALT_REC + 1
                  ENDIF
!
!                 RECORD THREE CAPACITY ADDED VARIABLES
!
                  IF(PA> 0) THEN
                     TOTAL_ANNUAL_CAPACITY_ADDED(PA) = &
                           TOTAL_ANNUAL_CAPACITY_ADDED(PA) + &
                                    CAPACITY_PLANNING_MW(HIGHEST_OPTION)
                  ENDIF
                  TOTAL_ANNUAL_CAPACITY_ADDED(0) = &
                           TOTAL_ANNUAL_CAPACITY_ADDED(0) + &
                                    CAPACITY_PLANNING_MW(HIGHEST_OPTION)
                  IF(CM > 0) THEN
                     CM_TOT_ANN_CAPACITY_ADDED(CM) = &
                           CM_TOT_ANN_CAPACITY_ADDED(CM) + &
                                    CAPACITY_PLANNING_MW(HIGHEST_OPTION)
                  ENDIF
                  TG_TOTAL_ANNUAL_CAPACITY_ADDED(TG) = &
                           TG_TOTAL_ANNUAL_CAPACITY_ADDED(TG) + &
                                    CAPACITY_PLANNING_MW(HIGHEST_OPTION)
!
!                 OTHER VARIABLES
!
                  LAST_SORTED_POSITION = HIGHEST_OPTION
                  LAST_RESOURCE_ADDED(TG) = HIGHEST_OPTION
                  LAST_PA_RESOURCE_ADDED(PA) = NEXT_CN
                  LAST_CM_RESOURCE_ADDED(CM) = NEXT_CN
                  INSTALL_CAP_VALUE_LAST_PA_UNIT(PA) = &
                                HIGHEST_ANN_LEVEL_CAP(HIGHEST_OPTION)
                  INSTALL_CAP_VALUE_LAST_CM_UNIT(CM) = &
                                HIGHEST_ANN_LEVEL_CAP(HIGHEST_OPTION)
                  STATE_SURPLUS_VARS(J) = STATE_SURPLUS_VARS(J) + &
                                         HIGHEST_MRX_ENERGY(NEXT_CN)* &
                                           RPS_PERCENT(HIGHEST_OPTION)
                  IF(J > 0) THEN
                     STATE_SURPLUS_VARS(0) = STATE_SURPLUS_VARS(0) + &
                                          HIGHEST_MRX_ENERGY(NEXT_CN)* &
                                           RPS_PERCENT(HIGHEST_OPTION)
                  ENDIF
                  IF(ST > 0 .OR. (REGION > 0 .AND. &
                                       REGION <= NUM_RPS_REGIONS) ) THEN
                     SYSTEM_SURPLUS_VARS(J) = SYSTEM_SURPLUS_VARS(J) + &
                                          HIGHEST_MRX_ENERGY(NEXT_CN)* &
                                           RPS_PERCENT(HIGHEST_OPTION)
                     IF(J > 0) THEN
                        SYSTEM_SURPLUS_VARS(0) = &
                                  SYSTEM_SURPLUS_VARS(0) + &
                                          HIGHEST_MRX_ENERGY(NEXT_CN)* &
                                           RPS_PERCENT(HIGHEST_OPTION)
                     ENDIF
                  ENDIF
               ENDDO ! RESOURCE ADDITIONS
            ENDDO ! RPS TYPE
         ENDDO ! STATE AND ZERO
      ENDIF ! RPS MODULE ACTIVE
!
      ELSE
!
      IF(R_YEAR == 7) THEN
         J = J
      ENDIF
!
      RPS_RESOURCE_ADDED = 0
!
! 080120. RPS ONLY MUTUALLY EXCLUSIVE FROM GRX_RPS_MODULE_ACTIVE
! 081620. PER 8/14/20 MEETING, PASS SUPPLY CURVE INFORMATION.
!
      IF(RPS_ONLY_SWITCH_ACTIVE) THEN

         RPS_PROGRAMS = NUMBER_OF_RPS_PROGRAMS()
!
         DO J = 1, RPS_PROGRAMS
            RPS_REC_PRICE = RPS_REC_PRICE_BY(J)
            RPS_GEN_WEIGHTED_PRICE = RPS_GEN_PRICE_BY(J)
            TEMP_R = PUT_RPS_PRICE_BY_PROGRAM(J,RPS_REC_PRICE, &
                                            RPS_GEN_WEIGHTED_PRICE)
         ENDDO
         DO J = 1, RPS_PROGRAMS

            RPS_PROGRAM_BALANCE = &
                        GET_LOCAL_ENDING_BALANCE(R_YEAR,J,RPS_ALT_PRICE)
            DO I = HO, 1, -1

               NEXT_CN = SORTED_OPTIONS(I)
               PM = HIGHEST_RPM(NEXT_CN)
               HIGHEST_OPTION = HGST_OPT_INDEX(NEXT_CN)
               RPS_NO = RPS_PROGRAM_NUMBER(HIGHEST_OPTION)
!
! 081920. REC PRICE FROM RPS SUPPLY CURVE FOR RPS ONLY.
! 081920. BEGIN
!
                  IF( .NOT. IS_IN_RPS_PROGRAM(J,RPS_NO)) CYCLE
                  IF(MRX_RPS_MX_UNIT_CLEARED(NEXT_CN) < 1 .OR. &
                                 RPS_RESOURCE_ADDED(NEXT_CN) == 1) THEN
                     CYCLE
                  ENDIF
! 013021.
                  IF(RPS_NO /= 0) THEN
                     RPS_COUNT = 1
                     RPS_NO = GET_RPS_PROGRAM_POSITION(RPS_NO,RPS_COUNT)
                     IF(RPS_NO /= 0) THEN
                        RPS_REC_PRICE = RPS_REC_PRICE_BY(RPS_NO)
                        RPS_GEN_WEIGHTED_PRICE = &
                                                RPS_GEN_PRICE_BY(RPS_NO)
                     ENDIF
                  ENDIF
                  RPS_REVENUE_PER_MWH(NEXT_CN) = RPS_REC_PRICE
                  RPS_REVENUE_MM(NEXT_CN) = 0.001 * RPS_REC_PRICE * &
                           HIGHEST_MRX_ENERGY(NEXT_CN) * &
                              RPS_PERCENT(HIGHEST_OPTION)
! 081920. END
                  TG = GET_TRANS_GROUP_POSITION( &
                                      TRANSACTION_GROUP(HIGHEST_OPTION))
                  PA = TG_2_PLANNING_AREA(TG)
                  CM = TG_2_CAPACITY_MARKET(TG)
                  CM_INDEX = GET_CM_INDEX_FROM_TG(CM)
                  IF(TG_MW_TRANSFERS) THEN
                     TG_CAPACITY_MW(TG) = &
                           TG_PLANNING_CAPACITY(TG) + &
                              TG_TOTAL_ANNUAL_CAPACITY_ADDED(TG) + & ! -
                                          TG_Net_Transfer(TG)
                  ELSE
                     TG_CAPACITY_MW(TG) = &
                           TG_PLANNING_CAPACITY(TG) + &
                                 TG_TOTAL_ANNUAL_CAPACITY_ADDED(TG)
                  ENDIF
                  IF(TG_PEAK_MW(TG) > .01) THEN
                     TG_RESERVE_MARGIN(TG) = 100. * &
                              (TG_CAPACITY_MW(TG)/TG_PEAK_MW(TG) - 1.0)
                  ELSE
                     TG_RESERVE_MARGIN(TG) = 100.
                  ENDIF
!
                  IF(TEST_CAP_VALUE_W_ADD) THEN
                     TEMP_R4 = CAPACITY_PLANNING_MW(HIGHEST_OPTION)
                  ELSE
                     TEMP_R4 = 0.0
                  ENDIF
                  IF(TG_MW_TRANSFERS) THEN
                     CM_CAPACITY_MW(CM) = &
                           CM_PLANNING_CAPACITY(CM) + &
                                 CM_TOT_ANN_CAPACITY_ADDED(CM) + &
                                    CM_Net_Transfer(CM) + &
                                       TEMP_R4
                  ELSE
                     CM_CAPACITY_MW(CM) = &
                           CM_PLANNING_CAPACITY(CM) + &
                              CM_TOT_ANN_CAPACITY_ADDED(CM) + &
                                       TEMP_R4
                  ENDIF
                  IF(CM_PEAK_MW(CM) > .01) THEN
                     CM_RESERVE_MARGIN(CM) = 100. * &
                              (CM_CAPACITY_MW(CM)/CM_PEAK_MW(CM) - 1.0)
                     CM_INTERUPTIBLRESERVE_MARGIN = 100. * &
                         (CM_CAPACITY_MW(CM) - &
                                    CM_PEAK_AFTER_INTERRUPTIBLE(CM)) / &
                                CM_PEAK_AFTER_INTERRUPTIBLE(CM)
                  ELSE
                     CM_RESERVE_MARGIN(CM) = 100.
                  ENDIF
!
                  FIRST_YEAR_CAPACITY = &
                     ADD_THIS_UNIT( &
                           OPTION_POSITION(HIGHEST_OPTION),THIS_YEAR + &
                                        LOCAL_LEAD_TIME(HIGHEST_OPTION))
                  RPS_RESOURCE_ADDED(NEXT_CN) = 1
!
                  TEMP_I2 = RPS_PROGRAM_NUMBER(HIGHEST_OPTION)
                  INT2_PM = INT(HIGHEST_RPM(NEXT_CN),2)
                  TEMP_R = PUT_RPS_PRICE_BY_OPTION( &
                                 TEMP_I2, &
                                 RPS_REC_PRICE, &
                                 1000.0*HIGHEST_MRX_ENERGY(NEXT_CN), &
                                 SCREEN_CAPACITY(HIGHEST_OPTION), &
                                 INT2_PM, &
                                 R_YEAR, &
                                 GRX_RPS_MODULE_ACTIVE, &
                                 RPS_GEN_WEIGHTED_PRICE)
                  HIGHEST_MARKET_REVENUE(NEXT_CN) = &
                                ENERGY_REVENUE_MM(NEXT_CN) + &
                                   RPS_REVENUE_MM(NEXT_CN) + &
                                     CAPACITY_REVENUE_MM(NEXT_CN)
                  HIGHEST_MARGIN_PER_KWYR(NEXT_CN) = 1000.0 * &
                              (HIGHEST_MARKET_REVENUE(NEXT_CN) - &
                                 VARIABLE_COST_MM(NEXT_CN) - &
                                       FIXED_COST_MM(NEXT_CN))/ &
                                   SCREEN_CAPACITY(HIGHEST_OPTION)
                  TEMP_R = HIGHEST_MRX_ENERGY(NEXT_CN) * &
                              RPS_PERCENT(HIGHEST_OPTION)
                  IF( TEMP_R > 0.0) THEN
                     REVENUE_PER_MWH(NEXT_CN) = &
                          HIGHEST_MARKET_REVENUE(NEXT_CN)*1000.0/TEMP_R
                     CAPACITY_REVENUE_PER_MWH(NEXT_CN) = &
                             CAPACITY_REVENUE_MM(NEXT_CN)*1000.0/TEMP_R
                     HIGHEST_MRX_MARGIN(NEXT_CN) = 1000.0 * &
                              (HIGHEST_MARKET_REVENUE(NEXT_CN) - &
                                 VARIABLE_COST_MM(NEXT_CN) - &
                                        FIXED_COST_MM(NEXT_CN))/TEMP_R
                  ELSE
                     REVENUE_PER_MWH(NEXT_CN) = 0.0
                     CAPACITY_REVENUE_PER_MWH(NEXT_CN) = 0.0
                     HIGHEST_MRX_MARGIN(NEXT_CN) = 0.0
                  ENDIF
                  IF(MX_SUMMARY_REPORT) THEN
                     TEMP_I2 = GET_REGIONAL_CM_NAME(CM,CM_NAME)
                     WRITE(LOCAL_NAME,1010) &
                            OPTION_NAME(HIGHEST_OPTION)(1:17),HO - I + 1
!
                        MRX_ICAP_INDEX(NEXT_CN) = 10
                     MX_ANNUAL_ALT_REC = RPTREC(MX_ANNUAL_ALT_NO)
!
                     WRITE(MX_ANNUAL_ALT_NO,REC=MX_ANNUAL_ALT_REC) &
                        PRT_ENDPOINT(), &
                        FLOAT(THIS_YEAR), &
                        CM_NAME, &
                        LOCAL_NAME, &
                        HIGHEST_STRIKE_PRICE(NEXT_CN), &
                        HIGHEST_ENERGY_REV_PER_MWH(NEXT_CN), & 
                        ! 110309. REDEFINED.
                        HIGHEST_MRX_MARGIN(NEXT_CN), &
                        HIGHEST_MRX_CF(NEXT_CN), &
                        ENERGY_REVENUE_MM(NEXT_CN), &
                        HIGHEST_MARKET_COST(NEXT_CN), & !  6
                        VARIABLE_COST_MM(NEXT_CN), &
                        FIXED_COST_MM(NEXT_CN), &
                        SCREEN_CAPACITY(HIGHEST_OPTION), &
                        HIGHEST_MRX_ENERGY(NEXT_CN), & !  10
                        TOTAL_AVERAGE_COST, & !  NEED DEFINITION
                        HIGHEST_ANN_LEVEL_CAP(NEXT_CN), &
                        HIGHEST_FUEL_COST(NEXT_CN), &
                        HIGHEST_ANNUAL_UNITS(NEXT_CN), &
                        CAPACITY_REVENUE_MM(NEXT_CN), &
                        HIGHEST_MARKET_REVENUE(NEXT_CN), & 
                        ! 16 TOTAL REVENUE
                        HIGHEST_MARGIN_PER_KWYR(NEXT_CN), &
                        HIGHEST_FUEL_COST_PER_MWH(NEXT_CN), &
                        HIGHEST_VAR_OM_PER_MWH(NEXT_CN), & !  19
                        HIGHEST_FUEL_COST_PER_MMBTU(NEXT_CN), &
                        REVENUE_PER_MWH(NEXT_CN), &
                        CAPACITY_REVENUE_PER_MWH(NEXT_CN), &
                        HIGHEST_EMISSIONS_PER_MWH(NEXT_CN), &
                        HIGHEST_FOM_PER_MWH(NEXT_CN), & !  24
                        HIGHEST_LEVEL_CAP_PER_MWH(NEXT_CN), &
                        HIGHEST_VOM_COST_MM(NEXT_CN), &
                        HIGHEST_EMISSIONS_COST_MM(NEXT_CN), & !  27
                        HIGHEST_FOM_COST_MM(NEXT_CN), &
                        FLOAT(TRANSACTION_GROUP(HIGHEST_OPTION)), &
                        TG_CAPACITY_MW(TG), &
                        TG_PEAK_MW(TG), &
                        TG_RESERVE_MARGIN(TG), &
                        FLOAT(CM_INDEX), & !  PLANNING_AREA_ID(NEXT_CN),
                        CM_CAPACITY_MW(CM), &
                        CM_PEAK_MW(CM), &
                        CM_RESERVE_MARGIN(CM), &
                        ICAP_DEMAND_CURVE_MULT(NEXT_CN), &
                        CONE_COST_PER_KWYR(NEXT_CN), & !  38
                        CONE_DEMAND_CURVE_ADJ_PER_KWYR(NEXT_CN), &
                        EAS_PROFIT_PER_KWYR(NEXT_CN), &
                        ICAP_REVENUE_PER_KWYR(NEXT_CN), &
                        HIGHEST_HEAT_RATE(NEXT_CN), & !  42
                        FIXED_COST_PER_MWH(NEXT_CN), &
                        HIGHEST_RFT(NEXT_CN), &
                        HIGHEST_RPM(NEXT_CN), &
                        TG_Net_Transfer(TG), &
                        CM_Net_Transfer(CM), &
                        MRX_ICAP_INDEX(NEXT_CN), &
                        CM_INTERUPTIBLRESERVE_MARGIN, &
                        CM_PEAK_AFTER_INTERRUPTIBLE(CM), &
                        CAPACITY_PLANNING_MW(HIGHEST_OPTION), &
                        RPS_REVENUE_MM(NEXT_CN), &
                        RPS_REVENUE_PER_MWH(NEXT_CN)
                     MX_ANNUAL_ALT_REC = MX_ANNUAL_ALT_REC + 1
                  ENDIF
!
!                 RECORD THREE CAPACITY ADDED VARIABLES
!
                  IF(PA> 0) THEN
                     TOTAL_ANNUAL_CAPACITY_ADDED(PA) = &
                           TOTAL_ANNUAL_CAPACITY_ADDED(PA) + &
                                    CAPACITY_PLANNING_MW(HIGHEST_OPTION)
                  ENDIF
                  TOTAL_ANNUAL_CAPACITY_ADDED(0) = &
                           TOTAL_ANNUAL_CAPACITY_ADDED(0) + &
                                    CAPACITY_PLANNING_MW(HIGHEST_OPTION)
                  IF(CM > 0) THEN
                     CM_TOT_ANN_CAPACITY_ADDED(CM) = &
                           CM_TOT_ANN_CAPACITY_ADDED(CM) + &
                                    CAPACITY_PLANNING_MW(HIGHEST_OPTION)
                  ENDIF
                  TG_TOTAL_ANNUAL_CAPACITY_ADDED(TG) = &
                           TG_TOTAL_ANNUAL_CAPACITY_ADDED(TG) + &
                                    CAPACITY_PLANNING_MW(HIGHEST_OPTION)
!
!                 OTHER VARIABLES
!
                  LAST_SORTED_POSITION = HIGHEST_OPTION
                  LAST_RESOURCE_ADDED(TG) = HIGHEST_OPTION
                  LAST_PA_RESOURCE_ADDED(PA) = NEXT_CN
                  LAST_CM_RESOURCE_ADDED(CM) = NEXT_CN
                  INSTALL_CAP_VALUE_LAST_PA_UNIT(PA) = &
                                HIGHEST_ANN_LEVEL_CAP(HIGHEST_OPTION)
                  INSTALL_CAP_VALUE_LAST_CM_UNIT(CM) = &
                                HIGHEST_ANN_LEVEL_CAP(HIGHEST_OPTION)

!
                  RPS_PROGRAM_BALANCE = RPS_PROGRAM_BALANCE + &
                                 1000.0 * HIGHEST_MRX_ENERGY(NEXT_CN)* &
                                           RPS_PERCENT(HIGHEST_OPTION)
               ENDDO ! RESOURCE ADDITIONS
         ENDDO ! RPS_PROGRAM
      ENDIF ! RPS MODULE ACTIVE
      ENDIF ! NEW OR OLD RPS_ROUTINE
!
         IF(.NOT. RPS_ONLY_SWITCH_ACTIVE) THEN
            TEMP_L = RESET_MRX_RPS_TEST_REQ() ! 022019.
         ENDIF
         DO I = HO, 1, -1
!
! MAXIMUM RESERVE MARGIN CONDITION
!
               NEXT_CN = SORTED_OPTIONS(I)
            HIGHEST_OPTION = HGST_OPT_INDEX(NEXT_CN)
!
            IF(RPS_RESOURCE_ADDED(NEXT_CN) > 0) CYCLE
            IF(.NOT. DEPENDENT_UNIT_AVAILABLE(HIGHEST_OPTION)) THEN
               MRX_ICAP_INDEX(NEXT_CN) = 9
               CYCLE
            ENDIF
!
            TG = GET_TRANS_GROUP_POSITION( &
                                      TRANSACTION_GROUP(HIGHEST_OPTION))
            PA = TG_2_PLANNING_AREA(TG)
            CM = TG_2_CAPACITY_MARKET(TG)
            CM_INDEX = GET_CM_INDEX_FROM_TG(CM)
!
            IF(TEST_CAP_VALUE_W_ADD) THEN
               TEMP_R4 = CAPACITY_PLANNING_MW(HIGHEST_OPTION)
            ELSE
               TEMP_R4 = 0.0
            ENDIF
            IF(TG_MW_TRANSFERS) THEN
               CM_CAPACITY_MW(CM) = &
                           CM_PLANNING_CAPACITY(CM) + &
                                 CM_TOT_ANN_CAPACITY_ADDED(CM) + &
                                    CM_Net_Transfer(CM) + &
                                       TEMP_R4
            ELSE
               CM_CAPACITY_MW(CM) = &
                           CM_PLANNING_CAPACITY(CM) + &
                              CM_TOT_ANN_CAPACITY_ADDED(CM) + &
                                       TEMP_R4
            ENDIF
!
            IF(CM_PEAK_MW(CM) > .01) THEN
               CM_RESERVE_MARGIN(CM) = 100. * &
                              (CM_CAPACITY_MW(CM)/CM_PEAK_MW(CM) - 1.0)
               CM_INTERUPTIBLRESERVE_MARGIN = 100. * &
                         (CM_CAPACITY_MW(CM) - &
                                    CM_PEAK_AFTER_INTERRUPTIBLE(CM)) / &
                                CM_PEAK_AFTER_INTERRUPTIBLE(CM)
            ELSE
               CM_RESERVE_MARGIN(CM) = 100.
            ENDIF
!
            TEMP_R4 = CM_RESERVE_MARGIN(CM)*0.01 + 1.0
            TEMP_R4 = CM_INTERUPTIBLRESERVE_MARGIN*0.01 + 1.0
!
            ICAP_DEMAND_CURVE_MULT(NEXT_CN) = &
                                GET_ICAP_REVENUE_MULT(R_YEAR,CM,TEMP_R4)
            IF(SCREEN_CAPACITY(HIGHEST_OPTION) > 0.01) THEN
               IF(ICAP_SWITCH_INDEX(CM) == 2) THEN
                  CONE_COST_PER_KWYR(NEXT_CN) = &
                                   LOWEST_FIXED_COST_BY_CM(CM)
               ELSE
                  CONE_COST_PER_KWYR(NEXT_CN) = ICAP_CONE_PRICE(CM)
               ENDIF
               CONE_DEMAND_CURVE_ADJ_PER_KWYR(NEXT_CN) = &
                                    CONE_COST_PER_KWYR(NEXT_CN) * &
                                    ICAP_DEMAND_CURVE_MULT(NEXT_CN)
               IF(ICAP_SWITCH_INDEX(CM) == 2 .OR.   & !  MARGINAL
                                        ICAP_SWITCH_INDEX(CM) == 3) THEN
                                        ! CONE-MRX
                  CAPACITY_REVENUE_MM(NEXT_CN) = &
                         MIN(CONE_DEMAND_CURVE_ADJ_PER_KWYR(NEXT_CN) * &
                           CAPACITY_PLANNING_MW(HIGHEST_OPTION)*0.001, &
                                MAX(0., &
                                 FIXED_COST_MM(NEXT_CN) + &
                                    VARIABLE_COST_MM(NEXT_CN) - &
                                         ENERGY_REVENUE_MM(NEXT_CN)))
                  ICAP_REVENUE_PER_KWYR(NEXT_CN) = &
                          MAX(0.0,CAPACITY_REVENUE_MM(NEXT_CN)*1000.0/ &
                                       SCREEN_CAPACITY(HIGHEST_OPTION))
               ELSE
                  ICAP_REVENUE_PER_KWYR(NEXT_CN) = &
                           MIN(ICAP_CONE_PRICE(CM), &
                                CONE_DEMAND_CURVE_ADJ_PER_KWYR(NEXT_CN))
                  CAPACITY_REVENUE_MM(NEXT_CN) = &
                       ICAP_REVENUE_PER_KWYR(NEXT_CN) * &
                            CAPACITY_PLANNING_MW(HIGHEST_OPTION) * 0.001
               ENDIF
!
               PRICE_OF_TRANSFER = ICAP_REVENUE_PER_KWYR(NEXT_CN)
               QUANTITY_OF_TRANSFER = 0.0
!
               BEST_BUYER_TG = 0
               BEST_BUYER_CM = 0
               BEST_BUYER_PA = 0
!
               DO K = 1, NUMBER_DIRECT_INTERCONNECTS(TG)
                  BUYER_TG = DIRECT_INTERCONNECT_TG(TG,K)
                  BUYER_CM = TG_2_CAPACITY_MARKET(BUYER_TG)
                  BUYER_PA = TG_2_PLANNING_AREA(BUYER_TG)
                  IF(CM == BUYER_CM .OR. CM == 0) CYCLE
!
                  IF(TransmissionConstraint(TG,BUYER_TG) < 0.01) CYCLE
!
                  QUANTITY_OF_TRANSFER = MIN( &
                                TransmissionConstraint(TG,BUYER_TG), &
                                   CAPACITY_PLANNING_MW(HIGHEST_OPTION))
!
                  CM_CAPACITY_MW(BUYER_CM) = &
                           CM_PLANNING_CAPACITY(BUYER_CM) + &
                                CM_TOT_ANN_CAPACITY_ADDED(BUYER_CM) + &
                                    CM_Net_Transfer(BUYER_CM) - &
                                       QUANTITY_OF_TRANSFER
                  IF(CM_PEAK_MW(BUYER_CM) > .01) THEN
                     CM_RESERVE_MARGIN(BUYER_CM) = 100. * &
                         (CM_CAPACITY_MW(BUYER_CM)/ &
                                            CM_PEAK_MW(BUYER_CM) - 1.0)
                     CM_INTERUPTIBLRESERVE_MARGIN = 100. * &
                         (CM_CAPACITY_MW(BUYER_CM) - &
                              CM_PEAK_AFTER_INTERRUPTIBLE(BUYER_CM)) / &
                                CM_PEAK_AFTER_INTERRUPTIBLE(BUYER_CM)
                  ELSE
                     CM_RESERVE_MARGIN(BUYER_CM) = 100.
                  ENDIF
                  TEMP_R4 = CM_RESERVE_MARGIN(BUYER_CM)*0.01 + 1.0
                  TEMP_R4 = CM_INTERUPTIBLRESERVE_MARGIN*0.01 + 1.0
!
                  MARGINAL_ICAP_PER_KWYR_BY_TG(BUYER_TG) = &
                     MIN(MARGINAL_ICAP_PER_KWYR_BY_TG(BUYER_TG), &
                       GET_ICAP_REVENUE_MULT(R_YEAR,BUYER_CM,TEMP_R4)* &
                                     LOWEST_FIXED_COST_BY_CM(BUYER_CM))
!
                  IF(PRICE_OF_TRANSFER >= &
                           MARGINAL_ICAP_PER_KWYR_BY_TG(BUYER_TG))CYCLE
!
                  PRICE_OF_TRANSFER = &
                                 MARGINAL_ICAP_PER_KWYR_BY_TG(BUYER_TG)
                  BEST_BUYER_TG = BUYER_TG
                  BEST_BUYER_CM = BUYER_CM
                  BEST_BUYER_PA = BUYER_PA
               END DO
!
               IF(1 == 2 .AND. PRICE_OF_TRANSFER > &
                       ICAP_REVENUE_PER_KWYR(NEXT_CN) .AND. &
                                       QUANTITY_OF_TRANSFER > 0.10) THEN
                  PRICE_CEILING = PRICE_OF_TRANSFER
               ELSE
                  PRICE_CEILING = &
                                 CONE_DEMAND_CURVE_ADJ_PER_KWYR(NEXT_CN)
                  QUANTITY_OF_TRANSFER = 0.0
               ENDIF
!
               PRICE_CEILING = &
                              CONE_DEMAND_CURVE_ADJ_PER_KWYR(NEXT_CN)
               IF(ICAP_SWITCH_INDEX(CM) == 2 .OR.   & !  MARGINAL
                                        ICAP_SWITCH_INDEX(CM) == 3) THEN
                                        ! CONE-MRX
                  IF(BASE_YEAR + R_YEAR == 2024) THEN
                     TEMP_I2 = TEMP_I2
                  ENDIF
                  CAPACITY_REVENUE_MM(NEXT_CN) = &
                           MIN(PRICE_CEILING * &
                           CAPACITY_PLANNING_MW(HIGHEST_OPTION)*0.001, &
                                MAX(0., &
                                 FIXED_COST_MM(NEXT_CN) + &
                                    VARIABLE_COST_MM(NEXT_CN) - &
                                       RPS_REVENUE_MM(NEXT_CN) - &
                                         ENERGY_REVENUE_MM(NEXT_CN)))
                  ICAP_REVENUE_PER_KWYR(NEXT_CN) = &
                          MAX(0.0,CAPACITY_REVENUE_MM(NEXT_CN)*1000.0/ &
                                       SCREEN_CAPACITY(HIGHEST_OPTION))
               ELSE
                  ICAP_REVENUE_PER_KWYR(NEXT_CN) = &
                                 MIN(ICAP_CONE_PRICE(CM),PRICE_CEILING)
                  CAPACITY_REVENUE_MM(NEXT_CN) = &
                       ICAP_REVENUE_PER_KWYR(NEXT_CN) * &
                            CAPACITY_PLANNING_MW(HIGHEST_OPTION)*0.001
               ENDIF
!
               EAS_PROFIT_PER_KWYR(NEXT_CN) = 0.001 * &
                        1000000. * (ENERGY_REVENUE_MM(NEXT_CN) - &
                                           VARIABLE_COST_MM(NEXT_CN))/ &
                                         SCREEN_CAPACITY(HIGHEST_OPTION)
               EAS_PROFIT_PER_KWYR(NEXT_CN) = MAX(0.0, &
                             CONE_DEMAND_CURVE_ADJ_PER_KWYR(NEXT_CN) - &
                                         ICAP_REVENUE_PER_KWYR(NEXT_CN))
               MARGINAL_ICAP_PER_KWYR_BY_TG(TG) = &
                                          ICAP_REVENUE_PER_KWYR(NEXT_CN)
            ELSE
               EAS_PROFIT_PER_KWYR(NEXT_CN) = 0.0
               CONE_COST_PER_KWYR(NEXT_CN) = 0.0
               CONE_DEMAND_CURVE_ADJ_PER_KWYR(NEXT_CN) = 0.0
               ICAP_REVENUE_PER_KWYR(NEXT_CN) = 0.0
               CAPACITY_REVENUE_MM(NEXT_CN) = 0.
               RPS_REVENUE_MM(NEXT_CN)= 0.0
            ENDIF
            HIGHEST_MARKET_REVENUE(NEXT_CN) = &
                                ENERGY_REVENUE_MM(NEXT_CN) + &
                                  RPS_REVENUE_MM(NEXT_CN) + &
                                     CAPACITY_REVENUE_MM(NEXT_CN)
            NET_MARGIN_PER_KWYR(HIGHEST_OPTION) = 1000.0 * &
                              (HIGHEST_MARKET_REVENUE(NEXT_CN) - &
                                 VARIABLE_COST_MM(NEXT_CN) - &
                                       FIXED_COST_MM(NEXT_CN))/ &
                                 SCREEN_CAPACITY(HIGHEST_OPTION)
            HGST_NET_MARG_ADDED(NEXT_CN) = &
                                     NET_MARGIN_PER_KWYR(HIGHEST_OPTION)

            IF(PA_PEAK_AFTER_INTERRUPTIBLE(PA) > NEAR_ZERO)  THEN
               PA_PLANNING_RESERVE_MARGIN(PA) = &
                        1. + (PA_PLANNING_CAPACITY(PA) &
                                    + PA_Net_Transfer(PA) - &
                                 PA_PEAK_AFTER_INTERRUPTIBLE(PA)) / &
                                      PA_PEAK_AFTER_INTERRUPTIBLE(PA)
               MINIMUM_ANNUAL_MRX_CAP(PA) = MAX(0., &
                        (CURRENT_TARGET_RATIO_PA(PA) - &
                           PA_PLANNING_RESERVE_MARGIN(PA))* &
                                        PA_PEAK_AFTER_INTERRUPTIBLE(PA))
!
            ELSE
               PA_PLANNING_RESERVE_MARGIN(PA) = ZERO
            ENDIF
            IF(PEAK_AFTER_INTERRUPTIBLE_TG(TG) > NEAR_ZERO)  THEN
               TG_PLANNING_RESERVE_MARGIN(TG) = &
                        1. + (TG_PLANNING_CAPACITY(TG) &
                                    + TG_Net_Transfer(TG) - &
                                  PEAK_AFTER_INTERRUPTIBLE_TG(TG)) / &
                                         PEAK_AFTER_INTERRUPTIBLE_TG(TG)
               MINIMUM_TG_MRX_CAP(TG) = &
                           MAX(0., &
                              (GET_MIN_CAP_TESTING_RATIO(TG)* &
                                             SCENARIO_RESERVE_MARGIN - &
                                  TG_PLANNING_RESERVE_MARGIN(TG))* &
                                        PEAK_AFTER_INTERRUPTIBLE_TG(TG))
               TG_PEAK_MW(TG) = PEAK_AFTER_INTERRUPTIBLE_TG(TG)
            ELSE
               MINIMUM_TG_MRX_CAP(TG) = ZERO
            ENDIF
!
            IF(USE_REGIONAL_MINIMUM_RM_LOGIC) THEN
               IF(TOTAL_ANNUAL_CAPACITY_ADDED(0) + &
                      CAPACITY_PLANNING_MW(HIGHEST_OPTION) > &
                                       MAXIMUM_ANNUAL_MRX_CAP(0) .OR. &
                     TOTAL_ANNUAL_CAPACITY_ADDED(PA) + &
                           CAPACITY_PLANNING_MW(HIGHEST_OPTION) > &
                                        MAXIMUM_ANNUAL_MRX_CAP(PA)) THEN
!
                  if(TOTAL_ANNUAL_CAPACITY_ADDED(0) + &
                      CAPACITY_PLANNING_MW(HIGHEST_OPTION) > &
                                       MAXIMUM_ANNUAL_MRX_CAP(0)) then
                     MRX_ICAP_INDEX(NEXT_CN) = 4
                  else
                     MRX_ICAP_INDEX(NEXT_CN) = 5
                  endif
                  TOTAL_REGIONAL_CAPACITY_NEEDED = 0.
                  DO PA = 1, PG
                     TOTAL_REGIONAL_CAPACITY_NEEDED = &
                          TOTAL_REGIONAL_CAPACITY_NEEDED + &
                              MAX(0.,MINIMUM_ANNUAL_MRX_CAP(PA) - &
                                        TOTAL_ANNUAL_CAPACITY_ADDED(PA))
                  ENDDO
!
                  IF(TOTAL_REGIONAL_CAPACITY_NEEDED > 0.) THEN
                     CYCLE
                  ELSE
                     EXIT
                  ENDIF
               ENDIF
            ELSE
               IF(TOTAL_ANNUAL_CAPACITY_ADDED(0) + &
                      CAPACITY_PLANNING_MW(HIGHEST_OPTION) > &
                                  MAX(MAXIMUM_ANNUAL_MRX_CAP(0), &
                                       MAXIMUM_ANNUAL_MRX_CAP(PA))) THEN
                  MRX_ICAP_INDEX(NEXT_CN) = 4
                  EXIT
               ENDIF
            ENDIF
!
            IF(TOTAL_ANNUAL_CAPACITY_ADDED(PA) + &
                      CAPACITY_PLANNING_MW(HIGHEST_OPTION) > &
                                       MAXIMUM_ANNUAL_MRX_CAP(PA)) THEN
               MRX_ICAP_INDEX(NEXT_CN) = 6
               CYCLE
            ENDIF
!
! 090709. ADDITIONAL MAXIMUM TG CONSTRAINT.
!
            IF(TG_TOTAL_ANNUAL_CAPACITY_ADDED(TG) + &
                      CAPACITY_PLANNING_MW(HIGHEST_OPTION) > &
                                       MAXIMUM_TG_MRX_CAP(TG)) THEN
               MRX_ICAP_INDEX(NEXT_CN) = 7
               CYCLE
            ENDIF
!
            IF(USE_MINIMUM_RM_LOGIC) THEN
               IF(TOTAL_ANNUAL_CAPACITY_ADDED(0)  >= &
                                         MINIMUM_ANNUAL_MRX_CAP(0)) THEN
                  IF(HGST_NET_MARG_ADDED(NEXT_CN)+ 0.01<= &
                                           MINIMUM_NET_MARGIN .OR. &
                                            RPS_ONLY_SWITCH_ACTIVE) THEN
                     MRX_ICAP_INDEX(NEXT_CN) = 4
                     EXIT
                  ENDIF
               ELSE ! HAVE NOT ADDED ENOUGH.
               ENDIF
            ELSEIF(USE_REGIONAL_MINIMUM_RM_LOGIC) THEN
!
! I'VE ALREADY ADDED ENOUGH
!
               IF(TOTAL_ANNUAL_CAPACITY_ADDED(PA)  >= &
                  MINIMUM_ANNUAL_MRX_CAP(PA) .AND. &
                     TG_TOTAL_ANNUAL_CAPACITY_ADDED(TG)  >= & ! 090709.
                      MINIMUM_TG_MRX_CAP(TG)) THEN
!
                  IF(HGST_NET_MARG_ADDED(NEXT_CN)+ 0.01<= &
                                           MINIMUM_NET_MARGIN .OR. &
                                            RPS_ONLY_SWITCH_ACTIVE) THEN
!
! MUST CYCLE THROUGH BECAUSE OF MULTIPLE PLANNING AREAS
!
                     MRX_ICAP_INDEX(NEXT_CN) = 8
                     CYCLE
                  ENDIF
               ELSE ! HAVE NOT ADDED ENOUGH.
!
! I NEED MORE TO MEET MINIMUM RESERVE. LET THIS ALWAYS FALL THROUGH TO 
! BUILD THE UNIT.
               ENDIF
!
            ELSEIF(HGST_NET_MARG_ADDED(NEXT_CN)+0.01 <= &
                                           MINIMUM_NET_MARGIN .OR. &
                                            RPS_ONLY_SWITCH_ACTIVE) THEN
!
               MRX_ICAP_INDEX(NEXT_CN) = 8
               EXIT
!
            ENDIF
!
! PASSED ALL TESTS.  ADD THE UNIT
!
            IF(PA > 0) THEN
               TOTAL_ANNUAL_CAPACITY_ADDED(PA) = &
                           TOTAL_ANNUAL_CAPACITY_ADDED(PA) + &
                                    CAPACITY_PLANNING_MW(HIGHEST_OPTION)
!
               TG_TOTAL_ANNUAL_CAPACITY_ADDED(TG) = &
                           TG_TOTAL_ANNUAL_CAPACITY_ADDED(TG) + &
                                    CAPACITY_PLANNING_MW(HIGHEST_OPTION)
            ENDIF
            IF(CM > 0) THEN
               CM_TOT_ANN_CAPACITY_ADDED(CM) = &
                           CM_TOT_ANN_CAPACITY_ADDED(CM) + &
                                    CAPACITY_PLANNING_MW(HIGHEST_OPTION)
            ENDIF
!
            TOTAL_ANNUAL_CAPACITY_ADDED(0) = &
                           TOTAL_ANNUAL_CAPACITY_ADDED(0) + &
                                    CAPACITY_PLANNING_MW(HIGHEST_OPTION)
!
            LAST_SORTED_POSITION = HIGHEST_OPTION
            LAST_RESOURCE_ADDED(TG) = HIGHEST_OPTION
!
            LAST_PA_RESOURCE_ADDED(PA) = NEXT_CN
            LAST_CM_RESOURCE_ADDED(CM) = NEXT_CN
            INSTALL_CAP_VALUE_LAST_PA_UNIT(PA) = &
                                HIGHEST_ANN_LEVEL_CAP(HIGHEST_OPTION)
            INSTALL_CAP_VALUE_LAST_CM_UNIT(CM) = &
                                HIGHEST_ANN_LEVEL_CAP(HIGHEST_OPTION)
!
            FIRST_YEAR_CAPACITY = &
                   ADD_THIS_UNIT( &
                           OPTION_POSITION(HIGHEST_OPTION),THIS_YEAR + &
                                        LOCAL_LEAD_TIME(HIGHEST_OPTION))
!
            IF(INDEPENDENT(HIGHEST_OPTION) > 0) THEN
               HYBRID_PROD_POINTER = INDEPENDENT(HIGHEST_OPTION)
               FIRST_YEAR_CAPACITY = &
                   ADD_THIS_UNIT( &
                      OPTION_POSITION(HYBRID_PROD_POINTER),THIS_YEAR + &
                                   LOCAL_LEAD_TIME(HYBRID_PROD_POINTER))
            ENDIF
!
            IF(MX_SUMMARY_REPORT) THEN
!
               TEMP_I2 = GET_REGIONAL_CM_NAME(CM,CM_NAME)
               IF(INDEPENDENT(HIGHEST_OPTION) > 0) THEN
                  WRITE(LOCAL_NAME,1017) &
                           OPTION_NAME(HIGHEST_OPTION)(1:15), &
                                                         ' H',HO - I + 1
               ELSE
                  WRITE(LOCAL_NAME,1010) &
                           OPTION_NAME(HIGHEST_OPTION)(1:17),HO - I + 1
               ENDIF
!
               MARKET_AVE_REVENUE = 0.
               TOTAL_AVERAGE_COST = 0.
!
! 032811. TOOK OUT SCREEN CAPACITY PER NAN
!
               IF(TG_MW_TRANSFERS) THEN
                  TG_CAPACITY_MW(TG) = &
                           TG_PLANNING_CAPACITY(TG) + &
                              TG_TOTAL_ANNUAL_CAPACITY_ADDED(TG) + & ! -
                                          TG_Net_Transfer(TG)
               ELSE
                  TG_CAPACITY_MW(TG) = &
                           TG_PLANNING_CAPACITY(TG) + &
                                 TG_TOTAL_ANNUAL_CAPACITY_ADDED(TG) ! -
               ENDIF
!
               IF(TG_PEAK_MW(TG) > .01) THEN
                  TG_RESERVE_MARGIN(TG) = 100. * &
                              (TG_CAPACITY_MW(TG)/TG_PEAK_MW(TG) - 1.0)
               ELSE
                  TG_RESERVE_MARGIN(TG) = 100.
               ENDIF
!
               PA_CAPACITY_MW(PA) = &
                           PA_PLANNING_CAPACITY(PA) + &
                                 TOTAL_ANNUAL_CAPACITY_ADDED(PA)
               IF(PA_PEAK_MW(PA) > .01) THEN
                  PA_RESERVE_MARGIN(PA) = 100. * &
                              (PA_CAPACITY_MW(PA)/PA_PEAK_MW(PA) - 1.0)
               ELSE
                  PA_RESERVE_MARGIN(PA) = 100.
               ENDIF
               TEMP_R4 = PA_RESERVE_MARGIN(PA)*0.01 + 1.0
!
               IF(TG_MW_TRANSFERS) THEN
                  CM_CAPACITY_MW(CM) = &
                           CM_PLANNING_CAPACITY(CM) + &
                                 CM_TOT_ANN_CAPACITY_ADDED(CM) + &
                                          CM_Net_Transfer(CM) + &
                                              QUANTITY_OF_TRANSFER
               ELSE
                  CM_CAPACITY_MW(CM) = &
                           CM_PLANNING_CAPACITY(CM) + &
                                 CM_TOT_ANN_CAPACITY_ADDED(CM)
               ENDIF
               IF(CM_PEAK_MW(CM) > .01) THEN
                  CM_RESERVE_MARGIN(CM) = 100. * &
                              (CM_CAPACITY_MW(CM)/CM_PEAK_MW(CM) - 1.0)
                  CM_INTERUPTIBLRESERVE_MARGIN = 100. * &
                         (CM_CAPACITY_MW(CM) - &
                                    CM_PEAK_AFTER_INTERRUPTIBLE(CM)) / &
                                CM_PEAK_AFTER_INTERRUPTIBLE(CM)
               ELSE
                  CM_RESERVE_MARGIN(CM) = 100.
               ENDIF
!
               TEMP_R4 = CM_RESERVE_MARGIN(CM)*0.01 + 1.0
               TEMP_R4 = CM_INTERUPTIBLRESERVE_MARGIN*0.01 + 1.0
               ICAP_DEMAND_CURVE_MULT(NEXT_CN) = &
                                GET_ICAP_REVENUE_MULT(R_YEAR,CM,TEMP_R4)
               IF(SCREEN_CAPACITY(HIGHEST_OPTION) > 0.01) THEN
                  EAS_PROFIT_PER_KWYR(NEXT_CN) = 0.001 * &
                        1000000. * (ENERGY_REVENUE_MM(NEXT_CN) - &
                                           VARIABLE_COST_MM(NEXT_CN))/ &
                                       SCREEN_CAPACITY(HIGHEST_OPTION)
!
                  IF(ICAP_SWITCH_INDEX(CM) == 2) THEN
                     CONE_COST_PER_KWYR(NEXT_CN) = &
                                             LOWEST_FIXED_COST_BY_CM(CM)
                  ELSE
                     CONE_COST_PER_KWYR(NEXT_CN) = ICAP_CONE_PRICE(CM)
                  ENDIF
!
                  BASE_CAP_REV_MM = -0.001 * &
                                CAPACITY_PLANNING_MW(HIGHEST_OPTION) * &
                                       CM_MAX_NET_ENRG_MARG_PER_KWYR(CM)
                  MAX_CAP_REV_MM = 0.001 * &
                                CAPACITY_PLANNING_MW(HIGHEST_OPTION) * &
                                 CONE_DEMAND_CURVE_ADJ_PER_KWYR(NEXT_CN)
                  MAX_RPS_REV_MM = RPS_REVENUE_MM(NEXT_CN)
                  ENERGY_UP_LIFT_MM = &
                          MIN(MAX_CAP_REV_MM, &
                               MAX(0.0,(FIXED_COST_MM(NEXT_CN) + &
                                    VARIABLE_COST_MM(NEXT_CN) - &
                                          ENERGY_REVENUE_MM(NEXT_CN))))
                  MAX_RPS_REV_MM = &
                    MAX(0.0, &
                       MIN(ENERGY_UP_LIFT_MM - BASE_CAP_REV_MM, &
                                                       MAX_RPS_REV_MM))
                  TEMP_R4 = &
                     MAX(0.0, &
                        MIN(ENERGY_UP_LIFT_MM - MAX_RPS_REV_MM, &
                                                        MAX_CAP_REV_MM))
                  TEMP_R4 = TEMP_R4 * 1000.0 / &
                            CAPACITY_PLANNING_MW(HIGHEST_OPTION)
!
                  IF(ICAP_SWITCH_INDEX(CM) == 2 .OR.   & !  MARGINAL
                                        ICAP_SWITCH_INDEX(CM) == 3) THEN
                                        ! CONE-MRX
                     ICAP_REVENUE_PER_KWYR(NEXT_CN) = &
                        MAX(0.0,MIN(PRICE_CEILING,TEMP_R4))
                  ELSE
                     ICAP_REVENUE_PER_KWYR(NEXT_CN) = &
                                                     ICAP_CONE_PRICE(CM)
                  ENDIF
               ELSE
                  EAS_PROFIT_PER_KWYR(NEXT_CN) = 0.0
                  CONE_COST_PER_KWYR(NEXT_CN) = 0.0
                  CONE_DEMAND_CURVE_ADJ_PER_KWYR(NEXT_CN) = &
                                                                  0.0
                  ICAP_REVENUE_PER_KWYR(NEXT_CN) = 0.0
               ENDIF
!
               CAPACITY_REVENUE_MM(NEXT_CN) = 0.001 * &
                                 ICAP_REVENUE_PER_KWYR(NEXT_CN) * &
                            CAPACITY_PLANNING_MW(HIGHEST_OPTION)
!
               RPS_REVENUE_MM(NEXT_CN) = &
                           MIN(RPS_REVENUE_MM(NEXT_CN), &
                                MAX(0., &
                                 FIXED_COST_MM(NEXT_CN) + &
                                    VARIABLE_COST_MM(NEXT_CN) - &
                                       CAPACITY_REVENUE_MM(NEXT_CN) - &
                                          ENERGY_REVENUE_MM(NEXT_CN)))
               TEMP_R = HIGHEST_MRX_ENERGY(NEXT_CN) * &
                                       RPS_PERCENT(HIGHEST_OPTION)
               IF(TEMP_R > 0.0) THEN
                  RPS_REVENUE_PER_MWH(NEXT_CN) = &
                              1000.0 * RPS_REVENUE_MM(NEXT_CN) / TEMP_R
               ELSE
                  RPS_REVENUE_PER_MWH(M) = 0.0
               ENDIF
               HIGHEST_MARKET_REVENUE(NEXT_CN) = &
                                ENERGY_REVENUE_MM(NEXT_CN) + &
                                  RPS_REVENUE_MM(NEXT_CN) + &
                                     CAPACITY_REVENUE_MM(NEXT_CN)
               IF(HIGHEST_MRX_ENERGY(NEXT_CN) > 0.01) THEN
                  CAPACITY_REVENUE_PER_MWH(NEXT_CN) = &
                            CAPACITY_REVENUE_MM(NEXT_CN) * 1000.0/ &
                                            HIGHEST_MRX_ENERGY(NEXT_CN)
                  REVENUE_PER_MWH(NEXT_CN) = &
                           HIGHEST_ENERGY_REV_PER_MWH(NEXT_CN) + &
                              RPS_REVENUE_PER_MWH(NEXT_CN) + &
                                CAPACITY_REVENUE_PER_MWH(NEXT_CN)
                  FIXED_COST_PER_MWH(NEXT_CN) = &
                        HIGHEST_FOM_PER_MWH(NEXT_CN) + &
                              HIGHEST_LEVEL_CAP_PER_MWH(NEXT_CN)
                  HIGHEST_MRX_MARGIN(NEXT_CN) = &
                              REVENUE_PER_MWH(NEXT_CN) - &
                                 HIGHEST_STRIKE_PRICE(NEXT_CN) - &
                                    FIXED_COST_PER_MWH(NEXT_CN)
               ELSE
                  CAPACITY_REVENUE_PER_MWH(NEXT_CN) = 0.0
                  REVENUE_PER_MWH(NEXT_CN) = 0.0
                  FIXED_COST_PER_MWH(NEXT_CN) = 0.0
                  HIGHEST_MRX_MARGIN(NEXT_CN) = 0.0
               ENDIF
!
               IF(SCREEN_CAPACITY(HIGHEST_OPTION) > 0.01) THEN
                  HIGHEST_MARGIN_PER_KWYR(NEXT_CN) = 1000.0 * &
                              (HIGHEST_MARKET_REVENUE(NEXT_CN) - &
                                 VARIABLE_COST_MM(NEXT_CN) - &
                                       FIXED_COST_MM(NEXT_CN))/ &
                              SCREEN_CAPACITY(HIGHEST_OPTION)
               ELSE
                  HIGHEST_MARGIN_PER_KWYR(NEXT_CN) = -999999.0
               ENDIF

               IF(RPS_REVENUE_MM(NEXT_CN) > 0.0) THEN ! .AND.
                  MRX_ICAP_INDEX(NEXT_CN) = 10
                  TEMP_I2 = RPS_PROGRAM_NUMBER(HIGHEST_OPTION)
                  RPS_COUNT = 1
                  RPS_NO = GET_RPS_PROGRAM_POSITION(TEMP_I2,RPS_COUNT)
                  INT2_PM = INT(HIGHEST_RPM(NEXT_CN),2)
                  IF(ABS(TEMP_I2) > 0) THEN
                     TEMP_R = PUT_RPS_PRICE_BY_OPTION( &
                                 TEMP_I2, &
                                 RPS_REVENUE_PER_MWH(NEXT_CN), &
                                 1000.0*HIGHEST_MRX_ENERGY(NEXT_CN), &
                                 SCREEN_CAPACITY(HIGHEST_OPTION), &
                                 INT2_PM, &
                                 R_YEAR, &
                                 GRX_RPS_MODULE_ACTIVE, &
                                 RPS_GEN_PRICE_BY(RPS_NO))
                  ENDIF
               ELSE
                  MRX_ICAP_INDEX(NEXT_CN) = 1
               ENDIF
!
               MX_ANNUAL_ALT_REC = RPTREC(MX_ANNUAL_ALT_NO)
               WRITE(MX_ANNUAL_ALT_NO,REC=MX_ANNUAL_ALT_REC) &
                        PRT_ENDPOINT(), &
                        FLOAT(THIS_YEAR), &
                        CM_NAME, &
                        LOCAL_NAME, &
                        HIGHEST_STRIKE_PRICE(NEXT_CN), &
                        HIGHEST_ENERGY_REV_PER_MWH(NEXT_CN), & 
                        ! 110309. REDEFINED.
                        HIGHEST_MRX_MARGIN(NEXT_CN), &
                        HIGHEST_MRX_CF(NEXT_CN), &
                        ENERGY_REVENUE_MM(NEXT_CN), &
                        HIGHEST_MARKET_COST(NEXT_CN), & !  6
                        VARIABLE_COST_MM(NEXT_CN), &
                        FIXED_COST_MM(NEXT_CN), &
                        SCREEN_CAPACITY(HIGHEST_OPTION), &
                        HIGHEST_MRX_ENERGY(NEXT_CN), & !  10
                        TOTAL_AVERAGE_COST, & !  NEED DEFINITION
                        HIGHEST_ANN_LEVEL_CAP(NEXT_CN), &
                        HIGHEST_FUEL_COST(NEXT_CN), &
                        HIGHEST_ANNUAL_UNITS(NEXT_CN), &
                        CAPACITY_REVENUE_MM(NEXT_CN), &
                        HIGHEST_MARKET_REVENUE(NEXT_CN), & 
                        ! 16 TOTAL REVENUE
                        HIGHEST_MARGIN_PER_KWYR(NEXT_CN), &
                        HIGHEST_FUEL_COST_PER_MWH(NEXT_CN), &
                        HIGHEST_VAR_OM_PER_MWH(NEXT_CN), & !  19
                        HIGHEST_FUEL_COST_PER_MMBTU(NEXT_CN), &
                        REVENUE_PER_MWH(NEXT_CN), &
                        CAPACITY_REVENUE_PER_MWH(NEXT_CN), &
                        HIGHEST_EMISSIONS_PER_MWH(NEXT_CN), &
                        HIGHEST_FOM_PER_MWH(NEXT_CN), & !  24
                        HIGHEST_LEVEL_CAP_PER_MWH(NEXT_CN), &
                        HIGHEST_VOM_COST_MM(NEXT_CN), &
                        HIGHEST_EMISSIONS_COST_MM(NEXT_CN), & !  27
                        HIGHEST_FOM_COST_MM(NEXT_CN), &
                        FLOAT(TRANSACTION_GROUP(HIGHEST_OPTION)), &
                        TG_CAPACITY_MW(TG), &
                        TG_PEAK_MW(TG), &
                        TG_RESERVE_MARGIN(TG), &
                        FLOAT(CM_INDEX), & !  PLANNING_AREA_ID(NEXT_CN),
                        CM_CAPACITY_MW(CM), &
                        CM_PEAK_MW(CM), &
                        CM_RESERVE_MARGIN(CM), &
                        ICAP_DEMAND_CURVE_MULT(NEXT_CN), &
                        CONE_COST_PER_KWYR(NEXT_CN), & !  38
                        CONE_DEMAND_CURVE_ADJ_PER_KWYR(NEXT_CN), &
                        EAS_PROFIT_PER_KWYR(NEXT_CN), &
                        ICAP_REVENUE_PER_KWYR(NEXT_CN), &
                        HIGHEST_HEAT_RATE(NEXT_CN), & !  42
                        FIXED_COST_PER_MWH(NEXT_CN), &
                        HIGHEST_RFT(NEXT_CN), &
                        HIGHEST_RPM(NEXT_CN), &
                        TG_Net_Transfer(TG), &
                        CM_Net_Transfer(CM), &
                        MRX_ICAP_INDEX(NEXT_CN), &
                        CM_INTERUPTIBLRESERVE_MARGIN, &
                        CM_PEAK_AFTER_INTERRUPTIBLE(CM), &
                        CAPACITY_PLANNING_MW(HIGHEST_OPTION), &
                        RPS_REVENUE_MM(NEXT_CN), &
                        RPS_REVENUE_PER_MWH(NEXT_CN)
               MX_ANNUAL_ALT_REC = MX_ANNUAL_ALT_REC + 1
            ENDIF
!
            IF(PRICE_OF_TRANSFER > &
                       ICAP_REVENUE_PER_KWYR(NEXT_CN) .AND. &
                                       QUANTITY_OF_TRANSFER > 0.10) THEN
!
! RECORD TRANSACTION AND TRY TO RESET THE LOOP
!
               TransmissionConstraint(TG,BEST_BUYER_TG) = &
                        TransmissionConstraint(TG,BEST_BUYER_TG) - &
                                                    QUANTITY_OF_TRANSFER
               CM_Net_Transfer(CM) = CM_Net_Transfer(CM) + &
                                                    QUANTITY_OF_TRANSFER
               CM_Net_Transfer(BEST_BUYER_CM) = &
                                 CM_Net_Transfer(BEST_BUYER_CM) - &
                                                    QUANTITY_OF_TRANSFER
               PA_Net_Transfer(PA) = PA_Net_Transfer(PA) + &
                                                    QUANTITY_OF_TRANSFER
               PA_Net_Transfer(BEST_BUYER_PA) = &
                                 PA_Net_Transfer(BEST_BUYER_PA) - &
                                                    QUANTITY_OF_TRANSFER
               TG_Net_Transfer(TG) = TG_Net_Transfer(TG) + &
                                                    QUANTITY_OF_TRANSFER
               TG_Net_Transfer(BEST_BUYER_TG) = &
                                 TG_Net_Transfer(BEST_BUYER_TG) - &
                                                    QUANTITY_OF_TRANSFER
            ELSE
            ENDIF
!
            DO L = I-1, 1, -1
               NEXT_CN = SORTED_OPTIONS(L)
               HIGHEST_OPTION = HGST_OPT_INDEX(NEXT_CN)
               NEXT_CN = I-1 - L + 1
!
! 042717. ADDED THIS LINE TO GET THE RIGHT VALUE FOR CAPACITY
!
               HIGHEST_OPTION = HGST_OPT_INDEX(NEXT_CN)
!
               IF(.NOT. DEPENDENT_UNIT_AVAILABLE(HIGHEST_OPTION)) CYCLE
               BEST_SELLER_TG = GET_TRANS_GROUP_POSITION( &
                                      TRANSACTION_GROUP(HIGHEST_OPTION))
               IF(BEST_SELLER_TG /= TG) CYCLE
               IF(TG_MW_TRANSFERS) THEN
                  TG_CAPACITY_MW(TG) = &
                           TG_PLANNING_CAPACITY(TG) + &
                                 TG_TOTAL_ANNUAL_CAPACITY_ADDED(TG) - &
                                CAPACITY_PLANNING_MW(HIGHEST_OPTION) + &
                                          TG_Net_Transfer(TG)
               ELSE
                  TG_CAPACITY_MW(TG) = &
                           TG_PLANNING_CAPACITY(TG) + &
                                 TG_TOTAL_ANNUAL_CAPACITY_ADDED(TG) - &
                                    CAPACITY_PLANNING_MW(HIGHEST_OPTION)
               ENDIF
               IF(TG_PEAK_MW(TG) > .01) THEN
                  TG_RESERVE_MARGIN(TG) = 100. * &
                              (TG_CAPACITY_MW(TG)/TG_PEAK_MW(TG) - 1.0)
               ELSE
                  TG_RESERVE_MARGIN(TG) = 100.
               ENDIF
               PA_CAPACITY_MW(PA) = &
                           PA_PLANNING_CAPACITY(PA) + &
                                 TOTAL_ANNUAL_CAPACITY_ADDED(PA)
               IF(PA_PEAK_MW(PA) > .01) THEN
                  PA_RESERVE_MARGIN(PA) = 100. * &
                              (PA_CAPACITY_MW(PA)/PA_PEAK_MW(PA) - 1.0)
               ELSE
                  PA_RESERVE_MARGIN(PA) = 100.
               ENDIF
               TEMP_R4 = PA_RESERVE_MARGIN(PA)*0.01 + 1.0
               IF(TG_MW_TRANSFERS) THEN
                  CM_CAPACITY_MW(CM) = &
                           CM_PLANNING_CAPACITY(CM) + &
                                 CM_TOT_ANN_CAPACITY_ADDED(CM) - &
                                CAPACITY_PLANNING_MW(HIGHEST_OPTION) + &
                                          CM_Net_Transfer(CM)
               ELSE
                  CM_CAPACITY_MW(CM) = &
                           CM_PLANNING_CAPACITY(CM) + &
                                 CM_TOT_ANN_CAPACITY_ADDED(CM) - &
                                    CAPACITY_PLANNING_MW(HIGHEST_OPTION)
               ENDIF
               IF(CM_PEAK_MW(CM) > .01) THEN
                  CM_RESERVE_MARGIN(CM) = 100. * &
                              (CM_CAPACITY_MW(CM)/CM_PEAK_MW(CM) - 1.0)
               ELSE
                  CM_RESERVE_MARGIN(CM) = 100.
               ENDIF
!
               TEMP_R4 = CM_RESERVE_MARGIN(CM)*0.01 + 1.0
               ICAP_DEMAND_CURVE_MULT(NEXT_CN) = &
                                GET_ICAP_REVENUE_MULT(R_YEAR,CM,TEMP_R4)
               IF(SCREEN_CAPACITY(HIGHEST_OPTION) > 0.01) THEN
                  EAS_PROFIT_PER_KWYR(NEXT_CN) = 0.001 * &
                        1000000. * (ENERGY_REVENUE_MM(NEXT_CN) - &
                                           VARIABLE_COST_MM(NEXT_CN))/ &
                                       SCREEN_CAPACITY(HIGHEST_OPTION)
                  IF(ICAP_SWITCH_INDEX(CM) == 2) THEN
                     CONE_COST_PER_KWYR(NEXT_CN) = &
                                             LOWEST_FIXED_COST_BY_CM(CM)
                  ELSE
                     CONE_COST_PER_KWYR(NEXT_CN) = ICAP_CONE_PRICE(CM)
                  ENDIF
                  CONE_DEMAND_CURVE_ADJ_PER_KWYR(NEXT_CN) = &
                                    CONE_COST_PER_KWYR(NEXT_CN) * &
                                  ICAP_DEMAND_CURVE_MULT(NEXT_CN)
                  TEMP_R4 = 1000.0 * &
                               (FIXED_COST_MM(NEXT_CN) + &
                                    VARIABLE_COST_MM(NEXT_CN) - &
                                       RPS_REVENUE_MM(NEXT_CN) - &
                                          ENERGY_REVENUE_MM(NEXT_CN))/ &
                                     SCREEN_CAPACITY(HIGHEST_OPTION)
                  IF(ICAP_SWITCH_INDEX(CM) == 2 .OR.   & !  MARGINAL
                                        ICAP_SWITCH_INDEX(CM) == 3) THEN
                                        ! CONE-MRX
                     ICAP_REVENUE_PER_KWYR(NEXT_CN) = &
                        MAX(0.0, &
                        MIN(CONE_DEMAND_CURVE_ADJ_PER_KWYR(NEXT_CN), &
                                                           TEMP_R4))
                  ELSE
                     ICAP_REVENUE_PER_KWYR(NEXT_CN) = &
                                                     ICAP_CONE_PRICE(CM)
                  ENDIF
               ELSE
                  EAS_PROFIT_PER_KWYR(NEXT_CN) = 0.0
                  CONE_COST_PER_KWYR(NEXT_CN) = 0.0
                  CONE_DEMAND_CURVE_ADJ_PER_KWYR(NEXT_CN) = &
                                                                  0.0
                  ICAP_REVENUE_PER_KWYR(NEXT_CN) = 0.0
               ENDIF
               MARGINAL_ICAP_PER_KWYR_BY_TG(TG) = &
                                          ICAP_REVENUE_PER_KWYR(NEXT_CN)
               EXIT
            ENDDO
!
         ENDDO ! MAIN ADD_THIS_UNIT LOOP WITH CONSTRAINTS
!
         if(1==2) then
            MaxTransmissionConstraint = TransmissionConstraint
            MaxTransferValue = 999.0
            LAST_SELLER = -1
            LAST_BUYER = -1
!
            TARGET_IS_PA = .FALSE.
            TG_MW_TRANSFERS = .TRUE.
            MAX_ITER = 20 * UPPER_TRANS_GROUP
            ITER = 0
            DO
               ITER = ITER + 1
               IF(ITER == MAX_ITER) EXIT
               TransferValue = -999.0
               MaxTransferValue = -999.0
               SELLER = 0
               BUYER = 0
!
! IDENTIFY MaxTransferValue, SELLER, BUYER
!
               DO I = 1, UPPER_TRANS_GROUP
                  DO J = I-1,1,-1 ! DON'T CHECK DIAGONAL
                     IF(TG_PEAK_MW(I) > NEAR_ZERO) THEN
                        CM = TG_2_CAPACITY_MARKET(I)
                        TEMP_R = &
                           (TG_Net_Transfer(I) + &
                                 TG_TOTAL_ANNUAL_CAPACITY_ADDED(I) + &
                                             TG_PLANNING_CAPACITY(I))/ &
                                                       TG_PEAK_MW(I)
                        TransferValue(I,J) = TEMP_R
                     ELSE
                        TransferValue(I,J) = 999.0 ! INFINITY
                     ENDIF
                     IF(TG_PEAK_MW(J) > NEAR_ZERO) THEN
                        CM = TG_2_CAPACITY_MARKET(J)
                        TEMP_R = &
                           (TG_Net_Transfer(J) + &
                                 TG_TOTAL_ANNUAL_CAPACITY_ADDED(J) + &
                                             TG_PLANNING_CAPACITY(J))/ &
                                                       TG_PEAK_MW(J)
                        TransferValue(J,I) = TEMP_R
!
                     ELSE
                        TransferValue(J,I) = 999.0 ! INFINITY
                     ENDIF
                     IF(GRX_ITERATIONS == 3 .AND. &
                                               THIS_YEAR == 2014)THEN
                        THIS_YEAR = THIS_YEAR
                     ENDIF
                     PA = TG_2_PLANNING_AREA(I)
                     IF(TARGET_IS_PA) THEN
                        TEMP_R = PA_PLANNING_RESERVE_MARGIN(PA)
                     ELSE
                        TEMP_R = PA_PLANNING_RESERVE_MARGIN(0)
                     ENDIF
                     I_CAPACITY = MAX(0.0, &
                         TG_Net_Transfer(I) + &
                                 TG_TOTAL_ANNUAL_CAPACITY_ADDED(I) + &
                         TG_PLANNING_CAPACITY(I)- &
                                     CURRENT_TARGET_RATIO_PA(PA)* &
                                                     TG_PEAK_MW(I) )
                     PA = TG_2_PLANNING_AREA(J)
                     IF(TARGET_IS_PA) THEN
                        TEMP_R = PA_PLANNING_RESERVE_MARGIN(PA)
                     ELSE
                        TEMP_R = PA_PLANNING_RESERVE_MARGIN(0)
                     ENDIF
                     J_CAPACITY = MAX(0.0, &
                         TG_Net_Transfer(J) + &
                                 TG_TOTAL_ANNUAL_CAPACITY_ADDED(J) + &
                         TG_PLANNING_CAPACITY(J)- &
                                     CURRENT_TARGET_RATIO_PA(PA)* &
                                                     TG_PEAK_MW(J) )
                     IF(I_CAPACITY > 0.1 .OR. J_CAPACITY > 0.1) THEN
                      IF(TransferValue(I,J) > TransferValue(J,I) .AND. &
                                 I_CAPACITY > 0.1 .AND. &
                                 TransmissionConstraint(I,J) > 0.1) THEN
                           TEMP_R = &
                              TransferValue(I,J) - TransferValue(J,I)
                           IF(I == LAST_SELLER .AND. &
                                          J == LAST_BUYER) THEN
                              SAME_TRANSACTION = .TRUE.
                           ELSE
                              SAME_TRANSACTION = .FALSE.
                           ENDIF
                           IF(TEMP_R > MaxTransferValue .AND. &
                                             .NOT. SAME_TRANSACTION)THEN
                              SELLER = I
                              BUYER = J
                              SELLER_CAPACITY = I_CAPACITY
                              BUYER_CAPACITY = J_CAPACITY
                              MaxTransferValue = TEMP_R
                           ENDIF
                        ELSEIF(TransmissionConstraint(J,I)> 0.1 .AND. &
                                                  J_CAPACITY > 0.1) THEN
                           TEMP_R = &
                              TransferValue(J,I) - TransferValue(I,J)
                           IF(J == LAST_SELLER .AND. &
                                          I == LAST_BUYER) THEN
                              SAME_TRANSACTION = .TRUE.
                           ELSE
                              SAME_TRANSACTION = .FALSE.
                           ENDIF
                           IF(TEMP_R > MaxTransferValue .AND. &
                                             .NOT. SAME_TRANSACTION)THEN
                              SELLER = J
                              BUYER = I
                              SELLER_CAPACITY = J_CAPACITY
                              BUYER_CAPACITY = I_CAPACITY
                              MaxTransferValue = TEMP_R
                           ENDIF ! CHECK FOR IMPROVED SOLUTION
                        ENDIF ! I OR J SELLER
                     ENDIF ! CAPACITY CHECK
                  END DO
               END DO
!
               IF(SELLER == 0 .OR. BUYER == 0 .OR. &
                            MaxTransferValue < TransferConvergence) EXIT
!
! IDENTIFY QUANTITY TO TRANSFER
!
               TEMP_R = MIN( &
                         SELLER_CAPACITY, &
                         TransmissionConstraint(SELLER,BUYER))
!
! 081110.   WHAT IS THE CAPACITY THAT MAKES THE TWO RESERVE MARGINS
!
               X_SEARCH = TEMP_R
               HALF = X_SEARCH
               Y_BEFORE_SELLER = TransferValue(SELLER,BUYER)
               X_BEFORE_SELLER = TEMP_R
               Y_BEFORE_BUYER = TransferValue(BUYER,SELLER)
               X_BEFORE_BUYER = 0.0
               DO I = 1, 10
                  X_AFTER_SELLER = TEMP_R - X_SEARCH
                  Y_AFTER_SELLER = &
                      (TG_Net_Transfer(SELLER) + &
                              TG_TOTAL_ANNUAL_CAPACITY_ADDED(SELLER) + &
                       TG_PLANNING_CAPACITY(SELLER)-X_SEARCH)/ &
                                                 TG_PEAK_MW(SELLER)
                  X_AFTER_BUYER = X_SEARCH
                  Y_AFTER_BUYER = &
                      (TG_Net_Transfer(BUYER) + &
                              TG_TOTAL_ANNUAL_CAPACITY_ADDED(BUYER) + &
                       TG_PLANNING_CAPACITY(BUYER)+X_SEARCH)/ &
                                               TG_PEAK_MW(BUYER)
                  IF(ABS(Y_AFTER_SELLER - Y_AFTER_BUYER) < 0.01) THEN
                     TEMP_R = X_SEARCH
                     EXIT
                  ELSE
                     HALF = HALF * 0.5
                     IF(Y_AFTER_SELLER > Y_AFTER_BUYER) THEN
                        IF(I == 1) THEN
                           TEMP_R = X_SEARCH
                           EXIT
                        ELSE ! X_SEARCH IS TOO SMALL
                           X_SEARCH = X_SEARCH + HALF
                        ENDIF
                     ELSE ! X_SEARCH IS TOO BIG
                        X_SEARCH = X_SEARCH - HALF
                     ENDIF
                  ENDIF
               ENDDO
!
               IF(FirmTransfer(BUYER,SELLER) < 0.001) THEN
                  FirmTransfer(SELLER,BUYER) = &
                     FirmTransfer(SELLER,BUYER) + TEMP_R
               ELSE
                  IF(FirmTransfer(BUYER,SELLER) > TEMP_R) THEN
                     FirmTransfer(BUYER,SELLER) = &
                                     FirmTransfer(BUYER,SELLER) - TEMP_R
                  ELSE
                     FirmTransfer(SELLER,BUYER) = &
                                     TEMP_R - FirmTransfer(BUYER,SELLER)
                     FirmTransfer(BUYER,SELLER) = 0.0
                  ENDIF
               ENDIF
               TransmissionConstraint(SELLER,BUYER) = &
                        MaxTransmissionConstraint(SELLER,BUYER) - &
                                              FirmTransfer(SELLER,BUYER)
               TransmissionConstraint(BUYER,SELLER) = &
                        MaxTransmissionConstraint(BUYER,SELLER) - &
                                              FirmTransfer(BUYER,SELLER)
!
               TG_Net_Transfer(SELLER) = &
                                       TG_Net_Transfer(SELLER) - TEMP_R
               TG_Net_Transfer(BUYER) = TG_Net_Transfer(BUYER) + TEMP_R
               SELLER_CM = TG_2_CAPACITY_MARKET(SELLER)
               BUYER_CM = TG_2_CAPACITY_MARKET(BUYER)
               SELLER_PA = TG_2_PLANNING_AREA(SELLER)
               BUYER_PA = TG_2_PLANNING_AREA(BUYER)
               IF(SELLER_CM /= BUYER_CM) THEN
                  CM_Net_Transfer(SELLER_CM) = &
                     CM_Net_Transfer(SELLER_CM) - TEMP_R
                  CM_Net_Transfer(BUYER_CM) = &
                     CM_Net_Transfer(BUYER_CM) +  TEMP_R
               ENDIF
               IF(SELLER_PA /= BUYER_PA) THEN
                  PA_Net_Transfer(SELLER_PA) = &
                     PA_Net_Transfer(SELLER_PA) - TEMP_R
                  PA_Net_Transfer(BUYER_PA) = &
                     PA_Net_Transfer(BUYER_PA) +  TEMP_R
               ENDIF
               LAST_SELLER = SELLER
               LAST_BUYER = BUYER
            END DO
!
! 040611. TO RECALCULATE EXISTING UNITS CAPACITY.
!
            DO CM = 1, CG
               IF(LAST_CM_RESOURCE_ADDED(CM) < 1) CYCLE
               DO I = HO, 1, -1
                  NEXT_CN = SORTED_OPTIONS(I)
                  HIGHEST_OPTION = &
                                 HGST_OPT_INDEX(NEXT_CN)
                  IF(MRX_ICAP_INDEX(NEXT_CN) /= 1 .AND. &
                                    MRX_ICAP_INDEX(NEXT_CN) /= 10) CYCLE
                  IF(.NOT. &
                         DEPENDENT_UNIT_AVAILABLE(HIGHEST_OPTION)) CYCLE

                  TG = GET_TRANS_GROUP_POSITION( &
                                      TRANSACTION_GROUP(HIGHEST_OPTION))
                  TEMP_CM = TG_2_CAPACITY_MARKET(TG)
!
                  IF(CM /= TEMP_CM) CYCLE
!
                  TEMP_I2 = GET_REGIONAL_CM_NAME(CM,CM_NAME)
                  WRITE(LOCAL_NAME,1010) &
                            OPTION_NAME(HIGHEST_OPTION)(1:17),HO - I + 1
!
                  MARKET_AVE_REVENUE = 0.
                  TOTAL_AVERAGE_COST = 0.
!
                  IF(TG_MW_TRANSFERS) THEN
                     TG_CAPACITY_MW(TG) = &
                           TG_PLANNING_CAPACITY(TG) + &
                                 TG_TOTAL_ANNUAL_CAPACITY_ADDED(TG) + &
                                    TG_Net_Transfer(TG)
                  ELSE
                     TG_CAPACITY_MW(TG) = &
                           TG_PLANNING_CAPACITY(TG) + &
                                 TG_TOTAL_ANNUAL_CAPACITY_ADDED(TG)
                  ENDIF
                  IF(TG_PEAK_MW(TG) > .01) THEN
                     TG_RESERVE_MARGIN(TG) = 100. * &
                              (TG_CAPACITY_MW(TG)/TG_PEAK_MW(TG) - 1.0)
                  ELSE
                     TG_RESERVE_MARGIN(TG) = 100.
                  ENDIF
!
                  PA_CAPACITY_MW(CM) = &
                           PA_PLANNING_CAPACITY(CM) + &
                                 TOTAL_ANNUAL_CAPACITY_ADDED(CM)
                  IF(PA_PEAK_MW(CM) > .01) THEN
                     PA_RESERVE_MARGIN(CM) = 100. * &
                              (PA_CAPACITY_MW(CM)/PA_PEAK_MW(CM) - 1.0)
                  ELSE
                     PA_RESERVE_MARGIN(CM) = 100.
                  ENDIF
!
                  IF(TG_MW_TRANSFERS) THEN
                     CM_CAPACITY_MW(CM) = &
                           CM_PLANNING_CAPACITY(CM) + &
                                 CM_TOT_ANN_CAPACITY_ADDED(CM) + &
                                    CM_Net_Transfer(CM)
                  ELSE
                     CM_CAPACITY_MW(CM) = &
                           CM_PLANNING_CAPACITY(CM) + &
                                 CM_TOT_ANN_CAPACITY_ADDED(CM)
                  ENDIF
                  IF(CM_PEAK_MW(CM) > .01) THEN
                     CM_RESERVE_MARGIN(CM) = 100. * &
                              (CM_CAPACITY_MW(CM)/CM_PEAK_MW(CM) - 1.0)
                  ELSE
                     CM_RESERVE_MARGIN(CM) = 100.
                  ENDIF
!
                  TEMP_R4 = CM_RESERVE_MARGIN(CM)*0.01 + 1.0
                  ICAP_DEMAND_CURVE_MULT(NEXT_CN) = &
                                GET_ICAP_REVENUE_MULT(R_YEAR,CM,TEMP_R4)
                  IF(SCREEN_CAPACITY(HIGHEST_OPTION) > 0.01) THEN
                     EAS_PROFIT_PER_KWYR(NEXT_CN) = 0.001 * &
                        1000000. * (ENERGY_REVENUE_MM(NEXT_CN) - &
                                           VARIABLE_COST_MM(NEXT_CN))/ &
                                       SCREEN_CAPACITY(HIGHEST_OPTION)
                     IF(ICAP_SWITCH_INDEX(CM) == 2) THEN
                        CONE_COST_PER_KWYR(NEXT_CN) = &
                                             LOWEST_FIXED_COST_BY_CM(CM)
                     ELSE
                        CONE_COST_PER_KWYR(NEXT_CN) =ICAP_CONE_PRICE(CM)
                     ENDIF
                     CONE_DEMAND_CURVE_ADJ_PER_KWYR(NEXT_CN) = &
                                    CONE_COST_PER_KWYR(NEXT_CN) * &
                                  ICAP_DEMAND_CURVE_MULT(NEXT_CN)
!
! 111509. REDEFINED PER NORM.
!
                     TEMP_R4 = 1000.0 * &
                               (FIXED_COST_MM(NEXT_CN) + &
                                    VARIABLE_COST_MM(NEXT_CN) - &
                                       RPS_REVENUE_MM(NEXT_CN) - &
                                          ENERGY_REVENUE_MM(NEXT_CN))/ &
                                     SCREEN_CAPACITY(HIGHEST_OPTION)
!
! 101010. TAKING OUT THE VRR DEMAND CURVE MULT FROM THIS CALC
!         TO SMOOTH THE CURVES.
!
                     IF(ICAP_SWITCH_INDEX(CM) == 2 .OR.   & !  MARGINAL
                                        ICAP_SWITCH_INDEX(CM) == 3) THEN
                                        ! CONE-MRX
                        ICAP_REVENUE_PER_KWYR(NEXT_CN) = &
                           MAX(0.0, &
                          MIN(CONE_DEMAND_CURVE_ADJ_PER_KWYR(NEXT_CN), &
                                                           TEMP_R4))
                     ELSE
                        ICAP_REVENUE_PER_KWYR(NEXT_CN) = &
                                                     ICAP_CONE_PRICE(CM)
                     ENDIF
                  ELSE
                     EAS_PROFIT_PER_KWYR(NEXT_CN) = 0.0
                     CONE_COST_PER_KWYR(NEXT_CN) = 0.0
                     CONE_DEMAND_CURVE_ADJ_PER_KWYR(NEXT_CN) = &
                                                                  0.0
                     ICAP_REVENUE_PER_KWYR(NEXT_CN) = 0.0
                  ENDIF
                  CAPACITY_REVENUE_MM(NEXT_CN) = 0.001 * &
                                 ICAP_REVENUE_PER_KWYR(NEXT_CN) * &
                            CAPACITY_PLANNING_MW(HIGHEST_OPTION)
                  IF(HIGHEST_MRX_ENERGY(NEXT_CN) > 0.01) THEN
                     CAPACITY_REVENUE_PER_MWH(NEXT_CN) = &
                            CAPACITY_REVENUE_MM(NEXT_CN) * 1000.0/ &
                                            HIGHEST_MRX_ENERGY(NEXT_CN)
                  ELSE
                     CAPACITY_REVENUE_PER_MWH(NEXT_CN) = 0.0
                  ENDIF
                  HIGHEST_MARKET_REVENUE(NEXT_CN) = &
                                ENERGY_REVENUE_MM(NEXT_CN) + &
                                 RPS_REVENUE_MM(NEXT_CN) + &
                                     CAPACITY_REVENUE_MM(NEXT_CN)
                  REVENUE_PER_MWH(NEXT_CN) = &
                           HIGHEST_ENERGY_REV_PER_MWH(NEXT_CN) + &
                                CAPACITY_REVENUE_PER_MWH(NEXT_CN)
                  FIXED_COST_PER_MWH(NEXT_CN) = &
                        HIGHEST_FOM_PER_MWH(NEXT_CN) + &
                              HIGHEST_LEVEL_CAP_PER_MWH(NEXT_CN)
                  HIGHEST_MRX_MARGIN(NEXT_CN) = &
                              REVENUE_PER_MWH(NEXT_CN) - &
                                 HIGHEST_STRIKE_PRICE(NEXT_CN) - &
                                    FIXED_COST_PER_MWH(NEXT_CN)
                  IF(SCREEN_CAPACITY(HIGHEST_OPTION) > 0.01) THEN
                     HIGHEST_MARGIN_PER_KWYR(NEXT_CN) = 1000.0 * &
                              (HIGHEST_MARKET_REVENUE(NEXT_CN) - &
                                 VARIABLE_COST_MM(NEXT_CN) - &
                                       FIXED_COST_MM(NEXT_CN))/ &
                                 SCREEN_CAPACITY(HIGHEST_OPTION)
                  ELSE
                     HIGHEST_MARGIN_PER_KWYR(NEXT_CN) = -999999.0
                  ENDIF
               ENDDO ! HO COUNTER
            ENDDO ! CM
!
! 040511. RECONSTRUCT ICAP CALC FOR UNITS THAT HAVE BEEN BUILD
!
         endif
!
! END OF TEST THIS SECTION
!
         DO CM = 1, CG
            CM_INDEX = GET_CM_INDEX_FROM_TG(CM)
            IF(LAST_CM_RESOURCE_ADDED(CM) < 1 .AND. &
                                                 MX_SUMMARY_REPORT) THEN
!
               DO I = HO, 1, -1
!
! MAXIMUM RESERVE MARGIN CONDITION
!
                  NEXT_CN = SORTED_OPTIONS(I)
                  HIGHEST_OPTION = &
                                 HGST_OPT_INDEX(NEXT_CN)
!
                  IF(.NOT. &
                         DEPENDENT_UNIT_AVAILABLE(HIGHEST_OPTION)) CYCLE
!
                  TG = GET_TRANS_GROUP_POSITION( &
                                      TRANSACTION_GROUP(HIGHEST_OPTION))
                  TEMP_CM = TG_2_CAPACITY_MARKET(TG)
                  IF(CM /= TEMP_CM) CYCLE
                  EXIT
               ENDDO
!
               IF(CM /= TEMP_CM) CYCLE
               LAST_CM_RESOURCE_ADDED(CM) = NEXT_CN ! HIGHEST_OPTION
!
               TEMP_I2 = GET_REGIONAL_CM_NAME(CM,CM_NAME)
               WRITE(LOCAL_NAME,1010) &
                            OPTION_NAME(HIGHEST_OPTION)(1:17),HO - I + 1
!
               MARKET_AVE_REVENUE = 0.
               TOTAL_AVERAGE_COST = 0.
!
               IF(TG_MW_TRANSFERS) THEN
                  TG_CAPACITY_MW(TG) = &
                           TG_PLANNING_CAPACITY(TG) + &
                                 TG_TOTAL_ANNUAL_CAPACITY_ADDED(TG) + &
                                    TG_Net_Transfer(TG)
               ELSE
                  TG_CAPACITY_MW(TG) = &
                           TG_PLANNING_CAPACITY(TG) + &
                                 TG_TOTAL_ANNUAL_CAPACITY_ADDED(TG)
               ENDIF
!
               IF(TG_PEAK_MW(TG) > .01) THEN
                  TG_RESERVE_MARGIN(TG) = 100. * &
                              (TG_CAPACITY_MW(TG)/TG_PEAK_MW(TG) - 1.0)
               ELSE
                  TG_RESERVE_MARGIN(TG) = 100.
               ENDIF
!
               PA_CAPACITY_MW(CM) = &
                           PA_PLANNING_CAPACITY(CM) + &
                                 TOTAL_ANNUAL_CAPACITY_ADDED(CM)
               IF(PA_PEAK_MW(CM) > .01) THEN
                  PA_RESERVE_MARGIN(CM) = 100. * &
                              (PA_CAPACITY_MW(CM)/PA_PEAK_MW(CM) - 1.0)
               ELSE
                  PA_RESERVE_MARGIN(CM) = 100.
               ENDIF
!
               IF(TG_MW_TRANSFERS) THEN
                  CM_CAPACITY_MW(CM) = &
                           CM_PLANNING_CAPACITY(CM) + &
                                 CM_TOT_ANN_CAPACITY_ADDED(CM) + &
                                    CM_Net_Transfer(CM)
               ELSE
                  CM_CAPACITY_MW(CM) = &
                           CM_PLANNING_CAPACITY(CM) + &
                                 CM_TOT_ANN_CAPACITY_ADDED(CM)
               ENDIF
               IF(CM_PEAK_MW(CM) > .01) THEN
                  CM_RESERVE_MARGIN(CM) = 100. * &
                              (CM_CAPACITY_MW(CM)/CM_PEAK_MW(CM) - 1.0)
                  CM_INTERUPTIBLRESERVE_MARGIN = 100. * &
                         (CM_CAPACITY_MW(CM) - &
                                    CM_PEAK_AFTER_INTERRUPTIBLE(CM)) / &
                                CM_PEAK_AFTER_INTERRUPTIBLE(CM)
               ELSE
                  CM_RESERVE_MARGIN(CM) = 100.
               ENDIF
!
               TEMP_R4 = CM_RESERVE_MARGIN(CM)*0.01 + 1.0
               TEMP_R4 = CM_INTERUPTIBLRESERVE_MARGIN*0.01 + 1.0
               ICAP_DEMAND_CURVE_MULT(NEXT_CN) = &
                                GET_ICAP_REVENUE_MULT(R_YEAR,CM,TEMP_R4)
               IF(SCREEN_CAPACITY(HIGHEST_OPTION) > 0.01) THEN
                  EAS_PROFIT_PER_KWYR(NEXT_CN) = 0.001 * &
                        1000000. * (ENERGY_REVENUE_MM(NEXT_CN) - &
                                           VARIABLE_COST_MM(NEXT_CN))/ &
                                       SCREEN_CAPACITY(HIGHEST_OPTION)
!
                  IF(ICAP_SWITCH_INDEX(CM) == 2) THEN
                     CONE_COST_PER_KWYR(NEXT_CN) = &
                                             LOWEST_FIXED_COST_BY_CM(CM)
                  ELSE
                     CONE_COST_PER_KWYR(NEXT_CN) = ICAP_CONE_PRICE(CM)
                  ENDIF
!
                  CONE_DEMAND_CURVE_ADJ_PER_KWYR(NEXT_CN) = &
                                    CONE_COST_PER_KWYR(NEXT_CN) * &
                                  ICAP_DEMAND_CURVE_MULT(NEXT_CN)
!
! 111509. REDEFINED PER NORM.
!
                  TEMP_R4 = 1000.0 * &
                               (FIXED_COST_MM(NEXT_CN) + &
                                    VARIABLE_COST_MM(NEXT_CN) - &
                                       RPS_REVENUE_MM(NEXT_CN) - &
                                          ENERGY_REVENUE_MM(NEXT_CN))/ &
                                     SCREEN_CAPACITY(HIGHEST_OPTION)
!
! 101010. TAKING OUT THE VRR DEMAND CURVE MULT FROM THIS CALC
!         TO SMOOTH THE CURVES.
!
                  IF(ICAP_SWITCH_INDEX(CM) == 2 .OR.   & !  MARGINAL
                                        ICAP_SWITCH_INDEX(CM) == 3) THEN
                                        ! CONE-MRX
                     ICAP_REVENUE_PER_KWYR(NEXT_CN) = &
                           MAX(0.0, &
                          MIN(CONE_DEMAND_CURVE_ADJ_PER_KWYR(NEXT_CN), &
                                                           TEMP_R4))
                  ELSE
                     ICAP_REVENUE_PER_KWYR(NEXT_CN) = &
                                                     ICAP_CONE_PRICE(CM)
                     ICAP_REVENUE_PER_KWYR(NEXT_CN) = &
                                 MIN(ICAP_CONE_PRICE(CM), &
                                CONE_DEMAND_CURVE_ADJ_PER_KWYR(NEXT_CN))
                  ENDIF
!
               ELSE
                  EAS_PROFIT_PER_KWYR(NEXT_CN) = 0.0
                  CONE_COST_PER_KWYR(NEXT_CN) = 0.0
                  CONE_DEMAND_CURVE_ADJ_PER_KWYR(NEXT_CN) = &
                                                                  0.0
                  ICAP_REVENUE_PER_KWYR(NEXT_CN) = 0.0
               ENDIF
!
! 081719. REMOVED RETIREMENT ICAP.
! 091210. ADDED NEW UNIT
!
               IF(ICAP_REVENUE_PER_KWYR(NEXT_CN) < 0.001) THEN
                  MRX_ICAP_INDEX(NEXT_CN) = 3
                  IF(ICAP_DEMAND_CURVE_MULT(NEXT_CN) < 0.1) THEN
                     IF(ICAP_SWITCH_INDEX(CM) == 2 .OR.   & !  MARGINAL
                                        ICAP_SWITCH_INDEX(CM) == 3) THEN
                                        ! CONE-MRX
                        ICAP_REVENUE_PER_KWYR(NEXT_CN) = &
                           MAX(0.0, &
                                    GET_CM_RETIREMENT_KW(CM,CM_UNIT_NO))
                     ELSE
                        ICAP_REVENUE_PER_KWYR(NEXT_CN) = &
                                                     ICAP_CONE_PRICE(CM)
                     ENDIF
                  ELSE
                     IF(ICAP_SWITCH_INDEX(CM) == 2 .OR.   & !  MARGINAL
                                        ICAP_SWITCH_INDEX(CM) == 3) THEN
                                        ! CONE-MRX
                        ICAP_REVENUE_PER_KWYR(NEXT_CN) = &
                           MAX(0.0, &
                             MIN(ICAP_REVENUE_PER_KWYR(NEXT_CN), &
                                GET_CM_RETIREMENT_KW(CM,CM_UNIT_NO)))
                     ELSE
                        ICAP_REVENUE_PER_KWYR(NEXT_CN) = &
                                                     ICAP_CONE_PRICE(CM)
                     ENDIF
                  ENDIF

                  IF(CM_UNIT_NO > 0) THEN
                     WRITE(4,*) 'MARGINAL ICAP RETIREMENT UNIT', &
                                    UNITNM(CM_UNIT_NO),'ICAP = ', &
                                    ICAP_REVENUE_PER_KWYR(NEXT_CN), &
                                    'Capacity Market =',CM_NAME
                  ELSE
                    WRITE(4,*) 'NO MARGINAL ICAP RETIREMENT UNIT FOR', &
                                'CAPACITY MARKET ',CM_NAME
                     IF(ICAP_SWITCH_INDEX(CM) == 2) THEN
                        ICAP_REVENUE_PER_KWYR(NEXT_CN) = &
                                 CONE_DEMAND_CURVE_ADJ_PER_KWYR(NEXT_CN)
                     ELSE
                        ICAP_REVENUE_PER_KWYR(NEXT_CN) = &
                                                     ICAP_CONE_PRICE(CM)
                     ENDIF
                  ENDIF
               ELSE
                  if(MRX_ICAP_INDEX(NEXT_CN) == 0) &
                                    MRX_ICAP_INDEX(NEXT_CN)= 2

               ENDIF
!
! 110709. ADDED THIS IN THIS SECTION.
!
               CAPACITY_REVENUE_MM(NEXT_CN) = 0.001 * &
                                 ICAP_REVENUE_PER_KWYR(NEXT_CN) * &
                            CAPACITY_PLANNING_MW(HIGHEST_OPTION)
               HIGHEST_MARKET_REVENUE(NEXT_CN) = &
                                ENERGY_REVENUE_MM(NEXT_CN) + &
                                 RPS_REVENUE_MM(NEXT_CN) + &
                                     CAPACITY_REVENUE_MM(NEXT_CN)
               IF(HIGHEST_MRX_ENERGY(NEXT_CN) > 0.01) THEN
                  CAPACITY_REVENUE_PER_MWH(NEXT_CN) = &
                            CAPACITY_REVENUE_MM(NEXT_CN) * 1000.0/ &
                                            HIGHEST_MRX_ENERGY(NEXT_CN)
                  REVENUE_PER_MWH(NEXT_CN) = &
                           HIGHEST_ENERGY_REV_PER_MWH(NEXT_CN) + &
                                CAPACITY_REVENUE_PER_MWH(NEXT_CN)
!
                  FIXED_COST_PER_MWH(NEXT_CN) = &
                        HIGHEST_FOM_PER_MWH(NEXT_CN) + &
                              HIGHEST_LEVEL_CAP_PER_MWH(NEXT_CN)
                  HIGHEST_MRX_MARGIN(NEXT_CN) = &
                              REVENUE_PER_MWH(NEXT_CN) - &
                                 HIGHEST_STRIKE_PRICE(NEXT_CN) - &
                                    FIXED_COST_PER_MWH(NEXT_CN)
               ELSE
                  REVENUE_PER_MWH(NEXT_CN) = 0.0
                  FIXED_COST_PER_MWH(NEXT_CN) = 0.0
                  HIGHEST_MRX_MARGIN(NEXT_CN) = 0.0
                  CAPACITY_REVENUE_PER_MWH(NEXT_CN) = 0.0
               ENDIF
               IF(SCREEN_CAPACITY(HIGHEST_OPTION) > 0.01) THEN
                  HIGHEST_MARGIN_PER_KWYR(NEXT_CN) = 1000.0 * &
                              (HIGHEST_MARKET_REVENUE(NEXT_CN) - &
                                 VARIABLE_COST_MM(NEXT_CN) - &
                                       FIXED_COST_MM(NEXT_CN))/ &
                                 SCREEN_CAPACITY(HIGHEST_OPTION)
               ELSE
                  HIGHEST_MARGIN_PER_KWYR(NEXT_CN) = -999999.0
               ENDIF
!
               MX_ANNUAL_ALT_REC = RPTREC(MX_ANNUAL_ALT_NO)
               WRITE(MX_ANNUAL_ALT_NO,REC=MX_ANNUAL_ALT_REC) &
                        PRT_ENDPOINT(), &
                        FLOAT(THIS_YEAR), &
                        CM_NAME, &
                        LOCAL_NAME, &
                        HIGHEST_STRIKE_PRICE(NEXT_CN), &
                        HIGHEST_ENERGY_REV_PER_MWH(NEXT_CN), & 
                        ! 110309. REDEFINED.
                        HIGHEST_MRX_MARGIN(NEXT_CN), &
                        HIGHEST_MRX_CF(NEXT_CN), &
                        ENERGY_REVENUE_MM(NEXT_CN), &
                        HIGHEST_MARKET_COST(NEXT_CN), & !  6
                        VARIABLE_COST_MM(NEXT_CN), &
                        FIXED_COST_MM(NEXT_CN), &
                        SCREEN_CAPACITY(HIGHEST_OPTION), &
                        HIGHEST_MRX_ENERGY(NEXT_CN), & !  10
                        TOTAL_AVERAGE_COST, & !  NEED DEFINITION
                        HIGHEST_ANN_LEVEL_CAP(NEXT_CN), &
                        HIGHEST_FUEL_COST(NEXT_CN), &
                        HIGHEST_ANNUAL_UNITS(NEXT_CN), &
                        CAPACITY_REVENUE_MM(NEXT_CN), &
                        HIGHEST_MARKET_REVENUE(NEXT_CN), & 
                        ! 16 TOTAL REVENUE
                        HIGHEST_MARGIN_PER_KWYR(NEXT_CN), &
                        HIGHEST_FUEL_COST_PER_MWH(NEXT_CN), &
                        HIGHEST_VAR_OM_PER_MWH(NEXT_CN), & !  19
                        HIGHEST_FUEL_COST_PER_MMBTU(NEXT_CN), &
                        REVENUE_PER_MWH(NEXT_CN), &
                        CAPACITY_REVENUE_PER_MWH(NEXT_CN), &
                        HIGHEST_EMISSIONS_PER_MWH(NEXT_CN), &
                        HIGHEST_FOM_PER_MWH(NEXT_CN), & !  24
                        HIGHEST_LEVEL_CAP_PER_MWH(NEXT_CN), &
                        HIGHEST_VOM_COST_MM(NEXT_CN), &
                        HIGHEST_EMISSIONS_COST_MM(NEXT_CN), & !  27
                        HIGHEST_FOM_COST_MM(NEXT_CN), &
                        FLOAT(TRANSACTION_GROUP(HIGHEST_OPTION)), &
                        TG_CAPACITY_MW(TG), &
                        TG_PEAK_MW(TG), &
                        TG_RESERVE_MARGIN(TG), &
                        FLOAT(CM_INDEX), & !  PLANNING_AREA_ID(NEXT_CN),
                        CM_CAPACITY_MW(CM), &
                        CM_PEAK_MW(CM), &
                        CM_RESERVE_MARGIN(CM), &
                        ICAP_DEMAND_CURVE_MULT(NEXT_CN), &
                        CONE_COST_PER_KWYR(NEXT_CN), & !  38
                        CONE_DEMAND_CURVE_ADJ_PER_KWYR(NEXT_CN), &
                        EAS_PROFIT_PER_KWYR(NEXT_CN), &
                        ICAP_REVENUE_PER_KWYR(NEXT_CN), &
                        HIGHEST_HEAT_RATE(NEXT_CN), & !  42
                        FIXED_COST_PER_MWH(NEXT_CN), &
                        HIGHEST_RFT(NEXT_CN), &
                        HIGHEST_RPM(NEXT_CN), &
                        TG_Net_Transfer(TG), &
                        CM_Net_Transfer(CM), &
                        MRX_ICAP_INDEX(NEXT_CN), &
                        CM_INTERUPTIBLRESERVE_MARGIN, &
                        CM_PEAK_AFTER_INTERRUPTIBLE(CM), &
                        CAPACITY_PLANNING_MW(HIGHEST_OPTION), &
                        RPS_REVENUE_MM(NEXT_CN), &
                        RPS_REVENUE_PER_MWH(NEXT_CN)
               MX_ANNUAL_ALT_REC = MX_ANNUAL_ALT_REC + 1
            ENDIF
!
            PA = CM
            IF(PA_PEAK_AFTER_INTERRUPTIBLE(PA) > NEAR_ZERO)  THEN
               PA_PLANNING_RESERVE_MARGIN(PA) = &
                        1. + (PA_PLANNING_CAPACITY(PA) + &
                               TOTAL_ANNUAL_CAPACITY_ADDED(PA) - &
                                 PA_PEAK_AFTER_INTERRUPTIBLE(PA)) / &
                                      PA_PEAK_AFTER_INTERRUPTIBLE(PA)
            ENDIF
            IF(CM_PEAK_AFTER_INTERRUPTIBLE(CM) > NEAR_ZERO)  THEN
               CM_PLANNING_RESERVE_MARGIN(CM) = & !  100410.
                                        CM_RESERVE_MARGIN(CM)*0.01 + 1.0
            ENDIF
            IF(MINIMUM_ANNUAL_MRX_CAP(PA)  <= .1 + &
                                  TOTAL_ANNUAL_CAPACITY_ADDED(PA)) CYCLE
            WRITE(4,*) "MINIMUM PLANNING AREA RESERVE NOT MET",PA
         ENDDO
!
! ADD ADDITIONS HIGHEST TO LOWEST WITH CAPACITY AND RM CONSTRAINTS
!
      RETURN ! ANNUAL_MARKET_OPTION_COST

!**********************************************************************
      ENTRY GET_TG_TRANSFER_CAPACITY(R_TG,R_TG_TRANSFER_CAPACITY)!082610
!**********************************************************************
         IF(ALLOCATED(TG_Net_Transfer) .AND. R_TG > 0) THEN
            R_TG_TRANSFER_CAPACITY =  TG_Net_Transfer(R_TG)
         ELSE
            R_TG_TRANSFER_CAPACITY =  0.0
         ENDIF
      RETURN

! **********************************************************************
      ENTRY GET_MRX_ICAP_REPORT_VALUES( R_PA, &
                                        R_MRX_CAPACITY, &
                                        R_MRX_ENERGY, &
                                        R_MRX_TOTAL_REVENUE, &
                                        R_MRX_TOTAL_VARIABLE, &
                                        R_MRX_FUEL, &
                                        R_MRX_VOM, &
                                        R_MRX_EMISSIONS, &
                                        R_MRX_TOTAL_COST, &
                                        R_MRX_ANN_CAP_COST, &
                                        R_MRX_NET_MARGIN_MM, &
                                        R_MRX_NET_MARGIN_PER_MWH, &
                                        R_MRX_NET_MARGIN_PER_KWMO, &
                                        R_MRX_CONE_PER_KWMO, &
                                        R_MRX_EAS_REV_PER_KWMO, &
                                        R_MRX_ICAP_VALUE_POINT)
!
         IF(ALLOCATED(INSTALL_CAP_VALUE_LAST_CM_UNIT) .AND. &
                                             R_PA > 0 .AND. &
                                  LAST_CM_RESOURCE_ADDED(R_PA) > 0) THEN
!
            NEXT_CN = LAST_CM_RESOURCE_ADDED(R_PA)
            R_MRX_CAPACITY = HIGHEST_SCREEN_CAPACITY(NEXT_CN)
            R_MRX_ENERGY = HIGHEST_MRX_ENERGY(NEXT_CN)
            R_MRX_TOTAL_REVENUE = HIGHEST_MARKET_REVENUE(NEXT_CN)
            R_MRX_TOTAL_VARIABLE = VARIABLE_COST_MM(NEXT_CN)
            R_MRX_FUEL = HIGHEST_FUEL_COST(NEXT_CN)
            R_MRX_VOM = HIGHEST_VOM_COST_MM(NEXT_CN)
            R_MRX_EMISSIONS = HIGHEST_EMISSIONS_COST_MM(NEXT_CN)
            R_MRX_TOTAL_COST = VARIABLE_COST_MM(NEXT_CN) + &
                                           FIXED_COST_MM(NEXT_CN)
            R_MRX_ANN_CAP_COST = &
                                HIGHEST_ANN_LEVEL_CAP(NEXT_CN)
            R_MRX_NET_MARGIN_MM = HIGHEST_MARKET_REVENUE(NEXT_CN) - &
                                     VARIABLE_COST_MM(NEXT_CN) - &
                                           FIXED_COST_MM(NEXT_CN)
            R_MRX_NET_MARGIN_PER_MWH = HIGHEST_MRX_MARGIN(NEXT_CN)
            R_MRX_NET_MARGIN_PER_KWMO = &
                                     HIGHEST_MARGIN_PER_KWYR(NEXT_CN)
            R_MRX_CONE_PER_KWMO = CONE_COST_PER_KWYR(NEXT_CN)
            R_MRX_EAS_REV_PER_KWMO = EAS_PROFIT_PER_KWYR(NEXT_CN)
            R_MRX_ICAP_VALUE_POINT = ICAP_REVENUE_PER_KWYR(NEXT_CN)
         ELSE
            R_MRX_CAPACITY = 0.0
            R_MRX_ENERGY = 0.0
            R_MRX_TOTAL_REVENUE = 0.0
            R_MRX_TOTAL_VARIABLE = 0.0
            R_MRX_FUEL = 0.0
            R_MRX_VOM = 0.0
            R_MRX_EMISSIONS = 0.0
            R_MRX_TOTAL_COST = 0.0
            R_MRX_ANN_CAP_COST = 0.0
            R_MRX_NET_MARGIN_MM = 0.0
            R_MRX_NET_MARGIN_PER_MWH = 0.0
            R_MRX_NET_MARGIN_PER_KWMO = 0.0
            R_MRX_CONE_PER_KWMO = 0.0
            R_MRX_EAS_REV_PER_KWMO = 0.0
            R_MRX_ICAP_VALUE_POINT = 0.0
         ENDIF
      RETURN

! **********************************************************************
      ENTRY GET_INSTALL_CAP_VALUE_LAST_PA(R_PA,R_RESERVE)
! **********************************************************************
         IF(ALLOCATED(INSTALL_CAP_VALUE_LAST_PA_UNIT)) THEN
            R_RESERVE = &
                  INSTALL_CAP_VALUE_LAST_PA_UNIT(R_PA)
         ELSE
            R_RESERVE = 0.0
         ENDIF
      RETURN

! **********************************************************************
      ENTRY GET_INSTALL_CAP_VALUE_LAST_CM(R_PA,R_RESERVE)
! **********************************************************************
         IF(ALLOCATED(INSTALL_CAP_VALUE_LAST_CM_UNIT)) THEN
            R_RESERVE = &
                  INSTALL_CAP_VALUE_LAST_CM_UNIT(R_PA)
         ELSE
            R_RESERVE = 0.0
         ENDIF
      RETURN

! **********************************************************************
      ENTRY GET_PLANNING_RESERVE_MARGIN(R_PA,R_RESERVE)
! **********************************************************************
         IF(ALLOCATED(PA_PLANNING_RESERVE_MARGIN)) THEN
            R_RESERVE = PA_PLANNING_RESERVE_MARGIN(R_PA)
         ELSE
            R_RESERVE = -999.0
         ENDIF
      RETURN

! **********************************************************************
      ENTRY GET_CAPACITY_MARKET_MARGIN(R_PA,R_RESERVE)
! **********************************************************************
         IF(ALLOCATED(CM_PLANNING_RESERVE_MARGIN)) THEN
            R_RESERVE =  CM_PLANNING_RESERVE_MARGIN(R_PA)
         ELSE
            R_RESERVE = -999.0
         ENDIF
      RETURN

! **********************************************************************
      ENTRY CALC_CM_RESERVE_MARGINS(R_YEAR)
! **********************************************************************
         UPPER_TRANS_GROUP = GET_NUMBER_OF_ACTIVE_GROUPS()
         PG = MAX(GET_NUMBER_OF_PLANNING_GROUPS(),INT(1,2))
         CG = MAX(GET_NUMBER_OF_CAPACITY_MARKETS(),INT(1,2))
         PA_PLANNING_CAPACITY = 0.
         CM_PLANNING_CAPACITY = 0.
         CM_PEAK_AFTER_INTERRUPTIBLE = 0.
         PA_PEAK_AFTER_INTERRUPTIBLE = 0.
         IF(.NOT. ALLOCATED(TG_2_PLANNING_AREA)) RETURN
         DO TG = 1, UPPER_TRANS_GROUP
!
            PLANNING_RM_BEFORE_ADDITIONS = ZERO
!
            TG_2_PLANNING_AREA(TG) = GET_PA_FROM_TG(TG)
            TG_2_CAPACITY_MARKET(TG) = GET_CM_FROM_TG(TG)
            PA = TG_2_PLANNING_AREA(TG)
            CM = TG_2_CAPACITY_MARKET(TG)
            CM_INDEX = GET_CM_INDEX_FROM_TG(CM)
!
            EXISTING_PLANNING_CAPACITY(0) = &
                        GET_CL_TG_CAP( &
                                 INT(0,2),TG,R_YEAR,INT(1,2)) + &
                        GET_EL_TG_CAP(TG,R_YEAR,INT(1,2)) + &
                          DERIV_CAPACITY_PLANNING_BY_TG(TG,R_YEAR)
!
            NEW_PLANNING_CAPACITY(0) = &
                           GET_CL_TG_CAP( &
                                 INT(0,2),TG,R_YEAR,INT(2,2)) + &
                         GET_EL_TG_CAP(TG,R_YEAR,INT(2,2))
            PA_PLANNING_CAPACITY(PA) = PA_PLANNING_CAPACITY(PA) + &
                        EXISTING_PLANNING_CAPACITY(0) + &
                                             NEW_PLANNING_CAPACITY(0)
            CM_PLANNING_CAPACITY(CM) = CM_PLANNING_CAPACITY(CM) + &
                        EXISTING_PLANNING_CAPACITY(0) + &
                                             NEW_PLANNING_CAPACITY(0)
            IF(MRX_SORTED_MIX) THEN ! I THINK THIS IS WHAT STEVE WANTS.
               CAPACITY_PLANNING_PEAK = &
                                     GET_GROUP_PEAK_ON_PEAK_MONTH(TG)
            ELSEIF(USE_REGIONAL_MINIMUM_RM_LOGIC) THEN
               CAPACITY_PLANNING_PEAK = &
                                     GET_PEAK_ON_PA_PEAK_MONTH(TG,PA)
            ELSE
               CAPACITY_PLANNING_PEAK = &
                                     GET_GROUP_PEAK_ON_PEAK_MONTH(TG)
            ENDIF
!
            INTERRUPTIBLE_LOAD = GET_ANNUAL_INTER_CAPACITY(TG)
            PA_PEAK_AFTER_INTERRUPTIBLE(PA) = &
                  PA_PEAK_AFTER_INTERRUPTIBLE(PA) + &
                     MAX(0., &
                         CAPACITY_PLANNING_PEAK - INTERRUPTIBLE_LOAD)
            PA_PEAK_MW(PA) = PA_PEAK_AFTER_INTERRUPTIBLE(PA)
            CM_PEAK_AFTER_INTERRUPTIBLE(CM) = &
                  CM_PEAK_AFTER_INTERRUPTIBLE(CM) + &
                     MAX(0., &
                         CAPACITY_PLANNING_PEAK - INTERRUPTIBLE_LOAD)
            CM_PEAK_MW(CM) = CM_PEAK_AFTER_INTERRUPTIBLE(CM)
!
! 090707. MIN AND MAX RM BY TG
!
            TG_PLANNING_CAPACITY(TG) = &
                        EXISTING_PLANNING_CAPACITY(0) + &
                                             NEW_PLANNING_CAPACITY(0)
            PEAK_AFTER_INTERRUPTIBLE_TG(TG) = &
                     MAX(0., &
                         CAPACITY_PLANNING_PEAK - INTERRUPTIBLE_LOAD)
            IF(PEAK_AFTER_INTERRUPTIBLE_TG(TG) > NEAR_ZERO)  THEN
               TG_PLANNING_RESERVE_MARGIN(TG) = &
                     1. + (TG_PLANNING_CAPACITY(TG) - &
                               PEAK_AFTER_INTERRUPTIBLE_TG(TG)) / &
                                      PEAK_AFTER_INTERRUPTIBLE_TG(TG)
               MINIMUM_TG_MRX_CAP(TG) = &
                        MAX(0., &
                           (GET_MIN_CAP_TESTING_RATIO(TG)* &
                                            SCENARIO_RESERVE_MARGIN - &
                               TG_PLANNING_RESERVE_MARGIN(TG))* &
                                     PEAK_AFTER_INTERRUPTIBLE_TG(TG))
!
               MAXIMUM_TG_MRX_CAP(TG) = &
                        MIN(999999., &
                             (GET_MAX_CAP_TESTING_RATIO(TG)* &
                                            SCENARIO_RESERVE_MARGIN - &
                                  TG_PLANNING_RESERVE_MARGIN(TG)) * &
                                     PEAK_AFTER_INTERRUPTIBLE_TG(TG))
               TG_PEAK_MW(TG) = PEAK_AFTER_INTERRUPTIBLE_TG(TG)
            ENDIF
         ENDDO ! TRANSACTION GROUPS
!
         DO PA = 1, PG
            PA_PEAK_AFTER_INTERRUPTIBLE(0) = &
               PA_PEAK_AFTER_INTERRUPTIBLE(0) + &
                  PA_PEAK_AFTER_INTERRUPTIBLE(PA)
            PA_PLANNING_CAPACITY(0) = &
               PA_PLANNING_CAPACITY(0) + &
                  PA_PLANNING_CAPACITY(PA)
!
            IF(PA_PEAK_AFTER_INTERRUPTIBLE(PA) > NEAR_ZERO)  THEN
               PA_PLANNING_RESERVE_MARGIN(PA) = &
                     1. + (PA_PLANNING_CAPACITY(PA) - &
                              PA_PEAK_AFTER_INTERRUPTIBLE(PA)) / &
                                   PA_PEAK_AFTER_INTERRUPTIBLE(PA)
               MINIMUM_ANNUAL_MRX_CAP(PA) = MAX(0., &
                     (CURRENT_TARGET_RATIO_PA(PA) - &
                        PA_PLANNING_RESERVE_MARGIN(PA))* &
                                     PA_PEAK_AFTER_INTERRUPTIBLE(PA))
!
               IF(USE_MAXIMUM_RM_LOGIC) THEN
                  MAXIMUM_ANNUAL_MRX_CAP(PA) = &
                    MIN(MAXIMUM_ANNUAL_MRX_CAP(PA), &
                        MAX(0., &
                             MAXIMUM_RM_PA(PA) - &
                                   PA_PLANNING_RESERVE_MARGIN(PA)) * &
                                    PA_PEAK_AFTER_INTERRUPTIBLE(PA))
               ENDIF
            ELSE
               PA_PLANNING_RESERVE_MARGIN(PA) = ZERO
            ENDIF
!
! 022210. TAKEN OUT: PA TO CM
!
            PA_ICAP_REVENUE_MULT(PA) = GET_ICAP_REVENUE_MULT( &
                               R_YEAR,PA,PA_PLANNING_RESERVE_MARGIN(PA))
         ENDDO
         IF(PA_PEAK_AFTER_INTERRUPTIBLE(0) > NEAR_ZERO)  THEN
            PA_PLANNING_RESERVE_MARGIN(0) = &
                        1. + (PA_PLANNING_CAPACITY(0) - &
                                 PA_PEAK_AFTER_INTERRUPTIBLE(0)) / &
                                      PA_PEAK_AFTER_INTERRUPTIBLE(0)
         ENDIF
         DO CM = 1, CG
!
! 082412.
!
            ICAP_SWITCH_INDEX(CM) = &
                 GET_ICAP_SWITCH_INDEX(CM, &
                                       R_YEAR, &
                                       TEMP_R4)
            ICAP_CONE_PRICE(CM) = TEMP_R4
!
            IF(CM_PEAK_AFTER_INTERRUPTIBLE(CM) > NEAR_ZERO)  THEN
               CM_PLANNING_RESERVE_MARGIN(CM) = &
                     1. + (CM_PLANNING_CAPACITY(CM) - &
                              CM_PEAK_AFTER_INTERRUPTIBLE(CM)) / &
                                   CM_PEAK_AFTER_INTERRUPTIBLE(CM)
            ELSE
               CM_PLANNING_RESERVE_MARGIN(CM) = ZERO
            ENDIF
!
! 022210. PA TO CM
!
            CM_ICAP_REVENUE_MULT(CM) = GET_ICAP_REVENUE_MULT( &
                               R_YEAR,CM,CM_PLANNING_RESERVE_MARGIN(CM))
         ENDDO
      RETURN
      END

! **********************************************************************
