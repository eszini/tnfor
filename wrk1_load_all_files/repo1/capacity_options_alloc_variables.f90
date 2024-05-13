!--------
  MODULE CAPACITY_OPTIONS_ALLOC_VARIABLES
  INTEGER (KIND=2) :: FOR_ALL_OPTIONS=0
  CHARACTER (LEN=20), ALLOCATABLE ::  UNIT_NAME(:)
  CHARACTER (LEN=10), ALLOCATABLE :: RESOURCE_TYPE(:)
  CHARACTER (LEN=40), ALLOCATABLE :: OVERNIGHT_CAPITAL_COST_STOCASTIC(:)
  CHARACTER (LEN=2), ALLOCATABLE :: FILE_SOURCE(:)
  CHARACTER (LEN=1), ALLOCATABLE :: YES_OR_NO_FILLER(:), &
                                    LOADING_TYPE(:)
  REAL (KIND=4), ALLOCATABLE :: UNIT_CAP(:), &
                                MAX_UNIT_CAPACITY(:), &
!                                    CONSTRUCTION_COSTS(:), &
                                CONSTRUCTION_COSTS(:,:), &
                                CCR_FOR_SCREENING(:), &
                                MRX_RISK_ADJUSTMENT(:), &
                                COMPOUND_ESC(:), &
                                THRESHOLD_CAPACITY(:,:), &
                                RENEWABLE_ENERGY_PERCENT(:)
  INTEGER (KIND=2), ALLOCATABLE :: FIRST_YEAR_AVAILABLE(:), &
                                   RESOURCE_ID_NUM(:), &
                                   UNIT_IS_IDENTICAL_TO(:), &
                                   RESOURCE_GROUP_NUM(:), &
                                   LAST_YEAR_AVAILABLE(:), &
                                   OPERATION_LIFE(:), &
                                   LEAD_TIME(:), &
                                   ANNUAL_UNITS(:), &
                                   PRODUCTION_DATA_POINTER(:), &
                                   INVESTMENT_DATA_POINTER(:), &
                                   CUMULATIVE_UNITS(:), &
                                   ESCALATION_VECTOR(:), &
                                   DEPEND_UNIT_NO(:), &
                                   MUT_EXC_UNIT(:), &
                                   POINTER_TO_DEPENDENT_UNIT(:), &
                                   DEPENDENT(:), &
                                   INDEPENDENT(:), &
                                   DEPEND_RESOURCE_ID_2_I(:), &
                                   INDEPENDENT_UNIT_STATUS(:), &
                                   INDPENDENT_UNIT(:), &
                                   BASE_OPTION_LIST(:), &
                                   CYCL_OPTION_LIST(:), &
                                   PEAK_OPTION_LIST(:), &
                                   LOAD_OPTION_LIST(:), &
                                   FILL_OPTION_LIST(:), &
                                   ACTIVE_OPTION_LIST(:), &
                                   HARD_WIRED_LIST(:), &
                                   HARD_WIRED_ON_LINE_YEAR(:), &
                                   SAVED_CUMULATIVE_UNITS(:), &
                                   GRX_SAVED_CUMULATIVE_UNITS(:), &
                                   ANNUAL_UNITS_LEFT(:), &
                                   TOTAL_UNITS_ADDED(:), &
                                   ADDITIONAL_INVESTMENT_POINTER(:), &
                                   OVER_CAPACITY_TABLE(:), &
                                   UNDER_CAPACITY_TABLE(:), &
                                   PRIMARY_ASSET_CLASS(:), &
                                   PRIMARY_ALLOCATION_VECTOR(:), &
                                   ADDITIONAL_ASSET_CLASS(:), &
                                   ADDITIONAL_ALLOCATION_VECTOR(:), &
                                   SAVE_NO_CL_ADDITIONS(:), &
                                   SAVE_NO_EL_ADDITIONS(:), &
                                   ADDED_LM_PROGRAMS(:), &
                                   ADDED_CONTRACT_PROGRAMS(:), &
                                   GRX_RESOURCE_LINK_ID(:)
  LOGICAL (KIND=1), ALLOCATABLE :: CONTROLS_DEPEND(:), &
                                   IS_A_HARD_WIRED_UNIT(:), &
                                   HAS_BEEN_SELECTED(:), &
                                   HARD_WIRED_UNIT_AVAILABLE(:)
  REAL (KIND=4), ALLOCATABLE :: LOWEST_CONE_COST_BY_CM(:), &
                                EAS_REVENUE_OFFSET_BY_CM(:)
  END MODULE CAPACITY_OPTIONS_ALLOC_VARIABLES