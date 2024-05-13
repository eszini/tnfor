      MODULE CAPACITY_OPTIONS_FIXED_VARIABLES
!
! VARIABLES NEEDED ACROSS ROUTINES
!
      INTEGER (KIND=2) :: TOTAL_ALL_OPTIONS=0, &
                          BASE_OPTIONS=0, &
                          CYCLE_OPTIONS=0, &
                          PEAK_OPTIONS=0, &
                          PEAK_REDUC_OPTIONS=0, &
                          FILL_OPTIONS=0, &
                          HARD_WIRED_OPTIONS=0, &
                          DSM_ADDITIONS=0, &
                          DSM_OPTIONS=0, &
                          CONTRACT_ADDITIONS=0, &
                          CONTRACT_OPTIONS=0, &
                          RENEWABLE_OPTIONS=0, &
                          EFFICIENCY_OPTIONS=0, &
                          CL_OPTIONS=0, &
                          TOTAL_ACTIVE_OPTIONS=0, &
                          MAX_RESOURCE_ID=0, &
                          TOTAL_POSSIBLE_OPTIONS=0
      INTEGER (KIND=2) :: BC_BASE_OPTIONS=0, &
                          BC_CYCLE_OPTIONS=0, &
                          BC_PEAK_OPTIONS=0, &
                          BC_PEAK_REDUC_OPTIONS=0, &
                          BC_FILL_OPTIONS=0, &
                          BC_HARD_WIRED_OPTIONS=0, &
                          BC_TOTAL_ACTIVE_OPTIONS=0, &
                          BC_TOTAL_ALL_OPTIONS=0, &
                          BC_MAX_RESOURCE_ID=0, &
                          BC_TOTAL_POSSIBLE_OPTIONS=0
      INTEGER (KIND=2) :: MAX_ID_FOR_ALL_OPTIONS=0
      INTEGER (KIND=2) :: ON_LINE_MONTH=1
      LOGICAL (KIND=1) :: ALL_OPTIONS_GT_ZERO=.FALSE., &
                          OPTIONS_GT_ZERO=.FALSE.
      END MODULE CAPACITY_OPTIONS_FIXED_VARIABLES
!--------