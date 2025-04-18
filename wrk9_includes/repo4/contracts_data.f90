    module contracts_data
    use prod_arrays_dimensions

     real :: MIN_ENERGY(MAX_CONTRACTS)
     real :: MAX_ENERGY(MAX_CONTRACTS)
     real :: MIN_CAPACITY(MAX_CONTRACTS)
     real :: MAX_CAPACITY(MAX_CONTRACTS)
     real :: CONTRACT_VARIABLE_COST(MAX_CONTRACTS)
     real :: MIN_CONTRACT_FIXED_COST(MAX_CONTRACTS)
     real :: CONTRACT_FIXED_COST(MAX_CONTRACTS)
     real :: CNTR_OWN(MAX_CONTRACTS),CNTR_POOL(MAX_CONTRACTS)
     real :: MAX_RATCHET_PATTERN(MAX_CONTRACTS)
     real :: CNTR_SO2(MAX_CONTRACTS)
     real :: CNTR_CAP_PLAN_FACTOR(MAX_CONTRACTS)
     real :: MIN_RATCHET_PATTERN(MAX_CONTRACTS)
     real :: MAX_ANNUAL_ENERGY(MAX_CONTRACTS)
     real :: MAX_ANNUAL_CAPACITY(MAX_CONTRACTS)
     real :: CT_FIRST_ENERGY_PRICE(MAX_CONTRACTS)
     real :: CT_SECOND_ENERGY_PRICE(MAX_CONTRACTS)
     real :: CT_ENERGY_COST_WEIGHTING_FACTOR(MAX_CONTRACTS)
     real :: CT_ENERGY_COST_ADDER(MAX_CONTRACTS)
     real :: CT_ANNUAL_FIXED_COST(MAX_CONTRACTS)
     real :: CT_CUM_1ST_ENERGY_COST(MAX_CONTRACTS)
     real :: CT_MONTHLY_1ST_ENERGY_PRICE(MAX_CONTRACTS)

     character (len=20) :: CNTRNM(MAX_CONTRACTS)
     character (len=1)  :: CNTR_EXP_ASSIGN(MAX_CONTRACTS)
     character (len=1)  :: CNTR_CAPACITY_SWITCH(MAX_CONTRACTS)
     character (len=1)  :: CNTR_ENERGY_SWITCH(MAX_CONTRACTS)
     character (len=1)  :: CNTRTYPE(MAX_CONTRACTS)
     character (len=18) :: CNTR_EXP_COLLECT(MAX_CONTRACTS)

     integer (kind=2) :: NUMBER_OF_CONTRACTS
     integer (kind=2) :: CNTR_ON_LI(MAX_CONTRACTS)
     integer (kind=2) :: CNTR_OFF_LI(MAX_CONTRACTS)
     integer (kind=2) :: CNTR_ENER_ESC(MAX_CONTRACTS)
     integer (kind=2) :: CNTR_MIN_CAP_ESC(MAX_CONTRACTS)
     integer (kind=2) :: CNTR_CAP_ESC(MAX_CONTRACTS)
     integer (kind=2) :: UPDATE_MONTH(MAX_CONTRACTS)
     integer (kind=2) :: CNTR_GROUP(MAX_CONTRACTS)
     integer (kind=2) :: CT_ENERGY_PATTERN_VECTOR(MAX_CONTRACTS)
     integer (kind=2) :: CT_RESOURCE_ID(MAX_CONTRACTS)
     integer (kind=2) :: CT_SECOND_ENERGY_ESCALATOR(MAX_CONTRACTS)
     integer (kind=2) :: CT_ENERGY_COST_ADDER_ESC_VECTOR(MAX_CONTRACTS) 
     integer (kind=2) :: CT_ANNUAL_FIXED_COST_ESC(MAX_CONTRACTS) 

    end module contracts_data
! 
!
