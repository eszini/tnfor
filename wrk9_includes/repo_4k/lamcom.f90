
module lamcom
use trancom
use PROD_ARRAYS_DIMENSIONS
use prod_arrays_dimensions
use fyregycom
implicit none

!     common: TIM
      INTEGER*2 SIMULATION_YEARS,NUMBER_OF_SEASONS
!

!     common: Reference_loads
      REAL SYSTEM_HISTORICAL_LOADS(24,2,12), &
           SYSTEM_HISTORICAL_PEAKS(24,2,12), &
           THREE_DAY_LOADS(24,3,12), &
           HISTORICAL_ENERGY_IN(2,12)
		   
!     common:  REFERENCE_LOADSI2
      INTEGER*2 SYSTEM_LOAD_ORDER(24,2,12),HISTORICAL_PEAK_HOUR(2,12), &
                HISTORICAL_DAY_COUNT(2,12),HOLIDAYS(12)

      
!     common: CLS_LOAD
      
      real (kind=4) :: FORECAST_CUSTOMERS(MAX_LOAD_CLASSES)=0, &
           HISTORICAL_LOADS(24,2,12,MAX_LOAD_CLASSES)=0, &
           COINCIDENT_HISTORICAL_PEAK(2,12,MAX_LOAD_CLASSES)=0, &
           FORECAST_COINCIDENT_PEAK(2,12,MAX_LOAD_CLASSES), &
           CLASS_LOSSES(MAX_LOAD_CLASSES)=0, &
           CLASS_COIN_FACTOR(MAX_LOAD_CLASSES)=0, &
           CLASS_RESERVE_MARGIN(MAX_LOAD_CLASSES)=0, &
           CLASS_PEAK_LOSSES(MAX_LOAD_CLASSES)=0
           
      real (kind=4) :: FORECAST_ENERGY(2,12,MAX_LOAD_CLASSES)
!
!  INCLUDE THREE DAY TYPE VARIABLES
!
	  ! common: tran2
      real :: THREE_DAY_CLASS_LOADS(24,3,12,7)
      
end module lamcom
