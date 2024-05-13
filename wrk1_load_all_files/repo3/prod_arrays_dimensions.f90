      MODULE PROD_ARRAYS_DIMENSIONS
	  implicit none
         INTEGER (KIND=2), PARAMETER :: MAX_EL_UNITS = 15000,&
                                        MAX_CL_UNITS = 25000, &
                                        max_retirement_units = MAX_CL_UNITS
         INTEGER (KIND=4), PARAMETER :: MAX_DISPATCH_BLOCKS = 2*MAX_CL_UNITS

         INTEGER (KIND=2), PARAMETER :: MAX_CONTRACTS=150,&
                                        MAX_REPORTING_GROUPS=15,&
                                        MAX_LOAD_CLASSES = 6,&
                                        MAXIMUM_FUEL_TYPES=200,&
                                        CONVOLUTION_POINTS=1000,&
                                        LOAD_CURVE_POINTS=1000,&
                                        MAX_DSM_DEVICES= 100,&
                                         MAX_DSM_FINANCIAL_RECORDS= 100,&
                                        MAX_DSM_RESPONSE_CURVES=100,&
                                        MAX_EMISSION_REPORT_GROUPS=10,&
                                        MAX_EMISSION_DISPATCH_GROUPS=6,&
                                        NUMBER_OF_EMISSION_TYPES = 5,&
                                        MAX_STATE_LOOKUP_IDS = 400, & !310, &
                                        TOTAL_EMISSION_GROUPS = MAX_EMISSION_REPORT_GROUPS &
                                                                +MAX_EMISSION_DISPATCH_GROUPS
      END MODULE PROD_ARRAYS_DIMENSIONS

