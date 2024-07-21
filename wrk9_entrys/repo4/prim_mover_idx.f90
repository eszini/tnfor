module prim_mover_idx
use miscmod
use params
use logging
use end_routine
use pms

implicit none
logical :: SawAC=.false., SawBlank=.false.
contains

FUNCTION PRIMARY_MOVER_INDEX_DB(R_UNIT_STR)
!      
!***********************************************************************
      INTEGER (KIND=2)  :: PRIMARY_MOVER_INDEX_DB
      CHARACTER (LEN=6) :: R_UNIT_STR
! END DATA DECLARATIONS 
         PRIMARY_MOVER_INDEX_DB = 13
         SELECT CASE(trim(R_UNIT_STR))
            CASE ('CC') ! Combined Cycle
               PRIMARY_MOVER_INDEX_DB = 1
            CASE ('FC') ! Biomass
               PRIMARY_MOVER_INDEX_DB = 2
            CASE ('GE') ! Geothermal
               PRIMARY_MOVER_INDEX_DB = 3
            CASE ('GT') ! combustion (Gas) Turbine
               PRIMARY_MOVER_INDEX_DB = 4
            CASE ('HY') 
               PRIMARY_MOVER_INDEX_DB = 5
            CASE ('IC') ! Internal Combustion
               PRIMARY_MOVER_INDEX_DB = 6
            CASE ('NA') ! Other
               PRIMARY_MOVER_INDEX_DB = 7  ! Other
            CASE ('NU')
               PRIMARY_MOVER_INDEX_DB = 8
            CASE ('SL') ! Solar
               PRIMARY_MOVER_INDEX_DB = 9
            CASE ('ST') 
               PRIMARY_MOVER_INDEX_DB = 10
            CASE ('WT') ! Wind Turbine
               PRIMARY_MOVER_INDEX_DB = 11
            CASE ('ZZ') ! Landfill synGas
               PRIMARY_MOVER_INDEX_DB = 12
            ! See top of routine and empty string case for 13.
            CASE ('BI') 
               PRIMARY_MOVER_INDEX_DB = 14
            CASE ('LF') 
               PRIMARY_MOVER_INDEX_DB = 15
            CASE ('BA') 
               PRIMARY_MOVER_INDEX_DB = 16
            CASE ('DG') ! Distributed Generation
               PRIMARY_MOVER_INDEX_DB = 17
            CASE ('OW') ! Offshore Wind
               PRIMARY_MOVER_INDEX_DB = 18
            CASE ('HB')
               PRIMARY_MOVER_INDEX_DB = 19
			CASE ('H2')
			   PRIMARY_MOVER_INDEX_DB=20
			CASE ('CS') ! Carbon capture/sequestration
				PRIMARY_MOVER_INDEX_DB=21
            CASE (' ') 
               PRIMARY_MOVER_INDEX_DB = 13
			   er_message="pmi:0001 - PM request for empty string " // &
			   " received" 
			   call end_program(er_message)
            ! TODO:  Ask Greg about "BLANK"
            case ('PMNONE')
			   PRIMARY_MOVER_INDEX_DB=PRIMARY_MOVER_INDEX_DB ! Redundant
			   if(.not. SawBlank) then
			       SawBlank=.true.
				   log_message="Assigning default (13) to unknown " // &
				   "PM string 'PMNONE'."
				   call write_log_entry("prim_mover_idx:0002", &
				   trim(log_message))
				endif
            ! TODO:  Ask Greg about "AC"
            CASE ('AC')
               PRIMARY_MOVER_INDEX_DB=PRIMARY_MOVER_INDEX_DB ! Redundant
			   if(.not.SawAC) then
			      SawAC=.true.
				  log_message="Assigning default (13) to unknown "//&
				   "PM string 'AC'."
				   call write_log_entry("prim_mover_idx:0003", &
				   trim(log_message))
			   endif
               
            CASE default
               er_message="Unknown PM for requested unit " // &
               "string " // trim(R_UNIT_STR) // "."
               er_message="prim_mover_idx:0001: " // trim(er_message)
               call end_program(er_message)
               
         END SELECT
      RETURN
      END function PRIMARY_MOVER_INDEX_DB

      
!! ***********************************************************************
      FUNCTION FUEL_TYPE_2_PRIM_MOVER(R_PRIM_FUEL_TYPE_STR)
!
! ***********************************************************************
      LOGICAL (KIND=1)  :: LEGACY_PRIMARY_MOVER,GET_LEGACY_PRIMARY_MOVER
      INTEGER (KIND=2)  :: FUEL_TYPE_2_PRIM_MOVER
      CHARACTER (LEN=6) :: R_PRIM_FUEL_TYPE_STR
!
! END DATA DECLARATIONS
!
            IF(             R_PRIM_FUEL_TYPE_STR == 'ANT   ' .OR.      & !  Anthracite Coal
                            R_PRIM_FUEL_TYPE_STR == 'BC    ' .OR.      & !  Beneficiated Coal
                            R_PRIM_FUEL_TYPE_STR == 'BIT   ' .OR.      & !  Bituminous Coal
                            R_PRIM_FUEL_TYPE_STR == 'COL   ' .OR.      & !  Coal
                            R_PRIM_FUEL_TYPE_STR == 'LIG   ' .OR.      & !  Lignite Coal
                            R_PRIM_FUEL_TYPE_STR == 'SC    ' .OR.      & !  Coal Based Synfuel
                            R_PRIM_FUEL_TYPE_STR == 'SUB   ' .OR.      & !  Subbituminous Coal
                            R_PRIM_FUEL_TYPE_STR == 'WC    ' .OR.      & !  Waste Coal
                            R_PRIM_FUEL_TYPE_STR == 'COAL  ' .OR.      & !  Coal Fuel Category
                            R_PRIM_FUEL_TYPE_STR == 'C     ' ) THEN    ! Legacy Coal Fuel Category
               FUEL_TYPE_2_PRIM_MOVER = 1                         ! Coal
            ELSEIF(         R_PRIM_FUEL_TYPE_STR == 'BFG   ' .OR.      & !  Blast Furnace Gas
                            R_PRIM_FUEL_TYPE_STR == 'COG   ' .OR.      & !  Coke Oven Gas
                            R_PRIM_FUEL_TYPE_STR == 'LNG   ' .OR.      & !  Liquefied Natural Gas
                            R_PRIM_FUEL_TYPE_STR == 'LPG   ' .OR.      & !  Liquefied Petroleum Gas
                            R_PRIM_FUEL_TYPE_STR == 'NG    ' .OR.      & !  Natural Gas
                            R_PRIM_FUEL_TYPE_STR == 'OG    ' .OR.      & !  Other Gas
                            R_PRIM_FUEL_TYPE_STR == 'PG    ' .OR.      & !  Propane
                            R_PRIM_FUEL_TYPE_STR == 'RG    ' .OR.      & !  Refinery Gas
                            R_PRIM_FUEL_TYPE_STR == 'SGC   ' .OR.      & ! Synthet !  Natural Gas
                            R_PRIM_FUEL_TYPE_STR == 'SNG   ' .OR.      & ! Synthet !  Natural Gas
                            R_PRIM_FUEL_TYPE_STR == 'GAS   ' .OR.      & !  Gas Fuel Category
                            R_PRIM_FUEL_TYPE_STR == 'G     ' ) THEN    ! Legacy Gas Fuel Category
               FUEL_TYPE_2_PRIM_MOVER = 2                          ! Gas
            ELSEIF(         R_PRIM_FUEL_TYPE_STR == 'COK   ' .OR.      & !  Coker bi-product
                            R_PRIM_FUEL_TYPE_STR == 'CRU   ' .OR.      & !  Crude Oil
                            R_PRIM_FUEL_TYPE_STR == 'CTO   ' .OR.      & !  Coal Tar Oil
                            R_PRIM_FUEL_TYPE_STR == 'DFO   ' .OR.      & !  Distillate Fuel Oil
                            R_PRIM_FUEL_TYPE_STR == 'DSL   ' .OR.      & !  Diesel
                            R_PRIM_FUEL_TYPE_STR == 'FO1   ' .OR.      & !  No. 1 Fuel Oil
                            R_PRIM_FUEL_TYPE_STR == 'FO2   ' .OR.      & !  No. 2 Fuel Oil
                            R_PRIM_FUEL_TYPE_STR == 'FO3   ' .OR.      & !  No. 3 Fuel Oil
                            R_PRIM_FUEL_TYPE_STR == 'FO4   ' .OR.      & !  No. 4 Fuel Oil
                            R_PRIM_FUEL_TYPE_STR == 'FO5   ' .OR.      & !  No. 5 Fuel Oil
                            R_PRIM_FUEL_TYPE_STR == 'FO6   ' .OR.      & !  No. 6 Fuel Oil
                            R_PRIM_FUEL_TYPE_STR == 'JF    ' .OR.      & !  Jet Fuel
                            R_PRIM_FUEL_TYPE_STR == 'KER   ' .OR.      & !  Kerosene
                            R_PRIM_FUEL_TYPE_STR == 'OIL   ' .OR.      & ! Oil
                            R_PRIM_FUEL_TYPE_STR == 'RFO   ' .OR.      & !  Residual Fuel Oil
                            R_PRIM_FUEL_TYPE_STR == 'WO    ' .OR.      & !  Waste Oil
                            R_PRIM_FUEL_TYPE_STR == 'FO    ' .OR.      & !  Legacy Oil Fuel Category
                            R_PRIM_FUEL_TYPE_STR == 'O     ' ) THEN    ! Legacy Oil Fuel Category
               FUEL_TYPE_2_PRIM_MOVER = 3                          ! Oil
           ELSEIF(          R_PRIM_FUEL_TYPE_STR == 'URA   ' .OR.      & !  Uranium
                            R_PRIM_FUEL_TYPE_STR == 'UR    ' .OR.      & !  Nuclear Fuel Category
                            R_PRIM_FUEL_TYPE_STR == 'NUC   ' .OR.      & !  Nuclear Fuel Category
                            R_PRIM_FUEL_TYPE_STR == 'NU    ' .OR.      & !  Nuclear Fuel Category
                            R_PRIM_FUEL_TYPE_STR == 'N     ' ) THEN    ! Legacy Nuclear Fuel Category
               FUEL_TYPE_2_PRIM_MOVER = 4                          ! Nuclear
            ELSEIF(         R_PRIM_FUEL_TYPE_STR == 'WAT   ' ) THEN      ! Water
               FUEL_TYPE_2_PRIM_MOVER = 5                         ! Water
            ELSEIF(         R_PRIM_FUEL_TYPE_STR == 'MF    ' .OR.      & !  Multifuel
                            R_PRIM_FUEL_TYPE_STR == 'OTH   ' .OR.      & !  Other
                            R_PRIM_FUEL_TYPE_STR == 'PUR   ' .OR.      & !  Purchased Steam
                            R_PRIM_FUEL_TYPE_STR == 'TDF   ' .OR.      & !  Tires
                            R_PRIM_FUEL_TYPE_STR == 'WH    ' .OR.      & ! Waste Heat
                            R_PRIM_FUEL_TYPE_STR == 'PC    ' .OR.      & !  Petroleum Coke
                            R_PRIM_FUEL_TYPE_STR == 'N/A   ' ) THEN    ! Not Available
               FUEL_TYPE_2_PRIM_MOVER = 6                         ! Other
            ELSEIF(         R_PRIM_FUEL_TYPE_STR == 'AB    ' .OR.      & !  Agriculture Byproduct
                            R_PRIM_FUEL_TYPE_STR == 'BLQ   ' .OR.      & !  Black Liquor
                            R_PRIM_FUEL_TYPE_STR == 'EFL   ' .OR.      & !  E-Fuel
                            R_PRIM_FUEL_TYPE_STR == 'GEO   ' .OR.      & !  Geothermal Steam
                            R_PRIM_FUEL_TYPE_STR == 'LFG   ' .OR.      & !  Landfill Gas
                            R_PRIM_FUEL_TYPE_STR == 'MSB   ' .OR.      & !  Municipal Solid Waste - Biogenic
                            R_PRIM_FUEL_TYPE_STR == 'MSN   ' .OR.      & !  Municipal Solid Waste - Non-Biogenic
                            R_PRIM_FUEL_TYPE_STR == 'MSW   ' .OR.      & !  Municipal Solid Waste
                            R_PRIM_FUEL_TYPE_STR == 'MTE   ' .OR.      & !  Methane
                            R_PRIM_FUEL_TYPE_STR == 'MTH   ' .OR.      & !  Methanol
                            R_PRIM_FUEL_TYPE_STR == 'OBG   ' .OR.      & !  Biomass Gases
                            R_PRIM_FUEL_TYPE_STR == 'OBL   ' .OR.      & !  Biomass Liquids
                            R_PRIM_FUEL_TYPE_STR == 'OBS   ' .OR.      & !  Biomass Solids
                            R_PRIM_FUEL_TYPE_STR == 'REF   ' .OR.      & !  Refuse
                            R_PRIM_FUEL_TYPE_STR == 'SLW   ' .OR.      & !  Sludge Waste
                            R_PRIM_FUEL_TYPE_STR == 'WD    ' .OR.      & !  Wood
                            R_PRIM_FUEL_TYPE_STR == 'WDL   ' .OR.      & !  Wood Waste Liquids
                            R_PRIM_FUEL_TYPE_STR == 'WDS   ' ) THEN    ! Wood Waste Solids
               FUEL_TYPE_2_PRIM_MOVER = 7                         ! Renewable
            ELSEIF(         R_PRIM_FUEL_TYPE_STR == 'SUN   ' .OR.      & !  Solar Thermal
                            R_PRIM_FUEL_TYPE_STR == 'PV    ' .OR.      & !  Solar Photovaoltaic
                            R_PRIM_FUEL_TYPE_STR == 'SOL   ' ) THEN    ! Legacy Solar Renewable
               FUEL_TYPE_2_PRIM_MOVER = 8                         ! Solar Renewable
            ELSEIF(         R_PRIM_FUEL_TYPE_STR == 'WND   ' ) THEN    ! Wind
               FUEL_TYPE_2_PRIM_MOVER = 9                         ! Wind Renewable
            ELSEIF(         R_PRIM_FUEL_TYPE_STR == 'DSM   ' .OR. &
                            R_PRIM_FUEL_TYPE_STR == 'EES   ' ) THEN    ! 022708.
               FUEL_TYPE_2_PRIM_MOVER = 10                         ! DSM AND ENERGY EFFICIENCY STANDARDS
            ELSEIF(         R_PRIM_FUEL_TYPE_STR == 'MWH  ' ) THEN    ! 052817
               FUEL_TYPE_2_PRIM_MOVER = 11                         ! BATTERY
            ELSEIF(         R_PRIM_FUEL_TYPE_STR == 'NA   ' ) THEN     ! NA RESERVED TO IGNORE
               FUEL_TYPE_2_PRIM_MOVER = 0
            ELSE
               IF(GET_LEGACY_PRIMARY_MOVER()) THEN
                  FUEL_TYPE_2_PRIM_MOVER = 6
               ELSE
                  FUEL_TYPE_2_PRIM_MOVER = 0
               ENDIF
            ENDIF
      RETURN
      END function FUEL_TYPE_2_PRIM_MOVER


!function IsPmIndirect(pm)
!logical :: IsPmIndirect, return_value
!integer :: index
!integer, intent(in) :: pm
!
!    return_value=.false.
!    do index=1, ubound(IndirectPMs, 1)
!        if(IndirectPMs(index)==pm) then
!            return_value=.true.
!            exit
!        endif
!    enddo
!    
!    IsPmIndirect=return_value
!    
!end function IsPmIndirect

!function IsPmDirect(pm)
!integer, intent(in) :: PM
!logical :: IsPmDirect, return_value
!integer :: index
!
!    ! TODO: replace loop with lookup. (see https://pgga-es.atlassian.net/browse/EPA-7565 )
!    return_value=.false.
!    
!    ! One of the things this loop does is enable summing of steam inputs into STEAM (MW) JTR
!    do index=1, ubound(DirectPMs,1)
!        if(DirectPMs(index)==pm) THEN
!            return_value=.true.
!            exit
!        ENDIF
!    enddo
!    
!    IsPmDirect=return_value
!
!
!end function IsPmDirect
!
end module prim_mover_idx
