!     ******************************************************************
!     module_Coal_Model_Input_to_CNW.f90
!     Created: 1/7/2011 10:44:33 AM
!     Author : MARK S GERBER
!     Last change: MSG 12/22/2012 12:46:28 PM
!     ******************************************************************

!***********************************************************************
! declarations for interface variables and arrays
      MODULE STATE_POSTAL_CODES
! includes North America NA and District of Columbia DC       
         CHARACTER (LEN=2), PARAMETER :: StateCodes(0:52)=
     +              "'NA','AL','AK','AZ','AR','CA','CO','CT','DE','DC',
     +                'FL','GA','HI','ID','IL','IN','IA','KS','KY','LA',
     +                'ME','MD','MA','MI','MN','MS','MO','MT','NE','NV',
     +                'NH','NJ','NM','NY','NC','ND','OH','OK','OR','PA',
     +                'RI','SC','SD','TN','TX','UT','VT','VA','WA','WV',
     +                'WI','WY','US'"
      END MODULE
      MODULE COAL_MODEL_INPUT_TO_cnw
! array size allocation dimension values
         INTEGER (KIND=2) :: nBasin,   ! number of active basins
     +                       nTransportLinks=0, ! size of both transport link files
     +                       nCoalContracts=0,
     +                       nCoalDemand=0,
     +                       nCoalSupply=0,
     +                       nGenericTransLinks=0, ! number of transaction transport links
     +                       nMaxGenericCoalDemand=0, ! coal units inthe generic coal file
     +                       nGenericCoalOptions=0,  ! number of potential coal units in the expansion plan
     +                       nMaxExistingCoalDemand=0,
     +                       nGenericCoalDemand=0,
     +                       nCoalDemandExistingUnits=0,
     +                       nTransportLinksExistingUnits=0,
     +                       nSO2Limits=0,
     +                       nExistingPlant=0,
     +                       nExistingPlantPlusAdds=0,
     +                       nUnitsInFixedStack=0
         INTEGER (KIND=2) :: nMaxBasinID=0
         REAL (KIND=4) :: ScrubCapCst=1000.0, ! $/kW of plant capacity
     +                    ScrubCapChgRat=0.05, ! fraction allocated to each year
     +                    ScrubFOMC=20.0, ! $/kW/yr
     +                    ScrubVOMC= 3.0, ! $/MWh
     +                    MWh_per_MBttu=10.0, ! (nominal heat-rate 10k Btu/MWh)*(MBtu/1000kBtu)
     +                    kWcap_per_MBtu=1.427, ! assume 80% duty for 8760 h: (10MWh/MBtu)*(1000k/M)*(MWcap/(8760*0.8 MWh))
     +                    SulphurCst=1.,
     +                    Sulphur_UB=10000
! basin arrays dimensioned using nBasin
!***********************************************************************
! VARIABLES USED IN cnw
         CHARACTER (LEN=27), ALLOCATABLE :: Basin_Abbrev(:) ! Basin Abbrev
!
         INTEGER (KIND=4), ALLOCATABLE :: Basin_ID(:),     ! Basin HI ID
     +                                    BasinPointer(:)
         REAL (KIND=4), ALLOCATABLE :: Basin_QtyLB(:),     ! Minimum Quantity Current Yr
     +                                 Basin_QtyUB(:),     ! Maximum Quantity Current Yr
     +                                 Basin_QtyLBByYr(:,:),    ! Maximum Quantity By Yr
     +                                 Basin_QtyUBByYr(:,:)     ! Maximum Quantity By Yr
! VARIABLES NOT USE IN cnw
         CHARACTER (LEN=50), ALLOCATABLE :: Basin_Name(:)
         REAL (KIND=4), ALLOCATABLE :: Basin_Annual_BaseYr_Quantity(:)
         LOGICAL (KIND=4), ALLOCATABLE :: BasinAvailable(:),
     +                                    BasinWithLinkstoPlant(:)
! end basin arrays
!***********************************************************************
! exiting plant transportation link arrays
         CHARACTER (LEN=50), ALLOCATABLE :: Route_Name(:),
     +                                      Transport_Vehicles(:)
         INTEGER (KIND=4), ALLOCATABLE :: Coal_Link_ID(:),
     +                                    Basin_HI_ID(:),
     +                                    Plant_EV_ID(:)
         REAL (KIND=4), ALLOCATABLE :: Transport_Price(:),
     +                                 Annual_Quantity_Limit(:)
         LOGICAL (KIND=4), ALLOCATABLE :: PlantToBasinLinkActive(:)
         INTEGER (KIND=4) :: EVPlantIDMin,EVPlantIDMax
         REAL (KIND=4), ALLOCATABLE :: Route_Price_By_Year(:,:),
     +                                 Route_Quantity_Limit_By_Year(:,:)         
         REAL (KIND=4), ALLOCATABLE :: RouteLimitToPlant(:)
! end transportation link arrays
!***********************************************************************
! generic plant transportation link arrays
         CHARACTER (LEN=50), ALLOCATABLE :: Generic_Route_Name(:),
     +                                      GenericTransportVehicles(:)
         INTEGER (KIND=4), ALLOCATABLE :: Generic_Coal_Link_ID(:),
     +                                    Generic_Basin_HI_ID(:),
     +                                    Generic_Plant_EV_ID(:)
         INTEGER (KIND=2), ALLOCATABLE :: GenericTransportPtrToTG(:,:)
         REAL (KIND=4), ALLOCATABLE :: Generic_Route_Price_By_Year(:,:),
     +                               Generic_Route_Quantity_By_Year(:,:)         
         LOGICAL (KIND=1), ALLOCATABLE :: GenericLinkUsed(:),
     +                                    GenericLinkUsedTemp(:)
! end transportation link arrays
!***********************************************************************
! supply arrays
         CHARACTER (LEN=50), ALLOCATABLE :: Supply_Description(:)
         INTEGER (KIND=2), ALLOCATABLE ::  Supply_Basin_ID(:),
     +                                     Supply_Fuel_Type_ID(:),
     +                                     Supply_Curve_Points(:),
     +                                     CoalDemandLinkToCLPlants(:)
         REAL (KIND=4), ALLOCATABLE :: Supply_Heat_Content(:),
     +                             Supply_SO2_Rate(:),
     +                             Supply_CO2_Rate(:),
     +                             Supply_Quantity_Consumed_BaseYr(:),
     +                             Supply_Minimum_Annual_Production(:),
     +                             Supply_Maximum_Annual_Production(:),
     +                             Supply_Production_Escalation(:),
     +                             Supply_Production_Cost_Escalation(:),
     +                             SupplyLastCost(:) 
        REAL (KIND=4), ALLOCATABLE :: Supply_Production(:,:),  ! alloc 20,narray
     +                                Supply_Cost(:,:),   ! alloc 20,narray
     +                                UnitCoalCostByBTU(:,:)
        REAL (KIND=4), ALLOCATABLE :: Supply_Production_By_Year(:,:,:),
     +                                Supply_Cost_By_Year(:,:,:)
! end supply  arrays
!***********************************************************************
! existing plant demand  arrays
         LOGICAL (KIND=4), ALLOCATABLE :: AddScrubber(:)
         CHARACTER (LEN=50), ALLOCATABLE :: Plant_Name(:),
     +                                      Plant_Unit_Name(:),
     +                                      PlantListName(:)
         CHARACTER (LEN=2), ALLOCATABLE :: StateLocation(:)
         INTEGER (KIND=4), ALLOCATABLE ::  EV_Plant_ID(:),Plant_ID(:)
         INTEGER (KIND=8), ALLOCATABLE ::  EV_Unit_ID(:)
         REAL (KIND=4), ALLOCATABLE :: Plant_Heat_Lower_Bound(:),
     +                                 Plant_Heat_Upper_Bound(:),
     +                                 Plant_Fraction_Unscrubbed(:),
     +                               PlantFractionUnscrubbedByYear(:,:),
     +                                 Plant_Heat_Content_Base_Year(:),
     +                                 Plant_Percent_Increase(:),
     +                                 Plant_Percent_Decrease(:),
     +                                 UnitCapacity(:),
     +                                 UnitIMEnergy(:),
     +                                 AverageHeatrate(:),
     +                                 SOxControlPercent(:),
     +                                 SOxControlDate(:),
     +                                 SOxControlVarCost(:),
     +                                 SOxControlFixedCost(:),
C     +                                 SO2BaseYear(:),
     +                                 SO2MaxInBlend(:),
     +                                 SO2MaxInAnyCoal(:),
C     +                                 SO2LowerBound(:),
C     +                                 SO2UpperBound(:),
     +                                 SOxControlCapitalCost(:),
     +                                 SOxControlCarryingCharge(:)
         REAL (KIND=8), ALLOCATABLE :: Plant_Annual_Demand_By_Year(:,:),
     +                                 ThermalDemandByTG(:),
     +                                 Plant_Annual_Demand(:)
         LOGICAL (KIND=4), ALLOCATABLE :: LinkToPlantLinkActive(:),
     +                                    OptUnScrubbedFrac(:),
     +                                    ExportNonUtilityDemand(:)
 
         INTEGER (KIND=2), ALLOCATABLE :: OnLineDate(:),OffLineDate(:),
     +                                TransGroupID(:),UnitToPlantPtr(:),
     +                                UnitLinkToState(:),
     +                                EmissionTablePointer(:)
         CHARACTER (LEN=30), ALLOCATABLE :: TransGroupName(:)
         REAL (KIND=4), ALLOCATABLE :: PlantThermalDemand(:),
     +                                 PlantTotalCapacity(:),
     +                                 PlantTotalEnergy(:),
     +                                 PlantAverageHeatRate(:)
         REAL (KIND=4), ALLOCATABLE :: SOxCapitalCostByYear(:,:),
     +                                 SOxCarryingChargeByYear(:,:),
     +                                 SOxVarCostByYear(:,:),
     +                                 SOxFixedCostByYear(:,:),
     +                                 MaxSO2InCoalBlend(:,:),
     +                                 MaxSO2ForAnyCoalInBlend(:,:)
! end demand  arrays
! generic demand  arrays
         CHARACTER (LEN=50), ALLOCATABLE :: Generic_Plant_Name(:)
         CHARACTER (LEN=2), ALLOCATABLE :: Generic_StateLocation(:)
         INTEGER (KIND=4), ALLOCATABLE ::  Generic_Plant_ID(:)
         REAL (KIND=4),ALLOCATABLE :: Generic_Plant_Heat_Lower_Bound(:),
     +                        Generic_Plant_Heat_Upper_Bound(:),
     +                        Generic_Plant_Fraction_Unscrubbed(:),
     +                        Generic_Plant_Heat_Content(:),
     +                        Generic_Plant_Percent_Increase(:),
     +                        Generic_Plant_Percent_Decrease(:),
     +                        Generic_Plant_ForecastDemand(:,:),
     +                        Generic_Plant_Capacity(:),
     +                        Generic_Plant_AverageHeatrate(:),
     +                        Generic_Plant_CoalDemandLinkToCLPlants(:),
     +                        Generic_MaxSO2InCoalBlend(:,:),
     +                        Generic_MaxSO2ForAnyCoalInBlend(:,:)
         REAL (KIND=8), ALLOCATABLE :: Generic_Plant_Annual_Demand(:),
     +                                Generic_Plant_ThermalDemandByTG(:)
         INTEGER (KIND=2), ALLOCATABLE :: Generic_Plant_TransGroupID(:),
     +                                    Generic_Plant_OnLineDate(:),
     +                                    Generic_Plant_OffLineDate(:),
     +                                    GenericPlantPtrByTG(:)
         CHARACTER (LEN=30), ALLOCATABLE :: 
     +                                   Generic_Plant_TransGroupName(:)
         CHARACTER (LEN=2),ALLOCATABLE :: Generic_Plant_StateLocation(:)
! end generic demand  arrays
!***********************************************************************
! contracts  arrays
         CHARACTER (LEN=50), ALLOCATABLE :: Contract_Name(:),
     +                                      Contract_Mine_Name(:),
     +                                      Contract_Mine_Basin_Name(:),
     +                                      Contract_Coal_Type(:)    ! ContractCoalName(iCont)
         INTEGER (KIND=4), ALLOCATABLE :: Contract_ID(:),
     +                                    Contract_Mine_ID(:),
     +                                    Nth_Contract(:)
         INTEGER (KIND=4), ALLOCATABLE :: Contract_Plant_ID(:),     ! GPntID(iCont)
     +                                    Contract_Basin_ID(:)
         CHARACTER (LEN=15), ALLOCATABLE :: Contract_Type(:)
        REAL (KIND=4), ALLOCATABLE  :: Contract_Annual_Quantity(:,:), ! qInYr(iCont,30)
     +                                  Contract_Annual_Price(:,:)     ! pInYr(iCont,30)

! end contracts  arrays
!***********************************************************************
! SO2 markets, limits, prices
      REAL (KIND=4) :: SO2EmissionLimits(0:52),
     +                 SO2CreditPrice(0:52),
     +                 SO2ResultsEmissions(0:2,0:52), ! 0=>kTSO2,1=>k$cost,2=>ShadowPrice(units?)
     +                 SO2ResultsShadPrGrp(2), ! ShadowPrice for state-groups
     +                 StateCapVolatility (0:52)
      CHARACTER (LEN=2) :: SO2MarketCodes(0:52)
      INTEGER (KIND=4) :: StateGroupAssignment(0:52)
      END MODULE COAL_MODEL_INPUT_TO_cnw
!***********************************************************************
      MODULE COAL_cnw_FILES_READ_INPUT
! COAL_CONTRACTS_READ_DATA
         INTEGER (KIND=2), PARAMETER :: COAL_CONTRACTS_LRECL=512 ! OUTPUT FILE RECORD LENGTH
         CHARACTER (LEN=40), PARAMETER ::
     +             COAL_CONTRACTS_BINARY_FILE_NAME ="COAL_CONTRACTS.BIN"
         LOGICAL (KIND=4),SAVE ::COAL_CONTRACTS_DATA_FILE_EXISTS=.FALSE.
         CHARACTER (LEN=2),SAVE ::PROCESSING_COAL_CONTRACTS_FILE_OL='BC'
         INTEGER (KIND=2),SAVE :: ACTIVE_COAL_CONTRACTS_IN_BASEFILE=0,
     +                             ACTIVE_COAL_CONTRACTS_IN_OVERLAY=0
! PLANT_DEMAND_READ_DATA
         INTEGER (KIND=2), PARAMETER :: PLANT_DEMAND_LRECL=512 ! OUTPUT FILE RECORD LENGTH
         CHARACTER (LEN=40), PARAMETER ::
     +            PLANT_DEMAND_BINARY_FILE_NAME ="PLANT_COAL_DEMAND.BIN"
         LOGICAL (KIND=4), SAVE :: PLANT_DEMAND_DATA_FILE_EXISTS=.FALSE.
         CHARACTER (LEN=2), SAVE :: PROCESSING_PLANT_DEMAND_FILE_OL='BC'
         INTEGER (KIND=2), SAVE :: ACTIVE_PLANT_DEMAND_IN_BASEFILE=0,
     +                             ACTIVE_PLANT_DEMAND_IN_OVERLAY=0
! GENERIC_DEMAND_READ_DATA
         INTEGER (KIND=2), PARAMETER :: GENERIC_DEMAND_LRECL=256 ! OUTPUT FILE RECORD LENGTH
         CHARACTER (LEN=40), PARAMETER ::
     +        GENERIC_DEMAND_BINARY_FILE_NAME ="GENERIC_COAL_DEMAND.BIN"
         LOGICAL(KIND=4),SAVE :: GENERIC_DEMAND_DATA_FILE_EXISTS=.FALSE.
         CHARACTER(LEN=2),SAVE :: PROCESSING_GENERIC_DEMAND_FILE_OL='BC'
         INTEGER (KIND=2), SAVE :: ACTIVE_GENERIC_DEMAND_IN_BASEFILE=0,
     +                             ACTIVE_GENERIC_DEMAND_IN_OVERLAY=0
! COAL_TRANSPORTATION_READ_DATA
         INTEGER (KIND=2), PARAMETER :: COAL_LINK_LRECL=256 ! OUTPUT FILE RECORD LENGTH
         CHARACTER (LEN=40) :: COAL_LINK_BINARY_FILE_NAME(2)=(/
     +                                    "COAL_TRANSPORTATION.BIN   ",
     +                                    "COAL_TRANSPORT_GENERIC.BIN"/)
         LOGICAL (KIND=4) ::  COAL_LINK_DATA_FILE_EXISTS=.FALSE.,
     +                             GENERIC_LINK_DATA_FILE_EXISTS=.FALSE.         
         CHARACTER (LEN=2) :: 
     +                    PROCESSING_COAL_LINK_FILE_OL(2)=(/"BC","BC"/)
         INTEGER (KIND=2) :: ACTIVE_COAL_LINK_IN_BASEFILE(2)=2*0,
     +                             ACTIVE_COAL_LINK_IN_OVERLAY(2)=2*0,
     +                             COAL_TRANSPORT_RECORDS(2)=2*0
         INTEGER (KIND=4) :: MAX_EV_PLANT_ID_BASEFILE(2)=2*0,
     +                       MIN_EV_PLANT_ID_BASEFILE(2)=2*99999999,         
     +                       MAX_EV_PLANT_ID_OVLFILE(2)=2*0,         
     +                       MIN_EV_PLANT_ID_OVLFILE(2)=2*99999999         
! COAL_BASIN_READ_DATA
         INTEGER (KIND=2), PARAMETER :: BASIN_LRECL=128 ! OUTPUT FILE RECORD LENGTH
         CHARACTER (LEN=40), PARAMETER ::
     +                    BASIN_BINARY_FILE_NAME ="COAL_BASIN.BIN"
         LOGICAL (KIND=4), SAVE ::  BASIN_DATA_FILE_EXISTS=.FALSE.
         CHARACTER (LEN=2), SAVE :: PROCESSING_BASIN_FILE_OL='BC'
         INTEGER (KIND=4), SAVE :: ACTIVE_BASINS_IN_BASEFILE=0,
     +                             ACTIVE_BASINS_IN_OVERLAY=0,
     +                             MAX_BASIN_ID_BASEFILE=0,
     +                             MAX_BASIN_ID_OVLFILE=0
! COAL_SUPPLY_READ_DATA
         INTEGER (KIND=2), PARAMETER :: COAL_SUPPLY_LRECL=256 ! OUTPUT FILE RECORD LENGTH
         CHARACTER (LEN=40), PARAMETER ::
     +                   COAL_SUPPLY_BINARY_FILE_NAME ="COAL_SUPPLY.BIN"
         LOGICAL (KIND=4), SAVE ::  COAL_SUPPLY_DATA_FILE_EXISTS=.FALSE.
         CHARACTER (LEN=2), SAVE :: PROCESSING_COAL_SUPPLY_FILE_OL='BC'
         INTEGER (KIND=4), SAVE :: ACTIVE_FUEL_SUPPLY_BASEFILE=0,
     +                             ACTIVE_FUEL_SUPPLY_OVERLAY=0,
     +                             MAX_SUPPLY_BASIN_ID_BASEFILE=0,
     +                             MAX_SUPPLY_BASIN_ID_OVLFILE=0
! COAL_SO2_Scrubber_READ_DATA
         INTEGER (KIND=2), PARAMETER :: COAL_SO2_Scrubber_LRECL=128 ! OUTPUT FILE RECORD LENGTH
         CHARACTER (LEN=40), PARAMETER ::
     +       COAL_SO2_Scrubber_BINARY_FILE ="COAL_SO2_Scrubber.BIN"
         LOGICAL (KIND=4), SAVE :: COAL_SO2_Scrubber_FILE_EXISTS=.FALSE.
         CHARACTER (LEN=2), SAVE ::
     +                         PROCESSING_COAL_SO2_Scrubber_FILE_OL='BC'
         INTEGER (KIND=2), SAVE :: ACTIVE_SO2_Scrubber_BASEFILE=0,
     +                             ACTIVE_SO2_Scrubber_OVERLAY=0
      END MODULE COAL_cnw_FILES_READ_INPUT
! ***********************************************************************
      module CoalModelOutputsFromCNW
      integer (kind=4), save :: MaxIndexSDRnOfLk
      integer (kind=2), allocatable, save :: SDRnOfLk(:,:)
      real (kind=4), allocatable, save ::
     +  MineQAMTcost(:,:),
     +  BasnDmndQty (:,:),
     +  DmndAvgFobCst(:),
     +  DmndAvgTrnCst(:),
     +  StoD_QyAfMfAdMdTrEmCst(:,:,:), ! (kT) and ($/T)
     +  DmndAvgEmsCst(:),
     +  fUnscNode (:,:)
      character (len=50), allocatable, save :: DNodName(:) ! MSG
      character (len=50), allocatable, save :: SNodName(:) ! MSG
      integer (kind=2), allocatable, save :: GenericUnitPos(:)
         REAL (KIND=4), ALLOCATABLE :: SO2BlendedRate(:)
      end module CoalModelOutputsFromCNW
! ***********************************************************************
      MODULE CoalModelVectorFileInterface
        interface SO2_ESCALATIONS
          module procedure SO2_ESCALATIONS_RATES_VALUES,
     +                     SO2_ESCALATIONS_VALUES_ONLY
        end interface SO2_ESCALATIONS
        contains
! ***********************************************************************
         SUBROUTINE SO2_ESCALATIONS_RATES_VALUES(EscalatedValue,
     +                                           EscalationRate)
! ***********************************************************************
            REAL (KIND=4), INTENT(IN OUT) :: EscalatedValue(0:30)
            REAL (KIND=4), INTENT(IN OUT) :: EscalationRate
            REAL (KIND=4) :: Vector_Values(1:30)
            INTEGER (KIND=2) :: iYr
            INTEGER (KIND=4) :: VectorNum
            LOGICAL (KIND=4) :: RETURN_COAL_VECTOR
!
            EscalatedValue(1:30) = EscalatedValue(0)
            IF(EscalationRate > 0.) THEN
               EscalationRate = 1. + EscalationRate/100.
               DO iYr = 1, 30
                  EscalatedValue(iYr) = EscalationRate *
     +                                             EscalatedValue(iYr-1)
               ENDDO
            ELSEIF(EscalationRate < 0.) THEN
               VectorNum = INT4(ABS(EscalationRate))
               IF(RETURN_COAL_VECTOR(Vector_Values,VectorNum)) THEN
                  EscalatedValue(1:30) = Vector_Values(1:30)
               ELSE
                  DO iYr = 1, 30
                     EscalatedValue(iYr)= (1.+Vector_Values(iYr)/100.) *
     +                                             EscalatedValue(iYr-1)
                  ENDDO
               ENDIF
            ENDIF
         RETURN
         END SUBROUTINE
! ***********************************************************************
         SUBROUTINE SO2_ESCALATIONS_VALUES_ONLY(EscalatedValue)
! ***********************************************************************
            REAL (KIND=4), INTENT(IN OUT) :: EscalatedValue(0:30)
            REAL (KIND=4) :: Vector_Values(1:30)
            INTEGER (KIND=4) :: VectorNum
            LOGICAL (KIND=4) :: RETURN_COAL_VECTOR
!
            EscalatedValue(1:30) = EscalatedValue(0)
            IF(EscalatedValue(0) < 0.) THEN
               VectorNum = int4(ABS(EscalatedValue(0)))
               IF(RETURN_COAL_VECTOR(Vector_Values,VectorNum)) THEN
                  EscalatedValue(1:30) = Vector_Values(1:30)
               ENDIF
            ENDIF
            RETURN
         END SUBROUTINE
      END MODULE CoalModelVectorFileInterface
! ***********************************************************************
      MODULE CONVERT_TO_ANNUAL_VALUES
         INTERFACE FILL_ANNUAL_ARRAYS
            MODULE PROCEDURE VALUE_ARRAY_POINTER,
     +                       VALUE_ESCAL_ARRAY_POINTER,
     +                       VALUE_ARRAY_POINTER_DOUBLE
         END INTERFACE
      CONTAINS
!+++++
       FUNCTION VALUE_ARRAY_POINTER(PointerValue) 
     +                                               RESULT(AnnualArray)
         REAL (KIND=4), INTENT(IN) :: PointerValue 
         REAL (KIND=4) :: AnnualArray(0:30),VectorValues(1:30)
         LOGICAL (KIND=4) :: RETURN_COAL_VECTOR
         INTEGER (KIND=4) :: VectorNum
            IF(PointerValue >= 0.) THEN
               AnnualArray(0:30) = PointerValue 
            ELSE
               VectorNum = ABS(PointerValue)
               IF(RETURN_COAL_VECTOR(VectorValues,VectorNum))THEN
                  AnnualArray(0) = PointerValue 
                  AnnualArray(1:30) = VectorValues(1:30)
               ENDIF
            ENDIF
         END FUNCTION
!+++++
       FUNCTION VALUE_ARRAY_POINTER_DOUBLE(PointerValue) 
     +                                               RESULT(AnnualArray)
         REAL (KIND=8), INTENT(IN) :: PointerValue 
         REAL (KIND=8) :: AnnualArray(0:30)
         REAL (KIND=4) :: VectorValues(1:30)
         LOGICAL (KIND=4) :: RETURN_COAL_VECTOR
         INTEGER (KIND=4) :: VectorNum
            IF(PointerValue >= 0.) THEN
               AnnualArray(0:30) = PointerValue 
            ELSE
               VectorNum = ABS(PointerValue)
               IF(RETURN_COAL_VECTOR(VectorValues,VectorNum))THEN
                  AnnualArray(0) = PointerValue 
                  AnnualArray(1:30) = VectorValues(1:30)
               ENDIF
            ENDIF
         END FUNCTION
!+++++
         FUNCTION VALUE_ESCAL_ARRAY_POINTER(ValueToEscalate,
     +                                      PointerValue) 
     +                                               RESULT(AnnualArray)
         REAL (KIND=4), INTENT(IN) :: PointerValue, ValueToEscalate 
         REAL (KIND=4) :: AnnualArray(0:30),VectorValues(1:30)
         REAL (KIND=4) :: EscalRate
         INTEGER (KIND=4) :: VectorNum
         INTEGER (KIND=2) :: Yr
         LOGICAL (KIND=4) :: RETURN_COAL_VECTOR
            IF(PointerValue >= 0.) THEN
               EscalRate = 1. + PointerValue/100.
               AnnualArray(0) = ValueToEscalate
               DO Yr = 1, 30
                  AnnualArray(Yr) = AnnualArray(Yr-1) * EscalRate
               ENDDO 
            ELSE
               VectorNum = ABS(PointerValue)
               IF(RETURN_COAL_VECTOR(VectorValues,VectorNum))THEN
                  AnnualArray(0) = PointerValue 
                  AnnualArray(1:30) = VectorValues(1:30)
               ELSE
                  AnnualArray(0) = ValueToEscalate
                  DO Yr = 1, 30
                     AnnualArray(Yr) = AnnualArray(Yr-1) * 
     +                                            VectorValues(Yr)
                  ENDDO 
               ENDIF
            ENDIF
         END FUNCTION
      END MODULE CONVERT_TO_ANNUAL_VALUES
!***********************************************************************
      MODULE ArrayAllocationInterface
        interface ! ROUTE_ESCALATIONS
         SUBROUTINE ROUTE_ESCALATIONS(iArray,EscalRateVector,
     +                                ResultByYr)
         REAL (KIND=4) :: ResultByYr(:,0:),EscalRateVector
         INTEGER (KIND=2) :: iArray
         END SUBROUTINE
        end interface ! ROUTE_ESCALATIONS
        interface AllocateArray
! this list must match the names of subroutines below
          module procedure
     +      AllArrayL1MinMaxD1,                     ! logical*1
     +      AllArrayL4,AllArrayL4M4,AllArrayL4MinMaxD1,     ! logical*4
     +      AllArrayI1,AllD2ArrayI1,                ! integer*1
     +      AllArrayI2,AllArrayI2I4,AllI2LowBUpBR8, ! integer*2
     +      AllArrayI4,AllArrayI4I4,AllI4LowBUpBR8, ! integer*4
     +      AllArrayI8,                             ! integer*8
     +      AllArrayR4,AllArrayR4I4,AllD2ArrayI2,   ! real*4
     +      AllD1LowBUpBR4,AllD2LowBUpBR4,AllD3LowBUpBR4,
     +      AllArrayR8,AllLowBUpBR8,                ! real*8
     +      AllArrayChrI2,AllArrayChrI4,             ! character*(*)
     +      AllL4LB1UB1LB2UB2                       ! logical*4 2 dims
        end interface AllocateArray
        contains
c-----
!     logical*1 arrays
         subroutine AllArrayL1MinMaxD1(AllArray,LowB1,UpB1) ! AGT: inconsistent name here
         logical (kind=1), allocatable :: AllArray(:)
         integer (kind=4) :: LowB1,UpB1
            if(allocated(AllArray)) then
               if(LBOUND(AllArray,1) /= LowB1 .OR.
     +                                  UBOUND(AllArray,1) /= UpB1) then
                  deallocate(AllArray)
                  allocate(AllArray(LowB1:UpB1))
               endif
            else
               allocate(AllArray(LowB1:UpB1))
            endif
         end subroutine
!     logical*4 arrays
         subroutine AllArrayL4(AllArray,UpB1)
         logical (kind=4), allocatable :: AllArray(:)
         integer (kind=2) :: UpB1
            if(allocated(AllArray)) then
               if(LBOUND(AllArray,1) /= 1 .OR.
     +                                  UBOUND(AllArray,1) /= UpB1) then
                  deallocate(AllArray)
                  allocate(AllArray(UpB1))
               endif
            else
               allocate(AllArray(UpB1))
            endif
         end subroutine
         subroutine AllArrayL4M4(AllArray,UpB1)
         logical (kind=4), allocatable :: AllArray(:)
         integer (kind=4) :: UpB1
            if(allocated(AllArray)) then
               if(LBOUND(AllArray,1) /= 1 .OR.
     +                                  UBOUND(AllArray,1) /= UpB1) then
                  deallocate(AllArray)
                  allocate(AllArray(UpB1))
               endif
            else
               allocate(AllArray(UpB1))
            endif
         end subroutine
         subroutine AllArrayL4MinMaxD1(AllArray,LowB1,UpB1) ! AGT: inconsistent name here
         logical (kind=4), allocatable :: AllArray(:)
         integer (kind=4) :: LowB1,UpB1
            if(allocated(AllArray)) then
               if(LBOUND(AllArray,1) /= LowB1 .OR.
     +                                  UBOUND(AllArray,1) /= UpB1) then
                  deallocate(AllArray)
                  allocate(AllArray(LowB1:UpB1))
               endif
            else
               allocate(AllArray(LowB1:UpB1))
            endif
         end subroutine
!     integer*1 arrays
         subroutine AllArrayI1(AllArray,UpB1)
         integer (kind=1), allocatable :: AllArray(:)
         integer (kind=4) :: UpB1
            if(allocated(AllArray)) then
               if(LBOUND(AllArray,1) /= 1 .OR.
     +                                  UBOUND(AllArray,1) /= UpB1) then
                  deallocate(AllArray)
                  allocate(AllArray(UpB1))
               endif
            else
               allocate(AllArray(UpB1))
            endif
         end subroutine
         subroutine AllD2ArrayI1(AllArray,UpB1,UpB2)
         integer (kind=1), allocatable :: AllArray(:,:)
         integer (kind=2) :: UpB1,UpB2
            if(allocated(AllArray)) then
               if(LBOUND(AllArray,1) /= 1 .OR.
     +                                  UBOUND(AllArray,1) /= UpB1 .OR.
     +            LBOUND(AllArray,2) /= 1 .OR.
     +                                  UBOUND(AllArray,2) /= UpB2) then
                  deallocate(AllArray)
                  allocate(AllArray(UpB1,UpB2))
               endif
            else
               allocate(AllArray(UpB1,UpB2))
            endif
         end subroutine
!     integer*2 arrays
         subroutine AllArrayI2(AllArray,UpB1)
         integer (kind=2), allocatable :: AllArray(:)
         integer (kind=2) :: UpB1
            if(allocated(AllArray)) then
               if(LBOUND(AllArray,1) /= 1 .OR.
     +                                  UBOUND(AllArray,1) /= UpB1) then
                  deallocate(AllArray)
                  allocate(AllArray(UpB1))
               endif
            else
               allocate(AllArray(UpB1))
            endif
         end subroutine
         subroutine AllArrayI2I4(AllArray,UpB1)
         integer (kind=2), allocatable :: AllArray(:)
         integer (kind=4) :: UpB1
            if(allocated(AllArray)) then
               if(LBOUND(AllArray,1) /= 1 .OR.
     +                                  UBOUND(AllArray,1) /= UpB1) then
                  deallocate(AllArray)
                  allocate(AllArray(UpB1))
               endif
            else
               allocate(AllArray(UpB1))
            endif
         end subroutine
         subroutine AllI2LowBUpBR8(AllArray,LowB1,UpB1,LowB2,UpB2) ! AGT: R8 misleading here
         integer (kind=2), allocatable :: AllArray(:,:)
         integer (kind=4) :: LowB1,UpB1,LowB2,UpB2
            if(allocated(AllArray)) then
               if(LBOUND(AllArray,1) /= LowB1  .OR.
     +                                  UBOUND(AllArray,1) /= UpB1 .OR.
     +            LBOUND(AllArray,2) /= LowB2 .OR.
     +                                  UBOUND(AllArray,2) /= UpB2) then
                  deallocate(AllArray)
                  allocate(AllArray(LowB1:UpB1,LowB2:UpB2))
               endif
            else
               allocate(AllArray(LowB1:UpB1,LowB2:UpB2))
            endif
         end subroutine
!     integer*4 arrays
         subroutine AllArrayI4(AllArray,UpB1)
         integer (kind=4), allocatable :: AllArray(:)
         integer (kind=2) :: UpB1
            if(allocated(AllArray)) then
               if(LBOUND(AllArray,1) /= 1 .OR.
     +                                  UBOUND(AllArray,1) /= UpB1) then
                  deallocate(AllArray)
                  allocate(AllArray(UpB1))
               endif
            else
               allocate(AllArray(UpB1))
            endif
         end subroutine
         subroutine AllArrayI4I4(AllArray,UpB1)
         integer (kind=4), allocatable :: AllArray(:)
         integer (kind=4) :: UpB1
            if(allocated(AllArray)) then
               if(LBOUND(AllArray,1) /= 1 .OR.
     +                                  UBOUND(AllArray,1) /= UpB1) then
                  deallocate(AllArray)
                  allocate(AllArray(UpB1))
               endif
            else
               allocate(AllArray(UpB1))
            endif
         end subroutine
         subroutine AllI4LowBUpBR8(AllArray,LowB1,UpB1,LowB2,UpB2) ! AGT: R8 misleading here
         integer (kind=4), allocatable :: AllArray(:,:)
         integer (kind=4) :: LowB1,UpB1,LowB2,UpB2
            if(allocated(AllArray)) then
               if(LBOUND(AllArray,1) /= LowB1  .OR.
     +                                  UBOUND(AllArray,1) /= UpB1 .OR.
     +            LBOUND(AllArray,2) /= LowB2 .OR.
     +                                  UBOUND(AllArray,2) /= UpB2) then
                  deallocate(AllArray)
                  allocate(AllArray(LowB1:UpB1,LowB2:UpB2))
               endif
            else
               allocate(AllArray(LowB1:UpB1,LowB2:UpB2))
            endif
         end subroutine
!     integer*8 arrays
         subroutine AllArrayI8(AllArray,UpB1)
         integer (kind=8), allocatable :: AllArray(:)
         integer (kind=2) :: UpB1
            if(allocated(AllArray)) then
               if(LBOUND(AllArray,1) /= 1 .OR.
     +                                  UBOUND(AllArray,1) /= UpB1) then
                  deallocate(AllArray)
                  allocate(AllArray(UpB1))
               endif
            else
               allocate(AllArray(UpB1))
            endif
         end subroutine
!     real*4 arrays
         subroutine AllArrayR4(AllArray,UpB1)
         real (kind=4), allocatable :: AllArray(:)
         integer (kind=2) :: UpB1
            if(allocated(AllArray)) then
               if(LBOUND(AllArray,1) /= 1 .OR.
     +                                  UBOUND(AllArray,1) /= UpB1) then
                  deallocate(AllArray)
                  allocate(AllArray(UpB1))
               endif
            else
               allocate(AllArray(UpB1))
            endif
         end subroutine
         subroutine AllArrayR4I4(AllArray,UpB1)
         real (kind=4), allocatable :: AllArray(:)
         integer (kind=4) :: UpB1
            if(allocated(AllArray)) then
               if(LBOUND(AllArray,1) /= 1 .OR.
     +                                  UBOUND(AllArray,1) /= UpB1) then
                  deallocate(AllArray)
                  allocate(AllArray(UpB1))
               endif
            else
               allocate(AllArray(UpB1))
            endif
         end subroutine
         subroutine AllD2ArrayI2(AllArray,UpB1,UpB2) ! AGT: suggests I2( to R4I2(
         real (kind=4), allocatable :: AllArray(:,:)
         integer (kind=2) :: UpB1,UpB2
            if(allocated(AllArray)) then
               if(LBOUND(AllArray,1) /= 1  .OR.
     +                                  UBOUND(AllArray,1) /= UpB1 .OR.
     +            LBOUND(AllArray,2) /= 1 .OR.
     +                                  UBOUND(AllArray,2) /= UpB2) then
                  deallocate(AllArray)
                  allocate(AllArray(UpB1,UpB2))
               endif
            else
               allocate(AllArray(UpB1,UpB2))
            endif
         end subroutine
         subroutine AllD1LowBUpBR4(AllArray,LowB1,UpB1)
         real (kind=4), allocatable :: AllArray(:)
         integer (kind=4) :: LowB1,UpB1
            if(allocated(AllArray)) then
               if(LBOUND(AllArray,1) /= LowB1 .OR.
     +                                  UBOUND(AllArray,1) /= UpB1) then
                  deallocate(AllArray)
                  allocate(AllArray(LowB1:UpB1))
               endif
            else
               allocate(AllArray(LowB1:UpB1))
            endif
         end subroutine
         subroutine AllD2LowBUpBR4(AllArray,LowB1,UpB1,LowB2,UpB2)
         real (kind=4), allocatable :: AllArray(:,:)
         integer (kind=4) :: LowB1,UpB1,LowB2,UpB2
            if(allocated(AllArray)) then
               if(LBOUND(AllArray,1) /= LowB1  .OR.
     +                                  UBOUND(AllArray,1) /= UpB1 .OR.
     +            LBOUND(AllArray,2) /= LowB2 .OR.
     +                                  UBOUND(AllArray,2) /= UpB2) then
                  deallocate(AllArray)
                  allocate(AllArray(LowB1:UpB1,LowB2:UpB2))
               endif
            else
               allocate(AllArray(LowB1:UpB1,LowB2:UpB2))
            endif
         end subroutine
         subroutine AllD3LowBUpBR4(AllArray,
     +                                 LowB1,UpB1,LowB2,UpB2,LowB3,UpB3)
         real (kind=4), allocatable :: AllArray(:,:,:)
         integer (kind=4) :: LowB1,UpB1,LowB2,UpB2,LowB3,UpB3
            if(allocated(AllArray)) then
               if(LBOUND(AllArray,1) /= LowB1  .OR.
     +                                  UBOUND(AllArray,1) /= UpB1 .OR.
     +            LBOUND(AllArray,2) /= LowB2 .OR.
     +                                  UBOUND(AllArray,2) /= UpB2 .OR.
     +            LBOUND(AllArray,3) /= LowB3 .OR.
     +                                  UBOUND(AllArray,3) /= UpB3) then
                  deallocate(AllArray)
                  allocate(AllArray(LowB1:UpB1,LowB2:UpB2,LowB3:UpB3))
               endif
            else
               allocate(AllArray(LowB1:UpB1,LowB2:UpB2,LowB3:UpB3))
            endif
         end subroutine
!     real*8 arrays
         subroutine AllArrayR8(AllArray,UpB1)
         real (kind=8), allocatable :: AllArray(:)
         integer (kind=2) :: UpB1
            if(allocated(AllArray)) then
               if(LBOUND(AllArray,1) /= 1 .OR.
     +                                  UBOUND(AllArray,1) /= UpB1) then
                  deallocate(AllArray)
                  allocate(AllArray(UpB1))
               endif
            else
               allocate(AllArray(UpB1))
            endif
         end subroutine
         subroutine AllLowBUpBR8(AllArray,LowB1,UpB1,LowB2,UpB2)
         real (kind=8), allocatable :: AllArray(:,:)
         integer (kind=4) :: LowB1,UpB1,LowB2,UpB2
            if(allocated(AllArray)) then
               if(LBOUND(AllArray,1) /= LowB1  .OR.
     +                                  UBOUND(AllArray,1) /= UpB1 .OR.
     +            LBOUND(AllArray,2) /= LowB2 .OR.
     +                                  UBOUND(AllArray,2) /= UpB2) then
                  deallocate(AllArray)
                  allocate(AllArray(LowB1:UpB1,LowB2:UpB2))
               endif
            else
               allocate(AllArray(LowB1:UpB1,LowB2:UpB2))
            endif
         end subroutine
!     character arrays
         subroutine AllArrayChrI2(AllArray,UpB1)
         Allocatable  AllArray(:)
         character*(*)  AllArray
         integer (kind=2) :: UpB1
            if(allocated(AllArray)) then
               if(LBOUND(AllArray,1) /= 1 .OR.
     +                                  UBOUND(AllArray,1) /= UpB1) then
                  deallocate(AllArray)
                  allocate(AllArray(UpB1))
               endif
            else
               allocate(AllArray(UpB1))
            endif
         end subroutine
         subroutine AllArrayChrI4(AllArray,UpB1)
         Allocatable  AllArray(:)
         character*(*)  AllArray
         integer (kind=4) :: UpB1
            if(allocated(AllArray)) then
               if(LBOUND(AllArray,1) /= 1 .OR.
     +                                  UBOUND(AllArray,1) /= UpB1) then
                  deallocate(AllArray)
                  allocate(AllArray(UpB1))
               endif
            else
               allocate(AllArray(UpB1))
            endif
         end subroutine
         subroutine AllL4LB1UB1LB2UB2(AllArray,LowB1,UpB1,LowB2,UpB2) 
         logical (kind=4), allocatable :: AllArray(:,:)
         integer (kind=4) :: LowB1,UpB1,LowB2,UpB2
            if(allocated(AllArray)) then
               if(LBOUND(AllArray,1) /= LowB1  .OR.
     +                                  UBOUND(AllArray,1) /= UpB1 .OR.
     +            LBOUND(AllArray,2) /= LowB2 .OR.
     +                                  UBOUND(AllArray,2) /= UpB2) then
                  deallocate(AllArray)
                  allocate(AllArray(LowB1:UpB1,LowB2:UpB2))
               endif
            else
               allocate(AllArray(LowB1:UpB1,LowB2:UpB2))
            endif
         end subroutine
      END MODULE ArrayAllocationInterface
      MODULE COAL_SUPPLY_INPUTS
          CHARACTER (LEN=50) :: Supply Description=" "
         INTEGER (KIND=2) ::  Basin ID=0,Fuel Type ID=0,
     +                        Supply Curve Points=0
         REAL (KIND=4) :: Heat Content=0,SO2 Rate=0,CO2 Rate=0,
     +                    Quantity Consumed Base Year=0,
     +                    Minimum Annual Production=0,
     +                    Maximum Annual Production=0,
     +                    Supply Production(20)=20*0,
     +                    Supply Cost(20)=20*0,
     +                    Production Escalation=0,
     +                    Production Cost Escalation=0,
     +                    Last Supply Cost=0                  
         CHARACTER (LEN=2), PARAMETER :: DATA_FILE_CODE="HS"
         CHARACTER (LEN=17), PARAMETER :: FILE_TYPE='Coal Supply  '
      END MODULE COAL_SUPPLY_INPUTS
      MODULE COAL_BASIN_MAKEBIN_INPUTS
         CHARACTER (LEN=50) :: Basin Name=" "
         CHARACTER (LEN=27) :: Basin Abbrev=" "
         INTEGER (KIND=4) ::  Basin HI ID=0
         REAL (KIND=4) :: Annual Base Yr Quantity=0,
     +                    Minimum Annual Quantity=0,
     +                    Maximum Annual Quantity=0
!           Basin Abbrev                 BasnName(iBasn)
!     +     Basin Active
!     +     Basin Name
!     +     Basin HI ID                  BasnID(iBasn)
!     +     Annual Base Yr Quantity
!     +     Minimum Annual Quantity      BasnQtyLB(iBasn)
!     +     Maximum Annual Quantity      BasnQtyUB(iBasn)
         CHARACTER (LEN=17), PARAMETER :: FILE_TYPE='Coal Basin  '
         CHARACTER (LEN=2), PARAMETER :: DATA_FILE_CODE="HN"
      END MODULE COAL_BASIN_MAKEBIN_INPUTS
      MODULE COAL_TRANSPORT_MAKE_INPUTS
         USE COAL_cnw_FILES_READ_INPUT
         CHARACTER (LEN=50) :: Route Name=" ",Basin to=" ",Plant=" ",
     +                         Transport Vehicles
         INTEGER (KIND=4) ::  Link ID=0,Basin HI ID=0,Plant EV ID=0
         REAL (KIND=4) :: Transport Price=0,
     +                    Transport Price Escalation=0,
     +                    Annual Quantity Limit=0,
     +                    Transport Quantity Escalaation=0
         CHARACTER (LEN=17), PARAMETER :: FILE_TYPE='Coal Transport'
         CHARACTER (LEN=2), PARAMETER :: DATA_FILE_CODE="HT"
      END MODULE COAL_TRANSPORT_MAKE_INPUTS
!           Route Name
!     +     Route Active              
!     +     Link ID                   
!     +     Basin HI ID               
!     +     Plant HI ID               
!     +     Transport Price           
!     +     Transport Price Escalation
!     +     Annual Quantity Limit
      MODULE  PLANT_DEMAND_MAKEBIN_INPUTS
         CHARACTER (LEN=50) :: Plant Name=" ",Unit Name=" "
         CHARACTER (LEN=2) :: State_Location=" "
         INTEGER (KIND=4) ::  EV Plant ID=0
         INTEGER (KIND=8) ::  EV Unit ID=0
         REAL (KIND=4) :: Heat Lower Bound=0,
     +                    Heat Upper Bound=0,
     +                    Fraction Unscrubbed=0,
     +                    Heat Content Base Year=0,
     +                    Percent Increase=0,
     +                    Percent Decrease=0,
     +                    Unit_Capacity=0,
     +                    Average_Heatrate=0,
     +                    SOx_Control_Percent=0,
     +                    SOx_Control_Date =0,
     +                    SOx_Control_Var_Cost=0,
     +                    SOx_Control_Fixed_Cost=0,
     +                    SO2 Base Year=0,
     +                    SO2 Percent Increase=0,
     +                    SO2 Percent Decrease=0,
     +                    SO2 Lower Bound=0,
     +                    SO2 Upper Bound=0,
     +                    SOx_Control_Capital_Cost=0,
     +                    SOx_Control_Carrying_Charge=0,
     +                    Escalation_SOx_Capital_Cost=0,    
     +                    Escalation_SOx_Carrying_Charge=0, 
     +                    Escalation_SOx_Var_Cost=0,        
     +                    Escalation_SOx_Fixed_Cost=0
      REAL (KIND=8) :: Annual Demand=0
      INTEGER (KIND=2) :: OnLineMo,OnLineYr,OffLineMo,OffLineYr,
     +                    Trans_Group_ID
         CHARACTER (LEN=30) :: Trans_Group_Name
         CHARACTER (LEN=2), PARAMETER :: DATA_FILE_CODE="HD"
         CHARACTER (LEN=17), PARAMETER :: FILE_TYPE='Coal Demand'
       END MODULE PLANT_DEMAND_MAKEBIN_INPUTS
      MODULE  GENERIC_DEMAND_MAKEBIN_INPUTS
         CHARACTER (LEN=50) :: Unit Name=" "
         CHARACTER (LEN=2) :: State_Location=" "
         REAL (KIND=4) :: Heat Lower Bound=0,
     +                    Heat Upper Bound=0,
     +                    Fraction Unscrubbed=0,
     +                    Heat Content=0,
     +                    Percent Increase=0,
     +                    Percent Decrease=0,
     +                    Unit_Capacity=0,
     +                    Average_Heatrate=0,
     +                    SO2 Emissions=0,
     +                    SO2 Percent Increase=0,
     +                    SO2 Percent Decrease=0,
     +                    SO2 Lower Bound=0,
     +                    SO2 Upper Bound=0
      REAL (KIND=8) :: Annual Demand=0
      INTEGER (KIND=2) :: Trans_Group_ID
         CHARACTER (LEN=30) :: Trans_Group_Name
         CHARACTER (LEN=2), PARAMETER :: DATA_FILE_CODE="ZE"
         CHARACTER (LEN=17),PARAMETER :: FILE_TYPE='Generic Coal Demand'
       END MODULE GENERIC_DEMAND_MAKEBIN_INPUTS
      MODULE COAL_CONTRACTS_MAKEBIN_INPUTS
         CHARACTER (LEN=50) :: Contract Name=" ",Mine Name=" ",
     +                         Mine Basin Name=" ",Coal Type=" "
         INTEGER (KIND=4) ::  Contract ID=0,Mine ID=0,
     +                        Nth Contract=0
         INTEGER (KIND=4) ::  Plant ID=0,BasinID=0 
     +                                  
         CHARACTER (LEN=15) :: ContractType=" "
         REAL (KIND=4) :: Annual Quantity(30)=30*0,
     +                    Annual Price(30)=30*0
         CHARACTER (LEN=2), PARAMETER :: DATA_FILE_CODE="FT"
         CHARACTER (LEN=17), PARAMETER :: FILE_TYPE='Coal Contracts'
!     +    Contract Name
!     +    Contract ID
!     +    Contract Active
!     +    Mine Name
!     +    Mine ID
!     +    Mine Basin Name
!     +    Coal Type          ContractCoalName(iCont)
!     +    Plant ID           GPntID(iCont)
!     +    Nth Contract
!     +    Annual Quantity    qInYr(iCont,30)
!     +    Annual Price       pInYr(iCont,30)
!     +    COMMENT
      END MODULE COAL_CONTRACTS_MAKEBIN_INPUTS
      MODULE COAL_SO2_INFO_MAKEBIN_INPUTS
         CHARACTER (LEN=20) :: Input Type=" "
         CHARACTER (LEN=2) :: Sulphur Market Area=" "
         CHARACTER (LEN=2), PARAMETER :: DATA_FILE_CODE="ZC"
         CHARACTER (LEN=17), PARAMETER :: FILE_TYPE='Coal SO2 Info'
         REAL (KIND=4) :: Sulphur Credit Cost,
     +                    Sulphur Credit Cost Escalation,
     +                    Sulphur Emissions Limit,
     +                    Scrubber Capital Cost,
     +                    Capital Cost Escalation,
     +                    Scrubber Fixed OM Cost,
     +                    Fixed OM Escalation,
     +                    Scrubber Variable OM Cost,
     +                    Variable OM Escalation,
     +                    StateCapVolatility 
         INTEGER (KIND=4) :: SOxGroupAssignment
      END MODULE COAL_SO2_INFO_MAKEBIN_INPUTS
      MODULE SO2_Scrubber_COAL_MODEL_DATA
         INTEGER (KIND=2), SAVE :: nStateSO2Limits
         INTEGER (KIND=4), PARAMETER :: nStates=52
         REAL (KIND=4), SAVE :: SulphurCreditCost(0:30,0:nStates),
     +                         SulphurCreditCostEscalation(0:nStates),
     +                         SulphurEmissionsLimit(0:30,0:nStates),
     +                         ScrubberCapitalCost(0:30,0:nStates),
     +                         CapitalCostEscalation(0:nStates),
     +                         ScrubberFixedOMCost(0:30,0:nStates),
     +                         FixedOMEscalation(0:nStates),
     +                         ScrubberVariableOMCost(0:30,0:nStates),
     +                         VariableOMEscalation(0:nStates),
     +                         StateCapVolatility ByYear(0:30,0:nStates)
         CHARACTER (LEN=20), SAVE :: SO2InputType(0:nStates)
         CHARACTER (LEN=2), SAVE :: SulphurMarketArea(0:nStates)
         LOGICAL (KIND=1), SAVE :: StateHasLimits(0:nStates)
         INTEGER (KIND=4), SAVE :: StateAssignedToGroup(0:nStates)
      END MODULE SO2_Scrubber_COAL_MODEL_DATA
