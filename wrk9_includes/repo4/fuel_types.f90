module mod_fuel_types
use miscmod
use end_routine

implicit none
integer, parameter :: ft_gas=2

contains


function FuelTypeToName(FT)
    character(len=128) :: result, FuelTypeToName
    integer :: FT

    result="UNKNOWN";
    select case(FT)
!        case(FT_COAL)
!            result="Coal"
        case(FT_GAS)
            result="Gas"
!        case(FT_OIL)
!            result="Oil"
!        case(FT_NUCLEAR)
!            result="Nuclear"
!        case(FT_WATER)
!            result="Water"
!        case(FT_OTHER)
!            result="Other"
!        case(FT_WHOLESALE_SALES)
!            result="Wholesale Sales"
!        case(FT_WHOLESALE_PURCHASES)
!            result="Wholesale Purchases"
        case DEFAULT
            er_message="fuel_types:0001: Unknown fuel type of " // trim(itos(FT)) // " in FuelTypeToName routine."
            call end_program(er_message)

    end select
    
    
    
    FuelTypeToName=result
end function FuelTypeToName

function NameToFuelType(FTN)
character(len=1) :: Upcase
character(len=128) :: FTN
integer :: result, NameToFuelType

    result=-1
    ftn=trim(upcase(ftn))

    select case(FTN)
!        case("COAL")
!            result=FT_COAL
        case("GAS")
            result=FT_GAS
!        case("OIL")
!            result=FT_OIL
!        case("NUCLEAR")
!            result=FT_NUCLEAR
!        case("WATER")
!            result=FT_WATER
!        case("OTHER")
!            result=FT_OTHER
!        case("WHOLESALE SALES")
!            result=FT_Wholesale_Sales
!        case("WHOLESALE Purchases")
!            result=FT_Wholesale_Purchases
        case DEFAULT
            er_message="fuel_types:0002: Fuel type of " // trim(FTN) // " unknown or unrecognized."
            call end_program(er_message)

    end select
    NameToFuelType=result
    
end function NameToFuelType


!    COL    Coal
!    DFO    Distillate fuel oil
!    DSM    Demand-side management
!    GEO    Geotherm
!    LFG    Landfill gas
!    LIG    Lignite
!    MWH    Batt
!    NA     Not applicable
!    NG     Natural Gas
!    NUC    Uranium
!    OBG    Biomass gas
!    OBS    Biomass solids
!    OG     Other Gas
!    OTH    Other
!    PC     Pulverized Coal
!    PUR    Purchase
!    PV     Photovoltaics
!    REF    Refuse
!    SUB    Sub-bituminous coal
!    SUN    Solar
!    TDF    Tires
!    URA    Uranium
!    WDS    Wood something
!    WND    Wind
end module mod_fuel_types
