module capacity_arrays
use abb_capmarketrptdata
implicit none

! *** WARNING ***
! max_prod_types might be up to five larger for tf_objt2.for.
! See DAILY_OPTION_OBJECT function for possibly-affected code. (That function 
! is not currently called from anywhere, however.)
! Issue with max_prod_types:
! max_prod_types allocations:
!  19 in clreport; 18 in MARGNOBJ; 24 in tf_objt2
! .

! max_prod_types (clr) went to 23 from 19. 19 was
! the last increment added for DG, so 
! 20-23 are OW, HB, H2, CS
! 1=COAL; 2=GAS ; 3=OIL; 4=NUCLEAR; 5=WATER; 6=OTHER 
    integer (kind=2), parameter :: NUMPMS=24
    integer (kind=2), parameter :: max_prod_types_clr=24 !clreport
    integer, parameter :: MAX_PROD_TYPES_mo=19 ! margnobl
    integer (kind=2), parameter :: max_prod_types_tfo=24 ! tf_objt
    integer (kind=2), parameter :: max_prod_types_pd=18 ! Picadisp
    integer (kind=2), parameter :: MAX_FUEL_TYPES_pd=6 ! Picadisp
    integer (kind=2), parameter :: max_fuel_types_clr=6
	
	 ! 080722. ADDED PM'S
	 ! 030124, added PMs, jtr
    real (kind=4) :: RPS_THERMAL_DB(2,400,NumPMs,0:12)
    real (kind=4) :: SYSTEM_PROD_BY_TG_BY_MWH
    real, allocatable :: LOAD_BY_TG_BY_MWH(:,:)
    real (kind=4), allocatable :: QUALIFYING_GEN_DB(:,:,:)
    real (kind=4), allocatable :: DEV_TG_CAP(:,:,:)
    real (kind=4), allocatable ::   DEV_NEW_TG_CAP(:,:,:)
    real (kind=4), allocatable ::  SAVED_DEV_NEW_TG_CAP(:,:,:)
    real (kind=4), allocatable :: RPS_HYDRO_DB(:,:,:,:)
    real (kind=4), allocatable ::  TG_CapacitySupplyObligation(:,:)
    real (kind=4), allocatable :: GRX_QUAL_GEN_DB(:,:,:), &
         SAVE_GRX_QUAL_GEN_DB(:,:,:)

    real (kind=4) :: TG_ResourceActualPreference
    real (kind=4), allocatable :: TG_EffectiveCapacity(:,:)
    integer (kind=2) :: TRANS_PROD_NO=0
    integer :: TRANS_PROD_REC
    ! ACCUMULATE BY TRANSACTION GROUP AND PROD TYPE
    real, allocatable :: ANNUAL_LOAD_BY_TG_BY_MWH(:,:)

    real, allocatable ::  ANNUAL_PROD_BY_TG_BY_MW(:,:), & 
        PROD_BY_TG_BY_MWH(:,:), ANNUAL_PROD_BY_TG_BY_MWH(:,:), &
        PROD_BY_TG_BY_MW(:,:),  MW_BY_TG_BY_FUEL(:,:), &
        ! ACCUMULATE BY TRANSACTION GROUP AND FUEL TYPE
        ANNUAL_MW_BY_TG_BY_FUEL(:,:), MWH_BY_TG_BY_FUEL(:,:)
    real (kind=4) :: WH_MONTH_ENERGY
    integer (kind=2) :: T_I,T_J
    logical (kind=1) :: TRANSACT_PROD_REPORT_ACTIVE=.false.
    
    ! ACCUMULATE BY TRANSACTION GROUP AND FUEL TYPE
    real, allocatable :: ANNUAL_MWH_BY_TG_BY_FUEL(:,:) 
contains
subroutine allocate_rps_hydro_db()
	if(allocated(RPS_HYDRO_DB)) Then
		deallocate(RPS_HYDRO_DB)
	endif
         ! Todo:  Move rps_hydro_db to capacity_arrays module.
    ALLOCATE(RPS_HYDRO_DB(2,400,NUMPMS,0:12))
end subroutine allocate_rps_hydro_db
subroutine  allocate_grx_db_arrays(active_rps)
integer (kind=2), intent(in) :: active_rps
	if(allocated(QUALIFYING_GEN_DB)) then
		deallocate(QUALIFYING_GEN_DB)
	endif
	if(allocated(GRX_QUAL_GEN_DB)) then
		deallocate(GRX_QUAL_GEN_DB)
	endif
	if(allocated(SAVE_GRX_QUAL_GEN_DB)) then
		deallocate(SAVE_GRX_QUAL_GEN_DB)
	endif
	
    allocate(QUALIFYING_GEN_DB(ACTIVE_RPS,0:NUMPMS,2))
	allocate(GRX_QUAL_GEN_DB(ACTIVE_RPS,0:NUMPMS,2))
	allocate(SAVE_GRX_QUAL_GEN_DB(ACTIVE_RPS,0:NUMPMS,2))
	
end subroutine allocate_grx_db_arrays

subroutine allocate_dev_tg_cap(nr_trans_classes, STUDY_PERIOD)
integer, intent(in) :: nr_trans_classes, STUDY_PERIOD

     if (allocated(DEV_TG_CAP)) then
        deallocate(DEV_TG_CAP)
     endif
     ALLOCATE(DEV_TG_CAP(0:16,0:MAX(1,nr_trans_classes), &
       STUDY_PERIOD))
       
     if(allocated(DEV_NEW_TG_CAP)) then
        deallocate(DEV_NEW_TG_CAP)
     endif
     ALLOCATE (DEV_NEW_TG_CAP(0:21,0:MAX(1,nr_trans_classes), &
                                                  STUDY_PERIOD))
     if (allocated(SAVED_DEV_NEW_TG_CAP)) then
        deallocate(SAVED_DEV_NEW_TG_CAP)
     endif
     allocate(SAVED_DEV_NEW_TG_CAP(0:21,0:MAX(1, &
                     nr_trans_classes), STUDY_PERIOD)) 

end subroutine allocate_dev_tg_cap
end module capacity_arrays
