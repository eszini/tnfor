!     ******************************************************************
!     cla_ALLOCATION_rEDUCTION.FOR
!     Copyright(c)  2000
!
!     Created: 6/23/2010 6:59:27 PM
!     Author : MARK S GERBER
!     Last change: MSG 6/23/2010 6:59:27 PM
!     ******************************************************************

      REAL (KIND=4), ALLOCATABLE, SAVE ::
     +       RETIRE_RETRO_UPLIFT_PER_TON(:),
     +       RETIRE_RETRO_CUM_CO2(:),
     +       NUC_FUEL_LEASED_BURN_BY_CLASS(:),
     +       NUC_FUEL_OWNED_BURN_BY_CLASS(:),
     +       NUCLEAR_MWH_BY_CLASS(:),
     +       NUCLEAR_MMBTU_BY_CLASS(:),
     +       INTRA_COMPANY_NF_BURN(:),
     +       ANNUAL_CL_MAINT_MW(:),
     +       MN_RATE,MAINT_DAYS(:),
     +       MAINTENANCE_DAYS(:),
     +       MAINT_INDEX_MW(:),
     +       MAINT_INDEX_DAYS(:),
     +       ASSET_CLASS_LIST(:),
     +       ASSET_ALLOCATION_LIST(:),
     +       ANNUAL_CL_MAINTENANCE(:,:),
     +      MON_MDS_CL_UNIT_CAPACITY(:,:),
     +      MON_MDS_CL_UNIT_ENERGY(:,:),
     +      MON_MDS_CL_UNIT_FUEL_COST(:,:),
     +      MON_MDS_CL_UNIT_VAR_COST(:,:),
     +      MON_MDS_CL_UNIT_FIXED_COST(:,:),
     +      MON_MDS_NUC_FUEL_ADDER_COST(:,:),
     +      MON_MDS_ECO_SALES_REV_FROM(:,:),
     +      MON_MDS_ECO_SALES_ENRG_FROM(:,:),
     +      MON_MDS_ECO_PUCH_COST_FROM(:,:),
     +      MON_MDS_ECO_PUCH_ENRG_FROM(:,:),
     +      MON_MDS_ICAP_REVENUES(:,:)
     +      CL_ANN_CLASS_FUEL_COST(:,:),
     +     CL_ANN_MULT_FUEL_COST(:,:),
     +     FE_ANN_MULT_FUEL_COST(:,:),
     +     CL_ANN_MULT_VAR_COST(:,:),
     +     CL_ANN_MULT_FIXED_COST(:,:),
     +     CL_ANN_MULT_REVENUE(:,:),
     +     CL_ANN_MULT_CAPACITY(:,:),
     +     CL_ANN_MULT_PURCHASES(:,:),
     +     CL_ANN_MULT_ENERGY(:,:),
     +     NF_FUEL_LEASED_BY_CLASS(:,:),
     +     NF_FUEL_OWNED_BY_CLASS(:,:),
     +     CL_ANN_CLASS_VAR_COST(:,:),
     +     CL_ANN_CLASS_FIXED_COST(:,:),
     +     CL_ANN_CLASS_REVENUE(:,:),
     +     CL_ANN_CLASS_CAPACITY(:,:),
     +     CL_ANN_CLASS_PURCHASES(:,:),
     +     CL_ANN_CLASS_ENERGY(:,:),
     +     CL_ANN_CLASS_EMISSIONS(:,:),
     +     INTRA_COMPANY_REVENUES(:,:),
     +     MON_MDS_NUC_FUEL_LEASE_BURN_BC(:,:),
     +     MON_MDS_NUC_FUEL_OWNED_BURN_BC(:,:),
     +     MON_MDS_NUCLEAR_MWH_BY_CLASS(:,:),
     +     MON_MDS_NUCLEAR_MMBTU_BY_CLASS(:,:),
     +     MON_MDS_ICAP_REV_BY_CLASS(:,:),
     +     MONTHLY_AC_WHOLESALE_PROD_COST(:,:),
     +     MONTHLY_AC_ECITY_VAR_PROD_COST(:,:),
     +     MONTHLY_AC_ECITY_NEW_FIX_COST(:,:)
     +     MAINT_INDEX_MONTHLY_MW(:,:),
     +     MON_MDS_CL_CLASS_FUEL_COST(:,:,:),
     +     ANNUAL_CL_CAPACITY(:,:,:),
     +     MON_MDS_CL_UNIT_EMISSIONS_COST(:,:,:),
     +     MON_MDS_CL_UNIT_EMISSIONS(:,:,:),
     +     MON_MDS_CL_MULT_FUEL_COST(:,:,:),
     +     MON_MDS_CL_MULT_VAR_COST(:,:,:),
     +     MON_MDS_CL_MULT_FIXED_COST(:,:,:),
     +     MON_MDS_CL_MULT_CAPACITY(:,:,:),
     +     MON_MDS_CL_MULT_ENERGY(:,:,:),
     +     MON_MDS_CL_MULT_PURCHASES(:,:,:),
     +     MON_MDS_CL_CLASS_VAR_COST(:,:,:),
     +     MON_MDS_CL_CLASS_FIXED_COST(:,:,:),
     +     MON_MDS_CL_CLASS_REVENUE(:,:,:),
     +     MON_MDS_CL_CAP_REVENUE(:,:,:),
     +     MON_MDS_CL_CLASS_CAPACITY(:,:,:),
     +     MON_MDS_CL_CLASS_PURCHASES(:,:,:),
     +     MON_MDS_CL_CAP_PURCHASES(:,:,:),
     +     MON_MDS_CL_CLASS_ENERGY(:,:,:),
     +     MON_MDS_CL_CLASS_EMISSIONS(:,:,:),
     +     MON_MDS_CL_CLASS_EMISSIONS_COST(:,:,:),
     +     MON_MDS_INTRA_COMPANY_REVENUES(:,:,:),
     +     MON_MDS_INTRA_COMPANY_NF_BURN(:,:),
     +     MON_MDS_NF_FUEL_LEASED_BC(:,:,:),
     +     MON_MDS_NF_FUEL_OWNED_BC(:,:,:)
!
! FIX TO MAINTENANCE SCHEDULER. 3/8/1.
!
      REAL (KIND=8),ALLOCATABLE :: MON_MDS_CL_UNIT_MMBTUS(:,:)




      INTEGER (KIND=2), ALLOCATABLE :: TG_2_PLANNING_AREA(:),
     +          RETIRE_RETRO_INDEX(:),
     +          TRANS_GROUP_FOR_DATA_BASE(:),
     +          RETIRE_RETRO_POSITION(:),
     +          RETIRE_RETRO_ABATE_INDEX(:),
     +          PURCHASE_ASSET_CLASS_ID(:),
     +          MARKET_RESOURCE_INDEX(:),
     +          MARKET_RESOURCE_COUNTER(:),
     +          CAP_LIMITED_CLASS_POINTER(:),
     +          ASSET_CLASS_NUM(MAX_CL_UNITS),
     +          ASSET_CLASS_VECTOR(MAX_CL_UNITS),
     +          INTRA_COMPANY_CLASS_ID(MAX_CL_UNITS),
     +          ASSET_CLASS_POINTER(:),
     +          MAINT_INDEX(:),
     +          DAY_TYPE_TRANS_GROUP_PAIR(:,:),
     +          DATA_BASE_POSITION(:,:)

      CHARACTER (LEN=1), ALLOCATABLE ::
     +               PURCHASE_POWER_ASSIGN(:),
     +               INTRA_COMPANY_TRANSACTION(MAX_CL_UNITS)

      LOGICAL (KIND=1), ALLOCATABLE :: MAINT_DIVISIBLE(:)
      CHARACTER*20 SPECIAL_UNIT_ID(MAX_CL_UNITS)
      
      upper_trans_group=get_number_of_active_groups()





      IF(ALLOCATED(TG_2_PLANNING_AREA)) DEALLOCATE(TG_2_PLANNING_AREA)
      ALLOCATE(TG_2_PLANNING_AREA(0:MAX(1,UPPER_TRANS_GROUP)))
      IF(ALLOCATED(DAY_TYPE_TRANS_GROUP_PAIR))
     +          DEALLOCATE(DAY_TYPE_TRANS_GROUP_PAIR,DATA_BASE_POSITION)
      ALLOCATE(DAY_TYPE_TRANS_GROUP_PAIR(MAX_DAY_TYPES,
     +                                              0:MAX_TRANS_GROUPS))
      ALLOCATE(DATA_BASE_POSITION(2*MAX_DAY_TYPES,MAX_CL_UNITS))
      IF(ALLOCATED(MAINT_DAYS)) DEALLOCATE(MAINT_DAYS)
      ALLOCATE(MAINT_DAYS(MAX_CL_UNITS))
      IF(ALLOCATED(CL_ANN_CAP)) DEALLOCATE(CL_ANN_CAP,CL_TG_CAP,
     +                                     CL_TG_AFTER_PEAK,
     +                                     CL_TG_RETIRE,
     +                                     NEWGEN_CAP_BY_INDEX,
     +                                     CL_TG_CAP_MARKET_MW,
     +                                     CL_TG_CAP_MARKET_REV)
      ALLOCATE(CL_ANN_CAP(3,STUDY_PERIOD,2))
      call allocate_cl_tg_cap(study_period, upper_trans_group)
      ALLOCATE(NEWGEN_CAP_BY_INDEX(MAX_NEWGEN_INDEX,
     +                                   0:MAX(1,UPPER_TRANS_GROUP),
     +                                                  STUDY_PERIOD))
      ALLOCATE(CL_TG_AFTER_PEAK(0:UPPER_TRANS_GROUP,STUDY_PERIOD))
      ALLOCATE(CL_TG_CAP_MARKET_MW(0:UPPER_TRANS_GROUP),
     +         CL_TG_CAP_MARKET_REV(0:UPPER_TRANS_GROUP))
      ALLOCATE(CL_TG_RETIRE(0:UPPER_TRANS_GROUP,STUDY_PERIOD))
      IF(ALLOCATED(CL_ANNUAL_LOAD_REDUCTION))
     +         DEALLOCATE(CL_ANNUAL_LOAD_REDUCTION)
      ALLOCATE(CL_ANNUAL_LOAD_REDUCTION(STUDY_PERIOD))

