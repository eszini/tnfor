   
 module base_file_names_from_bfil_obj  ! SOURCE IN MODULES95
     implicit NONE
     CHARACTER*256 :: COAL_NODE='NONE',              & ! 354
		COAL_TRANSPORT='NONE',         & ! 355
        COAL_SUPPLY_FORECAST='NONE',   & ! 356
        PLANT_DEMAND_FORECAST='NONE',   & ! 357
        COAL_CONTRACTS='NONE',          & ! 361
        COAL_MODEL_POINTER_FILE='NONE',  & ! 372
        COAL_MODEL_SO2_INFO_FILE='NONE', & ! 373
        COAL_MODEL_GENERIC_TRANSPORT_FILE='NONE', & ! 374
        COAL_MODEL_GENERIC_DEMAND_FILE='NONE'   ! 375
      					
end module base_file_names_from_bfil_obj
   