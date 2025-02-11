Type ( 9)          src:                       |grx_planning_routines.f90|
Allocate number       :     1                 | 
alloc sentence in line:   180 (    0 -   544) |allocate(saved_cl_ann_cap(3,study_period,2))|
block starts at line  :   167                 |SUBROUTINE GRX_SAVE_PLANNIG_VALUES(R_SAVE_VALUES)|
block ends   at line  :   201                 |END SUBROUTINE|
line to insert check  :   184 (  167 -   204) |SAVE_VALUES = .TRUE.|
key used              :     1                 | 
------------------------------------------( 1)-
Type (13)          src:                       |grx_planning_routines.f90|
Allocate number       :     2                 | 
alloc sentence in line:   190 (    0 -   549) |allocate(saved_cl_tg_cap(0:6,&|
block starts at line  :   167                 |SUBROUTINE GRX_SAVE_PLANNIG_VALUES(R_SAVE_VALUES)|
block ends   at line  :   206                 |END SUBROUTINE|
line to insert check  :   192 (  167 -   206) |SAVE_VALUES = .TRUE.|
key used              :     2                 | 
------------------------------------------( 4)-
Type ( 9)          src:                       |grx_planning_routines.f90|
Allocate number       :     3                 | 
alloc sentence in line:   231 (    0 -   551) |allocate(grx_bop_retrofit_active(max_cl_units))|
block starts at line  :   225                 |SUBROUTINE GRX_RETROFIT_OPTONS(R_SAVE_VALUES)|
block ends   at line  :   241                 |END SUBROUTINE|
line to insert check  :   235 (  225 -   244) |SAVE_VALUES = .TRUE.|
key used              :     3                 | 
------------------------------------------( 1)-
Type ( 9)          src:                       |grx_planning_routines.f90|
Allocate number       :     4                 | 
alloc sentence in line:   262 (    0 -   556) |allocate(grx_iter_co2_price(0:grx_max_iters,0:tg_max))|
block starts at line  :   248                 |SUBROUTINE GRX_CO2_REDUCTION_CURVE(TG)|
block ends   at line  :   299                 |END SUBROUTINE|
line to insert use all:   257 (  248 -   299) |INTEGER (KIND=2) :: GET_NUMBER_OF_ACTIVE_GROUPS,TG,I,GRX_MAX_ITERS|
line to insert check  :   266 (  248 -   302) |GRX_ITER_CO2_PRICE = 0.|
key used              :     4                 | 
------------------------------------------( 1)-
Type ( 9)          src:                       |grx_planning_routines.f90|
Allocate number       :     5                 | 
alloc sentence in line:   270 (    0 -   561) |allocate(grx_iter_tg_co2_emiss(0:grx_max_iters,0:tg_max))|
block starts at line  :   248                 |SUBROUTINE GRX_CO2_REDUCTION_CURVE(TG)|
block ends   at line  :   304                 |END SUBROUTINE|
line to insert check  :   271 (  248 -   304) |GRX_ITER_TG_CO2_EMISS = 0.|
key used              :     5                 | 
------------------------------------------( 1)-
Type ( 9)          src:                       |grx_planning_routines.f90|
Allocate number       :     6                 | 
alloc sentence in line:   276 (    0 -   563) |allocate(grx_co2_emiss_reduct_required(0:tg_max))|
block starts at line  :   248                 |SUBROUTINE GRX_CO2_REDUCTION_CURVE(TG)|
block ends   at line  :   306                 |END SUBROUTINE|
line to insert check  :   277 (  248 -   306) |GRX_CO2_EMISS_REDUCT_REQUIRED = 0.|
key used              :     6                 | 
------------------------------------------( 1)-
Type ( 9)          src:                       |grx_planning_routines.f90|
Allocate number       :     7                 | 
alloc sentence in line:   287 (    0 -   565) |allocate(grx_iter_co2_price(0:grx_max_iters,0:tg_max))|
block starts at line  :   248                 |SUBROUTINE GRX_CO2_REDUCTION_CURVE(TG)|
block ends   at line  :   308                 |END SUBROUTINE|
line to insert check  :   288 (  248 -   308) |GRX_ITER_CO2_PRICE = 0.|
key used              :     7                 | 
------------------------------------------( 1)-
Type ( 9)          src:                       |grx_planning_routines.f90|
Allocate number       :     8                 | 
alloc sentence in line:   293 (    0 -   567) |allocate(grx_iter_tg_co2_emiss(0:grx_max_iters,0:tg_max))|
block starts at line  :   248                 |SUBROUTINE GRX_CO2_REDUCTION_CURVE(TG)|
block ends   at line  :   310                 |END SUBROUTINE|
line to insert check  :   294 (  248 -   310) |GRX_ITER_TG_CO2_EMISS = 0.|
key used              :     8                 | 
------------------------------------------( 1)-
Type ( 9)          src:                       |grx_planning_routines.f90|
Allocate number       :     9                 | 
alloc sentence in line:   299 (    0 -   569) |allocate(grx_co2_emiss_reduct_required(0:tg_max))|
block starts at line  :   248                 |SUBROUTINE GRX_CO2_REDUCTION_CURVE(TG)|
block ends   at line  :   312                 |END SUBROUTINE|
line to insert check  :   300 (  248 -   312) |GRX_CO2_EMISS_REDUCT_REQUIRED = 0.|
key used              :     9                 | 
------------------------------------------( 1)-
