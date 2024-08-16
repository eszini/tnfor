!  global_variables_mod.f90
!
!  ANYCHANGES TO THIS COMMON BLOCK MUST ALSO BE DONE IN DSEC_OBJ.FOR
!  MSG 8/21/93
!
      module global_variables_mod
        implicit none
        integer(kind=2) :: base_year
        integer(kind=2) :: year
        integer(kind=2) :: end_point
        integer(kind=2) :: study_period
        integer(kind=2) :: endyr
        integer(kind=2) :: last_study_year
        integer(kind=2) :: extension_period
        integer(kind=2) :: last_extension_year
        integer(kind=2) :: max_years
        integer(kind=2), parameter :: last_available_monthly_year = 5
      end module global_variables_mod





!!
!!  ANYCHANGES TO THIS COMMON BLOCK MUST ALSO BE DONE IN DSEC_OBJ.FOR
!!  MSG 8/21/93
!!
!!  ADDED TESTING_PLAN ON 4/19/94. GAT.
!!
!       INTEGER*2 BASE_YEAR,YEAR,END_POINT,STUDY_PERIOD,ENDYR,
!     +          LAST_STUDY_YEAR,EXTENSION_PERIOD,LAST_EXTENSION_YEAR,
!     +          MAX_YEARS
!       COMMON /GLOBAL_VARIABLES/ BASE_YEAR,YEAR,END_POINT,STUDY_PERIOD,
!     +          LAST_STUDY_YEAR,EXTENSION_PERIOD,LAST_EXTENSION_YEAR,
!     +          ENDYR,MAX_YEARS
!       INTEGER*2 LAST_AVAILABLE_MONTHLY_YEAR
!       PARAMETER (LAST_AVAILABLE_MONTHLY_YEAR=5)
