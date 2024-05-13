!     ******************************************************************
!     ANDECOMP.FOR
!     Copyright(c)  2000
!
!     Created: 1/10/2010 2:44:53 PM
!     Author : MARK S GERBER
!     Last change: MSG 1/10/2010 3:02:39 PM
!     ******************************************************************

C***********************************************************************
C
C    ANNUAL DECOMPOSTION METHOD AS DESCRIBED BY STOLL
C    IN "LEAST-COST UTLITY PLANNING", 1989, JOHN WILEY & SONS
C    COPYRIGHT (C) 1993 M.S. GERBER & ASSOCIATES, INC
C    ALL RIGHTS RESERVED
C
C
C***********************************************************************
C
      RECURSIVE FUNCTION ANNUAL_DECOMPOSITION_START_UP()
C***********************************************************************
C
      INCLUDE 'SPINLIB.MON'
      SAVE
      INCLUDE 'SIZECOM.MON'
      INCLUDE 'GLOBECOM.MON'
C
      REAL RESET_LAST_DEPTH_WOREST_VALUE,
     +     SET_DEPTH_TEST_VALUES,OPTION_DEPTH_CONTINUE_ANALYSIS,
     +     STORE_CURRENT_DEPTH_VALUES
C      LOGICAL*1 TEST_DEPTH_2_ADVANCE
      REAL VOID_REAL
      INTEGER*2 MIN_OPTION_DEPTH
C
      INTEGER*2 J,I
      INTEGER*2 START_LOOP,END_LOOP,CHANGE_LOOP,L
      INTEGER*4 SORTED_POINTER(:)
      ALLOCATABLE :: SORTED_POINTER
      LOGICAL*1 MINIMIZED_OBJECT_FUNCTION,
     +          MIN_MAX_OBJECTIVE_FUNC
      LOGICAL*1 CHECK_RESOURCE_NEED,ANNUAL_DECOMPOSITION_OPTION
      LOGICAL*1 NO_OPTIONS_TO_TEST,ANN_DECOMP_TESTING_PLAN
      INTEGER*2 ANNUAL_DECOMPOSITION_START_UP
      LOGICAL*1 SET_ANNUAL_DECOMP_OPTIONS
      INTEGER*2 RETURN_ACTIVE_OPTIONS_LIST,
     +          NUMBER_OF_ACTIVE_OPTIONS,R_YEAR
      INTEGER*2 OPTION_POINTR(:),
     +          OPTION_ANN_UNITS(:),
     +          AVAIL_ANNUAL_UNITS(:),
     +          OPTION_AVAIL_UNITS(:),
     +          AVAIL_CUM_UNITS(:)
      REAL OPTION_CAP(:)
      CHARACTER*1 OPTION_LOADING_TYPE(:)
      REAL TEST_PEAK,TEST_ANN_CAP
      REAL ADD_UNIT_TEMPORARILY,CAPACITY_ADDED
      REAL DSM_CAPACITY,ANN_DECOMP_PICK_BEST_PLAN
      REAL ANN_DECOMP_SAVE_PLAN
      REAL CAPCITY_ADDED,ADD_THIS_UNIT
      LOGICAL*1 OPTION_IS_ACTIVE(:)
      ALLOCATABLE :: OPTION_POINTR,
     +               OPTION_ANN_UNITS,
     +               AVAIL_ANNUAL_UNITS,
     +               OPTION_AVAIL_UNITS,
     +               AVAIL_CUM_UNITS,
     +               OPTION_CAP,
     +               OPTION_LOADING_TYPE,
     +               OPTION_IS_ACTIVE
C
      INTEGER*4 PLAN_POINTR/0/
      REAL CURRENT_TARGET_RATIO,TARGET_RATIO
      INTEGER*2 CREATE_ANN_DECOMP_COMBINATIONS
      INTEGER*2 MAX_OPTION_DEPTH,OPTIM_DEPTH
      LOGICAL*1 TARGET_MET
      REAL ANN_CAP,EL_PLANNING_CAPACITY,
     +     CL_PLANNING_CAPACITY,
     +     CT_PLANNING_CAPACITY,
     +     ADJUSTMENT_BASE_CAPACITY,
     +     ADJUSTMENT_PEAK_CAPACITY,
     +     ADJUSTMENT_PEAK,
     +     CAPACITY_PEAKS,PEAK,
     +     UPDATE_NET_PLANNING_PEAK,
     +     CURRENT_CAPACITY_RATIO,
     +     CONTRACT_PEAK_ADJUSTMENT,
     +     PEAK_ADJ_OFF_SYSTEM_SALES,
     +     PLANNING_CAPACITY_BEING_TESTED,
     +     CAPACITY_BEING_TESTED/0./,
     +     RETURN_RESERVE_MARGIN_RATIO,
     +     RESERVE_MARGIN_RATIO
C
      INTEGER*2 ACTIVE_OPTIONS,POINTR,
     +          STILL_ACTIVE_OPTIONS,SUM_OF_OPTIONS
C
C VARIABLES FOR STORING OBJECTIVE FUNCTION VALUES
C
      LOGICAL*1 END_OF_PLAN_FLAG,SET_END_OF_PLAN_FLAG_FALSE,
     +          SET_END_OF_PLAN_FLAG_TRUE,
     +          EXPANSION_REPORT,WRITE_TO_EXPANSION_REPORT,
     +          TEST_FOR_BEST_VALUE
      CHARACTER*1 PLANNING_TRIGGER,PLANNING_TRIGGER_METHOD
      INTEGER*2 PLAN_IDENTIFIER(:),
     +          PLAN_OPTION_SEQUENCE(:,:)
      LOGICAL*1 PLAN_HAS_MUT_EXC_UNITS(:),
     +          MUT_EXC_UNIT_ACTIVE,
     +          PLAN_HAS_MUT_EXC_UNIT,
     +          BEST_PLAN_HAS_MUT_EXC_UNITS
      REAL PLAN_OBJECT_FUNCTION_VALUE(:),
     +     WORST_OBJECT_FUNCTION_VALUE/0./,
     +     OBJECTIVE_FUNCTION_VALUE/0./,
     +     ANN_DECOMP_OBJECTIVE_FUNCTION,
     +     MAXIMUM_TESTING_RM,MAX_CAPACITY_TESTING_MARGIN,
     +     MINIMUM_TESTING_RM,MIN_CAPACITY_TESTING_MARGIN
      REAL BEST_OBJECT_FUNCTION_VALUE
      INTEGER*2 BEST_VALUE_LOCATION,NEXT_OPTION_TO_CHECK/1/,
     +          OPTION_DEPTH
      INTEGER*4 NUMBER_OF_SAVED_PLANS/0/,MAX_NUMBER_OF_PLANS_TO_SAVE
      INTEGER*4 WORST_VALUE_LOCATION/0/
      ALLOCATABLE :: PLAN_IDENTIFIER,PLAN_OBJECT_FUNCTION_VALUE,
     +               PLAN_OPTION_SEQUENCE,
     +               PLAN_HAS_MUT_EXC_UNITS
      INTEGER*2 PLAN_BEING_TESTED(:),MAX_PLANS_TO_SAVE
      ALLOCATABLE :: PLAN_BEING_TESTED
      LOGICAL*1 I_TRUE
      PARAMETER(I_TRUE = .TRUE.)
      INTEGER*2 DEPTH_BEST_PLANS(:,:),
     +          PLANS_2_TEST_AT_NEXT_DEPTH(:,:),
     +          LIST_OF_COMBINATIONS(:,:)
C     
      INTEGER*2 DEPTH_BEST_PLAN_FOUND,LOCATION_OF_BEST_PLAN
C     
      INTEGER*2 DEPTH_WOREST_PLAN_LOCATION,
     +          DEPTH_PLANS_2_SAVE,
     +          DEPTH_PLANS_SAVED,
     +          PLANS_SAVED_4_NEXT_DEPTH,
     +          LAST_LEVEL_PLANS
      LOGICAL*1 MATCH_NOT_FOUND,POSITION_MATCHED(:),
     +          VOID_LOGICAL
      LOGICAL*1 VALID_PLAN_TO_TEST,VALID_PLAN_TO_TESTX
      INTEGER*2 NOT_FOUND
      INTEGER*4 MAX_PLANS_2_TEST,BYTES_2_MOVE
      REAL DEPTH_PLAN_OBJECTIVE_VALUE(:),
     +     CURRENT_DEPTH_WOREST_VALUE,
     +     OBJECTIVE_VALUE,
     +     STORE_DEPTH_BEST_PLANS,STORE_DEPTH_BEST_PLANSx,
     +     R_OBJECTIVE_FUNCTION_VALUE
      ALLOCATABLE :: DEPTH_BEST_PLANS,
     +               DEPTH_PLAN_OBJECTIVE_VALUE,
     +               PLANS_2_TEST_AT_NEXT_DEPTH,
     +               LIST_OF_COMBINATIONS,
     +               POSITION_MATCHED
!
      LOGICAL*1   ODD_PLANS
      LOGICAL*1 NOT_REDUNDENT_COMBO,NOT_REDUNDENT_COMBOX
      INTEGER*2   MAX_BAD_PLANS,NUM_OF_BAD_PLANS,
     +            NUM_OF_BAD_PLANS_LAST_DEPTH,
     +            BAD_EVEN_PLANS(:,:),BAD_ODD_PLANS(:,:),PLANS
      ALLOCATABLE :: BAD_EVEN_PLANS,BAD_ODD_PLANS
!
      LOGICAL*1 FILL_OPTIONS_ACTIVE
      INTEGER*2  NUMBER_OF_ACTIVE_FILL_OPTIONS,POS,
     +           ANNUAL_UNITS_LEFT,REDUCE_ANNUAL_UNITS_LEFT
C
      INTEGER*2 FILL_POINTR,FILL_UNITS_NEEDED,GET_FILL_UNITS_NEEDED
      INTEGER*4 TOTAL_BAD_BYTES
      INTEGER*2 TEMP_BAD_ARRAY(:,:)
      ALLOCATABLE :: TEMP_BAD_ARRAY
      REAL*4 RESERVE_CAPACITY_NEEDED,FIRST_YEAR_CAPACITY
      CHARACTER*4 MOST_NEEDED_CAP
      REAL*4 INCREMENTAL_CAPACITY
C      
c      SAVE ODD_PLANS,BAD_EVEN_PLANS,BAD_ODD_PLANS,
c     +     NUM_OF_BAD_PLANS,NUM_OF_BAD_PLANS_LAST_DEPTH,
c     +     MAX_BAD_PLANS
c      SAVE FILL_OPTIONS_ACTIVE
c      SAVE DEPTH_BEST_PLAN_FOUND,LOCATION_OF_BEST_PLAN
c      SAVE DEPTH_BEST_PLANS,
c     +     DEPTH_PLAN_OBJECTIVE_VALUE,
c     +     PLANS_2_TEST_AT_NEXT_DEPTH,
c     +     DEPTH_WOREST_PLAN_LOCATION,
c     +     DEPTH_PLANS_2_SAVE,
c     +     DEPTH_PLANS_SAVED,
c     +     PLANS_SAVED_4_NEXT_DEPTH,
c     +     CURRENT_DEPTH_WOREST_VALUE,
c     +     LAST_LEVEL_PLANS,
c     +     LIST_OF_COMBINATIONS
C
c      SAVE PLAN_IDENTIFIER,PLAN_OBJECT_FUNCTION_VALUE,
c     +     PLAN_OPTION_SEQUENCE,
c     +     PLAN_HAS_MUT_EXC_UNITS,
c     +     PLAN_HAS_MUT_EXC_UNIT,
c     +     ANN_CAP,PEAK,OPTION_POINTR,
c     +     OPTION_ANN_UNITS,OPTION_AVAIL_UNITS,
c     +     AVAIL_CUM_UNITS,
c     +     AVAIL_ANNUAL_UNITS,
c     +     OPTION_CAP,OPTION_LOADING_TYPE,
c     +     OPTION_IS_ACTIVE,
c     +     MAX_NUMBER_OF_PLANS_TO_SAVE,
c     +     ACTIVE_OPTIONS,
c     +     PLAN_BEING_TESTED,
c     +     MINIMIZED_OBJECT_FUNCTION,
c     +     BEST_OBJECT_FUNCTION_VALUE,
c     +     OPTION_DEPTH,
c     +     PLANNING_TRIGGER_METHOD,
c     +     CURRENT_TARGET_RATIO,
c     +     MAX_OPTION_DEPTH,MIN_OPTION_DEPTH,
c     +     WRITE_TO_EXPANSION_REPORT,
c     +     MAXIMUM_TESTING_RM,     
c     +     MINIMUM_TESTING_RM    
!
C
! END DATA DECLARATIONS
C
         ACTIVE_OPTIONS = NUMBER_OF_ACTIVE_OPTIONS()
         FILL_OPTIONS_ACTIVE = NUMBER_OF_ACTIVE_FILL_OPTIONS() > 0
         ANNUAL_DECOMPOSITION_START_UP = ACTIVE_OPTIONS
         IF(ACTIVE_OPTIONS < 1) RETURN
         MAX_NUMBER_OF_PLANS_TO_SAVE = MIN(5*ACTIVE_OPTIONS,
     +                                              MAX_PLANS_TO_SAVE())
         IF(ALLOCATED(PLAN_IDENTIFIER)) THEN
            DEALLOCATE(PLAN_IDENTIFIER,
     +                 PLAN_OBJECT_FUNCTION_VALUE,
     +                 PLAN_HAS_MUT_EXC_UNITS,
     +                 PLAN_OPTION_SEQUENCE)
         ENDIF
         ALLOCATE(PLAN_IDENTIFIER(0:MAX_NUMBER_OF_PLANS_TO_SAVE),
     +        PLAN_OBJECT_FUNCTION_VALUE(0:MAX_NUMBER_OF_PLANS_TO_SAVE),
     +        PLAN_HAS_MUT_EXC_UNITS(0:MAX_NUMBER_OF_PLANS_TO_SAVE),
     +        PLAN_OPTION_SEQUENCE(50,MAX_NUMBER_OF_PLANS_TO_SAVE))
         PLAN_IDENTIFIER = 0
         PLAN_OBJECT_FUNCTION_VALUE = 0.
         PLAN_OPTION_SEQUENCE = -1
         IF(ALLOCATED(OPTION_POINTR)) THEN
            DEALLOCATE(OPTION_POINTR,
     +                 OPTION_ANN_UNITS,
     +                 AVAIL_ANNUAL_UNITS,
     +                 OPTION_AVAIL_UNITS,
     +                 AVAIL_CUM_UNITS,
     +                 OPTION_CAP,
     +                 OPTION_LOADING_TYPE,
     +                 OPTION_IS_ACTIVE,
     +                 PLAN_BEING_TESTED)
         ENDIF
         ALLOCATE(OPTION_POINTR(ACTIVE_OPTIONS),
     +            OPTION_ANN_UNITS(ACTIVE_OPTIONS),
     +            AVAIL_ANNUAL_UNITS(ACTIVE_OPTIONS),
     +            OPTION_AVAIL_UNITS(ACTIVE_OPTIONS),
     +            AVAIL_CUM_UNITS(ACTIVE_OPTIONS),
     +            OPTION_CAP(ACTIVE_OPTIONS),
     +            OPTION_LOADING_TYPE(ACTIVE_OPTIONS),
     +            OPTION_IS_ACTIVE(ACTIVE_OPTIONS),
     +            PLAN_BEING_TESTED(50))
         MINIMIZED_OBJECT_FUNCTION = MIN_MAX_OBJECTIVE_FUNC()
         MAX_OPTION_DEPTH = OPTIM_DEPTH()
         MIN_OPTION_DEPTH = 3
         WRITE_TO_EXPANSION_REPORT = EXPANSION_REPORT()
         VOID_REAL = OPTION_DEPTH_CONTINUE_ANALYSIS()
         DEPTH_PLANS_2_SAVE = 5
      RETURN
C***********************************************************************
      ENTRY SET_ANNUAL_DECOMP_OPTIONS(R_YEAR)
C***********************************************************************
         ACTIVE_OPTIONS = RETURN_ACTIVE_OPTIONS_LIST(R_YEAR,
     +                                              OPTION_POINTR,
     +                                              OPTION_CAP,
     +                                              OPTION_ANN_UNITS,
     +                                              OPTION_AVAIL_UNITS,
     +                                              OPTION_LOADING_TYPE)
C     +                                              OPTION_MAX_CAPACITY)
         IF(ACTIVE_OPTIONS > 0) OPTION_IS_ACTIVE = I_TRUE
         END_OF_PLAN_FLAG = SET_END_OF_PLAN_FLAG_FALSE()
         NEXT_OPTION_TO_CHECK = 1
         OPTION_DEPTH = 1
         NUMBER_OF_SAVED_PLANS = 0
         CALL CLS(3,41,3,48)
C         
         DEPTH_BEST_PLAN_FOUND = 1 
C         
         IF(MINIMIZED_OBJECT_FUNCTION) THEN
            WORST_OBJECT_FUNCTION_VALUE = -999999.
            BEST_OBJECT_FUNCTION_VALUE = 999999.
         ELSE
            WORST_OBJECT_FUNCTION_VALUE = 999999.
            BEST_OBJECT_FUNCTION_VALUE = -999999.
         ENDIF
         VOID_REAL = RESET_LAST_DEPTH_WOREST_VALUE()
         VOID_REAL = SET_DEPTH_TEST_VALUES()
         SET_ANNUAL_DECOMP_OPTIONS = ACTIVE_OPTIONS <= 0
      RETURN
C***********************************************************************
      ENTRY CHECK_RESOURCE_NEED(R_YEAR)
C***********************************************************************
         PLANNING_TRIGGER_METHOD = PLANNING_TRIGGER()
         IF(PLANNING_TRIGGER_METHOD == 'L') THEN
C           TARGET_MET = LOLP_CHECK(LOLP)
         ELSEIF(PLANNING_TRIGGER_METHOD == 'U') THEN
C           TARGET_MET = UNSERVED_ENRG_CHECK(UNSERVEC_ENERGY)
         ELSE
            CURRENT_TARGET_RATIO = TARGET_RATIO(3,R_YEAR)
            MINIMUM_TESTING_RM = MIN_CAPACITY_TESTING_MARGIN(R_YEAR)
            MAXIMUM_TESTING_RM = MAX_CAPACITY_TESTING_MARGIN(R_YEAR)
C
            CALL GET_DSM_CAPACITY(DSM_CAPACITY,R_YEAR)
            ANN_CAP =  EL_PLANNING_CAPACITY(3,R_YEAR) +
     +                 CL_PLANNING_CAPACITY(3,R_YEAR) +
     +                 CT_PLANNING_CAPACITY(3,R_YEAR) +
     +                 DSM_CAPACITY +
     +                 ADJUSTMENT_BASE_CAPACITY(R_YEAR) +
     +                 ADJUSTMENT_PEAK_CAPACITY(R_YEAR)
            PEAK = UPDATE_NET_PLANNING_PEAK(R_YEAR)
!
            IF(FILL_OPTIONS_ACTIVE) THEN
!
               FILL_POINTR = 1
!
               RESERVE_CAPACITY_NEEDED = PEAK*CURRENT_TARGET_RATIO -
     +                                                           ANN_CAP
               FILL_UNITS_NEEDED = GET_FILL_UNITS_NEEDED(FILL_POINTR,
     +                                          R_YEAR+BASE_YEAR,R_YEAR,
     +                                          RESERVE_CAPACITY_NEEDED,
     +                                          MOST_NEEDED_CAP,
     +                                          INCREMENTAL_CAPACITY)
               IF( FILL_UNITS_NEEDED > 0) THEN
!            
                  DO L = 1, FILL_UNITS_NEEDED
                     FIRST_YEAR_CAPACITY=ADD_THIS_UNIT(FILL_POINTR,
     +                                                 R_YEAR+BASE_YEAR)
                     ANNUAL_UNITS_LEFT = 
     +                             REDUCE_ANNUAL_UNITS_LEFT(FILL_POINTR)
                  ENDDO
                  CALL GET_DSM_CAPACITY(DSM_CAPACITY,R_YEAR)
                  ANN_CAP =  EL_PLANNING_CAPACITY(3,R_YEAR) +
     +                       CL_PLANNING_CAPACITY(3,R_YEAR) +
     +                       CT_PLANNING_CAPACITY(3,R_YEAR) +
     +                       DSM_CAPACITY +
     +                       ADJUSTMENT_BASE_CAPACITY(R_YEAR) +
     +                       ADJUSTMENT_PEAK_CAPACITY(R_YEAR)
                  PEAK = UPDATE_NET_PLANNING_PEAK(R_YEAR)
!
               ENDIF
            ENDIF
!         
! END OF FILL
!
            TARGET_MET = 
     +         RETURN_RESERVE_MARGIN_RATIO(PEAK,ANN_CAP,R_YEAR)
     +                                           >= CURRENT_TARGET_RATIO
         ENDIF
         CHECK_RESOURCE_NEED = TARGET_MET
      RETURN
C***********************************************************************
      ENTRY ANNUAL_DECOMPOSITION_OPTION(R_YEAR)                   
C***********************************************************************
C
C CHECK SINGLE OPTIONS
C
         CALL CLEAR_OPTION_DISPLAY_AREA
         J = 0
         CAPACITY_BEING_TESTED = 0.
         PLAN_BEING_TESTED = -1
         ANNUAL_DECOMPOSITION_OPTION = .TRUE.
         PLAN_HAS_MUT_EXC_UNIT = .FALSE.
         IF(OPTION_DEPTH == 1) THEN

            IF(NEXT_OPTION_TO_CHECK == 1) THEN
               MAX_BAD_PLANS = 45
               NUM_OF_BAD_PLANS = 0
               ODD_PLANS = .TRUE.
               IF(ALLOCATED(BAD_ODD_PLANS)) DEALLOCATE(BAD_ODD_PLANS)
               ALLOCATE(BAD_ODD_PLANS(OPTION_DEPTH,MAX_BAD_PLANS))
            ENDIF
!
            DO I = NEXT_OPTION_TO_CHECK, ACTIVE_OPTIONS
               TEST_PEAK = PEAK
               TEST_ANN_CAP = ANN_CAP
               IF(OPTION_CAP(I) > 0) THEN
                  IF(PLANNING_TRIGGER_METHOD == 'L') THEN
C                    TARGET_MET = LOLP_CHECK()
                  ELSEIF(PLANNING_TRIGGER_METHOD == 'U') THEN
C                    TARGET_MET = UNSERVED_ENRG_CHECK()
                  ELSE
                     IF(OPTION_LOADING_TYPE(I) == 'L') THEN
                        TEST_PEAK = TEST_PEAK - OPTION_CAP(I)
                     ELSE
                        TEST_ANN_CAP = TEST_ANN_CAP + OPTION_CAP(I)
                     ENDIF
C                     CURRENT_CAPACITY_RATIO = TEST_ANN_CAP/TEST_PEAK
C                     TARGET_MET = 
C     +                  CURRENT_CAPACITY_RATIO >= CURRENT_TARGET_RATIO
                     RESERVE_MARGIN_RATIO = RETURN_RESERVE_MARGIN_RATIO
     +                                   (TEST_PEAK,TEST_ANN_CAP,R_YEAR)
                     TARGET_MET = RESERVE_MARGIN_RATIO >= 
     +                                        CURRENT_TARGET_RATIO .AND.
     +                            RESERVE_MARGIN_RATIO <= 
     +                                                MAXIMUM_TESTING_RM
                  ENDIF
                  IF(TARGET_MET) THEN
                     POINTR = OPTION_POINTR(I)
                     CAPACITY_ADDED = 
     +                     ADD_UNIT_TEMPORARILY(POINTR,R_YEAR+BASE_YEAR)
                     CAPACITY_BEING_TESTED = CAPACITY_BEING_TESTED +
     +                                                    CAPACITY_ADDED
C                     OPTION_IS_ACTIVE(I) = .FALSE. !!! TEMP !!!
                     OPTION_IS_ACTIVE(I) = RESERVE_MARGIN_RATIO <
     +                                                MAXIMUM_TESTING_RM
                     J = J + 1
                     PLAN_BEING_TESTED(J) = POINTR
                     ANNUAL_DECOMPOSITION_OPTION = .FALSE.
                     CALL DISPLAY_TESTING_OPTION(POINTR,
     +                                              MUT_EXC_UNIT_ACTIVE)
                     PLAN_HAS_MUT_EXC_UNIT = PLAN_HAS_MUT_EXC_UNIT .OR.
     +                                               MUT_EXC_UNIT_ACTIVE
                     IF(CAPACITY_ADDED > 0.) THEN
                        NEXT_OPTION_TO_CHECK = I + 1
                        NUMBER_OF_SAVED_PLANS = NUMBER_OF_SAVED_PLANS+1
!
                        NUM_OF_BAD_PLANS = NUM_OF_BAD_PLANS + 1
                        MAX_BAD_PLANS = MAX_BAD_PLANS + 1
                        IF(NUM_OF_BAD_PLANS <= MAX_BAD_PLANS) THEN
                           DO POS = 1, OPTION_DEPTH
                              BAD_ODD_PLANS(POS,NUM_OF_BAD_PLANS) =
     +                                            PLAN_BEING_TESTED(POS)
                           ENDDO
                        ENDIF
!
                        RETURN
                     ENDIF
                  ENDIF
               ENDIF
            ENDDO
            IF(J > 0) THEN
               NUMBER_OF_SAVED_PLANS = NUMBER_OF_SAVED_PLANS+1
               NEXT_OPTION_TO_CHECK = I + 1
               RETURN
            ENDIF
            NEXT_OPTION_TO_CHECK = 1
            OPTION_DEPTH = 2
!
            ODD_PLANS = .FALSE.
            NUM_OF_BAD_PLANS_LAST_DEPTH = NUM_OF_BAD_PLANS
            NUM_OF_BAD_PLANS = 0
            IF(ALLOCATED(BAD_EVEN_PLANS)) DEALLOCATE(BAD_EVEN_PLANS)
            ALLOCATE(BAD_EVEN_PLANS(OPTION_DEPTH,MAX_BAD_PLANS))
! REMEMBERS PREVIOUS DEPTHS
            DO PLANS = 1, NUM_OF_BAD_PLANS_LAST_DEPTH
               DO POS = 1, OPTION_DEPTH-1
                  BAD_EVEN_PLANS(POS,PLANS) = BAD_ODD_PLANS(POS,PLANS)
               ENDDO
               BAD_EVEN_PLANS(OPTION_DEPTH,PLANS) = -999
            ENDDO
            NUM_OF_BAD_PLANS = NUM_OF_BAD_PLANS_LAST_DEPTH
!
            STILL_ACTIVE_OPTIONS = CREATE_ANN_DECOMP_COMBINATIONS(
     +                                                 OPTION_DEPTH,
     +                                                 ACTIVE_OPTIONS,
     +                                                 OPTION_IS_ACTIVE)
            IF(STILL_ACTIVE_OPTIONS == 0) RETURN
            IF(ALLOCATED(DEPTH_BEST_PLANS))
     +                      DEALLOCATE(DEPTH_BEST_PLANS,
     +                                 DEPTH_PLAN_OBJECTIVE_VALUE,
     +                                 PLANS_2_TEST_AT_NEXT_DEPTH)
            MAX_PLANS_2_TEST = INT(STILL_ACTIVE_OPTIONS) *
     +                                    INT(STILL_ACTIVE_OPTIONS+1)/2
            ALLOCATE(DEPTH_BEST_PLANS(OPTION_DEPTH,DEPTH_PLANS_2_SAVE),
     +               DEPTH_PLAN_OBJECTIVE_VALUE(DEPTH_PLANS_2_SAVE),
     +               PLANS_2_TEST_AT_NEXT_DEPTH(OPTION_DEPTH,
     +                                                MAX_PLANS_2_TEST))
            DEPTH_PLANS_SAVED = 0
            PLANS_SAVED_4_NEXT_DEPTH = 0
         ENDIF
C
         SUM_OF_OPTIONS = 0
         IF(OPTION_DEPTH /= 1) THEN
C
            DO
C
C GET A PLAN TO TEST
C
               NO_OPTIONS_TO_TEST = 
     +                        ANN_DECOMP_TESTING_PLAN(PLAN_BEING_TESTED)
               IF(NO_OPTIONS_TO_TEST) THEN
C               
                  OPTION_DEPTH = OPTION_DEPTH + 1
                  IF(OPTION_DEPTH > MAX_OPTION_DEPTH+3) RETURN! 11/8/95. GAT. JUDGEMENT CALL.
!
                  NUM_OF_BAD_PLANS_LAST_DEPTH = NUM_OF_BAD_PLANS
                  NUM_OF_BAD_PLANS = 0
                  IF(OPTION_DEPTH - 2.*INT(OPTION_DEPTH/2.) > .001) THEN
                     ODD_PLANS = .TRUE.
                     IF(ALLOCATED(BAD_ODD_PLANS)) 
     +                                         DEALLOCATE(BAD_ODD_PLANS)
                     ALLOCATE(BAD_ODD_PLANS(OPTION_DEPTH,MAX_BAD_PLANS))
! REMEMBERS PREVIOUS DEPTHS
                     DO PLANS = 1, NUM_OF_BAD_PLANS_LAST_DEPTH
                        DO POS = 1, OPTION_DEPTH-1
                           BAD_ODD_PLANS(POS,PLANS) =
     +                                         BAD_EVEN_PLANS(POS,PLANS)
                        ENDDO
                        BAD_ODD_PLANS(OPTION_DEPTH,PLANS) = -999
                     ENDDO
                     NUM_OF_BAD_PLANS = NUM_OF_BAD_PLANS_LAST_DEPTH
                  ELSE
                     ODD_PLANS = .FALSE.
                     IF(ALLOCATED(BAD_EVEN_PLANS))
     +                                        DEALLOCATE(BAD_EVEN_PLANS)
                     ALLOCATE(
     +                       BAD_EVEN_PLANS(OPTION_DEPTH,MAX_BAD_PLANS))
! REMEMBERS PREVIOUS DEPTHS
                     DO PLANS = 1, NUM_OF_BAD_PLANS_LAST_DEPTH
                        DO POS = 1, OPTION_DEPTH-1
                           BAD_EVEN_PLANS(POS,PLANS) =
     +                                          BAD_ODD_PLANS(POS,PLANS)
                        ENDDO
                        BAD_EVEN_PLANS(OPTION_DEPTH,PLANS) = -999
                     ENDDO
                     NUM_OF_BAD_PLANS = NUM_OF_BAD_PLANS_LAST_DEPTH
                  ENDIF
!
                  STILL_ACTIVE_OPTIONS =
     +                  CREATE_ANN_DECOMP_COMBINATIONS(OPTION_DEPTH,
     +                                                 ACTIVE_OPTIONS,
     +                                                 OPTION_IS_ACTIVE)
!
!
!
                  IF(ALLOCATED(LIST_OF_COMBINATIONS))
     +                                  DEALLOCATE(LIST_OF_COMBINATIONS)
                  IF(DEPTH_BEST_PLAN_FOUND == OPTION_DEPTH-1) THEN
!
                     LAST_LEVEL_PLANS = PLANS_SAVED_4_NEXT_DEPTH + 1 ! 1 = BEST PLAN
                     ALLOCATE(LIST_OF_COMBINATIONS(OPTION_DEPTH-1,
     +                                                LAST_LEVEL_PLANS))
!     
                     BYTES_2_MOVE = 2*INT(OPTION_DEPTH-1)
                     IF(BYTES_2_MOVE > 0) THEN
                        DO POS = 1, OPTION_DEPTH-1
                           LIST_OF_COMBINATIONS(POS,
     +                                      LOCATION_OF_BEST_PLAN) =
     +                                           DEPTH_BEST_PLANS(POS,
     +                                            LOCATION_OF_BEST_PLAN)
                        ENDDO
                     ENDIF
                     BYTES_2_MOVE = 2*INT(OPTION_DEPTH-1) *
     +                                    INT(PLANS_SAVED_4_NEXT_DEPTH)
                     IF(BYTES_2_MOVE > 0) THEN 
                        DO PLANS = 1, PLANS_SAVED_4_NEXT_DEPTH
                           DO POS = 1, OPTION_DEPTH-1
                              LIST_OF_COMBINATIONS(POS,PLANS+1) =
     +                            PLANS_2_TEST_AT_NEXT_DEPTH(POS,PLANS)  
                           ENDDO
                        ENDDO
                     ENDIF
!
                  ELSE
!
                     LAST_LEVEL_PLANS = PLANS_SAVED_4_NEXT_DEPTH 
                     IF(LAST_LEVEL_PLANS == 0) RETURN
                     ALLOCATE(LIST_OF_COMBINATIONS(OPTION_DEPTH-1,
     +                                                LAST_LEVEL_PLANS))
!
                     BYTES_2_MOVE = 2*INT(OPTION_DEPTH-1) *
     +                                    INT(PLANS_SAVED_4_NEXT_DEPTH)
                     IF(BYTES_2_MOVE > 0) THEN
                        DO PLANS = 1, LAST_LEVEL_PLANS
                           DO POS = 1, OPTION_DEPTH-1
                              LIST_OF_COMBINATIONS(POS,PLANS) =
     +                             PLANS_2_TEST_AT_NEXT_DEPTH(POS,PLANS)
                           ENDDO
                        ENDDO
                     ENDIF
!
                  ENDIF
!
!
!
                  IF(ALLOCATED(DEPTH_BEST_PLANS))
     +                      DEALLOCATE(DEPTH_BEST_PLANS,
     +                                 DEPTH_PLAN_OBJECTIVE_VALUE,
     +                                 PLANS_2_TEST_AT_NEXT_DEPTH)
                  MAX_PLANS_2_TEST = INT(STILL_ACTIVE_OPTIONS) *
     +                             INT(MAX(1,PLANS_SAVED_4_NEXT_DEPTH))
                  ALLOCATE(
     +                DEPTH_BEST_PLANS(OPTION_DEPTH,DEPTH_PLANS_2_SAVE),
     +                DEPTH_PLAN_OBJECTIVE_VALUE(DEPTH_PLANS_2_SAVE),
     +                PLANS_2_TEST_AT_NEXT_DEPTH(OPTION_DEPTH,
     +                                                MAX_PLANS_2_TEST))
                  DEPTH_PLANS_SAVED = 0
                  PLANS_SAVED_4_NEXT_DEPTH = 0
                  CYCLE
!
!
!
               ELSE ! THERE ARE OPTIONS_TO_TEST
C
                  DO POS = 1, ACTIVE_OPTIONS
                     AVAIL_ANNUAL_UNITS(POS) = OPTION_ANN_UNITS(POS)
                     AVAIL_CUM_UNITS(POS) = OPTION_AVAIL_UNITS(POS)
                  ENDDO
                  TEST_PEAK = PEAK
                  TEST_ANN_CAP = ANN_CAP
                  TARGET_MET = .FALSE.
                  DO J = 1, OPTION_DEPTH
                     I = PLAN_BEING_TESTED(J)
                     IF(AVAIL_ANNUAL_UNITS(I) < 1) EXIT  
                     AVAIL_ANNUAL_UNITS(I) = AVAIL_ANNUAL_UNITS(I) - 1
                     IF(AVAIL_CUM_UNITS(I) < 1) EXIT
                     AVAIL_CUM_UNITS(I) = AVAIL_CUM_UNITS(I) - 1
                     IF(OPTION_CAP(I) > 0) THEN
                        IF(PLANNING_TRIGGER_METHOD == 'L') THEN
C                          TARGET_MET = LOLP_CHECK()
                        ELSEIF(PLANNING_TRIGGER_METHOD == 'U') THEN
C                          TARGET_MET = UNSERVED_ENRG_CHECK()
                        ELSE
                           IF(OPTION_LOADING_TYPE(I) == 'L') THEN
                              TEST_PEAK = TEST_PEAK - OPTION_CAP(I)
                           ELSE
                              TEST_ANN_CAP = TEST_ANN_CAP+OPTION_CAP(I)
                           ENDIF
C                          CURRENT_CAPACITY_RATIO=TEST_ANN_CAP/TEST_PEAK
C                          TARGET_MET = CURRENT_CAPACITY_RATIO >= 
C    +                                              CURRENT_TARGET_RATIO
C                          TARGET_MET = 
C    +                        RETURN_RESERVE_MARGIN_RATIO(
C    +                                 TEST_PEAK,TEST_ANN_CAP,R_YEAR)
C    +                                           >= CURRENT_TARGET_RATIO
                        ENDIF
                        IF(TARGET_MET) EXIT
                     ENDIF
                  ENDDO
                  IF(J > OPTION_DEPTH .AND. VALID_PLAN_TO_TESTX() .AND.
     +                                      NOT_REDUNDENT_COMBOX()) THEN
                     RESERVE_MARGIN_RATIO = RETURN_RESERVE_MARGIN_RATIO
     +                                   (TEST_PEAK,TEST_ANN_CAP,R_YEAR)
                     TARGET_MET = RESERVE_MARGIN_RATIO >= 
     +                                        CURRENT_TARGET_RATIO .AND.
     +                            RESERVE_MARGIN_RATIO <= 
     +                                                MAXIMUM_TESTING_RM
                     IF(TARGET_MET) THEN
                        PLAN_HAS_MUT_EXC_UNIT = .FALSE.
                        DO J = 1, OPTION_DEPTH
                           POINTR = OPTION_POINTR(PLAN_BEING_TESTED(J))
                           PLAN_BEING_TESTED(J) = POINTR
                           CAPACITY_ADDED = 
     +                     ADD_UNIT_TEMPORARILY(POINTR,R_YEAR+BASE_YEAR)
                           CAPACITY_BEING_TESTED=CAPACITY_BEING_TESTED +
     +                                                    CAPACITY_ADDED
                           CALL DISPLAY_TESTING_OPTION(POINTR,
     +                                              MUT_EXC_UNIT_ACTIVE)
                           PLAN_HAS_MUT_EXC_UNIT = PLAN_HAS_MUT_EXC_UNIT
     +                                          .OR. MUT_EXC_UNIT_ACTIVE
                        ENDDO
                        ANNUAL_DECOMPOSITION_OPTION = .FALSE.
                        NUMBER_OF_SAVED_PLANS = NUMBER_OF_SAVED_PLANS+1
!
                        IF(NUM_OF_BAD_PLANS == MAX_BAD_PLANS) THEN
                           TOTAL_BAD_BYTES = 2 * MAX_BAD_PLANS * 
     +                                                      OPTION_DEPTH
!
                           ALLOCATE(TEMP_BAD_ARRAY(OPTION_DEPTH, 
     +                                                   MAX_BAD_PLANS))
!
                           MAX_BAD_PLANS = MAX_BAD_PLANS + 10
!                           
                           IF(ODD_PLANS) THEN
                              DO PLANS = 1, MAX_BAD_PLANS
                                 DO POS = 1, OPTION_DEPTH 
                                    TEMP_BAD_ARRAY(POS,PLANS) =
     +                                          BAD_ODD_PLANS(POS,PLANS)
                                 ENDDO
                              ENDDO
                              DEALLOCATE(BAD_ODD_PLANS)
                              ALLOCATE(BAD_ODD_PLANS(OPTION_DEPTH,
     +                                                   MAX_BAD_PLANS))
                              DO PLANS = 1, MAX_BAD_PLANS
                                 DO POS = 1, OPTION_DEPTH 
                                    BAD_ODD_PLANS(POS,PLANS) =
     +                                         TEMP_BAD_ARRAY(POS,PLANS)
                                 ENDDO
                              ENDDO
                           ELSE
                              DO PLANS = 1, MAX_BAD_PLANS
                                 DO POS = 1, OPTION_DEPTH 
                                    TEMP_BAD_ARRAY(POS,PLANS) =
     +                                         BAD_EVEN_PLANS(POS,PLANS)
                                 ENDDO
                              ENDDO
                              DEALLOCATE(BAD_EVEN_PLANS)
                              ALLOCATE(BAD_EVEN_PLANS(OPTION_DEPTH,
     +                                                   MAX_BAD_PLANS))
                              DO PLANS = 1, MAX_BAD_PLANS
                                 DO POS = 1, OPTION_DEPTH 
                                    BAD_EVEN_PLANS(POS,PLANS) =
     +                                         TEMP_BAD_ARRAY(POS,PLANS)
                                 ENDDO
                              ENDDO
                           ENDIF
!
                           DEALLOCATE(TEMP_BAD_ARRAY)
                        ENDIF
                        NUM_OF_BAD_PLANS = NUM_OF_BAD_PLANS + 1
!                        IF(NUM_OF_BAD_PLANS <= MAX_BAD_PLANS) THEN
                           IF(ODD_PLANS) THEN
                              DO POS = 1, OPTION_DEPTH
                                 BAD_ODD_PLANS(POS,NUM_OF_BAD_PLANS) =
     +                                            PLAN_BEING_TESTED(POS)
                              ENDDO
                           ELSE
                              DO POS = 1, OPTION_DEPTH
                                 BAD_EVEN_PLANS(POS,NUM_OF_BAD_PLANS) =
     +                                            PLAN_BEING_TESTED(POS)
                              ENDDO
                           ENDIF
!                        ENDIF
!
C
                        RETURN
                     ELSEIF(RESERVE_MARGIN_RATIO < 
     +                                        CURRENT_TARGET_RATIO) THEN     
                        PLANS_SAVED_4_NEXT_DEPTH =
     +                                      PLANS_SAVED_4_NEXT_DEPTH + 1
                        DO J = 1, OPTION_DEPTH
                           POINTR = OPTION_POINTR(PLAN_BEING_TESTED(J))
                           PLANS_2_TEST_AT_NEXT_DEPTH
     +                                (J,PLANS_SAVED_4_NEXT_DEPTH) = 
     +                                                            POINTR
                        ENDDO
                     ENDIF ! DOES IT MEET THE RESERVE MARGIN?
                  ENDIF ! READY TO TEST A PLAN_BEING_TEST
               ENDIF ! NO_OPTIONS_TO_TEST
            ENDDO ! DO UNTIL OUT OF COMBINATIONS OR TARGET_MET
         ENDIF
         IF(.NOT. ANNUAL_DECOMPOSITION_OPTION) THEN
C           CALL CLOSE_FUTURE_ASSET_FILE
            NUMBER_OF_SAVED_PLANS = NUMBER_OF_SAVED_PLANS + 1
         ENDIF
      RETURN
C***********************************************************************
      ENTRY PLANNING_CAPACITY_BEING_TESTED 
C***********************************************************************
        PLANNING_CAPACITY_BEING_TESTED = CAPACITY_BEING_TESTED
        CAPACITY_BEING_TESTED = 0.
      RETURN
C
C***********************************************************************
      ENTRY ANN_DECOMP_OBJECTIVE_FUNCTION(R_YEAR)
C***********************************************************************
C
         CALL CALCULATE_OBJECTIVE_FUNCTION(R_YEAR,
     +                                         OBJECTIVE_FUNCTION_VALUE)
c         CALL LOCATE(4,66)
c         WRITE(6,"('&',F9.1/)")  OBJECTIVE_FUNCTION_VALUE
         WRITE(SCREEN_MESSAGES,"(F9.1/)")  OBJECTIVE_FUNCTION_VALUE
         CALL MG_LOCATE_WRITE(4,66,trim(SCREEN_MESSAGES),
     +                                                   ALL_VERSIONS,0)
         IF(NUMBER_OF_SAVED_PLANS > MAX_NUMBER_OF_PLANS_TO_SAVE) THEN
            IF(MINIMIZED_OBJECT_FUNCTION) THEN
               IF(OBJECTIVE_FUNCTION_VALUE > 
     +                                WORST_OBJECT_FUNCTION_VALUE) THEN
                  END_OF_PLAN_FLAG = SET_END_OF_PLAN_FLAG_TRUE()
               ENDIF
            ENDIF
         ENDIF
         ANN_DECOMP_OBJECTIVE_FUNCTION = OBJECTIVE_FUNCTION_VALUE
      RETURN
C***********************************************************************
      ENTRY ANN_DECOMP_SAVE_PLAN
C***********************************************************************
         TEST_FOR_BEST_VALUE = .FALSE.
         VOID_REAL=STORE_CURRENT_DEPTH_VALUES(OBJECTIVE_FUNCTION_VALUE)
c         IF(OPTION_DEPTH > 1)
c     +        VOID_REAL=STORE_DEPTH_BEST_PLANS(OBJECTIVE_FUNCTION_VALUE)
         IF(OPTION_DEPTH > 1)
     +       VOID_REAL=STORE_DEPTH_BEST_PLANSx(OBJECTIVE_FUNCTION_VALUE)
         IF(NUMBER_OF_SAVED_PLANS > MAX_NUMBER_OF_PLANS_TO_SAVE) THEN
            IF(MINIMIZED_OBJECT_FUNCTION) THEN
               IF(OBJECTIVE_FUNCTION_VALUE <=
     +                                WORST_OBJECT_FUNCTION_VALUE) THEN
                  TEST_FOR_BEST_VALUE = .TRUE.
                  PLAN_IDENTIFIER(WORST_VALUE_LOCATION) = PLAN_POINTR
                  PLAN_OBJECT_FUNCTION_VALUE(WORST_VALUE_LOCATION) = 
     +                                          OBJECTIVE_FUNCTION_VALUE
                  J = 1
                  PLAN_OPTION_SEQUENCE(:,WORST_VALUE_LOCATION) = -1
                  PLAN_HAS_MUT_EXC_UNITS(WORST_VALUE_LOCATION) =
     +                                             PLAN_HAS_MUT_EXC_UNIT
                  DOWHILE (PLAN_BEING_TESTED(J) > 0)
                     PLAN_OPTION_SEQUENCE(J,WORST_VALUE_LOCATION) =
     +                                              PLAN_BEING_TESTED(J)
                     J = J + 1
                  ENDDO
                  WORST_OBJECT_FUNCTION_VALUE = 
     +                                     PLAN_OBJECT_FUNCTION_VALUE(1)
                  WORST_VALUE_LOCATION = 1
                  DO J = 2, MAX_NUMBER_OF_PLANS_TO_SAVE
                     IF(PLAN_OBJECT_FUNCTION_VALUE(J) >=
     +                                WORST_OBJECT_FUNCTION_VALUE) THEN
                        WORST_OBJECT_FUNCTION_VALUE = 
     +                                          OBJECTIVE_FUNCTION_VALUE
                        WORST_VALUE_LOCATION = J
                     ENDIF
                  ENDDO
               ENDIF
            ELSE
               IF(OBJECTIVE_FUNCTION_VALUE >=
     +                                WORST_OBJECT_FUNCTION_VALUE) THEN
                  TEST_FOR_BEST_VALUE = .TRUE.
                  PLAN_IDENTIFIER(WORST_VALUE_LOCATION) = PLAN_POINTR
                  PLAN_OBJECT_FUNCTION_VALUE(WORST_VALUE_LOCATION) = 
     +                                          OBJECTIVE_FUNCTION_VALUE
                  PLAN_OPTION_SEQUENCE(:,WORST_VALUE_LOCATION) = -1
                  PLAN_HAS_MUT_EXC_UNITS(WORST_VALUE_LOCATION) =
     +                                             PLAN_HAS_MUT_EXC_UNIT
                  J = 1
                  DOWHILE (PLAN_BEING_TESTED(J) > 0)
                     PLAN_OPTION_SEQUENCE(J,WORST_VALUE_LOCATION) =
     +                                              PLAN_BEING_TESTED(J)
                     J = J + 1
                  ENDDO
                  WORST_OBJECT_FUNCTION_VALUE = 
     +                                     PLAN_OBJECT_FUNCTION_VALUE(1)
                  WORST_VALUE_LOCATION = 1
                  DO J = 2, MAX_NUMBER_OF_PLANS_TO_SAVE
                     IF(PLAN_OBJECT_FUNCTION_VALUE(J) <=
     +                                WORST_OBJECT_FUNCTION_VALUE) THEN
                        WORST_OBJECT_FUNCTION_VALUE = 
     +                                          OBJECTIVE_FUNCTION_VALUE
                        WORST_VALUE_LOCATION = J
                     ENDIF
                  ENDDO
               ENDIF
            ENDIF
         ELSE
            TEST_FOR_BEST_VALUE = .TRUE.
            PLAN_IDENTIFIER(NUMBER_OF_SAVED_PLANS) = PLAN_POINTR
            PLAN_OBJECT_FUNCTION_VALUE(NUMBER_OF_SAVED_PLANS) = 
     +                                          OBJECTIVE_FUNCTION_VALUE
            PLAN_OPTION_SEQUENCE(:,NUMBER_OF_SAVED_PLANS) = -1
            PLAN_HAS_MUT_EXC_UNITS(NUMBER_OF_SAVED_PLANS) =
     +                                             PLAN_HAS_MUT_EXC_UNIT
            J = 1
            DOWHILE (PLAN_BEING_TESTED(J) > 0)
               PLAN_OPTION_SEQUENCE(J,NUMBER_OF_SAVED_PLANS) = 
     +                                              PLAN_BEING_TESTED(J)
               J = J + 1
            ENDDO
            IF(MINIMIZED_OBJECT_FUNCTION) THEN
               IF(OBJECTIVE_FUNCTION_VALUE > 
     +                                 WORST_OBJECT_FUNCTION_VALUE)THEN
                  WORST_OBJECT_FUNCTION_VALUE=OBJECTIVE_FUNCTION_VALUE
                  WORST_VALUE_LOCATION = NUMBER_OF_SAVED_PLANS
               ENDIF
            ELSE
               IF(OBJECTIVE_FUNCTION_VALUE < 
     +                                 WORST_OBJECT_FUNCTION_VALUE)THEN
                  WORST_OBJECT_FUNCTION_VALUE=OBJECTIVE_FUNCTION_VALUE
                  WORST_VALUE_LOCATION = NUMBER_OF_SAVED_PLANS
               ENDIF
            ENDIF
         ENDIF
         IF(TEST_FOR_BEST_VALUE) THEN
            IF(MINIMIZED_OBJECT_FUNCTION) THEN
               BEST_OBJECT_FUNCTION_VALUE = MIN(
     +                                       BEST_OBJECT_FUNCTION_VALUE,
     +                                         OBJECTIVE_FUNCTION_VALUE)
            ELSE
               BEST_OBJECT_FUNCTION_VALUE = MAX(
     +                                       BEST_OBJECT_FUNCTION_VALUE,
     +                                         OBJECTIVE_FUNCTION_VALUE)
            ENDIF
c            CALL CLS(3,41,48)
c            CALL LOCATE(3,41)
c            WRITE(6,"('&',F7.1/)") BEST_OBJECT_FUNCTION_VALUE
            WRITE(SCREEN_MESSAGES,"(F7.1/)")  BEST_OBJECT_FUNCTION_VALUE
            CALL MG_CLEAR_LINE_WRITE(3,41,48,trim(SCREEN_MESSAGES),
     +                                                   ALL_VERSIONS,0)
         ENDIF
         ANN_DECOMP_SAVE_PLAN = WORST_OBJECT_FUNCTION_VALUE
      RETURN
C***********************************************************************
      ENTRY ANN_DECOMP_PICK_BEST_PLAN(R_YEAR)
C***********************************************************************
         NUMBER_OF_SAVED_PLANS = MIN(MAX_NUMBER_OF_PLANS_TO_SAVE,
     +                               NUMBER_OF_SAVED_PLANS)
         IF(NUMBER_OF_SAVED_PLANS == 0) THEN
            ANN_DECOMP_PICK_BEST_PLAN =  0.
            RETURN
         ENDIF
         CALL STORE_PLANNING_YEAR_ENDPOINT(R_YEAR+BASE_YEAR)
         BEST_OBJECT_FUNCTION_VALUE=PLAN_OBJECT_FUNCTION_VALUE(1)
         BEST_PLAN_HAS_MUT_EXC_UNITS = PLAN_HAS_MUT_EXC_UNITS(1)
         BEST_VALUE_LOCATION = 1
         DO J = 2, NUMBER_OF_SAVED_PLANS
            IF(MINIMIZED_OBJECT_FUNCTION) THEN
               IF(PLAN_OBJECT_FUNCTION_VALUE(J) <
     +                                  BEST_OBJECT_FUNCTION_VALUE) THEN
                  BEST_OBJECT_FUNCTION_VALUE = 
     +                                     PLAN_OBJECT_FUNCTION_VALUE(J)
                  BEST_VALUE_LOCATION = J
                  BEST_PLAN_HAS_MUT_EXC_UNITS=PLAN_HAS_MUT_EXC_UNITS(J)
               ELSEIF(PLAN_OBJECT_FUNCTION_VALUE(J) ==
     +                                  BEST_OBJECT_FUNCTION_VALUE) THEN
                  IF(PLAN_HAS_MUT_EXC_UNITS(J) .AND. 
     +                           .NOT. BEST_PLAN_HAS_MUT_EXC_UNITS) THEN
                     BEST_VALUE_LOCATION = J
                     BEST_PLAN_HAS_MUT_EXC_UNITS = .TRUE.
                  ENDIF
               ENDIF
            ELSE
               IF(PLAN_OBJECT_FUNCTION_VALUE(J) >
     +                                   BEST_OBJECT_FUNCTION_VALUE)THEN
                  BEST_OBJECT_FUNCTION_VALUE = 
     +                                     PLAN_OBJECT_FUNCTION_VALUE(J)
                  BEST_VALUE_LOCATION = J
                  BEST_PLAN_HAS_MUT_EXC_UNITS=PLAN_HAS_MUT_EXC_UNITS(J)
               ELSEIF(PLAN_OBJECT_FUNCTION_VALUE(J) ==
     +                                  BEST_OBJECT_FUNCTION_VALUE) THEN
                  IF(PLAN_HAS_MUT_EXC_UNITS(J) .AND. 
     +                           .NOT. BEST_PLAN_HAS_MUT_EXC_UNITS) THEN
                     BEST_VALUE_LOCATION = J
                     BEST_PLAN_HAS_MUT_EXC_UNITS = .TRUE.
                  ENDIF
               ENDIF
            ENDIF
         ENDDO
         IF(WRITE_TO_EXPANSION_REPORT) THEN
C
C WRITE THE SAVED PLANS TO DISK FROM FIRST TO WORST
C
            ALLOCATE(SORTED_POINTER(NUMBER_OF_SAVED_PLANS))
            CALL QUICKR(INT(NUMBER_OF_SAVED_PLANS),
     +                  PLAN_OBJECT_FUNCTION_VALUE(1),
     +                  SORTED_POINTER)
            IF(MINIMIZED_OBJECT_FUNCTION) THEN
               START_LOOP = 1
               END_LOOP = NUMBER_OF_SAVED_PLANS
               CHANGE_LOOP = 1
            ELSE
               START_LOOP = NUMBER_OF_SAVED_PLANS
               END_LOOP = 1
               CHANGE_LOOP = -1
            ENDIF
            J = 1
            CALL WRITE_SELECTED_OPTION(J,BEST_VALUE_LOCATION,
     +                  PLAN_OPTION_SEQUENCE(1,BEST_VALUE_LOCATION),
     +                  PLAN_OBJECT_FUNCTION_VALUE(BEST_VALUE_LOCATION))
            DO L = START_LOOP, END_LOOP, CHANGE_LOOP
               I = INT2(SORTED_POINTER(L))
               IF(I == BEST_VALUE_LOCATION) CYCLE
               J = J + 1
               CALL WRITE_SELECTED_OPTION(J,I,PLAN_OPTION_SEQUENCE(1,I),
     +                                    PLAN_OBJECT_FUNCTION_VALUE(I))
            ENDDO
            DEALLOCATE(SORTED_POINTER)
         ENDIF
C
         J = 1
         CALL DISPLAY_PLAN_YEAR_OBJ_FUNC(R_YEAR+BASE_YEAR,
     +                  PLAN_OBJECT_FUNCTION_VALUE(BEST_VALUE_LOCATION))
         DOWHILE (PLAN_OPTION_SEQUENCE(J,BEST_VALUE_LOCATION) > 0) 
            POINTR = PLAN_OPTION_SEQUENCE(J,BEST_VALUE_LOCATION)
            CAPCITY_ADDED = ADD_THIS_UNIT(POINTR,R_YEAR+BASE_YEAR)
            CALL DISPLAY_SELECTED_OPTION(POINTR)
            J = J + 1
         ENDDO
         PEAK = UPDATE_NET_PLANNING_PEAK(R_YEAR)
         CALL SHOW_SELECTED_OPTIONS
         ANN_DECOMP_PICK_BEST_PLAN = 
     +                   PLAN_OBJECT_FUNCTION_VALUE(BEST_VALUE_LOCATION)
      RETURN
C***********************************************************************
      ENTRY VALID_PLAN_TO_TEST
C***********************************************************************
         IF(OPTION_DEPTH > 2) THEN
            VALID_PLAN_TO_TEST = .FALSE.
            ALLOCATE(POSITION_MATCHED(OPTION_DEPTH-1))
            DO I = 1, LAST_LEVEL_PLANS
               NOT_FOUND = 0
               DO L = 1, OPTION_DEPTH - 1 
                  POSITION_MATCHED(L) = .FALSE.
               ENDDO
               DO J = 1, OPTION_DEPTH
                  POINTR = OPTION_POINTR(PLAN_BEING_TESTED(J))
                  MATCH_NOT_FOUND = .TRUE.
                  DO L = 1, OPTION_DEPTH - 1
                     IF(.NOT. POSITION_MATCHED(L) .AND.
     +                         LIST_OF_COMBINATIONS(L,I) == POINTR) THEN
                        MATCH_NOT_FOUND = .FALSE.
                        POSITION_MATCHED(L) = .TRUE.
                        EXIT
                     ENDIF
                  ENDDO
                  IF(MATCH_NOT_FOUND) NOT_FOUND = NOT_FOUND + 1
                  IF(NOT_FOUND > 1) EXIT
               ENDDO
               IF(NOT_FOUND <= 1) THEN
                  VALID_PLAN_TO_TEST = .TRUE.
                  DO L = 1, OPTION_DEPTH - 1 
                     IF(POSITION_MATCHED(L)) CYCLE
                     VALID_PLAN_TO_TEST = .FALSE.
                     EXIT   
                  ENDDO
                  IF(VALID_PLAN_TO_TEST) EXIT
               ENDIF
            ENDDO
            DEALLOCATE(POSITION_MATCHED)
         ELSE
            VALID_PLAN_TO_TEST = .TRUE.
         ENDIF
      RETURN
C***********************************************************************
      ENTRY NOT_REDUNDENT_COMBO
C***********************************************************************
         IF(OPTION_DEPTH > 1) THEN ! COULD BE SET PER MARK
            NOT_REDUNDENT_COMBO = .TRUE.
            ALLOCATE(POSITION_MATCHED(OPTION_DEPTH-1))
            DO I = 1, NUM_OF_BAD_PLANS_LAST_DEPTH
               NOT_FOUND = 0
               DO L = 1, OPTION_DEPTH - 1 
                  POSITION_MATCHED(L) = .FALSE.
               ENDDO
               DO J = 1, OPTION_DEPTH
                  POINTR = OPTION_POINTR(PLAN_BEING_TESTED(J))
                  MATCH_NOT_FOUND = .TRUE.
                  DO L = 1, OPTION_DEPTH - 1
                     IF(ODD_PLANS) THEN ! LAST OPTION_DEPTH WAS EVEN
                        IF(.NOT. POSITION_MATCHED(L) .AND.
     +                       (BAD_EVEN_PLANS(L,I) == POINTR .OR. 
     +                                BAD_EVEN_PLANS(L,I) == -999)) THEN
                           MATCH_NOT_FOUND = .FALSE.
                           POSITION_MATCHED(L) = .TRUE.
                           EXIT
                        ENDIF
                     ELSE ! LAST OPTION_DEPTH WAS ODD
                        IF(.NOT. POSITION_MATCHED(L) .AND.
     +                       (BAD_ODD_PLANS(L,I) == POINTR .OR. 
     +                                 BAD_ODD_PLANS(L,I) == -999)) THEN
                           MATCH_NOT_FOUND = .FALSE.
                           POSITION_MATCHED(L) = .TRUE.
                           EXIT
                        ENDIF
                     ENDIF
                  ENDDO
                  IF(MATCH_NOT_FOUND) NOT_FOUND = NOT_FOUND + 1
                  IF(NOT_FOUND > 1) EXIT
               ENDDO
               IF(NOT_FOUND <= 1) THEN
                  NOT_REDUNDENT_COMBO = .FALSE.
                  DO L = 1, OPTION_DEPTH - 1 
                     IF(POSITION_MATCHED(L)) CYCLE
                     NOT_REDUNDENT_COMBO = .TRUE.
                     EXIT   
                  ENDDO
                  IF(.NOT. NOT_REDUNDENT_COMBO) EXIT
               ENDIF
            ENDDO
            DEALLOCATE(POSITION_MATCHED)
         ELSE
            NOT_REDUNDENT_COMBO = .TRUE.
         ENDIF
      RETURN
C***********************************************************************
      ENTRY STORE_DEPTH_BEST_PLANS(R_OBJECTIVE_FUNCTION_VALUE)
C***********************************************************************
C
C         
         IF(DEPTH_PLANS_SAVED >= DEPTH_PLANS_2_SAVE) THEN
            IF(MINIMIZED_OBJECT_FUNCTION) THEN
               IF(R_OBJECTIVE_FUNCTION_VALUE <
     +                                  CURRENT_DEPTH_WOREST_VALUE) THEN
                  DEPTH_PLAN_OBJECTIVE_VALUE
     +                                    (DEPTH_WOREST_PLAN_LOCATION) = 
     +                                        R_OBJECTIVE_FUNCTION_VALUE
                  DO I = 1, OPTION_DEPTH
C                    POINTR = OPTION_POINTR(PLAN_BEING_TESTED(I))
                     POINTR = PLAN_BEING_TESTED(I)
                     DEPTH_BEST_PLANS(I,DEPTH_WOREST_PLAN_LOCATION) =
     +                                                            POINTR
                  ENDDO
                  CURRENT_DEPTH_WOREST_VALUE=R_OBJECTIVE_FUNCTION_VALUE
C                  
                  IF(CURRENT_DEPTH_WOREST_VALUE < 
     +                                  BEST_OBJECT_FUNCTION_VALUE) THEN
                     LOCATION_OF_BEST_PLAN = DEPTH_WOREST_PLAN_LOCATION
                     DEPTH_BEST_PLAN_FOUND = OPTION_DEPTH
                  ENDIF
C                  
                  DO I = 1, DEPTH_PLANS_2_SAVE 
                     IF(DEPTH_PLAN_OBJECTIVE_VALUE(I) >
     +                                  CURRENT_DEPTH_WOREST_VALUE) THEN
                        CURRENT_DEPTH_WOREST_VALUE = 
     +                                     DEPTH_PLAN_OBJECTIVE_VALUE(I)
                        DEPTH_WOREST_PLAN_LOCATION = I
                     ENDIF
                  ENDDO
               ENDIF   
            ELSE   
               IF(R_OBJECTIVE_FUNCTION_VALUE > 
     +                                  CURRENT_DEPTH_WOREST_VALUE) THEN
                  DEPTH_PLAN_OBJECTIVE_VALUE
     +                                    (DEPTH_WOREST_PLAN_LOCATION) =
     +                                        R_OBJECTIVE_FUNCTION_VALUE
                  DO I = 1, OPTION_DEPTH
C                    POINTR = OPTION_POINTR(PLAN_BEING_TESTED(I))
                     POINTR = PLAN_BEING_TESTED(I)
                     DEPTH_BEST_PLANS(I,DEPTH_WOREST_PLAN_LOCATION) =
     +                                                            POINTR
                  ENDDO
                  CURRENT_DEPTH_WOREST_VALUE=R_OBJECTIVE_FUNCTION_VALUE
C                  
                  IF(CURRENT_DEPTH_WOREST_VALUE >
     +                                  BEST_OBJECT_FUNCTION_VALUE) THEN
                     LOCATION_OF_BEST_PLAN = DEPTH_WOREST_PLAN_LOCATION
                     DEPTH_BEST_PLAN_FOUND = OPTION_DEPTH
                  ENDIF
C                  
                  DO I = 1, DEPTH_PLANS_2_SAVE 
                     IF(DEPTH_PLAN_OBJECTIVE_VALUE(I) <
     +                                 CURRENT_DEPTH_WOREST_VALUE ) THEN
                        CURRENT_DEPTH_WOREST_VALUE = 
     +                                     DEPTH_PLAN_OBJECTIVE_VALUE(I)
                        DEPTH_WOREST_PLAN_LOCATION = I
                     ENDIF
                  ENDDO
               ENDIF   
            ENDIF
         ELSE
            DEPTH_PLANS_SAVED = DEPTH_PLANS_SAVED + 1
            DEPTH_PLAN_OBJECTIVE_VALUE(DEPTH_PLANS_SAVED) =
     +                                        R_OBJECTIVE_FUNCTION_VALUE
            IF(MINIMIZED_OBJECT_FUNCTION) THEN
C                  
               IF(R_OBJECTIVE_FUNCTION_VALUE < 
     +                                  BEST_OBJECT_FUNCTION_VALUE) THEN
                  LOCATION_OF_BEST_PLAN = DEPTH_PLANS_SAVED
                  DEPTH_BEST_PLAN_FOUND = OPTION_DEPTH
               ENDIF
C                  
            ELSE
C                  
               IF(R_OBJECTIVE_FUNCTION_VALUE >
     +                                  BEST_OBJECT_FUNCTION_VALUE) THEN
                  LOCATION_OF_BEST_PLAN = DEPTH_PLANS_SAVED
                  DEPTH_BEST_PLAN_FOUND = OPTION_DEPTH
               ENDIF
C                  
            ENDIF
            DO I = 1, OPTION_DEPTH
C              POINTR = OPTION_POINTR(PLAN_BEING_TESTED(I))
               POINTR = PLAN_BEING_TESTED(I)
               DEPTH_BEST_PLANS(I,DEPTH_PLANS_SAVED) = POINTR
            ENDDO
            CURRENT_DEPTH_WOREST_VALUE = DEPTH_PLAN_OBJECTIVE_VALUE(1)
            DEPTH_WOREST_PLAN_LOCATION = 1
            DO I = 2, DEPTH_PLANS_SAVED
               OBJECTIVE_VALUE = DEPTH_PLAN_OBJECTIVE_VALUE(I)
               IF(MINIMIZED_OBJECT_FUNCTION) THEN
                  IF(OBJECTIVE_VALUE>CURRENT_DEPTH_WOREST_VALUE) THEN
                     CURRENT_DEPTH_WOREST_VALUE = OBJECTIVE_VALUE
                     DEPTH_WOREST_PLAN_LOCATION = I 
                  ENDIF   
               ELSE   
                  IF(OBJECTIVE_VALUE<CURRENT_DEPTH_WOREST_VALUE) THEN
                     CURRENT_DEPTH_WOREST_VALUE = OBJECTIVE_VALUE
                     DEPTH_WOREST_PLAN_LOCATION = I
                  ENDIF 
               ENDIF   
            ENDDO
         ENDIF
         STORE_DEPTH_BEST_PLANS = CURRENT_DEPTH_WOREST_VALUE 
      RETURN
      END
C***********************************************************************
      FUNCTION STORE_DEPTH_BEST_PLANSx(OBJECTIVE_FUNCTION_VALUE)
C***********************************************************************
         REAL STORE_DEPTH_BEST_PLANS,STORE_DEPTH_BEST_PLANSx
         REAL OBJECTIVE_FUNCTION_VALUE
         STORE_DEPTH_BEST_PLANSx =
     +                  STORE_DEPTH_BEST_PLANS(OBJECTIVE_FUNCTION_VALUE)
      RETURN
      END
C***********************************************************************
      FUNCTION VALID_PLAN_TO_TESTX()
C***********************************************************************
         LOGICAL*1 VALID_PLAN_TO_TEST,VALID_PLAN_TO_TESTX
         VALID_PLAN_TO_TESTX = VALID_PLAN_TO_TEST()
      RETURN
      END
C***********************************************************************
      FUNCTION NOT_REDUNDENT_COMBOX()
C***********************************************************************
         LOGICAL*1 NOT_REDUNDENT_COMBO,NOT_REDUNDENT_COMBOX
         NOT_REDUNDENT_COMBOX = NOT_REDUNDENT_COMBO()
      RETURN
      END
C***********************************************************************
      RECURSIVE FUNCTION OPTION_DEPTH_CONTINUE_ANALYSIS()
C***********************************************************************
C
      SAVE
      REAL R_OBJECTIVE_VALUE,OPTION_DEPTH_CONTINUE_ANALYSIS
      LOGICAL*1 MINIMIZED_OBJECT_FUNCTION,
     +          MIN_MAX_OBJECTIVE_FUNC,
     +          TEST_DEPTH_2_ADVANCE
      REAL*4 LAST_DEPTH_WOREST_VALUE,
     +       CURRENT_DEPTH_BEST_VALUE,
     +       CURRENT_DEPTH_WOREST_VALUE,
     +       SET_DEPTH_TEST_VALUES,
     +       STORE_CURRENT_DEPTH_VALUES,
     +       RESET_LAST_DEPTH_WOREST_VALUE
c      SAVE LAST_DEPTH_WOREST_VALUE,
c     +     CURRENT_DEPTH_BEST_VALUE,
c     +     CURRENT_DEPTH_WOREST_VALUE,
c     +     MINIMIZED_OBJECT_FUNCTION
C
         MINIMIZED_OBJECT_FUNCTION = MIN_MAX_OBJECTIVE_FUNC()
         CURRENT_DEPTH_WOREST_VALUE = 0.
         OPTION_DEPTH_CONTINUE_ANALYSIS = CURRENT_DEPTH_WOREST_VALUE
      RETURN
C***********************************************************************
c      ENTRY TEST_DEPTH_2_ADVANCE
cC***********************************************************************
c         TEST_DEPTH_2_ADVANCE = .TRUE.
c         IF(MINIMIZED_OBJECT_FUNCTION) THEN
c            IF(CURRENT_DEPTH_BEST_VALUE >= LAST_DEPTH_WOREST_VALUE)
c     +                                    TEST_DEPTH_2_ADVANCE = .FALSE. 
c         ELSE
c            IF(CURRENT_DEPTH_BEST_VALUE <= LAST_DEPTH_WOREST_VALUE)
c     +                                    TEST_DEPTH_2_ADVANCE = .FALSE. 
c         ENDIF
c         IF(TEST_DEPTH_2_ADVANCE) THEN
c            LAST_DEPTH_WOREST_VALUE = SET_DEPTH_TEST_VALUES()
c         ELSE
c            VOID_REAL = RESET_LAST_DEPTH_WOREST_VALUE()
c            VOID_REAL = SET_DEPTH_TEST_VALUES()
c         ENDIF
c      RETURN
C***********************************************************************
      ENTRY RESET_LAST_DEPTH_WOREST_VALUE
C***********************************************************************
         IF(MINIMIZED_OBJECT_FUNCTION) THEN
            LAST_DEPTH_WOREST_VALUE = 10.**(-30)
         ELSE   
            LAST_DEPTH_WOREST_VALUE = 10.**30
         ENDIF
         RESET_LAST_DEPTH_WOREST_VALUE = LAST_DEPTH_WOREST_VALUE
      RETURN
C***********************************************************************
      ENTRY SET_DEPTH_TEST_VALUES
C***********************************************************************
         SET_DEPTH_TEST_VALUES = CURRENT_DEPTH_WOREST_VALUE
         IF(MINIMIZED_OBJECT_FUNCTION) THEN
            CURRENT_DEPTH_BEST_VALUE = 10.**30
            CURRENT_DEPTH_WOREST_VALUE = 10.**(-30)
         ELSE   
            CURRENT_DEPTH_BEST_VALUE = 10.**(-30)
            CURRENT_DEPTH_WOREST_VALUE = 10.**30
         ENDIF
      RETURN
C***********************************************************************
      ENTRY STORE_CURRENT_DEPTH_VALUES(R_OBJECTIVE_VALUE)
C***********************************************************************
         IF(MINIMIZED_OBJECT_FUNCTION) THEN
            CURRENT_DEPTH_BEST_VALUE = MIN(CURRENT_DEPTH_BEST_VALUE,
     +                                     R_OBJECTIVE_VALUE)
            CURRENT_DEPTH_WOREST_VALUE = MAX(CURRENT_DEPTH_WOREST_VALUE,
     +                                     R_OBJECTIVE_VALUE)
         ELSE   
            CURRENT_DEPTH_BEST_VALUE = MAX(CURRENT_DEPTH_BEST_VALUE,
     +                                     R_OBJECTIVE_VALUE)
            CURRENT_DEPTH_WOREST_VALUE = MIN(CURRENT_DEPTH_WOREST_VALUE,
     +                                     R_OBJECTIVE_VALUE)
         ENDIF
         STORE_CURRENT_DEPTH_VALUES = CURRENT_DEPTH_BEST_VALUE
      RETURN
      END
C***********************************************************************
      FUNCTION CREATE_ANN_DECOMP_COMBINATIONS(OPTION_DEPTH,
     +                                        TOTAL_OPTIONS,
     +                                        OPTION_IS_ACTIVE)
C***********************************************************************
C
      INCLUDE 'SPINLIB.MON'
      LOGICAL*1 OPTION_IS_ACTIVE(*),
     +          ALL_OPTIONS_TESTED,
     +          ANN_DECOMP_TESTING_PLAN,
     +          RESET_DEPTH
      INTEGER*2 OPTION_DEPTH,TOTAL_OPTIONS,I
      INTEGER*2 CREATE_ANN_DECOMP_COMBINATIONS,
     +          PLAN_BEING_TESTED(*)
C
      INTEGER*2 DEPTH,OPTIONS_STILL_ACTIVE
      INTEGER*2 ACTIVE_OPTION_LIST(:),
     +          COMBINATION_ARRAYS(:)
      ALLOCATABLE :: ACTIVE_OPTION_LIST,
     +               COMBINATION_ARRAYS
      SAVE DEPTH,OPTIONS_STILL_ACTIVE
      SAVE ACTIVE_OPTION_LIST,
     +     COMBINATION_ARRAYS,
     +     ALL_OPTIONS_TESTED
C
      DEPTH = OPTION_DEPTH
      OPTIONS_STILL_ACTIVE = 0
      ALL_OPTIONS_TESTED = .FALSE.
C
      IF(ALLOCATED(ACTIVE_OPTION_LIST)) DEALLOCATE(ACTIVE_OPTION_LIST)
      ALLOCATE(ACTIVE_OPTION_LIST(TOTAL_OPTIONS))
      IF(ALLOCATED(COMBINATION_ARRAYS)) DEALLOCATE(COMBINATION_ARRAYS)
C
      DO I = 1, TOTAL_OPTIONS
         IF(.NOT. OPTION_IS_ACTIVE(I)) CYCLE
         OPTIONS_STILL_ACTIVE = OPTIONS_STILL_ACTIVE + 1
         ACTIVE_OPTION_LIST(OPTIONS_STILL_ACTIVE) = I
      ENDDO
      IF(OPTIONS_STILL_ACTIVE /= 0) THEN
         ALLOCATE(COMBINATION_ARRAYS(DEPTH))
         COMBINATION_ARRAYS = 1
      ENDIF
      CREATE_ANN_DECOMP_COMBINATIONS = OPTIONS_STILL_ACTIVE
      RETURN
C
C***********************************************************************
      ENTRY ANN_DECOMP_TESTING_PLAN(PLAN_BEING_TESTED)
C***********************************************************************
         IF(.NOT. ALL_OPTIONS_TESTED) THEN
            DO I = 1, DEPTH
               PLAN_BEING_TESTED(I) = 
     +                         ACTIVE_OPTION_LIST(COMBINATION_ARRAYS(I))
            ENDDO
            RESET_DEPTH = .FALSE.
            DO I = DEPTH, 1, -1
               COMBINATION_ARRAYS(I) = COMBINATION_ARRAYS(I) + 1
               IF(COMBINATION_ARRAYS(I) <= OPTIONS_STILL_ACTIVE) EXIT
               RESET_DEPTH = .TRUE.
            ENDDO
            ALL_OPTIONS_TESTED =  
     +                      COMBINATION_ARRAYS(1) > OPTIONS_STILL_ACTIVE
            IF(RESET_DEPTH .AND.  .NOT. ALL_OPTIONS_TESTED) THEN
               DO I = 2, DEPTH
                  IF(COMBINATION_ARRAYS(I) <= OPTIONS_STILL_ACTIVE)CYCLE
                  COMBINATION_ARRAYS(I) = COMBINATION_ARRAYS(I-1)
               ENDDO
            ENDIF
            ANN_DECOMP_TESTING_PLAN = .FALSE.
         ELSE
            ANN_DECOMP_TESTING_PLAN = .TRUE.
         ENDIF
      RETURN
      END
C***********************************************************************
      SUBROUTINE WRITE_SELECTED_OPTION(PLAN_ORDER,PLAN_NUMBER,
     +                                 PLAN_OPTION_SEQUENCE,
     +                                 PLAN_OBJECT_FUNCTION_VALUE)
C***********************************************************************
C
      USE IREC_ENDPOINT_CONTROL
      INCLUDE 'SPINLIB.MON'
      INCLUDE 'GLOBECOM.MON'
      INTEGER*2 PLAN_NUMBER,PLAN_ORDER,I,VOID_INT2
      INTEGER*2 PLANNING_YEAR,R_PLANNING_YEAR
      INTEGER*2 PLAN_OPTION_SEQUENCE(*)
      INTEGER*2 OPTIONS_IN_REPORT_FILE,
     +          RESOURCES_TESTED_RPT_HEADER
      PARAMETER (OPTIONS_IN_REPORT_FILE=20)
      REAL FLOAT_PLAN_SEQUENCE(OPTIONS_IN_REPORT_FILE)
      REAL PLAN_OBJECT_FUNCTION_VALUE,
     +     RETURN_RESOURCE_ID
      LOGICAL*1 REPORT_HEADER_WRITTEN/.FALSE./
      INTEGER NEXT_REC_1501
      SAVE NEXT_REC_1501
C
      SAVE PLANNING_YEAR
C
         IF(.NOT. REPORT_HEADER_WRITTEN) THEN
            VOID_INT2 = RESOURCES_TESTED_RPT_HEADER(NEXT_REC_1501)
            REPORT_HEADER_WRITTEN = .TRUE.
         ENDIF
         FLOAT_PLAN_SEQUENCE = -999999.
         DO I = 1, OPTIONS_IN_REPORT_FILE
            IF(PLAN_OPTION_SEQUENCE(I) < 1) EXIT
            FLOAT_PLAN_SEQUENCE(I) = 
     +                       RETURN_RESOURCE_ID(PLAN_OPTION_SEQUENCE(I))
         ENDDO
         WRITE(1501,REC=NEXT_REC_1501) PRT_ENDPOINT(),
     +               FLOAT(PLANNING_YEAR),
     +               FLOAT(PLAN_ORDER),FLOAT(PLAN_NUMBER),
     +               PLAN_OBJECT_FUNCTION_VALUE,
     +               FLOAT_PLAN_SEQUENCE
         NEXT_REC_1501 = NEXT_REC_1501 + 1
      RETURN
      ENTRY STORE_PLANNING_YEAR_ENDPOINT(R_PLANNING_YEAR)
         PLANNING_YEAR = R_PLANNING_YEAR
      RETURN
      END
C***********************************************************************
      SUBROUTINE DISPLAY_PLAN_YEAR_OBJ_FUNC(PLANNING_YEAR,
     +                                       PLAN_OBJECT_FUNCTION_VALUE)
C***********************************************************************
C
      INCLUDE 'SPINLIB.MON'
      CHARACTER*2 RESOURCE_TYPE
      CHARACTER*27 UNIT_NAME
      CHARACTER*20 GET_OPTIONS_RES_TYPE
      INTEGER*2 LINE,COLUMN
      LOGICAL*1 R_MUT_EXC_UNIT_ACTIVE
      SAVE LINE,COLUMN
C
C VIRTUAL SCREEN VARIABLES
C
      INTEGER*4 VS_ROW/0/,DISPLAY_ROW
      INTEGER*2 PLANNING_YEAR
      INTEGER*2 POINTR
      CHARACTER*20 NAME,GET_OPTIONS
      CHARACTER*54 OPTION_STRING
      REAL CAPACITY,COST
      REAL PLAN_OBJECT_FUNCTION_VALUE
      SAVE OPTION_STRING
C
         WRITE(OPTION_STRING,'(I4,3X,F7.1)') PLANNING_YEAR,
     +                                       PLAN_OBJECT_FUNCTION_VALUE
C        CALL LOCVS(1000,VS_ROW,0)
C        CALL PRINTV(1000,OPTION_STRING(1:14))
      RETURN
      ENTRY DISPLAY_SELECTED_OPTION(POINTR)
         NAME = GET_OPTIONS(CAPACITY,COST,POINTR)
         WRITE(OPTION_STRING(17:),'(F6.1,2X,F6.1,2X,A)') 
     +                                                CAPACITY,COST,NAME
         CALL LOCVS(1000,VS_ROW,0)
C        CALL LOCVS(1000,VS_ROW,15)
         CALL PRINTV(1000,OPTION_STRING)
         VS_ROW = VS_ROW + 1
      RETURN
      ENTRY SHOW_SELECTED_OPTIONS
         DISPLAY_ROW = MAX(VS_ROW-8,0)
         CALL VIEW(1000,11,DISPLAY_ROW,0)
      RETURN
      ENTRY SET_VIRTUAL_ROW_TO_ZERO
         VS_ROW = 0
      RETURN
C***********************************************************************
      ENTRY DISPLAY_TESTING_OPTION(POINTR,R_MUT_EXC_UNIT_ACTIVE)
C***********************************************************************
C
      UNIT_NAME = GET_OPTIONS_RES_TYPE(RESOURCE_TYPE,POINTR,
     +                                            R_MUT_EXC_UNIT_ACTIVE)
      CALL LOCATEW(10,LINE,COLUMN)
      CALL PRINTW(10,UNIT_NAME//RESOURCE_TYPE)
      IF(COLUMN == 0) THEN
         COLUMN = 32
      ELSE
         LINE = LINE + 1
         COLUMN = 0
         IF(LINE > 10) LINE = 8
      ENDIF
      RETURN
C
      ENTRY CLEAR_OPTION_DISPLAY_AREA
         LINE = 0
         COLUMN = 0
         CALL CLRW(10)
      RETURN
      END

