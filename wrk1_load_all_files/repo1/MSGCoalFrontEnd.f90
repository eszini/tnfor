!     ******************************************************************
!     MSGCoalFrontEnd.f90
!     Copyright(c)  2010
!
!     Created: 12/8/2010 3:41:11 PM
!     Author : MARK S GERBER
!     Last change: MSG 1/25/2011 4:32:22 PM
!     ******************************************************************
      MODULE RUN_PERIOD_DATA
         INTEGER (KIND=2) :: BASE_YR,END_YR
         CHARACTER (LEN=5) :: SCENAME
      END MODULE
      program MSGCoalModel
      USE RUN_PERIOD_DATA
      CHARACTER (LEN=256) :: CURRENT_DIRECTORY,COMMAND_LINE,
     +                       UC_COMMAND_LINE,DUMMY,FILE_NAME
      CHARACTER (LEN=4096) :: DSEXEC_LINE
      LOGICAL (KIND=4) :: FILE_EXISTS,cnw_DATA_PROCESSED,
     +                    READ_COAL_MODEL_DATA
      INTEGER IY,CHDIR,IOS,DELETE,I
      CHARACTER (LEN=5) :: STUDY_NAME,BASE_FILE_DEFINITION,
     +                     BASE_FILE_FAMILY
      INTEGER (KIND=2) :: CURRENT_YEAR,END_POINT=1,YEAR,
     +                    FirstRunYr
         CHARACTER (LEN=256) :: COAL_NODE='NONE',            ! 354
     +                          COAL_TRANSPORT='NONE',       ! 355
     +                          COAL_SUPPLY_FORECAST='NONE', ! 356
     +                          PLANT_DEMAND_FORECAST='NONE', ! 357
     +                          COAL_CONTRACTS='NONE',       ! 361
     +                          COAL_MODEL_POINTER_FILE='NONE', ! 372
     +                          COAL_MODEL_SO2_INFO_FILE        ! 373
      LOGICAL (KIND=4) :: VOID_LOGICAL,READ_COAL_ESCALATION_FILE
C MOVES TO CORRECT DIRECTORY FOR THE DEBUGGER )
      CALL GETENV("DEBUGDIR",CURRENT_DIRECTORY)
      IF(LEN_TRIM(CURRENT_DIRECTORY) >=1) THEN
         READ(CURRENT_DIRECTORY,*) UC_COMMAND_LINE,
     +                             COMMAND_LINE 
         IY = CHDIR(UC_COMMAND_LINE)
      ELSE
         CALL GETCL(COMMAND_LINE)
      ENDIF
!      CALL CURDIR(" ",CURRENT_DIRECTORY)
! the command line will be
! study project base_family base_year end_year
! The first three items are the standard command line values.
! Adding the last two eliminates the infromation that is currently inputted from
! MSGPARAM.VAL
      read(COMMAND_LINE,*)SCENAME,BASE_FILE_DEFINITION,BASE_FILE_FAMILY,
     +                    BASE_YR,END_YR,FirstRunYr
      write(*,*) SCENAME,BASE_FILE_DEFINITION,BASE_FILE_FAMILY,
     +           BASE_YR,END_YR,FirstRunYr
      FILE_NAME = "DSF"//trim(BASE_FILE_DEFINITION)//".DAT"
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      OPEN(10,FILE=FILE_NAME,ACCESS='SEQUENTIAL')
      READ(10,*)
      DO
         READ(10,"(A)",IOSTAT=IOS) DSEXEC_LINE
         IF(IOS /= 0) EXIT
         DSEXEC_LINE = trim(DSEXEC_LINE)//',,,,,,,,,,,,,,,,,,,,,,,,,,'
     +                                    //',,,,,,,,,,,,,,,,,,,,,,,,,,'
     +                                    //',,,,,,,,,,,,,,,,,,,,,,,,,,'
     +                                    //',,,,,,,,,,,,,,,,,,,,,,,,,,'
     +                                    //',,,,,,,,,,,,,,,,,,,,,,,,,,'
     +                                    //',,,,,,,,,,,,,,,,,,,,,,,,,,'
     +                                    //',,,,,,,,,,,,,,,,,,,,,,,,,,'
     +                                    //',,,,,,,,,,,,,,,,,,,,,,,,,,'
     +                                    //',,,,,,,,,,,,,,,,,,,,,,,,,,'
     +                                    //',,,,,,,,,,,,,,,,,,,,,,,,,,'
         READ(DSEXEC_LINE,*) DELETE,STUDY_NAME
         IF((INDEX(BASE_FILE_FAMILY,STUDY_NAME) /= 0 .AND.
     +            INDEX(STUDY_NAME,BASE_FILE_FAMILY) /= 0)) THEN
            READ(DSEXEC_LINE,*) DELETE,STUDY_NAME,
     +                          (DUMMY,I=1,353),
!     +                           ESC_FILE,
!     +                          (DUMMY,I=14,312),
!     +                           ESCALATION_FILE_1,
!     +                           ESCALATION_FILE_2,
!     +                           ESCALATION_FILE_3,
!     +                           ESCALATION_FILE_4,   ! 316
!     +                          (DUMMY,I=317,343),
!     +                           ESCALATION_FILE_5, ! 344
!     +                          (DUMMY,I=345,353),
     +                           COAL_NODE,
     +                           COAL_TRANSPORT,
     +                           COAL_SUPPLY_FORECAST,
     +                           PLANT_DEMAND_FORECAST,
     +                          (DUMMY,I=358,360),
     +                           COAL_CONTRACTS,  ! 361
     +                          (DUMMY,I=362,371),
     +                           COAL_MODEL_POINTER_FILE,      ! 372
     +                           COAL_MODEL_SO2_INFO_FILE        ! 373
            EXIT
         ENDIF
      ENDDO
      CLOSE(10)
         IF(INDEX(COAL_NODE,'NONE')==0)
     +                                CALL COAL_BASIN_MAKEBIN(COAL_NODE)
         IF(INDEX(COAL_SUPPLY_FORECAST,'NONE')==0)
     +                    CALL COAL_SUPPLY_MAKEBIN(COAL_SUPPLY_FORECAST)
         IF(INDEX(COAL_TRANSPORT,'NONE')==0)
     +            CALL COAL_TRANSPORTATION_LINKS_MAKEBIN(COAL_TRANSPORT)
         IF(INDEX(PLANT_DEMAND_FORECAST,'NONE')==0)
     +                  CALL PLANT_DEMAND_MAKEBIN(PLANT_DEMAND_FORECAST)
         IF(INDEX(COAL_CONTRACTS,'NONE')==0)
     +                       CALL COAL_CONTRACTS_MAKEBIN(COAL_CONTRACTS)
         IF(INDEX(COAL_MODEL_SO2_INFO_FILE,'NONE')==0)
     +              CALL COAL_SO2_INFO_MAKEBIN(COAL_MODEL_SO2_INFO_FILE)        ! 373
         CALL COAL_ESCALATIONS_MAKEBIN(COAL_MODEL_POINTER_FILE)
         VOID_LOGICAL = READ_COAL_ESCALATION_FILE(BASE_YR)
! TREE AND OVERLAY PROCESSING WOULD GO HERE IN AN ENDPOINT LOOP
         cnw_DATA_PROCESSED = READ_COAL_MODEL_DATA(BASE_YR)
         CALL InitCNWStructure(SCENAME,BASE_YR)
         do YEAR = 1, END_YR - BASE_YR
            CURRENT_YEAR = BASE_YR + YEAR
            CALL GregsCnwRoutine(BASE_YR,YEAR,YEAR==1) ! owner of interface arrays
            CALL COAL_MODEL_REPORTS(CURRENT_YEAR,END_POINT)
         enddo
! **********************************************************************
!   CALL TO THE cnw ROUTINES IN A YEAR LOOP
!   CALL TO OUPUT ROUTINE IN THE YEAR LOOP
! **********************************************************************
      STOP
      end program  MSGCoalModel
      FUNCTION OUTPUT_DIRECTORY()
      CHARACTER (LEN=256) :: OUTPUT_DIRECTORY, CurPath
         call CurDir(" ",CurPath)
         OUTPUT_DIRECTORY  = trim(CurPath)//'\'
      END
      FUNCTION BASE_FILE_DIRECTORY()
      CHARACTER (LEN=256) :: BASE_FILE_DIRECTORY, CurPath
         call CurDir(" ",CurPath)
         BASE_FILE_DIRECTORY  = trim(CurPath)//'\'
      END
      subroutine CurDir(DriveCharIn,CurPath) ! returns without trailing '\'
        USE SERVICE_ROUTINES
        character*1 DriveCharIn
        character*(*) CurPath
        CHARACTER*1024 CURRENT_DIRECTORY
        integer IY
        IY = GETCWD(CURRENT_DIRECTORY)
        IF(IY == 0) THEN
           IY = LEN_TRIM(CURRENT_DIRECTORY)
           IF(CURRENT_DIRECTORY(IY:IY) == '\') THEN
              CURRENT_DIRECTORY(IY:IY) = ' '
           ENDIF
           CurPath = TRIM(CURRENT_DIRECTORY)
        ELSE
           CurPath = ' '
        ENDIF
      end ! subroutine CurDir
      FUNCTION GET_I8_ID_TO_UNIT(EV_UNIT_ID)
         INTEGER (KIND=2) :: GET_I8_ID_TO_UNIT
         INTEGER (KIND=8) :: EV_UNIT_ID
         GET_I8_ID_TO_UNIT = 0
         RETURN
      END FUNCTION
      FUNCTION RUN_COAL_MODEL_ONLY()
         LOGICAL (KIND=1) :: RUN_COAL_MODEL_ONLY
         RUN_COAL_MODEL_ONLY = .TRUE. 
      END
!
! ROUTINES TO ACCESS THE ESCALATION FILES
!
!      FUNCTION RETURN_A_VECTOR_FOR_ALL_YEARS(Vector_Values,VectorNum)
!         LOGICAL (KIND=1) :: RETURN_A_VECTOR_FOR_ALL_YEARS
!         integer (kind=2) :: VectorNum
!         REAL (KIND=4) :: Vector_Values(*)
!         Vector_Values(1:30) = 1.
!         RETURN_A_VECTOR_FOR_ALL_YEARS = .false.
!      END FUNCTION
      FUNCTION TITLE()
         CHARACTER (LEN=40) :: TITLE
         TITLE = "Coal Pricing"
      END
      FUNCTION COMPANY_NAME()
         CHARACTER (LEN=28) :: COMPANY_NAME
         COMPANY_NAME = "Coal Utility"
      END
      FUNCTION GET_NUM_OF_END_POINTS()
         INTEGER (KIND=2) :: GET_NUM_OF_END_POINTS
         GET_NUM_OF_END_POINTS = 1
      END
      FUNCTION BASE_YEAR()
         USE RUN_PERIOD_DATA
         INTEGER (KIND=2) :: BASE_YEAR
         BASE_YEAR = BASE_YR
      END
      FUNCTION ENDYR()
         USE RUN_PERIOD_DATA
         INTEGER (KIND=2) :: ENDYR
         ENDYR = END_YR
      END
      FUNCTION GET_SCENAME()
         USE RUN_PERIOD_DATA
         CHARACTER (LEN=5) :: GET_SCENAME
         GET_SCENAME = SCENAME
      END
