!     ******************************************************************
!     Dsex_obj.for
!     Copyright(c)  2000
!
!     Created: 6/23/2003 2:14:00 PM
!     Author : MARK S GERBER
!     Last change: msg 11/21/2021 12:54:01 PM
!     ******************************************************************

! Last change: MSG 6/23/2003 2:12:08 PM
      FUNCTION START_UP(R_DEBUG_ON,R_BASE_FILE_DEFINITION,
     +                  POINTS_IN_LOAD_CURVE,R_BASE_FILE_FAMILY)
C
C
! ISOLATE START_UP VARIABLES
C
      use report_header
      use logging
      use startup_tasks
      USE SERVICE_ROUTINES
      USE prod_arrays_dimensions
      use cl_data
      use dsex_data
      INCLUDE 'DREPTCOM.MON'
      CHARACTER*(*) R_BASE_FILE_DEFINITION,R_BASE_FILE_FAMILY
      LOGICAL*1 SET_DELTA_X_IN_LOAD_CURVE
      REAL*4 DELTA_X_FOR_LOAD_CURVE
      CHARACTER*256 TEMP_BASE_FILE_DEF,TEMP_BASE_FAMILY_NAME
      CHARACTER*256 BASE_FILE_DIRECTORY,RESULTS_DIRECTORY,
     +              FILE_NAME,GET_RESULTS_DIRECTORY
      CHARACTER*256 BASE_FILE_DEFINITION
      INTEGER*2 LENGHT,POS,POINTS_IN_LOAD_CURVE
      LOGICAL*1 NO_WARNING_WINDOW,DONT_SUPPRESS_WARNING_WINDOW,
     +          DEBUG_ON,DEBUG_ON_ACTIVE,R_DEBUG_ON,
     +          ITERATION_ACTIVE,ITERATION_ON,
     +          MULTI_AREA_TRANC,MULTI_AREA_TRANC_ACTIVE,
     +          SIMSTATUS,SIMSTATUS_ACTIVE,
     +          NUCLEAR_FUEL_REPORT_ON,NUCLEAR_FUEL_REPORT_ACTIVE,
     +          MONTAUK_ACTIVE,MONTAUK_ACTIVE_FLAG,
     +          CREDITS_IN_INCOME_STATEMENT,CREDITS_IN_INCOME_FLAG,
     +          OVERLAY_BOOK_ACTIVE,USE_OVERLAY_BOOK,
     +          START_UP,
     +          SET_POINTS_IN_LOAD_CURVE,
     +          VOID_LOGICAL,
     +          SET_DELTA_X_VALUE,
     +          CD_LINE_ASSET_ANALYST_ACTIVE,
     +          CL_ASSET_ANALYST_ACTIVE,
     +          QUICKSILVER,QUICKSILVER_ACTIVE,
     +          NO_AUTO_INDEXING,NO_AUTO_INDEXING_ACTIVE,
     +          SP_CAPEX_ACTIVE,
     +          SET_SP_CAPEX_ACTIVE_WITHOUT_DISPLAY,
     +          SET_SP_CAPEX_ACTIVE_WITH_DISPLAY,
     +          SP_CAPEX_DISPLAY_ACTIVE,
     +          SET_DEBUG_ON,
     +          COAL_MODEL_ONLY_ACTIVE,RUN_COAL_MODEL_ONLY
      SAVE NUCLEAR_FUEL_REPORT_ACTIVE,
     +     USE_OVERLAY_BOOK,
     +     MONTAUK_ACTIVE_FLAG,
     +     DEBUG_ON_ACTIVE,
     +     ITERATION_ACTIVE,
     +     MULTI_AREA_TRANC,
     +     SIMSTATUS,
     +     CREDITS_IN_INCOME_FLAG,
     +     DONT_SUPPRESS_WARNING_WINDOW,
     +     CL_ASSET_ANALYST_ACTIVE,
     +     QUICKSILVER_ACTIVE,
     +     NO_AUTO_INDEXING_ACTIVE,
     +     COAL_MODEL_ONLY_ACTIVE
      LOGICAL (KIND=1), SAVE :: RUN_SP_CAPEX=.FALSE.,
     +                          DISPLAY_SPCAPEX=.TRUE.
      INTEGER (KIND=2), SAVE :: ENDING_ENDPOINT,STARTING_ENDPOINT,
     +                          ITERATION_NUMBER
      INTEGER (KIND=2) :: START_STUDY_ENDPOINT,LAST_STUDY_ENDPOINT,
     +                    GET_ITERATION_NUMBER
      integer :: idx, idx2, ic
      character (len=256) :: tempcl
      character (len=5) :: col1, col2, col3


        LENGHT=0 ! Avoid spurious lahey "not initialized" warning

        if(len_trim(cldata%command_line)<2) then
            call end_program("dsex_obj:0001 - Command line not set.")
        endif
        call startup_dsex(TEMP_BASE_FAMILY_NAME)
C
! SET COMMAND LOGIC SWITCHES
C
         tempcl=cldata%command_line
         MONTHLY_MIDAS_ACTIVE = (INDEX(tempcl,'/ACTMONTHY') /= 0)
         DEBUG_ON_ACTIVE = (INDEX(tempcl,'/DF') /= 0)
         MULTI_AREA_TRANC = (INDEX(tempcl,'/MULTI_TRANC') /= 0)
         SIMSTATUS = (INDEX(tempcl,'/SIMSTATUS') /= 0)
         R_DEBUG_ON = DEBUG_ON_ACTIVE
         NUCLEAR_FUEL_REPORT_ACTIVE = (INDEX(tempcl,'/NF') /= 0)
         DONT_SUPPRESS_WARNING_WINDOW =(INDEX(tempcl,'/BAT') == 0)
         USE_OVERLAY_BOOK = .TRUE. ! INDEX(tempcl,'/OVLBOOK') /= 0
         MONTAUK_ACTIVE_FLAG = INDEX(tempcl,'/MONTAUK') /= 0
         CREDITS_IN_INCOME_FLAG = INDEX(tempcl,'/CREDIT') /= 0
         CL_ASSET_ANALYST_ACTIVE = INDEX(tempcl,'/AACTIVE') /= 0
         QUICKSILVER_ACTIVE = INDEX(tempcl,'/QUICKSILVER') /= 0
         NO_AUTO_INDEXING_ACTIVE = INDEX(tempcl,'/NOINDX') /= 0
         COAL_MODEL_ONLY_ACTIVE=INDEX(tempcl,'/COALMODELONLY') /=0
         IF(QUICKSILVER_ACTIVE .OR. RUN_SP_CAPEX) then
             DONT_SUPPRESS_WARNING_WINDOW = .FALSE.
        endif
!
         if(len_trim(tempcl)<20) then
            call end_program("dsex:0004 - Command-line not set " //
     + "in startup_dsex.")
     
         endif
         ITERATION_ACTIVE = .FALSE.
         IF(INDEX(tempcl,'/ITER') /= 0) THEN
            POS = INDEX(tempcl,'/ITER') + 5
            READ(tempcl(POS:),*) ITERATION_NUMBER
            ITERATION_ACTIVE = .TRUE.
         ENDIF
C
         ENDING_ENDPOINT = 30000
         STARTING_ENDPOINT = 1
         IF(INDEX(tempcl,'/EPS') /= 0) THEN
            POS = INDEX(tempcl,'/EPS') + 4
            READ(tempcl(POS:),*) STARTING_ENDPOINT
            STARTING_ENDPOINT = MAX(1,STARTING_ENDPOINT)
         ENDIF
         IF(INDEX(tempcl,'/EPE') /= 0) THEN
            POS = INDEX(tempcl,'/EPE') + 4
            READ(tempcl(POS:),*) ENDING_ENDPOINT
            ENDING_ENDPOINT = MAX(1,ENDING_ENDPOINT,STARTING_ENDPOINT)
         ENDIF
         SET_POINTS_IN_LOAD_CURVE = INDEX(tempcl,'/PS') /= 0
         IF(SET_POINTS_IN_LOAD_CURVE) THEN
            POS = INDEX(tempcl,'/PS') + 3
            READ(tempcl(POS:),*) POINTS_IN_LOAD_CURVE
         ELSE
            POINTS_IN_LOAD_CURVE = 79
         ENDIF
         SET_DELTA_X_IN_LOAD_CURVE = INDEX(tempcl,'/DX') /= 0
         IF(SET_DELTA_X_IN_LOAD_CURVE) THEN
            POS = INDEX(tempcl,'/DX') + 3
            READ(tempcl(POS:),*) DELTA_X_FOR_LOAD_CURVE
         ELSE
            DELTA_X_FOR_LOAD_CURVE = -100.
         ENDIF
         VOID_LOGICAL = SET_DELTA_X_VALUE(DELTA_X_FOR_LOAD_CURVE)
C
! STRIP THE COMMAND LINE OF THE PROJECT NAME AND BASE FAmILY
C


C
! STRIP PROJECT/DSF FILE NAME
C

         tempcl = trim(tempcl)//",,,"
         TEMP_BASE_FILE_DEF = ""
         
         ! Read TEMP_BASE_FAMILY_NAME from command line
         idx=index(tempcl, ",")
         call read_cols_from_tempcl(tempcl, col1, col2, col3)
         temp_base_file_def=col1
         if(len_trim(temp_base_file_def)<2) then
            call end_program("dsex:99 - " //
     + "Could not read base file definition from command line.")
         endif
         BASE_FILE_DEFINITION=trim(col2)
         TEMP_BASE_FAMILY_NAME=trim(col3)
     
!         if(trim(TEMP_BASE_FILE_DEF) /= '') then
!            BASE_FILE_DEFINITION = TEMP_BASE_FILE_DEF
!         else
!        call end_program("dsex:0001 - base_file_definition not set.")
!         endif

         if(len_trim(base_file_definition)<2) then
             call end_program("dsex:0001 - " // 
     +           "base_file_definition not set.")
         endif
C
! STRIP BASE FAMILY NAME
C

       call write_log_entry("dsex:00020",
     + "TEMP_BASE_FILE_DEF=" // trim(TEMP_BASE_FILE_DEF) //
     + ", " // "TEMP_BASE_FAMILY_NAME=" //
     + trim(TEMP_BASE_FAMILY_NAME))
     
         R_BASE_FILE_FAMILY = 'NONE'
         
         IF(trim(TEMP_BASE_FAMILY_NAME) /= ' ') then
             call write_log_entry("dsex:00018", 
     +         "Setting R_BASE_FILE_FAMILY=" // 
     +          trim(TEMP_BASE_FAMILY_NAME))
     
             R_BASE_FILE_FAMILY = TEMP_BASE_FAMILY_NAME
             
         endif

         R_BASE_FILE_DEFINITION = BASE_FILE_DEFINITION

         RPT_BASE_FAMILY_NAME = R_BASE_FILE_FAMILY
         RPT_PROJECT_NAME = BASE_FILE_DEFINITION
         cldata%command_line=tempcl
         START_UP = .TRUE.
      RETURN
C***********************************************************************
      ENTRY GET_ITERATION_NUMBER
         GET_ITERATION_NUMBER = ITERATION_NUMBER
      RETURN
C***********************************************************************
      ENTRY START_STUDY_ENDPOINT
         START_STUDY_ENDPOINT = STARTING_ENDPOINT
      RETURN
C***********************************************************************
      ENTRY LAST_STUDY_ENDPOINT
         LAST_STUDY_ENDPOINT = ENDING_ENDPOINT
      RETURN
C***********************************************************************
      ENTRY MONTAUK_ACTIVE
         MONTAUK_ACTIVE = MONTAUK_ACTIVE_FLAG
      RETURN
C***********************************************************************
      ENTRY CREDITS_IN_INCOME_STATEMENT
         CREDITS_IN_INCOME_STATEMENT = CREDITS_IN_INCOME_FLAG
      RETURN
C***********************************************************************
      ENTRY OVERLAY_BOOK_ACTIVE
         OVERLAY_BOOK_ACTIVE = USE_OVERLAY_BOOK
      RETURN
C***********************************************************************
      ENTRY DEBUG_ON
         DEBUG_ON = DEBUG_ON_ACTIVE
      RETURN
C***********************************************************************
      ENTRY SET_DEBUG_ON
         DEBUG_ON_ACTIVE = .TRUE.
         SET_DEBUG_ON = DEBUG_ON_ACTIVE
      RETURN
C***********************************************************************
      ENTRY ITERATION_ON
         ITERATION_ON = ITERATION_ACTIVE
      RETURN
C***********************************************************************
      ENTRY MULTI_AREA_TRANC_ACTIVE
         MULTI_AREA_TRANC_ACTIVE = MULTI_AREA_TRANC
      RETURN
C***********************************************************************
      ENTRY SIMSTATUS_ACTIVE
         SIMSTATUS_ACTIVE = SIMSTATUS
      RETURN
C***********************************************************************
      ENTRY NO_WARNING_WINDOW
         NO_WARNING_WINDOW = DONT_SUPPRESS_WARNING_WINDOW
      RETURN
C***********************************************************************
      ENTRY QUICKSILVER
         QUICKSILVER = QUICKSILVER_ACTIVE
      RETURN
C***********************************************************************
      ENTRY SP_CAPEX_ACTIVE
         SP_CAPEX_ACTIVE = RUN_SP_CAPEX
      RETURN
C***********************************************************************
      ENTRY SP_CAPEX_DISPLAY_ACTIVE
         SP_CAPEX_DISPLAY_ACTIVE = DISPLAY_SPCAPEX
      RETURN
C***********************************************************************
      ENTRY SET_SP_CAPEX_ACTIVE_WITH_DISPLAY()
         RUN_SP_CAPEX = .true.
         DISPLAY_SPCAPEX = .TRUE.
         SET_SP_CAPEX_ACTIVE_WITH_DISPLAY = DISPLAY_SPCAPEX
      RETURN
C***********************************************************************
      ENTRY SET_SP_CAPEX_ACTIVE_WITHOUT_DISPLAY()
         RUN_SP_CAPEX = .true.
         DISPLAY_SPCAPEX = .FALSE.
         SET_SP_CAPEX_ACTIVE_WITHOUT_DISPLAY = RUN_SP_CAPEX
      RETURN
C***********************************************************************
      ENTRY NO_AUTO_INDEXING
         NO_AUTO_INDEXING = NO_AUTO_INDEXING_ACTIVE
      RETURN
! **********************************************************************
      ENTRY RUN_COAL_MODEL_ONLY
         RUN_COAL_MODEL_ONLY  = COAL_MODEL_ONLY_ACTIVE
      RETURN
C***********************************************************************
      ENTRY NUCLEAR_FUEL_REPORT_ON
         NUCLEAR_FUEL_REPORT_ON = NUCLEAR_FUEL_REPORT_ACTIVE
      RETURN
C***********************************************************************
      ENTRY CD_LINE_ASSET_ANALYST_ACTIVE
         CD_LINE_ASSET_ANALYST_ACTIVE =  CL_ASSET_ANALYST_ACTIVE
      RETURN
      END function START_UP
C
C***********************************************************************
      FUNCTION DSEXEC_OBJECT(POINTS_IN_LOAD_CURVE)
      use end_routine, only: end_program, er_message
      use params
      use logging
      use miscmod
      use dsex_data
      use cl_data
      use msgoutpt
      use production
      use spindriftlib
      use prod_arrays_dimensions
      use kepcocom
      use sizecom
      use globecom

      
      INCLUDE 'DREPTCOM.MON'
C
      CHARACTER*300 DSEXEC_LINE*2048,ERR_MESSAGE*152
      CHARACTER*1 ESC_DISP_ADDER_CHR,SHORT_FORM_CHR
      LOGICAL*1 SHORT_FORM_ACTIVE/.FALSE./
      CHARACTER*12 DES_FILE_NAME
      INTEGER*2 DELETE

      INTEGER*4 IOS
      INTEGER*2 DIREC_LEN,I,ACTIVE_VARIABLES
      INTEGER*2 STORED_PROCMETH,SET_PRODUCTION_METHOD,DIMENSIONS
      SAVE STORED_PROCMETH
      CHARACTER*256 BASE_FILE_DIRECTORY,FILE_NAME,RESULTS_DIRECTORY,
     +               LDE_FILE_DIRECTORY,
     +               LDG_FILE_DIRECTORY,
     +               ENRG_PATTERN_FILE_DIRECTORY,
     +               WORKING_FILE_DIRECTORY,
     +               GET_WORKING_FILE_DIRECTORY,
     +               PRB_FILE_DIRECTORY,
     +               SHB_FILE_DIRECTORY,
     +               PRJ_FILE_DIRECTORY,
     +               XML_FILE_DIRECTORY,
     +               COMMAND_LINE,GET_RESULTS_DIRECTORY
      INTEGER*2 PRODUCTION_PERIODS_IN,ENDING_MONTHS(4)/4*0/,ISERROR
      LOGICAL*1 ESC_DISP_ADDER_ACTIVE



      INTEGER*4 R_START_RECORD
      CHARACTER*28 COMPANY_NAME_STR,TITLE_STR*40,STUDY_DESCRIPTION*40
      CHARACTER*1 PRODUCTION_COST_METHOD_CHR,UTILITY_TYPE_CHR,
     +            UTILITY_TYPE
      CHARACTER*10 UTILITY_TYPE_STR,TEMP_STR_10
      CHARACTER*1 FORECAST_TYPE_CHR,COUNTRY_CHR

      INTEGER*4 REC_LENGHT
      LOGICAL*4 FILE_EXISTS
      CHARACTER*18 DATE_TIME,SYM_SYS_DATE_TIME


C
! CONVERSION FACTOR DEFININTIONS
C
      REAL GET_COST_CONVERSION,GET_HEAT_CONVERSION,GET_TONS_CONVERSION
      REAL COST_CONVERSION,HEAT_CONVERSION,TONS_CONVERSION
      SAVE COST_CONVERSION,HEAT_CONVERSION,TONS_CONVERSION
C
! FUNCTION DEFINITIONS
C
      CHARACTER*1 START_PRODUCTION_COST_METHOD
      INTEGER*2 LAST_VARIABLE
      CHARACTER*1 DSEXEC_OBJECT
      INTEGER*2 PRODUCTION_PERIODS
      INTEGER*4 EXE_DATE
      INTEGER*2 GET_NUM_END_POINTS_IN_OVL_FILE,END_POINTS
      CHARACTER*16 SET_RUN_DATE_TIME,RUN_DATE_TIME,GET_RUN_DATE_TIME
      CHARACTER*28 COMPANY_NAME,TITLE*40
      LOGICAL*1 VOID_LOGICAL,SET_DELTA_X_VALUE
      LOGICAL*1 SHORT_FORM
      CHARACTER*1 FORECAST_TYPE,COUNTRY
      INTEGER*4 NUM_ARGS
      CHARACTER*256 SHORT_FORM_FILE_NAME
      CHARACTER*64 ARGUMENTS(11)
      CHARACTER*32 SHORT_FORM_TITLE/' '/
      CHARACTER*8 SHORT_FORM_NAME ! CHANGED 12/6/93.GAT
      CHARACTER*12 SHORT_FORM_DES_NAME
C
      SAVE ESC_DISP_ADDER_ACTIVE,
     +     COMPANY_NAME_STR,TITLE_STR,
     +     PRODUCTION_COST_METHOD_CHR,UTILITY_TYPE_CHR,
     +     FORECAST_TYPE_CHR,COUNTRY_CHR,RESULTS_DIRECTORY,
     +     END_POINTS,REC_LENGHT,
!    +     STORED_PERIOD,
     +     PRODUCTION_PERIODS_IN,
     +     RUN_DATE_TIME
C
! PLANNING SWITCH INFORMATION
C
      CHARACTER*4 STR_BASE_YEAR,STR_END_YEAR,STR_EXTENSION_PERIOD
      CHARACTER*1 STR_PRODUCTION_PERIODS/" "/,
     +            START_PRODUCTION_PERIODS
      CHARACTER*64 OBJECTIVE_FUNC_POINTER
      SAVE SHORT_FORM_NAME
      CHARACTER*20 SIMULATION_MODEL
      CHARACTER*5 VERSION
c
! ISOLATE THIS ROUTINES VARIABLES
c
      REAL VOID_REAL,SET_HEAT_FACTORS_CLASS_GROWTH
      LOGICAL*1 SAVE_EXTERNAL_PRODUCTION_COST
      LOGICAL*1 SET_STUDY_PARAMS_LOGICAL
      INTEGER*2 POINTS_IN_LOAD_CURVE
      LOGICAL*1 SET_UTILITY_IDENTIFIER
      LOGICAL*1 ORLANDO_UC,SALT_RIVER_PROJECT
      CHARACTER*256 OUTPUT_DIRECTORY
      CHARACTER*2 ACTIVE_DATA_DRIVE
      CHARACTER*1 VOID_CHR1,CHECK_CAP_PLANNING_SWTCHES
      LOGICAL*1 LAHEY_LF95
      INTEGER*1 F7/z"f7"/
      CHARACTER*256 IOS_ERROR_MESSAGE
      LOGICAL (KIND=1) :: SP_CAPEX_ACTIVE
C
         DSEXEC_OBJECT = 'X'
         SIMULATION_MODEL = 'MIDAS Gold '//VERSION()
         COUNTRY_CHR = 'U'
         SHORT_FORM_CHR = 'F'
         STUDY_DESCRIPTION = ' '
C
! DETERMINE OUTPUT DIRECTORY
C
         CALL CURDIR(" ",RESULTS_DIRECTORY)
         DIREC_LEN = LEN(trim(RESULTS_DIRECTORY))
         IF(DIREC_LEN >= 3 .AND.
     +              RESULTS_DIRECTORY(DIREC_LEN:DIREC_LEN) /= '\')
     +                RESULTS_DIRECTORY = trim(RESULTS_DIRECTORY)//'\'
C
         FILE_NAME = trim(RESULTS_DIRECTORY)//'MSGPARAM.VAL'
         OPEN(10,FILE=FILE_NAME,ACCESS="SEQUENTIAL",STATUS="OLD",
     +                                                       IOSTAT=IOS)
         IF(IOS /= 0) THEN
            CALL IOSTAT_MSG(IOS,IOS_ERROR_MESSAGE)
            CALL CRITICAL_ERROR_MSG(2) !
         ENDIF
         READ(10,"(A)") DSEXEC_LINE
         CLOSE(10)
         IF(DSEXEC_LINE(1:1) /= '9') THEN
            STR_EXTENSION_PERIOD = '0000'
            DSEXEC_LINE = trim(DSEXEC_LINE)//',,,,,,,,,,,,,,,,,,,,,,,'
            READ(DSEXEC_LINE,*,ERR=9000) COMPANY_NAME_STR,TITLE_STR,
     +                                   STR_BASE_YEAR,
     +                                   PRODUCTION_COST_METHOD_CHR,
     +                                   STR_END_YEAR,
     +                                   STR_PRODUCTION_PERIODS,
     +                                   FORECAST_TYPE_CHR,
     +                                   BASE_FILE_DIRECTORY,
     +                                   UTILITY_TYPE_STR,
     +                                   COUNTRY_CHR,
     +                                   ESC_DISP_ADDER_CHR,
     +                                   SHORT_FORM_CHR,
     +                                   SHORT_FORM_NAME,
     +                                   STR_EXTENSION_PERIOD,
     +                                   SHORT_FORM_TITLE,
     +                                   STUDY_DESCRIPTION,
     +                                   LDE_FILE_DIRECTORY,
     +                                   PRB_FILE_DIRECTORY,
     +                                   SHB_FILE_DIRECTORY,
     +                                   PRJ_FILE_DIRECTORY,
     +                                   XML_FILE_DIRECTORY,
     +                                   ENRG_PATTERN_FILE_DIRECTORY,
     +                                   WORKING_FILE_DIRECTORY,
     +                                   LDG_FILE_DIRECTORY

            IF(trim(STUDY_DESCRIPTION) /= ' ')
     +                                     TITLE_STR = STUDY_DESCRIPTION
C
! PROCESS AND ADJUST THE READ INFORMATION
C
            IF(trim(STR_BASE_YEAR) /= ' ') THEN
               READ(STR_BASE_YEAR,*) STORED_BASE_YEAR
            ELSE
               STORED_BASE_YEAR = 2002
            ENDIF
            IF(trim(STR_END_YEAR) /= ' ') THEN
               READ(STR_END_YEAR,*) STORED_END_YEAR
            ELSE
               STORED_END_YEAR = 2010
            ENDIF
            STUDY_PERIOD = MAX(1,STORED_END_YEAR-STORED_BASE_YEAR)

            IF(trim(STR_EXTENSION_PERIOD) /= ' ') THEN
               READ(STR_EXTENSION_PERIOD,*) EXTENSION_PERIOD
            ELSE
               EXTENSION_PERIOD = 0
            ENDIF
            IF(COUNTRY_CHR == 'M') THEN
               COUNTRY_CHR = 'C'
            ELSE
               COUNTRY_CHR = 'U'
            ENDIF
C
! SET THE UTILITY ID INFORMATION 10/15/00 MSG
C
            VOID_LOGICAL = SET_UTILITY_IDENTIFIER(UTILITY_TYPE_STR,
     +                                            UTILITY_TYPE_CHR)
C
! CALCULATE AND PASS THE LOGICAL FUNCTION PARAMETERS
C
            SAVE_EXTERNAL_PRODUCTION_COST=UTILITY_TYPE_CHR == 'T' .AND.
     +                                        UTILITY_TYPE_STR(5:5)=='P'
            ESC_DISP_ADDER_ACTIVE=.NOT.((ESC_DISP_ADDER_CHR == 'N').OR.
     +                                  (ESC_DISP_ADDER_CHR == 'n'))
            VOID_LOGICAL = SET_STUDY_PARAMS_LOGICAL(FORECAST_TYPE_CHR,
     +                                    SAVE_EXTERNAL_PRODUCTION_COST,
     +                                    ESC_DISP_ADDER_ACTIVE)
            VOID_REAL =  SET_HEAT_FACTORS_CLASS_GROWTH(COUNTRY_CHR)
C
C
C
            STORED_PROCMETH =
     +                 SET_PRODUCTION_METHOD(PRODUCTION_COST_METHOD_CHR)
            PRODUCTION_PERIODS_IN = 12
            IF(STR_PRODUCTION_PERIODS == 'A') then
                PRODUCTION_PERIODS_IN = 1
            endif
            
            call SET_PRODUCTION_PERIODS(STR_PRODUCTION_PERIODS)
C
! ADDED 3/9/93
C
            IF(REALLY_KEPCO) THEN
               NUMBER_OF_AREAS = 5
            ELSEIF(WABASH_VALLEY) THEN
               NUMBER_OF_AREAS = 6
            ELSE
               NUMBER_OF_AREAS = 6
            ENDIF
! END UTILTY ID
! MOVED 3/17/94. GAT.
            IF(EXTENSION_PERIOD > STORED_END_YEAR) THEN
                LAST_EXTENSION_YEAR =
     +                 MIN(EXTENSION_PERIOD,
     +                           STORED_BASE_YEAR + MAX_PLANNING_YEARS)
                EXTENSION_PERIOD = LAST_EXTENSION_YEAR - STORED_END_YEAR
            ELSEIF(EXTENSION_PERIOD >= 1900) THEN
                LAST_EXTENSION_YEAR = STORED_BASE_YEAR
                EXTENSION_PERIOD = 0
            ELSE
               EXTENSION_PERIOD = MIN(EXTENSION_PERIOD,
     +                             MAX_PLANNING_YEARS-STUDY_PERIOD)
                LAST_EXTENSION_YEAR = STORED_END_YEAR + EXTENSION_PERIOD
            ENDIF
C
! 01/04/07 STOP CHECK ON THE EXTENSION PERIOD STARTING BEFORE THE 30TH STUDY YEAR.
! DrG
C
            IF(EXTENSION_PERIOD > 0 .AND. STUDY_PERIOD < 30) THEN
               CALL write_scroll_line_RW(
     +               "In order to use the Extension Period "//
     +               "the Study Period MUST BE 30 years.",5)
               WRITE(IOS_ERROR_MESSAGE,'(A,I2,A)')
     +              "The Study Period length is ",STUDY_PERIOD," years."
               CALL write_scroll_line_RW(IOS_ERROR_MESSAGE,5)

               er_message='Stop requested from Dsex_obj SIID78'
               call end_program(er_message)
            ENDIF
C
! CHECK FOR SHORT FORM FILE
C
            SHORT_FORM_NAME = 'MSG'//trim(SHORT_FORM_NAME)
            SHORT_FORM_FILE_NAME = trim(RESULTS_DIRECTORY)//
     +                                   trim(SHORT_FORM_NAME)//'.VRS'
            SHORT_FORM_DES_NAME = trim(SHORT_FORM_NAME)//'.DSS'
            INQUIRE(FILE=SHORT_FORM_FILE_NAME,EXIST=FILE_EXISTS)
         ELSE
            CALL CRITICAL_ERROR_MSG(4) ! MSGPARAM.VAL doesn't exist
         ENDIF
C
! CHECK FOR VALID BASE_FILE_DIRECTORY
C
         cldata%Scename=check_scename()

         CALL VERIFY_BASE_FILE_DIRECTORY(BASE_FILE_DIRECTORY,
     +                                   RESULTS_DIRECTORY,
     +                                   LDE_FILE_DIRECTORY,
     +                                   PRB_FILE_DIRECTORY,
     +                                   SHB_FILE_DIRECTORY,
     +                                   XML_FILE_DIRECTORY,
     +                                   LDG_FILE_DIRECTORY)
!
         END_POINTS = GET_NUM_END_POINTS_IN_OVL_FILE()
C
         IF(PRODUCTION_PERIODS_IN < 1 .OR. PRODUCTION_PERIODS_IN > 12)
     +                                        PRODUCTION_PERIODS_IN = 12

         IF(STORED_BASE_YEAR >= STORED_END_YEAR)
     +                                        CALL CRITICAL_ERROR_MSG(5)
         MAX_YEARS = MAX_SIMULATION_YEARS + 1

         STORED_END_YEAR = STORED_BASE_YEAR + STUDY_PERIOD
         LAST_STUDY_YEAR = STORED_END_YEAR
C
! THE FOLLOWING REPLACES THE CHECK_DISC_SPACE SUBROUTINE
C
C
! PASSED ALL CRITICAL TEST EXCEPT DISC SPACE
C
! DETERMINE LAST VARIABLE
C
         IF(SHORT_FORM_ACTIVE) THEN
            ACTIVE_VARIABLES = SET_SHORT_FORM_VARIABLES(
     +                                             SHORT_FORM_FILE_NAME)
            DES_FILE_NAME = trim(SHORT_FORM_DES_NAME)
         ELSE
            ACTIVE_VARIABLES = get_last_variable() + 1
            DES_FILE_NAME = 'DSSBIFP.DES'
         ENDIF
         CALL SAVE_SHORT_FORM_ACTIVE(SHORT_FORM_ACTIVE)
C
         VOID_CHR1 = CHECK_CAP_PLANNING_SWTCHES()
C
! OPEN WARNING MESSAGE FILE AND CLEAR BEFORE CHECKING DISK SPACE
C
         IF(.NOT. SP_CAPEX_ACTIVE())
     +    CALL DELETE_RESULTS_OF_PREVIOUS_RUN()
C
! OPEN ASCII REPORTS FILES
C
         FILE_NAME=" "
         FILE_NAME = trim(RESULTS_DIRECTORY)//"MSG"//
     +       trim(cldata%SCENAME)//".RNS"
       call write_log_entry("dsex:0023", 
     + "Opening " // trim(file_name) // "...")
     
         OPEN(9,FILE=FILE_NAME,CARRIAGE CONTROL="FORTRAN",RECL=132,
     +                                                 STATUS='REPLACE')
         WRITE(9,*) ' '
C
         FILE_NAME = trim(RESULTS_DIRECTORY)//"MSG"//
     +        trim(cldata%SCENAME)//".ERR"
       call write_log_entry("dsex:0024", 
     + "Opening " // trim(file_name) // "...")
     
         OPEN(4,FILE=FILE_NAME,CARRIAGE CONTROL="FORTRAN",
     +                                         STATUS='REPLACE')
         WRITE(4,*) ' '
C
! FIND SIMULATION MODEL NAME AND DATE TIME STAMP
C
         CALL MYNAME(FILE_NAME)
         SYM_SYS_DATE_TIME = DATE_TIME(FILE_NAME)
         RUN_DATE_TIME = SET_RUN_DATE_TIME()
C
! OPEN RESULT FILE AND WRITE HEADER DATA
C
         REC_LENGHT = MAX((ACTIVE_VARIABLES+3)*4,256)
         FILE_NAME = trim(RESULTS_DIRECTORY)//"BIP"//
     +      trim(cldata%SCENAME)//".BIN"
         IF(LAHEY_LF95()) THEN
            OPEN(38,FILE=FILE_NAME,ACCESS="DIRECT",STATUS='REPLACE',
     +                                                  RECL=REC_LENGHT)
            WRITE(38,REC=1) F7,INT2(REC_LENGHT),
     +                      END_POINTS,STORED_BASE_YEAR,
     +                      LAST_EXTENSION_YEAR,
     +                      COMPANY_NAME_STR,TITLE_STR,RUN_DATE_TIME,
     +                      UTILITY_TYPE_CHR,
     +                      STORED_PROCMETH,
     +                      FORECAST_TYPE_CHR,
     +                      PRODUCTION_PERIODS_IN,
     +                      COUNTRY_CHR,
     +                      SHORT_FORM_ACTIVE,
     +                      DES_FILE_NAME,
     +                      ENDING_MONTHS,
     +                      ESC_DISP_ADDER_CHR,
     +                      SIMULATION_MODEL,
     +                      SYM_SYS_DATE_TIME,
     +                      ACTIVE_VARIABLES-1
         ELSE
            OPEN(38,FILE=FILE_NAME,ACCESS="DIRECT",STATUS="REPLACE",
     +                                                  RECL=REC_LENGHT)
            CLOSE(38)

            OPEN(38,FILE=FILE_NAME,ACCESS="TRANSPARENT",STATUS="OLD")
            WRITE(38,REC=4) END_POINTS,STORED_BASE_YEAR,
     +                      LAST_EXTENSION_YEAR,
     +                      COMPANY_NAME_STR,TITLE_STR,RUN_DATE_TIME,
     +                      UTILITY_TYPE_CHR,
     +                      STORED_PROCMETH,
     +                      FORECAST_TYPE_CHR,
     +                      PRODUCTION_PERIODS_IN,
     +                      COUNTRY_CHR,
     +                      SHORT_FORM_ACTIVE,
     +                      DES_FILE_NAME,
     +                      ENDING_MONTHS,
     +                      ESC_DISP_ADDER_CHR,
     +                      SIMULATION_MODEL,
     +                      SYM_SYS_DATE_TIME,
     +                      ACTIVE_VARIABLES-1
            CLOSE(38)
            OPEN(38,FILE=FILE_NAME,ACCESS="DIRECT",STATUS="OLD",
     +                                                  RECL=REC_LENGHT)
         ENDIF
C
! OPEN LOAD PROB FILE
C
         OPEN(8800,FILE="MIDAS880.BIN",ACCESS="DIRECT",STATUS='REPLACE',
     +                                     RECL=40+12*LOAD_CURVE_POINTS)
      RETURN
C***********************************************************************
      ENTRY GET_RUN_DATE_TIME
C***********************************************************************
         GET_RUN_DATE_TIME = RUN_DATE_TIME
      RETURN
C***********************************************************************
      ENTRY COMPANY_NAME
         COMPANY_NAME = COMPANY_NAME_STR
      RETURN
C***********************************************************************
      ENTRY TITLE
         TITLE = TITLE_STR
      RETURN
C***********************************************************************
      ENTRY UTILITY_TYPE
         UTILITY_TYPE = UTILITY_TYPE_CHR
      RETURN
C***********************************************************************
      ENTRY FORECAST_TYPE
         FORECAST_TYPE = FORECAST_TYPE_CHR
      RETURN
C***********************************************************************
      ENTRY COUNTRY
         COUNTRY = COUNTRY_CHR
      RETURN
C***********************************************************************
      ENTRY ACTIVE_DATA_DRIVE
         ACTIVE_DATA_DRIVE = RESULTS_DIRECTORY(1:1)//':'
      RETURN
C***********************************************************************
      ENTRY OUTPUT_DIRECTORY
         OUTPUT_DIRECTORY = RESULTS_DIRECTORY
      RETURN
C***********************************************************************
      ENTRY GET_WORKING_FILE_DIRECTORY
         GET_WORKING_FILE_DIRECTORY = WORKING_FILE_DIRECTORY
      RETURN
C***********************************************************************
      ENTRY START_PRODUCTION_COST_METHOD
         START_PRODUCTION_COST_METHOD = PRODUCTION_COST_METHOD_CHR
      RETURN
 9000 CALL CRITICAL_ERROR_MSG(4) ! BAD MSGPARAM.VAL FILE
      END
C***********************************************************************
      FUNCTION DATE_TIME(R_FILE_NAME)
      use logging
      use report_header
C***********************************************************************
      INCLUDE 'DREPTCOM.MON'
      LOGICAL*1 LAHEY_LF95
      CHARACTER*18 DATE_TIME,SYM_SYS_DATE_TIME,S_SYM_SYS_DATE_TIME
      CHARACTER*5 MONTH_DAY
      CHARACTER*256 R_FILE_NAME,RECLN
      CHARACTER*120 SYSTEM_CMD
      LOGICAL*4 FILE_EXISTS
      INTEGER*4 R_EXE_DATE,EXE_TIME,SIZE
      INTEGER*4 VOID_INT4,STORE_SIM_SYSTEM_DATE
      INTEGER*4 HOURS,MINUTES,MONTH,DAY,YEAR,IOS,POS
      CHARACTER*9 TIME,DATE
      SAVE R_EXE_DATE,S_SYM_SYS_DATE_TIME
      CHARACTER*16 SET_RUN_DATE_TIME
      CHARACTER*2 MONTH_STR,DAY_STR,
     +            LEFT_JUSTIFY_I2_IN_STR*15
      CHARACTER*8 DAT,STORED_RUN_DATE,STORED_RUN_TIME
      CHARACTER*11 CLK

      INTEGER*2 HOUR
      CHARACTER*1 STR_PRODUCTION_PERIODS/" "/,
     +            START_PRODUCTION_PERIODS
      SAVE STORED_RUN_DATE
C
         CALL FILE_INQUIRE(R_FILE_NAME,FILE_EXISTS,
     +                                         R_EXE_DATE,EXE_TIME,SIZE)
         IF(LAHEY_LF95()) THEN
            CALL SEC2DATE(R_EXE_DATE,YEAR,MONTH,DAY) ! ,cHour,cMinute,cSecond)
            IF(YEAR >= 2000) YEAR = YEAR - 2000
            IF(YEAR >= 1900) YEAR = YEAR - 1900
            TIME = ' '
         ELSE
            HOURS = IAND(63488,EXE_TIME)/2048
            MINUTES = IAND(2016,EXE_TIME)/32
            MONTH = IAND(480,R_EXE_DATE)/32
            DAY = IAND(31,R_EXE_DATE)
            YEAR = IAND(65024,R_EXE_DATE)/512 + 80
            IF(YEAR >= 100) YEAR = YEAR - 100
            IF(HOURS < 12) THEN
               IF(HOURS == 0) HOURS = 12
               WRITE(TIME,'(1X,I2,":",I2.2," am")') HOURS,MINUTES
            ELSE
               IF(HOURS /= 12) HOURS = HOURS - 12
               WRITE(TIME,'(1X,I2,":",I2.2," pm")') HOURS,MINUTES
            ENDIF
         ENDIF
         WRITE(DATE,'(1X,I2.2,"/",I2.2,"/",I2.2)') MONTH,DAY,YEAR
         S_SYM_SYS_DATE_TIME = DATE//TIME
         DATE_TIME = DATE//TIME
         VOID_INT4 = STORE_SIM_SYSTEM_DATE(R_EXE_DATE)
      RETURN
C***********************************************************************
      ENTRY MONTH_DAY(R_FILE_NAME)
C***********************************************************************
         CALL FILE_INQUIRE(R_FILE_NAME,FILE_EXISTS,
     +                                         R_EXE_DATE,EXE_TIME,SIZE)
         IF(LAHEY_LF95()) THEN
            CALL SEC2DATE(R_EXE_DATE,YEAR,MONTH,DAY) ! ,cHour,cMinute,cSecond)
         ELSE
            MONTH = IAND(480,R_EXE_DATE)/32
            DAY = IAND(31,R_EXE_DATE)
         ENDIF
         DAY_STR = LEFT_JUSTIFY_I2_IN_STR(INT2(DAY))
         MONTH_STR = LEFT_JUSTIFY_I2_IN_STR(INT2(MONTH))
         MONTH_DAY = trim(MONTH_STR)//"/"//trim(DAY_STR)
      RETURN
C***********************************************************************
      ENTRY SYM_SYS_DATE_TIME
C***********************************************************************
         SYM_SYS_DATE_TIME = S_SYM_SYS_DATE_TIME
      RETURN
C***********************************************************************
      ENTRY SET_RUN_DATE_TIME
C***********************************************************************
         STORED_RUN_DATE = DAT()
         IF(STORED_RUN_DATE(1:1) == '0') STORED_RUN_DATE(1:1) = " "
         STORED_RUN_TIME(1:5) = CLK()
         READ(STORED_RUN_TIME,'(I2)') HOUR
         IF(HOUR < 12) THEN
            IF(HOUR == 0) HOUR = 12
            STORED_RUN_TIME = STORED_RUN_TIME(1:5)//' am'
         ELSE
            IF(HOUR /= 12) HOUR = HOUR - 12
            WRITE(STORED_RUN_TIME(1:2),'(I2)') HOUR
            STORED_RUN_TIME = STORED_RUN_TIME(1:5)//' pm'
         ENDIF
         IF(STORED_RUN_TIME(1:1) == '0') STORED_RUN_TIME(1:1) = " "
         SET_RUN_DATE_TIME = STORED_RUN_DATE//STORED_RUN_TIME
         RUN_DATE = STORED_RUN_DATE
         RUN_TIME = STORED_RUN_TIME
      RETURN
C***********************************************************************
      ENTRY START_PRODUCTION_PERIODS
         START_PRODUCTION_PERIODS = STR_PRODUCTION_PERIODS
      RETURN
C***********************************************************************
!      ENTRY RUN_DATE
!         RUN_DATE = STORED_RUN_DATE
!      RETURN
C***********************************************************************
!      ENTRY RUN_TIME
!         RUN_TIME = STORED_RUN_TIME
!      RETURN
      END
C***********************************************************************
      FUNCTION EXE_DATE()
      use logging
      use filename_tracker
C***********************************************************************
C
      INTEGER EXE_DATE,R_EXE_DATE,S_EXE_DATE,STORE_SIM_SYSTEM_DATE
      SAVE S_EXE_DATE
      INTEGER*2 GET_NUM_END_POINTS_IN_OVL_FILE,END_POINTS,DELETE
      INTEGER*4 IOS
      LOGICAL*4 FILE_EXISTS
      CHARACTER*256 FILE_NAME,GET_RESULTS_DIRECTORY
      CHARACTER*5 GET_SCENAME
!     INTEGER*2 STORED_PERIOD(2,5)/10*0/,PERIOD
      INTEGER*2 BEGIN_END,SEASON,GET_NUM_OF_END_POINTS,
     +          LAST_STUDY_ENDPOINT,START_STUDY_ENDPOINT
      SAVE END_POINTS
C
         EXE_DATE = S_EXE_DATE
      RETURN
      ENTRY STORE_SIM_SYSTEM_DATE(R_EXE_DATE)
         S_EXE_DATE = R_EXE_DATE
         STORE_SIM_SYSTEM_DATE = 1234
      RETURN
C***********************************************************************
!      ENTRY PERIOD(BEGIN_END,SEASON)
!         PERIOD = STORED_PERIOD(BEGIN_END,SEASON)
!      RETURN
C***********************************************************************
      ENTRY GET_NUM_END_POINTS_IN_OVL_FILE
         END_POINTS = 1
         FILE_NAME = get_scn_ovl_filename()

         INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
         IF(FILE_EXISTS) THEN
            OPEN(10,FILE=FILE_NAME,ACCESS='SEQUENTIAL')
            END_POINTS = -9999
            READ(10,'(A)') FILE_NAME
            FILE_NAME = trim(FILE_NAME)//',,,,,,,'
            READ(FILE_NAME,*) DELETE,END_POINTS
            IF(END_POINTS < 1) THEN
               IOS = 0
               DOWHILE (IOS == 0)
                  READ(10,*,IOSTAT=IOS) DELETE,END_POINTS
               ENDDO
            ENDIF
            CLOSE(10)
            END_POINTS = MAX(END_POINTS,1)
            END_POINTS = MIN(END_POINTS,
     +               LAST_STUDY_ENDPOINT() - START_STUDY_ENDPOINT() + 1)
         ENDIF
         GET_NUM_END_POINTS_IN_OVL_FILE = END_POINTS

      RETURN
C***********************************************************************
      ENTRY GET_NUM_OF_END_POINTS
C***********************************************************************
         GET_NUM_OF_END_POINTS = END_POINTS
      RETURN
      END
C***********************************************************************
      FUNCTION CHECK_CAP_PLANNING_SWTCHES()
C***********************************************************************
C
C
! GOLBAL VARIABLES
C
      use logging
      use globecom


      CHARACTER*256 FILE_NAME
      CHARACTER*40 REPORT_TITLE
      CHARACTER*1 TRIGGER_METHOD_VALUE/'X'/,
     +            PLANNING_TRIGGER,
     +            CAPACITY_PLANNING_SWTCHES,
     +            CHECK_CAP_PLANNING_SWTCHES
      CHARACTER*2 CAPACITY_PLANNING_METHOD,
     +            CAPACITY_PLANNING_METHOD_VALUE/'XX'/,
     +            GREEN_MRX_METHOD,
     +            GREEN_MRX_METHOD_VALUE/'NO'/
      CHARACTER*60 CAPACITY_PLANNING_METHOD_STR,TEMP_STR_60
      INTEGER*4 IOS
      INTEGER*2 DELETE
      INTEGER*2 VOID_INT2,SET_INTEGER_PLANNING_SWITCHES
      INTEGER*2 OPTIM_DEPTH_VALUE/1/,
     +          OPTIM_DEPTH,
     +          MAX_PLANS_TO_SAVE_VALUE/1/,
     +          MAX_PLANS_TO_SAVE,
     +          ISERROR
      CHARACTER*4 STR_MIN_MAX_VALUE/'MIN '/,
     +            OBJ_FUNCTION_MIN_MAX
      LOGICAL*1 CAPACITY_PLANNING_ACTIVE
      CHARACTER*50 OBJ_FUNCTION_DESC,
     +             OBJ_FUNCTION_DESC_STR/" "/
      CHARACTER*256 :: OBJ_FUNCTION_FILE_NAME_STR=" ",
     +              OBJ_FUNCTION_FILE_NAME
      CHARACTER*8 PEAK_GROWTH_RATE_METHOD_VALUE/'Forecast'/,
     +            PEAK_GROWTH_RATE_METHOD
C
! LOCAL VARIABLES
C
      CHARACTER*128 SWITCH_LINE
      CHARACTER*1 AN_DECOMP_DEBUG_STR,AN_DECOMP_DEBUG_SWITCH
      SAVE AN_DECOMP_DEBUG_STR
C
! FUNCTION TYPES
C
      CHARACTER*1 FORECAST_TYPE,COUNTRY,UTILITY_TYPE
C
!         CALL FINDFILE('MSGPLSWT.DAT',FILE_NAME)
         OPEN(10,FILE='MSGPLSWT.DAT',ACCESS="SEQUENTIAL",STATUS="OLD",
     +                                                       IOSTAT=IOS)
         IF(IOS /= 0) THEN
            REPORT_TITLE = 'Planning not active.'
            CAPACITY_PLANNING_METHOD_VALUE = 'RE'
            TRIGGER_METHOD_VALUE = 'R'
            OPTIM_DEPTH_VALUE = 1
            STR_MIN_MAX_VALUE = 'MIN'
            MAX_PLANS_TO_SAVE_VALUE = 1
            LAST_EXTENSION_YEAR = STORED_END_YEAR
!            EXTENSION_PERIOD = 0
            OBJ_FUNCTION_FILE_NAME_STR = 'NONE'
            OBJ_FUNCTION_DESC_STR = 'Default PV Revenues at 10%'
         ELSE
!            OPEN(10,FILE=FILE_NAME,ACCESS="SEQUENTIAL")
            READ(10,*)
            DO
               READ(10,"(A)") SWITCH_LINE
               SWITCH_LINE = trim(SWITCH_LINE)//',,,,,,,,,,,,,,,,,'
               READ(SWITCH_LINE,*,IOSTAT=IOS) DELETE,
     +                             TEMP_STR_60,
     +                             TRIGGER_METHOD_VALUE,
     +                             OPTIM_DEPTH_VALUE,
     +                             STR_MIN_MAX_VALUE,
     +                             MAX_PLANS_TO_SAVE_VALUE,
     +                             OBJ_FUNCTION_FILE_NAME_STR,
     +                             OBJ_FUNCTION_DESC_STR,
     +                             AN_DECOMP_DEBUG_STR
               IF(IOS /=0) EXIT
               IF(DELETE < 8) EXIT
            ENDDO
            CLOSE(10)
            CALL UPC(TEMP_STR_60,CAPACITY_PLANNING_METHOD_STR)
            CAPACITY_PLANNING_METHOD_VALUE =
     +                                 CAPACITY_PLANNING_METHOD_STR(1:2)
! 010609. GREATLY SIMPLIFIES BRANCHING
            IF(CAPACITY_PLANNING_METHOD_VALUE == 'GX') THEN
               GREEN_MRX_METHOD_VALUE = 'GX'
               CAPACITY_PLANNING_METHOD_VALUE = 'MX'
            ELSE
               GREEN_MRX_METHOD_VALUE = 'NO'
            ENDIF
!
            PEAK_GROWTH_RATE_METHOD_VALUE = 'Forecast'
            IF(CAPACITY_PLANNING_METHOD_VALUE == 'NE') THEN
               IF(INDEX(CAPACITY_PLANNING_METHOD_STR,'FILE') /= 0)
     +                          PEAK_GROWTH_RATE_METHOD_VALUE = 'Ratios'
               IF(INDEX(CAPACITY_PLANNING_METHOD_STR,'AVERAGE') /= 0)
     +                         PEAK_GROWTH_RATE_METHOD_VALUE = 'Average'
            ENDIF
C
! 3/17/94. GAT. MOVED EXTENSION PERIOD INTO .VAL FILE
C
C
! MOVED TO LINE 440 IN CN_OBJECT 3/7/94
C
!           IF(CAPACITY_PLANNING_METHOD_VALUE == 'AN' .AND.
!    +                                  CAPACITY_PLANNING_ACTIVE()) THEN
!              VOID_LOGICAL = AN_DECOMP_REPORT_HEADER()
!           ENDIF
         ENDIF
         VOID_INT2 =  SET_INTEGER_PLANNING_SWITCHES(STUDY_PERIOD,
     +                                          OPTIM_DEPTH_VALUE,
     +                                          MAX_PLANS_TO_SAVE_VALUE,
     +                                          EXTENSION_PERIOD,
     +                                          STORED_END_YEAR,
     +                                          STORED_BASE_YEAR)
         CHECK_CAP_PLANNING_SWTCHES = CAPACITY_PLANNING_METHOD_VALUE
      RETURN
C

C***********************************************************************
      ENTRY AN_DECOMP_DEBUG_SWITCH
C***********************************************************************
         AN_DECOMP_DEBUG_SWITCH = AN_DECOMP_DEBUG_STR
      RETURN
C***********************************************************************
      ENTRY CAPACITY_PLANNING_SWTCHES
C***********************************************************************
         CAPACITY_PLANNING_SWTCHES = CAPACITY_PLANNING_METHOD_VALUE
      RETURN
C***********************************************************************
      ENTRY PEAK_GROWTH_RATE_METHOD
C***********************************************************************
         PEAK_GROWTH_RATE_METHOD = PEAK_GROWTH_RATE_METHOD_VALUE
      RETURN
C***********************************************************************
      ENTRY GREEN_MRX_METHOD
C***********************************************************************
         GREEN_MRX_METHOD = GREEN_MRX_METHOD_VALUE
      RETURN
C***********************************************************************
      ENTRY CAPACITY_PLANNING_METHOD
C***********************************************************************
         CAPACITY_PLANNING_METHOD = CAPACITY_PLANNING_METHOD_VALUE
      RETURN
C***********************************************************************
      ENTRY OBJ_FUNCTION_FILE_NAME
C***********************************************************************
         OBJ_FUNCTION_FILE_NAME = OBJ_FUNCTION_FILE_NAME_STR
      RETURN
C***********************************************************************
      ENTRY OBJ_FUNCTION_DESC
C***********************************************************************
         OBJ_FUNCTION_DESC = OBJ_FUNCTION_DESC_STR
      RETURN
C***********************************************************************
      ENTRY OBJ_FUNCTION_MIN_MAX
C***********************************************************************
         OBJ_FUNCTION_MIN_MAX = STR_MIN_MAX_VALUE
      RETURN
C***********************************************************************
!     ENTRY PLANNING_TRIGGER ! THIS VALUE IS SPECIFIED IN RUN SPECS
C***********************************************************************
!        PLANNING_TRIGGER = TRIGGER_METHOD_VALUE
!     RETURN
      END
C***********************************************************************
      FUNCTION SET_INTEGER_PLANNING_SWITCHES(R_STUDY_PERIOD,
     +                                       R_OPTIM_DEPTH_VALUE,
     +                                       R_MAX_PLANS_TO_SAVE_VALUE,
     +                                       R_EXTENSION_PERIOD,
     +                                       R_STORED_END_YEAR,
     +                                       R_STORED_BASE_YEAR)
C***********************************************************************
C
      use logging
      INTEGER*2 SET_INTEGER_PLANNING_SWITCHES
      INTEGER*2 EXTENSION_PERIOD_START,STUDY_PERIOD
      INTEGER*2 RUN_YEARS,EXTENSION_YEARS,EXTENSION_PERIOD
      INTEGER*2 BASE_YEAR,STORED_BASE_YEAR
      INTEGER*2 ENDYR,STORED_END_YEAR
      INTEGER*2 OPTIM_DEPTH,OPTIM_DEPTH_VALUE
      INTEGER*2 MAX_PLANS_TO_SAVE,MAX_PLANS_TO_SAVE_VALUE
      INTEGER*2 R_STUDY_PERIOD,
     +          R_OPTIM_DEPTH_VALUE,
     +          R_MAX_PLANS_TO_SAVE_VALUE,
     +          R_EXTENSION_PERIOD,
     +          R_STORED_END_YEAR,
     +          R_STORED_BASE_YEAR
      SAVE STUDY_PERIOD,OPTIM_DEPTH_VALUE,MAX_PLANS_TO_SAVE_VALUE,
     +     EXTENSION_PERIOD,STORED_END_YEAR,STORED_BASE_YEAR
         STUDY_PERIOD = R_STUDY_PERIOD
         OPTIM_DEPTH_VALUE = R_OPTIM_DEPTH_VALUE
         MAX_PLANS_TO_SAVE_VALUE = R_MAX_PLANS_TO_SAVE_VALUE
         EXTENSION_PERIOD = R_EXTENSION_PERIOD
         STORED_END_YEAR = R_STORED_END_YEAR
         STORED_BASE_YEAR = R_STORED_BASE_YEAR
      RETURN
C***********************************************************************
      ENTRY EXTENSION_PERIOD_START
         EXTENSION_PERIOD_START = MIN(31,STUDY_PERIOD+1)
         EXTENSION_PERIOD_START = STUDY_PERIOD+1  ! 11/09/06
      RETURN
C***********************************************************************
      ENTRY OPTIM_DEPTH
C***********************************************************************
         OPTIM_DEPTH = OPTIM_DEPTH_VALUE
      RETURN
C***********************************************************************
      ENTRY MAX_PLANS_TO_SAVE
C***********************************************************************
         MAX_PLANS_TO_SAVE = MAX_PLANS_TO_SAVE_VALUE
      RETURN
C***********************************************************************
      ENTRY BASE_YEAR
C***********************************************************************
         BASE_YEAR = STORED_BASE_YEAR
      RETURN
C***********************************************************************
      ENTRY ENDYR
C***********************************************************************
         ENDYR = STORED_END_YEAR
      RETURN
C***********************************************************************
      ENTRY RUN_YEARS
C***********************************************************************
         RUN_YEARS = STUDY_PERIOD
      RETURN
C***********************************************************************
      ENTRY EXTENSION_YEARS()
C***********************************************************************
         EXTENSION_YEARS = EXTENSION_PERIOD
      RETURN
      END
C***********************************************************************
      FUNCTION SET_STUDY_PARAMS_LOGICAL(R_FORECAST_TYPE_CHR,
     +                                  R_SAVE_EXTERNAL_PRODUCTION_COST,
     +                                  R_ESC_DISP_ADDER_ACTIVE)
C***********************************************************************
C
      use logging
      use forecast
      LOGICAL*1 SET_STUDY_PARAMS_LOGICAL
      LOGICAL*1 SET_RUN_FUTURE_ASSETS,
     +          DETAIL_ANNUAL_DECOMP_REPORT,
     +          FIXED_COSTS_IN_ADJ_CLAUSE
      LOGICAL*1 
     +          NERC_REGION_BASED_FORECAST,
     +          POOLING_TRANSACTIONS,
     +          CONTROL_AREA_FORECAST,
     +          CLASS_BASED_FORECAST
      LOGICAL*1 TURN_OFF_SYSTEM_BASE_FORECAST
      LOGICAL*1 ESC_DISP_ADDER,SHORT_FORM,
     +          USE_EXTERNAL_PRODUCTION_COST
      CHARACTER*1 R_FORECAST_TYPE_CHR,
     +            AN_DECOMP_DEBUG_SWITCH
      LOGICAL*1 MIN_MAX_OBJECTIVE_FUNC
      LOGICAL*1 R_SAVE_EXTERNAL_PRODUCTION_COST,
     +          R_ESC_DISP_ADDER_ACTIVE
      LOGICAL*1 LAST_ELDC_REPORT,MARGINAL_COST_SWITCH
      CHARACTER*1 UTILITY_TYPE
      CHARACTER*4 OBJ_FUNCTION_MIN_MAX
      INTEGER*2 GET_NUM_OF_END_POINTS
C
! SAVED VARAIABLES THAT MAKE THE FUNCTIONS WORK
C
      LOGICAL*1 SHORT_FORM_ACTIVE/.FALSE./,
     +          ESC_DISP_ADDER_ACTIVE,
     +          SAVE_EXTERNAL_PRODUCTION_COST/.FALSE./
      SAVE ESC_DISP_ADDER_ACTIVE
C
         FORECAST_TYPE_CHR = R_FORECAST_TYPE_CHR
         SAVE_EXTERNAL_PRODUCTION_COST = R_SAVE_EXTERNAL_PRODUCTION_COST,
         ESC_DISP_ADDER_ACTIVE = R_ESC_DISP_ADDER_ACTIVE
         SET_STUDY_PARAMS_LOGICAL = .TRUE.
      RETURN
C***********************************************************************
      ENTRY USE_EXTERNAL_PRODUCTION_COST
C***********************************************************************
         USE_EXTERNAL_PRODUCTION_COST = SAVE_EXTERNAL_PRODUCTION_COST
      RETURN
C***********************************************************************
      ENTRY SHORT_FORM
C***********************************************************************
         SHORT_FORM = SHORT_FORM_ACTIVE ! .FALSE.
      RETURN
C***********************************************************************
      ENTRY ESC_DISP_ADDER
C***********************************************************************
         ESC_DISP_ADDER = ESC_DISP_ADDER_ACTIVE
      RETURN
C***********************************************************************
      ENTRY MIN_MAX_OBJECTIVE_FUNC
C***********************************************************************
         MIN_MAX_OBJECTIVE_FUNC=INDEX(OBJ_FUNCTION_MIN_MAX(),'MIN') /= 0
      RETURN
C***********************************************************************
      ENTRY FIXED_COSTS_IN_ADJ_CLAUSE
         FIXED_COSTS_IN_ADJ_CLAUSE = .TRUE.
      RETURN
C***********************************************************************

C***********************************************************************
      ENTRY TURN_OFF_SYSTEM_BASE_FORECAST
C***********************************************************************
         SYSTEM_FORECAST_FILE_IN_USE = .FALSE.
         TURN_OFF_SYSTEM_BASE_FORECAST = .FALSE.
      RETURN
C***********************************************************************
      ENTRY NERC_REGION_BASED_FORECAST
C***********************************************************************
         NERC_REGION_BASED_FORECAST = (
     +         (FORECAST_TYPE_CHR == 'S' .OR.
     +                        FORECAST_TYPE_CHR == 'A') .AND.
     +                                .NOT. SYSTEM_FORECAST_FILE_IN_USE)
      RETURN
C***********************************************************************
      ENTRY POOLING_TRANSACTIONS
C***********************************************************************
         POOLING_TRANSACTIONS  = (FORECAST_TYPE_CHR == 'P')
      RETURN
C***********************************************************************
      ENTRY CONTROL_AREA_FORECAST
C***********************************************************************
         CONTROL_AREA_FORECAST  = (FORECAST_TYPE_CHR == 'A')
      RETURN
C***********************************************************************
      ENTRY CLASS_BASED_FORECAST
C***********************************************************************
         CLASS_BASED_FORECAST = (FORECAST_TYPE_CHR == 'C')
      RETURN
C***********************************************************************
      ENTRY DETAIL_ANNUAL_DECOMP_REPORT
C***********************************************************************
         DETAIL_ANNUAL_DECOMP_REPORT = AN_DECOMP_DEBUG_SWITCH() == 'Y'
      RETURN
C***********************************************************************
      ENTRY SET_RUN_FUTURE_ASSETS
C***********************************************************************
         SET_RUN_FUTURE_ASSETS = GET_NUM_OF_END_POINTS() > 1
      RETURN
C***********************************************************************
      ENTRY LAST_ELDC_REPORT ! TEMPORARY UNTIL WE PUT INTO RUN SPEC'S
C***********************************************************************
         LAST_ELDC_REPORT = UTILITY_TYPE() == 'T' .AND.
     +                      MARGINAL_COST_SWITCH()
      RETURN
      END
C***********************************************************************
      FUNCTION SET_HEAT_FACTORS_CLASS_GROWTH(COUNTRY_CHR)
C***********************************************************************
C
      use logging
      CHARACTER*1 COUNTRY_CHR
      REAL GET_COST_CONVERSION,COST_CONVERSION,
     +     GET_HEAT_CONVERSION,HEAT_CONVERSION,
     +     GET_TONS_CONVERSION,TONS_CONVERSION
      REAL SET_HEAT_FACTORS_CLASS_GROWTH
      REAL SYS_GROWTH_TRIGGER,SYS_GROWTH_TRIGGER_VALUE/20./
      REAL CLASS_GROWTH_TRIGGER,CLASS_GROWTH_TRIGGER_VALUE/20./
      LOGICAL*1 ORLANDO_UC,SALT_RIVER_PROJECT
      SAVE COST_CONVERSION,
     +     HEAT_CONVERSION,
     +     TONS_CONVERSION
C
         SET_HEAT_FACTORS_CLASS_GROWTH = 0.
C
! SET CONVERSION FACTORS
C
         IF(COUNTRY_CHR == 'C') THEN
            COST_CONVERSION = 10.
            HEAT_CONVERSION = 1.
            TONS_CONVERSION = 1000.  ! KG/TON METRIC
         ELSE
            COST_CONVERSION = .01
            HEAT_CONVERSION = 1000.
            TONS_CONVERSION = 2000.   ! LBS/TON
         ENDIF
C
! LOWER CLASS GROWTH RATES FOR SMALLER COMPANYS
C
         IF(ORLANDO_UC()) THEN
            CLASS_GROWTH_TRIGGER_VALUE = 4.
          ELSEIF(SALT_RIVER_PROJECT()) THEN
            CLASS_GROWTH_TRIGGER_VALUE = 4.
          ENDIF
      RETURN
C
      ENTRY GET_COST_CONVERSION
         GET_COST_CONVERSION = COST_CONVERSION
      RETURN
C***********************************************************************
      ENTRY GET_HEAT_CONVERSION
C***********************************************************************
         GET_HEAT_CONVERSION = HEAT_CONVERSION
      RETURN
C***********************************************************************
      ENTRY GET_TONS_CONVERSION
C***********************************************************************
         GET_TONS_CONVERSION = TONS_CONVERSION
      RETURN
C***********************************************************************
      ENTRY SYS_GROWTH_TRIGGER
C***********************************************************************
         SYS_GROWTH_TRIGGER = SYS_GROWTH_TRIGGER_VALUE
      RETURN
C***********************************************************************
      ENTRY CLASS_GROWTH_TRIGGER
C***********************************************************************
         CLASS_GROWTH_TRIGGER = CLASS_GROWTH_TRIGGER_VALUE
      RETURN
      END
C***********************************************************************
      LOGICAL*1 FUNCTION SET_UTILITY_IDENTIFIER(UTILITY_TYPE_STR,
     +                                          UTILITY_TYPE_CHR)
C***********************************************************************
! end of planning switch information
      use SpinDriftLib
      use prod_arrays_dimensions
      USE PROD_ARRAYS_DIMENSIONS
      use kepcocom
      use sizecom
      use msgoutpt
      
      LOGICAL*1 SALT_RIVER_ACTIVE/.FALSE./,SALT_RIVER_PROJECT,
     +          ECITY_ACTIVE/.FALSE./,ECITY_COMPANY,
     +          DUKE_ACTIVE/.FALSE./,DUKE,
     +          IMPA,IMPA_ACTIVE/.FALSE./,
     +          CIPSCO_ACTIVE/.FALSE./,CIPSCO,
     +          SOUTHERN_COMPANY_ACTIVE/.FALSE./,SOUTHERN_COMPANY,
     +          KCPL_ACTIVE/.FALSE./,KCPL,
     +          LGE_ACTIVE/.FALSE./,LGandE,
     +          AMEREN_ACTIVE/.FALSE./,AMEREN,
     +          PACIFICORP_ACTIVE/.FALSE./,PACIFICORP,
     +          FirstEnergy_ACTIVE/.FALSE./,FirstEnergy,
     +          ODEC_ACTIVE/.FALSE./,ODEC
      LOGICAL*1 IP_ACTIVE/.FALSE./,UTILITY_IS_IP
      LOGICAL*1 ST_JOE_ACTIVE/.FALSE./,ST_JOE
      LOGICAL*1 OUC_ACTIVE/.FALSE./,ORLANDO_UC
      LOGICAL*1 EMPIRE_ACTIVE/.FALSE./,EMPIRE
      LOGICAL*1 UI_ACTIVE/.FALSE./,UI
      LOGICAL*1 BANGOR_ACTIVE/.FALSE./,BANGOR
      LOGICAL*1 APS_DQE_ACTIVE/.FALSE./,APS_DQE,
     +          CHANGE_CPL_STATUS_TO_ACTIVE,TEMPL1
      LOGICAL*1 CCN_ACTIVE/.FALSE./,CCN
      CHARACTER*10 UTILITY_TYPE_STR,TEMP_STR_10
      CHARACTER*1 UTILITY_TYPE_CHR
      LOGICAL*1 WKP_ACTIVE
      LOGICAL*1 MPS_ACTIVE/.FALSE./,MPS
      LOGICAL*1 WVPA,WABASH_VALLEY_FIN/.FALSE./
      LOGICAL*1 IPALCO,IPALCO_ACTIVE/.FALSE./
      LOGICAL*1 GreatRiver,GRE_ACTIVE/.FALSE./
C
         WABASH_VALLEY = .FALSE.
         REALLY_KEPCO = .FALSE.
         TEMP_STR_10 = UTILITY_TYPE_STR
         CALL UPC(TEMP_STR_10,UTILITY_TYPE_STR)
         IF(INDEX(UTILITY_TYPE_STR,'WABASH') /= 0) THEN
            WABASH_VALLEY = .TRUE.
            WABASH_VALLEY_FIN = .TRUE.
            UTILITY_TYPE_CHR = 'R'
         ELSEIF(INDEX(UTILITY_TYPE_STR,'GRE') /= 0) THEN
            GRE_ACTIVE = .TRUE.
            UTILITY_TYPE_CHR = 'R'
         ELSEIF(INDEX(UTILITY_TYPE_STR,'SALT') /= 0 .OR.
     +                          INDEX(UTILITY_TYPE_STR,'SRP') /= 0) THEN
            SALT_RIVER_ACTIVE = .TRUE.
            UTILITY_TYPE_CHR = 'R'
         ELSEIF(INDEX(UTILITY_TYPE_STR,'SOUT') /= 0) THEN
             UTILITY_TYPE_CHR = 'I'
            SOUTHERN_COMPANY_ACTIVE = .TRUE.
            CALL TELL_DISPATCH_SOUTHERN_STATUS
         ELSEIF(INDEX(UTILITY_TYPE_STR,'STJP&L') /= 0) THEN
             UTILITY_TYPE_CHR = 'I'
            ST_JOE_ACTIVE = .TRUE.
         ELSEIF(INDEX(UTILITY_TYPE_STR,'KCP&L') /= 0) THEN
             UTILITY_TYPE_CHR = 'I'
            KCPL_ACTIVE = .TRUE.
         ELSEIF(INDEX(UTILITY_TYPE_STR,'LG&E') /= 0) THEN
             UTILITY_TYPE_CHR = 'I'
            LGE_ACTIVE = .TRUE.
         ELSEIF(INDEX(UTILITY_TYPE_STR,'FIRSTE') /= 0) THEN
             UTILITY_TYPE_CHR = 'I'
            FirstEnergy_ACTIVE = .TRUE.
         ELSEIF(INDEX(UTILITY_TYPE_STR,'EMPIRE') /= 0) THEN
             UTILITY_TYPE_CHR = 'I'
            EMPIRE_ACTIVE = .TRUE.
         ELSEIF(INDEX(UTILITY_TYPE_STR,'UI') /= 0) THEN
             UTILITY_TYPE_CHR = 'I'
            UI_ACTIVE = .TRUE.
         ELSEIF(INDEX(UTILITY_TYPE_STR,'BGR') /= 0) THEN
             UTILITY_TYPE_CHR = 'I'
            BANGOR_ACTIVE = .TRUE.
         ELSEIF(INDEX(UTILITY_TYPE_STR,'MPS') /= 0) THEN
             UTILITY_TYPE_CHR = 'I'
            MPS_ACTIVE = .TRUE.
         ELSEIF(INDEX(UTILITY_TYPE_STR,'APS') /= 0) THEN
             UTILITY_TYPE_CHR = 'I'
            APS_DQE_ACTIVE = .TRUE.
         ELSEIF(INDEX(UTILITY_TYPE_STR,'IPALCO') /= 0 .OR.
     +                          INDEX(UTILITY_TYPE_STR,'IPL') /= 0) THEN
             UTILITY_TYPE_CHR = 'I'
            IPALCO_ACTIVE = .TRUE.
         ELSEIF(INDEX(UTILITY_TYPE_STR,'IP') /= 0) THEN
             UTILITY_TYPE_CHR = 'I'
            IP_ACTIVE = .TRUE.
         ELSEIF(INDEX(UTILITY_TYPE_STR,'CP&L') /= 0) THEN
             UTILITY_TYPE_CHR = 'I'
            TEMPL1 = CHANGE_CPL_STATUS_TO_ACTIVE()
         ELSEIF(INDEX(UTILITY_TYPE_STR,'CCN') /= 0) THEN
             UTILITY_TYPE_CHR = 'I'
            CCN_ACTIVE = .TRUE.
         ELSEIF(INDEX(UTILITY_TYPE_STR,'OUC') /= 0) THEN
             UTILITY_TYPE_CHR = 'X'
            OUC_ACTIVE = .TRUE.
         ELSEIF(INDEX(UTILITY_TYPE_STR,'CIPS') /= 0) THEN
             UTILITY_TYPE_CHR = 'I'
            CIPSCO_ACTIVE = .TRUE.
         ELSEIF(INDEX(UTILITY_TYPE_STR,'AMEREN') /= 0) THEN
             UTILITY_TYPE_CHR = 'I'
            AMEREN_ACTIVE = .TRUE.
         ELSEIF(INDEX(UTILITY_TYPE_STR,'PACIFI') /= 0) THEN
             UTILITY_TYPE_CHR = 'I'
            PACIFICORP_ACTIVE = .TRUE.
         ELSEIF(INDEX(UTILITY_TYPE_STR,'DUKE') /= 0) THEN
             UTILITY_TYPE_CHR = 'I'
            DUKE_ACTIVE = .TRUE.
         ELSEIF(INDEX(UTILITY_TYPE_STR,'IMPA') /= 0) THEN
             UTILITY_TYPE_CHR = 'I'
            IMPA_ACTIVE = .TRUE.
         ELSEIF(INDEX(UTILITY_TYPE_STR,'ECITY') /= 0) THEN
             UTILITY_TYPE_CHR = 'I'
            ECITY_ACTIVE = .TRUE.
         ELSEIF(INDEX(UTILITY_TYPE_STR,'KEPCO') /= 0) THEN
             UTILITY_TYPE_CHR = 'R'
            REALLY_KEPCO = .TRUE.
         ELSEIF(INDEX(UTILITY_TYPE_STR,'ODEC') /= 0) THEN
             UTILITY_TYPE_CHR = 'I'
            ODEC_ACTIVE = .TRUE.
         ELSE
             UTILITY_TYPE_CHR = UTILITY_TYPE_STR(1:1)
         ENDIF
         IF(UTILITY_TYPE_CHR == 'M') UTILITY_TYPE_CHR = 'P'
         WKP_ACTIVE = STORE_WKP_ACTIVE(UTILITY_TYPE_CHR, WKP_ACTIVE)
      RETURN
C***********************************************************************
      ENTRY ODEC
C***********************************************************************
         ODEC = ODEC_ACTIVE
      RETURN
C***********************************************************************
      ENTRY IPALCO
C***********************************************************************
         IPALCO = IPALCO_ACTIVE
      RETURN
C***********************************************************************
      ENTRY GreatRiver
C***********************************************************************
         GreatRiver = GRE_ACTIVE
      RETURN
C***********************************************************************
      ENTRY WVPA
         WVPA = WABASH_VALLEY_FIN
      RETURN
C***********************************************************************
      ENTRY AMEREN
C***********************************************************************
         AMEREN = AMEREN_ACTIVE
      RETURN
C***********************************************************************
      ENTRY PACIFICORP
C***********************************************************************
         PACIFICORP = PACIFICORP_ACTIVE
      RETURN
C***********************************************************************
      ENTRY CIPSCO
C***********************************************************************
         CIPSCO = CIPSCO_ACTIVE
      RETURN
C***********************************************************************
      ENTRY DUKE
C***********************************************************************
         DUKE = DUKE_ACTIVE
      RETURN
C***********************************************************************
      ENTRY IMPA
C***********************************************************************
         IMPA = IMPA_ACTIVE
      RETURN
C***********************************************************************
      ENTRY ECITY_COMPANY
C***********************************************************************
         ECITY_COMPANY = ECITY_ACTIVE
      RETURN
C***********************************************************************
      ENTRY UTILITY_IS_IP
C***********************************************************************
         UTILITY_IS_IP = IP_ACTIVE
      RETURN
C***********************************************************************
      ENTRY SOUTHERN_COMPANY
C***********************************************************************
         SOUTHERN_COMPANY = SOUTHERN_COMPANY_ACTIVE
      RETURN
C***********************************************************************
      ENTRY KCPL
C***********************************************************************
         KCPL = KCPL_ACTIVE
      RETURN
C***********************************************************************
      ENTRY LGandE
C***********************************************************************
         LGandE = LGE_ACTIVE
      RETURN
C***********************************************************************
      ENTRY FirstEnergy
C***********************************************************************
         FirstEnergy = FirstEnergy_ACTIVE
      RETURN
C***********************************************************************
      ENTRY SALT_RIVER_PROJECT
C***********************************************************************
         SALT_RIVER_PROJECT = SALT_RIVER_ACTIVE
      RETURN
C***********************************************************************
      ENTRY APS_DQE
C***********************************************************************
         APS_DQE = APS_DQE_ACTIVE
      RETURN
C***********************************************************************
      ENTRY ST_JOE
C***********************************************************************
         ST_JOE = ST_JOE_ACTIVE
      RETURN
C***********************************************************************
      ENTRY EMPIRE
C***********************************************************************
         EMPIRE = EMPIRE_ACTIVE
      RETURN
C***********************************************************************
      ENTRY UI
C***********************************************************************
         UI = .true. ! UI_ACTIVE
      RETURN
C***********************************************************************
      ENTRY BANGOR
         BANGOR = BANGOR_ACTIVE
      RETURN
C***********************************************************************
      ENTRY MPS
         MPS = MPS_ACTIVE
      RETURN
C***********************************************************************
      ENTRY ORLANDO_UC
         ORLANDO_UC = OUC_ACTIVE
      RETURN
C***********************************************************************
      ENTRY CCN
         CCN = CCN_ACTIVE
      RETURN
      END
C***********************************************************************
C
! MOVED FROM MIDAS.FOR 8/3/93
C
      SUBROUTINE VERIFY_BASE_FILE_DIRECTORY(BASE_FILE_DIRECTORY,
     +                                      RESULTS_DIRECTORY,
     +                                      LDE_FILE_DIRECTORY,
     +                                      PRB_FILE_DIRECTORY,
     +                                      SHB_FILE_DIRECTORY,
     +                                      XML_FILE_DIRECTORY,
     +                                      LDG_FILE_DIRECTORY)
C***********************************************************************
C
      use logging
      use dsex_data
       use SpinDriftLib
      use prod_arrays_dimensions
      USE PROD_ARRAYS_DIMENSIONS
      CHARACTER*(*) BASE_FILE_DIRECTORY,RESULTS_DIRECTORY
      CHARACTER*2 BASE_FILE_DRIVE,LDE_FILE_DRIVE,PRB_FILE_DRIVE,
     +            SHB_FILE_DRIVE,XML_FILE_DRIVE,LDG_FILE_DRIVE
      CHARACTER*256 FILE_NAME,SEARCH_FILE
      CHARACTER*256 RETURN_CHAR,STORE_BASE_FILE_DIRECTORY,
     +                                   LDE_FILE_DIRECTORY,
     +                                   LDG_FILE_DIRECTORY,
     +                                   PRB_FILE_DIRECTORY,
     +                                   SHB_FILE_DIRECTORY,
     +                                   XML_FILE_DIRECTORY
C
      INTEGER*2 I
      INTEGER*2 ISERROR,ERROR
      INTEGER*4 IOS
!
! BASE FILE DIRECTORY SET-UP
!
      cldata%SCENAME=check_scename()
      
      I = NBLANK(BASE_FILE_DIRECTORY)
      IF(I == 0) THEN
         BASE_FILE_DIRECTORY = RESULTS_DIRECTORY
         I = NBLANK(BASE_FILE_DIRECTORY)
      ELSEIF(I==1 .AND. BASE_FILE_DIRECTORY(1:1) /= '\') THEN
         BASE_FILE_DIRECTORY = trim(BASE_FILE_DIRECTORY)//':'
         I = 2
      ENDIF
      IF(BASE_FILE_DIRECTORY(I:I) /= '\') THEN
         BASE_FILE_DIRECTORY = trim(BASE_FILE_DIRECTORY)//'\'
      ELSE
         I = I - 1
      ENDIF
      OPEN(10,trim(BASE_FILE_DIRECTORY)//'TESTDIR',IOSTAT=IOS)
      IF(IOS /= 0) THEN
         BASE_FILE_DIRECTORY = RESULTS_DIRECTORY
      ELSE
         CLOSE(10,STATUS='DELETE')
      ENDIF
!      SEARCH_FILE = trim(BASE_FILE_DIRECTORY)//'??B*.DAT'
!      CALL FINDFRST(SEARCH_FILE,FILE_NAME)
!      ERROR = ISERROR()
!      IF(ERROR > 0) THEN
!         BASE_FILE_DIRECTORY = RESULTS_DIRECTORY
!      ENDIF
      I = NBLANK(BASE_FILE_DIRECTORY)
      IF(BASE_FILE_DIRECTORY(I:I) /= '\')
     +            BASE_FILE_DIRECTORY = trim(BASE_FILE_DIRECTORY)//'\'
      IF(INDEX(BASE_FILE_DIRECTORY,':') == 2) THEN
         BASE_FILE_DRIVE = BASE_FILE_DIRECTORY(1:2)
      ELSE
         BASE_FILE_DRIVE = RESULTS_DIRECTORY(1:1)//':'
         BASE_FILE_DIRECTORY = BASE_FILE_DRIVE//BASE_FILE_DIRECTORY
      ENDIF
!
! LDE FILE DIRECTORY SET-UP
!
      IF(trim(LDE_FILE_DIRECTORY) == ' ') THEN
         LDE_FILE_DIRECTORY = RESULTS_DIRECTORY
      ELSE
         I = NBLANK(LDE_FILE_DIRECTORY)
         IF(I == 0 .OR. I == 256) THEN
            LDE_FILE_DIRECTORY = RESULTS_DIRECTORY
            I = NBLANK(LDE_FILE_DIRECTORY)
         ELSEIF(I==1 .AND. LDE_FILE_DIRECTORY(1:1) /= '\') THEN
            LDE_FILE_DIRECTORY = trim(LDE_FILE_DIRECTORY)//':'
            I = 2
         ENDIF
         IF(LDE_FILE_DIRECTORY(I:I) /= '\') THEN
            LDE_FILE_DIRECTORY = trim(LDE_FILE_DIRECTORY)//'\'
         ELSE
            I = I - 1
         ENDIF
         OPEN(10,trim(LDE_FILE_DIRECTORY)//'TESTDIR',IOSTAT=IOS)
         IF(IOS > 0) THEN
            LDE_FILE_DIRECTORY = RESULTS_DIRECTORY
         ELSE
            CLOSE(10,STATUS='DELETE')
         ENDIF

         I = NBLANK(LDE_FILE_DIRECTORY)
         IF(LDE_FILE_DIRECTORY(I:I) /= '\')
     +              LDE_FILE_DIRECTORY = trim(LDE_FILE_DIRECTORY)//'\'
         IF(INDEX(LDE_FILE_DIRECTORY,':') == 2) THEN
            LDE_FILE_DRIVE = LDE_FILE_DIRECTORY(1:2)
         ELSE
            LDE_FILE_DRIVE = RESULTS_DIRECTORY(1:1)//':'
            LDE_FILE_DIRECTORY = LDE_FILE_DRIVE//LDE_FILE_DIRECTORY
         ENDIF
      ENDIF
!
! LDE FILE DIRECTORY SET-UP
!
      IF(trim(LDG_FILE_DIRECTORY) == ' ') THEN
         LDG_FILE_DIRECTORY = RESULTS_DIRECTORY
      ELSE
         I = NBLANK(LDG_FILE_DIRECTORY)
         IF(I == 0 .OR. I == 256) THEN
            LDG_FILE_DIRECTORY = RESULTS_DIRECTORY
            I = NBLANK(LDG_FILE_DIRECTORY)
         ELSEIF(I==1 .AND. LDG_FILE_DIRECTORY(1:1) /= '\') THEN
            LDG_FILE_DIRECTORY = trim(LDG_FILE_DIRECTORY)//':'
            I = 2
         ENDIF
         IF(LDG_FILE_DIRECTORY(I:I) /= '\') THEN
            LDG_FILE_DIRECTORY = trim(LDG_FILE_DIRECTORY)//'\'
         ELSE
            I = I - 1
         ENDIF
         OPEN(10,trim(LDG_FILE_DIRECTORY)//'TESTDIR',IOSTAT=IOS)
         IF(IOS > 0) THEN
            LDG_FILE_DIRECTORY = RESULTS_DIRECTORY
         ELSE
            CLOSE(10,STATUS='DELETE')
         ENDIF

         I = NBLANK(LDG_FILE_DIRECTORY)
         IF(LDG_FILE_DIRECTORY(I:I) /= '\')
     +              LDG_FILE_DIRECTORY = trim(LDG_FILE_DIRECTORY)//'\'
         IF(INDEX(LDG_FILE_DIRECTORY,':') == 2) THEN
            LDG_FILE_DRIVE = LDG_FILE_DIRECTORY(1:2)
         ELSE
            LDG_FILE_DRIVE = RESULTS_DIRECTORY(1:1)//':'
            LDG_FILE_DIRECTORY = LDG_FILE_DRIVE//LDG_FILE_DIRECTORY
         ENDIF
      ENDIF
!
! PRB FILE DIRECTORY SET-UP
!
      IF(trim(PRB_FILE_DIRECTORY) == ' ') THEN
         PRB_FILE_DIRECTORY = RESULTS_DIRECTORY
      ELSE
         I = NBLANK(PRB_FILE_DIRECTORY)
         IF(I == 0 .OR. I == 256) THEN
            PRB_FILE_DIRECTORY = RESULTS_DIRECTORY
            I = NBLANK(PRB_FILE_DIRECTORY)
         ELSEIF(I==1 .AND. PRB_FILE_DIRECTORY(1:1) /= '\') THEN
            PRB_FILE_DIRECTORY = trim(PRB_FILE_DIRECTORY)//':'
            I = 2
         ENDIF
         IF(PRB_FILE_DIRECTORY(I:I) /= '\') THEN
            PRB_FILE_DIRECTORY = trim(PRB_FILE_DIRECTORY)//'\'
         ELSE
            I = I - 1
         ENDIF
         OPEN(10,trim(PRB_FILE_DIRECTORY)//'TESTDIR',IOSTAT=IOS)
         IF(IOS > 0) THEN
            PRB_FILE_DIRECTORY = RESULTS_DIRECTORY
         ELSE
            CLOSE(10,STATUS='DELETE')
         ENDIF

         I = NBLANK(PRB_FILE_DIRECTORY)
         IF(PRB_FILE_DIRECTORY(I:I) /= '\')
     +              PRB_FILE_DIRECTORY = trim(PRB_FILE_DIRECTORY)//'\'
         IF(INDEX(PRB_FILE_DIRECTORY,':') == 2) THEN
            PRB_FILE_DRIVE = PRB_FILE_DIRECTORY(1:2)
         ELSE
            PRB_FILE_DRIVE = RESULTS_DIRECTORY(1:1)//':'
            PRB_FILE_DIRECTORY = PRB_FILE_DRIVE//PRB_FILE_DIRECTORY
         ENDIF
      ENDIF
!
! SHB FILE DIRECTORY SET-UP
!
      IF(trim(SHB_FILE_DIRECTORY) == ' ') THEN
         SHB_FILE_DIRECTORY = RESULTS_DIRECTORY
      ELSE
         I = NBLANK(SHB_FILE_DIRECTORY)
         IF(I == 0 .OR. I == 256) THEN
            SHB_FILE_DIRECTORY = RESULTS_DIRECTORY
            I = NBLANK(SHB_FILE_DIRECTORY)
         ELSEIF(I==1 .AND. SHB_FILE_DIRECTORY(1:1) /= '\') THEN
            SHB_FILE_DIRECTORY = trim(SHB_FILE_DIRECTORY)//':'
            I = 2
         ENDIF
         IF(SHB_FILE_DIRECTORY(I:I) /= '\') THEN
            SHB_FILE_DIRECTORY = trim(SHB_FILE_DIRECTORY)//'\'
         ELSE
            I = I - 1
         ENDIF
         OPEN(10,trim(SHB_FILE_DIRECTORY)//'TESTDIR',IOSTAT=IOS)
         IF(IOS > 0) THEN
            SHB_FILE_DIRECTORY = RESULTS_DIRECTORY
         ELSE
            CLOSE(10,STATUS='DELETE')
         ENDIF
!         SEARCH_FILE = trim(SHB_FILE_DIRECTORY)//'SHB*.*'
!         CALL FINDFRST(SEARCH_FILE,FILE_NAME)
!         ERROR = ISERROR()
!         IF(ERROR > 0) THEN
!            SHB_FILE_DIRECTORY = RESULTS_DIRECTORY
!         ENDIF
         I = NBLANK(SHB_FILE_DIRECTORY)
         IF(SHB_FILE_DIRECTORY(I:I) /= '\')
     +              SHB_FILE_DIRECTORY = trim(SHB_FILE_DIRECTORY)//'\'
         IF(INDEX(SHB_FILE_DIRECTORY,':') == 2) THEN
            SHB_FILE_DRIVE = SHB_FILE_DIRECTORY(1:2)
         ELSE
            SHB_FILE_DRIVE = RESULTS_DIRECTORY(1:1)//':'
            SHB_FILE_DIRECTORY = SHB_FILE_DRIVE//SHB_FILE_DIRECTORY
         ENDIF
      ENDIF
!
! XML FILE DIRECTORY SET-UP
!
      IF(trim(XML_FILE_DIRECTORY) == ' ') THEN
         XML_FILE_DIRECTORY = RESULTS_DIRECTORY
      ELSE
         I = NBLANK(XML_FILE_DIRECTORY)
         IF(I == 0 .OR. I == 256) THEN
            XML_FILE_DIRECTORY = RESULTS_DIRECTORY
            I = NBLANK(XML_FILE_DIRECTORY)
         ELSEIF(I==1 .AND. XML_FILE_DIRECTORY(1:1) /= '\') THEN
            XML_FILE_DIRECTORY = trim(XML_FILE_DIRECTORY)//':'
            I = 2
         ENDIF
         IF(XML_FILE_DIRECTORY(I:I) /= '\') THEN
            XML_FILE_DIRECTORY = trim(XML_FILE_DIRECTORY)//'\'
         ELSE
            I = I - 1
         ENDIF
         OPEN(10,trim(XML_FILE_DIRECTORY)//'TESTDIR',IOSTAT=IOS)
         IF(IOS > 0) THEN
            XML_FILE_DIRECTORY = RESULTS_DIRECTORY
         ELSE
            CLOSE(10,STATUS='DELETE')
         ENDIF

         I = NBLANK(XML_FILE_DIRECTORY)
         IF(XML_FILE_DIRECTORY(I:I) /= '\')
     +              XML_FILE_DIRECTORY = trim(XML_FILE_DIRECTORY)//'\'
         IF(INDEX(XML_FILE_DIRECTORY,':') == 2) THEN
            XML_FILE_DRIVE = XML_FILE_DIRECTORY(1:2)
         ELSE
            XML_FILE_DRIVE = RESULTS_DIRECTORY(1:1)//':'
            XML_FILE_DIRECTORY = XML_FILE_DRIVE//XML_FILE_DIRECTORY
         ENDIF
      ENDIF
!
      RETURN_CHAR = STORE_BASE_FILE_DIRECTORY(BASE_FILE_DIRECTORY,
     +                                        RESULTS_DIRECTORY,
     +                                        cldata%SCENAME,
     +                                        LDE_FILE_DIRECTORY,
     +                                        PRB_FILE_DIRECTORY,
     +                                        SHB_FILE_DIRECTORY,
     +                                        XML_FILE_DIRECTORY,
     +                                        LDG_FILE_DIRECTORY)
      RETURN
      END
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      FUNCTION NEW_DETAILED_REPORTS()
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!
      use rptreccontrol
      USE DRILLING_REPT_PARAMETERS
      USE IREC_ENDPOINT_CONTROL
      use logging
      use miscmod
      use midasmod_decs


      INCLUDE 'DREPTCOM.MON'
C
      CHARACTER (LEN=2) :: CAPACITY_PLANNING_METHOD,GREEN_MRX_METHOD
      LOGICAL*1 NEW_DETAILED_REPORTS
      INTEGER*2 BASE_YEAR
      INTEGER*2 CPL_EMC_CAP_REVS_RPT_HEADER,
     +          CPL_PAEN_RPT_HEADER,
     +          CPL_DEFERRED_FUEL_RPT_HEADER,
     +          DAY_WEEK_RPT_HEADER,
     +          TRANS_TIE_RPT_HEADER,
     +          DAILY_PRODUCTS_RPT_HEADER,
     +          SCENARIO_MAKER_RPT_HEADER,
     +          REGIONAL_MAKER_RPT_HEADER,
     +          MONTHLY_FUEL_DERIV_RPT_HEADER,
     +          TRANS_DEL_RPT_HEADER,TRANS_ANN_RPT_HEADER,
     +          MON_TRANC_RPT_HEADER,
     +          MON_POSIT_RPT_HEADER,
     +          TRANS_PRI_RPT_HEADER,TRANS_LDS_RPT_HEADER,
     +          TRANS_LBH_RPT_HEADER,
     +          TRANS_INTR_RPT_HEADER,
     +          TRANS_SCA_RPT_HEADER,
     +          TRANS_LAS_RPT_HEADER,
     +          TRANS_MCB_RPT_HEADER,
     +          MARKET_AREA_RPT_HEADER,
     +          GAS_MARKET_AREA_RPT_HEADER,
     +          GAS_MARKET_TRANS_RPT_HEADER,
     +          DAILY_GAS_SELL_BUY_HEADER,
     +          MONTHLY_GAS_SELL_BUY_HEADER,
     +          MONTHLY_GAS_LINK_HEADER,
     +          MONTHLY_GAS_RESERVE_HEADER,
     +          DAILY_GAS_LINK_HEADER,
     +          DAILY_GAS_LINK_LIMIT_HEADER,
     +          DAILY_GAS_LINK_PRICE_HEADER,
     +          DAILY_GAS_LINK_CF_HEADER,
     +          DAILY_GAS_AVE_PRICE_HEADER,
     +          DAILY_GAS_SUR_DEF_HEADER,
     +          MONTHLY_GAS_LINK_PRICE_HEADER,
     +          DAILY_GAS_PRICE_HEADER,
     +          DAILY_GAS_NODE_VOLUME_HEADER,
     +          DAILY_GAS_STORAGE_HEADER,
     +          MONTHLY_GAS_PRICE_HEADER,
     +          MONTHLY_GAS_DIFFER_HEADER,
     +          DAILY_GAS_DIFFER_HEADER,
     +          DAILY_GAS_UNSERVED_HEADER,
     +          DAILY_GAS_SUPPLY_HEADER,
     +          TRANS_HMP_RPT_HEADER,
     +          TRANS_MRK_RPT_HEADER,
     +          TRANS_PB4_RPT_HEADER,
     +          TRANS_PAF_RPT_HEADER,
     +          TRANS_CAP_RPT_HEADER,
     +          TRANS_DER_RPT_HEADER,
     +          TRANS_EUE_RPT_HEADER,
     +          TRANS_INC_RPT_HEADER,
     +          TRANS_LMD_RPT_HEADER,
     +          TRANS_TRM_RPT_HEADER,
     +          TRANS_EXC_RPT_HEADER,
     +          HOURLY_ECAP_RPT_HEADER,
     +          HOUR_DIAG_RPT_HEADER,
     +          HOUR_JDA_RPT_HEADER,
     +          DEPTH_MARKET_RPT_HEADER,
     +          TRANS_DEPTH_MARKET_RPT_HEADER,
     +          C_DEPTH_MARKET_RPT_HEADER,
     +          C_DEPTH_MARGIN_RPT_HEADER,
     +          DYNAMIC_JURIS_RPT_HEADER,
     +          TRANSACT_FUEL_RPT_HEADER,
     +          TRANSACT_STATE_RPT_HEADER,
     +          STATE_RPS_RPT_HEADER,
     +          CO2_ABATEMENT_RPT_HEADER,
     +          RPS_PROGRAMS_RPT_HEADER,
     +          TRANSACT_PROD_RPT_HEADER,
     +          TRAN_PROD_FUEL_RPT_HEADER,
     +          MARGINAL_UNIT_TECH_HEADER,
     +          TRANSACT_PROD_PROD_RPT_HEADER,
     +          TRANSACT_PROD_MW_RPT_HEADER,
     +          RPS_CURVE_RPT_HEADER,
     +          HOURLY_STORAGE_REPORT_HEADER,
     +          TRANSACT_PROD_FUEL_RPT_HEADER,
     +          NUC_FUEL_REGION_RPT,
     +          EXISTING_ASSET_RPT_HEADER,
     +          WVPA_ASSET_RPT_HEADER,
     +          WVPA_WORKORDER_CONTROL_RPT,
     +          WVPA_WORKORDER_EXPENSES_RPT,
     +          WVPA_WORKORDER_SUMMARY_RPT,
     +          WVPA_DEBT_FILE_RPT_HEADER,
     +          WVPA_MORTGAGE_SUMMARY_RPT_HEADR,
     +          WVPA_PLNT_BALANCES_RPT_HEADR,
     +          WVPA_CONSTR_SUMY_RPT_HEADR,
     +          WVPA_COMP_CONSTR_SUMY_RPT_HEADR,
     +          WVPA_CUM_DEP_RPT_HEADR,
     +          WVPA_RATE_TRACKER_HEADR,
     +          FUTURE_ASSET_RPT_HEADER,
     +          TRANS_CONTRIBUTION_RPT_HEADER,
     +          TRANS_TRN_RPT_HEADER,
     +          ASSET_PROD_RPT_HEADER,
     +          ANNUAL_SERVICE_TRANS_HEADER,
     +          MON_CL_TRANS_UNIT_HEADER,
     +          MON_DV_TRANS_UNIT_HEADER,
     +          MON_NU_TRANS_UNIT_HEADER,
     +          TECH_SCAR_HEADER,
     +          ECITY_OBJ_HEADER,
     +          MARKET_DURATION_HEADER,
     +          USER_DURATION_HEADER,
     +          TRANS_CLASS_SUM_HEADER,
     +          MONTHLY_GAS_DEMAND_HEADER,
     +          MONTHLY_GAS_BALANCE_HEADER,
     +          MONTHLY_GAS_SUPPLY_HEADER,
     +          MONTHLY_GAS_BASIN_HEADER,
     +          WH_HOURLY_HEADER,
     +          DEBT_FILE_RPT_HEADER,
     +          DEBIT_FILE_RPT_HEADER,
     +          CPL_SALES_GEN_HEADER,
     +          MONTH_GROUP_HEADER,
     +          MX_ANNUAL_HEADER,
     +          GRX_ANNUAL_HEADER,
     +          GRX_8761_HEADER,
     +          GAS_PIPE_EXPANSION_HEADER,
     +          MX_ICAP_ANNUAL_HEADER,
     +          CX_DAILY_HEADER,
     +          UU_DAILY_HEADER,
     +          UK_DAILY_HEADER,
     +          HX_HOURLY_HEADER,
     +          AJ_HOURLY_HEADER,
     +          AK_HOURLY_HEADER,
     +          UV_HOURLY_HEADER,
     +          XX_HOURLY_HEADER,
     +          KX_HOURLY_HEADER,
     +          QX_HOURLY_HEADER,
     +          WX_HOURLY_HEADER,
     +          HOURLY_FLOW_ANALYST_HEADER,
     +          HOURLY_FLOW_CONSTRAINT_HEADER,
     +          C_HOURLY_COST_HEADER,
     +          C_HOURLY_MWH_COST_HEADER,
     +          C_HOURLY_SUMMARY_HEADER,
     +          C_TG_HOURLY_SUMMARY_HEADER,
     +          GY_HOURLY_HEADER,
     +          DX_HOURLY_HEADER,
     +          OX_HOURLY_HEADER,
     +          HEC_HOURLY_HEADER,
     +          HYDRO_AG_HEADER,
     +          RC_REVENUE_FORECAST_HEADER,
     +          TRAILING_12_MOS_RPT_HEADER,
     +          IPL_REGULATED_REVENUES_RPT_HEADER,
     +          FE_MONTHLY_PCA_LEGAL_HEADER
      INTEGER*2 TRANS_DISPATCH_RPT_HEADER,
     +          CAPEX_THERMAL_RPT_HEADER,
     +          NEW_SUPPLY_CURVE_HEADER
      INTEGER*2 LM_COST_REPORT_HEADER,
     +          DSM_RATE_REPORT_HEADER,
     +          FORECASTED_DAYS_REPORT_HEADER,
     +          FORE_MONTH_REPORT_HEADER,
     +          ANN_CL_UNIT_HEADER,
     +          MON_CL_UNIT_HEADER,
     +          MON_LOAD_BY_BLOCK_HEADER,
     +          HOURLY_CUST_MARGIN_HEADER,
     +          IPL_FAC_HEADER,
     +          MON_CT_UNIT_HEADER,
     +          ANN_CL_SUMMARY_HEADER,
     +          ANN_POOL_SUMMARY_HEADER,
     +          MON_CL_SUMMARY_HEADER,
     +          LDC_REPORT_HEADER,
     +          LAST_ELDC_HEADER,
     +          LOLP_CURVE_HEADER,
     +          LOLP_MONTHLY_HEADER,
     +          ANN_EL_UNIT_HEADER,
     +          MON_EL_UNIT_HEADER,
     +          CLASS_REV_ALLOC_HEADER,
     +          REV_FUNC_HEADER,
     +          DISPATCH_ORDER_HEADER,
     +          CATAWBA_HEADER,
     +          FUEL_INVENTORY_HEADER,
     +          RESERVE_MARGIN_HEADER,
     +          TG_RESERVE_MARGIN_HEADER,
     +          EXPANSION_PATTERN_HEADER,
     +          MON_DSM_PATTERN_HEADER,
     +          WABASH_VALLEY_POWER_COST_HEADER,
     +          WABASH_VALLEY_POWER_DSM_HEADER,
     +          WVPA_COOP_REVENUE_HEADER,
     +          WABASH_VALLEY_POWER_COST_NO,
     +          WV_AREA_REPORT_HEADER,
     +          MAINTENANCE_HEADER,
     +          CM_MAINT_HEADER,
     +          R_MW_MAINT_NO,
     +          SCREENING_HEADER,
     +          MARGINAL_COST_HEADER,
     +          MARGINAL_DATA_BASE_HEADER,
     +          ECONOMY_INTER_HEADER,
     +          GET_EXT_ECONOMY_INTER_NO,
     +          SERVICE_TRANS_HEADER,
     +             FUTURE_ASSET_REPT_HEADER,
     +             GET_SCENAME_AND_EXT_PROD_NUM,
     +          GET_SCENAME_AND_EXT_AI_NUM,
     +             R_EXTERNAL_AI_NUM,
     +          FLEX_PLANNING_ACTION_HEADER,
     +          ASSET_CLASS_INCOME_HEADER,
     +          FE_PCA_LEGAL_HEADER,
     +          AN_DECOMP_REPORT_HEADER,
     +          RESOURCES_TESTED_RPT_HEADER
      INTEGER*2 OVERHEAD_LENGTH,VARIABLE_NUMBER,RECORD_LENGTH,
     +          RPT_UNIT_NUM,ESTABLISH_DETAIL_REPORT_FILE,
     +          GET_NUMBER_OF_ACTIVE_GROUPS,DIMENSIONS,
     +          R_VARIABLE_NUMBER,NEXT_REC,
     +          ESTABLISH_DETAIL_REPORT_FILE_WO
      INTEGER R_RECORD_LENGTH,R_START_RECORD
      CHARACTER*256 FILE_NAME,GET_RESULTS_DIRECTORY
      CHARACTER*5 GET_SCENAME
      CHARACTER*28 COMPANY_NAME,TITLE*40
      SAVE FILE_NAME,OVERHEAD_LENGTH,VARIABLE_NUMBER,DIMENSIONS,
     +     RPT_UNIT_NUM
      LOGICAL*1 UNIT_1500_IS_OPEN/.FALSE./
      CHARACTER*5 R_SCENAME
      CHARACTER*18 DATE_TIME,SYM_SYS_DATE_TIME
      LOGICAL*1 AN_DECOMP_ACTIVE,DETAIL_ANNUAL_DECOMP_REPORT
      INTEGER*2 GET_NUM_OF_END_POINTS
      LOGICAL FILE_EXISTS
      CHARACTER*12 DES_FILE_NAME
      INTEGER*2 WRITE_OPTIONS_REPORT_HEADER
      CHARACTER*32 SHORT_FORM_TITLE/' '/
      CHARACTER*8 SHORT_FORM_NAME/' '/ ! CHANGED 12/6/93.GAT
      INTEGER*2 ACTIVE_VARIABLES,LAST_VARIABLE,ENDYR
      CHARACTER*16 SET_RUN_DATE_TIME,RUN_DATE_TIME
      INTEGER*4 R_NEXT_REC,
     +          R_CL_NEXT_REC,
     +          R_MW_NEXT_REC
      CHARACTER*1 ACTIVE_DIMENSION
      LOGICAL*1 ADD_MONTHLY_DIMENSION

!
! END DATA DECLARATIONS
!                               NEW_DETAILED_REPORTS = .TRUE.
      RETURN
! Beginning of report headers
C***********************************************************************
      ENTRY GET_SCENAME_AND_EXT_PROD_NUM(R_SCENAME)
C***********************************************************************
        R_SCENAME = GET_SCENAME()
        GET_SCENAME_AND_EXT_PROD_NUM = EXTERNAL_PRODUCTION_NUM
      RETURN
C***********************************************************************
      ENTRY GET_SCENAME_AND_EXT_AI_NUM(R_SCENAME)
C***********************************************************************
        R_SCENAME = GET_SCENAME()
        GET_SCENAME_AND_EXT_AI_NUM = EXTERNAL_AI_NUM
      RETURN
C***********************************************************************
      ENTRY AN_DECOMP_REPORT_HEADER(R_NEXT_REC)
C***********************************************************************
C
! OPEN BIFP OUTPUT FILE
C
         AN_DECOMP_REPORT_HEADER = 1500
         IF(DETAIL_ANNUAL_DECOMP_REPORT() .AND.
     +                                      .NOT.UNIT_1500_IS_OPEN) THEN

            ACTIVE_VARIABLES = get_last_variable() + 1
            DES_FILE_NAME = trim(SHORT_FORM_NAME)//'.DSS'
            CALL COPY(DES_FILE_NAME,'MSGANDEC.RPT')
            DES_FILE_NAME = 'MSGANDEC.RPT'
C
            FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                                     trim(GET_SCENAME())//".ARD"
            OVERHEAD_LENGTH = 150
            VARIABLE_NUMBER = ACTIVE_VARIABLES+5
            RPT_UNIT_NUM = INT2(1500)
            DIMENSIONS = 4
            RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE_WO(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
C

            WRITE(1500,REC=NEXT_REC) 'End Point           ',
     +                                                   'N',INT2(4),'V'
            WRITE(1500,REC=NEXT_REC+1) 'Planning Year       ',
     +                                                   'N',INT2(4),'V'
            WRITE(1500,REC=NEXT_REC+2) 'Plan Number         ',
     +                                                   'N',INT2(4),'V'
            WRITE(1500,REC=NEXT_REC+3) 'Simulation Year     ',
     +                                                   'N',INT2(4),'V'
            R_NEXT_REC = NEXT_REC + 4
            IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
            UNIT_1500_IS_OPEN = .TRUE.
         ENDIF
      RETURN
C***********************************************************************
      ENTRY RESOURCES_TESTED_RPT_HEADER(R_NEXT_REC)
C***********************************************************************
C
! OPEN THE RESOURCE TESTED FILE
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                                     trim(GET_SCENAME())//".AOD"
         OVERHEAD_LENGTH = 128
         VARIABLE_NUMBER = 0
         RPT_UNIT_NUM = INT2(1501)
         DIMENSIONS = 3
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE_WO(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(1501,REC=NEXT_REC)   'End Point           ',
     +                                                   'N',INT2(4),'V'
         WRITE(1501,REC=NEXT_REC+1) 'Planning Year       ',
     +                                                   'N',INT2(4),'V'
         WRITE(1501,REC=NEXT_REC+2) 'Plan Order          ',
     +                                                   'N',INT2(4),'V'
         R_NEXT_REC = NEXT_REC + 3
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         RESOURCES_TESTED_RPT_HEADER = 1501
      RETURN
C
C***********************************************************************
      ENTRY WRITE_OPTIONS_REPORT_HEADER(R_NEXT_REC)
C***********************************************************************
C
! OPEN THE RESOURCE OPTIONS FILE
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                                     trim(GET_SCENAME())//".ALD"
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = 0
         RPT_UNIT_NUM = INT2(1502)
         DIMENSIONS = 2
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE_WO(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
C
         WRITE(1502,REC=NEXT_REC) 'End Point           ','N',INT2(4),'V'
         WRITE(1502,REC=NEXT_REC+1) 'Name                ',
     +                                                  'C',INT2(20),'V'
         R_NEXT_REC = NEXT_REC + 2
C
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         WRITE_OPTIONS_REPORT_HEADER = 1502
      RETURN
C***********************************************************************
      ENTRY LM_COST_REPORT_HEADER(R_NEXT_REC)
C***********************************************************************
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                                     trim(GET_SCENAME())//".DCD"
         OVERHEAD_LENGTH = 68
         VARIABLE_NUMBER = 24
         DIMENSIONS = 4
         RPT_UNIT_NUM = LM_COST_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'LM Financial Account','C',INT2(30),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Cost Component      ','C',INT2(30),'F','D'
         R_NEXT_REC = NEXT_REC + 2
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         LM_COST_REPORT_HEADER = LM_COST_NO
      RETURN
C***********************************************************************
      ENTRY MON_DSM_PATTERN_HEADER(R_NEXT_REC)
C***********************************************************************
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                                     trim(GET_SCENAME())//".MDD"
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = 25
         DIMENSIONS = 5
         RPT_UNIT_NUM = MON_DSM_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Month               ','C',INT2(9),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Load Manage. Device ','C',INT2(32),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Day Type            ','C',INT2(8),'F','D'
         R_NEXT_REC = NEXT_REC + 3
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         MON_DSM_PATTERN_HEADER = MON_DSM_NO
      RETURN
C***********************************************************************
      ENTRY DSM_RATE_REPORT_HEADER(R_NEXT_REC)
C***********************************************************************
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                                     trim(GET_SCENAME())//".DTD"
         OVERHEAD_LENGTH = 60
         VARIABLE_NUMBER = 24
         DIMENSIONS = 4
         RPT_UNIT_NUM = DSM_RATE_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Customer Class      ','C',INT2(20),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Load Manage. Device ','C',INT2(32),'V','D'
C
         R_NEXT_REC = NEXT_REC + 2
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         DSM_RATE_REPORT_HEADER = DSM_RATE_NO
      RETURN
C***********************************************************************
      ENTRY REV_FUNC_HEADER(R_NEXT_REC)
C***********************************************************************
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                                     trim(GET_SCENAME())//".FAD"
         OVERHEAD_LENGTH = 16
         VARIABLE_NUMBER = 5
         DIMENSIONS = 3
         RPT_UNIT_NUM = REV_FUNC_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Functional Allocator','C',INT2(8),'V','D'
         R_NEXT_REC = NEXT_REC + 1
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         REV_FUNC_HEADER = REV_FUNC_NO
      RETURN
C***********************************************************************
      ENTRY CLASS_REV_ALLOC_HEADER(R_NEXT_REC)
C***********************************************************************
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                                     trim(GET_SCENAME())//".RAD"
         OVERHEAD_LENGTH = 28
         VARIABLE_NUMBER = 10
         DIMENSIONS = 3
         RPT_UNIT_NUM = REV_ALLOC_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Customer Class      ','C',INT2(20),'V','D'
C
         R_NEXT_REC = NEXT_REC + 1
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         CLASS_REV_ALLOC_HEADER = REV_ALLOC_NO
      RETURN
C***********************************************************************
      ENTRY  FORECASTED_DAYS_REPORT_HEADER(R_NEXT_REC)
C***********************************************************************
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                                     trim(GET_SCENAME())//".FDD"
         OVERHEAD_LENGTH = 25
         VARIABLE_NUMBER = 24
         DIMENSIONS = 4
         RPT_UNIT_NUM = FORE_DAYS_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Month               ','C',INT2(9),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Day Type            ','C',INT2(8),'F','D'
         R_NEXT_REC = NEXT_REC + 2
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         FORECASTED_DAYS_REPORT_HEADER = FORE_DAYS_NO
      RETURN
C***********************************************************************
      ENTRY FORE_MONTH_REPORT_HEADER(R_NEXT_REC)
C***********************************************************************
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                                     trim(GET_SCENAME())//".FQD"
         OVERHEAD_LENGTH = 25
         VARIABLE_NUMBER = 4
         DIMENSIONS = 4
         RPT_UNIT_NUM = FORE_MONTH_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Month               ','C',INT2(9),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Day Type            ','C',INT2(8),'F','D'
         R_NEXT_REC = NEXT_REC + 2
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         FORE_MONTH_REPORT_HEADER = FORE_MONTH_NO
      RETURN
C***********************************************************************
      ENTRY LDC_REPORT_HEADER(R_NEXT_REC)
C***********************************************************************
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                                     trim(GET_SCENAME())//".LDD"
         OVERHEAD_LENGTH = 27
         VARIABLE_NUMBER = 2
         DIMENSIONS = 5
         RPT_UNIT_NUM = LDC_REPORT_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Month               ','C',INT2(9),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Before/After EL     ','C',INT2(6),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Load Position       ','N',INT2(4),'F','D'
         R_NEXT_REC = NEXT_REC + 3
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         LDC_REPORT_HEADER = LDC_REPORT_NO
      RETURN
C***********************************************************************
      ENTRY LAST_ELDC_HEADER
C***********************************************************************
! THIS IS A FORMATED ASCII FILE
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                                     trim(GET_SCENAME())//".LED"
         OPEN(LAST_ELDC_NO,FILE=FILE_NAME)
C
         LAST_ELDC_HEADER = LAST_ELDC_NO
      RETURN
C
C***********************************************************************
      ENTRY LOLP_MONTHLY_HEADER(R_NEXT_REC)
C***********************************************************************
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                                     trim(GET_SCENAME())//".LOD"
         OVERHEAD_LENGTH = 21
         VARIABLE_NUMBER = 13
         DIMENSIONS = 2
         RPT_UNIT_NUM = LOLP_REPORT_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         R_NEXT_REC = NEXT_REC
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         LOLP_MONTHLY_HEADER = LOLP_REPORT_NO
      RETURN
C***********************************************************************
      ENTRY LOLP_CURVE_HEADER
C***********************************************************************
! THIS IS A FORMATTED ASCII FILE
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                                     trim(GET_SCENAME())//".LCD"
         OPEN(LOLP_CURVE_NO,FILE=FILE_NAME)
C
         LOLP_CURVE_HEADER = LOLP_CURVE_NO
      RETURN
C***********************************************************************
      ENTRY CATAWBA_HEADER(R_NEXT_REC)
C***********************************************************************
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                                     trim(GET_SCENAME())//".CAD"
         OVERHEAD_LENGTH = 13
         VARIABLE_NUMBER = 26
         DIMENSIONS = 3
         RPT_UNIT_NUM = CATAWBA_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Company             ','C',INT2(5),'V','D'
         R_NEXT_REC = NEXT_REC + 1
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         CATAWBA_HEADER = CATAWBA_NO
      RETURN
C***********************************************************************
      ENTRY RESERVE_MARGIN_HEADER(R_NEXT_REC)
C***********************************************************************
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                                     trim(GET_SCENAME())//".RMD"
         OVERHEAD_LENGTH = 8
         ! debugmod - not sure whether variable_numbershould be increased here.
         ! TODO: increase variable number? John did that last time but
         ! it's unclear whether that's appropriate. Ask Greg. variable_number_dsex
         ! is used to determine the length of the record, and it's a count, not
         ! an ID
         ! TODO: Update for new vars - debugmod
         VARIABLE_NUMBER = 24
         DIMENSIONS = 2
         RPT_UNIT_NUM = RESERVE_MARGIN_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         R_NEXT_REC = NEXT_REC
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         RESERVE_MARGIN_HEADER = RESERVE_MARGIN_NO
      RETURN
C***********************************************************************
      ENTRY TG_RESERVE_MARGIN_HEADER(R_NEXT_REC)
C***********************************************************************
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                     trim(GET_SCENAME())//TG_RESERVE_MARGIN_CODE
         OVERHEAD_LENGTH = 64
         
         ! ** Previously set to 51 by JTR for OW, HB, H2, CS but 
         ! believe that was a mistake.
         ! debugmod - check this value.
         VARIABLE_NUMBER = 43
         
         ! debugmod
         ! variable_number_dsex = 51 ! 03/10/2023, JTR, OW, HB, H2, CS (+8)
         ! **
         
         DIMENSIONS = 3
         IF(CAPACITY_PLANNING_METHOD() == 'MX'  .AND.
     +                        GREEN_MRX_METHOD() == 'GX' .AND.
     +                             .NOT. HORIZONS_ACTIVE) THEN
            DIMENSIONS = 4
            OVERHEAD_LENGTH = 64
         ENDIF
         RPT_UNIT_NUM = TG_RESERVE_MARGIN_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         IF(CAPACITY_PLANNING_METHOD() == 'MX'  .AND.
     +                           GREEN_MRX_METHOD() == 'GX' .AND.
     +                                       .NOT. HORIZONS_ACTIVE) THEN
            WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                        'Iteration           ','N',INT2(4),'F','D'
            NEXT_REC = NEXT_REC + 1
         ENDIF
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Transaction Group   ','C',INT2(35),'V','D'
         R_NEXT_REC = NEXT_REC + 1
C
!         IF(.NOT. (CAPACITY_PLANNING_METHOD() == 'MX'  .AND.
!     +                           GREEN_MRX_METHOD() == 'GX') .OR.
!     +                                             HORIZONS_ACTIVE) THEN
          IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
!         ENDIF
         TG_RESERVE_MARGIN_HEADER = TG_RESERVE_MARGIN_NO
      RETURN
C***********************************************************************
      ENTRY EXPANSION_PATTERN_HEADER(R_NEXT_REC)
C***********************************************************************
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                                     trim(GET_SCENAME())//".EPD"
         OVERHEAD_LENGTH = 64 !46
         VARIABLE_NUMBER = 24
         DIMENSIONS = 4
         RPT_UNIT_NUM = EXPANSION_PATTERN_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE_WO(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Endpoint            ','N',INT2( 4),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Current Year        ','N',INT2( 4),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Start Year          ','N',INT2( 4),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+3)
     +                       'Resource Name/Status','C',INT2(35),'V','D'
!         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+4)
!     +      'Resource Name       ','C',INT2(24),'V','D'
!         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+5)
!     +      'Status              ','C',INT2(10),'V','D'
C
C
         R_NEXT_REC = NEXT_REC + 4
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         EXPANSION_PATTERN_HEADER = EXPANSION_PATTERN_NO
      RETURN
C***********************************************************************
      ENTRY FUTURE_ASSET_REPT_HEADER(R_NEXT_REC)
C***********************************************************************
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                                     trim(GET_SCENAME())//".NAD"
         OVERHEAD_LENGTH = 68
         VARIABLE_NUMBER = 31
         DIMENSIONS = 4
         RPT_UNIT_NUM = FUTURE_ASSETS_UNIT_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE_WO(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Endpoint            ','N',INT2(4),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Type                ','C',INT2(6),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Asset Name          ','C',INT2(38),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+3)
     +                       'Result Year         ','N',INT2(4),'V','D'
         R_NEXT_REC = NEXT_REC + 4
C
         FUTURE_ASSET_REPT_HEADER = FUTURE_ASSETS_UNIT_NO
      RETURN
C***********************************************************************
      ENTRY FLEX_PLANNING_ACTION_HEADER(R_NEXT_REC)
C***********************************************************************
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                                     trim(GET_SCENAME())//".FXD"
         OVERHEAD_LENGTH = 64 !54
         VARIABLE_NUMBER = 4
         DIMENSIONS = 3
         RPT_UNIT_NUM = FLEX_PLANNING_ACTION_UNIT_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE_WO(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Endpoint            ','N',INT2(4),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Asset Name          ','C',INT2(38),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Action              ','C',INT2(12),'V','D'
         R_NEXT_REC = NEXT_REC + 3
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         FLEX_PLANNING_ACTION_HEADER = FLEX_PLANNING_ACTION_UNIT_NO
      RETURN
C***********************************************************************
      ENTRY FUEL_INVENTORY_HEADER(R_NEXT_REC)
C***********************************************************************
! IN CL_REPORT
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                                     trim(GET_SCENAME())//".FID"
         OVERHEAD_LENGTH = 64 !41
         VARIABLE_NUMBER = 2
         DIMENSIONS = 5
         RPT_UNIT_NUM = FUEL_INVEN_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Month               ','C',INT2(9),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Fuel Source         ','N',INT2(4),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Generating Unit     ','C',INT2(20),'V',
     +                                                               'D'
         R_NEXT_REC = NEXT_REC + 3
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         FUEL_INVENTORY_HEADER = FUEL_INVEN_NO
      RETURN
C***********************************************************************
      ENTRY ANN_CL_UNIT_HEADER(R_NEXT_REC)
C***********************************************************************
! IN CLREPORT
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                                     trim(GET_SCENAME())//".TAD"
         OVERHEAD_LENGTH = 64 !28
         VARIABLE_NUMBER = 16
         DIMENSIONS = 3
         IF(CAPACITY_PLANNING_METHOD() == 'MX'  .AND.
     +                        GREEN_MRX_METHOD() == 'GX') DIMENSIONS = 4
         RPT_UNIT_NUM = ANN_CL_UNIT_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Generating Unit     ','C',INT2(20),'V','D'
         R_NEXT_REC = NEXT_REC + 1
         IF(CAPACITY_PLANNING_METHOD() == 'MX'  .AND.
     +                                  GREEN_MRX_METHOD() == 'GX') THEN
            WRITE(RPT_UNIT_NUM,REC=R_NEXT_REC)
     +                        'Iteration           ','N',INT2(4),'F','D'
            R_NEXT_REC = R_NEXT_REC + 1
         ENDIF
C
         ANN_CL_UNIT_HEADER = ANN_CL_UNIT_NO
      RETURN
C***********************************************************************
      ENTRY MON_CL_UNIT_HEADER(R_NEXT_REC)
C***********************************************************************
! CLREPORT
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                                     trim(GET_SCENAME())//".TMD"
         OVERHEAD_LENGTH = 64 !39
         VARIABLE_NUMBER = 14
         DIMENSIONS = 4
         RPT_UNIT_NUM = MON_CL_UNIT_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Month               ','C',INT2(9),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Unit Segment        ','C',INT2(22),'V','D'
         R_NEXT_REC = NEXT_REC + 2
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         MON_CL_UNIT_HEADER = MON_CL_UNIT_NO
      RETURN
C***********************************************************************
      ENTRY MON_LOAD_BY_BLOCK_HEADER(R_NEXT_REC,R_VARIABLE_NUMBER)
C***********************************************************************
! CLREPORT
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                         trim(GET_SCENAME())//LOAD_BY_BLOCK_CODE
         OVERHEAD_LENGTH = 64 !48
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 5
         RPT_UNIT_NUM = LOAD_BY_BLOCK_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Month               ','C',INT2(9),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Unit Segment        ','C',INT2(22),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Component           ','C',INT2(9),'V','D'
         R_NEXT_REC = NEXT_REC + 3
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         MON_LOAD_BY_BLOCK_HEADER = LOAD_BY_BLOCK_NO
      RETURN
C***********************************************************************
      ENTRY HOURLY_CUST_MARGIN_HEADER(R_NEXT_REC,R_VARIABLE_NUMBER)
C***********************************************************************
! 02/25/04.
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                    trim(GET_SCENAME())//HOURLY_CUST_MARGIN_CODE
         OVERHEAD_LENGTH = 64 !30
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         RPT_UNIT_NUM = HOURLY_CUST_MARGIN_NO
         DIMENSIONS = 5
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Month               ','C',INT2(9),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Hour                ','N',INT2(4),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Variable            ','C',INT2(9),'F','D'
C
         R_NEXT_REC = NEXT_REC + 3
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         HOURLY_CUST_MARGIN_HEADER = HOURLY_CUST_MARGIN_NO
      RETURN
C***********************************************************************
      ENTRY HOURLY_STORAGE_REPORT_HEADER(R_NEXT_REC,R_VARIABLE_NUMBER)
C***********************************************************************
! 02/25/04.
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                        trim(GET_SCENAME())//HOURLY_STORAGE_CODE
         OVERHEAD_LENGTH = 64 !30
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         RPT_UNIT_NUM = HOURLY_STORAGE_UNIT
         DIMENSIONS = 5
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Period              ','C',INT2(9),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Day of Month        ','N',INT2(4),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Transaction Group   ','C',INT2(35),'V','D'
C
         R_NEXT_REC = NEXT_REC + 3
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         HOURLY_STORAGE_REPORT_HEADER = HOURLY_STORAGE_UNIT
      RETURN
C***********************************************************************
      ENTRY IPL_FAC_HEADER(R_NEXT_REC,R_VARIABLE_NUMBER)
C***********************************************************************
! CLREPORT
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                         trim(GET_SCENAME())//IPL_FAC_CODE
         OVERHEAD_LENGTH = 64 !26
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 4
         RPT_UNIT_NUM = IPL_FAC_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Month               ','C',INT2(9),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Component           ','C',INT2(9),'V','D'
         R_NEXT_REC = NEXT_REC + 2
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         IPL_FAC_HEADER = IPL_FAC_NO
      RETURN
C***********************************************************************
      ENTRY ANN_CL_SUMMARY_HEADER(R_NEXT_REC)
C***********************************************************************
! CLREPORT
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                                     trim(GET_SCENAME())//".TSD"
         OVERHEAD_LENGTH = 8
         VARIABLE_NUMBER = 27
         DIMENSIONS = 2
         RPT_UNIT_NUM = ANN_CL_SUM_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         R_NEXT_REC = NEXT_REC
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         ANN_CL_SUMMARY_HEADER = ANN_CL_SUM_NO
      RETURN
C***********************************************************************
      ENTRY MON_CT_UNIT_HEADER(R_NEXT_REC)
C***********************************************************************
! CLREPORT
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                                     trim(GET_SCENAME())//".CMD"
         OVERHEAD_LENGTH = 37
         VARIABLE_NUMBER = 13
         DIMENSIONS = 4
         RPT_UNIT_NUM = MON_CT_UNIT_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Month               ','C',INT2(9),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Contract            ','C',INT2(20),'V','D'
         R_NEXT_REC = NEXT_REC + 2
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         MON_CT_UNIT_HEADER = MON_CT_UNIT_NO
      RETURN
C***********************************************************************
      ENTRY MON_CL_TRANS_UNIT_HEADER(R_NEXT_REC,R_VARIABLE_NUMBER,
     +                               R_RECORD_LENGTH)
C***********************************************************************
! CLREPORT
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                          trim(GET_SCENAME())//MON_CL_TRANS_CODE
         OVERHEAD_LENGTH = 64 !42
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
!         VARIABLE_NUMBER = 36
         DIMENSIONS = 4
         RPT_UNIT_NUM = MON_CL_TRANS_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Month               ','C',INT2(9),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +              'Generating Unit     ','C',INT2(25),'V','D',INT2(25)
         R_NEXT_REC = NEXT_REC + 2
C
         R_RECORD_LENGTH = RECORD_LENGTH
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         MON_CL_TRANS_UNIT_HEADER = MON_CL_TRANS_NO
      RETURN
C***********************************************************************
      ENTRY MON_DV_TRANS_UNIT_HEADER(R_NEXT_REC,R_VARIABLE_NUMBER,
     +                               R_RECORD_LENGTH)
C***********************************************************************
! CLREPORT
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                          trim(GET_SCENAME())//MON_DV_TRANS_CODE
         OVERHEAD_LENGTH = 64 !42
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
!         VARIABLE_NUMBER = 36
         DIMENSIONS = 4
         RPT_UNIT_NUM = MON_DV_TRANS_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Month               ','C',INT2(9),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +              'Generating Unit     ','C',INT2(25),'V','D',INT2(25)
         R_NEXT_REC = NEXT_REC + 2
C
         R_RECORD_LENGTH = RECORD_LENGTH
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         MON_DV_TRANS_UNIT_HEADER = MON_DV_TRANS_NO
      RETURN
C***********************************************************************
      ENTRY MON_NU_TRANS_UNIT_HEADER(R_NEXT_REC,R_VARIABLE_NUMBER,
     +                               R_RECORD_LENGTH)
C***********************************************************************
! CLREPORT
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                          trim(GET_SCENAME())//MON_NU_TRANS_CODE
         OVERHEAD_LENGTH = 64 !42
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
!         VARIABLE_NUMBER = 36
         DIMENSIONS = 4
         RPT_UNIT_NUM = MON_NU_TRANS_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Month               ','C',INT2(9),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +              'Generating Unit     ','C',INT2(25),'V','D',INT2(25)
         R_NEXT_REC = NEXT_REC + 2
C
         R_RECORD_LENGTH = RECORD_LENGTH
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         MON_NU_TRANS_UNIT_HEADER = MON_NU_TRANS_NO
      RETURN
C***********************************************************************
      ENTRY ANN_POOL_SUMMARY_HEADER(R_NEXT_REC)
C***********************************************************************
! CLREPORT
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                                     trim(GET_SCENAME())//".PSD"
         OVERHEAD_LENGTH = 14
         VARIABLE_NUMBER = 12
         DIMENSIONS = 3
         RPT_UNIT_NUM = ANN_POOL_SUM_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Member              ','C',INT2(6),'F','D'
         R_NEXT_REC = NEXT_REC + 1
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         ANN_POOL_SUMMARY_HEADER = ANN_POOL_SUM_NO
      RETURN
C***********************************************************************
      ENTRY MON_CL_SUMMARY_HEADER(R_NEXT_REC)
C***********************************************************************
! CLREPORT
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                                     trim(GET_SCENAME())//".TTD"
         OVERHEAD_LENGTH = 17
         VARIABLE_NUMBER = 31
         DIMENSIONS = 3
         RPT_UNIT_NUM = MON_CL_SUM_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Month               ','C',INT2(9),'F','D'
         R_NEXT_REC = NEXT_REC + 1
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         MON_CL_SUMMARY_HEADER = MON_CL_SUM_NO
      RETURN
C***********************************************************************
      ENTRY TRANSACT_FUEL_RPT_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)
C***********************************************************************
! CLREPORT
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                             trim(GET_SCENAME())//TRANS_FUL_CODE
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 4
         RPT_UNIT_NUM = TRANS_FUL_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Period              ','C',INT2(9),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Transaction Group   ','C',INT2(35),'V','D'
         R_NEXT_REC = NEXT_REC + 2
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         TRANSACT_FUEL_RPT_HEADER = TRANS_FUL_UNIT
      RETURN
C***********************************************************************
      ENTRY TRANSACT_STATE_RPT_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)
C***********************************************************************
! CLREPORT
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                           trim(GET_SCENAME())//TRANS_STATE_CODE
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 4
         RPT_UNIT_NUM = TRANS_STATE_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Period              ','C',INT2(9),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'State               ','C',INT2(20),'V','D'
         R_NEXT_REC = NEXT_REC + 2
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         TRANSACT_STATE_RPT_HEADER = TRANS_STATE_UNIT
      RETURN
C***********************************************************************
      ENTRY STATE_RPS_RPT_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)
C***********************************************************************
! CLREPORT
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                             trim(GET_SCENAME())//STATE_RPS_CODE
         OVERHEAD_LENGTH = 37
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 4
         RPT_UNIT_NUM = STATE_RPS_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Month               ','C',INT2(9),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'State or Province   ','C',INT2(20),'V','D'
         R_NEXT_REC = NEXT_REC + 2
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         STATE_RPS_RPT_HEADER = STATE_RPS_UNIT
      RETURN
C***********`************************************************************
      ENTRY RPS_PROGRAMS_RPT_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)
C***********************************************************************
! CLREPORT NOT A RESET REPORT
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                         trim(GET_SCENAME())//RPS_PROGRAMS_CODE
         OVERHEAD_LENGTH = 58
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 3
         RPT_UNIT_NUM = RPS_PROGRAMS_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Program             ','C',INT2(50),'V','D'
         R_NEXT_REC = NEXT_REC + 1
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
C
         RPS_PROGRAMS_RPT_HEADER = RPS_PROGRAMS_UNIT
      RETURN
C***********`************************************************************
      ENTRY CO2_ABATEMENT_RPT_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)
C***********************************************************************
! CLREPORT NOT A RESET REPORT
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                         trim(GET_SCENAME())//CO2_ABATEMENT_CODE
         OVERHEAD_LENGTH = 60
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 3
         IF(CAPACITY_PLANNING_METHOD() == 'MX'  .AND.
     +                        GREEN_MRX_METHOD() == 'GX') DIMENSIONS = 4
         RPT_UNIT_NUM = CO2_ABATEMENT_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Unit                ','C',INT2(48),'V','D'
         R_NEXT_REC = NEXT_REC + 1
         IF(CAPACITY_PLANNING_METHOD() == 'MX'  .AND.
     +                                  GREEN_MRX_METHOD() == 'GX') THEN
            WRITE(RPT_UNIT_NUM,REC=R_NEXT_REC)
     +                        'Iteration           ','N',INT2(4),'F','D'
            R_NEXT_REC = R_NEXT_REC + 1
         ENDIF
C
         CO2_ABATEMENT_RPT_HEADER = CO2_ABATEMENT_UNIT
      RETURN
C***********************************************************************
      ENTRY MARGINAL_UNIT_TECH_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)
C***********************************************************************
! MARGNOBJ
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                    trim(GET_SCENAME())//MARGINAL_UNIT_TECH_CODE
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 4
         RPT_UNIT_NUM = MARGINAL_UNIT_TECH_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Period              ','C',INT2(9),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Transaction Group   ','C',INT2(35),'V','D'
         R_NEXT_REC = NEXT_REC + 2
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         MARGINAL_UNIT_TECH_HEADER = MARGINAL_UNIT_TECH_UNIT
      RETURN
C***********************************************************************
      ENTRY TRANSACT_PROD_RPT_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)
C***********************************************************************
! CLREPORT
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                            trim(GET_SCENAME())//TRANS_PROD_CODE
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 4
         RPT_UNIT_NUM = TRANS_PROD_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Period              ','C',INT2(9),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Transaction Group   ','C',INT2(35),'V','D'
         R_NEXT_REC = NEXT_REC + 2
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         TRANSACT_PROD_RPT_HEADER = TRANS_PROD_UNIT
      RETURN
C***********************************************************************
      ENTRY TRAN_PROD_FUEL_RPT_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)
C***********************************************************************
! CLREPORT
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                        trim(GET_SCENAME())//TRAN_PROD_FUEL_CODE
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 4
         RPT_UNIT_NUM = TRAN_PROD_FUEL_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Period              ','C',INT2(9),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Transaction Group   ','C',INT2(35),'V','D'
         R_NEXT_REC = NEXT_REC + 2
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         TRAN_PROD_FUEL_RPT_HEADER = TRAN_PROD_FUEL_UNIT
      RETURN
C***********************************************************************
      ENTRY TRANSACT_PROD_PROD_RPT_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)
C***********************************************************************
! CLREPORT
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                       trim(GET_SCENAME())//TRANS_PROD_PROD_CODE
         OVERHEAD_LENGTH = 80
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 5
         RPT_UNIT_NUM = TRANS_PROD_PROD_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Period              ','C',INT2(9),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Transaction Group   ','C',INT2(35),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Product Name        ','C',INT2(20),'V','D'
         R_NEXT_REC = NEXT_REC + 3
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         TRANSACT_PROD_PROD_RPT_HEADER = TRANS_PROD_PROD_UNIT
      RETURN
C***********************************************************************
      ENTRY TRANSACT_PROD_MW_RPT_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)
C***********************************************************************
! CLREPORT
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                       trim(GET_SCENAME())//TRANS_PROD_MW_CODE
         OVERHEAD_LENGTH = 80 !57
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 5
         RPT_UNIT_NUM = TRANS_PROD_MW_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Period              ','C',INT2(9),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Transaction Group   ','C',INT2(35),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Product Name        ','C',INT2(20),'V','D'
         R_NEXT_REC = NEXT_REC + 3
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         TRANSACT_PROD_MW_RPT_HEADER = TRANS_PROD_MW_UNIT
      RETURN
C***********************************************************************
      ENTRY RPS_CURVE_RPT_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)
C***********************************************************************
! CLREPORT
C
         FILE_NAME = TRIM(GET_RESULTS_DIRECTORY())//"MSG"//
     +                            TRIM(GET_SCENAME())//RPS_CURVE_CODE
         OVERHEAD_LENGTH = 83 ! 64
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 4
         RPT_UNIT_NUM = RPS_CURVE_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Program             ','C',INT2(50),'V','D'
!         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
!     +                       'Iteration           ','N',INT2(4),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Resource            ','C',INT2(25),'V','D'
         R_NEXT_REC = NEXT_REC + 2
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         RPS_CURVE_RPT_HEADER = RPS_CURVE_UNIT
      RETURN
C***********************************************************************
      ENTRY TRANSACT_PROD_FUEL_RPT_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)
C***********************************************************************
! CLREPORT
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                       trim(GET_SCENAME())//TRANS_PROD_FUEL_CODE
         OVERHEAD_LENGTH = 80!57
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 5
         RPT_UNIT_NUM = TRANS_PROD_FUEL_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Period              ','C',INT2(9),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Transaction Group   ','C',INT2(35),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Product Name        ','C',INT2(20),'V','D'
         R_NEXT_REC = NEXT_REC + 3
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         TRANSACT_PROD_FUEL_RPT_HEADER = TRANS_PROD_FUEL_UNIT
      RETURN
C***********************************************************************
      ENTRY SERVICE_TRANS_HEADER(R_NEXT_REC)
C***********************************************************************
! SERVICAC FILE
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                                     trim(GET_SCENAME())//".STD"
         OVERHEAD_LENGTH = 56
         VARIABLE_NUMBER = 7
         DIMENSIONS = 6
         RPT_UNIT_NUM = TRANS_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Month               ','C',INT2(9),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Description         ','C',INT2(20),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Revenue/Expense     ','C',INT2(7),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+3)
     +                       'Type                ','C',INT2(12),'V','D'
         R_NEXT_REC = NEXT_REC + 4
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         SERVICE_TRANS_HEADER = TRANS_NO
      RETURN
C***********************************************************************
      ENTRY ANN_EL_UNIT_HEADER(R_NEXT_REC)
C***********************************************************************
! NOT FOUND IN ANY ROUTINE

         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                                     trim(GET_SCENAME())//".EAD"
         OVERHEAD_LENGTH = 28
         VARIABLE_NUMBER = 16
         DIMENSIONS = 3
         RPT_UNIT_NUM = ANN_EL_UNIT_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Generating Unit     ','C',INT2(20),'V','D'
         R_NEXT_REC = NEXT_REC + 1
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         ANN_EL_UNIT_HEADER = ANN_EL_UNIT_NO
C
      RETURN
C***********************************************************************
      ENTRY MON_EL_UNIT_HEADER(R_NEXT_REC)
C***********************************************************************

C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                                     trim(GET_SCENAME())//".EMD"
         OVERHEAD_LENGTH = 40
         VARIABLE_NUMBER = 11
         DIMENSIONS = 4
         RPT_UNIT_NUM = MON_EL_UNIT_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Month               ','C',INT2(9),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Energy Limited Unit ','C',INT2(20),'V','D'
         R_NEXT_REC = NEXT_REC + 2
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         MON_EL_UNIT_HEADER = MON_EL_UNIT_NO
C
      RETURN
C***********************************************************************
      ENTRY ASSET_PROD_RPT_HEADER(R_NEXT_REC)
C***********************************************************************

C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                            trim(GET_SCENAME())//ASSET_PROD_CODE
         OVERHEAD_LENGTH = 38
         VARIABLE_NUMBER = 8
         DIMENSIONS = 3
         RPT_UNIT_NUM = ASSET_PRODUCTION_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Asset Class Name    ','C',INT2(30),'V','D'
         R_NEXT_REC = NEXT_REC + 1
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         ASSET_PROD_RPT_HEADER = ASSET_PRODUCTION_UNIT
      RETURN
C***********************************************************************
      ENTRY SCREENING_HEADER(R_NEXT_REC)
C***********************************************************************
! CAP_OBJT
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                                     trim(GET_SCENAME())//".SCD"
         OVERHEAD_LENGTH = 28
         VARIABLE_NUMBER = 11
         DIMENSIONS = 3
         RPT_UNIT_NUM = SCREENING_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Expansion Option    ','C',INT2(20),'V','D'
         R_NEXT_REC = NEXT_REC + 1
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         SCREENING_HEADER = SCREENING_NO
      RETURN
C***********************************************************************
      ENTRY MX_ANNUAL_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)
C***********************************************************************
! CAP_OBJT
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                         trim(GET_SCENAME())//MX_ANNUAL_ALT_CODE
         OVERHEAD_LENGTH = 70 !50
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         RPT_UNIT_NUM = MX_ANNUAL_ALT_NO
         DIMENSIONS = 4
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Transaction Group   ','C',INT2(35),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Resource            ','C',INT2(22),'F','D'
         R_NEXT_REC = NEXT_REC + 2
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         MX_ANNUAL_HEADER = MX_ANNUAL_ALT_NO
      RETURN
C***********************************************************************
      ENTRY GRX_ANNUAL_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)
C***********************************************************************
! CAP_OBJT
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                 trim(GET_SCENAME())//GRX_DETAILED_RESOURCE_CODE
         OVERHEAD_LENGTH = 76
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         RPT_UNIT_NUM = GRX_DETAILED_RESOURCE_UNIT
         DIMENSIONS = 5
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Transaction Group   ','C',INT2(35),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Resource            ','C',INT2(22),'F','D'
         R_NEXT_REC = NEXT_REC + 2
         WRITE(RPT_UNIT_NUM,REC=R_NEXT_REC)
     +                        'Iteration           ','N',INT2(4),'F','D'
         R_NEXT_REC = R_NEXT_REC + 1
C
         GRX_ANNUAL_HEADER = GRX_DETAILED_RESOURCE_UNIT
      RETURN
C***********************************************************************
      ENTRY GRX_8761_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)
C***********************************************************************
! CAP_OBJT
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                              trim(GET_SCENAME())//GRX_8761_CODE
         OVERHEAD_LENGTH = 63
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         RPT_UNIT_NUM = GRX_8761_UNIT
         DIMENSIONS = 4
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Variable            ','C',INT2(20),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Selling Trans Group ','C',INT2(35),'F','D'
         R_NEXT_REC = NEXT_REC + 2
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
C
         GRX_8761_HEADER = GRX_8761_UNIT
      RETURN
C***********************************************************************
      ENTRY GAS_PIPE_EXPANSION_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)
C***********************************************************************
! CAP_OBJT
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                    trim(GET_SCENAME())//GAS_PIPE_EXPANSION_CODE
         OVERHEAD_LENGTH = 58
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         RPT_UNIT_NUM = GAS_PIPE_EXPANSION_UNIT
         DIMENSIONS = 3
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Pipeline Expansion  ','C',INT2(50),'F','D'
         R_NEXT_REC = NEXT_REC + 1
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         GAS_PIPE_EXPANSION_HEADER = GAS_PIPE_EXPANSION_UNIT
      RETURN
C***********************************************************************
      ENTRY MX_ICAP_ANNUAL_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)
C***********************************************************************
! CAP_OBJT
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                    trim(GET_SCENAME())//MX_ICAP_ANNUAL_ALT_CODE
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         RPT_UNIT_NUM = MX_ICAP_ANNUAL_ALT_NO
         DIMENSIONS = 4
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Period              ','C',INT2(9),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Transaction Group   ','C',INT2(35),'V','D'
         R_NEXT_REC = NEXT_REC + 2
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         MX_ICAP_ANNUAL_HEADER = MX_ICAP_ANNUAL_ALT_NO
      RETURN
C***********************************************************************
      ENTRY CM_MAINT_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)
C***********************************************************************
! NEW CLM_OBJT AVAILABILITY REPORT
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                              trim(GET_SCENAME())//CM_MAINT_CODE
         OVERHEAD_LENGTH = 34
!         OVERHEAD_LENGTH = 28
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 3
         RPT_UNIT_NUM = CM_MAINT_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Generating Unit     ','C',INT2(25),'V','D'
!     +                       'Generating Unit     ','C',INT2(20),'V','D'
         R_NEXT_REC = NEXT_REC + 1
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         CM_MAINT_HEADER = CM_MAINT_NO
      RETURN
C***********************************************************************
      ENTRY MAINTENANCE_HEADER(R_MW_MAINT_NO,
     +                         R_CL_NEXT_REC,
     +                         R_MW_NEXT_REC)
C***********************************************************************
! CLA_OBJT
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                                     trim(GET_SCENAME())//".UMD"
         OVERHEAD_LENGTH = 28
         VARIABLE_NUMBER = 39
         DIMENSIONS = 3
         RPT_UNIT_NUM = CL_MAINT_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Generating Unit     ','C',INT2(20),'V','D'
         R_CL_NEXT_REC = NEXT_REC + 1
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_CL_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         MAINTENANCE_HEADER = CL_MAINT_NO
C
! OPENS GROUP MAINTENANCE HEADER
C***********************************************************************
         R_MW_MAINT_NO = MW_MAINT_NO
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                                     trim(GET_SCENAME())//".WMD"
         OVERHEAD_LENGTH = 28
         VARIABLE_NUMBER = 39
         DIMENSIONS = 3
         RPT_UNIT_NUM = MW_MAINT_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Group Name          ','C',INT2(9),'V','D'
         R_MW_NEXT_REC = NEXT_REC + 1
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_MW_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
      RETURN
C***********************************************************************
      ENTRY MARGINAL_COST_HEADER(R_NEXT_REC)
C***********************************************************************
! MARGNOBJ
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                                     trim(GET_SCENAME())//".MCD"
         OVERHEAD_LENGTH = 21
         VARIABLE_NUMBER = 4
         DIMENSIONS = 4
         RPT_UNIT_NUM = MAR_COST_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Month               ','C',INT2(9),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Load Segment        ','N',INT2(4),'V','D'
         R_NEXT_REC = NEXT_REC + 2
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         MARGINAL_COST_HEADER = MAR_COST_NO
      RETURN
C***********************************************************************
      ENTRY TRANS_CONTRIBUTION_RPT_HEADER(R_NEXT_REC)
C***********************************************************************
! MARGNOBJ
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                          trim(GET_SCENAME())//TRANS_CON_CODE
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = 5
         DIMENSIONS = 5
         RPT_UNIT_NUM = TRANS_CON_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Period              ','C',INT2(9),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Hour of the Month   ','N',INT2(4),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Transaction Group   ','C',INT2(35),'V','D'
         R_NEXT_REC = NEXT_REC + 3
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         TRANS_CONTRIBUTION_RPT_HEADER = TRANS_CON_UNIT
      RETURN
C***********************************************************************
      ENTRY MARGINAL_DATA_BASE_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)
C***********************************************************************
! MARGNOBJ
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                                     trim(GET_SCENAME())//".MGD"
         OVERHEAD_LENGTH = 20
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         RECORD_LENGTH = MAX(1000,OVERHEAD_LENGTH + 4*VARIABLE_NUMBER)
         OPEN(MAR_DATA_NO,FILE=FILE_NAME,
     +              ACCESS="DIRECT",STATUS="REPLACE",RECL=RECORD_LENGTH)
C
! 1\8\96 END_POINTS MAY NOT BE DEFINED.
         WRITE(MAR_DATA_NO,REC=1) GET_NUM_OF_END_POINTS(),
     +                            INT2(ENDYR()-BASE_YEAR())
         R_NEXT_REC = 2
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         MARGINAL_DATA_BASE_HEADER = MAR_DATA_NO
      RETURN
C***********************************************************************
      ENTRY ECONOMY_INTER_HEADER(R_NEXT_REC)
C***********************************************************************
! ECONINTR
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                                     trim(GET_SCENAME())//".ECD"
         OVERHEAD_LENGTH = 21
         VARIABLE_NUMBER = 12
         DIMENSIONS = 4
         RPT_UNIT_NUM = ECONOMY_INTER_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Month               ','C',INT2(9),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Transaction Point   ','N',INT2(4),'V','D'
         R_NEXT_REC = NEXT_REC + 2
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         ECONOMY_INTER_HEADER = ECONOMY_INTER_NO
      RETURN
C***********************************************************************
      ENTRY GET_EXT_ECONOMY_INTER_NO
C***********************************************************************
! ECONINTR
C
        GET_EXT_ECONOMY_INTER_NO = EXTERNAL_ECONOMY_INTER_NO
      RETURN
C***********************************************************************
      ENTRY DISPATCH_ORDER_HEADER(R_NEXT_REC)
C***********************************************************************
! PICADISP
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                                     trim(GET_SCENAME())//".DOD"
         OVERHEAD_LENGTH = 39
         VARIABLE_NUMBER = 40
         DIMENSIONS = 4
         RPT_UNIT_NUM = DISP_ORDER_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Period              ','C',INT2(9),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Capacity Segment    ','C',INT2(22),'V','D'
         R_NEXT_REC = NEXT_REC + 2
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         DISPATCH_ORDER_HEADER = DISP_ORDER_NO
      RETURN
C***********************************************************************
      ENTRY TRANS_DISPATCH_RPT_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)
C***********************************************************************
! PICADISP
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                  trim(GET_SCENAME())//MONTHLY_DISPATCH_RPT_CODE
         OVERHEAD_LENGTH = 80
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         RPT_UNIT_NUM = MONTHLY_DISPATCH_RPT_NO
         DIMENSIONS = 5
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Tranaction Group    ','C',INT2(40),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Period              ','C',INT2(9),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Capacity Segment    ','C',INT2(22),'V','D'
         R_NEXT_REC = NEXT_REC + 3
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         TRANS_DISPATCH_RPT_HEADER = RPT_UNIT_NUM
      RETURN
C***********************************************************************
      ENTRY NEW_SUPPLY_CURVE_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)
C***********************************************************************
! PICADISP
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                  trim(GET_SCENAME())//NEW_SUPPLY_CURVE_CODE
         OVERHEAD_LENGTH = 72
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         RPT_UNIT_NUM = NEW_SUPPLY_CURVE_UNIT
         DIMENSIONS = 5
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Period              ','C',INT2(9),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Transaction Group   ','C',INT2(35),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Generating Resource ','C',INT2(20),'V','D'
         R_NEXT_REC = NEXT_REC + 3
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         NEW_SUPPLY_CURVE_HEADER = RPT_UNIT_NUM
      RETURN
C***********************************************************************
      ENTRY CAPEX_THERMAL_RPT_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)
C***********************************************************************
! IN PICADISP
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                                    trim(GET_SCENAME())//".XCD"
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         RPT_UNIT_NUM = 3247 ! MONTHLY_DISPATCH_RPT_NO
         DIMENSIONS = 4
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Month               ','N',INT2(4),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Unit Name           ','C',INT2(48),'V','D'
         R_NEXT_REC = NEXT_REC + 2
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         CAPEX_THERMAL_RPT_HEADER = RPT_UNIT_NUM
      RETURN
C***********************************************************************
      ENTRY WABASH_VALLEY_POWER_COST_HEADER(R_NEXT_REC)
C***********************************************************************
! REAENRG
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +              trim(GET_SCENAME())//WABASH_VALLEY_POWER_COST_CODE
         OVERHEAD_LENGTH = 38
         VARIABLE_NUMBER = 30 ! 03/19/04 ! ADDED TRACKERS 20
         DIMENSIONS = 4
         RPT_UNIT_NUM = WABASH_VALLEY_POWER_COST_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Period              ','C',INT2(9),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Resource            ','C',INT2(20),'V','D'
         R_NEXT_REC = NEXT_REC + 2
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         WABASH_VALLEY_POWER_COST_HEADER = WABASH_VALLEY_POWER_COST_UNIT
      RETURN
C***********************************************************************
      ENTRY WABASH_VALLEY_POWER_COST_NO
C***********************************************************************
         WABASH_VALLEY_POWER_COST_NO = WABASH_VALLEY_POWER_COST_UNIT
      RETURN
C***********************************************************************
      ENTRY WABASH_VALLEY_POWER_DSM_HEADER(R_NEXT_REC)
C***********************************************************************
! REAENRG
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +               trim(GET_SCENAME())//WABASH_VALLEY_POWER_DSM_CODE
         OVERHEAD_LENGTH = 32
         VARIABLE_NUMBER = 3
         DIMENSIONS = 4
         RPT_UNIT_NUM = WABASH_VALLEY_POWER_DSM_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Period              ','C',INT2(9),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Area                ','C',INT2(15),'V','D'
         R_NEXT_REC = NEXT_REC + 2
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         WABASH_VALLEY_POWER_DSM_HEADER = WABASH_VALLEY_POWER_DSM_UNIT
      RETURN
C***********************************************************************
      ENTRY WV_AREA_REPORT_HEADER(R_NEXT_REC)
C***********************************************************************
! REAENRG
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                               trim(GET_SCENAME())//WV_AREA_CODE
         OVERHEAD_LENGTH = 32
         VARIABLE_NUMBER = 13
         DIMENSIONS = 4
         RPT_UNIT_NUM = WV_AREA_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Period              ','C',INT2(9),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Area                ','C',INT2(15),'V','D'
         R_NEXT_REC = NEXT_REC + 2
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         WV_AREA_REPORT_HEADER = WV_AREA_UNIT
      RETURN
C***********************************************************************
      ENTRY ASSET_CLASS_INCOME_HEADER(R_START_RECORD,R_VARIABLE_NUMBER)
C***********************************************************************
! ASSET_CLASS_ANALYSIS
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                                     trim(GET_SCENAME())//".AID"
         OVERHEAD_LENGTH = 46
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 3
         RPT_UNIT_NUM = ASSET_INCOME_STATEMENT_NUM
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Asset Class Name    ','C',INT2(38),'F','D'
C
         R_START_RECORD = NEXT_REC + 1 !3 + DIMENSIONS
         ASSET_CLASS_INCOME_HEADER = ASSET_INCOME_STATEMENT_NUM
      RETURN
C***********************************************************************
      ENTRY FE_PCA_LEGAL_HEADER()
C***********************************************************************
! ASSET_CLASS_ANALYSIS
C
         FILE_NAME = TRIM(GET_RESULTS_DIRECTORY())//"MSG"//
     +                                     TRIM(GET_SCENAME())//".FED"
         OVERHEAD_LENGTH = 46
         VARIABLE_NUMBER = 300
         DIMENSIONS = 3
         RPT_UNIT_NUM = 2710
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Asset Class Name    ','C',INT2(38),'F','D'
C
         FE_PCA_LEGAL_HEADER = RPT_UNIT_NUM
      RETURN
C***********************************************************************
      ENTRY FE_MONTHLY_PCA_LEGAL_HEADER()
C***********************************************************************
! ASSET_CLASS_ANALYSIS
C
         FILE_NAME = TRIM(GET_RESULTS_DIRECTORY())//"MSG"//
     +                                     TRIM(GET_SCENAME())//".MFD"
         OVERHEAD_LENGTH = 46
         VARIABLE_NUMBER = 300
         DIMENSIONS = 4
         RPT_UNIT_NUM = 6742
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Asset Class Name    ','C',INT2(38),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Period              ','C',INT2(10),'V','D'
C
         FE_MONTHLY_PCA_LEGAL_HEADER = RPT_UNIT_NUM
      RETURN
C***********************************************************************
      ENTRY CPL_EMC_CAP_REVS_RPT_HEADER(R_NEXT_REC)
C***********************************************************************
! CAT2OBJT
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                              trim(GET_SCENAME())//CPL_ECAP_CODE
         OVERHEAD_LENGTH = 17
         VARIABLE_NUMBER = 21
         DIMENSIONS = 3
         RPT_UNIT_NUM = CPL_ECAP_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Period              ','C',INT2(9),'V','D'
         R_NEXT_REC = NEXT_REC + 1
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         CPL_EMC_CAP_REVS_RPT_HEADER = CPL_ECAP_UNIT
      RETURN
C***********************************************************************
      ENTRY CPL_PAEN_RPT_HEADER(R_NEXT_REC)
C***********************************************************************
! CAT2OBJT
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                              trim(GET_SCENAME())//CPL_PAEN_CODE
         OVERHEAD_LENGTH = 37
         VARIABLE_NUMBER = 10
         DIMENSIONS = 4
         RPT_UNIT_NUM = CPL_PAEN_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Period              ','C',INT2(9),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'PA Resource         ','C',INT2(20),'V','D'
         R_NEXT_REC = NEXT_REC + 2
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         CPL_PAEN_RPT_HEADER = CPL_PAEN_UNIT
      RETURN
C***********************************************************************
      ENTRY HOURLY_ECAP_RPT_HEADER(R_NEXT_REC)
C***********************************************************************
! CAT2OBJT
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                           trim(GET_SCENAME())//HOURLY_ECAP_CODE
         OVERHEAD_LENGTH = 21
         VARIABLE_NUMBER = 25
         DIMENSIONS = 4
         RPT_UNIT_NUM = HOURLY_ECAP_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Period              ','C',INT2(9),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Hour of Month       ','N',INT2(4),'V','D'
         R_NEXT_REC = NEXT_REC + 2
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         HOURLY_ECAP_RPT_HEADER = HOURLY_ECAP_UNIT
      RETURN
C***********************************************************************
      ENTRY CPL_SALES_GEN_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)
C***********************************************************************
! CAT2OBJT
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                         trim(GET_SCENAME())//CPL_SALES_GEN_CODE
         OVERHEAD_LENGTH = 68
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         RPT_UNIT_NUM = CPL_SALES_GEN_UNIT
         DIMENSIONS = 4
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Month               ','C',INT2(9),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Asset Class Name    ','C',INT2(38),'F','D'
         R_NEXT_REC = NEXT_REC + 2
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         CPL_SALES_GEN_HEADER = CPL_SALES_GEN_UNIT
      RETURN
C***********************************************************************
      ENTRY CPL_DEFERRED_FUEL_RPT_HEADER(R_NEXT_REC)
C***********************************************************************
! CPLDEFUL
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"CPL"//
     +                     trim(GET_SCENAME())//CPL_DEFERRED_FUEL_CODE
         OVERHEAD_LENGTH = 47
         VARIABLE_NUMBER = 50
         DIMENSIONS = 3
         RPT_UNIT_NUM = CPL_DEFERRRED_FUEL_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                        'Period              ','C',INT2(6),'F','D'
         R_NEXT_REC = NEXT_REC + 1
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         CPL_DEFERRED_FUEL_RPT_HEADER = CPL_DEFERRRED_FUEL_UNIT
      RETURN
C***********************************************************************
      ENTRY TRANS_TIE_RPT_HEADER(R_NEXT_REC)
C***********************************************************************
! TRANSOBJ
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                             trim(GET_SCENAME())//TRANS_TIE_CODE
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = 24
         DIMENSIONS = 5
         RPT_UNIT_NUM = TRANS_TIE_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Period              ','C',INT2(9),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Day of Month        ','N',INT2(4),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                      'Transaction Group   ','C',INT2(35),'V','D'
         R_NEXT_REC = NEXT_REC + 3
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         TRANS_TIE_RPT_HEADER = TRANS_TIE_UNIT
      RETURN
C***********************************************************************
      ENTRY DAILY_PRODUCTS_RPT_HEADER(R_NEXT_REC)
C***********************************************************************
! TRANSOBJ
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                            trim(GET_SCENAME())//DAILY_PROD_CODE
         OVERHEAD_LENGTH = 90
         VARIABLE_NUMBER = 9 ! 8
         DIMENSIONS = 5
         RPT_UNIT_NUM = DAILY_PRODUCTS_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Period              ',
     +                                               'C',INT2(9),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Transaction Group   ','C',
     +                                                  INT2(35),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Type of Product     ',
     +                                              'C',INT2(20),'V','D'
         R_NEXT_REC = NEXT_REC + 3
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         DAILY_PRODUCTS_RPT_HEADER = DAILY_PRODUCTS_UNIT
      RETURN
C***********************************************************************
      ENTRY SCENARIO_MAKER_RPT_HEADER(R_NEXT_REC)
C***********************************************************************
! TRANSOBJ
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                        trim(GET_SCENAME())//SCENARIO_MAKER_CODE
         OVERHEAD_LENGTH =  42
         VARIABLE_NUMBER = 16
         DIMENSIONS = 4
         RPT_UNIT_NUM = SCENARIO_MAKER_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
!         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
!     +                       'Period              ',
!     +                                               'C',INT2(9),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Time Frame          ','C',
     +                                                  INT2(9),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Uncertainty Variable',
     +                                              'C',INT2(25),'V','D'
         R_NEXT_REC = NEXT_REC + 2
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         SCENARIO_MAKER_RPT_HEADER = SCENARIO_MAKER_UNIT
      RETURN
C***********************************************************************
      ENTRY REGIONAL_MAKER_RPT_HEADER(R_NEXT_REC)
C***********************************************************************
! TRANSOBJ
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                        trim(GET_SCENAME())//REGIONAL_MAKER_CODE
         OVERHEAD_LENGTH =  46
         VARIABLE_NUMBER = 16
         DIMENSIONS = 5
         RPT_UNIT_NUM = REGIONAL_MAKER_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
!         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
!     +                       'Period              ',
!     +                                               'C',INT2(9),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Time Frame          ','C',
     +                                                   INT2(9),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Uncertainty Variable',
     +                                              'C',INT2(25),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Transaction Group   ','N',
     +                                                   INT2(4),'V','D'
         R_NEXT_REC = NEXT_REC + 3
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         REGIONAL_MAKER_RPT_HEADER = REGIONAL_MAKER_UNIT
      RETURN
C***********************************************************************
      ENTRY MONTHLY_FUEL_DERIV_RPT_HEADER(R_NEXT_REC,R_VARIABLE_NUMBER)
C***********************************************************************
! TRANSOBJ
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                    trim(GET_SCENAME())//MONTHLY_FUEL_DERIV_CODE
         OVERHEAD_LENGTH =  37
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 4
         RPT_UNIT_NUM = MONTHLY_FUEL_DERIV_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Period              ',
     +                                               'C',INT2(9),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Fuel Derivative     ','C',
     +                                                  INT2(20),'V','D'
         R_NEXT_REC = NEXT_REC + 2
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         MONTHLY_FUEL_DERIV_RPT_HEADER = MONTHLY_FUEL_DERIV_UNIT
      RETURN
C***********************************************************************
      ENTRY C_DEPTH_MARKET_RPT_HEADER(R_NEXT_REC)
C***********************************************************************
! TRANSOBJ
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                        trim(GET_SCENAME())//C_DEPTH_MARKET_CODE
         OVERHEAD_LENGTH = 72
         VARIABLE_NUMBER = 4
         DIMENSIONS = 5
         RPT_UNIT_NUM = C_DEPTH_MARKET_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Period              ',
     +                                               'C',INT2(9),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Transaction Group   ','C',
     +                                                  INT2(35),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Type of Product     ',
     +                                              'C',INT2(20),'V','D'
         R_NEXT_REC = NEXT_REC + 3
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         C_DEPTH_MARKET_RPT_HEADER = C_DEPTH_MARKET_UNIT
      RETURN
C***********************************************************************
      ENTRY C_DEPTH_MARGIN_RPT_HEADER(R_NEXT_REC,R_VARIABLE_NUMBER)
C***********************************************************************
! TRANSOBJ
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                        trim(GET_SCENAME())//C_DEPTH_MARGIN_CODE
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 5
         RPT_UNIT_NUM = C_DEPTH_MARGIN_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Period              ',
     +                                               'C',INT2(9),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Transaction Group   ','C',
     +                                                  INT2(35),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Depth Interval      ',
     +                                               'N',INT2(4),'V','D'
         R_NEXT_REC = NEXT_REC + 3
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         C_DEPTH_MARGIN_RPT_HEADER = C_DEPTH_MARGIN_UNIT
      RETURN
C***********************************************************************
      ENTRY TRANS_DEL_RPT_HEADER(R_NEXT_REC)
C***********************************************************************
! TRANSOBJ
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                             trim(GET_SCENAME())//TRANS_DEL_CODE
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = 24
         DIMENSIONS = 5
         RPT_UNIT_NUM = TRANS_DEL_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Period              ',
     +                                               'C',INT2(9),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Day of Month        ',
     +                                               'N',INT2(4),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Transaction Group   ','C',
     +                                                  INT2(35),'V','D'
         R_NEXT_REC = NEXT_REC + 3
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         TRANS_DEL_RPT_HEADER = TRANS_DEL_UNIT
      RETURN
C***********************************************************************
      ENTRY TRANS_PRI_RPT_HEADER(R_NEXT_REC)
C***********************************************************************
! TRANSOBJ
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                             trim(GET_SCENAME())//TRANS_PRI_CODE
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = 24
         DIMENSIONS = 5
         RPT_UNIT_NUM = TRANS_PRI_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Period              ','C',INT2(9),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Day of Month        ','N',INT2(4),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Transaction Group   ','C',INT2(35),'V','D'
         R_NEXT_REC = NEXT_REC + 3
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         TRANS_PRI_RPT_HEADER = TRANS_PRI_UNIT
      RETURN
C***********************************************************************
      ENTRY TRANS_SCA_RPT_HEADER(R_NEXT_REC)
C***********************************************************************
! TRANSOBJ
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                             trim(GET_SCENAME())//TRANS_SCA_CODE
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = 24
         DIMENSIONS = 5
         RPT_UNIT_NUM = TRANS_SCA_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Period              ','C',INT2(9),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Day of Month        ','N',INT2(4),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Transaction Group   ','C',INT2(35),'V','D'
         R_NEXT_REC = NEXT_REC + 3
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         TRANS_SCA_RPT_HEADER = TRANS_SCA_UNIT
      RETURN
C***********************************************************************
      ENTRY TRANS_MCB_RPT_HEADER(R_NEXT_REC)
C***********************************************************************
! TRANSOBJ
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                             trim(GET_SCENAME())//TRANS_MCB_CODE
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = 24
         DIMENSIONS = 5
         RPT_UNIT_NUM = TRANS_MCB_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Period              ','C',INT2(9),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Day of Month        ','N',INT2(4),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Transaction Group   ','C',INT2(35),'V','D'
         R_NEXT_REC = NEXT_REC + 3
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         TRANS_MCB_RPT_HEADER = TRANS_MCB_UNIT
      RETURN
C***********************************************************************
      ENTRY MARKET_AREA_RPT_HEADER(R_NEXT_REC)
C***********************************************************************
! TRANSOBJ
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                           trim(GET_SCENAME())//MARKET_AREA_CODE
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = 24 ! 30
         DIMENSIONS = 5
         RPT_UNIT_NUM = MARKET_AREA_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Period              ','C',INT2(9),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Day of Month        ','N',INT2(4),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Market Area         ','C',INT2(20),'V','D'
         R_NEXT_REC = NEXT_REC + 3
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         MARKET_AREA_RPT_HEADER = MARKET_AREA_UNIT
      RETURN
C***********************************************************************
      ENTRY GAS_MARKET_AREA_RPT_HEADER(R_NEXT_REC,R_VARIABLE_NUMBER)
C***********************************************************************
! TRANSOBJ
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                       trim(GET_SCENAME())//DAILY_CUST_GAS_CODE
         OVERHEAD_LENGTH = 21
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 4
         RPT_UNIT_NUM = DAILY_CUST_GAS_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Period              ','C',INT2(9),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Day of Month        ','N',INT2(4),'V','D'
!         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
!     +                       'Customer-Market Area','C',INT2(20),'V','D'
!         R_NEXT_REC = NEXT_REC + 3
         R_NEXT_REC = NEXT_REC + 2
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         GAS_MARKET_AREA_RPT_HEADER = DAILY_CUST_GAS_UNIT
      RETURN
C***********************************************************************
      ENTRY GAS_MARKET_TRANS_RPT_HEADER(R_NEXT_REC,R_VARIABLE_NUMBER)
C***********************************************************************
! TRANSOBJ
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                       trim(GET_SCENAME())//DAILY_TRANS_GAS_CODE
         OVERHEAD_LENGTH = 21
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 4
         RPT_UNIT_NUM = DAILY_TRANS_GAS_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Period              ','C',INT2(9),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Day of Month        ','N',INT2(4),'V','D'
!         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
!     +                      'Gas Region           ','C',INT2(20),'V','D'
!         R_NEXT_REC = NEXT_REC + 3
         R_NEXT_REC = NEXT_REC + 2
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         GAS_MARKET_TRANS_RPT_HEADER = DAILY_TRANS_GAS_UNIT
      RETURN
C***********************************************************************
      ENTRY DAILY_GAS_SELL_BUY_HEADER(R_NEXT_REC,R_VARIABLE_NUMBER)
C***********************************************************************
! TRANSOBJ
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                    trim(GET_SCENAME())//DAILY_GAS_SELL_BUY_CODE
         OVERHEAD_LENGTH = 41
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 5
         RPT_UNIT_NUM = DAILY_GAS_SELL_BUY_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Period              ','C',INT2(9),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Day of Month        ','N',INT2(4),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Gas Region          ','C',INT2(20),'V','D'
         R_NEXT_REC = NEXT_REC + 3
!         R_NEXT_REC = NEXT_REC + 2
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         DAILY_GAS_SELL_BUY_HEADER = DAILY_GAS_SELL_BUY_UNIT
      RETURN
C***********************************************************************
      ENTRY DAILY_GAS_LINK_HEADER(R_NEXT_REC,R_VARIABLE_NUMBER)
C***********************************************************************
! TRANSOBJ
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                    trim(GET_SCENAME())//DAILY_GAS_LINK_CODE
         OVERHEAD_LENGTH = 21
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 4
         RPT_UNIT_NUM = DAILY_GAS_LINK_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Period              ','C',INT2(9),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Day of Month        ','N',INT2(4),'V','D'
!         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
!     +                       'Gas Link Name       ','C',INT2(31),'V','D'
!         R_NEXT_REC = NEXT_REC + 3
         R_NEXT_REC = NEXT_REC + 2
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         DAILY_GAS_LINK_HEADER = DAILY_GAS_LINK_UNIT
      RETURN
C***********************************************************************
      ENTRY DAILY_GAS_LINK_LIMIT_HEADER(R_NEXT_REC,R_VARIABLE_NUMBER)
C***********************************************************************
! TRANSOBJ
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                  trim(GET_SCENAME())//DAILY_GAS_LINK_LIMIT_CODE
         OVERHEAD_LENGTH = 21
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 4
         RPT_UNIT_NUM = DAILY_GAS_LINK_LIMIT_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Period              ','C',INT2(9),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Day of Month        ','N',INT2(4),'V','D'
!         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
!     +                       'Gas Link Name       ','C',INT2(31),'V','D'
!         R_NEXT_REC = NEXT_REC + 3
         R_NEXT_REC = NEXT_REC + 2
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         DAILY_GAS_LINK_LIMIT_HEADER = DAILY_GAS_LINK_LIMIT_UNIT
      RETURN
C***********************************************************************
      ENTRY DAILY_GAS_LINK_PRICE_HEADER(R_NEXT_REC,R_VARIABLE_NUMBER)
C***********************************************************************
! GAS_OBJT
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                  trim(GET_SCENAME())//DAILY_GAS_LINK_PRICE_CODE
         OVERHEAD_LENGTH = 21
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 4
         RPT_UNIT_NUM = DAILY_GAS_LINK_PRICE_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Period              ','C',INT2(9),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Day of Month        ','N',INT2(4),'V','D'
         R_NEXT_REC = NEXT_REC + 2
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         DAILY_GAS_LINK_PRICE_HEADER = DAILY_GAS_LINK_PRICE_UNIT
      RETURN
C***********************************************************************
      ENTRY DAILY_GAS_LINK_CF_HEADER(R_NEXT_REC,R_VARIABLE_NUMBER)
C***********************************************************************
! GAS_OBJT
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                  trim(GET_SCENAME())//DAILY_GAS_LINK_CF_CODE
         OVERHEAD_LENGTH = 21
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 4
         RPT_UNIT_NUM = DAILY_GAS_LINK_CF_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Period              ','C',INT2(9),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Day of Month        ','N',INT2(4),'V','D'
         R_NEXT_REC = NEXT_REC + 2
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         DAILY_GAS_LINK_CF_HEADER = DAILY_GAS_LINK_CF_UNIT
      RETURN
C***********************************************************************
      ENTRY MONTHLY_GAS_SELL_BUY_HEADER(R_NEXT_REC,R_VARIABLE_NUMBER)
C***********************************************************************
! TRANSOBJ
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                  trim(GET_SCENAME())//MONTHLY_GAS_SELL_BUY_CODE
         OVERHEAD_LENGTH = 37
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 4
         RPT_UNIT_NUM = MONTHLY_GAS_SELL_BUY_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Period              ','C',INT2(9),'V','D'
!         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
!     +                       'Day of Month        ','N',INT2(4),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Gas Region          ','C',INT2(20),'V','D'
         R_NEXT_REC = NEXT_REC + 2
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         MONTHLY_GAS_SELL_BUY_HEADER = MONTHLY_GAS_SELL_BUY_UNIT
      RETURN

C***********************************************************************
      ENTRY DAILY_GAS_PRICE_HEADER(R_NEXT_REC,R_VARIABLE_NUMBER)
C***********************************************************************
! TRANSOBJ
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                    trim(GET_SCENAME())//DAILY_GAS_PRICE_CODE
         OVERHEAD_LENGTH = 21
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 4
         RPT_UNIT_NUM = DAILY_GAS_PRICE_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Period              ','C',INT2(9),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Day of Month        ','N',INT2(4),'V','D'

         R_NEXT_REC = NEXT_REC + 2
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         DAILY_GAS_PRICE_HEADER = DAILY_GAS_PRICE_UNIT
      RETURN
C***********************************************************************
      ENTRY DAILY_GAS_NODE_VOLUME_HEADER(R_NEXT_REC,R_VARIABLE_NUMBER)
C***********************************************************************
! TRANSOBJ
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                 trim(GET_SCENAME())//DAILY_GAS_NODE_VOLUME_CODE
         OVERHEAD_LENGTH = 21
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 4
         RPT_UNIT_NUM = DAILY_GAS_NODE_VOLUME_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Period              ','C',INT2(9),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Day of Month        ','N',INT2(4),'V','D'
!         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
!     +                       'Gas Region          ','C',INT2(20),'V','D'
!         R_NEXT_REC = NEXT_REC + 3
         R_NEXT_REC = NEXT_REC + 2
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         DAILY_GAS_NODE_VOLUME_HEADER = DAILY_GAS_NODE_VOLUME_UNIT
      RETURN
C***********************************************************************
      ENTRY DAILY_GAS_STORAGE_HEADER(R_NEXT_REC,R_VARIABLE_NUMBER)
C***********************************************************************
! TRANSOBJ
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                     trim(GET_SCENAME())//DAILY_GAS_STORAGE_CODE
         OVERHEAD_LENGTH = 21
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 4
         RPT_UNIT_NUM = DAILY_GAS_STORAGE_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Period              ','C',INT2(9),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Day of Month        ','N',INT2(4),'V','D'
!         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
!     +                       'Gas Region          ','C',INT2(20),'V','D'
!         R_NEXT_REC = NEXT_REC + 3
         R_NEXT_REC = NEXT_REC + 2
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         DAILY_GAS_STORAGE_HEADER = DAILY_GAS_STORAGE_UNIT
      RETURN
C***********************************************************************
      ENTRY DAILY_GAS_SUR_DEF_HEADER(R_NEXT_REC,R_VARIABLE_NUMBER)
C***********************************************************************
! TRANSOBJ
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +             trim(GET_SCENAME())//DAILY_SURPLUS_DEFICIT_CODE
         OVERHEAD_LENGTH = 21
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 4
         RPT_UNIT_NUM = DAILY_SURPLUS_DEFICIT_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Period              ','C',INT2(9),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Day of Month        ','N',INT2(4),'V','D'
         R_NEXT_REC = NEXT_REC + 2
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         DAILY_GAS_SUR_DEF_HEADER = DAILY_SURPLUS_DEFICIT_UNIT
      RETURN
C***********************************************************************
      ENTRY DAILY_GAS_AVE_PRICE_HEADER(R_NEXT_REC,R_VARIABLE_NUMBER)
C***********************************************************************
! TRANSOBJ
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                   trim(GET_SCENAME())//DAILY_GAS_AVE_PRICE_CODE
         OVERHEAD_LENGTH = 21
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 4
         RPT_UNIT_NUM = DAILY_GAS_AVE_PRICE_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Period              ','C',INT2(9),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Day of Month        ','N',INT2(4),'V','D'
!         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
!     +                       'Gas Region          ','C',INT2(20),'V','D'
!         R_NEXT_REC = NEXT_REC + 3
         R_NEXT_REC = NEXT_REC + 2
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         DAILY_GAS_AVE_PRICE_HEADER = DAILY_GAS_AVE_PRICE_UNIT
      RETURN
C***********************************************************************
      ENTRY DAILY_GAS_DIFFER_HEADER(R_NEXT_REC,R_VARIABLE_NUMBER)
C***********************************************************************
! TRANSOBJ
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                    trim(GET_SCENAME())//DAILY_GAS_DIFFER_CODE
         OVERHEAD_LENGTH = 21
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 4
         RPT_UNIT_NUM = DAILY_GAS_DIFFER_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Period              ','C',INT2(9),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Day of Month        ','N',INT2(4),'V','D'
!         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
!     +                       'Gas Region          ','C',INT2(20),'V','D'
!         R_NEXT_REC = NEXT_REC + 3
         R_NEXT_REC = NEXT_REC + 2
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         DAILY_GAS_DIFFER_HEADER = DAILY_GAS_DIFFER_UNIT
      RETURN
C***********************************************************************
      ENTRY MONTHLY_GAS_PRICE_HEADER(R_NEXT_REC,R_VARIABLE_NUMBER)
C***********************************************************************
! TRANSOBJ
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                    trim(GET_SCENAME())//MONTHLY_GAS_PRICE_CODE
         OVERHEAD_LENGTH = 17
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 3
         IF(CAPACITY_PLANNING_METHOD() == 'MX'  .AND.
     +                        GREEN_MRX_METHOD() == 'GX' .AND.
     +                             .NOT. HORIZONS_ACTIVE) THEN
            DIMENSIONS = 4
            OVERHEAD_LENGTH = 21
         ENDIF
         RPT_UNIT_NUM = MONTHLY_GAS_PRICE_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         IF(CAPACITY_PLANNING_METHOD() == 'MX'  .AND.
     +                           GREEN_MRX_METHOD() == 'GX' .AND.
     +                                       .NOT. HORIZONS_ACTIVE) THEN
            WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                        'Iteration           ','N',INT2(4),'F','D'
            NEXT_REC = NEXT_REC + 1
         ENDIF
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Period              ','C',INT2(9),'V','D'
!         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
!     +                       'Day of Month        ','N',INT2(4),'V','D'
!         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
!     +                       'Gas Region          ','C',INT2(20),'V','D'
!         R_NEXT_REC = NEXT_REC + 3
!         R_NEXT_REC = NEXT_REC + 2
         R_NEXT_REC = NEXT_REC + 1
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         MONTHLY_GAS_PRICE_HEADER = MONTHLY_GAS_PRICE_UNIT
      RETURN
C***********************************************************************
      ENTRY MONTHLY_GAS_DIFFER_HEADER(R_NEXT_REC,R_VARIABLE_NUMBER)
C***********************************************************************
! TRANSOBJ
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                    trim(GET_SCENAME())//MONTHLY_GAS_DIFFER_CODE
         OVERHEAD_LENGTH = 17
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 3
         RPT_UNIT_NUM = MONTHLY_GAS_DIFFER_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Period              ','C',INT2(9),'V','D'
!         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
!     +                       'Day of Month        ','N',INT2(4),'V','D'
!         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
!     +                       'Gas Region          ','C',INT2(20),'V','D'
!         R_NEXT_REC = NEXT_REC + 3
!         R_NEXT_REC = NEXT_REC + 2
         R_NEXT_REC = NEXT_REC + 1
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         MONTHLY_GAS_DIFFER_HEADER = MONTHLY_GAS_DIFFER_UNIT
      RETURN

C***********************************************************************
      ENTRY TRANS_HMP_RPT_HEADER(R_NEXT_REC)
C***********************************************************************
! TRANSOBJ
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                             trim(GET_SCENAME())//TRANS_HMP_CODE
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = 24 ! 30
         DIMENSIONS = 5
         RPT_UNIT_NUM = TRANS_HMP_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Period              ','C',INT2(9),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Day of Month        ','N',INT2(4),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Transaction Group   ','C',INT2(35),'V','D'
         R_NEXT_REC = NEXT_REC + 3
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         TRANS_HMP_RPT_HEADER = TRANS_HMP_UNIT
      RETURN
C***********************************************************************
      ENTRY TRANS_LDS_RPT_HEADER(R_NEXT_REC)
C***********************************************************************
! TRANSOBJ
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                             trim(GET_SCENAME())//TRANS_LDS_CODE
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = 24
         DIMENSIONS = 5
         RPT_UNIT_NUM = TRANS_LDS_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Period              ','C',INT2(9),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Day of Month        ','N',INT2(4),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Transaction Group   ','C',INT2(35),'V','D'
         R_NEXT_REC = NEXT_REC + 3
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         TRANS_LDS_RPT_HEADER = TRANS_LDS_UNIT
      RETURN
C***********************************************************************
      ENTRY TRANS_LAS_RPT_HEADER(R_NEXT_REC)
C***********************************************************************
! TRANSOBJ
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                             trim(GET_SCENAME())//TRANS_LAS_CODE
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = 24
         DIMENSIONS = 5
         RPT_UNIT_NUM = TRANS_LAS_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Period              ','C',INT2(9),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Day of Month        ','N',INT2(4),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Transaction Group   ','C',INT2(35),'V','D'
         R_NEXT_REC = NEXT_REC + 3
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         TRANS_LAS_RPT_HEADER = TRANS_LAS_UNIT
      RETURN
C***********************************************************************
      ENTRY TRANS_MRK_RPT_HEADER(R_NEXT_REC)
C***********************************************************************
! TRANSOBJ
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                             trim(GET_SCENAME())//TRANS_MRK_CODE
         OVERHEAD_LENGTH = 21
         VARIABLE_NUMBER = 24
         DIMENSIONS = 4
         RPT_UNIT_NUM = TRANS_MRK_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Period              ','C',INT2(9),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Day of Month        ','N',INT2(4),'V','D'
         R_NEXT_REC = NEXT_REC + 2
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         TRANS_MRK_RPT_HEADER = TRANS_MRK_UNIT
      RETURN
C***********************************************************************
      ENTRY TRANS_PB4_RPT_HEADER(R_NEXT_REC)
C***********************************************************************
! TRANSOBJ
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                             trim(GET_SCENAME())//TRANS_PB4_CODE
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = 24
         DIMENSIONS = 5
         RPT_UNIT_NUM = TRANS_PB4_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Period              ','C',INT2(9),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Day of Month        ','N',INT2(4),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Transaction Group   ','C',INT2(35),'V','D'
         R_NEXT_REC = NEXT_REC + 3
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         TRANS_PB4_RPT_HEADER = TRANS_PB4_UNIT
      RETURN
C***********************************************************************
      ENTRY HOUR_DIAG_RPT_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)
C***********************************************************************
! TRANSOBJ
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                             trim(GET_SCENAME())//HOUR_DIAG_CODE
         OVERHEAD_LENGTH = 25
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 5
         RPT_UNIT_NUM = HOUR_DIAG_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Period              ','C',INT2(9),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Hour of Month       ','N',INT2(4),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Transaction Number  ','N',INT2(4),'V','D'
         R_NEXT_REC = NEXT_REC + 3
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         HOUR_DIAG_RPT_HEADER = HOUR_DIAG_UNIT
      RETURN
C***********************************************************************
      ENTRY HOUR_JDA_RPT_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)
C***********************************************************************
! TRANSOBJ
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                             trim(GET_SCENAME())//HOUR_JDA_CODE
         OVERHEAD_LENGTH = 25
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 5
         RPT_UNIT_NUM = HOUR_JDA_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Period              ','C',INT2(9),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Day of Month        ','N',INT2(4),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Hour of Month       ','N',INT2(4),'V','D'
         R_NEXT_REC = NEXT_REC + 3
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         HOUR_JDA_RPT_HEADER = HOUR_JDA_UNIT
      RETURN
C***********************************************************************
      ENTRY DEPTH_MARKET_RPT_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)
C***********************************************************************
! TRANSOBJ
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                          trim(GET_SCENAME())//DEPTH_MARKET_CODE
         OVERHEAD_LENGTH = 25
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 5
         RPT_UNIT_NUM = DEPTH_MARKET_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Period              ','C',INT2(9),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Day of Month        ','N',INT2(4),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Market Interval     ','N',INT2(4),'V','D'
         R_NEXT_REC = NEXT_REC + 3
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         DEPTH_MARKET_RPT_HEADER = DEPTH_MARKET_UNIT
      RETURN
C***********************************************************************
      ENTRY TRANS_DEPTH_MARKET_RPT_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)
C***********************************************************************
! TRANSOBJ
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                    trim(GET_SCENAME())//TRANS_DEPTH_MARKET_CODE
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 6
         RPT_UNIT_NUM = TRANS_DEPTH_MARKET_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Period              ','C',INT2(9),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Day of Month        ','N',INT2(4),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Transaction Group   ','C',INT2(35),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+3)
     +                       'Market Interval     ','C',INT2(6),'V','D'
         R_NEXT_REC = NEXT_REC + 4
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         TRANS_DEPTH_MARKET_RPT_HEADER = TRANS_DEPTH_MARKET_UNIT
      RETURN
C***********************************************************************
      ENTRY TRANS_PAF_RPT_HEADER(R_NEXT_REC)
C***********************************************************************
! TRANSOBJ
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                             trim(GET_SCENAME())//TRANS_PAF_CODE
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = 24
         DIMENSIONS = 5
         RPT_UNIT_NUM = TRANS_PAF_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Period              ','C',INT2(9),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Day of Month        ','N',INT2(4),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Transaction Group   ','C',INT2(35),'V','D'
         R_NEXT_REC = NEXT_REC + 3
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         TRANS_PAF_RPT_HEADER = TRANS_PAF_UNIT
      RETURN
C***********************************************************************
      ENTRY TRANS_CAP_RPT_HEADER(R_NEXT_REC)
C***********************************************************************
! TRANSOBJ
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                             trim(GET_SCENAME())//TRANS_CAP_CODE
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = 24
         DIMENSIONS = 5
         RPT_UNIT_NUM = TRANS_CAP_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Period              ','C',INT2(9),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Day of Month        ','N',INT2(4),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Transaction Group   ','C',INT2(35),'V','D'
         R_NEXT_REC = NEXT_REC + 3
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         TRANS_CAP_RPT_HEADER = TRANS_CAP_UNIT
      RETURN
C***********************************************************************
      ENTRY TRANS_DER_RPT_HEADER(R_NEXT_REC)
C***********************************************************************
! TRANSOBJ
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                             trim(GET_SCENAME())//TRANS_DER_CODE
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = 24
         DIMENSIONS = 5
         RPT_UNIT_NUM = TRANS_DER_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Period              ','C',INT2(9),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Day of Month        ','N',INT2(4),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Transaction Group   ','C',INT2(35),'V','D'
         R_NEXT_REC = NEXT_REC + 3
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         TRANS_DER_RPT_HEADER = TRANS_DER_UNIT
      RETURN
C***********************************************************************
      ENTRY TRANS_EUE_RPT_HEADER(R_NEXT_REC)
C***********************************************************************
! TRANSOBJ
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                             trim(GET_SCENAME())//TRANS_EUE_CODE
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = 24
         DIMENSIONS = 5
         RPT_UNIT_NUM = TRANS_EUE_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Period              ','C',INT2(9),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Day of Month        ','N',INT2(4),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Transaction Group   ','C',INT2(35),'V','D'
         R_NEXT_REC = NEXT_REC + 3
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         TRANS_EUE_RPT_HEADER = TRANS_EUE_UNIT
      RETURN
C***********************************************************************
      ENTRY TRANS_TRM_RPT_HEADER(R_NEXT_REC)
C***********************************************************************
! TRANSOBJ
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                             trim(GET_SCENAME())//TRANS_TRM_CODE
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = 24
         DIMENSIONS = 5
         RPT_UNIT_NUM = TRANS_TRM_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Period              ','C',INT2(9),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Day of Month        ','N',INT2(4),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Transaction Group   ','C',INT2(35),'V','D'
         R_NEXT_REC = NEXT_REC + 3
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         TRANS_TRM_RPT_HEADER = TRANS_TRM_UNIT
      RETURN
C***********************************************************************
      ENTRY TRANS_EXC_RPT_HEADER(R_NEXT_REC)
C***********************************************************************
! TRANSOBJ
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                             trim(GET_SCENAME())//TRANS_EXC_CODE
         OVERHEAD_LENGTH = 64
!
         VARIABLE_NUMBER = 1 + GET_NUMBER_OF_ACTIVE_GROUPS() ! SET TO THE MAXIMUM TRANSACTION GROUPS
!
         DIMENSIONS = 4
         RPT_UNIT_NUM = TRANS_EXC_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Period              ','C',INT2(9),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Seller              ','C',INT2(35),'V','D'
         R_NEXT_REC = NEXT_REC + 2
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         TRANS_EXC_RPT_HEADER = TRANS_EXC_UNIT
      RETURN
C***********************************************************************
      ENTRY TRANS_INC_RPT_HEADER(R_NEXT_REC)
C***********************************************************************
! TRANSOBJ
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                             trim(GET_SCENAME())//TRANS_INC_CODE
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = 24
         DIMENSIONS = 5
         RPT_UNIT_NUM = TRANS_INC_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Period              ','C',INT2(9),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Day of Month        ','N',INT2(4),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Transaction Group   ','C',INT2(35),'V','D'
         R_NEXT_REC = NEXT_REC + 3
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         TRANS_INC_RPT_HEADER = TRANS_INC_UNIT
      RETURN
C***********************************************************************
      ENTRY TRANS_LMD_RPT_HEADER(R_NEXT_REC)
C***********************************************************************
! TRANSOBJ
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                             trim(GET_SCENAME())//TRANS_LMD_CODE
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = 24
         DIMENSIONS = 5
         RPT_UNIT_NUM = TRANS_LMD_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Period              ','C',INT2(9),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Day of Month        ','N',INT2(4),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Transaction Group   ','C',INT2(35),'V','D'
         R_NEXT_REC = NEXT_REC + 3
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         TRANS_LMD_RPT_HEADER = TRANS_LMD_UNIT
      RETURN
C***********************************************************************
      ENTRY TRANS_ANN_RPT_HEADER(R_NEXT_REC)
C***********************************************************************
! TRANSOBJ
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                             trim(GET_SCENAME())//TRANS_ANN_CODE
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = 27
         DIMENSIONS = 4
         RPT_UNIT_NUM = TRANS_ANN_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Period              ','C',INT2(9),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Transaction Group   ','C',INT2(35),'V','D'
         R_NEXT_REC = NEXT_REC + 2
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         TRANS_ANN_RPT_HEADER = TRANS_ANN_UNIT
      RETURN
C***********************************************************************
      ENTRY MON_TRANC_RPT_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)
C***********************************************************************
! TRANSOBJ
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                             trim(GET_SCENAME())//MON_TRANC_CODE
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 4
         RPT_UNIT_NUM = MON_TRANC_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Period              ','C',INT2(9),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Transaction Group   ','C',INT2(35),'V','D'
         R_NEXT_REC = NEXT_REC + 2
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         MON_TRANC_RPT_HEADER = MON_TRANC_UNIT
      RETURN
C***********************************************************************
      ENTRY MON_POSIT_RPT_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)
C***********************************************************************
! TRANSOBJ
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                             trim(GET_SCENAME())//MON_POSIT_CODE
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 5
         RPT_UNIT_NUM = MON_POSIT_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Period              ','C',INT2(9),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Transaction Group   ','C',INT2(35),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Product             ','C',INT2(10),'V','D'
         R_NEXT_REC = NEXT_REC + 3
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         MON_POSIT_RPT_HEADER = MON_POSIT_UNIT
      RETURN
C***********************************************************************
      ENTRY TRANS_TRN_RPT_HEADER(R_NEXT_REC)
C***********************************************************************
! TRANSOBJ
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                             trim(GET_SCENAME())//TRANS_TRN_CODE
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = 1 + GET_NUMBER_OF_ACTIVE_GROUPS() ! SET TO THE MAXIMUM TRANSACTION GROUPS
         DIMENSIONS = 6
         RPT_UNIT_NUM = TRANS_TRN_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Period              ','C',INT2(9),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Day of the Month    ','N',INT2(4),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Transaction Group   ','C',INT2(35),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+3)
     +                       'Hour of the Day     ','N',INT2(4),'V','D'
         R_NEXT_REC = NEXT_REC + 4
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         TRANS_TRN_RPT_HEADER = TRANS_TRN_UNIT
      RETURN
C***********************************************************************
      ENTRY USER_DURATION_HEADER(R_NEXT_REC)
C***********************************************************************
! TRANSOBJ
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                       trim(GET_SCENAME())//USER_DURATION_CODE
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = 4
         DIMENSIONS = 5
         RPT_UNIT_NUM = USER_DURATION_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Month               ','C',INT2(9),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Transaction Group   ','C',INT2(35),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Position            ','N',INT2(4),'V','D'
         R_NEXT_REC = NEXT_REC + 3
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         USER_DURATION_HEADER = USER_DURATION_NO
      RETURN
C***********************************************************************
      ENTRY TECH_SCAR_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)
C***********************************************************************
! TRANSOBJ
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                             trim(GET_SCENAME())//TECH_SCAR_CODE
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 5
         RPT_UNIT_NUM = TECH_SCAR_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Month               ','C',INT2(9),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Transaction Group   ','C',INT2(35),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Position            ','N',INT2(4),'V','D'
         R_NEXT_REC = NEXT_REC + 3
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         TECH_SCAR_HEADER = TECH_SCAR_NO
      RETURN
C***********************************************************************
      ENTRY ECITY_OBJ_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)
C***********************************************************************
! TRANSOBJ
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                       trim(GET_SCENAME())//ECITY_OBJ_CODE
         OVERHEAD_LENGTH = 21
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 4
         RPT_UNIT_NUM = ECITY_OBJ_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Asset Class Number  ','N',INT2(4),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Month               ','C',INT2(9),'F','D'
         R_NEXT_REC = NEXT_REC + 2
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         ECITY_OBJ_HEADER = ECITY_OBJ_NO
      RETURN
C***********************************************************************
      ENTRY MARKET_DURATION_HEADER(R_NEXT_REC)
C***********************************************************************
! TRANSOBJ
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                       trim(GET_SCENAME())//MARKET_DURATION_CODE
         OVERHEAD_LENGTH = 21
         VARIABLE_NUMBER = 4
         DIMENSIONS = 4
         RPT_UNIT_NUM = MARKET_DURATION_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Month               ','C',INT2(9),'F','D'
!         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+)
!     +                       'Transaction Group   ','C',INT2(35),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Position            ','N',INT2(4),'V','D'
         R_NEXT_REC = NEXT_REC + 2
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         MARKET_DURATION_HEADER = MARKET_DURATION_NO
      RETURN
C***********************************************************************
      ENTRY DAY_WEEK_RPT_HEADER(R_NEXT_REC)
C***********************************************************************
! TRANSOBJ
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                              trim(GET_SCENAME())//DAY_WEEK_CODE
         OVERHEAD_LENGTH = 17
         VARIABLE_NUMBER = 31
         DIMENSIONS = 3
         RPT_UNIT_NUM = DAY_WEEK_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) 'Period              ','C',
     +                                                  INT2(9),'F','D'
         R_NEXT_REC = NEXT_REC + 1
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         DAY_WEEK_RPT_HEADER = DAY_WEEK_UNIT
      RETURN
C***********************************************************************
      ENTRY NUC_FUEL_REGION_RPT(R_NEXT_REC)
C***********************************************************************
! FINNUCFL MSGMMNFL
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                    trim(GET_SCENAME())//NUC_FUEL_REPORTING_CODE
         OVERHEAD_LENGTH = 45
         VARIABLE_NUMBER = 15
         DIMENSIONS = 4
         RPT_UNIT_NUM = NUC_REPORTING_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Account Name        ','C',INT2(34),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Batch/Load          ','C',INT2(7),'V','D'
         R_NEXT_REC = NEXT_REC + 2
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         NUC_FUEL_REGION_RPT = NUC_REPORTING_UNIT
      RETURN
C***********************************************************************
      ENTRY TRANS_LBH_RPT_HEADER(R_NEXT_REC)
C***********************************************************************
! TF_OBJT
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                             trim(GET_SCENAME())//TRANS_LBH_CODE
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = 24 ! 30
         DIMENSIONS = 5
         RPT_UNIT_NUM = TRANS_LBH_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Period              ','C',INT2(9),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Day of Month        ','N',INT2(4),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Transaction Group   ','C',INT2(35),'V','D'
         R_NEXT_REC = NEXT_REC + 3
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         TRANS_LBH_RPT_HEADER = TRANS_LBH_UNIT
      RETURN
C***********************************************************************
      ENTRY TRANS_INTR_RPT_HEADER(R_NEXT_REC)
C***********************************************************************
! TF_OBJT
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                            trim(GET_SCENAME())//TRANS_INTR_CODE
         OVERHEAD_LENGTH = 51
         VARIABLE_NUMBER = 24
         DIMENSIONS = 5
         RPT_UNIT_NUM = TRANS_INTR_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Period              ','C',INT2(9),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Day of Month        ','N',INT2(4),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Customer Group      ','C',INT2(30),'V','D'
         R_NEXT_REC = NEXT_REC + 3
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         TRANS_INTR_RPT_HEADER = TRANS_INTR_UNIT
      RETURN
C***********************************************************************
      ENTRY WH_HOURLY_HEADER(R_NEXT_REC)
C***********************************************************************
! TF_OBJT
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                              trim(GET_SCENAME())//TRANS_WH_CODE
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = 24
         DIMENSIONS = 5
         RPT_UNIT_NUM = TRANS_WH_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Period              ','C',INT2(9),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Day of Month        ','N',INT2(4),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Transaction Group   ','C',INT2(35),'V','D'
         R_NEXT_REC = NEXT_REC + 3
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         WH_HOURLY_HEADER = TRANS_WH_UNIT
      RETURN
C***********************************************************************
      ENTRY HYDRO_AG_HEADER(R_NEXT_REC)
C***********************************************************************
! TF_OBJT
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                       trim(GET_SCENAME())//HYDRO_AG_HOURLY_CODE
         OVERHEAD_LENGTH = 64
         VARIABLE_NUMBER = 24
         DIMENSIONS = 5
         RPT_UNIT_NUM = HYDRO_AG_NO
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Period              ','C',INT2(9),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Day of Month        ','N',INT2(4),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Transaction Group   ','C',INT2(35),'V','D'
         R_NEXT_REC = NEXT_REC + 3
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         HYDRO_AG_HEADER = HYDRO_AG_NO
      RETURN
C***********************************************************************
      ENTRY TRANS_CLASS_SUM_HEADER(R_NEXT_REC)
C***********************************************************************
! TF_OBJT
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                      trim(GET_SCENAME())//MON_CUSTOMER_SUM_CODE
         OVERHEAD_LENGTH = 47
         VARIABLE_NUMBER = 30 ! 100907 ! 3/19/02
         DIMENSIONS = 4
         RPT_UNIT_NUM = TRANS_CLASS_SUM_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Month               ','C',INT2(9),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Customer Name       ','C',INT2(30),'V','D'
         R_NEXT_REC = NEXT_REC + 2
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         TRANS_CLASS_SUM_HEADER = TRANS_CLASS_SUM_UNIT
      RETURN
C***********************************************************************
      ENTRY MONTHLY_GAS_DEMAND_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)
C***********************************************************************
! GAS_OBJT
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                    trim(GET_SCENAME())//MONTHLY_GAS_DEMAND_CODE
         OVERHEAD_LENGTH = 47
         VARIABLE_NUMBER = R_VARIABLE_NUMBER ! 12 ! 3/19/02
         DIMENSIONS = 4
         RPT_UNIT_NUM = MONTHLY_GAS_DEMAND_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Month               ','C',INT2(9),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Customer Name       ','C',INT2(30),'V','D'
         R_NEXT_REC = NEXT_REC + 2
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         MONTHLY_GAS_DEMAND_HEADER = MONTHLY_GAS_DEMAND_UNIT
      RETURN
C***********************************************************************
      ENTRY MONTHLY_GAS_BALANCE_HEADER(R_NEXT_REC,R_VARIABLE_NUMBER)
C***********************************************************************
! GAS_OBJT
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                   trim(GET_SCENAME())//MONTHLY_GAS_BALANCE_CODE
         OVERHEAD_LENGTH = 23
         VARIABLE_NUMBER = R_VARIABLE_NUMBER ! 12 ! 3/19/02
         DIMENSIONS = 4
         RPT_UNIT_NUM = MONTHLY_GAS_BALANCE_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Month               ','C',INT2(9),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Country             ','C',INT2(6),'V','D'
         R_NEXT_REC = NEXT_REC + 2
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         MONTHLY_GAS_BALANCE_HEADER = MONTHLY_GAS_BALANCE_UNIT
      RETURN
C***********************************************************************
      ENTRY MONTHLY_GAS_SUPPLY_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)
C***********************************************************************
! GAS_OBJT
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                    trim(GET_SCENAME())//MONTHLY_GAS_SUPPLY_CODE
         OVERHEAD_LENGTH = 47
         VARIABLE_NUMBER = R_VARIABLE_NUMBER ! 12 ! 3/19/02
         DIMENSIONS = 4
         RPT_UNIT_NUM = MONTHLY_GAS_SUPPLY_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Month               ','C',INT2(9),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Supply Cost Curve   ','C',INT2(30),'V','D'
         R_NEXT_REC = NEXT_REC + 2
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         MONTHLY_GAS_SUPPLY_HEADER = MONTHLY_GAS_SUPPLY_UNIT
      RETURN
C***********************************************************************
      ENTRY MONTHLY_GAS_BASIN_HEADER(R_NEXT_REC,R_VARIABLE_NUMBER)
C***********************************************************************
! GAS_OBJT
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                    trim(GET_SCENAME())//MONTHLY_GAS_BASIN_CODE
         OVERHEAD_LENGTH = 17
         VARIABLE_NUMBER = R_VARIABLE_NUMBER ! 12 ! 3/19/02
         DIMENSIONS = 3
         RPT_UNIT_NUM = MONTHLY_GAS_BASIN_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Month               ','C',INT2(9),'F','D'
!         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
!     +                       'BASIN Cost Curve   ','C',INT2(30),'V','D'
         R_NEXT_REC = NEXT_REC + 1
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         MONTHLY_GAS_BASIN_HEADER = MONTHLY_GAS_BASIN_UNIT
      RETURN
C***********************************************************************
      ENTRY MONTHLY_GAS_LINK_HEADER(R_NEXT_REC,R_VARIABLE_NUMBER)
C***********************************************************************
! GAS_OBJT
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                      trim(GET_SCENAME())//MONTHLY_GAS_LINK_CODE
         OVERHEAD_LENGTH = 17
         VARIABLE_NUMBER = R_VARIABLE_NUMBER ! 12 ! 3/19/02
         DIMENSIONS = 3
         RPT_UNIT_NUM = MONTHLY_GAS_LINK_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Month               ','C',INT2(9),'F','D'
!         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
!     +                       'LINK Cost Curve   ','C',INT2(30),'V','D'
         R_NEXT_REC = NEXT_REC + 1
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         MONTHLY_GAS_LINK_HEADER = MONTHLY_GAS_LINK_UNIT
      RETURN
C***********************************************************************
      ENTRY MONTHLY_GAS_LINK_PRICE_HEADER(R_NEXT_REC,R_VARIABLE_NUMBER)
C***********************************************************************
! GAS_OBJT
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                trim(GET_SCENAME())//MONTHLY_GAS_LINK_PRICE_CODE
         OVERHEAD_LENGTH = 17
         VARIABLE_NUMBER = R_VARIABLE_NUMBER ! 12 ! 3/19/02
         DIMENSIONS = 3
         RPT_UNIT_NUM = MONTHLY_GAS_LINK_PRICE_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Month               ','C',INT2(9),'F','D'
!         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
!     +                       'LINK Cost Curve   ','C',INT2(30),'V','D'
         R_NEXT_REC = NEXT_REC + 1
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         MONTHLY_GAS_LINK_PRICE_HEADER = MONTHLY_GAS_LINK_PRICE_UNIT
      RETURN
C***********************************************************************
      ENTRY MONTHLY_GAS_RESERVE_HEADER(R_NEXT_REC,R_VARIABLE_NUMBER)
C***********************************************************************
! GAS_OBJT
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                   trim(GET_SCENAME())//MONTHLY_GAS_RESERVE_CODE
         OVERHEAD_LENGTH = 17
         VARIABLE_NUMBER = R_VARIABLE_NUMBER ! 12 ! 3/19/02
         DIMENSIONS = 3
         RPT_UNIT_NUM = MONTHLY_GAS_RESERVE_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Month               ','C',INT2(9),'F','D'
!         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
!     +                       'RESERVE Cost Curve   ','C',INT2(30),'V','D'
         R_NEXT_REC = NEXT_REC + 1
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         MONTHLY_GAS_RESERVE_HEADER = MONTHLY_GAS_RESERVE_UNIT
      RETURN
C***********************************************************************
      ENTRY CX_DAILY_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)
C***********************************************************************
! TF_OBJT
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                              trim(GET_SCENAME())//CX_DAILY_CODE
         OVERHEAD_LENGTH = 41
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         RPT_UNIT_NUM = CX_DAILY_NO
         DIMENSIONS = 5
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Month               ','C',INT2(9),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Day                 ','N',INT2(4),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Resource            ','C',INT2(20),'F','D'
         R_NEXT_REC = NEXT_REC + 3
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         CX_DAILY_HEADER = CX_DAILY_NO
      RETURN
C***********************************************************************
      ENTRY UU_DAILY_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)
C***********************************************************************
! TF_OBJT
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                    trim(GET_SCENAME())//DAILY_THERMAL_UNIT_CODE
         OVERHEAD_LENGTH = 46
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         RPT_UNIT_NUM = DAILY_THERMAL_UNIT_NO
         DIMENSIONS = 5
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Month               ','C',INT2(9),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Day                 ','N',INT2(4),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Resource            ','C',INT2(25),'F','D'
         R_NEXT_REC = NEXT_REC + 3
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         UU_DAILY_HEADER = DAILY_THERMAL_UNIT_NO
      RETURN
C***********************************************************************
      ENTRY UK_DAILY_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)
C***********************************************************************
! TF_OBJT
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                           trim(GET_SCENAME())//DAILY_TG_PM_CODE
         OVERHEAD_LENGTH = 56
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         RPT_UNIT_NUM = DAILY_TG_PM_NO
         DIMENSIONS = 5
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Month               ','C',INT2(9),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Day                 ','N',INT2(4),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Transaction Group   ','C',INT2(35),'F','D'
         R_NEXT_REC = NEXT_REC + 3
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         UK_DAILY_HEADER = DAILY_TG_PM_NO
      RETURN
C***********************************************************************
      ENTRY UV_HOURLY_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)
C***********************************************************************
! TF_OBJT
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                             trim(GET_SCENAME())//UV_HOURLY_CODE
         OVERHEAD_LENGTH = 46
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         RPT_UNIT_NUM = UV_HOURLY_NO
         DIMENSIONS = 5
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Month               ','C',INT2(9),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Day                 ','N',INT2(4),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Resource            ','C',INT2(25),'F','D'
         R_NEXT_REC = NEXT_REC + 3
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         UV_HOURLY_HEADER = UV_HOURLY_NO
      RETURN
C***********************************************************************
      ENTRY HX_HOURLY_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)
C***********************************************************************
! TF_OBJT
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                             trim(GET_SCENAME())//HX_HOURLY_CODE
         OVERHEAD_LENGTH = 41
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         RPT_UNIT_NUM = HX_HOURLY_NO
         DIMENSIONS = 5
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Month               ','C',INT2(9),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Day                 ','N',INT2(4),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Resource            ','C',INT2(20),'F','D'
         R_NEXT_REC = NEXT_REC + 3
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         HX_HOURLY_HEADER = HX_HOURLY_NO
      RETURN
C***********************************************************************
      ENTRY AJ_HOURLY_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)
C***********************************************************************
! TF_OBJT
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                             trim(GET_SCENAME())//AJ_HOURLY_CODE
         OVERHEAD_LENGTH = 41
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         RPT_UNIT_NUM = AJ_HOURLY_NO
         DIMENSIONS = 5
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Month               ','C',INT2(9),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Day                 ','N',INT2(4),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Resource            ','C',INT2(20),'F','D'
         R_NEXT_REC = NEXT_REC + 3
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         AJ_HOURLY_HEADER = AJ_HOURLY_NO
      RETURN
C***********************************************************************
      ENTRY AK_HOURLY_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)
C***********************************************************************
! TF_OBJT
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                             trim(GET_SCENAME())//AK_HOURLY_CODE
         OVERHEAD_LENGTH = 41
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         RPT_UNIT_NUM = AK_HOURLY_NO
         DIMENSIONS = 5
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Month               ','C',INT2(9),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Resource            ','C',INT2(20),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Hour                ','N',INT2(4),'F','D'
         R_NEXT_REC = NEXT_REC + 3
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         AK_HOURLY_HEADER = AK_HOURLY_NO
      RETURN
C***********************************************************************
      ENTRY XX_HOURLY_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)
C***********************************************************************
! TF_OBJT
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                             trim(GET_SCENAME())//XX_HOURLY_CODE
         OVERHEAD_LENGTH = 41
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         RPT_UNIT_NUM = XX_HOURLY_NO
         DIMENSIONS = 5
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Month               ','C',INT2(9),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Day                 ','N',INT2(4),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Resource            ','C',INT2(20),'F','D'
         R_NEXT_REC = NEXT_REC + 3
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         XX_HOURLY_HEADER = XX_HOURLY_NO
      RETURN
C***********************************************************************
      ENTRY KX_HOURLY_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)
C***********************************************************************
! TF_OBJT
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                             trim(GET_SCENAME())//KX_HOURLY_CODE
         OVERHEAD_LENGTH = 33
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         RPT_UNIT_NUM = KX_HOURLY_NO
         DIMENSIONS = 5
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Month               ','C',INT2(9),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Day                 ','N',INT2(4),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Resource ID         ','C',INT2(12),'F','D'
         R_NEXT_REC = NEXT_REC + 3
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         KX_HOURLY_HEADER = KX_HOURLY_NO
      RETURN
C***********************************************************************
      ENTRY QX_HOURLY_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)
C***********************************************************************
! TF_OBJT
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                             trim(GET_SCENAME())//QX_HOURLY_CODE
         OVERHEAD_LENGTH = 33
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         RPT_UNIT_NUM = QX_HOURLY_NO
         DIMENSIONS = 5
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Month               ','C',INT2(9),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Day                 ','N',INT2(4),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Resource ID         ','C',INT2(12),'F','D'
         R_NEXT_REC = NEXT_REC + 3
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         QX_HOURLY_HEADER = QX_HOURLY_NO
      RETURN
C***********************************************************************
      ENTRY HOURLY_FLOW_ANALYST_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)
C***********************************************************************
! TF_OBJT
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                   trim(GET_SCENAME())//HOURLY_FLOW_ANALYST_CODE
         OVERHEAD_LENGTH = 90
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         RPT_UNIT_NUM = HOURLY_FLOW_ANALYST_NO
         DIMENSIONS = 5
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Month               ','C',INT2(9),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Day                 ','N',INT2(4),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Resource            ','C',INT2(65),'F','D'
         R_NEXT_REC = NEXT_REC + 3
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         HOURLY_FLOW_ANALYST_HEADER = HOURLY_FLOW_ANALYST_NO
      RETURN
C***********************************************************************
      ENTRY WX_HOURLY_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)
C***********************************************************************
! TF_OBJT
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                             trim(GET_SCENAME())//WX_HOURLY_CODE
         OVERHEAD_LENGTH = 41
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         RPT_UNIT_NUM = WX_HOURLY_NO
         DIMENSIONS = 5
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Month               ','C',INT2(9),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Day                 ','N',INT2(4),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Resource            ','C',INT2(20),'F','D'
         R_NEXT_REC = NEXT_REC + 3
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         WX_HOURLY_HEADER = WX_HOURLY_NO
      RETURN
C***********************************************************************
      ENTRY HOURLY_FLOW_CONSTRAINT_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)
C***********************************************************************
! TF_OBJT
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                trim(GET_SCENAME())//HOURLY_FLOW_CONSTRAINT_CODE
         OVERHEAD_LENGTH = 90
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         RPT_UNIT_NUM = HOURLY_FLOW_CONSTRAINT_NO
         DIMENSIONS = 5
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Month               ','C',INT2(9),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Day                 ','N',INT2(4),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Resource            ','C',INT2(65),'F','D'
         R_NEXT_REC = NEXT_REC + 3
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         HOURLY_FLOW_CONSTRAINT_HEADER = HOURLY_FLOW_CONSTRAINT_NO
      RETURN
C***********************************************************************
      ENTRY C_HOURLY_COST_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)
C***********************************************************************
! TF_OBJT
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                         trim(GET_SCENAME())//C_HOURLY_COST_CODE
         OVERHEAD_LENGTH = 41
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         RPT_UNIT_NUM = C_HOURLY_COST_UNIT
         DIMENSIONS = 5
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Month               ','C',INT2(9),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Day                 ','N',INT2(4),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Resource            ','C',INT2(20),'F','D'
         R_NEXT_REC = NEXT_REC + 3
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         C_HOURLY_COST_HEADER = C_HOURLY_COST_UNIT
      RETURN
C***********************************************************************
      ENTRY C_HOURLY_MWH_COST_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)
C***********************************************************************
! TF_OBJT
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                     trim(GET_SCENAME())//C_HOURLY_MWH_COST_CODE
         OVERHEAD_LENGTH = 41
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         RPT_UNIT_NUM = C_HOURLY_MWH_COST_UNIT
         DIMENSIONS = 5
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Month               ','C',INT2(9),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Day                 ','N',INT2(4),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Resource            ','C',INT2(20),'F','D'
         R_NEXT_REC = NEXT_REC + 3
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         C_HOURLY_MWH_COST_HEADER = C_HOURLY_MWH_COST_UNIT
      RETURN
C***********************************************************************
      ENTRY C_HOURLY_SUMMARY_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)
C***********************************************************************
! TF_OBJT
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                      trim(GET_SCENAME())//C_HOURLY_SUMMARY_CODE
         OVERHEAD_LENGTH = 41
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         RPT_UNIT_NUM = C_HOURLY_SUMMARY_UNIT
         DIMENSIONS = 5
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Month               ','C',INT2(9),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Day                 ','N',INT2(4),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Resource            ','C',INT2(20),'F','D'
         R_NEXT_REC = NEXT_REC + 3
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         C_HOURLY_SUMMARY_HEADER = C_HOURLY_SUMMARY_UNIT
      RETURN
C***********************************************************************
      ENTRY C_TG_HOURLY_SUMMARY_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)
C***********************************************************************
! TF_OBJT
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                   trim(GET_SCENAME())//C_TG_HOURLY_SUMMARY_CODE
         OVERHEAD_LENGTH = 80
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         RPT_UNIT_NUM = C_TG_HOURLY_SUMMARY_UNIT
         DIMENSIONS = 6
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Month               ','C',INT2(9),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Transaction Group   ','C',INT2(35),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Day                 ','N',INT2(4),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+3)
     +                       'Resource            ','C',INT2(20),'F','D'
         R_NEXT_REC = NEXT_REC + 4
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         C_TG_HOURLY_SUMMARY_HEADER = C_TG_HOURLY_SUMMARY_UNIT
      RETURN
C***********************************************************************
      ENTRY DX_HOURLY_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)
C***********************************************************************
! TF_OBJT
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                             trim(GET_SCENAME())//DX_HOURLY_CODE
         OVERHEAD_LENGTH = 41
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         RPT_UNIT_NUM = DX_HOURLY_NO
         DIMENSIONS = 5
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Month               ','C',INT2(9),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Day                 ','N',INT2(4),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Resource            ','C',INT2(20),'F','D'
         R_NEXT_REC = NEXT_REC + 3
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         DX_HOURLY_HEADER = DX_HOURLY_NO
      RETURN
C***********************************************************************
      ENTRY GY_HOURLY_HEADER(R_NEXT_REC)
C***********************************************************************
! TF_OBJT
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                   trim(GET_SCENAME())//DEPTH_HOURLY_LAMBDA_CODE
         OVERHEAD_LENGTH = 21
         VARIABLE_NUMBER = 24
         RPT_UNIT_NUM = DEPTH_HOURLY_LAMBDA_UNIT
         DIMENSIONS = 4
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Month               ','C',INT2(9),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Day                 ','N',INT2(4),'F','D'
!         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
!     +                       'Resource            ','C',INT2(20),'F','D'
         R_NEXT_REC = NEXT_REC + 2
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         GY_HOURLY_HEADER = DEPTH_HOURLY_LAMBDA_UNIT
      RETURN
C***********************************************************************
      ENTRY OX_HOURLY_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)
C***********************************************************************
C
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                             trim(GET_SCENAME())//OX_HOURLY_CODE
         OVERHEAD_LENGTH = 41
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         RPT_UNIT_NUM = OX_HOURLY_NO
         DIMENSIONS = 5
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Month               ','C',INT2(9),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Day                 ','N',INT2(4),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Resource            ','C',INT2(20),'F','D'
         R_NEXT_REC = NEXT_REC + 3
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         OX_HOURLY_HEADER = OX_HOURLY_NO
      RETURN
C***********************************************************************
      ENTRY RC_REVENUE_FORECAST_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)
C***********************************************************************
C
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                       trim(GET_SCENAME())//".RCD" ! RC_REV_FORECAST_CODE
         OVERHEAD_LENGTH = 60
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         RPT_UNIT_NUM = RC_REV_FORECAST_NO
         DIMENSIONS = 5
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                                OVERHEAD_LENGTH,
     +                                                VARIABLE_NUMBER,
     +                                                DIMENSIONS,
     +                                                RPT_UNIT_NUM,
     +                                                NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Account Type        ','C',INT2(8),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +              'Account Name        ','C',INT2(40),'V','D',INT2(36)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Period              ','C',INT2(6),'V','D'
         R_NEXT_REC = NEXT_REC + 3
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         RC_REVENUE_FORECAST_HEADER = RC_REV_FORECAST_NO
      RETURN
C***********************************************************************
      ENTRY HEC_HOURLY_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)
C***********************************************************************
! CREATED 3/28/01.
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                            trim(GET_SCENAME())//HEC_HOURLY_CODE
         OVERHEAD_LENGTH = 41
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         RPT_UNIT_NUM = HEC_HOURLY_NO
         DIMENSIONS = 5
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Month               ','C',INT2(9),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Day                 ','N',INT2(4),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Resource            ','C',INT2(20),'F','D'
         R_NEXT_REC = NEXT_REC + 3
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         HEC_HOURLY_HEADER = HEC_HOURLY_NO
      RETURN
C***********************************************************************
      ENTRY EXISTING_ASSET_RPT_HEADER(R_NEXT_REC)
C***********************************************************************
! MSGMMEA FINACEA
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                          trim(GET_SCENAME())//EA_REPORTING_CODE
         OVERHEAD_LENGTH = 45
         VARIABLE_NUMBER = 15
         DIMENSIONS = 3
         RPT_UNIT_NUM = EA_REPORTING_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE_WO(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Endpoint            ','N',INT2(4),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +              'Account Name        ','C',INT2(34),'V','D',INT2(30)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Year                ','N',INT2(4),'V','D'
         R_NEXT_REC = NEXT_REC + 3
C
         EXISTING_ASSET_RPT_HEADER = EA_REPORTING_UNIT
      RETURN
C***********************************************************************
      ENTRY WVPA_ASSET_RPT_HEADER(R_NEXT_REC)
C***********************************************************************
! MSGMMEA FINACEA
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"WVP"//
     +                          trim(GET_SCENAME())//EA_REPORTING_CODE
         OVERHEAD_LENGTH = 45
         VARIABLE_NUMBER = 30
         DIMENSIONS = 5
         RPT_UNIT_NUM = EA_REPORTING_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE_WO(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Endpoint            ','N',INT2(4),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Year                ','N',INT2(4),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +              'Department Name     ','C',INT2(5),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+3)
     +              'Primary Account     ','C',INT2(40),'V','D',INT2(36)
!         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+3)
!     +              'Account Name        ','C',INT2(34),'V','D',INT2(30)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+4)
     +                       'Period              ','C',INT2(6),'F','D'
         R_NEXT_REC = NEXT_REC + 5
C
         WVPA_ASSET_RPT_HEADER = EA_REPORTING_UNIT
      RETURN
C***********************************************************************
      ENTRY WVPA_COOP_REVENUE_HEADER(R_NEXT_REC)
C***********************************************************************
C
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"WVP"//
     +                                     trim(GET_SCENAME())//".RCD" ! RC_REV_FORECAST_CODE
         OVERHEAD_LENGTH = 60
         VARIABLE_NUMBER = 30
         RPT_UNIT_NUM = RC_REV_FORECAST_NO
         DIMENSIONS = 4
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                                OVERHEAD_LENGTH,
     +                                                VARIABLE_NUMBER,
     +                                                DIMENSIONS,
     +                                                RPT_UNIT_NUM,
     +                                                NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +              'Member Name         ','C',INT2(40),'V','D',INT2(36)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Period              ','C',INT2(6),'V','D'
         R_NEXT_REC = NEXT_REC + 2
C
         WVPA_COOP_REVENUE_HEADER = RC_REV_FORECAST_NO
      RETURN
C***********************************************************************
      ENTRY WVPA_WORKORDER_CONTROL_RPT(R_NEXT_REC)
C***********************************************************************
! MSGMMEA FINACEA
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"WVP"//
     +                     trim(GET_SCENAME())//WVPA_CONTROL_REPT_CODE
         OVERHEAD_LENGTH = 45
         VARIABLE_NUMBER = 30
         DIMENSIONS = 3
         RPT_UNIT_NUM = WVPA_CONTROL_REPT_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE_WO(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +              'Endpoint            ','N',INT2(4),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +              'Department Head     ','C',INT2(15),'V','D'
!         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
!     +              'Year                ','N',INT2(4),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +              'Work Order          ','C',
     +                        INT2(DrillingAccountNameWidth),'V','D',
     +                        INT2(DrillingAccountNameWidth-4)
         R_NEXT_REC = NEXT_REC + 3
C
         WVPA_WORKORDER_CONTROL_RPT = WVPA_CONTROL_REPT_UNIT
      RETURN
C***********************************************************************
      ENTRY WVPA_WORKORDER_EXPENSES_RPT(R_NEXT_REC)
C***********************************************************************
! MSGMMEA FINACEA
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"WVP"//
     +                 trim(GET_SCENAME())//WVPA_WO_EXPENSES_REPT_CODE
         OVERHEAD_LENGTH = 45
         VARIABLE_NUMBER = 30
         DIMENSIONS = 4
         RPT_UNIT_NUM = WVPA_WO_EXPENSES_REPT_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE_WO(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +              'Endpoint            ','N',INT2(4),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +              'Department Head     ','C',INT2(15),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +              'Year                ','N',INT2(4),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+3)
     +              'Work Order          ','C',
     +                        INT2(DrillingAccountNameWidth),'V','D',
     +                        INT2(DrillingAccountNameWidth-4)
         R_NEXT_REC = NEXT_REC + 4
C
         WVPA_WORKORDER_EXPENSES_RPT = WVPA_WO_EXPENSES_REPT_UNIT
      RETURN
C***********************************************************************
      ENTRY WVPA_WORKORDER_SUMMARY_RPT(R_NEXT_REC)
C***********************************************************************
! MSGMMEA FINACEA
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"WVP"//
     +                  trim(GET_SCENAME())//WVPA_WO_SUMMARY_REPT_CODE
         OVERHEAD_LENGTH = 45
         VARIABLE_NUMBER = 30
         DIMENSIONS = 5
         RPT_UNIT_NUM = WVPA_WO_SUMMARY_REPT_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE_WO(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +              'Endpoint            ','N',INT2(4),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +              'Year                ','N',INT2(4),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +              'Department Head     ','C',INT2(15),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+3)
     +              'Work Order          ','C',
     +                        INT2(DrillingAccountNameWidth),'V','D',
     +                        INT2(DrillingAccountNameWidth-4)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+4)
     +              'Month Ending        ','C',INT2(6),'F','D'
         R_NEXT_REC = NEXT_REC + DIMENSIONS
C
         WVPA_WORKORDER_SUMMARY_RPT = WVPA_WO_SUMMARY_REPT_UNIT
      RETURN
C***********************************************************************
      ENTRY WVPA_DEBT_FILE_RPT_HEADER(R_NEXT_REC)
C***********************************************************************
! MSGMMDB FINACDBT
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"WVP"//
     +                          trim(GET_SCENAME())//DB_REPORTING_CODE
         OVERHEAD_LENGTH = 46
         VARIABLE_NUMBER = 20
         DIMENSIONS = 4
         RPT_UNIT_NUM = DB_REPORTING_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE_WO(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Endpoint            ','N',INT2(4),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Year                ','N',INT2(4),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +              'Issue Name          ','C',INT2(34),'V','D',INT2(30)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+3)
     +                       'Period              ','C',INT2(6),'F','D'
         R_NEXT_REC = NEXT_REC + 4
C
         WVPA_DEBT_FILE_RPT_HEADER = DB_REPORTING_UNIT
      RETURN
C***********************************************************************
      ENTRY WVPA_MORTGAGE_SUMMARY_RPT_HEADR(R_NEXT_REC)
C***********************************************************************
! MSGMMDB FINACDBT
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"WVP"//
     +              trim(GET_SCENAME())//WVPA_MORTGAGE_SUMRY_REPT_CODE
         OVERHEAD_LENGTH = 80
         VARIABLE_NUMBER = 10
         DIMENSIONS = 5
         RPT_UNIT_NUM = WVPA_MORTGAGE_SUMRY_REPT_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE_WO(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Endpoint            ','N',INT2(4),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Year                ','N',INT2(4),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +              'Issue Name          ','C',INT2(34),'V','D',INT2(30)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+3)
     +              'Mortgage Debt Type  ','C',INT2(30),'V','D',INT2(30)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+4)
     +                       'Period              ','C',INT2(6),'F','D'
         R_NEXT_REC = NEXT_REC + 5
C
         WVPA_MORTGAGE_SUMMARY_RPT_HEADR = WVPA_MORTGAGE_SUMRY_REPT_UNIT
      RETURN
C***********************************************************************
      ENTRY WVPA_PLNT_BALANCES_RPT_HEADR(R_NEXT_REC)
C***********************************************************************
! MSGMMDB FINACDBT
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"WVP"//
     +                                    trim(GET_SCENAME())//'.WPD' ! WVPA_PLNT_BALANCES_RPT_CODE
         OVERHEAD_LENGTH = 80
         VARIABLE_NUMBER = 200
         DIMENSIONS = 5
         RPT_UNIT_NUM = 5286 ! WVPA_PLNT_BALANCES_RPT_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE_WO(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                      'Endpoint            ','N',INT2(4),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                      'Year                ','N',INT2(4),'V','D'
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                      'Period              ','C',INT2(6),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+3)
     +                      'Company             ','C',INT2(10),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+4)
     +                      'Plant Values        ','C',INT2(30),'V','D'
         R_NEXT_REC = NEXT_REC + DIMENSIONS
C
         WVPA_PLNT_BALANCES_RPT_HEADR = RPT_UNIT_NUM
      RETURN

C***********************************************************************
      ENTRY WVPA_COMP_CONSTR_SUMY_RPT_HEADR(R_NEXT_REC)
C***********************************************************************
! MSGMMDB FINACDBT
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"WVP"//
     +                                     trim(GET_SCENAME())//'.CCD' ! WVPA_PLNT_BALANCES_RPT_CODE
         OVERHEAD_LENGTH = 80
         VARIABLE_NUMBER = 30
         DIMENSIONS = 5
         RPT_UNIT_NUM = 5291 ! WVPA_PLNT_BALANCES_RPT_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE_WO(FILE_NAME,
     +                                                  OVERHEAD_LENGTH,
     +                                                  VARIABLE_NUMBER,
     +                                                  DIMENSIONS,
     +                                                  RPT_UNIT_NUM,
     +                                                  NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                        'Endpoint            ','N',INT2(4),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                        'Year                ','N',INT2(4),'V','D'
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                        'Period              ','C',INT2(6),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+3)
     +                       'Company             ','C',INT2(10),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+4)
     +                       'Account-Work Order  ','C',INT2(40),'V','D'
         R_NEXT_REC = NEXT_REC + DIMENSIONS
C
         WVPA_COMP_CONSTR_SUMY_RPT_HEADR = RPT_UNIT_NUM
      RETURN
C***********************************************************************
      ENTRY WVPA_CONSTR_SUMY_RPT_HEADR(R_NEXT_REC)
C***********************************************************************
! MSGMMDB FINACDBT
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"WVP"//
     +                                    trim(GET_SCENAME())//'.WAD' ! WVPA_PLNT_BALANCES_RPT_CODE
         OVERHEAD_LENGTH = 80
         VARIABLE_NUMBER = 30
         DIMENSIONS = 5
         RPT_UNIT_NUM = 5288 ! WVPA_PLNT_BALANCES_RPT_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE_WO(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                      'Endpoint            ','N',INT2(4),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                      'Year                ','N',INT2(4),'V','D'
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                      'Period              ','C',INT2(6),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+3)
     +                      'Account Name        ','C',INT2(20),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+4)
     +                      'Work Order          ','C',INT2(34),'V','D'
         R_NEXT_REC = NEXT_REC + DIMENSIONS
C
         WVPA_CONSTR_SUMY_RPT_HEADR = RPT_UNIT_NUM
      RETURN
C***********************************************************************
      ENTRY WVPA_CUM_DEP_RPT_HEADR(R_NEXT_REC)
C***********************************************************************
! MSGMMDB FINACDBT
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"WVP"//
     +                                    trim(GET_SCENAME())//'.WDD' ! WVPA_PLNT_BALANCES_RPT_CODE
         OVERHEAD_LENGTH = 80
         VARIABLE_NUMBER = 200
         DIMENSIONS = 5
         RPT_UNIT_NUM = 5287 ! WVPA_PLNT_BALANCES_RPT_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE_WO(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                      'Endpoint            ','N',INT2(4),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                      'Year                ','N',INT2(4),'V','D'
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Period              ','C',INT2(6),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+3)
     +                      'Company             ','C',INT2(10),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+4)
     +                      'Plant Values        ','C',INT2(30),'V','D'
         R_NEXT_REC = NEXT_REC + DIMENSIONS
C
         WVPA_CUM_DEP_RPT_HEADR = RPT_UNIT_NUM
      RETURN
C***********************************************************************
      ENTRY WVPA_RATE_TRACKER_HEADR(R_NEXT_REC)
C***********************************************************************
! MSGMMDB FINACDBT
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"WVP"//
     +                                    trim(GET_SCENAME())//'.RTD' ! WVPA_PLNT_BALANCES_RPT_CODE
         OVERHEAD_LENGTH = 80
         VARIABLE_NUMBER = 200
         DIMENSIONS = 3
         RPT_UNIT_NUM = 5289 ! WVPA_PLNT_BALANCES_RPT_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE_WO(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                      'Endpoint            ','N',INT2(4),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                      'Year                ','N',INT2(4),'V','D'
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Period              ','C',INT2(6),'F','D'
         R_NEXT_REC = NEXT_REC + 3
C
         WVPA_RATE_TRACKER_HEADR = RPT_UNIT_NUM
      RETURN
C***********************************************************************
      ENTRY FUTURE_ASSET_RPT_HEADER(R_NEXT_REC,ACTIVE_DIMENSION)
C***********************************************************************
! FUTURE ASSETS
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                          trim(GET_SCENAME())//FA_REPORTING_CODE
         IF(ACTIVE_DIMENSION == 'M') THEN
            DIMENSIONS = 4
         ELSE
            DIMENSIONS = 3
         ENDIF
         OVERHEAD_LENGTH = 46

         VARIABLE_NUMBER =30
         RPT_UNIT_NUM = FA_REPORTING_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE_WO(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Endpoint            ','N',INT2(4),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +              'Account Name        ','C',
     +                        INT2(DrillingAccountNameWidth),'V','D',
     +                        INT2(DrillingAccountNameWidth-4)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Year                ','N',INT2(4),'V','D'
         IF(ACTIVE_DIMENSION == 'M') THEN
            WRITE(RPT_UNIT_NUM,REC=NEXT_REC+3)
     +                       'Period              ','C',INT2(10),'V','D'
            R_NEXT_REC = NEXT_REC + 4
         ELSE
            R_NEXT_REC = NEXT_REC + 3
         ENDIF
C
         FUTURE_ASSET_RPT_HEADER = FA_REPORTING_UNIT
      RETURN
C***********************************************************************
      ENTRY DEBIT_FILE_RPT_HEADER(R_NEXT_REC)
C***********************************************************************
! MSGMMDFD FINACDB
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                          trim(GET_SCENAME())//DD_REPORTING_CODE
         OVERHEAD_LENGTH = 46
         VARIABLE_NUMBER = 25
         DIMENSIONS = 3
         RPT_UNIT_NUM = DD_REPORTING_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE_WO(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Endpoint            ','N',INT2(4),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
!     +                       'Asset Class ID      ','N',INT2(4),'V','D'
!         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +              'Account Name        ','C',INT2(34),'V','D',INT2(30)
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
!         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+3)
     +                       'Year                ','N',INT2(4),'V','D'
          R_NEXT_REC = NEXT_REC + 3
!         R_NEXT_REC = NEXT_REC + 4
C
         DEBIT_FILE_RPT_HEADER = DD_REPORTING_UNIT
      RETURN
C***********************************************************************
      ENTRY TRAILING_12_MOS_RPT_HEADER(R_NEXT_REC)
C***********************************************************************
! MSGMMDFD FINACDB
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                       trim(GET_SCENAME())//T12MO_REPORTING_CODE
         OVERHEAD_LENGTH = 15
         VARIABLE_NUMBER = 300
         DIMENSIONS = 3
         RPT_UNIT_NUM = T12MO_REPORTING_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       '12 Months Ending    ','C',INT2(7),'V','D'
         R_NEXT_REC = NEXT_REC + 1
C
         TRAILING_12_MOS_RPT_HEADER = T12MO_REPORTING_UNIT
      RETURN
C***********************************************************************
      ENTRY IPL_REGULATED_REVENUES_RPT_HEADER(R_NEXT_REC)
C***********************************************************************
! MSGMMDFD FINACDB
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"IPL"//
     +                       trim(GET_SCENAME())//IPL_REG_REVENUE_CODE
         OVERHEAD_LENGTH = 78
         VARIABLE_NUMBER = 30
         DIMENSIONS = 5
         RPT_UNIT_NUM = IPL_REG_REVENUE_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE_WO(FILE_NAME,
     +                                                  OVERHEAD_LENGTH,
     +                                                  VARIABLE_NUMBER,
     +                                                  DIMENSIONS,
     +                                                  RPT_UNIT_NUM,
     +                                                  NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Company             ','C',INT2(38),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1) 'Endpoint            ','N',
     +                                                   INT2(4),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2) 'Year                ','N',
     +                                                   INT2(4),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+3)
     +                       '12 Months Ending    ','C',INT2(7),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+4)
     +                       'Columns             ','C',INT2(25),'V','D'
         R_NEXT_REC = NEXT_REC + 5
C
         IPL_REGULATED_REVENUES_RPT_HEADER = IPL_REG_REVENUE_UNIT
      RETURN
C***********************************************************************
      ENTRY DEBT_FILE_RPT_HEADER(R_NEXT_REC,ADD_MONTHLY_DIMENSION)
C***********************************************************************
! MSGMMDB FINACDBT
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                          trim(GET_SCENAME())//DB_REPORTING_CODE
         OVERHEAD_LENGTH = 46
         VARIABLE_NUMBER = 40
! 013114. COPIED FROM OLDER SOURCE TO REMOVE DIMENSION FOR ODEC.
         DIMENSIONS = 3
         IF(ADD_MONTHLY_DIMENSION) DIMENSIONS = 4
         RPT_UNIT_NUM = DB_REPORTING_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE_WO(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Endpoint            ','N',INT2(4),'V','D'
         IF(ADD_MONTHLY_DIMENSION) THEN
            WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Year                ','N',INT2(4),'V','D'
            WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +              'Account Name        ','C',INT2(34),'V','D',INT2(30)
            WRITE(RPT_UNIT_NUM,REC=NEXT_REC+3)
     +                       'Period              ','C',INT2(6),'F','D'
            R_NEXT_REC = NEXT_REC + 4
         ELSE
            WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +              'Account Name        ','C',INT2(34),'V','D',INT2(30)
            WRITE(RPT_UNIT_NUM,REC=NEXT_REC+2)
     +                       'Year                ','N',INT2(4),'V','D'
            R_NEXT_REC = NEXT_REC + 3
         ENDIF
C
         DEBT_FILE_RPT_HEADER = DB_REPORTING_UNIT
      RETURN
C***********************************************************************
      ENTRY ANNUAL_SERVICE_TRANS_HEADER(R_NEXT_REC)
C***********************************************************************
C
! SERVICAC FILE
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                   trim(GET_SCENAME())//ANNUAL_TRANSACTIONS_CODE
         OVERHEAD_LENGTH = 40
         VARIABLE_NUMBER = 10
         DIMENSIONS = 4
         RPT_UNIT_NUM = ANNUAL_TRANSACTIONS_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Description         ','C',INT2(25),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                        'Revenue/Expense     ','C',INT2(7),'V','D'
         R_NEXT_REC = NEXT_REC + 2
C
         ANNUAL_SERVICE_TRANS_HEADER = ANNUAL_TRANSACTIONS_UNIT
      RETURN
C***********************************************************************
      ENTRY DYNAMIC_JURIS_RPT_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)
C***********************************************************************
! MSGMMREV
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                         trim(GET_SCENAME())//DYNAMIC_JURIS_CODE
         OVERHEAD_LENGTH = 8
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         DIMENSIONS = 2
         RPT_UNIT_NUM = DYNAMIC_JURIS_UNIT
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
         R_NEXT_REC = NEXT_REC
C
         DYNAMIC_JURIS_RPT_HEADER = DYNAMIC_JURIS_UNIT
      RETURN
C***********************************************************************
      ENTRY MONTH_GROUP_HEADER(R_VARIABLE_NUMBER,R_NEXT_REC)
C***********************************************************************
! DR_BOOTH
C
         FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"//
     +                           trim(GET_SCENAME())//MONTH_GROUP_CODE
         OVERHEAD_LENGTH = 37
         VARIABLE_NUMBER = R_VARIABLE_NUMBER
         RPT_UNIT_NUM = MONTH_GROUP_UNIT
         DIMENSIONS = 4
         RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                            OVERHEAD_LENGTH,
     +                                            VARIABLE_NUMBER,
     +                                            DIMENSIONS,
     +                                            RPT_UNIT_NUM,
     +                                            NEXT_REC)
C
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)
     +                       'Month               ','C',INT2(9),'F','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)
     +                       'Group Name          ','C',INT2(20),'F','D'
         R_NEXT_REC = NEXT_REC + 2
C
         IREC_SAVED = RPTREC(RPT_UNIT_NUM,SAVE_REC=R_NEXT_REC,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
         MONTH_GROUP_HEADER = MONTH_GROUP_UNIT
      RETURN
      END
!
! END DETAILED REPORTS
!
C***********************************************************************
      FUNCTION ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,
     +                                      OVERHEAD_LENGTH,
     +                                      VARIABLE_NUMBER,
     +                                      DIMENSIONS,
     +                                      RPT_UNIT_NUM,
     +                                      NEXT_REC)
       use logging
C***********************************************************************
      INTEGER*2 ESTABLISH_DRILLING_REPORT_FILE,RECORD_LENGTH,
     +          VARIABLE_NUMBER,DIMENSIONS,BASE_YEAR,
     +          RPT_UNIT_NUM,OVERHEAD_LENGTH,
     +          NEXT_REC,ESTABLISH_DETAIL_REPORT_FILE,
     +          ENDYR,EXTENSION_YEARS,
     +          GET_NUM_OF_END_POINTS
      CHARACTER*(*) FILE_NAME
      CHARACTER*40 TITLE
      CHARACTER*28 COMPANY_NAME
      INTEGER*1 F7/z"f7"/
      LOGICAL*1 LAHEY_LF95
      LOGICAL FILE_EXISTS
C
         RECORD_LENGTH = MAX(64,OVERHEAD_LENGTH + 4*VARIABLE_NUMBER)
         OPEN(RPT_UNIT_NUM,FILE=FILE_NAME,ACCESS="DIRECT",
     +                              STATUS="REPLACE",RECL=RECORD_LENGTH)
C
         IF(LAHEY_LF95()) THEN
            WRITE(RPT_UNIT_NUM,REC=1) F7,RECORD_LENGTH,
     +                                DIMENSIONS,
     +                                BASE_YEAR(),
     +                                ENDYR()+EXTENSION_YEARS(),
     +                                GET_NUM_OF_END_POINTS()
            NEXT_REC = 2
         ELSE
            CLOSE(RPT_UNIT_NUM)
C
            OPEN(RPT_UNIT_NUM,FILE=FILE_NAME,
     +                                ACCESS="TRANSPARENT",STATUS="OLD")
            WRITE(RPT_UNIT_NUM,REC=4) DIMENSIONS,
     +                                BASE_YEAR(),
     +                                ENDYR()+EXTENSION_YEARS(),
     +                                GET_NUM_OF_END_POINTS()
            CLOSE(RPT_UNIT_NUM)
C
            OPEN(RPT_UNIT_NUM,FILE=FILE_NAME,ACCESS="DIRECT",
     +                                  STATUS="OLD",RECL=RECORD_LENGTH)
            NEXT_REC = 1
         ENDIF
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) COMPANY_NAME()
         NEXT_REC = NEXT_REC+1
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) TITLE()
         NEXT_REC = NEXT_REC+1
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) 'Endpoint            ','N',
     +                                                   INT2(4),'F','D'
         NEXT_REC = NEXT_REC+1
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) 'Year                ','N',
     +                                                   INT2(4),'F','D'
         NEXT_REC = NEXT_REC+1
         ESTABLISH_DETAIL_REPORT_FILE = RECORD_LENGTH
      RETURN
      END
C***********************************************************************
      FUNCTION ESTABLISH_DETAIL_REPORT_FILE_WO(FILE_NAME,
     +                                         OVERHEAD_LENGTH,
     +                                         VARIABLE_NUMBER,
     +                                         DIMENSIONS,
     +                                         RPT_UNIT_NUM,
     +                                         NEXT_REC)
       use logging
C***********************************************************************
      INTEGER*2 ESTABLISH_DRILLING_REPORT_FILE,RECORD_LENGTH,
     +          VARIABLE_NUMBER,DIMENSIONS,BASE_YEAR,
     +          RPT_UNIT_NUM,OVERHEAD_LENGTH,
     +          NEXT_REC,ESTABLISH_DETAIL_REPORT_FILE_WO,
     +          ENDYR,EXTENSION_YEARS,
     +          GET_NUM_OF_END_POINTS
      CHARACTER*(*) FILE_NAME
      CHARACTER*40 TITLE
      CHARACTER*28 COMPANY_NAME
      INTEGER*1 F7/z"f7"/
      LOGICAL*1 LAHEY_LF95
C
          RECORD_LENGTH = MAX(64,OVERHEAD_LENGTH + 4*VARIABLE_NUMBER)
         OPEN(RPT_UNIT_NUM,FILE=FILE_NAME,ACCESS="DIRECT",
     +                              STATUS="REPLACE",RECL=RECORD_LENGTH)
C
         IF(LAHEY_LF95()) THEN

            WRITE(RPT_UNIT_NUM,REC=1) F7,RECORD_LENGTH,
     +                                DIMENSIONS,
     +                                BASE_YEAR(),
     +                                ENDYR()+EXTENSION_YEARS(),
     +                                GET_NUM_OF_END_POINTS()
            NEXT_REC = 2
         ELSE
            CLOSE(RPT_UNIT_NUM)
C
            OPEN(RPT_UNIT_NUM,FILE=FILE_NAME,
     +                                ACCESS="TRANSPARENT",STATUS="OLD")
            WRITE(RPT_UNIT_NUM,REC=4) DIMENSIONS,
     +                                BASE_YEAR(),
     +                                ENDYR()+EXTENSION_YEARS(),
     +                                GET_NUM_OF_END_POINTS()
            CLOSE(RPT_UNIT_NUM)
C
            OPEN(RPT_UNIT_NUM,FILE=FILE_NAME,ACCESS="DIRECT",
     +                                  STATUS="OLD",RECL=RECORD_LENGTH)
            NEXT_REC = 1
         ENDIF
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) COMPANY_NAME()
         NEXT_REC = NEXT_REC+1
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC) TITLE()
         NEXT_REC = NEXT_REC+1
         ESTABLISH_DETAIL_REPORT_FILE_WO = RECORD_LENGTH
      RETURN
      END
! END DETAILED REPORTS

