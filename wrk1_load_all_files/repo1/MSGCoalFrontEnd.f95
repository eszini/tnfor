      END

!     ******************************************************************
!     MSGCoalFrontEnd.f90
!     Copyright(c)  2000
!
!     Created: 12/8/2010 3:41:11 PM
!     Author : MARK S GERBER
!     Last change: MSG 12/8/2010 5:16:45 PM
!     ******************************************************************
      USE realwin
      CHARACTER (LEN=256) :: CURRENT_DIRECTORY,COMMAND_LINE,
     +                      UC_COMMAND_LINE
      INTEGER IY,CHDIR
      CHARACTER (LEN=5) :: SCENAME,BASE_FILE_DEFINITION,BASE_FILE_FAMILY
      INTEGER (KIND=2) :: BASE_YEAR,ENDYR
C MOVES TO CORRECT DIRECTORY FOR THE DEBUGGER )
      CALL get_environment_variable("DEBUGDIR",CURRENT_DIRECTORY)
      IF(LEN_TRIM(CURRENT_DIRECTORY)) >=1) THEN
         READ(CURRENT_DIRECTORY,*) UC_COMMAND_LINE,
     +                             COMMAND_LINE 
         IY = CHDIR(UC_COMMAND_LINE)
      ELSE

        call getcl(COMMAND_LINE)
     
      ENDIF
      CALL CURDIR(" ",CURRENT_DIRECTORY)
! the command line will be
! study project base family base_year end_year
! this infromation that is current inputted from
! MSGPARAM.VAL and the standard command line values
      read(COMMAND_LINE,*)SCENAME,BASE_FILE_DEFINITION,BASE_FILE_FAMILY,
     +                    BASE_YEAR,ENDYR
      write(*,*) SCENAME,BASE_FILE_DEFINITION,BASE_FILE_FAMILY,
     +           BASE_YEAR,ENDYR
      call end_program("Stop requested from MSGCoalFrontEnd CFE1")
