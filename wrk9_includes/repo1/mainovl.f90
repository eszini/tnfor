!     ******************************************************************
!     Mainovl.for
!     Created: 11/11/02 10:25:53 AM
!     Author : msg
!     Last change: GT 9/14/2006 10:14:56 AM
!     ******************************************************************

      SUBROUTINE START_DISPLAY_TIME
      INCLUDE 'SPINLIB.MON'
      CHARACTER(len=6) :: TIME_STRING
      INTEGER(kind=4) :: SYMSTART,SYMSTOP,TOTIME,TEMP_TIME, &
        R_OVERHEAD_TIME, EXECUTION_TIME

      INTEGER(kind=4) :: HOURS,MINUTES,SECONDS,R_INDEXING_TIME
      INTEGER(kind=4) :: TOTAL_TIME=0,LAST_TIME=0,OVERHEAD_TIME=0, &
                INDEXING_START_TIME=0,INDEXING_STOP_TIME=0
      INTEGER(kind=2) :: INVERSE,INVERSE_VIDEO
      LOGICAL(kind=1) :: LF95,LAHEY_LF95
      SAVE SYMSTART,INVERSE,LF95
      CHARACTER(len=*) :: CODE_LOCATION
      CHARACTER(len=11) :: CLK
!
         INVERSE = INVERSE_VIDEO()
         CALL TIMER(LAST_TIME)
         SYMSTART = LAST_TIME
         LF95 = LAHEY_LF95()
      RETURN
!
      ENTRY CALULATE_OVERHEAD_TIME
         CALL TIMER(OVERHEAD_TIME)
         IF(OVERHEAD_TIME < SYMSTART) THEN
            OVERHEAD_TIME = 8640000 - SYMSTART + OVERHEAD_TIME
         ELSE
            OVERHEAD_TIME = OVERHEAD_TIME - SYMSTART
         ENDIF
!
      ENTRY DISPLAY_TIME
         CALL TIMER(SYMSTOP)
         IF(SYMSTOP < LAST_TIME) THEN
            TOTAL_TIME = TOTAL_TIME + 8640000 - LAST_TIME
            LAST_TIME = 0
         ENDIF
         TOTAL_TIME = TOTAL_TIME + SYMSTOP - LAST_TIME
         LAST_TIME = SYMSTOP
         TEMP_TIME = TOTAL_TIME/100
         SECONDS = MOD(TEMP_TIME,60)
         MINUTES = TEMP_TIME - SECONDS
         MINUTES = MOD(MINUTES,3600)/60
         HOURS = (TEMP_TIME-SECONDS-MINUTES*60)/3600
         IF(HOURS == 0) THEN
             WRITE(TIME_STRING,"(1X,I2,':',I2.2)") MINUTES,SECONDS
         ELSE
             WRITE(TIME_STRING,"(I3,':',I2.2)") HOURS,MINUTES
         ENDIF
         IF(LF95) THEN
            CALL UPDATE_TIME_ON_STATUS_LINE(TIME_STRING)
            CALL RW_PROCESS_MESSAGES()
         ELSE
            CALL LOCATEW(7,0,73)
            CALL SETATTRW(int(7,2),INVERSE)
            CALL PRINTW(7,TIME_STRING)
         ENDIF

      RETURN
! ********************************************************************
      ENTRY DISPLAY_CODE_RUNTIME(CODE_LOCATION)
! ********************************************************************
         CALL TIMER(SYMSTOP)
!          WRITE(3,*) SYMSTOP,'  ',CODE_LOCATION
      RETURN
!
!  ADDING A CLOCK TO THE INDEXING FUNCTION
!
! ********************************************************************
      ENTRY START_INDEX_TIMING
! ********************************************************************
         CALL TIMER(INDEXING_START_TIME)
      RETURN
! ********************************************************************
      ENTRY END_INDEX_TIMING
! ********************************************************************
         CALL TIMER(INDEXING_STOP_TIME)
         IF(INDEXING_STOP_TIME < INDEXING_START_TIME) THEN
            INDEXING_STOP_TIME = INDEXING_STOP_TIME + 8640000 &
                                - INDEXING_START_TIME
         ENDIF
      RETURN
!
! ********************************************************************
      ENTRY RETURN_TOTAL_TIME(TOTIME,EXECUTION_TIME,R_OVERHEAD_TIME, &
                              R_INDEXING_TIME)
! ********************************************************************
         TOTIME = TOTAL_TIME
         R_OVERHEAD_TIME = OVERHEAD_TIME
         EXECUTION_TIME = TOTAL_TIME - OVERHEAD_TIME
         R_INDEXING_TIME = INDEXING_STOP_TIME - INDEXING_START_TIME
      RETURN
      END


! ********************************************************************
      FUNCTION STRING_JUSTIFICATION_LIB()
! ********************************************************************
      INTEGER(kind=2) :: R_I2
      CHARACTER(len=15) :: LEFT_JUSTIFY_I2_IN_STR,&
          STRING_JUSTIFICATION_LIB
      CHARACTER(len=15) :: TEMP_STR
      CHARACTER(len=50) :: LEFT_JUSTIFY
!
         STRING_JUSTIFICATION_LIB = 'TRUE'
      RETURN
!
! ********************************************************************
      ENTRY LEFT_JUSTIFY_I2_IN_STR(R_I2)
! ********************************************************************
         TEMP_STR = ' '
         WRITE(TEMP_STR,*) R_I2
         LEFT_JUSTIFY_I2_IN_STR = LEFT_JUSTIFY(TEMP_STR)
      RETURN
      END
! ********************************************************************
      FUNCTION LEFT_JUSTIFY(STR)
! ********************************************************************
!
      CHARACTER(len=50) :: LEFT_JUSTIFY
      CHARACTER(len=*) :: STR
      INTEGER(kind=2) :: I
!
         IF(trim(STR) == ' ') THEN
            LEFT_JUSTIFY = ' '
         ELSE
            I = 0
            DO
               I = I + 1
               IF(STR(I:I) /= ' ') EXIT
            ENDDO
            LEFT_JUSTIFY = STR(I:)
         ENDIF
      RETURN
      END

