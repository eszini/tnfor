!     ******************************************************************
!     Mainovl.for
!     Created: 11/11/02 10:25:53 AM
!     Author : msg
!     Last change: GT 9/14/2006 10:14:56 AM
!     ******************************************************************

      SUBROUTINE START_DISPLAY_TIME
       use SpinDriftLib
      use prod_arrays_dimensions
      USE PROD_ARRAYS_DIMENSIONS
      CHARACTER*6 TIME_STRING
      INTEGER*4 SYMSTART,SYMSTOP,TOTIME,TEMP_TIME,R_OVERHEAD_TIME,
     +          EXECUTION_TIME
      INTEGER*4 HOURS,MINUTES,SECONDS,R_INDEXING_TIME
      INTEGER*4 TOTAL_TIME/0/,LAST_TIME/0/,OVERHEAD_TIME/0/,
     +          INDEXING_START_TIME/0/,INDEXING_STOP_TIME/0/
      INTEGER*2 INVERSE,INVERSE_VIDEO
      LOGICAL*1 LF95,LAHEY_LF95
      SAVE SYMSTART,INVERSE,LF95
      CHARACTER*(*) CODE_LOCATION
      CHARACTER*11 CLK
C
         INVERSE = INVERSE_VIDEO()
         CALL TIMER(LAST_TIME)
         SYMSTART = LAST_TIME
         LF95 = LAHEY_LF95()
      RETURN
C
      ENTRY CALULATE_OVERHEAD_TIME
         CALL TIMER(OVERHEAD_TIME)
         IF(OVERHEAD_TIME < SYMSTART) THEN
            OVERHEAD_TIME = 8640000 - SYMSTART + OVERHEAD_TIME
         ELSE
            OVERHEAD_TIME = OVERHEAD_TIME - SYMSTART
         ENDIF
C
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
            CALL SETATTRW(int2(7),INVERSE)
            CALL PRINTW(7,TIME_STRING)
         ENDIF

      RETURN
C***********************************************************************
      ENTRY DISPLAY_CODE_RUNTIME(CODE_LOCATION)
C***********************************************************************
         CALL TIMER(SYMSTOP)
C         WRITE(3,*) SYMSTOP,'  ',CODE_LOCATION
      RETURN
C
C ADDING A CLOCK TO THE INDEXING FUNCTION
C
C***********************************************************************
      ENTRY START_INDEX_TIMING
C***********************************************************************
         CALL TIMER(INDEXING_START_TIME)
      RETURN
C***********************************************************************
      ENTRY END_INDEX_TIMING
C***********************************************************************
         CALL TIMER(INDEXING_STOP_TIME)
         IF(INDEXING_STOP_TIME < INDEXING_START_TIME) THEN
            INDEXING_STOP_TIME = INDEXING_STOP_TIME + 8640000
     +                          - INDEXING_START_TIME
         ENDIF
      RETURN
C
C***********************************************************************
      ENTRY RETURN_TOTAL_TIME(TOTIME,EXECUTION_TIME,R_OVERHEAD_TIME,
     +                        R_INDEXING_TIME)
C***********************************************************************
         TOTIME = TOTAL_TIME
         R_OVERHEAD_TIME = OVERHEAD_TIME
         EXECUTION_TIME = TOTAL_TIME - OVERHEAD_TIME
         R_INDEXING_TIME = INDEXING_STOP_TIME - INDEXING_START_TIME
      RETURN
      END


C***********************************************************************
      FUNCTION STRING_JUSTIFICATION_LIB()
C***********************************************************************
      INTEGER*2 R_I2
      CHARACTER*15 LEFT_JUSTIFY_I2_IN_STR,STRING_JUSTIFICATION_LIB
      CHARACTER*15 TEMP_STR
      CHARACTER*50 LEFT_JUSTIFY
C
         STRING_JUSTIFICATION_LIB = 'TRUE'
      RETURN
C
C***********************************************************************
      ENTRY LEFT_JUSTIFY_I2_IN_STR(R_I2)
C***********************************************************************
         TEMP_STR = ' '
         WRITE(TEMP_STR,*) R_I2
         LEFT_JUSTIFY_I2_IN_STR = LEFT_JUSTIFY(TEMP_STR)
      RETURN
      END
C***********************************************************************
      FUNCTION LEFT_JUSTIFY(STR)
C***********************************************************************
C
      CHARACTER*50 LEFT_JUSTIFY
      CHARACTER*(*) STR
      INTEGER*2 I
C
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

