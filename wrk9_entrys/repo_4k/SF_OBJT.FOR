!     ******************************************************************
!     SF_OBJT.FOR
!     Created: 10/24/02 11:45:48 AM
!     Author : msg
!     Last change: gt 5/28/2013 2:40:03 PM
!     ******************************************************************

C***********************************************************************
      SUBROUTINE SF_OBJECT
      use end_routine, only: end_program, er_message
C***********************************************************************
C
      use SpinDriftLib
      use prod_arrays_dimensions
      USE PROD_ARRAYS_DIMENSIONS
      use sizecom
      use mod_base_year
      INTEGER*1 FORECAST_GROWTH_YEARS/AVAIL_DATA_YEARS/
      CHARACTER*1 EXTENSION_PERIOD_GROWTH_SWITCH/'X'/
      INTEGER*2 INUNIT,IREC,DELETE,LRECL/256/,BASE_YEAR
      INTEGER IOS
      CHARACTER*5 SYSFRC,OVERLAY_FAMILY_NAME
      CHARACTER*256 FILE_NAME
      CHARACTER*256 OUTPUT_DIRECTORY
      CHARACTER*256 BASE_FILE_DIRECTORY
      LOGICAL*1 SYSTEM_FILE_EXISTS/.FALSE./,R_SYSTEM_FILE_EXISTS
      CHARACTER*152 MESSAGE
      LOGICAL*4 FILE_EXISTS
      INTEGER*4 UNIT_NUMBER
C DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER*1024 RECLN
C DECLARATION FOR SYSTEM FORECAST VARIABLES
      INTEGER*2 YEAR
      REAL SYS_FC_DATA(4,12),MONTHLY_SYSTEM_PEAKS(12,30),R_PEAK
      REAL MARKET_RATES_CUSTOMERS(4) !3/26/95 MARKET PRICES AT SYSTEM LEVEL MSG
      CHARACTER*50 COMMENT
C     REAL SYS_FC_VALUES(4,12)
      CHARACTER*15 FILE_TYPE/'System Forecast'/
      CHARACTER*2 SYSFRC_OL/'BC'/,R_SYSFRC_OL
C
      LOGICAL*1 LAHEY_LF95
      CHARACTER*30 SCREEN_OUTPUT
      INTEGER*2 SF_YEAR,STUDY_BASE_YEAR,MO,MO_OFFSET,R_MO,R_YR
C
      SAVE STUDY_BASE_YEAR
C
C***********************************************************************
C
C          ROUTINE TO CONVERT METAFILE FILES TO DIRECT-ACESS BINARY
C          COPYRIGHT (C) 1983, 84, 85  M.S. GERBER & ASSOCIATES, INC.
C
C***********************************************************************
C
C CONVERT THE SYSTEM-FORECAST FILE
C
C
C***********************************************************************
      ENTRY SF_MAKEBIN
C***********************************************************************
      STUDY_BASE_YEAR = get_base_year()
      SF_YEAR = 0
      FILE_NAME = trim(BASE_FILE_DIRECTORY())//
     +                                   "SFB"//trim(SYSFRC())//".DAT"
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      IF(EXTENSION_PERIOD_GROWTH_SWITCH == 'F')
     +                          FORECAST_GROWTH_YEARS = AVAIL_DATA_YEARS
      SYSTEM_FILE_EXISTS = FILE_EXISTS
      IF(FILE_EXISTS) THEN
         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//SYSFRC()
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
            CALL MG_LOCATE_WRITE(16,30,SYSFRC(),ALL_VERSIONS,0)
         ENDIF
         SYS_FC_DATA = 0.
         OPEN(10,FILE=FILE_NAME)
         OPEN(11,FILE=trim(OUTPUT_DIRECTORY())//"BCSYSFC.BIN",
     +                      ACCESS="DIRECT",STATUS="REPLACE",RECL=LRECL)
         IREC = 0
         READ(10,*) DELETE
         DOWHILE (IREC < FORECAST_GROWTH_YEARS)
            IREC = IREC + 1
            READ(10,1000,IOSTAT=IOS) RECLN
            IF(IOS == 0) THEN
               SF_YEAR = SF_YEAR + 1
               RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               READ(RECLN,*,ERR=200) DELETE,YEAR,SYS_FC_DATA,COMMENT,
     +                               MARKET_RATES_CUSTOMERS
            ENDIF
c            IF(SF_YEAR /= YEAR - STUDY_BASE_YEAR) THEN
c               WRITE(4,*) 'The Base System Forecast File in'
c               WRITE(4,*) 'Record',SF_YEAR,'  and Year',YEAR
c               WRITE(4,*) 'is inconsistent with the Base Year',
c     +                                                 STUDY_BASE_YEAR
c               WRITE(4,*) 'in Set Parameters. Either reset Base '
c               WRITE(4,*) 'Year in Set Parameters or the Year in'
c               WRITE(4,*) 'System Forecast.'
c               WRITE(4,*) ' '
c            ENDIF
            WRITE(11,REC=IREC) SF_YEAR,SYS_FC_DATA,
     +                                            MARKET_RATES_CUSTOMERS
            CALL CALCULATE_SYSTEM_PEAKS(SYS_FC_DATA,SF_YEAR)
         ENDDO
         CLOSE(10)
c      ELSE IF(INDEX(SYSFRC(),'NONE') == 0) THEN
c         CALL STOP_NOFILE(FILE_TYPE,FILE_NAME)
      ELSE
         OPEN(11,FILE=trim(OUTPUT_DIRECTORY())//"BCSYSFC.BIN",
     +                      ACCESS="DIRECT",STATUS="REPLACE",RECL=LRECL)
      ENDIF
c     endfile(11)
      CLOSE(11)
      RETURN

C***********************************************************************
C
C          ROUTINE TO CREATE OVERLAY FILES
C          COPYRIGHT (C) 1984-88  M.S. GERBER & ASSOCIATES, INC.
C          COPYRIGHT (C) 1991-92  M.S. GERBER & ASSOCIATES, INC.
C
C***********************************************************************
C
C OVERLAY THE SYSTEM-FORECAST FILE
C***********************************************************************
      ENTRY SF_MAKEOVL(OVERLAY_FAMILY_NAME)
C***********************************************************************
C
      IF(LAHEY_LF95()) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSE
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         CALL LOCATE(10,51)
      ENDIF
      FILE_NAME=trim(OUTPUT_DIRECTORY())//"SFO"//
     +                               trim(OVERLAY_FAMILY_NAME)//".DAT"
      OPEN(10,FILE=FILE_NAME)
      READ(10,*) DELETE
      SF_YEAR = 0
      INUNIT = 12
      IF(SYSFRC_OL == 'BC') THEN
         OPEN(11,FILE=trim(OUTPUT_DIRECTORY())//"BCSYSFC.BIN",
     +                                       ACCESS="DIRECT",RECL=LRECL)
         INUNIT = 11
      ENDIF
C     INQUIRE(UNIT=12,OPENED=FILE_EXISTS)
      FILE_NAME = trim(OUTPUT_DIRECTORY())//"OLSYSFC.BIN"
C     INQUIRE(FILE=FILE_NAME,OPENED=FILE_EXISTS,NUMBER=UNIT_NUMBER)
      OPEN(12,FILE=FILE_NAME,ACCESS="DIRECT", 
     +                           STATUS="UNKNOWN",RECL=LRECL,IOSTAT=IOS)
      IF(IOS /= 0) THEN
         CALL IOSTAT_MSG(IOS,MESSAGE)
         WRITE(4,*) trim(MESSAGE)
         er_message='134 in SF_OFJT.FOR see WARNINGS messages'
         call end_program(er_message)
      ENDIF
      IREC = 0
      DELETE = 1
      DO
         IREC = IREC + 1
         READ(INUNIT,REC=IREC,IOSTAT=IOS) YEAR,SYS_FC_DATA,
     +                                    MARKET_RATES_CUSTOMERS
         IF(IOS /= 0) EXIT
         READ(10,1000,IOSTAT=IOS) RECLN
         IF(IOS == 0) THEN
            SF_YEAR = SF_YEAR + 1
            RECLN = trim(RECLN)//',,,,,,,,,,,,,'
            READ(RECLN,*,ERR=200) DELETE,YEAR,SYS_FC_DATA,COMMENT,
     +                                            MARKET_RATES_CUSTOMERS
         ENDIF
         IF(SF_YEAR /= YEAR - STUDY_BASE_YEAR) THEN
            WRITE(4,*) 'The Overlay System Forecast File  ',
     +                                     OVERLAY_FAMILY_NAME,','
            WRITE(4,*) 'Record',SF_YEAR,'  and Year',YEAR
            WRITE(4,*) 'is inconsistent with the Base Year',
     +                                              STUDY_BASE_YEAR
            WRITE(4,*) 'in Set Parameters. Either reset Base '
            WRITE(4,*) 'Year in Set Parameters or the Year in'
            WRITE(4,*) 'System Forecast.'
            WRITE(4,*) ' '
         ENDIF
         WRITE(12,REC=IREC) SF_YEAR,SYS_FC_DATA,MARKET_RATES_CUSTOMERS
         CALL CALCULATE_SYSTEM_PEAKS(SYS_FC_DATA,SF_YEAR)
      ENDDO
      CLOSE(10)
      CLOSE(12)
      IF(SYSFRC_OL == 'BC') CLOSE(11)
      SYSFRC_OL = 'OL'
      RETURN
C
C***********************************************************************
      ENTRY SF_GET_PEAKS
C***********************************************************************
C
      OPEN(10,FILE=trim(OUTPUT_DIRECTORY())//SYSFRC_OL//
     +                           "SYSFC.BIN",ACCESS="DIRECT",RECL=LRECL)
      IREC = 0
      MONTHLY_SYSTEM_PEAKS = 0.0
      DO
         IREC = IREC + 1
         READ(10,REC=IREC,IOSTAT=IOS) YEAR,SYS_FC_DATA
         IF(IOS /= 0) EXIT
         CALL CALCULATE_SYSTEM_PEAKS(SYS_FC_DATA,YEAR)
         DO MO = 1, 12
            MONTHLY_SYSTEM_PEAKS(MO,YEAR) = SYS_FC_DATA(2,MO)
         END DO
      ENDDO
      CLOSE(10)
      RETURN

  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from SF_OBJT SIID267'
      call end_program(er_message)
C
C***********************************************************************
      ENTRY SF_MONTHLY_PEAKS(R_MO,R_YR,R_PEAK)
C***********************************************************************
        R_PEAK = MONTHLY_SYSTEM_PEAKS(R_MO,R_YR)
      RETURN
C
C***********************************************************************
      ENTRY RESET_SYSFRC_OL
C***********************************************************************
         SYSFRC_OL = 'BC'
      RETURN
C
C***********************************************************************
      ENTRY RETURN_SYSFRC_OL(R_SYSFRC_OL)
C***********************************************************************
         R_SYSFRC_OL = SYSFRC_OL
      RETURN
C***********************************************************************
      ENTRY SYSTEM_FORECAST_FILE_EXISTS(R_SYSTEM_FILE_EXISTS)
C***********************************************************************
         R_SYSTEM_FILE_EXISTS = SYSTEM_FILE_EXISTS
      RETURN
C
 1000 FORMAT(A)
 1010 FORMAT('&',A)
      END
C
C***********************************************************************
      FUNCTION STORE_ANNUAL_PEAK_AND_MONTH(R_MO,R_YR,R_PEAK)
C***********************************************************************
C
       use SpinDriftLib
      use prod_arrays_dimensions
      USE PROD_ARRAYS_DIMENSIONS
      use sizecom
      use globecom
      SAVE
      INTEGER*2 YR,R_YR,R_MO
      REAL PEAK,STORE_ANNUAL_PEAK_AND_MONTH,R_PEAK
      REAL PLANNING_PEAK(MAX_PLANNING_YEARS),CAPACITY_PEAKS,
     +      SAVE_LM_RESOURCE_ADJUSTMENT,
     +      SAVE_LM_PLANNED_ADJUSTMENT,
     +      LM_AS_RESOURCE(MAX_PLANNING_YEARS),S_LM_AS_RESOURCE,
     +      LM_AS_PLANNED_CAPACITY(MAX_PLANNING_YEARS),
     +      S_LM_AS_PLANNED_CAPACITY,
     +      GET_INPUT_SYSTEM_PEAK,
     +      LM_RESOURCE_LOAD_CREDIT,LM_PLANNED_LOAD_CREDIT,
     +      PLANNING_PEAK_AFTER_FEEDBACK(MAX_PLANNING_YEARS),
     +      S_INIT_PLANNING_PEAK(3),INIT_PLANNING_PEAK(3),
     +      X1,X2,PEAK_GROWTH_RATE_FOR,RETURN_PEAK,
     +      RETURN_PEAK_ADJUSTMENT,ADJUSTMENT_CAPACITY,
     +      PLANNING_PEAK_GROWTH_RATE
      REAL SAVE_DSM_ADJUSTED_PLANNING_PEAK
      REAL  NET_PLANNING_PEAK,
     +      SAVE_NET_PLANNING_PEAK(MAX_PLANNING_YEARS),
     +      ADJUSTMENT_PEAK,CONTRACT_PEAK_ADJUSTMENT,
     +      PEAK_ADJ_OFF_SYSTEM_SALES,UPDATE_NET_PLANNING_PEAK ,
     +      UPDATE_PEAK_AFTER_FEEDBACK,
     +      CL_PLANNING_LOAD_REDUCTION
      INTEGER*2 PLANNING_PEAK_MONTH_IS(MAX_PLANNING_YEARS)
     +                                            /MAX_PLANNING_YEARS*7/
      INTEGER*2 PEAK_MONTH,UPDATE_PLANNING_PEAK_MONTH
      LOGICAL*1 STORE_INIT_PLANNING_PEAKS
      CHARACTER*8 PEAK_GROWTH_RATE_METHOD
      CHARACTER*1 CHR_PEAK_GROWTH_RATE_METHOD
c     SAVE PLANNING_PEAK,PLANNING_PEAK_MONTH_IS,SAVE_NET_PLANNING_PEAK,
c    +         LM_AS_RESOURCE,LM_AS_PLANNED_CAPACITY,
c    +         PLANNING_PEAK_AFTER_FEEDBACK,INIT_PLANNING_PEAK
         YR = R_YR
         LM_AS_RESOURCE(YR) = 0.
         LM_AS_PLANNED_CAPACITY(YR) = 0.
         PLANNING_PEAK(YR) = R_PEAK
         PLANNING_PEAK_AFTER_FEEDBACK(YR) = R_PEAK
         SAVE_NET_PLANNING_PEAK(YR) = R_PEAK
         PLANNING_PEAK_MONTH_IS(YR) = R_MO
         STORE_ANNUAL_PEAK_AND_MONTH = PLANNING_PEAK(YR)
      RETURN
C      
C***********************************************************************
      ENTRY CAPACITY_PEAKS(R_YR)
C***********************************************************************
         IF(R_YR > STUDY_PERIOD) THEN
            CAPACITY_PEAKS = PLANNING_PEAK_AFTER_FEEDBACK(STUDY_PERIOD)
         ELSE
            CAPACITY_PEAKS = PLANNING_PEAK_AFTER_FEEDBACK(R_YR)
         ENDIF
      RETURN
C
C***********************************************************************
      ENTRY UPDATE_PEAK_AFTER_FEEDBACK(R_YR,R_PEAK)
C***********************************************************************
         PLANNING_PEAK_AFTER_FEEDBACK(R_YR) = R_PEAK
         UPDATE_PEAK_AFTER_FEEDBACK = R_PEAK
      RETURN
C
C***********************************************************************
      ENTRY PEAK_MONTH(R_YR)
C***********************************************************************
         IF(R_YR > STUDY_PERIOD) THEN
            PEAK_MONTH = PLANNING_PEAK_MONTH_IS(STUDY_PERIOD)
         ELSE
            PEAK_MONTH = PLANNING_PEAK_MONTH_IS(R_YR)
         ENDIF
      RETURN
C***********************************************************************
      ENTRY UPDATE_PLANNING_PEAK_MONTH(R_YR,R_MO)
C***********************************************************************
         PLANNING_PEAK_MONTH_IS(R_YR) = R_MO
         UPDATE_PLANNING_PEAK_MONTH = R_MO
      RETURN
C***********************************************************************
      ENTRY SAVE_LM_PLANNED_ADJUSTMENT(R_YR,S_LM_AS_PLANNED_CAPACITY)
C***********************************************************************
         YR = R_YR
         LM_AS_PLANNED_CAPACITY(YR) = LM_AS_PLANNED_CAPACITY(YR) + 
     +                                      S_LM_AS_PLANNED_CAPACITY
         IF(YR > STUDY_PERIOD) THEN
            SAVE_LM_PLANNED_ADJUSTMENT =
     +                        PLANNING_PEAK_AFTER_FEEDBACK(STUDY_PERIOD)
     +                        + LM_AS_PLANNED_CAPACITY(YR)
         ELSE
            SAVE_LM_PLANNED_ADJUSTMENT =
     +                                  PLANNING_PEAK_AFTER_FEEDBACK(YR)
     +                                  + LM_AS_PLANNED_CAPACITY(YR)
         ENDIF
      RETURN
C      
C***********************************************************************
      ENTRY LM_PLANNED_LOAD_CREDIT(R_YR)
C***********************************************************************
         LM_PLANNED_LOAD_CREDIT = LM_AS_PLANNED_CAPACITY(R_YR) 
      RETURN
C      
C***********************************************************************
      ENTRY SAVE_LM_RESOURCE_ADJUSTMENT(R_YR,S_LM_AS_RESOURCE)
C***********************************************************************
         YR = R_YR
         LM_AS_RESOURCE(YR) = LM_AS_RESOURCE(YR) + S_LM_AS_RESOURCE
         IF(YR > STUDY_PERIOD) THEN
            SAVE_LM_PLANNED_ADJUSTMENT =
     +                        PLANNING_PEAK_AFTER_FEEDBACK(STUDY_PERIOD)
         ELSE
            SAVE_LM_PLANNED_ADJUSTMENT =
     +                                  PLANNING_PEAK_AFTER_FEEDBACK(YR)
         ENDIF
         SAVE_LM_RESOURCE_ADJUSTMENT = SAVE_LM_PLANNED_ADJUSTMENT + 
     +            LM_AS_RESOURCE(YR) + LM_AS_PLANNED_CAPACITY(YR)
      RETURN
C      
C***********************************************************************
      ENTRY LM_RESOURCE_LOAD_CREDIT(R_YR)
C***********************************************************************
         LM_RESOURCE_LOAD_CREDIT = LM_AS_RESOURCE(R_YR) 
      RETURN
C
C***********************************************************************
      ENTRY UPDATE_NET_PLANNING_PEAK(R_YR)
C***********************************************************************
         YR = R_YR
         IF(YR > STUDY_PERIOD) THEN
            SAVE_NET_PLANNING_PEAK(YR) =
     +                        PLANNING_PEAK_AFTER_FEEDBACK(STUDY_PERIOD)
         ELSE
            SAVE_NET_PLANNING_PEAK(YR) =
     +                                  PLANNING_PEAK_AFTER_FEEDBACK(YR)
         ENDIF
         SAVE_NET_PLANNING_PEAK(YR) = 
     +                           MAX(1.,SAVE_NET_PLANNING_PEAK(YR) 
     +                           + LM_AS_PLANNED_CAPACITY(YR)
     +                           + LM_AS_RESOURCE(YR)
     +                           + ADJUSTMENT_PEAK(YR)
     +                           - CONTRACT_PEAK_ADJUSTMENT(YR) 
     +                           - CL_PLANNING_LOAD_REDUCTION(YR)
     +                           + PEAK_ADJ_OFF_SYSTEM_SALES(YR))
         UPDATE_NET_PLANNING_PEAK = SAVE_NET_PLANNING_PEAK(YR)
      RETURN
C***********************************************************************
      ENTRY GET_INPUT_SYSTEM_PEAK(R_YR)
C***********************************************************************
         IF(R_YR > STUDY_PERIOD) THEN
            GET_INPUT_SYSTEM_PEAK = PLANNING_PEAK(STUDY_PERIOD)
         ELSE
            GET_INPUT_SYSTEM_PEAK = PLANNING_PEAK(R_YR)
         ENDIF
      RETURN
C***********************************************************************
      ENTRY NET_PLANNING_PEAK(R_YR)
C***********************************************************************
         IF(R_YR > STUDY_PERIOD) THEN
            NET_PLANNING_PEAK = SAVE_NET_PLANNING_PEAK(STUDY_PERIOD)
         ELSE
            NET_PLANNING_PEAK = SAVE_NET_PLANNING_PEAK(R_YR)
         ENDIF
      RETURN
C***********************************************************************
      ENTRY STORE_INIT_PLANNING_PEAKS(S_INIT_PLANNING_PEAK)
C***********************************************************************
         INIT_PLANNING_PEAK(1) = S_INIT_PLANNING_PEAK(1)
         INIT_PLANNING_PEAK(2) = S_INIT_PLANNING_PEAK(2)
         INIT_PLANNING_PEAK(3) = S_INIT_PLANNING_PEAK(3)
         STORE_INIT_PLANNING_PEAKS = .TRUE.
      RETURN
C***********************************************************************
      ENTRY PEAK_GROWTH_RATE_FOR(R_YR,RETURN_PEAK,
     +                                           RETURN_PEAK_ADJUSTMENT)
C***********************************************************************
         IF(R_YR > STUDY_PERIOD) THEN
            YR = STUDY_PERIOD
         ELSE
            YR = R_YR
         ENDIF
         RETURN_PEAK_ADJUSTMENT = ADJUSTMENT_CAPACITY(R_YR)
         IF(YR > 1) THEN
            RETURN_PEAK = SAVE_NET_PLANNING_PEAK(YR-1)
         ELSE
            RETURN_PEAK = INIT_PLANNING_PEAK(1)
         ENDIF
         CHR_PEAK_GROWTH_RATE_METHOD = PEAK_GROWTH_RATE_METHOD()
         IF(CHR_PEAK_GROWTH_RATE_METHOD == 'R') THEN
            PEAK_GROWTH_RATE_FOR = PLANNING_PEAK_GROWTH_RATE(R_YR)
            RETURN
         ENDIF
         IF(YR > 3) THEN
            PEAK_GROWTH_RATE_FOR = SQRT(SAVE_NET_PLANNING_PEAK(YR-1)/
     +                                     SAVE_NET_PLANNING_PEAK(YR-3))
         ELSEIF(YR == 3) THEN
            PEAK_GROWTH_RATE_FOR = SQRT(SAVE_NET_PLANNING_PEAK(YR-1)/
     +                                            INIT_PLANNING_PEAK(1))
         ELSEIF(YR == 2) THEN
            IF(INIT_PLANNING_PEAK(2) <= 15.) THEN
               PEAK_GROWTH_RATE_FOR = 1. + INIT_PLANNING_PEAK(2)/100.
            ELSE
               PEAK_GROWTH_RATE_FOR = SQRT(SAVE_NET_PLANNING_PEAK(YR-1)/
     +                                            INIT_PLANNING_PEAK(2))
            ENDIF
         ELSEIF(YR == 1) THEN
            IF(INIT_PLANNING_PEAK(1) == -9999. .OR.
     +                  INIT_PLANNING_PEAK(2) == -9999. .OR.
     +                             INIT_PLANNING_PEAK(3) == -9999.) THEN
               INIT_PLANNING_PEAK(1) = SAVE_NET_PLANNING_PEAK(1)
               INIT_PLANNING_PEAK(2) = SAVE_NET_PLANNING_PEAK(1)
               INIT_PLANNING_PEAK(3) = SAVE_NET_PLANNING_PEAK(1)
            ENDIF
            IF(INIT_PLANNING_PEAK(3) <= 15.) THEN
               PEAK_GROWTH_RATE_FOR = 1. + INIT_PLANNING_PEAK(3)/100.
            ELSE
               PEAK_GROWTH_RATE_FOR = SQRT(INIT_PLANNING_PEAK(1)/
     +                                         INIT_PLANNING_PEAK(3))
            ENDIF
         ENDIF
      RETURN
C***********************************************************************
      ENTRY SAVE_DSM_ADJUSTED_PLANNING_PEAK(R_YR,R_PEAK)
C***********************************************************************
C         PLANNING_PEAK(R_YR) = R_PEAK
         SAVE_DSM_ADJUSTED_PLANNING_PEAK = R_PEAK
         SAVE_NET_PLANNING_PEAK(R_YR) = R_PEAK
      RETURN   
      END
C
C FUNCTION FOR READING THE SYSTEM LOAD FILE BY YEAR AND PASSING IT TO 
C THE MODULE THAT CALCULATES 8760 LOAD POINTS
C
C***********************************************************************
      SUBROUTINE GET_SYSTEM_LOAD_FORECAST(YEAR,
     +                                    SYSTEM_ENRGY,
     +                                    SYSTEM_PEAKS)
C***********************************************************************
C
       use SpinDriftLib
      use prod_arrays_dimensions
      USE PROD_ARRAYS_DIMENSIONS
      REAL SYSTEM_ENRGY(12,2),SYSTEM_PEAKS(12,2)
      REAL VOID,SET_PEAK_SPLIT,SPLIT_ENERGY,SET_ENERGY_SPLIT,
     +     SPLIT_PEAKS
      INTEGER*2 YEAR,CURRENT_YEAR,MO,
     +          EXTENSION_PERIOD_START
      INTEGER IOS
      CHARACTER*256 OUTPUT_DIRECTORY
      CHARACTER*2 SYSFRC_OL
C
      IF(YEAR == 1) THEN
         CALL RETURN_SYSFRC_OL(SYSFRC_OL)
         OPEN(1222,FILE=trim(OUTPUT_DIRECTORY())//
     +                  SYSFRC_OL//'SYSFC.BIN',ACCESS='DIRECT',RECL=256)
         READ(1222,REC=1,IOSTAT=IOS) CURRENT_YEAR,
     +                            (SYSTEM_ENRGY(1,MO),
     +                             SYSTEM_PEAKS(1,MO),
     +                             SYSTEM_ENRGY(2,MO),
     +                             SYSTEM_PEAKS(2,MO),MO = 1,12)
         DO MO = 1, 12
            IF(SYSTEM_ENRGY(2,MO) == 0.) THEN
               SYSTEM_ENRGY(2,MO) = SPLIT_ENERGY(MO,SYSTEM_ENRGY(1,MO))
            ELSE
               VOID = SET_ENERGY_SPLIT(MO,SYSTEM_ENRGY(1,MO))
            ENDIF
            IF(SYSTEM_PEAKS(2,MO) == 0.) THEN
               SYSTEM_PEAKS(2,MO) = SPLIT_PEAKS(MO,SYSTEM_PEAKS(1,MO))
            ELSE
               VOID = SET_PEAK_SPLIT(MO,SYSTEM_PEAKS(1,MO))
            ENDIF
         ENDDO
         RETURN
      ELSEIF (YEAR >= EXTENSION_PERIOD_START()) THEN
         SYSTEM_ENRGY = 0.
         SYSTEM_PEAKS = 0.
      ELSE
         READ(1222,REC=YEAR,IOSTAT=IOS) CURRENT_YEAR,
     +                            (SYSTEM_ENRGY(1,MO),
     +                             SYSTEM_PEAKS(1,MO),
     +                             SYSTEM_ENRGY(2,MO),
     +                             SYSTEM_PEAKS(2,MO),MO = 1,12)
      ENDIF
      RETURN
      END
C
C ENERGY SPLIT INFORMATION
C
C***********************************************************************
      FUNCTION ENERGY_AND_PEAK_SPLIT()
C***********************************************************************
      REAL ENERGY_AND_PEAK_SPLIT,SPLIT_ENERGY,
     +     SET_ENERGY_SPLIT,SPLIT_PEAKS,SET_PEAK_SPLIT
      INTEGER*2 MO
      REAL VALUE_TO_SPLIT,SET_SPLIT_VALUES(2)
      REAL ENERGY_SPLIT_RATIO(12)/12*0./,PEAK_SPLIT_RATIO(12)/12*0./
C
      ENERGY_AND_PEAK_SPLIT = 1.
      RETURN
C***********************************************************************
      ENTRY SPLIT_ENERGY(MO,VALUE_TO_SPLIT)
C***********************************************************************
         SPLIT_ENERGY = VALUE_TO_SPLIT * ENERGY_SPLIT_RATIO(MO)
      RETURN
C***********************************************************************
      ENTRY SET_ENERGY_SPLIT(MO,SET_SPLIT_VALUES)
C***********************************************************************
         ENERGY_SPLIT_RATIO(MO) = SET_SPLIT_VALUES(2)/
     +                 (SET_SPLIT_VALUES(1) + SET_SPLIT_VALUES(2))         
         SET_ENERGY_SPLIT = ENERGY_SPLIT_RATIO(MO)
      RETURN
C***********************************************************************
      ENTRY SPLIT_PEAKS(MO,VALUE_TO_SPLIT)
C***********************************************************************
         SPLIT_PEAKS = VALUE_TO_SPLIT * PEAK_SPLIT_RATIO(MO)
      RETURN
C***********************************************************************
      ENTRY SET_PEAK_SPLIT(MO,SET_SPLIT_VALUES)
C***********************************************************************
         PEAK_SPLIT_RATIO(MO) = SET_SPLIT_VALUES(2)/SET_SPLIT_VALUES(1)
         SET_PEAK_SPLIT = PEAK_SPLIT_RATIO(MO)
      RETURN
      END

