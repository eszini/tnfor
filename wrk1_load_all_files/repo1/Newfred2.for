C     Last change: msg 9/13/2016 10:04:52 PM
C
C LOAD ANALYSIS CONTROL MODULE 8/11/93 M.S.G. COPYRIGHT (C) 1993
C  ALL RIGHTS RESERVED M.S. GERBER & ASSOCIATES, INC.
C
C***********************************************************************
      RECURSIVE SUBROUTINE LOAD_ANALYSIS_OBJECT
C***********************************************************************
C
      INCLUDE 'SpinLib.MON'
      INCLUDE 'SIZECOM.MON'
      INCLUDE 'GLOBECOM.MON'
C
      SAVE
      INTEGER*2 LAST_REFERENCE_LOADS
      INTEGER*2 HOURLY_LOAD_OUT,HOURLY_LOAD_IN
      CHARACTER*5 BSYRLOAD
      LOGICAL*1 CHANGED_REFERENCE_LOADS
      LOGICAL*1 VOID_LOGICAL,READ_REFERENCE_LOAD_DATA,
     +          SYSTEM_BASED_FORECAST,NERC_REGION_BASED_FORECAST,
     +          CONTROL_AREA_FORECAST
      REAL SYS_GROWTH_TRIGGER,SYS_GROWTH_TRIGGER_LEVEL
      REAL CLASS_GROWTH_TRIGGER,CLASS_GROWTH_TRIGGER_LEVEL
      REAL FYREGY(AVAIL_DATA_YEARS),FYRPK(AVAIL_DATA_YEARS)
      COMMON/FRC2/ FYREGY,FYRPK
C
c      SAVE LAST_REFERENCE_LOADS
C
      IF(NERC_REGION_BASED_FORECAST()) THEN
C
C HOW DO WE SET THE CALENDAR
C
         IF(INDEX(BSYRLOAD(),'NONE') == 0) THEN
            CALL READ_CALENDAR_FROM_REF_FILE()
         ELSE
            CALL USE_CALENDAR_YEAR()
         ENDIF
         RETURN
      ENDIF
      SYS_GROWTH_TRIGGER_LEVEL = SYS_GROWTH_TRIGGER()
      CLASS_GROWTH_TRIGGER_LEVEL = CLASS_GROWTH_TRIGGER()
      IF(HOURLY_LOAD_OUT() > 0) CALL SET_HOURLY_LOAD_OUT
C
      CALL UPDATE_REFERENCE_LOADS
C      
      IF(SYSTEM_BASED_FORECAST()) THEN
         CALL LOAD_FORECAST_AT_SYSTEM_LEVEL(YEAR,
     +                                      FYRPK(YEAR),FYREGY(YEAR),
     +                                      SYS_GROWTH_TRIGGER_LEVEL)
         CALL LODANAL
      ELSEIF(.NOT. NERC_REGION_BASED_FORECAST() .OR. 
     +                                     CONTROL_AREA_FORECAST()) THEN
         CALL LOAD_FORECAST_AT_CLASS_LEVEL(YEAR,
     +                                     FYRPK(YEAR),FYREGY(YEAR),
     +                                     CLASS_GROWTH_TRIGGER_LEVEL)
C        IF(CLASS_BASED_FORECAST() .AND. YEAR == 1) CALL CLS_HIST_SHAPE
         CALL LODANAL
      ENDIF
      RETURN
 1000 FORMAT('&',A,A,A,A)
C 
      ENTRY INIT_LAST_REFERENCE_LOADS
         LAST_REFERENCE_LOADS = 9999
      RETURN
C
      ENTRY UPDATE_REFERENCE_LOADS
         CHANGED_REFERENCE_LOADS = 
     +               LAST_REFERENCE_LOADS /= HOURLY_LOAD_IN()
         IF(CHANGED_REFERENCE_LOADS) THEN
            CALL SET_HOURLY_LOAD_IN
            LAST_REFERENCE_LOADS = HOURLY_LOAD_IN()
            VOID_LOGICAL = READ_REFERENCE_LOAD_DATA()
         ENDIF
      RETURN
      END
C
C***********************************************************************
      SUBROUTINE SET_HOURLY_LOAD_OUT
C***********************************************************************
      INTEGER*2 HOURLY_LOAD_OUT,FILE_EXT
      CHARACTER*5 BSYRLOAD
C     CHARACTER*2 HOURLY_CHAR
      CHARACTER*2 LOAD_FILE_CHAR_EXT
      CHARACTER*256 OUTPUT_DIRECTORY,FILE_NAME
      LOGICAL*4 FILE_EXISTS
      INTEGER*1 F7/z"f7"/
      LOGICAL*1 LAHEY_LF95
C
      FILE_EXT = INT(HOURLY_LOAD_OUT())

      FILE_NAME = trim(OUTPUT_DIRECTORY())//"LDE"//
     +            trim(BSYRLOAD())//".B"//LOAD_FILE_CHAR_EXT(FILE_EXT)

      OPEN(29,FILE=FILE_NAME,ACCESS="DIRECT",
     +                     STATUS="REPLACE",FORM="UNFORMATTED",RECL=118)
C     
      IF(LAHEY_LF95()) THEN
         WRITE(29,REC=1) F7,INT2(118)
      ENDIF
      RETURN
      END
C
C***********************************************************************
      SUBROUTINE SET_HOURLY_LOAD_IN
C***********************************************************************
      INTEGER*2 HOURLY_LOAD_IN,FILE_EXT
C
C     CHARACTER*2 HOURLY_CHAR
      CHARACTER*2 LOAD_FILE_CHAR_EXT
      CHARACTER*5 BSYRLOAD
      CHARACTER*256 LOAD_NAME,INPUT_LOAD_NAME
C
      FILE_EXT = HOURLY_LOAD_IN()
      IF(FILE_EXT == 0) THEN
C
C CHANGED TO REMOVE A CONFLICT WITH VERSION 2.0C
C 9/22/93 MSG
         INPUT_LOAD_NAME = trim(BSYRLOAD())//".BIN"
         LOAD_NAME = trim(BSYRLOAD())//".B00"
      ELSE
         INPUT_LOAD_NAME = trim(BSYRLOAD())//".B"//
     +                                      LOAD_FILE_CHAR_EXT(FILE_EXT)
         LOAD_NAME = INPUT_LOAD_NAME

      ENDIF
C
      CALL CHECK_WHETHER_TO_RUN_FRED(LOAD_NAME,INPUT_LOAD_NAME)
      RETURN
      END
C
C  CHECK FOR THE FREQUENCY AND TYPICAL DAY CORRESPONDING TO THE
C  BASE YEAR LOAD DATA FILE. IF THEY DO NOT EXIST OR THEY ARE YOUNGER
C  THAN THE LOAD DATA FILE CALCULATE THEM
C
C***********************************************************************
      SUBROUTINE CHECK_WHETHER_TO_RUN_FRED(LOAD_NAME,INPUT_LOAD_NAME)
      use end_routine, only: end_program, er_message
C***********************************************************************
C     2/14/93. GAT. ADDED DAY_* FOR KCPL      
      INCLUDE 'SPINLIB.MON'
      INCLUDE 'SIZECOM.MON'
      LOGICAL*4 TYP_EXISTS,FRQ_EXISTS,LDE_EXISTS,DAY_EXISTS
      LOGICAL*1 RUN_FRED,NERC_REGION_BASED_FORECAST
      INTEGER*4 TYP_TIME,TYP_DATE,FRQ_TIME,FRQ_DATE,LDE_TIME,LDE_DATE,
     +          DAY_TIME,DAY_DATE,DAY_LEN
      INTEGER*4 TYP_LEN,FRQ_LEN,LDE_LEN,EXE_DATE
      CHARACTER*256 FILE_NAME,LOAD_NAME,BASE_FILE_DIRECTORY,
     +             OUTPUT_DIRECTORY,INPUT_LOAD_NAME,
     +             REFERENCE_LOAD_FULL_NAME
      LOGICAL*1 LAHEY_LF95
      INTEGER*2 REC_LENGTH,REC_START
C
      IF(NERC_REGION_BASED_FORECAST()) RETURN
      IF(LAHEY_LF95()) THEN
         FILE_NAME = trim(OUTPUT_DIRECTORY())//"TYP95"//LOAD_NAME
         CALL FILE_INQUIRE_95(FILE_NAME,TYP_EXISTS,TYP_DATE,TYP_LEN)
         FILE_NAME = trim(OUTPUT_DIRECTORY())//"FRQ95"//LOAD_NAME
         CALL FILE_INQUIRE_95(FILE_NAME,FRQ_EXISTS,FRQ_DATE,FRQ_LEN)
C        2/14/93. GAT. ADDED FOR HOURLY_AFTER_DSM     
         FILE_NAME = trim(OUTPUT_DIRECTORY())//"DAY95"//LOAD_NAME
         CALL FILE_INQUIRE_95(FILE_NAME,DAY_EXISTS,DAY_DATE,DAY_LEN)
         FILE_NAME = trim(REFERENCE_LOAD_FULL_NAME())//"LDE"//
     +                                                   INPUT_LOAD_NAME
         CALL FILE_INQUIRE_95(FILE_NAME,LDE_EXISTS,LDE_DATE,LDE_LEN)
         RUN_FRED = EXE_DATE() >= TYP_DATE .OR.
     +                        EXE_DATE() >= FRQ_DATE .OR.
     +                                          EXE_DATE() >= DAY_DATE
         RUN_FRED = RUN_FRED .OR. 
     +                 .NOT.(TYP_EXISTS .OR. FRQ_EXISTS .OR. DAY_EXISTS)
         RUN_FRED = RUN_FRED .OR. TYP_LEN < 192 .OR. FRQ_LEN < 20000
         IF(LDE_EXISTS .AND. .NOT. RUN_FRED) THEN
            RUN_FRED = (LDE_DATE > TYP_DATE .OR. LDE_DATE >FRQ_DATE .OR.
     +                                         LDE_DATE > DAY_DATE)
         ENDIF
         CLOSE(25)
         CLOSE(26)
         CLOSE(27)
         FILE_NAME = trim(OUTPUT_DIRECTORY())//"TYP95"//LOAD_NAME
         OPEN(25,FILE=FILE_NAME,STATUS="UNKNOWN",FORM="UNFORMATTED")
         FILE_NAME = trim(OUTPUT_DIRECTORY())//"FRQ95"//LOAD_NAME
         OPEN(26,FILE=FILE_NAME,STATUS="UNKNOWN",FORM="UNFORMATTED")
         FILE_NAME = trim(OUTPUT_DIRECTORY())//"DAY95"//LOAD_NAME
         OPEN(27,FILE=FILE_NAME,STATUS="UNKNOWN",FORM="UNFORMATTED")
      ELSE
         FILE_NAME = trim(OUTPUT_DIRECTORY())//"TYP"//LOAD_NAME
         CALL FILE_INQUIRE(FILE_NAME,TYP_EXISTS,TYP_DATE,TYP_TIME,
     +                                                          TYP_LEN)
         FILE_NAME = trim(OUTPUT_DIRECTORY())//"FRQ"//LOAD_NAME
         CALL FILE_INQUIRE(FILE_NAME,FRQ_EXISTS,FRQ_DATE,FRQ_TIME,
     +                                                          FRQ_LEN)
C        2/14/93. GAT. ADDED FOR HOURLY_AFTER_DSM     
         FILE_NAME = trim(OUTPUT_DIRECTORY())//"DAY"//LOAD_NAME
         CALL FILE_INQUIRE(FILE_NAME,DAY_EXISTS,DAY_DATE,DAY_TIME,
     +                                                          DAY_LEN)
         FILE_NAME = trim(REFERENCE_LOAD_FULL_NAME())//"LDE"//
     +                                                   INPUT_LOAD_NAME
         CALL FILE_INQUIRE(FILE_NAME,LDE_EXISTS,LDE_DATE,LDE_TIME,
     +                                                          LDE_LEN)
         RUN_FRED = EXE_DATE() >= TYP_DATE .OR.
     +                        EXE_DATE() >= FRQ_DATE .OR.
     +                                          EXE_DATE() >= DAY_DATE
         RUN_FRED = RUN_FRED .OR. 
     +                 .NOT.(TYP_EXISTS .OR. FRQ_EXISTS .OR. DAY_EXISTS)
         RUN_FRED = RUN_FRED .OR. TYP_LEN < 192 .OR. FRQ_LEN < 20000
         IF(LDE_EXISTS .AND. .NOT. RUN_FRED) THEN
            RUN_FRED = (LDE_DATE > TYP_DATE .OR. LDE_DATE >FRQ_DATE .OR.
     +                                         LDE_DATE > DAY_DATE) .OR.
     +             (LDE_DATE == TYP_DATE .AND. LDE_TIME > TYP_TIME) .OR.
     +             (LDE_DATE == FRQ_DATE .AND. LDE_TIME > FRQ_TIME) .OR.
     +             (LDE_DATE == DAY_DATE .AND. LDE_TIME > DAY_TIME)
         ENDIF
         CLOSE(25)
         CLOSE(26)
         CLOSE(27)
         FILE_NAME = trim(OUTPUT_DIRECTORY())//"TYP"//LOAD_NAME
         OPEN(25,FILE=FILE_NAME,STATUS="UNKNOWN",FORM="UNFORMATTED")
         FILE_NAME = trim(OUTPUT_DIRECTORY())//"FRQ"//LOAD_NAME
         OPEN(26,FILE=FILE_NAME,STATUS="UNKNOWN",FORM="UNFORMATTED")
         FILE_NAME = trim(OUTPUT_DIRECTORY())//"DAY"//LOAD_NAME
         OPEN(27,FILE=FILE_NAME,STATUS="UNKNOWN",FORM="UNFORMATTED")
      ENDIF
C
      IF(RUN_FRED) THEN
         FILE_NAME = trim(REFERENCE_LOAD_FULL_NAME())//"LDE"//
     +                                           trim(INPUT_LOAD_NAME)
         INQUIRE(FILE=trim(FILE_NAME),EXIST=LDE_EXISTS)
         IF(LDE_EXISTS) THEN
            OPEN(10,FILE=FILE_NAME,ACCESS="TRANSPARENT",STATUS="OLD")
            READ(10,REC=2) REC_LENGTH
            CLOSE(10)
            OPEN(10,FILE=FILE_NAME,ACCESS="DIRECT",
     +                                     STATUS="OLD",RECL=REC_LENGTH)
            IF(LAHEY_LF95()) THEN
               REC_START = 2
            ELSE
               REC_START = 1
            ENDIF
            CALL DISPLAY_TIME
            CALL NEWFRED(REC_START)
            CLOSE(10)
         ELSE

           CALL MG_LOCATE_WRITE(16,9,
     +               'Base year load file '//trim(FILE_NAME)//
     +                                                   ' is Missing!',
     +                                                   ALL_VERSIONS,0)
           er_message='stop requested from Newfred2 SIID249'
           call end_program(er_message)
         ENDIF
      ENDIF
      CALL DISPLAY_TIME
      RETURN
      END
C     **********************************************
      SUBROUTINE NEWFRED(REC_START)
C     **********************************************
C
C     THIS PROGRAM REPLACES MFRED DATED APRIL 29, 1988. THE PURPOSE
C     OF THIS PROGRAM IS TO READ IN EEI FORMATTED HOURLY LOADS AND
C     PARTITION THESE LOADS INTO A FORM USEFUL TO THE MIDAS MODEL.
C     THE PRIMARY CHANGE TO THE MFRED MODEL IS THE ADDITION OF
C     ANOTHER DAY TYPE. THIS NEW DAY TYPE REPRESENTS THE AVERAGE
C     PEAK DAY OF THE MONTH. ALSO, THE NUMBER OF MONTHS IS ASSUMED
C     TO ALWAYS BE TWELVE AND THE PREVIOUS PLOTTING ROUTINE HAS
C     BEEN ELIMINATED.
C
C                                         GREG TURK
C                                         NOVEMBER 7, 1989
C
      INTEGER*2 I,REC,MO,DA,HR,CURRENT_YEAR,TEMPER,DELTMP,
     +          DAYWEK,TIMZON,DAY(12,31),
     +          IHRL(24),REC_START
      INTEGER IOS
C     INTEGER*2 LODA(:,:,:)
      INTEGER*4 LODA(:,:,:),HR_LOAD4(24)
      ALLOCATABLE :: LODA
      CHARACTER*8 EEICODE
      CHARACTER*256 FILE_NAME
      LOGICAL*1 NEW_LDE_FORMAT
      INTEGER*4 REC_LENGHT
C
C*    READ THE HOURLY LOAD DATA FROM LOGICAL UNIT "ILDIN"
      ALLOCATE(LODA(12,31,24))
      LODA = 0D0
      INQUIRE(10,RECL=REC_LENGHT)
      NEW_LDE_FORMAT = REC_LENGHT > 80
      DO REC = REC_START, 366+REC_START-1
         IF(NEW_LDE_FORMAT) THEN
            READ(10,REC=REC,IOSTAT=IOS) MO,DA,CURRENT_YEAR,EEICODE,
     +                                  DAYWEK,TIMZON,
     +                                  TEMPER,DELTMP,HR_LOAD4
         ELSE
            READ(10,REC=REC,IOSTAT=IOS) MO,DA,CURRENT_YEAR,EEICODE,
     +                                  DAYWEK,TIMZON,
     +                                  TEMPER,DELTMP,IHRL
         ENDIF
         IF(IOS /= 0) EXIT
         IF(MO > 0 .AND. MO < 13 .AND. DA > 0 .AND. DA < 32) THEN
            DAY(MO,DA) = DAYWEK
            IF(NEW_LDE_FORMAT) THEN
               DO HR = 1, 24
                  LODA(MO,DA,HR) = HR_LOAD4(HR)
               ENDDO
            ELSE
               DO HR = 1, 24
                  LODA(MO,DA,HR) = IHRL(HR)
               ENDDO
            ENDIF
         ENDIF
      ENDDO
C
C     SEASONS = MONTHS
C
      DO I = 1, 12
         CALL CREATE_DAY_TYPES(LODA,DAY,I,EEICODE,TIMZON,TEMPER,DELTMP)
      ENDDO
      DEALLOCATE(LODA)
      INQUIRE(UNIT=26,NAME=FILE_NAME)
      CLOSE(26)
      OPEN(26,FILE=FILE_NAME,FORM="UNFORMATTED")
      INQUIRE(UNIT=25,NAME=FILE_NAME)
      CLOSE(25)
      OPEN(25,FILE=FILE_NAME,FORM="UNFORMATTED")
      RETURN
      END
C     **************************************************
      SUBROUTINE CREATE_DAY_TYPES(LODA,DAY,MO,
     +                            EEICODE,TIMZON,TEMPER,DELTMP) 
C     **************************************************
C
      INTEGER*2 I,MO,DA,HR,
     +          WEEKEND_PEAK_DAYS,PKCOUNT,
     +          HRSIP(2),PKHRSIP(3),DAYS_PER_MONTH,DAYS_IN,
     +          DAY_TYPE,
     +          DAY(12,31),COUNT(3),
     +          POINTER(24,2),PEAK_HOUR(2),
     +          PEAK_POINTER(4),J,
     +          DAY_OF_MONTH(31,3),
     +          TIMZON,TEMPER,DELTMP
      INTEGER*4 BASE(2),PEAK(2),PKPEAK(3),
     +          HR_LOAD,
     +          HOURLY_LOADS(40,3),
     +          DAILY_PEAK,HIGHEST_4_PEAKS(5),
     +          PKPKD(24,3)
C 2/10/93. TEMP FOR HOURLY AFTER DSM LOADS     
      INTEGER*4 LODA(12,31,24)
      CHARACTER*8 EEICODE
      LOGICAL*1 WRITE_AFTER_DSM_LOADS
      REAL AVE(24,2),PKAVE(24,3),PKD(24,2),PKMWHRS(3),
     +     TOTAL_ENRGY_BY_DAY_TYPE(2)
C
C     INITIALIZE VARIABLES FOR THE THIRD DAY TYPE
C     
      DO I = 1 , 4
         PEAK_POINTER(I) = 0
         HIGHEST_4_PEAKS(I) = 0
      ENDDO
      HIGHEST_4_PEAKS(5) = 0
C
      DO I = 1 , 3
         PKPEAK(I) = 0
         DO HR = 1, 24
            PKAVE(HR,I) = 0
            PKPKD(HR,I) = 0
         ENDDO
      ENDDO
C
C     INITIALIZE REST OF THE VARIABLES
C
C     OUTPUT_REPORT = .FALSE.
      DO I = 1, 2
         BASE(I) = 100000
         PEAK(I) = 0.
         COUNT(I) = 0
         DO HR = 1,24
            PKD(HR,I)=0.
            AVE(HR,I)=0.
         ENDDO
      ENDDO
      COUNT(3) = 0.
C
C     IDENTIFY THE FOUR HIGHEST PEAK DAYS IN THE MONTH
C
      DAYS_PER_MONTH = DAYS_IN(MO)
      DO DA = 1, DAYS_PER_MONTH
C
C THIS HARD-WIRES IN WEEKDAYS AS PEAK DAYS
C
         IF(DAY(MO,DA) > 5) CYCLE
C
         DAILY_PEAK=0
         DO HR = 1, 24

            IF(DAILY_PEAK < LODA(MO,DA,HR)) DAILY_PEAK = LODA(MO,DA,HR)
         ENDDO
         J = 1
         DOWHILE ((DAILY_PEAK > HIGHEST_4_PEAKS(J)) .AND. (J < 5))
            IF(J > 1) THEN
               HIGHEST_4_PEAKS(J-1) = HIGHEST_4_PEAKS(J)
               PEAK_POINTER(J-1) = PEAK_POINTER(J)
            ENDIF
            HIGHEST_4_PEAKS(J) = DAILY_PEAK
            PEAK_POINTER(J) = DA
            J = J + 1
         ENDDO ! SEARCHING FOR PEAK DAYS
      ENDDO ! DAYS_PER_MONTH
C
C     2/10/93. GAT. ADDED FOR KCPL TO PUT CHRONO BACK INTO LOADS
C
      WRITE_AFTER_DSM_LOADS = .TRUE.
      IF(WRITE_AFTER_DSM_LOADS) THEN
         PKHRSIP(1) = 0
         PKHRSIP(2) = 0
         PKHRSIP(3) = 0
         DO DA = 1 , DAYS_PER_MONTH
            IF(DA == PEAK_POINTER(1) .OR. DA == PEAK_POINTER(2) .OR.
     +            DA == PEAK_POINTER(3) .OR. DA == PEAK_POINTER(4)) THEN
               PKHRSIP(3) =  PKHRSIP(3) + 1
               DAY_OF_MONTH(PKHRSIP(3),3) = DA
            ELSEIF(DAY(MO,DA) > 0 .AND. DAY(MO,DA) < 6) THEN
               PKHRSIP(1) =  PKHRSIP(1) + 1
               DAY_OF_MONTH(PKHRSIP(1),1) = DA
            ELSE
               PKHRSIP(2) =  PKHRSIP(2) + 1
               DAY_OF_MONTH(PKHRSIP(2),2) = DA
            ENDIF
         ENDDO
         IF(MO == 1) WRITE(27) EEICODE,TIMZON,TEMPER,DELTMP
         DO DA = 1 , 3
            WRITE(27) PKHRSIP(DA),(DAY_OF_MONTH(I,DA),I=1,PKHRSIP(DA))
         ENDDO
         WRITE(27) (DAY(MO,DA), DA = 1 , DAYS_PER_MONTH)
C 
         IF(MO == 12) REWIND(27)
      ENDIF
C
C     CHECK WHETHER ANY OF THE FOUR PEAK DAYS ARE WEEKEND DAYS
C
      WEEKEND_PEAK_DAYS=0
      DO J = 1 , 4
         IF(DAY(MO,PEAK_POINTER(J)) > 5 .AND. DAY(MO,PEAK_POINTER(J))<9)
     +                         WEEKEND_PEAK_DAYS = WEEKEND_PEAK_DAYS + 1
      ENDDO
C
C     THIS SECTION ORDERS THE LOAD DATA BY DAY AND HOUR INTO THE PEAK
C     AND TOTAL AMOUNT OF GENERATION. ALSO, THE TOTAL NUMBERS OF EACH
C     DAY ARE COUNTED
C
      TOTAL_ENRGY_BY_DAY_TYPE(1) = 0.
      TOTAL_ENRGY_BY_DAY_TYPE(2) = 0.
      DO HR = 1, 24
         DO J=1,2
            HRSIP(J) = 0
         ENDDO
         DO J=1,3
            PKHRSIP(J) = 0
            PKMWHRS(J) = 0.
         ENDDO
         DO DA = 1 , DAYS_PER_MONTH
            DAY_TYPE = 1
            IF(DAY(MO,DA)> 0 .AND. DAY(MO,DA) < 6) THEN
               DAY_TYPE=1
            ELSEIF(DAY(MO,DA) > 5 .AND. DAY(MO,DA) < 9) THEN
               DAY_TYPE=2
            ELSE
               WRITE(4,*) 'Invalid day type in reference load data.'
               WRITE(4,*) 'Valid day types are 1=Mon to 8=holiday.'
               WRITE(4,*) 'For ',MO,'/',DA,' the day type is',
     +                     DAY(MO,DA)
               WRITE(4,*) 'MIDAS Gold assigned this day to weekdays.'
               WRITE(4,*) 'To correct this problem check the'//
     +               ' reference load and reconvert the load data.'
            ENDIF
            IF(HR == 1) THEN
               COUNT(DAY_TYPE) = COUNT(DAY_TYPE) + 1
               IF(DAY(MO,DA) > 7) COUNT(3) = COUNT(3) + 1
            ENDIF
            HR_LOAD = LODA(MO,DA,HR)
            IF(HR_LOAD > 0) THEN
               BASE(DAY_TYPE) = MIN(BASE(DAY_TYPE),HR_LOAD)
            ELSE
               IF(HR < 24) THEN
                  LODA(MO,DA,HR) = LODA(MO,DA,HR+1)
               ELSE
                  LODA(MO,DA,HR) = LODA(MO,DA,HR-1)
               ENDIF
               HR_LOAD = LODA(MO,DA,HR)
            ENDIF
            BASE(DAY_TYPE) = MIN(BASE(DAY_TYPE),HR_LOAD)
            IF(PEAK(DAY_TYPE) <= HR_LOAD) THEN
               PEAK(DAY_TYPE) = HR_LOAD
               PEAK_HOUR(DAY_TYPE) = HR
            ENDIF
C
C           COLLECT STATISTICS FOR THE TWO DAY TYPES
C
            PKD(HR,DAY_TYPE) = MAX(PKD(HR,DAY_TYPE),FLOAT(HR_LOAD))
            AVE(HR,DAY_TYPE) = AVE(HR,DAY_TYPE) + HR_LOAD
            HRSIP(DAY_TYPE) = HRSIP(DAY_TYPE) + 1
C
            DO J = 1 , 4
               IF(DA == PEAK_POINTER(J)) DAY_TYPE=3
            ENDDO
C
C           COLLECT STATISTICS FOR THE THREE DAY TYPES
C
            IF(PKPEAK(DAY_TYPE) <= HR_LOAD) PKPEAK(DAY_TYPE) = HR_LOAD
            PKPKD(HR,DAY_TYPE) = MAX0(PKPKD(HR,DAY_TYPE),HR_LOAD)
            PKMWHRS(DAY_TYPE) = PKMWHRS(DAY_TYPE) + HR_LOAD
            PKAVE(HR,DAY_TYPE) = PKAVE(HR,DAY_TYPE) + HR_LOAD
            PKHRSIP(DAY_TYPE) = PKHRSIP(DAY_TYPE) + 1
            HOURLY_LOADS(PKHRSIP(DAY_TYPE),DAY_TYPE) = HR_LOAD
         ENDDO ! DAY OF THE MONTH
         DO DAY_TYPE = 1 , 3
            IF(DAY_TYPE==1) PKCOUNT = COUNT(DAY_TYPE) - 4 +  
     +                                                 WEEKEND_PEAK_DAYS
            IF(DAY_TYPE==2) PKCOUNT = COUNT(DAY_TYPE) - 
     +                                                 WEEKEND_PEAK_DAYS
            IF(DAY_TYPE==3) PKCOUNT = 4
            IF(PKCOUNT > 0)PKAVE(HR,DAY_TYPE)=PKAVE(HR,DAY_TYPE)/PKCOUNT
C
            WRITE(26) PKHRSIP(DAY_TYPE),PKMWHRS(DAY_TYPE),
     +                 (HOURLY_LOADS(I,DAY_TYPE), I=1,PKHRSIP(DAY_TYPE))
         ENDDO ! DAY_TYPE
         DO DAY_TYPE = 1 , 2
            TOTAL_ENRGY_BY_DAY_TYPE(DAY_TYPE) = AVE(HR,DAY_TYPE) +
     +                                 TOTAL_ENRGY_BY_DAY_TYPE(DAY_TYPE)
            IF(COUNT(DAY_TYPE)/=0) AVE(HR,DAY_TYPE) =
     +                           AVE(HR,DAY_TYPE)/COUNT(DAY_TYPE)
         ENDDO
      ENDDO ! HOUR OF THE DAY
C*
C* WRITE AVERAGE WEEKDAY, AVE WEEKEND W/HOLIDAY AND AVERAGE PEAK DAY
C*               RESULTS TO A SEQUENTIAL BINARY FILE
C*
      CALL SORT_R4ARRAY_FOR_POINTERS(AVE(1,1),POINTER(1,1))
      CALL SORT_R4ARRAY_FOR_POINTERS(AVE(1,2),POINTER(1,2))
C     
C     PASSES PKAVE,WEEKEND_PEAK_DAYS TO THE LAM MODEL
C
      WRITE(25) TOTAL_ENRGY_BY_DAY_TYPE,COUNT,AVE,POINTER,PKD,PEAK,
     +                    PEAK_HOUR,PKAVE,WEEKEND_PEAK_DAYS,PKPEAK,PKPKD
      RETURN
      END
C     *********************************
      SUBROUTINE SORT_R4ARRAY_FOR_POINTERS(IARRAY,ORD)
C     *********************************
      INTEGER*2 ORD(24),I,M,N,ITEMP
      REAL IARRAY(24),RTEMP,ARRAY(24)
      DO I = 1, 24
         ORD(I) = I
         ARRAY(I) = IARRAY(I)
      ENDDO
C
      DO I = 1, 23
         M = I
         N = I+1
   20    CONTINUE
         IF(M > 0) THEN
            IF(ARRAY(M) < ARRAY(N)) THEN
C           INTERCHANGE THE ARRAY POSITIONS
               ITEMP = ORD(M)
               ORD(M) = ORD(N)
               ORD(N) = ITEMP
               RTEMP = ARRAY(M)
               ARRAY(M) = ARRAY(N)
               ARRAY(N) = RTEMP
               N = M
               M = M-1
               GOTO 20
            ENDIF
          ENDIF
      ENDDO
      RETURN
      END 
C     **************************************************
C
      FUNCTION READ_REFERENCE_LOAD_DATA()
C
C     **************************************************
      INCLUDE 'SpinLib.MON'
      INCLUDE 'SIZECOM.MON'
      INCLUDE 'LAMCOM.MON'
      SAVE
      INTEGER*1 MONTH,DAY,HOUR,CLASSES
      INTEGER*2 MO
      REAL ENERGY_SPLIT(12),PEAK_SPLIT(12)
      INTEGER*4 SYSTEM_REFERENCE_ANNUAL_PEAK(2) ! 12/15/94. GAT.
      LOGICAL*1 PEAK_IS_ON_THE_WEEKEND(12)
C
C FUNCTION ITEMS
C
      REAL SPLIT_SYSTEM_ENERGY,TEMP_VALUE,FORECAST_VALUE
      REAL FORECAST_VALUE_BY_DAY(2),STORE_SYSTEM_ENERGY_SPLIT,
     +     STORE_SYSTEM_PEAK_SPLIT,SPLIT_SYSTEM_PEAK
      LOGICAL*1 CHECK_LOAD_FACTOR,LOAD_FACTOR_ADJUSTED,
     +          GET_HISTORICAL_DAYS_PEAK_HR
      INTEGER*2 R_HISTORICAL_DAY_COUNT(2,12),
     +          R_HISTORICAL_PEAK_HOUR(2,12),
     +          GET_HISTORICAL_PEAK_HR_IN
      REAL FORECAST_PEAK(2),ENERGY_FORECAST(2),LOAD_FACTOR,
     +     EXCESS_ENERGY,DEFICIENT_PEAK
C
C CLASS/AREA/POOLING VARIABLES
C
      REAL SPLIT_CLASS_PEAK,SPLIT_CLASS_ENERGY
      REAL STORE_CLASS_ENERGY_SPLIT,
     +     STORE_CLASS_PEAK_SPLIT
c      LOGICAL*1 INITIALIZE_CLASS_SPLITS
      LOGICAL*1 SYSTEM_BASED_FORECAST,VOID_LOGICAL
      INTEGER*2 CLASS
      LOGICAL*1 CLASS_PEAK_IS_ON_THE_WEEKEND(:,:)
      REAL ENERGY_CLASS_SPLIT(:,:),
     +     PEAK_CLASS_SPLIT(:,:)
      ALLOCATABLE :: ENERGY_CLASS_SPLIT,
     +               PEAK_CLASS_SPLIT,
     +               CLASS_PEAK_IS_ON_THE_WEEKEND
C
      LOGICAL*1 SALT_RIVER_PROJECT,NOT_SRP/.TRUE./
      CHARACTER*1 UTILITY_TYPE
C
C
C FUNCTION DEFINITIONS
C
      LOGICAL*1 READ_REFERENCE_LOAD_DATA

C
!
! end of declarations
!

      NOT_SRP = .NOT.(SALT_RIVER_PROJECT() .OR. UTILITY_TYPE() == 'O')
C
      REWIND 25
      DO MONTH = 1, 12
         READ(25) HISTORICAL_ENERGY_IN(1,MONTH),
     +            HISTORICAL_ENERGY_IN(2,MONTH),
     +            HISTORICAL_DAY_COUNT(1,MONTH),
     +            HISTORICAL_DAY_COUNT(2,MONTH),
     +            HOLIDAYS(MONTH),
     +           (SYSTEM_HISTORICAL_LOADS(HOUR,1,MONTH),HOUR=1,24),
     +           (SYSTEM_HISTORICAL_LOADS(HOUR,2,MONTH),HOUR=1,24),
     +           (SYSTEM_LOAD_ORDER(HOUR,1,MONTH),HOUR=1,24),
     +           (SYSTEM_LOAD_ORDER(HOUR,2,MONTH),HOUR=1,24),
     +           (SYSTEM_HISTORICAL_PEAKS(HOUR,1,MONTH),HOUR=1,24),
     +           (SYSTEM_HISTORICAL_PEAKS(HOUR,2,MONTH),HOUR=1,24),
     +           (SYSTEM_REFERENCE_ANNUAL_PEAK(DAY),DAY=1,2),
     +           (HISTORICAL_PEAK_HOUR(DAY,MONTH),DAY=1,2),
     +           (THREE_DAY_LOADS(HOUR,1,MONTH),HOUR=1,24),
     +           (THREE_DAY_LOADS(HOUR,2,MONTH),HOUR=1,24),
     +           (THREE_DAY_LOADS(HOUR,3,MONTH),HOUR=1,24),
     +            WEEKEND_PEAK_DAYS(MONTH),
     +           (PKPEAK(DAY,MONTH),DAY=1,3),
     +           (PKPKD(HOUR,1,MONTH),HOUR = 1,24),
     +           (PKPKD(HOUR,2,MONTH),HOUR = 1,24),
     +           (PKPKD(HOUR,3,MONTH),HOUR = 1,24)
C
         ENERGY_SPLIT(MONTH) = HISTORICAL_ENERGY_IN(2,MONTH)/
     +               (HISTORICAL_ENERGY_IN(1,MONTH) +
     +                                    HISTORICAL_ENERGY_IN(2,MONTH))
         PEAK_SPLIT(MONTH)=FLOAT(MIN(PKPEAK(2,MONTH),PKPEAK(3,MONTH)))/
     +                       FLOAT(MAX(PKPEAK(2,MONTH),PKPEAK(3,MONTH)))
         PEAK_IS_ON_THE_WEEKEND(MONTH) = PKPEAK(2,MONTH)>PKPEAK(3,MONTH)
         
C
         HISTORICAL_ENERGY_IN(1,MONTH) = HISTORICAL_ENERGY_IN(1,MONTH)/
     +                                     HISTORICAL_DAY_COUNT(1,MONTH)
         HISTORICAL_ENERGY_IN(2,MONTH) = HISTORICAL_ENERGY_IN(2,MONTH)/
     +                                     HISTORICAL_DAY_COUNT(2,MONTH)
C
      ENDDO
C LF95 HAS A PROBLEM WITH THIS CALL SINCE IT IS ONLY CALLED HERE I MOVED 
c THE CODE HERE
      IF(.NOT. SYSTEM_BASED_FORECAST()) THEN
         IF(.NOT. ALLOCATED(ENERGY_CLASS_SPLIT)) THEN
            ALLOCATE(ENERGY_CLASS_SPLIT(12,MAX_LOAD_CLASSES),
     +               PEAK_CLASS_SPLIT(12,MAX_LOAD_CLASSES),
     +               CLASS_PEAK_IS_ON_THE_WEEKEND(12,MAX_LOAD_CLASSES))
         ENDIF
         DO CLASSES = 1, MAX_LOAD_CLASSES
            DO MONTH = 1, 12
               CLASS_PEAK_IS_ON_THE_WEEKEND(MONTH,CLASSES) = 
     +                                     PEAK_IS_ON_THE_WEEKEND(MONTH)
               ENERGY_CLASS_SPLIT(MONTH,CLASSES) = ENERGY_SPLIT(MONTH)
               PEAK_CLASS_SPLIT(MONTH,CLASSES) = PEAK_SPLIT(MONTH)
            ENDDO
         ENDDO
      ENDIF
      READ_REFERENCE_LOAD_DATA = .TRUE.
      RETURN
C     **************************************************
      ENTRY SPLIT_SYSTEM_ENERGY(MO,FORECAST_VALUE)
C     **************************************************
         TEMP_VALUE = ENERGY_SPLIT(MO) * FORECAST_VALUE
         FORECAST_VALUE = FORECAST_VALUE - TEMP_VALUE
         SPLIT_SYSTEM_ENERGY = TEMP_VALUE
      RETURN
C     **************************************************
      ENTRY STORE_SYSTEM_ENERGY_SPLIT(MO,FORECAST_VALUE_BY_DAY)
C     **************************************************
         ENERGY_SPLIT(MO) = FORECAST_VALUE_BY_DAY(2)/
     +           (FORECAST_VALUE_BY_DAY(1) + FORECAST_VALUE_BY_DAY(2))
         STORE_SYSTEM_ENERGY_SPLIT = ENERGY_SPLIT(MO)
      RETURN
C     **************************************************
      ENTRY SPLIT_SYSTEM_PEAK(MO,FORECAST_VALUE)
C     **************************************************
         IF(PEAK_IS_ON_THE_WEEKEND(MO)) THEN
            TEMP_VALUE = FORECAST_VALUE
            FORECAST_VALUE = FORECAST_VALUE * PEAK_SPLIT(MO)
            SPLIT_SYSTEM_PEAK = TEMP_VALUE
         ELSE
            SPLIT_SYSTEM_PEAK = FORECAST_VALUE * PEAK_SPLIT(MO)
         ENDIF
      RETURN
C     **************************************************
      ENTRY STORE_SYSTEM_PEAK_SPLIT(MO,FORECAST_VALUE_BY_DAY)
C     **************************************************
         PEAK_SPLIT(MO) = MIN(FORECAST_VALUE_BY_DAY(1),
     +                                       FORECAST_VALUE_BY_DAY(2))/
     +                     MAX(FORECAST_VALUE_BY_DAY(1),
     +                                         FORECAST_VALUE_BY_DAY(2))
         PEAK_IS_ON_THE_WEEKEND(MO) = FORECAST_VALUE_BY_DAY(2) >
     +                                          FORECAST_VALUE_BY_DAY(1)
         STORE_SYSTEM_PEAK_SPLIT = PEAK_SPLIT(MO)
      RETURN

C     **************************************************
      ENTRY SPLIT_CLASS_ENERGY(MO,CLASS,FORECAST_VALUE)
C     **************************************************
         TEMP_VALUE = ENERGY_CLASS_SPLIT(MO,CLASS) * FORECAST_VALUE
         FORECAST_VALUE = FORECAST_VALUE - TEMP_VALUE
         SPLIT_CLASS_ENERGY = TEMP_VALUE
      RETURN
C     **************************************************
      ENTRY STORE_CLASS_ENERGY_SPLIT(MO,CLASS,FORECAST_VALUE_BY_DAY)
C     **************************************************
         ENERGY_CLASS_SPLIT(MO,CLASS) = FORECAST_VALUE_BY_DAY(2)/
     +           (FORECAST_VALUE_BY_DAY(1) + FORECAST_VALUE_BY_DAY(2))
         STORE_CLASS_ENERGY_SPLIT = ENERGY_CLASS_SPLIT(MO,CLASS)
      RETURN
C     **************************************************
      ENTRY SPLIT_CLASS_PEAK(MO,CLASS,FORECAST_VALUE)
C     **************************************************
         IF(CLASS_PEAK_IS_ON_THE_WEEKEND(MO,CLASS)) THEN
            TEMP_VALUE = FORECAST_VALUE
            FORECAST_VALUE = FORECAST_VALUE * PEAK_CLASS_SPLIT(MO,CLASS)
            SPLIT_CLASS_PEAK = TEMP_VALUE
         ELSE
            SPLIT_CLASS_PEAK = FORECAST_VALUE*PEAK_CLASS_SPLIT(MO,CLASS)
         ENDIF
      RETURN
C     **************************************************
      ENTRY STORE_CLASS_PEAK_SPLIT(MO,CLASS,FORECAST_VALUE_BY_DAY)
C     **************************************************
         PEAK_CLASS_SPLIT(MO,CLASS) = MIN(FORECAST_VALUE_BY_DAY(1),
     +                                        FORECAST_VALUE_BY_DAY(2))/
     +                                MAX(FORECAST_VALUE_BY_DAY(1),
     +                                         FORECAST_VALUE_BY_DAY(2))
         CLASS_PEAK_IS_ON_THE_WEEKEND(MO,CLASS) = 
     +               FORECAST_VALUE_BY_DAY(2) > FORECAST_VALUE_BY_DAY(1)
         STORE_CLASS_PEAK_SPLIT = PEAK_CLASS_SPLIT(MO,CLASS)
      RETURN
C     **************************************************
      ENTRY CHECK_LOAD_FACTOR(MO,FORECAST_PEAK,ENERGY_FORECAST)
C     **************************************************
C         
         LOAD_FACTOR_ADJUSTED = .FALSE.
         IF(FORECAST_PEAK(1) == 0.0) RETURN
         LOAD_FACTOR = ENERGY_FORECAST(1)/
     +                  (24*FORECAST_PEAK(1)*HISTORICAL_DAY_COUNT(1,MO))
         IF(LOAD_FACTOR > 1.0 ) THEN
            EXCESS_ENERGY = 1.0001*(ENERGY_FORECAST(1) -
     +                   24*FORECAST_PEAK(1)*HISTORICAL_DAY_COUNT(1,MO))
            ENERGY_FORECAST(1) = ENERGY_FORECAST(1) - EXCESS_ENERGY
            ENERGY_FORECAST(2) = ENERGY_FORECAST(2) + EXCESS_ENERGY
            LOAD_FACTOR_ADJUSTED = .TRUE.
         ENDIF
         IF(NOT_SRP .AND. FORECAST_PEAK(2) /= 0.) THEN
            LOAD_FACTOR = ENERGY_FORECAST(2)/
     +                  (24*FORECAST_PEAK(2)*HISTORICAL_DAY_COUNT(2,MO))
            IF(LOAD_FACTOR > 1.0 ) THEN
               DEFICIENT_PEAK = ENERGY_FORECAST(2)/
     +                (24*HISTORICAL_DAY_COUNT(2,MO)) - FORECAST_PEAK(2)
               FORECAST_PEAK(2) = FORECAST_PEAK(2) + 
     +                                             1.0001*DEFICIENT_PEAK
               LOAD_FACTOR_ADJUSTED = .TRUE.
            ENDIF
         ENDIF ! NOT_SRP
         CHECK_LOAD_FACTOR = LOAD_FACTOR_ADJUSTED
      RETURN
C     **************************************************
      ENTRY GET_HISTORICAL_DAYS_PEAK_HR(R_HISTORICAL_DAY_COUNT,
C     **************************************************
     +                                  R_HISTORICAL_PEAK_HOUR)
         DO MONTH = 1, 12
            R_HISTORICAL_DAY_COUNT(1,MONTH) = 
     +                                     HISTORICAL_DAY_COUNT(1,MONTH)
            R_HISTORICAL_DAY_COUNT(2,MONTH) = 
     +                                     HISTORICAL_DAY_COUNT(2,MONTH)
            R_HISTORICAL_PEAK_HOUR(1,MONTH) =
     +                                     HISTORICAL_PEAK_HOUR(1,MONTH)
            R_HISTORICAL_PEAK_HOUR(2,MONTH) =
     +                                     HISTORICAL_PEAK_HOUR(2,MONTH)
         ENDDO
         GET_HISTORICAL_DAYS_PEAK_HR = .TRUE.
      RETURN
C     **************************************************
      ENTRY GET_HISTORICAL_PEAK_HR_IN(MO)
C     **************************************************
         GET_HISTORICAL_PEAK_HR_IN = HISTORICAL_PEAK_HOUR(1,MO)
      RETURN
      END
C
C
C***********************************************************************
      SUBROUTINE LOAD_FORECAST_AT_SYSTEM_LEVEL(YEAR,FYRPK,FYREGY,
     +                                         SYS_GROWTH_TRIGGER)
C***********************************************************************
C
      INCLUDE 'SpinLib.MON'
      INCLUDE 'SIZECOM.MON'
      INTEGER*2 YEAR,DELETE,MO,MONTH,EXTENSION_PERIOD_START
      INTEGER IOS,UNIT_NO
      LOGICAL*4 LOAD_FILE_OPEN,FILE_EXISTS
      REAL SPLIT_SYSTEM_ENERGY,STORE_SYSTEM_ENERGY_SPLIT,
     +     SPLIT_SYSTEM_PEAK,STORE_SYSTEM_PEAK_SPLIT
      LOGICAL*1   LOAD_FACTOR_ADJUSTED,CHECK_LOAD_FACTOR,
     +            FEEDBACK_IS_ACTIVE,CALCULATE_PRICE_FEEDBACK
      REAL R_VOID,GROWTH_RATE
      REAL SPLIT_DAY_TYPE_ENERGY(2,12),
     +     SPLIT_DAY_TYPE_PEAK(2,12)
      CHARACTER*256 OUTPUT_DIRECTORY,FILE_NAME
      CHARACTER*2 SYSFRC_OL
      REAL SYSTEM_FORECAST_ENERGY(2,12),
     +     SYSTEM_FORECAST_PEAK(2,12),
     +     R_SYSTEM_ENERGY(2,12),
     +     R_SYSTEM_PEAK(2,12),
     +     DUMMY_FORECAST(2,12)
      REAL FYREGY,FYRPK,MO_ENERGY(2),MO_PEAK(2)
      REAL SYSTEM_ANNUAL_SALES,R_SYSTEM_ANNUAL_SALES,SYSTEM_LOSSES
      REAL SYS_GROWTH_TRIGGER
      REAL MARKET_ENERGY_BASE_REVENUES,
     +     MARKET_DEMAND_BASE_REVENUES,
     +     MARKET_CUSTOMER_BASE_REVENUES,
     +     MARKET_TOTAL_BASE_REVENUES,
     +     R_MARKET_TOTAL_BASE_REVENUES, 
     +     R_MARKET_CUSTOMER_REVENUES,
     +     R_MARKET_ENERGY_BASE_REVENUES,
     +     R_MARKET_DEMAND_BASE_REVENUES
      REAL MARKET_CUSTOMERS,
     +     MARKET_ENERGY_RATE,
     +     MARKET_DEMAND_RATE,
     +     MARKET_CUSTOMER_RATE,
     +     R_DSM_ENERGY_CHANGE
      SAVE MARKET_CUSTOMERS,
     +     MARKET_ENERGY_RATE,
     +     MARKET_DEMAND_RATE,
     +     MARKET_CUSTOMER_RATE
C
      SAVE MARKET_ENERGY_BASE_REVENUES,
     +     MARKET_DEMAND_BASE_REVENUES,
     +     MARKET_CUSTOMER_BASE_REVENUES,
     +     MARKET_TOTAL_BASE_REVENUES
      INTEGER*2 ASSET_CLASS,
     +          ASSET_VECTOR
      SAVE ASSET_CLASS,
     +     ASSET_VECTOR
      INTEGER*2 NUMBER_OF_RATE_CLASSES,
     +	       MAX_RATE_CLASS_ID_NUM,
     +          RATE_ASSET_CLASS_POINTER(1024)
      SAVE NUMBER_OF_RATE_CLASSES,
     +	  MAX_RATE_CLASS_ID_NUM,
     +     RATE_ASSET_CLASS_POINTER
C
      REAL ASSET_CLASS_LIST(:)
      REAL ASSET_ALLOCATION_LIST(:),ASSET_ALLOCATOR
      INTEGER*2 ASSET_CLASS_POINTER(:)
      ALLOCATABLE :: ASSET_CLASS_POINTER,
     +               ASSET_CLASS_LIST,
     +               ASSET_ALLOCATION_LIST
      SAVE ASSET_CLASS_POINTER
C
C TEMP UNTIL THESE ARE SET TO FUNCTIONS 8/13/93 MSG
C
      SAVE SPLIT_DAY_TYPE_ENERGY,
     +     SPLIT_DAY_TYPE_PEAK
      SAVE SYSTEM_FORECAST_ENERGY,
     +     SYSTEM_FORECAST_PEAK,
     +     SYSTEM_ANNUAL_SALES
C
      IF(YEAR >= EXTENSION_PERIOD_START()) RETURN !FORECASTS DO NOT CHANGE
      IF(YEAR > AVAIL_DATA_YEARS) RETURN !FORECASTS DO NOT CHANGE
      IF(YEAR == 1) THEN
         CALL RETURN_SYSFRC_OL(SYSFRC_OL)
         FILE_NAME = trim(OUTPUT_DIRECTORY())//SYSFRC_OL//"SYSFC.BIN"
         INQUIRE(FILE=FILE_NAME,NUMBER=UNIT_NO,
     +             OPENED=LOAD_FILE_OPEN,EXIST=FILE_EXISTS)
         IF(LOAD_FILE_OPEN) CLOSE(UNIT_NO,IOSTAT=IOS)
         CLOSE(900,IOSTAT=IOS)
         OPEN(900,FILE=FILE_NAME,ACCESS='DIRECT',RECL=256) ! THIS IS THE SAME AS MAKEBIN SYSTEM FORCASTS
      ENDIF
      RATE_ASSET_CLASS_POINTER = 0
      NUMBER_OF_RATE_CLASSES = 0
      MAX_RATE_CLASS_ID_NUM = 0
C
      READ(900,REC=YEAR) DELETE,
     +                  (SYSTEM_FORECAST_ENERGY(1,MO),
     +                   SYSTEM_FORECAST_PEAK(1,MO),
     +                   SYSTEM_FORECAST_ENERGY(2,MO),
     +                   SYSTEM_FORECAST_PEAK(2,MO),MO = 1,12),
     +                   MARKET_CUSTOMERS,
     +                   MARKET_ENERGY_RATE,
     +                   MARKET_DEMAND_RATE,
     +                   MARKET_CUSTOMER_RATE,
     +                   ASSET_CLASS,
     +                   ASSET_VECTOR
C
      CALL SET_ASSET_CLASSES(ASSET_CLASS,
     +                       NUMBER_OF_RATE_CLASSES,
     +	                    MAX_RATE_CLASS_ID_NUM,
     +                       RATE_ASSET_CLASS_POINTER)
      FYREGY = 0.
      FYRPK  = 0. 
      MARKET_DEMAND_BASE_REVENUES = 0.
      DO MO = 1, 12
         IF(YEAR == 1) THEN
            IF(SYSTEM_FORECAST_ENERGY(2,MO) == 0.) THEN
               SYSTEM_FORECAST_ENERGY(2,MO) = SPLIT_SYSTEM_ENERGY(MO,
     +                                     SYSTEM_FORECAST_ENERGY(1,MO))
            ELSE
               R_VOID = STORE_SYSTEM_ENERGY_SPLIT(MO,
     +                                     SYSTEM_FORECAST_ENERGY(1,MO))
            ENDIF
            SPLIT_DAY_TYPE_ENERGY(1,MO) = SYSTEM_FORECAST_ENERGY(1,MO)
            SPLIT_DAY_TYPE_ENERGY(2,MO) = SYSTEM_FORECAST_ENERGY(2,MO)
C
            IF(SYSTEM_FORECAST_PEAK(2,MO) == 0.) THEN
               SYSTEM_FORECAST_PEAK(2,MO) = SPLIT_SYSTEM_PEAK(MO,
     +                                       SYSTEM_FORECAST_PEAK(1,MO))
            ELSE
               R_VOID = STORE_SYSTEM_PEAK_SPLIT(MO,
     +                                       SYSTEM_FORECAST_PEAK(1,MO))
            ENDIF
         ELSE
            IF(SYSTEM_FORECAST_ENERGY(1,MO) <= SYS_GROWTH_TRIGGER) THEN
               GROWTH_RATE = 1. + SYSTEM_FORECAST_ENERGY(1,MO)/100.
               SPLIT_DAY_TYPE_ENERGY(1,MO) = GROWTH_RATE *
     +                                    SPLIT_DAY_TYPE_ENERGY(1,MO)
            ELSE
               GROWTH_RATE = SYSTEM_FORECAST_ENERGY(1,MO)/
     +                                    SPLIT_DAY_TYPE_ENERGY(1,MO)
               SPLIT_DAY_TYPE_ENERGY(1,MO) = 
     +                                   SYSTEM_FORECAST_ENERGY(1,MO)
            ENDIF
            IF(SYSTEM_FORECAST_ENERGY(2,MO) <= SYS_GROWTH_TRIGGER .AND.
     +                       SYSTEM_FORECAST_ENERGY(2,MO) /= 0.) THEN
               SPLIT_DAY_TYPE_ENERGY(2,MO) = (1. +
     +                           SYSTEM_FORECAST_ENERGY(2,MO)/100.) *
     +                                    SPLIT_DAY_TYPE_ENERGY(2,MO)
            ELSEIF(SYSTEM_FORECAST_ENERGY(2,MO) == 0. .AND.
     +          SYSTEM_FORECAST_ENERGY(1,MO) <= SYS_GROWTH_TRIGGER) THEN
               SPLIT_DAY_TYPE_ENERGY(2,MO) = GROWTH_RATE *
     +                                    SPLIT_DAY_TYPE_ENERGY(2,MO)
            ELSEIF(SYSTEM_FORECAST_ENERGY(2,MO) == 0.) THEN
               SYSTEM_FORECAST_ENERGY(1,MO)=SPLIT_DAY_TYPE_ENERGY(1,MO)
               SYSTEM_FORECAST_ENERGY(2,MO) = SPLIT_SYSTEM_ENERGY(MO,
     +                                     SYSTEM_FORECAST_ENERGY(1,MO))
               SPLIT_DAY_TYPE_ENERGY(1,MO)=SYSTEM_FORECAST_ENERGY(1,MO)
               SPLIT_DAY_TYPE_ENERGY(2,MO)=SYSTEM_FORECAST_ENERGY(2,MO)
            ELSE
               SPLIT_DAY_TYPE_ENERGY(2,MO)=SYSTEM_FORECAST_ENERGY(2,MO)
               R_VOID = STORE_SYSTEM_ENERGY_SPLIT(MO,
     +                                     SYSTEM_FORECAST_ENERGY(1,MO))
            ENDIF
            SYSTEM_FORECAST_ENERGY(1,MO)=SPLIT_DAY_TYPE_ENERGY(1,MO)
            SYSTEM_FORECAST_ENERGY(2,MO)=SPLIT_DAY_TYPE_ENERGY(2,MO)
C
C           THIS SECTION CHECKS ALL (8) UNIQUE CASES OF:
C              (1) PEAK(1) > 20 OR PEAK(1) <= 20
C              (2) PEAK(2) > 20 OR 0 < PEAK(2) <= 20 OR PEAK(2) = 0
C              (3) PEAK(1) > PEAK(2) OR PEAK(1) < PEAK(2)
C
            IF(SYSTEM_FORECAST_PEAK(1,MO) > SYS_GROWTH_TRIGGER) THEN
               IF(SYSTEM_FORECAST_PEAK(2,MO)> SYS_GROWTH_TRIGGER) THEN
C
C NO CALCULATION IS NECESSARY STORE THE NEW SPLIT
C
                  R_VOID = STORE_SYSTEM_PEAK_SPLIT(MO,
     +                                       SYSTEM_FORECAST_PEAK(1,MO))
               ELSEIF(SYSTEM_FORECAST_PEAK(2,MO) <= SYS_GROWTH_TRIGGER
     +                                 .AND.
     +                            SYSTEM_FORECAST_PEAK(2,MO) /= 0.) THEN
C     
                  SYSTEM_FORECAST_PEAK(2,MO) =
     +                          (1. + SYSTEM_FORECAST_PEAK(2,MO)/100.) *
     +                                         SPLIT_DAY_TYPE_PEAK(2,MO)
                  R_VOID = STORE_SYSTEM_PEAK_SPLIT(MO,
     +                                       SYSTEM_FORECAST_PEAK(1,MO))
C     
               ELSE IF(SYSTEM_FORECAST_PEAK(2,MO) == 0.) THEN
                  SYSTEM_FORECAST_PEAK(2,MO) = SPLIT_SYSTEM_PEAK(MO,
     +                                       SYSTEM_FORECAST_PEAK(1,MO))
               ENDIF
            ELSEIF(SYSTEM_FORECAST_PEAK(1,MO)<=SYS_GROWTH_TRIGGER) THEN
               IF(SYSTEM_FORECAST_PEAK(2,MO) > SYS_GROWTH_TRIGGER) THEN
C     
                  SYSTEM_FORECAST_PEAK(1,MO) =
     +                          (1. + SYSTEM_FORECAST_PEAK(1,MO)/100.) *
     +                                         SPLIT_DAY_TYPE_PEAK(1,MO)
                  R_VOID = STORE_SYSTEM_PEAK_SPLIT(MO,
     +                                       SYSTEM_FORECAST_PEAK(1,MO))
C     
               ELSEIF(SYSTEM_FORECAST_PEAK(2,MO) <= SYS_GROWTH_TRIGGER 
     +                        .AND.
     +                            SYSTEM_FORECAST_PEAK(2,MO) /= 0.) THEN
C     
                  SYSTEM_FORECAST_PEAK(1,MO) =
     +                          (1. + SYSTEM_FORECAST_PEAK(1,MO)/100.) *
     +                                         SPLIT_DAY_TYPE_PEAK(1,MO)
C     
                  SYSTEM_FORECAST_PEAK(2,MO) =
     +                          (1. + SYSTEM_FORECAST_PEAK(2,MO)/100.) *
     +                                         SPLIT_DAY_TYPE_PEAK(2,MO)
                  R_VOID = STORE_SYSTEM_PEAK_SPLIT(MO,
     +                                       SYSTEM_FORECAST_PEAK(1,MO))
C     
               ELSE IF(SYSTEM_FORECAST_PEAK(2,MO) == 0.) THEN
                  GROWTH_RATE = (1. + SYSTEM_FORECAST_PEAK(1,MO)/100.)
                  SYSTEM_FORECAST_PEAK(1,MO) = GROWTH_RATE *
     +                                         SPLIT_DAY_TYPE_PEAK(1,MO)
C     
                  SYSTEM_FORECAST_PEAK(2,MO) = GROWTH_RATE *
     +                                         SPLIT_DAY_TYPE_PEAK(2,MO)
               ENDIF
            ENDIF ! SYSTEM_FORECAST_PEAK(1,MO) > SYS_GROWTH_TRIGGER
         ENDIF ! YR = 1
         SPLIT_DAY_TYPE_PEAK(1,MO)=SYSTEM_FORECAST_PEAK(1,MO)
         SPLIT_DAY_TYPE_PEAK(2,MO)=SYSTEM_FORECAST_PEAK(2,MO)
C
         LOAD_FACTOR_ADJUSTED = CHECK_LOAD_FACTOR(MO,
     +                                     SYSTEM_FORECAST_PEAK(1,MO),
     +                                     SYSTEM_FORECAST_ENERGY(1,MO))
         FYREGY = FYREGY + SYSTEM_FORECAST_ENERGY(1,MO) +
     +                                 SYSTEM_FORECAST_ENERGY(2,MO)
         FYRPK = MAX(SYSTEM_FORECAST_PEAK(1,MO),FYRPK,
     +                                       SYSTEM_FORECAST_PEAK(2,MO))
C
C MARKET PRICE INFORMATION 3/26/95
C
         MARKET_DEMAND_BASE_REVENUES = MARKET_DEMAND_BASE_REVENUES +
     +                        (MARKET_DEMAND_RATE * (1.-SYSTEM_LOSSES) *
     +                          MAX(SYSTEM_FORECAST_PEAK(1,MO),
     +                                SYSTEM_FORECAST_PEAK(2,MO)))/1000.
      ENDDO
      CALL RETURN_SYSTEM_LOSSES(YEAR,SYSTEM_LOSSES)
      SYSTEM_ANNUAL_SALES = FYREGY * (1.-SYSTEM_LOSSES)
      FEEDBACK_IS_ACTIVE = CALCULATE_PRICE_FEEDBACK(YEAR,
     +                                    SYSTEM_FORECAST_ENERGY,
     +                                    1.,
     +                                    SYSTEM_FORECAST_PEAK,
     +                                    1.,
     +                                    DUMMY_FORECAST,
     +                                    DUMMY_FORECAST,
     +                                    FYREGY,
     +                                    SYSTEM_CLASS_NUM)
      RETURN
C***********************************************************************
      ENTRY SYSTEM_REVENUES_AFTER_DSM(R_DSM_ENERGY_CHANGE)
C***********************************************************************
C
         MARKET_CUSTOMER_BASE_REVENUES = MARKET_CUSTOMERS * 
     +                                     MARKET_CUSTOMER_RATE/1000000.
         MARKET_ENERGY_BASE_REVENUES = MARKET_ENERGY_RATE * 
     +                (SYSTEM_ANNUAL_SALES-R_DSM_ENERGY_CHANGE)/1000000.
         MARKET_TOTAL_BASE_REVENUES = MARKET_CUSTOMER_BASE_REVENUES +
     +                             MARKET_ENERGY_BASE_REVENUES +
     +                             MARKET_DEMAND_BASE_REVENUES
      RETURN
C***********************************************************************
      ENTRY RETURN_SYSTEM_MARKET_REVENUES(R_MARKET_TOTAL_BASE_REVENUES,
     +                                    R_MARKET_CUSTOMER_REVENUES,
     +                                    R_MARKET_ENERGY_BASE_REVENUES,
     +                                    R_MARKET_DEMAND_BASE_REVENUES)
C***********************************************************************
         R_MARKET_CUSTOMER_REVENUES = MARKET_CUSTOMER_BASE_REVENUES
         R_MARKET_ENERGY_BASE_REVENUES = MARKET_ENERGY_BASE_REVENUES
         R_MARKET_DEMAND_BASE_REVENUES = MARKET_DEMAND_BASE_REVENUES
C***********************************************************************
      ENTRY GET_REVENUES_FORM_SYSTEM_SALES(R_MARKET_TOTAL_BASE_REVENUES)
C***********************************************************************
         R_MARKET_TOTAL_BASE_REVENUES = MARKET_TOTAL_BASE_REVENUES
      RETURN

C***********************************************************************
      ENTRY RETURN_ENERGY_PEAK_SYSTEM(R_SYSTEM_ENERGY,R_SYSTEM_PEAK)
C***********************************************************************
         DO MO = 1, 12
            R_SYSTEM_ENERGY(1,MO) = SYSTEM_FORECAST_ENERGY(1,MO)
            R_SYSTEM_ENERGY(2,MO) = SYSTEM_FORECAST_ENERGY(2,MO)
            R_SYSTEM_PEAK(1,MO) = SYSTEM_FORECAST_PEAK(1,MO)
            R_SYSTEM_PEAK(2,MO) = SYSTEM_FORECAST_PEAK(2,MO)
         ENDDO
      RETURN
C***********************************************************************
      ENTRY GET_SYSTEM_ENERGY_FROM_SYSTEM(MONTH,MO_ENERGY)
C***********************************************************************
         MO_ENERGY(1) = SYSTEM_FORECAST_ENERGY(1,MONTH)
         MO_ENERGY(2) = SYSTEM_FORECAST_ENERGY(2,MONTH)
      RETURN
C***********************************************************************
      ENTRY GET_SYSTEM_PEAK_FROM_SYSTEM(MONTH,MO_PEAK)
C***********************************************************************
         MO_PEAK(1) = SYSTEM_FORECAST_PEAK(1,MONTH)
         MO_PEAK(2) = SYSTEM_FORECAST_PEAK(2,MONTH)
      RETURN
C***********************************************************************
      ENTRY GET_SYSTEM_SALES(R_SYSTEM_ANNUAL_SALES)
C***********************************************************************
         R_SYSTEM_ANNUAL_SALES = SYSTEM_ANNUAL_SALES
      RETURN
      END
C***********************************************************************
      RECURSIVE SUBROUTINE LOAD_FORECAST_AT_CLASS_LEVEL(YEAR,FYRPK,
     +                                      FYREGY,CLASS_GROWTH_TRIGGER)
C***********************************************************************
C
      INCLUDE 'SpinLib.MON'
      INCLUDE 'SIZECOM.MON'
      INCLUDE 'KEPCOCOM.MON'
      SAVE
C
      INTEGER*2 YEAR,IREC,DELETE,I,YR
      INTEGER IOS
      CHARACTER*32 CLASS_NAME(:)
      ALLOCATABLE :: CLASS_NAME
      CHARACTER*2 OVERLAY_TYPE(:)
      CHARACTER*5 CLASS_BIN_NAME(:)
      REAL CLASS_ENERGY_RATE(:),
     +     CLASS_DEMAND_RATE(:),
     +     CLASS_CUSTOMER_RATE(:)
      REAL  MONTHLY_COST_PATTERN(12)
      INTEGER*2  MONTHY_ENRG_COST_PATTERN(:),
     +           MONTHY_DEMAND_COST_PATTERN(:)
      LOGICAL*1 CLASS_EXISTS(:)
      REAL*4 DELTA_CLASS_SALES_FROM_DSM(0:12,*)
      INTEGER*2 ASSET_CLASS_NUM(:),
     +          ASSET_CLASS_VECTOR(:)
      ALLOCATABLE :: OVERLAY_TYPE,
     +               CLASS_BIN_NAME,
     +               CLASS_EXISTS,
     +               CLASS_ENERGY_RATE,
     +               CLASS_DEMAND_RATE,
     +               CLASS_CUSTOMER_RATE,
     +               MONTHY_ENRG_COST_PATTERN,
     +               MONTHY_DEMAND_COST_PATTERN,
     +               ASSET_CLASS_NUM,
     +               ASSET_CLASS_VECTOR
C
      CHARACTER*256 FILE_NAME,OUTPUT_DIRECTORY
      LOGICAL*1 FEEDBACK_IS_ACTIVE,CALCULATE_PRICE_FEEDBACK
      INTEGER*2 CLASS,MONTH,EXTENSION_PERIOD_START
      REAL MO_ENERGY(2),MO_PEAK(2)
      INTEGER*2 PEAK_MO,DAY_TYPE
      INTEGER*2 MO
      REAL R_TOTAL_REVENUES
      REAL SYSTEM_FORECAST_ENERGY(2,12),
     +     SYSTEM_FORECAST_PEAK(2,12),
     +     R_SYSTEM_ENERGY(2,12),
     +     R_SYSTEM_PEAK(2,12),
     +     R_CLASS_SALES(*),
     +     R_CLASS_REVENUES(*)
      REAL R_CLASS_ENERGY_REVENUES(MAX_LOAD_CLASSES),
     +     R_CLASS_DEMAND_REVENUES(MAX_LOAD_CLASSES),
     +     R_CLASS_CUSTOMER_REVENUES(MAX_LOAD_CLASSES)
      INTEGER*2 R_NUM_OF_RATE_REVENUE_CLASSES,
     +          R_MAX_RATE_REVENUE_CLASS_NUM,
     +          R_RATE_CLASS_POINTERS(*)
      REAL AVE_RATE,GET_MONTHLY_VAR_VALUES,MONTHLY_CLASS_ENERGY_RATE(12)
      REAL GROWTH_RATE,MONTH_PEAK,CLASS_SALES
      REAL AREA_FORECAST_ENERGY(:,:,:)
      REAL AREA_FORECAST_PEAK(:,:,:)
      REAL CLASS_COINCIDENT_PEAK(:),
     +     CLASS_ANNUAL_ENERGY(:),
     +     CLASS_ANNUAL_PEAK(:),
     +     CLASS_SUM_OF_PEAKS(:),
     +     CLASS_SUM_OF_CO_PEAKS(:),
     +     CLASS_ENERGY_REVENUES(:),
     +     CLASS_DEMAND_REVENUES(:),
     +     CLASS_CUSTOMER_REVENUES(:)
      ALLOCATABLE :: CLASS_COINCIDENT_PEAK,
     +               CLASS_ANNUAL_ENERGY,
     +               CLASS_ANNUAL_PEAK,
     +               CLASS_SUM_OF_PEAKS,
     +               CLASS_SUM_OF_CO_PEAKS,
     +               CLASS_ENERGY_REVENUES,
     +               CLASS_DEMAND_REVENUES,
     +               CLASS_CUSTOMER_REVENUES,
     +               AREA_FORECAST_ENERGY,
     +               AREA_FORECAST_PEAK
      REAL CLASS_MONTHLY_PEAK,
     +     CLASS_MONTHLY_ENERGY
      REAL CLASS_FORECAST_PEAK(2,12),
     +     CLASS_FORECAST_ENERGY(2,12)
      REAL SPLIT_DAY_TYPE_ENERGY(:,:,:),
     +     SPLIT_DAY_TYPE_PEAK(:,:,:)
      ALLOCATABLE :: SPLIT_DAY_TYPE_ENERGY,
     +     SPLIT_DAY_TYPE_PEAK
      REAL R_CLASS_CUSTOMERS(*)
      REAL CLASS_LOSSES(:),
     +     CLASS_DISTRIBUTION_LOSSES(:),
     +     CLASS_TRANSMISSION_LOSSES(:),
     +     CLASS_COIN_FACTOR(:),
     +     CLASS_PEAK_LOSSES(:),
     +     FORECAST_CUSTOMERS(:)
      ALLOCATABLE :: FORECAST_CUSTOMERS,
     +               CLASS_LOSSES,
     +               CLASS_DISTRIBUTION_LOSSES,
     +               CLASS_TRANSMISSION_LOSSES,
     +               CLASS_COIN_FACTOR,
     +               CLASS_PEAK_LOSSES
      INTEGER*4 VALUES_2_ZERO
      INTEGER*2 NUMBER_OF_RATE_CLASSES,
     +	       MAX_RATE_CLASS_ID_NUM,
     +          RATE_ASSET_CLASS_POINTER(:)
      ALLOCATABLE :: RATE_ASSET_CLASS_POINTER
C
      REAL ASSET_CLASS_LIST(:)
      REAL ASSET_ALLOCATION_LIST(:),ASSET_ALLOCATOR
      INTEGER*2 ASSET_CLASS_POINTER(:)
      ALLOCATABLE :: ASSET_CLASS_POINTER,
     +               ASSET_CLASS_LIST,
     +               ASSET_ALLOCATION_LIST
      INTEGER*2 R_CLASS,R_ASSET_CLASS_NUM(*),ASSET_CLASS,
     +          CLASS_POINTER
      INTEGER*2 ASSET_ALLOCATION_VECTOR,R_ASSET_CLASS_VECTOR(*)
      CHARACTER*1 DUMMY_TYPE
      LOGICAL*1 R_CLASS_EXISTS
      REAL R_RATE_CLASS_ENRG_REVENUES,
     +     R_RATE_CLASS_DEMAND_REVENUES,
     +     R_RATE_CLASS_CUSTOMER_REVENUES,
     +     R_RATE_CLASS_CUSTOMERS,
     +     R_RATE_CLASS_DEMAND,
     +     R_RATE_CLASS_ENERGY
      REAL RATE_CLASS_ENRG_REVENUES(:),
     +     RATE_CLASS_DEMAND_REVENUES(:),
     +     RATE_CLASS_CUSTOMER_REVENUES(:),
     +     RATE_CLASS_ENERGY(:),
     +     RATE_CLASS_DEMAND(:),
     +     RATE_CLASS_CUSTOMERS(:)
      ALLOCATABLE :: RATE_CLASS_ENRG_REVENUES,
     +               RATE_CLASS_DEMAND_REVENUES,
     +               RATE_CLASS_CUSTOMER_REVENUES,
     +               RATE_CLASS_ENERGY,
     +               RATE_CLASS_DEMAND,
     +               RATE_CLASS_CUSTOMERS
C
      INTEGER*2 CLASSES_IN_AREA(:)
      LOGICAL*1 WHICH_CLASSES_IN_AREA(:,:)
      REAL CLASS_RESERVE_MARGIN(:)
      ALLOCATABLE :: CLASS_RESERVE_MARGIN,
     +               CLASSES_IN_AREA,
     +               WHICH_CLASSES_IN_AREA
C
      REAL R_VOID
      LOGICAL*1 LOAD_FACTOR_ADJUSTED,LOGICAL_FALSE
      INTEGER*2 TEMP_POINTER
C
C FUNCTIONS
C
      REAL SPLIT_CLASS_ENERGY,SPLIT_CLASS_PEAK,
     +     STORE_CLASS_ENERGY_SPLIT,STORE_CLASS_PEAK_SPLIT
      LOGICAL*1 CHECK_LOAD_FACTOR,CLASS_BASED_FORECAST,
     +          POOLING_TRANSACTIONS,CONTROL_AREA_FORECAST
C
      REAL CLASS_GROWTH_TRIGGER
      REAL FYREGY,FYRPK
C
      INTEGER*2 PLANNING_MONTH,UPDATE_PLANNING_PEAK_MONTH
      REAL*4 PLANNING_PEAK,UPDATE_PEAK_AFTER_FEEDBACK,
     +         UPDATE_NET_PLANNING_PEAK,R_CLASS_LOSSES
C
      LOGICAL*1 R_CLASS_IN_AREA(*)
      INTEGER*2 R_AREA
      CHARACTER*(*) R_AREA_NAME(*)
      INTEGER*2 LENGHT
      LOGICAL*1 SPLIT_ENERGY_NOT_FOUND(12,MAX_LOAD_CLASSES)
C
C SYSTEM FORECAST IS SUM OF CLASS DATA FILES
C
      IF(YEAR >= EXTENSION_PERIOD_START())RETURN !FORECASTS DON'T CHANGE
      LOGICAL_FALSE = .FALSE.
C
C CLASS VARIABLE DECLARATIONS
C
      IF(ALLOCATED(CLASS_COINCIDENT_PEAK))
     +   DEALLOCATE(CLASS_COINCIDENT_PEAK,
     +              CLASS_ANNUAL_ENERGY,
     +              CLASS_ANNUAL_PEAK,
     +              CLASS_SUM_OF_PEAKS,
     +              CLASS_SUM_OF_CO_PEAKS,
     +              CLASS_ENERGY_REVENUES,
     +              CLASS_DEMAND_REVENUES,
     +              CLASS_CUSTOMER_REVENUES,
     +              AREA_FORECAST_ENERGY,
     +              AREA_FORECAST_PEAK,
     +              FORECAST_CUSTOMERS,
     +              CLASS_NAME,
     +              CLASS_LOSSES,
     +              CLASS_DISTRIBUTION_LOSSES,
     +              CLASS_TRANSMISSION_LOSSES,
     +              CLASS_COIN_FACTOR,
     +              CLASS_PEAK_LOSSES,
     +              CLASSES_IN_AREA,
     +              WHICH_CLASSES_IN_AREA,
     +              RATE_ASSET_CLASS_POINTER,
     +              CLASS_ENERGY_RATE,
     +              CLASS_DEMAND_RATE,
     +              CLASS_CUSTOMER_RATE,
     +              MONTHY_ENRG_COST_PATTERN,
     +              MONTHY_DEMAND_COST_PATTERN,
     +              ASSET_CLASS_NUM,
     +              ASSET_CLASS_VECTOR,
     +              CLASS_EXISTS,
     +              CLASS_RESERVE_MARGIN)
      ALLOCATE(CLASS_COINCIDENT_PEAK(MAX_LOAD_CLASSES),
     +         CLASS_ANNUAL_ENERGY(MAX_LOAD_CLASSES),
     +         CLASS_ANNUAL_PEAK(MAX_LOAD_CLASSES),
     +         CLASS_SUM_OF_PEAKS(MAX_LOAD_CLASSES),
     +         CLASS_SUM_OF_CO_PEAKS(MAX_LOAD_CLASSES),
     +         CLASS_ENERGY_REVENUES(MAX_LOAD_CLASSES),
     +         CLASS_DEMAND_REVENUES(MAX_LOAD_CLASSES),
     +         CLASS_CUSTOMER_REVENUES(MAX_LOAD_CLASSES),
     +         AREA_FORECAST_ENERGY(2,12,MAX_LOAD_CLASSES),
     +         AREA_FORECAST_PEAK(2,12,MAX_LOAD_CLASSES),
     +         CLASS_LOSSES(MAX_LOAD_CLASSES),
     +         CLASS_NAME(MAX_LOAD_CLASSES),
     +         CLASS_DISTRIBUTION_LOSSES(MAX_LOAD_CLASSES),
     +         CLASS_TRANSMISSION_LOSSES(MAX_LOAD_CLASSES),
     +         CLASS_COIN_FACTOR(MAX_LOAD_CLASSES),
     +         CLASS_PEAK_LOSSES(MAX_LOAD_CLASSES),
     +         FORECAST_CUSTOMERS(MAX_LOAD_CLASSES),
     +         CLASSES_IN_AREA(0:MAX_LOAD_CLASSES),
     +         WHICH_CLASSES_IN_AREA(0:MAX_LOAD_CLASSES,
     +                                                MAX_LOAD_CLASSES),
     +         RATE_ASSET_CLASS_POINTER(1024),
     +         CLASS_ENERGY_RATE(MAX_LOAD_CLASSES),
     +         CLASS_DEMAND_RATE(MAX_LOAD_CLASSES),
     +         CLASS_CUSTOMER_RATE(MAX_LOAD_CLASSES),
     +         MONTHY_ENRG_COST_PATTERN(MAX_LOAD_CLASSES),
     +         MONTHY_DEMAND_COST_PATTERN(MAX_LOAD_CLASSES),
     +         ASSET_CLASS_NUM(MAX_LOAD_CLASSES),
     +         ASSET_CLASS_VECTOR(MAX_LOAD_CLASSES),
     +         CLASS_EXISTS(MAX_LOAD_CLASSES),
     +         CLASS_RESERVE_MARGIN(MAX_LOAD_CLASSES))
         CLASS_ENERGY_RATE = 0.
         CLASS_DEMAND_RATE = 0.
         CLASS_CUSTOMER_RATE = 0.
         FORECAST_CUSTOMERS = 0.
         CLASS_LOSSES = 0.
         CLASS_COIN_FACTOR = 0.
         CLASS_RESERVE_MARGIN = 0.
         CLASS_PEAK_LOSSES = 0.
C
C LOCAL ALLOCATION
C
         RATE_ASSET_CLASS_POINTER = 0
         IF(.NOT. ALLOCATED(SPLIT_DAY_TYPE_ENERGY)) THEN
            ALLOCATE(SPLIT_DAY_TYPE_ENERGY(2,12,MAX_LOAD_CLASSES),
     +               SPLIT_DAY_TYPE_PEAK(2,12,MAX_LOAD_CLASSES))
         ENDIF
         ALLOCATE(OVERLAY_TYPE(MAX_LOAD_CLASSES),
     +            CLASS_BIN_NAME(MAX_LOAD_CLASSES))
         CALL GET_CLASS_EXISTS(CLASS_EXISTS)
         CALL GET_CLASS_OVLS(OVERLAY_TYPE)
         CLASS_BIN_NAME(1) = 'COMFC'
         CLASS_BIN_NAME(2) = 'RESFC'
         CLASS_BIN_NAME(3) = 'INDFC'
         CLASS_BIN_NAME(4) = 'OTH1F'
         CLASS_BIN_NAME(5) = 'OTH2F'
         CLASS_BIN_NAME(6) = 'OTH3F'
         IF(YEAR == 1) THEN
            SPLIT_ENERGY_NOT_FOUND= .TRUE.
         ENDIF
         SYSTEM_FORECAST_ENERGY = 0.
         SYSTEM_FORECAST_PEAK = 0.
         AREA_FORECAST_PEAK = 0.
         AREA_FORECAST_ENERGY = 0.
         CLASSES_IN_AREA = 0.
         WHICH_CLASSES_IN_AREA = LOGICAL_FALSE
C
         NUMBER_OF_RATE_CLASSES = 0
         MAX_RATE_CLASS_ID_NUM = 0
C
         IREC = YEAR
         DO CLASS = 1, MAX_LOAD_CLASSES
            CLASS_ANNUAL_ENERGY(CLASS) = 0.
            CLASS_ANNUAL_PEAK(CLASS) = 0.
            CLASS_SUM_OF_PEAKS(CLASS) = 0.
            CLASS_SUM_OF_CO_PEAKS(CLASS) = 0.
            CLASS_ENERGY_REVENUES(CLASS) = 0.
            CLASS_DEMAND_REVENUES(CLASS) = 0.
            CLASS_CUSTOMER_REVENUES(CLASS) = 0.
            ASSET_CLASS_NUM(CLASS) = 0
            ASSET_CLASS_VECTOR(CLASS) = 0
            IF(.NOT. CLASS_EXISTS(CLASS)) CYCLE
            IF(YEAR == 1) THEN
               CLOSE(900+CLASS,IOSTAT=IOS)
               FILE_NAME = trim(OUTPUT_DIRECTORY())//
     +                OVERLAY_TYPE(CLASS)//CLASS_BIN_NAME(CLASS)//'.BIN'
               OPEN(900+CLASS,FILE=FILE_NAME,ACCESS='DIRECT',RECL=280) ! SAME REC LENGHT AT CLASS FORCAST MAKEBIN
            ENDIF
            READ(900+CLASS,REC=IREC,IOSTAT=IOS) DELETE,YR,I,
     +                        CLASS_NAME(CLASS),
     +                        FORECAST_CUSTOMERS(CLASS),
     +                        CLASS_DISTRIBUTION_LOSSES(CLASS),
     +                       (CLASS_FORECAST_ENERGY(1,MO),
     +                        CLASS_FORECAST_PEAK(1,MO),
     +                        CLASS_FORECAST_ENERGY(2,MO),
     +                        CLASS_FORECAST_PEAK(2,MO),MO = 1, 12),
     +                        CLASS_COIN_FACTOR(CLASS),
     +                        CLASS_PEAK_LOSSES(CLASS),
     +                        CLASS_CONTROL_AREA(CLASS),
     +                        CLASS_RESERVE_MARGIN(CLASS),
     +                        CLASS_ENERGY_RATE(CLASS),
     +                        CLASS_DEMAND_RATE(CLASS),
     +                        CLASS_CUSTOMER_RATE(CLASS),
     +                        ASSET_CLASS_NUM(CLASS),
     +                        ASSET_CLASS_VECTOR(CLASS),
     +                        CLASS_TRANSMISSION_LOSSES(CLASS),
     +                        MONTHY_ENRG_COST_PATTERN(CLASS),
     +                        MONTHY_DEMAND_COST_PATTERN(CLASS)
           IF(IOS /= 0) THEN
               INQUIRE(900+CLASS,NAME=FILE_NAME)
               CALL CRITICAL_ERROR_FORECAST_FILE(FILE_NAME,IOS)
            ENDIF
            IF(DELETE > 7) CYCLE
            CLASS_CONTROL_AREA(CLASS) = MAX(1,CLASS_CONTROL_AREA(CLASS))
            TEMP_POINTER = CLASS_CONTROL_AREA(CLASS)
            CLASSES_IN_AREA(TEMP_POINTER) = 1 +
     +                                     CLASSES_IN_AREA(TEMP_POINTER)
            WHICH_CLASSES_IN_AREA(TEMP_POINTER,CLASS) = .TRUE.
! OUT 7/31/98. GAT. PER STEININGER. SEE REAENRG.FOR

            CLASS_COIN_FACTOR(CLASS) = CLASS_COIN_FACTOR(CLASS)/100.
            CLASS_LOSSES(CLASS)= CLASS_TRANSMISSION_LOSSES(CLASS) +
     +                           CLASS_DISTRIBUTION_LOSSES(CLASS) -
     +                            CLASS_TRANSMISSION_LOSSES(CLASS) * 
     +                                  CLASS_DISTRIBUTION_LOSSES(CLASS)
C
C
C
            CALL SET_ASSET_CLASSES(ASSET_CLASS_NUM(CLASS),
     +                             NUMBER_OF_RATE_CLASSES,
     +	                          MAX_RATE_CLASS_ID_NUM,
     +                             RATE_ASSET_CLASS_POINTER)
            DO MO = 1, 12
               IF(YEAR == 1 .OR. SPLIT_ENERGY_NOT_FOUND(MO,CLASS)) THEN
                  IF(CLASS_FORECAST_ENERGY(2,MO) == 0.) THEN
                     CLASS_FORECAST_ENERGY(2,MO) =
     +                     SPLIT_CLASS_ENERGY(MO,CLASS,
     +                                      CLASS_FORECAST_ENERGY(1,MO))
                  ELSE
                     R_VOID = STORE_CLASS_ENERGY_SPLIT(MO,CLASS,
     +                                      CLASS_FORECAST_ENERGY(1,MO))
                  ENDIF
                  SPLIT_DAY_TYPE_ENERGY(1,MO,CLASS) = 
     +                                       CLASS_FORECAST_ENERGY(1,MO)
                  SPLIT_DAY_TYPE_ENERGY(2,MO,CLASS) = 
     +                                       CLASS_FORECAST_ENERGY(2,MO)
C
                  IF(CLASS_FORECAST_PEAK(2,MO) == 0.) THEN
                     CLASS_FORECAST_PEAK(2,MO) = 
     +                                     SPLIT_CLASS_PEAK(MO,CLASS,
     +                                        CLASS_FORECAST_PEAK(1,MO))
                  ELSE
                     R_VOID = STORE_CLASS_PEAK_SPLIT(MO,CLASS,
     +                                       CLASS_FORECAST_PEAK(1,MO))
                  ENDIF
                  SPLIT_DAY_TYPE_PEAK(1,MO,CLASS) = 
     +                                         CLASS_FORECAST_PEAK(1,MO)
                  SPLIT_DAY_TYPE_PEAK(2,MO,CLASS) = 
     +                                         CLASS_FORECAST_PEAK(2,MO)
                  IF(CLASS_FORECAST_ENERGY(1,MO) /= 0. .AND.
     +                  CLASS_FORECAST_PEAK(2,MO) /= 0.)
     +                        SPLIT_ENERGY_NOT_FOUND(MO,CLASS) = .FALSE.
               ELSE
                  IF(CLASS_FORECAST_ENERGY(1,MO) <=
     +                                        CLASS_GROWTH_TRIGGER) THEN
                     GROWTH_RATE = 1. + CLASS_FORECAST_ENERGY(1,MO)/100.
                     SPLIT_DAY_TYPE_ENERGY(1,MO,CLASS) = GROWTH_RATE *
     +                                 SPLIT_DAY_TYPE_ENERGY(1,MO,CLASS)
                  ELSE
                     GROWTH_RATE = CLASS_FORECAST_ENERGY(1,MO)/
     +                                 SPLIT_DAY_TYPE_ENERGY(1,MO,CLASS)
                     SPLIT_DAY_TYPE_ENERGY(1,MO,CLASS) = 
     +                                   CLASS_FORECAST_ENERGY(1,MO)
                  ENDIF
                  IF(CLASS_FORECAST_ENERGY(2,MO) <= CLASS_GROWTH_TRIGGER
     +                     .AND. CLASS_FORECAST_ENERGY(2,MO) /= 0.) THEN
                     SPLIT_DAY_TYPE_ENERGY(2,MO,CLASS) = (1. +
     +                           CLASS_FORECAST_ENERGY(2,MO)/100.) *
     +                                 SPLIT_DAY_TYPE_ENERGY(2,MO,CLASS)
                  ELSEIF(CLASS_FORECAST_ENERGY(2,MO) == 0. .AND.
     +                          CLASS_FORECAST_ENERGY(1,MO) <=
     +                                        CLASS_GROWTH_TRIGGER) THEN
                     SPLIT_DAY_TYPE_ENERGY(2,MO,CLASS) = GROWTH_RATE *
     +                                 SPLIT_DAY_TYPE_ENERGY(2,MO,CLASS)
                  ELSEIF(CLASS_FORECAST_ENERGY(2,MO) == 0.) THEN
                     CLASS_FORECAST_ENERGY(1,MO) = 
     +                              SPLIT_DAY_TYPE_ENERGY(1,MO,CLASS)
                     CLASS_FORECAST_ENERGY(2,MO) = 
     +                                 SPLIT_CLASS_ENERGY(MO,CLASS,
     +                                      CLASS_FORECAST_ENERGY(1,MO))
                     SPLIT_DAY_TYPE_ENERGY(1,MO,CLASS) = 
     +                                       CLASS_FORECAST_ENERGY(1,MO)
                     SPLIT_DAY_TYPE_ENERGY(2,MO,CLASS) = 
     +                                       CLASS_FORECAST_ENERGY(2,MO)
                  ELSE
                     SPLIT_DAY_TYPE_ENERGY(2,MO,CLASS) = 
     +                                       CLASS_FORECAST_ENERGY(2,MO)
                     R_VOID = STORE_CLASS_ENERGY_SPLIT(MO,CLASS,
     +                                     CLASS_FORECAST_ENERGY(1,MO))
                  ENDIF
                  CLASS_FORECAST_ENERGY(1,MO) = 
     +                                 SPLIT_DAY_TYPE_ENERGY(1,MO,CLASS)
                  CLASS_FORECAST_ENERGY(2,MO) = 
     +                                 SPLIT_DAY_TYPE_ENERGY(2,MO,CLASS)
C
C THIS SECTION CHECKS ALL (8) UNIQUE CASES OF:
C (1) PEAK(1) > 20 OR PEAK(1) <= 20
C (2) PEAK(2) > 20 OR 0 < PEAK(2) <= 20 OR PEAK(2) = 0
C (3) PEAK(1) > PEAK(2) OR PEAK(1) < PEAK(2)
C
                  IF(CLASS_FORECAST_PEAK(1,MO) >
     +                                        CLASS_GROWTH_TRIGGER) THEN
                     IF(CLASS_FORECAST_PEAK(2,MO)>
     +                                        CLASS_GROWTH_TRIGGER) THEN
C
C NO CALCULATION IS NECESSARY STORE THE NEW SPLIT
C
                        R_VOID = STORE_CLASS_PEAK_SPLIT(MO,CLASS,
     +                                       CLASS_FORECAST_PEAK(1,MO))
                     ELSEIF(CLASS_FORECAST_PEAK(2,MO) <=
     +                                     CLASS_GROWTH_TRIGGER .AND.
     +                            CLASS_FORECAST_PEAK(2,MO) /= 0.) THEN
C     
                        CLASS_FORECAST_PEAK(2,MO) =
     +                          (1. + CLASS_FORECAST_PEAK(2,MO)/100.) *
     +                                   SPLIT_DAY_TYPE_PEAK(2,MO,CLASS)
                        R_VOID = STORE_CLASS_PEAK_SPLIT(MO,CLASS,
     +                                       CLASS_FORECAST_PEAK(1,MO))
C     
                     ELSEIF(CLASS_FORECAST_PEAK(2,MO) == 0.) THEN
                        CLASS_FORECAST_PEAK(2,MO) = 
     +                                   SPLIT_CLASS_PEAK(MO,CLASS,
     +                                       CLASS_FORECAST_PEAK(1,MO))
                     ENDIF
                  ELSEIF(CLASS_FORECAST_PEAK(1,MO) <=
     +                                        CLASS_GROWTH_TRIGGER) THEN
                     IF(CLASS_FORECAST_PEAK(2,MO) > 
     +                                        CLASS_GROWTH_TRIGGER) THEN
C     
                        CLASS_FORECAST_PEAK(1,MO) =
     +                          (1. + CLASS_FORECAST_PEAK(1,MO)/100.) *
     +                                   SPLIT_DAY_TYPE_PEAK(1,MO,CLASS)
                        R_VOID = STORE_CLASS_PEAK_SPLIT(MO,CLASS,
     +                                       CLASS_FORECAST_PEAK(1,MO))
C     
                     ELSEIF(CLASS_FORECAST_PEAK(2,MO) <=
     +                                     CLASS_GROWTH_TRIGGER .AND.
     +                             CLASS_FORECAST_PEAK(2,MO) /= 0.) THEN
C     
                        CLASS_FORECAST_PEAK(1,MO) =
     +                          (1. + CLASS_FORECAST_PEAK(1,MO)/100.) *
     +                                   SPLIT_DAY_TYPE_PEAK(1,MO,CLASS)
C     
                        CLASS_FORECAST_PEAK(2,MO) =
     +                          (1. + CLASS_FORECAST_PEAK(2,MO)/100.) *
     +                                   SPLIT_DAY_TYPE_PEAK(2,MO,CLASS)
                        R_VOID = STORE_CLASS_PEAK_SPLIT(MO,CLASS,
     +                                       CLASS_FORECAST_PEAK(1,MO))
C     
                     ELSEIF(CLASS_FORECAST_PEAK(2,MO) == 0.) THEN
                        GROWTH_RATE = 1.+CLASS_FORECAST_PEAK(1,MO)/100.
                        CLASS_FORECAST_PEAK(1,MO) = GROWTH_RATE *
     +                                   SPLIT_DAY_TYPE_PEAK(1,MO,CLASS)
C     
                        CLASS_FORECAST_PEAK(2,MO) = GROWTH_RATE *
     +                                   SPLIT_DAY_TYPE_PEAK(2,MO,CLASS)
                     ENDIF
                  ENDIF
                  SPLIT_DAY_TYPE_PEAK(1,MO,CLASS) = 
     +                                         CLASS_FORECAST_PEAK(1,MO)
                  SPLIT_DAY_TYPE_PEAK(2,MO,CLASS) = 
     +                                         CLASS_FORECAST_PEAK(2,MO)
               ENDIF
C
               LOAD_FACTOR_ADJUSTED = CHECK_LOAD_FACTOR(MO,
     +                                     CLASS_FORECAST_PEAK(1,MO),
     +                                     CLASS_FORECAST_ENERGY(1,MO))
               CLASS_ANNUAL_ENERGY(CLASS) = CLASS_ANNUAL_ENERGY(CLASS) + 
     +                                   CLASS_FORECAST_ENERGY(1,MO) +
     +                                   CLASS_FORECAST_ENERGY(2,MO)
               IF(REALLY_KEPCO) THEN
                  CLASS_MONTHLY_ENERGY = CLASS_FORECAST_ENERGY(1,MO)
               ELSE
                  CLASS_MONTHLY_ENERGY = CLASS_FORECAST_ENERGY(1,MO)/
     +                                        (1. - CLASS_LOSSES(CLASS))
               ENDIF
               IF(.NOT. WABASH_VALLEY .OR.
     +                                   CLASS_CONTROL_AREA(CLASS) == 1)
     +             SYSTEM_FORECAST_ENERGY(1,MO) = CLASS_MONTHLY_ENERGY +
     +                                      SYSTEM_FORECAST_ENERGY(1,MO)
               AREA_FORECAST_ENERGY(1,MO,CLASS_CONTROL_AREA(CLASS)) =
     +            AREA_FORECAST_ENERGY(1,MO,CLASS_CONTROL_AREA(CLASS)) +
     +                                              CLASS_MONTHLY_ENERGY
C     
               IF(REALLY_KEPCO) THEN
                  CLASS_MONTHLY_ENERGY = CLASS_FORECAST_ENERGY(2,MO)
               ELSE   
                  CLASS_MONTHLY_ENERGY = CLASS_FORECAST_ENERGY(2,MO)/
     +                                        (1. - CLASS_LOSSES(CLASS))
               ENDIF
               IF(.NOT. WABASH_VALLEY .OR.
     +                              CLASS_CONTROL_AREA(CLASS) == 1)
     +          SYSTEM_FORECAST_ENERGY(2,MO)=CLASS_MONTHLY_ENERGY +
     +                                      SYSTEM_FORECAST_ENERGY(2,MO)
               AREA_FORECAST_ENERGY(2,MO,CLASS_CONTROL_AREA(CLASS)) =
     +            AREA_FORECAST_ENERGY(2,MO,CLASS_CONTROL_AREA(CLASS)) +
     +                                              CLASS_MONTHLY_ENERGY
C     
               CLASS_MONTHLY_PEAK = CLASS_FORECAST_PEAK(1,MO) * 
     +            CLASS_COIN_FACTOR(CLASS)/(1.-CLASS_PEAK_LOSSES(CLASS))
               IF(.NOT. WABASH_VALLEY .OR.
     +                              CLASS_CONTROL_AREA(CLASS) == 1)
     +          SYSTEM_FORECAST_PEAK(1,MO) = SYSTEM_FORECAST_PEAK(1,MO)+
     +                                      CLASS_MONTHLY_PEAK               
               CLASS_MONTHLY_PEAK = CLASS_FORECAST_PEAK(2,MO) * 
     +            CLASS_COIN_FACTOR(CLASS)/(1.-CLASS_PEAK_LOSSES(CLASS))
               IF(.NOT. WABASH_VALLEY .OR.
     +                              CLASS_CONTROL_AREA(CLASS) == 1)
     +          SYSTEM_FORECAST_PEAK(2,MO) = SYSTEM_FORECAST_PEAK(2,MO)+
     +                                      CLASS_MONTHLY_PEAK
C
               CLASS_MONTHLY_PEAK = MAX(CLASS_FORECAST_PEAK(1,MO),
     +                                  CLASS_FORECAST_PEAK(2,MO))
               CLASS_ANNUAL_PEAK(CLASS) = MAX(CLASS_ANNUAL_PEAK(CLASS),
     +                                        CLASS_MONTHLY_PEAK)
               CLASS_SUM_OF_PEAKS(CLASS) = CLASS_SUM_OF_PEAKS(CLASS) +
     +                                     CLASS_MONTHLY_PEAK
C     
            ENDDO !MONTH LOOP
            CLASS_SUM_OF_CO_PEAKS(CLASS) = CLASS_COIN_FACTOR(CLASS) *               
     +                                     CLASS_SUM_OF_PEAKS(CLASS)
            FEEDBACK_IS_ACTIVE = CALCULATE_PRICE_FEEDBACK(YEAR,
     +                                 CLASS_FORECAST_ENERGY,
     +                                    1./(1. - CLASS_LOSSES(CLASS)),
     +                                 CLASS_FORECAST_PEAK,
     +                                 CLASS_COIN_FACTOR(CLASS)/
     +                                    (1.-CLASS_PEAK_LOSSES(CLASS)),
     +                                 SYSTEM_FORECAST_ENERGY,
     +                                 SYSTEM_FORECAST_PEAK,
     +                                 CLASS_ANNUAL_ENERGY(CLASS),
     +                                 CLASS)
            IF(CLASS_BASED_FORECAST() .OR. POOLING_TRANSACTIONS() .OR.
     +                                     CONTROL_AREA_FORECAST()) THEN
               CALL STORE_MONTHLY_CLASS_FORECASTS(CLASS,
     +                        CLASS_FORECAST_ENERGY,
     +                        CLASS_FORECAST_PEAK,
     +                        CLASS_PEAK_LOSSES)
            ENDIF
         ENDDO  !CLASS LOOP
C
         CALL STORE_CUSTOMERS_INTO_LAMCOM(FORECAST_CUSTOMERS,
     +                                    CLASS_COIN_FACTOR,
     +                                    CLASS_LOSSES,     
     +                                    CLASS_RESERVE_MARGIN,
     +                                    CLASS_PEAK_LOSSES)
C
         WABASH_IM_NIPSCO_PSI = WHICH_CLASSES_IN_AREA(1,1) .AND.
     +                          WHICH_CLASSES_IN_AREA(1,2) .AND.
     +                          WHICH_CLASSES_IN_AREA(1,3) .AND.
     +                          WABASH_VALLEY
         WABASH_IM_NIPSCO = .NOT. WABASH_IM_NIPSCO_PSI .AND.
     +                                                     WABASH_VALLEY
         FYRPK  = 0. 
         DO MO = 1, 12
            IF(FYRPK <= SYSTEM_FORECAST_PEAK(1,MO)) THEN
               PEAK_MO = MO
               DAY_TYPE = 1
               FYRPK = SYSTEM_FORECAST_PEAK(1,MO)
            ENDIF
            IF(FYRPK <= SYSTEM_FORECAST_PEAK(2,MO)) THEN
               PEAK_MO = MO
               DAY_TYPE = 2
               FYRPK = SYSTEM_FORECAST_PEAK(2,MO)
            ENDIF
         ENDDO
         IF(FEEDBACK_IS_ACTIVE .AND. YEAR > 1) THEN
            PLANNING_MONTH = UPDATE_PLANNING_PEAK_MONTH(YEAR,PEAK_MO)
            PLANNING_PEAK = UPDATE_PEAK_AFTER_FEEDBACK(YEAR,FYRPK)
            PLANNING_PEAK = UPDATE_NET_PLANNING_PEAK(YEAR)
         ENDIF
         FYREGY = 0.
C
C SET UP ASSET BASED ARRAYS
C
         DO CLASS = 1, MAX_LOAD_CLASSES
            IF(.NOT. CLASS_EXISTS(CLASS)) THEN
               CLASS_COINCIDENT_PEAK(CLASS) = 0.
               CLASS_ENERGY_REVENUES(CLASS) = 0.
               CLASS_DEMAND_REVENUES(CLASS) = 0.
               CLASS_CUSTOMER_REVENUES(CLASS) = 0.
               CLASS_ANNUAL_ENERGY(CLASS) = 0.
            ELSE
               IF(REALLY_KEPCO) THEN
                  FYREGY = FYREGY + CLASS_ANNUAL_ENERGY(CLASS)
               ELSE
                  FYREGY = FYREGY + CLASS_ANNUAL_ENERGY(CLASS)/
     +                                        (1. - CLASS_LOSSES(CLASS))
               ENDIF
               CLASS_COINCIDENT_PEAK(CLASS) =
     +                     SPLIT_DAY_TYPE_PEAK(DAY_TYPE,PEAK_MO,CLASS) *
     +                             CLASS_COIN_FACTOR(CLASS)/
     +                                   (1. - CLASS_PEAK_LOSSES(CLASS))
               CLASS_ENERGY_REVENUES(CLASS) = CLASS_ENERGY_RATE(CLASS) *
     +                               CLASS_ANNUAL_ENERGY(CLASS)/1000000.
               CLASS_DEMAND_REVENUES(CLASS) = CLASS_DEMAND_RATE(CLASS) *
     +              SPLIT_DAY_TYPE_PEAK(DAY_TYPE,PEAK_MO,CLASS)/1000. 
               CLASS_CUSTOMER_REVENUES(CLASS) =
     +                            CLASS_CUSTOMER_RATE(CLASS) *
     +                              (FORECAST_CUSTOMERS(CLASS)/1000000.)
            ENDIF
         ENDDO
C
C ALLOCATE REVENUES TO ASSET CLASSES
C
         CALL RATE_REVENUES_2_ASSET_CLASSES(ASSET_CLASS_NUM,
     +                                      ASSET_CLASS_VECTOR)
C
C DEALLOCATE LOCAL ARRAYS
C
         DEALLOCATE(OVERLAY_TYPE,
     +              CLASS_BIN_NAME)
      RETURN
!
! ADDED FOR CPL. 2/24/98. GAT.
!      
C***********************************************************************
      ENTRY RETURN_CLASS_LOSSES(R_CLASS_LOSSES,R_CLASS)
C***********************************************************************
         R_CLASS_LOSSES = CLASS_LOSSES(R_CLASS)
      RETURN
C***********************************************************************
      ENTRY GET_CLASS_NAME(R_AREA_NAME)
C***********************************************************************
C
         LENGHT = MIN(LEN(R_AREA_NAME(1)),LEN(CLASS_NAME(1)))
         DO CLASS = 1, MAX_LOAD_CLASSES
            IF(CLASS_EXISTS(CLASS)) THEN
               R_AREA_NAME(CLASS) = CLASS_NAME(CLASS)(1:LENGHT)
            ELSE
               R_AREA_NAME(CLASS) = ' '
            ENDIF
         ENDDO
      RETURN
C***********************************************************************
      ENTRY GET_CLASS_IN_AREA(R_CLASS_IN_AREA,R_AREA)
C***********************************************************************
C
         DO CLASS = 1, MAX_LOAD_CLASSES 
            R_CLASS_IN_AREA(CLASS) = WHICH_CLASSES_IN_AREA(R_AREA,CLASS)
         ENDDO
      RETURN
C***********************************************************************
      ENTRY CLASS_REVENUES_AFTER_DSM(DELTA_CLASS_SALES_FROM_DSM)
C***********************************************************************
         DO CLASS = 1, MAX_LOAD_CLASSES
           CLASS_ENERGY_REVENUES(CLASS) = 0.
           CLASS_DEMAND_REVENUES(CLASS) = 0.
           CLASS_CUSTOMER_REVENUES(CLASS) = 0.
           IF(.NOT. CLASS_EXISTS(CLASS)) CYCLE
           IF(MONTHY_ENRG_COST_PATTERN(CLASS) /= 0 ) THEN
               AVE_RATE = GET_MONTHLY_VAR_VALUES(
     +                                  MONTHY_ENRG_COST_PATTERN(CLASS),
     +                                             MONTHLY_COST_PATTERN)
               DO MO = 1, 12
                  CALL RETURN_CLASS_MONTHLY_SALES(MO,CLASS,CLASS_SALES)
                  CLASS_ENERGY_REVENUES(CLASS) =
     +                   CLASS_ENERGY_REVENUES(CLASS) +
     +                          MONTHLY_COST_PATTERN(MO) *
     +                            (CLASS_SALES -
     +                             DELTA_CLASS_SALES_FROM_DSM(MO,CLASS))
               ENDDO
               CLASS_ENERGY_REVENUES(CLASS) = CLASS_ENERGY_RATE(CLASS)*
     +                             CLASS_ENERGY_REVENUES(CLASS)/1000000.
            ELSE   
               CLASS_ENERGY_REVENUES(CLASS) = CLASS_ENERGY_RATE(CLASS) *
     +                    (CLASS_ANNUAL_ENERGY(CLASS) -
     +                     DELTA_CLASS_SALES_FROM_DSM(0,CLASS))/1000000.
            ENDIF
C
           IF(MONTHY_DEMAND_COST_PATTERN(CLASS) /= 0) THEN
               AVE_RATE = GET_MONTHLY_VAR_VALUES(
     +                                MONTHY_DEMAND_COST_PATTERN(CLASS),
     +                                             MONTHLY_COST_PATTERN)
               DO MO = 1, 12
                  CALL GET_CLASS_DSM_PEAK_FROM_CLASS(MO,CLASS,
     +                                                       MONTH_PEAK)
                  CLASS_DEMAND_REVENUES(CLASS) =
     +                          CLASS_DEMAND_REVENUES(CLASS) +
     +                          MONTHLY_COST_PATTERN(MO) * MONTH_PEAK
               ENDDO
               CLASS_DEMAND_REVENUES(CLASS) = CLASS_DEMAND_RATE(CLASS)*
     +                                CLASS_DEMAND_REVENUES(CLASS)/1000.
            ELSE   
               CALL GET_CLASS_DSM_PEAK_FROM_CLASS(PEAK_MO,
     +                                                 CLASS,MONTH_PEAK)
               CLASS_DEMAND_REVENUES(CLASS) = CLASS_DEMAND_RATE(CLASS)*
     +                                                  MONTH_PEAK/1000. 
            ENDIF
            CLASS_CUSTOMER_REVENUES(CLASS) = CLASS_CUSTOMER_RATE(CLASS)*
     +                              (FORECAST_CUSTOMERS(CLASS)/1000000.)
         ENDDO
C
C ALLOCATE REVENUES TO ASSET CLASSES
C
         CALL RATE_REVENUES_2_ASSET_CLASSES(ASSET_CLASS_NUM,
     +                                      ASSET_CLASS_VECTOR)
      RETURN
C***********************************************************************
      ENTRY RATE_REVENUES_2_ASSET_CLASSES(R_ASSET_CLASS_NUM,
     +                                    R_ASSET_CLASS_VECTOR)
C***********************************************************************
         IF(ALLOCATED(RATE_CLASS_ENRG_REVENUES))
     +                     DEALLOCATE(RATE_CLASS_ENRG_REVENUES,
     +                                RATE_CLASS_DEMAND_REVENUES,
     +                                RATE_CLASS_CUSTOMER_REVENUES,
     +                                RATE_CLASS_ENERGY,
     +                                RATE_CLASS_DEMAND,
     +                                RATE_CLASS_CUSTOMERS,
     +                                ASSET_CLASS_POINTER)
C
         IF(MAX_RATE_CLASS_ID_NUM > 0) THEN
            ALLOCATE(ASSET_CLASS_POINTER(MAX_RATE_CLASS_ID_NUM))
            CALL RETURN_CLASS_REVENUE_POINTER(ASSET_CLASS_POINTER)
         ENDIF
C
         ALLOCATE(RATE_CLASS_ENRG_REVENUES(0:NUMBER_OF_RATE_CLASSES),
     +           RATE_CLASS_DEMAND_REVENUES(0:NUMBER_OF_RATE_CLASSES),
     +           RATE_CLASS_CUSTOMER_REVENUES(0:NUMBER_OF_RATE_CLASSES),
     +           RATE_CLASS_ENERGY(0:NUMBER_OF_RATE_CLASSES),
     +           RATE_CLASS_DEMAND(0:NUMBER_OF_RATE_CLASSES),
     +           RATE_CLASS_CUSTOMERS(0:NUMBER_OF_RATE_CLASSES))
C         
         RATE_CLASS_ENRG_REVENUES = 0.
         RATE_CLASS_DEMAND_REVENUES = 0.
         RATE_CLASS_CUSTOMER_REVENUES = 0.
         RATE_CLASS_CUSTOMERS = 0.
         RATE_CLASS_DEMAND = 0.
         RATE_CLASS_ENERGY = 0.
C

         ALLOCATE(ASSET_CLASS_LIST(AVAIL_DATA_YEARS),
     +                          ASSET_ALLOCATION_LIST(AVAIL_DATA_YEARS))
C
         DO CLASS = 1, MAX_LOAD_CLASSES   
C
            IF(.NOT. CLASS_EXISTS(CLASS)) CYCLE
            ASSET_CLASS = R_ASSET_CLASS_NUM(CLASS)
            ASSET_ALLOCATION_VECTOR = R_ASSET_CLASS_VECTOR(CLASS)
C
            IF(ASSET_CLASS < 0) THEN
               CALL GET_ASSET_VAR(ABS(ASSET_CLASS),
     +                                      DUMMY_TYPE,ASSET_CLASS_LIST)
               CALL GET_ASSET_VAR(ABS(ASSET_ALLOCATION_VECTOR),
     +                                 DUMMY_TYPE,ASSET_ALLOCATION_LIST)
            ELSE
               ASSET_CLASS_LIST(1) = ASSET_CLASS
               ASSET_CLASS_LIST(2) = 0.
               ASSET_ALLOCATION_LIST(1) = 100.
               ASSET_ALLOCATION_LIST(2) = 0.
            ENDIF
            CLASS_POINTER = 1
            DO
               ASSET_CLASS = ASSET_CLASS_LIST(CLASS_POINTER)
               CALL CHECK_IF_CLASS_DEFINED(ASSET_CLASS)
               ASSET_CLASS = ASSET_CLASS + 1
               IF(ASSET_CLASS > 0) ASSET_CLASS = 
     +                                ASSET_CLASS_POINTER(ASSET_CLASS)
               ASSET_ALLOCATOR=ASSET_ALLOCATION_LIST(CLASS_POINTER)/100.
C
               RATE_CLASS_ENRG_REVENUES(ASSET_CLASS) = 
     +                           RATE_CLASS_ENRG_REVENUES(ASSET_CLASS) +
     +                    ASSET_ALLOCATOR * CLASS_ENERGY_REVENUES(CLASS)
               RATE_CLASS_DEMAND_REVENUES(ASSET_CLASS) = 
     +                         RATE_CLASS_DEMAND_REVENUES(ASSET_CLASS) +
     +                    ASSET_ALLOCATOR * CLASS_DEMAND_REVENUES(CLASS)
               RATE_CLASS_CUSTOMER_REVENUES(ASSET_CLASS) = 
     +                       RATE_CLASS_CUSTOMER_REVENUES(ASSET_CLASS) +
     +                  ASSET_ALLOCATOR * CLASS_CUSTOMER_REVENUES(CLASS)
               RATE_CLASS_CUSTOMERS(ASSET_CLASS) = 
     +                               RATE_CLASS_CUSTOMERS(ASSET_CLASS) +
     +                       ASSET_ALLOCATOR * FORECAST_CUSTOMERS(CLASS)
               RATE_CLASS_DEMAND(ASSET_CLASS) = 
     +                                  RATE_CLASS_DEMAND(ASSET_CLASS) +
     +                        ASSET_ALLOCATOR * CLASS_ANNUAL_PEAK(CLASS)
               RATE_CLASS_ENERGY(ASSET_CLASS) = 
     +                                  RATE_CLASS_ENERGY(ASSET_CLASS) +
     +                      ASSET_ALLOCATOR * CLASS_ANNUAL_ENERGY(CLASS)
C
               CLASS_POINTER = CLASS_POINTER + 1
               IF(CLASS_POINTER > AVAIL_DATA_YEARS) EXIT
               IF(ASSET_CLASS_LIST(CLASS_POINTER) == 0. .OR.
     +                      ASSET_CLASS_LIST(CLASS_POINTER) ==-99.) EXIT
            ENDDO ! ASSET CLASSES
         ENDDO !FORECAST CLASSES
         DEALLOCATE(ASSET_CLASS_LIST,ASSET_ALLOCATION_LIST)
         DO ASSET_CLASS = 1, NUMBER_OF_RATE_CLASSES
               RATE_CLASS_ENRG_REVENUES(0)=RATE_CLASS_ENRG_REVENUES(0) +
     +                             RATE_CLASS_ENRG_REVENUES(ASSET_CLASS)
               RATE_CLASS_DEMAND_REVENUES(0) = 
     +                           RATE_CLASS_DEMAND_REVENUES(0) +
     +                           RATE_CLASS_DEMAND_REVENUES(ASSET_CLASS)
               RATE_CLASS_CUSTOMER_REVENUES(0) = 
     +                         RATE_CLASS_CUSTOMER_REVENUES(0) +
     +                         RATE_CLASS_CUSTOMER_REVENUES(ASSET_CLASS)
               RATE_CLASS_CUSTOMERS(0) = RATE_CLASS_CUSTOMERS(0) +
     +                                 RATE_CLASS_CUSTOMERS(ASSET_CLASS)
               RATE_CLASS_DEMAND(0) = RATE_CLASS_DEMAND(0) +
     +                                    RATE_CLASS_DEMAND(ASSET_CLASS)
               RATE_CLASS_ENERGY(0) = RATE_CLASS_ENERGY(0) +
     +                                    RATE_CLASS_ENERGY(ASSET_CLASS)
         ENDDO
      RETURN
C***********************************************************************
      ENTRY RETURN_RATE_CLASS_REVENUES(R_CLASS,
     +                                 R_CLASS_EXISTS,
     +                                 R_RATE_CLASS_ENRG_REVENUES,
     +                                 R_RATE_CLASS_DEMAND_REVENUES,
     +                                 R_RATE_CLASS_CUSTOMER_REVENUES,
     +                                 R_RATE_CLASS_CUSTOMERS,
     +                                 R_RATE_CLASS_DEMAND,
     +                                 R_RATE_CLASS_ENERGY)
C***********************************************************************
         R_CLASS_EXISTS = .FALSE.
         R_RATE_CLASS_ENRG_REVENUES = 0.
         R_RATE_CLASS_DEMAND_REVENUES = 0.
         R_RATE_CLASS_CUSTOMER_REVENUES = 0.
         R_RATE_CLASS_CUSTOMERS = 0.
         R_RATE_CLASS_DEMAND = 0.
         R_RATE_CLASS_ENERGY = 0.
         IF(R_CLASS <= MAX_RATE_CLASS_ID_NUM .AND.
     +                         ALLOCATED(RATE_CLASS_ENRG_REVENUES)) THEN
            IF(R_CLASS ==0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN 
               R_CLASS_EXISTS = .TRUE.
C
               R_RATE_CLASS_ENRG_REVENUES =
     +                             RATE_CLASS_ENRG_REVENUES(ASSET_CLASS)
               R_RATE_CLASS_DEMAND_REVENUES =
     +                           RATE_CLASS_DEMAND_REVENUES(ASSET_CLASS)
               R_RATE_CLASS_CUSTOMER_REVENUES =
     +                         RATE_CLASS_CUSTOMER_REVENUES(ASSET_CLASS)
               R_RATE_CLASS_CUSTOMERS =RATE_CLASS_CUSTOMERS(ASSET_CLASS)
               R_RATE_CLASS_DEMAND = RATE_CLASS_DEMAND(ASSET_CLASS)
               R_RATE_CLASS_ENERGY = RATE_CLASS_ENERGY(ASSET_CLASS)
C
           ENDIF
         ENDIF
      RETURN
C***********************************************************************
      ENTRY RETURN_RATE_CLASS_ENERGY(R_CLASS,R_RATE_CLASS_ENERGY)
C***********************************************************************
C
         R_RATE_CLASS_ENERGY = 0.
         IF(R_CLASS <= MAX_RATE_CLASS_ID_NUM .AND.
     +                         ALLOCATED(RATE_CLASS_ENRG_REVENUES)) THEN
            IF(R_CLASS ==0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN 
               R_RATE_CLASS_ENERGY = RATE_CLASS_ENERGY(ASSET_CLASS)
            ENDIF
         ENDIF
      RETURN
C***********************************************************************
      ENTRY RETURN_FORECAST_CUSTOMERS(R_CLASS_CUSTOMERS)
C***********************************************************************
         DO CLASS = 1, MAX_LOAD_CLASSES
            R_CLASS_CUSTOMERS(CLASS) = FORECAST_CUSTOMERS(CLASS)
         ENDDO
      RETURN
C***********************************************************************
      ENTRY RETURN_ENERGY_PEAK_CLASS(R_SYSTEM_ENERGY,R_SYSTEM_PEAK)
C***********************************************************************
         DO MO = 1, 12
            R_SYSTEM_ENERGY(1,MO) = SYSTEM_FORECAST_ENERGY(1,MO)
            R_SYSTEM_ENERGY(2,MO) = SYSTEM_FORECAST_ENERGY(2,MO)
            R_SYSTEM_PEAK(1,MO) = SYSTEM_FORECAST_PEAK(1,MO)
            R_SYSTEM_PEAK(2,MO) = SYSTEM_FORECAST_PEAK(2,MO)
         ENDDO
      RETURN
C***********************************************************************
      ENTRY GET_SYSTEM_ENERGY_FROM_CLASS(MONTH,MO_ENERGY)
C***********************************************************************
         MO_ENERGY(1) = SYSTEM_FORECAST_ENERGY(1,MONTH)
         MO_ENERGY(2) = SYSTEM_FORECAST_ENERGY(2,MONTH)
      RETURN
C***********************************************************************
      ENTRY GET_SYSTEM_PEAK_FROM_CLASS(MONTH,MO_PEAK)
C***********************************************************************
         MO_PEAK(1) = SYSTEM_FORECAST_PEAK(1,MONTH)
         MO_PEAK(2) = SYSTEM_FORECAST_PEAK(2,MONTH)
      RETURN
C***********************************************************************
      ENTRY GET_CLASS_AND_SYSTEM_SALES(R_CLASS_SALES)
C***********************************************************************
         R_CLASS_SALES(SYSTEM_CLASS_NUM) = 0.
         DO CLASS = 1, MAX_LOAD_CLASSES
            R_CLASS_SALES(CLASS) = CLASS_ANNUAL_ENERGY(CLASS)
            R_CLASS_SALES(SYSTEM_CLASS_NUM)=CLASS_ANNUAL_ENERGY(CLASS) +
     +                                   R_CLASS_SALES(SYSTEM_CLASS_NUM)
         ENDDO
      RETURN
C***********************************************************************
      ENTRY GET_CLASS_REVENUES_FORM_SALES(R_CLASS_REVENUES)
C***********************************************************************
         R_CLASS_REVENUES(SYSTEM_CLASS_NUM) = 0.
         IF(ALLOCATED(CLASS_ENERGY_REVENUES)) THEN
         
            DO CLASS = 1, MAX_LOAD_CLASSES
               R_CLASS_REVENUES(CLASS) = CLASS_ENERGY_REVENUES(CLASS)
     +                                  + CLASS_DEMAND_REVENUES(CLASS)
     +                                  + CLASS_CUSTOMER_REVENUES(CLASS)
               R_CLASS_REVENUES(SYSTEM_CLASS_NUM) =
     +                              R_CLASS_REVENUES(CLASS)
     +                              + R_CLASS_REVENUES(SYSTEM_CLASS_NUM)
            ENDDO
         ELSE
            R_CLASS_REVENUES(1:MAX_LOAD_CLASSES) = 0.
         ENDIF
      RETURN
C***********************************************************************
      ENTRY GET_CLASS_REVENUES_BY_TYPE(R_CLASS_ENERGY_REVENUES,
     +                                 R_CLASS_DEMAND_REVENUES,
     +                                 R_CLASS_CUSTOMER_REVENUES)
C***********************************************************************
         IF(ALLOCATED(CLASS_ENERGY_REVENUES)) THEN
            DO CLASS = 1, MAX_LOAD_CLASSES
               R_CLASS_ENERGY_REVENUES(CLASS) =
     +                                      CLASS_ENERGY_REVENUES(CLASS)
               R_CLASS_DEMAND_REVENUES(CLASS) =
     +                                      CLASS_DEMAND_REVENUES(CLASS)
               R_CLASS_CUSTOMER_REVENUES(CLASS) =
     +                                    CLASS_CUSTOMER_REVENUES(CLASS)

            ENDDO
         ELSE
            R_CLASS_ENERGY_REVENUES(1:MAX_LOAD_CLASSES) = 0.
            R_CLASS_DEMAND_REVENUES(1:MAX_LOAD_CLASSES) = 0.
            R_CLASS_CUSTOMER_REVENUES(1:MAX_LOAD_CLASSES) = 0.
         ENDIF
      RETURN
C***********************************************************************
      ENTRY GET_REVENUES_FORM_CLASS_SALES(R_TOTAL_REVENUES)
C***********************************************************************
         R_TOTAL_REVENUES = 0.
         IF(ALLOCATED(CLASS_ENERGY_REVENUES)) THEN
            DO CLASS = 1, MAX_LOAD_CLASSES
               R_TOTAL_REVENUES = R_TOTAL_REVENUES +
     +                            CLASS_ENERGY_REVENUES(CLASS) +
     +                            CLASS_DEMAND_REVENUES(CLASS) +
     +                            CLASS_CUSTOMER_REVENUES(CLASS)
            ENDDO
         ENDIF
      RETURN
C***********************************************************************
      ENTRY RETURN_CLASS_REVENUE_CLASSES(R_NUM_OF_RATE_REVENUE_CLASSES,
     +                                   R_MAX_RATE_REVENUE_CLASS_NUM)
C***********************************************************************
         R_NUM_OF_RATE_REVENUE_CLASSES = NUMBER_OF_RATE_CLASSES
         R_MAX_RATE_REVENUE_CLASS_NUM = MAX_RATE_CLASS_ID_NUM
      RETURN
C***********************************************************************
      ENTRY RETURN_CLASS_REVENUE_POINTER(R_RATE_CLASS_POINTERS)
C***********************************************************************
         R_RATE_CLASS_POINTERS(1:MAX_RATE_CLASS_ID_NUM)  =
     +                 RATE_ASSET_CLASS_POINTER(1:MAX_RATE_CLASS_ID_NUM)
      RETURN
C***********************************************************************
      ENTRY CLOSE_FORECAST_DATA_FILES
C***********************************************************************
         DO CLASS = 0, MAX_LOAD_CLASSES
            CLOSE(900+CLASS,IOSTAT=IOS)
         ENDDO
      RETURN
      END
C***********************************************************************
      FUNCTION GET_CLASS_PEAK_NET_DSM_FOR_CAP(R_YR,UNIT_NAME,R_CLASS)
C***********************************************************************
         INTEGER*2   R_YR,R_CLASS,YR,CLASS
         REAL  GET_CLASS_PEAK_NET_DSM_FOR_CAP,DSM_FROM_DEVICE_PEAK,
     +         CLASS_PEAK
         
         LOGICAL*1 SYSTEM_BASED_FORECAST
         CHARACTER*(*) UNIT_NAME
C
         IF(SYSTEM_BASED_FORECAST()) THEN
            WRITE(4,*) trim(UNIT_NAME)//' is linked to area forecast',
     +                 CLASS
            WRITE(4,*) 'System forecast switch is set.'//
     +                 ' Unit capcity set to zero.'
            GET_CLASS_PEAK_NET_DSM_FOR_CAP = 0.
         ELSE
            YR = R_YR
            CLASS = R_CLASS
            CALL GET_RESERVES_FROM_DEVICE_PEAK(
     +                                    YR,CLASS,DSM_FROM_DEVICE_PEAK)
            CALL GET_CLASS_PEAK_FOR_CAPACITY(CLASS,YR,CLASS_PEAK)
            GET_CLASS_PEAK_NET_DSM_FOR_CAP = 
     +                                 CLASS_PEAK - DSM_FROM_DEVICE_PEAK
         ENDIF
      RETURN
      END
C***********************************************************************
C
C ROUTINE TO ALLOCATE AND SAVE MONTHY CLASS FORECAST INFORMATION ONLY
C IF NECESSARY
C
C***********************************************************************
C
      SUBROUTINE STORE_MONTHLY_CLASS_FORECASTS(CLASS,
     +                                         CLASS_FORECAST_ENERGY,
     +                                         CLASS_FORECAST_PEAK,
     +                                         CLASS_PEAK_LOSSES)
C***********************************************************************
C
      INCLUDE 'SpinLib.MON'
      INCLUDE 'SIZECOM.MON'
C
      INTEGER*2 CLASS
      REAL DSM_PEAK_DELTA(3,MAX_LOAD_CLASSES),R_CLASS_SALES
      REAL FORECAST_ENERGY(:,:,:),
     +     FORECAST_COINCIDENT_PEAK(:,:,:),
     +     FORECAST_COIN_PEAK_AFTER_DSM(:,:)
      ALLOCATABLE :: FORECAST_ENERGY,
     +     FORECAST_COINCIDENT_PEAK,
     +     FORECAST_COIN_PEAK_AFTER_DSM
      INTEGER*2 MO,I
      INTEGER*2 MONTH
C     REAL MO_PEAK(2)
      REAL MONTH_PEAK
      REAL R_CLASS_PEAK_AFTER_DSM(*)
C     REAL CLASS_NET_DSM_PEAK(MAX_LOAD_CLASSES)
      REAL CLASS_FORECAST_ENERGY(2,12),
     +     CLASS_FORECAST_PEAK(2,12),
     +     ENERGY(2,12),
     +     PEAK(2,12),
     +     CLASS_PEAK_LOSSES(*)
      SAVE FORECAST_ENERGY,
     +     FORECAST_COINCIDENT_PEAK,
     +     FORECAST_COIN_PEAK_AFTER_DSM
C
      IF(.NOT. ALLOCATED(FORECAST_ENERGY)) THEN
         ALLOCATE(FORECAST_ENERGY(2,12,MAX_LOAD_CLASSES),
     +            FORECAST_COINCIDENT_PEAK(2,12,MAX_LOAD_CLASSES),
     +            FORECAST_COIN_PEAK_AFTER_DSM(MAX_LOAD_CLASSES,12))
         FORECAST_ENERGY = 0.
         FORECAST_COINCIDENT_PEAK = 0.
         FORECAST_COIN_PEAK_AFTER_DSM = 0.
      ENDIF
      DO MO = 1, 12
           FORECAST_ENERGY(1,MO,CLASS) = CLASS_FORECAST_ENERGY(1,MO)
           FORECAST_ENERGY(2,MO,CLASS) = CLASS_FORECAST_ENERGY(2,MO)
           FORECAST_COINCIDENT_PEAK(1,MO,CLASS) =
     +                                         CLASS_FORECAST_PEAK(1,MO)
           FORECAST_COINCIDENT_PEAK(2,MO,CLASS) =
     +                                         CLASS_FORECAST_PEAK(2,MO)
           FORECAST_COIN_PEAK_AFTER_DSM(CLASS,MO) =
     +           MAX(CLASS_FORECAST_PEAK(1,MO)/
     +                                    (1.-CLASS_PEAK_LOSSES(CLASS)),
     +               CLASS_FORECAST_PEAK(2,MO)/
     +                                    (1.-CLASS_PEAK_LOSSES(CLASS)))
      ENDDO
C
      CALL STORE_CLASS_DATA_INTO_LAMCOM(CLASS,FORECAST_ENERGY,
     +                                        FORECAST_COINCIDENT_PEAK)
C
      RETURN
C***********************************************************************
      ENTRY STORE_CLASS_PEAK_NET_DSM
C***********************************************************************
         DO MO = 1, 12
            CALL GET_CLASS_MW_RESERVE_ALLOC(MO,DSM_PEAK_DELTA)
            DO I = 1, MAX_LOAD_CLASSES
               FORECAST_COIN_PEAK_AFTER_DSM(I,MO) =
     +                           FORECAST_COIN_PEAK_AFTER_DSM(I,MO) +
     +                                               DSM_PEAK_DELTA(1,I)
            ENDDO
         ENDDO
      RETURN
C***********************************************************************
      ENTRY RETURN_CLASS_MONTHLY_FORECASTS(CLASS,ENERGY,PEAK)
C***********************************************************************
         DO MO = 1, 12
            ENERGY(1,MO) = FORECAST_ENERGY(1,MO,CLASS)
            ENERGY(2,MO) = FORECAST_ENERGY(2,MO,CLASS)
            PEAK(1,MO) = FORECAST_COINCIDENT_PEAK(1,MO,CLASS)
            PEAK(2,MO) = FORECAST_COINCIDENT_PEAK(2,MO,CLASS)
         ENDDO
      RETURN
C***********************************************************************
      ENTRY RETURN_CLASS_MONTHLY_SALES(MONTH,CLASS,R_CLASS_SALES)
C***********************************************************************
         IF( MONTH /= 0) THEN   
            R_CLASS_SALES = FORECAST_ENERGY(1,MONTH,CLASS) +
     +                      FORECAST_ENERGY(2,MONTH,CLASS)
         ELSE
            R_CLASS_SALES = 0.
            DO MO = 1, 12
               R_CLASS_SALES = R_CLASS_SALES +
     +                     FORECAST_ENERGY(1,MO,CLASS) +
     +                           FORECAST_ENERGY(2,MO,CLASS)
            ENDDO
         ENDIF
      RETURN
C***********************************************************************
      ENTRY GET_CLASS_PEAK_FROM_CLASS(MONTH,CLASS,MONTH_PEAK)
C***********************************************************************
         IF(CLASS  > 0 .AND. CLASS <= MAX_LOAD_CLASSES) THEN
            MONTH_PEAK = MAX(FORECAST_COINCIDENT_PEAK(1,MONTH,CLASS),
     +                          FORECAST_COINCIDENT_PEAK(2,MONTH,CLASS))
         ELSE
            MONTH_PEAK = 0.
         ENDIF
      RETURN
C***********************************************************************
      ENTRY GET_CLASS_DSM_PEAK_FROM_CLASS(MONTH,CLASS,MONTH_PEAK)
C***********************************************************************
         IF(ALLOCATED(FORECAST_COIN_PEAK_AFTER_DSM)) THEN
            IF(CLASS  > 0 .AND. CLASS <= MAX_LOAD_CLASSES) THEN
               MONTH_PEAK = FORECAST_COIN_PEAK_AFTER_DSM(CLASS,MONTH)
            ELSE
               MONTH_PEAK = 0.
            ENDIF
         ELSE
            IF(CLASS  > 0 .AND. CLASS <= MAX_LOAD_CLASSES) THEN
               MONTH_PEAK = MAX(FORECAST_COINCIDENT_PEAK(1,MONTH,CLASS),
     +                          FORECAST_COINCIDENT_PEAK(2,MONTH,CLASS))
            ELSE
               MONTH_PEAK = 0.
            ENDIF
         ENDIF 
      RETURN
C***********************************************************************
      ENTRY GET_CLASSES_PEAK_AFTER_DSM(MONTH,R_CLASS_PEAK_AFTER_DSM)
C***********************************************************************
         IF(ALLOCATED(FORECAST_COIN_PEAK_AFTER_DSM)) THEN
            DO I = 1, MAX_LOAD_CLASSES
               R_CLASS_PEAK_AFTER_DSM(I) = 
     +                             FORECAST_COIN_PEAK_AFTER_DSM(I,MONTH)
            ENDDO
         ELSE
            DO I = 1, MAX_LOAD_CLASSES
               R_CLASS_PEAK_AFTER_DSM(I) = 
     +                      MAX(FORECAST_COINCIDENT_PEAK(1,MONTH,CLASS),
     +                          FORECAST_COINCIDENT_PEAK(2,MONTH,CLASS))
            ENDDO
         ENDIF
      RETURN
      END
C
C FUNCTION TO GET AND PASS CURRENT YEAR'S SYSTEM ENERGY AND PEAK 
C
C***********************************************************************
      FUNCTION CURRENT_YEAR_SYSTEM_STUFF()
C***********************************************************************
C
      INCLUDE 'SpinLib.MON'
      INCLUDE 'SIZECOM.MON'      
      LOGICAL*1 SYSTEM_BASED_FORECAST
      INTEGER*2 MO,CLASS,I
      CHARACTER*(*) UNIT_NAME
      REAL MO_PEAK(2),MO_ENERGY(2)
      REAL DAY_TYPE_PEAK_DELTA,CURRENT_YEAR_SYSTEM_STUFF
      REAL SEASON_SYSTEM_PEAK,SEASON_SYSTEM_ENERGY
      REAL GET_CLASS_PEAK_FORECAST,GET_AREA1_PEAK_NET_DSM
      REAL GET_CLASS_PEAK_NET_DSM,DSM_PEAK,CLASS_PEAK
      REAL GET_AREA1_PEAK_BEFORE_DSM
      LOGICAL*1 CLASS_IN_AREA(MAX_LOAD_CLASSES)
C
      CURRENT_YEAR_SYSTEM_STUFF = 0.
C
C***********************************************************************
      ENTRY SEASON_SYSTEM_ENERGY(MO)
C***********************************************************************
         IF(SYSTEM_BASED_FORECAST()) THEN
            CALL GET_SYSTEM_ENERGY_FROM_SYSTEM(MO,MO_ENERGY)
         ELSE
            CALL GET_SYSTEM_ENERGY_FROM_CLASS(MO,MO_ENERGY)
         ENDIF
         SEASON_SYSTEM_ENERGY = MO_ENERGY(1) + MO_ENERGY(2)
      RETURN
C***********************************************************************
      ENTRY SEASON_SYSTEM_PEAK(MO)
C***********************************************************************
         IF(SYSTEM_BASED_FORECAST()) THEN
            CALL GET_SYSTEM_PEAK_FROM_SYSTEM(MO,MO_PEAK)
         ELSE
            CALL GET_SYSTEM_PEAK_FROM_CLASS(MO,MO_PEAK)
         ENDIF
         SEASON_SYSTEM_PEAK = MAX(MO_PEAK(1),MO_PEAK(2))
      RETURN
C***********************************************************************
      ENTRY DAY_TYPE_PEAK_DELTA(MO)
C***********************************************************************
         IF(SYSTEM_BASED_FORECAST()) THEN
            CALL GET_SYSTEM_PEAK_FROM_SYSTEM(MO,MO_PEAK)
         ELSE
            CALL GET_SYSTEM_PEAK_FROM_CLASS(MO,MO_PEAK)
         ENDIF
         DAY_TYPE_PEAK_DELTA = MO_PEAK(1) - MO_PEAK(2)
      RETURN
C***********************************************************************
      ENTRY GET_CLASS_PEAK_FORECAST(MO,UNIT_NAME,CLASS)
C***********************************************************************
         IF(SYSTEM_BASED_FORECAST()) THEN
            WRITE(4,*) trim(UNIT_NAME)//' is linked to area forecast',
     +                 CLASS
            WRITE(4,*) 'System forecast switch is set.'//
     +                 ' Unit capcity set to zero.'
            GET_CLASS_PEAK_FORECAST = 0.
         ELSE
            CALL GET_CLASS_PEAK_FROM_CLASS(MO,CLASS,CLASS_PEAK)
            GET_CLASS_PEAK_FORECAST = CLASS_PEAK
         ENDIF
      RETURN
C***********************************************************************
      ENTRY GET_CLASS_PEAK_NET_DSM(MO,UNIT_NAME,CLASS)
C***********************************************************************
         IF(SYSTEM_BASED_FORECAST()) THEN
            WRITE(4,*) trim(UNIT_NAME)//' is linked to area forecast',
     +                 CLASS
            WRITE(4,*) 'System forecast switch is set.'//
     +                 ' Unit capcity set to zero.'
            GET_CLASS_PEAK_NET_DSM = 0.
         ELSE
            CALL GET_CLASS_DSM_PEAK_FROM_CLASS(MO,CLASS,DSM_PEAK)
            GET_CLASS_PEAK_NET_DSM = DSM_PEAK
         ENDIF
      RETURN
C***********************************************************************
      ENTRY GET_AREA1_PEAK_NET_DSM(MO,CLASS)
C***********************************************************************
         GET_AREA1_PEAK_NET_DSM = 0
         CALL GET_CLASS_IN_AREA(CLASS_IN_AREA,CLASS)
         DO I = 1, MAX_LOAD_CLASSES
            IF(.NOT. CLASS_IN_AREA(I)) CYCLE
            CALL GET_CLASS_DSM_PEAK_FROM_CLASS(MO,I,DSM_PEAK)
            GET_AREA1_PEAK_NET_DSM = GET_AREA1_PEAK_NET_DSM + DSM_PEAK
         ENDDO
      RETURN
C***********************************************************************
      ENTRY GET_AREA1_PEAK_BEFORE_DSM(MO,CLASS)
C***********************************************************************
         GET_AREA1_PEAK_BEFORE_DSM = 0
         CALL GET_CLASS_IN_AREA(CLASS_IN_AREA,CLASS)
         DO I = 1, MAX_LOAD_CLASSES
            IF(.NOT. CLASS_IN_AREA(I)) CYCLE
            CALL GET_CLASS_PEAK_FROM_CLASS(MO,I,CLASS_PEAK)
            GET_AREA1_PEAK_BEFORE_DSM = GET_AREA1_PEAK_BEFORE_DSM + 
     +                                                        CLASS_PEAK
         ENDDO
      RETURN
      END
C***********************************************************************
      SUBROUTINE STORE_CLASS_DATA_INTO_LAMCOM(R_CLASS,
     +                                       R_FORECAST_ENERGY,
     +                                       R_FORECAST_COINCIDENT_PEAK)
C***********************************************************************
      INCLUDE 'SpinLib.MON'
      INCLUDE 'SIZECOM.MON'
      INCLUDE 'LAMCOM.MON'
      INTEGER*2 MONTH,CLASS,R_CLASS
      REAL  R_FORECAST_ENERGY(2,12,MAX_LOAD_CLASSES),
     +      R_FORECAST_COINCIDENT_PEAK(2,12,MAX_LOAD_CLASSES),
     +      R_CLASS_CUSTOMERS(MAX_LOAD_CLASSES),
     +      R_CLASS_COIN_FACTOR(MAX_LOAD_CLASSES),
     +      R_CLASS_LOSSES(MAX_LOAD_CLASSES),
     +      R_CLASS_RESERVE_MARGIN(MAX_LOAD_CLASSES),
     +      R_CLASS_PEAK_LOSSES(MAX_LOAD_CLASSES)

      CLASS = R_CLASS
      DO MONTH = 1 , 12
         FORECAST_ENERGY(1,MONTH,CLASS) = 
     +                           R_FORECAST_ENERGY(1,MONTH,CLASS)
         FORECAST_COINCIDENT_PEAK(1,MONTH,CLASS) = 
     +                    R_FORECAST_COINCIDENT_PEAK(1,MONTH,CLASS)
         FORECAST_ENERGY(2,MONTH,CLASS) = 
     +                           R_FORECAST_ENERGY(2,MONTH,CLASS)
         FORECAST_COINCIDENT_PEAK(2,MONTH,CLASS) = 
     +                    R_FORECAST_COINCIDENT_PEAK(2,MONTH,CLASS)
      ENDDO
      RETURN
C***********************************************************************      
      ENTRY STORE_CUSTOMERS_INTO_LAMCOM(R_CLASS_CUSTOMERS,
     +                                  R_CLASS_COIN_FACTOR,
     +                                  R_CLASS_LOSSES,
     +                                  R_CLASS_RESERVE_MARGIN,
     +                                  R_CLASS_PEAK_LOSSES)
C***********************************************************************
         DO CLASS = 1 , MAX_LOAD_CLASSES  
            FORECAST_CUSTOMERS(CLASS) = R_CLASS_CUSTOMERS(CLASS)
            CLASS_LOSSES(CLASS) = R_CLASS_LOSSES(CLASS)
            CLASS_COIN_FACTOR(CLASS) = R_CLASS_COIN_FACTOR(CLASS)
            CLASS_RESERVE_MARGIN(CLASS) = R_CLASS_RESERVE_MARGIN(CLASS)
            CLASS_PEAK_LOSSES(CLASS) = R_CLASS_PEAK_LOSSES(CLASS)
         ENDDO
      RETURN
      END
C***********************************************************************
      FUNCTION GET_PEAK_ENERGY_AFTER_LOSSES(R_MO,R_PEAK_AFTER_LOSSES,
     +                                      R_ENERGY_AFTER_LOSSES)
C***********************************************************************
      INCLUDE 'SpinLib.MON'
      INCLUDE 'SIZECOM.MON'
      INCLUDE 'LAMCOM.MON'
      LOGICAL*1   GET_PEAK_ENERGY_AFTER_LOSSES,
     +            GET_THIS_MONTH_CLASS_FORECAST,
     +            RUN_TRANSACT/.FALSE./,YES_RUN_TRANSACT
      INTEGER*2 I,R_MO,R_CLASS
      REAL*4 R_PEAK_AFTER_LOSSES(2,MAX_LOAD_CLASSES),
     +       R_ENERGY_AFTER_LOSSES(2,MAX_LOAD_CLASSES),
     +       R_MONTH_ENERGY,R_MONTH_PEAK,
     +       GET_CUST_GROUP_PEAK,GET_CUST_GROUP_ENERGY
C      
! END DATA DECLARATIONS      
C
         RUN_TRANSACT = YES_RUN_TRANSACT()
!
         DO I = 1, MAX_LOAD_CLASSES
            IF(RUN_TRANSACT) THEN
               R_PEAK_AFTER_LOSSES(1,I) = GET_CUST_GROUP_PEAK(I,R_MO)
               R_PEAK_AFTER_LOSSES(2,I) = 0.
               R_ENERGY_AFTER_LOSSES(1,I) = 
     +                                     GET_CUST_GROUP_ENERGY(I,R_MO)
               R_ENERGY_AFTER_LOSSES(2,I) = 0.
            ELSE
               R_PEAK_AFTER_LOSSES(1,I) =
     +                         FORECAST_COINCIDENT_PEAK(1,R_MO,I)/
     +                                         (1.-CLASS_PEAK_LOSSES(I))
               R_PEAK_AFTER_LOSSES(2,I) =
     +                         FORECAST_COINCIDENT_PEAK(2,R_MO,I)/
     +                                         (1.-CLASS_PEAK_LOSSES(I))
               R_ENERGY_AFTER_LOSSES(1,I) = FORECAST_ENERGY(1,R_MO,I)/
     +                                              (1.-CLASS_LOSSES(I))
               R_ENERGY_AFTER_LOSSES(2,I) = FORECAST_ENERGY(2,R_MO,I)/
     +                                              (1.-CLASS_LOSSES(I))
            ENDIF
         ENDDO
         GET_PEAK_ENERGY_AFTER_LOSSES = .TRUE.
      RETURN
      ENTRY GET_THIS_MONTH_CLASS_FORECAST(R_MO,R_CLASS,R_MONTH_ENERGY,
     +                                                     R_MONTH_PEAK)
         IF(RUN_TRANSACT) THEN
            R_MONTH_ENERGY = GET_CUST_GROUP_PEAK(R_CLASS,R_MO)
            R_MONTH_PEAK = GET_CUST_GROUP_ENERGY(R_CLASS,R_MO)
         ELSE
            R_MONTH_ENERGY = (FORECAST_ENERGY(1,R_MO,R_CLASS) + 
     +                     FORECAST_ENERGY(2,R_MO,R_CLASS))/
     +                                        (1.-CLASS_LOSSES(R_CLASS))
            R_MONTH_PEAK = MAX(FORECAST_COINCIDENT_PEAK(1,R_MO,R_CLASS),
     +                       FORECAST_COINCIDENT_PEAK(1,R_MO,R_CLASS) )/
     +                                   (1.-CLASS_PEAK_LOSSES(R_CLASS))
         ENDIF
         GET_THIS_MONTH_CLASS_FORECAST = .TRUE.
      RETURN
      END
      
