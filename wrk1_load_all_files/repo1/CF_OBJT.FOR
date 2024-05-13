!     ******************************************************************
!     CF_OBJT.FOR
!     Copyright(c)  2000
!
!     Created: 1/29/2007 3:53:38 PM
!     Author : MARK S GERBER
!     Last change: gt 5/28/2013 12:28:38 PM
!     ******************************************************************

      SUBROUTINE CF_OBJECT
      use end_routine, only: end_program, er_message
      use logging
	  use filemod
      
C
      INCLUDE 'SpinLib.MON'
      INCLUDE 'SIZECOM.MON'
      INCLUDE 'KEPCOCOM.MON'
c                           ADDL Expansion: Probably need to change LRECL
      INTEGER*2 :: I,INUNIT,IREC,DELETE,LRECL=280
      INTEGER IOS
      CHARACTER*5 CLASS_OVL_NAME(MAX_LOAD_CLASSES),
     +            RFC_NAME,IFC_NAME,CFC_NAME,
     +            O1F_NAME,O2F_NAME,O3F_NAME
      CHARACTER*30 COMMENT
      CHARACTER*256 FILE_NAME
      CHARACTER*256 BASE_FILE_DIRECTORY,BASE_DATA_DIR
      CHARACTER*256 OUTPUT_DIRECTORY,DATA_DRIVE
      LOGICAL*4 FILE_EXISTS
C DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER*1024 RECLN
C LAM FILE NAMES
      CHARACTER*5 RESFRC,INDFRC,COMFRC,OTH1FRC,OTH2FRC,OTH3FRC
      INTEGER*2 BASE_YEAR
C DECLARATION FOR CLASS FORECASTS
      INTEGER*2 IS,YEAR,CLASS_NUM
      LOGICAL*1 CLASS_EXISTS(MAX_LOAD_CLASSES)
     +                                       /MAX_LOAD_CLASSES*.FALSE./,
     +          R_CLASS_EXISTS(MAX_LOAD_CLASSES),
     +          OVERLAY_CLASS(MAX_LOAD_CLASSES),
     +          CFC_EXISTS,RFC_EXISTS,IFC_EXISTS,O1F_EXISTS,
     +          O2F_EXISTS,O3F_EXISTS,
     +          CLASS_1_TO_5_FOUND,
     +          POOLING_TRANSACTIONS,
     +          CONTROL_AREA_FORECAST,
     +          CONTRIBUTES_TO_TOTAL,
     +          CLASS_6_AND_WABASH_VALLEY/.FALSE./,
     +          R_CLASS_6_AND_WV
      CHARACTER*5 SCENARIO_NAME(MAX_LOAD_CLASSES)
      CHARACTER*256 FILE_NAME1
      CHARACTER*32 CLASS_NAME
      CHARACTER*2 CLASS_OVL(MAX_LOAD_CLASSES),
     +            R_CLASS_OVL(MAX_LOAD_CLASSES),
     +            CFC_OL/'BC'/,RFC_OL/'BC'/,IFC_OL/'BC'/,
     +            O1F_OL/'BC'/,O2F_OL/'BC'/,O3F_OL/'BC'/
      CHARACTER*2 CLASS_TYPE(MAX_LOAD_CLASSES)
     +                                   /'CF','RF','IF','O1','O2','O3'/
      CHARACTER*5 CLASS_BIN_NAME(MAX_LOAD_CLASSES)
     +                /'COMFC','RESFC','INDFC','OTH1F','OTH2F','OTH3F'/
      CHARACTER*25 CLASS_NAMES(MAX_LOAD_CLASSES) /
     +             'Commercial Forecast'   ,
     +             'Residential Forecast'  ,
     +             'Industrial Forecast'   ,
     +             'Other Forecast Class 1',
     +             'Other Forecast Class 2',
     +             'Other Forecast Class 3'/
      REAL LOSSES,CUSTOMERS,CLASS_PEAK_LOSSES
      REAL CLSMWH1(12),CLSMW1(12),CLSMWH2(12),CLSMW2(12)
      REAL CLASS_COIN_FACTOR
      CHARACTER*15 FILE_TYPE/' Class Forecast'/ ! leading space needed
      INTEGER*1 FORECAST_GROWTH_YEARS
      INTEGER*2 CLASS_IN_CONTROL_AREA
      REAL CLASS_RESERVE_MARGIN,
     +     CLASS_ENERGY_PRICE,
     +     CLASS_DEMAND_PRICE,
     +     CLASS_CUSTOMERS_PRICE,
     +     TRANSMISSION_LOSSES
      INTEGER*2 ASSET_CLASS_NUM,
     +          ASSET_CLASS_VECTOR,
     +          MONTHY_ENRG_COST_PATTERN,
     +          MONTHY_DEMAND_COST_PATTERN
      LOGICAL*1 LAHEY_LF95
      CHARACTER*30 SCREEN_OUTPUT
C
C***********************************************************************
C
C          ROUTINE TO CONVERT METAFILE FILES TO DIRECT-ACESS BINARY
C          COPYRIGHT (C) 1983, 84, 85  M.S. GERBER & ASSOCIATES, INC.
C
C***********************************************************************
C
C CONVERT THE CLASS-FORECAST-DATA FILES
      ENTRY CF_MAKEBIN
C
      FORECAST_GROWTH_YEARS = AVAIL_DATA_YEARS
      SCENARIO_NAME(1) = COMFRC()
      SCENARIO_NAME(2) = RESFRC()
      SCENARIO_NAME(3) = INDFRC()
      SCENARIO_NAME(4) = OTH1FRC()
      SCENARIO_NAME(5) = OTH2FRC()
      SCENARIO_NAME(6) = OTH3FRC()
      DATA_DRIVE = OUTPUT_DIRECTORY()
      BASE_DATA_DIR = BASE_FILE_DIRECTORY()
      CLASS_1_TO_5_FOUND = .FALSE.
      DO I = 1, MAX_LOAD_CLASSES
         FILE_NAME = trim(BASE_DATA_DIR)//CLASS_TYPE(I)//'B'//
     +                                  trim(SCENARIO_NAME(I))//".DAT"
         INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
         IF(FILE_EXISTS) THEN
            CLASS_EXISTS(I) = .TRUE.
            IF(I < 6) THEN
               CLASS_1_TO_5_FOUND = .TRUE.
            ELSEIF(WABASH_VALLEY) THEN
               CLASS_6_AND_WABASH_VALLEY = .TRUE.
            ENDIF
            IF(LAHEY_LF95()) THEN
               SCREEN_OUTPUT = trim(CLASS_NAMES(I))//'-'//
     +                                                  SCENARIO_NAME(I)
               CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
            ELSE
               CALL MG_LOCATE_WRITE(16,30,SCENARIO_NAME(I),
     +                                                   ALL_VERSIONS,0)
               CALL MG_CLEAR_LINE_WRITE(17,9,36,trim(CLASS_NAMES(I)),
     +                                                   ALL_VERSIONS,0)
            ENDIF
C
            CUSTOMERS = 1.
            LOSSES = 0.            
C
            CLASS_COIN_FACTOR = 100.
            CLASS_PEAK_LOSSES = 0.
            CLASS_IN_CONTROL_AREA = -9999
            CLASS_RESERVE_MARGIN = 0.
            CLASS_NUM = -9999
            CLASS_ENERGY_PRICE = 0.
            CLASS_DEMAND_PRICE = 0.
            CLASS_CUSTOMERS_PRICE = 0.
            ASSET_CLASS_NUM = 0
            ASSET_CLASS_VECTOR = 0
            TRANSMISSION_LOSSES = 0.
            MONTHY_ENRG_COST_PATTERN = 0
            MONTHY_DEMAND_COST_PATTERN = 0
! 6/13/97. GAT. 
            CLSMWH1 = 0.
            CLSMW1 = 0.
            CLSMWH2 = 0.
            CLSMW2 = 0.
!            
            OPEN(10,FILE=FILE_NAME)
C
            FILE_NAME1 = trim(DATA_DRIVE)//"BC"//
     +                                         CLASS_BIN_NAME(I)//".BIN"
            OPEN(11,FILE=FILE_NAME1,ACCESS="DIRECT",
     +                                      STATUS="UNKNOWN",RECL=LRECL)
            IREC = 1
            READ(10,*) DELETE
            DOWHILE (IREC <= FORECAST_GROWTH_YEARS)
               READ(10,1000,IOSTAT=IOS) RECLN
               IF(IOS /=0) EXIT
               RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               READ(RECLN,*,ERR=200) DELETE,YEAR,CLASS_NUM,
     +           CLASS_NAME,CUSTOMERS,LOSSES,(CLSMWH1(IS),CLSMW1(IS),
     +           CLSMWH2(IS),CLSMW2(IS),IS=1,12),COMMENT,
     +           CLASS_COIN_FACTOR,CLASS_PEAK_LOSSES,
     +           CLASS_IN_CONTROL_AREA,
     +           CLASS_RESERVE_MARGIN,
     +           CLASS_ENERGY_PRICE,
     +           CLASS_DEMAND_PRICE,
     +           CLASS_CUSTOMERS_PRICE,
     +           ASSET_CLASS_NUM,
     +           ASSET_CLASS_VECTOR,
     +           TRANSMISSION_LOSSES,
     +           MONTHY_ENRG_COST_PATTERN,
     +           MONTHY_DEMAND_COST_PATTERN
C
               IF(CLASS_NUM == -9999) CLASS_NUM = I
               IF(CLASS_IN_CONTROL_AREA == -9999)
     +                                 CLASS_IN_CONTROL_AREA = CLASS_NUM
               IF(.NOT. WABASH_VALLEY) THEN
                  CLASS_IN_CONTROL_AREA = CLASS_NUM
               ENDIF
               WRITE(11,REC=IREC) DELETE,YEAR,CLASS_NUM,CLASS_NAME,
     +            CUSTOMERS,LOSSES,(CLSMWH1(IS),CLSMW1(IS),
     +            CLSMWH2(IS),CLSMW2(IS),IS=1,12),CLASS_COIN_FACTOR,
     +            CLASS_PEAK_LOSSES,
     +            CLASS_IN_CONTROL_AREA,
     +            CLASS_RESERVE_MARGIN,
     +            CLASS_ENERGY_PRICE,
     +            CLASS_DEMAND_PRICE,
     +            CLASS_CUSTOMERS_PRICE,
     +            ASSET_CLASS_NUM,
     +            ASSET_CLASS_VECTOR,
     +            TRANSMISSION_LOSSES,
     +            MONTHY_ENRG_COST_PATTERN,
     +            MONTHY_DEMAND_COST_PATTERN
C
               IREC = IREC + 1
            ENDDO
            CLOSE(10)
c           ENDFILE(11)
            CLOSE(11)
         ELSE IF(trim(SCENARIO_NAME(I)) .NE. 'NONE') THEN
            CALL STOP_NOFILE(CLASS_BIN_NAME(I)//FILE_TYPE,FILE_NAME)
         ENDIF
      ENDDO
      IF(REALLY_KEPCO .AND. CLASS_EXISTS(6)) NUMBER_OF_AREAS = 6
      IF(POOLING_TRANSACTIONS() .AND. 
     +          (.NOT. (CLASS_1_TO_5_FOUND .AND. CLASS_EXISTS(6)))) THEN
         IF(.NOT. (CLASS_1_TO_5_FOUND)) THEN
c            CALL LOCATE(16,9)
c            WRITE(6,*) 'There is no forecast information in',
c     +                         ' the primary utility in classes 1-5.'
            CALL MG_LOCATE_WRITE(16,9,
     +                 'There is no forecast information in'//
     +                         ' the primary utility in classes 1-5.',
     +                                                   ALL_VERSIONS,0)
         ENDIF
         IF(.NOT. CLASS_EXISTS(6)) THEN
c            CALL LOCATE(17,9)
c            WRITE(6,*) 'There is no forecast information for the',
c     +               ' secondary pool member in class 6.'
            CALL MG_LOCATE_WRITE(76,9,
     +                 'There is no forecast information for the'//
     +                      ' secondary pool member in class 6.',
     +                                                   ALL_VERSIONS,0)
         ENDIF
         er_message='stop requested from CF_OBJT SIID15'
         call end_program(er_message)
      ENDIF
      IF(CONTROL_AREA_FORECAST() .AND. 
     +          (.NOT. (CLASS_1_TO_5_FOUND .OR. CLASS_EXISTS(6)))) THEN
c         CALL LOCATE(17,9)
c         WRITE(6,*) 'There is no control area forecast information.'
         CALL MG_LOCATE_WRITE(17,9,
     +                 'There is no control area forecast information.',
     +                                                   ALL_VERSIONS,0)
         er_message='stop requested from CF_OBJT SIID16'
         call end_program(er_message)
      ENDIF
      RETURN
      ENTRY IS_CLASS_6_AND_WV(R_CLASS_6_AND_WV)
         R_CLASS_6_AND_WV = CLASS_6_AND_WABASH_VALLEY
      RETURN
C***********************************************************************
C
C          ROUTINE TO CREATE OVERLAY FILES
C          COPYRIGHT (C) 1984-88  M.S. GERBER & ASSOCIATES, INC.
C          COPYRIGHT (C) 1991-92  M.S. GERBER & ASSOCIATES, INC.
C
C***********************************************************************
C
C OVERLAY THE CLASS-FORECAST-DATA FILES
      ENTRY CF_MAKEOVL(RFC_NAME,IFC_NAME,CFC_NAME,
     +                 O1F_NAME,O2F_NAME,O3F_NAME,
     +                 RFC_EXISTS,IFC_EXISTS,CFC_EXISTS,
     +                 O1F_EXISTS,O2F_EXISTS,O3F_EXISTS)
C
      OVERLAY_CLASS(1) = CFC_EXISTS
      OVERLAY_CLASS(2) = RFC_EXISTS
      OVERLAY_CLASS(3) = IFC_EXISTS
      OVERLAY_CLASS(4) = O1F_EXISTS
      OVERLAY_CLASS(5) = O2F_EXISTS
      OVERLAY_CLASS(6) = O3F_EXISTS
      CLASS_OVL(1) = CFC_OL ! Consider use of EQUIVALENCE here and below
      CLASS_OVL(2) = RFC_OL
      CLASS_OVL(3) = IFC_OL
      CLASS_OVL(4) = O1F_OL
      CLASS_OVL(5) = O2F_OL
      CLASS_OVL(6) = O3F_OL
      CLASS_OVL_NAME(1) = CFC_NAME ! Consider use of EQUIVALENCE here and below
      CLASS_OVL_NAME(2) = RFC_NAME
      CLASS_OVL_NAME(3) = IFC_NAME
      CLASS_OVL_NAME(4) = O1F_NAME
      CLASS_OVL_NAME(5) = O2F_NAME
      CLASS_OVL_NAME(6) = O3F_NAME
      DATA_DRIVE = OUTPUT_DIRECTORY()
      DO I = 1, MAX_LOAD_CLASSES
         IF(OVERLAY_CLASS(I) .AND. CLASS_EXISTS(I)) THEN
            IF(LAHEY_LF95()) THEN
               SCREEN_OUTPUT = trim(CLASS_NAMES(I))//'-'//
     +                                                 CLASS_OVL_NAME(I)
            ELSE
               CALL MG_CLEAR_LINE_WRITE(17,9,36,CLASS_NAMES(I),
     +                                                   ALL_VERSIONS,0)
            ENDIF
            FILE_NAME = trim(DATA_DRIVE)//CLASS_TYPE(I)//'O'//
     +                                 trim(CLASS_OVL_NAME(I))//".DAT"
            OPEN(10,FILE=FILE_NAME)
            READ(10,*) DELETE
            INUNIT = 12
            IF(CLASS_OVL(I) == 'BC') THEN
               OPEN(11,FILE=trim(DATA_DRIVE)//'BC'//
     +                        CLASS_BIN_NAME(I)//".BIN",ACCESS="DIRECT")
               INUNIT = 11
            ENDIF
            OPEN(12,FILE=trim(DATA_DRIVE)//'OL'//
     +             CLASS_BIN_NAME(I)//".BIN",ACCESS="DIRECT",RECL=LRECL)
            IREC = 0
            DO
               IREC = IREC + 1
               READ(INUNIT,REC=IREC,IOSTAT=IOS) DELETE,YEAR,
     +               CLASS_NUM,CLASS_NAME,CUSTOMERS,LOSSES,
     +              (CLSMWH1(IS),CLSMW1(IS),CLSMWH2(IS),
     +               CLSMW2 (IS),IS=1,12),CLASS_COIN_FACTOR,
     +               CLASS_PEAK_LOSSES,
     +               CLASS_IN_CONTROL_AREA,
     +               CLASS_RESERVE_MARGIN,
     +               CLASS_ENERGY_PRICE,
     +               CLASS_DEMAND_PRICE,
     +               CLASS_CUSTOMERS_PRICE,
     +               ASSET_CLASS_NUM,
     +               ASSET_CLASS_VECTOR,
     +               TRANSMISSION_LOSSES,
     +               MONTHY_ENRG_COST_PATTERN,
     +               MONTHY_DEMAND_COST_PATTERN
               IF(IOS /= 0) EXIT
               READ(10,1000,IOSTAT=IOS) RECLN
               IF(IOS == 0) THEN
                  RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,'
                  READ(RECLN,*,ERR=200) DELETE,YEAR,CLASS_NUM,
     +               CLASS_NAME,CUSTOMERS,LOSSES,(CLSMWH1(IS),
     +               CLSMW1(IS),CLSMWH2(IS),CLSMW2(IS),IS=1,12),
     +               COMMENT,CLASS_COIN_FACTOR,
     +               CLASS_PEAK_LOSSES,
     +               CLASS_IN_CONTROL_AREA,
     +               CLASS_RESERVE_MARGIN,
     +               CLASS_ENERGY_PRICE,
     +               CLASS_DEMAND_PRICE,
     +               CLASS_CUSTOMERS_PRICE,
     +               ASSET_CLASS_NUM,
     +               ASSET_CLASS_VECTOR,
     +               TRANSMISSION_LOSSES,
     +               MONTHY_ENRG_COST_PATTERN,
     +               MONTHY_DEMAND_COST_PATTERN

               ENDIF
               WRITE(12,REC=IREC) DELETE,YEAR,CLASS_NUM,CLASS_NAME,
     +            CUSTOMERS,LOSSES,(CLSMWH1(IS),CLSMW1(IS),
     +            CLSMWH2(IS),CLSMW2(IS),IS=1,12),CLASS_COIN_FACTOR,
     +            CLASS_PEAK_LOSSES,
     +            CLASS_IN_CONTROL_AREA,
     +            CLASS_RESERVE_MARGIN,
     +            CLASS_ENERGY_PRICE,
     +            CLASS_DEMAND_PRICE,
     +            CLASS_CUSTOMERS_PRICE,
     +            ASSET_CLASS_NUM,
     +            ASSET_CLASS_VECTOR,
     +            TRANSMISSION_LOSSES,
     +            MONTHY_ENRG_COST_PATTERN,
     +            MONTHY_DEMAND_COST_PATTERN
            ENDDO
            CLOSE(10)
c           ENDFILE(12)
            CLOSE(12)
            IF(CLASS_OVL(I) == 'BC') CLOSE(11)
            CLASS_OVL(I) = 'OL'
         ENDIF
      ENDDO  ! CLASS LOOP
      CFC_OL = CLASS_OVL(1) ! Consider use of EQUIVALENCE here and above
      RFC_OL = CLASS_OVL(2)
      IFC_OL = CLASS_OVL(3)
      O1F_OL = CLASS_OVL(4)
      O2F_OL = CLASS_OVL(5)
      O3F_OL = CLASS_OVL(6)
      RETURN
C***********************************************************************
      ENTRY CF_GET_PEAKS
C***********************************************************************
C
      CLASS_OVL(1) = CFC_OL ! Consider use of EQUIVALENCE here and below
      CLASS_OVL(2) = RFC_OL
      CLASS_OVL(3) = IFC_OL
      CLASS_OVL(4) = O1F_OL
      CLASS_OVL(5) = O2F_OL
      CLASS_OVL(6) = O3F_OL
      DATA_DRIVE = OUTPUT_DIRECTORY()
      DO I = 1, MAX_LOAD_CLASSES
         IF(CLASS_EXISTS(I)) THEN
            OPEN(10,FILE=trim(DATA_DRIVE)//CLASS_OVL(I)//
     +             CLASS_BIN_NAME(I)//".BIN",ACCESS="DIRECT",RECL=LRECL)
            IREC = 0
            DO
               IREC = IREC + 1
               READ(10,REC=IREC,IOSTAT=IOS) DELETE,YEAR,
     +               CLASS_NUM,CLASS_NAME,CUSTOMERS,LOSSES,
     +              (CLSMWH1(IS),CLSMW1(IS),CLSMWH2(IS),
     +               CLSMW2 (IS),IS=1,12),CLASS_COIN_FACTOR,
     +               CLASS_PEAK_LOSSES,CLASS_IN_CONTROL_AREA
               IF(IOS /= 0) EXIT
               IF(WABASH_VALLEY .AND. CLASS_IN_CONTROL_AREA /= 1) THEN
                  CONTRIBUTES_TO_TOTAL = .FALSE.
               ELSE
                  CONTRIBUTES_TO_TOTAL = .TRUE.
               ENDIF
               CALL CALCULATE_CLASS_PEAKS(YEAR-BASE_YEAR(),
     +                                    CLASS_COIN_FACTOR/
     +                              (100.*(1.-CLASS_PEAK_LOSSES)),
     +                                    CLSMW1,CLSMW2,CLASS_NUM,
     +                                             CONTRIBUTES_TO_TOTAL)
            ENDDO
            CLOSE(10)
         ENDIF
      ENDDO  ! CLASS LOOP
      CALL FIND_AND_STORE_SYSTEM_PEAKS
!      CALL DEALLOCATE_SUM_CLASS_PEAKS
      CALL SET_FIRST_CLASS_TRUE
      RETURN
C***********************************************************************
      ENTRY RESET_CLASS_FORECAST_OL
C***********************************************************************
C
      CFC_OL = 'BC'
      RFC_OL = 'BC'
      IFC_OL = 'BC'
      O1F_OL = 'BC'
      O2F_OL = 'BC'
      O3F_OL = 'BC'
      RETURN
C
C***********************************************************************
      ENTRY GET_CLASS_OVLS(R_CLASS_OVL)
C***********************************************************************
         R_CLASS_OVL(1) = CFC_OL
         R_CLASS_OVL(2) = RFC_OL
         R_CLASS_OVL(3) = IFC_OL
         R_CLASS_OVL(4) = O1F_OL
         R_CLASS_OVL(5) = O2F_OL
         R_CLASS_OVL(6) = O3F_OL
      RETURN
C***********************************************************************
      ENTRY GET_CLASS_EXISTS(R_CLASS_EXISTS)
C***********************************************************************
         DO I = 1, MAX_LOAD_CLASSES
            R_CLASS_EXISTS(I) = CLASS_EXISTS(I)
         ENDDO
      RETURN
c  200 CALL LOCATE(20,0)
c      WRITE(6,1010) trim(RECLN)
  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from CF_OBJT SIID17'
      call end_program(er_message)

 1000 FORMAT(A)
 1010 FORMAT('&',A)
      END
C
C***********************************************************************
      SUBROUTINE CS_OBJECT
      use end_routine, only: end_program, er_message
      use logging
C***********************************************************************
C
      INCLUDE 'SPINLIB.MON'
      INCLUDE 'SIZECOM.MON'
      INTEGER*2 IREC,INUNIT,DELETE,LRECL/240/
      INTEGER IOS
      CHARACTER*5 OVERLAY_FAMILY_NAME
      CHARACTER*5 CLSHAPE
      CHARACTER*256 FILE_NAME
      CHARACTER*256 BASE_FILE_DIRECTORY
      CHARACTER*256 OUTPUT_DIRECTORY,DATA_DRIVE
      LOGICAL*4 FILE_EXISTS
C DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER*1024 RECLN
C DECLARATION FOR CLASS-LOAD VARIABLES
      INTEGER*2 CLASS_NUM,SEASON,UNITS
      REAL CLSLOD1(24),CLSLOD2(24)
      CHARACTER*32 CLASS_NAME
      CHARACTER*17 FILE_TYPE/'Class Load Shapes'/
      CHARACTER*2 CLS_OL/'BC'/,R_CLS_OL
      LOGICAL*1 LAHEY_LF95
      CHARACTER*30 SCREEN_OUTPUT

C***********************************************************************
C
C          ROUTINE TO CONVERT METAFILE FILES TO DIRECT-ACESS BINARY
C          COPYRIGHT (C) 1983, 84, 85  M.S. GERBER & ASSOCIATES, INC.
C
C***********************************************************************
C
C CONVERT THE CLASS LOAD-SHAPE FILE
      ENTRY CS_MAKEBIN
      DATA_DRIVE = OUTPUT_DIRECTORY()
      FILE_NAME = trim(BASE_FILE_DIRECTORY())//"CSB"//
     +                                         trim(CLSHAPE())//".DAT"
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      IF(FILE_EXISTS) THEN
         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//CLSHAPE()
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_LOCATE_WRITE(16,30,CLSHAPE(),ALL_VERSIONS,0)
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         ENDIF
         OPEN(10,FILE=FILE_NAME)
         OPEN(11,FILE=trim(DATA_DRIVE)//"BCCLSLS.BIN",
     +                      ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
         IREC = 1
         READ(10,*) DELETE
         DO
            READ(10,1000,IOSTAT=IOS) RECLN
            IF(IOS /=0) EXIT
            RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
            READ(RECLN,*,ERR=200) DELETE,CLASS_NUM,CLASS_NAME,SEASON,
     +                                             UNITS,CLSLOD1,CLSLOD2
            WRITE(11,REC=IREC) DELETE,CLASS_NUM,CLASS_NAME,SEASON,
     +                                             UNITS,CLSLOD1,CLSLOD2
            IREC = IREC + 1
         ENDDO
         CLOSE(10)
      ELSE IF(INDEX(CLSHAPE(),'NONE') == 0) THEN
         CALL STOP_NOFILE(FILE_TYPE,FILE_NAME)
      ELSE
         OPEN(11,FILE=trim(DATA_DRIVE)//"BCCLSLS.BIN",
     +                      ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
      ENDIF
c     ENDFILE(11)
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
C OVERLAY THE CLASS LOAD-SHAPE FILE
      ENTRY CS_MAKEOVL(OVERLAY_FAMILY_NAME)
      IF(LAHEY_LF95()) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSE
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         CALL LOCATE(10,51)
      ENDIF
      DATA_DRIVE = OUTPUT_DIRECTORY()
      FILE_NAME = trim(DATA_DRIVE)//"CSO"//
     +                               trim(OVERLAY_FAMILY_NAME)//".DAT"
      OPEN(10,FILE=FILE_NAME)
      READ(10,*) DELETE
      INUNIT = 12
      IF(CLS_OL == 'BC') THEN
         OPEN(11,FILE=trim(DATA_DRIVE)//"BCCLSLS.BIN",
     +                                       ACCESS="DIRECT",RECL=LRECL)
         INUNIT = 11
      ENDIF
      OPEN(12,FILE=trim(DATA_DRIVE)//"OLCLSLS.BIN",
     +                      ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
      IREC = 0
      DO
         IREC = IREC + 1
         READ(INUNIT,REC=IREC,IOSTAT=IOS) DELETE,CLASS_NUM,
     +      CLASS_NAME,SEASON,UNITS,CLSLOD1,CLSLOD2
         IF(IOS /= 0) EXIT
         READ(10,1000,IOSTAT=IOS) RECLN
         IF(IOS == 0) THEN
            RECLN = trim(RECLN)//',,,,,,,,,,,,,'
            READ(RECLN,*,ERR=200) DELETE,CLASS_NUM,
     +         CLASS_NAME,SEASON,UNITS,CLSLOD1,CLSLOD2
         ENDIF
         WRITE(12,REC=IREC) DELETE,CLASS_NUM,
     +      CLASS_NAME,SEASON,UNITS,CLSLOD1,CLSLOD2
      ENDDO
      CLOSE(10)
      CLOSE(12)
      IF(CLS_OL == 'BC') CLOSE(11)
      CLS_OL = 'OL'
      RETURN
c  200 CALL LOCATE(20,0)
c      WRITE(6,1010) trim(RECLN)
  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from CF_OBJT SIID18'
      call end_program(er_message)
C***********************************************************************
      ENTRY RESET_CLS_OL
C***********************************************************************
         CLS_OL = 'BC'
      RETURN
C***********************************************************************
      ENTRY GET_CLASS_LOAD_SHAPE_OL(R_CLS_OL)
C***********************************************************************
         R_CLS_OL = CLS_OL
      RETURN
 1000 FORMAT(A)
 1010 FORMAT('&',A)
      END
C
C***********************************************************************
      SUBROUTINE CH_OBJECT
      use end_routine, only: end_program, er_message
      use logging
C***********************************************************************
C
      INCLUDE 'SPINLIB.MON'
      INCLUDE 'SIZECOM.MON'
      INTEGER*2 IREC,INUNIT,DELETE,LRECL/140/
      INTEGER IOS
      LOGICAL*4 FILE_EXISTS
      CHARACTER*5 CLSHIST,OVERLAY_FAMILY_NAME
      CHARACTER*256 FILE_NAME
      CHARACTER*256 BASE_FILE_DIRECTORY
      CHARACTER*256 OUTPUT_DIRECTORY,DATA_DRIVE
C DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER*1024 RECLN
C DECLARATION FOR CLASS HISTORY FILE
      INTEGER*2 CLASS
      REAL VALUES_24(24)
      CHARACTER*13 FILE_TYPE/'Class History'/
      CHARACTER*32 CLASS_NAME
      CHARACTER*2 CLASS_HIST_OL/'BC'/,R_CLASS_HIST_OL
      LOGICAL*1 LAHEY_LF95
      CHARACTER*30 SCREEN_OUTPUT
C***********************************************************************
C
C          SUBROUTINE TO CONVERT ACSII FILES TO DIRECT ACESS BINARY
C          COPYRIGHT (C) 1993  M.S. GERBER & ASSOCIATES, INC.
C           ALL RIGHTS RESERVED
C***********************************************************************
C
C CONVERT THE CLASS-HISTORY FILE
      ENTRY CH_MAKEBIN
      FILE_NAME = trim(BASE_FILE_DIRECTORY())//
     +                                  "CHB"//trim(CLSHIST())//".DAT"
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      IF(FILE_EXISTS) THEN
         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//CLSHIST()
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_LOCATE_WRITE(16,30,CLSHIST(),ALL_VERSIONS,0)
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         ENDIF
         OPEN(10,FILE=FILE_NAME)
         OPEN(11,FILE=trim(OUTPUT_DIRECTORY())//"BCCLHIST.BIN",
     +                      ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
         IREC = 1
         READ(10,*) DELETE
         DO
            READ(10,1000,IOSTAT=IOS) RECLN
            IF(IOS /=0) EXIT
            RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
            READ(RECLN,*,ERR=200) DELETE,CLASS,CLASS_NAME,VALUES_24
            WRITE(11,REC=IREC) DELETE,CLASS,CLASS_NAME,VALUES_24
            IREC = IREC + 1
         ENDDO
         CLOSE(10)
C        ENDFILE(11)
         CLOSE(11)
      ELSE
         CALL STOP_NOFILE(FILE_TYPE,FILE_NAME)
      ENDIF
      RETURN
C OVERLAY THE CLASS-HISTORY FILE
C***********************************************************************
      ENTRY CH_MAKEOVL(OVERLAY_FAMILY_NAME)
C***********************************************************************
      IF(LAHEY_LF95()) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSE
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         CALL LOCATE(10,51)
      ENDIF
      DATA_DRIVE = OUTPUT_DIRECTORY()
      FILE_NAME = trim(DATA_DRIVE)//"CHO"//
     +                               trim(OVERLAY_FAMILY_NAME)//".DAT"
      OPEN(10,FILE=FILE_NAME)
      READ(10,*) DELETE
      INUNIT = 12
      IF(CLASS_HIST_OL == 'BC') THEN
         OPEN(11,FILE=trim(DATA_DRIVE)//"BCCLHIST.BIN",
     +                                       ACCESS="DIRECT",RECL=LRECL)
         INUNIT = 11
      ENDIF
      OPEN(12,FILE=trim(DATA_DRIVE)//"OLCLHIST.BIN",
     +                      ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
      IREC = 0
      DO
         IREC = IREC + 1
         READ(INUNIT,REC=IREC,IOSTAT=IOS) DELETE,CLASS,CLASS_NAME,
     +                                    VALUES_24
         IF(IOS /= 0) EXIT
         READ(10,1000,IOSTAT=IOS) RECLN
         IF(IOS == 0) THEN
            RECLN = trim(RECLN)//',,,,,,,,,,,,,'
            READ(RECLN,*,ERR=200) DELETE,CLASS,CLASS_NAME,VALUES_24
         ENDIF
         WRITE(12,REC=IREC) DELETE,CLASS,CLASS_NAME,VALUES_24
      ENDDO
      CLOSE(10)
      CLOSE(12)
      IF(CLASS_HIST_OL == 'BC') CLOSE(11)
      CLASS_HIST_OL = 'OL'
      RETURN
c  200 CALL LOCATE(20,0)
c      WRITE(6,1010) trim(RECLN)
  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from CF_OBJT SIID19'
      call end_program(er_message)
C***********************************************************************
      ENTRY RESET_CLASS_HIST_OL
C***********************************************************************
         CLASS_HIST_OL = 'BC'
      RETURN
C***********************************************************************
      ENTRY GET_CLASS_HIST_OL(R_CLASS_HIST_OL)
C***********************************************************************
         R_CLASS_HIST_OL = CLASS_HIST_OL
      RETURN
 1000 FORMAT(A)
 1010 FORMAT('&',A)
      END
C
C***********************************************************************
      SUBROUTINE CALCULATE_CLASS_PEAKS(IYR,CLASS_COIN_LOSS_FACTOR,
     +                         CLSMW1,CLSMW2,CLASS,CONTRIBUTES_TO_TOTAL)
C***********************************************************************
C
      INCLUDE 'SpinLib.MON'
      INCLUDE 'SIZECOM.MON'
      REAL CLASS_COIN_LOSS_FACTOR,CLSMW1(*),CLSMW2(*)
      REAL PLANNING_PEAK,CLASS_PEAK,R_PEAK
      INTEGER*2 IYR,MO,YEAR,CLASS,TOTAL_CLASSES,PEAK_MONTH,PEAK_DAY,
     +            R_CLASS,R_YR,R_MONTH,MAX_CLASS/7/,MIN_CLASS/1/
      LOGICAL*1 FIRST_CLASS/.TRUE./
C SYSTEM PEAK VARIABLES
      REAL PEAKS(12,2),SYS_FC_DATA(*),REAL_TEMP,R_CLASS_PEAK
      REAL STORE_ANNUAL_PEAK_AND_MONTH
      INTEGER*2 MO_OFFSET
C
C INCLUDE COMMON BLOCK OF YEARLY SYSTEM FORECAST STATISTICS
C
      REAL FYREGY(AVAIL_DATA_YEARS),FYRPK(AVAIL_DATA_YEARS)
      COMMON/FRC2/ FYREGY,FYRPK
C VARIBLES FOR CAPACITY PLANNING PEAKS
      INTEGER*1 FORECAST_GROWTH_YEARS
      PARAMETER (FORECAST_GROWTH_YEARS = AVAIL_DATA_YEARS)
      REAL SUM_CLASS_PEAKS(:,:,:,:),CLASS_ANN_COIN_PEAK(:,:)
      LOGICAL*1 CONTRIBUTES_TO_TOTAL
      ALLOCATABLE SUM_CLASS_PEAKS,CLASS_ANN_COIN_PEAK
      REAL CLASS_GROWTH_TRIGGER,CLASS_GROWTH_TRIGGER_LEVEL
      REAL SYS_GROWTH_TRIGGER,SYS_GROWTH_TRIGGER_LEVEL
      SAVE SUM_CLASS_PEAKS,PEAKS,CLASS_ANN_COIN_PEAK
C
      TOTAL_CLASSES = MAX_LOAD_CLASSES + 1
C     FORECAST_GROWTH_YEARS = AVAIL_DATA_YEARS
      IF(IYR == 1 .AND. FIRST_CLASS) THEN
         IF(ALLOCATED(SUM_CLASS_PEAKS)) DEALLOCATE(SUM_CLASS_PEAKS)
         ALLOCATE(SUM_CLASS_PEAKS(12,2,FORECAST_GROWTH_YEARS,
     +                                                   TOTAL_CLASSES))
         SUM_CLASS_PEAKS = 0.
         IF(ALLOCATED(CLASS_ANN_COIN_PEAK))
     +                                   DEALLOCATE(CLASS_ANN_COIN_PEAK)
         ALLOCATE(CLASS_ANN_COIN_PEAK(MAX_LOAD_CLASSES,
     +                                           FORECAST_GROWTH_YEARS))
         CLASS_ANN_COIN_PEAK = 0.
         FIRST_CLASS = .FALSE.
      ENDIF
      CLASS_GROWTH_TRIGGER_LEVEL = CLASS_GROWTH_TRIGGER()
      DO MO = 1, 12
         IF(IYR == 1) THEN
            PEAKS(MO,1) = CLSMW1(MO)
            PEAKS(MO,2) = CLSMW2(MO)
         ELSE
            CLASS_PEAK = CLSMW1(MO)
            IF(CLASS_PEAK <= CLASS_GROWTH_TRIGGER_LEVEL) THEN
               PEAKS(MO,1) = (1.+CLASS_PEAK/100.) * PEAKS(MO,1)
            ELSE
               PEAKS(MO,1) = CLSMW1(MO)
            ENDIF
            CLASS_PEAK = CLSMW2(MO)
            IF(CLASS_PEAK <= CLASS_GROWTH_TRIGGER_LEVEL) THEN
               PEAKS(MO,2) = (1.+CLASS_PEAK/100.) * PEAKS(MO,2)
            ELSE
               PEAKS(MO,2) = CLSMW2(MO)
            ENDIF
         ENDIF
!         
! ADDED 2/11/98. GAT. LARRY KEY, SRP
!
         IF(CLASS < MIN_CLASS .OR. CLASS > MAX_CLASS) THEN
            WRITE(4,*) "A class forecast file contains class"
            WRITE(4,*) "number ",CLASS," In year ",IYR
            WRITE(4,*) "It has been reassigned to Class=1."
            CLASS = 1
         ENDIF
!         
         IF(CONTRIBUTES_TO_TOTAL) THEN
C         
            SUM_CLASS_PEAKS(MO,1,IYR,CLASS) = 
     +                 SUM_CLASS_PEAKS(MO,1,IYR,CLASS) +
     +                                PEAKS(MO,1)*CLASS_COIN_LOSS_FACTOR
            SUM_CLASS_PEAKS(MO,2,IYR,CLASS) = 
     +                 SUM_CLASS_PEAKS(MO,2,IYR,CLASS) +
     +                                PEAKS(MO,2)*CLASS_COIN_LOSS_FACTOR
C     
            SUM_CLASS_PEAKS(MO,1,IYR,TOTAL_CLASSES) = 
     +                 SUM_CLASS_PEAKS(MO,1,IYR,TOTAL_CLASSES) +
     +                                PEAKS(MO,1)*CLASS_COIN_LOSS_FACTOR
            SUM_CLASS_PEAKS(MO,2,IYR,TOTAL_CLASSES) = 
     +                 SUM_CLASS_PEAKS(MO,2,IYR,TOTAL_CLASSES) +
     +                                PEAKS(MO,2)*CLASS_COIN_LOSS_FACTOR
         ENDIF
C     
      ENDDO
      RETURN
C
C***********************************************************************
      ENTRY FIND_AND_STORE_SYSTEM_PEAKS
C***********************************************************************
C
         DO YEAR = 1, FORECAST_GROWTH_YEARS
            CLASS_PEAK = 0.
            PEAK_MONTH = 0
            PEAK_DAY = 0
            DO MO = 1, 12
               IF(CLASS_PEAK > 
     +             MAX(SUM_CLASS_PEAKS(MO,1,YEAR,TOTAL_CLASSES),
     +                  SUM_CLASS_PEAKS(MO,2,YEAR,TOTAL_CLASSES))) CYCLE
               PEAK_MONTH = MO
               IF(SUM_CLASS_PEAKS(MO,1,YEAR,TOTAL_CLASSES) >
     +                    SUM_CLASS_PEAKS(MO,2,YEAR,TOTAL_CLASSES)) THEN
                  PEAK_DAY = 1
               ELSE
                  PEAK_DAY = 2
               ENDIF
               CLASS_PEAK = 
     +           SUM_CLASS_PEAKS(PEAK_MONTH,PEAK_DAY,YEAR,TOTAL_CLASSES)
            ENDDO
            PLANNING_PEAK = STORE_ANNUAL_PEAK_AND_MONTH(PEAK_MONTH,YEAR,
     +                                                       CLASS_PEAK)
            DO CLASS = 1 , MAX_LOAD_CLASSES
               CLASS_ANN_COIN_PEAK(CLASS,YEAR) = 
     +                   SUM_CLASS_PEAKS(PEAK_MONTH,PEAK_DAY,YEAR,CLASS)
            ENDDO
         ENDDO
      RETURN
C      
C***********************************************************************
      ENTRY DEALLOCATE_SUM_CLASS_PEAKS
C***********************************************************************
         IF(ALLOCATED(SUM_CLASS_PEAKS)) DEALLOCATE(SUM_CLASS_PEAKS)
      RETURN
C***********************************************************************
      ENTRY GET_CLASS_PEAK_FOR_CAPACITY(R_CLASS,R_YR,R_CLASS_PEAK)
C***********************************************************************
         R_CLASS_PEAK = CLASS_ANN_COIN_PEAK(R_CLASS,R_YR)         
      RETURN
C***********************************************************************
      ENTRY CALCULATE_SYSTEM_PEAKS(SYS_FC_DATA,IYR)
C***********************************************************************
C
         IF(IYR == 1 .AND. FIRST_CLASS) THEN
            IF(ALLOCATED(SUM_CLASS_PEAKS)) DEALLOCATE(SUM_CLASS_PEAKS)
            ALLOCATE(SUM_CLASS_PEAKS(12,2,FORECAST_GROWTH_YEARS,1))
            SUM_CLASS_PEAKS = 0.
            FIRST_CLASS = .FALSE.
         ENDIF
C
         PLANNING_PEAK = 0.
         SYS_GROWTH_TRIGGER_LEVEL = SYS_GROWTH_TRIGGER()
         DO MO = 1, 12
            MO_OFFSET = 4*(MO-1)
            IF(IYR == 1) THEN
               PEAKS(MO,1) = SYS_FC_DATA(MO_OFFSET+2)
               PEAKS(MO,2) = SYS_FC_DATA(MO_OFFSET+4)
            ELSE
               REAL_TEMP = SYS_FC_DATA(MO_OFFSET+2)
               IF(REAL_TEMP <= SYS_GROWTH_TRIGGER_LEVEL) THEN
                  PEAKS(MO,1) = (1. + REAL_TEMP/100.) * PEAKS(MO,1)
               ELSE
                  PEAKS(MO,1) = REAL_TEMP
               ENDIF
               REAL_TEMP = SYS_FC_DATA(MO_OFFSET+4)
               IF(REAL_TEMP <= SYS_GROWTH_TRIGGER_LEVEL) THEN
                  PEAKS(MO,2) = (1. + REAL_TEMP/100.) * PEAKS(MO,2)
               ELSE
                  PEAKS(MO,2) = REAL_TEMP
               ENDIF
            ENDIF
            IF(MAX(PEAKS(MO,1),PEAKS(MO,2)) >= PLANNING_PEAK) THEN
               PLANNING_PEAK = STORE_ANNUAL_PEAK_AND_MONTH(MO,IYR,
     +                                     MAX(PEAKS(MO,1),PEAKS(MO,2)))
            ENDIF
            SUM_CLASS_PEAKS(MO,1,IYR,1) = SUM_CLASS_PEAKS(MO,1,IYR,1) +
     +                                                       PEAKS(MO,1)
            SUM_CLASS_PEAKS(MO,2,IYR,1) = SUM_CLASS_PEAKS(MO,2,IYR,1) +
     +                                                       PEAKS(MO,2)
         ENDDO
      RETURN
C***********************************************************************
      ENTRY SET_FIRST_CLASS_TRUE
C***********************************************************************
        FIRST_CLASS = .TRUE.
      RETURN
CC***********************************************************************
      ENTRY GET_CLASS_MONTH_COIN_PEAK(R_CLASS,R_MONTH,R_YR,R_PEAK)
CC***********************************************************************
         R_PEAK = MAX(SUM_CLASS_PEAKS(R_MONTH,1,R_YR,R_CLASS),
     +                SUM_CLASS_PEAKS(R_MONTH,2,R_YR,R_CLASS))
      RETURN
      END

