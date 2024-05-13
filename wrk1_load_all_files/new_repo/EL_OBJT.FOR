!     ******************************************************************
!     EL_OBJT.FOR
!     Created: 10/19/02 3:15:03 PM
!     Author : msg
!     Last change: msg 2/7/2019 3:27:03 PM
!			MSG 6/6/2016 3:36:05 PM
!			MSG 6/6/2016 3:34:15 PM
!			MSG 6/5/2016 11:08:11 AM
!			MSG 6/5/2016 11:07:22 AM
!			MSG 6/4/2016 6:08:47 PM
!			MSG 5/22/2016 5:36:58 PM
!     ******************************************************************

      SUBROUTINE EL_OBJECT
      use end_routine, only: end_program, er_message
C
      INCLUDE 'SpinLib.MON'
      INCLUDE 'SIZECOM.MON'
      INCLUDE 'GLOBECOM.MON'
      INTEGER*2 I,IREC,INUNIT,DELETE,LRECL/355/ ! 120218
      INTEGER*2 R_ACTIVE_EL_RECORDS,
     +          ACTIVE_EL_OPTIONS,
     +          R_ACTIVE_EL_OPTIONS
      INTEGER*4 IOS,IOS_BASE
      INTEGER UNIT_NO
      CHARACTER*1 LOAD_TYPE,RESERVE_CONTRIBUTION
      CHARACTER*2 UNIT_TYPE,UNIT_TYPE_CATEGORY
      CHARACTER*5 HYDROFL,OVERLAY_FAMILY_NAME,SPECIAL_HYDRO_UNIT_ID
      CHARACTER*6 STATE_PROVINCE
      CHARACTER*20 UNIT_NAME,NEW_BUILD_STATUS
      CHARACTER*25 LONG_STATE_PROVINCE
      CHARACTER*30 DESC
      CHARACTER*256 FILE_NAME
      CHARACTER*256 BASE_FILE_DIRECTORY,OUTPUT_DIRECTORY
      LOGICAL*4 FILE_EXISTS
! DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER*1024 RECLN
! DECLARATION FOR /FOSSIL FILE/
      INTEGER*2 ONLIMO,ONLIYR,OFLIMO,OFLIYR,
     +          VRESCR,FIXED_COST_ESCALATOR,
     +          AI_CAPACITY_ESCALATOR,AI_ENERGY_ESCALATOR,
     +          AI_REMAINING_LIFE,
!     +          EL_UNIT_ID,
     +          ANNUAL_FIXED_COST_ESCALATOR,
     +          TRANSACTION_GROUP_ID,
     +          RPS_PROGRAM_NUMBER
      REAL AI_TAX_LIFE,
     +     AI_ADR_LIFE
! CHANGED FROM INTEGER*2 TO REAL 5/14/92 ADDED THE ID AS INTEGER*1
! END 5/14/92 CHANGES
      REAL FRAOWN,VARCST,
     +     FIXED_COST,AI_CAPACITY_RATE,
     +     AI_ENERGY_RATE,POOL_FRAC_OWN,
     +     ANNUAL_FIXED_COST
! DECLARATION FOR EMISSIONS ITEMS
      REAL P_SO2
! DECLARATION FOR /ENERGY LIMITED RESOURCE FILE/
! 5/20/92 ADDED EL GROUP CAPABILITY-GT
      REAL CAPMO(12),PSCAP,
     +      LATITUDE,
     +      LONGITUDE,
     +      MW_INSIDE_FENCE,
     +      RPS_CONTRIBUTION
      INTEGER*2 EL_GROUP,TEMP_ANNUAL_FIXED_COST_ESC
      REAL PSEFF,PLANNING_FACTOR,ENRGMO(12)
      INTEGER*8 TRANS_BUS_ID
      CHARACTER*1 EXPENSE_ASSIGNMENT,EXPENSE_COLLECTION,
! ADDED 3/10/93 FOR CLINTON TAX
     +            FUEL_TYPES(3),
     +            UNIT_ACTIVE,
     +            REPORT_THIS_UNIT,
     +            UTILITY_OWNED
      CHARACTER*6    BASECASE_PLANT_ID,
     +               BASECASE_UNIT_ID,
     +               BASECASE_MARKET_AREA_ID,
     +               BASECASE_TRANS_AREA_ID,
     +               BASECASE_PLANT_NERC_SUB_ID,
     +               BASECASE_PLANT_OWNER_ID,
     +               BASECASE_PRIMARY_MOVER
      CHARACTER*3 TEMP_EXPENSE_COLLECTION
      CHARACTER*20 FILE_TYPE/'Energy-limited Units'/
! ASSET CLASS VARIABLES
      INTEGER*2 ASSET_CLASS_NUM,ASSET_CLASS_VECTOR,
     +          INTRA_COMPANY_CLASS_ID
!
!
!
      LOGICAL*1   SAVED_EL_ASSET_INFO,STORE_EL_ASSET_INFO
      INTEGER*2   NUMBER_OF_HYDRO_CLASSES/0/,
     +	         MAX_HYDRO_CLASS_ID_NUM/0/,
     +            HYDRO_ASSET_CLASS_POINTER(1024)
!      SAVE        MAX_HYDRO_CLASS_ID_NUM
      INTEGER HESI_UNIT_ID_NUM,
     +        EL_UNIT_ID,
     +        POWERMAP_PLANT_ID
C
! MULTI-FILE VARIABLES 4/16/01
C
      INTEGER FILE_NUMBER,FILE_ID
      INTEGER (KIND=2), PARAMETER :: MAX_EL_FILES=20
      INTEGER*2 OVRLAY_PRT,OVERLAY_FILE_PTR
      CHARACTER*2 HYDRO_OL(0:MAX_EL_FILES-1)/MAX_EL_FILES*'BC'/
      INTEGER*2 ACTIVE_EL_RECORDS(0:MAX_EL_FILES-1)/MAX_EL_FILES*0/
      CHARACTER*5 EL_FILE_BASE_NAMES(0:MAX_EL_FILES-1)
      CHARACTER*5 EL_FILE_OVERLAY_NAMES(*),
     +            VOID_CHR,BASE_FILE_NAME
      LOGICAL*1 EL_OVERLAY_FILE_EXISTS(*)
      INTEGER*2 BASE_ACTIVE_EL_OPTIONS(0:MAX_EL_FILES-1)/MAX_EL_FILES*0/
      INTEGER*2 OVLY_ACTIVE_EL_OPTIONS(0:MAX_EL_FILES-1)/MAX_EL_FILES*0/
      CHARACTER*2 EL_FILE_CODES(0:MAX_EL_FILES-1)/
     +                              'EL','E1','E2','E3','E4','E5',
     +                              'E6','E7','E8','E9','E0',
     +                              'EB','ED','EE','EF','EG','EH','EI',
     +                              'EO','EP'/,
     +            FILE_CODE
      CHARACTER*5 EL_FILE_BINARY_NAMES(0:MAX_EL_FILES-1)/'HYDRO',
     +                                       'HYDR1',
     +                                       'HYDR2',
     +                                       'HYDR3',
     +                                       'HYDR4',
     +                                       'HYDR5',
     +                                       'HYDR6',
     +                                       'HYDR7',
     +                                       'HYDR8',
     +                                       'HYDR9',
     +                                       'HYDR0',
     +                                       'HYDRB',
     +                                       'HYDRD',
     +                                       'HYDRE',
     +                                       'HYDRF',
     +                                       'HYDRG',
     +                                       'HYDRH',
     +                                       'HYDRI',
     +                                       'HYDRJ',
     +                                       'HYDRK'/,
     +            BINARY_FILE_NAME
      LOGICAL ACTIVE_BASE_EL_FILES(0:MAX_EL_FILES-1)/
     +                                            MAX_EL_FILES*.FALSE./,
     +        ACTIVE_OVERLAY_EL_FILES(0:MAX_EL_FILES-1)/
     +                                            MAX_EL_FILES*.FALSE./,
     +        EL_FILES_ARE_ACTIVE/.FALSE./
      LOGICAL*1 R_EL_FILE_IS_ACTIVE
      LOGICAL*1 LAHEY_LF95
      CHARACTER*30 SCREEN_OUTPUT
!
! END DATA DECLARATIONS
!
C***********************************************************************
C
!          ROUTINE TO CONVERT METAFILE FILES TO DIRECT-ACESS BINARY
!          COPYRIGHT (C) 1983, 84, 85  M.S. GERBER & ASSOCIATES, INC.
C
C***********************************************************************
C
! CONVERT THE ENERGY-LIMITED RESOURCE FILE
      ENTRY EL_MAKEBIN
      VOID_CHR = HYDROFL(EL_FILE_BASE_NAMES)
      HYDRO_ASSET_CLASS_POINTER = 0
      DO FILE_ID = 0, MAX_EL_FILES-1
         BASE_FILE_NAME = EL_FILE_BASE_NAMES(FILE_ID)
         FILE_NAME = trim(BASE_FILE_DIRECTORY())//
     +                               EL_FILE_CODES(FILE_ID)//"B"//
     +                                    trim(BASE_FILE_NAME)//".DAT"
         INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
         IREC = 0
         ACTIVE_EL_OPTIONS = 0
!
         ACTIVE_BASE_EL_FILES(FILE_ID) = FILE_EXISTS
!
         IF(FILE_EXISTS) THEN
!
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//BASE_FILE_NAME
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
            EL_FILES_ARE_ACTIVE = .TRUE.
            OPEN(10,FILE=FILE_NAME)
            OPEN(11,FILE=trim(OUTPUT_DIRECTORY())//"BC"//
     +                    trim(EL_FILE_BINARY_NAMES(FILE_ID))//".BIN",
     +                      ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
            READ(10,*) DELETE
C
            DO
               ENRGMO = 0.
               CAPMO = 0.
               AI_CAPACITY_RATE = 0.
               AI_CAPACITY_ESCALATOR = 0.
               AI_ENERGY_RATE = 0.
               AI_ENERGY_ESCALATOR = 0.
               AI_REMAINING_LIFE = 0.
               P_SO2 = 0.
               POOL_FRAC_OWN = 100.
               ANNUAL_FIXED_COST = 0.
               EL_GROUP = 0
               FUEL_TYPES(1) = 'T'
! ASSET CLASS INITIALIZE
               AI_TAX_LIFE = 99.
               AI_ADR_LIFE = 99.
               ASSET_CLASS_NUM = 0
               ASSET_CLASS_VECTOR = 0
               INTRA_COMPANY_CLASS_ID = -2
C
               TEMP_ANNUAL_FIXED_COST_ESC = -9999 ! ADDED 5/8/96. GAT. FOR SRP.
!
               MAX_HYDRO_CLASS_ID_NUM = 0
C
               RESERVE_CONTRIBUTION = 'X' ! L = LOAD, C = CAPACITY, X = NOT INITIALIZED
!
               UNIT_ACTIVE = 'T'
               BASECASE_PLANT_ID = 'BLANK ' ! CHAR*6
               BASECASE_UNIT_ID = 'BLANK ' ! CHAR*6
               BASECASE_MARKET_AREA_ID = 'BLANK ' ! CHAR*6
               BASECASE_TRANS_AREA_ID = 'BLANK ' ! CHAR*6
               BASECASE_PLANT_NERC_SUB_ID = 'BLANK ' ! CHAR*6
               BASECASE_PLANT_OWNER_ID = 'BLANK ' ! CHAR*6
               TRANSACTION_GROUP_ID = 1
               BASECASE_PRIMARY_MOVER = 'WAT   '
               TRANS_BUS_ID = 0
               LATITUDE = 0.
               LONGITUDE = 0.
               MW_INSIDE_FENCE = 0.
!
               REPORT_THIS_UNIT = 'T'
               UTILITY_OWNED = 'U'
               STATE_PROVINCE = '#N/A'
               RPS_CONTRIBUTION = 0.0
               RPS_PROGRAM_NUMBER = 0
C
               DO
                  EL_UNIT_ID = IREC + 1
                  READ(10,1000,IOSTAT=IOS) RECLN
                  IF(IOS /=0) EXIT
                  IF(RECLN(1:1) == '7') EXIT
                  RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
!
                  SPECIAL_HYDRO_UNIT_ID = " "
!
                  READ(RECLN,*,ERR=200) DELETE,UNIT_NAME,LOAD_TYPE,
     +                  EXPENSE_ASSIGNMENT,
     +                  TEMP_EXPENSE_COLLECTION,FRAOWN,
     +                  ONLIMO,ONLIYR,OFLIMO,OFLIYR,PSEFF,PSCAP,VARCST,
     +                  VRESCR,FIXED_COST,FIXED_COST_ESCALATOR,
     +                  PLANNING_FACTOR,(CAPMO(I),ENRGMO(I),I=1,12),
     +                  DESC,POOL_FRAC_OWN,P_SO2,EL_GROUP,
     +                  FUEL_TYPES(1),
     +                  AI_CAPACITY_RATE,AI_CAPACITY_ESCALATOR,
     +                  AI_ENERGY_RATE,AI_ENERGY_ESCALATOR,
     +                  AI_REMAINING_LIFE,EL_UNIT_ID,
     +                  ANNUAL_FIXED_COST,
     +                  AI_TAX_LIFE,
     +                  AI_ADR_LIFE,
     +                  ASSET_CLASS_NUM,
     +                  ASSET_CLASS_VECTOR,
     +                  INTRA_COMPANY_CLASS_ID,
     +                  RESERVE_CONTRIBUTION,
     +                  TEMP_ANNUAL_FIXED_COST_ESC,
     +                  SPECIAL_HYDRO_UNIT_ID,
     +                  UNIT_ACTIVE,
     +                  BASECASE_PLANT_ID,
     +                  BASECASE_UNIT_ID,
     +                  BASECASE_MARKET_AREA_ID,
     +                  BASECASE_TRANS_AREA_ID,
     +                  BASECASE_PLANT_NERC_SUB_ID,
     +                  BASECASE_PLANT_OWNER_ID,
     +                  TRANSACTION_GROUP_ID,
     +                  UTILITY_OWNED,
     +                  REPORT_THIS_UNIT,
     +                  HESI_UNIT_ID_NUM,
     +                  POWERMAP_PLANT_ID,
     +                  BASECASE_PRIMARY_MOVER,
     +                  TRANS_BUS_ID,
     +                  LATITUDE,
     +                  LONGITUDE,
     +                  MW_INSIDE_FENCE, !
     +                  STATE_PROVINCE,  ! 78
     +                  RPS_CONTRIBUTION,
     +                  NEW_BUILD_STATUS,
     +                  LONG_STATE_PROVINCE,
     +                  UNIT_TYPE,
     +                  UNIT_TYPE_CATEGORY,
     +                  RPS_PROGRAM_NUMBER
!
!
!
                  CALL SET_ASSET_CLASSES(ASSET_CLASS_NUM,
     +                                   NUMBER_OF_HYDRO_CLASSES,
     +	                                MAX_HYDRO_CLASS_ID_NUM,
     +                                   HYDRO_ASSET_CLASS_POINTER)
!
!
!
                  IF(DELETE < 8 .AND.
     +                (UNIT_ACTIVE == 'T' .OR. UNIT_ACTIVE == 't')) THEN
                     IF(ONLIYR > LAST_STUDY_YEAR) ACTIVE_EL_OPTIONS = 1+
     +                                                 ACTIVE_EL_OPTIONS
                  ENDIF
                  IF(INDEX(TEMP_EXPENSE_COLLECTION,'BTL') /= 0) THEN
                     EXPENSE_COLLECTION = 'X'
                  ELSE
                     EXPENSE_COLLECTION = TEMP_EXPENSE_COLLECTION(1:1)
                  ENDIF
                  IF(TEMP_ANNUAL_FIXED_COST_ESC == -9999) THEN
                     ANNUAL_FIXED_COST_ESCALATOR = FIXED_COST_ESCALATOR
                  ELSE
                     ANNUAL_FIXED_COST_ESCALATOR =
     +                                        TEMP_ANNUAL_FIXED_COST_ESC
                  ENDIF
                  IREC = IREC + 1
                  WRITE(11,REC=IREC) DELETE,UNIT_NAME,LOAD_TYPE,
     +               EXPENSE_ASSIGNMENT,EXPENSE_COLLECTION,FRAOWN,
     +               ONLIMO,ONLIYR,OFLIMO,OFLIYR,PSEFF,PSCAP,
     +               VARCST,VRESCR,
     +               FIXED_COST,FIXED_COST_ESCALATOR,
     +               PLANNING_FACTOR,(CAPMO(I),ENRGMO(I),I=1,12),
     +               AI_CAPACITY_RATE,AI_CAPACITY_ESCALATOR,
     +               AI_ENERGY_RATE,AI_ENERGY_ESCALATOR,
     +               AI_REMAINING_LIFE,POOL_FRAC_OWN,P_SO2,EL_GROUP,
     +               FUEL_TYPES(1),EL_UNIT_ID,
     +               ANNUAL_FIXED_COST,
     +               AI_TAX_LIFE,
     +               AI_ADR_LIFE,
     +               ASSET_CLASS_NUM,
     +               ASSET_CLASS_VECTOR,
     +               INTRA_COMPANY_CLASS_ID,
     +               RESERVE_CONTRIBUTION,
     +               ANNUAL_FIXED_COST_ESCALATOR,
     +               SPECIAL_HYDRO_UNIT_ID,
     +               UNIT_ACTIVE,
     +               BASECASE_PLANT_ID,
     +               BASECASE_UNIT_ID,
     +               BASECASE_MARKET_AREA_ID,
     +               BASECASE_TRANS_AREA_ID,
     +               BASECASE_PLANT_NERC_SUB_ID,
     +               BASECASE_PLANT_OWNER_ID,
     +               TRANSACTION_GROUP_ID,
     +               UTILITY_OWNED,
     +               REPORT_THIS_UNIT,
     +               HESI_UNIT_ID_NUM,
     +               POWERMAP_PLANT_ID,
     +               BASECASE_PRIMARY_MOVER,
     +               TRANS_BUS_ID,
     +               LATITUDE,
     +               LONGITUDE,
     +               MW_INSIDE_FENCE,
     +               STATE_PROVINCE,
     +               RPS_CONTRIBUTION,
     +               NEW_BUILD_STATUS,
     +               LONG_STATE_PROVINCE,
     +               UNIT_TYPE,
     +               UNIT_TYPE_CATEGORY,
     +               RPS_PROGRAM_NUMBER
               ENDDO
               IF(IOS /= 0) EXIT
            ENDDO
            CLOSE(10)
!           ENDFILE(11)
            CLOSE(11)
C
         ELSE IF(INDEX(BASE_FILE_NAME,'NONE') == 0) THEN
            CALL STOP_NOFILE(FILE_TYPE,FILE_NAME)
         ENDIF
         ACTIVE_EL_RECORDS(FILE_ID) = IREC
         BASE_ACTIVE_EL_OPTIONS(FILE_ID) = ACTIVE_EL_OPTIONS
      ENDDO ! FILES LOOP
!
      SAVED_EL_ASSET_INFO = STORE_EL_ASSET_INFO(MAX_HYDRO_CLASS_ID_NUM,
     +                                        NUMBER_OF_HYDRO_CLASSES,
     +                                        HYDRO_ASSET_CLASS_POINTER)
!
      RETURN

C***********************************************************************
C
!          ROUTINE TO CREATE OVERLAY FILES
!          COPYRIGHT (C) 1984-88  M.S. GERBER & ASSOCIATES, INC.
!          COPYRIGHT (C) 1991-92  M.S. GERBER & ASSOCIATES, INC.
C
C***********************************************************************
C
! OVERLAY THE ENERGY-LIMITED RESOURCE FILE
      ENTRY EL_MAKEOVL(EL_FILE_OVERLAY_NAMES,EL_OVERLAY_FILE_EXISTS)
C
      OVRLAY_PRT = 15
      HYDRO_ASSET_CLASS_POINTER = 0
C
      DO FILE_ID = 0, MAX_EL_FILES-1

         OVERLAY_FILE_PTR = OVRLAY_PRT+FILE_ID
         IF(FILE_ID == 0) THEN
            OVRLAY_PRT = 136
         ELSEIF(FILE_ID == 14) THEN
            OVRLAY_PRT = 152
         ELSEIF(FILE_ID == 16) THEN
            OVRLAY_PRT = 194
         ENDIF
         IF(.NOT. ACTIVE_BASE_EL_FILES(FILE_ID)) CYCLE
         IF(.NOT. EL_OVERLAY_FILE_EXISTS(OVERLAY_FILE_PTR)) CYCLE
         FILE_CODE = EL_FILE_CODES(FILE_ID)
         BINARY_FILE_NAME = EL_FILE_BINARY_NAMES(FILE_ID)
         OVERLAY_FAMILY_NAME = EL_FILE_OVERLAY_NAMES(OVERLAY_FILE_PTR)
         FILE_NAME=trim(OUTPUT_DIRECTORY())//FILE_CODE//"O"//
     +                               trim(OVERLAY_FAMILY_NAME)//".DAT"
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         OPEN(10,FILE=FILE_NAME)
         READ(10,*) DELETE
         INUNIT = 12
         IF(HYDRO_OL(FILE_ID) == 'BC') THEN
            OPEN(11,FILE=trim(OUTPUT_DIRECTORY())//"BC"//
     +                         trim(BINARY_FILE_NAME)//".BIN",
     +                      ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
            INUNIT = 11
         ENDIF
         OPEN(12,FILE=trim(OUTPUT_DIRECTORY())//"OL"//
     +                         trim(BINARY_FILE_NAME)//".BIN",
     +                      ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
         IREC = 0
         ACTIVE_EL_OPTIONS = 0
C
         DO
            DO
               READ(10,1000,IOSTAT=IOS) RECLN
               IF(RECLN(1:1) == '7') EXIT
               IREC = IREC + 1
               READ(INUNIT,REC=IREC,IOSTAT=IOS_BASE) DELETE,UNIT_NAME,
     +            LOAD_TYPE,EXPENSE_ASSIGNMENT,
     +            EXPENSE_COLLECTION,FRAOWN,
     +            ONLIMO,ONLIYR,OFLIMO,OFLIYR,PSEFF,PSCAP,VARCST,VRESCR,
     +            FIXED_COST,FIXED_COST_ESCALATOR,
     +            PLANNING_FACTOR,(CAPMO(I),ENRGMO(I),I=1,12),
     +            AI_CAPACITY_RATE,AI_CAPACITY_ESCALATOR,
     +            AI_ENERGY_RATE,AI_ENERGY_ESCALATOR,AI_REMAINING_LIFE,
     +            POOL_FRAC_OWN,P_SO2,EL_GROUP,FUEL_TYPES(1),EL_UNIT_ID,
     +            ANNUAL_FIXED_COST,
     +            AI_TAX_LIFE,
     +            AI_ADR_LIFE,
     +            ASSET_CLASS_NUM,
     +            ASSET_CLASS_VECTOR,
     +            INTRA_COMPANY_CLASS_ID,
     +            RESERVE_CONTRIBUTION,
     +            ANNUAL_FIXED_COST_ESCALATOR,
     +            SPECIAL_HYDRO_UNIT_ID,
     +            UNIT_ACTIVE,
     +            BASECASE_PLANT_ID,
     +            BASECASE_UNIT_ID,
     +            BASECASE_MARKET_AREA_ID,
     +            BASECASE_TRANS_AREA_ID,
     +            BASECASE_PLANT_NERC_SUB_ID,
     +            BASECASE_PLANT_OWNER_ID,
     +            TRANSACTION_GROUP_ID,
     +            UTILITY_OWNED,
     +            REPORT_THIS_UNIT,
     +            HESI_UNIT_ID_NUM,
     +            POWERMAP_PLANT_ID,
     +            BASECASE_PRIMARY_MOVER,
     +            TRANS_BUS_ID,
     +            LATITUDE,
     +            LONGITUDE,
     +            MW_INSIDE_FENCE,
     +            STATE_PROVINCE,
     +            RPS_CONTRIBUTION,
     +            NEW_BUILD_STATUS,
     +            LONG_STATE_PROVINCE,
     +            UNIT_TYPE,
     +            UNIT_TYPE_CATEGORY,
     +            RPS_PROGRAM_NUMBER
               IF(IOS_BASE /= 0) EXIT
               IF(IOS == 0) THEN
                  RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
                  TEMP_EXPENSE_COLLECTION = EXPENSE_COLLECTION
                  READ(RECLN,*,ERR=200)  DELETE,UNIT_NAME,LOAD_TYPE,
     +                  EXPENSE_ASSIGNMENT,
     +                  TEMP_EXPENSE_COLLECTION,FRAOWN,
     +                  ONLIMO,ONLIYR,OFLIMO,OFLIYR,PSEFF,PSCAP,VARCST,
     +                  VRESCR,FIXED_COST,FIXED_COST_ESCALATOR,
     +                  PLANNING_FACTOR,(CAPMO(I),ENRGMO(I),I=1,12),
     +                  DESC,POOL_FRAC_OWN,P_SO2,EL_GROUP,
     +                  FUEL_TYPES(1),
     +                  AI_CAPACITY_RATE,AI_CAPACITY_ESCALATOR,
     +                  AI_ENERGY_RATE,AI_ENERGY_ESCALATOR,
     +                  AI_REMAINING_LIFE,EL_UNIT_ID,
     +                  ANNUAL_FIXED_COST,
     +                  AI_TAX_LIFE,
     +                  AI_ADR_LIFE,
     +                  ASSET_CLASS_NUM,
     +                  ASSET_CLASS_VECTOR,
     +                  INTRA_COMPANY_CLASS_ID,
     +                  RESERVE_CONTRIBUTION,
     +                  ANNUAL_FIXED_COST_ESCALATOR,
     +                  SPECIAL_HYDRO_UNIT_ID,
     +                  UNIT_ACTIVE,
     +                  BASECASE_PLANT_ID,
     +                  BASECASE_UNIT_ID,
     +                  BASECASE_MARKET_AREA_ID,
     +                  BASECASE_TRANS_AREA_ID,
     +                  BASECASE_PLANT_NERC_SUB_ID,
     +                  BASECASE_PLANT_OWNER_ID,
     +                  TRANSACTION_GROUP_ID,
     +                  UTILITY_OWNED,
     +                  REPORT_THIS_UNIT,
     +                  HESI_UNIT_ID_NUM,
     +                  POWERMAP_PLANT_ID,
     +                  BASECASE_PRIMARY_MOVER,
     +                  TRANS_BUS_ID,
     +                  LATITUDE,
     +                  LONGITUDE,
     +                  MW_INSIDE_FENCE,
     +                  STATE_PROVINCE,
     +                  RPS_CONTRIBUTION,
     +                  NEW_BUILD_STATUS,
     +                  LONG_STATE_PROVINCE,
     +                  UNIT_TYPE,
     +                  UNIT_TYPE_CATEGORY,
     +                  RPS_PROGRAM_NUMBER
                  IF(INDEX(TEMP_EXPENSE_COLLECTION,'BTL') /= 0) THEN
                     EXPENSE_COLLECTION = 'X'
                  ELSE
                     EXPENSE_COLLECTION = TEMP_EXPENSE_COLLECTION(1:1)
                  ENDIF
               ENDIF
!
!
!
               CALL SET_ASSET_CLASSES(ASSET_CLASS_NUM,
     +                                NUMBER_OF_HYDRO_CLASSES,
     +   	                          MAX_HYDRO_CLASS_ID_NUM,
     +                                HYDRO_ASSET_CLASS_POINTER)
!
!
!
               IF(DELETE < 8 .AND.
     +                (UNIT_ACTIVE == 'T' .OR. UNIT_ACTIVE == 't')) THEN
                  IF(ONLIYR > LAST_STUDY_YEAR) ACTIVE_EL_OPTIONS = 1 +
     +                                                 ACTIVE_EL_OPTIONS
               ENDIF
               WRITE(12,REC=IREC) DELETE,UNIT_NAME,LOAD_TYPE,
     +            EXPENSE_ASSIGNMENT,EXPENSE_COLLECTION,FRAOWN,
     +            ONLIMO,ONLIYR,OFLIMO,OFLIYR,PSEFF,PSCAP,VARCST,VRESCR,
     +            FIXED_COST,FIXED_COST_ESCALATOR,
     +            PLANNING_FACTOR,(CAPMO(I),ENRGMO(I),I=1,12),
     +            AI_CAPACITY_RATE,AI_CAPACITY_ESCALATOR,
     +            AI_ENERGY_RATE,AI_ENERGY_ESCALATOR,AI_REMAINING_LIFE,
     +            POOL_FRAC_OWN,P_SO2,EL_GROUP,
     +            FUEL_TYPES(1),EL_UNIT_ID,
     +            ANNUAL_FIXED_COST,
     +            AI_TAX_LIFE,
     +            AI_ADR_LIFE,
     +            ASSET_CLASS_NUM,
     +            ASSET_CLASS_VECTOR,
     +            INTRA_COMPANY_CLASS_ID,
     +            RESERVE_CONTRIBUTION,
     +            ANNUAL_FIXED_COST_ESCALATOR,
     +            SPECIAL_HYDRO_UNIT_ID,
     +            UNIT_ACTIVE,
     +            BASECASE_PLANT_ID,
     +            BASECASE_UNIT_ID,
     +            BASECASE_MARKET_AREA_ID,
     +            BASECASE_TRANS_AREA_ID,
     +            BASECASE_PLANT_NERC_SUB_ID,
     +            BASECASE_PLANT_OWNER_ID,
     +            TRANSACTION_GROUP_ID,
     +            UTILITY_OWNED,
     +            REPORT_THIS_UNIT,
     +            HESI_UNIT_ID_NUM,
     +            POWERMAP_PLANT_ID,
     +            BASECASE_PRIMARY_MOVER,
     +            TRANS_BUS_ID,
     +            LATITUDE,
     +            LONGITUDE,
     +            MW_INSIDE_FENCE,
     +            STATE_PROVINCE,
     +            RPS_CONTRIBUTION,
     +            NEW_BUILD_STATUS,
     +            LONG_STATE_PROVINCE,
     +            UNIT_TYPE,
     +            UNIT_TYPE_CATEGORY,
     +            RPS_PROGRAM_NUMBER
            ENDDO
            IF(IOS_BASE /= 0) EXIT
         ENDDO
         CLOSE(10)
         CLOSE(12)
         IF(HYDRO_OL(FILE_ID) == 'BC') CLOSE(11)
         HYDRO_OL(FILE_ID) = 'OL'
!
         OVLY_ACTIVE_EL_OPTIONS(FILE_ID) = ACTIVE_EL_OPTIONS
      ENDDO ! FILE LOOP
      SAVED_EL_ASSET_INFO = STORE_EL_ASSET_INFO(MAX_HYDRO_CLASS_ID_NUM,
     +                                          NUMBER_OF_HYDRO_CLASSES,
     +                                        HYDRO_ASSET_CLASS_POINTER)
!
C
      RETURN
C
  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from EL_OBJT SIID85'
      call end_program(er_message)
C
C***********************************************************************
      ENTRY RESET_HYDRO_OL
C***********************************************************************
!         DO FILE_ID = 0, MAX_EL_FILES-1
!            HYDRO_OL(FILE_ID) = 'BC'
!            OVLY_ACTIVE_EL_OPTIONS(FILE_ID) = 0
!         ENDDO
          HYDRO_OL= 'BC'
          OVLY_ACTIVE_EL_OPTIONS = 0
      RETURN
C
C***********************************************************************
      ENTRY EL_FILE_ACTIVE(R_EL_FILE_IS_ACTIVE,R_ACTIVE_EL_OPTIONS)
C***********************************************************************
         R_ACTIVE_EL_OPTIONS = 0.
         DO FILE_ID = 0, MAX_EL_FILES-1
            R_ACTIVE_EL_OPTIONS = R_ACTIVE_EL_OPTIONS
     +                            + MAX(OVLY_ACTIVE_EL_OPTIONS(FILE_ID),
     +                                  BASE_ACTIVE_EL_OPTIONS(FILE_ID))
         ENDDO
         R_EL_FILE_IS_ACTIVE = EL_FILES_ARE_ACTIVE
      RETURN
C***********************************************************************
      ENTRY OPEN_EL_FILE(UNIT_NO,R_ACTIVE_EL_RECORDS,FILE_NUMBER)
C***********************************************************************
         IF(ACTIVE_EL_RECORDS(FILE_NUMBER) > 0) THEN
            OPEN(UNIT_NO,FILE=trim(OUTPUT_DIRECTORY())//
     +             HYDRO_OL(FILE_NUMBER)//
     +                trim(EL_FILE_BINARY_NAMES(FILE_NUMBER))//".BIN",
     +                      ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
         ENDIF
         R_ACTIVE_EL_RECORDS = MAX(ACTIVE_EL_RECORDS(FILE_NUMBER),0)
      RETURN
C
 1000 FORMAT(A)
! 1010 FORMAT('&',A)
      END
C***********************************************************************
C
!          SUBROUTINE TO READ ENERGY LIMITED BINARY DATA FILES
!          COPYRIGHT (C) 1985  M.S. GERBER & ASSOCIATES, INC.
!          ALL RIGHTS RESERVED
C
C***********************************************************************
C
!     2/14/90: THE ROUTINE HAS BEEN CHANGED TO FIND THE AMOUNT OF
!     ENERGY LIMITED CAPACITY CONTRIBUTING TO CAPACITY PLANNING,
!     AND TO IDENTIFY CAPACITY POINTER VARIABLES.
C
C***********************************************************************
      RECURSIVE FUNCTION EL_UNITS_READ()
      use end_routine, only: end_program, er_message
      use prim_mover_idx
C***********************************************************************
C
      INCLUDE 'SpinLib.MON'
      SAVE
      INCLUDE 'SIZECOM.MON'
C
      INTEGER*2 I,J,IREC,DELETE,PLANNING_MO,
     +          EL_RECORDS,R_YEAR,
     +          MAX_EL_ID_NUM/0/,
     +          EL_UNITS_READ,
     +          BASE_EL_UNITS/0/,
     +          BASE_PLUS_HARDWIRED_EL_UNITS/0/,
     +          AVAILABLE_EL_UNITS/0/,
     +          INCREMENT_AVAILABLE_EL_UNITS,
     +          RESET_EL_UNITS,
     +          EL_PLANNING_ADDITIONS,
     +          MARKET_AREA_LOOKUP
      INTEGER*4 IOS,R_CAP_TYPE
      INTEGER HESI_UNIT_ID_NUM(MAX_EL_UNITS),
     +        POWERMAP_PLANT_ID(MAX_EL_UNITS),
     +         EL_UNIT_ID(:),
     +         EL_RESOURCE_ID,
     +         EL_OPTIONS_ID_NUM(:),
     +         GET_HESI_EL_UNIT_ID_NUM
      INTEGER*2 ON_LINE_MONTH(:),ON_LINE_YEAR(:),
     +          OFF_LINE_MONTH(:),OFF_LINE_YEAR(:),
     +          PRIMARY_MOVER_INDEX(:),
     +          RPS_PM_INDEX(:),
     +          INCREMENT_HARDWIRED_EL_UNITS,
     +          RETURN_EL_UNITS_B4_ADDITIONS
      INTEGER*2 EL_UNIT_NO
      CHARACTER*20 MONTH_NAME
      ALLOCATABLE :: ON_LINE_MONTH,ON_LINE_YEAR,
     +               OFF_LINE_MONTH,OFF_LINE_YEAR,
     +               EL_UNIT_ID,
     +               PRIMARY_MOVER_INDEX,
     +               RPS_PM_INDEX
C
      INTEGER*2 INSTALLED_POINTER/0/,
     +          TEMP_RETIRED_UNIT_NO(:),
     +          TEMP_RETIRED_OFF_LINE(:),
     +          EL_CAPACITY_TEMP_RETIRE_UNIT,
     +          UNIT_NO,HYDRO_TRANS_GROUP
      LOGICAL*1 SUBTRACT_IT,
     +          GET_EL_BASECASE_MARKET_ID,
     +          NO_POINTERS
C
      CHARACTER*1 RESERVE_CONTRIBUTION(MAX_EL_UNITS)
      CHARACTER*5 MARKET_ID
C
      ALLOCATABLE :: TEMP_RETIRED_UNIT_NO,
     +               TEMP_RETIRED_OFF_LINE
C
!     VARIABLES FOR CAPACITY PLANNING
C
      INTEGER*2 PEAK_MONTH,RETURN_MAX_EL_CLASS_NUM
      INTEGER*2 YEAR_START,YEAR_END,PT_YR,ST,R_TG,R_ST,R_ST_C
      REAL EL_ANN_CAP(:,:,:),
     +      EL_TG_CAP(:,:,:),
     +      GET_EL_TG_CAP,
     +      EN_MW,EL_PLANNING_CAPACITY,
     +      PEAK_ADJ_OFF_SYSTEM_SALES,GET_EL_MW_FROM_POINTR,
     +      OLD_EL_PLANNING_CAPACITY,NEW_EL_PLANNING_CAPACITY
      REAL PEAK_ADJ_2_OFF_SYSTEM_SALES(:)
      ALLOCATABLE :: PEAK_ADJ_2_OFF_SYSTEM_SALES,
     +               EL_ANN_CAP,
     +               EL_TG_CAP
      REAL VOID_REAL,
     +     R_EL_CAP_PLANNING_ADDITIONS,
     +     EL_CAPACITY_PLANNING_REMOVALS,
     +     EL_CAPACITY_PLANNING_REMOVAL_10,
     +     FIRST_YEAR_CAPACITY,GET_VAR
      INTEGER*2 R_UNIT_NO,RETURN_EL_UNIT_ON_LINE_MONTH
      INTEGER*2 EL_OPTIONS/0/,EL_OPTIONS_POINTR(:),
     +          EL_OPTIONS_COUNTR/0/,EL_ARRAY_POS/0/
      ALLOCATABLE :: EL_OPTIONS_ID_NUM,EL_OPTIONS_POINTR
      REAL ADD_NEW_EL_UNIT
      INTEGER*2 NEW_EL_UNIT_ID,UNIT_ON_LINE_YEAR,
     +          OPERATION_LIFE,R_ON_LINE_MONTH,
     +          SET_AI_EL_REMAINING_LIFE,AI_REMAINING_LIFE,TRANS_ID
      CHARACTER*4 YEAR_CHR,HYDRO_UNITS_CHR*3
C
      INCLUDE 'PRODCOM.MON'
      INCLUDE 'PROD2COM.MON'
C
      LOGICAL*1 EL_RESET_PRICES,
     +          EL_OPTION_CHECK,
     +          TRANS_GROUP_ACTIVE_SWITCH,
     +          IN_ACTIVE_HYDRO_MARKET_AREA,
     +          RUN_TRANSACT,YES_RUN_TRANSACT,
     +          REPORT_THIS_EL_UNIT,
     +          TG_FILE_EXISTS,TF_FILE_EXISTS,
     +          TRANSACT_ACTIVE_IN_ENDPOINT,
     +          GET_PW_EL_UNIT_TEXT_FIELDS,
     +          ZONAL_LEVEL_MARKET,YES_ZONAL_LEVEL_MARKET
      REAL PSEFF_SAVE(MAX_EL_UNITS),
     +     VAROM_SAVE(MAX_EL_UNITS),
     +     HYDRO_FIXED_COST_SAVE(MAX_EL_UNITS),
     +     EL_AI_CAPACITY_RATE_SAVE(MAX_EL_UNITS),
     +     EL_AI_ENERGY_RATE_SAVE(MAX_EL_UNITS),
     +     HYDRO_ANNUAL_FIXED_COST_SAVE(MAX_EL_UNITS),
     +     AI_EL_REMAINING_LIFE_SAVE(MAX_EL_UNITS),
     +     R_RPS_CONTRIB
C
      LOGICAL*1 PUT_HYDRO_RPS_ENRG_CAP
! 121618
      LOGICAL*1 PUT_RPS_ENRG_CAP,
     +          TEMP_L1
      INTEGER*2 TEMP_RPS_NO,
     +          TEMP_PM
      REAL*4    R_ENRG,
     +          R_CAP,
     +          TEMP_ENRG,
     +          TEMP_CAP
C
!
! ASSET ALLOCATION STUFF
C
      LOGICAL*1 ASSET_CLASS_ACTIVE,STORE_EL_ASSET_INFO,
     +          GET_HYDRO_RPS_DATA
      INTEGER*2 ASSET_CLASS_NUM(MAX_EL_UNITS),
     +          ASSET_CLASS_VECTOR(MAX_EL_UNITS),
     +          INTRA_COMPANY_CLASS_ID(MAX_EL_UNITS),
     +          UNIQUE_EL_ID(MAX_EL_UNITS)/MAX_EL_UNITS*0/,
     +          UNIQUE_ID_NUM,
     +          UNIQUE_REPORT_VALUE_FOR_EL_UNIT,
     +          R_ASSET_CLASS_NUM,
     +          R_ASSET_CLASS_VECTOR,
     +          R_PM,R_ST_TG
      INTEGER*2 NUMBER_OF_HYDRO_CLASSES/0/,
     +          S_NUMBER_OF_HYDRO_CLASSES,
     +	       MAX_HYDRO_CLASS_ID_NUM,
     +          S_MAX_HYDRO_CLASS_ID_NUM,
     +          HYDRO_ASSET_CLASS_POINTER(1024),
     +          S_HYDRO_ASSET_CLASS_POINTER(1024),
     +          TRANSACTION_GROUP_ID(:),
     +          GET_BELONGS_TO_GROUP,
     +          UPPER_TRANS_GROUP,
     +          GET_NUMBER_OF_ACTIVE_GROUPS,
     +          TG,
     +          GET_TRANS_GROUP_POSITION,
     +          GET_HYDRO_PRIMARY_M0VER
!     +          HYDRO_ASSET_CLASS_POINTER(:)
!      ALLOCATABLE :: HYDRO_ASSET_CLASS_POINTER
      INTEGER*2 RETURN_NUM_OF_HYDRO_CLASSES,
     +          R_MAX_HYDRO_CLASS_NUM,
     +          RETURN_HYDRO_CLASS_POINTER,
     +          R_HYDRO_CLASS_POINTERS(*),
     +          ST_TG,STATE_ID_LOOKUP,
     +          State_TG_Index(MAX_EL_UNITS),
     +          State_Index(MAX_EL_UNITS)
!      INTEGER*2 ASSET_CLASS_NUM(:),ASSET_CLASS_VECTOR(:)
!      ALLOCATABLE :: ASSET_CLASS_NUM,ASSET_CLASS_VECTOR
!      SAVE ASSET_CLASS_NUM,ASSET_CLASS_VECTOR
C
!
! RDI DATA
!
      CHARACTER*20      RDI_COMPANY_NAME,
     +                  RDI_NERC_REGION,
     +                  RDI_SUB_REGION,
     +                  RDI_PM_ABBREV,
     +                  RDI_PRIME_FUEL
      CHARACTER*30      RDI_DESC
      CHARACTER*2 UNIT_TYPE,UNIT_TYPE_CATEGORY
      CHARACTER*20 NEW_BUILD_STATUS
      CHARACTER*25 LONG_STATE_PROVINCE
      INTEGER*2   RDI_RECORDS,RDI_EL_RECORDS,
     +            RDI_IN_UNIT,RDI_OUT_UNIT,
     +            RDI_REC,RDI_EL_REC,
     +            RPS_PROGRAM_NUMBER(MAX_EL_UNITS)
      REAL*4      RDI_DEMONSTRATED_CAPACITY_MW,
     +            RDI_INCREMENTAL_FUEL_$MWH,
     +            RDI_AVERAGE_HEAT_RATE,
     +            RDI_FUEL_COST_$MWH,
     +            LATITUDE(:),
     +            LONGITUDE(:),
     +            MW_INSIDE_FENCE,
     +            TEMP_R,
     +            RPS_CONTRIBUTION(MAX_EL_UNITS),
     +            LOCAL_RPS_CONTRIB_PERCENT,
     +            ESCALATED_MONTHLY_VALUE
!     +            MONTH_GENERATION(12),
!     +            MONTH_CAPACITY(12)
      INTEGER*8 TRANS_BUS_ID(MAX_EL_UNITS)
      LOGICAL*4   RDI_FILE_EXISTS
      CHARACTER*1 UNIT_ACTIVE,
     +            REPORT_THIS_UNIT(:),
     +            UTILITY_OWNED(:)
      CHARACTER*6
     +        BASECASE_PLANT_ID(:),
     +        BASECASE_UNIT_ID(:),
     +        BASECASE_MARKET_AREA_ID(:),
     +        BASECASE_TRANS_AREA_ID(:),
     +        BASECASE_PLANT_NERC_SUB_ID(:),
     +        BASECASE_PLANT_OWNER_ID(:),
     +        BASECASE_PRIMARY_MOVER_STR,
     +        R_MARKET_ID,
     +        R_EIA_PLANT_CODE,
     +        STATE_PROVINCE,
     +        TEMP_CHAR6
      ALLOCATABLE ::
     +        LATITUDE,
     +        LONGITUDE,
     +        BASECASE_PLANT_ID,
     +        BASECASE_UNIT_ID,
     +        BASECASE_MARKET_AREA_ID,
     +        BASECASE_TRANS_AREA_ID,
     +        BASECASE_PLANT_NERC_SUB_ID,
     +        BASECASE_PLANT_OWNER_ID,
     +        TRANSACTION_GROUP_ID,
     +        REPORT_THIS_UNIT,
     +        UTILITY_OWNED
C
! MULTI-FILE VARIABLES
C
       INTEGER MAX_EL_FILES
       PARAMETER (MAX_EL_FILES=20)
       LOGICAL*1 FIRST_PASS,EL_FILE_IS_ACTIVE
       INTEGER FILE_ID

!
! END OF DATA DECLARATIONS
!
!
      CALL DOES_RDI_FILE_EXIST(RDI_FILE_EXISTS)
      IF(RDI_FILE_EXISTS) THEN
         CALL GET_NUM_OF_RDI_EL_RECORDS(RDI_RECORDS,RDI_EL_RECORDS)
         IF(RDI_EL_RECORDS > 0) THEN
            CALL OPEN_RDI_EL_FILES(RDI_IN_UNIT,RDI_OUT_UNIT)
            WRITE(RDI_OUT_UNIT,*) "9, "
         ENDIF
      ELSE
         RDI_RECORDS = 0
         RDI_EL_RECORDS = 0
      ENDIF
C
      CALL EL_FILE_ACTIVE(EL_FILE_IS_ACTIVE,EL_OPTIONS)
      IF(.NOT. EL_FILE_IS_ACTIVE .AND. RDI_EL_RECORDS < 1) THEN
         BASE_EL_UNITS  = 0
         BASE_PLUS_HARDWIRED_EL_UNITS = 0
         EL_UNITS_READ = 0
         IF(ALLOCATED(PEAK_ADJ_2_OFF_SYSTEM_SALES))
     +                           DEALLOCATE(PEAK_ADJ_2_OFF_SYSTEM_SALES)
         ALLOCATE(PEAK_ADJ_2_OFF_SYSTEM_SALES(STUDY_PERIOD))
         PEAK_ADJ_2_OFF_SYSTEM_SALES = 0.
         RETURN
      ENDIF
!
      UPPER_TRANS_GROUP = MAX(GET_NUMBER_OF_ACTIVE_GROUPS(),1)
!
      IF(ALLOCATED(ON_LINE_MONTH)) THEN
         DEALLOCATE(ON_LINE_MONTH,ON_LINE_YEAR,
     +        OFF_LINE_MONTH,OFF_LINE_YEAR,
     +        PRIMARY_MOVER_INDEX,
     +        RPS_PM_INDEX,
     +        EL_UNIT_ID,EL_ANN_CAP,
     +        EL_TG_CAP,
     +        LATITUDE,
     +        LONGITUDE,
     +        BASECASE_PLANT_ID,
     +        BASECASE_UNIT_ID,
     +        BASECASE_MARKET_AREA_ID,
     +        BASECASE_TRANS_AREA_ID,
     +        BASECASE_PLANT_NERC_SUB_ID,
     +        BASECASE_PLANT_OWNER_ID,
     +        TRANSACTION_GROUP_ID,
     +        REPORT_THIS_UNIT,
     +        UTILITY_OWNED)

      ENDIF
      ALLOCATE(ON_LINE_MONTH(MAX_EL_UNITS),
     +        ON_LINE_YEAR(MAX_EL_UNITS),
     +        OFF_LINE_MONTH(MAX_EL_UNITS),
     +        OFF_LINE_YEAR(MAX_EL_UNITS),
     +        PRIMARY_MOVER_INDEX(MAX_EL_UNITS),
     +        RPS_PM_INDEX(MAX_EL_UNITS),
     +        EL_UNIT_ID(MAX_EL_UNITS),
     +        EL_ANN_CAP(3,STUDY_PERIOD,2),
     +        EL_TG_CAP(0:MAX(1,UPPER_TRANS_GROUP),STUDY_PERIOD,2),
     +        LATITUDE(MAX_EL_UNITS),
     +        LONGITUDE(MAX_EL_UNITS),
     +        BASECASE_PLANT_ID(MAX_EL_UNITS),
     +        BASECASE_UNIT_ID(MAX_EL_UNITS),
     +        BASECASE_MARKET_AREA_ID(MAX_EL_UNITS),
     +        BASECASE_TRANS_AREA_ID(MAX_EL_UNITS),
     +        BASECASE_PLANT_NERC_SUB_ID(MAX_EL_UNITS),
     +        BASECASE_PLANT_OWNER_ID(MAX_EL_UNITS),
     +        TRANSACTION_GROUP_ID(MAX_EL_UNITS),
     +        REPORT_THIS_UNIT(MAX_EL_UNITS),
     +        UTILITY_OWNED(MAX_EL_UNITS))
!     +         HYDRO_ASSET_CLASS_POINTER(1024))
C
      ASSET_CLASS_NUM = 0
      INTRA_COMPANY_CLASS_ID = -2
      ASSET_CLASS_VECTOR = 0
      HYDRO_ASSET_CLASS_POINTER = 0
      PRIMARY_MOVER_INDEX = 0
      RPS_PM_INDEX = 0
C
      EL_TG_CAP = 0.
C
      IF(ALLOCATED(EL_OPTIONS_ID_NUM)) THEN
         DEALLOCATE(EL_OPTIONS_ID_NUM,EL_OPTIONS_POINTR)
      ENDIF
      IF(EL_OPTIONS > 0) THEN
         ALLOCATE(EL_OPTIONS_ID_NUM(EL_OPTIONS),
     +            EL_OPTIONS_POINTR(EL_OPTIONS))
         EL_OPTIONS_ID_NUM = 0
         EL_OPTIONS_POINTR = 0
      ENDIF
C
      IF(ALLOCATED(PEAK_ADJ_2_OFF_SYSTEM_SALES))
     +                           DEALLOCATE(PEAK_ADJ_2_OFF_SYSTEM_SALES)
      ALLOCATE(PEAK_ADJ_2_OFF_SYSTEM_SALES(STUDY_PERIOD))
      PEAK_ADJ_2_OFF_SYSTEM_SALES = 0.
C
! NEW DEFINITION 5/23/00.
!
!      RUN_TRANSACT = YES_RUN_TRANSACT()
      CALL DOES_TG_FILE_EXIST(TG_FILE_EXISTS)
      CALL DOES_TF_FILE_EXIST(TF_FILE_EXISTS)
      RUN_TRANSACT = TG_FILE_EXISTS .AND. TF_FILE_EXISTS
!    +               .AND. TRANSACT_ACTIVE_IN_ENDPOINT()
!
!
      ZONAL_LEVEL_MARKET = YES_ZONAL_LEVEL_MARKET()
!
!
      EL_OPTIONS_COUNTR = 0
      MAX_EL_ID_NUM = 0
      NUMBER_OF_HYDRO_CLASSES = 0
      MAX_HYDRO_CLASS_ID_NUM = 0
      EL_ANN_CAP = 0.
      FIRST_PASS = .TRUE.
      UNIQUE_ID_NUM = 0
      State_TG_Index = 0
      State_Index = 0
      I = 1
      DO FILE_ID = 0, MAX_EL_FILES-1
C
         CALL OPEN_EL_FILE(10,EL_RECORDS,FILE_ID)
         IREC = 0
         RDI_REC = 0
         RDI_EL_REC = 0
         DOWHILE (IREC < EL_RECORDS .OR. RDI_EL_REC < RDI_EL_RECORDS)
            IF(IREC < EL_RECORDS) THEN
               IREC = IREC + 1
               READ(10,REC=IREC,IOSTAT=IOS)
     +            DELETE,EL_UNIT_NAME(I),HYTYPE(I),
     +            HYDRO_EXPENSE_ASSIGNMENT(I),
     +            HYDRO_EXPENSE_COLLECTION(I),PCOWN(I),
     +            ON_LINE_MONTH(I),ON_LINE_YEAR(I),
     +            OFF_LINE_MONTH(I),OFF_LINE_YEAR(I),
     +            PSEFF(I),PSCAP(I),VAROM(I),VOMESR(I),
     +            HYDRO_FIXED_COST(I),HYDRO_FIXED_COST_ESCALATOR(I),
     +            PLANNING_FACTOR_HYDRO(I),
     +            (IN_CAPMO(J,I),ENRGMO(J,I),J=1,12),
     +            EL_AI_CAPACITY_RATE(I),EL_AI_CAPACITY_ESCALATOR(I),
     +            EL_AI_ENERGY_RATE(I),EL_AI_ENERGY_ESCALATOR(I),
     +            AI_EL_REMAINING_LIFE(I),EL_POOL_FRAC_OWN(I),
     +            EL_SO2_RATE(I),EL_GROUP(I),EL_FUEL_TYPE(I),
     +            EL_UNIT_ID(I), ! INTEGER RDI UNIT ID
     +            HYDRO_ANNUAL_FIXED_COST(I),
     +            AI_EL_TAX_LIFE(I),
     +            AI_EL_ADR_LIFE(I),
     +            ASSET_CLASS_NUM(I),
     +            ASSET_CLASS_VECTOR(I),
     +            INTRA_COMPANY_CLASS_ID(I),
     +            RESERVE_CONTRIBUTION(I),
     +            HYDRO_ANNUAL_FIXED_COST_ESC(I),
     +            SPECIAL_HYDRO_UNIT_ID(I),
     +            UNIT_ACTIVE,
     +            BASECASE_PLANT_ID(I),
     +            BASECASE_UNIT_ID(I),
     +            BASECASE_MARKET_AREA_ID(I),
     +            BASECASE_TRANS_AREA_ID(I),
     +            BASECASE_PLANT_NERC_SUB_ID(I),
     +            BASECASE_PLANT_OWNER_ID(I),
     +            TRANSACTION_GROUP_ID(I),
     +            UTILITY_OWNED(I),
     +            REPORT_THIS_UNIT(I),
     +            HESI_UNIT_ID_NUM(I),
     +            POWERMAP_PLANT_ID(I),
     +            BASECASE_PRIMARY_MOVER_STR,
     +            TRANS_BUS_ID(I),
     +            LATITUDE(I),
     +            LONGITUDE(I),
     +            MW_INSIDE_FENCE,
     +            STATE_PROVINCE,
     +            RPS_CONTRIBUTION(I),
     +            NEW_BUILD_STATUS,
     +            LONG_STATE_PROVINCE,
     +            UNIT_TYPE,
     +            UNIT_TYPE_CATEGORY,
     +            RPS_PROGRAM_NUMBER(I)
C
               IF(IOS == 0) UNIQUE_ID_NUM = UNIQUE_ID_NUM + 1
C
            ELSEIF(RDI_EL_REC < RDI_EL_RECORDS .AND. FIRST_PASS) THEN
               RDI_REC = RDI_REC + 1
               UNIT_ACTIVE = 'T'
               READ(RDI_IN_UNIT,REC=RDI_REC)
     +            DELETE,
     +            RDI_COMPANY_NAME,
     +            RDI_NERC_REGION,
     +            RDI_SUB_REGION,
     +            EL_UNIT_NAME(I),
     +            RDI_PM_ABBREV,
     +            RDI_PRIME_FUEL,
     +            ON_LINE_YEAR(I),! NUMBERS ! INT
     +            ON_LINE_MONTH(I), ! INT
     +            PCOWN(I),
     +            RDI_DEMONSTRATED_CAPACITY_MW,
     +            RDI_INCREMENTAL_FUEL_$MWH,
     +            RDI_FUEL_COST_$MWH,
     +            VAROM(I),
     +            HYDRO_FIXED_COST(I),
     +            RDI_AVERAGE_HEAT_RATE,
!     +            MONTH_GENERATION,
!     +            MONTH_CAPACITY
     +            (ENRGMO(J,I),J=1,12),
     +            (IN_CAPMO(J,I),J=1,12)
!
               IF(trim(RDI_PM_ABBREV) /= 'HY') CYCLE ! THIS PERMITS ONE BIN FILE
!
               RDI_EL_REC = RDI_EL_REC + 1
!
               HYTYPE(I) = 'B'
               HYDRO_EXPENSE_ASSIGNMENT(I) = "E"!nergy Expense"
               HYDRO_EXPENSE_COLLECTION(I) = "A"!djustment Clause"
               ON_LINE_MONTH(I) = 1
               ON_LINE_YEAR(I) = 1990
               OFF_LINE_MONTH(I) = 12
               OFF_LINE_YEAR(I) = 2050
               PCOWN(I) = 100.0 ! APPEARS TO BE ACCOUNTED FOR
               PSEFF(I) = 100.
               PSCAP(I) = 0.
               VOMESR(I) = 0
               HYDRO_FIXED_COST_ESCALATOR(I) = 0
               PLANNING_FACTOR_HYDRO(I) = 1.0
               EL_AI_CAPACITY_RATE(I) = 0.
               EL_AI_CAPACITY_ESCALATOR(I) = 0
               EL_AI_ENERGY_RATE(I) = 0.
               EL_AI_ENERGY_ESCALATOR(I) = 0
               AI_EL_REMAINING_LIFE(I) = 1
               EL_POOL_FRAC_OWN(I) = 100.
               EL_SO2_RATE(I) = 0.
               EL_GROUP(I) = 0
               EL_FUEL_TYPE(I) = "H"
               EL_UNIT_ID(I) = RDI_EL_REC + EL_RECORDS
               HYDRO_ANNUAL_FIXED_COST(I) = 0.
               AI_EL_TAX_LIFE(I) = 0.
               AI_EL_ADR_LIFE(I) = 0.
               ASSET_CLASS_NUM(I) = 0
               ASSET_CLASS_VECTOR(I) = 0
               INTRA_COMPANY_CLASS_ID(I) = -2
               RESERVE_CONTRIBUTION(I) = "C"!apacity"
               HYDRO_ANNUAL_FIXED_COST_ESC(I) = 0
               SPECIAL_HYDRO_UNIT_ID(I) = "None"
               RDI_DESC = trim(RDI_COMPANY_NAME)//':POWERDAT'
               TRANSACTION_GROUP_ID(I) = 1
!
               WRITE(RDI_OUT_UNIT,1000)
     +            "1,",
     +            trim(EL_UNIT_NAME(I)),
     +            trim(HYTYPE(I)),
     +            trim(HYDRO_EXPENSE_ASSIGNMENT(I)),
     +            trim(HYDRO_EXPENSE_COLLECTION(I)),
     +            PCOWN(I),
     +            ON_LINE_MONTH(I),
     +            ON_LINE_YEAR(I),
     +            OFF_LINE_MONTH(I),
     +            OFF_LINE_YEAR(I),
     +            PSEFF(I),
     +            PSCAP(I),
     +            VAROM(I),
     +            VOMESR(I),
     +            HYDRO_FIXED_COST(I),
     +            HYDRO_FIXED_COST_ESCALATOR(I),
     +            PLANNING_FACTOR_HYDRO(I),
     +            (IN_CAPMO(J,I),ENRGMO(J,I),J=1,12),
     +            trim(RDI_DESC),
     +            EL_POOL_FRAC_OWN(I),
     +            EL_SO2_RATE(I),
     +            EL_GROUP(I),
     +            trim(EL_FUEL_TYPE(I)),
     +            EL_AI_CAPACITY_RATE(I),
     +            EL_AI_CAPACITY_ESCALATOR(I),
     +            EL_AI_ENERGY_RATE(I),
     +            EL_AI_ENERGY_ESCALATOR(I),
     +            AI_EL_REMAINING_LIFE(I),
     +            EL_UNIT_ID(I),
     +            HYDRO_ANNUAL_FIXED_COST(I),
     +            AI_EL_TAX_LIFE(I),
     +            AI_EL_ADR_LIFE(I),
     +            ASSET_CLASS_NUM(I),
     +            ASSET_CLASS_VECTOR(I),
     +            INTRA_COMPANY_CLASS_ID(I),
     +            trim(RESERVE_CONTRIBUTION(I)),
     +            HYDRO_ANNUAL_FIXED_COST_ESC(I),
     +            trim(SPECIAL_HYDRO_UNIT_ID(I))
 1000          FORMAT( A,4('"',A,'"',','),
     +    	      F6.2,',',4(I4,','),F6.2,',',F9.2,',',F6.2,',',
     +		      I3,',',F8.3,',',I3,',',F6.2,',',24(F10.1,','),
     +		      '"',A,'"',',',F6.2,',',F9.2,',',I2,',',
     +            '"',A,'"',',',F9.2,',',
     +		      I4,',',F9.2,',',3(I4,','),F9.1,',',F8.3,',',
     +		      F8.3,',',I4,',',I5,',',I4,',',
     +            '"',A,'"',',',I4,',','"',A,'"')
            ELSE
               WRITE(4,*) "Untrapped error when processing"
               WRITE(4,*) "Capacity Limited Units"
               WRITE(4,*) '*** line 989 EL_OBJT.FOR ***'
               er_message='See WARNING MESSAGES -EL_OBJT.FOR-1'
               call end_program(er_message)
            ENDIF
            IF(IOS /= 0) EXIT
!
! 10/15/04.
!
            IF(ZONAL_LEVEL_MARKET) THEN
               MARKET_ID = BASECASE_MARKET_AREA_ID(I)
               TRANSACTION_GROUP_ID(I) =
     +                                     MARKET_AREA_LOOKUP(MARKET_ID)
            ENDIF
!
! 10/03/02. FOR REGIONAL CONSOLIDATION. NOTE REASSIGNMENT.
!
            TRANSACTION_GROUP_ID(I) =
     +                     GET_BELONGS_TO_GROUP(TRANSACTION_GROUP_ID(I))
!
            TRANS_ID = TRANSACTION_GROUP_ID(I)
            IF(DELETE > 7 .OR. UNIT_ACTIVE == 'F' .OR.
     +          (RUN_TRANSACT .AND.
     +                 (.NOT. TRANS_GROUP_ACTIVE_SWITCH(TRANS_ID) .OR.
     +                  .NOT. IN_ACTIVE_HYDRO_MARKET_AREA(
     +                                 BASECASE_MARKET_AREA_ID(I))
     +                                                         ))) CYCLE
!
! 100504. FOR THE EV DATABASE
! 112204. BACK IN
!
            IF(MW_INSIDE_FENCE > .5) THEN
               NO_POINTERS = .TRUE.
               DO J = 1, 12
                  IF(IN_CAPMO(J,I) > .5 .AND. ENRGMO(J,I) > .5 .AND.
     +                       MW_INSIDE_FENCE < IN_CAPMO(J,I)*.999) CYCLE
                  NO_POINTERS = .FALSE.
                  EXIT
               END DO
               IF(NO_POINTERS) THEN
                  DO J = 1, 12
                     TEMP_R = 1. - MW_INSIDE_FENCE/IN_CAPMO(J,I)
                     IN_CAPMO(J,I) = IN_CAPMO(J,I) - MW_INSIDE_FENCE
                     ENRGMO(J,I) = ENRGMO(J,I)*TEMP_R
                  ENDDO
               ENDIF
            ENDIF
C
C
            UNIQUE_EL_ID(I) = UNIQUE_ID_NUM
            CALL SET_ASSET_CLASSES(ASSET_CLASS_NUM(I),
     +                             NUMBER_OF_HYDRO_CLASSES,
     +	                          MAX_HYDRO_CLASS_ID_NUM,
     +                             HYDRO_ASSET_CLASS_POINTER)
            IF(HYTYPE(I) == 'B' .AND.
     +               HYDRO_EXPENSE_ASSIGNMENT(I) == 'R') HYTYPE(I) = 'A'
            MAX_EL_ID_NUM = MAX(MAX_EL_ID_NUM,EL_UNIT_ID(I))
            IF(ON_LINE_YEAR(I) > LAST_STUDY_YEAR) THEN
               EL_OPTIONS_COUNTR = EL_OPTIONS_COUNTR + 1
               EL_OPTIONS_ID_NUM(EL_OPTIONS_COUNTR) = EL_UNIT_ID(I)
               EL_OPTIONS_POINTR(EL_OPTIONS_COUNTR) = I
            ENDIF
            ON_LINE_YEAR(I) = MIN(MAX(ON_LINE_YEAR(I),1901),2200)
            OFF_LINE_YEAR(I) = MIN(OFF_LINE_YEAR(I),2200)
!         IF(EL_GROUP(I)<0 .OR. EL_GROUP(I)>MAX_REPORTING_GROUPS)
!     +                                                  EL_GROUP(I) = 0
            ON_LINE(I) = 100*(ON_LINE_YEAR(I)-1900) + ON_LINE_MONTH(I)
            OFF_LINE(I) = 100*(OFF_LINE_YEAR(I)-1900)+OFF_LINE_MONTH(I)
            IF(PSEFF(I) > 1.) PSEFF(I) = PSEFF(I)/100.
            IF(PCOWN(I) > 1.) PCOWN(I) = PCOWN(I)/100.
            PSEFF_SAVE(I) = PSEFF(I)
            VAROM_SAVE(I) = VAROM(I)
            HYDRO_FIXED_COST_SAVE(I) = HYDRO_FIXED_COST(I)
            HYDRO_ANNUAL_FIXED_COST_SAVE(I) = HYDRO_ANNUAL_FIXED_COST(I)
            EL_AI_CAPACITY_RATE_SAVE(I) = EL_AI_CAPACITY_RATE(I)
            EL_AI_ENERGY_RATE_SAVE(I) = EL_AI_ENERGY_RATE(I)
            AI_EL_REMAINING_LIFE_SAVE(I) = AI_EL_REMAINING_LIFE(I)
C
! MSG I ASSUME THAT THE T ASSIGNMENT IS AN OLD TRANSFER.
C
            IF(HYDRO_EXPENSE_ASSIGNMENT(I) == 'T')
     +                                HYDRO_EXPENSE_ASSIGNMENT(I) = 'E'
!
            IF(BASECASE_PRIMARY_MOVER_STR == 'PUMP  ') THEN
               PRIMARY_MOVER_INDEX(I) = 7
            ELSE
               PRIMARY_MOVER_INDEX(I) = 6
            ENDIF
            RPS_PM_INDEX(I) =
     +               PRIMARY_MOVER_INDEX_DB(BASECASE_PRIMARY_MOVER_STR)
            TEMP_CHAR6 = STATE_PROVINCE(1:2)
            ST_TG = STATE_ID_LOOKUP(TEMP_CHAR6)
            State_Index(I) = ST_TG
            ST_TG = STATE_ID_LOOKUP(STATE_PROVINCE)
            State_TG_Index(I) = ST_TG
C
! BEGINING OF THE CAPACITY PLANNING SECTION
C
            IF(ON_LINE_YEAR(I) <= OFF_LINE_YEAR(I)   .AND.
     +         ON_LINE_YEAR(I) - BASE_YEAR <= STUDY_PERIOD .AND.
     +                                OFF_LINE_YEAR(I) > BASE_YEAR) THEN
               HYDRO_UNITS = I  ! ADDED TO REMOVE THE I VARIABLE FORM THE HERE
               VOID_REAL = EL_CAPACITY_PLANNING_ADDITIONS(INT2(1))
C
! END OF CAPACITY PLANNING SECTION
C
            ENDIF
            I = I + 1
            IF(I>MAX_EL_UNITS) CALL EL_UNIT_LIMIT_EXCEEDED
         ENDDO
         CLOSE(10)
         IF(RDI_EL_RECORDS > 0 .AND. FIRST_PASS) THEN
            CALL CLOSE_BOTH_RDI_FILES
         ENDIF
         FIRST_PASS = .FALSE.
      ENDDO ! FILE LOOP
      BASE_EL_UNITS = I - 1
      BASE_PLUS_HARDWIRED_EL_UNITS = BASE_EL_UNITS
      AVAILABLE_EL_UNITS = BASE_EL_UNITS
      EL_UNITS_READ = BASE_EL_UNITS
C
      ASSET_CLASS_ACTIVE = .TRUE.
      IF(ASSET_CLASS_ACTIVE) THEN
         CALL STORE_EL_ASSET_CLASS_INFO(ASSET_CLASS_NUM,
     +                                  ASSET_CLASS_VECTOR,
     +                                  INTRA_COMPANY_CLASS_ID,
     +                                  BASE_EL_UNITS)
      ENDIF
C
      RETURN
C***********************************************************************
      ENTRY GET_HYDRO_RPS_DATA(EL_UNIT_NO,R_PM,R_ST_TG,R_ST,
     +                                                   R_RPS_CONTRIB)
C***********************************************************************
         R_PM = RPS_PM_INDEX(EL_UNIT_NO)
         R_ST_TG = State_TG_Index(EL_UNIT_NO)
         R_ST = State_Index(EL_UNIT_NO)
         R_RPS_CONTRIB = RPS_CONTRIBUTION(EL_UNIT_NO)* 0.01
         GET_HYDRO_RPS_DATA = .TRUE.
      RETURN
C***********************************************************************
      ENTRY PUT_HYDRO_RPS_ENRG_CAP(EL_UNIT_NO,R_ENRG,R_CAP,R_YEAR)
C***********************************************************************
         TEMP_RPS_NO = RPS_PROGRAM_NUMBER(EL_UNIT_NO)
         TEMP_PM = RPS_PM_INDEX(EL_UNIT_NO)
         LOCAL_RPS_CONTRIB_PERCENT = RPS_CONTRIBUTION(EL_UNIT_NO)
         IF(LOCAL_RPS_CONTRIB_PERCENT < -0.01) THEN
            LOCAL_RPS_CONTRIB_PERCENT =
     +               ESCALATED_MONTHLY_VALUE(LOCAL_RPS_CONTRIB_PERCENT,
     +                    ABS(INT2(RPS_CONTRIBUTION(EL_UNIT_NO))),
     +                                           R_YEAR,INT2(1),INT2(1))
         ENDIF
         TEMP_ENRG = R_ENRG * LOCAL_RPS_CONTRIB_PERCENT* 0.01
!         TEMP_ENRG = R_ENRG * LOCAL_RPS_CONTRIB_PERCENT* 0.01 * 0.001
         TEMP_CAP = R_CAP * LOCAL_RPS_CONTRIB_PERCENT * 0.01
         TEMP_L1 = PUT_RPS_ENRG_CAP(TEMP_RPS_NO,TEMP_PM,
     +                              TEMP_ENRG,TEMP_CAP)
         PUT_HYDRO_RPS_ENRG_CAP = .TRUE.
      RETURN
C***********************************************************************
      ENTRY GET_HYDRO_PRIMARY_M0VER(EL_UNIT_NO)
C***********************************************************************
         GET_HYDRO_PRIMARY_M0VER = PRIMARY_MOVER_INDEX(EL_UNIT_NO)
      RETURN
C***********************************************************************
      ENTRY GET_EL_BASECASE_MARKET_ID(EL_UNIT_NO,R_MARKET_ID)  ! L1
C***********************************************************************
         R_MARKET_ID = BASECASE_MARKET_AREA_ID(EL_UNIT_NO)
         GET_EL_BASECASE_MARKET_ID = .TRUE.
      RETURN
C***********************************************************************
      ENTRY GET_HESI_EL_UNIT_ID_NUM(EL_UNIT_NO)
C***********************************************************************
         GET_HESI_EL_UNIT_ID_NUM = HESI_UNIT_ID_NUM(EL_UNIT_NO)
      RETURN
C***********************************************************************
      ENTRY GET_PW_EL_UNIT_TEXT_FIELDS(EL_UNIT_NO,
     +                                 R_EIA_PLANT_CODE)
C***********************************************************************
         GET_PW_EL_UNIT_TEXT_FIELDS = .TRUE.
         R_EIA_PLANT_CODE = BASECASE_PLANT_ID(EL_UNIT_NO)
       RETURN
C***********************************************************************
      ENTRY REPORT_THIS_EL_UNIT(EL_UNIT_NO)
C***********************************************************************
         REPORT_THIS_EL_UNIT = REPORT_THIS_UNIT(EL_UNIT_NO) /= 'F'
      RETURN
C***********************************************************************
      ENTRY HYDRO_TRANS_GROUP(EL_UNIT_NO)
C***********************************************************************
         HYDRO_TRANS_GROUP = TRANSACTION_GROUP_ID(EL_UNIT_NO)
      RETURN
C***********************************************************************
      ENTRY RETURN_EL_UNITS_B4_ADDITIONS
C***********************************************************************
         RETURN_EL_UNITS_B4_ADDITIONS = BASE_EL_UNITS
      RETURN
C***********************************************************************
      ENTRY EL_RESOURCE_ID(EL_UNIT_NO)
C***********************************************************************
         EL_RESOURCE_ID = EL_UNIT_ID(EL_UNIT_NO)
      RETURN
C******************************************************************
      ENTRY RESET_EL_UNITS
C******************************************************************
         DO I = 1, INSTALLED_POINTER
            UNIT_NO = TEMP_RETIRED_UNIT_NO(INSTALLED_POINTER)
            OFF_LINE(UNIT_NO) = TEMP_RETIRED_OFF_LINE(INSTALLED_POINTER)
         ENDDO
         HYDRO_UNITS = AVAILABLE_EL_UNITS
         RESET_EL_UNITS = HYDRO_UNITS
         INSTALLED_POINTER = 0
      RETURN
C******************************************************************
      ENTRY INCREMENT_HARDWIRED_EL_UNITS
C******************************************************************
         BASE_PLUS_HARDWIRED_EL_UNITS = HYDRO_UNITS
         INCREMENT_HARDWIRED_EL_UNITS = BASE_PLUS_HARDWIRED_EL_UNITS
      RETURN
C********************************************************************
      ENTRY INCREMENT_AVAILABLE_EL_UNITS
C***********************************************************************
         AVAILABLE_EL_UNITS = HYDRO_UNITS
         INCREMENT_AVAILABLE_EL_UNITS = AVAILABLE_EL_UNITS
      RETURN
C******************************************************************
      ENTRY PEAK_ADJ_OFF_SYSTEM_SALES(R_YEAR)
C******************************************************************
         IF(R_YEAR > STUDY_PERIOD) THEN
            PEAK_ADJ_OFF_SYSTEM_SALES =
     +                         PEAK_ADJ_2_OFF_SYSTEM_SALES(STUDY_PERIOD)
         ELSE
            PEAK_ADJ_OFF_SYSTEM_SALES =
     +                               PEAK_ADJ_2_OFF_SYSTEM_SALES(R_YEAR)
         ENDIF
      RETURN
C******************************************************************
      ENTRY EL_OPTION_CHECK(NEW_EL_UNIT_ID)
C******************************************************************
!         DO I = 1, EL_OPTIONS
!            IF(NEW_EL_UNIT_ID /= EL_OPTIONS_ID_NUM(I) .OR.
!     +                    ON_LINE_YEAR(I) < LAST_STUDY_YEAR ) CYCLE
!            EL_OPTION_CHECK = .TRUE.
!            RETURN
!         ENDDO
         EL_OPTION_CHECK = .FALSE.
         DO I = 1, EL_OPTIONS_COUNTR
            IF(NEW_EL_UNIT_ID /= EL_OPTIONS_ID_NUM(I) .OR.
     +                    ON_LINE_YEAR(EL_OPTIONS_POINTR(I))
     +                                    < LAST_STUDY_YEAR ) CYCLE
            EL_OPTION_CHECK = .TRUE.
            EXIT
         ENDDO
      RETURN
C******************************************************************
      ENTRY ADD_NEW_EL_UNIT(UNIT_ON_LINE_YEAR,NEW_EL_UNIT_ID,
     +                      OPERATION_LIFE,R_ON_LINE_MONTH,
     +                      R_ASSET_CLASS_NUM,
     +                      R_ASSET_CLASS_VECTOR)
C******************************************************************
         EL_ARRAY_POS = 0
         DO I = 1, EL_OPTIONS
            IF(NEW_EL_UNIT_ID /= EL_OPTIONS_ID_NUM(I)) CYCLE
            EL_ARRAY_POS = EL_OPTIONS_POINTR(I)
            HYDRO_UNITS = HYDRO_UNITS + 1
            IF(HYDRO_UNITS > MAX_EL_UNITS) CALL EL_UNIT_LIMIT_EXCEEDED
            MAX_EL_ID_NUM = MAX_EL_ID_NUM + 1
            EL_UNIT_ID(HYDRO_UNITS) = MAX_EL_ID_NUM
            WRITE(YEAR_CHR,'(I4)') UNIT_ON_LINE_YEAR
            IF(HYDRO_UNITS < 10) THEN
               WRITE(HYDRO_UNITS_CHR,'(I1)') HYDRO_UNITS
            ELSEIF(HYDRO_UNITS < 100) THEN
               WRITE(HYDRO_UNITS_CHR,'(I2)') HYDRO_UNITS
            ELSE
               WRITE(HYDRO_UNITS_CHR,'(I3)') HYDRO_UNITS
            ENDIF
            EL_UNIT_NAME(HYDRO_UNITS) = YEAR_CHR(3:)//'/'//
     +                             trim(HYDRO_UNITS_CHR)//' '//
     +                                trim(EL_UNIT_NAME(EL_ARRAY_POS))
            HYTYPE(HYDRO_UNITS) = HYTYPE(EL_ARRAY_POS)
            HYDRO_EXPENSE_ASSIGNMENT(HYDRO_UNITS) =
     +            HYDRO_EXPENSE_ASSIGNMENT(EL_ARRAY_POS)
            HYDRO_EXPENSE_COLLECTION(HYDRO_UNITS) =
     +            HYDRO_EXPENSE_COLLECTION(EL_ARRAY_POS)
            PCOWN(HYDRO_UNITS) = PCOWN(EL_ARRAY_POS)
            ASSET_CLASS_NUM(HYDRO_UNITS) = R_ASSET_CLASS_NUM
            ASSET_CLASS_VECTOR(HYDRO_UNITS) = R_ASSET_CLASS_VECTOR
            INTRA_COMPANY_CLASS_ID(HYDRO_UNITS) =
     +                              INTRA_COMPANY_CLASS_ID(EL_ARRAY_POS)
C
            RESERVE_CONTRIBUTION(HYDRO_UNITS) =
     +                                RESERVE_CONTRIBUTION(EL_ARRAY_POS)
            CALL SET_ASSET_CLASSES(ASSET_CLASS_NUM(HYDRO_UNITS),
     +                             NUMBER_OF_HYDRO_CLASSES,
     +	                          MAX_HYDRO_CLASS_ID_NUM,
     +                             HYDRO_ASSET_CLASS_POINTER)
            ON_LINE_YEAR(HYDRO_UNITS) = UNIT_ON_LINE_YEAR
            OFF_LINE_YEAR(HYDRO_UNITS) = UNIT_ON_LINE_YEAR +
     +                                                    OPERATION_LIFE
            ON_LINE_MONTH(HYDRO_UNITS)=ON_LINE_MONTH(EL_ARRAY_POS)
            OFF_LINE_MONTH(HYDRO_UNITS) = OFF_LINE_MONTH(EL_ARRAY_POS)
            PSEFF(HYDRO_UNITS) = PSEFF(EL_ARRAY_POS)
            PSCAP(HYDRO_UNITS) = PSCAP(EL_ARRAY_POS)
            VAROM(HYDRO_UNITS) = VAROM(EL_ARRAY_POS)
            VOMESR(HYDRO_UNITS) = VOMESR(EL_ARRAY_POS)
            HYDRO_FIXED_COST(HYDRO_UNITS)=HYDRO_FIXED_COST(EL_ARRAY_POS)
            HYDRO_ANNUAL_FIXED_COST(HYDRO_UNITS) =
     +                             HYDRO_ANNUAL_FIXED_COST(EL_ARRAY_POS)
            HYDRO_FIXED_COST_ESCALATOR(HYDRO_UNITS) =
     +                          HYDRO_FIXED_COST_ESCALATOR(EL_ARRAY_POS)
            HYDRO_ANNUAL_FIXED_COST_ESC(HYDRO_UNITS) =
     +                         HYDRO_ANNUAL_FIXED_COST_ESC(EL_ARRAY_POS)
            SPECIAL_HYDRO_UNIT_ID(HYDRO_UNITS) =
     +                               SPECIAL_HYDRO_UNIT_ID(EL_ARRAY_POS)
            PLANNING_FACTOR_HYDRO(HYDRO_UNITS) =
     +                               PLANNING_FACTOR_HYDRO(EL_ARRAY_POS)
!            DO J = 1, 12
!               IN_CAPMO(J,HYDRO_UNITS) = IN_CAPMO(J,EL_ARRAY_POS)
!               ENRGMO(J,HYDRO_UNITS) = ENRGMO(J,EL_ARRAY_POS)
!            ENDDO
            IN_CAPMO(:,HYDRO_UNITS) = IN_CAPMO(:,EL_ARRAY_POS)
            ENRGMO(:,HYDRO_UNITS) = ENRGMO(:,EL_ARRAY_POS)
            EL_AI_CAPACITY_RATE(HYDRO_UNITS) =
     +                                 EL_AI_CAPACITY_RATE(EL_ARRAY_POS)
            EL_AI_CAPACITY_ESCALATOR(HYDRO_UNITS) =
     +                            EL_AI_CAPACITY_ESCALATOR(EL_ARRAY_POS)
            EL_AI_ENERGY_RATE(HYDRO_UNITS) =
     +                                   EL_AI_ENERGY_RATE(EL_ARRAY_POS)
            EL_AI_ENERGY_ESCALATOR(HYDRO_UNITS) =
     +                              EL_AI_ENERGY_ESCALATOR(EL_ARRAY_POS)
            AI_EL_REMAINING_LIFE(HYDRO_UNITS) =
     +                                AI_EL_REMAINING_LIFE(EL_ARRAY_POS)
            PSEFF_SAVE(HYDRO_UNITS) = PSEFF(EL_ARRAY_POS)
            VAROM_SAVE(HYDRO_UNITS) = VAROM(EL_ARRAY_POS)
            HYDRO_FIXED_COST_SAVE(HYDRO_UNITS) =
     +                                    HYDRO_FIXED_COST(EL_ARRAY_POS)
            HYDRO_ANNUAL_FIXED_COST_SAVE(HYDRO_UNITS) =
     +                             HYDRO_ANNUAL_FIXED_COST(EL_ARRAY_POS)
            EL_AI_CAPACITY_RATE_SAVE(HYDRO_UNITS) =
     +                                 EL_AI_CAPACITY_RATE(EL_ARRAY_POS)
            EL_AI_ENERGY_RATE_SAVE(HYDRO_UNITS) =
     +                                   EL_AI_ENERGY_RATE(EL_ARRAY_POS)
            AI_EL_REMAINING_LIFE_SAVE(HYDRO_UNITS) =
     +                                AI_EL_REMAINING_LIFE(EL_ARRAY_POS)
            AI_EL_TAX_LIFE(HYDRO_UNITS) = AI_EL_TAX_LIFE(EL_ARRAY_POS)
            AI_EL_ADR_LIFE(HYDRO_UNITS) = AI_EL_ADR_LIFE(EL_ARRAY_POS)
            EL_POOL_FRAC_OWN(HYDRO_UNITS) =
     +                                    EL_POOL_FRAC_OWN(EL_ARRAY_POS)
            EL_SO2_RATE(HYDRO_UNITS) = EL_SO2_RATE(EL_ARRAY_POS)
            EL_GROUP(HYDRO_UNITS) = EL_GROUP(EL_ARRAY_POS)
            EL_FUEL_TYPE(HYDRO_UNITS) = EL_FUEL_TYPE(EL_ARRAY_POS)
            ON_LINE(HYDRO_UNITS) = ON_LINE_MONTH(HYDRO_UNITS) +
     +                              100*(ON_LINE_YEAR(HYDRO_UNITS)-1900)
            OFF_LINE(HYDRO_UNITS) = OFF_LINE_MONTH(HYDRO_UNITS) +
     +                             100*(OFF_LINE_YEAR(HYDRO_UNITS)-1900)
            R_ON_LINE_MONTH = ON_LINE_MONTH(HYDRO_UNITS)
!
!
!
            PLANNING_MO = PEAK_MONTH(UNIT_ON_LINE_YEAR)
            EN_MW = IN_CAPMO(PLANNING_MO,HYDRO_UNITS)
            IF(EN_MW < 0.) THEN
               EN_MW =
     +             MAX(0,GET_EL_MW_FROM_POINTR(EN_MW,UNIT_ON_LINE_YEAR))
            ELSE
C
! IF NO POINTER HAS BEEN SPECIFIED FOR THE PEAK MONTH
C
               EN_MW = MAX(0,EN_MW)
            ENDIF
!
            IF(PLANNING_FACTOR_HYDRO(HYDRO_UNITS) < 0.) THEN
               PT_YR = ON_LINE_YEAR(HYDRO_UNITS) - BASE_YEAR
               EN_MW = EN_MW * PCOWN(HYDRO_UNITS) *
     +               GET_VAR(PLANNING_FACTOR_HYDRO(HYDRO_UNITS),
     +                       PT_YR,
     +                       EL_UNIT_NAME(HYDRO_UNITS))
            ELSE
             EN_MW = EN_MW * PCOWN(HYDRO_UNITS) *
     +                                PLANNING_FACTOR_HYDRO(HYDRO_UNITS)
            ENDIF
            ADD_NEW_EL_UNIT = EN_MW
!
! ABOVE ADDED 1/14/97. GAT FOR SJONES.
!
            ASSET_CLASS_ACTIVE = .TRUE.
            IF(ASSET_CLASS_ACTIVE)
     +           CALL STORE_NEW_EL_ASSET_CLASS(HYDRO_UNITS,EL_ARRAY_POS)
            RETURN
         ENDDO
         ADD_NEW_EL_UNIT = 0.

         ! Check HYTYPE array.
      RETURN
C******************************************************************
      ENTRY RETURN_EL_UNIT_ON_LINE_MONTH(NEW_EL_UNIT_ID)
C******************************************************************
         RETURN_EL_UNIT_ON_LINE_MONTH = 7
         DO I = 1, EL_OPTIONS
            IF(NEW_EL_UNIT_ID /= EL_OPTIONS_ID_NUM(I)) CYCLE
            EL_ARRAY_POS = EL_OPTIONS_POINTR(I)
            RETURN_EL_UNIT_ON_LINE_MONTH = ON_LINE_MONTH(EL_ARRAY_POS)
            EXIT
         ENDDO
      RETURN
C******************************************************************
      ENTRY SET_AI_EL_REMAINING_LIFE(AI_REMAINING_LIFE)
C******************************************************************
         IF(EL_ARRAY_POS /= 0) THEN
            AI_EL_REMAINING_LIFE(EL_ARRAY_POS) = AI_REMAINING_LIFE
            SET_AI_EL_REMAINING_LIFE = AI_REMAINING_LIFE
         ELSE
            SET_AI_EL_REMAINING_LIFE = 0
         ENDIF
      RETURN
C******************************************************************
      ENTRY EL_CAPACITY_PLANNING_ADDITIONS(ST)
     +                            RESULT(R_EL_CAP_PLANNING_ADDITIONS)
C******************************************************************
         YEAR_START = ON_LINE_YEAR(HYDRO_UNITS) - BASE_YEAR
         IF(YEAR_START < 1) THEN
            YEAR_START = 1
         ELSE
            IF(ON_LINE_MONTH(HYDRO_UNITS) > PEAK_MONTH(YEAR_START)) THEN
               WRITE(4,*) 'The on-line month, ',
     +                   trim(MONTH_NAME(ON_LINE_MONTH(HYDRO_UNITS))),
     +               ', for EL unit, ',trim(EL_UNIT_NAME(HYDRO_UNITS))
               WRITE(4,"(A,I4,A)") '& is after the peak month in ',
     +             ON_LINE_YEAR(HYDRO_UNITS),' of '//
     +             trim(MONTH_NAME(PEAK_MONTH(YEAR_START)))//'.'
               WRITE(4,*) "The capacity of this unit is not in",
     +                   " the planning capacity for it's on-line year."
               WRITE(4,*)
               YEAR_START = YEAR_START + 1
            ENDIF
         ENDIF
         IF(OFF_LINE_YEAR(HYDRO_UNITS) > LAST_STUDY_YEAR) THEN
            YEAR_END = LAST_STUDY_YEAR - BASE_YEAR
         ELSE
            YEAR_END = OFF_LINE_YEAR(HYDRO_UNITS) - BASE_YEAR
            IF(OFF_LINE_MONTH(HYDRO_UNITS) < PEAK_MONTH(YEAR_END)) THEN
               WRITE(4,*) 'The off-line month, ',
     +                  trim(MONTH_NAME(OFF_LINE_MONTH(HYDRO_UNITS))),
     +               ', for EL unit, ',trim(EL_UNIT_NAME(HYDRO_UNITS))
               WRITE(4,"(A,I4,A)")'& is before the peak month in ',
     +                OFF_LINE_YEAR(HYDRO_UNITS),' of '//
     +                trim(MONTH_NAME(PEAK_MONTH(YEAR_END)))//'.'
               WRITE(4,*) "The capacity of this unit is not in",
     +                  " the planning capacity for its off-line year."
               WRITE(4,*)
               YEAR_END = YEAR_END - 1
            ENDIF
         ENDIF
         SUBTRACT_IT = .FALSE.
         UNIT_NO = HYDRO_UNITS
         OFF_LINE_YEAR(UNIT_NO) = YEAR_END
         R_EL_CAP_PLANNING_ADDITIONS= FIRST_YEAR_CAPACITY
         GOTO 10
!         VOID_REAL = EL_CAPACITY_PLANNING_REMOVAL_10()
      RETURN
C***************************************************************
      ENTRY EL_CAPACITY_PLANNING_REMOVALS(R_YEAR,R_UNIT_NO,ST)
C***************************************************************
         UNIT_NO = R_UNIT_NO
         YEAR_START = R_YEAR - BASE_YEAR
         YEAR_END = OFF_LINE_YEAR(UNIT_NO)
         OFLINE(UNIT_NO) = 100*(R_YEAR - 1900) + OFF_LINE_MONTH(UNIT_NO)
         SUBTRACT_IT = .TRUE.
   10 CONTINUE
!     ENTRY EL_CAPACITY_PLANNING_REMOVAL_10
         DO PT_YR = YEAR_START, YEAR_END
C
            PLANNING_MO = PEAK_MONTH(PT_YR)
            EN_MW = IN_CAPMO(PLANNING_MO,UNIT_NO)
            IF(EN_MW < 0.) THEN
               EN_MW = MAX(0,GET_EL_MW_FROM_POINTR(EN_MW,PT_YR))
            ELSE
C
! IF NO POINTER HAS BEEN SPECIFIED FOR THE PEAK MONTH
C
               EN_MW = MAX(0,EN_MW)
            ENDIF
C
! EFFECTIVE MW'S OF PLANNING CAPACITY
C
            IF(PLANNING_FACTOR_HYDRO(UNIT_NO) < 0.) THEN
               EN_MW = EN_MW * PCOWN(UNIT_NO) *
     +               GET_VAR(PLANNING_FACTOR_HYDRO(UNIT_NO),
     +                       PT_YR,
     +                       EL_UNIT_NAME(UNIT_NO))
            ELSE
               EN_MW = EN_MW * PCOWN(UNIT_NO) *
     +                                    PLANNING_FACTOR_HYDRO(UNIT_NO)
            ENDIF
C
! THIS SECTION ADDS MW'S TO THE SYSTEM
C
            IF(SUBTRACT_IT) EN_MW = -ABS(EN_MW)
            IF(EN_MW > 0. .OR. SUBTRACT_IT) THEN
               IF((RESERVE_CONTRIBUTION(UNIT_NO) == 'X' .AND.
     +                   INDEX('OQA',HYTYPE(UNIT_NO)) /= 0) .OR.
     +                        RESERVE_CONTRIBUTION(UNIT_NO) == 'L') THEN
! IT IS A RESOURCE.  THUS, IT REDUCES PEAK_ADJ_2... CONTRIBUTION
                  IF( INDEX('OQA',HYTYPE(UNIT_NO)) == 0)
     +                                               EN_MW = -ABS(EN_MW)
C
                  PEAK_ADJ_2_OFF_SYSTEM_SALES(PT_YR) = EN_MW +
     +                         PEAK_ADJ_2_OFF_SYSTEM_SALES(PT_YR)
               ELSE
! IT IS A SALE. THUS, IT REDUCES EL_ANN_CAP CONTRIBUTION
                  IF( INDEX('OQA',HYTYPE(UNIT_NO)) /= 0)
     +                                               EN_MW = -ABS(EN_MW)
C
                  EL_ANN_CAP(3,PT_YR,ST) = EL_ANN_CAP(3,PT_YR,ST)+EN_MW
                  IF(HYTYPE(UNIT_NO) == 'B' .OR.
     +                                  HYTYPE(UNIT_NO) == 'U') THEN
                     EL_ANN_CAP(1,PT_YR,ST)=EL_ANN_CAP(1,PT_YR,ST)+EN_MW
                     EL_ANN_CAP(2,PT_YR,ST)=EL_ANN_CAP(2,PT_YR,ST)+EN_MW
                  ENDIF
               ENDIF
!
               TG=TRANSACTION_GROUP_ID(UNIT_NO)
               TG = GET_TRANS_GROUP_POSITION(TG)
               IF(TG > 0) THEN
                  EL_TG_CAP(TG,PT_YR,ST) =
     +                                    EL_TG_CAP(TG,PT_YR,ST) + EN_MW
                  EL_TG_CAP(0,PT_YR,ST) =
     +                                    EL_TG_CAP(0,PT_YR,ST) + EN_MW
               ENDIF
!
               IF(PT_YR == YEAR_START) FIRST_YEAR_CAPACITY = ABS(EN_MW)
            ENDIF
         ENDDO
         EL_CAPACITY_PLANNING_REMOVALS = FIRST_YEAR_CAPACITY
!        EL_CAPACITY_PLANNING_ADDITIONS = FIRST_YEAR_CAPACITY
      RETURN
C******************************************************************
      ENTRY GET_EL_TG_CAP(R_TG,R_YEAR,R_ST_C)
C******************************************************************
         IF(ALLOCATED(EL_TG_CAP)) THEN
            IF(R_YEAR <= STUDY_PERIOD) THEN
               GET_EL_TG_CAP = EL_TG_CAP(R_TG,R_YEAR,R_ST_C)
            ELSE
               GET_EL_TG_CAP = EL_TG_CAP(R_TG,STUDY_PERIOD,R_ST_C)
            ENDIF
         ELSE
            GET_EL_TG_CAP = 0.
         ENDIF
      RETURN
C******************************************************************
      ENTRY EL_PLANNING_CAPACITY(R_CAP_TYPE,R_YEAR)
C******************************************************************
         IF(ALLOCATED(EL_ANN_CAP)) THEN
            IF(R_YEAR > STUDY_PERIOD) THEN
               EL_PLANNING_CAPACITY=
     +               EL_ANN_CAP(R_CAP_TYPE,STUDY_PERIOD,1) +
     +               EL_ANN_CAP(R_CAP_TYPE,STUDY_PERIOD,2)
            ELSE
               EL_PLANNING_CAPACITY = EL_ANN_CAP(R_CAP_TYPE,R_YEAR,1) +
     +                                EL_ANN_CAP(R_CAP_TYPE,R_YEAR,2)
            ENDIF
         ELSE
            EL_PLANNING_CAPACITY = 0.
         ENDIF
      RETURN
C******************************************************************
      ENTRY NEW_EL_PLANNING_CAPACITY(R_CAP_TYPE,R_YEAR)
C******************************************************************
         IF(ALLOCATED(EL_ANN_CAP)) THEN
            IF(R_YEAR > STUDY_PERIOD) THEN
               NEW_EL_PLANNING_CAPACITY=
     +               EL_ANN_CAP(R_CAP_TYPE,STUDY_PERIOD,2)
            ELSE
               NEW_EL_PLANNING_CAPACITY =EL_ANN_CAP(R_CAP_TYPE,R_YEAR,2)
            ENDIF
         ELSE
            NEW_EL_PLANNING_CAPACITY = 0.
         ENDIF
      RETURN
C******************************************************************
      ENTRY OLD_EL_PLANNING_CAPACITY(R_CAP_TYPE,R_YEAR)
C******************************************************************
         IF(ALLOCATED(EL_ANN_CAP)) THEN
            IF(R_YEAR > STUDY_PERIOD) THEN
               OLD_EL_PLANNING_CAPACITY=
     +               EL_ANN_CAP(R_CAP_TYPE,STUDY_PERIOD,1)
            ELSE
               OLD_EL_PLANNING_CAPACITY =EL_ANN_CAP(R_CAP_TYPE,R_YEAR,1)
            ENDIF
         ELSE
            OLD_EL_PLANNING_CAPACITY = 0.
         ENDIF
      RETURN
C***************************************************************
      ENTRY EL_CAPACITY_TEMP_RETIRE_UNIT(R_YEAR,R_UNIT_NO)
C***************************************************************
         IF(.NOT. ALLOCATED(TEMP_RETIRED_UNIT_NO))
     +                               ALLOCATE(TEMP_RETIRED_UNIT_NO(20),
     +                                        TEMP_RETIRED_OFF_LINE(20))
         INSTALLED_POINTER = 1 + INSTALLED_POINTER
         TEMP_RETIRED_UNIT_NO(INSTALLED_POINTER) = R_UNIT_NO
         TEMP_RETIRED_OFF_LINE(INSTALLED_POINTER) = OFF_LINE(R_UNIT_NO)
         EL_CAPACITY_TEMP_RETIRE_UNIT = OFF_LINE(R_UNIT_NO)
         OFF_LINE(R_UNIT_NO)=100*(R_YEAR-1900)+OFF_LINE_MONTH(R_UNIT_NO)
      RETURN
C******************************************************************
      ENTRY EL_RESET_PRICES
C******************************************************************
         PSEFF = PSEFF_SAVE
         VAROM = VAROM_SAVE
         HYDRO_FIXED_COST = HYDRO_FIXED_COST_SAVE
         HYDRO_ANNUAL_FIXED_COST = HYDRO_ANNUAL_FIXED_COST_SAVE
         EL_AI_CAPACITY_RATE = EL_AI_CAPACITY_RATE_SAVE
         EL_AI_ENERGY_RATE = EL_AI_ENERGY_RATE_SAVE
         AI_EL_REMAINING_LIFE = AI_EL_REMAINING_LIFE_SAVE
!         DO I = 1, HYDRO_UNITS
!            PSEFF(I) = PSEFF_SAVE(I)
!            VAROM(I) = VAROM_SAVE(I)
!            HYDRO_FIXED_COST(I) = HYDRO_FIXED_COST_SAVE(I)
!            HYDRO_ANNUAL_FIXED_COST(I) = HYDRO_ANNUAL_FIXED_COST_SAVE(I)
!            EL_AI_CAPACITY_RATE(I) = EL_AI_CAPACITY_RATE_SAVE(I)
!            EL_AI_ENERGY_RATE(I) = EL_AI_ENERGY_RATE_SAVE(I)
!            AI_EL_REMAINING_LIFE(I) = AI_EL_REMAINING_LIFE_SAVE(I)
!         ENDDO
         EL_RESET_PRICES = .TRUE.
      RETURN
C***********************************************************************
      ENTRY EL_PLANNING_ADDITIONS
C***********************************************************************
         EL_PLANNING_ADDITIONS = AVAILABLE_EL_UNITS -
     +                                      BASE_PLUS_HARDWIRED_EL_UNITS
      RETURN
C***********************************************************************
      ENTRY RETURN_NUM_OF_HYDRO_CLASSES(R_MAX_HYDRO_CLASS_NUM)
C***********************************************************************
         RETURN_NUM_OF_HYDRO_CLASSES = NUMBER_OF_HYDRO_CLASSES
         R_MAX_HYDRO_CLASS_NUM = MAX_HYDRO_CLASS_ID_NUM
      RETURN
C***********************************************************************
      ENTRY RETURN_MAX_EL_CLASS_NUM
C***********************************************************************
         RETURN_MAX_EL_CLASS_NUM = MAX_HYDRO_CLASS_ID_NUM
      RETURN
C***********************************************************************
      ENTRY RETURN_HYDRO_CLASS_POINTER(R_HYDRO_CLASS_POINTERS)
C***********************************************************************
!         DO I = 1, MAX_HYDRO_CLASS_ID_NUM
!            R_HYDRO_CLASS_POINTERS(I) = HYDRO_ASSET_CLASS_POINTER(I)
!         ENDDO
         R_HYDRO_CLASS_POINTERS(1:MAX_HYDRO_CLASS_ID_NUM) =
     +               HYDRO_ASSET_CLASS_POINTER(1:MAX_HYDRO_CLASS_ID_NUM)
         RETURN_HYDRO_CLASS_POINTER = MAX_HYDRO_CLASS_ID_NUM
      RETURN
C***********************************************************************
      ENTRY STORE_EL_ASSET_INFO(S_MAX_HYDRO_CLASS_ID_NUM,
     +                          S_NUMBER_OF_HYDRO_CLASSES,
     +                          S_HYDRO_ASSET_CLASS_POINTER)
C***********************************************************************
         MAX_HYDRO_CLASS_ID_NUM = S_MAX_HYDRO_CLASS_ID_NUM
         NUMBER_OF_HYDRO_CLASSES = S_NUMBER_OF_HYDRO_CLASSES
!         DO I = 1, 1024
!            HYDRO_ASSET_CLASS_POINTER(I)=S_HYDRO_ASSET_CLASS_POINTER(I)
!         ENDDO
         HYDRO_ASSET_CLASS_POINTER = S_HYDRO_ASSET_CLASS_POINTER
      RETURN
C
C***********************************************************************
      ENTRY UNIQUE_REPORT_VALUE_FOR_EL_UNIT(R_UNIT_NO)
C***********************************************************************
         UNIQUE_REPORT_VALUE_FOR_EL_UNIT = UNIQUE_EL_ID(R_UNIT_NO)
      RETURN
      END
C***********************************************************************
      FUNCTION EL_UNIQUE_RPT_STR(R_UNIT)
C***********************************************************************
C
         CHARACTER*5 EL_UNIQUE_RPT_STR,RPT_STR
         INTEGER*2 R_UNIT,EL_NUM,UNIQUE_REPORT_VALUE_FOR_EL_UNIT
         EL_NUM = UNIQUE_REPORT_VALUE_FOR_EL_UNIT(R_UNIT)
         IF(EL_NUM <= 0) THEN
            RPT_STR = ' '
         ELSE
            WRITE(RPT_STR,'(I5)') EL_NUM
         ENDIF
         EL_UNIQUE_RPT_STR = RPT_STR
      RETURN
      END
C***********************************************************************
      SUBROUTINE EL_UNIT_LIMIT_EXCEEDED
      use end_routine, only: end_program, er_message
C***********************************************************************
      INCLUDE 'SPINLIB.MON'
      INCLUDE 'SIZECOM.MON'
C
         WRITE(SCREEN_MESSAGES,'(A,I4,A)')
     +                       'More than ',MAX_EL_UNITS,' energy-limited'
         CALL MG_LOCATE_WRITE(20,0,TRIM(SCREEN_MESSAGES),
     +                                                   ALL_VERSIONS,0)
         CALL MG_LOCATE_WRITE(21,0,
     +                    'units are in the energy-limited units file.',
     +                                                   ALL_VERSIONS,0)
      er_message='stop requested from EL_OBJT SIID87'
      call end_program(er_message)
      END

