!     ******************************************************************
!     STARTOVL.FOR
!     Copyright(c) M.S. Gerber & Associates 2000
!
!     Created: 4/21/2003 3:04:58 PM
!     Author : MARK S GERBER
!     Last change: MSG 10/8/2012 3:22:29 PM
!     ******************************************************************

C***********************************************************************
C
C        THIS SUBROUTINE CONTROLS THE PRODUCTION OF OVERLAY FILES
C        COPYRIGHT (c) 1985 M.S. GERBER & ASSOCIATES, INC.
C
C***********************************************************************
C
      SUBROUTINE STARTOVL(YEARS_TO_RUN,START_YEAR,END_POINT_loc,
     +                    ENDPOINTS_RUN,*,
     +                    RUN_FUTURE_ASSETS)
      use msgoutpt
      USE ProSymModule
      use cl_data
      use SpinDriftLib
      use prod_arrays_dimensions
      USE PROD_ARRAYS_DIMENSIONS
      use sizecom
      use mod_base_year
      LOGICAL*1 LAHEY_LF95
      INTEGER*2 DELETE,YEAR_,CHECK_YEAR,END_POINT_loc,CHECK_END_POINT,
     +          RUN_YEARS,YEARS_TO_RUN,START_YEAR,
     +          TEST_END_POINT,SOURCE_FILE_END_POINT,
     +          ENDPOINTS_RUN,START_STUDY_ENDPOINT,LAST_STUDY_ENDPOINT
      CHARACTER*5 OVLNAME,SOURCE_FILE,TEMP_CHAR
      CHARACTER*7 TREE_TYPE
      CHARACTER*256 FILE_NAME,OUTPUT_DIRECTORY
      CHARACTER*50 OVL_RECORD
      LOGICAL*4 FILEXIST
      LOGICAL*1 EOF_FLAG,MATCH_FOUND,
     +          SET_END_OF_STUDY_FLAG_TRUE,
     +          GOOD_END_POINT,
     +          RUN_FUTURE_ASSETS,
     +          CAP_PLANNING_METHOD_ANNDECOMP,WVPA
      CHARACTER*2 CAPACITY_PLANNING_METHOD
      CHARACTER (LEN=256) ::  ProSym_Interface_File
      INTEGER (KIND=4) :: IOS
      integer (kind=2) :: year_loc
C TYPE DECLARATION FOR EXISTING OVERLAY FILES /FILEOVLY/
C     INCLUDE 'OVLYCOM.MON'
      SAVE year_loc,CHECK_YEAR,CHECK_END_POINT,OVLNAME,SOURCE_FILE,
     +     SOURCE_FILE_END_POINT,TEST_END_POINT,
     +     CAP_PLANNING_METHOD_ANNDECOMP
C
      CAP_PLANNING_METHOD_ANNDECOMP = CAPACITY_PLANNING_METHOD() == 'AN'
      CALL MG_LOCATE_WRITE(16,9,'Overlay file overhead.',
     +                                                   ALL_VERSIONS,1)
C
C OPEN OVERLAY FILE AND READ METAFILE HEADER RECORD
C
      FILE_NAME = trim(OUTPUT_DIRECTORY())//"OVL"//
     +      trim(ClData%scename)//".DAT"
      INQUIRE(FILE=FILE_NAME,EXIST=FILEXIST)
      IF(FILEXIST) THEN
         OPEN(28,FILE=FILE_NAME,ACCESS="SEQUENTIAL")
         READ(28,*,IOSTAT=IOS) DELETE
         FILEXIST = IOS == 0
      ENDIF
      IF(FILEXIST) THEN
         OVLNAME = ' '
         SOURCE_FILE = ' '
         TEST_END_POINT = -1
         FILEXIST = .FALSE.
         END_POINT_loc = START_STUDY_ENDPOINT() - 1
         IOS = 0
         DO WHILE (IOS == 0)
            READ(28,'(A)',IOSTAT=IOS) OVL_RECORD
            IF(IOS /= 0) EXIT
            OVL_RECORD = trim(OVL_RECORD)//',,,,,,,,,,,,,'
            READ(OVL_RECORD,*) DELETE,TEST_END_POINT,year_loc,OVLNAME,
     +                         SOURCE_FILE,SOURCE_FILE_END_POINT
            IF(TEST_END_POINT == START_STUDY_ENDPOINT())THEN
               FILEXIST = .TRUE.
               EXIT
            ENDIF
         ENDDO
      ENDIF
      IF(.NOT. FILEXIST) THEN
         YEARS_TO_RUN = RUN_YEARS()
         EOF_FLAG = SET_END_OF_STUDY_FLAG_TRUE()
         END_POINT_loc = START_STUDY_ENDPOINT()
         START_YEAR = 1
         IF(CAP_PLANNING_METHOD_ANNDECOMP) THEN
         ELSE
            WRITE(SCREEN_MESSAGES,'(A,I4,A)') 'Endpoint:',END_POINT_loc,
     +                                                      '-Base Case'
            CALL MG_CLEAR_LINE_WRITE(8,59,63,trim(SCREEN_MESSAGES),
     +                                                   ALL_VERSIONS,1)
         ENDIF
         RETURN
      ENDIF
      TEMP_CHAR = OVLNAME
      CALL UPC(TEMP_CHAR,OVLNAME)
      TEMP_CHAR = SOURCE_FILE
      CALL UPC(TEMP_CHAR,SOURCE_FILE)
      CHECK_YEAR = year_loc
      CHECK_END_POINT = TEST_END_POINT
      ENTRY GETOVLS(YEARS_TO_RUN,START_YEAR,END_POINT_loc,
     +              ENDPOINTS_RUN,*,
     +              RUN_FUTURE_ASSETS)
    1 CONTINUE
      CALL MG_LOCATE_WRITE(16,9,'Overlay file overhead.',
     +                                                   ALL_VERSIONS,1)
      IF(trim(SOURCE_FILE) /= ' ') THEN
         IF(CAP_PLANNING_METHOD_ANNDECOMP) THEN
         ELSE
            WRITE(SCREEN_MESSAGES,'(I4)') TEST_END_POINT
            CALL MG_CLEAR_LINE_WRITE(8,59,63,trim(SCREEN_MESSAGES),
     +                                                   ALL_VERSIONS,0)
            WRITE(SCREEN_MESSAGES,'(1X,A5,1X,I3)') trim(SOURCE_FILE),
     +                                             SOURCE_FILE_END_POINT
            CALL MG_CLEAR_LINE_WRITE(9,56,71,trim(SCREEN_MESSAGES),
     +                                                   ALL_VERSIONS,0)
         ENDIF
         IF(trim(SOURCE_FILE) == trim(ClData%Scename)) THEN
            IF(SOURCE_FILE_END_POINT < TEST_END_POINT) THEN
               TREE_TYPE = "CURRENT"
               CALL FROM_TREE(SOURCE_FILE_END_POINT,TEST_END_POINT,
     +                        SOURCE_FILE,
     +                        GOOD_END_POINT,TREE_TYPE)
            ELSE
               GOOD_END_POINT = .FALSE.
            ENDIF
         ELSE
            TREE_TYPE = "ANOTHER"
            CALL FROM_TREE(SOURCE_FILE_END_POINT,TEST_END_POINT,
     +                     SOURCE_FILE,
     +                     GOOD_END_POINT,TREE_TYPE)
         ENDIF
         IF(GOOD_END_POINT) THEN
            ENDPOINTS_RUN = ENDPOINTS_RUN + 1
            CALL STORE_END_POINT(ENDPOINTS_RUN)
    2       OVLNAME = ' '
            SOURCE_FILE = ' '
            READ(28,'(A)',END=30) OVL_RECORD
            OVL_RECORD = trim(OVL_RECORD)//',,,'
            READ(OVL_RECORD,*) DELETE,TEST_END_POINT,year_loc,OVLNAME,
     +                         SOURCE_FILE,SOURCE_FILE_END_POINT
            year_loc = 1990
            TEMP_CHAR = OVLNAME
            CALL UPC(TEMP_CHAR,OVLNAME)
            TEMP_CHAR = SOURCE_FILE
            CALL UPC(TEMP_CHAR,SOURCE_FILE)
            IF(TEST_END_POINT == CHECK_END_POINT) GOTO 2
            CHECK_YEAR = year_loc
            CHECK_END_POINT = TEST_END_POINT
            GOTO 1
         ENDIF
      ENDIF
      GOOD_END_POINT = .FALSE.
      CALL MG_CLEAR_LINE_WRITE(9,56,71,' Base Case',ALL_VERSIONS,1)
      DO
         IF(TEST_END_POINT == CHECK_END_POINT) THEN
            IF(CAP_PLANNING_METHOD_ANNDECOMP) THEN
                !Don't do anything.
                ! Todo:  Fix this if statement so that this empty
                ! block no longer exists.
            ELSE
               WRITE(SCREEN_MESSAGES,'(I4)') TEST_END_POINT
               CALL MG_CLEAR_LINE_WRITE(8,59,63,trim(SCREEN_MESSAGES),
     +                                                   ALL_VERSIONS,0)
               IF(trim(OVLNAME) /= ' ') THEN
                  SCREEN_MESSAGES = 'Overlay family: '//OVLNAME
                  CALL MG_CLEAR_LINE_WRITE(8,59,63,
     +                           trim(SCREEN_MESSAGES),ALL_VERSIONS,1)
               ENDIF
            ENDIF
            IF(trim(OVLNAME)/=' ') THEN
               CALL FINDOVLS(OVLNAME,MATCH_FOUND,
     +                                 TEST_END_POINT,RUN_FUTURE_ASSETS)
            ENDIF
            IF(INDEX(ProSym_Interface_File(),'NONE') == 0 .AND.
     +                                                .NOT. WVPA()) THEN
               CALL ProSymInputData(.FALSE.,OVLNAME)
            ENDIF
            GOOD_END_POINT = .TRUE.
            OVLNAME = ' '
            SOURCE_FILE = ' '
            READ(28,'(A)',END=20) OVL_RECORD
            OVL_RECORD = trim(OVL_RECORD)//',,,'
            READ(OVL_RECORD,*) DELETE,TEST_END_POINT,year_loc,OVLNAME,
     +                         SOURCE_FILE,SOURCE_FILE_END_POINT

            TEMP_CHAR = OVLNAME
            CALL UPC(TEMP_CHAR,OVLNAME)
            TEMP_CHAR = SOURCE_FILE
            CALL UPC(TEMP_CHAR,SOURCE_FILE)
         ELSE
C
C NOTE AN INFINITE LOOP HAPPENS IF A GOOD END POINT IS NOT FOUND
C
            IF(.NOT. GOOD_END_POINT) GOTO 1
            IF(TEST_END_POINT /= CHECK_END_POINT) THEN
               YEARS_TO_RUN = RUN_YEARS()
            ELSE
               YEARS_TO_RUN = year_loc - get_base_year() - 1
            ENDIF
            START_YEAR = 1
            CHECK_YEAR = year_loc
            EXIT
         ENDIF
      ENDDO
      CALL MG_CLEAR_LINE(16,9,36,ALL_VERSIONS,0)
      CALL MG_CLEAR_LINE(17,9,36,ALL_VERSIONS,0)
      END_POINT_loc = CHECK_END_POINT
      CHECK_END_POINT = TEST_END_POINT
      IF(END_POINT_loc < LAST_STUDY_ENDPOINT()) RETURN
   20 EOF_FLAG = SET_END_OF_STUDY_FLAG_TRUE()
      CALL MG_CLEAR_LINE(16,9,36,ALL_VERSIONS,0)
      CALL MG_CLEAR_LINE(17,9,36,ALL_VERSIONS,0)
      IF(GOOD_END_POINT) THEN
         YEARS_TO_RUN = RUN_YEARS()
         START_YEAR = 1 
         ! CHECK_END_POINT
         END_POINT_loc = MIN(END_POINT_loc+1,LAST_STUDY_ENDPOINT()) 
         RETURN
      ENDIF
   30 RETURN 1
      END subroutine startovl
C***********************************************************************
C
C   THIS SUBROUTINE GETS END POINT RESULTS FROM THE EITHER THE CURRENT
C     TREE OR ANOTHER TREE.
C        COPYRIGHT (c) 1990 M.S. GERBER & ASSOCIATES, INC.
C
C***********************************************************************
C
      SUBROUTINE FROM_TREE(SOURCE_FILE_END_POINT,TEST_END_POINT,
     +                     SOURCE_FILE,
     +                     GOOD_END_POINT,TREE_TYPE)
      use mod_base_year
      use msgoutpt
      implicit none
      LOGICAL*1 GOOD_END_POINT
      INTEGER*2 SOURCE_FILE_END_POINT,TEST_END_POINT
      INTEGER*2 RUN_YEARS
      INTEGER*4 TEMP_OUTPUT_RECORD,INPUT_REC,yr_loc,
     +          OUT_REC_LENGTH
      INTEGER*2 REC_LENGTH,END_POINTS,FIRST_YEAR,LAST_YEAR,
     +   READ_DEVICE_NUM,PRIOR_YEARS,INPUT_RUN_YEARS
      CHARACTER*7 TREE_TYPE
      CHARACTER*5 SOURCE_FILE
      CHARACTER*256 FILE_NAME,OUTPUT_DIRECTORY
      LOGICAL*4 FILE_EXISTS
      CHARACTER*1 DATA_RECORD(:)
      ALLOCATABLE :: DATA_RECORD
      REAL VARIABLE,OUT_END_POINT,END_POINT_SOURCE_FILE
C
      GOOD_END_POINT = .FALSE.
      READ_DEVICE_NUM = 0
C
      INQUIRE(UNIT=38,RECL=OUT_REC_LENGTH)
      FILE_NAME = trim(OUTPUT_DIRECTORY())//"BIP"//
     +                                       trim(SOURCE_FILE)//'.BIN'
      IF(TREE_TYPE .EQ. "ANOTHER") THEN
         INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
         IF(.NOT. FILE_EXISTS) GOTO 100
         READ_DEVICE_NUM = 10
         OPEN(READ_DEVICE_NUM,FILE=FILE_NAME,ACCESS="TRANSPARENT",
     +                                                  STATUS="OLD")
         READ(READ_DEVICE_NUM,REC=2)
     +                     REC_LENGTH,END_POINTS,FIRST_YEAR,LAST_YEAR
         CLOSE(READ_DEVICE_NUM)
         IF(FIRST_YEAR > get_base_year() .OR.
     +               LAST_YEAR < get_base_year()+RUN_YEARS() .OR.
     +                   END_POINTS < SOURCE_FILE_END_POINT) GOTO 100
         OPEN(READ_DEVICE_NUM,FILE=FILE_NAME,ACCESS="DIRECT",
     +                             STATUS="OLD",RECL=REC_LENGTH)
         PRIOR_YEARS = get_base_year() - FIRST_YEAR
         INPUT_RUN_YEARS = LAST_YEAR - FIRST_YEAR
      ELSE
         READ_DEVICE_NUM = 38
         PRIOR_YEARS = 0
         REC_LENGTH = OUT_REC_LENGTH
         INPUT_RUN_YEARS = RUN_YEARS()
      ENDIF
C
      TEMP_OUTPUT_RECORD = OUTPUT_RECORD()
      REC_LENGTH = REC_LENGTH - 4
      END_POINT_SOURCE_FILE = FLOAT(SOURCE_FILE_END_POINT)
      OUT_END_POINT = FLOAT(TEST_END_POINT)
      INPUT_REC = (INT(SOURCE_FILE_END_POINT)-1) *
     +               (INT(INPUT_RUN_YEARS)+1) + INT(PRIOR_YEARS)
C
C     CALL TO READ EXISTING TREE
C
      ALLOCATE (DATA_RECORD(MIN(REC_LENGTH,OUT_REC_LENGTH-4)))
      DO yr_loc = 1, RUN_YEARS() + 1
         READ(READ_DEVICE_NUM,REC=INPUT_REC+yr_loc,ERR=95) VARIABLE,
     +                                         DATA_RECORD
         IF(VARIABLE .NE. END_POINT_SOURCE_FILE) GOTO 95

         WRITE(38,REC=TEMP_OUTPUT_RECORD) OUT_END_POINT,
     +                                         DATA_RECORD
         TEMP_OUTPUT_RECORD = TEMP_OUTPUT_RECORD + 1
      ENDDO
      IF(READ_DEVICE_NUM .EQ. 10) CLOSE(10)
      GOOD_END_POINT = .TRUE.
      TEMP_OUTPUT_RECORD = SET_OUTPUT_RECORD(TEMP_OUTPUT_RECORD-1)
      DEALLOCATE (DATA_RECORD)
      RETURN
   95 DEALLOCATE (DATA_RECORD)
  100 GOOD_END_POINT = .FALSE.
      IF(READ_DEVICE_NUM .EQ. 10) CLOSE(10)
      RETURN
      END


