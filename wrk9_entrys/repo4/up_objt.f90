!     ******************************************************************
!     UP_OBJT.FOR
!     Created: 11/11/02 10:23:26 AM
!     Author : msg
!     Last change: msg 10/11/2016 6:13:43 PM
!     ******************************************************************

      MODULE UNIT_PARAMETER_FILE_VARIABLES
         INTEGER (KIND=2), PARAMETER :: MAX_UP_FILES=10,LRECL_UP=156
         CHARACTER (LEN=2) :: UP_OL(0:MAX_UP_FILES-1)
         CHARACTER (LEN=2) :: UP_FILE_CODES(0:MAX_UP_FILES-1)=(/ &
                     'UP','U1','U2','U3','U4','U5','U6','U7','U8','U9'/)
        CHARACTER (LEN=11):: UP_FILE_BINARY_NAMES(0:MAX_UP_FILES-1)=(/ &
                                                'UNTPR      ', &
                                                'UNIT_PARAM1', &
                                                'UNIT_PARAM2', &
                                                'UNIT_PARAM3', &
                                                'UNIT_PARAM4', &
                                                'UNIT_PARAM5', &
                                                'UNIT_PARAM6', &
                                                'UNIT_PARAM7', &
                                                'UNIT_PARAM8', &
                                                'UNIT_PARAM9'/)
         LOGICAL (KIND=1) :: UP_BASE_FILE_ACTIVE(0:MAX_UP_FILES-1)
         INTEGER (KIND=2) :: BASE_MAX_VECTOR_NUM(0:MAX_UP_FILES-1), &
                             OVLY_MAX_VECTOR_NUM(0:MAX_UP_FILES-1)
         INTEGER (KIND=4) :: BASE_INPUT_RECORDS(0:MAX_UP_FILES-1)=0, &
                             OVERLAY_INPUT_RECORDS(0:MAX_UP_FILES-1)=0
      END MODULE
! ***********************************************************************
      SUBROUTINE UP_OBJECT
      use end_routine, only: end_program, er_message
	  use miscmod
! ***********************************************************************
!
      USE UNIT_PARAMETER_FILE_VARIABLES
      INCLUDE 'SpinLib.MON'
      USE SIZECOM
      INTEGER(kind=2) :: I,IREC,OL_REC,INUNIT,DELETE,DUMMY, &
                UNIT_NO, &
                VECTOR_NO
      INTEGER :: IOS
      INTEGER(kind=2) :: LAST_INPUT_RECORD=0
      LOGICAL(kind=1) :: VOID_LOGIC,VARIABLE_UNITS_OBJECT, &
                         ACTIVE_GRX_RPS_VECTOR
      LOGICAL(kind=4) :: FILE_EXISTS
      CHARACTER(len=5) :: OVERLAY_FAMILY_NAME,UNIT_PARMS
      CHARACTER(len=256) :: FILE_NAME,FILE_NAME2
      CHARACTER(len=256) :: BASE_FILE_DIRECTORY,OUTPUT_DIRECTORY
!  DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER(len=1024) :: RECLN
!  DECLARATION FOR GENERATING UNIT PARAMETERS
      REAL :: VALUES_30(30)
      CHARACTER(len=40) :: DESCRIPTION
      CHARACTER(len=26) :: FILE_TYPE='Variable Unit Parameters'
      CHARACTER(len=30) :: SCREEN_OUTPUT
!
!  VARIABLE ADDITIONS AND CHANGES 02/18/07 DR.G
!
      INTEGER (KIND=2) :: FILE_NO
      INTEGER (KIND=4) :: R_FILE_NO ,MAX_VECTOR_NUM
      CHARACTER (LEN=5) :: UP_FILE_BASE_NAMES(0:MAX_UP_FILES-1), &
                           VOID_CHR
!
! ***********************************************************************
      ENTRY UP_MAKEBIN
! ***********************************************************************
!
      UP_OL = 'BC'
      BASE_MAX_VECTOR_NUM = 0
      UP_BASE_FILE_ACTIVE = .FALSE.
      VOID_CHR = UNIT_PARMS(UP_FILE_BASE_NAMES)
      DO FILE_NO = 0, MAX_UP_FILES-1
      	FILE_NAME = TRIM(BASE_FILE_DIRECTORY())// &
                        TRIM(UP_FILE_CODES(FILE_NO))//"B"// &
                           TRIM(UP_FILE_BASE_NAMES(FILE_NO))//".DAT"
         INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
         IF(.NOT. FILE_EXISTS) CYCLE
         SCREEN_OUTPUT = TRIM(FILE_TYPE)//'-'// &
                                          UP_FILE_BASE_NAMES(FILE_NO)
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         OPEN(10,FILE=FILE_NAME)
         OPEN(11,FILE=TRIM(OUTPUT_DIRECTORY())//"BC"// &
              TRIM(UP_FILE_BINARY_NAMES(FILE_NO))//".BIN", &
                     ACCESS="DIRECT",STATUS="REPLACE",RECL=LRECL_UP)
         UP_BASE_FILE_ACTIVE(FILE_NO) = .TRUE.



         IREC = 0
         READ(10,*) DELETE
         DO
            READ(10,1000,IOSTAT=IOS) RECLN
            IF(IOS /= 0) EXIT
            IF(RECLN(1:1) == '7') CYCLE
            RECLN = TRIM(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
            VALUES_30 = -999999.
            READ(RECLN,*,ERR=200) DELETE,DESCRIPTION,VECTOR_NO, &
                                                            VALUES_30
            VECTOR_NO = ABS(VECTOR_NO)
            BASE_MAX_VECTOR_NUM(FILE_NO) = MAX(VECTOR_NO, &
                                        BASE_MAX_VECTOR_NUM(FILE_NO))
            IREC = IREC + 1
            IF(VALUES_30(1) == -999999.) VALUES_30(1) = 0.
            DO I = 2, AVAIL_DATA_YEARS
               IF(VALUES_30(I)==-999999.) VALUES_30(I)=VALUES_30(I-1)
            ENDDO
            WRITE(11,REC=IREC) VECTOR_NO,VALUES_30,DELETE
            VECTOR_NO = VECTOR_NO + 1
         ENDDO
         CLOSE(10)
         CLOSE(11)
         BASE_INPUT_RECORDS(FILE_NO) = IREC
      ENDDO ! FILES
      RETURN
!
!  OVERLAY THE GENERATING-UNIT PARAMETER FILE
!
! ***********************************************************************
      ENTRY UP_MAKEOVL(OVERLAY_FAMILY_NAME,R_FILE_NO)
! ***********************************************************************
!
      IF(.NOT. UP_BASE_FILE_ACTIVE(R_FILE_NO)) RETURN
      SCREEN_OUTPUT = TRIM(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
      CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      FILE_NO = R_FILE_NO
      MAX_VECTOR_NUM = OVLY_MAX_VECTOR_NUM(FILE_NO)
      IF(UP_OL(R_FILE_NO) == 'BC') THEN
         FILE_NAME = TRIM(OUTPUT_DIRECTORY())//"BC"// &
                             TRIM(UP_FILE_BINARY_NAMES(FILE_NO))//".BIN"
         FILE_NAME2 = TRIM(OUTPUT_DIRECTORY())//"OL"// &
                             TRIM(UP_FILE_BINARY_NAMES(FILE_NO))//".BIN"
         CALL COPY_FILE_2_FILE(FILE_NAME,FILE_NAME2)
         MAX_VECTOR_NUM = BASE_MAX_VECTOR_NUM(FILE_NO)
         UP_OL(FILE_NO) = 'OL'
      ENDIF
      OPEN(12,FILE=TRIM(OUTPUT_DIRECTORY())//"OL"// &
              TRIM(UP_FILE_BINARY_NAMES(FILE_NO))//".BIN", &
                     ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL_UP)
!
      OPEN(10,FILE=TRIM(OUTPUT_DIRECTORY())//UP_FILE_CODES(FILE_NO) &
                               //"O"//TRIM(OVERLAY_FAMILY_NAME)//".DAT")
      READ(10,*) DELETE
      OL_REC = 1
      DO
         READ(10,1000,IOSTAT=IOS) RECLN
         IF(IOS /= 0) EXIT
         IF(RECLN(1:1) == '7') CYCLE
         RECLN = TRIM(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
         READ(12,REC=OL_REC,IOSTAT=IOS) VECTOR_NO,VALUES_30
         IF(IOS /= 0) EXIT
         READ(RECLN,*,ERR=200) DELETE,DESCRIPTION,VECTOR_NO,VALUES_30
         VECTOR_NO = ABS(VECTOR_NO)
         MAX_VECTOR_NUM = MAX(MAX_VECTOR_NUM,int(VECTOR_NO,4))
         WRITE(12,REC=OL_REC) VECTOR_NO,VALUES_30,DELETE
         OL_REC = OL_REC + 1
      ENDDO
      CLOSE(10)
      CLOSE(12)
      OVLY_MAX_VECTOR_NUM(FILE_NO) = MAX_VECTOR_NUM
      OVERLAY_INPUT_RECORDS(FILE_NO) = OL_REC - 1
      RETURN
!
! ***********************************************************************
      ENTRY RESET_UPE_OL
! ***********************************************************************
         UP_OL(:) = 'BC'
         OVLY_MAX_VECTOR_NUM(:) = 0
         OVERLAY_INPUT_RECORDS(:) = 0
      RETURN
! ***********************************************************************
      ENTRY WRITE_GRX_RPS_CAPACITY
! ***********************************************************************
         IF(.TRUE.) THEN
            VOID_CHR = UNIT_PARMS(UP_FILE_BASE_NAMES)
            FILE_NO = 9
      	   FILE_NAME = TRIM(BASE_FILE_DIRECTORY())// &
                           "U9B"// &
                           TRIM(UP_FILE_BASE_NAMES(FILE_NO))//".DAT"
            INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
            IF(FILE_EXISTS) THEN
               OPEN(10,FILE=FILE_NAME)
               OPEN(11,FILE=TRIM(OUTPUT_DIRECTORY())//"U9OXXXXX.DAT")
!              READ(10,*) DELETE
               DO
                  READ(10,1000,IOSTAT=IOS) RECLN
                  IF(IOS /= 0) EXIT
                  IF(RECLN(1:1) == '7') THEN
                     WRITE(11,1000) RECLN ! '7, ,'
                  ELSEIF(RECLN(1:1) == '9') THEN
                     WRITE(11,1000) RECLN ! '9, ,'
                  ELSE
                     RECLN = &
                            TRIM(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
                     VALUES_30 = -999999.
                     READ(RECLN,*,ERR=200) DELETE,DESCRIPTION,VECTOR_NO, &
                                                            VALUES_30
                     IF(ACTIVE_GRX_RPS_VECTOR(VECTOR_NO,VALUES_30)) THEN
                        WRITE(11,1015) DELETE, &
                                          TRIM(DESCRIPTION), &
                                          VECTOR_NO, &
                                          VALUES_30
!                        RECLN = TRIM(RECLN)
!                        WRITE(11,*) RECLN
                     ELSE
                        WRITE(11,1020) DELETE,TRIM(DESCRIPTION), &
                               VECTOR_NO,',,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
                     ENDIF
                  ENDIF
               ENDDO
               CLOSE(10)
               CLOSE(11)
            ENDIF
         ENDIF
      RETURN
 1015 FORMAT(I2,',"',A,'",',I5,30(',',F9.2))
 1020 FORMAT(I2,',"',A,'",',I5,',',A)
! ***********************************************************************
  200 CALL MG_LOCATE_WRITE(20,0,TRIM(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from UP_OBJT SIID338'
      call end_program(er_message)
 1000 FORMAT(A)
 1010 FORMAT('&',A)
      END
!
! ***********************************************************************
      SUBROUTINE OPEN_VAR_UNITS_FILE
! ***********************************************************************
!
      LOGICAL(kind=1) :: VARIABLE_UNITS_OBJECT, VOID_LOGIC
!
         VOID_LOGIC = VARIABLE_UNITS_OBJECT()
      RETURN
!
      END
!
! ***********************************************************************
      FUNCTION VARIABLE_UNITS_OBJECT()
      use end_routine, only: end_program, er_message
! ***********************************************************************
!
      USE UNIT_PARAMETER_FILE_VARIABLES
      INCLUDE 'SpinLib.MON'
      USE SIZECOM
!
!  CONTRACT ENERGY PATTERNS
!
      REAL :: GET_MONTHLY_VAR_SUM
      INTEGER(kind=2) :: R_VECTOR_NUM,MO,DELETE
      REAL :: R_MONTHLY_VECTOR(12),GET_MONTHLY_VAR_VALUES
!
      REAL :: GET_VECTOR
      REAL :: GET_EL_MW_FROM_POINTR, &
           GET_CL_MW_FROM_POINTR, &
           GET_CT_MW_FROM_POINTR, &
           GET_VAR
      CHARACTER(len=*) UNITNM
      INTEGER(kind=2) :: ACTIVE_VECTORS, &
                VECTOR_NUMBER,I,J,YR
      LOGICAL(kind=1) :: VARIABLE_UNITS_OBJECT
      REAL (kind=4) :: VECTOR_VALUES(:,:)
      LOGICAL(kind=1) :: VECTOR_POINTER_ALLOCATED=.FALSE.
      INTEGER(kind=2) :: VECTOR_POINTR(:),MAX_VALID_VECTOR=-999
      ALLOCATABLE :: VECTOR_VALUES,VECTOR_POINTR
      SAVE VECTOR_VALUES,VECTOR_POINTR
      CHARACTER (LEN=256) :: OUTPUT_DIRECTORY
      INTEGER (KIND=4) :: RECORDS_IN_FILE(0:MAX_UP_FILES-1),REC_READ
      INTEGER (KIND=2) :: FILE_NO

!
         IF(COUNT(UP_BASE_FILE_ACTIVE(:)) == 0) RETURN
         RECORDS_IN_FILE = 0
         MAX_VALID_VECTOR = 0
         DO FILE_NO = 0, MAX_UP_FILES-1
            IF(UP_OL(FILE_NO) == 'OL') THEN
               RECORDS_IN_FILE(FILE_NO) = OVERLAY_INPUT_RECORDS(FILE_NO)
               MAX_VALID_VECTOR = MAX(OVLY_MAX_VECTOR_NUM(FILE_NO), &
                                                       MAX_VALID_VECTOR)
            ELSE
               RECORDS_IN_FILE(FILE_NO) = BASE_INPUT_RECORDS(FILE_NO)
               MAX_VALID_VECTOR = MAX(BASE_MAX_VECTOR_NUM(FILE_NO),            & !
                                                       MAX_VALID_VECTOR)
            ENDIF
         ENDDO
         ACTIVE_VECTORS = SUM(RECORDS_IN_FILE(:))
         IF(ACTIVE_VECTORS == 0) RETURN
         IF(ALLOCATED(VECTOR_VALUES)) THEN
            DEALLOCATE(VECTOR_VALUES,VECTOR_POINTR)
         ENDIF
         ALLOCATE(VECTOR_VALUES(ACTIVE_VECTORS,AVAIL_DATA_YEARS), &
                  VECTOR_POINTR(MAX_VALID_VECTOR))
         VECTOR_POINTER_ALLOCATED = .TRUE.
         VECTOR_POINTR = 0
         REC_READ = 1
         DO FILE_NO = 0, MAX_UP_FILES-1
            IF(.NOT. UP_BASE_FILE_ACTIVE(FILE_NO)) CYCLE
            OPEN(10,FILE=TRIM(OUTPUT_DIRECTORY())// &
                         TRIM(UP_OL(FILE_NO))// &
                         TRIM(UP_FILE_BINARY_NAMES(FILE_NO))//".BIN", &
                    ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL_UP)
            DO I = 1, RECORDS_IN_FILE(FILE_NO)
               READ(10,REC=I) VECTOR_NUMBER, &
                              VECTOR_VALUES(REC_READ,:), &
                              DELETE
               IF(DELETE > 7) CYCLE
               VECTOR_NUMBER = ABS(VECTOR_NUMBER)
               IF(VECTOR_POINTR(VECTOR_NUMBER) == 0) THEN
                  VECTOR_POINTR(VECTOR_NUMBER) = REC_READ
                  REC_READ = REC_READ + 1
               ELSE
                  WRITE(4,*) "DUPLICATE VECTOR NUMBER ",VECTOR_NUMBER
                  WRITE(4,*) "IN UNIT PARAMETER FILE WITH CODE ", &
                                                  UP_FILE_CODES(FILE_NO)
                  WRITE(4,*) "CHECK BASE AND OVERLAY FILES "
                  WRITE(4,*) "AT COLUMN POSITION ",I
               ENDIF
            ENDDO
            CLOSE(10)
         ENDDO
         VARIABLE_UNITS_OBJECT = .TRUE.
      RETURN
!
! ***********************************************************************
      ENTRY GET_CL_MW_FROM_POINTR(GET_VECTOR,YR)
! ***********************************************************************
!
         IF(VECTOR_POINTER_ALLOCATED) THEN
            VECTOR_NUMBER = INT(ABS(GET_VECTOR))
            GET_CL_MW_FROM_POINTR = 0.
            IF(VECTOR_NUMBER > 0 .AND. &
                                 VECTOR_NUMBER <= MAX_VALID_VECTOR) THEN
               VECTOR_NUMBER = VECTOR_POINTR(VECTOR_NUMBER)
               IF(VECTOR_NUMBER /= 0) GET_CL_MW_FROM_POINTR = &
                   VECTOR_VALUES(VECTOR_NUMBER,MIN(YR,AVAIL_DATA_YEARS))
            ELSE
!
               GET_CL_MW_FROM_POINTR = 1.0 ! 5/6/02. Per Mark Gerber.
!
               WRITE(4,*) 'Invalid capacity pointer',GET_VECTOR, &
                          ' in Capacity Limited file.'
            ENDIF
         ELSE
   er_message='SIID340 - Unit Parameter File NOT Active for CL pointer.'
            call end_program(er_message)
         ENDIF
      RETURN
!
! ***********************************************************************
      ENTRY GET_EL_MW_FROM_POINTR(GET_VECTOR,YR)
! ***********************************************************************
!
         IF(VECTOR_POINTER_ALLOCATED) THEN
            VECTOR_NUMBER = INT(ABS(GET_VECTOR))
            GET_EL_MW_FROM_POINTR = 0.
            IF(VECTOR_NUMBER > 0 .AND. &
                                 VECTOR_NUMBER <= MAX_VALID_VECTOR) THEN
               VECTOR_NUMBER = VECTOR_POINTR(VECTOR_NUMBER)
               IF(VECTOR_NUMBER /= 0) GET_EL_MW_FROM_POINTR = &
                   VECTOR_VALUES(VECTOR_NUMBER,MIN(YR,AVAIL_DATA_YEARS))
            ELSE
               WRITE(4,*) 'Invalid capacity pointer',GET_VECTOR, &
                          ' in Energy Limited file.'
            ENDIF
         ELSE
            er_message='SIID341 - Unit parameter file not active'
            call end_program(er_message)
         ENDIF
      RETURN
!
! ***********************************************************************
      ENTRY GET_CT_MW_FROM_POINTR(GET_VECTOR,YR)
! ***********************************************************************
!
         IF(VECTOR_POINTER_ALLOCATED) THEN
            VECTOR_NUMBER = INT(ABS(GET_VECTOR))
            GET_CT_MW_FROM_POINTR = 0.
            IF(VECTOR_NUMBER > 0 .AND. &
                                 VECTOR_NUMBER <= MAX_VALID_VECTOR) THEN
               VECTOR_NUMBER = VECTOR_POINTR(VECTOR_NUMBER)
               IF(VECTOR_NUMBER /= 0) GET_CT_MW_FROM_POINTR = &
                   VECTOR_VALUES(VECTOR_NUMBER,MIN(YR,AVAIL_DATA_YEARS))
            ELSE
               WRITE(4,*) 'Invalid capacity pointer',GET_VECTOR, &
                          ' in Contracts file.'
            ENDIF
         ELSE
            er_message='SIID342 - Unit parameter file not active'
            call end_program(er_message)
         ENDIF
      RETURN
! **********************************************************************
!
!              FUNCTION TO READ VARIABLE UNIT PARAMETERS FOR MIDAS
!                    COPYRIGHT (C) 1986
!                         M.S. GERBER & ASSOCIATES, INC.
!                               ALL RIGHTS RESERVED
!
! **********************************************************************
!
      ENTRY GET_VAR(GET_VECTOR,YR,UNITNM)
!
         IF(VECTOR_POINTER_ALLOCATED) THEN
            VECTOR_NUMBER = INT(ABS(GET_VECTOR))
            GET_VAR = 0.
            IF(VECTOR_NUMBER > 0 .AND. &
                                 VECTOR_NUMBER <= MAX_VALID_VECTOR) THEN
               VECTOR_NUMBER = VECTOR_POINTR(VECTOR_NUMBER)
               IF(VECTOR_NUMBER /= 0) GET_VAR = &
                   VECTOR_VALUES(VECTOR_NUMBER,MIN(YR,AVAIL_DATA_YEARS))
            ELSE
               WRITE(4,*) 'Pointer ',GET_VECTOR,' does not exist'// &
                          ' for '//TRIM(UNITNM)
            ENDIF
         ELSE
            er_message='SIID343 - Unit parameter file not active'
            call end_program(er_message)
         ENDIF
      RETURN
! ***********************************************************************
      ENTRY GET_MONTHLY_VAR_SUM(R_VECTOR_NUM)
! ***********************************************************************
!
         IF(VECTOR_POINTER_ALLOCATED) THEN
            GET_MONTHLY_VAR_SUM = 0.
            VECTOR_NUMBER = ABS(R_VECTOR_NUM)
            IF(VECTOR_NUMBER > 0 .AND. &
                                 VECTOR_NUMBER <= MAX_VALID_VECTOR) THEN
               VECTOR_NUMBER = VECTOR_POINTR(VECTOR_NUMBER)
               IF(VECTOR_NUMBER /= 0) THEN
                  DO MO = 1, 12
                     GET_MONTHLY_VAR_SUM = GET_MONTHLY_VAR_SUM + &
                                         VECTOR_VALUES(VECTOR_NUMBER,MO)
                  ENDDO
               ENDIF
            ELSE
               WRITE(4,*) 'Contract energy pattern ',GET_VECTOR, &
                                                     ' does not exist.'
            ENDIF
         ELSE
            er_message='SIID344 - Unit parameter file not active'
            call end_program(er_message)
         ENDIF
      RETURN
! ***********************************************************************
      ENTRY GET_MONTHLY_VAR_VALUES(R_VECTOR_NUM,R_MONTHLY_VECTOR)
! ***********************************************************************
!
         IF(VECTOR_POINTER_ALLOCATED) THEN
            VECTOR_NUMBER = ABS(R_VECTOR_NUM)
            GET_MONTHLY_VAR_VALUES = 0.
            IF(VECTOR_NUMBER > 0 .AND. &
                                 VECTOR_NUMBER <= MAX_VALID_VECTOR) THEN
               VECTOR_NUMBER = VECTOR_POINTR(VECTOR_NUMBER)
               IF(VECTOR_NUMBER /= 0) THEN
                  DO MO = 1, 12
                     R_MONTHLY_VECTOR(MO) = &
                                         VECTOR_VALUES(VECTOR_NUMBER,MO)
                  ENDDO
                  GET_MONTHLY_VAR_VALUES = GET_MONTHLY_VAR_VALUES/12.
               ENDIF
            ELSE
               WRITE(4,*) 'Monthly data vector ',VECTOR_NUMBER, &
                                                     ' does not exist.'
            ENDIF
         ELSE
            er_message='SIID345 - Unit parameter file not active'
            call end_program(er_message)
         ENDIF
      RETURN
! ***********************************************************************
      END

