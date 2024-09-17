module RptRecControl
implicit none
contains

!     ******************************************************************
!     RptRecControl.F95
!     Copyright(c)  2000
!
!     Created: 1/5/2010 11:27:30 AM
!     Author : MARK S GERBER
!     Last change: MSG 4/6/2010 11:33:00 AM
!     ******************************************************************
! routines to provide correct output write record and end point number
! used becasue of iterations in GRX routines
      INTEGER FUNCTION GRX_HORIZON_REPORTING(YR_ITER)
      USE GRX_PLANNING_ROUTINES
      use irec_endpoint_control
         REAL (KIND=4):: YR_ITER(2)
         CHARACTER (LEN=2) :: CAPACITY_PLANNING_METHOD,GREEN_MRX_METHOD
         GRX_HORIZON_REPORTING = 1
         IF(CAPACITY_PLANNING_METHOD() == 'MX'  .AND. &
                            GREEN_MRX_METHOD() == 'GX' .AND. &
                                            .NOT. HORIZONS_ACTIVE) THEN
             YR_ITER(2) = FLOAT(GRX_ITERATIONS)
             GRX_HORIZON_REPORTING = 2
         ENDIF
      END FUNCTION GRX_HORIZON_REPORTING



      INTEGER FUNCTION RPTREC(UNITNO,SAVE_REC,REC_OPT,FILE_OPT,CURRENT_REC, &
                              REC_LENGHT,FILE_NAME)
	  USE flusher
         INTEGER (KIND=2), OPTIONAL :: UNITNO,REC_LENGHT
         INTEGER (KIND=4), OPTIONAL :: SAVE_REC
         CHARACTER (LEN=1), OPTIONAL :: REC_OPT,FILE_OPT
         LOGICAL (KIND=4), OPTIONAL :: CURRENT_REC
         CHARACTER (LEN=*), OPTIONAL :: FILE_NAME
         INTEGER (KIND=4) :: NEXT_REC(0:32000)=0, &
		                     SAVED_REC(0:32000)=0,IREC_SAVED
         LOGICAL (KIND=1) :: FILE_OPEN(0:32000)=.false.,SIZE_FILE
         INTEGER (KIND=2) :: REC_LEN(0:32000)=0
         CHARACTER (LEN=256) :: OPEN_FILE_NAME(0:32000)=" "
         INTEGER (KIND=4) :: I,FILE_SIZE
         CHARACTER (LEN=2) :: CAPACITY_PLANNING_METHOD,GREEN_MRX_METHOD
         INTEGER (KIND=2) :: RPT_DIMENSION(0:32000)=0
!-----
         IF(PRESENT(FILE_NAME).AND. PRESENT(UNITNO)) THEN
            OPEN_FILE_NAME(UNITNO) = FILE_NAME
            FILE_OPEN(UNITNO) = .TRUE.
            RPT_DIMENSION(UNITNO) = SAVE_REC
         ENDIF
         IF(PRESENT(REC_LENGHT).AND. PRESENT(UNITNO)) THEN
            REC_LEN(UNITNO) = REC_LENGHT
         ENDIF
         IF(PRESENT(CURRENT_REC).AND. PRESENT(UNITNO)) THEN
            RPTREC = NEXT_REC(UNITNO)
            RETURN
         ENDIF
         IF(PRESENT(REC_OPT)) THEN
            IF(REC_OPT == 'R'.AND. PRESENT(UNITNO)) THEN  ! RESET THIS REPORT
               NEXT_REC(UNITNO) = SAVED_REC(UNITNO)
               RPTREC = NEXT_REC(UNITNO)
            ELSEIF(REC_OPT == 'A') THEN  ! RESET all report start RECORDS
               NEXT_REC= SAVED_REC
               RPTREC = 0
            ELSEIF(REC_OPT == 'U') THEN ! UPDATE  START RECORDS FOR ALL REPTS
              SAVED_REC = NEXT_REC
              RPTREC = 0
            ENDIF
            RETURN
         ENDIF
         IF(PRESENT(FILE_OPT)) THEN
            SIZE_FILE = CAPACITY_PLANNING_METHOD() == 'MX'  .AND.     &
                                              GREEN_MRX_METHOD() == 'GX'
            IF(FILE_OPT == 'F' .AND. PRESENT(UNITNO)) &
			CALL flush_unit(int(UNITNO,2))
            IF(FILE_OPT == 'A') THEN
               DO I = 1, 32000
                  IF(FILE_OPEN(I)) THEN
                     CALL flush_unit(int(I,2))
                  ENDIF
               ENDDO
            ENDIF
            IF(FILE_OPT == 'S'.AND. PRESENT(UNITNO)) THEN
               IF(FILE_OPEN(UNITNO)) THEN
                  CLOSE(UNITNO)
                  IF(SIZE_FILE .AND. NEXT_REC(UNITNO) > RPT_DIMENSION(UNITNO)) FILE_SIZE =  &
                            FILE_SIZE_ADJ(OPEN_FILE_NAME(UNITNO),REC_LEN(UNITNO), &
                                                            NEXT_REC(UNITNO) - 1)
                  FILE_OPEN(UNITNO) = .FALSE.
               ENDIF
            ENDIF
            IF(FILE_OPT == 'C') THEN
               DO I = 1, 32000
                  IF(FILE_OPEN(I)) THEN
                     CLOSE(I)
                     IF(SIZE_FILE.AND. NEXT_REC(I) > RPT_DIMENSION(I)) FILE_SIZE =  &
                              FILE_SIZE_ADJ(OPEN_FILE_NAME(I),REC_LEN(I), &
                                                                NEXT_REC(I) - 1)
                     FILE_OPEN(I) = .FALSE.
                  ENDIF
               ENDDO
            ENDIF
            RPTREC = 0
            RETURN
         ENDIF
         IF(PRESENT(SAVE_REC).AND. PRESENT(UNITNO)) THEN
!            FILE_OPEN(UNITNO) = .TRUE.
            SAVED_REC(UNITNO) = SAVE_REC
            NEXT_REC(UNITNO) = SAVE_REC
            RPTREC = NEXT_REC(UNITNO)
            RETURN
         ENDIF
         RPTREC = NEXT_REC(UNITNO)
         NEXT_REC(UNITNO) = NEXT_REC(UNITNO) + 1
      END FUNCTION

      INTEGER FUNCTION FILE_SIZE_ADJ(FILE_NAME,REC_LENGHT,REC_PTR)
	     use params
         USE MSG_WIN_API_CALLS
         INTEGER (KIND=4) :: REC_PTR,RET_VAL
         INTEGER (KIND=2) :: REC_LENGHT
         INTEGER (KIND=4) :: FILE_SIZE !,FILE_BEGIN/0/
         INTEGER (KIND=8) :: FILE_SET_BYTES,Length64b
         CHARACTER (LEN=*) :: FILE_NAME
         CHARACTER (LEN=256) :: FilePathAndName
         LOGICAL (KIND=4) :: RET_LOGICAL
         integer(kind=4) :: i4,Ls32bLeng,Ms32bLeng,hFile,hTemplateFile
!
         FilePathAndName = TRIM(FILE_NAME)//char(0)
      File_handle=CreateFileA(carg(offset(FilePathAndName)), &
        carg(GENERIC_READ+GENERIC_WRITE), &
        carg(7),& ! allow sharing in case this process fails
        carg(NULL), & ! offset(SecurityAttr)),
        carg(OPEN_EXISTING),& ! allow sharing, use default security
        carg(FILE_ATTRIBUTE_NORMAL), & ! +FILE_FLAG_RANDOM_ACCESS),
        carg(offset(hTemplateFile)))  ! ignored when OPEN_EXISTING
!
    ! Fortran cannot pass by value an integer*8; break it into two parts:
      Length64b = INT(REC_LENGHT,8) * INT(REC_PTR,8)
      Ls32bLeng = mod (Length64b,TwoTo32nd)
      Ms32bLeng = int(Length64b/TwoTo32nd,4) ! 'not a valid Fortran statement' without int4
      FILE_SET_BYTES = SetFilePointer(carg(File_handle),carg(Ls32bLeng),  &
                        carg(offset(Ms32bLeng)), & ! Ms32b is by address
                        carg(FILE_BEGIN))

 ! need values for NULL & FILE_BEGIN
         RET_LOGICAL = SetEndOfFile(carg(File_Handle))
         FILE_SIZE_ADJ = Length64b
         RET_LOGICAL = CloseHandle(carg(File_Handle))
    END FUNCTION FILE_SIZE_ADJ
end module rptreccontrol
