!     ******************************************************************
!     directtest.for
!     Copyright(c)  2000
!
!     Created: 1/26/2010 10:27:55 AM
!     Author : MARK S GERBER
!     Last change: MSG 3/17/2010 3:40:18 PM
!     ******************************************************************
      MODULE MSG_WIN_API_CALLS
         use windows_h
         dll_import CreateFileA
         dll_import SetFilePointer
         dll_import SetEndOfFile
         dll_import CloseHandle
         INTEGER (KIND=4) :: CreateFileA,File_handle
         INTEGER (KIND=8) :: SetFilePointer
         LOGICAL (KIND=4) :: SetEndOfFile, CloseHandle
      integer FILE_BEGIN             /z'00000000'/
      integer FILE_FLAG_RANDOM_ACCESS/z'10000000'/
      integer (KIND=8), parameter :: TwoTo32nd=z'100000000'
      END MODULE
      PROGRAM DIRECTTEST
      INTEGER (KIND=4) :: CPO,LAST_REC
      CHARACTER (LEN=50) :: RECLN
      INTEGER (KIND=1) :: CRLF_HEX(2)/z"0D",z"0A"/
      CHARACTER (LEN=2) :: CRLF
      EQUIVALENCE (CRLF,CRLF_HEX)
      INTEGER (KIND=4) :: IOS
      LOGICAL (KIND=4) :: FILE_SIZE_ADJ,ret_val
      character (LEN=256) :: FilePathAndName ! could be declared much larger
!
!
      RECLN = "X"     
      RECLN(45:50) = " "
      OPEN(9979,FILE='TEST.DAT',ACCESS='DIRECT', &
              FORM='FORMATTED',RECL=128, STATUS='REPLACE')
      WRITE(9979,"(A)",REC=1) "9,    ,2010"
      DO CPO = 2, 30
         WRITE(9979,"(A,I4,A)",REC=CPO) CRLF,CPO,RECLN//",,,,,,,,,,"
      ENDDO
      call FLUSH(9979)
      CLOSE(9979)
      PAUSE
      
      FilePathAndName='TEST.DAT'! example used for testing only
      ret_val = FILE_SIZE_ADJ(FilePathAndName,128_2,20)
!
!     truncate the file to a length of TargetLength bytes
!      FilePathAndName='TEST.DAT'//char(0) ! example used for testing only
!      ret_val=trunc_fs_bytes(FilePathAndName,REST_REC)
      call end_program("Stop requested from directtest DT1")
      END PROGRAM
      LOGICAL FUNCTION FILE_SIZE_ADJ(FILE_NAME,REC_LENGHT,REC_PTR)
        USE MSG_WIN_API_CALLS
!         dll_import  trunc_fs_bytes,trunc_fs_kiloB,
!         INTEGER (KIND=4) :: trunc_fs_bytes,trunc_fs_kiloB
         INTEGER (KIND=4) :: REC_PTR,RET_VAL
         INTEGER (KIND=2) :: REC_LENGHT
         INTEGER (KIND=4) :: FILE_SIZE !,FILE_BEGIN/0/
         INTEGER (KIND=8) :: FILE_SET_BYTES,Length64b
         CHARACTER (LEN=*) :: FILE_NAME
         CHARACTER (LEN=256) :: FilePathAndName
         LOGICAL (KIND=4) :: RET_LOGICAL
         integer*4 i4,Ls32bLeng,Ms32bLeng,hFile,hTemplateFile
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

 ! need vaules for NULL & FILE_BEGIN
         RET_LOGICAL = SetEndOfFile(carg(File_Handle))
         FILE_SIZE_ADJ = RET_LOGICAL
         RET_LOGICAL = CloseHandle(carg(File_Handle))
    END FUNCTION
