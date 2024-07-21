!     ******************************************************************
!     EC_OBJT.FOR
!     Copyright(c) M.S. Gerber & Associates 2000
!
!     Created: 1/2/2003 9:09:04 AM
!     Author : MARK S GERBER
!     Last change: MSG 1/29/2007 3:53:38 PM
!     ******************************************************************

      SUBROUTINE EC_OBJECT
      use end_routine, only: end_program, er_message
C
       use SpinDriftLib
      use prod_arrays_dimensions
      USE PROD_ARRAYS_DIMENSIONS
      use sizecom
      INTEGER*2 INUNIT,IREC,DELETE,LRECL/300/,R_LRECL
      INTEGER IOS
      CHARACTER*5 ECON_INT_FIL,OVERLAY_FAMILY_NAME
      CHARACTER*256 FILE_NAME
      CHARACTER*50 COMMENT
      CHARACTER*256 BASE_FILE_DIRECTORY,OUTPUT_DIRECTORY
      LOGICAL*4 FILE_EXISTS
C DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER*1024 RECLN
C DECLARATION FOR ECONOMY INTERCHANGE
      INTEGER*2 IS,YEAR
      REAL PEAK_RR(12),AVE_RR(12),IMPORT_CAP,EXPORT_CAP,RR_MULT
      CHARACTER*19 FILE_TYPE/'Economy Interchange'/
      CHARACTER*2 ECON_OL/'BC'/,R_ECON_OL
C
      REAL*4   ENERGY_MIX,PRIMARY_ENERGY_COST,SECONDARY_ENERGY_COST,
     +         PRIMARY_EMISSIONS_RATE(5),
     +         SECONDARY_EMISSIONS_RATE(5),
     +         EMISSIONS_CREDIT_PRICE(5)
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
C CONVERT THE ECONOMY-INTERCHANGE FILE
      ENTRY EC_MAKEBIN
      FILE_NAME=trim(BASE_FILE_DIRECTORY())//
     +                             "ECB"//trim(ECON_INT_FIL())//".DAT"
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      IF(FILE_EXISTS) THEN
         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//ECON_INT_FIL()
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_LOCATE_WRITE(16,30,ECON_INT_FIL(),ALL_VERSIONS,0)
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         ENDIF
         RR_MULT = 1.
         IMPORT_CAP = 99999.
         EXPORT_CAP = 99999.
         DO IS = 1, 12
             PEAK_RR(IS) = 0.
             AVE_RR(IS) = 0.
         ENDDO
C         
         PRIMARY_ENERGY_COST = -999.
         SECONDARY_ENERGY_COST = -999.
         ENERGY_MIX = -999.
         DO IS = 1, 5
            PRIMARY_EMISSIONS_RATE(IS) = 0.
            SECONDARY_EMISSIONS_RATE(IS) = 0.
            EMISSIONS_CREDIT_PRICE(IS) = 0.
         ENDDO
C         
         OPEN(10,FILE=FILE_NAME)
         OPEN(11,FILE=trim(OUTPUT_DIRECTORY())//"BCECINT.BIN",
     +                      ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
         IREC = 1
         READ(10,*) DELETE
         DO
            READ(10,1000,IOSTAT=IOS) RECLN
            IF(IOS /=0) EXIT
            RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
            READ(RECLN,*,ERR=200) DELETE,YEAR,(PEAK_RR(IS),
     +         AVE_RR(IS),IS=1,12),COMMENT,IMPORT_CAP,EXPORT_CAP,
     +         RR_MULT,
     +         ENERGY_MIX,PRIMARY_ENERGY_COST,SECONDARY_ENERGY_COST,
     +         PRIMARY_EMISSIONS_RATE,SECONDARY_EMISSIONS_RATE,
     +         EMISSIONS_CREDIT_PRICE
            WRITE(11,REC=IREC) (PEAK_RR(IS),AVE_RR(IS),IS=1,12),
     +         IMPORT_CAP,EXPORT_CAP,RR_MULT,
     +         ENERGY_MIX,PRIMARY_ENERGY_COST,SECONDARY_ENERGY_COST,
     +         PRIMARY_EMISSIONS_RATE,SECONDARY_EMISSIONS_RATE,
     +         EMISSIONS_CREDIT_PRICE
            IF(IREC .GE. 30) EXIT
            IREC = IREC + 1
         ENDDO
         CLOSE(10)
c        ENDFILE(11)
         CLOSE(11)
      ELSE IF(INDEX(ECON_INT_FIL(),'NONE') == 0) THEN
         CALL STOP_NOFILE(FILE_TYPE,FILE_NAME)
C      ELSE
C         OPEN(11,FILE=trim(OUTPUT_DIRECTORY())//"BCECINT.BIN",
C    +                      ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
      ENDIF
      RETURN

C***********************************************************************
C
C          ROUTINE TO CREATE OVERLAY FILES
C          COPYRIGHT (C) 1984-88  M.S. GERBER & ASSOCIATES, INC.
C          COPYRIGHT (C) 1991-92  M.S. GERBER & ASSOCIATES, INC.
C
C***********************************************************************
C
C OVERLAY THE ECONOMY-INTERCHANGE FILE
      ENTRY EC_MAKEOVL(OVERLAY_FAMILY_NAME)
      INQUIRE(FILE=trim(OUTPUT_DIRECTORY())//"BCECINT.BIN",
     +                                                EXIST=FILE_EXISTS)
      IF(FILE_EXISTS) THEN
         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
            CALL LOCATE(10,51)
         ENDIF
         FILE_NAME=trim(OUTPUT_DIRECTORY())//"ECO"//
     +                               trim(OVERLAY_FAMILY_NAME)//".DAT"
         OPEN(10,FILE=FILE_NAME)
         READ(10,*) DELETE
         INUNIT = 12
         IF(ECON_OL == 'BC') THEN
            OPEN(11,FILE=trim(OUTPUT_DIRECTORY())//"BCECINT.BIN",
     +                                       ACCESS="DIRECT",RECL=LRECL)
            INUNIT = 11
         ENDIF
         OPEN(12,FILE=trim(OUTPUT_DIRECTORY())//"OLECINT.BIN",
     +                      ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
         IREC = 0
         DO
            IREC = IREC + 1
            READ(INUNIT,REC=IREC,IOSTAT=IOS) 
     +         (PEAK_RR(IS),AVE_RR(IS),
     +                           IS=1,12),IMPORT_CAP,EXPORT_CAP,RR_MULT,
     +         ENERGY_MIX,PRIMARY_ENERGY_COST,SECONDARY_ENERGY_COST,
     +         PRIMARY_EMISSIONS_RATE,SECONDARY_EMISSIONS_RATE,
     +         EMISSIONS_CREDIT_PRICE
            IF(IOS /= 0) EXIT
            READ(10,1000,IOSTAT=IOS) RECLN
            IF(IOS == 0) THEN
               RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,'//
     +                          ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               READ(RECLN,*,ERR=200) DELETE,YEAR,(PEAK_RR(IS),
     +            AVE_RR(IS),IS=1,12),COMMENT,IMPORT_CAP,EXPORT_CAP,
     +            RR_MULT,
     +            ENERGY_MIX,PRIMARY_ENERGY_COST,SECONDARY_ENERGY_COST,
     +            PRIMARY_EMISSIONS_RATE,SECONDARY_EMISSIONS_RATE,
     +            EMISSIONS_CREDIT_PRICE
            ENDIF
            WRITE(12,REC=IREC) (PEAK_RR(IS),AVE_RR(IS),IS=1,12),
     +            IMPORT_CAP,EXPORT_CAP,RR_MULT,
     +            ENERGY_MIX,PRIMARY_ENERGY_COST,SECONDARY_ENERGY_COST,
     +            PRIMARY_EMISSIONS_RATE,SECONDARY_EMISSIONS_RATE,
     +            EMISSIONS_CREDIT_PRICE
         ENDDO
         CLOSE(10)
         CLOSE(12)
         IF(ECON_OL == 'BC') CLOSE(11)
         ECON_OL = 'OL'
      ENDIF
      RETURN

  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from EC_OBJT SIID84'
      call end_program(er_message)
C
      ENTRY RESET_ECON_OL
         ECON_OL = 'BC'
      RETURN
C
      ENTRY GET_ECON_OL(R_ECON_OL,R_LRECL)
         R_ECON_OL = ECON_OL
         R_LRECL = LRECL
      RETURN
C
 1000 FORMAT(A)
 1010 FORMAT('&',A)
      END

