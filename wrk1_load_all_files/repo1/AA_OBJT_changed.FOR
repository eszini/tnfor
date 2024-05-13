!     **************************************************************
!     AA_OBJT.FOR
!     Copyright(c) Global Energy Decisions 2000
!
!     Created: 11/7/2006 9:55:52 AM
!     Author : Tom Sweet
!     Last change: MSG 10/28/2012 11:28:42 AM
!     **************************************************************

      SUBROUTINE AA_OBJECT
      use end_routine, only: end_program, er_message
      use logging
	  use filename_tracker

      
c
      INCLUDE 'SpinLib.MON'
      INCLUDE 'SIZECOM.MON'
      INTEGER*2 DELETE,INUNIT,IREC,LRECL/128/,I
      INTEGER IOS
      CHARACTER*1 RESOURCE_ALLOCATION_METHOD
      INTEGER*2 UNIT_NUM/10/
      INTEGER*4 R_UNIT_NUM
      CHARACTER*5 BASE_FILE_NAME,OVERLAY_FAMILY_NAME,
     +            AREA_ALLOCATORS_FILE
      CHARACTER*256 FILE_NAME, BASE_FILE_DIRECTORY
      CHARACTER*256 DATA_DRIVE,OUTPUT_DIRECTORY

C DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER*1024 RECLN
C DECLARATION FOR AREA ALLOCATORS
      REAL AREA_VALUES(18)
      REAL :: CAPACITY_LOSS_FACTORS(6)
      INTEGER*2 RESOURCE_ID,ALLOCATION_PERIOD
      CHARACTER*16 FILE_TYPE/'Area Allocations'/
      CHARACTER*20 RESOURCE_NAME,COMMENT*50
      CHARACTER*2 RESOURCE_TYPE,AREA_OL/'BC'/
      LOGICAL*1 LAHEY_LF95
      CHARACTER*30 SCREEN_OUTPUT
      logical :: exists
      character*255 :: old_filename

C******************************************************************
C
C   ROUTINE TO CONVERT PRODUCTION PARAMETER FILE DATA
C
C                             COPYRIGHT (C) 1992
C                        M.S. GERBER & ASSOCIATES, INC.
C                             ALL RIGHTS RESERVED
C
C******************************************************************
C
C CONVERT THE CONTROL-AREA ALLOCATORS FILE
C
      ENTRY AA_MAKEBIN
C
C******************************************************************


c****** old
      BASE_FILE_NAME = AREA_ALLOCATORS_FILE()
      DATA_DRIVE = OUTPUT_DIRECTORY()
      old_FILENAME = trim(BASE_FILE_DIRECTORY())//
     +            "AAB"//trim(BASE_FILE_NAME)//'.DAT'
c****** end of old

      file_name=get_aab_filename()
	  if(trim(file_name)/=trim(old_filename)) THEN
		er_message="Filename not being generated " // 
     +    "correctly in AA_OBJT:2"
		call end_program(er_message)
		
	  endif
	  
	  

	  

      INQUIRE(FILE=FILE_NAME,EXIST=exists)
      IF(exists) THEN
         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//BASE_FILE_NAME
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,
     +          ALL_VERSIONS,0)
          CALL MG_LOCATE_WRITE(16,30,BASE_FILE_NAME,ALL_VERSIONS,0)
         ENDIF
         
         OPEN(10,FILE=FILE_NAME)
         data_drive=OUTPUT_DIRECTORY()
         OPEN(11,FILE=trim(DATA_DRIVE)//"BCAREA.BIN",
     +  ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
         IREC = 1
         READ(10,*) DELETE
         DO
            CAPACITY_LOSS_FACTORS = -99999.
            READ(10,1000,IOSTAT=IOS) RECLN
            IF(IOS /= 0) EXIT
            RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
            READ(RECLN,*,ERR=200) DELETE,
     +                         RESOURCE_NAME,RESOURCE_ID,
     +                         RESOURCE_TYPE,
     +                      ALLOCATION_PERIOD,AREA_VALUES,COMMENT,
     +                         CAPACITY_LOSS_FACTORS,
     +                         RESOURCE_ALLOCATION_METHOD
            DO I = 1, ubound(capacity_loss_factors,1)
               IF(CAPACITY_LOSS_FACTORS(I) == -99999.) then
				CAPACITY_LOSS_FACTORS(I) = 
     +            AREA_VALUES(3*(I-1)+3)
			   endif
                 

            ENDDO
            WRITE(11,REC=IREC) DELETE,
     +                      RESOURCE_NAME,RESOURCE_ID,
     +                      RESOURCE_TYPE,
     +                      ALLOCATION_PERIOD,AREA_VALUES,
     +                      CAPACITY_LOSS_FACTORS,
     +                      RESOURCE_ALLOCATION_METHOD
            IREC = IREC + 1
         ENDDO
         CLOSE(10)
         CLOSE(11)
      ELSE IF(INDEX(BASE_FILE_NAME,'NONE') == 0) THEN
         CALL STOP_NOFILE(FILE_TYPE,FILE_NAME)
      ENDIF
      RETURN



C OVERLAY THE CONTROL-AREA ALLOCATORS FILE
C*****************************************************************
      ENTRY AA_MAKEOVL(OVERLAY_FAMILY_NAME)
C*****************************************************************
      IF(LAHEY_LF95()) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSE
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         CALL LOCATE(10,51)
      ENDIF
	  file_name=get_aao_filename(OVERLAY_FAMILY_NAME)
	  
     

      OPEN(10,FILE=FILE_NAME)
      READ(10,*) DELETE
      INUNIT = 12
      IF(AREA_OL == 'BC') THEN
!        call write_log_entry("aa_objt", "opening " // 
!     +     "BCAREA.BIN" // "...")
         OPEN(11,FILE=trim(DATA_DRIVE)//"BCAREA.BIN",
     +               ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
         INUNIT = 11
      ENDIF
!       call write_log_entry("aa_objt", "opening " // 
!     + "OLAREA.BIN" // "...")
      OPEN(12,FILE=trim(DATA_DRIVE)//"OLAREA.BIN",ACCESS="DIRECT",
     +                               STATUS="UNKNOWN",RECL=LRECL)
      IREC = 0
      DO
         IREC = IREC + 1
         READ(INUNIT,REC=IREC,IOSTAT=IOS) DELETE,
     +                             RESOURCE_NAME,RESOURCE_ID,
     +                             RESOURCE_TYPE,
     +                             ALLOCATION_PERIOD,AREA_VALUES,
     +                             CAPACITY_LOSS_FACTORS,
     +                             RESOURCE_ALLOCATION_METHOD
         IF(IOS /= 0) EXIT
         READ(10,1000,IOSTAT=IOS) RECLN
         IF(IOS == 0) THEN
            RECLN = trim(RECLN)//',,,,,,,,,,,,,'
            READ(RECLN,*,ERR=300) DELETE,
     +                            RESOURCE_NAME,RESOURCE_ID,
     +                            RESOURCE_TYPE,
     +                   ALLOCATION_PERIOD,AREA_VALUES,COMMENT,
     +                            CAPACITY_LOSS_FACTORS,
     +                            RESOURCE_ALLOCATION_METHOD
         ENDIF
         WRITE(12,REC=IREC) DELETE,
     +                      RESOURCE_NAME,RESOURCE_ID,
     +                      RESOURCE_TYPE,
     +                      ALLOCATION_PERIOD,AREA_VALUES,
     +                      CAPACITY_LOSS_FACTORS,
     +                      RESOURCE_ALLOCATION_METHOD
      ENDDO
      CLOSE(10)
      CLOSE(12)
      IF(AREA_OL == 'BC') CLOSE(11)
      AREA_OL = 'OL'
      RETURN

c  200 CALL LOCATE(20,0)
c      WRITE(6,1010) trim(RECLN)
  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from AA_OBJT SIID0'
      call end_program(er_message)
c  300 CALL LOCATE(20,0)
c      WRITE(6,1010) trim(RECLN)
c      WRITE(6,1010) 'Error reading the above record.  Look for',
c     +              ' a "," in a character name.'
  300 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      CALL MG_LOCATE_WRITE(21,0,
     +         'Error reading the Control Area record. Look for'//
     +                               ' a "," in a character name.',
     +                     ALL_VERSIONS,1)
      er_message='stop requested from AA_OBJT SIID1'
      call end_program(er_message)
C
      ENTRY RESET_AREA_OL
         AREA_OL = 'BC'
      RETURN
C
      ENTRY OPEN_AREA_ALLOCATOR_FILE(R_UNIT_NUM)
         UNIT_NUM = R_UNIT_NUM
!         log_message="opening " // trim(AREA_OL) // "..."
!         call write_log_entry("aa_objt", log_message)
         OPEN(UNIT_NUM,FILE=trim(OUTPUT_DIRECTORY())//AREA_OL//
     +       "AREA.BIN",ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
      RETURN
C
      ENTRY CLOSE_AREA_ALLOCATOR_FILE
         CLOSE(UNIT_NUM)
      RETURN
C
 1000 FORMAT(A)
 1010 FORMAT('&',A)
      END
C*******************************************************************
      SUBROUTINE READ_AREA_ALLOCATORS
C*******************************************************************
C
      INCLUDE 'SpinLib.MON'
      INCLUDE 'SIZECOM.MON'
      INTEGER*2 DELETE,RESOURCE_ID,ALLOCATION_PERIOD,IREC,I,MO,AREA,
     +          ARRAY_POINTR,TEMP_POINTR
      CHARACTER*20 RESOURCE_NAME
      CHARACTER*2  RESOURCE_TYPE
      CHARACTER*1  RESOURCE_ALLOCATION_METHOD
      REAL CAP_ALLOCATORS(6),
     +     ENRG_ALLOCATORS(6),
     +     RESOURCE_LOSES(6),
     +     CAPACITY_LOSS_FACTORS(6)
      INCLUDE 'KEPCOCOM.MON'
C
      CL_ALLOCATE_POINTR = 0
      CT_ALLOCATE_POINTR = 0
      EL_ALLOCATE_POINTR = 0
      CALL OPEN_AREA_ALLOCATOR_FILE(10)
      IREC = 1
      ARRAY_POINTR = 0
      DO
         READ(10,REC=IREC,ERR=100) DELETE,
     +                          RESOURCE_NAME,RESOURCE_ID,
     +                          RESOURCE_TYPE,
     +                          ALLOCATION_PERIOD,
     +                         (CAP_ALLOCATORS(I),
     +                          ENRG_ALLOCATORS(I),
     +                          RESOURCE_LOSES(I), I=1, 6),
     +                          CAPACITY_LOSS_FACTORS,
     +                          RESOURCE_ALLOCATION_METHOD
         IREC = IREC + 1
         IF(DELETE > 8) CYCLE
         IF(INDEX(RESOURCE_TYPE,'CL') .NE. 0) THEN
            IF(CL_ALLOCATE_POINTR(RESOURCE_ID) == 0) THEN
               ARRAY_POINTR = ARRAY_POINTR + 1
               TEMP_POINTR = ARRAY_POINTR
               CL_ALLOCATE_POINTR(RESOURCE_ID) = ARRAY_POINTR
            ELSE
               TEMP_POINTR = CL_ALLOCATE_POINTR(RESOURCE_ID)
            ENDIF
         ELSEIF(INDEX(RESOURCE_TYPE,'CT') .NE. 0) THEN
            IF(CT_ALLOCATE_POINTR(RESOURCE_ID) == 0) THEN
               ARRAY_POINTR = ARRAY_POINTR + 1
               TEMP_POINTR = ARRAY_POINTR
               CT_ALLOCATE_POINTR(RESOURCE_ID) = ARRAY_POINTR
            ELSE
               TEMP_POINTR = CT_ALLOCATE_POINTR(RESOURCE_ID)
            ENDIF
         ELSEIF(INDEX(RESOURCE_TYPE,'EL') .NE. 0) THEN
            IF(EL_ALLOCATE_POINTR(RESOURCE_ID) == 0) THEN
               ARRAY_POINTR = ARRAY_POINTR + 1
               TEMP_POINTR = ARRAY_POINTR
               EL_ALLOCATE_POINTR(RESOURCE_ID) = ARRAY_POINTR
            ELSE
               TEMP_POINTR = EL_ALLOCATE_POINTR(RESOURCE_ID)
            ENDIF
         ELSE
            WRITE(4,*) 'Missing resource type for ',
     +                  trim(RESOURCE_NAME),
     +                 ' in Control Area Allocation file.'
            CYCLE
         ENDIF
         AREA_ALLOCATION_METHOD(ARRAY_POINTR)=
     +    RESOURCE_ALLOCATION_METHOD
         IF(ALLOCATION_PERIOD == 13) THEN
            DO MO = 1, 12
               DO AREA = 1, NUMBER_OF_AREAS
                  AREA_CAP_ALLOCATORS(AREA,MO,TEMP_POINTR) =
     +                                 CAP_ALLOCATORS(AREA)
                  AREA_ENRG_ALLOCATORS(AREA,MO,TEMP_POINTR) =
     +                                 ENRG_ALLOCATORS(AREA)
                  AREA_RESOURCE_LOSES(AREA,MO,TEMP_POINTR) =
     +                                  RESOURCE_LOSES(AREA)
                  AREA_CAPACITY_LOSSES(AREA,MO,TEMP_POINTR) =
     +                           CAPACITY_LOSS_FACTORS(AREA)
               ENDDO
            ENDDO
         ELSE
            DO AREA = 1, NUMBER_OF_AREAS
               AREA_CAP_ALLOCATORS(AREA,ALLOCATION_PERIOD,
     +   TEMP_POINTR)=CAP_ALLOCATORS(AREA)
               AREA_ENRG_ALLOCATORS(AREA,
     +          ALLOCATION_PERIOD,TEMP_POINTR)=ENRG_ALLOCATORS(AREA)
           AREA_RESOURCE_LOSES(AREA,ALLOCATION_PERIOD,TEMP_POINTR)=
     +                                         RESOURCE_LOSES(AREA)
               AREA_CAPACITY_LOSSES(AREA,MO,TEMP_POINTR) =
     +                                  CAPACITY_LOSS_FACTORS(AREA)
            ENDDO
         ENDIF
      ENDDO
  100 CALL CLOSE_AREA_ALLOCATOR_FILE
      RETURN
      END


