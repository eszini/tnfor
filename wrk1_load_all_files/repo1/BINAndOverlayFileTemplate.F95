!     ******************************************************************
!     COAL_MODULES.F95
!     Copyright(c)  2000
!
!     Created: 12/9/2010 10:54:40 AM
!     Author : MARK S GERBER
!     Last change: MSG 12/9/2010 12:28:46 PM
!     ******************************************************************

! declarations for interface variables and arrays
      module Coal_Input_File
	  use logging
      logical (kind=1) :: Feasible,Focussed
      integer (kind=2) :: iYr,nNode,nLink,nBasn,nCCSegs,iFocus
      integer (kind=2), allocatable :: NodeID  (    :),
     +  LinkID  (    :),
     +  LinkNdID(  :,:),
     +  LinkNdSN(  :,:),
     +  BasnID  (    :),
     +  BnOfNode(    :),
     +  HPntID  (    :),
     +  PnOfNode(    :)
      real (kind=4) ::  SulphurCst, ! /0.10/, ! $/LbS => $200/T coefficient in objective function
     +                  Sulphur_UB ! /9000.0/ ! kTon => 9MegaT annual limit on SO2 emissions
      real (kind=4), allocatable ::
     +  NodeQtyLB (  :),
     +  NodeQtyUB (  :),
     +  NodeQtyEsc(:,:),
     +  NodeQty   (  :),
     +  fUnscNode (  :),
     +  NodePri (    :),
c    +  NodeMgC (    :),
     +  MineFunc(:,:,:),
     +  LinkChg (    :),
     +  BasnQtyLB (  :),
     +  BasnQtyUB (  :),
     +  HeatConLB (  :),
     +  HeatConUB (  :),
     +  HeatConAv (:,:),
     +  HeatConID (:,:),
     +  SulphurCon(  :)
      character (len=27), allocatable ::
     +  BasnName(    :),
     +  NodeName(    :)
      end module
      MODULE COAL_INPUT_FILE_MANAGEMENT
         CHARACTER (LEN=256) :: COAL_NODE_FILE,
     +                          COAL_TRANSPORT_FILE,
     +                          COAL_SUPPLY_FORECAST_FILE,
     +                          COAL_DEMAND_FORECAST_FILE,
     +                          COAL_CONTRACTS_FILE
         INTEGER*2   UNIT_NUM/10/,INUNIT,IREC,DELETE
         INTEGER IOS
         CHARACTER (LEN=256) :: BASE_FILE_NAME,OVERLAY_FAMILY_NAME
     +                     SAVE_BASE_FILE_NAME,R_TEMP_NAME,FILE_NAME_OVL
     +                     FILE_NAME,OUTPUT_DIRECTORY,
     +                     BASE_FILE_DIRECTORY,SCREEN_OUTPUT
         LOGICAL*4 FILE_EXISTS
! DECLARATION FOR DBREAD COMMON BLOCK
         CHARACTER*1024 RECLN
      END MODULE COAL_INPUT_FILE_MANAGEMENT
C***********************************************************************
C
C        PROGRAM TO READ MULTI-TAB INFORMATION ON GAS NODE NODES
C                 AND CONVERT TO BINARY FORMAT
C                       COPYRIGHT (C) 2006
C      ALL RIGHTS RESERVED GLOBAL ENERGY DECISIONS
C
C***********************************************************************
!
!
      SUBROUTINE GN_OBJECT
      use end_program
!         
C***********************************************************************
C
! 120606. ADDED GAS NODE LAT/LONG AND STATE/PROVINCE VARAIBLES.
!
      INCLUDE 'SpinLib.MON'
      INCLUDE 'SIZECOM.MON'
!
! SIMULATION VARIABLES
!
      CHARACTER*50 COMMENT
!      
! FILE MANAGEMENT VARIABLES
!
      INTEGER (KIND=2) :: LRECL/846/ ! OUTPUT FILE RECORD LENGTH
      LOGICAL (KIND=1), SAVE ::  DATA_FILE_EXISTS/.FALSE./,R_FILE_EXISTS
      CHARACTER (LEN=17) :: FILE_TYPE/'GAS_SUP Group   '/
      CHARACTER (LEN=2), SAVE :: PROCESSING_FILE_OL/'BC'/
      CHARACTER (LEN=2) :: R_PROCESSING_FILE_OL
      INTEGER (KIND=2), SAVE :: TABLES_IN_FILE
      CHARACTER (LEN=20), PARAMETER :: BINARY_FILE_NAME ="GNGROUP.BIN"
!
!      
C
C***********************************************************************
C
C          ROUTINE TO CONVERT FROM ASCII TO DIRECT-ACESS BINARY
C          COPYRIGHT (C) 1983-98  M.S. GERBER & ASSOCIATES, INC.
C
C***********************************************************************
C
C CONVERT THE BASE FILE
C
C
C***********************************************************************
      ENTRY GAS_NODE_MAKEBIN  ! ENTERY NAME HERE
C***********************************************************************
      BASE_FILE_NAME = COAL_NODE_FILE()
      SCREEN_OUTPUT = TRIM(FILE_TYPE)//'-'//BASE_FILE_NAME
      CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      FILE_NAME = TRIM(BASE_FILE_DIRECTORY())//
     +                              "GNB"//TRIM(BASE_FILE_NAME )//".DAT"
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      IF(FILE_EXISTS) THEN
!
         SAVE_BASE_FILE_NAME = FILE_NAME
!         
         DATA_FILE_EXISTS = .TRUE.
!         call write_log_entry("BinAndOverlayFileTemplate", "Opening " // trim(file_name) // "...") 
         OPEN(10,FILE=FILE_NAME)
		 
!		 call write_log_entry("BinAndOverlayFileTemplate", "Opening " //trim(binary_file_name) // "...")
         OPEN(11,FILE=TRIM(OUTPUT_DIRECTORY())//"BC"//
     +                                           TRIM(BINARY_FILE_NAME),
     +                      ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
!         
!
         IREC = 0
         TABLES_IN_FILE = 0
         READ(10,*) DELETE
         DO ! TABLES
            DO ! TABLE RECORDS
               READ(10,1000,IOSTAT=IOS) RECLN
!               
               IF(IOS /= 0) EXIT ! END OF FILE
!
               IF(RECLN(1:1) == '7') THEN ! END OF TABLE ! EXIT AT BOTTOM OF IF
                  EXIT ! GO TO NEXT TABLE
               ENDIF
!               
               RECLN = TRIM(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               READ(RECLN,*,ERR=200)   DELETE,
     +                                             ! ITEM 1
!
               IREC = IREC + 1
               WRITE(11,REC=IREC)      DELETE,
     +                                                ! 1
!
            ENDDO ! TABLE RECORDS
            TABLES_IN_FILE = TABLES_IN_FILE + 1
            IF(IOS /= 0) EXIT
         ENDDO ! READ TABLES
         CLOSE(10)
      ELSE IF(INDEX(GAS_NODE_FILE(),'NONE') == 0) THEN
         CALL STOP_NOFILE(FILE_TYPE,FILE_NAME)
      ELSE
!		 
         OPEN(11,FILE=TRIM(OUTPUT_DIRECTORY())//"BC"//
     +                                           TRIM(BINARY_FILE_NAME),
     +                      ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
!
         DATA_FILE_EXISTS = .FALSE.
!         
      ENDIF
      SAVE_GAS_GROUPS_RECORDS = IREC
C      ENDFILE(11)
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
      ENTRY GAS_NODE_MAKEOVL(OVERLAY_FAMILY_NAME)
!      ENTRY GN_MAKEOVL(OVERLAY_FAMILY_NAME)
C***********************************************************************
      CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
      FILE_NAME_OVL=TRIM(OUTPUT_DIRECTORY())//"GNO"//
     +                               TRIM(OVERLAY_FAMILY_NAME)//".DAT"

	 
      OPEN(10,FILE=FILE_NAME_OVL)
      READ(10,*) DELETE
      INUNIT = 12
      IF(PROCESSING_FILE_OL == 'BC') THEN

         OPEN(11,FILE=TRIM(OUTPUT_DIRECTORY())//"BC"//
     +                                           TRIM(BINARY_FILE_NAME),
     +                                       ACCESS="DIRECT",RECL=LRECL)
         INUNIT = 11
      ENDIF
      FILE_NAME = TRIM(OUTPUT_DIRECTORY())//"OL"//TRIM(BINARY_FILE_NAME)

      OPEN(12,FILE=FILE_NAME,ACCESS="DIRECT",
     +                           STATUS="UNKNOWN",RECL=LRECL,IOSTAT=IOS)
      IF(IOS /= 0) THEN
         CALL IOSTAT_MSG(IOS,SCREEN_OUTPUT)
         WRITE(4,*) TRIM(SCREEN_OUTPUT)
         WRITE(4,*) '*** line 4176 TF_OBJT.FOR ***'
         call end_program('See WARNING MESSAGES BIN&Overlay')
      ENDIF
      IREC = 0
      DELETE = 1
!
      READ(10,1000,IOSTAT=IOS) RECLN
      DOWHILE(RECLN(1:1) == '7')  ! END OF OVERLAY TABLE 
!              
         READ(10,1000,IOSTAT=IOS) RECLN
!         
      ENDDO
!      
      DO
         IREC = IREC + 1
         READ(INUNIT,REC=IREC,IOSTAT=IOS)  DELETE,
     +                                             ! ITEM 1
         IF(IOS /= 0) EXIT
         RECLN = TRIM(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
         READ(RECLN,*,ERR=200)         DELETE,
     +                                             ! ITEM 1
!
         READ(10,1000,IOSTAT=IOS) RECLN
         DOWHILE(RECLN(1:1) == '7')  ! END OF OVERLAY TABLE 
!              
            READ(10,1000,IOSTAT=IOS) RECLN
!         
         ENDDO
!
         WRITE(12,REC=IREC)            DELETE,        
     +                                             ! ITEM 1
      ENDDO
      IF(IREC /= SAVE_GAS_GROUPS_RECORDS) THEN
         WRITE(4,*) "GAS_NODE GROUP OVERLAY DIFFERENT LENGTH"
         WRITE(4,*) "THAN THE BASE FILE. OVERLAY MUST BE THE SAME"
         WRITE(4,*) "LENGTH. ",FILE_NAME_OVL
      ENDIF
      CLOSE(10)
      CLOSE(12)
      IF(PROCESSING_FILE_OL== 'BC') CLOSE(11)
      PROCESSING_FILE_OL= 'OL'
      RETURN
C
C***********************************************************************
      ENTRY RESET_GAS_NODE_OL
C***********************************************************************
         PROCESSING_FILE_OL = 'BC'
      RETURN
C
C***********************************************************************
      ENTRY RETURN_GNGROUP_OL(R_GNGROUP_OL)
C***********************************************************************
         R_PROCESSING_FILE_OL = PROCESSING_FILE_OL
      RETURN
C***********************************************************************
      ENTRY DOES_GN_FILE_EXIST(R_FILE_EXISTS)
C***********************************************************************
         R_FILE_EXISTS = DATA_FILE_EXISTS
      RETURN
C***********************************************************************
      ENTRY GET_GAS_NODE_TABLES(R_GAS_GROUPS_TABLES)
C***********************************************************************
         R_GAS_GROUPS_TABLES = SAVE_GAS_GROUPS_TABLES
      RETURN
C***********************************************************************
      ENTRY GET_GAS_NODE_RECORDS(R_GAS_GROUPS_RECORDS)
C***********************************************************************
         R_GAS_GROUPS_RECORDS = SAVE_GAS_GROUPS_RECORDS
      RETURN
C***********************************************************************
      ENTRY GetGasNodeBaseName(R_TEMP_NAME)
C***********************************************************************
         R_TEMP_NAME = SAVE_BASE_FILE_NAME
      RETURN
C***********************************************************************
      ENTRY OPEN_GN_FILE

C***********************************************************************
         OPEN(UNIT_NUM,FILE=TRIM(OUTPUT_DIRECTORY())//
     +         PROCESSING_FILE_OL//TRIM(BINARY_FILE_NAME),
     +                      ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
      RETURN
C
C***********************************************************************
      ENTRY CLOSE_GN_FILE
C***********************************************************************
         CLOSE(UNIT_NUM)
      RETURN
C
c  200 CALL LOCATE(20,0)
c      WRITE(6,1010) TRIM(RECLN)
  200 CALL MG_LOCATE_WRITE(20,0,TRIM(RECLN),ALL_VERSIONS,1)
      call end_program('Bin&Overlay EOF ')
C
 1000 FORMAT(A)
 1010 FORMAT('&',A)
      END

