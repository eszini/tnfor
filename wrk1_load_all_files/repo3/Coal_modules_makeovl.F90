!     ******************************************************************
!     COAL_MODULES_MAKEOVL.F95
!     Copyright(c)  2000
!
!     Created: 12/13/2010 3:37:43 PM
!     Author : MARK S GERBER
!     Last change: MSG 10/24/2012 3:17:50 PM
!     ******************************************************************
C***********************************************************************
!
!
      SUBROUTINE COAL_CONTRACTS_MAKEOVL(OVERLAY_FILE_NAME)  ! ENTERY NAME HERE
!         
C***********************************************************************
C
!
      USE COAL_CONTRACTS_MAKEBIN_INPUTS
      USE COAL_cnw_FILES_READ_INPUT
!
! SIMULATION VARIABLES
!
         INTEGER (KIND=2) ::   UNIT_NUM/10/,INUNIT,IREC,DELETE
         INTEGER (KIND=4) :: IOS
         CHARACTER (LEN=*) :: OVERLAY_FILE_NAME
         CHARACTER (LEN=256) :: FILE_NAME,OUTPUT_DIRECTORY
         LOGICAL (KIND=4) :: DATA_FILE_EXISTS
         CHARACTER (LEN=1) :: Active="Y",Comment
! DECLARATION FOR DBREAD COMMON BLOCK
         CHARACTER*1024 RECLN
! **********************************************************************
!
      FILE_NAME = TRIM(OUTPUT_DIRECTORY())//
     +             DATA_FILE_CODE//"O"//TRIM(OVERLAY_FILE_NAME )//".DAT"
      INQUIRE(FILE=FILE_NAME,EXIST=DATA_FILE_EXISTS)
      ACTIVE_COAL_CONTRACTS_IN_OVERLAY = 0.
      IF(DATA_FILE_EXISTS .AND. COAL_CONTRACTS_DATA_FILE_EXISTS) THEN
! SETUP THE BINARY FILES
         INUNIT = 12
         IF(PROCESSING_COAL_CONTRACTS_FILE_OL == 'BC') THEN
            OPEN(11,FILE=TRIM(OUTPUT_DIRECTORY())//"BC"//
     +                            TRIM(COAL_CONTRACTS_BINARY_FILE_NAME),
     +                            ACCESS="DIRECT",STATUS="UNKNOWN",
     +                            RECL=COAL_CONTRACTS_LRECL)
!         
!
            INUNIT = 11
         ENDIF
         OPEN(12,FILE=TRIM(OUTPUT_DIRECTORY())//"OL"//
     +                            TRIM(COAL_CONTRACTS_BINARY_FILE_NAME),
     +                            ACCESS="DIRECT",STATUS="UNKNOWN",
     +                            RECL=COAL_CONTRACTS_LRECL)
! OPEN OVERLAY INPUT FILE
         OPEN(10,FILE=FILE_NAME)
         READ(10,*) DELETE
         DOWHILE(RECLN(1:1) == '7')  ! END OF OVERLAY TABLE
!              
            READ(10,'(A)',IOSTAT=IOS) RECLN
!         
         ENDDO
         IREC = 0
         DO ! RECORDS IN BINARY FILES
               IREC = IREC + 1
               WRITE(INUNIT,REC=IREC,IOSTAT=IOS) DELETE,
     +                                           Contract Name,
     +                                           Contract ID,
     +                                           Active,
     +                                           Mine Name,
     +                                           Mine ID,
     +                                           Mine Basin Name,
     +                                           Coal Type,
     +                                           Plant ID,
     +                                           Nth Contract,
     +                                           Annual Quantity,
     +                                           Annual Price,
     +                                           BasinID,
     +                                           ContractType
               IF(IOS /= 0) EXIT ! END OF BINARY FILE
!
               RECLN = TRIM(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               READ(RECLN,*,IOSTAT=IOS) DELETE,
     +                                  Contract Name,
     +                                  Contract ID,
     +                                  Active,
     +                                  Mine Name,
     +                                  Mine ID,
     +                                  Mine Basin Name,
     +                                  Coal Type,
     +                                  Plant ID,
     +                                  Nth Contract,
     +                                  Annual Quantity,
     +                                  Annual Price,
     +                                  Comment,
     +                                  BasinID,
     +                                  ContractType
!
               READ(10,'(A)',IOSTAT=IOS) RECLN
!               
               DOWHILE(RECLN(1:1) == '7')  ! END OF OVERLAY TABLE
                  READ(10,'(A)',IOSTAT=IOS) RECLN
               ENDDO
               IF(Active /= "N" .AND. DELETE < 8)
     +                ACTIVE_COAL_CONTRACTS_IN_OVERLAY =
     +                              ACTIVE_COAL_CONTRACTS_IN_OVERLAY + 1
               WRITE(12,REC=IREC) DELETE,
     +                            Contract Name,
     +                            Contract ID,
     +                            Active,
     +                            Mine Name,
     +                            Mine ID,
     +                            Mine Basin Name,
     +                            Coal Type,
     +                            Plant ID,
     +                            Nth Contract,
     +                            Annual Quantity,
     +                            Annual Price,
     +                            BasinID,
     +                            ContractType

         ENDDO ! READ RECORDS IN BINARY FILES
         CLOSE(10)
         CLOSE(12)
         IF(PROCESSING_COAL_CONTRACTS_FILE_OL == "BC") CLOSE(11)
         PROCESSING_COAL_CONTRACTS_FILE_OL = "OL"
      ENDIF
      RETURN
      END
C***********************************************************************
C
C          ROUTINE TO CREATE OVERLAY FILES
C          COPYRIGHT (C) 1984-88  M.S. GERBER & ASSOCIATES, INC.
C          COPYRIGHT (C) 1991-92  M.S. GERBER & ASSOCIATES, INC.
C
C***********************************************************************
C
C***********************************************************************
      SUBROUTINE COAL_SUPPLY_MAKEOVL(OVERLAY_FILE_NAME)
C***********************************************************************
C
!
      USE COAL_SUPPLY_INPUTS
      USE COAL_cnw_FILES_READ_INPUT
!
! SIMULATION VARIABLES
!
         INTEGER (KIND=2) ::   UNIT_NUM/10/,INUNIT,IREC,DELETE
         INTEGER (KIND=4) :: IOS
         CHARACTER (LEN=*) :: OVERLAY_FILE_NAME
         CHARACTER (LEN=256) :: FILE_NAME,OUTPUT_DIRECTORY
         LOGICAL (KIND=4) :: DATA_FILE_EXISTS
         CHARACTER (LEN=1) :: Active="Y"
! DECLARATION FOR DBREAD COMMON BLOCK
         CHARACTER*1024 RECLN
! **********************************************************************
!
      FILE_NAME=TRIM(OUTPUT_DIRECTORY())//DATA_FILE_CODE//"O"//
     +                                  TRIM(OVERLAY_FILE_NAME)//".DAT"
      INQUIRE(FILE=FILE_NAME,EXIST=DATA_FILE_EXISTS)
      IF(DATA_FILE_EXISTS .AND. COAL_SUPPLY_DATA_FILE_EXISTS) THEN
         ACTIVE_FUEL_SUPPLY_OVERLAY = 0
         MAX_SUPPLY_BASIN_ID_OVLFILE = 0
         INUNIT = 12
         IF(PROCESSING_COAL_SUPPLY_FILE_OL == 'BC') THEN
            OPEN(11,FILE=TRIM(OUTPUT_DIRECTORY())//"BC"//
     +                               TRIM(COAL_SUPPLY_BINARY_FILE_NAME),
     +          ACCESS="DIRECT",STATUS="UNKNOWN",RECL=COAL_SUPPLY_LRECL)
            INUNIT = 11
         ENDIF
         OPEN(12,FILE=TRIM(OUTPUT_DIRECTORY())//"OL"//
     +                               TRIM(COAL_SUPPLY_BINARY_FILE_NAME),
     +          ACCESS="DIRECT",STATUS="UNKNOWN",RECL=COAL_SUPPLY_LRECL)
!        
         OPEN(10,FILE=FILE_NAME)
         READ(10,*) DELETE
!
         READ(10,'(A)',IOSTAT=IOS) RECLN
         DOWHILE(RECLN(1:1) == '7')  ! END OF OVERLAY TABLE 
            READ(10,'(A)',IOSTAT=IOS) RECLN
         ENDDO
!         
         IREC = 0
         DO
            IREC = IREC + 1
            READ(INUNIT,REC=IREC,IOSTAT=IOS)DELETE,
     +                                      Supply Description,
     +                                      Active,
     +                                      Basin ID,
     +                                      Fuel Type ID,
     +                                      Heat Content,
     +                                      SO2 Rate,
     +                                      CO2 Rate,
     +                                      Quantity Consumed Base Year,
     +                                      Minimum Annual Production,
     +                                      Maximum Annual Production,
     +                                      Supply Curve Points,
     +                                      Production Escalation,
     +                                      Production Cost Escalation,
     +                                      Supply Production,
     +                                      Supply Cost,
     +                                      Last Supply Cost                  
            IF(IOS /= 0) EXIT
            RECLN = TRIM(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
            READ(RECLN,*,IOSTAT=IOS) DELETE,
     +                               Supply Description,
     +                               Active,
     +                               Basin ID,
     +                               Fuel Type ID,
     +                               Heat Content,
     +                               SO2 Rate,
     +                               CO2 Rate,
     +                               Quantity Consumed Base Year,
     +                               Minimum Annual Production,
     +                               Maximum Annual Production,
     +                               Supply Curve Points,
     +                               Production Escalation,
     +                               Production Cost Escalation,
     +                               Supply Production,
     +                               Supply Cost,
     +                               Last Supply Cost                  
            READ(10,'(A)',IOSTAT=IOS) RECLN
            DOWHILE(RECLN(1:1) == '7')  ! END OF OVERLAY TABLE 
               READ(10,'(A)',IOSTAT=IOS) RECLN
            ENDDO
            IF(Active /= "N" .AND. DELETE < 8) THEN
               ACTIVE_FUEL_SUPPLY_OVERLAY =
     +                                    ACTIVE_FUEL_SUPPLY_OVERLAY + 1
            ENDIF
            MAX_SUPPLY_BASIN_ID_OVLFILE = MAX(Basin ID,
     +                                      MAX_SUPPLY_BASIN_ID_OVLFILE)
            WRITE(12,REC=IREC) DELETE,
     +                         Supply Description,
     +                         Active,
     +                         Basin ID,
     +                         Fuel Type ID,
     +                         Heat Content,
     +                         SO2 Rate,
     +                         CO2 Rate,
     +                         Quantity Consumed Base Year,
     +                         Minimum Annual Production,
     +                         Maximum Annual Production,
     +                         Supply Curve Points,
     +                         Production Escalation,
     +                         Production Cost Escalation,
     +                         Supply Production,
     +                         Supply Cost,
     +                         Last Supply Cost                  
         ENDDO
         CLOSE(10)
         CLOSE(12)
         IF(PROCESSING_COAL_SUPPLY_FILE_OL== 'BC') CLOSE(11)
         PROCESSING_COAL_SUPPLY_FILE_OL= 'OL'
      ENDIF
      RETURN
      END
C***********************************************************************
      SUBROUTINE PLANT_DEMAND_MAKEOVL(OVERLAY_FILE_NAME)
C***********************************************************************
C
!
      USE PLANT_DEMAND_MAKEBIN_INPUTS
      USE COAL_cnw_FILES_READ_INPUT
!
! SIMULATION VARIABLES
!
         INTEGER (KIND=2) ::   UNIT_NUM/10/,INUNIT,IREC,DELETE
         INTEGER (KIND=4) :: IOS
         CHARACTER (LEN=*) :: OVERLAY_FILE_NAME
         CHARACTER (LEN=256) :: FILE_NAME,OUTPUT_DIRECTORY
         LOGICAL (KIND=4) :: DATA_FILE_EXISTS
         CHARACTER (LEN=1) :: Active="Y"
! DECLARATION FOR DBREAD COMMON BLOCK
         CHARACTER*1024 RECLN
! **********************************************************************
!
      FILE_NAME=TRIM(OUTPUT_DIRECTORY())//DATA_FILE_CODE//"O"//
     +                                  TRIM(OVERLAY_FILE_NAME)//".DAT"
      INQUIRE(FILE=FILE_NAME,EXIST=DATA_FILE_EXISTS)
      IF(DATA_FILE_EXISTS .AND. PLANT_DEMAND_DATA_FILE_EXISTS) THEN
         ACTIVE_PLANT_DEMAND_IN_OVERLAY = 0
         INUNIT = 12
         IF(PROCESSING_PLANT_DEMAND_FILE_OL == 'BC') THEN
            OPEN(11,FILE=TRIM(OUTPUT_DIRECTORY())//"BC"//
     +                              TRIM(PLANT_DEMAND_BINARY_FILE_NAME),
     +         ACCESS="DIRECT",STATUS="UNKNOWN",RECL=PLANT_DEMAND_LRECL)
            INUNIT = 11
         ENDIF
         OPEN(12,FILE=TRIM(OUTPUT_DIRECTORY())//"OL"//
     +                            TRIM(PLANT_DEMAND_BINARY_FILE_NAME),
     +         ACCESS="DIRECT",STATUS="UNKNOWN",RECL=PLANT_DEMAND_LRECL)
!        
         OPEN(10,FILE=FILE_NAME)
         READ(10,*) DELETE
!
         READ(10,'(A)',IOSTAT=IOS) RECLN
         DOWHILE(RECLN(1:1) == '7')  ! END OF OVERLAY TABLE 
            READ(10,'(A)',IOSTAT=IOS) RECLN
         ENDDO
!         
         IREC = 0
         DO
            IREC = IREC + 1
            READ(INUNIT,REC=IREC,IOSTAT=IOS)DELETE,
     +                                  Plant Name,
     +                                  Active,
     +                                  Unit Name,
     +                                  EV Plant ID,
     +                                  EV Unit ID,
     +                                  Heat Content Base Year,
     +                                  Percent Increase,
     +                                  Percent Decrease,
     +                                  Heat Lower Bound,
     +                                  Heat Upper Bound,
     +                                  Annual Demand,
     +                                  Fraction Unscrubbed,
     +                                  SO2 Base Year,
     +                                  SO2 Percent Increase,
     +                                  SO2 Percent Decrease,
     +                                  SO2 Lower Bound,
     +                                  SO2 Upper Bound,
     +                                  OnLineMo,OnLineYr,
     +                                  OffLineMo,OffLineYr,
     +                                  Trans_Group_ID,
     +                                  Trans_Group_Name,
     +                                  Unit_Capacity,
     +                                  SOx_Control_Percent,
     +                                  SOx_Control_Date,
     +                                  SOx_Control_Capital_Cost,
     +                                  SOx_Control_Carrying_Charge,
     +                                  SOx_Control_Var_Cost,
     +                                  SOx_Control_Fixed_Cost,
     +                                  Average_Heatrate,
     +                                  State_Location,
     +                                  Escalation_SOx_Capital_Cost,    
     +                                  Escalation_SOx_Carrying_Charge, 
     +                                  Escalation_SOx_Var_Cost,        
     +                                  Escalation_SOx_Fixed_Cost
            IF(IOS /= 0) EXIT
            RECLN = TRIM(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
            READ(RECLN,*,IOSTAT=IOS) DELETE,
     +                               Plant Name,
     +                               Active,
     +                               Unit Name,
     +                               EV Plant ID,
     +                               EV Unit ID,
     +                               Heat Content Base Year,
     +                               Percent Increase,
     +                               Percent Decrease,
     +                               Heat Lower Bound,
     +                               Heat Upper Bound,
     +                               Annual Demand,
     +                               Fraction Unscrubbed,
     +                               SO2 Base Year,
     +                               SO2 Percent Increase,
     +                               SO2 Percent Decrease,
     +                               SO2 Lower Bound,
     +                               SO2 Upper Bound,
     +                               OnLineMo,OnLineYr,
     +                               OffLineMo,OffLineYr,
     +                               Trans_Group_ID,
     +                               Trans_Group_Name,
     +                               Unit_Capacity,
     +                               SOx_Control_Percent,
     +                               SOx_Control_Date,
     +                               SOx_Control_Capital_Cost,
     +                               SOx_Control_Carrying_Charge,
     +                               SOx_Control_Var_Cost,
     +                               SOx_Control_Fixed_Cost,
     +                               Average_Heatrate,
     +                               State_Location,
     +                               Escalation_SOx_Capital_Cost,    
     +                               Escalation_SOx_Carrying_Charge, 
     +                               Escalation_SOx_Var_Cost,        
     +                               Escalation_SOx_Fixed_Cost
            READ(10,'(A)',IOSTAT=IOS) RECLN
            DOWHILE(RECLN(1:1) == '7')  ! END OF OVERLAY TABLE 
               READ(10,'(A)',IOSTAT=IOS) RECLN
            ENDDO
            IF(Active /= "N" .AND. DELETE < 8)
     +                        ACTIVE_PLANT_DEMAND_IN_OVERLAY =
     +                                ACTIVE_PLANT_DEMAND_IN_OVERLAY + 1
            WRITE(12,REC=IREC) DELETE,
     +                         Plant Name,
     +                         Active,
     +                         Unit Name,
     +                         EV Plant ID,
     +                         EV Unit ID,
     +                         Heat Content Base Year,
     +                         Percent Increase,
     +                         Percent Decrease,
     +                         Heat Lower Bound,
     +                         Heat Upper Bound,
     +                         Annual Demand,
     +                         Fraction Unscrubbed,
     +                         SO2 Base Year,
     +                         SO2 Percent Increase,
     +                         SO2 Percent Decrease,
     +                         SO2 Lower Bound,
     +                         SO2 Upper Bound,
     +                         OnLineMo,OnLineYr,
     +                         OffLineMo,OffLineYr,
     +                         Trans_Group_ID,
     +                         Trans_Group_Name,
     +                         Unit_Capacity,
     +                         SOx_Control_Percent,
     +                         SOx_Control_Date,
     +                         SOx_Control_Capital_Cost,
     +                         SOx_Control_Carrying_Charge,
     +                         SOx_Control_Var_Cost,
     +                         SOx_Control_Fixed_Cost,
     +                         Average_Heatrate,
     +                         State_Location,
     +                         Escalation_SOx_Capital_Cost,    
     +                         Escalation_SOx_Carrying_Charge, 
     +                         Escalation_SOx_Var_Cost,        
     +                         Escalation_SOx_Fixed_Cost
         ENDDO
         CLOSE(10)
         CLOSE(12)
         IF(PROCESSING_PLANT_DEMAND_FILE_OL == 'BC') CLOSE(11)
         PROCESSING_PLANT_DEMAND_FILE_OL = 'OL'
      ENDIF
      RETURN
      END
C***********************************************************************
      SUBROUTINE GENERIC_DEMAND_MAKEOVL(OVERLAY_FILE_NAME)
C***********************************************************************
C
!
      USE GENERIC_DEMAND_MAKEBIN_INPUTS
      USE COAL_cnw_FILES_READ_INPUT
!
! SIMULATION VARIABLES
!
         INTEGER (KIND=2) ::   UNIT_NUM/10/,INUNIT,IREC,DELETE
         INTEGER (KIND=4) :: IOS
         CHARACTER (LEN=*) :: OVERLAY_FILE_NAME
         CHARACTER (LEN=256) :: FILE_NAME,OUTPUT_DIRECTORY
         LOGICAL (KIND=4) :: DATA_FILE_EXISTS
         CHARACTER (LEN=1) :: Active="Y"
! DECLARATION FOR DBREAD COMMON BLOCK
         CHARACTER*1024 RECLN
! **********************************************************************
!
      FILE_NAME=TRIM(OUTPUT_DIRECTORY())//DATA_FILE_CODE//"O"//
     +                                  TRIM(OVERLAY_FILE_NAME)//".DAT"
      INQUIRE(FILE=FILE_NAME,EXIST=DATA_FILE_EXISTS)
      IF(DATA_FILE_EXISTS .AND. GENERIC_DEMAND_DATA_FILE_EXISTS) THEN
         ACTIVE_GENERIC_DEMAND_IN_OVERLAY = 0
         INUNIT = 12
         IF(PROCESSING_GENERIC_DEMAND_FILE_OL == 'BC') THEN
            OPEN(11,FILE=TRIM(OUTPUT_DIRECTORY())//"BC"//
     +                            TRIM(GENERIC_DEMAND_BINARY_FILE_NAME),
     +                        ACCESS="DIRECT",STATUS="UNKNOWN",
     +                        RECL=GENERIC_DEMAND_LRECL)
            INUNIT = 11
         ENDIF
         OPEN(12,FILE=TRIM(OUTPUT_DIRECTORY())//"OL"//
     +                            TRIM(GENERIC_DEMAND_BINARY_FILE_NAME),
     +       ACCESS="DIRECT",STATUS="UNKNOWN",RECL=GENERIC_DEMAND_LRECL)
!        
         OPEN(10,FILE=FILE_NAME)
         READ(10,*) DELETE
!
         READ(10,'(A)',IOSTAT=IOS) RECLN
         DOWHILE(RECLN(1:1) == '7')  ! END OF OVERLAY TABLE 
            READ(10,'(A)',IOSTAT=IOS) RECLN
         ENDDO
!         
         IREC = 0
         DO
            IREC = IREC + 1
            READ(INUNIT,REC=IREC,IOSTAT=IOS)DELETE,
     +                                      Unit Name,
     +                                      Active,
     +                                      Trans_Group_ID,
     +                                      Trans_Group_Name,
     +                                      State_Location,
     +                                      Unit_Capacity,
     +                                      Average_Heatrate,
     +                                      Heat Content,
     +                                      Percent Increase,
     +                                      Percent Decrease,
     +                                      Heat Lower Bound,
     +                                      Heat Upper Bound,
     +                                      Fraction Unscrubbed,
     +                                      SO2 Emissions,
     +                                      SO2 Percent Increase,
     +                                      SO2 Percent Decrease,
     +                                      SO2 Lower Bound,
     +                                      SO2 Upper Bound,
     +                                      Annual Demand
            IF(IOS /= 0) EXIT
            RECLN = TRIM(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
            READ(RECLN,*,IOSTAT=IOS) DELETE,
     +                               Unit Name,
     +                               Active,
     +                               Trans_Group_ID,
     +                               Trans_Group_Name,
     +                               State_Location,
     +                               Unit_Capacity,
     +                               Average_Heatrate,
     +                               Heat Content,
     +                               Percent Increase,
     +                               Percent Decrease,
     +                               Heat Lower Bound,
     +                               Heat Upper Bound,
     +                               Fraction Unscrubbed,
     +                               SO2 Emissions,
     +                               SO2 Percent Increase,
     +                               SO2 Percent Decrease,
     +                               SO2 Lower Bound,
     +                               SO2 Upper Bound,
     +                               Annual Demand
            READ(10,'(A)',IOSTAT=IOS) RECLN
            DOWHILE(RECLN(1:1) == '7')  ! END OF OVERLAY TABLE 
               READ(10,'(A)',IOSTAT=IOS) RECLN
            ENDDO
            IF(Active /= "N" .AND. DELETE < 8)
     +                        ACTIVE_GENERIC_DEMAND_IN_OVERLAY =
     +                              ACTIVE_GENERIC_DEMAND_IN_OVERLAY + 1
            WRITE(12,REC=IREC) DELETE,
     +                         Unit Name,
     +                         Active,
     +                         Trans_Group_ID,
     +                         Trans_Group_Name,
     +                         State_Location,
     +                         Unit_Capacity,
     +                         Average_Heatrate,
     +                         Heat Content,
     +                         Percent Increase,
     +                         Percent Decrease,
     +                         Heat Lower Bound,
     +                         Heat Upper Bound,
     +                         Fraction Unscrubbed,
     +                         SO2 Emissions,
     +                         SO2 Percent Increase,
     +                         SO2 Percent Decrease,
     +                         SO2 Lower Bound,
     +                         SO2 Upper Bound,
     +                         Annual Demand
         ENDDO
         CLOSE(10)
         CLOSE(12)
         IF(PROCESSING_GENERIC_DEMAND_FILE_OL == 'BC') CLOSE(11)
         PROCESSING_GENERIC_DEMAND_FILE_OL = 'OL'
      ENDIF
      RETURN
      END
C***********************************************************************
      SUBROUTINE COAL_TRANSPORTATION_LINKS_MAKEOVL(OVERLAY_FILE_NAME,
     +                                             FILE_CODE)
C***********************************************************************
C
!
      USE COAL_TRANSPORT_MAKE_INPUTS
      USE COAL_cnw_FILES_READ_INPUT
!
! SIMULATION VARIABLES
!
         INTEGER (KIND=2) ::   UNIT_NUM/10/,INUNIT,IREC,DELETE
         INTEGER (KIND=4) :: IOS
         CHARACTER (LEN=*) :: OVERLAY_FILE_NAME
         CHARACTER (LEN=256) :: FILE_NAME,OUTPUT_DIRECTORY
         LOGICAL (KIND=4) :: DATA_FILE_EXISTS
         CHARACTER (LEN=1) :: Active="Y"
! DECLARATION FOR DBREAD COMMON BLOCK
         CHARACTER (LEN=1024) :: RECLN
         CHARACTER (LEN=2) :: FILE_CODE
         LOGICAL (KIND=4) :: BASE_FILE_EXISTS
         INTEGER (KIND=4) :: FILE_NO
! **********************************************************************
!
      FILE_NAME=TRIM(OUTPUT_DIRECTORY())//FILE_CODE//"O"//
     +                                   TRIM(OVERLAY_FILE_NAME)//".DAT"
      INQUIRE(FILE=FILE_NAME,EXIST=DATA_FILE_EXISTS)
      IF(.NOT. DATA_FILE_EXISTS) RETURN
      FILE_NO = 1
      IF(FILE_CODE == 'ZD') FILE_NO = 2
      
      ACTIVE_COAL_LINK_IN_OVERLAY(FILE_NO) = 0
      INQUIRE(FILE=TRIM(OUTPUT_DIRECTORY())//"BC"// 
     +                   TRIM(COAL_LINK_BINARY_FILE_NAME(FILE_NO)),
     +                                           EXIST=BASE_FILE_EXISTS)
      IF(BASE_FILE_EXISTS) THEN
         INUNIT = 12
         IF(PROCESSING_COAL_LINK_FILE_OL(FILE_NO) == 'BC') THEN
            OPEN(11,FILE=TRIM(OUTPUT_DIRECTORY())//"BC"// 
     +                        TRIM(COAL_LINK_BINARY_FILE_NAME(FILE_NO)),
     +            ACCESS="DIRECT",STATUS="UNKNOWN",RECL=COAL_LINK_LRECL)
            INUNIT = 11
         ENDIF
         OPEN(12,FILE=TRIM(OUTPUT_DIRECTORY())//"OL"//
     +                        TRIM(COAL_LINK_BINARY_FILE_NAME(FILE_NO)),
     +            ACCESS="DIRECT",STATUS="UNKNOWN",RECL=COAL_LINK_LRECL)
!        
         OPEN(10,FILE=FILE_NAME)
         READ(10,*) DELETE
!
         READ(10,'(A)',IOSTAT=IOS) RECLN
         DOWHILE(RECLN(1:1) == '7')  ! END OF OVERLAY TABLE 
            READ(10,'(A)',IOSTAT=IOS) RECLN
         ENDDO
!         
         IREC = 0
         DO
            IREC = IREC + 1
            READ(INUNIT,REC=IREC,IOSTAT=IOS)DELETE,
     +                                    Basin to ,
     +                                      Plant,
     +                                    Active,
     +                                    Basin HI ID,
     +                                    Plant EV ID,
     +                                    Transport Price,
     +                                    Transport Price Escalation,
     +                                    Annual Quantity Limit,
     +                                    Transport Vehicles,
     +                                    Transport Quantity Escalaation
            IF(IOS /= 0) EXIT
            RECLN = TRIM(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
            READ(RECLN,*,IOSTAT=IOS) DELETE,
     +                               Basin to ,
     +                                 Plant,
     +                               Active,
     +                               Basin HI ID,
     +                               Plant EV ID,
     +                               Transport Price,
     +                               Transport Price Escalation,
     +                               Annual Quantity Limit,
     +                               Transport Vehicles,
     +                               Transport Quantity Escalaation
            READ(10,'(A)',IOSTAT=IOS) RECLN
            DOWHILE(RECLN(1:1) == '7')  ! END OF OVERLAY TABLE 
               READ(10,'(A)',IOSTAT=IOS) RECLN
            ENDDO
            IF(Active /= "N" .AND. DELETE < 8)
     +             ACTIVE_COAL_LINK_IN_OVERLAY(FILE_NO) =
     +                          ACTIVE_COAL_LINK_IN_OVERLAY(FILE_NO) + 1
            MAX_EV_PLANT_ID_OVLFILE(FILE_NO) = 
     +                 MAX(MAX_EV_PLANT_ID_OVLFILE(FILE_NO),Plant EV ID)
            MIN_EV_PLANT_ID_OVLFILE(FILE_NO) = 
     +                MIN(MIN_EV_PLANT_ID_OVL FILE(FILE_NO),Plant EV ID)
            WRITE(12,REC=IREC) DELETE,
     +                         Basin to ,
     +                           Plant,
     +                         Active,
     +                         Basin HI ID,
     +                         Plant EV ID,
     +                         Transport Price,
     +                         Transport Price Escalation,
     +                         Annual Quantity Limit,
     +                         Transport Vehicles,
     +                         Transport Quantity Escalaation
         ENDDO
         CLOSE(10)
         CLOSE(12)
         IF(PROCESSING_COAL_LINK_FILE_OL(FILE_NO) == 'BC') CLOSE(11)
         PROCESSING_COAL_LINK_FILE_OL(FILE_NO) = 'OL'
      ENDIF
      RETURN
      END
C***********************************************************************
      SUBROUTINE COAL_BASIN_MAKEOVL(OVERLAY_FILE_NAME)
C***********************************************************************
C
!
      USE COAL_BASIN_MAKEBIN_INPUTS
      USE COAL_cnw_FILES_READ_INPUT
!
! SIMULATION VARIABLES
!
         INTEGER (KIND=2) ::   UNIT_NUM/10/,INUNIT,IREC,DELETE
         INTEGER (KIND=4) :: IOS
         CHARACTER (LEN=*) :: OVERLAY_FILE_NAME
         CHARACTER (LEN=256) :: FILE_NAME,OUTPUT_DIRECTORY
         LOGICAL (KIND=4) :: DATA_FILE_EXISTS
         CHARACTER (LEN=1) :: Active="Y"
! DECLARATION FOR DBREAD COMMON BLOCK
         CHARACTER*1024 RECLN
! **********************************************************************
!
      FILE_NAME=TRIM(OUTPUT_DIRECTORY())//DATA_FILE_CODE//"O"//
     +                                  TRIM(OVERLAY_FILE_NAME)//".DAT"
      INQUIRE(FILE=FILE_NAME,EXIST=DATA_FILE_EXISTS)
      IF(DATA_FILE_EXISTS .AND. BASIN_DATA_FILE_EXISTS) THEN
         INUNIT = 12
         ACTIVE_BASINS_IN_OVERLAY = 0
         IF(PROCESSING_BASIN_FILE_OL == 'BC') THEN
            OPEN(11,FILE=TRIM(OUTPUT_DIRECTORY())//"BC"//
     +                                     TRIM(BASIN_BINARY_FILE_NAME),
     +                ACCESS="DIRECT",STATUS="UNKNOWN",RECL=BASIN_LRECL)
            INUNIT = 11
         ENDIF
         OPEN(12,FILE=TRIM(OUTPUT_DIRECTORY())//"OL"//
     +                                     TRIM(BASIN_BINARY_FILE_NAME),
     +                ACCESS="DIRECT",STATUS="UNKNOWN",RECL=BASIN_LRECL)
!        
         OPEN(10,FILE=FILE_NAME)
         READ(10,*) DELETE
!
         READ(10,'(A)',IOSTAT=IOS) RECLN
         DOWHILE(RECLN(1:1) == '7')  ! END OF OVERLAY TABLE 
            READ(10,'(A)',IOSTAT=IOS) RECLN
         ENDDO
!         
         IREC = 0
         DO
            IREC = IREC + 1
            READ(INUNIT,REC=IREC,IOSTAT=IOS)DELETE,
     +                                      Basin Abbrev,
     +                                      Active,
     +                                      Basin Name,
     +                                      Basin HI ID,
     +                                      Annual Base Yr Quantity,
     +                                      Minimum Annual Quantity,
     +                                      Maximum Annual Quantity
            IF(IOS /= 0) EXIT
            RECLN = TRIM(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
            READ(RECLN,*,IOSTAT=IOS) DELETE,
     +                               Basin Abbrev,
     +                               Active,
     +                               Basin Name,
     +                               Basin HI ID,
     +                               Annual Base Yr Quantity,
     +                               Minimum Annual Quantity,
     +                               Maximum Annual Quantity
            READ(10,'(A)',IOSTAT=IOS) RECLN
            DOWHILE(RECLN(1:1) == '7')  ! END OF OVERLAY TABLE 
               READ(10,'(A)',IOSTAT=IOS) RECLN
            ENDDO
            IF(Active /= "N" .AND. DELETE < 8)
     +                        ACTIVE_BASINS_IN_OVERLAY =
     +                                      ACTIVE_BASINS_IN_OVERLAY + 1
            MAX_BASIN_ID_OVLFILE = MAX(Basin HI ID,
     +                                 MAX_BASIN_ID_OVLFILE)
            WRITE(12,REC=IREC) DELETE,
     +                         Basin Abbrev,
     +                         Active,
     +                         Basin Name,
     +                         Basin HI ID,
     +                         Annual Base Yr Quantity,
     +                         Minimum Annual Quantity,
     +                         Maximum Annual Quantity
         ENDDO
         CLOSE(10)
         CLOSE(12)
         IF(PROCESSING_BASIN_FILE_OL == 'BC') CLOSE(11)
         PROCESSING_BASIN_FILE_OL = 'OL'
      ENDIF
      RETURN
      END
C***********************************************************************
      SUBROUTINE COAL_SO2_INFO_MAKEOVL(OVERLAY_FILE_NAME)
C***********************************************************************
C
!
      USE COAL_SO2_INFO_MAKEBIN_INPUTS
      USE COAL_cnw_FILES_READ_INPUT
!
! SIMULATION VARIABLES
!
         INTEGER (KIND=2) ::   UNIT_NUM/10/,INUNIT,IREC,DELETE
         INTEGER (KIND=4) :: IOS
         CHARACTER (LEN=*) :: OVERLAY_FILE_NAME
         CHARACTER (LEN=256) :: FILE_NAME,OUTPUT_DIRECTORY
         LOGICAL (KIND=4) :: DATA_FILE_EXISTS
         CHARACTER (LEN=1) :: Active="Y"
! DECLARATION FOR DBREAD COMMON BLOCK
         CHARACTER*1024 RECLN
! **********************************************************************
!
      FILE_NAME=TRIM(OUTPUT_DIRECTORY())//DATA_FILE_CODE//"O"//
     +                                  TRIM(OVERLAY_FILE_NAME)//".DAT"
      INQUIRE(FILE=FILE_NAME,EXIST=DATA_FILE_EXISTS)
      IF(DATA_FILE_EXISTS .AND. COAL_SO2_Scrubber_FILE_EXISTS) THEN
         ACTIVE_SO2_Scrubber_OVERLAY = 0
         INUNIT = 12
         IF(PROCESSING_COAL_SO2_Scrubber_FILE_OL == 'BC') THEN
            OPEN(11,FILE=TRIM(OUTPUT_DIRECTORY())//"BC"//
     +                         TRIM(COAL_SO2_Scrubber_BINARY_FILE),
     +              ACCESS="DIRECT",STATUS="UNKNOWN",
     +              RECL=COAL_SO2_Scrubber_LRECL)
            INUNIT = 11
         ENDIF
         OPEN(12,FILE=TRIM(OUTPUT_DIRECTORY())//"OL"//
     +                         TRIM(COAL_SO2_Scrubber_BINARY_FILE),
     +           ACCESS="DIRECT",STATUS="UNKNOWN",
     +           RECL=COAL_SO2_Scrubber_LRECL)
!        
         OPEN(10,FILE=FILE_NAME)
         READ(10,*) DELETE
!
         READ(10,'(A)',IOSTAT=IOS) RECLN
         DOWHILE(RECLN(1:1) == '7')  ! END OF OVERLAY TABLE 
            READ(10,'(A)',IOSTAT=IOS) RECLN
         ENDDO
!         
         IREC = 0
         DO
            IREC = IREC + 1
            READ(INUNIT,REC=IREC,IOSTAT=IOS)DELETE,
     +                                   Input Type,
     +                                   Active,
     +                                   Sulphur Market Area,
     +                                   Sulphur Credit Cost,
     +                                   Sulphur Credit Cost Escalation,
     +                                   Sulphur Emissions Limit,
     +                                   Scrubber Capital Cost,
     +                                   Capital Cost Escalation,
     +                                   Scrubber Fixed OM Cost,
     +                                   Fixed OM Escalation,
     +                                   Scrubber Variable OM Cost,
     +                                   Variable OM Escalation,
     +                                   SOxGroupAssignment,
     +                                   StateCapVolatility 
            IF(IOS /= 0) EXIT
            RECLN = TRIM(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
            READ(RECLN,*,IOSTAT=IOS) DELETE,
     +                               Input Type,
     +                               Active,
     +                               Sulphur Market Area,
     +                               Sulphur Credit Cost,
     +                               Sulphur Credit Cost Escalation,
     +                               Sulphur Emissions Limit,
     +                               Scrubber Capital Cost,
     +                               Capital Cost Escalation,
     +                               Scrubber Fixed OM Cost,
     +                               Fixed OM Escalation,
     +                               Scrubber Variable OM Cost,
     +                               Variable OM Escalation,
     +                               SOxGroupAssignment,
     +                               StateCapVolatility 
            READ(10,'(A)',IOSTAT=IOS) RECLN
            DOWHILE(RECLN(1:1) == '7')  ! END OF OVERLAY TABLE 
               READ(10,'(A)',IOSTAT=IOS) RECLN
            ENDDO
            IF(Active /= "N" .AND. DELETE < 8)
     +                        ACTIVE_SO2_Scrubber_OVERLAY =
     +                                  ACTIVE_SO2_Scrubber_OVERLAY + 1
            WRITE(12,REC=IREC) DELETE,
     +                         Input Type,
     +                         Active,
     +                         Sulphur Market Area,
     +                         Sulphur Credit Cost,
     +                         Sulphur Credit Cost Escalation,
     +                         Sulphur Emissions Limit,
     +                         Scrubber Capital Cost,
     +                         Capital Cost Escalation,
     +                         Scrubber Fixed OM Cost,
     +                         Fixed OM Escalation,
     +                         Scrubber Variable OM Cost,
     +                         Variable OM Escalation,
     +                         SOxGroupAssignment,
     +                         StateCapVolatility 
         ENDDO
         CLOSE(10)
         CLOSE(12)
         IF(PROCESSING_COAL_SO2_Scrubber_FILE_OL == 'BC') CLOSE(11)
         PROCESSING_COAL_SO2_Scrubber_FILE_OL = 'OL'
      ENDIF
      RETURN
      END
! **********************************************************************
      SUBROUTINE COAL_DEMAND_LINK()
         USE COAL_MODEL_INPUT_TO_cnw
         LOGICAL (KIND=1) :: RUN_COAL_MODEL_ONLY
         INTEGER (KIND=4) :: ActiveDemandUnits,iArray
         INTEGER (KIND=2) :: GET_I8_ID_TO_UNIT,GET_EMISSION_MARKET_LINK
         INTEGER (KIND=2) :: nUnitPos
!  LINK PLANTS IN THIS FILE TO MIDAS PLANTS BY EV UNIT ID NUMBER
         IF(.NOT. RUN_COAL_MODEL_ONLY()) THEN
!            ActiveDemandUnits = SIZE(CoalDemandLinkToCLPlants)
            DO iArray = 1, nCoalDemandExistingUnits
               IF(EV_Unit_ID(iArray) > 0) THEN
                  nUnitPos = GET_I8_ID_TO_UNIT(EV_Unit_ID(iArray))
                  CoalDemandLinkToCLPlants(iArray) = nUnitPos
                  EmissionTablePointer(iArray) = 
     +                                GET_EMISSION_MARKET_LINK(nUnitPos)
                  
               ELSE
                  CoalDemandLinkToCLPlants(iArray) = 0
                  EmissionTablePointer(iArray) = -1 
               ENDIF
            ENDDO
         ENDIF
      END SUBROUTINE
! **********************************************************************
      SUBROUTINE COAL_HARDWRIED_DEMAND_LINK(nUnits)
         USE ANNLCOMMON
         USE CLA_OBJT_ARRAYS
         USE COAL_MODEL_INPUT_TO_cnw
         USE ArrayAllocationInterface
         LOGICAL (KIND=1) :: RUN_COAL_MODEL_ONLY
         INTEGER (KIND=4) :: ActiveDemandUnits,iArray
         INTEGER (KIND=2) :: GET_I8_ID_TO_UNIT
         INTEGER (KIND=2) :: TgID,nUnits,nUnitPos,nTransPathPos,
     +                       pArray,nActivePaths,iPlant,i
         INTEGER (KIND=4) :: nUnit
         INTEGER (KIND=2) :: RETURN_CL_UNITS_B4_ADDITIONS,
     +                       RETURN_CL_UNITS_AFTER_HARD_ADDS
         CHARACTER (LEN=5) :: UnitName
!  LINK PLANTS IN THIS FILE TO MIDAS PLANTS BY EV UNIT ID NUMBER
            IF(NUNITS > RETURN_CL_UNITS_B4_ADDITIONS()) THEN   ! HardWired
               iArray = nCoalDemandExistingUnits
               pArray = nTransportLinksExistingUnits
               iPlant = nExistingPlant
               GenericLinkUsed = .FALSE. 
               DO nUnit = RETURN_CL_UNITS_B4_ADDITIONS()+1,
     +                                 RETURN_CL_UNITS_AFTER_HARD_ADDS() 
                  TgID = TRANSACTION_GROUP_ID(nUnit) 
                  nUnitPos = GenericPlantPtrByTG(TgId)
                  iArray = iArray + 1
                  CoalDemandLinkToCLPlants(iArray) = nUnit
                  Plant_Name(iArray) = Generic_Plant_Name(nUnitPos)
                  WRITE(UnitName,*) nUnit
                  Plant_Unit_Name(iArray) = TRIM(Plant_Name(iArray))//
     +                                               "-"//TRIM(UnitName)
                  EV_Plant_ID(iArray) = EXPANSION_UNIT_LOCATION(nUnit)
                  EV_Unit_ID(iArray) = nUnit
                  Plant_ID(iArray) = EV_Plant_ID(iArray)
                  iPlant = iPlant + 1
                  UnitToPlantPtr(iArray) = iPlant 
                  Plant_Heat_Content_Base_Year(iArray) = 
     +                              Generic_Plant_Heat_Content(nUnitPos)
                  Plant_Percent_Increase(iArray) = 
     +                          Generic_Plant_Percent_Increase(nUnitPos)
                  Plant_Percent_Decrease(iArray) = 
     +                          Generic_Plant_Percent_Decrease(nUnitPos)
                  Plant_Heat_Lower_Bound(iArray) = 
     +                          Generic_Plant_Heat_Lower_Bound(nUnitPos)
                  Plant_Heat_Upper_Bound(iArray) = 
     +                          Generic_Plant_Heat_Upper_Bound(nUnitPos)
                  PlantFractionUnscrubbedByYear(:,iArray) = 
     +                       Generic_Plant_Fraction_Unscrubbed(nUnitPos)
                  OptUnScrubbedFrac(iArray) = .FALSE. 
                  MaxSO2InCoalBlend(:,iArray) = 
     +                             Generic_MaxSO2InCoalBlend(:,nUnitPos)
                  MaxSO2ForAnyCoalInBlend(:,iArray) =
     +                       Generic_MaxSO2ForAnyCoalInBlend(:,nUnitPos)
                  TransGroupID(iArray) =  TgID
                  TransGroupName(iArray) = 
     +                            Generic_Plant_TransGroupName(nUnitPos)
                  UnitCapacity(iArray)= Generic_Plant_Capacity(nUnitPos)
                  StateLocation(iArray) =
     +                             Generic_Plant_StateLocation(nUnitPos)
                  Plant_Annual_Demand_By_Year(:,iArray) =  0.
C     +                          Generic_Plant_ForecastDemand(:,nUnitPos)
                  Plant_Annual_Demand(iArray) = 0.
C     +                          Generic_Plant_ForecastDemand(0,nUnitPos)
                  AverageHeatrate(iArray) = 
     +                           Generic_Plant_AverageHeatrate(nUnitPos)
c            UnitCapacity(iArray) = Capacity(nUnit)
c            UnitIMEnergy(iArray) = EnergyGen(nUnit)
! added the transport links to the transport list
                  nActivePaths = GenericTransportPtrToTG(TgID,1)
                  DO i = 1, nActivePaths  
                     nTransPathPos = GenericTransportPtrToTG(TgID,i+1)
                     IF(GenericLinkUsed(nTransPathPos)) CYCLE
                     GenericLinkUsed(nTransPathPos) = .TRUE. 
                     pArray = pArray + 1
                     Route_Name(pArray) =
     +                                 Generic_Route_Name(nTransPathPos)
                     Basin_HI_ID(pArray) = 
     +                                Generic_Basin_HI_ID(nTransPathPos)                 
                     Plant_EV_ID(pArray)=EXPANSION_UNIT_LOCATION(nUnit)                                  
                     Route_Price_By_Year(pArray,:) =
     +                     Generic_Route_Price_By_Year(nTransPathPos,:)
                     Route_Quantity_Limit_By_Year(pArray,:) =
     +                  Generic_Route_Quantity_By_Year(nTransPathPos,:)
                     Transport_Vehicles(pArray) =
     +                           GenericTransportVehicles(nTransPathPos)          
                  ENDDO
               ENDDO
               nCoalDemand = iArray
               nTransportLinks = pArray 
               nExistingPlant = iPlant 
            ENDIF
            nUnitsInFixedStack = nUnits
            nCoalDemandExistingUnits = nCoalDemand
            nTransportLinksExistingUnits = nTransportLinks
            nExistingPlantPlusAdds = nExistingPlant 
      END SUBROUTINE
! **********************************************************************
      SUBROUTINE COAL_DEMAND_TRANSFERRED_FROM_MIDAS(Yr,
     +                                              GRX_ITERATIONS,
     +                                              Thermal_Demand,
     +                                              Capacity,
     +                                              EnergyGen)
         USE COAL_MODEL_INPUT_TO_cnw
         USE ANNLCOMMON
         USE CONVERSTION_ROUTINES
         USE CL_UNITS_READ_DATA
         INTEGER (KIND=2) :: Yr,SO2_MARKETS_YR_BASED_VALUES
         INTEGER (KIND=4) :: ActiveDemandUnits,iArray,nUnit
         REAL (KIND=8) :: Thermal_Demand(*)
         REAL (KIND=4) :: Capacity(*),EnergyGen(*),CapFractor
         INTEGER (KIND=2) :: BASE_YEAR,CurYr
         INTEGER (KIND=4) :: GRX_ITERATIONS
         CHARACTER (LEN=256) :: RECLN
         CHARACTER (LEN=256) :: FILE_NAME,OUTPUT_DIRECTORY
         CHARACTER (LEN=5) :: GET_SCENAME
         LOGICAL (KIND=4), SAVE :: UNIT13773OPEN=.FALSE.
!
!            ActiveDemandUnits = SIZE(CoalDemandLinkToCLPlants)
         CurYr = BASE_YEAR() + Yr
c         nCoalDemand = nCoalDemandExistingUnits
c         nTransportLinks = nTransportLinksExistingUnits
         IF(.NOT. UNIT13773OPEN) THEN
            
            FILE_NAME = TRIM(OUTPUT_DIRECTORY())//"IMUnitData-"//
     +                                       TRIM(GET_SCENAME())//".CSV"
            OPEN(13773,FILE=FILE_NAME,STATUS='REPLACE')
            RECLN =  "Year,"//
     +               "Plant Name,"//
     +               "EV Unit ID,"//
     +               "Heat,"//
     +               "Energy,"//
     +               "Capacity,"//
     +               "Input Demand,"//
     +               "CapFactor,"//
     +               "AveHeatRate,"//
     +               "Iteration,"//
     +               "LPListPos,"//
     +               "IMUnitPos,"
             WRITE(13773,'(1X,A)') TRIM(RECLN)
             UNIT13773OPEN = .TRUE. 
         ENDIF   
         IF(GRX_ITERATIONS == 0) ThermalDemandPreviousIter = 0.
         DO iArray = 1, nCoalDemand ! nCoalDemandExistingUnits
            nUnit = CoalDemandLinkToCLPlants(iArray)
            IF(nUnit > 0) then
! for testing
!               Plant_Annual_Demand_By_Year(Yr,iArray) =
!     +                                             Thermal_Demand(nUnit)
               UnitCapacity(iArray) = Capacity(nUnit)
               UnitIMEnergy(iArray) = EnergyGen(nUnit)
               AverageHeatrate(iArray) = 0.
               IF(EnergyGen(nUnit) > 0.) THEN
                  AverageHeatrate(iArray) = 1000.*Thermal_Demand(nUnit)/
     +                                      EnergyGen(nUnit)
c                  IF(iArray >=  33) THEN
c                     Plant_Annual_Demand_By_Year(Yr,iArray) = 
c     +                                             Thermal_Demand(nUnit)
c                  ENDIF     
                  CapFractor = EnergyGen(nUnit)/(8760.*Capacity(nUnit))
                  RECLN = TRIM(CONVERT_2_STR(CurYr))//','// 
     +                 TRIM(CONVERT_2_STR(Plant_Name(iArray)))//','//               
     +                 TRIM(CONVERT_2_STR(EV_Unit_ID(iArray)))//','//               
     +                 TRIM(CONVERT_2_STR(Thermal_Demand(nUnit)))//','//               
     +                 TRIM(CONVERT_2_STR(EnergyGen(nUnit)))//','//               
     +                 TRIM(CONVERT_2_STR(Capacity(nUnit)))//','//
     +                 TRIM(CONVERT_2_STR(
     +                   Plant_Annual_Demand_By_Year(Yr,iArray)))//','//
     +                 TRIM(CONVERT_2_STR(CapFractor))//','//
     +               TRIM(CONVERT_2_STR(AverageHeatrate(iArray)))//','//
     +                 TRIM(CONVERT_2_STR(GRX_ITERATIONS))//','//               
     +                 TRIM(CONVERT_2_STR(iArray))//','//               
     +                 TRIM(CONVERT_2_STR(nUnit))//','
                              
                  write(13773,'(1X,A)') TRIM(RECLN)
c                  IF(CapFractor < 1. .AND. .FALSE.) THEN
                   IF(GRX_ITERATIONS == 0) THEN
                     Plant_Annual_Demand_By_Year(Yr,iArray) = 
     +                                             Thermal_Demand(nUnit)
                   ELSE
                     IF(ThermalDemandPreviousIter(nUnit) == 0. .OR.
     +                                  Thermal_Demand(nUnit)== 0.) THEN
                        Plant_Annual_Demand_By_Year(Yr,iArray) = 
     +                             MAX(ThermalDemandPreviousIter(nUnit),
     +                                            Thermal_Demand(nUnit))
                     ELSE
                        Plant_Annual_Demand_By_Year(Yr,iArray) = 
     +                                 (ThermalDemandPreviousIter(nUnit)                   
     +                                  + Thermal_Demand(nUnit))/2.
                     ENDIF   
                   ENDIF
                   ThermalDemandPreviousIter(nUnit) = 
     +                                             Thermal_Demand(nUnit)
c                  ENDIF
               ELSE
                  Plant_Annual_Demand_By_Year(Yr,iArray) = 0.
               ENDIF     
            ELSEIF(.NOT. ExportNonUtilityDemand(iArray)) THEN
               Plant_Annual_Demand_By_Year(Yr,iArray) = 0.
            ENDIF
         ENDDO
      END SUBROUTINE
! **********************************************************************
      SUBROUTINE SET_SCRUBBERS_FROM_COAL_LP(CurYr)
         USE CoalModelOutputsFromCNW
         USE COAL_MODEL_INPUT_TO_cnw
         INTEGER (KIND=2) :: iArray,nUnit,SO2ControlDate,CurYr
         LOGICAL (KIND=1) :: SET_SOX_CONTROL_COAL_LP
c
         SO2ControlDate = (CurYr - 2000) * 100 + 1 + 10000
         DO iArray = 1, nCoalDemandExistingUnits
           nUnit = CoalDemandLinkToCLPlants(iArray)
           IF(nUnit > 0) then
              IF(AddScrubber(iArray)) THEN
                 AddScrubber(iArray) = SET_SOX_CONTROL_COAL_LP(nUnit,
     +                                             SO2ControlDate,
     +                                             fUnscNode(1,iArray))
              ENDIF
            ENDIF
         ENDDO
      END SUBROUTINE
! **********************************************************************
      SUBROUTINE RESET_SCRUBBERS_FROM_COAL_LP()
         USE CoalModelOutputsFromCNW
         USE COAL_MODEL_INPUT_TO_cnw
         INTEGER (KIND=2) :: iArray,nUnit
         LOGICAL (KIND=1) :: RESET_SOX_CONTROL_COAL_LP
c
         DO iArray = 1, nCoalDemandExistingUnits
           nUnit = CoalDemandLinkToCLPlants(iArray)
           IF(nUnit > 0) then
              IF(AddScrubber(iArray)) THEN
                 AddScrubber(iArray) = RESET_SOX_CONTROL_COAL_LP(nUnit)
              ENDIF
            ENDIF
         ENDDO
      END SUBROUTINE
! **********************************************************************
      SUBROUTINE COAL_PRICES_TRANSFERRED_TO_MIDAS(iSeas,CurYr,EndPt,
     +                                            PBTUCT,
     +                                            FUEL_BTU_COST,
     +                                            DISP_BTU_COST,
     +                                            BLENDED_BTU_COST,
     +                                            SO2)
         USE ANNLCOMMON
         USE CoalModelOutputsFromCNW
         USE SO2_Scrubber_COAL_MODEL_DATA
         USE COAL_MODEL_INPUT_TO_cnw
         USE CLA_OBJT_ARRAYS
         USE IREC_ENDPOINT_CONTROL
         INTEGER (KIND=2) :: iArray,nUnit,SO2ControlDate,iSeas
         REAL (KIND=4) :: DelieveredHeat
         INTEGER (KIND=2) :: UNITS_BEFORE_ADDITIONS,TgID,
     +                       UnitsBeforeExpansion,
     +                       RETURN_CL_UNITS_B4_ADDITIONS
         REAL (KIND=4) :: TG_FuelTransportCosts(0:256,4),SO2Price,
     +                    GET_A_FUEL_PRICE
         LOGICAL (KIND=1) :: TG_FuelTransportAvail(0:256)
         LOGICAL (KIND=1) :: ReportFilesOpen=.FALSE.,
     +                       UseAverageGroupSO2PRice=.TRUE.  
         INTEGER (KIND=4), SAVE :: IRecUP,UnitNoUP=2703
         INTEGER (KIND=4) :: COAL_REPORT_UP
         CHARACTER (LEN=40) :: NodeNameEVCode
         INTEGER (KIND=2) :: CurYr,EndPt,Yr,EmissPrt,nSO2State
         CHARACTER (LEN=10) :: EV_UNIT_ID_STR
         CHARACTER (LEN=20) :: RETURN_UNITNM
         REAL (KIND=4) :: PBTUCT(*),
     +                    FUEL_BTU_COST(*),
     +                    DISP_BTU_COST(*),
     +                    BLENDED_BTU_COST(*),
     +                    SO2(*),TEMP_R,
     +                    AssignedStateGroup,FuelDeliveryCost
         INTEGER (KIND=4) :: SO2_EMISS_TYPE/1/
         LOGICAL (KIND=1) :: PUT_CoalLP_SO2_DISP_EMIS_ADDER,TempLogic
         REAL (KIND=4), SAVE :: Group1MaxSO2,Group2MaxSO2,
     +                          GroupMaxSO2Price,GroupAveSO2Price,
     +                          Group1SO2Emission,Group2SO2Emission,
     +                          Group1AveSO2Price,Group2AveSO2Price,
     +                          Group1SO2ShadowPrice,
     +                          Group2SO2ShadowPrice,
     +                          GroupSO2ShadowPrice,
     +                          StateSO2Price,SO2PricePushed
         REAL (KIND=4), PARAMETER :: NOT_AVAIL=-999999.
         INTEGER (KIND=2) :: iYr,BASE_YEAR
!  
         IF(.NOT.ReportFilesOpen) THEN
            ReportFilesOpen = .TRUE. 
            IRecUP = COAL_REPORT_UP(20,UnitNoUP,"UP",3)
         ENDIF
! push the national table which is 0        
         IF(iSeas == 1) THEN ! push naional price which is assumed to be table 1
            SO2Price=MAX(0.,-SO2ResultsEmissions(2,0)/1000.)
            EmissPrt = 0
            TempLogic = PUT_CoalLP_SO2_DISP_EMIS_ADDER(SO2_EMISS_TYPE,
     +                                                 EmissPrt,
     +                                                 SO2Price/2000.)
            Group1MaxSO2 = 0.
            Group1SO2Emission = 0.
            Group1AveSO2Price = 0.
            Group2MaxSO2 = 0.
            Group2SO2Emission = 0.
            Group1AveSO2Price = 0.
            DO nSO2State= 1, nSO2Limits ! number of active states in LP
               SO2Price =MAX(0.,-SO2ResultsEmissions(2,nSO2State)/1000.)
               IF(StateGroupAssignment(nSO2State) == 1) THEN
                  Group1MaxSO2 = MAX(Group1MaxSO2,SO2Price)               
                  Group1SO2Emission =  Group1SO2Emission
     +                                + SO2ResultsEmissions(0,nSO2State)
                  Group1AveSO2Price = Group1AveSO2Price
     +                                + SO2Price *                      
     +                                  SO2ResultsEmissions(0,nSO2State)
               ENDIF
               IF(StateGroupAssignment(nSO2State) == 2) THEN
                  Group2MaxSO2 = MAX(Group2MaxSO2,SO2Price) 
                  Group2SO2Emission =  Group2SO2Emission
     +                                + SO2ResultsEmissions(0,nSO2State)
                  Group2AveSO2Price = Group2AveSO2Price
     +                                + SO2Price *                      
     +                                  SO2ResultsEmissions(0,nSO2State)
               ENDIF
            ENDDO    
            Group1AveSO2Price = Group1AveSO2Price/Group1SO2Emission
            Group2AveSO2Price = Group2AveSO2Price/Group2SO2Emission
            Group1SO2ShadowPrice = MAX(0.,-SO2ResultsShadPrGrp(1)/1000.)
            Group2SO2ShadowPrice = MAX(0.,-SO2ResultsShadPrGrp(2)/1000.)
         ENDIF
         DO iArray = 1, nCoalDemandExistingUnits
            nUnit = CoalDemandLinkToCLPlants(iArray)
!            TEMP_R = SO2(nUnit)
            IF(iSeas == 1) THEN
               DelieveredHeat = UnitCoalCostByBTU(0,iArray)
               IF(DelieveredHeat > 0.) THEN
                  UnitCoalCostByBTU(1:6,iArray) =
     +                   UnitCoalCostByBTU(1:6,iArray)/DelieveredHeat
               ELSE
                  UnitCoalCostByBTU(:,iArray) = 0.
               ENDIF
            ENDIF
            IF(nUnit > 0) then
               EmissPrt = EmissionTablePointer(iArray)
               nSO2State = UnitLinkToState(iArray)
               GroupMaxSO2Price = NOT_AVAIL
               GroupAveSO2Price = NOT_AVAIL
               IF(nSO2State < 0) THEN ! use national price
                  StateSO2Price =
     +                  MAX(0.,-SO2ResultsEmissions(2,0)/1000.)
                  SO2Price = StateSO2Price
                  GroupSO2ShadowPrice = 0.
                  AssignedStateGroup = -1.  
               ELSE
                  StateSO2Price =
     +                   MAX(0.,-SO2ResultsEmissions(2,nSO2State)/1000.)
                  IF(StateAssignedToGroup(nSO2State) == 1) THEN
                     IF(UseAverageGroupSO2PRice) THEN
                        SO2Price = Group1AveSO2Price
                     ELSE
                        SO2Price = Group1MaxSO2
                     ENDIF
                     SO2Price = Group1SO2ShadowPrice
                     GroupMaxSO2Price = Group1MaxSO2
                     GroupAveSO2Price = Group1AveSO2Price
                     GroupSO2ShadowPrice = Group1SO2ShadowPrice
                  ELSEIF(StateAssignedToGroup(nSO2State) == 2) THEN
                     IF(UseAverageGroupSO2PRice) THEN
                        SO2Price = Group2AveSO2Price
                     ELSE
                        SO2Price = Group2MaxSO2
                     ENDIF
                     SO2Price = Group2SO2ShadowPrice
                     GroupMaxSO2Price = Group2MaxSO2
                     GroupAveSO2Price = Group2AveSO2Price
                     GroupSO2ShadowPrice = Group2SO2ShadowPrice
                  ELSE ! use solved for state price
                     SO2Price = StateSO2Price
                     GroupSO2ShadowPrice = 0.
                  ENDIF 
                  AssignedStateGroup =
     +                            FLOAT(StateAssignedToGroup(nSO2State))
               ENDIF
               SO2PricePushed = SO2Price/2000.
               TempLogic =PUT_CoalLP_SO2_DISP_EMIS_ADDER(SO2_EMISS_TYPE,
     +                                                   EmissPrt,
     +                                                   SO2PricePushed)
               SO2PricePushed  = 2000. * SO2PricePushed 
               IF(iSeas == 1) THEN
                  WRITE(EV_UNIT_ID_STR,*) EV_Unit_ID(iArray)
                  NodeNameEVCode = TRIM(Plant_Name(iArray))//' '//
     +                                                    EV_UNIT_ID_STR
                  IRecUP = RPTREC(INT2(UnitNoUP))
                  FuelDeliveryCost = P_FUEL_DELIVERY_2(nUnit) 
                  IF(FuelDeliveryCost < 0) THEN
                     iYr = CurYr - BASE_YEAR()
                     CALL GET_FUEL_PRICE_POINTR(FuelDeliveryCost,
     +                                          iSeas,iYr)                     
                     IF(FuelDeliveryCost < 0) 
     +                                      FuelDeliveryCost = NOT_AVAIL
                  ENDIF
                  write(UnitNoUP,REC=IRecUP) FLOAT(EndPt),FLOAT(CurYr),
     +                           NodeNameEVCode,
     +                           UnitCoalCostByBTU(2,iArray), ! marginal Coal
     +                           UnitCoalCostByBTU(5,iArray), ! marginal transportation
     +                           PBTUCT(nUnit),
     +                           FuelDeliveryCost,
     +                           UnitCoalCostByBTU(4,iArray), ! Coal & Transportation
     +                           FUEL_BTU_COST(nUnit),
     +                           DISP_BTU_COST(nUnit),
     +                           BLENDED_BTU_COST(nUnit),
     +                           SO2Price,   ! SO2 price pushed to IM
     +                           AssignedStateGroup,
     +                           GroupMaxSO2Price,    ! 10
     +                           GroupAveSO2Price,
     +                           FLOAT(iArray),
     +                           FLOAT(nUnit),
     +                           GroupSO2ShadowPrice, ! 14
     +                           StateSO2Price,        ! 15
     +                           SO2PricePushed,       ! 16
     +                           SO2BlendedRate(iArray)! 17
               ENDIF
               PBTUCT(nUnit) =  UnitCoalCostByBTU(2,iArray)   ! marginal FOB 
               FUEL_BTU_COST(nUnit) =  UnitCoalCostByBTU(4,iArray)
               DISP_BTU_COST(nUnit) =  UnitCoalCostByBTU(4,iArray)
               BLENDED_BTU_COST(nUnit) =  UnitCoalCostByBTU(4,iArray)
! 101112. test.
               SO2(nUnit) = SO2BlendedRate(iArray) *
     +                                 Plant_Fraction_Unscrubbed(iArray)
!               SO2(nUnit) = SO2BlendedRate(iArray)
               EMISSION_RATES(1,1,nUnit) = SO2(nUnit)
               EMISSION_RATES(1,3,nUnit) = SO2(nUnit)
               P_FUEL_DELIVERY_2(nUnit) = UnitCoalCostByBTU(5,iArray) ! marginal transportation
            ENDIF
!            SO2(nUnit) = TEMP_R
         ENDDO
!
! pass back expansion unit prices for units added before this year/iteration. 
!
         TG_FuelTransportAvail = .FALSE.
         TG_FuelTransportCosts = 0. 
         UnitsBeforeExpansion = UNITS_BEFORE_ADDITIONS()
         UnitsBeforeExpansion = RETURN_CL_UNITS_B4_ADDITIONS()
!
!assume for expansion units that there is an existing coal unit in that
! state. Therefore, the SO2 tables have been updated. 10-3-11 MSG
! 
         DO iArray = nCoalDemandExistingUnits+1, 
     +                              nCoalDemand-nCoalDemandExistingUnits
            nUnit = GenericUnitPos(iArray)
            IF(nUnit < 1) CYCLE
            TgID = TRANSACTION_GROUP_ID(nUnit)
! temp
!            TEMP_R = SO2(nUnit)
            IF(nUnit > 0 .AND. nUnit <= UnitsBeforeExpansion) then
               IF(iSeas == 1) THEN
                  NodeNameEVCode = Plant_Unit_Name(iArray)
                  IRecUP = RPTREC(INT2(UnitNoUP))
                  write(UnitNoUP,REC=IRecUP) FLOAT(EndPt),FLOAT(CurYr),
     +                                 NodeNameEVCode,
     +                                 UnitCoalCostByBTU(2,iArray),
     +                                 UnitCoalCostByBTU(5,iArray), ! marginal transportation
     +                                 PBTUCT(nUnit),
     +                                 P_FUEL_DELIVERY_2(nUnit),
     +                                 UnitCoalCostByBTU(4,iArray), ! Coal & Transportation
     +                                 FUEL_BTU_COST(nUnit),
     +                                 DISP_BTU_COST(nUnit),
     +                                 BLENDED_BTU_COST(nUnit)
               ENDIF
               PBTUCT(nUnit) =  UnitCoalCostByBTU(2,iArray)   ! marginal FOB 
               FUEL_BTU_COST(nUnit) =  UnitCoalCostByBTU(4,iArray)
               DISP_BTU_COST(nUnit) =  UnitCoalCostByBTU(4,iArray)
               BLENDED_BTU_COST(nUnit) =  UnitCoalCostByBTU(4,iArray)
!               SO2(nUnit) = SO2BlendedRate(iArray)
               SO2(nUnit) = SO2BlendedRate(iArray) *
     +                                 Plant_Fraction_Unscrubbed(iArray)
               EMISSION_RATES(1,1,nUnit) = SO2(nUnit)
               EMISSION_RATES(1,3,nUnit) = SO2(nUnit)
               
               P_FUEL_DELIVERY_2(nUnit) = UnitCoalCostByBTU(5,iArray) ! marginal transportation
               TG_FuelTransportCosts(TgID,1)=UnitCoalCostByBTU(2,iArray)
               TG_FuelTransportCosts(TgID,2)=UnitCoalCostByBTU(5,iArray)
               TG_FuelTransportCosts(TgID,3)=UnitCoalCostByBTU(4,iArray)
               TG_FuelTransportCosts(TgID,4)=SO2BlendedRate(iArray)
               TG_FuelTransportAvail(TgID) = .TRUE. 
            ELSEIF(TG_FuelTransportAvail(TgID)) THEN
               IF(iSeas == 1) THEN
                  NodeNameEVCode = RETURN_UNITNM(nUnit)
                  IRecUP = RPTREC(INT2(UnitNoUP))
                  write(UnitNoUP,REC=IRecUP) FLOAT(EndPt),FLOAT(CurYr),
     +                                 NodeNameEVCode,
     +                                 TG_FuelTransportCosts(TgID,1) ,
     +                                 TG_FuelTransportCosts(TgID,2), ! marginal transportation
     +                                 PBTUCT(nUnit),
     +                                 P_FUEL_DELIVERY_2(nUnit),
     +                                 FUEL_BTU_COST(nUnit),
     +                                 DISP_BTU_COST(nUnit),
     +                                 BLENDED_BTU_COST(nUnit)
               ENDIF
               PBTUCT(nUnit) =  TG_FuelTransportCosts(TgID,1)
               FUEL_BTU_COST(nUnit) =  TG_FuelTransportCosts(TgID,3) 
               DISP_BTU_COST(nUnit) =  TG_FuelTransportCosts(TgID,3) 
               BLENDED_BTU_COST(nUnit) =  TG_FuelTransportCosts(TgID,3) 
!               SO2(nUnit) = TG_FuelTransportCosts(TgID,4)
               SO2(nUnit) = TG_FuelTransportCosts(TgID,4) *
     +                                 Plant_Fraction_Unscrubbed(iArray)
               EMISSION_RATES(1,1,nUnit) = SO2(nUnit)
               EMISSION_RATES(1,3,nUnit) = SO2(nUnit)
               P_FUEL_DELIVERY_2(nUnit) = TG_FuelTransportCosts(TgID,2)
            ENDIF
! temp
!            SO2(nUnit) = TEMP_R
         ENDDO
      END SUBROUTINE
! **********************************************************************
      SUBROUTINE COAL_DEMAND_EXPANSION_UNITS(Yr,nUnits,
     +                                       Thermal_Coal_Demand,
     +                                       Capacity,
     +                                       EnergyGen)
         USE ANNLCOMMON
         USE CLA_OBJT_ARRAYS
         USE CoalModelOutputsFromCNW
         USE COAL_MODEL_INPUT_TO_cnw
         USE ArrayAllocationInterface
         INTEGER (KIND=2) :: TgID,nUnits,nUnitPos,nTransPathPos,Yr,
     +                       iArray,pArray,i,nActivePaths,iPlant
         INTEGER (KIND=4) :: nUnit
         REAL (KIND=8) :: Thermal_Coal_Demand(*)
         REAL (KIND=4) :: Capacity(*),EnergyGen(*)
         INTEGER (KIND=2) :: UNITS_BEFORE_ADDITIONS
         CHARACTER (LEN=5) :: UnitName
c
         IF(nUnits == nUnitsInFixedStack) RETURN
         IF(.NOT. ALLOCATED(GenericLinkUsed)) RETURN
         GenericLinkUsedTemp = GenericLinkUsed 
         iArray = nCoalDemandExistingUnits
         pArray = nTransportLinksExistingUnits
         iPlant = nExistingPlant
         CALL AllocateArray(GenericUnitPos,nUnits-nUnitsInFixedStack)
         GenericUnitPos = -1
         DO nUnit = nUnitsInFixedStack+1, nUnits
            IF(.NOT. ALLOCATED(GenericPlantPtrByTG)) EXIT
            IF(EXPANSION_UNIT_LOCATION(nUnit) < 1) CYCLE
            IF(Thermal_Coal_Demand(nUnit) == 0.) CYCLE
! added the coal expansion units to the list of coal demand.
            TgID = TRANSACTION_GROUP_ID(nUnit) 
            nUnitPos = GenericPlantPtrByTG(TgId)
            iArray = iArray + 1
            CoalDemandLinkToCLPlants(iArray) = nUnit
            Plant_Name(iArray) = Generic_Plant_Name(nUnitPos)
            WRITE(UnitName,*) nUnit
            Plant_Unit_Name(iArray) = TRIM(Plant_Name(iArray))//"-"//
     +                                                    TRIM(UnitName)
            EV_Plant_ID(iArray) = EXPANSION_UNIT_LOCATION(nUnit)
            GenericUnitPos(iArray-nCoalDemandExistingUnits) = nUnit
            EV_Unit_ID(iArray) = nUnit
            Plant_ID(iArray) = EV_Plant_ID(iArray)
            iPlant = iPlant + 1
            UnitToPlantPtr(iArray) = iPlant 
            Plant_Heat_Content_Base_Year(iArray) = 
     +                              Generic_Plant_Heat_Content(nUnitPos)
            Plant_Percent_Increase(iArray) = 
     +                          Generic_Plant_Percent_Increase(nUnitPos)
            Plant_Percent_Decrease(iArray) = 
     +                          Generic_Plant_Percent_Decrease(nUnitPos)
            Plant_Heat_Lower_Bound(iArray) = 
     +                          Generic_Plant_Heat_Lower_Bound(nUnitPos)
            Plant_Heat_Upper_Bound(iArray) = 
     +                          Generic_Plant_Heat_Upper_Bound(nUnitPos)
            PlantFractionUnscrubbedByYear(Yr,iArray) = 
     +                       Generic_Plant_Fraction_Unscrubbed(nUnitPos)
            OptUnScrubbedFrac(iArray) = .FALSE. 
            MaxSO2InCoalBlend(Yr,iArray) = 
     +                            Generic_MaxSO2InCoalBlend(Yr,nUnitPos)
            MaxSO2ForAnyCoalInBlend(Yr,iArray) =
     +                      Generic_MaxSO2ForAnyCoalInBlend(Yr,nUnitPos)
            TransGroupID(iArray) =  TgID
            TransGroupName(iArray) = 
     +                            Generic_Plant_TransGroupName(nUnitPos)
            UnitCapacity(iArray) =  Generic_Plant_Capacity(nUnitPos)
            StateLocation(iArray) =Generic_Plant_StateLocation(nUnitPos)
c            Plant_Annual_Demand_By_Year(Yr,iArray) =
c     +                                        Thermal_Coal_Demand(nUnit)
c            Plant_Annual_Demand(iArray) = Thermal_Coal_Demand(nUnit) 
c            IF(EnergyGen(nUnit) > 0.) THEN
c               AverageHeatrate(iArray)=1000.*Thermal_Coal_Demand(nUnit)/
c     +                                             EnergyGen(nUnit)
c            ELSE
c               AverageHeatrate(iArray) = 
c     +                           Generic_Plant_AverageHeatrate(nUnitPos)
c            ENDIF   
c            UnitCapacity(iArray) = Capacity(nUnit)
c            UnitIMEnergy(iArray) = EnergyGen(nUnit)
! added the transport links to the transport list
            nActivePaths = GenericTransportPtrToTG(TgID,1)
            DO i = 1, nActivePaths  
               nTransPathPos = GenericTransportPtrToTG(TgID,i+1)
               IF(GenericLinkUsedTemp(nTransPathPos)) CYCLE
               GenericLinkUsedTemp(nTransPathPos) = .TRUE. 
               pArray = pArray + 1
               Route_Name(pArray) =  Generic_Route_Name(nTransPathPos)
               Basin_HI_ID(pArray) = Generic_Basin_HI_ID(nTransPathPos)                 
               Plant_EV_ID(pArray) = EXPANSION_UNIT_LOCATION(nUnit)                                  
               Route_Price_By_Year(pArray,Yr) =
     +                     Generic_Route_Price_By_Year(nTransPathPos,Yr)
               Route_Quantity_Limit_By_Year(pArray,Yr) =
     +                  Generic_Route_Quantity_By_Year(nTransPathPos,Yr)
               Transport_Vehicles(pArray) =
     +                           GenericTransportVehicles(nTransPathPos)          
            ENDDO
         ENDDO
         nCoalDemand = iArray
         nTransportLinks = pArray
         nExistingPlantPlusAdds = iPlant
      END SUBROUTINE
C***********************************************************************
      INTEGER FUNCTION COAL_REPORT_UP(NumVars,UnitNum,FileCode,Dims)
C***********************************************************************
         USE IREC_ENDPOINT_CONTROL
         INTEGER (KIND=4) :: UnitNum,NumVars,Dims
         INTEGER (KIND=4) :: COAL_REPORT_HEADER,iRec
         INTEGER (KIND=2) :: RECORD_LENGTH
         CHARACTER (LEN=*) :: FileCode
         CHARACTER (LEN=256) :: FILE_NAME,OUTPUT_DIRECTORY
         CHARACTER (LEN=5) :: GET_SCENAME
         iRec = COAL_REPORT_HEADER(NumVars,UnitNum,FileCode,Dims)
         WRITE(UnitNum,REC=iRec)
     +                       'Plant Unit Name     ','C',INT2(40),'F','D'
!         WRITE(UnitNum,REC=iRec+1)
!     +                       'Coal Basin, Heat, SO','C',INT2(50),'F','D'
!         WRITE(UnitNum,REC=iRec+1)
!     +                       'Coal Basin          ','C',INT2(20),'F','D'
         COAL_REPORT_UP = iRec + 1 
         FILE_NAME = TRIM(OUTPUT_DIRECTORY())//"MSG"//
     +                     TRIM(GET_SCENAME())//"."//TRIM(FileCode)//"D"
         RECORD_LENGTH = MAX(256,128+4*NumVars)
         iRec = RPTREC(INT2(UnitNum),SAVE_REC=COAL_REPORT_UP,
     +                     REC_LENGHT=RECORD_LENGTH,FILE_NAME=FILE_NAME)
      END FUNCTION
! **********************************************************************
      LOGICAL FUNCTION PLANT_ACTIVE_IN_IM(EV_Unit_ID)
         INTEGER (KIND=2) :: GET_I8_ID_TO_UNIT
         INTEGER (KIND=8) :: EV_Unit_ID 
         LOGICAL (KIND=1) :: RUN_COAL_MODEL_ONLY
         PLANT_ACTIVE_IN_IM = GET_I8_ID_TO_UNIT(EV_Unit_ID) > 0 .OR.
     +                        RUN_COAL_MODEL_ONLY()
      END FUNCTION
! **********************************************************************
      subroutine AppendFloat9(u,rOrg) ! for use in RPTtoCSV per TS 20060227
      real*4 MaxMagR4,MinMagR4
      real*8 MaxMagR8,MinMagR8
      parameter(MaxMagR4=1.0e36,MinMagR4=1.0e-36,
     +          MaxMagR8=1.0d36,MinMagR8=1.0d-36) ! 0.38e38 was seen in test loop
!     end of parameters
      integer*4 u
      integer*2 iExpo
      integer*4 iMant
      real*4 rOrg
      real*8 r,t
!
      t=dble(rOrg)
      if(t<0.0d0) t=-10.0d0*t ! allow one more place for the minus-sign
      if(t<MinMagR8) then ! as if t were zero
c       if(rOrg<0.0d0) then
c         write(u, 6 ,advance='no') rOrg ! but no leading minus was evident in testing
c       else
          write(u, 7 ,advance='no') rOrg
c       end if
      elseif(t<0.0999995d0) then
        r=dble(rOrg) ! preclude modifying rOrg
        do iExpo=1,98 ! 4 is minimum needed to express nint(t) as iiiiE-n
          t=10.0d0*t
          r=10.0d0*r
          iMant=idnint(t)
          if((iMant>=10000).or.((iExpo>8).and.(iMant>=1000))) exit
        end do
        iMant=idnint(r)
        if(iExpo<=9) then
          write(u,12 ,advance='no') iMant,-iExpo
        else
          write(u,13 ,advance='no') iMant,-iExpo
        end if
      elseif(t<9.9999995d0) then
        write(u, 7 ,advance='no') rOrg
      elseif(t<99.999995d0) then
        write(u, 6 ,advance='no') rOrg
      elseif(t<999.99995d0) then
        write(u, 5 ,advance='no') rOrg
      elseif(t<9999.9995d0) then
        write(u, 4 ,advance='no') rOrg
      elseif(t<99999.995d0) then
        write(u, 3 ,advance='no') rOrg
      elseif(t<999999.95d0) then
        write(u, 2 ,advance='no') rOrg
      elseif(t<9999999.5d0) then
        write(u, 1 ,advance='no') rOrg
      elseif(t<=9999999.4d0) then
        write(u,10 ,advance='no') nint(rOrg)
      else
!       sacrifice significant figures in output to gain fixed field-width
        r=dble(rOrg) ! preclude modifying rOrg
      ! preclude problems with rOrg near overflow in magnitude
        if(rOrg>MaxMagR4) then
          r=MaxMagR8
          t=r
        else if(rOrg<-MaxMagR4) then
          r=-MaxMagR8
          t=-10.0d0*r
        end if
        do iExpo=1,98 ! 2 is minimum needed to express nint(t) as iiiiiEn
          t=0.1d0*t
          r=0.1d0*r
        ! preclude NDP error taking idnint of large #s
          if(t<2.147484d9) then ! idnint() can handle up to 2**31
            iMant=idnint(t)
            if(((iMant<=999999).and.(iExpo<=9)).or.(iMant<=99999)) exit
          end if
        end do
        iMant=idnint(r)
        call flush(u)
        if(iExpo<=9) then
          write(u,11 ,advance='no') iMant,iExpo
        else
          write(u,12 ,advance='no') iMant,iExpo
        end if
      end if
      return
    ! ampersand usage under F77L3 requires CarriageControl=fortran
    1 format( f9.1,',')
    2 format( f9.2,',')
    3 format( f9.3,',')
    4 format( f9.4,',')
    5 format( f9.5,',')
    6 format( f9.6,',')
    7 format( f9.7,',')
   10 format( i9,  ',')
   11 format( i6,'.E',i1,',') ! note wasteful decimal point as requested
   12 format( i5,'.E',i2,',')
   13 format( i4,'.E',i3,',')
      end ! subroutine AppendFloat9

