!     Last change: MSG 1/29/2007 2:01:29 PM
!***********************************************************************
!
!       SUBROUTINE TO INITIALIZE THE SCREEN CONTROL VARIABLES
!
!***********************************************************************
!
      SUBROUTINE DSSCREEN(SCENAME)
!
      use SpinDriftLib
      USE PROD_ARRAYS_DIMENSIONS
      use cl_data
      USE SIZECOM
      use globecom

      implicit none

      CHARACTER (LEN=1) :: UTILITY_TYPE,OBJ_FUNCTION_DESC*50
      INTEGER (KIND=2) :: PROCMETH,COVERAGE_RATIO
      LOGICAL (KIND=1) :: SHORT_FORM,SYSTEM_BASED_FORECAST
      LOGICAL (KIND=1) :: POOLING_TRANSACTIONS,CONTROL_AREA_FORECAST
      LOGICAL (KIND=1) :: CAP_PLANNING_METHOD_ANNDECOMP,FACET_IS_ACTIVE
      LOGICAL (KIND=1) :: ASSET_ANALYST_ONLY,TRANSACT_ANALYST_ONLY
      LOGICAL (KIND=1) :: INIT_FILE_IS_ACTIVE
      CHARACTER (LEN=5) :: CHR_POINTS
      CHARACTER (LEN=1) :: OPMETH
      INTEGER (KIND=4) :: SCREEN1(1024)
      INTEGER (KIND=4) :: SCREEN2(1024)
      INTEGER (KIND=2) :: NORMAL,INVERSE
      INTEGER (KIND=2) :: NORMAL_VIDEO,INVERSE_VIDEO
      INTEGER (KIND=2) :: END_POINTS,I,GET_NUM_END_POINTS_IN_OVL_FILE
      CHARACTER (LEN=4) :: END_POINT_STR
      CHARACTER (LEN=5) :: SCENAME
      CHARACTER (LEN=80) :: BANNER
      CHARACTER (LEN=4) :: TOTAL_NUM_OF_END_POINTS_STR=("1")
      CHARACTER (LEN=50) :: STUDY_PERIOD_STR
      SAVE BANNER,NORMAL,INVERSE,SCREEN1,CAP_PLANNING_METHOD_ANNDECOMP
      CHARACTER (LEN=256) :: SIM_FILE_NAME,LC_SIM_FILE_NAME
      CHARACTER (LEN=5) :: MONTH_DAY,SIM_MONTH_DAY
      CHARACTER (LEN=2) :: SIM_MONTH
      LOGICAL (KIND=1) :: LAHEY_LF95,VERSION_IS_RW
      CHARACTER (LEN=5) :: VERSION
      INTEGER (KIND=2) :: EXTENSION_YEARS
!
      CALL MYNAME(LC_SIM_FILE_NAME)

      CALL UPC(LC_SIM_FILE_NAME,SIM_FILE_NAME)
      SIM_MONTH_DAY = MONTH_DAY(SIM_FILE_NAME)
      SIM_MONTH = SIM_MONTH_DAY(1:2)
      IF(SIM_MONTH(2:2) == '/') THEN
         SIM_MONTH(2:2) = SIM_MONTH(1:1)
         SIM_MONTH(1:1) = '0'
      ENDIF
      SHORT_FORM = .FALSE.
      END_POINTS = GET_NUM_END_POINTS_IN_OVL_FILE()
!      IF(.NOT. ASSET_ANALYST_ONLY() .AND.
!     +  (TRANSACT_ANALYST_ONLY() .OR. .NOT. INIT_FILE_IS_ACTIVE())) THEN
      IF(.NOT. ASSET_ANALYST_ONLY() .AND.
     +                                 .NOT. INIT_FILE_IS_ACTIVE()) THEN
         BANNER = " MIDAS Gold  2003."//SIM_MONTH//
     +            "      TransAct Analyst & T"//
     +            "ransAct C              Version "//"7A" ! VERSION
      ELSEIF(UTILITY_TYPE() == 'K') THEN
            BANNER = " MIDAS Gold                       Simulation"
     +                          //" Module               KEPCO Release "
      ELSEIF(INDEX(SIM_FILE_NAME,'5B') /= 0) THEN
         IF(SHORT_FORM) THEN
            BANNER=" MIDAS Gold 99             Short Form Simulation"//
     +                     " Module     Asset Classes  "//VERSION()
         ELSE IF(UTILITY_TYPE() == 'P') THEN
            BANNER = " MIDAS Gold 99                  Muni Simulation"//
     +                     " Module      Asset Classes  "//VERSION()
         ELSE
            BANNER = " MIDAS Gold Analyst 2003.01         "
     +                   //"Simulation Module         Transact Analyst "
c MIDAS Gold Analyst 2000.03         Simulation Module         Transact Analyst "
         ENDIF
      ELSE
         IF(SHORT_FORM) THEN
            BANNER= " MIDAS Gold 99             Short Form Simulation"//
     +                     " Module     32 Bit Version "//VERSION()
         ELSE IF(UTILITY_TYPE() == 'P') THEN
            BANNER = " MIDAS Gold 99                  Muni Simulation"//
     +                     " Module      32 Bit Version "//VERSION()
         ELSE
            IF(INDEX(SIM_FILE_NAME,'7A') /= 0) THEN
               IF(MONTHLY_MIDAS_ACTIVE) THEN
                  BANNER=" MIDAS Gold 2003."//SIM_MONTH//
     +                     "      Monthly Financial and"//
     +                     " Transact Analyst      Version "//"7A" ! VERSION
               ELSE
                  BANNER=" MIDAS Gold 2003."//SIM_MONTH//
     +                     "            Annual Transact"//
     +                     " Analyst               Version "//"7A" ! VERSION
               ENDIF
            ELSEIF(INDEX(SIM_FILE_NAME,'7M') /= 0) THEN
               BANNER=" MIDAS Gold 2003."//SIM_MONTH//
     +                     "      Monthly Financial and"//
     +                     " Transact Analyst      Version "//"7M" ! VERSION
               BANNER=" MIDAS Gold 2003."//SIM_MONTH//
     +                     "                    Monthly"//
     +                     " MIDAS GOLD            Version "//"7M" ! VERSION
               MONTHLY_MIDAS_ACTIVE = .TRUE.
            ELSEIF(INDEX(SIM_FILE_NAME,'6A') /= 0) THEN
               IF(MONTHLY_MIDAS_ACTIVE) THEN
                  BANNER=" MIDAS Gold 2003."//SIM_MONTH//
     +                     "      Monthly Financial and"//
     +                     " Transact Analyst      Version "//"6A" ! VERSION
               ELSE
                  BANNER=" MIDAS Gold 2003."//SIM_MONTH//
     +                     "            Annual Transact"//
     +                     " Analyst               Version "//"6A" ! VERSION
               ENDIF
            ELSEIF(INDEX(SIM_FILE_NAME,'6M') /= 0) THEN
               BANNER=" MIDAS Gold 2003."//SIM_MONTH//
     +                     "           Monthly Transact"//
     +                     " Analyst               Version "//"6M" ! VERSION
               MONTHLY_MIDAS_ACTIVE = .TRUE.
            ELSEIF(INDEX(SIM_FILE_NAME,'AA') /= 0) THEN
               BANNER=" MIDAS Gold 2003."//SIM_MONTH//
     +                     "                     Asset "//
     +                     "Analyst                Version "//"AA" ! VERSION
            ELSEIF(INDEX(SIM_FILE_NAME,'MM') /= 0) THEN
               BANNER=" MIDAS Gold 2003."//SIM_MONTH//
     +                     "                    Monthly"//
     +                     " MIDAS GOLD            Version "//"MM" ! VERSION
               MONTHLY_MIDAS_ACTIVE = .TRUE.
            ELSE
               BANNER=" MIDAS Gold 2003."//SIM_MONTH//
     +                     "            Annual Transact"//
     +                     " Analyst               Version "//"7A" ! VERSION
            ENDIF
         ENDIF
      ENDIF
      CAP_PLANNING_METHOD_ANNDECOMP = .FALSE.
      CALL LWC(SIM_FILE_NAME,LC_SIM_FILE_NAME)
!
! FIND TOTAL NUMBER OF ENDPOINTS
!
      IF(END_POINTS > 1) THEN
         END_POINT_STR = ' '
         WRITE(END_POINT_STR,"(I4)") END_POINTS
         IF(END_POINTS < 1000) THEN
            I = 4
            DO WHILE ((END_POINT_STR(I:I) /= ' ') .AND. (I > 1))
               I = I - 1
            ENDDO
            TOTAL_NUM_OF_END_POINTS_STR = END_POINT_STR(I+1:4)
         ELSE
            TOTAL_NUM_OF_END_POINTS_STR = END_POINT_STR
         ENDIF
      ELSE
         TOTAL_NUM_OF_END_POINTS_STR = '1'
      ENDIF
      IF(EXTENSION_YEARS() > 0) THEN
         WRITE(STUDY_PERIOD_STR,"(I4,A1,I4,A1,I4)") BASE_YEAR+1,'-',
     +                                 ENDYR,'-',ENDYR+EXTENSION_YEARS()
      ELSE     
         WRITE(STUDY_PERIOD_STR,"(I4,A1,I4)") BASE_YEAR+1,'-',ENDYR
      ENDIF   
      IF(LAHEY_LF95()) THEN
         CALL LF95_VERSION_IS_RW(VERSION_IS_RW)
         IF(VERSION_IS_RW) THEN
            CALL RW_INITIALIZE_STATUS_LINE(cldata%SCENAME,
     +                                     TOTAL_NUM_OF_END_POINTS_STR,
     +                                     STUDY_PERIOD_STR,
     +                                     SIM_MONTH_DAY)
         ELSE
            BANNER="MIDAS Gold 2003."//SIM_MONTH//
     +                          "  Annual Transact Analyst Version 95XP" ! VERSION
            CALL MG_LOCATE_WRITE(5,60,BANNER,ALL_VERSIONS,0)
            CALL MG_LOCATE_WRITE(5,60,'Study: '//cldata%SCENAME
     + ,ALL_VERSIONS,0)
            CALL MG_LOCATE_WRITE(6,62,
     +                       'Study Period: '//trim(STUDY_PERIOD_STR),
     +                                                   ALL_VERSIONS,0)
            IF(END_POINTS > 1) THEN
               CALL MG_LOCATE_WRITE(8,64,'Running '//
     +               trim(TOTAL_NUM_OF_END_POINTS_STR)//' endpoints.',
     +                                                   ALL_VERSIONS,0)
            ELSE
               CALL MG_LOCATE_WRITE(8,64,'Running the Base Case',
     +                                                   ALL_VERSIONS,0)
            ENDIF
            CALL MG_LOCATE_WRITE(8,64,'Using simulation module '//
     +       trim(LC_SIM_FILE_NAME)//' dated '//trim(SIM_MONTH_DAY),
     +                                                   ALL_VERSIONS,0)
         ENDIF
      ELSE
         
         NORMAL = NORMAL_VIDEO()
         INVERSE = INVERSE_VIDEO()
         CALL WINDBUF(SCREEN1," ",NORMAL)
         CALL OPENWIND(1,0,0,0,79,0,INVERSE)
         CALL FILLB(1," ",INVERSE)
         CALL BLDB(1,0,0,BANNER,INVERSE)
         CALL OPENWIND(7,24,0,24,79,0,INVERSE)
         CALL FILLB(7," ",INVERSE)
         CALL BLDB(7,0,0," "//trim(SIM_MONTH_DAY)//" "//
     +                trim(LC_SIM_FILE_NAME)//" ",INVERSE)
         CALL OPENWIND(2,2,1,23,79,0,NORMAL)
         CALL FILLB(2," ",NORMAL)
! DO NOT INDENT THE FOLLOWING LINES
      CALL BLDB(2,1,0, "            Execution Status                    
     +Operating Parameters",NORMAL)
      IF(ASSET_ANALYST_ONLY()) THEN
      CALL BLDB(2,2,0, "      �����������������������������ͻ       ����
     +�������������������������ͻ",NORMAL)
      CALL BLDB(2,3,0, "      �                             �       �  S 
     +tudy name:                �",NORMAL)
      CALL BLDB(2,4,0, "      �          Year               �       �  S
     +tudy Period:              �",NORMAL)
      CALL BLDB(2,5,0, "      �                             �       �  E
     +xtension Period to        �",NORMAL)
      CALL BLDB(2,6,0, "      �        Financial            �       �  E
     +nd Point:                 �",NORMAL)
      CALL BLDB(2,7,0, "      �                             �       �  O
     +verlay:                   �",NORMAL)
      CALL BLDB(2,8,0, "      �                             �       �   
     +                          �",NORMAL)
      CALL BLDB(2,9,0, "      �                             �       ����
     +�������������������������ͼ",NORMAL)
      CALL BLDB(2,10,0,"      �                             �",NORMAL)
      CALL BLDB(2,11,0,"      �                             �       ����
     +�������������������������ͻ",NORMAL)
      CALL BLDB(2,12,0,"      �                             �       � De
     +bt Accounts               �",NORMAL)
      CALL BLDB(2,13,0,"      �                             �       � Ex
     +isting Asset Accounts     �",NORMAL)
      CALL BLDB(2,14,0,"      �                             �       � De
     +ferred Debit Accounts     �",NORMAL)
      CALL BLDB(2,15,0,"      �                             �       � Ex
     +pense Accounts            �",NORMAL)
      CALL BLDB(2,16,0,"      �                             �       � Fu
     +ture Asset Accounts       �",NORMAL)
      CALL BLDB(2,17,0,"      �                             �       � Nu
     +clear Fuel Accounts       �",NORMAL)
      CALL BLDB(2,18,0,"      �                             �       �   
     +                          �",NORMAL)
      CALL BLDB(2,19,0,"      �����������������������������ͼ       ����
     +�������������������������ͼ",NORMAL)
      ELSE
      CALL BLDB(2,2,0, "      �����������������������������ͻ       ����
     +�������������������������ͻ",NORMAL)
      CALL BLDB(2,3,0, "      �                             �       �  S 
     +tudy name:                �",NORMAL)
      CALL BLDB(2,4,0, "      �          Year               �       �  S
     +tudy Period:              �",NORMAL)
      CALL BLDB(2,5,0, "      �                             �       �  E
     +xtension Period to        �",NORMAL)
      CALL BLDB(2,6,0, "      �                             �       �  E
     +nd Point:                 �",NORMAL)
      CALL BLDB(2,7,0, "      �        Markets              �       �  O
     +verlay:                   �",NORMAL)
      CALL BLDB(2,8,0, "      �        Resources            �       �   
     +                          �",NORMAL)
      CALL BLDB(2,9,0, "      �        Operations           �       ����
     +�������������������������ͼ",NORMAL)
!      IF(.NOT. TRANSACT_ANALYST_ONLY() .AND. INIT_FILE_IS_ACTIVE()) THEN
      IF(INIT_FILE_IS_ACTIVE()) THEN
      CALL BLDB(2,10,0,"      �        Financial            �",NORMAL)

      CALL BLDB(2,11,0,"      �                             �       ����
     +�������������������������ͻ",NORMAL)
      CALL BLDB(2,12,0,"      �                             �       � De
     +bt Accounts               �",NORMAL)
      CALL BLDB(2,13,0,"      �                             �       � Ex
     +isting Asset Accounts     �",NORMAL)
      CALL BLDB(2,14,0,"      �                             �       � De
     +ferred Debit Accounts     �",NORMAL)
      CALL BLDB(2,15,0,"      �                             �       � Ex
     +pense Accounts            �",NORMAL)
      CALL BLDB(2,16,0,"      �                             �       � Fu
     +ture Asset Accounts       �",NORMAL)
      CALL BLDB(2,17,0,"      �                             �       � Nu
     +clear Fuel Accounts       �",NORMAL)
      CALL BLDB(2,18,0,"      �                             �       � Ge
     +nerating Units            �",NORMAL)
      CALL BLDB(2,19,0,"      �����������������������������ͼ       ����
     +�������������������������ͼ",NORMAL)
      ELSE
      CALL BLDB(2,10,0,"      �                             �",NORMAL)

      CALL BLDB(2,11,0,"      �                             �       ����
     +�������������������������ͻ",NORMAL)
      CALL BLDB(2,12,0,"      �                             �       � Ge
     +nerataing Units:          �",NORMAL)
      CALL BLDB(2,13,0,"      �                             �       �  T
     +hermal Units:             �",NORMAL)
      CALL BLDB(2,14,0,"      �                             �       �  
     +Dispatched Blocks:        �",NORMAL)
      CALL BLDB(2,15,0,"      �                             �       �  H    
     +ydro Units:               �",NORMAL)
      CALL BLDB(2,16,0,"      �                             �       � Tr
     +ansction Groups:          �",NORMAL)
      CALL BLDB(2,17,0,"      �                             �       �  
     +                          �",NORMAL)
      CALL BLDB(2,18,0,"      �                             �       �   
     +                          �",NORMAL)
      CALL BLDB(2,19,0,"      �����������������������������ͼ       ����
     +�������������������������ͼ",NORMAL)
      ENDIF
      ENDIF
      CALL BLDB(2,21,0,"                Copyright (c) 2003 M.S. Gerber &
     + Associates, Inc.          ",NORMAL)
         CALL DISPSCN(SCREEN1)
!
         CALL SETATTR(NORMAL)
         IF(END_POINTS > 1) CALL MG_LOCATE_WRITE(8,64,
     +                           'of '//TOTAL_NUM_OF_END_POINTS_STR,
     +                                                   ALL_VERSIONS,0)
         CALL MG_LOCATE_WRITE(5,60,cldata%SCENAME,ALL_VERSIONS,0)
         WRITE(SCREEN_MESSAGES,"(I4,A1,I4)") BASE_YEAR+1,'-',ENDYR
         CALL MG_LOCATE_WRITE(6,62,trim(SCREEN_MESSAGES),
     +                                                   ALL_VERSIONS,0)
         IF(LAST_EXTENSION_YEAR > ENDYR) THEN
            WRITE(SCREEN_MESSAGES,"(I4)") LAST_EXTENSION_YEAR
            CALL MG_LOCATE_WRITE(7,67,trim(SCREEN_MESSAGES),
     +                                                   ALL_VERSIONS,0)
         ENDIF
      ENDIF ! LF95 
      RETURN
!
      ENTRY ANN_DECOMP_SCREEN
         CAP_PLANNING_METHOD_ANNDECOMP = .TRUE.
         CALL WINDBUF(SCREEN2," ",NORMAL)
         CALL OPENWIND(6,0,0,0,79,0,INVERSE)
         CALL FILLB(6," ",INVERSE)
         CALL BLDB(6,0,0,BANNER,INVERSE)
         CALL OPENWIND(9,24,0,24,79,0,INVERSE)
         CALL FILLB(9," ",INVERSE)
         CALL OPENWIND(10,8,9,10,73,0,NORMAL)
         CALL FILLB(10," ",NORMAL)
         CALL OPENWIND(11,13,9,20,73,0,NORMAL)
         CALL FILLB(11," ",NORMAL)
         CALL OPENWIND(5,1,1,23,79,0,NORMAL)
         CALL FILLB(5," ",NORMAL)
         CALL BLDB(5,1,6, "����������������� Annualized Decomposition"//
     +                             " Planning ���������������ͻ",NORMAL)
         CALL BLDB(5,2,6, "� Planning Year:       Best Value:        "//
     +                             "  Testing Plan:           �",NORMAL)
         CALL BLDB(5,3,6, "�       Load  Production  Financial  Rates"//
     +                             "  Object Value:           �",NORMAL)
         CALL BLDB(5,4,6, "�                                         "//
     +                             "                          �",NORMAL)
         CALL BLDB(5,4,40,trim(OBJ_FUNCTION_DESC()))
         CALL BLDB(5,5,6, "���������������������� Options Being Teste"//
     +                             "d �����������������������͹",NORMAL)
         CALL BLDB(5,6,6, "� Resource                  Type  Resource"//
     +                             "                  Type    �",NORMAL)
         CALL BLDB(5,7,6, "�                                         "//
     +                             "                          �",NORMAL)
         CALL BLDB(5,8,6, "�                                         "//
     +                             "                          �",NORMAL)
         CALL BLDB(5,9,6, "�                                         "//
     +                             "                          �",NORMAL)
         CALL BLDB(5,10,6,"������������������������ Selected Options "//
     +                             "�������������������������͹",NORMAL)
         CALL BLDB(5,11,6,"� Year  Obj. Val.  Cap     Cost  Options A"//
     +                             "dded                      �",NORMAL)
         CALL BLDB(5,12,6,"�                                         "//
     +                             "                          �",NORMAL)
         CALL BLDB(5,13,6,"�                                         "//
     +                             "                          �",NORMAL)
         CALL BLDB(5,14,6,"�                                         "//
     +                             "                          �",NORMAL)
         CALL BLDB(5,15,6,"�                                         "//
     +                             "                          �",NORMAL)
         CALL BLDB(5,16,6,"�                                         "//
     +                             "                          �",NORMAL)
         CALL BLDB(5,17,6,"�                                         "//
     +                             "                          �",NORMAL)
         CALL BLDB(5,18,6,"�                                         "//
     +                             "                          �",NORMAL)
         CALL BLDB(5,19,6,"�                                         "//
     +                             "                          �",NORMAL)
         CALL BLDB(5,20,6,"������������������������������������������"//
     +                             "�������������������������ͼ",NORMAL)
         CALL BLDB(5,22,6,"          Copyright (c) 2003 M.S. Gerbe"//
     +                          "r & Associates, Inc.          ",NORMAL)
!        CALL BLDB(5,23,0,STATUS_LINE,INVERSE)
         CALL DISPSCN(SCREEN2)
         CALL LOCATEW(7,0,54)
         CALL PRINTW(7,'End Point     /'//TOTAL_NUM_OF_END_POINTS_STR)
!
! SET UP VIRTUAL SCREEN FOR OPTIONS SELCECTED AND OPTIONS TESTED
!
         CALL OPENVS(1000,255,55)
         CALL CLSVS(1000)
         CALL SET_VIRTUAL_ROW_TO_ZERO
      RETURN
!
      ENTRY DISPLAY_FORECAST_TYPE
         IF(CAP_PLANNING_METHOD_ANNDECOMP) THEN
            CALL LOCATEW(7,0,1)
            CALL PRINTW(7,'Load: ')
            IF(SYSTEM_BASED_FORECAST()) THEN
               CALL PRINTW(7,'System      ')
            ELSEIF(POOLING_TRANSACTIONS()) THEN 
               CALL PRINTW(7,'Power Pool  ')
            ELSEIF(CONTROL_AREA_FORECAST()) THEN
               CALL PRINTW(7,'Control Area')
            ELSE
               CALL PRINTW(7,'Class       ')
            ENDIF
         ELSE
         ENDIF
      RETURN
      ENTRY DISPLAY_PROC_METHOD
         IF(CAP_PLANNING_METHOD_ANNDECOMP) THEN
            CALL LOCATEW(7,0,20)
            CALL PRINTW(7,'Prod: ')
            IF(FACET_IS_ACTIVE()) THEN
               CALL PRINTW(7,'Facet')
            ELSEIF(PROCMETH() == 1) THEN
               CALL PRINTW(7,'Booth')
            ELSE IF(PROCMETH() == 2) THEN
               CALL PRINTW(7,'DRate')
            ELSE IF(PROCMETH() == 3) THEN
               CALL PRINTW(7,'BB&DR')
            ELSE IF(PROCMETH() == 4) THEN
               CALL PRINTW(7,'EnBal')
            ENDIF
         ELSE
            IF(FACET_IS_ACTIVE()) THEN
               CALL MG_LOCATE_WRITE(11,9,"Facet",ALL_VERSIONS,0)
            ELSEIF(PROCMETH() == 1) THEN
               CALL MG_LOCATE_WRITE(11,9,"Booth",ALL_VERSIONS,0)
            ELSE IF(PROCMETH() == 2) THEN
               CALL MG_LOCATE_WRITE(11,9,"DRate",ALL_VERSIONS,0)
            ELSE IF(PROCMETH() == 3) THEN
               CALL MG_LOCATE_WRITE(11,9,"BB&DR",ALL_VERSIONS,0)
            ELSE IF(PROCMETH() == 4) THEN
               CALL MG_LOCATE_WRITE(11,9,"EnBal",ALL_VERSIONS,0)
            ENDIF
         ENDIF
      RETURN
!
      ENTRY DISPLAY_FINANCIAL_DRIVER(OPMETH,COVERAGE_RATIO)
         IF(CAP_PLANNING_METHOD_ANNDECOMP) THEN
            CALL LOCATEW(7,0,35)
            CALL PRINTW(7,'Finance:')
            IF(OPMETH == 'P') THEN
               CALL PRINTW(7,' Price')
            ELSE IF(OPMETH == 'O') THEN
               CALL PRINTW(7,' Op Rv')
            ELSE IF(OPMETH == 'R') THEN
               CALL PRINTW(7,' ROR  ')
            ELSE IF(OPMETH == 'E') THEN
               CALL PRINTW(7,' ROE  ')
            ELSE IF(OPMETH == 'C') THEN
               CALL PRINTW(7,' Covrg')
            ELSE IF(OPMETH == 'N') THEN
               CALL PRINTW(7,' ROR  ')
            ENDIF
         ELSE
            IF(OPMETH == 'P') THEN
               CALL MG_CLEAR_LINE_WRITE(12,9,13,"Price",ALL_VERSIONS,0)
            ELSE IF(OPMETH == 'O') THEN
               CALL MG_CLEAR_LINE_WRITE(12,9,13,"Op Rv",ALL_VERSIONS,0)
            ELSE IF(OPMETH == 'R') THEN
               CALL MG_CLEAR_LINE_WRITE(12,9,13,"ROR",ALL_VERSIONS,0)
            ELSE IF(OPMETH == 'E') THEN
               CALL MG_CLEAR_LINE_WRITE(12,9,13,"ROE",ALL_VERSIONS,0)
            ELSE IF(OPMETH == 'C') THEN
               WRITE(SCREEN_MESSAGES,"('Cov',I2)") COVERAGE_RATIO 
               CALL MG_CLEAR_LINE_WRITE(12,9,13,
     +                           trim(SCREEN_MESSAGES),ALL_VERSIONS,0)
            ELSE IF(OPMETH == 'N') THEN
               CALL MG_CLEAR_LINE_WRITE(12,9,13,"ROR",ALL_VERSIONS,0)
            ENDIF
         ENDIF
      RETURN
      ENTRY DISPLAY_DSM_ACTIVE
         IF(CAP_PLANNING_METHOD_ANNDECOMP) THEN
            CALL LOCATEW(7,0,50)
            CALL PRINTW(7,'DSM')
         ELSE
            CALL MG_CLEAR_LINE_WRITE(9,9,13,"DSM",ALL_VERSIONS,0)
         ENDIF
      RETURN
 1000 FORMAT('&',A,A,A,A)
      END

!***********************************************************************
      SUBROUTINE MG_SCREEN_WRITE_FUNCTIONS()
!***********************************************************************
      use SpinDriftLib
      USE PROD_ARRAYS_DIMENSIONS
      INTEGER :: ROW1,ROW2,COL1,COL2,WINDOW,VERSION
      CHARACTER (LEN=*) :: SCREEN_MESSAGES
      CHARACTER (LEN=256) :: SIM_FILE_NAME,lc_SIM_FILE_NAME
!      INTEGER ALL_VERSIONS,LF77_VERSION,LF95_VERSION,WIN95_VERSION
      INTEGER (KIND=4) :: SET_VERSION,LENGHT
      INTEGER (KIND=4), PARAMETER :: ALL_VERSIONS=0,
     +                               LF77_VERSION=1,
     +                               LF95_VERSION=2,
     +                               WIN95_VERSION=3
      LOGICAL (KIND=1) :: THIS_VERSION_IS_WINDOWS=.FALSE.,
     +          THIS_VERSION_IS_LF95=.FALSE.,
     +          THIS_VERSION_IS_LF77=.FALSE.,
     +          THIS_VERSION_IS_RealWIN=.FALSE.,
     +          THIS_VERSION_IS_VSW=.FALSE.
      LOGICAL (KIND=1) :: SP_CAPEX_ACTIVE,R_RW_IS_ACTIVE,
     +                    SP_CAPEX_DISPLAY_ACTIVE
!
! ADD THE VERSION LOGIC HERE
!            THIS_VERSION_IS_RealWIN = .TRUE.
!      RETURN
         IF(SP_CAPEX_ACTIVE()) THEN
            IF(SP_CAPEX_DISPLAY_ACTIVE()) THEN
               THIS_VERSION_IS_RealWIN = .TRUE.
            ELSE
               THIS_VERSION_IS_LF95 = .TRUE.
            ENDIF
            RETURN
         ENDIF
         CALL MYNAME(lc_SIM_FILE_NAME)
         CALL UPC(lc_SIM_FILE_NAME,SIM_FILE_NAME)
         IF(INDEX(SIM_FILE_NAME,'WIN') /= 0) THEN
            THIS_VERSION_IS_WINDOWS = .TRUE.
         ELSEIF(INDEX(SIM_FILE_NAME,'RW') /= 0) THEN
            THIS_VERSION_IS_RealWIN = .TRUE.
         ELSEIF(INDEX(SIM_FILE_NAME,'VSW') /= 0) THEN
            THIS_VERSION_IS_VSW = .TRUE.
         ELSEIF(INDEX(SIM_FILE_NAME,'95') /= 0) THEN
            THIS_VERSION_IS_LF95 = .TRUE.
         ELSE
            THIS_VERSION_IS_RealWIN = .TRUE.
         ENDIF
            THIS_VERSION_IS_RealWIN = .TRUE.
      RETURN
!***********************************************************************
      ENTRY LF95_VERSION_IS_RW(R_RW_IS_ACTIVE)
!***********************************************************************
         R_RW_IS_ACTIVE =  THIS_VERSION_IS_RealWIN
      RETURN
!***********************************************************************
      ENTRY MG_CLEAR_AREA_WRITE(ROW1,COL1,ROW2,COL2,SCREEN_MESSAGES,
     +                                                   VERSION,WINDOW)
!***********************************************************************
         IF((VERSION==ALL_VERSIONS .OR. VERSION==LF95_VERSION) .AND.
     +             (THIS_VERSION_IS_VSW .OR. THIS_VERSION_IS_LF95)) THEN
            CALL WRITE_CONSOL_LINE(SCREEN_MESSAGES)
         ELSEIF((VERSION==ALL_VERSIONS .OR. VERSION==WIN95_VERSION).AND.
     +      (THIS_VERSION_IS_RealWIN .OR. THIS_VERSION_IS_WINDOWS)) THEN
            CALL write_scroll_line_RW(trim(SCREEN_MESSAGES),WINDOW)
         ELSEIF(VERSION==ALL_VERSIONS .OR. VERSION==LF77_VERSION) THEN !LF77 VERSION
            CALL CLS(ROW1,COL1,ROW2,COL2)
            CALL LOCATE(ROW1,COL1)
            CALL WRITE_LF77_SCREEN(SCREEN_MESSAGES)
         ENDIF
      RETURN   
!***********************************************************************
      ENTRY MG_CLEAR_LINE_WRITE(ROW1,COL1,COL2,SCREEN_MESSAGES,
     +                                                   VERSION,WINDOW)
!***********************************************************************
         IF((VERSION==ALL_VERSIONS .OR. VERSION==LF95_VERSION) .AND.
     +             (THIS_VERSION_IS_VSW .OR. THIS_VERSION_IS_LF95)) THEN
            CALL WRITE_CONSOL_LINE(SCREEN_MESSAGES)
         ELSEIF((VERSION==ALL_VERSIONS .OR. VERSION==WIN95_VERSION).AND.
     +      (THIS_VERSION_IS_RealWIN .OR. THIS_VERSION_IS_WINDOWS)) THEN
            CALL write_scroll_line_RW(trim(SCREEN_MESSAGES),WINDOW)
         ELSEIF(VERSION==ALL_VERSIONS .OR. VERSION==LF77_VERSION) THEN !LF77 VERSION 
            CALL CLS(ROW1,COL1,COL2)
            CALL LOCATE(ROW1,COL1)
            CALL WRITE_LF77_SCREEN(SCREEN_MESSAGES)
         ENDIF
      RETURN   
!***********************************************************************
      ENTRY MG_LOCATE_WRITE(ROW1,COL1,SCREEN_MESSAGES,VERSION,WINDOW)
!***********************************************************************
         IF((VERSION==ALL_VERSIONS .OR. VERSION==LF95_VERSION) .AND.
     +             (THIS_VERSION_IS_VSW .OR. THIS_VERSION_IS_LF95)) THEN
            CALL WRITE_CONSOL_LINE(SCREEN_MESSAGES)
         ELSEIF((VERSION==ALL_VERSIONS .OR. VERSION==WIN95_VERSION).AND.
     +      (THIS_VERSION_IS_RealWIN .OR. THIS_VERSION_IS_WINDOWS)) THEN
            CALL write_scroll_line_RW(trim(SCREEN_MESSAGES),WINDOW)
         ELSEIF(VERSION==ALL_VERSIONS .OR. VERSION==LF77_VERSION) THEN !LF77 VERSION
            LENGHT = LEN(SCREEN_MESSAGES) 
            CALL LOCATE(ROW1,COL1)
            CALL WRITE_LF77_SCREEN(SCREEN_MESSAGES)
         ENDIF
      RETURN   
!***********************************************************************
      ENTRY MG_CLEAR_LINE(ROW1,COL1,COL2,VERSION,WINDOW)
!***********************************************************************
         IF((VERSION==ALL_VERSIONS .OR. VERSION==LF95_VERSION) .AND.
     +             (THIS_VERSION_IS_VSW .OR. THIS_VERSION_IS_LF95)) THEN
         ELSEIF((VERSION==ALL_VERSIONS .OR. VERSION==WIN95_VERSION).AND.
     +      (THIS_VERSION_IS_RealWIN .OR. THIS_VERSION_IS_WINDOWS)) THEN
         ELSEIF(VERSION==ALL_VERSIONS .OR. VERSION==LF77_VERSION) THEN !LF77 VERSION
            CALL CLS(ROW1,COL1,COL2)
         ENDIF
      RETURN   
!***********************************************************************
      ENTRY UPDATE_TIME_ON_STATUS_LINE(SCREEN_MESSAGES)
!***********************************************************************
         IF(THIS_VERSION_IS_RealWIN) THEN
            CALL RW_UPDATE_STATUS_TIME(SCREEN_MESSAGES)
         ENDIF
      RETURN
      END

