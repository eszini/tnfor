!     ******************************************************************
!     msgmmcom.for
!     Copyright(c) M.S. Gerber & Associates 2000
!
!     Created: 1/29/2003 1:30:28 PM
!     Author : MARK S GERBER
!     Last change: MSG 9/15/2004 4:41:32 PM
!     ******************************************************************

C***********************************************************************
C*                                                                     *
C*                              A F U D C                              *
C*                                                                     *
C*          COPYRIGHT (C) 1982,85,86 M.S. GERBER & ASSOCIATES, INC     *
C*                         ALL RIGHTS RESERVED                         *
C*  THIS CODE IS A DERIVATIVE OF CODE DEVELOPED BY M.S.G. OF THE       *
C*  FIN MODEL AND IS COPYRIGHTED BY M.S. GERBER & ASSOCIATES, INC      *
C***********************************************************************
C                                                                      *
C     PURPOSE:                                                         *
C        AFUDC CALCULATES:                                             *
C           ANNUAL YEAR END CWIP BALANCE                     CWIP      *
C           ANNUAL AFUDC ON CONSTRUCTION EXPENSES            AFDC1     *
C           ANNUAL afudc_on_plant ENTERING SERVICE           AFDC2     *
C           ANNUAL AMOUNT OF PLANT ENTERING SERVICE          CEP       *
C           ANNUAL CASH EXPENDITURES                         CE        *
C           ANNUAL CWIP IN RATE BASE                         RBCWIP    *
C                                                                      *
C***********************************************************************
C
      SUBROUTINE AFUDC_MONTHLY(AFUDC1,CWIPRB,AFDCB1,AFDCFC,CWIP1,
     +   AFDCSW,AFDCPERIODS,CE,CEP,AFDC1,AFDC2,CWIP,RBCWIP,
     +   AFDC1B,AFDC2B,
     +   SERVICEMO,CAPINRST,DEPMET,PCAPINRST,AFDC_CAP_VECTOR,
     +   FIRSTYR,CURRENT_INTEREST_CAP,WHO_CALLED,
     +   FINANCIAL_SIMULATION_YEARS,CLASS_TAX_LIFE,AFUDC_IN_CWIP,
     +                 BOOK_EXPEN,
     +                 PLANT_2_SERVICE,
     +                 MONTHLY_AFUDC_ON_CASH,
     +                 MONTHLY_AFUDC_ON_PLANT,
     +                 MONTHLY_CWIP,
     +                 MONTHLY_AFUDC_IN_CWIP,
     +                 MONTHLY_CWIP_IN_RATEBASE,
     +                 MONTHLY_CAPITALIZED_INTEREST,
     +                 MONTHLY_CURRENT_INTEREST,
     +                 MONTHLY_INTEREST_TO_TAX_VALUE,
     +                 MONTHLY_CURRENT_INTEREST_CAP,
     +                 USE_PLANT_PERCENTAGE,
     +                 PERCENT_PLANT_2_SERVICE)
C
      INCLUDE 'SpinLib.MON'
      INCLUDE 'SIZECOM.MON'
      INCLUDE 'GLOBECOM.MON'
C
      CHARACTER*2 WHO_CALLED
      INTEGER*2 FINANCIAL_SIMULATION_YEARS
      REAL*4 BOOK_EXPEN(0:12,0:*),
     +       PLANT_2_SERVICE(0:12,0:*),
     +       MONTHLY_AFUDC_ON_CASH(0:12,0:*),
     +       MONTHLY_AFUDC_ON_PLANT(0:12,0:*),
     +       MONTHLY_CWIP(0:12,0:*),
     +       MONTHLY_AFUDC_IN_CWIP(0:12,0:*),
     +       MONTHLY_CWIP_IN_RATEBASE(0:12,0:*),
     +       MONTHLY_CAPITALIZED_INTEREST(0:12,0:*),
     +       MONTHLY_CURRENT_INTEREST(0:12,0:*),
     +       MONTHLY_INTEREST_TO_TAX_VALUE(0:12,0:*), 
     +       MONTHLY_CURRENT_INTEREST_CAP(0:12,0:*), 
     +       PERCENT_PLANT_2_SERVICE(0:12,0:*)
      LOGICAL*1 USE_PLANT_PERCENTAGE
      CHARACTER*1 RIPPLE_ZERO,RIPPLE_AVERAGE
      PARAMETER(RIPPLE_ZERO='Z',RIPPLE_AVERAGE='A')
      REAL*4 CLASS_TAX_LIFE,AFUDC_CASH_IN_CWIP,
     +     AFUDC_IN_CWIP(MAX_FINANCIAL_SIMULATION_YEARS)
      REAL*4 AFDCRT,AFDCBR,INTEREST_CAP_RATE,CURRENT_INTEREST_CAP_RATE,
     +     PROPERTY_ESCALATION,
     +     AFUDC_NF_RATE
      COMMON/AFUDC_STUFF/ AFDCRT(MAX_FINANCIAL_SIMULATION_YEARS),
     +        AFDCBR(MAX_FINANCIAL_SIMULATION_YEARS),
     +        INTEREST_CAP_RATE(MAX_FINANCIAL_SIMULATION_YEARS),
     +        PROPERTY_ESCALATION(MAX_FINANCIAL_SIMULATION_YEARS),
     +        CURRENT_INTEREST_CAP_RATE(MAX_FINANCIAL_SIMULATION_YEARS),
     +        AFUDC_NF_RATE(MAX_FINANCIAL_SIMULATION_YEARS)
C
      INTEGER*2 LAST_CAP_YEAR,YR,MO
      REAL*4 TOTAL_2_J,AFDC1_BALANCE,MONTHLY_AFUDC_RATE,
     +       ADJUST_FACTOR(MAX_FINANCIAL_SIMULATION_YEARS),
     +       ANNUAL_MONTHLY_AFDC1(MAX_FINANCIAL_SIMULATION_YEARS)
      INTEGER*2 I,J,IVEC,AFDCSW,AFDC_CAP_VECTOR
      INTEGER*2 AFDCPERIODS,SERVICEMO
      INTEGER*2 IOFFSET,FIRSTYR
      CHARACTER*4 DEPMET
      CHARACTER*1 DATA_TYPE
      REAL*4 OUTSERVICE,INSERVICE,AF1C,AF1CBAL,AF1BAL,RTEMP,AF2C
      REAL*4 CWIP1,RCWIP,CWIPRB,AFUDC1,AFDCFC,AFDCB1,TOTCEP,
     +     TAFDC1,TAFDC2,RATIO,BORAFDC,
     +     CE(*),CEP(*),AFDC1(*),AFDC2(*),CWIP(*),RBCWIP(*),
     +     AFDC1B(*),AFDC2B(*),
     +     CAPINRST(*),PCAPINRST(*),AFC1BOR,CURRENT_INTEREST_CAP(*)
      REAL*4 EFFAFDCRATE(MAX_FINANCIAL_SIMULATION_YEARS),
     +     EFFBRAFDC(MAX_FINANCIAL_SIMULATION_YEARS)
      REAL*4 TOTAL_INTEREST_CAP,
     +     TOTAL_CURRENT_INTEREST
      REAL*4 VECTOR_DATA(AVAIL_DATA_YEARS)
      LOGICAL*1 AFDCON,USED_ALL_VECTORS
      INTEGER*2 SAVE_NYRS
      CHARACTER*20 VECTOR_TYPE
      LOGICAL*1 USE_AFUDC_VECTOR_AS_RATES 
      REAL*4 SUM_OF_MONTHLY_AFUDC_ON_CASH
C
      CHARACTER*1 MONTHLY_DATA_UNITS(LAST_AVAILABLE_MONTHLY_YEAR)
      REAL*4 MONTHLY_VECTOR_DATA(12,LAST_AVAILABLE_MONTHLY_YEAR)
      INTEGER*2 MONTH_ENDING(LAST_AVAILABLE_MONTHLY_YEAR)
C     DECLARATION FOR WORLD VARIABLES FOR FUTURE ASSETS
      CHARACTER*2 DATDRIVE
      INTEGER*2 BASEYR,NYRS
      COMMON /WORLD/BASEYR,NYRS
      COMMON /DATA_DRIVE_LOCATION/ DATDRIVE
C
C     INITIALIZE VARIABLES
C
      
C
C  REPLACE WITH ARRAY ASSIGNMENT IN LF95
C
      AFDC1(1:FINANCIAL_SIMULATION_YEARS) = 0.
      AFDC2(1:FINANCIAL_SIMULATION_YEARS) = 0.
      CWIP(1:FINANCIAL_SIMULATION_YEARS) = 0.
      RBCWIP(1:FINANCIAL_SIMULATION_YEARS) = 0.
      AFDC1B(1:FINANCIAL_SIMULATION_YEARS) = 0.
      AFDC2B(1:FINANCIAL_SIMULATION_YEARS) = 0.
      CAPINRST(1:FINANCIAL_SIMULATION_YEARS) = 0.
      PCAPINRST(1:FINANCIAL_SIMULATION_YEARS) = 0.
      CURRENT_INTEREST_CAP(1:FINANCIAL_SIMULATION_YEARS) = 0.
C
      AFDC1(1) = AFUDC1
      AFUDC_IN_CWIP(1) = AFUDC1
      CWIP(1) = CWIP1
      RCWIP = CWIPRB / 100.
      RBCWIP(1) = CWIP1 * RCWIP
      
      MONTHLY_CWIP(12,0) = CWIP(1)
      MONTHLY_CWIP(0,1) = CWIP(1)
      MONTHLY_CWIP_IN_RATEBASE(12,0) = RBCWIP(1)
      MONTHLY_CWIP_IN_RATEBASE(0,1) = RBCWIP(1)
      MONTHLY_AFUDC_IN_CWIP(12,0) = AFUDC1
      MONTHLY_AFUDC_IN_CWIP(0,1) = AFUDC1
      DO YR = 1, FINANCIAL_SIMULATION_YEARS-1
         MONTHLY_CWIP(0,YR) = MONTHLY_CWIP(12,YR-1)
         MONTHLY_CWIP_IN_RATEBASE(0,YR) =
     +                                 MONTHLY_CWIP_IN_RATEBASE(12,YR-1)
         DO MO = 1, 12 
            MONTHLY_CWIP(MO,YR) = MONTHLY_CWIP(MO-1,YR) +
     +                            BOOK_EXPEN(MO,YR) -
     +                            PLANT_2_SERVICE(MO,YR)
            IF(ABS(MONTHLY_CWIP(MO,YR)) < .0001) MONTHLY_CWIP(MO,YR)=0.
            MONTHLY_CWIP_IN_RATEBASE(MO,YR) = RCWIP*MONTHLY_CWIP(MO,YR)
         ENDDO
      ENDDO
      LAST_CAP_YEAR = FINANCIAL_SIMULATION_YEARS-1
      IF(MONTHLY_CWIP(12,LAST_CAP_YEAR) /= 0.) THEN
         PLANT_2_SERVICE(12,LAST_CAP_YEAR) =
     +                                 PLANT_2_SERVICE(12,LAST_CAP_YEAR)
     +                                 + MONTHLY_CWIP(12,LAST_CAP_YEAR)
         PLANT_2_SERVICE(0,LAST_CAP_YEAR) =
     +                                  PLANT_2_SERVICE(0,LAST_CAP_YEAR)
     +                                  + MONTHLY_CWIP(12,LAST_CAP_YEAR)
         MONTHLY_CWIP(12,LAST_CAP_YEAR) = 0.
      ENDIF
C
C     CALCULATE YEAR END CWIP BALANCE AND  CWIP IN RATE BASE
C
      TOTCEP = 0.0
      LAST_CAP_YEAR = FINANCIAL_SIMULATION_YEARS + 1
      DO J = 2, FINANCIAL_SIMULATION_YEARS
         CWIP(J) = CWIP(J-1) + CE(J) - CEP(J)
         IF(ABS(CWIP(J)) < .001) CWIP(J) = 0.
         TOTCEP = TOTCEP + CEP(J)
         RBCWIP(J) = CWIP(J) * RCWIP
         IF(CEP(J) /= 0.) LAST_CAP_YEAR = J
         AFUDC_IN_CWIP(J) = AFUDC1
      ENDDO
      IF(CWIP(FINANCIAL_SIMULATION_YEARS) /= 0.) THEN
         CEP(FINANCIAL_SIMULATION_YEARS) =
     +                                CEP(FINANCIAL_SIMULATION_YEARS)
     +                                + CWIP(FINANCIAL_SIMULATION_YEARS) 
         TOTCEP = TOTCEP + CWIP(FINANCIAL_SIMULATION_YEARS) 
         CWIP(FINANCIAL_SIMULATION_YEARS) = 0.
      ENDIF
C
C     CALCULATE AFDC ON CONSTRUCTION AND AFUDC ENTERING SERVICE
C
	   IF(AFDCSW == 0 .OR. (AFDCFC == 0. .AND. AFDCSW == 1)) THEN
         IF(AFUDC1 /= 0. .AND. TOTCEP /= 0.) THEN
            AFDCSW = 0
            DO YR = 2, FINANCIAL_SIMULATION_YEARS 
               IF(CEP(YR) /= 0.) THEN
                  AFDC2(YR) = CEP(YR)/TOTCEP*AFUDC1
                  DO MO = 1, 12
                     IF(PLANT_2_SERVICE(MO,YR-1) /= 0.) THEN
                        MONTHLY_AFUDC_ON_PLANT(MO,YR-1) = AFUDC1 *
     +                                   PLANT_2_SERVICE(MO,YR-1)/TOTCEP
                     ENDIF
                  ENDDO
               ENDIF
            ENDDO
         ELSE
            RETURN ! There isn't any AFUDC Calculation
         ENDIF
      ENDIF
C
C  DETERMINE WHICH AFUDC RATE TO USE
C
      SAVE_NYRS = NYRS
      NYRS = FINANCIAL_SIMULATION_YEARS
      AFDCON = AFDCPERIODS >= 1
      USE_AFUDC_VECTOR_AS_RATES = .FALSE.
      IF(AFDCSW < 0) THEN                                   
         IVEC = ABS(AFDCSW)
         CALL GET_ASSET_VAR_TYPE(IVEC,DATA_TYPE,VECTOR_DATA,VECTOR_TYPE)
         USE_AFUDC_VECTOR_AS_RATES = INDEX(VECTOR_TYPE,'Rate') /= 0.
         IF(USE_AFUDC_VECTOR_AS_RATES) THEN
            AFDCSW = 2         
            DO J = 2, NYRS 
               IF(J <= AVAIL_DATA_YEARS) THEN
                  EFFAFDCRATE(J) = VECTOR_DATA(J-1)/100. 
               ELSE
                  EFFAFDCRATE(J) = VECTOR_DATA(AVAIL_DATA_YEARS)/100. 
               ENDIF
               IF(AFDCON) EFFAFDCRATE(J) = -1. + ((1. + EFFAFDCRATE(J)/
     +                                 FLOAT(AFDCPERIODS))**AFDCPERIODS)
            ENDDO
         ENDIF
      ENDIF
      IF(.NOT. USE_AFUDC_VECTOR_AS_RATES) THEN
         DO J = 2, NYRS
            IF(WHO_CALLED == 'NF') THEN
               EFFAFDCRATE(J) = AFUDC_NF_RATE(J)
            ELSE
               EFFAFDCRATE(J) = AFDCRT(J)
            ENDIF
            IF(AFDCON) EFFAFDCRATE(J) = -1. + ((1. + EFFAFDCRATE(J)/
     +                                 FLOAT(AFDCPERIODS))**AFDCPERIODS)
         ENDDO
      ENDIF
      IF(CWIPRB == 100.) THEN
         RCWIP = 1.
      ELSE
         RCWIP = 1. - RCWIP
      ENDIF
      
      TAFDC1 = AFUDC1
      TAFDC2 = 0.0
C ELSE THE USER PROVIDED THE INFORMATION
      IF(AFDCSW < 0 .AND. .NOT. USE_AFUDC_VECTOR_AS_RATES) THEN
         IOFFSET = MIN(FINANCIAL_SIMULATION_YEARS,
     +                 MAX(FIRSTYR - BASEYR,1))
         IVEC = ABS(AFDCSW)
         CALL GET_MONTHLY_ANNUAL_VALUES(IVEC,
     +                                  DATA_TYPE,
     +                                  VECTOR_TYPE,
     +                                  VECTOR_DATA,
     +                                  MONTHLY_VECTOR_DATA(1,1),
     +                                  MONTHLY_DATA_UNITS,
     +                                  MONTH_ENDING)
         CALL MOVE_ANNUAL_INPUT(MONTHLY_AFUDC_ON_CASH,
     +                          VECTOR_DATA,
     +                          IOFFSET,
     +                          FINANCIAL_SIMULATION_YEARS)
C         IF(MONTHLY_MIDAS_ACTIVE) THEN
C
            CALL RIPPLE_MOVE_MONTHLY_DATA(MONTHLY_AFUDC_ON_CASH,
     +                                    MONTHLY_VECTOR_DATA,
     +                                    IOFFSET,
     +                                    FINANCIAL_SIMULATION_YEARS,
     +                                    RIPPLE_AVERAGE)
            CALL TREND_MONTHLY_INPUT(MONTHLY_AFUDC_ON_CASH,
     +                               MONTHLY_DATA_UNITS,
     +                               MONTH_ENDING,
     +                               IOFFSET,
     +                               SERVICEMO,
     +                               FINANCIAL_SIMULATION_YEARS)
C         ENDIF
         DO YR = 2, NYRS
            AFDC1(YR) = MONTHLY_AFUDC_ON_CASH(0,YR-1)
            TAFDC1  = TAFDC1 + AFDC1(YR)
         ENDDO
C         I = 1
C        CALL GET_ASSET_VAR(IVEC,DATA_TYPE,VECTOR_DATA)
C        DO WHILE ((I+IOFFSET) <= FINANCIAL_SIMULATION_YEARS .AND.
C    +                                          (I <= AVAIL_DATA_YEARS))                
C           AFDC1(I+IOFFSET) = VECTOR_DATA(I)
C           TAFDC1  = TAFDC1 + VECTOR_DATA(I)
C           I = I + 1
C        ENDDO
      ELSEIF(AFDCSW == 1 .OR. AFDCSW == 2) THEN
C
C MONTHLY AFUDC
C
         OUTSERVICE = FLOAT(SERVICEMO-1)/12.
         INSERVICE = 1. - OUTSERVICE
         DO YR = 2, NYRS
            MONTHLY_AFUDC_RATE = RCWIP * EFFAFDCRATE(YR)/12.
            DO MO = 1, 12
               MONTHLY_AFUDC_ON_CASH(MO,YR-1) = MONTHLY_AFUDC_RATE *
     +              (MONTHLY_CWIP(MO-1,YR-1) + MONTHLY_CWIP(MO,YR-1))/2.
               MONTHLY_AFUDC_ON_CASH(MO,YR-1) = MONTHLY_AFUDC_RATE *
     +                                             MONTHLY_CWIP(MO,YR-1)
               MONTHLY_AFUDC_ON_CASH(MO,YR-1) = MONTHLY_AFUDC_RATE *
     +                (MONTHLY_CWIP(MO-1,YR-1) + BOOK_EXPEN(MO,YR-1)/2.)
            ENDDO
            SUM_OF_MONTHLY_AFUDC_ON_CASH =
     +                             SUM(MONTHLY_AFUDC_ON_CASH(1:12,YR-1))
            ANNUAL_MONTHLY_AFDC1(YR) = SUM_OF_MONTHLY_AFUDC_ON_CASH
            MONTHLY_AFUDC_ON_CASH(0,YR-1) = SUM_OF_MONTHLY_AFUDC_ON_CASH
         ENDDO
C
C 
C
         DO YR = 2, NYRS
            IF(AFDCSW == 2) THEN
               IF(CE(YR) == CEP(YR)) THEN
                  AFDC1(YR) = RCWIP * EFFAFDCRATE(YR) * 
     +                       (OUTSERVICE*CE(YR)/2.+CWIP(YR-1))
               ELSEIF(CEP(YR) == 0.) THEN
                  AFDC1(YR) = RCWIP * EFFAFDCRATE(YR) * (CE(YR)/2. +
     +                                                       CWIP(YR-1))
               ELSE
                  IF(CWIP(YR-1)+OUTSERVICE*CE(YR) >= CEP(YR)) THEN
                     AFDC1(YR) = RCWIP * EFFAFDCRATE(YR) * (CE(YR)/2. +
     +                          OUTSERVICE*CWIP(YR-1) + 
     +                          INSERVICE*(MAX((CWIP(YR-1)-CEP(YR)),0.)-
     +                          MAX((CEP(YR)-CWIP(YR-1)),0.)/2.))
                  ELSE
                     AFDC1(YR) = RCWIP * EFFAFDCRATE(YR) * (OUTSERVICE* 
     +                       (CWIP(YR-1) + (CEP(YR) - CWIP(YR-1))/2.) +
     +                  INSERVICE*((CE(YR) - (CEP(YR) - CWIP(YR-1)))/2.+
     +                          CWIP(YR)))
                  ENDIF
               ENDIF
            ELSEIF(AFDCSW == 1) THEN
               AFDC1(YR) = RCWIP*EFFAFDCRATE(YR)*(CE(YR)+CWIP(YR-1))
            ENDIF
c            IF(YR<=LAST_AVAILABLE_MONTHLY_YEAR+1 .AND. AFDCSW /= 1) THEN 
               AFDC1(YR) = ANNUAL_MONTHLY_AFDC1(YR)
c            ENDIF
            TAFDC1  = TAFDC1 + AFDC1(YR)
         ENDDO
c$ifdefined(use_old_annual_afudc_code)
c        DO J = 2, NYRS
C
C  CALCULATE AFDC1
C
c        OUTSERVICE = FLOAT(SERVICEMO-1)/12.
c        INSERVICE = 1. - OUTSERVICE
c           IF(AFDCSW == 2) THEN
c              IF(CE(J) == CEP(J)) THEN
c                 AFDC1(J) = RCWIP * EFFAFDCRATE(J) * 
c    +                       (OUTSERVICE*CE(J)/2.+CWIP(J-1))
c              ELSE
c                 AFDC1(J) = RCWIP * EFFAFDCRATE(J) * (CE(J)/2. +
c    +                         OUTSERVICE*CWIP(J-1) + 
c    +                         INSERVICE*(MAX((CWIP(J-1)-CEP(J)),0.) -
c    +                         MAX((CEP(J)-CWIP(J-1)),0.)/2.))
c              ENDIF
c           ELSE IF(AFDCSW == 1) THEN
c              AFDC1(J) = RCWIP*EFFAFDCRATE(J)*(CE(J) + CWIP(J-1))
c           ENDIF
c           TAFDC1  = TAFDC1 + AFDC1(J)
c        ENDDO
c$endif
         IF(CWIP(NYRS) /= 0.) THEN
            IF(AFDCSW == 2) THEN
               TAFDC1  = TAFDC1  + 
     +               RCWIP * EFFAFDCRATE(NYRS) * CWIP(NYRS) * OUTSERVICE
            ELSE
               TAFDC1  = TAFDC1 + RCWIP * EFFAFDCRATE(NYRS) * CWIP(NYRS)
            ENDIF
         ENDIF
      ENDIF
      IF(AFDC_CAP_VECTOR /= 0) THEN
         IOFFSET = MIN(FINANCIAL_SIMULATION_YEARS,
     +                 MAX(FIRSTYR - BASEYR,1))
         IVEC = ABS(AFDC_CAP_VECTOR)
         CALL GET_MONTHLY_ANNUAL_VALUES(IVEC,
     +                                  DATA_TYPE,
     +                                  VECTOR_TYPE,
     +                                  VECTOR_DATA,
     +                                  MONTHLY_VECTOR_DATA(1,1),
     +                                  MONTHLY_DATA_UNITS,
     +                                  MONTH_ENDING)
         CALL MOVE_ANNUAL_INPUT(MONTHLY_AFUDC_ON_PLANT,
     +                          VECTOR_DATA,
     +                          IOFFSET,
     +                          FINANCIAL_SIMULATION_YEARS)
C
         CALL RIPPLE_MOVE_MONTHLY_DATA(MONTHLY_AFUDC_ON_PLANT,
     +                                    MONTHLY_VECTOR_DATA,
     +                                    IOFFSET,
     +                                    FINANCIAL_SIMULATION_YEARS,
     +                                    RIPPLE_AVERAGE)
         CALL TREND_MONTHLY_INPUT(MONTHLY_AFUDC_ON_PLANT,
     +                               MONTHLY_DATA_UNITS,
     +                               MONTH_ENDING,
     +                               IOFFSET,
     +                               SERVICEMO,
     +                               FINANCIAL_SIMULATION_YEARS)
         DO YR = 2, NYRS
            
            AFDC2(YR) = MONTHLY_AFUDC_ON_PLANT(0,YR-1)
            TAFDC2  = TAFDC2 + AFDC2(YR)
         ENDDO
C        IVEC = ABS(AFDC_CAP_VECTOR)
C        CALL GET_ASSET_VAR(IVEC,DATA_TYPE,VECTOR_DATA)
C        IOFFSET = MIN(FINANCIAL_SIMULATION_YEARS,
C    +                 MAX(FIRSTYR - BASEYR,1))
C        I = 1
C        DO WHILE ((I+IOFFSET) <= FINANCIAL_SIMULATION_YEARS .AND.
C    +                                          (I <= AVAIL_DATA_YEARS))                
C           AFDC2(I+IOFFSET) = VECTOR_DATA(I)
C           TAFDC2  = TAFDC2 + VECTOR_DATA(I)
C           I = I + 1
C        ENDDO
      ELSEIF(AFDCSW /= 0) THEN
C
C  CALCULATE AFDC2
C
         DO J = 2, NYRS
            IF(AFDCSW == 1) THEN
               AFDC2(J) = RCWIP * AFDCFC * EFFAFDCRATE(J) * CEP(J)
            ELSE
               AFDC2(J) = RCWIP * EFFAFDCRATE(J) * CEP(J)
            ENDIF
            TAFDC2 = TAFDC2 + AFDC2(J)
         ENDDO
         IF(CWIP(NYRS) /= 0.) THEN
            IF(AFDCSW == 1) THEN
               TAFDC2  = TAFDC2  + 
     +               RCWIP * EFFAFDCRATE(NYRS) * CWIP(NYRS) * AFDCFC
            ELSE
               TAFDC2 = TAFDC2 + RCWIP * EFFAFDCRATE(NYRS) * CWIP(NYRS)
            ENDIF
         ENDIF
      ENDIF
C
C  BALANCE THE AFDC1 AND AFDC2
C
      USED_ALL_VECTORS = AFDCSW < 0. .AND. AFDC_CAP_VECTOR /= 0
      IF(.NOT. USED_ALL_VECTORS .AND. USE_PLANT_PERCENTAGE) THEN
         AFUDC_CASH_IN_CWIP = AFDC1(1)
         DO YR = 1, NYRS-1
            DO MO = 1, 12
               AFUDC_CASH_IN_CWIP  = AFUDC_CASH_IN_CWIP
     +                               + MONTHLY_AFUDC_ON_CASH(MO,YR)
               MONTHLY_AFUDC_ON_PLANT(MO,YR) = AFUDC_CASH_IN_CWIP *
     +                               PERCENT_PLANT_2_SERVICE(MO,YR)/100.
         
               AFUDC_CASH_IN_CWIP  = AFUDC_CASH_IN_CWIP
     +                               - MONTHLY_AFUDC_ON_PLANT(MO,YR)
            ENDDO
            AFDC2(YR+1) = SUM(MONTHLY_AFUDC_ON_PLANT(1:12,YR))
         ENDDO          
      ELSEIF(.NOT.USED_ALL_VECTORS .AND. (AFDCSW>0. .OR. AFDCFC>0.))THEN
         IF(AFDCSW < 0.) AFDCSW = 2
         IF(TOTCEP /= 0.0 .AND. AFDCSW /= 0) THEN
            IF(ABS(TAFDC1  - TAFDC2) > 0.0005 ) THEN
C           BALANCE THE AFUDC VALUES BASED ON SWITCH 1 OR 2
               IF(AFDCSW == 1) THEN
                  IF((TAFDC1 - AFDC1(1)) /= 0.) THEN
                     RATIO = (TAFDC2 - AFDC1(1)) / (TAFDC1 - AFDC1(1))
                     DO J = 2, NYRS
                        AFDC1(J) = AFDC1(J) * RATIO
                     ENDDO
                  ENDIF
               ELSE
                  IF(TAFDC2 /= 0.0) THEN
                     TOTAL_2_J = 0.
                     DO J = 2, NYRS
                        IF(AFDC2(J) /= 0.) THEN
                           TOTAL_2_J = TOTAL_2_J + AFDC2(J)
                           ADJUST_FACTOR(J) = MIN(TOTAL_2_J/TAFDC2,1.)
c                           LAST_CAP_YEAR = J
                        ENDIF
                     ENDDO
                     RATIO = TAFDC1/TAFDC2
                     AFDC1_BALANCE = AFDC1(1)
                     DO J = 2, LAST_CAP_YEAR-1
C                       AFDC2(J) = AFDC2(J) * RATIO
                        AFDC1_BALANCE = AFDC1_BALANCE + AFDC1(J)/2.
                        IF(AFDC2(J) /= 0.) THEN
                           AFDC2(J) = MIN(ADJUST_FACTOR(J)*TAFDC1,
     +                                                    AFDC1_BALANCE)
                        ENDIF
                        AFDC1_BALANCE = AFDC1_BALANCE + AFDC1(J)/2. -
     +                                                          AFDC2(J)
                     ENDDO
                     LAST_CAP_YEAR = MIN(LAST_CAP_YEAR,NYRS)
                     AFDC2(LAST_CAP_YEAR) = AFDC1_BALANCE +
     +                                              AFDC1(LAST_CAP_YEAR)
                  ENDIF
               ENDIF
            ENDIF
C
C  CALCULATE COMPOUNDING AMOUNTS
C
            IF(AFDCON .AND. AFDCSW > 0 .AND. .FALSE.) THEN !ADD FALSE 9/15/04 MAY CHANGE RESULTS
               AF1C = AFDC1(1)
               AF1CBAL = AFDC1(1)
               AF1BAL  = AF1CBAL
               OUTSERVICE = FLOAT(SERVICEMO-1)/12.
               INSERVICE = 1. - OUTSERVICE
               DO J = 2, NYRS
                  IF(AF1BAL /= 0.) THEN
                     RTEMP = AFDC2(J) * AF1CBAL/AF1BAL
                  ELSE
                     RTEMP = AFDC2(J)
                  ENDIF
                  AF1C = AFDC1(J) + EFFAFDCRATE(J) * (OUTSERVICE*AF1CBAL 
     +                   + INSERVICE*MAX((AF1CBAL - RTEMP),0.))
                  IF((AF1BAL + AFDC1(J)) /= 0.) THEN
                     AF2C = AFDC2(J) * (AF1CBAL + AF1C)/
     +                                 (AF1BAL + AFDC1(J))
                  ELSE
                     AF2C = AFDC2(J)
                  ENDIF
                  AF1CBAL = MAX((AF1CBAL + AF1C - AF2C),0.)
                  IF(AF1CBAL < .0001) AF1CBAL = 0.
                  AF1BAL  = MAX((AF1BAL + AFDC1(J) - AFDC2(J)),0.)
                  AFDC1(J) = AF1C
                  AFDC2(J) = AF2C
               ENDDO
            ENDIF
         ENDIF
C
         DO YR = 1, NYRS-1
            J = YR + 1
            MONTHLY_AFUDC_ON_PLANT(0,YR) = 0.
            IF(CEP(J) /= 0.) THEN
               DO MO = 1, 12
                  MONTHLY_AFUDC_ON_PLANT(MO,YR) = AFDC2(J)/CEP(J) *
     +                                            PLANT_2_SERVICE(MO,YR)
               ENDDO
               MONTHLY_AFUDC_ON_PLANT(0,YR) =
     +                              SUM(MONTHLY_AFUDC_ON_PLANT(1:12,YR))
            ENDIF
            MONTHLY_AFUDC_ON_CASH(0,YR) = AFDC1(J)
            IF(AFDC1(J) /= ANNUAL_MONTHLY_AFDC1(J)) THEN
               IF(ANNUAL_MONTHLY_AFDC1(J) /= 0.) THEN
                  MONTHLY_AFUDC_ON_CASH(0,YR) = 0.
                  DO MO = 1, 12
                     MONTHLY_AFUDC_ON_CASH(MO,YR) = AFDC1(J) *
     +                                   MONTHLY_AFUDC_ON_CASH(MO,YR)/
     +                                     ANNUAL_MONTHLY_AFDC1(J)
                  ENDDO
                  MONTHLY_AFUDC_ON_CASH(0,YR) =
     +                               SUM(MONTHLY_AFUDC_ON_CASH(1:12,YR))
               ENDIF
            ENDIF
         ENDDO
C
      ENDIF
C
C     CALCULATE THE BORROWED PORTION OF AFUDC
C     AND THE AMOUNT TO NORMALIZED IF REQUIRED
C
      AFDC1B(1) = AFDCB1
      AFC1BOR = AFDCB1
      AF1BAL  = AFDC1(1)
      DO J = 2, NYRS
         AFUDC_IN_CWIP(J) = AFUDC_IN_CWIP(J-1) + AFDC1(J) - AFDC2(J)
C         
         EFFBRAFDC(J) = AFDCBR(J)
         AFDC1B(J) = AFDC1(J)  * EFFBRAFDC(J)
         AFC1BOR   = AFC1BOR + AFDC1B(J) - AFDC2B(J-1)
         AF1BAL    = AF1BAL  + AFDC1(J)  - AFDC2(J-1)
         AFDC2B(J) = 0.0
         IF(((DEPMET /= 'ACRS' .AND.
     +             AFDCFC >= .5)) .AND. AF1BAL /= 0.) AFDC2B(J) = 
     +                                         AFDC2(J) * AFC1BOR/AF1BAL
      ENDDO
C
C FOR MIDAS GOLD ADDED MAY 25, 1991
C COPYRIGHT (C) M.S. GERBER & ASSOCIATES, INC
C ALL RIGHTS RESERVED
C
      IF(DEPMET /= 'ACRS' .AND.
     +                  (AFDCFC >= .5 .OR. CLASS_TAX_LIFE >=20.)) THEN
         TOTAL_INTEREST_CAP = AFDC1(1) * INTEREST_CAP_RATE(2)
         TOTAL_CURRENT_INTEREST = TOTAL_INTEREST_CAP *
     +                                 (1.-CURRENT_INTEREST_CAP_RATE(1))
         MONTHLY_CAPITALIZED_INTEREST(12,0) = TOTAL_INTEREST_CAP
         MONTHLY_CURRENT_INTEREST(12,0) = TOTAL_CURRENT_INTEREST
         BORAFDC = AFDC1(1)
         CAPINRST(1) = TOTAL_INTEREST_CAP
         CURRENT_INTEREST_CAP(1) = TOTAL_CURRENT_INTEREST
C                                             
         DO YR = 2, NYRS
            PCAPINRST(YR) = AFDC1(YR) * INTEREST_CAP_RATE(YR)
            TOTAL_INTEREST_CAP = TOTAL_INTEREST_CAP + PCAPINRST(YR)
            TOTAL_CURRENT_INTEREST = TOTAL_CURRENT_INTEREST
     +              + PCAPINRST(YR) * (1.-CURRENT_INTEREST_CAP_RATE(YR))
            BORAFDC = BORAFDC + AFDC1(YR)
            IF(BORAFDC /= 0.) THEN
               CAPINRST(YR) = TOTAL_INTEREST_CAP/BORAFDC * AFDC2(YR) 
               CURRENT_INTEREST_CAP(YR) = TOTAL_CURRENT_INTEREST/
     +                                         BORAFDC * AFDC2(YR) 
               TOTAL_INTEREST_CAP = MAX(0.,
     +                                TOTAL_INTEREST_CAP-CAPINRST(YR))
               TOTAL_CURRENT_INTEREST = MAX(0.,
     +                  TOTAL_CURRENT_INTEREST-CURRENT_INTEREST_CAP(YR))
               BORAFDC = MAX(0.,BORAFDC-AFDC2(YR))
            ENDIF
            DO MO = 0, 12 
               IF(AFDC2(YR) /= 0.) THEN
                  MONTHLY_INTEREST_TO_TAX_VALUE(MO,YR-1) = 
     +                                   MONTHLY_AFUDC_ON_PLANT(MO,YR-1)
     +                                   * CAPINRST(YR)/AFDC2(YR)
                  MONTHLY_CURRENT_INTEREST_CAP(MO,YR-1) = 
     +                              MONTHLY_AFUDC_ON_PLANT(MO,YR-1)
     +                              * CURRENT_INTEREST_CAP(YR)/AFDC2(YR)
               ENDIF 
               MONTHLY_CAPITALIZED_INTEREST(MO,YR-1) = 
     +                                  INTEREST_CAP_RATE(YR)
     +                                  * MONTHLY_AFUDC_ON_CASH(MO,YR-1)
               MONTHLY_CURRENT_INTEREST(MO,YR-1) = 
     +                           (1.-CURRENT_INTEREST_CAP_RATE(YR))
     +                           * MONTHLY_CAPITALIZED_INTEREST(MO,YR-1)
            ENDDO
         ENDDO
      ENDIF
C
C CALCULATE AFUDC IN CWIP
C      
      IF(CWIPRB == 100.) THEN
         RCWIP = 0. ! 1. - CWIPRB / 100.
         DO YR = 2, FINANCIAL_SIMULATION_YEARS
            AFDC1(YR) = RCWIP * AFDC1(YR)
            AFDC2(YR) = RCWIP * AFDC2(YR)
            AFDC1B(YR) = RCWIP * AFDC1B(YR)
            AFDC2B(YR) = RCWIP * AFDC2B(YR)
            AFUDC_IN_CWIP(YR) = RCWIP * AFUDC_IN_CWIP(YR)
         ENDDO
         DO YR = 1, NYRS-1
            DO MO = 0, 12
               MONTHLY_AFUDC_ON_CASH(MO,YR) = RCWIP
     +                                    * MONTHLY_AFUDC_ON_CASH(MO,YR)
               MONTHLY_AFUDC_ON_PLANT(MO,YR) = RCWIP
     +                                   * MONTHLY_AFUDC_ON_PLANT(MO,YR)
            ENDDO
         ENDDO
      ENDIF      
C
C CALCULATE AFUDC IN CWIP
C      
      DO YR = 1, NYRS-1
         DO MO = 1, 12
            MONTHLY_AFUDC_IN_CWIP(MO,YR) =
     +                                   MONTHLY_AFUDC_IN_CWIP(MO-1,YR)
     +                                   + MONTHLY_AFUDC_ON_CASH(MO,YR)
     +                                   - MONTHLY_AFUDC_ON_PLANT(MO,YR)
            IF(ABS(MONTHLY_AFUDC_IN_CWIP(MO,YR)) < .00001)  
     +                                MONTHLY_AFUDC_IN_CWIP(MO,YR) = 0.0  
         ENDDO
         MONTHLY_AFUDC_IN_CWIP(0,YR+1) = MONTHLY_AFUDC_IN_CWIP(12,YR)
      ENDDO      
c
      NYRS = SAVE_NYRS
      RETURN
      END
C***********************************************************************
C*                                                                     *
C*                              A F U D C                              *
C*                                                                     *
C*          COPYRIGHT (C) 1982,85,86 M.S. GERBER & ASSOCIATES, INC     *
C*                         ALL RIGHTS RESERVED                         *
C*  THIS CODE IS A DERIVATIVE OF CODE DEVELOPED BY M.S.G. OF THE       *
C*  FIN MODEL AND IS COPYRIGHTED BY M.S. GERBER & ASSOCIATES, INC      *
C***********************************************************************
C                                                                      *
C     PURPOSE:                                                         *
C        AFUDC CALCULATES:                                             *
C           ANNUAL YEAR END CWIP BALANCE                     CWIP      *
C           ANNUAL AFUDC ON CONSTRUCTION EXPENSES            AFDC1     *
C           ANNUAL afudc_on_plant ENTERING SERVICE           AFDC2     *
C           ANNUAL AMOUNT OF PLANT ENTERING SERVICE          CEP       *
C           ANNUAL CASH EXPENDITURES                         CE        *
C           ANNUAL CWIP IN RATE BASE                         RBCWIP    *
C                                                                      *
C***********************************************************************
C
      SUBROUTINE AFUDC(AFUDC1,CWIPRB,AFDCB1,AFDCFC,CWIP1,
     +   AFDCSW,AFDCPERIODS,CE,CEP,AFDC1,AFDC2,CWIP,RBCWIP,
     +   AFDC1B,AFDC2B,
     +   SERVICEMO,CAPINRST,DEPMET,PCAPINRST,AFDC_CAP_VECTOR,
     +   FIRSTYR,CURRENT_INTEREST_CAP,WHO_CALLED,
     +   FINANCIAL_SIMULATION_YEARS,CLASS_TAX_LIFE,AFUDC_IN_CWIP)
C
      INCLUDE 'SpinLib.MON'
      INCLUDE 'SIZECOM.MON'
C
      CHARACTER*2 WHO_CALLED
      INTEGER*2 FINANCIAL_SIMULATION_YEARS,YR
      REAL*4 CLASS_TAX_LIFE,
     +     AFUDC_IN_CWIP(MAX_FINANCIAL_SIMULATION_YEARS)
      REAL*4 AFDCRT,AFDCBR,INTEREST_CAP_RATE,CURRENT_INTEREST_CAP_RATE,
     +     PROPERTY_ESCALATION,
     +     AFUDC_NF_RATE
      COMMON/AFUDC_STUFF/ AFDCRT(MAX_FINANCIAL_SIMULATION_YEARS),
     +        AFDCBR(MAX_FINANCIAL_SIMULATION_YEARS),
     +        INTEREST_CAP_RATE(MAX_FINANCIAL_SIMULATION_YEARS),
     +        PROPERTY_ESCALATION(MAX_FINANCIAL_SIMULATION_YEARS),
     +        CURRENT_INTEREST_CAP_RATE(MAX_FINANCIAL_SIMULATION_YEARS),
     +        AFUDC_NF_RATE(MAX_FINANCIAL_SIMULATION_YEARS)
C
      INTEGER*2 LAST_CAP_YEAR
      REAL*4 TOTAL_2_J,AFDC1_BALANCE,
     +       ADJUST_FACTOR(MAX_FINANCIAL_SIMULATION_YEARS)
      INTEGER*2 I,J,IVEC,AFDCSW,AFDC_CAP_VECTOR
      INTEGER*2 AFDCPERIODS,SERVICEMO
      INTEGER*2 IOFFSET,FIRSTYR
      CHARACTER*4 DEPMET
      CHARACTER*1 DATA_TYPE
      REAL*4 OUTSERVICE,INSERVICE,AF1C,AF1CBAL,AF1BAL,RTEMP,AF2C
      REAL*4 CWIP1,RCWIP,CWIPRB,AFUDC1,AFDCFC,AFDCB1,TOTCEP,
     +     TAFDC1,TAFDC2,RATIO,BORAFDC,
     +     CE(*),CEP(*),AFDC1(*),AFDC2(*),CWIP(*),RBCWIP(*),
     +     AFDC1B(*),AFDC2B(*),
     +     CAPINRST(*),PCAPINRST(*),AFC1BOR,CURRENT_INTEREST_CAP(*)
      REAL*4 EFFAFDCRATE(MAX_FINANCIAL_SIMULATION_YEARS),
     +     EFFBRAFDC(MAX_FINANCIAL_SIMULATION_YEARS)
      REAL*4 TOTAL_INTEREST_CAP,
     +     TOTAL_CURRENT_INTEREST
      REAL*4 VECTOR_DATA(AVAIL_DATA_YEARS)
      LOGICAL*1 AFDCON,USED_ALL_VECTORS
      INTEGER*2 SAVE_NYRS
      CHARACTER*20 VECTOR_TYPE
      LOGICAL*1 USE_AFUDC_VECTOR_AS_RATES 
C     DECLARATION FOR WORLD VARIABLES FOR FUTURE ASSETS
      CHARACTER*2 DATDRIVE
      INTEGER*2 BASEYR,NYRS
      COMMON /WORLD/BASEYR,NYRS
      COMMON /DATA_DRIVE_LOCATION/ DATDRIVE
C
C     INITIALIZE VARIABLES
C
      DO YR = 1, FINANCIAL_SIMULATION_YEARS
         AFDC1(YR) = 0.
         AFDC2(YR) = 0.
         CWIP(YR) = 0.
         RBCWIP(YR) = 0.
         AFDC1B(YR) = 0.
         AFDC2B(YR) = 0.
         CAPINRST(YR) = 0.
         PCAPINRST(YR) = 0.
         CURRENT_INTEREST_CAP(YR) = 0.
      ENDDO
C
      AFDC1(1) = AFUDC1
      AFUDC_IN_CWIP(1) = AFUDC1
      CWIP(1) = CWIP1
      RCWIP = CWIPRB / 100.
      RBCWIP(1) = CWIP1 * RCWIP
C
C     CALCULATE YEAR END CWIP BALANCE AND  CWIP IN RATE BASE
C
      TOTCEP = 0.0
      LAST_CAP_YEAR = FINANCIAL_SIMULATION_YEARS + 1
      DO J = 2, FINANCIAL_SIMULATION_YEARS
         CWIP(J) = CWIP(J-1) + CE(J) - CEP(J)
         IF(ABS(CWIP(J)) < .00001) CWIP(J) = 0.
         TOTCEP = TOTCEP + CEP(J)
         RBCWIP(J) = CWIP(J) * RCWIP
         IF(CEP(J) /= 0.) LAST_CAP_YEAR = J
         AFUDC_IN_CWIP(J) = AFUDC1
      ENDDO
      IF(CWIP(FINANCIAL_SIMULATION_YEARS) /= 0.) THEN
         IF(LAST_CAP_YEAR > AVAIL_DATA_YEARS) THEN
            J = MIN(LAST_CAP_YEAR+1,FINANCIAL_SIMULATION_YEARS)
         ELSE
            J = MIN(AVAIL_DATA_YEARS+1,FINANCIAL_SIMULATION_YEARS)
         ENDIF
         CEP(J) = CEP(J) + CWIP(FINANCIAL_SIMULATION_YEARS) 
         TOTCEP = TOTCEP + CWIP(FINANCIAL_SIMULATION_YEARS) 
         DO I = J, FINANCIAL_SIMULATION_YEARS
            CWIP(I) = 0.
         ENDDO
      ENDIF
C
C     CALCULATE AFDC ON CONSTRUCTION AND AFUDC ENTERING SERVICE
C
	IF(AFDCSW == 0 .OR. (AFDCFC == 0. .AND. AFDCSW == 1)) THEN
         IF(AFUDC1 /= 0. .AND. TOTCEP /= 0.) THEN
            AFDCSW = 0
            DO J = 2, FINANCIAL_SIMULATION_YEARS 
               IF(CEP(J) /= 0.) THEN
                  AFDC2(J) = CEP(J)/TOTCEP*AFUDC1
               ENDIF
            ENDDO
         ELSE
            RETURN
         ENDIF
      ENDIF
C
C  DETERMINE WHICH AFUDC RATE TO USE
C
      SAVE_NYRS = NYRS
      NYRS = FINANCIAL_SIMULATION_YEARS
      AFDCON = AFDCPERIODS >= 1
      USE_AFUDC_VECTOR_AS_RATES = .FALSE.
      IF(AFDCSW < 0) THEN
         IVEC = ABS(AFDCSW)
         CALL GET_ASSET_VAR_TYPE(IVEC,DATA_TYPE,VECTOR_DATA,VECTOR_TYPE)
         USE_AFUDC_VECTOR_AS_RATES = INDEX(VECTOR_TYPE,'Rate') /= 0.
         IF(USE_AFUDC_VECTOR_AS_RATES) THEN
            AFDCSW = 2         
            DO J = 2, NYRS 
               IF(J <= AVAIL_DATA_YEARS) THEN
                  EFFAFDCRATE(J) = VECTOR_DATA(J-1)/100. 
               ELSE
                  EFFAFDCRATE(J) = VECTOR_DATA(AVAIL_DATA_YEARS)/100. 
               ENDIF
               IF(AFDCON) EFFAFDCRATE(J) = -1. + ((1. + EFFAFDCRATE(J)/
     +                                 FLOAT(AFDCPERIODS))**AFDCPERIODS)
            ENDDO
         ENDIF
      ENDIF
      IF(.NOT. USE_AFUDC_VECTOR_AS_RATES) THEN
         DO J = 2, NYRS
            IF(WHO_CALLED == 'NF') THEN
               EFFAFDCRATE(J) = AFUDC_NF_RATE(J)
            ELSE
               EFFAFDCRATE(J) = AFDCRT(J)
            ENDIF
            IF(AFDCON) EFFAFDCRATE(J) = -1. + ((1. + EFFAFDCRATE(J)/
     +                                 FLOAT(AFDCPERIODS))**AFDCPERIODS)
         ENDDO
      ENDIF
      RCWIP = 1. - RCWIP
      TAFDC1 = AFUDC1
      TAFDC2 = 0.0
C     ELSE THE USER PROVIDED THE INFORMATION
      IF(AFDCSW < 0 .AND. .NOT. USE_AFUDC_VECTOR_AS_RATES) THEN
         IVEC = ABS(AFDCSW)
         CALL GET_ASSET_VAR(IVEC,DATA_TYPE,VECTOR_DATA)
         IOFFSET = MIN(FINANCIAL_SIMULATION_YEARS,
     +                 MAX(FIRSTYR - BASEYR,1))
         I = 1
         DO WHILE ((I+IOFFSET) <= FINANCIAL_SIMULATION_YEARS .AND.
     +                                          (I <= AVAIL_DATA_YEARS))                
            AFDC1(I+IOFFSET) = VECTOR_DATA(I)
            TAFDC1  = TAFDC1 + VECTOR_DATA(I)
            I = I + 1
         ENDDO
      ELSE IF(AFDCSW == 1 .OR. AFDCSW == 2) THEN
         OUTSERVICE = FLOAT(SERVICEMO-1)/12.
         INSERVICE = 1. - OUTSERVICE
         DO J = 2, NYRS
C
C  CALCULATE AFDC1
C
            IF(AFDCSW == 2) THEN
               IF(CE(J) == CEP(J)) THEN
                  AFDC1(J) = RCWIP * EFFAFDCRATE(J) * 
     +                       (OUTSERVICE*CE(J)/2.+CWIP(J-1))
               ELSE
                  AFDC1(J) = RCWIP * EFFAFDCRATE(J) * (CE(J)/2. +
     +                         OUTSERVICE*CWIP(J-1) + 
     +                         INSERVICE*(MAX((CWIP(J-1)-CEP(J)),0.) -
     +                         MAX((CEP(J)-CWIP(J-1)),0.)/2.))
               ENDIF
               IF(CE(J) == CEP(J)) THEN
                  AFDC1(J) = RCWIP * EFFAFDCRATE(J) * 
     +                       (OUTSERVICE*CE(J)/2.+CWIP(J-1))
               ELSEIF(CEP(J) == 0.) THEN
                  AFDC1(J) = RCWIP * EFFAFDCRATE(J) * (CE(J)/2. +
     +                                                       CWIP(J-1))
               ELSE
                  IF(CWIP(J-1)+OUTSERVICE*CE(J) >= CEP(J)) THEN
                     AFDC1(J) = RCWIP * EFFAFDCRATE(J) * (CE(J)/2. +
     +                          OUTSERVICE*CWIP(J-1) + 
     +                          INSERVICE*(MAX((CWIP(J-1)-CEP(J)),0.)-
     +                          MAX((CEP(J)-CWIP(J-1)),0.)/2.))
                  ELSE
                     AFDC1(J) = RCWIP * EFFAFDCRATE(J) * (OUTSERVICE* 
     +                       (CWIP(J-1) + (CEP(J) - CWIP(J-1))/2.) +
     +                  INSERVICE*((CE(J) - (CEP(J) - CWIP(J-1)))/2.+
     +                          CWIP(J)))
                  ENDIF
               ENDIF
            ELSE IF(AFDCSW == 1) THEN
               AFDC1(J) = RCWIP*EFFAFDCRATE(J)*(CE(J) + CWIP(J-1))
            ENDIF
            TAFDC1  = TAFDC1 + AFDC1(J)
         ENDDO
         IF(CWIP(NYRS) /= 0.) THEN
            IF(AFDCSW == 2) THEN
               TAFDC1  = TAFDC1  + 
     +               RCWIP * EFFAFDCRATE(NYRS) * CWIP(NYRS) * OUTSERVICE
            ELSE
               TAFDC1  = TAFDC1 + RCWIP * EFFAFDCRATE(NYRS) * CWIP(NYRS)
            ENDIF
         ENDIF
      ENDIF
      IF(AFDC_CAP_VECTOR /= 0) THEN
         IVEC = ABS(AFDC_CAP_VECTOR)
         CALL GET_ASSET_VAR(IVEC,DATA_TYPE,VECTOR_DATA)
         IOFFSET = MIN(FINANCIAL_SIMULATION_YEARS,
     +                 MAX(FIRSTYR - BASEYR,1))
         I = 1
         DO WHILE ((I+IOFFSET) <= FINANCIAL_SIMULATION_YEARS .AND.
     +                                          (I <= AVAIL_DATA_YEARS))                
            AFDC2(I+IOFFSET) = VECTOR_DATA(I)
            TAFDC2  = TAFDC2 + VECTOR_DATA(I)
            I = I + 1
         ENDDO
      ELSEIF(AFDCSW /= 0) THEN
C
C  CALCULATE AFDC2
C
         DO J = 2, NYRS
            IF(AFDCSW == 1) THEN
               AFDC2(J) = RCWIP * AFDCFC * EFFAFDCRATE(J) * CEP(J)
            ELSE
               AFDC2(J) = RCWIP * EFFAFDCRATE(J) * CEP(J)
            ENDIF
            TAFDC2 = TAFDC2 + AFDC2(J)
         ENDDO
         IF(CWIP(NYRS) /= 0.) THEN
            IF(AFDCSW == 1) THEN
               TAFDC2  = TAFDC2  + 
     +               RCWIP * EFFAFDCRATE(NYRS) * CWIP(NYRS) * AFDCFC
            ELSE
               TAFDC2 = TAFDC2 + RCWIP * EFFAFDCRATE(NYRS) * CWIP(NYRS)
            ENDIF
         ENDIF
      ENDIF
C
C  BALANCE THE AFDC1 AND AFDC2
C
      USED_ALL_VECTORS = AFDCSW < 0. .AND. AFDC_CAP_VECTOR /= 0
      IF(.NOT. USED_ALL_VECTORS .AND. (AFDCSW>0. .OR. AFDCFC>0.)) THEN
         IF(AFDCSW < 0.) AFDCSW = 2
         IF(TOTCEP /= 0.0 .AND. AFDCSW /= 0) THEN
            IF(ABS(TAFDC1  - TAFDC2) > 0.0005 ) THEN
C           BALANCE THE AFUDC VALUES BASED ON SWITCH 1 OR 2
               IF(AFDCSW == 1) THEN
                  IF((TAFDC1 - AFDC1(1)) /= 0.) THEN
                     RATIO = (TAFDC2 - AFDC1(1)) / (TAFDC1 - AFDC1(1))
                     DO J = 2, NYRS
                        AFDC1(J) = AFDC1(J) * RATIO
                     ENDDO
                  ENDIF
               ELSE
                  IF(TAFDC2 /= 0.0) THEN
                     TOTAL_2_J = 0.
                     DO J = 2, NYRS
                        IF(AFDC2(J) /= 0.) THEN
                           TOTAL_2_J = TOTAL_2_J + AFDC2(J)
                           ADJUST_FACTOR(J) = MIN(TOTAL_2_J/TAFDC2,1.)
c                           LAST_CAP_YEAR = J
                        ENDIF
                     ENDDO
                     RATIO = TAFDC1/TAFDC2
                     AFDC1_BALANCE = AFDC1(1)
                     DO J = 2, LAST_CAP_YEAR-1
C                       AFDC2(J) = AFDC2(J) * RATIO
                        AFDC1_BALANCE = AFDC1_BALANCE + AFDC1(J)/2.
                        IF(AFDC2(J) /= 0.) THEN
                           AFDC2(J) = MIN(ADJUST_FACTOR(J)*TAFDC1,
     +                                                    AFDC1_BALANCE)
                        ENDIF
                        AFDC1_BALANCE = AFDC1_BALANCE + AFDC1(J)/2. -
     +                                                          AFDC2(J)
                     ENDDO
                     AFDC2(LAST_CAP_YEAR) = AFDC1_BALANCE +
     +                                              AFDC1(LAST_CAP_YEAR)
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
C
C  CALCULATE COMPOUNDING AMOUNTS
C
         IF(AFDCON .AND. AFDCSW > 0) THEN
            AF1C = AFDC1(1)
            AF1CBAL = AFDC1(1)
            AF1BAL  = AF1CBAL
            OUTSERVICE = FLOAT(SERVICEMO-1)/12.
            INSERVICE = 1. - OUTSERVICE
            DO J = 2, NYRS
               IF(AF1BAL /= 0.) THEN
                  RTEMP = AFDC2(J) * AF1CBAL/AF1BAL
               ELSE
                  RTEMP = AFDC2(J)
               ENDIF
               AF1C = AFDC1(J) + EFFAFDCRATE(J) * (OUTSERVICE*AF1CBAL +
     +                           INSERVICE*MAX((AF1CBAL - RTEMP),0.))
               IF((AF1BAL + AFDC1(J)) /= 0.) THEN
                  AF2C = AFDC2(J) * (AF1CBAL + AF1C)/
     +                              (AF1BAL + AFDC1(J))
               ELSE
                  AF2C = AFDC2(J)
               ENDIF
               AF1CBAL = MAX((AF1CBAL + AF1C - AF2C),0.)
               IF(AF1CBAL < .0001) AF1CBAL = 0.
               AF1BAL  = MAX((AF1BAL + AFDC1(J) - AFDC2(J)),0.)
               AFDC1(J) = AF1C
               AFDC2(J) = AF2C
            ENDDO
         ENDIF
      ENDIF
C
C     CALCULATE THE BORROWED PORTION OF AFUDC
C     AND THE AMOUNT TO NORMALIZED IF REQUIRED
C
      AFDC1B(1) = AFDCB1
      AFC1BOR = AFDCB1
      AF1BAL  = AFDC1(1)
      DO J = 2, NYRS
         AFUDC_IN_CWIP(J) = AFUDC_IN_CWIP(J-1) + AFDC1(J) - AFDC2(J)
C         
         EFFBRAFDC(J) = AFDCBR(J)
         AFDC1B(J) = AFDC1(J)  * EFFBRAFDC(J)
         AFC1BOR   = AFC1BOR + AFDC1B(J) - AFDC2B(J-1)
         AF1BAL    = AF1BAL  + AFDC1(J)  - AFDC2(J-1)
         AFDC2B(J) = 0.0
         IF(((DEPMET /= 'ACRS' .AND.
     +             AFDCFC >= .5)) .AND. AF1BAL /= 0.) AFDC2B(J) = 
     +                                         AFDC2(J) * AFC1BOR/AF1BAL
      ENDDO
C
C FOR MIDAS GOLD ADDED MAY 25, 1991
C COPYRIGHT (C) M.S. GERBER & ASSOCIATES, INC
C ALL RIGHTS RESERVED
C
      IF(DEPMET /= 'ACRS' .AND.
     +                  (AFDCFC >= .5 .OR. CLASS_TAX_LIFE >=20.)) THEN
         IF(BASEYR > 1986) THEN
            TOTAL_INTEREST_CAP = AFDC1(1) * INTEREST_CAP_RATE(2)
            TOTAL_CURRENT_INTEREST = TOTAL_INTEREST_CAP *
     +                                      CURRENT_INTEREST_CAP_RATE(1)
            BORAFDC = AFDC1(1)
         ELSE
            TOTAL_INTEREST_CAP = 0.
            TOTAL_CURRENT_INTEREST = 0.
            BORAFDC = 0.
         ENDIF
         CAPINRST(1) = TOTAL_INTEREST_CAP
         CURRENT_INTEREST_CAP(1) = TOTAL_CURRENT_INTEREST
         DO J = 2, NYRS
            IF(BASEYR + J - 1 >= 1987) THEN
               PCAPINRST(J) = AFDC1(J) * INTEREST_CAP_RATE(J)
               TOTAL_INTEREST_CAP = TOTAL_INTEREST_CAP + PCAPINRST(J)
               TOTAL_CURRENT_INTEREST = TOTAL_CURRENT_INTEREST +
     +                       PCAPINRST(J) * CURRENT_INTEREST_CAP_RATE(J)
               BORAFDC = BORAFDC + AFDC1(J)
               IF(BORAFDC /= 0.) THEN
                  CAPINRST(J) = TOTAL_INTEREST_CAP/BORAFDC * AFDC2(J) 
                  CURRENT_INTEREST_CAP(J) = TOTAL_CURRENT_INTEREST/
     +                                         BORAFDC * AFDC2(J) 
                  TOTAL_INTEREST_CAP = MAX(0.,
     +                                TOTAL_INTEREST_CAP-CAPINRST(J))
                  TOTAL_CURRENT_INTEREST = MAX(0.,
     +                   TOTAL_CURRENT_INTEREST-CURRENT_INTEREST_CAP(J))
                  BORAFDC = MAX(0.,BORAFDC-AFDC2(J))
               ENDIF
            ENDIF
         ENDDO
      ENDIF
      NYRS = SAVE_NYRS
      RETURN
      END
C***********************************************************************
C*                                                                     *
C*                         W R I T E O F F                             *
C*                                                                     *
C*          COPYRIGHT (C) 1982, 1994 M.S. GERBER & ASSOCIATES, INC     *
C*                         ALL RIGHTS RESERVED                         *
C*                                                                     *
C***********************************************************************
C                                                                      *
      SUBROUTINE WRITE_OFF(ABYEAR,ABMETH,WOYRS,DDB,RBDDB,AFCEXP,AJAFDC,
     +                     EXEXP,AMORTE,BOKBL,BOKDAL,BOKWO,AFCBL,AFCDAL,
     +                     AFCWO,ABACCT,WODFTX,NRTXWO,NYRS)
C
      INCLUDE 'SpinLib.MON'
      INCLUDE 'SIZECOM.MON'
      INTEGER*2 NYRS
      INTEGER*2 J,T,ABYEAR,ABMETH,LASTYR
      CHARACTER*1 ABACCT
      REAL*4 DDB(NYRS),RBDDB(NYRS),AJAFDC(NYRS),EXEXP(NYRS),
     +       AMORTE(NYRS),
     +       AFCEXP(NYRS),WODFTX(NYRS),NRTXWO,BOKBL,BOKWO,BOKDAL,AMORT,
     +       AMTWO,AFCWO,AFCBL,AFCDAL,WOYRS
      INTEGER*2 BASE_YEAR
C
      LASTYR = ABYEAR - BASE_YEAR()
      IF(LASTYR >= AVAIL_DATA_YEARS+1 .OR. LASTYR < 1) THEN
         ABMETH = 0
         RETURN
      ENDIF
C
C     ADJUST THE TAX EXPENSE VECTOR
C
      LASTYR = LASTYR + 1
      DDB = 0.
      RBDDB = 0.
      AJAFDC = 0.
      EXEXP = 0.
      AMORTE = 0.
      AFCEXP = 0.
      WODFTX = 0.
C
C***********************************************************************
C
C    ABANDONMENT METHODS       CWIP           AFUDC
C
C          1                     RB             RB
C          2                     RB             DD
C          3                     RB             EE
C          4                     DD             DD
C          5                     DD             EE
C          6                     EE             EE
C
C      RB = put into rate base (a return is earned and the amount is
C           expensed for revenue purposes)
C      DD = put into a deferred debit account (no return is earned
C           and the project is expensed for revenue purposes)
C      EE = extraordinary expense (written off below the line)
C
C***********************************************************************
C
      IF(ABMETH == 6 ) THEN
C
C ABANDONMENT 6 WAS SPECIFIED.  THE CWIP AND AFUDC AMOUNTS ARE
C WRITTEN OFF TO EXTRAORDINARY EXPENSE.
C
         EXEXP(LASTYR) = BOKBL
         AFCEXP(LASTYR) = AFCBL
      ELSE
C
C    CWIP WRITE-OFF CALCULATIONS FROM HERE TO STATEMENT # 100:
C
C           ADD THE DISALLOWED TAX PORTION OF THE BOOK WRITE OFF TO
C           THE DEFERRED TAX ADJUSTMENT VECTOR
C
         EXEXP(LASTYR)  = BOKDAL
C
C        COMPUTE THE AMOUNT TO WRITE OFF FOR BOOK PURPOSES
C
C        COMPUTE THE ANNUAL AMOUNT OF THE WRITE OFF TO AMORTIZE
         AMORT  = BOKWO
         AMTWO  = BOKWO
         IF( WOYRS > 1.0 ) AMORT = AMTWO / WOYRS
         IF( WOYRS >= 99.0 ) AMORT = 0.0
C
C        COMPUTE THE YEAR END UNAMORTIZED BALANCE AND ADD IT
C        TO THE APPROPRIATE DEBIT ACCOUNT
C
         DO J = LASTYR, NYRS
C        IF THE BALANCE TO BE AMORTIZED IS LESS THAN THE
C        ANNUAL AMORTIZATION EXPENSE
            IF((AMTWO - AMORT) <= 0.0) THEN
               AMORTE(J) = AMTWO
               DO T = J, NYRS
                  DDB(T) = BOKWO
               ENDDO
               EXIT
            ELSE
               AMTWO = AMTWO - AMORT
               IF(ABMETH <= 3) RBDDB(J) = AMTWO
C              THEN ADD BALANCE TO RATE BASE DEFERRED DEBIT
C              AND ADD BALANCE TO DEFERRED DEBIT ACCOUNT
               DDB(J) =  BOKWO
C        ADD THE ANNUAL AMORTIZATION EXPENSE TO THE
C        TOTAL AMORTIZATION EXPENSE
               AMORTE(J) = AMORT
            ENDIF
         ENDDO
C
C       NORMALIZE THE WRITE OFF OF THE TAX BENEFIT
C
         IF(ABACCT == 'N' .AND. WOYRS < 99.0) THEN
            AMORT  = NRTXWO
            AMTWO  = NRTXWO
            IF(WOYRS > 1.0) AMORT = AMTWO / WOYRS
C
C           COMPUTE THE YEAR END UNAMORTIZED TAX BALANCE AND ADD IT
C            TO THE WRITE OFF DEFERRED TAX VECTOR
C
            WODFTX(LASTYR) = NRTXWO
            DO J = LASTYR, NYRS
C IF THE BALANCE TO BE AMORTIZED IS LESS THAN THE
C ANNUAL AMORTIZATION EXPENSE
               IF((AMTWO - AMORT) <= 0.0) THEN
                  WODFTX(J) = WODFTX(J) - AMTWO
                  EXIT
               ELSE
                  AMTWO = AMTWO - AMORT
                  WODFTX(J) = WODFTX(J) - AMORT
               ENDIF
            ENDDO
         ENDIF
C
C     AFUDC WRITE OFF CALCULATIONS FROM HERE TO STATEMENT #140:
C
         IF(ABMETH == 3 .OR. ABMETH == 5) THEN
            AFCEXP(LASTYR) = AFCBL
         ELSE
            AJAFDC(LASTYR) = AFCWO
            AFCEXP(LASTYR) = AFCDAL
C           NOTE: BASE YEAR AFDC1 BALANCE IS ELEMENT AFDC1(1)
C
C           COMPUTE THE ANNUAL AMOUNT OF THE WRITE OFF TO AMORTIZE
            AMORT  = AFCWO
            AMTWO  = AFCWO
            IF( WOYRS > 1.0 ) AMORT = AMTWO/WOYRS
            IF( WOYRS >= 99.0 ) AMORT = 0.0
C
C           COMPUTE THE YEAR END UNAMORTIZED BALANCE AND ADD IT
C           TO THE APPROPRIATE DEBIT ACCOUNT
C
            DO J = LASTYR, NYRS
C           IF THE BALANCE TO BE AMORTIZED IS LESS THAN THE
C           ANNUAL AMORTIZATION EXPENSE
               IF((AMTWO - AMORT) <= 0.0) THEN
                  AMORTE(J) = AMTWO + AMORTE(J)
                  DO T = J, NYRS
                     DDB(T) = AFCWO + DDB(T)
                  ENDDO
                  EXIT
               ELSE
                  AMTWO = AMTWO - AMORT
C                 THEN ADD BALANCE TO RATE BASE DEFERRED DEBIT
                  IF(ABMETH == 1) RBDDB(J) = AMTWO + RBDDB(J)
C                 AND ADD BALANCE TO DEFERRED DEBIT ACCOUNT
                  DDB(J) = AFCWO + DDB(J)
C                 ADD THE ANNUAL AMORTIZATION EXPENSE TO THE
C                 TOTAL AMORTIZATION EXPENSE
                  AMORTE(J) = AMORT + AMORTE(J)
               ENDIF
            ENDDO
         ENDIF
      ENDIF
      RETURN
      END
C***********************************************************************
C*                                                                     *
C*                            INTEREST_CAP                             *
C*                                                                     *
C*          COPYRIGHT (C) 1990    M.S. GERBER & ASSOCIATES, INC        *
C*                         ALL RIGHTS RESERVED                         *
C*                        FOR THE ST-FIN MODEL                         *
C*          COPYRIGHT (C) 1993 M.S. GERBER & ASSOCIATES, INC.          *
C*                         ALL RIGHTS RESERVED                         *
C*              MODIFIED AND MORED TO MIDAS GOLD                       *
C*                                                                     *
C***********************************************************************
C                                                                      *
C     PURPOSE:                                                         *
C        AFUDC CALCULATES:                                             *
C           ANNUAL YEAR END CWIP BALANCE                    CWIP       *
C           ANNUAL INTEREST ON CONSTRUCTION EXPENSES        PCAPINRST  *
C           ANNUAL INTEREST ON PLANT ENTERING SERVICE       CAPINRST   *
C           ANNUAL AMOUNT OF PLANT ENTERING SERVICE         CEP        *
C           ANNUAL CASH EXPENDITURES                        CE         *
C                                                                      *
C***********************************************************************
C
      SUBROUTINE INTEREST_CAP(CE,CEP,CWIP,DEPMET,AFDCFC,
     +                        SERVICEMO,CAPINRST,
     +                        PCAPINRST,
     +                        INTEREST_CAPITALIZATION_RATE,
     +                        STARTING_INTEREST_BALANCE,
     +                        FINANCIAL_SIMULATION_YEARS)
C
      INCLUDE 'SIZECOM.MON'
      REAL*4 INTEREST_CAPITALIZATION_RATE(*),TOTAL_CEP,
     +     INTEREST_BALANCE,STARTING_INTEREST_BALANCE
C
      CHARACTER*4 DEPMET
      INTEGER*1 J,FINANCIAL_SIMULATION_YEARS
      REAL*4 CE(*),CEP(*),CWIP(*),AFDCFC,CWIP_BAL
      REAL*4 OUTSERVICE,INSERVICE,
     +     CAPINRST(*),PCAPINRST(*)
      INTEGER*2 SERVICEMO
C
      IF(INDEX(DEPMET,'ACRS') /= 0 .OR. AFDCFC < .5) THEN
         DO J = 1, FINANCIAL_SIMULATION_YEARS
            PCAPINRST(J) = 0.
            CAPINRST(J) = 0.
         ENDDO
      ELSE
         OUTSERVICE = FLOAT(SERVICEMO-1)/12.
         INSERVICE = 1. - OUTSERVICE
         TOTAL_CEP = 0.0
         PCAPINRST(1) = STARTING_INTEREST_BALANCE
         DO J = 2, FINANCIAL_SIMULATION_YEARS
            PCAPINRST(J) = 0.
C
C CALCULATE ANNUAL INTEREST
C
            IF(CE(J) == CEP(J)) THEN
               PCAPINRST(J) = INTEREST_CAPITALIZATION_RATE(J) *
     +                           (OUTSERVICE*CE(J)/2.+CWIP(J-1))
            ELSE
               PCAPINRST(J) = INTEREST_CAPITALIZATION_RATE(J) *
     +                           (CE(J)/2. + OUTSERVICE*CWIP(J-1) + 
     +                           INSERVICE*(MAX((CWIP(J-1)-CEP(J)),0.) -
     +                           MAX((CEP(J)-CWIP(J-1)),0.)/2.))
            ENDIF
            TOTAL_CEP   = TOTAL_CEP + CEP(J)
         ENDDO
C
C CAPITALIZE THE INTEREST
C
         INTEREST_BALANCE = STARTING_INTEREST_BALANCE
         CAPINRST(1) = 0.
         CWIP_BAL = CWIP(1)
         DO J = 2, FINANCIAL_SIMULATION_YEARS
            CWIP_BAL = CWIP_BAL + CE(J)
            INTEREST_BALANCE = INTEREST_BALANCE + PCAPINRST(J)
            IF(CEP(J) /= 0 .AND. CWIP_BAL /= 0.) THEN
               CAPINRST(J) = INTEREST_BALANCE * CEP(J)/CWIP_BAL
               INTEREST_BALANCE = INTEREST_BALANCE - CAPINRST(J)
               CWIP_BAL = CWIP_BAL - CEP(J)
            ELSE
               CAPINRST(J) = 0.
            ENDIF
         ENDDO
      ENDIF ! LARGE ENOUGHT PROJECT TO CAP INTEREST
      RETURN
      END
