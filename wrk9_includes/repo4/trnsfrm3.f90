!     ******************************************************************
!     TRNSFRM3.FOR
!     Copyright(c) M.S. Gerber & Associates 2000
!
!     Created: 1/2/2003 9:09:04 AM
!     Author : MARK S GERBER
!     Last change: MSG 11/15/2006 3:50:26 PM
!     ******************************************************************

!     ************************************************************
      SUBROUTINE SYSTEM_PEAK_FORECAST(FORCAST_ENRG,FORCAST_PEAK, &
                        CLASS_LOADS, &
                        CLASS_PEAKS,CLASS,YEAR, &
                        END_POINT,DAY_LOAD_SHAPES,FORCAST_LOADS, &
                        IMPORT_CAP,EXPORT_CAP)
!     ************************************************************
!
!  TAKES HISTORICAL TYPICAL DAY LOAD CURVE VALUES BY SEASON
!  BY EACH CLASS,  & FORECAST PEAK KW & TOTAL KWH,
!  FORMS FORECAST TYPICAL DAY LOAD CURVE VALUES BY SEASON FOR THE
!  CURRENT YEAR OF STUDY.
!
!  10/31/89. ORIGINAL CODE FROM FORCST.FOR AND SUBROUTINE FORCST.
!  CODE WAS MODIFIED TO REPLACE THE ORIGINAL HUERISTIC CREATION
!  OF FORECASTED DAY TYPES BY A LINEAR TRANSFORMATION OF THE LOAD
!  CURVE. THE LINEAR TRANSFORMATION IS CONSTRAINED BY TWO VARIABLES:
!  THE RATIO OF FORECASTED PEAK TO HISTORICAL PEAK, AND THE RATIO
!  OF FORECASTED ENERGY TO HISTORICAL ENERGY. THUS, THE TRANSFORMATION
!  ENSURES THAT THE DAY TYPE FORECASTED PEAK AND ENERGY IS MAINTAINED.
!
!     11/9/89. MODIFIED TO ALLOW FOR THREE DAY TYPES (AVE WKDAY, AVE
!     WKEND DAY, AVE PK DAY). CHANGES INCLUDE UP'ING THE DAY INDEX TO
!     3, PASSING TWO NEW VARIABLES,
!     MAKING INTERCEPT AND SLOPE VARIABLES ARRAYS, AND CREATING A
!     WEIGHTED LINEAR TRANSFORM FOR THE AVE PK DAY TYPE BASED UPON
!     THE NUMBER OF WKDAYS AND WKEND DAYS THAT COMPOSE THE AVE PK DAY.
!
!
!
!  DAY TYPE 1 = WEEKDAYS
!  DAY TYPE 2 = WEEKENDS WITH HOLIDAYS
!
!  CLASS # CLASS = 1: COMMERICAL
!  CLASS # CLASS = 2: RESIDENTIAL
!  CLASS # CLASS = 3: INDUSTRIAL
!  CLASS # CLASS = 4: OTHER 1
!  CLASS # CLASS = 5: OTHER 2
!  CLASS # CLASS = 6: OTHER 3
!  CLASS # CLASS = 7: SYSTEM
!
!
      use SpinDriftLib
      USE PROD_ARRAYS_DIMENSIONS
      use trans_slope
      use trans_weekend_peak_days
      use reference_loadsi2
      USE SIZECOM
!     INCLUDE 'LAMCOM.MON'

      CHARACTER (len=20) ::  MONTH_NAME
!    +(12)/'January','February','March','April',
!    +         'May','June','July','August','September','October',
!    +         'November','December'/
      CHARACTER (len=9) :: &
             DAY_NAME(3)=(/' weekdays',' weekends',' peakdays'/)
      CHARACTER (len=11) :: CLASS_NAME(7)=(/'Commercial ','Residential', &
                'Industrial ','Other 1    ','Other 2    ','Other 3    ', &
                'System     '/)
      INTEGER (kind=2) ::  SEASON,DAY,HOUR,LRECL, &
                YEAR,CLASS,END_POINT,THREE_DAYS,TOTAL_DAYS
!     REAL PEAK_GROWTH_RATE ! REMOVED 3/3/92
      REAL :: FORCAST_DAILY_ENRG,BASE_YEAR_DAILY_ENRG
      REAL :: FORCAST_ENRG(2,12)
      REAL :: CLASS_LOADS(24,2,12),CLASS_PEAKS(2,12),FORCAST_PEAK(2,12)
      REAL :: SUM_OF_LOADS(12),SUM
      REAL :: BASE_YEAR_AVE_DAILY_ENRG,FORECAST_AVE_DAILY_ENRG
      REAL :: BASE_YEAR_LOAD_FACTOR
      REAL :: FORECAST_LOAD_FACTOR,P1,P2,DENOMINATOR
      REAL :: LOAD_FACTOR_CHANGE,LOAD_FACTOR_TEST1,LOAD_FACTOR_TEST2
      REAL :: DAY_LOAD_SHAPES(24,3,12),MAX_RR(12),AVE_RR(12),RR_PEAK
      REAL :: RR_ENERGY,IMPORT_CAP,EXPORT_CAP,RR_MULT,R_IMPORT_CAP
      REAL :: R_EXPORT_CAP
!
!
      INTEGER (kind=2) :: BASE_YEAR
      CHARACTER (len=256) :: OUTPUT_DIRECTORY
      CHARACTER (len=256) :: FILE_NAME
      LOGICAL (kind=4) :: INTERCHANGE_FILE_EXISTS=.FALSE.
      LOGICAL (kind=4) :: ECON_FILE_OPENED
!
! INCLUDE COMMON BLOCK OF ALL FORECAST TYPICAL CURVES FOR THIS YEAR
!
      REAL ::  FORCAST_LOADS(24,3,12,7)
!
!     INCLUDE 'TRANCOM.MON' !TRANCOM.MON MOVED INTO LAMCOM.MON 8/12/93
!
!
!
!
!     INCLUDE 'OVLYCOM.MON FOR BIN FILE NAME'
!
!     INCLUDE 'OVLYCOM.MON'
      CHARACTER (len=2) ::  ECON_OL
!
      INTEGER (kind=2) ::  ECON_YEAR,R_YEAR,R_SEASON
      REAL :: S_CUM_EXTENSION_MULT(12),S_EXTENSION_MULT(12)
      REAL :: R_EXTENSION_MULT
      SAVE  S_CUM_EXTENSION_MULT,S_EXTENSION_MULT
!
      REAL (kind=4) :: ENERGY_MIX
      REAL (kind=4) :: PRIMARY_ENERGY_COST,SECONDARY_ENERGY_COST
      REAL (kind=4) :: PRIMARY_EMISSIONS_RATE(5)
      REAL (kind=4) :: SECONDARY_EMISSIONS_RATE(5)
      REAL (kind=4) :: EMISSIONS_CREDIT_PRICE(5)
      REAL (kind=4) :: MAX_ALLOCATOR
      REAL (kind=4) :: PRIMARY_ENERGY_COST_ADDER
      REAL (kind=4) :: SECONDARY_ENERGY_COST_ADDER,TOTAL_EXTERNAL_FACTOR
      LOGICAL (kind=1) :: USE_EXTERNAL_HOURLY_LOADS,USE_EXT_LOAD_SOURCE
!
!
! END OF DATA DECLARATIONS
!
      USE_EXTERNAL_HOURLY_LOADS = USE_EXT_LOAD_SOURCE()
      DO SEASON = 1, 12
        SUM_OF_LOADS(SEASON) = 0.0
      ENDDO
!
! NOW READ IN RUNNING RATE FORECASTS
!
      IF(YEAR == 1) THEN
         CALL GET_ECON_OL(ECON_OL,LRECL)
         FILE_NAME = trim(OUTPUT_DIRECTORY())//ECON_OL//'ECINT.BIN'
         INQUIRE(FILE=FILE_NAME,EXIST=INTERCHANGE_FILE_EXISTS)
         IF(INTERCHANGE_FILE_EXISTS) OPEN(84,FILE=FILE_NAME, &
                                             RECL=LRECL,ACCESS='DIRECT')
      ENDIF
      IF(INTERCHANGE_FILE_EXISTS) THEN
         READ(84,REC=YEAR) (MAX_RR(SEASON),AVE_RR(SEASON),SEASON=1,12), &
               IMPORT_CAP,EXPORT_CAP,RR_MULT, &
               ENERGY_MIX,PRIMARY_ENERGY_COST,SECONDARY_ENERGY_COST, &
               PRIMARY_EMISSIONS_RATE,SECONDARY_EMISSIONS_RATE, &
               EMISSIONS_CREDIT_PRICE
         IF(PRIMARY_ENERGY_COST /= -999. .AND. &
                                    SECONDARY_ENERGY_COST /= -999.) THEN
            MAX_ALLOCATOR = -9999999.
            DO SEASON = 1, 12
               MAX_ALLOCATOR = MAX(MAX_ALLOCATOR,MAX_RR(SEASON))
            ENDDO
            IF(MAX_ALLOCATOR /= 0.) THEN
               MAX_ALLOCATOR = 1./MAX_ALLOCATOR
            ELSE
               WRITE(4,*) "MAXIMUM RR IN ECONOMY INTERCHANGE IS ZERO"
            ENDIF
            PRIMARY_ENERGY_COST = PRIMARY_ENERGY_COST*ENERGY_MIX
            PRIMARY_ENERGY_COST_ADDER = ENERGY_MIX * ( &
               PRIMARY_EMISSIONS_RATE(1)*EMISSIONS_CREDIT_PRICE(1) + &
               PRIMARY_EMISSIONS_RATE(2)*EMISSIONS_CREDIT_PRICE(2) + &
               PRIMARY_EMISSIONS_RATE(3)*EMISSIONS_CREDIT_PRICE(3) + &
               PRIMARY_EMISSIONS_RATE(4)*EMISSIONS_CREDIT_PRICE(4) + &
               PRIMARY_EMISSIONS_RATE(5)*EMISSIONS_CREDIT_PRICE(5))
!
            SECONDARY_ENERGY_COST =SECONDARY_ENERGY_COST*(1.-ENERGY_MIX)
            SECONDARY_ENERGY_COST_ADDER = (1.-ENERGY_MIX) * ( &
               SECONDARY_EMISSIONS_RATE(1)*EMISSIONS_CREDIT_PRICE(1) + &
               SECONDARY_EMISSIONS_RATE(2)*EMISSIONS_CREDIT_PRICE(2) + &
               SECONDARY_EMISSIONS_RATE(3)*EMISSIONS_CREDIT_PRICE(3) + &
               SECONDARY_EMISSIONS_RATE(4)*EMISSIONS_CREDIT_PRICE(4) + &
               SECONDARY_EMISSIONS_RATE(5)*EMISSIONS_CREDIT_PRICE(5))
            TOTAL_EXTERNAL_FACTOR = &
                  PRIMARY_ENERGY_COST + PRIMARY_ENERGY_COST_ADDER + &
                  SECONDARY_ENERGY_COST + SECONDARY_ENERGY_COST_ADDER
            DO SEASON = 1,12
               MAX_RR(SEASON)=RR_MULT * MAX_RR(SEASON) * &
                        MAX_ALLOCATOR * TOTAL_EXTERNAL_FACTOR
               AVE_RR(SEASON)=RR_MULT * AVE_RR(SEASON) * &
                        MAX_ALLOCATOR * TOTAL_EXTERNAL_FACTOR
            ENDDO
!
         ELSE
            IF(RR_MULT /= 1.) THEN
               DO SEASON = 1, 12
                     MAX_RR(SEASON) = RR_MULT * MAX_RR(SEASON)
                     AVE_RR(SEASON) = RR_MULT * AVE_RR(SEASON)
               ENDDO
            ENDIF
         ENDIF
      ELSE
         DO SEASON = 1, 12
            MAX_RR(SEASON) = 0.
            AVE_RR(SEASON) = 0.
         ENDDO
      ENDIF
!
!
!  BEGIN FORECAST LOOP BY SEASON AND DAY TYPE
!
      DO SEASON = 1, 12
         RR_PEAK = 0.
         RR_ENERGY = 0.
         TOTAL_DAYS = 0
!
!
!
         DO DAY = 1, 3
!
            LOAD_FACTOR_TEST1=0
            LOAD_FACTOR_TEST2=0
!
!           FIND THE SLOPE AND INTERCEPT FOR THE FIRST TWO DAY TYPES
!
            IF(DAY < 3) THEN
!
!           COMPUTE AVERAGE DAILY ENERGY FOREC & HISTOR THIS DAY TYPE.
!
            FORCAST_DAILY_ENRG = FORCAST_ENRG(DAY,SEASON) / &
                           FLOAT(HISTORICAL_DAY_COUNT(DAY,SEASON))
            BASE_YEAR_DAILY_ENRG = 0.
            DO HOUR = 1,24
               BASE_YEAR_DAILY_ENRG = BASE_YEAR_DAILY_ENRG + &
                                      CLASS_LOADS(HOUR,DAY,SEASON)
            ENDDO
            RR_PEAK = MAX(RR_PEAK,CLASS_PEAKS(DAY,SEASON))
            RR_ENERGY = RR_ENERGY + BASE_YEAR_DAILY_ENRG * &
                           FLOAT(HISTORICAL_DAY_COUNT(DAY,SEASON))
            TOTAL_DAYS = TOTAL_DAYS + HISTORICAL_DAY_COUNT(DAY,SEASON)
!
!  FIND GROWTH RATE IN (COINCIDENT) PEAK & IN DAILY MWH
!  (GROWTH RATE IN PEAK FOR DAY TYPE 2 ASSUMED = THAT FOR DAY TYPE 1)
!
!           PEAK_GROWTH_RATE = FORCAST_PEAK(DAY,SEASON) /
!    +                            CLASS_PEAKS(DAY,SEASON) !NOT USED 3/3/92
!
!
!   WE WANT TO CREATE A NEW LOAD SHAPE BASED UPON NEW
!   ENERGY AND PEAK VALUES. CREATE A LINEAR RELATIONSHIP BETWEEN THE
!   OLD AVERAGE DAY TYPE AND THE NEW AVERAGE DAY TYPE. THE TWO POINTS
!   THAT DETERMINE THE LINE ARE THE HISTORICAL PEAK AND AVERAGE DAILY
!   ENERGY AND THE NEW PEAK AND AVERAGE DAILY ENERGY. THE LINE IS
!   CHARACTERIZED BY A SLOPE AND AN INTERCEPT.
!
!
            BASE_YEAR_AVE_DAILY_ENRG = BASE_YEAR_DAILY_ENRG/24
            FORECAST_AVE_DAILY_ENRG = FORCAST_DAILY_ENRG/24
            SLOPE(DAY,SEASON) = (CLASS_PEAKS(DAY,SEASON)- &
                    BASE_YEAR_AVE_DAILY_ENRG)/(FORCAST_PEAK(DAY,SEASON)- &
                                                FORECAST_AVE_DAILY_ENRG)
            INTERCEPT(DAY,SEASON) = CLASS_PEAKS(DAY,SEASON)- &
                              SLOPE(DAY,SEASON)*FORCAST_PEAK(DAY,SEASON)
!
!
!   CALCULATE THE OLD AND NEW LOAD FACTORS FOR THE DATA.
!
            BASE_YEAR_LOAD_FACTOR = BASE_YEAR_AVE_DAILY_ENRG/ &
                                                 CLASS_PEAKS(DAY,SEASON)
!
            FORECAST_LOAD_FACTOR = FORECAST_AVE_DAILY_ENRG/ &
                                                FORCAST_PEAK(DAY,SEASON)
!
!
!    CHECK TO SEE WHETHER THE LOAD FACTOR EXCEEDS 1.0
!
            IF(FORECAST_LOAD_FACTOR > 1.) LOAD_FACTOR_TEST1 = 1
!
!
!   CHECK TO SEE WHETHER THE LOAD FACTOR CHANGES BY MORE
!                 THAN TWENTY POINTS
!
!
            LOAD_FACTOR_CHANGE = ABS(FORECAST_LOAD_FACTOR- &
                                           BASE_YEAR_LOAD_FACTOR)
            IF(LOAD_FACTOR_CHANGE > .2) LOAD_FACTOR_TEST2 = 1
!
!           CREATE THE SLOPE AND INTERCEPT FOR THE AVE PEAK DAY TYPE
            ELSE
!
               P1 = DBLE(DBLE(4.0)-FLOAT(WEEKEND_PEAK_DAYS(SEASON)))/ &
                                                               DBLE(4.0)
               P2 = DBLE(1 - P1)
               DENOMINATOR = DBLE(SLOPE(2,SEASON)*P1 + &
                                                     SLOPE(1,SEASON)*P2)
               IF(DENOMINATOR == 0.0) THEN ! ASSUME EXTERNAL LOADS FOR NOW.
                  INTERCEPT(3,SEASON)= 0.0
                  SLOPE(3,SEASON) = 0.0
               ELSE
                  INTERCEPT(3,SEASON)= DBLE((SLOPE(2,SEASON)*P1* &
                               INTERCEPT(1,SEASON) + SLOPE(1,SEASON)*P2* &
                                       INTERCEPT(2,SEASON)))/DENOMINATOR
                  SLOPE(3,SEASON)=DBLE(SLOPE(1,SEASON)*SLOPE(2,SEASON))/ &
                                          DENOMINATOR
               ENDIF
             ENDIF
!
!   LINEARLY TRANSFORM OLD DATA BY DAY TYPE TO NEW DATA BY DAY TYPE
!
            SUM = 0.0
            DO HOUR = 1 , 24
               IF(SLOPE(DAY,SEASON) /= 0.) THEN
                  FORCAST_LOADS(HOUR,DAY,SEASON,CLASS)= &
                         (DAY_LOAD_SHAPES(HOUR,DAY,SEASON) - &
                                INTERCEPT(DAY,SEASON))/SLOPE(DAY,SEASON)
               ELSE
                  FORCAST_LOADS(HOUR,DAY,SEASON,CLASS) = &
                                        DAY_LOAD_SHAPES(HOUR,DAY,SEASON)
               ENDIF
               SUM = SUM +  FORCAST_LOADS(HOUR,DAY,SEASON,CLASS)
            ENDDO
!
!  CHECK THAT TOTAL ALLOCATED ENERGY = TOTAL FORECAST ENERGY
!
!
            IF(DAY == 1) THREE_DAYS = HISTORICAL_DAY_COUNT(DAY,SEASON) - &
                                           4 + WEEKEND_PEAK_DAYS(SEASON)
            IF(DAY == 2) THREE_DAYS = HISTORICAL_DAY_COUNT(DAY,SEASON) - &
                                               WEEKEND_PEAK_DAYS(SEASON)
            IF(DAY == 3) THREE_DAYS = 4
!
            SUM_OF_LOADS(SEASON) = SUM_OF_LOADS(SEASON) + SUM* &
                                                       FLOAT(THREE_DAYS)
!
!     WARN IF LOAD FACTOR EXCEEDS 1.0
!
            IF(LOAD_FACTOR_TEST1 == 1 .AND. &
                                 .NOT. USE_EXTERNAL_HOURLY_LOADS) THEN
               WRITE(4,*) 'For the ',trim(CLASS_NAME(CLASS)), &
                          ' class during ', &
               trim(MONTH_NAME(SEASON)),trim(DAY_NAME(DAY)), &
                 ' in year',BASE_YEAR()+YEAR,','
               WRITE(4,*) 'for end point',END_POINT
               WRITE(4,*) 'The load factor exceeded 1.0. Check the'
               WRITE(4,*) 'peak and energy growth forecasts'
               WRITE(4,*) ' '
!
! NOT FATAL FOR SRP. 11/25/98. GAT.
!
!               STOP 'Error in SYSTEM_PEAK_FORECAST. Check MIDAS.ERR
!     +               file.'
            ENDIF
!
!     WARN IF LOAD FACTORS CHANGE RAPIDLY
!
            IF(LOAD_FACTOR_TEST2.EQ.1 .AND. .NOT. &
                                       USE_EXTERNAL_HOURLY_LOADS) THEN
               WRITE(4,*) 'For the ',trim(CLASS_NAME(CLASS)), &
                          ' class during ', &
               trim(MONTH_NAME(SEASON)),trim(DAY_NAME(DAY)), &
                 ' in year',BASE_YEAR()+YEAR,','
               WRITE(4,*) 'for end point',END_POINT
               WRITE(4,*) 'the load factor changed dramatically.'
               WRITE(4,*) 'Check the peak and energy growth forecasts'
               WRITE(4,*) ' '
            ENDIF
!
!
         ENDDO  ! END DAY TYPE LOOP
!
!        ECONOMY INTERCHANGE SECTION
!
         RR_SLOPE(SEASON) = 1
         IF(INTERCHANGE_FILE_EXISTS) THEN
            RR_ENERGY = RR_ENERGY/FLOAT(TOTAL_DAYS*24)
            IF(MAX_RR(SEASON) .NE. AVE_RR(SEASON)) RR_SLOPE(SEASON) = &
                     (RR_PEAK-RR_ENERGY)/(MAX_RR(SEASON)-AVE_RR(SEASON))
            RR_INTERCEPT(SEASON)=RR_PEAK-RR_SLOPE(SEASON)*MAX_RR(SEASON)
         ELSE
            RR_ENERGY = 0.
            RR_INTERCEPT(SEASON) = 0.
         ENDIF
!
      ENDDO   ! END SEASON LOOP
      RETURN
!
      ENTRY CALC_ECONOMY_EXTENSION_MULT(R_YEAR)
!
         IF(.NOT. INTERCHANGE_FILE_EXISTS) RETURN
         ECON_YEAR = MAX(R_YEAR - 1,1)

         READ(84,REC=ECON_YEAR) &
               (MAX_RR(SEASON),AVE_RR(SEASON),SEASON=1,12), &
               R_IMPORT_CAP,R_EXPORT_CAP,RR_MULT, &
               ENERGY_MIX,PRIMARY_ENERGY_COST,SECONDARY_ENERGY_COST, &
               PRIMARY_EMISSIONS_RATE,SECONDARY_EMISSIONS_RATE, &
               EMISSIONS_CREDIT_PRICE
!
         ECON_YEAR = MAX(R_YEAR,INT(2,2))
         READ(84,REC=ECON_YEAR) &
               (MAX_RR(SEASON),S_EXTENSION_MULT(SEASON),SEASON=1,12), &
               R_IMPORT_CAP,R_EXPORT_CAP,RR_MULT, &
               ENERGY_MIX,PRIMARY_ENERGY_COST,SECONDARY_ENERGY_COST, &
               PRIMARY_EMISSIONS_RATE,SECONDARY_EMISSIONS_RATE, &
               EMISSIONS_CREDIT_PRICE
!
         DO SEASON = 1, 12
            S_CUM_EXTENSION_MULT(SEASON) = 1.
            S_EXTENSION_MULT(SEASON) = S_EXTENSION_MULT(SEASON)/ &
                                                          AVE_RR(SEASON)
         ENDDO
!
      RETURN
!
      ENTRY GET_ECONOMY_EXTENSION_MULT(R_SEASON,R_EXTENSION_MULT)
!
         S_CUM_EXTENSION_MULT(R_SEASON)=S_CUM_EXTENSION_MULT(R_SEASON)* &
                                              S_EXTENSION_MULT(R_SEASON)
         R_EXTENSION_MULT = S_CUM_EXTENSION_MULT(R_SEASON)
!
      RETURN
!
      END


