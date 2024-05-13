!     ******************************************************************
!     SNIPPIT.FOR
!     Copyright(c) M. S. Gerber 2000
!
!     Created: 3/29/2007 3:26:49 PM
!     Author : GREG TURK
!     Last change: GT  3/29/2007 3:26:49 PM
!     ******************************************************************

C***********************************************************************
      ENTRY WRITE_USER_TIE_FLOW_DATA(R_MONTH,R_DAY,R_DAY_WEEK,
     +                              R_TRANS_GROUP,
     +                              R_MULTI_AREA_NAME,
     +                              R_CREATE_HOURLY_PRICE)
C***********************************************************************
!
         LOCAL_UNIT_NO = 4455 + R_TRANS_GROUP
         IF(R_MONTH == FIRST_MONTHLY_TRANSACT .AND. R_DAY == 1) THEN
!               
            IF(R_CREATE_HOURLY_PRICE) THEN
!
! 092706. TEST WITH NEW TEMP_I AND NO YES_TWO_PRB_DIGITS.
! 092806. JUST CHANGED TEMP_I BACK.
! 092806. HARD-WIRED FROM LOAD_FILE_CHAR_EXT TO SCEN_FILE_CHAR_EXT 
!         (STILL NOT USING YES_TWO_PRB_DIGITS) 
! 092806. ADDED TEMP_MULTI_AREA_NAME
! 092906. SEEMS TO BE RELATED TO THE MARKET_PRICE_NAME WHEN 
!         IT IS THREE CHARACTERS.
! 093006. CONFIRMED THAT THE TWO DIGIT VERSION WORKS.
!
               TEMP_I = GET_PRB_SEQUENCE_NUMBER() + END_POINT - 1
               TEMP_MULTI_AREA_NAME = R_MULTI_AREA_NAME
!               TEMP_I = END_POINT
!
               IF(YES_TWO_PRB_DIGITS() .AND. TEMP_I < 99) THEN
                  MARKET_PRICE_NAME = TRIM(TEMP_MULTI_AREA_NAME)//
     +                                        LOAD_FILE_CHAR_EXT(TEMP_I)
               ELSE
!!!!                  WRITE(TEMP_STR,'(I3.3)') TEMP_I
!                  TEMP_TWO_DIGIT_NAME = R_MULTI_AREA_NAME
                  MARKET_PRICE_NAME = 
     +                   TRIM(R_MULTI_AREA_NAME(1:2))//
     +                                        SCEN_FILE_CHAR_EXT(TEMP_I)
               ENDIF
!
!              CHECK FOR UNIQUE NAME FOR EACH GROUP?
!
               FILE_NAME = trim(PRB_FILE_DIRECTORY())//"LDE"//
     +                                 trim(MARKET_PRICE_NAME)//".B"//
     +                   LOAD_FILE_CHAR_EXT(GET_MARKET_PRICE_YEAR(YEAR))
!
! 100206. 
!
               DO TEMP_I = 1, 256
                  IF(MARKET_PRICE_NAME ==
     +                              SAVE_MARKET_PRICE_NAME(TEMP_I)) THEN 
                     WRITE(4,*) 'REDUNDANT MARKET PRICE NAMES'
                     WRITE(4,*) 'MARKET NAME = ',MARKET_PRICE_NAME
                     WRITE(4,*) 'PLEASE RENAME IN THE TRANSACTION'
                     WRITE(4,*) 'GROUPS FILE'
                     er_message='Stop requested from SNIPPIT SIID269'
                     call end_program(er_message)
                  ELSEIF(SAVE_MARKET_PRICE_NAME(TEMP_I) == 'NONE ') THEN
                     SAVE_MARKET_PRICE_NAME(TEMP_I) = MARKET_PRICE_NAME
                     EXIT
                  ENDIF
               END DO
               INQUIRE(UNIT=LOCAL_UNIT_NO,OPENED=PRICE_FILE_OPEN)
               IF(PRICE_FILE_OPEN) CLOSE(LOCAL_UNIT_NO)
!
! 110206. THE FILE DID NOT SEEM TO PROPERLY ERASE PER DOUG E-MAIL. 
!         SO REPLACE IF EXISTS
!
               INQUIRE(FILE=FILE_NAME,EXIST=PRICE_FILE_EXISTS)
               IF(PRICE_FILE_EXISTS) THEN ! CALL ERASE(FILE_NAME)
                  OPEN(UNIT=LOCAL_UNIT_NO,
     +               FILE=FILE_NAME,FORM='UNFORMATTED',RECL=118,
     +                                ACCESS="DIRECT",STATUS="REPLACE")
               ELSE
                  OPEN(UNIT=LOCAL_UNIT_NO,
     +               FILE=FILE_NAME,FORM='UNFORMATTED',RECL=118,
     +                                ACCESS="DIRECT",STATUS="NEW")
               ENDIF
! 07/07/03.
               IF(LAHEY_LF95()) THEN
                  WRITE(LOCAL_UNIT_NO,REC=1) F7,INT2(118)
               ENDIF
            ENDIF
!
         ENDIF
! 11/1/99.
!         
         IF(R_CREATE_HOURLY_PRICE) THEN
            USER_IREC = ALINE_LOAD_DATA(USER_I,R_MONTH) + R_DAY - 1
!            
            WRITE(LOCAL_UNIT_NO,REC=USER_IREC)  
     +                         R_MONTH,R_DAY,USER_YEAR,
     +                         EEICODE,R_DAY_WEEK,
     +                         TIMZON,TEMPER,
     +                         DELTMP,
     +       (TIE_FLOW(R_TRANS_GROUP,MARKET_HR),MARKET_HR=1,24)
            USER_IREC = USER_IREC + 1
!                  
! ALWAYS ADD LEAP YEAR INTO LDE FILE
!
            IF(R_DAY == 28 .AND. R_MONTH == 2) THEN
               WRITE(LOCAL_UNIT_NO,REC=USER_IREC)
     +                          R_MONTH,INT2(29),USER_YEAR,
     +                          EEICODE,R_DAY_WEEK,
     +                          TIMZON,TEMPER,
     +                          DELTMP,
     +          (TIE_FLOW(R_TRANS_GROUP,MARKET_HR),MARKET_HR=1,24)
               USER_IREC = USER_IREC + 1
            ENDIF
         ENDIF
!        
!                  
! ALWAYS ADD LEAP YEAR INTO LDE FILE
!
         IF(R_DAY == 28 .AND. R_MONTH == 2) THEN
            IF(R_CREATE_HOURLY_PRICE) THEN
               WRITE(LOCAL_UNIT_NO,REC=USER_IREC)    
     +                              R_MONTH,INT2(29),USER_YEAR,
     +                              EEICODE,R_DAY_WEEK,
     +                              TIMZON,TEMPER,
     +                              DELTMP,
     +       (HOURLY_LAST_PRICE(R_TRANS_GROUP,MARKET_HR),MARKET_HR=1,24)
               USER_IREC = USER_IREC + 1
            ENDIF
         ENDIF
!               ENDDO ! DAYS
!
!
!            ENDDO ! MONTHS
         IF(R_MONTH == LAST_MONTHLY_TRANSACT .AND. R_DAY == 31) THEN
!         
            IF(R_CREATE_HOURLY_PRICE) THEN
!         
               CLOSE(LOCAL_UNIT_NO)
!               
            ENDIF
!
         ENDIF
!         
C
      RETURN

