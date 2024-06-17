!     ******************************************************************
!     CATAWBA.FOR
!     Copyright(c)  2000
!
!     Created: 1/10/2010 2:47:44 PM
!     Author : MARK S GERBER
!     Last change: MSG 1/10/2010 2:47:44 PM
!     ******************************************************************

C***********************************************************************
C
C                ROUTINE TO CALCULATE DUKE CATAWBA COSTS
C
C                             COPYRIGHT (C) 1996
C                        M.S. GERBER & ASSOCIATES, INC.
C                             ALL RIGHTS RESERVED
C
C***********************************************************************
C
      FUNCTION DUKE_TOTAL_CAPACITY_CHARGES(R_YEAR)
      USE SIZECOM
      INCLUDE 'GLOBECOM.MON'
      INTEGER*2 COMPANY
      REAL*4 DUKE_TOTAL_CAPACITY_CHARGES
!
      REAL*4
!        
     +		RESERVE_CAPACITY_MW,
     +		RESERVE_CAPACITY_RATE,
     +      RESERVE_CAPACITY_DOLLARS, 
     +		OEI_RESERVE_CAPACITY_DOLLARS,
     +		PEAK_REVISION_TRUE_UPS,
     +      ADJ_RESERVE_CAP_DOLLARS,
!
     +  		SUPPLEMENTAL_CAPACITY,
     +		SUPPLEMENTAL_CAPACITY_RATE,
     +      A_FACTOR,
     + 		B_FACTOR,
!
     +      SUPPLEMENTAL_CAP_DOLLARS,
     +      OEI_SUPPLEMENTAL_CAP_DOLLARS,
     +      SUPPLEMENTAL_TRUE_UPS,
     +		ADJ_SUPPLEMENTAL_CAP_DOLLARS,
!
     + 		MCGUIRE_X_CAPACITY,
     +		MCGUIRE_X_CAPACITY_RATE,
     +      ADJ_MCGUIRE_X_CAP_DOLLARS,
!     
     +      PURCHASED_CAPACITY_MW,
     +		PURCHASED_CAPACITY_RATE,
     +  	   UNADJ_PURCHASE_CAP_DOLLARS, 
     +		OEI_PURCHASED_CAP_PAYMENTS,
     +		PURCHASE_INTEREST_CREDIT,
     +		PURCHASE_CAP_TRUE_UPS,
     +		ADJ_PURCHASE_CAP_DOLLARS,
!
     +      DUKE_WHEELING_RATE,
     +      DUKE_WHEELING_COSTS
      INTEGER*2 CATAWBA_NO/0/,CATAWBA_HEADER,THIS_YEAR,R_YEAR
      INTEGER CATAWBA_REC
      SAVE CATAWBA_REC
      LOGICAL*1 CATAWBA_REPORT_NOT_OPEN/.TRUE./
      CHARACTER*5 COMPANY_NAME(4)
      DATA COMPANY_NAME/'NCEMC','SR   ','NCMPA','PMPA'/
      REAL*4 OWNERS_COSTS
      LOGICAL*1 WRITE_CATAWBA_REPORT
!
! END DATA DECLARATIONS
C***********************************************************************
      ENTRY WRITE_CATAWBA_REPORT(R_YEAR)
C***********************************************************************
         IF(CATAWBA_REPORT_NOT_OPEN) THEN
            CATAWBA_REPORT_NOT_OPEN = .FALSE.
            CATAWBA_NO = CATAWBA_HEADER(CATAWBA_REC)
         ENDIF
         THIS_YEAR = R_YEAR + BASE_YEAR
         OWNERS_COSTS = 0.
         WRITE_CATAWBA_REPORT = .FALSE.

         DUKE_TOTAL_CAPACITY_CHARGES = OWNERS_COSTS
      RETURN
      END
!
C***********************************************************************
C
C                ROUTINE TO CONVERT CATAWBA CAPACITY CHARGES
C
C                             COPYRIGHT (C) 1996
C                        M.S. GERBER & ASSOCIATES, INC.
C                             ALL RIGHTS RESERVED
C
C***********************************************************************
C
	FUNCTION DUKE_CAPACITY_CHARGES()
!
         INTEGER*2 R_COMPANY
         REAL*4
     +      DUKE_CAPACITY_CHARGES,
!        
     +      ADJ_RESERVE_CAP_DOLLARS,
     +      RESERVE_CAPACITY_DOLLARS, 
     +		RESERVE_CAPACITY_MW,
     +		RESERVE_CAPACITY_RATE,
     +		OEI_RESERVE_CAPACITY_DOLLARS,
     +		PEAK_REVISION_TRUE_UPS,
!
     +      SUPPLEMENTAL_CAP_DOLLARS,
     +		ADJ_SUPPLEMENTAL_CAP_DOLLARS,
     +  		SUPPLEMENTAL_CAPACITY,
     +		SUPPLEMENTAL_CAPACITY_RATE,
     +      OEI_SUPPLEMENTAL_CAP_DOLLARS,
     +      SUPPLEMENTAL_TRUE_UPS,
     +      A_FACTOR,
     + 		B_FACTOR,
!
     +      ADJ_MCGUIRE_X_CAP_DOLLARS,
     + 		MCGUIRE_X_CAPACITY,
     +		MCGUIRE_X_CAPACITY_RATE,
!     
     +  	   UNADJ_PURCHASE_CAP_DOLLARS, 
     +      PURCHASED_CAPACITY_MW,
     +		PURCHASED_CAPACITY_RATE,
     +		ADJ_PURCHASE_CAP_DOLLARS,
     +		OEI_PURCHASED_CAP_PAYMENTS,
     +		PURCHASE_CAP_TRUE_UPS,
     +		PURCHASE_INTEREST_CREDIT,
     +      APPLY_FACTOR
!     
      REAL*4   L_RESERVE_CAPACITY_DOLLARS(4),
     +         L_SUPPLEMENTAL_CAP_DOLLARS(4),
     + 	      L_UNADJ_PURCHASE_CAP_DOLLARS(4)
      SAVE  L_RESERVE_CAPACITY_DOLLARS,L_SUPPLEMENTAL_CAP_DOLLARS,
     + 	   L_UNADJ_PURCHASE_CAP_DOLLARS 
!
! END OF DATA DECLARATIONS
!
            DUKE_CAPACITY_CHARGES = -999.
         RETURN
!
C***********************************************************************
C		ENTRY ADJ_RESERVE_CAP_DOLLARS(R_COMPANY)
C***********************************************************************
C			L_RESERVE_CAPACITY_DOLLARS(R_COMPANY) = 
C     +			RESERVE_CAPACITY_MW(R_COMPANY) * 
C     +			RESERVE_CAPACITY_RATE(R_COMPANY) 
C     		   IF(OEI_RESERVE_CAPACITY_DOLLARS(R_COMPANY) /= 0.) THEN
C				ADJ_RESERVE_CAP_DOLLARS =
C     +				OEI_RESERVE_CAPACITY_DOLLARS(R_COMPANY) +
C     +				PEAK_REVISION_TRUE_UPS(R_COMPANY)
C            ELSE
C				ADJ_RESERVE_CAP_DOLLARS =
C     +				L_RESERVE_CAPACITY_DOLLARS(R_COMPANY) +
C     +				PEAK_REVISION_TRUE_UPS(R_COMPANY)
C           	ENDIF
C		RETURN	
!        
C***********************************************************************
		ENTRY ADJ_SUPPLEMENTAL_CAP_DOLLARS(R_COMPANY)
C***********************************************************************
C            IF(R_COMPANY < 3) THEN                      
C               APPLY_FACTOR = A_FACTOR(R_COMPANY)
C            ELSE
C               APPLY_FACTOR = B_FACTOR(R_COMPANY)
C            ENDIF
C            L_SUPPLEMENTAL_CAP_DOLLARS(R_COMPANY) = 
C     +				SUPPLEMENTAL_CAPACITY(R_COMPANY) *
C     +				SUPPLEMENTAL_CAPACITY_RATE(R_COMPANY) *
C     +   			APPLY_FACTOR	
C            IF(OEI_SUPPLEMENTAL_CAP_DOLLARS(R_COMPANY) /= 0.) THEN
C               ADJ_SUPPLEMENTAL_CAP_DOLLARS = 
C     +            OEI_SUPPLEMENTAL_CAP_DOLLARS(R_COMPANY) + 
C     +            SUPPLEMENTAL_TRUE_UPS(R_COMPANY)
C            ELSE
C               ADJ_SUPPLEMENTAL_CAP_DOLLARS = 
C     +            L_SUPPLEMENTAL_CAP_DOLLARS(R_COMPANY) +
C     +            SUPPLEMENTAL_TRUE_UPS(R_COMPANY)
C            ENDIF
C       	RETURN
C***********************************************************************
C		ENTRY ADJ_MCGUIRE_X_CAP_DOLLARS(R_COMPANY)
C***********************************************************************
C		   ADJ_MCGUIRE_X_CAP_DOLLARS = 
C     +				MCGUIRE_X_CAPACITY(R_COMPANY) *
C     +				MCGUIRE_X_CAPACITY_RATE(R_COMPANY) 
C       	RETURN
C***********************************************************************
C		ENTRY ADJ_PURCHASE_CAP_DOLLARS(R_COMPANY)
C***********************************************************************
C        	   L_UNADJ_PURCHASE_CAP_DOLLARS(R_COMPANY) = 
C     +              PURCHASED_CAPACITY_MW(R_COMPANY) *
C     +				PURCHASED_CAPACITY_RATE(R_COMPANY)
C     		   IF(OEI_PURCHASED_CAP_PAYMENTS(R_COMPANY) > 0.) THEN
C				ADJ_PURCHASE_CAP_DOLLARS =
C     +				OEI_PURCHASED_CAP_PAYMENTS(R_COMPANY) +
C     +				PURCHASE_CAP_TRUE_UPS(R_COMPANY)
C            ELSE
C				ADJ_PURCHASE_CAP_DOLLARS =
C     +				L_UNADJ_PURCHASE_CAP_DOLLARS(R_COMPANY) +
C     +				PURCHASE_CAP_TRUE_UPS(R_COMPANY) -
C     +				PURCHASE_INTEREST_CREDIT(R_COMPANY)
C            ENDIF
C		RETURN
C***********************************************************************
C         ENTRY RESERVE_CAPACITY_DOLLARS(R_COMPANY)
C***********************************************************************
C			RESERVE_CAPACITY_DOLLARS = 
C     +                             L_RESERVE_CAPACITY_DOLLARS(R_COMPANY)
C         RETURN
C***********************************************************************
C         ENTRY SUPPLEMENTAL_CAP_DOLLARS(R_COMPANY)
C***********************************************************************
C            SUPPLEMENTAL_CAP_DOLLARS = 
C     +                             L_SUPPLEMENTAL_CAP_DOLLARS(R_COMPANY)
C         RETURN
C***********************************************************************
C         ENTRY UNADJ_PURCHASE_CAP_DOLLARS(R_COMPANY)
C***********************************************************************
C        	   UNADJ_PURCHASE_CAP_DOLLARS = 
C     +      	                  L_UNADJ_PURCHASE_CAP_DOLLARS(R_COMPANY)
C         RETURN
C      END
C***********************************************************************
C
C                ROUTINE TO CONVERT CATAWBA WHEELING CHARGES
C
C                             COPYRIGHT (C) 1996
C                        M.S. GERBER & ASSOCIATES, INC.
C                             ALL RIGHTS RESERVED
C
C***********************************************************************
C
C      FUNCTION DUKE_WHEELING_CHARGES()
C      LOGICAL*1 DUKE_WHEELING_CHARGES
C      INTEGER*2 R_COMPANY
C      REAL*4
C     +         S_DUKE_WHEELING_RATE(4),
C     +         DUKE_WHEELING_RATE,
!
C     +         DUKE_WHEELING_COSTS,
C     +         FIXED_TRANSMISSION_OM,
C     +         FIXED_TRANSMISSION_AG,
C     +         TRANSMISSION_ROI,
C     +         TRANSMISSION_INCOME_TAXES,
C     +         TRANSMISSION_DEPRECIATION,
C     +         TRANS_OTHER_TAXES,
C     +         TRANS_RENTS,
C     +         OTHER_WHEELING_REVENUES,
C     +         TRANSMISSION_TAX_ADJ_FACTOR,
C     +         DEMAND_ON_TRANSMISSION_SYSTEM
C      SAVE S_DUKE_WHEELING_RATE
!      
! END DATA DECLARATIONS      
!
C         DUKE_WHEELING_CHARGES = .TRUE.
C      RETURN
C***********************************************************************
C      ENTRY DUKE_WHEELING_COSTS(R_COMPANY)
C***********************************************************************
C         DUKE_WHEELING_COSTS =
C     +         FIXED_TRANSMISSION_OM(R_COMPANY) +
C     +         FIXED_TRANSMISSION_AG(R_COMPANY) +
C     +         TRANSMISSION_ROI(R_COMPANY) +
C     +         TRANSMISSION_INCOME_TAXES(R_COMPANY) +
C     +         TRANSMISSION_DEPRECIATION(R_COMPANY) +
C     +         TRANS_OTHER_TAXES(R_COMPANY) +
C     +         TRANS_RENTS(R_COMPANY) +
C     +         OTHER_WHEELING_REVENUES(R_COMPANY)
C         S_DUKE_WHEELING_RATE(R_COMPANY) = 
C     +         TRANSMISSION_TAX_ADJ_FACTOR(R_COMPANY) *
C     +         DUKE_WHEELING_COSTS /
C     +                          DEMAND_ON_TRANSMISSION_SYSTEM(R_COMPANY)
!     
C      RETURN
C***********************************************************************
C      ENTRY DUKE_WHEELING_RATE(R_COMPANY)
C***********************************************************************
C         DUKE_WHEELING_RATE = S_DUKE_WHEELING_RATE(R_COMPANY)
C      RETURN
      END
C***********************************************************************
C
C                ROUTINE TO CONVERT CATAWBA ENERGY CHARGES
C
C                             COPYRIGHT (C) 1996
C                        M.S. GERBER & ASSOCIATES, INC.
C                             ALL RIGHTS RESERVED
C
C***********************************************************************
C
      FUNCTION DUKE_RELIABILITY_EXCHANGE()
C***********************************************************************
C
      LOGICAL*1 DUKE_RELIABILITY_EXCHANGE
      REAL CALCULATED_SECTION_82_CAP_RATE
      REAL BUYERS_SECTION_82_ENRG_RATE,BUYERS_ENRG_COSTS
      REAL SECTION_102_RATE,DUKE_ENRG_COSTS,DUKE_ENRG
      SAVE SECTION_102_RATE
      REAL BUYERS_ENRG,KWH_SALES_TAX,REVENUE_TAX_RATE
      REAL MCGUIRE_ENRG,MCGUIRE_FUEL_COST,MCGUIRE_OM_COST,
     +     MCGUIRE_FIXED_OM_COST
      SAVE MCGUIRE_ENRG,MCGUIRE_FUEL_COST,MCGUIRE_OM_COST,
     +     MCGUIRE_FIXED_OM_COST
      REAL MCGUIRE_ENRG_RATE,MCGUIRE_CAPPED_RATE,MCGUIRE_FUEL_RATE
      SAVE MCGUIRE_ENRG_RATE,MCGUIRE_CAPPED_RATE,MCGUIRE_FUEL_RATE
      REAL CATAWBA_ENRG,CATAWBA_FUEL_COST,CATAWBA_OM_COST,
     +     CATAWBA_FIXED_OM_COST
      SAVE CATAWBA_ENRG,CATAWBA_FUEL_COST,CATAWBA_OM_COST,
     +     CATAWBA_FIXED_OM_COST
      REAL CATAWBA_ENRG_RATE,CATAWBA_CAPPED_RATE,CATAWBA_FUEL_RATE
      SAVE CATAWBA_ENRG_RATE,CATAWBA_CAPPED_RATE,CATAWBA_FUEL_RATE
      REAL FUEL_RATE,OM_EXPENSE,ADJUSTED_GENERATION,OM_RATE,DERIVED_RATE
      SAVE FUEL_RATE,OM_EXPENSE,ADJUSTED_GENERATION,OM_RATE,DERIVED_RATE
      REAL IC_IV_1_LINE_7,SECTION_82_RATE,ANNUAL_ENERGY_CHARGE,
     +     ANNUAL_ENERGY_CHARGE_WITH_TAXES,ADJUSTMENT_2_TAXES
      SAVE SECTION_82_RATE,ANNUAL_ENERGY_CHARGE,
     +     ANNUAL_ENERGY_CHARGE_WITH_TAXES,ADJUSTMENT_2_TAXES
      REAL MCGUIRE_2_CATAWBA_ENRG_RATIO
      LOGICAL*1 INITIALIZE_DUKE_ROUTINES,DUKE,
     +          DUKE_CATAWBA_MCGUIRE_RESULTS
      INTEGER*2 R_UNIT_ID
      REAL*4 R_UNIT_ENRG,
     +       R_UNIT_FUEL_COST,
     +       R_UNIT_OM_COST,
     +       R_UNIT_FIXED_COST
      CHARACTER*20 UNIT_NAME,SPECIAL_ID_NAME
      REAL*4 JOINT_OWNERS_COSTS
C
         DUKE_RELIABILITY_EXCHANGE = .TRUE.
      RETURN
C***********************************************************************
      ENTRY INITIALIZE_DUKE_ROUTINES()
C***********************************************************************
         CATAWBA_ENRG = 0.
         CATAWBA_FUEL_COST = 0.
         CATAWBA_OM_COST = 0.
         CATAWBA_FIXED_OM_COST = 0.
         MCGUIRE_ENRG = 0.
         MCGUIRE_FUEL_COST = 0.
         MCGUIRE_OM_COST = 0.
         MCGUIRE_FIXED_OM_COST = 0.
         INITIALIZE_DUKE_ROUTINES = DUKE()
      RETURN
C***********************************************************************
      ENTRY DUKE_CATAWBA_MCGUIRE_RESULTS(R_UNIT_ID,
     +                                   R_UNIT_ENRG,
     +                                   R_UNIT_FUEL_COST,
     +                                   R_UNIT_OM_COST,
     +                                   R_UNIT_FIXED_COST)
C***********************************************************************
C
         UNIT_NAME = SPECIAL_ID_NAME(R_UNIT_ID)
         DUKE_CATAWBA_MCGUIRE_RESULTS = .FALSE.
         IF(INDEX(UNIT_NAME,'Catawba') /= 0) THEN
            CATAWBA_ENRG = CATAWBA_ENRG + R_UNIT_ENRG
            CATAWBA_FUEL_COST = CATAWBA_FUEL_COST + R_UNIT_FUEL_COST
            CATAWBA_OM_COST = CATAWBA_OM_COST + R_UNIT_OM_COST
            CATAWBA_FIXED_OM_COST = CATAWBA_FIXED_OM_COST +
     +                                                 R_UNIT_FIXED_COST
            DUKE_CATAWBA_MCGUIRE_RESULTS = .TRUE.
         ELSEIF(INDEX(UNIT_NAME,'McGuire') /= 0) THEN
            MCGUIRE_ENRG = MCGUIRE_ENRG + R_UNIT_ENRG
            MCGUIRE_FUEL_COST = MCGUIRE_FUEL_COST + R_UNIT_FUEL_COST
            MCGUIRE_OM_COST = MCGUIRE_OM_COST + R_UNIT_OM_COST
            MCGUIRE_FIXED_OM_COST = MCGUIRE_FIXED_OM_COST +
     +                                                 R_UNIT_FIXED_COST
            DUKE_CATAWBA_MCGUIRE_RESULTS = .TRUE.
         ENDIF
      RETURN
C***********************************************************************
      ENTRY BUYERS_SECTION_82_ENRG_RATE()
C***********************************************************************
C
C FORM IC-IV-1
C
         IF(MCGUIRE_ENRG /= 0.) THEN
            MCGUIRE_ENRG_RATE = (MCGUIRE_FUEL_COST + MCGUIRE_OM_COST)/
     +                                      MCGUIRE_ENRG
            MCGUIRE_FUEL_RATE = MCGUIRE_FUEL_COST/MCGUIRE_ENRG
         ELSE
            MCGUIRE_ENRG_RATE = 0.
            MCGUIRE_FUEL_RATE = 0.
         ENDIF
C         
         IF(CATAWBA_ENRG /= 0.) THEN
            CATAWBA_ENRG_RATE = (CATAWBA_FUEL_COST + CATAWBA_OM_COST)/
     +                                      CATAWBA_ENRG
            CATAWBA_FUEL_RATE = CATAWBA_FUEL_COST/CATAWBA_ENRG
            MCGUIRE_2_CATAWBA_ENRG_RATIO = MCGUIRE_ENRG/CATAWBA_ENRG 
         ELSE
            CATAWBA_ENRG_RATE = 0.
            CATAWBA_FUEL_RATE = 0.
            MCGUIRE_2_CATAWBA_ENRG_RATIO = 999.
         ENDIF
C
         IF(MCGUIRE_ENRG == 0.) THEN
            MCGUIRE_ENRG_RATE = CATAWBA_ENRG_RATE
            MCGUIRE_FUEL_RATE = CATAWBA_FUEL_RATE
         ENDIF
         IF(CATAWBA_ENRG == 0.) THEN
            CATAWBA_ENRG_RATE = MCGUIRE_ENRG_RATE
            CATAWBA_FUEL_RATE = MCGUIRE_FUEL_RATE
         ENDIF
         MCGUIRE_CAPPED_RATE = 1.25 * MCGUIRE_ENRG_RATE
         CATAWBA_CAPPED_RATE = 1.25 * CATAWBA_ENRG_RATE
C
         IF(MCGUIRE_2_CATAWBA_ENRG_RATIO < .65) THEN
            FUEL_RATE = MCGUIRE_FUEL_RATE
            OM_EXPENSE = MCGUIRE_OM_COST
            ADJUSTED_GENERATION = .65*CATAWBA_ENRG
            OM_RATE = OM_EXPENSE/ADJUSTED_GENERATION
            DERIVED_RATE = FUEL_RATE + OM_RATE
         ELSE   
            FUEL_RATE = CATAWBA_FUEL_RATE
            OM_EXPENSE = CATAWBA_OM_COST
            ADJUSTED_GENERATION = .65*MCGUIRE_ENRG
            OM_RATE = OM_EXPENSE/ADJUSTED_GENERATION
            IF(MCGUIRE_2_CATAWBA_ENRG_RATIO <= 1./.65) THEN
               DERIVED_RATE = 0.
            ELSE
               DERIVED_RATE = FUEL_RATE + OM_RATE
            ENDIF
         ENDIF
C
         IC_IV_1_LINE_7 = MIN(DERIVED_RATE,MCGUIRE_CAPPED_RATE)
         IF(MCGUIRE_2_CATAWBA_ENRG_RATIO < 1./.80) THEN
            SECTION_82_RATE = MAX(CATAWBA_ENRG_RATE,MCGUIRE_ENRG_RATE)
         ELSEIF (MCGUIRE_2_CATAWBA_ENRG_RATIO <= 1./.65) THEN
            SECTION_82_RATE = MAX(MCGUIRE_ENRG_RATE,
     +                       MIN(CATAWBA_ENRG_RATE,MCGUIRE_CAPPED_RATE))
         ELSE
            SECTION_82_RATE = MAX(MCGUIRE_ENRG_RATE,IC_IV_1_LINE_7)
         ENDIF
         BUYERS_SECTION_82_ENRG_RATE = SECTION_82_RATE
      RETURN
C***********************************************************************
      ENTRY JOINT_OWNERS_COSTS()
C***********************************************************************
         JOINT_OWNERS_COSTS = .875*CATAWBA_FUEL_COST/1000000. 
      RETURN         
C***********************************************************************
      ENTRY BUYERS_ENRG_COSTS(BUYERS_ENRG,
     +                                   KWH_SALES_TAX,REVENUE_TAX_RATE)
C***********************************************************************
C
         ANNUAL_ENERGY_CHARGE = BUYERS_ENRG * SECTION_82_RATE
         ANNUAL_ENERGY_CHARGE_WITH_TAXES = (ANNUAL_ENERGY_CHARGE +
     +                                     BUYERS_ENRG * KWH_SALES_TAX)/
     +                                        (1.-REVENUE_TAX_RATE)
         ADJUSTMENT_2_TAXES = ANNUAL_ENERGY_CHARGE_WITH_TAXES -
     +                                              ANNUAL_ENERGY_CHARGE
         BUYERS_ENRG_COSTS = ANNUAL_ENERGY_CHARGE_WITH_TAXES
      RETURN   
C***********************************************************************
      ENTRY CALCULATED_SECTION_82_CAP_RATE()
C***********************************************************************
C
         CALCULATED_SECTION_82_CAP_RATE = 0.
      RETURN
C***********************************************************************
      ENTRY DUKE_ENRG_COSTS(DUKE_ENRG,KWH_SALES_TAX,REVENUE_TAX_RATE)
C***********************************************************************
C
C FORM IC-IV-2
C
         IF(MCGUIRE_2_CATAWBA_ENRG_RATIO < .65) THEN
            SECTION_102_RATE = MAX(CATAWBA_ENRG_RATE,
     +                            MIN(DERIVED_RATE,CATAWBA_CAPPED_RATE))
         ELSEIF (MCGUIRE_2_CATAWBA_ENRG_RATIO <= .8) THEN
            SECTION_102_RATE = MAX(CATAWBA_ENRG_RATE,
     +                       MIN(MCGUIRE_ENRG_RATE,CATAWBA_CAPPED_RATE))
         ELSE
            SECTION_102_RATE = MAX(CATAWBA_ENRG_RATE,MCGUIRE_ENRG_RATE)
         ENDIF
         DUKE_ENRG_COSTS = DUKE_ENRG*(SECTION_102_RATE + KWH_SALES_TAX)/
     +                                  (1.-REVENUE_TAX_RATE)
      RETURN
      END
C***********************************************************************
C
C     END MARK'S SECTION OF THE CODE
C
C***********************************************************************
 