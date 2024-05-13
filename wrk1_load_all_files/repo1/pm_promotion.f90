        module pm_promotion

            use mod_fuel_types
            implicit none
            contains
              function promote_pm(pm)
              integer*2 :: promote_pm
              integer*2, intent(in) :: pm
              promote_pm=pm
!             ISSUE: promoting pm to pm used by OW - definitely wrong
              
!               IF(PM == 16) THEN
!                  promote_PM = 18 ! BATTERY
!               ELSEIF(PM == 17) THEN
!                  promote_PM = 19 ! DG
!               ENDIF

              end function promote_pm
              
              
              function promote_tft_for_trid_altresult(FT, PM)
              integer*2 :: FT, PM, promote_tft_for_trid_altresult 
              integer*2 :: TFT
              
                 IF(FT == FT_OTHER) THEN
                        IF(PM == 2) THEN
                           TFT = 7
                        ELSEIF(PM == 3) THEN
                           TFT = 8
                        ELSEIF(PM == 7) THEN
                           TFT = 9
                        ELSEIF(PM == 9) THEN
                           TFT = 10
                        ELSEIF(PM == 11) THEN
                           TFT = 11
                        ELSEIF(PM == 12) THEN
                           TFT = 12
                        ELSEIF(PM == 14) THEN
                           TFT = 14
                        ELSEIF(PM == 15) THEN
                           TFT = 15
                        ELSE
                           TFT = 13
                        ENDIF
                ELSE 
                    tft=ft
                endif
              
                  promote_tft_for_trid_altresult=TFT
                  
              end function promote_tft_for_trid_altresult
! TODO: Look at promote_tft_for_trid, to assign only parameters where integer constants currently being used 
!       and/or remove commented-out code.
              function promote_tft_for_trid(FT, PM)


              integer*2 :: FT, PM, promote_tft_for_trid, TFT
              integer*2 :: altresult=0 ! Compare to ensure returns same result as 12/22 code.
              character*1024, dimension(20) :: messages, tmpstr
              integer :: msgindex, msglast=0
              logical :: found_message
              character*256 :: pm_name
              
                                          
              tft=ft
              altresult=promote_tft_for_trid_altresult(FT, PM)


              if(FT==FT_OTHER) then  
                IF(PM == PM_FC) THEN ! 2
                   TFT = 7
                ELSEIF(PM == PM_GE) THEN !3
                   TFT = 8
                ELSEIF(PM == PM_NUCLEAR) THEN !7
                   TFT = 9     !9
                ELSEIF(PM == pm_sl) THEN !9 - Promotes to 10
                   TFT = 10           !10
                ELSEIF(PM == pm_wt) THEN !11
                   TFT = 11          !11
                ELSEIF(PM == pm_zz) THEN !12
                   TFT = 12           !12
                ELSEIF(pm == pm_bi) THEN 
                   TFT = 14 ! BATTERY 14
                ELSEIF(pm == pm_lf) THEN
                   TFT = 15 
                ELSE
                   TFT = 13 ! Default
                ENDIF
            else
				! It should never hit this point. 
             er_message="pm_promotion:0001: promote_tft_for_trid " // &
			    "was called with PM=" // trim(itos(int(PM)))
				call end_program(er_message)
            ENDIF
                
                promote_tft_for_trid=TFT
                
              end function promote_tft_for_trid
              
        end module pm_promotion
