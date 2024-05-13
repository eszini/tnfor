subroutine seed_clreport(year, BASE_YEAR, iseas, CL_MONTH_NAME, I, MULTI_AREA_NAME, MAX_LOAD_TYPES, &
    MFT1)
integer :: upb, upb2
integer*2 :: iseas, I, MAX_LOAD_TYPES, MFT1
real :: year, base_year
character*9 :: CL_MONTH_NAME(14)
character*35, allocatable ::  multi_area_name(:)
integer :: i_index, j_index, out_index
character*255 :: text_output_filename="clreport_summing.log"
character*255 :: index_num_str


            if(clreport_seed_with_sample_data) then

! *****************  Seed report with primary mover values
                PROD_BY_TG_BY_MW=666
                prod_by_tg_by_mwh=777
                load_by_tg_by_mwh=888
                mw_by_tg_by_fuel=999
                mwh_by_tg_by_fuel=101010

                upb=ubound(PROD_BY_TG_BY_MW,1)
                prod_by_tg_by_mw(1:upb, pm_ba)=pm_ba
                prod_by_tg_by_mw(1:upb, pm_default)=pm_default
                prod_by_tg_by_mw(1:upb, pm_ow)=pm_ow ! Offshore Wind
                prod_by_tg_by_mw(1:upb, pm_hydro)=pm_hydro
                prod_by_tg_by_mw(1:upb, pm_nu)=pm_nu
                prod_by_tg_by_mw(1:upb, pm_cc)=pm_cc
                prod_by_tg_by_mw(1:upb, pm_cs)=pm_cs
                prod_by_tg_by_mw(1:upb, pm_hb)=pm_hb
                prod_by_tg_by_mw(1:upb, pm_h2)=pm_h2
                prod_by_tg_by_mw(1:upb, pm_fc)=pm_fc
                prod_by_tg_by_mw(1:upb, pm_ba)=pm_ba              
                prod_by_tg_by_mw(1:upb, pm_ge)=pm_ge ! Geothermal
                prod_by_tg_by_mw(1:upb, pm_gt)=pm_gt ! Combustion Turbine
                prod_by_tg_by_mw(1:upb, pm_hydro)=pm_hydro
                prod_by_tg_by_mw(1:upb, pm_ic)=pm_ic ! Internal Combustion
                prod_by_tg_by_mw(1:upb, pm_na)=pm_na
                prod_by_tg_by_mw(1:upb, pm_nu)=pm_nu
                prod_by_tg_by_mw(1:upb, pm_sl)=pm_sl !Solar
                prod_by_tg_by_mw(1:upb, pm_st)=pm_st
                prod_by_tg_by_mw(1:upb, pm_wt)=pm_wt ! Wind Turbine
                prod_by_tg_by_mw(1:upb, pm_zz)=pm_zz ! Landfill Gas?
                prod_by_tg_by_mw(1:upb, pm_dg)=pm_dg
                prod_by_tg_by_mw(1:upb, pm_lm1_steam)=pm_lm1_steam
                prod_by_tg_by_mw(1:upb, pm_lm2_steam)=pm_lm2_steam
                prod_by_tg_by_mw(1:upb, pm_lm3_steam)=pm_lm3_steam
                
                

                upb=ubound(PROD_BY_TG_BY_MWh,1)
                prod_by_tg_by_mwh(1:upb, pm_default)=pm_default*10
                prod_by_tg_by_mwh(1:upb, pm_ba)=pm_ba*10
                prod_by_tg_by_mwh(1:upb, pm_ow)=pm_ow*10
                prod_by_tg_by_mwh(1:upb, pm_hydro)=pm_hydro*10
                prod_by_tg_by_mwh(1:upb, pm_nu)=pm_nu*10
                prod_by_tg_by_mwh(1:upb, pm_cc)=pm_cc*10
                prod_by_tg_by_mwh(1:upb, pm_cs)=pm_cs*10
                prod_by_tg_by_mwh(1:upb, pm_hb)=pm_hb*10
                prod_by_tg_by_mwh(1:upb, pm_h2)=pm_h2*10
                prod_by_tg_by_mwh(1:upb, pm_fc)=pm_fc*10
                prod_by_tg_by_mwh(1:upb, pm_ge)=pm_ge*10
                prod_by_tg_by_mwh(1:upb, pm_gt)=pm_gt*10
                prod_by_tg_by_mwh(1:upb, pm_ic)=pm_ic*10
                prod_by_tg_by_mwh(1:upb, pm_na)=pm_na*10
                prod_by_tg_by_mwh(1:upb, pm_sl)=pm_sl*10
                prod_by_tg_by_mwh(1:upb, pm_st)=pm_st*10
                prod_by_tg_by_mwh(1:upb, pm_wt)=pm_wt*10
                prod_by_tg_by_mwh(1:upb, pm_zz)=pm_zz*10 ! Landfill Gas?
                prod_by_tg_by_mwh(1:upb, pm_dg)=pm_dg*10
                prod_by_tg_by_mwh(1:upb, pm_lm1_steam)=pm_lm1_steam*10
                prod_by_tg_by_mwh(1:upb, pm_lm2_steam)=pm_lm2_steam*10
                prod_by_tg_by_mwh(1:upb, pm_lm2_steam)=pm_lm2_steam*10
                
                
                
                ! Second report
                upb=ubound(mw_by_tg_by_fuel, 1)
                mw_by_tg_by_fuel(1:upb, pm_default)=pm_default
                mw_by_tg_by_fuel(1:upb, pm_ba)=pm_ba
                mw_by_tg_by_fuel(1:upb, pm_bi)=pm_bi
                mw_by_tg_by_fuel(1:upb, pm_cc)=pm_cc
                mw_by_tg_by_fuel(1:upb, pm_cs)=pm_cs
                mw_by_tg_by_fuel(1:upb, pm_dg)=pm_dg
                mw_by_tg_by_fuel(1:upb, pm_fc)=pm_fc
                mw_by_tg_by_fuel(1:upb, pm_ge)=PM_GE
                mw_by_tg_by_fuel(1:upb, pm_gt)=pm_gt
                mw_by_tg_by_fuel(1:upb, pm_h2)=pm_h2
                mw_by_tg_by_fuel(1:upb, pm_hb)=pm_hb
                mw_by_tg_by_fuel(1:upb, pm_hydro)=pm_hydro
                mw_by_tg_by_fuel(1:upb, pm_ic)=pm_ic
                mw_by_tg_by_fuel(1:upb, pm_lf)=pm_lf
                mw_by_tg_by_fuel(1:upb, pm_na)=pm_na
                mw_by_tg_by_fuel(1:upb, pm_ba)=pm_ba
                mw_by_tg_by_fuel(1:upb, pm_nu)=pm_nu
                mw_by_tg_by_fuel(1:upb, pm_ow)=pm_ow
                mw_by_tg_by_fuel(1:upb, pm_sl)=pm_sl
                mw_by_tg_by_fuel(1:upb, pm_st)=pm_st
                mw_by_tg_by_fuel(1:upb, pm_wt)=pm_wt
                mw_by_tg_by_fuel(1:upb, pm_zz)=pm_zz
                mw_by_tg_by_fuel(1:upb, pm_lm1_steam)=pm_lm1_steam
                mw_by_tg_by_fuel(1:upb, pm_lm2_steam)=pm_lm2_steam
                mw_by_tg_by_fuel(1:upb, pm_lm3_steam)=pm_lm3_steam
                
                upb=ubound(mwh_by_tg_by_fuel, 1)
                mwh_by_tg_by_fuel(1:upb,pm_default)=pm_default*100
                mwh_by_tg_by_fuel(1:upb,pm_ba)=pm_ba*100
                mwh_by_tg_by_fuel(1:upb, pm_bi)=pm_bi*100
                mwh_by_tg_by_fuel(1:upb, pm_cc)=pm_cc*100
                mwh_by_tg_by_fuel(1:upb, pm_cs)=pm_cs*100
                mwh_by_tg_by_fuel(1:upb, pm_dg)=pm_dg*100
                mwh_by_tg_by_fuel(1:upb, pm_fc)=pm_fc*100
                mwh_by_tg_by_fuel(1:upb, pm_ge)=PM_GE*100
                mwh_by_tg_by_fuel(1:upb, pm_gt)=pm_gt*100
                mwh_by_tg_by_fuel(1:upb, pm_h2)=pm_h2*100
                mwh_by_tg_by_fuel(1:upb, pm_hb)=pm_hb*100
                mwh_by_tg_by_fuel(1:upb, pm_hydro)=pm_hydro*100
                mwh_by_tg_by_fuel(1:upb, pm_ic)=pm_ic*100
                mwh_by_tg_by_fuel(1:upb, pm_lf)=pm_lf*100
                mwh_by_tg_by_fuel(1:upb, pm_na)=pm_na*100
                mwh_by_tg_by_fuel(1:upb, pm_ba)=pm_ba*100
                mwh_by_tg_by_fuel(1:upb, pm_nu)=pm_nu*100
                mwh_by_tg_by_fuel(1:upb, pm_ow)=pm_ow*100
                mwh_by_tg_by_fuel(1:upb, pm_sl)=pm_sl*100
                mwh_by_tg_by_fuel(1:upb, pm_st)=pm_st*100
                mwh_by_tg_by_fuel(1:upb, pm_wt)=pm_wt*100
                mwh_by_tg_by_fuel(1:upb, pm_zz)=pm_zz*100
                mwh_by_tg_by_fuel(1:upb, pm_lm1_steam)=pm_lm1_steam*100
                mwh_by_tg_by_fuel(1:upb, pm_lm2_steam)=pm_lm2_steam*100
                mwh_by_tg_by_fuel(1:upb, pm_lm3_steam)=pm_lm3_steam*100


                upb=ubound(PROD_BY_TG_BY_MW, 1)
                upb2=ubound(PROD_BY_TG_BY_MW,2)
                
                prod_by_tg_by_mw(1:upb, pm_ba)=pm_ba
                prod_by_tg_by_mwh(1:upb,pm_ba)=pm_ba*10
                prod_by_tg_by_mwh(1:upb, pm_lm1_steam)=pm_lm1_steam*10
                prod_by_tg_by_mwh(1:upb, pm_lm2_steam)=pm_lm2_steam*10
                prod_by_tg_by_mwh(1:upb, pm_lm3_steam)=pm_lm3_steam*10

                
                prod_by_tg_by_mwh(1:upb,pm_ba)=pm_ba*10
                
                if(pm_ba>upb2) then
                    er_message="pm_ba is > upper bound of prod_by_tg_by_mw (second shape rank.)."
                    call end_program(er_message);

                    mwh_by_tg_by_fuel=19
                    mwh_by_tg_by_fuel(1:upb, pm_lm1_steam)=pm_lm1_steam*100
                    mwh_by_tg_by_fuel(1:upb, pm_lm2_steam)=pm_lm2_steam*100
                    mwh_by_tg_by_fuel(1:upb, pm_lm3_steam)=pm_lm3_steam*100
                
                    prod_by_tg_by_mwh(1:upb, pm_ba)=pm_ba*10

                endif

    
                
                                
! ****************    End of seed report

       ! Block of debugging code that must be removed before production
       if(.false.) then
           if (.not. text_output_file_open) then
               text_output_file_handle=get_new_unit()
               open(text_output_file_handle, file=text_output_filename, status="replace", form="formatted")
               text_output_file_open=.true.
           endif
       
           ! logging data ...
           WRITE(text_output_file_handle, *) "Endpoint: ", PRT_ENDPOINT()
           write(text_output_file_handle, *) "Year: ", REAL(YEAR+BASE_YEAR)
           write(text_output_file_handle, *) "Month_Name: " , CL_MONTH_NAME(ISEAS)
           write(text_output_file_handle, *) "Multi-Area Name: ",  MULTI_AREA_NAME(I)
            
            out_index=0
            
            do j_index=1,MAX_PROD_TYPES
                out_index=out_index+1
                index_num_str=""
                write(index_num_str, *) out_index
                index_num_str=trim(index_num_str) // ": "
                
                write(text_output_file_handle, *) &
                trim(index_num_str), " prod_by_tg_by_mw: ",pm2str(j_index), prod_by_tg_by_mw(I, j_index)
            ENDDO
            
            do j_index=1, MAX_PROD_TYPES
                out_index=out_index+1
                
                index_num_str=""
                write(index_num_str, *) out_index
                index_num_str=trim(index_num_str) // ":"
                write(text_output_file_handle, *) &
                trim(index_num_str), "prod_by_tg_by_mwh: ",pm2str(j_index), prod_by_tg_by_mwh(I, j_index)
            ENDDO
            
            out_index=out_index+1
            index_num_str=""
            write(index_num_str, *) out_index
            index_num_str=trim(index_num_str) // ":"
            write(text_output_file_handle, *) trim(index_num_str), 2.0
            
            do j_index=1, MAX_LOAD_TYPES
                out_index=out_index+1
                index_num_str=""
                write(index_num_str, *) out_index
                write(text_output_file_handle, *) trim(index_num_str), "LOAD_BY_TG_BY_MWH: ", pm2str(j_index), load_by_tg_by_mwh(i, j_index)
            ENDDO
            
            out_index=out_index+1
            index_num_str=""
            write(index_num_str, *) out_index
            write(text_output_file_handle, *) trim(index_num_str), "TG_EffectiveCapacity:", TG_EffectiveCapacity(I, ISEAS)
            
            out_index=out_index+1
            index_num_str=""
            write(index_num_str, *) out_index
            write(text_output_file_handle, *) trim(index_num_str), "TG_CapacitySupplyObligation: ", TG_CapacitySupplyObligation(I, ISEAS)
            
            out_index=out_index+1
            index_num_str=""
            write(index_num_str, *) out_index
            write(text_output_file_handle, *) trim(index_num_str), "TG_ResourceActualPreference:", TG_ResourceActualPreference
            
            write(text_output_file_handle, *) "MSGPROFL DATA-->:"
            out_index=0
            upb=ubound(mw_by_tg_by_fuel, 1)
            do j_index=1, upb
                out_index=out_index+1
                write(index_num_str, *) out_index
                write(text_output_file_handle, *) &
                    trim(index_num_str)//" ", "MW_BY_TG_BY_FUEL: ", MW_BY_TG_BY_FUEL(i, j_index)
            enddo
            
            do j_index=1, MAX_PROD_TYPES
                out_index=out_index+1
                
                index_num_str=""
                write(index_num_str, *) out_index
                index_num_str=trim(index_num_str) // ":"
                write(text_output_file_handle, *) &
                trim(index_num_str), "prod_by_tg_by_mwh: ",pm2str(j_index), prod_by_tg_by_mwh(I, j_index)
            ENDDO
            

            do j_index=1, MFT1
                out_index=out_index+1
                index_num_str=""
                write(index_num_str, *) out_index
                write(text_output_file_handle, *) trim(index_num_str) // " ", "mwh_by_tg_by_fuel: (",i,j_index,"),", MWH_BY_TG_BY_FUEL(i, j_index)
            enddo
            
            do j_index=1, MFT1
                out_index=out_index+1
                index_num_str=""
                write(index_num_str, *) out_index
                write(text_output_file_handle, *) trim(index_num_str)// " ", "mwh_by_tg_by_fuel: (",i,j_index,"),", MWH_BY_TG_BY_FUEL(i, j_index)
            enddo
                                
            write(text_output_file_handle, *) "<-- End of MSGPROFL DATA"


   
       
        endif ! if .true|.false (defines block of debugging code that must be removed before production)
    endif

end subroutine seed_clreport
