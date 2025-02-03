               allocate (coal_vector_map(-1:maxvectornum))
               allocate (coal_vector_type_is_value(-1:narray), &
                  allocate(allarray(lowb1:upb1))
               allocate(allarray(lowb1:upb1))
                  allocate(allarray(upb1))
               allocate(allarray(upb1))
                  allocate(allarray(upb1))
               allocate(allarray(upb1))
                  allocate(allarray(lowb1:upb1))
               allocate(allarray(lowb1:upb1))
                  allocate(allarray(upb1))
               allocate(allarray(upb1))
                  allocate(allarray(upb1,upb2))
               allocate(allarray(upb1,upb2))
                  allocate(allarray(upb1))
               allocate(allarray(upb1))
                  allocate(allarray(upb1))
               allocate(allarray(upb1))
                  allocate(allarray(lowb1:upb1,lowb2:upb2))
               allocate(allarray(lowb1:upb1,lowb2:upb2))
                  allocate(allarray(upb1))
               allocate(allarray(upb1))
                  allocate(allarray(upb1))
               allocate(allarray(upb1))
                  allocate(allarray(lowb1:upb1,lowb2:upb2))
               allocate(allarray(lowb1:upb1,lowb2:upb2))
                  allocate(allarray(upb1))
               allocate(allarray(upb1))
                  allocate(allarray(upb1))
               allocate(allarray(upb1))
                  allocate(allarray(upb1))
               allocate(allarray(upb1))
                  allocate(allarray(upb1,upb2))
               allocate(allarray(upb1,upb2))
                  allocate(allarray(lowb1:upb1))
               allocate(allarray(lowb1:upb1))
                  allocate(allarray(lowb1:upb1,lowb2:upb2))
               allocate(allarray(lowb1:upb1,lowb2:upb2))
                  allocate(allarray(lowb1:upb1,lowb2:upb2,lowb3:upb3))
               allocate(allarray(lowb1:upb1,lowb2:upb2,lowb3:upb3))
                  allocate(allarray(upb1))
               allocate(allarray(upb1))
                  allocate(allarray(lowb1:upb1,lowb2:upb2))
               allocate(allarray(lowb1:upb1,lowb2:upb2))
                  allocate(allarray(upb1))
               allocate(allarray(upb1))
                  allocate(allarray(upb1))
               allocate(allarray(upb1))
                  allocate(allarray(lowb1:upb1,lowb2:upb2))
               allocate(allarray(lowb1:upb1,lowb2:upb2))
         allocate(dimension_name(dimensions),      &
         allocate(dem_1_ptr(max_unique_values))
         allocate(dem_1_name_storage(max_unique_values))
         allocate(dem_2_ptr(max_unique_values))
         allocate(dem_2_name_storage(max_unique_values))
            allocate(dem_3_ptr(max_unique_values))
            allocate(dem_3_name_storage(max_unique_values))
            allocate(dem_4_ptr(max_unique_values))
            allocate(dem_4_name_storage(max_unique_values))
            allocate(dem_5_ptr(max_unique_values))
            allocate(dem_5_name_storage(max_unique_values))
            allocate(dem_6_ptr(max_unique_values))
            allocate(dem_6_name_storage(max_unique_values))
            allocate(dem_7_ptr(max_unique_values))
            allocate(dem_7_name_storage(max_unique_values))
         allocate(dem_1_ptr(dem_1_unique_values),           &
         allocate(dem_2_ptr(dem_2_unique_values),           & 
            allocate(dem_3_ptr(dem_3_unique_values),        &
            allocate(dem_4_ptr(dem_4_unique_values),        &
            allocate(dem_5_ptr(dem_5_unique_values),        &
            allocate(dem_6_ptr(dem_6_unique_values),        &
            allocate(dem_7_ptr(dem_7_unique_values),        &
         allocate(plan_identifier(0:max_number_of_plans_to_save), stat=i4st)
         allocate(plan_object_function_value(0:max_number_of_plans_to_save), stat=i4st)
         allocate(plan_has_mut_exc_units(0:max_number_of_plans_to_save), stat=i4st)
         allocate(plan_option_sequence(50, max_number_of_plans_to_save), stat=i4st)
         allocate(option_pointr(active_options), stat=i4st)
         allocate(option_ann_units(active_options), stat=i4st)
         allocate(avail_annual_units(active_options), stat=i4st)
         allocate(option_avail_units(active_options), stat=i4st)
         allocate(avail_cum_units(active_options), stat=i4st)
         allocate(option_cap(active_options), stat=i4st)
         allocate(option_loading_type(active_options), stat=i4st)
         allocate(option_is_active(active_options), stat=i4st)
         allocate(plan_being_tested(50), stat=i4st)
               allocate(bad_odd_plans(option_depth,max_bad_plans))
            allocate(bad_even_plans(option_depth,max_bad_plans))
            allocate(depth_best_plans(option_depth, depth_plans_2_save), stat=i4st)
            allocate(depth_plan_objective_value(depth_plans_2_save), stat=i4st)
            allocate(plans_2_test_at_next_depth(option_depth, max_plans_2_test), stat=i4st)
                     allocate(bad_odd_plans(option_depth,max_bad_plans))
                     allocate(   &
                     allocate(list_of_combinations(option_depth-1,   &
                     allocate(list_of_combinations(option_depth-1,   &
                  allocate(depth_best_plans(option_depth, depth_plans_2_save), stat=i4st)
                  allocate(depth_plan_objective_value(depth_plans_2_save), stat=i4st)
                  allocate(plans_2_test_at_next_depth(option_depth, max_plans_2_test), stat=i4st)
                           allocate(temp_bad_array(option_depth, max_bad_plans), stat=i4st)
                              allocate(bad_odd_plans(option_depth,   &
                              allocate(bad_even_plans(option_depth,   &
            allocate(sorted_pointer(number_of_saved_plans))
            allocate(position_matched(option_depth-1))
            allocate(position_matched(option_depth-1))
      allocate(active_option_list(total_options))
         allocate(combination_arrays(depth))
      allocate(dsm_cap(3,study_period))
         allocate(plan_identifier(0:max_number_of_plans_to_save), &
            allocate(active_options(option_depth))
      allocate(dsm_cap(3,study_period))
         allocate(committed_option_pointers(max_committed_options), &
      allocate(dsm_cap(3,study_period))
         allocate(delayed_on_line_date(next_committed_option))
         allocate(temp_i2_array(next_committed_option))
         allocate(committed_option_pointers(max_committed_options))
         allocate(committed_option_times_delayed(max_committed_options))
         allocate(committed_option_start_year(max_committed_options))
         allocate(committed_option_on_line_yr(max_committed_options))
         allocate( &
         allocate(rec_locator(max_committed_options))
         allocate(temp_r4_array(next_committed_option))
         allocate(option_percent_completed(max_committed_options))
         allocate(temp_l1_array(next_committed_option))
         allocate(has_been_accelerated(max_committed_options))
         allocate(temp_i2_array(next_planned_option))
         allocate(planned_option_pointers(max_committed_options))
         allocate(planned_option_start_year(max_committed_options))
         allocate(planned_option_on_line_yr(max_committed_options))
            allocate(recln(2))
            allocate(recln(200))
         allocate(over_cost_levels(max_over_table_rows, &
         allocate(under_cost_levels(max_under_table_rows, &
        allocate(last_resource_added(max(int(1,2),upper_trans_group)), &
            allocate(option_name(total_all_options), &
            allocate(resourceidtooptionposprt(maxproductionprointer))
         allocate(screen_slope(total_all_options), &
         allocate(screen_slope(total_all_options), &
         allocate(multi_area_name(0:upper_trans_group), &
            allocate(mrx_rps_mx_curve_margin(ho,pa), &
         allocate(last_resource_added(max(int(1,2),upper_trans_group)))
            allocate(option_name(total_all_options), &
         allocate(screen_slope(total_all_options), &
         allocate(multi_area_name(0:upper_trans_group), &
         allocate(co2_emissions_cap(0:upper_trans_group), &
    allocate(rps_hydro_db(2,400,numpms,0:12))
    allocate(qualifying_gen_db(active_rps,0:numpms,2))
	allocate(grx_qual_gen_db(active_rps,0:numpms,2))
	allocate(save_grx_qual_gen_db(active_rps,0:numpms,2))
     allocate(dev_tg_cap(0:16,0:max(1,nr_trans_classes), &
     allocate (dev_new_tg_cap(0:21,0:max(1,nr_trans_classes), &
     allocate(saved_dev_new_tg_cap(0:21,0:max(1, &
         allocate(temp_asset_class_pointer(1024))
            allocate(bc_asset_class_pointer(max_bc_class_id_num))
      allocate(temp_asset_class_pointer(1024))
         allocate(ol_asset_class_pointer(max_ol_class_id_num))
            allocate(asset_class_pointer(max_asset_class_num))
         allocate(catawba_revenues(0:num_of_asset_classes, &
         allocate(asset_class_list(avail_data_years), &
            allocate(asset_class_pointer(max_asset_class_num))
         allocate(asset_class_list(avail_data_years), &
      allocate(pa_hourly_loads(hours_increment), &
         allocate(pa_unit_index(no_pa_resources), &
            allocate(ann_pa_energy(0:no_pa_resources+1), &
         allocate(monthly_pa_loads(local_hours))
         allocate(sum_class_peaks(12,2,forecast_growth_years, &
         allocate(class_ann_coin_peak(max_load_classes, &
            allocate(sum_class_peaks(12,2,forecast_growth_years,1))
         allocate(s_fuelmx(cl_options))
         allocate(s_pbtuct(cl_options))
         allocate(s_pfescr(cl_options))
         allocate(s_sbtuct(cl_options))
         allocate(s_rps_program_number(cl_options))
         allocate(s_rps_percent(cl_options))
         allocate(s_sfescr(cl_options))
         allocate(s_fueladj_in(cl_options))
         allocate(s_mnrate(cl_options,12))
         allocate(s_inter_blocks(cl_options,3))
         allocate(s_fueladj(cl_options))
         allocate(s_efor(cl_options))
         allocate(s_vcpmwh_in(cl_options))
         allocate(s_omescr(cl_options))
         allocate(s_fixed_cost_in(cl_options))
         allocate(s_fixed_cost_escalator(cl_options))
         allocate(s_monthly_capacity_pointer(cl_options))
         allocate(s_hr_factor(cl_options))
         allocate(s_input_mw(2,cl_options))
         allocate(s_cap_planning_fac(cl_options))
         allocate(s_coeff(3,cl_options),p_so2(cl_options),p_nox(cl_options),p_particulates(cl_options),p_emis_oth2(cl_options), &
         allocate(ahr(cl_options))
         allocate(prod_pntr_by_cl_units(prod_pntr))
         allocate(s_unitnm(cl_options))
         allocate(ldtype(cl_options))
         allocate(s_annual_cl_fixed_cost(cl_options))
         allocate(s_transaction_group_id(cl_options))
         allocate(s_emission_market_link(cl_options))
         allocate(s_annual_cl_fixed_cost_esc(cl_options))
         allocate(p_fuel_delivery(cl_options))
         allocate(start_up_costs(cl_options))
         allocate(start_up_costs_escalation(cl_options))
         allocate(min_down_time(cl_options))
         allocate(min_up_time(cl_options))
         allocate(p_fuel_delivery_2(cl_options))
         allocate(p_fuel_delivery_3(cl_options))
         allocate(s_fuel_delivery(cl_options))
         allocate(s_fuel_delivery_2(cl_options))
         allocate(s_fuel_delivery_3(cl_options))
         allocate(apply_nox_season_date(cl_options),s_primary_mover(cl_options),s_unit_gas_region_index(cl_options), &
            allocate(cl_tg_cap(0:max_technology_counters, 0:maxvariable, study_period,2))
      allocate(maint_rq_da_dec(n_active_units),stat=i4st)
      allocate(maint_weight(n_active_units),stat=i4st)
      allocate(order_dec(n_active_units),stat=i4st)
      allocate(opt_range_ofs(n_active_units),stat=i4st)
      allocate(opt_range_dur(n_active_units),stat=i4st)
            allocate(hrstostate(0:1,12*max_fo_per_month),stat=i4st)
      allocate(   &
      allocate(so_beg_moda(2,nunits),stat=i4st)
      allocate(so_end_moda(2,nunits),stat=i4st)
      allocate(fo_beg_hr(max_fo_per_month,12,nunits),stat=i4st)
      allocate(fo_end_hr(max_fo_per_month,12,nunits),stat=i4st)
      allocate(pd_beg_hr(max_pd_per_month,12,nunits),stat=i4st)
      allocate(pd_end_hr(max_pd_per_month,12,nunits),stat=i4st)
         allocate(bc_table_position_for(0:max_bc_class_number))
         allocate(ol_table_position_for(0:max_ol_class_number))
            allocate(   &
         allocate(temp_asset_class_pointer(1024))
            allocate(options_bc_asset_class_pointer (max_bc_class_id_num))
      allocate(temp_asset_class_pointer(1024))
         allocate(options_ol_asset_class_pointer (max_ol_class_id_num))
         allocate(escalation_vector(for_all_options), stat=astat)
         allocate(unit_names(for_all_options),loading_type(for_all_options),file_source(for_all_options), &
         allocate(threshold_capacity(for_all_options, &
         if(base_options > 0) allocate(base_option_list(base_options))
         if(cycle_options > 0) allocate(cycl_option_list(cycle_options))
         if(peak_options > 0) allocate(peak_option_list(peak_options))
         if(peak_reduc_options > 0) allocate(load_option_list(peak_reduc_options))
         if(fill_options > 0) allocate(fill_option_list(fill_options))
         if(total_active_options > 0) allocate(active_option_list(total_active_options))
            allocate(hard_wired_list(hard_wired_options),hard_wired_on_line_year(hard_wired_options))
         allocate(save_no_cl_additions(max_simulation_years))
         allocate(save_no_el_additions(max_simulation_years))
            allocate(added_lm_programs(dsm_options))
            allocate(added_contract_programs(contract_options))
            allocate(targets(3,study_period), &
            allocate(targets(3,study_period), &
            allocate (supply_production_by_year(20,narray,0:30), &
            allocate(generictransportptrtotg(0:256,20)) ! tg,1 = paths tg,>1 pointer
               allocate(genericplantptrbytg(0:256))
         allocate(temp_asset_class_pointer(1024))
            allocate(bc_asset_class_pointer(max_bc_class_id_num))
      allocate(temp_asset_class_pointer(1024))
         allocate(ol_asset_class_pointer(max_ol_class_id_num))
      allocate(contract_adjustments_to_peak(study_period), &
      allocate(contract_buy_capacity(0:max_contract_class_id_num), &
      allocate(available_shadow_capacity(max(nunits,int(1,2))))
            allocate(cl_losses(nunits))
            allocate(ct_losses(number_of_contracts))
         allocate(sox_by_fuel_type(3,local_max_cl_units), &
      allocate(frclod(8800),forecast_loads(24,3,12,system_class_num))
         allocate(ext_rr_at_load_point(800))
         allocate(peak_adj_2_off_system_sales(study_period))
      allocate(on_line_month(max_el_units),on_line_year(max_el_units),off_line_month(max_el_units),off_line_year(max_el_units), &
         allocate(el_options_id_num(el_options),el_options_pointr(el_options))
      allocate(peak_adj_2_off_system_sales(study_period))
         if(.not. allocated(temp_retired_unit_no)) allocate(temp_retired_unit_no(20),temp_retired_off_line(20))
         allocate(bc_table_position_for(0:max_bc_class_number))
      allocate(ol_table_position_for(0:max_ol_class_number))
         allocate(envir_class_2_table(0:max_class_num))
         allocate(class_epa_eas(0:max_table_num),   &
         allocate(class_net_ann_emis(number_of_emission_types),   &
         allocate(trans_ror_capacity(0:r_number_of_trans_groups))
         allocate(trans_peak_capacity(0:r_number_of_trans_groups))
         allocate(trans_pump_capacity(0:r_number_of_trans_groups))
            allocate( &
            allocate(mon_mds_el_energy(hydro_units,2,0:12))
            allocate(mon_mds_el_capacity(hydro_units,2,0:12))
            allocate(mon_mds_el_var_cost(hydro_units,2,0:12))
            allocate(mon_mds_el_fixed_cost(hydro_units,2,0:12))
            allocate(mon_mds_el_so2_emis(hydro_units,2,0:12))
            allocate(mon_mds_el_market_revenue(hydro_units,2,0:12))
            allocate(annual_el_energy(hydro_units,2))
            allocate(annual_el_capacity(hydro_units,2))
            allocate(annual_el_var_cost(hydro_units,2))
            allocate(annual_el_fixed_cost(hydro_units,2))
            allocate(annual_el_so2_emis(hydro_units,2))
            allocate(monthly_el_group_report(0:12,0:max_monthly_groups, &
               allocate(fiscal_el_energy(hydro_units,2))
               allocate(fiscal_el_capacity(hydro_units,2))
               allocate(fiscal_el_var_cost(hydro_units,2))
               allocate(fiscal_el_fixed_cost(hydro_units,2))
               allocate(fiscal_el_so2_emis(hydro_units,2))
               allocate(fiscal_el_market_revenue(hydro_units,2))
         allocate(asset_class_num(max_el_units))
         allocate(intra_company_class_id(max_el_units))
         allocate(asset_class_vector(max_el_units))
            allocate(asset_class_pointer(max_hydro_class_num))
         allocate(el_ann_class_var_cost(0:num_of_hydro_classes,4))
         allocate(el_ann_class_fixed_cost(0:num_of_hydro_classes,4))
         allocate(el_ann_class_revenue(0:num_of_hydro_classes,4))
         allocate(el_ann_class_capacity(0:num_of_hydro_classes,4))
         allocate(el_ann_class_energy(0:num_of_hydro_classes,4))
         allocate(el_ann_class_sell_capacity(0:num_of_hydro_classes,4))
         allocate(el_ann_class_sell_energy(0:num_of_hydro_classes,4))
         allocate(el_ann_class_purchases(0:num_of_hydro_classes,4))
         allocate(el_ann_class_so2(0:num_of_hydro_classes))
         allocate(intra_company_sales_revenue(0:1024))
         allocate(intra_company_purchase_expense(0:1024))
         allocate(el_ann_class_market_revenues(0:num_of_hydro_classes))
         allocate(el_ann_class_market_purchase(0:num_of_hydro_classes))
         allocate(el_mon_mds_var_cost(0:num_of_hydro_classes,4,0:12))
         allocate(el_mon_mds_fixed_cost(0:num_of_hydro_classes,4,0:12))
         allocate(el_mon_mds_revenue(0:num_of_hydro_classes,4,0:12))
         allocate(el_mon_mds_capacity(0:num_of_hydro_classes,4,0:12))
         allocate(el_mon_mds_energy(0:num_of_hydro_classes,4,0:12))
         allocate(el_mon_mds_sell_capacity( &
         allocate(el_mon_mds_sell_energy(0:num_of_hydro_classes,4,0:12))
         allocate(el_mon_mds_purchases(0:num_of_hydro_classes,4,0:12))
         allocate(el_mon_mds_so2(0:num_of_hydro_classes,0:12))
         allocate(mon_mds_inco_sales_revenue(0:1024,0:12))
         allocate(mon_mds_inco_purchase_expense(0:1024,0:12))
         allocate(mon_mds_in_sales_revenue(0:12))
         allocate(mon_mds_in_purchase_expenses(0:12))
         allocate(mon_mds_in_variable_expenses(0:12))
         allocate(mon_mds_in_fixed_expenses(0:12))
         allocate(el_mon_mds_market_revenues( &
         allocate(el_mon_mds_market_purchase( &
         allocate(asset_class_list(avail_data_years))
         allocate(asset_allocation_list(avail_data_years))
            allocate(class_name(0:max_class_num))
      allocate(sort_pos(load_hours_in_period))
         allocate(hourly_hydro(load_hours_in_period, &
         allocate(i4_vector_map(-1:max_vector_num),vector_values(avail_data_years+1,-1:max_vectors),i4_reference_vector_map(0:9), &
         allocate(active_vector_values(-1:maximum_vectors))
        allocate(ioffacalgs(nvr),stat=as)
        allocate(x         (nvr),stat=as)
        allocate(corg      (nvr),stat=as)
        allocate(cmod      (nvr),stat=as)
        allocate(glb       (nvr),stat=as)
        allocate(lub       (nvr),stat=as)
        allocate(capacity  (nvr),stat=as)
        allocate(equivavail(nvr),stat=as)
        allocate(unitowning(nvr),stat=as)
        allocate(blockpos  (nvr),stat=as)
        allocate(auxrow    (nvr),stat=as)
        allocate(auxcycled (nvr),stat=as)
        allocate(auxicovers(nvr),stat=as)
        allocate(blockofsameunitas(nvr,2),stat=as)
        allocate(varofdisprank(nvr),stat=as)
        allocate(auxrhs    (naux),stat=as)
        allocate(auxcovers (naux,nvraux),stat=as)
        allocate(acolumn   (naux,nvraux),stat=as) ! data varies 1st across rows
        allocate(prevstate(nvar),stat=as)
        allocate(varstate(nvar),stat=as)
        allocate(prevbdirow(nvar),stat=as)
        allocate(basisdirow(nvar),stat=as)
        allocate(prevbkyvar(nvar),stat=as)
        allocate(basiskyvar(nvar),stat=as)
        allocate(fctowning(nvar),stat=as)
        allocate(keyvar(nvar),stat=as)
        allocate(d(nvar),stat=as)
        allocate(phi(nvar),stat=as)
        allocate(dualvar(nvar),stat=as)
        allocate(bkowns(nvar),stat=as)
        allocate(bindsetowns(nvar),stat=as)
        allocate(testsetowns(nvar),stat=as)
        allocate(wouldbezero(nvar),stat=as)
        allocate(varinfctorder(nvar),stat=as)
        allocate(vfofirstinfct(0:nvar),stat=as) ! 0 allows room for invalidfacet
          allocate(bnkinrow(naux),stat=as)
          allocate(auxdualmult(naux),stat=as)
          allocate(auxcolumn(naux),stat=as)
          allocate(dcolumn(naux,naux),stat=as)
          allocate(dorgrow(naux,naux),stat=as)
          allocate(dinvrow(naux,naux),stat=as)
          allocate(varcumul(nkappa,-1:nvar),stat=as)
        allocate(auxcoeff(nvarub),stat=as)
        allocate(cflb    (nvarub),stat=as)
        allocate(cfub    (nvarub),stat=as)
        allocate(blockdispcost(nunits,2),stat=as)
        allocate(blockheatrate(nunits,2),stat=as)
        allocate(arrayold(nrows,nrows),stat=as)
          allocate(jmarginal(limunknowns),stat=as) ! need to count even if fctvoidofdj
          allocate(grayorder(0:supigr),stat=as)
                  allocate(holdorder(0:supigr),stat=as)
                  allocate(grayorder(0:supigr),stat=as)
            allocate(fuel_price(0:avail_data_years))
            allocate(write_fuel_price(0:avail_data_years))
      allocate(fuel_price(0:avail_data_years))
      allocate( p_emis_report(max_emission_report_groups,nunits))
      allocate( s_emis_report(max_emission_report_groups,nunits))
      allocate( emiss_fuel_emis_report(max_emission_report_groups,nunits))
      allocate(pointr_record(0:1024),stat=error) ! save pointr_record
      allocate(emission_rates(0:r_records,total_emission_groups)) ! save pointr_record
            allocate(family_names(number_of_definitions),overlay_files_in_family(number_of_data_files,number_of_definitions),   &
         allocate(temp_asset_class_pointer(1024), &
            allocate(bc_nuc_fuel_class_pointer(max_bc_nuc_fuel_id_num))
      allocate(temp_asset_class_pointer(1024), &
         allocate(ol_nuc_fuel_class_pointer(max_ol_nuc_fuel_id_num))
      allocate(first_years_vec(avail_data_years), &
      allocate(nfce(max_financial_simulation_years), &
      allocate(nfis(financial_simulation_years), &
      allocate(asset_class_list(avail_data_years), &
      allocate( &
         allocate(id_pointer(0:10000), &
            allocate(asset_class_pointer(max_asset_class_num))
         allocate(tnnfv(financial_simulation_years, &
      allocate(fuel_inventory_is_monthly(0:max_fuel_inv_series))
      allocate(annual_file_pointr(0:max_fuel_inv_series))
      allocate(monthly_file_pointr(0:max_fuel_inv_series))
      allocate(mmbtu_balance_pointr(maximum_fuel_types))
      allocate(previous_annual_amount(0:avail_data_years))
      allocate(annual_amount(0:avail_data_years))
      allocate(monthly_amount(0:avail_data_years))
      allocate(first_time_for_this_monthly(12,0:max_fuel_inv_series))
      allocate(fuel_inventory_is_annual(0:max_fuel_inv_series))
      allocate(first_time_for_this(0:max_fuel_price_series),stat=error)
      allocate (new_fuel_price &
         allocate(fuel_price_pointr(0:max_fuel_price_series),stat=error)
      allocate(fuel_price(0:fuel_price_years))
         allocate(p_btu_cost_pointr(nunits))
         allocate(p_btu_content(nunits))
         allocate(s_btu_content(nunits))
         allocate(e_btu_content(nunits))
         allocate(p_shipping_units(nunits))
         allocate(s_shipping_units(nunits))
         allocate(e_shipping_units(nunits))
         allocate(s_btu_cost_pointr(nunits))
         allocate(e_btu_cost_pointr(nunits))
         allocate(p_btu_cost_is_a_pointr(nunits))
         allocate(s_btu_cost_is_a_pointr(nunits))
         allocate(e_btu_cost_is_a_pointr(nunits))
         allocate(primary_delivery_cost(r_unit))
         allocate(secondary_delivery_cost(r_unit))
               allocate(saved_cl_ann_cap(3,study_period,2))
              allocate(saved_cl_tg_cap(0:6,&
              allocate(grx_bop_retrofit_active(max_cl_units))
           allocate(grx_iter_co2_price(0:grx_max_iters,0:tg_max))
           allocate(grx_iter_tg_co2_emiss(0:grx_max_iters,0:tg_max))
           allocate(grx_co2_emiss_reduct_required(0:tg_max))
              allocate(grx_iter_co2_price(0:grx_max_iters,0:tg_max))
              allocate(grx_iter_tg_co2_emiss(0:grx_max_iters,0:tg_max))
              allocate(grx_co2_emiss_reduct_required(0:tg_max))
            allocate(temp_asset_class_pointer(1024),sbu_linked_counter(1024),bc_class_linkage(0:1024), &
               allocate(bc_init_class_pointer(max_bc_init_class_id_num))
         allocate(temp_asset_class_pointer(1024),sbu_linked_counter(1024),ol_class_linkage(0:1024), &
            allocate(ol_init_class_pointer(max_ol_init_class_id_num))
        allocate(bmatrix(0:msup,0:msup)    ,stat=as)
        allocate(bmatinv(0:msup,0:msup)    ,stat=as)
        allocate(swapvector (0:msup)       ,stat=as)
        allocate(phi(0:msup)               ,stat=as)
        allocate(yj (0:msup)               ,stat=as)
        allocate(yk (0:msup)               ,stat=as)
        allocate(z_c(0:nsup)               ,stat=as)
        allocate(zstar_c(0:nsup)           ,stat=as)
        allocate(zorg_c(0:nsup)            ,stat=as)
        allocate(gmatrix(0:sirank,0:sirank),stat=as)
        allocate(gmatluf(0:sirank,0:sirank),stat=as)
        allocate(upperbounded(0:nsup)    ,stat=as)
        allocate(symmetriclimits(0:nsup) ,stat=as)
        allocate(lineend(0:1,0:siline)   ,stat=as)
        allocate(basiscol(0:msup)        ,stat=as)
        allocate(basispos(0:nsup)        ,stat=as)
        allocate(b(0:sicons)             ,stat=as)
        allocate(c(0:nsup)               ,stat=as)
        allocate(ub(0:nsup)              ,stat=as)
        allocate(xb(0:msup)              ,stat=as)
        allocate(constrtrow(0:nsup)      ,stat=as)
        allocate(yline(0:siline)         ,stat=as)
        allocate(ej(0:msup)              ,stat=as)
        allocate(consrhs(0:sicons)       ,stat=as)
        allocate(segweight(0:nsup)       ,stat=as)
        allocate(currlimit(0:1,0:siline) ,stat=as)
        allocate(amatrix(0:sicons,0:nsup),stat=as)
        allocate(linename(0:siline)      ,stat=as)
        allocate(cb(0:msup)           ,stat=as)
        allocate(nodevoltage(0:sinode),stat=as)
        allocate(nodecurrent(0:sinode),stat=as)
          allocate(bmatluf(0:msup,0:msup),stat=as)
          allocate(pivotrow      (0:msup),stat=as)
          allocate(supmaginrow   (0:msup),stat=as)
               allocate(cum_unit_util(trans_group_points, &
               allocate(current_unit_util(trans_group_points, &
               allocate(tot_embed_cost_by_point(trans_group_points, &
               allocate(cum_unit_util_zero(trans_group_points, &
               allocate(current_unit_util_zero(trans_group_points, &
               allocate(blockincrcost(blok_array_allocator))
               allocate(cost_curve_pointer_by(0:blok_array_allocator, &
               allocate( &
            allocate(depth_price(depth_market_intervals,24))
               allocate(trans_depth_price( &
         allocate ( &
         allocate(new_marginal_data_base(trans_group_points, &
         allocate(loddur_for_mc(0:1000,max_data_bases))
         allocate(lprob_for_mc(0:1000,2,max_data_bases))
         allocate(active_unit(nunits))
         allocate(start_up_position(nunits,save_upper_trans_group))
         allocate(scarcity_mult(save_upper_trans_group,2))
         allocate(sales_b4_after(0:trans_group_points,2,max_data_bases))
         allocate(wholesale_rev_and_exp(trans_group_points,2, &
         allocate(wholesale_sal_and_pur(trans_group_points,2, &
         allocate(retail_rev_and_exp(trans_group_points,2, &
         allocate(retail_sal_and_pur(trans_group_points,2, &
         allocate(new_point_count(trans_group_points,max_data_bases))
         allocate(new_loddur(trans_group_points,max_data_bases))
         allocate(expected_marginal_cost(trans_group_points, &
         allocate(transition_marginal_cost(trans_group_points, &
         allocate(capacity_given_market(trans_group_points, &
         allocate(block_b4_market(trans_group_points,max_data_bases))
         allocate(remaining_contribution_mw(trans_group_points, &
         allocate( &
         allocate(cum_cap(max_data_bases))
         allocate(last_db_point(max_data_bases))
         allocate(cumdbdx(max_data_bases))
         allocate(db_adj_total_cap(max_data_bases))
         allocate(db_cap_after_outages(max_data_bases))
         allocate(db_dx(max_data_bases))
         allocate(previous_interval(max_data_bases))
         allocate(i_correct(max_data_bases))
         allocate(active_data_base(max_data_bases))
         allocate(detailed_outage_data_base(max_data_bases))
         allocate(current(max_data_bases))
         allocate(next(max_data_bases))
         allocate(pos(max_data_bases))
         allocate(native_pos(max_data_bases))
         allocate(cum_cost(max_data_bases))
         allocate(cum_prob(max_data_bases))
         allocate(save_load(max_data_bases))
         allocate(native_pos_percent(max_data_bases))
         allocate(cum_sales_after(max_data_bases))
         allocate(cum_must_run_mw(max_data_bases))
         allocate(cum_must_run_position(max_data_bases))
         allocate(unit_energy_b4_after(blok_array_allocator,2, &
      allocate(unit_save(nblok2),blkno_save(nblok2)) ! 11/28/00.
      allocate(unit_disp_cost(nunits,2),block_disp_cost(nblok2))
      allocate(save_trans_for_data_base(max_data_bases))
      allocate(outage_unit_index(0:data_basis))
      allocate(outage_block_index(0:data_basis))
      allocate(trans_block_capacity(0:data_basis))
      allocate(cum_tc_unit_mwh(nunits))
      allocate(first_economy_cost(max_data_bases))
      allocate(must_run_cost(max_data_bases))
      allocate(mw_after_outages(nblok2,max_data_bases))
      allocate(mw_diff(0:nblok2,max_data_bases))
      allocate(start_up_unit_by_tg(save_upper_trans_group))
      allocate(marginal_unit_by_tech(save_upper_trans_group, &
      allocate(outage_db_used(max_trans_group_number))
      allocate(commitment_db(max_trans_group_number))
                  allocate(temp_transfer_array1(0:allocated_data_bases))
                  allocate(temp_transfer_array2(0:allocated_data_bases))
                  allocate(temp_transfer_array3(0:allocated_data_bases))
                  allocate( &
                  allocate( &
                  allocate( &
      allocate(outage_singular(0:save_outage_blocks))
      allocate(adjust_block(0:save_outage_blocks))
      allocate(derate_block(0:save_outage_blocks))
      allocate(outage_type(0:save_outage_blocks))
         allocate(hourly_outage_state(nblok2,save_hours_in_month))
         allocate(hourly_outage_type(nblok2,save_hours_in_month))
         allocate(hourly_last_state(nblok2))
         allocate(hourly_last_derate(nblok2))
         allocate(hourly_last_type(nblok2))
            allocate(outage_energy(save_outage_blocks))
            allocate(outage_wholesale_energy(save_outage_blocks,2))
            allocate(outage_wholesale_revenue(save_outage_blocks,2))
      allocate(sort_pos(load_hours_in_period), &
      allocate(a_temp_array(0:nunique),b_temp_array(0:nunique))
         allocate(temp_asset_class_pointer(1024))
            allocate(addendum_bc_asset_class_pointer(max_bc_addendum_class_id_num))
      allocate(temp_asset_class_pointer(1024))
         allocate(addendum_ol_asset_class_pointer(max_ol_addendum_class_id_num))
      allocate(escalation_rates(avail_data_years),addendum_values(0:max(financial_simulation_years,avail_data_years)), &
            allocate(asset_class_pointer(max_asset_class_num))
         allocate(payments(0:12,0:financial_simulation_years,-1:num_of_asset_classes,last_payment_item), &
         allocate(temp_asset_class_pointer(1024))
            allocate(debit_bc_asset_class_pointer(max_bc_debit_class_id_num))
      allocate(temp_asset_class_pointer(1024))
         allocate(debit_ol_asset_class_pointer(max_ol_debit_class_id_num))
      allocate(asset_class_list(avail_data_years), &
         allocate(asset_class_pointer(max_asset_class_num))
      allocate(tddb(0:12,0:financial_simulation_years,0:num_of_asset_classes,0:bal_sheet_options,unique_acct_type), &
         allocate(temp_asset_class_pointer(1024))
            allocate(bc_existing_asset_class_pointer &
      allocate(temp_asset_class_pointer(1024))
         allocate(ol_existing_asset_class_pointer &
      allocate(bokdp(0:12,0:financial_simulation_years))
      allocate(gpv(0:12,0:financial_simulation_years))
      allocate(cum_book_dep(0:12,0:financial_simulation_years))
      allocate(npv(0:12,0:financial_simulation_years))
      allocate(retire(0:12,0:financial_simulation_years))
      allocate(taxdp(0:financial_simulation_years))
      allocate(boktax(0:financial_simulation_years))
      allocate(asset_deferred_taxes(0:financial_simulation_years))
      allocate(asset_deferred_tax_basis(0:financial_simulation_years))
      allocate(monthly_book_dep_vector_adder(0:12, &
         allocate(asset_class_pointer(max_asset_class_num))
      allocate(tretie(0:12,financial_simulation_years, &
      allocate(tbokdp(0:12,financial_simulation_years, &
      allocate(tgpv(0:12,financial_simulation_years, &
      allocate(cumulative_book_dep(0:12,financial_simulation_years, &
      allocate(total_npv_ratebase(0:12,financial_simulation_years, &
      allocate(ttaxdp(financial_simulation_years, &
      allocate(property_tax_gpv(financial_simulation_years, &
      allocate(property_tax_npv(financial_simulation_years, &
      allocate(eataxbokdep(financial_simulation_years, &
      allocate(eanormtaxdp(financial_simulation_years, &
      allocate(ea_def_tax(financial_simulation_years, &
      allocate(ea_def_tax_basis(financial_simulation_years, &
      allocate(total_def_tax_ratebase(financial_simulation_years))
      allocate(ea_ace_depreciation(financial_simulation_years))
      allocate(total_def_tax_basis_ratebase(financial_simulation_years))
      allocate(asset_class_list(avail_data_years))
      allocate(asset_allocation_list(avail_data_years))
      allocate(read_expen(0:avail_data_years))
      allocate(expenses(0:financial_simulation_years))
      allocate(midas_annual_cash(0:financial_simulation_years))
      allocate(account_payable_values(0:financial_simulation_years))
      allocate(output_value(0:financial_simulation_years))
      allocate(alloc_expen(0:financial_simulation_years))
      allocate(alloc_cash_expen(0:financial_simulation_years))
      allocate(asset_class_list(avail_data_years))
      allocate(asset_allocation_list(avail_data_years))
            allocate(asset_class_pointer(max_asset_class_num))
         allocate(expenses_monthly(0:12,0:financial_simulation_years, &
         allocate(budget_expense(0:12,0:financial_simulation_years,10))
         allocate(revenues_monthly(0:12,0:financial_simulation_years, &
         allocate(secondary_sales_not_in_rates(0:12, &
         allocate(other_revenues_not_in_rates(0:12, &
         allocate(cash_monthly(0:12, &
         allocate(cash_rev_exp_monthly(0:12, &
         allocate(annual_cash_roll_over(1:12,cash_vars, &
      allocate(temp_asset_class_pointer(1024))
         allocate(bc_fa_class_pointer(max_bc_class_id_num))
      allocate(temp_asset_class_pointer(1024))
         allocate(ol_fa_class_pointer(max_ol_class_id_num))
         allocate(cep(max_financial_simulation_years))
         allocate(ce(max_financial_simulation_years))
         allocate(escalation_rates(avail_data_years))
         allocate(cep(max_financial_simulation_years))
         allocate(ce(max_financial_simulation_years))
         allocate(escalation_rates(avail_data_years))
            allocate(temp_holding_array(max_financial_simulation_years))
      allocate(ceptxbok(financial_simulation_years))
      allocate(total_bkdptx(financial_simulation_years))
      allocate(current_interest_cap(financial_simulation_years))
      allocate(pcapinrst(financial_simulation_years))
      allocate(taxdpalt(financial_simulation_years))
      allocate(capinrst(financial_simulation_years))
      allocate(txprefdep(financial_simulation_years))
      allocate(book_value(financial_simulation_years))
      allocate(ace_book_dep(financial_simulation_years))
      allocate(tax_value_beginning_90(financial_simulation_years))
      allocate(dummy(financial_simulation_years))
      allocate(book_expen(0:12,0:financial_simulation_years))
      allocate(cash_om_adder(0:12,0:financial_simulation_years))
      allocate(non_cash_om_adder(0:12,0:financial_simulation_years))
      allocate(plant_2_service(0:12,0:financial_simulation_years))
      allocate(percent_plant_2_service(0:12,0:financial_simulation_years))
      allocate(monthly_afudc_on_cash(0:12,0:financial_simulation_years))
      allocate(monthly_afudc_on_plant(0:12,0:financial_simulation_years))
      allocate(monthly_cwip(0:12,0:financial_simulation_years))
      allocate(monthly_afudc_in_cwip(0:12,0:financial_simulation_years))
      allocate(monthly_cwip_in_ratebase(0:12,0:financial_simulation_years))
      allocate(monthly_capitalized_interest(0:12,0:financial_simulation_years))
      allocate(monthly_current_interest(0:12,0:financial_simulation_years))
      allocate(cash_expenditures(0:12,0:financial_simulation_years))
      allocate(monthly_interest_to_tax_value(0:12,0:financial_simulation_years))
      allocate(monthly_tax_value_of_asset(0:12,0:financial_simulation_years))
      allocate(bonus_dep_monthly_2001(0:12,0:financial_simulation_years))
      allocate(monthly_tax_dep_preference(0:12,0:financial_simulation_years))
      allocate(monthly_tax_depreciation_alt(0:12,0:financial_simulation_years))
      allocate(monthly_tax_book_dep(0:12,0:financial_simulation_years))
      allocate(monthly_ace_book_dep(0:12,0:financial_simulation_years))
      allocate(dummy_monthly_array(0:12,0:financial_simulation_years))
      allocate(monthly_tax_depreciation(0:12,0:financial_simulation_years))
      allocate(monthly_deferred_tax_basis(0:12,0:financial_simulation_years))
      allocate(monthly_tax_expense(0:12,0:financial_simulation_years))
      allocate(monthly_current_interest_cap(0:12,0:financial_simulation_years))
      allocate(monthly_book_dep(0:12,0:financial_simulation_years))
      allocate(cum_book_dep(0:12,0:financial_simulation_years))
      allocate(gpv(0:12,0:financial_simulation_years))
      allocate(npv(0:12,0:financial_simulation_years))
      allocate(temp_plant_2_service(0:financial_simulation_years))
      allocate(vector_data_cash(avail_data_years))
      allocate(vector_data_plnt(avail_data_years))
      allocate(vector_data(avail_data_years))
            allocate(asset_class_pointer(max_asset_class_num))
         allocate(tce(financial_simulation_years,0:num_of_fa_classes))
         allocate(tcep(financial_simulation_years,0:num_of_fa_classes))
         allocate(class_gpv(financial_simulation_years,0:num_of_fa_classes))
         allocate(class_cumulative_depreciation(financial_simulation_years,0:num_of_fa_classes))
         allocate(class_afudc_in_cwip(financial_simulation_years,0:num_of_fa_classes))
         allocate(ttaxdp(financial_simulation_years,0:num_of_fa_classes))
         allocate(tbokdp(financial_simulation_years,0:num_of_fa_classes))
         allocate(tafcdp(financial_simulation_years,0:num_of_fa_classes))
         allocate(tafdc1(financial_simulation_years,0:num_of_fa_classes))
         allocate(tafdc2(financial_simulation_years,0:num_of_fa_classes))
         allocate(tcwip(financial_simulation_years,0:num_of_fa_classes))
         allocate(trbcwp(financial_simulation_years,0:num_of_fa_classes))
         allocate(titcdp(financial_simulation_years,0:num_of_fa_classes))
         allocate(titcdf(financial_simulation_years,0:num_of_fa_classes))
         allocate(ttxexp(financial_simulation_years,0:num_of_fa_classes))
         allocate(ttxdef(financial_simulation_years,0:num_of_fa_classes))
         allocate(tddb(financial_simulation_years,0:num_of_fa_classes))
         allocate(trbddb(financial_simulation_years,0:num_of_fa_classes))
         allocate(tamrte(financial_simulation_years,0:num_of_fa_classes))
         allocate(texexp(financial_simulation_years,0:num_of_fa_classes))
         allocate(tafdpa(financial_simulation_years,0:num_of_fa_classes))
         allocate(tceptx(financial_simulation_years,0:num_of_fa_classes))
         allocate(tafcaj(financial_simulation_years,0:num_of_fa_classes))
         allocate(tbkdpt(financial_simulation_years,0:num_of_fa_classes))
         allocate(titcpy(financial_simulation_years,0:num_of_fa_classes))
         allocate(tafexp(financial_simulation_years,0:num_of_fa_classes))
         allocate(tbkdpa(financial_simulation_years,0:num_of_fa_classes))
         allocate(tpcapinrst(financial_simulation_years,0:num_of_fa_classes))
         allocate(ttxprefdep(financial_simulation_years,0:num_of_fa_classes))
         allocate(total_ace_book_dep(financial_simulation_years,0:num_of_fa_classes))
         allocate(tafdcb(financial_simulation_years,0:num_of_fa_classes))
         allocate(twodft(financial_simulation_years,0:num_of_fa_classes))
         allocate(tafdcf(financial_simulation_years,0:num_of_fa_classes))
         allocate(property_tax_gpv(financial_simulation_years,0:num_of_fa_classes))
         allocate(property_tax_npv(financial_simulation_years,0:num_of_fa_classes))
         allocate(rb_npv(financial_simulation_years))
         allocate(rb_taxdp(financial_simulation_years))
         allocate(rb_bokdp(financial_simulation_years))
         allocate(rb_pcapinrst(financial_simulation_years))
         allocate(rbcwip_afdc_meth2(financial_simulation_years))
         allocate(rb_txexp(financial_simulation_years))
         allocate(rb_txdef(financial_simulation_years))
         allocate(rb_txprefdep(financial_simulation_years))
         allocate(rb_amrte(financial_simulation_years))
         allocate(rb_itcpy(financial_simulation_years))
         allocate(rb_itc(financial_simulation_years))
         allocate(rb_afdc1(financial_simulation_years))
            allocate(asset_class_pointer(max_asset_class_num))
         allocate(tce(financial_simulation_years,0:num_of_fa_classes))
         allocate(tcep(financial_simulation_years,0:num_of_fa_classes))
         allocate(class_gpv(financial_simulation_years,0:num_of_fa_classes))
         allocate(class_cumulative_depreciation(financial_simulation_years,0:num_of_fa_classes))
         allocate(class_afudc_in_cwip(financial_simulation_years,0:num_of_fa_classes))
         allocate(ttaxdp(financial_simulation_years,0:num_of_fa_classes))
         allocate(tbokdp(financial_simulation_years,0:num_of_fa_classes))
         allocate(tafcdp(financial_simulation_years,0:num_of_fa_classes))
         allocate(tafdc1(financial_simulation_years,0:num_of_fa_classes))
         allocate(tafdc2(financial_simulation_years,0:num_of_fa_classes))
         allocate(tcwip(financial_simulation_years,0:num_of_fa_classes))
         allocate(trbcwp(financial_simulation_years,0:num_of_fa_classes))
         allocate(titcdp(financial_simulation_years,0:num_of_fa_classes))
         allocate(titcdf(financial_simulation_years,0:num_of_fa_classes))
         allocate(ttxexp(financial_simulation_years,0:num_of_fa_classes))
         allocate(ttxdef(financial_simulation_years,0:num_of_fa_classes))
         allocate(tddb(financial_simulation_years,0:num_of_fa_classes))
         allocate(trbddb(financial_simulation_years,0:num_of_fa_classes))
         allocate(tamrte(financial_simulation_years,0:num_of_fa_classes))
         allocate(texexp(financial_simulation_years,0:num_of_fa_classes))
         allocate(tafdpa(financial_simulation_years,0:num_of_fa_classes))
         allocate(tceptx(financial_simulation_years,0:num_of_fa_classes))
         allocate(tafcaj(financial_simulation_years,0:num_of_fa_classes))
         allocate(tbkdpt(financial_simulation_years,0:num_of_fa_classes))
         allocate(titcpy(financial_simulation_years,0:num_of_fa_classes))
         allocate(tafexp(financial_simulation_years,0:num_of_fa_classes))
         allocate(tbkdpa(financial_simulation_years,0:num_of_fa_classes))
         allocate(tpcapinrst(financial_simulation_years,0:num_of_fa_classes))
         allocate(ttxprefdep(financial_simulation_years,0:num_of_fa_classes))
         allocate(total_ace_book_dep(financial_simulation_years,0:num_of_fa_classes))
         allocate(tafdcb(financial_simulation_years,0:num_of_fa_classes))
         allocate(twodft(financial_simulation_years,0:num_of_fa_classes))
         allocate(tafdcf(financial_simulation_years,0:num_of_fa_classes))
         allocate(property_tax_gpv(financial_simulation_years,0:num_of_fa_classes))
         allocate(property_tax_npv(financial_simulation_years,0:num_of_fa_classes))
         allocate(rb_npv(financial_simulation_years), &
            allocate(asset_class_pointer(max_asset_class_num))
            allocate(total_monthly_fa_values(0:12, &
         allocate(temp_pointer(0:10000))
            allocate(base_vector_pointer(0:max_base_vector_num))
      allocate(temp_pointer(0:10000))
         allocate(ovly_vector_pointer(0:max_overlay_vector_num))
         allocate(temp_pointer(0:10000))
            allocate(base_vector_pointer(0:max_base_vector_num))
      allocate(temp_pointer(0:10000))
         allocate(ovly_vector_pointer(0:max_overlay_vector_num))
         allocate(file_payable_values(12,5,payment_vars, &
         allocate(temp_asset_class_pointer(1024))
            allocate(bc_asset_class_pointer(max_bc_class_id_num))
         allocate(temp_asset_class_pointer(1024))
            allocate(ol_asset_class_pointer(max_ol_class_id_num))
            allocate(asset_class_list(avail_data_years), &
               allocate(asset_class_pointer(max_asset_class_num))
            allocate(revenues_monthly(0:12,-1:num_of_asset_classes,last_income_line), &
            allocate(asset_class_customers(0:12,-1:num_of_asset_classes))
            allocate(asset_class_energy(0:12,-1:num_of_asset_classes))
            allocate(state_rps_db(13,600,0:12), &
               allocate(ann_rps_intra_region_surplus(0:6,0:rps_region_count))
            allocate( &
         allocate(trans_group_position(-1:max_gas_group_number), &
         allocate(resources_per_type(max_states),rps_resources_index(r_resource_num))
         allocate(rps_supply_curves( &
         allocate(rps_net_rev(num_rps_resources), &
            allocate(co2_emission_cap(yr1_data:yr2_data),residential_emissions(yr1_data:yr2_data), &
         allocate(      initial_bank(num_rps_programs), &
            allocate(program_position(max_program_number))
         allocate(reporting_variables(0:num_active_variables))
         allocate(transfr_fuel_cost_amount(0:12,number_of_cost_classes))
         allocate(transfr_var_cost_amount(0:12,number_of_cost_classes))
         allocate(transfr_cost_quantities(0:12,number_of_cost_classes))
         allocate(transfr_cost_class_pointr(0:max_cost_class_number+1))
         allocate(transfr_rev_amount(0:12,number_of_rev_classes),transfr_rev_quantities(0:12,number_of_rev_classes), &
         allocate(temp_asset_class_pointer(1024))
            allocate(bc_asset_class_pointer(max_bc_class_id_num))
      allocate(temp_asset_class_pointer(1024))
         allocate(ol_asset_class_pointer(max_ol_class_id_num))
      allocate(data_values(0:avail_data_years),annual_tax_values(0:financial_simulation_years), &
            allocate(asset_class_pointer(max_class_num))
         allocate(tax_values(financial_simulation_years,0:12,-5:num_of_classes,last_tax_item,3))
         allocate(tax_values_nols(0:15,0:12,-5:num_of_classes,35:40))
         allocate(actual_monthly_tax_values(0:12,financial_simulation_years,-1:num_of_classes,last_actual_tax_item))
         allocate(actual_value_found(-1:num_of_classes,last_actual_tax_item))
            allocate(asset_class_pointer(base_case_max_class_num))
         allocate(tax_values(financial_simulation_years,0:12,-5:base_case_num_of_classes,last_tax_item,3))
         allocate(tax_values_nols(0:15,0:12,-5:base_case_num_of_classes,35:40))
         allocate(actual_monthly_tax_values(0:12,financial_simulation_years,-1:base_case_num_of_classes,last_actual_tax_item))
         allocate(actual_value_found(-1:base_case_num_of_classes,last_actual_tax_item))
         allocate(federal_book_losses(-1:yrs,-1:max_class_num),sec_42_credits(-1:yrs,-1:max_class_num), &
            allocate(prosymrevandexps(0:12,basestartyr:baseendyr, &
      allocate(asset_class_pointer(max_asset_class_num))
      allocate(prosymrevandexps(0:12,startyr:endyr, &
      allocate(values(datavariables,startyr:endyr))
         allocate(resourcelist(resourcesinfile))     
         allocate(resourcelistids(resourcesinfile))
         allocate(operation_type(que_stack_pointr), &
         allocate(partial_objfunc_des(num_of_variables), &
         allocate(bc_table_position_for(0:max_bc_class_number))
      allocate(ol_table_position_for(0:max_ol_class_number))
         allocate(max_blocks_per_trans(0:upper_trans_group)) ! 0 is the max of all groups
      allocate(block_disp_cost(nunits,2),second_disp_cost(nunits,2))
      allocate(shadow_dispatch_adder(nblok))
            allocate(lamda_blk1(blk1_dispatch),blkno_blk1(blk1_dispatch),inheat_blk1(blk1_dispatch),mwblok_blk1(blk1_dispatch), &
            allocate(lamda_blk2(blk2_dispatch),blkno_blk2(blk2_dispatch),inheat_blk2(blk2_dispatch),mwblok_blk2(blk2_dispatch), &
            allocate(lamda_mr(must_run_blocks),blkno_mr(must_run_blocks),inheat_mr(must_run_blocks),mwblok_mr(must_run_blocks), &
         allocate(first_blk_used(max_cl_units))
            allocate(posblk0(blk0))
            allocate(posblk1(blk1))
            allocate(lamda_temp(blk1_dispatch))
            allocate(lamda_temp(blk2_dispatch))
            allocate(posblk2(blk2))
            allocate(second_blk_passed(blk2))
      allocate(block_disp_cost(nunits,2),second_disp_cost(nunits,2))
      allocate(shadow_dispatch_adder(nblok))
      allocate(partial_objfunc_des(50),group_string(50),   &
         allocate(operator_stack(0:256),   &
         allocate(energy(max_simulation_years), &
            allocate(nox_season_region(max_trans_group_number), &
         allocate(blended_btu_cost(max(nunits,int(1,2))))
         allocate(annual_blended_btu_cost(max(nunits,int(1,2))))
         allocate(annual_fuel_btu_cost(max(nunits,int(1,2))))
         allocate(annual_disp_btu_cost(max(nunits,int(1,2))))
            allocate(first_hydro_load_no(0:number_of_hydro_load_groups), &
      allocate (rdi_variable_name(number_of_rdi_variables))
         allocate(months_per_plant(max_rdi_plants), &
      allocate (rbc_variable_name(number_of_rbc_variables))
         allocate(months_per_plant(max_rbc_plants))
      allocate(service_asset_class_pointer(1024), &
      allocate(service_name(num_service_trans), &
      allocate(capacity_charge(service_trans), &
            allocate(annual_st_energy(number_of_services), &
            allocate(monthly_st_energy(0:12,number_of_services), &
            allocate(annual_st_energy(number_of_services), &
            allocate(monthly_st_energy(0:12,number_of_services), &
         allocate(st_ann_class_atl_expense(0:12, &
         allocate(asset_class_list(avail_data_years), &
               allocate(annual_costs(6,total_annual_records))
               allocate(trans_name(no_of_trans), &
         allocate (temp_chrono_load(temp_hours))
      allocate (data_record(min(help,out_rec_length-int(4,4))))
               allocate(temp_trans_class_pointer(0:1023), &
            allocate(temp_trans_class_pointer(0:1023), &
             allocate( &
            allocate(customer_group(max_trans_load_tables)) ! int2
            allocate(customer_group_2(max_trans_load_tables))
            allocate( &
            allocate(scenario_index(max_trans_load_tables))
            allocate(transaction_group(max_trans_load_tables)) ! int2
            allocate(reference_load_number(max_trans_load_tables)) ! int2
            allocate(market_energy_price(max_trans_load_tables)) ! real4
            allocate(monthly_energy_price_pattern( &
            allocate(market_demand_price(max_trans_load_tables)) ! real4
            allocate(monthly_demand_price_pattern( &
            allocate(market_customer_price(max_trans_load_tables)) ! real4
            allocate(asset_class_id(max_trans_load_tables)) ! real4
            allocate(asset_class_rev_alloc_vector( &
            allocate(annual_energy(max_trans_load_tables)) ! real4
            allocate(annual_peak(max_trans_load_tables)) ! real4
            allocate(annual_customers(max_trans_load_tables)) ! real4
            allocate(annual_multiplier(max_trans_load_tables)) ! real4
            allocate(monthly_energy(12,max_trans_load_tables)) ! (12) real4
            allocate(monthly_peak(12,max_trans_load_tables)) ! (12) real4
            allocate(monthly_customers(12,max_trans_load_tables)) ! (12) real4
            allocate(dist_energy_loss_factor(max_trans_load_tables)) ! real4
            allocate(trans_energy_loss_factor(max_trans_load_tables)) ! real4
            allocate(peak_loss_factor(max_trans_load_tables)) ! real4
            allocate(peak_coin_factor(max_trans_load_tables)) ! real4
            allocate(distribution_price(max_trans_load_tables)) ! real4
            allocate(transmission_price(max_trans_load_tables)) !real4!
            allocate(customer_class_name(max_trans_load_tables)) ! char*30
            allocate(calculation_mode(max_trans_load_tables)) ! char*1
            allocate(reference_load_name(max_trans_load_tables)) ! char*5
            allocate(trans_monthly_energy(12,max_trans_load_tables))
            allocate(wh_trans_monthly_energy(12, &
            allocate(wh_trans_monthly_capacity(12, &
            allocate(trans_monthly_peak(12,max_trans_load_tables))
            allocate(trans_monthly_customers(12,max_trans_load_tables))
            allocate(table_active(max_trans_load_tables))
            allocate(basecase_market_area_id(max_trans_load_tables))
            allocate(wd_index(max_trans_load_tables))
            allocate(basecase_trans_area_id(max_trans_load_tables))
            allocate(basecase_nerc_sub_id(max_trans_load_tables))
            allocate(monthly_units(max_trans_load_tables))
            allocate(price_index_active(max_trans_load_tables))
            allocate(three_factor_transform(max_trans_load_tables))
            allocate(jurisdictional_customer(max_trans_load_tables))
            allocate(fuel_cost_recovery_through_fac( &
            allocate(base_cost_of_fac_fuel(max_trans_load_tables))
            allocate(minimum_market_price(max_trans_load_tables))
            allocate(maximum_market_price(max_trans_load_tables))
            allocate(indexed_energy_price(max_trans_load_tables))
            allocate(tg_counter(max_trans_groups))
            allocate(load_dispatch_position(max_trans_load_tables, &
            allocate(load_dispatch_index(max_trans_load_tables, &
            allocate(trans_load_groups_index(max_trans_groups))
            allocate(cust_class_groups_index(0:max_class_groups))
            allocate(cust2_class_groups_index(0:max_class2_groups))
            allocate(asset_class_groups_index(0:max_asset_groups))
            allocate(asset_2_trans_index(0:max_asset_groups, &
            allocate(number_asset_2_trans(max_trans_groups))
            allocate(asset_transaction_cross_index(0:max_asset_groups, &
            allocate(number_trans_per_ac_tg(0:max_asset_groups, &
            allocate(trans_within_ac_tg(0:max_asset_groups, &
            allocate(first_ac_tg(0:max_asset_groups))
            allocate(first_table_tg(0:max_trans_groups))
            allocate(trans_load_2_trans_groups(max_trans_groups))
            allocate(trans_load_group_2_tg(max_trans_groups))
            allocate(cust_class_group_2_cg(max_class_groups+1))
            allocate(cust2_class_group_2_cg(max_class_groups+1))
            allocate(asset_class_group_2_ac(0:max_asset_groups+1))
            allocate(last_table_for_tg(max_trans_groups))
            allocate(last_table_for_cg(max_class_groups+1))
            allocate(last_table_for_cg2(max_class_groups+1))
            allocate(tg_cg_database(0:12,0:max_trans_groups, &
            allocate(tg_cg2_database(0:12,0:max_trans_groups, &
            allocate(table_day_shift(max_trans_load_tables))
            allocate(tf_planning_peak(0:max_trans_groups,12))
            allocate(pa_planning_peak(0:number_of_active_groups,12))
            allocate(cm_planning_peak(0:number_of_active_groups,12))
            allocate(global_pa_peak(0:number_of_active_groups))
            allocate(global_cm_peak(0:number_of_active_groups))
            allocate(global_pa_peak_month(0:number_of_active_groups))
            allocate(global_cm_peak_month(0:number_of_active_groups))
            allocate(tg_2_planning_area(max_trans_groups_from_tg))
            allocate(tg_2_capacity_market(max_trans_groups_from_tg))
            allocate(rev_class_index(0:max_trans_load_tables,4))
            allocate(energy_loss_mult(max_trans_load_tables))
            allocate(ref_leap_year_day_shift(max_trans_load_tables))
            allocate(demand_pricing_method(max_trans_load_tables))
           allocate(monthly_table_peak_sales(max_trans_load_tables,12))
            allocate(intra_company_transaction(max_trans_load_tables))
            allocate(intra_asset_class_id(max_trans_load_tables))
            allocate(intra_asset_class_allocation( &
            allocate(intra_account_classification( &
            allocate(intra_expense_collection(max_trans_load_tables))
         allocate(monthly_interruptible_revenue( &
            allocate(last_this_yr_energy(2,12,0:max_trans_load_tables)) ! 1=last_year, 2=this year
            allocate(last_this_yr_peak(2,12,0:max_trans_load_tables))
         allocate(monthly_ac_cost_at_market( &
         allocate(monthly_ac_contract_revenue( &
         allocate(monthly_ac_contract_expense( &
         allocate(day_of_week(31,0:max_trans_load_tables))
            allocate( &
            allocate( &
            allocate( &
            allocate( &
            allocate( &
            allocate( &
            allocate( &
            allocate( &
         allocate (wh_trans_alloc(0:max_trans_load_tables, &
         allocate(trans_hourly_load( &
         allocate(wh_loads_per_hour( &
         allocate(hydro_hourly_load( & !  need to count hydro groups
         allocate(table_hourly_load( &
         allocate(monthly_trans_energy(0:max_trans_load_groups))
         allocate(monthly_hydro_energy(0:number_of_hydro_groups))
         allocate(monthly_trans_peak(0:max_trans_load_groups))
         allocate(monthly_trans_base(0:max_trans_load_groups))
         allocate(monthly_hydro_peak(0:number_of_hydro_groups))
         allocate(monthly_hydro_base(0:number_of_hydro_groups))
         allocate(monthly_table_energy(0:max_trans_load_tables))
         allocate(monthly_table_sales_energy( &
         allocate(monthly_table_peak(0:max_trans_load_tables))
         allocate(table_energy_price(max_trans_load_tables))
         allocate(table_energy_revenue(0:max_trans_load_tables))
         allocate(table_demand_revenue(0:max_trans_load_tables))
         allocate(table_customer_revenue(0:max_trans_load_tables))
         allocate(trans_energy_revenue(0:max_trans_load_groups))
         allocate(trans_demand_revenue(0:max_trans_load_groups))
         allocate(trans_customer_revenue(0:max_trans_load_groups))
         allocate(class_energy_revenue(0:max_cust_class_groups))
         allocate(trans_indexed_revenue(0:max_cust_class_groups))
         allocate(class_peak_revenue(0:max_cust_class_groups))
         allocate(class_customer_revenue(0:max_cust_class_groups))
         allocate(cap_customer_expense(0:max_cust_class_groups))
         allocate(tf_tg_cap_market_mw(0:max_trans_groups))
         allocate(tf_tg_cap_market_cost(0:max_trans_groups))
         allocate(monthly_class_energy(0:max_cust_class_groups))
         allocate(monthly_class_peak(0:max_cust_class_groups))
         allocate(monthly_class_customers(0:max_cust_class_groups))
         allocate(local_customer_name(0:max_cust_class_groups))
         allocate(asset_class_hourly_load(max_hours_in_month, &
         allocate(ann_load_dispatch_cost_by_unit(nunits+2, &
         allocate(ann_load_dispatch_rev_by_unit(nunits+2, &
         allocate(ann_load_dispatch_by_unit(nunits+2, &
            allocate(hourly_cust_margin_db(4,hourly_cust_margin_var))
            allocate(load_dispatch_by_block( &
            allocate(load_dispatch_cost( &
            allocate(local_must_run_options(r_total_dispatch_blocks))
            allocate(load_dispatch_cost_by_unit( &
            allocate(load_dispatch_rev( &
           allocate(load_dispatch_rev_by_unit(nunits+2,load_block_var))
            allocate(load_dispatch_by_unit(nunits+2,load_block_var))
         allocate(cum_block_generation(last_block,3))
         allocate(temp_chrono_load(save_hours))
         allocate(temp_chrono_load(save_hours))
         allocate(trans_group_index(0:max_trans_group_index))
         allocate(tg_scenario_variable_index(0:max_trans_group_index))
         allocate(hydro_group_2_tg(0:max_trans_group_index))
         allocate(trans_group_position(0:max_trans_group_index))
         allocate(report_this_group(0:max_trans_group_index))
         allocate(tg_2_hydro_group(0:max_trans_group_index))
         allocate(tg_2_planning_area(0:max_trans_group_index))
         allocate(tg_2_capacity_market(0:max_trans_group_index))
         allocate(hydro_aggregation_position(0:max_trans_group_index))
         allocate(hydro_aggregation_index(0:max_trans_group_index))
         allocate(planning_area_position(0:max_trans_group_index))
         allocate(capacity_market_position(0:max_trans_group_index))
         allocate(planning_area_index(0:max_trans_group_index))
         allocate(capacity_market_index(0:max_trans_group_index))
         allocate(asset_class_groups_index(0:max_asset_groups))
         allocate(asset_class_2_tg(0:max_asset_groups))
         allocate(asset_class_group_2_ac(0:max_trans_group_index))
         allocate(group_name(0:trans_groups_records))
         allocate(group_active(trans_groups_records))
         allocate(transaction_group(trans_groups_records))
         allocate(trans_group_full_name(trans_groups_records))
         allocate(tg_basecase_market_area_id(trans_groups_records))
         allocate(basecase_market_area_id(trans_groups_records))
         allocate(base_case_trans_area_id(trans_groups_records))
         allocate(basecase_subregion_id(trans_groups_records))
         allocate(spinning_units(trans_groups_records))
         allocate(off_peak_spinning_units(trans_groups_records))
         allocate(spinning_reserve(trans_groups_records))
         allocate(off_peak_spinning_reserve(trans_groups_records))
         allocate(max_hourly_ramp_up(trans_groups_records))
         allocate(max_hourly_ramp_down(trans_groups_records))
         allocate(first_capacity_value(0:trans_groups_records))
         allocate(first_capacity_percent(0:trans_groups_records))
         allocate(second_capacity_value(0:trans_groups_records))
         allocate(second_capacity_percent(0:trans_groups_records))
         allocate(third_capacity_value(0:trans_groups_records))
         allocate(third_capacity_percent(0:trans_groups_records))
         allocate(additional_capacity_value(0:trans_groups_records,7))
        allocate(additional_capacity_percent(0:trans_groups_records,7))
         allocate(rto_group(trans_groups_records))
         allocate(nox_year(trans_groups_records))
         allocate(end_nox_year(trans_groups_records))
         allocate(st_lhs_for_prices(trans_groups_records))
         allocate(mrx_volatility_mult(trans_groups_records))
         allocate(night_scarcity_mult(trans_groups_records))
         allocate(weekend_scarcity_mult(trans_groups_records))
         allocate(price_cap(trans_groups_records))
         allocate(price_minimum(trans_groups_records))
         allocate(regional_capacity_market(trans_groups_records))
         allocate(max_hourly_tg_import(trans_groups_records))
         allocate(max_hourly_tg_export(trans_groups_records))
         allocate(hydro_load_aggregation(trans_groups_records))
         allocate(regional_planning_area(trans_groups_records))
         allocate(report_cl_capacity(trans_groups_records))
         allocate(asset_class_id(trans_groups_records))
         allocate(asset_class_rev_alloc_vector(trans_groups_records))
         allocate(time_zone(trans_groups_records))
         allocate(capacity_adder(trans_groups_records))
         allocate(nox_season(trans_groups_records))
         allocate(purchase_power_assign(trans_groups_records))
         allocate(create_hourly_price(trans_groups_records))
         allocate(create_hourly_price_index(trans_groups_records))
         allocate(create_hourly_price_pos(trans_groups_records))
         allocate(hourly_price_name(trans_groups_records))
         allocate(purchase_asset_class_id(trans_groups_records))
         allocate(purchase_asset_alloc_vector(trans_groups_records))
         allocate(price_escalation_rate(trans_groups_records), &
         allocate(demand(r_max_trans_load_groups))
         allocate(dx(r_max_trans_load_groups))
         allocate(peak(r_max_trans_load_groups))
         allocate(loddur(load_points,r_max_trans_load_groups))
         allocate(lprob(load_points,r_max_trans_load_groups))
         allocate(demand(r_max_hydro_load_groups))
         allocate(dx(r_max_hydro_load_groups))
         allocate(peak(r_max_hydro_load_groups))
         allocate(loddur(1000,r_max_hydro_load_groups))
         allocate(lprob(1000,r_max_hydro_load_groups))
      allocate(hourly_hydro(load_hours_in_period), &
      allocate(sort_pos(load_hours_in_period))
      allocate(sort_pos(load_hours_in_period))
         allocate(trans_load_after_el( &
         allocate(trans_hourly_hydro( &
         allocate(monthly_trans_load_mwh(0:max_trans_load_groups))
        allocate(monthly_trans_load_hydro_mwh(0:max_trans_load_groups))
         allocate(monthly_trans_load_hydro_mw(0:max_trans_load_groups))
         allocate(monthly_trans_peak(0:max_trans_load_groups))
         allocate(monthly_trans_base(0:max_trans_load_groups))
            allocate(scenario_variable_active(num_scen_var))
            allocate(scenario_variable_name(num_scen_var))
            allocate(time_frame(num_scen_var,avail_data_years))
            allocate(hourly_reference_name(num_scen_var, &
            allocate(hourly_reference_number(num_scen_var, &
            allocate(scen_maker_variable(num_scen_var,local_months, &
         allocate(unit_names(max_outages), &
         allocate( begin_month_day(max_events_in_month), &
         allocate( month_event_counter(max_trans_groups) )
         allocate(events_record_index( &
         allocate(begin_in_hour_alan(max_fo_per_month+1), &
            allocate(clm_maint_index(total_indexed_outages))
            allocate( &
            allocate( &
         allocate(max_blocks_per_trans(0:upper_trans_group), &
         allocate(product_gen_by_s_u(0:max_start_up_units_by_tg,3, &
         allocate(product_fuel_by_s_u(0:max_start_up_units_by_tg,3, &
         allocate ( &
         allocate ( &
         allocate ( &
         allocate ( &
         allocate ( &
         allocate (tranc_database(1,1,1,1))
       allocate (generation_by_segment(max_outage_blocks,800))
       allocate(daily_cum_dispatch_mw(max_outage_blocks,24))
      allocate(depth_hourly_dispatch_mw(max_outage_blocks))
      allocate(daily_cum_dispatch_cost(max_outage_blocks,24))
      allocate(daily_gen_by_s_u(0:no_start_up_units,24))
      allocate(daily_fuel_by_s_u( &
      allocate(daily_system_cost_and_rev(0:2,24))
      allocate(daily_cost_by_s_u(0:no_start_up_units,24))
      allocate(daily_spin_by_s_u(0:no_start_up_units,24))
      allocate(month_must_by_block(max_outage_blocks,800))
      allocate(daily_must_run_capacity(24))
      allocate(daily_emergency_capacity(24))
      allocate(daily_hard_wired_by_block(24,max_outage_blocks))
      allocate(incremental_fuel_cost(max_outage_blocks))
      allocate(option_name(0:no_start_up_units))
      allocate(contributes_to_spin(no_start_up_units))
      allocate(month_unit_starts(no_start_up_units))
      allocate(month_unit_downs(no_start_up_units))
      allocate(in_min_down_time_state(no_start_up_units,800))
      allocate(in_min_up_time_state(no_start_up_units,800))
      allocate(start_up_costs(no_start_up_units))
      allocate(emergency_capacity(no_start_up_units))
      allocate(min_spin_cap(no_start_up_units))
      allocate(max_spin_cap(no_start_up_units))
      allocate(hourly_emergency_capacity(no_start_up_units))
      allocate(max_hourly_ramp_up(0:max_outage_blocks))
      allocate(max_hourly_ramp_down(0:max_outage_blocks))
      allocate(emergency_heatrate(no_start_up_units))
      allocate(month_unit_start_cost(no_start_up_units))
      allocate(month_operating_hours(no_start_up_units))
      allocate(must_run_block(max_outage_blocks))
      allocate(unit_is_up(no_start_up_units))
      allocate(original_value_of_u(no_start_up_units))
      allocate(increasing_value_of_u(no_start_up_units))
      allocate(global_block_index(max_outage_blocks))
      allocate(min_up_time(no_start_up_units))
      allocate(min_down_time(no_start_up_units))
      allocate(ramp_rate(no_start_up_units))
      allocate(ramp_down_rate(no_start_up_units))
      allocate(start_up_cost_per_mwh(no_start_up_units))
      allocate(emergency_cost_per_mwh(no_start_up_units))
      allocate(minimum_up_index(no_start_up_units))
      allocate(sorted_options(max_outage_blocks))
      allocate(floor_sorted_options(max_outage_blocks))
      allocate(ceiling_sorted_options(max_outage_blocks))
      allocate(must_run_options(max_outage_blocks))
      allocate(unit_for_outage_block(max_outage_blocks))
      allocate(s_u_for_outage_block(max_outage_blocks))
      allocate(block_for_outage_block(max_outage_blocks))
      allocate(dispatch_cost_for_block(max_outage_blocks))
      allocate(alt_dispatch_cost_for_block(max_outage_blocks))
      allocate(floor_price_for_block(max_outage_blocks))
      allocate(ceiling_price_for_block(max_outage_blocks))
      allocate(market_floor_unit(no_start_up_units))
      allocate(market_ceiling_unit(no_start_up_units))
      allocate(retail_revenue_for_block(max_outage_blocks))
      allocate(unit_gen_in_system(2,0:no_start_up_units))
      allocate(unit_heat_param(2,0:no_start_up_units))
      allocate(unit_spin_in_system(2,0:no_start_up_units))
      allocate(outage_block_by_segment(2,no_start_up_units))
      allocate(market_resource_counter(total_start_up_units))
      allocate(daily_option_status(0:max_days,no_start_up_units))
      allocate(pricing_group_sell_price( &
      allocate(pricing_group_sell_name(local_pricing_groups))
      allocate(pricing_group_sell_quant(0:local_pricing_groups))
      allocate(pricing_group_sell_quant_avail( &
      allocate(pricing_group_buy_quant_avail( &
      allocate(pricing_group_sell_gen(0:local_pricing_groups,24))
      allocate(pricing_group_sell_index(local_pricing_groups))
      allocate(pricing_group_buy_price( &
      allocate(pricing_group_buy_name( &
      allocate(pricing_group_buy_quant(0: &
      allocate(pricing_group_buy_index(local_buy_pricing_groups))
      allocate(all_resource_type(all_resource_number))
      allocate(all_resource_index(all_resource_number))
      allocate(all_demand_type(all_demand_number))
      allocate(all_demand_index(all_demand_number))
      allocate(all_resource_price(all_resource_number))
      allocate(all_resource_quantity(all_resource_number))
      allocate(all_resource_quantity_used(all_resource_number))
      allocate(all_demand_served(0:local_pricing_groups, &
      allocate(all_demand_price(all_demand_number))
      allocate(all_demand_quantity(all_demand_number))
      allocate(all_demand_name(all_demand_number))
      allocate(all_demand_buy_gen(0:all_demand_number,24))
      allocate(hourly_resources_and_markets( &
         allocate(joint_dispatch_order(max_nblok,4))
         allocate(combined_gen_in_system(2,local_nunits), &
      allocate(multi_area_name(0:max_trans_groups))
      allocate(trans_group_load_active(max_trans_group_number))
      allocate(report_area_active(max_trans_groups))
      allocate(monthly_market_prices(hours_in_month))
         allocate(annual_market_prices(annual_hours)) 
            allocate(annual_market_prices(annual_hours)) 
               allocate( &
               allocate( &
               allocate(max_user_w_price(upper_trans_group))
               allocate(min_user_w_price(upper_trans_group))
               allocate(month_max_user_w_price(12,upper_trans_group))
               allocate(month_min_user_w_price(12,upper_trans_group))
               allocate(sum_user_w_prices(upper_trans_group))
               allocate(month_sum_user_w_prices(12,upper_trans_group))
               allocate( &
               allocate(max_user_w_price(upper_trans_group))
               allocate(min_user_w_price(upper_trans_group))
               allocate(month_max_user_w_price(12,upper_trans_group))
               allocate(month_min_user_w_price(12,upper_trans_group))
               allocate(sum_user_w_prices(upper_trans_group))
               allocate(month_sum_user_w_prices(12,upper_trans_group))
            allocate(m_annual_purchase_energy(upper_trans_group))
            allocate(m_annual_purchase_costs(upper_trans_group))
            allocate(m_annual_sales_energy(upper_trans_group))
            allocate(m_annual_sales_revenues(upper_trans_group))
            allocate(m_annual_native_cost(upper_trans_group))
            allocate(m_annual_load_b4_sales(upper_trans_group))
            allocate(m_annual_pro_cost_b4_sales(upper_trans_group))
            allocate(m_annual_load_after_sales(upper_trans_group))
            allocate(m_annual_pro_cost_after_sales(upper_trans_group))
            allocate(annual_tl_mwh(upper_trans_group))
            allocate(annual_tl_peak(upper_trans_group))
            allocate(annual_coin_peak(0:upper_trans_group))
            allocate(annual_tl_base(upper_trans_group))
            allocate(annual_tl_hydro_mwh(upper_trans_group))
            allocate(annual_tl_hydro_mw(upper_trans_group))
            allocate(annual_tl_hydro_ror(upper_trans_group))
            allocate(annual_spinning_mwh(upper_trans_group))
            allocate(annual_effective_capacity(upper_trans_group))
            allocate(annual_unserved_energy(upper_trans_group,0:12))
            allocate(annual_above_resources(upper_trans_group,0:12))
            allocate(annual_unserved_energy_cost(upper_trans_group))
            allocate(annual_cost_above_resources(upper_trans_group, &
            allocate(annual_product_price(0:upper_trans_group, &
            allocate(annual_product_quantity(0:upper_trans_group, &
            allocate(annual_product_heatrate(0:upper_trans_group, &
            allocate(annual_product_marginal_fuel(0:upper_trans_group, &
            allocate(annual_product_fuel_price(0:upper_trans_group, &
            allocate(annual_product_hours(num_products))
            allocate(system_hourly_loads( &
         allocate(m_monthly_pro_cost_after_sales(upper_trans_group))
         allocate(m_monthly_pro_cost_b4_sales(upper_trans_group))
         allocate(m_monthly_load_b4_sales(upper_trans_group))
         allocate(m_monthly_load_after_sales(upper_trans_group))
         allocate(m_purchase_energy(upper_trans_group))
         allocate(m_purchase_costs(upper_trans_group))
         allocate(market_cost_above_resources(upper_trans_group))
         allocate(m_sales_energy(upper_trans_group))
         allocate(m_sales_revenues(upper_trans_group))
         allocate(m_native_cost(upper_trans_group))
         allocate(m_spinning_mwh(upper_trans_group))
         allocate(m_unserved_energy(upper_trans_group))
         allocate(m_above_resources(upper_trans_group))
         allocate(m_unserved_energy_cost(upper_trans_group))
         allocate(trans_ror_capacity(0:upper_trans_group))
         allocate(hourly_dump_capacity(upper_trans_group))
         allocate(trans_must_capacity(upper_trans_group))
         allocate(trans_spinning_capacity(upper_trans_group))
         allocate(area_price_mult(upper_trans_group))
         allocate(tg_using_price_distn(upper_trans_group))
         allocate(hourly_spinning_capacity(upper_trans_group))
         allocate(daily_peak(upper_trans_group))
         allocate(off_peak_spinning_capacity(upper_trans_group))
         allocate(trans_ramp_up(upper_trans_group))
         allocate(trans_ramp_down(upper_trans_group))
         allocate(trans_max_import(upper_trans_group))
         allocate(trans_max_export(upper_trans_group))
         allocate(last_hour_sell(upper_trans_group))
         allocate(month_coin_peak(0:upper_trans_group))
         allocate(month_non_coin_peak(0:upper_trans_group))
         allocate(hourly_transaction(upper_trans_group))
         allocate(max_hourly_import(upper_trans_group))
         allocate(max_hourly_export(upper_trans_group))
         allocate(scarcity_mult(0:upper_trans_group))
         allocate(system_storage(0:800,0:upper_trans_group))
         allocate(system_derivatives(0:800,upper_trans_group))
            allocate(fiscal_purchase_energy(upper_trans_group))
            allocate(fiscal_purchase_costs(upper_trans_group))
            allocate(fiscal_sales_energy(upper_trans_group))
            allocate(fiscal_sales_revenues(upper_trans_group))
            allocate(fiscal_native_cost(upper_trans_group))
            allocate(fiscal_load_b4_sales(upper_trans_group))
            allocate(fiscal_pro_cost_b4_sales(upper_trans_group))
            allocate(fiscal_load_after_sales(upper_trans_group))
            allocate(fiscal_pro_cost_after_sales(upper_trans_group))
            allocate(fiscal_tl_mwh(upper_trans_group))
            allocate(fiscal_tl_peak(upper_trans_group))
            allocate(fiscal_coin_peak(0:upper_trans_group))
            allocate(fiscal_tl_base(upper_trans_group))
            allocate(fiscal_tl_hydro_mwh(upper_trans_group))
            allocate(fiscal_tl_hydro_mw(upper_trans_group))
            allocate(fiscal_tl_hydro_ror(upper_trans_group))
            allocate(fiscal_spinning_mwh(upper_trans_group))
            allocate(fiscal_effective_capacity(upper_trans_group))
            allocate(fiscal_unserved_energy(upper_trans_group))
            allocate(fiscal_above_resources(upper_trans_group))
            allocate(fiscal_unserved_energy_cost(upper_trans_group))
            allocate(fiscal_cost_above_resources(upper_trans_group))
         allocate(all_month_prices(r_hours_in_month, &
         allocate(hourly_marginal_cost(0:upper_trans_group,daily_hours))
         allocate(hourly_last_price(0:upper_trans_group,daily_hours))
         allocate(daily_products_capacity( &
         allocate(hourly_in_price(0:upper_trans_group,daily_hours))
         allocate(hourly_mc_after(0:upper_trans_group,daily_hours))
         allocate(system_output(daily_hours))
         allocate(tie_flow(0:upper_trans_group,daily_hours))
         allocate(hourly_load_b4_sales(0:upper_trans_group,daily_hours))
         allocate(hourly_transfer_mwh(upper_trans_group,daily_hours))
         allocate(hourly_forwards_4_month(upper_trans_group, &
         allocate(daily_market_price(daily_hours))
         allocate(hourly_loads(0:upper_trans_group,daily_hours))
         allocate(test_hourly_revenue(0:upper_trans_group,daily_hours))
         allocate(marginal_cost_delta(0:upper_trans_group,daily_hours))
         allocate(hourly_pro_cost_after_sales(0:upper_trans_group, &
         allocate(hourly_capacity(0:upper_trans_group,daily_hours))
         allocate(hourly_eue(0:upper_trans_group,daily_hours))
         allocate(hourly_derivatives(0:upper_trans_group,daily_hours))
         allocate(hourly_pro_cost_b4_sales(0:upper_trans_group, &
         allocate(hourly_incremental_cost(0:upper_trans_group, &
         allocate(hourly_lamda(0:upper_trans_group,daily_hours))
         allocate(product_price(0:upper_trans_group,num_products))
         allocate(product_volatility(0:upper_trans_group,num_products))
         allocate(product_quantity(0:upper_trans_group,num_products))
         allocate(product_heatrate(0:upper_trans_group,num_products))
         allocate(product_marginal_fuel(0:upper_trans_group, &
         allocate(product_fuel_price(0:upper_trans_group,num_products))
         allocate(product_hours(num_products))
         allocate(product_mean_return(0:upper_trans_group,num_products))
         allocate(sum_squared_deviations( &
         allocate(product_daily_return( &
         allocate(scarcity_cost(0:upper_trans_group,daily_hours))
         allocate(energy_cost(0:upper_trans_group,daily_hours))
            allocate(product_last_price( &
         allocate(hourly_marginal_cost(0:upper_trans_group,daily_hours))
         allocate(last_trans_mc(0:upper_trans_group))
         allocate(hourly_last_price(0:upper_trans_group,daily_hours))
         allocate(m_hourly_mc_b4_sales(0:upper_trans_group,daily_hours))
         allocate(last_buyer(upper_trans_group))
         allocate(last_seller(upper_trans_group))
         allocate(price_anchored(upper_trans_group))
         allocate(hourly_lamda(upper_trans_group,daily_hours))
         allocate(transactions_within_hour(daily_hours))
         allocate(buy_for_transaction(max_trans_within_hour))
         allocate(sell_for_transaction(max_trans_within_hour))
         allocate(cum_redundant_transaction(max_trans_within_hour))
         allocate(last_buy_for_transaction(max_trans_within_hour))
         allocate(last_sell_for_transaction(max_trans_within_hour))
         allocate(long_path_transaction(max_trans_within_hour))
         allocate(margin_for_transaction(max_trans_within_hour))
         allocate(mw_for_transaction(max_trans_within_hour))
         allocate(last_mw_for_transaction(max_trans_within_hour))
         allocate(tie_flow(upper_trans_group,daily_hours))
         allocate(marginal_cost_delta(upper_trans_group,daily_hours))
         allocate(m_hourly_loads(upper_trans_group,daily_hours))
         allocate(test_hour_tie_limit(upper_trans_group, &
         allocate(m_hourly_pro_cost_after_sales(upper_trans_group, &
         allocate(hourly_capacity(upper_trans_group,daily_hours))
         allocate(hourly_derivatives(upper_trans_group,daily_hours))
         allocate(hourly_eue(upper_trans_group,daily_hours))
         allocate(m_hourly_pro_cost_b4_sales(upper_trans_group, &
         allocate(m_hourly_incremental_cost(upper_trans_group, &
         allocate(hourly_sell_mwh(total_buyer_index,total_buyer_index))
         allocate(monthly_transaction_mwh(upper_trans_group, &
         allocate(total_delivered_cost(upper_trans_group, &
         allocate(sellers_local_capacity(upper_trans_group, &
         allocate(m_monthly_pro_cost_after_sales(upper_trans_group))
         allocate(m_monthly_pro_cost_b4_sales(upper_trans_group))
         allocate(m_monthly_load_b4_sales(upper_trans_group))
         allocate(m_monthly_load_after_sales(upper_trans_group))
         allocate(m_purchase_energy(upper_trans_group))
         allocate(m_purchase_costs(upper_trans_group))
         allocate(market_cost_above_resources(upper_trans_group))
         allocate(m_sales_energy(upper_trans_group))
         allocate(m_sales_revenues(upper_trans_group))
         allocate(m_native_cost(upper_trans_group))
         allocate(m_spinning_mwh(upper_trans_group))
         allocate(allowed_transaction_pair(upper_trans_group, &
         allocate(transactions_per_hour(upper_trans_group, &
         allocate(long_path_transaction_pair(upper_trans_group, &
         allocate(m_unserved_energy(upper_trans_group))
         allocate(m_above_resources(upper_trans_group))
         allocate(capped_price(upper_trans_group))
         allocate(price_minimum(upper_trans_group))
         allocate(m_unserved_energy_cost(upper_trans_group))
         allocate(net_margin(upper_trans_group))
         allocate(best_price_to_buyer(upper_trans_group,2))
         allocate(m_hourly_load_b4_sales( &
         allocate(trans_ror_capacity(0:upper_trans_group))
         allocate(hourly_dump_capacity(upper_trans_group))
         allocate(hourly_dump_before(upper_trans_group))
         allocate(trans_must_capacity(upper_trans_group))
         allocate(trans_spinning_capacity(upper_trans_group))
         allocate(hourly_spinning_capacity(upper_trans_group))
         allocate(daily_peak(upper_trans_group))
         allocate(off_peak_spinning_capacity(upper_trans_group))
         allocate(trans_ramp_up(upper_trans_group))
         allocate(trans_ramp_down(upper_trans_group))
         allocate(trans_max_import(upper_trans_group))
         allocate(trans_max_export(upper_trans_group))
         allocate(last_hour_sell(upper_trans_group))
         allocate(month_coin_peak(0:upper_trans_group))
         allocate(month_non_coin_peak(0:upper_trans_group))
         allocate(hourly_transaction(upper_trans_group))
         allocate(max_hourly_import(upper_trans_group))
         allocate(max_hourly_export(upper_trans_group))
         allocate(scarcity_mult(0:upper_trans_group))
         allocate(hourly_trans_group_load_active(upper_trans_group))
         allocate(hourly_trans_group_gen_active(upper_trans_group))
         allocate(product_price(0:upper_trans_group,num_products))
         allocate(product_volatility(0:upper_trans_group,num_products))
         allocate(product_quantity(0:upper_trans_group,num_products))
         allocate(product_heatrate(0:upper_trans_group,num_products))
         allocate(product_marginal_fuel( &
         allocate(product_fuel_price(0:upper_trans_group,num_products))
         allocate(product_hours(num_products))
         allocate(product_mean_return(0:upper_trans_group,num_products))
         allocate(product_scarcity(0:upper_trans_group,num_products))
         allocate(sum_squared_deviations( &
         allocate(scarcity_cost(0:upper_trans_group,daily_hours))
         allocate(energy_cost(0:upper_trans_group,daily_hours))
         allocate(product_daily_return(0:upper_trans_group, &
            allocate( &
            allocate(m_annual_purchase_energy(upper_trans_group))
            allocate(m_annual_purchase_costs(upper_trans_group))
            allocate(m_annual_sales_energy(upper_trans_group))
            allocate(m_annual_sales_revenues(upper_trans_group))
            allocate(m_annual_native_cost(upper_trans_group))
            allocate(m_annual_load_b4_sales(upper_trans_group))
            allocate(m_annual_pro_cost_b4_sales(upper_trans_group))
            allocate(m_annual_load_after_sales(upper_trans_group))
            allocate(m_annual_pro_cost_after_sales(upper_trans_group))
            allocate(annual_tl_mwh(upper_trans_group))
            allocate(annual_tl_peak(upper_trans_group))
            allocate(annual_coin_peak(0:upper_trans_group))
            allocate(annual_tl_base(upper_trans_group))
            allocate(annual_tl_hydro_mwh(upper_trans_group))
            allocate(annual_tl_hydro_mw(upper_trans_group))
            allocate(annual_tl_hydro_ror(upper_trans_group))
            allocate(annual_spinning_mwh(upper_trans_group))
            allocate(annual_effective_capacity(upper_trans_group))
            allocate(annual_unserved_energy(upper_trans_group,0:12))
            allocate(annual_above_resources(upper_trans_group,0:12))
            allocate(annual_unserved_energy_cost(upper_trans_group))
            allocate(annual_cost_above_resources(upper_trans_group, &
            allocate(annual_transaction_mwh(upper_trans_group, &
            allocate(annual_product_price(0:upper_trans_group, &
            allocate(annual_product_scarcity(0:upper_trans_group, &
            allocate(annual_product_quantity(0:upper_trans_group, &
            allocate(annual_product_heatrate(0:upper_trans_group, &
            allocate(annual_product_marginal_fuel(0:upper_trans_group, &
            allocate(annual_product_fuel_price(0:upper_trans_group, &
            allocate(annual_product_hours(num_products))
         allocate(hourly_trans_revenue(max_tie_groups,daily_hours))
      allocate(trans_class_pointer(0:1023), &
            allocate(transaction_name(num_transactions))
            allocate(monthly_user_cf(num_transactions))
            allocate(monthly_user_cf_counter(num_transactions))
            allocate(monthly_cf_trans(num_transactions))
            allocate(user_cf(num_transactions))
            allocate(counterparty_name(num_transactions))
            allocate(transaction_id(num_transactions))
            allocate(transaction_group(num_transactions))
            allocate(transaction_type(num_transactions))
            allocate(derivative_type(num_transactions))
            allocate(product_index(num_transactions))
            allocate(transaction_class(num_transactions))
            allocate(product_type(num_transactions))
            allocate(option_position(num_transactions))
            allocate(product_active(num_transactions))
            allocate(strike_frequency(num_transactions))
            allocate(report_product(num_transactions))
            allocate(unit_contingency(num_transactions))
            allocate(unit_contingent_link(num_transactions))
            allocate(energy_multiplier(num_transactions))
            allocate(trans_active_in_endpoint(num_transactions))
            allocate(heat_rate_for_spread(num_transactions))
            allocate(user_day_types_id(num_transactions))
            allocate(pumping_capacity(num_transactions))
            allocate(pumping_storage_efficiency(num_transactions))
            allocate(daily_pumping_mult(num_transactions))
            allocate(fuel_type(num_transactions))
            allocate(fuel_price_type(num_transactions))
            allocate(fuel_price(num_transactions))
            allocate(transportation_basis_type(num_transactions))
            allocate(transportation_basis(num_transactions))
            allocate(delivery_adder(num_transactions))
            allocate(counterparty_bond_rating(num_transactions))
            allocate(dollar_mwh(num_transactions))
            allocate(wvpa_rate_tracker(num_transactions))
            allocate(wvpa_mem_tracker(num_transactions), &
            allocate(tax_credit_begin_date(num_transactions), &
            allocate(grx_rps_capacity(num_transactions,30))
            allocate(save_grx_rps_capacity(num_transactions,30))
            allocate(grx_storage_pattern(num_of_trans_classes,8760))
            allocate(grx_indep_pattern(num_of_trans_classes,8760))
            allocate(save_grx_storage_pattern &
            allocate(energy_price_multiplier(num_transactions))
            allocate(max_quantity_of_product(num_transactions))
            allocate(monthly_energy_mult(num_transactions))
            allocate(contract_date(num_transactions))
            allocate(distinguishing(num_transactions))
            allocate(reporting_generation_group(num_transactions))
            allocate(expense_assignment(num_transactions))
            allocate(expense_collection(num_transactions))
            allocate(asset_class_id(num_transactions))
            allocate(asset_allocation_vector(num_transactions))
            allocate(quantity_of_product(num_transactions))
            allocate(proposed_quant_of_product(num_transactions))
            allocate(saved_quant_of_product(num_transactions))
            allocate(hourly_quantity(num_transactions))
            allocate(energy_price(num_transactions))
            allocate(monthly_energy_price(num_transactions))
            allocate(contingent_capacity(24,num_transactions))
            allocate(monthly_2nd_energy_price(num_transactions))
            allocate(price_type(num_transactions))
            allocate(second_energy_price(num_transactions))
            allocate(maximum_strikes(num_transactions))
            allocate(billing_lag(num_transactions))
            allocate(minimum_strikes(num_transactions))
            allocate(dollar_kw_day(num_transactions))
            allocate(dollar_kw_day_esc(num_transactions))
            allocate(dollar_kw_month(num_transactions))
            allocate(dollar_kw_month_esc(num_transactions))
            allocate(dollar_kw_year(num_transactions))
            allocate(grx_dollar_kw_year(num_transactions))
            allocate(grx_dollar_kw_month(num_transactions))
            allocate(dollar_kw_year_esc(num_transactions))
            allocate(dollar_month(num_transactions))
            allocate(dollar_month_esc(num_transactions))
            allocate(dollar_deal(num_transactions))
            allocate(dollar_deal_esc(num_transactions))
            allocate(begin_day(num_transactions))
            allocate(begin_day_in_month(num_transactions))
            allocate(begin_ep(num_transactions))
            allocate(end_day(num_transactions))
            allocate(end_day_in_month(num_transactions))
            allocate(end_ep(num_transactions))
            allocate(active_in_month(num_transactions))
            allocate(trans_to_active_month(num_transactions))
            allocate(active_in_year(num_transactions))
            allocate(active_in_year_index(num_transactions))
            allocate(asset_class_groups_index(0:max_asset_groups))
            allocate(asset_2_trans_index(0:max_asset_groups, &
            allocate(number_asset_2_trans(max_trans_groups))
            allocate(first_ac_tg(0:max_asset_groups))
            allocate(num_forwards(num_of_trans_classes))
            allocate(monthly_contingent_capacity(num_of_trans_classes))
            allocate(annual_interruptible_capacity( &
            allocate(annu_planning_intrpt_capacity( &
            allocate(annual_storage_capacity(num_of_trans_classes))
            allocate(annual_capacity(744,12,num_of_trans_classes))
            allocate(num_puts(num_of_trans_classes))
            allocate(num_storage(num_of_trans_classes))
            allocate(ann_num_stor(num_of_trans_classes))
            allocate(scen_num_stor(num_of_trans_classes))
            allocate(ann_stor_pos(num_transactions, &
            allocate(scen_stor_pos(num_transactions, &
            allocate(num_lf_puts(num_of_trans_classes))
            allocate(num_month_puts(num_of_trans_classes))
            allocate(num_annual_puts(num_of_trans_classes))
            allocate(num_inter_puts(num_of_trans_classes))
            allocate(forward_position(num_transactions, &
            allocate(put_position(num_transactions, &
            allocate(month_put_position(num_transactions, &
            allocate(annual_put_position(num_transactions, &
            allocate(storage_position(num_transactions, &
            allocate(battery_position(num_transactions, &
            allocate(lf_put_position(num_transactions, &
            allocate(inter_put_position(num_transactions, &
            allocate(trans_e_p_index(max_trans_group_num))
            allocate(trans_ep_2_trans_groups(max_trans_group_num))
            allocate(trans_ep_2_tg(max_trans_group_num))
            allocate(years_beg_day_in_mo(num_transactions,0:12))
            allocate(years_end_day_in_mo(num_transactions,0:12))
            allocate(rps_program_number(num_transactions), &
            allocate(fiscal_energy_cost(num_transactions))
            allocate(fiscal_energy(num_transactions))
            allocate(fiscal_capacity(num_transactions))
            allocate(fiscal_energy_revenue(num_transactions))
            allocate(fiscal_trans_hours(num_transactions))
            allocate(fiscal_product_hours(num_transactions))
            allocate(fiscal_product_days(num_transactions))
            allocate(fiscal_product_months(num_transactions))
            allocate(fiscal_strikes(num_transactions))
            allocate(fiscal_active_in_year_index(num_transactions))
            allocate(fiscal_active_in_year(num_transactions))
            allocate(fiscal_transaction_cost(num_transactions))
            allocate(fiscal_transaction_revenue(num_transactions))
         allocate(monthly_energy_cost(num_transactions,0:12))
         allocate(monthly_energy(num_transactions,0:12))
         allocate(monthly_charge(num_transactions,0:12))
         allocate(monthly_enr_for_rev(num_transactions,0:12))
         allocate(monthly_enr_for_exp(num_transactions,0:12))
         allocate(monthly_capacity(num_transactions,0:12))
         allocate(monthly_energy_revenue(num_transactions,0:12))
         allocate(monthly_ac_revenue(0:max_asset_class_groups,4,0:12))
         allocate(monthly_ac_revenue_energy( &
         allocate(monthly_ac_expense(0:max_asset_class_groups,4,0:12))
         allocate(monthly_ac_expense_energy( &
         allocate(monthly_trans_hours(num_transactions,0:12))
         allocate(monthly_product_hours(num_transactions,0:12))
         allocate(monthly_product_days(num_transactions,0:12))
         allocate(monthly_product_months(num_transactions))
         allocate(monthly_strikes(num_transactions,0:12))
         allocate(monthly_transaction_cost(num_transactions,0:12))
         allocate(monthly_transaction_revenue(num_transactions,0:12))
         allocate(strikes_available(num_transactions))
         allocate(strikes_required(num_transactions))
            allocate(monthly_ct_group_report(0:12,0:max_monthly_groups, &
         allocate(hrly_tg_scen_elect_mult(744,12,r_tg))
         allocate(obs(price_points,r_max_trans_groups)) ! don't need monthly
         allocate(lprob(price_points,r_max_trans_groups)) ! don't need monthly
         allocate(loddur(price_points,r_max_trans_groups,13)) !  done.
         allocate(cum_hours(price_points,r_max_trans_groups,13)) ! done.
         allocate(cum_revenue(price_points,r_max_trans_groups,13)) ! need monthly
         allocate(energy(price_points,r_max_trans_groups)) ! don't need monthly
         allocate(trans_group_position(-1:max_trans_group_number))
         allocate(seller_transaction_group(max_paths))
         allocate(long_path_path(max_paths))
         allocate(active_path_for_long_path(max_paths))
         allocate(belongs_to_a_long_path(max_paths))
         allocate(buyer_transaction_group(max_paths))
         allocate(path_percent(max_paths))
         allocate(wheel_path(max_paths,max_wheels))
         allocate(paths_per_pair(-1:num_trans_groups, &
         allocate(long_path_for_pair(-1:num_trans_groups, &
         allocate(hour_long_path_for_pair(-1:num_trans_groups, &
         allocate(hour_paths_per_pair(-1:num_trans_groups, &
         allocate(hour_path_for_long_path(max_paths))
         allocate(hour_wheel_path(max(int(1,2),max_hourly_long_paths), &
         allocate(paths_index(-1:num_trans_groups,-1:num_trans_groups, &
         allocate(hour_paths_index( &
         allocate(ties_per_pair(-1:num_trans_groups, &
         allocate(tie_index(-1:num_trans_groups,-1:num_trans_groups, &
         allocate(tie_wheel_index(-1:num_trans_groups, &
         allocate(season_path_limit(-1:num_trans_groups, &
         allocate(hour_path_limit(-1:num_trans_groups, &
         allocate(hour_path_mw(-1:num_trans_groups, &
         allocate(save_hour_path_limit(-1:num_trans_groups, &
         allocate(hour_tie_loading(-1:num_trans_groups, &
         allocate(daily_tie_loading(max_paths,24))
         allocate(daily_tie_constraint(max_paths,24))
         allocate(path_name(max_paths))
         allocate(long_path_name(max_paths))
         allocate(path_wheel_rate(max_paths))
         allocate(path_spread(max_paths))
         allocate(path_owner(max_paths))
         allocate(path_kv_rating(max_paths))
         allocate(path_inductance(max_paths))
         allocate(path_wheel_mult(max_paths))
         allocate(path_spread_mult(max_paths))
         allocate(path_spread_off_mult(max_paths))
         allocate(path_wheel_off_mult(max_paths))
         allocate(market_price_id(max_paths))
         allocate(scenario_num(max_paths))
         allocate(market_price_delta(max_paths))
         allocate(trans_line_constraint(max_paths))
         allocate(peak_price_delta_mult(max_paths))
         allocate(off_peak_price_delta_mult(max_paths))
         allocate(trans_line_index(max_paths))
         allocate(grx_id(max_paths))
         allocate(tg_used_in_path(-1:max_trans_group_number))
         allocate(reverse_path(-1:num_trans_groups, &
         allocate(multi_area_month_price(r_hours_in_month,temp_i2))
         allocate(sell_beg_fo_hr_tl(active_path_number))
         allocate(sell_end_fo_hr_tl(active_path_number))
         allocate(multi_area_buy_month_price(r_hours_in_month,temp_i2))
         allocate(buy_beg_fo_hr_tl(active_path_number))
         allocate(buy_end_fo_hr_tl(active_path_number))
         allocate(trans_group_position(-1:max_trans_group_number))
         allocate(constraint_id(max_constraints))
         allocate(season(max_constraints))
         allocate(mw_limit(max_constraints))
         allocate(from_area(max_constraints,max_constraint_areas))
         allocate(to_area(max_constraints,max_constraint_areas))
         allocate(peak_mult(max_constraints))
         allocate(off_peak_mult(max_constraints))
         allocate(forced_outage_rate(max_constraints))
         allocate(forced_outage_derate_mw(max_constraints))
         allocate(trans_line_position(0:max_trans_line_index))
         allocate(constraint_percent(max_constraints))
         allocate(captransmultiplier(0:num_trans_groups, &
         allocate(constraints_per_pair( &
         allocate(local_season(max_constraints))
         allocate(constraints_index( &
         allocate(ties_per_pair(-1:num_trans_groups, &
         allocate(tie_index(-1:num_trans_groups,-1:num_trans_groups, &
         allocate(tie_wheel_index(-1:num_trans_groups, &
         allocate(season_constraint_limit( &
         allocate(hour_constraint_limit( &
         allocate(hour_tie_loading(-1:num_trans_groups, &
         allocate(vector_values(active_vectors,avail_data_years), &
         allocate(   temp_trans_class_pointer(0:1023), &
      allocate(temp_trans_class_pointer(0:1023), &
            allocate( &
      allocate(z(ncases))
      allocate(orgorder(ncases))
      allocate(orgorder(ncases))
         allocate(   temp_trans_class_pointer(0:1023), &
      allocate(temp_trans_class_pointer(0:1023), &
            allocate( &
            allocate( &
         allocate( &
      allocate(capacity_adder(0:trans_groups_records), &
            allocate( &
         allocate (user_day_data(hours_per_day,user_days_in_month, &
         allocate(cf_user_day_data(24,7,r_max_user_id))
      allocate( &
            allocate( depth_price(depth_market_intervals, &
         allocate(   temp_trans_class_pointer(0:1023), &
      allocate(   temp_trans_class_pointer(0:1023), &
            allocate(transaction_name(num_transactions), &
            allocate(contract_begin_date(num_transactions), &
            allocate(transportation_basis(num_transactions), &
            allocate(begin_day_in_month(num_transactions), &
         allocate( &
            allocate( &
            allocate(regional_variable_active(num_scen_var), &
            allocate( &
         allocate(report_data(0:max_comp_id,0:max_accounts_tracked,0:15,0:12,0:30), &               
         allocate(out_record(0:max_accounts_tracked,plant_bal_rept_cols))
         allocate(rate_code_list(0:num_rate_tables), &
         allocate(temp_asset_class_pointer(1024))
            allocate(bc_asset_class_pointer(max_bc_class_id_num))
         allocate(wvpa_tracked_expenses(0:12,30,0:4), &
            allocate(actual_values(12,7,200), &
