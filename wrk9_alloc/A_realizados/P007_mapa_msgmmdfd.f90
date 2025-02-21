Type ( 9)          src:                       |msgmmdfd.f90|
Allocate number       :     1                 | 
alloc sentence in line:    81 (    0 -  1891) |allocate(temp_asset_class_pointer(1024))|
block starts at line  :    12                 |SUBROUTINE DD_OBJECT|
block ends   at line  :   334                 |END|
line to insert check  :    85 (   12 -   337) |TEMP_ASSET_CLASS_POINTER = 0|
key used              :     1                 | 
------------------------------------------( 1)-
Type ( 9)          src:                       |msgmmdfd.f90|
Allocate number       :     2                 | 
alloc sentence in line:   151 (    0 -  1896) |allocate(debit_bc_asset_class_pointer(max_bc_debit_class_id_num))|
block starts at line  :    12                 |SUBROUTINE DD_OBJECT|
block ends   at line  :   339                 |END|
line to insert check  :   152 (   12 -   339) |DO I = 1, MAX_BC_DEBIT_CLASS_ID_NUM|
key used              :     2                 | 
------------------------------------------( 1)-
Type ( 9)          src:                       |msgmmdfd.f90|
Allocate number       :     3                 | 
alloc sentence in line:   194 (    0 -  1898) |allocate(temp_asset_class_pointer(1024))|
block starts at line  :    12                 |SUBROUTINE DD_OBJECT|
block ends   at line  :   341                 |END|
line to insert check  :   195 (   12 -   341) |TEMP_ASSET_CLASS_POINTER = 0|
key used              :     3                 | 
------------------------------------------( 1)-
Type ( 9)          src:                       |msgmmdfd.f90|
Allocate number       :     4                 | 
alloc sentence in line:   266 (    0 -  1900) |allocate(debit_ol_asset_class_pointer(max_ol_debit_class_id_num))|
block starts at line  :    12                 |SUBROUTINE DD_OBJECT|
block ends   at line  :   343                 |END|
line to insert check  :   267 (   12 -   343) |DO I = 1, MAX_OL_DEBIT_CLASS_ID_NUM|
key used              :     4                 | 
------------------------------------------( 1)-
Type (15)          src:                       |msgmmdfd.f90|
Allocate number       :     5                 | 
alloc sentence in line:   559 (    0 -  1902) |allocate(asset_class_list(avail_data_years), &|
block starts at line  :   364                 |RECURSIVE SUBROUTINE DEFERRED_DEBITS(SAVE_BASE_CASE)|
block ends   at line  :  1903                 |END|
line to replace allocs:   561  (  5 allocs)   |ALLOCATE(ASSET_CLASS_LIST(AVAIL_DATA_YEARS), &| 
key used              :     5                 | 
allocate added        :     1                 |allocate(asset_class_list(avail_data_years),stat=stv_er)|
allocate added        :     2                 |allocate(asset_allocation_list(avail_data_years),stat=stv_er)|
allocate added        :     3                 |allocate(ann_amort(0:12,0:financial_simulation_years),stat=stv_er)|
allocate added        :     4                 |allocate(ann_cash_additions(0:12,0:financial_simulation_years),stat=stv_er)|
allocate added        :     5                 |allocate(monthly_amort_amount(0:12,0:financial_simulation_years),stat=stv_er)|
------------------------------------------( 5)-
Type ( 9)          src:                       |msgmmdfd.f90|
Allocate number       :     6                 | 
alloc sentence in line:  1851 (    0 -  1911) |allocate(asset_class_pointer(max_asset_class_num))|
block starts at line  :   364                 |RECURSIVE SUBROUTINE DEFERRED_DEBITS(SAVE_BASE_CASE)|
block ends   at line  :  1912                 |END|
line to insert check  :  1852 (  364 -  1912) |CALL RETURN_DEBIT_CLASS_POINTER(ASSET_CLASS_POINTER)|
key used              :    10                 | 
------------------------------------------( 1)-
Type (15)          src:                       |msgmmdfd.f90|
Allocate number       :     7                 | 
alloc sentence in line:  1871 (    0 -  1913) |allocate(tddb(0:12,0:financial_simulation_years,0:num_of_asset_classes,0:bal_sheet_options,unique_acct_type), &|
block starts at line  :   364                 |RECURSIVE SUBROUTINE DEFERRED_DEBITS(SAVE_BASE_CASE)|
block ends   at line  :  1914                 |END|
line to replace allocs:  1870  ( 20 allocs)   |ALLOCATE(TDDB(0:12,0:FINANCIAL_SIMULATION_YEARS,0:NUM_OF_ASSET_CLASSES,0:BAL_SHEET_OPTIONS,UNIQUE_ACCT_TYPE), &| 
key used              :    11                 | 
allocate added        :     1                 |allocate(tddb(0:12,0:financial_simulation_years,0:num_of_asset_classes,0:bal_sheet_options,unique_acct_type),stat=stv_er)|
allocate added        :     2                 |allocate(tddrbb(0:12,0:financial_simulation_years,0:num_of_asset_classes,unique_acct_type),stat=stv_er)|
allocate added        :     3                 |allocate(tamrte(0:12,0:financial_simulation_years,0:num_of_asset_classes,unique_acct_type),stat=stv_er)|
allocate added        :     4                 |allocate(tndb(0:12,0:financial_simulation_years,0:num_of_asset_classes,0:bal_sheet_options,unique_acct_type),stat=stv_er)|
allocate added        :     5                 |allocate(net_change_in_db(0:12,0:financial_simulation_years,0:num_of_asset_classes,0:bal_sheet_options,unique_acct_type),stat=stv_er)|
allocate added        :     6                 |allocate(tcamrt(0:12,0:financial_simulation_years,0:num_of_asset_classes,unique_acct_type),stat=stv_er)|
allocate added        :     7                 |allocate(rb_dd_amrte(0:12,0:financial_simulation_years,0:num_of_asset_classes,unique_acct_type),stat=stv_er)|
allocate added        :     8                 |allocate(interest_amortization(0:12,0:financial_simulation_years,0:num_of_asset_classes,unique_acct_type),stat=stv_er)|
allocate added        :     9                 |allocate(btl_amortization(0:12,0:financial_simulation_years,0:num_of_asset_classes,unique_acct_type),stat=stv_er)|
allocate added        :    10                 |allocate(atl_amortization(0:12,0:financial_simulation_years,0:num_of_asset_classes,unique_acct_type),stat=stv_er)|
allocate added        :    11                 |allocate(atl_amort_sub_items(0:12,0:financial_simulation_years,0:num_of_asset_classes,0:bal_sheet_options,unique_acct_type),stat=stv_er)|
allocate added        :    12                 |allocate(atl_def_tax(0:12,0:financial_simulation_years,0:num_of_asset_classes,unique_acct_type),stat=stv_er)|
allocate added        :    13                 |allocate(btl_def_tax(0:12,0:financial_simulation_years,0:num_of_asset_classes,unique_acct_type),stat=stv_er)|
allocate added        :    14                 |allocate(deferred_cash_additions(0:12,0:financial_simulation_years,0:num_of_asset_classes,unique_acct_type),stat=stv_er)|
allocate added        :    15                 |allocate(atl_current_tax_expense(0:12,0:financial_simulation_years,0:num_of_asset_classes,unique_acct_type),stat=stv_er)|
allocate added        :    16                 |allocate(btl_current_tax_expense(0:12,0:financial_simulation_years,0:num_of_asset_classes,unique_acct_type),stat=stv_er)|
allocate added        :    17                 |allocate(variable_om_amort(0:12,0:financial_simulation_years,0:num_of_asset_classes,unique_acct_type),stat=stv_er)|
allocate added        :    18                 |allocate(other_om_amort(0:12,0:financial_simulation_years,0:num_of_asset_classes,unique_acct_type),stat=stv_er)|
allocate added        :    19                 |allocate(purchase_power_amort(0:12,0:financial_simulation_years,0:num_of_asset_classes,unique_acct_type),stat=stv_er)|
allocate added        :    20                 |allocate(tax_timing_differences(0:financial_simulation_years,0:num_of_asset_classes,2,2,unique_acct_type),stat=stv_er)|
------------------------------------------( 5)-
