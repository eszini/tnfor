module class_run_switchesL4
  implicit none
  logical(kind=4) :: calculate_amt
end module class_run_switchesL4

module class_run_switchesL1
  implicit none
  logical(kind=1) :: normalize_federal_taxes
  logical(kind=1) :: normalize_state_taxes
  logical(kind=1) :: mortgage_debt
  logical(kind=1) :: calculate_btl_income_taxes
  logical(kind=1) :: use_burn_4_nuc_fuel_tax_exp
  logical(kind=1) :: place_holder
  logical(kind=1) :: use_budget_variable_om
  logical(kind=1) :: use_nf_burn_4_expense
  logical(kind=1) :: use_fed_tax_table
  logical(kind=1) :: use_deferred_fuel_accounting
  logical(kind=1) :: use_fasb_109_accounting
  logical(kind=1) :: use_budget_fuel_purchase
  logical(kind=1) :: use_all_fed_tax_benefits
  logical(kind=1) :: use_state_tax_benefits_now
  logical(kind=1) :: use_production_module_expense
  logical(kind=1) :: use_pga_accounting
  logical(kind=1) :: use_purchase_power_accounting
end module class_run_switchesL1

module class_run_switchesC
  implicit none
  character(len=1) :: dividend_payment_method
  character(len=1) :: afudc_return_policy
  character(len=1) :: return_on_ratebase_source
  character(len=1) :: operating_method
  character(len=1) :: price_source_for_price_driver
  character(len=1) :: equity_definition
  character(len=1) :: ratebase_valuation
end module class_run_switchesC

