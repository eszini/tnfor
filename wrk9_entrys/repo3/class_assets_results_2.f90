module class_assets_results_2
    implicit none
    type t_ns_class_assets_results_2
    real :: secondary_sales_revenues
    real :: atl_income_taxes
    real :: ouc_tax_payments
    end type t_ns_class_assets_results_2
    type (t_ns_class_assets_results_2), save :: &
        ns_class_assets_results_2
end module class_assets_results_2
