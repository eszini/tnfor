module class_fin_results
implicit none
    type t_class_fin_results
        real :: INC_SALES_REVENUES,INC_FUEL_EXPENSE
        real :: INC_VAROM_EXPENSE,INC_FIXEDOM_EXPENSE
        real :: INC_PURCHASE_EXPENSE,INC_NUCLEAR_FUEL_EXPENSE
        real :: INC_DSM_EXPENSE,INC_DSM_REBATE
        real :: INC_NON_UTIL_COSTS
        real :: X_PRICE_1,X_PRICE_2,X_PRICE_3
        real :: TAXINCOME
        real :: INC_ADJUSTMENT_CLAUSE_REVENUES
        real :: INC_BASE_RATES_REVENUES
        real :: INC_OTHER_REVENUES
        real :: INC_BTL_REVENUES
    end type t_class_fin_results
    
    ! SAVE required by Lahey, and should be implicit per the Fortran standard.
    type(t_class_fin_results), save :: ns_class_fin_results ! Namespace
    
end module class_fin_results
