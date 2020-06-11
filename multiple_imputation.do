use "statin_phenotypic_to_impute.dta", clear

ssc install ice

set more off

log using multiple_imputation.log, replace

ice o.ethnicity  age bmi sbp sd_sbp town i.b_AF i.b_atypicalantipsy i.b_corticosteroids i.b_migraine i.b_ra i.b_renal i.b_semi i.b_treatedhyp i.b_type1 i.b_type2 i.fh_cvd i.CVD_all i.statin total_chol hdl_c rati i.smoke_cat, m(25) seed(123) by(eduyears sex) saving(imputed_statins_25, replace)
	
use imputed_statins_25, clear

mi import ice, automatic 

save "imputed_statins.dta", replace

capture log close

