log using imputed_analysis.log, replace

use imputed_statins.dta, clear
lab var qrisk_score QRISK

gen statin_type=1 if statin_cat==1
replace statin_type=0 if statin_cat==5
lab var statin_type "Atorvastatin vs simvastatin"
lab def statin_type 1 "Atorvastatin" 0 "Simvastatin"
lab val statin_type statin_type


********************************************************************************
							* Setting up results file *
********************************************************************************

foreach exp in eduyears qrisk_score  {
	foreach out in qrisk_score statin statin_type{
	putexcel set 202003_imputed, sheet(`exp'-`out') modify
	putexcel A1="Exposure" B1="Outcome" C1="Interaction" D1="Scale" ///
		E1="Beta/OR" F1="LCI" G1="UCI" H1="P Value" I1 = "P value for interaction (with EA)" ///
		J1 = "Sample size" K1="N for statin use"
		
}
}
********************************************************************************
						* Assessing the role of EA *
********************************************************************************

*Linear Models 
	
foreach out in qrisk_score statin statin_type {
	local x=1	
	local x=`x'+1
	
	putexcel set 202003_imputed, sheet(eduyears-`out') modify

	*1 - male and female combined
	mi estimate: regress `out' eduyears
	
	matrix results = r(table)

	local beta = results[1,1]
	local lci = results[5,1]
	local uci = results[6,1]
	local p_value = results[4,1]
	local out_label : var label `out'
	
	putexcel A`x'="Education" B`x'="`out_label'" C`x'="None" D`x'="Linear" E`x'=`beta' F`x'=`lci' G`x'=`uci' H`x'=`p_value'
	
	mi estimate: regress `out' i.eduyears
	
	local x=`x'+1
	matrix results = r(table)
	
	local out_label : var label `out'
		local beta_7 = results[1,1]
	local lci_7 = results[5,1]
	local uci_7 = results[6,1]
	local p_value_7 = results[4,1]
		local beta_10 = results[1,2]
	local lci_10 = results[5,2]
	local uci_10 = results[6,2]
	local p_value_10 = results[4,2]
		local beta_13 = results[1,3]
	local lci_13 = results[5,3]
	local uci_13 = results[6,3]
	local p_value_13 = results[4,3]
		local beta_15 = results[1,4]
	local lci_15 = results[5,4]
	local uci_15 = results[6,4]
	local p_value_15 = results[4,4]
		local beta_19 = results[1,5]
	local lci_19 = results[5,5]
	local uci_19 = results[6,5]
	local p_value_19 = results[4,5]
		local beta_20 = results[1,6]
	local lci_20 = results[5,6]
	local uci_20 = results[6,6]
	local p_value_20 = results[4,6]

	putexcel A`x'="Education - 7 years" B`x'="`out_label'" C`x'="None" D`x'="Linear" E`x'=`beta_7' F`x'=`lci_7' G`x'=`uci_7' H`x'=`p_value_7'
	local x=`x'+1	
	putexcel A`x'="Education - 10 years" B`x'="`out_label'" C`x'="None" D`x'="Linear" E`x'=`beta_10' F`x'=`lci_10' G`x'=`uci_10' H`x'=`p_value_10' 
	local x=`x'+1
	putexcel A`x'="Education - 13 years" B`x'="`out_label'" C`x'="None" D`x'="Linear" E`x'=`beta_13' F`x'=`lci_13' G`x'=`uci_13' H`x'=`p_value_13'
	local x=`x'+1
	putexcel A`x'="Education - 15 years" B`x'="`out_label'" C`x'="None" D`x'="Linear" E`x'=`beta_15' F`x'=`lci_15' G`x'=`uci_15' H`x'=`p_value_15'
	local x=`x'+1
	putexcel A`x'="Education - 19 years" B`x'="`out_label'" C`x'="None" D`x'="Linear" E`x'=`beta_19' F`x'=`lci_19' G`x'=`uci_19' H`x'=`p_value_19'
	local x=`x'+1
	putexcel A`x'="Education - 20 years" B`x'="`out_label'" C`x'="None" D`x'="Linear" E`x'=`beta_20' F`x'=`lci_20' G`x'=`uci_20' H`x'=`p_value_20'
	
	local x=`x'+1
	putexcel A`x'="" B`x'="" C`x'="" D`x'="" E`x'="" F`x'="" G`x'="" H`x'=""
	
	*2 - Female only
	mi estimate: regress `out' eduyears if sex==0
	
	matrix results = r(table)

	local beta = results[1,1]
	local lci = results[5,1]
	local uci = results[6,1]
	local p_value = results[4,1]
	local out_label : var label `out'
	local x=`x'+1
	
	putexcel A`x'="Education" B`x'="`out_label'" C`x'="Female" D`x'="Linear" E`x'=`beta' F`x'=`lci' G`x'=`uci' H`x'=`p_value'
	
	mi estimate: regress `out' i.eduyears if sex==0
	
	local x=`x'+1
	matrix results = r(table)

	local out_label : var label `out'
		local beta_7 = results[1,1]
	local lci_7 = results[5,1]
	local uci_7 = results[6,1]
	local p_value_7 = results[4,1]	
		local beta_10 = results[1,2]
	local lci_10 = results[5,2]
	local uci_10 = results[6,2]
	local p_value_10 = results[4,2]
		local beta_13 = results[1,3]
	local lci_13 = results[5,3]
	local uci_13 = results[6,3]
	local p_value_13 = results[4,3]
		local beta_15 = results[1,4]
	local lci_15 = results[5,4]
	local uci_15 = results[6,4]
	local p_value_15 = results[4,4]
		local beta_19 = results[1,5]
	local lci_19 = results[5,5]
	local uci_19 = results[6,5]
	local p_value_19 = results[4,5]
		local beta_20 = results[1,6]
	local lci_20 = results[5,6]
	local uci_20 = results[6,6]
	local p_value_20 = results[4,6]

	putexcel A`x'="Education - 7 years" B`x'="`out_label'" C`x'="None" D`x'="Linear" E`x'=`beta_7' F`x'=`lci_7' G`x'=`uci_7' H`x'=`p_value_7'
	local x=`x'+1	
	putexcel A`x'="Education - 10 years" B`x'="`out_label'" C`x'="None" D`x'="Linear" E`x'=`beta_10' F`x'=`lci_10' G`x'=`uci_10' H`x'=`p_value_10' 
	local x=`x'+1
	putexcel A`x'="Education - 13 years" B`x'="`out_label'" C`x'="None" D`x'="Linear" E`x'=`beta_13' F`x'=`lci_13' G`x'=`uci_13' H`x'=`p_value_13'
	local x=`x'+1
	putexcel A`x'="Education - 15 years" B`x'="`out_label'" C`x'="None" D`x'="Linear" E`x'=`beta_15' F`x'=`lci_15' G`x'=`uci_15' H`x'=`p_value_15'
	local x=`x'+1
	putexcel A`x'="Education - 19 years" B`x'="`out_label'" C`x'="None" D`x'="Linear" E`x'=`beta_19' F`x'=`lci_19' G`x'=`uci_19' H`x'=`p_value_19'
	local x=`x'+1
	putexcel A`x'="Education - 20 years" B`x'="`out_label'" C`x'="None" D`x'="Linear" E`x'=`beta_20' F`x'=`lci_20' G`x'=`uci_20' H`x'=`p_value_20'
	
	local x=`x'+1
	putexcel A`x'="" B`x'="" C`x'="" D`x'="" E`x'="" F`x'="" G`x'="" H`x'=""
	
	*2 - Male only
	mi estimate: regress `out' eduyears if sex==1
	
	matrix results = r(table)

	local beta = results[1,1]
	local lci = results[5,1]
	local uci = results[6,1]
	local p_value = results[4,1]
	local out_label : var label `out'
	local x=`x'+1
	
	putexcel A`x'="Education" B`x'="`out_label'" C`x'="Male" D`x'="Linear" E`x'=`beta' F`x'=`lci' G`x'=`uci' H`x'=`p_value'
	
		mi estimate: regress `out' i.eduyears if sex==1
	
	local x=`x'+1
	matrix results = r(table)

	local out_label : var label `out'
		local beta_7 = results[1,1]
	local lci_7 = results[5,1]
	local uci_7 = results[6,1]
	local p_value_7 = results[4,1]	
		local beta_10 = results[1,2]
	local lci_10 = results[5,2]
	local uci_10 = results[6,2]
	local p_value_10 = results[4,2]
		local beta_13 = results[1,3]
	local lci_13 = results[5,3]
	local uci_13 = results[6,3]
	local p_value_13 = results[4,3]
		local beta_15 = results[1,4]
	local lci_15 = results[5,4]
	local uci_15 = results[6,4]
	local p_value_15 = results[4,4]
		local beta_19 = results[1,5]
	local lci_19 = results[5,5]
	local uci_19 = results[6,5]
	local p_value_19 = results[4,5]
		local beta_20 = results[1,6]
	local lci_20 = results[5,6]
	local uci_20 = results[6,6]
	local p_value_20 = results[4,6]

	putexcel A`x'="Education - 7 years" B`x'="`out_label'" C`x'="None" D`x'="Linear" E`x'=`beta_7' F`x'=`lci_7' G`x'=`uci_7' H`x'=`p_value_7'
	local x=`x'+1	
	putexcel A`x'="Education - 10 years" B`x'="`out_label'" C`x'="None" D`x'="Linear" E`x'=`beta_10' F`x'=`lci_10' G`x'=`uci_10' H`x'=`p_value_10' 
	local x=`x'+1
	putexcel A`x'="Education - 13 years" B`x'="`out_label'" C`x'="None" D`x'="Linear" E`x'=`beta_13' F`x'=`lci_13' G`x'=`uci_13' H`x'=`p_value_13'
	local x=`x'+1
	putexcel A`x'="Education - 15 years" B`x'="`out_label'" C`x'="None" D`x'="Linear" E`x'=`beta_15' F`x'=`lci_15' G`x'=`uci_15' H`x'=`p_value_15'
	local x=`x'+1
	putexcel A`x'="Education - 19 years" B`x'="`out_label'" C`x'="None" D`x'="Linear" E`x'=`beta_19' F`x'=`lci_19' G`x'=`uci_19' H`x'=`p_value_19'
	local x=`x'+1
	putexcel A`x'="Education - 20 years" B`x'="`out_label'" C`x'="None" D`x'="Linear" E`x'=`beta_20' F`x'=`lci_20' G`x'=`uci_20' H`x'=`p_value_20'
	
	
	local x=`x'+1
	putexcel A`x'="" B`x'="" C`x'="" D`x'="" E`x'="" F`x'="" G`x'="" H`x'=""
	
	}	
	
	
*Binary Models 
	
foreach out in statin statin_type {
	local x=25	
	local x=`x'+1
	
	putexcel set 202003_imputed, sheet(eduyears-`out') modify

	*1 - male and female combined
	mi estimate: logistic `out' eduyears
	
	matrix results = r(table)

	local beta = results[1,1]
	local lci = results[5,1]
	local uci = results[6,1]
	local p_value = results[4,1]
	local out_label : var label `out'
	
	putexcel A`x'="Education" B`x'="`out_label'" C`x'="None" D`x'="Logistic" E`x'=`beta' F`x'=`lci' G`x'=`uci' H`x'=`p_value'
	
	mi estimate: logistic `out' i.eduyears
	
	local x=`x'+1
	matrix results = r(table)

	local out_label : var label `out'
		local beta_7 = results[1,1]
	local lci_7 = results[5,1]
	local uci_7 = results[6,1]
	local p_value_7 = results[4,1]	
		local beta_10 = results[1,2]
	local lci_10 = results[5,2]
	local uci_10 = results[6,2]
	local p_value_10 = results[4,2]
		local beta_13 = results[1,3]
	local lci_13 = results[5,3]
	local uci_13 = results[6,3]
	local p_value_13 = results[4,3]
		local beta_15 = results[1,4]
	local lci_15 = results[5,4]
	local uci_15 = results[6,4]
	local p_value_15 = results[4,4]
		local beta_19 = results[1,5]
	local lci_19 = results[5,5]
	local uci_19 = results[6,5]
	local p_value_19 = results[4,5]
		local beta_20 = results[1,6]
	local lci_20 = results[5,6]
	local uci_20 = results[6,6]
	local p_value_20 = results[4,6]

	putexcel A`x'="Education - 7 years" B`x'="`out_label'" C`x'="None" D`x'="Logistic" E`x'=`beta_7' F`x'=`lci_7' G`x'=`uci_7' H`x'=`p_value_7'
	local x=`x'+1	
	putexcel A`x'="Education - 10 years" B`x'="`out_label'" C`x'="None" D`x'="Logistic" E`x'=`beta_10' F`x'=`lci_10' G`x'=`uci_10' H`x'=`p_value_10' 
	local x=`x'+1
	putexcel A`x'="Education - 13 years" B`x'="`out_label'" C`x'="None" D`x'="Logistic" E`x'=`beta_13' F`x'=`lci_13' G`x'=`uci_13' H`x'=`p_value_13'
	local x=`x'+1
	putexcel A`x'="Education - 15 years" B`x'="`out_label'" C`x'="None" D`x'="Logistic" E`x'=`beta_15' F`x'=`lci_15' G`x'=`uci_15' H`x'=`p_value_15'
	local x=`x'+1
	putexcel A`x'="Education - 19 years" B`x'="`out_label'" C`x'="None" D`x'="Logistic" E`x'=`beta_19' F`x'=`lci_19' G`x'=`uci_19' H`x'=`p_value_19'
	local x=`x'+1
	putexcel A`x'="Education - 20 years" B`x'="`out_label'" C`x'="None" D`x'="Logistic" E`x'=`beta_20' F`x'=`lci_20' G`x'=`uci_20' H`x'=`p_value_20'
	
	local x=`x'+1
	putexcel A`x'="" B`x'="" C`x'="" D`x'="" E`x'="" F`x'="" G`x'="" H`x'=""
	
	*2 - Female only
	mi estimate: logistic `out' eduyears if sex==0
	
	matrix results = r(table)

	local beta = results[1,1]
	local lci = results[5,1]
	local uci = results[6,1]
	local p_value = results[4,1]
	local out_label : var label `out'
	local x=`x'+1
	
	putexcel A`x'="Education" B`x'="`out_label'" C`x'="Female" D`x'="Logistic" E`x'=`beta' F`x'=`lci' G`x'=`uci' H`x'=`p_value'
	
	mi estimate: logistic `out' i.eduyears  if sex==0
	
	local x=`x'+1
	matrix results = r(table)

	local out_label : var label `out'
		local beta_7 = results[1,1]
	local lci_7 = results[5,1]
	local uci_7 = results[6,1]
	local p_value_7 = results[4,1]	
		local beta_10 = results[1,2]
	local lci_10 = results[5,2]
	local uci_10 = results[6,2]
	local p_value_10 = results[4,2]
		local beta_13 = results[1,3]
	local lci_13 = results[5,3]
	local uci_13 = results[6,3]
	local p_value_13 = results[4,3]
		local beta_15 = results[1,4]
	local lci_15 = results[5,4]
	local uci_15 = results[6,4]
	local p_value_15 = results[4,4]
		local beta_19 = results[1,5]
	local lci_19 = results[5,5]
	local uci_19 = results[6,5]
	local p_value_19 = results[4,5]
		local beta_20 = results[1,6]
	local lci_20 = results[5,6]
	local uci_20 = results[6,6]
	local p_value_20 = results[4,6]

	putexcel A`x'="Education - 7 years" B`x'="`out_label'" C`x'="None" D`x'="Logistic" E`x'=`beta_7' F`x'=`lci_7' G`x'=`uci_7' H`x'=`p_value_7'
	local x=`x'+1	
	putexcel A`x'="Education - 10 years" B`x'="`out_label'" C`x'="None" D`x'="Logistic" E`x'=`beta_10' F`x'=`lci_10' G`x'=`uci_10' H`x'=`p_value_10' 
	local x=`x'+1
	putexcel A`x'="Education - 13 years" B`x'="`out_label'" C`x'="None" D`x'="Logistic" E`x'=`beta_13' F`x'=`lci_13' G`x'=`uci_13' H`x'=`p_value_13'
	local x=`x'+1
	putexcel A`x'="Education - 15 years" B`x'="`out_label'" C`x'="None" D`x'="Logistic" E`x'=`beta_15' F`x'=`lci_15' G`x'=`uci_15' H`x'=`p_value_15'
	local x=`x'+1
	putexcel A`x'="Education - 19 years" B`x'="`out_label'" C`x'="None" D`x'="Logistic" E`x'=`beta_19' F`x'=`lci_19' G`x'=`uci_19' H`x'=`p_value_19'
	local x=`x'+1
	putexcel A`x'="Education - 20 years" B`x'="`out_label'" C`x'="None" D`x'="Logistic" E`x'=`beta_20' F`x'=`lci_20' G`x'=`uci_20' H`x'=`p_value_20'
	
	local x=`x'+1
	putexcel A`x'="" B`x'="" C`x'="" D`x'="" E`x'="" F`x'="" G`x'="" H`x'=""
	
	*2 - Male only
	mi estimate: logistic `out' eduyears  if sex==1
	
	matrix results = r(table)

	local beta = results[1,1]
	local lci = results[5,1]
	local uci = results[6,1]
	local p_value = results[4,1]
	local out_label : var label `out'
	local x=`x'+1
	
	putexcel A`x'="Education" B`x'="`out_label'" C`x'="Male" D`x'="Linear" E`x'=`beta' F`x'=`lci' G`x'=`uci' H`x'=`p_value'
	
	mi estimate: logistic `out' i.eduyears if sex==1
	
	local x=`x'+1
	matrix results = r(table)

	local out_label : var label `out'
		local beta_7 = results[1,1]
	local lci_7 = results[5,1]
	local uci_7 = results[6,1]
	local p_value_7 = results[4,1]	
		local beta_10 = results[1,2]
	local lci_10 = results[5,2]
	local uci_10 = results[6,2]
	local p_value_10 = results[4,2]
		local beta_13 = results[1,3]
	local lci_13 = results[5,3]
	local uci_13 = results[6,3]
	local p_value_13 = results[4,3]
		local beta_15 = results[1,4]
	local lci_15 = results[5,4]
	local uci_15 = results[6,4]
	local p_value_15 = results[4,4]
		local beta_19 = results[1,5]
	local lci_19 = results[5,5]
	local uci_19 = results[6,5]
	local p_value_19 = results[4,5]
		local beta_20 = results[1,6]
	local lci_20 = results[5,6]
	local uci_20 = results[6,6]
	local p_value_20 = results[4,6]

	putexcel A`x'="Education - 7 years" B`x'="`out_label'" C`x'="None" D`x'="Logistic" E`x'=`beta_7' F`x'=`lci_7' G`x'=`uci_7' H`x'=`p_value_7'
	local x=`x'+1	
	putexcel A`x'="Education - 10 years" B`x'="`out_label'" C`x'="None" D`x'="Logistic" E`x'=`beta_10' F`x'=`lci_10' G`x'=`uci_10' H`x'=`p_value_10' 
	local x=`x'+1
	putexcel A`x'="Education - 13 years" B`x'="`out_label'" C`x'="None" D`x'="Logistic" E`x'=`beta_13' F`x'=`lci_13' G`x'=`uci_13' H`x'=`p_value_13'
	local x=`x'+1
	putexcel A`x'="Education - 15 years" B`x'="`out_label'" C`x'="None" D`x'="Logistic" E`x'=`beta_15' F`x'=`lci_15' G`x'=`uci_15' H`x'=`p_value_15'
	local x=`x'+1
	putexcel A`x'="Education - 19 years" B`x'="`out_label'" C`x'="None" D`x'="Logistic" E`x'=`beta_19' F`x'=`lci_19' G`x'=`uci_19' H`x'=`p_value_19'
	local x=`x'+1
	putexcel A`x'="Education - 20 years" B`x'="`out_label'" C`x'="None" D`x'="Logistic" E`x'=`beta_20' F`x'=`lci_20' G`x'=`uci_20' H`x'=`p_value_20'
	
	
	local x=`x'+1
	putexcel A`x'="" B`x'="" C`x'="" D`x'="" E`x'="" F`x'="" G`x'="" H`x'=""
	
	}	


********************************************************************************
						* Assessing the role of QRISK *
********************************************************************************

*Binary Models 
********************************************************************************

*Logistic model


lab var qrisk_score "None"
lab var score_male "Male"
lab var score_female "Female"



foreach out in statin statin_type {
	local x=1
	foreach score in qrisk_score score_female score_male  {
		
	
	local x=`x'+1
	
	putexcel set 202003_imputed, sheet(qrisk_score-`out') modify	
		
	mi estimate: logistic `out' `score'
	
	matrix results = r(table)

	local beta = results[1,1]
	local lci = results[5,1]
	local uci = results[6,1]
	local p_value = results[4,1]
	local out_label : var label `out'
	local exp_label : var label `score'
	
	putexcel A`x'="QRISK" B`x'="`out_label'" C`x'="`exp_label'" D`x'="Logistic" E`x'=`beta' F`x'=`lci' G`x'=`uci' H`x'=`p_value'	
	
	}
}

lab var qrisk_score ""


levelsof eduyears, local(levels)

foreach out in statin statin_type {
local x = 5	
foreach score in qrisk_score score_female score_male {	
local x=`x'+1
foreach l of local levels {


	putexcel set 202003_imputed, sheet(qrisk_score-`out') modify
	
	mi estimate: logistic `out' `score'  if eduyears==`l' 
	
	matrix results = r(table)
	
	local beta = results[1,1]
	local lci = results[5,1]
	local uci = results[6,1]
	local p_value = results[4,1]
	local exp_label : var label `score'
	local out_label : var label `out'
	
	putexcel A`x'="QRISK" B`x'="`out_label'" C`x'="Education `l' - `exp_label'" D`x'="Log odds" E`x'=`beta' F`x'=`lci' G`x'=`uci' H`x'=`p_value'

	local x=`x'+1
	putexcel A`x'="" B`x'="" C`x'="" D`x'="" E`x'="" F`x'="" G`x'="" H`x'=""
	
	
		}
		}
		}


capture log close

