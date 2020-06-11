

*use "statin_phenotypic_master.dta", clear


lab var qrisk_score QRISK

gen statin_type=1 if statin_cat==1
replace statin_type=0 if statin_cat==5
lab var statin_type "Atorvastatin vs simvastatin"
lab def statin_type 1 "Atorvastatin" 0 "Simvastatin"
lab val statin_type statin_type

mark touse
markout touse eduyears qrisk_score statin
keep if touse==1

egen sd_qrisk = std(qrisk_score)
egen sd_qrisk_female = std(score_female)
egen sd_qrisk_male = std(score_male)
egen sd_fram = std(risk10)

gen ln_qrisk = ln(qrisk_score)
gen ln_qrisk_female = ln(score_female)
gen ln_qrisk_male = ln(score_male)

drop if CVD_to_exclude==1 

********************************************************************************
							* Setting up results file *
********************************************************************************

foreach exp in eduyears qrisk_score  {
	foreach out in qrisk_score statin statin_type{
	putexcel set 202003_complete_case, sheet(`exp'-`out') modify
	putexcel A1="Exposure" B1="Outcome" C1="Interaction" D1="Scale" ///
		E1="Beta/OR" F1="LCI" G1="UCI" H1="P Value" I1 = "P value for interaction (with EA)" ///
		J1 = "Sample size" K1="N for statin use"
		
}
}

********************************************************************************
						* Assessing the role of EA *
********************************************************************************
{
*Linear Models 
	
foreach out in qrisk_score statin statin_type {
	local x=1	
	local x=`x'+1
	
	putexcel set 202003_complete_case, sheet(eduyears-`out') modify

	*1 - male and female combined
	regress `out' eduyears
	
	matrix results = r(table)

	local beta = results[1,1]
	local lci = results[5,1]
	local uci = results[6,1]
	local p_value = results[4,1]
	local out_label : var label `out'
	
	putexcel A`x'="Education" B`x'="`out_label'" C`x'="None" D`x'="Linear" E`x'=`beta' F`x'=`lci' G`x'=`uci' H`x'=`p_value'
	
	regress `out' i.eduyears
	
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
	regress `out' eduyears if sex==0
	
	matrix results = r(table)

	local beta = results[1,1]
	local lci = results[5,1]
	local uci = results[6,1]
	local p_value = results[4,1]
	local out_label : var label `out'
	local x=`x'+1
	
	putexcel A`x'="Education" B`x'="`out_label'" C`x'="Female" D`x'="Linear" E`x'=`beta' F`x'=`lci' G`x'=`uci' H`x'=`p_value'
	
	regress `out' i.eduyears if sex==0
	
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
	regress `out' eduyears if sex==1
	
	matrix results = r(table)

	local beta = results[1,1]
	local lci = results[5,1]
	local uci = results[6,1]
	local p_value = results[4,1]
	local out_label : var label `out'
	local x=`x'+1
	
	putexcel A`x'="Education" B`x'="`out_label'" C`x'="Male" D`x'="Linear" E`x'=`beta' F`x'=`lci' G`x'=`uci' H`x'=`p_value'
	
		regress `out' i.eduyears if sex==1
	
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
	
	putexcel set 202003_complete_case, sheet(eduyears-`out') modify

	*1 - male and female combined
	logistic `out' eduyears
	
	matrix results = r(table)

	local beta = results[1,1]
	local lci = results[5,1]
	local uci = results[6,1]
	local p_value = results[4,1]
	local out_label : var label `out'
	
	putexcel A`x'="Education" B`x'="`out_label'" C`x'="None" D`x'="Logistic" E`x'=`beta' F`x'=`lci' G`x'=`uci' H`x'=`p_value'
	
	logistic `out' i.eduyears
	
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
	logistic `out' eduyears if sex==0
	
	matrix results = r(table)

	local beta = results[1,1]
	local lci = results[5,1]
	local uci = results[6,1]
	local p_value = results[4,1]
	local out_label : var label `out'
	local x=`x'+1
	
	putexcel A`x'="Education" B`x'="`out_label'" C`x'="Female" D`x'="Logistic" E`x'=`beta' F`x'=`lci' G`x'=`uci' H`x'=`p_value'
	
	logistic `out' i.eduyears  if sex==0
	
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
	logistic `out' eduyears  if sex==1
	
	matrix results = r(table)

	local beta = results[1,1]
	local lci = results[5,1]
	local uci = results[6,1]
	local p_value = results[4,1]
	local out_label : var label `out'
	local x=`x'+1
	
	putexcel A`x'="Education" B`x'="`out_label'" C`x'="Male" D`x'="Linear" E`x'=`beta' F`x'=`lci' G`x'=`uci' H`x'=`p_value'
	
		logistic `out' i.eduyears if sex==1
	
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
	foreach score in qrisk_score score_female score_male {
		
	
	local x=`x'+1
	
	putexcel set 202003_complete_case, sheet(qrisk_score-`out') modify	
		
		logistic `out' `score'
	
	matrix results = r(table)

	local beta = results[1,1]
	local lci = results[5,1]
	local uci = results[6,1]
	local p_value = results[4,1]
	local out_label : var label `out'
	local exp_label : var label `score'
	
	putexcel A`x'="QRISK" B`x'="`out_label'" C`x'="`exp_label'" D`x'="Logistic" E`x'=`beta' F`x'=`lci' G`x'=`uci' H`x'=`p_value'
	
	logistic `out' c.`score' i.eduyears 
	est store A
	logistic `out' c.`score'##i.eduyears
	est store B
	lrtest A B
	
	scalar p_value = r(p)
	putexcel I`x'=p_value
	
	
	}
}

lab var qrisk_score ""


levelsof eduyears, local(levels)

foreach out in statin statin_type {
local x = 5	
foreach score in qrisk_score score_female score_male {	
local x=`x'+1
foreach l of local levels {



	
	putexcel set 202003_complete_case, sheet(qrisk_score-`out') modify
	
	logistic `out' `score' if eduyears==`l' 
	
	matrix results = r(table)
	
	local beta = results[1,1]
	local lci = results[5,1]
	local uci = results[6,1]
	local p_value = results[4,1]
	local exp_label : var label `score'
	local out_label : var label `out'
	
	putexcel A`x'="QRISK" B`x'="`out_label'" C`x'="Education `l' - `exp_label'" D`x'="Logistic" E`x'=`beta' F`x'=`lci' G`x'=`uci' H`x'=`p_value'

	local x=`x'+1
	putexcel A`x'="" B`x'="" C`x'="" D`x'="" E`x'="" F`x'="" G`x'="" H`x'=""

	
	
		}
		}
		}

		
lab var qrisk_score "QRISK"

*
*Log odds model

lab var qrisk_score "None"
lab var score_male "Male"
lab var score_female "Female"

foreach out in statin statin_type {
	local x=26
	foreach score in qrisk_score score_female score_male {
		
	
	local x=`x'+1
	
	putexcel set 202003_complete_case, sheet(qrisk_score-`out') modify	
		
		logit `out' `score' 
	
	matrix results = r(table)

	local beta = results[1,1]
	local lci = results[5,1]
	local uci = results[6,1]
	local p_value = results[4,1]
	local out_label : var label `out'
	local exp_label : var label `score'
	
	putexcel A`x'="QRISK" B`x'="`out_label'" C`x'="`exp_label'" D`x'="Log odds" E`x'=`beta' F`x'=`lci' G`x'=`uci' H`x'=`p_value'
	
	logit `out' c.`score' i.eduyears
	est store A
	logit `out' c.`score'##i.eduyears
	est store B
	lrtest A B
	
	scalar p_value = r(p)
	putexcel I`x'=p_value
	
	}
}

lab var qrisk_score ""

levelsof eduyears, local(levels)
foreach out in statin statin_type {
local x = 30
foreach score in qrisk_score score_female score_male {	
local x=`x'+1
foreach l of local levels {

	
	putexcel set 202003_complete_case, sheet(qrisk_score-`out') modify
	
	logit `out' `score'  if eduyears==`l' 
	
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


*Linear models

foreach out in statin statin_type {
	local x=51
	foreach score in qrisk_score score_female score_male {
		
	
	local x=`x'+1
	
	putexcel set 202003_complete_case, sheet(qrisk_score-`out') modify	
		
		regress `out' `score'
	
	matrix results = r(table)

	local beta = results[1,1]
	local lci = results[5,1]
	local uci = results[6,1]
	local p_value = results[4,1]
	local out_label : var label `out'
	local exp_label : var label `score'
	
	putexcel A`x'="QRISK" B`x'="`out_label'" C`x'="`exp_label'" D`x'="Linear" E`x'=`beta' F`x'=`lci' G`x'=`uci' H`x'=`p_value'
	
	regress `out' c.`score' i.eduyears 
	est store A
	regress `out' c.`score'##i.eduyears
	est store B
	lrtest A B
	
	scalar p_value = r(p)
	putexcel I`x'=p_value
	
	}
}

lab var qrisk_score ""

levelsof eduyears, local(levels)

foreach out in statin statin_type {
local x = 55	
foreach score in qrisk_score score_female score_male {	
local x=`x'+1
foreach l of local levels {

	
	putexcel set 202003_complete_case, sheet(qrisk_score-`out') modify
	
	regress `out' `score'  if eduyears==`l' 
	
	matrix results = r(table)
	
	local beta = results[1,1]
	local lci = results[5,1]
	local uci = results[6,1]
	local p_value = results[4,1]
	local exp_label : var label `score'
	local out_label : var label `out'
	
	putexcel A`x'="QRISK" B`x'="`out_label'" C`x'="Education `l' - `exp_label'" D`x'="Linear" E`x'=`beta' F`x'=`lci' G`x'=`uci' H`x'=`p_value'

	*mhodds `out' qrisk_score , by(eduyears)
	
	
	
		}
		}
		}

		
lab var qrisk_score "QRISK"



********************************************************************************
gen eduyears_female = eduyears if sex==0
gen eduyears_male = eduyears if sex==1

foreach outcome in statin statin_type {
		foreach exposure in eduyears eduyears_female eduyears_male {
		
		 tab `outcome' `exposure', matcell(numbers)
		 
		local case_7 = numbers[1,2]
		local case_10 = numbers[2,2]
		local case_13 = numbers[3,2]
		local case_15 = numbers[4,2]
		local case_19 = numbers[5,2]
		local case_20 = numbers[6,2]
		
		local n_7 = sum(numbers[1,1]+numbers[1,2])
		local n_10 = sum(numbers[2,1]+numbers[2,2])
		local n_13 = sum(numbers[3,1]+numbers[3,2])
		local n_15 = sum(numbers[4,1]+numbers[4,2])
		local n_19 = sum(numbers[5,1]+numbers[5,2])
		local n_20 = sum(numbers[6,1]+numbers[6,2])

		*putexcel 
		 
		 }
}		 

	tab eduyears `out', matcell(numbers)
	
	

	putexcel C2=`n_7' c3=`n_10' c4=`n_13' c5=`n_15' c6=`n_19' c7=`n_20'
	putexcel D2=`case_7' D3=`case_10' D4=`case_13' D5=`case_15' D6=`case_19' D7=`case_20'
			
