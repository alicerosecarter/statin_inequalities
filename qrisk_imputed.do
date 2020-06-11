log using qrisk_derivation.log, replace
								** FEMALE **

use imputed_statins.dta, clear

keep if sex==0

rename sd_sbp sbps5
/* Survivor function generate at the end of the script
double survivor[16] = {
		0,
		0,
		0,
		0,
		0,
		0,
		0,
		0,
		0,
		0,
		0.988876402378082,
		0,
		0,
		0,
		0,
		0
	};
	
*/
	
/* Ethnicity - assigining values to each ethnic group. No clear documentation
	on which groups these apply to, so have coded in the order they are listed
	in the QRISK3 manuscript main text. */

mi passive: gen double Iethrisk = 0 if ethnicity == 1 
mi passive: replace Iethrisk = 0.2804031433299542500000000 if ethnicity == 2
mi passive: replace Iethrisk = 0.5629899414207539800000000 if ethnicity == 3
mi passive: replace Iethrisk = 0.2959000085111651600000000 if ethnicity == 4
mi passive: replace Iethrisk = 0.0727853798779825450000000 if ethnicity == 5
mi passive: replace Iethrisk = -0.1707213550885731700000000 if ethnicity == 6
mi passive: replace Iethrisk = -0.3937104331487497100000000 if ethnicity == 7
mi passive: replace Iethrisk = -0.3263249528353027200000000 if ethnicity == 8
mi passive: replace Iethrisk = -0.1712705688324178400000000 if ethnicity == 9

/* Smoking - assigning values to each smoking category. Again, assigned according
to the order listed in the QRISK3 manuscript main text as no clear documentation for the groups */

mi passive: gen double Ismoke = 0 if smoke_cat==0
mi passive: replace Ismoke = 0.1338683378654626200000000 if smoke_cat==1 
mi passive: replace Ismoke = 0.5620085801243853700000000 if smoke_cat==2 
mi passive: replace Ismoke = 0.6674959337750254700000000 if smoke_cat==3 
mi passive: replace Ismoke = 0.8494817764483084700000000 if smoke_cat==4 


/* Applying the fractional polynomial transforms */
	/* (which includes scaling)                      */

mi passive: gen double dage = age/10
mi passive: gen double age_1 = (dage^-2)
mi passive: gen double age_2 = dage
	
mi passive: gen double dbmi = bmi/10
mi passive: gen double bmi_1 = (dbmi^-2)
mi passive: gen double bmi_2 = (dbmi^-2)*log(dbmi)

/* Centring the continuous variables */

mi passive: replace age_1 = age_1 - 0.053274843841791
mi passive: replace age_2 = age_2 - 4.332503318786621
mi passive: replace bmi_1 = bmi_1 - 0.154946178197861
mi passive: replace bmi_2 = bmi_2 - 0.144462317228317
mi xeq: replace rati = rati - 3.476326465606690
mi xeq: replace sbp = sbp - 123.130012512207030
mi xeq: replace sbps5 = sbps5 - 9.002537727355957
mi xeq: replace town = town - 0.392308831214905

/* Start of Sum */
mi passive: gen double a=0

/* The conditional sums */

/* for each *replace* the value of a is being added to by the variable or calculation
	on the right hand side */

mi passive: replace a = a + Iethrisk
mi passive: replace a = a + Ismoke


/* Sum from continuous values */
mi passive: replace a = a + age_1 * -8.1388109247726188000000000
mi passive: replace a = a + age_2 * 0.7973337668969909800000000
mi passive: replace a = a + bmi_1 * 0.2923609227546005200000000
mi passive: replace a = a + bmi_2 * -4.1513300213837665000000000
mi passive: replace a = a + rati * 0.1533803582080255400000000
mi passive: replace a = a + sbp * 0.0131314884071034240000000
mi passive: replace a = a + sbps5 * 0.0078894541014586095000000
mi passive: replace a = a + town * 0.0772237905885901080000000

/* Sum from boolean values */

mi passive: replace a = a + b_AF * 1.5923354969269663000000000
mi passive: replace a = a + b_atypicalantipsy * 0.2523764207011555700000000
mi passive: replace a = a + b_corticosteroids * 0.5952072530460185100000000
mi passive: replace a = a + b_migraine * 0.3012672608703450000000000
mi passive: replace a = a + b_ra * 0.2136480343518194200000000
mi passive: replace a = a + b_renal * 0.6519456949384583300000000
mi passive: replace a = a + b_semi * 0.1255530805882017800000000
mi passive: replace a = a + b_sle * 0.7588093865426769300000000
mi passive: replace a = a + b_treatedhyp * 0.5093159368342300400000000
mi passive: replace a = a + b_type1 * 1.7267977510537347000000000
mi passive: replace a = a + b_type2 * 1.0688773244615468000000000
mi passive: replace a = a + fh_cvd * 0.4544531902089621300000000

/* Sum from interaction terms */

/* Specifically for smoking - assigning values based on four categories of smoking - 
no value assinged for non-smokers as would be zero, hence only four not five categories */
mi passive: replace a = a + age_1 * -4.7057161785851891000000000 if smoke_cat==1
mi passive: replace a = a + age_1 * -2.7430383403573337000000000 if smoke_cat==2
mi passive: replace a = a + age_1 * -0.8660808882939218200000000 if smoke_cat==3
mi passive: replace a = a + age_1 * 0.9024156236971064800000000	if smoke_cat==4

mi passive: replace a = a + age_1 * b_AF * 19.9380348895465610000000000
mi passive: replace a = a + age_1 * b_corticosteroids * -0.9840804523593628100000000
mi passive: replace a = a + age_1 * b_migraine * 1.7634979587872999000000000
mi passive: replace a = a + age_1 * b_renal * -3.5874047731694114000000000
mi passive: replace a = a + age_1 * b_sle * 19.6903037386382920000000000
mi passive: replace a = a + age_1* b_treatedhyp * 11.8728097339218120000000000
mi passive: replace a = a + age_1 * b_type1 * -1.2444332714320747000000000
mi passive: replace a = a + age_1 * b_type2 * 6.8652342000009599000000000
mi passive: replace a = a + age_1 * bmi_1 * 23.8026234121417420000000000
mi passive: replace a = a + age_1 * bmi_2 * -71.1849476920870070000000000
mi passive: replace a = a + age_1 * fh_cvd * 0.9946780794043512700000000
mi passive: replace a = a + age_1 * sbp * 0.0341318423386154850000000
mi passive: replace a = a + age_1 * town * -1.0301180802035639000000000

mi passive: replace a = a + age_2 * -0.0755892446431930260000000 if smoke_cat==1
mi passive: replace a = a + age_2 * -0.1195119287486707400000000 if smoke_cat==2
mi passive: replace a = a + age_2 * -0.1036630639757192300000000 if smoke_cat==3
mi passive: replace a = a + age_2 * -0.1399185359171838900000000 if smoke_cat==4
mi passive: replace a = a + age_2 * b_AF * -0.0761826510111625050000000
mi passive: replace a = a + age_2 * b_corticosteroids * -0.1200536494674247200000000
mi passive: replace a = a + age_2 * b_migraine * -0.0655869178986998590000000
mi passive: replace a = a + age_2 * b_renal * -0.2268887308644250700000000
mi passive: replace a = a + age_2 * b_sle * 0.0773479496790162730000000
mi passive: replace a = a + age_2* b_treatedhyp * 0.0009685782358817443600000
mi passive: replace a = a + age_2 * b_type1 * -0.2872406462448894900000000
mi passive: replace a = a + age_2 * b_type2 * -0.0971122525906954890000000
mi passive: replace a = a + age_2 * bmi_1 * 0.5236995893366442900000000
mi passive: replace a = a + age_2 * bmi_2 * 0.0457441901223237590000000
mi passive: replace a = a + age_2 * fh_cvd * -0.0768850516984230380000000
mi passive: replace a = a + age_2 * sbp * -0.0015082501423272358000000
mi passive: replace a = a + age_2 * town * -0.0315934146749623290000000

** FOR PURPOSES OF RUNNING THIS ASSIGNED ALL TO A SURVIVOR SCORE OF 0.98 **

/* As desribed above, here is where we generate a value for the survivor, which is applied
to every person */
mi passive: gen double survivor = 0.988876402378082

/* Calculating the actual score */
mi passive: gen double score_female = (100.0 * (1-(survivor^ exp(a))))

sum score_female
rename a a_female

save imputed_female_qrisk, replace

********************************************************************************
								** MALE **
use imputed_statins

keep if sex==1

rename sd_sbp sbps5

/* 
double survivor[16] = {
		0,
		0,
		0,
		0,
		0,
		0,
		0,
		0,
		0,
		0,
		0.977268040180206,
		0,
		0,
		0,
		0,
		0
	};
	
	*/
	
/* Ethnicity */

mi passive: gen double Iethrisk = 0 if ethnicity == 1 
mi passive: replace Iethrisk = 0.2771924876030827900000000 if ethnicity == 2
mi passive: replace Iethrisk = 0.4744636071493126800000000 if ethnicity == 3
mi passive: replace Iethrisk = 0.5296172991968937100000000 if ethnicity == 4
mi passive: replace Iethrisk = 0.0351001591862990170000000 if ethnicity == 5
mi passive: replace Iethrisk = -0.3580789966932791900000000 if ethnicity == 6
mi passive: replace Iethrisk = -0.4005648523216514000000000 if ethnicity == 7
mi passive: replace Iethrisk = -0.4152279288983017300000000 if ethnicity == 8
mi passive: replace Iethrisk = -0.2632134813474996700000000 if ethnicity == 9

/* Smoking */

mi passive: gen double Ismoke = 0 if smoke_cat==0
mi passive: replace Ismoke = 0.1912822286338898300000000 if smoke_cat==1 
mi passive: replace Ismoke = 0.5524158819264555200000000 if smoke_cat==2 
mi passive: replace Ismoke = 0.6383505302750607200000000 if smoke_cat==3 
mi passive: replace Ismoke = 0.7898381988185801900000000 if smoke_cat==4 

/* Applying the fractional polynomial transforms */
	/* (which includes scaling)                      */

mi passive: gen double dage = age/10
mi passive: gen double age_1 = (dage^-1)
mi passive: gen double age_2 = (dage^3)
	
mi passive: gen double dbmi = bmi/10
mi passive: gen double bmi_1 = (dbmi^-2)
mi passive: gen double bmi_2 = (dbmi^-2)*log(dbmi)

/* Centring the continuous variables */

mi passive: replace age_1 = age_1 - 0.234766781330109
mi passive: replace age_2 = age_2 - 77.284080505371094
mi passive: replace bmi_1 = bmi_1 - 0.149176135659218
mi passive: replace bmi_2 = bmi_2 - 0.141913309693336
mi xeq: replace rati = rati - 4.300998687744141
mi xeq: replace sbp = sbp - 128.571578979492190
mi xeq: replace sbps5 = sbps5 - 8.756621360778809
mi xeq: replace town = town - 0.526304900646210

/* Start of Sum */
mi passive: gen double a=0

/* The conditional sums */

mi passive: replace a = a + Iethrisk
mi passive: replace a = a + Ismoke


/* Sum from continuous values */
mi passive: replace a = a + age_1 	* -17.8397816660055750000000000
mi passive: replace a = a + age_2 	* 0.0022964880605765492000000
mi passive: replace a = a + bmi_1 	* 2.4562776660536358000000000
mi passive: replace a = a + bmi_2 	* -8.3011122314711354000000000
mi passive: replace a = a + rati 	* 0.1734019685632711100000000
mi passive: replace a = a + sbp 	* 0.0129101265425533050000000
mi passive: replace a = a + sbps5 	* 0.0102519142912904560000000
mi passive: replace a = a + town 	* 0.0332682012772872950000000

/* Sum from boolean values */

mi passive: replace a = a + b_AF 				* 0.8820923692805465700000000
mi passive: replace a = a + b_atypicalantipsy 	* 0.1304687985517351300000000
mi passive: replace a = a + b_corticosteroids 	* 0.4548539975044554300000000
mi passive: replace a = a + b_impotence2		* 0.2225185908670538300000000 
mi passive: replace a = a + b_migraine 			* 0.2558417807415991300000000
mi passive: replace a = a + b_ra 				* 0.2097065801395656700000000
mi passive: replace a = a + b_renal 			* 0.7185326128827438400000000
mi passive: replace a = a + b_semi 				* 0.1213303988204716400000000
mi passive: replace a = a + b_sle 		 		* 0.4401572174457522000000000
mi passive: replace a = a + b_treatedhyp		* 0.5165987108269547400000000
mi passive: replace a = a + b_type1 			* 1.2343425521675175000000000
mi passive: replace a = a + b_type2 			* 0.8594207143093222100000000
mi passive: replace a = a + fh_cvd 	        * 0.5405546900939015600000000

/* Sum from interaction terms */


mi passive: replace a = a + age_1 * -0.2101113393351634600000000 if smoke_cat==1
mi passive: replace a = a + age_1 * 0.7526867644750319100000000 if smoke_cat==2
mi passive: replace a = a + age_1 * 0.9931588755640579100000000 if smoke_cat==3
mi passive: replace a = a + age_1 * 2.1331163414389076000000000 if smoke_cat==4
mi passive: replace a = a + age_1 * b_AF 				* 3.4896675530623207000000000
mi passive: replace a = a + age_1 * b_corticosteroids 	* 1.1708133653489108000000000
mi passive: replace a = a + age_1 * b_impotence2			* -1.5064009857454310000000000 
mi passive: replace a = a + age_1 * b_migraine 			* 2.3491159871402441000000000
mi passive: replace a = a + age_1 * b_renal 			* -0.5065671632722369400000000
mi passive: replace a = a + age_1* b_treatedhyp 		* 6.5114581098532671000000000
mi passive: replace a = a + age_1 * b_type1 			* 5.3379864878006531000000000
mi passive: replace a = a + age_1 * b_type2 			* 3.6461817406221311000000000
mi passive: replace a = a + age_1 * bmi_1 				* 31.0049529560338860000000000
mi passive: replace a = a + age_1 * bmi_2 				* -111.291571843916430000000000
mi passive: replace a = a + age_1 * fh_cvd 			* 2.7808628508531887000000000
mi passive: replace a = a + age_1 * sbp 				* 0.0188585244698658530000000
mi passive: replace a = a + age_1 * town 				* -0.1007554870063731000000000
	

mi passive: replace a = a + age_2 * -0.0004985487027532612100000 if smoke_cat==1
mi passive: replace a = a + age_2 * -0.0007987563331738541400000 if smoke_cat==2
mi passive: replace a = a + age_2 * -0.0008370618426625129600000 if smoke_cat==3
mi passive: replace a = a + age_2 * -0.0007840031915563728900000 if smoke_cat==4
mi passive: replace a = a + age_2 * b_AF 				* -0.0003499560834063604900000
mi passive: replace a = a + age_2 * b_corticosteroids 	* -0.0002496045095297166000000
mi passive: replace a = a + age_2 * b_impotence2	 		* -0.0011058218441227373000000
mi passive: replace a = a + age_2 * b_migraine			* 0.0001989644604147863100000 
mi passive: replace a = a + age_2 * b_renal 			* -0.0018325930166498813000000
mi passive: replace a = a + age_2* b_treatedhyp 		* 0.0006383805310416501300000
mi passive: replace a = a + age_2 * b_type1 			* 0.0006409780808752897000000
mi passive: replace a = a + age_2 * b_type2 			* -0.0002469569558886831500000
mi passive: replace a = a + age_2 * bmi_1 				* 0.0050380102356322029000000
mi passive: replace a = a + age_2 * bmi_2 				* -0.0130744830025243190000000
mi passive: replace a = a + age_2 * fh_cvd 			* -0.0002479180990739603700000
mi passive: replace a = a + age_2 * sbp 				* -0.0000127187419158845700000
mi passive: replace a = a + age_2 * town 				* -0.0000932996423232728880000
		

** FOR PURPOSES OF RUNNING THIS ASSIGNED ALL TO A SURVIVOR SCORE OF 0.98 **

mi passive: gen double survivor_male = 0.977268040180206

mi passive: gen double score_male = (100.0 * (1-(survivor_male^ exp(a))))

sum score_male
rename a a_male

save imputed_male_qrisk, replace

use imputed_statins.dta, clear

mi merge 1:1 n_eid using imputed_female_qrisk
capture drop _m
mi merge 1:1 n_eid using imputed_male_qrisk
capture drop _m

mi xeq: gen qrisk_score = score_female
mi xeq: replace qrisk_score = score_male if qrisk_score==.

drop if CVD_to_exclude==1
drop if eduyears==.
mi update

save imputed_statins, replace

capture log close
