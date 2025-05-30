
***This code creates trip cost distributions based on the 2017 trip expenditure survey data (data from Sabrina Lovell)

* Pull in expenditure survey data 
u "$input_data_cd\atl_states_2017_expsurvey.dta", clear
renvarlab *, lower

* Keep relevant states 
keep if inlist(st, 25, 44, 9,  36 , 34, 10, 24, 51, 37)

* 16 variable trip cost categories - drop if all 16 categories are missing
egen n_missing_cats=rowmiss(afuelexp arentexp ptransexp lodgexp grocexp restexp baitexp iceexp parkexp bfuelexp brentexp guideexp crewexp procexp feesexp giftsexp)
drop if n_missing==16 

mvencode afuelexp arentexp ptransexp lodgexp grocexp restexp baitexp iceexp parkexp bfuelexp brentexp guideexp crewexp procexp feesexp giftsexp  othexp, mv(0) override

* Compute total trip expenditure
egen total_exp=rowtotal(afuelexp arentexp ptransexp lodgexp grocexp restexp baitexp iceexp parkexp bfuelexp brentexp guideexp crewexp procexp feesexp giftsexp) 

svyset psu_id [pweight= sample_wt], strata(var_id) singleunit(certainty)

gen st2 = string(st,"%02.0f")
gen state="CT" if st2=="09" 
replace state="DE" if st2=="10"
replace state="ME" if st2=="23"
replace state="MD" if st2=="24"
replace state="MA" if st2=="25"
replace state="NJ" if st2=="34"
replace state="NY" if st2=="36"
replace state="NC" if st2=="37"
replace state="RI" if st2=="44"
replace state="VA" if st2=="51"
replace state="NH" if st2=="33"

* Sabrina's definition of for-hire mode include both headboat and charter boats
* Survey mode definitions: 3=shore, 4=headboat,5=charter, =private boat
	
gen mode1="sh" if inlist(mode_fx, "1", "2", "3")
replace mode1="fh" if inlist(mode_fx, "4", "5")
replace mode1="pr" if inlist(mode_fx,  "7")
svy: mean total_exp

/*
svy: mean total_exp if state=="MA" & mode1=="pr"
svy: mean total_exp if state=="RI"
svy: mean total_exp if state=="CT"
svy: mean total_exp if state=="NY"
svy: mean total_exp if state=="NJ"
svy: mean total_exp if state=="DE"
svy: mean total_exp if state=="MD"
svy: mean total_exp if state=="VA"
svy: mean total_exp if state=="NC"
*/

* Adjust for inflation
replace total_exp = total_exp*$inflation_expansion

* Generate total expenditures each state/mode combination
tempfile new
save `new', replace

global costs
levelsof state, local(sts)
foreach s of local sts{
u `new', clear 

keep if state=="`s'"

tempfile new`s'
save `new`s'', replace

levelsof mode1, local(modes)
foreach m of local modes{
	
	u `new`s'', clear
	keep if mode1=="`m'"

	replace wp_int=round(wp_int)
	expand wp_int
	count
	if `r(N)'<10000{
		local expand = round(10000/`r(N)')+1
		expand `expand'
		sample 10000, count

	}
	else{
	sample 10000, count
	}
	
	tempfile costs`m'`s'
	save `costs`m'`s'', replace
	global costs "$costs "`costs`m'`s''" " 
}
}
clear
dsconcat $costs
keep mode1 state total_exp
rename total_exp cost 
save "$iterative_input_data_cd\trip_costs.dta", replace 
