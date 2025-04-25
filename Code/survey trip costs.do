
***This code creates trip cost distributions based on the Sabrina's 2017 trip expenditure survey data

*Enter a directory with the expenditure survey data 
u "$input_data_cd\atl_states_2017_expsurvey.dta", clear
renvarlab *, lower


// ensure only relevant states 
keep if inlist(st, 25, 44, 9,  36 , 34, 10, 24, 51, 37)

egen n_missing_cats=rowmiss(afuelexp arentexp ptransexp lodgexp grocexp restexp baitexp iceexp parkexp bfuelexp brentexp guideexp crewexp procexp feesexp giftsexp)
drop if n_missing==16

mvencode afuelexp arentexp ptransexp lodgexp grocexp restexp baitexp iceexp parkexp bfuelexp brentexp guideexp crewexp procexp feesexp giftsexp  othexp, mv(0) override
egen total_exp=rowtotal(afuelexp arentexp ptransexp lodgexp grocexp restexp baitexp iceexp parkexp bfuelexp brentexp guideexp crewexp procexp feesexp giftsexp)
egen total_exp2=rowtotal(afuelexp arentexp ptransexp lodgexp grocexp restexp baitexp iceexp parkexp bfuelexp brentexp guideexp crewexp procexp feesexp giftsexp)

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

*Sabrina's definition of for-hire mode include both headboat and charter boats
*Survey mode definitions:
	*3=shore
	*4=headboat
	*5=charter
	*7=private boat
/*
svy: tabstat total_exp, stat(mean sd) by(state)
svy: mean total_exp if state=="MA"
svy: mean total_exp if state=="RI"
svy: mean total_exp if state=="CT"
svy: mean total_exp if state=="NY"
svy: mean total_exp if state=="NJ"
svy: mean total_exp if state=="DE"
svy: mean total_exp if state=="MD"
svy: mean total_exp if state=="VA"
svy: mean total_exp if state=="NC"
*/
/*
mat b=e(b)'
mat v= e(V)

clear 
svmat b
rename b1 mean
svmat v
rename v1 st_error
replace st_error=sqrt(st_error)
*/

gen mode1="sh" if inlist(mode_fx, "1", "2", "3")
replace mode1="fh" if inlist(mode_fx, "4", "5")
replace mode1="pr" if inlist(mode_fx,  "7")

*Adjust for inflation
replace total_exp = total_exp*$inflation_expansion

*Now generate total expenditures each state/mode combination
tempfile new
save `new', replace

global costs
levelsof mode1, local(modes)
foreach m of local modes{
u `new', clear 

keep if mode1=="`m'"

tempfile new`m'
save `new`m'', replace

levelsof state, local(sts)
	foreach s of local sts{
	
	u `new`m'', clear
	keep if state=="`s'"


	svy: mean total_exp 

	mat b=e(b)'
	mat v= e(V)

	clear 
	svmat b
	rename b1 mean
	svmat v
	rename v1 st_error
	replace st_error=sqrt(st_error)
	gen mode1="`m'"
	gen state="`s'"


	tempfile costs`m'`s'
	save `costs`m'`s'', replace
	global costs "$costs "`costs`m'`s''" " 
}
}
clear
dsconcat $costs

/*
su mean if mode1=="fh"
global fh_cost_est`r(mean)'
su st if mode1=="fh"
global fh_cost_sd `r(mean)'


su mean if mode1=="pr"
global pr_cost_est `r(mean)'
su st if mode1=="pr"
global pr_cost_sd `r(mean)'


su mean if mode1=="sh"
global sh_cost_est `r(mean)'
su st if mode1=="sh"
global sh_cost_sd `r(mean)'
*/


