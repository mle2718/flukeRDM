

clear
mata: mata clear
clear matrix 
set matsize 10000
set maxvar  120000


*cd "\\net.nefsc.noaa.gov\aharris\2022 fluke survey data\"
global choice_exp_data "Z:\choice experiment data\2022 fluke survey data"

use "${choice_exp_data}/surveydata.dta", clear 
keep qtid a4
duplicates drop 
tempfile boat
save `boat', replace

use "${choice_exp_data}/formattedsurveydata.dta", clear 
merge m:1 qtid using `boat', keep(3) nogen


/* Creating additional variables for running the logit models */
gen constant=0
replace constant=1 if trip=="C"
gen avidity=(a3_party+a3_chart+a3_shore+a3_priva)
gen male=d1
replace male=0 if d1==2
gen birthday=date(registra,"DMY")
gen age=age(birthday, td(25jul2022))
drop birthday
gen likely=1 if inlist(a5, 1,2)
replace likely=0 if inlist(a5, 3, 4)
gen ownboat=1 if inlist(a4, 1)
replace ownboat=0 if inlist(a4, 0)


gen income_low=1 if inlist(d4, 1, 2, 3) /*Creating the dummies for Income. The excluded category if Income_low, which includes income levels of 1,2 and 3*/
gen income_medium=0  /*Creating the dummies for Income. The excluded category if Income_low, which includes income levels of 1,2 and 3*/
replace income_medium=1 if (d4>3 & d4<7)
replace income_medium=. if mi(d4)
gen income_high=0
replace income_high=1 if d4>6
replace income_high=. if mi(d4)
gen education_basic =1 if inlist(d3, 1, 2)
gen education_college=0  /*Creating the dummies for Education. The excluded category if Education_basic, which includes education levels of 1,2 and 3*/
replace education_college=1 if (d3>2 & d3<6)
replace education_college=. if mi(d3)
gen education_graduate=0
replace education_graduate=1 if d3>5
replace education_graduate=. if mi(d3)

replace avidity=constant*avidity
replace age=constant*age
replace income_medium=constant*income_medium
replace income_high=constant*income_high
replace education_college=constant*education_college
replace education_graduate=constant*education_graduate
replace male=constant*male
replace likely=constant*likely
replace ownboat=constant*ownboat

gen SFkept=(keep_fluke_max+keep_fluke_min)/2
replace SFkept=(keep_fluke_0*p_keep_fluke_0+keep_fluke_1*p_keep_fluke_1+keep_fluke_2*p_keep_fluke_2+keep_fluke_3*p_keep_fluke_3+keep_fluke_4*p_keep_fluke_4+keep_fluke_5*p_keep_fluke_5+keep_fluke_6*p_keep_fluke_6+keep_fluke_7*p_keep_fluke_7+keep_fluke_8*p_keep_fluke_8) if keep_fluke_distribution=="Yes"
gen SFrelease=(catch_fluke_max+catch_fluke_min)/2-SFkept

gen BSBkept=keep_bsb
gen BSBrelease=catch_bsb-keep_bsb

gen SCUPkept=keep_scup
gen SCUPrelease=catch_scup-keep_scup

gen KeepOther=(keep_bsb+keep_scup)
gen ReleaseOther=(catch_bsb+catch_scup)-KeepOther

/*The opt-out option*/
replace SFkept=0 if trip=="C"
replace SFrelease=0 if trip=="C"
replace ReleaseOther=0 if trip=="C"
replace KeepOther=0 if trip=="C"
replace BSBkept=0 if trip=="C"
replace BSBrelease=0 if trip=="C"
replace SCUPkept=0 if trip=="C"
replace SCUPrelease=0 if trip=="C"
replace cost=cost
replace cost=0 if trip=="C"
replace catch_scup=0 if trip=="C"

/* square root of catch*/
local vars SFkept SFrelease BSBkept BSBrelease SCUPkept SCUPrelease
foreach v of local vars{
	gen sqrt_`v'=sqrt(`v')
	
}

gen sqrt_scup_catch= sqrt(catch_scup)
gen sqrt_SF_catch= sqrt(SFkept+SFrelease)
gen sqrt_BSB_catch= sqrt(BSBkept+BSBrelease)

replace sqrt_scup_catch=0 if trip=="C"
replace sqrt_SF_catch=0 if trip=="C"
replace sqrt_BSB_catch=0 if trip=="C"

gen sqrt_SF_BSB_keep = sqrt_SFkept*sqrt_BSBkept
gen SF_BSB_keep = SFkept*BSBkept

*Check for protest responses: chose opt-out for every question answered
gen check=1 if chosen==1 & trip=="C"
bysort qtid: egen sumoptout=sum(check)

bysort qtid id: gen nset=1 if _n==1
egen nsets=sum(nset), by(qtid)

gen protest=1 if sumoptout==nsets
drop check sumoptout nset 


*Model estimation
egen rowmiss3=rowmiss(age avidity)
clogit chosen one if rowmiss3==0 & a1_none==0 & protest!=1 ,  group(identifier) 
local l0 = e(ll) 
di `l0'

eststo m3: mixlogit chosen cost age avidity if a1_none==0 & protest!=1, group(identifier) id(qtid) rand(sqrt_SFkept sqrt_SFrelease sqrt_BSBkept sqrt_BSBrelease sqrt_SF_BSB_keep sqrt_scup_catch constant) 
local ll = e(ll) 
di `ll'

di 1-`ll'/`l0'

encode trip, gen(option)
cmset qtid question option
eststo m0_SFSBSB: cmxtmixlogit chosen cost age avidity if a1_none==0 & protest!=1 , ///
					random(sqrt_SFkept sqrt_SFrelease sqrt_BSBkept sqrt_BSBrelease sqrt_SF_BSB_keep sqrt_scup_catch constant) noconstant
					
estimates save "$input_data_cd\m0_SFSBSB.ster", replace 					


estimates use "$input_data_cd\m0_SFSBSB.ster"
local cnames : colnames e(b)
display "`cnames'"


global params
qui forv x=1/$ndraws{
local K=e(k) //-e(krnd)
mat bfull=e(b)
mat b=bfull[1,1..`K']
mat Vfull=e(V)
mat Ve=Vfull[1..`K',1..`K']
mat cholV=cholesky(Ve)

mat iid_err=J(`K',1,0)
        
        forvalues i=1/`K' {
            mat iid_err[`i',1]=rnormal()
        }
    
        * generate draws from vector beta - sampling uncertainty
        mat beta_draw=b' + cholV * iid_err
		mat  list beta_draw
		
		* generate 10,000 draws based on the drawn mean and SD above for the betas specified as random (preference heterogeneity)
		* enter zeroes for the parameters above the 10% level of significance
		
		clear 
		set obs 10000
		
		*original non-linear in catch spec
		gen beta_cost=beta_draw[1,1]
		gen beta_opt_out_age=beta_draw[2,1]
		gen beta_opt_out_avidity=beta_draw[3,1]
		gen beta_sqrt_sf_keep=rnormal(beta_draw[4,1], abs(beta_draw[11,1]))
		gen beta_sqrt_sf_rel=rnormal(beta_draw[5,1], abs(beta_draw[12,1]))
		gen beta_sqrt_bsb_keep=rnormal(beta_draw[6,1], 0)
		gen beta_sqrt_bsb_release=rnormal(beta_draw[7,1], abs(beta_draw[14,1]))
		gen beta_sqrt_sf_bsb_keep=rnormal(beta_draw[8,1], abs(beta_draw[15,1]))
		gen beta_sqrt_scup_catch=rnormal(beta_draw[9,1], 0)
		gen beta_opt_out=rnormal(beta_draw[10,1], abs(beta_draw[17,1]))
		
	
	gen draw=`x'
		
	tempfile params`x'
	save `params`x'', replace
	global params "$params "`params`x''" " 

}	

clear
dsconcat $params

save  "$input_data_cd\preference_params.dta", replace
