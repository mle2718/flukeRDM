


* A) compare copula-simulated mean catch-per-trip to MRIP 
* Estimates were generated at the state, wave, mode level

*A) 
clear
tempfile master
save `master', emptyok

local statez "MA RI CT NY NJ DE MD VA NC"
*local statez "MA RI CT"

foreach s of local statez{
forv i=1/100{

import excel using "$iterative_input_data_cd\proj_catch_draws_`s'_`i'.xlsx", clear firstrow
gen sf_cat_sim  = sf_keep_sim + sf_rel_sim
gen bsb_cat_sim = bsb_keep_sim + bsb_rel_sim
gen scup_cat_sim = scup_keep_sim + scup_rel_sim
collapse (mean) sf_keep_sim sf_rel_sim sf_cat_sim bsb_keep_sim bsb_rel_sim bsb_cat_sim scup_keep_sim scup_rel_sim scup_cat_sim, by(my_dom_id_string)
gen draw=`i'

append using `master'
save `master', replace
}
}
use `master', clear
save "$input_data_cd\simulated_projected_means_copula.dta", replace 



u "$input_data_cd\simulated_projected_means_copula.dta", clear 
ds draw my_dom_id, not
local vars `r(varlist)'
foreach v of local vars{
	mvencode `v', mv(0) override
}

collapse (mean) sf_keep_sim sf_rel_sim sf_cat_sim bsb_keep_sim bsb_rel_sim bsb_cat_sim scup_keep_sim scup_rel_sim scup_cat_sim 	///
						(sd) sd_sf_keep_sim=sf_keep_sim sd_sf_cat_sim=sf_cat_sim sd_sf_rel_sim=sf_rel_sim ///
						sd_bsb_keep_sim=bsb_keep_sim sd_bsb_rel_sim=bsb_rel_sim sd_bsb_cat_sim=bsb_cat_sim ///
						sd_scup_keep_sim=scup_keep_sim sd_scup_rel_sim=scup_rel_sim sd_scup_cat_sim=scup_cat_sim, by(my)
						
renvarlab sf* bsb* scup*, prefix(tot_)					
split my, parse(_)
rename my_dom_id_string1 state
rename my_dom_id_string2 wave
rename my_dom_id_string3 mode
drop my_dom_id_string4

reshape long tot_ sd_, i(state wave mode) j(new) string
rename tot_ sim_total 
rename sd_ sim_sd
split new, parse(_)
rename new1 species
rename new2 disp
drop new3
drop new
tempfile sim
save `sim', replace

*Pull in the MRIP means/SEs dataset
import excel using "$input_data_cd\projected_mrip_catch_processed.xlsx", clear first 
keep my_dom_id_string-missing_sesf_rel
drop missing*
duplicates drop
split my, parse(_)
rename my_dom_id_string1 state
rename my_dom_id_string2 wave
rename my_dom_id_string3 mode
drop my_dom_id_string4
reshape long mean se, i(state wave mode) j(new) string
rename mean mrip_total 
rename se mrip_sd
split new, parse(_)
rename new1 species
rename new2 disp
drop new

*Join simulated means to MRIP means
merge 1:1 state wave mode my species disp using `sim'
browse if _merge==1
browse

gen mrip_ul=mrip_total+1.96*mrip_sd
gen mrip_ll=mrip_total-1.96*mrip_sd
gen sim_ul=sim_total+1.96*sim_sd
gen sim_ll=sim_total-1.96*sim_sd

drop if mrip_total==0 & sim_total==0
drop if disp=="cat"

gen domain=species+"_"+disp

gen pct_diff = ((sim_total-mrip_total)/mrip_total)*100
gen diff= sim_total-mrip_total
sort pct_diff
sort my_dom

tempfile new
save `new', replace 

levelsof state, local(state_list)

foreach s in `state_list'{
	u `new', clear 
	keep if state=="`s'"

	tempfile new1
	save `new1', replace
	
	levelsof domain, local(domain_list)
		
		foreach d in `domain_list' {
		u `new1', clear
		keep if domain=="`d'"
	
		encode my_dom_id_string, gen(my_dom_id)
		gen my_dom_id_mrip = my_dom_id+0.1 
		gen my_dom_id_sim = my_dom_id-0.1  

* Start by clearing any existing macro
local xlabels ""

* Loop over the levels of the encoded variable
levelsof my_dom_id, local(levels)

foreach l of local levels {
    local label : label (my_dom_id) `l'
    local xlabels `xlabels' `l' "`label'" 
}

qui twoway (rcap mrip_ul mrip_ll my_dom_id_mrip if domain=="`d'", color(blue)  ) ///
			(scatter mrip_total my_dom_id_mrip if domain=="`d'",  msymbol(o) mcolor(blue)) ///
			(rcap sim_ul sim_ll my_dom_id_sim if domain=="`d'",  color(red)) ///
			(scatter sim_total my_dom_id_sim if domain=="`d'", msymbol(o) mcolor(red)), ///
			legend(order(2 "MRIP estimate" 4 "Simulated estimate") size(small) rows(1)) ///
			ytitle("") xtitle("") ylabel(#10,  angle(horizontal) ) ///
			xlabel(`xlabels',  labsize(small) angle(45)) ///
			title("`d'", size(medium)) name(`d'_`s', replace) 
		}
  }
  
u `new', clear 
sort state wave mode 
grc1leg  bsb_keep_MA sf_keep_MA  scup_keep_MA bsb_rel_MA sf_rel_MA   scup_rel_MA , cols(3) title("Mean projected catch-per-trip, MRIP vs. copula estimates MA", size(small))
graph export "$figure_cd/mean_proj_catch_MRIP_copula_MA.png", as(png) replace

grc1leg  bsb_keep_RI sf_keep_RI scup_keep_RI  bsb_rel_RI sf_rel_RI scup_rel_RI    , cols(3)	title("Mean projected catch-per-trip, MRIP vs. copula estimates RI", size(small))	
graph export "$figure_cd/mean_proj_catch_MRIP_copula_RI.png", as(png) replace
		
grc1leg  bsb_keep_CT sf_keep_CT scup_keep_CT  bsb_rel_CT sf_rel_CT  scup_rel_CT  , cols(3) title("Mean projected catch-per-trip, MRIP vs. copula estimates CT", size(small))		
graph export "$figure_cd/mean_proj_catch_MRIP_copula_CT.png", as(png) replace
		
grc1leg  bsb_keep_NY sf_keep_NY scup_keep_NY  bsb_rel_NY sf_rel_NY  scup_rel_NY  , cols(3) title("Mean projected catch-per-trip, MRIP vs. copula estimates NY", size(small))	
graph export "$figure_cd/mean_proj_catch_MRIP_copula_NY.png", as(png) replace

grc1leg  bsb_keep_NJ sf_keep_NJ scup_keep_NJ  bsb_rel_NJ sf_rel_NJ  scup_rel_NJ  , cols(3) title("Mean projected catch-per-trip, MRIP vs. copula estimates NJ", size(small))
graph export "$figure_cd/mean_proj_catch_MRIP_copula_NJ.png", as(png) replace

grc1leg  bsb_keep_DE sf_keep_DE scup_keep_DE  bsb_rel_DE sf_rel_DE  scup_rel_DE  , cols(3) title("Mean projected catch-per-trip, MRIP vs. copula estimates DE", size(small))
graph export "$figure_cd/mean_proj_catch_MRIP_copula_DEA.png", as(png) replace

grc1leg  bsb_keep_MD sf_keep_MD scup_keep_MD  bsb_rel_MD sf_rel_MD  scup_rel_MD  , cols(3) title("Mean projected catch-per-trip, MRIP vs. copula estimates MD", size(small))	
graph export "$figure_cd/mean_proj_catch_MRIP_copula_MD.png", as(png) replace

grc1leg  bsb_keep_VA sf_keep_VA  bsb_rel_VA sf_rel_VA  scup_rel_VA  , cols(3) title("Mean projected catch-per-trip, MRIP vs. copula estimates VA", size(small))	
graph export "$figure_cd/mean_proj_catch_MRIP_copula_VA.png", as(png) replace

grc1leg  bsb_keep_NC  sf_keep_NC scup_keep_NC bsb_rel_NC  scup_rel_NC , cols(3) title("Mean projected catch-per-trip, MRIP vs. copula estimates NC", size(small))
graph export "$figure_cd/mean_proj_catch_MRIP_copula_NC.png", as(png) replace



*B) The copula model data is used to generate daily catch-draw data, so here, I:
		*1) compute mean catch-per-trip from the daily catch-draw data
		*2) compare catch-per-trip means from 1) with estimates from MRIP

*B1 and B2)  
clear
tempfile master
save `master', emptyok

local statez "MA RI CT NY NJ DE MD VA NC"

foreach s of local statez{
forv i=1/100{

*local i=1
*local s="RI"

use "$iterative_input_data_cd\proj_catch_draws_`s'_`i'.dta", clear 

drop if dtrip==0

collapse (mean) sf_keep_sim sf_cat sf_rel_sim bsb_keep_sim bsb_rel_sim bsb_cat scup_keep_sim scup_rel_sim scup_cat, by(my_dom_id_string)
split my_dom_id_string, parse(_)
rename my_dom_id_string1 state
rename my_dom_id_string2 wave
rename my_dom_id_string3 mode
drop my_dom_id_string4

rename sf_cat sf_cat_sim
rename bsb_cat bsb_cat_sim
rename scup_cat scup_cat_sim

tempfile catch
save `catch', replace 

gen draw=`i'

append using `master'
save `master', replace
}
}
use `master', clear

save "$input_data_cd\simulated_projected_catch_means3.dta", replace 


*B3 compare means @ state, mode, wave level
u "$input_data_cd\simulated_projected_catch_means3.dta", clear 

ds draw mode state wave, not
local vars `r(varlist)'
foreach v of local vars{
	mvencode `v', mv(0) override
}

tostring draw, gen(draw2)
gen domain=state+"_"+mode+"_"+draw2
encode domain, gen(domain2)
encode wave, gen(wave2)
xtset domain2 wave2
tsfill, full

decode domain2, gen(domain3)
split domain3, parse(_)
replace state=domain31
replace mode=domain32
replace draw2=domain33

drop draw draw2  domain domain2 domain3 domain31 domain32
destring domain33, replace
rename domain33 draw
drop wave
decode wave2, gen(wave)
drop wave2

order mode state wave draw
ds draw mode state wave, not
local vars `r(varlist)'
foreach v of local vars{
	mvencode `v', mv(0) override
}

collapse (mean) sf_keep_sim sf_rel_sim sf_cat_sim bsb_keep_sim bsb_rel_sim bsb_cat_sim scup_keep_sim scup_rel_sim scup_cat_sim 	///
						(sd) sd_sf_keep_sim=sf_keep_sim  ///
						sd_sf_cat_sim=sf_cat_sim  ///
						sd_sf_rel_sim=sf_rel_sim ///
						sd_bsb_keep_sim=bsb_keep_sim ///
						sd_bsb_rel_sim=bsb_rel_sim ///
						sd_bsb_cat_sim=bsb_cat_sim ///
						sd_scup_keep_sim=scup_keep_sim ///
						sd_scup_rel_sim=scup_rel_sim ///
						sd_scup_cat_sim=scup_cat_sim, by(state mode wave)
						
renvarlab sf* bsb* scup*, prefix(tot_)					

reshape long tot_ sd_, i(state wave mode) j(new) string
rename tot_ sim_total 
rename sd_ sim_sd
split new, parse(_)
rename new1 species
rename new2 disp
drop new3
drop new
tempfile sim
save `sim', replace


import excel using "$input_data_cd\projected_mrip_catch_processed.xlsx", clear first 
keep my_dom_id_string-missing_sesf_rel
drop missing*
duplicates drop
split my, parse(_)
rename my_dom_id_string1 state
rename my_dom_id_string2 wave
rename my_dom_id_string3 mode
drop my_dom_id_string4
reshape long mean se, i(state wave mode) j(new) string
rename mean mrip_total 
rename se mrip_sd
split new, parse(_)
rename new1 species
rename new2 disp
drop new
merge 1:1 state wave mode  species disp using `sim'
browse if _merge==2
browse

gen mrip_ul=mrip_total+1.96*mrip_sd
gen mrip_ll=mrip_total-1.96*mrip_sd
gen sim_ul=sim_total+1.96*sim_sd
gen sim_ll=sim_total-1.96*sim_sd

drop if mrip_total==0 & sim_total==0
keep if disp=="cat"
drop if mrip_total==. & sim_total==0

gen domain=species+"_"+disp

gen pct_diff = ((sim_total-mrip_total)/mrip_total)*100
gen diff= sim_total-mrip_total
sort pct_diff

tempfile new
save `new', replace 

levelsof state, local(state_list)

foreach s in `state_list'{
	u `new', clear 
	keep if state=="`s'"

	tempfile new1
	save `new1', replace
	
	levelsof domain, local(domain_list)
		
		foreach d in `domain_list' {
		u `new1', clear
		keep if domain=="`d'"
	
		encode my_dom_id_string, gen(my_dom_id)
		gen my_dom_id_mrip = my_dom_id+0.1 
		gen my_dom_id_sim = my_dom_id-0.1  

* Start by clearing any existing macro
local xlabels ""

* Loop over the levels of the encoded variable
levelsof my_dom_id, local(levels)

foreach l of local levels {
    local label : label (my_dom_id) `l'
    local xlabels `xlabels' `l' "`label'" 
}

qui twoway (rcap mrip_ul mrip_ll my_dom_id_mrip if domain=="`d'", color(blue)  ) ///
			(scatter mrip_total my_dom_id_mrip if domain=="`d'",  msymbol(o) mcolor(blue)) ///
			(rcap sim_ul sim_ll my_dom_id_sim if domain=="`d'",  color(red)) ///
			(scatter sim_total my_dom_id_sim if domain=="`d'", msymbol(o) mcolor(red)), ///
			legend(order(2 "MRIP estimate" 4 "Simulated estimate") size(small) rows(1)) ///
			ytitle("") xtitle("") ylabel(#10,  angle(horizontal) ) ///
			xlabel(`xlabels',  labsize(small) angle(45)) ///
			title("`d'", size(medium)) name(`d'_`s'_new, replace) 
		}
  }
u `new', clear 
sort state wave mode 

grc1leg  sf_cat_MA_new bsb_cat_MA_new  scup_cat_MA_new  , cols(3) title("Mean catch-per-trip, MRIP vs. simulated estimates MA", size(small))
graph export "$figure_cd/mean_proj_catch_MRIP_simulated_MA.png", as(png) replace

grc1leg  sf_cat_RI_new bsb_cat_RI_new  scup_cat_RI_new , cols(3) title("Mean catch-per-trip, MRIP vs. simulated estimates RI", size(small))
graph export "$figure_cd/mean_proj_catch_MRIP_simulated_RI.png", as(png) replace		

grc1leg  sf_cat_CT_new bsb_cat_CT_new  scup_cat_CT_new , cols(3)	title("Mean catch-per-trip, MRIP vs. simulated estimates CT", size(small))
graph export "$figure_cd/mean_proj_catch_MRIP_simulated_CT.png", as(png) replace

grc1leg  sf_cat_NY_new bsb_cat_NY_new  scup_cat_NY_new   , cols(3) title("Mean catch-per-trip, MRIP vs. simulated estimates NY", size(small))
graph export "$figure_cd/mean_proj_catch_MRIP_simulated_NY.png", as(png) replace	

grc1leg  sf_cat_NJ_new bsb_cat_NJ_new  scup_cat_NJ_new  , cols(3) title("Mean catch-per-trip, MRIP vs. simulated estimates NJ", size(small))
graph export "$figure_cd/mean_proj_catch_MRIP_simulated_NJ.png", as(png) replace

grc1leg  sf_cat_DE_new bsb_cat_DE_new  scup_cat_DE_new  , cols(3) title("Mean catch-per-trip, MRIP vs. simulated estimates DE", size(small))
graph export "$figure_cd/mean_proj_catch_MRIP_simulated_DE.png", as(png) replace

grc1leg  sf_cat_MD_new bsb_cat_MD_new  scup_cat_MD_new  , cols(3) title("Mean catch-per-trip, MRIP vs. simulated estimates MD", size(small))
graph export "$figure_cd/mean_proj_catch_MRIP_simulated_MD.png", as(png) replace	

grc1leg  sf_cat_VA_new bsb_cat_VA_new  scup_cat_VA_new   , cols(3)	 title("Mean catch-per-trip, MRIP vs. simulated estimates VA", size(small))
graph export "$figure_cd/mean_proj_catch_MRIP_simulated_VA.png", as(png) replace

grc1leg sf_cat_NC_new bsb_cat_VA_new  scup_cat_VA_new  , cols(3) title("Mean catch-per-trip, MRIP vs. simulated estimates NC", size(small))
graph export "$figure_cd/mean_proj_catch_MRIP_simulated_NC.png", as(png) replace



** FINAL STEP
* Remove extraneous columns from the catch-per-trip data
clear
mata: mata clear

local statez "MA RI CT NY NJ DE MD VA NC"
foreach s of local statez {
	forvalues i = 1/100{
		use "$iterative_input_data_cd\proj_catch_draws_`s'_`i'.dta", clear 
	    drop my_dom_id_string day month wave day_i sf_keep_sim sf_rel_sim bsb_keep_sim bsb_rel_sim scup_keep_sim scup_rel_sim dtrip
	    compress
		save  "$iterative_input_data_cd\proj_catch_draws_`s'_`i'.dta", replace 
		}
}	

		
	
	