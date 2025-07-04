

global tripz
local statez "MA RI CT NY NJ DE MD VA NC"
foreach s of local statez{
	
forv i=1/150{
	
*local i=1
*local s="MA"

import delimited using "$iterative_input_data_cd\calib_catch_draws_`s'_`i'.csv", clear 

collapse (mean) sf_keep_sim sf_cat sf_rel_sim bsb_keep_sim bsb_rel_sim bsb_cat scup_keep_sim scup_rel_sim scup_cat, by(date day_i mode state)


preserve 
import delimited using  "$input_data_cd\directed_trips_calibration_`s'", clear 
keep if draw==`i'
drop if dtrip==0
keep mode date  dtrip
tempfile dtrip
save `dtrip', replace
restore

merge 1:1 mode date  using `dtrip', nogen

mvencode sf_keep_sim sf_cat sf_rel_sim bsb_keep_sim bsb_rel_sim bsb_cat scup_keep_sim scup_rel_sim scup_cat dtrip, mv(0) override

rename sf_cat sf_cat_sim
rename bsb_cat bsb_cat_sim
rename scup_cat scup_cat_sim


local vars sf_keep_sim sf_cat_sim sf_rel_sim bsb_keep_sim bsb_rel_sim bsb_cat_sim scup_keep_sim scup_rel_sim scup_cat_sim 
foreach v of local vars{
	gen tot_`v'= dtrip*`v'
	
}

collapse (sum) tot* dtrip, by(mode)
gen draw=`i'
gen state="`s'"
rename dtrip tot_dtrip_sim

tempfile tripz`i'`s'
save `tripz`i'`s'', replace
global tripz "$tripz "`tripz`i'`s''" " 
}
}
dsconcat $tripz
order state mode draw
save "$iterative_input_data_cd\simulated_catch_totals.dta", replace 









*By state and mode
u "$iterative_input_data_cd\simulated_catch_totals.dta", replace 

collapse (mean) tot_sf_keep_sim tot_sf_cat_sim tot_sf_rel_sim ///
						  tot_bsb_keep_sim tot_bsb_rel_sim tot_bsb_cat_sim ///
						  tot_scup_keep_sim tot_scup_rel_sim tot_scup_cat_sim tot_dtrip_sim ///
				(sd)	sd_sf_keep_sim=tot_sf_keep_sim sd_sf_cat_sim =tot_sf_cat_sim sd_sf_rel_sim =tot_sf_rel_sim ///
						  sd_bsb_keep_sim=tot_bsb_keep_sim sd_bsb_rel_sim =tot_bsb_rel_sim sd_bsb_cat_sim =tot_bsb_cat_sim ///
						  sd_scup_keep_sim = tot_scup_keep_sim sd_scup_rel_sim = tot_scup_rel_sim sd_scup_cat_sim = tot_scup_cat_sim sd_dtrip_sim=tot_dtrip_sim, by(state mode)
						  
reshape long tot_ sd_, i(mode state) j(new) string
rename tot_ sim_total 
rename sd_ sim_sd
split new, parse(_)
rename new1 species
rename new2 disp
drop new3
drop new
replace disp="dtrip" if species=="dtrip"
replace species="NA" if disp=="dtrip"


preserve
keep if disp=="dtrip"
gen sim_ul = sim_total+1.96*sim_sd
gen sim_ll = sim_total-1.96*sim_sd
tempfile simdtrip
save `simdtrip', replace
restore 

drop if disp=="dtrip"
tempfile sim
save `sim', replace

u  "$iterative_input_data_cd\catch_total_calib_mrip.dta", clear  
reshape long total se ll ul , i(mode state) j(new) string
rename tot mrip_total 
rename ll mrip_ll
rename ul mrip_ul
drop se
split new, parse(_)
rename new1 species
rename new2 disp
drop new3
drop new 
merge 1:1 state mode species disp using `sim',  keep(3) nogen
gen sim_ul = sim_total+1.96*sim_sd
gen sim_ll = sim_total-1.96*sim_sd

sort species disp mode
split my_dom_id_string, parse(_)
replace  my_dom_id_string=state+"_"+mode
tempfile catch
save `catch', replace 


u  "$iterative_input_data_cd\directed_trip_calib_mrip.dta", clear  
rename se_mrip se_dtrip_mrip
rename ll ll_dtrip_mrip
rename ul ul_dtrip_mrip
rename dtrip_mrip tot_dtrip_mrip
reshape long tot_ se_ ll_ ul_, i(mode state) j(new) string
rename tot_ mrip_total 
rename ll mrip_ll
rename ul_ mrip_ul
drop se_

drop year 
rename new disp
replace disp="dtrip"
gen species="NA"
replace  my_dom_id_string=state+"_"+mode
merge 1:1 state mode species disp using `simdtrip', keep(3)

append using `catch'

drop _merge my_dom_id_string1 my_dom_id_string2 my_dom_id_string3

replace disp="discards" if disp=="rel"
replace disp="harvest" if disp=="keep"
replace disp="catch" if disp=="cat"

gen domain=species+"_"+disp
replace domain="dtrip" if domain=="NA_dtrip"

local vars mrip_total mrip_ll mrip_ul sim_total sim_ul sim_ll
foreach v of local vars{
	replace `v'=`v'/1000
}

tempfile new
save `new', replace 

levelsof state, local(state_list)

foreach s in `state_list'{
	u `new', clear 
	keep if state=="`s'"
	
	encode my_dom_id_string , gen(my_dom_id)  
	gen my_dom_id_mrip = my_dom_id+0.1 
	gen my_dom_id_sim = my_dom_id-0.1  
		
	levelsof domain, local(domain_list)
		foreach d in `domain_list' {


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
			legend(order(2 "MRIP estimate" 4 "Simulated estimate")) ///
			ytitle("# ('000s)", xoffset(-3)) xtitle("") ylabel(#10,  angle(horizontal) ) ///
			xlabel(`xlabels',  angle(45)) ///
			title("`d'", size(medium)) name(`d'_`s', replace) 
			*graph export "`sp'_`disp'_rcap.png", name(graph_`sp'_`disp') width(1000) replace
		}
  }

u `new', clear 

gen pct_diff=((sim_total-mrip_total)/mrip_total)*100
gen diff=sim_total-mrip_total
gen abs_diff=abs(diff)

order state mode species disp pct diff abs_diff sim_total mrip_total
gsort -abs_diff
browse if pct_diff<-5 | pct_diff>5


local vars mrip_total mrip_ll mrip_ul sim_total sim_ul sim_ll
foreach v of local vars{
	replace `v'=`v'*1000
}



grc1leg  bsb_catch_MA sf_catch_MA  scup_catch_MA bsb_harvest_MA sf_harvest_MA   scup_harvest_MA , cols(3) //slightly higher			
grc1leg  bsb_catch_RI sf_catch_RI scup_catch_RI  bsb_harvest_RI sf_harvest_RI scup_harvest_RI    , cols(3)	//looks good	 			
grc1leg  bsb_catch_CT sf_catch_CT scup_catch_CT  bsb_harvest_CT sf_harvest_CT  scup_harvest_CT  , cols(3)	//slightly higher
grc1leg  bsb_catch_NY sf_catch_NY scup_catch_NY  bsb_harvest_NY sf_harvest_NY  scup_harvest_NY  , cols(3)	//looks good	 
grc1leg  bsb_catch_NJ sf_catch_NJ scup_catch_NJ  bsb_harvest_NJ sf_harvest_NJ  scup_harvest_NJ  , cols(3) // higher	
grc1leg  bsb_catch_DE sf_catch_DE scup_catch_DE  bsb_harvest_DE sf_harvest_DE  scup_harvest_DE  , cols(3) //looks good	 	
grc1leg  bsb_catch_MD sf_catch_MD scup_catch_MD  bsb_harvest_MD sf_harvest_MD  scup_harvest_MD  , cols(3)	//looks good	  
grc1leg  bsb_catch_VA sf_catch_VA scup_catch_VA  bsb_harvest_VA sf_harvest_VA  scup_harvest_VA  , cols(3)	//slightly higher
grc1leg  bsb_catch_NC sf_catch_NC scup_catch_NC  bsb_harvest_NC sf_harvest_NC  scup_harvest_NC  , cols(3) //looks good	   	

grc1leg  dtrip_MA  dtrip_RI dtrip_CT dtrip_NY dtrip_NJ dtrip_DE dtrip_MD dtrip_VA dtrip_NC , cols(3) //looks good	   	



*By state 
u "$iterative_input_data_cd\simulated_catch_totals.dta", replace 

collapse (sum) tot_sf_keep_sim tot_sf_cat_sim tot_sf_rel_sim ///
						  tot_bsb_keep_sim tot_bsb_rel_sim tot_bsb_cat_sim ///
						  tot_scup_keep_sim tot_scup_rel_sim tot_scup_cat_sim tot_dtrip_sim, by(state draw)
						  
collapse (mean) tot_sf_keep_sim tot_sf_cat_sim tot_sf_rel_sim ///
						  tot_bsb_keep_sim tot_bsb_rel_sim tot_bsb_cat_sim ///
						  tot_scup_keep_sim tot_scup_rel_sim tot_scup_cat_sim tot_dtrip_sim ///
						  (sd)	sd_sf_keep_sim=tot_sf_keep_sim sd_sf_cat_sim =tot_sf_cat_sim sd_sf_rel_sim =tot_sf_rel_sim ///
						  sd_bsb_keep_sim=tot_bsb_keep_sim sd_bsb_rel_sim =tot_bsb_rel_sim sd_bsb_cat_sim =tot_bsb_cat_sim ///
						  sd_scup_keep_sim = tot_scup_keep_sim sd_scup_rel_sim = tot_scup_rel_sim sd_scup_cat_sim = tot_scup_cat_sim sd_dtrip_sim=tot_dtrip_sim, by(state )			
						  
						  
reshape long tot_ sd_, i(state) j(new) string
rename tot_ sim_total 
rename sd_ sim_sd
split new, parse(_)
rename new1 species
rename new2 disp
drop new3
drop new
replace disp="dtrip" if species=="dtrip"
replace species="NA" if disp=="dtrip"


preserve
keep if disp=="dtrip"
gen sim_ul = sim_total+1.96*sim_sd
gen sim_ll = sim_total-1.96*sim_sd
tempfile simdtrip
save `simdtrip', replace
restore 

drop if disp=="dtrip"
tempfile sim
save `sim', replace

u  "$iterative_input_data_cd\catch_total_calib_mrip_state_total.dta", clear  
reshape long total se ll ul , i(state) j(new) string
rename tot mrip_total 
rename ll mrip_ll
rename ul mrip_ul
drop se
split new, parse(_)
rename new1 species
rename new2 disp
drop new3
drop new 
merge 1:1 state species disp using `sim',  keep(3) nogen
gen sim_ul = sim_total+1.96*sim_sd
gen sim_ll = sim_total-1.96*sim_sd

sort species disp 
split my_dom_id_string, parse(_)
replace  my_dom_id_string=state
tempfile catch
save `catch', replace 


u  "$iterative_input_data_cd\directed_trip_calib_mrip_state_total.dta", clear  
rename se_mrip se_dtrip_mrip
rename ll ll_dtrip_mrip
rename ul ul_dtrip_mrip
rename dtrip_mrip tot_dtrip_mrip
reshape long tot_ se_ ll_ ul_, i(state) j(new) string
rename tot_ mrip_total 
rename ll mrip_ll
rename ul_ mrip_ul
drop se_

drop year 
rename new disp
replace disp="dtrip"
gen species="NA"
replace  my_dom_id_string=state
merge 1:1 state  species disp using `simdtrip', keep(3)

append using `catch'

drop _merge my_dom_id_string1 my_dom_id_string2 

replace disp="discards" if disp=="rel"
replace disp="harvest" if disp=="keep"
replace disp="catch" if disp=="cat"

gen domain=species+"_"+disp
replace domain="dtrip" if domain=="NA_dtrip"

local vars mrip_total mrip_ll mrip_ul sim_total sim_ul sim_ll
foreach v of local vars{
	replace `v'=`v'/1000
}

gen pct_diff=((sim_total-mrip_total)/mrip_total)*100


local species "sf bsb scup"
local disps "catch harvest"
foreach s of local species{
	foreach d of local disps{

su mrip_total if disp=="`d'" & species=="`s'"
local mrip=`r(sum)'

su sim_total if disp=="`d'" & species=="`s'"
local sim=`r(sum)'

local pct_diff=((`sim'-`mrip')/`mrip')*100
di "`s' `d' : `pct_diff'"

	}
}



tempfile new
save `new', replace 

levelsof state, local(state_list)

foreach s in `state_list'{
	u `new', clear 
	keep if state=="`s'"
	
	encode my_dom_id_string , gen(my_dom_id)  
	gen my_dom_id_mrip = my_dom_id+0.05 
	gen my_dom_id_sim = my_dom_id-0.05  
		
	levelsof domain, local(domain_list)
		foreach d in `domain_list' {


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
			legend(order(2 "MRIP estimate" 4 "Simulated estimate")) ///
			ytitle("# ('000s)", xoffset(-3)) xtitle("") ylabel(#10,  angle(horizontal) ) ///
			xlabel(`xlabels',  angle(45)) ///
			title("`d'", size(medium)) name(`d'_`s', replace) 
			*graph export "`sp'_`disp'_rcap.png", name(graph_`sp'_`disp') width(1000) replace
		}
  }

grc1leg  bsb_catch_MA sf_catch_MA  scup_catch_MA bsb_harvest_MA sf_harvest_MA   scup_harvest_MA , cols(3) //slightly higher			
grc1leg  bsb_catch_RI sf_catch_RI scup_catch_RI  bsb_harvest_RI sf_harvest_RI scup_harvest_RI    , cols(3)	//looks good	 			
grc1leg  bsb_catch_CT sf_catch_CT scup_catch_CT  bsb_harvest_CT sf_harvest_CT  scup_harvest_CT  , cols(3)	//slightly higher
grc1leg  bsb_catch_NY sf_catch_NY scup_catch_NY  bsb_harvest_NY sf_harvest_NY  scup_harvest_NY  , cols(3)	//looks good	 
grc1leg  bsb_catch_NJ sf_catch_NJ scup_catch_NJ  bsb_harvest_NJ sf_harvest_NJ  scup_harvest_NJ  , cols(3) // higher	
grc1leg  bsb_catch_DE sf_catch_DE scup_catch_DE  bsb_harvest_DE sf_harvest_DE  scup_harvest_DE  , cols(3) //looks good	 	
grc1leg  bsb_catch_MD sf_catch_MD scup_catch_MD  bsb_harvest_MD sf_harvest_MD  scup_harvest_MD  , cols(3)	//looks good	  
grc1leg  bsb_catch_VA sf_catch_VA scup_catch_VA  bsb_harvest_VA sf_harvest_VA  scup_harvest_VA  , cols(3)	//slightly higher
grc1leg  bsb_catch_NC sf_catch_NC scup_catch_NC  bsb_harvest_NC sf_harvest_NC  scup_harvest_NC  , cols(3) //looks good	   	

grc1leg  dtrip_MA  dtrip_RI dtrip_CT dtrip_NY dtrip_NJ dtrip_DE dtrip_MD dtrip_VA dtrip_NC , cols(3) //looks good	   	
