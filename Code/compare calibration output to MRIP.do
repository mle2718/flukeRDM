

global tripz
local statez "MA RI"
foreach s of local statez{
	
forv i=1/5{
	

u "$iterative_input_data_cd\calib_catch_draws_`s'_`i'.dta", clear 

collapse (mean) sf_keep_sim sf_cat sf_rel_sim bsb_keep_sim bsb_rel_sim bsb_cat scup_keep_sim scup_rel_sim scup_cat, by(date day_i mode state)


preserve 
import delimited using  "$input_data_cd\directed_trips_calibration_`s'", clear 
keep if draw==`i'
keep mode date  dtrip
tempfile dtrip
save `dtrip', replace
restore

merge 1:1 mode date  using `dtrip'
mvencode sf_keep_sim sf_cat sf_rel_sim bsb_keep_sim bsb_rel_sim bsb_cat scup_keep_sim scup_rel_sim scup_cat dtrip, mv(0) override

rename sf_cat sf_cat_sim
rename bsb_cat bsb_cat_sim
rename scup_cat scup_cat_sim

drop _merge


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


u "$iterative_input_data_cd\simulated_catch_totals.dta", clear 


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
replace species="NA" if species=="dtrip"
replace disp="NA" if disp=="sim"

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
rename se_ mrip_se

drop year 
rename new disp
replace disp="NA"
gen species="NA"
replace  my_dom_id_string=state+"_"+mode
merge 1:1 state mode species disp using `catch'
,  keep(3) nogen

,  keep(3) nogen




encode my_dom_id_string, gen(my_dom_id)

levelsof species, local(species_list)
levelsof disp, local(disp_list)

foreach sp in `species_list' {
  foreach disp in `disp_list' {
    
    twoway ///
      (rcap mrip_ul mrip_ll my_dom_id if species == "`sp'" & disp == "`disp'", ///
         color(blue) label) ///
      (scatter mrip_total my_dom_id if species == "`sp'" & disp == "`disp'", ///
        msymbol(o) mcolor(blue)) ///
      (rcap sim_ul sim_ll my_dom_id if species == "`sp'" & disp == "`disp'", ///
         color(red)) ///
      (scatter sim_total my_dom_id if species == "`sp'" & disp == "`disp'", ///
        msymbol(x) mcolor(red)), ///
      legend(order(2 "MRIP estimate" 4 "Simulated estimate")) ///
      ytitle("Catch total") xtitle("Total") ///
      title("`disp' totals for `sp' by domain") ///
      name(graph_`sp'_`disp', replace) ///
      graphregion(color(white)) ///
      scheme(s1color)
      
  }
}


gen my_dom_id_mrip = my_dom_id+0.05 
gen my_dom_id_sim = my_dom_id-0.05 

replace disp="discards" if disp=="rel"
replace disp="harvest" if disp=="keep"
replace disp="catch" if disp=="cat"

levelsof species, local(species_list)
levelsof disp, local(disp_list)


foreach sp in `species_list' {
  foreach disp in `disp_list' {
    
	
* Start by clearing any existing macro
local xlabels ""

* Loop over the levels of the encoded variable
levelsof my_dom_id, local(levels)

foreach l of local levels {
    local label : label (my_dom_id) `l'
    local xlabels `xlabels' `l' "`label'" 
}


twoway (rcap mrip_ul mrip_ll my_dom_id_mrip if species == "`sp'" & disp == "`disp'", color(blue)  ) ///
			(scatter mrip_total my_dom_id_mrip if species == "`sp'" & disp == "`disp'",  msymbol(o) mcolor(blue)) ///
			(rcap sim_ul sim_ll my_dom_id_sim if species == "`sp'" & disp == "`disp'",  color(red)) ///
			(scatter sim_total my_dom_id_sim if species == "`sp'" & disp == "`disp'", msymbol(o) mcolor(red)), ///
			legend(order(2 "MRIP estimate" 4 "Simulated estimate")) ///
			ytitle("# fish") xtitle("") ///
			xlabel(`xlabels',  angle(45)) ///
			title("`disp' totals for `sp'")
			*graph export "`sp'_`disp'_rcap.png", name(graph_`sp'_`disp') width(1000) replace

  }
}
	  






merge 1:1 state mode using  "$iterative_input_data_cd\catch_total_calib_mrip.dta", nogen
drop year 


order my_dom_id_string se_mrip ll_mrip ul_mrip state year mode dtrip* *sf*