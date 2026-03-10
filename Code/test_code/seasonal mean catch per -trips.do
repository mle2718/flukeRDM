

u "$input_data_cd\simulated_projected_means_copula.dta", clear 

split my, parse(_)
drop my_dom_id_string4
rename my_dom_id_string1 state
rename my_dom_id_string2 wave
rename my_dom_id_string3 mode

encode wave, gen(wave2)


keep sf_cat_sim bsb_cat_sim scup_cat_sim draw state wave mode wave2
replace mode="_"+mode
reshape wide sf bsb scup, i(draw state wave wave2) j(mode) string

ds draw state wave wave2, not
local vars "`r(varlist)'"
foreach v of local vars{
	replace `v'=. if `v'==0
}

graph box sf_cat_sim_pr sf_cat_sim_fh sf_cat_sim_sh if state=="MA" , ///
    over(wave, label(labsize(small))) ///
    box(1, fcolor(navy)   lcolor(navy)) ///
    box(2, fcolor(maroon) lcolor(maroon)) ///
	box(3, fcolor(gray) lcolor(gray)) ///
    marker(1, msymbol(O) msize(tiny) mcolor(navy)) ///
    marker(2, msymbol(O) msize(tiny) mcolor(maroon)) ///
	marker(3, msymbol(O) msize(tiny) mcolor(gray)) ///
    legend(order(1 "Private" 2 "For-hire" 3 "Shore") position(6) cols(3)) /// 
	ylab(, labsize(small))

	
	
graph box bsb_cat_sim_pr bsb_cat_sim_fh bsb_cat_sim_sh if state=="MA" , ///
    over(wave, label(labsize(small))) ///
    box(1, fcolor(navy)   lcolor(navy)) ///
    box(2, fcolor(maroon) lcolor(maroon)) ///
	box(3, fcolor(gray) lcolor(gray)) ///
    marker(1, msymbol(O) msize(tiny) mcolor(navy)) ///
    marker(2, msymbol(O) msize(tiny) mcolor(maroon)) ///
	marker(3, msymbol(O) msize(tiny) mcolor(gray)) ///
    legend(order(1 "Private" 2 "For-hire" 3 "Shore") position(6) cols(3)) /// 
	ylab(, labsize(small))
	
	
	///
    ///
	
	///
    title("Black sea bass North", size(medium)) ///
	ytitle(Proportion of fish that are length-{it:l}) ///



