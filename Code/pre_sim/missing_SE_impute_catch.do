

*This file computes estimates of catch/harvest/discards at more highly aggregated strata than the baseline strata in order to obtain SEs for baseline strata with missing SEs
*These estimates are for rounds 2-5 below, and are save as separate data files


// Deal with strata containing no estimated SEs. The estimation strata is: most recent two years + wave + state + mode
// For these strata, I'll impute a PSE from other strata and apply it to the missing-SE strata. Up to 7 rounds of imputation are conducted:
	// 1) most recent two years + wave + state + mode, averaged across shouldering catch-frequency bins unless a shouldering catch-frequency bin==0
	// 2a) most recent two years + wave + region (MA-NY, NJ-NC)  + mode
		// 2b) repeat 1) because some missing SE may have been imputed by 2a) 
	// 3a) most recent two years + half-year + region (MA-NY, NJ-NC)  + mode
		// 3b) repeat 1) because some missing SE may have been imputed by 3a) 
	// 4a) most recent two years + full-year + region (MA-NY, NJ-NC)  + mode
		// 4b) repeat 1) because some missing SE may have been imputed by 4a) 
	// 5a) most recent two years + full-year + coastwide (MA-NY, NJ-NC)  + mode
		// 5b) repeat 1) because some missing SE may have been imputed by 4a) 
	// 6) set the missing SE equal to the point estimate

gen region2="N" if inlist(state, "MA", "RI", "CT", "NY")
replace region2="S" if inlist(state, "NJ", "DE", "MD", "VA", "NC")

gen my_dom_id_string_aggregate=region2+"_"+wv2+"_"+mode1+"_"+common_dom

tempfile new 
save `new', replace

*Loop over domains to get estimated numbers of trips that caught, harvested, and released X fish and se's
global catch 
levelsof my_dom_id_string_aggregate, local(sts)
foreach s of local sts{

di "`s'"
qui{
u `new', clear 

**SF catch
preserve
estpost svy: tab sf_catch my_dom_id_string_aggregate if my_dom_id_string_aggregate=="`s'", count ci se format(%11.3g)

mat b = e(b)'
mat se = e(se)'
mat rownames = e(Row)'

clear 
svmat rownames
svmat b
svmat se
		
drop if rownames==.
rename rownames nfish
rename b1 tot_ntrips
rename se se_ntrips
gen disp="catch"		
gen species="sf"
gen domain="`s'"
tempfile sf`s'catch
save `sf`s'catch', replace 
restore 


**BSB catch
preserve
estpost svy: tab bsb_catch my_dom_id_string_aggregate if my_dom_id_string_aggregate=="`s'", count ci se format(%11.3g)
mat b = e(b)'
mat se = e(se)'
mat rownames = e(Row)'

clear 
svmat rownames
svmat b
svmat se
		
drop if rownames==.
rename rownames nfish
rename b1 tot_ntrips
rename se se_ntrips
gen disp="catch"			
gen species="bsb"
gen domain="`s'"
tempfile bsb`s'catch
save `bsb`s'catch', replace 
restore 


**scup catch
preserve
estpost svy: tab scup_catch my_dom_id_string_aggregate if my_dom_id_string_aggregate=="`s'", count ci se format(%11.3g)
mat b = e(b)'
mat se = e(se)'
mat rownames = e(Row)'

clear 
svmat rownames
svmat b
svmat se
		
drop if rownames==.
rename rownames nfish
rename b1 tot_ntrips
rename se se_ntrips
gen disp="catch"			
gen species="scup"
gen domain="`s'"
tempfile scup`s'catch
save `scup`s'catch', replace 
restore 

**sf keep
preserve
estpost svy: tab sf_keep my_dom_id_string_aggregate if my_dom_id_string_aggregate=="`s'", count ci se format(%11.3g)

mat b = e(b)'
mat se = e(se)'
mat rownames = e(Row)'

clear 
svmat rownames
svmat b
svmat se
		
drop if rownames==.
rename rownames nfish
rename b1 tot_ntrips
rename se se_ntrips
gen disp="keep"		
gen species="sf"
gen domain="`s'"
tempfile sf`s'keep
save `sf`s'keep', replace 
restore 


**bsb keep
preserve
estpost svy: tab bsb_keep my_dom_id_string_aggregate if my_dom_id_string_aggregate=="`s'", count ci se format(%11.3g)
mat b = e(b)'
mat se = e(se)'
mat rownames = e(Row)'

clear 
svmat rownames
svmat b
svmat se
		
drop if rownames==.
rename rownames nfish
rename b1 tot_ntrips
rename se se_ntrips
gen disp="keep"	
gen species="bsb"
gen domain="`s'"
tempfile bsb`s'keep
save `bsb`s'keep', replace 
restore 

**scup keep
preserve
estpost svy: tab scup_keep my_dom_id_string_aggregate if my_dom_id_string_aggregate=="`s'", count ci se format(%11.3g)
mat b = e(b)'
mat se = e(se)'
mat rownames = e(Row)'

clear 
svmat rownames
svmat b
svmat se
		
drop if rownames==.
rename rownames nfish
rename b1 tot_ntrips
rename se se_ntrips
gen disp="keep"	
gen species="scup"
gen domain="`s'"
tempfile scup`s'keep
save `scup`s'keep', replace 
restore 


**sf release
preserve
estpost svy: tab sf_rel my_dom_id_string_aggregate if my_dom_id_string_aggregate=="`s'", count ci se format(%11.3g)

mat b = e(b)'
mat se = e(se)'
mat rownames = e(Row)'

clear 
svmat rownames
svmat b
svmat se
		
drop if rownames==.
rename rownames nfish
rename b1 tot_ntrips
rename se se_ntrips
gen disp="rel"	
gen species="sf"
gen domain="`s'"
tempfile sf`s'rel
save `sf`s'rel', replace 
restore 


**bsb release
preserve
estpost svy: tab bsb_rel my_dom_id_string_aggregate if my_dom_id_string_aggregate=="`s'", count ci se format(%11.3g)
mat b = e(b)'
mat se = e(se)'
mat rownames = e(Row)'

clear 
svmat rownames
svmat b
svmat se
		
drop if rownames==.
rename rownames nfish
rename b1 tot_ntrips
rename se se_ntrips
gen disp="rel"	
gen species="bsb"
gen domain="`s'"
tempfile bsb`s'rel
save `bsb`s'rel', replace 
restore 

**scup release
preserve
estpost svy: tab scup_rel my_dom_id_string_aggregate if my_dom_id_string_aggregate=="`s'", count ci se format(%11.3g)
mat b = e(b)'
mat se = e(se)'
mat rownames = e(Row)'

clear 
svmat rownames
svmat b
svmat se
		
drop if rownames==.
rename rownames nfish
rename b1 tot_ntrips
rename se se_ntrips
gen disp="rel"	
gen species="scup"
gen domain="`s'"
tempfile scup`s'rel
save `scup`s'rel', replace 
restore 

u `sf`s'catch', clear 
append using  `bsb`s'catch'
append using  `scup`s'catch'
append using  `sf`s'keep'
append using  `bsb`s'keep'
append using  `scup`s'keep'
append using  `sf`s'rel'
append using  `bsb`s'rel'
append using  `scup`s'rel'

}
tempfile catch`s'
save `catch`s'', replace
global catch "$catch "`catch`s''" " 

}	

clear
dsconcat $catch // This dataset contains an estimate of the number of trips that caught/kept/discarded X number of each of the species.
sort domain species nfish
split domain, parse(_)
rename domain2 wave
rename domain3 mode

expand 4 if domain1=="N"
sort domain species disp nfish
bysort domain species disp nfish: gen n=_n
gen state="MA" if domain1=="N" & n==1
replace state="RI" if domain1=="N" & n==2
replace state="CT" if domain1=="N" & n==3
replace state="NY" if domain1=="N" & n==4
expand 5 if domain1=="S"
drop n
bysort domain species disp nfish: gen n=_n
replace state="NJ" if domain1=="S" & n==1
replace state="DE" if domain1=="S" & n==2
replace state="MD" if domain1=="S" & n==3
replace state="VA" if domain1=="S" & n==4
replace state="NC" if domain1=="S" & n==5
drop n domain*

rename tot tot_ntrips_agg
rename se se_ntrips_agg

save "$iterative_input_data_cd\catch per trip aggregate.dta", replace 