


/*This code uses the MRIP data to 
	1) estimate harvest, discards, and catch frequencies (i.e., # of trips that harvested/discarded/caught X fish) and their standard error at the month and mode (pr/fh) level using the most recent full year of data
	****Note that we compute catch-per-trip distributions based on harvest- and discard-per-trip distribrutions because we calibrate the model to total harvest and we need,  
	 for each draw of MRIP data, a draw of total harvest that is reflective of the random draw from the directed trips and harvest-per-trip distribution.Essesntially, to compute 
	 catch-per-trip, I take the sum of a random draw of discard frequencies and harvest frequencies. That way I also retain harvest-per-trip distributions, which I can multiply 
	 by the total number of trips drawn from the directed trip distributions to arrive at total simulated harvest. I also assess whether my simulated total harvest/discards/catch estimates
	 are similar to the MRIP point esimates in catch_totals_compare_model2mrip_open_seasons.
	 random draw of discar
	2) use those estimates to create 150 random draws of harvest, discards, and catch frequencies for each stratum
	2b) correct for the fact that a draw of number of trips cannot be <0 
	3) combine these data with the angler characterstics data
	4) create 150 draws of catch-per-trip and angler characteristics. 
*/
		

* Pull in MRIP data

cd $input_data_cd

clear
mata: mata clear

tempfile tl1 cl1
dsconcat $triplist

sort year strat_id psu_id id_code
drop if strmatch(id_code, "*xx*")==1
duplicates drop 
save `tl1'
clear

dsconcat $catchlist
sort year strat_id psu_id id_code
replace common=subinstr(lower(common)," ","",.)
save `cl1'

replace var_id=strat_id if strmatch(var_id,"")

use `tl1'
merge 1:m year strat_id psu_id id_code using `cl1', keep(1 3) nogenerate /*Keep all trips including catch==0*/
replace var_id=strat_id if strmatch(var_id,"")


* Format MRIP data for estimation 

// ensure only relevant states 
keep if inlist(st, 25, 44, 9,  36 , 34, 10, 24, 51, 37)


keep if $calibration_catch_per_trip_years
 
gen st2 = string(st,"%02.0f")

gen state="MA" if st==25
replace state="MD" if st==24
replace state="RI" if st==44
replace state="CT" if st==9
replace state="NY" if st==36
replace state="NJ" if st==34
replace state="DE" if st==10
replace state="VA" if st==51
replace state="NC" if st==37

gen mode1="sh" if inlist(mode_fx, "1", "2", "3")
replace mode1="pr" if inlist(mode_fx, "7")
replace mode1="fh" if inlist(mode_fx, "4", "5")

// classify trips that I care about into the things I care about (caught or targeted sf/bsb) and things I don't care about "ZZ" 
replace prim1_common=subinstr(lower(prim1_common)," ","",.)
replace prim2_common=subinstr(lower(prim1_common)," ","",.)

/* we need to retain 1 observation for each strat_id, psu_id, and id_code.  */
/* A.  Trip (Targeted or Caught) (fluke, sea bass, or scup) then it should be marked in the domain "_ATLCO"
   B.  Trip did not (Target or Caught) (fluke, sea bass, or scup) then it is marked in the the domain "ZZZZZ"
*/

gen common_dom="ZZ"
replace common_dom="SF" if inlist(common, "summerflounder") 
replace common_dom="SF" if inlist(common, "blackseabass") 
replace common_dom="SF" if inlist(common, "scup") 

replace common_dom="SF"  if inlist(prim1_common, "summerflounder") 
replace common_dom="SF"  if inlist(prim1_common, "blackseabass") 
replace common_dom="SF"  if inlist(prim1_common, "scup") 

tostring wave, gen(wv2)
tostring year, gen(yr2)

gen my_dom_id_string=state+"_"+wv2+"_"+mode1+"_"+common_dom

gen sf_tot_cat=tot_cat if common=="summerflounder"
egen sum_sf_tot_cat=sum(sf_tot_cat), by(strat_id psu_id id_code)

gen sf_harvest=landing if common=="summerflounder"
egen sum_sf_harvest=sum(sf_harvest), by(strat_id psu_id id_code)
 
gen sf_releases=release if common=="summerflounder"
egen sum_sf_releases=sum(sf_releases), by(strat_id psu_id id_code)
 
gen bsb_tot_cat=tot_cat if common=="blackseabass"
egen sum_bsb_tot_cat=sum(bsb_tot_cat), by(strat_id psu_id id_code)

gen bsb_harvest=landing if common=="blackseabass"
egen sum_bsb_harvest=sum(bsb_harvest), by(strat_id psu_id id_code)

gen bsb_releases=release if common=="blackseabass"
egen sum_bsb_releases=sum(bsb_releases), by(strat_id psu_id id_code)

gen scup_tot_cat=tot_cat if common=="scup"
egen sum_scup_tot_cat=sum(scup_tot_cat), by(strat_id psu_id id_code)

gen scup_harvest=landing if common=="scup"
egen sum_scup_harvest=sum(scup_harvest), by(strat_id psu_id id_code)

gen scup_releases=release if common=="scup"
egen sum_scup_releases=sum(scup_releases), by(strat_id psu_id id_code)

drop sf_tot_cat sf_harvest sf_releases bsb_tot_cat bsb_harvest bsb_releases  scup_tot_cat scup_harvest scup_releases
rename sum_sf_tot_cat sf_catch
rename sum_sf_harvest sf_keep
rename sum_sf_releases sf_rel
rename sum_bsb_tot_cat bsb_catch
rename sum_bsb_harvest bsb_keep
rename sum_bsb_releases bsb_rel
rename sum_scup_tot_cat scup_catch
rename sum_scup_harvest scup_keep
rename sum_scup_releases scup_rel

/* Set a variable "no_dup"=0 if the record is "$my_common" catch and no_dup=1 otherwise.*/
  
gen no_dup=0
replace no_dup=1 if  strmatch(common, "summerflounder")==0
replace no_dup=1 if strmatch(common, "blackseabass")==0
replace no_dup=1 if strmatch(common, "scup")==0

/*
We sort on year, strat_id, psu_id, id_code, "no_dup", and "my_dom_id_string". For records with duplicate year, strat_id, psu_id, and id_codes, the first entry will be "my_common catch" if it exists.  These will all be have sp_dom "SF."  If there is no my_common catch, but the trip targeted (fluke, sea bass, or scup) or caught either species, the secondary sorting on "my_dom_id_string" ensures the trip is properly classified.

After sorting, we generate a count variable (count_obs1 from 1....n) and we keep only the "first" observations within each "year, strat_id, psu_id, and id_codes" group.
*/

bysort year strat_id psu_id id_code (my_dom_id_string no_dup): gen count_obs1=_n

keep if count_obs1==1 // This keeps only one record for trips with catch of multiple species. We have already computed catch of the species of interest above and saved these in a trip-row

order strat_id psu_id id_code no_dup my_dom_id_string count_obs1 common

svyset psu_id [pweight= wp_int], strata(strat_id) singleunit(certainty)

/*
local vars sf_catch sf_keep sf_rel bsb_catch bsb_keep bsb_rel  scup_catch scup_keep scup_rel
foreach v of local vars{
	replace `v'=round(`v')
}
*/

keep if common_dom=="SF"
drop if wp_int==0
encode my_dom_id_string, gen(my_dom_id)

*keep if state=="CT" & mode1=="fh" & common_dom=="SF" & inlist(wv2, "6", "5")

*keep if state=="MA"
*keep my_dom_id_string my_dom_id sf_catch-scup_rel wp_int mode1 state wv2
*replace wp_int=round(wp_int)

preserve
keep my_dom_id my_dom_id_string
duplicates drop 
tempfile domains
save `domains', replace 
restore


tempfile basefile
save `basefile', replace 


* Here I will estimate mean catch/harvest/discards per trip for each strata in order to identify strata with missing SE
* For strata with missing SE's, I'll follow similar approch to MRIP's hot and cold deck imputation for observations with missing lengths and weights
/* From the MRIP data handbook:
For intercepted angler trips with landings where both length and weight measurements are missing, paired length and weight observations are imputed from complete cases using hot and cold deck imputation. (Complete cases include records with both length and weight data available, as well as records where we were able to compute a missing length or weight using the length-weight modeling described above.) Up to five rounds of imputation are conducted in an attempt to fill in missing values. These rounds begin with imputation cells that correspond to the most detailed MRIP estimation cells, but are aggregated to higher levels in subsequent rounds to bring in more length-weight data. 
	● Round 1: Current year, two-month sampling wave, sub-region, state, mode, area fished, species. 
	● Round 2: Current year, half-year, sub-region, state, mode, species. 
	● Round 3: Current + most recent prior year, two-month sampling wave, sub-region, state, mode, area fished, species. 
	● Round 4: Current + most recent prior year, sub-region, state, mode, species. 
	● Round 5: Current + most recent prior year, sub-region, species.
 */

* The current estimation strata is: most recent two years + wave + state + mode, for harvest/discards/catch per trip
* For strata with missing, I'll impute a PSE from other strata and apply it to the missing-SE strata. 
	*● Round 1: most recent two years + TWO WAVE PERIOD + state + mode
	*● Round 2: most recent two years + HALF YEAR PERIOD + state + mode


* Create a postfile to collect results
tempfile results
postfile handle str15 varname str15 domain float mean se ll95 ul95 using `results', replace

* Loop over variables
foreach var in sf_keep sf_rel sf_catch bsb_keep bsb_rel bsb_catch scup_keep scup_rel scup_catch {

    * Run svy mean for the variable by domain
    svy: mean `var', over(my_dom_id)

    * Grab result matrix and domain labels
    matrix M = r(table)
    local colnames : colnames M

    * Loop over columns (domains)
    foreach col of local colnames {
        local m  = M[1, "`col'"]
        local se = M[2, "`col'"]
        local lb = M[5, "`col'"]
        local ub = M[6, "`col'"]

        post handle ("`var'") ("`col'") (`m') (`se') (`lb') (`ub')
    }
}

postclose handle

* Load results back into memory
use `results', clear

split domain, parse("@")
drop domain1
split domain2, parse(.)
drop domain2 domain22
replace domain21="1" if domain21=="1bn"
destring domain21, replace
rename domain21 my_dom_id
merge m:1 my_dom_id using `domains' 
sort varname  my_dom_id
keep varname mean se my_dom_id_string

tempfile base_results
save `base_results', replace

drop if mean==0
gen pse=se/mean
keep if se==.

split my, parse(_)
rename my_dom_id_string1 state
rename my_dom_id_string2 wave
rename my_dom_id_string3 mode

gen shoulder_wave="2" if wave=="1"
replace shoulder_wave="1" if wave=="2"
replace shoulder_wave="4" if wave=="3"
replace shoulder_wave="3" if wave=="4"
replace shoulder_wave="6" if wave=="5"
replace shoulder_wave="5" if wave=="6"

gen strata_id=_n
levelsof strata_id, local(stratz)

tempfile missing_se
save `missing_se', replace 

* Round 1
global impute
foreach s of local stratz{
	u `missing_se', clear 
	keep if strata_id==`s'
	
	levelsof state, local(st) clean
	levelsof mode, local(md) clean
	levelsof wave, local(wave1) clean
	levelsof shoulder_wave, local(wave2) clean
	levelsof varname, local(outcome) clean
	levelsof my_dom_id_string, local(my_dom_id_string) clean

	u `basefile', clear 
	keep if state=="`st'" & mode1=="`md'" & inlist(wv2, "`wave1'", "`wave2'")
	drop my_dom_id_string my_dom_id
	gen my_dom_id_string="`my_dom_id_string'"
	encode my_dom_id_string, gen(my_dom_id)

	
	* Create a postfile to collect results
	tempfile results
	postfile handle2 str15 varname str15 domain float mean se ll95 ul95 using `results', replace

    * Run svy mean for the variable by domain
    svy: mean `outcome', over(my_dom_id)

    * Grab result matrix and domain labels
    matrix M = r(table)
    local colnames : colnames M

    * Loop over columns (domains)
    foreach col of local colnames {
        local m  = M[1, "`col'"]
        local se = M[2, "`col'"]
        local lb = M[5, "`col'"]
        local ub = M[6, "`col'"]

        post handle2 ("`outcome'") ("`my_dom_id_string'") (`m') (`se') (`lb') (`ub')
    }


postclose handle2

* Load results back into memory
use `results', clear

gen pse_impute=se/mean
rename domain my_dom_id_string
keep varname pse_impute my_dom_id_string

tempfile impute`s'
save `impute`s'', replace
global impute "$impute "`impute`s''" " 

}	
dsconcat $impute 

merge 1:1  varname my_dom_id_string using `missing_se'

preserve 
keep if pse_impute!=.
tempfile round1
save `round1', replace
restore

drop if pse_impute!=.

drop shoulder_wave*

gen shoulder_wave1="2" if wave=="1"
gen shoulder_wave2="3" if wave=="1"

replace shoulder_wave1="1" if wave=="2"
replace shoulder_wave2="3" if wave=="2"

replace shoulder_wave1="1" if wave=="3"
replace shoulder_wave2="2" if wave=="3"

replace shoulder_wave1="5" if wave=="4"
replace shoulder_wave2="6" if wave=="4"

replace shoulder_wave1="4" if wave=="5"
replace shoulder_wave2="6" if wave=="5"

replace shoulder_wave1="4" if wave=="6"
replace shoulder_wave2="5" if wave=="6"

levelsof strata_id, local(stratz)

drop _merge 

tempfile missing_se
save `missing_se', replace 

* Round 2
global impute2
foreach s of local stratz{
	u `missing_se', clear 
	keep if strata_id==`s'
	
	levelsof state, local(st) clean
	levelsof mode, local(md) clean
	levelsof wave, local(wave1) clean
	levelsof shoulder_wave1, local(wave2) clean
	levelsof shoulder_wave2, local(wave3) clean
	levelsof varname, local(outcome) clean
	levelsof my_dom_id_string, local(my_dom_id_string) clean

	u `basefile', clear 
	keep if state=="`st'" & mode1=="`md'" & inlist(wv2, "`wave1'", "`wave2'", "`wave3'")
	drop my_dom_id_string my_dom_id
	gen my_dom_id_string="`my_dom_id_string'"
	encode my_dom_id_string, gen(my_dom_id)

	
	* Create a postfile to collect results
	tempfile results
	postfile handle2 str15 varname str15 domain float mean se ll95 ul95 using `results', replace

    * Run svy mean for the variable by domain
    svy: mean `outcome', over(my_dom_id)

    * Grab result matrix and domain labels
    matrix M = r(table)
    local colnames : colnames M

    * Loop over columns (domains)
    foreach col of local colnames {
        local m  = M[1, "`col'"]
        local se = M[2, "`col'"]
        local lb = M[5, "`col'"]
        local ub = M[6, "`col'"]

        post handle2 ("`outcome'") ("`my_dom_id_string'") (`m') (`se') (`lb') (`ub')
    }


postclose handle2

* Load results back into memory
use `results', clear

gen pse_impute=se/mean
rename domain my_dom_id_string
keep varname pse_impute my_dom_id_string

tempfile impute2`s'
save `impute2`s'', replace
global impute2 "$impute2 "`impute2`s''" " 

}	
dsconcat $impute2

merge 1:1  varname my_dom_id_string using `missing_se'
append using `round1'
keep varname my_dom_id_string pse_impute 
merge 1:1 varname my_dom_id_string using `base_results'

replace se=mean*pse_impute if se==. & _merge==3


* Stop code if non-value mean harvest/discards/catch-per trip are missing standard errors
* Check condition across the dataset
summarize if mean != 0 & missing(se)

* If any observations meet the condition, stop
if r(N) > 0 {
    display "Stopping: mean is not zero and se is missing for some observations."
    exit 1
}

gen missing_se=1 if _merge==3
drop _merge
sort my_dom_id_string var
drop pse
reshape wide mean se missing, i(my) j(varname) string

* make indicator variables for whether each domain contains keep, discards, or keep and discards of each species 
mvencode meanbsb_keep meanbsb_rel meanscup_keep meanscup_rel meansf_keep meansf_rel, mv(0) override

gen sf_only_keep=1 if meansf_keep>0 & meansf_rel==0
gen sf_only_rel=1 if meansf_rel>0 & meansf_keep==0
gen sf_keep_and_rel=1 if meansf_rel>0 & meansf_keep>0
gen sf_no_catch=1 if meansf_rel==0 & meansf_keep==0

gen bsb_only_keep=1 if meanbsb_keep>0 & meanbsb_rel==0
gen bsb_only_rel=1 if meanbsb_rel>0 & meanbsb_keep==0
gen bsb_keep_and_rel=1 if meanbsb_rel>0 & meanbsb_keep>0
gen bsb_no_catch=1 if meanbsb_rel==0 & meanbsb_keep==0

gen scup_only_keep=1 if meanscup_keep>0 & meanscup_rel==0
gen scup_only_rel=1 if meanscup_rel>0 & meanscup_keep==0
gen scup_keep_and_rel=1 if meanscup_rel>0 & meanscup_keep>0
gen scup_no_catch=1 if meanscup_rel==0 & meanscup_keep==0

mvencode sf_only_keep sf_only_rel sf_keep_and_rel sf_no_catch bsb_only_keep bsb_only_rel bsb_keep_and_rel bsb_no_catch scup_only_keep scup_only_rel scup_keep_and_rel scup_no_catch, mv(0) override

*egen check_sf=rowtotal(sf_only_keep sf_only_rel sf_keep_and_rel sf_no_catch)
*egen check_bsb=rowtotal(bsb_only_keep bsb_only_rel bsb_keep_and_rel bsb_no_catch)
*egen check_scup=rowtotal(scup_only_keep scup_only_rel scup_keep_and_rel scup_no_catch)

merge 1:m my_dom_id_string using `basefile'


keep wp_int my_dom_id_string meanbsb_keep-id_code year yr2 st  state common_dom sf_catch-scup_rel my_dom_id sf_only_keep sf_only_rel sf_keep_and_rel sf_no_catch bsb_only_keep bsb_only_rel bsb_keep_and_rel bsb_no_catch scup_only_keep scup_only_rel scup_keep_and_rel scup_no_catch

mvencode se*, mv(0) override
mvencode missing*, mv(0) override

export excel "$iterative_input_data_cd\baseline_mrip_catch_processed.xlsx", firstrow(variables) replace


keep if state=="MA"
keep my_dom_id_string  meanbsb_keep meanbsb_rel meanscup_keep missing_sescup_keep meansf_keep meansf_rel
duplicates drop 
tempfile base
save `base', replace

import excel using "C:/Users/andrew.carr-harris/Desktop/flukeRDM_iterative_data/check_data.xlsx", clear firstrow
merge 1:1 my_dom_id_string using `base'
 order my mean_sf_keep meansf_keep mean_sf_rel meansf_rel mean_bsb_keep meanbsb_keep mean_bsb_rel meanbsb_rel mean_scup_keep meanscup_keep

save "$iterative_input_data_cd\MRIP catch data.dta", replace 
u "$iterative_input_data_cd\MRIP catch data.dta", clear 
keep if state=="MA"
keep if meansf_keep==0 & meansf_rel!=0
keep if my_dom_id_string=="MA_4_sh_SF"
keep wp_int sf_keep sf_rel sf_catch my_dom_id  strat_id psu_id my_dom_id_string
export excel using "$iterative_input_data_cd\test_copula2.xlsx", firstrow(variable) replace 

save "$iterative_input_data_cd\test_copula2.dta", replace 

save "$iterative_input_data_cd\MRIP catch data.dta", replace 


*Now use the emprical distribution of catch at the state, wave, and mode level to create a file that contains:
	*a) 50 trips per day of the fishing season, each with 30 draws of catch-per-trip
	*b) demographics for each trip that are constant across catch draws
	
*Add age and avidities here
// ages
*Ages come from the fishing effort survey 12 MONTH files. These are NOT publicly available and the data have not been processeed for QA/QC like the publicly available 2-month files. 
*Data from 2018-2023 was delivered by Lucas Johanssen on 4/23/2025. A few notes/caveats from Lucas:
		*FES QC processes focus on the 2-month reference periods, and we do very little evaluation and editing of 12-month effort responses.  Responses for these fields are essentially unedited, raw data.  
		*The final weight trimming procedures focus on reducing the impacts of outlier values on wave-level estimates.  The data may include records that are highly influential with respect to 12-month effort and any estimates may be highly variable.  
		*Wave data will produce independent estimates of 12-month effort.
		
*I will use the most recent year available (2023)

global dems
local wvs 1 2 3 4 5 6
foreach w of local wvs{
	
u  "$input_data_cd\fes_person_final_2023`w'.dta", clear 

gen state="MA" if st==25
replace state="MD" if st==24
replace state="RI" if st==44
replace state="CT" if st==9
replace state="NY" if st==36
replace state="NJ" if st==34
replace state="DE" if st==10
replace state="VA" if st==51
replace state="NC" if st==37
keep if state!=""

keep if state=="MA"

gen total_trips_12=boat_trips_12+shore_trips_12
gen total_trips_2=boat_trips+shore_trips

*Data checks
drop if age==-3 //drop missing ages
replace total_trips_2=round(total_trips_2)
replace total_trips_12=round(total_trips_12)
drop if total_trips_2>62 //drop if total trips 2 months>60 
drop if total_trips_12>365 //drop if total trips 12 months>365 
drop if total_trips_2>total_trips_12 //drop if total trips 2 months>total trips 12 months
keep if age>=16 //drop anglers below the minimum age required for license. I do so to align the age distribution with choice experiment sampling frame, which is based on license frames

replace final_wt=round(final_wt)
expand final_wt
sample 10000, count


tempfile dems`w'
save `dems`w'', replace
global dems "$dems "`dems`w''" " 

}
clear
dsconcat $dems
keep age total_trips_12 wave state


tempfile angler_dems 
save `angler_dems', replace 
save "$iterative_input_data_cd\angler_dems.dta", replace 



import delimited using  "$input_data_cd\directed_trips_calibration_MA", clear 
gen wave = 1 if inlist(month, 1, 2)
replace wave=2 if inlist(month, 3, 4)
replace wave=3 if inlist(month, 5, 6)
replace wave=4 if inlist(month, 7, 8)
replace wave=5 if inlist(month, 9, 10)
replace wave=6 if inlist(month, 11, 12)
drop month1
tostring month, gen(month1)
tostring wave, gen(wave1)

drop if dtrip==0

keep day_i date  mode month month1 dtrip wave wave1 draw

gen domain1=mode+"_"+date
gen domain=mode+"_"+wave1

encode domain1, gen(domain3)

tempfile trips
save `trips', replace

qui forv i=1/$ndraws{

u `trips', clear  
keep if draw==`i'
keep if draw==1

tempfile trips2
save `trips2', replace 

global domainz

levelsof domain3, local(doms) 
foreach d of local doms{
	u `trips2', clear 
	
	keep if domain3==320 
	levelsof mode, local(md) clean
	levelsof month, local(mnth)
	levelsof day_i, local(day_i1)
	levelsof day, local(day1) clean
	levelsof domain, local(dom) clean
	levelsof wave, local(wv) 
	levelsof dtrip, local(dtrip) 
	levelsof domain1, local(dom1) clean

	u "$iterative_input_data_cd\catch per trip1 MA.dta", clear 
	gen domain=mode+"_"+wv2
	order domain*

	keep if domain=="pr_5"
	
	expand wp_int
	count 
	if `r(N)'<1500{
		local expand = round(1500/`r(N)')
		sample 1500, count

	}
	else{
			sample 1500, count
	}
	
	egen tripid = seq(), f(1) t(50)
	bysort tripid: gen catch_draw=_n
	
	keep domain wv2 mode tripid catch_draw sf_catch sf_keep sf_rel bsb_catch bsb_keep bsb_rel scup_catch scup_keep scup_rel


	preserve
	u "$iterative_input_data_cd\angler_dems.dta", clear 
	keep if wave==3
	sample 50, count
	gen tripid=_n
	tempfile dems2
	save `dems2', replace	
	restore 
	
	merge m:1 tripid using `dems2', keep(3) nogen

	preserve 
	u "$iterative_input_data_cd\trip_costs.dta", clear
	keep if state=="MA" & mode=="pr"
	sample 50, count
	gen tripid=_n
	tempfile costs
	save `costs', replace	
	restore 
	
	merge m:1 tripid using `costs', keep(3) nogen

	
	tempfile domainz`d'
	save `domainz`d'', replace
	global domainz "$domainz "`domainz`d''" " 

}


clear
dsconcat $domainz

order domain1 domain wave wave month mode day day_i dtrip draw tripid catch_draw cod_keep cod_rel cod_catch hadd_keep hadd_rel hadd_catch 
drop domain3 domain1 domain
sort day_i mode tripid catch_draw
export delimited using "$iterative_input_data_cd\catch_draws`i'_full.csv", replace 

}
















keep if my_dom_id_string=="DE_3_sh_SF"
save "$iterative_input_data_cd\test_compula2.dta", replace 

preserve
keep my_dom_id my_dom_id_string
duplicates drop 
tempfile domains
save `domains', replace 
restore

* Create a postfile to collect results
tempfile results
postfile handle str15 varname str15 domain float mean se ll95 ul95 using `results', replace

* Loop over variables
foreach var in sf_keep sf_rel bsb_keep bsb_rel scup_keep scup_rel {

    * Run svy mean for the variable by domain
    svy: mean `var', over(my_dom_id)

    * Grab result matrix and domain labels
    matrix M = r(table)
    local colnames : colnames M

    * Loop over columns (domains)
    foreach col of local colnames {
        local m  = M[1, "`col'"]
        local se = M[2, "`col'"]
        local lb = M[5, "`col'"]
        local ub = M[6, "`col'"]

        post handle ("`var'") ("`col'") (`m') (`se') (`lb') (`ub')
    }
}

postclose handle

* Load results back into memory
use `results', clear

split domain, parse("@")
drop domain1
split domain2, parse(.)
drop domain2 domain22
replace domain21="1" if domain21=="1bn"
destring domain21, replace
rename domain21 my_dom_id
merge m:1 my_dom_id using `domains' 
sort varname  my_dom_id
keep varname mean se my_dom_id_string
drop if mean==0
gen pse=se/mean
keep if se==.









mat b = e(b)'
mat se = e(se)'
mat rownames = e(Row)'
mat list  rownames

clear 
svmat rownames
svmat b
svmat se
		
drop if rownames==.
rename rownames nfish
rename b1 tot_ntrips
rename se se_ntrips

keep if my_dom_id_string=="MA_5_pr_SF"
save "$iterative_input_data_cd\test_compula.dta", replace 

tempfile base1
save `base1', replace 

	
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



*Estimate catch/harvest/discards from rounds 2-5 in separate .do file and save these estimates as datasets to use if needed. 
*Prior to estimating catch per trip by state, wave, mode, I will estimate it by region (MA-NY, NJ-NC). 
*I will retain the PSE and use it to fill in missing PSEs when we estimate catch per trip by state, wave, mode



*Loop over domains to get estimated numbers of trips that caught, harvested, and released X fish and se's
u `base1', replace 

global catch 
levelsof my_dom_id_string, local(sts)
foreach s of local sts{

di "`s'"
qui{
u `new', clear 

**SF catch
preserve
estpost svy: tab sf_catch my_dom_id_string if my_dom_id_string=="`s'", count ci se format(%11.3g)

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
estpost svy: tab bsb_catch my_dom_id_string if my_dom_id_string=="`s'", count ci se format(%11.3g)
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
estpost svy: tab scup_catch my_dom_id_string if my_dom_id_string=="`s'", count ci se format(%11.3g)
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
estpost svy: tab sf_keep my_dom_id_string if my_dom_id_string=="`s'", count ci se format(%11.3g)

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
estpost svy: tab bsb_keep my_dom_id_string if my_dom_id_string=="`s'", count ci se format(%11.3g)
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
estpost svy: tab scup_keep my_dom_id_string if my_dom_id_string=="`s'", count ci se format(%11.3g)
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
estpost svy: tab sf_rel my_dom_id_string if my_dom_id_string=="`s'", count ci se format(%11.3g)

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
estpost svy: tab bsb_rel my_dom_id_string if my_dom_id_string=="`s'", count ci se format(%11.3g)
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
estpost svy: tab scup_rel my_dom_id_string if my_dom_id_string=="`s'", count ci se format(%11.3g)
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
save "$iterative_input_data_cd\catch per trip1.dta", replace 


// Deal with missing standard errors
// The strategy to impute missing standard error is to: 
	// 1) Take the average PSU across n_fish bins shouldering the missing SE, and apply that average to the missing SE 
	// 2) If there is only one catch bin shouldering the missing SE, use the PSE from that one 
	// 3) If a SE is still missing, merge to the aggregate catch-per-trip dataset above and use that SE
	// 4) If a SE is still missing, repeat steps 1) to 3) because now some missing SEs have been filled in by the aggregate catch-per-trip dataset
	// 5)  If a SE is still missing, replace the SE equal to the point estimate
	
sort domain sp disp nf
count if se==0
gen domain2=domain+"_"+species+"_"+disp

replace se=. if se==0
gen pse=se/tot
gen nfish_next=nfish+1
gen nfish_prev=nfish-1
gen pse_impute=.

gen nfish2=nfish
tostring nfish2, replace
gen nfish_domain=nfish2+"_"+ domain2

// Steps 1) and 2) 
levelsof nfish_domain if se==. , local(missings)
	foreach m of local missings{
		di "`m'"
	
		levelsof nfish_next if nfish_domain=="`m'"
		local nfish_next=`r(levels)'
		di `nfish_next'
		
		levelsof nfish_prev if nfish_domain=="`m'"
		local nfish_prev=`r(levels)'
		di `nfish_prev'

		levelsof domain2 if nfish_domain=="`m'", clean
		local domain2="`r(levels)'"
		di "`domain2'"

		
		su pse if domain2=="`domain2'" & (nfish==`nfish_next' | nfish==`nfish_prev') 
		return list
		
		if `r(N)'==2{
		replace pse_impute=`r(mean)'  if nfish_domain=="`m'"
		}
		
		else{
		su pse if domain2=="`domain2'" & (nfish==`nfish_next' ) 
		return list
		
		if `r(N)'==1{
		replace pse_impute=`r(mean)'  if nfish_domain=="`m'"
		}
		
		else{
		su pse if domain2=="`domain2'" & (nfish==`nfish_prev' ) 
		return list
		
		if `r(N)'==1{
		replace pse_impute=`r(mean)'  if nfish_domain=="`m'"
		}
		
		else{
		
		}
		
		}
		}
		}	
	
sort domain2 nfish
drop domain2 nfish_next nfish_prev nfish2 nfish_domain
split domain, parse(_)
rename domain1 state
rename domain2 wave
rename domain3 mode
drop  domain4 
replace se_ntrips=tot_ntrips*pse_impute if se_ntrips==. & pse_impute!=.	

// Step 3)	
merge 1:1 disp species state wave mode nfish using "$iterative_input_data_cd\catch per trip aggregate.dta", keep(1 3)
gen pse_agg=se_ntrips_agg/tot_ntrips_agg
replace pse_impute=pse_agg if pse_impute==. & pse_agg!=0
sort domain  species disp nf
replace se_ntrips=tot_ntrips*pse_impute if se_ntrips==. & pse_impute!=.
count if se_ntrips==.

// Step 4)
replace se_ntrips=tot_ntrips if se_ntrips==.


drop pse pse_impute tot_ntrips_agg se_ntrips_agg _merge pse_agg 	
replace tot_n=round(tot_n)
replace se=round(se)


replace domain=state+"_"+species+"_"+disp+"_"+wave+"_"+mode
order state species disp wave mode domain nfish tot se

keep if state=="MA"
*With the point estimates and standard errors, create 150 probability distributions of harvest, relased, total catch 
tempfile new
save `new', replace 

global x

levelsof domain, local(doms)
foreach d of local doms{
	
u `new', clear 
keep if domain=="`d'"
tempfile new1
save `new1', replace 

levelsof nfish, local(levs)
foreach c of local levs{
	
u `new1', clear 

keep if nfish==`c'

su tot_ntrip
local mean=`r(mean)'

su se
local se=`r(mean)'

clear
set obs $ndraws
gen nfish=`c'

gen draw=_n

gen tot_ntrip_not_trunc=rnormal(`mean', `se')
gen tot_ntrip=max(0, tot_ntrip_not_trunc)

gen domain="`d'"
tempfile x`c'`d'
save `x`c'`d'', replace
global x "$x "`x`c'`d''" " 


}	
}
clear
dsconcat $x

egen group=group(domain nfish)
sort domain draw nfish


*The following code snippet correct for bias that occurs when drawing from uncertain MRIP estimates. 
*When an MRIP estimate is very uncertain, some draws of x from the distribution of X can result in x_i<0. 
*Because trip outcomes cannot be negative I change these values to 0. But doing so results in an upward 
*shift of the mean value across draws. To correct for this, I first sum the x_i's across draws where x_i<0. 
*Then I add some of this (negative) value to each of the other x_i's (i.e., to x_i's >0) in proportion to each 
*x_i>0's contribution to the total value of X. 

*I have tried paramaterizing non-negative distributions using the MRIP point estimate and SE, but what I have tried has resulted in larger shifts in the mean compared to the correction below

gen tab=1 if tot_ntrip_not_trunc<0
egen sum_neg=sum(tot_ntrip_not_trunc) if tab==1, by(group)
sort group
egen mean_sum_neg=mean(sum_neg), by(group)

egen sum_non_neg=sum(tot_ntrip_not_trunc) if tot_ntrip_not_trunc>0 , by(group)
gen prop=tot_ntrip_not_trunc/sum_non_neg
gen adjust=prop*mean_sum_neg

gen tot_ntrip2=tot_ntrip+adjust if tot_ntrip!=0 & adjust !=.
replace tot_ntrip2=tot_ntrip if tot_ntrip2==.
replace tot_ntrip2=0 if tot_ntrip2<0
 
 *checks
 /*
split domain, parse(_)
gen nfish0= tot_ntrip*nfish 
gen nfish1= tot_ntrip_not*nfish  
gen nfish2= tot_ntrip2*nfish 

su nfish2 
return list
local new = `r(sum)'

su nfish1 
return list
local not_truc = `r(sum)'
di `new'-`not_truc'
di ((`new'-`not_truc')/`not_truc')*100

su tot_ntrip2 
return list
local new = `r(sum)'

su tot_ntrip_not 
return list
local not_truc = `r(sum)'
di `new'-`not_truc'
di ((`new'-`not_truc')/`not_truc')*100
*/
*end checks 

drop group tab sum_neg sum_non_neg mean_sum_neg  prop  adjust 
drop tot_ntrip_not_trunc tot_ntrip
rename tot_ntrip2 tot_ntrip
sort domain draw nfish  

*When the # of trips that catch zero fish==0, it creates problems. Replace these instances with tot_ntrips=1
replace tot_ntrip=1 if nfish==0 & tot_ntrip==0 
*browse 

reshape wide tot_ntrip, i(nfish domain) j(draw)

encode domain, gen(domain2)
xtset domain2 nfish
tsfill, full

mvencode tot*, mv(0) override
decode domain2, gen(domain3)
drop domain2 domain
rename domain3 domain

order domain
sort domain nfish 
split domain, parse(_)

rename domain1 state
rename domain2 species
rename domain3 disp
rename domain4 wave
rename domain5 mode

order domain state species disp wave mode

gen domain2=wave+"_"+mode

order domain* wave mode species disp

tempfile new
save `new', replace 

global domainz

levelsof domain2, local(doms) 
foreach d of local doms{
u `new', clear 

keep if domain2=="2_sh"

*keep if domain2=="`d'"

keep  domain* wave mode species disp nfish tot_ntrip*

drop if disp=="catch"


preserve
keep if disp=="keep"
keep if species=="sf"
renvarlab tot*, postfix(_keep_sf)
drop disp
drop species
tempfile keepsf
save `keepsf', replace 
restore

preserve
keep if disp=="keep"
keep if species=="bsb"
renvarlab tot*, postfix(_keep_bsb)
drop disp
drop species
tempfile keepbsb
save `keepbsb', replace 
restore

preserve
keep if disp=="keep"
keep if species=="scup"
renvarlab tot*, postfix(_keep_scup)
drop disp
drop species
tempfile keepscup
save `keepscup', replace 
restore

preserve
keep if disp=="rel"
keep if species=="sf"
renvarlab tot*, postfix(_rel_sf)
drop disp
drop species
tempfile relsf
save `relsf', replace 
restore

preserve
keep if disp=="rel"
keep if species=="bsb"
renvarlab tot*, postfix(_rel_bsb)
drop disp
drop species
tempfile relbsb
save `relbsb', replace 
restore

preserve
keep if disp=="rel"
keep if species=="scup"
renvarlab tot*, postfix(_rel_scup)
drop disp
drop species
tempfile relscup
save `relscup', replace 
restore

u `relscup', clear
merge 1:1 nfish wave mode   using `keepscup', keep(3) nogen
merge 1:1 nfish wave mode   using `relsf', keep(3) nogen
merge 1:1 nfish wave mode   using `keepsf', keep(3) nogen
merge 1:1 nfish wave mode   using `relbsb', keep(3) nogen
merge 1:1 nfish wave mode   using `keepbsb', keep(3) nogen

*We need to make sure that for each draw, the total number of trips 
*in the keep distribution equals the total number of trips in the release distirbution

forv i =1/$ndraws{


*compute the sum of trips in the keep/release distributions by draw 
egen sum_trips_keep`i'_sf=sum(tot_ntrip`i'_keep_sf)
egen sum_trips_rel`i'_sf=sum(tot_ntrip`i'_rel_sf)

egen sum_trips_keep`i'_bsb=sum(tot_ntrip`i'_keep_bsb)
egen sum_trips_rel`i'_bsb=sum(tot_ntrip`i'_rel_bsb)

egen sum_trips_keep`i'_scup=sum(tot_ntrip`i'_keep_scup)
egen sum_trips_rel`i'_scup=sum(tot_ntrip`i'_rel_scup)


*compute proportions of trips that kept/released c fish in each draw
gen prop_ntrips_keep`i'_sf=tot_ntrip`i'_keep_sf/sum_trips_keep`i'_sf
gen prop_ntrips_rel`i'_sf=tot_ntrip`i'_rel_sf/sum_trips_rel`i'_sf

gen prop_ntrips_keep`i'_bsb=tot_ntrip`i'_keep_bsb/sum_trips_keep`i'_bsb
gen prop_ntrips_rel`i'_bsb=tot_ntrip`i'_rel_bsb/sum_trips_rel`i'_bsb

gen prop_ntrips_keep`i'_scup=tot_ntrip`i'_keep_scup/sum_trips_keep`i'_scup
gen prop_ntrips_rel`i'_scup=tot_ntrip`i'_rel_scup/sum_trips_rel`i'_scup

*compute the number of trips that kept/released c fish in each draw. 
*use the sf rel distribution as the baseline 
gen base_trips`i'=sum_trips_rel`i'_sf

*multiply the proportion of trips that released cod, kept haddock, and released haddock
*by the total number of trips in the hadd rel distribution

gen tot_ntrip`i'_keep_new_sf=prop_ntrips_keep`i'_sf*base_trips`i'
gen tot_ntrip`i'_rel_new_sf=prop_ntrips_rel`i'_sf*base_trips`i'

gen tot_ntrip`i'_keep_new_bsb=prop_ntrips_keep`i'_bsb*base_trips`i'
gen tot_ntrip`i'_rel_new_bsb=prop_ntrips_rel`i'_bsb*base_trips`i'

gen tot_ntrip`i'_keep_new_scup=prop_ntrips_keep`i'_scup*base_trips`i'
gen tot_ntrip`i'_rel_new_scup=prop_ntrips_rel`i'_scup*base_trips`i'


 drop  sum_trips_keep`i'_sf sum_trips_rel`i'_sf  sum_trips_keep`i'_bsb sum_trips_rel`i'_bsb   sum_trips_keep`i'_scup sum_trips_rel`i'_scup ///
		  prop_ntrips_rel`i'_sf prop_ntrips_keep`i'_sf  prop_ntrips_rel`i'_bsb prop_ntrips_keep`i'_bsb   prop_ntrips_rel`i'_scup prop_ntrips_keep`i'_scup    ///
		  tot_ntrip`i'_rel_sf tot_ntrip`i'_keep_sf	  tot_ntrip`i'_rel_bsb tot_ntrip`i'_keep_bsb	   tot_ntrip`i'_rel_scup tot_ntrip`i'_keep_scup ///	
		   base_trips`i'

}


*drop domain*

preserve
drop *rel_new* *keep_new_bsb *keep_new_scup
renvarlab tot_ntrip*, postdrop(12) 
gen disp="keep"
tempfile keepsf
save `keepsf', replace
restore

preserve
drop *rel_new* *keep_new_sf *keep_new_scup
renvarlab tot_ntrip*, postdrop(13) 
gen disp="keep"
tempfile keepbsb
save `keepbsb', replace
restore

preserve
drop *rel_new* *keep_new_sf *keep_new_bsb
renvarlab tot_ntrip*, postdrop(14) 
gen disp="keep"
tempfile keepscup
save `keepscup', replace
restore


preserve
drop *keep_new* *rel_new_bsb *rel_new_scup
renvarlab tot_ntrip*, postdrop(11) 
gen disp="rel"
tempfile relsf
save `relsf', replace
restore

preserve
drop *keep_new* *rel_new_sf *rel_new_scup
renvarlab tot_ntrip*, postdrop(12) 
gen disp="rel"
tempfile relbsb
save `relbsb', replace
restore

drop *keep_new* *rel_new_sf *rel_new_bsb
renvarlab tot_ntrip*, postdrop(13) 
gen disp="rel"


append using `keepsf'
append using `keepbsb'
append using `keepscup'
append using `relsf'
append using `relbsb'


gen domain = wave+"_"+mode+"_"+species+"_"+disp
order domain wave mode species disp nfish

tempfile domainz`d'
save `domainz`d'', replace
global domainz "$domainz "`domainz`d''" " 

}
clear 
dsconcat $domainz
mvencode tot_ntrip*, mv(0) over
sort domain nfish
duplicates drop

expand 2
bysort domain nfish : gen n=_n
gen month=1 if wave=="1" & n==1
replace month=2 if wave=="1" & n==2
replace month=3 if wave=="2" & n==1
replace month=4 if wave=="2" & n==2
replace month=5 if wave=="3" & n==1
replace month=6 if wave=="3" & n==2
replace month=7 if wave=="4" & n==1
replace month=8 if wave=="4" & n==2
replace month=9 if wave=="5" & n==1
replace month=10 if wave=="5" & n==2
replace month=11 if wave=="6" & n==1
replace month=12 if wave=="6" & n==2

save "$iterative_input_data_cd\catch per trip1 MA.dta", replace 


/*
*check how the resulting catch totals match up to mrip estimates
u "$iterative_data_cd\catch per trip1.dta", clear 

forv i =1/150{
gen nfish2`i'=tot_ntrip`i'*nfish
egen sum_nfish2`i'=sum(nfish2`i'), by(species disp mode  )
}

collapse (mean) sum_nfish2*, by(species disp mode )
reshape long sum_nfish2, i(species disp mode ) j(new)
collapse (mean) sum_nfish2*, by(species disp mode )
tempfile sim 
save `sim', replace 

import delimited using "$input_data_cd\MRIP_catch_totals_open_season.csv", clear 
drop ll* ul* 
ds mode season, not
renvarlab `r(varlist)', prefix(mrip_)
reshape long mrip_, i(mode season) j(new) string
split new, parse(_)
rename new1 species
rename new2 disp
drop new3 new
collapse (sum) mrip_, by(species disp mode)
drop if disp=="catch"
merge 1:1 mode disp species using `sim'
gen diff=sum-mrip
gen pdiff=((sum-mrip)/mrip)*100

collapse (sum) mrip_ sum, by(species  mode)
gen diff=sum-mrip
gen pdiff=((sum-mrip)/mrip)*100

*/

*Now create a file for each draw that contains:
	*a) 50 trips per day of the fishing season, each with 30 draws of catch-per-trip
	*b) demographics for each trip that are constant across catch draws
	
*Add age and avidities here
// ages
local s "MA"
import excel using "$input_data_cd\population ages.xlsx", clear firstrow
expand 4 if region=="MENY"
bysort region age: gen n=_n
gen state="MA" if region=="MENY" & n==1
replace state="RI" if region=="MENY" & n==2
replace state="CT" if region=="MENY" & n==3
replace state="NY" if region=="MENY" & n==4

replace state="NJ" if region=="NJ"

drop n
expand 2 if region=="DEMD"
bysort region age: gen n=_n
replace state="DE" if region=="DEMD" & n==1
replace state="MD" if region=="DEMD" & n==2

drop n
expand 2 if region=="VANC"
bysort region age: gen n=_n
replace state="VA" if region=="VANC" & n==1
replace state="NC" if region=="VANC" & n==2

keep if state=="`s'"
replace wtd_fre=round(wtd_fre)
expand wtd_fre
keep age
tempfile ages 
save `ages', replace 


// avidities (# trips past 12 months)
import excel using "$input_data_cd\population avidity.xlsx", clear firstrow

expand 4 if region=="MENY"
bysort region days_fished: gen n=_n
gen state="MA" if region=="MENY" & n==1
replace state="RI" if region=="MENY" & n==2
replace state="CT" if region=="MENY" & n==3
replace state="NY" if region=="MENY" & n==4

replace state="NJ" if region=="NJ"

drop n
expand 2 if region=="DEMD"
bysort region days_fished: gen n=_n
replace state="DE" if region=="DEMD" & n==1
replace state="MD" if region=="DEMD" & n==2

drop n
expand 2 if region=="VANC"
bysort region days_fished: gen n=_n
replace state="VA" if region=="VANC" & n==1
replace state="NC" if region=="VANC" & n==2

keep if state=="`s'"
replace wtd_fre=round(wtd_fre)
expand wtd_fre
keep days_fished
tempfile avidities 
save `avidities', replace 



import delimited using  "$input_data_cd\directed_trips_calibration_MA", clear 
gen wave = 1 if inlist(month, 1, 2)
replace wave=2 if inlist(month, 3, 4)
replace wave=3 if inlist(month, 5, 6)
replace wave=4 if inlist(month, 7, 8)
replace wave=5 if inlist(month, 9, 10)
replace wave=6 if inlist(month, 11, 12)
drop month1
tostring month, gen(month1)
tostring wave, gen(wave1)

drop if dtrip==0

keep day_i date  mode month month1 dtrip wave wave1 draw

gen domain1=mode+"_"+date
gen domain=mode+"_"+month1

encode domain1, gen(domain3)

tempfile trips
save `trips', replace

qui forv i=1/$ndraws{

u `trips', clear  
keep if draw==`i'
keep if draw==1

tempfile trips2
save `trips2', replace 

global domainz

levelsof domain3, local(doms) 
foreach d of local doms{
	u `trips2', clear 
	keep if domain3==320 
	levelsof mode, local(md) clean
	levelsof month, local(mnth)
	levelsof day_i, local(day_i1)
	levelsof day, local(day1) clean
	levelsof domain, local(dom) clean
	levelsof wave, local(wv) 
	levelsof dtrip, local(dtrip) 
	levelsof domain1, local(dom1) clean

	u "$iterative_input_data_cd\catch per trip1 MA.dta", clear 
	drop if disp=="catch"
	drop domain
	destring month, replace
	tostring month, gen(month1)
	drop month 
	rename month1 month 
	gen domain=mode+"_"+month
	gen domain2=mode+"_"+month+"_"+species
	order domain*

	keep if domain=="`dom'"

	tempfile base`d' 
	save `base`d'', replace 
	
	**keep draws sf
	u `base`d'', clear 
	keep if species=="sf"
	local i=1
	ds domain domain2  month mode species disp nfish
	keep `r(varlist)' tot_ntrip`i'

	keep if disp=="keep"

	su tot_ntrip`i'
	gen double prop=tot_ntrip`i'/r(sum)

	keep if prop!=0
	gen ntrip=2000
	replace ntrip=round(ntrip*prop)
	expand ntrip
	
	sample 1500, count
	renvarlab nfish, postfix("sf")
	renvarlab nfish, postfix("keep")
	egen tripid = seq(), f(1) t(50)
	bysort tripid: gen catch_draw=_n
	
	keep month mode nfish tripid catch_draw 
	
	tempfile keep
	save `keep', replace

	
	*release draws cod
	u `base`d'', clear 

	keep if species=="cod"
	
	ds domain domain2  month mode species disp nfish
	keep `r(varlist)' tot_ntrip`i'

	keep if disp=="rel"

	su tot_ntrip`i'
	gen double prop=tot_ntrip`i'/r(sum)

	keep if prop!=0
	gen ntrip=2000
	replace ntrip=round(ntrip*prop)
	expand ntrip
	
	sample 1500, count
	renvarlab nfish, postfix("cod")
	renvarlab nfish, postfix("rel")
	egen tripid = seq(), f(1) t(50)
	bysort tripid: gen catch_draw=_n
	
	keep month mode nfish tripid catch_draw 
	
	merge 1:1 tripid catch_draw using `keep', nogen
	
	tempfile cod
	save `cod', replace
	
	
	**keep draws hadd
	u `base`d'', clear 
	keep if species=="hadd"
	
	ds domain domain2  month mode species disp nfish
	keep `r(varlist)' tot_ntrip`i'

	keep if disp=="keep"
	
	su tot_ntrip`i'
	gen double prop=tot_ntrip`i'/r(sum)

	keep if prop!=0
	gen ntrip=2000
	replace ntrip=round(ntrip*prop)
	expand ntrip
	
	sample 1500, count
	renvarlab nfish, postfix("hadd")
	renvarlab nfish, postfix("keep")
	egen tripid = seq(), f(1) t(50)
	bysort tripid: gen catch_draw=_n
	
	keep month mode nfish tripid catch_draw 
	
	tempfile keep
	save `keep', replace


	*release draws hadd
	u `base`d'', clear 

	keep if species=="hadd"
	
	ds domain domain2  month mode species disp nfish
	keep `r(varlist)' tot_ntrip`i'

	keep if disp=="rel"
	
	su tot_ntrip`i'
	
	gen double prop=tot_ntrip`i'/r(sum)

	keep if prop!=0
	gen ntrip=2000
	replace ntrip=round(ntrip*prop)
	expand ntrip
	
	sample 1500, count
	renvarlab nfish, postfix("hadd")
	renvarlab nfish, postfix("rel")
	egen tripid = seq(), f(1) t(50)
	bysort tripid: gen catch_draw=_n
	
	keep month mode nfish tripid catch_draw 
	
	merge 1:1 tripid catch_draw using `keep', nogen
	
		
	***Merge to the other species 
	merge 1:1 tripid catch_draw using  `cod', nogen
	gen domain3=`d'
	gen domain="`dom'"
	gen day_i=`day_i1'
	gen day ="`day1'"
	gen wave=`wv'	
	gen draw = `i'
	gen dtrip=`dtrip'
	gen domain1="`dom1'"

	gen cod_catch=nfishcodrel+nfishcodkeep
	gen hadd_catch=nfishhaddrel+nfishhaddkeep

	rename nfishcodrel cod_rel
	rename nfishcodkeep cod_keep
	
	rename nfishhaddrel hadd_rel
	rename nfishhaddkeep hadd_keep

	preserve
	u `ages', clear 
	sample 50, count
	gen tripid=_n
	tempfile ages2
	save `ages2', replace	
	restore 
	
	preserve
	u `avidities', clear 
	sample 50, count
	gen tripid=_n
	tempfile avidities2
	save `avidities2', replace	
	restore 
	
	merge m:1 tripid using `ages2', keep(3) nogen
	merge m:1 tripid using `avidities2', keep(3) nogen

	gen cost=rnormal($fh_cost_est, $fh_cost_sd) if mode=="fh" & catch_draw==1
	replace cost=rnormal($pr_cost_est, $pr_cost_sd) if mode=="pr" & catch_draw==1
	replace cost=rnormal($sh_cost_est, $sh_cost_sd) if mode=="sh" & catch_draw==1

	egen mean_cost=mean(cost), by(tripid)
	
	drop cost
	rename mean_cost cost 
	
	
	tempfile domainz`d'
	save `domainz`d'', replace
	global domainz "$domainz "`domainz`d''" " 

}


clear
dsconcat $domainz

order domain1 domain wave wave month mode day day_i dtrip draw tripid catch_draw cod_keep cod_rel cod_catch hadd_keep hadd_rel hadd_catch 
drop domain3 domain1 domain
sort day_i mode tripid catch_draw
export delimited using "$iterative_input_data_cd\catch_draws`i'_full.csv", replace 

}


*Now for each of these files, make sure that there are catch draws for periods with directed trips but no catch 

global drawz
  forv i=1/$ndraws{

import excel using "$input_data_cd\population ages.xlsx", clear firstrow
keep if region=="MENY"
replace wtd_fre=round(wtd_fre)
expand wtd_fre
keep age
tempfile ages 
save `ages', replace 

import excel using "$input_data_cd\population avidity.xlsx", clear firstrow
keep if region=="MENY"
replace wtd_fre=round(wtd_fre)
expand wtd_fre
keep days_fished
tempfile avidities 

save `avidities', replace 
preserve
u `ages', clear 
sample 50, count
gen tripid=_n
tempfile ages2
save `ages2', replace	
restore 
	
preserve
u `avidities', clear 
sample 50, count
gen tripid=_n
tempfile avidities2
save `avidities2', replace	
restore 

import delimited using  "$input_data_cd\directed_trips_calib_150draws_cm.csv", clear 
keep if draw==`i'
keep day day_i mode dtrip 
duplicates drop 
tempfile draws
save `draws', replace 


import delimited using "$iterative_input_data_cd\catch_draws`i'_full.csv", clear 
tempfile basedraws
save `basedraws', replace 

rename dtrip dtrip_catchfile
keep day day_i mode dtrip 
duplicates drop 

merge 1:1 day day_i mode using `draws'

keep if _merge==2
gen domain=mode+"_"+day
sort  mode day

tempfile base2
save `base2', replace 

global domainz
levelsof domain, local(doms)
foreach d of local doms{

u `base2', clear 

keep if domain=="`d'"
expand 1500
egen tripid = seq(), f(1) t(50)
bysort tripid: gen catch_draw=_n

merge m:1 tripid using `ages2', keep(3) nogen
merge m:1 tripid using `avidities2', keep(3) nogen

gen cost=rnormal($fh_cost_est, $fh_cost_sd) if mode=="fh" & catch_draw==1
replace cost=rnormal($pr_cost_est, $pr_cost_sd) if mode=="pr" & catch_draw==1
replace cost=rnormal($sh_cost_est, $sh_cost_sd) if mode=="sh" & catch_draw==1

egen mean_cost=mean(cost), by(tripid)
	
drop cost
rename mean_cost cost 
gen draw=`i'
tempfile domainz`d'
save `domainz`d'', replace
global domainz "$domainz "`domainz`d''" " 
}

dsconcat $domainz
append using  `basedraws'
mvencode cod_keep cod_rel cod_catch hadd_keep hadd_rel hadd_catch, mv(0) override
drop _merge month wave domain dtrip_catchfile dtrip

gen day1 = substr(day, 1, 2)
gen month1= substr(day, 3, 3)
gen month="1" if month1=="jan"
replace month="2" if month1=="feb"
replace month="3" if  month1=="mar"
replace month="4" if   month1=="apr"
replace month="5" if  month1=="may"
replace month="6" if  month1=="jun"
replace month="7" if  month1=="jul"
replace month="8" if  month1=="aug"
replace month="9" if  month1=="sep"
replace month="10" if  month1=="oct"
replace month="11" if  month1=="nov"
replace month="12" if  month1=="dec"
gen period2 = month+ "_"+ day1+ "_"+ mode
destring month, replace 
destring day1, replace 


export delimited using "$iterative_input_data_cd\catch_draws`i'_full.csv", replace 

 }
 

