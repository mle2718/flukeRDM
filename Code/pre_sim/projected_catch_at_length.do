*******************************************************
* User controls
*******************************************************
global n_frac_draws 100
set seed 12345

*******************************************************
* A) Catch-at-length: keep catch draw separate
*******************************************************
use "$misc_data_cd/baseline_catch_at_length_region.dta", clear  
keep length draw species region catch
gen domain = region + "_" + species
drop if catch==0
collapse (sum) catch, by(length species region draw domain)


tempfile cal
save `cal', replace 

*******************************************************
* B1) Population numbers-at-age: create pop_draw
*******************************************************
import delimited using "$misc_data_cd/J1_2024Summer_Flounder.csv", clear
gen species = "sf"
gen region  = "CST"
duplicates drop 
drop year
sample $ndraws, count
gen draw = _n
tempfile sf
save `sf', replace 

import delimited using "$misc_data_cd/J1_2024Scup.csv", clear
gen species = "scup"
gen region  = "CST"
duplicates drop 
drop year
sample $ndraws, count
gen draw = _n
tempfile scup
save `scup', replace 

import delimited using "$misc_data_cd/fit_NAA_NORTH_2024.csv", clear
gen species = "bsb"
gen region  = "NO"
duplicates drop 
sample $ndraws, count
replace draw = _n
forv i = 1/8 {
    rename v`i' a`i'    
}

tempfile bsbN
save `bsbN', replace 

import delimited using "$misc_data_cd/fit_NAA_SOUTH_2024.csv", clear
gen species = "bsb"
gen region  = "SO"
duplicates drop 
sample $ndraws, count
replace draw = _n
forv i = 1/8 {
    rename v`i' a`i'    
}
tempfile bsbS
save `bsbS', replace 

append using `bsbN'
append using `scup'
append using `sf'

reshape long a, i(species region draw) j(age) string
destring age, replace
rename a pop_naa
replace pop_naa = pop_naa * 1000


*******************************************************
* B2) Expand population regions
*******************************************************
expand 2 if species == "bsb" & region == "SO"
bysort draw species region age: gen n = _n 
gen region2 = "NJ" if species == "bsb" & region == "SO" & n == 1
replace region2 = "SO" if species == "bsb" & region == "SO" & n == 2
drop n

replace region2 = "NO" if species == "bsb" & region == "NO"

expand 3 if species == "sf"
bysort draw species region age: gen n = _n 
replace region2 = "NJ" if species == "sf" & n == 1
replace region2 = "SO" if species == "sf" & n == 2
replace region2 = "NO" if species == "sf" & n == 3
drop n

replace region2 = "CST" if species == "scup"

drop region 
rename region2 region

tempfile pop_naa_calibration
save `pop_naa_calibration', replace



*******************************************************
* B1.2) Population numbers-at-age: projection
*******************************************************
import delimited using "$misc_data_cd/J1_2026Summer_Flounder.csv", clear
gen species = "sf"
gen region  = "CST"
duplicates drop 
drop year
sample $ndraws, count
gen draw = _n
tempfile sf
save `sf', replace 

import delimited using "$misc_data_cd/J1_2026Scup.csv", clear
gen species = "scup"
gen region  = "CST"
duplicates drop 
drop year
sample $ndraws, count
gen draw = _n
tempfile scup
save `scup', replace 

import delimited using "$misc_data_cd/fit_proj_NAA_NORTH_2026.csv", clear
gen species = "bsb"
gen region  = "NO"
duplicates drop 
sample $ndraws, count
replace draw = _n
forv i = 1/8 {
    rename v`i' a`i'
	replace a`i'=a`i'*1000

}

tempfile bsbN
save `bsbN', replace 

import delimited using "$misc_data_cd/fit_proj_NAA_SOUTH_2026.csv", clear
gen species = "bsb"
gen region  = "SO"
duplicates drop 
sample $ndraws, count
replace draw = _n
forv i = 1/8 {
    rename v`i' a`i'
	replace a`i'=a`i'*1000

}
tempfile bsbS
save `bsbS', replace 

append using `bsbN'
append using `scup'
append using `sf'

reshape long a, i(species region draw) j(age) string
destring age, replace
rename a pop_naa
replace pop_naa = pop_naa 


*******************************************************
* B2.2) Expand population regions
*******************************************************
expand 2 if species == "bsb" & region == "SO"
bysort draw species region age: gen n = _n 
gen region2 = "NJ" if species == "bsb" & region == "SO" & n == 1
replace region2 = "SO" if species == "bsb" & region == "SO" & n == 2
drop n

replace region2 = "NO" if species == "bsb" & region == "NO"

expand 3 if species == "sf"
bysort draw species region age: gen n = _n 
replace region2 = "NJ" if species == "sf" & n == 1
replace region2 = "SO" if species == "sf" & n == 2
replace region2 = "NO" if species == "sf" & n == 3
drop n

replace region2 = "CST" if species == "scup"

drop region 
rename region2 region

tempfile pop_naa_projection
save `pop_naa_projection', replace


*C3
*C) Create age-length keys from NEFSC trawl survey data
	*1) Pull in NEFSC trawl survey data
	*2) Smooth counts across age classes over the range of observed catch-at-lengths for a given state-species using a LOWESS bandwidth=0.3
	*3) Compute the proportion of fish of age a that are length l
	*4) Expand to the number of draws used in the simulation 
	*5) Merge the age-length keys to the population numbers-at-age, compute population numbers-at-length

*C1) 
import delimited using "$misc_data_cd/NEFSC trawl survey data.csv", clear
tab stratum
gen str5 stratum2 = string(stratum, "%05.0f")
gen str2 stratum_group = substr(stratum2, 1, 2)   // first 2 characters
gen str3 stratum_number = substr(stratum2, 3, 3)   // last 3 characters


/*
Stratum group code: 
01 = Trawl, offshore north of Hatteras; 
02 = BIOM; 
03 = Trawl, inshore north of Hatteras; 
04 = Shrimp; 
05 = Scotian shelf; 
06 = Shellfish; 
07 = Trawl, inshore south of Hatteras; 
08 = Trawl, Offshore south of Hatteras; 
09 = MA DMF; 
99 = Offshore deepwater (outside the stratified area)
*/

keep if inlist(stratum_group, "01", "03", "09")

tostring cruise, gen(cruise2)
gen year=substr(cruise2, 1, 4)
destring year, replace

/*
svspp codes:
summer flounder =103
bsb =141
scup =143
*/

gen species="sf" if svspp==103 
replace species="bsb" if svspp==141 
replace species="scup" if svspp==143 

keep if $NEFSC_svy_yrs
replace age=7 if age>=7 & inlist(species, "sf", "scup")
replace age=8 if age>=8 & inlist(species, "bsb")

collapse (sum) countage, by(species age length)
rename count nfish

expand 3 if species=="sf"
bysort species length age: gen n=_n 
sort species length age
gen region="NO" if species=="sf" & n==1
replace region="SO" if species=="sf" & n==2
replace region="NJ" if species=="sf" & n==3
drop n

expand 3 if species=="bsb"
bysort species length age: gen n=_n 
sort species length age
replace region="NO" if species=="bsb" & n==1
replace region="SO" if species=="bsb" & n==2
replace region="NJ" if species=="bsb" & n==3
drop n

replace region="CST" if species=="scup"

sort species region age length 


*C2)  
gen domain=region+"_"+species
levelsof domain, local(domz)

tempfile base
save `base', replace

clear 
tempfile master
save `master', emptyok

foreach d of local domz{
		
	u `base', clear 
	keep if domain=="`d'"
	tsset age length
	tsfill, full
	mvencode nfish, mv(0) override 

	sort age length 
	replace domain="`d'" if domain==""
	

levelsof age, local(ages)
foreach a of local ages{
	
	lowess nfish length if age==`a' , adjust bwidth(.3) gen(s`a') nograph

	replace s`a'=0 if s`a'<=0
}

append using `master'
save `master', replace
clear                            
}
use `master', clear
split domain, parse(_)
replace region=domain1
replace species=domain2
drop domain1 domain2

egen smoothed_nfish=rowtotal(s0-s8)
drop s0-s8

*C3) generate smoothed and unsmoother proprtions at age
egen sum_smooth=sum(smoothed_nfish), by(age species region)	
gen prop_smoothed = smoothed_nfish / sum_smooth

egen sum_raw=sum(nfish), by(age species region)	
gen prop_raw=nfish/sum_raw	
drop sum*		


* C4) Expand age-length keys to population draws
expand $ndraws
bysort species domain age length: gen draw=_n

tempfile age_length
save `age_length', replace


* C5) Merge ALK to NAA and compute NaL - calibration year
u  `age_length', clear

merge m:1 species region age draw using `pop_naa_calibration', keep(match) nogen
sort draw species region age length   

gen nal_smooth = pop_naa * prop_smoothed
gen nal_raw    = pop_naa * prop_raw

collapse (sum) nal_smooth nal_raw, by(draw species region length)
drop if missing(length)

collapse (sum) nal_smooth nal_raw, by(draw species region length)

tempfile nal
save `nal', replace

* C5.2) Merge ALK to NAA and compute NaL - projection year
u  `age_length', clear

merge m:1 species region age draw using `pop_naa_projection', keep(match) nogen
sort draw species region age length   

gen nal_smooth = pop_naa * prop_smoothed
gen nal_raw    = pop_naa * prop_raw

collapse (sum) nal_smooth nal_raw, by(draw species region length)
drop if missing(length)

collapse (sum) nal_smooth nal_raw, by(draw species region length)

tempfile nal_proj
save `nal_proj', replace


*Merge catch data to population data and compute selectivity
use `cal', clear
merge m:1 species region draw length using `nal'

drop if catch==0
drop if draw==.

mvencode catch nal_smooth nal_raw, mv(0) override
sort species region  draw length

gen frac_caught_smooth = catch / nal_smooth if nal_smooth > 0
gen frac_caught_raw    = catch / nal_raw    if nal_raw > 0

gen flag_gt_pop_smooth = catch > nal_smooth & catch > 0
gen flag_gt_pop_raw    = catch > nal_raw    & catch > 0

sort species region draw length
drop if catch==0

egen min_length_pop=min(length) if nal_smooth!=0, by(species region draw)
egen max_length_pop=max(length) if nal_smooth!=0, by(species region draw)

egen min_length_catch=min(length) if catch!=0, by(species region draw)
egen max_length_catch=max(length) if catch!=0, by(species region draw)

local vars min_length_pop max_length_pop min_length_catch max_length_pop max_length_catch
foreach v of local vars{
	egen mean_`v'=mean(`v'), by(species region draw)
	replace `v'= mean_`v'
	drop mean_`v'
	
}

replace length=max_length_pop if catch>0 & nal_smooth==0 & length>max_length_pop
replace length=min_length_pop if catch>0 & nal_smooth==0 & length<min_length_pop

collapse (sum) catch nal*, by(species region  draw domain length )
drop if catch==0

gen frac_caught_smooth = catch / nal_smooth if nal_smooth > 0
gen frac_caught_raw    = catch / nal_raw    if nal_raw > 0

gen flag_gt_pop_smooth = catch > nal_smooth & catch > 0
gen flag_gt_pop_raw    = catch > nal_raw    & catch > 0

sort species region draw length
egen max_frac_caught_smooth = max(frac_caught_smooth), by(species region draw domain)
gen sel_scaled = frac_caught_smooth / max_frac_caught_smooth

tempfile selectivity
save `selectivity', replace

* Merge to projected NaL
use `nal_proj', clear
renvarlab nal*, postfix(_proj)
merge 1:1 length species region draw using `selectivity'
sort species region draw length
drop if _merge==1

gen catch_proj=nal_smooth_proj*frac_caught_smooth
keep length species region draw domain catch catch_proj
drop if domain==""

egen sum=sum(catch), by(species region draw domain)
gen observed_prob_base=catch/sum
egen sum_proj=sum(catch_proj), by(species region draw domain)
gen observed_prob_proj=catch_proj/sum_proj
format sum* %20.0gc
drop sum*

tostring draw, gen(draw2)
replace domain=region+"_"+species +"_"+draw2

	/*
twoway ///
    (line observed_prob_base length if region=="NJ" & species=="bsb" & draw==1, sort lcolor(navy) lpattern(solid))  ///
	(line observed_prob_proj length if region=="NJ" & species=="bsb" & draw==1, sort lcolor(maroon) lpattern(solid)  ///
    xtitle("Length (cm)", size(small)) ///
    ytitle("") ///
	ylabel(#5, labsize(small)) xlabel(#10, labsize(small)))
	*/
	
preserve 
rename length fitted_length
keep fitted_length observed_prob*  species region domain draw
duplicates drop
tempfile observed_prob
save `observed_prob', replace
restore


* new code using MOM to avoid non-convergence 
tempfile new
save `new', replace

global fitted_sizes

levelsof domain, local(regs)

foreach r of local regs {
    use `new', clear
    keep if domain=="`r'"
    di "`r'"

    keep length catch_proj
    drop if missing(length) | missing(catch_proj)
    drop if catch_proj<=0
	replace catch_proj=round(catch_proj)
	su catch_proj
	local tot_n_fish=`r(sum)'
	
    * Gamma needs strictly positive support
    drop if length<=0

    * observed range (weighted or unweighted; here unweighted over remaining bins)
    quietly summarize length
    local minL = r(min)
    local maxL = r(max)

    * --------
    * (A) Estimate gamma parameters robustly (MOM with freq weights)
    * --------
    quietly summarize length [fw=catch], meanonly
    local mu = r(mean)
    local Nw = r(sum_w)

    * Weighted variance: Var = E[x^2] - (E[x])^2 using the same freq weights
    gen double length2 = length^2
    quietly summarize length2 [fw=catch], meanonly
    local ex2 = r(mean)
    local v   = `ex2' - (`mu'^2)

    * Guard: if variance is 0 or numerically tiny, make it a near-degenerate gamma
    if (`v'<=1e-10 | missing(`v') | missing(`mu') | `mu'<=0) {
        * Put essentially all mass at mu by using huge alpha
        local alpha = 1e6
        local beta  = `mu'/`alpha'
    }
    else {
        local alpha = (`mu'^2)/`v'
        local beta  = `v'/`mu'
    }

    * --------
    * (B) Simulate a truncated gamma sample via rejection sampling
    * --------
    local ndraw = `tot_n_fish'   // sample size for the simulated distribution
    clear
    set obs `ndraw'

    * draw
    gen double gammafit = rgamma(`alpha', `beta')
    replace gammafit = round(gammafit)

    * truncate to observed range
    keep if gammafit>=`minL' & gammafit<=`maxL'

    * If rejection killed everything, try again with more draws (once)
    if _N==0 {
        clear
        set obs `=5*`ndraw''
        gen double gammafit = rgamma(`alpha', `beta')
        replace gammafit = round(gammafit)
        keep if gammafit>=`minL' & gammafit<=`maxL'
        if _N==0 continue
    }

    gen nfish = 1
    collapse (sum) nfish, by(gammafit)
    egen sumnfish = total(nfish)
    gen double fitted_prob = nfish/sumnfish
    gen domain = "`r'"

    tempfile fitted_sizes_`=_N'   
    save `fitted_sizes_`=_N'', replace
    global fitted_sizes "$fitted_sizes `fitted_sizes_`=_N''"
}

clear
dsconcat $fitted_sizes
rename gammafit fitted_length

merge 1:1 fitted_length domain using `observed_prob'
sort domain fitted_length 
mvencode fitted_prob observed_prob*, mv(0) override 

split domain, parse(_)
replace species=domain2
replace region=domain1
drop domain1 domain2
drop draw
rename domain3 draw
destring draw, replace 
rename fitted_l length

drop _merge
drop nfish sum
order species region domain draw length
rename fitted_prob fitted_prob_proj
merge 1:1  species region draw length using "$misc_data_cd/baseline_catch_at_length_region.dta"

keep species region domain draw length fitted* observed*
mvencode fitted* observed*, mv(0) override
drop observed_prob
rename fitted_prob fitted_prob_base

/*
*plots of base and projected catch-at-length
collapse (mean) observed* fitted*, by(species region length)
gen domain=region+"_"+species

levelsof domain if spe!="scup", local(domz)
foreach d of local domz{
	twoway (scatter observed_prob_base length if domain=="`d'" ,   cmissing(no) connect(direct) lcol(gray) lwidth(med)  lpat(solid) msymbol(o) mcol(gray) $graphoptions) ///
		    (scatter observed_prob_proj length if  domain=="`d'"  , cmissing(no) connect(direct) lcol(black)   lwidth(med)  lpat(solid) msymbol(i)   ///
			xtitle("Length (cm)", yoffset(-2)) ytitle("Prob")    ylab(, angle(horizontal) labsize(vsmall)) ///
			legend(lab(1 "base") lab(2 "proj") cols() yoffset(-2) region(color(none)))   title("`d'", size(small))  name(dom`d', replace))
 local graphnames `graphnames' dom`d'
}

grc1leg `graphnames', rows(2)

levelsof domain if spe!="scup", local(domz)
foreach d of local domz{

twoway (scatter fitted_prob_base length if domain=="`d'" ,   connect(direct) cmissing(no)  lcol(gray) lwidth(med)  lpat(solid) msymbol(o) mcol(gray) $graphoptions) ///
		    (scatter fitted_prob_proj length if  domain=="`d'"  , connect(direct) cmissing(no)  lcol(black)   lwidth(med)  lpat(solid) msymbol(i)   ///
			xtitle("Length (cm)", yoffset(-2)) ytitle("Prob")    ylab(, angle(horizontal) labsize(vsmall)) ///
			legend(lab(1 "base") lab(2 "proj") cols() yoffset(-2) region(color(none)))   title("`d'", size(small))  name(dom`d', replace))
 local graphnames `graphnames' dom`d'
}

grc1leg `graphnames', rows(2)
*/


* Expand regional data by state and export for simulation
keep length fitted_prob_proj species region draw
drop if fitted==0
rename fitted_prob_proj fitted_prob

preserve
keep if species=="scup"
expand 9 
bysort species length draw: gen n=_n 
gen state="MA" if n==1
replace state="MD" if n==2
replace state="RI" if n==3
replace state="CT" if n==4
replace state="NY" if n==5
replace state="NJ" if n==6
replace state="DE" if n==7
replace state="VA" if n==8
replace state="NC" if n==9
drop n
tempfile scup
save `scup', replace 
restore 

preserve
keep if species=="sf"
expand 4 if region=="NO"
bysort region length draw: gen n=_n if region=="NO"
gen state="MA" if n==1
replace state="RI" if n==2
replace state="CT" if n==3
replace state="NY" if n==4
drop n

expand 4 if region=="SO"
bysort region length draw: gen n=_n if region=="SO"
replace state="MD" if n==1
replace state="VA" if n==2
replace state="DE" if n==3
replace state="NC" if n==4
drop n
replace state="NJ" if region=="NJ"
tempfile sf
save `sf', replace 
restore 

preserve
keep if species=="bsb"
expand 4 if region=="NO"
bysort region length draw: gen n=_n if region=="NO"
gen state="MA" if n==1
replace state="RI" if n==2
replace state="CT" if n==3
replace state="NY" if n==4
drop n

expand 4 if region=="SO"
bysort region length draw: gen n=_n if region=="SO"
replace state="MD" if n==1
replace state="VA" if n==2
replace state="DE" if n==3
replace state="NC" if n==4
drop n
replace state="NJ" if region=="NJ"
tempfile bsb
save `bsb', replace 
restore 

clear
u `scup', clear 
append using `sf'
append using `bsb'

destring draw, replace
order state species draw length
sort state species draw length
drop region 

export delimited using "$misc_data_cd/proj_catch_at_length_state.csv", replace 