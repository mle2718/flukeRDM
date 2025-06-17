
* This file generates catch-at-length distributions for each species for:
		* a) the calibration year. 
		* b) the projection year, which is created using (a) along with stock assessment numbers-at-age projections 
* The general strategy is:
		* a) 
			* 1) pull release length data and compute proportion released-at-length
			* 2) multiply (1) by an estimate of total discards to get numbers released-at-length
			* 3) pull harvest length data and compute proportion harvest-at-length
			* 4) multiply (3) by an estimate of total harvest to get numbers released-at-length 
			* 5) sum (2) and (5) across length categories to get catch-at-length 

* Define local macro
local region_logic `""cond(inlist(state, "MA", "RI", "CT", "NY"), "NO", cond(state == "NJ", "NJ", cond(inlist(state, "DE", "MD", "VA", "NC"), "SO")))""'




* CT VAS
cd "C:\Users\andrew.carr-harris\Desktop\MRIP_data_2025\length_data"
import excel "2024 CT VAS SFL SCUP BSB.xlsx", clear first 
renvarlab, lower
gen species="sf" if catch_common_name== "FLOUNDER, SUMMER"
replace species="scup" if catch_common_name== "SCUP"
replace species="bsb" if catch_common_name== "BASS, BLACK SEA"
keep if disp=="RELEASED"
expand quantity
gen nfish=1
rename length length_inches
gen length_cm=length_inches*2.54
gen year=year(tripdate)
*tab year 
gen state="CT"
gen source="CT_VAS"
keep year length* state source species nfish

tempfile ct_vas
save `ct_vas', replace

* NJ VAS
import excel "NJ-VAS BSB.xlsx", clear first 
tempfile njbsb
save `njbsb', replace

import excel "NJ-VAS SF.xlsx", clear first 
append using `njbsb'
renvarlab, lower
tab species
replace species="bsb" if species== "blkseabass"
replace species="sf" if species== "sumflndr"
keep if disp=="Rel"
rename length length_inches
gen length_cm=length_inches*2.54
rename fishyear year
gen state="NJ"
gen source="NJ_VAS"
gen nfish=1
keep year length* state source species nfish
tempfile nj_vas
save `nj_vas', replace

* RI VAS
import excel "sfl_scu_bsb_24_25_RI_release.xlsx", clear first 
renvarlab, lower
tab common
gen species="sf" if common== "FLOUNDER, SUMMER"
replace species="scup" if common== "SCUP"
replace species="bsb" if common== "BASS, BLACK SEA"
expand reportedquantity
rename specieslength length_inches
gen length_cm=length_inches*2.54
drop state
gen state="RI"
gen source="RI_VAS"
gen nfish=1
drop lengthunit
keep year length* state source species nfish
tempfile ri_vas
save `ri_vas', replace

* ALS tag
cd "C:\Users\andrew.carr-harris\Desktop\MRIP_data_2025\length_data"
import excel using "FLK_ALS_TAG_DATA_2025.xlsx", clear first 
keep Date Place Species Length zonedescription
tempfile als_fluke_tag
save `als_fluke_tag', replace 

import excel using "BSB_ALS_TAG_DATA_2025.xlsx", clear first 
keep Date Place Species Length zonedescription
tempfile als_bsb_tag
save `als_bsb_tag', replace 

import excel using "SCP_ALS_TAG_DATA_2025.xlsx", clear first 
keep Date Place Species Length zonedescription
append using `als_bsb_tag'
append using `als_fluke_tag'

renvarlab, lower
split date, pars("/")
destring date3, replace
tab date3, missing
rename date3 year
keep if year==$calibration_year_num
drop date1 date2

split placetagged, pars(",")
replace placetagged2=ltrim(rtrim(placetagged2))
replace placetagged3=ltrim(rtrim(placetagged3))

gen state="MA" if placetagged2=="MA"
replace state="RI" if placetagged2=="RI"
replace state="CT" if placetagged2=="CT"
replace state="NY" if placetagged2=="NY"
replace state="NJ" if placetagged2=="NJ"
replace state="DE" if placetagged2=="DE"
replace state="MD" if placetagged2=="MD"
replace state="VA" if placetagged2=="VA"
replace state="NC" if placetagged2=="NC"

replace state="MA" if placetagged3=="MA" & state==""
replace state="RI" if placetagged3=="RI" & state==""
replace state="CT" if placetagged3=="CT" & state==""
replace state="NY" if placetagged3=="NY" & state==""
replace state="NJ" if placetagged3=="NJ" & state==""
replace state="DE" if placetagged3=="DE" & state==""
replace state="MD" if placetagged3=="MD" & state==""
replace state="VA" if placetagged3=="VA" & state==""
replace state="NC" if placetagged3=="NC" & state==""

*browse if state==""

drop if strmatch(placetagged, "* SC")==1
replace state= "NJ" if strmatch(placetagged, "* NJ")==1
replace state= "NY" if strmatch(placetagged, "* NY")==1
replace state= "NY" if strmatch(placetagged, "* NYC")==1
replace state= "DE" if strmatch(zonedescription, "* DE")==1
replace state= "MD" if strmatch(zonedescription, "* MD")==1
replace state= "NJ" if strmatch(placetagged, "* Nj")==1
replace state= "NY" if strmatch(placetagged, "Long Island Sound")==1
replace state= "NY" if strmatch(placetagged, "Cold Spring Harbor")==1
replace state= "NJ" if strmatch(placetagged, "Axel Carson Reef")==1
replace state= "CT" if strmatch(placetagged, "*CT")==1
replace state= "NJ" if strmatch(zonedescription, "NJ*")==1
replace state= "NY" if strmatch(placetagged, "*NY*")==1
replace state= "RI" if strmatch(zonedescription, "*Rhode Island*")==1
replace state= "NY" if strmatch(zonedescription, "*NY*")==1
replace state= "NJ" if strmatch(placetagged, "*South Amboy*")==1
replace state= "NJ" if strmatch(placetagged, "*S. Amboy*")==1
replace state= "NY" if strmatch(placetagged, "*Brooklyn*")==1
replace state= "NY" if strmatch(placetagged, "*Bklyn*")==1
replace state= "NY" if strmatch(placetagged, "*Rockaway*")==1
replace state= "NJ" if strmatch(placetagged, "*Atlantic Beach Reef*")==1
replace state= "NY" if strmatch(placetagged, "*Coney Island*")==1
replace state= "NY" if strmatch(zonedescription, "*Fire Island Inlet*")==1 & state==""
replace state= "NJ" if strmatch(placetagged, "*Keansburg*")==1 & state==""
replace state= "NJ" if strmatch(placetagged, "*Keyport*")==1 & state==""
replace state= "NY" if strmatch(placetagged, "*Mattituck, LIS*")==1 & state==""
replace state= "NY" if strmatch(placetagged, "*Middle Ground Light, LIS*")==1 & state==""
replace state= "NJ" if strmatch(placetagged, "*Navesink*")==1 & state==""
replace state= "CT" if strmatch(placetagged, "*Sandy Hook*")==1 & state==""
replace state= "NJ" if strmatch(placetagged, "*Perth Amboy*")==1 & state==""
replace state= "NY" if strmatch(placetagged, "*Verizano Bridge*")==1 & state==""
replace state= "NY" if strmatch(placetagged, "*Raritan Bay, Buoy 41*")==1 & state==""
replace state= "NY" if strmatch(zonedescription, "*Raritan Bay*")==1 & state==""
replace state= "NY" if strmatch(zonedescription, "*Port Jefferson*")==1 & state==""
*browse if state==""
drop if state==""

rename length length_inches
gen length_cm=length_inches*2.54
gen nfish=1
gen source="ALS"
replace species="sf" if species== "Fluke"
replace species="scup" if species== "Scup"
replace species="bsb" if species== "Black Sea Bass"
keep state source length* species nfish year

tempfile als_tag
save `als_tag', replace 

* ALS recapture
import excel using "FLK_ALS_REC_DATA_2025.xlsx", clear first 
keep RecaptureDate PlaceRecaptured Species Length zonedesc
tempfile als_fluke_rec
save `als_fluke_rec', replace 

import excel using "BSB_ALS_REC_DATA_2025.xlsx", clear first 
keep RecaptureDate PlaceRecaptured Species Length zonedesc
append using `als_fluke_rec'

renvarlab, lower
split recapturedate, pars("/")
destring recapturedate3, replace
tab recapturedate3, missing
rename recapturedate3 year
tab year 
keep if year==$calibration_year_num

drop recapturedate1 recapturedate2

split placerecaptured, pars(",")
replace placerecaptured2=ltrim(rtrim(placerecaptured2))
replace placerecaptured3=ltrim(rtrim(placerecaptured3))

gen state="MA" if placerecaptured2=="MA"
replace state="RI" if placerecaptured2=="RI"
replace state="CT" if placerecaptured2=="CT"
replace state="NY" if placerecaptured2=="NY"
replace state="NJ" if placerecaptured2=="NJ"
replace state="DE" if placerecaptured2=="DE"
replace state="MD" if placerecaptured2=="MD"
replace state="VA" if placerecaptured2=="VA"
replace state="NC" if placerecaptured2=="NC"

replace state="MA" if placerecaptured3=="MA" & state==""
replace state="RI" if placerecaptured3=="RI" & state==""
replace state="CT" if placerecaptured3=="CT" & state==""
replace state="NY" if placerecaptured3=="NY" & state==""
replace state="NJ" if placerecaptured3=="NJ" & state==""
replace state="DE" if placerecaptured3=="DE" & state==""
replace state="MD" if placerecaptured3=="MD" & state==""
replace state="VA" if placerecaptured3=="VA" & state==""
replace state="NC" if placerecaptured3=="NC" & state==""

*browse if state==""

drop if strmatch(placerecaptured, "* SC")==1
replace state= "NJ" if strmatch(placerecaptured, "* NJ")==1
replace state= "NY" if strmatch(placerecaptured, "* NY")==1
replace state= "NY" if strmatch(placerecaptured, "* NYC")==1
replace state= "DE" if strmatch(zonedesc, "* DE")==1
replace state= "MD" if strmatch(zonedesc, "* MD")==1
replace state= "NJ" if strmatch(placerecaptured, "* Nj")==1
replace state= "NY" if strmatch(placerecaptured, "Long Island Sound")==1
replace state= "NY" if strmatch(placerecaptured, "Cold Spring Harbor")==1
replace state= "NJ" if strmatch(placerecaptured, "Axel Carson Reef")==1
replace state= "CT" if strmatch(placerecaptured, "*CT")==1
replace state= "NJ" if strmatch(zonedesc, "NJ*")==1
replace state= "NY" if strmatch(placerecaptured, "*NY*")==1
replace state= "RI" if strmatch(zonedesc, "*Rhode Island*")==1
replace state= "NY" if strmatch(zonedesc, "*NY*")==1
replace state= "NJ" if strmatch(placerecaptured, "*South Amboy*")==1
replace state= "NJ" if strmatch(placerecaptured, "*S. Amboy*")==1
replace state= "NY" if strmatch(placerecaptured, "*Brooklyn*")==1
replace state= "NY" if strmatch(placerecaptured, "*Bklyn*")==1
replace state= "NY" if strmatch(placerecaptured, "*Rockaway*")==1
replace state= "NJ" if strmatch(placerecaptured, "*Atlantic Beach Reef*")==1
replace state= "NY" if strmatch(placerecaptured, "*Coney Island*")==1
replace state= "NY" if strmatch(zonedesc, "*Fire Island Inlet*")==1 & state==""
replace state= "NJ" if strmatch(placerecaptured, "*Keansburg*")==1 & state==""
replace state= "NJ" if strmatch(placerecaptured, "*Keyport*")==1 & state==""
replace state= "NY" if strmatch(placerecaptured, "*Mattituck, LIS*")==1 & state==""
replace state= "NY" if strmatch(placerecaptured, "*Middle Ground Light, LIS*")==1 & state==""
replace state= "NJ" if strmatch(placerecaptured, "*Navesink*")==1 & state==""
replace state= "CT" if strmatch(placerecaptured, "*Sandy Hook*")==1 & state==""
replace state= "NJ" if strmatch(placerecaptured, "*Perth Amboy*")==1 & state==""
replace state= "NY" if strmatch(placerecaptured, "*Verizano Bridge*")==1 & state==""
replace state= "NY" if strmatch(placerecaptured, "*Raritan Bay, Buoy 41*")==1 & state==""
replace state= "NY" if strmatch(zonedesc, "*Raritan Bay*")==1 & state==""
replace state= "NY" if strmatch(zonedesc, "*Port Jefferson*")==1 & state==""
*browse if state==""
drop if state==""

rename length length_inches
gen length_cm=length_inches*2.54
gen nfish=1
gen source="ALS"
replace species="sf" if species== "Fluke"
replace species="scup" if species== "Scup"
replace species="bsb" if species== "Black Sea Bass"
keep state source length* species nfish year

tempfile als_rec
save `als_rec', replace 


* MRIP data 
cd $input_data_cd

dsconcat $b2list
sort year strat_id psu_id id_code
drop if strmatch(id_code, "*xx*")==1
replace common=subinstr(lower(common)," ","",.)

sort year strat_id psu_id id_code
replace common=subinstr(lower(common)," ","",.)

* keep management unit states
keep if inlist(st,25, 44, 9, 36, 34, 51, 10, 24, 37)

gen state="MA" if st==25
replace state="MD" if st==24
replace state="RI" if st==44
replace state="CT" if st==9
replace state="NY" if st==36
replace state="NJ" if st==34
replace state="DE" if st==10
replace state="VA" if st==51
replace state="NC" if st==37


keep if $calibration_year

gen species="sf" if inlist(common, "summerflounder") 
replace species="scup" if inlist(common, "scup") 
replace species="bsb" if inlist(common, "blackseabass") 

keep if inlist(common, "summerflounder",  "scup", "blackseabass") 
gen source="MRIP"
rename l_cm_bin length_cm 
rename l_in_bin length_inches

gen nfish=1
keep length* state species year nfish source
tempfile mrip
save `mrip', replace 


* Append all the discard data together
u `mrip', clear
append using `als_rec'
append using `als_tag'
append using `ri_vas'
append using `nj_vas'
append using `ct_vas'

* Group the data into regions
*gen region = `region_logic'

gen region="NO" if inlist(state, "MA", "RI", "CT", "NY")
replace region="NJ" if inlist(state, "NJ")
replace region="SO" if inlist(state, "DE", "MD", "VA", "NC")

drop if  length_cm==. | length_in==.

replace length_cm = round(length_cm)
collapse (sum) nfish, by(species region length_cm)
sort species region length nfish
order species region length nfish

gen spec_reg=species+"_"+region
tabstat nfish, stat(sum) by(spec_reg)

egen sumfish=sum(nfish), by(species region)
gen prop_b2=nfish/sumfish

tempfile prop_b2
save `prop_b2', replace 



* Pull harvest lengths from MRIP 
clear
mata: mata clear
tempfile tl1 sl1
dsconcat $triplist
sort year strat_id psu_id id_code
save `tl1'

clear
dsconcat $sizelist
sort year strat_id psu_id id_code
replace common=subinstr(lower(common)," ","",.)
save `sl1'

use `tl1'
merge 1:m year strat_id psu_id id_code using `sl1', keep(1 3) nogen

keep if $calibration_year

* keep management unit states
keep if inlist(st,25, 44, 9, 36, 34, 51, 10, 24, 37)

gen state="MA" if st==25
replace state="MD" if st==24
replace state="RI" if st==44
replace state="CT" if st==9
replace state="NY" if st==36
replace state="NJ" if st==34
replace state="DE" if st==10
replace state="VA" if st==51
replace state="NC" if st==37

drop region
gen region="NO" if inlist(state, "MA", "RI", "CT", "NY")
replace region="NJ" if inlist(state, "NJ")
replace region="SO" if inlist(state, "DE", "MD", "VA", "NC")


/*classify catch into the things I care about (common==$mycommon) and things I don't care about "ZZ" */
gen common_dom="ZZ"
replace common_dom="SF" if strmatch(common, "summerflounder") 
replace common_dom="BS" if strmatch(common, "blackseabass") 
replace common_dom="SC" if strmatch(common, "scup") 

tostring wave, gen(w2)
tostring year, gen(year2)

gen my_dom_id_string=region+"_"+common_dom
replace my_dom_id_string=subinstr(ltrim(rtrim(my_dom_id_string))," ","",.)

preserve
keep my_dom_id my_dom_id_string
duplicates drop 
tempfile domains
save `domains', replace 
restore


/* this might speed things up if I re-classify all length=0 for the species I don't care about */
replace l_cm_bin=0 if !inlist(common, "summerflounder", "blackseabass", "scup")

sort year2  w2 strat_id psu_id id_code common_dom
svyset psu_id [pweight= wp_size], strata(strat_id) singleunit(certainty)

svy: tab l_cm_bin my_dom_id_string, count

/*save some stuff:matrix of proportions, row names, column names, estimate of total population size*/
	mat eP=e(Prop)
	mat eR=e(Row)'
	mat eC=e(Col)
	local PopN=e(N_pop)

	local mycolnames: colnames(eC)
	mat colnames eP=`mycolnames'
	
	clear
/*read the eP into a dataset and convert proportion of population into numbers*/
	svmat eP, names(col)
	foreach var of varlist *{
		replace `var'=`var'*`PopN'
	}
	/*read in the "row" */
svmat eR
order eR
rename eR l_cm_bin
	
drop *ZZ	
ds l, not
renvarlab `r(varlist)', prefix(i)
	
reshape long i, i(l_cm) j(new) string
split new, parse(_)
rename new1 region
rename new2 species
drop new
rename i nfish
rename l_cm length_cm
replace species="sf" if species=="SF"
replace species="bsb" if species=="BS"
replace species="scup" if species=="SC"
order region species l n
sort region species l n
drop if length_cm==0
egen sumfish=sum(nfish), by(species region)
gen prop_ab1=nfish/sumfish

tempfile prop_ab1
save `prop_ab1', replace 

* Merge proportion discards and harvest-at-length together 
merge 1:1 species region length using `prop_b2'
drop _merge spec_reg sumfish

tempfile props
save `props', replace 

*Pull estimates of total harvest and discards from MRIP by region  
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

* keep management unit states
keep if inlist(st,25, 44, 9, 36, 34, 51, 10, 24, 37)

gen state="MA" if st==25
replace state="MD" if st==24
replace state="RI" if st==44
replace state="CT" if st==9
replace state="NY" if st==36
replace state="NJ" if st==34
replace state="DE" if st==10
replace state="VA" if st==51
replace state="NC" if st==37

drop region
gen region="NO" if inlist(state, "MA", "RI", "CT", "NY")
replace region="NJ" if inlist(state, "NJ")
replace region="SO" if inlist(state, "DE", "MD", "VA", "NC")

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

tostring wave, gen(wv2)
tostring year, gen(yr2)

gen my_dom_id_string=region+"_"+common_dom
replace my_dom_id_string=subinstr(ltrim(rtrim(my_dom_id_string))," ","",.)

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
rename sum_sf_tot_cat sf_cat
rename sum_sf_harvest sf_keep
rename sum_sf_releases sf_rel
rename sum_bsb_tot_cat bsb_cat
rename sum_bsb_harvest bsb_keep
rename sum_bsb_releases bsb_rel
rename sum_scup_tot_cat scup_cat
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

keep if common_dom=="SF"
drop if wp_int==0
encode my_dom_id_string, gen(my_dom_id)

preserve
keep my_dom_id my_dom_id_string
duplicates drop 
tempfile domains
save `domains', replace 
restore

tempfile basefile
save `basefile', replace 
* Create a postfile to collect results
tempfile results
postfile handle str15 varname str15 domain float total se ll95 ul95 using `results', replace

* Loop over variables
foreach var in sf_keep sf_rel sf_cat bsb_keep bsb_rel bsb_cat scup_keep scup_rel scup_cat {

    * Run svy mean for the variable by domain
    svy: total `var', over(my_dom_id)

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
split domain21, parse(bn)
drop domain21
destring domain211, replace
rename domain21 my_dom_id
merge m:1 my_dom_id using `domains' 
sort varname  my_dom_id
keep varname total  my_dom_id_string
split varname, parse(_)
drop varname
rename varname1 species
rename varname2 disp
reshape wide total, i(species my) j(disp) string
split my, parse(_)
rename my_dom_id_string1 region
drop  my_dom_id_string2  my_dom_id_string
order region species 

merge 1:m species region using `props'
drop _merge
replace prop_ab1=0 if prop_ab1==.
replace prop_b2=0 if prop_b2==.

gen harvest=totalkeep*prop_ab1
gen discards=totalrel*prop_b2
gen catch=harvest+discards
collapse catch, by(region species length_cm)
sort region species length_cm



