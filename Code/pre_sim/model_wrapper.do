

/**** SFSBSB RDM code wrapper ****/

**Data availability**

* We make next year's projections in Oct./November
	// Wave 4 MRIP data available Sepetember 15th

* MRIP effort data: most recent full fishing year

* MRIP length data: we combine MRIP data with VAS discard length data. 
	// sources of VAS data used for discard length: NJ VAS, ALS, CT VAS, RI VAS, MRIP

* MRIP catch data for calibration year: most recent full fishing year
		
* MRIP catch data for projection year: most recent 12 waves of MRIP data
		// For FY 2026 projections: we use MRIP data from 2025 waves 1 to 4,  2024 waves 1 to 6 , 2023 wave 5 and 6
		
* Stock assessment projections data:
		// Jan 1 2024 NAA to compute historical rec. selectivity 
		// Jan 1 2026 NAA to compute projected catch-at-length
		
* NEFSC trawl survey data from 2024, 2023, 2022 years used to create age-length keys

* MRIP data is stored in  
	// "smb://net/mrfss/products/mrip_estim/Public_data_cal2018"
	// Windows, just mount \\net.nefsc.noaa.gov\mrfss to A:\

* Dependencies
	// ssc install xsvmat 
    // ssc install gammafit 



**Adjust globals**

* These need to be changed every year 

* year-waves of MRIP data. 
global yr_wvs 20221 20222 20223 20224 20225 20226 20231 20232 20233 20234 20235 20236  20241 20242 20243 20244 20245  20246 20251 20252 20253 20255
global yearlist 2022 2023 2024 2025
global wavelist 1 2 3 4 5 6

global calibration_year "(year==2024 & inlist(wave, 1, 2, 3, 4, 5, 6))"
global calibration_year_num 2024

global projection_catch_per_trip_years "(year==2025 & inlist(wave, 1, 2, 3, 4)) | (year==2024) | (year==2023) | (year==2022 & inlist(wave, 5, 6))" //ADJUST THIS AFTER MRIP DATA RELEASE

global calibration_start_date td(01jan2024)
global calibration_end_date td(31dec2024)

global projection_date_start td(01jan2026)
global projection_date_end td(31dec2026)


* Federal holidays are are considered "weekend" days by the MRIP 
	// store in global list for estimation of fishing effort at the month and kind-of-day (weekend/weekday) level
/*
Federal Holidays Included:
New Year's Day: January 1 (observed on January 2 if it falls on Sunday).
Martin Luther King Jr. Day: Third Monday in January.
Presidents' Day: Third Monday in February.
Memorial Day: Last Monday in May.
Juneteenth National Independence Day: June 19.
Independence Day: July 4 (observed on July 5 if it falls on Sunday).
Labor Day: First Monday in September.
Columbus Day: Second Monday in October.
Veterans Day: November 11 (observed on November 10 if it falls on Sunday).
Thanksgiving Day: Fourth Thursday in November.
Christmas Day: December 25 (observed on December 26 if it falls on Sunday).
*/


* Federal holidays in the calibration year 
global fed_holidays "inlist(day, td(10nov2023), td(23nov2023), td(25dec2023), td(01jan2024), td(15jan2024), td(19feb2024), td(27may2024), td(19jun2024), td(04jul2024), td(02sep2024), td(14oct2024), td(11nov2024), td(28nov2024), td(25dec2024))" 

* Fed holidays in the projection year 
global fed_holidays_y2 "inlist(day_y2, td(01jan2026), td(19jan2026), td(16feb2026), td(25may2026), td(19jun2026), td(03jul2026), td(07sep2026), td(12oct2026), td(11nov2026), td(26nov2026), td(25dec2026))"

* Put leap-year days here
global leap_yr_days "td(29feb2024)" 

* Number of model iterations
global ndraws 5

* set years of which to pull the NEFSC trawl survey data
global NEFSC_svy_yrs "inlist(year,2024, 2023, 2022)"

* Adjustment to 2022 survey trip costs to account for inflation
	// https://www.bls.gov/data/inflation_calculator.htm, January 2022 - January 202X 
global inflation_expansion=1.31 

* Adjust project paths based on user
global input_code_cd "${here}/Code/pre_sim" 
global misc_data_cd "${sfdatadir}/miscellaneous" 
global calib_catch_data_cd "${sfdatadir}\calib_catch_draws"
global proj_catch_data_cd "${sfdatadir}\proj_catch_draws"
global figure_cd  "${sfdatadir}\figures"

global log_dir "${input_code_cd}/logs" 

/* make directories if necessary */
capture mkdir $misc_data_cd
capture mkdir $calib_catch_draws_cd
capture mkdir $proj_catch_data_cd
capture mkdir $figure_cd
capture mkdir $log_dir

/* start log */
cap log close
log using "${log_dir}\sf_model_wrapper_log_$S_DATE.smcl", replace
														   


global seed 03211990

**************************************************Model calibration ************************************************** 
* 1) Pull the MRIP data
do "$input_code_cd\MRIP_data_wrapper.do"


* 2) Estimate directed trips during calibration period
do "$input_code_cd\directed_trips_calibration.do"
		// This file calls "set_regulations.do". You must enter the SQ regulations in the calibration and projection year. 
		// THIS NEEDS TO BE ADJUSTED EVERY YEAR. 

* 3) Create distirbutions of costs per trip across strata
do "$input_code_cd\survey_trip_costs.do"

* 4) Estimate and simulate distirbution of angler utility coefficients 
do "$input_code_cd\estimate_angler_preferences.do"

* 5) Estimate catch-per-trip at the month and mode level
		// a) compute mean catch-per-trip and standard error, imputing standard errors from historcial data when they are missing. 
		do "$input_code_cd\catch_per_trip_calibration_part1.do"

		// b) use copula model (in R) to simulate harvest and discards per-trip
		* run "copula_modeling_calibration.R"
		
		// c) generate estimates of simulated total harvest based on random draws of catch-per-trip and directed trips
		do "$input_code_cd\calibration_catch_per_trip_part2.do"


// 6) compare calibration output to MRIP, and retain total simulated harvest and discards to apply to the baseline catch-at-length distribution
do "$input_code_cd\compare_calibration_data_to_MRIP.do" 


// 7) Generate baseline-year catch-at-length, using the simulated harvest/discard totals from step 6
do "$input_code_cd\calibration_catch_at_length.do"


// 8) Generate projection-year catch-at-length, incorporating the stock assessment data
		do "$input_code_cd\projected_catch_at_length.do"

		
// 9)  Estimate projected catch-per-trips at the month and mode level
		 *use MRIP catch data from the last THREE full years. 
		 
		//a) compute mean catch-per-trip and standard error, imputing standard errors from historcial data when they are missing. 
		 do "$input_code_cd\catch_per_trip_projection_part1.do"

		//b) use copula model (in R) to simulate harvest and discards per-trip
		* run "copula_modeling_projection.R"
		
		//c) generate estimates of simulated total harvest based on random draws of catch-per-trip and directed trips
		do "$input_code_cd\catch_per_trip_projection_part2.do"
		
		//d) compare estimates of mean projected catch to MRIP data to ensure consistency and remove extraneous columns from projected catch draw data
		do "$input_code_cd\compare_projection_data_to_MRIP.do"
		

// 10) Run the projection loop in R











