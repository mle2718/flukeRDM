


* This script create projection catch draw files for each state that contains:
	* 50 trips in each day of the calibration year in which there were directed trips, each with 30 draws of catch-per-trip
	* Demographics: age and avidity (number trips past 12 months) come from the base_outcomes files generated in the calibration simulation 

	
********************************


/* Check to make sure the copula model did not produce NAs
local statez "MA RI CT NY NJ DE MD VA NC"
foreach s of local statez{
	
forv i=1/100{

import excel using "$iterative_input_data_cd\proj_catch_draws_`s'_`i'.xlsx", clear firstrow
gen sf_cat=sf_keep_sim+sf_rel_sim
gen bsb_cat=bsb_keep_sim+bsb_rel_sim
gen scup_cat=scup_keep_sim+scup_rel_sim
*di "`s'"
*di `i'
qui{
	 count if sf_keep_sim==. | sf_rel_sim==. | bsb_keep_sim==. | bsb_rel_sim==. | scup_keep_sim==. | scup_rel_sim==. | sf_cat==. | bsb_cat ==. | scup_cat==. 
}

if `r(N)'>0{
	di `r(N)'
	levelsof my_dom if  sf_keep_sim==. | sf_rel_sim==. | bsb_keep_sim==. | bsb_rel_sim==. | scup_keep_sim==. | scup_rel_sim==. | sf_cat==. | bsb_cat ==. | scup_cat==. 
	
}
}
}

*/



 
* faster version 
local regions "MA RI CT NY NJ DE MD VA NC"
set more off
*set rmsg off

foreach s of local regions {
	
	*local s "DE"
    import delimited using "$iterative_input_data_cd\archive\directed_trips_calibration\directed_trips_calibration_`s'.csv", clear

    gen double date_num = date(date, "DMY")
	drop month month1
    gen byte   month    = month(date_num)
    gen str2   month1   = string(month, "%02.0f")
    gen byte   wave     = cond(inlist(month,1,2),1, ///
                        cond(inlist(month,3,4),2, ///
                        cond(inlist(month,5,6),3, ///
                        cond(inlist(month,7,8),4, ///
                        cond(inlist(month,9,10),5,6)))))
    drop date_num

    drop if dtrip==0

    *gen str2 state = substr(region,1,2)

	drop  dtrip *_bag *_min *_y2
	
	tempfile base
    save `base', replace

    *-----------------------------------------
    * 2) Loop draws
    *-----------------------------------------
	forvalues i=1/$ndraws {
		*local i 1
        use `base', clear
        keep if draw==`i'

        * Expand to 50 trips x 30 catch draws within each (mode,date)
        egen long dom = group(mode date)   // replaces encode(domain1)
        expand 50
        bysort mode date: gen int tripid = _n
        expand 30
        bysort mode date tripid: gen byte catch_draw = _n

		egen group=group(date tripid mode)
		
		qui distinct group if mode=="pr"
		local n_pr = `r(ndistinct)'
		
		qui distinct group if mode=="fh"
		local n_fh = `r(ndistinct)'
		
		qui distinct group if mode=="sh"
		local n_sh = `r(ndistinct)'
		
		preserve 
		keep date mode tripid
		duplicates drop 
		by mode: gen mode_id=_n
		tempfile mode_id
		save `mode_id', replace
		restore 
		
		merge m:1 date mode tripid using `mode_id', keep(3) nogen  
		
		qui distinct group if wave==1
		local n_wave1 = `r(ndistinct)'
		
		qui distinct group if wave==2
		local n_wave2 = `r(ndistinct)'
		
		qui distinct group if wave==3
		local n_wave3 = `r(ndistinct)'
		
		qui distinct group if wave==4
		local n_wave4 = `r(ndistinct)'
		
		qui distinct group if wave==5
		local n_wave5 = `r(ndistinct)'
		
		qui distinct group if wave==6
		local n_wave6 = `r(ndistinct)'
		
		preserve 
		keep date wave tripid
		duplicates drop 
		sort date wave tripid
		bysort wave: gen wave_id=_n
		tempfile wave_id
		save `wave_id', replace
		restore 
		
		merge m:1 date wave tripid using `wave_id', keep(3) nogen  
		
		
        preserve
            import excel using "$iterative_input_data_cd\archive\proj_catch_draws\proj_catch_draws_`s'_`i'.xlsx", clear firstrow
            split my_dom_id_string, parse(_)
            *rename my_dom_id_string1 state
            rename my_dom_id_string2 wave
            rename my_dom_id_string3 mode
            drop my_dom_id_string4 
            keep my_dom_id_string state wave mode  sf_* bsb_* scup_*
            tempfile excelpool
            save `excelpool', replace
        restore

        *---------------------------------------
        * BIG SPEEDUP:
        * sample catch outcomes by (mode,wave)
        *---------------------------------------
        egen long g = group(mode wave)
        bysort g: gen long gid = _n
        bysort g: gen long n_g = _N
        levelsof g, local(gs)

        tempfile trips_expanded
        save `trips_expanded', replace

        * Build catch outcomes dataset with keys (g, gid)
        clear
        tempfile catchall
        save `catchall', emptyok replace
        local seeded 0

        foreach gg of local gs {
		*local gg 10
            use `trips_expanded', clear
            keep if g==`gg'
            keep mode wave 
            local md  = mode[1]
            local wv  = wave[1]
            local n_needed = _N
			di "`md'"
			di "`wv'"
			di `n_needed'
            use `excelpool', clear
            keep if wave=="`wv'" & mode=="`md'"

			quietly count
			local mult = ceil(`n_needed'/r(N))
			expand `mult'
			sample `n_needed', count
			
            * If you need more control: ensure enough rows before sampling
            quietly count
            if (r(N) < `n_needed') {
                di as error "Not enough catch rows for st=`st' draw=`i' mode=`md' wave=`wv' need=`n_needed' have=" r(N)
                continue
            }

            gen long g   = `gg'
            gen long gid = _n
			destring wave, replace

            tempfile chunk
            save `chunk', replace

            if (`seeded'==0) {
                use `chunk', clear
				destring wave, replace
                save `catchall', replace
                local seeded 1
            }
            else {
                use `catchall', clear
                append using `chunk'
				destring wave, replace
                save `catchall', replace
            }
        }
		
        * Merge sampled catch onto trips by (g,gid)
        use `trips_expanded', clear
		destring wave, replace 
        merge 1:1 g gid using `catchall', keep(3) nogen

        drop g gid n_g
        compress
		
		sort date tripid catch_
		rename sf_catch sf_cat
		rename bsb_catch bsb_cat
		rename scup_catch scup_cat


		keep state draw ///
                 sf_keep sf_cat sf_rel ///
                 bsb_keep bsb_rel bsb_cat ///
                 scup_keep scup_rel scup_cat ///
                 mode month date day_i  wave ///
                 tripid catch_draw  day  
				 
		gen double date_num = date(date, "DMY")
		format date_num %td
		order state mode date tripid catch 
		compress
	
		save "$iterative_input_data_cd\archive\proj_catch_draws\proj_catch_draws_`s'_`i'.dta", replace
		
}		
}

