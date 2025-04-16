


****Set regulations for the calibration period and the projection period****
*These need to be changed every year 


*FY 2026 model
*************************
*Create the baseline regulations for the calibration period, 
*which covers (year==2024 & inlist(wave, 1, 2, 3, 4, 5)) | (year==2023 & inlist(wave, 6)). So FY 2023 regs until May 1, 2024, FY2024 regs after May 1, 2024
gen fluke_bag=0 
gen fluke_min=100

gen bsb_bag=0
gen bsb_min=100

gen scup_bag=0
gen scup_min=100

*Fluke regs calibration year 
replace hadd_bag=15 if  day>=td(14aug2023) & day<=td(28feb2024) & inlist(mode, "fh")
replace hadd_min=18 if  day>=td(14aug2023) & day<=td(28feb2024)  & inlist(mode, "fh")

replace hadd_bag=10 if  day>=td(14aug2023) & day<=td(28feb2024) & inlist(mode, "pr", "sh")
replace hadd_min=17 if  day>=td(14aug2023) & day<=td(28feb2024)  & inlist(mode, "pr", "sh")

replace hadd_bag=15 if  day>=td(01apr2024) & day<=td(30apr2024) & inlist(mode, "fh")
replace hadd_min=18 if  day>=td(01apr2024) & day<=td(30apr2024)  & inlist(mode, "fh")

replace hadd_bag=10 if  day>=td(01apr2024) & day<=td(30apr2024) & inlist(mode, "pr", "sh")
replace hadd_min=17 if  day>=td(01apr2024) & day<=td(30apr2024)  & inlist(mode, "pr", "sh")

replace hadd_bag=15 if  day>=td(01may2024) & day<=td(28feb2025) 
replace hadd_min=18 if  day>=td(01may2024) & day<=td(28feb2025)  


*bsb regs calibration year 
replace cod_bag=1 if  day>=td(01sep2023) & day<=td(31oct2023)
replace cod_min=22 if  day>=td(01sep2023) & day<=td(31oct2023)

replace cod_bag=1 if  day>=td(01sep2024) & day<=td(31oct2024)
replace cod_min=23 if  day>=td(01sep2024) & day<=td(31oct2024)


*scup regs calibration year 
replace cod_bag=1 if  day>=td(01sep2023) & day<=td(31oct2023)
replace cod_min=22 if  day>=td(01sep2023) & day<=td(31oct2023)

replace cod_bag=1 if  day>=td(01sep2024) & day<=td(31oct2024)
replace cod_min=23 if  day>=td(01sep2024) & day<=td(31oct2024)
*************************

tempfile regulations
save `regulations', replace 

*now merge to this file the calender for y+1 (_y2)
clear 
set obs 2
gen day_y2=$projection_date_start if _n==1
replace day_y2=$projection_date_end if _n==2
format day_y2 %td
tsset day_y2
tsfill, full

gen day1=day(day_y2)
gen month1=month(day_y2)
gen year_y2=year(day_y2)
drop if day_y2==$leap_yr_days
gen dow_y2 = dow(day_y2)  

gen kod_y2="we" if inlist(dow, 5, 6, 0)
replace kod_y2="wd" if inlist(dow, 1, 2, 3, 4)		
replace kod_y2="we" if $fed_holidays_y2

gen month2_y2= string(month1,"%02.0f")
rename month2_y2 month_y2
gen mode="sh"
expand 2, gen(dup)
replace mode="pr" if dup==1
drop dup
expand 2 if mode=="pr", gen(dup)
replace mode="fh" if dup==1
drop dup


merge 1:m  mode day1 month1 using `regulations'
drop if day==$leap_yr_days
drop _merge 
order year mode month kod dow day  draw fluke_bag fluke_min bsb_bag bsb_min scup_bag scup_min  day_y2 dow_y2 kod_y2 month_y2
sort  mode day draw


*************************
*Create status-quo regualtions for projection period here: td(01jan2026)  -  td(31dec2026)
gen fluke_bag_y2=0 
gen fluke_min_y2=100

gen bsb_bag_y2=0
gen bsb_min_y2=100

gen scup_bag_y2=0
gen scup_min_y2=100

*Fluke status quo regs for projection year 
replace hadd_bag_y2=15 if day_y2>=td(01may2025) & day_y2<=td(28feb2026) 
replace hadd_min_y2=18 if day_y2>=td(01may2025) & day_y2<=td(28feb2026) 

replace hadd_bag_y2=15 if day_y2>=td(01apr2026) & day_y2<=td(30apr2026) 
replace hadd_min_y2=18 if day_y2>=td(01apr2026) & day_y2<=td(30apr2026) 

*BSB status quo regs for projection year 
replace cod_bag_y2=1 if  day_y2>=td(01sep2025) & day_y2<=td(31oct2025)
replace cod_min_y2=23 if  day_y2>=td(01sep2025) & day_y2<=td(31oct2025)


*Scup status quo regs for projection year 
replace cod_bag_y2=1 if  day_y2>=td(01sep2025) & day_y2<=td(31oct2025)
replace cod_min_y2=23 if  day_y2>=td(01sep2025) & day_y2<=td(31oct2025)


*************************


*translate to inches
replace fluke_min = fluke_min*2.54
replace bsb_min = bsb_min*2.54
replace scup_min = scup_min*2.54

replace fluke_min_y2 = fluke_min_y2*2.54
replace bsb_min_y2 = bsb_min_y2*2.54
replace scup_min_y2 = scup_min_y2*2.54


