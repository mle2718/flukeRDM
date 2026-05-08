
options(scipen = 999)

packages <- c("tidyr",  "magrittr", "tidyverse", "reshape2", "splitstackshape","doBy","WriteXLS","Rcpp",
                 "ggplot2","dplyr","rlist","fitdistrplus","MASS","psych","rgl","copula","VineCopula","scales",
                 "univariateML","logspline","readr","data.table","conflicted", "readxl", "writexl", "fs", "fst",
                 "purrr", "readr", "here","plyr" , "furrr", "profvis", "future", "magrittr", "feather", "RStata", "haven")

# Install only those not already installed
installed <- packages %in% rownames(installed.packages())
if (any(!installed)) {
  install.packages(packages[!installed])
}
lapply(packages, library, character.only = TRUE)

library(plyr)
library(dplyr)

conflicts_prefer(here::here)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::mutate)
conflicts_prefer(dplyr::rename)
conflicts_prefer(dplyr::summarize)
conflicts_prefer(dplyr::summarise)
conflicts_prefer(dplyr::count)
conflicts_prefer(feather::read_feather)
conflicts_prefer(feather::write_feather)


# helpers
parse_date_any <- function(x) {
  data.table::as.IDate(as.Date(
    x,
    tryFormats = c("%d%b%Y", "%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y")
  ))
}

#There are four folders needed::
#input data - contains all the MRIP, biological data, angler characteristics data, as well as some data generated in the simulation
#code - contains all the model code
#output_data - this folder is empty to begin with. It stores final simulation output
#iterative_data -this folder is empty to begin with. It compiles data generated in the simulation

#Need to ensure that the globals below are set up in both this file and the stata model_wrapper.do file. 


#Set up R globals for input/output data and code scripts
code_cd=here("Code", "sim")
input_data_cd="C:/Users/andrew.carr-harris/Desktop/MRIP_data_2025"
iterative_input_data_cd="E:/Lou_projects/flukeRDM/flukeRDM_iterative_data"

###################################################
###############Pre-sim Stata code##################
###################################################

#Stata code extracts and prepares the data needed for the simulation

#Connect Rstudio to Stata
#options("RStata.StataPath" = "\"C:\\Program Files\\Stata17\\StataMP-64\"")
#options("RStata.StataVersion" = 17)

#Set number of original draws. We create 125 (in case some don't converge in the calibration), but only use 100 for the final run. Choose a lot fewer for test runs
n_simulations<-10

n_draws<-50 #Number of simulated trips per day

#First, open "$code_cd\model wrapper.do" and set globals:
#a) data years for different datasets
#b) number of draws (ndraws), which should be the same as the object n_simulations above
#c) cd's

#Second, open "$code_cd\set regulations.do" and set regulations for the calibration and projection period.

#Third, run the model wrapper code below:
#stata('do "C:/Users/andrew.carr-harris/Desktop/Git/rdmtool/lou_files/cod_haddock/code/model wrapper.do"')

###################################################




###################################################
###############Simulation R code###################
###################################################

# Notes:

# Simulation strata are the groups of choice occasions sharing common input data.
# For the 2025 SFSBSB RDM, the stratum is the combination of mode (pr/fh/sh) and state 

# Projection results are based on 100+ iterations of the model. In each iteration I pull 
# new distributions of catch-per-trip, directed fishing effort, projected catch-at-length, 
# and angler preferences. I calibrate the model with 125 iterations,

# Prior to running the simulations, save the catch_draw and directed_trip files as .fst 
statez <- c("MA", "RI", "CT", "NY", "NJ", "DE", "MD", "VA", "NC")

for(s in statez) {
  
  dtrip0<-read.csv(paste0("E:/Lou_projects/flukeRDM/flukeRDM_iterative_data/archive/directed_trips_calibration/directed_trips_calibration_", s,".csv"))
  write_fst(dtrip0, paste0("E:/Lou_projects/flukeRDM/flukeRDM_iterative_data/archive/directed_trips_calibration/directed_trips_calibration_", s,".fst"))

   for(i in 1:n_simulations) {
   catch<-read_dta(paste0("E:/Lou_projects/flukeRDM/flukeRDM_iterative_data/archive/calib_catch_draws/calib_catch_draws_",s, "_", i,".dta"))
   write_fst(catch, paste0("E:/Lou_projects/flukeRDM/flukeRDM_iterative_data/archive/calib_catch_draws/calib_catch_draws_",s, "_", i,".fst"))

}
}



##################### STEP 1 #####################
# Run the calibration0 algorithm to determine the difference between model- and MRIP-based harvest. 
# Repeat across iterations and strata 
# This code retains for each stratum the percent/absolute difference between model-based harvest and MRIP-based harvest by species. 

source(file.path(code_cd,"calibrate_rec_catch0_optimized.R"))

# Output files: 
# calibration_comparison.fst


##################### STEP 2 #####################
# Repeat the calibration algorithm but reallocate trip level harvest as discards, or vice versa 
# until the difference between model- and MRIP-based total harvest is within abs(5%) or <500 fish. 
# Model- and MRIP-based discards and total catch should also align.  

# If a reallocation of discards to harvest is needed, I reallocate h* percent of all discarded 
# fish that are between [(min_size - X inches), min_size] as harvest. If there are not enough eligible
# discards, increase X, which starts at 3 and increased to 4 if necessary. 

# If a reallocation of harvest to discards is needed, I reallocate h* percent of all harvested fish as discards.

# Note that in some iterations, the difference in harvest between the model and MRIP from Step 1 may be
# too large relative to the number of fish discarded in the model. The code in Step 2 identifies and drops these iterations.

# This script saves calibration output and reallocation parameters.

# Pre-compute date variables
for(s in statez) {
  dtrip <- data.table::as.data.table(
    fst::read_fst(file.path(
      iterative_input_data_cd,
      paste0("archive/directed_trips_calibration/directed_trips_calibration_", s, ".fst")))) %>% 
    dplyr::mutate(date_parsed = parse_date_any(date), 
                  date_parsed_y2 = parse_date_any(day_y2),
                  month=data.table::month(date_parsed)) %>% 
    dplyr::select(-date, -day_y2)
  
  write_fst(dtrip, file.path(
    iterative_input_data_cd,
    paste0("archive/directed_trips_calibration/directed_trips_calibration_", s, ".fst")))
}


statez <- c("MA", "RI", "CT", "NY", "NJ", "DE", "MD", "VA", "NC")
for(s in statez) {
  for(i in 1:n_simulations) {

    catch0<-read_fst(paste0("E:/Lou_projects/flukeRDM/flukeRDM_iterative_data/archive/calib_catch_draws/calib_catch_draws_",s, "_", i,".fst")) %>%
      dplyr::mutate(date_parsed = parse_date_any(date),
                    month=data.table::month(date_parsed)) %>%
      dplyr::select(-date_num, -date)
    write_fst(catch0, paste0("E:/Lou_projects/flukeRDM/flukeRDM_iterative_data/archive/calib_catch_draws/calib_catch_draws_",s, "_", i,".fst"))
    
  }
}

source(file.path(code_cd,"calibration_routine_final.R")) # this script calls "calibrate_rec_catch1_final.R"

# Output files: 
  # file.path(iterative_input_data_cd, paste0("archive/miscellaneous/calibrated_model_stats.fst")))
  # n_choice_occasions_ST_MD_DRAW.fst -  choice occasions to simulate in projection
  # base_outcomes_ST_MD_DRAW-  baseline trip outcomes


# Transfer projected catch draw files from .dta to .fst
statez <- c("MA", "RI", "CT", "NY", "NJ", "DE", "MD", "VA", "NC")

for(s in statez) {
  for(i in 1:n_simulations) {
    catch<-read_dta(paste0("E:/Lou_projects/flukeRDM/flukeRDM_iterative_data/archive/proj_catch_draws/proj_catch_draws_",s, "_", i,".dta")) %>%
      dplyr::mutate(date_parsed = parse_date_any(date),
                    month=data.table::month(date_parsed)) %>%
      dplyr::select(-date_num, -date)
    write_fst(catch, paste0("E:/Lou_projects/flukeRDM/flukeRDM_iterative_data/archive/proj_catch_draws/proj_catch_draws_",s, "_", i,".fst"))
    
  }
}


##################### STEP 3 #####################
# Run the projection algorithm. This algorithm pulls in population-adjusted catch-at-length distributions and allocates 
# fish discarded as harvest or vice versa in proportion to how they were allocated in the calibration. 
source(file.path(code_cd, "predict_rec_catch_final.R"))











