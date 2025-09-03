
options(scipen = 999)

packages <- c("tidyr",  "magrittr", "tidyverse", "reshape2", "splitstackshape","doBy","WriteXLS","Rcpp",
                 "ggplot2","dplyr","rlist","fitdistrplus","MASS","psych","rgl","copula","VineCopula","scales",
                 "univariateML","logspline","readr","data.table","conflicted", "readxl", "writexl", "fs",
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

#There are four folders needed::
#input data - contains all the MRIP, biological data, angler characteristics data, as well as some data generated in the simulation
#code - contains all the model code
#output_data - this folder is empty to begin with. It stores final simulation output
#iterative_data -this folder is empty to begin with. It compiles data generated in the simulation

#Need to ensure that the globals below are set up in both this file and the stata model_wrapper.do file. 


#Set up R globals for input/output data and code scripts
code_cd=here("Code", "sim")
input_data_cd="C:/Users/andrew.carr-harris/Desktop/MRIP_data_2025"
iterative_input_data_cd="E:/Lou's projects/flukeRDM/flukeRDM_iterative_data"

###################################################
###############Pre-sim Stata code##################
###################################################

#Stata code extracts and prepares the data needed for the simulation

#Connect Rstudio to Stata
#options("RStata.StataPath" = "\"C:\\Program Files\\Stata17\\StataMP-64\"")
#options("RStata.StataVersion" = 17)

#Set number of original draws. We create 125 (in case some don't converge in the calibration), but only use 100 for the final run. Choose a lot fewer for test runs
n_simulations<-125

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

#Notes:

#Simulation stratum are the groups in which we allocate and simulate choice occasions.
#For the 2025 SFSBSB RDM, the stratum is the combination of mode (pr/fh/sh) and state 

#Projection results are based on 100 iterations of the model. In each iteration we pull 
#in new distributions of catch-per-trip, directed fishing effort, projected catch-at-length, 
#and angler preferences. I calibrate the model with 125 iterations, some of which are 
#excluded after Step 2. From the pool of remaining iterations, I use the first 100 in the projection.

#Prior to running the model, transfer the catch_draw files from .csv to .feather to reduce computing time
statez <- c("MA", "RI", "CT", "NY", "NJ", "DE", "MD", "VA", "NC")

for(s in statez) {
  
  dtrip0<-read.csv(file.path(iterative_input_data_cd, paste0("directed_trips_calibration_", s,".csv")))
  write_feather(dtrip0, file.path(iterative_input_data_cd, paste0("directed_trips_calibration_", s,".feather")))

  for(i in 1:n_simulations) {
  catch<-read_dta(file.path(input_data_cd, paste0("calib_catch_draws_",s, "_", i,".dta")))
  write_feather(catch, file.path(iterative_input_data_cd, paste0("calib_catch_draws_",s, "_", i,".feather")))
  
  # make fake projection draws
  # write_feather(catch, file.path(iterative_input_data_cd, paste0("projected_catch_draws_",s, "_", i,".feather")))
  
}
}



##################### STEP 1 #####################
# Run the calibration algorithm to determine the difference between model-based harvest and MRIP-based harvest. 
# I do this for each stratum and each stratum's 125 draws of MRIP trips/catch/harvest.
# This code retains for each stratum the percent/absolute difference between model-based harvest and MRIP-based harvest by species. 

MRIP_comparison = read_dta(file.path(iterative_input_data_cd,"simulated_catch_totals.dta")) %>% 
      dplyr::rename(estimated_trips=tot_dtrip_sim, 
              sf_catch=tot_sf_cat_sim, 
              bsb_catch=tot_bsb_cat_sim, 
              scup_catch=tot_scup_cat_sim, 
              sf_keep=tot_sf_keep_sim, 
              bsb_keep=tot_bsb_keep_sim, 
              scup_keep=tot_scup_keep_sim,
              sf_rel=tot_sf_rel_sim, 
              bsb_rel=tot_bsb_rel_sim, 
              scup_rel=tot_scup_rel_sim) 

#Files needed:

#Scripts needed:


source(file.path(code_cd,"calibrate_rec_catch0.R"))

#Output files: 
#calibration_comparison.feather




##################### STEP 2 #####################
#Now, run each stratum's calibration simulation again, but this time allocate discards to harvest, 
#or harvest to discards, until the difference between model-based harvest and MRIP-based harvest 
#is within abs(5%) or <500 fish. 

#If a reallocation of discards to harvest is needed, I reallocate h* percent of all released 
# fish that are between [(min. size - 3 inches), min.size] as harvest.

#If a reallocation of harvest to discards is needed, I reallocate h* percent of all harvested fish as discards

#Note that in some iterations, the difference in harvest between the model and MRIP from Step 1 is too large relative to the number of 
#fish discarded in the model. So even if I allocate all discards as harvest, the percent difference in harvest
#between the model and MRIP will not be within abs(5%) or <500 fish. The code in Step 2 identifies and drops these iterations.

#This script saves calibration output, as well as the proportion of choice occasions in which we reallocate discards as harvest, 
#or vice versa. 

#Files needed:

#Scripts needed:
#calibration_catch_weights.R - can be commented out to save time if calibration catch weight are not needed.

source(file.path(code_cd,"calibration routine.R"))

#Output files: 
#calibration_comparison.rds
#calibration_catch_weights_cm.xlsx
#paste0("pds_new_", i,".rds")), where i is an indicator for a domain-draw combination
#paste0("costs_", i,".rds"))), where i is an indicator for a domain-draw combination




##################### STEP 3 #####################
#Run the projection algorithm. This algorithm pulls in population-adjusted catch-at-length distributions and allocates 
#fish discarded as harvest or vice versa in proportion to how they were allocated in the calibration. So for example, if in the 
#Step 2 we find that for draw 10 in the pr_closed domain, we needed to allocated fish discarded as harvest for 40% of choice occasions, 
#then in draw 10 projection of the pr_closed domain, we will also allocate fish discarded as harvest for 40% of the choice occasions. 

#New files needed:
#projected_CaL_cod_hadd_cm.csv - stock-adjusted catch-at-length
#calibration_comparison.rds - gives the proportions of trips to allocate harvest as release or vice versa
#costs_MODE_SEASON.feather - gives baseline variables and baseline catch levels. These are held constant in the projections. 

source(file.path(code_cd, "predict_rec_catch_season3_new1.R"))

#Save the output
write_xlsx(output2, file.path(output_data_cd, "model_results.xlsx"))

#Output files: 
# RDM_predictions.xlsx - output by mode, season, and draw









