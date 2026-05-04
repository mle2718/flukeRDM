
options(scipen = 999)

packages <- c("tidyr",  "magrittr", "tidyverse", "reshape2", "splitstackshape","doBy","WriteXLS","Rcpp",
              "ggplot2","dplyr","rlist","fitdistrplus","MASS","psych","rgl","copula","VineCopula","scales",
              "univariateML","logspline","readr","data.table","conflicted", "readxl", "writexl", "fs",
              "purrr", "readr", "here","plyr" , "furrr", "profvis", "future", "magrittr", "feather", "RStata", "haven")

# Install only those not already installed
# installed <- packages %in% rownames(installed.packages())
# if (any(!installed)) {
#   install.packages(packages[!installed])
# }
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


#Lou's repos
iterative_input_data_cd="E:/Lou_projects/flukeRDM/flukeRDM_iterative_data"
input_data_cd="C:/Users/andrew.carr-harris/Desktop/MRIP_data_2025"

st<-"MA"
dr<-1
system.time({
  
state_list<-list()
#for (st in c("MA", "RI", "CT", "NY", "DE", "MD", "VA", "NC")){
  for (st in c("MA")){
    
  predictions_list<-list()
  for (dr in 1:10){
    
    source("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Code/test_code/predict_rec_catch_data_read_test2.R")
    source("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Code/sim/predict_rec_catch_functions.R")
    source("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Code/test_code/predict_rec_catch_test2.R")
    
    predictions_list[[dr]]<-predictions
    
  }
  prediction_draws <- dplyr::bind_rows(predictions_list)
  
  #write_csv(prediction_draws, file.path(paste0("C:/Users/andrew.carr-harris/Desktop/MRIP_data_2025/rdm testing data/SQ_runs_10_20/SQ_new_", st, ".csv")))
  
}

})
mean(prediction_draws[metric=="change_CS" & mode=="all modes"]$value)
mean(prediction_draws[metric=="n_trips_alt" & mode=="all modes"]$value)
mean(prediction_draws[metric=="n_trips_base" & mode=="all modes"]$value)
mean(prediction_draws[metric=="tot_keep_bsb_new" & mode=="all modes"]$value)
mean(prediction_draws[metric=="tot_keep_bsb_base" & mode=="all modes"]$value)
mean(prediction_draws[metric=="tot_keep_bsb_new" & mode=="all modes"]$value)
mean(prediction_draws[metric=="tot_cat_bsb_base" & mode=="all modes"]$value)


sum(prediction_draws[metric=="tot_keep_sf_new" & mode=="all modes"]$value)


