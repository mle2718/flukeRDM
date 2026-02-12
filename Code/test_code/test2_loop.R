
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


#st<-"NJ"
#dr<-1
#Lou's repos
iterative_input_data_cd="E:/Lou_projects/flukeRDM/flukeRDM_iterative_data"
input_data_cd="C:/Users/andrew.carr-harris/Desktop/MRIP_data_2025"


# Status quo regs
state_list<-list()
#for (st in c("MA", "RI", "CT", "NY", "NJ", "DE", "MD", "VA", "NC")){
  for (st in c("CT")){
    
  predictions_list<-list()
  for (dr in 1:5){
    source("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Code/sim/predict_rec_catch_data_read.R")
    dr<-1
    #### directed trips ####
    directed_trips<-feather::read_feather(file.path(iterative_input_data_cd, paste0("directed_trips_calibration_new/directed_trips_calibration_new_", "CT", ".feather"))) %>% 
      tibble::tibble() %>%
      dplyr::select(mode, date, draw, bsb_bag, bsb_min, fluke_bag,fluke_min, scup_bag, scup_min,
                    bsb_bag_y2, bsb_min_y2, fluke_bag_y2,fluke_min_y2, scup_bag_y2, scup_min_y2) %>% 
      dplyr::mutate(date_adj = lubridate::dmy(date), 
                    date_adj = lubridate::yday(date_adj), 
                    date_adj = dplyr::case_when(date_adj > 60 ~ date_adj -1, TRUE ~ date_adj)) %>% 
      dplyr::filter(draw==dr & mode=="pr")
    
    if (exists("SFct_seas1_op")) {
      directed_trips<- directed_trips %>%
        dplyr::mutate(#Summer Flounder
          fluke_bag_y2=dplyr::case_when(date_adj >= yday(ymd(SFct_seas1_op)) & date_adj <= yday(ymd(SFct_seas1_cl)) ~ as.numeric(SFct_1_bag), TRUE ~ 0), 
          fluke_bag_y2=dplyr::case_when(date_adj >= yday(ymd(SFct_seas2_op)) & date_adj <= yday(ymd(SFct_seas2_cl)) ~ as.numeric(SFct_2_bag), TRUE ~ fluke_bag_y2), 
          fluke_min_y2=dplyr::case_when(date_adj >= yday(ymd(SFct_seas1_op)) & date_adj <= yday(ymd(SFct_seas1_cl)) ~ as.numeric(SFct_1_len) * 2.54, TRUE ~ 254),
          fluke_min_y2=dplyr::case_when(date_adj >= yday(ymd(SFct_seas2_op)) & date_adj <= yday(ymd(SFct_seas2_cl)) ~ as.numeric(SFct_2_len) * 2.54, TRUE ~ fluke_min_y2))
    } else {
      directed_trips<- directed_trips %>%
        dplyr::mutate(
          fluke_bag_y2=dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SFctFH_seas1_op)) & date_adj <= yday(ymd(SFctFH_seas1_cl)) ~ as.numeric(SFctFH_1_bag), TRUE ~ 0), 
          fluke_bag_y2=dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SFctPR_seas1_op)) & date_adj <= yday(ymd(SFctPR_seas1_cl)) ~ as.numeric(SFctPR_1_bag), TRUE ~ fluke_bag_y2), 
          fluke_bag_y2=dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SFctSH_seas1_op)) & date_adj <= yday(ymd(SFctSH_seas1_cl)) ~ as.numeric(SFctSH_1_bag), TRUE ~ fluke_bag_y2),
          
          fluke_bag_y2=dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SFctFH_seas2_op)) & date_adj <= yday(ymd(SFctFH_seas2_cl)) ~ as.numeric(SFctFH_2_bag), TRUE ~ fluke_bag_y2),
          fluke_bag_y2=dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SFctPR_seas2_op)) & date_adj <= yday(ymd(SFctPR_seas2_cl)) ~ as.numeric(SFctPR_2_bag), TRUE ~ fluke_bag_y2),
          fluke_bag_y2=dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SFctSH_seas2_op)) & date_adj <= yday(ymd(SFctSH_seas2_cl)) ~ as.numeric(SFctSH_2_bag), TRUE ~ fluke_bag_y2), 
          
          fluke_min_y2=dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SFctFH_seas1_op)) & date_adj <= yday(ymd(SFctFH_seas1_cl)) ~ as.numeric(SFctFH_1_len) * 2.54, TRUE ~ 254), 
          fluke_min_y2=dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SFctPR_seas1_op)) & date_adj <= yday(ymd(SFctPR_seas1_cl)) ~ as.numeric(SFctPR_1_len) * 2.54, TRUE ~ fluke_min_y2), 
          fluke_min_y2=dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SFctSH_seas1_op)) & date_adj <= yday(ymd(SFctSH_seas1_cl)) ~ as.numeric(SFctSH_1_len) * 2.54, TRUE ~ fluke_min_y2),
          
          fluke_min_y2=dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SFctFH_seas2_op)) & date_adj <= yday(ymd(SFctFH_seas2_cl)) ~ as.numeric(SFctFH_2_len) * 2.54, TRUE ~ fluke_min_y2),
          fluke_min_y2=dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SFctPR_seas2_op)) & date_adj <= yday(ymd(SFctPR_seas2_cl)) ~ as.numeric(SFctPR_2_len) * 2.54, TRUE ~ fluke_min_y2),
          fluke_min_y2=dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SFctSH_seas2_op)) & date_adj <= yday(ymd(SFctSH_seas2_cl)) ~ as.numeric(SFctSH_2_len) * 2.54, TRUE ~ fluke_min_y2))
      
    }
    
    
    if (exists("BSBct_seas1_op")) {
      directed_trips<- directed_trips %>%
        dplyr::mutate(# Black Sea Bass Bag Limit
          bsb_bag_y2=dplyr::case_when(date_adj >= yday(ymd(BSBct_seas1_op)) & date_adj <= yday(ymd(BSBct_seas1_cl)) ~ as.numeric(BSBct_1_bag), TRUE ~ 0), 
          bsb_bag_y2=dplyr::case_when(date_adj >= yday(ymd(BSBct_seas2_op)) & date_adj <= yday(ymd(BSBct_seas2_cl)) ~ as.numeric(BSBct_2_bag), TRUE ~ bsb_bag_y2),
          bsb_min_y2=dplyr::case_when(date_adj >= yday(ymd(BSBct_seas1_op)) & date_adj <= yday(ymd(BSBct_seas1_cl)) ~ as.numeric(BSBct_1_len) * 2.54, TRUE ~ 254), 
          bsb_min_y2=dplyr::case_when(date_adj >= yday(ymd(BSBct_seas2_op)) & date_adj <= yday(ymd(BSBct_seas2_cl)) ~ as.numeric(BSBct_2_len) * 2.54, TRUE ~ bsb_min_y2))
    } else {
      directed_trips<- directed_trips %>%
        dplyr::mutate(# Black Sea Bass Bag Limit by Mode
          bsb_bag_y2=dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(BSBctFH_seas1_op)) & date_adj <= yday(ymd(BSBctFH_seas1_cl)) ~ as.numeric(BSBctFH_1_bag), TRUE ~ 0), 
          bsb_bag_y2=dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(BSBctPR_seas1_op)) & date_adj <= yday(ymd(BSBctPR_seas1_cl)) ~ as.numeric(BSBctPR_1_bag), TRUE ~ bsb_bag_y2), 
          bsb_bag_y2=dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(BSBctSH_seas1_op)) & date_adj <= yday(ymd(BSBctSH_seas1_cl)) ~ as.numeric(BSBctSH_1_bag), TRUE ~ bsb_bag_y2),
          bsb_bag_y2=dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(BSBctFH_seas2_op)) & date_adj <= yday(ymd(BSBctFH_seas2_cl)) ~ as.numeric(BSBctFH_2_bag), TRUE ~ bsb_bag_y2), 
          bsb_bag_y2=dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(BSBctPR_seas2_op)) & date_adj <= yday(ymd(BSBctPR_seas2_cl)) ~ as.numeric(BSBctPR_2_bag), TRUE ~ bsb_bag_y2), 
          bsb_bag_y2=dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(BSBctSH_seas2_op)) & date_adj <= yday(ymd(BSBctSH_seas2_cl)) ~ as.numeric(BSBctSH_2_bag), TRUE ~ bsb_bag_y2),
          
          # Black Sea Bass Minimum Length by Mode
          bsb_min_y2=dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(BSBctFH_seas1_op)) & date_adj <= yday(ymd(BSBctFH_seas1_cl)) ~ as.numeric(BSBctFH_1_len) * 2.54, TRUE ~ 254), 
          bsb_min_y2=dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(BSBctPR_seas1_op)) & date_adj <= yday(ymd(BSBctPR_seas1_cl)) ~ as.numeric(BSBctPR_1_len) * 2.54, TRUE ~ bsb_min_y2), 
          bsb_min_y2=dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(BSBctSH_seas1_op)) & date_adj <= yday(ymd(BSBctSH_seas1_cl)) ~ as.numeric(BSBctSH_1_len) * 2.54, TRUE ~ bsb_min_y2),
          bsb_min_y2=dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(BSBctFH_seas2_op)) & date_adj <= yday(ymd(BSBctFH_seas2_cl)) ~ as.numeric(BSBctFH_2_len) * 2.54, TRUE ~ bsb_min_y2), 
          bsb_min_y2=dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(BSBctPR_seas2_op)) & date_adj <= yday(ymd(BSBctPR_seas2_cl)) ~ as.numeric(BSBctPR_2_len) * 2.54, TRUE ~ bsb_min_y2), 
          bsb_min_y2=dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(BSBctSH_seas2_op)) & date_adj <= yday(ymd(BSBctSH_seas2_cl)) ~ as.numeric(BSBctSH_2_len) * 2.54, TRUE ~ bsb_min_y2))
    }
    
    
    
    directed_trips<- directed_trips %>%  
      dplyr::mutate(
        fluke_bag_y2=dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SFctFH_seas3_op)) & date_adj <= yday(ymd(SFctFH_seas3_cl)) ~ as.numeric(SFctFH_3_bag), TRUE ~ fluke_bag_y2),
        fluke_bag_y2=dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SFctPR_seas3_op)) & date_adj <= yday(ymd(SFctPR_seas3_cl)) ~ as.numeric(SFctPR_3_bag), TRUE ~ fluke_bag_y2),
        fluke_bag_y2=dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SFctSH_seas3_op)) & date_adj <= yday(ymd(SFctSH_seas3_cl)) ~ as.numeric(SFctSH_3_bag), TRUE ~ fluke_bag_y2), 
        
        fluke_min_y2=dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SFctFH_seas3_op)) & date_adj <= yday(ymd(SFctFH_seas3_cl)) ~ as.numeric(SFctFH_3_len) * 2.54, TRUE ~ fluke_min_y2),
        fluke_min_y2=dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SFctPR_seas3_op)) & date_adj <= yday(ymd(SFctPR_seas3_cl)) ~ as.numeric(SFctPR_3_len) * 2.54, TRUE ~ fluke_min_y2),
        fluke_min_y2=dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SFctSH_seas3_op)) & date_adj <= yday(ymd(SFctSH_seas3_cl)) ~ as.numeric(SFctSH_3_len) * 2.54, TRUE ~ fluke_min_y2), 
        
        
        bsb_bag_y2=dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(BSBctFH_seas3_op)) & date_adj <= yday(ymd(BSBctFH_seas3_cl)) ~ as.numeric(BSBctFH_3_bag), TRUE ~ bsb_bag_y2), 
        bsb_bag_y2=dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(BSBctPR_seas3_op)) & date_adj <= yday(ymd(BSBctPR_seas3_cl)) ~ as.numeric(BSBctPR_3_bag), TRUE ~ bsb_bag_y2), 
        bsb_bag_y2=dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(BSBctSH_seas3_op)) & date_adj <= yday(ymd(BSBctSH_seas3_cl)) ~ as.numeric(BSBctSH_3_bag), TRUE ~ bsb_bag_y2), 
        
        bsb_min_y2=dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(BSBctFH_seas3_op)) & date_adj <= yday(ymd(BSBctFH_seas3_cl)) ~ as.numeric(BSBctFH_3_len) * 2.54, TRUE ~ bsb_min_y2), 
        bsb_min_y2=dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(BSBctPR_seas3_op)) & date_adj <= yday(ymd(BSBctPR_seas3_cl)) ~ as.numeric(BSBctPR_3_len) * 2.54, TRUE ~ bsb_min_y2), 
        bsb_min_y2=dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(BSBctSH_seas3_op)) & date_adj <= yday(ymd(BSBctSH_seas3_cl)) ~ as.numeric(BSBctSH_3_len) * 2.54, TRUE ~ bsb_min_y2),
        
        scup_bag_y2=dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPctFH_seas1_op)) & date_adj <= yday(ymd(SCUPctFH_seas1_cl)) ~ as.numeric(SCUPctFH_1_bag), TRUE ~ 0), 
        scup_min_y2=dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPctFH_seas1_op)) & date_adj <= yday(ymd(SCUPctFH_seas1_cl)) ~ as.numeric(SCUPctFH_1_len) * 2.54, TRUE ~ 254),
        scup_bag_y2=dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPctFH_seas2_op)) & date_adj <= yday(ymd(SCUPctFH_seas2_cl)) ~ as.numeric(SCUPctFH_2_bag), TRUE ~ scup_bag_y2),
        scup_min_y2=dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPctFH_seas2_op)) & date_adj <= yday(ymd(SCUPctFH_seas2_cl)) ~ as.numeric(SCUPctFH_2_len) * 2.54, TRUE ~ scup_min_y2),
        scup_bag_y2=dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPctFH_seas3_op)) & date_adj <= yday(ymd(SCUPctFH_seas3_cl)) ~ as.numeric(SCUPctFH_3_bag), TRUE ~ scup_bag_y2),
        scup_min_y2=dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPctFH_seas3_op)) & date_adj <= yday(ymd(SCUPctFH_seas3_cl)) ~ as.numeric(SCUPctFH_3_len) * 2.54, TRUE ~ scup_min_y2),
        scup_bag_y2=dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPctFH_seas4_op)) & date_adj <= yday(ymd(SCUPctFH_seas4_cl)) ~ as.numeric(SCUPctFH_4_bag), TRUE ~ scup_bag_y2),
        scup_min_y2=dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPctFH_seas4_op)) & date_adj <= yday(ymd(SCUPctFH_seas4_cl)) ~ as.numeric(SCUPctFH_4_len) * 2.54, TRUE ~ scup_min_y2),
        
        scup_bag_y2=dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SCUPctPR_seas1_op)) & date_adj <= yday(ymd(SCUPctPR_seas1_cl)) ~ as.numeric(SCUPctPR_1_bag), TRUE ~ scup_bag_y2),
        scup_min_y2=dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SCUPctPR_seas1_op)) & date_adj <= yday(ymd(SCUPctPR_seas1_cl)) ~ as.numeric(SCUPctPR_1_len) * 2.54, TRUE ~ scup_min_y2),
        scup_bag_y2=dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SCUPctPR_seas2_op)) & date_adj <= yday(ymd(SCUPctPR_seas2_cl)) ~ as.numeric(SCUPctPR_2_bag), TRUE ~ scup_bag_y2),
        scup_min_y2=dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SCUPctPR_seas2_op)) & date_adj <= yday(ymd(SCUPctPR_seas2_cl)) ~ as.numeric(SCUPctPR_2_len) * 2.54, TRUE ~ scup_min_y2),
        
        scup_bag_y2=dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SCUPctSH_seas1_op)) & date_adj <= yday(ymd(SCUPctSH_seas1_cl)) ~ as.numeric(SCUPctSH_1_bag), TRUE ~ scup_bag_y2),
        scup_min_y2=dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SCUPctSH_seas1_op)) & date_adj <= yday(ymd(SCUPctSH_seas1_cl)) ~ as.numeric(SCUPctSH_1_len) * 2.54, TRUE ~ scup_min_y2),
        scup_bag_y2=dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SCUPctSH_seas2_op)) & date_adj <= yday(ymd(SCUPctSH_seas2_cl)) ~ as.numeric(SCUPctSH_2_bag), TRUE ~ scup_bag_y2),
        scup_min_y2=dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SCUPctSH_seas2_op)) & date_adj <= yday(ymd(SCUPctSH_seas2_cl)) ~ as.numeric(SCUPctSH_2_len) * 2.54, TRUE ~ scup_min_y2))

        source("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Code/sim/predict_rec_catch_functions.R")
    
    library(data.table)
    
    #Convert key data frames to data.table format early:
    data.table::setDT(directed_trips)
    data.table::setDT(catch_data)
    data.table::setDT(calib_comparison)
    data.table::setDT(sf_size_data)
    data.table::setDT(bsb_size_data)
    data.table::setDT(scup_size_data)
    
    #Set up constants (unchanged):
    mode_draw <- c("sh", "pr", "fh")
    
    #Step 2: Reorganize calibration parameters#
    calib_lookup <- calib_comparison %>%
      dplyr::select(mode, species, rel_to_keep, keep_to_rel, 
                    p_rel_to_keep, p_keep_to_rel, 
                    prop_sub_kept, prop_legal_rel) %>%
      tidyr::pivot_wider(
        names_from = species,
        values_from = c(rel_to_keep, keep_to_rel, p_rel_to_keep, p_keep_to_rel, prop_sub_kept, prop_legal_rel),
        names_glue = "{.value}_{species}"
      )
    
    data.table::setDT(calib_lookup)
    data.table::setkey(calib_lookup, mode)
    
    ## Run for all modes + aggregate  - summer flounder 
    results_list <- lapply(mode_draw, simulate_mode_sf, calib_lookup = calib_lookup,
                           sf_size_data = sf_size_data, catch_data = catch_data)
    
    sf_trip_data <- data.table::rbindlist(lapply(results_list, `[[`, "trip_data"))
    data.table::setkey(sf_trip_data, domain2)
    
    zero_catch_sf <- data.table::rbindlist(lapply(results_list, `[[`, "zero_catch"))
    
    size_data_sf <- data.table::rbindlist(lapply(results_list, `[[`, "size_data"), fill=TRUE)
    
    # Replace NA=0 in all columns
    size_data_sf <- size_data_sf %>%
      dplyr::mutate(dplyr::across(everything(), ~tidyr::replace_na(., 0)))
    
    ## Run for all modes + aggregate  - black sea bass 
    results_list <- lapply(mode_draw, simulate_mode_bsb, calib_lookup = calib_lookup, 
                           bsb_size_data = bsb_size_data, catch_data = catch_data)
    
    bsb_trip_data <- data.table::rbindlist(lapply(results_list, `[[`, "trip_data")) %>% 
      dplyr::select(-date, -mode, -catch_draw, -tripid)
    
    data.table::setkey(bsb_trip_data, domain2)
    
    zero_catch_bsb <- data.table::rbindlist(lapply(results_list, `[[`, "zero_catch"))
    
    size_data_bsb <- data.table::rbindlist(lapply(results_list, `[[`, "size_data"), fill=TRUE)
    
    # Replace NA=0 in all columns
    size_data_bsb <- size_data_bsb %>%
      dplyr::mutate(dplyr::across(everything(), ~tidyr::replace_na(., 0)))
    
    ## Run for all modes + aggregate  - scup 
    results_list <- lapply(mode_draw, simulate_mode_scup, calib_lookup = calib_lookup, 
                           scup_size_data = scup_size_data, catch_data = catch_data)
    
    scup_trip_data <- data.table::rbindlist(lapply(results_list, `[[`, "trip_data")) %>% 
      dplyr::select(-date, -mode, -catch_draw, -tripid)
    
    data.table::setkey(scup_trip_data, domain2)
    
    zero_catch_scup <- data.table::rbindlist(lapply(results_list, `[[`, "zero_catch"))
    
    size_data_scup <- data.table::rbindlist(lapply(results_list, `[[`, "size_data"), fill=TRUE)
    
    # Replace NA=0 in all columns
    size_data_scup <- size_data_scup %>%
      dplyr::mutate(dplyr::across(everything(), ~tidyr::replace_na(., 0)))
    
    #merge the trip data
    # Join summer flounder (sf) and black sea bass (bsb) on domain2
    trip_data_a <- merge(sf_trip_data, bsb_trip_data, by = "domain2", all = TRUE)
    
    # Join the result with scup data on domain2
    trip_data <- merge(trip_data_a, scup_trip_data, by = "domain2", all = TRUE)
    
    trip_data[is.na(trip_data)] <- 0
    
    # sf_catch_check<-sum(sf_trip_data$tot_keep_sf_new+sf_trip_data$tot_rel_sf_new)
    # bsb_catch_check<-sum(bsb_trip_data$tot_keep_bsb_new+bsb_trip_data$tot_rel_bsb_new)
    # scup_catch_check<-sum(scup_trip_data$tot_keep_scup_new+scup_trip_data$tot_rel_scup_new)
    
    rm(trip_data_a)
    
    
    # Convert to data.table
    data.table::setDT(size_data_sf)
    data.table::setDT(size_data_bsb)
    data.table::setDT(size_data_scup)
    data.table::setDT(zero_catch_sf)
    data.table::setDT(zero_catch_bsb)
    data.table::setDT(zero_catch_scup)
    
    # First merge sf and bsb
    length_temp <- merge(size_data_sf, size_data_bsb,
                         by = c("date", "mode", "tripid", "catch_draw"),
                         all = TRUE)
    
    # Then merge the result with scup
    length_data <- merge(length_temp, size_data_scup,
                         by = c("date", "mode", "tripid", "catch_draw"),
                         all = TRUE)
    
    #First merge sf and bsb zero catches
    zero_catch_temp<- merge(zero_catch_sf, zero_catch_bsb,
                            by = c("date", "mode", "tripid", "catch_draw"),
                            all = TRUE)
    
    # Then merge the zero catches result with scup
    zero_catch_check <- merge(zero_catch_temp, zero_catch_scup,
                              by = c("date", "mode", "tripid", "catch_draw"),
                              all = TRUE)[
                                tot_keep_sf_new == 0 & tot_rel_sf_new == 0 & 
                                  tot_keep_bsb_new == 0 & tot_rel_bsb_new == 0 & 
                                  tot_keep_scup_new == 0 & tot_rel_scup_new == 0, 
                                .(date, mode, tripid, catch_draw)
                              ]
    
    
    # Bind rows (rbindlist is faster and more memory-efficient)
    length_data <- data.table::rbindlist(list(length_data, zero_catch_check), fill = TRUE) 
    
    
    # Replace NA values with 0 again (if necessary)
    length_data[is.na(length_data)] <- 0
    
    rm(zero_catch_sf,zero_catch_bsb,zero_catch_scup,zero_catch_check, length_temp, zero_catch_temp)
    
    length_data<-data.table::as.data.table(length_data) 
    
    
    # If there is catch of only sf 
    # if(sf_catch_check !=0 & bsb_catch_check==0 & scup_catch_check==0){
    
    #     dplyr::select("period2","tripid", "catch_draw") %>% 
    #     dplyr::mutate(keep_had_1=0, release_had_1=0)
    #   
    #   length_data <- keep_release_cod %>%
    #     dplyr::full_join(keep_release_hadd, by = c("period2","tripid", "catch_draw"))
    #   
    #   length_data[is.na(length_data)] <- 0
    
    #   
    #   
    # }
    # 
    # #If there is catch of only cod 
    # if(cod_catch_check !=0 & had_catch_check==0){
    #   
    #   keep_release_hadd<-trip_data %>% 
    #     dplyr::select("period2","tripid", "catch_draw") %>% 
    #     dplyr::mutate(keep_had_1=0, release_had_1=0)
    #   
    #   length_data <- keep_release_cod %>%
    #     dplyr::full_join(keep_release_hadd, by = c("period2","tripid", "catch_draw"))
    #   
    #   length_data[is.na(length_data)] <- 0
    #   
    # }
    
    # Convert to data.table
    data.table::setDT(trip_data)
    data.table::setDT(length_data)
    data.table::setDT(base_outcomes)
    
    # Mutate efficiently
    trip_data[, date_parsed := lubridate::dmy(date)]
    trip_data[, `:=`(
      tot_cat_scup_new = tot_keep_scup_new + tot_rel_scup_new,
      tot_cat_bsb_new  = tot_keep_bsb_new + tot_rel_bsb_new,
      tot_cat_sf_new   = tot_keep_sf_new + tot_rel_sf_new,
      date = NULL
      
    )]
    
    length_data[, date_parsed := lubridate::dmy(date)][, date := NULL]
    
    # Merge
    trip_data <- trip_data[base_outcomes, on = .(date_parsed, mode, tripid, catch_draw), nomatch = 0L]
    
    trip_data[, domain2 := NULL]
    
    print("before remove catch_data")
    rm(sf_trip_data, scup_trip_data, bsb_trip_data, 
       size_data_sf, size_data_bsb,size_data_scup, 
       base_outcomes, catch_data)
    print("after remove catch_data")
    #trip_data$NJ_dummy<-case_when(s=="NJ"~1, TRUE~0)
    
    # compute utility/choice probabilites/welfare
    # Convert to data.table if not already
    data.table::setDT(trip_data)
    
    # Precompute square roots once
    trip_data[, `:=`(
      sqrt_keep_sf_new = sqrt(tot_keep_sf_new),
      sqrt_rel_sf_new = sqrt(tot_rel_sf_new),
      sqrt_keep_bsb_new = sqrt(tot_keep_bsb_new),
      sqrt_rel_bsb_new = sqrt(tot_rel_bsb_new),
      sqrt_keep_sf_base = sqrt(tot_keep_sf_base),
      sqrt_rel_sf_base = sqrt(tot_rel_sf_base),
      sqrt_keep_bsb_base = sqrt(tot_keep_bsb_base),
      sqrt_rel_bsb_base = sqrt(tot_rel_bsb_base),
      sqrt_cat_scup_new = sqrt(tot_cat_scup_new),
      sqrt_cat_scup_base = sqrt(tot_cat_scup_base)
    )]
    
    # Compute vA and v0
    trip_data[, `:=`(
      vA = beta_sqrt_sf_keep * sqrt_keep_sf_new +
        #beta_NJ_sf_keep*NJ_dummy +
        beta_sqrt_sf_release * sqrt_rel_sf_new +
        beta_sqrt_bsb_keep * sqrt_keep_bsb_new +
        beta_sqrt_bsb_release * sqrt_rel_bsb_new +
        beta_sqrt_sf_bsb_keep * (sqrt_keep_sf_new * sqrt_keep_bsb_new) +
        beta_sqrt_scup_catch * sqrt_cat_scup_new +
        beta_cost * cost,
      
      v0 = beta_sqrt_sf_keep * sqrt_keep_sf_base +
        #beta_NJ_sf_keep*NJ_dummy +
        beta_sqrt_sf_release * sqrt_rel_sf_base +
        beta_sqrt_bsb_keep * sqrt_keep_bsb_base +
        beta_sqrt_bsb_release * sqrt_rel_bsb_base +
        beta_sqrt_sf_bsb_keep * (sqrt_keep_sf_base * sqrt_keep_bsb_base) +
        beta_sqrt_scup_catch * sqrt_cat_scup_base +
        beta_cost * cost
    )]
    
    # remove the temp sqrt columns to save memory
    trip_data[, c("sqrt_keep_sf_new", "sqrt_rel_sf_new", "sqrt_keep_bsb_new", "sqrt_rel_bsb_new",
                  "sqrt_keep_sf_base", "sqrt_rel_sf_base", "sqrt_keep_bsb_base", "sqrt_rel_bsb_base",
                  "sqrt_cat_scup_new", "sqrt_cat_scup_base") := NULL]
    
    
    mean_trip_data <- trip_data %>% data.table::data.table() %>% 
      .[, group_index := .GRP, by = .(date_parsed, mode, catch_draw, tripid)]
    
    # expand the data to create two alternatives, representing the alternatives available in choice survey
    mean_trip_data <- mean_trip_data %>%
      dplyr::mutate(n_alt = rep(2,nrow(.))) %>%
      tidyr::uncount(n_alt) %>%
      dplyr::mutate(alt = rep(1:2,nrow(.)/2),
                    opt_out = ifelse(alt == 2, 1, 0))
    
    #Calculate the expected utility of alts 2 parameters of the utility function,
    #put the two values in the same column, exponentiate, and calculate their sum (vA_col_sum)
    
    data.table::setDT(mean_trip_data)
    
    # Filter only alt == 2 once, and calculate vA and v0
    mean_trip_data[alt == 2, c("vA", "v0") := .(
      beta_opt_out * opt_out +
        beta_opt_out_age * (age * opt_out) +
        beta_opt_out_avidity * (total_trips_12 * opt_out) 
    )]
    
    # Pre-compute exponential terms
    mean_trip_data[, `:=`(exp_vA = exp(vA), exp_v0 = exp(v0))]
    
    # Group by group_index and calculate probabilities and log-sums
    
    mean_trip_data[, `:=`(
      probA = exp_vA / sum(exp_vA),
      prob0 = exp_v0 / sum(exp_v0), 
      log_sum_alt = log(sum(exp_vA)),
      log_sum_base = log(sum(exp_v0)) 
    ), by = group_index]
    
    # Calculate consumer surplus 
    mean_trip_data[, `:=`(
      CS_base = log_sum_base / -beta_cost,
      CS_alt = log_sum_alt / -beta_cost
    )]
    
    # Calculate change consumer surplus 
    mean_trip_data[, `:=`(
      CV = CS_alt - CS_base
    )]
    
    # Get rid of things we don't need.
    mean_trip_data <- mean_trip_data %>% 
      dplyr::filter(alt==1) %>% 
      dplyr::select(-matches("beta")) %>% 
      dplyr::select(-"alt", -"opt_out", -"vA" , -"v0",-"exp_v0", -"exp_vA", 
                    -"cost", -"age", -"total_trips_12", -"catch_draw", -"group_index", 
                    -"log_sum_alt", -"log_sum_base", "tot_keep_sf_base",   "tot_rel_sf_base",  "tot_cat_sf_base", 
                    "tot_keep_bsb_base",  "tot_rel_bsb_base", "tot_cat_bsb_base",  
                    "tot_keep_scup_base","tot_rel_scup_base",  "tot_cat_scup_base") 
    
    all_vars<-c()
    all_vars <- names(mean_trip_data)[!names(mean_trip_data) %in% c("date_parsed","mode", "tripid")]
    
    #all_vars
    # average outcomes across draws
    mean_trip_data<-mean_trip_data  %>% data.table::as.data.table() %>%
      .[,lapply(.SD, mean), by = c("date_parsed","mode", "tripid"), .SDcols = all_vars]
    
    # multiply the average trip probability in the new scenario (probA) by each catch variable to get probability-weighted catch
    list_names <- c("tot_keep_sf_new",   "tot_rel_sf_new",  "tot_cat_sf_new", 
                    "tot_keep_bsb_new",  "tot_rel_bsb_new", "tot_cat_bsb_new",  
                    "tot_keep_scup_new","tot_rel_scup_new",  "tot_cat_scup_new")
    
    all_vars <- c(list_names)
    
    mean_trip_data <- mean_trip_data %>%
      data.table::as.data.table() %>%
      .[,as.vector(all_vars) := lapply(.SD, function(x) x * probA), .SDcols = all_vars] %>%
      .[]
    

    
    ## select the same number of choice occasions in the prediction year as in the calibration year
    # We will multiply each simulated choice equation by an appropriate expansion factor, 
    # then multiply this expansion factor by the projection-year calendar adjustment to account for
    # different numbers of weekend vs. weekday in the projection year versus the calibration
    ndraws = 50
    mean_trip_data<-mean_trip_data %>% 
      dplyr::left_join(n_choice_occasions, by = c("mode", "date_parsed")) %>% 
      dplyr::mutate(month = lubridate::month(date_parsed))  %>% 
      dplyr::mutate(dplyr::across(where(is.numeric), ~tidyr::replace_na(., 0))) %>%  #replace NAs for n_choice_occasions and estimated trips
      dplyr::left_join(calendar_adjustments, by = c("mode", "month")) %>% 
      dplyr::rename(n_choice_occasions0=n_choice_occasions, 
                    estimated_trips0=estimated_trips) %>% 
      dplyr::mutate(n_choice_occasions=n_choice_occasions0*expansion_factor,
                    expand=n_choice_occasions/ndraws) 
    
    
    #retain expansion factors by strata to multiply with length data 
    expansion_factors<-mean_trip_data %>% 
      dplyr::select("date_parsed","mode", "tripid", "expand", "probA")
    
    
    # Expand outcomes for projection year
    list_names <- c("tot_keep_sf_new",   "tot_rel_sf_new",  "tot_cat_sf_new", 
                    "tot_keep_bsb_new",  "tot_rel_bsb_new", "tot_cat_bsb_new",  
                    "tot_keep_scup_new","tot_rel_scup_new",  "tot_cat_scup_new", 
                    "probA", "CV", "prob0", "tot_keep_sf_base")
    
    all_vars <- c(list_names)
    
    mean_trip_data <- mean_trip_data %>%
      data.table::as.data.table() %>%
      .[,as.vector(all_vars) := lapply(.SD, function(x) x * expand), .SDcols = all_vars] %>%
      .[]
    
    
    #process length data 
    pattern_vars <- grep("^keep_(sf_|bsb_|scup_)[0-9.]*$|^release_(sf_|bsb_|scup_)[0-9.]*$", 
                         names(length_data), value = TRUE)
    
    length_data<-length_data  %>% data.table::as.data.table() %>%
      .[,lapply(.SD, mean), by = c("date_parsed","mode", "tripid"), .SDcols = pattern_vars]  
    
    length_data<-length_data %>% 
      dplyr::right_join(expansion_factors, b=c("date_parsed","mode", "tripid"))
    
    # mulitply length data first by the average probability, then by the expansion factor
    length_data <- length_data %>%
      data.table::as.data.table() %>%
      .[,as.vector(pattern_vars) := lapply(.SD, function(x) x * probA * expand), .SDcols = pattern_vars] %>%
      .[]  
    
    ## Compute welfare and predicted trips
    # Aggregate by mode
    mean_trip_data <- mean_trip_data %>%
      dplyr::rename(predicted_trips = probA, base_trips = prob0)
    
    # Ensure mean_trip_data is a data.table
    data.table::setDT(mean_trip_data)
    list_names <- c("CV","predicted_trips", "base_trips",
                    "tot_keep_sf_new",   "tot_rel_sf_new",  "tot_cat_sf_new", 
                    "tot_keep_bsb_new",  "tot_rel_bsb_new", "tot_cat_bsb_new",  
                    "tot_keep_scup_new","tot_rel_scup_new",  "tot_cat_scup_new", 
                    "tot_keep_sf_base",   "tot_rel_sf_base",  "tot_cat_sf_base", 
                    "tot_keep_bsb_base",  "tot_rel_bsb_base", "tot_cat_bsb_base",  
                    "tot_keep_scup_base","tot_rel_scup_base",  "tot_cat_scup_base")
    
    aggregate_trip_data_mode <- mean_trip_data[, lapply(.SD, sum), by = .(mode), .SDcols = list_names]
    
    # Aggregate for all modes
    aggregate_trip_data_allmodes <- mean_trip_data[, lapply(.SD, sum), .SDcols = list_names][
      , mode := "all modes"
    ]
    
    # Combine and reshape
    model_output1 <- data.table::rbindlist(list(aggregate_trip_data_mode, aggregate_trip_data_allmodes), use.names=TRUE)
    model_output1_long <- data.table::melt(
      model_output1,
      id.vars = c("mode"),   # keep these as identifiers
      measure.vars = c("CV", "predicted_trips", "base_trips", "tot_keep_sf_new",   "tot_rel_sf_new",  "tot_cat_sf_new", 
                       "tot_keep_bsb_new",  "tot_rel_bsb_new", "tot_cat_bsb_new",  
                       "tot_keep_scup_new","tot_rel_scup_new",  "tot_cat_scup_new"),
      variable.name = "metric",
      value.name = "value"
    )
    
    model_output1_long[, metric := data.table::fifelse(metric == "change_CS", "CV",
                                                       data.table::fifelse(metric == "n_trips_alt", "predicted trips", "metric"))]
    
    
    model_output1_long$species<-"NA"
    
    model_output1_long_base<-model_output1_long %>% 
      dplyr::filter(metric=="base_trips") %>% 
      dplyr::select(mode, value, species) %>% 
      dplyr::rename(value_base=value) 
    
    model_output1_long_new<-model_output1_long %>% 
      dplyr::filter(metric=="predicted_trips") %>% 
      dplyr::select(mode, value, species) %>% 
      dplyr::rename(value_new=value) %>% 
      dplyr::left_join(model_output1_long_base, by=c("mode", "species")) %>% 
      dplyr::mutate(additional_trips=value_new-value_base) %>% 
      dplyr::mutate(metric="additional_trips") %>% 
      dplyr::select(metric, mode, additional_trips, species) %>% 
      dplyr::rename(value=additional_trips)
    
    model_output1_long <- model_output1_long %>% 
      dplyr::bind_rows(model_output1_long_new)
    
    
    
    ## Compute catch weight estimates
    # Process length-frequency data
    ## Identify the length columns
    pattern_vars <- grep(
      "^keep_(sf_|bsb_|scup_)[0-9.]*$|^release_(sf_|bsb_|scup_)[0-9.]*$",
      names(length_data),
      value = TRUE
    )
    
    ## Select needed columns and add month
    length_data1 <- length_data[, .SD, .SDcols = c("date_parsed", "mode", pattern_vars)]
    length_data1[, month := lubridate::month(date_parsed)]
    
    ## Aggregate sums by mode + month
    length_data1 <- length_data1[, lapply(.SD, sum), 
                                 by = .(mode, month), 
                                 .SDcols = pattern_vars]
    
    ## MELT to long
    length_data1 <- data.table::melt(
      length_data1,
      id.vars = c("month", "mode"),
      variable.name = "Var",
      value.name = "number_at_length"
    )
    
    ## Split Var into keep_release, species, length
    length_data1[, c("keep_release", "species", "length") := data.table::tstrsplit(Var, "_", fixed = TRUE)]
    length_data1[, length := as.numeric(length)]
    
    ## Join with l_w_conversion
    data.table::setDT(l_w_conversion)
    length_data1 <- l_w_conversion[length_data1, on = .(month, species)]
    
    ## Compute weight
    length_data1[, weight := data.table::fcase(
      species == "scup", exp(ln_a + b * log(length)),
      species %chin% c("sf", "bsb"), a * length^b,
      default = NA_real_
    )]
    
    ## Convert to lbs
    length_data1[, weight := weight * 2.20462262185]
    
    ## Totals
    length_data1[, keep_weight := data.table::fifelse(keep_release == "keep", 
                                                      number_at_length * weight, 
                                                      0)]
    
    length_data1[, release_weight := data.table::fifelse(keep_release == "release", 
                                                         number_at_length * weight, 
                                                         0)]
    
    length_data1[, keep_numbers := data.table::fifelse(keep_release == "keep", 
                                                       number_at_length, 
                                                       0)]
    
    length_data1[, release_numbers := data.table::fifelse(keep_release == "release", 
                                                          number_at_length, 
                                                          0)]
    
    ## Discard mortality weight
    length_data1[, discmort_weight := data.table::fcase(
      keep_release == "release" & species == "sf", 0.10 * number_at_length * weight,
      keep_release == "release" & species == "scup", 0.15 * number_at_length * weight,
      keep_release == "release" & species == "bsb", 0.15 * number_at_length * weight,
      default = 0
    )]
    
    ## Discard mortality numbers
    length_data1[, discmort_number := data.table::fcase(
      keep_release == "release" & species == "sf", 0.10 * number_at_length,
      keep_release == "release" & species == "scup", 0.15 * number_at_length,
      keep_release == "release" & species == "bsb", 0.15 * number_at_length,
      default = 0
    )]
    
    ## Summarise by species, mode
    length_data1 <- length_data1[, .(
      keep_numbers = sum(keep_numbers),
      release_numbers = sum(release_numbers),
      keep_weight = sum(keep_weight),
      release_weight = sum(release_weight),
      discmort_weight = sum(discmort_weight),
      discmort_number = sum(discmort_number)
    ), by = .(species, mode)]
    
    
    length_data_long <- data.table::melt(
      length_data1,
      id.vars = c("species", "mode"),   # keep these as identifiers
      measure.vars = c("keep_numbers", "release_numbers",
                       "keep_weight", "release_weight",
                       "discmort_weight", "discmort_number"),
      variable.name = "metric",
      value.name = "value"
    )
    
    ## Remove NAs
    length_data_long <- length_data_long[!is.na(value)]
    
    ## Split and classify
    length_data_long_all <- length_data_long[, .(value = sum(value)),
                                             by = .(metric, species)]
    
    length_data_long_all[, mode := "all modes"]
    
    ## Final bind
    length_output <- data.table::rbindlist(list(length_data_long_all, length_data_long) ,
                                           use.names = TRUE,
                                           fill = TRUE
    )
    
    
    predictions <- data.table::rbindlist(
      list(length_output, model_output1_long),
      use.names = TRUE,
      fill = TRUE) %>% 
      dplyr::mutate(state = st, draw=dr)
    
    predictions<-predictions %>% 
      dplyr::mutate(state=st, draw=dr)
    
    #source("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Code/sim/predict_rec_catch_functions.R")
    #source("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Code/sim/predict_rec_catch.R")
    
    # source("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Code/test_code/predict_rec_catch_data_read_test2.R")
    # source("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Code/test_code/predict_rec_catch_functions_test2.R")
    # source("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Code/test_code/predict_rec_catch_test2.R")

    predictions_list[[dr]]<-predictions

  }
  prediction_draws <- dplyr::bind_rows(predictions_list)

  #write_csv(prediction_draws, file.path(paste0("C:/Users/andrew.carr-harris/Desktop/MRIP_data_2025/rdm testing data/check_1_7/SQ_new_", st, ".csv")))
  #write_csv(prediction_draws, file.path(paste0("C:/Users/andrew.carr-harris/Desktop/MRIP_data_2025/rdm testing data/check_1_7/SQ_old_", st, ".csv")))
  
  }

state_list<-list()
#for (st in c("MA", "RI", "CT", "NY", "NJ", "DE", "MD", "VA", "NC")){
for (st in c("NJ")){
  
  predictions_list<-list()
  for (dr in 65:100){
    
    source("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Code/test_code/predict_rec_catch_data_read_test2.R")
    source("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Code/sim/predict_rec_catch_functions.R")
    source("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Code/test_code/predict_rec_catch_test2.R")
    
    predictions_list[[dr]]<-predictions
    
  }
  prediction_draws <- dplyr::bind_rows(predictions_list)
  
  write_csv(prediction_draws, file.path(paste0("C:/Users/andrew.carr-harris/Desktop/MRIP_data_2025/rdm testing data/SQ_runs_10_20/SQ_new65_", st, ".csv")))
  
}

state_list<-list()
for (st in c("MA", "RI", "CT", "NY", "DE", "MD", "VA", "NC")){
#for (st in c("NJ")){
  
  predictions_list<-list()
  for (dr in 1:100){
    
    source("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Code/test_code/predict_rec_catch_data_read_test2.R")
    source("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Code/sim/predict_rec_catch_functions.R")
    source("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Code/test_code/predict_rec_catch_test2.R")
    
    predictions_list[[dr]]<-predictions
    
  }
  prediction_draws <- dplyr::bind_rows(predictions_list)
  
  write_csv(prediction_draws, file.path(paste0("C:/Users/andrew.carr-harris/Desktop/MRIP_data_2025/rdm testing data/SQ_runs_10_20/SQ_new_", st, ".csv")))
  
}

# NO CHANGE
predictions_list<-list()
  st<-"CT"
  for (dr in 1:2){
    k<-dr
    source("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Code/sim/predict_rec_catch_data_read.R")
    directed_trips<-directed_trips %>% 
      tibble::tibble() %>%
      dplyr::select(mode, date, 
                    bsb_bag, bsb_min, bsb_bag_y2, bsb_min_y2, 
                    fluke_bag, fluke_min, fluke_bag_y2,fluke_min_y2,
                    scup_bag, scup_min, scup_bag_y2, scup_min_y2) %>% 
      dplyr::mutate(
        bsb_min_y2 = dplyr::if_else(
          # Condition: bsb_bag > 0
          bsb_bag > 0,
          # Value if TRUE: subtract 2
          bsb_min_y2 - 2*2.54,
          # Value if FALSE: keep the original value
          bsb_min_y2))
    source("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Code/sim/predict_rec_catch_functions.R")
    source("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Code/test_code/predict_rec_catch_test2.R")
    
    predictions_list[[k]]<-predictions
    k<-k+1
    
  }
  prediction_draws <- dplyr::bind_rows(predictions_list)
  prediction_draws_check <- prediction_draws %>% 
    dplyr::filter(is.na(value))
  prediction_draws1 <- prediction_draws %>% 
    dplyr::filter(mode=="all modes")
    
  
  
  calib_comparison<-readRDS(file.path(iterative_input_data_cd, "miscellanous/calibrated_model_stats_new.rds")) %>% 
    dplyr::select(mode, state, species, draw, MRIP_catch, MRIP_rel, MRIP_keep, model_catch, model_rel, model_keep) %>% 
    dplyr::filter(draw<=1 & state=="MA") %>% 
    dplyr::group_by(species, draw) %>% 
    dplyr::summarise(MRIP_catch=sum(MRIP_catch), 
                     MRIP_rel=sum(MRIP_rel), 
                     MRIP_keep=sum(MRIP_keep), 
                     model_catch=sum(model_catch), 
                     model_rel=sum(model_rel), 
                     model_keep=sum(model_keep)) %>% 
    dplyr::ungroup()
    %>% 
    dplyr::group_by(species) %>% 
    dplyr::summarise(MRIP_catch=mean(MRIP_catch), 
                     MRIP_rel=mean(MRIP_rel), 
                     MRIP_keep=mean(MRIP_keep), 
                     model_catch=mean(model_catch), 
                     model_rel=mean(model_rel), 
                     model_keep=mean(model_keep)) 


prediction_draws <- dplyr::bind_rows(predictions_list)
prediction_draws_check <- prediction_draws %>% 
  dplyr::filter(is.na(value))

write_csv(prediction_draws, file.path(input_data_cd, "test2output_SQ_new2.csv"))



# Reduce the minimum size by two for all species
predictions_list<-list()
k<-1
for (st in c("MA", "RI", "CT", "NY", "NJ", "DE", "MD", "VA", "NC")){

  for (dr in 1:25){

    source("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Code/test_code/predict_rec_catch_data_read_test2_min_minus2.R")
    source("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Code/sim/predict_rec_catch_functions.R")
    source("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Code/test_code/predict_rec_catch_test2.R")
    
    predictions_list[[k]]<-predictions
    k<-k+1
    
  }
}

prediction_draws <- dplyr::bind_rows(predictions_list)
prediction_draws_check <- prediction_draws %>% 
  dplyr::filter(is.na(value))

write_csv(prediction_draws, file.path(input_data_cd, "test2output_minus2_new2.csv"))



# Increase the minimum size by two for all species
predictions_list<-list()
k<-1
for (st in c("MA", "RI", "CT", "NY", "NJ", "DE", "MD", "VA", "NC")){

  for (dr in 1:25){
    k<-k+1
    
    source("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Code/test_code/predict_rec_catch_data_read_test2_min_plus2.R")
    source("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Code/sim/predict_rec_catch_functions.R")
    source("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Code/test_code/predict_rec_catch_test2.R")
    
    predictions_list[[k]]<-predictions
    k<-k+1
    
  }
}

prediction_draws <- dplyr::bind_rows(predictions_list)
prediction_draws_check <- prediction_draws %>% 
  dplyr::filter(is.na(value))

write_csv(prediction_draws, file.path(input_data_cd, "test2output_plus2_new2.csv"))



