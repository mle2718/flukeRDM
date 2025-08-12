# Predict Rec Catch
# This function predict recreational catch for summer flounder, black sea bass, and scup. 

# Run predict_rec_catch_data_read testing. 


predict_rec_catch <- function(st, dr, directed_trips, catch_data, 
                              sf_size_data, bsb_size_data, scup_size_data, 
                              l_w_conversion, calib_comparison, n_choice_occasions, 
                              base_outcomes){
  
  
  ## Run for all modes + aggregate  - summer flounder 
  results_list <- lapply(mode_draw, simulate_mode_sf)
  
  sf_trip_data <- rbindlist(lapply(results_list, `[[`, "trip_data"))
  data.table::setkey(sf_trip_data, domain2)
  
  zero_catch_sf <- rbindlist(lapply(results_list, `[[`, "zero_catch"))
  
  size_data_sf <- rbindlist(lapply(results_list, `[[`, "size_data"))
  
  
  ## Run for all modes + aggregate  - black sea bass 
  results_list <- lapply(mode_draw, simulate_mode_bsb)
  
  bsb_trip_data <- rbindlist(lapply(results_list, `[[`, "trip_data")) %>% 
    dplyr::select(-date, -mode, -catch_draw, -tripid)
  
  data.table::setkey(bsb_trip_data, domain2)
  
  zero_catch_bsb <- rbindlist(lapply(results_list, `[[`, "zero_catch"))
  
  size_data_bsb <- rbindlist(lapply(results_list, `[[`, "size_data"))
  
  
  ## Run for all modes + aggregate  - scup 
  results_list <- lapply(mode_draw, simulate_mode_scup)
  
  scup_trip_data <- rbindlist(lapply(results_list, `[[`, "trip_data")) %>% 
    dplyr::select(-date, -mode, -catch_draw, -tripid)
  
  data.table::setkey(scup_trip_data, domain2)
  
  zero_catch_scup <- rbindlist(lapply(results_list, `[[`, "zero_catch"))

  
  size_data_scup <- rbindlist(lapply(results_list, `[[`, "size_data"))
 
  #If there is catch of all three species:
  if(sf_catch_check !=0 & bsb_catch_check!=0 & scup_catch_check!=0){
    
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
  }
  

  # #If there is catch of only sf 
  # if(sf_catch_check !=0 & bsb_catch_check==0){
  #   
  #   keep_release_cod<-trip_data %>% 
  #     dplyr::select("period2","tripid", "catch_draw") %>% 
  #     dplyr::mutate(keep_cod_1=0, release_cod_1=0)
  #   
  #   length_data <- keep_release_hadd %>%
  #     dplyr::full_join(keep_release_cod, by = c("period2","tripid", "catch_draw"))
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

  trip_data[, domain2 := NULL]
  
  rm(sf_trip_data, scup_trip_data, bsb_trip_data, 
     size_data_sf, size_data_bsb,size_data_scup, 
     base_outcomes, catch_data)
  
  
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
      beta_sqrt_sf_release * sqrt_rel_sf_new +
      beta_sqrt_bsb_keep * sqrt_keep_bsb_new +
      beta_sqrt_bsb_release * sqrt_rel_bsb_new +
      beta_sqrt_sf_bsb_keep * (sqrt_keep_sf_new * sqrt_keep_bsb_new) +
      beta_sqrt_scup_catch * sqrt_cat_scup_new +
      beta_cost * cost,
    
    v0 = beta_sqrt_sf_keep * sqrt_keep_sf_base +
      beta_sqrt_sf_release * sqrt_rel_sf_base +
      beta_sqrt_bsb_keep * sqrt_keep_bsb_base +
      beta_sqrt_bsb_release * sqrt_rel_bsb_base +
      beta_sqrt_sf_bsb_keep * (sqrt_keep_sf_base * sqrt_keep_bsb_base) +
      beta_sqrt_scup_catch * sqrt_cat_scup_base +
      beta_cost * cost
  )]
  
  # Optionally, remove the temp sqrt columns to save memory
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
    change_CS = CS_alt - CS_base
  )]
  
  # Get rid of things we don't need.
  mean_trip_data <- mean_trip_data %>% 
    dplyr::filter(alt==1) %>% 
    dplyr::select(-matches("beta")) %>% 
    dplyr::select(-"alt", -"opt_out", -"vA" , -"v0",-"exp_v0", -"exp_vA", 
                  -"cost", -"age", -"total_trips_12", -"catch_draw", -"group_index", 
                  -"log_sum_alt", -"log_sum_base", "tot_keep_sf_base",  "tot_rel_sf_base",  "tot_cat_sf_base", 
                  "tot_keep_bsb_base",  "tot_rel_bsb_base", "tot_cat_bsb_base",  
                  "tot_keep_scup_base","tot_rel_scup_base",  "tot_cat_scup_base", "prob0") 
  
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
  
  mean_trip_data<-mean_trip_data %>% 
    dplyr::left_join(n_choice_occasions, by = c("mode", "date_parsed")) %>% 
    dplyr::mutate(month = lubridate::month(date_parsed))  %>% 
    dplyr::left_join(calendar_adjustments, by = c("mode", "month")) %>% 
    dplyr::rename(n_choice_occasions0=n_choice_occasions, 
                  estimated_trips0=estimated_trips) %>% 
    dplyr::mutate(n_choice_occasions=n_choice_occasions0*expansion_factor,
                  expand=n_choice_occasions/ndraws) 
  
  # Expand outcomes for projection year

  predictions <- rbindlist(
    list(length_output, model_output1),
    use.names = TRUE,
    fill = TRUE)
  
  print("Finished predict_rec_catch")
  
  return(predictions) 
}

