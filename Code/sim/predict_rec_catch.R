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
  
  
  #merge the trip data
  # Join summer flounder (sf) and black sea bass (bsb) on domain2
  trip_data_a <- merge(sf_trip_data, bsb_trip_data, by = "domain2", all = TRUE)
  
  # Join the result with scup data on domain2
  trip_data <- merge(trip_data_a, scup_trip_data, by = "domain2", all = TRUE)
  
  sf_catch_check<-sum(sf_trip_data$tot_keep_sf_new+sf_trip_data$tot_rel_sf_new)
  bsb_catch_check<-sum(bsb_trip_data$tot_keep_bsb_new+bsb_trip_data$tot_rel_bsb_new)
  scup_catch_check<-sum(scup_trip_data$tot_keep_scup_new+scup_trip_data$tot_rel_scup_new)
  
  rm(trip_data_a)
  
  
# merge the length data
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
  
  
  dim(trip_data)
  dim(length_data)
  
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
  trip_data <- trip_data[base_outcomes, on = .(date_parsed, mode, tripid, catch_draw), nomatch = 0]
  trip_data <- trip_data[length_data, on = .(date_parsed, mode, tripid, catch_draw), nomatch = 0]
  
  trip_data[, domain2 := NULL]
  
  rm(sf_trip_data, scup_trip_data, bsb_trip_data, 
     size_data_sf, size_data_bsb,size_data_scup, 
     base_outcomes, length_data, catch_data)
  
  
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
  # mean_trip_data <- mean_trip_data %>%
  #   dplyr::mutate(n_alt = rep(2,nrow(.))) %>%
  #   tidyr::uncount(n_alt) %>%
  #   dplyr::mutate(alt = rep(1:2,nrow(.)/2),
  #                 opt_out = ifelse(alt == 2, 1, 0))
  
  mean_trip_data <- rbindlist(list(mean_trip_data, copy(mean_trip_data)))[, alt := rep(1:2, length.out = .N)][, opt_out := (alt == 2)*1]
  
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
  
  
  #mean(mean_trip_data$change_CS)
  
  # Get rid of things we don't need.
  mean_trip_data <- mean_trip_data %>% 
    dplyr::filter(alt==1) %>% 
    dplyr::select(-matches("beta")) %>% 
    dplyr::select(-"alt", -"opt_out", -"vA" , -"v0",-"exp_v0", -"exp_vA", 
                  -"cost", -"age", -"total_trips_12", -"catch_draw", -"group_index", 
                  -"log_sum_alt", -"log_sum_base") 
  
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
  
  pattern_vars <- grep("^keep_(sf_|bsb_|scup_)[0-9.]*$|^release_(sf_|bsb_|scup_)[0-9.]*$", 
                       names(mean_trip_data), value = TRUE)
  
  # Combine
  all_vars <- c(list_names, pattern_vars)
  
  mean_trip_data <- mean_trip_data %>%
    data.table::as.data.table() %>%
    .[,as.vector(all_vars) := lapply(.SD, function(x) x * probA), .SDcols = all_vars] %>%
    .[]
  
  # multiply the average trip probability in the base scenario (prob0) by each catch variable to get probability-weighted catch
  list_names <- c("tot_keep_sf_base",   "tot_rel_sf_base",  "tot_cat_sf_base", 
                  "tot_keep_bsb_base",  "tot_rel_bsb_base", "tot_cat_bsb_base",  
                  "tot_keep_scup_base","tot_rel_scup_base",  "tot_cat_scup_base")
  
  mean_trip_data <- mean_trip_data %>%
    data.table::as.data.table() %>%
    .[,as.vector(list_names) := lapply(.SD, function(x) x * probA), .SDcols = list_names] %>%
    .[]
  
  # select the same number of choice occasions in the prediction year as in the calibration year
  mean_trip_data<-mean_trip_data %>% 
    dplyr::left_join(n_choice_occasions, by = c("mode", "date_parsed"))
  
  # mean_trip_data <-mean_trip_data %>% 
  #   dplyr::mutate(n_sim_choices=1) %>% 
  #   dplyr::group_by(date_parsed, mode) %>% 
  #   dplyr::mutate(sum_n_sim_choices=sum(n_sim_choices)) %>% 
  #   dplyr::ungroup()
  # 
  # mean_trip_data <- mean_trip_data %>% 
  #   dplyr::mutate(uniform=runif(dplyr::n(), min=0, max=1)) %>% 
  #   dplyr::arrange(date_parsed, mode, uniform)
  # 
  # mean_trip_data1 <- mean_trip_data %>%
  #   dplyr::group_by(date_parsed, mode) %>%
  #   dplyr::mutate(id_within_group = dplyr::row_number()) %>%
  #   dplyr::filter(n_choice_occasions<=sum_n_sim_choices & id_within_group<=n_choice_occasions) %>%
  #   dplyr::ungroup()
  # 
  # mean_trip_data2 <- mean_trip_data %>%
  #   dplyr::filter(n_choice_occasions>sum_n_sim_choices)  %>%
  #   dplyr::mutate(expand=round(n_choice_occasions/sum_n_sim_choices)+1)
  # 
  # row_inds <- seq_len(nrow(mean_trip_data2))
  # 
  # mean_trip_data2<-mean_trip_data2 %>%
  #   dplyr::slice(rep(row_inds,expand))

  # mean_trip_data2 <- mean_trip_data2 %>%
  #   dplyr::mutate(uniform=runif(dplyr::n(), min=0, max=1)) %>%
  #   dplyr::arrange(date_parsed, mode, uniform) %>%
  #   dplyr::group_by(date_parsed, mode) %>%
  #   dplyr::mutate(id_within_group = dplyr::row_number()) %>%
  #   dplyr::filter(id_within_group<=n_choice_occasions)

  # results<-mean_trip_data1 %>% 
  #   dplyr::bind_rows(mean_trip_data2) %>% 
  #   dplyr::rename(n_trips_base=prob0, 
  #                 n_trips_alt=probA) %>% 
  #   dplyr::select(-uniform, id_within_group, -expand)
  # 
  # rm(mean_trip_data1, mean_trip_data2, trip_data)
  
  # Ensure mean_trip_data is a data.table
  data.table::setDT(mean_trip_data)
  
  # Step 1: Add n_sim_choices and compute sum_n_sim_choices by group
  mean_trip_data[, n_sim_choices := 1]
  mean_trip_data[, sum_n_sim_choices := .N, by = .(date_parsed, mode)]
  
  # Step 2: Generate uniform random numbers and sort
  mean_trip_data[, uniform := runif(.N)]
  data.table::setorder(mean_trip_data, date_parsed, mode, uniform)

  # Step 3: Keep rows where we can select based on n_choice_occasions and sum_n_sim_choices
  mean_trip_data[, id_within_group := seq_len(.N), by = .(date_parsed, mode)]
  mean_trip_data1 <- mean_trip_data[
    n_choice_occasions <= sum_n_sim_choices & id_within_group <= n_choice_occasions
  ]
  #sf_catch_data <- sf_catch_data[rep(1:.N, sf_cat)]
  
  # Step 4: Handle groups where n_choice_occasions > sum_n_sim_choices
  mean_trip_data2 <- mean_trip_data[
    n_choice_occasions > sum_n_sim_choices][,
    expand := round(n_choice_occasions / sum_n_sim_choices) + 1
  ]
  
  mean_trip_data2<-mean_trip_data2[rep(1:.N, expand)]

  
  # Step 5: Shuffle again after replication and reassign row numbers within group
  mean_trip_data2[, uniform := runif(.N)]
  data.table::setorder(mean_trip_data2, date_parsed, mode, uniform)
  mean_trip_data2[, id_within_group := seq_len(.N), by = .(date_parsed, mode)]
  
  # Step 6: Filter to keep only enough rows per group
  mean_trip_data2 <- mean_trip_data2[id_within_group <= n_choice_occasions]
  mean_trip_data2[, expand := NULL]
  
  # Step 7: Combine and clean
  results <- rbindlist(list(mean_trip_data1, mean_trip_data2), use.names = TRUE)
  results[, `:=`(
    n_trips_base = prob0,
    n_trips_alt = probA
  )]
  results[, c("uniform", "id_within_group") := NULL]
  
  # Clean up
  rm(mean_trip_data1, mean_trip_data2, trip_data)
  
  # aggregate welfare and fishing effort estimates
  
  list_names = c("change_CS",  "n_trips_alt")
  
  # aggregate_trip_data_mode <- results %>%
  #   data.table::as.data.table() %>%
  #   .[,lapply(.SD, sum),  by = c("mode"), .SDcols = list_names]
  # 
  # aggregate_trip_data_allmodes <- results %>%
  #   data.table::as.data.table() %>%
  #   .[,lapply(.SD, sum),  .SDcols = list_names] %>% 
  #   dplyr::mutate(mode="all modes")
  # 
  # model_output1<-aggregate_trip_data_mode %>% 
  #   dplyr::bind_rows(aggregate_trip_data_allmodes) %>% 
  #   dplyr::select(change_CS, n_trips_alt, mode) %>% 
  #   dplyr::rename(CV=change_CS, ntrips=n_trips_alt)
  # 
  # 
  # # model_output1 contains CV and ntrips estimates
  # model_output1<- model_output1 %>%
  #   tidyr::pivot_longer(!c(mode), names_to = "var", values_to = "value") %>%
  #   dplyr::mutate(category=dplyr::case_when(var=="CV"~"CV",TRUE ~ "predicted trips"), 
  #                 keep_release="N/A", param="N/A", 
  #                 number_weight=dplyr::case_when(var=="CV"~"dollars",TRUE ~ "trips") ) %>% 
  #   dplyr::select(-var)
  # 
  # 
  # # use the lengths of fish harvested and released to compute weights
  # pattern_vars <- grep("^keep_(sf_|bsb_|scup_)[0-9.]*$|^release_(sf_|bsb_|scup_)[0-9.]*$", 
  #                      names(results), value = TRUE)
  # 
  # length_data1 <- results %>%
  #   dplyr::select(date_parsed, mode,  all_of(pattern_vars)) %>% 
  #   dplyr::mutate(month=lubridate::month(date_parsed))
  # 
  # length_data1 <- length_data1 %>%
  #   data.table::as.data.table() %>%
  #   .[,lapply(.SD, base::sum), by = c("mode", "month"), .SDcols = pattern_vars]
  # 
  # length_data1<- length_data1 %>%
  #   tidyr::pivot_longer(cols = !month & !mode,  names_to = "Var", values_to = "number_at_length") %>%
  #   tidyr::separate(Var, into = c("keep_release", "species", "length"), sep = "_") %>%
  #   dplyr::left_join(l_w_conversion, by = c("month", "species")) %>%
  #   dplyr::mutate(length_in = as.numeric(length),
  #                 ln_a = as.numeric(ln_a),
  #                 ln_b = as.numeric(ln_b),
  #                 a = as.numeric(a),
  #                 b = as.numeric(b),
  #                 length_cm = length_in*2.54)  %>%  #Convert to cm
  #   dplyr::mutate(ln_a = ifelse(is.na(ln_a), 0, ln_a)) %>% 
  #   dplyr::mutate(a = ifelse(is.na(a), 0, a)) %>%
  #   dplyr::mutate(b = ifelse(is.na(b), 0, b)) %>%
  #   dplyr::mutate(ln_b = ifelse(is.na(ln_b), 0, ln_b))  %>%   
  #   dplyr::mutate(weight = dplyr::case_when(species == "scup" ~ exp(ln_a + ln_b*log(length_cm)))) %>% 
  #   dplyr::mutate(weight = dplyr::case_when(species == "sf" ~ a*length_cm^b, TRUE ~ weight))  %>% 
  #   dplyr::mutate(weight = dplyr::case_when(species == "bsb" ~ a*length_cm^b, TRUE ~ weight),
  #                 weight = weight*2.20462262185, #convert to lbs
  #                 total_weight = number_at_length * weight,
  #                 discmort_weight = dplyr::case_when(keep_release == "release" & species == "sf" ~ (.1 * number_at_length * weight)),
  #                 discmort_weight = dplyr::case_when(keep_release == "release" & species == "scup" ~ (.15 * number_at_length * weight), TRUE ~ discmort_weight),
  #                 discmort_weight = dplyr::case_when(keep_release == "release" & species == "bsb" ~ (.15 * number_at_length * weight), TRUE ~ discmort_weight),
  #                 discmort_number = dplyr::case_when(keep_release == "release" & species == "sf" ~ (.1 * number_at_length)),
  #                 discmort_number = dplyr::case_when(keep_release == "release" & species == "scup" ~ (.15 * number_at_length), TRUE ~ discmort_number),
  #                 discmort_number = dplyr::case_when(keep_release == "release" & species == "bsb" ~ (.15 * number_at_length), TRUE ~ discmort_number))  %>%
  #   dplyr::group_by(species, mode, keep_release) %>%
  #   dplyr::summarise(total_numbers = sum(number_at_length),
  #                    total_weight = sum(total_weight),
  #                    discmort_weight = sum(discmort_weight),
  #                    discmort_number = sum(discmort_number),.groups = 'drop') %>%
  #   dplyr::ungroup() 
  # 
  # length_data_long <- length_data1 %>%
  #   dplyr::mutate(var1 = paste0(species, "_", mode, "_", keep_release )) %>%
  #   dplyr::select(var1, total_numbers, total_weight, discmort_weight, discmort_number) %>%
  #   tidyr::pivot_longer(!var1, names_to = "var", values_to = "value") %>%
  #   dplyr::mutate(var = paste0(var1,"_",var)) %>%
  #   dplyr::select(!var1) %>%
  #   dplyr::filter(is.na(value)==FALSE) %>% 
  #   tidyr::separate(var, into = c("category","mode", "keep_release", "param", "number_weight")) 
  # 
  # length_data_long_all<-length_data_long %>% 
  #   dplyr::group_by(category, keep_release,param, number_weight) %>% 
  #   dplyr::summarise(value=sum(value),.groups = 'drop') %>% 
  #   dplyr::mutate(mode="all modes")
  # 
  # length_output<-length_data_long_all %>% 
  #   dplyr::bind_rows(length_data_long)
  
  
  # Convert results to data.table
  setDT(results)
  
  # Aggregate by mode
  aggregate_trip_data_mode <- results[, lapply(.SD, sum), by = .(mode), .SDcols = list_names]
  
  # Aggregate for all modes
  aggregate_trip_data_allmodes <- results[, lapply(.SD, sum), .SDcols = list_names][
    , mode := "all modes"
  ]
  
  # Combine and reshape
  model_output1 <- rbindlist(list(aggregate_trip_data_mode, aggregate_trip_data_allmodes))[
    , .(CV = change_CS, ntrips = n_trips_alt, mode)
  ][
    , .(var = c("CV", "ntrips"), value = c(CV, ntrips)), by = mode
  ][
    , `:=`(
      category = fifelse(var == "CV", "CV", "predicted trips"),
      keep_release = "N/A",
      param = "N/A",
      number_weight = fifelse(var == "CV", "dollars", "trips")
    )
  ][, var := NULL]
  
  # Process length-frequency data
  pattern_vars <- grep("^keep_(sf_|bsb_|scup_)[0-9.]*$|^release_(sf_|bsb_|scup_)[0-9.]*$", names(results), value = TRUE)
  
  length_data1 <- results[, c("date_parsed", "mode", pattern_vars), with = FALSE]
  length_data1[, month := month(date_parsed)]
  
  # Aggregate by mode and month
  length_data1 <- length_data1[, lapply(.SD, sum), by = .(mode, month), .SDcols = pattern_vars]
  
  # Reshape and join with conversion table
  length_data1 <- melt(length_data1, id.vars = c("mode", "month"), variable.name = "Var", value.name = "number_at_length")[
    , c("keep_release", "species", "length") := tstrsplit(Var, "_")
  ][
    , `:=`(
      length_in = as.numeric(length),
      length_cm = as.numeric(length) * 2.54
    )
  ][
    l_w_conversion, on = .(month, species)
  ][
    , `:=`(
      ln_a = fifelse(is.na(ln_a), 0, as.numeric(ln_a)),
      ln_b = fifelse(is.na(ln_b), 0, as.numeric(ln_b)),
      a = fifelse(is.na(a), 0, as.numeric(a)),
      b = fifelse(is.na(b), 0, as.numeric(b))
    )
  ][
    , weight := fcase(
      species == "scup", exp(ln_a + ln_b * log(length_cm)),
      species %chin% c("sf", "bsb"), a * length_cm^b,
      default = 0
    )
  ][
    , `:=`(
      weight = weight * 2.20462262185,  # kg to lbs
      total_weight = number_at_length * weight,
      discmort_weight = fcase(
        keep_release == "release" & species == "sf", 0.1 * number_at_length * weight,
        keep_release == "release" & species == "scup", 0.15 * number_at_length * weight,
        keep_release == "release" & species == "bsb", 0.15 * number_at_length * weight,
        default = 0
      ),
      discmort_number = fcase(
        keep_release == "release" & species == "sf", 0.1 * number_at_length,
        keep_release == "release" & species == "scup", 0.15 * number_at_length,
        keep_release == "release" & species == "bsb", 0.15 * number_at_length,
        default = 0
      )
    )
  ]
  
  # Final aggregation
  length_data_long <- length_data1[
    , .(
      total_numbers = sum(number_at_length),
      total_weight = sum(total_weight),
      discmort_weight = sum(discmort_weight),
      discmort_number = sum(discmort_number)
    ), by = .(species, mode, keep_release)
  ][
    , var1 := paste(species, mode, keep_release, sep = "_")
  ][
    , melt(.SD, id.vars = "var1", variable.name = "param", value.name = "value"),
    .SDcols = c("total_numbers", "total_weight", "discmort_weight", "discmort_number")
  ][
    , `:=`(
      number_weight = fifelse(grepl("weight", param), "weight", "number"),
      category = tstrsplit(var1, "_")[[1]],
      mode = tstrsplit(var1, "_")[[2]],
      keep_release = tstrsplit(var1, "_")[[3]],
      param = gsub("total_|discmort_", "", param)
    )
  ][
    , .(category, mode, keep_release, param, number_weight, value)
  ]
  
  # Create "all modes"
  length_data_long_all <- length_data_long[
    , .(value = sum(value)), by = .(category, keep_release, param, number_weight)
  ][
    , mode := "all modes"
  ]
  
  # Combine
  length_output <- rbindlist(list(length_data_long_all, length_data_long))
  
  # combine catch and welfare results 
  
  predictions <- plyr::rbind.fill(length_output, model_output1) %>%
    dplyr::select(category, mode, everything() ) %>% 
    dplyr::mutate(state = st, draw=dr)
      
  print("Finished predict_rec_catch")
      
  return(predictions) 
}

