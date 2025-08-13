# Predict Rec Catch
# This function predict recreational catch for summer flounder, black sea bass, and scup. 

# Run predict_rec_catch_data_read testing. 


predict_rec_catch <- function(st, dr, directed_trips, catch_data, 
                              sf_size_data, bsb_size_data, scup_size_data, 
                              l_w_conversion, calib_comparison, n_choice_occasions, 
                              calendar_adjustments, base_outcomes){
  
   setDT(directed_trips)
   setDT(catch_data)
   setDT(calib_comparison)
   setDT(sf_size_data)
   setDT(bsb_size_data)
   setDT(scup_size_data)
  
  mode_draw <- c("sh", "pr", "fh")
  floor_subl_sf_harv <- min(directed_trips$fluke_min_y2) - 3 * 2.54
  floor_subl_bsb_harv <- min(directed_trips$bsb_min_y2) - 3 * 2.54
  floor_subl_scup_harv <- min(directed_trips$scup_min_y2) - 3 * 2.54
  
  calib_lookup <- calib_comparison %>%
    dplyr::select(mode, species, rel_to_keep, keep_to_rel,
                  p_rel_to_keep, p_keep_to_rel,
                  prop_sub_kept, prop_legal_rel) %>%
    tidyr::pivot_wider(
      names_from = species,
      values_from = c(rel_to_keep, keep_to_rel, p_rel_to_keep, p_keep_to_rel, prop_sub_kept, prop_legal_rel),
      names_glue = "{.value}_{species}"
    )
  
  setDT(calib_lookup)
  setkey(calib_lookup, mode)
  ## Run for all modes + aggregate  - summer flounder 
  results_list <- lapply(mode_draw, simulate_mode_sf, calib_lookup = calib_lookup, 
                         floor_subl_sf_harv= floor_subl_sf_harv, sf_size_data = sf_size_data)
  
  sf_trip_data <- rbindlist(lapply(results_list, `[[`, "trip_data"))
  data.table::setkey(sf_trip_data, domain2)
  
  zero_catch_sf <- rbindlist(lapply(results_list, `[[`, "zero_catch"))
  print("Starting SF Size")
  size_data_sf <- rbindlist(lapply(results_list, `[[`, "size_data"))
  print("made it out SF Size")
  
  ## Run for all modes + aggregate  - black sea bass 
  results_list <- lapply(mode_draw, simulate_mode_bsb, calib_lookup = calib_lookup, 
                         floor_subl_bsb_harv = floor_subl_bsb_harv, bsb_size_data = bsb_size_data)
  
  bsb_trip_data <- rbindlist(lapply(results_list, `[[`, "trip_data")) %>% 
    dplyr::select(-date, -mode, -catch_draw, -tripid)
  
  data.table::setkey(bsb_trip_data, domain2)
  
  zero_catch_bsb <- rbindlist(lapply(results_list, `[[`, "zero_catch"))
  
  size_data_bsb <- rbindlist(lapply(results_list, `[[`, "size_data"))
  
  
  ## Run for all modes + aggregate  - scup 
  results_list <- lapply(mode_draw, simulate_mode_scup, calib_lookup = calib_lookup, 
                         floor_subl_scup_harv= floor_subl_scup_harv, scup_size_data = scup_size_data)
  
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
  ndraws = 50
  mean_trip_data<-mean_trip_data %>% 
    dplyr::left_join(n_choice_occasions, by = c("mode", "date_parsed")) %>% 
    dplyr::mutate(month = lubridate::month(date_parsed))  %>% 
    dplyr::left_join(calendar_adjustments, by = c("mode", "month")) %>% 
    dplyr::rename(n_choice_occasions0=n_choice_occasions, 
                  estimated_trips0=estimated_trips) %>% 
    dplyr::mutate(n_choice_occasions=n_choice_occasions0*expansion_factor,
                  expand=n_choice_occasions/ndraws) 
  
  # Expand outcomes for projection year
  list_names <- c("tot_keep_sf_new",   "tot_rel_sf_new",  "tot_cat_sf_new", 
                  "tot_keep_bsb_new",  "tot_rel_bsb_new", "tot_cat_bsb_new",  
                  "tot_keep_scup_new","tot_rel_scup_new",  "tot_cat_scup_new", 
                  "probA", "change_CS")
  
  all_vars <- c(list_names)
  
  mean_trip_data <- mean_trip_data %>%
    data.table::as.data.table() %>%
    .[,as.vector(all_vars) := lapply(.SD, function(x) x * expand), .SDcols = all_vars] %>%
    .[]
  
  #retain expansion factors by strata to multiply with length data 
  expansion_factors<-mean_trip_data %>% 
    dplyr::select("date_parsed","mode", "tripid", "expand")
  
  #process length data 
  pattern_vars <- grep("^keep_(sf_|bsb_|scup_)[0-9.]*$|^release_(sf_|bsb_|scup_)[0-9.]*$", 
                       names(length_data), value = TRUE)
  
  length_data<-length_data  %>% data.table::as.data.table() %>%
    .[,lapply(.SD, mean), by = c("date_parsed","mode", "tripid"), .SDcols = pattern_vars]  
  
  length_data<-length_data %>% 
    dplyr::left_join(expansion_factors, b=c("date_parsed","mode", "tripid"))
  
  length_data <- length_data %>%
    data.table::as.data.table() %>%
    .[,as.vector(pattern_vars) := lapply(.SD, function(x) x * expand), .SDcols = pattern_vars] %>%
    .[]  
  
  ## Compute welfare and predicted trips
  # Aggregate by mode
  mean_trip_data <- mean_trip_data %>%
    dplyr::rename(n_trips_alt = probA)
  
  # Ensure mean_trip_data is a data.table
  data.table::setDT(mean_trip_data)
  list_names <- c("change_CS","n_trips_alt")
  
  aggregate_trip_data_mode <- mean_trip_data[, lapply(.SD, sum), by = .(mode), .SDcols = list_names]
  
  # Aggregate for all modes
  aggregate_trip_data_allmodes <- mean_trip_data[, lapply(.SD, sum), .SDcols = list_names][
    , mode := "all modes"
  ]
  
  # Combine and reshape
  model_output1 <- rbindlist(list(aggregate_trip_data_mode, aggregate_trip_data_allmodes), use.names=TRUE)[
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
  length_data1 <- melt(
    length_data1,
    id.vars = c("month", "mode"),
    variable.name = "Var",
    value.name = "number_at_length"
  )
  
  ## Split Var into keep_release, species, length
  length_data1[, c("keep_release", "species", "length") := tstrsplit(Var, "_", fixed = TRUE)]
  length_data1[, length := as.numeric(length)]
  
  ## Join with l_w_conversion
  setDT(l_w_conversion)
  length_data1 <- l_w_conversion[length_data1, on = .(month, species)]
  
  ## Compute weight
  length_data1[, weight := fcase(
    species == "scup", exp(ln_a + b * log(length)),
    species %chin% c("sf", "bsb"), a * length^b,
    default = NA_real_
  )]
  
  ## Convert to lbs
  length_data1[, weight := weight * 2.20462262185]
  
  ## Totals
  length_data1[, total_weight := number_at_length * weight]
  
  ## Discard mortality weight
  length_data1[, discmort_weight := fcase(
    keep_release == "release" & species == "sf", 0.10 * number_at_length * weight,
    keep_release == "release" & species == "scup", 0.15 * number_at_length * weight,
    keep_release == "release" & species == "bsb", 0.15 * number_at_length * weight,
    default = 0
  )]
  
  ## Discard mortality number
  length_data1[, discmort_number := fcase(
    keep_release == "release" & species == "sf", 0.10 * number_at_length,
    keep_release == "release" & species == "scup", 0.15 * number_at_length,
    keep_release == "release" & species == "bsb", 0.15 * number_at_length,
    default = 0
  )]
  
  ## Summarise by species, mode, keep_release
  length_data1 <- length_data1[, .(
    total_numbers = sum(number_at_length),
    total_weight = sum(total_weight),
    discmort_weight = sum(discmort_weight),
    discmort_number = sum(discmort_number)
  ), by = .(species, mode, keep_release)]
  
  ## Create var1 and melt again
  length_data_long <- melt(
    length_data1[, var1 := paste(species, mode, keep_release, sep = "_")],
    id.vars = c("var1"),  # only identifiers
    measure.vars = c("total_numbers", "total_weight", "discmort_weight", "discmort_number"),
    variable.name = "param",
    value.name = "value"
  )
  
  ## Remove NAs
  length_data_long <- length_data_long[!is.na(value)]
  
  ## Split and classify
  length_data_long[, c("category", "mode", "keep_release") := tstrsplit(var1, "_")]
  length_data_long[, number_weight := fifelse(grepl("weight", param), "weight", "number")]
  length_data_long[, param := gsub("total_|discmort_", "", param)]
  
  ## All modes aggregation
  length_data_long_all <- length_data_long[, .(value = sum(value)),
                                           by = .(category, keep_release, param, number_weight)]
  length_data_long_all[, mode := "all modes"]
  
  ## Final bind
  length_output <- rbindlist(
    list(length_data_long_all, length_data_long),
    use.names = TRUE,
    fill = TRUE
  )[ , var1 := NULL ]
  
  
  predictions <- rbindlist(
    list(length_output, model_output1),
    use.names = TRUE,
    fill = TRUE) %>% 
    dplyr::mutate(state = st, draw=dr)
  
  print("Finished predict_rec_catch")
  
  return(predictions) 
}