
#test code for predict rec catch
# this is the "new" code 8/7/25
iterative_input_data_cd="C:/Users/andrew.carr-harris/Desktop/flukeRDM_iterative_data"
input_data_cd="C:/Users/andrew.carr-harris/Desktop/MRIP_data_2025"

#check

############# To Run Individual
# Variables to change 
dr<-1
st="MA"


library(magrittr)
############# To Run in Loop 

#for (st in c("MA", "RI")){
#  for (dr in 1:2){

# import necessary data

# For kim: We need to retain the SQ regulation variables. 
#          As of now, the SQ regulations variables are the ones without subscripts. 
#          I will copy these variables with the subscript _SQ to make this explicit.
#          The alternative regulations that will be adjusted by the users will be 
#          have subscripts _y2 (note this is slightly different from cod and haddock 2024)


directed_trips<-feather::read_feather(file.path(input_data_cd, paste0("directed_trips_calibration_", st, ".feather"))) %>% 
  tibble::tibble() %>%
  dplyr::filter(draw == dr) %>%
  dplyr::select(mode, date, 
                bsb_bag, bsb_min, bsb_bag_y2, bsb_min_y2, 
                fluke_bag, fluke_min, fluke_bag_y2,fluke_min_y2,
                scup_bag, scup_min, scup_bag_y2, scup_min_y2) %>% 
  dplyr::mutate(fluke_min_SQ=fluke_min, fluke_bag_SQ=fluke_bag, 
                bsb_min_SQ=bsb_min, bsb_bag_SQ=bsb_bag, 
                scup_min_SQ=scup_min, scup_bag_SQ=scup_bag)


catch_data <- feather::read_feather(file.path(iterative_input_data_cd, paste0("projected_catch_draws_",st, "_", dr,".feather"))) %>% 
  dplyr::left_join(directed_trips, by=c("mode", "date")) 

catch_data<-catch_data %>% 
  dplyr::select(-cost, -total_trips_12, -age, -bsb_keep_sim, -bsb_rel_sim, -day_i, -my_dom_id_string, 
                -scup_keep_sim, -scup_rel_sim, -sf_keep_sim, -sf_rel_sim, -wave)

l_w_conversion <- readr::read_csv(file.path("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Data", "L_W_Conversion.csv"), show_col_types = FALSE)  %>% 
  dplyr::filter(state==st)


sf_size_data <- read_csv(file.path(input_data_cd, "baseline_catch_at_length.csv"), show_col_types = FALSE)  %>% 
  dplyr::filter(state == st, species=="sf", draw==dr) %>% 
  dplyr::filter(!is.na(fitted_prob)) %>% 
  dplyr::select(state, fitted_prob, length)

bsb_size_data <- read_csv(file.path(input_data_cd, "baseline_catch_at_length.csv"), show_col_types = FALSE)  %>% 
  dplyr::filter(state == st, species=="bsb" , draw==dr) %>% 
  dplyr::filter(!is.na(fitted_prob)) %>% 
  dplyr::select(state, fitted_prob, length)

scup_size_data <- read_csv(file.path(input_data_cd, "baseline_catch_at_length.csv"), show_col_types = FALSE)  %>% 
  dplyr::filter(state == st, species=="scup", draw==dr) %>% 
  dplyr::filter(!is.na(fitted_prob)) %>% 
  dplyr::select(state,  fitted_prob, length)

# base-year trip outcomes
base_outcomes0 <- list()
n_choice_occasions0 <- list()

mode_draw <- c("sh", "pr", "fh")
for (md in mode_draw) {
  
  # pull trip outcomes from the calibration year
  base_outcomes0[[md]]<-feather::read_feather(file.path(iterative_input_data_cd, paste0("base_outcomes_", st, "_", md, "_", dr, ".feather"))) %>% 
    data.table::as.data.table()
  
  base_outcomes0[[md]]<-base_outcomes0[[md]] %>% 
    dplyr::select(-domain2) %>% 
    dplyr::mutate(date_parsed=lubridate::dmy(date)) %>% 
    dplyr::select(-date)
  
  # pull in data on the number of choice occasions per mode-day
  n_choice_occasions0[[md]]<-feather::read_feather(file.path(iterative_input_data_cd, paste0("n_choice_occasions_MA_", md, "_", dr, ".feather")))  
  n_choice_occasions0[[md]]<-n_choice_occasions0[[md]] %>% 
    dplyr::mutate(date_parsed=lubridate::dmy(date)) %>% 
    dplyr::select(-date)
  
}

base_outcomes <- bind_rows(base_outcomes0)
n_choice_occasions <- bind_rows(n_choice_occasions0) %>% 
  dplyr::arrange(date_parsed, mode)
rm(base_outcomes0, n_choice_occasions0)

base_outcomes<-base_outcomes %>% 
  dplyr::arrange(date_parsed, mode, tripid, catch_draw)




# Pull in calibration comparison information about trip-level harvest/discard re-allocations 
calib_comparison<-readRDS(file.path(iterative_input_data_cd, "calibrated_model_stats.rds")) %>% 
  dplyr::filter(state==st & draw==dr ) 

calib_comparison<-calib_comparison %>% 
  dplyr::rename(n_legal_rel_bsb=n_legal_bsb_rel, 
                n_legal_rel_scup=n_legal_scup_rel, 
                n_legal_rel_sf=n_legal_sf_rel, 
                n_sub_kept_bsb=n_sub_bsb_kept,
                n_sub_kept_sf=n_sub_sf_kept,
                n_sub_kept_scup=n_sub_scup_kept,
                prop_legal_rel_bsb=prop_legal_bsb_rel,
                prop_legal_rel_sf=prop_legal_sf_rel,
                prop_legal_rel_scup=prop_legal_scup_rel,
                prop_sub_kept_bsb=prop_sub_bsb_kept,
                prop_sub_kept_sf=prop_sub_sf_kept,
                prop_sub_kept_scup=prop_sub_scup_kept,
                convergence_sf=sf_convergence,
                convergence_bsb=bsb_convergence,
                convergence_scup=scup_convergence) 

##########
# List of species suffixes
species_suffixes <- c("sf", "bsb", "scup")

# Get all variable names
all_vars <- names(calib_comparison)

# Identify columns that are species-specific (contain _sf, _bsb, or _scup)
species_specific_vars <- all_vars[
  str_detect(all_vars, paste0("(_", species_suffixes, ")$", collapse = "|"))
]

id_vars <- setdiff(all_vars, species_specific_vars)

calib_comparison<-calib_comparison %>% 
  dplyr::select(mode, all_of(species_specific_vars))

# Extract base variable names (without _sf, _bsb, _scup)
base_names <- unique(str_replace(species_specific_vars, "_(sf|bsb|scup)$", ""))

# Pivot the data longer on the species-specific columns
calib_comparison <- calib_comparison %>%
  pivot_longer(
    cols = all_of(species_specific_vars),
    names_to = c(".value", "species"),
    names_pattern = "(.*)_(sf|bsb|scup)"
  ) %>% 
  dplyr::distinct()


#  }
#}


set.seed(12345)

#Step 1: Preliminaries – load libraries and prepare inputs
library(data.table)
library(dplyr)
library(tidyr)


#Convert key data frames to data.table format early:
setDT(directed_trips)
setDT(catch_data)
setDT(calib_comparison)
setDT(sf_size_data)

#Set up constants (unchanged):
floor_subl_sf_harv <- min(directed_trips$fluke_min) - 3 * 2.54
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

setDT(calib_lookup)
setkey(calib_lookup, mode)

md="sh"

simulate_mode_sf <- function(md) {
  # Extract calibration parameters
  calib_row <- calib_lookup[mode == md]
  
  rel_to_keep_sf     <- calib_row$rel_to_keep_sf
  keep_to_rel_sf     <- calib_row$keep_to_rel_sf
  p_rel_to_keep_sf   <- calib_row$p_rel_to_keep_sf
  p_keep_to_rel_sf   <- calib_row$p_keep_to_rel_sf
  prop_sublegal_kept_sf <- calib_row$prop_sub_kept_sf
  prop_legal_rel_sf     <- calib_row$prop_legal_rel_sf
  all_keep_to_rel_sf <- as.integer(p_keep_to_rel_sf == 1)
  
  # Filter catch data by mode
  catch_data_md <- catch_data[mode == md]
  sf_catch_check_md <- sum(catch_data_md$sf_cat)
  
  if (sf_catch_check_md == 0) {
    return(list(
      trip_data = catch_data_md[, .(date, catch_draw, tripid, mode,
                                    tot_keep_sf_new = 0L, tot_rel_sf_new = 0L,
                                    domain2 = paste0(date, "_", mode, "_", catch_draw, "_", tripid))],
      zero_catch = catch_data_md[sf_cat == 0]
    ))
  }
  
  # Expand fish by number caught
  sf_catch_data <- catch_data_md[sf_cat > 0]
  sf_catch_data <- sf_catch_data[rep(1:.N, sf_cat)]
  sf_catch_data[, fishid := .I]
  
  # Sample fish lengths
  sf_catch_data[, fitted_length := sample(sf_size_data$length, .N, 
                                          prob = sf_size_data$fitted_prob, replace = TRUE)]
  
  # Identify keepable fish
  sf_catch_data[, posskeep := as.integer(fitted_length >= fluke_min_y2)]
  sf_catch_data[, csum_keep := ave(posskeep, tripid, date, mode, catch_draw, FUN = cumsum)]
  sf_catch_data[, keep_adj := as.integer(posskeep == 1 & csum_keep <= fluke_bag_y2)]
  sf_catch_data[, `:=`(keep = keep_adj, release = 1L - keep_adj)]
  sf_catch_data[, subl_harv_indicator := as.integer(release == 1 & fitted_length >= floor_subl_sf_harv)]
  
  # --- Reallocate rel to keep ---
  if (rel_to_keep_sf == 1 && sum(sf_catch_data$release) > 0) {
    sublegal_keeps <- sf_catch_data[subl_harv_indicator == 1]
    base <- sf_catch_data[subl_harv_indicator == 0]
    
    n_to_keep <- round(prop_sublegal_kept_sf * nrow(sublegal_keeps))
    sublegal_keeps[, uniform := runif(.N)]
    setorder(sublegal_keeps, uniform)
    sublegal_keeps[, fishid2 := .I]
    sublegal_keeps[, `:=`(
      keep = as.integer(fishid2 <= n_to_keep),
      release = as.integer(fishid2 > n_to_keep)
    )]
    sum(sublegal_keeps$release)
    sum(sublegal_keeps$keep)
    
    # Drop helper columns *only if they exist*
    cols_to_drop_sub <- intersect(names(sublegal_keeps), c("uniform", "fishid2", "subl_harv_indicator"))
    sublegal_keeps[, (cols_to_drop_sub) := NULL]
    
    cols_to_drop_base <- intersect(names(base), "subl_harv_indicator")
    base[, (cols_to_drop_base) := NULL]
    
    sf_catch_data <- rbindlist(list(sublegal_keeps, base), use.names = TRUE, fill = TRUE)
  }
  
  # --- Reallocate keep to rel ---
  if (keep_to_rel_sf == 1 && sum(sf_catch_data$keep) > 0) {
    if (all_keep_to_rel_sf == 1) {
      sf_catch_data[, `:=`(release = keep + release, keep = 0L)]
    } else {
      kept <- sf_catch_data[keep == 1]
      base <- sf_catch_data[keep == 0]
      n_to_release <- round(prop_legal_rel_sf * nrow(kept))
      
      kept[, uniform := runif(.N)]
      setorder(kept, uniform)
      kept[, fishid2 := .I]
      kept[, `:=`(
        release = as.integer(fishid2 <= n_to_release),
        keep = as.integer(fishid2 > n_to_release)
      )]
      kept[, `:=`(uniform = NULL, fishid2 = NULL)]
      
      sf_catch_data <- rbindlist(list(kept, base), use.names = TRUE)
    }
  }
  
  # --- Append length-specific keep/release summary ---
  sf_catch_data <- data.table::as.data.table(sf_catch_data)
  
  new_size_data <- sf_catch_data[, .(
    keep = sum(keep),
    release = sum(release)
  ), by = .(mode, date, catch_draw, tripid, fitted_length)]
  
  keep_size_data <- new_size_data %>%
    dplyr::select(-release) %>%
    tidyr::pivot_wider(
      names_from = fitted_length,
      names_glue = "keep_sf_{fitted_length}",
      names_sort = TRUE,
      values_from = keep,
      values_fill = 0
    )
  
  release_size_data <- new_size_data %>%
    dplyr::select(-keep) %>%
    tidyr::pivot_wider(
      names_from = fitted_length,
      names_glue = "release_sf_{fitted_length}",
      names_sort = TRUE,
      values_from = release,
      values_fill = 0
    )
  
  keep_release_size_data <- keep_size_data %>%
    dplyr::left_join(release_size_data, by = c("date", "mode", "tripid", "catch_draw"))
  
  
  # Summarize trip-level data
  trip_summary <- sf_catch_data[, .(tot_keep_sf_new = sum(keep), tot_rel_sf_new = sum(release)), 
                                by = .(date, catch_draw, tripid, mode)]
  
  # Add zero-catch trips
  zero_catch <- catch_data_md[sf_cat == 0, .(date, catch_draw, tripid, mode)]
  zero_catch[, `:=`(tot_keep_sf_new = 0L, tot_rel_sf_new = 0L)]
  
  trip_data <- rbindlist(list(trip_summary, zero_catch))
  trip_data[, domain2 := paste0(date, "_", mode, "_", catch_draw, "_", tripid)]
  
  return(list(
    trip_data = trip_data,
    zero_catch = zero_catch,
    size_data = keep_release_size_data
  ))
}

#Step 4: Run for all modes + aggregate
results_list <- lapply(mode_draw_sf, simulate_mode_sf)

sf_trip_data <- rbindlist(lapply(results_list, `[[`, "trip_data"))
data.table::setkey(sf_trip_data, domain2)

zero_catch_sf <- rbindlist(lapply(results_list, `[[`, "zero_catch"))
size_data_sf <- rbindlist(lapply(results_list, `[[`, "size_data"))


sum(sf_trip_data$tot_keep_sf_new)
sum(sf_trip_data$tot_rel_sf_new)
nrow(zero_catch_sf)
sum(size_data_sf$keep_sf_42)
sum(size_data_sf$release_sf_45)
