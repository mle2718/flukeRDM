##############################
### MD Rec model run  ########
##############################
Run_Name <- args[1]

saved_regs<- read.csv(here::here(paste0("saved_regs/regs_", Run_Name, ".csv")))

for (a in seq_len(nrow(save_regs))) {
  # Extract name and value
  obj_name <- save_regs$input[a]
  obj_value <- save_regs$value[a]
  
  # Assign to object in the environment
  assign(obj_name, obj_value)
}

print("start model_MD")
state1 = "MD"
predictions_all = list()

data_path <- here::here("Data/")


#### Read in size data ####
size_data <- readr::read_csv(file.path(here::here("Data"), "projected_catch_at_length_new.csv"), show_col_types = FALSE)  %>% 
  dplyr::filter(state == "MD")

sf_size_data <- size_data %>% 
  dplyr::filter(species=="sf") %>% 
  dplyr::filter(!is.na(fitted_prob)) %>% 
  dplyr::select(state, fitted_prob, length, draw, mode)
bsb_size_data <- size_data  %>% 
  dplyr::filter(species=="bsb") %>% 
  dplyr::filter(!is.na(fitted_prob)) %>% 
  dplyr::select(state, fitted_prob, length, draw, mode)
scup_size_data <- size_data %>% 
  dplyr::filter(species=="scup") %>% 
  dplyr::filter(!is.na(fitted_prob)) %>% 
  dplyr::select(state,  fitted_prob, length, draw, mode)


l_w_conversion <- readr::read_csv(file.path(data_path, "L_W_Conversion.csv"), show_col_types = FALSE)  %>% 
  dplyr::filter(state=="MD")

#### directed trips ####
directed_trips<-feather::read_feather(file.path(data_path, paste0("directed_trips_calibration_new_MD.feather"))) %>% 
  tibble::tibble() %>%
  dplyr::select(mode, date, draw, bsb_bag, bsb_min, fluke_bag,fluke_min, scup_bag, scup_min,
                bsb_bag_y2, bsb_min_y2, fluke_bag_y2,fluke_min_y2, scup_bag_y2, scup_min_y2) %>% 
  dplyr::mutate(date_adj = lubridate::dmy(date), 
                date_adj = lubridate::yday(date_adj))

if (exists("SFmd_seas1_op")) {
  directed_trips<- directed_trips %>%
    dplyr::mutate(#Summer Flounder
      fluke_bag_y2=dplyr::case_when(date_adj >= lubridate::yday(SFmd_seas1_op) & date_adj <= lubridate::yday(SFmd_seas1_cl) ~ as.numeric(SFmd_1_bag), TRUE ~ 0), 
      fluke_min_y2=dplyr::case_when(date_adj >= lubridate::yday(SFmd_seas1_op) & date_adj <= lubridate::yday(SFmd_seas1_cl) ~ as.numeric(SFmd_1_len) * 2.54, TRUE ~ 254), 
      fluke_bag_y2=dplyr::case_when(date_adj >= lubridate::yday(SFmd_seas2_op) & date_adj <= lubridate::yday(SFmd_seas2_cl) ~ as.numeric(SFmd_2_bag), TRUE ~ fluke_bag_y2), 
      fluke_min_y2=dplyr::case_when(date_adj >= lubridate::yday(SFmd_seas2_op) & date_adj <= lubridate::yday(SFmd_seas2_cl) ~ as.numeric(SFmd_2_len) * 2.54, TRUE ~ fluke_min_y2))
  
} else {
  directed_trips<- directed_trips %>%
    dplyr::mutate(
      fluke_bag_y2=dplyr::case_when(mode == "fh" & date_adj >= lubridate::yday(SFmdFH_seas1_op) & date_adj <= lubridate::yday(SFmdFH_seas1_cl) ~ as.numeric(SFmdFH_1_bag), TRUE ~ 0), 
      fluke_bag_y2=dplyr::case_when(mode == "pr" & date_adj >= lubridate::yday(SFmdPR_seas1_op) & date_adj <= lubridate::yday(SFmdPR_seas1_cl) ~ as.numeric(SFmdPR_1_bag), TRUE ~ fluke_bag_y2), 
      fluke_bag_y2=dplyr::case_when(mode == "sh" & date_adj >= lubridate::yday(SFmdSH_seas1_op) & date_adj <= lubridate::yday(SFmdSH_seas1_cl) ~ as.numeric(SFmdSH_1_bag), TRUE ~ fluke_bag_y2),
      
      fluke_bag_y2=dplyr::case_when(mode == "fh" & date_adj >= lubridate::yday(SFmdFH_seas2_op) & date_adj <= lubridate::yday(SFmdFH_seas2_cl) ~ as.numeric(SFmdFH_2_bag), TRUE ~ fluke_bag_y2),
      fluke_bag_y2=dplyr::case_when(mode == "pr" & date_adj >= lubridate::yday(SFmdPR_seas2_op) & date_adj <= lubridate::yday(SFmdPR_seas2_cl) ~ as.numeric(SFmdPR_2_bag), TRUE ~ fluke_bag_y2),
      fluke_bag_y2=dplyr::case_when(mode == "sh" & date_adj >= lubridate::yday(SFmdSH_seas2_op) & date_adj <= lubridate::yday(SFmdSH_seas2_cl) ~ as.numeric(SFmdSH_2_bag), TRUE ~ fluke_bag_y2), 
      
      fluke_min_y2=dplyr::case_when(mode == "fh" & date_adj >= lubridate::yday(SFmdFH_seas1_op) & date_adj <= lubridate::yday(SFmdFH_seas1_cl) ~ as.numeric(SFmdFH_1_len) * 2.54, TRUE ~ 254), 
      fluke_min_y2=dplyr::case_when(mode == "pr" & date_adj >= lubridate::yday(SFmdPR_seas1_op) & date_adj <= lubridate::yday(SFmdPR_seas1_cl) ~ as.numeric(SFmdPR_1_len) * 2.54, TRUE ~ fluke_min_y2), 
      fluke_min_y2=dplyr::case_when(mode == "sh" & date_adj >= lubridate::yday(SFmdSH_seas1_op) & date_adj <= lubridate::yday(SFmdSH_seas1_cl) ~ as.numeric(SFmdSH_1_len) * 2.54, TRUE ~ fluke_min_y2),
      
      fluke_min_y2=dplyr::case_when(mode == "fh" & date_adj >= lubridate::yday(SFmdFH_seas2_op) & date_adj <= lubridate::yday(SFmdFH_seas2_cl) ~ as.numeric(SFmdFH_2_len) * 2.54, TRUE ~ fluke_min_y2),
      fluke_min_y2=dplyr::case_when(mode == "pr" & date_adj >= lubridate::yday(SFmdPR_seas2_op) & date_adj <= lubridate::yday(SFmdPR_seas2_cl) ~ as.numeric(SFmdPR_2_len) * 2.54, TRUE ~ fluke_min_y2),
      fluke_min_y2=dplyr::case_when(mode == "sh" & date_adj >= lubridate::yday(SFmdSH_seas2_op) & date_adj <= lubridate::yday(SFmdSH_seas2_cl) ~ as.numeric(SFmdSH_2_len) * 2.54, TRUE ~ fluke_min_y2))
  
}



if (exists("BSBmd_seas1_op")) {
  directed_trips<- directed_trips %>%
    dplyr::mutate(#black sea bass
      bsb_bag_y2=dplyr::case_when(date_adj >= lubridate::yday(BSBmd_seas1_op) & date_adj <= lubridate::yday(BSBmd_seas1_cl) ~ as.numeric(BSBmd_1_bag), TRUE ~ 0), 
      bsb_min_y2=dplyr::case_when(date_adj >= lubridate::yday(BSBmd_seas1_op) & date_adj <= lubridate::yday(BSBmd_seas1_cl) ~ as.numeric(BSBmd_1_len) * 2.54, TRUE ~ 254), 
      bsb_bag_y2=dplyr::case_when(date_adj >= lubridate::yday(BSBmd_seas2_op) & date_adj <= lubridate::yday(BSBmd_seas2_cl) ~ as.numeric(BSBmd_2_bag), TRUE ~ bsb_bag_y2), 
      bsb_min_y2=dplyr::case_when(date_adj >= lubridate::yday(BSBmd_seas2_op) & date_adj <= lubridate::yday(BSBmd_seas2_cl) ~ as.numeric(BSBmd_2_len) * 2.54, TRUE ~ bsb_min_y2))
  
} else {
  directed_trips<- directed_trips %>%
    dplyr::mutate(
      bsb_bag_y2=dplyr::case_when(mode == "fh" & date_adj >= lubridate::yday(BSBmdFH_seas1_op) & date_adj <= lubridate::yday(BSBmdFH_seas1_cl) ~ as.numeric(BSBmdFH_1_bag), TRUE ~ 0), 
      bsb_bag_y2=dplyr::case_when(mode == "pr" & date_adj >= lubridate::yday(BSBmdPR_seas1_op) & date_adj <= lubridate::yday(BSBmdPR_seas1_cl) ~ as.numeric(BSBmdPR_1_bag), TRUE ~ bsb_bag_y2), 
      bsb_bag_y2=dplyr::case_when(mode == "sh" & date_adj >= lubridate::yday(BSBmdSH_seas1_op) & date_adj <= lubridate::yday(BSBmdSH_seas1_cl) ~ as.numeric(BSBmdSH_1_bag), TRUE ~ bsb_bag_y2),
      
      bsb_bag_y2=dplyr::case_when(mode == "fh" & date_adj >= lubridate::yday(BSBmdFH_seas2_op) & date_adj <= lubridate::yday(BSBmdFH_seas2_cl) ~ as.numeric(BSBmdFH_2_bag), TRUE ~ bsb_bag_y2),
      bsb_bag_y2=dplyr::case_when(mode == "pr" & date_adj >= lubridate::yday(BSBmdPR_seas2_op) & date_adj <= lubridate::yday(BSBmdPR_seas2_cl) ~ as.numeric(BSBmdPR_2_bag), TRUE ~ bsb_bag_y2),
      bsb_bag_y2=dplyr::case_when(mode == "sh" & date_adj >= lubridate::yday(BSBmdSH_seas2_op) & date_adj <= lubridate::yday(BSBmdSH_seas2_cl) ~ as.numeric(BSBmdSH_2_bag), TRUE ~ bsb_bag_y2), 
      
      bsb_min_y2=dplyr::case_when(mode == "fh" & date_adj >= lubridate::yday(BSBmdFH_seas1_op) & date_adj <= lubridate::yday(BSBmdFH_seas1_cl) ~ as.numeric(BSBmdFH_1_len) * 2.54, TRUE ~ 254), 
      bsb_min_y2=dplyr::case_when(mode == "pr" & date_adj >= lubridate::yday(BSBmdPR_seas1_op) & date_adj <= lubridate::yday(BSBmdPR_seas1_cl) ~ as.numeric(BSBmdPR_1_len) * 2.54, TRUE ~ bsb_min_y2), 
      bsb_min_y2=dplyr::case_when(mode == "sh" & date_adj >= lubridate::yday(BSBmdSH_seas1_op) & date_adj <= lubridate::yday(BSBmdSH_seas1_cl) ~ as.numeric(BSBmdSH_1_len) * 2.54, TRUE ~ bsb_min_y2),
      
      bsb_min_y2=dplyr::case_when(mode == "fh" & date_adj >= lubridate::yday(BSBmdFH_seas2_op) & date_adj <= lubridate::yday(BSBmdFH_seas2_cl) ~ as.numeric(BSBmdFH_2_len) * 2.54, TRUE ~ bsb_min_y2),
      bsb_min_y2=dplyr::case_when(mode == "pr" & date_adj >= lubridate::yday(BSBmdPR_seas2_op) & date_adj <= lubridate::yday(BSBmdPR_seas2_cl) ~ as.numeric(BSBmdPR_2_len) * 2.54, TRUE ~ bsb_min_y2),
      bsb_min_y2=dplyr::case_when(mode == "sh" & date_adj >= lubridate::yday(BSBmdSH_seas2_op) & date_adj <= lubridate::yday(BSBmdSH_seas2_cl) ~ as.numeric(BSBmdSH_2_len) * 2.54, TRUE ~ bsb_min_y2))
  
}

if (exists("SCUPmd_seas1_op")) {
  directed_trips<- directed_trips %>%
    dplyr::mutate(#Scup
      scup_bag_y2=dplyr::case_when(date_adj >= lubridate::yday(SCUPmd_seas1_op) & date_adj <= lubridate::yday(SCUPmd_seas1_cl) ~ as.numeric(SCUPmd_1_bag), TRUE ~ 0), 
      scup_min_y2=dplyr::case_when(date_adj >= lubridate::yday(SCUPmd_seas1_op) & date_adj <= lubridate::yday(SCUPmd_seas1_cl) ~ as.numeric(SCUPmd_1_len) * 2.54, TRUE ~ 254))
} else {
  directed_trips<- directed_trips %>%
    dplyr::mutate(
      scup_bag_y2=dplyr::case_when(mode == "fh" & date_adj >= lubridate::yday(SCUPmdFH_seas1_op) & date_adj <= lubridate::yday(SCUPmdFH_seas1_cl) ~ as.numeric(SCUPmdFH_1_bag), TRUE ~ 0), 
      scup_bag_y2=dplyr::case_when(mode == "pr" & date_adj >= lubridate::yday(SCUPmdPR_seas1_op) & date_adj <= lubridate::yday(SCUPmdPR_seas1_cl) ~ as.numeric(SCUPmdPR_1_bag), TRUE ~ scup_bag_y2), 
      scup_bag_y2=dplyr::case_when(mode == "sh" & date_adj >= lubridate::yday(SCUPmdSH_seas1_op) & date_adj <= lubridate::yday(SCUPmdSH_seas1_cl) ~ as.numeric(SCUPmdSH_1_bag), TRUE ~ scup_bag_y2),
      
      scup_min_y2=dplyr::case_when(mode == "fh" & date_adj >= lubridate::yday(SCUPmdFH_seas1_op) & date_adj <= lubridate::yday(SCUPmdFH_seas1_cl) ~ as.numeric(SCUPmdFH_1_len) * 2.54, TRUE ~ 254), 
      scup_min_y2=dplyr::case_when(mode == "pr" & date_adj >= lubridate::yday(SCUPmdPR_seas1_op) & date_adj <= lubridate::yday(SCUPmdPR_seas1_cl) ~ as.numeric(SCUPmdPR_1_len) * 2.54, TRUE ~ scup_min_y2), 
      scup_min_y2=dplyr::case_when(mode == "sh" & date_adj >= lubridate::yday(SCUPmdSH_seas1_op) & date_adj <= lubridate::yday(SCUPmdSH_seas1_cl) ~ as.numeric(SCUPmdSH_1_len) * 2.54, TRUE ~ scup_min_y2))
}

directed_trips<- directed_trips %>%
  dplyr::mutate(
    fluke_bag_y2=dplyr::case_when(mode == "fh" & date_adj >= lubridate::yday(SFmdFH_seas3_op) & date_adj <= lubridate::yday(SFmdFH_seas3_cl) ~ as.numeric(SFmdFH_3_bag), TRUE ~ fluke_bag_y2),
    fluke_bag_y2=dplyr::case_when(mode == "pr" & date_adj >= lubridate::yday(SFmdPR_seas3_op) & date_adj <= lubridate::yday(SFmdPR_seas3_cl) ~ as.numeric(SFmdPR_3_bag), TRUE ~ fluke_bag_y2),
    fluke_bag_y2=dplyr::case_when(mode == "sh" & date_adj >= lubridate::yday(SFmdSH_seas3_op) & date_adj <= lubridate::yday(SFmdSH_seas3_cl) ~ as.numeric(SFmdSH_3_bag), TRUE ~ fluke_bag_y2), 
    
    fluke_min_y2=dplyr::case_when(mode == "fh" & date_adj >= lubridate::yday(SFmdFH_seas3_op) & date_adj <= lubridate::yday(SFmdFH_seas3_cl) ~ as.numeric(SFmdFH_3_len) * 2.54, TRUE ~ fluke_min_y2),
    fluke_min_y2=dplyr::case_when(mode == "pr" & date_adj >= lubridate::yday(SFmdPR_seas3_op) & date_adj <= lubridate::yday(SFmdPR_seas3_cl) ~ as.numeric(SFmdPR_3_len) * 2.54, TRUE ~ fluke_min_y2),
    fluke_min_y2=dplyr::case_when(mode == "sh" & date_adj >= lubridate::yday(SFmdSH_seas3_op) & date_adj <= lubridate::yday(SFmdSH_seas3_cl) ~ as.numeric(SFmdSH_3_len) * 2.54, TRUE ~ fluke_min_y2),
    
    bsb_bag_y2=dplyr::case_when(mode == "fh" & date_adj >= lubridate::yday(BSBmdFH_seas3_op) & date_adj <= lubridate::yday(BSBmdFH_seas3_cl) ~ as.numeric(BSBmdFH_3_bag), TRUE ~ bsb_bag_y2),
    bsb_bag_y2=dplyr::case_when(mode == "pr" & date_adj >= lubridate::yday(BSBmdPR_seas3_op) & date_adj <= lubridate::yday(BSBmdPR_seas3_cl) ~ as.numeric(BSBmdPR_3_bag), TRUE ~ bsb_bag_y2),
    bsb_bag_y2=dplyr::case_when(mode == "sh" & date_adj >= lubridate::yday(BSBmdSH_seas3_op) & date_adj <= lubridate::yday(BSBmdSH_seas3_cl) ~ as.numeric(BSBmdSH_3_bag), TRUE ~ bsb_bag_y2), 
    
    bsb_min_y2=dplyr::case_when(mode == "fh" & date_adj >= lubridate::yday(BSBmdFH_seas3_op) & date_adj <= lubridate::yday(BSBmdFH_seas3_cl) ~ as.numeric(BSBmdFH_3_len) * 2.54, TRUE ~ bsb_min_y2),
    bsb_min_y2=dplyr::case_when(mode == "pr" & date_adj >= lubridate::yday(BSBmdPR_seas3_op) & date_adj <= lubridate::yday(BSBmdPR_seas3_cl) ~ as.numeric(BSBmdPR_3_len) * 2.54, TRUE ~ bsb_min_y2),
    bsb_min_y2=dplyr::case_when(mode == "sh" & date_adj >= lubridate::yday(BSBmdSH_seas3_op) & date_adj <= lubridate::yday(BSBmdSH_seas3_cl) ~ as.numeric(BSBmdSH_3_len) * 2.54, TRUE ~ bsb_min_y2),
    
    scup_bag_y2=dplyr::case_when(mode == "fh" & date_adj >= lubridate::yday(SCUPmdFH_seas2_op) & date_adj <= lubridate::yday(SCUPmdFH_seas2_cl) ~ as.numeric(SCUPmdFH_2_bag), TRUE ~ scup_bag_y2),
    scup_bag_y2=dplyr::case_when(mode == "pr" & date_adj >= lubridate::yday(SCUPmdPR_seas2_op) & date_adj <= lubridate::yday(SCUPmdPR_seas2_cl) ~ as.numeric(SCUPmdPR_2_bag), TRUE ~ scup_bag_y2),
    scup_bag_y2=dplyr::case_when(mode == "sh" & date_adj >= lubridate::yday(SCUPmdSH_seas2_op) & date_adj <= lubridate::yday(SCUPmdSH_seas2_cl) ~ as.numeric(SCUPmdSH_2_bag), TRUE ~ scup_bag_y2), 
    
    scup_min_y2=dplyr::case_when(mode == "fh" & date_adj >= lubridate::yday(SCUPmdFH_seas2_op) & date_adj <= lubridate::yday(SCUPmdFH_seas2_cl) ~ as.numeric(SCUPmdFH_2_len) * 2.54, TRUE ~ scup_min_y2),
    scup_min_y2=dplyr::case_when(mode == "pr" & date_adj >= lubridate::yday(SCUPmdPR_seas2_op) & date_adj <= lubridate::yday(SCUPmdPR_seas2_cl) ~ as.numeric(SCUPmdPR_2_len) * 2.54, TRUE ~ scup_min_y2),
    scup_min_y2=dplyr::case_when(mode == "sh" & date_adj >= lubridate::yday(SCUPmdSH_seas2_op) & date_adj <= lubridate::yday(SCUPmdSH_seas2_cl) ~ as.numeric(SCUPmdSH_2_len) * 2.54, TRUE ~ scup_min_y2))


#predictions_out10 <- data.frame()
#future::plan(future::multisession, workers = 36)
future::plan(future::multisession, workers = 25)
get_predictions_out<- function(x){
#for(x in 1:25){
  
  print(x)
  
  directed_trips2 <- directed_trips %>% 
    dplyr::filter(draw == x) # %>%
  # dplyr::mutate(day = stringr::str_extract(day, "^\\d{2}"), 
  #               period2 = paste0(month24, "-", day, "-", mode))
  
  catch_data <- feather::read_feather(file.path(data_path, paste0("proj_catch_draws_MD", "_", x,".feather"))) %>% 
    dplyr::left_join(directed_trips2, by=c("mode", "date", "draw")) 
  
  calendar_adjustments <- readr::read_csv(
    file.path(here::here(paste0("Data/proj_year_calendar_adjustments_new_MD.csv"))), show_col_types = FALSE) %>%
    dplyr::filter(draw==x) 
  
  
  base_outcomes0 <- list()
  n_choice_occasions0 <- list()
  
  mode_draw <- c("sh", "pr", "fh")
  for (md in mode_draw) {
    
    # pull trip outcomes from the calibration year
    base_outcomes0[[md]]<-feather::read_feather(file.path(data_path, paste0("base_outcomes_new_MD_", md, "_", x, ".feather"))) %>% 
      data.table::as.data.table()
    
    base_outcomes0[[md]]<-base_outcomes0[[md]] %>% 
      dplyr::select(-domain2) %>% 
      dplyr::mutate(date_parsed=lubridate::dmy(date)) %>% 
      dplyr::select(-date)
    
    # pull in data on the number of choice occasions per mode-day
    n_choice_occasions0[[md]]<-feather::read_feather(file.path(data_path, paste0("n_choice_occasions_new_MD_", md, "_", x, ".feather")))  
    n_choice_occasions0[[md]]<-n_choice_occasions0[[md]] %>% 
      dplyr::mutate(date_parsed=lubridate::dmy(date)) %>% 
      dplyr::select(-date)
    
  }
  
  base_outcomes <- dplyr::bind_rows(base_outcomes0)
  n_choice_occasions <- dplyr::bind_rows(n_choice_occasions0) %>% 
    dplyr::arrange(date_parsed, mode)
  rm(base_outcomes0, n_choice_occasions0)
  
  base_outcomes<-base_outcomes %>% 
    dplyr::arrange(date_parsed, mode, tripid, catch_draw)
  
  
  # Pull in calibration comparison information about trip-level harvest/discard re-allocations 
  calib_comparison<-readRDS(file.path(data_path,"calibrated_model_stats_new.rds")) %>%
    dplyr::filter(state=="MD" & draw==x )  
  
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
    stringr::str_detect(all_vars, paste0("(_", species_suffixes, ")$", collapse = "|"))
  ]
  
  id_vars <- setdiff(all_vars, species_specific_vars)
  
  calib_comparison<-calib_comparison %>% 
    dplyr::select(mode, all_of(species_specific_vars))
  
  # Extract base variable names (without _sf, _bsb, _scup)
  base_names <- unique(stringr::str_replace(species_specific_vars, "_(sf|bsb|scup)$", ""))
  
  # Pivot the data longer on the species-specific columns
  calib_comparison <- calib_comparison %>%
    tidyr::pivot_longer(
      cols = all_of(species_specific_vars),
      names_to = c(".value", "species"),
      names_pattern = "(.*)_(sf|bsb|scup)"
    ) %>% 
    dplyr::distinct()
  
  sf_size_data2 <- sf_size_data %>% 
    dplyr::filter(draw == x) %>%  #Change to X for model for sf and scup
    dplyr::select(-draw)
  
  ### Change when bsb_size is updated
  bsb_size_data2 <- bsb_size_data %>% 
    dplyr::filter(draw == x) %>% 
    dplyr::select(-draw)
  
  scup_size_data2 <- scup_size_data %>% 
    dplyr::filter(draw == x) %>% 
    dplyr::select(-draw)
  
  
  ## Run the predict catch function
  source(here::here("Code/sim/predict_rec_catch_functions.R"))
  source(here::here("Code/sim/predict_rec_catch.R"))
  
  test<- predict_rec_catch(st = "MD", dr = x,
                           directed_trips = directed_trips2, 
                           catch_data = catch_data, 
                           sf_size_data = sf_size_data2,
                           bsb_size_data = bsb_size_data2, 
                           scup_size_data = scup_size_data2, 
                           l_w_conversion = l_w_conversion,
                           calib_comparison = calib_comparison, 
                           n_choice_occasions = n_choice_occasions, 
                           calendar_adjustments = calendar_adjustments, 
                           base_outcomes = base_outcomes)
  
  test <- test %>% 
    dplyr::mutate(draw = c(x),
                  #model = c("Alt"))
                  model = c(Run_Name))
  
  #regs <- # Input table will be used to fill out regs in DT
  
  #predictions_out10<- predictions_out10 %>% rbind(test) 
}


print("out of loop")



# use furrr package to parallelize the get_predictions_out function 100 times
# This will spit out a dataframe with 100 predictions 
#predictions_out10<- furrr::future_map_dfr(1:100, ~get_predictions_out(.), .id = "draw")
predictions_out10<- furrr::future_map_dfr(1:25, ~get_predictions_out(.), .id = "draw")

#readr::write_csv(predictions_out10, file = here::here(paste0("output/output_MA_", Run_Name, "_", format(Sys.time(), "%Y%m%d_%H%M%S"),  ".csv")))
readr::write_csv(predictions_out10, file = here::here(paste0("output/output_MD_", Run_Name, "_", format(Sys.time(), "%Y%m%d_%H%M%S"),  ".csv")))


end_time <- Sys.time()

print(end_time - start_time)