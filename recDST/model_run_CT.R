##############################
### CT Rec model run  ########
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

print("start model_CT")
state1 = "CT"
predictions_all = list()

sf_size_data <- readr::read_csv(file.path(data_path, "fluke_projected_catch_at_lengths.csv"), show_col_types = FALSE)  %>% 
  dplyr::filter(state == "CT") %>% 
  dplyr::filter(!is.na(fitted_prob)) %>% 
  dplyr::select(state, fitted_prob, length, draw)


bsb_size_data <- readr::read_csv(file.path(data_path, "bsb_projected_catch_at_lengths.csv"), show_col_types = FALSE)  %>% 
  dplyr::filter(state == "CT") %>% 
  dplyr::filter(!is.na(fitted_prob)) %>% 
  dplyr::select(state, fitted_prob, length, draw)


scup_size_data <- readr::read_csv(file.path(data_path, "scup_projected_catch_at_lengths.csv"), show_col_types = FALSE)  %>% 
  dplyr::filter(state == "CT") %>% 
  dplyr::filter(!is.na(fitted_prob)) %>% 
  dplyr::select(state, fitted_prob, length, draw)

l_w_conversion <-readr::read_csv(file.path(here::here("data-raw/size_data/L_W_Conversion.csv")),  show_col_types = FALSE) %>%
  dplyr::filter(State=="CT") 


directed_trips<-feather::read_feather(file.path(data_path, paste0("directed_trips_calibration_CT.feather"))) %>% 
  tibble::tibble() %>%
  dplyr::select(mode, date, draw, bsb_bag_y2, bsb_min_y2, fluke_bag,fluke_min, scup_bag_y2, scup_min_y2,
                bsb_bag_y2_y2, bsb_min_y2_y2, fluke_bag_y2,fluke_min_y2, scup_bag_y2_y2, scup_min_y2_y2) %>% 
  dplyr::mutate(date_adj = lubridate::dmy(date), 
                date_adj = lubridate::yday(date_adj)) 

if (exists("SFct_seas1_op")) {
  directed_trips<- directed_trips %>%
    dplyr::mutate(#Summer Flounder
      fluke_bag_y2=dplyr::case_when(day_i >= lubridate::yday(SFct_seas1_op) & day_i <= lubridate::yday(SFct_seas1_cl) ~ as.numeric(SFct_1_bag), TRUE ~ 0), 
      fluke_min_y2=dplyr::case_when(day_i >= lubridate::yday(SFct_seas1_op) & day_i <= lubridate::yday(SFct_seas1_cl) ~ as.numeric(SFct_1_len) * 2.54, TRUE ~ 254), 
      fluke_bag_y2=dplyr::case_when(day_i >= lubridate::yday(SFct_seas2_op) & day_i <= lubridate::yday(SFct_seas2_cl) ~ as.numeric(SFct_2_bag), TRUE ~ fluke_bag_y2), 
      fluke_min_y2=dplyr::case_when(day_i >= lubridate::yday(SFct_seas2_op) & day_i <= lubridate::yday(SFct_seas2_cl) ~ as.numeric(SFct_2_len) * 2.54, TRUE ~ fluke_min_y2))
} else {
  directed_trips<- directed_trips %>%
    dplyr::mutate(
      fluke_bag_y2=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(SFctFH_seas1_op) & day_i <= lubridate::yday(SFctFH_seas1_cl) ~ as.numeric(SFctFH_1_bag), TRUE ~ 0), 
      fluke_bag_y2=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(SFctPR_seas1_op) & day_i <= lubridate::yday(SFctPR_seas1_cl) ~ as.numeric(SFctPR_1_bag), TRUE ~ fluke_bag_y2), 
      fluke_bag_y2=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(SFctSH_seas1_op) & day_i <= lubridate::yday(SFctSH_seas1_cl) ~ as.numeric(SFctSH_1_bag), TRUE ~ fluke_bag_y2),
      
      fluke_bag_y2=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(SFctFH_seas2_op) & day_i <= lubridate::yday(SFctFH_seas2_cl) ~ as.numeric(SFctFH_2_bag), TRUE ~ fluke_bag_y2),
      fluke_bag_y2=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(SFctPR_seas2_op) & day_i <= lubridate::yday(SFctPR_seas2_cl) ~ as.numeric(SFctPR_2_bag), TRUE ~ fluke_bag_y2),
      fluke_bag_y2=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(SFctSH_seas2_op) & day_i <= lubridate::yday(SFctSH_seas2_cl) ~ as.numeric(SFctSH_2_bag), TRUE ~ fluke_bag_y2), 
      
      
      fluke_min_y2=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(SFctFH_seas1_op) & day_i <= lubridate::yday(SFctFH_seas1_cl) ~ as.numeric(SFctFH_1_len) * 2.54, TRUE ~ 254), 
      fluke_min_y2=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(SFctPR_seas1_op) & day_i <= lubridate::yday(SFctPR_seas1_cl) ~ as.numeric(SFctPR_1_len) * 2.54, TRUE ~ fluke_min_y2), 
      fluke_min_y2=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(SFctSH_seas1_op) & day_i <= lubridate::yday(SFctSH_seas1_cl) ~ as.numeric(SFctSH_1_len) * 2.54, TRUE ~ fluke_min_y2),
      
      fluke_min_y2=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(SFctFH_seas2_op) & day_i <= lubridate::yday(SFctFH_seas2_cl) ~ as.numeric(SFctFH_2_len) * 2.54, TRUE ~ fluke_min_y2),
      fluke_min_y2=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(SFctPR_seas2_op) & day_i <= lubridate::yday(SFctPR_seas2_cl) ~ as.numeric(SFctPR_2_len) * 2.54, TRUE ~ fluke_min_y2),
      fluke_min_y2=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(SFctSH_seas2_op) & day_i <= lubridate::yday(SFctSH_seas2_cl) ~ as.numeric(SFctSH_2_len) * 2.54, TRUE ~ fluke_min_y2))
  
}



directed_trips<- directed_trips %>%
  dplyr::mutate(
    fluke_bag_y2=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(SFctFH_seas3_op) & day_i <= lubridate::yday(SFctFH_seas3_cl) ~ as.numeric(SFctFH_3_bag), TRUE ~ fluke_bag_y2),
    fluke_bag_y2=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(SFctPR_seas3_op) & day_i <= lubridate::yday(SFctPR_seas3_cl) ~ as.numeric(SFctPR_3_bag), TRUE ~ fluke_bag_y2),
    fluke_bag_y2=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(SFctSH_seas3_op) & day_i <= lubridate::yday(SFctSH_seas3_cl) ~ as.numeric(SFctSH_3_bag), TRUE ~ fluke_bag_y2),

    fluke_min_y2=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(SFctFH_seas3_op) & day_i <= lubridate::yday(SFctFH_seas3_cl) ~ as.numeric(SFctFH_3_len) * 2.54, TRUE ~ fluke_min_y2),
    fluke_min_y2=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(SFctPR_seas3_op) & day_i <= lubridate::yday(SFctPR_seas3_cl) ~ as.numeric(SFctPR_3_len) * 2.54, TRUE ~ fluke_min_y2),
    fluke_min_y2=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(SFctSH_seas3_op) & day_i <= lubridate::yday(SFctSH_seas3_cl) ~ as.numeric(SFctSH_3_len) * 2.54, TRUE ~ fluke_min_y2),
    # Black Sea Bass Bag Limit by Mode
    bsb_bag_y2=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(BSBctFH_seas1_op) & day_i <= lubridate::yday(BSBctFH_seas1_cl) ~ as.numeric(BSBctFH_1_bag), TRUE ~ 0), 
    bsb_bag_y2=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(BSBctPR_seas1_op) & day_i <= lubridate::yday(BSBctPR_seas1_cl) ~ as.numeric(BSBctPR_1_bag), TRUE ~ bsb_bag_y2), 
    bsb_bag_y2=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(BSBctSH_seas1_op) & day_i <= lubridate::yday(BSBctSH_seas1_cl) ~ as.numeric(BSBctSH_1_bag), TRUE ~ bsb_bag_y2),
    bsb_bag_y2=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(BSBctFH_seas2_op) & day_i <= lubridate::yday(BSBctFH_seas2_cl) ~ as.numeric(BSBctFH_2_bag), TRUE ~ bsb_bag_y2), 
    bsb_bag_y2=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(BSBctPR_seas2_op) & day_i <= lubridate::yday(BSBctPR_seas2_cl) ~ as.numeric(BSBctPR_2_bag), TRUE ~ bsb_bag_y2), 
    bsb_bag_y2=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(BSBctSH_seas2_op) & day_i <= lubridate::yday(BSBctSH_seas2_cl) ~ as.numeric(BSBctSH_2_bag), TRUE ~ bsb_bag_y2),
    bsb_bag_y2=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(BSBctFH_seas3_op) & day_i <= lubridate::yday(BSBctFH_seas3_cl) ~ as.numeric(BSBctFH_3_bag), TRUE ~ bsb_bag_y2), 
    bsb_bag_y2=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(BSBctPR_seas3_op) & day_i <= lubridate::yday(BSBctPR_seas3_cl) ~ as.numeric(BSBctPR_3_bag), TRUE ~ bsb_bag_y2), 
    bsb_bag_y2=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(BSBctSH_seas3_op) & day_i <= lubridate::yday(BSBctSH_seas3_cl) ~ as.numeric(BSBctSH_3_bag), TRUE ~ bsb_bag_y2),
    # Black Sea Bass Minimum Length by Mode
    bsb_min_y2=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(BSBctFH_seas1_op) & day_i <= lubridate::yday(BSBctFH_seas1_cl) ~ as.numeric(BSBctFH_1_len) * 2.54, TRUE ~ 254), 
    bsb_min_y2=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(BSBctPR_seas1_op) & day_i <= lubridate::yday(BSBctPR_seas1_cl) ~ as.numeric(BSBctPR_1_len) * 2.54, TRUE ~ bsb_min_y2), 
    bsb_min_y2=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(BSBctSH_seas1_op) & day_i <= lubridate::yday(BSBctSH_seas1_cl) ~ as.numeric(BSBctSH_1_len) * 2.54, TRUE ~ bsb_min_y2),
    bsb_min_y2=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(BSBctFH_seas2_op) & day_i <= lubridate::yday(BSBctFH_seas2_cl) ~ as.numeric(BSBctFH_2_len) * 2.54, TRUE ~ bsb_min_y2), 
    bsb_min_y2=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(BSBctPR_seas2_op) & day_i <= lubridate::yday(BSBctPR_seas2_cl) ~ as.numeric(BSBctPR_2_len) * 2.54, TRUE ~ bsb_min_y2), 
    bsb_min_y2=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(BSBctSH_seas2_op) & day_i <= lubridate::yday(BSBctSH_seas2_cl) ~ as.numeric(BSBctSH_2_len) * 2.54, TRUE ~ bsb_min_y2), 
    bsb_min_y2=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(BSBctFH_seas3_op) & day_i <= lubridate::yday(BSBctFH_seas3_cl) ~ as.numeric(BSBctFH_3_len) * 2.54, TRUE ~ bsb_min_y2), 
    bsb_min_y2=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(BSBctPR_seas3_op) & day_i <= lubridate::yday(BSBctPR_seas3_cl) ~ as.numeric(BSBctPR_3_len) * 2.54, TRUE ~ bsb_min_y2), 
    bsb_min_y2=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(BSBctSH_seas3_op) & day_i <= lubridate::yday(BSBctSH_seas3_cl) ~ as.numeric(BSBctSH_3_len) * 2.54, TRUE ~ bsb_min_y2),

    scup_bag_y2=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(SCUPctFH_seas1_op) & day_i <= lubridate::yday(SCUPctFH_seas1_cl) ~ as.numeric(SCUPctFH_1_bag), TRUE ~ 0), 
    scup_min_y2=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(SCUPctFH_seas1_op) & day_i <= lubridate::yday(SCUPctFH_seas1_cl) ~ as.numeric(SCUPctFH_1_len) * 2.54, TRUE ~ 254),
    scup_bag_y2=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(SCUPctFH_seas2_op) & day_i <= lubridate::yday(SCUPctFH_seas2_cl) ~ as.numeric(SCUPctFH_2_bag), TRUE ~ scup_bag_y2),
    scup_min_y2=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(SCUPctFH_seas2_op) & day_i <= lubridate::yday(SCUPctFH_seas2_cl) ~ as.numeric(SCUPctFH_2_len) * 2.54, TRUE ~ scup_min_y2),
    scup_bag_y2=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(SCUPctFH_seas3_op) & day_i <= lubridate::yday(SCUPctFH_seas3_cl) ~ as.numeric(SCUPctFH_3_bag), TRUE ~ scup_bag_y2),
    scup_min_y2=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(SCUPctFH_seas3_op) & day_i <= lubridate::yday(SCUPctFH_seas3_cl) ~ as.numeric(SCUPctFH_3_len) * 2.54, TRUE ~ scup_min_y2),
    scup_bag_y2=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(SCUPctFH_seas4_op) & day_i <= lubridate::yday(SCUPctFH_seas4_cl) ~ as.numeric(SCUPctFH_4_bag), TRUE ~ scup_bag_y2),
    scup_min_y2=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(SCUPctFH_seas4_op) & day_i <= lubridate::yday(SCUPctFH_seas4_cl) ~ as.numeric(SCUPctFH_4_len) * 2.54, TRUE ~ scup_min_y2),
    
    scup_bag_y2=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(SCUPctPR_seas1_op) & day_i <= lubridate::yday(SCUPctPR_seas1_cl) ~ as.numeric(SCUPctPR_1_bag), TRUE ~ scup_bag_y2),
    scup_min_y2=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(SCUPctPR_seas1_op) & day_i <= lubridate::yday(SCUPctPR_seas1_cl) ~ as.numeric(SCUPctPR_1_len) * 2.54, TRUE ~ scup_min_y2),
    scup_bag_y2=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(SCUPctPR_seas2_op) & day_i <= lubridate::yday(SCUPctPR_seas2_cl) ~ as.numeric(SCUPctPR_2_bag), TRUE ~ scup_bag_y2),
    scup_min_y2=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(SCUPctPR_seas2_op) & day_i <= lubridate::yday(SCUPctPR_seas2_cl) ~ as.numeric(SCUPctPR_2_len) * 2.54, TRUE ~ scup_min_y2),
    
    scup_bag_y2=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(SCUPctSH_seas1_op) & day_i <= lubridate::yday(SCUPctSH_seas1_cl) ~ as.numeric(SCUPctSH_1_bag), TRUE ~ scup_bag_y2),
    scup_min_y2=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(SCUPctSH_seas1_op) & day_i <= lubridate::yday(SCUPctSH_seas1_cl) ~ as.numeric(SCUPctSH_1_len) * 2.54, TRUE ~ scup_min_y2),
    scup_bag_y2=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(SCUPctSH_seas2_op) & day_i <= lubridate::yday(SCUPctSH_seas2_cl) ~ as.numeric(SCUPctSH_2_bag), TRUE ~ scup_bag_y2),
    scup_min_y2=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(SCUPctSH_seas2_op) & day_i <= lubridate::yday(SCUPctSH_seas2_cl) ~ as.numeric(SCUPctSH_2_len) * 2.54, TRUE ~ scup_min_y2))


predictions_out10<- data.frame()
for(x in 1:1){
# future::plan(future::multisession, workers = 36)
# get_predictions_out<- function(x){
  
  
  print(x)
  
  directed_trips <- directed_trips %>% 
    dplyr::filter(draw == x) # %>%
  # dplyr::mutate(day = stringr::str_extract(day, "^\\d{2}"), 
  #               period2 = paste0(month24, "-", day, "-", mode))
  
  catch_data <- feather::read_feather(file.path(data_path, paste0("projected_catch_draws_CT", "_", x,".feather"))) %>% 
    dplyr::left_join(directed_trips, by=c("mode", "date", "draw")) 
  
  catch_data<-catch_data %>% 
    dplyr::select(-cost, -total_trips_12, -age, -bsb_keep_sim, -bsb_rel_sim, -day_i, -my_dom_id_string, 
                  -scup_keep_sim, -scup_rel_sim, -sf_keep_sim, -sf_rel_sim, -wave)
  
  
  base_outcomes0 <- list()
  n_choice_occasions0 <- list()
  
  mode_draw <- c("sh", "pr", "fh")
  for (md in mode_draw) {
    
    # pull trip outcomes from the calibration year
    base_outcomes0[[md]]<-feather::read_feather(file.path(data_path, paste0("base_outcomes_CT_", md, "_", x, ".feather"))) %>% 
      data.table::as.data.table()
    
    base_outcomes0[[md]]<-base_outcomes0[[md]] %>% 
      dplyr::select(-domain2) %>% 
      dplyr::mutate(date_parsed=lubridate::dmy(date)) %>% 
      dplyr::select(-date)
    
    # pull in data on the number of choice occasions per mode-day
    n_choice_occasions0[[md]]<-feather::read_feather(file.path(data_path, paste0("n_choice_occasions_CT_", md, "_", x, ".feather")))  
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
  calib_comparison<-feather::read_feather(file.path(data_path, "calibration_comparison.feather")) %>% 
    dplyr::filter(state=="CT" & draw==x )   
  
  sf_size_data <- sf_size_data %>% 
    dplyr::filter(draw == x) %>%  #Change to X for model for sf and scup
    dplyr::select(-draw)
  
  ### Change when bsb_size is updated
  bsb_size_data <- bsb_size_data %>% 
    dplyr::filter(draw == 0) %>% 
    dplyr::select(-draw)
  
  scup_size_data <- scup_size_data %>% 
    dplyr::filter(draw == x) %>% 
    dplyr::select(-draw)
  
  
  ## Run the predict catch function
  source(here::here("Code/sim/predict_rec_catch.R"))
  
  test<- predict_rec_catch(st = "CT", dr = x,
                           directed_trips, catch_data, 
                           sf_size_data, bsb_size_data, scup_size_data, 
                           l_w_conversion, calib_comparison, n_choice_occasions, 
                           base_outcomes)
  
  test <- test %>% 
    dplyr::mutate(draw = c(x),
                  #model = c("Alt"))
                  model = c(Run_Name))
  
  #regs <- # Input table will be used to fill out regs in DT
  
  predictions_out10<- predictions_out10 %>% rbind(test) 
}

print("out of loop")



# use furrr package to parallelize the get_predictions_out function 100 times
# This will spit out a dataframe with 100 predictions 
#predictions_out10<- furrr::future_map_dfr(1:100, ~get_predictions_out(.), .id = "draw")
#predictions_out10<- furrr::future_map_dfr(1:3, ~get_predictions_out(.), .id = "draw")

#readr::write_csv(predictions_out10, file = here::here(paste0("output/output_MA_", Run_Name, "_", format(Sys.time(), "%Y%m%d_%H%M%S"),  ".csv")))
readr::write_csv(predictions_out10, file = here::here(paste0("output/output_CT_Alt_", format(Sys.time(), "%Y%m%d_%H%M%S"),  ".csv")))


end_time <- Sys.time()

print(end_time - start_time)