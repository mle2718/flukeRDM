##############################
### VA Rec model run  ########
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

print("start model_VA")
state1 = "VA"
predictions_all = list()

data_path <- here::here("Data/")


#### Read in size data ####
sf_size_data <- readr::read_csv(file.path(data_path, "fluke_projected_catch_at_lengths.csv"), show_col_types = FALSE)  %>% 
  dplyr::filter(state == "VA") %>% 
  dplyr::filter(!is.na(fitted_prob)) %>% 
  dplyr::select(state, fitted_prob, length, draw)

bsb_size_data <- readr::read_csv(file.path(data_path, "bsb_projected_catch_at_lengths.csv"), show_col_types = FALSE)  %>% 
  dplyr::filter(state == "VA") %>% 
  dplyr::filter(!is.na(fitted_prob)) %>% 
  dplyr::select(state, fitted_prob, length, draw)

scup_size_data <- readr::read_csv(file.path(data_path, "scup_projected_catch_at_lengths.csv"), show_col_types = FALSE)  %>% 
  dplyr::filter(state == "VA") %>% 
  dplyr::filter(!is.na(fitted_prob)) %>% 
  dplyr::select(state,  fitted_prob, length, draw)


l_w_conversion <- readr::read_csv(file.path(data_path, "L_W_Conversion.csv"), show_col_types = FALSE)  %>% 
  dplyr::filter(state=="VA")

#### directed trips ####
directed_trips<-feather::read_feather(file.path(data_path, paste0("directed_trips_calibration_VA.feather"))) %>% 
  tibble::tibble() %>%
  dplyr::select(mode, date, draw, bsb_bag_y2, bsb_min_y2, fluke_bag,fluke_min, scup_bag_y2, scup_min_y2,
                bsb_bag_y2_y2, bsb_min_y2_y2, fluke_bag_y2,fluke_min_y2, scup_bag_y2_y2, scup_min_y2_y2) %>% 
  dplyr::mutate(date_adj = lubridate::dmy(date), 
                date_adj = lubridate::yday(date_adj)) 

if (exists("SFva_seas1_op")) {
  directed_trips<- directed_trips %>%
    dplyr::mutate(#Summer Flounder
      fluke_bag_y2=dplyr::case_when(day_i >= lubridate::yday(SFva_seas1_op) & day_i <= lubridate::yday(SFva_seas1_cl) ~ as.numeric(SFva_1_bag), TRUE ~ 0), 
      fluke_min_y2=dplyr::case_when(day_i >= lubridate::yday(SFva_seas1_op) & day_i <= lubridate::yday(SFva_seas1_cl) ~ as.numeric(SFva_1_len) * 2.54, TRUE ~ 254), 
      fluke_bag_y2=dplyr::case_when(day_i >= lubridate::yday(SFva_seas2_op) & day_i <= lubridate::yday(SFva_seas2_cl) ~ as.numeric(SFva_2_bag), TRUE ~ fluke_bag_y2), 
      fluke_min_y2=dplyr::case_when(day_i >= lubridate::yday(SFva_seas2_op) & day_i <= lubridate::yday(SFva_seas2_cl) ~ as.numeric(SFva_2_len) * 2.54, TRUE ~ fluke_min_y2))
} else {
  directed_trips<- directed_trips %>%
    dplyr::mutate(
      fluke_bag_y2=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(SFvaFH_seas1_op) & day_i <= lubridate::yday(SFvaFH_seas1_cl) ~ as.numeric(SFvaFH_1_bag), TRUE ~ 0), 
      fluke_bag_y2=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(SFvaPR_seas1_op) & day_i <= lubridate::yday(SFvaPR_seas1_cl) ~ as.numeric(SFvaPR_1_bag), TRUE ~ fluke_bag_y2), 
      fluke_bag_y2=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(SFvaSH_seas1_op) & day_i <= lubridate::yday(SFvaSH_seas1_cl) ~ as.numeric(SFvaSH_1_bag), TRUE ~ fluke_bag_y2),
      
      fluke_bag_y2=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(SFvaFH_seas2_op) & day_i <= lubridate::yday(SFvaFH_seas2_cl) ~ as.numeric(SFvaFH_2_bag), TRUE ~ fluke_bag_y2),
      fluke_bag_y2=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(SFvaPR_seas2_op) & day_i <= lubridate::yday(SFvaPR_seas2_cl) ~ as.numeric(SFvaPR_2_bag), TRUE ~ fluke_bag_y2),
      fluke_bag_y2=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(SFvaSH_seas2_op) & day_i <= lubridate::yday(SFvaSH_seas2_cl) ~ as.numeric(SFvaSH_2_bag), TRUE ~ fluke_bag_y2),
      
      fluke_min_y2=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(SFvaFH_seas1_op) & day_i <= lubridate::yday(SFvaFH_seas1_cl) ~ as.numeric(SFvaFH_1_len) * 2.54, TRUE ~ 254), 
      fluke_min_y2=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(SFvaPR_seas1_op) & day_i <= lubridate::yday(SFvaPR_seas1_cl) ~ as.numeric(SFvaPR_1_len) * 2.54, TRUE ~ fluke_min_y2), 
      fluke_min_y2=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(SFvaSH_seas1_op) & day_i <= lubridate::yday(SFvaSH_seas1_cl) ~ as.numeric(SFvaSH_1_len) * 2.54, TRUE ~ fluke_min_y2),
      
      fluke_min_y2=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(SFvaFH_seas2_op) & day_i <= lubridate::yday(SFvaFH_seas2_cl) ~ as.numeric(SFvaFH_2_len) * 2.54, TRUE ~ fluke_min_y2),
      fluke_min_y2=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(SFvaPR_seas2_op) & day_i <= lubridate::yday(SFvaPR_seas2_cl) ~ as.numeric(SFvaPR_2_len) * 2.54, TRUE ~ fluke_min_y2),
      fluke_min_y2=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(SFvaSH_seas2_op) & day_i <= lubridate::yday(SFvaSH_seas2_cl) ~ as.numeric(SFvaSH_2_len) * 2.54, TRUE ~ fluke_min_y2))
}


if (exists("BSBva_seas1_op")) {
  directed_trips<- directed_trips %>%
    dplyr::mutate(#black sea bass
      bsb_bag_y2=dplyr::case_when(day_i >= lubridate::yday(BSBva_seas1_op) & day_i <= lubridate::yday(BSBva_seas1_cl) ~ as.numeric(BSBva_1_bag), TRUE ~ 0), 
      bsb_min_y2=dplyr::case_when(day_i >= lubridate::yday(BSBva_seas1_op) & day_i <= lubridate::yday(BSBva_seas1_cl) ~ as.numeric(BSBva_1_len) * 2.54, TRUE ~ 254), 
      bsb_bag_y2=dplyr::case_when(day_i >= lubridate::yday(BSBva_seas2_op) & day_i <= lubridate::yday(BSBva_seas2_cl) ~ as.numeric(BSBva_2_bag), TRUE ~ bsb_bag_y2), 
      bsb_min_y2=dplyr::case_when(day_i >= lubridate::yday(BSBva_seas2_op) & day_i <= lubridate::yday(BSBva_seas2_cl) ~ as.numeric(BSBva_2_len) * 2.54, TRUE ~ bsb_min_y2))
      
} else {
  directed_trips<- directed_trips %>%
    dplyr::mutate(
      bsb_bag_y2=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(BSBvaFH_seas1_op) & day_i <= lubridate::yday(BSBvaFH_seas1_cl) ~ as.numeric(BSBvaFH_1_bag), TRUE ~ 0), 
      bsb_bag_y2=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(BSBvaPR_seas1_op) & day_i <= lubridate::yday(BSBvaPR_seas1_cl) ~ as.numeric(BSBvaPR_1_bag), TRUE ~ bsb_bag_y2), 
      bsb_bag_y2=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(BSBvaSH_seas1_op) & day_i <= lubridate::yday(BSBvaSH_seas1_cl) ~ as.numeric(BSBvaSH_1_bag), TRUE ~ bsb_bag_y2),
      
      bsb_bag_y2=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(BSBvaFH_seas2_op) & day_i <= lubridate::yday(BSBvaFH_seas2_cl) ~ as.numeric(BSBvaFH_2_bag), TRUE ~ bsb_bag_y2),
      bsb_bag_y2=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(BSBvaPR_seas2_op) & day_i <= lubridate::yday(BSBvaPR_seas2_cl) ~ as.numeric(BSBvaPR_2_bag), TRUE ~ bsb_bag_y2),
      bsb_bag_y2=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(BSBvaSH_seas2_op) & day_i <= lubridate::yday(BSBvaSH_seas2_cl) ~ as.numeric(BSBvaSH_2_bag), TRUE ~ bsb_bag_y2), 
      
      bsb_min_y2=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(BSBvaFH_seas1_op) & day_i <= lubridate::yday(BSBvaFH_seas1_cl) ~ as.numeric(BSBvaFH_1_len) * 2.54, TRUE ~ 254), 
      bsb_min_y2=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(BSBvaPR_seas1_op) & day_i <= lubridate::yday(BSBvaPR_seas1_cl) ~ as.numeric(BSBvaPR_1_len) * 2.54, TRUE ~ bsb_min_y2), 
      bsb_min_y2=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(BSBvaSH_seas1_op) & day_i <= lubridate::yday(BSBvaSH_seas1_cl) ~ as.numeric(BSBvaSH_1_len) * 2.54, TRUE ~ bsb_min_y2),
      
      bsb_min_y2=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(BSBvaFH_seas2_op) & day_i <= lubridate::yday(BSBvaFH_seas2_cl) ~ as.numeric(BSBvaFH_2_len) * 2.54, TRUE ~ bsb_min_y2),
      bsb_min_y2=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(BSBvaPR_seas2_op) & day_i <= lubridate::yday(BSBvaPR_seas2_cl) ~ as.numeric(BSBvaPR_2_len) * 2.54, TRUE ~ bsb_min_y2),
      bsb_min_y2=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(BSBvaSH_seas2_op) & day_i <= lubridate::yday(BSBvaSH_seas2_cl) ~ as.numeric(BSBvaSH_2_len) * 2.54, TRUE ~ bsb_min_y2))
  
}


if (exists("SCUPva_seas1_op")) {
  directed_trips<- directed_trips %>%
    dplyr::mutate(#Scup
      scup_bag_y2=dplyr::case_when(day_i >= lubridate::yday(SCUPva_seas1_op) & day_i <= lubridate::yday(SCUPva_seas1_cl) ~ as.numeric(SCUPva_1_bag), TRUE ~ 0), 
      scup_min_y2=dplyr::case_when(day_i >= lubridate::yday(SCUPva_seas1_op) & day_i <= lubridate::yday(SCUPva_seas1_cl) ~ as.numeric(SCUPva_1_len) * 2.54, TRUE ~ 254))
} else {
  directed_trips<- directed_trips %>%
    dplyr::mutate(
      scup_bag_y2=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(SCUPvaFH_seas1_op) & day_i <= lubridate::yday(SCUPvaFH_seas1_cl) ~ as.numeric(SCUPvaFH_1_bag), TRUE ~ 0), 
      scup_bag_y2=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(SCUPvaPR_seas1_op) & day_i <= lubridate::yday(SCUPvaPR_seas1_cl) ~ as.numeric(SCUPvaPR_1_bag), TRUE ~ scup_bag_y2), 
      scup_bag_y2=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(SCUPvaSH_seas1_op) & day_i <= lubridate::yday(SCUPvaSH_seas1_cl) ~ as.numeric(SCUPvaSH_1_bag), TRUE ~ scup_bag_y2),
      
      scup_min_y2=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(SCUPvaFH_seas1_op) & day_i <= lubridate::yday(SCUPvaFH_seas1_cl) ~ as.numeric(SCUPvaFH_1_len) * 2.54, TRUE ~ 254), 
      scup_min_y2=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(SCUPvaPR_seas1_op) & day_i <= lubridate::yday(SCUPvaPR_seas1_cl) ~ as.numeric(SCUPvaPR_1_len) * 2.54, TRUE ~ scup_min_y2), 
      scup_min_y2=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(SCUPvaSH_seas1_op) & day_i <= lubridate::yday(SCUPvaSH_seas1_cl) ~ as.numeric(SCUPvaSH_1_len) * 2.54, TRUE ~ scup_min_y2))
}

directed_trips<- directed_trips %>% 
  dplyr::mutate(
    fluke_bag_y2=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(SFvaFH_seas3_op) & day_i <= lubridate::yday(SFvaFH_seas3_cl) ~ as.numeric(SFvaFH_3_bag), TRUE ~ fluke_bag_y2),
    fluke_bag_y2=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(SFvaPR_seas3_op) & day_i <= lubridate::yday(SFvaPR_seas3_cl) ~ as.numeric(SFvaPR_3_bag), TRUE ~ fluke_bag_y2),
    fluke_bag_y2=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(SFvaSH_seas3_op) & day_i <= lubridate::yday(SFvaSH_seas3_cl) ~ as.numeric(SFvaSH_3_bag), TRUE ~ fluke_bag_y2), 
    
    fluke_min_y2=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(SFvaFH_seas3_op) & day_i <= lubridate::yday(SFvaFH_seas3_cl) ~ as.numeric(SFvaFH_3_len) * 2.54, TRUE ~ fluke_min_y2),
    fluke_min_y2=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(SFvaPR_seas3_op) & day_i <= lubridate::yday(SFvaPR_seas3_cl) ~ as.numeric(SFvaPR_3_len) * 2.54, TRUE ~ fluke_min_y2),
    fluke_min_y2=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(SFvaSH_seas3_op) & day_i <= lubridate::yday(SFvaSH_seas3_cl) ~ as.numeric(SFvaSH_3_len) * 2.54, TRUE ~ fluke_min_y2),
    
    bsb_bag_y2=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(BSBvaFH_seas3_op) & day_i <= lubridate::yday(BSBvaFH_seas3_cl) ~ as.numeric(BSBvaFH_3_bag), TRUE ~ bsb_bag_y2),
    bsb_bag_y2=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(BSBvaPR_seas3_op) & day_i <= lubridate::yday(BSBvaPR_seas3_cl) ~ as.numeric(BSBvaPR_3_bag), TRUE ~ bsb_bag_y2),
    bsb_bag_y2=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(BSBvaSH_seas3_op) & day_i <= lubridate::yday(BSBvaSH_seas3_cl) ~ as.numeric(BSBvaSH_3_bag), TRUE ~ bsb_bag_y2), 
    
    bsb_min_y2=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(BSBvaFH_seas3_op) & day_i <= lubridate::yday(BSBvaFH_seas3_cl) ~ as.numeric(BSBvaFH_3_len) * 2.54, TRUE ~ bsb_min_y2),
    bsb_min_y2=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(BSBvaPR_seas3_op) & day_i <= lubridate::yday(BSBvaPR_seas3_cl) ~ as.numeric(BSBvaPR_3_len) * 2.54, TRUE ~ bsb_min_y2),
    bsb_min_y2=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(BSBvaSH_seas3_op) & day_i <= lubridate::yday(BSBvaSH_seas3_cl) ~ as.numeric(BSBvaSH_3_len) * 2.54, TRUE ~ bsb_min_y2),
    
    scup_bag_y2=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(SCUPvaFH_seas2_op) & day_i <= lubridate::yday(SCUPvaFH_seas2_cl) ~ as.numeric(SCUPvaFH_2_bag), TRUE ~ scup_bag_y2),
    scup_bag_y2=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(SCUPvaPR_seas2_op) & day_i <= lubridate::yday(SCUPvaPR_seas2_cl) ~ as.numeric(SCUPvaPR_2_bag), TRUE ~ scup_bag_y2),
    scup_bag_y2=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(SCUPvaSH_seas2_op) & day_i <= lubridate::yday(SCUPvaSH_seas2_cl) ~ as.numeric(SCUPvaSH_2_bag), TRUE ~ scup_bag_y2), 
    
    scup_min_y2=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(SCUPvaFH_seas2_op) & day_i <= lubridate::yday(SCUPvaFH_seas2_cl) ~ as.numeric(SCUPvaFH_2_len) * 2.54, TRUE ~ scup_min_y2),
    scup_min_y2=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(SCUPvaPR_seas2_op) & day_i <= lubridate::yday(SCUPvaPR_seas2_cl) ~ as.numeric(SCUPvaPR_2_len) * 2.54, TRUE ~ scup_min_y2),
    scup_min_y2=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(SCUPvaSH_seas2_op) & day_i <= lubridate::yday(SCUPvaSH_seas2_cl) ~ as.numeric(SCUPvaSH_2_len) * 2.54, TRUE ~ scup_min_y2))

predictions_out10 <- data.frame()
#future::plan(future::multisession, workers = 36)
#future::plan(future::multisession, workers = 3)
#get_predictions_out<- function(x){
for(x in 1:1){
  
  print(x)
  
  directed_trips <- directed_trips %>% 
    dplyr::filter(draw == x) # %>%
  # dplyr::mutate(day = stringr::str_extract(day, "^\\d{2}"), 
  #               period2 = paste0(month24, "-", day, "-", mode))
  
  catch_data <- feather::read_feather(file.path(data_path, paste0("projected_catch_draws_VA", "_", x,".feather"))) %>% 
    dplyr::left_join(directed_trips, by=c("mode", "date", "draw")) 
  
  catch_data<-catch_data %>% 
    dplyr::select(-cost, -total_trips_12, -age, -bsb_keep_sim, -bsb_rel_sim, -day_i, -my_dom_id_string, 
                  -scup_keep_sim, -scup_rel_sim, -sf_keep_sim, -sf_rel_sim, -wave)
  
  
  base_outcomes0 <- list()
  n_choice_occasions0 <- list()
  
  mode_draw <- c("sh", "pr", "fh")
  for (md in mode_draw) {
    
    # pull trip outcomes from the calibration year
    base_outcomes0[[md]]<-feather::read_feather(file.path(data_path, paste0("base_outcomes_VA_", md, "_", x, ".feather"))) %>% 
      data.table::as.data.table()
    
    base_outcomes0[[md]]<-base_outcomes0[[md]] %>% 
      dplyr::select(-domain2) %>% 
      dplyr::mutate(date_parsed=lubridate::dmy(date)) %>% 
      dplyr::select(-date)
    
    # pull in data on the number of choice occasions per mode-day
    n_choice_occasions0[[md]]<-feather::read_feather(file.path(data_path, paste0("n_choice_occasions_VA_", md, "_", x, ".feather")))  
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
    dplyr::filter(state=="VA" & draw==x )   
  
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
  
  test<- predict_rec_catch(st = "VA", dr = x,
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
readr::write_csv(predictions_out10, file = here::here(paste0("output/output_VA_Alt_", format(Sys.time(), "%Y%m%d_%H%M%S"),  ".csv")))


end_time <- Sys.time()

print(end_time - start_time)



