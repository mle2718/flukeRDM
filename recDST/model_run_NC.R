##############################
### NC Rec model run  ########
##############################

print("start model_NC")
state1 = "NC"
predictions_all = list()

data_path <- here::here("Data/")


#### Read in size data ####
sf_size_data <- readr::read_csv(file.path(data_path, "fluke_projected_catch_at_lengths.csv"), show_col_types = FALSE)  %>% 
  dplyr::filter(state == "NC") %>% 
  dplyr::filter(!is.na(fitted_prob)) %>% 
  dplyr::select(state, fitted_prob, length, draw)

bsb_size_data <- readr::read_csv(file.path(data_path, "bsb_projected_catch_at_lengths.csv"), show_col_types = FALSE)  %>% 
  dplyr::filter(state == "NC") %>% 
  dplyr::filter(!is.na(fitted_prob)) %>% 
  dplyr::select(state, fitted_prob, length, draw)

scup_size_data <- readr::read_csv(file.path(data_path, "scup_projected_catch_at_lengths.csv"), show_col_types = FALSE)  %>% 
  dplyr::filter(state == "NC") %>% 
  dplyr::filter(!is.na(fitted_prob)) %>% 
  dplyr::select(state,  fitted_prob, length, draw)


l_w_conversion <- readr::read_csv(file.path(data_path, "L_W_Conversion.csv"), show_col_types = FALSE)  %>% 
  dplyr::filter(state=="NC")

#### directed trips ####
directed_trips<-feather::read_feather(file.path(data_path, paste0("directed_trips_calibration_NC.feather"))) %>% 
  tibble::tibble() %>%
  dplyr::select(mode, date, draw, bsb_bag_y2, bsb_min_y2, fluke_bag,fluke_min, scup_bag_y2, scup_min_y2,
                bsb_bag_y2_y2, bsb_min_y2_y2, fluke_bag_y2,fluke_min_y2, scup_bag_y2_y2, scup_min_y2_y2) %>% 
  dplyr::mutate(date_adj = lubridate::dmy(date), 
                date_adj = lubridate::yday(date_adj))

if(input$SF_NC_input_type == "All Modes Combined"){
  directed_trips<- directed_trips %>%
    dplyr::mutate(#Summer Flounder
      fluke_bag_y2=dplyr::case_when(day_i >= lubridate::yday(SFnc_seas1_op) & day_i <= lubridate::yday(SFnc_seas1_cl) ~ as.numeric(SFnc_1_bag), TRUE ~ 0), 
      fluke_min_y2=dplyr::case_when(day_i >= lubridate::yday(SFnc_seas1_op) & day_i <= lubridate::yday(SFnc_seas1_cl) ~ as.numeric(SFnc_1_len), TRUE ~ 100), 
      
      fluke_bag_y2=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(SFncFH_seas2_op) & day_i <= lubridate::yday(SFncFH_seas2_cl) ~ as.numeric(SFncFH_2_bag), TRUE ~ fluke_bag_y2),
      fluke_bag_y2=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(SFncPR_seas2_op) & day_i <= lubridate::yday(SFncPR_seas2_cl) ~ as.numeric(SFncPR_2_bag), TRUE ~ fluke_bag_y2),
      fluke_bag_y2=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(SFncSH_seas2_op) & day_i <= lubridate::yday(SFncSH_seas2_cl) ~ as.numeric(SFncSH_2_bag), TRUE ~ fluke_bag_y2), 
      
      fluke_min_y2=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(SFncFH_seas2_op) & day_i <= lubridate::yday(SFncFH_seas2_cl) ~ as.numeric(SFncFH_2_len), TRUE ~ fluke_min_y2),
      fluke_min_y2=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(SFncPR_seas2_op) & day_i <= lubridate::yday(SFncPR_seas2_cl) ~ as.numeric(SFncPR_2_len), TRUE ~ fluke_min_y2),
      fluke_min_y2=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(SFncSH_seas2_op) & day_i <= lubridate::yday(SFncSH_seas2_cl) ~ as.numeric(SFncSH_2_len), TRUE ~ fluke_min_y2))
} else {
  directed_trips<- directed_trips %>%
    dplyr::mutate(
      fluke_bag_y2=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(SFncFH_seas1_op) & day_i <= lubridate::yday(SFncFH_seas1_cl) ~ as.numeric(SFncFH_1_bag), TRUE ~ 0), 
      fluke_bag_y2=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(SFncPR_seas1_op) & day_i <= lubridate::yday(SFncPR_seas1_cl) ~ as.numeric(SFncPR_1_bag), TRUE ~ fluke_bag_y2), 
      fluke_bag_y2=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(SFncSH_seas1_op) & day_i <= lubridate::yday(SFncSH_seas1_cl) ~ as.numeric(SFncSH_1_bag), TRUE ~ fluke_bag_y2),
      
      fluke_bag_y2=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(SFncFH_seas2_op) & day_i <= lubridate::yday(SFncFH_seas2_cl) ~ as.numeric(SFncFH_2_bag), TRUE ~ fluke_bag_y2),
      fluke_bag_y2=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(SFncPR_seas2_op) & day_i <= lubridate::yday(SFncPR_seas2_cl) ~ as.numeric(SFncPR_2_bag), TRUE ~ fluke_bag_y2),
      fluke_bag_y2=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(SFncSH_seas2_op) & day_i <= lubridate::yday(SFncSH_seas2_cl) ~ as.numeric(SFncSH_2_bag), TRUE ~ fluke_bag_y2), 
      
      fluke_min_y2=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(SFncFH_seas1_op) & day_i <= lubridate::yday(SFncFH_seas1_cl) ~ as.numeric(SFncFH_1_len), TRUE ~ 100), 
      fluke_min_y2=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(SFncPR_seas1_op) & day_i <= lubridate::yday(SFncPR_seas1_cl) ~ as.numeric(SFncPR_1_len), TRUE ~ fluke_min_y2), 
      fluke_min_y2=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(SFncSH_seas1_op) & day_i <= lubridate::yday(SFncSH_seas1_cl) ~ as.numeric(SFncSH_1_len), TRUE ~ fluke_min_y2),
      
      fluke_min_y2=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(SFncFH_seas2_op) & day_i <= lubridate::yday(SFncFH_seas2_cl) ~ as.numeric(SFncFH_2_len), TRUE ~ fluke_min_y2),
      fluke_min_y2=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(SFncPR_seas2_op) & day_i <= lubridate::yday(SFncPR_seas2_cl) ~ as.numeric(SFncPR_2_len), TRUE ~ fluke_min_y2),
      fluke_min_y2=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(SFncSH_seas2_op) & day_i <= lubridate::yday(SFncSH_seas2_cl) ~ as.numeric(SFncSH_2_len), TRUE ~ fluke_min_y2))
  
}



if(BSB_NC_input_type == "All Modes Combined"){
  directed_trips<- directed_trips %>%
    dplyr::mutate(#black sea bass
      bsb_bag_y2=dplyr::case_when(day_i >= lubridate::yday(BSBnc_seas1_op) & day_i <= lubridate::yday(BSBnc_seas1_cl) ~ as.numeric(BSBnc_1_bag), TRUE ~ 0), 
      bsb_min_y2=dplyr::case_when(day_i >= lubridate::yday(BSBnc_seas1_op) & day_i <= lubridate::yday(BSBnc_seas1_cl) ~ as.numeric(BSBnc_1_len), TRUE ~ 100), 
      bsb_bag_y2=dplyr::case_when(day_i >= lubridate::yday(BSBnc_seas2_op) & day_i <= lubridate::yday(BSBnc_seas2_cl) ~ as.numeric(BSBnc_2_bag), TRUE ~ bsb_bag_y2), 
      bsb_min_y2=dplyr::case_when(day_i >= lubridate::yday(BSBnc_seas2_op) & day_i <= lubridate::yday(BSBnc_seas2_cl) ~ as.numeric(BSBnc_2_len), TRUE ~ bsb_min_y2),
      
      bsb_bag_y2=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(BSBncFH_seas3_op) & day_i <= lubridate::yday(BSBncFH_seas3_cl) ~ as.numeric(BSBncFH_3_bag), TRUE ~ bsb_bag_y2),
      bsb_bag_y2=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(BSBncPR_seas3_op) & day_i <= lubridate::yday(BSBncPR_seas3_cl) ~ as.numeric(BSBncPR_3_bag), TRUE ~ bsb_bag_y2),
      bsb_bag_y2=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(BSBncSH_seas3_op) & day_i <= lubridate::yday(BSBncSH_seas3_cl) ~ as.numeric(BSBncSH_3_bag), TRUE ~ bsb_bag_y2), 
      
      bsb_min_y2=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(BSBncFH_seas3_op) & day_i <= lubridate::yday(BSBncFH_seas3_cl) ~ as.numeric(BSBncFH_3_len), TRUE ~ bsb_min_y2),
      bsb_min_y2=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(BSBncPR_seas3_op) & day_i <= lubridate::yday(BSBncPR_seas3_cl) ~ as.numeric(BSBncPR_3_len), TRUE ~ bsb_min_y2),
      bsb_min_y2=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(BSBncSH_seas3_op) & day_i <= lubridate::yday(BSBncSH_seas3_cl) ~ as.numeric(BSBncSH_3_len), TRUE ~ bsb_min_y2))
} else {
  directed_trips<- directed_trips %>%
    dplyr::mutate(
      bsb_bag_y2=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(BSBncFH_seas1_op) & day_i <= lubridate::yday(BSBncFH_seas1_cl) ~ as.numeric(BSBncFH_1_bag), TRUE ~ 0), 
      bsb_bag_y2=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(BSBncPR_seas1_op) & day_i <= lubridate::yday(BSBncPR_seas1_cl) ~ as.numeric(BSBncPR_1_bag), TRUE ~ bsb_bag_y2), 
      bsb_bag_y2=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(BSBncSH_seas1_op) & day_i <= lubridate::yday(BSBncSH_seas1_cl) ~ as.numeric(BSBncSH_1_bag), TRUE ~ bsb_bag_y2),
      
      bsb_bag_y2=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(BSBncFH_seas2_op) & day_i <= lubridate::yday(BSBncFH_seas2_cl) ~ as.numeric(BSBncFH_2_bag), TRUE ~ bsb_bag_y2),
      bsb_bag_y2=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(BSBncPR_seas2_op) & day_i <= lubridate::yday(BSBncPR_seas2_cl) ~ as.numeric(BSBncPR_2_bag), TRUE ~ bsb_bag_y2),
      bsb_bag_y2=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(BSBncSH_seas2_op) & day_i <= lubridate::yday(BSBncSH_seas2_cl) ~ as.numeric(BSBncSH_2_bag), TRUE ~ bsb_bag_y2), 
      
      bsb_bag_y2=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(BSBncFH_seas3_op) & day_i <= lubridate::yday(BSBncFH_seas3_cl) ~ as.numeric(BSBncFH_3_bag), TRUE ~ bsb_bag_y2),
      bsb_bag_y2=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(BSBncPR_seas3_op) & day_i <= lubridate::yday(BSBncPR_seas3_cl) ~ as.numeric(BSBncPR_3_bag), TRUE ~ bsb_bag_y2),
      bsb_bag_y2=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(BSBncSH_seas3_op) & day_i <= lubridate::yday(BSBncSH_seas3_cl) ~ as.numeric(BSBncSH_3_bag), TRUE ~ bsb_bag_y2),
      
      bsb_min_y2=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(BSBncFH_seas1_op) & day_i <= lubridate::yday(BSBncFH_seas1_cl) ~ as.numeric(BSBncFH_1_len), TRUE ~ 100), 
      bsb_min_y2=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(BSBncPR_seas1_op) & day_i <= lubridate::yday(BSBncPR_seas1_cl) ~ as.numeric(BSBncPR_1_len), TRUE ~ bsb_min_y2), 
      bsb_min_y2=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(BSBncSH_seas1_op) & day_i <= lubridate::yday(BSBncSH_seas1_cl) ~ as.numeric(BSBncSH_1_len), TRUE ~ bsb_min_y2),
      
      bsb_min_y2=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(BSBncFH_seas2_op) & day_i <= lubridate::yday(BSBncFH_seas2_cl) ~ as.numeric(BSBncFH_2_len), TRUE ~ bsb_min_y2),
      bsb_min_y2=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(BSBncPR_seas2_op) & day_i <= lubridate::yday(BSBncPR_seas2_cl) ~ as.numeric(BSBncPR_2_len), TRUE ~ bsb_min_y2),
      bsb_min_y2=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(BSBncSH_seas2_op) & day_i <= lubridate::yday(BSBncSH_seas2_cl) ~ as.numeric(BSBncSH_2_len), TRUE ~ bsb_min_y2), 
      
      bsb_min_y2=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(BSBncFH_seas3_op) & day_i <= lubridate::yday(BSBncFH_seas3_cl) ~ as.numeric(BSBncFH_3_len), TRUE ~ bsb_min_y2),
      bsb_min_y2=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(BSBncPR_seas3_op) & day_i <= lubridate::yday(BSBncPR_seas3_cl) ~ as.numeric(BSBncPR_3_len), TRUE ~ bsb_min_y2),
      bsb_min_y2=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(BSBncSH_seas3_op) & day_i <= lubridate::yday(BSBncSH_seas3_cl) ~ as.numeric(BSBncSH_3_len), TRUE ~ bsb_min_y2))
  
}




if(SCUP_NC_input_type == "All Modes Combined"){
  directed_trips<- directed_trips %>%
    dplyr::mutate(#Scup
      scup_bag_y2=dplyr::case_when(day_i >= lubridate::yday(SCUPnc_seas1_op) & day_i <= lubridate::yday(SCUPnc_seas1_cl) ~ as.numeric(SCUPnc_1_bag), TRUE ~ 0), 
      scup_min_y2=dplyr::case_when(day_i >= lubridate::yday(SCUPnc_seas1_op) & day_i <= lubridate::yday(SCUPnc_seas1_cl) ~ as.numeric(SCUPnc_1_len), TRUE ~ 100), 
      
      scup_bag_y2=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(SCUPncFH_seas2_op) & day_i <= lubridate::yday(SCUPncFH_seas2_cl) ~ as.numeric(SCUPncFH_2_bag), TRUE ~ scup_bag_y2),
      scup_bag_y2=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(SCUPncPR_seas2_op) & day_i <= lubridate::yday(SCUPncPR_seas2_cl) ~ as.numeric(SCUPncPR_2_bag), TRUE ~ scup_bag_y2),
      scup_bag_y2=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(SCUPncSH_seas2_op) & day_i <= lubridate::yday(SCUPncSH_seas2_cl) ~ as.numeric(SCUPncSH_2_bag), TRUE ~ scup_bag_y2), 
      
      scup_min_y2=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(SCUPncFH_seas2_op) & day_i <= lubridate::yday(SCUPncFH_seas2_cl) ~ as.numeric(SCUPncFH_2_len), TRUE ~ scup_min_y2),
      scup_min_y2=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(SCUPncPR_seas2_op) & day_i <= lubridate::yday(SCUPncPR_seas2_cl) ~ as.numeric(SCUPncPR_2_len), TRUE ~ scup_min_y2),
      scup_min_y2=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(SCUPncSH_seas2_op) & day_i <= lubridate::yday(SCUPncSH_seas2_cl) ~ as.numeric(SCUPncSH_2_len), TRUE ~ scup_min_y2))
} else {
  directed_trips<- directed_trips %>%
    dplyr::mutate(
      scup_bag_y2=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(SCUPncFH_seas1_op) & day_i <= lubridate::yday(SCUPncFH_seas1_cl) ~ as.numeric(SCUPncFH_1_bag), TRUE ~ 0), 
      scup_bag_y2=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(SCUPncPR_seas1_op) & day_i <= lubridate::yday(SCUPncPR_seas1_cl) ~ as.numeric(SCUPncPR_1_bag), TRUE ~ scup_bag_y2), 
      scup_bag_y2=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(SCUPncSH_seas1_op) & day_i <= lubridate::yday(SCUPncSH_seas1_cl) ~ as.numeric(SCUPncSH_1_bag), TRUE ~ scup_bag_y2),
      
      scup_bag_y2=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(SCUPncFH_seas2_op) & day_i <= lubridate::yday(SCUPncFH_seas2_cl) ~ as.numeric(SCUPncFH_2_bag), TRUE ~ scup_bag_y2),
      scup_bag_y2=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(SCUPncPR_seas2_op) & day_i <= lubridate::yday(SCUPncPR_seas2_cl) ~ as.numeric(SCUPncPR_2_bag), TRUE ~ scup_bag_y2),
      scup_bag_y2=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(SCUPncSH_seas2_op) & day_i <= lubridate::yday(SCUPncSH_seas2_cl) ~ as.numeric(SCUPncSH_2_bag), TRUE ~ scup_bag_y2), 
      
      scup_min_y2=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(SCUPncFH_seas1_op) & day_i <= lubridate::yday(SCUPncFH_seas1_cl) ~ as.numeric(SCUPncFH_1_len), TRUE ~ 100), 
      scup_min_y2=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(SCUPncPR_seas1_op) & day_i <= lubridate::yday(SCUPncPR_seas1_cl) ~ as.numeric(SCUPncPR_1_len), TRUE ~ scup_min_y2), 
      scup_min_y2=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(SCUPncSH_seas1_op) & day_i <= lubridate::yday(SCUPncSH_seas1_cl) ~ as.numeric(SCUPncSH_1_len), TRUE ~ scup_min_y2),
      
      scup_min_y2=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(SCUPncFH_seas2_op) & day_i <= lubridate::yday(SCUPncFH_seas2_cl) ~ as.numeric(SCUPncFH_2_len), TRUE ~ scup_min_y2),
      scup_min_y2=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(SCUPncPR_seas2_op) & day_i <= lubridate::yday(SCUPncPR_seas2_cl) ~ as.numeric(SCUPncPR_2_len), TRUE ~ scup_min_y2),
      scup_min_y2=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(SCUPncSH_seas2_op) & day_i <= lubridate::yday(SCUPncSH_seas2_cl) ~ as.numeric(SCUPncSH_2_len), TRUE ~ scup_min_y2))
}



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
  
  catch_data <- feather::read_feather(file.path(data_path, paste0("projected_catch_draws_NC", "_", x,".feather"))) %>% 
    dplyr::left_join(directed_trips, by=c("mode", "date", "draw")) 
  
  catch_data<-catch_data %>% 
    dplyr::select(-cost, -total_trips_12, -age, -bsb_keep_sim, -bsb_rel_sim, -day_i, -my_dom_id_string, 
                  -scup_keep_sim, -scup_rel_sim, -sf_keep_sim, -sf_rel_sim, -wave)
  
  
  base_outcomes0 <- list()
  n_choice_occasions0 <- list()
  
  mode_draw <- c("sh", "pr", "fh")
  for (md in mode_draw) {
    
    # pull trip outcomes from the calibration year
    base_outcomes0[[md]]<-feather::read_feather(file.path(data_path, paste0("base_outcomes_NC_", md, "_", x, ".feather"))) %>% 
      data.table::as.data.table()
    
    base_outcomes0[[md]]<-base_outcomes0[[md]] %>% 
      dplyr::select(-domain2) %>% 
      dplyr::mutate(date_parsed=lubridate::dmy(date)) %>% 
      dplyr::select(-date)
    
    # pull in data on the number of choice occasions per mode-day
    n_choice_occasions0[[md]]<-feather::read_feather(file.path(data_path, paste0("n_choice_occasions_NC_", md, "_", x, ".feather")))  
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
    dplyr::filter(state=="NC" & draw==x )   
  
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
  
  test<- predict_rec_catch(st = "NC", dr = x,
                           directed_trips, catch_data, 
                           sf_size_data, bsb_size_data, scup_size_data, 
                           l_w_conversion, calib_comparison, n_choice_occasions, 
                           base_outcomes)
  
  test <- test %>% 
    dplyr::mutate(draw = c(x),
                  #model = c("Alt"))
                  model = c(run_name))
  
  #regs <- # Input table will be used to fill out regs in DT
  
  predictions_out10<- predictions_out10 %>% rbind(test) 
}


print("out of loop")



# use furrr package to parallelize the get_predictions_out function 100 times
# This will spit out a dataframe with 100 predictions 
#predictions_out10<- furrr::future_map_dfr(1:100, ~get_predictions_out(.), .id = "draw")
#predictions_out10<- furrr::future_map_dfr(1:3, ~get_predictions_out(.), .id = "draw")

#readr::write_csv(predictions_out10, file = here::here(paste0("output/output_MA_", Run_Name, "_", format(Sys.time(), "%Y%m%d_%H%M%S"),  ".csv")))
readr::write_csv(predictions_out10, file = here::here(paste0("output/output_NC_Alt_", format(Sys.time(), "%Y%m%d_%H%M%S"),  ".csv")))


end_time <- Sys.time()

print(end_time - start_time)



