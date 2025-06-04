##############################
#### MA Rec model run  ########
##############################


# directed_trips<- directed_trips %>%  
print("start model_MA")
state1 = "MA"
predictions_all = list()

data_path <- here::here("Data/")

#### Read in size data ####
sf_size_data <- readr::read_csv(file.path(data_path, "fluke_projected_catch_at_lengths.csv"), show_col_types = FALSE)  %>% 
  dplyr::filter(state == "MA") %>% 
  dplyr::filter(!is.na(fitted_prob)) %>% 
  dplyr::select(state, fitted_prob, length, draw)

bsb_size_data <- readr::read_csv(file.path(data_path, "bsb_projected_catch_at_lengths.csv"), show_col_types = FALSE)  %>% 
  dplyr::filter(state == "MA") %>% 
  dplyr::filter(!is.na(fitted_prob)) %>% 
  dplyr::select(state, fitted_prob, length, draw)

scup_size_data <- readr::read_csv(file.path(data_path, "scup_projected_catch_at_lengths.csv"), show_col_types = FALSE)  %>% 
  dplyr::filter(state == "MA") %>% 
  dplyr::filter(!is.na(fitted_prob)) %>% 
  dplyr::select(state,  fitted_prob, length, draw)


l_w_conversion <- readr::read_csv(file.path(data_path, "L_W_Conversion.csv"), show_col_types = FALSE)  %>% 
  dplyr::filter(state=="MA")

#### directed trips ####
start_time <- Sys.time()

directed_trips<-feather::read_feather(file.path(data_path, paste0("directed_trips_calibration_MA.feather"))) %>% 
  tibble::tibble() %>%
  dplyr::select(mode, date, draw, bsb_bag, bsb_min, fluke_bag,fluke_min, scup_bag, scup_min,
                bsb_bag_y2, bsb_min_y2, fluke_bag_y2,fluke_min_y2, scup_bag_y2, scup_min_y2) %>% 
  dplyr::mutate(date_adj = lubridate::dmy(date), 
                date_adj = format(date_adj, "%m-%d")) %>% 
  dplyr::mutate(
    fluke_bag1_y2=dplyr::case_when(mode == "fh" & date_adj >= lubridate::yday(input$SFmaFH_seas1[1]) & date_adj <= lubridate::yday(input$SFmaFH_seas1[2]) ~ as.numeric(input$SFmaFH_1_bag), TRUE ~ 0),
    fluke_min1_y2=dplyr::case_when(mode == "fh" & date_adj >= lubridate::yday(input$SFmaFH_seas1[1]) & date_adj <= lubridate::yday(input$SFmaFH_seas1[2]) ~ as.numeric(input$SFmaFH_1_len), TRUE ~ 100),
    fluke_bag1_y2=dplyr::case_when(mode == "fh" & date_adj >= lubridate::yday(input$SFmaFH_seas2[1]) & date_adj <= lubridate::yday(input$SFmaFH_seas2[2]) ~ as.numeric(input$SFmaFH_2_bag), TRUE ~ fluke_bag1_y2),
    fluke_min1_y2=dplyr::case_when(mode == "fh" & date_adj >= lubridate::yday(input$SFmaFH_seas2[1]) & date_adj <= lubridate::yday(input$SFmaFH_seas2[2]) ~ as.numeric(input$SFmaFH_2_len), TRUE ~ fluke_min1_y2),

    fluke_bag1_y2=dplyr::case_when(mode == "pr" & date_adj >= lubridate::yday(input$SFmaPR_seas1[1]) & date_adj <= lubridate::yday(input$SFmaPR_seas1[2]) ~ as.numeric(input$SFmaPR_1_bag), TRUE ~ fluke_bag1_y2),
    fluke_min1_y2=dplyr::case_when(mode == "pr" & date_adj >= lubridate::yday(input$SFmaPR_seas1[1]) & date_adj <= lubridate::yday(input$SFmaPR_seas1[2]) ~ as.numeric(input$SFmaPR_1_len), TRUE ~ fluke_min1_y2),
    fluke_bag1_y2=dplyr::case_when(mode == "pr" & date_adj >= lubridate::yday(input$SFmaPR_seas2[1]) & date_adj <= lubridate::yday(input$SFmaPR_seas2[2]) ~ as.numeric(input$SFmaPR_2_bag), TRUE ~ fluke_bag1_y2),
    fluke_min1_y2=dplyr::case_when(mode == "pr" & date_adj >= lubridate::yday(input$SFmaPR_seas2[1]) & date_adj <= lubridate::yday(input$SFmaPR_seas2[2]) ~ as.numeric(input$SFmaPR_2_len), TRUE ~ fluke_min1_y2),

    fluke_bag1_y2=dplyr::case_when(mode == "sh" & date_adj >= lubridate::yday(input$SFmaSH_seas1[1]) & date_adj <= lubridate::yday(input$SFmaSH_seas1[2]) ~ as.numeric(input$SFmaSH_1_bag), TRUE ~ fluke_bag1_y2),
    fluke_min1_y2=dplyr::case_when(mode == "sh" & date_adj >= lubridate::yday(input$SFmaSH_seas1[1]) & date_adj <= lubridate::yday(input$SFmaSH_seas1[2]) ~ as.numeric(input$SFmaSH_1_len), TRUE ~ fluke_min1_y2),
    fluke_bag1_y2=dplyr::case_when(mode == "sh" & date_adj >= lubridate::yday(input$SFmaSH_seas2[1]) & date_adj <= lubridate::yday(input$SFmaSH_seas2[2]) ~ as.numeric(input$SFmaSH_2_bag), TRUE ~ fluke_bag1_y2),
    fluke_min1_y2=dplyr::case_when(mode == "sh" & date_adj >= lubridate::yday(input$SFmaSH_seas2[1]) & date_adj <= lubridate::yday(input$SFmaSH_seas2[2]) ~ as.numeric(input$SFmaSH_2_len), TRUE ~ fluke_min1_y2))



if(input$BSB_MA_input_type == "All Modes Combined"){
  directed_trips<- directed_trips %>%
    dplyr::mutate(# Black Sea Bass Bag Limit
      bsb_bag_y2=dplyr::case_when(date_adj >= lubridate::yday(input$BSBma_seas1[1]) & date_adj <= lubridate::yday(input$BSBma_seas1[2]) ~ as.numeric(input$BSBma_1_bag), TRUE ~ 0),
      bsb_bag_y2=dplyr::case_when(mode == "fh" & date_adj >= lubridate::yday(input$BSBmaFH_seas2[1]) & date_adj <= lubridate::yday(input$BSBmaFH_seas2[2]) ~ as.numeric(input$BSBmaFH_2_bag), TRUE ~ bsb_bag_y2),
      bsb_bag_y2=dplyr::case_when(mode == "pr" & date_adj >= lubridate::yday(input$BSBmaPR_seas2[1]) & date_adj <= lubridate::yday(input$BSBmaPR_seas2[2]) ~ as.numeric(input$BSBmaPR_2_bag), TRUE ~ bsb_bag_y2),
      bsb_bag_y2=dplyr::case_when(mode == "sh" & date_adj >= lubridate::yday(input$BSBmaSH_seas2[1]) & date_adj <= lubridate::yday(input$BSBmaSH_seas2[2]) ~ as.numeric(input$BSBmaSH_2_bag), TRUE ~ bsb_bag_y2),
      # Black Sea Bass Minimun Size
      bsb_min_y2=dplyr::case_when(date_adj >= lubridate::yday(input$BSBma_seas1[1]) & date_adj <= lubridate::yday(input$BSBma_seas1[2]) ~ as.numeric(input$BSBma_1_len), TRUE ~ 100),
      bsb_min_y2=dplyr::case_when(mode == "fh" & date_adj >= lubridate::yday(input$BSBmaFH_seas2[1]) & date_adj <= lubridate::yday(input$BSBmaFH_seas2[2]) ~ as.numeric(input$BSBmaFH_2_len), TRUE ~ bsb_min_y2),
      bsb_min_y2=dplyr::case_when(mode == "pr" & date_adj >= lubridate::yday(input$BSBmaPR_seas2[1]) & date_adj <= lubridate::yday(input$BSBmaPR_seas2[2]) ~ as.numeric(input$BSBmaPR_2_len), TRUE ~ bsb_min_y2),
      bsb_min_y2=dplyr::case_when(mode == "sh" & date_adj >= lubridate::yday(input$BSBmaSH_seas2[1]) & date_adj <= lubridate::yday(input$BSBmaSH_seas2[2]) ~ as.numeric(input$BSBmaSH_2_len), TRUE ~ bsb_min_y2))
} else {
  directed_trips<- directed_trips %>%
    dplyr::mutate(# Black Sea Bass Bag Limit by Mode
      bsb_bag_y2=dplyr::case_when(mode == "fh" & date_adj >= lubridate::yday(input$BSBmaFH_seas1[1]) & date_adj <= lubridate::yday(input$BSBmaFH_seas1[2]) ~ as.numeric(input$BSBmaFH_1_bag), TRUE ~ 0),
      bsb_bag_y2=dplyr::case_when(mode == "pr" & date_adj >= lubridate::yday(input$BSBmaPR_seas1[1]) & date_adj <= lubridate::yday(input$BSBmaPR_seas1[2]) ~ as.numeric(input$BSBmaPR_1_bag), TRUE ~ bsb_bag_y2),
      bsb_bag_y2=dplyr::case_when(mode == "sh" & date_adj >= lubridate::yday(input$BSBmaSH_seas1[1]) & date_adj <= lubridate::yday(input$BSBmaSH_seas1[2]) ~ as.numeric(input$BSBmaSH_1_bag), TRUE ~ bsb_bag_y2),
      bsb_bag_y2=dplyr::case_when(mode == "fh" & date_adj >= lubridate::yday(input$BSBmaFH_seas2[1]) & date_adj <= lubridate::yday(input$BSBmaFH_seas2[2]) ~ as.numeric(input$BSBmaFH_2_bag), TRUE ~ bsb_bag_y2),
      bsb_bag_y2=dplyr::case_when(mode == "pr" & date_adj >= lubridate::yday(input$BSBmaPR_seas2[1]) & date_adj <= lubridate::yday(input$BSBmaPR_seas2[2]) ~ as.numeric(input$BSBmaPR_2_bag), TRUE ~ bsb_bag_y2),
      bsb_bag_y2=dplyr::case_when(mode == "sh" & date_adj >= lubridate::yday(input$BSBmaSH_seas2[1]) & date_adj <= lubridate::yday(input$BSBmaSH_seas2[2]) ~ as.numeric(input$BSBmaSH_2_bag), TRUE ~ bsb_bag_y2),
      # Black Sea Bass Minimum Length by Mode
      bsb_min_y2=dplyr::case_when(mode == "fh" & date_adj >= lubridate::yday(input$BSBmaFH_seas1[1]) & date_adj <= lubridate::yday(input$BSBmaFH_seas1[2]) ~ as.numeric(input$BSBmaFH_1_len), TRUE ~ 100),
      bsb_min_y2=dplyr::case_when(mode == "pr" & date_adj >= lubridate::yday(input$BSBmaPR_seas1[1]) & date_adj <= lubridate::yday(input$BSBmaPR_seas1[2]) ~ as.numeric(input$BSBmaPR_1_len), TRUE ~ bsb_min_y2),
      bsb_min_y2=dplyr::case_when(mode == "sh" & date_adj >= lubridate::yday(input$BSBmaSH_seas1[1]) & date_adj <= lubridate::yday(input$BSBmaSH_seas1[2]) ~ as.numeric(input$BSBmaSH_1_len), TRUE ~ bsb_min_y2),
      bsb_min_y2=dplyr::case_when(mode == "fh" & date_adj >= lubridate::yday(input$BSBmaFH_seas2[1]) & date_adj <= lubridate::yday(input$BSBmaFH_seas2[2]) ~ as.numeric(input$BSBmaFH_2_len), TRUE ~ bsb_min_y2),
      bsb_min_y2=dplyr::case_when(mode == "pr" & date_adj >= lubridate::yday(input$BSBmaPR_seas2[1]) & date_adj <= lubridate::yday(input$BSBmaPR_seas2[2]) ~ as.numeric(input$BSBmaPR_2_len), TRUE ~ bsb_min_y2),
      bsb_min_y2=dplyr::case_when(mode == "sh" & date_adj >= lubridate::yday(input$BSBmaSH_seas2[1]) & date_adj <= lubridate::yday(input$BSBmaSH_seas2[2]) ~ as.numeric(input$BSBmaSH_2_len), TRUE ~ bsb_min_y2))
}



  directed_trips<- directed_trips %>%
    dplyr::mutate(
      scup_bag_y2=dplyr::case_when(mode == "fh" & date_adj >= lubridate::yday(input$SCUPmaFH_seas1[1]) & date_adj <= lubridate::yday(input$SCUPmaFH_seas1[2]) ~ as.numeric(input$SCUPmaFH_1_bag), TRUE ~ 0),
      scup_min_y2=dplyr::case_when(mode == "fh" & date_adj >= lubridate::yday(input$SCUPmaFH_seas1[1]) & date_adj <= lubridate::yday(input$SCUPmaFH_seas1[2]) ~ as.numeric(input$SCUPmaFH_1_len), TRUE ~ 100),
      scup_bag_y2=dplyr::case_when(mode == "fh" & date_adj >= lubridate::yday(input$SCUPmaFH_seas2[1]) & date_adj <= lubridate::yday(input$SCUPmaFH_seas2[2]) ~ as.numeric(input$SCUPmaFH_2_bag), TRUE ~ scup_bag_y2),
      scup_min_y2=dplyr::case_when(mode == "fh" & date_adj >= lubridate::yday(input$SCUPmaFH_seas2[1]) & date_adj <= lubridate::yday(input$SCUPmaFH_seas2[2]) ~ as.numeric(input$SCUPmaFH_2_len), TRUE ~ scup_min_y2),
      scup_bag_y2=dplyr::case_when(mode == "fh" & date_adj >= lubridate::yday(input$SCUPmaFH_seas3[1]) & date_adj <= lubridate::yday(input$SCUPmaFH_seas3[2]) ~ as.numeric(input$SCUPmaFH_3_bag), TRUE ~ scup_bag_y2),
      scup_min_y2=dplyr::case_when(mode == "fh" & date_adj >= lubridate::yday(input$SCUPmaFH_seas3[1]) & date_adj <= lubridate::yday(input$SCUPmaFH_seas3[2]) ~ as.numeric(input$SCUPmaFH_3_len), TRUE ~ scup_min_y2),

      scup_bag_y2=dplyr::case_when(mode == "pr" & date_adj >= lubridate::yday(input$SCUPmaPR_seas1[1]) & date_adj <= lubridate::yday(input$SCUPmaPR_seas1[2]) ~ as.numeric(input$SCUPmaPR_1_bag), TRUE ~ scup_bag_y2),
      scup_min_y2=dplyr::case_when(mode == "pr" & date_adj >= lubridate::yday(input$SCUPmaPR_seas1[1]) & date_adj <= lubridate::yday(input$SCUPmaPR_seas1[2]) ~ as.numeric(input$SCUPmaPR_1_len), TRUE ~ scup_min_y2),
      scup_bag_y2=dplyr::case_when(mode == "pr" & date_adj >= lubridate::yday(input$SCUPmaPR_seas2[1]) & date_adj <= lubridate::yday(input$SCUPmaPR_seas2[2]) ~ as.numeric(input$SCUPmaPR_2_bag), TRUE ~ scup_bag_y2),
      scup_min_y2=dplyr::case_when(mode == "pr" & date_adj >= lubridate::yday(input$SCUPmaPR_seas2[1]) & date_adj <= lubridate::yday(input$SCUPmaPR_seas2[2]) ~ as.numeric(input$SCUPmaPR_2_len), TRUE ~ scup_min_y2),

      scup_bag_y2=dplyr::case_when(mode == "sh" & date_adj >= lubridate::yday(input$SCUPmaSH_seas1[1]) & date_adj <= lubridate::yday(input$SCUPmaSH_seas1[2]) ~ as.numeric(input$SCUPmaSH_1_bag), TRUE ~ scup_bag_y2),
      scup_min_y2=dplyr::case_when(mode == "sh" & date_adj >= lubridate::yday(input$SCUPmaSH_seas1[1]) & date_adj <= lubridate::yday(input$SCUPmaSH_seas1[2]) ~ as.numeric(input$SCUPmaSH_1_len), TRUE ~ scup_min_y2),
      scup_bag_y2=dplyr::case_when(mode == "sh" & date_adj >= lubridate::yday(input$SCUPmaSH_seas2[1]) & date_adj <= lubridate::yday(input$SCUPmaSH_seas2[2]) ~ as.numeric(input$SCUPmaSH_2_bag), TRUE ~ scup_bag_y2),
      scup_min_y2=dplyr::case_when(mode == "sh" & date_adj >= lubridate::yday(input$SCUPmaSH_seas2[1]) & date_adj <= lubridate::yday(input$SCUPmaSH_seas2[2]) ~ as.numeric(input$SCUPmaSH_2_len), TRUE ~ scup_min_y2))

  end_time <- Sys.time()
  print(end_time - start_time)
  
  readr::write_csv(directed_trips, file = here::here(paste0("output/MA_directed_trips_", input$Run_Name, ".csv")))




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
  
  catch_data <- feather::read_feather(file.path(data_path, paste0("projected_catch_draws_MA", "_", x,".feather"))) %>% 
    dplyr::left_join(directed_trips, by=c("mode", "date", "draw")) 
  
  catch_data<-catch_data %>% 
    dplyr::select(-cost, -total_trips_12, -age, -bsb_keep_sim, -bsb_rel_sim, -day_i, -my_dom_id_string, 
                  -scup_keep_sim, -scup_rel_sim, -sf_keep_sim, -sf_rel_sim, -wave)

  
  base_outcomes0 <- list()
  n_choice_occasions0 <- list()
  
  mode_draw <- c("sh", "pr", "fh")
  for (md in mode_draw) {
    
    # pull trip outcomes from the calibration year
    base_outcomes0[[md]]<-feather::read_feather(file.path(data_path, paste0("base_outcomes_MA_", md, "_", x, ".feather"))) %>% 
      data.table::as.data.table()
    
    base_outcomes0[[md]]<-base_outcomes0[[md]] %>% 
      dplyr::select(-domain2) %>% 
      dplyr::mutate(date_parsed=lubridate::dmy(date)) %>% 
      dplyr::select(-date)
    
    # pull in data on the number of choice occasions per mode-day
    n_choice_occasions0[[md]]<-feather::read_feather(file.path(data_path, paste0("n_choice_occasions_MA_", md, "_", x, ".feather")))  
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
    dplyr::filter(state=="MA" & draw==x )   

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
  
  test<- predict_rec_catch(st = "MA", dr = x,
                           directed_trips, catch_data, 
                           sf_size_data, bsb_size_data, scup_size_data, 
                           l_w_conversion, calib_comparison, n_choice_occasions, 
                           base_outcomes)
  
  test <- test %>% 
    dplyr::mutate(draw = c(x))
  
  predictions_out10<- predictions_out10 %>% rbind(test)
}


print("out of loop")


# use furrr package to parallelize the get_predictions_out function 100 times
# This will spit out a dataframe with 100 predictions 
#predictions_out10<- furrr::future_map_dfr(1:100, ~get_predictions_out(.), .id = "draw")
#predictions_out10<- furrr::future_map_dfr(1:3, ~get_predictions_out(.), .id = "draw")

readr::write_csv(predictions_out10, file = here::here(paste0("output/output_MA_", input$Run_Name, "_", format(Sys.time(), "%Y%m%d_%H%M%S"),  ".csv")))

predictions_out10<- predictions_out10 %>%
  dplyr::filter(!mode == "all")

#### read in SQ values and corrections ####
StatusQuo <- openxlsx::read.xlsx(here::here("data-raw/StatusQuo/SQ_projections_11_9_MA.xlsx"))

StatusQuo_corrections<- openxlsx::read.xlsx(here::here("data-raw/StatusQuo/All_states_SQ_corrections1.xlsx")) %>% 
  dplyr::filter(state == state1)

StatusQuo<-StatusQuo %>% 
  dplyr::left_join(StatusQuo_corrections, by=c("state", "mode", "Category", "keep_release", "number_weight")) %>% 
  dplyr::mutate(correction=dplyr::case_when(is.na(correction)~1, TRUE~correction)) %>% 
  dplyr::mutate(Value=as.numeric(Value), correction=as.numeric(correction),
                Value=Value*correction) %>% 
  dplyr::rename(value_SQ = Value)

predictions_merge <- predictions_out10 %>% #predictions_out10 %>% 
  dplyr::rename(value_alt= Value) %>% 
  dplyr::mutate(draw = as.numeric(draw)) %>% 
  dplyr::left_join(StatusQuo, by = c("Category","mode", "keep_release","param" ,"number_weight","state", "draw")) 

all_dat <- predictions_merge %>% 
  dplyr::select(!c("correction", "param")) %>%
  dplyr::mutate(Category = dplyr::recode(Category,  "bsb" = "Black Sea Bass", 
                                         "sf" = "Summer Flounder", 
                                         "scup" = "Scup"), 
                keep_release = dplyr::recode(keep_release,  "keep" = "harvest", 
                                             "Discmortality" = "dead release"), 
                number_weight = dplyr::recode(number_weight,  "Weight" = "pounds", 
                                              "Number" = "numbers"), 
                mode = dplyr::recode(mode,  "fh" = "For Hire", 
                                     "pr" = "Private", 
                                     "sh" = "Shore"), 
                stat = paste(keep_release, number_weight), 
                median_perc_diff = "NA",
                reach_target = "NA") %>% 
  dplyr::rename(region = state, 
                species = Category, 
                median_value_alt = value_alt,
                median_value_SQ = value_SQ) %>% 
  dplyr::select(!c("keep_release", "number_weight"))

predictions_merge <- predictions_merge %>% 
  #dplyr::left_join(StatusQuo, by = c("Category","mode", "keep_release","number_weight","state")) %>% 
  dplyr::filter(Category %in% c("sf", "bsb", "scup"),
                mode!="all", 
                keep_release=="keep", 
                #number_weight %in% c("Weight_avg", "Weight") ) %>% 
                number_weight %in% c("Weight", "Weight_avg") ) %>% 
  dplyr::select(-param) %>% 
  dplyr::mutate(value_SQ = as.numeric(value_SQ), 
                value_alt = as.numeric(value_alt))

predictions_weight <- predictions_merge %>%
  dplyr::filter(number_weight == "Weight") %>%
  dplyr::rename(value_alt_Weight=value_alt, value_SQ_Weight=value_SQ) %>%
  dplyr::mutate(perc_change_weight = (((value_alt_Weight-value_SQ_Weight)/value_SQ_Weight) * 100))

predictions_avg <- predictions_merge %>%
  dplyr::filter(number_weight == "Weight_avg") 

predictions_merge2<- predictions_avg %>% 
  dplyr::left_join(predictions_weight, by = c("Category","mode", "state","draw")) %>%
  dplyr::select(-keep_release.x, -keep_release.y, -number_weight.y) %>% 
  dplyr::mutate(imputed_value_alt= perc_change_weight/100,
                imputed_value_alt = value_SQ * imputed_value_alt, 
                imputed_value_alt=imputed_value_alt+value_SQ) 
#check = ((imputed_value_alt-value_SQ)/value_SQ)*100)

predictions_merge2<- predictions_merge2 %>% 
  #If imputed_value_alt is missing and value_alt_Weight is not missing, replace imputed_value_alt with value_alt_Weight
  dplyr::mutate(imputed_value_alt = dplyr::case_when(is.na(imputed_value_alt) & 
                                                       !is.na(value_alt_Weight) ~ value_alt_Weight, TRUE ~ imputed_value_alt)) %>% 
  #If imputed_value_alt is missing and value_alt_Weight IS missing, and value_SQ =0,  replace imputed_value_alt with 0
  dplyr::mutate(imputed_value_alt = dplyr::case_when(is.na(imputed_value_alt) & 
                                                       is.na(value_alt_Weight) & 
                                                       value_SQ==0 ~ 0, TRUE ~ imputed_value_alt))
state_harvest_output<- predictions_merge2 %>% 
  dplyr::group_by(draw, Category, state) %>% 
  dplyr::summarise(imputed_value_alt_sum = sum(imputed_value_alt),
                   value_SQ_sum = sum(value_SQ)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(perc_diff=((imputed_value_alt_sum-value_SQ_sum)/value_SQ_sum)*100) %>% 
  dplyr::arrange(state, Category, draw) %>% 
  dplyr::mutate(perc_diff = dplyr::case_when(value_SQ_sum==0 &
                                               imputed_value_alt_sum==0 ~ 0, TRUE ~ perc_diff))

state_harvest_output <- state_harvest_output %>% 
  dplyr::mutate(domain=paste0(state, "_", Category)) %>% 
  dplyr::group_by(domain) %>% 
  dplyr::arrange(domain,imputed_value_alt_sum) %>% 
  dplyr::mutate(n_weight = dplyr::row_number(domain)) %>% 
  dplyr::arrange(domain,perc_diff) %>% 
  dplyr::mutate(n_perc = dplyr::row_number(domain)) %>% 
  dplyr::arrange(domain,value_SQ_sum) %>% 
  dplyr::mutate(n_SQ = dplyr::row_number(domain)) %>% 
  dplyr::ungroup() 

state_harvest_output<- state_harvest_output %>% 
  dplyr::mutate(harv_target=dplyr::case_when(Category=="scup"~scup_percent_change*value_SQ_sum, TRUE~NA), 
                #harv_target=dplyr::case_when(Category=="bsb"~bsb_percent_change*value_SQ_sum, TRUE~harv_target),
                harv_target=dplyr::case_when(Category=="sf"~sf_percent_change*value_SQ_sum, TRUE~harv_target)) %>% 
  dplyr::mutate(reach_target=dplyr::case_when(imputed_value_alt_sum<=harv_target  ~1, TRUE~0))

categories_state=list()

for(d in unique(state_harvest_output$domain)){
  
  new<- state_harvest_output %>% 
    dplyr::filter(domain==d) #%>% 
  #dplyr::arrange(n_perc)
  
  lb_value_alt<- new$imputed_value_alt_sum[new$n_weight==11] 
  lb_perc_diff<- new$perc_diff[new$n_perc==11] 
  lb_value_SQ<- new$value_SQ_sum[new$n_SQ==11] 
  
  ub_value_alt<- new$imputed_value_alt_sum[new$n_weight==90] 
  ub_perc_diff<- new$perc_diff[new$n_perc==90] 
  ub_value_SQ<- new$value_SQ_sum[new$n_SQ==90] 
  
  median_value_alt<- median(new$imputed_value_alt_sum)
  median_perc_diff<- median(new$perc_diff)
  median_value_SQ<- median(new$value_SQ_sum)
  
  reach_target<- sum(new$reach_target)
  
  
  categories_state[[d]] <- as.data.frame(
    cbind(
      median_perc_diff,lb_perc_diff, ub_perc_diff,
      median_value_alt,lb_value_alt, ub_value_alt,
      median_value_SQ,lb_value_SQ, ub_value_SQ, reach_target
    ))
  
  categories_state[[d]]$domain<-d
  
  
}
state_harvest_results= rlist::list.stack(categories_state, fill=TRUE)
state_harvest_results<- state_harvest_results %>% 
  tidyr::separate(domain, into = c("region", "species"))  %>% 
  dplyr::mutate(stat="harvest pounds", 
                mode="all modes") %>% 
  dplyr::relocate(region, stat, species, mode)


###########################
#state by mode-level output
state_mode_harvest_output<- predictions_merge2 %>% 
  dplyr::group_by(draw, Category, state, mode) %>% 
  dplyr::summarise(imputed_value_alt_sum = sum(imputed_value_alt),
                   value_SQ_sum = sum(value_SQ)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(perc_diff=((imputed_value_alt_sum-value_SQ_sum)/value_SQ_sum)*100) %>% 
  dplyr::arrange(state, mode, Category, draw) %>% 
  dplyr::mutate(perc_diff = dplyr::case_when(value_SQ_sum==0 &
                                               imputed_value_alt_sum==0 ~ 0, TRUE ~ perc_diff))

state_mode_harvest_output_check<- state_mode_harvest_output %>% 
  dplyr::filter(perc_diff=="NaN")

state_mode_harvest_output <- state_mode_harvest_output %>% 
  dplyr::mutate(domain=paste0(state, "_", Category, "_", mode)) %>% 
  dplyr::group_by(domain) %>% 
  dplyr::arrange(domain,imputed_value_alt_sum) %>% 
  dplyr::mutate(n_weight = dplyr::row_number(domain)) %>% 
  dplyr::arrange(domain,perc_diff) %>% 
  dplyr::mutate(n_perc = dplyr::row_number(domain)) %>% 
  dplyr::arrange(domain,value_SQ_sum) %>% 
  dplyr::mutate(n_SQ = dplyr::row_number(domain)) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(domain,perc_diff) 

state_mode_harvest_output<- state_mode_harvest_output %>% 
  dplyr::mutate(harv_target=dplyr::case_when(Category=="scup"~scup_percent_change*value_SQ_sum, TRUE~NA), 
                #harv_target=dplyr::case_when(Category=="bsb"~bsb_percent_change*value_SQ_sum, TRUE~harv_target),
                harv_target=dplyr::case_when(Category=="sf"~sf_percent_change*value_SQ_sum, TRUE~harv_target)) %>% 
  dplyr::mutate(reach_target=dplyr::case_when(imputed_value_alt_sum<=harv_target~1, TRUE~0))

categories_state_mode=list()

for(d in unique(state_mode_harvest_output$domain)){
  
  new<- state_mode_harvest_output %>% 
    dplyr::filter(domain==d) #%>% 
  #dplyr::arrange(n_perc)
  
  lb_value_alt<- new$imputed_value_alt_sum[new$n_weight==11] 
  lb_perc_diff<- new$perc_diff[new$n_perc==11] 
  lb_value_SQ<- new$value_SQ_sum[new$n_SQ==11] 
  
  ub_value_alt<- new$imputed_value_alt_sum[new$n_weight==90] 
  ub_perc_diff<- new$perc_diff[new$n_perc==90] 
  ub_value_SQ<- new$value_SQ_sum[new$n_SQ==90] 
  
  median_value_alt<- median(new$imputed_value_alt_sum)
  median_perc_diff<- median(new$perc_diff)
  median_value_SQ<- median(new$value_SQ_sum)
  
  reach_target<- sum(new$reach_target)
  
  
  
  categories_state_mode[[d]] <- as.data.frame(
    cbind(
      median_perc_diff,lb_perc_diff, ub_perc_diff,
      median_value_alt,lb_value_alt, ub_value_alt,
      median_value_SQ,lb_value_SQ, ub_value_SQ, reach_target
    ))
  
  categories_state_mode[[d]]$domain<-d
  
  
}
state_mode_harvest_results= rlist::list.stack(categories_state_mode, fill=TRUE)
state_mode_harvest_results<- state_mode_harvest_results %>% 
  tidyr::separate(domain, into = c("region", "species", "mode"))  %>% 
  dplyr::mutate(stat="harvest pounds") 



### Release

predictions_releases_merge <- predictions_out10 %>% 
  dplyr::rename(value_alt= Value) %>% 
  dplyr::mutate(draw = as.numeric(draw)) %>% 
  dplyr::left_join(StatusQuo, by=c("Category","mode", "number_weight","state", "draw", "keep_release")) %>%
  dplyr::mutate(value_SQ = as.numeric(value_SQ),
                value_alt = as.numeric(value_alt)) %>% 
  dplyr::filter(keep_release %in% c("release", "Discmortality") )

predictions_release_weight <- predictions_releases_merge %>%
  dplyr::filter(number_weight == "Weight") %>%
  dplyr::rename(value_alt_Weight=value_alt, value_SQ_Weight=value_SQ) %>%
  dplyr::mutate(perc_change_weight = (((value_alt_Weight-value_SQ_Weight)/value_SQ_Weight) * 100))

predictions_release_avg <- predictions_releases_merge %>%
  dplyr::filter(number_weight == "Weight_avg") 



predictions_releases_merge2<- predictions_release_avg %>% 
  dplyr::left_join(predictions_release_weight, by = c("Category","mode", "state","draw", "keep_release")) %>%
  dplyr::select(-number_weight.y) %>% 
  dplyr::mutate(imputed_value_alt= perc_change_weight/100,
                imputed_value_alt = value_SQ * imputed_value_alt, 
                imputed_value_alt=imputed_value_alt+value_SQ)  
#check = ((imputed_value_alt-value_SQ)/value_SQ)*100)

predictions_releases_merge2<- predictions_releases_merge2 %>% 
  #If imputed_value_alt is missing and value_alt_Weight is not missing, replace imputed_value_alt with value_alt_Weight
  dplyr::mutate(imputed_value_alt = dplyr::case_when(is.na(imputed_value_alt) & 
                                                       !is.na(value_alt_Weight) ~ value_alt_Weight, TRUE ~ imputed_value_alt)) %>% 
  #If imputed_value_alt is missing and value_alt_Weight IS missing, and value_SQ =0,  replace imputed_value_alt with 0
  dplyr::mutate(imputed_value_alt = dplyr::case_when(is.na(imputed_value_alt) & 
                                                       is.na(value_alt_Weight) & 
                                                       value_SQ==0 ~ 0, TRUE ~ imputed_value_alt))  

state_release_output<- predictions_releases_merge2 %>% 
  dplyr::mutate(domain=paste0(Category, "_", state, "_", keep_release)) %>% 
  dplyr::group_by(draw, domain) %>% 
  dplyr::summarise(imputed_value_alt_sum = sum(imputed_value_alt),
                   value_SQ_sum = sum(value_SQ)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(perc_diff=((imputed_value_alt_sum-value_SQ_sum)/value_SQ_sum)*100) %>% 
  dplyr::arrange(domain, draw) %>% 
  dplyr::mutate(perc_diff = dplyr::case_when(value_SQ_sum==0 &
                                               imputed_value_alt_sum==0 ~ 0, TRUE ~ perc_diff))

state_release_output <- state_release_output %>% 
  dplyr::group_by(domain) %>% 
  dplyr::arrange(domain,imputed_value_alt_sum) %>% 
  dplyr::mutate(n_weight = dplyr::row_number(domain)) %>% 
  dplyr::arrange(domain,perc_diff) %>% 
  dplyr::mutate(n_perc = dplyr::row_number(domain)) %>% 
  dplyr::arrange(domain,value_SQ_sum) %>% 
  dplyr::mutate(n_SQ = dplyr::row_number(domain)) %>% 
  dplyr::ungroup() 

categories_release_state=list()

for(d in unique(state_release_output$domain)){
  
  new<- state_release_output %>% 
    dplyr::filter(domain==d) #%>% 
  #dplyr::arrange(n_perc)
  
  lb_value_alt<- new$imputed_value_alt_sum[new$n_weight==11] 
  lb_perc_diff<- new$perc_diff[new$n_perc==11] 
  lb_value_SQ<- new$value_SQ_sum[new$n_SQ==11] 
  
  ub_value_alt<- new$imputed_value_alt_sum[new$n_weight==90] 
  ub_perc_diff<- new$perc_diff[new$n_perc==90] 
  ub_value_SQ<- new$value_SQ_sum[new$n_SQ==90] 
  
  median_value_alt<- median(new$imputed_value_alt_sum)
  median_perc_diff<- median(new$perc_diff)
  median_value_SQ<- median(new$value_SQ_sum)
  
  
  categories_release_state[[d]] <- as.data.frame(
    cbind(
      median_perc_diff,lb_perc_diff, ub_perc_diff,
      median_value_alt,lb_value_alt, ub_value_alt,
      median_value_SQ,lb_value_SQ, ub_value_SQ
    ))
  
  categories_release_state[[d]]$domain<-d
  
  
}
state_release_results= rlist::list.stack(categories_release_state, fill=TRUE)

state_release_results<- state_release_results %>% 
  tidyr::separate(domain, into = c("species", "region", "stat1")) %>% 
  dplyr::mutate(stat=dplyr::case_when(stat1=="release" ~ "release pounds", TRUE~stat1), 
                stat=dplyr::case_when(stat1=="Discmortality"~ "dead release pounds", TRUE~stat), 
                mode="all modes") %>% 
  dplyr::select(-stat1)

###########################
#state by mode-level release output
state_mode_release_output<- predictions_releases_merge2 %>% 
  dplyr::group_by(draw, Category, state, mode, keep_release) %>% 
  dplyr::summarise(imputed_value_alt_sum = sum(imputed_value_alt),
                   value_SQ_sum = sum(value_SQ)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(perc_diff=((imputed_value_alt_sum-value_SQ_sum)/value_SQ_sum)*100) %>% 
  dplyr::arrange(state, mode, Category, draw) %>% 
  dplyr::mutate(perc_diff = dplyr::case_when(value_SQ_sum==0 &
                                               imputed_value_alt_sum==0 ~ 0, TRUE ~ perc_diff))


state_mode_release_output <- state_mode_release_output %>% 
  dplyr::mutate(domain=paste0(state, "_", Category, "_", mode, "_", keep_release)) %>% 
  dplyr::group_by(domain) %>% 
  dplyr::arrange(domain,imputed_value_alt_sum) %>% 
  dplyr::mutate(n_weight = dplyr::row_number(domain)) %>% 
  dplyr::arrange(domain,perc_diff) %>% 
  dplyr::mutate(n_perc = dplyr::row_number(domain)) %>% 
  dplyr::arrange(domain,value_SQ_sum) %>% 
  dplyr::mutate(n_SQ = dplyr::row_number(domain)) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(domain,perc_diff) 

categories_releases_state_mode=list()

for(d in unique(state_mode_release_output$domain)){
  
  new<- state_mode_release_output %>% 
    dplyr::filter(domain==d) #%>% 
  #dplyr::arrange(n_perc)
  
  lb_value_alt<- new$imputed_value_alt_sum[new$n_weight==11] 
  lb_perc_diff<- new$perc_diff[new$n_perc==11] 
  lb_value_SQ<- new$value_SQ_sum[new$n_SQ==11] 
  
  ub_value_alt<- new$imputed_value_alt_sum[new$n_weight==90] 
  ub_perc_diff<- new$perc_diff[new$n_perc==90] 
  ub_value_SQ<- new$value_SQ_sum[new$n_SQ==90] 
  
  median_value_alt<- median(new$imputed_value_alt_sum)
  median_perc_diff<- median(new$perc_diff)
  median_value_SQ<- median(new$value_SQ_sum)
  
  
  categories_releases_state_mode[[d]] <- as.data.frame(
    cbind(
      median_perc_diff,lb_perc_diff, ub_perc_diff,
      median_value_alt,lb_value_alt, ub_value_alt,
      median_value_SQ,lb_value_SQ, ub_value_SQ
    ))
  
  categories_releases_state_mode[[d]]$domain<-d
  
  
}
state_mode_release_results= rlist::list.stack(categories_releases_state_mode, fill=TRUE)
state_mode_release_results<- state_mode_release_results %>% 
  tidyr::separate(domain, into = c("region", "species",  "mode", "stat1"))  %>% 
  dplyr::mutate(stat=dplyr::case_when(stat1=="release" ~ "release pounds", TRUE~stat1), 
                stat=dplyr::case_when(stat1=="Discmortality"~ "dead release pounds", TRUE~stat)) %>% 
  dplyr::select(-stat1)

release_ouput<- state_release_results %>% rbind(state_mode_release_results) 


### CV
CV_state_mode <- predictions_out10 %>% 
  dplyr::mutate(draw = as.numeric(draw)) %>%
  dplyr::rename(value_alt= Value) %>% 
  dplyr::left_join(StatusQuo, by=c("Category","mode", "number_weight","state", "draw")) %>%
  dplyr::mutate(value_SQ = as.numeric(value_SQ),
                value_alt = as.numeric(value_alt)) %>% 
  dplyr::filter(Category %in% c("CV", "ntrips"),
                mode!="all" ) %>% 
  dplyr::select(!c(param.x, param.y, keep_release.x, keep_release.y)) %>% 
  dplyr::mutate(value_SQ = as.numeric(value_SQ),
                value_alt = as.numeric(value_alt), 
                perc_diff=((value_alt-value_SQ)/value_SQ)*100)

state_CV<- CV_state_mode %>% 
  dplyr::group_by(draw, Category, state) %>% 
  dplyr::summarise(value_alt_sum = sum(value_alt),
                   value_SQ_sum = sum(value_SQ)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(perc_diff=(value_SQ_sum-value_alt_sum)) 


#sort observations and create index by species
state_CV<- state_CV %>% 
  dplyr::group_by(Category, state) %>% 
  dplyr::arrange(Category,state, value_alt_sum) %>% 
  dplyr::mutate(n_weight = dplyr::row_number(Category)) %>% 
  dplyr::arrange(Category,state, perc_diff) %>% 
  dplyr::mutate(n_perc = dplyr::row_number(Category)) %>% 
  dplyr::arrange(Category,state,value_SQ_sum) %>% 
  dplyr::mutate(n_SQ = dplyr::row_number(Category)) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(Category,state,perc_diff) %>% 
  dplyr::mutate(domain=paste0(state, "_", Category))



state_Cvs=list()

for(d in unique(state_CV$domain)){
  
  new<- state_CV %>% 
    dplyr::filter(domain==d) #%>% 
  #dplyr::arrange(n_perc)
  
  lb_value_alt<- new$value_alt_sum[new$n_weight==11] 
  lb_perc_diff<- new$perc_diff[new$n_perc==11] 
  lb_value_SQ<- new$value_SQ_sum[new$n_SQ==11] 
  
  ub_value_alt<- new$value_alt_sum[new$n_weight==90] 
  ub_perc_diff<- new$perc_diff[new$n_perc==90] 
  ub_value_SQ<- new$value_SQ_sum[new$n_SQ==90] 
  
  median_value_alt<- median(new$value_alt_sum)
  median_perc_diff<- median(new$perc_diff)
  median_value_SQ<- median(new$value_SQ_sum)
  
  
  state_Cvs[[d]] <- as.data.frame(
    cbind(
      median_perc_diff,lb_perc_diff, ub_perc_diff,
      median_value_alt,lb_value_alt, ub_value_alt,
      median_value_SQ,lb_value_SQ, ub_value_SQ
    ))
  
  state_Cvs[[d]]$domain<-d
  
  
}
state_CV_results= rlist::list.stack(state_Cvs, fill=TRUE)   
state_CV_results<- state_CV_results %>% 
  tidyr::separate(domain, into = c("region", "stat")) %>% 
  dplyr::mutate(mode="all modes", species="all species")



###########################
#state mode-level CV output
state_mode_CV_output<- CV_state_mode %>% 
  dplyr::group_by(draw, Category, state, mode) %>% 
  dplyr::summarise(value_alt_sum = sum(value_alt),
                   value_SQ_sum = sum(value_SQ)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(perc_diff=value_SQ_sum-value_alt_sum) %>% 
  dplyr::arrange(state, mode, Category, draw) %>% 
  dplyr::mutate(perc_diff = dplyr::case_when(value_SQ_sum==0 &
                                               value_alt_sum==0 ~ 0, TRUE ~ perc_diff))


state_mode_CV_output <- state_mode_CV_output %>% 
  dplyr::mutate(domain=paste0(state, "_", Category, "_", mode)) %>% 
  dplyr::group_by(domain) %>% 
  dplyr::arrange(domain,value_alt_sum) %>% 
  dplyr::mutate(n_weight = dplyr::row_number(domain)) %>% 
  dplyr::arrange(domain,perc_diff) %>% 
  dplyr::mutate(n_perc = dplyr::row_number(domain)) %>% 
  dplyr::arrange(domain,value_SQ_sum) %>% 
  dplyr::mutate(n_SQ = dplyr::row_number(domain)) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(domain,perc_diff) 

categories_CV_state_mode=list()

for(d in unique(state_mode_CV_output$domain)){
  
  new<- state_mode_CV_output %>% 
    dplyr::filter(domain==d) #%>% 
  #dplyr::arrange(n_perc)
  
  lb_value_alt<- new$value_alt_sum[new$n_weight==11] 
  lb_perc_diff<- new$perc_diff[new$n_perc==11] 
  lb_value_SQ<- new$value_SQ_sum[new$n_SQ==11] 
  
  ub_value_alt<- new$value_alt_sum[new$n_weight==90] 
  ub_perc_diff<- new$perc_diff[new$n_perc==90] 
  ub_value_SQ<- new$value_SQ_sum[new$n_SQ==90] 
  
  median_value_alt<- median(new$value_alt_sum)
  median_perc_diff<- median(new$perc_diff)
  median_value_SQ<- median(new$value_SQ_sum)
  
  
  categories_CV_state_mode[[d]] <- as.data.frame(
    cbind(
      median_perc_diff,lb_perc_diff, ub_perc_diff,
      median_value_alt,lb_value_alt, ub_value_alt,
      median_value_SQ,lb_value_SQ, ub_value_SQ
    ))
  
  categories_CV_state_mode[[d]]$domain<-d
  
  
}
state_mode_CV_results= rlist::list.stack(categories_CV_state_mode, fill=TRUE)
state_mode_CV_results<- state_mode_CV_results %>% 
  tidyr::separate(domain, into = c("region", "stat",  "mode"))  %>% 
  dplyr::mutate(species="all species")



##### Numbers 
alt<- predictions_out10 %>% 
  dplyr::mutate(draw = as.numeric(draw)) %>% 
  dplyr::rename(value_alt = Value) %>% 
  dplyr::filter(Category %in% c("sf", "bsb", "scup")) %>%
  dplyr::filter(mode!="all" ) %>%
  dplyr::filter(keep_release %in% c("keep", "release", "Discmortality") ) %>%
  dplyr::filter(number_weight %in% c("Number") ) %>% 
  dplyr::select(-param)


StatusQuo <- rbind(StatusQuo) %>% 
  dplyr::filter(Category %in% c("sf", "bsb", "scup")) %>%
  dplyr::filter(mode!="all" ) %>%
  dplyr::filter(keep_release %in% c("keep", "release", "Discmortality") ) %>%
  dplyr::filter(number_weight %in% c("Number") ) 

predictions_harv_num_merge <- alt %>% 
  dplyr::left_join(StatusQuo, by=c("Category","mode", "number_weight","state", "draw", "keep_release")) %>%
  dplyr::mutate(value_SQ = as.numeric(value_SQ),
                value_alt = as.numeric(value_alt))

###########################
#state-level output
state_harv_num_output<- predictions_harv_num_merge %>% 
  dplyr::mutate(domain=paste0(Category, "_", state, "_", keep_release)) %>% 
  dplyr::group_by(draw, domain) %>% 
  dplyr::summarise(value_alt_sum = sum(value_alt),
                   value_SQ_sum = sum(value_SQ)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(perc_diff=((value_alt_sum-value_SQ_sum)/value_SQ_sum)*100) %>% 
  dplyr::arrange(domain, draw) %>% 
  dplyr::mutate(perc_diff = dplyr::case_when(value_SQ_sum==0 &
                                               value_alt_sum==0 ~ 0, TRUE ~ perc_diff))

state_harv_num_output <- state_harv_num_output %>% 
  dplyr::group_by(domain) %>% 
  dplyr::arrange(domain,value_alt_sum) %>% 
  dplyr::mutate(n_weight = dplyr::row_number(domain)) %>% 
  dplyr::arrange(domain,perc_diff) %>% 
  dplyr::mutate(n_perc = dplyr::row_number(domain)) %>% 
  dplyr::arrange(domain,value_SQ_sum) %>% 
  dplyr::mutate(n_SQ = dplyr::row_number(domain)) %>% 
  dplyr::ungroup() 

categories_harv_num_state=list()

for(d in unique(state_harv_num_output$domain)){
  
  new<- state_harv_num_output %>% 
    dplyr::filter(domain==d) #%>% 
  #dplyr::arrange(n_perc)
  
  lb_value_alt<- new$value_alt_sum[new$n_weight==11] 
  lb_perc_diff<- new$perc_diff[new$n_perc==11] 
  lb_value_SQ<- new$value_SQ_sum[new$n_SQ==11] 
  
  ub_value_alt<- new$value_alt_sum[new$n_weight==90] 
  ub_perc_diff<- new$perc_diff[new$n_perc==90] 
  ub_value_SQ<- new$value_SQ_sum[new$n_SQ==90] 
  
  median_value_alt<- median(new$value_alt_sum)
  median_perc_diff<- median(new$perc_diff)
  median_value_SQ<- median(new$value_SQ_sum)
  
  
  categories_harv_num_state[[d]] <- as.data.frame(
    cbind(
      median_perc_diff,lb_perc_diff, ub_perc_diff,
      median_value_alt,lb_value_alt, ub_value_alt,
      median_value_SQ,lb_value_SQ, ub_value_SQ
    ))
  
  categories_harv_num_state[[d]]$domain<-d
  
  
}
state_harv_num_results= rlist::list.stack(categories_harv_num_state, fill=TRUE)

state_harv_num_results<- state_harv_num_results %>% 
  tidyr::separate(domain, into = c("species", "region", "stat1")) %>% 
  dplyr::mutate(stat=dplyr::case_when(stat1=="release" ~ "release numbers", TRUE~stat1),
                stat=dplyr::case_when(stat1=="Discmortality"~ "dead release numbers", TRUE~stat),
                stat=dplyr::case_when(stat1=="keep"~ "harvest numbers", TRUE~stat), 
                mode="all modes") %>% 
  dplyr::select(-stat1)


rm(lb_value_alt,lb_perc_diff,lb_value_SQ, ub_value_alt, ub_perc_diff,
   ub_value_SQ,median_value_alt,median_perc_diff,median_value_SQ)


###########################
#state by mode-level harvest num output
state_mode_harv_num_output<- predictions_harv_num_merge %>% 
  dplyr::group_by(draw, Category, state, mode, keep_release) %>% 
  dplyr::summarise(value_alt_sum = sum(value_alt),
                   value_SQ_sum = sum(value_SQ)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(perc_diff=((value_alt_sum-value_SQ_sum)/value_SQ_sum)*100) %>% 
  dplyr::arrange(state, mode, Category, draw) %>% 
  dplyr::mutate(perc_diff = dplyr::case_when(value_SQ_sum==0 & value_alt_sum==0 ~ 0, TRUE ~ perc_diff))


state_mode_harv_num_output <- state_mode_harv_num_output %>% 
  dplyr::mutate(domain=paste0(state, "_", Category, "_", mode, "_", keep_release)) %>% 
  dplyr::group_by(domain) %>% 
  dplyr::arrange(domain,value_alt_sum) %>% 
  dplyr::mutate(n_weight = dplyr::row_number(domain)) %>% 
  dplyr::arrange(domain,perc_diff) %>% 
  dplyr::mutate(n_perc = dplyr::row_number(domain)) %>% 
  dplyr::arrange(domain,value_SQ_sum) %>% 
  dplyr::mutate(n_SQ = dplyr::row_number(domain)) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(domain,value_SQ_sum) 

# state_mode_harv_num_output_check<-state_mode_harv_num_output %>% 
#   dplyr::filter(state=="NY" & Category=="scup" & keep_release=="keep" & mode=="fh")

categories_harv_num_state_mode=list()

for(d in unique(state_mode_harv_num_output$domain)){
  
  new<- state_mode_harv_num_output %>% 
    dplyr::filter(domain==d) #%>% 
  #dplyr::arrange(n_perc)
  
  lb_value_alt<- new$value_alt_sum[new$n_weight==11] 
  lb_perc_diff<- new$perc_diff[new$n_perc==11] 
  lb_value_SQ<- new$value_SQ_sum[new$n_SQ==11] 
  
  ub_value_alt<- new$value_alt_sum[new$n_weight==90] 
  ub_perc_diff<- new$perc_diff[new$n_perc==90] 
  ub_value_SQ<- new$value_SQ_sum[new$n_SQ==90] 
  
  median_value_alt<- median(new$value_alt_sum)
  median_perc_diff<- median(new$perc_diff)
  median_value_SQ<- median(new$value_SQ_sum)
  
  
  categories_harv_num_state_mode[[d]] <- as.data.frame(
    cbind(
      median_perc_diff,lb_perc_diff, ub_perc_diff,
      median_value_alt,lb_value_alt, ub_value_alt,
      median_value_SQ,lb_value_SQ, ub_value_SQ
    ))
  
  categories_harv_num_state_mode[[d]]$domain<-d
  
  
}
state_mode_harv_num_results= rlist::list.stack(categories_harv_num_state_mode, fill=TRUE)
state_mode_harv_num_results<- state_mode_harv_num_results %>% 
  tidyr::separate(domain, into = c("region", "species",  "mode", "stat1"))  %>% 
  dplyr::mutate(stat=dplyr::case_when(stat1=="release" ~ "release numbers", TRUE~stat1),
                stat=dplyr::case_when(stat1=="Discmortality"~ "dead release numbers", TRUE~stat),
                stat=dplyr::case_when(stat1=="keep"~ "harvest numbers", TRUE~stat)) %>% 
  dplyr::select(-stat1)


rm(lb_value_alt,lb_perc_diff,lb_value_SQ, ub_value_alt, ub_perc_diff,
   ub_value_SQ,median_value_alt,median_perc_diff,median_value_SQ)

#### predictions ####
predictions<- plyr::rbind.fill(state_mode_harv_num_results, state_harv_num_results,
                               state_harvest_results, state_mode_harvest_results,release_ouput) %>% 
  dplyr::mutate(reach_target = as.character(reach_target),
                reach_target = dplyr::case_when(median_value_SQ == 0 ~ "Not Applicable", TRUE ~ reach_target),
                reach_target = dplyr::case_when(species == "bsb" ~ "No harvest target", TRUE ~ reach_target),
                reach_target = dplyr::case_when(stat == "harvest numbers" ~ "No harvest target", TRUE ~ reach_target),
                reach_target = dplyr::case_when(stat == "release numbers" ~ "No harvest target", TRUE ~ reach_target),
                reach_target = dplyr::case_when(stat == "dead release numbers" ~ "No harvest target", TRUE ~ reach_target),
                median_perc_diff = round(median_perc_diff, 2), 
                median_value_alt = round(median_value_SQ + ((median_perc_diff/100)* median_value_SQ), 0), 
                median_value_SQ = round(median_value_SQ, 0)) %>% 
  plyr::rbind.fill(state_CV_results, state_mode_CV_results) %>% 
  dplyr::mutate(species = dplyr::recode(species, "bsb"= "Black Sea Bass", "sf" = "Summer Flounder", "scup" = "Scup"), 
                mode = dplyr::recode(mode, "fh" = "For Hire", "pr" = "Private", "sh" = "Shore"),
                draw = "Summary") %>%
  dplyr::select(region, stat, mode, draw, species, median_value_SQ, median_value_alt, median_perc_diff, reach_target) %>% 
  rbind(all_dat) %>% 
  dplyr::rename("State" = region,
                "Statistic" = stat,
                "Mode" = mode, 
                "Species" = species, 
                "Status-quo value (median)" = median_value_SQ, 
                "Alternative option value" = median_value_alt, 
                "% difference from status-quo outcome (median)" = median_perc_diff, 
                "% under harvest target (out of 100 simulations)" = reach_target)

