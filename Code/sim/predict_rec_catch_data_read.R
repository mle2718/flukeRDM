# Data read for non-shiny run of predict_rec_catch.R
## Run this script prior to predict rec catch


input_data_cd <- here::here("Data/")
iterative_input_data_cd <- here::here("Data/")
test_data_cd <- here::here("Data/")

# Variables to change 
dr<-1
st="MA"
md="pr"
      
# import necessary data
directed_trips<-feather::read_feather(file.path(input_data_cd, paste0("directed_trips_calibration_", st, ".feather"))) %>% 
  tibble::tibble() %>%
  dplyr::filter(draw == dr) %>%
  dplyr::select(mode, date, bsb_bag, bsb_min, fluke_bag,fluke_min, scup_bag, scup_min,
                bsb_bag_y2, bsb_min_y2, fluke_bag_y2,fluke_min_y2, scup_bag_y2, scup_min_y2) %>% 
  dplyr::filter(mode==md)

catch_data <- feather::read_feather(file.path(iterative_input_data_cd, paste0("projected_catch_draws_",st, "_", dr,".feather"))) %>% 
  dplyr::left_join(directed_trips, by=c("mode", "date"))%>% 
  dplyr::filter(mode==md)

catch_data<-catch_data %>% 
  dplyr::select(-cost, -total_trips_12, -age, -bsb_keep_sim, -bsb_rel_sim, -day_i, -my_dom_id_string, 
                -scup_keep_sim, -scup_rel_sim, -sf_keep_sim, -sf_rel_sim, -wave)

l_w_conversion <- readr::read_csv(file.path(test_data_cd, "L_W_Conversion.csv"), show_col_types = FALSE)  %>% 
  dplyr::filter(state==st)


sf_size_data <- readr::read_csv(file.path(test_data_cd, "fluke_projected_catch_at_lengths.csv"), show_col_types = FALSE)  %>% 
  dplyr::filter(state == st, draw==0 ) %>% 
  dplyr::filter(!is.na(fitted_prob)) %>% 
  dplyr::select(state, fitted_prob, length)

bsb_size_data <- readr::read_csv(file.path(test_data_cd, "bsb_projected_catch_at_lengths.csv"), show_col_types = FALSE)  %>% 
  dplyr::filter(state == st, draw==0 ) %>% 
  dplyr::filter(!is.na(fitted_prob)) %>% 
  dplyr::select(state, fitted_prob, length)

scup_size_data <- readr::read_csv(file.path(test_data_cd, "scup_projected_catch_at_lengths.csv"), show_col_types = FALSE)  %>% 
  dplyr::filter(state == st, draw==0 ) %>% 
  dplyr::filter(!is.na(fitted_prob)) %>% 
  dplyr::select(state,  fitted_prob, length)

# Pull in calibration comparison information about trip-level harvest/discard re-allocations 
calib_comparison<-feather::read_feather(file.path(iterative_input_data_cd, "calibration_comparison.feather")) %>% 
  dplyr::filter(state==st & draw==dr & mode==md)   


# pull data on the number of choice occasions in the calibration 
n_choice_occasions<-feather::read_feather(file.path(iterative_input_data_cd, paste0("n_choice_occasions_", st,"_", md, "_", dr, ".feather")))  
