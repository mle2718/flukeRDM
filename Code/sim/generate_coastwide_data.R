## Coastwide medians
library(dplyr)
library(stringr)
library(readr)
library(purrr)
library(tidyr)

## Find all runs and states that we need to incorporate SQ
### Read in all of output folder

files <- list.files(path = here::here("output/"), pattern = "\\.csv$", full.names = TRUE)

all_data <- files %>%
  set_names(files) %>%  # Optional: keep file names for reference
  purrr::map_dfr(readr::read_csv, .id = "filename", col_select=all_of(read_cols), col_types=read_cols_types) %>% 
  dplyr::mutate(filename = stringr::str_extract(filename, "(?<=output_).+?(?=_202)"))

state_list <- unique(all_data$state)
for (i in unique(all_data$model)){
  df <- all_data %>% dplyr::filter(model == i)
  
  states_present <- unique(df$state)
  
  ### Compare list
  states_to_get <- setdiff(state_list, states_present)
  
  missing_states <- all_data %>% 
    dplyr::filter(state %in% states_to_get & model == "SQ")
  
  ### Add SQ states to variables
  df <- rbind(df, missing_states) %>% 
    dplyr::mutate(model = i)
  
  
  ## Fill empty runs 
  expected_draws = 1:100
  ### RI
  ri <- df %>% dplyr::filter(state == "RI")
  
  ## Using random number generator 40 and 83 
  ri_dupl <- ri %>% dplyr::filter(draw %in% c(40, 83)) %>% 
    distinct() %>% 
    dplyr::mutate(draw = recode(draw,`40` = 99,  `83` = 100))
  
  ri <- rbind(ri, ri_dupl)
  
  ### MD
  md <- df %>% dplyr::filter(state == "MD")
  length(unique(md$draw))
  
  ## Using random number generator 31
  md_dupl <- md %>% dplyr::filter(draw %in% c(31)) %>% 
    distinct() %>% 
    dplyr::mutate(draw = recode(draw,`31` = 100))
  
  md <- rbind(md, md_dupl)
  
  df<- df %>% dplyr::filter(state != "RI",
                            state != "MD") %>% 
    rbind(ri, md)
  
  ## Write new data file out
  write.csv(df, here::here(paste("output_coastwide/", i, ".csv")))
} 





## Read in coastwide folder 
files <- list.files(path = here::here("output_coastwide/"), pattern = "\\.csv$", full.names = TRUE)

coastwide <- files %>%
  set_names(files) %>%  # Optional: keep file names for reference
  purrr::map_dfr(readr::read_csv, .id = "filename", col_select=all_of(read_cols), col_types=read_cols_types) 


## Sum all run 1,2,3...
coastwide2<- coastwide %>% dplyr::group_by(metric, species, mode, draw, model) %>% 
  dplyr::reframe(value = sum(value)) 


## Take medians of sums

