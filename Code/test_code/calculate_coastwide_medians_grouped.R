## Generate coastwide values grouped

policy_lookup <- read_csv(here::here("Data/PolicyShortList.csv"))

# file path
filepath <- here::here("output/")
options(readr.show_col_types = FALSE)
options(tibble.name_repair = "minimal")
# select data for each state

policy_nested <- policy_lookup %>%
  group_by(Region, ID_Number) %>%
  summarise(
    state_map = list(setNames(Model_Name, State)),
    .groups = "drop"
  )

north_ids <- policy_nested %>% filter(Region == "North")
south_ids <- policy_nested %>% filter(Region == "South")
nj_ids    <- policy_nested %>% filter(Region == "NJ")

run_grid <- expand.grid(
  north = north_ids$ID_Number,
  south = south_ids$ID_Number,
  nj    = nj_ids$ID_Number,
  stringsAsFactors = FALSE
)

filenames <- list.files(path = here::here("output/"), pattern = "\\.csv$", full.names = TRUE)
file_states <- str_extract(filenames, "(?<=output_)[A-Z]{2}")


## Get status quo
SQ_filenames <- list.files(path = here::here("output/"), pattern = "SQ.*\\.csv$", full.names = TRUE)

SQ_data_list <- map_dfr(SQ_filenames, read_csv, show_col_types = FALSE)  

SQ_data_adj <- SQ_data_list %>% 
  select(-any_of(c("...8", "filename"))) %>% 
  filter( case_when(
    state %in% c("MA", "CT", "NY", "NJ", "DE", "VA", "NC") & draw %in% c(20, 21, 78) ~ FALSE,
    state == "MD" & model != "Lou_SQ" &draw %in% c(20, 21) ~ FALSE,
    state == "MD" & model == "Lou_SQ" & draw %in% c(20, 21, 78) ~ FALSE,
    state == "RI" & model != "Lou_SQ" & draw %in% c(76) ~ FALSE,
    state == "RI" & model == "Lou_SQ" & draw %in% c(20, 21, 78) ~ FALSE,
    TRUE ~ TRUE))

SQ_draw_map <- SQ_data_adj %>%
  distinct(state, model, draw) %>%
  group_by(state, model) %>%
  arrange(draw, .by_group = TRUE) %>%
  mutate(new_draw = row_number()) %>%
  ungroup()

SQ <- SQ_data_adj %>%
  left_join(SQ_draw_map, by = c("state", "model", "draw")) %>%
  mutate(draw = new_draw, 
         SQ_value = value, 
         SQ_model = model) %>%
  select(-c(new_draw, value, model)) 

#all_results <- vector("list", nrow(policy_grid))
all_results <- vector("list", nrow(run_grid))

for(i in seq_len(nrow(run_grid))) {
  
  north_id <- run_grid$north[i]
  south_id <- run_grid$south[i]
  nj_id    <- run_grid$nj[i]
  
  # Extract state policies
  north_map <- north_ids %>%
    filter(ID_Number == north_id) %>%
    pull(state_map) %>%
    .[[1]]
  
  south_map <- south_ids %>%
    filter(ID_Number == south_id) %>%
    pull(state_map) %>%
    .[[1]]
  
  nj_map <- nj_ids %>%
    filter(ID_Number == nj_id) %>%
    pull(state_map) %>%
    .[[1]]
  
  # Merge them into one state → policy map
  policy_map_run <- c(north_map, south_map, nj_map)
  
  files_keep <- filenames[
    mapply(function(file, state) {
      
      policy <- policy_map_run[[state]]
      if (is.null(policy)) return(FALSE)
      
      stringr::str_detect(file, paste0("_", policy, "_"))
      
    }, filenames, file_states)
  ]
  
  data_list <- map_dfr(files_keep, read_csv)
  
  data_adj <- data_list %>% 
    select(-any_of(c("...8", "filename"))) %>% 
    filter( case_when(
      state %in% c("MA", "CT", "NY", "NJ", "DE", "VA", "NC") & draw %in% c(20, 21, 78) ~ FALSE,
      state == "MD" & model != "Lou_SQ" &draw %in% c(20, 21) ~ FALSE,
      state == "MD" & model == "Lou_SQ" & draw %in% c(20, 21, 78) ~ FALSE,
      state == "RI" & model != "Lou_SQ" & draw %in% c(76) ~ FALSE,
      state == "RI" & model == "Lou_SQ" & draw %in% c(20, 21, 78) ~ FALSE,
      TRUE ~ TRUE))
  
  draw_map <- data_adj %>%
    distinct(state, model, draw) %>%
    group_by(state, model) %>%
    arrange(draw, .by_group = TRUE) %>%
    mutate(new_draw = row_number()) %>%
    ungroup()
  
  df <- data_adj %>%
    left_join(draw_map, by = c("state", "model", "draw")) %>%
    mutate(draw = new_draw) %>%
    select(-new_draw)
  
  data<- df %>% left_join(SQ, by = c("metric", "species", "mode", "state", "draw"))
  
  #Calculate coastwide median 
  
  harv <- data %>% #all_data %>%
    dplyr::filter(metric == "keep_weight" & mode == "all modes") %>%
    dplyr::group_by(metric, species, mode, draw) %>% 
    dplyr::summarise(value = sum(value), 
                     SQ_value = sum(SQ_value)) %>% 
    dplyr::mutate(pct_diff = (value - SQ_value) / (SQ_value)  * 100) %>%
    dplyr::group_by(species, metric) %>%
    dplyr::summarise(median_pct_diff = round(median(pct_diff),2)) %>%
    tidyr::pivot_wider(names_from = species, values_from = median_pct_diff)
  
  north_df <- as_tibble_row(north_map)
  south_df <- as_tibble_row(south_map)
  nj_df <- as_tibble_row(nj_map)
  results <- cbind(
    North_ID = north_id,
    South_ID = south_id,
    NJ_ID = nj_id,
    north_df, 
    south_df, 
    nj_df,
    harv
  )
  all_results <- dplyr::bind_rows(all_results, results)
  
  rm(harv, data_adj, draw_map, df, data)
}

write.csv(
  all_results,
  here::here("all_coastwide_results1.csv"),
  row.names = FALSE)

