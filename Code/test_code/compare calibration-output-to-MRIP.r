
# Global Variables: You'll need to define iterative_input_data_cd and input_data_cd paths



# Adjust project paths based on user
project_path<-"C:\Users\andrew.carr-harris\Desktop\Git\flukeRDM"  #  Lou's project path 
iterative_data_path<-"C:\Users\andrew.carr-harris\Desktop\flukeRDM_iterative_data" # Lou's path for iterative catch data that is too large to upload to GitHub*/

input_data_cd<-file.path(project_path,"Data")
input_code_cd<-file.path(project_path,"Code")
iterative_input_data_cd<-iterative_data_path
figure_cd<-file.path(project_path,"figures")


library(tidyverse)
library(readr)

# Initialize variables
tripz_files <- c()
statez <- c("MA", "RI")

# Main processing loop
for (s in statez) {
  for (i in 1:5) {
    
    # Load calibration catch draws data
    catch_data <- read_csv(paste0(iterative_input_data_cd, "/calib_catch_draws_", s, "_", i, ".dta"))
    
    # Collapse (aggregate) by date, day_i, mode, state
    catch_collapsed <- catch_data %>%
      group_by(date, day_i, mode, state) %>%
      summarise(
        sf_keep_sim = mean(sf_keep_sim, na.rm = TRUE),
        sf_cat = mean(sf_cat, na.rm = TRUE),
        sf_rel_sim = mean(sf_rel_sim, na.rm = TRUE),
        bsb_keep_sim = mean(bsb_keep_sim, na.rm = TRUE),
        bsb_rel_sim = mean(bsb_rel_sim, na.rm = TRUE),
        bsb_cat = mean(bsb_cat, na.rm = TRUE),
        scup_keep_sim = mean(scup_keep_sim, na.rm = TRUE),
        scup_rel_sim = mean(scup_rel_sim, na.rm = TRUE),
        scup_cat = mean(scup_cat, na.rm = TRUE),
        .groups = 'drop'
      )
    
    # Load directed trips data
    dtrip_data <- read_csv(paste0(input_data_cd, "/directed_trips_calibration_", s)) %>%
      filter(draw == i) %>%
      select(mode, date, dtrip)
    
    # Merge datasets
    merged_data <- catch_collapsed %>%
      left_join(dtrip_data, by = c("mode", "date")) %>%
      # Replace NA values with 0 (equivalent to mvencode)
      mutate(across(c(sf_keep_sim, sf_cat, sf_rel_sim, bsb_keep_sim, bsb_rel_sim, 
                      bsb_cat, scup_keep_sim, scup_rel_sim, scup_cat, dtrip), 
                    ~replace_na(.x, 0))) %>%
      # Rename variables
      rename(
        sf_cat_sim = sf_cat,
        bsb_cat_sim = bsb_cat,
        scup_cat_sim = scup_cat
      )
    
    # Create total variables
    vars_list <- c("sf_keep_sim", "sf_cat_sim", "sf_rel_sim", "bsb_keep_sim", 
                   "bsb_rel_sim", "bsb_cat_sim", "scup_keep_sim", "scup_rel_sim", "scup_cat_sim")
    
    for (v in vars_list) {
      merged_data[[paste0("tot_", v)]] <- merged_data$dtrip * merged_data[[v]]
    }
    
    # Collapse by mode
    trip_totals <- merged_data %>%
      group_by(mode) %>%
      summarise(
        across(starts_with("tot_"), sum, na.rm = TRUE),
        tot_dtrip_sim = sum(dtrip, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      mutate(
        draw = i,
        state = s
      )
    
    # Store temporary file path
    temp_file <- paste0("tripz", i, s, ".rds")
    saveRDS(trip_totals, temp_file)
    tripz_files <- c(tripz_files, temp_file)
  }
}

# Combine all temporary files
all_trips <- map_dfr(tripz_files, readRDS)

# Clean up temporary files
file.remove(tripz_files)

# Reorder columns and save
all_trips <- all_trips %>%
  select(state, mode, draw, everything())

saveRDS(all_trips, paste0(iterative_input_data_cd, "/simulated_catch_totals.rds"))

# Load the simulated catch totals
simulated_totals <- readRDS(paste0(iterative_input_data_cd, "/simulated_catch_totals.rds"))

# Calculate means and standard deviations by state and mode
catch_summary <- simulated_totals %>%
  group_by(state, mode) %>%
  summarise(
    # Means
    tot_sf_keep_sim = mean(tot_sf_keep_sim, na.rm = TRUE),
    tot_sf_cat_sim = mean(tot_sf_cat_sim, na.rm = TRUE),
    tot_sf_rel_sim = mean(tot_sf_rel_sim, na.rm = TRUE),
    tot_bsb_keep_sim = mean(tot_bsb_keep_sim, na.rm = TRUE),
    tot_bsb_rel_sim = mean(tot_bsb_rel_sim, na.rm = TRUE),
    tot_bsb_cat_sim = mean(tot_bsb_cat_sim, na.rm = TRUE),
    tot_scup_keep_sim = mean(tot_scup_keep_sim, na.rm = TRUE),
    tot_scup_rel_sim = mean(tot_scup_rel_sim, na.rm = TRUE),
    tot_scup_cat_sim = mean(tot_scup_cat_sim, na.rm = TRUE),
    tot_dtrip_sim = mean(tot_dtrip_sim, na.rm = TRUE),
    # Standard deviations
    sd_sf_keep_sim = sd(tot_sf_keep_sim, na.rm = TRUE),
    sd_sf_cat_sim = sd(tot_sf_cat_sim, na.rm = TRUE),
    sd_sf_rel_sim = sd(tot_sf_rel_sim, na.rm = TRUE),
    sd_bsb_keep_sim = sd(tot_bsb_keep_sim, na.rm = TRUE),
    sd_bsb_rel_sim = sd(tot_bsb_rel_sim, na.rm = TRUE),
    sd_bsb_cat_sim = sd(tot_bsb_cat_sim, na.rm = TRUE),
    sd_scup_keep_sim = sd(tot_scup_keep_sim, na.rm = TRUE),
    sd_scup_rel_sim = sd(tot_scup_rel_sim, na.rm = TRUE),
    sd_scup_cat_sim = sd(tot_scup_cat_sim, na.rm = TRUE),
    sd_dtrip_sim = sd(tot_dtrip_sim, na.rm = TRUE),
    .groups = 'drop'
  )

# Reshape from wide to long format
catch_long <- catch_summary %>%
  pivot_longer(
    cols = -c(mode, state),
    names_to = c("stat_type", "species", "disp", "extra"),
    names_pattern = "(tot|sd)_([^_]+)_([^_]+)_(.*)",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = stat_type,
    values_from = value,
    names_prefix = ""
  ) %>%
  rename(
    sim_total = tot,
    sim_sd = sd
  ) %>%
  # Clean up species and disp variables
  mutate(
    species = case_when(
      species == "dtrip" ~ "NA",
      TRUE ~ species
    ),
    disp = case_when(
      species == "dtrip" ~ "NA",
      disp == "sim" ~ "NA",
      TRUE ~ disp
    )
  ) %>%
  select(-extra)

# Load MRIP catch total calibration data
mrip_catch <- read_csv(paste0(iterative_input_data_cd, "/catch_total_calib_mrip.dta"))

# Reshape MRIP data
mrip_long <- mrip_catch %>%
  select(-se) %>%
  pivot_longer(
    cols = c(total, ll, ul),
    names_to = "stat_type",
    values_to = "value"
  ) %>%
  # Extract species and disp from column patterns (this may need adjustment based on actual data structure)
  separate(col = "additional_col_if_exists", into = c("species", "disp"), sep = "_", remove = FALSE) %>%
  pivot_wider(
    names_from = stat_type,
    values_from = value,
    names_prefix = "mrip_"
  )

# Merge simulated and MRIP data
combined_data <- catch_long %>%
  full_join(mrip_long, by = c("state", "mode", "species", "disp")) %>%
  mutate(
    sim_ul = sim_total + 1.96 * sim_sd,
    sim_ll = sim_total - 1.96 * sim_sd,
    my_dom_id_string = paste(state, mode, sep = "_")
  ) %>%
  arrange(species, disp, mode)

# Load directed trip MRIP data
dtrip_mrip <- read_csv(paste0(iterative_input_data_cd, "/directed_trip_calib_mrip.dta")) %>%
  rename(
    se_dtrip_mrip = se_mrip,
    ll_dtrip_mrip = ll,
    ul_dtrip_mrip = ul,
    tot_dtrip_mrip = dtrip_mrip
  ) %>%
  select(-year) %>%
  pivot_longer(
    cols = c(tot_dtrip_mrip, se_dtrip_mrip, ll_dtrip_mrip, ul_dtrip_mrip),
    names_to = c("stat_type", "extra1", "extra2"),
    names_pattern = "([^_]+)_([^_]+)_(.*)",
    values_to = "value"
  ) %>%
  mutate(
    disp = "NA",
    species = "NA",
    my_dom_id_string = paste(state, mode, sep = "_")
  ) %>%
  pivot_wider(
    names_from = c(stat_type, extra1, extra2),
    values_from = value,
    names_sep = "_",
    names_prefix = "mrip_"
  )

# Final merge
final_data <- combined_data %>%
  full_join(dtrip_mrip, by = c("state", "mode", "species", "disp", "my_dom_id_string")) %>%
  mutate(
    my_dom_id = as.numeric(factor(my_dom_id_string))
  )

# Create plots
species_list <- unique(final_data$species)
disp_list <- unique(final_data$disp)

# Create plotting function
create_comparison_plot <- function(data, sp, disp_val) {
  plot_data <- data %>%
    filter(species == sp, disp == disp_val) %>%
    mutate(
      my_dom_id_mrip = my_dom_id + 0.05,
      my_dom_id_sim = my_dom_id - 0.05,
      disp = case_when(
        disp == "rel" ~ "discards",
        disp == "keep" ~ "harvest",
        disp == "cat" ~ "catch",
        TRUE ~ disp
      )
    )
  
  if (nrow(plot_data) == 0) return(NULL)
  
  ggplot(plot_data) +
    # MRIP estimates
    geom_errorbar(aes(x = my_dom_id_mrip, ymin = mrip_ll, ymax = mrip_ul), 
                  color = "blue", width = 0.1) +
    geom_point(aes(x = my_dom_id_mrip, y = mrip_total), 
               color = "blue", shape = 16) +
    # Simulated estimates  
    geom_errorbar(aes(x = my_dom_id_sim, ymin = sim_ll, ymax = sim_ul), 
                  color = "red", width = 0.1) +
    geom_point(aes(x = my_dom_id_sim, y = sim_total), 
               color = "red", shape = 16) +
    scale_x_continuous(
      breaks = unique(plot_data$my_dom_id),
      labels = unique(plot_data$my_dom_id_string)
    ) +
    labs(
      title = paste(disp_val, "totals for", sp),
      x = "",
      y = "# fish"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.background = element_rect(fill = "white"),
      plot.background = element_rect(fill = "white")
    ) +
    scale_color_manual(
      name = "",
      values = c("blue", "red"),
      labels = c("MRIP estimate", "Simulated estimate")
    )
}

# Generate all plots
for (sp in species_list) {
  for (disp_val in disp_list) {
    if (!is.na(sp) && !is.na(disp_val)) {
      plot_obj <- create_comparison_plot(final_data, sp, disp_val)
      if (!is.null(plot_obj)) {
        print(plot_obj)
        # Uncomment to save plots
        # ggsave(paste0(sp, "_", disp_val, "_rcap.png"), plot = plot_obj, width = 10, height = 6, dpi = 100)
      }
    }
  }
}

# Final data preparation and ordering
final_output <- final_data %>%
  # Add any additional merges or data processing as needed
  select(my_dom_id_string, everything()) %>%
  arrange(state, mode)