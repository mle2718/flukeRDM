saved_regs <- data.frame(run_names = list.files(path = here::here("saved_regs"))) %>% 
  dplyr::mutate(run_names = gsub("^regs_|\\.csv$", "", run_names))

output <- data.frame(run_names = list.files(path = here::here("output"))) %>% 
  dplyr::mutate(run_names = gsub("^output_|_[0-9]+_[0-9]+|\\.csv$", "", run_names))

compare <- saved_regs %>% 
  dplyr::anti_join(output, by = colnames(saved_regs)) %>% 
  dplyr::mutate(run_names = paste0("regs_", run_names, ".csv"))

for(i in compare$run_names){
  saved_regs <- read.csv(file.path(here::here(paste0("saved_regs/", i))))
  
  
  
  
  
  
  ## Massachusetts
  if(any(grepl("ma", saved_regs$input))){
  
    save_regs <- saved_regs %>%
      dplyr::filter(grepl("ma", saved_regs$input))
    
    for (a in seq_len(nrow(save_regs))) {
      # Extract name and value
      obj_name <- save_regs$input[a]
      obj_value <- save_regs$value[a]
      
      # Assign to object in the environment
      assign(obj_name, obj_value)
    }
    
    source(here::here("recDST/model_run_MA.R"))
  }



## Rhode Island
if(any(grepl("ri", saved_regs$input))){
  
  save_regs <- saved_regs %>%
    dplyr::filter(grepl("ri", saved_regs$input))
  
  for (a in seq_len(nrow(save_regs))) {
    # Extract name and value
    obj_name <- save_regs$input[a]
    obj_value <- save_regs$value[a]
    
    # Assign to object in the environment
    assign(obj_name, obj_value)
  }
  
  source(here::here("recDST/model_run_RI.R"))
}
  
  ## Connecticut
  if(any(grepl("ct", saved_regs$input))){
    
    save_regs <- saved_regs %>%
      dplyr::filter(grepl("ct", saved_regs$input))
    
    for (a in seq_len(nrow(save_regs))) {
      # Extract name and value
      obj_name <- save_regs$input[a]
      obj_value <- save_regs$value[a]
      
      # Assign to object in the environment
      assign(obj_name, obj_value)
    }
    
    source(here::here("recDST/model_run_CT.R"))
  }
  
  ## New York
  if(any(grepl("ny", saved_regs$input))){
    
    save_regs <- saved_regs %>%
      dplyr::filter(grepl("ny", saved_regs$input))
    
    for (a in seq_len(nrow(save_regs))) {
      # Extract name and value
      obj_name <- save_regs$input[a]
      obj_value <- save_regs$value[a]
      
      # Assign to object in the environment
      assign(obj_name, obj_value)
    }
    
    source(here::here("recDST/model_run_NY.R"))
  }
  
  ## New Jersey
  if(any(grepl("nj", saved_regs$input))){
    
    save_regs <- saved_regs %>%
      dplyr::filter(grepl("nj", saved_regs$input))
    
    for (a in seq_len(nrow(save_regs))) {
      # Extract name and value
      obj_name <- save_regs$input[a]
      obj_value <- save_regs$value[a]
      
      # Assign to object in the environment
      assign(obj_name, obj_value)
    }
    
    source(here::here("recDST/model_run_NJ.R"))
  }
  
  ## Deleware
  if(any(grepl("de", saved_regs$input))){
    
    save_regs <- saved_regs %>%
      dplyr::filter(grepl("de", saved_regs$input))
    
    for (a in seq_len(nrow(save_regs))) {
      # Extract name and value
      obj_name <- save_regs$input[a]
      obj_value <- save_regs$value[a]
      
      # Assign to object in the environment
      assign(obj_name, obj_value)
    }
    
    source(here::here("recDST/model_run_DE.R"))
  }
  
  ## MAryland
  if(any(grepl("md", saved_regs$input))){
    
    save_regs <- saved_regs %>%
      dplyr::filter(grepl("md", saved_regs$input))
    
    for (a in seq_len(nrow(save_regs))) {
      # Extract name and value
      obj_name <- save_regs$input[a]
      obj_value <- save_regs$value[a]
      
      # Assign to object in the environment
      assign(obj_name, obj_value)
    }
    
    source(here::here("recDST/model_run_MD.R"))
  }
  
  ## Virginia
  if(any(grepl("va", saved_regs$input))){
    
    save_regs <- saved_regs %>%
      dplyr::filter(grepl("",va saved_regs$input))
    
    for (a in seq_len(nrow(save_regs))) {
      # Extract name and value
      obj_name <- save_regs$input[a]
      obj_value <- save_regs$value[a]
      
      # Assign to object in the environment
      assign(obj_name, obj_value)
    }
    
    source(here::here("recDST/model_run_VA.R"))
  }
  
  ## North Carolina
  if(any(grepl("nc", saved_regs$input))){
    
    save_regs <- saved_regs %>%
      dplyr::filter(grepl("nc", saved_regs$input))
    
    for (a in seq_len(nrow(save_regs))) {
      # Extract name and value
      obj_name <- save_regs$input[a]
      obj_value <- save_regs$value[a]
      
      # Assign to object in the environment
      assign(obj_name, obj_value)
    }
    
    source(here::here("recDST/model_run_NC.R"))
  }
}