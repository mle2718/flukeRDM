### Injest run name and run model

# Rscript Run_Model.R Run_Name

library(magrittr)

args <- commandArgs(trailingOnly = TRUE)

saved_regs<- read.csv(here::here(paste0("saved_regs/regs_", args[1], ".csv")))


## Massachusetts
if(any(grepl("ma", saved_regs$input))){
  
  save_regs <- saved_regs %>%
    dplyr::filter(grepl("ma", saved_regs$input))
  
  source(here::here("recDST/model_run_MA.R"))
}

## Rhode Island
if(any(grepl("ri", saved_regs$input))){
  
  save_regs <- saved_regs %>%
    dplyr::filter(grepl("ri", saved_regs$input))
  
  source(here::here("recDST/model_run_RI.R"))
}