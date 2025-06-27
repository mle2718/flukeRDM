saved_regs <- data.frame(run_names = list.files(path = here::here("saved_regs"))) %>% 
  dplyr::mutate(run_names = gsub("^regs_|\\.csv$", "", run_names))

output <- data.frame(run_names = list.files(path = here::here("output"))) %>% 
  dplyr::mutate(run_names = gsub("^output_|_[0-9]+_[0-9]+|\\.csv$", "", run_names))

compare <- saved_regs %>% 
  dplyr::anti_join(output, by = colnames(saved_regs)) %>% 
  dplyr::mutate(run_names = paste0("regs_", run_names, ".csv"))

for i in compare{
  
}