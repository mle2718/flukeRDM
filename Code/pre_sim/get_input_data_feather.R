# Code to get feather files from a google drive url.
# This is a piece of utility code, it is not used to run the actual RDM
# 
library(here)
library(googledrive)
library(purrr)
library(glue)
here::i_am("Code/pre_sim/get_input_data_feather.R")


# 
# # This puts the url for the flukeRDM gooogle drive into the object "flukeRDM_url"

source(here("Code","pre_sim","flukeRDM_url.R"))



flukeRDM<-drive_get(path=flukeRDM_url)
files_in_folder <- drive_ls(flukeRDM)


# There are alot of files.  This will copy them all to "Data". It WILL overwrite.
walk2(
  files_in_folder$id,
  files_in_folder$name,
  ~ drive_download(
    file = .x,
   path = here("Data",.y),
   overwrite = TRUE
  )
)



# For testing purpose, here is some code to filter 
# 
# # This should get files that match the pattern 1?.feather (that is 10.feather, 11.feather, ..., 19.feather). It will NOT match 1.feather or 100.feather

# files_in_folder2 <- files_in_folder[grepl("^.*1.\\.feather$", files_in_folder$name, ignore.case = TRUE), ]
# 
# 
# # This will pull all the Massachusetts feathers
# files_in_folder2 <- files_in_folder[grepl("^.*MA.*\\.feather$", files_in_folder$name, ignore.case = TRUE), ]
# 
# # There are alot of files.  This will copy them all to "Data". It will not overwrite.
# walk2(
#   files_in_folder2$id,
#   files_in_folder2$name,
#   ~ drive_download(
#     file = .x,
#     path = here("Data",.y),
#     overwrite = FALSE
#   )
# )
# 
