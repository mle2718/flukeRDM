# Code to get feather files from a google drive url.
# This is a piece of utility code, it is not used to run the actual RDM
# 
library(here)
library(googledrive)
library(purrr)
library(glue)
library(dplyr)
library(conflicted)
conflicts_prefer(dplyr::filter)
here::i_am("Code/pre_sim/get_input_data_feather.R")


# 
# # This puts the url for the flukeRDM gooogle drive into the object "flukeRDM_url"

source(here("Code","pre_sim","flukeRDM_url.R"))

flukeRDM<-drive_get(path=flukeRDM_input_data_url)
files_in_folder <- drive_ls(flukeRDM)

# There are alot of files.  This will copy them all to "Data". It WILL overwrite.
# It is not recursive
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




###############################################################################
###############################################################################
###############################################################################
# Function to recursively get all files and their paths
get_files_recursive <- function(folder_item, current_path = "") {
  # Get contents of current folder
  contents <- drive_ls(folder_item)
  
  all_files <- data.frame()
  
  if (nrow(contents) > 0) {
    for (i in 1:nrow(contents)) {
      item <- contents[i, ]
      item_path <- file.path(current_path, item$name)
      
      if (item$drive_resource[[1]]$mimeType == "application/vnd.google-apps.folder") {
        # It's a folder - recurse into it
        subfolder_files <- get_files_recursive(item, item_path)
        all_files <- rbind(all_files, subfolder_files)
      } else {
        # It's a file - add to list
        file_info <- data.frame(
          id = item$id,
          name = item$name,
          path = current_path,
          full_path = item_path,
          stringsAsFactors = FALSE
        )
        all_files <- rbind(all_files, file_info)
      }
    }
  }
  
  return(all_files)
}



###############################################################################
# Get all subfolders
subfolder_list<-c("a_projected_catch_draws","aa_L_W_conversion","aaa_catch_weights_SQ")


for (k in 1:nrow(subfolder_list)) {
  
  subfolder_name <- subfolder_list[k]
    
  subfolder <- files_in_folder %>% 
    filter(name == subfolder_name)

  if (nrow(subfolder) > 0) {
    # Download from this specific subfolder
    subfolder_files <- get_files_recursive(subfolder[1, ], subfolder_name)
    
    # Download preserving structure
    for (i in 1:nrow(subfolder_files)) {
      local_path <- here("Data", subfolder_files$path[i])
      if (!dir.exists(local_path) && local_path != here("Data")) {
        dir.create(local_path, recursive = TRUE)
      }
      
      local_file_path <- here("Data", subfolder_files$full_path[i])
      drive_download(
        file = subfolder_files$id[i],
        path = local_file_path,
        overwrite = TRUE
      )
      
      cat("Downloaded:", subfolder_files$full_path[i], "\n")
    }
  }
}


###############################################################################
###############################################################################
###############################################################################


###############################################################################
# No paths,everything into the Data directory
###############################################################################

# for (k in 1:nrow(subfolder_list)) {
#   subfolder_name <- subfolder_list[k]
#   
#   if (nrow(subfolder) > 0) {
#     # Download from this specific subfolder
#     subfolder_files <- get_files_recursive(subfolder[1, ], subfolder_name)
#   
#     # Download preserving structure
#     for (i in 1:nrow(subfolder_files)) {
#       local_path <- here("Data", subfolder_files$path[i])
#       local_file_path <- here("Data", subfolder_files$full_path[i])
#       drive_download(
#         file = subfolder_files$id[i],
#         path = here("Data",subfolder_files$name[i]),
#         overwrite = TRUE
#       )
#   
#       cat("Downloaded:", subfolder_files$name[i], "\n")
#     }
#   }
# }



