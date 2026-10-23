
################################################################################
# Script:       export_all_to_GDrive.R
# Purpose:      Synchronize local simulation outputs with Google Drive.
#               Upload new/changed files, retry rate-limited requests,
#               and report missing files and upload failures.
# Dependencies: googledrive, here, developer_setup.R, cached credentials
# Pipeline:     Sourced by model_wrapper.do after calibration.
################################################################################

#===============================================================================
# 1. SETTINGS
#===============================================================================

n_simulations <- 101
cat("Number of Simulations:", n_simulations, "\n")

library(here)
library(googledrive)

code_cd <- here::here("Code", "sim")
source(here::here("Code", "helpers", "developer_setup.R"))

# Google Drive settings
shared_drive_name <- "NMFS NEC READ SSB"
drive_root <- "socialsci/RecreationalDST/2027_management_cycle_data/flukeRDM"

# Upload settings
compare_mode <- "size_time"  # "size_time", "size", or "always"
max_attempts <- 8
upload_delay <- 0.5
max_retry_wait <- 120
timestamp_tolerance <- 2

# "size_time": replace if size differs or local file is newer
# "size":      replace only if file size differs
# "always":    replace every existing file
#
# Note: size_time is a heuristic, not a checksum comparison.
# Files with identical size and older timestamps can still differ.

# Authenticate
googledrive::drive_auth(
  cache = here::here(".secrets"),
  email = TRUE
)

#===============================================================================
# 2. GOOGLE DRIVE RETRY FUNCTION
#===============================================================================

# Retry temporary Google API errors using exponential backoff.
# Do not retry unrelated errors, such as invalid permissions or paths.

is_retryable_error <- function(e) {
  msg <- conditionMessage(e)
  
  grepl(
    "userRateLimitExceeded|rateLimitExceeded|rate limit|quota exceeded|429|500|502|503|504",
    msg,
    ignore.case = TRUE
  )
}

drive_retry <- function(fun, label = "Drive request", n_attempts = max_attempts) {
  
  for (attempt in seq_len(n_attempts)) {
    
    result <- tryCatch(
      fun(),
      error = function(e) e
    )
    
    if (!inherits(result, "error")) {
      return(result)
    }
    
    if (!is_retryable_error(result)) {
      stop(result)
    }
    
    if (attempt == n_attempts) {
      stop(
        label, " failed after ", n_attempts,
        " attempts: ", conditionMessage(result)
      )
    }
    
    wait <- min(
      max_retry_wait,
      2^(attempt - 1) + stats::runif(1, 0, 3)
    )
    
    message(
      "[Rate limit] ", label,
      " | attempt ", attempt, "/", n_attempts,
      " | waiting ", round(wait, 1), " seconds"
    )
    
    Sys.sleep(wait)
  }
}

#===============================================================================
# 3. RESOLVE GOOGLE DRIVE FOLDER IDS
#===============================================================================

# Resolve each destination folder only once.

drive_folder_names <- c(
  "base_outcomes",
  "n_choice_occasions",
  "calib_catch_draws",
  "miscellaneous",
  "calibration_checkpoints",
  "proj_catch_draws",
  "figures"
)

drive_folder_paths <- c(
  paste0(drive_root, "/base_outcomes"),
  paste0(drive_root, "/n_choice_occasions"),
  paste0(drive_root, "/calib_catch_draws"),
  paste0(drive_root, "/miscellaneous"),
  paste0(drive_root, "/miscellaneous/calibration_checkpoints"),
  paste0(drive_root, "/proj_catch_draws"),
  paste0(drive_root, "/figures")
)

drive_folder_ids <- setNames(
  rep(NA_character_, length(drive_folder_names)),
  drive_folder_names
)

message("Resolving Google Drive folders...")

for (i in seq_along(drive_folder_names)) {
  
  folder_name <- drive_folder_names[i]
  folder_path <- drive_folder_paths[i]
  
  message("Resolving: ", folder_name)
  
  info <- drive_retry(
    fun = function() {
      googledrive::drive_get(
        path = folder_path,
        shared_drive = shared_drive_name
      )
    },
    label = paste("Resolve folder", folder_name)
  )
  
  if (nrow(info) != 1) {
    stop("Expected exactly one Drive folder for: ", folder_path)
  }
  
  drive_folder_ids[folder_name] <- info$id[1]
}

# Preserve original folder ID variable names.
base_outcomes_path <- drive_folder_ids["base_outcomes"]
n_choice_occasions_path <- drive_folder_ids["n_choice_occasions"]
calib_catch_draws_path <- drive_folder_ids["calib_catch_draws"]
miscellaneous_path <- drive_folder_ids["miscellaneous"]
calibration_checkpoints_path <- drive_folder_ids["calibration_checkpoints"]
proj_catch_draws_path <- drive_folder_ids["proj_catch_draws"]
figures_path <- drive_folder_ids["figures"]

message("All Google Drive folders resolved.")

#===============================================================================
# 4. LOCAL FOLDER SETUP
#===============================================================================

final_process_data_cd <- sf.data.dir

final_process_outcomes_cd <- file.path(final_process_data_cd, "base_outcomes")
final_process_choice_occasions_cd <- file.path(final_process_data_cd, "n_choice_ocassions")
final_process_misc_cd <- file.path(final_process_data_cd, "miscellaneous")
final_process_calib_catch_cd <- file.path(final_process_data_cd, "calib_catch_draws")
#final_process_calib_checkpoints_cd <- file.path(final_process_data_cd, "miscellaneous", "calibration_checkpoints")
final_process_proj_catch_draws_cd <- file.path(final_process_data_cd, "proj_catch_draws")
final_process_figures_cd <- file.path(final_process_data_cd, "figures")

#===============================================================================
# 5. HELPER: EXTRACT DRIVE FILE METADATA
#===============================================================================

# googledrive stores additional file metadata in drive_resource.
# Extract size and modifiedTime for comparison with local files.

get_drive_metadata <- function(drive_files) {
  
  if (nrow(drive_files) == 0) {
    return(data.frame(
      name = character(),
      id = character(),
      size = numeric(),
      modified_time = as.POSIXct(character(), tz = "UTC")
    ))
  }
  
  resource <- drive_files$drive_resource
  
  size <- vapply(
    resource,
    function(x) {
      value <- x$size
      if (is.null(value)) NA_real_ else as.numeric(value)
    },
    numeric(1)
  )
  
  modified_time <- vapply(
    resource,
    function(x) {
      value <- x$modifiedTime
      if (is.null(value)) NA_character_ else as.character(value)
    },
    character(1)
  )
  
  data.frame(
    name = drive_files$name,
    id = drive_files$id,
    size = size,
    modified_time = as.POSIXct(
      modified_time,
      format = "%Y-%m-%dT%H:%M:%OS",
      tz = "UTC"
    ),
    stringsAsFactors = FALSE
  )
}

#===============================================================================
# 6. HELPER: DETERMINE WHETHER FILE NEEDS UPDATING
#===============================================================================

file_needs_update <- function(local_file, drive_row,
                              compare_mode = compare_mode) {
  
  if (compare_mode == "always") {
    return(TRUE)
  }
  
  local_info <- file.info(local_file)
  
  local_size <- as.numeric(local_info$size)
  local_time <- as.POSIXct(local_info$mtime, tz = "UTC")
  
  drive_size <- as.numeric(drive_row$size)
  drive_time <- as.POSIXct(drive_row$modified_time, tz = "UTC")
  
  # Missing metadata: update to be safe.
  if (is.na(local_size) || is.na(drive_size)) {
    return(TRUE)
  }
  
  if (local_size != drive_size) {
    return(TRUE)
  }
  
  if (compare_mode == "size") {
    return(FALSE)
  }
  
  # If local file is newer, update it.
  if (is.na(local_time) || is.na(drive_time)) {
    return(TRUE)
  }
  
  as.numeric(difftime(local_time, drive_time, units = "secs")) > timestamp_tolerance
}

#===============================================================================
# 7. UPLOAD FOLDER TO GOOGLE DRIVE
#===============================================================================

upload_folder_to_drive <- function(
    local_folder,
    drive_folder_id,
    pattern = "\\.(fst|csv|xlsx|dta)$",
    compare_mode = compare_mode,
    recursive = FALSE) {
  
  folder_name <- basename(local_folder)
  
  message("\n====================================================")
  message("Processing folder: ", folder_name)
  message("====================================================")
  
  if (!dir.exists(local_folder)) {
    warning("Local folder does not exist: ", local_folder)
    return(data.frame(
      folder = folder_name,
      file = NA_character_,
      status = "missing_local_folder",
      details = "Local folder does not exist"
    ))
  }
  
  files_to_upload <- list.files(
    path = local_folder,
    pattern = pattern,
    full.names = TRUE,
    recursive = recursive,
    ignore.case = TRUE
  )
  
  files_to_upload <- files_to_upload[
    !dir.exists(files_to_upload)
  ]
  
  if (length(files_to_upload) == 0) {
    message("No matching files found.")
    
    return(data.frame(
      folder = folder_name,
      file = NA_character_,
      status = "no_local_files",
      details = "No matching files"
    ))
  }
  
  message("Local files found: ", length(files_to_upload))
  
  # List Drive folder once, rather than for every file.
  existing_files <- drive_retry(
    fun = function() {
      googledrive::drive_ls(
        path = googledrive::as_id(drive_folder_id)
      )
    },
    label = paste("List folder", folder_name)
  )
  
  drive_meta <- get_drive_metadata(existing_files)
  
  # Initialize results.
  results <- vector("list", length(files_to_upload))
  
  for (i in seq_along(files_to_upload)) {
    
    f <- files_to_upload[i]
    file_name <- basename(f)
    
    message(
      "[", i, "/", length(files_to_upload), "] ",
      file_name
    )
    
    existing_match <- drive_meta[
      drive_meta$name == file_name,
      ,
      drop = FALSE
    ]
    
    status <- NA_character_
    details <- NA_character_
    
    # Google Drive permits duplicate file names.
    # Do not update an arbitrary file if multiple matches exist.
    if (nrow(existing_match) > 1) {
      
      status <- "duplicate_drive_names"
      details <- "Multiple Drive files have the same name"
      
      warning("Duplicate Drive filenames: ", file_name)
      
    } else {
      
      needs_upload <- TRUE
      
      if (nrow(existing_match) == 1) {
        
        needs_upload <- file_needs_update(
          local_file = f,
          drive_row = existing_match,
          compare_mode = compare_mode
        )
      }
      
      if (!needs_upload) {
        
        status <- "skipped_unchanged"
        details <- "Existing file considered current"
        message("  Skipped (unchanged)")
        
      } else {
        
        # New files are uploaded.
        # Existing files are updated directly by ID.
        # This avoids repeated name searches and duplicate creation.
        
        result <- tryCatch(
          
          drive_retry(
            fun = function() {
              
              if (nrow(existing_match) == 1) {
                
                googledrive::drive_update(
                  file = googledrive::as_id(existing_match$id[1]),
                  media = f
                )
                
              } else {
                
                googledrive::drive_upload(
                  media = f,
                  path = googledrive::as_id(drive_folder_id),
                  name = file_name
                )
              }
            },
            label = paste("Upload", file_name)
          ),
          
          error = function(e) e
        )
        
        if (inherits(result, "error")) {
          
          status <- "failed"
          details <- conditionMessage(result)
          
          warning(
            "Upload failed: ", file_name,
            " | ", details
          )
          
        } else {
          
          status <- if (nrow(existing_match) == 1) {
            "updated"
          } else {
            "uploaded"
          }
          
          details <- "Success"
          
          message("  Success: ", status)
          
          # Update inventory for this file.
          # Useful if local filenames are repeated.
          new_row <- data.frame(
            name = file_name,
            id = result$id[1],
            size = as.numeric(file.info(f)$size),
            modified_time = as.POSIXct(
              file.info(f)$mtime,
              tz = "UTC"
            )
          )
          
          drive_meta <- rbind(drive_meta, new_row)
          
          # Remove stale metadata for same ID.
          drive_meta <- drive_meta[
            !duplicated(drive_meta$id, fromLast = TRUE),
            ,
            drop = FALSE
          ]
        }
        
        # Pause between API write operations.
        Sys.sleep(upload_delay)
      }
    }
    
    results[[i]] <- data.frame(
      folder = folder_name,
      file = file_name,
      status = status,
      details = details,
      stringsAsFactors = FALSE
    )
  }
  
  results <- do.call(rbind, results)
  
  message("\nFolder summary: ", folder_name)
  print(table(results$status))
  
  return(results)
}

#===============================================================================
# 8. DEFINE ALL EXPORT FOLDERS
#===============================================================================

# A single configuration table avoids duplicate upload calls.

export_folders <- data.frame(
  folder = c(
    "miscellaneous",
    "base_outcomes",
    "n_choice_occasions",
    "calib_catch_draws",
    #"calibration_checkpoints",
    "proj_catch_draws",
    "figures"
  ),
  local_folder = c(
    final_process_misc_cd,
    final_process_outcomes_cd,
    final_process_choice_occasions_cd,
    final_process_calib_catch_cd,
    #final_process_calib_checkpoints_cd,
    final_process_proj_catch_draws_cd,
    final_process_figures_cd
  ),
  drive_folder_id = unname(drive_folder_ids[c(
    "miscellaneous",
    "base_outcomes",
    "n_choice_occasions",
    "calib_catch_draws",
    #"calibration_checkpoints",
    "proj_catch_draws",
    "figures"
  )]),
  pattern = c(
    "\\.(fst|csv|xlsx|dta|rds)$",
    "\\.(fst)$",
    "\\.(fst)$",
    "\\.(fst)$",
    #"\\.(rds)$",
    "\\.(fst)$",
    "\\.(fst|csv|xlsx|dta|png|jpg|jpeg|eps|pdf)$"),
  stringsAsFactors = FALSE
)


# Check which files match the upload patterns
for (i in seq_len(nrow(export_folders))) {
  
  config <- export_folders[i, ]
  
  files <- list.files(
    path = config$local_folder,
    pattern = config$pattern,
    full.names = FALSE,
    ignore.case = TRUE
  )
  
  cat("\nFolder:", config$folder,
      "\nMatching files:", length(files),
      "\nExtensions:", paste(unique(tools::file_ext(files)), collapse = ", "),
      "\n")
}

#===============================================================================
# 9. RUN EXPORTS
#===============================================================================

message("\nStarting Google Drive synchronization...")
start_time <- Sys.time()

upload_results <- vector("list", nrow(export_folders))

for (i in seq_len(nrow(export_folders))) {
  
  config <- export_folders[i, ]
  
  upload_results[[i]] <- tryCatch(
    
    upload_folder_to_drive(
      local_folder = config$local_folder,
      drive_folder_id = config$drive_folder_id,
      pattern = config$pattern,
      compare_mode = compare_mode
    ),
    
    error = function(e) {
      
      warning(
        "Folder failed: ", config$folder,
        " | ", conditionMessage(e)
      )
      
      data.frame(
        folder = config$folder,
        file = NA_character_,
        status = "folder_failed",
        details = conditionMessage(e)
      )
    }
  )
}

upload_results <- do.call(rbind, upload_results)

#===============================================================================
# 10. VERIFY LOCAL VS GOOGLE DRIVE FILE INVENTORY
#===============================================================================

message("\nVerifying Google Drive file inventory...")

verification_results <- vector("list", nrow(export_folders))

for (i in seq_len(nrow(export_folders))) {
  
  config <- export_folders[i, ]
  
  if (!dir.exists(config$local_folder)) {
    verification_results[[i]] <- data.frame(
      folder = config$folder,
      local_files = NA_integer_,
      drive_files = NA_integer_,
      missing_files = NA_integer_,
      missing_names = "Local folder does not exist",
      verification_status = "failed"
    )
    next
  }
  
  local_files <- list.files(
    config$local_folder,
    pattern = config$pattern,
    full.names = FALSE,
    recursive = FALSE,
    ignore.case = TRUE
  )
  
  # Verify each folder using a fresh Drive listing.
  drive_files <- tryCatch(
    
    drive_retry(
      fun = function() {
        googledrive::drive_ls(
          path = googledrive::as_id(config$drive_folder_id)
        )
      },
      label = paste("Verify", config$folder)
    ),
    
    error = function(e) e
  )
  
  if (inherits(drive_files, "error")) {
    
    verification_results[[i]] <- data.frame(
      folder = config$folder,
      local_files = length(local_files),
      drive_files = NA_integer_,
      missing_files = NA_integer_,
      missing_names = conditionMessage(drive_files),
      verification_status = "failed"
    )
    
    next
  }
  
  missing <- setdiff(local_files, drive_files$name)
  
  verification_results[[i]] <- data.frame(
    folder = config$folder,
    local_files = length(local_files),
    drive_files = nrow(drive_files),
    missing_files = length(missing),
    missing_names = paste(missing, collapse = "; "),
    verification_status = "completed"
  )
}

verification_results <- do.call(rbind, verification_results)

#===============================================================================
# 11. FINAL REPORT
#===============================================================================

end_time <- Sys.time()
elapsed_minutes <- as.numeric(
  difftime(end_time, start_time, units = "mins")
)

message("\n====================================================")
message("GOOGLE DRIVE EXPORT SUMMARY")
message("====================================================")

print(table(upload_results$status))

message("\nInventory verification:")
print(
  verification_results[
    ,
    c(
      "folder",
      "local_files",
      "drive_files",
      "missing_files",
      "verification_status"
    )
  ],
  row.names = FALSE
)

failed_uploads <- upload_results[
  upload_results$status %in% c(
    "failed",
    "folder_failed",
    "duplicate_drive_names",
    "missing_local_folder"
  ),
  ,
  drop = FALSE
]

if (nrow(failed_uploads) > 0) {
  
  message("\nFiles/folders requiring attention:")
  print(failed_uploads, row.names = FALSE)
  
} else {
  
  message("\nNo upload failures recorded.")
}

message(
  "\nTotal elapsed time: ",
  round(elapsed_minutes, 2),
  " minutes"
)

# Save reports locally for later inspection.
# report_dir <- file.path(final_process_data_cd, "miscellaneous")
# dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)

# utils::write.csv(
#   upload_results,
#   file.path(report_dir, "gdrive_upload_report.csv"),
#   row.names = FALSE
# )
# 
# utils::write.csv(
#   verification_results,
#   file.path(report_dir, "gdrive_verification_report.csv"),
#   row.names = FALSE
# )

# message("\nReports saved to: ", report_dir)
# 
# if (
#   nrow(failed_uploads) > 0 ||
#   any(verification_results$verification_status != "completed") ||
#   any(verification_results$missing_files > 0, na.rm = TRUE)
# ) {
#   warning("Google Drive synchronization completed with issues. Check reports.")
# } else {
#   message("Google Drive synchronization completed successfully.")
# }




#===============================================================================
# 12. RECONCILE EXPECTED SIMULATION DRAWS VS GOOGLE DRIVE
#===============================================================================

library(data.table)

# Folders where we expect 101 draws for each state or state-mode group.
draw_folders <- c(
  "base_outcomes",
  "calib_catch_draws",
  "n_choice_occasions",
  "proj_catch_draws"
)

draws <- seq_len(n_simulations)

# Expected filename format:
#   prefix_1.fst, prefix_2.fst, ..., prefix_101.fst
#
# Example:
#   proj_catch_draws_MA_1.fst
#   proj_catch_draws_MA_2.fst
#
# Detect prefixes from local filenames, then construct all 101 draws.
# This assumes the draw number is the final numeric component before .fst.

get_expected_draws <- function(local_folder, folder_label, draws) {
  
  local_files <- list.files(
    local_folder,
    pattern = "\\.fst$",
    full.names = FALSE,
    ignore.case = TRUE
  )
  
  # Identify filenames ending in _DRAW.fst
  valid_files <- grep("_[0-9]+\\.fst$", local_files, value = TRUE)
  
  if (length(valid_files) == 0) {
    warning("No draw-pattern files detected in: ", folder_label)
    return(data.table(
      folder = character(),
      file_name = character(),
      prefix = character(),
      draw = integer()
    ))
  }
  
  # Extract the filename prefix and draw number.
  observed <- data.table(
    file_name = valid_files
  )
  
  observed[, `:=`(
    prefix = sub("_[0-9]+\\.fst$", "", file_name),
    draw = as.integer(sub("^.*_([0-9]+)\\.fst$", "\\1", file_name))
  )]
  
  # Keep only valid simulation draws when identifying prefixes.
  prefixes <- unique(observed[
    draw %in% draws,
    prefix
  ])
  
  expected <- data.table::CJ(
    prefix = prefixes,
    draw = draws
  )
  
  expected[, `:=`(
    folder = folder_label,
    file_name = paste0(prefix, "_", draw, ".fst")
  )]
  
  expected[, .(folder, file_name, prefix, draw)]
}

#===============================================================================
# 12A. GENERATE EXPECTED FILE INVENTORY
#===============================================================================

expected_files <- data.table::rbindlist(
  lapply(draw_folders, function(folder_name) {
    
    config <- export_folders[
      export_folders$folder == folder_name,
      ,
      drop = FALSE
    ]
    
    get_expected_draws(
      local_folder = config$local_folder[1],
      folder_label = folder_name,
      draws = draws
    )
  }),
  use.names = TRUE
)

#===============================================================================
# 12B. COLLECT LOCAL AND GOOGLE DRIVE INVENTORIES
#===============================================================================

local_inventory <- list()
drive_inventory <- list()
listing_errors <- character()

for (folder_name in draw_folders) {
  
  config <- export_folders[
    export_folders$folder == folder_name,
    ,
    drop = FALSE
  ]
  
  message("Checking inventory: ", folder_name)
  
  # Local inventory
  local_paths <- list.files(
    config$local_folder[1],
    pattern = "\\.fst$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  local_info <- file.info(local_paths)
  
  local_inventory[[folder_name]] <- data.table(
    folder = folder_name,
    file_name = basename(local_paths),
    local_size = as.numeric(local_info$size)
  )
  
  # Google Drive inventory
  drive_files <- tryCatch(
    drive_retry(
      fun = function() googledrive::drive_ls(
        path = googledrive::as_id(config$drive_folder_id[1])
      ),
      label = paste("Reconcile", folder_name)
    ),
    error = function(e) e
  )
  
  if (inherits(drive_files, "error")) {
    listing_errors <- c(
      listing_errors,
      paste(folder_name, conditionMessage(drive_files))
    )
    next
  }
  
  drive_meta <- get_drive_metadata(drive_files)
  
  drive_inventory[[folder_name]] <- data.table(
    folder = folder_name,
    file_name = drive_meta$name,
    drive_id = drive_meta$id,
    drive_size = drive_meta$size
  )
}

local_inventory <- data.table::rbindlist(local_inventory, fill = TRUE)
drive_inventory <- data.table::rbindlist(drive_inventory, fill = TRUE)

# Do not report success if a Drive listing failed.
if (length(listing_errors) > 0) {
  stop("Could not complete reconciliation:\n",
       paste(listing_errors, collapse = "\n"))
}

#===============================================================================
# 12C. IDENTIFY MISSING FILES
#===============================================================================

# Expected files missing locally
missing_local <- expected_files[
  !local_inventory,
  on = .(folder, file_name)
]

# Expected files missing from Google Drive
missing_drive <- expected_files[
  !drive_inventory,
  on = .(folder, file_name)
]

# Local files missing from Google Drive, including files not in the
# expected-draw inventory.
missing_uploads <- local_inventory[
  !drive_inventory,
  on = .(folder, file_name)
]

#===============================================================================
# 12D. IDENTIFY SIZE MISMATCHES AND EMPTY FILES
#===============================================================================

comparison <- merge(
  local_inventory,
  drive_inventory,
  by = c("folder", "file_name"),
  all = FALSE,
  allow.cartesian = TRUE
)

size_mismatches <- comparison[
  is.na(local_size) | is.na(drive_size) |
    local_size != drive_size
]

empty_files <- comparison[
  (!is.na(local_size) & local_size == 0) |
    (!is.na(drive_size) & drive_size == 0)
]

# Identify duplicate filenames on Drive
duplicate_drive_files <- drive_inventory[
  ,
  .N,
  by = .(folder, file_name)
][N > 1]

#===============================================================================
# 12E. PRINT SUMMARY
#===============================================================================

cat("\n====================================================\n")
cat("SIMULATION DRAW RECONCILIATION\n")
cat("====================================================\n")

summary <- data.table(folder = draw_folders)

summary[, expected := expected_files[, .N, by = folder][
  match(folder, expected_files[, unique(folder)]), N
]]

# Use explicit matching for reliable folder counts.
count_by_folder <- function(dt, folders) {
  counts <- dt[, .N, by = folder]
  result <- counts$N[match(folders, counts$folder)]
  result[is.na(result)] <- 0L
  result
}

summary[, `:=`(
  expected = count_by_folder(expected_files, draw_folders),
  local = count_by_folder(local_inventory, draw_folders),
  drive = count_by_folder(drive_inventory, draw_folders),
  missing_local = count_by_folder(missing_local, draw_folders),
  missing_drive = count_by_folder(missing_drive, draw_folders),
  size_mismatch = count_by_folder(size_mismatches, draw_folders),
  empty = count_by_folder(empty_files, draw_folders)
)]

print(summary)

cat("\nExpected files missing locally:", nrow(missing_local), "\n")
cat("Expected files missing on Drive:", nrow(missing_drive), "\n")
cat("Local files not uploaded:", nrow(missing_uploads), "\n")
cat("Size mismatches:", nrow(size_mismatches), "\n")
cat("Empty files:", nrow(empty_files), "\n")
cat("Duplicate Drive filenames:", nrow(duplicate_drive_files), "\n")

if (nrow(missing_drive) > 0) {
  cat("\nExpected files missing on Google Drive:\n")
  print(missing_drive[order(folder, prefix, draw)])
}

if (nrow(size_mismatches) > 0) {
  cat("\nFiles with different local and Drive sizes:\n")
  print(size_mismatches[order(folder, file_name)])
}

if (nrow(duplicate_drive_files) > 0) {
  cat("\nDuplicate filenames on Google Drive:\n")
  print(duplicate_drive_files[order(folder, file_name)])
}


#===============================================================================
# 13. RETRY MISSING GOOGLE DRIVE FILES
#===============================================================================

# Save the missing file list before attempting uploads.
missing_files_retry <- data.table::copy(missing_drive)

# Add local file paths and Google Drive folder IDs.
missing_files_retry <- merge(
  missing_files_retry,
  data.table::as.data.table(export_folders)[
    , .(folder, local_folder, drive_folder_id)
  ],
  by = "folder",
  all.x = TRUE,
  sort = FALSE
)

missing_files_retry[
  , local_path := file.path(local_folder, file_name)
]

# Check that all missing files exist locally.
missing_files_retry[
  , local_exists := file.exists(local_path)
]

if (any(!missing_files_retry$local_exists)) {
  warning("Some missing Drive files do not exist locally.")
  print(missing_files_retry[local_exists == FALSE])
}

# Retry uploads.
retry_results <- vector("list", nrow(missing_files_retry))

for (i in seq_len(nrow(missing_files_retry))) {
  
  f <- missing_files_retry[i]
  
  message(
    "[Retry ", i, "/", nrow(missing_files_retry), "] ",
    f$file_name
  )
  
  if (!f$local_exists) {
    retry_results[[i]] <- data.table::data.table(
      folder = f$folder,
      file_name = f$file_name,
      status = "missing_local",
      details = "Local file does not exist"
    )
    next
  }
  
  result <- tryCatch(
    drive_retry(
      fun = function() {
        googledrive::drive_upload(
          media = f$local_path,
          path = googledrive::as_id(f$drive_folder_id),
          name = f$file_name
        )
      },
      label = paste("Retry upload", f$file_name)
    ),
    error = function(e) e
  )
  
  if (inherits(result, "error")) {
    
    status <- "failed"
    details <- conditionMessage(result)
    
    warning("Retry failed: ", f$file_name, " | ", details)
    
  } else {
    
    status <- "uploaded"
    details <- "Success"
    
    message("  Uploaded successfully.")
  }
  
  retry_results[[i]] <- data.table::data.table(
    folder = f$folder,
    file_name = f$file_name,
    status = status,
    details = details
  )
  
  Sys.sleep(upload_delay)
}

retry_results <- data.table::rbindlist(retry_results, fill = TRUE)

cat("\nRetry upload results:\n")
print(retry_results)

#===============================================================================
# 13B. VERIFY RETRIED FILES
#===============================================================================

# Check each affected Drive folder once.
retry_verification <- list()

for (folder_name in unique(missing_files_retry$folder)) {
  
  folder_id <- export_folders$drive_folder_id[
    export_folders$folder == folder_name
  ]
  
  drive_files <- tryCatch(
    drive_retry(
      fun = function() googledrive::drive_ls(
        path = googledrive::as_id(folder_id)
      ),
      label = paste("Verify retry", folder_name)
    ),
    error = function(e) e
  )
  
  if (inherits(drive_files, "error")) {
    warning("Could not verify folder: ", folder_name)
    next
  }
  
  retry_verification[[folder_name]] <- data.table::data.table(
    folder = folder_name,
    file_name = drive_files$name,
    drive_id = drive_files$id
  )
}

retry_verification <- data.table::rbindlist(
  retry_verification,
  fill = TRUE
)

# Identify any files still missing after retry.
still_missing <- missing_files_retry[
  !retry_verification,
  on = .(folder, file_name)
]

cat("\n====================================================\n")
cat("RETRY VERIFICATION RESULTS\n")
cat("====================================================\n")

if (nrow(still_missing) == 0) {
  message("All retried files are now present on Google Drive.")
} else {
  warning(nrow(still_missing), " files still missing from Google Drive.")
  print(still_missing[, .(folder, file_name, local_path)])
}

# Save retry results.
# data.table::fwrite(
#   retry_results,
#   file.path(report_dir, "gdrive_retry_results.csv")
# )
# 
# data.table::fwrite(
#   still_missing,
#   file.path(report_dir, "gdrive_still_missing.csv")
# )


################################################################################
# END OF SCRIPT
################################################################################
