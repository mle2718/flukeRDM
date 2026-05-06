
# iterative calibration routine for fluke / black sea bass / scup
# bounded search with best-so-far selection; no catch-hold adjustment.

library(data.table)
library(arrow)
library(haven)
library(readr)
library(fst)


if (!exists("MRIP_comparison", inherits = FALSE)) {
  MRIP_comparison <- read_dta("E:/Lou_projects/flukeRDM/flukeRDM_iterative_data/archive/calib_catch_draws/simulated_catch_totals.dta") |>
    as.data.table()
}

setnames(
  MRIP_comparison,
  old = c("tot_dtrip_sim", "tot_sf_cat_sim", "tot_bsb_cat_sim", "tot_scup_cat_sim",
          "tot_sf_keep_sim", "tot_bsb_keep_sim", "tot_scup_keep_sim",
          "tot_sf_rel_sim", "tot_bsb_rel_sim", "tot_scup_rel_sim"),
  new = c("estimated_trips", "sf_catch", "bsb_catch", "scup_catch",
          "sf_keep", "bsb_keep", "scup_keep",
          "sf_rel", "bsb_rel", "scup_rel"),
  skip_absent = TRUE
)

baseline_output0 <- as.data.table(fst::read_fst(
  file.path(iterative_input_data_cd,
            paste0("archive/miscellaneous/calibration_comparison.fst"))))

# Reconstruct catch columns defensively if the step-0 file omitted them
if (!("MRIP_catch" %in% names(baseline_output0)) && all(c("MRIP_keep", "MRIP_rel") %in% names(baseline_output0))) {
  baseline_output0[, MRIP_catch := MRIP_keep + MRIP_rel]
}
if (!("model_catch" %in% names(baseline_output0)) && all(c("model_keep", "model_rel") %in% names(baseline_output0))) {
  baseline_output0[, model_catch := model_keep + model_rel]
}
if (!("diff_catch" %in% names(baseline_output0)) && all(c("model_catch", "MRIP_catch") %in% names(baseline_output0))) {
  baseline_output0[, diff_catch := model_catch - MRIP_catch]
}
if (!("pct_diff_catch" %in% names(baseline_output0)) && all(c("diff_catch", "MRIP_catch") %in% names(baseline_output0))) {
  baseline_output0[, pct_diff_catch := fifelse(MRIP_catch != 0, 100 * diff_catch / MRIP_catch, NA_real_)]
}

states <- c("MA", "RI", "CT", "NY", "NJ", "DE", "MD", "VA", "NC")
states <- c("MA", "RI")

mode_draw <- c("sh", "pr", "fh")
draws <- 1:3

# states <- c("MA")
# mode_draw <- c("pr")
# draws <- 1:1

tol_abs_fish <- 500
tol_abs_pct  <- 5
max_iter     <- 25
p_tol        <- 1e-4

species_vec <- c("sf", "bsb", "scup")

is_achieved <- function(diff_keep, pct_diff_keep, MRIP_keep = NA_real_) {
  if (is.finite(MRIP_keep) && MRIP_keep == 0) {
    return(is.finite(diff_keep) && abs(diff_keep) < tol_abs_fish)
  }
  
  (is.finite(diff_keep) && abs(diff_keep) < tol_abs_fish) ||
    (is.finite(pct_diff_keep) && abs(pct_diff_keep) < tol_abs_pct)
}

score_species <- function(diff_keep, pct_diff_keep, diff_catch, pct_diff_catch,
                          MRIP_keep = NA_real_, MRIP_catch = NA_real_) {
  
  keep_score <- if (is.finite(MRIP_keep) && MRIP_keep == 0) {
    if (is.finite(diff_keep)) abs(diff_keep) / tol_abs_fish else Inf
  } else {
    min(
      if (is.finite(diff_keep)) abs(diff_keep) / tol_abs_fish else Inf,
      if (is.finite(pct_diff_keep)) abs(pct_diff_keep) / tol_abs_pct else Inf
    )
  }
  
  catch_score <- if (is.finite(MRIP_catch) && MRIP_catch == 0) {
    if (is.finite(diff_catch)) abs(diff_catch) / (5 * tol_abs_fish) else Inf
  } else {
    min(
      if (is.finite(diff_catch)) abs(diff_catch) / (5 * tol_abs_fish) else Inf,
      if (is.finite(pct_diff_catch)) abs(pct_diff_catch) / (4 * tol_abs_pct) else Inf
    )
  }
  
  keep_score + 0.15 * catch_score
}

extract_species_row <- function(dt, sp, md, s, i) {
  out <- as.data.table(dt)[species == sp]
  if (nrow(out) == 0L) {
    out <- data.table(
      mode = md, species = sp,
      MRIP_keep = NA_real_, model_keep = 0, diff_keep = NA_real_, pct_diff_keep = NA_real_,
      MRIP_rel = NA_real_, model_rel = 0, diff_rel = NA_real_, pct_diff_rel = NA_real_,
      MRIP_catch = NA_real_, model_catch = 0, diff_catch = NA_real_, pct_diff_catch = NA_real_,
      rel_to_keep_new = 0, keep_to_rel_new = 0, p_rel_to_keep_new = 0, p_keep_to_rel_new = 0,
      draw = i, state = s
    )
  }
  out[1]
}

make_state <- function(base_row) {
  
  mrip_keep  <- as.numeric(base_row$MRIP_keep)
  model_keep <- as.numeric(base_row$model_keep)
  diff_keep  <- as.numeric(base_row$diff_keep)
  
  # zero-target case
  if (is.finite(mrip_keep) && mrip_keep == 0) {
    
    # if model is also effectively zero, nothing to do
    if (is.finite(model_keep) && model_keep == 0) {
      return(list(
        direction = "none",
        p = 0,
        lo = 0,
        hi = NA_real_,
        achieved = TRUE,
        convergence = 1L,
        best_score = Inf,
        best_row = NULL
      ))
    }
    
    # if model_keep > 0 and MRIP_keep == 0, only keep->rel makes sense
    direction <- "keep_to_rel"
    p0 <- ifelse(is.finite(base_row$p_keep_to_rel), as.numeric(base_row$p_keep_to_rel), 0)
    p0 <- max(0, min(1, p0))
    
    return(list(
      direction = direction,
      p = p0,
      lo = 0,
      hi = 1,
      achieved = FALSE,
      convergence = 1L,
      best_score = Inf,
      best_row = NULL
    ))
  }
  
  direction <- if (isTRUE(base_row$rel_to_keep == 1)) {
    "rel_to_keep"
  } else if (isTRUE(base_row$keep_to_rel == 1)) {
    "keep_to_rel"
  } else {
    "none"
  }
  
  p0 <- if (direction == "rel_to_keep") {
    base_row$p_rel_to_keep
  } else if (direction == "keep_to_rel") {
    base_row$p_keep_to_rel
  } else {
    0
  }
  
  p0 <- max(0, min(1, as.numeric(p0)))
  
  list(
    direction = direction,
    p = p0,
    lo = 0,
    hi = if (p0 > 0 && p0 < 1) 1 else NA_real_,
    achieved = FALSE,
    convergence = 1L,
    best_score = Inf,
    best_row = NULL
  )
}

push_globals <- function(states_by_sp, target_env = .GlobalEnv) {
  for (sp in species_vec) {
    st <- states_by_sp[[sp]]
    assign(paste0("rel_to_keep_", sp),
           as.integer(st$direction == "rel_to_keep"),
           envir = target_env)
    assign(paste0("keep_to_rel_", sp),
           as.integer(st$direction == "keep_to_rel"),
           envir = target_env)
    assign(paste0("p_rel_to_keep_", sp),
           if (st$direction == "rel_to_keep") st$p else 0,
           envir = target_env)
    assign(paste0("p_keep_to_rel_", sp),
           if (st$direction == "keep_to_rel") st$p else 0,
           envir = target_env)
    assign(paste0("all_keep_to_rel_", sp),
           as.integer(st$direction == "keep_to_rel" && st$p >= 1 - p_tol),
           envir = target_env)
  }
}

update_bracket <- function(st, row) {
  if (st$direction == "none") {
    st$achieved <- TRUE
    return(st)
  }
  
  diff_keep     <- as.numeric(row$diff_keep)
  pct_diff_keep <- as.numeric(row$pct_diff_keep)
  diff_catch    <- as.numeric(row$diff_catch)
  pct_diff_catch<- as.numeric(row$pct_diff_catch)
  MRIP_keep     <- as.numeric(row$MRIP_keep)
  MRIP_catch    <- as.numeric(row$MRIP_catch)
  
  st$achieved <- is_achieved(diff_keep, pct_diff_keep, MRIP_keep)
  
  this_score <- score_species(
    diff_keep      = diff_keep,
    pct_diff_keep  = pct_diff_keep,
    diff_catch     = diff_catch,
    pct_diff_catch = pct_diff_catch,
    MRIP_keep      = MRIP_keep,
    MRIP_catch     = MRIP_catch
  )
  
  if (st$achieved) {
    st$best_row <- copy(row)
    st$best_score <- -Inf
    return(st)
  }
  
  if (this_score < st$best_score) {
    st$best_score <- this_score
    st$best_row <- copy(row)
  }
  
  if (st$direction == "rel_to_keep") {
    # larger p => more keep
    if (is.finite(diff_keep) && diff_keep < 0) {
      st$lo <- max(st$lo, st$p)
    } else if (is.finite(diff_keep) && diff_keep > 0) {
      st$hi <- if (is.na(st$hi)) st$p else min(st$hi, st$p)
    }
  } else if (st$direction == "keep_to_rel") {
    # larger p => fewer keep
    if (is.finite(diff_keep) && diff_keep > 0) {
      st$lo <- max(st$lo, st$p)
    } else if (is.finite(diff_keep) && diff_keep < 0) {
      st$hi <- if (is.na(st$hi)) st$p else min(st$hi, st$p)
    }
  }
  
  old_p <- st$p
  
  if (!is.na(st$hi)) {
    st$p <- (st$lo + st$hi) / 2
  } else {
    st$p <- if (old_p == 0) 0.1 else min(1, max(old_p * 1.5, old_p + 0.05))
  }
  
  st$p <- max(0, min(1, st$p))
  
  if (abs(st$p - old_p) < p_tol && !st$achieved) {
    st$convergence <- 0L
  }
  
  if (st$p >= 1 - p_tol && is.na(st$hi) && !st$achieved) {
    st$convergence <- 0L
  }
  
  st
}
  

calibrated <- vector("list", length(states) * length(mode_draw) * length(draws))
k <- 1L

for (s in states) {
  for (md in mode_draw) {
    for (i in draws) {

      baseline_targets_current <- baseline_output0[state == s & draw == i & mode == md]
      if (nrow(baseline_targets_current) == 0L) next

      if (all(is.na(baseline_targets_current$MRIP_keep))) {
        out <- copy(baseline_targets_current)
        out[, `:=`(
          keep_to_rel_sf = 0, rel_to_keep_sf = 0, p_rel_to_keep_sf = 0, p_keep_to_rel_sf = 0, convergence_sf = NA_real_,
          keep_to_rel_bsb = 0, rel_to_keep_bsb = 0, p_rel_to_keep_bsb = 0, p_keep_to_rel_bsb = 0, convergence_bsb = NA_real_,
          keep_to_rel_scup = 0, rel_to_keep_scup = 0, p_rel_to_keep_scup = 0, p_keep_to_rel_scup = 0, convergence_scup = NA_real_,
          iter_used = 0L
        )]
        calibrated[[k]] <- out
        k <- k + 1L
        next
      }

      states_by_sp <- setNames(vector("list", length(species_vec)), species_vec)
      for (sp in species_vec) {
        base_row <- extract_species_row(baseline_targets_current, sp, md, s, i)
        states_by_sp[[sp]] <- make_state(base_row)
        states_by_sp[[sp]]$best_row <- copy(base_row)
        states_by_sp[[sp]]$best_score <- score_species(
          base_row$diff_keep,
          base_row$pct_diff_keep,
          base_row$diff_catch,
          base_row$pct_diff_catch,
          base_row$MRIP_keep,
          base_row$MRIP_catch
        )
        
        states_by_sp[[sp]]$achieved <- is_achieved(
          base_row$diff_keep,
          base_row$pct_diff_keep,
          base_row$MRIP_keep
        )
      }

      iter_used <- 0L
      last_result <- NULL

      sf_floor_below_min_in   <- 3
      bsb_floor_below_min_in  <- 3
      scup_floor_below_min_in <- 3
      
      repeat {
        

        push_globals(states_by_sp, target_env = environment())
        source(file.path(code_cd, "calibrate_rec_catch1_final.R"), local = environment())
        
        last_result <- copy(as.data.table(calib_comparison1))

        all_done <- TRUE
        for (sp in species_vec) {
          row <- extract_species_row(last_result, sp, md, s, i)
          states_by_sp[[sp]] <- update_bracket(states_by_sp[[sp]], row)

          if (!states_by_sp[[sp]]$achieved && states_by_sp[[sp]]$convergence == 1L) {
            all_done <- FALSE
          }
        }

        iter_used <- iter_used + 1L

        if (all_done || iter_used >= max_iter) break
      }


      floor_used_in_sf   <- sf_floor_below_min_in
      floor_used_in_bsb  <- bsb_floor_below_min_in
      floor_used_in_scup <- scup_floor_below_min_in
      
      
      final_rows <- rbindlist(lapply(species_vec, function(sp) {
        st <- states_by_sp[[sp]]
        row <- if (!is.null(st$best_row)) copy(st$best_row) else extract_species_row(last_result, sp, md, s, i)
        row
      }), use.names = TRUE, fill = TRUE)

      final_rows[, `:=`(
        n_sub_kept_sf      = n_sub_kept_sf,
        prop_sub_kept_sf   = prop_sub_kept_sf,
        n_legal_rel_sf     = n_legal_rel_sf,
        prop_legal_rel_sf  = prop_legal_rel_sf,
        
        n_sub_kept_bsb     = n_sub_kept_bsb,
        prop_sub_kept_bsb  = prop_sub_kept_bsb,
        n_legal_rel_bsb    = n_legal_rel_bsb,
        prop_legal_rel_bsb = prop_legal_rel_bsb,
        
        n_sub_kept_scup     = n_sub_kept_scup,
        prop_sub_kept_scup  = prop_sub_kept_scup,
        n_legal_rel_scup    = n_legal_rel_scup,
        prop_legal_rel_scup = prop_legal_rel_scup, 
        
        floor_used_in_sf   = floor_used_in_sf,
        floor_used_in_bsb  = floor_used_in_bsb,
        floor_used_in_scup = floor_used_in_scup
      )]
      
      # set final convergence based on best-so-far row, not just the last attempted row
      final_rows[species == "sf", `:=`(
        keep_to_rel_sf = as.integer(states_by_sp[["sf"]]$direction == "keep_to_rel"),
        rel_to_keep_sf = as.integer(states_by_sp[["sf"]]$direction == "rel_to_keep"),
        p_rel_to_keep_sf = if (states_by_sp[["sf"]]$direction == "rel_to_keep") states_by_sp[["sf"]]$p else 0,
        p_keep_to_rel_sf = if (states_by_sp[["sf"]]$direction == "keep_to_rel") states_by_sp[["sf"]]$p else 0,
        convergence_sf = as.integer(is_achieved(diff_keep, pct_diff_keep, MRIP_keep))
        )]
      
      final_rows[species == "bsb", `:=`(
        keep_to_rel_bsb = as.integer(states_by_sp[["bsb"]]$direction == "keep_to_rel"),
        rel_to_keep_bsb = as.integer(states_by_sp[["bsb"]]$direction == "rel_to_keep"),
        p_rel_to_keep_bsb = if (states_by_sp[["bsb"]]$direction == "rel_to_keep") states_by_sp[["bsb"]]$p else 0,
        p_keep_to_rel_bsb = if (states_by_sp[["bsb"]]$direction == "keep_to_rel") states_by_sp[["bsb"]]$p else 0,
        convergence_bsb = as.integer(is_achieved(diff_keep, pct_diff_keep, MRIP_keep))
        )]
      
      final_rows[species == "scup", `:=`(
        keep_to_rel_scup = as.integer(states_by_sp[["scup"]]$direction == "keep_to_rel"),
        rel_to_keep_scup = as.integer(states_by_sp[["scup"]]$direction == "rel_to_keep"),
        p_rel_to_keep_scup = if (states_by_sp[["scup"]]$direction == "rel_to_keep") states_by_sp[["scup"]]$p else 0,
        p_keep_to_rel_scup = if (states_by_sp[["scup"]]$direction == "keep_to_rel") states_by_sp[["scup"]]$p else 0,
        convergence_scup = as.integer(is_achieved(diff_keep, pct_diff_keep, MRIP_keep))
        )]

      # fill non-target species columns with zeros where still missing
      fill_zero_cols <- c(
        "keep_to_rel_sf","rel_to_keep_sf","p_rel_to_keep_sf","p_keep_to_rel_sf","convergence_sf",
        "keep_to_rel_bsb","rel_to_keep_bsb","p_rel_to_keep_bsb","p_keep_to_rel_bsb","convergence_bsb",
        "keep_to_rel_scup","rel_to_keep_scup","p_rel_to_keep_scup","p_keep_to_rel_scup","convergence_scup",
        "n_sub_kept_scup","prop_sub_kept_scup","n_legal_rel_scup","prop_legal_rel_scup",
        "n_sub_kept_sf","n_legal_rel_sf","prop_sub_kept_sf","prop_legal_rel_sf",
        "n_sub_kept_bsb","n_legal_rel_bsb","prop_sub_kept_bsb","prop_legal_rel_bsb", 
        "floor_used_in_sf", "floor_used_in_bsb", "floor_used_in_scup")
      
      for (cc in intersect(fill_zero_cols, names(final_rows))) {
        set(final_rows, which(is.na(final_rows[[cc]])), cc, 0)
      }

      final_rows[, iter_used := iter_used]
      setcolorder(final_rows, c("draw","state", "mode","species","MRIP_rel","model_rel","diff_rel","pct_diff_rel",
                                "MRIP_keep","model_keep","diff_keep","pct_diff_keep",
                                "MRIP_catch","model_catch","diff_catch","pct_diff_catch",
                                "rel_to_keep_new","keep_to_rel_new","p_rel_to_keep_new","p_keep_to_rel_new",
                                "floor_used_in_sf", "floor_used_in_bsb", "floor_used_in_scup",
                                setdiff(names(final_rows), c("draw","state", "mode","species","MRIP_rel","model_rel","diff_rel","pct_diff_rel",
                                                             "MRIP_keep","model_keep","diff_keep","pct_diff_keep",
                                                             "MRIP_catch","model_catch","diff_catch","pct_diff_catch",
                                                             "rel_to_keep_new","keep_to_rel_new","p_rel_to_keep_new","p_keep_to_rel_new", 
                                                             "floor_used_in_sf", "floor_used_in_bsb", "floor_used_in_scup"))))

      calibrated[[k]] <- final_rows
      k <- k + 1L
    }
  }
}

calibrated_combined <- rbindlist(calibrated, use.names = TRUE, fill = TRUE)

drop_cols <- c(
  "rel_to_keep_new", "keep_to_rel_new",
  "p_rel_to_keep_new", "p_keep_to_rel_new"
)

drop_cols <- intersect(drop_cols, names(calibrated_combined))
calibrated_combined[, (drop_cols) := NULL]

# one row per state-mode-draw is enough, since the final calibration values are wide
calibrated_combined <- unique(calibrated_combined, by = c("state", "mode", "draw", "species"))

front_cols <- c("state", "mode", "draw")
front_cols <- intersect(front_cols, names(calibrated_combined))
data.table::setcolorder(calibrated_combined, c(front_cols, setdiff(names(calibrated_combined), front_cols)))


# identify all species suffixes
species_levels <- c("sf", "bsb", "scup")

# find all columns that have species suffixes
suffix_pattern <- paste0("(", paste(species_levels, collapse = "|"), ")$")
cols <- names(calibrated_combined)

suffix_cols <- cols[grepl(paste0("_", suffix_pattern), cols)]

# get base variable names (remove suffix)
base_names <- unique(sub(paste0("_", suffix_pattern), "", suffix_cols))

# for each base variable, create a collapsed version
for (v in base_names) {
  
  new_col <- v
  
  calibrated_combined[, (new_col) := fifelse(
    species == "sf",  get(paste0(v, "_sf")),
    fifelse(
      species == "bsb", get(paste0(v, "_bsb")),
      fifelse(
        species == "scup", get(paste0(v, "_scup")),
        NA_real_
      )
    )
  )]
}

# drop the wide columns
calibrated_combined[, (suffix_cols) := NULL]

# reorder columns
setcolorder(calibrated_combined, c("state", "mode", "draw", "species", base_names))


# identify non-coverged cells and re-run with expanded floor_sublegal_harvest
library(data.table)

# assume this is your first-pass output in the CURRENT naming format
# one row per state-mode-draw-species
# columns include:
# state, mode, draw, species, floor_used_in,
# keep_to_rel, rel_to_keep, p_rel_to_keep, p_keep_to_rel, convergence,
# MRIP_keep, model_keep, diff_keep, pct_diff_keep, etc.

calibrated_combined <- data.table::as.data.table(calibrated_combined)

# helper for your current long-format output
needs_floor4_rerun <- function(rel_to_keep, convergence, diff_keep, pct_diff_keep, MRIP_keep) {
  # only rerun rel_to_keep cases that still did not converge
  if (!isTRUE(rel_to_keep == 1)) return(FALSE)
  if (!isTRUE(convergence == 0)) return(FALSE)
  
  # zero-MRIP keep case: use abs diff only
  if (is.finite(MRIP_keep) && MRIP_keep == 0) {
    return(is.finite(diff_keep) && abs(diff_keep) >= 500)
  }
  
  # otherwise use your usual tolerance logic
  keep_bad_abs <- is.finite(diff_keep) && abs(diff_keep) >= 500
  keep_bad_pct <- is.finite(pct_diff_keep) && abs(pct_diff_keep) >= 5
  
  keep_bad_abs || keep_bad_pct || !is.finite(pct_diff_keep)
}

problem_rows <- calibrated_combined[
  , needs_rerun := mapply(
    needs_floor4_rerun,
    rel_to_keep,
    convergence,
    diff_keep,
    pct_diff_keep,
    MRIP_keep
  )
][needs_rerun == TRUE]

# optional: inspect what will be rerun
print(problem_rows[, .(
  state, mode, draw, species, floor_used_in,
  rel_to_keep, p_rel_to_keep,
  MRIP_keep, model_keep, diff_keep, pct_diff_keep,
  convergence
)])

rerun_results <- vector("list", nrow(problem_rows))

if (nrow(problem_rows) > 0) {
  for (rr in seq_len(nrow(problem_rows))) {
    
    row_i <- problem_rows[rr]
    
    # map current long-format names into the scalar objects expected by the rerun script
    s  <- row_i$state
    md <- row_i$mode
    i  <- row_i$draw
    target_species <- row_i$species
    
          
          baseline_targets_current <- baseline_output0[state == s & draw == i & mode == md]
          if (nrow(baseline_targets_current) == 0L) next
          
          if (all(is.na(baseline_targets_current$MRIP_keep))) {
            out <- copy(baseline_targets_current)
            out[, `:=`(
              keep_to_rel_sf = 0, rel_to_keep_sf = 0, p_rel_to_keep_sf = 0, p_keep_to_rel_sf = 0, convergence_sf = NA_real_,
              keep_to_rel_bsb = 0, rel_to_keep_bsb = 0, p_rel_to_keep_bsb = 0, p_keep_to_rel_bsb = 0, convergence_bsb = NA_real_,
              keep_to_rel_scup = 0, rel_to_keep_scup = 0, p_rel_to_keep_scup = 0, p_keep_to_rel_scup = 0, convergence_scup = NA_real_,
              iter_used = 0L
            )]
            rerun_results[[rr]] <- out
            rr <- rr + 1L
            next
          }
          
          states_by_sp <- setNames(vector("list", length(species_vec)), species_vec)
          for (sp in species_vec) {
            base_row <- extract_species_row(baseline_targets_current, sp, md, s, i)
            states_by_sp[[sp]] <- make_state(base_row)
            states_by_sp[[sp]]$best_row <- copy(base_row)
            states_by_sp[[sp]]$best_score <- score_species(
              base_row$diff_keep,
              base_row$pct_diff_keep,
              base_row$diff_catch,
              base_row$pct_diff_catch,
              base_row$MRIP_keep,
              base_row$MRIP_catch
            )
            
            states_by_sp[[sp]]$achieved <- is_achieved(
              base_row$diff_keep,
              base_row$pct_diff_keep,
              base_row$MRIP_keep
            )
          }
          
          iter_used <- 0L
          last_result <- NULL
          
          floor_below_min_in <- 4
          sf_floor_below_min_in   <- 3
          bsb_floor_below_min_in  <- 3
          scup_floor_below_min_in <- 3
          
          if (target_species == "sf")   sf_floor_below_min_in   <- floor_below_min_in
          if (target_species == "bsb")  bsb_floor_below_min_in  <- floor_below_min_in
          if (target_species == "scup") scup_floor_below_min_in <- floor_below_min_in
          
          repeat {
            
            
            push_globals(states_by_sp, target_env = environment())
            source(file.path(code_cd, "calibrate_rec_catch1_final.R"), local = environment())
            
            last_result <- copy(as.data.table(calib_comparison1))
            
            all_done <- TRUE
            for (sp in species_vec) {
              row <- extract_species_row(last_result, sp, md, s, i)
              states_by_sp[[sp]] <- update_bracket(states_by_sp[[sp]], row)
              
              if (!states_by_sp[[sp]]$achieved && states_by_sp[[sp]]$convergence == 1L) {
                all_done <- FALSE
              }
            }
            
            iter_used <- iter_used + 1L
            
            if (all_done || iter_used >= max_iter) break
          }
          
   
          floor_used_in_sf   <- sf_floor_below_min_in
          floor_used_in_bsb  <- bsb_floor_below_min_in
          floor_used_in_scup <- scup_floor_below_min_in
          
          
          final_rows <- rbindlist(lapply(species_vec, function(sp) {
            st <- states_by_sp[[sp]]
            row <- if (!is.null(st$best_row)) copy(st$best_row) else extract_species_row(last_result, sp, md, s, i)
            row
          }), use.names = TRUE, fill = TRUE)
          
          final_rows[, `:=`(
            n_sub_kept_sf      = n_sub_kept_sf,
            prop_sub_kept_sf   = prop_sub_kept_sf,
            n_legal_rel_sf     = n_legal_rel_sf,
            prop_legal_rel_sf  = prop_legal_rel_sf,
            
            n_sub_kept_bsb     = n_sub_kept_bsb,
            prop_sub_kept_bsb  = prop_sub_kept_bsb,
            n_legal_rel_bsb    = n_legal_rel_bsb,
            prop_legal_rel_bsb = prop_legal_rel_bsb,
            
            n_sub_kept_scup     = n_sub_kept_scup,
            prop_sub_kept_scup  = prop_sub_kept_scup,
            n_legal_rel_scup    = n_legal_rel_scup,
            prop_legal_rel_scup = prop_legal_rel_scup, 
            
            floor_used_in_sf   = floor_used_in_sf,
            floor_used_in_bsb  = floor_used_in_bsb,
            floor_used_in_scup = floor_used_in_scup
          )]
          
          # set final convergence based on best-so-far row, not just the last attempted row
          final_rows[species == "sf", `:=`(
            keep_to_rel_sf = as.integer(states_by_sp[["sf"]]$direction == "keep_to_rel"),
            rel_to_keep_sf = as.integer(states_by_sp[["sf"]]$direction == "rel_to_keep"),
            p_rel_to_keep_sf = if (states_by_sp[["sf"]]$direction == "rel_to_keep") states_by_sp[["sf"]]$p else 0,
            p_keep_to_rel_sf = if (states_by_sp[["sf"]]$direction == "keep_to_rel") states_by_sp[["sf"]]$p else 0,
            convergence_sf = as.integer(is_achieved(diff_keep, pct_diff_keep, MRIP_keep))
          )]
          
          final_rows[species == "bsb", `:=`(
            keep_to_rel_bsb = as.integer(states_by_sp[["bsb"]]$direction == "keep_to_rel"),
            rel_to_keep_bsb = as.integer(states_by_sp[["bsb"]]$direction == "rel_to_keep"),
            p_rel_to_keep_bsb = if (states_by_sp[["bsb"]]$direction == "rel_to_keep") states_by_sp[["bsb"]]$p else 0,
            p_keep_to_rel_bsb = if (states_by_sp[["bsb"]]$direction == "keep_to_rel") states_by_sp[["bsb"]]$p else 0,
            convergence_bsb = as.integer(is_achieved(diff_keep, pct_diff_keep, MRIP_keep))
          )]
          
          final_rows[species == "scup", `:=`(
            keep_to_rel_scup = as.integer(states_by_sp[["scup"]]$direction == "keep_to_rel"),
            rel_to_keep_scup = as.integer(states_by_sp[["scup"]]$direction == "rel_to_keep"),
            p_rel_to_keep_scup = if (states_by_sp[["scup"]]$direction == "rel_to_keep") states_by_sp[["scup"]]$p else 0,
            p_keep_to_rel_scup = if (states_by_sp[["scup"]]$direction == "keep_to_rel") states_by_sp[["scup"]]$p else 0,
            convergence_scup = as.integer(is_achieved(diff_keep, pct_diff_keep, MRIP_keep))
          )]
          
          # fill non-target species columns with zeros where still missing
          fill_zero_cols <- c(
            "keep_to_rel_sf","rel_to_keep_sf","p_rel_to_keep_sf","p_keep_to_rel_sf","convergence_sf",
            "keep_to_rel_bsb","rel_to_keep_bsb","p_rel_to_keep_bsb","p_keep_to_rel_bsb","convergence_bsb",
            "keep_to_rel_scup","rel_to_keep_scup","p_rel_to_keep_scup","p_keep_to_rel_scup","convergence_scup",
            "n_sub_kept_scup","prop_sub_kept_scup","n_legal_rel_scup","prop_legal_rel_scup",
            "n_sub_kept_sf","n_legal_rel_sf","prop_sub_kept_sf","prop_legal_rel_sf",
            "n_sub_kept_bsb","n_legal_rel_bsb","prop_sub_kept_bsb","prop_legal_rel_bsb", 
            "floor_used_in_sf", "floor_used_in_bsb", "floor_used_in_scup")
          
          for (cc in intersect(fill_zero_cols, names(final_rows))) {
            set(final_rows, which(is.na(final_rows[[cc]])), cc, 0)
          }
          
          final_rows[, iter_used := iter_used]
          setcolorder(final_rows, c("draw","state", "mode","species","MRIP_rel","model_rel","diff_rel","pct_diff_rel",
                                    "MRIP_keep","model_keep","diff_keep","pct_diff_keep",
                                    "MRIP_catch","model_catch","diff_catch","pct_diff_catch",
                                    "rel_to_keep_new","keep_to_rel_new","p_rel_to_keep_new","p_keep_to_rel_new",
                                    "floor_used_in_sf", "floor_used_in_bsb", "floor_used_in_scup",
                                    setdiff(names(final_rows), c("draw","state", "mode","species","MRIP_rel","model_rel","diff_rel","pct_diff_rel",
                                                                 "MRIP_keep","model_keep","diff_keep","pct_diff_keep",
                                                                 "MRIP_catch","model_catch","diff_catch","pct_diff_catch",
                                                                 "rel_to_keep_new","keep_to_rel_new","p_rel_to_keep_new","p_keep_to_rel_new", 
                                                                 "floor_used_in_sf", "floor_used_in_bsb", "floor_used_in_scup"))))
          
          rerun_results[[rr]] <- final_rows
          rr <- rr + 1L
        }
      }
    
    
  calibrated_combined2 <- rbindlist(rerun_results, use.names = TRUE, fill = TRUE)
    
    drop_cols <- c(
      "rel_to_keep_new", "keep_to_rel_new",
      "p_rel_to_keep_new", "p_keep_to_rel_new"
    )
    
    drop_cols <- intersect(drop_cols, names(calibrated_combined2))
    calibrated_combined2[, (drop_cols) := NULL]
    
    # one row per state-mode-draw is enough, since the final calibration values are wide
    calibrated_combined2 <- unique(calibrated_combined2, by = c("state", "mode", "draw", "species"))
    
    front_cols <- c("state", "mode", "draw")
    front_cols <- intersect(front_cols, names(calibrated_combined2))
    data.table::setcolorder(calibrated_combined2, c(front_cols, setdiff(names(calibrated_combined2), front_cols)))
    
    
    # identify all species suffixes
    species_levels <- c("sf", "bsb", "scup")
    
    # find all columns that have species suffixes
    suffix_pattern <- paste0("(", paste(species_levels, collapse = "|"), ")$")
    cols <- names(calibrated_combined2)
    
    suffix_cols <- cols[grepl(paste0("_", suffix_pattern), cols)]
    
    # get base variable names (remove suffix)
    base_names <- unique(sub(paste0("_", suffix_pattern), "", suffix_cols))
    
    # for each base variable, create a collapsed version
    for (v in base_names) {
      
      new_col <- v
      
      calibrated_combined2[, (new_col) := fifelse(
        species == "sf",  get(paste0(v, "_sf")),
        fifelse(
          species == "bsb", get(paste0(v, "_bsb")),
          fifelse(
            species == "scup", get(paste0(v, "_scup")),
            NA_real_
          )
        )
      )]
    }
    
    # drop the wide columns
    calibrated_combined2[, (suffix_cols) := NULL]
    
    # reorder columns
    #setcolorder(calibrated_combined2, c("state", "mode", "draw", "species", base_names))
    
    
# replace original problematic rows with the rerun rows
if (nrow(calibrated_combined2) > 0) {
  key_cols <- c("state", "mode", "draw", "species")
  
  calibrated_combined_final <- calibrated_combined[
    !calibrated_combined2,
    on = key_cols
  ]
  
  calibrated_combined_final <- data.table::rbindlist(
    list(calibrated_combined_final, calibrated_combined2),
    use.names = TRUE,
    fill = TRUE
  )
}else{
  calibrated_combined_final<- calibrated_combined
}

# optional final sort
data.table::setorderv(calibrated_combined_final, c("state", "mode", "draw", "species"))

# check for any problem rows 
problem_rows <- calibrated_combined_final[
  , needs_rerun := mapply(
    needs_floor4_rerun,
    rel_to_keep,
    convergence,
    diff_keep,
    pct_diff_keep,
    MRIP_keep
  )
][needs_rerun == TRUE]

fst::write_fst(calibrated_combined_final,
               file.path(iterative_input_data_cd,
                         paste0("archive/miscellaneous/calibrated_model_stats.fst")))