# calibration-year trip simulation WITHOUT any adjustments for illegal harvest or voluntary release
# rewritten for speed/efficiency while RETAINING fish-level expansion

library(data.table)
library(arrow)
library(readr)
library(haven)

safe_divide <- function(num, den) {
  ifelse(is.na(den) | den == 0, NA_real_, num / den)
}

# stable binary logit probability for the trip alternative
calc_prob_trip <- function(v_trip, v_optout) {
  z <- v_trip - v_optout
  out <- numeric(length(z))
  pos <- z >= 0
  out[pos] <- 1 / (1 + exp(-z[pos]))
  ez <- exp(z[!pos])
  out[!pos] <- ez / (1 + ez)
  out
}

simulate_species <- function(catch_dt,
                             catch_col,
                             bag_col,
                             min_col,
                             size_dt,
                             species_prefix = c("sf", "bsb", "scup")) {

  species_prefix <- match.arg(species_prefix)

  keep_col <- paste0("tot_keep_", species_prefix, "_new")
  rel_col  <- paste0("tot_rel_",  species_prefix, "_new")

  key_cols <- c("date", "mode", "tripid", "catch_draw")

  pos_dt <- catch_dt[get(catch_col) > 0,
                     .(date, mode, tripid, catch_draw,
                       catch_n = get(catch_col),
                       bag     = get(bag_col),
                       min_sz  = get(min_col))]

  zero_dt <- catch_dt[get(catch_col) == 0,
                      .(date, mode, tripid, catch_draw)]

  trip_out_zero <- copy(zero_dt)
  trip_out_zero[, c(keep_col, rel_col) := .(0L, 0L)]

  if (nrow(pos_dt) == 0L) {
    setcolorder(trip_out_zero, c(key_cols, keep_col, rel_col))
    setkeyv(trip_out_zero, key_cols)
    return(trip_out_zero)
  }

  fish_dt <- pos_dt[rep(seq_len(.N), catch_n)]
  fish_dt[, fishid := seq_len(.N)]

  fish_dt[, fitted_length := sample(size_dt$length,
                                    .N,
                                    replace = TRUE,
                                    prob = size_dt$fitted_prob)]

  fish_dt[, posskeep := fifelse(fitted_length >= min_sz, 1L, 0L)]

  setorder(fish_dt, date, mode, tripid, catch_draw, fishid)
  fish_dt[, csum_keep := cumsum(posskeep), by = key_cols]
  fish_dt[, keep := fifelse(bag > 0 & posskeep == 1L & csum_keep <= bag, 1L, 0L)]
  fish_dt[, release := fifelse(keep == 0L, 1L, 0L)]

  trip_out_pos <- fish_dt[, .(
    keep_n = sum(keep),
    rel_n  = sum(release)
  ), by = key_cols]

  setnames(trip_out_pos, c("keep_n", "rel_n"), c(keep_col, rel_col))

  trip_out <- rbindlist(list(trip_out_pos, trip_out_zero), use.names = TRUE, fill = TRUE)
  setkeyv(trip_out, key_cols)
  trip_out[]
}

build_compare_table <- function(summed_results, MRIP_comparison_draw, md) {

  metric_cols <- c(
    "sf_keep", "sf_rel", "sf_catch",
    "bsb_keep", "bsb_rel", "bsb_catch",
    "scup_keep", "scup_rel", "scup_catch"
  )

  model_metrics  <- intersect(metric_cols, names(summed_results))
  mrip_metrics   <- intersect(metric_cols, names(MRIP_comparison_draw))
  common_metrics <- intersect(model_metrics, mrip_metrics)

  if (length(common_metrics) == 0L) {
    stop("No common metric columns found between summed_results and MRIP_comparison_draw.")
  }

  model_long <- melt(
    as.data.table(summed_results)[, c("mode", common_metrics), with = FALSE],
    id.vars = "mode",
    measure.vars = common_metrics,
    variable.name = "metric",
    value.name = "model"
  )

  mrip_long <- melt(
    as.data.table(MRIP_comparison_draw)[, c("mode", common_metrics), with = FALSE],
    id.vars = "mode",
    measure.vars = common_metrics,
    variable.name = "metric",
    value.name = "MRIP"
  )

  model_long[, model := as.numeric(model)]
  mrip_long[, MRIP := as.numeric(MRIP)]

  cmp <- merge(model_long, mrip_long, by = c("mode", "metric"), all = FALSE)
  cmp[, c("species", "disposition") := tstrsplit(metric, "_", fixed = TRUE, keep = 1:2)]
  cmp[, diff := model - MRIP]
  cmp[, pct_diff := fifelse(MRIP != 0, 100 * diff / MRIP, NA_real_)]
  cmp[, abs_diff_val := abs(diff)]
  cmp[, abs_pct_diff_val := fifelse(MRIP != 0, abs(100 * diff / MRIP), NA_real_)]
  cmp[, mode := md]

  cmp <- cmp[
    species %in% c("sf", "bsb", "scup") &
      disposition %in% c("keep", "rel", "catch"),
    .(species, disposition, mode, MRIP, model, diff, pct_diff,
      abs_diff_val, abs_pct_diff_val)
  ]

  compare_k <- cmp[disposition == "keep",
                   .(mode, species,
                     MRIP_keep = MRIP,
                     model_keep = model,
                     diff_keep = diff,
                     pct_diff_keep = pct_diff)]

  compare_c <- cmp[disposition == "catch",
                   .(mode, species,
                     MRIP_catch = MRIP,
                     model_catch = model,
                     diff_catch = diff,
                     pct_diff_catch = pct_diff)]

  compare_r <- cmp[disposition == "rel",
                   .(mode, species,
                     MRIP_rel = MRIP,
                     model_rel = model,
                     diff_rel = diff,
                     pct_diff_rel = pct_diff)]

  out <- merge(compare_r, compare_k, by = c("mode", "species"), all = TRUE)
  out <- merge(out, compare_c, by = c("mode", "species"), all = TRUE)

  out[, rel_to_keep := fifelse(diff_keep < 0, 1, 0)]
  out[, keep_to_rel := fifelse(diff_keep > 0, 1, 0)]
  out[, p_rel_to_keep := abs(safe_divide(diff_keep, model_rel))]
  out[, p_keep_to_rel := abs(safe_divide(diff_keep, model_keep))]

  out[]
}

MRIP_comparison <- read_dta("E:/Lou_projects/flukeRDM/flukeRDM_iterative_data/archive/calib_catch_draws/simulated_catch_totals.dta") |>
  as.data.table()

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

states <- c("MA", "RI", "CT", "NY", "NJ", "DE", "MD", "VA", "NC")
mode_draw <- c("sh", "pr", "fh")
draws <- 1:n_simulations

# preload catch-at-length once instead of reading inside the innermost loop
size_lookup_raw <- as.data.table(
  read_csv(file.path(input_data_cd, "baseline_catch_at_length.csv"),
           show_col_types = FALSE)
)

size_lookup_raw <- size_lookup_raw[
  !is.na(fitted_prob),
  .(state, draw, species, fitted_prob, length)
]
setkey(size_lookup_raw, state, draw, species)

calib_comparison <- vector("list", length(states) * length(mode_draw) * length(draws))
k <- 1L

# s<-"MA"
# md<-"pr"
# i<-1

for (s in states) {

  dtrip_state <- as.data.table(
    read_fst(file.path(
      iterative_input_data_cd,
      paste0("archive/directed_trips_calibration/directed_trips_calibration_", s, ".fst")
    ))
  )

  catch_state <- NULL

  for (i in draws) {

    dtrip_draw <- dtrip_state[
      draw == i,
      .(mode, date, dtrip, bsb_bag, bsb_min, fluke_bag, fluke_min, scup_bag, scup_min)
    ]

    catch_draw_dt <- as.data.table(
      read_fst(file.path(
        iterative_input_data_cd,
        paste0("archive/calib_catch_draws/calib_catch_draws_", s, "_", i, ".fst")
      ))
    )

    sf_size_data   <- size_lookup_raw[list(s, i, "sf"),   .(fitted_prob, length)]
    bsb_size_data  <- size_lookup_raw[list(s, i, "bsb"),  .(fitted_prob, length)]
    scup_size_data <- size_lookup_raw[list(s, i, "scup"), .(fitted_prob, length)]

    for (md in mode_draw) {

      dtripz <- dtrip_draw[mode == md]
      catch_data <- merge(
        catch_draw_dt[mode == md],
        dtripz,
        by = c("mode", "date"),
        all.x = TRUE
      )

      if (nrow(catch_data) == 0L) {
        MRIP_comparison_draw <- MRIP_comparison[
          draw == i & state == s & mode == md,
          .(mode, sf_keep, sf_rel, sf_catch,
            bsb_keep, bsb_rel, bsb_catch,
            scup_keep, scup_rel, scup_catch)
        ]

        if (nrow(MRIP_comparison_draw) == 0L) {
          MRIP_comparison_draw <- data.table(
            mode = md,
            sf_keep = NA_real_, sf_rel = NA_real_, sf_catch = NA_real_,
            bsb_keep = NA_real_, bsb_rel = NA_real_, bsb_catch = NA_real_,
            scup_keep = NA_real_, scup_rel = NA_real_, scup_catch = NA_real_
          )
        }

        summed_results <- data.table(
          mode = md,
          sf_catch = 0, sf_keep = 0, sf_rel = 0,
          bsb_catch = 0, bsb_keep = 0, bsb_rel = 0,
          scup_catch = 0, scup_keep = 0, scup_rel = 0,
          estimated_trips = 0, n_choice_occasions = 0
        )

        compare_out <- build_compare_table(summed_results, MRIP_comparison_draw, md)
        compare_out[, `:=`(draw = i, state = s)]
        calib_comparison[[k]] <- compare_out
        k <- k + 1L
        next
      }

      angler_dems <- unique(
        catch_data[, .(date, mode, tripid, total_trips_12, age, cost)]
      )

      sf_trip_data <- simulate_species(
        catch_dt = catch_data,
        catch_col = "sf_cat",
        bag_col   = "fluke_bag",
        min_col   = "fluke_min",
        size_dt   = sf_size_data,
        species_prefix = "sf"
      )
      # catch_dt = catch_data
      # catch_col = "sf_cat"
      # bag_col   = "fluke_bag"
      # min_col   = "fluke_min"
      # size_dt   = sf_size_data
      # species_prefix = "sf"
      
      bsb_trip_data <- simulate_species(
        catch_dt = catch_data,
        catch_col = "bsb_cat",
        bag_col   = "bsb_bag",
        min_col   = "bsb_min",
        size_dt   = bsb_size_data,
        species_prefix = "bsb"
      )

      scup_trip_data <- simulate_species(
        catch_dt = catch_data,
        catch_col = "scup_cat",
        bag_col   = "scup_bag",
        min_col   = "scup_min",
        size_dt   = scup_size_data,
        species_prefix = "scup"
      )

      key_cols <- c("date", "mode", "tripid", "catch_draw")
      setkeyv(sf_trip_data, key_cols)
      setkeyv(bsb_trip_data, key_cols)
      setkeyv(scup_trip_data, key_cols)

      trip_data <- merge(sf_trip_data, bsb_trip_data, by = key_cols, all = TRUE)
      trip_data <- merge(trip_data, scup_trip_data, by = key_cols, all = TRUE)

      zero_fill_cols <- intersect(
        c("tot_keep_sf_new", "tot_rel_sf_new",
          "tot_keep_bsb_new", "tot_rel_bsb_new",
          "tot_keep_scup_new", "tot_rel_scup_new"),
        names(trip_data)
      )
      for (cc in zero_fill_cols) {
        set(trip_data, which(is.na(trip_data[[cc]])), cc, 0L)
      }

      trip_data[, `:=`(
        tot_scup_catch = tot_keep_scup_new + tot_rel_scup_new,
        tot_bsb_catch  = tot_keep_bsb_new + tot_rel_bsb_new,
        tot_sf_catch   = tot_keep_sf_new + tot_rel_sf_new
      )]

      parameters <- unique(trip_data[, .(date, mode, tripid)])

      parameters[, `:=`(
        beta_sqrt_sf_keep     = rnorm(.N, mean = 0.827, sd = 1.267),
        beta_sqrt_sf_release  = rnorm(.N, mean = 0.065, sd = 0.325),
        beta_sqrt_bsb_keep    = rnorm(.N, mean = 0.353, sd = 0.129),
        beta_sqrt_bsb_release = rnorm(.N, mean = 0.074, sd = 0),
        beta_sqrt_sf_bsb_keep = rnorm(.N, mean = -0.056, sd = 0.196),
        beta_sqrt_scup_catch  = rnorm(.N, mean = 0.018, sd = 0),
        beta_opt_out          = rnorm(.N, mean = -2.056, sd = 1.977),
        beta_opt_out_avidity  = rnorm(.N, mean = -0.010, sd = 0),
        beta_opt_out_age      = rnorm(.N, mean = 0.010, sd = 0),
        beta_cost             = -0.012
      )]

      setkey(parameters, date, mode, tripid)
      setkey(angler_dems, date, mode, tripid)
      trip_data <- merge(trip_data, parameters, by = c("date", "mode", "tripid"), all.x = TRUE)
      trip_data <- merge(trip_data, angler_dems, by = c("date", "mode", "tripid"), all.x = TRUE)

      setorder(trip_data, date, mode, tripid, catch_draw)

      trip_data[, `:=`(
        vA_trip =
          beta_sqrt_sf_keep * sqrt(tot_keep_sf_new) +
          beta_sqrt_sf_release * sqrt(tot_rel_sf_new) +
          beta_sqrt_bsb_keep * sqrt(tot_keep_bsb_new) +
          beta_sqrt_bsb_release * sqrt(tot_rel_bsb_new) +
          beta_sqrt_sf_bsb_keep * (sqrt(tot_keep_sf_new) * sqrt(tot_keep_bsb_new)) +
          beta_sqrt_scup_catch * sqrt(tot_scup_catch) +
          beta_cost * cost,

        vA_optout =
          beta_opt_out +
          beta_opt_out_age * age +
          beta_opt_out_avidity * total_trips_12
      )]

      mean_trip_data <- copy(trip_data)

      drop_cols <- intersect(
        c("beta_cost", "beta_opt_out", "beta_opt_out_age",
          "beta_opt_out_avidity", "beta_sqrt_bsb_keep", "beta_sqrt_bsb_release",
          "beta_sqrt_scup_catch", "beta_sqrt_sf_bsb_keep",
          "beta_sqrt_sf_keep", "beta_sqrt_sf_release",
          "age", "cost", "total_trips_12"),
        names(mean_trip_data)
      )
      if (length(drop_cols)) mean_trip_data[, (drop_cols) := NULL]

      keep_vars <- setdiff(names(mean_trip_data), c("date", "mode", "tripid"))
      mean_trip_data <- mean_trip_data[, lapply(.SD, mean),
                                       by = .(date, mode, tripid),
                                       .SDcols = keep_vars]

      mean_trip_data[, probA := calc_prob_trip(vA_trip, vA_optout)]
      mean_trip_data[, c("vA_trip", "vA_optout", "catch_draw") := NULL]

      wt_cols <- c(
        "tot_keep_sf_new", "tot_rel_sf_new", "tot_sf_catch",
        "tot_keep_bsb_new", "tot_rel_bsb_new", "tot_bsb_catch",
        "tot_keep_scup_new", "tot_rel_scup_new", "tot_scup_catch"
      )

      mean_trip_data[, (wt_cols) := lapply(.SD, function(x) x * probA), .SDcols = wt_cols]

      mean_trip_data <- merge(mean_trip_data, dtripz, by = c("mode", "date"), all.x = TRUE)
      drop_reg_cols <- intersect(
        c("bsb_bag", "bsb_min", "fluke_bag", "fluke_min", "scup_bag", "scup_min"),
        names(mean_trip_data)
      )
      if (length(drop_reg_cols)) mean_trip_data[, (drop_reg_cols) := NULL]

      mean_trip_data[, mean_prob := mean(probA), by = .(mode, date)]
      mean_trip_data[is.na(mean_prob) | mean_prob == 0, mean_prob := NA_real_]
      mean_trip_data[, sims := fifelse(!is.na(mean_prob), round(dtrip / mean_prob), 0)]
      mean_trip_data[, expand := sims / n_draws]
      mean_trip_data[, n_choice_occasions := 1]

      expand_cols <- c(wt_cols, "n_choice_occasions", "probA")
      mean_trip_data[, (expand_cols) := lapply(.SD, function(x) x * expand), .SDcols = expand_cols]

      for (j in names(mean_trip_data)) setattr(mean_trip_data[[j]], "label", NULL)

      aggregate_trip_data <- mean_trip_data[, lapply(.SD, sum),
                                            by = .(date, mode),
                                            .SDcols = expand_cols]

      setnames(
        aggregate_trip_data,
        old = c("probA", "tot_sf_catch", "tot_bsb_catch", "tot_scup_catch",
                "tot_keep_sf_new", "tot_keep_bsb_new", "tot_keep_scup_new",
                "tot_rel_sf_new", "tot_rel_bsb_new", "tot_rel_scup_new"),
        new = c("estimated_trips", "sf_catch", "bsb_catch", "scup_catch",
                "sf_keep", "bsb_keep", "scup_keep",
                "sf_rel", "bsb_rel", "scup_rel"),
        skip_absent = TRUE
      )

      list_names <- c("bsb_catch", "bsb_keep", "bsb_rel",
                      "scup_catch", "scup_keep", "scup_rel",
                      "sf_catch", "sf_keep", "sf_rel",
                      "estimated_trips", "n_choice_occasions")

      summed_results <- aggregate_trip_data[, lapply(.SD, sum),
                                            by = .(mode),
                                            .SDcols = list_names]

      MRIP_comparison_draw <- MRIP_comparison[
        draw == i & state == s & mode == md,
        .(mode, sf_keep, sf_rel, sf_catch,
          bsb_keep, bsb_rel, bsb_catch,
          scup_keep, scup_rel, scup_catch)
      ]

      compare_out <- build_compare_table(summed_results, MRIP_comparison_draw, md)
      compare_out[, `:=`(draw = i, state = s)]

      calib_comparison[[k]] <- compare_out
      k <- k + 1L
    }
  }
}

calib_comparison_combined <- rbindlist(calib_comparison, use.names = TRUE, fill = TRUE)
setcolorder(calib_comparison_combined, c("state", "mode", "species", "draw",
                                         setdiff(names(calib_comparison_combined),
                                                 c("state", "mode", "species", "draw"))))

fst::write_fst(calib_comparison_combined,
                   file.path(iterative_input_data_cd,
                   paste0("archive/miscellaneous/calibration_comparison.fst")))
                   
