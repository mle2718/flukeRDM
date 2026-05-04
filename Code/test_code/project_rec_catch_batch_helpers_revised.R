# project_rec_catch_batch_helpers.R
# Batch helpers for project_rec_catch_final.R.
# Source project_rec_catch_final.R first, because read_projection_common_inputs()
# and compute_projection() are defined there.

make_projection_grid <- function(states,
                                 draws,
                                 run_tag = "final",
                                 ndraws = 50L,
                                 modes = c("sh", "pr", "fh")) {
  grid <- data.table::CJ(state = states, draw = draws, sorted = FALSE)
  
  grid[, run_tag := rep(run_tag, .N)]
  grid[, ndraws  := rep(as.integer(ndraws), .N)]
  grid[, modes   := rep(list(modes), .N)]
  
  grid[]
}

run_one_projection_job <- function(state,
                                   draw,
                                   iterative_input_data_cd,
                                   input_data_cd,
                                   output_cd = file.path(iterative_input_data_cd, "archive/projection_outputs"),
                                   ndraws = 50L,
                                   modes = c("sh", "pr", "fh"),
                                   run_tag = "final",
                                   write_intermediate = FALSE,
                                   common_inputs = NULL,
                                   base_outcomes_date_tag = "4_16_26",
                                   quiet = FALSE) {
  if (!quiet) message("Running ", state, " draw ", draw)

  compute_projection(
    st = state,
    dr = draw,
    iterative_input_data_cd = iterative_input_data_cd,
    input_data_cd = input_data_cd,
    output_cd = output_cd,
    ndraws = ndraws,
    modes = modes,
    run_tag = run_tag,
    write_intermediate = write_intermediate,
    common_inputs = common_inputs,
    base_outcomes_date_tag = base_outcomes_date_tag
  )
}

run_projection_batch_purrr <- function(states,
                                       draws,
                                       iterative_input_data_cd,
                                       input_data_cd,
                                       common_inputs = NULL,
                                       run_tag = "final",
                                       ndraws = 50L,
                                       modes = c("sh", "pr", "fh"),
                                       output_cd = NULL) {
  
  if (is.null(common_inputs)) {
    common_inputs <- read_projection_common_inputs(
      iterative_input_data_cd = iterative_input_data_cd,
      input_data_cd = input_data_cd,
      states = states,
      draws = draws
    )
  }
  
  grid <- make_projection_grid(
    states = states,
    draws = draws,
    run_tag = run_tag,
    ndraws = ndraws,
    modes = modes
  )
  
  out_list <- purrr::pmap(
    .l = list(
      state = grid$state,
      draw = grid$draw,
      ndraws = grid$ndraws,
      modes = grid$modes
    ),
    .f = function(state, draw, ndraws, modes) {
      run_one_projection_job(
        state = state,
        draw = draw,
        ndraws = ndraws,
        modes = modes,
        iterative_input_data_cd = iterative_input_data_cd,
        input_data_cd = input_data_cd,
        common_inputs = common_inputs,
        run_tag = run_tag,
        output_cd = output_cd
      )
    }
  )
  
  data.table::rbindlist(
    out_list,
    fill = TRUE,
    use.names = TRUE
  )
}

run_projection_batch_furrr <- function(states,
                                       draws,
                                       iterative_input_data_cd,
                                       input_data_cd,
                                       common_inputs = NULL,
                                       run_tag = "final",
                                       ndraws = 50L,
                                       modes = c("sh", "pr", "fh"),
                                       output_cd = NULL) {
  
  if (is.null(common_inputs)) {
    common_inputs <- read_projection_common_inputs(
      iterative_input_data_cd = iterative_input_data_cd,
      input_data_cd = input_data_cd,
      states = states,
      draws = draws
    )
  }
  
  grid <- make_projection_grid(
    states = states,
    draws = draws,
    run_tag = run_tag,
    ndraws = ndraws,
    modes = modes
  )
  
  out_list <- furrr::pmap(
    .l = list(
      state = grid$state,
      draw = grid$draw,
      ndraws = grid$ndraws,
      modes = grid$modes
    ),
    .f = function(state, draw, ndraws, modes) {
      run_one_projection_job(
        state = state,
        draw = draw,
        ndraws = ndraws,
        modes = modes,
        iterative_input_data_cd = iterative_input_data_cd,
        input_data_cd = input_data_cd,
        common_inputs = common_inputs,
        run_tag = run_tag,
        output_cd = output_cd
      )
    }
  )
  
  data.table::rbindlist(
    out_list,
    fill = TRUE,
    use.names = TRUE
  )
}

# Example usage:
# source("project_rec_catch_final.R")
# source("project_rec_catch_batch_helpers.R")
#
# iterative_input_data_cd <- "E:/Lou_projects/flukeRDM/flukeRDM_iterative_data"
# input_data_cd <- "C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Data"
# states <- c("MA", "RI", "CT", "NY", "NJ", "DE", "MD", "VA", "NC")
# draws <- 1:100
#
# common_inputs <- read_projection_common_inputs(
#   iterative_input_data_cd = iterative_input_data_cd,
#   input_data_cd = input_data_cd,
#   states = states,
#   draws = draws
# )
#
# pred <- run_projection_batch_purrr(
#   states = states,
#   draws = draws,
#   iterative_input_data_cd = iterative_input_data_cd,
#   input_data_cd = input_data_cd,
#   common_inputs = common_inputs,
#   run_tag = "candidate_reg_set_1"
# )
