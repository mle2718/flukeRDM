
#Step 5: Benchmarking speed for "old" vs "new" code 8/7/25

library(microbenchmark)

microbenchmark(
  old = { source("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Code/test_code/new_code_8_7_25.R") },   # if saved as a script
  new = {
    results_list <- lapply(mode_draw, simulate_mode)
    sf_trip_data <- rbindlist(lapply(results_list, `[[`, "trip_data"))
  },
  times = 5
)


