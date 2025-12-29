# Test script to verify progressr works in make_spatial_folds()
library(spatialRF)
library(future)
library(progressr)

data(plants_df, plants_xy)

# WITHOUT handlers - no progress shown
cat("\n=== Test 1: WITHOUT progressr handlers (no progress bars) ===\n")
future::plan(future::sequential)

xy.thin <- thinning_til_n(
  xy = plants_xy,
  n = 10
)

folds1 <- make_spatial_folds(
  xy.selected = xy.thin,
  xy = plants_xy,
  distance.step.x = 0.05,
  training.fraction = 0.6,
  n.cores = 1
)

cat("Completed without progress bars (expected)\n")

# WITH handlers - progress shown
cat("\n=== Test 2: WITH progressr handlers (progress bars enabled) ===\n")
progressr::handlers(global = TRUE)

folds2 <- make_spatial_folds(
  xy.selected = xy.thin,
  xy = plants_xy,
  distance.step.x = 0.05,
  training.fraction = 0.6,
  n.cores = 1
)

cat("\nCompleted with progress bars (should have shown progress)\n")

# WITH parallel plan
cat("\n=== Test 3: WITH parallel plan + handlers ===\n")
future::plan(future::multisession, workers = 2)

folds3 <- make_spatial_folds(
  xy.selected = xy.thin,
  xy = plants_xy,
  distance.step.x = 0.05,
  training.fraction = 0.6,
  n.cores = 1
)

cat("\nCompleted with parallel execution and progress\n")

future::plan(future::sequential)
cat("\n=== All tests complete ===\n")
