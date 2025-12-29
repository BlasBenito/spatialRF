# Profile parallelization overhead vs benefit
library(spatialRF)
library(future)

data(plants_rf, plants_xy)

# Test 1: Sequential with ranger threading
cat("\n=== Test 1: Sequential + ranger threading ===\n")
plan(sequential)
t1 <- system.time({
  result1 <- rf_evaluate(
    model = plants_rf,
    xy = plants_xy,
    repetitions = 30,
    n.cores = 15,  # Ranger uses 15 threads
    verbose = FALSE
  )
})
cat("Time:", t1[3], "seconds\n")

# Test 2: Parallel (4 workers) + ranger sequential
cat("\n=== Test 2: 4 workers + n.cores=1 ===\n")
plan(multisession, workers = 4)
t2 <- system.time({
  result2 <- rf_evaluate(
    model = plants_rf,
    xy = plants_xy,
    repetitions = 30,
    n.cores = 1,  # Each worker runs ranger sequentially
    verbose = FALSE
  )
})
cat("Time:", t2[3], "seconds\n")

# Test 3: Parallel (15 workers) + ranger sequential
cat("\n=== Test 3: 15 workers + n.cores=1 ===\n")
plan(multisession, workers = 15)
t3 <- system.time({
  result3 <- rf_evaluate(
    model = plants_rf,
    xy = plants_xy,
    repetitions = 30,
    n.cores = 1,
    verbose = FALSE
  )
})
cat("Time:", t3[3], "seconds\n")

# Test 4: Multicore (Linux only)
if (.Platform$OS.type == "unix") {
  cat("\n=== Test 4: 15 workers (multicore) + n.cores=1 ===\n")
  plan(multicore, workers = 15)
  t4 <- system.time({
    result4 <- rf_evaluate(
      model = plants_rf,
      xy = plants_xy,
      repetitions = 30,
      n.cores = 1,
      verbose = FALSE
    )
  })
  cat("Time:", t4[3], "seconds\n")
}

# Summary
cat("\n=== Summary ===\n")
cat(sprintf("Sequential + ranger threading (15 cores): %.2fs\n", t1[3]))
cat(sprintf("Multisession 4 workers: %.2fs (%.1fx)\n", t2[3], t1[3]/t2[3]))
cat(sprintf("Multisession 15 workers: %.2fs (%.1fx)\n", t3[3], t1[3]/t3[3]))
if (.Platform$OS.type == "unix") {
  cat(sprintf("Multicore 15 workers: %.2fs (%.1fx)\n", t4[3], t1[3]/t4[3]))
}

cat("\nRecommendation for this dataset size:\n")
if (t1[3] < min(t2[3], t3[3])) {
  cat("Use plan(sequential) with n.cores=15 for best performance\n")
} else {
  cat("Parallel processing provides speedup - use 4-8 workers\n")
}

plan(sequential)
