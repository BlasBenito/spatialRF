library(spatialRF)
library(future)
library(progressr)
library(collinear)

options(future.globals.maxSize = 3 * 1024^3) # 3 GB

data(vi, vi_predictors_numeric)

vi_xy <- vi[, c("longitude", "latitude")]
colnames(vi_xy) <- c("x", "y")

future::plan(
  strategy = future::multisession,
  workers = future::availableCores() - 1
)

progressr::handlers(global = TRUE)


m <- rf(
  data = vi,
  dependent.variable.name = "vi_numeric",
  predictor.variable.names = vi_predictors_numeric,
  n.cores = future::availableCores() - 1
)

m_eval <- rf_evaluate(
  model = m,
  xy = vi_xy,
  repetitions = 100,
  training.fraction = 0.5,
  metrics = c("r.squared", "rmse"),
  verbose = TRUE
)

m <- rf_repeat(
  model = m,
  xy = vi_xy,
  repetitions = 100,
  n.cores = 1
)

future::plan(
  strategy = future::sequential
)

# ============================================================================
# SECTION 3: rf_evaluate() - Spatial Cross-Validation
# ============================================================================
cat("\n=== rf_evaluate(): Spatial Cross-Validation ===\n")

# Parallel evaluation with multisession
future::plan(future::multisession, workers = 4)

m_eval <- rf_evaluate(
  model = m,
  xy = vi_xy,
  repetitions = 30,
  training.fraction = 0.75,
  metrics = c("r.squared", "rmse"),
  verbose = TRUE
)

# View results
get_evaluation(m_eval)
plot_evaluation(m_eval)

# ============================================================================
# SECTION 4: rf_tuning() - Hyperparameter Optimization
# ============================================================================
cat("\n=== rf_tuning(): Hyperparameter Optimization ===\n")

# Parallel tuning - tests combinations in parallel
# Each combination uses n.cores=1 (auto-set with parallel plan)
future::plan(future::multisession, workers = 8)

m_tuned <- rf_tuning(
  model = m,
  xy = vi_xy,
  num.trees = c(500, 1000),
  mtry = c(5, 10, 15),
  min.node.size = c(5, 10),
  repetitions = 10,
  verbose = TRUE
)

# View tuning results
plot_tuning(m_tuned)

# ============================================================================
# SECTION 5: rf_spatial() - Spatial Random Forest
# ============================================================================
cat("\n=== rf_spatial(): Spatial Random Forest ===\n")

# Prepare distance matrix for spatial modeling
vi_distance <- as.matrix(dist(vi_xy))

# Parallel spatial predictor ranking and selection
future::plan(future::multisession, workers = 6)

m_spatial <- rf_spatial(
  data = vi,
  dependent.variable.name = "vi_numeric",
  predictor.variable.names = vi_predictors_numeric,
  distance.matrix = vi_distance,
  distance.thresholds = c(0, 100, 500),
  method = "mem.moran.sequential", # MEM + Moran ranking + sequential selection
  verbose = TRUE
)

# View spatial model results
print(m_spatial)
plot_importance(m_spatial)
plot_moran(m_spatial)

# ============================================================================
# SECTION 6: rf_importance() - Importance via Cross-Validation
# ============================================================================
cat("\n=== rf_importance(): Variable Importance via CV ===\n")

# Calculate variable importance through spatial cross-validation
# Tests each variable's contribution to model transferability
future::plan(future::sequential) # Sequential because it's already testing multiple variables

m_importance <- rf_importance(
  model = m,
  xy = vi_xy,
  repetitions = 15,
  metric = "r.squared",
  verbose = TRUE
)

# View importance results
print(m_importance$importance$per.variable)

# ============================================================================
# SECTION 7: rf_compare() - Model Comparison
# ============================================================================
cat("\n=== rf_compare(): Compare Multiple Models ===\n")

# Compare non-spatial vs spatial models
future::plan(future::multisession, workers = 4)

comparison <- rf_compare(
  models = list(
    `Non-spatial` = m,
    `Spatial` = m_spatial
  ),
  xy = vi_xy,
  repetitions = 20,
  metrics = c("r.squared", "rmse"),
  verbose = TRUE
)

# View comparison
print(comparison$comparison.df)
print(comparison$plot)

# ============================================================================
# SECTION 8: the_feature_engineer() - Feature Interactions
# ============================================================================
cat("\n=== the_feature_engineer(): Feature Interaction Discovery ===\n")

# Increase globals limit for this function
options(future.globals.maxSize = 3 * 1024^3)

# Use subset of data for faster demonstration
vi_subset <- vi[1:100, ]
vi_xy_subset <- vi_xy[1:100, ]
predictors_subset <- vi_predictors_numeric[1:8] # Use fewer predictors

# Parallel testing of variable interactions
future::plan(future::multisession, workers = 4)

interactions <- the_feature_engineer(
  data = vi_subset,
  dependent.variable.name = "vi_numeric",
  predictor.variable.names = predictors_subset,
  xy = vi_xy_subset,
  repetitions = 10,
  importance.threshold = 0.5,
  cor.threshold = 0.75,
  verbose = TRUE
)

# View selected interactions
if (!is.null(interactions)) {
  print(interactions$selected)
  print(interactions$plot)
}

# ============================================================================
# SECTION 9: Performance Comparison - Different Strategies
# ============================================================================
cat("\n=== Performance Comparison: Different Parallelization Strategies ===\n")

# Reset to smaller dataset for timing comparison
set.seed(1)
test_data <- vi[sample(nrow(vi), 200), ]
test_xy <- vi_xy[sample(nrow(vi_xy), 200), ]

# Strategy 1: Sequential with ranger threading
cat("\nStrategy 1: Sequential plan + ranger threading\n")
future::plan(future::sequential)
t1 <- system.time({
  m1 <- rf_repeat(
    data = test_data,
    dependent.variable.name = "vi_numeric",
    predictor.variable.names = vi_predictors_numeric[1:10],
    repetitions = 10,
    n.cores = future::availableCores(omit = 1), # All cores to ranger
    verbose = FALSE
  )
})
cat("Time:", round(t1[3], 2), "seconds\n")

# Strategy 2: Multisession with multiple workers
cat("\nStrategy 2: Multisession plan (4 workers) + ranger single-threaded\n")
future::plan(future::multisession, workers = 4)
t2 <- system.time({
  m2 <- rf_repeat(
    data = test_data,
    dependent.variable.name = "vi_numeric",
    predictor.variable.names = vi_predictors_numeric[1:10],
    repetitions = 10,
    n.cores = 1, # Auto-set to 1 with parallel plan
    verbose = FALSE
  )
})
cat("Time:", round(t2[3], 2), "seconds\n")

# Strategy 3: Multicore (Linux/Mac only)
if (.Platform$OS.type == "unix") {
  cat("\nStrategy 3: Multicore plan (fork-based) + ranger single-threaded\n")
  future::plan(future::multicore, workers = 4)
  t3 <- system.time({
    m3 <- rf_repeat(
      data = test_data,
      dependent.variable.name = "vi_numeric",
      predictor.variable.names = vi_predictors_numeric[1:10],
      repetitions = 10,
      verbose = FALSE
    )
  })
  cat("Time:", round(t3[3], 2), "seconds\n")
}

# ============================================================================
# CLEANUP
# ============================================================================
cat("\n=== Workflow Complete ===\n")

# Reset to sequential
future::plan(future::sequential)

# Print summary
cat("\nMigrated functions demonstrated:\n")
cat("  ✓ rf() - Basic random forest\n")
cat("  ✓ rf_repeat() - Model repetitions\n")
cat("  ✓ rf_evaluate() - Spatial cross-validation\n")
cat("  ✓ rf_tuning() - Hyperparameter tuning\n")
cat("  ✓ rf_spatial() - Spatial random forest\n")
cat("  ✓ rf_importance() - CV-based importance\n")
cat("  ✓ rf_compare() - Model comparison\n")
cat("  ✓ the_feature_engineer() - Feature interactions\n")
cat("\nAll functions now use the future ecosystem!\n")
cat("Users control parallelization via future::plan()\n")
cat("Progress reporting via progressr::handlers(global = TRUE)\n")
