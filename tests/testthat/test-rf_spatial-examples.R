test_that("`rf_spatial()` works", {
  data(distance_matrix)
  data(plant_richness_df)

  # Use subset of data for faster testing
  # Sample 80 rows to keep test fast while maintaining statistical validity
  set.seed(123)
  sample_idx <- sample(1:nrow(plant_richness_df), 80)
  plant_richness_subset <- plant_richness_df[sample_idx, ]
  distance_matrix_subset <- distance_matrix[sample_idx, sample_idx]

  # Use fewer predictors (8 instead of 17) for faster testing
  predictor.variable.names <- colnames(plant_richness_df)[5:12]

  # Test 1: hengl method (fastest, good for comprehensive testing)
  cluster <- parallel::makeCluster(
    parallel::detectCores() - 1,
    type = "PSOCK"
  )

  out <- rf_spatial(
    data = plant_richness_subset,
    dependent.variable.name = "richness_species_vascular",
    predictor.variable.names = predictor.variable.names,
    distance.matrix = distance_matrix_subset,
    distance.thresholds = 0, # Single threshold for speed
    method = "hengl",
    verbose = FALSE,
    cluster = cluster
  )

  # Comprehensive assertions for primary method
  expect_equal(inherits(out, "rf_spatial"), TRUE)
  expect_equal(inherits(out$importance$per.variable.plot, "ggplot"), TRUE)
  expect_equal(inherits(out$residuals$autocorrelation$plot, "ggplot"), TRUE)
  expect_equal(length(out$performance$rmse), 1)
  expect_equal(length(out$performance$nrmse), 1)
  expect_equal(length(out$performance$r.squared), 1)
  expect_equal(length(out$performance$pseudo.r.squared), 1)
  expect_s3_class(out$importance$per.variable, "data.frame")
  expect_true(nrow(out$importance$per.variable) > 0)

  # Test 2: mem.moran.sequential (medium speed, important method)
  out2 <- rf_spatial(
    data = plant_richness_subset,
    dependent.variable.name = "richness_species_vascular",
    predictor.variable.names = predictor.variable.names,
    distance.matrix = distance_matrix_subset,
    distance.thresholds = 0,
    method = "mem.moran.sequential",
    verbose = FALSE,
    cluster = cluster
  )

  # Basic checks to ensure method works
  expect_equal(inherits(out2, "rf_spatial"), TRUE)
  expect_true(!is.null(out2$spatial))
  expect_true(length(out2$spatial$names) > 0)
  expect_s3_class(out2$spatial$optimization, "data.frame")

  # Test 3: mem.effect.recursive (slowest, just check it doesn't error)
  # Use even smaller subset for this expensive method
  sample_idx_small <- sample(1:nrow(plant_richness_df), 50)
  plant_richness_tiny <- plant_richness_df[sample_idx_small, ]
  distance_matrix_tiny <- distance_matrix[sample_idx_small, sample_idx_small]

  expect_error(
    {
      out3 <- rf_spatial(
        data = plant_richness_tiny,
        dependent.variable.name = "richness_species_vascular",
        predictor.variable.names = predictor.variable.names[1:5], # Even fewer predictors
        distance.matrix = distance_matrix_tiny,
        distance.thresholds = 0,
        method = "mem.effect.recursive",
        verbose = FALSE,
        cluster = cluster
      )
    },
    NA # Expect no error
  )

  parallel::stopCluster(cluster)
})
