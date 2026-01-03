test_that("`rf_spatial()` works", {
  data(plants_distance)
  data(plants_df)

  # Test 1: hengl method (fastest, good for comprehensive testing)

  out <- rf_spatial(
    data = plants_df,
    dependent.variable.name = "richness_species_vascular",
    predictor.variable.names = plants_predictors,
    distance.matrix = plants_distance,
    distance.thresholds = 100, # Single threshold for speed
    method = "hengl",
    verbose = FALSE
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
    data = plants_df,
    dependent.variable.name = "richness_species_vascular",
    predictor.variable.names = plants_predictors,
    distance.matrix = plants_distance,
    distance.thresholds = 100,
    method = "mem.moran.sequential",
    verbose = FALSE
  )

  # Basic checks to ensure method works
  expect_equal(inherits(out2, "rf_spatial"), TRUE)
  expect_true(!is.null(out2$spatial))
  expect_true(length(out2$spatial$names) > 0)
  expect_s3_class(out2$spatial$optimization, "data.frame")
})
