test_that("`rf_spatial()` works", {
  data(distance_matrix)
  data(plant_richness_df)
  out <- rf_spatial(
    data = plant_richness_df,
    dependent.variable.name = "richness_species_vascular",
    predictor.variable.names = colnames(plant_richness_df)[5:21],
    distance.matrix = distance_matrix,
    distance.thresholds = c(0, 500, 1000),
    method = "hengl",
    repetitions = 5,
    n.cores = 1,
    verbose = FALSE
  )
  expect_equal(inherits(out, "rf_spatial"), TRUE)
  expect_equal(inherits(out$variable.importance$plot, "ggplot"), TRUE)
  expect_equal(inherits(out$spatial.correlation.residuals$plot, "ggplot"), TRUE)
  expect_equal(length(out$performance$rmse), 5)
  expect_equal(length(out$performance$nrmse), 5)
  expect_equal(length(out$performance$r.squared), 5)
  expect_equal(length(out$performance$pseudo.r.squared), 5)
})
