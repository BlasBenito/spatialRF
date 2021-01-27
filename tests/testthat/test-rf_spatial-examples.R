test_that("`rf_spatial()` works", {
  data("distance_matrix")
  data("plant_richness_df")
  data <- plant_richness_df
  dependent.variable.name <- "richness_species_vascular"
  predictor.variable.names <- colnames(plant_richness_df)[5:21]
  distance.matrix <- distance_matrix
  distance.thresholds <- c(0, 500, 1000)
  m1 <- rf_spatial(
    data = data,
    dependent.variable.name = dependent.variable.name,
    predictor.variable.names = predictor.variable.names,
    distance.matrix = distance.matrix,
    distance.thresholds = distance.thresholds,
    method = "hengl",
    repetitions = 5,
    n.cores = 1
  )
  m1$variable.importance$plot
  expect_equal(inherits(m1, "ranger"), TRUE)
  expect_equal(inherits(m1$variable.importance$plot, "ggplot"), TRUE)
  expect_equal(inherits(m1$spatial.correlation.residuals$plot, "ggplot"), TRUE)
  expect_equal(length(m1$rmse), 5)
  expect_equal(length(m1$nrmse), 5)
  expect_equal(length(m1$rmse), 5)
  expect_equal(length(m1$r.squared), 5)
  expect_equal(length(m1$pseudo.r.squared), 5)
  # m2 <- rf_spatial(
  #   data = data,
  #   dependent.variable.name = dependent.variable.name,
  #   predictor.variable.names = predictor.variable.names,
  #   distance.matrix = distance.matrix,
  #   distance.thresholds = distance.thresholds,
  #   method = "mem.moran.sequential",
  #   n.cores = 1
  # )
  # expect_equal(inherits(m2, "ranger"), TRUE)
  # expect_equal(inherits(m2$variable.importance$plot, "ggplot"), TRUE)
  # expect_equal(inherits(m2$spatial.correlation.residuals$plot, "ggplot"), TRUE)
  # expect_equal(inherits(m2$selection.spatial.predictors$plot, "ggplot"), TRUE)
  # expect_equal(length(m2$rmse), 1)
  # expect_equal(length(m2$nrmse), 1)
  # expect_equal(length(m2$rmse), 1)
  # expect_equal(length(m2$r.squared), 1)
  # expect_equal(length(m2$pseudo.r.squared), 1)
})
