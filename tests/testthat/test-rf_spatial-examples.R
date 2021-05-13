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
    n.cores = 7,
    verbose = FALSE
  )
  expect_equal(inherits(out, "rf_spatial"), TRUE)
  expect_equal(inherits(out$importance$per.variable.plot, "ggplot"), TRUE)
  expect_equal(inherits(out$residuals$autocorrelation$plot, "ggplot"), TRUE)
  expect_equal(length(out$performance$rmse), 1)
  expect_equal(length(out$performance$nrmse), 1)
  expect_equal(length(out$performance$r.squared), 1)
  expect_equal(length(out$performance$pseudo.r.squared), 1)

  out <- rf_spatial(
    data = plant_richness_df,
    dependent.variable.name = "richness_species_vascular",
    predictor.variable.names = colnames(plant_richness_df)[5:21],
    distance.matrix = distance_matrix,
    distance.thresholds = c(0, 500, 1000),
    method = "mem.moran.sequential",
    n.cores = 7,
    verbose = FALSE
  )
  expect_equal(inherits(out, "rf_spatial"), TRUE)
  expect_equal(inherits(out$importance$per.variable.plot, "ggplot"), TRUE)
  expect_equal(inherits(out$residuals$autocorrelation$plot, "ggplot"), TRUE)
  expect_equal(length(out$performance$rmse), 1)
  expect_equal(length(out$performance$nrmse), 1)
  expect_equal(length(out$performance$r.squared), 1)
  expect_equal(length(out$performance$pseudo.r.squared), 1)

  out <- rf_spatial(
    data = plant_richness_df,
    dependent.variable.name = "richness_species_vascular",
    predictor.variable.names = colnames(plant_richness_df)[5:21],
    distance.matrix = distance_matrix,
    distance.thresholds = c(0, 500, 1000),
    method = "mem.effect.recursive",
    n.cores = 7,
    verbose = FALSE
  )
  expect_equal(inherits(out, "rf_spatial"), TRUE)
  expect_equal(inherits(out$importance$per.variable.plot, "ggplot"), TRUE)
  expect_equal(inherits(out$residuals$autocorrelation$plot, "ggplot"), TRUE)
  expect_equal(length(out$performance$rmse), 1)
  expect_equal(length(out$performance$nrmse), 1)
  expect_equal(length(out$performance$r.squared), 1)
  expect_equal(length(out$performance$pseudo.r.squared), 1)
})
