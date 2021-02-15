test_that("`rf()` works", {
  data("plant_richness_df")
  data("distance_matrix")
  out <- rf(
    data = plant_richness_df,
    dependent.variable.name = "richness_species_vascular",
    predictor.variable.names = colnames(plant_richness_df)[5:21],
    distance.matrix = distance_matrix,
    distance.thresholds = c(0,100, 1000, 10000),
    verbose = FALSE
  )
  expect_s3_class(out, "rf")
  expect_s3_class(out$variable.importance$per.variable, "data.frame")
  expect_named(out$variable.importance$per.variable, c("variable", "importance"))
  expect_s3_class(out$spatial.correlation.residuals$per.distance, "data.frame")
  expect_named(out$spatial.correlation.residuals$per.distance, c("distance.threshold", "moran.i", "p.value", "interpretation"))
})
