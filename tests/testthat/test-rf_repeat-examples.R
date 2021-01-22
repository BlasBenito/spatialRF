test_that("`rf_repeat()` works", {
  data("plant_richness_df")
  data("distance_matrix")
  out <- rf_repeat(
    data = plant_richness_df, dependent.variable.name = "richness_species_vascular",
    predictor.variable.names = colnames(plant_richness_df)[5:21],
    distance.matrix = distance_matrix, distance.thresholds = c(0,100, 1000, 10000),
    repetitions = 10,
    n.cores = 1
  )
  expect_s3_class(out, "ranger")
  expect_s3_class(out$variable.importance$df, "data.frame")
  expect_named(out$variable.importance$df, c("variable", "importance", "standard_deviation"))
  expect_s3_class(out$spatial.correlation.residuals$df, "data.frame")
  expect_named(out$spatial.correlation.residuals$df, c("distance.threshold", "moran.i", "p.value", "interpretation"))
})
