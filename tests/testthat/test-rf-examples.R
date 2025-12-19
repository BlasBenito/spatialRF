test_that("`rf()` works", {
  data("plants_df")
  data("plants_distance")
  out <- rf(
    data = plants_df,
    dependent.variable.name = "richness_species_vascular",
    predictor.variable.names = colnames(plants_df)[5:21],
    distance.matrix = plants_distance,
    distance.thresholds = c(0, 100, 1000, 10000),
    verbose = FALSE
  )
  expect_s3_class(out, "rf")
  expect_s3_class(out$importance$per.variable, "data.frame")
  expect_named(out$importance$per.variable, c("variable", "importance"))
  expect_s3_class(out$residuals$autocorrelation$per.distance, "data.frame")
  expect_named(
    out$residuals$autocorrelation$per.distance,
    c(
      "distance.threshold",
      "moran.i",
      "moran.i.null",
      "p.value",
      "interpretation"
    )
  )
})
