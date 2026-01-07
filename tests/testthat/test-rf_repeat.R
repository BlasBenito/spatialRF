test_that("`rf_repeat()` works", {
  data(plants_rf)
  data(plants_distance)
  data(plants_predictors)

  out <- rf_repeat(
    data = plants_df,
    dependent.variable.name = "richness_species_vascular",
    predictor.variable.names = plants_predictors,
    distance.matrix = plants_distance,
    distance.thresholds = c(100, 1000, 2000),
    repetitions = 5,
    verbose = FALSE
  )

  invisible(gc())

  expect_s3_class(out, "rf_repeat")
  expect_s3_class(out$importance$per.variable, "data.frame")
  expect_s3_class(out$residuals$autocorrelation$per.distance, "data.frame")
  expect_named(
    out$residuals$autocorrelation$per.distance,
    c("distance.threshold", "moran.i", "p.value", "interpretation")
  )
})
