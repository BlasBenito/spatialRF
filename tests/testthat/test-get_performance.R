test_that("`get_performance()` works", {
  data(plants_df)
  data(plants_distance)
  data(plants_predictors)
  rf.model <- rf(
    data = plants_df,
    dependent.variable.name = "richness_species_vascular",
    predictor.variable.names = plants_predictors,
    distance.matrix = plants_distance,
    distance.thresholds = c(100, 1000, 2000),
    verbose = FALSE
  )
  x <- get_performance(rf.model)
  expect_s3_class(x, "data.frame")
  expect_named(x, c("metric", "value"))
})
