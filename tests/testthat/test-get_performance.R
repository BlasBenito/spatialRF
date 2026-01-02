test_that("`get_performance()` works", {
  data(plants_df)
  data(plants_distance)
  rf.model <- rf(
    data = plants_df,
    dependent.variable.name = "richness_species_vascular",
    predictor.variable.names = colnames(plants_df)[5:21],
    distance.matrix = plants_distance,
    distance.thresholds = c(0, 1000, 2000),
    verbose = FALSE
  )
  x <- get_performance(rf.model)
  expect_s3_class(x, "data.frame")
  expect_named(x, c("metric", "value"))
})
