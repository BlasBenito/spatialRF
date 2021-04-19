test_that("`get_performance()` works", {
  data(plant_richness_df)
  data(distance_matrix)
  rf.model <- rf(
    data = plant_richness_df,
    dependent.variable.name = "richness_species_vascular",
    predictor.variable.names = colnames(plant_richness_df)[5:21],
    distance.matrix = distance_matrix,
    distance.thresholds = c(0,1000, 2000),
    verbose = FALSE
  )
  x <- get_performance(rf.model)
  expect_s3_class(x, "data.frame")
  expect_equal(nrow(x), 6)
  expect_equal(ncol(x), 4)
  expect_named(x, c("metric", "mean", "standard_error", "standard_deviation"))
})
