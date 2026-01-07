test_that("`get_predictions()` works", {
  data(plants_df)
  data(plants_predictors)
  rf.model <- rf(
    data = plants_df,
    dependent.variable.name = "richness_species_vascular",
    predictor.variable.names = plants_predictors,
    verbose = FALSE
  )
  x <- get_predictions(rf.model)
  expect_equal(length(x), nrow(plants_df))
})
