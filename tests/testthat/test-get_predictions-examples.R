test_that("`get_predictions()` works", {
  data(plants_df)
  rf.model <- rf(
    data = plants_df,
    dependent.variable.name = "richness_species_vascular",
    predictor.variable.names = colnames(plants_df)[5:21],
    verbose = FALSE
  )
  x <- get_predictions(rf.model)
  expect_equal(length(x), nrow(plants_df))
})
