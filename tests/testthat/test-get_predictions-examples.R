test_that("`get_predictions()` works", {
  data(plant_richness_df)
  rf.model <- rf(
    data = plant_richness_df, dependent.variable.name = "richness_species_vascular",
    predictor.variable.names = colnames(plant_richness_df)[5:21],
    verbose = FALSE
  )
  x <- get_predictions(rf.model)
  expect_equal(length(x), nrow(plant_richness_df))
})
