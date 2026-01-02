test_that("`get_residuals()` works", {
  data(plants_df)
  rf.model <- rf(
    data = plants_df,
    dependent.variable.name = "richness_species_vascular",
    predictor.variable.names = colnames(plants_df)[5:21],
    verbose = FALSE
  )
  x <- get_residuals(rf.model)
  expect_equal(length(x), nrow(plants_df))
})
