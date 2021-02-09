test_that("`get_residuals()` works", {
  data(plant_richness_df)
  rf.model <- rf(
    data = plant_richness_df, dependent.variable.name = "richness_species_vascular",
    predictor.variable.names = colnames(plant_richness_df)[5:21],
    verbose = FALSE
  )
  x <- get_residuals(x = rf.model)
  expect_s3_class(x, "data.frame")
  expect_named(x, c("residuals"))
})
