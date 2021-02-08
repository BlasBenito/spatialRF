test_that("`response_surface()` works", {
  data(plant_richness_df)
  m <- rf(
    data = plant_richness_df, dependent.variable.name = "richness_species_vascular",
    predictor.variable.names = colnames(plant_richness_df)[5:21],
    verbose = FALSE
  )
  p <- response_surface(model = m)
  expect_type(p, "list")
  expect_length(p, 3)
  expect_s3_class(p[[1]], "ggplot")
})
