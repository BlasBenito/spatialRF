test_that("`plot_response_surface()` works", {
  data(plant_richness_df)
  m <- rf(
    data = plant_richness_df, dependent.variable.name = "richness_species_vascular",
    predictor.variable.names = colnames(plant_richness_df)[5:21],
    verbose = FALSE
  )
  p <- plot_response_surface(m)
  expect_s3_class(p, "ggplot")
})
