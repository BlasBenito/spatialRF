test_that("`rf_interactions()` works", {
  data(plant_richness_df)
  interactions <- rf_interactions(
    data = plant_richness_df,
    dependent.variable.name = "richness_species_vascular",
    predictor.variable.names = colnames(plant_richness_df)[5:21],
    verbose = FALSE,
    seed = 100,
    n.cores = 1
    )
  expect_s3_class(interactions$screening, "data.frame")
  expect_s3_class(interactions$selected, "data.frame")
  expect_s3_class(interactions$columns, "data.frame")
})
