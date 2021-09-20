test_that("`the_feature_engineer()` works", {
  data(plant_richness_df)
  interactions <- the_feature_engineer(
    data = plant_richness_df,
    dependent.variable.name = "richness_species_vascular",
    predictor.variable.names = colnames(plant_richness_df)[5:21],
    xy = plant_richness_df[, c("x", "y")],
    verbose = FALSE,
    seed = 100,
    n.cores = 7
    )
  expect_s3_class(interactions$screening, "data.frame")
  expect_s3_class(interactions$selected, "data.frame")
})
