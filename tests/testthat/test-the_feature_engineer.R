test_that("`the_feature_engineer()` works", {
  data(plants_df)
  data(plants_predictors)
  data(plants_xy)

  interactions <- the_feature_engineer(
    data = plants_df,
    dependent.variable.name = "richness_species_vascular",
    predictor.variable.names = c(
      "human_population",
      "bias_area_km2",
      "climate_bio1_average",
      "human_population_density"
    ),
    xy = plants_xy,
    importance.threshold = 1000,
    verbose = FALSE,
    seed = 100
  )

  expect_s3_class(interactions$screening, "data.frame")
  expect_s3_class(interactions$selected, "data.frame")
})
