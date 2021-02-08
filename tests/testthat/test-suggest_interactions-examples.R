test_that("`suggest_interactions()` works", {
  data(plant_richness_df)
  data(distance_matrix)
  rf.model <- rf(
    data = plant_richness_df, dependent.variable.name = "richness_species_vascular",
    predictor.variable.names = colnames(plant_richness_df)[5:21],
    distance.matrix = distance_matrix, distance.thresholds = c(
      0,
      1000, 2000
    ), verbose = FALSE
  )
  interactions <- suggest_interactions(model = rf.model, n.cores = 1)
  expect_type(interactions, "list")
  expect_s3_class(interactions$screening, "data.frame")
  expect_s3_class(interactions$selected, "data.frame")
  expect_s3_class(interactions$columns, "data.frame")
})
