test_that("`rf_tuning()` works", {
  data(plant_richness_df)
  tuning <- rf_tuning(
    data = plant_richness_df,
    dependent.variable.name = "richness_species_vascular",
    predictor.variable.names = colnames(plant_richness_df)[5:21],
    method = "oob",
    n.cores = 1,
    verbose = FALSE)
  expect_s3_class(tuning$tuning$tuning.df, "data.frame")
  expect_type(tuning$tuning, "list")
})
