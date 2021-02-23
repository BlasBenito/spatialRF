test_that("`rf_tuning()` works", {
  data(plant_richness_df)

  out <- rf(
    data = plant_richness_df,
    dependent.variable.name = "richness_species_vascular",
    predictor.variable.names = colnames(plant_richness_df)[5:21],
    verbose = FALSE
  )

  out <- rf_tuning(
    model = out,
    method = "oob",
    n.cores = 1,
    verbose = FALSE)
  expect_s3_class(out$tuning$tuning.df, "data.frame")
  expect_type(out$tuning, "list")
})
