test_that("`rf_tuning()` works", {
  data(plants_df)

  out <- rf(
    data = plants_df,
    dependent.variable.name = "richness_species_vascular",
    predictor.variable.names = colnames(plants_df)[5:21],
    verbose = FALSE
  )

  out <- rf_tuning(
    model = out,
    xy = plants_df[, c("x", "y")],
    num.trees = c(100),
    mtry = c(2),
    min.node.size = c(5, 10),
    verbose = FALSE
  )

  expect_s3_class(out$tuning$tuning.df, "data.frame")
  expect_type(out$tuning, "list")
})
