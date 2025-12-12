test_that("`auto_vif()` works", {
  data(plant_richness_df)
  out <- auto_vif(
    x = plant_richness_df[, 5:21],
    verbose = FALSE,
    preference.order = colnames(plant_richness_df[5:21])
  )
  expect_type(out, "list")
  expect_length(out, 3)
  expect_s3_class(out$vif, "data.frame")
  expect_named(out, c("vif", "selected.variables", "selected.variables.df"))
})
