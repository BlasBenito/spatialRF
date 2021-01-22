test_that("`vif()` works", {
  data(plant_richness_df)
  out <- vif(plant_richness_df[, 5:21])
  expect_s3_class(out, "data.frame")
  expect_named(out, c("variable", "vif"))
})
