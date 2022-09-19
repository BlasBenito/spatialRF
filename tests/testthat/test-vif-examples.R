test_that("`vif()` works", {

   library(spatialRF)

   data(
   ecoregions_df,
   ecoregions_predictor_variable_names
   )

  out <- vif(x = ecoregions_df[, ecoregions_predictor_variable_names])

  expect_s3_class(out, "data.frame")
  expect_named(out, c("variable", "vif"))
})
