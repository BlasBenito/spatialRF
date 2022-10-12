testthat::test_that("`auto_vif()` works", {

  library(spatialRF)
  library(magrittr)

  data(
    ecoregions_df,
    ecoregions_predictor_variable_names
  )

  ecoregions_df <- tibble::as_tibble(ecoregions_df)

  out <- auto_vif(
    data = ecoregions_df,
    predictor.variable.names = ecoregions_predictor_variable_names,
    verbose = TRUE,
    preference.order = ecoregions_predictor_variable_names[1:10]
  )

  testthat::expect_type(out, "list")
  testthat::expect_length(out, 3)
  testthat::expect_s3_class(out$vif, "data.frame")
  testthat::expect_named(out, c("vif", "selected.variables", "selected.variables.df"))

})
