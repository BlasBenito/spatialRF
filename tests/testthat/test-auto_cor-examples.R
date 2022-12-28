testthat::test_that("`auto_cor()` works", {

  library(spatialRF)
  library(magrittr)

  data(
    ecoregions_df,
    ecoregions_all_predictors,
    ecoregions_continuous_response
  )

  ecoregions_df <- tibble::as_tibble(ecoregions_df)

  out <- auto_cor(
    predictor.variable.names = ecoregions_all_predictors,
    data = ecoregions_df,
    dependent.variable.name = ecoregions_continuous_response,
    verbose = FALSE,
    preference.order = ecoregions_all_predictors
  )



  #adding auto_vif()
  out <- auto_cor(
    data = ecoregions_df,
    predictor.variable.names = ecoregions_numeric_predictors,
    verbose = FALSE,
    preference.order = ecoregions_numeric_predictors
  ) %>%
    auto_vif()

  testthat::expect_type(out, "list")
  testthat::expect_length(out, 3)
  testthat::expect_named(out, c("vif", "selected.variables", "selected.variables.df"))

})
