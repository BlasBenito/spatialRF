testthat::test_that("`auto_cor()` works", {

  library(spatialRF)
  library(magrittr)

  data(
    ecoregions_df,
    ecoregions_numeric_predictors
  )

  ecoregions_df <- tibble::as_tibble(ecoregions_df)

  out <- auto_cor(
    data = ecoregions_df,
    predictor.variable.names = ecoregions_numeric_predictors,
    verbose = FALSE,
    preference.order = ecoregions_numeric_predictors[1:10]
  )

  testthat::expect_type(out, "list")
  testthat::expect_length(out, 3)
  testthat::expect_named(out, c("cor", "selected.variables", "selected.variables.df"))

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
