testthat::test_that("mc_ functions work", {

  library(spatialRF)
  library(magrittr)

  data(
    ecoregions_df,
    ecoregions_tibble,
    ecoregions_sf,
    ecoregions_continuous_response,
    ecoregions_all_predictors
  )

  #mc_auto
  ###############################
  selected.predictors <- mc_auto(
    data = ecoregions_df,
    predictor.variable.names = ecoregions_all_predictors,
    dependent.variable.name = ecoregions_continuous_response,
    preference.order = ecoregions_all_predictors[1:5],
    max.cor = 0.75,
    max.vif = 5,
    verbose = TRUE
    )

  testthat::expect_equal(
    is.vector(selected.predictors),
    TRUE
  )

  test.vif <- mc_vif(
    data = ecoregions_df,
    predictor.variable.names = selected.predictors,
    dependent.variable.name = ecoregions_continuous_response
  )

  testthat::expect_equal(
    max(test.vif$vif) <= 5,
    TRUE
  )

  testthat::expect_equal(
    names(test.vif),
    c("variable", "vif")
  )

  test.cor <- mc_cor(
    data = ecoregions_tibble,
    predictor.variable.names = selected.predictors,
    dependent.variable.name = ecoregions_continuous_response
  )

  testthat::expect_equal(
    names(test.cor),
    c("a", "b", "cor")
  )

  testthat::expect_equal(
    max(test.cor$cor) <= 0.75,
    TRUE
  )

  #passing non-collinear variables
  selected.predictors <- mc_auto(
    data = ecoregions_df,
    predictor.variable.names = selected.predictors,
    dependent.variable.name = ecoregions_continuous_response,
    max.cor = 0.75,
    max.vif = 5,
    verbose = TRUE
  )

  testthat::expect_equal(
    is.vector(selected.predictors),
    TRUE
  )

  #passing NULL predictors
  selected.predictors <- mc_auto(
    data = ecoregions_sf[, ecoregions_all_predictors],
    predictor.variable.names = NULL,
    dependent.variable.name = NULL,
    max.cor = 0.75,
    max.vif = 5,
    verbose = TRUE
  )

  testthat::expect_equal(
    is.vector(selected.predictors),
    TRUE
  )

  #passing NULL dependent variable name
  selected.predictors <- mc_auto(
    data = ecoregions_sf,
    predictor.variable.names = ecoregions_all_predictors,
    dependent.variable.name = NULL,
    preference.order = NULL,
    max.cor = 0.75,
    max.vif = 5,
    verbose = TRUE
  )

  testthat::expect_equal(
    is.vector(selected.predictors),
    TRUE
  )

  #passing zero variance column
  ecoregions_df$zeros <- 0

  selected.predictors <- mc_auto(
    data = ecoregions_sf,
    predictor.variable.names = NULL,
    dependent.variable.name = NULL,
    max.cor = 0.75,
    max.vif = 5,
    verbose = TRUE
  )

  testthat::expect_equal(
    is.vector(selected.predictors),
    TRUE
  )

  #mc_cor
  #################################
  #with tibble
  test.cor <- mc_cor(
    data = ecoregions_tibble,
    predictor.variable.names = selected.predictors,
    dependent.variable.name = ecoregions_continuous_response
  )

  testthat::expect_equal(
    tibble::is_tibble(test.cor),
    TRUE
  )

  #with sf
  test.cor <- mc_cor(
    data = ecoregions_sf,
    predictor.variable.names = selected.predictors,
    dependent.variable.name = ecoregions_continuous_response
  )

  testthat::expect_equal(
    is.data.frame(test.vif),
    TRUE
  )

  #with NULL dependent variable name
  test.cor <- mc_cor(
    data = ecoregions_sf,
    predictor.variable.names = ecoregions_numeric_predictors,
    dependent.variable.name = NULL
  )

  testthat::expect_equal(
    names(test.cor),
    c("a", "b", "cor")
  )

  #without data
  testthat::expect_error(
  test.cor <- mc_cor(
    data = NULL,
    predictor.variable.names = ecoregions_numeric_predictors,
    dependent.variable.name = NULL
  )
  )

  #with zero variance column
  ecoregions_df$zeros <- rep(0, nrow(ecoregions_df))

  predictors <- c(ecoregions_numeric_predictors, "zeros")

  test.cor <- mc_cor(
    data = ecoregions_sf,
    predictor.variable.names = predictors,
    dependent.variable.name = NULL
  )

  testthat::expect_equal(
    names(test.cor),
    c("a", "b", "cor")
  )

  #without predictors
  test.cor <- mc_cor(
    data = ecoregions_sf,
    predictor.variable.names = NULL,
    dependent.variable.name = ecoregions_continuous_response
  )

  testthat::expect_equal(
    names(test.cor),
    c("a", "b", "cor")
  )

  #without predictors or responses
  test.cor <- mc_cor(
    data = ecoregions_sf[, ecoregions_numeric_predictors],
    predictor.variable.names = NULL,
    dependent.variable.name = NULL
  )

  testthat::expect_equal(
    names(test.cor),
    c("a", "b", "cor")
  )

  #mc_vif
  #####################################
  #with tibble
  test.vif <- mc_vif(
    data = ecoregions_tibble,
    predictor.variable.names = selected.predictors,
    dependent.variable.name = ecoregions_continuous_response
  )

  testthat::expect_equal(
    tibble::is_tibble(test.vif),
    TRUE
  )

  testthat::expect_equal(
    max(test.vif$vif) <= 5,
    TRUE
  )

  #with sf
  test.vif <- mc_vif(
    data = ecoregions_sf,
    predictor.variable.names = selected.predictors,
    dependent.variable.name = ecoregions_continuous_response
  )

  testthat::expect_equal(
    max(test.vif$vif) <= 5,
    TRUE
  )

  #with NULL dependent variable name
  test.vif <- mc_vif(
    data = ecoregions_sf,
    predictor.variable.names = ecoregions_numeric_predictors,
    dependent.variable.name = NULL
  )

  testthat::expect_equal(
    names(test.vif),
    c("variable", "vif")
  )

  #without data
  testthat::expect_error(
    test.cor <- mc_vif(
      data = NULL,
      predictor.variable.names = ecoregions_numeric_predictors,
      dependent.variable.name = NULL
    )
  )

  #with zero variance column
  ecoregions_df$zeros <- rep(0, nrow(ecoregions_df))

  predictors <- c(ecoregions_numeric_predictors, "zeros")

  test.vif <- mc_vif(
    data = ecoregions_sf,
    predictor.variable.names = predictors,
    dependent.variable.name = NULL
  )

  testthat::expect_equal(
    names(test.vif),
    c("variable", "vif")
  )

  #without predictors
  test.vif <- mc_vif(
    data = ecoregions_sf,
    predictor.variable.names = NULL,
    dependent.variable.name = NULL
  )

  testthat::expect_equal(
    names(test.vif),
    c("variable", "vif")
  )

  #without predictors or responses
  test.vif <- mc_vif(
    data = ecoregions_sf[, ecoregions_numeric_predictors],
    predictor.variable.names = NULL,
    dependent.variable.name = NULL
  )

  testthat::expect_equal(
    names(test.vif),
    c("variable", "vif")
  )

  test.vif <- mc_vif(
    data = ecoregions_sf,
    predictor.variable.names = NULL,
    dependent.variable.name = ecoregions_continuous_response
  )

  testthat::expect_equal(
    names(test.vif),
    c("variable", "vif")
  )

  #mc_auto_cor
  #############################

})
