testthat::test_that("`rf_repeat()` works", {

  library(spatialRF)
  library(magrittr)

  #loading data
  data(
    ecoregions_df,
    ecoregions_distance_matrix,
    ecoregions_numeric_predictors,
    ecoregions_continuous_response
  )

  #from model with tibble
  out <- rf(
    data = tibble::as_tibble(ecoregions_df),
    dependent.variable.name = ecoregions_continuous_response,
    predictor.variable.names = ecoregions_numeric_predictors,
    distance.matrix = ecoregions_distance_matrix,
    distance.thresholds = c(0,100, 1000, 10000),
    xy = ecoregions_df[, c("x", "y")],
    verbose = FALSE
  ) %>%
    rf_repeat(verbose = FALSE)

  testthat::expect_equal(
    object = "tbl" %in% class(out$variable.importance.local),
    expected = TRUE
  )

  testthat::expect_equal(
    object = "tbl" %in% class(out$predictions$oob.repetitions),
    expected = TRUE
  )

  testthat::expect_equal(
    object = "tbl" %in% class(out$residuals$autocorrelation$per.repetition),
    expected = TRUE
  )



  #with n.cores
  out <- rf_repeat(
    data = ecoregions_df,
    dependent.variable.name = ecoregions_continuous_response,
    predictor.variable.names = ecoregions_numeric_predictors,
    distance.matrix = ecoregions_distance_matrix,
    distance.thresholds = c(0,100, 1000, 10000),
    scaled.importance = FALSE,
    repetitions = 10,
    verbose = FALSE
  )

  testthat::expect_true(
    out$ranger.arguments$repetitions == 10
  )

  testthat::expect_s3_class(
    out,
    "rf_repeat"
    )

  testthat::expect_s3_class(
    out$importance$per.variable,
    "data.frame"
    )

  testthat::expect_s3_class(
    out$residuals$autocorrelation$per.distance,
    "data.frame"
    )

  testthat::expect_named(
    out$residuals$autocorrelation$per.distance,
    c("distance.threshold", "moran.i", "p.value", "interpretation")
    )

  #with cluster
  out.1 <- rf_repeat(
    data = ecoregions_df,
    dependent.variable.name = ecoregions_continuous_response,
    predictor.variable.names = ecoregions_numeric_predictors,
    distance.matrix = ecoregions_distance_matrix,
    distance.thresholds = c(0,100, 1000, 10000),
    repetitions = 10,
    cluster = spatialRF::start_cluster(),
    verbose = FALSE
  )

  spatialRF::stop_cluster()

  testthat::expect_true(
    out.1$ranger.arguments$repetitions == 10
  )

  testthat::expect_s3_class(
    out.1,
    "rf_repeat"
  )

  testthat::expect_s3_class(
    out.1$importance$per.variable,
    "data.frame"
  )

  testthat::expect_s3_class(
    out.1$residuals$autocorrelation$per.distance,
    "data.frame"
  )

  testthat::expect_named(
    out.1$residuals$autocorrelation$per.distance,
    c("distance.threshold", "moran.i", "p.value", "interpretation")
  )

  #using output of auto_vif and auto_cor as predictor.variable.names

  testthat::expect_warning(
    variable.selection <- auto_cor(
      data = ecoregions_df,
      verbose = FALSE,
      preference.order = ecoregions_numeric_predictors
    ) %>%
      auto_vif()
  )

  #fitting model
  out.2 <- rf_repeat(
    data = ecoregions_df,
    dependent.variable.name = ecoregions_continuous_response,
    predictor.variable.names = variable.selection,
    distance.matrix = ecoregions_distance_matrix,
    distance.thresholds = c(0,100, 1000, 10000),
    repetitions = 10,
    verbose = FALSE,
    cluster = spatialRF::start_cluster()
  )

  spatialRF::stop_cluster()

  testthat::expect_true(
    out.2$ranger.arguments$repetitions == 10
  )

  testthat::expect_s3_class(
    out.2,
    "rf_repeat"
  )

  testthat::expect_s3_class(
    out.2$importance$per.variable,
    "data.frame"
  )

  testthat::expect_s3_class(
    out.2$residuals$autocorrelation$per.distance,
    "data.frame"
  )

  testthat::expect_named(
    out.2$residuals$autocorrelation$per.distance,
    c("distance.threshold", "moran.i", "p.value", "interpretation")
  )

})
