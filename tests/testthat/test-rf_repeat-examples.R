testthat::test_that("`rf_repeat()` works", {

  library(spatialRF)
  library(magrittr)

  #loading data
  data(
    ecoregions_df,
    ecoregions_distance_matrix,
    ecoregions_predvar_names,
    ecoregions_depvar_name
  )

  #with n.cores
  out <- rf_repeat(
    data = ecoregions_df,
    dependent.variable.name = ecoregions_depvar_name,
    predictor.variable.names = ecoregions_predvar_names,
    distance.matrix = ecoregions_distance_matrix,
    distance.thresholds = c(0,100, 1000, 10000),
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
    dependent.variable.name = ecoregions_depvar_name,
    predictor.variable.names = ecoregions_predvar_names,
    distance.matrix = ecoregions_distance_matrix,
    distance.thresholds = c(0,100, 1000, 10000),
    repetitions = 10,
    cluster = spatialRF::make_cluster(),
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
      x = ecoregions_df,
      verbose = FALSE,
      preference.order = ecoregions_predvar_names
    ) %>%
      auto_vif()
  )

  #fitting model
  out.2 <- rf_repeat(
    data = ecoregions_df,
    dependent.variable.name = ecoregions_depvar_name,
    predictor.variable.names = variable.selection,
    distance.matrix = ecoregions_distance_matrix,
    distance.thresholds = c(0,100, 1000, 10000),
    repetitions = 10,
    verbose = FALSE,
    cluster = spatialRF::make_cluster()
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
