testthat::test_that("`rf_tuning()` works", {

  library(spatialRF)
  library(magrittr)

  #loading data
  data(
    ecoregions_df,
    ecoregions_distance_matrix,
    ecoregions_predictor_variable_names,
    ecoregions_dependent_variable_name
  )

  ecoregions_df <- tibble::as_tibble(ecoregions_df)

  cluster <- start_cluster()

  out <- rf(
    data = ecoregions_df,
    dependent.variable.name = ecoregions_dependent_variable_name,
    predictor.variable.names = ecoregions_predictor_variable_names,
    distance.matrix = ecoregions_distance_matrix,
    xy = ecoregions_df[, c("x", "y")],
    distance.thresholds = c(0,100, 1000, 10000),
    scaled.importance = FALSE,
    verbose = FALSE
  ) %>%
    rf_tuning(
    cluster = cluster,
    verbose = FALSE
    )

  spatialRF::stop_cluster()

  testthat::expect_type(
    out$tuning,
    "list"
  )

  testthat::expect_s3_class(
    out$tuning$tuning.df,
    "data.frame"
    )

})
