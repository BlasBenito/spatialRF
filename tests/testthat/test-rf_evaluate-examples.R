testthat::test_that("`rf_evaluate()` works", {

  library(spatialRF)
  library(magrittr)

  #loading data
  data(
    ecoregions_df,
    ecoregions_distance_matrix,
    ecoregions_numeric_predictors,
    ecoregions_continuous_response
  )

  cluster <- start_cluster()

  #with tibble
  model <- rf(
    data = tibble::as_tibble(ecoregions_df),
    dependent.variable.name = ecoregions_continuous_response,
    predictor.variable.names = ecoregions_numeric_predictors,
    distance.matrix = ecoregions_distance_matrix,
    distance.thresholds = c(
      0,
      1000,
      2000
    ),
    xy = ecoregions_df[,c("x", "y")],
    cluster = cluster,
    verbose = FALSE
  )

  model <- rf_evaluate(
    model = model,
    xy = NULL,
    repetitions = 30,
    training.fraction = 0.75,
    distance.step = NULL,
    swap.spatial.folds = FALSE,
    seed = 1,
    verbose = FALSE,
    n.cores = parallel::detectCores() - 1,
    cluster = NULL
    )

  testthat::expect_s3_class(
    model,
    "rf_evaluate"
  )

  testthat::expect_type(
    model$evaluation,
    "list"
  )

  testthat::expect_equal(
    object = "tbl" %in% class(model$evaluation$per.fold),
    expected = TRUE
  )

  testthat::expect_equal(
    object = "tbl" %in% class(model$evaluation$aggregated),
    expected = TRUE
  )

  #plotting


  #binary response
  ########################################
  ecoregions_df <- ecoregions_df %>%
    dplyr::mutate(
      plant_richness = ifelse(
        plant_richness > 5000,
        1,
        0
      )
    )

  model <- rf(
    data = ecoregions_df,
    dependent.variable.name = ecoregions_continuous_response,
    predictor.variable.names = ecoregions_numeric_predictors,
    distance.matrix = ecoregions_distance_matrix,
    distance.thresholds = c(
      0,
      1000,
      2000
    ),
    xy = ecoregions_df[,c("x", "y")],
    cluster = cluster,
    verbose = FALSE
  ) %>%
    rf_evaluate(
      verbose = FALSE
    )

  spatialRF::stop_cluster()

  testthat::expect_s3_class(
    model,
    "rf_evaluate"
  )

  testthat::expect_type(
    model$evaluation,
    "list"
  )

  testthat::expect_s3_class(
    model$evaluation$per.fold,
    "data.frame"
  )

  testthat::expect_s3_class(
    model$evaluation$aggregated,
    "data.frame"
  )

})
