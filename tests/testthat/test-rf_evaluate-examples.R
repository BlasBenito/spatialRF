testthat::test_that("`rf_evaluate()` works", {

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

  cluster <- spatialRF::start_cluster()

  #continuous response
  rf.model <- rf(
    data = ecoregions_df,
    dependent.variable.name = ecoregions_dependent_variable_name,
    predictor.variable.names = ecoregions_predictor_variable_names,
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

  print_performance(rf.model)

  rf.model <- rf_repeat(rf.model)

  print_performance(rf.model)

  rf.model <- rf_evaluate(
    model = rf.model,
    xy = NULL,
    repetitions = 30,
    training.fraction = 0.75,
    metrics = c(
        "auc",
        "r.squared",
        "rmse",
        "nrmse"
      ),
    distance.step = NULL,
    distance.step.x = NULL,
    distance.step.y = NULL,
    grow.testing.folds = FALSE,
    seed = 1,
    verbose = TRUE,
    n.cores = parallel::detectCores() - 1,
    cluster = NULL
    )

  print_performance(rf.model)

  testthat::expect_s3_class(
    rf.model,
    "rf_evaluate"
    )

  testthat::expect_type(
    rf.model$evaluation,
    "list"
    )

  testthat::expect_s3_class(
    rf.model$evaluation$per.fold,
    "data.frame"
    )

  testthat::expect_s3_class(
    rf.model$evaluation$per.model,
    "data.frame"
    )

  testthat::expect_s3_class(
    rf.model$evaluation$aggregated,
    "data.frame"
    )


  #binary response
  ecoregions_df <- ecoregions_df %>%
    dplyr::mutate(
      plant_richness = ifelse(
        plant_richness > 5000,
        1,
        0
      )
    )

  metrics <- c(
    "r.squared",
    "rmse",
    "auc",
    "nrmse"
  )

  rf.model <- rf(
    data = ecoregions_df,
    dependent.variable.name = ecoregions_dependent_variable_name,
    predictor.variable.names = ecoregions_predictor_variable_names,
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
      metrics = metrics,
      verbose = FALSE
    )

  spatialRF::stop_cluster()

  testthat::expect_s3_class(
    rf.model,
    "rf_evaluate"
  )

  testthat::expect_type(
    rf.model$evaluation,
    "list"
  )

  testthat::expect_s3_class(
    rf.model$evaluation$per.fold,
    "data.frame"
  )

  testthat::expect_s3_class(
    rf.model$evaluation$per.model,
    "data.frame"
  )

  testthat::expect_s3_class(
    rf.model$evaluation$aggregated,
    "data.frame"
  )

})
