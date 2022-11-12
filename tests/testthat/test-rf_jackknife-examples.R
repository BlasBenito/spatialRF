testthat::test_that("`rf_repeat()` works", {

  library(spatialRF)
  library(magrittr)

  #loading example data
  data(
    ecoregions_df,
    ecoregions_distance_matrix,
    ecoregions_predictor_variable_names,
    ecoregions_dependent_variable_name
    )

  #fitting random forest model
  model <- rf(
    data = tibble::as_tibble(ecoregions_df),
    dependent.variable.name = ecoregions_dependent_variable_name,
    predictor.variable.names = ecoregions_predictor_variable_names[1:5],
    distance.matrix = ecoregions_distance_matrix,
    distance.thresholds = 0,
    xy = ecoregions_df[, c("x", "y")],
    n.cores = 1,
    verbose = FALSE
  )

  cluster <- start_cluster()

  #computing predictor contribution to model transferability
  model <- rf_jackknife(
    model = model,
    cluster = cluster,
    verbose = TRUE
    )

  testthat::expect_s3_class(
    model$jackknife$r.squared$df,
    "data.frame"
  )

  testthat::expect_s3_class(
    model$jackknife$r.squared$plot,
    "ggplot"
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

  #fitting random forest model
  model <- rf(
    data = ecoregions_df,
    dependent.variable.name = ecoregions_dependent_variable_name,
    predictor.variable.names = ecoregions_predictor_variable_names[1:5],
    distance.matrix = ecoregions_distance_matrix,
    distance.thresholds = 0,
    xy = ecoregions_df[, c("x", "y")],
    n.cores = 1,
    verbose = FALSE
  )

  #computing predictor contribution to model transferability
  model <- rf_jackknife(
    model = model,
    cluster = cluster,
    verbose = TRUE
  )

  stop_cluster()

  testthat::expect_s3_class(
    model$jackknife$r.squared$df,
    "data.frame"
  )

  testthat::expect_s3_class(
    model$jackknife$r.squared$plot,
    "ggplot"
  )




})
