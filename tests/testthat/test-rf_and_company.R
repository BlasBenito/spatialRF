testthat::test_that("`rf() and company` works", {

  #description
  #this file tests the functions
  #rf()
    #case_weights()
    #plot_importance()
    #auc()
    #roc_curve()
    #root_mean_squared_error()
    #moran_multithreshold()
      #moran()
      #default_distance_thresholds()
    #residuals_diagnostics()


  #loading libraries
  library(spatialRF)
  library(magrittr)

  #loading data
  data(
    ecoregions_df,
    ecoregions_distance_matrix,
    ecoregions_predictor_variable_names,
    ecoregions_dependent_variable_name
  )

  #creating binary response
  ecoregions_df$binary_response <- ifelse(
    ecoregions_df$plant_richness > 5000,
    1,
    0
  )

  #CONTINUOUS RESPONSE
  ####################

  #ARGUMENTS FOR DEBUGGING
  model = NULL
  data = tibble::as_tibble(ecoregions_df)
  dependent.variable.name = ecoregions_dependent_variable_name
  predictor.variable.names = ecoregions_predictor_variable_names
  distance.matrix = ecoregions_distance_matrix
  distance.thresholds = NULL
  xy = ecoregions_df[, c("x", "y")]
  ranger.arguments = NULL
  scaled.importance = FALSE
  seed = 1
  verbose = FALSE
  n.cores = parallel::detectCores() - 1
  cluster = NULL

  #model
  model <- rf(
    data = tibble::as_tibble(ecoregions_df),
    dependent.variable.name = ecoregions_dependent_variable_name,
    predictor.variable.names = ecoregions_predictor_variable_names,
    distance.matrix = ecoregions_distance_matrix,
    distance.thresholds = NULL,
    xy = ecoregions_df[, c("x", "y")],
    verbose = FALSE
  )


  #BINARY RESPONSE
  ####################
  model = NULL
  data = tibble::as_tibble(ecoregions_df)
  dependent.variable.name = "binary_response"
  predictor.variable.names = ecoregions_predictor_variable_names
  distance.matrix = ecoregions_distance_matrix
  distance.thresholds = NULL
  xy = ecoregions_df[, c("x", "y")]
  ranger.arguments = NULL
  scaled.importance = FALSE
  seed = 1
  verbose = FALSE
  n.cores = parallel::detectCores() - 1
  cluster = NULL


  model <- rf(
    data = tibble::as_tibble(ecoregions_df),
    dependent.variable.name = "binary_response",
    predictor.variable.names = ecoregions_predictor_variable_names,
    distance.matrix = ecoregions_distance_matrix,
    distance.thresholds = NULL,
    xy = ecoregions_df[, c("x", "y")],
    verbose = FALSE
  )


})
