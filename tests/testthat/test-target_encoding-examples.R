testthat::test_that("`target_encoding()` works", {


  #loading libraries
  library(spatialRF)
  library(magrittr)

  #loading data
  data(
    ecoregions_df,
    ecoregions_sf,
    ecoregions_tibble,
    ecoregions_all_predictors,
    ecoregions_continuous_response,
    ecoregions_binary_response
  )

  #lists to iterate over
  training <- list(
    df = ecoregions_df,
    sf = ecoregions_sf,
    tibble = ecoregions_tibble
  )

  response <- list(
    continuous = ecoregions_continuous_response,
    binary = ecoregions_binary_response
  )

  predictors <- list(
    numeric = ecoregions_numeric_predictors,
    all = ecoregions_all_predictors
  )

  #iterating over input lists
  for(training.i in names(training)){
    for(response.i in names(response)){
      for(predictors.i in names(predictors)){

        #arguments for debugging
        model = NULL
        data = training[[training.i]]
        dependent.variable.name = response[[response.i]]
        predictor.variable.names = predictors[[predictors.i]]
        distance.matrix = ecoregions_distance_matrix
        distance.thresholds = NULL
        xy = ecoregions_df[, c("x", "y")]
        ranger.arguments = NULL
        scaled.importance = FALSE
        seed = 1
        verbose = FALSE
        n.cores = parallel::detectCores() - 1
        cluster = NULL

        #rf model
        df <- target_encoding(
          data = training[[training.i]],
          dependent.variable.name = response[[response.i]],
          predictor.variable.names = predictors[[predictors.i]]
        )


      }
    }
  }



})
