testthat::test_that("`target_encoding()` works", {


  #loading libraries
  library(spatialRF)
  library(magrittr)
  library(sf)
  library(dplyr)

  #loading data
  data(
    ecoregions_df,
    ecoregions_sf,
    ecoregions_tibble,
    ecoregions_all_predictors,
    ecoregions_continuous_response,
    ecoregions_binary_response
  )

  #TESTING NOISE
  ###################################
  test.1 <- target_encoding(
    data = ecoregions_df,
    dependent.variable.name = ecoregions_continuous_response,
    predictor.variable.names = ecoregions_all_predictors,
    method = "mean",
    noise = 0,
    seed = 100
  )

  test.2 <- target_encoding(
    data = ecoregions_df,
    dependent.variable.name = ecoregions_continuous_response,
    predictor.variable.names = ecoregions_all_predictors,
    method = "mean",
    noise = 0.01,
    seed = 10
  )

  test.3 <- target_encoding(
    data = ecoregions_df,
    dependent.variable.name = ecoregions_continuous_response,
    predictor.variable.names = ecoregions_all_predictors,
    method = "mean",
    noise = 0.01,
    seed = 100
  )

  testthat::expect_false(
    all(test.1$encoding_map$primary_productivity$new_value[1:10] == test.2$encoding_map$primary_productivity$new_value[1:10])
  )

  testthat::expect_false(
    all(test.1$encoding_map$primary_productivity$new_value[1:10] == test.3$encoding_map$primary_productivity$new_value[1:10])
  )

  testthat::expect_false(
    all(test.2$encoding_map$primary_productivity$new_value[1:10] == test.3$encoding_map$primary_productivity$new_value[1:10])
  )


  #TESTING WITH DIFFERENT DATA TYPES
  #################################

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
    all = ecoregions_all_predictors
  )

  methods <- c("mean", "rnorm", "rank", "loo")

  #iterating over input lists
  for(training.i in names(training)){
    for(response.i in names(response)){
      for(predictors.i in names(predictors)){
        for(method.i in methods){


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

        #target_encoding
        df <- target_encoding(
          data = training[[training.i]],
          dependent.variable.name = response[[response.i]],
          predictor.variable.names = predictors[[predictors.i]],
          method = method.i,
          noise = 0.05,
          seed = 100
        )

        #tests
        testthat::expect_equal(
          names(df),
          c("data", "leakage_test", "encoding_map")
        )

        testthat::expect_equal(
          is.data.frame(df$data),
          TRUE
        )

        testthat::expect_equal(
          is.data.frame(df$leakage_test),
          TRUE
        )

        testthat::expect_equal(
          is.list(df$encoding_map),
          TRUE
        )

        }
      }
    }
  }

})
