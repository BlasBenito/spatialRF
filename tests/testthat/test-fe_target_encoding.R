testthat::test_that("`target_encoding()` works", {

  #functions tested
  #fe_target_encoding()
  #fe_target_encoding_mean()
  #fe_target_encoding_rnorm()
  #fe_target_encoding_loo()
  #fe_target_encoding_rank()
  #fe_target_encoding_noise()

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
    ecoregions_numeric_predictors,
    ecoregions_continuous_response,
    ecoregions_binary_response
  )

  #A FEW INDIVIDUAL TESTS TO INCREASE CODE COVERAGE
  #################################

  #target_encoding
  testthat::expect_error(
  out <- fe_target_encoding(
    data = NULL,
    dependent.variable.name = NULL,
    predictor.variable.names = NULL,
    noise = NULL,
    sd.width = NULL
  ))

  testthat::expect_error(
    out <- fe_target_encoding(
      data = ecoregions_df,
      dependent.variable.name = NULL,
      predictor.variable.names = NULL,
      noise = NULL,
      sd.width = NULL
    ))

  testthat::expect_error(
    out <- fe_target_encoding(
      data = ecoregions_df,
      dependent.variable.name = "hola",
      predictor.variable.names = NULL,
      noise = NULL,
      sd.width = NULL
    ))

  testthat::expect_error(
    out <- fe_target_encoding(
      data = ecoregions_df,
      dependent.variable.name = "dominant_landcover",
      predictor.variable.names = NULL,
      noise = NULL,
      sd.width = NULL
    ))

  testthat::expect_error(
    out <- fe_target_encoding(
      data = ecoregions_df,
      dependent.variable.name = ecoregions_continuous_response,
      predictor.variable.names = "adios",
      noise = NULL,
      sd.width = NULL
    ))

  testthat::expect_error(
    out <- fe_target_encoding(
      data = ecoregions_df,
      dependent.variable.name = ecoregions_continuous_response,
      predictor.variable.names = NULL,
      noise = NULL,
      sd.width = NULL
    ))


    out <- fe_target_encoding(
      data = ecoregions_df,
      dependent.variable.name = ecoregions_continuous_response,
      predictor.variable.names = ecoregions_all_predictors,
      noise = c(-1, 2),
      sd.width = c(-1, 2)
    )

    out <- fe_target_encoding(
      data = ecoregions_df,
      dependent.variable.name = ecoregions_binary_response,
      predictor.variable.names = ecoregions_all_predictors,
      noise = c(0, 0.0001),
      sd.width = c(-1, 2)
    )

    out <- fe_target_encoding(
      data = ecoregions_df,
      dependent.variable.name = ecoregions_binary_response,
      predictor.variable.names = ecoregions_numeric_predictors
    )

    testthat::expect_equal(
      colnames(out),
      colnames(ecoregions_df)
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

  #iterating over input lists
  for(training.i in names(training)){
    for(response.i in names(response)){

      #target_encoding
      out <- fe_target_encoding(
        data = training[[training.i]],
        dependent.variable.name = response[[response.i]],
        predictor.variable.names = ecoregions_all_predictors,
        noise = c(0, 0.5),
        sd.width = c(0.1, 0.5)
      )

      testthat::expect_equal(
        length(out),
        3
      )

      if("sf" %in% class(training[[training.i]])){
        testthat::expect_equal(
          "sf" %in% class(out$data),
          TRUE
        )
      }

      if("tbl_df" %in% class(training[[training.i]])){
        testthat::expect_equal(
          "tbl_df" %in% class(out$data),
          TRUE
        )
      }


    }
  }

})
