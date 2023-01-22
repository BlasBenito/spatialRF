testthat::test_that("fe_scale works", {

  library(spatialRF)
  library(magrittr)

  #loading data
  data(
    ecoregions_df,
    ecoregions_tibble,
    ecoregions_sf,
    ecoregions_distance_matrix,
    ecoregions_numeric_predictors,
    ecoregions_all_predictors,
    ecoregions_continuous_response,
    ecoregions_binary_response
  )

  #adding zero variance zolumn
  #passing zero variance column
  ecoregions_df$zeros <- runif(n = nrow(ecoregions_df)) /10000
  ecoregions_tibble$zeros <- runif(n = nrow(ecoregions_df)) /10000
  ecoregions_sf$zeros <- runif(n = nrow(ecoregions_df)) /10000

  ecoregions_numeric_predictors <- c(ecoregions_numeric_predictors, "zeros")

  #lists to iterate over
  training <- list(
    df = ecoregions_df,
    matrix = as.matrix(ecoregions_df),
    sf = ecoregions_sf,
    tibble = ecoregions_tibble,
    null = NULL
  )

  response <- list(
    continuous = ecoregions_continuous_response,
    binary = ecoregions_binary_response,
    null = NULL,
    other = "other"
  )

  predictors <- list(
    all = ecoregions_all_predictors,
    numeric = ecoregions_numeric_predictors,
    null = NULL
  )

  scale.list <- list(
    true = TRUE,
    false = FALSE
  )

  center.list <- list(
    true = TRUE,
    false = FALSE
  )

  verbose.list <- list(
    true = TRUE,
    false = FALSE
  )

  for(training.i in names(training)){
    for(response.i in names(response)){
      for(predictor.i in names(predictors)){
        for(scale.i in names(scale.list)){
          for(center.i in names(center.list)){
            for(verbose.i in names(verbose.list)){

              #exception
              if(!("data.frame" %in% class(training[[training.i]]))){

                testthat::expect_error(
                  x <- fe_scale(
                    data = training[[training.i]],
                    dependent.variable.name = response[[response.i]],
                    predictor.variable.names = predictors[[predictor.i]],
                    scale = scale.list[[scale.i]],
                    center = center.list[[center.i]],
                    verbose = verbose.list[[verbose.i]]
                  )
                )

              } else {

                x <- fe_scale(
                  data = training[[training.i]],
                  dependent.variable.name = response[[response.i]],
                  predictor.variable.names = predictors[[predictor.i]],
                  scale = scale.list[[scale.i]],
                  center = center.list[[center.i]],
                  verbose = verbose.list[[verbose.i]]
                )

                #test
                if("sampling_bias" %in% predictors[[predictor.i]]){

                  y <- ecoregions_df[["sampling_bias"]]
                  y_ <- x[["sampling_bias"]]

                  if(center.list[[center.i]] == TRUE){
                    if(scale.list[[scale.i]] == TRUE){

                      y <- (y - mean(y))/sd(y)

                    } else {

                      y <- (y - mean(y))

                    }
                  }

                  if(center.list[[center.i]] == FALSE){
                    if(scale.list[[scale.i]] == TRUE){

                      y <- y/(sqrt(sum(y^2)/(length(y)-1)))

                    }
                  }

                  testthat::expect_equal(
                    all(round(y, 2) == round(y_, 2)),
                    TRUE
                  )

                }
              }
            }
          }
        }
      }
    }
  }

})
