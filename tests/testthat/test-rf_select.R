testthat::test_that("`rf_select()` works", {

  library(spatialRF)
  library(magrittr)
  library(doParallel)

  #loading data
  data(
    ecoregions_df,
    ecoregions_distance_matrix,
    ecoregions_numeric_predictors,
    ecoregions_continuous_response
  )

  ecoregions_df <- tibble::as_tibble(ecoregions_df)

   #fitting model
   out <- rf(
     data = ecoregions_df,
     dependent.variable.name = ecoregions_continuous_response,
     predictor.variable.names = ecoregions_numeric_predictors,
     distance.matrix = ecoregions_distance_matrix,
     distance.thresholds = c(0,100, 1000, 10000),
     xy = ecoregions_df[, c("x", "y")],
     verbose = FALSE
   )

   cluster <- start_cluster()

   #running selection
   out <- rf_select(
     model = out,
     cluster = cluster
   )

   stop_cluster()


   testthat::expect_true(
     "selection" %in% names(out)
   )

   testthat::expect_true(
     "plot" %in% names(out$selection)
   )

   testthat::expect_true(
     "df" %in% names(out$selection)
   )

})
