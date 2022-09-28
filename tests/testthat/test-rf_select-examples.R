testthat::test_that("`rf_select()` works", {

  library(spatialRF)
  library(magrittr)
  library(doParallel)

  #loading data
  data(
    ecoregions_df,
    ecoregions_distance_matrix,
    ecoregions_predictor_variable_names,
    ecoregions_dependent_variable_name
  )

   #fitting model
   out <- rf(
     data = ecoregions_df,
     dependent.variable.name = ecoregions_dependent_variable_name,
     predictor.variable.names = ecoregions_predictor_variable_names,
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
     "selection.plot" %in% names(out$selection)
   )

})
