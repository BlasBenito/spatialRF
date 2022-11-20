testthat::test_that("`default_distance_thresholds()` works", {

  data(
    ecoregions_df,
    ecoregions_distance_matrix,
    ecoregions_predictor_variable_names,
    ecoregions_dependent_variable_name
  )

  #default_distance_thresholds
  ################################
  x <- default_distance_thresholds(ecoregions_distance_matrix)

  testthat::expect_length(x, 4)
  testthat::expect_type(x, "double")


  #filter_spatial_predictors
  ##############################

  #computing Moran's Eigenvector Maps
  spatial.predictors.df <- mem_multithreshold(
    distance.matrix = ecoregions_distance_matrix,
    distance.thresholds = c(0, 1000)
  )

  #filtering spatial predictors
  spatial.predictors.df.filtered <- filter_spatial_predictors(
    data = ecoregions_df,
    predictor.variable.names = ecoregions_predictor_variable_names,
    spatial.predictors.df = spatial.predictors.df,
    max.cor = 0.50
  )

  testthat::expect_equal(
    is.data.frame(spatial.predictors.df.filtered),
    TRUE
  )

  testthat::expect_equal(
    ncol(spatial.predictors.df.filtered) < nrow(spatial.predictors.df),
    TRUE
  )


  #default_distance_thresholds
  ##############################################
   x <- default_distance_thresholds(ecoregions_distance_matrix)

  testthat::expect_equal(
    is.numeric(x),
    TRUE
  )

  testthat::expect_equal(
    length(x),
    4
  )


  #is_binary_response
  #############################################
  x <- is_binary_response(x = 1:10)

  testthat::expect_equal(
    FALSE,
    x
  )

  x <- is_binary_response(x = c(1, 1, 1, 0, 0, 0))

  testthat::expect_equal(
    TRUE,
    x
  )


  #prepare_importance_spatial
  ###########################################
   model <- rf_spatial(
     data = ecoregions_df,
     dependent.variable.name = ecoregions_dependent_variable_name,
     predictor.variable.names = ecoregions_predictor_variable_names,
     distance.matrix = ecoregions_distance_matrix,
     distance.thresholds = 0,
     method = "hengl",
     n.cores = 1,
     verbose = FALSE
   )

   #preparing the importance data frame
   importance <- prepare_importance_spatial(model)

   testthat::expect_equal(
     is.list(importance),
     TRUE
   )

   testthat::expect_equal(
     names(importance),
     c(
       "per.variable",
       "per.variable.plot",
       "spatial.predictors",
       "spatial.predictors.plot",
       "spatial.predictors.stats",
       "spatial.predictors.stats.plot"
     )
   )

})
