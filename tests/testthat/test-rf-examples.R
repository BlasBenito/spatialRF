testthat::test_that("`rf()` works", {

  library(spatialRF)
  library(magrittr)

  #loading data
   data(
    ecoregions_df,
    ecoregions_distance_matrix,
    ecoregions_predictor_variable_names,
    ecoregions_dependent_variable_name
    )

   #checking with tibble
   ##########################################################

  #fitting model
  model <- rf(
    data = tibble::as_tibble(ecoregions_df),
    dependent.variable.name = ecoregions_dependent_variable_name,
    predictor.variable.names = ecoregions_predictor_variable_names,
    distance.matrix = ecoregions_distance_matrix,
    distance.thresholds = c(0,100, 1000, 10000),
    xy = ecoregions_df[, c("x", "y")],
    verbose = FALSE
  )

  testthat::expect_s3_class(
    model,
    "rf"
  )

  testthat::expect_equal(
    object = "tbl" %in% class(model$variable.importance.local),
    expected = TRUE
  )

  testthat::expect_equal(
    object = "tbl" %in% class(model$importance$per.variable),
    expected = TRUE
  )

  testthat::expect_equal(
    object = "tbl" %in% class(model$residuals$autocorrelation$per.distance),
    expected = TRUE
  )

  testthat::expect_true(
    model$ranger.arguments$num.trees == 500
  )

  testthat::expect_named(
    model$residuals$autocorrelation$per.distance,
    c("distance.threshold", "moran.i", "moran.i.null", "p.value", "interpretation")
    )

  #re-fitting model with new hyperparameters
  ###########################################
  model <- rf(
    model = model,
    ranger.arguments = list(num.trees = 5000),
    verbose = FALSE
  )

  testthat::expect_true(
    model$ranger.arguments$num.trees == 5000
  )

  #checking what seed takes precedence
  model <- rf(
    model = model,
    ranger.arguments = list(seed = 2),
    seed = 3,
    verbose = FALSE
  )

  testthat::expect_equal(
    object = model$ranger.arguments$seed,
    expected = 3
  )


  #fitting model with ranger.arguments
  ############################################
   my.ranger.arguments <- list(
   data = ecoregions_df,
   dependent.variable.name = ecoregions_dependent_variable_name,
   predictor.variable.names = ecoregions_predictor_variable_names,
   distance.matrix = ecoregions_distance_matrix,
   distance.thresholds = c(0, 1000)
   )

   model <- rf(
     ranger.arguments = my.ranger.arguments,
     verbose = FALSE
     )

   testthat::expect_true(
     model$ranger.arguments$num.trees == 500
   )

   testthat::expect_s3_class(
     model,
     "rf"
   )

   testthat::expect_s3_class(
     model$importance$per.variable,
     "data.frame"
   )

   testthat::expect_named(
     model$importance$per.variable,
     c("variable", "importance")
   )

   testthat::expect_s3_class(
     model$residuals$autocorrelation$per.distance,
     "data.frame"
   )

   testthat::expect_named(
     model$residuals$autocorrelation$per.distance,
     c("distance.threshold", "moran.i", "moran.i.null", "p.value", "interpretation")
   )

   #using output of auto_vif and auto_cor as predictor.variable.names
   testthat::expect_warning(
     variable.selection <- auto_cor(
       data = ecoregions_df,
       verbose = FALSE,
       preference.order = ecoregions_predictor_variable_names
     ) %>%
       auto_vif()
   )

   #fitting model
   model <- rf(
     data = ecoregions_df,
     dependent.variable.name = ecoregions_dependent_variable_name,
     predictor.variable.names = variable.selection,
     distance.matrix = ecoregions_distance_matrix,
     distance.thresholds = c(0,100, 1000, 10000),
     verbose = FALSE
   )

   testthat::expect_true(
     model$ranger.arguments$num.trees == 500
   )

   testthat::expect_s3_class(
     model,
     "rf"
   )

   testthat::expect_s3_class(
     model$importance$per.variable,
     "data.frame"
   )

   testthat::expect_named(
     model$importance$per.variable,
     c("variable", "importance")
   )

   testthat::expect_s3_class(
     model$residuals$autocorrelation$per.distance,
     "data.frame"
   )

   testthat::expect_named(
     model$residuals$autocorrelation$per.distance,
     c("distance.threshold", "moran.i", "moran.i.null", "p.value", "interpretation")
   )

   #fitting model with binary response
   ###################################
   ecoregions_df$binary_response <- ifelse(
     ecoregions_df$plant_richness > 5000,
     1,
     0
   )

   model <- rf(
     data = ecoregions_df,
     dependent.variable.name = "binary_response",
     predictor.variable.names = ecoregions_predictor_variable_names,
     distance.matrix = ecoregions_distance_matrix,
     distance.thresholds = c(0,100, 1000, 10000),
     verbose = FALSE
   )

   testthat::expect_true(
     is.data.frame(model$performance$roc.curve.oob) == TRUE
   )

   testthat::expect_true(
     nrow(model$performance$roc.curve.oob) == 11
   )

   testthat::expect_true(
     round(unique(model$ranger.arguments$case.weights), 4)[1] == 0.0061
   )

   testthat::expect_true(
     round(unique(model$ranger.arguments$case.weights), 4)[2] == 0.0161
   )


})
