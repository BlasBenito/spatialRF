testthat::test_that("`rf()` works", {

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

       model <- rf(
         data = training[[training.i]],
         dependent.variable.name = response[[response.i]],
         predictor.variable.names = ecoregions_all_predictors,
         distance.matrix = ecoregions_distance_matrix,
         verbose = FALSE
       )

     }}

  #  #checking with tibble
  #  ##########################################################
  #
  # #fitting model
  # model <- rf(
  #   data = tibble::as_tibble(ecoregions_df),
  #   dependent.variable.name = ecoregions_continuous_response,
  #   predictor.variable.names = ecoregions_numeric_predictors,
  #   distance.matrix = ecoregions_distance_matrix,
  #   distance.thresholds = c(0,100, 1000, 10000),
  #   xy = ecoregions_df[, c("x", "y")],
  #   verbose = TRUE
  # )
  #
  # testthat::expect_s3_class(
  #   model,
  #   "rf"
  # )
  #
  # testthat::expect_equal(
  #   object = "tbl" %in% class(model$variable.importance.local),
  #   expected = TRUE
  # )
  #
  # testthat::expect_equal(
  #   object = "tbl" %in% class(model$importance$per.variable),
  #   expected = TRUE
  # )
  #
  # testthat::expect_equal(
  #   object = "tbl" %in% class(model$residuals$autocorrelation$per.distance),
  #   expected = TRUE
  # )
  #
  # testthat::expect_true(
  #   model$ranger.arguments$num.trees == 500
  # )
  #
  # testthat::expect_named(
  #   model$residuals$autocorrelation$per.distance,
  #   c("distance.threshold", "moran.i", "moran.i.null", "p.value", "interpretation")
  #   )
  #
  # #re-fitting model with new hyperparameters
  # ###########################################
  # model <- rf(
  #   model = model,
  #   ranger.arguments = list(num.trees = 5000),
  #   verbose = FALSE
  # )
  #
  # testthat::expect_true(
  #   model$ranger.arguments$num.trees == 5000
  # )
  #
  # #checking what seed takes precedence
  # model <- rf(
  #   model = model,
  #   ranger.arguments = list(seed = 2),
  #   seed = 3,
  #   verbose = FALSE
  # )
  #
  # testthat::expect_equal(
  #   object = model$ranger.arguments$seed,
  #   expected = 3
  # )
  #
  #
  # #fitting model with ranger.arguments
  # ############################################
  #  my.ranger.arguments <- list(
  #  data = ecoregions_df,
  #  dependent.variable.name = ecoregions_continuous_response,
  #  predictor.variable.names = ecoregions_numeric_predictors,
  #  distance.matrix = ecoregions_distance_matrix,
  #  distance.thresholds = c(0, 1000)
  #  )
  #
  #  model <- rf(
  #    ranger.arguments = my.ranger.arguments,
  #    verbose = FALSE
  #    )
  #
  #  testthat::expect_true(
  #    model$ranger.arguments$num.trees == 500
  #  )
  #
  #  testthat::expect_s3_class(
  #    model,
  #    "rf"
  #  )
  #
  #  testthat::expect_s3_class(
  #    model$importance$per.variable,
  #    "data.frame"
  #  )
  #
  #  testthat::expect_named(
  #    model$importance$per.variable,
  #    c("variable", "importance")
  #  )
  #
  #  testthat::expect_s3_class(
  #    model$residuals$autocorrelation$per.distance,
  #    "data.frame"
  #  )
  #
  #  testthat::expect_named(
  #    model$residuals$autocorrelation$per.distance,
  #    c("distance.threshold", "moran.i", "moran.i.null", "p.value", "interpretation")
  #  )
  #
  #  #using output of auto_vif and auto_cor as predictor.variable.names
  #  testthat::expect_warning(
  #    variable.selection <- auto_cor(
  #      data = ecoregions_df,
  #      verbose = FALSE,
  #      preference.order = ecoregions_numeric_predictors
  #    ) %>%
  #      auto_vif(
  #        verbose = FALSE
  #      )
  #  )
  #
  #  #fitting model
  #  model <- rf(
  #    data = ecoregions_df,
  #    dependent.variable.name = ecoregions_continuous_response,
  #    predictor.variable.names = variable.selection,
  #    distance.matrix = ecoregions_distance_matrix,
  #    distance.thresholds = c(0,100, 1000, 10000),
  #    verbose = FALSE
  #  )
  #
  #  testthat::expect_true(
  #    model$ranger.arguments$num.trees == 500
  #  )
  #
  #  testthat::expect_s3_class(
  #    model,
  #    "rf"
  #  )
  #
  #  testthat::expect_s3_class(
  #    model$importance$per.variable,
  #    "data.frame"
  #  )
  #
  #  testthat::expect_named(
  #    model$importance$per.variable,
  #    c("variable", "importance")
  #  )
  #
  #  testthat::expect_s3_class(
  #    model$residuals$autocorrelation$per.distance,
  #    "data.frame"
  #  )
  #
  #  testthat::expect_named(
  #    model$residuals$autocorrelation$per.distance,
  #    c("distance.threshold", "moran.i", "moran.i.null", "p.value", "interpretation")
  #  )
  #
  #  #fitting model with binary response
  #  ###################################
  #  ecoregions_df$binary_response <- ifelse(
  #    ecoregions_df$plant_richness > 5000,
  #    1,
  #    0
  #  )
  #
  #  model <- rf(
  #    data = ecoregions_df,
  #    dependent.variable.name = "binary_response",
  #    predictor.variable.names = ecoregions_numeric_predictors,
  #    distance.matrix = ecoregions_distance_matrix,
  #    distance.thresholds = c(0,100, 1000, 10000),
  #    verbose = FALSE
  #  )
  #
  #  testthat::expect_true(
  #    is.data.frame(model$performance$roc.curve.oob) == TRUE
  #  )
  #
  #  testthat::expect_true(
  #    nrow(model$performance$roc.curve.oob) == 11
  #  )
  #
  #  testthat::expect_true(
  #    round(unique(model$ranger.arguments$case.weights), 4)[1] == 0.0061
  #  )
  #
  #  testthat::expect_true(
  #    round(unique(model$ranger.arguments$case.weights), 4)[2] == 0.0161
  #  )


})
