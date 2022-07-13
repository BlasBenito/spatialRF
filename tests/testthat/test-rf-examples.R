testthat::test_that("`rf()` works", {

  library(spatialRF)
  library(magrittr)

  #loading data
   data(
    ecoregions_df,
    ecoregions_distance_matrix,
    ecoregions_predvar_names,
    ecoregions_depvar_name
    )

  #fitting model
  out <- rf(
    data = ecoregions_df,
    dependent.variable.name = ecoregions_depvar_name,
    predictor.variable.names = ecoregions_predvar_names,
    distance.matrix = ecoregions_distance_matrix,
    distance.thresholds = c(0,100, 1000, 10000),
    verbose = FALSE
  )

  testthat::expect_true(
    out$ranger.arguments$num.trees == 500
  )

  testthat::expect_s3_class(
    out,
    "rf"
    )

  testthat::expect_s3_class(
    out$importance$per.variable,
    "data.frame"
    )

  testthat::expect_named(
    out$importance$per.variable,
    c("variable", "importance")
    )

  testthat::expect_s3_class(
    out$residuals$autocorrelation$per.distance,
    "data.frame"
    )

  testthat::expect_named(
    out$residuals$autocorrelation$per.distance,
    c("distance.threshold", "moran.i", "moran.i.null", "p.value", "interpretation")
    )

  #re-fitting model with new hyperparameters
  out.2 <- rf(
    model = out,
    ranger.arguments = list(num.trees = 5000),
    verbose = FALSE
  )

  testthat::expect_true(
    out.2$ranger.arguments$num.trees == 5000
  )

  #fitting model with ranger.arguments
   my.ranger.arguments <- list(
   data = ecoregions_df,
   dependent.variable.name = ecoregions_depvar_name,
   predictor.variable.names = ecoregions_predvar_names,
   distance.matrix = ecoregions_distance_matrix,
   distance.thresholds = c(0, 1000)
   )

   out.3 <- rf(
     ranger.arguments = my.ranger.arguments,
     verbose = FALSE
     )

   testthat::expect_true(
     out.3$ranger.arguments$num.trees == 500
   )

   testthat::expect_s3_class(
     out.3,
     "rf"
   )

   testthat::expect_s3_class(
     out.3$importance$per.variable,
     "data.frame"
   )

   testthat::expect_named(
     out.3$importance$per.variable,
     c("variable", "importance")
   )

   testthat::expect_s3_class(
     out.3$residuals$autocorrelation$per.distance,
     "data.frame"
   )

   testthat::expect_named(
     out.3$residuals$autocorrelation$per.distance,
     c("distance.threshold", "moran.i", "moran.i.null", "p.value", "interpretation")
   )

   #using output of auto_vif and auto_cor as predictor.variable.names

   testthat::expect_warning(
     variable.selection <- auto_cor(
       x = ecoregions_df,
       verbose = FALSE,
       preference.order = ecoregions_predvar_names
     ) %>%
       auto_vif()
   )

   #fitting model
   out.4 <- rf(
     data = ecoregions_df,
     dependent.variable.name = ecoregions_depvar_name,
     predictor.variable.names = variable.selection,
     distance.matrix = ecoregions_distance_matrix,
     distance.thresholds = c(0,100, 1000, 10000),
     verbose = FALSE
   )

   testthat::expect_true(
     out.4$ranger.arguments$num.trees == 500
   )

   testthat::expect_s3_class(
     out.4,
     "rf"
   )

   testthat::expect_s3_class(
     out.4$importance$per.variable,
     "data.frame"
   )

   testthat::expect_named(
     out.4$importance$per.variable,
     c("variable", "importance")
   )

   testthat::expect_s3_class(
     out.4$residuals$autocorrelation$per.distance,
     "data.frame"
   )

   testthat::expect_named(
     out.4$residuals$autocorrelation$per.distance,
     c("distance.threshold", "moran.i", "moran.i.null", "p.value", "interpretation")
   )

})
