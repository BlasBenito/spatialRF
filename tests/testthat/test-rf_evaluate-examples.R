test_that("`rf_evaluate()` works", {
  library(spatialRF)
  data(ecoregions_df)
  data("ecoregions_distance_matrix")
  rf.model <- rf(
    data = ecoregions_df, dependent.variable.name = ecoregions_depvar_name,
    predictor.variable.names = ecoregions_predvar_names,
    distance.matrix = ecoregions_distance_matrix, distance.thresholds = c(
      0,
      1000, 2000
    ), verbose = FALSE
  )
  rf.model <- rf_evaluate(
    model = rf.model,
    xy = ecoregions_df[,c("x", "y")],
    verbose = FALSE,
    n.cores = 7
    )
  expect_s3_class(rf.model, "rf_evaluate")
  expect_type(rf.model$evaluation, "list")
  expect_s3_class(rf.model$evaluation$per.fold, "data.frame")
  expect_s3_class(rf.model$evaluation$per.model, "data.frame")
  expect_s3_class(rf.model$evaluation$aggregated, "data.frame")
})
