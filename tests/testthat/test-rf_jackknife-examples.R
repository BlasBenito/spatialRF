testthat::test_that("`rf_repeat()` works", {

  library(spatialRF)
  library(magrittr)

  #loading example data
  data(
    ecoregions_df,
    ecoregions_distance_matrix,
    ecoregions_predictor_variable_names,
    ecoregions_dependent_variable_name
    )

  cluster <- make_cluster()

  #fitting random forest model
  rf.model <- rf(
    data = ecoregions_df,
    dependent.variable.name = ecoregions_dependent_variable_name,
    predictor.variable.names = ecoregions_predictor_variable_names,
    distance.matrix = ecoregions_distance_matrix,
    distance.thresholds = 0,
    xy = ecoregions_df[, c("x", "y")],
    n.cores = 1,
    verbose = FALSE
  )

  #computing predictor contribution to model transferability
  rf.model <- rf_jackknife(
    model = rf.model,
    cluster = cluster,
    verbose = TRUE
    )

  stop_cluster()

  testthat::expect_s3_class(
    rf.model$importance$jackknife.df,
    "data.frame"
  )

  testthat::expect_s3_class(
    rf.model$importance$jackknife.plot,
    "ggplot"
  )


})
