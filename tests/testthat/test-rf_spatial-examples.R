testthat::test_that("`rf_spatial()` works", {

  library(spatialRF)
  library(magrittr)

  #loading data
  data(
    ecoregions_df,
    ecoregions_distance_matrix,
    ecoregions_predictor_variable_names,
    ecoregions_dependent_variable_name
  )

  cluster <- start_cluster()

  #fitting model
  out.1 <- rf_spatial(
    data = ecoregions_df,
    dependent.variable.name = ecoregions_dependent_variable_name,
    predictor.variable.names = ecoregions_predictor_variable_names,
    distance.matrix = ecoregions_distance_matrix,
    distance.thresholds = c(0,100, 1000, 10000),
    method = "hengl",
    cluster = cluster,
    verbose = FALSE
  )

  testthat::expect_equal(
    inherits(
      out.1,
      "rf_spatial"
      ),
    TRUE
    )


  #fitting model
  out.2 <- rf_spatial(
    data = ecoregions_df,
    dependent.variable.name = ecoregions_dependent_variable_name,
    predictor.variable.names = ecoregions_predictor_variable_names,
    distance.matrix = ecoregions_distance_matrix,
    distance.thresholds = c(0,100, 1000, 10000),
    method = "mem.moran.sequential",
    verbose = FALSE,
    cluster = cluster,
  )

  testthat::expect_equal(
    inherits(
      out.2,
      "rf_spatial"
    ),
    TRUE
  )

  #fitting model
  out.3 <- rf_spatial(
    data = ecoregions_df,
    dependent.variable.name = ecoregions_dependent_variable_name,
    predictor.variable.names = ecoregions_predictor_variable_names,
    distance.matrix = ecoregions_distance_matrix,
    distance.thresholds = c(0,100, 1000, 10000),
    method = "mem.effect.sequential",
    verbose = FALSE,
    cluster = cluster,
  )

  testthat::expect_equal(
    inherits(
      out.3,
      "rf_spatial"
    ),
    TRUE
  )

  #fitting model
  out.4 <- rf_spatial(
    data = ecoregions_df,
    dependent.variable.name = ecoregions_dependent_variable_name,
    predictor.variable.names = ecoregions_predictor_variable_names,
    distance.matrix = ecoregions_distance_matrix,
    distance.thresholds = c(0,100, 1000, 10000),
    method = "mem.effect.recursive",
    verbose = FALSE,
    cluster = cluster,
  )

  testthat::expect_equal(
    inherits(
      out.4,
      "rf_spatial"
    ),
    TRUE
  )

  stop_cluster()


})
