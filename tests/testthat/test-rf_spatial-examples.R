testthat::test_that("`rf_spatial()` works", {

  library(spatialRF)
  library(magrittr)

  #loading data
  data(
    ecoregions_df,
    ecoregions_distance_matrix,
    ecoregions_predvar_names,
    ecoregions_depvar_name
  )

  cluster <- make_cluster()

  #fitting model
  testthat::expect_warning(
  out.1 <- rf_spatial(
    data = ecoregions_df,
    dependent.variable.name = ecoregions_depvar_name,
    predictor.variable.names = ecoregions_predvar_names,
    distance.matrix = ecoregions_distance_matrix,
    distance.thresholds = c(0,100, 1000, 10000),
    method = "hengl",
    cluster = cluster,
    verbose = FALSE
  )
  )

  testthat::expect_equal(
    inherits(
      out.1,
      "rf_spatial"
      ),
    TRUE
    )

  #fitting model
  testthat::expect_warning(
  out.2 <- rf_spatial(
    data = ecoregions_df,
    dependent.variable.name = ecoregions_depvar_name,
    predictor.variable.names = ecoregions_predvar_names,
    distance.matrix = ecoregions_distance_matrix,
    distance.thresholds = c(0,100, 1000, 10000),
    method = "hengl.moran.sequential",
    verbose = FALSE,
    cluster = cluster,
  )
  )

  testthat::expect_equal(
    inherits(
      out.2,
      "rf_spatial"
    ),
    TRUE
  )

  #fitting model
  testthat::expect_warning(
  out.3 <- rf_spatial(
    data = ecoregions_df,
    dependent.variable.name = ecoregions_depvar_name,
    predictor.variable.names = ecoregions_predvar_names,
    distance.matrix = ecoregions_distance_matrix,
    distance.thresholds = c(0,100, 1000, 10000),
    method = "hengl.effect.sequential",
    verbose = FALSE,
    cluster = cluster,
  )
  )

  testthat::expect_equal(
    inherits(
      out.3,
      "rf_spatial"
    ),
    TRUE
  )

  #fitting model
  testthat::expect_warning(
  out.4 <- rf_spatial(
    data = ecoregions_df,
    dependent.variable.name = ecoregions_depvar_name,
    predictor.variable.names = ecoregions_predvar_names,
    distance.matrix = ecoregions_distance_matrix,
    distance.thresholds = c(0,100, 1000, 10000),
    method = "hengl.effect.recursive",
    verbose = FALSE,
    cluster = cluster,
  )
  )

  testthat::expect_equal(
    inherits(
      out.4,
      "rf_spatial"
    ),
    TRUE
  )


  #fitting model
  testthat::expect_warning(
  out.5 <- rf_spatial(
    data = ecoregions_df,
    dependent.variable.name = ecoregions_depvar_name,
    predictor.variable.names = ecoregions_predvar_names,
    distance.matrix = ecoregions_distance_matrix,
    distance.thresholds = c(0,100, 1000, 10000),
    method = "pca.moran.sequential",
    verbose = FALSE,
    cluster = cluster,
  )
  )

  testthat::expect_equal(
    inherits(
      out.5,
      "rf_spatial"
    ),
    TRUE
  )

  #fitting model
  testthat::expect_warning(
  out.6 <- rf_spatial(
    data = ecoregions_df,
    dependent.variable.name = ecoregions_depvar_name,
    predictor.variable.names = ecoregions_predvar_names,
    distance.matrix = ecoregions_distance_matrix,
    distance.thresholds = c(0,100, 1000, 10000),
    method = "pca.effect.sequential",
    verbose = FALSE,
    cluster = cluster,
  )
  )

  testthat::expect_equal(
    inherits(
      out.6,
      "rf_spatial"
    ),
    TRUE
  )


  #fitting model
  testthat::expect_warning(
  out.7 <- rf_spatial(
    data = ecoregions_df,
    dependent.variable.name = ecoregions_depvar_name,
    predictor.variable.names = ecoregions_predvar_names,
    distance.matrix = ecoregions_distance_matrix,
    distance.thresholds = c(0,100, 1000, 10000),
    method = "pca.effect.recursive",
    verbose = FALSE,
    cluster = cluster,
  )
  )

  testthat::expect_equal(
    inherits(
      out.7,
      "rf_spatial"
    ),
    TRUE
  )

  #fitting model
  testthat::expect_warning(
  out.8 <- rf_spatial(
    data = ecoregions_df,
    dependent.variable.name = ecoregions_depvar_name,
    predictor.variable.names = ecoregions_predvar_names,
    distance.matrix = ecoregions_distance_matrix,
    distance.thresholds = c(0,100, 1000, 10000),
    method = "mem.moran.sequential",
    verbose = FALSE,
    cluster = cluster,
  )
  )

  testthat::expect_equal(
    inherits(
      out.8,
      "rf_spatial"
    ),
    TRUE
  )

  #fitting model
  testthat::expect_warning(
  out.9 <- rf_spatial(
    data = ecoregions_df,
    dependent.variable.name = ecoregions_depvar_name,
    predictor.variable.names = ecoregions_predvar_names,
    distance.matrix = ecoregions_distance_matrix,
    distance.thresholds = c(0,100, 1000, 10000),
    method = "mem.effect.sequential",
    verbose = FALSE,
    cluster = cluster,
  )
  )

  testthat::expect_equal(
    inherits(
      out.9,
      "rf_spatial"
    ),
    TRUE
  )

  #fitting model
  testthat::expect_warning(
  out.10 <- rf_spatial(
    data = ecoregions_df,
    dependent.variable.name = ecoregions_depvar_name,
    predictor.variable.names = ecoregions_predvar_names,
    distance.matrix = ecoregions_distance_matrix,
    distance.thresholds = c(0,100, 1000, 10000),
    method = "mem.effect.recursive",
    verbose = FALSE,
    cluster = cluster,
  )
  )

  testthat::expect_equal(
    inherits(
      out.10,
      "rf_spatial"
    ),
    TRUE
  )

  stop_cluster(cluster = cluster)


})
