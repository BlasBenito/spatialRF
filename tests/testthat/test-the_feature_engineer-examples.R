testthat::test_that("`the_feature_engineer()` works", {

  library(spatialRF)
  library(magrittr)

  #loading data
  data(
    ecoregions_df,
    ecoregions_distance_matrix,
    ecoregions_predictor_variable_names,
    ecoregions_dependent_variable_name
  )

  ecoregions_df <- tibble::as_tibble(ecoregions_df)

  cluster <- start_cluster()

  interactions <- the_feature_engineer(
    data = ecoregions_df,
    dependent.variable.name = ecoregions_dependent_variable_name,
    predictor.variable.names = ecoregions_predictor_variable_names,
    xy = ecoregions_df[, c("x", "y")],
    verbose = FALSE,
    seed = 100,
    cluster = cluster
    )

  stop_cluster()

  testthat::expect_s3_class(interactions$screening, "data.frame")
  testthat::expect_s3_class(interactions$selected, "data.frame")
})
