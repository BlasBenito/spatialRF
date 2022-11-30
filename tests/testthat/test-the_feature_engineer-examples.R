testthat::test_that("`the_feature_engineer()` works", {

  library(spatialRF)
  library(magrittr)

  #loading data
  data(
    ecoregions_df,
    ecoregions_distance_matrix,
    ecoregions_numeric_predictors,
    ecoregions_continuous_response
  )

  ecoregions_df <- tibble::as_tibble(ecoregions_df)

  cluster <- start_cluster()

  interactions <- the_feature_engineer(
    data = ecoregions_df,
    dependent.variable.name = ecoregions_continuous_response,
    predictor.variable.names = ecoregions_numeric_predictors,
    xy = ecoregions_df[, c("x", "y")],
    verbose = FALSE,
    seed = 100,
    cluster = cluster
    )

  stop_cluster()

  testthat::expect_s3_class(interactions$screening, "data.frame")
  testthat::expect_s3_class(interactions$selected, "data.frame")
})
