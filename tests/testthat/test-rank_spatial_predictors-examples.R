testthat::test_that("`rank_spatial_predictors()` works", {

  data(ecoregions_distance_matrix)

  spatial.predictors.df <- mem_multithreshold(
    ecoregions_distance_matrix[1:50, 1:50],
    distance.thresholds = c(0, 100, 1000)
    )

  rank <- rank_spatial_predictors(
    distance.matrix = ecoregions_distance_matrix[1:50, 1:50],
    distance.thresholds = c(0, 100, 1000),
    spatial.predictors.df = spatial.predictors.df,
    ranking.method = "moran",
    n.cores = 1
  )

  testthat::expect_type(rank, "list")
  testthat::expect_named(rank, c("method", "criteria", "ranking", "spatial.predictors.df"))
  testthat::expect_length(rank, 4)
  testthat::expect_s3_class(rank$criteria, "data.frame")

})
