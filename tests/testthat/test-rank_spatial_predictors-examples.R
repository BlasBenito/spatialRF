test_that("`rank_spatial_predictors()` works", {
  data("distance_matrix")
  spatial.predictors.df <- pca_multithreshold(
    distance_matrix[1:50, 1:50],
    distance.thresholds = c(0, 100, 1000)
    )
  rank <- rank_spatial_predictors(
    distance.matrix = distance_matrix[1:50, 1:50],
    distance.thresholds = c(0, 100, 1000),
    spatial.predictors.df = spatial.predictors.df,
    ranking.method = "moran",
    n.cores = 1
  )
  expect_type(rank, "list")
  expect_named(rank, c("method", "criteria", "ranking", "spatial.predictors.df"))
  expect_length(rank, 4)
  expect_s3_class(rank$criteria, "data.frame")

})
