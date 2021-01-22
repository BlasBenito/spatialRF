test_that("`rank_spatial_predictors()` works", {
  data("distance_matrix")
  spatial.predictors.df <- pca_distance_matrix(
    x = distance_matrix[1:50, 1:50],
    distance.thresholds = c(0, 100, 1000)
    )
  rank <- rank_spatial_predictors(
    ranking.method = "mem",
    spatial.predictors.df = spatial.predictors.df,
    distance.matrix = distance_matrix[1:50, 1:50],
    distance.thresholds = c(0, 100, 1000),
    n.cores = 1,
    multicollinearity.filter = "vif"
  )
  expect_type(rank, "list")
  expect_length(rank, 2)
  expect_s3_class(rank$ranking.criteria, "data.frame")
  expect_length(rank, 2)
  expect_named(rank$ranking.criteria, c("spatial.predictors.name", "ranking.criteria", "interpretation"))

})
