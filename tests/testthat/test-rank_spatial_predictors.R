test_that("`rank_spatial_predictors()` works", {
  data("plants_distance")
  spatial.predictors.df <- pca_multithreshold(
    plants_distance[1:50, 1:50],
    distance.thresholds = c(100, 100, 1000)
  )

  rank <- rank_spatial_predictors(
    distance.matrix = plants_distance[1:50, 1:50],
    distance.thresholds = c(100, 100, 1000),
    spatial.predictors.df = spatial.predictors.df,
    ranking.method = "moran"
  )

  expect_type(rank, "list")
  expect_named(
    rank,
    c("method", "criteria", "ranking", "spatial.predictors.df")
  )
  expect_length(rank, 4)
  expect_s3_class(rank$criteria, "data.frame")
})


test_that("rank_spatial_predictors() works with method = 'moran'", {
  data(plants_df, plants_distance)

  # Generate spatial predictors
  spatial_preds <- mem(
    distance.matrix = plants_distance[1:50, 1:50],
    distance.threshold = 1000
  )

  result <- rank_spatial_predictors(
    distance.matrix = plants_distance[1:50, 1:50],
    distance.thresholds = 1000,
    spatial.predictors.df = spatial_preds,
    ranking.method = "moran",
    verbose = FALSE
  )

  expect_type(result, "list")
  expect_named(
    result,
    c("method", "criteria", "ranking", "spatial.predictors.df")
  )
  expect_equal(result$method, "moran")
  expect_s3_class(result$criteria, "data.frame")
  expect_true(is.character(result$ranking))
})

test_that("rank_spatial_predictors() works with method = 'effect'", {
  data(plants_df, plants_distance)

  # Generate spatial predictors
  spatial_preds <- mem(
    distance.matrix = plants_distance[1:50, 1:50],
    distance.threshold = 1000
  )

  result <- rank_spatial_predictors(
    data = plants_df[1:50, ],
    dependent.variable.name = "richness_species_vascular",
    predictor.variable.names = colnames(plants_df)[5:8],
    distance.matrix = plants_distance[1:50, 1:50],
    distance.thresholds = 1000,
    spatial.predictors.df = spatial_preds,
    ranking.method = "effect",
    verbose = FALSE
  )

  expect_type(result, "list")
  expect_named(
    result,
    c("method", "criteria", "ranking", "spatial.predictors.df")
  )
  expect_equal(result$method, "effect")
  expect_s3_class(result$criteria, "data.frame")

  # Effect method should have model.r.squared column
  expect_true("moran.i" %in% colnames(result$criteria))
  expect_true("model.r.squared" %in% colnames(result$criteria))
  expect_true("p.value" %in% colnames(result$criteria))
})

test_that("rank_spatial_predictors() output structure is correct", {
  data(plants_df, plants_distance)

  spatial_preds <- mem(
    distance.matrix = plants_distance[1:50, 1:50],
    distance.threshold = 1000
  )

  result <- rank_spatial_predictors(
    distance.matrix = plants_distance[1:50, 1:50],
    distance.thresholds = 1000,
    spatial.predictors.df = spatial_preds,
    ranking.method = "moran",
    verbose = FALSE
  )

  # Check all required components
  expect_true("method" %in% names(result))
  expect_true("criteria" %in% names(result))
  expect_true("ranking" %in% names(result))
  expect_true("spatial.predictors.df" %in% names(result))

  # Check types
  expect_true(is.character(result$method))
  expect_s3_class(result$criteria, "data.frame")
  expect_true(is.character(result$ranking))
  expect_s3_class(result$spatial.predictors.df, "data.frame")
})

test_that("rank_spatial_predictors() filters redundant predictors", {
  data(plants_df, plants_distance)

  spatial_preds <- mem_multithreshold(
    distance.matrix = plants_distance[1:50, 1:50],
    distance.thresholds = c(100, 500, 1000)
  )

  result <- rank_spatial_predictors(
    distance.matrix = plants_distance[1:50, 1:50],
    distance.thresholds = c(100, 500, 1000),
    spatial.predictors.df = spatial_preds,
    ranking.method = "moran",
    verbose = FALSE
  )

  # Should filter out some redundant predictors
  expect_true(ncol(result$spatial.predictors.df) <= ncol(spatial_preds))
  expect_true(length(result$ranking) <= ncol(spatial_preds))
})

test_that("rank_spatial_predictors() verbose parameter works", {
  data(plants_df, plants_distance)

  spatial_preds <- mem(
    distance.matrix = plants_distance[1:50, 1:50],
    distance.threshold = 1000
  )

  # verbose = FALSE should not error
  expect_silent(
    result <- rank_spatial_predictors(
      distance.matrix = plants_distance[1:50, 1:50],
      distance.thresholds = 1000,
      spatial.predictors.df = spatial_preds,
      ranking.method = "moran",
      verbose = FALSE
    )
  )

  expect_type(result, "list")
})

test_that("rank_spatial_predictors() handles multiple distance thresholds", {
  data(plants_df, plants_distance)

  spatial_preds <- mem_multithreshold(
    distance.matrix = plants_distance[1:50, 1:50],
    distance.thresholds = c(100, 1000, 2000)
  )

  result <- rank_spatial_predictors(
    distance.matrix = plants_distance[1:50, 1:50],
    distance.thresholds = c(100, 1000, 2000),
    spatial.predictors.df = spatial_preds,
    ranking.method = "moran",
    verbose = FALSE
  )

  expect_type(result, "list")
  expect_s3_class(result$criteria, "data.frame")
  expect_true(nrow(result$criteria) > 0)
})
