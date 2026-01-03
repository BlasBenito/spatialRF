test_that("filter_spatial_predictors() works with basic inputs", {
  data(plants_df, plants_predictors, plants_distance)

  # Generate spatial predictors
  mem_df <- mem_multithreshold(
    distance.matrix = plants_distance,
    distance.thresholds = c(100, 1000)
  )

  filtered <- filter_spatial_predictors(
    data = plants_df,
    predictor.variable.names = plants_predictors,
    spatial.predictors.df = mem_df,
    cor.threshold = 0.50
  )

  expect_s3_class(filtered, "data.frame")
  expect_true(ncol(filtered) <= ncol(mem_df))
  expect_equal(nrow(filtered), nrow(mem_df))
})

test_that("filter_spatial_predictors() reduces number of predictors", {
  data(plants_df, plants_predictors, plants_distance)

  mem_df <- mem_multithreshold(
    distance.matrix = plants_distance,
    distance.thresholds = c(100, 1000, 5000)
  )

  filtered <- filter_spatial_predictors(
    data = plants_df,
    predictor.variable.names = plants_predictors,
    spatial.predictors.df = mem_df,
    cor.threshold = 0.50
  )

  # Should filter out some predictors
  expect_true(ncol(filtered) < ncol(mem_df))
})

test_that("filter_spatial_predictors() cor.threshold affects filtering", {
  data(plants_df, plants_predictors, plants_distance)

  mem_df <- mem_multithreshold(
    distance.matrix = plants_distance,
    distance.thresholds = c(100, 1000)
  )

  # Strict threshold (low) - fewer predictors retained
  filtered_strict <- filter_spatial_predictors(
    data = plants_df,
    predictor.variable.names = plants_predictors,
    spatial.predictors.df = mem_df,
    cor.threshold = 0.30
  )

  # Lenient threshold (high) - more predictors retained
  filtered_lenient <- filter_spatial_predictors(
    data = plants_df,
    predictor.variable.names = plants_predictors,
    spatial.predictors.df = mem_df,
    cor.threshold = 0.75
  )

  expect_true(ncol(filtered_strict) <= ncol(filtered_lenient))
})

test_that("filter_spatial_predictors() output has correct structure", {
  data(plants_df, plants_predictors, plants_distance)

  mem_df <- mem_multithreshold(
    distance.matrix = plants_distance,
    distance.thresholds = c(100, 1000)
  )

  filtered <- filter_spatial_predictors(
    data = plants_df,
    predictor.variable.names = plants_predictors,
    spatial.predictors.df = mem_df,
    cor.threshold = 0.50
  )

  # Output should be data frame
  expect_s3_class(filtered, "data.frame")

  # Same number of rows
  expect_equal(nrow(filtered), nrow(mem_df))

  # All columns should be numeric
  expect_true(all(sapply(filtered, is.numeric)))

  # Column names should be subset of original
  expect_true(all(colnames(filtered) %in% colnames(mem_df)))
})

test_that("filter_spatial_predictors() handles very strict threshold", {
  data(plants_df, plants_predictors, plants_distance)

  mem_df <- mem_multithreshold(
    distance.matrix = plants_distance,
    distance.thresholds = c(100, 1000)
  )

  # Very strict threshold might filter out most/all predictors
  filtered <- filter_spatial_predictors(
    data = plants_df,
    predictor.variable.names = plants_predictors,
    spatial.predictors.df = mem_df,
    cor.threshold = 0.10
  )

  expect_s3_class(filtered, "data.frame")
  expect_equal(nrow(filtered), nrow(mem_df))
  # Very few or no predictors retained
  expect_true(ncol(filtered) <= ncol(mem_df))
})

test_that("filter_spatial_predictors() handles very lenient threshold", {
  data(plants_df, plants_predictors, plants_distance)

  mem_df <- mem_multithreshold(
    distance.matrix = plants_distance,
    distance.thresholds = c(100, 1000)
  )

  # Very lenient threshold - should retain most/all predictors
  filtered <- filter_spatial_predictors(
    data = plants_df,
    predictor.variable.names = plants_predictors,
    spatial.predictors.df = mem_df,
    cor.threshold = 0.95
  )

  expect_s3_class(filtered, "data.frame")
  expect_equal(nrow(filtered), nrow(mem_df))
  # Most predictors should be retained
  expect_true(ncol(filtered) >= ncol(mem_df) * 0.5)
})

test_that("filter_spatial_predictors() works with different threshold values", {
  data(plants_df, plants_predictors, plants_distance)

  mem_df <- mem_multithreshold(
    distance.matrix = plants_distance,
    distance.thresholds = c(100, 1000)
  )

  thresholds <- c(0.30, 0.50, 0.70, 0.90)
  results <- list()

  for (thresh in thresholds) {
    results[[as.character(thresh)]] <- filter_spatial_predictors(
      data = plants_df,
      predictor.variable.names = plants_predictors,
      spatial.predictors.df = mem_df,
      cor.threshold = thresh
    )
  }

  # Number of retained predictors should generally increase with threshold
  n_cols <- sapply(results, ncol)
  names(n_cols) <- thresholds

  # Check that trend is generally increasing (allowing for some variation)
  expect_true(n_cols["0.9"] >= n_cols["0.3"])
})

test_that("filter_spatial_predictors() default threshold is 0.50", {
  data(plants_df, plants_predictors, plants_distance)

  mem_df <- mem_multithreshold(
    distance.matrix = plants_distance,
    distance.thresholds = c(100, 1000)
  )

  filtered_default <- filter_spatial_predictors(
    data = plants_df,
    predictor.variable.names = plants_predictors,
    spatial.predictors.df = mem_df
  )

  filtered_explicit <- filter_spatial_predictors(
    data = plants_df,
    predictor.variable.names = plants_predictors,
    spatial.predictors.df = mem_df,
    cor.threshold = 0.50
  )

  expect_equal(ncol(filtered_default), ncol(filtered_explicit))
  expect_equal(colnames(filtered_default), colnames(filtered_explicit))
})

test_that("filter_spatial_predictors() preserves row order", {
  data(plants_df, plants_predictors, plants_distance)

  mem_df <- mem_multithreshold(
    distance.matrix = plants_distance,
    distance.thresholds = c(100, 1000)
  )

  filtered <- filter_spatial_predictors(
    data = plants_df,
    predictor.variable.names = plants_predictors,
    spatial.predictors.df = mem_df,
    cor.threshold = 0.50
  )

  # Row order should be preserved
  expect_equal(nrow(filtered), nrow(mem_df))

  # Check that values in first column match (if any columns retained)
  if (ncol(filtered) > 0) {
    col_name <- colnames(filtered)[1]
    expect_equal(filtered[[col_name]], mem_df[[col_name]])
  }
})

test_that("filter_spatial_predictors() works with mem() output", {
  data(plants_df, plants_predictors, plants_distance)

  # Single threshold using mem()
  mem_df <- mem(
    distance.matrix = plants_distance,
    distance.threshold = 1000
  )

  filtered <- filter_spatial_predictors(
    data = plants_df,
    predictor.variable.names = plants_predictors,
    spatial.predictors.df = mem_df,
    cor.threshold = 0.60
  )

  expect_s3_class(filtered, "data.frame")
  expect_true(ncol(filtered) <= ncol(mem_df))
  expect_equal(nrow(filtered), nrow(mem_df))
})

test_that("filter_spatial_predictors() filters both internally and vs non-spatial", {
  data(plants_df, plants_predictors, plants_distance)

  # Generate many potentially correlated spatial predictors
  mem_df <- mem_multithreshold(
    distance.matrix = plants_distance,
    distance.thresholds = c(100, 500, 1000, 2000)
  )

  filtered <- filter_spatial_predictors(
    data = plants_df,
    predictor.variable.names = plants_predictors,
    spatial.predictors.df = mem_df,
    cor.threshold = 0.50
  )

  # Should significantly reduce number due to both types of filtering
  expect_true(ncol(filtered) < ncol(mem_df))

  # Filtered predictors should have low correlation with non-spatial
  if (ncol(filtered) > 0) {
    non_spatial <- plants_df[, plants_predictors[1], drop = FALSE]
    if (is.numeric(non_spatial[[1]])) {
      cors <- abs(cor(non_spatial, filtered, use = "pairwise.complete.obs"))
      expect_true(all(cors < 0.50 | is.na(cors)))
    }
  }
})
