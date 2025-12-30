test_that("make_spatial_fold() validates xy.i column names", {
  data(plants_xy)

  # Missing columns
  xy_wrong <- plants_xy
  names(xy_wrong) <- c("id", "longitude", "latitude")

  expect_error(
    make_spatial_fold(
      xy.i = xy_wrong[1, ],
      xy = plants_xy,
      training.fraction = 0.6
    ),
    "xy.i must contain the column names 'id', 'x', and 'y'"
  )
})

test_that("make_spatial_fold() validates xy column names", {
  data(plants_xy)

  # Missing columns
  xy_wrong <- plants_xy
  names(xy_wrong) <- c("id", "longitude", "latitude")

  expect_error(
    make_spatial_fold(
      xy.i = plants_xy[1, ],
      xy = xy_wrong,
      training.fraction = 0.6
    ),
    "xy must contain the column names 'id', 'x', and 'y'"
  )
})

test_that("make_spatial_fold() validates training.fraction", {
  data(plants_xy)

  # training.fraction >= 1
  expect_error(
    make_spatial_fold(
      xy.i = plants_xy[1, ],
      xy = plants_xy,
      training.fraction = 1.0
    ),
    "training.fraction should be a number between 0.1 and 0.9"
  )

  expect_error(
    make_spatial_fold(
      xy.i = plants_xy[1, ],
      xy = plants_xy,
      training.fraction = 1.5
    ),
    "training.fraction should be a number between 0.1 and 0.9"
  )
})

test_that("make_spatial_fold() works with basic inputs", {
  data(plants_xy)

  result <- make_spatial_fold(
    xy.i = plants_xy[1, ],
    xy = plants_xy,
    training.fraction = 0.6
  )

  expect_type(result, "logical")
  expect_length(result, nrow(plants_xy))

  # Check that we have both training and testing records
  expect_true(sum(result) > 0)  # Has training records
  expect_true(sum(!result) > 0)  # Has testing records

  # Check that training and testing together cover all records
  expect_equal(sum(result) + sum(!result), nrow(plants_xy))
})

test_that("make_spatial_fold() handles distance.step.x vector", {
  data(plants_xy)

  # Pass vector for distance.step.x (should use first element)
  result <- make_spatial_fold(
    xy.i = plants_xy[1, ],
    xy = plants_xy,
    distance.step.x = c(100, 200, 300),
    training.fraction = 0.6
  )

  expect_type(result, "logical")
  expect_length(result, nrow(plants_xy))
})

test_that("make_spatial_fold() handles distance.step.y vector", {
  data(plants_xy)

  # Pass vector for distance.step.y (should use first element)
  result <- make_spatial_fold(
    xy.i = plants_xy[1, ],
    xy = plants_xy,
    distance.step.y = c(100, 200, 300),
    training.fraction = 0.6
  )

  expect_type(result, "logical")
  expect_length(result, nrow(plants_xy))
})

test_that("make_spatial_fold() works with custom distance steps", {
  data(plants_xy)

  # Use smaller distance steps to avoid including all points
  result <- make_spatial_fold(
    xy.i = plants_xy[1, ],
    xy = plants_xy,
    distance.step.x = 10,
    distance.step.y = 10,
    training.fraction = 0.6
  )

  expect_type(result, "logical")
  expect_true(sum(result) > 0)  # Has training records
  expect_true(sum(!result) > 0)  # Has testing records
})

test_that("make_spatial_fold() works with different training.fraction values", {
  data(plants_xy)

  result_small <- make_spatial_fold(
    xy.i = plants_xy[1, ],
    xy = plants_xy,
    training.fraction = 0.2
  )

  result_large <- make_spatial_fold(
    xy.i = plants_xy[1, ],
    xy = plants_xy,
    training.fraction = 0.8
  )

  # Larger training.fraction should have more training records
  expect_true(sum(result_large) > sum(result_small))
  expect_true(sum(!result_large) < sum(!result_small))
})

test_that("make_spatial_fold() works with binary response variable", {
  data(plants_df, plants_xy)

  # Create binary response
  plants_df$binary_response <- ifelse(
    plants_df$richness_species_vascular > median(plants_df$richness_species_vascular),
    1, 0
  )

  result <- make_spatial_fold(
    data = plants_df[1:100, ],
    dependent.variable.name = "binary_response",
    xy.i = plants_xy[1, ],
    xy = plants_xy[1:100, ],
    training.fraction = 0.6
  )

  expect_type(result, "logical")
  expect_length(result, 100)
  expect_true(sum(result) > 0)  # Has training records
  expect_true(sum(!result) > 0)  # Has testing records

  # Verify complete coverage
  expect_equal(sum(result) + sum(!result), 100)
})

test_that("make_spatial_fold() respects training.fraction for binary response", {
  data(plants_df, plants_xy)

  # Create binary response with known distribution
  plants_df$binary_response <- ifelse(
    plants_df$richness_species_vascular > median(plants_df$richness_species_vascular),
    1, 0
  )

  result <- make_spatial_fold(
    data = plants_df[1:100, ],
    dependent.variable.name = "binary_response",
    xy.i = plants_xy[1, ],
    xy = plants_xy[1:100, ],
    training.fraction = 0.5
  )

  # For binary, training.fraction applies to presences (1s)
  total_presences <- sum(plants_df$binary_response[1:100])
  training_presences <- sum(plants_df$binary_response[1:100][result])

  # Should be approximately half of the presences in training
  expect_true(training_presences <= total_presences)
  expect_true(training_presences > 0)
})

test_that("make_spatial_fold() works without data for non-binary case", {
  data(plants_xy)

  # Should work without data and dependent.variable.name
  result <- make_spatial_fold(
    xy.i = plants_xy[1, ],
    xy = plants_xy,
    training.fraction = 0.7
  )

  expect_type(result, "logical")
  expect_true(sum(result) > 0)
})

test_that("make_spatial_fold() preserves spatial structure", {
  data(plants_xy)

  result <- make_spatial_fold(
    xy.i = plants_xy[1, ],
    xy = plants_xy,
    training.fraction = 0.6
  )

  # Training records should be spatially clustered around xy.i
  training_coords <- plants_xy[result, ]
  testing_coords <- plants_xy[!result, ]

  # Mean distance of training from focal point should be smaller than testing
  focal_x <- plants_xy$x[1]
  focal_y <- plants_xy$y[1]

  mean_dist_training <- mean(sqrt((training_coords$x - focal_x)^2 +
                                   (training_coords$y - focal_y)^2))
  mean_dist_testing <- mean(sqrt((testing_coords$x - focal_x)^2 +
                                  (testing_coords$y - focal_y)^2))

  expect_true(mean_dist_training < mean_dist_testing)
})
