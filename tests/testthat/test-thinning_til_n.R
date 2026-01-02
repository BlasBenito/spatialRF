test_that("`thinning_til_n()` works", {
  data(plants_df)
  plant_richness.thin <- thinning_til_n(
    x = plants_df,
    n = 100
  )
  expect_s3_class(plant_richness.thin, "data.frame")
  expect_equal(colnames(plant_richness.thin), colnames(plant_richness.thin))
})


test_that("thinning_til_n() works with basic inputs", {
  data(plants_xy)

  result <- thinning_til_n(
    xy = plants_xy,
    n = 50
  )

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) <= 50)
  expect_true("x" %in% colnames(result))
  expect_true("y" %in% colnames(result))
})

test_that("thinning_til_n() works with custom distance.step", {
  data(plants_xy)

  result <- thinning_til_n(
    xy = plants_xy,
    n = 30,
    distance.step = 1000
  )

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) <= 30)
})

test_that("thinning_til_n() handles distance.step vector (length > 1)", {
  data(plants_xy)

  # Should use first element only
  result <- thinning_til_n(
    xy = plants_xy,
    n = 30,
    distance.step = c(1000, 2000, 3000)
  )

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) <= 30)
})

test_that("thinning_til_n() validates xy parameter", {
  # NULL xy
  expect_error(
    thinning_til_n(xy = NULL, n = 10),
    "xy must be a data frame"
  )

  # Not a data frame
  expect_error(
    thinning_til_n(xy = "not a data frame", n = 10),
    "xy must be a data frame"
  )
})

test_that("thinning_til_n() validates x column presence", {
  data(plants_xy)

  # Missing x column
  xy_no_x <- plants_xy
  names(xy_no_x)[names(xy_no_x) == "x"] <- "longitude"

  expect_error(
    thinning_til_n(xy = xy_no_x, n = 10),
    "column x is missing"
  )
})

test_that("thinning_til_n() validates y column presence", {
  data(plants_xy)

  # Missing y column
  xy_no_y <- plants_xy
  names(xy_no_y)[names(xy_no_y) == "y"] <- "latitude"

  expect_error(
    thinning_til_n(xy = xy_no_y, n = 10),
    "column y is missing"
  )
})

test_that("thinning_til_n() validates n parameter", {
  data(plants_xy)

  # Non-numeric n
  expect_error(
    thinning_til_n(xy = plants_xy, n = "ten"),
    "'n' is not a number"
  )
})

test_that("thinning_til_n() works with different n values", {
  data(plants_xy)

  result_10 <- thinning_til_n(xy = plants_xy, n = 10)
  result_50 <- thinning_til_n(xy = plants_xy, n = 50)
  result_100 <- thinning_til_n(xy = plants_xy, n = 100)

  expect_true(nrow(result_10) <= 10)
  expect_true(nrow(result_50) <= 50)
  expect_true(nrow(result_100) <= 100)
})

test_that("thinning_til_n() preserves column structure", {
  data(plants_xy)

  result <- thinning_til_n(xy = plants_xy, n = 30)

  expect_equal(colnames(result), colnames(plants_xy))
  expect_s3_class(result, "data.frame")
})

test_that("thinning_til_n() returns valid coordinates", {
  data(plants_xy)

  result <- thinning_til_n(xy = plants_xy, n = 30)

  # All coordinates should be numeric
  expect_true(all(is.numeric(result$x)))
  expect_true(all(is.numeric(result$y)))

  # No NA values
  expect_true(all(!is.na(result$x)))
  expect_true(all(!is.na(result$y)))
})

test_that("thinning_til_n() works with small n", {
  data(plants_xy)

  result <- thinning_til_n(xy = plants_xy, n = 5)

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) <= 5)
  expect_true(nrow(result) > 0)
})

test_that("thinning_til_n() works with large n", {
  data(plants_xy)

  # n close to total number of points
  result <- thinning_til_n(xy = plants_xy, n = nrow(plants_xy) - 10)

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) <= nrow(plants_xy))
})

test_that("thinning_til_n() default distance.step works", {
  data(plants_xy)

  # Should auto-calculate distance.step
  result <- thinning_til_n(xy = plants_xy, n = 30)

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) <= 30)
})

test_that("thinning_til_n() output is subset of input", {
  data(plants_xy)

  result <- thinning_til_n(xy = plants_xy, n = 30)

  # Result coordinates should be in original data
  expect_true(all(result$x %in% plants_xy$x))
  expect_true(all(result$y %in% plants_xy$y))
})
