testthat::test_that("`make_spatial_folds()` works", {


  training.fraction <- 0.75

  data(
    ecoregions_df,
    ecoregions_distance_matrix,
    ecoregions_numeric_predictors,
    ecoregions_continuous_response
  )

  ecoregions_df <- tibble::as_tibble(ecoregions_df)

  xy <- ecoregions_df[, c("x", "y")]

  fold.centers <- thinning_til_n(
    xy = xy,
    n = 20
    )

  out <- make_spatial_folds(
    fold.centers = fold.centers,
    xy = xy,
    training.fraction = 0.6,
    n.cores = 1
  )

  testthat::expect_type(out, "list")
  testthat::expect_length(out, nrow(fold.centers))
  testthat::expect_named(out[[1]], c("training", "testing"))
  testthat::expect_type(out[[1]]$training, "integer")
  testthat::expect_type(out[[1]]$testing, "integer")


  out.1 <- make_spatial_fold(
    data = ecoregions_df,
    dependent.variable.name = ecoregions_continuous_response,
    fold.center = xy[1, ],
    xy = xy,
    training.fraction = training.fraction
  )

  testthat::expect_named(out.1, c("training", "testing"))
  testthat::expect_type(out.1$training, "integer")
  testthat::expect_type(out.1$testing, "integer")
  testthat::expect_equal(
    length(intersect(
      x = out.1$training,
      y = out.1$testing
    )),
    0
  )

  out.2 <- make_spatial_fold(
    data = ecoregions_df,
    dependent.variable.name = ecoregions_continuous_response,
    fold.center = xy[1, ],
    xy = xy,
    training.fraction = training.fraction,
    swap.spatial.folds = TRUE
  )

  testthat::expect_named(out.2, c("training", "testing"))
  testthat::expect_type(out.2$training, "integer")
  testthat::expect_type(out.2$testing, "integer")


})
