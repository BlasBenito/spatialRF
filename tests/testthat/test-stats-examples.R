testthat::test_that("`auc()` works", {

    #auc
    out <- auc(
      o = sample(x = c(1, 0), size = 10, replace = TRUE),
      p = runif(10)
      )
    testthat::expect_length(out, 1)
    testthat::expect_type(out, "double")

    #statistical_mode
    out <- statistical_mode(c(rep(10, 10), rep(9, 9)))
    testthat::expect_equal(out, 10)

    #standard_error
    out <- standard_error(1:10)
    testthat::expect_true(round(0.957, 3) == round(out, 3))


    #root_mean_squared_error
    out <- root_mean_squared_error(o = runif(10), p = runif(10))
    testthat::expect_true(!is.na(out) == TRUE)

    out <- root_mean_squared_error(o = runif(10), p = runif(10))
    testthat::expect_named(out, "rmse")
    testthat::expect_true(!is.na(out) == TRUE)

    out <- root_mean_squared_error(o = runif(10), p = runif(10), normalization = "iq")
    testthat::expect_named(out, "iq")
    testthat::expect_true(!is.na(out) == TRUE)

    out <- root_mean_squared_error(o = runif(10), p = runif(10), normalization = "mean")
    testthat::expect_named(out, "mean")
    testthat::expect_true(!is.na(out) == TRUE)

    out <- root_mean_squared_error(o = runif(10), p = runif(10), normalization = "sd")
    testthat::expect_named(out, "sd")
    testthat::expect_true(!is.na(out) == TRUE)

    out <- root_mean_squared_error(o = runif(10), p = runif(10), normalization = "maxmin")
    testthat::expect_named(out, "maxmin")
    testthat::expect_true(!is.na(out) == TRUE)

    out <- root_mean_squared_error(o = runif(10), p = runif(10), normalization = "all")
    testthat::expect_named(out, c("rmse", "iq", "maxmin", "mean", "sd"))
    testthat::expect_true(sum(!is.na(out)) == 5)
})
