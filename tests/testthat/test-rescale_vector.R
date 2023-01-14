testthat::test_that("`rescale_vector()` works", {
  out <- rescale_vector(
    x = rnorm(100), new.min = 0, new.max = 100,
    integer = TRUE
  )
  testthat::expect_equal(length(out), length(rnorm(100)))
  testthat::expect_equal(min(out), 0)
  testthat::expect_equal(max(out), 100)
})
