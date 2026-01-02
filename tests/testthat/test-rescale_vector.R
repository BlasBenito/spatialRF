test_that("`rescale_vector()` works", {
  out <- rescale_vector(
    x = rnorm(100),
    new.min = 0,
    new.max = 100,
    integer = TRUE
  )
  expect_equal(length(out), length(rnorm(100)))
  expect_equal(min(out), 0)
  expect_equal(max(out), 100)
})
