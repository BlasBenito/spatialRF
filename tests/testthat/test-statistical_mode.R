test_that("`statistical_mode()` works", {
  out <- statistical_mode(c(rep(10, 10), rep(9, 9)))
  expect_equal(out, 10)
})
