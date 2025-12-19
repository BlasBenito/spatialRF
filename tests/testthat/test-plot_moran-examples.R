test_that("`plot_moran()` works", {
  data(plants_rf)

  p <- plot_moran(plants_rf, verbose = FALSE)
  expect_equal(inherits(p, "ggplot"), TRUE)
})
