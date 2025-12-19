test_that("`plot_importance()` works", {
  data(plants_rf)

  p <- plot_importance(plants_rf, verbose = FALSE)
  expect_equal(inherits(p, "ggplot"), TRUE)
})
