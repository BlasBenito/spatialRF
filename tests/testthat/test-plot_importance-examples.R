testthat::test_that("`plot_importance()` works", {

  #loading data
  data(
    ecoregions_df,
    ecoregions_distance_matrix,
    ecoregions_numeric_predictors,
    ecoregions_continuous_response
  )

  rf.model <- rf(
    data = ecoregions_df,
    dependent.variable.name = ecoregions_continuous_response,
    predictor.variable.names = ecoregions_numeric_predictors,
    distance.matrix = ecoregions_distance_matrix,
    distance.thresholds = c(0, 1000, 2000),
    verbose = FALSE
  )

  p <- plot_importance(rf.model, verbose = FALSE)

  testthat::expect_equal(
    inherits(p, "ggplot"),
    TRUE
    )
})
