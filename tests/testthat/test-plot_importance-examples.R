testthat::test_that("`plot_importance()` works", {

  #loading data
  data(
    ecoregions_df,
    ecoregions_distance_matrix,
    ecoregions_predictor_variable_names,
    ecoregions_dependent_variable_name
  )

  rf.model <- rf(
    data = ecoregions_df,
    dependent.variable.name = ecoregions_dependent_variable_name,
    predictor.variable.names = ecoregions_predictor_variable_names,
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
