testthat::test_that("`plot_response_curves()` works", {

  library(spatialRF)
  library(magrittr)

  #loading data
  data(
    ecoregions_df,
    ecoregions_distance_matrix,
    ecoregions_predictor_variable_names,
    ecoregions_dependent_variable_name
  )

  ecoregions_df <- tibble::as_tibble(ecoregions_df)

  #fitting model
  out <- rf(
    data = ecoregions_df,
    dependent.variable.name = ecoregions_dependent_variable_name,
    predictor.variable.names = ecoregions_predictor_variable_names,
    distance.matrix = ecoregions_distance_matrix,
    distance.thresholds = c(0,100, 1000, 10000),
    verbose = FALSE
  )

  p <- plot_response_curves(out)

  testthat::expect_type(p, "list")
  testthat::expect_s3_class(p[[1]], "ggplot")

})
