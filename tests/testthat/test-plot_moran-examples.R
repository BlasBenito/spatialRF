testthat::test_that("`plot_moran()` works", {

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

   p <- plot_moran(out, verbose = FALSE)

   testthat::expect_equal(inherits(p, "ggplot"), TRUE)
})
