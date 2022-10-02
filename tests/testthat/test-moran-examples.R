testthat::test_that("`moran()` works", {

  library(spatialRF)
  library(magrittr)

  #loading data
  data(
    ecoregions_df,
    ecoregions_distance_matrix,
    ecoregions_predictor_variable_names,
    ecoregions_dependent_variable_name
  )

   x <- moran(
     ecoregions_df$plant_richness,
     distance.matrix = ecoregions_distance_matrix,
     verbose = FALSE
     )
   testthat::expect_type(x, "list")
   testthat::expect_s3_class(x$test, "data.frame")
   testthat::expect_s3_class(x$plot.df, "data.frame")
   testthat::expect_length(x, 3)
   testthat::expect_named(x, c("test", "plot", "plot.df"))

   x <- moran_multithreshold(
     ecoregions_df$plant_richness,
     distance.matrix = ecoregions_distance_matrix,
     distance.thresholds = c(0, 100, 1000),
     verbose = FALSE
   )
   testthat::expect_type(x, "list")
   testthat::expect_s3_class(x$per.distance, "data.frame")
   testthat::expect_named(x$per.distance, c("distance.threshold", "moran.i", "moran.i.null", "p.value", "interpretation"))
   testthat::expect_length(x, 4)

})
