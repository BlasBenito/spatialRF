testthat::test_that("`rf()` works", {

  library(spatialRF)
  library(magrittr)
  library(doParallel)

  #loading data
   data(
    ecoregions_df,
    ecoregions_distance_matrix,
    ecoregions_predvar_names,
    ecoregions_depvar_name
    )

  #selection without preference.order and without jackknife
  out1 <- rf_select(
    data = ecoregions_df,
    dependent.variable.name = ecoregions_depvar_name,
    predictor.variable.names = ecoregions_predvar_names,
    distance.matrix = ecoregions_distance_matrix,
    distance.thresholds = c(0,100, 1000, 10000),
    verbose = FALSE
  )

  testthat::expect_s3_class(
    out1$variable.selection$univariate.importance,
    "data.frame"
  )

  testthat::expect_named(
    out1$variable.selection,
    c("univariate.importance", "jackknife.result", "cor", "vif", "selected.variables")
    )


  #selection with preference.order and without jackknife
  out2 <- rf_select(
    data = ecoregions_df,
    dependent.variable.name = ecoregions_depvar_name,
    predictor.variable.names = ecoregions_predvar_names,
    preference.order = c("ecoregion_area_km2", "neighbors_count", "climate_bio1_average", "climate_bio12_average"),
    distance.matrix = ecoregions_distance_matrix,
    distance.thresholds = c(0,100, 1000, 10000),
    verbose = FALSE
  )

  testthat::expect_equal(
    out2$variable.selection$univariate.importance, expected = NA
  )

  testthat::expect_named(
    out2$variable.selection,
    c("univariate.importance", "jackknife.result", "cor", "vif", "selected.variables")
  )

  #selection with preference.order and with jackknife
  out3 <- rf_select(
    data = ecoregions_df,
    dependent.variable.name = ecoregions_depvar_name,
    predictor.variable.names = ecoregions_predvar_names,
    preference.order = c("ecoregion_area_km2", "neighbors_count", "climate_bio1_average", "climate_bio12_average"),
    jackknife = TRUE,
    distance.matrix = ecoregions_distance_matrix,
    distance.thresholds = c(0,100, 1000, 10000),
    verbose = FALSE
  )

  testthat::expect_equal(
    out2$variable.selection$univariate.importance, expected = NA
  )

  testthat::expect_named(
    out2$variable.selection,
    c("univariate.importance", "jackknife.result", "cor", "vif", "selected.variables")
  )


   #with cluster
   out4 <- rf_select(
     data = ecoregions_df,
     dependent.variable.name = ecoregions_depvar_name,
     predictor.variable.names = ecoregions_predvar_names,
     preference.order = c("ecoregion_area_km2", "neighbors_count", "climate_bio1_average", "climate_bio12_average"),
     jackknife = TRUE,
     distance.matrix = ecoregions_distance_matrix,
     distance.thresholds = c(0,100, 1000, 10000),
     cluster = make_cluster(),
     verbose = FALSE
   )

   stop_cluster()

   testthat::expect_equal(
     out4$variable.selection$univariate.importance, expected = NA
   )

   testthat::expect_named(
     out4$variable.selection,
     c("univariate.importance", "jackknife.result", "cor", "vif", "selected.variables")
   )


})
