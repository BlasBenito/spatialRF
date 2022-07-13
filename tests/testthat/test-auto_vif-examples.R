testthat::test_that("`auto_vif()` works", {

  library(spatialRF)
  library(magrittr)

  data(
    ecoregions_df,
    ecoregions_distance_matrix,
    ecoregions_predvar_names,
    ecoregions_depvar_name
  )

  testthat::expect_warning(
    out <- auto_vif(
      x = ecoregions_df,
      verbose = FALSE,
      preference.order = ecoregions_predvar_names
    )
  )
  testthat::expect_type(out, "list")
  testthat::expect_length(out, 3)
  testthat::expect_s3_class(out$vif, "data.frame")
  testthat::expect_named(out, c("vif", "selected.variables", "selected.variables.df"))

  #adding auto_cor()
  testthat::expect_warning(
    out <- auto_vif(
      x = ecoregions_df,
      verbose = FALSE,
      preference.order = ecoregions_predvar_names
    ) %>%
      auto_cor()
  )
  testthat::expect_type(out, "list")
  testthat::expect_length(out, 3)
  testthat::expect_named(out, c("cor", "selected.variables", "selected.variables.df"))

})
