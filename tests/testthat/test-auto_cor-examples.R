testthat::test_that("`auto_cor()` works", {

  library(spatialRF)
  library(magrittr)

  data(
    ecoregions_df,
    ecoregions_distance_matrix,
    ecoregions_predvar_names,
    ecoregions_depvar_name
  )

  testthat::expect_warning(
    out <- auto_cor(
      x = ecoregions_df,
      verbose = FALSE,
      preference.order = ecoregions_predvar_names[1:10]
    )
  )
  testthat::expect_type(out, "list")
  testthat::expect_length(out, 3)
  testthat::expect_named(out, c("cor", "selected.variables", "selected.variables.df"))

  #adding auto_vif()
  testthat::expect_warning(
    out <- auto_cor(
      x = ecoregions_df,
      verbose = FALSE,
      preference.order = ecoregions_predvar_names
    ) %>%
      auto_vif()
  )
  testthat::expect_type(out, "list")
  testthat::expect_length(out, 3)
  testthat::expect_named(out, c("vif", "selected.variables", "selected.variables.df"))

})
