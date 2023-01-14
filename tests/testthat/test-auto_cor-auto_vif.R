testthat::test_that("`auto_cor()` works", {

  library(spatialRF)
  library(magrittr)

  data(
    ecoregions_df,
    ecoregions_all_predictors
  )

  #auto_cor()
  ###########################

  #regular input
  testthat::expect_warning(
    out <- auto_cor(
      x = ecoregions_df[, ecoregions_all_predictors],
      verbose = TRUE,
      preference.order = ecoregions_all_predictors[1:5]
    ))

  testthat::expect_type(out, "list")
  testthat::expect_length(out, 3)
  testthat::expect_named(out, c("cor", "selected.variables", "selected.variables.df"))

  #null preference order
  testthat::expect_warning(
    out <- auto_cor(
      x = ecoregions_df[, ecoregions_all_predictors],
      verbose = TRUE,
      preference.order = NULL
    ))

  testthat::expect_type(out, "list")
  testthat::expect_length(out, 3)
  testthat::expect_named(out, c("cor", "selected.variables", "selected.variables.df"))

  #using result of autovif
  testthat::expect_warning(
    out <- auto_vif(
      x = ecoregions_df[, ecoregions_all_predictors],
      verbose = FALSE,
      preference.order = ecoregions_all_predictors[1:5]
    ) %>%
      auto_cor()
    )

  #not colinear variables
  testthat::expect_warning(
    out <- auto_vif(
      x = out$selected.variables.df,
      verbose = FALSE,
      preference.order = out$selected.variables
    ) %>%
      auto_cor()
  )


  #adding zero variance column
  ecoregions_df$zeros <- runif(nrow(ecoregions_df))/1000
  #
  ecoregions_all_predictors <- c(ecoregions_all_predictors, "zeros")

  #input to tibble
  ecoregions_df <- tibble::as_tibble(ecoregions_df)

  testthat::expect_warning(
    out <- auto_cor(
      x = ecoregions_df[, ecoregions_all_predictors],
      verbose = FALSE,
      preference.order = ecoregions_all_predictors[1:5]
    )
  )

  testthat::expect_type(out, "list")
  testthat::expect_length(out, 3)
  testthat::expect_named(out, c("cor", "selected.variables", "selected.variables.df"))


  #auto_vif()
  ##############################
  ecoregions_df <- tibble::as_tibble(ecoregions_df)

  testthat::expect_warning(
    out <- auto_vif(
      x = ecoregions_df[, ecoregions_all_predictors],
      verbose = TRUE,
      preference.order = NULL
    ))

  testthat::expect_type(out, "list")
  testthat::expect_length(out, 3)
  testthat::expect_named(out, c("vif", "selected.variables", "selected.variables.df"))

  testthat::expect_warning(
    out <- auto_cor(
      x = ecoregions_df[, ecoregions_all_predictors],
      verbose = TRUE,
      preference.order = NULL
    ) %>%
      auto_vif())

  testthat::expect_type(out, "list")
  testthat::expect_length(out, 3)
  testthat::expect_named(out, c("vif", "selected.variables", "selected.variables.df"))

})
