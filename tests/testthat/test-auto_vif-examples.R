testthat::test_that("`auto_vif()` works", {

  library(magrittr)

  data(
    ecoregions_df,
    ecoregions_continuous_response,
    ecoregions_numeric_predictors,
    ecoregions_all_predictors
  )

  #testing vif
  ###############################
  vifs <- vif(data = ecoregions_df[, ecoregions_numeric_predictors])

  # Test that all VIF values are below the threshold
  testthat::expect_true(all(vifs$vif < vif_threshold))

  # Test that the function returns a data frame with the correct structure
  testthat::expect_is(vifs, "data.frame")
  testthat::expect_equal(names(vifs), c("variable", "vif"))
  testthat::expect_is(vifs$variable, "character")
  testthat::expect_is(vifs$vif, "numeric")


  #testing auto_vif
  #################################


})
