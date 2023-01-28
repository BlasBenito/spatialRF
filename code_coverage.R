library(covr)
library(codetools)


# get list of functions called by a particular function
codetools::findGlobals(fun = rf)

#fe_target_encoding
covr::report(
  x = covr::file_coverage(
    source_files = c(
      "R/fe_target_encoding.R",
      "R/fe_target_encoding_methods.R"
    ),
    test_files = "tests/testthat/test-fe_target_encoding.R"
  )
)

#rf
covr::report(
  x = covr::file_coverage(
    source_files = c(
      "R/rf.R",
      "R/stats.R",
      "R/internal.R",
      "R/internal.R",
      "R/moran.R",
      "R/residuals_diagnostics.R",
      "R/print.R",
      "R/print_importance.R",
      "R/print_moran.R",
      "R/print_performance.R"
    ),
    test_files = "tests/testthat/test-rf.R"
  )
)

#auto_coor auto_vif functions
#100%
covr::report(
  x = covr::file_coverage(
    source_files = c(
      "R/auto_cor.R",
      "R/auto_vif.R"
    ),
    test_files = "tests/testthat/test-auto_cor-auto_vif.R"
  )
)

#mc functions

covr::report(
  x = covr::file_coverage(
    source_files = c(
      "R/mc_auto.R",
      "R/mc_cor.R",
      "R/mc_vif.R",
      "R/mc_auto_vif.R",
      "R/mc_auto_cor.R"
    ),
    test_files = "tests/testthat/test-mc_functions.R"
  )
)

covr::report(
  x = covr::file_coverage(
    source_files = c(
      "R/fe_scale.R"
    ),
    test_files = "tests/testthat/test-fe_scale.R"
  )
)

covr::report(
  x = covr::file_coverage(
    source_files = c(
      "R/fe_rescale.R"
    ),
    test_files = "tests/testthat/test-fe_rescale.R"
  )
)



#run all tests with coverage
covr::with_coverage(
  testthat::test_dir("path/to/test/directory"),
  covr::package_coverage()
  )
