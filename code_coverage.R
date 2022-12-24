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


#run all tests with coverage
covr::with_coverage(
  testthat::test_dir("path/to/test/directory"),
  covr::package_coverage()
  )
