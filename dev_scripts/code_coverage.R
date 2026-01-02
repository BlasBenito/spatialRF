library(covr)
library(codetools)
library(testthat)
library(devtools)

# Load the package
devtools::load_all()

#get code coverage for rf.R
x = covr::file_coverage(
  source_files = "R/rf.R",
  test_files = "tests/testthat/test-rf.R"
)


#full coverage
covr::package_coverage()

# Show detailed coverage
print(x)
cat("\nPercent coverage:", covr::percent_coverage(x), "%\n\n")

# Show uncovered lines
cat("Lines NOT covered:\n")
df <- covr::tally_coverage(x, by = "line")
uncovered <- df[df$value == 0, ]
if (nrow(uncovered) > 0) {
  print(uncovered[, c("filename", "line", "value")])
} else {
  cat("All lines covered!\n")
}
