#!/usr/bin/env Rscript

# Simpler script to run test files one by one
# Usage: Rscript dev_scripts/test_one_by_one_simple.R

library(testthat)

# Get all test files
test_dir <- "tests/testthat"
test_files <- list.files(
  test_dir,
  pattern = "^test-.*\\.[rR]$",
  full.names = TRUE
)

test_files <- sort(test_files)

cat("Found", length(test_files), "test files\n\n")

for (test_file in test_files) {
  test_name <- basename(test_file)
  cat("=" , rep("=", 60), "\n", sep = "")
  cat("Testing:", test_name, "\n")
  cat("=" , rep("=", 60), "\n", sep = "")

  # Set option to convert warnings to errors
  options(warn = 2)  # Turn warnings into errors

  tryCatch({
    result <- test_file(test_file, reporter = "progress")
    cat("\n✓ PASSED:", test_name, "\n\n")
  }, warning = function(w) {
    cat("\n✗ WARNING in:", test_name, "\n")
    cat("Message:", conditionMessage(w), "\n\n")
    cat("Stopping here. Fix this test before continuing.\n")
    quit(status = 1)
  }, error = function(e) {
    cat("\n✗ ERROR in:", test_name, "\n")
    cat("Message:", conditionMessage(e), "\n\n")
    cat("Stopping here. Fix this test before continuing.\n")
    quit(status = 1)
  })

  # Reset warning option
  options(warn = 0)
}

cat("\n")
cat("=" , rep("=", 60), "\n", sep = "")
cat("All tests passed without warnings!\n")
cat("=" , rep("=", 60), "\n", sep = "")
