#!/usr/bin/env Rscript

# Script to run test files one by one and stop on warnings
# Usage: Rscript dev_scripts/test_one_by_one.R

library(testthat)

# Get all test files
test_dir <- "tests/testthat"
test_files <- list.files(
  test_dir,
  pattern = "^test-.*\\.[rR]$",
  full.names = TRUE
)

# Sort for consistent order
test_files <- sort(test_files)

cat("Found", length(test_files), "test files\n")
cat("Running tests one by one...\n\n")

# Track results
failed_file <- NULL
warning_file <- NULL

for (test_file in test_files) {
  test_name <- basename(test_file)
  cat("Testing:", test_name, "... ")

  # Clear previous warnings
  assign("last.warning", NULL, envir = baseenv())

  # Run the test and capture output
  result <- tryCatch({

    # Capture warnings
    warnings_list <- NULL
    withCallingHandlers(
      {
        test_results <- test_file(test_file, reporter = "minimal")
        list(
          success = TRUE,
          results = test_results,
          warnings = warnings_list
        )
      },
      warning = function(w) {
        warnings_list <<- c(warnings_list, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )

  }, error = function(e) {
    list(
      success = FALSE,
      error = conditionMessage(e),
      warnings = NULL
    )
  })

  # Check for warnings
  current_warnings <- warnings()
  has_warnings <- !is.null(current_warnings) ||
                  !is.null(result$warnings) ||
                  length(result$warnings) > 0

  if (!result$success) {
    cat("ERROR\n")
    cat("  Error message:", result$error, "\n\n")
    failed_file <- test_name
    break
  } else if (has_warnings) {
    cat("WARNING\n")
    if (!is.null(current_warnings)) {
      cat("  Warnings detected:\n")
      print(current_warnings)
    }
    if (!is.null(result$warnings) && length(result$warnings) > 0) {
      cat("  Captured warnings:\n")
      for (w in result$warnings) {
        cat("   -", w, "\n")
      }
    }
    cat("\n")
    warning_file <- test_name
    break
  } else {
    cat("OK\n")
  }
}

# Summary
cat("\n")
cat("=" , rep("=", 60), "\n", sep = "")

if (!is.null(failed_file)) {
  cat("FAILED at:", failed_file, "\n")
  quit(status = 1)
} else if (!is.null(warning_file)) {
  cat("WARNINGS detected in:", warning_file, "\n")
  cat("\nTo investigate, run:\n")
  cat("  testthat::test_file('", file.path(test_dir, warning_file), "')\n", sep = "")
  quit(status = 1)
} else {
  cat("All tests passed without warnings!\n")
  quit(status = 0)
}
