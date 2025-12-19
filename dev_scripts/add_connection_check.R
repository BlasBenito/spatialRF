#!/usr/bin/env Rscript

# Add connection check to all test files that use parallel execution

test_files <- c(
  "tests/testthat/test-make_spatial_folds-examples.R",
  "tests/testthat/test-rank_spatial_predictors-examples.R",
  "tests/testthat/test-rf_evaluate-examples.R",
  "tests/testthat/test-rf_repeat-examples.R",
  "tests/testthat/test-rf_spatial-examples.R",
  "tests/testthat/test-rf_tuning-examples.R",
  "tests/testthat/test-select_spatial_predictors_recursive-examples.R",
  "tests/testthat/test-select_spatial_predictors_sequential-examples.R",
  "tests/testthat/test-the_feature_engineer-examples.R"
)

for (test_file in test_files) {
  if (!file.exists(test_file)) {
    cat("Skipping", test_file, "(not found)\n")
    next
  }

  # Read file
  content <- readLines(test_file)

  # Check if connection check already exists
  if (any(grepl("expect_no_open_connections", content))) {
    cat("Skipping", test_file, "(already has check)\n")
    next
  }

  # Find the last line (before any trailing blank lines)
  last_non_blank <- max(which(nchar(trimws(content)) > 0))

  # Add the check after the last line
  new_content <- c(
    content[1:last_non_blank],
    "",
    "# Verify no connections left open",
    "expect_no_open_connections()",
    content[seq(last_non_blank + 1, length(content))]
  )

  # Remove duplicate trailing empty lines
  while (length(new_content) > 1 &&
         nchar(trimws(new_content[length(new_content)])) == 0 &&
         nchar(trimws(new_content[length(new_content) - 1])) == 0) {
    new_content <- new_content[-length(new_content)]
  }

  # Write back
  writeLines(new_content, test_file)
  cat("Updated", test_file, "\n")
}

cat("\nDone! Run devtools::test() to identify which test leaves connections open.\n")
