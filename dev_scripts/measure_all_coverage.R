#!/usr/bin/env Rscript
# Measures coverage for all test files and generates a summary report

library(covr)
devtools::load_all()

test_files <- list.files(
  "tests/testthat",
  pattern = "^test-.*\\.R$",
  full.names = TRUE
)

results <- data.frame(
  test_file = character(),
  source_file = character(),
  coverage_pct = numeric(),
  stringsAsFactors = FALSE
)

cat("Measuring coverage for", length(test_files), "test files...\n\n")

for (test_file in test_files) {
  # Extract function name from test file
  func_name <- sub("^test-", "", basename(test_file))
  func_name <- sub("-examples\\.R$|\\.R$", "", func_name)

  source_file <- paste0("R/", func_name, ".R")

  if (file.exists(source_file)) {
    cat("Processing:", basename(test_file), "->", basename(source_file), "... ")

    tryCatch({
      coverage <- covr::file_coverage(
        source_files = source_file,
        test_files = test_file
      )

      pct <- covr::percent_coverage(coverage)

      results <- rbind(results, data.frame(
        test_file = basename(test_file),
        source_file = basename(source_file),
        coverage_pct = round(pct, 1),
        stringsAsFactors = FALSE
      ))

      cat(round(pct, 1), "%\n")
    }, error = function(e) {
      cat("ERROR:", e$message, "\n")
    })
  } else {
    cat("SKIPPING:", basename(test_file), "(source file not found)\n")
  }
}

# Sort by coverage percentage (lowest first)
results <- results[order(results$coverage_pct), ]

cat("\n=== Coverage Summary ===\n\n")
print(results, row.names = FALSE)

cat("\n=== Statistics ===\n")
cat("Total files analyzed:", nrow(results), "\n")
cat("Files with coverage >= 70%:", sum(results$coverage_pct >= 70), "/", nrow(results), "\n")
cat("Files with coverage < 70%:", sum(results$coverage_pct < 70), "\n")
cat("Files with coverage < 50%:", sum(results$coverage_pct < 50), "\n")
cat("Mean coverage:", round(mean(results$coverage_pct), 1), "%\n")
cat("Median coverage:", round(median(results$coverage_pct), 1), "%\n")

# Save results
write.csv(results, "coverage_results.csv", row.names = FALSE)
cat("\nDetailed results saved to coverage_results.csv\n")

# Show files that need enhancement
cat("\n=== Files Needing Enhancement (< 70%) ===\n")
low_coverage <- results[results$coverage_pct < 70, ]
if (nrow(low_coverage) > 0) {
  print(low_coverage, row.names = FALSE)
} else {
  cat("All files meet the 70% coverage threshold!\n")
}
