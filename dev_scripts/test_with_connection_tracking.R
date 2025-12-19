#!/usr/bin/env Rscript

# Track connections before and after each test
library(testthat)

test_dir <- "tests/testthat"
test_files <- sort(list.files(test_dir, pattern = "^test-.*\\.[rR]$", full.names = TRUE))

cat("Tracking connections for each test...\n\n")

get_connection_count <- function() {
  cons <- showConnections(all = TRUE)
  # Exclude stdin/stdout/stderr (0, 1, 2)
  sum(as.numeric(rownames(cons)) > 2)
}

initial_count <- get_connection_count()
cat("Initial connections:", initial_count, "\n\n")

for (test_file in test_files) {
  test_name <- basename(test_file)

  before_count <- get_connection_count()

  # Run test silently
  suppressMessages(
    test_file(test_file, reporter = "silent")
  )

  after_count <- get_connection_count()
  diff <- after_count - before_count

  if (diff > 0) {
    cat(sprintf("%-50s +%d connections (now %d)\n", test_name, diff, after_count))

    # Show the new connections
    cons <- showConnections(all = TRUE)
    if (nrow(cons) > 0) {
      cat("  New connections:\n")
      print(tail(cons, diff))
    }
  } else if (diff < 0) {
    cat(sprintf("%-50s %d connections (now %d)\n", test_name, diff, after_count))
  }
}

final_count <- get_connection_count()
cat("\n")
cat("Final connections:", final_count, "\n")
cat("Net change:", final_count - initial_count, "\n")

if (final_count > initial_count) {
  cat("\nRemaining connections:\n")
  print(showConnections(all = TRUE))
}
