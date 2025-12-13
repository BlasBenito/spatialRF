# Script to extract all examples from .Rd files and write to examples.R

# Get all .Rd files
rd_files <- list.files("man", pattern = "\\.Rd$", full.names = TRUE)

# Initialize output
all_examples <- "# Extract and run all examples from the spatialRF package documentation\n# Generated automatically from .Rd files in man/\n\nlibrary(spatialRF)\n\n"

# Function to extract examples from an Rd file
extract_examples <- function(rd_file) {

  # Read the file
  lines <- readLines(rd_file, warn = FALSE)

  # Find the examples section
  examples_start <- which(grepl("^\\\\examples\\{", lines))

  if (length(examples_start) == 0) {
    return(NULL)
  }

  # Find the closing brace
  # Start from the line after \examples{
  start_line <- examples_start + 1

  # Find matching closing brace
  # We need to track brace depth
  brace_depth <- 1
  end_line <- start_line

  for (i in start_line:length(lines)) {
    # Count opening and closing braces
    open_braces <- lengths(regmatches(lines[i], gregexpr("\\{", lines[i])))
    close_braces <- lengths(regmatches(lines[i], gregexpr("\\}", lines[i])))

    brace_depth <- brace_depth + open_braces - close_braces

    if (brace_depth == 0) {
      end_line <- i - 1
      break
    }
  }

  if (end_line < start_line) {
    return(NULL)
  }

  # Extract the example code
  example_lines <- lines[start_line:end_line]

  # Remove leading/trailing empty lines
  while (length(example_lines) > 0 && grepl("^\\s*$", example_lines[1])) {
    example_lines <- example_lines[-1]
  }
  while (length(example_lines) > 0 && grepl("^\\s*$", example_lines[length(example_lines)])) {
    example_lines <- example_lines[-length(example_lines)]
  }

  if (length(example_lines) == 0) {
    return(NULL)
  }

  # Get function name from file
  func_name <- sub("\\.Rd$", "", basename(rd_file))

  # Format the output
  paste0(
    "# ", func_name, " ----\n",
    paste(example_lines, collapse = "\n"),
    "\n\n"
  )
}

# Extract examples from all files
for (rd_file in rd_files) {
  example <- extract_examples(rd_file)
  if (!is.null(example)) {
    all_examples <- paste0(all_examples, example)
  }
}

# Write to file
writeLines(all_examples, "dev_scripts/examples.R")

cat("Examples extracted to dev_scripts/examples.R\n")
