#' @title Prints cross-validation results
#' @description Prints the results of an spatial cross-validation performed with [rf_evaluate()].
#' @param x A model resulting from [rf_evaluate()].
#' @return A table printed to the standard output.
#' @seealso [plot_evaluation()], [get_evaluation()]
#' @rdname print_evaluation
#' @export
print_evaluation <- function(x){

  #stop if no evaluation slot
  if(!inherits(x, "rf_evaluate")){
    stop("Object 'x' does not have an 'evaluation' slot.")
  }

  metric <- NULL
  model <- x
  x <- model$evaluation$aggregated

    x <- x %>%
      dplyr::filter(
        model == "Testing"
      ) %>%
      dplyr::select(
        metric,
        median,
        mean,
        min,
        max
      ) %>%
      dplyr::mutate(
        median = round(median, 3),
        mean = round(mean, 3),
        min = round(min, 3),
        max = round(max, 3)
      ) %>%
      dplyr::rename(
        Metric = metric,
        Mean = mean,
        Median = median,
        Minimum = min,
        Maximum = max
      )

  #printing output
  cat("\n")
  cat("Spatial evaluation \n")
  cat("  - Training fraction:             ", model$evaluation$training.fraction, "\n", sep="")
  cat("  - Spatial folds:                 ", length(model$evaluation$spatial.folds), "\n\n", sep="")
  print(x, row.names = FALSE)

}
