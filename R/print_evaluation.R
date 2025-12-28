#' @title Prints cross-validation results
#' @description Prints the results of an spatial cross-validation performed with [rf_evaluate()].
#' @param model A model resulting from [rf_evaluate()].
#' @return A table printed to the standard output.
#' @seealso [plot_evaluation()], [get_evaluation()]
#' @examples
#'
#' if(interactive()){
#'
#' data(
#'   plants_rf,
#'   plants_xy
#' )
#'
#' plants_rf <- rf_evaluate(
#'   model = plants_rf,
#'   xy = plants_xy,
#'   repetitions = 5,
#'   n.cores = 1
#' )
#'
#' print_evaluation(plants_rf)
#'
#' }
#'
#' @rdname print_evaluation
#' @family model_info
#' @export
#' @autoglobal
print_evaluation <- function(model) {
  #stop if no evaluation slot
  if (!inherits(model, "rf_evaluate")) {
    stop("Object 'x' does not have an 'evaluation' slot.")
  }

  x <- model$evaluation$aggregated[
    model$evaluation$aggregated$model == "Testing",
    c("metric", "median", "median_absolute_deviation", "min", "max")
  ]

  x$median <- round(x$median, 3)
  x$median_absolute_deviation <- round(x$median_absolute_deviation, 3)
  x$min <- round(x$min, 3)
  x$max <- round(x$max, 3)

  colnames(x)[colnames(x) == "metric"] <- "Metric"
  colnames(x)[colnames(x) == "median"] <- "Median"
  colnames(x)[colnames(x) == "median_absolute_deviation"] <- "MAD"
  colnames(x)[colnames(x) == "min"] <- "Minimum"
  colnames(x)[colnames(x) == "max"] <- "Maximum"

  x <- x[, colSums(is.na(x)) < nrow(x)]

  x <- stats::na.omit(x)

  #printing output
  cat("\n")
  cat("Spatial evaluation \n")
  cat(
    "  - Training fraction:             ",
    model$evaluation$training.fraction,
    "\n",
    sep = ""
  )
  cat(
    "  - Spatial folds:                 ",
    length(model$evaluation$spatial.folds),
    "\n\n",
    sep = ""
  )
  print(x, row.names = FALSE)
}
