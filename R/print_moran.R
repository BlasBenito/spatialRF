#' @title Prints results of a Moran's I test
#' @description Prints the results of a Moran's I test on the residuals of a model.
#' @usage
#' print_moran(
#'   model
#' )
#' @param model A model fitted with [rf()], [rf_repeat()], or [rf_spatial()].
#' @return Prints a table to the console.
#' @seealso [moran()], [moran_multithreshold()], [get_moran()], [plot_moran()]
#' @examples
#'
#' data(plants_rf)
#'
#' print_moran(plants_rf)
#'
#' @rdname print_moran
#' @family model_info
#' @export
print_moran <- function(model) {
  #if model is not a data frame
  if (
    inherits(model, "rf") ||
      inherits(model, "rf_repeat") ||
      inherits(model, "rf_spatial")
  ) {
    x <- model$residuals$autocorrelation$per.distance
  }

  #subsetting columns
  x <- x[, c("distance.threshold", "moran.i", "p.value", "interpretation")]

  #for models rf and rf_repeat
  if (!("model" %in% colnames(x))) {
    #adding pretty colnames
    colnames(x) <- c("Distance", "Moran's I", "P value", "Interpretation")

    #format numeric columns
    x[, 1] <- sprintf("%.1f", x[, 1])
    x[, 2] <- sprintf("%.3f", x[, 2])
    x[, 3] <- sprintf("%.3f", x[, 3])
  }

  #for rf_spatial with rf
  if ("model" %in% colnames(x) && !("repetition" %in% colnames(x))) {
    #adding pretty colnames
    colnames(x) <- c(
      "Distance",
      "Moran's I",
      "P value",
      "Interpretation",
      "Model"
    )

    #reordering x
    x <- x[, c("Model", "Distance", "Moran's I", "P value", "Interpretation")]

    #format numeric columns
    x[, 2] <- sprintf("%.1f", x[, 2])
    x[, 3] <- sprintf("%.3f", x[, 3])
    x[, 4] <- sprintf("%.3f", x[, 4])
  }

  #print to screen
  # Capture output
  output <- capture.output(print(x, row.names = FALSE, right = FALSE))

  # Add indentation (e.g., 2 spaces) to each line
  cat(paste0("   ", output, collapse = "\n"), "\n")

  # print(x, row.names = FALSE, right = FALSE)
}
