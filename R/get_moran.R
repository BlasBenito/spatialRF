#' @title Extract Moran's I test results for model residuals
#' @description Extracts Moran's I test results for spatial autocorrelation in model residuals from models fitted with [rf()], [rf_repeat()], or [rf_spatial()].
#' @param model Model object from [rf()], [rf_repeat()], or [rf_spatial()].
#' @return Data frame with Moran's I statistics at multiple distance thresholds. Columns include `distance.threshold`, `moran.i` (statistic), `p.value`, `interpretation`, and `method`.
#' @details
#' Moran's I tests for spatial autocorrelation in model residuals. Significant positive values indicate residuals are spatially clustered, suggesting the model hasn't fully captured spatial patterns. For spatial models ([rf_spatial()]), low or non-significant Moran's I values indicate successful removal of spatial autocorrelation.
#' @seealso [moran()], [moran_multithreshold()], [plot_moran()], [print_moran()]
#' @examples
#' data(plants_rf)
#'
#' # Extract Moran's I test results
#' moran_results <- get_moran(plants_rf)
#' moran_results
#'
#' # Check for significant spatial autocorrelation
#' significant <- moran_results[moran_results$p.value < 0.05, ]
#' significant
#'
#' @rdname get_moran
#' @family model_info
#' @export
#' @autoglobal
get_moran <- function(model) {
  model$residuals$autocorrelation$per.distance
}
