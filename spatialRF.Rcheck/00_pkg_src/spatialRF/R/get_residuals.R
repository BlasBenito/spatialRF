#' @title Extract model residuals
#' @description Extracts residuals (observed - predicted values) from models fitted with [rf()], [rf_repeat()], or [rf_spatial()].
#' @param model Model object from [rf()], [rf_repeat()], or [rf_spatial()].
#' @return Numeric vector of residuals with length equal to the number of training observations. For [rf_repeat()] models, returns the median residual across repetitions.
#' @details
#' Residuals are calculated as observed minus predicted values. They can be used to assess model fit, check assumptions, and diagnose patterns such as spatial autocorrelation (see [get_moran()]). Ideally, residuals should be randomly distributed with no systematic patterns.
#' @seealso [rf()], [rf_repeat()], [rf_spatial()], [get_predictions()], [get_moran()], [plot_residuals_diagnostics()]
#' @examples
#' data(plants_rf)
#'
#' # Extract residuals
#' residuals <- get_residuals(plants_rf)
#' head(residuals)
#'
#' # Check basic statistics
#' summary(residuals)
#'
#' # Plot distribution to check for patterns
#' hist(residuals, main = "Residual Distribution", xlab = "Residuals")
#'
#' @rdname get_residuals
#' @family model_info
#' @export
get_residuals <- function(model) {
  model$residuals$values
}
