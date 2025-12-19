#' @title Compute optimization scores for spatial predictor selection
#' @description Computes optimization scores for candidate spatial predictor sets using either the "moran.i" or "p.value" method. Higher scores indicate better trade-offs between spatial autocorrelation reduction, model performance, and parsimony.
#' @param x Data frame containing optimization metrics for candidate spatial predictor sets. Generated internally by [select_spatial_predictors_sequential()] or [select_spatial_predictors_recursive()]. Must include columns: `moran.i`, `r.squared`, `penalization.per.variable`, and `p.value.binary` (for "p.value" method).
#' @param weight.r.squared Numeric value between 0 and 1 specifying the weight for R-squared in the optimization score. Higher values prioritize model performance.
#' @param weight.penalization.n.predictors Numeric value between 0 and 1 specifying the weight for penalizing the number of spatial predictors. Higher values favor more parsimonious models.
#' @param optimization.method Character string specifying the optimization method: `"moran.i"` (default) or `"p.value"`. Default: `"moran.i"`.
#' @return Numeric vector of optimization scores, one per row in `x`. Higher scores indicate better solutions. All values are rescaled between 0 and 1 for comparability.
#' @details
#' This function balances three objectives when selecting spatial predictors:
#' \enumerate{
#'   \item **Reduce spatial autocorrelation**: Maximize `1 - Moran's I` to minimize residual spatial autocorrelation
#'   \item **Maintain model performance**: Account for model R-squared
#'   \item **Favor parsimony**: Penalize models with many spatial predictors
#' }
#'
#' **Optimization methods:**
#'
#' The **"moran.i"** method computes:
#'
#' `score = (1 - Moran's I) + w1 × R² - w2 × penalization`
#'
#' where all components are rescaled to the range 0 to 1, `w1` = `weight.r.squared`, and `w2` = `weight.penalization.n.predictors`.
#'
#' The **"p.value"** method computes:
#'
#' `score = max(1 - Moran's I, binary p-value) + w1 × R² - w2 × penalization`
#'
#' where the binary p-value is 1 if p equal or lower than 0.05 (no significant spatial autocorrelation), and 0 otherwise.
#'
#' **Practical differences:**
#'
#' - The "moran.i" method uses continuous Moran's I values and typically selects more spatial predictors to achieve lower spatial autocorrelation
#' - The "p.value" method uses binary significance thresholds and typically selects fewer predictors, stopping once spatial autocorrelation becomes non-significant
#'
#' The optimal model is the one with the highest optimization score.
#' @seealso [select_spatial_predictors_recursive()], [select_spatial_predictors_sequential()], [moran()]
#' @examples
#' \dontrun{
#' # This function is typically called internally during spatial predictor selection
#' # Example showing the structure of input data:
#'
#' # Simulated optimization data frame
#' opt_data <- data.frame(
#'   moran.i = c(0.5, 0.3, 0.2, 0.15),
#'   r.squared = c(0.6, 0.65, 0.68, 0.69),
#'   penalization.per.variable = c(0.1, 0.2, 0.3, 0.4),
#'   p.value.binary = c(0, 0, 1, 1)
#' )
#'
#' # Compute optimization scores
#' scores_moran <- optimization_function(
#'   x = opt_data,
#'   weight.r.squared = 0.5,
#'   weight.penalization.n.predictors = 0.5,
#'   optimization.method = "moran.i"
#' )
#'
#' # Compare methods
#' scores_pvalue <- optimization_function(
#'   x = opt_data,
#'   weight.r.squared = 0.5,
#'   weight.penalization.n.predictors = 0.5,
#'   optimization.method = "p.value"
#' )
#'
#' # Higher score indicates better solution
#' which.max(scores_moran)
#' which.max(scores_pvalue)
#' }
#' @rdname optimization_function
#' @family utilities
#' @export
optimization_function <- function(
  x = NULL,
  weight.r.squared = NULL,
  weight.penalization.n.predictors = NULL,
  optimization.method = "moran.i"
) {
  #using Moran's I and its p-value
  if (optimization.method == "p.value") {
    optimization <- rescale_vector(
      pmax(
        rescale_vector(1 - x$moran.i),
        x$p.value.binary
      ) +
        (weight.r.squared * rescale_vector(x$r.squared)) -
        (weight.penalization.n.predictors *
          rescale_vector(x$penalization.per.variable))
    )
  }

  #Using only Moran's I
  if (optimization.method == "moran.i") {
    optimization <- rescale_vector(
      rescale_vector(1 - x$moran.i) +
        (weight.r.squared * rescale_vector(x$r.squared)) -
        (weight.penalization.n.predictors *
          rescale_vector(x$penalization.per.variable))
    )
  }

  optimization
}
