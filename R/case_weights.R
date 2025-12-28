#' @title Generate case weights for imbalanced binary data
#' @description Generates case weights to balance binary response variables for use with `ranger` models. Used internally by [rf()].
#' @param data Data frame containing the response variable. Default: `NULL`.
#' @param dependent.variable.name Character string specifying the response variable name. Must be a column in `data`. Default: `NULL`.
#' @return Numeric vector of length `nrow(data)` with case weights. Each weight is the inverse of the class frequency: `1/n_zeros` for 0s and `1/n_ones` for 1s.
#' @details
#' The weighting scheme assigns higher weights to the minority class to balance training:
#' \itemize{
#'   \item Cases with value 0: weight = `1 / n_zeros`
#'   \item Cases with value 1: weight = `1 / n_ones`
#' }
#' This ensures both classes contribute equally to model training regardless of class imbalance.
#' @examples
#' # Imbalanced dataset: 3 zeros, 2 ones
#' weights <- case_weights(
#'   data = data.frame(
#'     response = c(0, 0, 0, 1, 1)
#'   ),
#'   dependent.variable.name = "response"
#' )
#'
#' weights
#' # Returns: 0.333, 0.333, 0.333, 0.5, 0.5
#' # Zeros get weight 1/3, ones get weight 1/2
#'
#' @rdname case_weights
#' @family preprocessing
#' @export
#' @autoglobal
case_weights <- function(
  data = NULL,
  dependent.variable.name = NULL
) {
  #counting number of ones and zeros
  n <- nrow(data)
  n.1 <- sum(data[, dependent.variable.name])
  n.0 <- n - n.1

  #computing weights
  weight.1 <- 1 / n.1
  weight.0 <- 1 / n.0

  #vector of weights
  case.weights <- rep(NA, n)
  case.weights[data[, dependent.variable.name] == 1] <- weight.1
  case.weights[data[, dependent.variable.name] == 0] <- weight.0

  #return case weights
  case.weights
}
