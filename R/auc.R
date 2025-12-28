#' @title Area under the ROC curve
#' @description Computes the area under the ROC curve (AUC) for binary classification.
#' @param o Numeric vector of actual binary labels (0 or 1). Must have the same length as `p`.
#' @param p Numeric vector of predicted probabilities (typically 0 to 1). Must have the same length as `o`.
#' @return Numeric value between 0 and 1 representing the AUC. Higher values indicate better classification performance, with 0.5 indicating random performance and 1.0 indicating perfect classification.
#' @examples
#'
#' auc(
#'   o = c(0, 0, 1, 1),
#'   p = c(0.1, 0.6, 0.4, 0.8)
#'   )
#'
#' @rdname auc
#' @family utilities
#' @export
#' @autoglobal
auc <- function(o, p) {
  #predicted values of the ones and the zeroes
  ones <- stats::na.omit(p[o == 1])
  zeros <- stats::na.omit(p[o == 0])

  #lengths of each vector
  n.ones <- length(ones)
  n.zeros <- length(zeros)

  #curve computation
  curve <- sum(rank(c(ones, zeros))[1:n.ones]) - (n.ones * (n.ones + 1) / 2)

  #area under the curve
  auc <- curve / (n.zeros * n.ones)

  auc
}
