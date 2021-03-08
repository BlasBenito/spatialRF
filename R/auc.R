#' @title Area under the ROC curve
#' @description Computes the area under the ROC curve in models with binary responses.
#' @param o Numeric vector with observations, must have the same length as `p`.
#' @param p Numeric vector with predictions, must have the same length as `o`.
#' @return Numeric, AUC value.
#' @examples
#' \donttest{
#' if(interactive()){
#'
#'  out <- auc(
#'    o = runif(10),
#'    p = runif(10)
#'    )
#'
#' }
#' }
#' @rdname auc
#' @export
auc <- function(o, p){

  #predicted values of the ones and the zeroes
  ones <- stats::na.omit(p[o == 1])
  zeros <- stats::na.omit(p[o == 0])

  #lengths of each vector
  n.ones <- length(ones)
  n.zeros <- length(zeros)

  #curve computation
  curve <- sum(rank(c(ones, zeros))[1:n.ones]) - (n.ones*(n.ones+1)/2)

  #area under the curve
  auc <- curve / (n.zeros * n.ones)

  auc

}
