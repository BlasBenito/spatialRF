#' @title Transforms a distance matrix into a matrix of weights
#' @description Transforms a distance matrix into weights (1/distance.matrix) normalized by the row sums. Used to compute Moran's I values and Moran's Eigenvector Maps. Allows to apply a threshold to the distance matrix before computing the weights.
#' @usage
#' weights_from_distance_matrix(
#'   x,
#'   distance.threshold = 0
#' )
#' @param x Numeric squared and symmetric distance matrix.
#' @param distance.threshold Numeric, positive, in the range of values of `distance.matrix`. Distances below this value in the distance matrix are set to 0., Default: 0
#' @return A weighted distance matrix.
#' @examples
#' \donttest{
#' if(interactive()){
#'
#'  data(distance_matrix)
#'
#'  distance.matrix.weights <- weights_from_distance_matrix(
#'    x = distance_matrix
#'    )
#'
#'  distance.matrix.weights
#'
#' }
#' }
#' @rdname weights_from_distance_matrix
#' @export
weights_from_distance_matrix <- function(
  x,
  distance.threshold = 0
  ){

  #thresholding distance matrix
  x[x <= distance.threshold] <- 1
  diag(x) <- NA

  #computing weights
  x.weights <- 1/x

  #normalizing weights
  weight.rowsums <- rowSums(
    x.weights,
    na.rm = TRUE
    )
  x.weights <- x.weights/weight.rowsums

  #fixing Inf and diag
  x.weights[x.weights == Inf] <- 0
  diag(x.weights) <- 0

  #returning output
  x.weights

}
