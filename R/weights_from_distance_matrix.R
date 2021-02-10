#' @title Transforms a distance matrix into a matrix of weights
#' @description Transforms a distance matrix into weights (1/distance.matrix) normalized by the row sums. Used to compute Moran's I values and Moran's Eigenvector Maps. Allows to apply a threshold to the distance matrix before computing the weights.
#' @param x Numeric squared and symmetric distance matrix.
#' @param distance.threshold Numeric, positive, in the range of values of `distance.matrix`. Distances below this value in the distance matrix are set to 0., Default: 0
#' @return A weighted distance matrix.
#' @examples
#' \dontrun{
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

  #checking matrix dimensions
  if(!is.matrix(x) | ncol(x) != nrow(x)){
    stop("x must be a symmetric square matrix.")
  }

  if(!is.null(distance.threshold) & !is.numeric(distance.threshold) & distance.threshold > 0){
    stop("distance.threshold must be numeric and >= 0.")
  }

  #thresholding distance matrix
  if(distance.threshold > 0){
    x[x < distance.threshold] <- 0
  }

  #computing weights
  x.weights <- 1/x
  x.weights[is.infinite(x.weights)] <- 1
  diag(x.weights) <- 0

  #normalizing weights
  weight.rowsums <- rowSums(x.weights)
  weight.rowsums[weight.rowsums == 0] <- 1
  x.weights <- x.weights/weight.rowsums

  #returning output
  x.weights

}
