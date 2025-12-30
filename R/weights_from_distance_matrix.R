#' @title Transforms a distance matrix into a matrix of weights
#' @description Transforms a distance matrix into weights (1/distance.matrix) normalized by the row sums. Used to compute Moran's I values and Moran's Eigenvector Maps. Allows to apply a threshold to the distance matrix before computing the weights.
#' @param distance.matrix Distance matrix. Default: `NULL`.
#' @param distance.threshold Numeric, positive, in the range of values of `distance.matrix`. Distances below this value in the distance matrix are set to 0., Default: `0`.
#' @return A weighted distance matrix.
#' @examples
#'
#' data(plants_distance)
#'
#' y <- weights_from_distance_matrix(
#'   distance.matrix = plants_distance
#' )
#'
#' y[1:5, 1:5]
#'
#' @rdname weights_from_distance_matrix
#' @family preprocessing
#' @autoglobal
#' @export
weights_from_distance_matrix <- function(
  distance.matrix = NULL,
  distance.threshold = 0
) {
  if (is.null(distance.matrix)) {
    stop("Argument 'distance.matrix' is missing.`")
  }

  #thresholding distance matrix
  distance.matrix[distance.matrix <= distance.threshold] <- 1

  #diagonal as NA
  diag(distance.matrix) <- NA

  #computing weights
  x.weights <- 1 / distance.matrix

  #normalizing weights
  weight.rowsums <- rowSums(
    x.weights,
    na.rm = TRUE
  )
  x.weights <- x.weights / weight.rowsums

  #fixing Inf and diag
  x.weights[x.weights == Inf] <- 0
  diag(x.weights) <- 0

  #returning output
  x.weights
}
