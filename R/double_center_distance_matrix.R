#' @title double_center_distance_matrix
#' @description generates a double-centered matrix from the weights of a distance matrix (see [weights_from_distance_matrix]) and a distance threshold. This is a required step before the computation of Moran's Eigenvector Maps.
#' @param x distance matrix among the records represented in the numeric vector. The number of rows of this matrix must be equal to the length of x. Default: NULL, Default: NULL
#' @param distance.threshold (optional) numeric, positive, in the range of values of `distance.matrix` distances below this value in the distance matrix are set to 0.  Default: 0
#' @return A double-centered matrix of the same dimensions as `x`.
#' @details Applies [weights_from_distance_matrix] to the distance matrix `x`,
#'
#'   ach element has subtracted fromit its corresponding row mean and column mean, and we add back the averageof all the elements.. Based on \link[ade4]{bicenter.wt} by Daniel Chessel.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  data(distance_matrix)
#'  x <- double_center_distance_matrix(
#'    x = distance_matrix
#'  )
#'  x
#'  }
#' }
#' @rdname double_center_distance_matrix
#' @export
double_center_distance_matrix <- function(
  x = NULL,
  distance.threshold = 0
){

  #distance matrix weights
  x <- weights_from_distance_matrix(
    x = x,
    distance.threshold = distance.threshold
  )

  #bicenter matrix
  #compute row means
  x.row.means <- x*0 + rowMeans(x)
  #compute col means
  x.col.means <- t(x*0 + colMeans(x))
  #double centering
  x.double.centered <- x - x.row.means - x.col.means + mean(x[])

  #bicenter wt code
  #col and row weights
  # cr.weights <- rep(1, ncol(x))
  # cr.weights <- cr.weights / sum(cr.weights)
  # cr.weights.sqrt <- sqrt(cr.weights)
  #
  # row.mean <- apply(cr.weights * x, 2, sum)
  # col.mean <- apply(cr.weights * t(x), 2, sum)
  # col.mean <- col.mean - sum(row.mean * cr.weights)
  # x <- sweep(x, 2, row.mean)
  # x <- t(sweep(t(x), 2, col.mean))
  # UNTIL HERE BOTH OPTIONS ARE THE SAME
  #
  # #multiplying by square root of the weights
  # x <- x * cr.weights.sqrt
  # x <- t(t(x) * cr.weights.sqrt)

  #return output
  x

}
