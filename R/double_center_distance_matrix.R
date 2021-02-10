#' @title Double centers a distance matrix
#' @description Generates a double-centered matrix from the weights of a distance matrix (see [weights_from_distance_matrix()]) and a distance threshold. This is a required step before the computation of Moran's Eigenvector Maps.
#' @param x Numeric distance matrix. Default: `NULL`.
#' @param distance.threshold Numeric, positive, in the range of values of `x`. Distances below this value in the distance matrix are set to 0.  Default: `0`.
#' @return A double-centered matrix of the same dimensions as `x.
#' @examples
#' \dontrun{
#' if(interactive()){
#'
#'  data(distance_matrix)
#'
#'  x <- double_center_distance_matrix(
#'    x = distance_matrix
#'  )
#'  x
#'
#'  }
#' }
#' @seealso [weights_from_distance_matrix()], [mem()], [mem_multithreshold()]
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
