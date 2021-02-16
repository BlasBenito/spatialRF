#' @title Double centers a distance matrix
#' @description Generates a double-centered matrix from the weights of a distance matrix (see [weights_from_distance_matrix()]) and a distance threshold. This is a required step before the computation of Moran's Eigenvector Maps.
#' @param x Numeric distance matrix. Default: `NULL`.
#' @param distance.threshold Numeric, positive, in the range of values of `x`. Distances below this value in the distance matrix are set to 0.  Default: `0`.
#' @return A double-centered matrix of the same dimensions as `x.
#' @examples
#' \donttest{
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

  #return output
  x

}
